//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2018-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScSignalRHttpTransports;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  Classes, SysUtils, TypInfo, SyncObjs,
  ScTypes, ScFunctions, ScCLRClasses, ScVio, ScUtils,
  ScSecureConnection, ScHttp, ScWebSocketClient, ScPipe,
  ScSignalRConsts, ScSignalRProtocol, ScSignalRHttpConnection;

type
  TThreadEx = class(TThread)
  private
    FFinished: boolean;
  end;

  TScWebSocketsTransport = class(TScTransport)
  private
    FLogger: TScLogger;
    FAccessTokenProvider: TScGetString;
    FTransport: TScDuplexPipe;
    FApplication: TScDuplexPipe;
    FWebSocket: TScWebSocketClient;
    FWebSocketMessageType: TScWebSocketMessageType;
    FCloseTimeout: cardinal;
    FAborted: boolean;

    FReceivingThread: TThreadEx;
    FSendingThread: TThreadEx;

  protected
    function GetInput: TScPipeReader; override;
    function GetOutput: TScPipeWriter; override;
    procedure ProcessWebSocket;
    procedure StartReceiving;
    procedure StartSending;
    function WebSocketCanSend(WebSocket: TScWebSocketClient): boolean;
    function ResolveWebSocketsUrl(const Url: string): string;

  public
    constructor Create(AHttpConnectionOptions: TScHttpConnectionOptions; AAccessTokenProvider: TScGetString; ALogger: TScLogger);
    destructor Destroy; override;

    procedure Start(const Url: string; TransferFormat: TScTransferFormat; CancellationToken: TScCancellationToken); override;
    procedure Stop; override;
  end;

  TScLongPollingTransport = class(TScTransport)
  private
    FLogger: TScLogger;
    FHttpClient: TScHttpClient;
    FTransport: TScDuplexPipe;
    FApplication: TScDuplexPipe;
    FCancellationToken: TScCancellationToken;

    FReceivingThread: TThreadEx;
    FSendingThread: TThreadEx;

    procedure Process(const Url: string);
    procedure Poll(const PollUrl: string; CancellationToken: TScCancellationToken);
    procedure SendDeleteRequest(const Url: string);
    procedure SendMessages(const SendUrl: string; CancellationToken: TScCancellationToken = nil);

  protected
    function GetInput: TScPipeReader; override;
    function GetOutput: TScPipeWriter; override;

  public
    constructor Create(AHttpClient: TScHttpClient; ALogger: TScLogger);
    destructor Destroy; override;

    procedure Start(const Url: string; TransferFormat: TScTransferFormat; CancellationToken: TScCancellationToken); override;
    procedure Stop; override;
  end;

  TScServerSentEventsTransport = class(TScTransport)
  private
    FLogger: TScLogger;
    FHttpClient: TScHttpClient;

  protected
    function GetInput: TScPipeReader; override;
    function GetOutput: TScPipeWriter; override;

  public
    constructor Create(AHttpClient: TScHttpClient; ALogger: TScLogger);
    destructor Destroy; override;

    procedure Start(const Url: string; TransferFormat: TScTransferFormat; CancellationToken: TScCancellationToken); override;
    procedure Stop; override;
  end;

implementation

type
  TScWebSocketsReceivingLoop = class(TThreadEx)
  private
    FWebSocketsTransport: TScWebSocketsTransport;
  protected
    procedure Execute; override;
  public
    constructor Create(AWebSocketsTransport: TScWebSocketsTransport);
  end;

  TScWebSocketsSendingLoop = class(TThreadEx)
  private
    FWebSocketsTransport: TScWebSocketsTransport;
    FFinishedEvent: TEvent;
  protected
    procedure Execute; override;
  public
    constructor Create(AWebSocketsTransport: TScWebSocketsTransport);
    destructor Destroy; override;
  end;

  TScLongPollingReceivingLoop = class(TThreadEx)
  private
    FLongPollingTransport: TScLongPollingTransport;
    FUrl: string;
  protected
    procedure Execute; override;
  public
    constructor Create(ALongPollingTransport: TScLongPollingTransport; const AUrl: string);
  end;

  TScLongPollingSendingLoop = class(TThreadEx)
  private
    FLongPollingTransport: TScLongPollingTransport;
    FUrl: string;
  protected
    procedure Execute; override;
  public
    constructor Create(ALongPollingTransport: TScLongPollingTransport; const AUrl: string);
  end;

{ TScWebSocketsReceivingLoop }

constructor TScWebSocketsReceivingLoop.Create(AWebSocketsTransport: TScWebSocketsTransport);
begin
  FWebSocketsTransport := AWebSocketsTransport;

  inherited Create(False); /// Input\Output error(5) for non-Windows platforms
end;

procedure TScWebSocketsReceivingLoop.Execute;
begin
  try
    try
      try
        FWebSocketsTransport.StartReceiving;
      finally
        if not FWebSocketsTransport.FSendingThread.FFinished then begin
          FWebSocketsTransport.FApplication.Input.CancelPendingRead;

          if TScWebSocketsSendingLoop(FWebSocketsTransport.FSendingThread).FFinishedEvent.WaitFor(FWebSocketsTransport.FCloseTimeout) <> wrSignaled then begin
            FWebSocketsTransport.FAborted := True;

            // Abort the websocket if we're stuck in a pending send to the client
            FWebSocketsTransport.FWebSocket.Abort;
          end;
        end;
      end;
    finally
      FFinished := True;
    end;
  except
  end;
end;

{ TScWebSocketsSendingLoop }

constructor TScWebSocketsSendingLoop.Create(AWebSocketsTransport: TScWebSocketsTransport);
begin
  FWebSocketsTransport := AWebSocketsTransport;
  FFinishedEvent := CreateEvent;

  inherited Create(False); /// Input\Output error(5) for non-Windows platforms
end;

destructor TScWebSocketsSendingLoop.Destroy;
begin
  inherited;
  FFinishedEvent.Free;
end;

procedure TScWebSocketsSendingLoop.Execute;
begin
  try
    try
      try
        FWebSocketsTransport.StartSending;
      finally
        if not FWebSocketsTransport.FReceivingThread.FFinished then begin
          FWebSocketsTransport.FAborted := True;

          // Abort the websocket if we're stuck in a pending receive from the client
          FWebSocketsTransport.FWebSocket.Abort;

          // Cancel any pending flush so that we can quit
          FWebSocketsTransport.FApplication.Output.CancelPendingFlush;
        end;
      end;
    finally
      FFinished := True;
      FFinishedEvent.SetEvent;
    end;
  except
  end;
end;

{ TScWebSocketsTransport }

constructor TScWebSocketsTransport.Create(AHttpConnectionOptions: TScHttpConnectionOptions;
  AAccessTokenProvider: TScGetString; ALogger: TScLogger);
begin
  inherited Create;

  FLogger := ALogger;

  FAccessTokenProvider := AAccessTokenProvider;
  FCloseTimeout := AHttpConnectionOptions.CloseTimeout;

  FWebSocket := TScWebSocketClient.Create(nil);
  FWebSocket.Options.Cookies := AHttpConnectionOptions.Cookies.Text;
  FWebSocket.Options.Credentials := AHttpConnectionOptions.Credentials;
  FWebSocket.Proxy := AHttpConnectionOptions.Proxy;
  FWebSocket.SSLOptions := AHttpConnectionOptions.SSLOptions;
  FWebSocket.Options.RequestHeaders.Assign(AHttpConnectionOptions.Headers);

  FWebSocket.Options.RequestHeaders['X-Requested-With'] := 'XMLHttpRequest';
end;

destructor TScWebSocketsTransport.Destroy;
begin
  FTransport.Free;
  FApplication.Free;
  FReceivingThread.Free;
  FSendingThread.Free;
  FWebSocket.Free;

  inherited;
end;

function TScWebSocketsTransport.GetInput: TScPipeReader;
begin
  Result := FTransport.Input;
end;

function TScWebSocketsTransport.GetOutput: TScPipeWriter;
begin
  Result := FTransport.Output;
end;

procedure TScWebSocketsTransport.Start(const Url: string; TransferFormat: TScTransferFormat;
  CancellationToken: TScCancellationToken);
var
  ResolvedUrl: string;
  AccessToken: string;
  Pair: TScDuplexPipePair;
begin
  if Url = '' then
    raise ArgumentException.Create('Url');

  if (TransferFormat <> tfBinary) and (TransferFormat <> tfText) then
    raise ArgumentException.CreateFmt(STransferFormatNotSupported, [TScTransferFormatHelper.ToString(TransferFormat)]);

  if TransferFormat = tfBinary then
    FWebSocketMessageType := mtBinary
  else
    FWebSocketMessageType := mtText;

  ResolvedUrl := ResolveWebSocketsUrl(Url);

  if Assigned(FAccessTokenProvider) then begin
    AccessToken := FAccessTokenProvider;
    if AccessToken <> '' then
      FWebSocket.Options.RequestHeaders['Authorization'] := 'Bearer ' + AccessToken;
  end;

  Log.LogInformation(FLogger, SWebSocketsTransport_StartTransport, [TScTransferFormatHelper.ToString(TransferFormat), ResolvedUrl]);

  FWebSocket.Connect(ResolvedUrl, CancellationToken);

  Log.LogDebug(FLogger, STransport_StartedTransport);

  // Create the pipe pair (Application's writer is connected to Transport's reader, and vice versa)
  Pair := TScDuplexPipe.CreateConnectionPair(DefaultPipeOptions);
  FTransport := Pair.Transport;
  FApplication := Pair.Application;

  ProcessWebSocket;
end;

procedure TScWebSocketsTransport.ProcessWebSocket;
begin
  // Begin sending and receiving
  FReceivingThread := TScWebSocketsReceivingLoop.Create(Self);
  FSendingThread := TScWebSocketsSendingLoop.Create(Self);
end;

procedure TScWebSocketsTransport.StartReceiving;
var
  MessageType: TScWebSocketMessageType;
  EndOfMessage: boolean;
  Count: integer;
  MemoryRef: TScMemoryRef;
  FlushResult: TScFlushResult;
begin
  try
    try
      try
        while True do begin
          MemoryRef := FApplication.Output.GetMemory;
          Count := FWebSocket.Receive(PtrOffset(MemoryRef.Memory, MemoryRef.Offset), MemoryRef.Length, MessageType, EndOfMessage);

          if MessageType = mtClose then begin
            Log.LogInformation(FLogger, SWebSocketsTransport_WebSocketClosed, [GetEnumName(TypeInfo(TScWebSocketCloseStatus), integer(FWebSocket.CloseStatus))]);

            if FWebSocket.CloseStatus <> csNormalClosure then
              raise InvalidOperationException.CreateFmt(SWebsocketClosedWithError, [GetEnumName(TypeInfo(TScWebSocketCloseStatus), integer(FWebSocket.CloseStatus))]);

            Exit;
          end;

          Log.LogDebug(FLogger, SWebSocketsTransport_MessageReceived, [GetEnumName(TypeInfo(TScWebSocketMessageType), integer(MessageType)), Count]);

          if Count > 0 then begin
            FApplication.Output.Advance(Count);
            FlushResult := FApplication.Output.Flush;
            // We canceled in the middle of applying back pressure or if the consumer is done
            if FlushResult.IsCanceled or FlushResult.IsCompleted then
              Break;
          end;
        end;
      except
        on OperationCanceledException do
          Log.LogDebug(FLogger, STransport_ReceiveCanceled);

        on E: Exception do begin
          if not FAborted then begin
            FApplication.Output.Complete(CloneException(E));

            // We re-throw here so we can communicate that there was an error when sending the close frame
            raise;
          end;
        end;
      end;
    finally
      // We're done writing
      FApplication.Output.Complete;
    end;
  finally
    Log.LogDebug(FLogger, STransport_ReceiveStopped);
  end;
end;

procedure TScWebSocketsTransport.StartSending;
var
  HasError: boolean;
  ReadResult: TScReadResult;
  Position: TScSequencePosition;
  MemoryRef: TScMemoryRef;
  Count: integer;
  EndOfMessage: boolean;
begin
  HasError := False;

  try
    try
      try
        while True do begin
          ReadResult := FApplication.Input.Read;

          // Get a frame from the application
          try
            if ReadResult.IsCanceled then
              Break;

            if (ReadResult.Buffer <> nil) and not ReadResult.Buffer.IsEmpty then begin
              try
                Count := ReadResult.Buffer.Length;
                Log.LogDebug(FLogger, SWebSocketsTransport_ReceivedFromApp, [Count]);

                if WebSocketCanSend(FWebSocket) then begin
                  Position := ReadResult.Buffer.StartPos;
                  while ReadResult.Buffer.TryGet(Position, MemoryRef) do begin
                    EndOfMessage := Position.Obj = nil;
                    FWebSocket.Send(PByteArray(PtrOffset(MemoryRef.Memory, MemoryRef.Offset))^, MemoryRef.Length, FWebSocketMessageType, EndOfMessage);
                  end;
                end
                else
                  Break;
              except
                on E: Exception do begin
                  if not FAborted then
                    Log.LogError(FLogger, SWebSocketsTransport_ErrorSendingMessage, E);
                  Break;
                end;
              end;
            end
            else
            if ReadResult.IsCompleted then
              Break;
          finally
            if ReadResult.Buffer <> nil then
              FApplication.Input.AdvanceTo(ReadResult.Buffer.EndPos);
            ReadResult.Buffer.Free;
          end;
        end;
      except
        on E: Exception do
          HasError := True;
      end;
    finally
      if WebSocketCanSend(FWebSocket) then begin
        // We're done sending, send the close frame to the client if the websocket is still open
        if HasError then
          FWebSocket.Close(csInternalServerError, '')
        else
          FWebSocket.Close(csNormalClosure, '');
      end;

      FApplication.Input.Complete;
    end;
  finally
    Log.LogDebug(FLogger, STransport_SendStopped);
  end;
end;

function TScWebSocketsTransport.WebSocketCanSend(WebSocket: TScWebSocketClient): boolean;
begin
  Result := not (WebSocket.State in [sAborted, sClosed, sCloseSent]);
end;

function TScWebSocketsTransport.ResolveWebSocketsUrl(const Url: string): string;
var
  TmpUrl, Scheme: string;
begin
  TmpUrl := Url;
  Scheme := TScHttpParser.ParseScheme(TmpUrl);

  if SameText(Scheme, 'http:') then
    Scheme := 'ws:'
  else
  if SameText(Scheme, 'https:') then
    Scheme := 'wss:';

  Result := Scheme + TmpUrl;
end;

procedure TScWebSocketsTransport.Stop;
begin
  Log.LogInformation(FLogger, STransport_TransportStopping);

  if FApplication = nil then begin
    // We never started
    Exit;
  end;

  FTransport.Output.Complete;
  FTransport.Input.Complete;

  // Cancel any pending reads from the application, this should start the entire shutdown process
  FApplication.Input.CancelPendingRead;

  try
    FSendingThread.WaitFor;
    FReceivingThread.WaitFor;
  except
    on Exception do  begin
      Log.LogDebug(FLogger, STransport_TransportStopped);
      // exceptions have been handled in the Running task continuation by closing the channel with the exception
      Exit;
    end;
  end;

  Log.LogDebug(FLogger, STransport_TransportStopped);
end;

{ TScLongPollingReceivingLoop }

constructor TScLongPollingReceivingLoop.Create(ALongPollingTransport: TScLongPollingTransport; const AUrl: string);
begin
  FLongPollingTransport := ALongPollingTransport;
  FUrl := AUrl;

  inherited Create(False); /// Input\Output error(5) for non-Windows platforms
end;

procedure TScLongPollingReceivingLoop.Execute;
begin
  try
    try
      try
        FLongPollingTransport.Poll(FUrl, FLongPollingTransport.FCancellationToken);
      finally
        if not FLongPollingTransport.FSendingThread.FFinished then
          FLongPollingTransport.FApplication.Input.CancelPendingRead;
      end;
    finally
      FFinished := True;
    end;
  except
  end;
end;

{ TScLongPollingSendingLoop }

constructor TScLongPollingSendingLoop.Create(ALongPollingTransport: TScLongPollingTransport; const AUrl: string);
begin
  FLongPollingTransport := ALongPollingTransport;
  FUrl := AUrl;

  inherited Create(False); /// Input\Output error(5) for non-Windows platforms
end;

procedure TScLongPollingSendingLoop.Execute;
begin
  try
    try
      try
        FLongPollingTransport.SendMessages(FUrl);
      finally
        if not FLongPollingTransport.FReceivingThread.FFinished then begin
          // Cancel the poll request
          FLongPollingTransport.FCancellationToken.Cancel;

          // Cancel any pending flush so that we can quit
          FLongPollingTransport.FApplication.Output.CancelPendingFlush;

          // Send the DELETE request to clean-up the connection on the server
          FLongPollingTransport.SendDeleteRequest(FUrl);
        end;
      end;
    finally
      FFinished := True;
    end;
  except
  end;
end;

{ TScLongPollingTransport }

constructor TScLongPollingTransport.Create(AHttpClient: TScHttpClient; ALogger: TScLogger);
begin
  inherited Create;

  FLogger := ALogger;

  FHttpClient := AHttpClient;
  FCancellationToken := TScCancellationToken.Create;
end;

destructor TScLongPollingTransport.Destroy;
begin
  FSendingThread.Free;
  FReceivingThread.Free;
  FTransport.Free;
  FApplication.Free;
  FCancellationToken.Free;

  inherited;
end;

function TScLongPollingTransport.GetInput: TScPipeReader;
begin
  Result := FTransport.Input;
end;

function TScLongPollingTransport.GetOutput: TScPipeWriter;
begin
  Result := FTransport.Output;
end;

procedure TScLongPollingTransport.Start(const Url: string; TransferFormat: TScTransferFormat;
  CancellationToken: TScCancellationToken);
var
  Request: TScHttpRequestMessage;
  Response: TScHttpWebResponse;
  Pair: TScDuplexPipePair;
begin
  if (TransferFormat <> tfBinary) and (TransferFormat <> tfText) then
    raise ArgumentException.CreateFmt(STransferFormatNotSupported, [TScTransferFormatHelper.ToString(TransferFormat)]);

  Log.LogInformation(FLogger, SLongPollingTransport_StartTransport, [TScTransferFormatHelper.ToString(TransferFormat)]);

  // Make initial long polling request
  // Server uses first long polling request to finish initializing connection and it returns without data
  Request := TScHttpRequestMessage.Create(rmGet, Url);
  try
    Response := FHttpClient.Send(Request, CancellationToken);
    Response.Free;
  finally
    Request.Free;
  end;

  // Create the pipe pair (Application's writer is connected to Transport's reader, and vice versa)
  Pair := TScDuplexPipe.CreateConnectionPair(DefaultPipeOptions);
  FTransport := Pair.Transport;
  FApplication := Pair.Application;

  Process(Url);
end;

procedure TScLongPollingTransport.Process(const Url: string);
begin
  // Start sending and polling
  FReceivingThread := TScLongPollingReceivingLoop.Create(Self, Url);
  FSendingThread := TScLongPollingSendingLoop.Create(Self, Url);
end;

procedure TScLongPollingTransport.Stop;
begin
  Log.LogInformation(FLogger, STransport_TransportStopping);

  if FApplication = nil then begin
    // We never started
    Exit;
  end;

  FApplication.Input.CancelPendingRead;

  try
    FSendingThread.WaitFor;
    FReceivingThread.WaitFor;
  except
    Log.LogDebug(FLogger, STransport_TransportStopped);
    raise;
  end;

  FTransport.Output.Complete;
  FTransport.Input.Complete;
  Log.LogDebug(FLogger, STransport_TransportStopped);
end;

procedure TScLongPollingTransport.Poll(const PollUrl: string; CancellationToken: TScCancellationToken);
var
  Request: TScHttpRequestMessage;
  Response: TScHttpWebResponse;
  MemoryRef: TScMemoryRef;
  ReadCount: integer;
  FlushResult: TScFlushResult;
  Error: Exception;
begin
  Log.LogDebug(FLogger, SLongPollingTransport_StartReceive);

  Error := nil;
  try
    try
      try
        while not CancellationToken.IsCancellationRequested do begin
          Response := nil;
          try
            Request := TScHttpRequestMessage.Create(rmGet, PollUrl, 300{5 min});
            try
              try
                Request.AllowedStatuses := [scNoContent];
                Response := FHttpClient.Send(Request, CancellationToken);
              except
                on OperationCanceledException do begin
                  Log.LogDebug(FLogger, STransport_ReceiveCanceled);
                  Continue;
                end;
                on E: HttpException do begin
                  if E.Code = 504 {Timeout} then
                    // We want to start a new poll
                    Continue
                  else
                    raise;
                end;
                on SocketException do begin
                  // We want to start a new poll
                  Continue;
                end;
                on Exception do
                  raise;
              end;
            finally
              Request.Free;
            end;

            Log.LogDebug(FLogger, SLongPollingTransport_PollResponseReceived, [GetEnumName(TypeInfo(TScHttpStatusCode), integer(Response.StatusCode)), Response.ContentLength]);

            if (Response.StatusCode = scNoContent) or CancellationToken.IsCancellationRequested then begin
              Log.LogDebug(FLogger, SLongPollingTransport_ClosingConnection);

              // Transport closed or polling stopped, we're done
              Break;
            end
            else begin
              Log.LogDebug(FLogger, SLongPollingTransport_ReceivedMessages);

              repeat
                MemoryRef := FApplication.Output.GetMemory;
                ReadCount := Response.ReadBuffer(TValueArr(MemoryRef.Memory), MemoryRef.Offset, MemoryRef.Length);
                FApplication.Output.Advance(ReadCount);
              until ReadCount <= 0;

              FlushResult := FApplication.Output.Flush;
              // We canceled in the middle of applying back pressure or if the consumer is done
              if FlushResult.IsCanceled or FlushResult.IsCompleted then
                Break;
            end;
          finally
            Response.Free;
          end;
        end;
      except
        on OperationCanceledException do begin
          // transport is being closed
          Log.LogDebug(FLogger, STransport_ReceiveCanceled);
        end;
        on E: Exception do begin
          Log.LogError(FLogger, SLongPollingTransport_ErrorPolling, [PollUrl], E);
          Error := CloneException(E);
        end;
      end;

    finally
      FApplication.Output.Complete(Error);
    end;
  finally
    Log.LogDebug(FLogger, STransport_ReceiveStopped);
  end;
end;

procedure TScLongPollingTransport.SendDeleteRequest(const Url: string);
var
  Response: TScHttpWebResponse;
begin
  try
    Log.LogDebug(FLogger, SLongPollingTransport_SendingDeleteRequest, [Url]);
    Response := FHttpClient.Send(rmDelete, Url, [scNotFound]);
    try
      if Response.StatusCode = scNotFound then
        Log.LogDebug(FLogger, SLongPollingTransport_ConnectionAlreadyClosedSendingDeleteRequest, [Url])
      else
        Log.LogDebug(FLogger, SLongPollingTransport_DeleteRequestAccepted, [Url]);
    finally
      Response.Free;
    end;
  except
    on E: Exception do
      Log.LogError(FLogger, SLongPollingTransport_ErrorSendingDeleteRequest, [Url], E);
  end;
end;

procedure TScLongPollingTransport.SendMessages(const SendUrl: string; CancellationToken: TScCancellationToken = nil);
var
  Request: TScHttpRequestMessage;
  Response: TScHttpWebResponse;
  ReadResult: TScReadResult;
begin
  Log.LogDebug(FLogger, SLongPollingTransport_SendStarted);

  try
    try
      try
        while True do begin
          ReadResult := FApplication.Input.Read;

          try
            if ReadResult.IsCanceled then begin
              Log.LogDebug(FLogger, SLongPollingTransport_SendCanceled);
              Break;
            end
            else
            if (ReadResult.Buffer <> nil) and not ReadResult.Buffer.IsEmpty then begin
              Log.LogDebug(FLogger, SLongPollingTransport_SendingMessages, [ReadResult.Buffer.Length, SendUrl]);

              // Send them in a single post
              Request := TScHttpRequestMessage.Create(rmPost, SendUrl);
              try
                Request.Content := ReadResult.Buffer;

                // ResponseHeadersRead instructs Send to return once headers are read
                // rather than buffer the entire response. This gives a small perf boost.
                // Note that it is important to dispose of the response when doing this to
                // avoid leaving the connection open.
                Response := FHttpClient.Send(Request, CancellationToken);
                Response.Free;
              finally
                Request.Free;
              end;

              Log.LogDebug(FLogger, SLongPollingTransport_SentSuccessfully);
            end
            else
            if ReadResult.IsCompleted then
              Break
            else begin
              Log.LogDebug(FLogger, SLongPollingTransport_NoMessages);
            end;
          finally
            if ReadResult.Buffer <> nil then
              FApplication.Input.AdvanceTo(ReadResult.Buffer.EndPos);
            ReadResult.Buffer.Free;
          end;
        end;
      except
        on OperationCanceledException do
          Log.LogDebug(FLogger, SLongPollingTransport_SendCanceled);

        on E: Exception do begin
          Log.LogError(FLogger, SLongPollingTransport_ErrorSending, [SendUrl], E);
          raise;
        end;
      end;
    finally
      FApplication.Input.Complete;
    end;
  finally
    Log.LogDebug(FLogger, STransport_SendStopped);
  end;
end;

{ TScServerSentEventsTransport }

constructor TScServerSentEventsTransport.Create(AHttpClient: TScHttpClient; ALogger: TScLogger);
begin
  inherited Create;

  FLogger := ALogger;
  FHttpClient := AHttpClient;
end;

destructor TScServerSentEventsTransport.Destroy;
begin
  inherited;
end;

function TScServerSentEventsTransport.GetInput: TScPipeReader;
begin
  Result := nil;
end;

function TScServerSentEventsTransport.GetOutput: TScPipeWriter;
begin
  Result := nil;
end;

procedure TScServerSentEventsTransport.Start(const Url: string; TransferFormat: TScTransferFormat;
  CancellationToken: TScCancellationToken);
begin

end;

procedure TScServerSentEventsTransport.Stop;
begin

end;

end.
