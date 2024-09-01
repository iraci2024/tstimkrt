
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF DAC}
{$I SB.inc}
unit ScWebSocketClient;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF VER16P}
  System.Types,
{$ENDIF}
  Classes, SysUtils, SyncObjs,
  ScConsts, ScTypes, ScFunctions, ScCLRClasses, ScRNG,
  ScUtils, ScThread,
  ScHashAlgorithm, ScHash,
  ScBase64, ScBridge,
  ScVio, ScSSLClient, ScSecureConnection, ScHttp;

const
  DefMaxFragmentSize = 0;
  DefHeartBeatInterval = 15;
  DefHeartBeatTimeout = 120;
  DefWatchDogAttempts = 0;
  DefWatchDogInterval = -1;

type
  TScWebSocketClient = class;

  TScWebSocketFrameType = (ftContinue, ftText, ftBinary, ftClose, ftPing, ftPong);

  TScWebSocketMessageType = (mtBinary, mtText, mtClose);
  TScWebSocketControlMessageType = (cmtPing, cmtPong);

  TScOnWebSocketMessageEvent = procedure (Sender: TObject; const Data: TBytes; MessageType: TScWebSocketMessageType; EndOfMessage: boolean) of object;
  TScOnWebSocketControlMessageEvent = procedure (Sender: TObject; ControlMessageType: TScWebSocketControlMessageType) of object;

  TScWebSocketState = (sConnecting, sOpen, sCloseSent, sCloseReceived, sClosed, sAborted);

  TScWebSocketCloseStatus = (csNormalClosure, csEndpointUnavailable, csProtocolError,
    csUnsupportedData, csEmpty,  csAbnormalClosure, csInvalidPayloadData,
    csPolicyViolation, csMessageTooBig, csMandatoryExtension,
    csInternalServerError, csTLSHandshakeError,
    csUnknown);

  TScWebSocketDataFrame = class
    Data: TBytes;
    Offset: integer;
    MessageType: TScWebSocketMessageType;
    EndOfMessage: boolean;
  end;

  TScWebSocketOnMessageAsyncEvent = class(TScAsyncEvent)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TScWebSocketClient;
    FData: TBytes;
    FMessageType: TScWebSocketMessageType;
    FEndOfMessage: boolean;

  protected
    procedure InternalNotify; override;
  public
    constructor Create(Owner: TScWebSocketClient;
      const Data: TBytes; MessageType: TScWebSocketMessageType; EndOfMessage: boolean);
  end;

  TScWebSocketClientOptions = class(TPersistent)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TScWebSocketClient;

    FIPVersion: TIPVersion;
    FReadWriteTimeout: integer;
    FMillisecTimeout: cardinal;
    FCredentials: TScNetworkCredential;
    FOrigin: string;
    FSubProtocols: string;
    FExtensions: string;
    FCookies: string;
    FUserAgent: string;
    FRequestHeaders: TScWebHeaderCollection;
    FMaxFragmentSize: cardinal;

    procedure ReadRequestHeadersText(Reader: TReader);
    procedure WriteRequestHeadersText(Writer: TWriter);

    procedure SetIPVersion(const Value: TIPVersion);
    procedure SetReadWriteTimeout(const Value: integer);
    procedure SetCredentials(Value: TScNetworkCredential);
    procedure SetOrigin(const Value: string);
    procedure SetSubProtocols(const Value: string);
    procedure SetExtensions(const Value: string);
    procedure SetCookies(const Value: string);
    procedure SetUserAgent(const Value: string);
    procedure SetRequestHeaders(Value: TScWebHeaderCollection);

  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DefineProperties(Filer: TFiler); override;

  public
    constructor Create(Owner: TScWebSocketClient);
    destructor Destroy; override;

  published
    property IPVersion: TIPVersion read FIPVersion write SetIPVersion default DefValIPVersion;
    property ReadWriteTimeout: Integer read FReadWriteTimeout write SetReadWriteTimeout default DEFAULT_TIMEOUT;
    property Credentials: TScNetworkCredential read FCredentials write SetCredentials;
    property Origin: string read FOrigin write SetOrigin;
    property SubProtocols: string read FSubProtocols write SetSubProtocols;
    property Extensions: string read FExtensions write SetExtensions;
    property Cookies: string read FCookies write SetCookies;
    property UserAgent: string read FUserAgent write SetUserAgent;
    property RequestHeaders: TScWebHeaderCollection read FRequestHeaders write SetRequestHeaders;
    property MaxFragmentSize: cardinal read FMaxFragmentSize write FMaxFragmentSize default DefMaxFragmentSize;
  end;

  TScHeartBeatOptions = class(TPersistent)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TScWebSocketClient;

    FEnabled: boolean;
    FInterval: integer;
    FTimeout: integer;

    procedure SetEnabled(Value: boolean);
    procedure SetInterval(Value: integer);
    procedure SetTimeout(Value: integer);

  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Owner: TScWebSocketClient);

  published
    property Enabled: boolean read FEnabled write SetEnabled default False;
    property Interval: integer read FInterval write SetInterval default DefHeartBeatInterval;
    property Timeout: integer read FTimeout write SetTimeout default DefHeartBeatTimeout;
  end;

  TScWatchDogOptions = class(TPersistent)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TScWebSocketClient;

    FEnabled: boolean;
    FAttempts: integer;
    FInterval: integer;
    procedure SetEnabled(Value: boolean);
    procedure SetAttempts(Value: integer);
    procedure SetInterval(Value: integer);

  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Owner: TScWebSocketClient);

  published
    property Enabled: boolean read FEnabled write SetEnabled default False;
    property Attempts: integer read FAttempts write SetAttempts default DefWatchDogAttempts;
    property Interval: integer read FInterval write SetInterval default DefWatchDogInterval;
  end;

  TScWSConnectionIntervalThread = class(TThread)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TScWebSocketClient;

    FFinished: boolean;
    FIntervalEvent: TEvent;

  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TScWebSocketClient);
    destructor Destroy; override;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TScWebSocketClient = class(TComponent)
  private
    FRandom: IScRandom;
    FSecureConnection: TScSecureConnection;

    FRequestUri: string;
    FSecKeyClient: string;
    FSecKeyServer: string;
    FSubProtocolInUse: string;
    FExtensionsInUse: string;
    FCloseStatusCode: integer;
    FCloseStatusDescription: string;
    FState: TScWebSocketState;
    FLastSentEndOfMessage: boolean;
    FLastSentMessageType: TScWebSocketFrameType;
    FLastRecvMessageType: TScWebSocketMessageType;
    FEventsCallMode: TScEventCallMode;

    FIsMasked: boolean;
    FSendData: TBytes;
    FSendDataOffset: integer;
    FRecvData: TBytes;
    FRecvDataOffset: integer;
    FRecvDataCount: integer;
    FRecvDataFrames: TCRObjectList;
    FRecvDataFramesLock: TCriticalSection;
    FRecvDataFramesAvailable: TEvent;
    FRecvControlMessages: array of TScWebSocketControlMessageType;

    FPingMessageTime: cardinal;
    FPongMessageTime: cardinal;
    FPongFrameAvailable: TEvent;

    FIsAbnormalClosure: boolean;
    FCloseFrameAvailable: TEvent;
    FStateLock: TCriticalSection;
    FSendLock: TCriticalSection;
    FConnectionLock: TCriticalSection;

    FHashAlgorithm: THashAlgorithm;
    FResponseHeaders: TScWebHeaderCollection;
    FOptions: TScWebSocketClientOptions;
    FProxy: TScWebProxy;
    FSSLOptions: TScSSLClientOptions;
    FHeartBeatOptions: TScHeartBeatOptions;
    FWatchDogOptions: TScWatchDogOptions;
    FIntervalThread: TScWSConnectionIntervalThread;
    FLastException: Exception;

    FBeforeConnect: TNotifyEvent;
    FAfterConnect: TNotifyEvent;
    FAfterDisconnect: TNotifyEvent;
    FOnAsyncError: TScErrorEvent;
    FOnConnectFail: TNotifyEvent;
    FOnMessage: TScOnWebSocketMessageEvent;
    FOnControlMessage: TScOnWebSocketControlMessageEvent;

    procedure NotifyAfterDisconnect;
    procedure NotifyOnConnectFail;
    procedure NotifyOnAsyncError;
    procedure NotifyOnControlMessage;

    procedure CheckNewHeader(const Key, Value: string);
    function HasData: boolean;
    function CheckForData(Count: integer): boolean;
    procedure DefragRecvBuffer;

    function GetCloseStatus: TScWebSocketCloseStatus;
    function GetIsSecure: boolean;

    procedure SetEventsCallMode(Value: TScEventCallMode);
    procedure SetRequestUri(const Value: string);
    procedure SetOptions(Value: TScWebSocketClientOptions);
    procedure SetProxy(Value: TScWebProxy);
    procedure SetSSLOptions(Value: TScSSLClientOptions);
    procedure SetHeartBeatOptions(Value: TScHeartBeatOptions);
    procedure SetWatchDogOptions(Value: TScWatchDogOptions);

  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Notification(Component: TComponent; Operation: TOperation); override;

    procedure InitSecKey;
    procedure CheckActive;
    procedure CheckInactive;
    procedure MaskData(const Data: PByteArray; Count: integer; FrameType: TScWebSocketFrameType; IsFinal: boolean);
    function ReadAndParseFrame(out Data: TBytes; out FrameType: TScWebSocketFrameType; out IsFinal: boolean): boolean;
    procedure ProcessClose(const Data: TBytes);
    procedure Pong(const Data: TBytes); overload;
    procedure SendData(Data: PByteArray; Count: integer; FrameType: TScWebSocketFrameType; EndOfMessage: boolean);
    procedure SendCloseMessage(Status: TScWebSocketCloseStatus; const Description: string);

    procedure DoAsyncReceive(Sender: TObject);
    procedure DoAsyncError(E: Exception);
    procedure DoConnectFail;

    procedure ProcessMessage(const Data: TBytes; FrameType: TScWebSocketFrameType; EndOfMessage: boolean);
    procedure ProcessControlMessage(const Data: TBytes; FrameType: TScWebSocketFrameType);

    procedure DoAfterDisconnect(Sender: TObject);
    procedure UnlockEvents;
    procedure FreeConnection;

  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(const URL: string); reintroduce; overload;
    destructor Destroy; override;

    procedure Connect(const Uri: string; CancellationToken: TScCancellationToken = nil); overload;
    procedure Connect(CancellationToken: TScCancellationToken = nil); overload;
    procedure Close; overload;
    procedure Close(Status: TScWebSocketCloseStatus; const Description: string = ''); overload;
    procedure Abort;
    procedure Ping; overload;
    procedure Ping(const Data: TBytes); overload;
    procedure PingAsync;
    procedure Pong; overload;

    procedure Send(const Buffer; Count: integer; MessageType: TScWebSocketMessageType = mtBinary; EndOfMessage: boolean = True); overload;
    procedure Send(const Buffer: TBytes; Offset, Count: integer; MessageType: TScWebSocketMessageType = mtBinary; EndOfMessage: boolean = True); overload;
    procedure Send(const Str: string); overload;
    function Receive(const Buffer: PByteArray; Count: integer; out MessageType: TScWebSocketMessageType; out EndOfMessage: boolean): integer; overload;
    function Receive(const Buffer: TBytes; Offset, Count: integer; out MessageType: TScWebSocketMessageType; out EndOfMessage: boolean): integer; overload;

    procedure ReceiveMessage(Stream: TStream; MSecTimeout: cardinal; out MessageType: TScWebSocketMessageType); overload;
    function ReceiveMessage(MSecTimeout: cardinal; out MessageType: TScWebSocketMessageType): TBytes; overload;
    procedure ReceiveMessage(Stream: TStream; out MessageType: TScWebSocketMessageType); overload;
    function ReceiveMessage(out MessageType: TScWebSocketMessageType): TBytes; overload;

    property ResponseHeaders: TScWebHeaderCollection read FResponseHeaders;
    property SecWebSocketKey: string read FSecKeyClient;
    property SubProtocolInUse: string read FSubProtocolInUse;
    property ExtensionsInUse: string read FExtensionsInUse;

    property CloseStatus: TScWebSocketCloseStatus read GetCloseStatus;
    property CloseStatusDescription: string read FCloseStatusDescription;
    property State: TScWebSocketState read FState;
    property IsSecure: boolean read GetIsSecure;

  published
    property RequestUri: string read FRequestUri write SetRequestUri;
    property Options: TScWebSocketClientOptions read FOptions write SetOptions;
    property Proxy: TScWebProxy read FProxy write SetProxy;
    property SSLOptions: TScSSLClientOptions read FSSLOptions write SetSSLOptions;
    property HeartBeatOptions: TScHeartBeatOptions read FHeartBeatOptions write SetHeartBeatOptions;
    property WatchDogOptions: TScWatchDogOptions read FWatchDogOptions write SetWatchDogOptions;

    property EventsCallMode: TScEventCallMode read FEventsCallMode write SetEventsCallMode default ecAsynchronous;

    property BeforeConnect: TNotifyEvent read FBeforeConnect write FBeforeConnect;
    property AfterConnect: TNotifyEvent read FAfterConnect write FAfterConnect;
    property AfterDisconnect: TNotifyEvent read FAfterDisconnect write FAfterDisconnect;
    property OnAsyncError: TScErrorEvent read FOnAsyncError write FOnAsyncError;
    property OnConnectFail: TNotifyEvent read FOnConnectFail write FOnConnectFail;

    property OnMessage: TScOnWebSocketMessageEvent read FOnMessage write FOnMessage;
    property OnControlMessage: TScOnWebSocketControlMessageEvent read FOnControlMessage write FOnControlMessage;
  end;

  WebSocketException = class(Exception);

  TScWebSocketClientUtils = class
  public
    class function GetOptionsOwner(Obj: TScWebSocketClientOptions): TScWebSocketClient;
  end;

implementation

const
  // Headers
  SWebsocket = 'websocket';
  SUpgrade = 'Upgrade';
  SConnection = 'Connection';
  SOrigin = 'Origin';
  SSec_WebSocket_Key = 'Sec-WebSocket-Key';
  SSec_WebSocket_Accept = 'Sec-WebSocket-Accept';
  SSec_WebSocket_Protocol = 'Sec-WebSocket-Protocol';
  SSec_WebSocket_Version = 'Sec-WebSocket-Version';
  SSec_WebSocket_Extensions = 'Sec-WebSocket-Extensions';

resourcestring
  // Errors
  SChangingOptionClientOpened = 'Changing an option is not allowed when the client is open';
  SClientOpened = 'The operation is not allowed when the client is open';
  SClientOpening = 'The operation is not allowed when the client is opening';
  SClientClosed = 'The operation is not allowed when the client is closed';
  SServerNotUseWebSocket = 'WebSocket protocol is not used by the server';
  SHeaderFieldNotFound = '"%s" header field is not found';
  SHeaderFieldIncorrect = '"%s" header field is incorrect';
  SMaskedFrameFromServer = 'The client detects a masked frame from the server';
  SInvalidFrameType = 'Protocol error: frame type is invalid';
  SCallingReceiveProhibited  = 'Calling the Receive method is prohibited when the OnMessage event handler is set';
  SNotEnoughData = 'There is no enough data to read';
  STimeoutExpired = 'Timeout expired, server has not responded';
  SEmptyRequestUri = 'The RequestUri cannot be empty';
  SUseCloseMethod = 'Use the Close method to send the close message';
  SMessageTypeNotEqualPrev = 'The MessageType argument is not equal to the previous value';

const
  FRAME_TYPE_CODES: array [TScWebSocketFrameType] of byte = (
    0 {ftContinue},
    1 {ftText},
    2 {ftBinary},
    8 {ftClose},
    9 {ftPing},
    10{ftPong}
  );

  CLOSE_STATUS_CODES: array [TScWebSocketCloseStatus] of word = (
    1000 {csNormalClosure},
    1001 {csEndpointUnavailable},
    1002 {csProtocolError},
    1003 {csUnsupportedData},
    1005 {csEmpty},
    1006 {csAbnormalClosure},
    1007 {csInvalidPayloadData},
    1008 {csPolicyViolation},
    1009 {csMessageTooBig},
    1010 {csMandatoryExtension},
    1011 {csInternalServerError},
    1015 {csTLSHandshakeError},
       0 {csUnknown}
  );

function ConvertToFrameType(Value: byte): TScWebSocketFrameType;
begin
  for Result := Low(TScWebSocketFrameType) to High(TScWebSocketFrameType) do
    if Value = FRAME_TYPE_CODES[Result] then
      Exit;

  raise WebSocketException.CreateRes(@SInvalidFrameType);
end;

function ConvertToCloseStatus(Value: integer): TScWebSocketCloseStatus;
begin
  for Result := Low(TScWebSocketCloseStatus) to High(TScWebSocketCloseStatus) do
    if Value = CLOSE_STATUS_CODES[Result] then
      Exit;

  Result := csUnknown;
end;

{ TScWebSocketClientOptions }

constructor TScWebSocketClientOptions.Create(Owner: TScWebSocketClient);
begin
  inherited Create;

  Assert(Owner <> nil);
  FOwner := Owner;

  FIPVersion := DefValIPVersion;
  FReadWriteTimeout := DEFAULT_TIMEOUT;
  FMillisecTimeout := FReadWriteTimeout * 1000;
  FMaxFragmentSize := DefMaxFragmentSize;

  FCredentials := TScNetworkCredential.Create;
  FRequestHeaders := TScWebHeaderCollection.Create;
end;

destructor TScWebSocketClientOptions.Destroy;
begin
  FCredentials.Free;
  FRequestHeaders.Free;

  inherited;
end;

procedure TScWebSocketClientOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TScWebSocketClientOptions then begin
    TScWebSocketClientOptions(Dest).IPVersion := FIPVersion;
    TScWebSocketClientOptions(Dest).ReadWriteTimeout := FReadWriteTimeout;
    TScWebSocketClientOptions(Dest).Origin := FOrigin;
    TScWebSocketClientOptions(Dest).SubProtocols := FSubProtocols;
    TScWebSocketClientOptions(Dest).Extensions := FExtensions;
    TScWebSocketClientOptions(Dest).Cookies := FCookies;
    TScWebSocketClientOptions(Dest).UserAgent := FUserAgent;
    TScWebSocketClientOptions(Dest).MaxFragmentSize := FMaxFragmentSize;
    TScWebSocketClientOptions(Dest).FCredentials.Assign(FCredentials);
    TScWebSocketClientOptions(Dest).FRequestHeaders.Assign(FRequestHeaders);
  end
  else
    inherited;
end;

procedure TScWebSocketClientOptions.ReadRequestHeadersText(Reader: TReader);
begin
  RequestHeaders.Text := Reader.ReadString;
end;

procedure TScWebSocketClientOptions.WriteRequestHeadersText(Writer: TWriter);
begin
  Writer.WriteString(RequestHeaders.Text);
end;

procedure TScWebSocketClientOptions.DefineProperties(Filer: TFiler);
begin
  inherited;

  Filer.DefineProperty('RequestHeadersText', ReadRequestHeadersText, WriteRequestHeadersText, RequestHeaders.Text <> '');
end;

procedure TScWebSocketClientOptions.SetIPVersion(const Value: TIPVersion);
begin
  if FIPVersion <> Value then begin
    FOwner.CheckInactive;
    FIPVersion := Value;
  end;
end;

procedure TScWebSocketClientOptions.SetReadWriteTimeout(const Value: Integer);
begin
  if FReadWriteTimeout <> Value then begin
    FReadWriteTimeout := Value;

    if FReadWriteTimeout < 0 then
      FMillisecTimeout := cardinal(-1)
    else
    if (Int64(FReadWriteTimeout) * 1000) > MaxInt then
      FMillisecTimeout := MaxInt
    else
      FMillisecTimeout := FReadWriteTimeout * 1000;

    if FOwner.FSecureConnection <> nil then
      FOwner.FSecureConnection.SetReadWriteTimeout(Value);
  end;
end;

procedure TScWebSocketClientOptions.SetCredentials(Value: TScNetworkCredential);
begin
  if FCredentials <> Value then begin
    FOwner.CheckInactive;
    FCredentials.Assign(Value);
  end;
end;

procedure TScWebSocketClientOptions.SetOrigin(const Value: string);
begin
  if FOrigin <> Value then begin
    FOwner.CheckInactive;
    FOrigin := Value;
  end;
end;

procedure TScWebSocketClientOptions.SetSubProtocols(const Value: string);
begin
  if FSubProtocols <> Value then begin
    FOwner.CheckInactive;
    FSubProtocols := Value;
  end;
end;

procedure TScWebSocketClientOptions.SetExtensions(const Value: string);
begin
  if FExtensions <> Value then begin
    FOwner.CheckInactive;
    FExtensions := Value;
  end;
end;

procedure TScWebSocketClientOptions.SetCookies(const Value: string);
begin
  if FCookies <> Value then begin
    FOwner.CheckInactive;
    FCookies := Value;
  end;
end;

procedure TScWebSocketClientOptions.SetUserAgent(const Value: string);
begin
  if FUserAgent <> Value then begin
    FOwner.CheckInactive;
    FUserAgent := Value;
  end;
end;

procedure TScWebSocketClientOptions.SetRequestHeaders(Value: TScWebHeaderCollection);
begin
  if FRequestHeaders <> Value then begin
    FOwner.CheckInactive;
    FRequestHeaders.Assign(Value);
  end;
end;

{ TScHeartBeatOptions }

constructor TScHeartBeatOptions.Create(Owner: TScWebSocketClient);
begin
  inherited Create;

  Assert(Owner <> nil);
  FOwner := Owner;

  FEnabled := False;
  FInterval := DefHeartBeatInterval;
  FTimeout := DefHeartBeatTimeout;
end;

procedure TScHeartBeatOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TScHeartBeatOptions then begin
    TScHeartBeatOptions(Dest).Enabled := FEnabled;
    TScHeartBeatOptions(Dest).Interval := FInterval;
    TScHeartBeatOptions(Dest).Timeout := FTimeout;
  end
  else
    inherited;
end;

procedure TScHeartBeatOptions.SetEnabled(Value: boolean);
begin
  if FEnabled <> Value then begin
    FOwner.CheckInactive;
    FEnabled := Value;
  end;
end;

procedure TScHeartBeatOptions.SetInterval(Value: integer);
begin
  if FInterval <> Value then begin
    FOwner.CheckInactive;
    FInterval := Value;
  end;
end;

procedure TScHeartBeatOptions.SetTimeout(Value: integer);
begin
  if FTimeout <> Value then begin
    FOwner.CheckInactive;
    FTimeout := Value;
  end;
end;

{ TScWatchDogOptions }

constructor TScWatchDogOptions.Create(Owner: TScWebSocketClient);
begin
  inherited Create;

  Assert(Owner <> nil);
  FOwner := Owner;

  FEnabled := False;
  FAttempts := DefWatchDogAttempts; // max number of reconnects. If zero, then unlimited
  FInterval := DefWatchDogInterval; // seconds before reconnects. If -1, then random
end;

procedure TScWatchDogOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TScWatchDogOptions then begin
    TScWatchDogOptions(Dest).Enabled := FEnabled;
    TScWatchDogOptions(Dest).Attempts := FAttempts;
    TScWatchDogOptions(Dest).Interval := FInterval;
  end
  else
    inherited;
end;

procedure TScWatchDogOptions.SetEnabled(Value: boolean);
begin
  if FEnabled <> Value then begin
    FOwner.CheckInactive;
    FEnabled := Value;
  end;
end;

procedure TScWatchDogOptions.SetAttempts(Value: integer);
begin
  if FAttempts <> Value then begin
    FOwner.CheckInactive;
    FAttempts := Value;
  end;
end;

procedure TScWatchDogOptions.SetInterval(Value: integer);
begin
  if FInterval <> Value then begin
    FOwner.CheckInactive;
    FInterval := Value;
  end;
end;

{ TScWSConnectionIntervalThread }

constructor TScWSConnectionIntervalThread.Create(AOwner: TScWebSocketClient);
begin
  FOwner := AOwner;

  FIntervalEvent := CreateEvent;
  inherited Create(False); /// Input\Output error(5) for non-Windows platforms
end;

destructor TScWSConnectionIntervalThread.Destroy;
begin
  Terminate;
  FIntervalEvent.SetEvent;

  inherited;

  FIntervalEvent.Free;
end;

procedure TScWSConnectionIntervalThread.Execute;
var
  Attempts: integer;
  Interval: cardinal;
  HeartBeatCounter: integer;
  WasPong: boolean;
begin
  try
    Interval := INFINITE;
    Attempts := MaxInt;
    HeartBeatCounter := 0;

    while not Terminated do begin
      FIntervalEvent.WaitFor(Interval);
      FIntervalEvent.ResetEvent;
      if Terminated then
        Exit;

      if FOwner.HeartBeatOptions.Enabled and (FOwner.State = sOpen) then begin
        Attempts := MaxInt;
        if Interval = INFINITE then // previous step was WatchDog
          HeartBeatCounter := 0;

        Interval := 1000;
        Inc(HeartBeatCounter);
        WasPong := (FOwner.FPongMessageTime >= FOwner.FPingMessageTime) or
          ((FOwner.FPingMessageTime > $F0000000) and (FOwner.FPongMessageTime < $10000000) and (FOwner.FPongMessageTime <> 0));
        if not WasPong then
          if integer((GetTickInterval(FOwner.FPingMessageTime, GetTickCount) div 1000)) > FOwner.HeartBeatOptions.Timeout then begin
            try
              try
                FOwner.FIsAbnormalClosure := True;
                FOwner.SendCloseMessage(csAbnormalClosure, '');
              finally
                FOwner.FSecureConnection.Disconnect;
              end;
            except
            end;
            Continue;
          end;

        if HeartBeatCounter >= FOwner.HeartBeatOptions.Interval then begin
          if WasPong then
            FOwner.FPingMessageTime := GetTickCount;
          HeartBeatCounter := 0;

          try
            FOwner.PingAsync;
          except
            on E: Exception do
              try
                FOwner.DoAsyncError(E);
              except
              end;
          end;
        end;
      end
      else
      if FOwner.WatchDogOptions.Enabled and not Terminated then begin
        FOwner.FCloseFrameAvailable.WaitFor(INFINITE);
        if Terminated then
          Exit;

        if Attempts = MaxInt then begin
          if FOwner.WatchDogOptions.Attempts <= 0 then
            Attempts := MaxInt
          else
            Attempts := FOwner.WatchDogOptions.Attempts;

          // https://tools.ietf.org/html/rfc6455#section-7.2.3
          if FOwner.WatchDogOptions.Interval < 0 then begin
            FOwner.FRandom.Random(@Interval, 4);
            Interval := (Interval mod 5000) + 1; // 1..5 sec
          end
          else
            Interval := FOwner.WatchDogOptions.Interval * 1000;
        end
        else
          try
            Dec(Attempts);
            if FOwner.State <> sOpen then
              FOwner.Connect;
            Interval := INFINITE;
            Attempts := MaxInt;
          except
            on E: Exception do
              if FOwner.State = sOpen then begin // was opened from another thread
                Interval := INFINITE;
                Attempts := MaxInt;
              end
              else begin
                try
                  FOwner.DoAsyncError(E);
                except
                end;

                if Attempts <= 0 then begin
                  FOwner.DoConnectFail;
                  Exit;
                end;

                if (FOwner.WatchDogOptions.Interval < 0) and (Interval < 120000) then
                  Interval := Interval + (Interval shr 1);
              end;
          end;
      end
      else
        Interval := INFINITE;
    end;
  finally
    FFinished := True;
  end;
end;

{ TScWebSocketOnMessageAsyncEvent }

constructor TScWebSocketOnMessageAsyncEvent.Create(Owner: TScWebSocketClient;
  const Data: TBytes; MessageType: TScWebSocketMessageType; EndOfMessage: boolean);
begin
  inherited Create;

  FOwner := Owner;
  FData := Data;
  FMessageType := MessageType;
  FEndOfMessage := EndOfMessage;
end;

procedure TScWebSocketOnMessageAsyncEvent.InternalNotify;
begin
  if Assigned(FOwner.FOnMessage) then
    FOwner.FOnMessage(FOwner, FData, FMessageType, FEndOfMessage);
end;

{ TScWebSocketClient }

constructor TScWebSocketClient.Create(const URL: string);
begin
  Create(nil);

  FRequestUri := URL;
end;

constructor TScWebSocketClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  if Random = nil then
    raise Exception.Create(SInternalError);
  FRandom := Random;

  FEventsCallMode := ecAsynchronous;
  FIsMasked := True;
  FLastSentEndOfMessage := True;
  FState := sClosed;
  FSecKeyClient := '';
  FSecKeyServer := '';

  SetLength(FSendData, 1024);
  SetLength(FRecvData, UNBUFFERED_READ_MIN_SIZE);

  FRecvDataFrames := TCRObjectList.Create;
  FRecvDataFramesLock := TCriticalSection.Create;
  FRecvDataFramesAvailable := CreateEvent;
  FPongFrameAvailable := CreateEvent;
  FCloseFrameAvailable := CreateEvent;
  FStateLock := TCriticalSection.Create;
  FSendLock := TCriticalSection.Create;
  FConnectionLock := TCriticalSection.Create;

  FHashAlgorithm := THash_SHA1.Create;
  FResponseHeaders := TScWebHeaderCollection.Create;
  FOptions := TScWebSocketClientOptions.Create(Self);
  FProxy := TScWebProxy.Create;
  FSSLOptions := TScSSLClientOptions.Create(Self);
  FHeartBeatOptions := TScHeartBeatOptions.Create(Self);
  FWatchDogOptions := TScWatchDogOptions.Create(Self);
end;

destructor TScWebSocketClient.Destroy;
begin
  if FIntervalThread <> nil then begin
    FIntervalThread.Terminate;
    FState := sClosed;
    FIntervalThread.FIntervalEvent.SetEvent;
    FIntervalThread.WaitFor;
  end;

  FState := sClosed;
  FreeConnection;

  DisposeAsyncEvent(NotifyAfterDisconnect);
  DisposeAsyncEvent(NotifyOnConnectFail);
  DisposeAsyncEvent(NotifyOnAsyncError);
  DisposeAsyncEvent(NotifyOnControlMessage);
  FreeAndNil(FLastException);

  FRecvDataFrames.Free;
  FRecvDataFramesLock.Free;
  FRecvDataFramesAvailable.Free;
  FPongFrameAvailable.Free;
  FCloseFrameAvailable.Free;
  FStateLock.Free;
  FSendLock.Free;
  FConnectionLock.Free;

  FHashAlgorithm.Free;
  FResponseHeaders.Free;
  FOptions.Free;
  FProxy.Free;
  FSSLOptions.Free;
  FHeartBeatOptions.Free;
  FWatchDogOptions.Free;
  FIntervalThread.Free;

  inherited;
end;

procedure TScWebSocketClient.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScWebSocketClient) then begin
    TScWebSocketClient(Dest).RequestUri := RequestUri;
    TScWebSocketClient(Dest).EventsCallMode := EventsCallMode;

    TScWebSocketClient(Dest).FOptions.Assign(FOptions);
    TScWebSocketClient(Dest).FProxy.Assign(FProxy);
    TScWebSocketClient(Dest).FSSLOptions.Assign(FSSLOptions);
    TScWebSocketClient(Dest).FHeartBeatOptions.Assign(FHeartBeatOptions);
    TScWebSocketClient(Dest).FWatchDogOptions.Assign(FWatchDogOptions);
  end
  else
    inherited;
end;

procedure TScWebSocketClient.Notification(Component: TComponent; Operation: TOperation);
begin
  if (FSSLOptions <> nil) and (Component = FSSLOptions.Storage) and (Operation = opRemove) then
    FSSLOptions.Storage := nil;

  inherited;
end;

procedure TScWebSocketClient.InitSecKey;
var
  Buf: TBytes;
begin
  SetLength(Buf, 16);
  FRandom.Random(Buf, 0, 16);
  FSecKeyClient := Encoding.Default.GetString(TBase64.Encode(Buf));
  Buf := Encoding.Default.GetBytes(FSecKeyClient + '258EAFA5-E914-47DA-95CA-C5AB0DC85B11');
  Buf := FHashAlgorithm.ComputeHash(Buf);
  FSecKeyServer := Encoding.Default.GetString(TBase64.Encode(Buf));
end;

procedure TScWebSocketClient.CheckNewHeader(const Key, Value: string);
begin
  // None: to allow adding any header key
end;

procedure TScWebSocketClient.CheckActive;
begin
  if not (FState in [sOpen, sCloseReceived]) or (FSecureConnection = nil) then
    raise WebSocketException.CreateRes(@SClientClosed);
end;

procedure TScWebSocketClient.CheckInactive;
begin
  if not (FState in [sClosed, sAborted]) then
    raise WebSocketException.CreateRes(@SChangingOptionClientOpened);
end;

procedure TScWebSocketClient.FreeConnection;
begin
  if FSecureConnection <> nil then begin
    UnlockEvents;

    try
      FSecureConnection.Disconnect;
    except
    end;

    FSecureConnection.OnAsyncReceive := nil; // to make sure, that DoAsyncReceive will not be called more

    FConnectionLock.Acquire;
    FSendLock.Acquire;
    try
      FSecureConnection.Release;
      FSecureConnection := nil;
    finally
      FSendLock.Release;
      FConnectionLock.Release;
    end;
  end;
end;

procedure TScWebSocketClient.Connect(const Uri: string; CancellationToken: TScCancellationToken = nil);
begin
  RequestUri := Uri;
  Connect(CancellationToken);
end;

procedure TScWebSocketClient.Connect(CancellationToken: TScCancellationToken = nil);
var
  HttpWebRequest: TScHttpWebRequest;
  HttpWebResponse: TScHttpWebResponse;
  ConnectionVal, UpgradeVal, SecKeyVal: string;
  CookieArr: TStringArray;
  i: integer;
begin
  if FRequestUri = '' then
    raise WebSocketException.CreateRes(@SEmptyRequestUri);

  if FEventsCallMode = ecAsynchronous then
    CheckIfAsyncEventProcessorStarted;

  FConnectionLock.Acquire;
  try
    FStateLock.Acquire;
    try
      if not (FState in [sClosed, sAborted]) then
        raise WebSocketException.CreateRes(@SClientOpened);

      FreeAndNil(FLastException);
    finally
      FStateLock.Release;
    end;

    FreeConnection;

    FState := sConnecting;
    try
      FIsAbnormalClosure := False;
      FCloseFrameAvailable.ResetEvent;
      FRecvDataFramesAvailable.ResetEvent;
      FRecvDataFrames.Clear;
      SetLength(FRecvControlMessages, 0);
      FResponseHeaders.Clear;
      FCloseStatusCode := 0;
      FCloseStatusDescription := '';
      FExtensionsInUse := '';
      FSubProtocolInUse := '';

      if Assigned(BeforeConnect) then
        BeforeConnect(Self);

      if (FIntervalThread = nil) or not IsThread(FIntervalThread.ThreadID) then // if not Recovering
        InitSecKey;

      if FHeartBeatOptions.Enabled or FWatchDogOptions.Enabled then begin
        if (FIntervalThread <> nil) and FIntervalThread.FFinished then
          FreeAndNil(FIntervalThread);

        if FIntervalThread = nil then
          FIntervalThread := TScWSConnectionIntervalThread.Create(Self);

        FPingMessageTime := 0;
        FPongMessageTime := 0;
      end;

      HttpWebResponse := nil;
      HttpWebRequest := TScHttpWebRequest.Create(FRequestUri);
      try
        HttpWebRequest.ReadWriteTimeout := FOptions.ReadWriteTimeout;
        HttpWebRequest.Credentials := FOptions.Credentials;
        HttpWebRequest.Proxy := FProxy;
        HttpWebRequest.SSLOptions := FSSLOptions;
        HttpWebRequest.IPVersion := FOptions.IPVersion;

        if FOptions.Cookies <> '' then begin
          SetLength(CookieArr, 0);
          CookieArr := TScHttpParser.ParseTokens(FOptions.Cookies, ';');
          for i := 0 to Length(CookieArr) - 1 do
            HttpWebRequest.Cookies.Add(CookieArr[i]);
        end;

        HttpWebRequest.UserAgent := FOptions.UserAgent;
        HttpWebRequest.Headers.OnCheckNewHeader := CheckNewHeader;
        HttpWebRequest.Headers := FOptions.RequestHeaders;

        HttpWebRequest.Upgrade := SWebsocket;
        HttpWebRequest.Connection := SUpgrade;
        if FOptions.Origin <> '' then
          HttpWebRequest.Headers.Add(SOrigin, FOptions.Origin);

        HttpWebRequest.Headers.Add(SSec_WebSocket_Key, FSecKeyClient);

        if FOptions.SubProtocols <> '' then
          HttpWebRequest.Headers.Add(SSec_WebSocket_Protocol, FOptions.SubProtocols);

        HttpWebRequest.Headers.Add(SSec_WebSocket_Version, '13');

        if FOptions.Extensions <> '' then
          HttpWebRequest.Headers.Add(SSec_WebSocket_Extensions, FOptions.Extensions);

        HttpWebResponse := HttpWebRequest.GetResponse(CancellationToken);

        if HttpWebResponse.StatusCode <> scSwitchingProtocols then
          raise WebSocketException.Create(SServerNotUseWebSocket);

        if HttpWebResponse.Headers.TryGetValue(SUpgrade, UpgradeVal) then begin
          if not AnsiSameText(UpgradeVal, SWebsocket) then
            raise WebSocketException.CreateFmt(SHeaderFieldIncorrect, [SUpgrade]);
        end
        else
          raise WebSocketException.CreateFmt(SHeaderFieldNotFound, [SUpgrade]);

        if HttpWebResponse.Headers.TryGetValue(SConnection, ConnectionVal) then begin
          if TScHttpParser.FindToken(SUpgrade, ConnectionVal, ',') < 0 then
            raise WebSocketException.CreateFmt(SHeaderFieldIncorrect, [SConnection]);
        end
        else
          raise WebSocketException.CreateFmt(SHeaderFieldNotFound, [SConnection]);

        if HttpWebResponse.Headers.TryGetValue(SSec_WebSocket_Accept, SecKeyVal) then begin
          if not AnsiSameText(SecKeyVal, FSecKeyServer) then
            raise WebSocketException.CreateFmt(SHeaderFieldIncorrect, [SSec_WebSocket_Accept]);
        end
        else
          raise WebSocketException.CreateFmt(SHeaderFieldNotFound, [SSec_WebSocket_Accept]);

        if HttpWebResponse.Headers.TryGetValue(SSec_WebSocket_Extensions, FExtensionsInUse) then
          if TScHttpParser.FindToken(FExtensionsInUse, FOptions.Extensions, ',') < 0 then
            raise WebSocketException.CreateFmt(SHeaderFieldIncorrect, [SSec_WebSocket_Extensions]);

        if HttpWebResponse.Headers.TryGetValue(SSec_WebSocket_Protocol, FSubProtocolInUse) then
          if TScHttpParser.FindToken(FSubProtocolInUse, FOptions.SubProtocols, ',') < 0 then
            raise WebSocketException.CreateFmt(SHeaderFieldIncorrect, [SSec_WebSocket_Protocol]);

        FResponseHeaders.Assign(HttpWebResponse.Headers);

        FSecureConnection := TScHttpWebResponseHelper.GetSecureConnection(HttpWebResponse);
        TScHttpWebResponseHelper.SetSecureConnection(HttpWebResponse, nil);

        // Init async read
        FSecureConnection.AfterDisconnect := DoAfterDisconnect;
        FSecureConnection.OnAsyncReceive := DoAsyncReceive;
      finally
        HttpWebRequest.Free;
        HttpWebResponse.Free;
      end;

      FState := sOpen;

      if FHeartBeatOptions.Enabled then
        FIntervalThread.FIntervalEvent.SetEvent;
    except
      FState := sAborted;
      raise;
    end;
  finally
    FConnectionLock.Release;
  end;

  if Assigned(AfterConnect) then
    AfterConnect(Self);
end;

procedure TScWebSocketClient.Close;
begin
  Close(csNormalClosure);
end;

procedure TScWebSocketClient.Close(Status: TScWebSocketCloseStatus; const Description: string = '');
begin
  FConnectionLock.Acquire;
  try
    if FSecureConnection = nil then
      raise WebSocketException.CreateRes(@SClientClosed);

    try
      SendCloseMessage(Status, Description);

      if FState = sCloseSent then
        FCloseFrameAvailable.WaitFor(FOptions.FMillisecTimeout);
    finally
      FSecureConnection.Disconnect;
    end;
  finally
    FConnectionLock.Release;
  end;
end;

procedure TScWebSocketClient.SendCloseMessage(Status: TScWebSocketCloseStatus; const Description: string);
var
  Buf, DescrBuf: TBytes;
begin
  FStateLock.Acquire;
  try
    if FState in [sOpen, sCloseReceived] then begin
      FCloseStatusCode := CLOSE_STATUS_CODES[Status];
      FCloseStatusDescription := Description;

      SetLength(DescrBuf, 0);
      if Description <> '' then
        DescrBuf := Encoding.UTF8.GetBytes(Description);

      SetLength(Buf, Length(DescrBuf) + 2);
      Buf[0] := Byte(CLOSE_STATUS_CODES[Status] shr 8);
      Buf[1] := Byte(CLOSE_STATUS_CODES[Status]);
      if Length(DescrBuf) > 0 then
        Move(DescrBuf[0], Buf[2], Length(DescrBuf));

      try
        SendData(PByteArray(Buf), Length(Buf), ftClose, True);
      finally
        if (FIntervalThread <> nil) and not FIsAbnormalClosure then
          FIntervalThread.Terminate;

        if FState = sCloseReceived then
          FState := sClosed
        else
          FState := sCloseSent;
      end;
    end;
  finally
    FStateLock.Release;
  end;
end;

procedure TScWebSocketClient.ProcessClose(const Data: TBytes);
begin
  FStateLock.Acquire;
  try
    if (FIntervalThread <> nil) and not FIsAbnormalClosure then
      FIntervalThread.Terminate;

    if Length(Data) >= 2 then
      FCloseStatusCode := integer(Data[0] shl 8) + integer(Data[1])
    else
      FCloseStatusCode := 1005;

    if Length(Data) > 2 then
      FCloseStatusDescription := Encoding.UTF8.GetString(Data, 2, Length(Data) - 2)
    else
      FCloseStatusDescription := '';

    if FState = sCloseSent then
      FState := sClosed
    else
    if FState <> sClosed then
      FState := sCloseReceived;

    FCloseFrameAvailable.SetEvent;
  finally
    FStateLock.Release;
  end;
end;

procedure TScWebSocketClient.Abort;
begin
  FConnectionLock.Acquire;
  try
    if FSecureConnection <> nil then begin
      FStateLock.Acquire;
      try
        if FIntervalThread <> nil then
          FIntervalThread.Terminate;

        FState := sAborted;
      finally
        FStateLock.Release;
      end;

      try
        FSecureConnection.Disconnect;
      finally
        UnlockEvents;
      end;
    end;
  finally
    FConnectionLock.Release;
  end;
end;

procedure TScWebSocketClient.UnlockEvents;
begin
  FRecvDataFramesAvailable.SetEvent;
  FPongFrameAvailable.SetEvent;
  FCloseFrameAvailable.SetEvent;
end;

procedure TScWebSocketClient.DoAfterDisconnect(Sender: TObject);
begin
  try
    if (FIntervalThread <> nil) and not FWatchDogOptions.Enabled then
      FIntervalThread.Terminate;

    // https://tools.ietf.org/html/rfc6455#section-7.1.5
    if not (FState in [sCloseReceived, sClosed]) then begin
      FCloseStatusCode := 1006;
      FState := sAborted;
    end
    else
      FState := sClosed;

    UnlockEvents;

    if Assigned(AfterDisconnect) then
      case FEventsCallMode of
        ecDirectly:
          NotifyAfterDisconnect;
        ecAsynchronous:
          HandleEventAsync(NotifyAfterDisconnect);
        ecSynchronous:
          SynchronizeWithMainThread(NotifyAfterDisconnect);
      else
        Assert(False);
      end;
  finally
    if FIntervalThread <> nil then
      FIntervalThread.FIntervalEvent.SetEvent;
  end;
end;

procedure TScWebSocketClient.NotifyAfterDisconnect;
begin
  if Assigned(AfterDisconnect) then
    AfterDisconnect(Self);
end;

procedure TScWebSocketClient.DoConnectFail;
begin
  if Assigned(OnConnectFail) then
    case FEventsCallMode of
      ecDirectly:
        NotifyOnConnectFail;
      ecAsynchronous:
        HandleEventAsync(NotifyOnConnectFail);
      ecSynchronous:
        SynchronizeWithMainThread(NotifyOnConnectFail);
    else
      Assert(False);
    end;
end;

procedure TScWebSocketClient.NotifyOnConnectFail;
begin
  if Assigned(OnConnectFail) then
    OnConnectFail(Self);
end;

procedure TScWebSocketClient.Ping;
begin
  Ping(nil);
end;

procedure TScWebSocketClient.Ping(const Data: TBytes);
begin
  FPongFrameAvailable.ResetEvent;
  SendData(PByteArray(Data), Length(Data), ftPing, True);
  if (FPongFrameAvailable.WaitFor(FOptions.FMillisecTimeout) <> wrSignaled) or (FState in [sClosed, sAborted]) then
    raise WebSocketException.CreateRes(@STimeoutExpired);
end;

procedure TScWebSocketClient.PingAsync;
begin
  SendData(nil, 0, ftPing, True);
end;

procedure TScWebSocketClient.Pong;
begin
  SendData(nil, 0, ftPong, True);
end;

procedure TScWebSocketClient.Pong(const Data: TBytes);
begin
  SendData(PByteArray(Data), Length(Data), ftPong, True);
end;

procedure TScWebSocketClient.Send(const Buffer: TBytes; Offset, Count: integer;
  MessageType: TScWebSocketMessageType = mtBinary; EndOfMessage: boolean = True);
begin
  Send(Buffer[Offset], Count, MessageType, EndOfMessage);
end;

procedure TScWebSocketClient.Send(const Buffer; Count: integer;
  MessageType: TScWebSocketMessageType = mtBinary; EndOfMessage: boolean = True);
var
  FrameType: TScWebSocketFrameType;
begin
  case MessageType of
    mtText:
      FrameType := ftText;
    mtBinary:
      FrameType := ftBinary;
    mtClose:
      raise WebSocketException.Create(SUseCloseMethod);
  else
    raise WebSocketException.CreateFmt(SInvalidInputArg, ['MessageType']);
  end;

  SendData(PByteArray(@Buffer), Count, FrameType, EndOfMessage);
end;

procedure TScWebSocketClient.Send(const Str: string);
var
  Buf: TBytes;
begin
  Buf := Encoding.UTF8.GetBytes(Str);
  SendData(PByteArray(Buf), Length(Buf), ftText, True);
end;

procedure TScWebSocketClient.SendData(Data: PByteArray; Count: integer;
  FrameType: TScWebSocketFrameType; EndOfMessage: boolean);
var
  Cnt: integer;
begin
  FSendLock.Acquire;
  try
    CheckActive;

    if (Data = nil) and (Count > 0) then
      raise WebSocketException.CreateFmt(SInvalidInputArg, ['Data']);

    if FrameType in [ftClose, ftPing, ftPong] then begin
      FSendDataOffset := 0;
      MaskData(Data, Count, FrameType, True);
      FSecureConnection.Write(TValueArr(FSendData), 0, FSendDataOffset);
    end
    else begin
      if FrameType = ftContinue then
        raise WebSocketException.CreateFmt(SInvalidInputArg, ['FrameType']);

      if FLastSentEndOfMessage = False then begin
        if FLastSentMessageType <> FrameType then
          raise WebSocketException.Create(SMessageTypeNotEqualPrev);

        // FLastSentMessageType = FrameType
        FrameType := ftContinue;
      end
      else
        FLastSentMessageType := FrameType;

      FLastSentEndOfMessage := EndOfMessage;

      repeat
        if (FOptions.MaxFragmentSize > 0) and (cardinal(Count) > FOptions.MaxFragmentSize) then begin
          Cnt := FOptions.MaxFragmentSize;
          EndOfMessage := False;
        end
        else begin
          Cnt := Count;
          EndOfMessage := FLastSentEndOfMessage;
        end;

        FSendDataOffset := 0;
        MaskData(Data, Cnt, FrameType, EndOfMessage);
        FSecureConnection.Write(TValueArr(FSendData), 0, FSendDataOffset);
        FrameType := ftContinue;
        Inc(Data, Cnt);
        Dec(Count, Cnt);
      until Count = 0;
    end;
  finally
    FSendLock.Release;
  end;
end;

function TScWebSocketClient.Receive(const Buffer: TBytes; Offset, Count: integer;
  out MessageType: TScWebSocketMessageType; out EndOfMessage: boolean): integer;
begin
  Result := Receive(@Buffer[Offset], Count, MessageType, EndOfMessage);
end;

function TScWebSocketClient.Receive(const Buffer: PByteArray; Count: integer;
  out MessageType: TScWebSocketMessageType; out EndOfMessage: boolean): integer;
var
  Frame: TScWebSocketDataFrame;
  Offset: integer;
  DataCount: integer;
begin
  if FSecureConnection = nil then
    raise WebSocketException.CreateRes(@SClientClosed);

  if Assigned(FOnMessage) then
    raise WebSocketException.CreateRes(@SCallingReceiveProhibited);

  EndOfMessage := False;
  Result := 0;

  if (FState = sOpen) and (FRecvDataFramesAvailable.WaitFor(FOptions.FMillisecTimeout) <> wrSignaled) then
    Exit;

  FRecvDataFramesLock.Acquire;
  try
    if FRecvDataFrames.Count = 0 then
      raise WebSocketException.CreateRes(@SNotEnoughData);

    if Count = 0 then begin
      Frame := TScWebSocketDataFrame(FRecvDataFrames[0]);
      MessageType := Frame.MessageType;
      Exit;
    end;

    Offset := 0;
    while (Count > 0) and not EndOfMessage and (FRecvDataFrames.Count > 0) do begin
      Frame := TScWebSocketDataFrame(FRecvDataFrames[0]);
      MessageType := Frame.MessageType;

      DataCount := Length(Frame.Data) - Frame.Offset;
      if Count >= DataCount then begin
        EndOfMessage := Frame.EndOfMessage;
        if Length(Frame.Data) > 0 then // Data can be nil
          Move(Frame.Data[Frame.Offset], Buffer[Offset], DataCount);
        FRecvDataFrames.Delete(0);
        if FRecvDataFrames.Count = 0 then
          FRecvDataFramesAvailable.ResetEvent;
      end
      else begin
        DataCount := Count;
        EndOfMessage := False;
        if Length(Frame.Data) > 0 then
          Move(Frame.Data[Frame.Offset], Buffer[Offset], DataCount);
        Frame.Offset := Frame.Offset + DataCount;
      end;

      Inc(Result, DataCount);
      Inc(Offset, DataCount);
      Dec(Count, DataCount);
    end;
  finally
    FRecvDataFramesLock.Release;
  end;
end;

procedure TScWebSocketClient.ReceiveMessage(Stream: TStream; out MessageType: TScWebSocketMessageType);
begin
  ReceiveMessage(Stream, FOptions.FMillisecTimeout, MessageType);
end;

procedure TScWebSocketClient.ReceiveMessage(Stream: TStream; MSecTimeout: cardinal; out MessageType: TScWebSocketMessageType);
var
  Frame: TScWebSocketDataFrame;
  EndOfMessage: boolean;
begin
  if FSecureConnection = nil then
    raise WebSocketException.CreateRes(@SClientClosed);

  if Assigned(FOnMessage) then
    raise WebSocketException.CreateRes(@SCallingReceiveProhibited);

  if (FState = sOpen) and (FRecvDataFramesAvailable.WaitFor(MSecTimeout) <> wrSignaled) then
    raise WebSocketException.CreateRes(@SNotEnoughData);

  FRecvDataFramesLock.Acquire;
  try
    if FRecvDataFrames.Count = 0 then
      raise WebSocketException.CreateRes(@SNotEnoughData);

    repeat
      if FRecvDataFrames.Count = 0 then begin
        FRecvDataFramesLock.Release;
        try
          if FRecvDataFramesAvailable.WaitFor(MSecTimeout) <> wrSignaled then
            raise WebSocketException.CreateRes(@SNotEnoughData);
        finally
          FRecvDataFramesLock.Acquire;
        end;
      end;

      if FRecvDataFrames.Count = 0 then
        raise WebSocketException.CreateRes(@SNotEnoughData);

      Frame := TScWebSocketDataFrame(FRecvDataFrames[0]);
      MessageType := Frame.MessageType;
      EndOfMessage := Frame.EndOfMessage;

      if Length(Frame.Data) > 0 then // Data can be nil
        Stream.WriteBuffer(Frame.Data[Frame.Offset], Length(Frame.Data) - Frame.Offset);

      FRecvDataFrames.Delete(0);
      if FRecvDataFrames.Count = 0 then
        FRecvDataFramesAvailable.ResetEvent;
    until EndOfMessage;
  finally
    FRecvDataFramesLock.Release;
  end;
end;

function TScWebSocketClient.ReceiveMessage(out MessageType: TScWebSocketMessageType): TBytes;
begin
  Result := ReceiveMessage(FOptions.FMillisecTimeout, MessageType);
end;

function TScWebSocketClient.ReceiveMessage(MSecTimeout: cardinal; out MessageType: TScWebSocketMessageType): TBytes;
var
  Frame: TScWebSocketDataFrame;
  Offset, DataCount: integer;
  n, i: integer;
begin
  if FSecureConnection = nil then
    raise WebSocketException.CreateRes(@SClientClosed);

  if Assigned(FOnMessage) then
    raise WebSocketException.CreateRes(@SCallingReceiveProhibited);

  SetLength(Result, 0);
  if (FState = sOpen) and (FRecvDataFramesAvailable.WaitFor(MSecTimeout) <> wrSignaled) then
    raise WebSocketException.CreateRes(@SNotEnoughData);

  FRecvDataFramesLock.Acquire;
  try
    if FRecvDataFrames.Count = 0 then
      raise WebSocketException.CreateRes(@SNotEnoughData);

    DataCount := 0;
    n := 0;
    while True do begin
      if n = FRecvDataFrames.Count then begin
        FRecvDataFramesLock.Release;
        try
          if FRecvDataFramesAvailable.WaitFor(MSecTimeout) <> wrSignaled then
            raise WebSocketException.CreateRes(@SNotEnoughData);
        finally
          FRecvDataFramesLock.Acquire;
        end;
      end;

      if n = FRecvDataFrames.Count then
        raise WebSocketException.CreateRes(@SNotEnoughData);

      Frame := TScWebSocketDataFrame(FRecvDataFrames[n]);
      Inc(DataCount, Length(Frame.Data) - Frame.Offset);
      if Frame.EndOfMessage then
        break;

      Inc(n);
    end;

    SetLength(Result, DataCount);
    Offset := 0;
    for i := 0 to n do begin
      Frame := TScWebSocketDataFrame(FRecvDataFrames[0]);
      MessageType := Frame.MessageType;
      DataCount := Length(Frame.Data) - Frame.Offset;
      if Length(Frame.Data) > 0 then // Data can be nil
        Move(Frame.Data[Frame.Offset], Result[Offset], DataCount);

      Inc(Offset, DataCount);
      FRecvDataFrames.Delete(0);
      if FRecvDataFrames.Count = 0 then
        FRecvDataFramesAvailable.ResetEvent;
    end;
  finally
    FRecvDataFramesLock.Release;
  end;
end;

procedure TScWebSocketClient.ProcessMessage(const Data: TBytes; FrameType: TScWebSocketFrameType; EndOfMessage: boolean);
var
  Frame: TScWebSocketDataFrame;
  OnMessageAsyncEvent: TScWebSocketOnMessageAsyncEvent;
begin
  case FrameType of
    ftContinue:
      ; // None
    ftText:
      FLastRecvMessageType := mtText;
    ftBinary:
      FLastRecvMessageType := mtBinary;
    ftClose: begin
      FLastRecvMessageType := mtClose;
      ProcessClose(Data);
    end;
  else
    Assert(False);
  end;

  if Assigned(FOnMessage) then begin
    OnMessageAsyncEvent := TScWebSocketOnMessageAsyncEvent.Create(Self, Data, FLastRecvMessageType, EndOfMessage);
    OnMessageAsyncEvent.Notify(FEventsCallMode);
  end
  else begin
    Frame := TScWebSocketDataFrame.Create;
    Frame.Data := Data;
    Frame.Offset := 0;
    Frame.MessageType := FLastRecvMessageType;
    Frame.EndOfMessage := EndOfMessage;

    FRecvDataFramesLock.Acquire;
    try
      FRecvDataFrames.Add(Frame);
      FRecvDataFramesAvailable.SetEvent;
    finally
      FRecvDataFramesLock.Release;
    end;
  end;

  if FrameType = ftClose then begin
    try
      SendCloseMessage(ConvertToCloseStatus(FCloseStatusCode), FCloseStatusDescription);
    finally
      FSecureConnection.Disconnect;
    end;
  end;
end;

procedure TScWebSocketClient.ProcessControlMessage(const Data: TBytes; FrameType: TScWebSocketFrameType);
var
  ControlMessageType: TScWebSocketControlMessageType;
  Len: integer;
begin
  case FrameType of
    ftPing: begin
      ControlMessageType := cmtPing;
    end;
    ftPong: begin
      ControlMessageType := cmtPong;
      FPongMessageTime := GetTickCount;
      FPongFrameAvailable.SetEvent;
    end;
  else
    ControlMessageType := cmtPing;
    Assert(False);
  end;

  if (FEventsCallMode = ecDirectly) and Assigned(FOnControlMessage) then
    FOnControlMessage(Self, ControlMessageType)
  else begin
    FRecvDataFramesLock.Acquire;
    try
      Len := Length(FRecvControlMessages);
      SetLength(FRecvControlMessages, Len + 1);
      FRecvControlMessages[Len] := ControlMessageType;
    finally
      FRecvDataFramesLock.Release;
    end;

    if Assigned(FOnControlMessage) then
      if FEventsCallMode = ecAsynchronous then
        HandleEventAsync(NotifyOnControlMessage)
      else // ecSynchronous
        SynchronizeWithMainThread(NotifyOnControlMessage);
  end;

  if FrameType = ftPing then
    Pong(Data);
end;

procedure TScWebSocketClient.NotifyOnControlMessage;
var
  CopyRecvControlMessages: array of TScWebSocketControlMessageType;
  i: integer;
begin
  if not Assigned(FOnControlMessage) then
    Exit;

  FRecvDataFramesLock.Acquire;
  try
    if Length(FRecvControlMessages) = 0 then
      Exit;

    SetLength(CopyRecvControlMessages, Length(FRecvControlMessages));
    Move(FRecvControlMessages[0], CopyRecvControlMessages[0], Length(FRecvControlMessages) * sizeof(TScWebSocketControlMessageType));
    SetLength(FRecvControlMessages, 0);
  finally
    FRecvDataFramesLock.Release;
  end;

  for i := 0 to Length(CopyRecvControlMessages) - 1 do
    FOnControlMessage(Self, CopyRecvControlMessages[i]);
end;

procedure TScWebSocketClient.DoAsyncReceive(Sender: TObject);
var
  Data: TBytes;
  FrameType: TScWebSocketFrameType;
  IsFinal: boolean;
begin
  try
    repeat
      if not ReadAndParseFrame(Data, FrameType, IsFinal) then
        Exit;

      if FrameType in [ftContinue, ftText, ftBinary, ftClose] then
        ProcessMessage(Data, FrameType, IsFinal)
      else
        ProcessControlMessage(Data, FrameType);
    until not HasData;

  except
    on E: Exception do begin
      try
        DoAsyncError(E);

        FIsAbnormalClosure := True;
        SendCloseMessage(csAbnormalClosure, '');
      finally
        FSecureConnection.Disconnect;
      end;
    end;
  end;
end;

procedure TScWebSocketClient.DoAsyncError(E: Exception);
begin
  if Assigned(OnAsyncError) then begin
    FStateLock.Acquire;
    try
      FreeAndNil(FLastException);
      FLastException := CloneException(E);
    finally
      FStateLock.Release;
    end;

    case FEventsCallMode of
      ecDirectly:
        NotifyOnAsyncError;
      ecAsynchronous:
        HandleEventAsync(NotifyOnAsyncError);
      ecSynchronous:
        SynchronizeWithMainThread(NotifyOnAsyncError);
    else
      Assert(False);
    end;
  end;
end;

procedure TScWebSocketClient.NotifyOnAsyncError;
var
  E: Exception;
begin
  FStateLock.Acquire;
  try
    E := FLastException;
    FLastException := nil;
  finally
    FStateLock.Release;
  end;

  try
    if Assigned(OnAsyncError) and Assigned(E) then
      OnAsyncError(Self, E);
  finally
    E.Free;
  end;
end;

function TScWebSocketClient.HasData: boolean;
begin
  Result := (FRecvDataCount > FRecvDataOffset) or FSecureConnection.WaitForData(0);
end;

function TScWebSocketClient.CheckForData(Count: integer): boolean;
var
  Cnt: integer;
begin
  if FRecvDataOffset + Count > Length(FRecvData) then
    if FRecvDataOffset + Count > Length(FRecvData) + UNBUFFERED_READ_MIN_SIZE then
      SetLength(FRecvData, FRecvDataOffset + Count)
    else
      SetLength(FRecvData, Length(FRecvData) + UNBUFFERED_READ_MIN_SIZE);

  while FRecvDataOffset + Count > FRecvDataCount do begin
    if not FSecureConnection.WaitForData(0) then begin
      Result := False;
      Exit;
    end;

    Cnt := FSecureConnection.Read(TValueArr(FRecvData), FRecvDataCount, Length(FRecvData) - FRecvDataCount);
    if Cnt <= 0 then
      FSecureConnection.RaiseLastError;
    Inc(FRecvDataCount, Cnt);
  end;

  Result := True;
end;

procedure TScWebSocketClient.DefragRecvBuffer;
begin
  if FRecvDataOffset = FRecvDataCount then begin
    FRecvDataOffset := 0;
    FRecvDataCount := 0;
  end
  else
  if FRecvDataOffset >= UNBUFFERED_READ_MIN_SIZE then begin // performance increase
    Move(FRecvData[FRecvDataOffset], FRecvData[0], FRecvDataCount - FRecvDataOffset);
    Dec(FRecvDataCount, FRecvDataOffset);
    FRecvDataOffset := 0;
  end;
end;

function TScWebSocketClient.ReadAndParseFrame(out Data: TBytes;
  out FrameType: TScWebSocketFrameType; out IsFinal: boolean): boolean;
var
  Mask: array[0..3] of byte;
  IsMasked: boolean;
  Opcode, PayloadLen: byte;
  DataCount, BlockCount: integer;
  DstOffset: integer;
  OldRecvDataOffset: integer;
  i: integer;
begin
  OldRecvDataOffset := FRecvDataOffset;
  Result := False;
  if not CheckForData(2) then begin
    FRecvDataOffset := OldRecvDataOffset;
    Exit;
  end;

  IsFinal := (FRecvData[FRecvDataOffset] and $80) <> 0;
  Opcode := FRecvData[FRecvDataOffset] and $0F;
  FrameType := ConvertToFrameType(Opcode);

  IsMasked := (FRecvData[FRecvDataOffset + 1] and $80) <> 0;
  if IsMasked then
    raise WebSocketException.CreateRes(@SMaskedFrameFromServer);

  PayloadLen := FRecvData[FRecvDataOffset + 1] and $7F;
  Inc(FRecvDataOffset, 2);

  if PayloadLen = 127 then begin
    if not CheckForData(8) then begin
      FRecvDataOffset := OldRecvDataOffset;
      Exit;
    end;

    DataCount := integer(FRecvData[FRecvDataOffset + 4] shl 24) +
                 integer(FRecvData[FRecvDataOffset + 5] shl 16) +
                 integer(FRecvData[FRecvDataOffset + 6] shl 8) +
                 integer(FRecvData[FRecvDataOffset + 7]);
    Inc(FRecvDataOffset, 8);
  end
  else
  if PayloadLen = 126 then begin
    if not CheckForData(2) then begin
      FRecvDataOffset := OldRecvDataOffset;
      Exit;
    end;

    DataCount := integer(FRecvData[FRecvDataOffset] shl 8) + integer(FRecvData[FRecvDataOffset + 1]);
    Inc(FRecvDataOffset, 2);
  end
  else
    DataCount := PayloadLen;

  SetLength(Data, DataCount);

  if IsMasked then begin
    if not CheckForData(DataCount + 4) then begin
      FRecvDataOffset := OldRecvDataOffset;
      Exit;
    end;

    Move(FRecvData[FRecvDataOffset], Mask[0], 4);
    Inc(FRecvDataOffset, 4);

    DstOffset := 0;
    BlockCount := DataCount shr 2; // div 4
    for i := 0 to BlockCount - 1 do begin
      Data[DstOffset] := FRecvData[FRecvDataOffset] xor Mask[0];
      Data[DstOffset + 1] := FRecvData[FRecvDataOffset + 1] xor Mask[1];
      Data[DstOffset + 2] := FRecvData[FRecvDataOffset + 2] xor Mask[2];
      Data[DstOffset + 3] := FRecvData[FRecvDataOffset + 3] xor Mask[3];
      Inc(DstOffset, 4);
      Inc(FRecvDataOffset, 4);
    end;

    DataCount := DataCount and 3;
    if DataCount = 3 then begin
      Data[DstOffset] := FRecvData[FRecvDataOffset] xor Mask[0];
      Data[DstOffset + 1] := FRecvData[FRecvDataOffset + 1] xor Mask[1];
      Data[DstOffset + 2] := FRecvData[FRecvDataOffset + 2] xor Mask[2];
      Inc(FRecvDataOffset, 3);
    end
    else
    if DataCount = 2 then begin
      Data[DstOffset] := FRecvData[FRecvDataOffset] xor Mask[0];
      Data[DstOffset + 1] := FRecvData[FRecvDataOffset + 1] xor Mask[1];
      Inc(FRecvDataOffset, 2);
    end
    else
    if DataCount = 1 then begin
      Data[DstOffset] := FRecvData[FRecvDataOffset] xor Mask[0];
      Inc(FRecvDataOffset);
    end;
  end
  else
    if DataCount > 0 then begin
      if not CheckForData(DataCount) then begin
        FRecvDataOffset := OldRecvDataOffset;
        Exit;
      end;

      Move(FRecvData[FRecvDataOffset], Data[0], DataCount);
      Inc(FRecvDataOffset, DataCount);
    end;

  DefragRecvBuffer;
  Result := True;
end;

procedure TScWebSocketClient.MaskData(const Data: PByteArray; Count: integer;
  FrameType: TScWebSocketFrameType; IsFinal: boolean);
var
  Mask: array[0..3] of byte;
  MaskBit: byte;
  MaskedDataCount, BlockCount: integer;
  Offset: integer;
  i: integer;
begin
  if Count > $FFFF then
    MaskedDataCount := 8 + 2
  else
  if Count > 125 then
    MaskedDataCount := 2 + 2
  else
    MaskedDataCount := 0 + 2;

  if FIsMasked then begin
    MaskBit := $80;
    Inc(MaskedDataCount, 4);
  end
  else
    MaskBit := 0;

  Inc(MaskedDataCount, Count);

  if MaskedDataCount > Length(FSendData) then
    SetLength(FSendData, MaskedDataCount);

  if IsFinal then
    FSendData[FSendDataOffset] := $80 or FRAME_TYPE_CODES[FrameType]
  else
    FSendData[FSendDataOffset] := FRAME_TYPE_CODES[FrameType];

  if Count > $FFFF then begin
    FSendData[FSendDataOffset + 1] := MaskBit or 127;
    FSendData[FSendDataOffset + 2] := 0;
    FSendData[FSendDataOffset + 3] := 0;
    FSendData[FSendDataOffset + 4] := 0;
    FSendData[FSendDataOffset + 5] := 0;
    FSendData[FSendDataOffset + 6] := Byte(Count shr 24);
    FSendData[FSendDataOffset + 7] := Byte(Count shr 16);
    FSendData[FSendDataOffset + 8] := Byte(Count shr 8);
    FSendData[FSendDataOffset + 9] := Byte(Count);
    Inc(FSendDataOffset, 10);
  end
  else
  if Count > 125 then begin
    FSendData[FSendDataOffset + 1] := MaskBit or 126;
    FSendData[FSendDataOffset + 2] := Byte(Count shr 8);
    FSendData[FSendDataOffset + 3] := Byte(Count);
    Inc(FSendDataOffset, 4);
  end
  else begin
    FSendData[FSendDataOffset + 1] := MaskBit or byte(Count);
    Inc(FSendDataOffset, 2);
  end;

  if FIsMasked then begin
    FRandom.Random(@Mask, 4);
    Move(Mask[0], FSendData[FSendDataOffset], 4);
    Inc(FSendDataOffset, 4);

    Offset := 0;
    BlockCount := Count shr 2; // div 4
    for i := 0 to BlockCount - 1 do begin
      FSendData[FSendDataOffset] := Data[Offset] xor Mask[0];
      FSendData[FSendDataOffset + 1] := Data[Offset + 1] xor Mask[1];
      FSendData[FSendDataOffset + 2] := Data[Offset + 2] xor Mask[2];
      FSendData[FSendDataOffset + 3] := Data[Offset + 3] xor Mask[3];
      Inc(FSendDataOffset, 4);
      Inc(Offset, 4);
    end;

    Count := Count and 3;
    if Count = 3 then begin
      FSendData[FSendDataOffset] := Data[Offset] xor Mask[0];
      FSendData[FSendDataOffset + 1] := Data[Offset + 1] xor Mask[1];
      FSendData[FSendDataOffset + 2] := Data[Offset + 2] xor Mask[2];
      Inc(FSendDataOffset, 3);
    end
    else
    if Count = 2 then begin
      FSendData[FSendDataOffset] := Data[Offset] xor Mask[0];
      FSendData[FSendDataOffset + 1] := Data[Offset + 1] xor Mask[1];
      Inc(FSendDataOffset, 2);
    end
    else
    if Count = 1 then begin
      FSendData[FSendDataOffset] := Data[Offset] xor Mask[0];
      Inc(FSendDataOffset);
    end;
  end
  else
    if Count > 0 then begin
      Move(Data[0], FSendData[FSendDataOffset], Count);
      Inc(FSendDataOffset, Count);
    end;
end;

function TScWebSocketClient.GetCloseStatus: TScWebSocketCloseStatus;
begin
  Result := ConvertToCloseStatus(FCloseStatusCode);
end;

function TScWebSocketClient.GetIsSecure: boolean;
begin
  if FSecureConnection <> nil then
    Result := FSecureConnection.IsSecure
  else
    Result := False;
end;

procedure TScWebSocketClient.SetEventsCallMode(Value: TScEventCallMode);
begin
  if Value <> FEventsCallMode then begin
    CheckInactive;
    FEventsCallMode := Value;
  end;
end;

procedure TScWebSocketClient.SetRequestUri(const Value: string);
begin
  if FRequestUri <> Value then begin
    CheckInactive;
    FRequestUri := Value;
  end;
end;

procedure TScWebSocketClient.SetOptions(Value: TScWebSocketClientOptions);
begin
  if FOptions <> Value then begin
    CheckInactive;
    FOptions.Assign(Value);
  end;
end;

procedure TScWebSocketClient.SetProxy(Value: TScWebProxy);
begin
  if FProxy <> Value then begin
    CheckInactive;
    FProxy.Assign(Value);
  end;
end;

procedure TScWebSocketClient.SetSSLOptions(Value: TScSSLClientOptions);
begin
  if FSSLOptions <> Value then begin
    CheckInactive;
    FSSLOptions.Assign(Value);
  end;
end;

procedure TScWebSocketClient.SetHeartBeatOptions(Value: TScHeartBeatOptions);
begin
  if FHeartBeatOptions <> Value then begin
    CheckInactive;
    FHeartBeatOptions.Assign(Value);
  end;
end;

procedure TScWebSocketClient.SetWatchDogOptions(Value: TScWatchDogOptions);
begin
  if FWatchDogOptions <> Value then begin
    CheckInactive;
    FWatchDogOptions.Assign(Value);
  end;
end;

{ TScWebSocketClientUtils }

class function TScWebSocketClientUtils.GetOptionsOwner(Obj: TScWebSocketClientOptions): TScWebSocketClient;
begin
  Result := Obj.FOwner;
end;

end.
