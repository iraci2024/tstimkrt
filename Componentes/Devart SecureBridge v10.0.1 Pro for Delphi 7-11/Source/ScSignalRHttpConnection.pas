//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2018-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScSignalRHttpConnection;

interface

uses
  Classes, SysUtils, SyncObjs,
  StrUtils,
  ScTypes, ScFunctions, ScCLRClasses, ScUtils,
  ScJson, ScSecureConnection, ScHttp, ScSSLClient,
  ScPipe,
  ScSignalRConsts, ScSignalRProtocol;

type
  TScHttpTransportType = (ttWebSockets, ttLongPolling{, ttServerSentEvents});
  TScHttpTransportTypes = set of TScHttpTransportType;

const
  DefValCloseTimeout = 5000;
  DefValTransports = [ttWebSockets, ttLongPolling];

type
  TScGetString = function: string of object;

  TScTransport = class
  protected
    function GetInput: TScPipeReader; virtual; abstract;
    function GetOutput: TScPipeWriter; virtual; abstract;

  public
    procedure Start(const Url: string; TransferFormat: TScTransferFormat;
      CancellationToken: TScCancellationToken); virtual; abstract;
    procedure Stop; virtual; abstract;

    property Input: TScPipeReader read GetInput;
    property Output: TScPipeWriter read GetOutput;
  end;

  TScHttpTransportTypeHelper = class
  public
    class function ToString(const Value: TScHttpTransportType): string; {$IFDEF VER12P}reintroduce;{$ELSE}{$IFDEF FPC}reintroduce;{$ENDIF}{$ENDIF} overload;
    class function ToString(const Value: TScHttpTransportTypes): string; {$IFDEF VER12P}reintroduce;{$ELSE}{$IFDEF FPC}reintroduce;{$ENDIF}{$ENDIF} overload;
    class function Parse(const Value: string; out TransportType: TScHttpTransportType): boolean;
  end;

  TScHttpConnectionOptions = class(TPersistent)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TComponent;

    FCookies: TStringList;
    FCredentials: TScNetworkCredential;
    FHeaders: TScWebHeaderCollection;
    FProxy: TScWebProxy;
    FSSLOptions: TScSSLClientOptions;
    FUrl: string;
    FCloseTimeout: cardinal;
    FTransports: TScHttpTransportTypes;

    FAccessTokenProvider: TScGetString;
    FSkipNegotiation: boolean;

    procedure SetCookies(Value: TStringList);
    procedure SetCredentials(Value: TScNetworkCredential);
    procedure SetHeaders(Value: TScWebHeaderCollection);
    procedure SetProxy(Value: TScWebProxy);
    procedure SetSSLOptions(Value: TScSSLClientOptions);

    procedure ReadHeadersText(Reader: TReader);
    procedure WriteHeadersText(Writer: TWriter);

  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DefineProperties(Filer: TFiler); override;

  public
    constructor Create(Owner: TComponent);
    destructor Destroy; override;

    property AccessTokenProvider: TScGetString read FAccessTokenProvider write FAccessTokenProvider;
    property SkipNegotiation: boolean read FSkipNegotiation write FSkipNegotiation;

  published
    property Cookies: TStringList read FCookies write SetCookies;
    property Credentials: TScNetworkCredential read FCredentials write SetCredentials;
    property Headers: TScWebHeaderCollection read FHeaders write SetHeaders;
    property Proxy: TScWebProxy read FProxy write SetProxy;
    property SSLOptions: TScSSLClientOptions read FSSLOptions write SetSSLOptions;

    property Url: string read FUrl write FUrl;
    property CloseTimeout: cardinal read FCloseTimeout write FCloseTimeout default DefValCloseTimeout;
    property Transports: TScHttpTransportTypes read FTransports write FTransports default DefValTransports;
  end;

  TScHttpRequestMessage = class
  private
    FAllowedStatuses: TScHttpStatusCodes;
    FContent: TScReadOnlySequence;
    FMethod: TScRequestMethod;
    FUrl: string;
    FTimeout: integer;
  public
    constructor Create(Method: TScRequestMethod; const Url: string); overload;
    constructor Create(Method: TScRequestMethod; const Url: string; Timeout: integer); overload;

    property AllowedStatuses: TScHttpStatusCodes read FAllowedStatuses write FAllowedStatuses;
    property Content: TScReadOnlySequence read FContent write FContent;
    property Method: TScRequestMethod read FMethod;
    property Url: string read FUrl;
    property Timeout: integer read FTimeout write FTimeout;
  end;

  TScReadOnlySequenceReader = class
  private
    FReadOnlySequence: TScReadOnlySequence;
    FPosition: TScSequencePosition;
    procedure OnGetNextChunkData(Sender: TObject; out Buffer: TValueArr; out Count: Integer);
  public
    constructor Create(ReadOnlySequence: TScReadOnlySequence);
  end;

  TScHttpClient = class
  private
    FConnectionGroupName: string;
    FHttpConnectionOptions: TScHttpConnectionOptions;
    FBaseAddress: string;
    FDefaultRequestHeaders: TScWebHeaderCollection;
    FTimeout: integer;
    FVersion: TScVersion;
    FBeforeSendRequest: TNotifyEvent;
    FAfterSendRequest: TNotifyEvent;

    procedure CheckNewHeader(const Key, Value: string);

  public
    constructor Create(AHttpConnectionOptions: TScHttpConnectionOptions);
    destructor Destroy; override;

    function Send(Request: TScHttpRequestMessage; CancellationToken: TScCancellationToken = nil): TScHttpWebResponse; overload;
    function Send(const Method: TScRequestMethod; const Url: string;
      const AllowedStatuses: TScHttpStatusCodes; CancellationToken: TScCancellationToken = nil): TScHttpWebResponse; overload;

    property BaseAddress: string read FBaseAddress write FBaseAddress;
    property DefaultRequestHeaders: TScWebHeaderCollection read FDefaultRequestHeaders;
    property Timeout: integer read FTimeout write FTimeout;
    property BeforeSendRequest: TNotifyEvent read FBeforeSendRequest write FBeforeSendRequest;
    property AfterSendRequest: TNotifyEvent read FAfterSendRequest write FAfterSendRequest;
  end;

  TScAvailableTransport = record
    Transport: string;
    TransferFormats: TStringArray;
  end;

  TScAvailableTransports = array of TScAvailableTransport;

  TScNegotiationResponse = class
  public
    Url: string;
    AccessToken: string;
    ConnectionId: string;
    Error: string;
    AvailableTransports: TScAvailableTransports;
  end;

  TScNegotiateProtocol = class
  private
    class procedure ParseAvailableTransport(Reader: TJSONTextReader; var AvailableTransport: TScAvailableTransport);
  public
    class procedure WriteResponse(Response: TScNegotiationResponse; out Output: TBytes);
    class function ParseResponse(const Content: TBytes): TScNegotiationResponse;
  end;

  TScHttpConnection = class
  private
    FLogger: TScLogger;
    FConnectionLock: TCriticalSection;
    FStarted: boolean;
    FDisposed: boolean;
    FTransport: TScTransport;
    FHttpClient: TScHttpClient;
    FHttpConnectionOptions: TScHttpConnectionOptions;
    FHttpClientTimeout: integer;
    FMaxRedirects: integer;
    FAccessTokenProvider: TScGetString;
    FCurAccessToken: string;
    FConnectionId: string;
    FHasInherentKeepAlive: boolean;

    function CreateTransport(AvailableServerTransport: TScHttpTransportType): TScTransport;
    procedure SelectAndStartTransport(TransferFormat: TScTransferFormat; CancellationToken: TScCancellationToken);
    function GetNegotiationResponse(const Url: string; CancellationToken: TScCancellationToken): TScNegotiationResponse;
    function Negotiate(const Url: string; CancellationToken: TScCancellationToken): TScNegotiationResponse;
    function CreateConnectUrl(const Url, ConnectionId: string): string;
    procedure StartTransport(const ConnectUrl: string; TransportType: TScHttpTransportType;
      TransferFormat: TScTransferFormat; CancellationToken: TScCancellationToken);
    function CreateHttpClient: TScHttpClient;
    procedure BeforeSendRequestDoAccessToken(Sender: TObject);

    procedure CheckDisposed;

    class function AppendQueryString(const Url, NewQuery: string): string;

    function GetCurAccessToken: string;
    function GetNoAccessToken: string;
    function GetAccessToken: string;

    function GetTransport: TScTransport;

  public
    constructor Create(AHttpConnectionOptions: TScHttpConnectionOptions; ALogger: TScLogger);
    destructor Destroy; override;

    procedure Start(TransferFormat: TScTransferFormat; CancellationToken: TScCancellationToken);
    procedure Dispose;

    property ConnectionId: string read FConnectionId;
    property HasInherentKeepAlive: boolean read FHasInherentKeepAlive;
    property Transport: TScTransport read GetTransport;
  end;

  TScHttpConnectionOptionsUtils = class
  public
    class function GetOptionsOwner(Obj: TScHttpConnectionOptions): TComponent;
  end;

const
  UserAgentHeader: string = 'Microsoft.AspNetCore.Http.Connections.Client/2.0';

implementation

uses
  ScSignalRHttpTransports;

{ TScHttpTransportTypeHelper }

class function TScHttpTransportTypeHelper.ToString(const Value: TScHttpTransportType): string;
begin
  case Value of
    ttWebSockets:
      Result := 'WebSockets';
    ttLongPolling:
      Result := 'LongPolling';
//    ttServerSentEvents:
//      Result := 'ServerSentEvents';
  else
    Assert(False);
  end;
end;

class function TScHttpTransportTypeHelper.ToString(const Value: TScHttpTransportTypes): string;
var
  tt: TScHttpTransportType;
begin
  if Value = [] then begin
    Result := 'None';
    Exit;
  end;

  Result := '';
  for tt := Low(TScHttpTransportType) to High(TScHttpTransportType) do
    if tt in Value then
      Result := Result + ', ' + TScHttpTransportTypeHelper.ToString(tt);
end;

class function TScHttpTransportTypeHelper.Parse(const Value: string; out TransportType: TScHttpTransportType): boolean;
begin
  Result := True;
  if SameText(Value, 'WebSockets') then
    TransportType := ttWebSockets
  else
  if SameText(Value, 'LongPolling') then
    TransportType := ttLongPolling
//  else
//  if SameText(Value, 'ServerSentEvents') then
//    TransportType := ttServerSentEvents
  else
    Result := False;
end;

{ TScHttpConnectionOptions }

constructor TScHttpConnectionOptions.Create(Owner: TComponent);
begin
  inherited Create;

  Assert(Owner <> nil);
  FOwner := Owner;

  FCookies := TStringList.Create;
{$IFDEF VER11P}
  FCookies.StrictDelimiter := True;
{$ENDIF}
  FCookies.Delimiter := ';';
  FCredentials := TScNetworkCredential.Create;
  FHeaders := TScWebHeaderCollection.Create;
  FProxy := TScWebProxy.Create;
  FSSLOptions := TScSSLClientOptions.Create(nil);

  FCloseTimeout := DefValCloseTimeout;
  FTransports := DefValTransports;
end;

destructor TScHttpConnectionOptions.Destroy;
begin
  FCookies.Free;
  FCredentials.Free;
  FHeaders.Free;
  FProxy.Free;
  FSSLOptions.Free;

  inherited;
end;

procedure TScHttpConnectionOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TScHttpConnectionOptions then begin
    TScHttpConnectionOptions(Dest).FCookies.Assign(FCookies);
    TScHttpConnectionOptions(Dest).FCredentials.Assign(FCredentials);
    TScHttpConnectionOptions(Dest).FHeaders.Assign(FHeaders);
    TScHttpConnectionOptions(Dest).FProxy.Assign(FProxy);
    TScHttpConnectionOptions(Dest).FSSLOptions.Assign(FSSLOptions);
    TScHttpConnectionOptions(Dest).FUrl := FUrl;
    TScHttpConnectionOptions(Dest).FCloseTimeout := FCloseTimeout;
    TScHttpConnectionOptions(Dest).FTransports := FTransports;

    TScHttpConnectionOptions(Dest).FAccessTokenProvider := FAccessTokenProvider;
    TScHttpConnectionOptions(Dest).FSkipNegotiation := FSkipNegotiation;
  end
  else
    inherited;
end;

procedure TScHttpConnectionOptions.ReadHeadersText(Reader: TReader);
begin
  FHeaders.Text := Reader.ReadString;
end;

procedure TScHttpConnectionOptions.WriteHeadersText(Writer: TWriter);
begin
  Writer.WriteString(FHeaders.Text);
end;

procedure TScHttpConnectionOptions.DefineProperties(Filer: TFiler);
begin
  inherited;

  Filer.DefineProperty('HeadersText', ReadHeadersText, WriteHeadersText, FHeaders.Text <> '');
end;

procedure TScHttpConnectionOptions.SetCookies(Value: TStringList);
begin
  if FCookies <> Value then
    FCookies.Assign(Value);
end;

procedure TScHttpConnectionOptions.SetCredentials(Value: TScNetworkCredential);
begin
  if FCredentials <> Value then
    FCredentials.Assign(Value);
end;

procedure TScHttpConnectionOptions.SetHeaders(Value: TScWebHeaderCollection);
begin
  if FHeaders <> Value then
    FHeaders.Assign(Value);
end;

procedure TScHttpConnectionOptions.SetProxy(Value: TScWebProxy);
begin
  if FProxy <> Value then
    FProxy.Assign(Value);
end;

procedure TScHttpConnectionOptions.SetSSLOptions(Value: TScSSLClientOptions);
begin
  if FSSLOptions <> Value then
    FSSLOptions.Assign(Value);
end;

{ TScHttpRequestMessage }

constructor TScHttpRequestMessage.Create(Method: TScRequestMethod; const Url: string);
begin
  inherited Create;
  FMethod := Method;
  FUrl := Url;
  FTimeout := 0;
end;

constructor TScHttpRequestMessage.Create(Method: TScRequestMethod; const Url: string; Timeout: integer);
begin
  inherited Create;
  FMethod := Method;
  FUrl := Url;
  FTimeout := Timeout;
end;

{ TScReadOnlySequenceReader }

constructor TScReadOnlySequenceReader.Create(ReadOnlySequence: TScReadOnlySequence);
begin
  inherited Create;

  Assert(ReadOnlySequence <> nil);
  FReadOnlySequence := ReadOnlySequence;
  FPosition := FReadOnlySequence.StartPos;
end;

procedure TScReadOnlySequenceReader.OnGetNextChunkData(Sender: TObject; out Buffer: TValueArr; out Count: Integer);
var
  Memory: TScMemoryRef;
begin
  if FReadOnlySequence.TryGet(FPosition, Memory) then begin
    Buffer := PtrOffset(Memory.Memory, Memory.Offset);
    Count := Memory.Length;
  end
  else begin
    Buffer := nil;
    Count := 0;
  end;
end;
{ TScHttpClient }

constructor TScHttpClient.Create(AHttpConnectionOptions: TScHttpConnectionOptions);
var
  Guid: TGuid;
begin
  inherited Create;

  Assert(AHttpConnectionOptions <> nil);
  FHttpConnectionOptions := TScHttpConnectionOptions.Create(AHttpConnectionOptions.FOwner);
  FHttpConnectionOptions.Assign(AHttpConnectionOptions);
  FDefaultRequestHeaders := TScWebHeaderCollection.Create;
  FVersion := TScVersion.Create(1, 1);

  CreateGUID(Guid);
  FConnectionGroupName := GuidToString(Guid);
end;

destructor TScHttpClient.Destroy;
begin
  FHttpConnectionOptions.Free;
  FDefaultRequestHeaders.Free;
  FVersion.Free;

  inherited;
end;

function TScHttpClient.Send(Request: TScHttpRequestMessage; CancellationToken: TScCancellationToken = nil): TScHttpWebResponse;
var
  HttpWebRequest: TScHttpWebRequest;
  ReadOnlySequenceReader: TScReadOnlySequenceReader;
begin
  ReadOnlySequenceReader := nil;
  HttpWebRequest := TScHttpWebRequest.Create(nil);
  try
    if Request.Url = '' then
      HttpWebRequest.RequestUri := FBaseAddress
    else
      HttpWebRequest.RequestUri := Request.Url;

    HttpWebRequest.ConnectionGroupName := FConnectionGroupName;

    HttpWebRequest.Cookies := FHttpConnectionOptions.Cookies;
    HttpWebRequest.Credentials := FHttpConnectionOptions.Credentials;
    HttpWebRequest.Proxy := FHttpConnectionOptions.Proxy;
    HttpWebRequest.SSLOptions := FHttpConnectionOptions.SSLOptions;

    HttpWebRequest.Headers.OnCheckNewHeader := CheckNewHeader;
    HttpWebRequest.Headers.Assign(FDefaultRequestHeaders);

    if Request.Timeout = 0 then
      HttpWebRequest.ReadWriteTimeout := FTimeout
    else
      HttpWebRequest.ReadWriteTimeout := Request.Timeout;

    HttpWebRequest.ProtocolVersion := FVersion;
    HttpWebRequest.BeforeSendRequest := FBeforeSendRequest;
    HttpWebRequest.AfterSendRequest := FAfterSendRequest;

    HttpWebRequest.SetAllowedStatuses(Request.AllowedStatuses);
    HttpWebRequest.Method := Request.Method;

    if Request.Content <> nil then begin
      HttpWebRequest.ContentLength := Request.Content.Length;
      HttpWebRequest.ContentType := 'text/plain;charset=UTF-8';
      HttpWebRequest.SendChunked := True;
      ReadOnlySequenceReader := TScReadOnlySequenceReader.Create(Request.Content);
      HttpWebRequest.OnGetNextChunkData := ReadOnlySequenceReader.OnGetNextChunkData;
    end
    else
    if HttpWebRequest.Method in [rmPOST, rmPUT] then begin
      HttpWebRequest.ContentLength := 0;
      HttpWebRequest.ContentType := 'text/plain;charset=UTF-8';
    end;

    Result := HttpWebRequest.GetResponse(CancellationToken);
  finally
    HttpWebRequest.Free;
    ReadOnlySequenceReader.Free;
  end;
end;

function TScHttpClient.Send(const Method: TScRequestMethod; const Url: string;
  const AllowedStatuses: TScHttpStatusCodes; CancellationToken: TScCancellationToken = nil): TScHttpWebResponse;
var
  HttpWebRequest: TScHttpWebRequest;
begin
  HttpWebRequest := TScHttpWebRequest.Create(nil);
  try
    if Url = '' then
      HttpWebRequest.RequestUri := FBaseAddress
    else
      HttpWebRequest.RequestUri := Url;
    HttpWebRequest.ConnectionGroupName := FConnectionGroupName;

    HttpWebRequest.Cookies := FHttpConnectionOptions.Cookies;
    HttpWebRequest.Credentials := FHttpConnectionOptions.FCredentials;
    HttpWebRequest.Proxy := FHttpConnectionOptions.Proxy;
    HttpWebRequest.SSLOptions := FHttpConnectionOptions.SSLOptions;

    HttpWebRequest.Headers.OnCheckNewHeader := CheckNewHeader;
    HttpWebRequest.Headers.Assign(FDefaultRequestHeaders);

    HttpWebRequest.ReadWriteTimeout := FTimeout;
    HttpWebRequest.ProtocolVersion := FVersion;
    HttpWebRequest.BeforeSendRequest := FBeforeSendRequest;
    HttpWebRequest.AfterSendRequest := FAfterSendRequest;

    HttpWebRequest.SetAllowedStatuses(AllowedStatuses);
    HttpWebRequest.Method := Method;

    Result := HttpWebRequest.GetResponse(CancellationToken);
  finally
    HttpWebRequest.Free;
  end;
end;

procedure TScHttpClient.CheckNewHeader(const Key, Value: string);
begin
  // None: to allow adding any header key
end;

{ TScHttpConnection }

constructor TScHttpConnection.Create(AHttpConnectionOptions: TScHttpConnectionOptions; ALogger: TScLogger);
begin
  inherited Create;

  FLogger := ALogger;

  FHttpClientTimeout := 120;
  FMaxRedirects := 100;
  FStarted := False;

  Assert(AHttpConnectionOptions <> nil);
  FHttpConnectionOptions := TScHttpConnectionOptions.Create(AHttpConnectionOptions.FOwner);
  FHttpConnectionOptions.Assign(AHttpConnectionOptions);
  if FHttpConnectionOptions.Url = '' then
    raise ArgumentException.Create('Url');

  FConnectionLock := TCriticalSection.Create;

  if not FHttpConnectionOptions.SkipNegotiation or (FHttpConnectionOptions.Transports <> [ttWebSockets]) then
    FHttpClient := CreateHttpClient;

  if (FHttpClient = nil) and (FHttpConnectionOptions.Transports <> [ttWebSockets]) then
    raise ArgumentException.Create('HttpClient');
end;

destructor TScHttpConnection.Destroy;
begin
  Dispose;

  FTransport.Free;
  FHttpClient.Free;

  FConnectionLock.Free;
  FHttpConnectionOptions.Free;

  inherited;
end;

function TScHttpConnection.GetTransport: TScTransport;
begin
  CheckDisposed;

  if FTransport = nil then
    raise InvalidOperationException.Create(SCannotAccessTransportPipeBeforeConnecting);

  Result := FTransport;
end;

function TScHttpConnection.GetAccessToken: string;
begin
  if Assigned(FAccessTokenProvider) then
    Result := FAccessTokenProvider
  else
    Result := GetNoAccessToken;
end;

function TScHttpConnection.GetCurAccessToken: string;
begin
  Result := FCurAccessToken;
end;

function TScHttpConnection.GetNoAccessToken: string;
begin
  Result := '';
end;

function TScHttpConnection.CreateTransport(AvailableServerTransport: TScHttpTransportType): TScTransport;
var
  RequestedTransportType: TScHttpTransportTypes;
begin
  RequestedTransportType := FHttpConnectionOptions.Transports;

  if (AvailableServerTransport = ttWebSockets) and ((ttWebSockets in RequestedTransportType) or (RequestedTransportType = [])) then
    Result := TScWebSocketsTransport.Create(FHttpConnectionOptions, GetAccessToken, FLogger)
  else
//  if (AvailableServerTransport = ttServerSentEvents) and ((ttServerSentEvents in RequestedTransportType) or (RequestedTransportType = [])) then
//    Result := TScServerSentEventsTransport.Create(FHttpClient, FLogger)
//  else
  if (AvailableServerTransport = ttLongPolling) and ((ttLongPolling in RequestedTransportType) or (RequestedTransportType = [])) then
    Result := TScLongPollingTransport.Create(FHttpClient, FLogger)
  else
    raise InvalidOperationException.Create(SNoRequestedTransports);
end;

procedure TScHttpConnection.Start(TransferFormat: TScTransferFormat; CancellationToken: TScCancellationToken);
begin
  CheckDisposed;

  if FStarted then begin
    Log.LogDebug(FLogger, SHttpConnection_SkippingStart);
    Exit;
  end;

  FConnectionLock.Enter;
  try
    CheckDisposed;

    if FStarted then begin
      Log.LogDebug(FLogger, SHttpConnection_SkippingStart);
      Exit;
    end;

    Log.LogDebug(FLogger, SHttpConnection_Starting);
    SelectAndStartTransport(TransferFormat, CancellationToken);
    FStarted := True;

    Log.LogInformation(FLogger, SHttpConnection_Started);
  finally
    FConnectionLock.Leave;
  end;
end;

procedure TScHttpConnection.Dispose;
begin
  if FDisposed then
    Exit;

  FConnectionLock.Enter;
  try
    if not FDisposed and FStarted then begin
      Log.LogDebug(FLogger, SHttpConnection_DisposingHttpConnection);

      // Stop the transport, but we don't care if it throws.
      // The transport should also have completed the pipe with this exception.
      try
        if FTransport <> nil then
          FTransport.Stop;
      except
        on E: Exception do
          Log.LogError(FLogger, SHttpConnection_TransportThrewExceptionOnStop, E);
      end;

      Log.LogInformation(FLogger, SHttpConnection_Disposed);
    end
    else
      Log.LogDebug(FLogger, SHttpConnection_SkippingDispose);

    FDisposed := True;

    FreeAndNil(FTransport);
    FreeAndNil(FHttpClient);
  finally
    FDisposed := True;
    FConnectionLock.Leave;
  end;
end;

procedure TScHttpConnection.SelectAndStartTransport(TransferFormat: TScTransferFormat; CancellationToken: TScCancellationToken);

  function Contains(const Value: string; const Arr: TStringArray): boolean;
  var
    i: integer;
  begin
    for i := 0 to Length(Arr) - 1 do
      if Value = Arr[i] then begin
        Result := True;
        Exit;
      end;

    Result := False;
  end;

var
  AUri: string;
  TransportExceptions: string;
  NegotiationResponse: TScNegotiationResponse;
  Redirects: integer;
  ConnectUrl: string;
  AvailableTransports: TScAvailableTransports;
  TransferFormatString: string;
  Transport: TScAvailableTransport;
  TransportType: TScHttpTransportType;
  i: integer;
begin
  AUri := FHttpConnectionOptions.Url;
  // Set the initial access token provider back to the original one from options
  FAccessTokenProvider := FHttpConnectionOptions.AccessTokenProvider;

  TransportExceptions := '';
  NegotiationResponse := nil;
  try
    if FHttpConnectionOptions.SkipNegotiation then begin
      if FHttpConnectionOptions.Transports = [ttWebSockets] then begin
        Log.LogDebug(FLogger, SHttpConnection_StartingTransport, [TScHttpTransportTypeHelper.ToString(FHttpConnectionOptions.Transports), AUri]);
        StartTransport(AUri, ttWebSockets, TransferFormat, CancellationToken);
      end
      else
        raise InvalidOperationException.Create(SNegotiationCanNotBeSkipped);
    end
    else begin
      Redirects := 0;
      repeat
        FreeAndNil(NegotiationResponse);
        NegotiationResponse := GetNegotiationResponse(AUri, CancellationToken);

        if NegotiationResponse.Url <> '' then
          AUri := NegotiationResponse.Url;

        if NegotiationResponse.AccessToken <> '' then begin
          FCurAccessToken := NegotiationResponse.AccessToken;
          FAccessTokenProvider := GetCurAccessToken;
        end;

        Inc(Redirects);
      until (NegotiationResponse.Url = '') or (Redirects >= FMaxRedirects);

      if (Redirects = FMaxRedirects) and (NegotiationResponse.Url <> '') then
        raise InvalidOperationException.Create(SNegotiateRedirectionLimitExceeded);

      // This should only need to happen once
      ConnectUrl := CreateConnectUrl(AUri, NegotiationResponse.ConnectionId);

      // We're going to search for the transfer format as a string because we don't want to parse
      // all the transfer formats in the negotiation response, and we want to allow transfer formats
      // we don't understand in the negotiate response.
      TransferFormatString := TScTransferFormatHelper.ToString(TransferFormat);

    {$IFNDEF VER11P}
      SetLength(AvailableTransports, 0);
    {$ENDIF}
      AvailableTransports := NegotiationResponse.AvailableTransports;
      for i := 0 to Length(AvailableTransports) - 1 do begin
        Transport := AvailableTransports[i];

        if not TScHttpTransportTypeHelper.Parse(Transport.Transport, TransportType) then begin
          Log.LogDebug(FLogger, SHttpConnection_TransportNotSupported, [Transport.Transport]);
          TransportExceptions := TransportExceptions + #13#10'The ' + Transport.Transport + ' transport is not supported by the client.';
          Continue;
        end;

        try
          if not (TransportType in FHttpConnectionOptions.Transports) and (FHttpConnectionOptions.Transports <> []) then begin
            Log.LogDebug(FLogger, SHttpConnection_TransportDisabledByClient, [Transport.Transport]);
            TransportExceptions := TransportExceptions + #13#10'The ' + Transport.Transport + ' transport is disabled by the client.';
          end
          else if not Contains(TransferFormatString, Transport.TransferFormats) then begin
            Log.LogDebug(FLogger, SHttpConnection_TransportDoesNotSupportTransferFormat, [Transport.Transport, TScTransferFormatHelper.ToString(TransferFormat)]);
            TransportExceptions := TransportExceptions + #13#10'The transport does not support the ' + TScTransferFormatHelper.ToString(TransferFormat) + ' transfer format.';
          end
          else begin
            // The negotiation response gets cleared in the fallback scenario.
            if NegotiationResponse = nil then begin
              NegotiationResponse := GetNegotiationResponse(AUri, CancellationToken);
              ConnectUrl := CreateConnectUrl(AUri, NegotiationResponse.ConnectionId);
            end;

            Log.LogDebug(FLogger, SHttpConnection_StartingTransport, [TScHttpTransportTypeHelper.ToString(TransportType), ConnectUrl]);
            StartTransport(ConnectUrl, TransportType, TransferFormat, CancellationToken);
            break;
          end;
        except
          on E: Exception do begin
            Log.LogDebug(FLogger, SHttpConnection_TransportFailed, [Transport.Transport, E.Message]);
            TransportExceptions := TransportExceptions + #13#10 + E.Message;

            // Try the next transport
            // Clear the negotiation response so we know to re-negotiate.
            FreeAndNil(NegotiationResponse);
          end;
        end;
      end;
    end;
  finally
    NegotiationResponse.Free;
  end;

  if FTransport = nil then
    if TransportExceptions <> '' then
      raise InvalidOperationException.Create(SUnableConnectWithAnyTransports + #13#10 + TransportExceptions)
    else
      raise InvalidOperationException.Create(SNoneTransportsNotSupported);
end;

function TScHttpConnection.GetNegotiationResponse(const Url: string;
  CancellationToken: TScCancellationToken): TScNegotiationResponse;
begin
  Result := Negotiate(Url, CancellationToken);
  FConnectionId := Result.ConnectionId;
end;

function TScHttpConnection.Negotiate(const Url: string; CancellationToken: TScCancellationToken): TScNegotiationResponse;
var
  Scheme, User, Password, NetworkLocation, Port, Path, Resource, Parameters, Query, Fragment: string;
  Uri: string;
  Request: TScHttpRequestMessage;
  Response: TScHttpWebResponse;
  ResponseBuffer: TBytes;
begin
  if FHttpClient = nil then
    raise ArgumentException.Create('HttpClient');

  try
    // Get a connection ID from the server
    Log.LogDebug(FLogger, SHttpConnection_EstablishingConnection, [Url]);

    TScHttpParser.ParseURL(Url, Scheme, User, Password, NetworkLocation, Port, Path, Resource, Parameters, Query, Fragment);
    if Scheme <> '' then
      Uri := Scheme + '//'
    else
      Uri := '';
    if RightStr(Path, 1) <> '/' then
      Path := Path + '/';
    Path := Path + 'negotiate';
    Uri := Uri + NetworkLocation + Port + Path + Resource + Parameters + Query + Fragment;

    Request := TScHttpRequestMessage.Create(rmPost, Uri);
    try
      Response := FHttpClient.Send(Request, CancellationToken);
      try
        ResponseBuffer := Response.ReadAsBytes;
        Result := TScNegotiateProtocol.ParseResponse(ResponseBuffer);
        try
          if Result.Error <> '' then
            raise HubException.Create(Result.Error);
        except
          Result.Free;
          raise;
        end;

        Log.LogDebug(FLogger, SHttpConnection_ConnectionEstablished, [Result.ConnectionId]);
      finally
        Response.Free;
      end;
    finally
      Request.Free;
    end;
  except
    on E: Exception do begin
      Log.LogError(FLogger, SHttpConnection_ErrorWithNegotiation, [Url], E);
      raise;
    end;
  end;
end;

class function TScHttpConnection.AppendQueryString(const Url, NewQuery: string): string;
var
  Scheme, User, Password, NetworkLocation, Port, Path, Resource, Parameters, Query, Fragment: string;
begin
  if NewQuery = '' then begin
    Result := Url;
    Exit;
  end;

  TScHttpParser.ParseURL(Url, Scheme, User, Password, NetworkLocation, Port, Path, Resource, Parameters, Query, Fragment);
  if Query <> '' then
    Query := Query + '&'
  else
    Query := '?';

  Query := Query + NewQuery;

  if Scheme <> '' then
    Result := Scheme + '//'
  else
    Result := '';
  Result := Result + NetworkLocation + Port + Path + Resource + Parameters + Query + Fragment;
end;

function TScHttpConnection.CreateConnectUrl(const Url, ConnectionId: string): string;
begin
  if Trim(ConnectionId) = '' then
    raise ArgumentException.Create(SInvalidConnectionId);

  Result := AppendQueryString(Url, 'id=' + ConnectionId);
end;

procedure TScHttpConnection.StartTransport(const ConnectUrl: string; TransportType: TScHttpTransportType;
  TransferFormat: TScTransferFormat; CancellationToken: TScCancellationToken);
var
  ATransport: TScTransport;
begin
  if FTransport <> nil then
    raise InvalidOperationException.Create(STransportStarted);

  ATransport := CreateTransport(TransportType);

  try
    // Start the transport, giving it one end of the pipe
    ATransport.Start(ConnectUrl, TransferFormat, CancellationToken);
  except
    on E: Exception do begin
      Log.LogError(FLogger, SHttpConnection_ErrorStartingTransport, [TScHttpTransportTypeHelper.ToString(TransportType)], E);
      ATransport.Free;
      FTransport := nil;
      raise;
    end;
  end;

  // Disable keep alives for long polling
  FHasInherentKeepAlive := TransportType = ttLongPolling;

  FTransport := ATransport;

  Log.LogDebug(FLogger, SHttpConnection_TransportStarted, [TScHttpTransportTypeHelper.ToString(TransportType)]);
end;

function TScHttpConnection.CreateHttpClient: TScHttpClient;
begin
  Result := TScHttpClient.Create(FHttpConnectionOptions);
  try
    Result.BeforeSendRequest := BeforeSendRequestDoAccessToken;
    Result.Timeout := FHttpClientTimeout;
    Result.DefaultRequestHeaders.Assign(FHttpConnectionOptions.Headers);

    // Start with the user agent header
    Result.DefaultRequestHeaders['User-Agent'] := UserAgentHeader;

    Result.DefaultRequestHeaders.Remove('X-Requested-With');
    // Tell auth middleware to 401 instead of redirecting
    Result.DefaultRequestHeaders.Add('X-Requested-With', 'XMLHttpRequest');
  except
    Result.Free;
    raise;
  end;
end;

procedure TScHttpConnection.BeforeSendRequestDoAccessToken(Sender: TObject);
var
  AccessToken: string;
begin
  // Apply the authorization header in a handler instead of a default header because it can change with each request
  AccessToken := GetAccessToken();
  if AccessToken <> '' then
    (Sender as TScHttpWebRequest).Headers['Authorization'] := 'Bearer ' + AccessToken;
end;

procedure TScHttpConnection.CheckDisposed;
begin
  if FDisposed then
    raise InvalidOperationException.Create(SHttpConnectionDisposed);
end;

{ TScNegotiateProtocol }

const
  ConnectionIdPropertyName = 'connectionId';
  UrlPropertyName = 'url';
  AccessTokenPropertyName  = 'accessToken';
  AvailableTransportsPropertyName = 'availableTransports';
  TransportPropertyName = 'transport';
  TransferFormatsPropertyName = 'transferFormats';
  ErrorPropertyName = 'error';
  ProtocolVersionPropertyName = 'ProtocolVersion';

class procedure TScNegotiateProtocol.WriteResponse(Response: TScNegotiationResponse; out Output: TBytes);
var
  Writer: TJSONTextWriter;
  i, j: integer;
begin
  Writer := TJSONTextWriter.Create;
  try
    Writer.WriteObjectBegin;

    if Response.Url <> '' then begin
      Writer.WriteString(UrlPropertyName);
      Writer.WriteValueSeparator;
      Writer.WriteString(Response.Url);
      Writer.WriteElementSeparator;
    end;

    if Response.AccessToken <> '' then begin
      Writer.WriteString(AccessTokenPropertyName);
      Writer.WriteValueSeparator;
      Writer.WriteString(Response.AccessToken);
      Writer.WriteElementSeparator;
    end;

    if Response.ConnectionId <> '' then begin
      Writer.WriteString(ConnectionIdPropertyName);
      Writer.WriteValueSeparator;
      Writer.WriteString(Response.ConnectionId);
      Writer.WriteElementSeparator;
    end;

    Writer.WriteString(AvailableTransportsPropertyName);
    Writer.WriteValueSeparator;
    Writer.WriteArrayBegin;

    if Length(Response.AvailableTransports) > 0 then begin
      for i := 0 to Length(Response.AvailableTransports) - 1 do begin
        if i > 0 then
          Writer.WriteElementSeparator;

        Writer.WriteObjectBegin;

        Writer.WriteString(TransportPropertyName);
        Writer.WriteValueSeparator;
        if Response.AvailableTransports[i].Transport <> '' then
          Writer.WriteString(Response.AvailableTransports[i].Transport)
        else
          Writer.WriteNull;

        Writer.WriteElementSeparator;
        Writer.WriteString(TransferFormatsPropertyName);
        Writer.WriteValueSeparator;
        Writer.WriteArrayBegin;

        if Length(Response.AvailableTransports[i].TransferFormats) > 0 then begin
          for j := 0 to Length(Response.AvailableTransports[i].TransferFormats) - 1 do begin
            if j > 0 then
              Writer.WriteElementSeparator;

            Writer.WriteString(Response.AvailableTransports[i].TransferFormats[j]);
          end;
        end;

        Writer.WriteArrayEnd;
        Writer.WriteObjectEnd;
      end;
    end;

    Writer.WriteArrayEnd;
    Writer.WriteObjectEnd;

    Output := Writer.AsBytes;
  finally
    Writer.Free;
  end;
end;

class function TScNegotiateProtocol.ParseResponse(const Content: TBytes): TScNegotiationResponse;
var
  Reader: TJSONTextReader;
  PropertyName: string;
  Completed: boolean;
  n: integer;
begin
  Result := TScNegotiationResponse.Create;
  try
    Reader := TJSONTextReader.Create;
    try
      Reader.SetBytes(Content);
      Reader.Initialize;

      TJsonUtils.CheckRead(Reader);
      if Reader.TokenType <> jtObject then
        raise InvalidDataException.CreateFmt(SUnexpectedJsonObjectTokenType, [TJsonUtils.TokenToStr(Reader.TokenType)]);

      Completed := False;
      while not Completed and TJsonUtils.CheckRead(Reader) do begin
        case Reader.TokenType of
          jtString: begin
            PropertyName := Reader.ReadString;
            if not Reader.ReadValueSeparator then
              raise JSONException.Create(cInvalidValueSeparator);

            if PropertyName = UrlPropertyName then
              Result.Url := TJsonUtils.ReadAsString(Reader, UrlPropertyName)
            else
            if PropertyName = AccessTokenPropertyName then
              Result.AccessToken := TJsonUtils.ReadAsString(Reader, AccessTokenPropertyName)
            else
            if PropertyName = ConnectionIdPropertyName then
              Result.ConnectionId := TJsonUtils.ReadAsString(Reader, ConnectionIdPropertyName)
            else
            if PropertyName = AvailableTransportsPropertyName then begin
              TJsonUtils.CheckRead(Reader);
              if Reader.TokenType <> jtArray then
                raise InvalidDataException.CreateFmt(SUnexpectedJsonArrayTokenType, [TJsonUtils.TokenToStr(Reader.TokenType)]);

              SetLength(Result.AvailableTransports, 0);
              n := 0;
              while TJsonUtils.CheckRead(Reader) do begin
                if Reader.TokenType = jtObject then begin
                  SetLength(Result.AvailableTransports, n + 1);
                  ParseAvailableTransport(Reader, Result.AvailableTransports[n]);
                  Inc(n);
                end
                else
                if Reader.TokenType = jtArrayEnd then
                  break;
              end;
            end
            else
            if PropertyName = ErrorPropertyName then
              Result.Error := TJsonUtils.ReadAsString(Reader, ErrorPropertyName)
            else
            if PropertyName = ProtocolVersionPropertyName then
              raise InvalidOperationException.Create(SOnlyCoreSignalRServerSupported)
            else
              Reader.Skip;
          end;

          jtComma:
            ;
          jtObjectEnd:
            Completed := True;
          else
            raise InvalidDataException.CreateFmt(SUnexpectedNegotiationResponseToken, [TJsonUtils.TokenToStr(Reader.TokenType)]);
        end;
      end;

      if (Result.Url = '') and (Result.Error = '') then begin
        // if url isn't specified or there isn't an error, ConnectionId and available transports are required
        if Result.ConnectionId = '' then
          raise InvalidDataException.CreateFmt(SMissingRequiredProperty, [ConnectionIdPropertyName]);

        if Length(Result.AvailableTransports) = 0 then
          raise InvalidDataException.CreateFmt(SMissingRequiredProperty, [AvailableTransportsPropertyName]);
      end;
    finally
      Reader.Free;
    end;
  except
    on E: Exception do begin
      Result.Free;
      raise InvalidDataException.Create(SInvalidNegotiationResponse + #13#10 + E.Message);
    end;
  end;
end;

class procedure TScNegotiateProtocol.ParseAvailableTransport(Reader: TJSONTextReader; var AvailableTransport: TScAvailableTransport);
var
  MemberName: string;
  Completed: boolean;
  n: integer;
begin
  while TJsonUtils.CheckRead(Reader) do begin
    case Reader.TokenType of
      jtString: begin
        MemberName := Reader.ReadString;
        if not Reader.ReadValueSeparator then
          raise JSONException.Create(cInvalidValueSeparator);

        if MemberName = TransportPropertyName then
          AvailableTransport.Transport := TJsonUtils.ReadAsString(Reader, TransportPropertyName)
        else
        if MemberName = TransferFormatsPropertyName then begin
          TJsonUtils.CheckRead(Reader);
          if Reader.TokenType <> jtArray then
            raise InvalidDataException.CreateFmt(SUnexpectedJsonArrayTokenType, [TJsonUtils.TokenToStr(Reader.TokenType)]);

          Completed := False;
          SetLength(AvailableTransport.TransferFormats, 0);
          n := 0;
          while not Completed and TJsonUtils.CheckRead(Reader) do begin
            case Reader.TokenType of
              jtString: begin
                SetLength(AvailableTransport.TransferFormats, n + 1);
                AvailableTransport.TransferFormats[n] := TJsonUtils.ReadAsString(Reader, TransferFormatsPropertyName);
                Inc(n);
              end;
              jtComma:
                ;
              jtArrayEnd:
                Completed := True;
              else
                raise InvalidDataException.CreateFmt(SUnexpectedReadingTransferToken, [TJsonUtils.TokenToStr(Reader.TokenType)]);
            end;
          end;
        end
        else
          Reader.Skip;
      end;

      jtComma:
        ;
      jtObjectEnd: begin
        if AvailableTransport.Transport = '' then
          raise InvalidDataException.CreateFmt(SMissingRequiredProperty, [TransportPropertyName]);
        if Length(AvailableTransport.TransferFormats) = 0 then
          raise InvalidDataException.CreateFmt(SMissingRequiredProperty, [TransferFormatsPropertyName]);
        Exit;
      end;
    else
      raise InvalidDataException.CreateFmt(SUnexpectedReadingTransportToken, [TJsonUtils.TokenToStr(Reader.TokenType)]);
    end;
  end;

  raise InvalidDataException.Create(SUnexpectedJsonEnd);
end;

{ TScHttpConnectionOptionsUtils }

class function TScHttpConnectionOptionsUtils.GetOptionsOwner(Obj: TScHttpConnectionOptions): TComponent;
begin
  Result := Obj.FOwner;
end;

end.
