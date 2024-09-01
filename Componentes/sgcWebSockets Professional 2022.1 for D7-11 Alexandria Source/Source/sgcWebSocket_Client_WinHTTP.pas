{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Client_WinHTTP;

interface

{$I sgcVer.inc}
{$IFDEF SGC_WINHTTP}

uses
  Classes, SysUtils, Windows, StrUtils,
  // websocket
  sgcWebSocket_Classes, sgcWebSocket_WinAPI, sgcWebSocket_Classes_WinHTTP,
  sgcWebSocket_Types, sgcWebSocket_Helpers, sgcWebSocket_Extensions,
  sgcWebSocket_Classes_SyncObjs, sgcBase_Helpers;

type
  TOnReadMessageEvent = procedure(Sender: TObject; Buffer: TsgcArrayOfBytes;
    BufferType: WINHTTP_WEB_SOCKET_BUFFER_TYPE; BufferSize: Cardinal) of object;

type
  TsgcWSClient_WinHTTP = class;

  TsgcWSReadThread = class(TThread)
  private
    FBufferSize: DWORD;
    FSession: HINTERNET;
    function GetBufferSize: DWORD;
  protected
    procedure Execute; override;
  public
    property Session: HINTERNET read FSession write FSession;
  private
    FOnReadMessage: TOnReadMessageEvent;
    FOnDisconnect: TNotifyEvent;
  public
    property BufferSize: DWORD read GetBufferSize write FBufferSize;
  public
    property OnReadMessage: TOnReadMessageEvent read FOnReadMessage
      write FOnReadMessage;
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;
  end;

  TsgcWSOptionsClient = class(TPersistent)
  private
    FFragmentedMessages: TwsFragmentedMessages;
    FOrigin: String;
    FParameters: String;
    function GetParameters: String;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property FragmentedMessages: TwsFragmentedMessages read FFragmentedMessages
      write FFragmentedMessages;
    property Origin: String read FOrigin write FOrigin;
    property Parameters: String read GetParameters write FParameters;
  end;

  TsgcWSAuthenticationClient_Basic = class(TsgcWSAuthentication_Basic)

  end;

  TsgcWSAuthenticationClient_Options = class(TsgcWSAuthentication_Options)
  private
    FPassword: String;
    FBasic: TsgcWSAuthenticationClient_Basic;
    FUser: String;
  protected
    procedure SetBasic(const Value: TsgcWSAuthenticationClient_Basic);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Password: String read FPassword write FPassword;
    property Basic: TsgcWSAuthenticationClient_Basic read FBasic write SetBasic;
    property User: String read FUser write FUser;
  end;

  TsgcWSProxy_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FHost: String;
    FPassword: String;
    FPort: Integer;
    FUsername: String;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property Host: String read FHost write FHost;
    property Password: String read FPassword write FPassword;
    property Port: Integer read FPort write FPort;
    property Username: String read FUsername write FUsername;
  end;

  TsgcWSConnectionClient_WinHTTP = class(TsgcWSConnection_WinHTTP)
  private
    FAuthentication: TsgcWSAuthenticationClient_Options;
    FOptions: TsgcWSOptionsClient;
  protected
    procedure SetAuthentication(const Value
      : TsgcWSAuthenticationClient_Options);
    procedure SetOptions(const Value: TsgcWSOptionsClient);
  protected
    property Authentication: TsgcWSAuthenticationClient_Options
      read FAuthentication write SetAuthentication;
    property Options: TsgcWSOptionsClient read FOptions write SetOptions;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TClientWinhttpThread = class(TThread)
  private
    FClient: TsgcWSClient_WinHTTP;
    FMethod: string;
  protected
    procedure Execute; override;
  public
    constructor Create(const aMethod: string; const aClient: TsgcWSClient_WinHTTP);
    class procedure Stop(const aClient: TsgcWSClient_WinHTTP);
    class procedure Start(const aClient: TsgcWSClient_WinHTTP);
  end;

  TsgcWSClient_WinHTTP = class(TsgcWSComponent_Client_WinHTTP)
    { from TComponent }
  protected
    procedure Loaded; override;
    { from TComponent }

    { from TsgcWSComponent_Base }
  protected
    procedure DoEventDisconnect(aConnection: TsgcWSConnection;
      Code: Integer); override;
    { from TsgcWSComponent_Base }

    { from TsgcWSComponent }
  protected
    procedure DoNotifyConnect(aConnection: TsgcWSConnection); override;
    procedure DoNotifyDisconnect(aConnection: TsgcWSConnection); override;
  protected
    procedure OnHeartBeatTimeoutEvent(Sender: TObject); override;
    procedure OnHeartBeatExceptionEvent(Sender: TObject; E: Exception);
      override;
    procedure OnHeartBeatTimeoutExceptionEvent(Sender: TObject;
      E: Exception); override;
    procedure OnHeartBeatEvent(Sender: TObject); override;
  protected
    function GetHeartBeat: TsgcWSHeartBeat_Options; override;
    function GetExtensions: TsgcWSExtensions; override;
    function GetSpecifications: TsgcWSSpecifications; override;
    function GetLogFile: TsgcWSLogFile; override;
    { from TsgcWSComponent }

    { from TsgcWSComponent_Client }
  public
    procedure WriteData(const aText: String; const aSize: Integer = 0;
      const aStreaming: TwsStreaming = stmNone); overload; override;
    procedure WriteData(const aStream: TStream; const aSize: Integer = 0;
      const aStreaming: TwsStreaming = stmNone); overload; override;
  public
    function WriteAndWaitData(const aText: String;
      const aTimeout: Integer = 10000): string; override;
  protected
    procedure SetURL(const Value: String); override;
    { from TsgcWSComponent_client }

    { WatchDog }
  private
    FOnBeforeWatchDog: TsgcWSOnBeforeWatchDogEvent;
  private
    procedure DoStart;
  protected
    procedure OnWatchDogEvent(Sender: TObject); override;
    procedure OnWatchDogExceptionEvent(Sender: TObject; E: Exception); override;
  public
    property OnBeforeWatchDog: TsgcWSOnBeforeWatchDogEvent
      read FOnBeforeWatchDog write FOnBeforeWatchDog;
    { WatchDog }

    { from TsgcWSComponent_WSClient }
  private
    FOnBeforeConnect: TsgcWSOnBeforeConnectEvent;
  protected
    function GetActive: Boolean; override;
    procedure SetActive(const Value: Boolean); override;
  public
    property OnBeforeConnect: TsgcWSOnBeforeConnectEvent read FOnBeforeConnect
      write FOnBeforeConnect;
    { from TsgcWSComponent_WSClient }

    { properties }
  private
    FWSConnection: TsgcWSConnectionClient_WinHTTP;
    FDisconnecting: Boolean;
  private
    FAsyncBuffer: TsgcArrayOfBytes;
  private
    FConnectTimeout: Integer;
    FReadTimeout: Integer;
    FActive: Boolean;
    FAsynchronous: Boolean;
    FReceiveBufferSize: Integer;
    FOptions: TsgcWSOptionsClient;
    FFragmentedUTF8: Boolean;
    FFragmentedBinary: Boolean;
    FFragmentedUTF8Bytes: TsgcArrayOfBytes;
    FFragmentedBinaryBytes: TsgcArrayOfBytes;
  protected
    procedure SetOptions(const Value: TsgcWSOptionsClient); virtual;
    procedure SetAsynchronous(const Value: Boolean); virtual;
  public
    property Asynchronous: Boolean read FAsynchronous write SetAsynchronous;
    property ConnectTimeout: Integer read FConnectTimeout write FConnectTimeout;
    property Options: TsgcWSOptionsClient read FOptions write SetOptions;
    property ReadTimeout: Integer read FReadTimeout write FReadTimeout;
    property ReceiveBufferSize: Integer read FReceiveBufferSize
      write FReceiveBufferSize;
    { properties }

    { thread methods }
  private
    FThreadStart: Boolean;
  public
    procedure Start;
    procedure Stop;
    { thread methods }

    { connect/disconnect and wait }
  private
    FConnecting: Boolean;
  public
    function Connect(const aTimeout: Integer = 10000): Boolean;
    function Disconnect(const aTimeout: Integer = 10000): Boolean;
    { connect/disconnect and wait }

    { authentication }
  private
    FAuthentication: TsgcWSAuthenticationClient_Options;
    procedure SetAuthentication(const Value
      : TsgcWSAuthenticationClient_Options);
  public
    property Authentication: TsgcWSAuthenticationClient_Options
      read FAuthentication write SetAuthentication;
    { authentication }

    { proxy }
  private
    FProxy: TsgcWSProxy_Options;
  protected
    procedure SetProxy(const Value: TsgcWSProxy_Options); virtual;
  public
    property Proxy: TsgcWSProxy_Options read FProxy write SetProxy;
    { proxy }

    { handles }
  private
    FHTTPSession: HINTERNET;
    FWSSession: HINTERNET;
    FHTTPHandle: HINTERNET;
    FHTTPRequest: HINTERNET;
    { handles }

    { constructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor }

    { sync read thread }
  private
    FReadThread: TsgcWSReadThread;
  private
    procedure DoReadMessageEvent(Buffer: TsgcArrayOfBytes;
      BufferType: WINHTTP_WEB_SOCKET_BUFFER_TYPE; BufferSize: DWORD);
    procedure DoDisconnectEvent;
  protected
    procedure OnReadMessageEvent(Sender: TObject; Buffer: TsgcArrayOfBytes;
      BufferType: WINHTTP_WEB_SOCKET_BUFFER_TYPE; BufferSize: DWORD); virtual;
    procedure OnDisconnectEvent(Sender: TObject); virtual;
  protected
    procedure DoReadStart; virtual;
    procedure DoReadStop; virtual;
    { sync read thread }

    { methods }
  private
    procedure DoWriteFragmentedUTF8(const aBuffer: TsgcArrayOfBytes);
    procedure DoWriteFragmentedBinary(const aBuffer: TsgcArrayOfBytes);
  private
    procedure DoClear;
  private
    procedure DoInitWatchDog;
    procedure DoFinWatchDog;
  private
    function DoValidateServerCertificate: Boolean;
  private
    procedure DoQueryCloseStatus;
  private
    procedure DoCreateConnection;
    procedure DoHttpOpen;
    procedure DoAsyncStatusCallback;
    procedure DoHttpConnect;
    procedure DoHttpSecure;
    procedure DoHttpTimeout;
    procedure DoRequestUpgrade;
    procedure DoDisableRedirects;
    procedure DoDisableCookies;
    procedure DoSetProxy;
    procedure DoCustomHeaders;
    procedure DoRequestGET;
    procedure DoResponseGET;
    procedure DoDisableTLSValidations;
    function DoAsyncCompleteUpgrade: Boolean;
    procedure DoCompleteUpgrade;
    procedure DoSetProtocol;
    procedure DoSetHeartBeat;
  private
    procedure DoAsyncWebSocketReceive;
    procedure DoAsyncStatusReadComplete(const aBufferType
      : WINHTTP_WEB_SOCKET_BUFFER_TYPE; const aBufferSize: Cardinal);
  private
    procedure DoEventNotifyConnect;
    procedure DoEventNotifyDisconnect;
    procedure DoEventNotifyError(AsyncResult: WINHTTP_ASYNC_RESULT;
      Operation: WINHTTP_WEB_SOCKET_OPERATION);
  private
    procedure DoSyncConnect;
    procedure DoAsyncConnect;
  protected
    procedure DoBeforeConnect; virtual;
    procedure DoConnect; virtual;
    procedure DoDisconnect; virtual;
  public
    procedure Ping;
  public
    function CheckPlatform: Boolean;
    { methods }
  end;

  TsgcWSAsyncClients = class(TsgcThreadList)
  public
    procedure AddClient(const aClient: TsgcWSClient_WinHTTP);
    procedure DeleteClient(const aClient: TsgcWSClient_WinHTTP);
  public
    function GetClientByHINTERNET(aHINTERNET: HINTERNET): TsgcWSClient_WinHTTP;
  end;

  TsgcWSAsyncClient = class
  private
    FClient: TsgcWSClient_WinHTTP;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    property Client: TsgcWSClient_WinHTTP read FClient write FClient;
  end;


{$ENDIF}

implementation

{$IFDEF SGC_WINHTTP}

uses
  sgcBase_Const,
  sgcWebSocket_Const, sgcSocket_Const;

var
  oAsyncClients: TsgcWSAsyncClients = nil;

function GetAsyncClients: TsgcWSAsyncClients;
begin
  if not Assigned(oAsyncClients) then
    oAsyncClients := TsgcWSAsyncClients.Create;
  result := oAsyncClients;
end;

constructor TsgcWSClient_WinHTTP.Create(aOwner: TComponent);
begin
  inherited;
  FOptions := TsgcWSOptionsClient.Create;
  FProxy := TsgcWSProxy_Options.Create;
  FProxy.Enabled := False;
  FProxy.Port := 8080;
  FAuthentication := TsgcWSAuthenticationClient_Options.Create;
  ConnectTimeout := 60000;
  ReadTimeout := 60000;
  TLS := False;
  WatchDog.Interval := 10;
  ReceiveBufferSize := 1024;
  FDisconnecting := False;
  SetLength(FFragmentedUTF8Bytes, 0);
  SetLength(FFragmentedBinaryBytes, 0);
end;

destructor TsgcWSClient_WinHTTP.Destroy;
begin
  DoClear;
  sgcFree(FReadThread);
  sgcFree(FAuthentication);
  sgcFree(FProxy);
  sgcFree(FOptions);
  if Asynchronous then
    GetAsyncClients.DeleteClient(self);
  inherited;
end;

function TsgcWSClient_WinHTTP.CheckPlatform: Boolean;
begin
  result := WinHttpCheckPlatform;
end;

procedure TsgcWSClient_WinHTTP.DoClear;
begin
  FConnecting := False;
  if Assigned(FHTTPSession) then
  begin
    WinHttpCloseHandle(FHTTPSession);
    FHTTPSession := nil;
  end;
  if Assigned(FWSSession) then
  begin
    WinHttpCloseHandle(FWSSession);
    FWSSession := nil;
  end;
  if Assigned(FHTTPHandle) then
  begin
    WinHttpCloseHandle(FHTTPHandle);
    FHTTPHandle := nil;
  end;
  if Assigned(FWSSession) then
  begin
    WinHttpCloseHandle(FWSSession);
    FWSSession := nil;
  end;
  if Assigned(FHTTPRequest) then
  begin
    WinHttpCloseHandle(FHTTPRequest);
    FHTTPRequest := nil;
  end;
  sgcFree(FWSConnection);
end;

procedure OnStatusCallback(HINTERNET: HINTERNET; dwContext: Pointer;
  dwInternetStatus: DWORD; lpvStatusInformation: Pointer;
  dwStatusInformationLength: DWORD); stdcall;
var
  oClient: TsgcWSClient_WinHTTP;
  vFlags: DWORD;
  vError: String;
begin
  case dwInternetStatus of
    // WINHTTP_CALLBACK_STATUS_RESOLVING_NAME:
    // WINHTTP_CALLBACK_STATUS_NAME_RESOLVED:
    // WINHTTP_CALLBACK_STATUS_CONNECTING_TO_SERVER:
    // WINHTTP_CALLBACK_STATUS_CONNECTED_TO_SERVER:
    // WINHTTP_CALLBACK_STATUS_SENDING_REQUEST:
    // WINHTTP_CALLBACK_STATUS_REQUEST_SENT:
    // WINHTTP_CALLBACK_STATUS_RECEIVING_RESPONSE:
    // WINHTTP_CALLBACK_STATUS_RESPONSE_RECEIVED:
    // WINHTTP_CALLBACK_STATUS_CLOSING_CONNECTION:
    // WINHTTP_CALLBACK_STATUS_CONNECTION_CLOSED:
    // WINHTTP_CALLBACK_STATUS_HANDLE_CREATED: // DoHttpConnect - DoHttpSecure - DoCompleteUpgrade
    // WINHTTP_CALLBACK_STATUS_HANDLE_CLOSING:
    // WINHTTP_CALLBACK_STATUS_DETECTING_PROXY:
    // WINHTTP_CALLBACK_STATUS_REDIRECT:
    // WINHTTP_CALLBACK_STATUS_INTERMEDIATE_RESPONSE:
    WINHTTP_CALLBACK_STATUS_SECURE_FAILURE:
      begin
        oClient := GetAsyncClients.GetClientByHINTERNET(HINTERNET);
        vError := '';
        if Assigned(oClient) then
        begin
          vFlags := DWORD(lpvStatusInformation^);
          if (vFlags and WINHTTP_CALLBACK_STATUS_FLAG_CERT_REV_FAILED) <> $0
          then
            vError := vError + 'Certificate revision failed.';
          if (vFlags and WINHTTP_CALLBACK_STATUS_FLAG_INVALID_CERT) <> $0 then
            vError := vError + 'Invalid certificate.';
          if (vFlags and WINHTTP_CALLBACK_STATUS_FLAG_CERT_REVOKED) <> $0 then
            vError := vError + 'Certificate revoked.';
          if (vFlags and WINHTTP_CALLBACK_STATUS_FLAG_INVALID_CA) <> $0 then
            vError := vError + 'Certificate revision failed.';
          if (vFlags and WINHTTP_CALLBACK_STATUS_FLAG_CERT_CN_INVALID) <> $0
          then
            vError := vError + 'Certificate revision failed.';
          if (vFlags and WINHTTP_CALLBACK_STATUS_FLAG_CERT_DATE_INVALID) <> $0
          then
            vError := vError + 'Certificate Date invalid.';
          if (vFlags and WINHTTP_CALLBACK_STATUS_FLAG_CERT_WRONG_USAGE) <> $0
          then
            vError := vError + 'Certificate wrong usage.';
          if (vFlags and WINHTTP_CALLBACK_STATUS_FLAG_SECURITY_CHANNEL_ERROR) <> $0
          then
            vError := vError + 'Security channel error.';
          if vError <> '' then
            oClient.DoException(oClient.FWSConnection, vError);
        end;
      end;
    WINHTTP_CALLBACK_STATUS_HEADERS_AVAILABLE:
      begin
        oClient := GetAsyncClients.GetClientByHINTERNET(HINTERNET);
        if Assigned(oClient) then
        begin
          oClient.DoSetProtocol;
          if oClient.DoAsyncCompleteUpgrade then
          begin
            oClient.DoSetHeartBeat;
            oClient.DoAsyncWebSocketReceive;
            oClient.DoEventNotifyConnect;
          end;
        end;
      end;
    // WINHTTP_CALLBACK_STATUS_DATA_AVAILABLE:
    WINHTTP_CALLBACK_STATUS_READ_COMPLETE:
      begin
        oClient := GetAsyncClients.GetClientByHINTERNET(HINTERNET);
        if Assigned(oClient) then
        begin
          oClient.DoAsyncStatusReadComplete
            (LPWINHTTP_WEB_SOCKET_STATUS(lpvStatusInformation).eBufferType,
            LPWINHTTP_WEB_SOCKET_STATUS(lpvStatusInformation)
            .dwBytesTransferred);
          case LPWINHTTP_WEB_SOCKET_STATUS(lpvStatusInformation).eBufferType of
            WINHTTP_WEB_SOCKET_CLOSE_BUFFER_TYPE:
              oClient.DoDisconnectEvent
          else
            oClient.DoAsyncWebSocketReceive;
          end;
        end;
      end;
    // WINHTTP_CALLBACK_STATUS_WRITE_COMPLETE:
    WINHTTP_CALLBACK_STATUS_REQUEST_ERROR:
      begin
        oClient := GetAsyncClients.GetClientByHINTERNET(HINTERNET);
        if Assigned(oClient) then
          oClient.DoEventNotifyError
            (LPWINHTTP_WEB_SOCKET_ASYNC_RESULT(lpvStatusInformation)
            .AsyncResult, LPWINHTTP_WEB_SOCKET_ASYNC_RESULT
            (lpvStatusInformation).Operation);
      end;
    WINHTTP_CALLBACK_STATUS_SENDREQUEST_COMPLETE:
      begin
        oClient := GetAsyncClients.GetClientByHINTERNET(HINTERNET);
        if Assigned(oClient) then
          oClient.DoResponseGET;
      end;
    // WINHTTP_CALLBACK_STATUS_GETPROXYFORURL_COMPLETE:
    WINHTTP_CALLBACK_STATUS_CLOSE_COMPLETE:
      begin
        oClient := GetAsyncClients.GetClientByHINTERNET(HINTERNET);
        if Assigned(oClient) then
        begin
          oClient.DoEventNotifyDisconnect;
          oClient.DoClear;
        end;
      end;
    // WINHTTP_CALLBACK_STATUS_SHUTDOWN_COMPLETE:
  end;
end;

function TsgcWSClient_WinHTTP.Connect(const aTimeout: Integer = 10000): Boolean;
var
  vStart: Cardinal;
begin
  Active := True;

  result := False;

  if (not FConnecting) and (not Assigned(FWSConnection)) then
    exit;
  // ... wait connect
  vStart := sgcGetTicks;
  if aTimeout > 0 then
  begin
    repeat
      if (not FConnecting) and (not Assigned(FWSConnection)) then
        break;
      if Assigned(FWSConnection) then
      begin
        if FWSConnection.Enabled then
        begin
          result := True;
          break;
        end;
      end;
      sleep(1);
    until sgcGetTickDiff(vStart, sgcGetTicks) >= Cardinal(aTimeout);
  end;
end;

function TsgcWSClient_WinHTTP.Disconnect(const aTimeout: Integer = 10000):
    Boolean;
var
  vStart: Cardinal;
begin
  Active := False;

  result := False;
  // ... wait disconnect
  vStart := sgcGetTicks;
  if aTimeout > 0 then
  begin
    repeat
      if not Assigned(FWSConnection) then
      begin
        result := True;
        break;
      end;
      sleep(1);
    until sgcGetTickDiff(vStart, sgcGetTicks) >= Cardinal(aTimeout);
  end;
end;

function TsgcWSClient_WinHTTP.DoAsyncCompleteUpgrade: Boolean;
begin
  result := False;
  Try
    DoCompleteUpgrade;
    result := True;
  Except
    On E: Exception do
      DoException(FWSConnection, E.Message);
  end;
end;

procedure TsgcWSClient_WinHTTP.DoAsyncConnect;
begin
  DoCreateConnection;
  DoHttpOpen;
  DoAsyncStatusCallback;
  DoHttpConnect;
  DoHttpSecure;
  DoHttpTimeout;
  DoRequestUpgrade;
  DoDisableTLSValidations;
  DoDisableRedirects;
  DoDisableCookies;
  DoSetProxy;
  DoCustomHeaders;
  DoRequestGET;
end;

procedure TsgcWSClient_WinHTTP.DoAsyncStatusCallback;
begin
  WinHttpSetStatusCallback(FHTTPSession, OnStatusCallback,
    WINHTTP_CALLBACK_FLAG_ALL_NOTIFICATIONS, 0);
end;

procedure TsgcWSClient_WinHTTP.DoCompleteUpgrade;
begin
  FWSSession := WinHttpWebSocketCompleteUpgrade(FHTTPRequest, 0);
  if FWSSession = nil then
    raise TsgcWSException.Create(SysErrorMessage(GetLastError));

  // ... assign session to connection
  FWSConnection.WSSession := FWSSession;
end;

procedure TsgcWSClient_WinHTTP.DoConnect;
begin
  FConnecting := True;

  if Asynchronous then
    DoAsyncConnect
  else
    DoSyncConnect;
end;

procedure TsgcWSClient_WinHTTP.DoCreateConnection;
begin
  FWSConnection := TsgcWSConnectionClient_WinHTTP.Create;
  FWSConnection.Protocol := GetProtocols;
  FWSConnection.Options := Options;
  FWSConnection.Authentication := Authentication;
end;

procedure TsgcWSClient_WinHTTP.DoCustomHeaders;
begin
  // ... custom header: PROTOCOL
  if FWSConnection.Protocol <> '' then
    WinHttpAddRequestHeaders(FHTTPRequest,
      PWideChar(WideString('Sec-WebSocket-Protocol: ' + FWSConnection.Protocol)
      ), $FFFFFFFF, WINHTTP_ADDREQ_FLAG_COALESCE_WITH_COMMA);

  // ... custom header: ORIGIN
  if FWSConnection.Options.Origin <> '' then
    WinHttpAddRequestHeaders(FHTTPRequest,
      PWideChar(WideString('Origin: ' + FWSConnection.Options.Origin)),
      $FFFFFFFF, WINHTTP_ADDREQ_FLAG_COALESCE_WITH_COMMA)
  else
    WinHttpAddRequestHeaders(FHTTPRequest,
      PWideChar(WideString('Origin: ' + Host)), $FFFFFFFF,
      WINHTTP_ADDREQ_FLAG_COALESCE_WITH_COMMA);

  // ... custom header: USER-AGENT
  WinHttpAddRequestHeaders(FHTTPRequest,
    PWideChar(WideString('User-agent' + ': ' + CS_APPLICATION_NAME + ' ' +
    CS_VERSION)), $FFFFFFFF, WINHTTP_ADDREQ_FLAG_COALESCE_WITH_COMMA);

  // ... custom header: AUTHORIZATION-BASIC
  if Authentication.Enabled then
  begin
    if Authentication.Basic.Enabled then
      WinHttpAddRequestHeaders(FHTTPRequest,
        PWideChar(WideString(CS_AUTH_BASIC + ' ' +
        EncodeBase64(Authentication.User + ':' + Authentication.Password))),
        $FFFFFFFF, WINHTTP_ADDREQ_FLAG_COALESCE_WITH_COMMA);
  end;
end;

procedure TsgcWSClient_WinHTTP.DoDisableCookies;
var
  vStatus: Boolean;
  vOptionValue: DWORD;
begin
  vOptionValue := WINHTTP_DISABLE_COOKIES;
  vStatus := WinHttpSetOption(FHTTPRequest, WINHTTP_OPTION_DISABLE_FEATURE,
    @vOptionValue, sizeof(vOptionValue));
  if not vStatus then
    raise TsgcWSException.Create(SysErrorMessage(GetLastError));
end;

procedure TsgcWSClient_WinHTTP.DoDisableRedirects;
var
  vStatus: Boolean;
  vOptionValue: DWORD;
begin
  vOptionValue := WINHTTP_DISABLE_REDIRECTS;
  vStatus := WinHttpSetOption(FHTTPRequest, WINHTTP_OPTION_DISABLE_FEATURE,
    @vOptionValue, sizeof(vOptionValue));
  if not vStatus then
    raise TsgcWSException.Create(SysErrorMessage(GetLastError));
end;

procedure TsgcWSClient_WinHTTP.DoDisconnect;
begin
  if Active then
    FWSConnection.Disconnect;
end;

procedure TsgcWSClient_WinHTTP.DoDisconnectEvent;
begin
  DoQueryCloseStatus;
  DoNotifyDisconnect(FWSConnection);

  DoReadStop;
  DoClear;
end;

procedure TsgcWSClient_WinHTTP.DoEventDisconnect(aConnection: TsgcWSConnection;
  Code: Integer);
begin
  if IsDestroying then
    exit;

  DoStopHeartBeatTimeOut;
  DoStopHeartBeat;

  inherited;

  if FWatchDogEnabled then
    DoStartWatchDog;
end;

procedure TsgcWSClient_WinHTTP.DoEventNotifyConnect;
begin
  DoNotifyConnect(FWSConnection);
end;

procedure TsgcWSClient_WinHTTP.DoEventNotifyDisconnect;
begin
  if IsDestroying then
    exit;

  DoNotifyDisconnect(FWSConnection);
end;

procedure TsgcWSClient_WinHTTP.DoEventNotifyError
  (AsyncResult: WINHTTP_ASYNC_RESULT; Operation: WINHTTP_WEB_SOCKET_OPERATION);
var
  vError: cardinal;
begin
  if Assigned(FWSConnection) then
  begin
    case AsyncResult.dwResult of
      API_RECEIVE_RESPONSE:
        FWSConnection.MsgError := S_API_RECEIVE_RESPONSE;
      API_QUERY_DATA_AVAILABLE:
        FWSConnection.MsgError := S_API_QUERY_DATA_AVAILABLE;
      API_READ_DATA:
        FWSConnection.MsgError := S_API_READ_DATA;
      API_WRITE_DATA:
        FWSConnection.MsgError := S_API_WRITE_DATA;
      API_SEND_REQUEST:
        FWSConnection.MsgError := S_API_SEND_REQUEST;
    else
      begin
        DoEventNotifyDisconnect;
        DoClear;
        exit;
      end;
    end;

    if FWSConnection.MsgError <> '' then
      FWSConnection.MsgError := '[' + FWSConnection.MsgError + ']';

    vError := GetLastError;
    case vError of
      ERROR_WINHTTP_CANNOT_CONNECT:
        FWSConnection.MsgError := FWSConnection.MsgError + ' ' +
          S_ERROR_CONNECTING_SERVER;
      ERROR_WINHTTP_OPERATION_CANCELLED:
        FWSConnection.MsgError := FWSConnection.MsgError + ' ' +
          S_ERROR_OPERATION_CANCELLED
    else
      FWSConnection.MsgError := FWSConnection.MsgError + ' ' +
        Format(S_ERROR_SYSTEM_CODE, [GetLastError]);
    end;
    DoNotifyError(FWSConnection);
    // ... clear internal connection if cannot connect
    if vError = ERROR_WINHTTP_CANNOT_CONNECT then
      DoClear;
  end;
end;

procedure TsgcWSClient_WinHTTP.DoFinWatchDog;
begin
  FWatchDogEnabled := False;
end;

procedure TsgcWSClient_WinHTTP.DoHttpConnect;
begin
  FHTTPHandle := WinHttpConnect(FHTTPSession, PWideChar(WideString(Host)
    ), Port, 0);
  if FHTTPHandle = nil then
    raise TsgcWSException.Create(S_ERROR_CONNECTING_SERVER);
end;

procedure TsgcWSClient_WinHTTP.DoHttpOpen;
var
  vFlags: DWORD;
begin
  vFlags := 0;
  if Asynchronous then
    vFlags := WINHTTP_FLAG_ASYNC;
  FHTTPSession := WinHttpOpen(PWideChar(WideString('')),
    WINHTTP_ACCESS_TYPE_NO_PROXY, WINHTTP_NO_PROXY_NAME,
    WINHTTP_NO_PROXY_BYPASS, vFlags);
  if FHTTPSession = nil then
    raise TsgcWSException.Create(S_ERROR_SESSION_HANDLE);
end;

procedure TsgcWSClient_WinHTTP.DoHttpSecure;
var
  vOptions: DWORD;
begin
  // ... secure
  vOptions := 0;
  if TLS then
    vOptions := vOptions or WINHTTP_FLAG_SECURE;

  // ... open request
  FHTTPRequest := WinHttpOpenRequest(FHTTPHandle, 'GET',
    PWideChar(WideString(Options.Parameters)), nil, WINHTTP_NO_REFERER,
    WINHTTP_DEFAULT_ACCEPT_TYPES, vOptions);
  if FHTTPRequest = nil then
    raise TsgcWSException.Create(S_ERROR_OPEN_REQUEST);
end;

procedure TsgcWSClient_WinHTTP.DoHttpTimeout;
begin
  if WinHttpSetTimeouts(FHTTPRequest, 0, ConnectTimeout, ReadTimeout,
    ReadTimeout) = False then
    raise TsgcWSException.Create(S_ERROR_RESPONSE_DATA);
end;

procedure TsgcWSClient_WinHTTP.DoInitWatchDog;
begin
  FWatchDogEnabled := WatchDog.Enabled;
end;

procedure TsgcWSClient_WinHTTP.DoNotifyDisconnect
  (aConnection: TsgcWSConnection);
begin
  FDisconnecting := True;
  Try
    inherited;
    FWSConnection := nil;
  Finally
    FDisconnecting := False;
  End;
end;

procedure TsgcWSClient_WinHTTP.DoSetProxy;
var
  vOptionValue: DWORD;
  oProxyInfo: TWinHttpProxyInfo;
begin
  if Proxy.Enabled then
  begin
    oProxyInfo.dwAccessType := WINHTTP_ACCESS_TYPE_NAMED_PROXY;
    oProxyInfo.lpszProxy := PWideChar('http://' + Proxy.Host + ':' +
      IntToStr(Proxy.Port));
    oProxyInfo.lpszProxyBypass := PWideChar('');
    vOptionValue := sizeof(oProxyInfo);
    WinHttpSetOption(FHTTPRequest, WINHTTP_OPTION_PROXY, @oProxyInfo,
      vOptionValue);
    if Proxy.Username <> '' then //
      WinHttpSetCredentials(FHTTPRequest, WINHTTP_AUTH_TARGET_PROXY,
        WINHTTP_AUTH_SCHEME_BASIC, PWideChar(Proxy.Username),
        PWideChar(Proxy.Password), nil);
  end;
end;

procedure TsgcWSClient_WinHTTP.DoReadStart;
begin
  sgcFree(FReadThread);
  FReadThread := TsgcWSReadThread.Create(True);
  FReadThread.Session := FWSSession;
  FReadThread.BufferSize := ReceiveBufferSize;
  FReadThread.OnReadMessage := OnReadMessageEvent;
  FReadThread.OnDisconnect := OnDisconnectEvent;
{$IFDEF D2010}
  FReadThread.Start;
{$ELSE}
  FReadThread.Resume;
{$ENDIF}
end;

procedure TsgcWSClient_WinHTTP.DoReadStop;
begin
  if Assigned(FReadThread) then
  begin
    if not FReadThread.Terminated then
    begin
      FReadThread.Terminate;
      if Asynchronous then
        FReadThread.WaitFor;
    end;
  end;
end;

procedure TsgcWSClient_WinHTTP.DoAsyncStatusReadComplete(const aBufferType
  : WINHTTP_WEB_SOCKET_BUFFER_TYPE; const aBufferSize: Cardinal);
var
  vMessage: TsgcArrayOfBytes;
begin
  SetLength(vMessage, aBufferSize);
  sgcMove(FAsyncBuffer[0], vMessage[0], aBufferSize);

  DoReadMessageEvent(vMessage, aBufferType, aBufferSize);
end;

procedure TsgcWSClient_WinHTTP.DoRequestGET;
var
  vStatus: Boolean;
begin
  vStatus := WinHttpSendRequest(FHTTPRequest, WINHTTP_NO_ADDITIONAL_HEADERS, 0,
    WINHTTP_NO_REQUEST_DATA, 0, 0, 0);
  if not vStatus then
  begin
    case GetLastError of
      ERROR_WINHTTP_SECURE_FAILURE:
        if not DoValidateServerCertificate then
          raise TsgcWSException.Create(S_INVALID_SERVER_CERTIFICATE);
      ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED:
        raise TsgcWSException.Create(S_INVALID_CLIENT_CERTIFICATE);
      ERROR_WINHTTP_CANNOT_CONNECT:
        raise TsgcWSException.Create(S_ERROR_CONNECTING_SERVER);
    else
      raise TsgcWSException.Create(SysErrorMessage(GetLastError));
    end;
  end;
end;

procedure TsgcWSClient_WinHTTP.DoRequestUpgrade;
var
  vStatus: Boolean;
begin
  vStatus := WinHttpSetOption(FHTTPRequest,
    WINHTTP_OPTION_UPGRADE_TO_WEB_SOCKET, nil, 0);
  if not vStatus then
    raise TsgcWSException.Create(SysErrorMessage(GetLastError));
end;

procedure TsgcWSClient_WinHTTP.DoSetHeartBeat;
var
  vStatus: Boolean;
  vOptionValue: DWORD;
begin
  if HeartBeat.Enabled then
  begin
    // ... minimum interval 15000 ms
    if HeartBeat.Interval >= 15 then
      vOptionValue := HeartBeat.Interval * 1000
    else
      vOptionValue := 15000;
  end
  else
    vOptionValue := 0;
  vStatus := WinHttpSetOption(FWSSession,
    WINHTTP_OPTION_WEB_SOCKET_KEEPALIVE_INTERVAL, @vOptionValue,
    sizeof(vOptionValue));
  if not vStatus then
    raise TsgcWSException.Create(SysErrorMessage(GetLastError));
end;

procedure TsgcWSClient_WinHTTP.DoSetProtocol;
var
  vBuffer: TsgcArrayOfBytes;
  vBufferSize: DWORD;
  vStatus: Boolean;
begin
  vBufferSize := ReceiveBufferSize;
  SetLength(vBuffer, vBufferSize);
  vStatus := WinHttpQueryHeaders(FHTTPRequest, 65535,
    PWideChar(WideString('Sec-WebSocket-Protocol')), vBuffer,
    @vBufferSize, nil);
  if vStatus then
    FWSConnection.Protocol := sgcArrayOfBytesToStringWithoutNull(vBuffer);
end;

procedure TsgcWSClient_WinHTTP.DoStart;
begin
  Active := True;
end;

procedure TsgcWSClient_WinHTTP.DoSyncConnect;
begin
  DoCreateConnection;
  DoHttpOpen;
  DoHttpConnect;
  DoHttpSecure;
  DoHttpTimeout;
  DoRequestUpgrade;
  DoDisableRedirects;
  DoDisableCookies;
  DoSetProxy;
  DoCustomHeaders;
  DoRequestGET;
  DoResponseGET;
  DoSetProtocol;
  DoCompleteUpgrade;
  DoSetHeartBeat;
end;

function TsgcWSClient_WinHTTP.DoValidateServerCertificate: Boolean;
var
  vFlags: DWORD;
begin
  vFlags := WINHTTP_SECURITY_FLAG_IGNORE_UNKNOWN_CA or
    WINHTTP_SECURITY_FLAG_IGNORE_CERT_WRONG_USAGE or
    WINHTTP_SECURITY_FLAG_IGNORE_CERT_CN_INVALID or
    WINHTTP_SECURITY_FLAG_IGNORE_CERT_DATE_INVALID;

  WinHttpSetOption(FHTTPRequest, WINHTTP_OPTION_SECURITY_FLAGS, @vFlags,
    sizeof(vFlags));

  result := WinHttpSendRequest(FHTTPRequest, WINHTTP_NO_ADDITIONAL_HEADERS, 0,
    WINHTTP_NO_REQUEST_DATA, 0, 0, 0);
end;

procedure TsgcWSClient_WinHTTP.DoAsyncWebSocketReceive;
var
  dwError: Cardinal;
begin
  if Assigned(FWSSession) then
  begin
    SetLength(FAsyncBuffer, ReceiveBufferSize);
    // ... receive data
    dwError := WinHttpWebSocketReceive(FWSSession, FAsyncBuffer, ReceiveBufferSize,
      nil, nil);
    // ... check if error
    if dwError <> 0 then
    begin
      case dwError of
        ERROR_WINHTTP_OPERATION_CANCELLED:
          exit;
        ERROR_WINHTTP_CONNECTION_ERROR:
          begin
            DoDisconnectEvent;
            exit;
          end
      else
        raise Exception.Create(SysErrorMessage(dwError));
      end;
    end;
  end;
end;

procedure TsgcWSClient_WinHTTP.DoBeforeConnect;
begin
  if Assigned(FOnBeforeConnect) then
    FOnBeforeConnect(self);
end;

procedure TsgcWSClient_WinHTTP.DoDisableTLSValidations;
var
  vOptions: DWORD;
begin
  vOptions := WINHTTP_SECURITY_FLAG_IGNORE_UNKNOWN_CA or
    WINHTTP_SECURITY_FLAG_IGNORE_CERT_DATE_INVALID or
    WINHTTP_SECURITY_FLAG_IGNORE_CERT_CN_INVALID or
    WINHTTP_SECURITY_FLAG_IGNORE_CERT_WRONG_USAGE;

  WinHttpSetOption(FHTTPRequest, WINHTTP_OPTION_SECURITY_FLAGS, @vOptions,
    sizeof(vOptions));
end;

procedure TsgcWSClient_WinHTTP.DoNotifyConnect(aConnection: TsgcWSConnection);
begin
  TsgcWSConnectionClient_WinHTTP(aConnection).FEnabled := True;
  FConnecting := False;
  inherited;
end;

procedure TsgcWSClient_WinHTTP.DoQueryCloseStatus;
var
  vBufferSize: DWORD;
  dwError: Cardinal;
  vBuffer: TsgcArrayOfBytes;
  oStatus: WINHTTP_WEB_SOCKET_CLOSE_STATUS;
  vPBufferLength: PDWORD;
begin
  vBufferSize := ReceiveBufferSize;
  SetLength(vBuffer, vBufferSize);
  GetMem(vPBufferLength, sizeof(DWORD));

  dwError := WinHttpWebSocketQueryCloseStatus(FWSSession, @oStatus, vBuffer,
    Length(vBuffer), vPBufferLength);
  if dwError = 0 then
    FWSConnection.CloseCode := Ord(oStatus)
  else
  begin
    case dwError of
      ERROR_INSUFFICIENT_BUFFER:
        exit; // There is not enough space in pvReason to write the whole close reason.
      ERROR_INVALID_PARAMETER:
        exit; // A parameter is invalid.
      ERROR_WINHTTP_INVALID_OPERATION:
        exit; // No close frame has been received yet.
    else
      raise Exception.Create(SysErrorMessage(dwError));
    end;
  end;
end;

function TsgcWSClient_WinHTTP.GetActive: Boolean;
begin
  inherited;
  if not IsDesigning and not IsLoading then
  begin
    result := False;
    if Assigned(FWSConnection) then
      if Assigned(FWSConnection.WSSession) then
        result := True;
  end
  else
    result := FActive;
end;

function TsgcWSClient_WinHTTP.GetExtensions: TsgcWSExtensions;
begin
  result := inherited GetExtensions;
  result.DeflateFrame.Enabled := False;
  result.PerMessage_Deflate.Enabled := False;
end;

function TsgcWSClient_WinHTTP.GetHeartBeat: TsgcWSHeartBeat_Options;
begin
  result := inherited GetHeartBeat;
  result.Timeout := 0;
  if result.Interval > 0 then
    if result.Interval < 15 then
      result.Interval := 15;
end;

function TsgcWSClient_WinHTTP.GetLogFile: TsgcWSLogFile;
begin
  result := inherited GetLogFile;
  result.Enabled := False;
end;

function TsgcWSClient_WinHTTP.GetSpecifications: TsgcWSSpecifications;
begin
  result := inherited GetSpecifications;
  result.Drafts.Hixie76 := False;
  result.RFC6455 := True;
end;

procedure TsgcWSClient_WinHTTP.Loaded;
begin
  inherited;
  if Active <> FActive then
    Active := FActive;
end;

procedure TsgcWSClient_WinHTTP.OnDisconnectEvent(Sender: TObject);
begin
  DoDisconnectEvent;
end;

procedure TsgcWSClient_WinHTTP.OnHeartBeatEvent(Sender: TObject);
begin
  inherited;
  // implemented in winhttp api
end;

procedure TsgcWSClient_WinHTTP.OnHeartBeatTimeoutEvent(Sender: TObject);
begin
  inherited;
  // not implemented
end;

procedure TsgcWSClient_WinHTTP.DoReadMessageEvent(Buffer: TsgcArrayOfBytes;
  BufferType: WINHTTP_WEB_SOCKET_BUFFER_TYPE; BufferSize: DWORD);
var
  oStream: TMemoryStream;
begin
  if Assigned(FWSConnection) then
  begin
    // ... save read bytes
    FWSConnection.RecBytes := FWSConnection.RecBytes + Int64(BufferSize);
    // ... read buffer
    case BufferType of
      WINHTTP_WEB_SOCKET_BINARY_MESSAGE_BUFFER_TYPE:
        begin
          if FFragmentedBinary then
          begin
            FFragmentedBinary := False;
            // ... fragment
            if (Options.FragmentedMessages = frgAll) or
              (Options.FragmentedMessages = frgOnlyFragmented) then
            begin
              oStream := TMemoryStream.Create;
              Try
                oStream.Write(Buffer[0], BufferSize);
                DoNotifyFragmented(FWSConnection, oStream, opBinary, False);
              Finally
                sgcFree(oStream);
              end;
            end;
            // ... full message
            if (Options.FragmentedMessages = frgAll) or
              (Options.FragmentedMessages = frgOnlyBuffer) then
            begin
              DoWriteFragmentedBinary(Buffer);
              FWSConnection.MsgBinaryReceived.Clear;
              FWSConnection.MsgBinaryReceived.Write(FFragmentedBinaryBytes[0],
                Length(FFragmentedBinaryBytes));
              SetLength(FFragmentedBinaryBytes, 0);
              DoNotifyBinary(FWSConnection);
            end;
          end
          else
          begin
            FWSConnection.MsgBinaryReceived.Clear;
            FWSConnection.MsgBinaryReceived.Write(Buffer[0], BufferSize);
            DoNotifyBinary(FWSConnection);
          end;
        end;
      WINHTTP_WEB_SOCKET_BINARY_FRAGMENT_BUFFER_TYPE:
        begin
          FFragmentedBinary := True;
          case Options.FragmentedMessages of
            frgOnlyBuffer:
              DoWriteFragmentedBinary(Buffer);
            frgOnlyFragmented:
              begin
                FWSConnection.MsgBinaryReceived.Clear;
                FWSConnection.MsgBinaryReceived.Write(Buffer[0], BufferSize);
                DoNotifyFragmented(FWSConnection,
                  FWSConnection.MsgBinaryReceived, opBinary, True);
              end;
            frgAll:
              begin
                DoWriteFragmentedBinary(Buffer);
                oStream := TMemoryStream.Create;
                Try
                  oStream.Write(FFragmentedBinaryBytes[0],
                    Length(FFragmentedBinaryBytes));
                  DoNotifyFragmented(FWSConnection, oStream, opBinary, True);
                Finally
                  sgcFree(oStream);
                End;
              end;
          end;
        end;
      WINHTTP_WEB_SOCKET_UTF8_MESSAGE_BUFFER_TYPE:
        begin
          if FFragmentedUTF8 then
          begin
            FFragmentedUTF8 := False;
            // ... fragment
            if (Options.FragmentedMessages = frgAll) or
              (Options.FragmentedMessages = frgOnlyFragmented) then
            begin
              oStream := TMemoryStream.Create;
              Try
                oStream.Write(Buffer[0], BufferSize);
                DoNotifyFragmented(FWSConnection, oStream, opText, False);
              Finally
                sgcFree(oStream);
              End;
            end;
            // ... full message
            if (Options.FragmentedMessages = frgAll) or
              (Options.FragmentedMessages = frgOnlyBuffer) then
            begin
              DoWriteFragmentedUTF8(Buffer);
              FWSConnection.MsgReceived :=
                sgcArrayOfBytesToString(FFragmentedUTF8Bytes);
              SetLength(FFragmentedUTF8Bytes, 0);
              DoNotifyMessage(FWSConnection);
            end
          end
          else
          begin
            FWSConnection.MsgReceived := sgcArrayOfBytesToString(Buffer);
            DoNotifyMessage(FWSConnection);
          end;
        end;
      WINHTTP_WEB_SOCKET_UTF8_FRAGMENT_BUFFER_TYPE:
        begin
          FFragmentedUTF8 := True;
          case Options.FragmentedMessages of
            frgOnlyBuffer:
              DoWriteFragmentedUTF8(Buffer);
            frgOnlyFragmented:
              begin
                oStream := TMemoryStream.Create;
                Try
                  oStream.Write(Buffer[0], BufferSize);
                  DoNotifyFragmented(FWSConnection, oStream, opText, True);
                Finally
                  sgcFree(oStream);
                End;
              end;
            frgAll:
              begin
                DoWriteFragmentedUTF8(Buffer);
                oStream := TMemoryStream.Create;
                Try
                  oStream.Write(FFragmentedUTF8Bytes[0],
                    Length(FFragmentedUTF8Bytes));
                  DoNotifyFragmented(FWSConnection, oStream, opText, True);
                Finally
                  sgcFree(oStream);
                End;
              end;
          end;
        end;
      WINHTTP_WEB_SOCKET_CLOSE_BUFFER_TYPE:
        begin
          if not FWSConnection.Disconnected then
            FWSConnection.Close;
        end;
    end;
  end;
end;

procedure TsgcWSClient_WinHTTP.DoResponseGET;
var
  vStatus: Boolean;
begin
  vStatus := WinHttpReceiveResponse(FHTTPRequest, nil);
  if not vStatus then
  begin
    case GetLastError of
      ERROR_WINHTTP_SECURE_FAILURE:
        raise TsgcWSException.Create(S_INVALID_SERVER_CERTIFICATE);
      ERROR_WINHTTP_CLIENT_AUTH_CERT_NEEDED:
        raise TsgcWSException.Create(S_INVALID_CLIENT_CERTIFICATE);
      ERROR_WINHTTP_INVALID_SERVER_RESPONSE:
        raise TsgcWSException.Create(S_INVALID_SERVER_RESPONSE);
    else
      raise TsgcWSException.Create(SysErrorMessage(GetLastError));
    end;
  end;
end;

procedure TsgcWSClient_WinHTTP.DoWriteFragmentedBinary(const aBuffer
  : TsgcArrayOfBytes);
var
  i: Integer;
begin
  i := Length(FFragmentedBinaryBytes);
  SetLength(FFragmentedBinaryBytes, i + Length(aBuffer));
  sgcMove(aBuffer[0], FFragmentedBinaryBytes[i], Length(aBuffer));
end;

procedure TsgcWSClient_WinHTTP.DoWriteFragmentedUTF8(const aBuffer
  : TsgcArrayOfBytes);
var
  i: Integer;
begin
  i := Length(FFragmentedUTF8Bytes);
  SetLength(FFragmentedUTF8Bytes, i + Length(aBuffer));
  sgcMove(aBuffer[0], FFragmentedUTF8Bytes[i], Length(aBuffer));
end;

procedure TsgcWSClient_WinHTTP.OnHeartBeatExceptionEvent(Sender: TObject;
  E: Exception);
begin
  inherited;
  DoException(FWSConnection, E.Message, E);
end;

procedure TsgcWSClient_WinHTTP.OnHeartBeatTimeoutExceptionEvent(Sender: TObject;
  E: Exception);
begin
  inherited;
  DoException(FWSConnection, E.Message, E);
end;

procedure TsgcWSClient_WinHTTP.OnReadMessageEvent(Sender: TObject;
  Buffer: TsgcArrayOfBytes; BufferType: WINHTTP_WEB_SOCKET_BUFFER_TYPE;
  BufferSize: DWORD);
begin
  DoReadMessageEvent(Buffer, BufferType, BufferSize);
end;

procedure TsgcWSClient_WinHTTP.OnWatchDogEvent(Sender: TObject);
var
  vHandled: Boolean;
begin
  if IsDestroying then
    exit;

  vHandled := False;
  if Assigned(FOnBeforeWatchDog) then
    FOnBeforeWatchDog(self, vHandled);
  if not vHandled then
  begin
    case NotifyEvents of
      neNoSync:
        begin
          if not Assigned(FWSConnection) then
            Start;
        end;
    else
      begin
        // ... if client starts connection in a thread, watchdog too
        if FThreadStart then
        begin
          if not Assigned(FWSConnection) then
            Start;
        end
        else
        begin
          if not Active then
            NotifyMethod(DoStart);
        end;
      end;
    end;
  end;
  inherited; // after set active, to stop watchdog if needed
end;

procedure TsgcWSClient_WinHTTP.OnWatchDogExceptionEvent(Sender: TObject;
  E: Exception);
begin
  inherited;
  DoException(FWSConnection, E.Message, E);
end;

procedure TsgcWSClient_WinHTTP.Ping;
begin
  if Assigned(FWSConnection) then
    FWSConnection.Ping('');
end;

procedure TsgcWSClient_WinHTTP.SetActive(const Value: Boolean);
begin
  inherited;
  Try
    if not IsDesigning and not IsLoading then
    begin
      if Value then
      begin
        DoBeforeConnect;
        DoInitWatchDog;
        DoClear;
        DoConnect;
        if not Asynchronous then
        begin
          DoReadStart;
          DoEventNotifyConnect;
        end;
      end
      else
      begin
        DoFinWatchDog;
        if not FDisconnecting then
          DoDisconnect;
      end;
    end
    else
      FActive := Value;
  Except
    On E: Exception do
    begin
      FConnecting := False;
      // ... exception
      DoException(FWSConnection, E.Message, E);
      // ... watchdog
      if FWatchDogEnabled then
      begin
        DoClear;
        DoStartWatchDog;
      end;
    end;
  End;
end;

procedure TsgcWSClient_WinHTTP.SetAsynchronous(const Value: Boolean);
begin
  FAsynchronous := Value;
  if FAsynchronous then
    GetAsyncClients.AddClient(self)
  else
    GetAsyncClients.DeleteClient(self);
end;

procedure TsgcWSClient_WinHTTP.SetAuthentication(const Value
  : TsgcWSAuthenticationClient_Options);
begin
  FAuthentication.Assign(Value);
end;

procedure TsgcWSClient_WinHTTP.SetOptions(const Value: TsgcWSOptionsClient);
begin
  FOptions.Assign(Value);
end;

procedure TsgcWSClient_WinHTTP.SetProxy(const Value: TsgcWSProxy_Options);
begin
  FProxy.Assign(Value);
end;

procedure TsgcWSClient_WinHTTP.SetURL(const Value: String);
var
  vProtocol, vHost, vPort, vPath, vQuery: String;
begin
  inherited;
  ParseURL(Value, vProtocol, vHost, vPort, vPath, vQuery);

  Host := vHost;
  if vProtocol = 'wss' then
  begin
    TLS := True;
    if vPort <> '' then
      Port := StrToInt(vPort)
    else
      Port := 443;
  end
  else
  begin
    TLS := False;
    if vPort <> '' then
      Port := StrToInt(vPort)
    else
      Port := 80;
  end;
  Options.Parameters := vPath;
  if vQuery <> '' then
  begin
    if (RightStr(Options.Parameters, 1) = '/') and
      (Length(Options.Parameters) > 1) then
      Options.Parameters := LeftStr(Options.Parameters,
        Length(Options.Parameters) - 1);

    Options.Parameters := Options.Parameters + '?' + vQuery;
  end;
end;

procedure TsgcWSClient_WinHTTP.Start;
begin
  if IsDestroying then
    exit;

  FThreadStart := True;
  TClientWinhttpThread.Start(self);
end;

procedure TsgcWSClient_WinHTTP.Stop;
begin
  if IsDestroying then
    exit;

  TClientWinhttpThread.Stop(self);
end;

function TsgcWSClient_WinHTTP.WriteAndWaitData(const aText: String;
  const aTimeout: Integer = 10000): string;
begin
  if Active then
    FWSConnection.WriteAndWaitData(aText, aTimeout);
end;

procedure TsgcWSClient_WinHTTP.WriteData(const aText: String;
  const aSize: Integer = 0; const aStreaming: TwsStreaming = stmNone);
begin
  if Active then
    FWSConnection.WriteData(aText, aSize, aStreaming);
end;

procedure TsgcWSClient_WinHTTP.WriteData(const aStream: TStream;
  const aSize: Integer = 0; const aStreaming: TwsStreaming = stmNone);
begin
  if Active then
    FWSConnection.WriteData(aStream, aSize, aStreaming);
end;

procedure TsgcWSReadThread.Execute;
var
  dwError: Cardinal;
  vBuffer, vMessage: TsgcArrayOfBytes;
  vPBytesTransferred: PDWORD;
  vBytesTransferred: DWORD;
  vBufferType: WINHTTP_WEB_SOCKET_BUFFER_TYPE;
  i, j: Integer;
  vLength: LongInt;
begin
  while not Terminated do
  begin
    GetMem(vPBytesTransferred, sizeof(DWORD));
    Try
      i := 0;
      repeat
        SetLength(vBuffer, 0);
        SetLength(vBuffer, BufferSize);
        // ... receive data
        dwError := WinHttpWebSocketReceive(Session, vBuffer, BufferSize,
          vPBytesTransferred, @vBufferType);
        // ... check if error
        if dwError <> 0 then
        begin
          case dwError of
            ERROR_WINHTTP_OPERATION_CANCELLED:
              exit;
            ERROR_WINHTTP_CONNECTION_ERROR:
              begin
                if Assigned(FOnDisconnect) then
                  FOnDisconnect(self);
                exit;
              end;
            ERROR_WINHTTP_INVALID_OPERATION:
              begin
                if Assigned(FOnDisconnect) then
                  FOnDisconnect(self);
                exit;
              end
          else
            raise Exception.Create(SysErrorMessage(dwError));
          end;
        end;
        vBytesTransferred := vPBytesTransferred^;

        // ... if first packet, copy to vMessage only bytes transferred
        if i = 0 then
        begin
          SetLength(vMessage, vBytesTransferred);
          sgcMove(vBuffer[0], vMessage[0], vBytesTransferred);
        end
        // ... if fragment packets, resize message and buffer
        else
        begin
          // ... if bytes transferred < buffer_size, resize buffer
          if vBytesTransferred < BufferSize then
            SetLength(vBuffer, vBytesTransferred);

          // ... copy data to vMessage
          vLength := Length(vMessage);
          SetLength(vMessage, vLength + Length(vBuffer));
          for j := 0 to Length(vBuffer) - 1 do
            vMessage[vLength + j] := vBuffer[j]
        end;

        inc(i);
      until (vBufferType <> WINHTTP_WEB_SOCKET_BINARY_FRAGMENT_BUFFER_TYPE) and
        (vBufferType <> WINHTTP_WEB_SOCKET_UTF8_FRAGMENT_BUFFER_TYPE);

      // ... OnReadMessage
      if Assigned(FOnReadMessage) then
        FOnReadMessage(self, vMessage, vBufferType, Length(vMessage));
    Finally
      FreeMem(vPBytesTransferred);
      SetLength(vBuffer, 0);
      SetLength(vMessage, 0);
    End;
  end;
end;

function TsgcWSReadThread.GetBufferSize: DWORD;
begin
  if FBufferSize = 0 then
    FBufferSize := 1024;
  result := FBufferSize;
end;

constructor TsgcWSOptionsClient.Create;
begin
  inherited;
  FFragmentedMessages := frgOnlyBuffer;
end;

procedure TsgcWSOptionsClient.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSOptionsClient then
  begin
    FParameters := TsgcWSOptionsClient(aSource).Parameters;
    FOrigin := TsgcWSOptionsClient(aSource).Origin;
    FFragmentedMessages := TsgcWSOptionsClient(aSource).FragmentedMessages;
  end
  else
    inherited Assign(aSource);
end;

function TsgcWSOptionsClient.GetParameters: String;
begin
  if LeftStr(FParameters, 1) <> '/' then
    FParameters := '/' + FParameters;
  result := FParameters;
end;

constructor TsgcWSConnectionClient_WinHTTP.Create;
begin
  inherited;
  FOptions := TsgcWSOptionsClient.Create;
  FAuthentication := TsgcWSAuthenticationClient_Options.Create;
end;

destructor TsgcWSConnectionClient_WinHTTP.Destroy;
begin
  sgcFree(FAuthentication);
  sgcFree(FOptions);
  inherited;
end;

procedure TsgcWSConnectionClient_WinHTTP.SetAuthentication
  (const Value: TsgcWSAuthenticationClient_Options);
begin
  if Assigned(FAuthentication) then
    FAuthentication.Assign(Value);
end;

procedure TsgcWSConnectionClient_WinHTTP.SetOptions
  (const Value: TsgcWSOptionsClient);
begin
  if Assigned(FOptions) then
    FOptions.Assign(Value);
end;

constructor TsgcWSAuthenticationClient_Options.Create;
begin
  FBasic := TsgcWSAuthenticationClient_Basic.Create;
end;

destructor TsgcWSAuthenticationClient_Options.Destroy;
begin
  sgcFree(FBasic);
  inherited;
end;

procedure TsgcWSAuthenticationClient_Options.Assign(aSource: TPersistent);
begin
  inherited Assign(aSource);
  if aSource is TsgcWSAuthenticationClient_Options then
  begin
    User := TsgcWSAuthenticationClient_Options(aSource).User;
    Password := TsgcWSAuthenticationClient_Options(aSource).Password;

    FBasic.Assign(TsgcWSAuthenticationClient_Options(aSource).Basic);
  end
end;

procedure TsgcWSAuthenticationClient_Options.SetBasic
  (const Value: TsgcWSAuthenticationClient_Basic);
begin
  FBasic.Assign(Value);
end;

procedure TsgcWSProxy_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSProxy_Options then
  begin
    Enabled := TsgcWSProxy_Options(aSource).Enabled;
    Host := TsgcWSProxy_Options(aSource).Host;
    Port := TsgcWSProxy_Options(aSource).Port;
    Username := TsgcWSProxy_Options(aSource).Username;
    Password := TsgcWSProxy_Options(aSource).Password;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcWSAsyncClient.Create;
begin
  inherited;
end;

destructor TsgcWSAsyncClient.Destroy;
begin
  FClient := nil;
  inherited;
end;

procedure TsgcWSAsyncClients.AddClient(const aClient: TsgcWSClient_WinHTTP);
var
  oClient: TsgcWSAsyncClient;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  DeleteClient(aClient);

  oList := LockList;
  Try
    oClient := TsgcWSAsyncClient.Create;
    oClient.Client := aClient;
    oList.Add(oClient);
  Finally
    UnLockList;
  End;
end;

procedure TsgcWSAsyncClients.DeleteClient(const aClient: TsgcWSClient_WinHTTP);
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
  oObject: TObject;
begin
  oList := LockList;
  Try
    for i := 0 to oList.Count - 1 do
    begin
      if TsgcWSAsyncClient(oList[i]).Client = aClient then
      begin
        oObject := TObject(oList.Items[i]);
        sgcFree(oObject);
        oList.Delete(i);
        break;
      end;
    end;
  Finally
    UnLockList;
  End;
end;

function TsgcWSAsyncClients.GetClientByHINTERNET(aHINTERNET: HINTERNET)
  : TsgcWSClient_WinHTTP;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  result := nil;

  oList := LockList;
  Try
    for i := 0 to oList.Count - 1 do
    begin
      if TsgcWSAsyncClient(oList[i]).Client.FHTTPRequest = aHINTERNET then
      begin
        result := TsgcWSAsyncClient(oList[i]).Client;
        break;
      end
      else if TsgcWSAsyncClient(oList[i]).Client.FWSSession = aHINTERNET then
      begin
        result := TsgcWSAsyncClient(oList[i]).Client;
        break;
      end
    end;
  Finally
    UnLockList;
  End;
end;

constructor TClientWinhttpThread.Create(const aMethod: string; const aClient:
    TsgcWSClient_WinHTTP);
begin
  FMethod := aMethod;
  FClient := aClient;
  FreeOnTerminate := True;
  inherited Create(False);
{$IFDEF DEBUG}
  sgcSetThreadName(ClassName);
{$ENDIF}
end;

procedure TClientWinhttpThread.Execute;
begin
  inherited;

  if Assigned(FClient) then
  begin
    // ... Stop
    if FMethod = CS_START then
    begin
      if FClient.DoAssignedCS then
      begin
        FClient.EnterCS;
        Try
          FClient.Active := True
        Finally
          FClient.LeaveCS;
        End;
      end;
    end
    // ... Start
    else if FMethod = CS_STOP then
    begin
      if FClient.DoAssignedCS then
      begin
        FClient.EnterCS;
        Try
          FClient.Active := False;
        Finally
          FClient.LeaveCS;
        End;
      end;
    end;
    Terminate;
  end;
end;

class procedure TClientWinhttpThread.Start(const aClient: TsgcWSClient_WinHTTP);
begin
  Create(CS_START, aClient);
end;

class procedure TClientWinhttpThread.Stop(const aClient: TsgcWSClient_WinHTTP);
begin
  Create(CS_STOP, aClient);
end;

initialization

finalization

sgcFree(oAsyncClients);

{$ENDIF}

end.
