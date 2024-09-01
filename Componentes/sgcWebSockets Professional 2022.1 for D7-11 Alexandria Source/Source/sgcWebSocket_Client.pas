{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Client;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils, Math, StrUtils,
{$IFDEF NEXTGEN} System.Generics.Collections, {$ELSE} Contnrs, {$ENDIF}
  // indy
{$IFDEF SGC_INDY} sgcIdTCPClient {$ELSE} IdTCPClient {$ENDIF},
{$IFDEF SGC_INDY} sgcIdTCPConnection {$ELSE} IdTCPConnection {$ENDIF},
{$IFDEF SGC_INDY} sgcIdGlobal {$ELSE} IdGlobal {$ENDIF},
{$IFDEF SGC_INDY} sgcIdComponent {$ELSE} IdComponent {$ENDIF},
{$IFDEF SGC_INDY} sgcIdHeaderList {$ELSE} IdHeaderList {$ENDIF},
{$IFDEF SGC_INDY} sgcIdIOHandler {$ELSE} IdIOHandler {$ENDIF},
{$IFDEF SGC_INDY} sgcIdIOHandlerSocket {$ELSE} IdIOHandlerSocket {$ENDIF},
{$IFDEF SGC_INDY} sgcIdIOHandlerStack {$ELSE} IdIOHandlerStack {$ENDIF},
{$IFDEF SGC_INDY} sgcIdSSL {$ELSE} IdSSL {$ENDIF},
{$IFDEF SGC_INDY} sgcIdSSLOpenSSL {$ELSE} IdSSLOpenSSL {$ENDIF},
{$IFDEF SGC_INDY} sgcIdSSLOpenSSLHeaders {$ELSE} IdSSLOpenSSLHeaders {$ENDIF},
{$IFDEF SGC_INDY} sgcIdConnectThroughHttpProxy
{$ELSE} IdConnectThroughHttpProxy {$ENDIF},
{$IFDEF SGC_INDY} sgcIdSocks {$ELSE} IdSocks {$ENDIF},
{$IFDEF SGC_INDY} sgcIdIntercept {$ELSE} IdIntercept {$ENDIF},
{$IFDEF SGC_INDY} sgcIdException {$ELSE} IdException {$ENDIF},
{$IFNDEF LAZARUS}
{$IFDEF SGC_INDY} sgcIdInterceptThrottler {$ELSE} IdInterceptThrottler {$ENDIF},
{$ENDIF}
{$IFDEF SGC_HTTP2} sgcHTTP2_HPACK, {$ENDIF}
  // websocket
  sgcWebSocket_Classes, sgcWebSocket_Classes_Indy, sgcWebSocket_CustomClient,
  sgcWebSocket_Types, sgcWebSocket_Const, sgcTCP_Client, sgcTCP_Classes,
  sgcHTTP_OAuth_Client, sgcSocket_Classes_Indy, sgcSocket_Classes,
  sgcHTTP_JWT_Client;

type
  TsgcWSOnSSLGetHandler = procedure(Sender: TObject; aType: TwsSSLHandler;
    var aSSLHandler: TIdSSLIOHandlerSocketBase) of object;
  TsgcWSOnSSLAfterCreateHandler = procedure(Sender: TObject;
    aType: TwsSSLHandler; aSSLHandler: TIdSSLIOHandlerSocketBase) of object;
  TsgcWSOnSSLVerifyPeer = procedure(Sender: TObject; Certificate: TIdX509;
    AOk: Boolean; ADepth, AError: Integer; var Accept: Boolean) of object;
  TsgcWSOnRedirect = procedure(Sender: TObject; const aLocation: string)
    of object;

  TsgcWSClient = class;

  TsgcWSProtocol_Client_KeepAlive = class(TsgcWSProtocol_Client)

  end;


  // TsgcWSHandShakeClient //

  TsgcWSHandShakeClient = class(TsgcWSHandshake)

    { fields properties }
  private
    FRedirect: Boolean;
    FAccept: string;
    FLocation: string;
  public
    property Accept: string read FAccept;
    property Redirect: Boolean read FRedirect;
    property Location: String read FLocation;

    { decode }
  protected
    procedure DoDecodeConnection(const aHeader: String);
    procedure DoDecodeHeader(const aConnection: TIdTCPConnection;
      aHeaders: TIdHeaderList);
    procedure DoDecodeHTTP(const aHeader: String);
    procedure DoDecodeAccept(const aHeader: String);
    procedure DoDecodeProtocol(const aHeader: String);
    procedure DoDecodeUpgrade(const aHeader: String);
    procedure DoDecodeExtensions(const aHeader: String);
    procedure DoDecodeLocation(const aHeader: String);

    { constructor }
  public
    constructor Create(aConnection: TIdTCPConnection;
      aHeaders: TIdHeaderList); virtual;
  End;


  // TsgcWSConnectionClient //

  TsgcWSOptionsClient = class(TPersistent)
  private
    FCleanDisconnect: Boolean;
    FParameters: String;
    FFragmentedMessages: TwsFragmentedMessages;
    FOrigin: String;
    FRaiseDisconnectExceptions: Boolean;
    FValidateUTF8: Boolean;
    function GetParameters: String;
    procedure SetParameters(const Value: String);
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
{$IFNDEF BCB}
  published
{$ELSE}
  public
{$ENDIF}
    property CleanDisconnect: Boolean read FCleanDisconnect
      write FCleanDisconnect;
  published
    property FragmentedMessages: TwsFragmentedMessages read FFragmentedMessages
      write FFragmentedMessages;
    property Origin: String read FOrigin write FOrigin;
    property Parameters: String read GetParameters write SetParameters;
    property RaiseDisconnectExceptions: Boolean read FRaiseDisconnectExceptions
      write FRaiseDisconnectExceptions;
    property ValidateUTF8: Boolean read FValidateUTF8 write FValidateUTF8;
  end;


  // TsgcWSAuthenticationServer_Session //

  TsgcWSAuthenticationClient_Session = class(TsgcWSAuthentication_Session)
  private
    FExtensions: String;
    FProtocols: String;
    FID: String;
  public
    property Extensions: String read FExtensions write FExtensions;
    property Protocols: String read FProtocols write FProtocols;
    property ID: String read FID write FID;
  end;


  // TsgcWSAuthenticationClient_URL //

  TsgcWSAuthenticationClient_URL = class(TsgcWSAuthentication_URL)

  end;


  // TsgcWSAuthenticationClient_Basic //

  TsgcWSAuthenticationClient_Basic = class(TsgcWSAuthentication_Basic)

  end;

  // TsgcWSAuthenticationClient_Token //

  TsgcWSAuthenticationClient_Token = class(TsgcWSAuthentication_Token)
  private
    FJWT: TsgcHTTPComponentClient_JWT;
    FOAuth: TsgcHTTPComponentClient_OAuth;
  public
    constructor Create; override;
  published
    property JWT: TsgcHTTPComponentClient_JWT read FJWT write FJWT;
    property OAuth: TsgcHTTPComponentClient_OAuth read FOAuth write FOAuth;
  end;

  // TsgcWSAuthenticationClient_Options //
  TsgcWSAuthenticationClient_Options = class(TsgcWSAuthentication_Options)
  private
    FURL: TsgcWSAuthenticationClient_URL;
    FPassword: String;
    FSession: TsgcWSAuthenticationClient_Session;
    FBasic: TsgcWSAuthenticationClient_Basic;
    FToken: TsgcWSAuthenticationClient_Token;
    FUser: String;
    procedure SetToken(const Value: TsgcWSAuthenticationClient_Token);
  protected
    procedure SetURL(const Value: TsgcWSAuthenticationClient_URL);
    procedure SetSession(const Value: TsgcWSAuthenticationClient_Session);
    procedure SetBasic(const Value: TsgcWSAuthenticationClient_Basic);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property URL: TsgcWSAuthenticationClient_URL read FURL write SetURL;
    property Password: String read FPassword write FPassword;
    property Session: TsgcWSAuthenticationClient_Session read FSession
      write SetSession;
    property Basic: TsgcWSAuthenticationClient_Basic read FBasic write SetBasic;
    property Token: TsgcWSAuthenticationClient_Token read FToken write SetToken;
    property User: String read FUser write FUser;
  end;

  // TsgcWSLoadBalancerClient_Options //
  TsgcWSLoadBalancerClient_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FHost: String;
    FPort: Integer;
    FServers: TStringList;
    procedure SetServers(const Value: TStringList);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property Host: String read FHost write FHost;
    property Port: Integer read FPort write FPort;
    property Servers: TStringList read FServers write SetServers;
  end;


  // TsgcWSConnectionClient //

  TsgcWSConnectionClient = class(TsgcWSConnection_Indy)
    { from TsgcWSConnection }
  protected
    function GetURL: String; override;
    { from TsgcWSConnection }

    { properties }
  private
    FHost: String;
    FPort: Integer;
    FOptions: TsgcWSOptionsClient;
    FAuthentication: TsgcWSAuthenticationClient_Options;
  protected
    procedure SetOptions(const Value: TsgcWSOptionsClient);
    procedure SetAuthentication(const Value
      : TsgcWSAuthenticationClient_Options);
  public
    property Host: String read FHost write FHost;
    property Port: Integer read FPort write FPort;
    { properties }

    { disconnect }
  private
    FBeforeDisconnect: Boolean;
    procedure DoBeforeDisconnectEvent;
  protected
    procedure DoClose(aCloseCode: Integer = CS_CLOSE_NORMAL); override;
    procedure DoDisconnect(const AError: string = '';
      aCloseCode: Integer = CS_CLOSE_NORMAL); override;
    { disconnect }

    { validate }
  private
    procedure DoValidateProtocols;
    { validate }

    { alpn }
  private
    function GetALPNProtocol: string;
  public
    property ALPNProtocol: string read GetALPNProtocol;
    { alpn }

    { handshake }
  private
    FKey: String;
    FHandshake: TsgcWSHandShakeClient;
    FOnEnabled: TNotifyEvent;
  protected
    procedure DoReadHandshake_RFC8441; virtual;
    procedure DoSendHandshake_RFC8441; virtual;
  protected
    procedure DoHandShakeExtensions; virtual;
    procedure DoHandshake; override;
  protected
    function GetWSKey: String;
    procedure DoSendHandshake_RFC6445;
    property Authentication: TsgcWSAuthenticationClient_Options
      read FAuthentication write SetAuthentication;
    property Options: TsgcWSOptionsClient read FOptions write SetOptions;
    { handshake }

    { initialize }
  public
    procedure DoInitialize(aConnection: TIdTCPConnection);
    { initialize }

    { events }
  public
    property OnEnabled: TNotifyEvent read FOnEnabled write FOnEnabled;
    { events }

    { constructor }
  public
    constructor Create; override;
    { constructor }

    { destructor }
  public
    destructor Destroy; override;
    { destructor }

    { event }
  private
    FOnBeforeDisconnect: TNotifyEvent;
    FOnRedirect: TsgcWSOnRedirect;
  public
    property OnBeforeDisconnect: TNotifyEvent read FOnBeforeDisconnect
      write FOnBeforeDisconnect;
    property OnRedirect: TsgcWSOnRedirect read FOnRedirect write FOnRedirect;
    { event }
  End;

  TsgcWSQueueClient_Options = class(TsgcWSQueueOptions)

  end;

  TsgcWSProxy_Options = class(TsgcTCPProxy_Options)

  end;

  // TClientThread //
  TClientThread = class(TThread)
  private
    FClient: TsgcWSClient;
    FMethod: string;
  protected
    procedure Execute; override;
  public
    constructor Create(const aMethod: string; const aClient: TsgcWSClient);
    class procedure Stop(const aClient: TsgcWSClient);
    class procedure Start(const aClient: TsgcWSClient);
  end;

  // TsgcWSClient //

  TsgcWSClient = class(TsgcWSComponent_Client_Indy)

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
    function GetSpecifications: TsgcWSSpecifications; override;
  protected
    procedure DoNotifyDisconnect(aConnection: TsgcWSConnection); override;
    { from TsgcWSComponent }

    { from TsgcWSComponent_client }
  protected
    procedure SetURL(const Value: String); override;
    { from TsgcWSComponent_client }

    { tcp client }
  private
    FTCPClient: TsgcWSCustomClient;
    function GetTCPClient: TsgcWSCustomClient;
  protected
    procedure DoStopReadThread; virtual;
  protected
    property TCPClient: TsgcWSCustomClient read GetTCPClient write FTCPClient;
    { tcp client }

    { helpers }
  private
    procedure DoStopThreads;
    { helpers }

    { connection }
  protected
    FWSConnection: TsgcWSConnectionClient;
    FDisconnecting: Boolean;
  protected
    function GetConnection: TsgcWSConnection;
  public
    property Connection: TsgcWSConnection read GetConnection;
    { connection }

    { LogFile }
  private
    FInterceptLogFile: TsgcWSIdLogFileClient;
    function GetInterceptLogFile: TsgcWSIdLogFileClient;
  protected
    property InterceptLogFile: TsgcWSIdLogFileClient read GetInterceptLogFile
      write FInterceptLogFile;
    { LogFile }
{$IFNDEF LAZARUS}
    { Throttle }
  private
    FInterceptThrottle: TIdInterceptThrottler;
    function GetInterceptThrottle: TIdInterceptThrottler;
  protected
    property InterceptThrottle: TIdInterceptThrottler read GetInterceptThrottle
      write FInterceptThrottle;
    { Throttle }
{$ENDIF}
    { intercepts }
  protected
    function GetConnectionIntercept: TIdConnectionIntercept;
    { intercepts }

    { proxy }
  private
    FProxy: TsgcWSProxy_Options;
    FProxyIOHandler: TIdIOHandlerStack;
    FProxyHTTP: TIdConnectThroughHttpProxy;
    FProxySocks: TIdSocksInfo;
  private
    function GetProxyIOHandler: TIdIOHandlerStack;
    function GetProxyHTTP: TIdConnectThroughHttpProxy;
    function GetProxySocks: TIdSocksInfo;
  protected
    procedure SetProxy(const Value: TsgcWSProxy_Options); virtual;
  public
    property Proxy: TsgcWSProxy_Options read FProxy write SetProxy;
    { proxy }

    { heartbeat }
  private
    FOnBeforeHeartBeat: TsgcWSOnBeforeHeartBeatEvent;
  protected
    procedure OnHeartBeatTimeoutEvent(Sender: TObject); override;
    procedure OnHeartBeatExceptionEvent(Sender: TObject; E: Exception);
      override;
    procedure OnHeartBeatTimeoutExceptionEvent(Sender: TObject;
      E: Exception); override;
  protected
    procedure OnHeartBeatEvent(Sender: TObject); override;
  public
    property OnBeforeHeartBeat: TsgcWSOnBeforeHeartBeatEvent
      read FOnBeforeHeartBeat write FOnBeforeHeartBeat;
    { heartbeat }

    { authentication }
  private
    FAuthentication: TsgcWSAuthenticationClient_Options;
  private
    procedure SetAuthentication(const Value
      : TsgcWSAuthenticationClient_Options);
  protected
    function DoGetSession: Boolean; virtual;
    function DoGetSessionURL: string; virtual;
    function DoGetSessionBody: string; virtual;
    function DoSessionResponse(aStream: TStringStream): Boolean; virtual;
  protected
    procedure OnAuthTokenEvent(Sender: TObject;
      const TokenType, Token, Data: String); virtual;
    procedure OnAuthTokenErrorEvent(Sender: TObject;
      const Error, ErrorDescription, Data: String); virtual;
  public
    property Authentication: TsgcWSAuthenticationClient_Options
      read FAuthentication write SetAuthentication;
    { authentication }

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

    { LoadBalancer }
  private
    FLoadBalancer: TsgcWSLoadBalancerClient_Options;
  private
    FOnLoadBalancerError: TsgcWSLoadBalancerErrorEvent;
  protected
    procedure SetLoadBalancer(const Value: TsgcWSLoadBalancerClient_Options);
  private
    function GetHostByServers: Boolean;
    function GetHostByLoadBalancerServer: Boolean;
  protected
    procedure DoLoadBalancerGetHost; virtual;
  public
    property LoadBalancer: TsgcWSLoadBalancerClient_Options read FLoadBalancer
      write SetLoadBalancer;
  public
    property OnLoadBalancerError: TsgcWSLoadBalancerErrorEvent
      read FOnLoadBalancerError write FOnLoadBalancerError;
    { LoadBalancer }

    { ssl }
  private
    FHandlerSSL: TIdSSLIOHandlerSocketBase;
  protected
    FOnSSLGetHandler: TsgcWSOnSSLGetHandler;
    FOnSSLAfterCreateHandler: TsgcWSOnSSLAfterCreateHandler;
    FOnSSLVerifyPeer: TsgcWSOnSSLVerifyPeer;
  protected
    property HandlerSSL: TIdSSLIOHandlerSocketBase read FHandlerSSL;
  protected
    procedure GetHandlerSSLEvent(const aSSLType: TwsSSLHandler;
      var aSSLHandler: TIdSSLIOHandlerSocketBase); virtual;
    procedure DoSSL; virtual;
  protected
    procedure OnGetPasswordEvent(var Password:
{$IFDEF INDY10_6}String{$ELSE} AnsiString {$ENDIF}); virtual;
    function OnVerifyPeerEvent(Certificate: TIdX509; AOk: Boolean
{$IFDEF INDY10_2}; ADepth {$IFDEF INDY10_5_8}, AError
{$ENDIF}: Integer {$ENDIF}): Boolean; virtual;
  public
    property OnSSLGetHandler: TsgcWSOnSSLGetHandler read FOnSSLGetHandler
      write FOnSSLGetHandler;
    property OnSSLAfterCreateHandler: TsgcWSOnSSLAfterCreateHandler
      read FOnSSLAfterCreateHandler write FOnSSLAfterCreateHandler;
    property OnSSLVerifyPeer: TsgcWSOnSSLVerifyPeer read FOnSSLVerifyPeer
      write FOnSSLVerifyPeer;
    { ssl }

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
    FReadingThread: Boolean;
  private
    FActive: Boolean;
    FConnectTimeout: Integer;
    FIPVersion: TIdIPVersion;
    FOptions: TsgcWSOptionsClient;
    FQueueOptions: TsgcWSQueueClient_Options;
    FReadTimeout: Integer;
    FWriteTimeout: Integer;
    FUseNagle: Boolean;
    FBoundPortMax: TIdPort;
    FBoundPortMin: TIdPort;
    FLingerState: Integer;
    procedure SetQueueOptions(const Value: TsgcWSQueueClient_Options);
  protected
    function GetProtocol: String; virtual;
    procedure SetOptions(const Value: TsgcWSOptionsClient); virtual;
  public
    property BoundPortMin: TIdPort read FBoundPortMin write FBoundPortMin;
    property BoundPortMax: TIdPort read FBoundPortMax write FBoundPortMax;
    property Protocol: String read GetProtocol;
    property ConnectTimeout: Integer read FConnectTimeout write FConnectTimeout;
    property Options: TsgcWSOptionsClient read FOptions write SetOptions;
    property ReadTimeout: Integer read FReadTimeout write FReadTimeout;
    property IPVersion: TIdIPVersion read FIPVersion write FIPVersion;
    property LingerState: Integer read FLingerState write FLingerState;
    property QueueOptions: TsgcWSQueueClient_Options read FQueueOptions
      write SetQueueOptions;
    property UseNagle: Boolean read FUseNagle write FUseNagle stored True;
    property WriteTimeout: Integer read FWriteTimeout write FWriteTimeout;
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
  public
    function Connected: Boolean;
    { connect/disconnect and wait }

    { events }
  protected
    procedure OnConnectionBeforeDisconnect(Sender: TObject); virtual;
    procedure OnConnectionRedirect(Sender: TObject;
      const aLocation: string); virtual;
  protected
    procedure OnClientConnectEvent(Sender: TObject); virtual;
    procedure OnClientMessageEvent(aConnection: TsgcWSConnection;
      const Text: string); virtual;
    procedure OnClientBinaryEvent(aConnection: TsgcWSConnection;
      const aStream: TMemoryStream); virtual;
    procedure OnClientFragmentedEvent(aConnection: TsgcWSConnection;
      const aData: TMemoryStream; const aOpCode: TOpCode;
      const aContinuation: Boolean); virtual;
    procedure OnClientDisconnectEvent(Sender: TObject); virtual;
    procedure OnClientStatusEvent(ASender: TObject; const AStatus: TIdStatus;
      const AStatusText: string); virtual;
    procedure OnClientEnabledEvent(Sender: TObject); virtual;
  protected
    procedure OnClientHandShakeEvent(aConnection: TsgcWSConnection;
      var Headers: TStringList); virtual;
    { events }

    { from thread }
  protected
    procedure OnReadEvent(Sender: TObject);
    procedure OnThreadEvent(Sender: TObject);
    procedure OnReadExceptionEvent(aException: Exception);
    { from thread }

    { constructor }
  private
    procedure DoClear;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor }

    { WriteData }
  private
    function KeepAliveProtocol: Boolean;
  protected
    procedure DoPing(const aText: String = ''); overload; virtual;
    function DoPing(aTimeout: Integer; const aText: String = ''): Boolean;
      overload; virtual;
  public
    procedure Ping(const aText: String = ''); overload;
    function Ping(aTimeout: Integer; const aText: String = '')
      : Boolean; overload;
  public
    procedure WriteData(const aText: String; const aSize: Integer = 0;
      const aStreaming: TwsStreaming = stmNone); override;
    procedure WriteData(const aStream: TStream; const aSize: Integer = 0;
      const aStreaming: TwsStreaming = stmNone); override;
  public
    function WriteAndWaitData(const aText: String;
      const aTimeout: Integer = 10000): string; override;
    { WriteData }
  end;

implementation

uses
  DateUtils,
  // sgc
{$IFDEF SGC_SCHANNEL} sgcSSL_SChannel_Indy, {$ENDIF}
{$IFDEF SGC_HTTP2} sgcHTTP2_Frame, sgcHTTP2_Types, {$ENDIF}
{$IFDEF SGC_INDY} sgcIdStackConsts, {$ELSE} IdStackConsts, {$ENDIF}
  sgcWebSocket_Helpers, sgcWebSocket_LoadBalancer_Message, sgcBase_Const,
  sgcSocket_Const, sgcBase_Helpers, sgcBase_Classes, sgcHTTP_Client;

{ TWSHandShakeServer }

constructor TsgcWSHandShakeClient.Create(aConnection: TIdTCPConnection;
  aHeaders: TIdHeaderList);
begin
  FRedirect := False;
  DoDecodeHeader(aConnection, aHeaders);
end;

procedure TsgcWSHandShakeClient.DoDecodeConnection(const aHeader: String);
begin
  if not sgcMatchesMask(aHeader, '*Upgrade*') then
    raise TsgcWSException.Create(S_ERROR_DECODING_UPGRADE);
end;

procedure TsgcWSHandShakeClient.DoDecodeHeader(const aConnection
  : TIdTCPConnection; aHeaders: TIdHeaderList);
var
  vText: String;
  i: Integer;
begin
  for i := 0 to aHeaders.Count - 1 do
  begin
    vText := aHeaders[i];

    if sgcMatchesMask(vText, 'HTTP/*') then
    begin
      Try
        DoDecodeHTTP(vText);
      Except
        On E: Exception do
          raise Exception.Create(E.Message + ' [' + aHeaders[i] + ']');
      End;
    end
    else if FRedirect then
    begin
      if sgcMatchesMask(vText, 'Location: *') then
      begin
        DoDecodeLocation(vText);
        exit;
      end;
    end
    else
    begin
      if sgcMatchesMask(vText, 'Upgrade: *') then
        DoDecodeUpgrade(vText)
      else if sgcMatchesMask(vText, 'Connection: *') then
        DoDecodeConnection(vText)
      else if sgcMatchesMask(vText, 'Sec-WebSocket-Accept: *') then
        DoDecodeAccept(vText)
      else if sgcMatchesMask(vText, 'Sec-WebSocket-Protocol: *') then
        DoDecodeProtocol(vText)
      else if sgcMatchesMask(vText, 'Sec-WebSocket-Extensions: *') then
        DoDecodeExtensions(vText);
    end;
  end;
end;

procedure TsgcWSHandShakeClient.DoDecodeHTTP(const aHeader: String);
var
  vText: String;
begin
  FRedirect := False;
  vText := Copy(aHeader, 10, Length(aHeader) - 9);
  if (LeftStr(vText, 2) = '30') or (LeftStr(vText, 2) = '30') then
  begin
    FRedirect := True;
    exit;
  end;
  if not sgcMatchesMask(vText, '*101*') then
    raise TsgcWSException.Create(S_ERROR_DECODING_SWITCHING_PROTOCOLS);
end;

procedure TsgcWSHandShakeClient.DoDecodeAccept(const aHeader: String);
begin
  if not sgcMatchesMask(aHeader, 'Sec-WebSocket-Accept: *') then
    raise TsgcWSException.Create('');
  FAccept := Copy(aHeader, 23, Length(aHeader) - 22);
end;

procedure TsgcWSHandShakeClient.DoDecodeProtocol(const aHeader: String);
begin
  if sgcMatchesMask(aHeader, 'Sec-WebSocket-Protocol: *') then
    FProtocols := Copy(aHeader, 25, Length(aHeader) - 24);
end;

procedure TsgcWSHandShakeClient.DoDecodeUpgrade(const aHeader: String);
begin
  if not sgcMatchesMask(aHeader, '* websocket') then
    raise TsgcWSException.Create(S_ERROR_DECODING_UPGRADE);
end;

procedure TsgcWSHandShakeClient.DoDecodeExtensions(const aHeader: String);
begin
  if sgcMatchesMask(aHeader, 'Sec-WebSocket-Extensions: *') then
    FExtensions := Copy(aHeader, 27, Length(aHeader) - 26);
end;

procedure TsgcWSHandShakeClient.DoDecodeLocation(const aHeader: String);
begin
  FLocation := Copy(aHeader, 11, Length(aHeader) - 10);
end;

constructor TsgcWSClient.Create(aOwner: TComponent);
begin
  inherited;
  FOptions := TsgcWSOptionsClient.Create;
  FProxy := TsgcWSProxy_Options.Create;
  FProxy.Enabled := False;
  FProxy.Port := 8080;
  FAuthentication := TsgcWSAuthenticationClient_Options.Create;
  FAuthentication.URL.Enabled := True;
  FAuthentication.Session.Enabled := False;
  FAuthentication.Basic.Enabled := False;
  FAuthentication.Token.Enabled := False;
  FLoadBalancer := TsgcWSLoadBalancerClient_Options.Create;
  FLoadBalancer.Enabled := False;
  Port := CS_DEFAULT_PORT;
  TLS := False;
  Extensions.Mode := appClient;
  WatchDog.Interval := 10;
  FQueueOptions := TsgcWSQueueClient_Options.Create;
  FQueueOptions.Text.Level := qmNone;
  FQueueOptions.Binary.Level := qmNone;
  FQueueOptions.Ping.Level := qmNone;
  FConnectTimeout := 0;
  FReadTimeout := -1;
  FDisconnecting := False;
  FBoundPortMin := 0;
  FBoundPortMax := 0;
  FLingerState := -1;
end;

destructor TsgcWSClient.Destroy;
begin
  Active := False;
  DoClear;
  sgcFree(FQueueOptions);
  sgcFree(FLoadBalancer);
  sgcFree(FWSConnection);
  sgcFree(FAuthentication);
  sgcFree(FOptions);
  sgcFree(FProxy);
  inherited;
end;

function TsgcWSClient.DoGetSession: Boolean;
var
  oHTTP: TsgcIdHTTP;
  oStream: TStringStream;
begin
  oHTTP := TsgcIdHTTP.Create(nil);
  oStream := TStringStream.Create(DoGetSessionBody);
  Try
    // ... proxy
    if Proxy.Enabled then
    begin
      oHTTP.ProxyParams.ProxyServer := Proxy.Host;
      oHTTP.ProxyParams.ProxyPort := Proxy.Port;
      oHTTP.ProxyParams.ProxyUsername := Proxy.Username;
      oHTTP.ProxyParams.ProxyPassword := Proxy.Password;
    end;

    oHTTP.TLSOptions.Assign(TLSOptions);

    oHTTP.ReadTimeout := 10000;
    oHTTP.Request.UserAgent := CS_USER_AGENT;
    oHTTP.Get(DoGetSessionURL, oStream);

    result := DoSessionResponse(oStream);

  Finally
    sgcFree(oStream);
    sgcFree(oHTTP);
  End;
end;

procedure TsgcWSClient.DoClear;
begin
  // ... destroy heartbeat
  sgcThreadFree(FHeartBeatTimeoutTimer);
  sgcThreadFree(FHeartBeatTimer);
  // ... destroy ssl
  sgcFree(FHandlerSSL);
  // ... destroy proxy
  sgcFree(FProxyHTTP);
  sgcFree(FProxyIOHandler);
  // ... destroy FTCPClient
  sgcFree(FTCPClient);
end;

procedure TsgcWSClient.DoEventDisconnect(aConnection: TsgcWSConnection;
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

function TsgcWSClient.DoGetSessionBody: string;
begin
  result := '';
end;

function TsgcWSClient.DoSessionResponse(aStream: TStringStream): Boolean;
var
  oList: TStringList;
begin
  result := False;

  oList := TStringList.Create;
  Try
    oList.Text := aStream.DataString;
    if oList.Count > 2 then
    begin
      Authentication.Session.ID := oList[0];
      Authentication.Session.Extensions := oList[1];
      Authentication.Session.Protocols := oList[2];
      result := Authentication.Session.ID <> '';
    end;
  Finally
    sgcFree(oList);
  End;
end;

function TsgcWSClient.DoGetSessionURL: string;
begin
  if TLS then
    result := 'https://'
  else
    result := 'http://';
  result := result + Host + ':' + IntToStr(Port) + CS_REQ_AUTH_SESSION +
    Authentication.User + '/' + Authentication.Password;
end;

procedure TsgcWSClient.DoLoadBalancerGetHost;
begin
  if not GetHostByLoadBalancerServer then
    GetHostByServers;
end;

function TsgcWSClient.GetHostByLoadBalancerServer: Boolean;
var
  oHTTP: TsgcIdHTTP;
  oStream: TStringStream;
  oBinding: TsgcWSLoadBalancerServerBinding;
  vProtocol: String;
begin
  result := False;
  Try
    oHTTP := TsgcIdHTTP.Create(nil);
    oStream := TStringStream.Create('');
    Try
      // ... proxy
      if Proxy.Enabled then
      begin
        oHTTP.ProxyParams.ProxyServer := Proxy.Host;
        oHTTP.ProxyParams.ProxyPort := Proxy.Port;
        oHTTP.ProxyParams.ProxyUsername := Proxy.Username;
        oHTTP.ProxyParams.ProxyPassword := Proxy.Password;
      end;

      oHTTP.TLSOptions.Assign(TLSOptions);
      oHTTP.ReadTimeout := 10000;
      vProtocol := 'http';
      if TLS then
        vProtocol := 'https';
      oHTTP.Get(vProtocol + '://' + LoadBalancer.Host + ':' +
        IntToStr(LoadBalancer.Port) + '/' + CS_LB_CLIENT_GET_BINDING, oStream);

      if oStream.DataString <> '' then
      begin
        oBinding := TsgcWSLoadBalancerServerBinding.Create(nil);
        Try
          oBinding.Read(oStream.DataString);
          Host := oBinding.Host;
          Port := oBinding.Port;
          TLS := oBinding.SSL;
          result := True;
        Finally
          sgcFree(oBinding);
        End;
      end;
    Finally
      sgcFree(oStream);
      sgcFree(oHTTP);
    End;
  Except
    On E: Exception do
    begin
      if Assigned(FOnLoadBalancerError) then
        FOnLoadBalancerError(self, E.Message);
    end;
  End;
end;

function TsgcWSClient.GetHostByServers: Boolean;
var
  i, n: Integer;
  vHost: String;
  vPort: Integer;
  vSSL: Boolean;
begin
  result := False;

  if LoadBalancer.Servers.Count > 0 then
  begin
    n := 1;
    repeat
      i := RandomRange(0, LoadBalancer.Servers.Count);
      result := DecodeWSSURL(LoadBalancer.Servers[i], vHost, vPort, vSSL);
      if result then
      begin
        Host := vHost;
        Port := vPort;
        TLS := vSSL;
      end;
      inc(n)
    until result or (n > 10);
  end;
end;

procedure TsgcWSClient.DoNotifyDisconnect(aConnection: TsgcWSConnection);
begin
  FDisconnecting := True;
  Try
    // clear before inherit
    // avoid clear FWSConnection, if connect inside OnDisconnect event
    FWSConnection := nil;
    inherited;
  Finally
    FDisconnecting := False;
  End;
end;

procedure TsgcWSClient.DoPing(const aText: String = '');
var
  vText: String;
begin
  vText := aText;
  if vText = '' then
    vText := FormatDateTime('yyyymmddhhnnsszzz', Now);

  FWSConnection.Ping(vText);
end;

function TsgcWSClient.DoPing(aTimeout: Integer;
  const aText: String = ''): Boolean;
var
  vText: String;
begin
  vText := aText;
  if vText = '' then
    vText := FormatDateTime('yyyymmddhhnnsszzz', Now);

  result := FWSConnection.Ping(vText, aTimeout);
end;

function TsgcWSClient.GetActive: Boolean;
begin
  inherited;
  if not IsDesigning and not IsLoading then
    result := Assigned(FWSConnection)
  else
    result := FActive;
end;

function TsgcWSClient.GetConnection: TsgcWSConnection;
begin
  result := FWSConnection;
end;

function TsgcWSClient.GetInterceptLogFile: TsgcWSIdLogFileClient;
begin
  if not Assigned(FInterceptLogFile) then
  begin
    FInterceptLogFile := TsgcWSIdLogFileClient.Create(self);
    FInterceptLogFile.ReplaceCRLF := False;
  end;
  if not FInterceptLogFile.Active then
    FInterceptLogFile.Filename := LogFile.Filename;
  FInterceptLogFile.Active := LogFile.Enabled;
  FInterceptLogFile.UnMaskFrames := LogFile.UnMaskFrames;
  result := FInterceptLogFile;
end;

function TsgcWSClient.GetConnectionIntercept: TIdConnectionIntercept;
begin
  result := nil;
{$IFNDEF LAZARUS}
  if Throttle.Enabled and LogFile.Enabled and (LogFile.Filename <> '') then
  begin
    InterceptLogFile.Intercept := nil;
    InterceptThrottle.Intercept := InterceptLogFile;
    result := InterceptThrottle;
  end
  else if Throttle.Enabled then
  begin
    InterceptThrottle.Intercept := nil;
    result := InterceptThrottle;
  end
  else
{$ENDIF}
    if LogFile.Enabled and (LogFile.Filename <> '') then
    begin
      InterceptLogFile.Intercept := nil;
      result := InterceptLogFile;
    end;
end;
{$IFNDEF LAZARUS}

function TsgcWSClient.GetInterceptThrottle: TIdInterceptThrottler;
begin
  if not Assigned(FInterceptThrottle) then
    FInterceptThrottle := TIdInterceptThrottler.Create(self);
  result := FInterceptThrottle;
end;
{$ENDIF}

function TsgcWSClient.GetProtocol: String;
begin
  result := GetProtocols;
end;

function TsgcWSClient.GetProxyHTTP: TIdConnectThroughHttpProxy;
begin
  if not Assigned(FProxyHTTP) then
    FProxyHTTP := TIdConnectThroughHttpProxy.Create(self);
  FProxyHTTP.Host := Proxy.Host;
  FProxyHTTP.Port := Proxy.Port;
  FProxyHTTP.Username := Proxy.Username;
  FProxyHTTP.Password := Proxy.Password;
  FProxyHTTP.Enabled := Proxy.Enabled;
  result := FProxyHTTP;
end;

function TsgcWSClient.GetProxyIOHandler: TIdIOHandlerStack;
begin
  if not TLS then
  begin
    if not Assigned(FProxyIOHandler) then
      FProxyIOHandler := TIdIOHandlerStack.Create(self);
    result := FProxyIOHandler;
  end
  else
    result := FHandlerSSL;
end;

function TsgcWSClient.GetSpecifications: TsgcWSSpecifications;
begin
  result := inherited GetSpecifications;
  result.Drafts.Hixie76 := False;
end;

function TsgcWSClient.GetTCPClient: TsgcWSCustomClient;
begin
  if not Assigned(FTCPClient) then
  begin
    FTCPClient := TsgcWSCustomClient.Create(self);

    FTCPClient.OnConnected := OnClientConnectEvent;
    // handle hsDisconnected status
    // FTCPClient.OnDisconnected := OnClientDisconnectEvent;
    FTCPClient.OnStatus := OnClientStatusEvent;

    FTCPClient.OnThread := OnThreadEvent;
    FTCPClient.OnRead := OnReadEvent;
    FTCPClient.OnReadException := OnReadExceptionEvent;
  end;
  result := FTCPClient;
end;

procedure TsgcWSClient.Loaded;
begin
  inherited;
  if Active <> FActive then
    Active := FActive;
end;

procedure TsgcWSClient.OnClientBinaryEvent(aConnection: TsgcWSConnection;
  const aStream: TMemoryStream);
begin
  if IsDestroying then
    exit;

  Try
    aConnection.MsgBinaryReceived.LoadFromStream(aStream);
    DoNotifyBinary(aConnection);
  Except
    On E: Exception do
      DoError(aConnection, E.Message);
  end;
end;

procedure TsgcWSClient.OnClientConnectEvent(Sender: TObject);
begin
  FConnecting := False;

  if IsDestroying then
    exit;
{$IFDEF MSWINDOWS}
  if WriteTimeout > 0 then
    TCPClient.Socket.Binding.SetSockOpt(Id_SOL_SOCKET, Id_SO_SNDTIMEO,
      WriteTimeout);
{$ENDIF}
{$IFDEF INDY10_5_8}
  if TCPKeepAlive.Enabled then
    TCPClient.Socket.Binding.SetKeepAliveValues(True, TCPKeepAlive.Time,
      TCPKeepAlive.Interval);
{$ENDIF}
  Try
    FWSConnection := TsgcWSConnectionClient.Create;
    FWSConnection.Specification := spRFC6455;
    if Specifications.RFC8441 then
      FWSConnection.Transport := trpRFC8441
    else if Specifications.RFC6455 then
      FWSConnection.Transport := trpRFC6455
    else if Specifications.Drafts.Hixie76 then
      FWSConnection.Transport := trpHixie76
    else
      FWSConnection.Transport := trpTCP;
    FWSConnection.Extensions := Extensions;
    FWSConnection.Masked := True;
    FWSConnection.Protocol := Protocol;
    FWSConnection.Host := TCPClient.Host;
    FWSConnection.Port := TCPClient.Port;
    FWSConnection.Options := Options;
    FWSConnection.ValidateUTF8 := Options.ValidateUTF8;
    FWSConnection.CleanDisconnect := Options.CleanDisconnect;
    FWSConnection.RaiseDisconnectExceptions :=
      Options.RaiseDisconnectExceptions;
    FWSConnection.FragmentedMessages := Options.FragmentedMessages;
    FWSConnection.QueueOptions := QueueOptions;
    FWSConnection.Authentication := Authentication;
    FWSConnection.OnHandshake := OnClientHandShakeEvent;
    FWSConnection.OnMessage := OnClientMessageEvent;
    FWSConnection.OnBinary := OnClientBinaryEvent;
    FWSConnection.OnFragmented := OnClientFragmentedEvent;
    FWSConnection.OnEnabled := OnClientEnabledEvent;
    FWSConnection.OnBeforeDisconnect := OnConnectionBeforeDisconnect;
    FWSConnection.OnRedirect := OnConnectionRedirect;
    FWSConnection.DoInitialize(TCPClient);

    DoStartHeartBeat;
  Except
    On E: Exception do
      DoError(FWSConnection, E.Message);
  end;
end;

procedure TsgcWSClient.OnClientDisconnectEvent(Sender: TObject);
begin
  if IsDestroying then
    exit;

  Try
    if not(csDestroying in ComponentState) then
    begin
      // disconnect if connected, if not appears as Active connection
      if Assigned(TCPClient) then
      begin
        if TCPClient.Connected then
          TCPClient.Disconnect(False);
      end;
      DoNotifyDisconnect(FWSConnection);
    end;
  Except
    On E: Exception do
      DoError(FWSConnection, E.Message);
  end;
end;

procedure TsgcWSClient.OnClientEnabledEvent(Sender: TObject);
begin
  if IsDestroying then
    exit;

  DoNotifyConnect(FWSConnection);
end;

procedure TsgcWSClient.OnClientFragmentedEvent(aConnection: TsgcWSConnection;
  const aData: TMemoryStream; const aOpCode: TOpCode;
  const aContinuation: Boolean);
begin
  if IsDestroying then
    exit;

  Try
    DoNotifyFragmented(aConnection, aData, aOpCode, aContinuation);
  Except
    On E: Exception do
      DoError(aConnection, E.Message);
  end;
end;

procedure TsgcWSClient.OnClientHandShakeEvent(aConnection: TsgcWSConnection;
  var Headers: TStringList);
begin
  Try
    if not IsDestroying then
      DoNotifyHandshake(aConnection);
  Except
    On E: Exception do
      DoError(aConnection, E.Message);
  end;
end;

procedure TsgcWSClient.OnClientMessageEvent(aConnection: TsgcWSConnection;
  const Text: string);
begin
  if IsDestroying then
    exit;

  Try
    aConnection.MsgReceived := Text;
    DoNotifyMessage(aConnection);
  Except
    On E: Exception do
      DoError(aConnection, E.Message);
  end;
end;

procedure TsgcWSClient.OnClientStatusEvent(ASender: TObject;
  const AStatus: TIdStatus; const AStatusText: string);
begin
  if IsDestroying then
    exit;

  if AStatus = hsDisconnected then
    OnClientDisconnectEvent(ASender);
end;

procedure TsgcWSClient.OnHeartBeatEvent(Sender: TObject);
var
  vHandled: Boolean;
begin
  inherited;
  if IsDestroying then
    exit;

  Try
    if Assigned(FWSConnection) and (FWSConnection.Disconnected = False) then
    begin
      vHandled := False;
      if Assigned(FOnBeforeHeartBeat) then
        FOnBeforeHeartBeat(self, FWSConnection, vHandled);

      if vHandled = False then
      begin
        if not KeepAliveProtocol then
          // if ping is managed by protocol, don't send a standard ping
          Ping;
        DoStartHeartBeatTimeout;
      end;
    end;
  Except
    On E: Exception do
      DoException(FWSConnection, E.Message, E);
  end;
end;

procedure TsgcWSClient.OnHeartBeatTimeoutEvent(Sender: TObject);
begin
  inherited;
  if IsDestroying then
    exit;

  if Assigned(FWSConnection) then
  begin
    if SecondsBetWeen(FWSConnection.LastPing, FWSConnection.LastPong) >=
      HeartBeat.Timeout then
    begin
      if ((HeartBeat.Interval >= HeartBeat.Timeout) and
        (SecondsBetWeen(Now, FWSConnection.LastPing) >= HeartBeat.Timeout) or
        (HeartBeat.Timeout >= HeartBeat.Interval) and
        (SecondsBetWeen(Now, FWSConnection.FirstPing) >= HeartBeat.Timeout))
      then
      begin
{$IFDEF SGC_DEBUG}
        DoLog(self, FWSConnection, S_HEARTBEAT_TIMEOUT_EXCEEDED);
{$ENDIF}
        DoStopHeartBeatTimeOut;
        DoException(FWSConnection, S_HEARTBEAT_TIMEOUT_EXCEEDED, nil);
      end
    end
    else
      FWSConnection.FirstPing := 0;
  end;
end;

procedure TsgcWSClient.OnReadExceptionEvent(aException: Exception);
begin
  if IsDestroying then
    exit;

  if aException.InheritsFrom(EIdSilentException) then
  begin
    if Options.RaiseDisconnectExceptions then
      DoException(FWSConnection, aException.Message, aException);
  end
  else
    DoException(FWSConnection, aException.Message, aException);

  if Assigned(FWSConnection) then
  begin
    if Assigned(TCPClient) then
    begin
      if Assigned(TCPClient.IOHandler) then
      begin
        TCPClient.IOHandler.InputBuffer.Clear;
        if TLS then
        begin
          case TLSOptions.IOHandler of
            iohSChannel:
              ;
            iohOpenSSL:
              begin
                if TCPClient.IOHandler.Opened then
                  TCPClient.IOHandler.Close;
              end;
          end;
        end;
      end;
    end;
    if aException.ClassName <> 'EIdClosedSocket' then
      DoNotifyDisconnect(FWSConnection);
  end;
end;

procedure TsgcWSClient.OnReadEvent(Sender: TObject);
begin
  if IsDestroying then
    exit;

  Try
    if Assigned(FWSConnection) then
    begin
      if not FWSConnection.Disconnected then
      begin
        FReadingThread := True;
        Try
          FWSConnection.ReadData;
        Finally
          FReadingThread := False;
        End;
      end;
    end;
  Except
    On E: Exception do
      DoException(FWSConnection, E.Message, E);
  end;
end;

procedure TsgcWSClient.OnWatchDogEvent(Sender: TObject);
var
  vHandled: Boolean;
begin
  if IsDestroying then
    exit;

  // if current attempts > max attempts don't process request
  if (WatchDog.Attempts > 0) and (FWatchDogAttempts >= WatchDog.Attempts) then
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

procedure TsgcWSClient.OnThreadEvent(Sender: TObject);
begin
  Try
    if Assigned(FWSConnection) then
    begin
      if (Assigned(QueueOptions) and ((QueueOptions.Text.Level <> qmNone) or
        (QueueOptions.Binary.Level <> qmNone) or (QueueOptions.Ping.Level <>
        qmNone))) then
        DoWriteQueueMsgLevels(FWSConnection);
    end;
  Except
    On E: Exception do
      DoError(FWSConnection, E.Message);
  end;
end;

procedure TsgcWSClient.Ping(const aText: String = '');
begin
  Try
    if Assigned(FWSConnection) then
      DoPing(aText);
  Except
    On E: Exception do
      DoError(FWSConnection, E.Message);
  end;
end;

function TsgcWSClient.Ping(aTimeout: Integer; const aText: String = '')
  : Boolean;
begin
  result := False;
  Try
    if Assigned(FWSConnection) then
      result := DoPing(aTimeout, aText);
  Except
    On E: Exception do
      DoError(FWSConnection, E.Message);
  end;
end;

procedure TsgcWSClient.WriteData(const aText: String; const aSize: Integer = 0;
  const aStreaming: TwsStreaming = stmNone);
begin
  Try
    if Assigned(FWSConnection) then
      FWSConnection.WriteData(aText, aSize, aStreaming);
  Except
    On E: Exception do
      DoError(FWSConnection, E.Message);
  end;
end;

procedure TsgcWSClient.SetActive(const Value: Boolean);
begin
  if IsDestroying then
    exit;

  Try
    inherited;
    // set inside try, api components use beforeconnect and fail there
    if not IsDesigning and not IsLoading then
    begin
      if Value then
      begin
        if not TCPClient.Connected then
        begin
          // ... clear
          DoClear;
          // ... before connect
          if Assigned(FOnBeforeConnect) then
            FOnBeforeConnect(self);
          // ... TLS
          DoSSL;
          // ... LoadBalancer
          if LoadBalancer.Enabled then
            DoLoadBalancerGetHost;
          // ... properties
          TCPClient.Host := Host;
          TCPClient.Port := Port;
{$IFDEF INDY10_5_7}
          TCPClient.UseNagle := UseNagle;
{$ENDIF}
          TCPClient.IPVersion := IPVersion;
          TCPClient.ConnectTimeout := FConnectTimeout;
          // TLS 1.3+ sends internal TLS messages after handshake
          // this locks ssl_read on connect, set a small read timeout to avoid it
          if (Specifications.RFC6455 = False) and (ReadTimeout = -1) and (TLSOptions.Version >= tls1_3) then
            TCPClient.ReadTimeout := 100
          else
            TCPClient.ReadTimeout := FReadTimeout;
          TCPClient.BoundPortMin := FBoundPortMin;
          TCPClient.BoundPortMax := FBoundPortMax;
{$IFDEF SGC_INDY_LIB}
          TCPClient.LingerState := FLingerState;
{$ENDIF}
          // ... proxy
          if Proxy.Enabled then
          begin
            TCPClient.IOHandler := GetProxyIOHandler;
            case Proxy.ProxyType of
              pxyHTTP:
                begin
                  GetProxyIOHandler.TransparentProxy := GetProxyHTTP;
                  sgcFree(FProxySocks);
                end;
              pxySocks4, pxySocks4A, pxySocks5:
                begin
                  GetProxyIOHandler.TransparentProxy := GetProxySocks;
                  sgcFree(FProxyHTTP);
                end;
            end;
            // ... set the host of the server to connect, not the proxy
            if TLS and TLSOptions.VerifyCertificate then
            begin
              if Assigned(FHandlerSSL) then
                FHandlerSSL.URIToCheck := 'http://' + Host;
            end;
          end;
          // ... intercepts
{$IFNDEF INDY10_2}
          if Assigned(TCPClient.Socket) then
            TCPClient.Socket.Intercept := GetConnectionIntercept;
{$ELSE}
          TCPClient.Intercept := GetConnectionIntercept;
{$ENDIF}
          // ... WatchDog
          FWatchDogEnabled := WatchDog.Enabled;
          // ... authentication
          if Authentication.Enabled then
          begin
            if (Authentication.Token.Enabled = False) then
            begin
              if Authentication.Session.Enabled then
              begin
                if DoGetSession then
                  Options.Parameters := CS_AUTH_SESSION +
                    Authentication.Session.ID;
              end
              else if Authentication.URL.Enabled then
                Options.Parameters := CS_AUTH_URL + Authentication.User + '/' +
                  Authentication.Password;
            end
            else if Assigned(Authentication.Token.OAuth) then
            begin
              Authentication.Token.OAuth.OnAuthToken := OnAuthTokenEvent;
              Authentication.Token.OAuth.OnAuthTokenError :=
                OnAuthTokenErrorEvent;
              Authentication.Token.OAuth.Start;

              exit; // wait successful authentication
            end
            else if Assigned(Authentication.Token.JWT) then
            begin
              Authentication.Token.JWT.OnAuthToken := OnAuthTokenEvent;
              Authentication.Token.JWT.OnAuthTokenError :=
                OnAuthTokenErrorEvent;
              Authentication.Token.JWT.Start;

              exit; // wait successful authentication
            end;
          end;
          // ... Start
          FConnecting := True;
          TCPClient.Connect;
        end;
      end
      else
      begin
        FWatchDogEnabled := False;
        // ... Stop
        if Assigned(FWSConnection) and (FDisconnecting = False) then
        begin
          if Options.CleanDisconnect then
            FWSConnection.Close;
          DoStopThreads;
          TCPClient.IOHandler.WriteBufferClear;
          TCPClient.IOHandler.InputBuffer.Clear;
{$IFNDEF INDY10_6_2_D10_4} // from this indy version, setting passtrhough := True, locks the thread
{$IFNDEF NEXTGEN}
          if TCPClient.IOHandler is TIdSSLIOHandlerSocketBase then
            TIdSSLIOHandlerSocketBase(TCPClient.IOHandler).PassThrough := True;
          // don't read ssl messages if disconnected
{$ENDIF}
{$ENDIF}
          TCPClient.Disconnect(False);

          // OAuth
          if Assigned(Authentication.Token.OAuth) then
            Authentication.Token.OAuth.Stop;
        end
        else
          DoStopWatchDog;
      end;
    end
    else
      FActive := Value;
  Except
    On E: Exception do
    begin
      if not IsDestroying then
      begin
        FConnecting := False;
        // ... exception
        DoException(FWSConnection, E.Message, E);
        // ... watchdog
        if FWatchDogEnabled then
          DoStartWatchDog;
      end;
    end;
  End;
end;

procedure TsgcWSClient.SetAuthentication(const Value
  : TsgcWSAuthenticationClient_Options);
begin
  if Assigned(FAuthentication) then
    FAuthentication.Assign(Value);
end;

procedure TsgcWSClient.SetLoadBalancer(const Value
  : TsgcWSLoadBalancerClient_Options);
begin
  if Assigned(FLoadBalancer) then
    FLoadBalancer.Assign(Value);
end;

procedure TsgcWSClient.SetOptions(const Value: TsgcWSOptionsClient);
begin
  if Assigned(FOptions) then
    FOptions.Assign(Value);
end;

procedure TsgcWSClient.SetProxy(const Value: TsgcWSProxy_Options);
begin
  if Assigned(FProxy) then
    FProxy.Assign(Value);
end;

procedure TsgcWSClient.DoSSL;
begin
  if TLS then
  begin
    if not Assigned(FHandlerSSL) then
    begin
      // ... RFC8441
{$IFDEF SGC_INDY_LIB}
      if Specifications.RFC8441 then
      begin
        if TLSOptions.ALPNProtocols.IndexOf(CS_ALPN_H2) = -1 then
          TLSOptions.ALPNProtocols.Add(CS_ALPN_H2);
      end;
{$ENDIF}
      GetHandlerSSLEvent(sslClient, FHandlerSSL);
      TCPClient.IOHandler := FHandlerSSL;
    end;
  end
  else
  begin
    if Assigned(FHandlerSSL) then
    begin
      TCPClient.IOHandler := nil;
      sgcFree(FHandlerSSL);
    end;
  end;
end;

procedure TsgcWSClient.DoStart;
begin
  Try
    Active := True;
  Except
    On E: Exception do
      DoException(Connection, E.Message, E);
  End;
end;

procedure TsgcWSClient.DoStopReadThread;
var
  i: Integer;
  oThread: TsgcTCPReadThread;
begin
  if IsDestroying then
    exit;
{$IFDEF SGC_DEBUG}
  DoLog(self, FWSConnection, 'DoStopReadThread_Start');
{$ENDIF}
  If Assigned(FWSConnection) then
    FWSConnection.FMustDisconnect := True;

  if Assigned(TCPClient) then
  begin
    oThread := TCPClient.TCPRead;
    if Assigned(oThread) then
    begin
      if not oThread.Terminated then
      begin
        oThread.Terminate;
        if not FReadingThread then
        begin
          if not(TLS and (TLSOptions.IOHandler = iohSChannel)) then
            oThread.WaitFor;
        end
        else
        begin
          i := 0;
          repeat
            sleep(1);
            inc(i);
          until (i > 10) or (FReadingThread = False);
          if not FReadingThread then
            oThread.WaitFor;
        end;
      end;
    end;
  end;
{$IFDEF SGC_DEBUG}
  DoLog(self, FWSConnection, 'DoStopReadThread_End');
{$ENDIF}
end;

procedure TsgcWSClient.DoStopThreads;
begin
  DoStopHeartBeatTimeOut;
  DoStopHeartBeat;
  DoStopReadThread;
end;

procedure TsgcWSClient.GetHandlerSSLEvent(const aSSLType: TwsSSLHandler;
  var aSSLHandler: TIdSSLIOHandlerSocketBase);
begin
  // ... customer can assign own handler
  if Assigned(FOnSSLGetHandler) then
    FOnSSLGetHandler(self, aSSLType, aSSLHandler);
  // ... if not customized, create by default
  if not Assigned(aSSLHandler) then
  begin
{$IFDEF SGC_SCHANNEL}
    if TLSOptions.IOHandler = iohSChannel then
    begin
      aSSLHandler := TsgcIdSSLIOHandlerSocketSChannel.Create(self);
      aSSLHandler.PassThrough := False;
      TsgcIdSSLIOHandlerSocketSChannel(aSSLHandler).TLSVersion :=
        TLSOptions.Version;
      TsgcIdSSLIOHandlerSocketSChannel(aSSLHandler)
        .Certificate.VerifyCertificate := TLSOptions.VerifyCertificate;
      TsgcIdSSLIOHandlerSocketSChannel(aSSLHandler).Certificate.CertHash :=
        TLSOptions.SChannel_Options.CertHash;
      TsgcIdSSLIOHandlerSocketSChannel(aSSLHandler).Certificate.CertStorePath :=
        TLSOptions.SChannel_Options.CertStorePath;
      TsgcIdSSLIOHandlerSocketSChannel(aSSLHandler).Certificate.CertStoreName :=
        TLSOptions.SChannel_Options.CertStoreName;
      TsgcIdSSLIOHandlerSocketSChannel(aSSLHandler).CipherList :=
        TLSOptions.SChannel_Options.CipherList;
      TsgcIdSSLIOHandlerSocketSChannel(aSSLHandler).ALPNProtocols.Assign
        (TLSOptions.ALPNProtocols);
      TsgcIdSSLIOHandlerSocketSChannel(aSSLHandler).Certificate.CertFile :=
        TLSOptions.CertFile;
      TsgcIdSSLIOHandlerSocketSChannel(aSSLHandler).Certificate.CertFilePassword
        := String(TLSOptions.Password);
    end
    else
{$ENDIF}
    begin
      aSSLHandler := TsgcIdSSLIOHandlerSocketOpenSSL.Create(self);
      aSSLHandler.PassThrough := False;
{$IFDEF SGC_INDY_LIB}
      case TLSOptions.OpenSSL_Options.APIVersion of
        oslAPI_1_0:
          OPENSSL_API_VERSION := opSSL_1_0;
        oslAPI_1_1:
          OPENSSL_API_VERSION := opSSL_1_1;
        oslAPI_3_0:
          OPENSSL_API_VERSION := opSSL_3_0;
      end;
{$ENDIF}
      case TLSOptions.OpenSSL_Options.LibPath of
        oslpNone:
          ;
        oslpDefaultFolder:
          sgcIdOpenSSLSetLibPath(sgcGetOpenSSLDefaultFolder);
        oslpCustomPath:
          sgcIdOpenSSLSetLibPath(TLSOptions.OpenSSL_Options.LibPathCustom);
      end;
      {$IFDEF SGC_UNIX_SYMLINKS}
      case TLSOptions.OpenSSL_Options.UnixSymLinks of
        oslsSymLinksDefault:
          begin
            {$IFDEF OSX64}
            IdOpenSSLSetLoadSymLinksFirst(False);
            IdOpenSSLSetCanLoadSymLinks(False);
            {$ENDIF}
          end;
        oslsSymLinksLoadFirst:
          begin
            IdOpenSSLSetLoadSymLinksFirst(True);
            IdOpenSSLSetCanLoadSymLinks(True);
          end;
        oslsSymLinksLoad:
          begin
            IdOpenSSLSetLoadSymLinksFirst(False);
            IdOpenSSLSetCanLoadSymLinks(True);
          end;
        oslsSymLinksDontLoad:
          begin
            IdOpenSSLSetLoadSymLinksFirst(False);
            IdOpenSSLSetCanLoadSymLinks(False);
          end;
      end;
      {$ENDIF}
      case TLSOptions.Version of
        tls1_0:
          TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler).SSLOptions.Method :=
            sslvTLSv1;
{$IFDEF INDY10_6}
        tls1_1:
          TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler).SSLOptions.Method :=
            sslvTLSv1_1;
        tls1_2:
          TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler).SSLOptions.Method :=
            sslvTLSv1_2;
        tls1_3:
          TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler).SSLOptions.Method :=
{$IFDEF SGC_INDY_LIB} sslvTLSv1_3 {$ELSE} sslvTLSv1_2 {$ENDIF};
{$ENDIF}
      else
{$IFDEF INDY10_5_8}
        TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler).SSLOptions.SSLVersions :=
          [sslvTLSv1 {$IFDEF INDY10_6}, sslvTLSv1_1, sslvTLSv1_2
{$IFDEF SGC_INDY_LIB}, sslvTLSv1_3 {$ENDIF}{$ENDIF}];
{$ELSE}
        TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler).SSLOptions.Method :=
          sslvTLSv1;
{$ENDIF}
      end;
{$IFDEF SGC_INDY_LIB}
      TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler).ALPNProtocols.Assign
        (TLSOptions.ALPNProtocols);
{$ENDIF}
      TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler).SSLOptions.Mode :=
        sslmClient;
      TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler).SSLOptions.RootCertFile :=
        TLSOptions.RootCertFile;
      TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler).SSLOptions.CertFile :=
        TLSOptions.CertFile;
      TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler).SSLOptions.KeyFile :=
        TLSOptions.KeyFile;
      if TLSOptions.VerifyCertificate then
      begin
        TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler).SSLOptions.VerifyMode :=
          [sslvrfPeer];
        TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler).OnVerifyPeer :=
          OnVerifyPeerEvent;
        if TLSOptions.VerifyDepth >= 0 then
          TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler).SSLOptions.VerifyDepth :=
            TLSOptions.VerifyDepth;
      end
      else
        TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler)
          .SSLOptions.VerifyMode := [];
      TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler).OnGetPassword :=
        OnGetPasswordEvent;
    end;
    if Assigned(FOnSSLAfterCreateHandler) then
      FOnSSLAfterCreateHandler(self, aSSLType, aSSLHandler);
  end;
  // ... assign to handler to field
  FHandlerSSL := aSSLHandler;
end;

function TsgcWSClient.GetProxySocks: TIdSocksInfo;
begin
  if not Assigned(FProxySocks) then
    FProxySocks := TIdSocksInfo.Create(self);
  FProxySocks.Host := Proxy.Host;
  FProxySocks.Port := Proxy.Port;
  if Proxy.Username <> '' then
    FProxySocks.Authentication := saUsernamePassword
  else
    FProxySocks.Authentication := saNoAuthentication;
  FProxySocks.Username := Proxy.Username;
  FProxySocks.Password := Proxy.Password;
  case Proxy.ProxyType of
    pxySocks4:
      FProxySocks.Version := svSocks4;
    pxySocks4A:
      FProxySocks.Version := svSocks4A;
    pxySocks5:
      FProxySocks.Version := svSocks5;
  end;
  result := FProxySocks;
end;

function TsgcWSClient.KeepAliveProtocol: Boolean;
var
  i: Integer;
begin
  result := False;
  if Assigned(FWSConnection) then
  begin
    if FWSConnection.Protocol <> '' then
    begin
      for i := 0 to FProtocolObjectList.Count - 1 do
      begin
        if ProtocolRegistered(FWSConnection.Protocol,
          TsgcWSProtocol_Client_KeepAlive(FProtocolObjectList[i])) then
          result := TsgcWSProtocol_Client_KeepAlive(FProtocolObjectList[i])
            .DoKeepAlive;
      end;
    end
    else if Assigned(FAPI) then
      result := TsgcWSAPI_client_KeepAlive(FAPI).DoKeepAlive;
  end;
end;

procedure TsgcWSClient.OnConnectionBeforeDisconnect(Sender: TObject);
begin
  if IsDestroying then
    exit;

  DoStopThreads;
end;

procedure TsgcWSClient.OnHeartBeatExceptionEvent(Sender: TObject; E: Exception);
begin
  inherited;
  DoException(FWSConnection, E.Message, E);
end;

procedure TsgcWSClient.OnHeartBeatTimeoutExceptionEvent(Sender: TObject;
  E: Exception);
begin
  inherited;
  DoException(FWSConnection, E.Message, E);
end;

procedure TsgcWSClient.OnWatchDogExceptionEvent(Sender: TObject; E: Exception);
begin
  inherited;
  DoException(FWSConnection, E.Message, E);
end;

procedure TsgcWSClient.SetQueueOptions(const Value: TsgcWSQueueClient_Options);
begin
  if Assigned(FQueueOptions) then
    FQueueOptions.Assign(Value);
end;

procedure TsgcWSClient.SetURL(const Value: String);
var
  vProtocol, vHost, vPort, vPath, vQuery: String;
begin
  inherited;
  ParseURL(Value, vProtocol, vHost, vPort, vPath, vQuery);

  Host := vHost;
  if (vProtocol = 'wss') or (vProtocol = 'tcps') then
  begin
    TLS := True;
    if vPort <> '' then
      Port := StrToInt(vPort)
    else
      Port := 443;
    if vProtocol = 'tcps' then
      Specifications.RFC6455 := False
    else
      Specifications.RFC6455 := True;
  end
  else
  begin
    TLS := False;
    if vPort <> '' then
      Port := StrToInt(vPort)
    else
      Port := 80;
    if vProtocol = 'tcp' then
      Specifications.RFC6455 := False
    else
      Specifications.RFC6455 := True;
  end;
  Options.Parameters := vPath;
  if vQuery <> '' then
    Options.Parameters := Options.Parameters + '?' + vQuery;
end;

procedure TsgcWSClient.Start;
begin
  if IsDestroying then
    exit;

  FThreadStart := True;
  TClientThread.Start(self);
end;

procedure TsgcWSClient.Stop;
begin
  if IsDestroying then
    exit;

  TClientThread.Stop(self);
end;

function TsgcWSClient.Connect(const aTimeout: Integer = 10000): Boolean;
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

function TsgcWSClient.Connected: Boolean;
begin
  result := False;
  if Assigned(TCPClient) then
    result := TCPClient.Connected;
end;

function TsgcWSClient.Disconnect(const aTimeout: Integer = 10000): Boolean;
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

procedure TsgcWSClient.OnAuthTokenErrorEvent(Sender: TObject;
  const Error, ErrorDescription, Data: String);
begin
  DoException(nil, Trim(Error + ' ' + ErrorDescription));
end;

procedure TsgcWSClient.OnAuthTokenEvent(Sender: TObject;
  const TokenType, Token, Data: String);
begin
  if Token <> '' then
  begin
    Authentication.Token.AuthName := TokenType;
    Authentication.Token.AuthToken := Token;

    if not TCPClient.Connected then
      TCPClient.Connect;
  end
  else
    OnAuthTokenErrorEvent(self, 'OAuth Error', 'Token is empty', '');
end;

procedure TsgcWSClient.OnConnectionRedirect(Sender: TObject;
  const aLocation: string);
begin
  Disconnect;
  Try
    URL := aLocation;
  Finally
    Connect;
  End;
end;

procedure TsgcWSClient.OnGetPasswordEvent(var Password:
{$IFDEF INDY10_6}String{$ELSE} AnsiString {$ENDIF});
begin
  Password := TLSOptions.Password;
end;

function TsgcWSClient.OnVerifyPeerEvent(Certificate: TIdX509; AOk: Boolean
{$IFDEF INDY10_2}; ADepth {$IFDEF INDY10_5_8}, AError {$ENDIF}: Integer
{$ENDIF}): Boolean;
begin
  result := AOk;
  if Assigned(FOnSSLVerifyPeer) then
{$IFDEF INDY10_6}
    FOnSSLVerifyPeer(self, Certificate, AOk, ADepth, AError, result);
{$ELSE}
{$IFDEF INDY10_2}
    FOnSSLVerifyPeer(self, Certificate, True, ADepth, 0, result);
{$ELSE}
    FOnSSLVerifyPeer(self, Certificate, True, 0, 0, result);
{$ENDIF}
{$ENDIF}
end;

function TsgcWSClient.WriteAndWaitData(const aText: String;
  const aTimeout: Integer = 10000): string;
begin
  Try
    result := '';
    if Assigned(FWSConnection) then
      result := FWSConnection.WriteAndWaitData(aText, aTimeout);
  Except
    On E: Exception do
      DoError(FWSConnection, E.Message);
  end;
end;

procedure TsgcWSClient.WriteData(const aStream: TStream;
  const aSize: Integer = 0; const aStreaming: TwsStreaming = stmNone);
begin
  Try
    if Assigned(FWSConnection) then
      FWSConnection.WriteData(aStream, aSize, aStreaming);
  Except
    On E: Exception do
      DoError(FWSConnection, E.Message);
  end;
end;

constructor TsgcWSConnectionClient.Create;
begin
  inherited;
  FOptions := TsgcWSOptionsClient.Create;
  FAuthentication := TsgcWSAuthenticationClient_Options.Create;
end;

destructor TsgcWSConnectionClient.Destroy;
begin
  sgcFree(FAuthentication);
  sgcFree(FOptions);
  sgcFree(FHandshake);
  inherited;
end;

procedure TsgcWSConnectionClient.DoBeforeDisconnectEvent;
begin
  if FBeforeDisconnect then
    exit;

  if Assigned(FOnBeforeDisconnect) then
  begin
    FBeforeDisconnect := True;
    FOnBeforeDisconnect(self);
  end;
end;

procedure TsgcWSConnectionClient.DoClose(aCloseCode: Integer = CS_CLOSE_NORMAL);
begin
  DoBeforeDisconnectEvent;
  inherited;
end;

procedure TsgcWSConnectionClient.DoDisconnect(const AError: string = '';
  aCloseCode: Integer = CS_CLOSE_NORMAL);
begin
  DoBeforeDisconnectEvent;
  inherited;
end;

procedure TsgcWSConnectionClient.DoHandShakeExtensions;
begin
  Extensions.DecodeExtensions(FHandshake.Extensions);
end;

procedure TsgcWSConnectionClient.DoHandshake;
begin
  Assert(not Enabled);

  HeadersRequest.Clear;

  if Transport = trpRFC8441 then
    DoReadHandshake_RFC8441
  else
  begin
    TCPConnection.IOHandler.Capture(HeadersRequest, '');
    FHandshake := TsgcWSHandShakeClient.Create(TCPConnection, HeadersRequest);
    if FHandshake.Redirect and (FHandshake.Location <> '') then
    begin
      if Assigned(FOnRedirect) then
      begin
        FOnRedirect(self, FHandshake.Location);
        exit;
      end
      else
        raise TsgcWSException.Create('Redirect to: ' + FHandshake.Location);
    end;
    if FHandshake.GetWSAccept(FKey) <> FHandshake.Accept then
      raise TsgcWSException.Create(S_INVALID_ACCEPT_KEY);

    DoValidateProtocols;
    Protocol := FHandshake.Protocols;
    DoHandShakeExtensions;

    FEnabled := True;

    if Assigned(FOnEnabled) then
      FOnEnabled(self);
  end;
end;

procedure TsgcWSConnectionClient.DoReadHandshake_RFC8441;
var
  vBuffer: TIdBytes;
  vSize: Integer;
{$IFDEF SGC_HTTP2}
  oFrame: TsgcHTTP2_Frame;
{$ENDIF}
begin
  vSize := GetReadBufferSize;
  DoReadBytes(vBuffer, vSize);
{$IFDEF SGC_HTTP2}
  oFrame := TsgcHTTP2_Frame.Create;
  Try
    oFrame.ReadBytes(TBytes(vBuffer));
    // ... first frame is settings
    if oFrame.FrameType = h2ftSETTINGS then
    begin
      if oFrame.Frame_Settings.EnableConnectProtocol then
        DoSendHandshake_RFC8441
      else
        raise TsgcWSException.Create(S_ERROR_H2_SETTINGS_ENABLE_CONNECT);
    end
    // ... second frame is headers
    else if oFrame.FrameType = h2ftHEADERS then
    begin
      if oFrame.Frame_Headers.Headers.Values[':status'] = '200' then
      begin
        Protocol := oFrame.Frame_Headers.Headers.Values
          ['sec-websocket-protocol'];
        // DoValidateProtocols;
        // DoHandShakeExtensions;

        FEnabled := True;

        if Assigned(FOnEnabled) then
          FOnEnabled(self);
      end
      else
        raise TsgcWSException.Create(S_ERROR_H2_HEADERS_HANDSHAKE);
    end
    else
      raise TsgcWSException.Create(S_ERROR_H2_INVALID_FRAME_TYPE);
  Finally
    sgcFree(oFrame);
  End;
{$ENDIF}
end;

procedure TsgcWSConnectionClient.DoInitialize(aConnection: TIdTCPConnection);
begin
  TCPConnection := aConnection;
  case Transport of
    trpTCP:
      begin
        FEnabled := True;
        if Assigned(FOnEnabled) then
          FOnEnabled(self);
      end;
    trpRFC8441:
      begin
        if GetALPNProtocol <> CS_ALPN_H2 then
        begin
          Transport := trpRFC6455;
          DoSendHandshake_RFC6445;
        end;
      end
  else
    DoSendHandshake_RFC6445;
  end;
end;

procedure TsgcWSConnectionClient.DoValidateProtocols;
var
  oList: TsgcDelimitedStringList;
begin
  if (Protocol <> '') or (FHandshake.Protocols <> '') then
  begin
    oList := TsgcDelimitedStringList.Create;
    Try
      oList.DelimitedText := Protocol;

      if oList.IndexOf(FHandshake.Protocols) = -1 then
        raise TsgcWSException.CreateFmt(S_PROTOCOL_NOT_SUPPORTED,
          [FHandshake.Protocols]);

    Finally
      sgcFree(oList);
    End;
  end;
end;

function TsgcWSConnectionClient.GetALPNProtocol: string;
begin
  result := '';
{$IFDEF SGC_INDY_LIB}
  if TCPConnection.IOHandler.ClassType = TsgcIdSSLIOHandlerSocketOpenSSL then
    result := TsgcIdSSLIOHandlerSocketOpenSSL(TCPConnection.IOHandler)
      .GetALPNProtocol
{$IFDEF SGC_SCHANNEL}
  else if TCPConnection.IOHandler.ClassType = TsgcIdSSLIOHandlerSocketSChannel
  then
    result := TsgcIdSSLIOHandlerSocketSChannel(TCPConnection.IOHandler)
      .GetALPNProtocol;
{$ENDIF}
{$ENDIF}
end;

function TsgcWSConnectionClient.GetURL: String;
begin
  result := Options.Parameters;
end;

function TsgcWSConnectionClient.GetWSKey: String;
var
  oBytes: TIdBytes;
  i: Integer;
begin
  SetLength(oBytes, 16);
  Randomize;
  for i := 0 to Length(oBytes) - 1 do
    oBytes[i] := RandomRange(0, 255);
  result := HexToBase64(ToHex(oBytes));

  FKey := result;
end;

procedure TsgcWSConnectionClient.DoSendHandshake_RFC6445;
var
  i: Integer;
begin
  HeadersResponse.Clear;
  HeadersResponse.Add('GET ' + Options.Parameters + ' HTTP/1.1');
  if (Port = 80) or (Port = 443) then
    HeadersResponse.Add('Host: ' + Host)
  else
    HeadersResponse.Add('Host: ' + Host + ':' + IntToStr(Port));
  HeadersResponse.Add('Upgrade: websocket');
  HeadersResponse.Add('Connection: Upgrade');
  HeadersResponse.Add('Sec-WebSocket-Key: ' + GetWSKey);

  if Options.Origin <> '' then
    HeadersResponse.Add('Origin: ' + Options.Origin)
  else
    HeadersResponse.Add('Origin: ' + Host);
  if Protocol <> '' then
    HeadersResponse.Add('Sec-WebSocket-Protocol: ' + Protocol);
  if Authentication.Enabled then
  begin
    if Authentication.Basic.Enabled then
      HeadersResponse.Add(CS_AUTH_BASIC + ' ' + EncodeBase64(Authentication.User
        + ':' + Authentication.Password));
    if Authentication.Token.Enabled then
      HeadersResponse.Add(CS_AUTHORIZATION + Authentication.Token.AuthName + ' '
        + Authentication.Token.AuthToken);
  end;
  // ... extensions
  Extensions.WriteHeader(HeadersResponse);

  HeadersResponse.Add('Sec-WebSocket-Version: 13');

  if Assigned(FOnHandshake) then
    FOnHandshake(self, FHeadersResponse);

  TCPConnection.IOHandler.WriteBufferOpen;
  Try
    for i := 0 to HeadersResponse.Count - 1 do
      TCPConnection.IOHandler.WriteLn(HeadersResponse[i]);
    TCPConnection.IOHandler.WriteLn;
    TCPConnection.IOHandler.WriteBufferFlush;
  Finally
    TCPConnection.IOHandler.WriteBufferClose;
  End;
end;

procedure TsgcWSConnectionClient.DoSendHandshake_RFC8441;
var
  i: Integer;
{$IFDEF SGC_HTTP2}
  // oFrame: TsgcHTTP2_Frame;
  // vBytes: TBytes;
{$ENDIF}
begin
  HeadersResponse.Clear;
  HeadersResponse.Add(':method = CONNECT');
  HeadersResponse.Add(':protocol = websocket');
  HeadersResponse.Add(':scheme = https');
  HeadersResponse.Add(':path = ' + Options.Parameters);
  if (Port = 80) or (Port = 443) then
    HeadersResponse.Add(':authority = ' + Host)
  else
    HeadersResponse.Add(':authority = ' + Host + ':' + IntToStr(Port));
  if Options.Origin <> '' then
    HeadersResponse.Add('origin = ' + Options.Origin)
  else
    HeadersResponse.Add('origin = ' + Host);
  if Protocol <> '' then
    HeadersResponse.Add('sec-websocket-protocol = ' + Protocol);
  if Authentication.Enabled then
  begin
    if Authentication.Basic.Enabled then
      HeadersResponse.Add(CS_AUTH_BASIC + ' ' + EncodeBase64(Authentication.User
        + '=' + Authentication.Password));
    if Authentication.Token.Enabled then
      HeadersResponse.Add(CS_AUTHORIZATION + Authentication.Token.AuthName + ' '
        + Authentication.Token.AuthToken);
  end;
  // ... extensions
  Extensions.WriteHeader(HeadersResponse);
  for i := 0 to HeadersResponse.Count - 1 do
    HeadersResponse[i] := sgcStringReplace(HeadersResponse[i],
      'sec-websocket-extensions:', 'sec-websocket-extensions = ');

  HeadersResponse.Add('sec-webSocket-version = 13');

  if Assigned(FOnHandshake) then
    FOnHandshake(self, FHeadersResponse);
end;

procedure TsgcWSConnectionClient.SetOptions(const Value: TsgcWSOptionsClient);
begin
  if Assigned(FOptions) then
    FOptions.Assign(Value);
end;

procedure TsgcWSConnectionClient.SetAuthentication
  (const Value: TsgcWSAuthenticationClient_Options);
begin
  if Assigned(FAuthentication) then
    FAuthentication.Assign(Value);
end;

constructor TsgcWSOptionsClient.Create;
begin
  inherited;
  ValidateUTF8 := False;
  CleanDisconnect := False;
  RaiseDisconnectExceptions := True;
  FragmentedMessages := frgOnlyBuffer;
end;

procedure TsgcWSOptionsClient.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSOptionsClient then
  begin
    FParameters := TsgcWSOptionsClient(aSource).Parameters;
    FOrigin := TsgcWSOptionsClient(aSource).Origin;
    FValidateUTF8 := TsgcWSOptionsClient(aSource).ValidateUTF8;
    FCleanDisconnect := TsgcWSOptionsClient(aSource).CleanDisconnect;
    FRaiseDisconnectExceptions := TsgcWSOptionsClient(aSource)
      .RaiseDisconnectExceptions;
    FragmentedMessages := TsgcWSOptionsClient(aSource).FragmentedMessages;
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

procedure TsgcWSOptionsClient.SetParameters(const Value: String);
begin
  FParameters := Value;
end;

constructor TsgcWSAuthenticationClient_Options.Create;
begin
  FURL := TsgcWSAuthenticationClient_URL.Create;
  FSession := TsgcWSAuthenticationClient_Session.Create;
  FBasic := TsgcWSAuthenticationClient_Basic.Create;
  FToken := TsgcWSAuthenticationClient_Token.Create;
end;

destructor TsgcWSAuthenticationClient_Options.Destroy;
begin
  sgcFree(FToken);
  sgcFree(FBasic);
  sgcFree(FURL);
  sgcFree(FSession);
  inherited;
end;

procedure TsgcWSAuthenticationClient_Options.Assign(aSource: TPersistent);
begin
  inherited Assign(aSource);
  if aSource is TsgcWSAuthenticationClient_Options then
  begin
    User := TsgcWSAuthenticationClient_Options(aSource).User;
    Password := TsgcWSAuthenticationClient_Options(aSource).Password;

    FSession.Assign(TsgcWSAuthenticationClient_Options(aSource).Session);
    FURL.Assign(TsgcWSAuthenticationClient_Options(aSource).URL);
    FBasic.Assign(TsgcWSAuthenticationClient_Options(aSource).Basic);
    FToken.Assign(TsgcWSAuthenticationClient_Options(aSource).Token);
  end
end;

procedure TsgcWSAuthenticationClient_Options.SetURL
  (const Value: TsgcWSAuthenticationClient_URL);
begin
  if Assigned(FURL) then
    FURL.Assign(Value);
end;

procedure TsgcWSAuthenticationClient_Options.SetSession
  (const Value: TsgcWSAuthenticationClient_Session);
begin
  if Assigned(FSession) then
    FSession.Assign(Value);
end;

procedure TsgcWSAuthenticationClient_Options.SetBasic
  (const Value: TsgcWSAuthenticationClient_Basic);
begin
  if Assigned(FBasic) then
    FBasic.Assign(Value);
end;

procedure TsgcWSAuthenticationClient_Options.SetToken
  (const Value: TsgcWSAuthenticationClient_Token);
begin
  if Assigned(FToken) then
    FToken.Assign(Value);
end;

constructor TClientThread.Create(const aMethod: string;
  const aClient: TsgcWSClient);
begin
  FMethod := aMethod;
  FClient := aClient;
  FreeOnTerminate := True;
  inherited Create(False);
{$IFDEF DEBUG}
  sgcSetThreadName(ClassName);
{$ENDIF}
end;

procedure TClientThread.Execute;
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

class procedure TClientThread.Start(const aClient: TsgcWSClient);
begin
  Create(CS_START, aClient);
end;

class procedure TClientThread.Stop(const aClient: TsgcWSClient);
begin
  Create(CS_STOP, aClient);
end;

constructor TsgcWSLoadBalancerClient_Options.Create;
begin
  inherited;
  FServers := TStringList.Create;
  Enabled := False;
end;

destructor TsgcWSLoadBalancerClient_Options.Destroy;
begin
  sgcFree(FServers);
  inherited;
end;

procedure TsgcWSLoadBalancerClient_Options.SetServers(const Value: TStringList);
begin
  if Assigned(FServers) then
    FServers.Assign(Value);
end;

constructor TsgcWSAuthenticationClient_Token.Create;
begin
  inherited;
  FJWT := nil;
  FOAuth := nil;
end;

end.
