{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Server;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils, StrUtils,
{$IFDEF NEXTGEN}System.Generics.Collections{$ELSE}Contnrs{$ENDIF},
{$IFDEF MSWINDOWS}Windows, {$ENDIF}
  // indy
{$IFDEF SGC_INDY}sgcIdContext{$ELSE}IdContext{$ENDIF},
{$IFDEF SGC_INDY}sgcIdTCPConnection{$ELSE}IdTCPConnection{$ENDIF},
{$IFDEF SGC_INDY}sgcIdTCPServer{$ELSE}IdTCPServer{$ENDIF},
{$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
{$IFDEF SGC_INDY}sgcIdHeaderList{$ELSE}IdHeaderList{$ENDIF},
{$IFDEF SGC_INDY}sgcIdException{$ELSE}IdException{$ENDIF},
{$IFDEF SGC_INDY}sgcIdCustomHTTPServer{$ELSE}IdCustomHTTPServer{$ENDIF},
{$IFDEF SGC_INDY}sgcIdSocketHandle{$ELSE}IdSocketHandle{$ENDIF},
{$IFDEF SGC_INDY}sgcIdHTTPServer{$ELSE}IdHTTPServer{$ENDIF},
{$IFDEF SGC_INDY}sgcIdCustomTCPServer{$ELSE}IdCustomTCPServer{$ENDIF},
{$IFDEF SGC_INDY}sgcIdSchedulerOfThreadPool{$ELSE}IdSchedulerOfThreadPool{$ENDIF},
{$IFDEF SGC_INDY}sgcIdGlobalProtocols{$ELSE}IdGlobalProtocols{$ENDIF},
{$IFDEF SGC_INDY}sgcIdServerIOHandler{$ELSE}IdServerIOHandler{$ENDIF},
{$IFDEF SGC_INDY}sgcIdScheduler{$ELSE}IdScheduler{$ENDIF},
{$IFDEF SGC_INDY}sgcIdSSL{$ELSE}IdSSL{$ENDIF},
{$IFDEF SGC_INDY}sgcIdSSLOpenSSL{$ELSE}IdSSLOpenSSL{$ENDIF},
{$IFNDEF LAZARUS}
{$IFDEF SGC_INDY}sgcIdInterceptThrottler{$ELSE}IdInterceptThrottler{$ENDIF},
{$ENDIF}
  // sgcIndy
{$IFDEF SGC_INDY_IOCP}
  sgcIndy_IOHandler_IOCP_Server, sgcIndy_Scheduler_IOCP, sgcIndy_IOHandler_IOCP,
  sgcIndy_IOHandler_IOCP_OpenSSL,
{$ENDIF}
{$IFDEF SGC_HTTP2}sgcHTTP2_Classes, sgcHTTP2_Frame, {$ENDIF}
{$IFDEF SGC_HTML}sgcWebSocket_Classes_HTML, {$ENDIF}
  // websocket
  sgcWebSocket_Classes, sgcWebSocket_Classes_Indy, sgcWebSocket_Types,
  sgcWebSocket_CustomServer, sgcWebSocket_LoadBalancer_Client,
  sgcWebSocket_Helpers, sgcWebSocket_Server_Base, sgcTCP_Classes,
  sgcBase_Helpers;

type

  TsgcWSOnSSLGetHandler = procedure(Sender: TObject; aType: TwsSSLHandler;
    var aSSLHandler: TIdServerIOHandlerSSLBase) of object;
  TsgcWSOnSSLAfterCreateHandler = procedure(Sender: TObject;
    aType: TwsSSLHandler; aSSLHandler: TIdServerIOHandlerSSLBase) of object;
  TsgcWSOnSSLALPNSelect = procedure(Sender: TObject; aProtocols: TStringList;
    var aProtocol: String) of object;
  TsgcWSUnknownAuthenticationEvent = procedure(Connection: TsgcWSConnection;
    AuthType, AuthData: String; var aUser, aPassword: String;
    var Authenticated: Boolean) of object;
  TsgcWSOnTCPConnect = procedure(Connection: TsgcWSConnection;
    var Accept: Boolean) of object;
  TsgcWSOnBeforeForwardHTTP = procedure(Connection: TsgcWSConnection;
    ARequestInfo: TIdHTTPRequestInfo; aForward: TsgcWSServerForwardHTTP)
    of object;
  TsgcWSOnAfterForwardHTTP = procedure(Connection: TsgcWSConnection;
    ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo;
    E: Exception) of object;

  TsgcWSServer_Indy_Base = class;

  // TsgcWSHandShakeServer //

  TsgcWSHandShakeServer = class(TsgcWSHandshakeServer_Base)
  private
    procedure DoDecodeHeaders(const aConnection: TIdTCPConnection;
      aHeaders: TIdHeaderList);
  public
    constructor Create(aConnection: TIdTCPConnection; aHeaders: TIdHeaderList);
      reintroduce; overload; virtual;
  end;

  // TsgcWSConnectionServer //

  TsgcWSConnectionServer = class(TsgcWSConnection_Indy)
    { from TsgcWSConnection }
  protected
    function GetURL: String; override;
    { from TsgcWSConnection }

    { from TsgcTCPConnection_Base }
  protected
    procedure SetTransport(const Value: TwsTransport); override;
    { from TsgcTCPConnection_Base }

    { server properties }
  private
    FReadCount: Integer;
    FReadStartSSLCount: Integer;
    FServerOriginsAllowed: String;
    FServerSSL: Boolean;
    FCustomHeartBeat: Boolean;
  protected
{$IFDEF SGC_HTTP2}
    FContext: TIdContext;
{$ENDIF}
  public
    property ServerOriginsAllowed: String read FServerOriginsAllowed
      write FServerOriginsAllowed;
    property ServerSSL: Boolean read FServerSSL write FServerSSL;
    { server properties }

    { RequestURL protocol }
  private
    function DoGetProtocol: string;
    { RequestURL protocol }

    { handshake }
  private
    FHandshake: TsgcWSHandShakeServer;
    procedure DoHandShake_RFC6455;
    procedure DoHandShake_Hixie76;
  private
    procedure DoOriginsAllowed;
  protected
    procedure DoHandShakeSecurity; virtual;
    procedure DoHandShakeExtensions; virtual;
  protected
    procedure DoHandshake; override;
  public
    property HandShake: TsgcWSHandShakeServer read FHandshake;
    { handshake }

    { sse }
  protected
    procedure DoProtocolSSE; virtual;
    { sse }

    { authentication }
  private
    FAuthentication: TsgcWSConnectionAuthentication;
    FOnAuthentication: TsgcWSAuthenticationEvent;
  protected
    function GetAuthentication: TsgcWSConnectionAuthentication;
  protected
    procedure DoOnAuthenticationEvent; virtual;
  protected
    FAuthenticationCloseCode: Integer;
  public
    procedure SetAuthenticationCloseCode(const aCloseCode: Integer);
  public
    property Authentication: TsgcWSConnectionAuthentication
      read GetAuthentication write FAuthentication;
  public
    property OnAuthentication: TsgcWSAuthenticationEvent read FOnAuthentication
      write FOnAuthentication;
    { authentication }

    { http }
  private
    FIPVersion: TIdIPVersion;
  private
    function DoHTTPFileResponse(const aFileName, aContentType: String;
      aDisconnect: Boolean; aStreamIdentifier: Integer = 0): Boolean;
  private
    procedure DoAuthSessionResponse(const aParams: String);
  protected
    function DoAuthenticationURL(const aParams: string): Boolean;
    function DoAuthenticationBasic(const aParams: string): Boolean;
  public
    procedure SendResponseHTTP(const aContent, aContentType: String);
  protected
    property IPVersion: TIdIPVersion read FIPVersion write FIPVersion;
    { http }

    { constructor / destructor }
  public
    constructor Create; override;
    destructor Destroy; override;
    { constructor / destructor }
  end;

  // TsgcWThreadPool_Options //
  TsgcWSThreadPool_Options = class(TPersistent)
  private
    FMaxThreads: Integer;
    FPoolSize: Integer;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property MaxThreads: Integer read FMaxThreads write FMaxThreads;
    property PoolSize: Integer read FPoolSize write FPoolSize;
  end;

  TsgcWSIOHandler_IOCP_Options = class(TPersistent)
  private
    FIOCPThreads: Integer;
    FTimeOut: Integer;
    FWorkOpThreads: Integer;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property IOCPThreads: Integer read FIOCPThreads write FIOCPThreads;
    property TimeOut: Integer read FTimeOut write FTimeOut;
    property WorkOpThreads: Integer read FWorkOpThreads write FWorkOpThreads;
  end;

  TsgcWSIOHandler_Options = class(TPersistent)
  private
    FIOCP: TsgcWSIOHandler_IOCP_Options;
    FIOHandlerType: TwsIOHandler;
    procedure SetIOCP(const Value: TsgcWSIOHandler_IOCP_Options);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property IOCP: TsgcWSIOHandler_IOCP_Options read FIOCP write SetIOCP;
    property IOHandlerType: TwsIOHandler read FIOHandlerType
      write FIOHandlerType;
  end;

  // TsgcWSLoadBalancerServer_Options //
  TsgcWSLoadBalancerServer_Options = class(TPersistent)
  private
    FAutoRegisterBindings: Boolean;
    FAutoRestart: Integer;
    FEnabled: Boolean;
    FHost: string;
    FPort: Integer;
    FBindings: TStringList;
    FGuid: String;
    procedure SetBindings(const Value: TStringList);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property AutoRegisterBindings: Boolean read FAutoRegisterBindings
      write FAutoRegisterBindings;
    property AutoRestart: Integer read FAutoRestart write FAutoRestart;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Host: string read FHost write FHost;
    property Port: Integer read FPort write FPort;
    property Bindings: TStringList read FBindings write SetBindings;
    property Guid: String read FGuid write FGuid;
  end;

  // TServerThread //
  TServerThread = class(TThread)
  private
    FServer: TsgcWSServer_Indy_Base;
    FMethod: string;
  protected
    procedure Execute; override;
  public
    constructor Create(const aMethod: string;
      const aServer: TsgcWSServer_Indy_Base);
    class procedure Start(const aServer: TsgcWSServer_Indy_Base);
    class procedure Stop(const aServer: TsgcWSServer_Indy_Base);
    class procedure ReStart(const aServer: TsgcWSServer_Indy_Base);
  end;


  // TsgcWSServer_Indy_Base //

  TsgcWSServer_Indy_Base = class
    ({$IFDEF SGC_HTML}TsgcWSComponent_Server_HTML{$ELSE}TsgcWSComponent_Server{$ENDIF})

    { from TComponent }
  protected
    procedure Loaded; override;
    { from TComponent }

    { from TsgcWSComponent_Base }
    procedure DoNotifyConnect(aConnection: TsgcWSConnection); override;
    procedure DoNotifyDisconnect(aConnection: TsgcWSConnection); override;
    { from TsgcWSComponent_Base }

    { LockList }
  public
    function LockList:
{$IFDEF NEXTGEN}TList <
{$IFDEF SGC_INDY}sgcIdContext{$ELSE}IdContext{$ENDIF}.TIdContext >
{$ELSE}TList{$ENDIF}; override;
    procedure UnLockList; override;
    { LockList }

    { tcp server }
  protected
    function GetCustomServer: TIdCustomTCPServer; virtual; abstract;
  protected
    property CustomServer: TIdCustomTCPServer read GetCustomServer;
    { tcp server }

    { Throttle }
  private
    FThrottle: TsgcWSThrottle;
  protected
    function GetThrottle: TsgcWSThrottle; virtual;
    procedure SetThrottle(const Value: TsgcWSThrottle); virtual;
  public
    property Throttle: TsgcWSThrottle read GetThrottle write SetThrottle;
    { Throttle }

    { LogFile }
  private
    FInterceptLogFile: TsgcWSIdLogFileServer;
    function GetInterceptLogFile: TsgcWSIdLogFileServer;
  protected
    property InterceptLogFile: TsgcWSIdLogFileServer read GetInterceptLogFile
      write FInterceptLogFile;
    { LogFile }

    { WatchDog }
  private
    FWatchDogMonitorSecret: string;
    procedure DoStart;
    procedure DoWatchDogMonitor;
  protected
    procedure OnWatchDogEvent(Sender: TObject); override;
    procedure OnWatchDogExceptionEvent(Sender: TObject; E: Exception); override;
    { WatchDog }

    { events }
  private
    FOnTCPConnect: TsgcWSOnTCPConnect;
  protected
    procedure OnClientUpdateEvent(aConnection: TsgcWSConnection;
      aType: Integer);
    procedure OnClientDisconnectedEvent(Sender: TObject);
  protected
    procedure OnServerConnectEvent(AContext: TIdContext); virtual;
    procedure OnServerMessageEvent(aConnection: TsgcWSConnection;
      const Text: string); virtual;
    procedure OnServerBinaryEvent(aConnection: TsgcWSConnection;
      const aStream: TMemoryStream); virtual;
    procedure OnServerFragmentedEvent(aConnection: TsgcWSConnection;
      const aData: TMemoryStream; const aOpCode: TOpCode;
      const aContinuation: Boolean); virtual;
    procedure OnServerDisconnectEvent(AContext: TIdContext); virtual;
    procedure OnServerExecuteEvent(AContext: TIdContext); virtual;
    procedure OnServerSubscriptionEvent(Connection: TsgcWSConnection;
      const Subscription: String);
    procedure OnServerUnSubscriptionEvent(Connection: TsgcWSConnection;
      const Subscription: String);
  protected
    procedure OnServerStartupEvent(Sender: TObject); virtual;
    procedure OnServerShutdownEvent(Sender: TObject); virtual;
  public
    property OnTCPConnect: TsgcWSOnTCPConnect read FOnTCPConnect
      write FOnTCPConnect;
    { events }

    { properties fields }
  private
    FOptions: TsgcWSOptionsServer;
    FServerType: TwsServerType;
    FSecurityOptions: TsgcWSSecurity_Options;
    FQueueOptions: TsgcWSQueueServer_Options;
    FUseNagle: Boolean;
    FMaxReadBufferSize: Int64;
    function GetMaxConnections: Integer;
    procedure SetMaxConnections(const Value: Integer);
    function GetBindings: TIdSocketHandles;
    function GetPort: Integer;
    procedure SetBindings(const Value: TIdSocketHandles);
    procedure _SetPort(const Value: Integer);
    procedure SetSecurityOptions(const Value: TsgcWSSecurity_Options);
    procedure SetOptions(const Value: TsgcWSOptionsServer);
    procedure SetQueueOptions(const Value: TsgcWSQueueServer_Options);
  public
    property Port: Integer read GetPort write _SetPort;
    property Bindings: TIdSocketHandles read GetBindings write SetBindings;
    property MaxConnections: Integer read GetMaxConnections
      write SetMaxConnections;
    property Options: TsgcWSOptionsServer read FOptions write SetOptions;
    property SecurityOptions: TsgcWSSecurity_Options read FSecurityOptions
      write SetSecurityOptions;
    property QueueOptions: TsgcWSQueueServer_Options read FQueueOptions
      write SetQueueOptions;
    property UseNagle: Boolean read FUseNagle write FUseNagle stored True;
    property MaxReadBufferSize: Int64 read FMaxReadBufferSize
      write FMaxReadBufferSize;
    { properties fields }

    { SSL }
  private
    FSSL: Boolean;
    FSSLOptions: TsgcWSSSL_Options;
    FHandlerSSL: TIdServerIOHandlerSSLBase;
  protected
    FOnSSLGetHandler: TsgcWSOnSSLGetHandler;
    FOnSSLAfterCreateHandler: TsgcWSOnSSLAfterCreateHandler;
  protected
    function GetSSL: Boolean;
    procedure OnGetPasswordSSLEvent(var Password:
{$IFDEF INDY10_6}String{$ELSE}AnsiString{$ENDIF}); virtual;
    procedure OnGetSSLPortEvent(aPort: Integer; var aSSL: Boolean); virtual;
    procedure SetSSL(const Value: Boolean); virtual;
    procedure SetSSLOptions(const Value: TsgcWSSSL_Options); virtual;
    function IsOpenSSL: Boolean;
  protected
    procedure GetHandlerSSLEvent(const aSSLType: TwsSSLHandler;
      var aSSLHandler: TIdServerIOHandlerSSLBase); virtual;
  protected
    procedure DoOpenSSLECDHE;
  public
    property SSLOptions: TsgcWSSSL_Options read FSSLOptions write SetSSLOptions;
    property SSL: Boolean read GetSSL write SetSSL default False;
  public
    property OnSSLGetHandler: TsgcWSOnSSLGetHandler read FOnSSLGetHandler
      write FOnSSLGetHandler;
    property OnSSLAfterCreateHandler: TsgcWSOnSSLAfterCreateHandler
      read FOnSSLAfterCreateHandler write FOnSSLAfterCreateHandler;
    { SSL }

    { ALPN }
  private
    FOnSSLALPNSelect: TsgcWSOnSSLALPNSelect;
  protected
    procedure OnSSLALPNSelectEvent(Sender: TObject; aProtocols: TStringList;
      var aProtocol: String); virtual;
  public
    property OnSSLALPNSelect: TsgcWSOnSSLALPNSelect read FOnSSLALPNSelect
      write FOnSSLALPNSelect;
    { ALPN }

    { HTTPUploadFiles }
  private
    FHTTPUploadFiles: TsgcHTTPUploadFilesServer;
    procedure SetHTTPUploadFiles(const Value: TsgcHTTPUploadFilesServer);
  protected
    procedure OnHTTPUploadBeforeSaveFileEvent(Sender: TObject; var aFileName,
        aFilePath: string); virtual;
    procedure OnHTTPUploadAfterSaveFileEvent(Sender: TObject; const aFileName,
        aFilePath: string); virtual;
  public
    property HTTPUploadFiles: TsgcHTTPUploadFilesServer read FHTTPUploadFiles
      write SetHTTPUploadFiles;
    { HTTPUploadFiles }

    { http/2 }
  private
    FHTTP2Options: TsgcWSHTTP2Server_Options;
    procedure SetHTTP2Options(const Value: TsgcWSHTTP2Server_Options);
  protected
    procedure DoStartHTTP2(const aConnection: TsgcWSConnectionServer); virtual;
  protected
    procedure OnHTTP2RequestEvent(const aConnection: TsgcWSConnection;
      const aHeaders: TStringList; const aBytes: TBytes;
      aStreamIdentifierRequest, aStreamIdentifierPush: Integer;
      const aPostStream: TStream); virtual;
    procedure OnHTTP2ReadCreatePostStreamEvent(Sender: TObject;
      const aHeaders: TStrings; var Stream: TStream); virtual;
    procedure OnHTTP2ReadDonePostStreamEvent(Sender: TObject;
      const aHeaders: TStrings; const aStream: TStream); virtual;
  protected
    property HTTP2Options: TsgcWSHTTP2Server_Options read FHTTP2Options
      write SetHTTP2Options;
    { http/2 }

    { OAuth2 }
  protected
    procedure OnOAuth2AuthenticationEvent(aConnection: TsgcWSConnection;
      aUser, aPassword: String; var Authenticated: Boolean); virtual;
    procedure OnOAuth2ResponseEvent(aConnection: TsgcWSConnection;
      aContent, aContentType: string); virtual;
    procedure OnOAuth2ResponseErrorEvent(aConnection
      : TsgcWSConnection); virtual;
    { OAuth2 }

    { jwt }
  protected
    procedure OnJWTResponseErrorEvent(aConnection: TsgcWSConnection); virtual;
    { jwt }

    { ThreadPoolOptions }
  private
    FThreadPool: Boolean;
    FThreadPoolOptions: TsgcWSThreadPool_Options;
    FSchedulerThreadPool: TIdSchedulerOfThreadPool;
  protected
    procedure SetThreadPoolOptions(const Value
      : TsgcWSThreadPool_Options); virtual;
    function GetThreadPool: Boolean; virtual;
    procedure SetThreadPool(const Value: Boolean); virtual;
  public
    property ThreadPoolOptions: TsgcWSThreadPool_Options read FThreadPoolOptions
      write SetThreadPoolOptions;
    property ThreadPool: Boolean read GetThreadPool write SetThreadPool;
    { ThreadPoolOptions }

    { IOHandlerOptions }
  private
    FIOHandlerOptions: TsgcWSIOHandler_Options;
  private
    procedure DoIOHandler;
    procedure DoScheduler;
  protected
    procedure SetIOHandlerOptions(const Value
      : TsgcWSIOHandler_Options); virtual;
  public
    property IOHandlerOptions: TsgcWSIOHandler_Options read FIOHandlerOptions
      write SetIOHandlerOptions;
    { IOHandlerOptions }

    { authentication }
  private
    FSessionIDList: TsgcDelimitedStringList;
    FAuthentication: TsgcWSAuthenticationServer_Options;
  protected
    function GetSessionIDList: TsgcDelimitedStringList;
    procedure DoAddSessionID(const aSessionID, aUser, aPassword: String);
    procedure DoDelSessionID(const aSessionID: String);
    function GetSessionID(const aSessionID: String): Boolean;
    function GetSessionUserPassword(const aSessionID: String;
      var User, Password: string): Boolean;
  protected
    procedure SetAuthentication(const Value
      : TsgcWSAuthenticationServer_Options);
  protected
    function DoAuthenticationSession(aConnection: TsgcWSConnectionServer;
      const aParams: String): Boolean;
    function DoUnknownAuthentication(aConnection
      : TsgcWSConnectionServer): Boolean;
  public
    procedure OnServerAuthenticationEvent(Connection: TsgcWSConnection;
      aUser, aPassword: String; var Authenticated: Boolean);
  public
    property Authentication: TsgcWSAuthenticationServer_Options
      read FAuthentication write SetAuthentication;
    { authentication }

    { start / stop }
  public
    procedure Start;
    procedure Stop;
    procedure ReStart;
    { start / stop }

    { xhr }
  protected
    procedure DoXHR(const aConnection: TsgcWSConnectionServer; const aStream:
        TStream = nil; aStreamIdentifier: Integer = 0); virtual;
    { xhr }

    { http protocol }
  protected
    procedure DoHTTPProtocol(const aConnection: TsgcWSConnectionServer; const
        aStream: TStream = nil; aStreamIdentifier: Integer = 0); virtual;
    { http protocol }

    { fallback }
  private
    FFallBack: TsgcWSFallBack_Options;
  protected
    procedure SetFallBack(const Value: TsgcWSFallBack_Options);
  public
    property FallBack: TsgcWSFallBack_Options read FFallBack write SetFallBack;
    { fallback }

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

    { LoadBalancer }
  private
    FLoadBalancer: TsgcWSLoadBalancerServer_Options;
  private
    FClientLoadBalancer: TsgcWSLoadBalancerClient;
  private
    FOnLoadBalancerConnect: TsgcWSConnectEvent;
    FOnLoadBalancerDisconnect: TsgcWSDisconnectEvent;
    FOnLoadBalancerError: TsgcWSLoadBalancerErrorEvent;
  protected
    procedure SetLoadBalancer(const Value: TsgcWSLoadBalancerServer_Options);
  protected
    function GetClientLoadBalancer: TsgcWSLoadBalancerClient;
  protected
    procedure OnLoadBalancerConnectEvent(Connection: TsgcWSConnection); virtual;
    procedure OnLoadBalancerDisconnectEvent(Connection: TsgcWSConnection;
      Code: Integer); virtual;
    procedure OnLoadBalancerErrorEvent(Connection: TsgcWSConnection;
      const Error: string); virtual;
    procedure OnLoadBalancerExceptionEvent(Connection: TsgcWSConnection;
      E: Exception); virtual;
  protected
    function GetLoadBalancerConnectionMessage(const aConnection
      : TsgcWSConnection; aConnected: Boolean): String; virtual;
  protected
    procedure DoLoadBalancerSendServerData; virtual;
    procedure DoLoadBalancerSendActiveConnections; virtual;
    procedure DoLoadBalancerSendServerBindings; virtual;
    procedure DoLoadBalancerSendServerReady; virtual;
  protected
    procedure OnLoadBalancerWriteDataTextEvent(const aGuid,
      aText: String); virtual;
    procedure OnLoadBalancerWriteDataBinaryEvent(const aGuid: String;
      aStream: TStream); virtual;
    procedure OnLoadBalancerBroadCastTextEvent(const aText, aChannel, aProtocol,
      aExclude, aInclude: String); virtual;
    procedure OnLoadBalancerBroadCastBinaryEvent(aStream: TStream;
      const aChannel, aProtocol, aExclude, aInclude: String; aSize: Integer;
      aStreaming: TwsStreaming); virtual;
  public
    property LoadBalancer: TsgcWSLoadBalancerServer_Options read FLoadBalancer
      write SetLoadBalancer;
    property ClientLoadBalancer: TsgcWSLoadBalancerClient
      read GetClientLoadBalancer write FClientLoadBalancer;
    property OnLoadBalancerConnect: TsgcWSConnectEvent
      read FOnLoadBalancerConnect write FOnLoadBalancerConnect;
    property OnLoadBalancerDisconnect: TsgcWSDisconnectEvent
      read FOnLoadBalancerDisconnect write FOnLoadBalancerDisconnect;
    property OnLoadBalancerError: TsgcWSLoadBalancerErrorEvent
      read FOnLoadBalancerError write FOnLoadBalancerError;
    { LoadBalancer }

    { built-in libraries }
  protected
    function DoBuiltInLibraries(aConnection: TsgcWSConnectionServer;
      const aText: String; aDisconnect: Boolean; aStreamIdentifier: Integer = 0)
      : Boolean; virtual;
    { built-in libraries }

    { connections }
  private
    function GetCount: Integer;
    function GetConnectionByIndex(Index: Integer): TsgcWSConnectionServer;
    function GetConnectionByGuid(const aGuid: String): TsgcWSConnectionServer;
  protected
    procedure DoTCPDisconnect(aConnection: TsgcWSConnectionServer); virtual;
    procedure DoContextFree(AContext: TIdContext); virtual;
  public
    procedure DisconnectAll;
  public
    property ConnectionsByGUID[const Guid: String]: TsgcWSConnectionServer
      read GetConnectionByGuid;
    property Connections[Index: Integer]: TsgcWSConnectionServer
      read GetConnectionByIndex; default;
    property Count: Integer read GetCount;
    { connections }

    { enable / disable }
  private
    FActive: Boolean;
  private
    function GetActive: Boolean;
  protected
    procedure SetActive(const Value: Boolean); virtual;
  public
    property Active: Boolean read GetActive write SetActive default False;
    { enable / disable }

    { constructor / destructor }
  private
    procedure DoClear;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { broadcast }
  protected
    procedure DoBroadCast(const aMessage: string; aStream: TStream;
      aOpCode: TOpCode; const aChannel: String = '';
      const aProtocol: string = ''; const Exclude: String = '';
      const Include: String = ''; const aSize: Integer = 0;
      const aStreaming: TwsStreaming = stmNone); virtual;
  public
    procedure Broadcast(const aMessage: string; const aChannel: string = '';
      const aProtocol: string = ''; const Exclude: String = '';
      const Include: String = ''); override;
    procedure Broadcast(aStream: TStream; const aChannel: string = '';
      const aProtocol: string = ''; const Exclude: String = '';
      const Include: String = ''; const aSize: Integer = 0;
      const aStreaming: TwsStreaming = stmNone); override;
    { broadcast }

    { send }
  private
    function DoWriteData(const aGuid, aMessage: string): Boolean; overload;
    function DoWriteData(const aGuid: string; const aStream: TStream;
      aSize: Integer = 0; const aStreaming: TwsStreaming = stmNone)
      : Boolean; overload;
  public
    function WriteData(const aGuid, aMessage: string): Boolean; override;
    function WriteData(const aGuid: String; aStream: TStream;
      aSize: Integer = 0; const aStreaming: TwsStreaming = stmNone)
      : Boolean; override;
    { send }

    { ping }
  protected
    procedure DoPing(const aText: string = ''); virtual;
  public
    procedure Ping(const aText: string = '');
    { ping }

    { events }
  private
    FOnStartup: TNotifyEvent;
    FOnShutdown: TNotifyEvent;
    FOnAuthentication: TsgcWSAuthenticationEvent;
    FOnUnknownAuthentication: TsgcWSUnknownAuthenticationEvent;
    FOnHTTPUploadAfterSaveFile: TsgcWSHTTPUploadAfterSaveFileEvent;
    FOnHTTPUploadBeforeSaveFile: TsgcWSHTTPUploadBeforeSaveFileEvent;
  public
    property OnStartup: TNotifyEvent read FOnStartup write FOnStartup;
    property OnShutdown: TNotifyEvent read FOnShutdown write FOnShutdown;
    property OnAuthentication: TsgcWSAuthenticationEvent read FOnAuthentication
      write FOnAuthentication;
    property OnUnknownAuthentication: TsgcWSUnknownAuthenticationEvent
      read FOnUnknownAuthentication write FOnUnknownAuthentication;
    property OnHTTPUploadBeforeSaveFile: TsgcWSHTTPUploadBeforeSaveFileEvent
        read FOnHTTPUploadBeforeSaveFile write FOnHTTPUploadBeforeSaveFile;
    property OnHTTPUploadAfterSaveFile: TsgcWSHTTPUploadAfterSaveFileEvent
        read FOnHTTPUploadAfterSaveFile write FOnHTTPUploadAfterSaveFile;
    { events }
  end;




  // TsgcWSServer //

  TsgcWSServer = Class(TsgcWSServer_Indy_Base)
    { tcp server }
  private
    FTCPServer: TsgcWSCustomServer;
  private
    function GetTCPServer: TsgcWSCustomServer;
  protected
    property TCPServer: TsgcWSCustomServer read GetTCPServer;
    { tcp server }

    { http/2 }
  protected
    procedure OnHTTP2RequestEvent(const aConnection: TsgcWSConnection;
      const aHeaders: TStringList; const aBytes: TBytes;
      aStreamIdentifierRequest, aStreamIdentifierPush: Integer;
      const aPostStream: TStream); override;
    { http/2 }

    { from TsgcWSServer_Indy_Base }
  protected
    function GetCustomServer: TIdCustomTCPServer; override;
    procedure OnServerExecuteEvent(AContext: TIdContext); override;
    procedure OnServerDisconnectEvent(AContext: TIdContext); override;
    { from TsgcWSServer_Indy_Base }

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  End;



  // TsgcWSServerHTTP //

  TsgcWSHTTPServer = class(TsgcWSServer_Indy_Base)
    { server http }
  protected
    FHTTPServer: TIdCustomHTTPServer;
  protected
    property HTTPServer: TIdCustomHTTPServer read FHTTPServer;
    { server http }

{$IFDEF SGC_HTTP2}
    { http/2 }
  private
  protected
    function DoHTTP2Authentication(const aConnection: TsgcWSConnection;
      const aRequest: TIdHTTPRequestInfo; const aHeaders: TStringList): Boolean;
  protected
    function DoHTTP2Request(const aConnection: TsgcWSConnection;
      const aRequest: TIdHTTPRequestInfo; const aHeaders: TStringList;
      const aBytes: TBytes): Boolean; virtual;
    procedure DoHTTP2Response(const aConnection: TsgcWSConnection;
      const aRequest: TIdHTTPRequestInfo; const aResponse: TIdHTTPResponseInfo;
      aStreamIdentifierRequest: Integer;
      aStreamIdentifierPush: Integer = 0); virtual;
  protected
    function DoHTTP2DocumentRoot(const aConnection: TsgcWSConnection;
      const aRequest: TIdHTTPRequestInfo; const aResponse: TIdHTTPResponseInfo)
      : Boolean; virtual;
  protected
    function DoHTTP2BeforeCommandGet(const aConnection: TsgcWSConnection;
      const aRequest: TIdHTTPRequestInfo; const aResponse: TIdHTTPResponseInfo;
      aStreamIdentifier: Integer): Boolean; virtual;
    procedure DoHTTP2CommandGet(const aConnection: TsgcWSConnection;
      const aRequest: TIdHTTPRequestInfo; const aResponse: TIdHTTPResponseInfo;
      aStreamIdentifierRequest, aStreamIdentifierPush: Integer); virtual;
  protected
    procedure DoHTTP2CommandOther(const aConnection: TsgcWSConnection;
      const aRequest: TIdHTTPRequestInfo; const aResponse: TIdHTTPResponseInfo;
      aStreamIdentifierRequest: Integer); virtual;
  protected
    procedure OnHTTP2RequestEvent(const aConnection: TsgcWSConnection;
      const aHeaders: TStringList; const aBytes: TBytes;
      aStreamIdentifierRequest, aStreamIdentifierPush: Integer;
      const aPostStream: TStream); override;
    procedure OnHTTP2ReadCreatePostStreamEvent(Sender: TObject;
      const aHeaders: TStrings; var Stream: TStream); override;
    procedure OnHTTP2ReadDonePostStreamEvent(Sender: TObject;
      const aHeaders: TStrings; const aStream: TStream); override;
    { push promise }
  private
    FPushPromiseList: TsgcHTTP2PushPromiseList;
  protected
    function GetPushPromiseList: TsgcHTTP2PushPromiseList;
  public
    procedure PushPromiseAddPreLoadLinks(const aPathMatch: String;
      const aLinks: TStrings);
    procedure PushPromiseRemovePreLoadLinks(const aPathMatch: String);
    { push promise }

    { http/2 }
{$ENDIF}
    { events http }
  private
    FCharset: string;
    FDocumentRoot: String;
    FOnCreateSession: {$IFDEF INDY10_5_7} TIdHTTPCreateSession
{$ELSE} TOnCreateSession {$ENDIF};
    FOnInvalidSession: TIdHTTPInvalidSessionEvent;
    FOnSessionEnd: {$IFDEF INDY10_5_7} TIdHTTPSessionEndEvent
{$ELSE} TOnSessionEndEvent {$ENDIF};
    FOnSessionStart: {$IFDEF INDY10_5_7} TIdHTTPSessionStartEvent
{$ELSE} TOnSessionStartEvent {$ENDIF};
    FReadEmptySource: Integer;
    FReadStartSSL: Integer;
  protected
    FOnBeforeForwardHTTP: TsgcWSOnBeforeForwardHTTP;
    FOnAfterForwardHTTP: TsgcWSOnAfterForwardHTTP;
    FOnCommandGet: TIdHTTPCommandEvent;
    FOnCommandOther: TIdHTTPCommandEvent;
    function DoBeforeOnCommand(aConnection: TsgcWSConnection;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo)
      : Boolean; virtual;
  protected
    procedure OnCommandGetEvent(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); virtual;
    procedure OnCommandOtherEvent(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); virtual;
    procedure OnCreateSessionEvent(ASender: TIdContext;
      var VHTTPSession: TIdHTTPSession); virtual;
    procedure OnInvalidSessionEvent(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo;
      var VContinueProcessing: Boolean;
      const AInvalidSessionID: string); virtual;
    procedure OnSessionStartEvent(Sender: TIdHTTPSession); virtual;
    procedure OnSessionEndEvent(Sender: TIdHTTPSession); virtual;
    procedure OnExceptionEvent(AContext: TIdContext;
      AException: Exception); virtual;
    procedure OnServerParseAuthenticationEvent(AContext: TIdContext;
      const AAuthType, AAuthData: String; var VUsername, VPassword: String;
      var VHandled: Boolean); virtual;
    procedure OnCreatePostStreamEvent(AContext: TIdContext;
      aHeaders: TIdHeaderList; var VPostStream: TStream); virtual;
    procedure OnDoneWithPostStreamEvent(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; var VCanFree: Boolean); virtual;
  protected
    function DoResponseHTTP(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo)
      : Boolean; virtual;
    procedure DoResponseAfterCommand(aConnection: TsgcWSConnectionServer;
      AResponseInfo: TIdHTTPResponseInfo); virtual;
  protected
    function GetCustomHeadersFromRaw(aRawHeaders: TIdHeaderList): string;
    function DoForwardHTTP(const aConnection: TsgcWSConnection;
      ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo)
      : Boolean; virtual;
  public
    property OnCommandGet: TIdHTTPCommandEvent read FOnCommandGet
      write FOnCommandGet;
    property OnCommandOther: TIdHTTPCommandEvent read FOnCommandOther
      write FOnCommandOther;
    property OnCreateSession: {$IFDEF INDY10_5_7} TIdHTTPCreateSession
{$ELSE} TOnCreateSession
{$ENDIF} read FOnCreateSession write FOnCreateSession;
    property OnInvalidSession: TIdHTTPInvalidSessionEvent read FOnInvalidSession
      write FOnInvalidSession;
    property OnSessionEnd: {$IFDEF INDY10_5_7} TIdHTTPSessionEndEvent
{$ELSE} TOnSessionEndEvent {$ENDIF} read FOnSessionEnd write FOnSessionEnd;
    property OnSessionStart: {$IFDEF INDY10_5_7} TIdHTTPSessionStartEvent
{$ELSE} TOnSessionStartEvent
{$ENDIF} read FOnSessionStart write FOnSessionStart;
    property OnBeforeForwardHTTP: TsgcWSOnBeforeForwardHTTP
      read FOnBeforeForwardHTTP write FOnBeforeForwardHTTP;
    property OnAfterForwardHTTP: TsgcWSOnAfterForwardHTTP
      read FOnAfterForwardHTTP write FOnAfterForwardHTTP;

    { events http }

    { properties http }
  private
    function GetAutoStartSession: Boolean;
    function GetDocumentRoot: String;
    function GetKeepAlive: Boolean;
    function GetParseParams: Boolean;
    function GetSessionList: TIdHTTPCustomSessionList;
    function GetSessionState: Boolean;
    function GetSessionTimeOut: Integer;
    procedure SetAutoStartSession(const Value: Boolean);
    procedure SetKeepAlive(const Value: Boolean);
    procedure SetParseParams(const Value: Boolean);
    procedure SetSessionState(const Value: Boolean);
    procedure SetSessionTimeOut(const Value: Integer);
  public
    property AutoStartSession: Boolean read GetAutoStartSession
      write SetAutoStartSession;
    property Charset: string read FCharset write FCharset;
    property DocumentRoot: String read GetDocumentRoot write FDocumentRoot;
    property KeepAlive: Boolean read GetKeepAlive write SetKeepAlive;
    property ParseParams: Boolean read GetParseParams write SetParseParams;
    property ReadEmptySource: Integer read FReadEmptySource
      write FReadEmptySource;
    property ReadStartSSL: Integer read FReadStartSSL write FReadStartSSL;
    property SessionList: TIdHTTPCustomSessionList read GetSessionList;
    property SessionState: Boolean read GetSessionState write SetSessionState;
    property SessionTimeOut: Integer read GetSessionTimeOut
      write SetSessionTimeOut;
    { properties http }

    { from TsgcWSServer_Indy_Base }
  protected
    procedure DoExecuteHTTP(AContext: TIdContext); virtual;
    function GetCustomServer: TIdCustomTCPServer; override;
    procedure OnServerExecuteEvent(AContext: TIdContext); override;
    procedure OnServerDisconnectEvent(AContext: TIdContext); override;
    { from TsgcWSServer_Indy_Base }

    { auth basic }
  protected
    function DoHTTPBasicAuthentication(aConnection: TsgcWSConnection;
      var ARequestInfo: TIdHTTPRequestInfo;
      var AResponseInfo: TIdHTTPResponseInfo): Boolean; virtual;
    { auth basic }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  End;

implementation

uses
  DateUtils,
{$IFDEF POSIX}Posix.Unistd, {$ENDIF}
  // sgc
{$IFDEF SGC_SCHANNEL}sgcSSL_SChannel_Indy, {$ENDIF}
{$IFDEF SGC_INDY_IOCP}sgcIndy_IOHandler_IOCP_Server_OpenSSL, {$ENDIF}
{$IFDEF SGC_INDY}sgcIdSSLOpenSSLHeaders, {$ELSE}IdSSLOpenSSLHeaders, {$ENDIF}
{$IFDEF SGC_INDY}sgcIdStack, {$ELSE}IdStack, {$ENDIF}
{$IFDEF SGC_INDY}sgcIdStackConsts, {$ELSE}IdStackConsts, {$ENDIF}
{$IFDEF SGC_INDY}sgcIdURI{$ELSE}IdURI{$ENDIF},
{$IFDEF SGC_INDY}sgcIdResourceStringsProtocols{$ELSE}IdResourceStringsProtocols{$ENDIF},
{$IFDEF SGC_INDY}sgcIdHTTPHeaderInfo{$ELSE}IdHTTPHeaderInfo{$ENDIF},
  sgcBase_Const, sgcWebSocket_Client, sgcBase_Classes,
  sgcWebSocket_HTTPResponse, sgcWebSocket_Const,
  sgcWebSocket_Protocol_Base_Server, sgcWebSocket_Resources,
  sgcWebSocket_LoadBalancer_Message, sgcSocket_Const, sgcHTTP_Helpers,
  sgcHTTP_Client, sgcHTTP_MultipartFormData;

const
  CS_SESSION_ID = 1;
  CS_MAX_READ_BUFFER_SIZE = 1024000;

type
  TsgcWSProtocol_Server_Hack = class(TsgcWSProtocol_Server)
  end;

  TsgcIdSSLContext_Hack = class(TIdSSLContext)
  end;

  TIdHTTPRequestInfo_Hack = class(TIdHTTPRequestInfo)
  end;

  TIdCustomHTTPServer_Hack = class(TIdCustomHTTPServer)
  end;

  TsgcWSClient_Hack = class(TsgcWSClient)
  end;

  TIdEntityHeaderInfo_Hack = class(TIdEntityHeaderInfo)

  end;

{$IFNDEF DXE2}

var
  SSL_CTX_ctrl: function(SSL: PSSL_CTX; cmd: Integer; larg: Integer;
    parg: Pointer): Integer cdecl = nil;
{$ENDIF}
  { TsgcWSHandShakeServer }

constructor TsgcWSHandShakeServer.Create(aConnection: TIdTCPConnection;
  aHeaders: TIdHeaderList);
begin
  DoDecodeHeaders(aConnection, aHeaders);
end;

procedure TsgcWSHandShakeServer.DoDecodeHeaders(const aConnection
  : TIdTCPConnection; aHeaders: TIdHeaderList);
var
  i: Integer;
  vText: String;
  vBytes: TIdBytes;
begin
  for i := 0 to aHeaders.Count - 1 do
  begin
    vText := aHeaders[i];

    if sgcStartsText(vText, 'GET /') then
      DoDecodeGET(vText)
    else if sgcStartsText(vText, 'Upgrade: ') then
      DoDecodeUpgrade(vText)
    else if sgcStartsText(vText, 'Connection: ') then
      DoDecodeConnection(vText)
    else if sgcStartsText(vText, 'Host: ') then
      DoDecodeHost(vText)
    else if sgcStartsText(vText, 'Sec-WebSocket-Key: ') then
      DoDecodeKey(vText)
    else if sgcStartsText(vText, 'Origin: ') then
      DoDecodeOrigin(vText)
    else if sgcStartsText(vText, 'Sec-WebSocket-Protocol: ') then
      DoDecodeProtocols(vText)
    else if sgcStartsText(vText, 'Sec-WebSocket-Version: ') then
      DoDecodeVersion(vText)
    else if sgcStartsText(vText, 'Sec-WebSocket-Extensions: ') then
      DoDecodeExtensions(vText)
      // hixie
    else if sgcStartsText(vText, 'Sec-WebSocket-Key1: ') then
      DoDecodeKey1(vText)
    else if sgcStartsText(vText, 'Sec-WebSocket-Key2: ') then
      DoDecodeKey2(vText);
  end;

  if not aConnection.IOHandler.InputBufferIsEmpty then
  begin
    if (FKey1 <> '') and (FKey2 <> '') then
    begin
      aConnection.IOHandler.ReadBytes(vBytes, 8);
      FKey3 := {$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF}.
        ToHex(vBytes);
    end;
  end;

  // validate key
  if ((FIsKey = False) and (FIsKey1 = False) and (FIsKey2 = False)) then
    raise TsgcWSException.Create(S_ERROR_DECODING_WEBSOCKET_KEY);
end;

constructor TsgcWSConnectionServer.Create;
begin
  inherited;
  FCustomHeartBeat := False;
  FAuthenticationCloseCode := CS_CLOSE_NORMAL;
  FReadStartSSLCount := 0;
end;

destructor TsgcWSConnectionServer.Destroy;
begin
  sgcFree(FAuthentication);
  sgcFree(FHandshake);
  inherited;
end;

function TsgcWSConnectionServer.DoAuthenticationBasic(const aParams
  : string): Boolean;
var
  oParams: TsgcDelimitedStringList;
begin
  Result := False;

  oParams := TsgcDelimitedStringList.Create;
  Try
    oParams.Delimiter := ':';
    oParams.DelimitedText := aParams;
    if oParams.Count = 2 then
    begin
      Authentication.AuthType := authBasic;
      // ... read authentication
      Authentication.User := oParams[0];
      Authentication.Password := oParams[1];
      // ... request authentication
      DoOnAuthenticationEvent;
      // ... result
      Result := Authentication.Authenticated;
    end;
  Finally
    sgcFree(oParams);
  End;

end;

function TsgcWSConnectionServer.DoAuthenticationURL(const aParams
  : string): Boolean;
var
  oParams: TsgcDelimitedStringList;
begin
  Result := False;

  oParams := TsgcDelimitedStringList.Create;
  Try
    oParams.Delimiter := '/';
    oParams.DelimitedText := aParams;
    if oParams.Count > 5 then
    begin
      Authentication.AuthType := authURL;
      // ... read authentication
      Authentication.User := oParams[4];
      Authentication.Password := oParams[5];
      // ... request authentication
      DoOnAuthenticationEvent;
      // ... result
      Result := Authentication.Authenticated;
    end;
  Finally
    sgcFree(oParams);
  End;
end;

procedure TsgcWSConnectionServer.DoHandshake;
var
  i: Integer;
begin
  Assert(not Enabled);
  FHandshake := TsgcWSHandShakeServer.Create(TCPConnection, HeadersRequest);
  Specification := FHandshake.Specification;

  Protocol := DoGetProtocol;

  HeadersResponse.Clear;

  DoHandShakeSecurity;
  DoHandShakeExtensions;

  case Specification of
    spRFC6455:
      begin
        if Transport = trpUndefined then
          Transport := trpRFC6455;
        DoHandShake_RFC6455;
      end;
    spHixie76:
      begin
        if Transport = trpUndefined then
          Transport := trpHixie76;
        DoHandShake_Hixie76;
      end;
  end;

  if Assigned(FOnHandshake) then
    FOnHandshake(self, FHeadersResponse);

  TCPConnection.IOHandler.WriteBufferOpen;
  Try
    for i := 0 to HeadersResponse.Count - 1 do
      TCPConnection.IOHandler.WriteLn(HeadersResponse[i]);
    TCPConnection.IOHandler.WriteLn;
    if FHandshake.Specification = spHixie76 then
      TCPConnection.IOHandler.write(FHandshake.GetWSAccept(FHandshake.FKey1,
        FHandshake.FKey2, FHandshake.FKey3));

    TCPConnection.IOHandler.WriteBufferFlush;
  Finally
    TCPConnection.IOHandler.WriteBufferClose;
  End;

  FEnabled := True;
  if Assigned(FOnUpdate) then
    FOnUpdate(self, 1);

  if FAuthenticationCloseCode <> CS_CLOSE_NORMAL then
    Close(FAuthenticationCloseCode);
end;

function TsgcWSConnectionServer.DoGetProtocol: string;
var
  i: Integer;
  oSProtocols, oCProtocols: TsgcDelimitedStringList;
begin
  if FHandshake.Protocols <> '' then
  begin
    oSProtocols := TsgcDelimitedStringList.Create;
    oCProtocols := TsgcDelimitedStringList.Create;
    Try
      oSProtocols.DelimitedText := Protocol;

      oCProtocols.DelimitedText := FHandshake.Protocols;

      for i := 0 to oCProtocols.Count - 1 do
      begin
        if oSProtocols.IndexOf(oCProtocols[i]) > -1 then
        begin
          Result := oCProtocols[i];
          break;
        end;
      end;

      if Result = '' then
        raise TsgcWSException.CreateFmt(S_PROTOCOL_UNSUPPORTED,
          [oCProtocols.Text]);

    Finally
      sgcFree(oCProtocols);
      sgcFree(oSProtocols);
    End;
  end;
end;

procedure TsgcWSConnectionServer.DoHandShakeExtensions;
begin
  Extensions.DecodeExtensions(FHandshake.Extensions);
end;

procedure TsgcWSConnectionServer.DoHandShakeSecurity;
begin
  if ServerOriginsAllowed <> '' then
    DoOriginsAllowed;
end;

procedure TsgcWSConnectionServer.DoHandShake_Hixie76;
begin
  HeadersResponse.Add('HTTP/1.1 101 WebSocket Protocol Handshake');
  HeadersResponse.Add('Upgrade: WebSocket');
  HeadersResponse.Add('Connection: Upgrade');
  HeadersResponse.Add('Sec-WebSocket-Origin: ' + FHandshake.Origin);
  HeadersResponse.Add('Sec-WebSocket-Location: ws://' + FHandshake.Host +
    FHandshake.Get);
  if Protocol <> '' then
    HeadersResponse.Add('Sec-WebSocket-Protocol: ' + Protocol)
  else
    HeadersResponse.Add('Sec-WebSocket-Protocol: *');
  HeadersResponse.Add('Server: ' + CS_APPLICATION_NAME + ' ' + CS_VERSION);
end;

procedure TsgcWSConnectionServer.DoHandShake_RFC6455;
begin
  HeadersResponse.Add('HTTP/1.1 101 Switching Protocols');
  HeadersResponse.Add('Upgrade: websocket');
  HeadersResponse.Add('Connection: Upgrade');
  if Protocol <> '' then
    HeadersResponse.Add('Sec-WebSocket-Protocol: ' + Protocol);
  HeadersResponse.Add('Sec-WebSocket-Accept: ' + FHandshake.GetWSAccept
    (FHandshake.Key));
  HeadersResponse.Add('Server: ' + CS_APPLICATION_NAME + ' ' + CS_VERSION);
  // ... extensions
  Extensions.WriteHeader(HeadersResponse);
end;

procedure TsgcWSConnectionServer.DoAuthSessionResponse(const aParams: String);
var
  oParams: TsgcDelimitedStringList;
begin
  oParams := TsgcDelimitedStringList.Create;
  Try
    oParams.Delimiter := '/';
    oParams.DelimitedText := aParams;
    if oParams.Count > 5 then
    begin
      Authentication.AuthType := authSession;
      // ... read authentication
      Authentication.User := oParams[5];
      if oParams.Count > 6 then
        Authentication.Password := oParams[6];
      // ... request authentication
      DoOnAuthenticationEvent;
      // ... authentication response
      if Authentication.Authenticated then
        DoHTTPResponse(Trim(Authentication.SessionID) + #13#10 +
          Trim(Extensions.List.Text) + #13#10 + Trim(Protocol) + #13#10,
          CS_TEXT_PLAIN)
      else
        DoHTTPError;
    end;
  Finally
    sgcFree(oParams);
  End;
end;

function TsgcWSConnectionServer.DoHTTPFileResponse(const aFileName,
  aContentType: String; aDisconnect: Boolean;
  aStreamIdentifier: Integer = 0): Boolean;
var
{$IFDEF D2006}
  oResponse: TsgcWSHTTPResponse;
{$ENDIF}
  vText: String;
  vLocalIP: String;
begin
  Result := False;

  // ... RequestURL file content
{$IFDEF D2006}
  oResponse := TsgcWSHTTPResponse.Create;
  Try
    vText := oResponse.GetResponseMessage(aFileName);
  Finally
    sgcFree(oResponse);
  End;
{$ELSE}
  vText := TsgcWSHTTPResponse.GetResponseMessage(aFileName);
{$ENDIF}
  // ... replace sys vars
  vLocalIP := LocalIP;
  if IPVersion = Id_IPv6 then
    vLocalIP := '[' + vLocalIP + ']';
  vText := StringReplace(vText, '{%host%}', vLocalIP,
    [rfReplaceAll, rfIgnoreCase]);
  vText := StringReplace(vText, '{%port%}', IntToStr(LocalPort),
    [rfReplaceAll, rfIgnoreCase]);
  if ServerSSL then
    vText := StringReplace(vText, '{%ssl%}', 's', [rfReplaceAll, rfIgnoreCase])
  else
    vText := StringReplace(vText, '{%ssl%}', '', [rfReplaceAll, rfIgnoreCase]);

  // ... send http response
  if vText <> '' then
  begin
    DoHTTPResponse(vText, aContentType, nil, aStreamIdentifier);
    if Transport = trpHTTP then
    begin
      TCPConnection.OnDisconnected := nil;
      TCPConnection.Disconnect(False); // close if RequestURL response
    end;
    Result := True;
  end
  else if aDisconnect then // send error if must disconnect
  begin
    DoHTTPError;
    if Transport = trpHTTP then
    begin
      TCPConnection.OnDisconnected := nil;
      TCPConnection.Disconnect(False);
    end;
    Result := True;
  end;
end;

procedure TsgcWSConnectionServer.DoOnAuthenticationEvent;
var
  vAuthenticated: Boolean;
begin
  vAuthenticated := Authentication.Authenticated;
  if Assigned(FOnAuthentication) then
  begin
    FOnAuthentication(self, Authentication.User, Authentication.Password,
      vAuthenticated);
    Authentication.Authenticated := vAuthenticated;
  end;
end;

procedure TsgcWSConnectionServer.DoOriginsAllowed;
var
  oList: TsgcDelimitedStringList;
  i: Integer;
begin
  if FHandshake.Origin <> '' then
  begin
    // ... find origin if allowed
    oList := TsgcDelimitedStringList.Create;
    Try
      oList.DelimitedText := ServerOriginsAllowed;
      for i := 0 to oList.Count - 1 do
      begin
        if sgcMatchesMask(FHandshake.Origin, oList[i]) then
          exit;
      end;
    Finally
      sgcFree(oList);
    End;

    // ... if not allowed raise exception
    raise TsgcWSException.CreateFmt(S_ORIGIN_NOT_ALLOWED, [FHandshake.Origin]);
  end;
end;

procedure TsgcWSConnectionServer.DoProtocolSSE;
var
  i: Integer;
  oList: TsgcDelimitedStringList;
begin
  oList := TsgcDelimitedStringList.Create;
  Try
    oList.DelimitedText := Protocol;
    for i := 0 to oList.Count - 1 do
    begin
      if sgcMatchesMask(URL, '*' + oList[i] + '*') then
      begin
        Protocol := oList[i];
        exit;
      end;
    end;
    Protocol := '';
  Finally
    sgcFree(oList);
  End;
end;

function TsgcWSConnectionServer.GetAuthentication
  : TsgcWSConnectionAuthentication;
begin
  if not Assigned(FAuthentication) then
    FAuthentication := TsgcWSConnectionAuthentication.Create;
  Result := FAuthentication;
end;

function TsgcWSConnectionServer.GetURL: String;
begin
  if FURL = '' then
  begin
    if Assigned(HeadersRequest) then
    begin
      FURL := DecodeGETFullPath(HeadersRequest);
      if FURL = '' then
        FURL := DecodePOSTFullPath(HeadersRequest);
    end;
  end;
  Result := FURL;
end;

procedure TsgcWSConnectionServer.SendResponseHTTP(const aContent,
  aContentType: String);
begin
  DoHTTPResponse(aContent, aContentType);
end;

procedure TsgcWSConnectionServer.SetAuthenticationCloseCode(const aCloseCode
  : Integer);
begin
  FAuthenticationCloseCode := aCloseCode;
end;

procedure TsgcWSConnectionServer.SetTransport(const Value: TwsTransport);
begin
  inherited;
{$IFDEF SGC_INDY_IOCP}
  if Value in [trpRFC6455, trpHixie76, trpFlash, trpTCP, trpHTTP2] then
  begin
    if (TCPConnection.IOHandler.ClassType = TsgcIndy_IOHandler_IOCP) then
    begin
      TsgcIndy_IOHandler_IOCP(TCPConnection.IOHandler).WriteAsync := False;
      TsgcIndy_IOHandler_IOCP(TCPConnection.IOHandler).ReadAsync := True;
    end
    else if (TCPConnection.IOHandler.ClassType = TsgcIndy_IOHandler_IOCP_OpenSSL)
    then
    begin
      TsgcIndy_IOHandler_IOCP_OpenSSL(TCPConnection.IOHandler)
        .WriteAsync := False;
      TsgcIndy_IOHandler_IOCP_OpenSSL(TCPConnection.IOHandler)
        .ReadAsync := True;
    end;
  end;
{$ENDIF}
end;

constructor TsgcWSServer_Indy_Base.Create(aOwner: TComponent);
begin
  inherited;
  FIOHandlerOptions := TsgcWSIOHandler_Options.Create;
  Port := CS_DEFAULT_PORT;
  FOptions := TsgcWSOptionsServer.Create;
  FSecurityOptions := TsgcWSSecurity_Options.Create;
  FSSLOptions := TsgcWSSSL_Options.Create;
  FThreadPoolOptions := TsgcWSThreadPool_Options.Create;
  FThreadPoolOptions.PoolSize := 32;
  FAuthentication := TsgcWSAuthenticationServer_Options.Create;
  FAuthentication.URL.Enabled := True;
  FAuthentication.Session.Enabled := True;
  FAuthentication.Basic.Enabled := False;
  FFallBack := TsgcWSFallBack_Options.Create;
  FFallBack.Flash.Enabled := False;
  FFallBack.ServerSentEvents.Enabled := False;
  FLoadBalancer := TsgcWSLoadBalancerServer_Options.Create;
  FLoadBalancer.Enabled := False;
  Extensions.Mode := appServer;
  FQueueOptions := TsgcWSQueueServer_Options.Create;
  FQueueOptions.Text.Level := qmNone;
  FQueueOptions.Binary.Level := qmNone;
  FQueueOptions.Ping.Level := qmNone;
  FHTTP2Options := TsgcWSHTTP2Server_Options.Create;
  FHTTP2Options.Enabled := False;
  FMaxReadBufferSize := CS_MAX_READ_BUFFER_SIZE;
  FHTTPUploadFiles := TsgcHTTPUploadFilesServer.Create;
end;

{ TsgcWSServer_Indy_Base }

procedure TsgcWSServer_Indy_Base.OnServerMessageEvent
  (aConnection: TsgcWSConnection; const Text: string);
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

destructor TsgcWSServer_Indy_Base.Destroy;
begin
  DoClear;
  sgcFree(FHTTP2Options);
  sgcFree(FIOHandlerOptions);
  sgcFree(FLoadBalancer);
  sgcFree(FFallBack);
  sgcFree(FAuthentication);
  sgcFree(FSessionIDList);
  sgcFree(FThreadPoolOptions);
  sgcFree(FSSLOptions);
  sgcFree(FSecurityOptions);
  sgcFree(FOptions);
  sgcFree(FQueueOptions);
  sgcFree(FThrottle);
  sgcFree(FHTTPUploadFiles);
  inherited;
end;

procedure TsgcWSServer_Indy_Base.Broadcast(aStream: TStream;
  const aChannel: string = ''; const aProtocol: string = '';
  const Exclude: String = ''; const Include: String = '';
  const aSize: Integer = 0; const aStreaming: TwsStreaming = stmNone);
begin
  if LoadBalancer.Enabled then
    ClientLoadBalancer.Broadcast(aStream, aChannel, aProtocol, Exclude, Include,
      aSize, aStreaming)
  else
    DoBroadCast('', aStream, opBinary, aChannel, aProtocol, Exclude, Include,
      aSize, aStreaming);
end;

procedure TsgcWSServer_Indy_Base.Broadcast(const aMessage: string;
  const aChannel: string = ''; const aProtocol: string = '';
  const Exclude: String = ''; const Include: String = '');
begin
  if LoadBalancer.Enabled then
    ClientLoadBalancer.Broadcast(aMessage, aChannel, aProtocol,
      Exclude, Include)
  else
    DoBroadCast(aMessage, nil, opText, aChannel, aProtocol, Exclude, Include);
end;

procedure TsgcWSServer_Indy_Base.DisconnectAll;
var
  i: Integer;
  oConnection: TsgcWSConnectionServer;
  oList: TList{$IFDEF NEXTGEN}<TIdContext>{$ENDIF};
begin
  oList := LockList;
  Try
    for i := oList.Count - 1 downto 0 do
    begin
      oConnection := TsgcWSConnectionServer(TIdContext(oList[i])
        .{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF});
      if Assigned(oConnection) then
      begin
        if oConnection.Active and not oConnection.Disconnected then
        begin
          // if server is IOCP, Connection Closed is gracefully error
          // is shown to user
          Try
            oConnection.DisconnectPeer;
          Except
            On E: Exception do
              DoException(oConnection, E.Message, E);
          End;
        end;
      end;
    end;
  Finally
    UnLockList;
  end;
end;

procedure TsgcWSServer_Indy_Base.DoAddSessionID(const aSessionID, aUser,
  aPassword: String);
begin
  DoEnterCS(CS_SESSION_ID);
  Try
    if GetSessionIDList.IndexOfName(aSessionID) = -1 then
      GetSessionIDList.Add(aSessionID + '=' + aUser + ':' + aPassword);
  Finally
    DoLeaveCS(CS_SESSION_ID);
  End;
end;

function TsgcWSServer_Indy_Base.DoAuthenticationSession
  (aConnection: TsgcWSConnectionServer; const aParams: String): Boolean;
var
  oParams: TsgcDelimitedStringList;
  vSessionID: String;
  vUser, VPassword: String;
begin
  Result := False;

  oParams := TsgcDelimitedStringList.Create;
  Try
    oParams.Delimiter := '/';
    oParams.DelimitedText := aParams;
    if oParams.Count > 4 then
    begin
      // ... read authentication
      vSessionID := oParams[4];
      if vSessionID <> '' then
        Result := GetSessionID(vSessionID);
      if Result then
      begin
        // ... assign values to session fields
        aConnection.Authentication.SessionID := vSessionID;
        GetSessionUserPassword(vSessionID, vUser, VPassword);
        aConnection.Authentication.User := vUser;
        aConnection.Authentication.Password := VPassword;
        // ... delete session id
        DoDelSessionID(vSessionID);
      end;
    end;
  Finally
    sgcFree(oParams);
  End;
end;

procedure TsgcWSServer_Indy_Base.DoBroadCast(const aMessage: string;
  aStream: TStream; aOpCode: TOpCode; const aChannel: String = '';
  const aProtocol: string = ''; const Exclude: String = '';
  const Include: String = ''; const aSize: Integer = 0;
  const aStreaming: TwsStreaming = stmNone);

  procedure DoWriteDataConnection(const aConnection: TsgcWSConnection;
    const aMessage: string; aStream: TStream; aOpCode: TOpCode;
    const aChannel, aProtocol: String;
    const aExclude, aInclude: TsgcDelimitedStringList; const aSize: Integer;
    const aStreaming: TwsStreaming; aOptimized: Boolean);
  var
    vExclude, vInclude: Boolean;
    VHandled: Boolean;
  begin
    Try
      if Assigned(aConnection) then
      begin
        if aConnection.Enabled and ((aConnection.Disconnected = False) or
          (aOpCode = opPing)) then
        begin
          // ... aExclude
          if Assigned(aExclude) then
            vExclude := aExclude.IndexOf(aConnection.Guid) <> -1
          else
            vExclude := False;
          // ... aInclude
          if Assigned(aInclude) then
            vInclude := aInclude.IndexOf(aConnection.Guid) <> -1
          else
            vInclude := True;
          // ... broadcast
          if (vExclude = False) and (vInclude = True) then
          begin
            case aOpCode of
              opText:
                begin
                  if aConnection.Transport <> trpHTTP2 then
                  begin
                    if UpperCase(aConnection.Protocol) = UpperCase(aProtocol)
                    then
                    begin
                      if aOptimized or aConnection.Subscribed(aChannel) then
                        aConnection.WriteData(aMessage);
                    end;
                  end;
                end;
              opBinary:
                begin
                  if aConnection.Transport <> trpHTTP2 then
                  begin
                    if UpperCase(aConnection.Protocol) = UpperCase(aProtocol)
                    then
                    begin
                      if aOptimized or aConnection.Subscribed(aChannel) then
                        aConnection.WriteData(aStream, aSize, aStreaming);
                    end;
                  end;
                end;
              opPing:
                begin
                  VHandled := False;
                  if Assigned(FOnBeforeHeartBeat) then
                    FOnBeforeHeartBeat(self, aConnection, VHandled);
                  TsgcWSConnectionServer(aConnection).FCustomHeartBeat
                    := VHandled;
                  if VHandled = False then
                    aConnection.Ping(aMessage);
                end;
            end;
          end;
        end;
      end;
    Except
      On E: Exception do
        DoException(aConnection, E.Message, E);
    end;
  end;

var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TIdContext>{$ENDIF};
  oExclude: TsgcDelimitedStringList;
  oInclude: TsgcDelimitedStringList;
  oConnection: TsgcWSConnectionServer;
  oConnectionList: TList{$IFDEF NEXTGEN}<TsgcWSConnection>{$ENDIF};
begin
  oExclude := nil;
  oInclude := nil;

  // ... exclude list
  if Exclude <> '' then
  begin
    oExclude := TsgcDelimitedStringList.Create;
    oExclude.DelimitedText := Exclude;
  end;
  // ... include list
  if Include <> '' then
  begin
    oInclude := TsgcDelimitedStringList.Create;
    oInclude.DelimitedText := Include;
  end;

  // ... broadcast
  Try
    // ... optimization channel enabled
    if (aChannel <> '') and Optimizations.Channels.Enabled then
    begin
      oConnectionList := TList{$IFDEF NEXTGEN}<TsgcWSConnection>{$ENDIF}.Create;
      Try
        GetChannels.GetConnections(aChannel, oConnectionList);
        for i := oConnectionList.Count - 1 Downto 0 do
        begin
          oConnection := TsgcWSConnectionServer(oConnectionList[i]);
          DoWriteDataConnection(oConnection, aMessage, aStream, aOpCode,
            aChannel, aProtocol, oExclude, oInclude, aSize, aStreaming, True);
        end;
      Finally
        sgcFree(oConnectionList);
      End;
    end
    else
    // ... search indy connection list
    begin
      oList := LockList;
      Try
        for i := oList.Count - 1 Downto 0 do
        begin
          oConnection := TsgcWSConnectionServer(TIdContext(oList[i])
            .{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF});
          DoWriteDataConnection(oConnection, aMessage, aStream, aOpCode,
            aChannel, aProtocol, oExclude, oInclude, aSize, aStreaming, False);
        end;
      Finally
        UnLockList;
      End;
    end;
  Finally
    sgcFree(oExclude);
    sgcFree(oInclude);
  End;
end;

procedure TsgcWSServer_Indy_Base.DoDelSessionID(const aSessionID: String);
var
  vId: Integer;
begin
  DoEnterCS(CS_SESSION_ID);
  Try
    vId := GetSessionIDList.IndexOfName(aSessionID);
    if vId > -1 then
      GetSessionIDList.Delete(vId);
  Finally
    DoLeaveCS(CS_SESSION_ID);
  End;
end;

function TsgcWSServer_Indy_Base.DoBuiltInLibraries
  (aConnection: TsgcWSConnectionServer; const aText: String;
  aDisconnect: Boolean; aStreamIdentifier: Integer = 0): Boolean;
var
  oStream: TMemoryStream;
  vFileName: String;
begin
  Result := False;

  // ... javascript request
  if sgcMatchesMask(aText, 'GET *.js *') then
  begin
    if aConnection.Transport = trpUndefined then
      aConnection.Transport := trpHTTP;
    if Options.JavascriptFiles then
    begin
      vFileName := DecodeGETFileName(aText);
      if vFileName = CS_JS_SGCWEBSOCKETS then
      begin
        if FallBack.Flash.Enabled then
          vFileName := CS_JS_SGCWEBSOCKETSFLASH + ',' + vFileName;
        if FallBack.ServerSentEvents.Enabled then
          vFileName := CS_JS_SGCWEBSOCKETSEVENTSOURCE + ',' + vFileName;
      end;

      Result := aConnection.DoHTTPFileResponse(vFileName,
        'text/javascript; charset=utf-8', aDisconnect, aStreamIdentifier);
    end;
  end
  // ... html request
  else if sgcMatchesMask(aText, 'GET *.html *') then
  begin
    if aConnection.Transport = trpUndefined then
      aConnection.Transport := trpHTTP;
    if Options.HTMLFiles then
      Result := aConnection.DoHTTPFileResponse(DecodeGETFileName(aText),
        'text/html', aDisconnect, aStreamIdentifier);
  end
  // ... css request
  else if sgcMatchesMask(aText, 'GET *.css *') then
  begin
    if aConnection.Transport = trpUndefined then
      aConnection.Transport := trpHTTP;
    if Options.HTMLFiles then
      Result := aConnection.DoHTTPFileResponse(DecodeGETFileName(aText),
        'text/css', aDisconnect, aStreamIdentifier);
  end
  // ... flash MainWebSocket.swf
  else if sgcMatchesMask(aText, 'GET *' + CS_WEBSOCKETMAIN_SWF + ' *') then
  begin
    if aConnection.Transport = trpUndefined then
      aConnection.Transport := trpHTTP;
    oStream := TMemoryStream.Create;
    Try
      if GetResourceStream(CS_SGC_WEBSOCKETMAIN_SWF, oStream) then
        aConnection.DoHTTPResponse(oStream, 'application/x-shockwave-flash',
          nil, aStreamIdentifier);
    Finally
    // don't free stream, it's freed internally
    //  sgcFree(oStream);
    End;
    Result := True;
  end
  // ... favicon request
  else if sgcMatchesMask(aText, 'GET *favicon*') then
  begin
    if aConnection.Transport = trpUndefined then
      aConnection.Transport := trpHTTP;
    if aDisconnect then
      Result := True;
  end;
end;

procedure TsgcWSServer_Indy_Base.DoClear;
begin
  sgcThreadFree(FHeartBeatTimeoutTimer);
  sgcThreadFree(FHeartBeatTimer);
  sgcThreadFree(FConnectionsFreeTimer);
end;

procedure TsgcWSServer_Indy_Base.DoHTTPProtocol(const aConnection:
    TsgcWSConnectionServer; const aStream: TStream = nil; aStreamIdentifier:
    Integer = 0);
var
  i: Integer;
  vPath, vProtocol, vParams: String;
  oParams: TStringList;
  vLength: Int64;
  oStream: TsgcStringStream;
  vBody: string;
begin
  vParams := StringReplace(DecodePOSTFullPath(aConnection.HeadersRequest),
    CS_REQ_PROTOCOL, '', [rfReplaceAll]);

  oParams := TStringList.Create;
  Try
    oParams.Delimiter := '/';
    oParams.DelimitedText := vParams;
    if oParams.Count > 0 then
    begin
      vProtocol := oParams[0];
      for i := 1 to oParams.Count - 1 do
      begin
        if vPath = '' then
          vPath := oParams[i]
        else
          vPath := vPath + '/' + oParams[i];
      end;
      if vProtocol <> '' then
      begin
        for i := 0 to FProtocolObjectList.Count - 1 do
        begin
          if ProtocolRegistered(vProtocol,
            TsgcWSProtocol_Server(FProtocolObjectList[i])) then
          begin
            if aConnection.Transport = trpHTTP2 then
            begin
              if Assigned(aStream) then
              begin
                oStream := TsgcStringStream.Create('');
                Try
                  oStream.CopyFrom(aStream, aStream.Size);
                  vBody := oStream.DataString;
                Finally
                  sgcFree(oStream);
                End;
              end;
            end
            else
            begin
              vLength := sgcGetContentLength(aConnection.HeadersRequest);
              vBody := '';
              if vLength > 0 then
              begin
                oStream := TsgcStringStream.Create('');
                Try
                  aConnection.TCPConnection.IOHandler.ReadStream(oStream,
                    vLength);
                  vBody := oStream.DataString;
                Finally
                  sgcFree(oStream);
                End;
              end;
            end;
            aConnection.DoHTTPResponse
              (TsgcWSProtocol_Server_Hack(FProtocolObjectList[i])
              .GetHTTPResponse(aConnection, vPath, vBody), 'text/html', nil,
              aStreamIdentifier);
          end;
        end;
      end;
    end;
  Finally
    sgcFree(oParams);
  End;
end;

procedure TsgcWSServer_Indy_Base.DoIOHandler;
{$IFDEF SGC_INDY_IOCP}
var
  i: Integer;
  oBinding: TIdSocketHandle;
{$ENDIF}
begin
{$IFNDEF SGC_INDY_IOCP}
  exit;
{$ENDIF}
  if SSL then
    exit;

  case IOHandlerOptions.IOHandlerType of
    iohDefault:
      begin
        if Assigned(CustomServer.IOHandler) then
        begin
{$IFNDEF NEXTGEN}
          CustomServer.IOHandler.Free;
{$ENDIF}
          CustomServer.IOHandler := nil;
        end;
      end;
    iohIOCP:
      begin
{$IFDEF SGC_INDY_IOCP}
        // ... free if needed
        if Assigned(CustomServer.IOHandler) then
        begin
          if CustomServer.IOHandler.ClassType <> TsgcIdServerIOHandlerIOCP then
          begin
            CustomServer.IOHandler.Free;
            CustomServer.IOHandler := nil;
          end
          else
            exit;
        end;
        CustomServer.IOHandler := TsgcIdServerIOHandlerIOCP.Create(self);
        TsgcIdServerIOHandlerIOCP(CustomServer.IOHandler).IOCPThreads :=
          IOHandlerOptions.IOCP.IOCPThreads;
        TsgcIdServerIOHandlerIOCP(CustomServer.IOHandler).WorkOpThreads :=
          IOHandlerOptions.IOCP.WorkOpThreads;
        TsgcIdServerIOHandlerIOCP(CustomServer.IOHandler).TimeOut :=
          IOHandlerOptions.IOCP.TimeOut;

        // ... set overlapped = true
        if CustomServer.Bindings.Count = 0 then
        begin
          oBinding := CustomServer.Bindings.Add;
          case oBinding.IPVersion of
            Id_IPv4:
              begin
                if GStack.SupportsIPv6 then
                  Bindings.Add.IPVersion := Id_IPv6;
              end;
            Id_IPv6:
              begin
{$IFDEF SGC_INDY_LIB}
                if GStack.SupportsIPv4 then
{$ENDIF}
                  Bindings.Add.IPVersion := Id_IPv4;
              end;
          end;
        end;

        for i := 0 to CustomServer.Bindings.Count - 1 do
          CustomServer.Bindings.Items[i].OverLapped := True;
{$ENDIF}
      end;
  end;
end;

procedure TsgcWSServer_Indy_Base.DoLoadBalancerSendActiveConnections;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TIdContext>{$ENDIF};
  oConnection: TsgcWSConnection;
begin
  oConnection := nil;

  oList := LockList;
  Try
    for i := 0 to oList.Count - 1 do
    begin
      Try
        oConnection := TsgcWSConnection(TIdContext(oList[i])
          .{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF});
        if Assigned(oConnection) then
          ClientLoadBalancer.WriteData
            (GetLoadBalancerConnectionMessage(oConnection, True));
      Except
        On E: Exception do
          DoException(oConnection, E.Message, E);
      end;
    end;
  Finally
    UnLockList;
  End;
end;

procedure TsgcWSServer_Indy_Base.DoLoadBalancerSendServerBindings;
var
  i: Integer;
  oBinding: TsgcWSLoadBalancerServerBinding;
  vHost: String;
  vPort: Integer;
  vSSL: Boolean;
begin
  // ... AutoRegisterBindings
  if LoadBalancer.AutoRegisterBindings then
  begin
    for i := 0 to Bindings.Count - 1 do
    begin
      oBinding := TsgcWSLoadBalancerServerBinding.Create(nil);
      Try
        // ... read bindings
        if SSLOptions.Port = Bindings[i].Port then
          oBinding.SSL := True
        else
          oBinding.SSL := False;
        oBinding.Host := Bindings[i].IP;
        oBinding.Port := Bindings[i].Port;

        // ... send binding to load balancer
        ClientLoadBalancer.WriteData(oBinding.write);
      Finally
        sgcFree(oBinding);
      End;
    end;
  end;

  // ... send bindings defined manually
  for i := 0 to LoadBalancer.Bindings.Count - 1 do
  begin
    if DecodeWSSURL(LoadBalancer.Bindings[i], vHost, vPort, vSSL) then
    begin
      oBinding := TsgcWSLoadBalancerServerBinding.Create(nil);
      Try
        // ... read bindings
        oBinding.SSL := vSSL;
        oBinding.Host := vHost;
        oBinding.Port := vPort;

        // ... send binding to load balancer
        ClientLoadBalancer.WriteData(oBinding.write);
      Finally
        sgcFree(oBinding);
      End;
    end;
  end;
end;

procedure TsgcWSServer_Indy_Base.DoLoadBalancerSendServerData;
var
  oData: TsgcWSLoadBalancerServerData;
begin
  oData := TsgcWSLoadBalancerServerData.Create(nil);
  Try
    oData.Guid := LoadBalancer.Guid;
    ClientLoadBalancer.WriteData(oData.write);
  Finally
    sgcFree(oData);
  End;
end;

procedure TsgcWSServer_Indy_Base.DoLoadBalancerSendServerReady;
begin
  ClientLoadBalancer.WriteData(CS_LB_SERVER_READY);
end;

procedure TsgcWSServer_Indy_Base.DoNotifyConnect(aConnection: TsgcWSConnection);
begin
  inherited;
  if LoadBalancer.Enabled then
    ClientLoadBalancer.WriteData(GetLoadBalancerConnectionMessage
      (aConnection, True));
end;

procedure TsgcWSServer_Indy_Base.DoNotifyDisconnect
  (aConnection: TsgcWSConnection);
begin
  if LoadBalancer.Enabled then
    ClientLoadBalancer.WriteData(GetLoadBalancerConnectionMessage
      (aConnection, False));
  inherited;
end;

procedure TsgcWSServer_Indy_Base.DoOpenSSLECDHE;
{$IFNDEF SGC_INDY_LIB}
{$IFDEF MSWINDOWS}
var
  oModule: HMODULE;
{$ENDIF}
{$ENDIF}
begin
  if SSL then
  begin
    if Assigned(FHandlerSSL) then
    begin
      if (SSLOptions.OpenSSL_Options.APIVersion = oslAPI_1_0) and
        SSLOptions.OpenSSL_Options.ECDHE then
      begin
{$IFDEF SGC_INDY_LIB}
        SSL_CTX_set_ecdh_auto
          (TsgcIdSSLContext_Hack(TIdServerIOHandlerSSLOpenSSL(FHandlerSSL)
          .SSLContext).FContext, 1);;
{$ELSE}
{$IFDEF MSWINDOWS}
        oModule := SafeLoadLibrary('ssleay32.dll');
        if oModule <> 0 then
        begin
          @SSL_CTX_ctrl := GetProcAddress(oModule, PChar('SSL_CTX_ctrl'));
          if Assigned(@SSL_CTX_ctrl) then
            SSL_CTX_ctrl(TsgcIdSSLContext_Hack(TIdServerIOHandlerSSLOpenSSL
              (FHandlerSSL).SSLContext).FContext, 94, 1, nil);
        end;
{$ENDIF}
{$ENDIF}
      end;
    end;
  end;
end;

procedure TsgcWSServer_Indy_Base.DoPing(const aText: string = '');
var
  vPing: string;
begin
  vPing := aText;
  if vPing = '' then
    vPing := FormatDateTime('yyyymmddhhnnsszzz', Now);

  DoBroadCast(vPing, nil, opPing);
end;

procedure TsgcWSServer_Indy_Base.DoScheduler;
begin
{$IFNDEF SGC_INDY_IOCP}
  exit;
{$ENDIF}
  case IOHandlerOptions.IOHandlerType of
    iohDefault:
      begin
        if Assigned(CustomServer.Scheduler) then
        begin
{$IFNDEF NEXTGEN}
          CustomServer.Scheduler.Free;
{$ENDIF}
          CustomServer.Scheduler := nil;
        end
      end;
    iohIOCP:
      begin
{$IFDEF SGC_INDY_IOCP}
        if Assigned(CustomServer.Scheduler) then
        begin
          if CustomServer.Scheduler.ClassType <> TsgcIdSchedulerOfIOCP then
          begin
            CustomServer.Scheduler.Free;
            CustomServer.Scheduler := nil;
          end
          else
            exit;
        end;
        CustomServer.Scheduler := TsgcIdSchedulerOfIOCP.Create(self);
{$ENDIF}
      end;
  end;

end;

procedure TsgcWSServer_Indy_Base.DoStart;
begin
  Start;
end;

procedure TsgcWSServer_Indy_Base.DoTCPDisconnect(aConnection
  : TsgcWSConnectionServer);
begin
  if Assigned(aConnection) then
  begin
    if Assigned(aConnection.TCPConnection) then
    begin
      aConnection.TCPConnection.OnDisconnected := nil;
      // enabled for IOCP, ignore OnDisconnected event
      aConnection.TCPConnection.Disconnect(False);
    end;
  end;
end;

function TsgcWSServer_Indy_Base.DoUnknownAuthentication
  (aConnection: TsgcWSConnectionServer): Boolean;
var
  vAuthType, vAuthData: String;
  vUser, VPassword: String;
begin
  Result := False;
  if Assigned(FOnUnknownAuthentication) then
  begin
    vAuthData := aConnection.HeadersRequest.Values['Authorization'];
    if Length(vAuthData) > 0 then
      vAuthType := Fetch(vAuthData, ' ');
    vUser := aConnection.Authentication.User;
    VPassword := aConnection.Authentication.Password;

    FOnUnknownAuthentication(aConnection, vAuthType, vAuthData, vUser,
      VPassword, Result);

    if Result then
    begin
      aConnection.Authentication.User := vUser;
      aConnection.Authentication.Password := VPassword;
    end;
  end;
end;

procedure TsgcWSServer_Indy_Base.GetHandlerSSLEvent(const aSSLType
  : TwsSSLHandler; var aSSLHandler: TIdServerIOHandlerSSLBase);
begin
  // ... customer can assign own handler
  if Assigned(FOnSSLGetHandler) then
    FOnSSLGetHandler(self, aSSLType, aSSLHandler);
  // ... if not customized, create by default
  if not Assigned(aSSLHandler) then
  begin
    case IOHandlerOptions.IOHandlerType of
      iohIOCP:
        begin
{$IFDEF SGC_INDY_IOCP}
          aSSLHandler := TsgcIdServerIOHandlerIOCPOpenSSL.Create(self);
          TsgcIdServerIOHandlerIOCPOpenSSL(FHandlerSSL).IOCPThreads :=
            IOHandlerOptions.IOCP.IOCPThreads;
          TsgcIdServerIOHandlerIOCPOpenSSL(FHandlerSSL).WorkOpThreads :=
            IOHandlerOptions.IOCP.WorkOpThreads;
          TsgcIdServerIOHandlerIOCPOpenSSL(FHandlerSSL).TimeOut :=
            IOHandlerOptions.IOCP.TimeOut;
{$ELSE}
          aSSLHandler := TIdServerIOHandlerSSLOpenSSL.Create(self);
{$ENDIF}
        end
    else
      aSSLHandler := TIdServerIOHandlerSSLOpenSSL.Create(self);
    end;

    TIdServerIOHandlerSSLOpenSSL(aSSLHandler).OnGetPassword :=
      OnGetPasswordSSLEvent;
    GetCustomServer.IOHandler := aSSLHandler;
{$IFDEF SGC_INDY_LIB}
    case SSLOptions.OpenSSL_Options.APIVersion of
      oslAPI_1_0:
        OPENSSL_API_VERSION := opSSL_1_0;
      oslAPI_1_1:
        OPENSSL_API_VERSION := opSSL_1_1;
      oslAPI_3_0:
        OPENSSL_API_VERSION := opSSL_3_0;
    end;
{$ENDIF}
    case SSLOptions.OpenSSL_Options.LibPath of
      oslpNone:
        ;
      oslpDefaultFolder:
        sgcIdOpenSSLSetLibPath(sgcGetOpenSSLDefaultFolder);
      oslpCustomPath:
        sgcIdOpenSSLSetLibPath(SSLOptions.OpenSSL_Options.LibPathCustom);
    end;
    {$IFDEF SGC_UNIX_SYMLINKS}
    case SSLOptions.OpenSSL_Options.UnixSymLinks of
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
    case SSLOptions.Version of
      tls1_0:
        TIdServerIOHandlerSSLOpenSSL(aSSLHandler).SSLOptions.Method :=
          sslvTLSv1;
{$IFDEF INDY10_6}
      tls1_1:
        TIdServerIOHandlerSSLOpenSSL(aSSLHandler).SSLOptions.Method :=
          sslvTLSv1_1;
      tls1_2:
        TIdServerIOHandlerSSLOpenSSL(aSSLHandler).SSLOptions.Method :=
          sslvTLSv1_2;
      tls1_3:
        TIdServerIOHandlerSSLOpenSSL(aSSLHandler).SSLOptions.Method :=
{$IFDEF SGC_INDY_LIB}sslvTLSv1_3{$ELSE}sslvTLSv1_2{$ENDIF};
{$ENDIF}
    else
      TIdServerIOHandlerSSLOpenSSL(aSSLHandler).SSLOptions.Method := sslvSSLv23;
    end;
    TIdServerIOHandlerSSLOpenSSL(aSSLHandler).SSLOptions.Mode := sslmServer;
    if SSLOptions.OpenSSL_Options.ECDHE then
      TIdServerIOHandlerSSLOpenSSL(FHandlerSSL).SSLOptions.CipherList :=
        'EECDH+AESGCM:EDH+AESGCM:AES256+EECDH:AES256+EDH';
    if SSLOptions.VerifyCertificate then
    begin
      TIdServerIOHandlerSSLOpenSSL(aSSLHandler).SSLOptions.VerifyMode :=
        [sslvrfPeer];
      if SSLOptions.VerifyDepth >= 0 then
        TIdServerIOHandlerSSLOpenSSL(aSSLHandler).SSLOptions.VerifyDepth :=
          SSLOptions.VerifyDepth;
    end
    else
      TIdServerIOHandlerSSLOpenSSL(aSSLHandler).SSLOptions.VerifyMode := [];
{$IFDEF SGC_INDY_LIB}
    TIdServerIOHandlerSSLOpenSSL(aSSLHandler).OnALPNSelect :=
      OnSSLALPNSelectEvent;
{$ENDIF}
    if Assigned(FOnSSLAfterCreateHandler) then
      FOnSSLAfterCreateHandler(self, aSSLType, aSSLHandler);
  end;
end;

function TsgcWSServer_Indy_Base.DoWriteData(const aGuid,
  aMessage: string): Boolean;
var
  oConnection: TsgcWSConnectionServer;
begin
  Result := False;

  oConnection := GetConnectionByGuid(aGuid);
  if Assigned(oConnection) then
  begin
    if oConnection.Enabled and (oConnection.Disconnected = False) then
    begin
      Try
        oConnection.WriteData(aMessage);
        Result := True;
      Except
        On E: Exception do
          DoError(oConnection, E.Message);
      end;
    end;
  end;
end;

function TsgcWSServer_Indy_Base.DoWriteData(const aGuid: string;
  const aStream: TStream; aSize: Integer = 0;
  const aStreaming: TwsStreaming = stmNone): Boolean;
var
  oConnection: TsgcWSConnectionServer;
begin
  Result := False;

  oConnection := GetConnectionByGuid(aGuid);
  if Assigned(oConnection) then
  begin
    if oConnection.Enabled and (oConnection.Disconnected = False) then
    begin
      Try
        oConnection.WriteData(aStream, aSize, aStreaming);
        Result := True;
      Except
        On E: Exception do
          DoError(oConnection, E.Message);
      end;
    end;
  end;
end;

procedure TsgcWSServer_Indy_Base.DoXHR(const aConnection:
    TsgcWSConnectionServer; const aStream: TStream = nil; aStreamIdentifier:
    Integer = 0);
var
  vGUID: string;
  vLength: Integer;
  oConnection: TsgcWSConnectionServer;
  oStream: TsgcStringStream;
  vBody: string;
begin
  vGUID := StringReplace(DecodePOSTFullPath(aConnection.HeadersRequest),
    CS_REQ_XHR, '', [rfReplaceAll]);
  if vGUID <> '' then
  begin
    oConnection := GetConnectionByGuid(vGUID);
    if Assigned(oConnection) then
    begin
      vBody := '';
      if aConnection.Transport = trpHTTP2 then
      begin
        if Assigned(aStream) then
        begin
          oStream := TsgcStringStream.Create('');
          Try
            oStream.CopyFrom(aStream, aStream.Size);
            vBody := oStream.DataString;
          Finally
            sgcFree(oStream);
          End;
        end;
      end
      else
      begin
        vLength := sgcGetContentLength(aConnection.HeadersRequest);
        if vLength > 0 then
        begin
          oStream := TsgcStringStream.Create('');
          Try
            aConnection.TCPConnection.IOHandler.ReadStream(oStream, vLength);
            vBody := oStream.DataString;
          Finally
            sgcFree(oStream);
          End;
        end;
      end;
      oConnection.DoMessageEvent(vBody);
      aConnection.DoHTTPResponse('', 'text/html', nil, aStreamIdentifier);
    end;
  end;
end;

function TsgcWSServer_Indy_Base.GetActive: Boolean;
begin
  if not IsDesigning and not IsLoading then
    Result := CustomServer.Active
  else
    Result := FActive;
end;

function TsgcWSServer_Indy_Base.GetSessionID(const aSessionID: String): Boolean;
begin
  Result := GetSessionIDList.IndexOfName(aSessionID) <> -1;
end;

function TsgcWSServer_Indy_Base.GetSessionIDList: TsgcDelimitedStringList;
begin
  if not Assigned(FSessionIDList) then
    FSessionIDList := TsgcDelimitedStringList.Create;
  Result := FSessionIDList;
end;

function TsgcWSServer_Indy_Base.GetBindings: TIdSocketHandles;
begin
  Result := CustomServer.Bindings;
end;

function TsgcWSServer_Indy_Base.GetClientLoadBalancer: TsgcWSLoadBalancerClient;
begin
  if not Assigned(FClientLoadBalancer) then
  begin
    FClientLoadBalancer := TsgcWSLoadBalancerClient.Create(self);
    FClientLoadBalancer.Host := LoadBalancer.Host;
    FClientLoadBalancer.Port := LoadBalancer.Port;
    if FLoadBalancer.AutoRestart > 0 then
    begin
      FClientLoadBalancer.WatchDog.Interval := FLoadBalancer.AutoRestart;
      FClientLoadBalancer.WatchDog.Enabled := True;
    end;
    // ... events
    FClientLoadBalancer.OnConnect := OnLoadBalancerConnectEvent;
    FClientLoadBalancer.OnWriteDataText := OnLoadBalancerWriteDataTextEvent;
    FClientLoadBalancer.OnWriteDataBinary := OnLoadBalancerWriteDataBinaryEvent;
    FClientLoadBalancer.OnBroadCastText := OnLoadBalancerBroadCastTextEvent;
    FClientLoadBalancer.OnBroadCastBinary := OnLoadBalancerBroadCastBinaryEvent;
    FClientLoadBalancer.OnDisconnect := OnLoadBalancerDisconnectEvent;
    FClientLoadBalancer.OnError := OnLoadBalancerErrorEvent;
    FClientLoadBalancer.OnException := OnLoadBalancerExceptionEvent;
  end;
  Result := FClientLoadBalancer;
end;

function TsgcWSServer_Indy_Base.GetConnectionByGuid(const aGuid: String)
  : TsgcWSConnectionServer;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TIdContext>{$ENDIF};
  oConnection: TsgcWSConnectionServer;
begin
  Result := nil;
  oConnection := nil;

  if Optimizations.Connections.Enabled then
    Result := TsgcWSConnectionServer(GetConnections.GetConnection(aGuid))
  else
  begin
    oList := LockList;
    Try
      for i := 0 to oList.Count - 1 do
      begin
        Try
          oConnection := TsgcWSConnectionServer(TIdContext(oList[i])
            .{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF});
          if Assigned(oConnection) then
          begin
            if oConnection.Guid = aGuid then
            begin
              Result := oConnection;
              break;
            end;
          end;
        Except
          On E: Exception do
            DoException(oConnection, E.Message, E);
        end;
      end;
    Finally
      UnLockList;
    End;
  end;
end;

function TsgcWSServer_Indy_Base.GetConnectionByIndex(Index: Integer)
  : TsgcWSConnectionServer;
var
  oList: TList{$IFDEF NEXTGEN}<TIdContext>{$ENDIF};
begin
  Result := nil;

  oList := LockList;
  Try
    if Index < oList.Count then
      Result := TsgcWSConnectionServer(TIdContext(oList[Index])
        .{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF});
  Finally
    UnLockList;
  End;
end;

function TsgcWSServer_Indy_Base.GetCount: Integer;
var
  oList: TList{$IFDEF NEXTGEN}<TIdContext>{$ENDIF};
begin
  oList := LockList;
  Try
    Result := oList.Count;
  Finally
    UnLockList;
  End;
end;

function TsgcWSServer_Indy_Base.GetInterceptLogFile: TsgcWSIdLogFileServer;
begin
  if not Assigned(FInterceptLogFile) then
  begin
    FInterceptLogFile := TsgcWSIdLogFileServer.Create(self);
    FInterceptLogFile.ReplaceCRLF := False;
  end;
  FInterceptLogFile.Filename := LogFile.Filename;
  FInterceptLogFile.UnMaskFrames := LogFile.UnMaskFrames;
  Result := FInterceptLogFile;
end;

function TsgcWSServer_Indy_Base.GetLoadBalancerConnectionMessage
  (const aConnection: TsgcWSConnection; aConnected: Boolean): String;
var
  oMessage: TsgcWSLoadBalancerClientConnection;
begin
  oMessage := TsgcWSLoadBalancerClientConnection.Create(nil);
  Try
    oMessage.Active := aConnected;
    oMessage.Guid := aConnection.Guid;
    oMessage.IP := aConnection.IP;
    oMessage.Protocol := aConnection.Protocol;
    oMessage.Channel := aConnection.Subscriptions.Text;

    Result := oMessage.write;
  Finally
    sgcFree(oMessage);
  End;
end;

function TsgcWSServer_Indy_Base.GetMaxConnections: Integer;
begin
  Result := CustomServer.MaxConnections;
end;

function TsgcWSServer_Indy_Base.GetPort: Integer;
begin
  Result := CustomServer.DefaultPort;
end;

function TsgcWSServer_Indy_Base.GetSessionUserPassword(const aSessionID: String;
  var User, Password: string): Boolean;
var
  oList: TsgcDelimitedStringList;
begin
  Result := False;

  oList := TsgcDelimitedStringList.Create;
  Try
    oList.Delimiter := ':';
    oList.DelimitedText := GetSessionIDList.Values[aSessionID];
    if oList.Count > 0 then
    begin
      User := oList[0];
      Result := True;
    end;
    if oList.Count > 1 then
      Password := MidStr(oList.DelimitedText, Length(User) + 2,
        Length(oList.DelimitedText));
  Finally
    sgcFree(oList);
  End;
end;

function TsgcWSServer_Indy_Base.GetSSL: Boolean;
begin
  if not IsLoading and not IsDesigning then
    Result := GetCustomServer.IOHandler is TIdServerIOHandlerSSLOpenSSL
  else
    Result := FSSL;
end;

function TsgcWSServer_Indy_Base.GetThreadPool: Boolean;
begin
  if not IsLoading and not IsDesigning then
    Result := GetCustomServer.Scheduler is TIdSchedulerOfThreadPool
  else
    Result := FThreadPool;
end;

function TsgcWSServer_Indy_Base.IsOpenSSL: Boolean;
begin
  Result := False;
  if Assigned(FHandlerSSL) then
    Result := (FHandlerSSL.ClassType = TIdServerIOHandlerSSLOpenSSL) or
      (FHandlerSSL.InheritsFrom(TIdServerIOHandlerSSLOpenSSL))
end;

procedure TsgcWSServer_Indy_Base.Loaded;
begin
  inherited;
  if SSL <> FSSL then
    SSL := FSSL;
  if ThreadPool <> FThreadPool then
    ThreadPool := FThreadPool;
  if Active <> FActive then
    Active := FActive;
end;

function TsgcWSServer_Indy_Base.LockList:
{$IFDEF NEXTGEN}TList <
{$IFDEF SGC_INDY}sgcIdContext{$ELSE}IdContext{$ENDIF}.TIdContext >
{$ELSE}TList{$ENDIF};
begin
  Result := CustomServer.Contexts.LockList;
end;

procedure TsgcWSServer_Indy_Base.OnClientDisconnectedEvent(Sender: TObject);
begin
  // call only for IOCP IOHandler
  // fixes bug when client.options.CleanDisconnect = False
  // server never gets OnDisconnected event
  TIdTCPConnection(Sender).CheckForGracefulDisconnect(True);
end;

procedure TsgcWSServer_Indy_Base.SetActive(const Value: Boolean);
begin
  if not IsLoading and not IsDesigning then
  begin
    // ... avoid start again
    if Value and CustomServer.Active then
      exit;

    if Value then
    begin
      // ... ssl options
      if FSSL and Assigned(FHandlerSSL) and IsOpenSSL then
      begin
        TIdServerIOHandlerSSLOpenSSL(FHandlerSSL).SSLOptions.CertFile :=
          SSLOptions.CertFile;
        TIdServerIOHandlerSSLOpenSSL(FHandlerSSL).SSLOptions.KeyFile :=
          SSLOptions.KeyFile;
        TIdServerIOHandlerSSLOpenSSL(FHandlerSSL).SSLOptions.RootCertFile :=
          SSLOptions.RootCertFile;
      end;

      // ... threadpool options
      if FThreadPool and Assigned(FSchedulerThreadPool) then
      begin
        FSchedulerThreadPool.MaxThreads := ThreadPoolOptions.MaxThreads;
        FSchedulerThreadPool.PoolSize := ThreadPoolOptions.PoolSize;
      end;

      // ... LogFile
      if LogFile.Enabled then
        CustomServer.Intercept := InterceptLogFile
      else
        CustomServer.Intercept := nil;

      // ... iohandler
      DoIOHandler;
      DoScheduler;
    end;

    // ... clear
    if not CustomServer.Active then
      DoClear
    else
      DisconnectAll;

    // ... UseNagle
{$IFDEF INDY10_5_7}
    CustomServer.UseNagle := UseNagle;
{$ENDIF}
    // ... active
    CustomServer.DefaultPort := Port;
    CustomServer.Active := Value;

    if CustomServer.Active then
    begin
      // ... timers
      DoStartHeartBeat;
      DoStartConnectionFree;

      FWatchDogEnabled := WatchDog.Enabled;
      if FWatchDogEnabled then
        DoStartWatchDog;
    end
    else
    begin
      // ... timers
      FWatchDogEnabled := False;
      DoStopWatchDog;

      DoStopConnectionFree;
      DoStopHeartBeatTimeout;
      DoStopHeartBeat;
    end;
  end
  else
    FActive := Value;
end;

procedure TsgcWSServer_Indy_Base.OnServerConnectEvent(AContext: TIdContext);
var
  oConnection: TsgcWSConnectionServer;
  vAccept: Boolean;
{$IFNDEF LAZARUS}
  oThrottle: TIdInterceptThrottler;
{$ENDIF}
begin
  if IsDestroying then
    exit;

  oConnection := nil;

{$IFDEF MSWINDOWS}
  if Options.WriteTimeOut > 0 then
    AContext.Connection.Socket.Binding.SetSockOpt(Id_SOL_SOCKET, Id_SO_SNDTIMEO,
      Options.WriteTimeOut);
{$ENDIF}
{$IFDEF INDY10_5_8}
  if TCPKeepAlive.Enabled then
    AContext.Connection.Socket.Binding.SetKeepAliveValues(True,
      TCPKeepAlive.Time, TCPKeepAlive.Interval);
{$ENDIF}
  Try
    oConnection := TsgcWSConnectionServer.Create;
    oConnection.ServerSSL := SSL;
    oConnection.ServerOriginsAllowed := SecurityOptions.OriginsAllowed;
    oConnection.ValidateUTF8 := Options.ValidateUTF8;
    oConnection.CleanDisconnect := Options.CleanDisconnect;
    oConnection.RaiseDisconnectExceptions := Options.RaiseDisconnectExceptions;
    oConnection.QueueOptions := QueueOptions;
    oConnection.FragmentedMessages := Options.FragmentedMessages;
    oConnection.Extensions := Extensions;
    oConnection.Masked := False;
    oConnection.Protocol := GetProtocols;
    oConnection.ReadTimeOut := Options.ReadTimeOut;
    oConnection.TCPConnection := AContext.Connection;
    oConnection.OnMessage := OnServerMessageEvent;
    oConnection.OnBinary := OnServerBinaryEvent;
    oConnection.OnFragmented := OnServerFragmentedEvent;
    oConnection.OnUpdate := OnClientUpdateEvent;
    oConnection.OnHandshake := OnHandshake;
{$IFDEF SGC_HTTP2}
    oConnection.HTTP2FrameWrite.Frame_Settings.EnableConnectProtocol :=
      HTTP2Options.Settings.EnableConnectProtocol;
    oConnection.HTTP2FrameWrite.Frame_Settings.EnablePush :=
      HTTP2Options.Settings.EnablePush;
    oConnection.HTTP2FrameWrite.Frame_Settings.HeaderTableSize :=
      HTTP2Options.Settings.HeaderTableSize;
    oConnection.HTTP2FrameWrite.Frame_Settings.InitialWindowSize :=
      HTTP2Options.Settings.InitialWindowSize;
    oConnection.HTTP2FrameWrite.Frame_Settings.MaxConcurrentStreams :=
      HTTP2Options.Settings.MaxConcurrentStreams;
    oConnection.HTTP2FrameWrite.Frame_Settings.MaxFrameSize :=
      HTTP2Options.Settings.MaxFrameSize;
    oConnection.HTTP2FrameWrite.Frame_Settings.MaxHeaderListSize :=
      HTTP2Options.Settings.MaxHeaderListSize;
    oConnection.HTTP2FrameRead.MaxConcurrentStreams :=
      HTTP2Options.Settings.MaxConcurrentStreams;
    oConnection.HTTP2FrameRead.FragmentedData := HTTP2Options.FragmentedData;
    oConnection.FContext := AContext;
    oConnection.OnHTTP2Request := OnHTTP2RequestEvent;
    oConnection.HTTP2FrameRead.OnReadCreatePostStream :=
      OnHTTP2ReadCreatePostStreamEvent;
    oConnection.HTTP2FrameRead.OnReadDonePostStream :=
      OnHTTP2ReadDonePostStreamEvent;
    if SSL and HTTP2Options.Enabled and HTTP2Options.AltSvc.Enabled then
      oConnection.AltSvc := 'Alt-Svc: h2=":' + IntToStr(SSLOptions.Port) + '"';
{$ENDIF}
    if IOHandlerOptions.IOHandlerType = iohIOCP then
      AContext.Connection.OnDisconnected := OnClientDisconnectedEvent;
    if Optimizations.Channels.Enabled then
    begin
      oConnection.OnSubscription := OnServerSubscriptionEvent;
      oConnection.OnUnSubscription := OnServerUnSubscriptionEvent;
    end;
    if Authentication.Enabled then
      oConnection.OnAuthentication := OnServerAuthenticationEvent;
{$IFNDEF LAZARUS}
    if Throttle.Enabled then
    begin
      oThrottle := TIdInterceptThrottler.Create(nil);
      oThrottle.BitsPerSec := Throttle.BitsPerSec;
      AContext.Connection.IOHandler.Intercept := oThrottle;
    end;
{$ENDIF}
    AContext.{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF} := oConnection;
    oConnection.IPVersion := AContext.Binding.IPVersion;

    vAccept := True;
    if Assigned(FOnTCPConnect) then
    begin
      FOnTCPConnect(oConnection, vAccept);
      if not vAccept then
        raise TsgcWSException.Create(S_ERROR_CONNECTION_REJECTED);
    end;

{$IFDEF DEBUG}
    case IOHandlerOptions.IOHandlerType of
      iohDefault:
        sgcSetThreadName(ClassName + ': ' + oConnection.Guid);
      iohIOCP:
        sgcSetThreadName(ClassName);
    end;
{$ENDIF}
  Except
    On E: Exception do
      DoError(oConnection, E.Message);
  end;
end;

procedure TsgcWSServer_Indy_Base.OnClientUpdateEvent
  (aConnection: TsgcWSConnection; aType: Integer);
begin
  Case aType of
    // ... open connection
    1:
      begin
        case aConnection.Specification of
          spRFC6455:
            if not Specifications.RFC6455 then
              aConnection.Disconnect;
          spHixie76:
            if not Specifications.Drafts.Hixie76 then
              aConnection.Disconnect;
        end;
        // ... notify websocket connection
        if aConnection.Enabled then
          DoNotifyConnect(aConnection);
      end;
    // ... close connection
    -1: // nothing
    else
      // nothing
  End;
end;

procedure TsgcWSServer_Indy_Base.OnGetPasswordSSLEvent(var Password:
{$IFDEF INDY10_6}String{$ELSE}AnsiString{$ENDIF});
begin
  Password := SSLOptions.Password;
end;

procedure TsgcWSServer_Indy_Base.OnServerBinaryEvent
  (aConnection: TsgcWSConnection; const aStream: TMemoryStream);
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

procedure TsgcWSServer_Indy_Base.OnServerDisconnectEvent(AContext: TIdContext);
var
  oConnection: TsgcWSConnectionServer;
begin
  oConnection := nil;

  if Assigned(AContext.{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF}) then
  begin
    Try
      oConnection := AContext.{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF} as
        TsgcWSConnectionServer;

      if Assigned(oConnection) then
      begin
        // ... notify ONLY WebSocket / SSE connections
        if oConnection.Enabled then
        begin
          oConnection.Disconnected := True;
          if IsDestroying then // ... free connection on destroy
          begin
            sgcFree(oConnection);
            AContext.{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF} := nil;
            exit;
          end
          else
          begin
            DoNotifyDisconnect(oConnection);
            if NotifyEvents <> neAsynchronous then // ... nil if not async
              AContext.{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF} := nil;
          end;
        end
      end;
    Except
      On E: Exception do
        DoError(oConnection, E.Message);
    end;
  end;
end;

procedure TsgcWSServer_Indy_Base.OnGetSSLPortEvent(aPort: Integer;
  var aSSL: Boolean);
begin
  if (SSLOptions.Port = 0) or (SSLOptions.Port = aPort) then
    aSSL := True;
end;

procedure TsgcWSServer_Indy_Base.OnHeartBeatEvent(Sender: TObject);
begin
  inherited;
  // ... send ping
  DoPing;

  // ... start timeout
  DoStartHeartBeatTimeout;
end;

procedure TsgcWSServer_Indy_Base.OnHeartBeatExceptionEvent(Sender: TObject;
  E: Exception);
begin
  inherited;
  DoNotifyException(E.Message, E);
end;

procedure TsgcWSServer_Indy_Base.OnHeartBeatTimeoutEvent(Sender: TObject);
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TIdContext>{$ENDIF};
  oConnection: TsgcWSConnectionServer;
begin
  inherited;
  if IsDestroying then
    exit;

  // ... search in list
  oList := LockList;
  Try
    for i := 0 to oList.Count - 1 do
    begin
      oConnection := TsgcWSConnectionServer(TIdContext(oList[i])
        .{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF});
      if Assigned(oConnection) then
      begin
        if oConnection.Transport in [trpRFC6455, trpRFC8441, trpHixie76,
          trpFlash] then
        begin
          if Assigned(oConnection) and (oConnection.FCustomHeartBeat = False)
          then
          begin
            if SecondsBetWeen(oConnection.LastPing, oConnection.LastPong) >=
              HeartBeat.TimeOut then
            begin
              if ((HeartBeat.Interval >= HeartBeat.TimeOut) and
                (SecondsBetWeen(Now, oConnection.LastPing) >= HeartBeat.TimeOut)
                or (HeartBeat.TimeOut >= HeartBeat.Interval) and
                (SecondsBetWeen(Now, oConnection.FirstPing) >=
                HeartBeat.TimeOut)) then
              begin
{$IFDEF SGC_DEBUG}
                DoLog(self, oConnection, S_HEARTBEAT_TIMEOUT_EXCEEDED);
{$ENDIF}
                DoException(oConnection, S_HEARTBEAT_TIMEOUT_EXCEEDED, nil);
              end
            end
            else
              oConnection.FirstPing := 0;
          end;
        end;
      end;
    end;
  Finally
    UnLockList;
  End;
end;

procedure TsgcWSServer_Indy_Base.OnHeartBeatTimeoutExceptionEvent
  (Sender: TObject; E: Exception);
begin
  inherited;
  DoNotifyException(E.Message, E);
end;

procedure TsgcWSServer_Indy_Base.OnLoadBalancerBroadCastBinaryEvent
  (aStream: TStream; const aChannel, aProtocol, aExclude, aInclude: String;
  aSize: Integer; aStreaming: TwsStreaming);
begin
  DoBroadCast('', aStream, opBinary, aChannel, aProtocol, aExclude, aInclude,
    aSize, aStreaming);
end;

procedure TsgcWSServer_Indy_Base.OnLoadBalancerBroadCastTextEvent(const aText,
  aChannel, aProtocol, aExclude, aInclude: String);
begin
  DoBroadCast(aText, nil, opText, aChannel, aProtocol, aExclude, aInclude);
end;

procedure TsgcWSServer_Indy_Base.OnLoadBalancerConnectEvent
  (Connection: TsgcWSConnection);
begin
  if Assigned(FOnLoadBalancerConnect) then
    FOnLoadBalancerConnect(Connection);

  DoLoadBalancerSendServerData;
  DoLoadBalancerSendServerBindings;
  DoLoadBalancerSendActiveConnections;
  DoLoadBalancerSendServerReady;
end;

procedure TsgcWSServer_Indy_Base.OnLoadBalancerDisconnectEvent
  (Connection: TsgcWSConnection; Code: Integer);
begin
  if Assigned(FOnLoadBalancerDisconnect) then
    FOnLoadBalancerDisconnect(Connection, Code);
end;

procedure TsgcWSServer_Indy_Base.OnLoadBalancerErrorEvent
  (Connection: TsgcWSConnection; const Error: string);
begin
  if Assigned(FOnLoadBalancerError) then
    FOnLoadBalancerError(self, Error);
end;

procedure TsgcWSServer_Indy_Base.OnLoadBalancerExceptionEvent
  (Connection: TsgcWSConnection; E: Exception);
begin
  if Assigned(FOnLoadBalancerError) then
    FOnLoadBalancerError(self, E.Message);
end;

procedure TsgcWSServer_Indy_Base.OnLoadBalancerWriteDataBinaryEvent
  (const aGuid: String; aStream: TStream);
begin
  DoWriteData(aGuid, aStream);
end;

procedure TsgcWSServer_Indy_Base.OnLoadBalancerWriteDataTextEvent(const aGuid,
  aText: String);
begin
  DoWriteData(aGuid, aText);
end;

procedure TsgcWSServer_Indy_Base.OnServerAuthenticationEvent
  (Connection: TsgcWSConnection; aUser, aPassword: String;
  var Authenticated: Boolean);
begin
  // ... authlist
  if Authentication.AuthUsers.Count > 0 then
    if Authentication.AuthUsers.IndexOfName(aUser) <> -1 then
      if Authentication.AuthUsers.Values[aUser] = aPassword then
        Authenticated := True;

  // ... event
  if Assigned(FOnAuthentication) then
    FOnAuthentication(Connection, aUser, aPassword, Authenticated);

  // ... save session if authenticated
  if Authenticated then
  begin
    if TsgcWSConnectionServer(Connection).Authentication.AuthType = authSession
    then
      DoAddSessionID(TsgcWSConnectionServer(Connection)
        .Authentication.SessionID, aUser, aPassword);
  end;
end;

procedure TsgcWSServer_Indy_Base.OnServerFragmentedEvent
  (aConnection: TsgcWSConnection; const aData: TMemoryStream;
  const aOpCode: TOpCode; const aContinuation: Boolean);
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

procedure TsgcWSServer_Indy_Base.OnServerShutdownEvent(Sender: TObject);
begin
  FWatchDogMonitorSecret := '';

  // ... LoadBalancer
  if LoadBalancer.Enabled then
    ClientLoadBalancer.Active := False;

  // ... event
  if Assigned(FOnShutdown) then
    FOnShutdown(self);
end;

procedure TsgcWSServer_Indy_Base.OnServerStartupEvent(Sender: TObject);
begin
  FWatchDogMonitorSecret := NewGuid;

  // ... event
  if Assigned(FOnStartup) then
    FOnStartup(self);

  // ... LoadBalancer
  if LoadBalancer.Enabled then
    ClientLoadBalancer.Active := True;

  // ... OpenSSL ECDHE
  DoOpenSSLECDHE;
end;

procedure TsgcWSServer_Indy_Base.OnServerSubscriptionEvent
  (Connection: TsgcWSConnection; const Subscription: String);
var
  oItem: TsgcQueueItemChannel;
begin
  if IsDestroying then
    exit;

  if Optimizations.Channels.Enabled then
  begin
    if Assigned(Connection) then
    begin
      oItem := TsgcQueueItemChannel.Create;
      oItem.Connection := Connection;
      oItem.Queue := Subscription;
      GetChannels.AddItem(oItem);
    end;
  end;
end;

procedure TsgcWSServer_Indy_Base.OnServerUnSubscriptionEvent
  (Connection: TsgcWSConnection; const Subscription: String);
begin
  if IsDestroying then
    exit;

  if Optimizations.Channels.Enabled then
  begin
    if Assigned(Connection) then
      GetChannels.DeleteItem(Subscription, Connection.Guid);
  end;
end;

procedure TsgcWSServer_Indy_Base.OnSSLALPNSelectEvent(Sender: TObject;
  aProtocols: TStringList; var aProtocol: String);
begin
  // ... http/2 protocol
  if HTTP2Options.Enabled then
  begin
    if aProtocols.IndexOf(CS_ALPN_H2) > -1 then
    begin
      aProtocol := CS_ALPN_H2;
      exit;
    end;
  end;

  if Assigned(FOnSSLALPNSelect) then
    FOnSSLALPNSelect(Sender, aProtocols, aProtocol);
end;

procedure TsgcWSServer_Indy_Base.OnWatchDogEvent(Sender: TObject);
begin
  if IsDestroying then
    exit;

  case NotifyEvents of
    neNoSync:
      begin
        if not Active then
          Start
        else if WatchDog.Monitor.Enabled then
          DoWatchDogMonitor;
      end
  else
    begin
      if not Active then
        NotifyMethod(DoStart)
      else if WatchDog.Monitor.Enabled then
        NotifyMethod(DoWatchDogMonitor);
    end;
  end;
  inherited;
end;

procedure TsgcWSServer_Indy_Base.OnWatchDogExceptionEvent(Sender: TObject;
  E: Exception);
begin
  inherited;
  DoNotifyException(E.Message, E);
end;

procedure TsgcWSServer_Indy_Base.Ping(const aText: string = '');
begin
  DoPing(aText);
end;

procedure TsgcWSServer_Indy_Base.SetAuthentication
  (const Value: TsgcWSAuthenticationServer_Options);
begin
  if Assigned(FAuthentication) then
    FAuthentication.Assign(Value);
end;

procedure TsgcWSServer_Indy_Base.SetBindings(const Value: TIdSocketHandles);
begin
  CustomServer.Bindings := Value;
end;

procedure TsgcWSServer_Indy_Base.SetFallBack(const Value
  : TsgcWSFallBack_Options);
begin
  if Assigned(FFallBack) then
    FFallBack.Assign(Value);
end;

procedure TsgcWSServer_Indy_Base.SetIOHandlerOptions
  (const Value: TsgcWSIOHandler_Options);
begin
  if Assigned(FIOHandlerOptions) then
    FIOHandlerOptions.Assign(Value);
end;

procedure TsgcWSServer_Indy_Base.SetLoadBalancer(const Value
  : TsgcWSLoadBalancerServer_Options);
begin
  if Assigned(FLoadBalancer) then
    FLoadBalancer.Assign(Value);
end;

procedure TsgcWSServer_Indy_Base.SetSecurityOptions
  (const Value: TsgcWSSecurity_Options);
begin
  if Assigned(FSecurityOptions) then
    FSecurityOptions.Assign(Value);
end;

procedure TsgcWSServer_Indy_Base.SetMaxConnections(const Value: Integer);
begin
  CustomServer.MaxConnections := Value;
end;

procedure TsgcWSServer_Indy_Base.SetOptions(const Value: TsgcWSOptionsServer);
begin
  if Assigned(FOptions) then
    FOptions.Assign(Value);
end;

procedure TsgcWSServer_Indy_Base._SetPort(const Value: Integer);
begin
  CustomServer.DefaultPort := Value;
end;

procedure TsgcWSServer_Indy_Base.SetQueueOptions(const Value
  : TsgcWSQueueServer_Options);
begin
  if Assigned(FQueueOptions) then
    FQueueOptions.Assign(Value);
end;

procedure TsgcWSServer_Indy_Base.SetSSL(const Value: Boolean);
begin
  FSSL := Value;

  if IsLoading then
    exit;

  if FSSL then
  begin
    if not Assigned(FHandlerSSL) then
    begin
      GetHandlerSSLEvent(sslServer, FHandlerSSL);
      GetCustomServer.IOHandler := FHandlerSSL;
    end;
  end
  else
  begin
    if not IsDesigning then
    begin
      if Assigned(FHandlerSSL) then
      begin
        GetCustomServer.IOHandler := nil;
        sgcFree(FHandlerSSL);
      end;
    end;
  end;
end;

procedure TsgcWSServer_Indy_Base.SetSSLOptions(const Value: TsgcWSSSL_Options);
begin
  if Assigned(FSSLOptions) then
    FSSLOptions.Assign(Value);
end;

procedure TsgcWSServer_Indy_Base.SetThreadPool(const Value: Boolean);
begin
  FThreadPool := Value;

  if IsLoading then
    exit;

  if FThreadPool then
  begin
    if not IsDesigning then
    begin
      if not Assigned(FSchedulerThreadPool) then
      begin
        FSchedulerThreadPool := TIdSchedulerOfThreadPool.Create(self);
        GetCustomServer.Scheduler := FSchedulerThreadPool;
      end;
    end;
  end
  else
  begin
    if not IsDesigning then
    begin
      if Assigned(FSchedulerThreadPool) then
      begin
        GetCustomServer.Scheduler := nil;
        sgcFree(FSchedulerThreadPool);
      end;
    end;
  end;
end;

procedure TsgcWSServer_Indy_Base.SetThreadPoolOptions
  (const Value: TsgcWSThreadPool_Options);
begin
  if Assigned(FThreadPoolOptions) then
    FThreadPoolOptions.Assign(Value);
end;

procedure TsgcWSServer_Indy_Base.Start;
begin
  TServerThread.Start(self);
end;

procedure TsgcWSServer_Indy_Base.Stop;
begin
  TServerThread.Stop(self);
end;

procedure TsgcWSServer_Indy_Base.UnLockList;
begin
  CustomServer.Contexts.UnLockList;
end;

function TsgcWSServer_Indy_Base.WriteData(const aGuid,
  aMessage: string): Boolean;
begin
  Result := DoWriteData(aGuid, aMessage);

  if not Result then
  begin
    if LoadBalancer.Enabled then
    begin
      ClientLoadBalancer.WriteData(aGuid, aMessage);
      Result := True;
    end
  end;
end;

function TsgcWSServer_Indy_Base.WriteData(const aGuid: String; aStream: TStream;
  aSize: Integer = 0; const aStreaming: TwsStreaming = stmNone): Boolean;
begin
  Result := DoWriteData(aGuid, aStream, aSize, aStreaming);

  if not Result then
  begin
    if LoadBalancer.Enabled then
    begin
      ClientLoadBalancer.WriteData(aGuid, aStream, aSize, aStreaming);
      Result := True;
    end
  end;
end;

constructor TsgcWSServer.Create(aOwner: TComponent);
begin
  inherited;
  FServerType := wsTCP;
end;

destructor TsgcWSServer.Destroy;
begin
  if Assigned(FTCPServer) then
  begin
    if Active then
    begin
      Try
        Active := False;
        Bindings.Clear;
      Finally
        sgcFree(FTCPServer);
      End;
    end
    else
      sgcFree(FTCPServer);
  end;
  inherited;
end;

function TsgcWSServer.GetCustomServer: TIdCustomTCPServer;
begin
  if not Assigned(FTCPServer) then
  begin
    FTCPServer := TsgcWSCustomServer.Create;
    FTCPServer.DefaultPort := Port;

    // Events
    FTCPServer.OnConnect := OnServerConnectEvent;
    FTCPServer.OnDisconnect := OnServerDisconnectEvent;
    FTCPServer.OnExecute := OnServerExecuteEvent;
    FTCPServer.OnQuerySSLPort := OnGetSSLPortEvent;

    FTCPServer.OnStartup := OnServerStartupEvent;
    FTCPServer.OnShutdown := OnServerShutdownEvent;
  end;

  Result := FTCPServer;
end;

function TsgcWSServer.GetTCPServer: TsgcWSCustomServer;
begin
  Result := CustomServer as TsgcWSCustomServer;
end;

procedure TsgcWSServer.OnHTTP2RequestEvent(const aConnection: TsgcWSConnection;
  const aHeaders: TStringList; const aBytes: TBytes;
  aStreamIdentifierRequest, aStreamIdentifierPush: Integer;
  const aPostStream: TStream);
begin
  inherited;
  DoBuiltInLibraries(TsgcWSConnectionServer(aConnection),
    aHeaders.Values[':method'] + ' ' + aHeaders.Values[':path'] + ' HTTP/2',
    False, aStreamIdentifierRequest);
end;

procedure TsgcWSServer.OnServerDisconnectEvent(AContext: TIdContext);
var
  oConnection: TsgcWSConnectionServer;
begin
  inherited;
  if Assigned(AContext.{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF}) then
  begin
    oConnection := AContext.{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF} as
      TsgcWSConnectionServer;
    if Assigned(oConnection) then
    begin
      if not oConnection.Enabled then
        sgcFree(oConnection);
    end;

    AContext.{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF} := nil;
  end;
end;

procedure TsgcWSServer.OnServerExecuteEvent(AContext: TIdContext);
var
  oConnection: TsgcWSConnectionServer;
  vRead: Boolean;
  vAuthenticated: Boolean;
begin
  inherited;

  if IsDestroying then
    exit;

  if AContext.Connection.IOHandler.InputBufferIsEmpty then
    AContext.Connection.IOHandler.CheckForDataOnSource(Options.ReadTimeOut);

  oConnection := TsgcWSConnectionServer
    (AContext.{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF});
  if not Assigned(oConnection) then
  begin
    AContext.Connection.IOHandler.InputBuffer.Clear;
    AContext.Connection.Disconnect(False);
    exit;
  end
  else if oConnection.Disconnected then
  begin
    oConnection.TCPConnection.IOHandler.InputBuffer.Clear;
    AContext.Connection.Disconnect(False);
    exit;
  end;

  if (Assigned(QueueOptions) and ((QueueOptions.Text.Level <> qmNone) or
    (QueueOptions.Binary.Level <> qmNone) or (QueueOptions.Ping.Level <>
    qmNone))) then
    DoWriteQueueMsgLevels(oConnection);

  if AContext.Connection.IOHandler.InputBuffer.Size > 0 then
  begin
    vRead := True;
    Try
      if not oConnection.Enabled then
      begin
{$IFDEF D2009}
        // ... handle fragmented tcp messages
        repeat
        until not AContext.Connection.IOHandler.CheckForDataOnSource
          (Options.ReadTimeOut) or
          (AContext.Connection.IOHandler.InputBuffer.Size > MaxReadBufferSize);
{$ENDIF}
        // ... capture headers
        oConnection.HeadersRequest.Clear;
        oConnection.HeadersRequest.Text :=
          AContext.Connection.IOHandler.InputBuffer.AsString;

        // ... response authentication
        if IsRequestAuthenticationSessionHeader(oConnection.HeadersRequest) then
        begin
          if self.Authentication.Enabled and self.Authentication.Session.Enabled
          then
            oConnection.DoAuthSessionResponse
              (DecodeGETFullPath(oConnection.HeadersRequest));
          // ... free connection
          DoContextFree(AContext);
          vRead := False;
        end
        else if IsWebSocketHeader(oConnection.HeadersRequest) then
        begin
          oConnection.TCPConnection.IOHandler.InputBuffer.Clear;
          vRead := True;
          // ... watchdog connection
          if WatchDog.Enabled and WatchDog.Monitor.Enabled and
            IsWatchDogConnection(oConnection.HeadersRequest,
            FWatchDogMonitorSecret) then
            oConnection.IsInternal := True;
        end
        // ... xhr
        else if IsXHRHeader(oConnection.HeadersRequest) then
        begin
          oConnection.Transport := trpHTTP;
          oConnection.HeadersRequest.Clear;
          oConnection.TCPConnection.IOHandler.Capture
            (oConnection.HeadersRequest, '');
          DoXHR(oConnection);
          // ... free connection
          DoContextFree(AContext);
          vRead := False;
        end
        // ... flash fallback
        else if IsRequestFlashPolicy(oConnection.HeadersRequest) then
        begin
          oConnection.Transport := trpFlash;
          oConnection.HeadersRequest.Clear;
          oConnection.TCPConnection.IOHandler.InputBuffer.Clear;
          if FallBack.Flash.Enabled then
          begin
            oConnection.TCPConnection.IOHandler.WriteLn('<cross-domain-policy>'
              + '<allow-access-from domain="' + FallBack.Flash.Domain +
              '" to-ports="' + FallBack.Flash.Ports + '" />' +
              '</cross-domain-policy>' + chr(0));
          end
          else
          begin
            // ... free connection
            DoContextFree(AContext);
            vRead := False;
          end;
        end
        // ... server sent events
        else if IsSSEHeader(oConnection.HeadersRequest) then
        begin
          oConnection.Transport := trpSSE;
          oConnection.HeadersRequest.Clear;
          oConnection.TCPConnection.IOHandler.Capture
            (oConnection.HeadersRequest, '');
          oConnection.TCPConnection.IOHandler.InputBuffer.Clear;
          if FallBack.ServerSentEvents.Enabled then
          begin
            oConnection.DoProtocolSSE;
            DoNotifyConnect(oConnection);
            oConnection.WriteData
              ('retry: ' + IntToStr(FallBack.ServerSentEvents.Retry * 1000));
            oConnection.WriteData(oConnection.Guid);
            oConnection.WriteData(oConnection.Guid);
            oConnection.FEnabled := True;
          end
          else
          begin
            // ... free connection
            DoContextFree(AContext);
            vRead := False;
          end;
        end
        // ... http protocol
        else if IsHTTPProtocolHeader(oConnection.HeadersRequest) then
        begin
          oConnection.Transport := trpHTTP;
          oConnection.HeadersRequest.Clear;
          oConnection.TCPConnection.IOHandler.Capture
            (oConnection.HeadersRequest, '');
          DoHTTPProtocol(oConnection);
          // ... free connection
          DoContextFree(AContext);
          vRead := False;
        end
        // ... built-in libraries
        else if DoBuiltInLibraries(oConnection,
          DecodeGETHeader(oConnection.HeadersRequest), True) then
        begin
          DoContextFree(AContext);
          vRead := False;
        end
        else if DoUnknownProtocol(oConnection) then
        begin
          oConnection.FEnabled := True;
          OnClientUpdateEvent(oConnection, 1);
          oConnection.ReadData;
        end
        else
        begin
          DoContextFree(AContext);
          vRead := False;
        end;
      end;
      // ... authenticate websocket
      if self.Authentication.Enabled and vRead and
        (oConnection.IsInternal = False) then
      begin
        if not oConnection.Authentication.Authenticated then
        begin
{$IFDEF SGC_OAUTH_SERVER}
          // ... oauth2
          if Assigned(self.Authentication.OAuth.OAuth2) and
            self.Authentication.OAuth.OAuth2.IsOAuth2TokenValid(oConnection,
            oConnection.HeadersRequest) then
          begin
            oConnection.Authentication.Authenticated := True;
          end
          else
{$ENDIF}
{$IFDEF SGC_JWT_SERVER}
            // ... jwt
            if Assigned(self.Authentication.JWT.JWT) and
              self.Authentication.JWT.JWT.IsJWTTokenValid(oConnection,
              oConnection.HeadersRequest) then
            begin
              oConnection.Authentication.Authenticated := True;
            end
            else
{$ENDIF}
              // ... authenticate basic
              if Authentication.Basic.Enabled and
                IsAuthenticationBasicHeader(oConnection.HeadersRequest) then
              begin
                oConnection.Authentication.Authenticated :=
                  oConnection.DoAuthenticationBasic
                  (DecodeAuthorizationBasic(oConnection.HeadersRequest));
              end
              // ... authenticate url
              else if Authentication.URL.Enabled and
                IsAuthenticationURLHeader(oConnection.HeadersRequest) then
              begin
                oConnection.DoAuthenticationURL
                  (DecodeGETFullPath(oConnection.HeadersRequest));
              end
              // ... authenticate http
              else if Authentication.Session.Enabled and
                IsAuthenticationSessionHeader(oConnection.HeadersRequest) then
              begin
                oConnection.Authentication.Authenticated :=
                  DoAuthenticationSession(oConnection,
                  DecodeGETFullPath(oConnection.HeadersRequest));
              end
              // ... unknown authorization
              else if IsAuthenticationHeader(oConnection.HeadersRequest) then
                oConnection.Authentication.Authenticated :=
                  DoUnknownAuthentication(oConnection)
                // ... raise OnAuthentication event
              else
              begin
                vAuthenticated := oConnection.Authentication.Authenticated;
                if Assigned(FOnAuthentication) then
                begin
                  FOnAuthentication(oConnection, '', '', vAuthenticated);
                  oConnection.Authentication.Authenticated := vAuthenticated;
                end;
              end;
          // ... if not authenticated then close
          if not oConnection.Authentication.Authenticated then
          begin
            AContext.{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF} := nil;
            if oConnection.FAuthenticationCloseCode = CS_CLOSE_NORMAL then
            // send custom close code after handshake
            begin
              DoContextFree(AContext);
              vRead := False;
            end;
          end;
        end;
      end;
      // ... read data
      if vRead then
        oConnection.ReadData(True);
    Except
      On E: EIdSilentException do
        raise;
      On E: Exception do
        DoError(TsgcWSConnectionServer
          (AContext.{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF}), E.Message);
    end;
  end;
end;

constructor TsgcWSHTTPServer.Create(aOwner: TComponent);
begin
  inherited;
  FServerType := wsHTTP;
  FHTTPServer.ServerSoftware := CS_APPLICATION_NAME + ' ' + CS_VERSION;
  ReadEmptySource := 0;
  ReadStartSSL := 10;
end;

destructor TsgcWSHTTPServer.Destroy;
begin
  if Assigned(FHTTPServer) then
  begin
    if Active then
    begin
      Try
        Active := False;
        Bindings.Clear;
      Finally
        sgcFree(FHTTPServer);
      End;
    end
    else
      sgcFree(FHTTPServer);
  end;
{$IFDEF SGC_HTTP2}
  sgcFree(FPushPromiseList);
{$ENDIF}
  inherited;
end;

procedure TsgcWSHTTPServer.DoExecuteHTTP(AContext: TIdContext);
begin
  TsgcWSHTTPCustomServer(FHTTPServer).DoExecuteHTTP(AContext);
end;

function TsgcWSHTTPServer.DoHTTPBasicAuthentication
  (aConnection: TsgcWSConnection; var ARequestInfo: TIdHTTPRequestInfo;
  var AResponseInfo: TIdHTTPResponseInfo): Boolean;
begin
  Result := False;
  if ARequestInfo.AuthExists then
  begin
    if Authentication.AuthUsers.Count > 0 then
    begin
      if Authentication.AuthUsers.IndexOfName(ARequestInfo.AuthUsername) <> -1
      then
      begin
        if Authentication.AuthUsers.Values[ARequestInfo.AuthUsername]
          = ARequestInfo.AuthPassword then
          Result := True
      end;
    end;
    if not Result then
      if Assigned(FOnAuthentication) then
        FOnAuthentication(aConnection, ARequestInfo.AuthUsername,
          ARequestInfo.AuthPassword, Result);
  end;
  if not Result then
    AResponseInfo.AuthRealm := CS_APPLICATION_NAME + ' ' + CS_VERSION;
end;

function TsgcWSHTTPServer.DoResponseHTTP(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo)
  : Boolean;
var
  oStream: TMemoryStream;
  vDocument: String;
begin
  Result := False;

  if DocumentRoot <> '' then
  begin
{$IFDEF LINUX64}
    vDocument := DocumentRoot + ARequestInfo.Document;
{$ELSE}
    vDocument := StringReplace(DocumentRoot + ARequestInfo.Document, '/', '\',
      [rfReplaceAll]);
{$ENDIF}
    if FileExists(vDocument) then
    begin
{$IFDEF INDY10_5_5}
      AResponseInfo.Charset := Charset;
{$ENDIF}
      AResponseInfo.ContentType := HTTPServer.MIMETable.GetFileMIMEType
        (vDocument);
      oStream := TMemoryStream.Create;
      Try
        oStream.LoadFromFile(vDocument);
        oStream.Position := 0;
      Except
        sgcFree(oStream);
        raise;
      End;
      AResponseInfo.ContentStream := oStream;
      Result := True;
    end;
  end;
end;

procedure TsgcWSHTTPServer.DoResponseAfterCommand
  (aConnection: TsgcWSConnectionServer; AResponseInfo: TIdHTTPResponseInfo);
begin
  if aConnection.AltSvc <> '' then
    AResponseInfo.CustomHeaders.Add(aConnection.AltSvc);
end;

function TsgcWSHTTPServer.GetAutoStartSession: Boolean;
begin
  Result := FHTTPServer.AutoStartSession;
end;

function TsgcWSHTTPServer.GetCustomServer: TIdCustomTCPServer;
begin
  if not Assigned(FHTTPServer) then
  begin
    FHTTPServer := TsgcWSHTTPCustomServer.Create;
    FHTTPServer.DefaultPort := Port;

    // Events
    FHTTPServer.OnConnect := OnServerConnectEvent;
    FHTTPServer.OnDisconnect := OnServerDisconnectEvent;
    TsgcWSHTTPCustomServer(FHTTPServer).OnExecute := OnServerExecuteEvent;
    TsgcWSHTTPCustomServer(FHTTPServer).OnQuerySSLPort := OnGetSSLPortEvent;

    TsgcWSHTTPCustomServer(FHTTPServer).OnStartup := OnServerStartupEvent;
    TsgcWSHTTPCustomServer(FHTTPServer).OnShutdown := OnServerShutdownEvent;
{$IFDEF INDY10_5_7}
    TsgcWSHTTPCustomServer(FHTTPServer).OnParseAuthentication :=
      OnServerParseAuthenticationEvent;
{$ENDIF}
    // Events HTTP
    TsgcWSHTTPCustomServer(FHTTPServer).OnCommandGet := OnCommandGetEvent;
    FHTTPServer.OnCommandOther := OnCommandOtherEvent;
    FHTTPServer.OnCreateSession := OnCreateSessionEvent;
    FHTTPServer.OnInvalidSession := OnInvalidSessionEvent;
    FHTTPServer.OnSessionStart := OnSessionStartEvent;
    FHTTPServer.OnSessionEnd := OnSessionEndEvent;
    FHTTPServer.OnException := OnExceptionEvent;
{$IFDEF INDY10_5_7}
    TsgcWSHTTPCustomServer(FHTTPServer).OnCreatePostStream :=
      OnCreatePostStreamEvent;
    TsgcWSHTTPCustomServer(FHTTPServer).OnDoneWithPostStream :=
      OnDoneWithPostStreamEvent;
{$ENDIF}
  end;

  Result := FHTTPServer;
end;

function TsgcWSHTTPServer.GetDocumentRoot: String;
begin
  if RightStr(FDocumentRoot, 1) = '/' then
    FDocumentRoot := MidStr(FDocumentRoot, 1, Length(FDocumentRoot) - 1)
  else if RightStr(FDocumentRoot, 1) = '\' then
    FDocumentRoot := MidStr(FDocumentRoot, 1, Length(FDocumentRoot) - 1);
  Result := FDocumentRoot;
end;

function TsgcWSHTTPServer.GetKeepAlive: Boolean;
begin
  Result := FHTTPServer.KeepAlive;
end;

function TsgcWSHTTPServer.GetParseParams: Boolean;
begin
  Result := FHTTPServer.ParseParams;
end;

function TsgcWSHTTPServer.GetSessionList: TIdHTTPCustomSessionList;
begin
  Result := FHTTPServer.SessionList;
end;

function TsgcWSHTTPServer.GetSessionState: Boolean;
begin
  Result := FHTTPServer.SessionState;
end;

function TsgcWSHTTPServer.GetSessionTimeOut: Integer;
begin
  Result := FHTTPServer.SessionTimeOut;
end;

procedure TsgcWSHTTPServer.OnCommandGetEvent(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  oConnection: TsgcWSConnection;
begin
  oConnection := TsgcWSConnection
    (AContext.{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF});

  if DoForwardHTTP(oConnection, ARequestInfo, AResponseInfo) then
    exit;

  if DoBeforeOnCommand(oConnection, ARequestInfo, AResponseInfo) then
  begin
    if not DoResponseHTTP(AContext, ARequestInfo, AResponseInfo) then
    begin
      if Assigned(FOnCommandGet) then
      begin
        FOnCommandGet(AContext, ARequestInfo, AResponseInfo);
        DoResponseAfterCommand(TsgcWSConnectionServer(oConnection),
          AResponseInfo);
      end;
    end;
  end;
end;

procedure TsgcWSHTTPServer.OnCommandOtherEvent(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  oConnection: TsgcWSConnection;
begin
  oConnection := TsgcWSConnection
    (AContext.{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF});

  if DoForwardHTTP(oConnection, ARequestInfo, AResponseInfo) then
    exit;

  if DoBeforeOnCommand(oConnection, ARequestInfo, AResponseInfo) then
  begin
    if Assigned(FOnCommandOther) then
    begin
      FOnCommandOther(AContext, ARequestInfo, AResponseInfo);
      DoResponseAfterCommand(TsgcWSConnectionServer(oConnection),
        AResponseInfo);
    end;
  end;
end;

procedure TsgcWSHTTPServer.OnCreateSessionEvent(ASender: TIdContext;
  var VHTTPSession: TIdHTTPSession);
begin
  if Assigned(FOnCreateSession) then
    FOnCreateSession(ASender, VHTTPSession);
end;

procedure TsgcWSHTTPServer.OnExceptionEvent(AContext: TIdContext;
  AException: Exception);
begin
  DoException(TsgcWSConnection(AContext.{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF}), AException.Message, AException);
end;

procedure TsgcWSHTTPServer.OnInvalidSessionEvent(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo;
  var VContinueProcessing: Boolean; const AInvalidSessionID: string);
begin
  if Assigned(FOnInvalidSession) then
    FOnInvalidSession(AContext, ARequestInfo, AResponseInfo,
      VContinueProcessing, AInvalidSessionID);
end;

procedure TsgcWSHTTPServer.OnServerDisconnectEvent(AContext: TIdContext);
var
  oConnection: TsgcWSConnectionServer;
begin
  inherited;
  if Assigned(AContext.{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF}) then
  begin
    oConnection := AContext.{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF} as
      TsgcWSConnectionServer;
    if Assigned(oConnection) then
    begin
      if not oConnection.Enabled then
        sgcFree(oConnection);
    end;

    AContext.{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF} := nil;
  end;
end;

procedure TsgcWSHTTPServer.OnServerExecuteEvent(AContext: TIdContext);
var
  oConnection: TsgcWSConnectionServer;
  vAuthenticated: Boolean;
begin
  inherited;

  if IsDestroying then
    exit;

  if AContext.Connection.IOHandler.InputBufferIsEmpty then
    AContext.Connection.IOHandler.CheckForDataOnSource(Options.ReadTimeOut);

  oConnection := TsgcWSConnectionServer
    (AContext.{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF});

  if not Assigned(oConnection) then
  begin
    AContext.Connection.IOHandler.InputBuffer.Clear;
    AContext.Connection.Disconnect(False);
    exit;
  end
  else if oConnection.Disconnected then
  begin
    oConnection.TCPConnection.IOHandler.InputBuffer.Clear;
    AContext.Connection.Disconnect(False);
    exit;
  end;

  if (Assigned(QueueOptions) and ((QueueOptions.Text.Level <> qmNone) or
    (QueueOptions.Binary.Level <> qmNone) or (QueueOptions.Ping.Level <>
    qmNone))) then
    DoWriteQueueMsgLevels(oConnection);

  if AContext.Connection.IOHandler.InputBuffer.Size > 0 then
  begin
    oConnection.FReadCount := 0;

    // ssl open connection
    if not oConnection.Enabled then
    begin
      if SSL then
      begin
        if AContext.Connection.IOHandler.InputBuffer.Size = 1 then
        begin
          oConnection.FReadStartSSLCount := oConnection.FReadStartSSLCount + 1;
          // close connection if reads more than 10 (by default)
          if oConnection.FReadStartSSLCount > ReadStartSSL then
          begin
            oConnection.HeadersRequest.Clear;
            oConnection.TCPConnection.IOHandler.InputBuffer.Clear;
            raise TsgcWSException.Create(S_ERROR_START_SSL);
          end;
          exit;
        end;
      end;
    end;

    Try
      if not oConnection.Enabled then
      begin
{$IFDEF D2009}
        // ... handle fragmented tcp messages
        repeat
        until not AContext.Connection.IOHandler.CheckForDataOnSource
          (Options.ReadTimeOut) or
          (AContext.Connection.IOHandler.InputBuffer.Size > MaxReadBufferSize);
{$ENDIF}
        // ... capture headers
        oConnection.HeadersRequest.Clear;
        oConnection.HeadersRequest.Text :=
          AContext.Connection.IOHandler.InputBuffer.AsString;

        if IsWebSocketHeader(oConnection.HeadersRequest) then
        begin
          // ... watchdog connection
          if WatchDog.Enabled and WatchDog.Monitor.Enabled and
            IsWatchDogConnection(oConnection.HeadersRequest,
            FWatchDogMonitorSecret) then
            oConnection.IsInternal := True;

          // ... authentication
          if self.Authentication.Enabled and (oConnection.IsInternal = False)
          then
          begin
{$IFDEF SGC_OAUTH_SERVER}
            // ... oauth2
            if Assigned(self.Authentication.OAuth.OAuth2) and
              self.Authentication.OAuth.OAuth2.IsOAuth2TokenValid(oConnection,
              oConnection.HeadersRequest) then
            begin
              oConnection.Authentication.Authenticated := True;
            end
            else
{$ENDIF}
{$IFDEF SGC_JWT_SERVER}
              // ... jwt
              if Assigned(self.Authentication.JWT.JWT) and
                self.Authentication.JWT.JWT.IsJWTTokenValid(oConnection,
                oConnection.HeadersRequest) then
              begin
                oConnection.Authentication.Authenticated := True;
              end
              else
{$ENDIF}
                // ... authenticate basic
                if self.Authentication.Basic.Enabled and
                  IsAuthenticationBasicHeader(oConnection.HeadersRequest) then
                begin
                  oConnection.Authentication.Authenticated :=
                    oConnection.DoAuthenticationBasic
                    (DecodeAuthorizationBasic(oConnection.HeadersRequest));
                end
                // ... authenticate URL
                else if self.Authentication.URL.Enabled and
                  IsAuthenticationURLHeader(oConnection.HeadersRequest) then
                begin
                  oConnection.DoAuthenticationURL
                    (DecodeGETFullPath(oConnection.HeadersRequest));
                end
                // ... authenticate http
                else if self.Authentication.Session.Enabled and
                  IsAuthenticationSessionHeader(oConnection.HeadersRequest) then
                begin
                  oConnection.Authentication.Authenticated :=
                    DoAuthenticationSession(oConnection,
                    DecodeGETFullPath(oConnection.HeadersRequest));
                end
                // ... unknown authorization
                else if IsAuthenticationHeader(oConnection.HeadersRequest) then
                  oConnection.Authentication.Authenticated :=
                    DoUnknownAuthentication(oConnection)
                  // ... raise OnAuthentication event
                else
                begin
                  vAuthenticated := oConnection.Authentication.Authenticated;
                  if Assigned(FOnAuthentication) then
                  begin
                    FOnAuthentication(oConnection, '', '', vAuthenticated);
                    oConnection.Authentication.Authenticated := vAuthenticated
                  end;
                end;
            // ... if not authenticated then close
            if not(oConnection.Authentication.Authenticated) and
              (self.Authentication.AllowNonAuth = False) then
            begin
              if oConnection.FAuthenticationCloseCode = CS_CLOSE_NORMAL then
              // send custom close code after Handshake
              begin
                oConnection.HeadersRequest.Clear;
                oConnection.TCPConnection.IOHandler.InputBuffer.Clear;
                raise TsgcWSException.Create(S_AUTHENTICATION_DENIED);
              end;
            end;
          end;
          // ... read websocket
          oConnection.TCPConnection.IOHandler.InputBuffer.Clear;
          oConnection.ReadData(True);
        end
        else
        begin
          // ... response built-in authentication
          if IsRequestAuthenticationSessionHeader(oConnection.HeadersRequest)
            and self.Authentication.Enabled and self.Authentication.Session.Enabled
          then
          begin
            oConnection.DoAuthSessionResponse
              (DecodeGETFullPath(oConnection.HeadersRequest));
            oConnection.HeadersRequest.Clear;
            oConnection.TCPConnection.IOHandler.Capture
              (oConnection.HeadersRequest, '');
            DoContextFree(AContext);
          end
          // ... xhr
          else if IsXHRHeader(oConnection.HeadersRequest) then
          begin
            oConnection.Transport := trpHTTP;
            oConnection.HeadersRequest.Clear;
            oConnection.TCPConnection.IOHandler.Capture
              (oConnection.HeadersRequest, '');
            DoXHR(oConnection);
            DoContextFree(AContext);
          end
          // ... http protocol
          else if IsHTTPProtocolHeader(oConnection.HeadersRequest) then
          begin
            oConnection.Transport := trpHTTP;
            oConnection.HeadersRequest.Clear;
            oConnection.TCPConnection.IOHandler.Capture
              (oConnection.HeadersRequest, '');
            DoHTTPProtocol(oConnection);
            DoContextFree(AContext);
          end
          // ... OAuth2
{$IFDEF SGC_OAUTH_SERVER}
          else if SSL and self.Authentication.Enabled and
            Assigned(self.Authentication.OAuth.OAuth2) and
            IsHTTPHeader(oConnection.HeadersRequest) and
            self.Authentication.OAuth.OAuth2.DoProcessHTTP(oConnection,
            oConnection.HeadersRequest) then
          begin
            // ... free connection
            DoContextFree(AContext);
          end
{$ENDIF}
          else
{$IFDEF SGC_HTTP2}
            // ... http/2 protocol
            if HTTP2Options.Enabled and
              IsHTTP2Header(AContext.Connection.IOHandler.InputBuffer.AsString)
            then
            begin
              DoStartHTTP2(oConnection);
              oConnection.TCPConnection.IOHandler.InputBuffer.Clear;
            end
            else
{$ENDIF}
              // ... response built-in javascript libraries
              if DoBuiltInLibraries(oConnection,
                DecodeGETHeader(oConnection.HeadersRequest), False) then
              begin
                oConnection.HeadersRequest.Clear;
                oConnection.TCPConnection.IOHandler.Capture
                  (oConnection.HeadersRequest, '');
                DoContextFree(AContext);
              end
              // ... flash fallback
              else if IsRequestFlashPolicy(oConnection.HeadersRequest) then
              begin
                oConnection.Transport := trpFlash;
                oConnection.HeadersRequest.Clear;
                oConnection.TCPConnection.IOHandler.InputBuffer.Clear;
                if FallBack.Flash.Enabled then
                begin
                  oConnection.TCPConnection.IOHandler.WriteLn
                    ('<cross-domain-policy>' + '<allow-access-from domain="' +
                    FallBack.Flash.Domain + '" to-ports="' +
                    FallBack.Flash.Ports + '" />' + '</cross-domain-policy>'
                    + chr(0));
                end
                else
                  DoContextFree(AContext);
              end
              // ... server sent events
              else if IsSSEHeader(oConnection.HeadersRequest) then
              begin
                oConnection.Transport := trpSSE;
                oConnection.HeadersRequest.Clear;
                oConnection.TCPConnection.IOHandler.Capture
                  (oConnection.HeadersRequest, '');
                if FallBack.ServerSentEvents.Enabled then
                begin
                  oConnection.DoProtocolSSE;
                  DoNotifyConnect(oConnection);
                  oConnection.DoWriteData_SSE
                    ('retry: ' +
                    IntToStr(FallBack.ServerSentEvents.Retry * 1000));
                  oConnection.DoWriteData_SSE(oConnection.Guid);
                  oConnection.DoWriteData_SSE(oConnection.Guid);
                  oConnection.FEnabled := True;
                end
                else
                  DoContextFree(AContext);
              end
              // ... response other http
              else if IsHTTPHeader(oConnection.HeadersRequest) then
              begin
                oConnection.Transport := trpHTTP;
                DoExecuteHTTP(AContext);
                DoContextFree(AContext);
              end
              else if DoUnknownProtocol(oConnection) then
              begin
                oConnection.FEnabled := True;
                OnClientUpdateEvent(oConnection, 1);
                oConnection.ReadData;
              end
              else
                DoContextFree(AContext);
        end;
      end
      // ... websocket data
      else
        oConnection.ReadData(True);

    Except
      On E: EIdSilentException do
        raise;
      On E: Exception do
        DoError(oConnection, E.Message);
    end;
  end
  else
  begin
    // ... disconnect non-websocket connections
    if not oConnection.Enabled then
    begin
      if ReadEmptySource > 0 then
      begin
        oConnection.FReadCount := oConnection.FReadCount + 1;
        if oConnection.FReadCount >= ReadEmptySource then
        begin
          AContext.{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF} := nil;
          DoTCPDisconnect(oConnection);
          sgcFree(oConnection);
        end;
      end
      else
        sleep(1); // ... prevent 100% cpu usage
    end;
  end;
end;

procedure TsgcWSHTTPServer.OnServerParseAuthenticationEvent
  (AContext: TIdContext; const AAuthType, AAuthData: String;
  var VUsername, VPassword: String; var VHandled: Boolean);
var
  oConnection: TsgcWSConnection;
begin
  oConnection := TsgcWSConnection
    (AContext.{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF});

  // ... oauth2
{$IFDEF SGC_OAUTH_SERVER}
  if Assigned(self.Authentication.OAuth.OAuth2) and
    (UpperCase(AAuthType) = 'BEARER') then
  begin
    TsgcWSConnectionServer(oConnection).Authentication.AuthType := authOAuth2;
    if not self.Authentication.OAuth.OAuth2.IsOAuth2TokenValid(oConnection,
      AAuthData) then
      OnOAuth2ResponseErrorEvent(oConnection)
    else
    begin
      TsgcWSConnectionServer(oConnection).Authentication.Authenticated := True;
      VHandled := True;
    end;
    exit;
  end;
{$ENDIF}
  // ... jwt
{$IFDEF SGC_JWT_SERVER}
  if Assigned(self.Authentication.JWT.JWT) and (UpperCase(AAuthType) = 'BEARER')
  then
  begin
    TsgcWSConnectionServer(oConnection).Authentication.AuthType := authJWT;
    if not self.Authentication.JWT.JWT.IsJWTTokenValid(oConnection, AAuthData)
    then
      OnJWTResponseErrorEvent(oConnection)
    else
    begin
      TsgcWSConnectionServer(oConnection).Authentication.Authenticated := True;
      VHandled := True;
    end;
    exit;
  end;
{$ENDIF}
  if Assigned(FOnUnknownAuthentication) then
    FOnUnknownAuthentication(oConnection, AAuthType, AAuthData, VUsername,
      VPassword, VHandled);
end;

procedure TsgcWSHTTPServer.OnSessionEndEvent(Sender: TIdHTTPSession);
begin
  if Assigned(FOnSessionEnd) then
    FOnSessionEnd(Sender);
end;

procedure TsgcWSHTTPServer.OnSessionStartEvent(Sender: TIdHTTPSession);
begin
  if Assigned(FOnSessionStart) then
    FOnSessionStart(Sender);
end;

procedure TsgcWSHTTPServer.SetAutoStartSession(const Value: Boolean);
begin
  FHTTPServer.AutoStartSession := Value;
end;

procedure TsgcWSServer_Indy_Base.DoStartHTTP2(const aConnection
  : TsgcWSConnectionServer);
{$IFDEF SGC_HTTP2}
var
  vBuffer: TIdBytes;
{$ENDIF}
begin
{$IFDEF SGC_HTTP2}
  aConnection.Transport := trpHTTP2;
  aConnection.FNotifyConnection := HTTP2Options.Events.OnConnect;
  aConnection.FNotifyDisconnection := HTTP2Options.Events.OnDisconnect;
  aConnection.DoHTTP2Start;
  // ... if buffer has more than one message, read the next message
  if aConnection.TCPConnection.IOHandler.InputBuffer.Size > 24 then
  begin
    SetLength(vBuffer, aConnection.TCPConnection.IOHandler.InputBuffer.
      Size - 24);
    aConnection.TCPConnection.IOHandler.InputBuffer.ExtractToBytes(vBuffer,
      Length(vBuffer), False, 24);
    aConnection.TCPConnection.IOHandler.InputBuffer.Clear;
    aConnection.TCPConnection.IOHandler.InputBuffer.write(vBuffer);
    aConnection.ReadData(True);
  end;
{$ENDIF}
end;

procedure TsgcWSServer_Indy_Base.DoContextFree(AContext: TIdContext);
var
  oConnection: TsgcWSConnectionServer;
begin
  oConnection := TsgcWSConnectionServer
    (AContext.{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF});

  if Assigned(oConnection) then
  begin
    oConnection.TCPConnection.IOHandler.InputBuffer.Clear;
    AContext.{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF} := nil;
    DoTCPDisconnect(oConnection);
    sgcFree(oConnection);
  end;
end;

procedure TsgcWSServer_Indy_Base.DoWatchDogMonitor;
var
  oClient: TsgcWSClient;
begin
  if FWatchDogMonitorSecret = '' then
    exit;

  oClient := TsgcWSClient.Create(nil);
  Try
    TsgcWSClient_Hack(oClient).NotifyEvents := neNoSync;
    if Bindings.Count > 0 then
      oClient.Host := Bindings[0].IP;
    if (oClient.Host = '0.0.0.0') or (oClient.Host = '0:0:0:0:0:0:0:0') or
      (oClient.Host = '') then
      oClient.Host := '127.0.0.1';
    if SSL then
      oClient.Port := SSLOptions.Port
    else
      oClient.Port := Port;
    oClient.TLSOptions.OpenSSL_Options.APIVersion :=
      SSLOptions.OpenSSL_Options.APIVersion;
    oClient.TLSOptions.Version := SSLOptions.Version;
    oClient.TLS := SSL;
    oClient.Options.Parameters := '/' + FWatchDogMonitorSecret;
    if not oClient.Connect(WatchDog.Monitor.TimeOut * 1000) then
      ReStart;
  Finally
    sgcFree(oClient);
  End;
end;

function TsgcWSServer_Indy_Base.GetThrottle: TsgcWSThrottle;
begin
  if not Assigned(FThrottle) then
    FThrottle := TsgcWSThrottle.Create;
  Result := FThrottle;
end;

procedure TsgcWSServer_Indy_Base.OnHTTP2ReadCreatePostStreamEvent
  (Sender: TObject; const aHeaders: TStrings; var Stream: TStream);
begin
  // not implemented
end;

procedure TsgcWSServer_Indy_Base.OnHTTP2ReadDonePostStreamEvent(Sender: TObject;
  const aHeaders: TStrings; const aStream: TStream);
begin
  // not implemented
end;

procedure TsgcWSServer_Indy_Base.OnHTTP2RequestEvent(const aConnection
  : TsgcWSConnection; const aHeaders: TStringList; const aBytes: TBytes;
  aStreamIdentifierRequest, aStreamIdentifierPush: Integer;
  const aPostStream: TStream);
begin
  // not implemented
end;

procedure TsgcWSServer_Indy_Base.OnHTTPUploadAfterSaveFileEvent(Sender:
    TObject; const aFileName, aFilePath: string);
begin
  if Assigned(FOnHTTPUploadAfterSaveFile) then
    FOnHTTPUploadAfterSaveFile(self, aFileName, aFilePath);
end;

procedure TsgcWSServer_Indy_Base.OnHTTPUploadBeforeSaveFileEvent(Sender:
    TObject; var aFileName, aFilePath: string);
begin
  if Assigned(FOnHTTPUploadBeforeSaveFile) then
    FOnHTTPUploadBeforeSaveFile(self, aFileName, aFilePath);
end;

procedure TsgcWSServer_Indy_Base.OnJWTResponseErrorEvent
  (aConnection: TsgcWSConnection);
begin
  TsgcWSConnectionServer(aConnection).DoHTTPError(401, 'Unauthorized');
end;

procedure TsgcWSServer_Indy_Base.OnOAuth2AuthenticationEvent
  (aConnection: TsgcWSConnection; aUser, aPassword: String;
  var Authenticated: Boolean);
begin
  if Assigned(FOnAuthentication) then
    FOnAuthentication(aConnection, aUser, aPassword, Authenticated);
end;

procedure TsgcWSServer_Indy_Base.OnOAuth2ResponseEvent
  (aConnection: TsgcWSConnection; aContent, aContentType: string);
begin
  TsgcWSConnectionServer(aConnection).DoHTTPResponse(aContent, aContentType);
end;

procedure TsgcWSServer_Indy_Base.OnOAuth2ResponseErrorEvent
  (aConnection: TsgcWSConnection);
begin
  TsgcWSConnectionServer(aConnection).DoHTTPError(401, 'Unauthorized');
end;

procedure TsgcWSServer_Indy_Base.OnServerExecuteEvent(AContext: TIdContext);
begin
  // -->start sgc_trial
{$IFDEF SGC_TRIAL}
  if (Now > EncodeDate(2022, 7, 1)) or (Now < EncodeDate(2022, 1, 1)) then
    sleep(StrToInt(FormatDateTime('sszzz', Now)));
{$ENDIF}
  // <--end sgc_trial
end;

procedure TsgcWSServer_Indy_Base.ReStart;
begin
  TServerThread.ReStart(self);
end;

procedure TsgcWSServer_Indy_Base.SetHTTP2Options(const Value
  : TsgcWSHTTP2Server_Options);
begin
  if Assigned(FHTTP2Options) then
    FHTTP2Options.Assign(Value);
end;

procedure TsgcWSServer_Indy_Base.SetHTTPUploadFiles
  (const Value: TsgcHTTPUploadFilesServer);
begin
  if Assigned(FHTTPUploadFiles) then
    FHTTPUploadFiles.Assign(Value);
end;

procedure TsgcWSServer_Indy_Base.SetThrottle(const Value: TsgcWSThrottle);
begin
  Throttle.Assign(Value);
end;

function TsgcWSHTTPServer.DoBeforeOnCommand(aConnection: TsgcWSConnection;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo)
  : Boolean;
var
  oResponse: TsgcHTTPResponse;
  vContent: string;
begin
  Result := True;

  if Authentication.Enabled then
  begin
    // ... oauth2
    if SSL and Assigned(self.Authentication.OAuth.OAuth2) then
    begin
      if not((TsgcWSConnectionServer(aConnection)
        .Authentication.AuthType = authOAuth2) and
        (TsgcWSConnectionServer(aConnection)
        .Authentication.Authenticated = True)) then
      begin
        OnOAuth2ResponseErrorEvent(aConnection);
        aConnection.Disconnect;
        Result := False;
        exit;
      end;
    end;

    // ... JWT
{$IFDEF SGC_JWT_SERVER}
    if Assigned(self.Authentication.JWT.JWT) then
    begin
      if not((TsgcWSConnectionServer(aConnection)
        .Authentication.AuthType = authJWT) and
        (TsgcWSConnectionServer(aConnection)
        .Authentication.Authenticated = True)) then
      begin
        OnJWTResponseErrorEvent(aConnection);
        aConnection.Disconnect;
        Result := False;
        exit;
      end;
    end;
{$ENDIF}
    // ... basic authentication
    if Authentication.Basic.Enabled then
    begin
      if not DoHTTPBasicAuthentication(aConnection, ARequestInfo, AResponseInfo)
      then
      begin
        aConnection.Disconnect;
        Result := False;
        exit;
      end;
    end;
  end;

  // ... API
  oResponse := TsgcHTTPResponse.Create;
  Try
    vContent := '';
    if Assigned(ARequestInfo.PostStream) then
      vContent := ReadStringFromStream(ARequestInfo.PostStream, -1);
    if DoHTTPRequestAPI(ARequestInfo.Command, ARequestInfo.Document, ARequestInfo.QueryParams, vContent, oResponse) then
    begin
      AResponseInfo.ContentText := oResponse.Content;
      AResponseInfo.ResponseNo := oResponse.Code;
      AResponseInfo.ContentType := oResponse.ContentType;

      DoResponseAfterCommand(TsgcWSConnectionServer(aConnection), AResponseInfo);
      Result := False;
      exit;
    end;
  Finally
    sgcFree(oResponse);
  End;
end;

function TsgcWSHTTPServer.GetCustomHeadersFromRaw
  (aRawHeaders: TIdHeaderList): string;
const
  oProcessedHeaders: Array [0 .. 21] of String = ('Cache-control', 'Connection',
    'Content-Version', 'Content-Disposition', 'Content-Encoding',
    'Content-Language', 'Content-Type', 'Content-Length', 'Content-Range',
    'Date', 'Last-Modified', 'Expires', 'ETag', 'Pragma', 'Transfer-Encoding',
    'Location', 'Server', 'Proxy-Connection', 'WWW-Authenticate',
    'Proxy-Authenticate', 'Accept-Patch', 'Accept-Ranges');
var
  i: Integer;
  j: Integer;
  oList: TStringList;
  vFound: Boolean;
begin
  oList := TStringList.Create;
  Try
    for i := 0 to aRawHeaders.Count - 1 do
    begin
      vFound := False;
      for j := Low(oProcessedHeaders) to High(oProcessedHeaders) do
      begin
        if LowerCase(aRawHeaders.Names[i]) = LowerCase(oProcessedHeaders[j])
        then
        begin
          vFound := True;
          break;
        end;
      end;

      if not vFound then
        oList.Add(aRawHeaders[i]);
    end;

    Result := oList.Text;
  Finally
    sgcFree(oList);
  End;
end;

function TsgcWSHTTPServer.DoForwardHTTP(const aConnection: TsgcWSConnection;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo)
  : Boolean;
var
  oHTTP: TsgcIdHTTP;
  oStream: TMemoryStream;
begin
  Result := False;

  if Assigned(FOnBeforeForwardHTTP) then
  begin
    TsgcWSConnectionServer(aConnection).Forward_HTTP.Document :=
      ARequestInfo.Document;
    FOnBeforeForwardHTTP(aConnection, ARequestInfo,
      TsgcWSConnectionServer(aConnection).Forward_HTTP);

    // ... process forward
    Result := TsgcWSConnectionServer(aConnection).Forward_HTTP.Enabled;
    if Result then
    begin
      oHTTP := TsgcIdHTTP.Create(nil);
      Try
        oHTTP.TLSOptions.Assign(TsgcWSConnectionServer(aConnection)
          .Forward_HTTP.TLSOptions);
        // ... raw headers
        oHTTP.Request.RawHeaders.Text := ARequestInfo.RawHeaders.Text;
        TIdEntityHeaderInfo_Hack(oHTTP.Request).ProcessHeaders;
{$IFDEF INDY10_5_7}
        oHTTP.Request.TransferEncoding := ''; // avoid chunked
{$ENDIF}
        // ... custom headers
        oHTTP.Request.CustomHeaders.Text :=
          GetCustomHeadersFromRaw(ARequestInfo.RawHeaders);
        // ... forward headers
        oHTTP.Request.CustomHeaders.Add('X-Forwarded-For: ' + aConnection.IP);
        oHTTP.Request.CustomHeaders.Add('X-Forwarded-Host: ' +
          ARequestInfo.Host);
        // ... forward http
        oStream := TMemoryStream.Create;
        Try
          Try
            case ARequestInfo.CommandType of
              hcHEAD:
                oHTTP.Head(TsgcWSConnectionServer(aConnection)
                  .Forward_HTTP.GetForwardURL);
              hcGET:
                oHTTP.Get(TsgcWSConnectionServer(aConnection)
                  .Forward_HTTP.GetForwardURL, oStream);
              hcPOST:
                oHTTP.Post(TsgcWSConnectionServer(aConnection)
                  .Forward_HTTP.GetForwardURL, ARequestInfo.PostStream,
                  oStream);
              hcDELETE:
{$IFDEF INDY10_5_5}
                oHTTP.Delete(TsgcWSConnectionServer(aConnection)
                  .Forward_HTTP.GetForwardURL{$IFDEF INDY10_6_0_5122},
                  oStream{$ENDIF});
{$ELSE}
                raise Exception.Create('Delete method is not supported.');
{$ENDIF}
              hcPUT:
                oHTTP.Put(TsgcWSConnectionServer(aConnection)
                  .Forward_HTTP.GetForwardURL, ARequestInfo.PostStream,
                  oStream);
              hcTRACE:
                oHTTP.Trace(TsgcWSConnectionServer(aConnection)
                  .Forward_HTTP.GetForwardURL, oStream);
              hcOPTION:
                oHTTP.Options(TsgcWSConnectionServer(aConnection)
                  .Forward_HTTP.GetForwardURL{$IFDEF INDY10_6_0_5122},
                  oStream{$ENDIF});
            end;
            // ... raw headers
            AResponseInfo.RawHeaders.Text := oHTTP.Response.RawHeaders.Text;
            TIdEntityHeaderInfo_Hack(AResponseInfo).ProcessHeaders;
{$IFDEF INDY10_5_7}
            AResponseInfo.TransferEncoding := ''; // avoid chunked
{$ENDIF}
            // ... custom headers
            AResponseInfo.CustomHeaders.Text :=
              GetCustomHeadersFromRaw(oHTTP.Response.RawHeaders);
            // ... response code
            AResponseInfo.ResponseNo := oHTTP.ResponseCode;
            // ... response stream
            if oStream.Size > 0 then
              AResponseInfo.ContentStream := oStream;
          Except
            On E: Exception do
            begin
              if oHTTP.ResponseCode > 0 then
                AResponseInfo.ResponseNo := oHTTP.ResponseCode
              else
                AResponseInfo.ResponseNo := 500;
              if Assigned(FOnAfterForwardHTTP) then
                FOnAfterForwardHTTP(aConnection, ARequestInfo,
                  AResponseInfo, E);
              exit;
            end;
          end;

          if Assigned(FOnAfterForwardHTTP) then
            FOnAfterForwardHTTP(aConnection, ARequestInfo, AResponseInfo, nil);
        Finally
          if oStream.Size = 0 then
            sgcFree(oStream);
        End;
      Finally
        sgcFree(oHTTP);
      End;
    end;
  end;
end;

{$IFDEF SGC_HTTP2}

function TsgcWSHTTPServer.DoHTTP2Authentication(const aConnection
  : TsgcWSConnection; const aRequest: TIdHTTPRequestInfo;
  const aHeaders: TStringList): Boolean;
var
  vAuthorization, vAuthType: String;
begin
  Result := True;
  // ... authentication
  if Authentication.Enabled then
  begin
    // ... OAuth2
    if Assigned(Authentication.OAuth.OAuth2) then
    begin
      Result := False;
      if not Authentication.OAuth.OAuth2.DoProcessHTTP
        (TsgcWSConnectionServer(aConnection),
        TsgcWSConnectionServer(aConnection).HeadersRequest) then
      begin
        if not Authentication.OAuth.OAuth2.IsOAuth2TokenValid(aConnection,
          TsgcWSConnectionServer(aConnection).HeadersRequest) then
          raise EIdHTTPUnsupportedAuthorisationScheme.Create
            (RSHTTPUnsupportedAuthorisationScheme)
        else
        begin
          Result := True;
          exit;
        end;
      end
      else
        exit;
    end;
    // ... JWT
{$IFDEF SGC_JWT_SERVER}
    if Assigned(Authentication.JWT.JWT) then
    begin
      if not Authentication.JWT.JWT.IsJWTTokenValid(aConnection,
        TsgcWSConnectionServer(aConnection).HeadersRequest) then
        raise EIdHTTPUnsupportedAuthorisationScheme.Create
          (RSHTTPUnsupportedAuthorisationScheme)
      else
      begin
        Result := True;
        exit;
      end;
    end;
{$ENDIF}
  end;
  // ... authorization
  // ... read user/password doesn't require authentication is enabled
  vAuthorization := aHeaders.Values['authorization'];
  if vAuthorization <> '' then
  begin
    vAuthType := Fetch(vAuthorization, ' ');
    TIdHTTPRequestInfo_Hack(aRequest).FAuthExists :=
      TIdCustomHTTPServer_Hack(FHTTPServer).DoParseAuthentication
      (TsgcWSConnectionServer(aConnection).FContext, vAuthType, vAuthorization,
      TIdHTTPRequestInfo_Hack(aRequest).FAuthUsername,
      TIdHTTPRequestInfo_Hack(aRequest).FAuthPassword);
    if Authentication.Enabled then
    begin
      if not TIdHTTPRequestInfo_Hack(aRequest).FAuthExists then
      begin
        raise EIdHTTPUnsupportedAuthorisationScheme.Create
          (RSHTTPUnsupportedAuthorisationScheme);
      end;
    end;
  end;
end;

function TsgcWSHTTPServer.DoHTTP2BeforeCommandGet(const aConnection
  : TsgcWSConnection; const aRequest: TIdHTTPRequestInfo;
  const aResponse: TIdHTTPResponseInfo; aStreamIdentifier: Integer): Boolean;
begin
  Result := True;

  if IsXHRHeader(TsgcWSConnectionServer(aConnection).HeadersRequest) then
  begin
    TsgcWSConnectionServer(aConnection).Transport := trpHTTP;
    TsgcWSConnectionServer(aConnection).HeadersRequest.Clear;
    TsgcWSConnectionServer(aConnection).TCPConnection.IOHandler.Capture
      (TsgcWSConnectionServer(aConnection).HeadersRequest, '');
    DoXHR(TsgcWSConnectionServer(aConnection), aRequest.PostStream, aStreamIdentifier);
    Result := False;
  end
  else if IsHTTPProtocolHeader(TsgcWSConnectionServer(aConnection)
    .HeadersRequest) then
  begin
    DoHTTPProtocol(TsgcWSConnectionServer(aConnection), aRequest.PostStream, aStreamIdentifier);
    Result := False;
  end;
end;

procedure TsgcWSHTTPServer.DoHTTP2CommandGet(const aConnection
  : TsgcWSConnection; const aRequest: TIdHTTPRequestInfo;
  const aResponse: TIdHTTPResponseInfo; aStreamIdentifierRequest,
  aStreamIdentifierPush: Integer);
begin
  if Assigned(FOnCommandGet) then
  begin
    FOnCommandGet(TsgcWSConnectionServer(aConnection).FContext, aRequest,
      aResponse);
    DoHTTP2Response(aConnection, aRequest, aResponse, aStreamIdentifierRequest,
      aStreamIdentifierPush);
  end;
end;

procedure TsgcWSHTTPServer.DoHTTP2CommandOther(const aConnection
  : TsgcWSConnection; const aRequest: TIdHTTPRequestInfo;
  const aResponse: TIdHTTPResponseInfo; aStreamIdentifierRequest: Integer);
begin
  if Assigned(FOnCommandOther) then
  begin
    FOnCommandOther(TsgcWSConnectionServer(aConnection).FContext, aRequest,
      aResponse);
    DoHTTP2Response(aConnection, aRequest, aResponse, aStreamIdentifierRequest);
  end;
end;

function TsgcWSHTTPServer.DoHTTP2DocumentRoot(const aConnection
  : TsgcWSConnection; const aRequest: TIdHTTPRequestInfo;
  const aResponse: TIdHTTPResponseInfo): Boolean;
var
  oStream: TMemoryStream;
  vDocument: string;
begin
  Result := False;
  if DocumentRoot <> '' then
  begin
{$IFDEF LINUX64}
    vDocument := DocumentRoot + aRequest.Document;
{$ELSE}
    vDocument := StringReplace(DocumentRoot + aRequest.Document, '/', '\',
      [rfReplaceAll]);
{$ENDIF}
    if FileExists(vDocument) then
    begin
      aResponse.Charset := Charset;
      aResponse.ContentType := HTTPServer.MIMETable.GetFileMIMEType(vDocument);
      oStream := TMemoryStream.Create;
      Try
        oStream.LoadFromFile(vDocument);
        oStream.Position := 0;
      Except
        sgcFree(oStream);
        raise;
      End;
      aResponse.ContentStream := oStream;
      Result := True;
    end;
  end;
end;

function TsgcWSHTTPServer.DoHTTP2Request(const aConnection: TsgcWSConnection;
  const aRequest: TIdHTTPRequestInfo; const aHeaders: TStringList;
  const aBytes: TBytes): Boolean;

  procedure ReadCookiesFromRequestHeader;
  var
    oCookies: TStringList;
  begin
    oCookies := TStringList.Create;
    try
      aRequest.RawHeaders.Extract('cookie', oCookies);
      aRequest.Cookies.AddClientCookies(oCookies);
    finally
      sgcFree(oCookies);
    end;
  end;

var
  i: Integer;
  oURI: TIdURI;
  oEncoding: IIdTextEncoding;
  oStream: TsgcStringStream;
begin
  Result := True;
  // ... assign params
  TsgcWSConnectionServer(aConnection).HeadersRequest.Text := aHeaders.Text;
  // ... byte stream
  if Length(aBytes) > 0 then
  begin
    oStream := TsgcStringStream.Create(aBytes);
    aRequest.PostStream := oStream;
  end;
  // ... indy parsing
  TIdHTTPRequestInfo_Hack(aRequest).FVersion := 'HTTP/2';
  TIdHTTPRequestInfo_Hack(aRequest).FVersionMajor := 2;
  TIdHTTPRequestInfo_Hack(aRequest).FVersionMinor := 0;
  aRequest.RawHeaders.NameValueSeparator := '=';
  aRequest.RawHeaders.Text := aHeaders.Text;
  TIdHTTPRequestInfo_Hack(aRequest).ProcessHeaders;
  // ... custom parsing
  TIdHTTPRequestInfo_Hack(aRequest).FRemoteIP := aConnection.IP;

  TIdHTTPRequestInfo_Hack(aRequest).FRawHTTPCommand :=
    aHeaders.Values[':method'] + ' ' + aHeaders.Values[':path'] + ' HTTP/2';
  TIdHTTPRequestInfo_Hack(aRequest).FQueryParams := aHeaders.Values[':path'];
  TIdHTTPRequestInfo_Hack(aRequest).FURI :=
    Fetch(TIdHTTPRequestInfo_Hack(aRequest).FQueryParams, '?');
  oURI := TIdURI.Create(aRequest.URI);
  Try
    aRequest.Document := TIdURI.URLDecode(oURI.Path) +
      TIdURI.URLDecode(oURI.Document);
  Finally
    sgcFree(oURI);
  End;
  aRequest.Host := aHeaders.Values[':authority'];
  TIdHTTPRequestInfo_Hack(aRequest).FCommand := aHeaders.Values[':method'];
  TIdHTTPRequestInfo_Hack(aRequest).FCommandType := hcUnknown;
  for i := Low(HTTPRequestStrings) to High(HTTPRequestStrings) do
  begin
    if TextIsSame(aRequest.Command, HTTPRequestStrings[i]) then
    begin
      TIdHTTPRequestInfo_Hack(aRequest).FCommandType := THTTPCommandType(i);
      break;
    end;
  end;
  // ... post stream
  if aRequest.PostStream <> nil then
  begin
    if TextIsSame(aRequest.ContentType, 'application/x-www-form-urlencoded')
    then
    begin
      EnsureEncoding(oEncoding, enc8Bit);
      aRequest.FormParams := ReadStringFromStream(aRequest.PostStream, -1,
        oEncoding);
    end;
  end;
  // ... unparsed params
  aRequest.UnparsedParams := aRequest.FormParams;
  if Length(aRequest.QueryParams) > 0 then
  begin
    if Length(aRequest.UnparsedParams) = 0 then
      TIdHTTPRequestInfo_Hack(aRequest).FUnparsedParams := aRequest.QueryParams
    else
      TIdHTTPRequestInfo_Hack(aRequest).FUnparsedParams :=
        aRequest.UnparsedParams + '&' + aRequest.QueryParams;
  end;
  // ... decode and set params
  if TextIsSame(aRequest.ContentType, 'application/x-www-form-urlencoded') then
    TIdHTTPRequestInfo_Hack(aRequest).DecodeAndSetParams
      (aRequest.UnparsedParams)
  else
    TIdHTTPRequestInfo_Hack(aRequest).DecodeAndSetParams(aRequest.QueryParams);
  // ... Cookies
  ReadCookiesFromRequestHeader;
end;

procedure TsgcWSHTTPServer.DoHTTP2Response(const aConnection: TsgcWSConnection;
  const aRequest: TIdHTTPRequestInfo; const aResponse: TIdHTTPResponseInfo;
  aStreamIdentifierRequest: Integer; aStreamIdentifierPush: Integer = 0);
var
  oLinks: TStrings;
  vCharSet: string;
begin
  // ... from indy IdCustomHTTPServer
  if aResponse.AuthRealm <> '' then
  begin
    aResponse.ResponseNo := 401;
    if (Length(aResponse.ContentText) = 0) and
      (not Assigned(aResponse.ContentStream)) then
    begin
      aResponse.ContentType := 'text/html; charset=utf-8';
      aResponse.ContentText := '<HTML><BODY><B>' +
        IntToStr(aResponse.ResponseNo) + ' ' + aResponse.ResponseText +
        '</B></BODY></HTML>';
    end;
  end;

  if aResponse.ContentType = '' then
  begin
    if (aResponse.ContentText <> '') or Assigned(aResponse.ContentStream) then
    begin
      vCharSet := aResponse.Charset;
      if vCharSet = '' then
      begin
        vCharSet := 'ISO-8859-1';
      end;
      aResponse.ContentType := 'text/html; charset=' + vCharSet;
    end;
  end;

  if aResponse.Date <= 0 then
    aResponse.Date := Now;

  TIdHTTPRequestInfo_Hack(aResponse).SetHeaders;

  // ... push promise
  oLinks := nil;
  if TsgcWSConnectionServer(aConnection).HTTP2FrameWrite.Frame_Settings.EnablePush
  then
  begin
    if aRequest.CommandType = hcGET then
      oLinks := GetPushPromiseList.GetPushPromiseLinks(aRequest.Document);
  end;
  // ... send response
  TsgcWSConnectionServer(aConnection).DoHTTP2Response(aRequest, aResponse,
    oLinks, aStreamIdentifierRequest, aStreamIdentifierPush);
end;

function TsgcWSHTTPServer.GetPushPromiseList: TsgcHTTP2PushPromiseList;
begin
  if not Assigned(FPushPromiseList) then
    FPushPromiseList := TsgcHTTP2PushPromiseList.Create;
  Result := FPushPromiseList;
end;

procedure TsgcWSHTTPServer.OnHTTP2RequestEvent(const aConnection
  : TsgcWSConnection; const aHeaders: TStringList; const aBytes: TBytes;
  aStreamIdentifierRequest, aStreamIdentifierPush: Integer;
  const aPostStream: TStream);
var
  oRequest: TIdHTTPRequestInfo;
  oResponse: TIdHTTPResponseInfo;
  vContinue: Boolean;
begin
  inherited;
  if DoBuiltInLibraries(TsgcWSConnectionServer(aConnection),
    aHeaders.Values[':method'] + ' ' + aHeaders.Values[':path'] + ' HTTP/2',
    False, aStreamIdentifierRequest) then
    exit;

  oRequest := TIdHTTPRequestInfo.Create(nil);
  oResponse := TIdHTTPResponseInfo.Create(TIdCustomHTTPServer(CustomServer),
    oRequest, TsgcWSConnectionServer(aConnection).TCPConnection);
  Try
    if DoHTTP2Request(aConnection, oRequest, aHeaders, aBytes) then
    begin
      // ... forward
      if DoForwardHTTP(aConnection, oRequest, oResponse) then
      begin
        DoHTTP2Response(aConnection, oRequest, oResponse,
          aStreamIdentifierRequest, aStreamIdentifierPush);
        exit;
      end;

      // ... Authentication
      if not DoHTTP2Authentication(aConnection, oRequest, aHeaders) then
        exit;

      // ... Session management
      TIdCustomHTTPServer_Hack(FHTTPServer).GetSessionFromCookie
        (TsgcWSConnectionServer(aConnection).FContext, oRequest, oResponse,
        vContinue);
      if vContinue then
      begin
        // ... document root
        if DoHTTP2DocumentRoot(aConnection, oRequest, oResponse) then
        begin
          DoHTTP2Response(aConnection, oRequest, oResponse,
            aStreamIdentifierRequest, aStreamIdentifierPush);
          exit;
        end;

        // ... GET, POST, HEAD
        if oRequest.CommandType in [hcGET, hcPOST, hcHEAD] then
        begin
          if (oRequest.CommandType = hcPOST) and Assigned(aPostStream) then
            oRequest.PostStream := aPostStream;
          if DoHTTP2BeforeCommandGet(aConnection, oRequest, oResponse,
            aStreamIdentifierRequest) then
            DoHTTP2CommandGet(aConnection, oRequest, oResponse,
              aStreamIdentifierRequest, aStreamIdentifierPush);
        end
        // ... OTHER
        else
          DoHTTP2CommandOther(aConnection, oRequest, oResponse,
            aStreamIdentifierRequest);
      end;
    end;
  Finally
    sgcFree(oResponse);
    if Assigned(oRequest) then
      if Assigned(oRequest.PostStream) then
        oRequest.PostStream := nil;
    sgcFree(oRequest);
  End;
end;

procedure TsgcWSHTTPServer.OnHTTP2ReadCreatePostStreamEvent(Sender: TObject;
  const aHeaders: TStrings; var Stream: TStream);
var
  vContentLength: Int64;
  vFileName: string;
begin
  if (HTTPUploadFiles.StreamType = pstFileStream) then
  begin
    vContentLength := 0;
    if HTTPUploadFiles.MinSize > 0 then
      TryStrToInt64(aHeaders.Values['content-length'], vContentLength);

    if (HTTPUploadFiles.MinSize = 0) or
      (vContentLength >= HTTPUploadFiles.MinSize) then
    begin
      if sgcContainsText(aHeaders.Values['content-type'], 'multipart/form-data')
      then
      begin
        repeat
          vFileName := HTTPUploadFiles.SaveDirectory +
            FormatDateTime('yyyymmddhhnnsszzz', Now) + '.sgc_ps';
          if not FileExists(vFileName) then
            Stream := TsgcFileStream.Create(vFileName, fmCreate);
        until Stream <> nil;
      end;
    end;
  end;
end;

procedure TsgcWSHTTPServer.OnHTTP2ReadDonePostStreamEvent(Sender: TObject;
  const aHeaders: TStrings; const aStream: TStream);
var
  vFileName: string;
  oDecoder: TsgcHTTPMultipartFormDataDecoder;
begin
  if Assigned(aStream) then
  begin
    if (HTTPUploadFiles.StreamType = pstFileStream) and
      (aStream.ClassType = TsgcFileStream) then
    begin
      aStream.Position := 0;
      if HTTPUploadFiles.RemoveBoundaries then
      begin
        // ... extract files
        oDecoder := TsgcHTTPMultipartFormDataDecoder.Create(nil);
        Try
          oDecoder.OnBeforeSaveFile := OnHTTPUploadBeforeSaveFileEvent;
          oDecoder.OnAfterSaveFile := OnHTTPUploadAfterSaveFileEvent;
          oDecoder.ExtractFiles(aStream,
            GetBoundaryFromContentType(aHeaders.Values['content-type']),
            HTTPUploadFiles.SaveDirectory);
        Finally
          sgcFree(oDecoder);
        End;
        // ... delete post stream
        Try
          vFileName := TsgcFileStream(aStream).Filename;
        Finally
{$IFDEF NEXTGEN}
          aStream.DisposeOf;
{$ELSE}
          aStream.Free;
{$ENDIF}
          SysUtils.DeleteFile(vFileName);
        end;
      end
      else
{$IFDEF NEXTGEN}
        aStream.DisposeOf;
{$ELSE}
        aStream.Free;
{$ENDIF}
    end;
  end;
end;

procedure TsgcWSHTTPServer.PushPromiseAddPreLoadLinks(const aPathMatch: String;
  const aLinks: TStrings);
begin
  GetPushPromiseList.AddPushPromise(aPathMatch, aLinks);
end;

procedure TsgcWSHTTPServer.PushPromiseRemovePreLoadLinks
  (const aPathMatch: String);
begin
  GetPushPromiseList.DeleteItem(aPathMatch);
end;

{$ENDIF}

procedure TsgcWSHTTPServer.OnCreatePostStreamEvent(AContext: TIdContext;
  aHeaders: TIdHeaderList; var VPostStream: TStream);
var
  vContentLength: Int64;
  vFileName: string;
begin
  if (HTTPUploadFiles.StreamType = pstFileStream) then
  begin
    vContentLength := 0;
    if HTTPUploadFiles.MinSize > 0 then
      TryStrToInt64(aHeaders.Values['Content-Length'], vContentLength);

    if (HTTPUploadFiles.MinSize = 0) or
      (vContentLength >= HTTPUploadFiles.MinSize) then
    begin
      if sgcContainsText(aHeaders.Values['Content-Type'], 'multipart/form-data')
      then
      begin
        repeat
          vFileName := HTTPUploadFiles.SaveDirectory +
            FormatDateTime('yyyymmddhhnnsszzz', Now) + '.sgc_ps';
          if not FileExists(vFileName) then
            VPostStream := TsgcFileStream.Create(vFileName, fmCreate);
        until VPostStream <> nil;
      end;
    end;
  end;
end;

procedure TsgcWSHTTPServer.OnDoneWithPostStreamEvent(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; var VCanFree: Boolean);
var
  oDecoder: TsgcHTTPMultipartFormDataDecoder;
  vFileName: string;
  oStream: TStream;
begin
  oStream := ARequestInfo.PostStream;
  if Assigned(oStream) then
  begin
    if (HTTPUploadFiles.StreamType = pstFileStream) and
      (oStream.ClassType = TsgcFileStream) then
    begin
      if HTTPUploadFiles.RemoveBoundaries then
      begin
        // ... extract files
        oDecoder := TsgcHTTPMultipartFormDataDecoder.Create(nil);
        Try
          oDecoder.OnBeforeSaveFile := OnHTTPUploadBeforeSaveFileEvent;
          oDecoder.OnAfterSaveFile := OnHTTPUploadAfterSaveFileEvent;
          oDecoder.ExtractFiles(oStream,
            GetBoundaryFromContentType(ARequestInfo.ContentType),
            HTTPUploadFiles.SaveDirectory);
        Finally
          sgcFree(oDecoder);
        End;
        // ... delete post stream
        Try
          vFileName := TsgcFileStream(oStream).Filename;
        Finally
          sgcFree(oStream);
          SysUtils.DeleteFile(vFileName);
        end;
        VCanFree := False;
      end;
    end;
  end;
end;


procedure TsgcWSHTTPServer.SetKeepAlive(const Value: Boolean);
begin
  FHTTPServer.KeepAlive := Value;
end;

procedure TsgcWSHTTPServer.SetParseParams(const Value: Boolean);
begin
  FHTTPServer.ParseParams := Value;
end;

procedure TsgcWSHTTPServer.SetSessionState(const Value: Boolean);
begin
  FHTTPServer.SessionState := Value;
end;

procedure TsgcWSHTTPServer.SetSessionTimeOut(const Value: Integer);
begin
  FHTTPServer.SessionTimeOut := Value;
end;

procedure TsgcWSThreadPool_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSThreadPool_Options then
  begin
    MaxThreads := TsgcWSThreadPool_Options(aSource).MaxThreads;
    PoolSize := TsgcWSThreadPool_Options(aSource).PoolSize;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcWSLoadBalancerServer_Options.Create;
begin
  inherited;
  FBindings := TStringList.Create;
  Enabled := False;
  Host := '';
  Port := 0;
  AutoRestart := 0;
  AutoRegisterBindings := False;
end;

destructor TsgcWSLoadBalancerServer_Options.Destroy;
begin
  sgcFree(FBindings);
  inherited;
end;

procedure TsgcWSLoadBalancerServer_Options.SetBindings
  (const Value: TStringList);
begin
  if Assigned(FBindings) then
    FBindings.Assign(Value);
end;

constructor TServerThread.Create(const aMethod: string;
  const aServer: TsgcWSServer_Indy_Base);
begin
  FMethod := aMethod;
  FServer := aServer;
  FreeOnTerminate := True;
  inherited Create(False);
end;

class procedure TServerThread.Start(const aServer: TsgcWSServer_Indy_Base);
begin
  Create(CS_START, aServer);
end;

class procedure TServerThread.Stop(const aServer: TsgcWSServer_Indy_Base);
begin
  Create(CS_STOP, aServer);
end;

procedure TServerThread.Execute;
begin
  inherited;
  if Assigned(FServer) then
  begin
    // ... Start
    if FMethod = CS_START then
    begin
      if FServer.DoAssignedCS then
      begin
        FServer.EnterCS;
        Try
          FServer.Active := True
        Finally
          FServer.LeaveCS;
        End;
      end;
    end
    // ... Stop
    else if FMethod = CS_STOP then
    begin
      if FServer.DoAssignedCS then
      begin
        FServer.EnterCS;
        Try
          FServer.Active := False;
        Finally
          FServer.LeaveCS;
        End;
      end;
    end
    // ... Restart
    else if FMethod = CS_RESTART then
    begin
      if FServer.DoAssignedCS then
      begin
        if FServer.Active then
          Stop(FServer);
        repeat
          sleep(1);
        until (not FServer.Active);
        Start(FServer);
      end;
    end;
    Terminate;
  end;
end;

class procedure TServerThread.ReStart(const aServer: TsgcWSServer_Indy_Base);
begin
  Create(CS_RESTART, aServer);
end;

constructor TsgcWSIOHandler_Options.Create;
begin
  inherited;
  FIOHandlerType := iohDefault;
  FIOCP := TsgcWSIOHandler_IOCP_Options.Create;
end;

destructor TsgcWSIOHandler_Options.Destroy;
begin
  sgcFree(FIOCP);
  inherited;
end;

procedure TsgcWSIOHandler_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSIOHandler_Options then
  begin
    IOHandlerType := TsgcWSIOHandler_Options(aSource).IOHandlerType;
    IOCP := TsgcWSIOHandler_Options(aSource).IOCP;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSIOHandler_Options.SetIOCP(const Value
  : TsgcWSIOHandler_IOCP_Options);
begin
  if Assigned(FIOCP) then
    FIOCP.Assign(Value);
end;

constructor TsgcWSIOHandler_IOCP_Options.Create;
begin
  inherited;
  FIOCPThreads := 8;
  FWorkOpThreads := 32;
  FTimeOut := 10000;
end;

procedure TsgcWSIOHandler_IOCP_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSIOHandler_IOCP_Options then
  begin
    WorkOpThreads := TsgcWSIOHandler_IOCP_Options(aSource).WorkOpThreads;
    IOCPThreads := TsgcWSIOHandler_IOCP_Options(aSource).IOCPThreads;
    TimeOut := TsgcWSIOHandler_IOCP_Options(aSource).TimeOut;
  end
  else
    inherited Assign(aSource);
end;

end.
