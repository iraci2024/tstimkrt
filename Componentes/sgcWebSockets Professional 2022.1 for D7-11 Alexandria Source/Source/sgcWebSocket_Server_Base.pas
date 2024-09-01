{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Server_Base;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils, StrUtils,
{$IFDEF NEXTGEN}System.Generics.Collections, {$ELSE}Contnrs, {$ENDIF}
  // sgc
{$IFDEF SGC_OAUTH_SERVER}sgcHTTP_OAuth2_Server, {$ENDIF}
{$IFDEF SGC_JWT_SERVER}sgcHTTP_JWT_Server, {$ENDIF}
{$IFDEF SGC_HTML}sgcWebSocket_Classes_HTML, {$ENDIF}
{$IFDEF SGC_HTTP2}sgcHTTP2_Types, {$ENDIF}
  sgcWebSocket_Classes, sgcWebSocket_Types,
  sgcWebSocket_Helpers, sgcWebSocket_Classes_Indy;

type
  TsgcWSServer_Base = class;
  TsgcWSConnectionServer_Base = class;

  TsgcWSAuthenticationEvent = procedure(Connection: TsgcWSConnection;
    aUser, aPassword: String; var Authenticated: Boolean) of object;
  TsgcWSHTTPUploadBeforeSaveFileEvent = procedure(Sender: TObject; var aFileName,
      aFilePath: string) of object;
  TsgcWSHTTPUploadAfterSaveFileEvent = procedure(Sender: TObject; const
      aFileName, aFilePath: string) of object;

  // TServerThread //
  TServerThread = class(TThread)
  private
    FServer: TsgcWSServer_Base;
    FMethod: string;
  protected
    procedure Execute; override;
  public
    constructor Create(const aMethod: string; const aServer: TsgcWSServer_Base);
    class procedure Start(const aServer: TsgcWSServer_Base);
    class procedure Stop(const aServer: TsgcWSServer_Base);
    class procedure ReStart(const aServer: TsgcWSServer_Base);
  end;

  TsgcWSBinding = class(TPersistent)
  private
    FHost: string;
    FIPVersion: TwsIPVersion;
    FParameters: String;
    FPort: Integer;
    FSSL: Boolean;
    FSSLCertStoreName: String;
    FSSLHash: String;
  public
    constructor Create;
    procedure Assign(aSource: TPersistent); override;
  public
    property Host: string read FHost write FHost;
    property IPVersion: TwsIPVersion read FIPVersion write FIPVersion;
    property Parameters: String read FParameters write FParameters;
    property Port: Integer read FPort write FPort;
    property SSL: Boolean read FSSL write FSSL;
    property SSLCertStoreName: String read FSSLCertStoreName
      write FSSLCertStoreName;
    property SSLHash: String read FSSLHash write FSSLHash;
  end;

  TsgcWSBindings = class(TList{$IFDEF NEXTGEN}<TsgcWSBinding>{$ENDIF})
  public
    procedure Clear; {$IFNDEF NEXTGEN}override; {$ENDIF}
    procedure NewBinding(const aHost: string; const aPort: Integer;
      const aParameters: string = ''; const aSSL: Boolean = False;
      const aSSLHash: String = ''; const aIPVersion: TwsIPVersion = ipV4;
      const aCertStoreName: String = ''); overload;
    procedure NewBinding(const aBinding: TsgcWSBinding); overload;
  public
    function GetURL(const aIndex: Integer): String;
  end;

  TsgcWSSSL_Options_Base = class(TPersistent)

  end;

  TsgcWSSecurity_Options = class(TPersistent)
  private
    FOriginsAllowed: String;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property OriginsAllowed: String read FOriginsAllowed write FOriginsAllowed;
  end;

  TsgcWSQueueServer_Options = class(TsgcWSQueueOptions)

  end;

  TsgcWSAuthenticationServer_Session = class(TsgcWSAuthentication_Session)

  end;

  TsgcWSAuthenticationServer_URL = class(TsgcWSAuthentication_URL)

  end;

  TsgcWSAuthenticationServer_Basic = class(TsgcWSAuthentication_Basic)

  end;

  TsgcWSAuthenticationServer_OAuth = class(TPersistent)
  private
    FOAuth2: {$IFDEF SGC_OAUTH_SERVER}TsgcHTTPComponentServer_OAuth2{$ELSE}TObject{$ENDIF};
  public
    constructor Create; virtual;
  published
    property OAuth2:
{$IFDEF SGC_OAUTH_SERVER}TsgcHTTPComponentServer_OAuth2{$ELSE}TObject{$ENDIF} read FOAuth2 write FOAuth2;
  end;

  TsgcWSAuthenticationServer_JWT = class(TPersistent)
  private
    FJWT: {$IFDEF SGC_JWT_SERVER}TsgcHTTPComponentServer_JWT{$ELSE}TObject{$ENDIF};
  public
    constructor Create; virtual;
  published
    property JWT:
{$IFDEF SGC_JWT_SERVER}TsgcHTTPComponentServer_JWT{$ELSE}TObject{$ENDIF} read FJWT write FJWT;
  end;

  // TsgcWSFallBack_Flash //
  TsgcWSFallBack_Flash = class(TPersistent)
  private
    FDomain: String;
    FEnabled: Boolean;
    FPorts: String;
  public
    constructor Create;
  published
    property Domain: String read FDomain write FDomain;
    property Enabled: Boolean read FEnabled write FEnabled;
    property Ports: String read FPorts write FPorts;
  end;

  TsgcHTTPUploadFilesServer = class(TPersistent)
  private
    FMinSize: Int64;
    FRemoveBoundaries: Boolean;
    FSaveDirectory: string;
    FStreamType: TwsPostStreamType;
    procedure SetStreamType(const Value: TwsPostStreamType);
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property MinSize: Int64 read FMinSize write FMinSize;
    property RemoveBoundaries: Boolean read FRemoveBoundaries
      write FRemoveBoundaries;
    property SaveDirectory: string read FSaveDirectory write FSaveDirectory;
    property StreamType: TwsPostStreamType read FStreamType write SetStreamType;
  end;

  TsgcHTTP2ServerSettings = class(TPersistent)
  private
    FEnableConnectProtocol: Boolean;
    FEnablePush: Boolean;
    FHeaderTableSize: Integer;
    FInitialWindowSize: Integer;
    FMaxConcurrentStreams: Integer;
    FMaxFrameSize: Integer;
    FMaxHeaderListSize: Integer;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
    property EnableConnectProtocol: Boolean read FEnableConnectProtocol
      write FEnableConnectProtocol;
  published
    property EnablePush: Boolean read FEnablePush write FEnablePush;
    property HeaderTableSize: Integer read FHeaderTableSize
      write FHeaderTableSize;
    property InitialWindowSize: Integer read FInitialWindowSize
      write FInitialWindowSize;
    property MaxConcurrentStreams: Integer read FMaxConcurrentStreams
      write FMaxConcurrentStreams;
    property MaxFrameSize: Integer read FMaxFrameSize write FMaxFrameSize;
    property MaxHeaderListSize: Integer read FMaxHeaderListSize
      write FMaxHeaderListSize;
  end;

  // TsgcWSFallBack_SSE //
  TsgcWSFallBack_SSE = class(TPersistent)
  private
    FEnabled: Boolean;
    FRetry: Integer;
  public
    constructor Create;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property Retry: Integer read FRetry write FRetry;
  end;

  // TsgcWSFallBack_Options //
  TsgcWSFallBack_Options = class(TPersistent)
  private
    FFlash: TsgcWSFallBack_Flash;
    FServerSentEvents: TsgcWSFallBack_SSE;
  protected
    procedure SetServerSentEvents(const Value: TsgcWSFallBack_SSE);
    procedure SetFlash(const Value: TsgcWSFallBack_Flash);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Flash: TsgcWSFallBack_Flash read FFlash write SetFlash;
    property ServerSentEvents: TsgcWSFallBack_SSE read FServerSentEvents
      write SetServerSentEvents;
  end;

  TsgcHTTP2ServerEvents = class(TPersistent)
  private
    FOnConnect: Boolean;
    FOnDisconnect: Boolean;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property OnConnect: Boolean read FOnConnect write FOnConnect;
    property OnDisconnect: Boolean read FOnDisconnect write FOnDisconnect;
  end;

  TsgcWSHTTP2AlternateService = class(TPersistent)
  private
    FEnabled: Boolean;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

  TsgcWSHTTP2Server_Options = class(TPersistent)
  private
    FAltSvc: TsgcWSHTTP2AlternateService;
    FEnabled: Boolean;
    FEvents: TsgcHTTP2ServerEvents;
    FFragmentedData: Th2FragmentedData;
    FSettings: TsgcHTTP2ServerSettings;
    function GetEnabled: Boolean;
    procedure SetAltSvc(const Value: TsgcWSHTTP2AlternateService);
    procedure SetEvents(const Value: TsgcHTTP2ServerEvents);
    procedure SetSettings(const Value: TsgcHTTP2ServerSettings);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property AltSvc: TsgcWSHTTP2AlternateService read FAltSvc write SetAltSvc;
    property Enabled: Boolean read GetEnabled write FEnabled;
    property Events: TsgcHTTP2ServerEvents read FEvents write SetEvents;
    property FragmentedData: Th2FragmentedData read FFragmentedData
      write FFragmentedData;
    property Settings: TsgcHTTP2ServerSettings read FSettings write SetSettings;
  end;

  TsgcWSOptionsServer = class(TPersistent)
  private
    FCleanDisconnect: Boolean;
    FFragmentedMessages: TwsFragmentedMessages;
    FHTMLFiles: Boolean;
    FJavascriptFiles: Boolean;
    FReadTimeOut: Integer;
    FRaiseDisconnectExceptions: Boolean;
    FValidateUTF8: Boolean;
    FWriteTimeOut: Integer;
    function GetReadTimeOut: Integer;
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
    property HTMLFiles: Boolean read FHTMLFiles write FHTMLFiles;
    property JavascriptFiles: Boolean read FJavascriptFiles
      write FJavascriptFiles;
    property ReadTimeOut: Integer read GetReadTimeOut write FReadTimeOut;
    property WriteTimeOut: Integer read FWriteTimeOut write FWriteTimeOut;
    property RaiseDisconnectExceptions: Boolean read FRaiseDisconnectExceptions
      write FRaiseDisconnectExceptions;
    property ValidateUTF8: Boolean read FValidateUTF8 write FValidateUTF8;
  end;

  TsgcWSConnectionAuthentication = class(TPersistent)
  private
    FAuthenticated: Boolean;
    FAuthType: TwsAuthentication;
    FPassword: String;
    FUser: String;
    FSessionID: string;
    function GetSessionID: string;
  public
    constructor Create;
  public
    property Authenticated: Boolean read FAuthenticated write FAuthenticated;
    property AuthType: TwsAuthentication read FAuthType write FAuthType;
    property SessionID: string read GetSessionID write FSessionID;
    property Password: String read FPassword write FPassword;
    property User: String read FUser write FUser;
  end;

  TsgcWSAuthenticationServer_Options = class(TsgcWSAuthentication_Options)
  private
    FAllowNonAuth: Boolean;
    FAuthUsers: TStringList;
    FURL: TsgcWSAuthenticationServer_URL;
    FSession: TsgcWSAuthenticationServer_Session;
    FBasic: TsgcWSAuthenticationServer_Basic;
    FJWT: TsgcWSAuthenticationServer_JWT;
    FOAuth: TsgcWSAuthenticationServer_OAuth;
  protected
    procedure SetAuthUsers(const Value: TStringList);
    procedure SetURL(const Value: TsgcWSAuthenticationServer_URL);
    procedure SetSession(const Value: TsgcWSAuthenticationServer_Session);
    procedure SetBasic(const Value: TsgcWSAuthenticationServer_Basic);
    procedure SetOAuth(const Value: TsgcWSAuthenticationServer_OAuth);
    procedure SetJWT(const Value: TsgcWSAuthenticationServer_JWT);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property AllowNonAuth: Boolean read FAllowNonAuth write FAllowNonAuth;
    property AuthUsers: TStringList read FAuthUsers write SetAuthUsers;
    property URL: TsgcWSAuthenticationServer_URL read FURL write SetURL;
    property Session: TsgcWSAuthenticationServer_Session read FSession
      write SetSession;
    property Basic: TsgcWSAuthenticationServer_Basic read FBasic write SetBasic;
    property JWT: TsgcWSAuthenticationServer_JWT read FJWT write SetJWT;
    property OAuth: TsgcWSAuthenticationServer_OAuth read FOAuth write SetOAuth;
  end;

  TsgcWSHandShakeServer_Base = class(TsgcWSHandshake)
    { fields }
  protected
    FGet: String;
    FHost: string;
    FOrigin: string;
    FKey: string;
    FVersion: string;
  public
    property Get: string read FGet;
    property Host: string read FHost;
    property Origin: string read FOrigin;
    property Key: string read FKey;
    property Version: string read FVersion;
    { fields }

    { specification }
  private
    function GetSpecification: TwsSpecification;
  public
    property Specification: TwsSpecification read GetSpecification;
    { specification }

    { decode }
  protected
    FIsConnection: Boolean;
    FIsGET: Boolean;
    FIsHost: Boolean;
    FIsKey: Boolean;
    FIsOrigin: Boolean;
    FIsProtocols: Boolean;
    FIsUpgrade: Boolean;
    FIsVersion: Boolean;
    FIsExtensions: Boolean;
  private
    procedure DoDecodeHeaders(aHeaders: TStringList);
  protected
    procedure DoDecodeConnection(const aHeader: String);
    procedure DoDecodeGET(const aHeader: String);
    procedure DoDecodeHost(const aHeader: String);
    procedure DoDecodeKey(const aHeader: String);
    procedure DoDecodeOrigin(const aHeader: String);
    procedure DoDecodeProtocols(const aHeader: String);
    procedure DoDecodeUpgrade(const aHeader: String);
    procedure DoDecodeVersion(const aHeader: String);
    procedure DoDecodeExtensions(const aHeader: String);
    { decode }

    { draft hixie76 }
  protected
    FIsKey1: Boolean;
    FIsKey2: Boolean;
  protected
    FKey1: string;
    FKey2: string;
    FKey3: String;
    procedure DoDecodeKey1(const aHeader: String);
    procedure DoDecodeKey2(const aHeader: String);
    { draft hixie76 }

    { constructor }
  public
    constructor Create(aHeaders: TStringList); virtual;
    { constructor }
  end;

  TsgcWSConnectionServer_Base = class(TsgcWSConnection_Base_Indy)
    { from TsgcWSConnection }
  protected
    function GetURL: String; override;
    { from TsgcWSConnection }

    { RequestURL protocol }
  protected
    function DoGetProtocol: string;
    { RequestURL protocol }

    { request }
  protected
    procedure DoReadHeadersRequest; virtual; abstract;
    { request }

    { handshake }
  private
    procedure DoOriginsAllowed;
  protected
    procedure DoHandShake_RFC6455;
    procedure DoHandShake_Hixie76;
  protected
    function GetHandShake: TsgcWSHandShakeServer_Base; virtual; abstract;
  protected
    procedure DoHandShakeExtensions; virtual;
    procedure DoHandShakeSecurity; virtual;
  protected
    procedure DoHandshake; override;
  public
    property HandShake: TsgcWSHandShakeServer_Base read GetHandShake;
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
  protected
    procedure DoHTTPAuthRealm; virtual; abstract;
  private
    function DoHTTPFileResponse(const aFileName, aContentType: String;
      aDisconnect: Boolean): Boolean;
  protected
    procedure DoAuthSessionResponse(const aParams: String);
  protected
    function DoAuthenticationURL(const aParams: string): Boolean;
    function DoAuthenticationBasic(const aParams: string): Boolean;
  public
    procedure SetAuthenticationCloseCode(const aCloseCode: Integer);
  public
    property Authentication: TsgcWSConnectionAuthentication
      read GetAuthentication write FAuthentication;
  public
    property OnAuthentication: TsgcWSAuthenticationEvent read FOnAuthentication
      write FOnAuthentication;
    { authentication }

    { server properties }
  private
    FCustomHeartBeat: Boolean;
    FServerOriginsAllowed: String;
  public
    property ServerOriginsAllowed: String read FServerOriginsAllowed
      write FServerOriginsAllowed;
    { server properties }

    { constructor / destructor }
  public
    constructor Create; override;
    destructor Destroy; override;
    { constructor / destructor }
  end;

  TsgcWSServer_Base = class({$IFDEF SGC_HTML}TsgcWSComponent_Server_HTML{$ELSE}TsgcWSComponent_Server{$ENDIF})
    { from TsgcWSComponent }
  protected
    function GetLogFile: TsgcWSLogFile; override;
    { from TsgcWSComponent }

    { from TsgcWSComponent_Server }
  public
{$IFDEF NEXTGEN}
    function LockList: TList<TsgcWSConnection>; reintroduce; overload;
      virtual; abstract;
{$ELSE}
    function LockList: TList; override;
{$ENDIF}
    { from TsgcWSComponent_Server }

    { hearbeat }
  private
    FOnBeforeHeartBeat: TsgcWSOnBeforeHeartBeatEvent;
  protected
    procedure OnHeartBeatTimeoutEvent(Sender: TObject); override;
    procedure OnHeartBeatExceptionEvent(Sender: TObject; E: Exception);
      override;
    procedure OnHeartBeatTimeoutExceptionEvent(Sender: TObject;
      E: Exception); override;
    procedure OnHeartBeatEvent(Sender: TObject); override;
  public
    property OnBeforeHeartBeat: TsgcWSOnBeforeHeartBeatEvent
      read FOnBeforeHeartBeat write FOnBeforeHeartBeat;
    { heartbeat }

    { bindings }
  private
    FBindings: TsgcWSBindings;
    function GetBindings: TsgcWSBindings;
  public
    property Bindings: TsgcWSBindings read GetBindings write FBindings;
    { bindings }

    { events }
  protected
    procedure OnClientUpdateEvent(aConnection: TsgcWSConnection;
      aType: Integer); virtual;
  protected
    procedure OnServerMessageEvent(aConnection: TsgcWSConnection;
      const Text: string); virtual;
    procedure OnServerBinaryEvent(aConnection: TsgcWSConnection;
      const aStream: TMemoryStream); virtual;
    procedure OnServerFragmentedEvent(aConnection: TsgcWSConnection;
      const aData: TMemoryStream; const aOpCode: TOpCode;
      const aContinuation: Boolean); virtual;
    procedure OnServerDisconnectEvent(aConnection: TsgcWSConnection;
      aErrorCode: Integer); virtual;
  protected
    procedure OnHTTPUploadBeforeSaveFileEvent(Sender: TObject; var aFileName,
        aFilePath: string); virtual;
    procedure OnHTTPUploadAfterSaveFileEvent(Sender: TObject; const aFileName,
        aFilePath: string); virtual;
    { events }

    { built-in libraries }
  protected
    function DoBuiltInLibraries(aConnection: TsgcWSConnectionServer_Base;
      const aText: String; aDisconnect: Boolean): Boolean; virtual;
    { built-in libraries }

    { connections }
  private
    procedure DoReadHeadersRequest(aConnection: TsgcWSConnectionServer_Base);
  protected
    function GetHTTPStatusCode(aConnection: TsgcWSConnectionServer_Base)
      : Cardinal; virtual;
  protected
    function GetConnectionByIndex(Index: Integer)
      : TsgcWSConnectionServer_Base; virtual;
    function GetConnectionByGuid(const aGUID: String)
      : TsgcWSConnectionServer_Base; virtual;
  protected
    function GetCount: Integer; virtual;
    procedure AddConnection(aConnection: TsgcWSConnection); virtual;
    procedure RemoveConnection(aConnection: TsgcWSConnection); virtual;
    procedure ClearConnections; virtual;
  protected
    procedure DoDisconnectAll;
  public
    procedure DisconnectAll;
  public
    property ConnectionsByGUID[const Guid: String]: TsgcWSConnectionServer_Base
      read GetConnectionByGuid;
    property Connections[Index: Integer]: TsgcWSConnectionServer_Base
      read GetConnectionByIndex; default;
    property Count: Integer read GetCount;
  public
    function ShareList: TList{$IFDEF NEXTGEN}<TsgcWSConnection>{$ENDIF};
      virtual; abstract;
    procedure UnShareList; virtual; abstract;
    { connections }

    { properties }
  private
    FOptions: TsgcWSOptionsServer;
    FSecurityOptions: TsgcWSSecurity_Options;
    FQueueOptions: TsgcWSQueueServer_Options;
    FHTTPUploadFiles: TsgcHTTPUploadFilesServer;
    procedure SetQueueOptions(const Value: TsgcWSQueueServer_Options);
    procedure SetSecurityOptions(const Value: TsgcWSSecurity_Options);
    procedure SetOptions(const Value: TsgcWSOptionsServer);
    procedure SetHTTPUploadFiles(const Value: TsgcHTTPUploadFilesServer);
  public
    property Options: TsgcWSOptionsServer read FOptions write SetOptions;
    property QueueOptions: TsgcWSQueueServer_Options read FQueueOptions
      write SetQueueOptions;
    property SecurityOptions: TsgcWSSecurity_Options read FSecurityOptions
      write SetSecurityOptions;
    property HTTPUploadFiles: TsgcHTTPUploadFilesServer read FHTTPUploadFiles
      write SetHTTPUploadFiles;
    { properties }

    { default host/port }
  protected
    function GetPort: Integer; virtual; abstract;
    function GetHost: string; virtual; abstract;
    procedure _SetPort(const Value: Integer); virtual; abstract;
    procedure SetHost(const Value: string); virtual; abstract;
  public
    property Host: string read GetHost write SetHost;
    property Port: Integer read GetPort write _SetPort;
    { default host/port }

    { authentication }
  private
    FSessionIDList: TsgcDelimitedStringList;
    FAuthentication: TsgcWSAuthenticationServer_Options;
  protected
    FOnAuthentication: TsgcWSAuthenticationEvent;
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
    function DoAuthenticationSession(aConnection: TsgcWSConnectionServer_Base;
      const aParams: String): Boolean;
  public
    procedure OnServerAuthenticationEvent(Connection: TsgcWSConnection;
      aUser, aPassword: String; var Authenticated: Boolean);
  public
    property Authentication: TsgcWSAuthenticationServer_Options
      read FAuthentication write SetAuthentication;
    { authentication }

    { xhr }
  protected
    procedure DoXHR(const aConnection: TsgcWSConnectionServer_Base);
    { xhr }

    { http protocol }
  protected
    procedure DoHTTPProtocol(const aConnection
      : TsgcWSConnectionServer_Base); virtual;
    { http protocol }

    { fallback }
  private
    FFallBack: TsgcWSFallBack_Options;
  protected
    procedure SetFallBack(const Value: TsgcWSFallBack_Options);
  public
    property FallBack: TsgcWSFallBack_Options read FFallBack write SetFallBack;
    { fallback }

    { WatchDog }
  private
    FWatchDogMonitorSecret: string;
    procedure DoWatchDogMonitor;
  protected
    procedure OnWatchDogEvent(Sender: TObject); override;
    procedure OnWatchDogExceptionEvent(Sender: TObject; E: Exception); override;
    { WatchDog }

    { OAuth2 }
  protected
    procedure OnOAuth2ResponseEvent(aConnection: TsgcWSConnection;
      aContent, aContentType: String); virtual;
    procedure OnAuth2ResponseErrorEvent(aConnection: TsgcWSConnection); virtual;
    procedure OnOAuth2AuthenticationEvent(aConnection: TsgcWSConnection;
      aUser, aPassword: String; var Authenticated: Boolean); virtual;
    { OAuth2 }

    { enable / disable }
  protected
    FActive: Boolean;
    function GetActive: Boolean; virtual;
    procedure SetActive(const Value: Boolean); virtual;
  protected
    procedure DoStart; virtual;
    procedure DoStop; virtual;
  public
    procedure Start;
    procedure Stop;
    procedure ReStart;
  public
    property Active: Boolean read GetActive write SetActive default False;
    { enable / disable }

    { ssl }
  private
    FSSL: Boolean;
  protected
    function GetSSL: Boolean; virtual;
    procedure SetSSL(const Value: Boolean); virtual;
  public
    property SSL: Boolean read GetSSL write SetSSL default False;
    { ssl }

    { constructor }
  private
    procedure DoClear;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor }

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
    function DoWriteData(const aGUID, aMessage: string): Boolean; overload;
    function DoWriteData(const aGUID: string; const aStream: TStream;
      aSize: Integer = 0; const aStreaming: TwsStreaming = stmNone)
      : Boolean; overload;
  public
    function WriteData(const aGUID, aMessage: string): Boolean; override;
    function WriteData(const aGUID: String; aStream: TStream;
      aSize: Integer = 0; const aStreaming: TwsStreaming = stmNone)
      : Boolean; override;
    { send }

    { ping }
  protected
    procedure DoPing; virtual;
  public
    procedure Ping;
    { ping }

    { events }
  protected
    FOnStartup: TNotifyEvent;
    FOnShutdown: TNotifyEvent;
    FOnHTTPUploadBeforeSaveFile: TsgcWSHTTPUploadBeforeSaveFileEvent;
    FOnHTTPUploadAfterSaveFile: TsgcWSHTTPUploadAfterSaveFileEvent;
  public
    property OnStartup: TNotifyEvent read FOnStartup write FOnStartup;
    property OnShutdown: TNotifyEvent read FOnShutdown write FOnShutdown;
    property OnAuthentication: TsgcWSAuthenticationEvent read FOnAuthentication
      write FOnAuthentication;
    property OnHTTPUploadBeforeSaveFile: TsgcWSHTTPUploadBeforeSaveFileEvent read
        FOnHTTPUploadBeforeSaveFile write FOnHTTPUploadBeforeSaveFile;
    property OnHTTPUploadAfterSaveFile: TsgcWSHTTPUploadAfterSaveFileEvent read
        FOnHTTPUploadAfterSaveFile write FOnHTTPUploadAfterSaveFile;
    { events }
  End;

implementation

uses
  DateUtils,
  // indy
{$IFDEF SGC_INDY}sgcIdContext{$ELSE}IdContext{$ENDIF},
  // sgc
{$IFDEF SGC_HTTP2}sgcHTTP2_Const, {$ENDIF}
  sgcBase_Const, sgcBase_Helpers, sgcWebSocket_Client,
  sgcWebSocket_Const, sgcWebSocket_HTTPResponse, sgcWebSocket_Resources,
  sgcBase_Classes, sgcSocket_Const;

const
  CS_SESSION_ID = 1;

type
  TsgcWSProtocol_Server_Hack = class(TsgcWSProtocol_Server)
  end;

  TsgcWSClient_Hack = class(TsgcWSClient)
  end;

constructor TsgcWSServer_Base.Create(aOwner: TComponent);
begin
  inherited;
  NotifyEvents := neNoSync;
  FOptions := TsgcWSOptionsServer.Create;
  FSecurityOptions := TsgcWSSecurity_Options.Create;
  FAuthentication := TsgcWSAuthenticationServer_Options.Create;
  FAuthentication.URL.Enabled := True;
  FAuthentication.Session.Enabled := True;
  FAuthentication.Basic.Enabled := False;
  Extensions.Mode := appServer;
  FQueueOptions := TsgcWSQueueServer_Options.Create;
  FQueueOptions.Text.Level := qmNone;
  FQueueOptions.Binary.Level := qmNone;
  FQueueOptions.Ping.Level := qmNone;
  FFallBack := TsgcWSFallBack_Options.Create;
  FFallBack.Flash.Enabled := False;
  FFallBack.ServerSentEvents.Enabled := False;
  FHTTPUploadFiles := TsgcHTTPUploadFilesServer.Create;
end;

destructor TsgcWSServer_Base.Destroy;
begin
  DoClear;
  ClearConnections;
  sgcFree(FHTTPUploadFiles);
  sgcFree(FBindings);
  sgcFree(FFallBack);
  sgcFree(FSecurityOptions);
  sgcFree(FOptions);
  sgcFree(FAuthentication);
  sgcFree(FQueueOptions);
  sgcFree(FSessionIDList);
  inherited;
end;

procedure TsgcWSServer_Base.AddConnection(aConnection: TsgcWSConnection);
var
  oList: TList{$IFDEF NEXTGEN}<TsgcWSConnection>{$ENDIF};
begin
  oList := LockList;
  Try
    oList.Add(aConnection);
  Finally
    UnLockList;
  End;
end;

procedure TsgcWSServer_Base.Broadcast(const aMessage: string;
  const aChannel: string = ''; const aProtocol: string = '';
  const Exclude: String = ''; const Include: String = '');
begin
  DoBroadCast(aMessage, nil, opText, aChannel, aProtocol, Exclude, Include);
end;

procedure TsgcWSServer_Base.Broadcast(aStream: TStream;
  const aChannel: string = ''; const aProtocol: string = '';
  const Exclude: String = ''; const Include: String = '';
  const aSize: Integer = 0; const aStreaming: TwsStreaming = stmNone);
begin
  DoBroadCast('', aStream, opBinary, aChannel, aProtocol, Exclude, Include,
    aSize, aStreaming);
end;

procedure TsgcWSServer_Base.DisconnectAll;
begin
  DoDisconnectAll;
end;

procedure TsgcWSServer_Base.DoAddSessionID(const aSessionID, aUser,
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

function TsgcWSServer_Base.DoAuthenticationSession
  (aConnection: TsgcWSConnectionServer_Base; const aParams: String): Boolean;
var
  oParams: TsgcDelimitedStringList;
  vSessionID: String;
  vUser, vPassword: String;
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
        GetSessionUserPassword(vSessionID, vUser, vPassword);
        vUser := aConnection.Authentication.User;
        vPassword := aConnection.Authentication.Password;
        // ... delete session id
        DoDelSessionID(vSessionID);
      end;
    end;
  Finally
    sgcFree(oParams);
  End;
end;

procedure TsgcWSServer_Base.DoBroadCast(const aMessage: string;
  aStream: TStream; aOpCode: TOpCode; const aChannel: String = '';
  const aProtocol: string = ''; const Exclude: String = '';
  const Include: String = ''; const aSize: Integer = 0;
  const aStreaming: TwsStreaming = stmNone);
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TsgcWSConnection>{$ENDIF};
  oList2: TList{$IFDEF NEXTGEN}<TsgcWSConnection>{$ENDIF};
  oExclude: TsgcDelimitedStringList;
  oInclude: TsgcDelimitedStringList;
  vExclude, vInclude: Boolean;
  oConnection: TsgcWSConnectionServer_Base;
  vHandled: Boolean;
begin
  oConnection := nil;

  oExclude := nil;
  oInclude := nil;

  oList2 := TList{$IFDEF NEXTGEN}<TsgcWSConnection>{$ENDIF}.Create;
  Try
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

    // ... fill list connections
    Try
      oList := LockList;
      Try
        for i := 0 to oList.Count - 1 do
        begin
          Try
            oConnection := TsgcWSConnectionServer_Base(oList[i]);
            if Assigned(oConnection) then
            begin
              if ((oConnection.Enabled) or (oConnection.Disconnected = False) or
                (aOpCode = opPing)) then
              begin
                // ... exclude
                if Exclude <> '' then
                  vExclude := oExclude.IndexOf(oConnection.Guid) <> -1
                else
                  vExclude := False;
                // ... include
                if Include <> '' then
                  vInclude := oInclude.IndexOf(oConnection.Guid) <> -1
                else
                  vInclude := True;
                // ... broadcast
                if (vExclude = False) and (vInclude = True) then
                  oList2.Add(oConnection);
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
    Finally
      sgcFree(oExclude);
      sgcFree(oInclude);
    End;

    // ... broadcast
    for i := 0 to oList2.Count - 1 do
    begin
      oConnection := TsgcWSConnectionServer_Base(oList2[i]);
      if Assigned(oConnection) then
      begin
        case aOpCode of
          opText:
            begin
              if UpperCase(oConnection.Protocol) = UpperCase(aProtocol) then
              begin
                if oConnection.Subscribed(aChannel) then
                  oConnection.WriteData(aMessage);
              end;
            end;
          opBinary:
            begin
              if UpperCase(oConnection.Protocol) = UpperCase(aProtocol) then
              begin
                if oConnection.Subscribed(aChannel) then
                  oConnection.WriteData(aStream, aSize, aStreaming);
              end;
            end;
          opPing:
            begin
              if oConnection.Disconnected or (oConnection.QueueOptions = nil) or
                (oConnection.Enabled = False) then
                oConnection.DisconnectPeer
              else
              begin
                vHandled := False;
                if Assigned(FOnBeforeHeartBeat) then
                  FOnBeforeHeartBeat(self, oConnection, vHandled);
                TsgcWSConnectionServer_Base(oConnection).FCustomHeartBeat
                  := vHandled;
                if vHandled = False then
                  oConnection.Ping(aMessage);
              end;
            end;
        end;
      end;
    end;
  Finally
    sgcFree(oList2);
  End;
end;

procedure TsgcWSServer_Base.DoDelSessionID(const aSessionID: String);
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

procedure TsgcWSServer_Base.DoPing;
begin
  DoBroadCast(FormatDateTime('yyyymmddhhnnsszzz', Now), nil, opPing);
end;

function TsgcWSServer_Base.DoWriteData(const aGUID, aMessage: string): Boolean;
var
  oConnection: TsgcWSConnectionServer_Base;
begin
  Result := False;

  oConnection := GetConnectionByGuid(aGUID);
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

function TsgcWSServer_Base.DoWriteData(const aGUID: string;
  const aStream: TStream; aSize: Integer = 0;
  const aStreaming: TwsStreaming = stmNone): Boolean;
var
  oConnection: TsgcWSConnectionServer_Base;
begin
  Result := False;

  oConnection := GetConnectionByGuid(aGUID);
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

function TsgcWSServer_Base.GetActive: Boolean;
begin
  Result := FActive;
end;

function TsgcWSServer_Base.GetConnectionByGuid(const aGUID: String)
  : TsgcWSConnectionServer_Base;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TsgcWSConnection>{$ENDIF};
  oConnection: TsgcWSConnectionServer_Base;
begin
  Result := nil;
  oConnection := nil;

  oList := LockList;
  Try
    for i := 0 to oList.Count - 1 do
    begin
      Try
        oConnection := TsgcWSConnectionServer_Base(oList[i]);
        if Assigned(oConnection) then
        begin
          if oConnection.Guid = aGUID then
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

function TsgcWSServer_Base.GetConnectionByIndex(Index: Integer)
  : TsgcWSConnectionServer_Base;
var
  oList: TList{$IFDEF NEXTGEN}<TsgcWSConnection>{$ENDIF};
begin
  Result := nil;

  oList := ShareList;
  Try
    if Index < oList.Count then
      Result := TsgcWSConnectionServer_Base(oList[Index]);
  Finally
    UnShareList;
  End;
end;

function TsgcWSServer_Base.GetCount: Integer;
var
  oList: TList{$IFDEF NEXTGEN}<TsgcWSConnection>{$ENDIF};
begin
  oList := ShareList;
  Try
    Result := oList.Count;
  Finally
    UnShareList;
  End;
end;

function TsgcWSServer_Base.GetSessionID(const aSessionID: String): Boolean;
begin
  Result := GetSessionIDList.IndexOfName(aSessionID) <> -1;
end;

function TsgcWSServer_Base.GetSessionIDList: TsgcDelimitedStringList;
begin
  if not Assigned(FSessionIDList) then
    FSessionIDList := TsgcDelimitedStringList.Create;
  Result := FSessionIDList;
end;

function TsgcWSServer_Base.GetSessionUserPassword(const aSessionID: String;
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

procedure TsgcWSServer_Base.OnClientUpdateEvent(aConnection: TsgcWSConnection;
  aType: Integer);
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
    -1:
      begin
        // nothing
      end;
  End;
end;

function TsgcWSServer_Base.GetHTTPStatusCode(aConnection
  : TsgcWSConnectionServer_Base): Cardinal;
var
  vAuthenticated: Boolean;
begin
  Result := 404;

  DoReadHeadersRequest(aConnection);

  if IsWebSocketHeader(aConnection.HeadersRequest) then
  begin
    Result := 101;
    // ... watchdog connection
    if WatchDog.Enabled and WatchDog.Monitor.Enabled and
      IsWatchDogConnection(aConnection.HeadersRequest, FWatchDogMonitorSecret)
    then
      aConnection.IsInternal := True;

    // ... authentication
    if self.Authentication.Enabled and (aConnection.IsInternal = False) then
    begin
{$IFDEF SGC_OAUTH_SERVER}
      if Assigned(self.Authentication.OAuth.OAuth2) then
        TsgcWSConnectionServer_Base(aConnection).Authentication.Authenticated :=
          self.Authentication.OAuth.OAuth2.IsOAuth2TokenValid(aConnection,
          aConnection.HeadersRequest)
      else
{$ENDIF}
{$IFDEF SGC_JWT_SERVER}
        if Assigned(self.Authentication.JWT.JWT) then
          TsgcWSConnectionServer_Base(aConnection).Authentication.Authenticated
            := self.Authentication.JWT.JWT.IsJWTTokenValid(aConnection,
            aConnection.HeadersRequest)
        else
{$ENDIF}
          // ... authenticate URL
          if self.Authentication.URL.Enabled and
            IsAuthenticationURLHeader(aConnection.HeadersRequest) then
            TsgcWSConnectionServer_Base(aConnection)
              .DoAuthenticationURL
              (DecodeGetFullPath(aConnection.HeadersRequest))
            // ... authenticate http
          else if self.Authentication.Session.Enabled and
            IsAuthenticationSessionHeader(aConnection.HeadersRequest) then
          begin
            TsgcWSConnectionServer_Base(aConnection)
              .Authentication.Authenticated :=
              DoAuthenticationSession(TsgcWSConnectionServer_Base(aConnection),
              DecodeGetFullPath(aConnection.HeadersRequest));
          end
          // ... authenticate basic
          else if self.Authentication.Basic.Enabled and
            IsAuthenticationBasicHeader(aConnection.HeadersRequest) then
          begin
            TsgcWSConnectionServer_Base(aConnection)
              .Authentication.Authenticated := TsgcWSConnectionServer_Base
              (aConnection).DoAuthenticationBasic
              (DecodeAuthorizationBasic(aConnection.HeadersRequest));
          end
          // ... raise OnAuthentication event
          else
          begin
            vAuthenticated := TsgcWSConnectionServer_Base(aConnection)
              .Authentication.Authenticated;
            if Assigned(FOnAuthentication) then
              FOnAuthentication(aConnection, '', '', vAuthenticated);
            TsgcWSConnectionServer_Base(aConnection)
              .Authentication.Authenticated := vAuthenticated;
          end;
      // ... if not authenticated then close
      if not(TsgcWSConnectionServer_Base(aConnection)
        .Authentication.Authenticated) and
        (self.Authentication.AllowNonAuth = False) then
      begin
        if TsgcWSConnectionServer_Base(aConnection).FAuthenticationCloseCode = CS_CLOSE_NORMAL
        then // send custom close code after Handshake
        begin
          aConnection.HeadersRequest.Clear;
          Result := 404;
        end;
      end;
    end;
  end
  // ... auth session request
  else if IsRequestAuthenticationSessionHeader(aConnection.HeadersRequest) then
  begin
    if self.Authentication.Enabled and self.Authentication.Session.Enabled then
      aConnection.DoAuthSessionResponse
        (DecodeGetFullPath(aConnection.HeadersRequest));
    Result := 200;
  end
  // ... xhr
  else if IsXHRHeader(aConnection.HeadersRequest) then
  begin
    DoXHR(aConnection);
    Result := 200;
  end
  // ... http protocol
  else if IsHTTPProtocolHeader(aConnection.HeadersRequest) then
  begin
    DoHTTPProtocol(aConnection);
    Result := 200;
  end
  // ... flash fallback
  else if IsRequestFlashPolicy(aConnection.HeadersRequest) then
  begin
    aConnection.Transport := trpFlash;
    if FallBack.Flash.Enabled then
    begin
      aConnection.DoWriteBytes(sgcStringToBytes('<cross-domain-policy>' +
        '<allow-access-from domain="' + FallBack.Flash.Domain + '" to-ports="' +
        FallBack.Flash.Ports + '" />' + '</cross-domain-policy>' + chr(0)));
      Result := 200;
    end;
  end
  // ... server sent events
  else if IsSSEHeader(aConnection.HeadersRequest) then
  begin
    aConnection.Transport := trpSSE;
    if FallBack.ServerSentEvents.Enabled then
    begin
      aConnection.DoProtocolSSE;
      DoNotifyConnect(aConnection);
      aConnection.FEnabled := True;
      aConnection.DoWriteData_SSE
        ('retry: ' + IntToStr(FallBack.ServerSentEvents.Retry * 1000));
      aConnection.DoWriteData_SSE(aConnection.Guid);
      aConnection.DoWriteData_SSE(aConnection.Guid);
      Result := 200;
    end;
  end
  // ... built-in libraries
  else if DoBuiltInLibraries(aConnection,
    DecodeGETHeader(aConnection.HeadersRequest), False) then
    Result := 200
  else if IsHTTPHeader(aConnection.HeadersRequest) then
    aConnection.Transport := trpHTTP
  else if DoUnknownProtocol(aConnection) then
  begin
    aConnection.FEnabled := True;
    Result := 101;
  end;
end;

procedure TsgcWSServer_Base.OnHeartBeatEvent(Sender: TObject);
begin
  inherited;
  // ... send ping
  DoPing;

  // ... start timeout
  DoStartHeartBeatTimeout;
end;

procedure TsgcWSServer_Base.OnHeartBeatTimeoutEvent(Sender: TObject);
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TsgcWSConnection>{$ENDIF};
  oConnection: TsgcWSConnectionServer_Base;
begin
  inherited;

  // ... search in list
  oList := LockList;
  Try
    for i := 0 to oList.Count - 1 do
    begin
      oConnection := TsgcWSConnectionServer_Base(oList[i]);
      if Assigned(oConnection) then
      begin
        if oConnection.Transport in [trpRFC6455, trpRFC8441, trpHixie76,
          trpFlash] then
        begin
          if Assigned(oConnection) and (oConnection.FCustomHeartBeat = False)
          then
          begin
            if SecondsBetWeen(oConnection.LastPing, oConnection.LastPong) >=
              HeartBeat.Timeout then
            begin
              if ((HeartBeat.Interval >= HeartBeat.Timeout) and
                (SecondsBetWeen(Now, oConnection.LastPing) >= HeartBeat.Timeout)
                or (HeartBeat.Timeout >= HeartBeat.Interval) and
                (SecondsBetWeen(Now, oConnection.FirstPing) >=
                HeartBeat.Timeout)) then
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

procedure TsgcWSServer_Base.OnServerAuthenticationEvent
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
    if TsgcWSConnectionServer_Base(Connection).Authentication.AuthType = authSession
    then
      DoAddSessionID(TsgcWSConnectionServer_Base(Connection)
        .Authentication.SessionID, aUser, aPassword);
  end;
end;

procedure TsgcWSServer_Base.OnServerDisconnectEvent
  (aConnection: TsgcWSConnection; aErrorCode: Integer);
begin
  if IsDestroying then
    exit;

  Try
    RemoveConnection(aConnection);
  Finally
    DoNotifyDisconnect(aConnection);
  End;
end;

procedure TsgcWSServer_Base.OnServerMessageEvent(aConnection: TsgcWSConnection;
  const Text: string);
begin
  if IsDestroying then
    exit;

  // -->start sgc_trial
{$IFDEF SGC_TRIAL}
  if (Now > EncodeDate(2022, 7, 1)) or (Now < EncodeDate(2022, 1, 1)) then
    sleep(StrToInt(FormatDateTime('sszzz', Now)));
{$ENDIF}
  // <--end sgc_trial
  Try
    aConnection.MsgReceived := Text;
    DoNotifyMessage(aConnection);
  Except
    On E: Exception do
      DoError(aConnection, E.Message);
  end;
end;

procedure TsgcWSServer_Base.OnServerBinaryEvent(aConnection: TsgcWSConnection;
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

procedure TsgcWSServer_Base.OnServerFragmentedEvent
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

procedure TsgcWSServer_Base.Ping;
begin
  DoPing;
end;

procedure TsgcWSServer_Base.RemoveConnection(aConnection: TsgcWSConnection);
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TsgcWSConnection>{$ENDIF};
begin
  oList := LockList;
  Try
    for i := oList.Count - 1 Downto 0 do
    begin
      if TsgcWSConnectionServer_Base(oList[i]).Guid = aConnection.Guid then
      begin
        oList.Delete(i);
        break;
      end;
    end;
  Finally
    UnLockList;
  End;
end;

procedure TsgcWSServer_Base.ClearConnections;
var
  oList: TList{$IFDEF NEXTGEN}<TsgcWSConnection>{$ENDIF};
begin
  oList := LockList;
  Try
    while oList.Count > 0 do
    begin
      TsgcWSConnectionServer_Base(oList[0]).Free;
      oList.Delete(0);
    end;
  Finally
    UnLockList;
  End;
end;

function TsgcWSServer_Base.DoBuiltInLibraries(aConnection
  : TsgcWSConnectionServer_Base; const aText: String;
  aDisconnect: Boolean): Boolean;
var
  oStream: TMemoryStream;
  vFileName: String;
begin
  Result := False;

  // ... javascript request
  if sgcMatchesMask(aText, 'GET *.js *') then
  begin
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

      Result := aConnection.DoHTTPFileResponse(vFileName, 'text/javascript',
        aDisconnect);
    end;
  end
  // ... html request
  else if sgcMatchesMask(aText, 'GET *.html *') then
  begin
    if Options.HTMLFiles then
      Result := aConnection.DoHTTPFileResponse(DecodeGETFileName(aText),
        'text/html; charset=utf-8', aDisconnect);
  end
  // ... css request
  else if sgcMatchesMask(aText, 'GET *.css *') then
  begin
    if Options.HTMLFiles then
      Result := aConnection.DoHTTPFileResponse(DecodeGETFileName(aText),
        'text/css', aDisconnect);
  end
  // ... flash MainWebSocket.swf
  else if sgcMatchesMask(aText, 'GET *' + CS_WEBSOCKETMAIN_SWF + ' *') then
  begin
    oStream := TMemoryStream.Create;
    Try
      if GetResourceStream(CS_SGC_WEBSOCKETMAIN_SWF, oStream) then
        aConnection.DoHTTPResponse(oStream, 'application/x-shockwave-flash');
    Finally
      // don't free stream, it's freed internally
      // sgcFree(oStream);
    End;
    Result := True;
  end
  // ... favicon request
  else if sgcMatchesMask(aText, 'GET *favicon*') then
  begin
    if aDisconnect then
      Result := True;
  end;
end;

procedure TsgcWSServer_Base.DoClear;
begin
  sgcThreadFree(FHeartBeatTimeoutTimer);
  sgcThreadFree(FHeartBeatTimer);
end;

procedure TsgcWSServer_Base.DoDisconnectAll;
var
  i: Integer;
  oConnection: TsgcWSConnectionServer_Base;
  oList, oList2: TList{$IFDEF NEXTGEN}<TsgcWSConnection>{$ENDIF};
begin
  oList2 := TList{$IFDEF NEXTGEN}<TsgcWSConnection>{$ENDIF}.Create;
  Try
    oList := ShareList;
    Try
      // ... get all active connections
      for i := oList.Count - 1 Downto 0 do
      begin
        oConnection := TsgcWSConnectionServer_Base(oList[i]);
        if Assigned(oConnection) then
        begin
          if oConnection.Active and not oConnection.Disconnected then
            oList2.Add(oConnection);
        end;
      end;
    Finally
      UnShareList;
    end;
    // ... disconnect all connections
    for i := oList2.Count - 1 Downto 0 do
      TsgcWSConnectionServer_Base(oList2[i]).DisconnectPeer;
  Finally
    sgcFree(oList2);
  end;
end;

procedure TsgcWSServer_Base.DoHTTPProtocol(const aConnection
  : TsgcWSConnectionServer_Base);
var
  i, j: Integer;
  vPath, vProtocol, vParams: String;
  oParams: TStringList;
  vLength: Int64;
  oStream: TsgcStringStream;
  vBodyExists: Boolean;
  vBody: String;
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
            vLength := sgcGetContentLength(aConnection.HeadersRequest);
            vBody := '';
            if vLength > 0 then
            begin
              oStream := TsgcStringStream.Create('');
              Try
                vBodyExists := False;
                for j := 0 to aConnection.HeadersRequest.Count - 1 do
                begin
                  if aConnection.HeadersRequest.Text = '' then
                    vBodyExists := True
                  else if vBodyExists then
                    oStream.WriteString(aConnection.HeadersRequest.Text);
                end;
                vBody := oStream.DataString;
              Finally
                sgcFree(oStream);
              End;
              aConnection.DoHTTPResponse
                (TsgcWSProtocol_Server_Hack(FProtocolObjectList[i])
                .GetHTTPResponse(aConnection, vPath, vBody), 'text/html');
            end;
          end;
        end;
      end;
    end;
  Finally
    sgcFree(oParams);
  End;
end;

procedure TsgcWSServer_Base.DoReadHeadersRequest(aConnection
  : TsgcWSConnectionServer_Base);
begin
  if Assigned(aConnection) then
    aConnection.DoReadHeadersRequest;
end;

procedure TsgcWSServer_Base.DoStart;
begin
  FWatchDogMonitorSecret := NewGuid;
end;

procedure TsgcWSServer_Base.DoStop;
begin
  FWatchDogMonitorSecret := '';
end;

procedure TsgcWSServer_Base.DoWatchDogMonitor;
var
  oClient: TsgcWSClient;
begin
  if FWatchDogMonitorSecret = '' then
    exit;

  oClient := TsgcWSClient.Create(nil);
  Try
    TsgcWSClient_Hack(oClient).NotifyEvents := neNoSync;
    if Bindings.Count > 0 then
    begin
      oClient.Host := TsgcWSBinding(Bindings.Items[0]).Host;
      oClient.Port := TsgcWSBinding(Bindings.Items[0]).Port;
      oClient.Options.Parameters := TsgcWSBinding(Bindings.Items[0]).Parameters;
      oClient.TLS := TsgcWSBinding(Bindings.Items[0]).SSL;
    end
    else
    begin
      oClient.Host := Host;
      oClient.Port := Port;
      oClient.TLS := SSL;
    end;
    if oClient.TLS then
    begin
      oClient.TLSOptions.IOHandler := iohSChannel;
      oClient.TLSOptions.Version := tls1_2;
    end;

    if RightStr(oClient.Options.Parameters, 1) = '/' then
      oClient.Options.Parameters := oClient.Options.Parameters +
        FWatchDogMonitorSecret
    else
      oClient.Options.Parameters := oClient.Options.Parameters + '/' +
        FWatchDogMonitorSecret;
    if not oClient.Connect(WatchDog.Monitor.Timeout * 1000) then
      ReStart
    else
      oClient.Active := False;
  Finally
    sgcFree(oClient);
  End;
end;

procedure TsgcWSServer_Base.DoXHR(const aConnection
  : TsgcWSConnectionServer_Base);
var
  vGUID: string;
  vLength: Integer;
  oConnection: TsgcWSConnectionServer_Base;
  oStream: TsgcStringStream;
  i: Integer;
  vBody: Boolean;
begin
  vGUID := StringReplace(DecodePOSTFullPath(aConnection.HeadersRequest),
    CS_REQ_XHR, '', [rfReplaceAll]);
  if vGUID <> '' then
  begin
    oConnection := GetConnectionByGuid(vGUID);
    if Assigned(oConnection) then
    begin
      vLength := sgcGetContentLength(aConnection.HeadersRequest);
      if vLength > 0 then
      begin
        oStream := TsgcStringStream.Create('');
        Try
          vBody := False;
          for i := 0 to aConnection.HeadersRequest.Count - 1 do
          begin
            if aConnection.HeadersRequest.Text = '' then
              vBody := True
            else if vBody then
              oStream.WriteString(aConnection.HeadersRequest.Text);
          end;
          oConnection.DoMessageEvent(oStream.DataString);
          aConnection.DoHTTPResponse('', 'text/html');
          exit;
        Finally
          sgcFree(oStream);
        End;
      end;
    end;
  end;
end;

function TsgcWSServer_Base.GetBindings: TsgcWSBindings;
begin
  if not Assigned(FBindings) then
    FBindings := TsgcWSBindings.Create;
  Result := FBindings;
end;

function TsgcWSServer_Base.GetLogFile: TsgcWSLogFile;
begin
  Result := inherited GetLogFile;
  Result.FileName := ExtractFilePath(Result.FileName);
end;

{$IFNDEF NEXTGEN}

function TsgcWSServer_Base.LockList: TList;
begin
{$IFDEF LAZARUS}
  Result := nil
{$ELSE}
  Result := inherited LockList;
{$ENDIF}
end;
{$ENDIF}

function TsgcWSServer_Base.GetSSL: Boolean;
begin
  Result := FSSL;
end;

procedure TsgcWSServer_Base.OnAuth2ResponseErrorEvent
  (aConnection: TsgcWSConnection);
begin
  TsgcWSConnectionServer_Base(aConnection).DoHTTPError(401, 'Unauthorized');
end;

procedure TsgcWSServer_Base.OnHeartBeatExceptionEvent(Sender: TObject;
  E: Exception);
begin
  inherited;
  DoNotifyException(E.Message, E);
end;

procedure TsgcWSServer_Base.OnHeartBeatTimeoutExceptionEvent(Sender: TObject;
  E: Exception);
begin
  inherited;
  DoNotifyException(E.Message, E);
end;

procedure TsgcWSServer_Base.OnHTTPUploadBeforeSaveFileEvent(Sender: TObject;
    var aFileName, aFilePath: string);
begin
  if Assigned(FOnHTTPUploadBeforeSaveFile) then
    FOnHTTPUploadBeforeSaveFile(self, aFileName, aFilePath);
end;

procedure TsgcWSServer_Base.OnHTTPUploadAfterSaveFileEvent(Sender: TObject;
    const aFileName, aFilePath: string);
begin
  if Assigned(FOnHTTPUploadAfterSaveFile) then
    FOnHTTPUploadAfterSaveFile(self, aFileName, aFilePath);
end;

procedure TsgcWSServer_Base.OnOAuth2AuthenticationEvent
  (aConnection: TsgcWSConnection; aUser, aPassword: String;
  var Authenticated: Boolean);
begin
  if Assigned(FOnAuthentication) then
    FOnAuthentication(aConnection, aUser, aPassword, Authenticated);
end;

procedure TsgcWSServer_Base.OnOAuth2ResponseEvent(aConnection: TsgcWSConnection;
  aContent, aContentType: String);
begin
  TsgcWSConnectionServer_Base(aConnection).DoHTTPResponse(aContent,
    aContentType);
end;

procedure TsgcWSServer_Base.OnWatchDogEvent(Sender: TObject);
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
      end;
  else
    begin
      if not Active then
        NotifyMethod(Start)
      else if WatchDog.Monitor.Enabled then
        NotifyMethod(DoWatchDogMonitor);
    end;
  end;
  inherited;
end;

procedure TsgcWSServer_Base.OnWatchDogExceptionEvent(Sender: TObject;
  E: Exception);
begin
  inherited;
  DoNotifyException(E.Message, E);
end;

procedure TsgcWSServer_Base.ReStart;
begin
  TServerThread.ReStart(self);
end;

procedure TsgcWSServer_Base.SetActive(const Value: Boolean);
begin
  if not IsLoading and not IsDesigning then
  begin
    if Value and (FActive = False) then
    begin
      DoStart;
      DoStartHeartBeat;
      DoStartConnectionFree;

      FWatchDogEnabled := WatchDog.Enabled;
      if FWatchDogEnabled then
        DoStartWatchDog;
    end
    else if (Value = False) and FActive then
    begin
      FWatchDogEnabled := False;
      DoStopWatchDog;

      DoStopConnectionFree;
      DoStopHeartBeatTimeout;
      DoStopHeartBeat;
      DoStop;
    end;
  end
  else
    FActive := Value;
end;

procedure TsgcWSServer_Base.SetAuthentication(const Value
  : TsgcWSAuthenticationServer_Options);
begin
  if Assigned(FAuthentication) then
    FAuthentication.Assign(Value);
end;

procedure TsgcWSServer_Base.SetFallBack(const Value: TsgcWSFallBack_Options);
begin
  if Assigned(FFallBack) then
    FFallBack.Assign(Value);
end;

procedure TsgcWSServer_Base.SetOptions(const Value: TsgcWSOptionsServer);
begin
  if Assigned(FOptions) then
    FOptions.Assign(Value);
end;

procedure TsgcWSServer_Base.SetHTTPUploadFiles(const Value
  : TsgcHTTPUploadFilesServer);
begin
  if Assigned(FHTTPUploadFiles) then
    FHTTPUploadFiles.Assign(Value);
end;

procedure TsgcWSServer_Base.SetQueueOptions(const Value
  : TsgcWSQueueServer_Options);
begin
  if Assigned(FQueueOptions) then
    FQueueOptions.Assign(Value);
end;

procedure TsgcWSServer_Base.SetSecurityOptions(const Value
  : TsgcWSSecurity_Options);
begin
  if Assigned(FSecurityOptions) then
    FSecurityOptions.Assign(Value);
end;

procedure TsgcWSServer_Base.SetSSL(const Value: Boolean);
begin
  FSSL := Value;
end;

procedure TsgcWSServer_Base.Start;
begin
  TServerThread.Start(self);
end;

procedure TsgcWSServer_Base.Stop;
begin
  TServerThread.Stop(self);
end;

function TsgcWSServer_Base.WriteData(const aGUID, aMessage: string): Boolean;
begin
  Result := DoWriteData(aGUID, aMessage);
end;

function TsgcWSServer_Base.WriteData(const aGUID: String; aStream: TStream;
  aSize: Integer = 0; const aStreaming: TwsStreaming = stmNone): Boolean;
begin
  Result := DoWriteData(aGUID, aStream, aSize, aStreaming);
end;

constructor TsgcWSConnectionServer_Base.Create;
begin
  inherited;
  FAuthenticationCloseCode := CS_CLOSE_NORMAL;
  FCustomHeartBeat := False;
end;

destructor TsgcWSConnectionServer_Base.Destroy;
begin
  sgcFree(FAuthentication);
  inherited;
end;

function TsgcWSConnectionServer_Base.DoAuthenticationBasic
  (const aParams: string): Boolean;
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

function TsgcWSConnectionServer_Base.DoAuthenticationURL(const aParams
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

procedure TsgcWSConnectionServer_Base.DoAuthSessionResponse
  (const aParams: String);
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

function TsgcWSConnectionServer_Base.DoGetProtocol: string;
var
  i: Integer;
  oSProtocols, oCProtocols: TsgcDelimitedStringList;
begin
  if HandShake.Protocols <> '' then
  begin
    oSProtocols := TsgcDelimitedStringList.Create;
    oCProtocols := TsgcDelimitedStringList.Create;
    Try
      oSProtocols.DelimitedText := Protocol;

      oCProtocols.DelimitedText := HandShake.Protocols;

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

procedure TsgcWSConnectionServer_Base.DoHandshake;
begin
  inherited;
  Specification := HandShake.Specification;

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
end;

procedure TsgcWSConnectionServer_Base.DoHandShakeExtensions;
begin
  Extensions.DecodeExtensions(HandShake.Extensions);
end;

procedure TsgcWSConnectionServer_Base.DoHandShakeSecurity;
begin
  if ServerOriginsAllowed <> '' then
    DoOriginsAllowed;
end;

procedure TsgcWSConnectionServer_Base.DoHandShake_Hixie76;
begin
  HeadersResponse.Add('HTTP/1.1 101 WebSocket Protocol Handshake');
  HeadersResponse.Add('Upgrade: WebSocket');
  HeadersResponse.Add('Connection: Upgrade');
  HeadersResponse.Add('Sec-WebSocket-Origin: ' + HandShake.Origin);
  HeadersResponse.Add('Sec-WebSocket-Location: ws://' + HandShake.Host +
    HandShake.Get);
  if Protocol <> '' then
    HeadersResponse.Add('Sec-WebSocket-Protocol: ' + Protocol)
  else
    HeadersResponse.Add('Sec-WebSocket-Protocol: *');
  HeadersResponse.Add('Server: ' + CS_APPLICATION_NAME + ' ' + CS_VERSION);
end;

procedure TsgcWSConnectionServer_Base.DoHandShake_RFC6455;
begin
  HeadersResponse.Clear;

  HeadersResponse.Add('HTTP/1.1 101 Switching Protocols');
  HeadersResponse.Add('Upgrade: websocket');
  HeadersResponse.Add('Connection: Upgrade');
  if Protocol <> '' then
    HeadersResponse.Add('Sec-WebSocket-Protocol: ' + Protocol);
  HeadersResponse.Add('Sec-WebSocket-Accept: ' + HandShake.GetWSAccept
    (HandShake.Key));
  HeadersResponse.Add('Server: ' + CS_APPLICATION_NAME + ' ' + CS_VERSION);
  Extensions.WriteHeader(HeadersResponse);

end;

function TsgcWSConnectionServer_Base.DoHTTPFileResponse(const aFileName,
  aContentType: String; aDisconnect: Boolean): Boolean;
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
  {
    if IPVersion = Id_IPv6 then
    vLocalIP := '[' + vLocalIP + ']';
  }
  vText := StringReplace(vText, '{%host%}', vLocalIP,
    [rfReplaceAll, rfIgnoreCase]);
  vText := StringReplace(vText, '{%port%}', IntToStr(LocalPort),
    [rfReplaceAll, rfIgnoreCase]);
  if UpperCase(LeftStr(URL, 5)) = 'HTTPS' then
    vText := StringReplace(vText, '{%ssl%}', 's', [rfReplaceAll, rfIgnoreCase])
  else
    vText := StringReplace(vText, '{%ssl%}', '', [rfReplaceAll, rfIgnoreCase]);

  // ... send http response
  if vText <> '' then
  begin
    DoHTTPResponse(vText, aContentType);
    DisconnectPeer; // close if RequestURL response
    Result := True;
  end
  else if aDisconnect then // send error if must disconnect
  begin
    DoHTTPError;
    DisconnectPeer;
    Result := True;
  end;
end;

procedure TsgcWSConnectionServer_Base.DoOnAuthenticationEvent;
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

procedure TsgcWSConnectionServer_Base.DoOriginsAllowed;
var
  oList: TsgcDelimitedStringList;
  i: Integer;
begin
  if HandShake.Origin <> '' then
  begin
    // ... find origin if allowed
    oList := TsgcDelimitedStringList.Create;
    Try
      oList.DelimitedText := ServerOriginsAllowed;
      for i := 0 to oList.Count - 1 do
      begin
        if sgcMatchesMask(HandShake.Origin, oList[i]) then
          exit;
      end;
    Finally
      sgcFree(oList);
    End;

    // ... if not allowed raise exception
    raise TsgcWSException.CreateFmt(S_ORIGIN_NOT_ALLOWED, [HandShake.Origin]);
  end;
end;

procedure TsgcWSConnectionServer_Base.DoProtocolSSE;
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

function TsgcWSConnectionServer_Base.GetAuthentication
  : TsgcWSConnectionAuthentication;
begin
  if not Assigned(FAuthentication) then
    FAuthentication := TsgcWSConnectionAuthentication.Create;
  Result := FAuthentication;
end;

function TsgcWSConnectionServer_Base.GetURL: String;
begin
  if FURL = '' then
  begin
    if Assigned(HeadersRequest) then
    begin
      FURL := DecodeGetFullPath(HeadersRequest);
      if FURL = '' then
        FURL := DecodePOSTFullPath(HeadersRequest);
    end;
  end;
  Result := FURL;
end;

procedure TsgcWSConnectionServer_Base.SetAuthenticationCloseCode
  (const aCloseCode: Integer);
begin
  FAuthenticationCloseCode := aCloseCode;
end;

constructor TsgcWSHandShakeServer_Base.Create(aHeaders: TStringList);
begin
  DoDecodeHeaders(aHeaders);
end;

{ TsgcWSHandShakeServer_Base }

procedure TsgcWSHandShakeServer_Base.DoDecodeConnection(const aHeader: String);
begin
  FIsConnection := True;
  if not sgcMatchesMask(aHeader,
{$IFNDEF LAZARUS}'*Upgrade*'{$ELSE}'*Upgrade'{$ENDIF}) then
    raise TsgcWSException.Create(S_ERROR_DECODING_UPGRADE);
end;

procedure TsgcWSHandShakeServer_Base.DoDecodeExtensions(const aHeader: String);
begin
  FIsExtensions := True;

  if sgcMatchesMask(aHeader, 'Sec-WebSocket-Extensions: *') then
    FExtensions := Copy(aHeader, 27, Length(aHeader) - 26);
end;

procedure TsgcWSHandShakeServer_Base.DoDecodeGET(const aHeader: String);
var
  i: Integer;
  vVersion: String;
begin
  FIsGET := True;

  i := AnsiPos('HTTP', aHeader);
  if i <> -1 then
  begin
    vVersion := MidStr(aHeader, i + 5, Length(aHeader));
    if vVersion = '1.0' then
      raise TsgcWSException.Create(S_ERROR_DECODING_GET);
    FGet := MidStr(aHeader, 5, i - 6);
  end
  else
    raise TsgcWSException.Create(S_ERROR_DECODING_GET);
end;

procedure TsgcWSHandShakeServer_Base.DoDecodeHeaders(aHeaders: TStringList);
var
  i: Integer;
  vText: String;
begin
  for i := 0 to aHeaders.Count - 1 do
  begin
    vText := aHeaders[i];

    if sgcContainsText(vText, 'GET /') then
      DoDecodeGET(vText)
    else if sgcContainsText(vText, 'Upgrade: ') then
      DoDecodeUpgrade(vText)
    else if sgcContainsText(vText, 'Connection: ') then
      DoDecodeConnection(vText)
    else if sgcContainsText(vText, 'Host: ') then
      DoDecodeHost(vText)
    else if sgcContainsText(vText, 'Sec-WebSocket-Key: ') then
      DoDecodeKey(vText)
    else if sgcContainsText(vText, 'Origin: ') then
      DoDecodeOrigin(vText)
    else if sgcContainsText(vText, 'Sec-WebSocket-Protocol: ') then
      DoDecodeProtocols(vText)
    else if sgcContainsText(vText, 'Sec-WebSocket-Version: ') then
      DoDecodeVersion(vText)
    else if sgcContainsText(vText, 'Sec-WebSocket-Extensions: ') then
      DoDecodeExtensions(vText)
  end;

  // validate key
  if FIsKey = False then
    raise TsgcWSException.Create(S_ERROR_DECODING_WEBSOCKET_KEY);
end;

procedure TsgcWSHandShakeServer_Base.DoDecodeHost(const aHeader: String);
begin
  FIsHost := True;

  if not sgcMatchesMask(aHeader, 'Host: *') then
    raise TsgcWSException.Create(S_ERROR_DECODING_HOST);
  FHost := Copy(aHeader, 7, Length(aHeader) - 6);
end;

procedure TsgcWSHandShakeServer_Base.DoDecodeKey(const aHeader: String);
begin
  FIsKey := True;

  if not sgcMatchesMask(aHeader, 'Sec-WebSocket-Key: *') then
    raise TsgcWSException.Create(S_ERROR_DECODING_WEBSOCKET_KEY);
  FKey := Copy(aHeader, 20, Length(aHeader) - 19);
end;

procedure TsgcWSHandShakeServer_Base.DoDecodeKey1(const aHeader: String);
begin
  FIsKey1 := True;

  if not sgcMatchesMask(aHeader, 'Sec-WebSocket-Key1: *') then
    raise TsgcWSException.Create(S_ERROR_DECODING_WEBSOCKET_KEY1);
  FKey1 := Copy(aHeader, 21, Length(aHeader) - 20);
end;

procedure TsgcWSHandShakeServer_Base.DoDecodeKey2(const aHeader: String);
begin
  FIsKey2 := True;

  if not sgcMatchesMask(aHeader, 'Sec-WebSocket-Key2: *') then
    raise TsgcWSException.Create(S_ERROR_DECODING_WEBSOCKET_KEY2);
  FKey2 := Copy(aHeader, 21, Length(aHeader) - 20);
end;

procedure TsgcWSHandShakeServer_Base.DoDecodeOrigin(const aHeader: String);
begin
  FIsOrigin := True;

  if not sgcMatchesMask(aHeader, 'Origin: *') then
    raise TsgcWSException.Create(S_ERROR_DECODING_ORIGIN);
  FOrigin := Copy(aHeader, 9, Length(aHeader) - 8);
end;

procedure TsgcWSHandShakeServer_Base.DoDecodeProtocols(const aHeader: String);
begin
  FIsProtocols := True;

  if sgcMatchesMask(aHeader, 'Sec-WebSocket-Protocol: *') then
    FProtocols := Copy(aHeader, 25, Length(aHeader) - 24);
end;

procedure TsgcWSHandShakeServer_Base.DoDecodeUpgrade(const aHeader: String);
begin
  FIsUpgrade := True;

  if not sgcMatchesMask(aHeader, '*websocket') then
    raise TsgcWSException.Create(S_ERROR_DECODING_WEBSOCKET);
end;

procedure TsgcWSHandShakeServer_Base.DoDecodeVersion(const aHeader: String);
begin
  FIsVersion := True;

  if not sgcMatchesMask(aHeader, 'Sec-WebSocket-Version: *') then
    raise TsgcWSException.Create(S_ERROR_DECODING_WEBSOCKET_VERSION);
  FVersion := Copy(aHeader, 24, Length(aHeader) - 23);
end;

function TsgcWSHandShakeServer_Base.GetSpecification: TwsSpecification;
begin
  Result := spRFC6455;
  if FKey1 <> '' then
    Result := spHixie76;
end;

constructor TsgcWSConnectionAuthentication.Create;
begin
  inherited;
  FAuthenticated := False;
  AuthType := authNone;
end;

function TsgcWSConnectionAuthentication.GetSessionID: string;
begin
  if FSessionID = '' then
    FSessionID := NewGuid;
  Result := FSessionID;
end;

constructor TsgcWSOptionsServer.Create;
begin
  inherited;
  ValidateUTF8 := False;
  JavascriptFiles := True;
  HTMLFiles := False;
  ReadTimeOut := 10;
  WriteTimeOut := 0;
  RaiseDisconnectExceptions := True;
  FragmentedMessages := frgOnlyBuffer;
  CleanDisconnect := False;
end;

procedure TsgcWSOptionsServer.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSOptionsServer then
  begin
    FValidateUTF8 := TsgcWSOptionsServer(aSource).ValidateUTF8;
    FCleanDisconnect := TsgcWSOptionsServer(aSource).CleanDisconnect;
    FJavascriptFiles := TsgcWSOptionsServer(aSource).JavascriptFiles;
    FHTMLFiles := TsgcWSOptionsServer(aSource).HTMLFiles;
    FReadTimeOut := TsgcWSOptionsServer(aSource).ReadTimeOut;
    FRaiseDisconnectExceptions := TsgcWSOptionsServer(aSource)
      .RaiseDisconnectExceptions;
    FragmentedMessages := TsgcWSOptionsServer(aSource).FragmentedMessages;
  end
  else
    inherited Assign(aSource);
end;

function TsgcWSOptionsServer.GetReadTimeOut: Integer;
begin
  if FReadTimeOut <= 0 then
    FReadTimeOut := 1;
  Result := FReadTimeOut;
end;

procedure TsgcWSSecurity_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSSecurity_Options then
  begin
    FOriginsAllowed := TsgcWSSecurity_Options(aSource).OriginsAllowed;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcWSAuthenticationServer_Options.Create;
begin
  FAuthUsers := TStringList.Create;
  FURL := TsgcWSAuthenticationServer_URL.Create;
  FSession := TsgcWSAuthenticationServer_Session.Create;
  FBasic := TsgcWSAuthenticationServer_Basic.Create;
  FOAuth := TsgcWSAuthenticationServer_OAuth.Create;
  FJWT := TsgcWSAuthenticationServer_JWT.Create;
  AllowNonAuth := False;
end;

destructor TsgcWSAuthenticationServer_Options.Destroy;
begin
  sgcFree(FJWT);
  sgcFree(FOAuth);
  sgcFree(FBasic);
  sgcFree(FURL);
  sgcFree(FSession);
  sgcFree(FAuthUsers);
  inherited;
end;

procedure TsgcWSAuthenticationServer_Options.Assign(aSource: TPersistent);
begin
  inherited Assign(aSource);
  if aSource is TsgcWSAuthenticationServer_Options then
  begin
    FSession.Assign(TsgcWSAuthenticationServer_Options(aSource).Session);
    FURL.Assign(TsgcWSAuthenticationServer_Options(aSource).URL);
    FBasic.Assign(TsgcWSAuthenticationServer_Options(aSource).Basic);
    FOAuth.Assign(TsgcWSAuthenticationServer_Options(aSource).OAuth);
    FJWT.Assign(TsgcWSAuthenticationServer_Options(aSource).JWT);
    AllowNonAuth := TsgcWSAuthenticationServer_Options(aSource).AllowNonAuth;
    FAuthUsers.Text := TsgcWSAuthenticationServer_Options(aSource)
      .AuthUsers.Text;
  end
end;

procedure TsgcWSAuthenticationServer_Options.SetAuthUsers
  (const Value: TStringList);
begin
  if Assigned(FAuthUsers) then
    FAuthUsers.Assign(Value);
end;

procedure TsgcWSAuthenticationServer_Options.SetURL
  (const Value: TsgcWSAuthenticationServer_URL);
begin
  if Assigned(FURL) then
    FURL.Assign(Value);
end;

procedure TsgcWSAuthenticationServer_Options.SetSession
  (const Value: TsgcWSAuthenticationServer_Session);
begin
  if Assigned(FSession) then
    FSession.Assign(Value);
end;

procedure TsgcWSAuthenticationServer_Options.SetBasic
  (const Value: TsgcWSAuthenticationServer_Basic);
begin
  if Assigned(FBasic) then
    FBasic.Assign(Value);
end;

procedure TsgcWSAuthenticationServer_Options.SetJWT
  (const Value: TsgcWSAuthenticationServer_JWT);
begin
  if Assigned(FJWT) then
    FJWT.Assign(Value);
end;

procedure TsgcWSAuthenticationServer_Options.SetOAuth
  (const Value: TsgcWSAuthenticationServer_OAuth);
begin
  if Assigned(FOAuth) then
    FOAuth.Assign(Value);
end;

constructor TsgcWSFallBack_Flash.Create;
begin
  inherited;
  Enabled := False;
  Domain := '*';
  Ports := '*';
end;

constructor TsgcWSFallBack_SSE.Create;
begin
  inherited;
  Enabled := False;
  Retry := 3000;
end;

constructor TsgcWSFallBack_Options.Create;
begin
  inherited;
  FFlash := TsgcWSFallBack_Flash.Create;
  FServerSentEvents := TsgcWSFallBack_SSE.Create;
end;

destructor TsgcWSFallBack_Options.Destroy;
begin
  sgcFree(FServerSentEvents);
  sgcFree(FFlash);
  inherited;
end;

procedure TsgcWSFallBack_Options.Assign(aSource: TPersistent);
begin
  inherited Assign(aSource);
  if aSource is TsgcWSFallBack_Options then
  begin
    FFlash.Assign(TsgcWSFallBack_Options(aSource).Flash);
    FServerSentEvents.Assign(TsgcWSFallBack_Options(aSource).ServerSentEvents);
  end;
end;

procedure TsgcWSFallBack_Options.SetFlash(const Value: TsgcWSFallBack_Flash);
begin
  if Assigned(FFlash) then
    FFlash.Assign(Value);
end;

procedure TsgcWSFallBack_Options.SetServerSentEvents
  (const Value: TsgcWSFallBack_SSE);
begin
  if Assigned(FServerSentEvents) then
    FServerSentEvents.Assign(Value);
end;

constructor TsgcWSBinding.Create;
begin
  inherited;
  Host := '';
  Port := 0;
  Parameters := '';
  SSL := False;
  SSLHash := '';
  IPVersion := ipV4;
end;

procedure TsgcWSBinding.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSBinding then
  begin
    Host := TsgcWSBinding(aSource).Host;
    Port := TsgcWSBinding(aSource).Port;
    Parameters := TsgcWSBinding(aSource).Parameters;
    SSL := TsgcWSBinding(aSource).SSL;
    SSLHash := TsgcWSBinding(aSource).SSLHash;
    SSLCertStoreName := TsgcWSBinding(aSource).SSLCertStoreName;
    IPVersion := TsgcWSBinding(aSource).IPVersion;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSBindings.Clear;
var
  i: Integer;
begin
  for i := Count - 1 Downto 0 do
  begin
{$IFNDEF NEXTGEN}
    TsgcWSBinding(Items[i]).Free;
{$ENDIF}
    Delete(i);
  end;
  inherited;
end;

function TsgcWSBindings.GetURL(const aIndex: Integer): String;
begin
  Result := '';
  if aIndex < Count then
  begin
    Result := 'http';
    if TsgcWSBinding(Items[aIndex]).SSL then
      Result := Result + 's';
    Result := Result + '://' + TsgcWSBinding(Items[aIndex]).Host;
    Result := Result + ':' + IntToStr(TsgcWSBinding(Items[aIndex]).Port);
    if TsgcWSBinding(Items[aIndex]).Parameters <> '' then
    begin
      if LeftStr(TsgcWSBinding(Items[aIndex]).Parameters, 1) <> '/' then
        Result := Result + '/' + TsgcWSBinding(Items[aIndex]).Parameters
      else
        Result := Result + TsgcWSBinding(Items[aIndex]).Parameters;
    end;
    if RightStr(Result, 1) <> '/' then
      Result := Result + '/';
  end;
end;

procedure TsgcWSBindings.NewBinding(const aHost: string; const aPort: Integer;
  const aParameters: string = ''; const aSSL: Boolean = False;
  const aSSLHash: String = ''; const aIPVersion: TwsIPVersion = ipV4;
  const aCertStoreName: String = '');
var
  oBinding: TsgcWSBinding;
begin
  oBinding := TsgcWSBinding.Create;

  oBinding.Host := aHost;
  oBinding.Port := aPort;
  oBinding.Parameters := aParameters;
  oBinding.SSL := aSSL;
  oBinding.IPVersion := aIPVersion;
  oBinding.SSLHash := aSSLHash;
  oBinding.SSLCertStoreName := aCertStoreName;

  Add(oBinding);
end;

procedure TsgcWSBindings.NewBinding(const aBinding: TsgcWSBinding);
var
  oBinding: TsgcWSBinding;
begin
  oBinding := TsgcWSBinding.Create;
  oBinding.Assign(aBinding);
  Add(oBinding);
end;

constructor TsgcWSHTTP2Server_Options.Create;
begin
  inherited;
  Enabled := False;
  FSettings := TsgcHTTP2ServerSettings.Create;
  FEvents := TsgcHTTP2ServerEvents.Create;
  FAltSvc := TsgcWSHTTP2AlternateService.Create;
  FFragmentedData := h2fdOnlyBuffer;
end;

destructor TsgcWSHTTP2Server_Options.Destroy;
begin
  sgcFree(FAltSvc);
  sgcFree(FEvents);
  sgcFree(FSettings);
  inherited;
end;

procedure TsgcWSHTTP2Server_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSHTTP2Server_Options then
  begin
    Enabled := TsgcWSHTTP2Server_Options(aSource).Enabled;
    Settings := TsgcWSHTTP2Server_Options(aSource).Settings;
    FragmentedData := TsgcWSHTTP2Server_Options(aSource).FragmentedData;
    AltSvc := TsgcWSHTTP2Server_Options(aSource).AltSvc;
    Events := TsgcWSHTTP2Server_Options(aSource).Events;
  end
  else
    inherited Assign(aSource);
end;

function TsgcWSHTTP2Server_Options.GetEnabled: Boolean;
begin
{$IFDEF SGC_HTTP2}
  Result := FEnabled;
{$ELSE}
  Result := False;
{$ENDIF}
end;

procedure TsgcWSHTTP2Server_Options.SetAltSvc(const Value
  : TsgcWSHTTP2AlternateService);
begin
  if Assigned(FAltSvc) then
    FAltSvc.Assign(Value);
end;

procedure TsgcWSHTTP2Server_Options.SetEvents(const Value
  : TsgcHTTP2ServerEvents);
begin
  if Assigned(FEvents) then
    FEvents.Assign(Value);
end;

procedure TsgcWSHTTP2Server_Options.SetSettings(const Value
  : TsgcHTTP2ServerSettings);
begin
  if Assigned(FSettings) then
    FSettings.Assign(Value);
end;

constructor TsgcHTTP2ServerSettings.Create;
begin
  inherited;
{$IFDEF SGC_HTTP2}
  FHeaderTableSize := CS_HTTP2_DEFAULT_HEADER_TABLE_SIZE;
  FEnablePush := CS_HTTP2_DEFAULT_ENABLE_PUSH;
  FMaxConcurrentStreams := CS_HTTP2_DEFAULT_MAX_CONCURRENT_STREAMS;
  FInitialWindowSize := CS_HTTP2_DEFAULT_INITIAL_WINDOW_SIZE;
  FMaxFrameSize := CS_HTTP2_DEFAULT_MAX_FRAME_SIZE;
  FMaxHeaderListSize := CS_HTTP2_DEFAULT_MAX_HEADER_LIST_SIZE;
  FEnableConnectProtocol := CS_HTTP2_DEFAULT_ENABLE_CONNECT_PROTOCOL;
{$ELSE}
  FHeaderTableSize := 4096;
  FEnablePush := True;
  FMaxConcurrentStreams := MaxInt;
  FInitialWindowSize := 65535;
  FMaxFrameSize := 16384;
  FMaxHeaderListSize := MaxInt;
  FEnableConnectProtocol := False;
{$ENDIF}
end;

procedure TsgcHTTP2ServerSettings.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTP2ServerSettings then
  begin
    EnableConnectProtocol := TsgcHTTP2ServerSettings(aSource)
      .EnableConnectProtocol;
    EnablePush := TsgcHTTP2ServerSettings(aSource).EnablePush;
    HeaderTableSize := TsgcHTTP2ServerSettings(aSource).HeaderTableSize;
    InitialWindowSize := TsgcHTTP2ServerSettings(aSource).InitialWindowSize;
    MaxConcurrentStreams := TsgcHTTP2ServerSettings(aSource)
      .MaxConcurrentStreams;
    MaxFrameSize := TsgcHTTP2ServerSettings(aSource).MaxFrameSize;
    MaxHeaderListSize := TsgcHTTP2ServerSettings(aSource).MaxHeaderListSize;
  end
  else
    inherited Assign(aSource);
end;

constructor TServerThread.Create(const aMethod: string;
  const aServer: TsgcWSServer_Base);
begin
  FMethod := aMethod;
  FServer := aServer;
  FreeOnTerminate := True;
  inherited Create(False);
end;

class procedure TServerThread.Start(const aServer: TsgcWSServer_Base);
begin
  Create(CS_START, aServer);
end;

class procedure TServerThread.Stop(const aServer: TsgcWSServer_Base);
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
      if FServer.AssignedCS then
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
      if FServer.AssignedCS then
      begin
        FServer.EnterCS;
        Try
          FServer.Active := False;
        Finally
          FServer.LeaveCS;
        End;
      end;
    end
    else if FMethod = CS_RESTART then
    begin
      if FServer.AssignedCS then
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

class procedure TServerThread.ReStart(const aServer: TsgcWSServer_Base);
begin
  Create(CS_RESTART, aServer);
end;

constructor TsgcHTTP2ServerEvents.Create;
begin
  inherited;
  OnConnect := False;
  OnDisconnect := False;
end;

procedure TsgcHTTP2ServerEvents.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTP2ServerEvents then
  begin
    OnConnect := TsgcHTTP2ServerEvents(aSource).OnConnect;
    OnDisconnect := TsgcHTTP2ServerEvents(aSource).OnDisconnect;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcWSHTTP2AlternateService.Create;
begin
  inherited;
  FEnabled := True;
end;

procedure TsgcWSHTTP2AlternateService.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSHTTP2AlternateService then
  begin
    Enabled := TsgcWSHTTP2AlternateService(aSource).Enabled;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcHTTPUploadFilesServer.Create;
begin
  inherited;
  FStreamType := pstMemoryStream;
  FMinSize := 0;
  FRemoveBoundaries := True;
end;

procedure TsgcHTTPUploadFilesServer.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTPUploadFilesServer then
  begin
    StreamType := TsgcHTTPUploadFilesServer(aSource).StreamType;
    SaveDirectory := TsgcHTTPUploadFilesServer(aSource).SaveDirectory;
    MinSize := TsgcHTTPUploadFilesServer(aSource).MinSize;
    RemoveBoundaries := TsgcHTTPUploadFilesServer(aSource).RemoveBoundaries;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcHTTPUploadFilesServer.SetStreamType
  (const Value: TwsPostStreamType);
begin
{$IFDEF INDY10_5_7}
  FStreamType := Value;
{$ELSE}
  FStreamType := pstMemoryStream;
{$ENDIF}
end;

constructor TsgcWSAuthenticationServer_JWT.Create;
begin
  inherited;
  FJWT := nil;
end;

constructor TsgcWSAuthenticationServer_OAuth.Create;
begin
  inherited;
  FOAuth2 := nil;
end;

end.
