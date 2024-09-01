{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Classes;

interface

{$I sgcVer.inc}

uses
  Classes, Math, SysUtils, StrUtils,
{$IFDEF NEXTGEN}System.Generics.Collections, {$ELSE}Contnrs, {$ENDIF}
{$IFDEF MSWINDOWS}Windows, {$ENDIF}
  // macos
{$IFDEF MACOS}
{$IFNDEF APPMETHOD}
  types,
{$ENDIF}
{$ENDIF}
  // xe
{$IFDEF DXE7}
{$IFNDEF IOS}
{$IFNDEF MACOS}
  System.types,
{$ENDIF}
{$ENDIF}
{$ENDIF}
  // indy
{$IFDEF SGC_INDY}sgcIdThread{$ELSE}IdThread{$ENDIF},
{$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
{$IFDEF SGC_INDY}sgcIdContext{$ELSE}IdContext{$ENDIF},
  // sgc
  sgcBase_Classes, sgcBase_Const,
  // socket
  sgcSocket_Classes,
  // tcp
  sgcTCP_Classes,
  // websocket
  sgcWebSocket_Extensions, sgcWebSocket_Types, sgcWebSocket_Helpers,
  sgcWebSocket_Const, sgcBase_Helpers,
  sgcWebSocket_Classes_SyncObjs, sgcWebSocket_Classes_Queues;

type

  TsgcWSConnection = Class;

  TsgcWSProtocol = class;
  TsgcWSAPI = class;
{$IFDEF NEXTGEN}

  TsgcWSProtocols = class(TList<TsgcWSProtocol>)
  private
    FOwnsObjects: Boolean;
  public
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;
{$ENDIF}

  TsgcWS_ProtocolClass = Class of TsgcWSProtocol;

  TsgcWSMessageEvent = procedure(Connection: TsgcWSConnection;
    const Text: string) of object;
  TsgcWSBinaryEvent = procedure(Connection: TsgcWSConnection;
    const Data: TMemoryStream) of object;
  TsgcWSFragmentedEvent = procedure(Connection: TsgcWSConnection;
    const Data: TMemoryStream; const OpCode: TOpCode;
    const Continuation: Boolean) of object;
  TsgcWSConnectEvent = procedure(Connection: TsgcWSConnection) of object;
  TsgcWSDisconnectEvent = procedure(Connection: TsgcWSConnection; Code: Integer)
    of object;
  TsgcWSErrorEvent = procedure(Connection: TsgcWSConnection;
    const Error: string) of object;
  TsgcWSSubscriptionEvent = procedure(Connection: TsgcWSConnection;
    const Subscription: String) of object;
  TsgcWSBeforeSubscriptionEvent = procedure(Connection: TsgcWSConnection;
    const Subscription: String; var Accept: Boolean) of object;
  TsgcWSHandshakeEvent = procedure(Connection: TsgcWSConnection;
    var Headers: TStringList) of object;
  TsgcWSUpdateEvent = procedure(Connection: TsgcWSConnection; aType: Integer)
    of object;
  TsgcWSUnknownProtocolEvent = procedure(Connection: TsgcWSConnection;
    var Accept: Boolean) of object;
  TsgcWSOnBeforeHeartBeatEvent = procedure(Sender: TObject;
    const Connection: TsgcWSConnection; var Handled: Boolean) of object;
  TsgcWSOnBeforeWatchDogEvent = procedure(Sender: TObject; var Handled: Boolean)
    of object;
  TsgcWSOnBeforeConnectEvent = procedure(Sender: TObject) of Object;

  TsgcExceptionEvent = procedure(Connection: TsgcWSConnection; E: Exception)
    of object;
  TsgcWSRawMessageEvent = procedure(Connection: TsgcWSConnection;
    const Text: string; var Handled: Boolean) of object;
  TsgcWSLoadBalancerErrorEvent = procedure(Sender: TObject; const Error: String)
    of object;

  TsgcWSException = class(Exception)
  end;

  TsgcQueueItemConnection = class(TsgcQueueItemBase)
  private
    FConnection: TsgcWSConnection;
    procedure SetConnection(const Value: TsgcWSConnection);
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    property Connection: TsgcWSConnection read FConnection write SetConnection;
  end;

  TsgcCacheQueueListConnections = class(TsgcCacheQueueList)
  public
    function GetConnection(const aGuid: String): TsgcWSConnection;
  end;

  TsgcQueueItemChannel = class(TsgcQueueNameItemBase)
  private
    FConnection: TsgcWSConnection;
    procedure SetConnection(const Value: TsgcWSConnection);
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    property Connection: TsgcWSConnection read FConnection write SetConnection;
  end;

  TsgcQueueListChannels = class(TsgcQueueNameListBase)
  public
    procedure GetConnections(const aChannel: String;
      var aList: TList{$IFDEF NEXTGEN}<TsgcWSConnection>{$ENDIF});
    procedure GetChannels(const aID: String;
      var aList: TList{$IFDEF NEXTGEN}<TsgcQueueItemChannel>{$ENDIF});
  end;

  TsgcQueueList = class(TsgcThreadList{$IFDEF NEXTGEN}<TObject>{$ENDIF})
  public
    destructor Destroy; override;
  public
    procedure FreeObjects;
  end;

  TsgcQueueObject = class
  private
    FSize: Integer;
    FStreaming: TwsStreaming;
    FText: String;
    FOpCode: TOpCode;
    FStream: TMemoryStream;
    function GetStream: TStream;
    procedure SetStream(const Value: TStream);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    property Size: Integer read FSize write FSize;
    property Streaming: TwsStreaming read FStreaming write FStreaming;
    property Text: String read FText write FText;
    property OpCode: TOpCode read FOpCode write FOpCode;
    property Stream: TStream read GetStream write SetStream;
  end;

  TsgcWSNotifyObject = class
  private
    FConnection: TsgcWSConnection;
    FInt: Integer;
    FRawException: Exception;
    FStream: TMemoryStream;
    FText: String;
    FText1: String;
    procedure SetRawException(const Value: Exception);
  protected
    function GetStream: TMemoryStream;
  public
    destructor Destroy; override;
  public
    property Connection: TsgcWSConnection read FConnection write FConnection;
    property Int: Integer read FInt write FInt;
    property RawException: Exception read FRawException write SetRawException;
    property Stream: TMemoryStream read GetStream write FStream;
    property Text: String read FText write FText;
    property Text1: String read FText1 write FText1;
  end;

  TsgcWSObjectList = class
    (TsgcThreadList{$IFDEF NEXTGEN}<TsgcWSNotifyObject>{$ENDIF})
  private
    procedure DoAdd(Item: Pointer);
  public
    procedure AddNotifyObject(aConnection: TsgcWSConnection); overload;
    procedure AddNotifyObject(aConnection: TsgcWSConnection;
      aParam: String); overload;
    procedure AddNotifyObject(aConnection: TsgcWSConnection; aParam: String;
      aParam1: String); overload;
    procedure AddNotifyObject(aConnection: TsgcWSConnection;
      aParam: Integer); overload;
    procedure AddNotifyObject(aConnection: TsgcWSConnection;
      aParam: TMemoryStream); overload;
    procedure AddNotifyObject(aConnection: TsgcWSConnection;
      aStream: TMemoryStream; aOpCode: TOpCode; aBoolean: Boolean); overload;
    procedure AddNotifyObject(aConnection: TsgcWSConnection; aParam: String;
      aException: Exception); overload;
  public
    procedure DeleteAll(aFreeConnection: Boolean = False);
  end;

  TsgcOpenSSLServer_Options = class(TsgcTCPOpenSSL_Options)
  private
    FECDHE: Boolean;
  public
    constructor Create; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property ECDHE: Boolean read FECDHE write FECDHE;
  end;

  TsgcWSSSL_Options = class(TsgcTCPTLS_Options_Base)
  private
    FOpenSSL_Options: TsgcOpenSSLServer_Options;
    FPort: Integer;
    procedure SetOpenSSL_Options(const Value: TsgcOpenSSLServer_Options);
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property OpenSSL_Options: TsgcOpenSSLServer_Options read FOpenSSL_Options
      write SetOpenSSL_Options;
    property Port: Integer read FPort write FPort;
  end;

  TsgcHTTPRequest = class
  private
    FContent: string;
    FDocument: string;
    FMethod: string;
    FQueryParams: string;
  public
    property Content: string read FContent write FContent;
    property Document: string read FDocument write FDocument;
    property Method: string read FMethod write FMethod;
    property QueryParams: string read FQueryParams write FQueryParams;
  end;

  TsgcHTTPResponse = class
  private
    FCode: Integer;
    FContent: string;
    FContentType: string;
  public
    property Code: Integer read FCode write FCode;
    property Content: string read FContent write FContent;
    property ContentType: string read FContentType write FContentType;
  end;


  // TsgcWSHeartBeat_Options //

  TsgcWSHeartBeat_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FInterval: Integer;
    FTimeout: Integer;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property Interval: Integer read FInterval write FInterval;
    property Timeout: Integer read FTimeout write FTimeout;
  end;

  // TsgcWSConnectionsFree_Options //

  TsgcWSConnectionsFree_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FInterval: Integer;
    function GetEnabled: Boolean;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read GetEnabled write FEnabled;
    property Interval: Integer read FInterval write FInterval;
  end;


  // TsgcWSConnections_Options //

  TsgcWSConnections_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FCacheSize: Integer;
    FGroupLevel: Integer;
    function GetEnabled: Boolean;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read GetEnabled write FEnabled;
    property CacheSize: Integer read FCacheSize write FCacheSize;
    property GroupLevel: Integer read FGroupLevel write FGroupLevel;
  end;

  TsgcWSChannels_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    function GetEnabled: Boolean;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read GetEnabled write FEnabled;
  end;

  TsgcWSOptimization_Options = class(TPersistent)
  private
    FConnections: TsgcWSConnections_Options;
    FChannels: TsgcWSChannels_Options;
    FConnectionsFree: TsgcWSConnectionsFree_Options;
    procedure SetConnections(const Value: TsgcWSConnections_Options);
    procedure SetChannels(const Value: TsgcWSChannels_Options);
    procedure SetConnectionsFree(const Value: TsgcWSConnectionsFree_Options);
  public
    constructor Create; virtual;
  public
    destructor Destroy; override;
    procedure Assign(aSource: TPersistent); override;
  published
    property Connections: TsgcWSConnections_Options read FConnections
      write SetConnections;
    property Channels: TsgcWSChannels_Options read FChannels write SetChannels;
    property ConnectionsFree: TsgcWSConnectionsFree_Options
      read FConnectionsFree write SetConnectionsFree;
  end;

  TsgcWSTLS_Options = class(TsgcTCPTLS_Options)
  end;

  TsgcWSWatchDog_Options = class(TsgcTCPWatchDog_Options)

  end;

  TsgcWSWatchDogClient_Options = class(TsgcWSWatchDog_Options)

  end;

  TsgcWSWatchDogMonitorServer_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FTimeout: Integer;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property Timeout: Integer read FTimeout write FTimeout;
  end;

  TsgcWSWatchDogServer_Options = class(TsgcWSWatchDog_Options)
  private
    FMonitor: TsgcWSWatchDogMonitorServer_Options;
    procedure SetMonitor(const Value: TsgcWSWatchDogMonitorServer_Options);
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Monitor: TsgcWSWatchDogMonitorServer_Options read FMonitor
      write SetMonitor;
  end;

  TsgcWSDrafts = class(TPersistent)
  private
    FHixie76: Boolean;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Hixie76: Boolean read FHixie76 write FHixie76;
  end;

  TsgcWSSpecifications = class(TPersistent)
  private
    FDrafts: TsgcWSDrafts;
    FRFC6455: Boolean;
    FRFC8441: Boolean;
    procedure SetDrafts(const Value: TsgcWSDrafts);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property Drafts: TsgcWSDrafts read FDrafts write SetDrafts;
    property RFC6455: Boolean read FRFC6455 write FRFC6455;
  public
    property RFC8441: Boolean read FRFC8441 write FRFC8441;
  end;

  TsgcWSQueue = class(TPersistent)
  private
    FLevel: TwsQueueMsgLevels;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Level: TwsQueueMsgLevels read FLevel write FLevel;
  end;

  TsgcWSQueueOptions = class(TPersistent)
  private
    FBinary: TsgcWSQueue;
    FPing: TsgcWSQueue;
    FText: TsgcWSQueue;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Binary: TsgcWSQueue read FBinary write FBinary;
    property Ping: TsgcWSQueue read FPing write FPing;
    property Text: TsgcWSQueue read FText write FText;
  end;

  TsgcWSHandshake_Base = class
    { properties }
  protected
    FProtocols: String;
    FExtensions: String;
  public
    property Protocols: String read FProtocols;
    property Extensions: String read FExtensions;
    { properties }
  end;

{$IFDEF NEXTGEN}

  TsgcWSHandshakes = class(TList<TsgcWSConnection>)
  end;
{$ENDIF}

  TMaskArray = array [0 .. 3] of Byte;

  TsgcWSConnection = class(TsgcTCPConnection)
    { helpers }
  protected
    function IsControlFrame(aOpCode: TOpCode): Boolean;
    { helpers }

    { extensions }
  private
    FExtensions: TsgcWSExtensions;
  protected
    procedure SetExtensions(const Value: TsgcWSExtensions); virtual;
  public
    property Extensions: TsgcWSExtensions read FExtensions write SetExtensions;
    { extensions }

    { queues }
  protected
    FQueueLevel0: TsgcQueueList;
    FQueueLevel1: TsgcQueueList;
    FQueueLevel2: TsgcQueueList;
    function GetQueueLevel0: TsgcQueueList;
    function GetQueueLevel1: TsgcQueueList;
    function GetQueueLevel2: TsgcQueueList;
  protected
    property QueueLevel0: TsgcQueueList read GetQueueLevel0 write FQueueLevel0;
    property QueueLevel1: TsgcQueueList read GetQueueLevel1 write FQueueLevel1;
    property QueueLevel2: TsgcQueueList read GetQueueLevel2 write FQueueLevel2;
  private
    FQueueOptions: TsgcWSQueueOptions;
    procedure SetQueueOptions(const Value: TsgcWSQueueOptions);
  public
    property QueueOptions: TsgcWSQueueOptions read FQueueOptions
      write SetQueueOptions;
    { queues }

    { protocol fields }
  private
    FCloseCode: Integer;
    FCloseReason: String;
    FMsgError: String;
    FSpecification: TwsSpecification;
  public
    property CloseCode: Integer read FCloseCode write FCloseCode;
    property MsgError: String read FMsgError write FMsgError;
    property Specification: TwsSpecification read FSpecification
      write FSpecification;
    property CloseReason: String read FCloseReason write FCloseReason;
    { protocol fields }

    { fields }
  private
    FMasked: Boolean;
    FProtocol: String;
    FValidateUTF8: Boolean;
    FRaiseDisconnectExceptions: Boolean;
    FFragmentedMessages: TwsFragmentedMessages;
    FCleanDisconnect: Boolean;
  protected
    FURL: String;
    function GetURL: String; virtual; abstract;
  public
    property Masked: Boolean read FMasked write FMasked;
    property Protocol: String read FProtocol write FProtocol;
    property URL: String read GetURL;
    property ValidateUTF8: Boolean read FValidateUTF8 write FValidateUTF8;
    property RaiseDisconnectExceptions: Boolean read FRaiseDisconnectExceptions
      write FRaiseDisconnectExceptions;
    property FragmentedMessages: TwsFragmentedMessages read FFragmentedMessages
      write FFragmentedMessages;
    property CleanDisconnect: Boolean read FCleanDisconnect
      write FCleanDisconnect;
    { fields }

    { protocol data }
  private
    FProtocolData: TsgcQueue;
    function GetProtocolData: TsgcQueue;
  protected
    property ProtocolData: TsgcQueue read GetProtocolData write FProtocolData;
    { protocol data }

    { api data }
  private
    FApiData: TObject;
  protected
    property ApiData: TObject read FApiData write FApiData;
    { api data }

    { subscriptions }
  private
    FSubscriptions: TStringList;
    FLastSubscription: String;
    FLastUnSubscription: String;
    function GetSubscriptions: TStringList;
    procedure SetDisconnected(const Value: Boolean);
  public
    procedure DoSubscribe(const aChannels: String);
    procedure DoUnSubscribe(const aChannels: String);
  public
    function Subscribed(const aChannel: String): Boolean;
    property Subscriptions: TStringList read GetSubscriptions
      write FSubscriptions;
    property LastSubscription: String read FLastSubscription
      write FLastSubscription;
    property LastUnSubscription: String read FLastUnSubscription
      write FLastUnSubscription;
    { subscriptions }

    { disconnect }
  private
    FDisconnected: Boolean;
  public
    procedure Disconnect; overload; virtual;
    procedure Disconnect(const aCloseCode: Integer); overload; virtual;
  protected
    procedure DoClose(aCloseCode: Integer = CS_CLOSE_NORMAL); virtual;
  public
    procedure Close; overload; virtual;
    procedure Close(const aCloseCode: Integer); overload; virtual;
  public
    property Disconnected: Boolean read FDisconnected write SetDisconnected;
    { disconnect }

    { ping / pong }
  private
    FLastPing: TDateTime;
    FLastPong: TDateTime;
    FFirstPing: TDateTime;
  protected
    FPong: string;
  protected
    procedure DoQueuePing(const aText: string); virtual;
  protected
    procedure DoPing(const aText: String); virtual;
  public
    procedure Ping(const aText: string); overload;
    function Ping(const aText: string; aTimeOut: Integer): Boolean; overload;
  public
    property LastPing: TDateTime read FLastPing write FLastPing;
    property FirstPing: TDateTime read FFirstPing write FFirstPing;
    property LastPong: TDateTime read FLastPong write FLastPong;
    { ping / pong }

    { Handshake }
  protected
    FHeadersResponse: TStringList;
  protected
    FEnabled: Boolean;
    function GetHeadersResponse: TStringList;
  public
    property HeadersResponse: TStringList read GetHeadersResponse;
    property Enabled: Boolean read FEnabled;
    { Handshake }

    { events }
  protected
    FOnUpdate: TsgcWSUpdateEvent;
    FOnMessage: TsgcWSMessageEvent;
    FOnBinary: TsgcWSBinaryEvent;
    FOnHandshake: TsgcWSHandshakeEvent;
    FOnSubscription: TsgcWSSubscriptionEvent;
    FOnUnSubscription: TsgcWSSubscriptionEvent;
    FOnFragmented: TsgcWSFragmentedEvent;
    procedure DoMessageEvent(const aText: String);
    procedure DoBinaryEvent(const aData: TMemoryStream);
    procedure DoFragmentedEvent(const aData: TMemoryStream;
      const aOpCode: TOpCode; const aContinuation: Boolean);
  protected
    FNotifyConnection: Boolean;
    FNotifyDisconnection: Boolean;
  protected
    property NotifyConnection: Boolean read FNotifyConnection;
    property NotifyDisconnection: Boolean read FNotifyDisconnection;
  public
    property OnUpdate: TsgcWSUpdateEvent read FOnUpdate write FOnUpdate;
    property OnMessage: TsgcWSMessageEvent read FOnMessage write FOnMessage;
    property OnBinary: TsgcWSBinaryEvent read FOnBinary write FOnBinary;
    property OnFragmented: TsgcWSFragmentedEvent read FOnFragmented
      write FOnFragmented;
    property OnSubscription: TsgcWSSubscriptionEvent read FOnSubscription
      write FOnSubscription;
    property OnUnSubscription: TsgcWSSubscriptionEvent read FOnUnSubscription
      write FOnUnSubscription;
    property OnHandshake: TsgcWSHandshakeEvent read FOnHandshake
      write FOnHandshake;
    { events }

    { write data }
  protected
    procedure DoWriteData(const aText: string; const aSize: Integer = 0;
      const aStreaming: TwsStreaming = stmNone); overload; virtual; abstract;
    procedure DoWriteData(const aStream: TStream; const aSize: Integer = 0;
      const aStreaming: TwsStreaming = stmNone); overload; virtual; abstract;
  protected
    procedure DoQueueData(const aText: string; const aSize: Integer = 0;
      const aStreaming: TwsStreaming = stmNone); overload;
    procedure DoQueueData(const aStream: TStream; const aSize: Integer = 0;
      const aStreaming: TwsStreaming = stmNone); overload;
  public
    procedure WriteData(const aText: string; const aSize: Integer = 0;
      const aStreaming: TwsStreaming = stmNone); overload;
    procedure WriteData(const aStream: TStream; const aSize: Integer = 0;
      const aStreaming: TwsStreaming = stmNone); overload;
  public
    function WriteAndWaitData(const aText: String;
      const aTimeOut: Integer = 10000): string;
    { write data }

    { constructor }
  public
    constructor Create; override;
    destructor Destroy; override;
    { constructor }

  end;

  TsgcWSConnection_Abstract = class(TsgcWSConnection)
    { read data }
  protected
    function GetReadBufferSize: Integer; virtual; abstract;
    procedure DoReadBytes(var aBuffer: TIdBytes; aCount: Integer);
      virtual; abstract;
    { read data }

    { write data }
  protected
    procedure DoWriteBytes(aBuffer: TIdBytes); virtual; abstract;
    procedure DoWriteLn(const aText: String = ''); virtual; abstract;
  protected
    procedure DoWriteBufferOpen; virtual; abstract;
    procedure DoWriteBufferFlush; virtual; abstract;
    procedure DoWriteBufferClose; virtual; abstract;
    { write data }
  end;

  TsgcWSComponent_Base = class(TsgcTCPComponent_Base)
  private
    FQueueMessages: Boolean;
    FQueueMessage: TsgcWSObjectList;
    FQueueBinary: TsgcWSObjectList;
    FQueueFragmented: TsgcWSObjectList;
  private
    FOnConnect: TsgcWSConnectEvent;
    FOnDisconnect: TsgcWSDisconnectEvent;
    FOnMessage: TsgcWSMessageEvent;
    FOnBinary: TsgcWSBinaryEvent;
    FOnFragmented: TsgcWSFragmentedEvent;
    FOnError: TsgcWSErrorEvent;
    FOnException: TsgcExceptionEvent;
{$IFNDEF SGC_EVENT_DISPATCH}
  private
    FNotifyConnect: TsgcWSObjectList;
    FNotifyDisconnect: TsgcWSObjectList;
    FNotifyMessage: TsgcWSObjectList;
    FNotifyBinary: TsgcWSObjectList;
    FNotifyFragmented: TsgcWSObjectList;
    FNotifyError: TsgcWSObjectList;
    FNotifyException: TsgcWSObjectList;
  private
    FAsyncConnect: Boolean;
    FAsyncDisconnect: Boolean;
    FAsyncMessage: Boolean;
    FAsyncBinary: Boolean;
    FAsyncFragmented: Boolean;
    FAsyncError: Boolean;
    FAsyncException: Boolean;
    procedure DoAsyncConnect;
    procedure DoAsyncDisconnect;
    procedure DoAsyncMessage;
    procedure DoAsyncBinary;
    procedure DoAsyncFragmented;
    procedure DoAsyncError;
    procedure DoAsyncException;
  private
    function GetNotifyConnect: TsgcWSObjectList;
    function GetNotifyDisconnect: TsgcWSObjectList;
    function GetNotifyMessage: TsgcWSObjectList;
    function GetNotifyBinary: TsgcWSObjectList;
    function GetNotifyFragmented: TsgcWSObjectList;
    function GetNotifyError: TsgcWSObjectList;
    function GetNotifyException: TsgcWSObjectList;
  private
    property NotifyConnect: TsgcWSObjectList read GetNotifyConnect;
    property NotifyDisconnect: TsgcWSObjectList read GetNotifyDisconnect;
    property NotifyMessage: TsgcWSObjectList read GetNotifyMessage;
    property NotifyBinary: TsgcWSObjectList read GetNotifyBinary;
    property NotifyFragmented: TsgcWSObjectList read GetNotifyFragmented;
    property NotifyError: TsgcWSObjectList read GetNotifyError;
    property NotifyException: TsgcWSObjectList read GetNotifyException;
{$ENDIF}
  private
    function GetQueueMessage: TsgcWSObjectList;
    function GetQueueBinary: TsgcWSObjectList;
    function GetQueueFragmented: TsgcWSObjectList;
  protected
  protected
    property QueueMessage: TsgcWSObjectList read GetQueueMessage;
    property QueueBinary: TsgcWSObjectList read GetQueueBinary;
    property QueueFragmented: TsgcWSObjectList read GetQueueFragmented;
  protected
    procedure DoEventConnect(aConnection: TsgcWSConnection); virtual;
    procedure DoEventDisconnect(aConnection: TsgcWSConnection;
      Code: Integer); virtual;
    procedure DoEventMessage(aConnection: TsgcWSConnection;
      const Text: string); virtual;
    procedure DoEventBinary(const aConnection: TsgcWSConnection;
      Data: TMemoryStream); virtual;
    procedure DoEventFragmented(const aConnection: TsgcWSConnection;
      Data: TMemoryStream; OpCode: TOpCode; Fin: Boolean); virtual;
    procedure DoEventError(aConnection: TsgcWSConnection;
      const Error: string); virtual;
    procedure DoEventException(aConnection: TsgcWSConnection;
      const Error: String; aException: Exception); virtual;
  protected
    procedure DoNotifyConnect(aConnection: TsgcWSConnection); virtual;
    procedure DoNotifyDisconnect(aConnection: TsgcWSConnection); virtual;
    procedure DoNotifyMessage(aConnection: TsgcWSConnection); virtual;
    procedure DoNotifyBinary(aConnection: TsgcWSConnection); virtual;
    procedure DoNotifyFragmented(aConnection: TsgcWSConnection;
      const aStream: TMemoryStream; const aOpCode: TOpCode;
      const aContinuation: Boolean); virtual;
    procedure DoNotifyError(aConnection: TsgcWSConnection); virtual;
    procedure DoNotifyException(aConnection: TsgcWSConnection);
      overload; virtual;
    procedure DoNotifyException(const Error: STring; aException: Exception);
      overload; virtual;
  protected
    procedure DoException(aConnection: TsgcWSConnection; aMsgException: String;
      aException: Exception = nil); virtual;
  protected
    procedure DoError(aConnection: TsgcWSConnection; E: string);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  protected
    FQueueProcessing: Boolean;
    procedure SetQueueMessages(const Value: Boolean); virtual;
    procedure DoProcessQueue; virtual;
    procedure DoClearQueue; virtual;
  public
    procedure QueueClear;
    property QueueMessages: Boolean read FQueueMessages write SetQueueMessages;
  public
    property OnConnect: TsgcWSConnectEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TsgcWSDisconnectEvent read FOnDisconnect
      write FOnDisconnect;
    property OnMessage: TsgcWSMessageEvent read FOnMessage write FOnMessage;
    property OnBinary: TsgcWSBinaryEvent read FOnBinary write FOnBinary;
    property OnFragmented: TsgcWSFragmentedEvent read FOnFragmented
      write FOnFragmented;
    property OnError: TsgcWSErrorEvent read FOnError write FOnError;
    property OnException: TsgcExceptionEvent read FOnException
      write FOnException;
  end;

  TsgcWSLogFile = class(TsgcTCPLogFile)
  private
    FUnMaskFrames: Boolean;
  public
    constructor Create; override;
  public
    procedure Assign(aSource: TPersistent); override;
  public
    property UnMaskFrames: Boolean read FUnMaskFrames write FUnMaskFrames;
  end;

  TsgcWSComponent = class(TsgcWSComponent_Base)
    { from TsgcWSComponent_Base }
  protected
    procedure SetNotifyEvents(const Value: TwsNotifyEvent); override;
  public
    procedure EnterCS;
    procedure LeaveCS;
    function AssignedCS: Boolean;
    { from TsgcWSComponent_Base }

    { protocol }
  protected
    FProtocolObjectList:
{$IFDEF NEXTGEN}TsgcWSProtocols{$ELSE}TObjectList{$ENDIF};
    function ProtocolRegistered(aProtocolName: String;
      aProtocol: TsgcWSProtocol): Boolean; virtual;
    // don't set aProtocolName as const, increment locks when free
    function GetProtocols: String;
    procedure DoRegisterProtocol(aObject: TsgcWSProtocol); overload; virtual;
    procedure DoRegisterProtocol(aProtocol: String); overload; virtual;
    procedure DoUnRegisterProtocol(aObject: TsgcWSProtocol); overload; virtual;
    procedure DoUnRegisterProtocol(aProtocol: String); overload; virtual;
    { protocol }

    { api }
  protected
    FAPI: TsgcWSAPI;
    procedure RegisterAPI(aObject: TsgcWSAPI); virtual;
    procedure UnRegisterAPI(aObject: TsgcWSAPI); virtual;
    { api }

    { specifications }
  private
    FSpecifications: TsgcWSSpecifications;
  protected
    function GetSpecifications: TsgcWSSpecifications; virtual;
    procedure SetSpecifications(const Value: TsgcWSSpecifications); virtual;
  public
    property Specifications: TsgcWSSpecifications read GetSpecifications
      write SetSpecifications;
    { specifications }

    { LogFile }
  private
    FLogFile: TsgcWSLogFile;
  protected
    function GetLogFile: TsgcWSLogFile; virtual;
    procedure SetLogFile(const Value: TsgcWSLogFile); virtual;
  public
    property LogFile: TsgcWSLogFile read GetLogFile write SetLogFile;
    { LogFile }

    { heartbeat }
  private
    FHeartBeat: TsgcWSHeartBeat_Options;
  protected
    FHeartBeatTimeoutTimer: TsgcTimer;
    FHeartBeatTimer: TsgcTimer;
  protected
    procedure OnHeartBeatTimeoutEvent(Sender: TObject); virtual; abstract;
    procedure OnHeartBeatTimeoutExceptionEvent(Sender: TObject; E: Exception);
      virtual; abstract;
    procedure DoStartHeartBeatTimeout;
    procedure DoStopHeartBeatTimeout;
  protected
    procedure OnHeartBeatEvent(Sender: TObject); virtual; abstract;
    procedure OnHeartBeatExceptionEvent(Sender: TObject; E: Exception);
      virtual; abstract;
    procedure DoStartHeartBeat; virtual;
    procedure DoStopHeartBeat; virtual;
  protected
    function GetHeartBeat: TsgcWSHeartBeat_Options; virtual;
    procedure SetHeartBeat(const Value: TsgcWSHeartBeat_Options); virtual;
  public
    property HeartBeat: TsgcWSHeartBeat_Options read GetHeartBeat
      write SetHeartBeat;
    { heartbeat }

    { keepalive }
  private
    FTCPKeepAlive: TsgcTCPKeepAlive;
    procedure SetTCPKeepAlive(const Value: TsgcTCPKeepAlive);
  public
    property TCPKeepAlive: TsgcTCPKeepAlive read FTCPKeepAlive
      write SetTCPKeepAlive;
    { keepalive }

    { WatchDog }
  protected
    FWatchDogAttempts: Integer;
  protected
    FWatchDogTimer: TsgcTimer;
    FWatchDogEnabled: Boolean;
  protected
    procedure DoStartWatchDog; virtual;
    procedure DoStopWatchDog; virtual;
    function GetWatchDog: TsgcWSWatchDog_Options; virtual; abstract;
  protected
    procedure OnWatchDogEvent(Sender: TObject); virtual;
    procedure OnWatchDogExceptionEvent(Sender: TObject; E: Exception);
      virtual; abstract;
    { WatchDog }

    { queue data }
  private
    procedure DoWriteQueueData(const aConnection: TsgcWSConnection;
      const aQueue: TsgcQueueList);
  protected
    procedure DoWriteQueueMsgLevels(const aConnection
      : TsgcWSConnection); virtual;
    { queue data }

    { extensions }
  private
    FExtensions: TsgcWSExtensions;
  protected
    function GetExtensions: TsgcWSExtensions; virtual;
    procedure SetExtensions(const Value: TsgcWSExtensions); virtual;
  public
    property Extensions: TsgcWSExtensions read GetExtensions
      write SetExtensions;
    { extensions }

    { notify }
  protected
    procedure DoNotifyConnect(aConnection: TsgcWSConnection); override;
    procedure DoNotifyDisconnect(aConnection: TsgcWSConnection); override;
    procedure DoNotifyError(aConnection: TsgcWSConnection); override;
    procedure DoNotifyException(aConnection: TsgcWSConnection); override;
    procedure DoNotifyMessage(aConnection: TsgcWSConnection); override;
    procedure DoNotifyBinary(aConnection: TsgcWSConnection); override;
    procedure DoNotifyFragmented(aConnection: TsgcWSConnection;
      const aStream: TMemoryStream; const aOpCode: TOpCode;
      const aContinuation: Boolean); override;
    { notify }

    { handshake }
  private
    FSyncHandshake: {$IFDEF NEXTGEN}TsgcWSHandshakes{$ELSE}TObjectList{$ENDIF};
    function GetSyncHandshake:
{$IFDEF NEXTGEN}TsgcWSHandshakes{$ELSE}TObjectList{$ENDIF};
  private
    procedure DoAsyncHandshake;
  private
    FOnHandshake: TsgcWSHandshakeEvent;
  protected
    procedure DoEventHandshake(aConnection: TsgcWSConnection;
      aHeaders: TStringList); virtual;
  protected
    procedure DoNotifyHandshake(aConnection: TsgcWSConnection); virtual;
  protected
    property SyncHandshake:
{$IFDEF NEXTGEN}TsgcWSHandshakes{$ELSE}TObjectList{$ENDIF} read GetSyncHandshake;
  public
    property OnHandshake: TsgcWSHandshakeEvent read FOnHandshake
      write FOnHandshake;
    { handshake }

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TsgcWSComponent_Server = Class(TsgcWSComponent)
    { from TsgcWSComponent_Base }
  protected
    procedure DoDestroyConnection(aConnection: TsgcSocketConnection); override;
  public
    procedure SetNotifyEvents(const Value: TwsNotifyEvent); override;
    { from TsgcWSComponent_Base }

    { from TsgcWSComponent }
  protected
    procedure DoNotifyConnect(aConnection: TsgcWSConnection); override;
    procedure DoNotifyDisconnect(aConnection: TsgcWSConnection); override;
  public
    procedure RegisterProtocol(aObject: TsgcWSProtocol); overload;
    procedure RegisterProtocol(aProtocol: String); overload;
    procedure UnRegisterProtocol(aObject: TsgcWSProtocol); overload;
    procedure UnRegisterProtocol(aProtocol: String); overload;
    { from TsgcWSComponent }

    { broadcast }
  public
    procedure Broadcast(const aMessage: string; const aChannel: string = '';
      const aProtocol: string = ''; const Exclude: String = '';
      const Include: String = ''); overload; virtual; abstract;
    procedure Broadcast(aStream: TStream; const aChannel: string = '';
      const aProtocol: string = ''; const Exclude: String = '';
      const Include: String = ''; const aSize: Integer = 0;
      const aStreaming: TwsStreaming = stmNone); overload; virtual; abstract;
    { broadcast }

    { WriteData }
  public
    function WriteData(const aGuid, aMessage: string): Boolean; overload;
      virtual; abstract;
    function WriteData(const aGuid: string; aStream: TStream;
      aSize: Integer = 0; const aStreaming: TwsStreaming = stmNone): Boolean;
      overload; virtual; abstract;
    { WriteData }

    { http }
  protected
    function DoHTTPRequestAPI(const aMethod, aDocument, aQueryParams, aContent:
        string; var Response: TsgcHTTPResponse): Boolean; virtual;
    { http }

    { Optimizations }
    // ... connections
  private
    FConnections: TsgcCacheQueueListConnections;
  protected
    function GetConnections: TsgcCacheQueueListConnections;
    // ... channels
  private
    FChannels: TsgcQueueListChannels;
  protected
    function GetChannels: TsgcQueueListChannels;
    // ... connections free
  private
    FConnectionsFree: TsgcQueueCommon;
  protected
    FConnectionsFreeTimer: TsgcTimer;
    function GetConnectionsFree: TsgcQueueCommon;
  protected
    procedure DoStartConnectionFree; virtual;
    procedure DoStopConnectionFree; virtual;
  protected
    procedure OnConnectionsFreeEvent(Sender: TObject); virtual;
    procedure OnConnectionsFreeExceptionEvent(Sender: TObject;
      E: Exception); virtual;
    // ... optimizations
  private
    FOptimizations: TsgcWSOptimization_Options;
    procedure SetOptimizations(const Value: TsgcWSOptimization_Options);
  public
    property Optimizations: TsgcWSOptimization_Options read FOptimizations
      write SetOptimizations;
    { Optimizations }

    { watchdog }
  protected
    FWatchDog: TsgcWSWatchDogServer_Options;
    procedure SetWatchDog(const Value: TsgcWSWatchDogServer_Options); virtual;
    function GetWatchDog: TsgcWSWatchDog_Options; override;
  public
    property WatchDog: TsgcWSWatchDogServer_Options read FWatchDog
      write SetWatchDog;
    { watchdog }

    { constructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor }

    { locklist }
  public
    function LockList:
{$IFDEF NEXTGEN}TList <
{$IFDEF SGC_INDY}sgcIdContext{$ELSE}IdContext{$ENDIF}.TIdContext >
{$ELSE}TList{$ENDIF}; virtual; abstract;
    procedure UnLockList; virtual; abstract;
    { locklist }

    { events }
  private
    FOnUnknownProtocol: TsgcWSUnknownProtocolEvent;
  protected
    function DoUnknownProtocol(aConnnection: TsgcWSConnection)
      : Boolean; virtual;
  public
    property OnUnknownProtocol: TsgcWSUnknownProtocolEvent
      read FOnUnknownProtocol write FOnUnknownProtocol;
    { events }
  End;

  TsgcWSComponent_Client = Class(TsgcWSComponent)
    { from TsgcWSComponent_Base }
  private
    FFreeConnectionOnDestroy: Boolean;
    procedure DoDestroyDelayedConnection;
  protected
    FDelayedFreeConnection: TsgcWSConnection;
    procedure DoDestroyConnection(aConnection: TsgcSocketConnection); override;
    procedure SetNotifyEvents(const Value: TwsNotifyEvent); override;
  protected
    procedure DoEventConnect(aConnection: TsgcWSConnection); override;
  public
    property FreeConnectionOnDestroy: Boolean read FFreeConnectionOnDestroy
      write FFreeConnectionOnDestroy;
    { from TsgcWSComponent_Base }

    { from TsgcWSComponent }
  public
    procedure RegisterProtocol(aObject: TsgcWSProtocol); overload;
    procedure RegisterProtocol(aProtocol: String); overload;
    procedure UnRegisterProtocol(aObject: TsgcWSProtocol); overload;
    procedure UnRegisterProtocol(aProtocol: String); overload;
  public
    procedure RegisterAPI(aObject: TsgcWSAPI); override;
    procedure UnRegisterAPI(aObject: TsgcWSAPI); override;
    { from TsgcWSComponent }

    { watchdog }
  protected
    FWatchDog: TsgcWSWatchDogClient_Options;
    procedure SetWatchDog(const Value: TsgcWSWatchDogClient_Options); virtual;
    function GetWatchDog: TsgcWSWatchDog_Options; override;
  public
    property WatchDog: TsgcWSWatchDogClient_Options read FWatchDog
      write SetWatchDog;
    { watchdog }

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    { WriteData }
  public
    procedure WriteData(const aText: String; const aSize: Integer = 0;
      const aStreaming: TwsStreaming = stmNone); overload; virtual; abstract;
    procedure WriteData(const aStream: TStream; const aSize: Integer = 0;
      const aStreaming: TwsStreaming = stmNone); overload; virtual; abstract;
  public
    function WriteAndWaitData(const aText: String;
      const aTimeOut: Integer = 10000): string; virtual; abstract;
    { WriteData }

    { url }
  protected
    procedure SetURL(const Value: String); virtual; abstract;
  public
    property URL: String write SetURL;
    { url }
  End;

  TsgcWSComponent_WSClient = class(TsgcWSComponent_Client)
  private
    FHost: String;
    FPort: Integer;
    FTLS: Boolean;
    FTLSOptions: TsgcWSTLS_Options;
  protected
    function GetActive: Boolean; virtual; abstract;
    procedure SetActive(const Value: Boolean); virtual;
    procedure SetTLSOptions(const Value: TsgcWSTLS_Options); virtual;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  public
    property Active: Boolean read GetActive write SetActive default False;
  public
    property Host: String read FHost write FHost;
    property Port: Integer read FPort write FPort;
    property TLS: Boolean read FTLS write FTLS;
    property TLSOptions: TsgcWSTLS_Options read FTLSOptions write SetTLSOptions;
  end;

  TsgcWSComponent_WSClient_API = class(TsgcWSComponent_WSClient)
  public
    procedure DoStartHeartBeat; override;
    procedure DoStopHeartBeat; override;
  end;

  TsgcWSAPI = class(TsgcWSComponent_Base)
  private
    FRawMessages: Boolean;
  protected
    function IsInternalMessage(const aMessage: String): Boolean; virtual;
  public
    constructor Create(aOwner: TComponent); override;
  public
    property RawMessages: Boolean read FRawMessages write FRawMessages
      stored False;
  end;

  TsgcWSAPI_client = class(TsgcWSAPI)
    { from TComponent }
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Loaded; override;
    { from TComponent }

    { client component }
  protected
    FClient: TsgcWSComponent_WSClient;
    procedure SetClient(const Value: TsgcWSComponent_WSClient); virtual;
  public
    property Client: TsgcWSComponent_WSClient read FClient write SetClient;
    { client component }

    { event }
  protected
    procedure DoNotifyConnect(aConnection: TsgcWSConnection); override;
    procedure DoNotifyDisconnect(aConnection: TsgcWSConnection); override;
  protected
    procedure DoBeforeConnect; virtual;
    { event }

    { URL }
  protected
    function GetURL: String; virtual;
  public
    property URL: String read GetURL;
    { URL }

    { keep alive }
  protected
    function DoKeepAlive: Boolean; virtual;
    { keep alive }
  end;

  TsgcWSAPI_client_KeepAlive = class(TsgcWSAPI_client)
  public
    function DoKeepAlive: Boolean; override;
  end;

  TsgcWSAPI_server = class(TsgcWSAPI)
    { from TComponent }
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Loaded; override;
    { from TComponent }

    { Server component }
  protected
    FServer: TsgcWSComponent_Server;
    procedure SetServer(const Value: TsgcWSComponent_Server); virtual;
  public
    property Server: TsgcWSComponent_Server read FServer write SetServer;
    { Server component }

    { resources }
  private
    FResources: TStringList;
    function GetResources: TStringList;
  protected
    property Resources: TStringList read GetResources write FResources;
  protected
    procedure ClearResources; virtual;
    procedure AddResource(const aDocument, aResource: string);
    function GetResource(const aDocument: string): string;
    { resources }

    { http }
  protected
    function DoHTTPRequest(const aRequest: TsgcHTTPRequest; const aResponse:
        TsgcHTTPResponse): Boolean; virtual;
    function DoHTTPRequestApi(const aRequest: TsgcHTTPRequest; const aResponse:
        TsgcHTTPResponse): Boolean; virtual; abstract;
    procedure DoHTTPBeforeResourceResponse(const aRequest: TsgcHTTPRequest; const
        aResponse: TsgcHTTPResponse); virtual; abstract;
    { http }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }
  end;

  TsgcQueueItemProtocolMessage = class(TsgcQueueItemBase)
  private
    FWSMessage: TComponent;
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    property WSMessage: TComponent read FWSMessage write FWSMessage;
  end;

  TsgcWSProtocol = class(TsgcWSComponent_Base)
    { from TsgcWSComponent_Base }
  public
    procedure EnterCS;
    procedure LeaveCS;
    { from TsgcWSComponent_Base }

    { properties }
  protected
    FGuid: String;
    FProtocol: string;
    FMsgType: TwsProtocolMessage;
  public
    property Protocol: string read FProtocol;
    property Guid: String read FGuid write FGuid;
    property MsgType: TwsProtocolMessage read FMsgType write FMsgType;
    { properties }

    { initialize }
  protected
    procedure DoInitialize(aConnection: TsgcWSConnection); virtual;
    procedure DoFinalize(aConnection: TsgcWSConnection); virtual;
    procedure DoClear(aConnection: TsgcWSConnection); virtual;
    { initialize }

    { protocol data }
  protected
    function GetProtocolDataItem(aConnection: TsgcWSConnection;
      const aID: String): TsgcQueueItemProtocolMessage;
    procedure AddProtocolDataItem(const aConnection: TsgcWSConnection;
      const aID: string; const aItem: TsgcQueueItemProtocolMessage);
    { protocol data }

    { events }
  private
    FOnRawMessage: TsgcWSRawMessageEvent;
  protected
    function DoRawMessage(aConnection: TsgcWSConnection; const Text: String)
      : Boolean; virtual;
  protected
    property OnRawMessage: TsgcWSRawMessageEvent read FOnRawMessage
      write FOnRawMessage;
    { events }
  end;

  TsgcWSProtocol_Client = Class(TsgcWSProtocol)
    { from TsgcWSComponent_Base }
  protected
    procedure DoDestroyConnection(aConnection: TsgcSocketConnection); override;
    { from TsgcWSComponent_Base }

    { from TsgcWSComponent }
  protected
    procedure DoNotifyBinary(aConnection: TsgcWSConnection); override;
    { from TsgcWSComponent }

    { from TsgcWSComponent_Base }
  protected
    procedure DoEventConnect(aConnection: TsgcWSConnection); override;
    procedure DoEventDisconnect(aConnection: TsgcWSConnection;
      Code: Integer); override;
    { from TsgcWSComponent_Base }

    { from TComponent }
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure Loaded; override;
    { from TComponent }

    { client component }
  private
    FClient: TsgcWSComponent_WSClient;
  protected
    procedure SetClient(const Value: TsgcWSComponent_WSClient); virtual;
  public
    property Client: TsgcWSComponent_WSClient read FClient write SetClient;
    { client component }

    { custom handles }
  protected
    function DoHandleEventConnect: Boolean; virtual;
    { custom handles }

    { methods }
  protected
    function DoKeepAlive: Boolean; virtual;
    { methods }

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  End;

  TsgcWSProtocol_Server = Class(TsgcWSProtocol)
    { from TsgcWSComponent_Base }
  protected
    procedure DoDestroyConnection(aConnection: TsgcSocketConnection); override;
    { from TsgcWSComponent_Base }

    { from TsgcWSComponent }
  protected
    procedure DoNotifyBinary(aConnection: TsgcWSConnection); override;
    { from TsgcWSComponent }

    { from TComponent }
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    { from TComponent }

    { http request }
  protected
    function GetHTTPResponse(const aConnection: TsgcWSConnection;
      const aPath, aBody: String): String; virtual;
    { http request }

    { server component }
  private
    FServer: TsgcWSComponent_Server;
  protected
    procedure SetServer(const Value: TsgcWSComponent_Server); virtual;
  public
    property Server: TsgcWSComponent_Server read FServer write SetServer;
    { server component }

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  End;

  // TsgcWSAuthentication_Session //

  TsgcWSAuthentication_Session = class(TPersistent)
  private
    FEnabled: Boolean;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

  // TsgcWSAuthentication_URL //

  TsgcWSAuthentication_URL = class(TPersistent)
  private
    FEnabled: Boolean;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
  end;


  // TsgcWSAuthentication_Basic //

  TsgcWSAuthentication_Basic = class(TPersistent)
  private
    FEnabled: Boolean;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

  // TsgcWSAuthentication_Token //

  TsgcWSAuthentication_Token = class(TPersistent)
  private
    FAuthName: String;
    FAuthToken: String;
    FEnabled: Boolean;
  public
    constructor Create; virtual;
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property AuthName: String read FAuthName write FAuthName;
    property AuthToken: String read FAuthToken write FAuthToken;
  end;


  // TsgcWSAuthentication_Options //

  TsgcWSAuthentication_Options = class(TPersistent)
  private
    FEnabled: Boolean;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
  end;



{$IFDEF SGC_DEBUG}

procedure DoLog(const aObject: TObject; const aConnection: TsgcWSConnection;
  const aMethod: String = ''; const aParams: String = '');
{$ENDIF}

implementation

uses
  sgcWebSocket_Resources,
{$IFDEF SGC_INDY}sgcIdException{$ELSE}IdException{$ENDIF};

{$IFDEF SGC_DEBUG}

procedure DoLog(const aObject: TObject; const aConnection: TsgcWSConnection;
  const aMethod: String = ''; const aParams: String = '');
begin
  sgcTCP_Classes.DoLog(aObject, aConnection, aMethod, aParams);
end;
{$ENDIF}

const
  CS_SUBSCRIPTIONS = 0;

  { TsgcWSConnection }

constructor TsgcWSConnection.Create;
begin
  inherited;
  FApiData := nil;
  FExtensions := TsgcWSExtensions.Create;
  FQueueOptions := TsgcWSQueueOptions.Create;
  FEnabled := False;
  ValidateUTF8 := False;
  RaiseDisconnectExceptions := True;
  FTransport := trpUndefined;
  FragmentedMessages := frgOnlyBuffer;
  CleanDisconnect := True;
  TCPEndOfFrameScanBuffer := eofScanNone;
  FNotifyConnection := True;
  FNotifyDisconnection := True;
end;

destructor TsgcWSConnection.Destroy;
begin
  FApiData := nil; // don't destroy
  sgcFree(FExtensions);
  sgcFree(FQueueOptions);
  sgcFree(FQueueLevel2);
  sgcFree(FQueueLevel1);
  sgcFree(FQueueLevel0);
  sgcFree(FExtensions);
  sgcFree(FSubscriptions);
  sgcFree(FHeadersResponse);
  sgcFree(FProtocolData);
  inherited;
end;

procedure TsgcWSConnection.Close;
begin
  Disconnected := True;
end;

procedure TsgcWSConnection.Close(const aCloseCode: Integer);
begin
  Disconnected := True;
end;

procedure TsgcWSConnection.Disconnect(const aCloseCode: Integer);
begin
  Disconnected := True;
end;

procedure TsgcWSConnection.Disconnect;
begin
  Disconnected := True;
end;

procedure TsgcWSConnection.DoBinaryEvent(const aData: TMemoryStream);
begin
  if Assigned(FOnBinary) then
    FOnBinary(Self, aData);
end;

procedure TsgcWSConnection.DoClose(aCloseCode: Integer = CS_CLOSE_NORMAL);
begin
  Disconnected := True;
end;

procedure TsgcWSConnection.DoFragmentedEvent(const aData: TMemoryStream;
  const aOpCode: TOpCode; const aContinuation: Boolean);
begin
  if Assigned(FOnFragmented) then
    FOnFragmented(Self, aData, aOpCode, aContinuation);
end;

procedure TsgcWSConnection.DoMessageEvent(const aText: String);
begin
  if Assigned(FOnMessage) then
    FOnMessage(Self, aText);
end;

procedure TsgcWSConnection.DoPing(const aText: String);
begin
  if FFirstPing = 0 then
    FFirstPing := Now;
  FLastPing := Now;
end;

procedure TsgcWSConnection.DoQueueData(const aText: string;
  const aSize: Integer = 0; const aStreaming: TwsStreaming = stmNone);
var
  oQueue: TsgcQueueObject;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
  oThreadList: TsgcQueueList;
begin
  // ... get thread list
  case QueueOptions.Text.Level of
    qmLevel1:
      oThreadList := QueueLevel1;
    qmLevel2:
      oThreadList := QueueLevel2;
  else
    oThreadList := QueueLevel0;
  end;

  // ... add to queue
  oList := oThreadList.LockList;
  Try
    oQueue := TsgcQueueObject.Create;
    oQueue.OpCode := opText;
    oQueue.Text := aText;
    oQueue.Size := aSize;
    oQueue.Streaming := aStreaming;

    oList.Add(oQueue);
  Finally
    oThreadList.UnLockList;
  End;
end;

procedure TsgcWSConnection.DoQueueData(const aStream: TStream;
  const aSize: Integer = 0; const aStreaming: TwsStreaming = stmNone);
var
  oQueue: TsgcQueueObject;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
  oThreadList: TsgcQueueList;
begin
  // ... get thread list
  case QueueOptions.Binary.Level of
    qmLevel1:
      oThreadList := QueueLevel1;
    qmLevel2:
      oThreadList := QueueLevel2;
  else
    oThreadList := QueueLevel0;
  end;

  // ... add to queue
  oList := oThreadList.LockList;
  Try
    oQueue := TsgcQueueObject.Create;
    oQueue.OpCode := opBinary;
    oQueue.Stream := aStream;
    oQueue.Size := aSize;
    oQueue.Streaming := aStreaming;

    oList.Add(oQueue);
  Finally
    oThreadList.UnLockList;
  End;
end;

procedure TsgcWSConnection.DoQueuePing(const aText: string);
var
  oQueue: TsgcQueueObject;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
  oThreadList: TsgcQueueList;
begin
  // ... get thread list
  case QueueOptions.Ping.Level of
    qmLevel1:
      oThreadList := QueueLevel1;
    qmLevel2:
      oThreadList := QueueLevel2;
  else
    oThreadList := QueueLevel0;
  end;

  // ... add to queue
  oList := oThreadList.LockList;
  Try
    oQueue := TsgcQueueObject.Create;
    oQueue.OpCode := opPing;
    oQueue.Text := aText;
    oQueue.Size := 0;
    oQueue.Streaming := stmNone;

    oList.Add(oQueue);
  Finally
    oThreadList.UnLockList;
  End;
end;

procedure TsgcWSConnection.DoSubscribe(const aChannels: String);
var
  oList: TsgcDelimitedStringList;
  i: Integer;
  vChannel: String;
begin
  oList := TsgcDelimitedStringList.Create;
  Try
    oList.DelimitedText := aChannels;
    for i := 0 to oList.Count - 1 do
    begin
      vChannel := oList[i];
      if Subscriptions.IndexOf(vChannel) = -1 then
      begin
        DoEnterCS(CS_SUBSCRIPTIONS);
        Try
          Subscriptions.Add(vChannel);
        Finally
          DoLeaveCS(CS_SUBSCRIPTIONS);
        End;
        if Assigned(FOnSubscription) then
          FOnSubscription(Self, vChannel);
      end;
    end;
  Finally
    sgcFree(oList);
  End;
end;

procedure TsgcWSConnection.DoUnSubscribe(const aChannels: String);
var
  oList: TsgcDelimitedStringList;
  i, j: Integer;
  vChannel: String;
begin
  oList := TsgcDelimitedStringList.Create;
  Try
    oList.DelimitedText := aChannels;
    for i := 0 to oList.Count - 1 do
    begin
      vChannel := oList[i];
      j := Subscriptions.IndexOf(vChannel);
      if j <> -1 then
      begin
        DoEnterCS(CS_SUBSCRIPTIONS);
        Try
          Subscriptions.Delete(j);
        Finally
          DoLeaveCS(CS_SUBSCRIPTIONS);
        End;
        if Assigned(FOnUnSubscription) then
          FOnUnSubscription(Self, vChannel);
      end;
    end;
  Finally
    sgcFree(oList);
  End;
end;

procedure TsgcWSConnection.WriteData(const aText: string;
  const aSize: Integer = 0; const aStreaming: TwsStreaming = stmNone);
begin
  if Assigned(QueueOptions) and (QueueOptions.Text.Level <> qmNone) then
    DoQueueData(aText, aSize, aStreaming)
  else
    DoWriteData(aText, aSize, aStreaming);
end;

function TsgcWSConnection.GetSubscriptions: TStringList;
begin
  if not Assigned(FSubscriptions) then
    FSubscriptions := TStringList.Create;
  Result := FSubscriptions;
end;

function TsgcWSConnection.GetHeadersResponse: TStringList;
begin
  if not Assigned(FHeadersResponse) then
    FHeadersResponse := TStringList.Create;
  Result := FHeadersResponse;
end;

function TsgcWSConnection.GetProtocolData: TsgcQueue;
begin
  if not Assigned(FProtocolData) then
  begin
    FProtocolData := TsgcQueue.Create;
    FProtocolData.OwnObjects := True;
  end;
  Result := FProtocolData;
end;

function TsgcWSConnection.GetQueueLevel0: TsgcQueueList;
begin
  if not Assigned(FQueueLevel0) then
    FQueueLevel0 := TsgcQueueList.Create;
  Result := FQueueLevel0;
end;

function TsgcWSConnection.GetQueueLevel1: TsgcQueueList;
begin
  if not Assigned(FQueueLevel1) then
    FQueueLevel1 := TsgcQueueList.Create;
  Result := FQueueLevel1;
end;

function TsgcWSConnection.GetQueueLevel2: TsgcQueueList;
begin
  if not Assigned(FQueueLevel2) then
    FQueueLevel2 := TsgcQueueList.Create;
  Result := FQueueLevel2;
end;

function TsgcWSConnection.IsControlFrame(aOpCode: TOpCode): Boolean;
begin
  Result := aOpCode in [opClose, opPing, opPong];
end;

function TsgcWSConnection.Ping(const aText: string; aTimeOut: Integer): Boolean;
var
  vStart: Cardinal;
begin
  Result := False;
  // ... send ping
  Ping(aText);
  // ... wait pong
  vStart := sgcGetTicks;
  if aTimeOut > 0 then
  begin
    repeat
      if aText = FPong then
      begin
        Result := True;
        break;
      end;
      sleep(1);
    until sgcGetTickDiff(vStart, sgcGetTicks) >= Cardinal(aTimeOut);
  end;
end;

procedure TsgcWSConnection.Ping(const aText: string);
begin
  if Assigned(QueueOptions) and (QueueOptions.Ping.Level <> qmNone) then
    DoQueuePing(aText)
  else
    DoPing(aText);
end;

procedure TsgcWSConnection.SetDisconnected(const Value: Boolean);
begin
  if not FDisconnected then
  begin
    // ... free objects in queue
    if Assigned(FQueueLevel0) then
    begin
      FQueueLevel0.FreeObjects;
      FQueueLevel0.Clear;
    end;
    if Assigned(FQueueLevel1) then
    begin
      FQueueLevel1.FreeObjects;
      FQueueLevel1.Clear;
    end;
    if Assigned(FQueueLevel2) then
    begin
      FQueueLevel2.FreeObjects;
      FQueueLevel2.Clear;
    end;
  end;
  // ... set value
  FDisconnected := Value;
end;

procedure TsgcWSConnection.SetExtensions(const Value: TsgcWSExtensions);
begin
  FExtensions.Assign(Value);
end;

procedure TsgcWSConnection.SetQueueOptions(const Value: TsgcWSQueueOptions);
begin
  FQueueOptions.Assign(Value);
end;

function TsgcWSConnection.Subscribed(const aChannel: String): Boolean;
var
  oList: TsgcDelimitedStringList;
  i: Integer;
begin
  Result := aChannel = '';
  if not Result then
  begin
    oList := TsgcDelimitedStringList.Create;
    Try
      oList.DelimitedText := aChannel;
      DoEnterCS(CS_SUBSCRIPTIONS);
      Try
        for i := 0 to oList.Count - 1 do
        begin
          if Subscriptions.IndexOf(oList[i]) <> -1 then
          begin
            Result := True;
            break;
          end;
        end;
      Finally
        DoLeaveCS(CS_SUBSCRIPTIONS);
      End;
    Finally
      sgcFree(oList);
    End;
  end;
end;

function TsgcWSConnection.WriteAndWaitData(const aText: String;
  const aTimeOut: Integer = 10000): string;
var
  vStart: Cardinal;
begin
  Result := '';
  MsgReceived := '';
  // ... send text
  WriteData(aText);
  // ... wait response
  vStart := sgcGetTicks;
  if aTimeOut > 0 then
  begin
    repeat
      if MsgReceived <> '' then
      begin
        Result := MsgReceived;
        break;
      end;
      sleep(1);
    until sgcGetTickDiff(vStart, sgcGetTicks) >= Cardinal(aTimeOut);
  end;
end;

procedure TsgcWSConnection.WriteData(const aStream: TStream;
  const aSize: Integer = 0; const aStreaming: TwsStreaming = stmNone);
var
  oStream: TStream;
begin
  // ... compress the stream before send fragmented
  if (aSize > 0) and (Extensions.PerMessage_Deflate.Active or Extensions.DeflateFrame.Active) then
  begin
    aStream.Position := 0;
    Try
      DoEnterCS;
      Try
        oStream := aStream;
        Extensions.WriteStream(oStream);
        oStream.Position := 0;
      Finally
        DoLeaveCS;
      End;
    Except
      On E: Exception do
        raise TsgcWSException.CreateFmt(S_ERROR_INFLATING_FRAME, [E.message]);
    End;
  end;

  // ... write data
  if Assigned(QueueOptions) and (QueueOptions.Binary.Level <> qmNone) then
    DoQueueData(aStream, aSize, aStreaming)
  else
    DoWriteData(aStream, aSize, aStreaming);
end;

constructor TsgcWSComponent_Base.Create(aOwner: TComponent);
begin
  inherited;
{$IFDEF CONSOLE}
  FNotifyEvents := neNoSync;
{$ELSE}
{$IFDEF MSWINDOWS}
  if IsLibrary then
    FNotifyEvents := neNoSync
  else
    FNotifyEvents := neAsynchronous;
{$ELSE}
  FNotifyEvents := neAsynchronous;
{$ENDIF}
{$ENDIF}
  FQueueMessages := False;
end;

destructor TsgcWSComponent_Base.Destroy;
begin
{$IFNDEF SGC_EVENT_DISPATCH}
  sgcFree(FNotifyConnect);
  if Assigned(FNotifyDisconnect) then
    FNotifyDisconnect.DeleteAll(True);
  sgcFree(FNotifyDisconnect);
  sgcFree(FNotifyMessage);
  sgcFree(FNotifyError);
  sgcFree(FNotifyException);
  sgcFree(FNotifyBinary);
{$ENDIF}
  sgcFree(FQueueMessage);
  sgcFree(FQueueBinary);
  inherited;
end;

{$IFNDEF SGC_EVENT_DISPATCH}

procedure TsgcWSComponent_Base.DoAsyncConnect;
var
  oList: TList{$IFDEF NEXTGEN}<TsgcWSNotifyObject>{$ENDIF};
  oObject: TObject;
begin
  if IsDestroying then
    exit;
  if not Assigned(FNotifyConnect) then
    exit;

  oList := FNotifyConnect.LockList;
  Try
    if FAsyncConnect then
      exit;
    FAsyncConnect := True;
    Try
      while oList.Count > 0 do
      begin
        DoEventConnect(TsgcWSNotifyObject(oList.Items[0]).Connection);
        // ... avoid exception if object has been destroyed inside event
        if not Assigned(FNotifyConnect) then
          exit;
        if oList.Count > 0 then
        begin
          oObject := TObject(oList.Items[0]);
          sgcFree(oObject);
        end;
        if oList.Count > 0 then
          oList.Delete(0);
      end;
    Finally
      FAsyncConnect := False;
    End;
  Finally
    if Assigned(FNotifyConnect) then
      FNotifyConnect.UnLockList;
  End;
end;

procedure TsgcWSComponent_Base.DoAsyncDisconnect;
var
  oList: TList{$IFDEF NEXTGEN}<TsgcWSNotifyObject>{$ENDIF};
  oObject: TObject;
begin
  if IsDestroying then
    exit;
  if not Assigned(FNotifyDisconnect) then
    exit;

  oList := FNotifyDisconnect.LockList;
  Try
    if FAsyncDisconnect then
      exit;
    FAsyncDisconnect := True;
    Try
      while oList.Count > 0 do
      begin
        Try
          DoEventDisconnect(TsgcWSNotifyObject(oList.Items[0]).Connection,
            TsgcWSNotifyObject(oList.Items[0]).Int);
          // ... avoid exception if object has been destroyed inside event
          if not Assigned(FNotifyDisconnect) then
            exit;
          if Assigned(TsgcWSNotifyObject(oList.Items[0]).Connection) then
          begin
            if not TsgcWSNotifyObject(oList.Items[0]).Connection.FFreed then
            begin
              Try
                DoDestroyConnection(TsgcWSNotifyObject(oList.Items[0])
                  .Connection);
              Except
                // todo: handle exception
              End;
            end;
          end;
        Finally
          if Assigned(FNotifyDisconnect) then
          begin
            if oList.Count > 0 then
            begin
              oObject := TObject(oList.Items[0]);
              sgcFree(oObject);
            end;
            if oList.Count > 0 then
              oList.Delete(0);
          end;
        end;
      end;
    Finally
      FAsyncDisconnect := False;
    End;
  Finally
    if Assigned(FNotifyDisconnect) then
      FNotifyDisconnect.UnLockList;
  End;
end;

procedure TsgcWSComponent_Base.DoAsyncMessage;
var
  oList: TList{$IFDEF NEXTGEN}<TsgcWSNotifyObject>{$ENDIF};
  oObject: TObject;
begin
  if IsDestroying then
    exit;
  if not Assigned(FNotifyMessage) then
    exit;

  oList := FNotifyMessage.LockList;
  Try
    if FAsyncMessage then
      exit;
    FAsyncMessage := True;
    Try
      while oList.Count > 0 do
      begin
        DoEventMessage(TsgcWSNotifyObject(oList.Items[0]).Connection,
          TsgcWSNotifyObject(oList.Items[0]).Text);
        // ... avoid exception if object has been destroyed inside event
        if not Assigned(FNotifyMessage) then
          exit;
        if oList.Count > 0 then
        begin
          oObject := TObject(oList.Items[0]);
          sgcFree(oObject);
        end;
        if oList.Count > 0 then
          oList.Delete(0);
      end;
    Finally
      FAsyncMessage := False;
    End;
  Finally
    if Assigned(FNotifyMessage) then
      FNotifyMessage.UnLockList;
  End;
end;

procedure TsgcWSComponent_Base.DoAsyncError;
var
  oList: TList{$IFDEF NEXTGEN}<TsgcWSNotifyObject>{$ENDIF};
  oObject: TObject;
begin
  if IsDestroying then
    exit;
  if not Assigned(FNotifyError) then
    exit;

  oList := FNotifyError.LockList;
  Try
    if FAsyncError then
      exit;
    FAsyncError := True;
    Try
      while oList.Count > 0 do
      begin
        DoEventError(TsgcWSNotifyObject(oList.Items[0]).Connection,
          TsgcWSNotifyObject(oList.Items[0]).Text);
        if not Assigned(FNotifyError) then
          exit;
        if oList.Count > 0 then
        begin
          oObject := TObject(oList.Items[0]);
          sgcFree(oObject);
        end;
        if oList.Count > 0 then
          oList.Delete(0);
      end;
    Finally
      FAsyncError := False;
    End;
  Finally
    if Assigned(FNotifyError) then
      FNotifyError.UnLockList;
  End;
end;

procedure TsgcWSComponent_Base.DoAsyncBinary;
var
  oList: TList{$IFDEF NEXTGEN}<TsgcWSNotifyObject>{$ENDIF};
  oObject: TObject;
begin
  if IsDestroying then
    exit;
  if not Assigned(FNotifyBinary) then
    exit;

  oList := FNotifyBinary.LockList;
  Try
    if FAsyncBinary then
      exit;
    FAsyncBinary := True;
    Try
      while oList.Count > 0 do
      begin
        DoEventBinary(TsgcWSNotifyObject(oList.Items[0]).Connection,
          TsgcWSNotifyObject(oList.Items[0]).Stream);
        // ... avoid exception if object has been destroyed inside event
        if not Assigned(FNotifyBinary) then
          exit;
        if oList.Count > 0 then
        begin
          oObject := TObject(oList.Items[0]);
          sgcFree(oObject);
        end;
        if oList.Count > 0 then
          oList.Delete(0);
      end;
    Finally
      FAsyncBinary := False;
    End;
  Finally
    if Assigned(FNotifyBinary) then
      FNotifyBinary.UnLockList;
  End;
end;

procedure TsgcWSComponent_Base.DoAsyncException;
var
  oList: TList{$IFDEF NEXTGEN}<TsgcWSNotifyObject>{$ENDIF};
  oObject: TObject;
begin
  if IsDestroying then
    exit;
  if not Assigned(FNotifyException) then
    exit;

  oList := FNotifyException.LockList;
  Try
    if FAsyncException then
      exit;
    FAsyncException := True;
    Try
      while oList.Count > 0 do
      begin
        DoEventException(TsgcWSNotifyObject(oList.Items[0]).Connection,
          TsgcWSNotifyObject(oList.Items[0]).Text,
          TsgcWSNotifyObject(oList.Items[0]).RawException);
        if not Assigned(FNotifyException) then
          exit;
        if oList.Count > 0 then
        begin
          oObject := TObject(oList.Items[0]);
          sgcFree(oObject);
        end;
        if oList.Count > 0 then
          oList.Delete(0);
      end;
    Finally
      FAsyncException := False;
    End;
  Finally
    if Assigned(FNotifyException) then
      FNotifyException.UnLockList;
  End;
end;

procedure TsgcWSComponent_Base.DoAsyncFragmented;
var
  oList: TList{$IFDEF NEXTGEN}<TsgcWSNotifyObject>{$ENDIF};
  oObject: TObject;
begin
  if IsDestroying then
    exit;
  if not Assigned(FNotifyFragmented) then
    exit;

  oList := FNotifyFragmented.LockList;
  Try
    if FAsyncFragmented then
      exit;
    FAsyncFragmented := True;
    Try
      while oList.Count > 0 do
      begin
        DoEventFragmented(TsgcWSNotifyObject(oList.Items[0]).Connection,
          TsgcWSNotifyObject(oList.Items[0]).Stream,
          TOpCode(TsgcWSNotifyObject(oList.Items[0]).Int),
          TsgcWSNotifyObject(oList.Items[0]).Text1 = '1');
        // ... avoid exception if object has been destroyed inside event
        if not Assigned(FNotifyFragmented) then
          exit;
        if oList.Count > 0 then
        begin
          oObject := TObject(oList.Items[0]);
          sgcFree(oObject);
        end;
        if oList.Count > 0 then
          oList.Delete(0);
      end;
    Finally
      FAsyncFragmented := False;
    End;
  Finally
    if Assigned(FNotifyFragmented) then
      FNotifyFragmented.UnLockList;
  End;
end;

function TsgcWSComponent_Base.GetNotifyConnect: TsgcWSObjectList;
begin
  if not Assigned(FNotifyConnect) then
    FNotifyConnect := TsgcWSObjectList.Create;
  Result := FNotifyConnect;
end;

function TsgcWSComponent_Base.GetNotifyDisconnect: TsgcWSObjectList;
begin
  if not Assigned(FNotifyDisconnect) then
    FNotifyDisconnect := TsgcWSObjectList.Create;
  Result := FNotifyDisconnect;
end;

function TsgcWSComponent_Base.GetNotifyError: TsgcWSObjectList;
begin
  if not Assigned(FNotifyError) then
    FNotifyError := TsgcWSObjectList.Create;
  Result := FNotifyError;
end;

function TsgcWSComponent_Base.GetNotifyMessage: TsgcWSObjectList;
begin
  if not Assigned(FNotifyMessage) then
    FNotifyMessage := TsgcWSObjectList.Create;
  Result := FNotifyMessage;
end;

function TsgcWSComponent_Base.GetNotifyBinary: TsgcWSObjectList;
begin
  if not Assigned(FNotifyBinary) then
    FNotifyBinary := TsgcWSObjectList.Create;
  Result := FNotifyBinary;
end;

function TsgcWSComponent_Base.GetNotifyException: TsgcWSObjectList;
begin
  if not Assigned(FNotifyException) then
    FNotifyException := TsgcWSObjectList.Create;
  Result := FNotifyException;
end;

function TsgcWSComponent_Base.GetNotifyFragmented: TsgcWSObjectList;
begin
  if not Assigned(FNotifyFragmented) then
    FNotifyFragmented := TsgcWSObjectList.Create;
  Result := FNotifyFragmented;
end;
{$ENDIF}

procedure TsgcWSComponent_Base.DoClearQueue;
begin
  if Assigned(FQueueMessage) then
    FQueueMessage.Clear;

  if Assigned(FQueueBinary) then
    FQueueBinary.Clear;
end;

procedure TsgcWSComponent_Base.DoError(aConnection: TsgcWSConnection;
  E: string);
begin
  if IsDestroying then
    exit;

  Try
    if Assigned(FOnError) then
    begin
      if Assigned(aConnection) then
      begin
        aConnection.MsgError := E;
        DoNotifyError(aConnection);
        // ... disconnect on error
        aConnection.DoDisconnectIfActive;
      end
      else
        DoEventError(nil, E);
    end
    else
    begin
      if Assigned(aConnection) then
      begin
        // ... disconnect on error
        aConnection.DoDisconnectIfActive;
      end;
    end;
  Except
    // nothing
  end;
end;

procedure TsgcWSComponent_Base.DoNotifyConnect(aConnection: TsgcWSConnection);
begin
  if IsDestroying then
    exit;

  if Assigned(aConnection) then
  begin
    if aConnection.IsInternal or (aConnection.NotifyConnection = False) then
      exit;

    case NotifyEvents of
      neNoSync:
        DoEventConnect(aConnection);
      neAsynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Queue(nil,
            procedure
            begin
              DoEventConnect(aConnection);
            end);
{$ELSE}
          NotifyConnect.AddNotifyObject(aConnection);
          NotifyMethod(DoAsyncConnect);
{$ENDIF}
        end;
      neSynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Synchronize(nil,
            procedure
            begin
              DoEventConnect(aConnection);
            end);
{$ELSE}
          NotifyConnect.AddNotifyObject(aConnection);
          SynchronizeMethod(DoAsyncConnect);
{$ENDIF}
        end;
    end;
  end;
end;

procedure TsgcWSComponent_Base.DoNotifyDisconnect
  (aConnection: TsgcWSConnection);
var
  vCode: Integer;
begin
  if IsDestroying then
    exit;

  if Assigned(aConnection) then
  begin
    if aConnection.IsInternal or (aConnection.NotifyDisconnection = False) then
    begin
      DoDestroyConnection(aConnection);
      exit;
    end;

    vCode := aConnection.CloseCode;
    case NotifyEvents of
      neNoSync:
        begin
          DoEventDisconnect(aConnection, vCode);
          DoDestroyConnection(aConnection);
        end;
      neAsynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Queue(nil,
            procedure
            begin
              DoEventDisconnect(aConnection, vCode);
              DoDestroyConnection(aConnection);
            end);
{$ELSE}
          NotifyDisconnect.AddNotifyObject(aConnection, vCode);
          NotifyMethod(DoAsyncDisconnect);
{$ENDIF}
        end;
      neSynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Synchronize(nil,
            procedure
            begin
              DoEventDisconnect(aConnection, vCode);
              DoDestroyConnection(aConnection);
            end);
{$ELSE}
          NotifyDisconnect.AddNotifyObject(aConnection, vCode);
          SynchronizeMethod(DoAsyncDisconnect);
{$ENDIF}
        end;
    end;
  end;
end;

procedure TsgcWSComponent_Base.DoNotifyError(aConnection: TsgcWSConnection);
var
  vError: string;
begin
  if IsDestroying then
    exit;

  if Assigned(aConnection) then
  begin
    if aConnection.IsInternal then
      exit;

    vError := aConnection.MsgError;
    case NotifyEvents of
      neNoSync:
        DoEventError(aConnection, vError);
      neAsynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Queue(nil,
            procedure
            begin
              DoEventError(aConnection, vError);
            end);
{$ELSE}
          NotifyError.AddNotifyObject(aConnection, vError);
          NotifyMethod(DoAsyncError);
{$ENDIF}
        end;
      neSynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Synchronize(nil,
            procedure
            begin
              DoEventError(aConnection, vError);
            end);
{$ELSE}
          NotifyError.AddNotifyObject(aConnection, vError);
          SynchronizeMethod(DoAsyncError);
{$ENDIF}
        end;
    end;
  end;
end;

procedure TsgcWSComponent_Base.DoNotifyMessage(aConnection: TsgcWSConnection);
var
  vMessage: String;
begin
  if IsDestroying then
    exit;

  if Assigned(aConnection) then
  begin
    if aConnection.IsInternal then
      exit;

    vMessage := aConnection.MsgReceived;
    case NotifyEvents of
      neNoSync:
        DoEventMessage(aConnection, vMessage);
      neAsynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Queue(nil,
            procedure
            begin
              DoEventMessage(aConnection, vMessage);
            end);
{$ELSE}
          NotifyMessage.AddNotifyObject(aConnection, vMessage);
          NotifyMethod(DoAsyncMessage);
{$ENDIF}
        end;
      neSynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Synchronize(nil,
            procedure
            begin
              DoEventMessage(aConnection, vMessage);
            end);
{$ELSE}
          NotifyMessage.AddNotifyObject(aConnection, vMessage);
          SynchronizeMethod(DoAsyncMessage);
{$ENDIF}
        end;
    end;
  end;
end;

procedure TsgcWSComponent_Base.DoNotifyBinary(aConnection: TsgcWSConnection);
{$IFDEF SGC_EVENT_DISPATCH}
var
  oStream: TMemoryStream;
{$ENDIF}
begin
  if IsDestroying then
    exit;

  if Assigned(aConnection) then
  begin
    if aConnection.IsInternal then
      exit;

    case NotifyEvents of
      neNoSync:
        DoEventBinary(aConnection, aConnection.MsgBinaryReceived);
      neAsynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          oStream := TMemoryStream.Create;
          oStream.LoadFromStream(aConnection.MsgBinaryReceived);
          TThread.Queue(nil,
            procedure
            begin
              DoEventBinary(aConnection, oStream);
              oStream.Free;
            end);
{$ELSE}
          NotifyBinary.AddNotifyObject(aConnection,
            aConnection.MsgBinaryReceived);
          NotifyMethod(DoAsyncBinary);
{$ENDIF}
        end;
      neSynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          oStream := TMemoryStream.Create;
          oStream.LoadFromStream(aConnection.MsgBinaryReceived);
          TThread.Synchronize(nil,
            procedure
            begin
              DoEventBinary(aConnection, oStream);
              oStream.Free;
            end);
{$ELSE}
          NotifyBinary.AddNotifyObject(aConnection,
            aConnection.MsgBinaryReceived);
          SynchronizeMethod(DoAsyncBinary);
{$ENDIF}
        end;
    end;
  end;
end;

procedure TsgcWSComponent_Base.DoEventBinary(const aConnection
  : TsgcWSConnection; Data: TMemoryStream);
begin
{$IFDEF SGC_DEBUG}
  DoLog(Self, aConnection, 'DoEventBinary');
{$ENDIF}
  if Assigned(FOnBinary) then
  begin
    if QueueMessages then
      QueueBinary.AddNotifyObject(aConnection, Data)
    else
      FOnBinary(aConnection, Data);
  end;
end;

procedure TsgcWSComponent_Base.DoEventConnect(aConnection: TsgcWSConnection);
begin
{$IFDEF SGC_DEBUG}
  DoLog(Self, aConnection, 'DoEventConnect');
{$ENDIF}
  if Assigned(FOnConnect) then
    FOnConnect(aConnection);
end;

procedure TsgcWSComponent_Base.DoEventDisconnect(aConnection: TsgcWSConnection;
Code: Integer);
begin
{$IFDEF SGC_DEBUG}
  DoLog(Self, aConnection, 'DoEventDisconnect');
{$ENDIF}
  if Assigned(FOnDisconnect) then
    FOnDisconnect(aConnection, Code);
end;

procedure TsgcWSComponent_Base.DoEventError(aConnection: TsgcWSConnection;
const Error: string);
begin
{$IFDEF SGC_DEBUG}
  DoLog(Self, aConnection, 'DoEventError', '[Error]: ' + Error);
{$ENDIF}
  if Assigned(FOnError) then
    FOnError(aConnection, Error);
end;

procedure TsgcWSComponent_Base.DoEventException(aConnection: TsgcWSConnection;
const Error: String; aException: Exception);
var
  vException: Exception;
begin
{$IFDEF SGC_DEBUG}
  DoLog(Self, aConnection, 'DoEventException', '[Error]: ' + Error);
{$ENDIF}
  if Assigned(FOnException) then
  begin
    if not Assigned(aException) then
    begin
      vException := Exception.Create(Error);
      Try
        FOnException(aConnection, vException);
      Finally
        sgcFree(vException);
      End;
    end
    else
    begin
      if aException.Message = '' then
        aException.Message := Error;
      FOnException(aConnection, aException);
    end;
  end;
end;

procedure TsgcWSComponent_Base.DoEventFragmented(const aConnection
  : TsgcWSConnection; Data: TMemoryStream; OpCode: TOpCode; Fin: Boolean);
begin
{$IFDEF SGC_DEBUG}
  DoLog(Self, aConnection, 'DoEventFragmented');
{$ENDIF}
  if Assigned(FOnFragmented) then
  begin
    if QueueMessages then
      QueueFragmented.AddNotifyObject(aConnection, Data, OpCode, Fin)
    else
      FOnFragmented(aConnection, Data, OpCode, Fin);
  end;
end;

procedure TsgcWSComponent_Base.DoEventMessage(aConnection: TsgcWSConnection;
const Text: string);
begin
{$IFDEF SGC_DEBUG}
  DoLog(Self, aConnection, 'DoEventMessage', '[Text]: ' + Text);
{$ENDIF}
  if Assigned(FOnMessage) then
  begin
    if QueueMessages then
      QueueMessage.AddNotifyObject(aConnection, Text)
    else
      FOnMessage(aConnection, Text);
  end;
end;

procedure TsgcWSComponent_Base.DoException(aConnection: TsgcWSConnection;
aMsgException: String; aException: Exception = nil);
begin
  if IsDestroying then
    exit;

  Try
    if Assigned(FOnException) then
    begin
      if Assigned(aConnection) then
      begin
        aConnection.MsgException := aMsgException;
        aConnection.RawException := aException;
        DoNotifyException(aConnection);
        // ... disconnect on error
        aConnection.DoDisconnectIfActive;
      end
      else
        DoNotifyException(aMsgException, aException);
    end
    else
    begin
      if Assigned(aConnection) then
      begin
        // ... disconnect on error
        aConnection.DoDisconnectIfActive;
      end;
    end;
  Except
    // nothing
  end;
end;

procedure TsgcWSComponent_Base.DoNotifyException(aConnection: TsgcWSConnection);
var
  vMessage: string;
  vException: Exception;
begin
  if IsDestroying then
    exit;

  if Assigned(aConnection) then
  begin
    if aConnection.IsInternal then
      exit;

    vMessage := aConnection.MsgException;
    vException := aConnection.RawException;
    case NotifyEvents of
      neNoSync:
        DoEventException(aConnection, vMessage, vException);
      neAsynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Queue(nil,
            procedure
            begin
              DoEventException(aConnection, vMessage, vException);
            end);
{$ELSE}
          NotifyException.AddNotifyObject(aConnection, vMessage, vException);
          NotifyMethod(DoAsyncException);
{$ENDIF}
        end;
      neSynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Synchronize(nil,
            procedure
            begin
              DoEventException(aConnection, vMessage, vException);
            end);
{$ELSE}
          NotifyException.AddNotifyObject(aConnection, vMessage, vException);
          SynchronizeMethod(DoAsyncException);
{$ENDIF}
        end;
    end;
  end;
end;

procedure TsgcWSComponent_Base.DoNotifyException(const Error: STring;
aException: Exception);
begin
  if IsDestroying then
    exit;

  case NotifyEvents of
    neNoSync:
      DoEventException(nil, Error, aException);
    neAsynchronous:
      begin
{$IFDEF SGC_EVENT_DISPATCH}
        TThread.Queue(nil,
          procedure
          begin
            DoEventException(nil, Error, vException);
          end);
{$ELSE}
        NotifyException.AddNotifyObject(nil, Error, aException);
        NotifyMethod(DoAsyncException);
{$ENDIF}
      end;
    neSynchronous:
      begin
{$IFDEF SGC_EVENT_DISPATCH}
        TThread.Synchronize(nil,
          procedure
          begin
            DoEventException(nil, Error, aException);
          end);
{$ELSE}
        NotifyException.AddNotifyObject(nil, Error, aException);
        SynchronizeMethod(DoAsyncException);
{$ENDIF}
      end;
  end;
end;

procedure TsgcWSComponent_Base.DoNotifyFragmented(aConnection: TsgcWSConnection;
const aStream: TMemoryStream; const aOpCode: TOpCode;
const aContinuation: Boolean);
{$IFDEF SGC_EVENT_DISPATCH}
var
  oStream: TMemoryStream;
  vOpCode: TOpCode;
  vContinuation: Boolean;
{$ENDIF}
begin
  if IsDestroying then
    exit;

  if Assigned(aConnection) then
  begin
    if aConnection.IsInternal then
      exit;

    case NotifyEvents of
      neNoSync:
        DoEventFragmented(aConnection, aStream, aOpCode, aContinuation);
      neAsynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          oStream := TMemoryStream.Create;
          oStream.LoadFromStream(aStream);
          vOpCode := aOpCode;
          vContinuation := aContinuation;
          TThread.Queue(nil,
            procedure
            begin
              DoEventFragmented(aConnection, oStream, vOpCode, vContinuation);
              oStream.Free;
            end);
{$ELSE}
          NotifyFragmented.AddNotifyObject(aConnection, aStream, aOpCode,
            aContinuation);
          NotifyMethod(DoAsyncFragmented);
{$ENDIF}
        end;
      neSynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          oStream := TMemoryStream.Create;
          oStream.LoadFromStream(aStream);
          vOpCode := aOpCode;
          vContinuation := aContinuation;
          TThread.Synchronize(nil,
            procedure
            begin
              DoEventFragmented(aConnection, oStream, vOpCode, vContinuation);
              oStream.Free;
            end);
{$ELSE}
          NotifyFragmented.AddNotifyObject(aConnection, aStream, aOpCode,
            aContinuation);
          SynchronizeMethod(DoAsyncFragmented);
{$ENDIF}
        end;
    end;
  end;
end;

procedure TsgcWSComponent_Base.DoProcessQueue;
var
  oList: TList{$IFDEF NEXTGEN}<TsgcWSNotifyObject>{$ENDIF};
  oObject: TObject;
begin
  FQueueProcessing := True;
  Try
    { messages }
    oList := QueueMessage.LockList;
    Try
      While oList.Count > 0 do
      begin
        DoEventMessage(TsgcWSNotifyObject(oList.Items[0]).Connection,
          TsgcWSNotifyObject(oList.Items[0]).Text);
        oObject := TObject(oList.Items[0]);
        sgcFree(oObject);
        oList.Delete(0);
      end;
    Finally
      QueueMessage.UnLockList;
    End;

    { binary }
    oList := QueueBinary.LockList;
    Try
      while oList.Count > 0 do
      begin
        DoEventBinary(TsgcWSNotifyObject(oList.Items[0]).Connection,
          TsgcWSNotifyObject(oList.Items[0]).Stream);
        oObject := TObject(oList.Items[0]);
        sgcFree(oObject);
        oList.Delete(0);
      end;
    Finally
      QueueBinary.UnLockList;
    End;
  Finally
    FQueueProcessing := False;
  End;
end;

function TsgcWSComponent_Base.GetQueueBinary: TsgcWSObjectList;
begin
  if not Assigned(FQueueBinary) then
    FQueueBinary := TsgcWSObjectList.Create;
  Result := FQueueBinary;
end;

function TsgcWSComponent_Base.GetQueueFragmented: TsgcWSObjectList;
begin
  if not Assigned(FQueueFragmented) then
    FQueueFragmented := TsgcWSObjectList.Create;
  Result := FQueueFragmented;
end;

function TsgcWSComponent_Base.GetQueueMessage: TsgcWSObjectList;
begin
  if not Assigned(FQueueMessage) then
    FQueueMessage := TsgcWSObjectList.Create;
  Result := FQueueMessage;
end;

procedure TsgcWSComponent_Base.QueueClear;
begin
  DoClearQueue;
end;

procedure TsgcWSComponent_Base.SetQueueMessages(const Value: Boolean);
begin
  if not FQueueProcessing then
  begin
    FQueueMessages := Value;
    if not FQueueMessages then
      DoProcessQueue;
  end;
end;

constructor TsgcWSSpecifications.Create;
begin
  inherited;
  FDrafts := TsgcWSDrafts.Create;
end;

destructor TsgcWSSpecifications.Destroy;
begin
  sgcFree(FDrafts);
  inherited;
end;

procedure TsgcWSSpecifications.SetDrafts(const Value: TsgcWSDrafts);
begin
  FDrafts.Assign(Value);
end;

procedure TsgcWSObjectList.AddNotifyObject(aConnection: TsgcWSConnection;
aParam: String);
var
  oComponent: TsgcWSNotifyObject;
begin
  oComponent := TsgcWSNotifyObject.Create;
  oComponent.Connection := aConnection;
  oComponent.Text := aParam;

  DoAdd(oComponent);
end;

procedure TsgcWSObjectList.AddNotifyObject(aConnection: TsgcWSConnection;
aParam: Integer);
var
  oComponent: TsgcWSNotifyObject;
begin
  oComponent := TsgcWSNotifyObject.Create;
  oComponent.Connection := aConnection;
  oComponent.Text := '';
  oComponent.Int := aParam;

  DoAdd(oComponent);
end;

procedure TsgcWSObjectList.AddNotifyObject(aConnection: TsgcWSConnection;
aParam: TMemoryStream);
var
  oComponent: TsgcWSNotifyObject;
begin
  oComponent := TsgcWSNotifyObject.Create;
  oComponent.Connection := aConnection;
  oComponent.Stream.LoadFromStream(aParam);

  DoAdd(oComponent);
end;

procedure TsgcWSObjectList.AddNotifyObject(aConnection: TsgcWSConnection);
var
  oComponent: TsgcWSNotifyObject;
begin
  oComponent := TsgcWSNotifyObject.Create;
  oComponent.Connection := aConnection;

  DoAdd(oComponent);
end;

procedure TsgcWSObjectList.AddNotifyObject(aConnection: TsgcWSConnection;
aParam: String; aParam1: String);
var
  oComponent: TsgcWSNotifyObject;
begin
  oComponent := TsgcWSNotifyObject.Create;
  oComponent.Connection := aConnection;
  oComponent.Text := aParam;
  oComponent.Text1 := aParam1;

  DoAdd(oComponent);
end;

procedure TsgcWSObjectList.AddNotifyObject(aConnection: TsgcWSConnection;
aStream: TMemoryStream; aOpCode: TOpCode; aBoolean: Boolean);
var
  oComponent: TsgcWSNotifyObject;
begin
  oComponent := TsgcWSNotifyObject.Create;
  oComponent.Connection := aConnection;
  oComponent.Stream.LoadFromStream(aStream);
  oComponent.Int := Ord(aOpCode);
  oComponent.Text1 := '0';
  if aBoolean then
    oComponent.Text1 := '1';

  DoAdd(oComponent);
end;

procedure TsgcWSObjectList.AddNotifyObject(aConnection: TsgcWSConnection;
aParam: String; aException: Exception);
var
  oComponent: TsgcWSNotifyObject;
begin
  oComponent := TsgcWSNotifyObject.Create;
  oComponent.Connection := aConnection;
  oComponent.Text := aParam;
  oComponent.RawException := aException;

  DoAdd(oComponent);
end;

procedure TsgcWSObjectList.DeleteAll(aFreeConnection: Boolean = False);
var
  oList: TList{$IFDEF NEXTGEN}<TsgcWSNotifyObject>{$ENDIF};
  oObject: TsgcWSNotifyObject;
begin
  oList := LockList;
  Try
    while oList.Count > 0 do
    begin
      oObject := TsgcWSNotifyObject(oList.Items[0]);
      if aFreeConnection then
      begin
        if Assigned(oObject.Connection) then
        begin
          if not oObject.Connection.FFreed then
          begin
            Try
              oObject.Connection.Free;
            Except
              // todo: handle exception
            End;
          end;
        end;
      end;
      sgcFree(oObject);
      oList.Delete(0);
    end;
  Finally
    UnLockList;
  End;
end;

procedure TsgcWSObjectList.DoAdd(Item: Pointer);
var
  oList: TList{$IFDEF NEXTGEN}<TsgcWSNotifyObject>{$ENDIF};
begin
  oList := LockList;
  Try
    oList.Add(Item);
  Finally
    UnLockList;
  End;
end;

constructor TsgcWSComponent.Create(aOwner: TComponent);
begin
  inherited;
  // -->start sgc_trial
{$IFDEF SGC_TRIAL}
  if not IsDesigning then
  begin
    if ((Now > EncodeDate(2022, 4, 8)) and (FormatDateTime('ss', Now) = '08'))
    then
    begin
      raise Exception.Create
        (DecodeBase64
        ('VGhpcyBkZW1vIHZlcnNpb24gY2FuIG9ubHkgcnVuIGZvciBhIGxpbWl0ZWQgdGltZSBwZXJpb2QuIFBsZWFzZSB2aXNpdCB3d3cuZXNlZ2VjZS5jb20gdG8gcHVyY2hhc2UgeW91ciBjb3B5IG9mIHRoZSBsaWJyYXJ5Lg==')
        );
    end;
  end;
{$ENDIF}
  // <--end sgc_trial
  FProtocolObjectList :=
{$IFDEF NEXTGEN}TsgcWSProtocols{$ELSE}TObjectList{$ENDIF}.Create;
  FProtocolObjectList.OwnsObjects := False;
  FHeartBeat := TsgcWSHeartBeat_Options.Create;
  FHeartBeat.Enabled := False;
  FHeartBeat.Interval := 300;
  FHeartBeat.Timeout := 0;
  FTCPKeepAlive := TsgcTCPKeepAlive.Create;
  FTCPKeepAlive.Enabled := False;
  FTCPKeepAlive.Time := 7200000;
  FTCPKeepAlive.Interval := 1000;
end;

destructor TsgcWSComponent.Destroy;
begin
  sgcFree(FTCPKeepAlive);
  sgcFree(FHeartBeat);
  sgcThreadFree(FWatchDogTimer);
  sgcFree(FExtensions);
  sgcFree(FSyncHandshake);
  sgcFree(FSpecifications);
  sgcFree(FLogFile);
  sgcFree(FProtocolObjectList);
  inherited;
end;

procedure TsgcWSComponent.DoAsyncHandshake;
begin
  while SyncHandshake.Count > 0 do
  begin
    DoEventHandshake(TsgcWSConnection(SyncHandshake.Items[0]),
      TsgcWSConnection(SyncHandshake.Items[0]).FHeadersResponse);
    SyncHandshake.Extract(TsgcWSConnection(SyncHandshake.Items[0]));
  end;
end;

procedure TsgcWSComponent.DoEventHandshake(aConnection: TsgcWSConnection;
aHeaders: TStringList);
begin
{$IFDEF SGC_DEBUG}
  DoLog(Self, aConnection, 'DoEventHandshake', '[Headers]: ' + aHeaders.Text);
{$ENDIF}
  if Assigned(FOnHandshake) then
    FOnHandshake(aConnection, aHeaders);
end;

procedure TsgcWSComponent.DoNotifyConnect(aConnection: TsgcWSConnection);
var
  i: Integer;
begin
  if IsDestroying then
    exit;

  if Assigned(aConnection) then
  begin
    if aConnection.Protocol <> '' then
    begin
      for i := 0 to FProtocolObjectList.Count - 1 do
      begin
        if ProtocolRegistered(aConnection.Protocol,
          TsgcWSProtocol(FProtocolObjectList[i])) then
          TsgcWSProtocol(FProtocolObjectList[i]).DoNotifyConnect(aConnection);
      end;
    end
    else if Assigned(FAPI) then
    begin
      Try
        FAPI.DoNotifyConnect(aConnection);
      Finally
        if FAPI.RawMessages then
          inherited;
      End;
    end
    else
      inherited;
  end
  else
    inherited;
end;

procedure TsgcWSComponent.DoNotifyDisconnect(aConnection: TsgcWSConnection);
var
  i: Integer;
begin
  if IsDestroying then
    exit;

  if Assigned(aConnection) then
  begin
    if aConnection.Protocol <> '' then
    begin
      for i := 0 to FProtocolObjectList.Count - 1 do
      begin
        if ProtocolRegistered(aConnection.Protocol,
          TsgcWSProtocol(FProtocolObjectList[i])) then
        begin
          TsgcWSProtocol(FProtocolObjectList[i])
            .DoNotifyDisconnect(aConnection);
          exit; // ... prevents to destroy aConnection twice
        end;
      end;
      // ... if disconnects before handshake and is behind broker, protocol can not be found
      // ... call disconnect of broker object
      if sgcContainsText(aConnection.Protocol, CS_PROTOCOL_BROKER) then
      begin
        for i := 0 to FProtocolObjectList.Count - 1 do
        begin
          if ProtocolRegistered(CS_PROTOCOL_BROKER,
            TsgcWSProtocol(FProtocolObjectList[i])) then
          begin
            TsgcWSProtocol(FProtocolObjectList[i])
              .DoNotifyDisconnect(aConnection);
            exit; // ... prevents to destroy aConnection twice
          end;
        end;
      end;
      // ... if not found call inherited
      inherited;
    end
    else if Assigned(FAPI) then
    begin
      Try
        FAPI.DoNotifyDisconnect(aConnection);
      Finally
        if FAPI.RawMessages then
          inherited;
      End;
    end
    else
      inherited;
  end
  else
    inherited;
end;

procedure TsgcWSComponent.DoNotifyError(aConnection: TsgcWSConnection);
var
  i: Integer;
begin
  if IsDestroying then
    exit;

  if Assigned(aConnection) then
  begin
    if aConnection.Protocol <> '' then
    begin
      for i := 0 to FProtocolObjectList.Count - 1 do
      begin
        if ProtocolRegistered(aConnection.Protocol,
          TsgcWSProtocol(FProtocolObjectList[i])) then
          TsgcWSProtocol(FProtocolObjectList[i]).DoNotifyError(aConnection);
      end;
    end
    else if Assigned(FAPI) and (FAPI.RawMessages = False) then
      FAPI.DoNotifyError(aConnection)
    else
      inherited;
  end
  else
    inherited;
end;

procedure TsgcWSComponent.DoNotifyMessage(aConnection: TsgcWSConnection);
var
  i: Integer;
begin
  if IsDestroying then
    exit;

  if Assigned(aConnection) then
  begin
    if aConnection.Protocol <> '' then
    begin
      for i := 0 to FProtocolObjectList.Count - 1 do
      begin
        if ProtocolRegistered(aConnection.Protocol,
          TsgcWSProtocol(FProtocolObjectList[i])) then
          TsgcWSProtocol(FProtocolObjectList[i]).DoNotifyMessage(aConnection);
      end;
    end
    else if Assigned(FAPI) and (FAPI.RawMessages = False) then
      FAPI.DoNotifyMessage(aConnection)
    else if Assigned(FAPI) and (FAPI.IsInternalMessage(aConnection.MsgReceived)
      = False) then
      inherited
    else
      inherited;
  end
  else
    inherited;
end;

procedure TsgcWSComponent.DoNotifyBinary(aConnection: TsgcWSConnection);
var
  i: Integer;
begin
  if IsDestroying then
    exit;

  if Assigned(aConnection) then
  begin
    if aConnection.Protocol <> '' then
    begin
      for i := 0 to FProtocolObjectList.Count - 1 do
      begin
        if ProtocolRegistered(aConnection.Protocol,
          TsgcWSProtocol(FProtocolObjectList[i])) then
          TsgcWSProtocol(FProtocolObjectList[i]).DoNotifyBinary(aConnection);
      end;
    end
    else if Assigned(FAPI) and (FAPI.RawMessages = False) then
      FAPI.DoNotifyBinary(aConnection)
    else
      inherited;
  end
  else
    inherited;
end;

procedure TsgcWSComponent.DoNotifyException(aConnection: TsgcWSConnection);
var
  i: Integer;
begin
  if IsDestroying then
    exit;

  if Assigned(aConnection) then
  begin
    if aConnection.Protocol <> '' then
    begin
      for i := 0 to FProtocolObjectList.Count - 1 do
      begin
        if ProtocolRegistered(aConnection.Protocol,
          TsgcWSProtocol(FProtocolObjectList[i])) then
          TsgcWSProtocol(FProtocolObjectList[i]).DoNotifyException(aConnection);
      end;
    end
    else if Assigned(FAPI) and (FAPI.RawMessages = False) then
      FAPI.DoNotifyException(aConnection)
    else
      inherited;
  end
  else
    inherited;
end;

procedure TsgcWSComponent.DoNotifyFragmented(aConnection: TsgcWSConnection;
const aStream: TMemoryStream; const aOpCode: TOpCode;
const aContinuation: Boolean);
var
  i: Integer;
begin
  if IsDestroying then
    exit;

  if Assigned(aConnection) then
  begin
    if aConnection.Protocol <> '' then
    begin
      for i := 0 to FProtocolObjectList.Count - 1 do
      begin
        if ProtocolRegistered(aConnection.Protocol,
          TsgcWSProtocol(FProtocolObjectList[i])) then
          TsgcWSProtocol(FProtocolObjectList[i]).DoNotifyFragmented(aConnection,
            aStream, aOpCode, aContinuation);
      end;
    end
    else if Assigned(FAPI) and (FAPI.RawMessages = False) then
      FAPI.DoNotifyFragmented(aConnection, aStream, aOpCode, aContinuation)
    else
      inherited;
  end
  else
    inherited;
end;

procedure TsgcWSComponent.DoNotifyHandshake(aConnection: TsgcWSConnection);
begin
  if Assigned(aConnection) then
  begin
    case NotifyEvents of
      neNoSync:
        DoEventHandshake(aConnection, aConnection.HeadersResponse);
      neAsynchronous, neSynchronous:
        begin
          SyncHandshake.Add(aConnection);
          SynchronizeMethod(DoAsyncHandshake);
        end;
    end;
  end;
end;

procedure TsgcWSComponent.DoStartHeartBeat;
begin
  if HeartBeat.Enabled then
  begin
    if not Assigned(FHeartBeatTimer) then
    begin
      FHeartBeatTimer := TsgcTimer.Create;
      FHeartBeatTimer.DebugName := 'HeartBeat';
      FHeartBeatTimer.NotifyEvents := NotifyEvents;
      FHeartBeatTimer.OnTimer := OnHeartBeatEvent;
      FHeartBeatTimer.OnException := OnHeartBeatExceptionEvent;
    end;
    FHeartBeatTimer.Interval := HeartBeat.Interval * 1000;

    if FHeartBeatTimer.Interval > 0 then
    begin
      if not FHeartBeatTimer.Enabled then
        FHeartBeatTimer.Enabled := True;
    end;
  end;
end;

procedure TsgcWSComponent.DoStartHeartBeatTimeout;
begin
  if HeartBeat.Timeout > 0 then
  begin
    if not Assigned(FHeartBeatTimeoutTimer) then
    begin
      FHeartBeatTimeoutTimer := TsgcTimer.Create;
      FHeartBeatTimeoutTimer.DebugName := 'HeartBeatTimeout';
      FHeartBeatTimeoutTimer.NotifyEvents := NotifyEvents;
      FHeartBeatTimeoutTimer.OnTimer := OnHeartBeatTimeoutEvent;
      FHeartBeatTimeoutTimer.OnException := OnHeartBeatTimeoutExceptionEvent;
    end;
    FHeartBeatTimeoutTimer.Interval := HeartBeat.Timeout * 1000;

    if FHeartBeatTimeoutTimer.Interval > 0 then
    begin
      if not FHeartBeatTimeoutTimer.Enabled then
        FHeartBeatTimeoutTimer.Enabled := True;
    end;
  end;
end;

procedure TsgcWSComponent.DoStartWatchDog;
begin
  if GetWatchDog.Enabled then
  begin
    if not Assigned(FWatchDogTimer) then
    begin
      FWatchDogTimer := TsgcTimer.Create;
      FWatchDogTimer.DebugName := 'WatchDog';
      FWatchDogTimer.NotifyEvents := NotifyEvents;
      FWatchDogTimer.OnTimer := OnWatchDogEvent;
      FWatchDogTimer.OnException := OnWatchDogExceptionEvent;
    end;
    FWatchDogTimer.Interval := GetWatchDog.Interval * 1000;

    if FWatchDogTimer.Interval > 0 then
    begin
      if not FWatchDogTimer.Enabled then
        FWatchDogTimer.Enabled := True;
    end;
  end;
end;

procedure TsgcWSComponent.DoStopHeartBeat;
begin
  if Assigned(FHeartBeatTimer) then
  begin
    FHeartBeatTimer.Enabled := False;
    // sgcFree(FHeartBeatTimeoutTimer); // don't release object: pontential thread lock
  end;
end;

procedure TsgcWSComponent.DoStopHeartBeatTimeout;
begin
  if Assigned(FHeartBeatTimeoutTimer) then
  begin
    FHeartBeatTimeoutTimer.Enabled := False;
    // sgcFree(FHeartBeatTimeoutTimer); // don't release object: pontential thread lock
  end;
end;

procedure TsgcWSComponent.DoStopWatchDog;
begin
  sgcThreadFree(FWatchDogTimer);
end;

procedure TsgcWSComponent.DoWriteQueueData(const aConnection: TsgcWSConnection;
const aQueue: TsgcQueueList);
var
  oListQueue: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
  oListQueue2: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
  vSize: Integer;
  vStream: TStream;
  vStreaming: TwsStreaming;
  vText: String;
  oQueueObject: TsgcQueueObject;
{$IFDEF NEXTGEN}
  i: Integer;
{$ENDIF}
begin
  if aQueue.IsEmpty then
    exit;

  if Assigned(aConnection) then
  begin
    if aConnection.Enabled then
    begin
      oListQueue2 := TList{$IFDEF NEXTGEN}<TObject>{$ENDIF}.Create;
      Try
        oListQueue := aQueue.LockList;
        Try
{$IFDEF NEXTGEN}
          for i := 0 to oListQueue.Count - 1 do
            oListQueue2.Add(oListQueue[i]);
{$ELSE}
          oListQueue2.Assign(oListQueue);
{$ENDIF}
          oListQueue.Clear;
        Finally
          aQueue.UnLockList;
        End;

        while oListQueue2.Count > 0 do
        begin
          Case TsgcQueueObject(oListQueue2[0]).OpCode of
            opText:
              begin
                vText := TsgcQueueObject(oListQueue2[0]).Text;
                vSize := TsgcQueueObject(oListQueue2[0]).Size;
                vStreaming := TsgcQueueObject(oListQueue2[0]).Streaming;

                aConnection.DoWriteData(vText, vSize, vStreaming);
              end;
            opBinary:
              begin
                vStream := TsgcQueueObject(oListQueue2[0]).Stream;
                vSize := TsgcQueueObject(oListQueue2[0]).Size;
                vStreaming := TsgcQueueObject(oListQueue2[0]).Streaming;

                aConnection.DoWriteData(vStream, vSize, vStreaming);
              end;
            opPing:
              begin
                vText := TsgcQueueObject(oListQueue2[0]).Text;
                aConnection.DoPing(vText);
              end;
          End;
          oQueueObject := TsgcQueueObject(oListQueue2[0]);
          sgcFree(oQueueObject);
          oListQueue2.Delete(0);
        end;
      Finally
        sgcFree(oListQueue2);
      End;
    end;
  end;
end;

procedure TsgcWSComponent.DoWriteQueueMsgLevels(const aConnection
  : TsgcWSConnection);
begin
  DoWriteQueueData(aConnection, aConnection.QueueLevel0);
  DoWriteQueueData(aConnection, aConnection.QueueLevel1);
  DoWriteQueueData(aConnection, aConnection.QueueLevel2);
end;

procedure TsgcWSComponent.EnterCS;
begin
  DoEnterCS;
end;

function TsgcWSComponent.GetExtensions: TsgcWSExtensions;
begin
  if not Assigned(FExtensions) then
    FExtensions := TsgcWSExtensions.Create;
  Result := FExtensions;
end;

function TsgcWSComponent.GetHeartBeat: TsgcWSHeartBeat_Options;
begin
  Result := FHeartBeat;
end;

function TsgcWSComponent.GetLogFile: TsgcWSLogFile;
begin
  if not Assigned(FLogFile) then
    FLogFile := TsgcWSLogFile.Create;
  Result := FLogFile;
end;

function TsgcWSComponent.GetProtocols: String;
var
  i, j: Integer;
  oList, oList2: TStringList;
begin
  Result := '';
  oList := TStringList.Create;
  oList.Delimiter := CS_DELIMITER;
  Try
    // ... if there is a broker, set it first
    for i := 0 to FProtocolObjectList.Count - 1 do
    begin
      if TsgcWSProtocol(FProtocolObjectList[i]).Protocol = CS_PROTOCOL_BROKER
      then
      begin
        oList.Add(CS_PROTOCOL_BROKER);
        break;
      end;
    end;

    // ... get protocols list
    for i := 0 to FProtocolObjectList.Count - 1 do
    begin
      if TsgcWSProtocol(FProtocolObjectList[i]).Protocol <> CS_PROTOCOL_BROKER
      then
      begin
        if not sgcContainsText(TsgcWSProtocol(FProtocolObjectList[i]).Protocol,
          CS_DELIMITER) then
        begin
          // ... guid + protocol
          if TsgcWSProtocol(FProtocolObjectList[i]).FGuid <> '' then
            oList.Add(TsgcWSProtocol(FProtocolObjectList[i]).FGuid + '.' +
              TsgcWSProtocol(FProtocolObjectList[i]).Protocol)
            // ... protocol
          else
            oList.Add(TsgcWSProtocol(FProtocolObjectList[i]).Protocol);
        end
        else
        begin
          oList2 := TStringList.Create;
          Try
            oList2.CommaText := TsgcWSProtocol(FProtocolObjectList[i]).Protocol;
            for j := 0 to oList2.Count - 1 do
            begin
              // ... guid + protocol
              if TsgcWSProtocol(FProtocolObjectList[i]).FGuid <> '' then
                oList.Add(TsgcWSProtocol(FProtocolObjectList[i]).FGuid + '.' +
                  oList2[j])
                // ... protocol
              else
                oList.Add(oList2[j]);
            end;
          Finally
            sgcFree(oList2);
          End;
        end;
      end;
    end;

    // ... return result
    Result := oList.CommaText;
  Finally
    sgcFree(oList);
  End;
end;

function TsgcWSComponent.GetSpecifications: TsgcWSSpecifications;
begin
  if not Assigned(FSpecifications) then
  begin
    FSpecifications := TsgcWSSpecifications.Create;
    FSpecifications.RFC6455 := True;
    FSpecifications.RFC8441 := False;
    FSpecifications.Drafts.Hixie76 := True;
  end;
  Result := FSpecifications;
end;

function TsgcWSComponent.GetSyncHandshake:
{$IFDEF NEXTGEN}TsgcWSHandshakes{$ELSE}TObjectList{$ENDIF};
begin
  if not Assigned(FSyncHandshake) then
    FSyncHandshake :=
{$IFDEF NEXTGEN}TsgcWSHandshakes{$ELSE}TObjectList{$ENDIF}.Create;
  Result := FSyncHandshake;
end;

procedure TsgcWSComponent.LeaveCS;
begin
  DoLeaveCS;
end;

procedure TsgcWSComponent.OnWatchDogEvent(Sender: TObject);
begin
  if GetWatchDog.Attempts > 0 then
  begin
    FWatchDogAttempts := FWatchDogAttempts + 1;

    if FWatchDogAttempts >= GetWatchDog.Attempts then
      DoStopWatchDog;
  end;
end;

function TsgcWSComponent.ProtocolRegistered(aProtocolName: String;
aProtocol: TsgcWSProtocol): Boolean;
var
  i: Integer;
  oList: TsgcDelimitedStringList;
begin
  // ... search by name
  Result := UpperCase(aProtocol.FGuid + '.' + aProtocol.Protocol)
    = UpperCase(aProtocolName);
  if not Result then
    Result := UpperCase(aProtocol.Protocol) = UpperCase(aProtocolName);

  // ... search by protocol
  if not Result and (Pos(CS_DELIMITER, aProtocol.Protocol) > 0) then
  begin
    oList := TsgcDelimitedStringList.Create;
    Try
      oList.Delimiter := CS_DELIMITER;
      oList.DelimitedText := aProtocol.Protocol;
      for i := 0 to oList.Count - 1 do
      begin
        Result := UpperCase(aProtocol.FGuid + '.' + oList[i])
          = UpperCase(aProtocolName);
        if not Result then
          Result := UpperCase(oList[i]) = UpperCase(aProtocolName);
        if Result then
          break;
      end;
    Finally
      sgcFree(oList);
    End;
  end;
end;

procedure TsgcWSComponent.RegisterAPI(aObject: TsgcWSAPI);
begin
  // ... assign component notification type
  aObject.NotifyEvents := NotifyEvents;

  // ... assign object
  FAPI := aObject;
end;

procedure TsgcWSComponent.DoRegisterProtocol(aObject: TsgcWSProtocol);
begin
  // ... assign component notification type
  aObject.NotifyEvents := NotifyEvents;

  // ... register protocol
  FProtocolObjectList.Add(aObject);
end;

procedure TsgcWSComponent.DoRegisterProtocol(aProtocol: String);
var
  oProtocol: TsgcWSProtocol;
begin
  oProtocol := TsgcWSProtocol.Create(Self);
  oProtocol.FProtocol := aProtocol;
  oProtocol.OnConnect := OnConnect;
  oProtocol.OnDisconnect := OnDisconnect;
  oProtocol.OnMessage := OnMessage;
  oProtocol.OnBinary := OnBinary;
  oProtocol.OnFragmented := OnFragmented;
  oProtocol.OnError := OnError;
  oProtocol.OnException := OnException;

  DoRegisterProtocol(oProtocol);
end;

procedure TsgcWSComponent.SetExtensions(const Value: TsgcWSExtensions);
begin
  Extensions.Assign(Value);
end;

procedure TsgcWSComponent.SetHeartBeat(const Value: TsgcWSHeartBeat_Options);
begin
  FHeartBeat.Assign(Value);
end;

procedure TsgcWSComponent.SetLogFile(const Value: TsgcWSLogFile);
begin
  LogFile.Assign(Value);
end;

procedure TsgcWSComponent.SetNotifyEvents(const Value: TwsNotifyEvent);
begin
  inherited;
  if Assigned(FHeartBeatTimer) then
    FHeartBeatTimer.NotifyEvents := Value;
  if Assigned(FHeartBeatTimeoutTimer) then
    FHeartBeatTimeoutTimer.NotifyEvents := Value;
  if Assigned(FWatchDogTimer) then
    FWatchDogTimer.NotifyEvents := Value;
end;

procedure TsgcWSComponent.SetSpecifications(const Value: TsgcWSSpecifications);
begin
  Specifications.Assign(Value);
end;

procedure TsgcWSComponent_Server.SetWatchDog(const Value
  : TsgcWSWatchDogServer_Options);
begin
  if Assigned(FWatchDog) then
    FWatchDog.Assign(Value);
end;

procedure TsgcWSComponent.UnRegisterAPI(aObject: TsgcWSAPI);
begin
  FAPI := nil;
end;

procedure TsgcWSComponent.DoUnRegisterProtocol(aObject: TsgcWSProtocol);
begin
  FProtocolObjectList.Extract(aObject);
end;

procedure TsgcWSComponent.DoUnRegisterProtocol(aProtocol: String);
var
  i: Integer;
  oProtocol: TObject;
begin
  for i := FProtocolObjectList.Count - 1 downto 0 do
  begin
    if TsgcWSProtocol(FProtocolObjectList.Items[i]).Protocol = aProtocol then
    begin
      oProtocol := FProtocolObjectList.Extract
        (TsgcWSProtocol(FProtocolObjectList.Items[i]));
{$IFNDEF NEXTGEN}
      if Assigned(oProtocol) then
        TsgcWSProtocol(oProtocol).Free;
{$ENDIF}
    end;
  end;
end;

function TsgcWSComponent.AssignedCS: Boolean;
begin
  Result := DoAssignedCS;
end;

procedure TsgcWSComponent.SetTCPKeepAlive(const Value: TsgcTCPKeepAlive);
begin
  if Assigned(FTCPKeepAlive) then
    FTCPKeepAlive.Assign(Value);
end;

constructor TsgcWSProtocol_Client.Create(aOwner: TComponent);
begin
  inherited;
  FClient := nil;
end;

destructor TsgcWSProtocol_Client.Destroy;
begin
  Client := nil;
  inherited;
end;

procedure TsgcWSProtocol_Client.DoDestroyConnection
  (aConnection: TsgcSocketConnection);
begin
  if Assigned(Client) then
  begin
    if Client.FreeConnectionOnDestroy then
    begin
      Client.DoDestroyDelayedConnection;
      // destroy previous connection, if exists
      Client.FDelayedFreeConnection := TsgcWSConnection(aConnection);
    end
    else
      inherited;
  end
  else
    inherited;
end;

procedure TsgcWSProtocol_Client.DoNotifyBinary(aConnection: TsgcWSConnection);
var
  oStream: TsgcStringStream;
begin
  if MsgType = msgText then
  begin
    if Assigned(Client) then
    begin
      // if protocol rewrite as text
      if Client.Specifications.RFC6455 = False then
      begin
        oStream := TsgcStringStream.Create('');
        Try
          aConnection.MsgBinaryReceived.Position := 0;
          oStream.CopyFrom(aConnection.MsgBinaryReceived,
            aConnection.MsgBinaryReceived.Size);
          aConnection.MsgReceived := oStream.DataString;
          DoNotifyMessage(aConnection);
        Finally
          sgcFree(oStream);
        end
      end
      else
        inherited;
    end
    else
      inherited;
  end
  else
    inherited;
end;

procedure TsgcWSProtocol_Client.DoEventConnect(aConnection: TsgcWSConnection);
begin
  inherited;
  // ... stop watchdog
  if Assigned(Client) then
  begin
    Client.DoStopWatchDog;
    Client.FWatchDogAttempts := 0;
  end;
end;

procedure TsgcWSProtocol_Client.DoEventDisconnect(aConnection: TsgcWSConnection;
Code: Integer);
begin
  inherited;
  if Assigned(Client) then
  begin
    // ... stop heartbeat
    Client.DoStopHeartBeatTimeout;
    Client.DoStopHeartBeat;
    // ... start watchdog
    if Client.FWatchDogEnabled then
    begin
      if (Client.WatchDog.Attempts = 0) or
        (Client.WatchDog.Attempts > Client.FWatchDogAttempts) then
        Client.DoStartWatchDog;
    end;
  end;
end;

function TsgcWSProtocol_Client.DoHandleEventConnect: Boolean;
begin
  Result := False;
end;

function TsgcWSProtocol_Client.DoKeepAlive: Boolean;
begin
  Result := False;
end;

procedure TsgcWSProtocol_Client.Loaded;
begin
  inherited;
  if Assigned(Client) then
    Client.DoRegisterProtocol(Self);
end;

procedure TsgcWSProtocol_Client.Notification(AComponent: TComponent;
Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FClient) then
    FClient := nil;
end;

procedure TsgcWSProtocol_Client.SetClient(const Value
  : TsgcWSComponent_WSClient);
begin
  if Assigned(FClient) then
    FClient.RemoveFreeNotification(Self);

  if [csDesigning, csLoading] * ComponentState = [] then
  begin
    if Assigned(FClient) then
    begin
      if not Assigned(Value) then
      begin
        FClient.DoUnRegisterProtocol(Self);
        FClient := Value;
      end
      else if FClient <> Value then
      begin
        FClient := Value;
        FClient.DoRegisterProtocol(Self);
      end;
    end
    else
    begin
      FClient := Value;
      if Assigned(Value) then
        FClient.DoRegisterProtocol(Self);
    end;
  end
  else
    FClient := Value;

  if Assigned(FClient) then
    FClient.FreeNotification(Self);
end;

constructor TsgcWSProtocol_Server.Create(aOwner: TComponent);
begin
  inherited;
  FServer := nil;
end;

destructor TsgcWSProtocol_Server.Destroy;
begin
  Server := nil;
  inherited;
end;

procedure TsgcWSProtocol_Server.DoDestroyConnection
  (aConnection: TsgcSocketConnection);
begin
  if Assigned(Server) then
  begin
    if Server.Optimizations.ConnectionsFree.Enabled then
      Server.GetConnectionsFree.Add(aConnection)
    else
      inherited;
  end
  else
    inherited;
end;

procedure TsgcWSProtocol_Server.DoNotifyBinary(aConnection: TsgcWSConnection);
var
  oStream: TsgcStringStream;
begin
  if MsgType = msgText then
  begin
    if Assigned(Server) then
    begin
      // if protocol rewrite as text
      if Server.Specifications.RFC6455 = False then
      begin
        oStream := TsgcStringStream.Create('');
        Try
          aConnection.MsgBinaryReceived.Position := 0;
          oStream.CopyFrom(aConnection.MsgBinaryReceived,
            aConnection.MsgBinaryReceived.Size);
          aConnection.MsgReceived := oStream.DataString;
          DoNotifyMessage(aConnection);
        Finally
          sgcFree(oStream);
        end
      end
      else
        inherited;
    end
    else
      inherited;
  end
  else
    inherited;
end;

function TsgcWSProtocol_Server.GetHTTPResponse(const aConnection
  : TsgcWSConnection; const aPath, aBody: String): String;
begin
  Result := '';
end;

procedure TsgcWSProtocol_Server.Loaded;
begin
  inherited;
  if Assigned(Server) then
    Server.DoRegisterProtocol(Self);
end;

procedure TsgcWSProtocol_Server.Notification(AComponent: TComponent;
Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FServer) then
    FServer := nil;
end;

procedure TsgcWSProtocol_Server.SetServer(const Value: TsgcWSComponent_Server);
begin
  if Assigned(FServer) then
    FServer.RemoveFreeNotification(Self);

  if [csDesigning, csLoading] * ComponentState = [] then
  begin
    if Assigned(FServer) then
    begin
      if not Assigned(Value) then
      begin
        FServer.DoUnRegisterProtocol(Self);
        FServer := Value;
      end
      else if FServer <> Value then
      begin
        FServer := Value;
        FServer.DoRegisterProtocol(Self);
      end;
    end
    else
    begin
      FServer := Value;
      if Assigned(Value) then
        FServer.DoRegisterProtocol(Self);
    end;
  end
  else
    FServer := Value;

  if Assigned(FServer) then
    FServer.FreeNotification(Self);
end;

destructor TsgcWSNotifyObject.Destroy;
begin
  sgcFree(FRawException);
  sgcFree(FStream);
  inherited;
end;

function TsgcWSNotifyObject.GetStream: TMemoryStream;
begin
  if not Assigned(FStream) then
    FStream := TMemoryStream.Create;
  Result := FStream;
end;

procedure TsgcWSNotifyObject.SetRawException(const Value: Exception);
begin
  if Assigned(Value) then
  begin
    if Value.ClassType = EIdSilentException then
      FRawException := EIdSilentException.Create(Value.Message)
    else if Value.ClassType = EIdConnClosedGracefully then
      FRawException := EIdConnClosedGracefully.Create(Value.Message)
    else if Value.ClassType = EIdSocketHandleError then
      FRawException := EIdSocketHandleError.Create(Value.Message)
    else if Value.ClassType = EIdCouldNotBindSocket then
      FRawException := EIdCouldNotBindSocket.Create(Value.Message)
    else if Value.ClassType = EIdCanNotBindPortInRange then
      FRawException := EIdCanNotBindPortInRange.Create(Value.Message)
    else if Value.ClassType = EIdInvalidPortRange then
      FRawException := EIdInvalidPortRange.Create(Value.Message)
    else if Value.ClassType = EIdCannotSetIPVersionWhenConnected then
      FRawException := EIdCannotSetIPVersionWhenConnected.Create(Value.Message)
    else if Value.ClassType = TsgcWSException then
      FRawException := TsgcWSException.Create(Value.Message)
    else
      FRawException := Exception.Create(Value.Message);
  end;
end;

procedure TsgcWSDrafts.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSDrafts then
    FHixie76 := TsgcWSDrafts(aSource).Hixie76
  else
    inherited Assign(aSource);
end;

constructor TsgcWSComponent_Server.Create(aOwner: TComponent);
begin
  inherited;
{$IFDEF CONSOLE}
  NotifyEvents := neNoSync;
{$ELSE}
{$IFDEF MSWINDOWS}
  if IsLibrary then
    NotifyEvents := neNoSync
  else
    NotifyEvents := neAsynchronous;
{$ELSE}
  NotifyEvents := neAsynchronous;
{$ENDIF}
{$ENDIF}
  FWatchDog := TsgcWSWatchDogServer_Options.Create;
  FWatchDog.Enabled := False;
  FOptimizations := TsgcWSOptimization_Options.Create;
  FOptimizations.Connections.Enabled := True;
  FOptimizations.Connections.CacheSize := 100;
  FOptimizations.Connections.GroupLevel := 1;
  FOptimizations.Channels.Enabled := True;
  FOptimizations.ConnectionsFree.Enabled := True;
end;

destructor TsgcWSComponent_Server.Destroy;
begin
  sgcFree(FWatchDog);
  sgcFree(FConnectionsFreeTimer);
  sgcFree(FConnectionsFree);
  sgcFree(FOptimizations);
  sgcFree(FConnections);
  sgcFree(FChannels);
  inherited;
end;

procedure TsgcWSComponent_Server.DoDestroyConnection
  (aConnection: TsgcSocketConnection);
begin
  if Optimizations.ConnectionsFree.Enabled then
    GetConnectionsFree.Add(aConnection)
  else
    inherited;
end;

function TsgcWSComponent_Server.DoHTTPRequestAPI(const aMethod, aDocument,
    aQueryParams, aContent: string; var Response: TsgcHTTPResponse): Boolean;
var
  oRequest: TsgcHTTPRequest;
begin
  result := False;

  if Assigned(FAPI) then
  begin
    oRequest := TsgcHTTPRequest.Create;
    Try
      oRequest.Method := aMethod;
      oRequest.Document := aDocument;
      oRequest.QueryParams := aQueryParams;
      oRequest.Content := aContent;

      result := TsgcWSAPI_server(FAPI).DoHTTPRequest(oRequest, Response);
    Finally
      sgcFree(oRequest);
    End;
  end;
end;

procedure TsgcWSComponent_Server.DoNotifyConnect(aConnection: TsgcWSConnection);
var
  oItem: TsgcQueueItemConnection;
begin
  if IsDestroying then
    exit;

  if Optimizations.Connections.Enabled then
  begin
    if Assigned(aConnection) then
    begin
      oItem := TsgcQueueItemConnection.Create;
      oItem.Connection := aConnection;
      GetConnections.AddItem(oItem);
    end;
  end;

  inherited;
end;

procedure TsgcWSComponent_Server.DoNotifyDisconnect
  (aConnection: TsgcWSConnection);
var
  vGuid: String;
begin
  if not IsDestroying and Assigned(aConnection) then
  begin
    vGuid := aConnection.Guid;

    if Optimizations.Channels.Enabled then
    begin
      if Assigned(aConnection) then
        GetChannels.DeleteItem(vGuid);
    end;

    if Optimizations.Connections.Enabled then
    begin
      if Assigned(aConnection) then
        GetConnections.DeleteItem(vGuid);
    end;
  end;

  inherited;
end;

procedure TsgcWSComponent_Server.DoStartConnectionFree;
begin
  if Optimizations.ConnectionsFree.Enabled then
  begin
    if not Assigned(FConnectionsFreeTimer) then
    begin
      FConnectionsFreeTimer := TsgcTimer.Create;
      FConnectionsFreeTimer.DebugName := 'ConnectionsFree';
      FConnectionsFreeTimer.NotifyEvents := NotifyEvents;
      FConnectionsFreeTimer.OnTimer := OnConnectionsFreeEvent;
      FConnectionsFreeTimer.OnException := OnConnectionsFreeExceptionEvent;
      FConnectionsFreeTimer.Interval :=
        Optimizations.ConnectionsFree.Interval * 1000;
    end;
    if not FConnectionsFreeTimer.Enabled then
      FConnectionsFreeTimer.Enabled := True;
  end;
end;

procedure TsgcWSComponent_Server.DoStopConnectionFree;
begin
  if Assigned(FConnectionsFreeTimer) then
  begin
    if FConnectionsFreeTimer.Enabled then
    begin
      FConnectionsFreeTimer.Enabled := False;
      sgcFree(FConnectionsFreeTimer); // release object
      GetConnectionsFree.Clear;
    end;
  end;
end;

function TsgcWSComponent_Server.DoUnknownProtocol(aConnnection
  : TsgcWSConnection): Boolean;
begin
  Result := False;
  if Assigned(FOnUnknownProtocol) then
    FOnUnknownProtocol(aConnnection, Result);
end;

function TsgcWSComponent_Server.GetChannels: TsgcQueueListChannels;
begin
  if not Assigned(FChannels) then
    FChannels := TsgcQueueListChannels.Create;
  Result := FChannels;
end;

function TsgcWSComponent_Server.GetConnections: TsgcCacheQueueListConnections;
begin
  if not Assigned(FConnections) then
  begin
    FConnections := TsgcCacheQueueListConnections.Create(nil);
    FConnections.CacheSize := Optimizations.Connections.CacheSize;
    FConnections.GroupLevel := Optimizations.Connections.GroupLevel;
  end;
  Result := FConnections;
end;

function TsgcWSComponent_Server.GetConnectionsFree: TsgcQueueCommon;
begin
  if not Assigned(FConnectionsFree) then
  begin
    FConnectionsFree := TsgcQueueCommon.Create;
    FConnectionsFree.OwnObjects := True;
  end;
  Result := FConnectionsFree;
end;

function TsgcWSComponent_Server.GetWatchDog: TsgcWSWatchDog_Options;
begin
  Result := TsgcWSWatchDog_Options(FWatchDog);
end;

procedure TsgcWSComponent_Server.OnConnectionsFreeEvent(Sender: TObject);
begin
  GetConnectionsFree.Clear;
end;

procedure TsgcWSComponent_Server.OnConnectionsFreeExceptionEvent
  (Sender: TObject; E: Exception);
begin
  DoNotifyException(E.Message, E);
end;

procedure TsgcWSComponent_Server.RegisterProtocol(aObject: TsgcWSProtocol);
begin
  TsgcWSProtocol_Server(aObject).Server := Self;
end;

procedure TsgcWSComponent_Server.RegisterProtocol(aProtocol: String);
begin
  DoRegisterProtocol(aProtocol);
end;

procedure TsgcWSComponent_Server.SetNotifyEvents(const Value: TwsNotifyEvent);
begin
  inherited;
  if Assigned(FConnectionsFreeTimer) then
    FConnectionsFreeTimer.NotifyEvents := Value;
end;

procedure TsgcWSComponent_Server.SetOptimizations
  (const Value: TsgcWSOptimization_Options);
begin
  FOptimizations.Assign(Value);
end;

procedure TsgcWSComponent_Server.UnRegisterProtocol(aObject: TsgcWSProtocol);
begin
  TsgcWSProtocol_Server(aObject).Server := nil;
end;

procedure TsgcWSComponent_Server.UnRegisterProtocol(aProtocol: String);
begin
  DoUnRegisterProtocol(aProtocol);
end;

constructor TsgcWSComponent_Client.Create(aOwner: TComponent);
begin
  inherited;
  FDelayedFreeConnection := nil;
{$IFDEF CONSOLE}
  NotifyEvents := neNoSync;
{$ELSE}
{$IFDEF MSWINDOWS}
  if IsLibrary then
    NotifyEvents := neNoSync
  else
    NotifyEvents := neAsynchronous;
{$ELSE}
  NotifyEvents := neAsynchronous;
{$ENDIF}
{$ENDIF}
  FWatchDog := TsgcWSWatchDogClient_Options.Create;
  FWatchDog.Enabled := False;
  FWatchDog.Interval := 10;
end;

destructor TsgcWSComponent_Client.Destroy;
begin
  sgcFree(FWatchDog);
  sgcFree(FDelayedFreeConnection);
  inherited;
end;

procedure TsgcWSComponent_Client.DoDestroyConnection
  (aConnection: TsgcSocketConnection);
begin
  if FreeConnectionOnDestroy then
  begin
    DoDestroyDelayedConnection; // destroy previous connection, if exists
    FDelayedFreeConnection := TsgcWSConnection(aConnection);
  end
  else
    inherited;
end;

procedure TsgcWSComponent_Client.DoDestroyDelayedConnection;
begin
  if Assigned(FDelayedFreeConnection) then
  begin
    inherited DoDestroyConnection(FDelayedFreeConnection);
    FDelayedFreeConnection := nil;
  end;
end;

procedure TsgcWSComponent_Client.DoEventConnect(aConnection: TsgcWSConnection);
begin
  DoStopWatchDog;

  inherited;

  FWatchDogAttempts := 0;
end;

function TsgcWSComponent_Client.GetWatchDog: TsgcWSWatchDog_Options;
begin
  Result := TsgcWSWatchDog_Options(FWatchDog);
end;

procedure TsgcWSComponent_Client.RegisterAPI(aObject: TsgcWSAPI);
begin
  if not aObject.InheritsFrom(TsgcWSAPI_client) then
    raise Exception.CreateFmt(S_ERROR_API_NOT_SUPPORTED, [aObject.className]);
  TsgcWSAPI_client(aObject).Client := TsgcWSComponent_WSClient(Self);
  inherited;
  if TsgcWSAPI_client(aObject).URL <> '' then
    URL := TsgcWSAPI_client(aObject).URL;
end;

procedure TsgcWSComponent_Client.RegisterProtocol(aObject: TsgcWSProtocol);
begin
  TsgcWSProtocol_Client(aObject).Client := TsgcWSComponent_WSClient(Self);
end;

procedure TsgcWSComponent_Client.RegisterProtocol(aProtocol: String);
begin
  DoRegisterProtocol(aProtocol);
end;

procedure TsgcWSComponent_Client.SetNotifyEvents(const Value: TwsNotifyEvent);
begin
  inherited;
  case Value of
    neNoSync:
      FreeConnectionOnDestroy := True;
  else
    FreeConnectionOnDestroy := False;
  end;
end;

procedure TsgcWSComponent_Client.SetWatchDog(const Value
  : TsgcWSWatchDogClient_Options);
begin
  if Assigned(FWatchDog) then
    FWatchDog.Assign(Value);
end;

procedure TsgcWSComponent_Client.UnRegisterAPI(aObject: TsgcWSAPI);
begin
  if not aObject.InheritsFrom(TsgcWSAPI_client) then
    raise Exception.CreateFmt(S_ERROR_API_NOT_SUPPORTED, [aObject.className]);
  inherited;
end;

procedure TsgcWSComponent_Client.UnRegisterProtocol(aObject: TsgcWSProtocol);
begin
  TsgcWSProtocol_Client(aObject).Client := nil;
end;

procedure TsgcWSComponent_Client.UnRegisterProtocol(aProtocol: String);
begin
  DoUnRegisterProtocol(aProtocol);
end;

procedure TsgcWSAuthentication_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSAuthentication_Options then
    FEnabled := TsgcWSAuthentication_Options(aSource).Enabled
  else
    inherited Assign(aSource);
end;

procedure TsgcWSAuthentication_Session.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSAuthentication_Session then
    FEnabled := TsgcWSAuthentication_Session(aSource).Enabled
  else
    inherited Assign(aSource);
end;

procedure TsgcWSAuthentication_URL.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSAuthentication_URL then
    FEnabled := TsgcWSAuthentication_URL(aSource).Enabled
  else
    inherited Assign(aSource);
end;

procedure TsgcWSProtocol.AddProtocolDataItem(const aConnection
  : TsgcWSConnection; const aID: string;
const aItem: TsgcQueueItemProtocolMessage);
begin
  aItem.ID := aID;
  aConnection.ProtocolData.AddItem(aItem);
end;

procedure TsgcWSProtocol.DoClear(aConnection: TsgcWSConnection);
begin
  // clear
end;

procedure TsgcWSProtocol.DoFinalize(aConnection: TsgcWSConnection);
begin
  // finalize
end;

procedure TsgcWSProtocol.DoInitialize(aConnection: TsgcWSConnection);
begin
  // initialize
end;

function TsgcWSProtocol.DoRawMessage(aConnection: TsgcWSConnection;
const Text: String): Boolean;

begin
  Result := False;

  if Assigned(FOnRawMessage) then
    FOnRawMessage(aConnection, Text, Result);
end;

procedure TsgcWSProtocol.EnterCS;
begin
  DoEnterCS;
end;

function TsgcWSProtocol.GetProtocolDataItem(aConnection: TsgcWSConnection;
const aID: String): TsgcQueueItemProtocolMessage;
begin
  Result := nil;

  if Assigned(aConnection) then
    Result := TsgcQueueItemProtocolMessage
      (aConnection.ProtocolData.GetItem(aID));
end;

procedure TsgcWSProtocol.LeaveCS;
begin
  DoLeaveCS;
end;

constructor TsgcQueueObject.Create;
begin
  inherited;
  OpCode := opText;
  Size := 0;
  Streaming := stmNone;
end;

destructor TsgcQueueObject.Destroy;
begin
  sgcFree(FStream);
  inherited;
end;

function TsgcQueueObject.GetStream: TStream;
begin
  Result := TStream(FStream);
end;

procedure TsgcQueueObject.SetStream(const Value: TStream);
begin
  if Assigned(FStream) then
    sgcFree(FStream);
  FStream := TMemoryStream.Create;
  Value.Position := 0;
  FStream.CopyFrom(Value, Value.Size);
end;

procedure TsgcWSAuthentication_Basic.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSAuthentication_Basic then
    FEnabled := TsgcWSAuthentication_Basic(aSource).Enabled
  else
    inherited Assign(aSource);
end;

procedure TsgcWSHeartBeat_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSHeartBeat_Options then
  begin
    Enabled := TsgcWSHeartBeat_Options(aSource).Enabled;
    Interval := TsgcWSHeartBeat_Options(aSource).Interval;
    Timeout := TsgcWSHeartBeat_Options(aSource).Timeout;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcWSQueueOptions.Create;
begin
  inherited;
  FText := TsgcWSQueue.Create;
  FBinary := TsgcWSQueue.Create;
  FPing := TsgcWSQueue.Create;
end;

destructor TsgcWSQueueOptions.Destroy;
begin
  sgcFree(FPing);
  sgcFree(FBinary);
  sgcFree(FText);
  inherited;
end;

procedure TsgcWSQueueOptions.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSQueueOptions then
  begin
    Text.Level := TsgcWSQueueOptions(aSource).Text.Level;
    Binary.Level := TsgcWSQueueOptions(aSource).Binary.Level;
    Ping.Level := TsgcWSQueueOptions(aSource).Ping.Level;
  end
  else
    inherited Assign(aSource);
end;

destructor TsgcQueueList.Destroy;
begin
  FreeObjects;
  Clear;
  inherited;
end;

procedure TsgcQueueList.FreeObjects;
var
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
  oQueueObject: TsgcQueueObject;
begin
  oList := LockList;
  Try
    while oList.Count > 0 do
    begin
      oQueueObject := TsgcQueueObject(oList.Items[0]);
      sgcFree(oQueueObject);
      oList.Delete(0);
    end;
  Finally
    UnLockList;
  End;
end;

procedure TsgcWSQueue.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSQueue then
  begin
    Level := TsgcWSQueue(aSource).Level;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcWSAPI.Create(aOwner: TComponent);
begin
  inherited;
  RawMessages := False;
end;

function TsgcWSAPI.IsInternalMessage(const aMessage: String): Boolean;
begin
  Result := False;
end;

procedure TsgcWSAPI_client.DoBeforeConnect;
begin

end;

function TsgcWSAPI_client.DoKeepAlive: Boolean;
begin
  Result := False;
end;

procedure TsgcWSAPI_client.DoNotifyConnect(aConnection: TsgcWSConnection);
begin
  // inherited; don't inherit, this is done later if RawMessages = True
  // ... stop watchdog
  if Assigned(Client) then
  begin
    if FRawMessages = False then
    begin
      Client.DoStopWatchDog;
      Client.FWatchDogAttempts := 0;
      inherited;
    end;
  end;
end;

procedure TsgcWSAPI_client.DoNotifyDisconnect(aConnection: TsgcWSConnection);
begin
  // inherited; don't inherit, this is done later if RawMessages = True
  if Assigned(Client) then
  begin
    if RawMessages = False then
    begin
      // ... stop heartbeat
      Client.DoStopHeartBeatTimeout;
      Client.DoStopHeartBeat;
      // ... start watchdog
      if (Client.WatchDog.Attempts = 0) or
        (Client.WatchDog.Attempts > Client.FWatchDogAttempts) then
        Client.DoStartWatchDog;
      inherited;
    end;
  end;
end;

function TsgcWSAPI_client.GetURL: String;
begin
  Result := '';
end;

procedure TsgcWSAPI_client.Loaded;
begin
  inherited;
  if Assigned(Client) then
    Client.RegisterAPI(Self);
end;

procedure TsgcWSAPI_client.Notification(AComponent: TComponent;
Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FClient) then
    FClient := nil;
end;

procedure TsgcWSAPI_client.SetClient(const Value: TsgcWSComponent_WSClient);
begin
  if Assigned(FClient) then
    FClient.RemoveFreeNotification(Self);

  if [csDesigning, csLoading] * ComponentState = [] then
  begin
    if Assigned(FClient) then
    begin
      if not Assigned(Value) then
      begin
        FClient.UnRegisterAPI(Self);
        FClient := Value;
      end
      else if FClient <> Value then
      begin
        FClient := Value;
        FClient.RegisterAPI(Self);
      end;
    end
    else
    begin
      FClient := Value;
      if Assigned(Value) then
        FClient.RegisterAPI(Self);
    end;
  end
  else
    FClient := Value;

  if Assigned(FClient) then
    FClient.FreeNotification(Self);
end;

function TsgcWSAPI_client_KeepAlive.DoKeepAlive: Boolean;
begin
  Result := inherited DoKeepAlive;
end;

constructor TsgcWSComponent_WSClient.Create(aOwner: TComponent);
begin
  inherited;
  FTLSOptions := TsgcWSTLS_Options.Create;
end;

destructor TsgcWSComponent_WSClient.Destroy;
begin
  sgcFree(FTLSOptions);
  inherited;
end;

procedure TsgcWSComponent_WSClient.SetActive(const Value: Boolean);
begin
  if Value then
  begin
    if Assigned(FAPI) then
    begin
      if Assigned(TsgcWSAPI_client(FAPI).Client) then
      begin
        if not TsgcWSAPI_client(FAPI).Client.Active then
          TsgcWSAPI_client(FAPI).DoBeforeConnect;
      end;
    end;
  end;
end;

procedure TsgcWSComponent_WSClient.SetTLSOptions(const Value
  : TsgcWSTLS_Options);
begin
  FTLSOptions.Assign(Value);
end;

procedure TsgcWSComponent_WSClient_API.DoStartHeartBeat;
begin
  inherited;
end;

procedure TsgcWSComponent_WSClient_API.DoStopHeartBeat;
begin
  inherited;
end;

constructor TsgcQueueItemConnection.Create;
begin
  inherited;
  FConnection := nil;
end;

destructor TsgcQueueItemConnection.Destroy;
begin
  Connection := nil;
  inherited;
end;

procedure TsgcQueueItemConnection.SetConnection(const Value: TsgcWSConnection);
begin
  FConnection := Value;
  if Assigned(FConnection) then
    ID := FConnection.Guid;
end;

function TsgcCacheQueueListConnections.GetConnection(const aGuid: String)
  : TsgcWSConnection;
var
  oItem: TsgcQueueItemConnection;
begin
  Result := nil;

  oItem := TsgcQueueItemConnection(GetItem(aGuid));
  if Assigned(oItem) then
    Result := oItem.Connection;
end;

procedure TsgcWSConnections_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSConnections_Options then
  begin
    Enabled := TsgcWSConnections_Options(aSource).Enabled;
    CacheSize := TsgcWSConnections_Options(aSource).CacheSize;
    GroupLevel := TsgcWSConnections_Options(aSource).GroupLevel;
  end
  else
    inherited Assign(aSource);
end;

function TsgcWSConnections_Options.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

constructor TsgcWSOptimization_Options.Create;
begin
  inherited;
  FConnections := TsgcWSConnections_Options.Create;
  FChannels := TsgcWSChannels_Options.Create;
  FConnectionsFree := TsgcWSConnectionsFree_Options.Create;
end;

destructor TsgcWSOptimization_Options.Destroy;
begin
  sgcFree(FConnectionsFree);
  sgcFree(FChannels);
  sgcFree(FConnections);
  inherited;
end;

procedure TsgcWSOptimization_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSOptimization_Options then
  begin
    Connections := TsgcWSOptimization_Options(aSource).Connections;
    Channels := TsgcWSOptimization_Options(aSource).Channels;
    ConnectionsFree := TsgcWSOptimization_Options(aSource).ConnectionsFree;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSOptimization_Options.SetConnections
  (const Value: TsgcWSConnections_Options);
begin
  FConnections.Assign(Value);
end;

procedure TsgcWSOptimization_Options.SetChannels(const Value
  : TsgcWSChannels_Options);
begin
  FChannels.Assign(Value);
end;

procedure TsgcWSOptimization_Options.SetConnectionsFree
  (const Value: TsgcWSConnectionsFree_Options);
begin
  FConnectionsFree.Assign(Value);
end;

procedure TsgcWSChannels_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSChannels_Options then
  begin
    Enabled := TsgcWSChannels_Options(aSource).Enabled;
  end
  else
    inherited Assign(aSource);
end;

function TsgcWSChannels_Options.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

constructor TsgcQueueItemChannel.Create;
begin
  inherited;
  FConnection := nil;
end;

destructor TsgcQueueItemChannel.Destroy;
begin
  FConnection := nil;
  inherited;
end;

procedure TsgcQueueItemChannel.SetConnection(const Value: TsgcWSConnection);
begin
  FConnection := Value;
  if Assigned(FConnection) then
    ID := FConnection.Guid;
end;

procedure TsgcQueueListChannels.GetChannels(const aID: String;
var aList: TList{$IFDEF NEXTGEN}<TsgcQueueItemChannel>{$ENDIF});
var
  i: Integer;
  oItem: TsgcQueueItemChannel;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  oList := LockList;
  Try
    for i := oList.Count - 1 Downto 0 do
    begin
      oItem := TsgcQueueItemChannel(TsgcQueueNameBase(oList[i]).GetItem(aID));
      if Assigned(oItem) then
        aList.Add(oItem);
    end;
  Finally
    UnLockList;
  End;
end;

procedure TsgcQueueListChannels.GetConnections(const aChannel: String;
var aList: TList{$IFDEF NEXTGEN}<TsgcWSConnection>{$ENDIF});
var
  i: Integer;
  oQueue: TsgcQueueNameBase;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  oQueue := GetQueueByName(aChannel);
  if Assigned(oQueue) then
  begin
    oList := oQueue.LockList;
    Try
      for i := oList.Count - 1 Downto 0 do
        aList.Add(TsgcQueueItemChannel(oList[i]).Connection);
    Finally
      oQueue.UnLockList;
    End;
  end;
end;

constructor TsgcWSConnectionsFree_Options.Create;
begin
  inherited;
  FEnabled := True;
  FInterval := 60;
end;

procedure TsgcWSConnectionsFree_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSConnectionsFree_Options then
  begin
    Enabled := TsgcWSConnectionsFree_Options(aSource).Enabled;
    Interval := TsgcWSConnectionsFree_Options(aSource).Interval;
  end
  else
    inherited Assign(aSource);
end;

function TsgcWSConnectionsFree_Options.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

constructor TsgcQueueItemProtocolMessage.Create;
begin
  inherited;
  FWSMessage := nil;
end;

destructor TsgcQueueItemProtocolMessage.Destroy;
begin
  sgcFree(FWSMessage);
  inherited;
end;

constructor TsgcWSAuthentication_Token.Create;
begin
  inherited;
  FAuthName := CS_AUTH_NAME_DEFAULT;
end;

procedure TsgcWSAuthentication_Token.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSAuthentication_Token then
  begin
    FEnabled := TsgcWSAuthentication_Token(aSource).Enabled;
    FAuthName := TsgcWSAuthentication_Token(aSource).AuthName;
    FAuthToken := TsgcWSAuthentication_Token(aSource).AuthToken;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcWSWatchDogServer_Options.Create;
begin
  inherited;
  FMonitor := TsgcWSWatchDogMonitorServer_Options.Create;
end;

destructor TsgcWSWatchDogServer_Options.Destroy;
begin
  sgcFree(FMonitor);
  inherited;
end;

procedure TsgcWSWatchDogServer_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSWatchDogServer_Options then
  begin
    inherited Assign(aSource);
    Monitor.Assign(TsgcWSWatchDogServer_Options(aSource).Monitor);
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSWatchDogServer_Options.SetMonitor
  (const Value: TsgcWSWatchDogMonitorServer_Options);
begin
  FMonitor := Value;
end;

constructor TsgcWSWatchDogMonitorServer_Options.Create;
begin
  inherited;
  FEnabled := False;
  FTimeout := 10;
end;

procedure TsgcWSWatchDogMonitorServer_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSWatchDogMonitorServer_Options then
  begin
    Enabled := TsgcWSWatchDogMonitorServer_Options(aSource).Enabled;
    Timeout := TsgcWSWatchDogMonitorServer_Options(aSource).Timeout;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcWSSSL_Options.Create;
begin
  inherited;
  FOpenSSL_Options := TsgcOpenSSLServer_Options.Create;
end;

destructor TsgcWSSSL_Options.Destroy;
begin
  sgcFree(FOpenSSL_Options);
  inherited;
end;

procedure TsgcWSSSL_Options.Assign(aSource: TPersistent);
begin
  inherited;
  if aSource is TsgcWSSSL_Options then
  begin
    Port := TsgcWSSSL_Options(aSource).Port;
    OpenSSL_Options := TsgcWSSSL_Options(aSource).OpenSSL_Options;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSSSL_Options.SetOpenSSL_Options(const Value
  : TsgcOpenSSLServer_Options);
begin
  FOpenSSL_Options.Assign(Value);
end;

constructor TsgcOpenSSLServer_Options.Create;
begin
  inherited;
  ECDHE := False;
end;

procedure TsgcOpenSSLServer_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcOpenSSLServer_Options then
  begin
    ECDHE := TsgcOpenSSLServer_Options(aSource).ECDHE;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcWSLogFile.Create;
begin
  inherited;
  FUnMaskFrames := True;
end;

procedure TsgcWSLogFile.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSLogFile then
  begin
    UnMaskFrames := TsgcWSLogFile(aSource).UnMaskFrames;
  end;
  inherited Assign(aSource);
end;

constructor TsgcWSAPI_server.Create(aOwner: TComponent);
begin
  inherited;
  // TODO -cMM: TsgcWSAPI_server.Create default body inserted
end;

destructor TsgcWSAPI_server.Destroy;
begin
  sgcFree(FResources);
  inherited;
end;

procedure TsgcWSAPI_server.AddResource(const aDocument, aResource: string);
begin
  if Resources.CommaText = '' then
    Resources.CommaText := aDocument + '=' + aResource
  else
    Resources.CommaText := Resources.CommaText + ',' + aDocument + '=' + aResource;
end;

procedure TsgcWSAPI_server.ClearResources;
begin
  Resources.CommaText := '';
end;

function TsgcWSAPI_server.DoHTTPRequest(const aRequest: TsgcHTTPRequest; const
    aResponse: TsgcHTTPResponse): Boolean;
begin
  aResponse.Content := GetResourceString(GetResource(aRequest.Document));
  if aResponse.Content <> '' then
  begin
    aResponse.Code := 200;
    if UpperCase(RightStr(aRequest.Document, 3)) = '.JS' then
      aResponse.ContentType := 'application/javascript'
    else if UpperCase(RightStr(aRequest.Document, 4)) = '.CSS' then
      aResponse.ContentType := 'text/css'
    else
      aResponse.ContentType := 'text/html';

    DoHTTPBeforeResourceResponse(aRequest, aResponse);

    result := True;

    exit;
  end;

  result := DoHTTPRequestAPI(aRequest, aResponse);
end;

function TsgcWSAPI_server.GetResource(const aDocument: string): string;
begin
  Result := Resources.Values[aDocument];
end;

function TsgcWSAPI_server.GetResources: TStringList;
begin
  if not Assigned(FResources) then
    FResources := TStringList.Create;
  Result := FResources;
end;

procedure TsgcWSAPI_server.Loaded;
begin
  inherited;
  if Assigned(Server) then
    Server.RegisterAPI(Self);
end;

procedure TsgcWSAPI_server.Notification(AComponent: TComponent;
Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FServer) then
    FServer := nil;
end;

procedure TsgcWSAPI_server.SetServer(const Value: TsgcWSComponent_Server);
begin
  if Assigned(FServer) then
    FServer.RemoveFreeNotification(Self);

  if [csDesigning, csLoading] * ComponentState = [] then
  begin
    if Assigned(FServer) then
    begin
      if not Assigned(Value) then
      begin
        FServer.UnRegisterAPI(Self);
        FServer := Value;
      end
      else if FServer <> Value then
      begin
        FServer := Value;
        FServer.RegisterAPI(Self);
      end;
    end
    else
    begin
      FServer := Value;
      if Assigned(Value) then
        FServer.RegisterAPI(Self);
    end;
  end
  else
    FServer := Value;

  if Assigned(FServer) then
    FServer.FreeNotification(Self);
end;

initialization

// -->start sgc_trial
{$IFDEF SGC_TRIAL}
if ((Now > EncodeDate(2022, 4, 25)) and (FormatDateTime('s', Now) = '9')) then
begin
  raise Exception.Create
    (DecodeBase64
    ('VGhpcyBkZW1vIHZlcnNpb24gY2FuIG9ubHkgcnVuIGZvciBhIGxpbWl0ZWQgdGltZSBwZXJpb2QuIFBsZWFzZSB2aXNpdCB3d3cuZXNlZ2VjZS5jb20gdG8gcHVyY2hhc2UgeW91ciBjb3B5IG9mIHRoZSBsaWJyYXJ5Lg==')
    );
end;
{$ENDIF}
// <--end sgc_trial

finalization

end.
