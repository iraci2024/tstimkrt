{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }
unit sgcWebSocket;

interface

{$I sgcVer.inc}

uses
  Classes, sgcWebSocket_Client
{$IFDEF SGC_WINHTTP}, sgcWebSocket_Client_WinHTTP{$ENDIF}
{$IFDEF SGC_EDT_PRO}
    , sgcWebSocket_Server, sgcWebSocket_Server_Proxy,
  sgcWebSocket_LoadBalancer_Server
{$IFDEF SGC_EDT_ENT}
{$IFDEF SGC_HTTPAPI}, sgcWebSocket_Server_HTTPAPI{$ENDIF}
{$ENDIF}
{$IFDEF SGC_QUIC}, sgcQUIC_Client{$ENDIF}
{$ENDIF}
    ;

type
  TsgcWebSocketClient = class(TsgcWSClient)
  published
    property Active;
    property Host;
    property Port;
    property URL;
    property ConnectTimeout;
    property ReadTimeout;
    property WriteTimeout;
  published
    property TLS;
    property Proxy;
    property HeartBeat;
    property IPVersion;
  published
    property OnConnect;
    property OnMessage;
    property OnBinary;
    property OnFragmented;
    property OnDisconnect;
    property OnError;
    property OnHandshake;
    property OnException;
    property OnSSLGetHandler;
    property OnSSLAfterCreateHandler;
    property OnSSLVerifyPeer;
    property OnLoadBalancerError;
    property OnBeforeHeartBeat;
    property OnBeforeWatchDog;
    property OnBeforeConnect;
  published
    property Authentication;
    property Extensions;
    property Options;
    property Specifications;
    property NotifyEvents;
    property LogFile;
    property QueueOptions;
    property WatchDog;
    property Throttle;
    property LoadBalancer;
    property TLSOptions;
    property Version;
  end;

{$IFDEF SGC_WINHTTP}

  TsgcWebSocketClient_WinHTTP = class(TsgcWSClient_WinHTTP)
  published
    property Active;
    property Host;
    property Port;
    property URL;
  published
    property ConnectTimeout;
    property ReadTimeout;
    property HeartBeat;
    property TLS;
  published
    property OnConnect;
    property OnMessage;
    property OnBinary;
    property OnFragmented;
    property OnDisconnect;
    property OnError;
    // property OnHandshake;
    property OnException;
    property OnBeforeWatchDog;
    property OnBeforeConnect;
  published
    property Authentication;
    property Asynchronous;
    property Proxy;
    property Options;
    property NotifyEvents;
    property WatchDog;
    property Version;
  end;
{$ENDIF}
{$IFDEF SGC_EDT_PRO}

  TsgcWebSocketServer = class(TsgcWSServer)
  published
    property Active;
    property Port;
  published
    property OnStartup;
    property OnShutdown;
    property OnTCPConnect;
    property OnConnect;
    property OnMessage;
    property OnBinary;
    property OnFragmented;
    property OnDisconnect;
    property OnError;
    property OnHandshake;
    property OnException;
    property OnSSLGetHandler;
    property OnSSLAfterCreateHandler;
    property OnSSLALPNSelect;
    property OnUnknownProtocol;
    property OnBeforeHeartBeat;
    property LoadBalancer;
    property OnLoadBalancerConnect;
    property OnLoadBalancerDisconnect;
    property OnLoadBalancerError;
    property Authentication;
    property OnAuthentication;
    property OnUnknownAuthentication;
  published
    property Bindings;
    property HeartBeat;
    property MaxConnections;
    property SSL;
    property SSLOptions;
    property ThreadPool;
    property ThreadPoolOptions;
    property Extensions;
    property FallBack;
    property Options;
    property QueueOptions;
    property SecurityOptions;
    property Specifications;
    property NotifyEvents;
    property LogFile;
    property Throttle;
    property WatchDog;
    property IOHandlerOptions;
    property HTTP2Options;
    property Version;
  end;

  TsgcWebSocketHTTPServer = class(TsgcWSHTTPServer)
  published
    property Active;
    property Port;
  published
    property OnStartup;
    property OnShutdown;
    property OnTCPConnect;
    property OnConnect;
    property OnMessage;
    property OnBinary;
    property OnFragmented;
    property OnDisconnect;
    property OnError;
    property OnHandshake;
    property OnException;
    property OnSSLGetHandler;
    property OnSSLAfterCreateHandler;
    property OnSSLALPNSelect;
    property OnUnknownProtocol;
    property OnBeforeHeartBeat;
    property OnBeforeForwardHTTP;
    property OnAfterForwardHTTP;
    property OnCommandGet;
    property OnCommandOther;
    property OnCreateSession;
    property OnInvalidSession;
    property OnSessionStart;
    property OnSessionEnd;
  published
    property LoadBalancer;
    property OnLoadBalancerConnect;
    property OnLoadBalancerDisconnect;
    property OnLoadBalancerError;
  published
    property Authentication;
    property OnAuthentication;
    property OnUnknownAuthentication;
  published
    property OnHTTPUploadBeforeSaveFile;
    property OnHTTPUploadAfterSaveFile;
  published
    property AutoStartSession;
    property Bindings;
    property DocumentRoot;
    property HeartBeat;
    property KeepAlive;
    property MaxConnections;
    property SessionState;
    property SessionTimeOut;
    property SSL;
    property SSLOptions;
    property ThreadPool;
    property ThreadPoolOptions;
    property Extensions;
    property FallBack;
    property Options;
    property QueueOptions;
    property SecurityOptions;
    property Specifications;
    property NotifyEvents;
    property LogFile;
    property Throttle;
    property WatchDog;
    property IOHandlerOptions;
    property HTTP2Options;
    property HTTPUploadFiles;
    property Version;
  end;

  TsgcWebSocketProxyServer = class(TsgcWSProxyServer)
  published
    property Active;
    property Port;
    property Authentication;
    property Bindings;
    property MaxConnections;
    property SSL;
    property SSLOptions;
    property ThreadPool;
    property ThreadPoolOptions;
    property Extensions;
    property Options;
    property SecurityOptions;
    property Specifications;
    property LogFile;
    property Throttle;
    property FallBack;
    property Proxy;
    property Version;
  end;

  TsgcWebSocketLoadBalancerServer = class(TsgcWSLoadBalancerServer)
  published
    property Active;
    property Port;
    property OnConnect;
    property OnMessage;
    property OnBinary;
    property OnFragmented;
    property OnDisconnect;
    property OnError;
    property OnHandshake;
    property OnException;
    property OnSSLGetHandler;
    property OnSSLAfterCreateHandler;
    property OnRawMessage;
    property OnClientConnect;
    property OnClientDisconnect;
    property OnClientMessage;
    property OnClientBinary;
    property OnClientFragmented;
    property OnServerConnect;
    property OnServerDisconnect;
    property OnServerReady;
    property OnBeforeSendServerBinding;
    property Bindings;
    property MaxConnections;
    property SSL;
    property SSLOptions;
    property ThreadPool;
    property ThreadPoolOptions;
    property Extensions;
    property Options;
    property SecurityOptions;
    property Specifications;
    property LogFile;
    property Throttle;
    property FallBack;
    property LoadBalancer;
    property Version;
  end;

{$IFDEF SGC_EDT_ENT}
{$IFDEF SGC_HTTPAPI}

  TsgcWebSocketServer_HTTPAPI = class(TsgcWSServer_HTTPAPI)
  published
    property Active;
    property Host;
    property Port;
  published
    property OnStartup;
    property OnShutdown;
    property OnBeforeBinding;
    property OnConnect;
    property OnMessage;
    property OnBinary;
    property OnFragmented;
    property OnDisconnect;
    property OnError;
    property OnHandshake;
    property OnException;
  published
    property Authentication;
    property OnAuthentication;
  published
    property OnUnknownProtocol;
    property OnAsynchronous;
    property OnBeforeHeartBeat;
    property OnBeforeForwardHTTP;
    property OnAfterForwardHTTP;
    property OnHTTPRequest;
  published
    property OnHTTPUploadBeforeSaveFile;
    property OnHTTPUploadAfterSaveFile;
  published
    property HeartBeat;
    property Extensions;
    property Options;
    property QueueOptions;
    property SecurityOptions;
    property Specifications;
    property LogFile;
    property SSL;
    property SSLOptions;
    property WatchDog;
    property Timeouts;
    property Asynchronous;
    property BindingOptions;
    property MaxConnections;
    property MaxBandwidth;
    property ThreadPoolSize;
    property ReadBufferSize;
    property HTTPUploadFiles;
  published
    property Version;
  end;
{$ENDIF}
{$ENDIF}
{$IFDEF SGC_QUIC}

  TsgcQUICClient = class(TsgcQUICCLient_Base)
  published
    property Host;
    property Port;
    property IPVersion;
    property LogFile;
    property Proxy;
    property QUICOptions;
    property OnUDPRead;
    property OnUDPException;
    property Version;
  end;
{$ENDIF}
{$ENDIF}

implementation

end.
