// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket.pas' rev: 35.00 (Windows)

#ifndef SgcwebsocketHPP
#define SgcwebsocketHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <sgcWebSocket_Client.hpp>
#include <sgcWebSocket_Client_WinHTTP.hpp>
#include <sgcWebSocket_Server.hpp>
#include <sgcWebSocket_Server_Proxy.hpp>
#include <sgcWebSocket_LoadBalancer_Server.hpp>
#include <sgcWebSocket_Classes_Indy.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>
#include <IdGlobal.hpp>
#include <sgcWebSocket_Extensions.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Server_Base.hpp>
#include <IdSocketHandle.hpp>
#include <IdCustomHTTPServer.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWebSocketClient;
class DELPHICLASS TsgcWebSocketClient_WinHTTP;
class DELPHICLASS TsgcWebSocketServer;
class DELPHICLASS TsgcWebSocketHTTPServer;
class DELPHICLASS TsgcWebSocketProxyServer;
class DELPHICLASS TsgcWebSocketLoadBalancerServer;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcWebSocketClient : public Sgcwebsocket_client::TsgcWSClient
{
	typedef Sgcwebsocket_client::TsgcWSClient inherited;
	
__published:
	__property Active = {default=0};
	__property Host = {default=0};
	__property Port;
	__property URL = {default=0};
	__property ConnectTimeout;
	__property ReadTimeout;
	__property WriteTimeout;
	__property TLS;
	__property Proxy;
	__property HeartBeat;
	__property IPVersion;
	__property OnConnect;
	__property OnMessage;
	__property OnBinary;
	__property OnFragmented;
	__property OnDisconnect;
	__property OnError;
	__property OnHandshake;
	__property OnException;
	__property OnSSLGetHandler;
	__property OnSSLAfterCreateHandler;
	__property OnSSLVerifyPeer;
	__property OnLoadBalancerError;
	__property OnBeforeHeartBeat;
	__property OnBeforeWatchDog;
	__property OnBeforeConnect;
	__property Authentication;
	__property Extensions;
	__property Options;
	__property Specifications;
	__property NotifyEvents;
	__property LogFile;
	__property QueueOptions;
	__property WatchDog;
	__property Throttle;
	__property LoadBalancer;
	__property TLSOptions;
	__property Version = {default=0};
public:
	/* TsgcWSClient.Create */ inline __fastcall virtual TsgcWebSocketClient(System::Classes::TComponent* aOwner) : Sgcwebsocket_client::TsgcWSClient(aOwner) { }
	/* TsgcWSClient.Destroy */ inline __fastcall virtual ~TsgcWebSocketClient() { }
	
};


class PASCALIMPLEMENTATION TsgcWebSocketClient_WinHTTP : public Sgcwebsocket_client_winhttp::TsgcWSClient_WinHTTP
{
	typedef Sgcwebsocket_client_winhttp::TsgcWSClient_WinHTTP inherited;
	
__published:
	__property Active = {default=0};
	__property Host = {default=0};
	__property Port;
	__property URL = {default=0};
	__property ConnectTimeout;
	__property ReadTimeout;
	__property HeartBeat;
	__property TLS;
	__property OnConnect;
	__property OnMessage;
	__property OnBinary;
	__property OnFragmented;
	__property OnDisconnect;
	__property OnError;
	__property OnException;
	__property OnBeforeWatchDog;
	__property OnBeforeConnect;
	__property Authentication;
	__property Asynchronous;
	__property Proxy;
	__property Options;
	__property NotifyEvents;
	__property WatchDog;
	__property Version = {default=0};
public:
	/* TsgcWSClient_WinHTTP.Create */ inline __fastcall virtual TsgcWebSocketClient_WinHTTP(System::Classes::TComponent* aOwner) : Sgcwebsocket_client_winhttp::TsgcWSClient_WinHTTP(aOwner) { }
	/* TsgcWSClient_WinHTTP.Destroy */ inline __fastcall virtual ~TsgcWebSocketClient_WinHTTP() { }
	
};


class PASCALIMPLEMENTATION TsgcWebSocketServer : public Sgcwebsocket_server::TsgcWSServer
{
	typedef Sgcwebsocket_server::TsgcWSServer inherited;
	
__published:
	__property Active = {default=0};
	__property Port;
	__property OnStartup;
	__property OnShutdown;
	__property OnTCPConnect;
	__property OnConnect;
	__property OnMessage;
	__property OnBinary;
	__property OnFragmented;
	__property OnDisconnect;
	__property OnError;
	__property OnHandshake;
	__property OnException;
	__property OnSSLGetHandler;
	__property OnSSLAfterCreateHandler;
	__property OnSSLALPNSelect;
	__property OnUnknownProtocol;
	__property OnBeforeHeartBeat;
	__property LoadBalancer;
	__property OnLoadBalancerConnect;
	__property OnLoadBalancerDisconnect;
	__property OnLoadBalancerError;
	__property Authentication;
	__property OnAuthentication;
	__property OnUnknownAuthentication;
	__property Bindings;
	__property HeartBeat;
	__property MaxConnections;
	__property SSL = {default=0};
	__property SSLOptions;
	__property ThreadPool;
	__property ThreadPoolOptions;
	__property Extensions;
	__property FallBack;
	__property Options;
	__property QueueOptions;
	__property SecurityOptions;
	__property Specifications;
	__property NotifyEvents;
	__property LogFile;
	__property Throttle;
	__property WatchDog;
	__property IOHandlerOptions;
	__property HTTP2Options;
	__property Version = {default=0};
public:
	/* TsgcWSServer.Create */ inline __fastcall virtual TsgcWebSocketServer(System::Classes::TComponent* aOwner) : Sgcwebsocket_server::TsgcWSServer(aOwner) { }
	/* TsgcWSServer.Destroy */ inline __fastcall virtual ~TsgcWebSocketServer() { }
	
};


class PASCALIMPLEMENTATION TsgcWebSocketHTTPServer : public Sgcwebsocket_server::TsgcWSHTTPServer
{
	typedef Sgcwebsocket_server::TsgcWSHTTPServer inherited;
	
__published:
	__property Active = {default=0};
	__property Port;
	__property OnStartup;
	__property OnShutdown;
	__property OnTCPConnect;
	__property OnConnect;
	__property OnMessage;
	__property OnBinary;
	__property OnFragmented;
	__property OnDisconnect;
	__property OnError;
	__property OnHandshake;
	__property OnException;
	__property OnSSLGetHandler;
	__property OnSSLAfterCreateHandler;
	__property OnSSLALPNSelect;
	__property OnUnknownProtocol;
	__property OnBeforeHeartBeat;
	__property OnBeforeForwardHTTP;
	__property OnAfterForwardHTTP;
	__property OnCommandGet;
	__property OnCommandOther;
	__property OnCreateSession;
	__property OnInvalidSession;
	__property OnSessionStart;
	__property OnSessionEnd;
	__property LoadBalancer;
	__property OnLoadBalancerConnect;
	__property OnLoadBalancerDisconnect;
	__property OnLoadBalancerError;
	__property Authentication;
	__property OnAuthentication;
	__property OnUnknownAuthentication;
	__property OnHTTPUploadBeforeSaveFile;
	__property OnHTTPUploadAfterSaveFile;
	__property AutoStartSession;
	__property Bindings;
	__property DocumentRoot = {default=0};
	__property HeartBeat;
	__property KeepAlive;
	__property MaxConnections;
	__property SessionState;
	__property SessionTimeOut;
	__property SSL = {default=0};
	__property SSLOptions;
	__property ThreadPool;
	__property ThreadPoolOptions;
	__property Extensions;
	__property FallBack;
	__property Options;
	__property QueueOptions;
	__property SecurityOptions;
	__property Specifications;
	__property NotifyEvents;
	__property LogFile;
	__property Throttle;
	__property WatchDog;
	__property IOHandlerOptions;
	__property HTTP2Options;
	__property HTTPUploadFiles;
	__property Version = {default=0};
public:
	/* TsgcWSHTTPServer.Create */ inline __fastcall virtual TsgcWebSocketHTTPServer(System::Classes::TComponent* aOwner) : Sgcwebsocket_server::TsgcWSHTTPServer(aOwner) { }
	/* TsgcWSHTTPServer.Destroy */ inline __fastcall virtual ~TsgcWebSocketHTTPServer() { }
	
};


class PASCALIMPLEMENTATION TsgcWebSocketProxyServer : public Sgcwebsocket_server_proxy::TsgcWSProxyServer
{
	typedef Sgcwebsocket_server_proxy::TsgcWSProxyServer inherited;
	
__published:
	__property Active = {default=0};
	__property Port;
	__property Authentication;
	__property Bindings;
	__property MaxConnections;
	__property SSL = {default=0};
	__property SSLOptions;
	__property ThreadPool;
	__property ThreadPoolOptions;
	__property Extensions;
	__property Options;
	__property SecurityOptions;
	__property Specifications;
	__property LogFile;
	__property Throttle;
	__property FallBack;
	__property Proxy;
	__property Version = {default=0};
public:
	/* TsgcWSProxyServer.Create */ inline __fastcall virtual TsgcWebSocketProxyServer(System::Classes::TComponent* aOwner) : Sgcwebsocket_server_proxy::TsgcWSProxyServer(aOwner) { }
	/* TsgcWSProxyServer.Destroy */ inline __fastcall virtual ~TsgcWebSocketProxyServer() { }
	
};


class PASCALIMPLEMENTATION TsgcWebSocketLoadBalancerServer : public Sgcwebsocket_loadbalancer_server::TsgcWSLoadBalancerServer
{
	typedef Sgcwebsocket_loadbalancer_server::TsgcWSLoadBalancerServer inherited;
	
__published:
	__property Active = {default=0};
	__property Port;
	__property OnConnect;
	__property OnMessage;
	__property OnBinary;
	__property OnFragmented;
	__property OnDisconnect;
	__property OnError;
	__property OnHandshake;
	__property OnException;
	__property OnSSLGetHandler;
	__property OnSSLAfterCreateHandler;
	__property OnRawMessage;
	__property OnClientConnect;
	__property OnClientDisconnect;
	__property OnClientMessage;
	__property OnClientBinary;
	__property OnClientFragmented;
	__property OnServerConnect;
	__property OnServerDisconnect;
	__property OnServerReady;
	__property OnBeforeSendServerBinding;
	__property Bindings;
	__property MaxConnections;
	__property SSL = {default=0};
	__property SSLOptions;
	__property ThreadPool;
	__property ThreadPoolOptions;
	__property Extensions;
	__property Options;
	__property SecurityOptions;
	__property Specifications;
	__property LogFile;
	__property Throttle;
	__property FallBack;
	__property LoadBalancer;
	__property Version = {default=0};
public:
	/* TsgcWSLoadBalancerServer.Create */ inline __fastcall virtual TsgcWebSocketLoadBalancerServer(System::Classes::TComponent* aOwner) : Sgcwebsocket_loadbalancer_server::TsgcWSLoadBalancerServer(aOwner) { }
	/* TsgcWSLoadBalancerServer.Destroy */ inline __fastcall virtual ~TsgcWebSocketLoadBalancerServer() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET)
using namespace Sgcwebsocket;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SgcwebsocketHPP
