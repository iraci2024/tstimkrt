// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Server.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_serverHPP
#define Sgcwebsocket_serverHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.StrUtils.hpp>
#include <System.Contnrs.hpp>
#include <Winapi.Windows.hpp>
#include <IdContext.hpp>
#include <IdTCPConnection.hpp>
#include <IdTCPServer.hpp>
#include <IdGlobal.hpp>
#include <IdHeaderList.hpp>
#include <IdException.hpp>
#include <IdCustomHTTPServer.hpp>
#include <IdSocketHandle.hpp>
#include <IdHTTPServer.hpp>
#include <IdCustomTCPServer.hpp>
#include <IdSchedulerOfThreadPool.hpp>
#include <IdGlobalProtocols.hpp>
#include <IdServerIOHandler.hpp>
#include <IdScheduler.hpp>
#include <IdSSL.hpp>
#include <IdSSLOpenSSL.hpp>
#include <IdInterceptThrottler.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Classes_Indy.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_CustomServer.hpp>
#include <sgcWebSocket_LoadBalancer_Client.hpp>
#include <sgcWebSocket_Helpers.hpp>
#include <sgcWebSocket_Server_Base.hpp>
#include <sgcTCP_Classes.hpp>
#include <sgcBase_Helpers.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_server
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSHandShakeServer;
class DELPHICLASS TsgcWSConnectionServer;
class DELPHICLASS TsgcWSThreadPool_Options;
class DELPHICLASS TsgcWSIOHandler_IOCP_Options;
class DELPHICLASS TsgcWSIOHandler_Options;
class DELPHICLASS TsgcWSLoadBalancerServer_Options;
class DELPHICLASS TServerThread;
class DELPHICLASS TsgcWSServer_Indy_Base;
class DELPHICLASS TsgcWSServer;
class DELPHICLASS TsgcWSHTTPServer;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSOnSSLGetHandler)(System::TObject* Sender, Sgcwebsocket_types::TwsSSLHandler aType, Idssl::TIdServerIOHandlerSSLBase* &aSSLHandler);

typedef void __fastcall (__closure *TsgcWSOnSSLAfterCreateHandler)(System::TObject* Sender, Sgcwebsocket_types::TwsSSLHandler aType, Idssl::TIdServerIOHandlerSSLBase* aSSLHandler);

typedef void __fastcall (__closure *TsgcWSOnSSLALPNSelect)(System::TObject* Sender, System::Classes::TStringList* aProtocols, System::UnicodeString &aProtocol);

typedef void __fastcall (__closure *TsgcWSUnknownAuthenticationEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString AuthType, System::UnicodeString AuthData, System::UnicodeString &aUser, System::UnicodeString &aPassword, bool &Authenticated);

typedef void __fastcall (__closure *TsgcWSOnTCPConnect)(Sgcwebsocket_classes::TsgcWSConnection* Connection, bool &Accept);

typedef void __fastcall (__closure *TsgcWSOnBeforeForwardHTTP)(Sgcwebsocket_classes::TsgcWSConnection* Connection, Idcustomhttpserver::TIdHTTPRequestInfo* ARequestInfo, Sgcwebsocket_classes_indy::TsgcWSServerForwardHTTP* aForward);

typedef void __fastcall (__closure *TsgcWSOnAfterForwardHTTP)(Sgcwebsocket_classes::TsgcWSConnection* Connection, Idcustomhttpserver::TIdHTTPRequestInfo* ARequestInfo, Idcustomhttpserver::TIdHTTPResponseInfo* AResponseInfo, System::Sysutils::Exception* E);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSHandShakeServer : public Sgcwebsocket_server_base::TsgcWSHandShakeServer_Base
{
	typedef Sgcwebsocket_server_base::TsgcWSHandShakeServer_Base inherited;
	
private:
	HIDESBASE void __fastcall DoDecodeHeaders(Idtcpconnection::TIdTCPConnection* const aConnection, Idheaderlist::TIdHeaderList* aHeaders);
	
public:
	__fastcall virtual TsgcWSHandShakeServer(Idtcpconnection::TIdTCPConnection* aConnection, Idheaderlist::TIdHeaderList* aHeaders)/* overload */;
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSHandShakeServer() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSConnectionServer : public Sgcwebsocket_classes_indy::TsgcWSConnection_Indy
{
	typedef Sgcwebsocket_classes_indy::TsgcWSConnection_Indy inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetURL();
	virtual void __fastcall SetTransport(const Sgcwebsocket_types::TwsTransport Value);
	
private:
	int FReadCount;
	int FReadStartSSLCount;
	System::UnicodeString FServerOriginsAllowed;
	bool FServerSSL;
	bool FCustomHeartBeat;
	
public:
	__property System::UnicodeString ServerOriginsAllowed = {read=FServerOriginsAllowed, write=FServerOriginsAllowed};
	__property bool ServerSSL = {read=FServerSSL, write=FServerSSL, nodefault};
	
private:
	System::UnicodeString __fastcall DoGetProtocol();
	TsgcWSHandShakeServer* FHandshake;
	void __fastcall DoHandShake_RFC6455();
	void __fastcall DoHandShake_Hixie76();
	void __fastcall DoOriginsAllowed();
	
protected:
	virtual void __fastcall DoHandShakeSecurity();
	virtual void __fastcall DoHandShakeExtensions();
	virtual void __fastcall DoHandshake();
	
public:
	__property TsgcWSHandShakeServer* HandShake = {read=FHandshake};
	
protected:
	virtual void __fastcall DoProtocolSSE();
	
private:
	Sgcwebsocket_server_base::TsgcWSConnectionAuthentication* FAuthentication;
	Sgcwebsocket_server_base::TsgcWSAuthenticationEvent FOnAuthentication;
	
protected:
	Sgcwebsocket_server_base::TsgcWSConnectionAuthentication* __fastcall GetAuthentication();
	virtual void __fastcall DoOnAuthenticationEvent();
	int FAuthenticationCloseCode;
	
public:
	void __fastcall SetAuthenticationCloseCode(const int aCloseCode);
	__property Sgcwebsocket_server_base::TsgcWSConnectionAuthentication* Authentication = {read=GetAuthentication, write=FAuthentication};
	__property Sgcwebsocket_server_base::TsgcWSAuthenticationEvent OnAuthentication = {read=FOnAuthentication, write=FOnAuthentication};
	
private:
	Idglobal::TIdIPVersion FIPVersion;
	bool __fastcall DoHTTPFileResponse(const System::UnicodeString aFileName, const System::UnicodeString aContentType, bool aDisconnect, int aStreamIdentifier = 0x0);
	void __fastcall DoAuthSessionResponse(const System::UnicodeString aParams);
	
protected:
	bool __fastcall DoAuthenticationURL(const System::UnicodeString aParams);
	bool __fastcall DoAuthenticationBasic(const System::UnicodeString aParams);
	
public:
	void __fastcall SendResponseHTTP(const System::UnicodeString aContent, const System::UnicodeString aContentType);
	
protected:
	__property Idglobal::TIdIPVersion IPVersion = {read=FIPVersion, write=FIPVersion, nodefault};
	
public:
	__fastcall virtual TsgcWSConnectionServer();
	__fastcall virtual ~TsgcWSConnectionServer();
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSThreadPool_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	int FMaxThreads;
	int FPoolSize;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property int MaxThreads = {read=FMaxThreads, write=FMaxThreads, nodefault};
	__property int PoolSize = {read=FPoolSize, write=FPoolSize, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSThreadPool_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSThreadPool_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSIOHandler_IOCP_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	int FIOCPThreads;
	int FTimeOut;
	int FWorkOpThreads;
	
public:
	__fastcall virtual TsgcWSIOHandler_IOCP_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property int IOCPThreads = {read=FIOCPThreads, write=FIOCPThreads, nodefault};
	__property int TimeOut = {read=FTimeOut, write=FTimeOut, nodefault};
	__property int WorkOpThreads = {read=FWorkOpThreads, write=FWorkOpThreads, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSIOHandler_IOCP_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSIOHandler_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TsgcWSIOHandler_IOCP_Options* FIOCP;
	Sgcwebsocket_types::TwsIOHandler FIOHandlerType;
	void __fastcall SetIOCP(TsgcWSIOHandler_IOCP_Options* const Value);
	
public:
	__fastcall virtual TsgcWSIOHandler_Options();
	__fastcall virtual ~TsgcWSIOHandler_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property TsgcWSIOHandler_IOCP_Options* IOCP = {read=FIOCP, write=SetIOCP};
	__property Sgcwebsocket_types::TwsIOHandler IOHandlerType = {read=FIOHandlerType, write=FIOHandlerType, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSLoadBalancerServer_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FAutoRegisterBindings;
	int FAutoRestart;
	bool FEnabled;
	System::UnicodeString FHost;
	int FPort;
	System::Classes::TStringList* FBindings;
	System::UnicodeString FGuid;
	void __fastcall SetBindings(System::Classes::TStringList* const Value);
	
public:
	__fastcall virtual TsgcWSLoadBalancerServer_Options();
	__fastcall virtual ~TsgcWSLoadBalancerServer_Options();
	
__published:
	__property bool AutoRegisterBindings = {read=FAutoRegisterBindings, write=FAutoRegisterBindings, nodefault};
	__property int AutoRestart = {read=FAutoRestart, write=FAutoRestart, nodefault};
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property System::UnicodeString Host = {read=FHost, write=FHost};
	__property int Port = {read=FPort, write=FPort, nodefault};
	__property System::Classes::TStringList* Bindings = {read=FBindings, write=SetBindings};
	__property System::UnicodeString Guid = {read=FGuid, write=FGuid};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TServerThread : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
private:
	TsgcWSServer_Indy_Base* FServer;
	System::UnicodeString FMethod;
	
protected:
	virtual void __fastcall Execute();
	
public:
	__fastcall TServerThread(const System::UnicodeString aMethod, TsgcWSServer_Indy_Base* const aServer);
	__classmethod void __fastcall Start(TsgcWSServer_Indy_Base* const aServer);
	__classmethod void __fastcall Stop(TsgcWSServer_Indy_Base* const aServer);
	__classmethod void __fastcall ReStart(TsgcWSServer_Indy_Base* const aServer);
public:
	/* TThread.Destroy */ inline __fastcall virtual ~TServerThread() { }
	
};


class PASCALIMPLEMENTATION TsgcWSServer_Indy_Base : public Sgcwebsocket_classes::TsgcWSComponent_Server
{
	typedef Sgcwebsocket_classes::TsgcWSComponent_Server inherited;
	
public:
	TsgcWSConnectionServer* operator[](int Index) { return this->Connections[Index]; }
	
protected:
	virtual void __fastcall Loaded();
	virtual void __fastcall DoNotifyConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoNotifyDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	
public:
	virtual System::Classes::TList* __fastcall LockList();
	virtual void __fastcall UnLockList();
	
protected:
	virtual Idcustomtcpserver::TIdCustomTCPServer* __fastcall GetCustomServer() = 0 ;
	__property Idcustomtcpserver::TIdCustomTCPServer* CustomServer = {read=GetCustomServer};
	
private:
	Sgcwebsocket_classes_indy::TsgcWSThrottle* FThrottle;
	
protected:
	virtual Sgcwebsocket_classes_indy::TsgcWSThrottle* __fastcall GetThrottle();
	virtual void __fastcall SetThrottle(Sgcwebsocket_classes_indy::TsgcWSThrottle* const Value);
	
public:
	__property Sgcwebsocket_classes_indy::TsgcWSThrottle* Throttle = {read=GetThrottle, write=SetThrottle};
	
private:
	Sgcwebsocket_classes_indy::TsgcWSIdLogFileServer* FInterceptLogFile;
	Sgcwebsocket_classes_indy::TsgcWSIdLogFileServer* __fastcall GetInterceptLogFile();
	
protected:
	__property Sgcwebsocket_classes_indy::TsgcWSIdLogFileServer* InterceptLogFile = {read=GetInterceptLogFile, write=FInterceptLogFile};
	
private:
	System::UnicodeString FWatchDogMonitorSecret;
	void __fastcall DoStart();
	void __fastcall DoWatchDogMonitor();
	
protected:
	virtual void __fastcall OnWatchDogEvent(System::TObject* Sender);
	virtual void __fastcall OnWatchDogExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	
private:
	TsgcWSOnTCPConnect FOnTCPConnect;
	
protected:
	void __fastcall OnClientUpdateEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int aType);
	void __fastcall OnClientDisconnectedEvent(System::TObject* Sender);
	virtual void __fastcall OnServerConnectEvent(Idcontext::TIdContext* AContext);
	virtual void __fastcall OnServerMessageEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual void __fastcall OnServerBinaryEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::Classes::TMemoryStream* const aStream);
	virtual void __fastcall OnServerFragmentedEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::Classes::TMemoryStream* const aData, const Sgcwebsocket_types::TOpcode aOpCode, const bool aContinuation);
	virtual void __fastcall OnServerDisconnectEvent(Idcontext::TIdContext* AContext);
	virtual void __fastcall OnServerExecuteEvent(Idcontext::TIdContext* AContext);
	void __fastcall OnServerSubscriptionEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, const System::UnicodeString Subscription);
	void __fastcall OnServerUnSubscriptionEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, const System::UnicodeString Subscription);
	virtual void __fastcall OnServerStartupEvent(System::TObject* Sender);
	virtual void __fastcall OnServerShutdownEvent(System::TObject* Sender);
	
public:
	__property TsgcWSOnTCPConnect OnTCPConnect = {read=FOnTCPConnect, write=FOnTCPConnect};
	
private:
	Sgcwebsocket_server_base::TsgcWSOptionsServer* FOptions;
	Sgcwebsocket_types::TwsServerType FServerType;
	Sgcwebsocket_server_base::TsgcWSSecurity_Options* FSecurityOptions;
	Sgcwebsocket_server_base::TsgcWSQueueServer_Options* FQueueOptions;
	bool FUseNagle;
	__int64 FMaxReadBufferSize;
	int __fastcall GetMaxConnections();
	void __fastcall SetMaxConnections(const int Value);
	Idsockethandle::TIdSocketHandles* __fastcall GetBindings();
	int __fastcall GetPort();
	void __fastcall SetBindings(Idsockethandle::TIdSocketHandles* const Value);
	void __fastcall _SetPort(const int Value);
	void __fastcall SetSecurityOptions(Sgcwebsocket_server_base::TsgcWSSecurity_Options* const Value);
	void __fastcall SetOptions(Sgcwebsocket_server_base::TsgcWSOptionsServer* const Value);
	void __fastcall SetQueueOptions(Sgcwebsocket_server_base::TsgcWSQueueServer_Options* const Value);
	
public:
	__property int Port = {read=GetPort, write=_SetPort, nodefault};
	__property Idsockethandle::TIdSocketHandles* Bindings = {read=GetBindings, write=SetBindings};
	__property int MaxConnections = {read=GetMaxConnections, write=SetMaxConnections, nodefault};
	__property Sgcwebsocket_server_base::TsgcWSOptionsServer* Options = {read=FOptions, write=SetOptions};
	__property Sgcwebsocket_server_base::TsgcWSSecurity_Options* SecurityOptions = {read=FSecurityOptions, write=SetSecurityOptions};
	__property Sgcwebsocket_server_base::TsgcWSQueueServer_Options* QueueOptions = {read=FQueueOptions, write=SetQueueOptions};
	__property bool UseNagle = {read=FUseNagle, write=FUseNagle, stored=true, nodefault};
	__property __int64 MaxReadBufferSize = {read=FMaxReadBufferSize, write=FMaxReadBufferSize};
	
private:
	bool FSSL;
	Sgcwebsocket_classes::TsgcWSSSL_Options* FSSLOptions;
	Idssl::TIdServerIOHandlerSSLBase* FHandlerSSL;
	
protected:
	TsgcWSOnSSLGetHandler FOnSSLGetHandler;
	TsgcWSOnSSLAfterCreateHandler FOnSSLAfterCreateHandler;
	bool __fastcall GetSSL();
	virtual void __fastcall OnGetPasswordSSLEvent(System::UnicodeString &Password);
	virtual void __fastcall OnGetSSLPortEvent(int aPort, bool &aSSL);
	virtual void __fastcall SetSSL(const bool Value);
	virtual void __fastcall SetSSLOptions(Sgcwebsocket_classes::TsgcWSSSL_Options* const Value);
	bool __fastcall IsOpenSSL();
	virtual void __fastcall GetHandlerSSLEvent(const Sgcwebsocket_types::TwsSSLHandler aSSLType, Idssl::TIdServerIOHandlerSSLBase* &aSSLHandler);
	void __fastcall DoOpenSSLECDHE();
	
public:
	__property Sgcwebsocket_classes::TsgcWSSSL_Options* SSLOptions = {read=FSSLOptions, write=SetSSLOptions};
	__property bool SSL = {read=GetSSL, write=SetSSL, default=0};
	__property TsgcWSOnSSLGetHandler OnSSLGetHandler = {read=FOnSSLGetHandler, write=FOnSSLGetHandler};
	__property TsgcWSOnSSLAfterCreateHandler OnSSLAfterCreateHandler = {read=FOnSSLAfterCreateHandler, write=FOnSSLAfterCreateHandler};
	
private:
	TsgcWSOnSSLALPNSelect FOnSSLALPNSelect;
	
protected:
	virtual void __fastcall OnSSLALPNSelectEvent(System::TObject* Sender, System::Classes::TStringList* aProtocols, System::UnicodeString &aProtocol);
	
public:
	__property TsgcWSOnSSLALPNSelect OnSSLALPNSelect = {read=FOnSSLALPNSelect, write=FOnSSLALPNSelect};
	
private:
	Sgcwebsocket_server_base::TsgcHTTPUploadFilesServer* FHTTPUploadFiles;
	void __fastcall SetHTTPUploadFiles(Sgcwebsocket_server_base::TsgcHTTPUploadFilesServer* const Value);
	
protected:
	virtual void __fastcall OnHTTPUploadBeforeSaveFileEvent(System::TObject* Sender, System::UnicodeString &aFileName, System::UnicodeString &aFilePath);
	virtual void __fastcall OnHTTPUploadAfterSaveFileEvent(System::TObject* Sender, const System::UnicodeString aFileName, const System::UnicodeString aFilePath);
	
public:
	__property Sgcwebsocket_server_base::TsgcHTTPUploadFilesServer* HTTPUploadFiles = {read=FHTTPUploadFiles, write=SetHTTPUploadFiles};
	
private:
	Sgcwebsocket_server_base::TsgcWSHTTP2Server_Options* FHTTP2Options;
	void __fastcall SetHTTP2Options(Sgcwebsocket_server_base::TsgcWSHTTP2Server_Options* const Value);
	
protected:
	virtual void __fastcall DoStartHTTP2(TsgcWSConnectionServer* const aConnection);
	virtual void __fastcall OnHTTP2RequestEvent(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, System::Classes::TStringList* const aHeaders, const System::DynamicArray<System::Byte> aBytes, int aStreamIdentifierRequest, int aStreamIdentifierPush, System::Classes::TStream* const aPostStream);
	virtual void __fastcall OnHTTP2ReadCreatePostStreamEvent(System::TObject* Sender, System::Classes::TStrings* const aHeaders, System::Classes::TStream* &Stream);
	virtual void __fastcall OnHTTP2ReadDonePostStreamEvent(System::TObject* Sender, System::Classes::TStrings* const aHeaders, System::Classes::TStream* const aStream);
	__property Sgcwebsocket_server_base::TsgcWSHTTP2Server_Options* HTTP2Options = {read=FHTTP2Options, write=SetHTTP2Options};
	virtual void __fastcall OnOAuth2AuthenticationEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aUser, System::UnicodeString aPassword, bool &Authenticated);
	virtual void __fastcall OnOAuth2ResponseEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aContent, System::UnicodeString aContentType);
	virtual void __fastcall OnOAuth2ResponseErrorEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall OnJWTResponseErrorEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	
private:
	bool FThreadPool;
	TsgcWSThreadPool_Options* FThreadPoolOptions;
	Idschedulerofthreadpool::TIdSchedulerOfThreadPool* FSchedulerThreadPool;
	
protected:
	virtual void __fastcall SetThreadPoolOptions(TsgcWSThreadPool_Options* const Value);
	virtual bool __fastcall GetThreadPool();
	virtual void __fastcall SetThreadPool(const bool Value);
	
public:
	__property TsgcWSThreadPool_Options* ThreadPoolOptions = {read=FThreadPoolOptions, write=SetThreadPoolOptions};
	__property bool ThreadPool = {read=GetThreadPool, write=SetThreadPool, nodefault};
	
private:
	TsgcWSIOHandler_Options* FIOHandlerOptions;
	void __fastcall DoIOHandler();
	void __fastcall DoScheduler();
	
protected:
	virtual void __fastcall SetIOHandlerOptions(TsgcWSIOHandler_Options* const Value);
	
public:
	__property TsgcWSIOHandler_Options* IOHandlerOptions = {read=FIOHandlerOptions, write=SetIOHandlerOptions};
	
private:
	Sgcwebsocket_helpers::TsgcDelimitedStringList* FSessionIDList;
	Sgcwebsocket_server_base::TsgcWSAuthenticationServer_Options* FAuthentication;
	
protected:
	Sgcwebsocket_helpers::TsgcDelimitedStringList* __fastcall GetSessionIDList();
	void __fastcall DoAddSessionID(const System::UnicodeString aSessionID, const System::UnicodeString aUser, const System::UnicodeString aPassword);
	void __fastcall DoDelSessionID(const System::UnicodeString aSessionID);
	bool __fastcall GetSessionID(const System::UnicodeString aSessionID);
	bool __fastcall GetSessionUserPassword(const System::UnicodeString aSessionID, System::UnicodeString &User, System::UnicodeString &Password);
	void __fastcall SetAuthentication(Sgcwebsocket_server_base::TsgcWSAuthenticationServer_Options* const Value);
	bool __fastcall DoAuthenticationSession(TsgcWSConnectionServer* aConnection, const System::UnicodeString aParams);
	bool __fastcall DoUnknownAuthentication(TsgcWSConnectionServer* aConnection);
	
public:
	void __fastcall OnServerAuthenticationEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString aUser, System::UnicodeString aPassword, bool &Authenticated);
	__property Sgcwebsocket_server_base::TsgcWSAuthenticationServer_Options* Authentication = {read=FAuthentication, write=SetAuthentication};
	void __fastcall Start();
	void __fastcall Stop();
	void __fastcall ReStart();
	
protected:
	virtual void __fastcall DoXHR(TsgcWSConnectionServer* const aConnection, System::Classes::TStream* const aStream = (System::Classes::TStream*)(0x0), int aStreamIdentifier = 0x0);
	virtual void __fastcall DoHTTPProtocol(TsgcWSConnectionServer* const aConnection, System::Classes::TStream* const aStream = (System::Classes::TStream*)(0x0), int aStreamIdentifier = 0x0);
	
private:
	Sgcwebsocket_server_base::TsgcWSFallBack_Options* FFallBack;
	
protected:
	void __fastcall SetFallBack(Sgcwebsocket_server_base::TsgcWSFallBack_Options* const Value);
	
public:
	__property Sgcwebsocket_server_base::TsgcWSFallBack_Options* FallBack = {read=FFallBack, write=SetFallBack};
	
private:
	Sgcwebsocket_classes::TsgcWSOnBeforeHeartBeatEvent FOnBeforeHeartBeat;
	
protected:
	virtual void __fastcall OnHeartBeatTimeoutEvent(System::TObject* Sender);
	virtual void __fastcall OnHeartBeatExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	virtual void __fastcall OnHeartBeatTimeoutExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	virtual void __fastcall OnHeartBeatEvent(System::TObject* Sender);
	
public:
	__property Sgcwebsocket_classes::TsgcWSOnBeforeHeartBeatEvent OnBeforeHeartBeat = {read=FOnBeforeHeartBeat, write=FOnBeforeHeartBeat};
	
private:
	TsgcWSLoadBalancerServer_Options* FLoadBalancer;
	Sgcwebsocket_loadbalancer_client::TsgcWSLoadBalancerClient* FClientLoadBalancer;
	Sgcwebsocket_classes::TsgcWSConnectEvent FOnLoadBalancerConnect;
	Sgcwebsocket_classes::TsgcWSDisconnectEvent FOnLoadBalancerDisconnect;
	Sgcwebsocket_classes::TsgcWSLoadBalancerErrorEvent FOnLoadBalancerError;
	
protected:
	void __fastcall SetLoadBalancer(TsgcWSLoadBalancerServer_Options* const Value);
	Sgcwebsocket_loadbalancer_client::TsgcWSLoadBalancerClient* __fastcall GetClientLoadBalancer();
	virtual void __fastcall OnLoadBalancerConnectEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection);
	virtual void __fastcall OnLoadBalancerDisconnectEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, int Code);
	virtual void __fastcall OnLoadBalancerErrorEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, const System::UnicodeString Error);
	virtual void __fastcall OnLoadBalancerExceptionEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::Sysutils::Exception* E);
	virtual System::UnicodeString __fastcall GetLoadBalancerConnectionMessage(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, bool aConnected);
	virtual void __fastcall DoLoadBalancerSendServerData();
	virtual void __fastcall DoLoadBalancerSendActiveConnections();
	virtual void __fastcall DoLoadBalancerSendServerBindings();
	virtual void __fastcall DoLoadBalancerSendServerReady();
	virtual void __fastcall OnLoadBalancerWriteDataTextEvent(const System::UnicodeString aGuid, const System::UnicodeString aText);
	virtual void __fastcall OnLoadBalancerWriteDataBinaryEvent(const System::UnicodeString aGuid, System::Classes::TStream* aStream);
	virtual void __fastcall OnLoadBalancerBroadCastTextEvent(const System::UnicodeString aText, const System::UnicodeString aChannel, const System::UnicodeString aProtocol, const System::UnicodeString aExclude, const System::UnicodeString aInclude);
	virtual void __fastcall OnLoadBalancerBroadCastBinaryEvent(System::Classes::TStream* aStream, const System::UnicodeString aChannel, const System::UnicodeString aProtocol, const System::UnicodeString aExclude, const System::UnicodeString aInclude, int aSize, Sgcwebsocket_types::TwsStreaming aStreaming);
	
public:
	__property TsgcWSLoadBalancerServer_Options* LoadBalancer = {read=FLoadBalancer, write=SetLoadBalancer};
	__property Sgcwebsocket_loadbalancer_client::TsgcWSLoadBalancerClient* ClientLoadBalancer = {read=GetClientLoadBalancer, write=FClientLoadBalancer};
	__property Sgcwebsocket_classes::TsgcWSConnectEvent OnLoadBalancerConnect = {read=FOnLoadBalancerConnect, write=FOnLoadBalancerConnect};
	__property Sgcwebsocket_classes::TsgcWSDisconnectEvent OnLoadBalancerDisconnect = {read=FOnLoadBalancerDisconnect, write=FOnLoadBalancerDisconnect};
	__property Sgcwebsocket_classes::TsgcWSLoadBalancerErrorEvent OnLoadBalancerError = {read=FOnLoadBalancerError, write=FOnLoadBalancerError};
	
protected:
	virtual bool __fastcall DoBuiltInLibraries(TsgcWSConnectionServer* aConnection, const System::UnicodeString aText, bool aDisconnect, int aStreamIdentifier = 0x0);
	
private:
	int __fastcall GetCount();
	TsgcWSConnectionServer* __fastcall GetConnectionByIndex(int Index);
	TsgcWSConnectionServer* __fastcall GetConnectionByGuid(const System::UnicodeString aGuid);
	
protected:
	virtual void __fastcall DoTCPDisconnect(TsgcWSConnectionServer* aConnection);
	virtual void __fastcall DoContextFree(Idcontext::TIdContext* AContext);
	
public:
	void __fastcall DisconnectAll();
	__property TsgcWSConnectionServer* ConnectionsByGUID[const System::UnicodeString Guid] = {read=GetConnectionByGuid};
	__property TsgcWSConnectionServer* Connections[int Index] = {read=GetConnectionByIndex/*, default*/};
	__property int Count = {read=GetCount, nodefault};
	
private:
	bool FActive;
	bool __fastcall GetActive();
	
protected:
	virtual void __fastcall SetActive(const bool Value);
	
public:
	__property bool Active = {read=GetActive, write=SetActive, default=0};
	
private:
	void __fastcall DoClear();
	
public:
	__fastcall virtual TsgcWSServer_Indy_Base(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSServer_Indy_Base();
	
protected:
	virtual void __fastcall DoBroadCast(const System::UnicodeString aMessage, System::Classes::TStream* aStream, Sgcwebsocket_types::TOpcode aOpCode, const System::UnicodeString aChannel = System::UnicodeString(), const System::UnicodeString aProtocol = System::UnicodeString(), const System::UnicodeString Exclude = System::UnicodeString(), const System::UnicodeString Include = System::UnicodeString(), const int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0));
	
public:
	virtual void __fastcall Broadcast(const System::UnicodeString aMessage, const System::UnicodeString aChannel = System::UnicodeString(), const System::UnicodeString aProtocol = System::UnicodeString(), const System::UnicodeString Exclude = System::UnicodeString(), const System::UnicodeString Include = System::UnicodeString())/* overload */;
	virtual void __fastcall Broadcast(System::Classes::TStream* aStream, const System::UnicodeString aChannel = System::UnicodeString(), const System::UnicodeString aProtocol = System::UnicodeString(), const System::UnicodeString Exclude = System::UnicodeString(), const System::UnicodeString Include = System::UnicodeString(), const int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0))/* overload */;
	
private:
	bool __fastcall DoWriteData(const System::UnicodeString aGuid, const System::UnicodeString aMessage)/* overload */;
	bool __fastcall DoWriteData(const System::UnicodeString aGuid, System::Classes::TStream* const aStream, int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0))/* overload */;
	
public:
	virtual bool __fastcall WriteData(const System::UnicodeString aGuid, const System::UnicodeString aMessage)/* overload */;
	virtual bool __fastcall WriteData(const System::UnicodeString aGuid, System::Classes::TStream* aStream, int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0))/* overload */;
	
protected:
	virtual void __fastcall DoPing(const System::UnicodeString aText = System::UnicodeString());
	
public:
	void __fastcall Ping(const System::UnicodeString aText = System::UnicodeString());
	
private:
	System::Classes::TNotifyEvent FOnStartup;
	System::Classes::TNotifyEvent FOnShutdown;
	Sgcwebsocket_server_base::TsgcWSAuthenticationEvent FOnAuthentication;
	TsgcWSUnknownAuthenticationEvent FOnUnknownAuthentication;
	Sgcwebsocket_server_base::TsgcWSHTTPUploadAfterSaveFileEvent FOnHTTPUploadAfterSaveFile;
	Sgcwebsocket_server_base::TsgcWSHTTPUploadBeforeSaveFileEvent FOnHTTPUploadBeforeSaveFile;
	
public:
	__property System::Classes::TNotifyEvent OnStartup = {read=FOnStartup, write=FOnStartup};
	__property System::Classes::TNotifyEvent OnShutdown = {read=FOnShutdown, write=FOnShutdown};
	__property Sgcwebsocket_server_base::TsgcWSAuthenticationEvent OnAuthentication = {read=FOnAuthentication, write=FOnAuthentication};
	__property TsgcWSUnknownAuthenticationEvent OnUnknownAuthentication = {read=FOnUnknownAuthentication, write=FOnUnknownAuthentication};
	__property Sgcwebsocket_server_base::TsgcWSHTTPUploadBeforeSaveFileEvent OnHTTPUploadBeforeSaveFile = {read=FOnHTTPUploadBeforeSaveFile, write=FOnHTTPUploadBeforeSaveFile};
	__property Sgcwebsocket_server_base::TsgcWSHTTPUploadAfterSaveFileEvent OnHTTPUploadAfterSaveFile = {read=FOnHTTPUploadAfterSaveFile, write=FOnHTTPUploadAfterSaveFile};
};


class PASCALIMPLEMENTATION TsgcWSServer : public TsgcWSServer_Indy_Base
{
	typedef TsgcWSServer_Indy_Base inherited;
	
private:
	Sgcwebsocket_customserver::TsgcWSCustomServer* FTCPServer;
	Sgcwebsocket_customserver::TsgcWSCustomServer* __fastcall GetTCPServer();
	
protected:
	__property Sgcwebsocket_customserver::TsgcWSCustomServer* TCPServer = {read=GetTCPServer};
	virtual void __fastcall OnHTTP2RequestEvent(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, System::Classes::TStringList* const aHeaders, const System::DynamicArray<System::Byte> aBytes, int aStreamIdentifierRequest, int aStreamIdentifierPush, System::Classes::TStream* const aPostStream);
	virtual Idcustomtcpserver::TIdCustomTCPServer* __fastcall GetCustomServer();
	virtual void __fastcall OnServerExecuteEvent(Idcontext::TIdContext* AContext);
	virtual void __fastcall OnServerDisconnectEvent(Idcontext::TIdContext* AContext);
	
public:
	__fastcall virtual TsgcWSServer(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSServer();
};


class PASCALIMPLEMENTATION TsgcWSHTTPServer : public TsgcWSServer_Indy_Base
{
	typedef TsgcWSServer_Indy_Base inherited;
	
protected:
	Idcustomhttpserver::TIdCustomHTTPServer* FHTTPServer;
	__property Idcustomhttpserver::TIdCustomHTTPServer* HTTPServer = {read=FHTTPServer};
	
private:
	System::UnicodeString FCharset;
	System::UnicodeString FDocumentRoot;
	Idcustomhttpserver::TIdHTTPCreateSession FOnCreateSession;
	Idcustomhttpserver::TIdHTTPInvalidSessionEvent FOnInvalidSession;
	Idcustomhttpserver::TIdHTTPSessionEndEvent FOnSessionEnd;
	Idcustomhttpserver::TIdHTTPSessionStartEvent FOnSessionStart;
	int FReadEmptySource;
	int FReadStartSSL;
	
protected:
	TsgcWSOnBeforeForwardHTTP FOnBeforeForwardHTTP;
	TsgcWSOnAfterForwardHTTP FOnAfterForwardHTTP;
	Idcustomhttpserver::TIdHTTPCommandEvent FOnCommandGet;
	Idcustomhttpserver::TIdHTTPCommandEvent FOnCommandOther;
	virtual bool __fastcall DoBeforeOnCommand(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Idcustomhttpserver::TIdHTTPRequestInfo* ARequestInfo, Idcustomhttpserver::TIdHTTPResponseInfo* AResponseInfo);
	virtual void __fastcall OnCommandGetEvent(Idcontext::TIdContext* AContext, Idcustomhttpserver::TIdHTTPRequestInfo* ARequestInfo, Idcustomhttpserver::TIdHTTPResponseInfo* AResponseInfo);
	virtual void __fastcall OnCommandOtherEvent(Idcontext::TIdContext* AContext, Idcustomhttpserver::TIdHTTPRequestInfo* ARequestInfo, Idcustomhttpserver::TIdHTTPResponseInfo* AResponseInfo);
	virtual void __fastcall OnCreateSessionEvent(Idcontext::TIdContext* ASender, Idcustomhttpserver::TIdHTTPSession* &VHTTPSession);
	virtual void __fastcall OnInvalidSessionEvent(Idcontext::TIdContext* AContext, Idcustomhttpserver::TIdHTTPRequestInfo* ARequestInfo, Idcustomhttpserver::TIdHTTPResponseInfo* AResponseInfo, bool &VContinueProcessing, const System::UnicodeString AInvalidSessionID);
	virtual void __fastcall OnSessionStartEvent(Idcustomhttpserver::TIdHTTPSession* Sender);
	virtual void __fastcall OnSessionEndEvent(Idcustomhttpserver::TIdHTTPSession* Sender);
	virtual void __fastcall OnExceptionEvent(Idcontext::TIdContext* AContext, System::Sysutils::Exception* AException);
	virtual void __fastcall OnServerParseAuthenticationEvent(Idcontext::TIdContext* AContext, const System::UnicodeString AAuthType, const System::UnicodeString AAuthData, System::UnicodeString &VUsername, System::UnicodeString &VPassword, bool &VHandled);
	virtual void __fastcall OnCreatePostStreamEvent(Idcontext::TIdContext* AContext, Idheaderlist::TIdHeaderList* aHeaders, System::Classes::TStream* &VPostStream);
	virtual void __fastcall OnDoneWithPostStreamEvent(Idcontext::TIdContext* AContext, Idcustomhttpserver::TIdHTTPRequestInfo* ARequestInfo, bool &VCanFree);
	virtual bool __fastcall DoResponseHTTP(Idcontext::TIdContext* AContext, Idcustomhttpserver::TIdHTTPRequestInfo* ARequestInfo, Idcustomhttpserver::TIdHTTPResponseInfo* AResponseInfo);
	virtual void __fastcall DoResponseAfterCommand(TsgcWSConnectionServer* aConnection, Idcustomhttpserver::TIdHTTPResponseInfo* AResponseInfo);
	System::UnicodeString __fastcall GetCustomHeadersFromRaw(Idheaderlist::TIdHeaderList* aRawHeaders);
	virtual bool __fastcall DoForwardHTTP(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, Idcustomhttpserver::TIdHTTPRequestInfo* ARequestInfo, Idcustomhttpserver::TIdHTTPResponseInfo* AResponseInfo);
	
public:
	__property Idcustomhttpserver::TIdHTTPCommandEvent OnCommandGet = {read=FOnCommandGet, write=FOnCommandGet};
	__property Idcustomhttpserver::TIdHTTPCommandEvent OnCommandOther = {read=FOnCommandOther, write=FOnCommandOther};
	__property Idcustomhttpserver::TIdHTTPCreateSession OnCreateSession = {read=FOnCreateSession, write=FOnCreateSession};
	__property Idcustomhttpserver::TIdHTTPInvalidSessionEvent OnInvalidSession = {read=FOnInvalidSession, write=FOnInvalidSession};
	__property Idcustomhttpserver::TIdHTTPSessionEndEvent OnSessionEnd = {read=FOnSessionEnd, write=FOnSessionEnd};
	__property Idcustomhttpserver::TIdHTTPSessionStartEvent OnSessionStart = {read=FOnSessionStart, write=FOnSessionStart};
	__property TsgcWSOnBeforeForwardHTTP OnBeforeForwardHTTP = {read=FOnBeforeForwardHTTP, write=FOnBeforeForwardHTTP};
	__property TsgcWSOnAfterForwardHTTP OnAfterForwardHTTP = {read=FOnAfterForwardHTTP, write=FOnAfterForwardHTTP};
	
private:
	bool __fastcall GetAutoStartSession();
	System::UnicodeString __fastcall GetDocumentRoot();
	bool __fastcall GetKeepAlive();
	bool __fastcall GetParseParams();
	Idcustomhttpserver::TIdHTTPCustomSessionList* __fastcall GetSessionList();
	bool __fastcall GetSessionState();
	int __fastcall GetSessionTimeOut();
	void __fastcall SetAutoStartSession(const bool Value);
	void __fastcall SetKeepAlive(const bool Value);
	void __fastcall SetParseParams(const bool Value);
	void __fastcall SetSessionState(const bool Value);
	void __fastcall SetSessionTimeOut(const int Value);
	
public:
	__property bool AutoStartSession = {read=GetAutoStartSession, write=SetAutoStartSession, nodefault};
	__property System::UnicodeString Charset = {read=FCharset, write=FCharset};
	__property System::UnicodeString DocumentRoot = {read=GetDocumentRoot, write=FDocumentRoot};
	__property bool KeepAlive = {read=GetKeepAlive, write=SetKeepAlive, nodefault};
	__property bool ParseParams = {read=GetParseParams, write=SetParseParams, nodefault};
	__property int ReadEmptySource = {read=FReadEmptySource, write=FReadEmptySource, nodefault};
	__property int ReadStartSSL = {read=FReadStartSSL, write=FReadStartSSL, nodefault};
	__property Idcustomhttpserver::TIdHTTPCustomSessionList* SessionList = {read=GetSessionList};
	__property bool SessionState = {read=GetSessionState, write=SetSessionState, nodefault};
	__property int SessionTimeOut = {read=GetSessionTimeOut, write=SetSessionTimeOut, nodefault};
	
protected:
	virtual void __fastcall DoExecuteHTTP(Idcontext::TIdContext* AContext);
	virtual Idcustomtcpserver::TIdCustomTCPServer* __fastcall GetCustomServer();
	virtual void __fastcall OnServerExecuteEvent(Idcontext::TIdContext* AContext);
	virtual void __fastcall OnServerDisconnectEvent(Idcontext::TIdContext* AContext);
	virtual bool __fastcall DoHTTPBasicAuthentication(Sgcwebsocket_classes::TsgcWSConnection* aConnection, Idcustomhttpserver::TIdHTTPRequestInfo* &ARequestInfo, Idcustomhttpserver::TIdHTTPResponseInfo* &AResponseInfo);
	
public:
	__fastcall virtual TsgcWSHTTPServer(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSHTTPServer();
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_server */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_SERVER)
using namespace Sgcwebsocket_server;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_serverHPP
