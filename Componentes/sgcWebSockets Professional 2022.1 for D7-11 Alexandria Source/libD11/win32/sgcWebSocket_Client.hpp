// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Client.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_clientHPP
#define Sgcwebsocket_clientHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Math.hpp>
#include <System.StrUtils.hpp>
#include <System.Contnrs.hpp>
#include <IdTCPClient.hpp>
#include <IdTCPConnection.hpp>
#include <IdGlobal.hpp>
#include <IdComponent.hpp>
#include <IdHeaderList.hpp>
#include <IdIOHandler.hpp>
#include <IdIOHandlerSocket.hpp>
#include <IdIOHandlerStack.hpp>
#include <IdSSL.hpp>
#include <IdSSLOpenSSL.hpp>
#include <IdSSLOpenSSLHeaders.hpp>
#include <IdConnectThroughHttpProxy.hpp>
#include <IdSocks.hpp>
#include <IdIntercept.hpp>
#include <IdException.hpp>
#include <IdInterceptThrottler.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Classes_Indy.hpp>
#include <sgcWebSocket_CustomClient.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Const.hpp>
#include <sgcTCP_Client.hpp>
#include <sgcTCP_Classes.hpp>
#include <sgcHTTP_OAuth_Client.hpp>
#include <sgcSocket_Classes_Indy.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcHTTP_JWT_Client.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_client
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSProtocol_Client_KeepAlive;
class DELPHICLASS TsgcWSHandShakeClient;
class DELPHICLASS TsgcWSOptionsClient;
class DELPHICLASS TsgcWSAuthenticationClient_Session;
class DELPHICLASS TsgcWSAuthenticationClient_URL;
class DELPHICLASS TsgcWSAuthenticationClient_Basic;
class DELPHICLASS TsgcWSAuthenticationClient_Token;
class DELPHICLASS TsgcWSAuthenticationClient_Options;
class DELPHICLASS TsgcWSLoadBalancerClient_Options;
class DELPHICLASS TsgcWSConnectionClient;
class DELPHICLASS TsgcWSQueueClient_Options;
class DELPHICLASS TsgcWSProxy_Options;
class DELPHICLASS TClientThread;
class DELPHICLASS TsgcWSClient;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSOnSSLGetHandler)(System::TObject* Sender, Sgcwebsocket_types::TwsSSLHandler aType, Idssl::TIdSSLIOHandlerSocketBase* &aSSLHandler);

typedef void __fastcall (__closure *TsgcWSOnSSLAfterCreateHandler)(System::TObject* Sender, Sgcwebsocket_types::TwsSSLHandler aType, Idssl::TIdSSLIOHandlerSocketBase* aSSLHandler);

typedef void __fastcall (__closure *TsgcWSOnSSLVerifyPeer)(System::TObject* Sender, Idsslopenssl::TIdX509* Certificate, bool AOk, int ADepth, int AError, bool &Accept);

typedef void __fastcall (__closure *TsgcWSOnRedirect)(System::TObject* Sender, const System::UnicodeString aLocation);

class PASCALIMPLEMENTATION TsgcWSProtocol_Client_KeepAlive : public Sgcwebsocket_classes::TsgcWSProtocol_Client
{
	typedef Sgcwebsocket_classes::TsgcWSProtocol_Client inherited;
	
public:
	/* TsgcWSProtocol_Client.Create */ inline __fastcall virtual TsgcWSProtocol_Client_KeepAlive(System::Classes::TComponent* aOwner) : Sgcwebsocket_classes::TsgcWSProtocol_Client(aOwner) { }
	/* TsgcWSProtocol_Client.Destroy */ inline __fastcall virtual ~TsgcWSProtocol_Client_KeepAlive() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSHandShakeClient : public Sgcwebsocket_classes_indy::TsgcWSHandshake
{
	typedef Sgcwebsocket_classes_indy::TsgcWSHandshake inherited;
	
private:
	bool FRedirect;
	System::UnicodeString FAccept;
	System::UnicodeString FLocation;
	
public:
	__property System::UnicodeString Accept = {read=FAccept};
	__property bool Redirect = {read=FRedirect, nodefault};
	__property System::UnicodeString Location = {read=FLocation};
	
protected:
	void __fastcall DoDecodeConnection(const System::UnicodeString aHeader);
	void __fastcall DoDecodeHeader(Idtcpconnection::TIdTCPConnection* const aConnection, Idheaderlist::TIdHeaderList* aHeaders);
	void __fastcall DoDecodeHTTP(const System::UnicodeString aHeader);
	void __fastcall DoDecodeAccept(const System::UnicodeString aHeader);
	void __fastcall DoDecodeProtocol(const System::UnicodeString aHeader);
	void __fastcall DoDecodeUpgrade(const System::UnicodeString aHeader);
	void __fastcall DoDecodeExtensions(const System::UnicodeString aHeader);
	void __fastcall DoDecodeLocation(const System::UnicodeString aHeader);
	
public:
	__fastcall virtual TsgcWSHandShakeClient(Idtcpconnection::TIdTCPConnection* aConnection, Idheaderlist::TIdHeaderList* aHeaders);
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSHandShakeClient() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSOptionsClient : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FCleanDisconnect;
	System::UnicodeString FParameters;
	Sgcwebsocket_types::TwsFragmentedMessages FFragmentedMessages;
	System::UnicodeString FOrigin;
	bool FRaiseDisconnectExceptions;
	bool FValidateUTF8;
	System::UnicodeString __fastcall GetParameters();
	void __fastcall SetParameters(const System::UnicodeString Value);
	
public:
	__fastcall virtual TsgcWSOptionsClient();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool CleanDisconnect = {read=FCleanDisconnect, write=FCleanDisconnect, nodefault};
	__property Sgcwebsocket_types::TwsFragmentedMessages FragmentedMessages = {read=FFragmentedMessages, write=FFragmentedMessages, nodefault};
	__property System::UnicodeString Origin = {read=FOrigin, write=FOrigin};
	__property System::UnicodeString Parameters = {read=GetParameters, write=SetParameters};
	__property bool RaiseDisconnectExceptions = {read=FRaiseDisconnectExceptions, write=FRaiseDisconnectExceptions, nodefault};
	__property bool ValidateUTF8 = {read=FValidateUTF8, write=FValidateUTF8, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSOptionsClient() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAuthenticationClient_Session : public Sgcwebsocket_classes::TsgcWSAuthentication_Session
{
	typedef Sgcwebsocket_classes::TsgcWSAuthentication_Session inherited;
	
private:
	System::UnicodeString FExtensions;
	System::UnicodeString FProtocols;
	System::UnicodeString FID;
	
public:
	__property System::UnicodeString Extensions = {read=FExtensions, write=FExtensions};
	__property System::UnicodeString Protocols = {read=FProtocols, write=FProtocols};
	__property System::UnicodeString ID = {read=FID, write=FID};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSAuthenticationClient_Session() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSAuthenticationClient_Session() : Sgcwebsocket_classes::TsgcWSAuthentication_Session() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAuthenticationClient_URL : public Sgcwebsocket_classes::TsgcWSAuthentication_URL
{
	typedef Sgcwebsocket_classes::TsgcWSAuthentication_URL inherited;
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSAuthenticationClient_URL() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSAuthenticationClient_URL() : Sgcwebsocket_classes::TsgcWSAuthentication_URL() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAuthenticationClient_Basic : public Sgcwebsocket_classes::TsgcWSAuthentication_Basic
{
	typedef Sgcwebsocket_classes::TsgcWSAuthentication_Basic inherited;
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSAuthenticationClient_Basic() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSAuthenticationClient_Basic() : Sgcwebsocket_classes::TsgcWSAuthentication_Basic() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAuthenticationClient_Token : public Sgcwebsocket_classes::TsgcWSAuthentication_Token
{
	typedef Sgcwebsocket_classes::TsgcWSAuthentication_Token inherited;
	
private:
	Sgchttp_jwt_client::TsgcHTTPComponentClient_JWT* FJWT;
	Sgchttp_oauth_client::TsgcHTTPComponentClient_OAuth* FOAuth;
	
public:
	__fastcall virtual TsgcWSAuthenticationClient_Token();
	
__published:
	__property Sgchttp_jwt_client::TsgcHTTPComponentClient_JWT* JWT = {read=FJWT, write=FJWT};
	__property Sgchttp_oauth_client::TsgcHTTPComponentClient_OAuth* OAuth = {read=FOAuth, write=FOAuth};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSAuthenticationClient_Token() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAuthenticationClient_Options : public Sgcwebsocket_classes::TsgcWSAuthentication_Options
{
	typedef Sgcwebsocket_classes::TsgcWSAuthentication_Options inherited;
	
private:
	TsgcWSAuthenticationClient_URL* FURL;
	System::UnicodeString FPassword;
	TsgcWSAuthenticationClient_Session* FSession;
	TsgcWSAuthenticationClient_Basic* FBasic;
	TsgcWSAuthenticationClient_Token* FToken;
	System::UnicodeString FUser;
	void __fastcall SetToken(TsgcWSAuthenticationClient_Token* const Value);
	
protected:
	void __fastcall SetURL(TsgcWSAuthenticationClient_URL* const Value);
	void __fastcall SetSession(TsgcWSAuthenticationClient_Session* const Value);
	void __fastcall SetBasic(TsgcWSAuthenticationClient_Basic* const Value);
	
public:
	__fastcall virtual TsgcWSAuthenticationClient_Options();
	__fastcall virtual ~TsgcWSAuthenticationClient_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property TsgcWSAuthenticationClient_URL* URL = {read=FURL, write=SetURL};
	__property System::UnicodeString Password = {read=FPassword, write=FPassword};
	__property TsgcWSAuthenticationClient_Session* Session = {read=FSession, write=SetSession};
	__property TsgcWSAuthenticationClient_Basic* Basic = {read=FBasic, write=SetBasic};
	__property TsgcWSAuthenticationClient_Token* Token = {read=FToken, write=SetToken};
	__property System::UnicodeString User = {read=FUser, write=FUser};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSLoadBalancerClient_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	System::UnicodeString FHost;
	int FPort;
	System::Classes::TStringList* FServers;
	void __fastcall SetServers(System::Classes::TStringList* const Value);
	
public:
	__fastcall virtual TsgcWSLoadBalancerClient_Options();
	__fastcall virtual ~TsgcWSLoadBalancerClient_Options();
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property System::UnicodeString Host = {read=FHost, write=FHost};
	__property int Port = {read=FPort, write=FPort, nodefault};
	__property System::Classes::TStringList* Servers = {read=FServers, write=SetServers};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSConnectionClient : public Sgcwebsocket_classes_indy::TsgcWSConnection_Indy
{
	typedef Sgcwebsocket_classes_indy::TsgcWSConnection_Indy inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetURL();
	
private:
	System::UnicodeString FHost;
	int FPort;
	TsgcWSOptionsClient* FOptions;
	TsgcWSAuthenticationClient_Options* FAuthentication;
	
protected:
	void __fastcall SetOptions(TsgcWSOptionsClient* const Value);
	void __fastcall SetAuthentication(TsgcWSAuthenticationClient_Options* const Value);
	
public:
	__property System::UnicodeString Host = {read=FHost, write=FHost};
	__property int Port = {read=FPort, write=FPort, nodefault};
	
private:
	bool FBeforeDisconnect;
	void __fastcall DoBeforeDisconnectEvent();
	
protected:
	virtual void __fastcall DoClose(int aCloseCode = 0x3e8);
	virtual void __fastcall DoDisconnect(const System::UnicodeString AError = System::UnicodeString(), int aCloseCode = 0x3e8);
	
private:
	void __fastcall DoValidateProtocols();
	System::UnicodeString __fastcall GetALPNProtocol();
	
public:
	__property System::UnicodeString ALPNProtocol = {read=GetALPNProtocol};
	
private:
	System::UnicodeString FKey;
	TsgcWSHandShakeClient* FHandshake;
	System::Classes::TNotifyEvent FOnEnabled;
	
protected:
	virtual void __fastcall DoReadHandshake_RFC8441();
	virtual void __fastcall DoSendHandshake_RFC8441();
	virtual void __fastcall DoHandShakeExtensions();
	virtual void __fastcall DoHandshake();
	System::UnicodeString __fastcall GetWSKey();
	void __fastcall DoSendHandshake_RFC6445();
	__property TsgcWSAuthenticationClient_Options* Authentication = {read=FAuthentication, write=SetAuthentication};
	__property TsgcWSOptionsClient* Options = {read=FOptions, write=SetOptions};
	
public:
	void __fastcall DoInitialize(Idtcpconnection::TIdTCPConnection* aConnection);
	__property System::Classes::TNotifyEvent OnEnabled = {read=FOnEnabled, write=FOnEnabled};
	__fastcall virtual TsgcWSConnectionClient();
	__fastcall virtual ~TsgcWSConnectionClient();
	
private:
	System::Classes::TNotifyEvent FOnBeforeDisconnect;
	TsgcWSOnRedirect FOnRedirect;
	
public:
	__property System::Classes::TNotifyEvent OnBeforeDisconnect = {read=FOnBeforeDisconnect, write=FOnBeforeDisconnect};
	__property TsgcWSOnRedirect OnRedirect = {read=FOnRedirect, write=FOnRedirect};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSQueueClient_Options : public Sgcwebsocket_classes::TsgcWSQueueOptions
{
	typedef Sgcwebsocket_classes::TsgcWSQueueOptions inherited;
	
public:
	/* TsgcWSQueueOptions.Create */ inline __fastcall virtual TsgcWSQueueClient_Options() : Sgcwebsocket_classes::TsgcWSQueueOptions() { }
	/* TsgcWSQueueOptions.Destroy */ inline __fastcall virtual ~TsgcWSQueueClient_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSProxy_Options : public Sgctcp_classes::TsgcTCPProxy_Options
{
	typedef Sgctcp_classes::TsgcTCPProxy_Options inherited;
	
public:
	/* TsgcSocketProxy_Options.Create */ inline __fastcall virtual TsgcWSProxy_Options() : Sgctcp_classes::TsgcTCPProxy_Options() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSProxy_Options() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TClientThread : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
private:
	TsgcWSClient* FClient;
	System::UnicodeString FMethod;
	
protected:
	virtual void __fastcall Execute();
	
public:
	__fastcall TClientThread(const System::UnicodeString aMethod, TsgcWSClient* const aClient);
	__classmethod void __fastcall Stop(TsgcWSClient* const aClient);
	__classmethod void __fastcall Start(TsgcWSClient* const aClient);
public:
	/* TThread.Destroy */ inline __fastcall virtual ~TClientThread() { }
	
};


class PASCALIMPLEMENTATION TsgcWSClient : public Sgcwebsocket_classes_indy::TsgcWSComponent_Client_Indy
{
	typedef Sgcwebsocket_classes_indy::TsgcWSComponent_Client_Indy inherited;
	
protected:
	virtual void __fastcall Loaded();
	virtual void __fastcall DoEventDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int Code);
	virtual Sgcwebsocket_classes::TsgcWSSpecifications* __fastcall GetSpecifications();
	virtual void __fastcall DoNotifyDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall SetURL(const System::UnicodeString Value);
	
private:
	Sgcwebsocket_customclient::TsgcWSCustomClient* FTCPClient;
	Sgcwebsocket_customclient::TsgcWSCustomClient* __fastcall GetTCPClient();
	
protected:
	virtual void __fastcall DoStopReadThread();
	__property Sgcwebsocket_customclient::TsgcWSCustomClient* TCPClient = {read=GetTCPClient, write=FTCPClient};
	
private:
	void __fastcall DoStopThreads();
	
protected:
	TsgcWSConnectionClient* FWSConnection;
	bool FDisconnecting;
	Sgcwebsocket_classes::TsgcWSConnection* __fastcall GetConnection();
	
public:
	__property Sgcwebsocket_classes::TsgcWSConnection* Connection = {read=GetConnection};
	
private:
	Sgcwebsocket_classes_indy::TsgcWSIdLogFileClient* FInterceptLogFile;
	Sgcwebsocket_classes_indy::TsgcWSIdLogFileClient* __fastcall GetInterceptLogFile();
	
protected:
	__property Sgcwebsocket_classes_indy::TsgcWSIdLogFileClient* InterceptLogFile = {read=GetInterceptLogFile, write=FInterceptLogFile};
	
private:
	Idinterceptthrottler::TIdInterceptThrottler* FInterceptThrottle;
	Idinterceptthrottler::TIdInterceptThrottler* __fastcall GetInterceptThrottle();
	
protected:
	__property Idinterceptthrottler::TIdInterceptThrottler* InterceptThrottle = {read=GetInterceptThrottle, write=FInterceptThrottle};
	Idintercept::TIdConnectionIntercept* __fastcall GetConnectionIntercept();
	
private:
	TsgcWSProxy_Options* FProxy;
	Idiohandlerstack::TIdIOHandlerStack* FProxyIOHandler;
	Idconnectthroughhttpproxy::TIdConnectThroughHttpProxy* FProxyHTTP;
	Idsocks::TIdSocksInfo* FProxySocks;
	Idiohandlerstack::TIdIOHandlerStack* __fastcall GetProxyIOHandler();
	Idconnectthroughhttpproxy::TIdConnectThroughHttpProxy* __fastcall GetProxyHTTP();
	Idsocks::TIdSocksInfo* __fastcall GetProxySocks();
	
protected:
	virtual void __fastcall SetProxy(TsgcWSProxy_Options* const Value);
	
public:
	__property TsgcWSProxy_Options* Proxy = {read=FProxy, write=SetProxy};
	
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
	TsgcWSAuthenticationClient_Options* FAuthentication;
	void __fastcall SetAuthentication(TsgcWSAuthenticationClient_Options* const Value);
	
protected:
	virtual bool __fastcall DoGetSession();
	virtual System::UnicodeString __fastcall DoGetSessionURL();
	virtual System::UnicodeString __fastcall DoGetSessionBody();
	virtual bool __fastcall DoSessionResponse(System::Classes::TStringStream* aStream);
	virtual void __fastcall OnAuthTokenEvent(System::TObject* Sender, const System::UnicodeString TokenType, const System::UnicodeString Token, const System::UnicodeString Data);
	virtual void __fastcall OnAuthTokenErrorEvent(System::TObject* Sender, const System::UnicodeString Error, const System::UnicodeString ErrorDescription, const System::UnicodeString Data);
	
public:
	__property TsgcWSAuthenticationClient_Options* Authentication = {read=FAuthentication, write=SetAuthentication};
	
private:
	Sgcwebsocket_classes::TsgcWSOnBeforeWatchDogEvent FOnBeforeWatchDog;
	void __fastcall DoStart();
	
protected:
	virtual void __fastcall OnWatchDogEvent(System::TObject* Sender);
	virtual void __fastcall OnWatchDogExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	
public:
	__property Sgcwebsocket_classes::TsgcWSOnBeforeWatchDogEvent OnBeforeWatchDog = {read=FOnBeforeWatchDog, write=FOnBeforeWatchDog};
	
private:
	TsgcWSLoadBalancerClient_Options* FLoadBalancer;
	Sgcwebsocket_classes::TsgcWSLoadBalancerErrorEvent FOnLoadBalancerError;
	
protected:
	void __fastcall SetLoadBalancer(TsgcWSLoadBalancerClient_Options* const Value);
	
private:
	bool __fastcall GetHostByServers();
	bool __fastcall GetHostByLoadBalancerServer();
	
protected:
	virtual void __fastcall DoLoadBalancerGetHost();
	
public:
	__property TsgcWSLoadBalancerClient_Options* LoadBalancer = {read=FLoadBalancer, write=SetLoadBalancer};
	__property Sgcwebsocket_classes::TsgcWSLoadBalancerErrorEvent OnLoadBalancerError = {read=FOnLoadBalancerError, write=FOnLoadBalancerError};
	
private:
	Idssl::TIdSSLIOHandlerSocketBase* FHandlerSSL;
	
protected:
	TsgcWSOnSSLGetHandler FOnSSLGetHandler;
	TsgcWSOnSSLAfterCreateHandler FOnSSLAfterCreateHandler;
	TsgcWSOnSSLVerifyPeer FOnSSLVerifyPeer;
	__property Idssl::TIdSSLIOHandlerSocketBase* HandlerSSL = {read=FHandlerSSL};
	virtual void __fastcall GetHandlerSSLEvent(const Sgcwebsocket_types::TwsSSLHandler aSSLType, Idssl::TIdSSLIOHandlerSocketBase* &aSSLHandler);
	virtual void __fastcall DoSSL();
	virtual void __fastcall OnGetPasswordEvent(System::UnicodeString &Password);
	virtual bool __fastcall OnVerifyPeerEvent(Idsslopenssl::TIdX509* Certificate, bool AOk, int ADepth, int AError);
	
public:
	__property TsgcWSOnSSLGetHandler OnSSLGetHandler = {read=FOnSSLGetHandler, write=FOnSSLGetHandler};
	__property TsgcWSOnSSLAfterCreateHandler OnSSLAfterCreateHandler = {read=FOnSSLAfterCreateHandler, write=FOnSSLAfterCreateHandler};
	__property TsgcWSOnSSLVerifyPeer OnSSLVerifyPeer = {read=FOnSSLVerifyPeer, write=FOnSSLVerifyPeer};
	
private:
	Sgcwebsocket_classes::TsgcWSOnBeforeConnectEvent FOnBeforeConnect;
	
protected:
	virtual bool __fastcall GetActive();
	virtual void __fastcall SetActive(const bool Value);
	
public:
	__property Sgcwebsocket_classes::TsgcWSOnBeforeConnectEvent OnBeforeConnect = {read=FOnBeforeConnect, write=FOnBeforeConnect};
	
private:
	bool FReadingThread;
	bool FActive;
	int FConnectTimeout;
	Idglobal::TIdIPVersion FIPVersion;
	TsgcWSOptionsClient* FOptions;
	TsgcWSQueueClient_Options* FQueueOptions;
	int FReadTimeout;
	int FWriteTimeout;
	bool FUseNagle;
	System::Word FBoundPortMax;
	System::Word FBoundPortMin;
	int FLingerState;
	void __fastcall SetQueueOptions(TsgcWSQueueClient_Options* const Value);
	
protected:
	virtual System::UnicodeString __fastcall GetProtocol();
	virtual void __fastcall SetOptions(TsgcWSOptionsClient* const Value);
	
public:
	__property System::Word BoundPortMin = {read=FBoundPortMin, write=FBoundPortMin, nodefault};
	__property System::Word BoundPortMax = {read=FBoundPortMax, write=FBoundPortMax, nodefault};
	__property System::UnicodeString Protocol = {read=GetProtocol};
	__property int ConnectTimeout = {read=FConnectTimeout, write=FConnectTimeout, nodefault};
	__property TsgcWSOptionsClient* Options = {read=FOptions, write=SetOptions};
	__property int ReadTimeout = {read=FReadTimeout, write=FReadTimeout, nodefault};
	__property Idglobal::TIdIPVersion IPVersion = {read=FIPVersion, write=FIPVersion, nodefault};
	__property int LingerState = {read=FLingerState, write=FLingerState, nodefault};
	__property TsgcWSQueueClient_Options* QueueOptions = {read=FQueueOptions, write=SetQueueOptions};
	__property bool UseNagle = {read=FUseNagle, write=FUseNagle, stored=true, nodefault};
	__property int WriteTimeout = {read=FWriteTimeout, write=FWriteTimeout, nodefault};
	
private:
	bool FThreadStart;
	
public:
	void __fastcall Start();
	void __fastcall Stop();
	
private:
	bool FConnecting;
	
public:
	bool __fastcall Connect(const int aTimeout = 0x2710);
	bool __fastcall Disconnect(const int aTimeout = 0x2710);
	bool __fastcall Connected();
	
protected:
	virtual void __fastcall OnConnectionBeforeDisconnect(System::TObject* Sender);
	virtual void __fastcall OnConnectionRedirect(System::TObject* Sender, const System::UnicodeString aLocation);
	virtual void __fastcall OnClientConnectEvent(System::TObject* Sender);
	virtual void __fastcall OnClientMessageEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual void __fastcall OnClientBinaryEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::Classes::TMemoryStream* const aStream);
	virtual void __fastcall OnClientFragmentedEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::Classes::TMemoryStream* const aData, const Sgcwebsocket_types::TOpcode aOpCode, const bool aContinuation);
	virtual void __fastcall OnClientDisconnectEvent(System::TObject* Sender);
	virtual void __fastcall OnClientStatusEvent(System::TObject* ASender, const Idcomponent::TIdStatus AStatus, const System::UnicodeString AStatusText);
	virtual void __fastcall OnClientEnabledEvent(System::TObject* Sender);
	virtual void __fastcall OnClientHandShakeEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::Classes::TStringList* &Headers);
	void __fastcall OnReadEvent(System::TObject* Sender);
	void __fastcall OnThreadEvent(System::TObject* Sender);
	void __fastcall OnReadExceptionEvent(System::Sysutils::Exception* aException);
	
private:
	void __fastcall DoClear();
	
public:
	__fastcall virtual TsgcWSClient(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSClient();
	
private:
	bool __fastcall KeepAliveProtocol();
	
protected:
	virtual void __fastcall DoPing(const System::UnicodeString aText = System::UnicodeString())/* overload */;
	virtual bool __fastcall DoPing(int aTimeout, const System::UnicodeString aText = System::UnicodeString())/* overload */;
	
public:
	void __fastcall Ping(const System::UnicodeString aText = System::UnicodeString())/* overload */;
	bool __fastcall Ping(int aTimeout, const System::UnicodeString aText = System::UnicodeString())/* overload */;
	virtual void __fastcall WriteData(const System::UnicodeString aText, const int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0))/* overload */;
	virtual void __fastcall WriteData(System::Classes::TStream* const aStream, const int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0))/* overload */;
	virtual System::UnicodeString __fastcall WriteAndWaitData(const System::UnicodeString aText, const int aTimeout = 0x2710);
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_client */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_CLIENT)
using namespace Sgcwebsocket_client;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_clientHPP
