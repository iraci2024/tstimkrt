// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Server_Proxy.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_server_proxyHPP
#define Sgcwebsocket_server_proxyHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <IdSSLOpenSSL.hpp>
#include <IdSSL.hpp>
#include <IdIOHandler.hpp>
#include <sgcWebSocket_Server.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_CustomClient.hpp>
#include <sgcWebSocket_Client.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_server_proxy
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSProxyServer_Options;
class DELPHICLASS TsgcWSProxyConnection;
class DELPHICLASS TsgcWSProxyServer;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSOnSSLGetHandler)(System::TObject* Sender, Sgcwebsocket_types::TwsSSLHandler aType, Idssl::TIdSSLIOHandlerSocketBase* &aSSLHandler);

typedef void __fastcall (__closure *TsgcWSOnSSLAfterCreateHandler)(System::TObject* Sender, Sgcwebsocket_types::TwsSSLHandler aType, Idssl::TIdSSLIOHandlerSocketBase* aSSLHandler);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSProxyServer_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FHost;
	Sgcwebsocket_types::TwsMessageType FMessageType;
	int FPort;
	bool FTLS;
	
public:
	__fastcall virtual TsgcWSProxyServer_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString Host = {read=FHost, write=FHost};
	__property Sgcwebsocket_types::TwsMessageType MessageType = {read=FMessageType, write=FMessageType, nodefault};
	__property int Port = {read=FPort, write=FPort, nodefault};
	__property bool TLS = {read=FTLS, write=FTLS, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSProxyServer_Options() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSProxyConnection : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Sgcwebsocket_classes::TsgcWSConnection* FWSConnection;
	bool FTLS;
	Sgcwebsocket_types::TwsMessageType FMessageType;
	System::UnicodeString __fastcall GetHost();
	int __fastcall GetPort();
	void __fastcall SetHost(const System::UnicodeString Value);
	void __fastcall SetPort(const int Value);
	
public:
	__property System::UnicodeString Host = {read=GetHost, write=SetHost};
	__property Sgcwebsocket_types::TwsMessageType MessageType = {read=FMessageType, write=FMessageType, nodefault};
	__property int Port = {read=GetPort, write=SetPort, nodefault};
	__property bool TLS = {read=FTLS, write=FTLS, nodefault};
	
private:
	Sgcwebsocket_customclient::TsgcWSCustomClient* FTCPClient;
	Sgcwebsocket_customclient::TsgcWSCustomClient* __fastcall GetTCPClient();
	
protected:
	virtual void __fastcall OnReadEvent(System::TObject* Sender);
	__property Sgcwebsocket_customclient::TsgcWSCustomClient* TCPClient = {read=GetTCPClient, write=FTCPClient};
	
private:
	Idssl::TIdSSLIOHandlerSocketBase* FHandlerSSL;
	
protected:
	TsgcWSOnSSLGetHandler FOnSSLGetHandler;
	TsgcWSOnSSLAfterCreateHandler FOnSSLAfterCreateHandler;
	virtual void __fastcall GetHandlerSSLEvent(const Sgcwebsocket_types::TwsSSLHandler aSSLType, Idssl::TIdSSLIOHandlerSocketBase* &aSSLHandler);
	__property Idssl::TIdSSLIOHandlerSocketBase* HandlerSSL = {read=FHandlerSSL};
	
public:
	__property TsgcWSOnSSLGetHandler OnSSLGetHandler = {read=FOnSSLGetHandler, write=FOnSSLGetHandler};
	__property TsgcWSOnSSLAfterCreateHandler OnSSLAfterCreateHandler = {read=FOnSSLAfterCreateHandler, write=FOnSSLAfterCreateHandler};
	void __fastcall Connect();
	void __fastcall Disconnect();
	void __fastcall WriteData(const System::UnicodeString aText)/* overload */;
	void __fastcall WriteData(System::Classes::TStream* const aData)/* overload */;
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcWSProxyConnection(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcWSProxyConnection() { }
	
};


class PASCALIMPLEMENTATION TsgcWSProxyServer : public Sgcwebsocket_server::TsgcWSServer
{
	typedef Sgcwebsocket_server::TsgcWSServer inherited;
	
protected:
	virtual void __fastcall DoEventConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int Code);
	virtual void __fastcall DoEventMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual void __fastcall DoEventBinary(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, System::Classes::TMemoryStream* Data);
	virtual void __fastcall DoEventError(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Error);
	virtual void __fastcall DoEventException(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Error, System::Sysutils::Exception* aException);
	virtual void __fastcall DoNotifyConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoNotifyMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoNotifyBinary(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoNotifyDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	
private:
	TsgcWSProxyServer_Options* FProxy;
	
protected:
	virtual void __fastcall SetProxy(TsgcWSProxyServer_Options* const Value);
	
public:
	__property TsgcWSProxyServer_Options* Proxy = {read=FProxy, write=SetProxy};
	__fastcall virtual TsgcWSProxyServer(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSProxyServer();
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_server_proxy */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_SERVER_PROXY)
using namespace Sgcwebsocket_server_proxy;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_server_proxyHPP
