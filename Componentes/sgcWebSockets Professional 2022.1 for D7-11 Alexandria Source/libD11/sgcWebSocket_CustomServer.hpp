// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_CustomServer.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_customserverHPP
#define Sgcwebsocket_customserverHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <IdHTTPServer.hpp>
#include <IdTCPServer.hpp>
#include <IdContext.hpp>
#include <IdSSL.hpp>
#include <IdCustomTCPServer.hpp>
#include <IdComponent.hpp>
#include <IdBaseComponent.hpp>
#include <IdCustomHTTPServer.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_customserver
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSCustomServer;
class DELPHICLASS TsgcWSHTTPCustomServer;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSQuerySSLPortEvent)(int aPort, bool &aSSL);

class PASCALIMPLEMENTATION TsgcWSCustomServer : public Idtcpserver::TIdTCPServer
{
	typedef Idtcpserver::TIdTCPServer inherited;
	
private:
	System::Classes::TNotifyEvent FOnStartup;
	System::Classes::TNotifyEvent FOnShutdown;
	
protected:
	virtual void __fastcall Startup();
	virtual void __fastcall Shutdown();
	
public:
	__property System::Classes::TNotifyEvent OnStartup = {read=FOnStartup, write=FOnStartup};
	__property System::Classes::TNotifyEvent OnShutdown = {read=FOnShutdown, write=FOnShutdown};
	
protected:
	TsgcWSQuerySSLPortEvent FOnQuerySSLPort;
	virtual void __fastcall DoConnect(Idcontext::TIdContext* AContext);
	virtual bool __fastcall DoQuerySSLPort(int aPort);
	
public:
	__property TsgcWSQuerySSLPortEvent OnQuerySSLPort = {read=FOnQuerySSLPort, write=FOnQuerySSLPort};
public:
	/* TIdCustomTCPServer.Destroy */ inline __fastcall virtual ~TsgcWSCustomServer() { }
	
public:
	/* TIdBaseComponent.Create */ inline __fastcall TsgcWSCustomServer(System::Classes::TComponent* AOwner)/* overload */ : Idtcpserver::TIdTCPServer(AOwner) { }
	
public:
	/* TIdInitializerComponent.Create */ inline __fastcall TsgcWSCustomServer()/* overload */ : Idtcpserver::TIdTCPServer() { }
	
};


class PASCALIMPLEMENTATION TsgcWSHTTPCustomServer : public Idhttpserver::TIdHTTPServer
{
	typedef Idhttpserver::TIdHTTPServer inherited;
	
private:
	System::Classes::TNotifyEvent FOnStartup;
	System::Classes::TNotifyEvent FOnShutdown;
	
protected:
	virtual void __fastcall Startup();
	virtual void __fastcall Shutdown();
	
public:
	__property System::Classes::TNotifyEvent OnStartup = {read=FOnStartup, write=FOnStartup};
	__property System::Classes::TNotifyEvent OnShutdown = {read=FOnShutdown, write=FOnShutdown};
	
protected:
	virtual bool __fastcall DoExecute(Idcontext::TIdContext* AContext);
	
public:
	void __fastcall DoExecuteHTTP(Idcontext::TIdContext* AContext);
	
protected:
	TsgcWSQuerySSLPortEvent FOnQueryHTTPSSLPort;
	virtual void __fastcall DoConnect(Idcontext::TIdContext* AContext);
	HIDESBASE virtual bool __fastcall DoQuerySSLPort(int aPort);
	
public:
	__property OnExecute;
	__property TsgcWSQuerySSLPortEvent OnQuerySSLPort = {read=FOnQueryHTTPSSLPort, write=FOnQueryHTTPSSLPort};
public:
	/* TIdCustomHTTPServer.Create */ inline __fastcall TsgcWSHTTPCustomServer(System::Classes::TComponent* AOwner)/* overload */ : Idhttpserver::TIdHTTPServer(AOwner) { }
	/* TIdCustomHTTPServer.Destroy */ inline __fastcall virtual ~TsgcWSHTTPCustomServer() { }
	
public:
	/* TIdInitializerComponent.Create */ inline __fastcall TsgcWSHTTPCustomServer()/* overload */ : Idhttpserver::TIdHTTPServer() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_customserver */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_CUSTOMSERVER)
using namespace Sgcwebsocket_customserver;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_customserverHPP
