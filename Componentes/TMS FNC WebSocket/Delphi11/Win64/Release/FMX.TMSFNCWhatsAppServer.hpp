// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'FMX.TMSFNCWhatsAppServer.pas' rev: 35.00 (Windows)

#ifndef Fmx_TmsfncwhatsappserverHPP
#define Fmx_TmsfncwhatsappserverHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <FMX.TMSFNCWebSocketServer.hpp>
#include <IdContext.hpp>
#include <IdCustomHTTPServer.hpp>
#include <FMX.TMSFNCTypes.hpp>
#include <FMX.TMSFNCCustomComponent.hpp>
#include <FMX.Controls.hpp>
#include <FMX.Types.hpp>
#include <FMX.TMSFNCWebSocketCommon.hpp>

//-- user supplied -----------------------------------------------------------

namespace Fmx
{
namespace Tmsfncwhatsappserver
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TTMSFNCCustomWhatsAppServer;
class DELPHICLASS TTMSFNCWhatsAppServer;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TTMSFNCWhatsAppServerMessageEvent)(System::TObject* Sender, System::UnicodeString AMessage, bool &ABroadcast);

class PASCALIMPLEMENTATION TTMSFNCCustomWhatsAppServer : public Fmx::Tmsfncwebsocketserver::TTMSFNCCustomWebSocketServer
{
	typedef Fmx::Tmsfncwebsocketserver::TTMSFNCCustomWebSocketServer inherited;
	
private:
	System::UnicodeString FVerifyToken;
	TTMSFNCWhatsAppServerMessageEvent FOnMessage;
	
protected:
	virtual NativeUInt __fastcall GetInstance();
	virtual System::UnicodeString __fastcall GetDocURL();
	virtual void __fastcall DoHTTPRequest(Idcontext::TIdContext* AContext, Idcustomhttpserver::TIdHTTPRequestInfo* ARequestInfo, Idcustomhttpserver::TIdHTTPResponseInfo* AResponseInfo);
	virtual void __fastcall DoRawMessage(System::UnicodeString AMessage, bool &ABroadcast);
	__property System::UnicodeString VerifyToken = {read=FVerifyToken, write=FVerifyToken};
	__property TTMSFNCWhatsAppServerMessageEvent OnRawMessage = {read=FOnMessage, write=FOnMessage};
	
public:
	__fastcall virtual TTMSFNCCustomWhatsAppServer(System::Classes::TComponent* AOwner)/* overload */;
public:
	/* TTMSFNCCustomWebSocketServer.Create */ inline __fastcall virtual TTMSFNCCustomWhatsAppServer()/* overload */ : Fmx::Tmsfncwebsocketserver::TTMSFNCCustomWebSocketServer() { }
	/* TTMSFNCCustomWebSocketServer.Create */ inline __fastcall virtual TTMSFNCCustomWhatsAppServer(System::Classes::TComponent* AOwner, int APort)/* overload */ : Fmx::Tmsfncwebsocketserver::TTMSFNCCustomWebSocketServer(AOwner, APort) { }
	/* TTMSFNCCustomWebSocketServer.Destroy */ inline __fastcall virtual ~TTMSFNCCustomWhatsAppServer() { }
	
};


class PASCALIMPLEMENTATION TTMSFNCWhatsAppServer : public TTMSFNCCustomWhatsAppServer
{
	typedef TTMSFNCCustomWhatsAppServer inherited;
	
__published:
	__property AutoSyncEvents;
	__property Port = {default=8888};
	__property PathName = {default=0};
	__property Active = {default=0};
	__property VerifyToken = {default=0};
	__property CertificateFile = {default=0};
	__property CertificateKeyfile = {default=0};
	__property RootCertificateFile = {default=0};
	__property OnConnect;
	__property OnAllow;
	__property OnDisconnect;
	__property OnPing;
	__property OnPong;
	__property OnClose;
	__property OnRawMessage;
	__property OnGetSSLPassword;
	__property OnCommandGet;
	__property OnMessageReceived;
	__property OnBinaryDataReceived;
public:
	/* TTMSFNCCustomWhatsAppServer.Create */ inline __fastcall virtual TTMSFNCWhatsAppServer(System::Classes::TComponent* AOwner)/* overload */ : TTMSFNCCustomWhatsAppServer(AOwner) { }
	
public:
	/* TTMSFNCCustomWebSocketServer.Create */ inline __fastcall virtual TTMSFNCWhatsAppServer()/* overload */ : TTMSFNCCustomWhatsAppServer() { }
	/* TTMSFNCCustomWebSocketServer.Create */ inline __fastcall virtual TTMSFNCWhatsAppServer(System::Classes::TComponent* AOwner, int APort)/* overload */ : TTMSFNCCustomWhatsAppServer(AOwner, APort) { }
	/* TTMSFNCCustomWebSocketServer.Destroy */ inline __fastcall virtual ~TTMSFNCWhatsAppServer() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Tmsfncwhatsappserver */
}	/* namespace Fmx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX_TMSFNCWHATSAPPSERVER)
using namespace Fmx::Tmsfncwhatsappserver;
#endif
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_FMX)
using namespace Fmx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Fmx_TmsfncwhatsappserverHPP
