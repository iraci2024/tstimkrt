// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Classes_WinHTTP.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_classes_winhttpHPP
#define Sgcwebsocket_classes_winhttpHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Helpers.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Const.hpp>
#include <sgcWebSocket_WinAPI.hpp>
#include <sgcBase_Helpers.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_classes_winhttp
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSConnection_WinHTTP;
class DELPHICLASS TsgcWSComponent_Client_WinHTTP;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcWSConnection_WinHTTP : public Sgcwebsocket_classes::TsgcWSConnection
{
	typedef Sgcwebsocket_classes::TsgcWSConnection inherited;
	
protected:
	virtual bool __fastcall GetActive();
	
private:
	void *FWSSession;
	
protected:
	virtual System::UnicodeString __fastcall GetIP();
	virtual int __fastcall GetPort();
	virtual System::UnicodeString __fastcall GetLocalIP();
	virtual int __fastcall GetLocalPort();
	virtual Sgcwebsocket_types::TwsIPVersion __fastcall GetIPVersion();
	virtual System::UnicodeString __fastcall GetURL();
	
public:
	__property void * WSSession = {read=FWSSession, write=FWSSession};
	
protected:
	virtual void __fastcall DoDisconnectIfActive();
	
private:
	bool FDoDisconnect;
	void __fastcall DoDisconnect(const System::UnicodeString aError = System::UnicodeString(), int aCloseCode = 0x3e8);
	
public:
	virtual void __fastcall Disconnect()/* overload */;
	virtual void __fastcall Disconnect(const int aCloseCode)/* overload */;
	
protected:
	virtual void __fastcall DoClose(int aCloseCode = 0x3e8);
	
public:
	virtual void __fastcall Close()/* overload */;
	virtual void __fastcall Close(const int aCloseCode)/* overload */;
	
protected:
	virtual void __fastcall DoPing(const System::UnicodeString aText);
	
private:
	void __fastcall DoWriteData_RFC6455(const System::UnicodeString aText, Sgcwebsocket_types::TOpcode aOpCode, bool aFragmented = false)/* overload */;
	void __fastcall DoWriteData_RFC6455(System::Classes::TStream* aStream, Sgcwebsocket_types::TOpcode aOpCode, bool aFragmented = false)/* overload */;
	void __fastcall DoWriteData_RFC6455_Fragmented(const System::UnicodeString aText, Sgcwebsocket_types::TOpcode aOpCode, int aSize, const bool aSkipFirst = false)/* overload */;
	void __fastcall DoWriteData_RFC6455_Fragmented(System::Classes::TStream* aStream, Sgcwebsocket_types::TOpcode aOpCode, int aSize, const bool aSkipFirst = false)/* overload */;
	
protected:
	virtual void __fastcall DoWriteData(const System::UnicodeString aText, const int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0))/* overload */;
	virtual void __fastcall DoWriteData(System::Classes::TStream* const aStream, const int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0))/* overload */;
	
public:
	__fastcall virtual TsgcWSConnection_WinHTTP();
	__fastcall virtual ~TsgcWSConnection_WinHTTP();
};


class PASCALIMPLEMENTATION TsgcWSComponent_Client_WinHTTP : public Sgcwebsocket_classes::TsgcWSComponent_WSClient
{
	typedef Sgcwebsocket_classes::TsgcWSComponent_WSClient inherited;
	
public:
	/* TsgcWSComponent_WSClient.Create */ inline __fastcall virtual TsgcWSComponent_Client_WinHTTP(System::Classes::TComponent* aOwner) : Sgcwebsocket_classes::TsgcWSComponent_WSClient(aOwner) { }
	/* TsgcWSComponent_WSClient.Destroy */ inline __fastcall virtual ~TsgcWSComponent_Client_WinHTTP() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_classes_winhttp */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_CLASSES_WINHTTP)
using namespace Sgcwebsocket_classes_winhttp;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_classes_winhttpHPP
