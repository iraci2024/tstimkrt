// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcTCP_Classes_Indy.pas' rev: 35.00 (Windows)

#ifndef Sgctcp_classes_indyHPP
#define Sgctcp_classes_indyHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.SyncObjs.hpp>
#include <IdGlobal.hpp>
#include <IdSSL.hpp>
#include <IdTCPConnection.hpp>
#include <IdComponent.hpp>
#include <sgcTCP_Classes.hpp>
#include <sgcWebSocket_Classes_Indy.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcSocket_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgctcp_classes_indy
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcTCPConnection_Indy;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcTCPConnection_Indy : public Sgctcp_classes::TsgcTCPConnection
{
	typedef Sgctcp_classes::TsgcTCPConnection inherited;
	
private:
	Idtcpconnection::TIdTCPConnection* FTCPConnection;
	Idtcpconnection::TIdTCPConnection* __fastcall GetTCPConnection();
	void __fastcall SetTCPConnection(Idtcpconnection::TIdTCPConnection* const Value);
	
protected:
	__property Idtcpconnection::TIdTCPConnection* TCPConnection = {read=GetTCPConnection, write=SetTCPConnection};
	virtual bool __fastcall ExistsTCPConnection();
	virtual bool __fastcall GetActive();
	virtual System::UnicodeString __fastcall GetIP();
	virtual int __fastcall GetPort();
	virtual System::UnicodeString __fastcall GetLocalIP();
	virtual int __fastcall GetLocalPort();
	virtual Sgcwebsocket_types::TwsIPVersion __fastcall GetIPVersion();
	
private:
	virtual void __fastcall DoBeforeDisconnectEvent();
	
protected:
	virtual void __fastcall DoClearBuffer();
	
public:
	virtual void __fastcall DisconnectPeer(bool aCallOnDisconnectIfClosed = false);
	
protected:
	virtual void __fastcall DoWriteBytes(const Idglobal::TIdBytes aBuffer);
	virtual void __fastcall DoWriteLn(const System::UnicodeString aText = System::UnicodeString());
	virtual void __fastcall DoWriteBufferOpen();
	virtual void __fastcall DoWriteBufferFlush();
	virtual void __fastcall DoWriteBufferClose();
	virtual void __fastcall DoReadBytes(Idglobal::TIdBytes &aBuffer, int aCount);
	virtual int __fastcall GetReadBufferSize();
	
public:
	__fastcall virtual ~TsgcTCPConnection_Indy();
	
private:
	System::Classes::TNotifyEvent FOnBeforeDisconnect;
	
public:
	__property System::Classes::TNotifyEvent OnBeforeDisconnect = {read=FOnBeforeDisconnect, write=FOnBeforeDisconnect};
public:
	/* TsgcTCPConnection_Base.Create */ inline __fastcall virtual TsgcTCPConnection_Indy() : Sgctcp_classes::TsgcTCPConnection() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgctcp_classes_indy */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCTCP_CLASSES_INDY)
using namespace Sgctcp_classes_indy;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgctcp_classes_indyHPP
