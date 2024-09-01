// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcUDP_Classes_Indy.pas' rev: 35.00 (Windows)

#ifndef Sgcudp_classes_indyHPP
#define Sgcudp_classes_indyHPP

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
#include <IdSocketHandle.hpp>
#include <sgcUDP_Classes.hpp>
#include <sgcWebSocket_Classes_Indy.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcSocket_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcudp_classes_indy
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcUDPSocket_Indy;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcUDPSocket_Indy : public Sgcudp_classes::TsgcUDPSocket
{
	typedef Sgcudp_classes::TsgcUDPSocket inherited;
	
private:
	Idsockethandle::TIdSocketHandle* FSocket;
	
protected:
	__property Idsockethandle::TIdSocketHandle* Socket = {read=FSocket, write=FSocket};
	virtual bool __fastcall GetActive();
	virtual System::UnicodeString __fastcall GetIP();
	virtual int __fastcall GetPort();
	virtual System::UnicodeString __fastcall GetLocalIP();
	virtual int __fastcall GetLocalPort();
	virtual Sgcwebsocket_types::TwsIPVersion __fastcall GetIPVersion();
	
private:
	virtual void __fastcall DoBeforeDisconnectEvent();
	
public:
	virtual void __fastcall DisconnectPeer();
	
protected:
	virtual void __fastcall DoWriteBytes(const Idglobal::TIdBytes aBuffer);
	
public:
	__fastcall virtual ~TsgcUDPSocket_Indy();
	
private:
	System::Classes::TNotifyEvent FOnBeforeDisconnect;
	System::Classes::TNotifyEvent FOnRead;
	
public:
	__property System::Classes::TNotifyEvent OnBeforeDisconnect = {read=FOnBeforeDisconnect, write=FOnBeforeDisconnect};
	__property System::Classes::TNotifyEvent OnRead = {read=FOnRead, write=FOnRead};
public:
	/* TsgcSocketConnection_Base.Create */ inline __fastcall virtual TsgcUDPSocket_Indy() : Sgcudp_classes::TsgcUDPSocket() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcudp_classes_indy */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCUDP_CLASSES_INDY)
using namespace Sgcudp_classes_indy;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcudp_classes_indyHPP
