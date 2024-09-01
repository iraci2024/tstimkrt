// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_STOMP_Message.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_stomp_messageHPP
#define Sgcwebsocket_protocol_stomp_messageHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <sgcSTOMP.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_stomp_message
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSSTOMPMessage;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcWSSTOMPMessage : public Sgcstomp::TsgcSTOMP
{
	typedef Sgcstomp::TsgcSTOMP inherited;
	
private:
	unsigned FThreadId;
	System::UnicodeString FConnectionId;
	
public:
	__property unsigned ThreadId = {read=FThreadId, write=FThreadId, nodefault};
	__property System::UnicodeString ConnectionId = {read=FConnectionId, write=FConnectionId};
public:
	/* TsgcSTOMP.Create */ inline __fastcall virtual TsgcWSSTOMPMessage(System::Classes::TComponent* aOwner) : Sgcstomp::TsgcSTOMP(aOwner) { }
	/* TsgcSTOMP.Destroy */ inline __fastcall virtual ~TsgcWSSTOMPMessage() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_stomp_message */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_STOMP_MESSAGE)
using namespace Sgcwebsocket_protocol_stomp_message;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_stomp_messageHPP
