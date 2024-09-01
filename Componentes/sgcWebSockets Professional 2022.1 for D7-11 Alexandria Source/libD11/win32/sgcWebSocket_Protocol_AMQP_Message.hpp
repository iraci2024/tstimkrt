// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_AMQP_Message.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_amqp_messageHPP
#define Sgcwebsocket_protocol_amqp_messageHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <sgcAMQP_Client.hpp>
#include <sgcAMQP.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_amqp_message
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSAMQPMessage;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcWSAMQPMessage : public Sgcamqp_client::TsgcAMQP_Client
{
	typedef Sgcamqp_client::TsgcAMQP_Client inherited;
	
private:
	unsigned FThreadId;
	System::UnicodeString FConnectionId;
	
public:
	__property unsigned ThreadId = {read=FThreadId, write=FThreadId, nodefault};
	__property System::UnicodeString ConnectionId = {read=FConnectionId, write=FConnectionId};
public:
	/* TsgcAMQP.Create */ inline __fastcall virtual TsgcWSAMQPMessage(System::Classes::TComponent* aOwner) : Sgcamqp_client::TsgcAMQP_Client(aOwner) { }
	/* TsgcAMQP.Destroy */ inline __fastcall virtual ~TsgcWSAMQPMessage() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_amqp_message */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_AMQP_MESSAGE)
using namespace Sgcwebsocket_protocol_amqp_message;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_amqp_messageHPP
