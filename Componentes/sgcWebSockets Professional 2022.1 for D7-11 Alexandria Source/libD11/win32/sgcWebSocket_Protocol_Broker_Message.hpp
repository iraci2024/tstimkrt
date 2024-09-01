// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_Broker_Message.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_broker_messageHPP
#define Sgcwebsocket_protocol_broker_messageHPP

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

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_broker_message
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSBrokerMessage;
class DELPHICLASS TsgcWSBrokerBinary;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcWSBrokerMessage : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::UnicodeString FConnectionId;
	System::UnicodeString FID;
	System::UnicodeString FText;
	unsigned FThreadId;
	
public:
	void __fastcall Read(const System::UnicodeString aMessage);
	System::UnicodeString __fastcall Write();
	__property System::UnicodeString ID = {read=FID, write=FID};
	__property System::UnicodeString Text = {read=FText, write=FText};
	__property System::UnicodeString ConnectionId = {read=FConnectionId, write=FConnectionId};
	__property unsigned ThreadId = {read=FThreadId, write=FThreadId, nodefault};
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcWSBrokerMessage(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcWSBrokerMessage() { }
	
};


class PASCALIMPLEMENTATION TsgcWSBrokerBinary : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	System::UnicodeString FConnectionId;
	System::UnicodeString FID;
	unsigned FThreadId;
	
public:
	void __fastcall Read(System::Classes::TMemoryStream* &aStream);
	void __fastcall Write(System::Classes::TMemoryStream* &aData);
	__property System::UnicodeString ID = {read=FID, write=FID};
	__property System::UnicodeString ConnectionId = {read=FConnectionId, write=FConnectionId};
	__property unsigned ThreadId = {read=FThreadId, write=FThreadId, nodefault};
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcWSBrokerBinary(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcWSBrokerBinary() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_broker_message */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_BROKER_MESSAGE)
using namespace Sgcwebsocket_protocol_broker_message;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_broker_messageHPP
