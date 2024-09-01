// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_WAMP2_Message.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_wamp2_messageHPP
#define Sgcwebsocket_protocol_wamp2_messageHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <sgcWebSocket_Protocol_Base_Message.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_wamp2_message
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSMessageWAMP2;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcWSMessageWAMP2 : public Sgcwebsocket_protocol_base_message::TsgcWSMessage_Base
{
	typedef Sgcwebsocket_protocol_base_message::TsgcWSMessage_Base inherited;
	
private:
	int __fastcall DoReadMethodId();
	__int64 __fastcall GetRequestId();
	
public:
	__fastcall virtual TsgcWSMessageWAMP2(System::Classes::TComponent* aOwner);
	virtual void __fastcall Clear(bool aForceClear = false);
	virtual void __fastcall Read(const System::UnicodeString aMessage);
	virtual System::UnicodeString __fastcall Write();
	
private:
	System::UnicodeString FArguments;
	System::UnicodeString FArgumentsKw;
	System::UnicodeString FAuthentication;
	__int64 FRequestId;
	__int64 FSubscriptionId;
	int FErrorMethodId;
	__int64 FErrorRequestId;
	System::UnicodeString FErrorDetails;
	System::UnicodeString FErrorURI;
	System::UnicodeString FDetails;
	__int64 FSessionId;
	System::UnicodeString FTopicUri;
	int FMethodId;
	System::UnicodeString FOptions;
	__int64 FPublicationId;
	System::UnicodeString FReason;
	
public:
	__property System::UnicodeString Arguments = {read=FArguments, write=FArguments};
	__property System::UnicodeString ArgumentsKw = {read=FArgumentsKw, write=FArgumentsKw};
	__property System::UnicodeString Authentication = {read=FAuthentication, write=FAuthentication};
	__property __int64 RequestId = {read=FRequestId, write=FRequestId};
	__property __int64 SubscriptionId = {read=FSubscriptionId, write=FSubscriptionId};
	__property int ErrorMethodId = {read=FErrorMethodId, write=FErrorMethodId, nodefault};
	__property __int64 ErrorRequestId = {read=FErrorRequestId, write=FErrorRequestId};
	__property System::UnicodeString ErrorDetails = {read=FErrorDetails, write=FErrorDetails};
	__property System::UnicodeString ErrorURI = {read=FErrorURI, write=FErrorURI};
	__property System::UnicodeString Details = {read=FDetails, write=FDetails};
	__property __int64 SessionId = {read=FSessionId, write=FSessionId};
	__property System::UnicodeString TopicUri = {read=FTopicUri, write=FTopicUri};
	__property int MethodId = {read=FMethodId, write=FMethodId, nodefault};
	__property System::UnicodeString Options = {read=FOptions, write=FOptions};
	__property __int64 PublicationId = {read=FPublicationId, write=FPublicationId};
	__property System::UnicodeString Reason = {read=FReason, write=FReason};
public:
	/* TsgcWSMessage_Base.Destroy */ inline __fastcall virtual ~TsgcWSMessageWAMP2() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_wamp2_message */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_WAMP2_MESSAGE)
using namespace Sgcwebsocket_protocol_wamp2_message;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_wamp2_messageHPP
