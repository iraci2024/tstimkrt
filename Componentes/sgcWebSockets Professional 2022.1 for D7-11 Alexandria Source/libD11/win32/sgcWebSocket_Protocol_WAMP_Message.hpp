// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_WAMP_Message.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_wamp_messageHPP
#define Sgcwebsocket_protocol_wamp_messageHPP

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

namespace Sgcwebsocket_protocol_wamp_message
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSMessageWAMP;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcWSMessageWAMP : public Sgcwebsocket_protocol_base_message::TsgcWSMessage_Base
{
	typedef Sgcwebsocket_protocol_base_message::TsgcWSMessage_Base inherited;
	
protected:
	int __fastcall DoReadTypeId();
	
public:
	__fastcall virtual TsgcWSMessageWAMP(System::Classes::TComponent* aOwner);
	virtual void __fastcall Clear(bool aForceClear = false);
	virtual void __fastcall Read(const System::UnicodeString aMessage);
	virtual System::UnicodeString __fastcall Write();
	
private:
	System::UnicodeString FArguments;
	System::UnicodeString FCallId;
	System::UnicodeString FCallResult;
	System::UnicodeString FEligible;
	System::UnicodeString FErrorDesc;
	System::UnicodeString FErrorDetails;
	System::UnicodeString FErrorURI;
	System::UnicodeString FEvent;
	System::UnicodeString FExclude;
	System::UnicodeString FPrefix;
	System::UnicodeString FPrefixURI;
	System::UnicodeString FProcUri;
	System::UnicodeString FSessionId;
	System::UnicodeString FTopicUri;
	int FTypeId;
	System::UnicodeString FProtocolVersion;
	System::UnicodeString FServerIdent;
	
public:
	__property System::UnicodeString Arguments = {read=FArguments, write=FArguments};
	__property System::UnicodeString CallId = {read=FCallId, write=FCallId};
	__property System::UnicodeString CallResult = {read=FCallResult, write=FCallResult};
	__property System::UnicodeString Eligible = {read=FEligible, write=FEligible};
	__property System::UnicodeString ErrorDesc = {read=FErrorDesc, write=FErrorDesc};
	__property System::UnicodeString ErrorDetails = {read=FErrorDetails, write=FErrorDetails};
	__property System::UnicodeString ErrorURI = {read=FErrorURI, write=FErrorURI};
	__property System::UnicodeString Event = {read=FEvent, write=FEvent};
	__property System::UnicodeString Exclude = {read=FExclude, write=FExclude};
	__property System::UnicodeString Prefix = {read=FPrefix, write=FPrefix};
	__property System::UnicodeString PrefixURI = {read=FPrefixURI, write=FPrefixURI};
	__property System::UnicodeString ProcUri = {read=FProcUri, write=FProcUri};
	__property System::UnicodeString SessionId = {read=FSessionId, write=FSessionId};
	__property System::UnicodeString TopicUri = {read=FTopicUri, write=FTopicUri};
	__property int TypeId = {read=FTypeId, write=FTypeId, nodefault};
	__property System::UnicodeString ProtocolVersion = {read=FProtocolVersion, write=FProtocolVersion};
	__property System::UnicodeString ServerIdent = {read=FServerIdent, write=FServerIdent};
public:
	/* TsgcWSMessage_Base.Destroy */ inline __fastcall virtual ~TsgcWSMessageWAMP() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_wamp_message */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_WAMP_MESSAGE)
using namespace Sgcwebsocket_protocol_wamp_message;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_wamp_messageHPP
