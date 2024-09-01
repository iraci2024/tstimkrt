// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_WebRTC_Server.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_webrtc_serverHPP
#define Sgcwebsocket_protocol_webrtc_serverHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <sgcJSON.hpp>
#include <sgcWebSocket_Protocol_WebRTC_Message.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Protocol_Base_Server.hpp>
#include <sgcWebSocket_HTTPResponse.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_webrtc_server
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSWebRTC_Options;
class DELPHICLASS TsgcWSProtocol_WebRTC_Server;
class DELPHICLASS TsgcWSProtocol_JS_WebRTC;
class DELPHICLASS TsgcWSProtocol_HTML_WebRTC;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSWebRTC_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::Classes::TStringList* FIceServers;
	void __fastcall SetIceServers(System::Classes::TStringList* const Value);
	
public:
	__fastcall TsgcWSWebRTC_Options();
	__fastcall virtual ~TsgcWSWebRTC_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::Classes::TStringList* IceServers = {read=FIceServers, write=SetIceServers};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSProtocol_WebRTC_Server : public Sgcwebsocket_protocol_base_server::TsgcWSProtocol_Subscription_Server_Base
{
	typedef Sgcwebsocket_protocol_base_server::TsgcWSProtocol_Subscription_Server_Base inherited;
	
private:
	Sgcwebsocket_protocol_webrtc_message::TsgcWSMessageWebRTC* FWSMessageWebRTC;
	Sgcwebsocket_protocol_webrtc_message::TsgcWSMessageWebRTC* __fastcall GetWSMessageWebRTC();
	
protected:
	__property Sgcwebsocket_protocol_webrtc_message::TsgcWSMessageWebRTC* WSMessageWebRTC = {read=GetWSMessageWebRTC, write=FWSMessageWebRTC};
	virtual void __fastcall DoEventConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual void __fastcall DoEventDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int Code);
	
private:
	System::Classes::TStringList* FWebRTCSubscriptions;
	
protected:
	void __fastcall WebRTCSubscription_Add(System::UnicodeString aSubscription);
	void __fastcall WebRTCSubscription_Clear(System::UnicodeString aSubscription);
	int __fastcall WebRTCSubscription_Count(System::UnicodeString aSubscription);
	System::Classes::TStringList* __fastcall GetWebRTCSubscriptions();
	
public:
	__property System::Classes::TStringList* WebRTCSubscriptions = {read=GetWebRTCSubscriptions, write=FWebRTCSubscriptions};
	
private:
	TsgcWSWebRTC_Options* FWebRTC;
	void __fastcall SetWebRTC(TsgcWSWebRTC_Options* const Value);
	
public:
	__property TsgcWSWebRTC_Options* WebRTC = {read=FWebRTC, write=SetWebRTC};
	
protected:
	virtual void __fastcall DoBroadCast(Sgcwebsocket_protocol_webrtc_message::TsgcWSMessageWebRTC* aMessage, System::UnicodeString aChannel, System::UnicodeString Exclude = System::UnicodeString());
	virtual void __fastcall BroadcastWebRTC(System::UnicodeString aMessage, System::UnicodeString aChannel = System::UnicodeString(), System::UnicodeString aGuid = System::UnicodeString());
	
public:
	__fastcall virtual TsgcWSProtocol_WebRTC_Server(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSProtocol_WebRTC_Server();
	virtual void __fastcall Broadcast(System::UnicodeString aMessage, System::UnicodeString aChannel = System::UnicodeString(), System::UnicodeString Exclude = System::UnicodeString(), System::UnicodeString Include = System::UnicodeString())/* overload */;
	virtual bool __fastcall WriteData(System::UnicodeString aGuid, System::UnicodeString aMessage)/* overload */;
	/* Hoisted overloads: */
	
protected:
	inline void __fastcall  Broadcast(System::Classes::TStream* aStream, System::UnicodeString aChannel = System::UnicodeString(), System::UnicodeString Exclude = System::UnicodeString(), System::UnicodeString Include = System::UnicodeString(), int aSize = 0x0, Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0)){ Sgcwebsocket_protocol_base_server::TsgcWSProtocol_Server_Base::Broadcast(aStream, aChannel, Exclude, Include, aSize, aStreaming); }
	inline bool __fastcall  WriteData(System::UnicodeString aGuid, System::Classes::TStream* aStream, int aSize = 0x0, Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0)){ return Sgcwebsocket_protocol_base_server::TsgcWSProtocol_Server_Base::WriteData(aGuid, aStream, aSize, aStreaming); }
	inline bool __fastcall  WriteData(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aMessage){ return Sgcwebsocket_protocol_base_server::TsgcWSProtocol_Server_Base::WriteData(aConnection, aMessage); }
	inline void __fastcall  WriteData(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::Classes::TStream* aStream){ Sgcwebsocket_protocol_base_server::TsgcWSProtocol_Server_Base::WriteData(aConnection, aStream); }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSProtocol_JS_WebRTC : public Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base
{
	typedef Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetResponse();
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetFileName();
public:
	/* TObject.Create */ inline __fastcall TsgcWSProtocol_JS_WebRTC() : Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSProtocol_JS_WebRTC() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSProtocol_HTML_WebRTC : public Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base
{
	typedef Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetResponse();
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetFileName();
public:
	/* TObject.Create */ inline __fastcall TsgcWSProtocol_HTML_WebRTC() : Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSProtocol_HTML_WebRTC() { }
	
};

#pragma pack(pop)

//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_webrtc_server */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_WEBRTC_SERVER)
using namespace Sgcwebsocket_protocol_webrtc_server;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_webrtc_serverHPP
