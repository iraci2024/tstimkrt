// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_LoadBalancer_Client.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_loadbalancer_clientHPP
#define Sgcwebsocket_loadbalancer_clientHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <sgcWebSocket_Client.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_LoadBalancer_Message.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Classes_Indy.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_loadbalancer_client
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSLoadBalancerClient;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TOnWriteDataText)(const System::UnicodeString aGuid, const System::UnicodeString aText);

typedef void __fastcall (__closure *TOnWriteDataBinary)(const System::UnicodeString aGuid, System::Classes::TStream* aStream);

typedef void __fastcall (__closure *TOnBroadCastText)(const System::UnicodeString aText, const System::UnicodeString aChannel, const System::UnicodeString aProtocol, const System::UnicodeString aExclude, const System::UnicodeString aInclude);

typedef void __fastcall (__closure *TOnBroadCastBinary)(System::Classes::TStream* aStream, const System::UnicodeString aChannel, const System::UnicodeString aProtocol, const System::UnicodeString aExclude, const System::UnicodeString aInclude, int aSize, Sgcwebsocket_types::TwsStreaming aStreaming);

class PASCALIMPLEMENTATION TsgcWSLoadBalancerClient : public Sgcwebsocket_client::TsgcWSClient
{
	typedef Sgcwebsocket_client::TsgcWSClient inherited;
	
__published:
	virtual void __fastcall OnClientMessageEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual void __fastcall OnClientBinaryEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::Classes::TMemoryStream* const aStream);
	virtual void __fastcall OnClientFragmentedEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::Classes::TMemoryStream* const aData, const Sgcwebsocket_types::TOpcode aOpCode, const bool aContinuation);
	
private:
	Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerMessage* FMessageWriteText;
	Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerMessage* FMessageReadText;
	
protected:
	Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerMessage* __fastcall GetMessageWriteText();
	Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerMessage* __fastcall GetMessageReadText();
	
public:
	__property Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerMessage* MessageWriteText = {read=GetMessageWriteText, write=FMessageWriteText};
	__property Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerMessage* MessageReadText = {read=GetMessageReadText, write=FMessageReadText};
	
private:
	Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerBinary* FMessageWriteBinary;
	Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerBinary* FMessageReadBinary;
	
protected:
	Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerBinary* __fastcall GetMessageWriteBinary();
	Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerBinary* __fastcall GetMessageReadBinary();
	
public:
	__property Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerBinary* MessageWriteBinary = {read=GetMessageWriteBinary, write=FMessageWriteBinary};
	__property Sgcwebsocket_loadbalancer_message::TsgcWSLoadBalancerBinary* MessageReadBinary = {read=GetMessageReadBinary, write=FMessageReadBinary};
	__fastcall virtual TsgcWSLoadBalancerClient(System::Classes::TComponent* aOwner);
	HIDESBASE void __fastcall WriteData(const System::UnicodeString aGuid, const System::UnicodeString aMessage)/* overload */;
	HIDESBASE void __fastcall WriteData(const System::UnicodeString aGuid, System::Classes::TStream* aStream, int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0))/* overload */;
	void __fastcall Broadcast(const System::UnicodeString aMessage, const System::UnicodeString aChannel = System::UnicodeString(), const System::UnicodeString aProtocol = System::UnicodeString(), const System::UnicodeString Exclude = System::UnicodeString(), const System::UnicodeString Include = System::UnicodeString())/* overload */;
	void __fastcall Broadcast(System::Classes::TStream* aStream, const System::UnicodeString aChannel = System::UnicodeString(), const System::UnicodeString aProtocol = System::UnicodeString(), const System::UnicodeString Exclude = System::UnicodeString(), const System::UnicodeString Include = System::UnicodeString(), const int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0))/* overload */;
	
private:
	TOnWriteDataText FOnWriteDataText;
	TOnBroadCastText FOnBroadCastText;
	TOnBroadCastBinary FOnBroadCastBinary;
	TOnWriteDataBinary FOnWriteDataBinary;
	
public:
	__property TOnWriteDataText OnWriteDataText = {read=FOnWriteDataText, write=FOnWriteDataText};
	__property TOnBroadCastText OnBroadCastText = {read=FOnBroadCastText, write=FOnBroadCastText};
	__property TOnBroadCastBinary OnBroadCastBinary = {read=FOnBroadCastBinary, write=FOnBroadCastBinary};
	__property TOnWriteDataBinary OnWriteDataBinary = {read=FOnWriteDataBinary, write=FOnWriteDataBinary};
	
__published:
	__property OnConnect;
	__property OnDisconnect;
public:
	/* TsgcWSClient.Destroy */ inline __fastcall virtual ~TsgcWSLoadBalancerClient() { }
	
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  WriteData(const System::UnicodeString aText, const int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0)){ Sgcwebsocket_client::TsgcWSClient::WriteData(aText, aSize, aStreaming); }
	inline void __fastcall  WriteData(System::Classes::TStream* const aStream, const int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0)){ Sgcwebsocket_client::TsgcWSClient::WriteData(aStream, aSize, aStreaming); }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_loadbalancer_client */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_LOADBALANCER_CLIENT)
using namespace Sgcwebsocket_loadbalancer_client;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_loadbalancer_clientHPP
