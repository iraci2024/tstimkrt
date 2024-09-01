// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_Base_Client.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_base_clientHPP
#define Sgcwebsocket_protocol_base_clientHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.Contnrs.hpp>
#include <Winapi.Windows.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Protocol_Broker_Client.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Protocol_Base_Message.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_base_client
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSProtocol_Client_Base;
class DELPHICLASS TsgcWSProtocol_Subscription_Client_Base;
class DELPHICLASS TsgcWSAckClient_Options;
class DELPHICLASS TsgcWSProtocol_Acknowledgment_Client_Base;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcWSProtocol_Client_Base : public Sgcwebsocket_classes::TsgcWSProtocol_Client
{
	typedef Sgcwebsocket_classes::TsgcWSProtocol_Client inherited;
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall Loaded();
	
private:
	Sgcwebsocket_protocol_broker_client::TsgcWSProtocol_Broker_Client* FBroker;
	
protected:
	virtual void __fastcall SetBroker(Sgcwebsocket_protocol_broker_client::TsgcWSProtocol_Broker_Client* const Value);
	
public:
	__property Sgcwebsocket_protocol_broker_client::TsgcWSProtocol_Broker_Client* Broker = {read=FBroker, write=SetBroker};
	
protected:
	Sgcwebsocket_classes::TsgcWSComponent_WSClient* __fastcall GetWSClient();
	
public:
	__fastcall virtual TsgcWSProtocol_Client_Base(System::Classes::TComponent* aOwner);
	virtual void __fastcall WriteData(const System::UnicodeString aText)/* overload */;
	virtual void __fastcall WriteData(System::Classes::TStream* aStream, int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0))/* overload */;
public:
	/* TsgcWSProtocol_Client.Destroy */ inline __fastcall virtual ~TsgcWSProtocol_Client_Base() { }
	
};


class PASCALIMPLEMENTATION TsgcWSProtocol_Subscription_Client_Base : public TsgcWSProtocol_Client_Base
{
	typedef TsgcWSProtocol_Client_Base inherited;
	
protected:
	virtual void __fastcall DoEventDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int aCode);
	
private:
	Sgcwebsocket_classes::TsgcWSSubscriptionEvent FOnSubscription;
	Sgcwebsocket_classes::TsgcWSSubscriptionEvent FOnUnSubscription;
	Sgcwebsocket_classes::TsgcWSObjectList* FNotifySubscription;
	Sgcwebsocket_classes::TsgcWSObjectList* FNotifyUnSubscription;
	bool FAsyncSubscription;
	bool FAsyncUnSubscription;
	void __fastcall DoAsyncSubscription();
	void __fastcall DoAsyncUnSubscription();
	Sgcwebsocket_classes::TsgcWSObjectList* __fastcall GetNotifySubscription();
	Sgcwebsocket_classes::TsgcWSObjectList* __fastcall GetNotifyUnSubscription();
	__property Sgcwebsocket_classes::TsgcWSObjectList* NotifySubscription = {read=GetNotifySubscription};
	__property Sgcwebsocket_classes::TsgcWSObjectList* NotifyUnSubscription = {read=GetNotifyUnSubscription};
	
protected:
	virtual void __fastcall DoNotifySubscription(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoNotifyUnSubscription(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	void __fastcall DoEventSubscription(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString aSubscription);
	void __fastcall DoEventUnSubscription(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString aSubscription);
	
public:
	__fastcall virtual ~TsgcWSProtocol_Subscription_Client_Base();
	
private:
	System::Classes::TStringList* FSubscriptions;
	System::Classes::TStringList* __fastcall GetSubscriptions();
	
protected:
	virtual void __fastcall DoSubscribe(const System::UnicodeString aChannel);
	virtual void __fastcall DoUnSubscribe(const System::UnicodeString aChannel);
	virtual void __fastcall DoUnSubscriptions(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	
public:
	__property System::Classes::TStringList* Subscriptions = {read=GetSubscriptions, write=FSubscriptions};
	virtual void __fastcall Subscribe(const System::UnicodeString aChannel, const System::UnicodeString aGuid = System::UnicodeString()) = 0 ;
	virtual void __fastcall UnSubscribe(const System::UnicodeString aChannel, const System::UnicodeString aGuid = System::UnicodeString()) = 0 ;
	__property Sgcwebsocket_classes::TsgcWSSubscriptionEvent OnSubscription = {read=FOnSubscription, write=FOnSubscription};
	__property Sgcwebsocket_classes::TsgcWSSubscriptionEvent OnUnSubscription = {read=FOnUnSubscription, write=FOnUnSubscription};
public:
	/* TsgcWSProtocol_Client_Base.Create */ inline __fastcall virtual TsgcWSProtocol_Subscription_Client_Base(System::Classes::TComponent* aOwner) : TsgcWSProtocol_Client_Base(aOwner) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAckClient_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	int FInterval;
	int FTimeout;
	
public:
	__fastcall virtual TsgcWSAckClient_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property int Interval = {read=FInterval, write=FInterval, nodefault};
	__property int Timeout = {read=FTimeout, write=FTimeout, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSAckClient_Options() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSProtocol_Acknowledgment_Client_Base : public TsgcWSProtocol_Client_Base
{
	typedef TsgcWSProtocol_Client_Base inherited;
	
private:
	Sgcwebsocket_classes::TsgcWSConnection* FWSConnection;
	
protected:
	virtual void __fastcall DoEventConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int Code);
	virtual void __fastcall DoInitialize(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoFinalize(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	
private:
	Sgcwebsocket_protocol_base_message::TsgcWSAckMessageList* FAcknowledgmentList;
	Sgcwebsocket_protocol_base_message::TsgcWSAckMessageList* __fastcall GetAcknowledgmentList();
	
protected:
	__property Sgcwebsocket_protocol_base_message::TsgcWSAckMessageList* AcknowledgmentList = {read=GetAcknowledgmentList, write=FAcknowledgmentList};
	
private:
	TsgcWSAckClient_Options* FAcknowledgment;
	void __fastcall SetAcknowledgment(TsgcWSAckClient_Options* const Value);
	
protected:
	__property TsgcWSAckClient_Options* Acknowledgment = {read=FAcknowledgment, write=SetAcknowledgment};
	virtual void __fastcall OnExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	virtual void __fastcall OnAcknowledgmentMessageEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString aText) = 0 ;
	
public:
	__fastcall virtual TsgcWSProtocol_Acknowledgment_Client_Base(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSProtocol_Acknowledgment_Client_Base();
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_base_client */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_BASE_CLIENT)
using namespace Sgcwebsocket_protocol_base_client;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_base_clientHPP
