// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_Base_Server.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_base_serverHPP
#define Sgcwebsocket_protocol_base_serverHPP

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
#include <System.Contnrs.hpp>
#include <Winapi.Windows.hpp>
#include <IdContext.hpp>
#include <sgcWebSocket_Protocol_Base_Message.hpp>
#include <sgcWebSocket_HTTPResponse.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Protocol_Broker_Server.hpp>
#include <sgcWebSocket_Helpers.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_base_server
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSProtocol_JS_Base;
class DELPHICLASS TsgcWSProtocol_JS_Flash;
class DELPHICLASS TsgcWSProtocol_JS_EventSource;
class DELPHICLASS TsgcWSProtocol_HTML_Base;
class DELPHICLASS TsgcWSProtocol_Server_Base;
class DELPHICLASS TsgcWSProtocol_Subscription_Server_Base;
class DELPHICLASS TsgcWSAckServer_Options;
class DELPHICLASS TsgcWSProtocol_Acknowledgment_Server_Base;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSProtocol_JS_Base : public Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base
{
	typedef Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetResponse();
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetFileName();
public:
	/* TObject.Create */ inline __fastcall TsgcWSProtocol_JS_Base() : Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSProtocol_JS_Base() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSProtocol_JS_Flash : public Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base
{
	typedef Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetResponse();
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetFileName();
public:
	/* TObject.Create */ inline __fastcall TsgcWSProtocol_JS_Flash() : Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSProtocol_JS_Flash() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSProtocol_JS_EventSource : public Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base
{
	typedef Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetResponse();
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetFileName();
public:
	/* TObject.Create */ inline __fastcall TsgcWSProtocol_JS_EventSource() : Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSProtocol_JS_EventSource() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSProtocol_HTML_Base : public Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base
{
	typedef Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetResponse();
	
public:
	__classmethod virtual System::UnicodeString __fastcall GetFileName();
public:
	/* TObject.Create */ inline __fastcall TsgcWSProtocol_HTML_Base() : Sgcwebsocket_httpresponse::TsgcWSHTTPResponse_Base() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSProtocol_HTML_Base() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSProtocol_Server_Base : public Sgcwebsocket_classes::TsgcWSProtocol_Server
{
	typedef Sgcwebsocket_classes::TsgcWSProtocol_Server inherited;
	
protected:
	virtual void __fastcall Notification(System::Classes::TComponent* AComponent, System::Classes::TOperation Operation);
	virtual void __fastcall Loaded();
	
private:
	Sgcwebsocket_protocol_broker_server::TsgcWSProtocol_Broker_Server* FBroker;
	
protected:
	virtual void __fastcall SetBroker(Sgcwebsocket_protocol_broker_server::TsgcWSProtocol_Broker_Server* const Value);
	
public:
	__property Sgcwebsocket_protocol_broker_server::TsgcWSProtocol_Broker_Server* Broker = {read=FBroker, write=SetBroker};
	__fastcall virtual TsgcWSProtocol_Server_Base(System::Classes::TComponent* aOwner);
	
protected:
	Sgcwebsocket_classes::TsgcWSComponent_Server* __fastcall GetWSServer();
	virtual bool __fastcall BroadcastConnection(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aChannel, Sgcwebsocket_helpers::TsgcDelimitedStringList* aExclude, Sgcwebsocket_helpers::TsgcDelimitedStringList* aInclude);
	virtual void __fastcall GetBroadcastConnections(System::UnicodeString aChannel, System::UnicodeString Exclude, System::UnicodeString Include, System::Classes::TList* &Connections);
	virtual void __fastcall Broadcast(System::UnicodeString aMessage, System::UnicodeString aChannel = System::UnicodeString(), System::UnicodeString Exclude = System::UnicodeString(), System::UnicodeString Include = System::UnicodeString())/* overload */;
	virtual void __fastcall Broadcast(System::Classes::TStream* aStream, System::UnicodeString aChannel = System::UnicodeString(), System::UnicodeString Exclude = System::UnicodeString(), System::UnicodeString Include = System::UnicodeString(), int aSize = 0x0, Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0))/* overload */;
	virtual bool __fastcall WriteData(System::UnicodeString aGuid, System::UnicodeString aMessage)/* overload */;
	virtual bool __fastcall WriteData(System::UnicodeString aGuid, System::Classes::TStream* aStream, int aSize = 0x0, Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0))/* overload */;
	virtual bool __fastcall WriteData(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aMessage)/* overload */;
	virtual void __fastcall WriteData(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::Classes::TStream* aStream)/* overload */;
public:
	/* TsgcWSProtocol_Server.Destroy */ inline __fastcall virtual ~TsgcWSProtocol_Server_Base() { }
	
};


class PASCALIMPLEMENTATION TsgcWSProtocol_Subscription_Server_Base : public TsgcWSProtocol_Server_Base
{
	typedef TsgcWSProtocol_Server_Base inherited;
	
protected:
	virtual void __fastcall DoEventDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int aCode);
	
private:
	Sgcwebsocket_classes::TsgcWSSubscriptionEvent FOnSubscription;
	Sgcwebsocket_classes::TsgcWSSubscriptionEvent FOnUnSubscription;
	Sgcwebsocket_classes::TsgcWSBeforeSubscriptionEvent FOnBeforeSubscription;
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
	virtual void __fastcall DoEventBeforeSubscription(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Subscription, bool &Accept);
	virtual void __fastcall DoEventSubscription(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Subscription);
	virtual void __fastcall DoEventUnSubscription(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Subscription);
	virtual void __fastcall DoNotifySubscription(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoNotifyUnSubscription(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	
private:
	Sgcwebsocket_classes::TsgcQueueListChannels* FSubscriptions;
	bool FUseMatchesMask;
	Sgcwebsocket_classes::TsgcQueueListChannels* __fastcall GetSubscriptions();
	
protected:
	virtual void __fastcall DoSubscribe(System::UnicodeString aChannel, Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoUnSubscribe(System::UnicodeString aChannel, Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoUnSubscriptions(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall GetChannelsFromSubscriptions(const System::UnicodeString aChannel, System::Classes::TStringList* &Channels);
	
public:
	__property Sgcwebsocket_classes::TsgcQueueListChannels* Subscriptions = {read=GetSubscriptions, write=FSubscriptions};
	__property bool UseMatchesMask = {read=FUseMatchesMask, write=FUseMatchesMask, nodefault};
	__fastcall virtual TsgcWSProtocol_Subscription_Server_Base(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSProtocol_Subscription_Server_Base();
	__property Sgcwebsocket_classes::TsgcWSBeforeSubscriptionEvent OnBeforeSubscription = {read=FOnBeforeSubscription, write=FOnBeforeSubscription};
	__property Sgcwebsocket_classes::TsgcWSSubscriptionEvent OnSubscription = {read=FOnSubscription, write=FOnSubscription};
	__property Sgcwebsocket_classes::TsgcWSSubscriptionEvent OnUnSubscription = {read=FOnUnSubscription, write=FOnUnSubscription};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAckServer_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	int FInterval;
	int FTimeout;
	
public:
	__fastcall virtual TsgcWSAckServer_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property int Interval = {read=FInterval, write=FInterval, nodefault};
	__property int Timeout = {read=FTimeout, write=FTimeout, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSAckServer_Options() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSProtocol_Acknowledgment_Server_Base : public TsgcWSProtocol_Server_Base
{
	typedef TsgcWSProtocol_Server_Base inherited;
	
protected:
	virtual void __fastcall Loaded();
	
private:
	Sgcwebsocket_protocol_base_message::TsgcWSAckMessageList* FAcknowledgmentList;
	Sgcwebsocket_protocol_base_message::TsgcWSAckMessageList* __fastcall GetAcknowledgmentList();
	
protected:
	__property Sgcwebsocket_protocol_base_message::TsgcWSAckMessageList* AcknowledgmentList = {read=GetAcknowledgmentList, write=FAcknowledgmentList};
	
private:
	TsgcWSAckServer_Options* FAcknowledgment;
	void __fastcall SetAcknowledgment(TsgcWSAckServer_Options* const Value);
	
protected:
	__property TsgcWSAckServer_Options* Acknowledgment = {read=FAcknowledgment, write=SetAcknowledgment};
	virtual void __fastcall OnExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	virtual void __fastcall OnAcknowledgmentMessageEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString aText) = 0 ;
	
public:
	__fastcall virtual TsgcWSProtocol_Acknowledgment_Server_Base(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSProtocol_Acknowledgment_Server_Base();
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_base_server */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_BASE_SERVER)
using namespace Sgcwebsocket_protocol_base_server;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_base_serverHPP
