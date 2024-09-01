// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_STOMP_ActiveMQ_Client.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_stomp_activemq_clientHPP
#define Sgcwebsocket_protocol_stomp_activemq_clientHPP

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
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Protocol_STOMP_Broker_Client.hpp>
#include <sgcWebSocket_Const.hpp>
#include <sgcSTOMP.hpp>
#include <sgcWebSocket_Protocol_STOMP_Client.hpp>
#include <sgcWebSocket_Protocol_Base_Client.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_stomp_activemq_client
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSActiveMQSTOMPHeadersConnected;
class DELPHICLASS TsgcWSActiveMQSTOMPHeadersMessage;
class DELPHICLASS TsgcWSActiveMQSTOMPHeadersError;
class DELPHICLASS TsgcWSActiveMQSTOMPHeadersReceipt;
class DELPHICLASS TsgcWSActiveMQSTOMP_Queue_Options;
class DELPHICLASS TsgcWSActiveMQSTOMP_Message_Options;
class DELPHICLASS TsgcWSActiveMQSTOMP_Options;
class DELPHICLASS TsgcWSProtocol_STOMP_ActiveMQ_Client;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSActiveMQSTOMPConnectedEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, TsgcWSActiveMQSTOMPHeadersConnected* Headers);

typedef void __fastcall (__closure *TsgcWSActiveMQSTOMPMessageEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString MessageText, TsgcWSActiveMQSTOMPHeadersMessage* Headers, Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPSubscriptionItem* Subscription);

typedef void __fastcall (__closure *TsgcWSActiveMQSTOMPErrorEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString MessageText, TsgcWSActiveMQSTOMPHeadersError* Headers);

typedef void __fastcall (__closure *TsgcWSActiveMQSTOMPReceiptEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, TsgcWSActiveMQSTOMPHeadersReceipt* Headers);

typedef void __fastcall (__closure *TsgcWSActiveMQDisconnectedEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, int Code);

class PASCALIMPLEMENTATION TsgcWSActiveMQSTOMPHeadersConnected : public Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPHeadersConnected
{
	typedef Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPHeadersConnected inherited;
	
public:
	/* TsgcWSBrokerSTOMPHeadersBase.Create */ inline __fastcall virtual TsgcWSActiveMQSTOMPHeadersConnected(System::Classes::TComponent* aOwner) : Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPHeadersConnected(aOwner) { }
	/* TsgcWSBrokerSTOMPHeadersBase.Destroy */ inline __fastcall virtual ~TsgcWSActiveMQSTOMPHeadersConnected() { }
	
};


class PASCALIMPLEMENTATION TsgcWSActiveMQSTOMPHeadersMessage : public Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPHeadersMessage
{
	typedef Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPHeadersMessage inherited;
	
private:
	int FExpires;
	int FPriority;
	System::UnicodeString FTimestamp;
	
protected:
	virtual void __fastcall DoRead();
	
public:
	__property int Expires = {read=FExpires, write=FExpires, nodefault};
	__property int Priority = {read=FPriority, write=FPriority, nodefault};
	__property System::UnicodeString Timestamp = {read=FTimestamp, write=FTimestamp};
public:
	/* TsgcWSBrokerSTOMPHeadersBase.Create */ inline __fastcall virtual TsgcWSActiveMQSTOMPHeadersMessage(System::Classes::TComponent* aOwner) : Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPHeadersMessage(aOwner) { }
	/* TsgcWSBrokerSTOMPHeadersBase.Destroy */ inline __fastcall virtual ~TsgcWSActiveMQSTOMPHeadersMessage() { }
	
};


class PASCALIMPLEMENTATION TsgcWSActiveMQSTOMPHeadersError : public Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPHeadersError
{
	typedef Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPHeadersError inherited;
	
public:
	/* TsgcWSBrokerSTOMPHeadersBase.Create */ inline __fastcall virtual TsgcWSActiveMQSTOMPHeadersError(System::Classes::TComponent* aOwner) : Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPHeadersError(aOwner) { }
	/* TsgcWSBrokerSTOMPHeadersBase.Destroy */ inline __fastcall virtual ~TsgcWSActiveMQSTOMPHeadersError() { }
	
};


class PASCALIMPLEMENTATION TsgcWSActiveMQSTOMPHeadersReceipt : public Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPHeadersReceipt
{
	typedef Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPHeadersReceipt inherited;
	
public:
	/* TsgcWSBrokerSTOMPHeadersBase.Create */ inline __fastcall virtual TsgcWSActiveMQSTOMPHeadersReceipt(System::Classes::TComponent* aOwner) : Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPHeadersReceipt(aOwner) { }
	/* TsgcWSBrokerSTOMPHeadersBase.Destroy */ inline __fastcall virtual ~TsgcWSActiveMQSTOMPHeadersReceipt() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSActiveMQSTOMP_Queue_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::Byte FPriority;
	System::UnicodeString FSelector;
	int FMaximumPendingMessageLimit;
	bool FNoLocal;
	int FPrefetchSize;
	bool FDispatchAsync;
	System::Classes::TStringList* FHeaders;
	void __fastcall DoHeaders();
	
protected:
	System::Classes::TStringList* __fastcall GetHeaders();
	
public:
	void __fastcall Clear();
	__fastcall virtual TsgcWSActiveMQSTOMP_Queue_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	__property bool DispatchAsync = {read=FDispatchAsync, write=FDispatchAsync, nodefault};
	__property int MaximumPendingMessageLimit = {read=FMaximumPendingMessageLimit, write=FMaximumPendingMessageLimit, nodefault};
	__property bool NoLocal = {read=FNoLocal, write=FNoLocal, nodefault};
	__property int PrefetchSize = {read=FPrefetchSize, write=FPrefetchSize, nodefault};
	__property System::Byte Priority = {read=FPriority, write=FPriority, nodefault};
	__property System::UnicodeString Selector = {read=FSelector, write=FSelector};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSActiveMQSTOMP_Queue_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSActiveMQSTOMP_Message_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FCorrelationId;
	int FExpires;
	System::UnicodeString FJMSXGroupID;
	int FJMSXGroupSeq;
	System::UnicodeString FMsgType;
	bool FPersistent;
	int FPriority;
	System::UnicodeString FReplyTo;
	System::Classes::TStringList* FHeaders;
	void __fastcall DoHeaders();
	
protected:
	System::Classes::TStringList* __fastcall GetHeaders();
	
public:
	void __fastcall Clear();
	__fastcall virtual TsgcWSActiveMQSTOMP_Message_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString CorrelationId = {read=FCorrelationId, write=FCorrelationId};
	__property int Expires = {read=FExpires, write=FExpires, nodefault};
	__property System::UnicodeString JMSXGroupID = {read=FJMSXGroupID, write=FJMSXGroupID};
	__property int JMSXGroupSeq = {read=FJMSXGroupSeq, write=FJMSXGroupSeq, nodefault};
	__property System::UnicodeString MsgType = {read=FMsgType, write=FMsgType};
	__property bool Persistent = {read=FPersistent, write=FPersistent, nodefault};
	__property int Priority = {read=FPriority, write=FPriority, nodefault};
	__property System::UnicodeString ReplyTo = {read=FReplyTo, write=FReplyTo};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSActiveMQSTOMP_Message_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSActiveMQSTOMP_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FClientId;
	
public:
	__fastcall virtual TsgcWSActiveMQSTOMP_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString ClientId = {read=FClientId, write=FClientId};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSActiveMQSTOMP_Options() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSProtocol_STOMP_ActiveMQ_Client : public Sgcwebsocket_protocol_stomp_broker_client::TsgcWSProtocol_STOMP_Broker_Client
{
	typedef Sgcwebsocket_protocol_stomp_broker_client::TsgcWSProtocol_STOMP_Broker_Client inherited;
	
protected:
	virtual void __fastcall DoBrokerConnectEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aRawHeaders);
	virtual void __fastcall DoBrokerMessageEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aMessageText, System::UnicodeString aRawHeaders, Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPSubscriptionItem* aSubscription);
	virtual void __fastcall DoBrokerErrorEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aMessageText, System::UnicodeString aRawHeaders);
	virtual void __fastcall DoBrokerReceiptEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aRawHeaders);
	virtual void __fastcall DoBrokerDisconnectedEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int aCode);
	virtual void __fastcall DoConnect(Sgcwebsocket_classes::TsgcWSConnection* const aConnection);
	
private:
	TsgcWSActiveMQSTOMP_Queue_Options* FQueue;
	void __fastcall SetQueue(TsgcWSActiveMQSTOMP_Queue_Options* const Value);
	
public:
	__property TsgcWSActiveMQSTOMP_Queue_Options* Queue = {read=FQueue, write=SetQueue};
	
private:
	TsgcWSActiveMQSTOMP_Options* FActiveMQ_Options;
	void __fastcall SetActiveMQ_Options(TsgcWSActiveMQSTOMP_Options* const Value);
	
public:
	__property TsgcWSActiveMQSTOMP_Options* ActiveMQ_Options = {read=FActiveMQ_Options, write=SetActiveMQ_Options};
	__fastcall virtual TsgcWSProtocol_STOMP_ActiveMQ_Client(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSProtocol_STOMP_ActiveMQ_Client();
	
private:
	System::Classes::TStringList* FSubscriptionHeaders;
	System::Classes::TStringList* FSendHeaders;
	System::Classes::TStringList* __fastcall GetSubscriptionHeaders(const bool aDurable, bool aExclusive, TsgcWSActiveMQSTOMP_Queue_Options* aOptions);
	
protected:
	virtual void __fastcall DoSubscribe(const System::UnicodeString aDestination, const bool aDurable, const bool aExclusive, const Sgcstomp::TsgcSTOMPACK aACK, TsgcWSActiveMQSTOMP_Queue_Options* const aOptions);
	virtual void __fastcall DoUnSubscribe(const System::UnicodeString aDestination);
	void __fastcall DoPublish(const System::UnicodeString aDestination, const System::UnicodeString aText, const System::UnicodeString aContentType, const System::UnicodeString aTransaction, TsgcWSActiveMQSTOMP_Message_Options* const aOptions);
	
public:
	void __fastcall SubscribeEx(const System::UnicodeString aDestination, const bool aDurable = true, const bool aExclusive = false, const Sgcstomp::TsgcSTOMPACK aACK = (Sgcstomp::TsgcSTOMPACK)(0x0), TsgcWSActiveMQSTOMP_Queue_Options* const aOptions = (TsgcWSActiveMQSTOMP_Queue_Options*)(0x0));
	void __fastcall PublishEx(const System::UnicodeString aDestination, const System::UnicodeString aText, const System::UnicodeString aContentType = L"text/plain", const System::UnicodeString aTransaction = System::UnicodeString(), TsgcWSActiveMQSTOMP_Message_Options* const aOptions = (TsgcWSActiveMQSTOMP_Message_Options*)(0x0));
	void __fastcall UnSubscribeEx(const System::UnicodeString aDestination);
	void __fastcall SubscribeTopic(const System::UnicodeString aTopic, const bool aDurable = true, const bool aExclusive = false, const Sgcstomp::TsgcSTOMPACK aACK = (Sgcstomp::TsgcSTOMPACK)(0x0), TsgcWSActiveMQSTOMP_Queue_Options* const aOptions = (TsgcWSActiveMQSTOMP_Queue_Options*)(0x0));
	void __fastcall UnSubscribeTopic(const System::UnicodeString aTopic);
	void __fastcall PublishTopic(const System::UnicodeString aTopic, const System::UnicodeString aText, const System::UnicodeString aContentType = L"text/plain", const System::UnicodeString aTransaction = System::UnicodeString(), TsgcWSActiveMQSTOMP_Message_Options* const aOptions = (TsgcWSActiveMQSTOMP_Message_Options*)(0x0));
	void __fastcall SubscribeQueue(const System::UnicodeString aQueue, const bool aDurable = true, const bool aExclusive = false, const Sgcstomp::TsgcSTOMPACK aACK = (Sgcstomp::TsgcSTOMPACK)(0x0), TsgcWSActiveMQSTOMP_Queue_Options* const aOptions = (TsgcWSActiveMQSTOMP_Queue_Options*)(0x0));
	void __fastcall UnSubscribeQueue(const System::UnicodeString aQueue);
	void __fastcall PublishQueue(const System::UnicodeString aQueue, const System::UnicodeString aText, const System::UnicodeString aContentType = L"text/plain", const System::UnicodeString aTransaction = System::UnicodeString(), TsgcWSActiveMQSTOMP_Message_Options* const aOptions = (TsgcWSActiveMQSTOMP_Message_Options*)(0x0));
	
private:
	TsgcWSActiveMQSTOMPConnectedEvent FOnActiveMQConnected;
	TsgcWSActiveMQDisconnectedEvent FOnActiveMQDisconnected;
	TsgcWSActiveMQSTOMPErrorEvent FOnActiveMQError;
	TsgcWSActiveMQSTOMPMessageEvent FOnActiveMQMessage;
	TsgcWSActiveMQSTOMPReceiptEvent FOnActiveMQReceipt;
	
protected:
	virtual void __fastcall DoActiveMQConnectEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aRawHeaders);
	virtual void __fastcall DoActiveMQMessageEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aMessageText, System::UnicodeString aRawHeaders, Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPSubscriptionItem* aSubscription);
	virtual void __fastcall DoActiveMQErrorEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aMessageText, System::UnicodeString aRawHeaders);
	virtual void __fastcall DoActiveMQReceiptEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aRawHeaders);
	virtual void __fastcall DoActiveMQDisconnectedEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int aCode);
	
public:
	__property TsgcWSActiveMQSTOMPConnectedEvent OnActiveMQConnected = {read=FOnActiveMQConnected, write=FOnActiveMQConnected};
	__property TsgcWSActiveMQSTOMPMessageEvent OnActiveMQMessage = {read=FOnActiveMQMessage, write=FOnActiveMQMessage};
	__property TsgcWSActiveMQSTOMPErrorEvent OnActiveMQError = {read=FOnActiveMQError, write=FOnActiveMQError};
	__property TsgcWSActiveMQSTOMPReceiptEvent OnActiveMQReceipt = {read=FOnActiveMQReceipt, write=FOnActiveMQReceipt};
	__property TsgcWSActiveMQDisconnectedEvent OnActiveMQDisconnected = {read=FOnActiveMQDisconnected, write=FOnActiveMQDisconnected};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_stomp_activemq_client */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_STOMP_ACTIVEMQ_CLIENT)
using namespace Sgcwebsocket_protocol_stomp_activemq_client;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_stomp_activemq_clientHPP
