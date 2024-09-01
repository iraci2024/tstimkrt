// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_STOMP_RabbitMQ_Client.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_stomp_rabbitmq_clientHPP
#define Sgcwebsocket_protocol_stomp_rabbitmq_clientHPP

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
#include <sgcSTOMP.hpp>
#include <sgcWebSocket_Const.hpp>
#include <sgcWebSocket_Protocol_STOMP_Client.hpp>
#include <sgcWebSocket_Protocol_Base_Client.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_stomp_rabbitmq_client
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSRabbitMQSTOMPHeadersConnected;
class DELPHICLASS TsgcWSRabbitMQSTOMPHeadersMessage;
class DELPHICLASS TsgcWSRabbitMQSTOMPHeadersError;
class DELPHICLASS TsgcWSRabbitMQSTOMPHeadersReceipt;
class DELPHICLASS TsgcWSRabbitMQSTOMP_Queue_Options;
class DELPHICLASS TsgcWSProtocol_STOMP_RabbitMQ_Client;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSRabbitMQSTOMPConnectedEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, TsgcWSRabbitMQSTOMPHeadersConnected* Headers);

typedef void __fastcall (__closure *TsgcWSRabbitMQSTOMPMessageEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString MessageText, TsgcWSRabbitMQSTOMPHeadersMessage* Headers, Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPSubscriptionItem* Subscription);

typedef void __fastcall (__closure *TsgcWSRabbitMQSTOMPErrorEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString MessageText, TsgcWSRabbitMQSTOMPHeadersError* Headers);

typedef void __fastcall (__closure *TsgcWSRabbitMQSTOMPReceiptEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, TsgcWSRabbitMQSTOMPHeadersReceipt* Headers);

typedef void __fastcall (__closure *TsgcWSRabbitMQDisconnectedEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, int Code);

class PASCALIMPLEMENTATION TsgcWSRabbitMQSTOMPHeadersConnected : public Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPHeadersConnected
{
	typedef Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPHeadersConnected inherited;
	
public:
	/* TsgcWSBrokerSTOMPHeadersBase.Create */ inline __fastcall virtual TsgcWSRabbitMQSTOMPHeadersConnected(System::Classes::TComponent* aOwner) : Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPHeadersConnected(aOwner) { }
	/* TsgcWSBrokerSTOMPHeadersBase.Destroy */ inline __fastcall virtual ~TsgcWSRabbitMQSTOMPHeadersConnected() { }
	
};


class PASCALIMPLEMENTATION TsgcWSRabbitMQSTOMPHeadersMessage : public Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPHeadersMessage
{
	typedef Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPHeadersMessage inherited;
	
public:
	/* TsgcWSBrokerSTOMPHeadersBase.Create */ inline __fastcall virtual TsgcWSRabbitMQSTOMPHeadersMessage(System::Classes::TComponent* aOwner) : Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPHeadersMessage(aOwner) { }
	/* TsgcWSBrokerSTOMPHeadersBase.Destroy */ inline __fastcall virtual ~TsgcWSRabbitMQSTOMPHeadersMessage() { }
	
};


class PASCALIMPLEMENTATION TsgcWSRabbitMQSTOMPHeadersError : public Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPHeadersError
{
	typedef Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPHeadersError inherited;
	
public:
	/* TsgcWSBrokerSTOMPHeadersBase.Create */ inline __fastcall virtual TsgcWSRabbitMQSTOMPHeadersError(System::Classes::TComponent* aOwner) : Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPHeadersError(aOwner) { }
	/* TsgcWSBrokerSTOMPHeadersBase.Destroy */ inline __fastcall virtual ~TsgcWSRabbitMQSTOMPHeadersError() { }
	
};


class PASCALIMPLEMENTATION TsgcWSRabbitMQSTOMPHeadersReceipt : public Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPHeadersReceipt
{
	typedef Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPHeadersReceipt inherited;
	
public:
	/* TsgcWSBrokerSTOMPHeadersBase.Create */ inline __fastcall virtual TsgcWSRabbitMQSTOMPHeadersReceipt(System::Classes::TComponent* aOwner) : Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPHeadersReceipt(aOwner) { }
	/* TsgcWSBrokerSTOMPHeadersBase.Destroy */ inline __fastcall virtual ~TsgcWSRabbitMQSTOMPHeadersReceipt() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSRabbitMQSTOMP_Queue_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FDeadLetterExchange;
	System::UnicodeString FDeadLetterRoutingKey;
	int FExpires;
	int FMaxLength;
	int FMaxLengthBytes;
	int FMaxPriority;
	int FMessageTTL;
	System::Classes::TStringList* FHeaders;
	void __fastcall DoHeaders();
	
protected:
	System::Classes::TStringList* __fastcall GetHeaders();
	
public:
	void __fastcall Clear();
	__fastcall virtual TsgcWSRabbitMQSTOMP_Queue_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	__property int MessageTTL = {read=FMessageTTL, write=FMessageTTL, nodefault};
	__property int Expires = {read=FExpires, write=FExpires, nodefault};
	__property int MaxLength = {read=FMaxLength, write=FMaxLength, nodefault};
	__property int MaxLengthBytes = {read=FMaxLengthBytes, write=FMaxLengthBytes, nodefault};
	__property System::UnicodeString DeadLetterExchange = {read=FDeadLetterExchange, write=FDeadLetterExchange};
	__property System::UnicodeString DeadLetterRoutingKey = {read=FDeadLetterRoutingKey, write=FDeadLetterRoutingKey};
	__property int MaxPriority = {read=FMaxPriority, write=FMaxPriority, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSRabbitMQSTOMP_Queue_Options() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSProtocol_STOMP_RabbitMQ_Client : public Sgcwebsocket_protocol_stomp_broker_client::TsgcWSProtocol_STOMP_Broker_Client
{
	typedef Sgcwebsocket_protocol_stomp_broker_client::TsgcWSProtocol_STOMP_Broker_Client inherited;
	
protected:
	virtual void __fastcall DoBrokerConnectEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aRawHeaders);
	virtual void __fastcall DoBrokerMessageEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aMessageText, System::UnicodeString aRawHeaders, Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPSubscriptionItem* aSubscription);
	virtual void __fastcall DoBrokerErrorEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aMessageText, System::UnicodeString aRawHeaders);
	virtual void __fastcall DoBrokerReceiptEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aRawHeaders);
	virtual void __fastcall DoBrokerDisconnectedEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int aCode);
	
private:
	TsgcWSRabbitMQSTOMP_Queue_Options* FQueue;
	void __fastcall SetQueue(TsgcWSRabbitMQSTOMP_Queue_Options* const Value);
	
public:
	__property TsgcWSRabbitMQSTOMP_Queue_Options* Queue = {read=FQueue, write=SetQueue};
	__fastcall virtual TsgcWSProtocol_STOMP_RabbitMQ_Client(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSProtocol_STOMP_RabbitMQ_Client();
	
private:
	System::Classes::TStringList* FSubscriptionHeaders;
	System::Classes::TStringList* FSendHeaders;
	System::Classes::TStringList* __fastcall GetSubscriptionHeaders(const bool aDurable, const bool aAutoDelete, bool aExclusive, TsgcWSRabbitMQSTOMP_Queue_Options* aOptions);
	System::Classes::TStringList* __fastcall GetSendHeaders(const System::UnicodeString aReplyTo);
	
protected:
	virtual void __fastcall DoSubscribe(const System::UnicodeString aDestination, const bool aDurable, const bool aAutoDelete, const bool aExclusive, const Sgcstomp::TsgcSTOMPACK aACK, TsgcWSRabbitMQSTOMP_Queue_Options* const aOptions);
	virtual void __fastcall DoUnSubscribe(const System::UnicodeString aDestination);
	void __fastcall DoPublish(const System::UnicodeString aDestination, const System::UnicodeString aText, const System::UnicodeString aContentType, const System::UnicodeString aTransaction, const System::UnicodeString aReplyTo);
	
public:
	void __fastcall SubscribeEx(const System::UnicodeString aDestination, const bool aDurable = true, const bool aAutoDelete = false, const bool aExclusive = false, const Sgcstomp::TsgcSTOMPACK aACK = (Sgcstomp::TsgcSTOMPACK)(0x0), TsgcWSRabbitMQSTOMP_Queue_Options* const aOptions = (TsgcWSRabbitMQSTOMP_Queue_Options*)(0x0));
	void __fastcall PublishEx(const System::UnicodeString aDestination, const System::UnicodeString aText, const System::UnicodeString aContentType = L"text/plain", const System::UnicodeString aTransaction = System::UnicodeString());
	void __fastcall UnSubscribeEx(const System::UnicodeString aDestination);
	void __fastcall SubscribeTopic(const System::UnicodeString aTopic, const bool aDurable = true, const bool aAutoDelete = false, const bool aExclusive = false, const Sgcstomp::TsgcSTOMPACK aACK = (Sgcstomp::TsgcSTOMPACK)(0x0), TsgcWSRabbitMQSTOMP_Queue_Options* const aOptions = (TsgcWSRabbitMQSTOMP_Queue_Options*)(0x0));
	void __fastcall UnSubscribeTopic(const System::UnicodeString aTopic);
	void __fastcall PublishTopic(const System::UnicodeString aTopic, const System::UnicodeString aText, const System::UnicodeString aContentType = L"text/plain", const System::UnicodeString aTransaction = System::UnicodeString());
	void __fastcall SubscribeQueue(const System::UnicodeString aQueue, const bool aDurable = true, const bool aAutoDelete = false, const bool aExclusive = false, const Sgcstomp::TsgcSTOMPACK aACK = (Sgcstomp::TsgcSTOMPACK)(0x0), TsgcWSRabbitMQSTOMP_Queue_Options* const aOptions = (TsgcWSRabbitMQSTOMP_Queue_Options*)(0x0));
	void __fastcall UnSubscribeQueue(const System::UnicodeString aQueue);
	void __fastcall PublishQueue(const System::UnicodeString aQueue, const System::UnicodeString aText, const System::UnicodeString aContentType = L"text/plain", const System::UnicodeString aTransaction = System::UnicodeString());
	void __fastcall SubscribeQueueOutside(const System::UnicodeString aQueue, const bool aDurable = true, const bool aAutoDelete = false, const bool aExclusive = false, const Sgcstomp::TsgcSTOMPACK aACK = (Sgcstomp::TsgcSTOMPACK)(0x0), TsgcWSRabbitMQSTOMP_Queue_Options* const aOptions = (TsgcWSRabbitMQSTOMP_Queue_Options*)(0x0));
	void __fastcall UnSubscribeQueueOutside(const System::UnicodeString aQueue);
	void __fastcall PublishQueueOutside(const System::UnicodeString aQueue, const System::UnicodeString aText, const System::UnicodeString aContentType = L"text/plain", const System::UnicodeString aTransaction = System::UnicodeString());
	void __fastcall SubscribeTemporaryQueue(const System::UnicodeString aQueue, const System::UnicodeString aReplyTo, const bool aDurable = true, const bool aAutoDelete = false, const bool aExclusive = false, const Sgcstomp::TsgcSTOMPACK aACK = (Sgcstomp::TsgcSTOMPACK)(0x0), TsgcWSRabbitMQSTOMP_Queue_Options* const aOptions = (TsgcWSRabbitMQSTOMP_Queue_Options*)(0x0));
	void __fastcall UnSubscribeTemporaryQueue(const System::UnicodeString aQueue);
	void __fastcall PublishTemporaryQueue(const System::UnicodeString aQueue, const System::UnicodeString aReplyTo, const System::UnicodeString aText, const System::UnicodeString aContentType = L"text/plain", const System::UnicodeString aTransaction = System::UnicodeString());
	void __fastcall SubscribeExchange(const System::UnicodeString aName, const System::UnicodeString aPattern, const bool aDurable = true, const bool aAutoDelete = false, const bool aExclusive = false, const Sgcstomp::TsgcSTOMPACK aACK = (Sgcstomp::TsgcSTOMPACK)(0x0), TsgcWSRabbitMQSTOMP_Queue_Options* const aOptions = (TsgcWSRabbitMQSTOMP_Queue_Options*)(0x0));
	void __fastcall UnSubscribeExchange(const System::UnicodeString aName, const System::UnicodeString aPattern);
	void __fastcall PublishExchange(const System::UnicodeString aName, const System::UnicodeString aRoutingKey, const System::UnicodeString aText, const System::UnicodeString aContentType = L"text/plain", const System::UnicodeString aTransaction = System::UnicodeString());
	
private:
	TsgcWSRabbitMQSTOMPConnectedEvent FOnRabbitMQConnected;
	TsgcWSRabbitMQDisconnectedEvent FOnRabbitMQDisconnected;
	TsgcWSRabbitMQSTOMPErrorEvent FOnRabbitMQError;
	TsgcWSRabbitMQSTOMPMessageEvent FOnRabbitMQMessage;
	TsgcWSRabbitMQSTOMPReceiptEvent FOnRabbitMQReceipt;
	
protected:
	virtual void __fastcall DoRabbitMQConnectEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aRawHeaders);
	virtual void __fastcall DoRabbitMQMessageEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aMessageText, System::UnicodeString aRawHeaders, Sgcwebsocket_protocol_stomp_broker_client::TsgcWSBrokerSTOMPSubscriptionItem* aSubscription);
	virtual void __fastcall DoRabbitMQErrorEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aMessageText, System::UnicodeString aRawHeaders);
	virtual void __fastcall DoRabbitMQReceiptEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, System::UnicodeString aRawHeaders);
	virtual void __fastcall DoRabbitMQDisconnectedEvent(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int aCode);
	
public:
	__property TsgcWSRabbitMQSTOMPConnectedEvent OnRabbitMQConnected = {read=FOnRabbitMQConnected, write=FOnRabbitMQConnected};
	__property TsgcWSRabbitMQSTOMPMessageEvent OnRabbitMQMessage = {read=FOnRabbitMQMessage, write=FOnRabbitMQMessage};
	__property TsgcWSRabbitMQSTOMPErrorEvent OnRabbitMQError = {read=FOnRabbitMQError, write=FOnRabbitMQError};
	__property TsgcWSRabbitMQSTOMPReceiptEvent OnRabbitMQReceipt = {read=FOnRabbitMQReceipt, write=FOnRabbitMQReceipt};
	__property TsgcWSRabbitMQDisconnectedEvent OnRabbitMQDisconnected = {read=FOnRabbitMQDisconnected, write=FOnRabbitMQDisconnected};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_stomp_rabbitmq_client */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_STOMP_RABBITMQ_CLIENT)
using namespace Sgcwebsocket_protocol_stomp_rabbitmq_client;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_stomp_rabbitmq_clientHPP
