// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_AMQP_Client.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_amqp_clientHPP
#define Sgcwebsocket_protocol_amqp_clientHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <Winapi.Windows.hpp>
#include <System.Contnrs.hpp>
#include <sgcWebSocket_Protocol_Base_Client.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcAMQP.hpp>
#include <sgcWebSocket_Protocol_AMQP_Message.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>
#include <sgcAMQP_Classes.hpp>
#include <sgcAMQP_Const.hpp>
#include <sgcBase_Helpers.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_amqp_client
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSAMQPAuthentication_Options;
class DELPHICLASS TsgcWSAMQPHeartBeat_Options;
class DELPHICLASS TsgcWSAMQP_Options;
class DELPHICLASS TsgcWSProtocol_AMQP_Client_Base;
class DELPHICLASS TsgcWSProtocol_AMQP_Client;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAMQPAuthentication_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	System::UnicodeString FPassword;
	System::UnicodeString FUserName;
	
public:
	__fastcall virtual TsgcWSAMQPAuthentication_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property System::UnicodeString Password = {read=FPassword, write=FPassword};
	__property System::UnicodeString UserName = {read=FUserName, write=FUserName};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSAMQPAuthentication_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAMQPHeartBeat_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	int FInterval;
	int FTimeout;
	
public:
	__fastcall virtual TsgcWSAMQPHeartBeat_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property int Interval = {read=FInterval, write=FInterval, nodefault};
	__property int Timeout = {read=FTimeout, write=FTimeout, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSAMQPHeartBeat_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSAMQP_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FLocale;
	System::Word FMaxChannels;
	unsigned FMaxFrameSize;
	System::UnicodeString FVirtualHost;
	
public:
	__fastcall virtual TsgcWSAMQP_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString Locale = {read=FLocale, write=FLocale};
	__property System::Word MaxChannels = {read=FMaxChannels, write=FMaxChannels, nodefault};
	__property unsigned MaxFrameSize = {read=FMaxFrameSize, write=FMaxFrameSize, nodefault};
	__property System::UnicodeString VirtualHost = {read=FVirtualHost, write=FVirtualHost};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSAMQP_Options() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSProtocol_AMQP_Client_Base : public Sgcwebsocket_protocol_base_client::TsgcWSProtocol_Client_Base
{
	typedef Sgcwebsocket_protocol_base_client::TsgcWSProtocol_Client_Base inherited;
	
private:
	System::UnicodeString FWSMessageId;
	void __fastcall DoAMQPDisconnectedEvent(int aCode);
	void __fastcall DoAMQPInitialize(Sgcwebsocket_protocol_amqp_message::TsgcWSAMQPMessage* const aMessage);
	
protected:
	Sgcwebsocket_protocol_amqp_message::TsgcWSAMQPMessage* FWSMessage;
	Sgcwebsocket_protocol_amqp_message::TsgcWSAMQPMessage* __fastcall GetWSMessage();
	Sgcwebsocket_protocol_amqp_message::TsgcWSAMQPMessage* __fastcall GetWSMessageByConnection(Sgcwebsocket_classes::TsgcWSConnection* const aConnection);
	virtual void __fastcall OnAMQPWriteBytesEvent(System::TObject* Sender, const System::DynamicArray<System::Byte> aBytes);
	virtual void __fastcall OnAMQPAuthenticationEvent(System::TObject* Sender, Sgcamqp_classes::TsgcAMQPAuthentications aMechanisms, Sgcamqp_classes::TsgcAMQPAuthentication &Mechanism, System::UnicodeString &User, System::UnicodeString &Password);
	virtual void __fastcall OnAMQPChallengeEvent(System::TObject* Sender, const System::UnicodeString aChallenge, System::UnicodeString &Challenge);
	virtual void __fastcall OnAMQPConnectEvent(System::TObject* Sender);
	virtual void __fastcall OnAMQPHeartBeatEvent(System::TObject* Sender);
	virtual void __fastcall OnAMQPDisconnectEvent(System::TObject* Sender, Sgcamqp_classes::TsgcAMQPFramePayload_Method_ConnectionClose* const aClose, bool aAck);
	virtual void __fastcall OnAMQPChannelOpenEvent(System::TObject* Sender, const System::UnicodeString aChannel);
	virtual void __fastcall OnAMQPChannelCloseEvent(System::TObject* Sender, const System::UnicodeString aChannel, Sgcamqp_classes::TsgcAMQPFramePayload_Method_ChannelClose* const aChannelClose, bool aAck);
	virtual void __fastcall OnAMQPChannelFlowEvent(System::TObject* Sender, const System::UnicodeString aChannel, bool aFlow, bool aAck);
	virtual void __fastcall OnAMQPExchangeDeclareEvent(System::TObject* Sender, const System::UnicodeString aChannel, const System::UnicodeString aExchange);
	virtual void __fastcall OnAMQPExchangeDeleteEvent(System::TObject* Sender, const System::UnicodeString aChannel, const System::UnicodeString aExchange);
	virtual void __fastcall OnAMQPQueueDeclareEvent(System::TObject* Sender, const System::UnicodeString aChannel, const System::UnicodeString aQueue, int aMessageCount, int aConsumerCount);
	virtual void __fastcall OnAMQPQueueBindEvent(System::TObject* Sender, const System::UnicodeString aChannel, const System::UnicodeString aQueue, const System::UnicodeString aExchange);
	virtual void __fastcall OnAMQPQueueUnBindEvent(System::TObject* Sender, const System::UnicodeString aChannel, const System::UnicodeString aQueue, const System::UnicodeString aExchange);
	virtual void __fastcall OnAMQPQueuePurgeEvent(System::TObject* Sender, const System::UnicodeString aChannel, const System::UnicodeString aQueue, int aMessageCount);
	virtual void __fastcall OnAMQPQueueDeleteEvent(System::TObject* Sender, const System::UnicodeString aChannel, const System::UnicodeString aQueue, int aMessageCount);
	virtual void __fastcall OnAMQPBasicQoSEvent(System::TObject* Sender, const System::UnicodeString aChannel, Sgcamqp_classes::TsgcAMQPFramePayload_Method_BasicQoS* const aQoS);
	virtual void __fastcall OnAMQPBasicConsumeEvent(System::TObject* Sender, const System::UnicodeString aChannel, const System::UnicodeString aConsumerTag);
	virtual void __fastcall OnAMQPBasicCancelConsumeEvent(System::TObject* Sender, const System::UnicodeString aChannel, const System::UnicodeString aConsumerTag);
	virtual void __fastcall OnAMQPBasicReturnEvent(System::TObject* Sender, const System::UnicodeString aChannel, Sgcamqp_classes::TsgcAMQPFramePayload_Method_BasicReturn* const aReturn, Sgcamqp_classes::TsgcAMQPMessageContent* const aMessage);
	virtual void __fastcall OnAMQPBasicDeliverEvent(System::TObject* Sender, const System::UnicodeString aChannel, Sgcamqp_classes::TsgcAMQPFramePayload_Method_BasicDeliver* const aDeliver, Sgcamqp_classes::TsgcAMQPMessageContent* const aMessage);
	virtual void __fastcall OnAMQPBasicGetOkEvent(System::TObject* Sender, const System::UnicodeString aChannel, Sgcamqp_classes::TsgcAMQPFramePayload_Method_BasicGetOk* const aGetOk, Sgcamqp_classes::TsgcAMQPMessageContent* const aMessage);
	virtual void __fastcall OnAMQPBasicGetEmptyEvent(System::TObject* Sender, const System::UnicodeString aChannel);
	virtual void __fastcall OnAMQPBasicRecoverOkEvent(System::TObject* Sender, const System::UnicodeString aChannel);
	virtual void __fastcall OnAMQPTransactionOkEvent(System::TObject* Sender, const System::UnicodeString aChannel, Sgcamqp_classes::TsgcAMQPTransaction aTransaction);
	virtual void __fastcall OnAMQPExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	void __fastcall OnAMQPConnectedEvent(System::TObject* Sender, System::UnicodeString aVersion, System::UnicodeString aServer, System::UnicodeString aSession, System::UnicodeString aHeartBeat, System::UnicodeString aRawHeaders);
	void __fastcall OnAMQPErrorEvent(System::TObject* Sender, System::UnicodeString aMessageText, System::UnicodeString aContentType, int aContentLength, System::UnicodeString aReceiptId, System::UnicodeString aRawHeaders);
	void __fastcall OnAMQPMessageEvent(System::TObject* Sender, System::UnicodeString aMessageText, System::UnicodeString aDestination, System::UnicodeString aMessageId, System::UnicodeString aSubscription, System::UnicodeString aAck, System::UnicodeString aContentType, System::UnicodeString aRawHeaders);
	void __fastcall OnAMQPReceiptEvent(System::TObject* Sender, System::UnicodeString aReceiptId, System::UnicodeString aRawHeaders);
	__property Sgcwebsocket_protocol_amqp_message::TsgcWSAMQPMessage* WSMessage = {read=GetWSMessage, write=FWSMessage};
	
private:
	Sgcwebsocket_classes::TsgcWSConnection* FWSConnection;
	
protected:
	virtual void __fastcall DoNotifyBinary(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoNotifyDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventBinary(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, System::Classes::TMemoryStream* Data);
	virtual void __fastcall DoEventDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int Code);
	virtual void __fastcall DoInitialize(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoFinalize(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoClear(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	
public:
	__fastcall virtual TsgcWSProtocol_AMQP_Client_Base(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSProtocol_AMQP_Client_Base();
	
private:
	Sgcamqp_classes::TsgcAMQPChannelThreads* __fastcall GetChannels();
	
public:
	__property Sgcamqp_classes::TsgcAMQPChannelThreads* Channels = {read=GetChannels};
	
protected:
	virtual void __fastcall DoWriteAMQP(const System::DynamicArray<System::Byte> aBytes);
	virtual void __fastcall DoConnect(Sgcwebsocket_classes::TsgcWSConnection* const aConnection);
	
public:
	void __fastcall Close(System::Word aReplyCode, const System::UnicodeString aReplyText, Sgcamqp_classes::TsgcAMQPClass aFailClassId = (Sgcamqp_classes::TsgcAMQPClass)(0x0), Sgcamqp_classes::TsgcAMQPMethod aFailMethodId = (Sgcamqp_classes::TsgcAMQPMethod)(0x0))/* overload */;
	bool __fastcall CloseEx(System::Word aReplyCode, const System::UnicodeString aReplyText, Sgcamqp_classes::TsgcAMQPClass aFailClassId = (Sgcamqp_classes::TsgcAMQPClass)(0x0), Sgcamqp_classes::TsgcAMQPMethod aFailMethodId = (Sgcamqp_classes::TsgcAMQPMethod)(0x0));
	void __fastcall Close()/* overload */;
	void __fastcall OpenChannel(const System::UnicodeString aChannel);
	bool __fastcall OpenChannelEx(const System::UnicodeString aChannel, int aTimeout = 0x2710);
	void __fastcall CloseChannel(const System::UnicodeString aChannel, System::Word aReplyCode = (System::Word)(0x0), const System::UnicodeString aReplyText = System::UnicodeString(), Sgcamqp_classes::TsgcAMQPClass aFailClassId = (Sgcamqp_classes::TsgcAMQPClass)(0x0), Sgcamqp_classes::TsgcAMQPMethod aFailMethodId = (Sgcamqp_classes::TsgcAMQPMethod)(0x0))/* overload */;
	bool __fastcall CloseChannelEx(const System::UnicodeString aChannel, System::Word aReplyCode = (System::Word)(0x0), const System::UnicodeString aReplyText = System::UnicodeString(), Sgcamqp_classes::TsgcAMQPClass aFailClassId = (Sgcamqp_classes::TsgcAMQPClass)(0x0), Sgcamqp_classes::TsgcAMQPMethod aFailMethodId = (Sgcamqp_classes::TsgcAMQPMethod)(0x0), int aTimeout = 0x2710)/* overload */;
	void __fastcall EnableChannel(const System::UnicodeString aChannel);
	bool __fastcall EnableChannelEx(const System::UnicodeString aChannel, int aTimeout = 0x2710);
	void __fastcall DisableChannel(const System::UnicodeString aChannel);
	bool __fastcall DisableChannelEx(const System::UnicodeString aChannel, int aTimeout = 0x2710);
	void __fastcall DeclareExchange(const System::UnicodeString aChannel, const System::UnicodeString aExchange, const System::UnicodeString aExchangeType = L"direct", bool aPassive = false, bool aDurable = false, bool aAutoDelete = false, bool aInternal = false, bool aNoWait = false);
	bool __fastcall DeclareExchangeEx(const System::UnicodeString aChannel, const System::UnicodeString aExchange, const System::UnicodeString aExchangeType = L"direct", bool aPassive = false, bool aDurable = false, bool aAutoDelete = false, bool aInternal = false, bool aNoWait = false, int aTimeout = 0x2710);
	void __fastcall DeleteExchange(const System::UnicodeString aChannel, const System::UnicodeString aExchange, bool aIfUnused = false, bool aNoWait = false);
	bool __fastcall DeleteExchangeEx(const System::UnicodeString aChannel, const System::UnicodeString aExchange, bool aIfUnused = false, bool aNoWait = false, int aTimeout = 0x2710);
	void __fastcall DeclareQueue(const System::UnicodeString aChannel, const System::UnicodeString aQueue, bool aPassive = false, bool aDurable = false, bool aExclusive = false, bool aAutoDelete = false, bool aNoWait = false);
	bool __fastcall DeclareQueueEx(const System::UnicodeString aChannel, const System::UnicodeString aQueue, bool aPassive = false, bool aDurable = false, bool aExclusive = false, bool aAutoDelete = false, bool aNoWait = false, int aTimeout = 0x2710);
	void __fastcall BindQueue(const System::UnicodeString aChannel, const System::UnicodeString aQueue, const System::UnicodeString aExchange, const System::UnicodeString aRoutingKey, bool aNoWait = false);
	bool __fastcall BindQueueEx(const System::UnicodeString aChannel, const System::UnicodeString aQueue, const System::UnicodeString aExchange, const System::UnicodeString aRoutingKey, bool aNoWait = false, int aTimeout = 0x2710);
	void __fastcall UnBindQueue(const System::UnicodeString aChannel, const System::UnicodeString aQueue, const System::UnicodeString aExchange, const System::UnicodeString aRoutingKey);
	bool __fastcall UnBindQueueEx(const System::UnicodeString aChannel, const System::UnicodeString aQueue, const System::UnicodeString aExchange, const System::UnicodeString aRoutingKey, int aTimeout = 0x2710);
	void __fastcall PurgeQueue(const System::UnicodeString aChannel, const System::UnicodeString aQueue, bool aNoWait = false);
	bool __fastcall PurgeQueueEx(const System::UnicodeString aChannel, const System::UnicodeString aQueue, bool aNoWait = false, int aTimeout = 0x2710);
	void __fastcall DeleteQueue(const System::UnicodeString aChannel, const System::UnicodeString aQueue, bool aIfUnused = false, bool aIfEmpty = false, bool aNoWait = false);
	bool __fastcall DeleteQueueEx(const System::UnicodeString aChannel, const System::UnicodeString aQueue, bool aIfUnused = false, bool aIfEmpty = false, bool aNoWait = false, int aTimeout = 0x2710);
	void __fastcall SetQoS(const System::UnicodeString aChannel, unsigned aPrefetchSize = (unsigned)(0x0), System::Word aPrefetchCount = (System::Word)(0x0), bool aGlobal = false);
	bool __fastcall SetQoSEx(const System::UnicodeString aChannel, unsigned aPrefetchSize = (unsigned)(0x0), System::Word aPrefetchCount = (System::Word)(0x0), bool aGlobal = false, int aTimeout = 0x2710);
	void __fastcall Consume(const System::UnicodeString aChannel, const System::UnicodeString aQueue, const System::UnicodeString aConsumerTag = System::UnicodeString(), bool aNoLocal = false, bool aNoAck = false, bool aExclusive = false, bool aNoWait = false);
	bool __fastcall ConsumeEx(const System::UnicodeString aChannel, const System::UnicodeString aQueue, const System::UnicodeString aConsumerTag = System::UnicodeString(), bool aNoLocal = false, bool aNoAck = false, bool aExclusive = false, int aTimeout = 0x2710);
	void __fastcall CancelConsume(const System::UnicodeString aChannel, const System::UnicodeString aConsumerTag, bool aNoWait = false);
	bool __fastcall CancelConsumeEx(const System::UnicodeString aChannel, const System::UnicodeString aConsumerTag, int aTimeout = 0x2710);
	void __fastcall PublishMessage(const System::UnicodeString aChannel, const System::UnicodeString aExchange, const System::UnicodeString aRoutingKey, const System::UnicodeString aMessage, bool aMandatory = false, bool aImmediate = false)/* overload */;
	void __fastcall PublishMessage(const System::UnicodeString aChannel, const System::UnicodeString aExchange, const System::UnicodeString aRoutingKey, System::Classes::TStream* aMessage, bool aMandatory = false, bool aImmediate = false)/* overload */;
	void __fastcall PublishMessage(const System::UnicodeString aChannel, const System::UnicodeString aExchange, const System::UnicodeString aRoutingKey, Sgcamqp_classes::TsgcAMQPBasicProperties* const aHeader, const System::UnicodeString aMessage, bool aMandatory = false, bool aImmediate = false)/* overload */;
	void __fastcall PublishMessage(const System::UnicodeString aChannel, const System::UnicodeString aExchange, const System::UnicodeString aRoutingKey, Sgcamqp_classes::TsgcAMQPBasicProperties* const aHeader, System::Classes::TStream* const aMessage, bool aMandatory = false, bool aImmediate = false)/* overload */;
	void __fastcall GetMessage(const System::UnicodeString aChannel, const System::UnicodeString aQueue, bool aNoAck = false);
	bool __fastcall GetMessageEx(const System::UnicodeString aChannel, const System::UnicodeString aQueue, int aTimeout = 0x2710);
	void __fastcall AckMessage(const System::UnicodeString aChannel, unsigned __int64 aDeliveryTag, bool aMultiple = false);
	void __fastcall RejectMessage(const System::UnicodeString aChannel, unsigned __int64 aDeliveryTag, bool aRequeue = false);
	void __fastcall RecoverAsync(const System::UnicodeString aChannel, bool aRequeue = false);
	void __fastcall Recover(const System::UnicodeString aChannel, bool aRequeue = false);
	bool __fastcall RecoverEx(const System::UnicodeString aChannel, bool aRequeue = false, int aTimeout = 0x2710);
	void __fastcall SelectTransaction(const System::UnicodeString aChannel);
	bool __fastcall SelectTransactionEx(const System::UnicodeString aChannel, int aTimeout = 0x2710);
	void __fastcall CommitTransaction(const System::UnicodeString aChannel);
	bool __fastcall CommitTransactionEx(const System::UnicodeString aChannel, int aTimeout = 0x2710);
	void __fastcall RollbackTransaction(const System::UnicodeString aChannel);
	bool __fastcall RollbackTransactionEx(const System::UnicodeString aChannel, int aTimeout = 0x2710);
	
private:
	TsgcWSAMQPAuthentication_Options* FAuthentication;
	void __fastcall SetAuthentication(TsgcWSAMQPAuthentication_Options* const Value);
	
public:
	__property TsgcWSAMQPAuthentication_Options* Authentication = {read=FAuthentication, write=SetAuthentication};
	
private:
	TsgcWSAMQPHeartBeat_Options* FHeartBeat;
	void __fastcall SetHeartBeat(TsgcWSAMQPHeartBeat_Options* const Value);
	
public:
	__property TsgcWSAMQPHeartBeat_Options* HeartBeat = {read=FHeartBeat, write=SetHeartBeat};
	
private:
	System::TDateTime FFirstPing;
	System::TDateTime FLastPing;
	System::TDateTime FLastFrame;
	
protected:
	virtual bool __fastcall DoKeepAlive();
	
private:
	TsgcWSAMQP_Options* FAMQPOptions;
	void __fastcall SetAMQPOptions(TsgcWSAMQP_Options* const Value);
	
public:
	__property TsgcWSAMQP_Options* AMQPOptions = {read=FAMQPOptions, write=SetAMQPOptions};
	
private:
	Sgcamqp::TsgcAMQPAuthenticationEvent FOnAMQPAuthentication;
	Sgcamqp::TsgcAMQPBasicCancelConsumeEvent FOnAMQPBasicCancelConsume;
	Sgcamqp::TsgcAMQPBasicConsumeEvent FOnAMQPBasicConsume;
	Sgcamqp::TsgcAMQPBasicDeliverEvent FOnAMQPBasicDeliver;
	Sgcamqp::TsgcAMQPBasicGetOkEvent FOnAMQPBasicGetOk;
	Sgcamqp::TsgcAMQPChallengeEvent FOnAMQPChallenge;
	Sgcamqp::TsgcAMQPChannelCloseEvent FOnAMQPChannelClose;
	Sgcamqp::TsgcAMQPChannelFlowEvent FOnAMQPChannelFlow;
	Sgcamqp::TsgcAMQPChannelOpenEvent FOnAMQPChannelOpen;
	Sgcamqp::TsgcAMQPConnectEvent FOnAMQPConnect;
	Sgcamqp::TsgcAMQPDisconnectEvent FOnAMQPDisconnect;
	Sgcamqp::TsgcAMQPExchangeDeclareEvent FOnAMQPExchangeDeclare;
	Sgcamqp::TsgcAMQPExchangeDeleteEvent FOnAMQPExchangeDelete;
	Sgcamqp::TsgcAMQPHeartBeatEvent FOnAMQPHeartBeat;
	Sgcamqp::TsgcAMQPBasicQoSEvent FOnAMQPBasicQoS;
	Sgcamqp::TsgcAMQPBasicReturnEvent FOnAMQPBasicReturn;
	Sgcamqp::TsgcAMQPBasicGetEmptyEvent FOnAMQPBasicGetEmpty;
	Sgcamqp::TsgcAMQPBasicRecoverOkEvent FOnAMQPBasicRecoverOk;
	Sgcamqp::TsgcAMQPExceptionEvent FOnAMQPException;
	Sgcamqp::TsgcAMQPQueueBindEvent FOnAMQPQueueBind;
	Sgcamqp::TsgcAMQPQueueDeclareEvent FOnAMQPQueueDeclare;
	Sgcamqp::TsgcAMQPQueueDeleteEvent FOnAMQPQueueDelete;
	Sgcamqp::TsgcAMQPQueuePurgeEvent FOnAMQPQueuePurge;
	Sgcamqp::TsgcAMQPQueueUnBindEvent FOnAMQPQueueUnBind;
	Sgcamqp::TsgcAMQPTransactionOkEvent FOnAMQPTransactionOk;
	
public:
	__property Sgcamqp::TsgcAMQPAuthenticationEvent OnAMQPAuthentication = {read=FOnAMQPAuthentication, write=FOnAMQPAuthentication};
	__property Sgcamqp::TsgcAMQPBasicCancelConsumeEvent OnAMQPBasicCancelConsume = {read=FOnAMQPBasicCancelConsume, write=FOnAMQPBasicCancelConsume};
	__property Sgcamqp::TsgcAMQPBasicConsumeEvent OnAMQPBasicConsume = {read=FOnAMQPBasicConsume, write=FOnAMQPBasicConsume};
	__property Sgcamqp::TsgcAMQPBasicDeliverEvent OnAMQPBasicDeliver = {read=FOnAMQPBasicDeliver, write=FOnAMQPBasicDeliver};
	__property Sgcamqp::TsgcAMQPBasicGetOkEvent OnAMQPBasicGetOk = {read=FOnAMQPBasicGetOk, write=FOnAMQPBasicGetOk};
	__property Sgcamqp::TsgcAMQPChallengeEvent OnAMQPChallenge = {read=FOnAMQPChallenge, write=FOnAMQPChallenge};
	__property Sgcamqp::TsgcAMQPChannelCloseEvent OnAMQPChannelClose = {read=FOnAMQPChannelClose, write=FOnAMQPChannelClose};
	__property Sgcamqp::TsgcAMQPChannelFlowEvent OnAMQPChannelFlow = {read=FOnAMQPChannelFlow, write=FOnAMQPChannelFlow};
	__property Sgcamqp::TsgcAMQPChannelOpenEvent OnAMQPChannelOpen = {read=FOnAMQPChannelOpen, write=FOnAMQPChannelOpen};
	__property Sgcamqp::TsgcAMQPConnectEvent OnAMQPConnect = {read=FOnAMQPConnect, write=FOnAMQPConnect};
	__property Sgcamqp::TsgcAMQPDisconnectEvent OnAMQPDisconnect = {read=FOnAMQPDisconnect, write=FOnAMQPDisconnect};
	__property Sgcamqp::TsgcAMQPExchangeDeclareEvent OnAMQPExchangeDeclare = {read=FOnAMQPExchangeDeclare, write=FOnAMQPExchangeDeclare};
	__property Sgcamqp::TsgcAMQPExchangeDeleteEvent OnAMQPExchangeDelete = {read=FOnAMQPExchangeDelete, write=FOnAMQPExchangeDelete};
	__property Sgcamqp::TsgcAMQPHeartBeatEvent OnAMQPHeartBeat = {read=FOnAMQPHeartBeat, write=FOnAMQPHeartBeat};
	__property Sgcamqp::TsgcAMQPBasicQoSEvent OnAMQPBasicQoS = {read=FOnAMQPBasicQoS, write=FOnAMQPBasicQoS};
	__property Sgcamqp::TsgcAMQPBasicReturnEvent OnAMQPBasicReturn = {read=FOnAMQPBasicReturn, write=FOnAMQPBasicReturn};
	__property Sgcamqp::TsgcAMQPBasicGetEmptyEvent OnAMQPBasicGetEmpty = {read=FOnAMQPBasicGetEmpty, write=FOnAMQPBasicGetEmpty};
	__property Sgcamqp::TsgcAMQPBasicRecoverOkEvent OnAMQPBasicRecoverOk = {read=FOnAMQPBasicRecoverOk, write=FOnAMQPBasicRecoverOk};
	__property Sgcamqp::TsgcAMQPExceptionEvent OnAMQPException = {read=FOnAMQPException, write=FOnAMQPException};
	__property Sgcamqp::TsgcAMQPQueueBindEvent OnAMQPQueueBind = {read=FOnAMQPQueueBind, write=FOnAMQPQueueBind};
	__property Sgcamqp::TsgcAMQPQueueDeclareEvent OnAMQPQueueDeclare = {read=FOnAMQPQueueDeclare, write=FOnAMQPQueueDeclare};
	__property Sgcamqp::TsgcAMQPQueueDeleteEvent OnAMQPQueueDelete = {read=FOnAMQPQueueDelete, write=FOnAMQPQueueDelete};
	__property Sgcamqp::TsgcAMQPQueuePurgeEvent OnAMQPQueuePurge = {read=FOnAMQPQueuePurge, write=FOnAMQPQueuePurge};
	__property Sgcamqp::TsgcAMQPQueueUnBindEvent OnAMQPQueueUnBind = {read=FOnAMQPQueueUnBind, write=FOnAMQPQueueUnBind};
	__property Sgcamqp::TsgcAMQPTransactionOkEvent OnAMQPTransactionOk = {read=FOnAMQPTransactionOk, write=FOnAMQPTransactionOk};
};


class PASCALIMPLEMENTATION TsgcWSProtocol_AMQP_Client : public TsgcWSProtocol_AMQP_Client_Base
{
	typedef TsgcWSProtocol_AMQP_Client_Base inherited;
	
public:
	/* TsgcWSProtocol_AMQP_Client_Base.Create */ inline __fastcall virtual TsgcWSProtocol_AMQP_Client(System::Classes::TComponent* aOwner) : TsgcWSProtocol_AMQP_Client_Base(aOwner) { }
	/* TsgcWSProtocol_AMQP_Client_Base.Destroy */ inline __fastcall virtual ~TsgcWSProtocol_AMQP_Client() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_amqp_client */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_AMQP_CLIENT)
using namespace Sgcwebsocket_protocol_amqp_client;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_amqp_clientHPP
