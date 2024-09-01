// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcAMQP_Client.pas' rev: 35.00 (Windows)

#ifndef Sgcamqp_clientHPP
#define Sgcamqp_clientHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <sgcAMQP.hpp>
#include <sgcAMQP_Classes.hpp>
#include <sgcAMQP_Const.hpp>
#include <sgcBase_Helpers.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcamqp_client
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcAMQP_Client;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcAMQP_Client : public Sgcamqp::TsgcAMQP
{
	typedef Sgcamqp::TsgcAMQP inherited;
	
protected:
	virtual void __fastcall DoRead_ConnStart(Sgcamqp_classes::TsgcAMQPFramePayload_Method_ConnectionStart* const aPayload);
	virtual void __fastcall DoRead_ConnSecure(Sgcamqp_classes::TsgcAMQPFramePayload_Method_ConnectionSecure* const aPayload);
	virtual void __fastcall DoRead_ConnTune(Sgcamqp_classes::TsgcAMQPFramePayload_Method_ConnectionTune* const aPayload);
	virtual void __fastcall DoRead_ConnOpenOk(Sgcamqp_classes::TsgcAMQPFramePayload_Method_ConnectionOpenOk* const aPayload);
	virtual void __fastcall DoRead_ChannOpenOk(Sgcamqp_classes::TsgcAMQPFrame* const aFrame, Sgcamqp_classes::TsgcAMQPFramePayload_Method_ChannelOpenOk* const aPayload);
	virtual void __fastcall DoRead_ExchDeclareOk(Sgcamqp_classes::TsgcAMQPFrame* const aFrame, Sgcamqp_classes::TsgcAMQPFramePayload_Method_ExchangeDeclareOk* const aPayload);
	virtual void __fastcall DoRead_ExchDeleteOk(Sgcamqp_classes::TsgcAMQPFrame* const aFrame, Sgcamqp_classes::TsgcAMQPFramePayload_Method_ExchangeDeleteOk* const aPayload);
	virtual void __fastcall DoRead_QueueDeclareOk(Sgcamqp_classes::TsgcAMQPFrame* const aFrame, Sgcamqp_classes::TsgcAMQPFramePayload_Method_QueueDeclareOk* const aPayload);
	virtual void __fastcall DoRead_QueueBindOk(Sgcamqp_classes::TsgcAMQPFrame* const aFrame, Sgcamqp_classes::TsgcAMQPFramePayload_Method_QueueBindOk* const aPayload);
	virtual void __fastcall DoRead_QueueUnBindOk(Sgcamqp_classes::TsgcAMQPFrame* const aFrame, Sgcamqp_classes::TsgcAMQPFramePayload_Method_QueueUnBindOk* const aPayload);
	virtual void __fastcall DoRead_QueuePurgeOk(Sgcamqp_classes::TsgcAMQPFrame* const aFrame, Sgcamqp_classes::TsgcAMQPFramePayload_Method_QueuePurgeOk* const aPayload);
	virtual void __fastcall DoRead_QueueDeleteOk(Sgcamqp_classes::TsgcAMQPFrame* const aFrame, Sgcamqp_classes::TsgcAMQPFramePayload_Method_QueueDeleteOk* const aPayload);
	virtual void __fastcall DoRead_BasicQoSOk(Sgcamqp_classes::TsgcAMQPFrame* const aFrame, Sgcamqp_classes::TsgcAMQPFramePayload_Method_BasicQoSOk* const aPayload);
	virtual void __fastcall DoRead_BasicConsumeOk(Sgcamqp_classes::TsgcAMQPFrame* const aFrame, Sgcamqp_classes::TsgcAMQPFramePayload_Method_BasicConsumeOk* const aPayload);
	virtual void __fastcall DoRead_BasicCancelOk(Sgcamqp_classes::TsgcAMQPFrame* const aFrame, Sgcamqp_classes::TsgcAMQPFramePayload_Method_BasicCancelOk* const aPayload);
	virtual void __fastcall DoRead_BasicReturn(Sgcamqp_classes::TsgcAMQPFrame* const aFrame, Sgcamqp_classes::TsgcAMQPFramePayload_Method_BasicReturn* const aPayload);
	virtual void __fastcall DoRead_BasicDeliver(Sgcamqp_classes::TsgcAMQPFrame* const aFrame, Sgcamqp_classes::TsgcAMQPFramePayload_Method_BasicDeliver* const aPayload);
	virtual void __fastcall DoRead_BasicGetOk(Sgcamqp_classes::TsgcAMQPFrame* const aFrame, Sgcamqp_classes::TsgcAMQPFramePayload_Method_BasicGetOk* const aPayload);
	virtual void __fastcall DoRead_BasicGetEmpty(Sgcamqp_classes::TsgcAMQPFrame* const aFrame, Sgcamqp_classes::TsgcAMQPFramePayload_Method_BasicGetEmpty* const aPayload);
	virtual void __fastcall DoRead_BasicRecoverOk(Sgcamqp_classes::TsgcAMQPFrame* const aFrame, Sgcamqp_classes::TsgcAMQPFramePayload_Method_BasicRecoverOk* const aPayload);
	virtual void __fastcall DoRead_TxSelectOk(Sgcamqp_classes::TsgcAMQPFrame* const aFrame, Sgcamqp_classes::TsgcAMQPFramePayload_Method_TxSelectOk* const aPayload);
	virtual void __fastcall DoRead_TxCommitOk(Sgcamqp_classes::TsgcAMQPFrame* const aFrame, Sgcamqp_classes::TsgcAMQPFramePayload_Method_TxCommitOk* const aPayload);
	virtual void __fastcall DoRead_TxRollbackOk(Sgcamqp_classes::TsgcAMQPFrame* const aFrame, Sgcamqp_classes::TsgcAMQPFramePayload_Method_TxRollbackOk* const aPayload);
	void __fastcall DoMessageContentEvent(Sgcamqp_classes::TsgcAMQPChannelThreadItem* const aChannel);
	virtual void __fastcall DoReadFrameMethod(Sgcamqp_classes::TsgcAMQPFrame* const aFrame, Sgcamqp_classes::TsgcAMQPFramePayloadType_Method* const aMethod);
	virtual void __fastcall DoReadFrameHeader(Sgcamqp_classes::TsgcAMQPFrame* const aFrame, Sgcamqp_classes::TsgcAMQPFramePayloadType_ContentHeader* const aHeader);
	virtual void __fastcall DoReadFrameBody(Sgcamqp_classes::TsgcAMQPFrame* const aFrame, Sgcamqp_classes::TsgcAMQPFramePayloadType_ContentBody* const aBody);
	
private:
	void __fastcall DoWrite_ConnSecureOk(Sgcamqp_classes::TsgcAMQPFramePayload_Method_ConnectionSecure* const aPayload);
	void __fastcall DoWrite_ConnStartOk(Sgcamqp_classes::TsgcAMQPFramePayload_Method_ConnectionStart* const aPayload);
	void __fastcall DoWrite_ConnTuneOk(Sgcamqp_classes::TsgcAMQPFramePayload_Method_ConnectionTune* const aPayload);
	void __fastcall DoWrite_ConnOpen();
	
public:
	void __fastcall Connect();
	void __fastcall Disconnect();
	
private:
	void __fastcall DoWrite_ChannOpen(System::Word aChannelId);
	
public:
	void __fastcall OpenChannel(const System::UnicodeString aChannel);
	bool __fastcall OpenChannelEx(const System::UnicodeString aChannel, int aTimeout = 0x2710);
	
private:
	void __fastcall DoWrite_ExchDeclare(Sgcamqp_classes::TsgcAMQPChannelThreadItem* const aChannel, const System::UnicodeString aExchange, const System::UnicodeString aExchangeType, bool aPassive, bool aDurable, bool aAutoDelete, bool aInternal, bool aNoWait);
	void __fastcall DoWrite_ExchDelete(Sgcamqp_classes::TsgcAMQPChannelThreadItem* const aChannel, const System::UnicodeString aExchange, bool aIfUnused, bool aNoWait);
	
public:
	void __fastcall DeclareExchange(const System::UnicodeString aChannel, const System::UnicodeString aExchange, const System::UnicodeString aExchangeType, bool aPassive, bool aDurable, bool aAutoDelete, bool aInternal, bool aNoWait);
	bool __fastcall DeclareExchangeEx(const System::UnicodeString aChannel, const System::UnicodeString aExchange, const System::UnicodeString aExchangeType, bool aPassive, bool aDurable, bool aAutoDelete, bool aInternal, bool aNoWait, int aTimeout = 0x2710);
	void __fastcall DeleteExchange(const System::UnicodeString aChannel, const System::UnicodeString aExchange, bool aIfUnused, bool aNoWait);
	bool __fastcall DeleteExchangeEx(const System::UnicodeString aChannel, const System::UnicodeString aExchange, bool aIfUnused, bool aNoWait, int aTimeout = 0x2710);
	
private:
	void __fastcall DoWrite_QueueDeclare(Sgcamqp_classes::TsgcAMQPChannelThreadItem* const aChannel, const System::UnicodeString aQueue, bool aPassive, bool aDurable, bool aExclusive, bool aAutoDelete, bool aNoWait);
	void __fastcall DoWrite_QueueBind(Sgcamqp_classes::TsgcAMQPChannelThreadItem* const aChannel, const System::UnicodeString aQueue, const System::UnicodeString aExchange, const System::UnicodeString aRoutingKey, bool aNoWait);
	void __fastcall DoWrite_QueueUnBind(Sgcamqp_classes::TsgcAMQPChannelThreadItem* const aChannel, const System::UnicodeString aQueue, const System::UnicodeString aExchange, const System::UnicodeString aRoutingKey);
	void __fastcall DoWrite_QueuePurge(Sgcamqp_classes::TsgcAMQPChannelThreadItem* const aChannel, const System::UnicodeString aQueue, bool aNoWait);
	void __fastcall DoWrite_QueueDelete(Sgcamqp_classes::TsgcAMQPChannelThreadItem* const aChannel, const System::UnicodeString aQueue, bool aIfUnused, bool aIfEmpty, bool aNoWait);
	
public:
	void __fastcall DeclareQueue(const System::UnicodeString aChannel, const System::UnicodeString aQueue, bool aPassive, bool aDurable, bool aExclusive, bool aAutoDelete, bool aNoWait);
	bool __fastcall DeclareQueueEx(const System::UnicodeString aChannel, const System::UnicodeString aQueue, bool aPassive, bool aDurable, bool aExclusive, bool aAutoDelete, bool aNoWait, int aTimeout = 0x2710);
	void __fastcall BindQueue(const System::UnicodeString aChannel, const System::UnicodeString aQueue, const System::UnicodeString aExchange, const System::UnicodeString aRoutingKey, bool aNoWait);
	bool __fastcall BindQueueEx(const System::UnicodeString aChannel, const System::UnicodeString aQueue, const System::UnicodeString aExchange, const System::UnicodeString aRoutingKey, bool aNoWait, int aTimeout = 0x2710);
	void __fastcall UnBindQueue(const System::UnicodeString aChannel, const System::UnicodeString aQueue, const System::UnicodeString aExchange, const System::UnicodeString aRoutingKey);
	bool __fastcall UnBindQueueEx(const System::UnicodeString aChannel, const System::UnicodeString aQueue, const System::UnicodeString aExchange, const System::UnicodeString aRoutingKey, int aTimeout = 0x2710);
	void __fastcall PurgeQueue(const System::UnicodeString aChannel, const System::UnicodeString aQueue, bool aNoWait);
	bool __fastcall PurgeQueueEx(const System::UnicodeString aChannel, const System::UnicodeString aQueue, bool aNoWait, int aTimeout = 0x2710);
	void __fastcall DeleteQueue(const System::UnicodeString aChannel, const System::UnicodeString aQueue, bool aIfUnused, bool aIfEmpty, bool aNoWait);
	bool __fastcall DeleteQueueEx(const System::UnicodeString aChannel, const System::UnicodeString aQueue, bool aIfUnused, bool aIfEmpty, bool aNoWait, int aTimeout = 0x2710);
	
private:
	void __fastcall DoWriteContentHeader(Sgcamqp_classes::TsgcAMQPChannelThreadItem* const aChannel, Sgcamqp_classes::TsgcAMQPClass aClassId, Sgcamqp_classes::TsgcAMQPBasicProperties* const aHeader, System::Classes::TStream* const aBody, System::DynamicArray<System::Byte> &aBytes);
	void __fastcall DoWriteContentBody(Sgcamqp_classes::TsgcAMQPChannelThreadItem* const aChannel, System::Classes::TStream* const aData, System::DynamicArray<System::Byte> &aBytes);
	void __fastcall DoWrite_BasicQoS(Sgcamqp_classes::TsgcAMQPChannelThreadItem* const aChannel, unsigned aPrefetchSize, System::Word aPrefetchCount, bool aGlobal);
	void __fastcall DoWrite_BasicConsume(Sgcamqp_classes::TsgcAMQPChannelThreadItem* const aChannel, const System::UnicodeString aQueue, const System::UnicodeString aConsumerTag, bool aNoLocal, bool aNoAck, bool aExclusive, bool aNoWait);
	void __fastcall DoWrite_BasicCancel(Sgcamqp_classes::TsgcAMQPChannelThreadItem* const aChannel, const System::UnicodeString aConsumerTag, bool aNoWait);
	void __fastcall DoWrite_BasicPublish(Sgcamqp_classes::TsgcAMQPChannelThreadItem* const aChannel, const System::UnicodeString aExchange, const System::UnicodeString aRoutingKey, Sgcamqp_classes::TsgcAMQPBasicProperties* const aHeader, System::Classes::TStream* const aBody, bool aMandatory, bool aImmediate);
	void __fastcall DoWrite_BasicGet(Sgcamqp_classes::TsgcAMQPChannelThreadItem* const aChannel, const System::UnicodeString aQueue, bool aNoAck);
	void __fastcall DoWrite_BasicAck(Sgcamqp_classes::TsgcAMQPChannelThreadItem* const aChannel, const unsigned __int64 aDeliveryTag, bool aMultiple);
	void __fastcall DoWrite_BasicReject(Sgcamqp_classes::TsgcAMQPChannelThreadItem* const aChannel, const unsigned __int64 aDeliveryTag, bool aRequeue);
	void __fastcall DoWrite_BasicRecoverAsync(Sgcamqp_classes::TsgcAMQPChannelThreadItem* const aChannel, bool aRequeue);
	void __fastcall DoWrite_BasicRecover(Sgcamqp_classes::TsgcAMQPChannelThreadItem* const aChannel, bool aRequeue);
	
public:
	void __fastcall SetQoS(const System::UnicodeString aChannel, unsigned aPrefetchSize, System::Word aPrefetchCount, bool aGlobal);
	bool __fastcall SetQoSEx(const System::UnicodeString aChannel, unsigned aPrefetchSize, System::Word aPrefetchCount, bool aGlobal, int aTimeout = 0x2710);
	void __fastcall Consume(const System::UnicodeString aChannel, const System::UnicodeString aQueue, const System::UnicodeString aConsumerTag, bool aNoLocal, bool aNoAck, bool aExclusive, bool aNoWait);
	bool __fastcall ConsumeEx(const System::UnicodeString aChannel, const System::UnicodeString aQueue, const System::UnicodeString aConsumerTag, bool aNoLocal, bool aNoAck, bool aExclusive, int aTimeout = 0x2710);
	void __fastcall CancelConsume(const System::UnicodeString aChannel, const System::UnicodeString aConsumerTag, bool aNoWait);
	bool __fastcall CancelConsumeEx(const System::UnicodeString aChannel, const System::UnicodeString aConsumerTag, int aTimeout = 0x2710);
	void __fastcall PublishMessage(const System::UnicodeString aChannel, const System::UnicodeString aExchange, const System::UnicodeString aRoutingKey, Sgcamqp_classes::TsgcAMQPBasicProperties* const aHeader, const System::UnicodeString aMessage, bool aMandatory = false, bool aImmediate = false)/* overload */;
	void __fastcall PublishMessage(const System::UnicodeString aChannel, const System::UnicodeString aExchange, const System::UnicodeString aRoutingKey, Sgcamqp_classes::TsgcAMQPBasicProperties* const aHeader, System::Classes::TStream* const aMessage, bool aMandatory, bool aImmediate)/* overload */;
	void __fastcall PublishMessage(const System::UnicodeString aChannel, const System::UnicodeString aExchange, const System::UnicodeString aRoutingKey, const System::UnicodeString aMessage, bool aMandatory = false, bool aImmediate = false)/* overload */;
	void __fastcall PublishMessage(const System::UnicodeString aChannel, const System::UnicodeString aExchange, const System::UnicodeString aRoutingKey, System::Classes::TStream* const aMessage, bool aMandatory = false, bool aImmediate = false)/* overload */;
	void __fastcall GetMessage(const System::UnicodeString aChannel, const System::UnicodeString aQueue, bool aNoAck);
	bool __fastcall GetMessageEx(const System::UnicodeString aChannel, const System::UnicodeString aQueue, int aTimeout = 0x2710);
	void __fastcall AckMessage(const System::UnicodeString aChannel, unsigned __int64 aDeliveryTag, bool aMultiple);
	void __fastcall RejectMessage(const System::UnicodeString aChannel, unsigned __int64 aDeliveryTag, bool aRequeue);
	void __fastcall RecoverAsync(const System::UnicodeString aChannel, bool aRequeue);
	void __fastcall Recover(const System::UnicodeString aChannel, bool aRequeue);
	bool __fastcall RecoverEx(const System::UnicodeString aChannel, bool aRequeue, int aTimeout = 0x2710);
	
private:
	void __fastcall DoWrite_TxSelect(Sgcamqp_classes::TsgcAMQPChannelThreadItem* const aChannel);
	void __fastcall DoWrite_TxCommit(Sgcamqp_classes::TsgcAMQPChannelThreadItem* const aChannel);
	void __fastcall DoWrite_TxRollback(Sgcamqp_classes::TsgcAMQPChannelThreadItem* const aChannel);
	
public:
	void __fastcall SelectTransaction(const System::UnicodeString aChannel);
	bool __fastcall SelectTransactionEx(const System::UnicodeString aChannel, int aTimeout = 0x2710);
	void __fastcall CommitTransaction(const System::UnicodeString aChannel);
	bool __fastcall CommitTransactionEx(const System::UnicodeString aChannel, int aTimeout = 0x2710);
	void __fastcall RollbackTransaction(const System::UnicodeString aChannel);
	bool __fastcall RollbackTransactionEx(const System::UnicodeString aChannel, int aTimeout = 0x2710);
	
private:
	Sgcamqp::TsgcAMQPAuthenticationEvent FOnAMQPAuthentication;
	Sgcamqp::TsgcAMQPBasicCancelConsumeEvent FOnAMQPBasicCancelConsume;
	Sgcamqp::TsgcAMQPChallengeEvent FOnAMQPChallenge;
	Sgcamqp::TsgcAMQPConnectEvent FOnAMQPConnect;
	Sgcamqp::TsgcAMQPBasicConsumeEvent FOnAMQPBasicConsume;
	Sgcamqp::TsgcAMQPBasicDeliverEvent FOnAMQPBasicDeliver;
	Sgcamqp::TsgcAMQPBasicGetEmptyEvent FOnAMQPBasicGetEmpty;
	Sgcamqp::TsgcAMQPBasicGetOkEvent FOnAMQPBasicGetOk;
	Sgcamqp::TsgcAMQPExchangeDeclareEvent FOnAMQPExchangeDeclare;
	Sgcamqp::TsgcAMQPExchangeDeleteEvent FOnAMQPExchangeDelete;
	Sgcamqp::TsgcAMQPBasicQoSEvent FOnAMQPBasicQoS;
	Sgcamqp::TsgcAMQPBasicReturnEvent FOnAMQPBasicReturn;
	Sgcamqp::TsgcAMQPQueueBindEvent FOnAMQPQueueBind;
	Sgcamqp::TsgcAMQPQueueDeclareEvent FOnAMQPQueueDeclare;
	Sgcamqp::TsgcAMQPQueueDeleteEvent FOnAMQPQueueDelete;
	Sgcamqp::TsgcAMQPQueuePurgeEvent FOnAMQPQueuePurge;
	Sgcamqp::TsgcAMQPQueueUnBindEvent FOnAMQPQueueUnBind;
	Sgcamqp::TsgcAMQPBasicRecoverOkEvent FOnAMQPBasicRecoverOk;
	Sgcamqp::TsgcAMQPTransactionOkEvent FOnAMQPTransactionOk;
	
public:
	__property Sgcamqp::TsgcAMQPAuthenticationEvent OnAMQPAuthentication = {read=FOnAMQPAuthentication, write=FOnAMQPAuthentication};
	__property Sgcamqp::TsgcAMQPBasicCancelConsumeEvent OnAMQPBasicCancelConsume = {read=FOnAMQPBasicCancelConsume, write=FOnAMQPBasicCancelConsume};
	__property Sgcamqp::TsgcAMQPChallengeEvent OnAMQPChallenge = {read=FOnAMQPChallenge, write=FOnAMQPChallenge};
	__property Sgcamqp::TsgcAMQPConnectEvent OnAMQPConnect = {read=FOnAMQPConnect, write=FOnAMQPConnect};
	__property Sgcamqp::TsgcAMQPBasicConsumeEvent OnAMQPBasicConsume = {read=FOnAMQPBasicConsume, write=FOnAMQPBasicConsume};
	__property Sgcamqp::TsgcAMQPBasicDeliverEvent OnAMQPBasicDeliver = {read=FOnAMQPBasicDeliver, write=FOnAMQPBasicDeliver};
	__property Sgcamqp::TsgcAMQPBasicGetEmptyEvent OnAMQPBasicGetEmpty = {read=FOnAMQPBasicGetEmpty, write=FOnAMQPBasicGetEmpty};
	__property Sgcamqp::TsgcAMQPBasicGetOkEvent OnAMQPBasicGetOk = {read=FOnAMQPBasicGetOk, write=FOnAMQPBasicGetOk};
	__property Sgcamqp::TsgcAMQPExchangeDeclareEvent OnAMQPExchangeDeclare = {read=FOnAMQPExchangeDeclare, write=FOnAMQPExchangeDeclare};
	__property Sgcamqp::TsgcAMQPExchangeDeleteEvent OnAMQPExchangeDelete = {read=FOnAMQPExchangeDelete, write=FOnAMQPExchangeDelete};
	__property Sgcamqp::TsgcAMQPBasicQoSEvent OnAMQPBasicQoS = {read=FOnAMQPBasicQoS, write=FOnAMQPBasicQoS};
	__property Sgcamqp::TsgcAMQPBasicReturnEvent OnAMQPBasicReturn = {read=FOnAMQPBasicReturn, write=FOnAMQPBasicReturn};
	__property Sgcamqp::TsgcAMQPQueueBindEvent OnAMQPQueueBind = {read=FOnAMQPQueueBind, write=FOnAMQPQueueBind};
	__property Sgcamqp::TsgcAMQPQueueUnBindEvent OnAMQPQueueUnBind = {read=FOnAMQPQueueUnBind, write=FOnAMQPQueueUnBind};
	__property Sgcamqp::TsgcAMQPBasicRecoverOkEvent OnAMQPBasicRecoverOk = {read=FOnAMQPBasicRecoverOk, write=FOnAMQPBasicRecoverOk};
	__property Sgcamqp::TsgcAMQPQueueDeclareEvent OnAMQPQueueDeclare = {read=FOnAMQPQueueDeclare, write=FOnAMQPQueueDeclare};
	__property Sgcamqp::TsgcAMQPQueueDeleteEvent OnAMQPQueueDelete = {read=FOnAMQPQueueDelete, write=FOnAMQPQueueDelete};
	__property Sgcamqp::TsgcAMQPQueuePurgeEvent OnAMQPQueuePurge = {read=FOnAMQPQueuePurge, write=FOnAMQPQueuePurge};
	__property Sgcamqp::TsgcAMQPTransactionOkEvent OnAMQPTransactionOk = {read=FOnAMQPTransactionOk, write=FOnAMQPTransactionOk};
public:
	/* TsgcAMQP.Create */ inline __fastcall virtual TsgcAMQP_Client(System::Classes::TComponent* aOwner) : Sgcamqp::TsgcAMQP(aOwner) { }
	/* TsgcAMQP.Destroy */ inline __fastcall virtual ~TsgcAMQP_Client() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcamqp_client */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCAMQP_CLIENT)
using namespace Sgcamqp_client;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcamqp_clientHPP
