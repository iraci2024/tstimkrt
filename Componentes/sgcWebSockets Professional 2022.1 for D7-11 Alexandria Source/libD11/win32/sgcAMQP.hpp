// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcAMQP.pas' rev: 35.00 (Windows)

#ifndef SgcamqpHPP
#define SgcamqpHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <sgcAMQP_Classes.hpp>
#include <sgcAMQP_Const.hpp>
#include <sgcBase_Helpers.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcamqp
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcAMQPHeartBeat;
class DELPHICLASS TsgcAMQPOptions;
class DELPHICLASS TsgcAMQPNegotiatedValues;
class DELPHICLASS TsgcAMQP;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcAMQPBeforeReadFrameEvent)(System::TObject* Sender, Sgcamqp_classes::TsgcAMQPFrame* const aFrame, bool &Handled);

typedef System::DynamicArray<System::Byte> _dt_Sgcamqp_1;
typedef void __fastcall (__closure *TsgcAMQPWriteBytesEvent)(System::TObject* Sender, const _dt_Sgcamqp_1 aBytes);

typedef void __fastcall (__closure *TsgcAMQPAuthenticationEvent)(System::TObject* Sender, Sgcamqp_classes::TsgcAMQPAuthentications aMechanisms, Sgcamqp_classes::TsgcAMQPAuthentication &Mechanism, System::UnicodeString &User, System::UnicodeString &Password);

typedef void __fastcall (__closure *TsgcAMQPChallengeEvent)(System::TObject* Sender, const System::UnicodeString aChallenge, System::UnicodeString &Challenge);

typedef void __fastcall (__closure *TsgcAMQPConnectEvent)(System::TObject* Sender);

typedef void __fastcall (__closure *TsgcAMQPHeartBeatEvent)(System::TObject* Sender);

typedef void __fastcall (__closure *TsgcAMQPDisconnectEvent)(System::TObject* Sender, Sgcamqp_classes::TsgcAMQPFramePayload_Method_ConnectionClose* const aClose, bool aAck);

typedef void __fastcall (__closure *TsgcAMQPChannelOpenEvent)(System::TObject* Sender, const System::UnicodeString aChannel);

typedef void __fastcall (__closure *TsgcAMQPChannelCloseEvent)(System::TObject* Sender, const System::UnicodeString aChannel, Sgcamqp_classes::TsgcAMQPFramePayload_Method_ChannelClose* const aChannelClose, bool aAck);

typedef void __fastcall (__closure *TsgcAMQPChannelFlowEvent)(System::TObject* Sender, const System::UnicodeString aChannel, bool aFlow, bool aAck);

typedef void __fastcall (__closure *TsgcAMQPExchangeDeclareEvent)(System::TObject* Sender, const System::UnicodeString aChannel, const System::UnicodeString aExchange);

typedef void __fastcall (__closure *TsgcAMQPExchangeDeleteEvent)(System::TObject* Sender, const System::UnicodeString aChannel, const System::UnicodeString aExchange);

typedef void __fastcall (__closure *TsgcAMQPQueueDeclareEvent)(System::TObject* Sender, const System::UnicodeString aChannel, const System::UnicodeString aQueue, int aMessageCount, int aConsumerCount);

typedef void __fastcall (__closure *TsgcAMQPQueueBindEvent)(System::TObject* Sender, const System::UnicodeString aChannel, const System::UnicodeString aQueue, const System::UnicodeString aExchange);

typedef void __fastcall (__closure *TsgcAMQPQueueUnBindEvent)(System::TObject* Sender, const System::UnicodeString aChannel, const System::UnicodeString aQueue, const System::UnicodeString aExchange);

typedef void __fastcall (__closure *TsgcAMQPQueuePurgeEvent)(System::TObject* Sender, const System::UnicodeString aChannel, const System::UnicodeString aQueue, int aMessageCount);

typedef void __fastcall (__closure *TsgcAMQPQueueDeleteEvent)(System::TObject* Sender, const System::UnicodeString aChannel, const System::UnicodeString aQueue, int aMessageCount);

typedef void __fastcall (__closure *TsgcAMQPBasicQoSEvent)(System::TObject* Sender, const System::UnicodeString aChannel, Sgcamqp_classes::TsgcAMQPFramePayload_Method_BasicQoS* const aQoS);

typedef void __fastcall (__closure *TsgcAMQPBasicConsumeEvent)(System::TObject* Sender, const System::UnicodeString aChannel, const System::UnicodeString aConsumerTag);

typedef void __fastcall (__closure *TsgcAMQPBasicCancelConsumeEvent)(System::TObject* Sender, const System::UnicodeString aChannel, const System::UnicodeString aConsumerTag);

typedef void __fastcall (__closure *TsgcAMQPBasicReturnEvent)(System::TObject* Sender, const System::UnicodeString aChannel, Sgcamqp_classes::TsgcAMQPFramePayload_Method_BasicReturn* const aReturn, Sgcamqp_classes::TsgcAMQPMessageContent* const aContent);

typedef void __fastcall (__closure *TsgcAMQPBasicDeliverEvent)(System::TObject* Sender, const System::UnicodeString aChannel, Sgcamqp_classes::TsgcAMQPFramePayload_Method_BasicDeliver* const aDeliver, Sgcamqp_classes::TsgcAMQPMessageContent* const aContent);

typedef void __fastcall (__closure *TsgcAMQPBasicGetOkEvent)(System::TObject* Sender, const System::UnicodeString aChannel, Sgcamqp_classes::TsgcAMQPFramePayload_Method_BasicGetOk* const aGetOk, Sgcamqp_classes::TsgcAMQPMessageContent* const aContent);

typedef void __fastcall (__closure *TsgcAMQPBasicGetEmptyEvent)(System::TObject* Sender, const System::UnicodeString aChannel);

typedef void __fastcall (__closure *TsgcAMQPBasicRecoverOkEvent)(System::TObject* Sender, const System::UnicodeString aChannel);

typedef void __fastcall (__closure *TsgcAMQPTransactionOkEvent)(System::TObject* Sender, const System::UnicodeString aChannel, Sgcamqp_classes::TsgcAMQPTransaction aTransaction);

typedef void __fastcall (__closure *TsgcAMQPExceptionEvent)(System::TObject* Sender, System::Sysutils::Exception* E);

enum DECLSPEC_DENUM TsgcAMQPRead : unsigned char { amqpReadWait, amqpReadWaitData };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPHeartBeat : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	int FInterval;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property int Interval = {read=FInterval, write=FInterval, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcAMQPHeartBeat() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcAMQPHeartBeat() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPOptions : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FLocale;
	System::Word FMaxChannels;
	unsigned FMaxFrameSize;
	System::UnicodeString FVirtualHost;
	
public:
	__fastcall virtual TsgcAMQPOptions();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	__property System::UnicodeString Locale = {read=FLocale, write=FLocale};
	__property System::Word MaxChannels = {read=FMaxChannels, write=FMaxChannels, nodefault};
	__property unsigned MaxFrameSize = {read=FMaxFrameSize, write=FMaxFrameSize, nodefault};
	__property System::UnicodeString VirtualHost = {read=FVirtualHost, write=FVirtualHost};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcAMQPOptions() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPNegotiatedValues : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Word FHeartBeat;
	System::UnicodeString FLocale;
	System::Word FMaxChannels;
	unsigned FMaxFrameSize;
	
public:
	__property System::Word HeartBeat = {read=FHeartBeat, write=FHeartBeat, nodefault};
	__property System::UnicodeString Locale = {read=FLocale, write=FLocale};
	__property System::Word MaxChannels = {read=FMaxChannels, write=FMaxChannels, nodefault};
	__property unsigned MaxFrameSize = {read=FMaxFrameSize, write=FMaxFrameSize, nodefault};
public:
	/* TObject.Create */ inline __fastcall TsgcAMQPNegotiatedValues() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPNegotiatedValues() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcAMQP : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
protected:
	virtual Sgcamqp_classes::TsgcAMQPChannelThreadItem* __fastcall GetChannel(const System::UnicodeString aChannel, bool aRaiseIfNotFound = true);
	
private:
	Sgcamqp_classes::TsgcAMQPChannelThreads* FChannels;
	Sgcamqp_classes::TsgcAMQPChannelThreads* __fastcall GetChannels();
	
protected:
	virtual void __fastcall OnChannelExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	
public:
	__property Sgcamqp_classes::TsgcAMQPChannelThreads* Channels = {read=GetChannels, write=FChannels};
	
protected:
	void __fastcall DoInitWaitAMQPMethod(const System::UnicodeString aChannel, const Sgcamqp_classes::TsgcAMQPMethods &aMethods);
	bool __fastcall DoWaitAMQPMethod(const System::UnicodeString aChannel, const int aTimeout = 0x2710, const Sgcamqp_classes::TsgcAMQPMethods &aMethodsFalse = Sgcamqp_classes::TsgcAMQPMethods() );
	
private:
	System::DynamicArray<System::Byte> FBytes;
	Sgcamqp_classes::TsgcAMQPFramePayload_Method_ConnectionClose* FConnectionClose;
	Sgcamqp_classes::TsgcAMQPFramePayload_Method_ConnectionClose* __fastcall GetConnectionClose();
	
protected:
	bool FConnected;
	__property Sgcamqp_classes::TsgcAMQPFramePayload_Method_ConnectionClose* ConnectionClose = {read=GetConnectionClose, write=FConnectionClose};
	
private:
	void __fastcall DoRead_ConnClose(Sgcamqp_classes::TsgcAMQPFramePayload_Method_ConnectionClose* const aPayload);
	void __fastcall DoRead_ConnCloseOk(Sgcamqp_classes::TsgcAMQPFramePayload_Method_ConnectionCloseOk* const aPayload);
	void __fastcall DoRead_ChannFlow(System::Word aChannelId, Sgcamqp_classes::TsgcAMQPFramePayload_Method_ChannelFlow* const aPayload);
	void __fastcall DoRead_ChannFlowOk(System::Word aChannelId, Sgcamqp_classes::TsgcAMQPFramePayload_Method_ChannelFlowOk* const aPayload);
	void __fastcall DoRead_ChannClose(System::Word aChannelId, Sgcamqp_classes::TsgcAMQPFramePayload_Method_ChannelClose* const aPayload);
	void __fastcall DoRead_ChannCloseOk(System::Word aChannelId, Sgcamqp_classes::TsgcAMQPFramePayload_Method_ChannelCloseOk* const aPayload);
	
protected:
	virtual void __fastcall OnReadFrameEvent(System::TObject* Sender);
	virtual void __fastcall DoReadFrameMethod(Sgcamqp_classes::TsgcAMQPFrame* const aFrame, Sgcamqp_classes::TsgcAMQPFramePayloadType_Method* const aMethod);
	virtual void __fastcall DoReadFrameHeader(Sgcamqp_classes::TsgcAMQPFrame* const aFrame, Sgcamqp_classes::TsgcAMQPFramePayloadType_ContentHeader* const aHeader) = 0 ;
	virtual void __fastcall DoReadFrameBody(Sgcamqp_classes::TsgcAMQPFrame* const aFrame, Sgcamqp_classes::TsgcAMQPFramePayloadType_ContentBody* const aBody) = 0 ;
	virtual void __fastcall DoReadFrameHeartBeat(Sgcamqp_classes::TsgcAMQPFrame* const aFrame);
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	
public:
	void __fastcall Read(System::Classes::TMemoryStream* const aData)/* overload */;
	void __fastcall Read(const System::DynamicArray<System::Byte> aBytes)/* overload */;
	
private:
	void __fastcall DoWrite_ConnClose(System::Word aReplyCode, const System::UnicodeString aReplyText, Sgcamqp_classes::TsgcAMQPClass aFailClassId, Sgcamqp_classes::TsgcAMQPMethod aFailMethodId);
	void __fastcall DoWrite_ConnCloseOk();
	
public:
	void __fastcall Ping();
	void __fastcall Close(System::Word aReplyCode, const System::UnicodeString aReplyText, Sgcamqp_classes::TsgcAMQPClass aFailClassId, Sgcamqp_classes::TsgcAMQPMethod aFailMethodId);
	bool __fastcall CloseEx(System::Word aReplyCode, const System::UnicodeString aReplyText, Sgcamqp_classes::TsgcAMQPClass aFailClassId, Sgcamqp_classes::TsgcAMQPMethod aFailMethodId, int aTimeout = 0x2710);
	
private:
	void __fastcall DoWrite_ChannClose(System::Word aChannelId, System::Word aReplyCode, const System::UnicodeString aReplyText, Sgcamqp_classes::TsgcAMQPClass aFailClassId, Sgcamqp_classes::TsgcAMQPMethod aFailMethodId);
	void __fastcall DoWrite_ChannCloseOk();
	void __fastcall DoWrite_ChannFlow(System::Word aChannelId, bool aActive);
	void __fastcall DoWrite_ChannFlowOk(System::Word aChannelId, bool aActive);
	
public:
	void __fastcall CloseChannel(const System::UnicodeString aChannel, System::Word aReplyCode, const System::UnicodeString aReplyText, Sgcamqp_classes::TsgcAMQPClass aFailClassId, Sgcamqp_classes::TsgcAMQPMethod aFailMethodId);
	bool __fastcall CloseChannelEx(const System::UnicodeString aChannel, System::Word aReplyCode, const System::UnicodeString aReplyText, Sgcamqp_classes::TsgcAMQPClass aFailClassId, Sgcamqp_classes::TsgcAMQPMethod aFailMethodId, int aTimeout = 0x2710);
	void __fastcall EnableChannel(const System::UnicodeString aChannel);
	bool __fastcall EnableChannelEx(const System::UnicodeString aChannel, int aTimeout = 0x2710);
	void __fastcall DisableChannel(const System::UnicodeString aChannel);
	bool __fastcall DisableChannelEx(const System::UnicodeString aChannel, int aTimeout = 0x2710);
	
private:
	TsgcAMQPHeartBeat* FHeartBeat;
	TsgcAMQPOptions* FAMQPOptions;
	TsgcAMQPNegotiatedValues* FAMQPNegotiatedValues;
	TsgcAMQPNegotiatedValues* __fastcall GetAMQPNegotiatedValues();
	void __fastcall SetAMQPOptions(TsgcAMQPOptions* const Value);
	void __fastcall SetHeartBeat(TsgcAMQPHeartBeat* const Value);
	
public:
	__property TsgcAMQPOptions* AMQPOptions = {read=FAMQPOptions, write=SetAMQPOptions};
	__property TsgcAMQPHeartBeat* HeartBeat = {read=FHeartBeat, write=SetHeartBeat};
	__property TsgcAMQPNegotiatedValues* AMQPNegotiatedValues = {read=GetAMQPNegotiatedValues, write=FAMQPNegotiatedValues};
	
protected:
	virtual void __fastcall DoClear();
	
public:
	void __fastcall Clear();
	__fastcall virtual TsgcAMQP(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcAMQP();
	
protected:
	TsgcAMQPBeforeReadFrameEvent FOnAMQPBeforeReadFrame;
	TsgcAMQPChannelOpenEvent FOnAMQPChannelOpen;
	TsgcAMQPChannelCloseEvent FOnAMQPChannelClose;
	TsgcAMQPDisconnectEvent FOnAMQPDisconnect;
	TsgcAMQPHeartBeatEvent FOnAMQPHeartBeat;
	TsgcAMQPWriteBytesEvent FOnAMQPWriteBytes;
	TsgcAMQPChannelFlowEvent FOnAMQPChannelFlow;
	TsgcAMQPExceptionEvent FOnAMQPException;
	virtual void __fastcall DoWriteBytes(const System::DynamicArray<System::Byte> aBytes);
	virtual void __fastcall DoAMQPExceptionEvent(System::Sysutils::Exception* E);
	
public:
	__property TsgcAMQPBeforeReadFrameEvent OnAMQPBeforeReadFrame = {read=FOnAMQPBeforeReadFrame, write=FOnAMQPBeforeReadFrame};
	__property TsgcAMQPChannelCloseEvent OnAMQPChannelClose = {read=FOnAMQPChannelClose, write=FOnAMQPChannelClose};
	__property TsgcAMQPChannelFlowEvent OnAMQPChannelFlow = {read=FOnAMQPChannelFlow, write=FOnAMQPChannelFlow};
	__property TsgcAMQPDisconnectEvent OnAMQPDisconnect = {read=FOnAMQPDisconnect, write=FOnAMQPDisconnect};
	__property TsgcAMQPHeartBeatEvent OnAMQPHeartBeat = {read=FOnAMQPHeartBeat, write=FOnAMQPHeartBeat};
	__property TsgcAMQPWriteBytesEvent OnAMQPWriteBytes = {read=FOnAMQPWriteBytes, write=FOnAMQPWriteBytes};
	__property TsgcAMQPChannelOpenEvent OnAMQPChannelOpen = {read=FOnAMQPChannelOpen, write=FOnAMQPChannelOpen};
	__property TsgcAMQPExceptionEvent OnAMQPException = {read=FOnAMQPException, write=FOnAMQPException};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcamqp */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCAMQP)
using namespace Sgcamqp;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SgcamqpHPP
