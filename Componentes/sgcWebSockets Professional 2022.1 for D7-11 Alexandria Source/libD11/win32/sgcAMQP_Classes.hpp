// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcAMQP_Classes.pas' rev: 35.00 (Windows)

#ifndef Sgcamqp_classesHPP
#define Sgcamqp_classesHPP

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
#include <sgcWebSocket_Classes_Queues.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>
#include <sgcBase_Helpers.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcamqp_classes
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcAMQPException;
class DELPHICLASS TsgcAMQPBasicProperties;
class DELPHICLASS TsgcAMQPHeader;
class DELPHICLASS TsgcAMQPPayload;
class DELPHICLASS TsgcAMQPFramePayloadType;
class DELPHICLASS TsgcAMQPFramePayload_Base;
class DELPHICLASS TsgcAMQPFramePayload_Method;
class DELPHICLASS TsgcAMQPFramePayload_Method_Connection;
class DELPHICLASS TsgcAMQPFramePayload_Method_ConnectionStart;
class DELPHICLASS TsgcAMQPFramePayload_Method_ConnectionStartOk;
class DELPHICLASS TsgcAMQPFramePayload_Method_ConnectionSecure;
class DELPHICLASS TsgcAMQPFramePayload_Method_ConnectionSecureOk;
class DELPHICLASS TsgcAMQPFramePayload_Method_ConnectionTune;
class DELPHICLASS TsgcAMQPFramePayload_Method_ConnectionTuneOk;
class DELPHICLASS TsgcAMQPFramePayload_Method_ConnectionOpen;
class DELPHICLASS TsgcAMQPFramePayload_Method_ConnectionOpenOk;
class DELPHICLASS TsgcAMQPFramePayload_Method_ConnectionClose;
class DELPHICLASS TsgcAMQPFramePayload_Method_ConnectionCloseOk;
class DELPHICLASS TsgcAMQPFramePayload_Method_Channel;
class DELPHICLASS TsgcAMQPFramePayload_Method_ChannelOpen;
class DELPHICLASS TsgcAMQPFramePayload_Method_ChannelOpenOk;
class DELPHICLASS TsgcAMQPFramePayload_Method_ChannelFlow;
class DELPHICLASS TsgcAMQPFramePayload_Method_ChannelFlowOk;
class DELPHICLASS TsgcAMQPFramePayload_Method_ChannelClose;
class DELPHICLASS TsgcAMQPFramePayload_Method_ChannelCloseOk;
class DELPHICLASS TsgcAMQPFramePayload_Method_Exchange;
class DELPHICLASS TsgcAMQPFramePayload_Method_ExchangeDeclare;
class DELPHICLASS TsgcAMQPFramePayload_Method_ExchangeDeclareOk;
class DELPHICLASS TsgcAMQPFramePayload_Method_ExchangeDelete;
class DELPHICLASS TsgcAMQPFramePayload_Method_ExchangeDeleteOk;
class DELPHICLASS TsgcAMQPFramePayload_Method_Queue;
class DELPHICLASS TsgcAMQPFramePayload_Method_QueueDeclare;
class DELPHICLASS TsgcAMQPFramePayload_Method_QueueDeclareOk;
class DELPHICLASS TsgcAMQPFramePayload_Method_QueueBind;
class DELPHICLASS TsgcAMQPFramePayload_Method_QueueBindOk;
class DELPHICLASS TsgcAMQPFramePayload_Method_QueueUnBind;
class DELPHICLASS TsgcAMQPFramePayload_Method_QueueUnBindOk;
class DELPHICLASS TsgcAMQPFramePayload_Method_QueuePurge;
class DELPHICLASS TsgcAMQPFramePayload_Method_QueuePurgeOk;
class DELPHICLASS TsgcAMQPFramePayload_Method_QueueDelete;
class DELPHICLASS TsgcAMQPFramePayload_Method_QueueDeleteOk;
class DELPHICLASS TsgcAMQPFramePayload_Method_Basic;
class DELPHICLASS TsgcAMQPFramePayload_Method_BasicQoS;
class DELPHICLASS TsgcAMQPFramePayload_Method_BasicQoSOk;
class DELPHICLASS TsgcAMQPFramePayload_Method_BasicConsume;
class DELPHICLASS TsgcAMQPFramePayload_Method_BasicConsumeOk;
class DELPHICLASS TsgcAMQPFramePayload_Method_BasicCancel;
class DELPHICLASS TsgcAMQPFramePayload_Method_BasicCancelOk;
class DELPHICLASS TsgcAMQPFramePayload_Method_BasicPublish;
class DELPHICLASS TsgcAMQPFramePayload_Method_BasicReturn;
class DELPHICLASS TsgcAMQPFramePayload_Method_BasicDeliver;
class DELPHICLASS TsgcAMQPFramePayload_Method_BasicGet;
class DELPHICLASS TsgcAMQPFramePayload_Method_BasicGetOk;
class DELPHICLASS TsgcAMQPFramePayload_Method_BasicGetEmpty;
class DELPHICLASS TsgcAMQPFramePayload_Method_BasicAck;
class DELPHICLASS TsgcAMQPFramePayload_Method_BasicReject;
class DELPHICLASS TsgcAMQPFramePayload_Method_BasicRecoverAsync;
class DELPHICLASS TsgcAMQPFramePayload_Method_BasicRecover;
class DELPHICLASS TsgcAMQPFramePayload_Method_BasicRecoverOk;
class DELPHICLASS TsgcAMQPFramePayload_Method_Tx;
class DELPHICLASS TsgcAMQPFramePayload_Method_TxSelect;
class DELPHICLASS TsgcAMQPFramePayload_Method_TxSelectOk;
class DELPHICLASS TsgcAMQPFramePayload_Method_TxCommit;
class DELPHICLASS TsgcAMQPFramePayload_Method_TxCommitOk;
class DELPHICLASS TsgcAMQPFramePayload_Method_TxRollback;
class DELPHICLASS TsgcAMQPFramePayload_Method_TxRollbackOk;
class DELPHICLASS TsgcAMQPFramePayloadType_Method;
class DELPHICLASS TsgcAMQPFramePayloadType_ContentHeader;
class DELPHICLASS TsgcAMQPFramePayloadType_ContentBody;
class DELPHICLASS TsgcAMQPFramePayloadType_HeartBeat;
class DELPHICLASS TsgcAMQPFrame;
class DELPHICLASS TsgcAMQPMessageContent;
class DELPHICLASS TsgcAMQPChannelThread;
class DELPHICLASS TsgcAMQPChannelThreadWaitRequest;
class DELPHICLASS TsgcAMQPChannelThreadRequest;
class DELPHICLASS TsgcAMQPChannelThreadItem;
class DELPHICLASS TsgcAMQPChannelThreads;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcAMQPThreadExceptionEvent)(System::TObject* Sender, System::Sysutils::Exception* E);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPException : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
private:
	int FCode;
	
public:
	__property int Code = {read=FCode, write=FCode, nodefault};
public:
	/* Exception.Create */ inline __fastcall TsgcAMQPException(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall TsgcAMQPException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall TsgcAMQPException(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall TsgcAMQPException(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall TsgcAMQPException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall TsgcAMQPException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall TsgcAMQPException(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall TsgcAMQPException(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall TsgcAMQPException(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall TsgcAMQPException(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall TsgcAMQPException(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall TsgcAMQPException(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~TsgcAMQPException() { }
	
};

#pragma pack(pop)

enum DECLSPEC_DENUM TsgcAMQPFrameType : unsigned char { amqpFrameNone, amqpFrameMethod, amqpFrameHeader, amqpFrameBody, amqpFrameHeartBeat };

enum DECLSPEC_DENUM TsgcAMQPClass : unsigned char { amqpClassNone, amqpClassConnection, amqpClassChannel, amqpClassExchange, amqpClassQueue, amqpClassBasic, amqpClassTx };

enum DECLSPEC_DENUM TsgcAMQPMethod : unsigned char { amqpMethodNone, amqpConnNone, amqpConnStart, amqpConnStartOk, amqpConnSecure, amqpConnSecureOk, amqpConnTune, amqpConnTuneOk, amqpConnOpen, amqpConnOpenOk, amqpConnClose, amqpConnCloseOk, amqpChannNone, amqpChannOpen, amqpChannOpenOk, amqpChannFlow, amqpChannFlowOk, amqpChannClose, amqpChannCloseOk, amqpExchNone, amqpExchDeclare, amqpExchDeclareOk, amqpExchDelete, amqpExchDeleteOk, amqpQueueNone, amqpQueueDeclare, amqpQueueDeclareOk, amqpQueueBind, amqpQueueBindOk, amqpQueueUnBind, amqpQueueUnBindOk, amqpQueuePurge, amqpQueuePurgeOk, amqpQueueDelete, amqpQueueDeleteOk, amqpBasicNone, amqpBasicQos, amqpBasicQosOk, amqpBasicConsume, amqpBasicConsumeOk, amqpBasicCancel, amqpBasicCancelOk, amqpBasicPublish, 
	amqpBasicReturn, amqpBasicDeliver, amqpBasicGet, amqpBasicGetOk, amqpBasicGetEmpty, amqpBasicAck, amqpBasicReject, amqpBasicRecoverAsync, amqpBasicRecover, amqpBasicRecoverOk, amqpTxNone, amqpTxSelect, amqpTxSelectOk, amqpTxCommit, amqpTxCommitOk, amqpTxRollback, amqpTxRollbackOk };

typedef System::Set<TsgcAMQPMethod, TsgcAMQPMethod::amqpMethodNone, TsgcAMQPMethod::amqpTxRollbackOk> TsgcAMQPMethods;

enum DECLSPEC_DENUM TsgcAMQPTransaction : unsigned char { amqpTransactionSelect, amqpTransactionCommit, amqpTransactionRollback };

enum DECLSPEC_DENUM TsgcAMQPAuthentication : unsigned char { amqpAuthNone, amqpAuthAnnonymous, amqpAuthPlain, amqpAuthAMQPlain, amqpAuthExternal };

typedef System::Set<TsgcAMQPAuthentication, TsgcAMQPAuthentication::amqpAuthNone, TsgcAMQPAuthentication::amqpAuthExternal> TsgcAMQPAuthentications;

enum DECLSPEC_DENUM TsgcAMQPMessageDeliveryMode : unsigned char { amqpMDMNone, amqpMDMNonPersistent, amqpMDMPersistent };

class PASCALIMPLEMENTATION TsgcAMQPBasicProperties : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FOffset;
	System::UnicodeString FAppId;
	System::UnicodeString FContentEncoding;
	System::UnicodeString FContentType;
	System::UnicodeString FCorrelationId;
	TsgcAMQPMessageDeliveryMode FDeliveryMode;
	System::UnicodeString FExpiration;
	System::UnicodeString FHeaders;
	System::UnicodeString FMessageId;
	System::Byte FPriority;
	System::UnicodeString FReplyTo;
	System::UnicodeString FReserved;
	unsigned __int64 FTimeStamp;
	System::UnicodeString FUserId;
	System::UnicodeString F_Type;
	void __fastcall DoInitialize();
	
public:
	__fastcall virtual TsgcAMQPBasicProperties();
	void __fastcall Read(const System::DynamicArray<System::Byte> aBytes, int aOffset = 0x0);
	System::DynamicArray<System::Byte> __fastcall Write();
	__property System::UnicodeString ContentType = {read=FContentType, write=FContentType};
	__property System::UnicodeString ContentEncoding = {read=FContentEncoding, write=FContentEncoding};
	__property System::UnicodeString Headers = {read=FHeaders, write=FHeaders};
	__property TsgcAMQPMessageDeliveryMode DeliveryMode = {read=FDeliveryMode, write=FDeliveryMode, nodefault};
	__property System::Byte Priority = {read=FPriority, write=FPriority, nodefault};
	__property System::UnicodeString CorrelationId = {read=FCorrelationId, write=FCorrelationId};
	__property System::UnicodeString ReplyTo = {read=FReplyTo, write=FReplyTo};
	__property System::UnicodeString Expiration = {read=FExpiration, write=FExpiration};
	__property System::UnicodeString MessageId = {read=FMessageId, write=FMessageId};
	__property unsigned __int64 TimeStamp = {read=FTimeStamp, write=FTimeStamp};
	__property System::UnicodeString _Type = {read=F_Type, write=F_Type};
	__property System::UnicodeString UserId = {read=FUserId, write=FUserId};
	__property System::UnicodeString AppId = {read=FAppId, write=FAppId};
	__property System::UnicodeString Reserved = {read=FReserved, write=FReserved};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPBasicProperties() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPHeader : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TsgcAMQPFrameType F_Type;
	System::Word FChannel;
	int FSize;
	
public:
	void __fastcall Clear();
	void __fastcall Read(const System::DynamicArray<System::Byte> aBytes, int aOffset);
	__property TsgcAMQPFrameType _Type = {read=F_Type, write=F_Type, nodefault};
	__property System::Word Channel = {read=FChannel, write=FChannel, nodefault};
	__property int Size = {read=FSize, write=FSize, nodefault};
public:
	/* TObject.Create */ inline __fastcall TsgcAMQPHeader() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPHeader() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPPayload : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::DynamicArray<System::Byte> FPayload;
	
public:
	void __fastcall Clear();
	void __fastcall Read(const System::DynamicArray<System::Byte> aBytes, int aSize, int aOffset = 0x0);
	__property System::DynamicArray<System::Byte> Payload = {read=FPayload};
public:
	/* TObject.Create */ inline __fastcall TsgcAMQPPayload() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPPayload() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcAMQPFramePayloadType : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayloadType();
	void __fastcall Read(const System::DynamicArray<System::Byte> aBytes);
	System::DynamicArray<System::Byte> __fastcall Write();
	
private:
	System::Classes::TNotifyEvent FOnReadFramePayload;
	
protected:
	__property System::Classes::TNotifyEvent OnReadFramePayload = {read=FOnReadFramePayload, write=FOnReadFramePayload};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayloadType() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Base : public System::TObject
{
	typedef System::TObject inherited;
	
protected:
	TsgcAMQPClass FClassId;
	TsgcAMQPMethod FMethodId;
	int FOffset;
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Base();
	void __fastcall Read(const System::DynamicArray<System::Byte> aBytes);
	System::DynamicArray<System::Byte> __fastcall Write();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Base() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method : public TsgcAMQPFramePayload_Base
{
	typedef TsgcAMQPFramePayload_Base inherited;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
public:
	/* TsgcAMQPFramePayload_Base.Create */ inline __fastcall virtual TsgcAMQPFramePayload_Method() : TsgcAMQPFramePayload_Base() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_Connection : public TsgcAMQPFramePayload_Method
{
	typedef TsgcAMQPFramePayload_Method inherited;
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_Connection();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_Connection() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_ConnectionStart : public TsgcAMQPFramePayload_Method_Connection
{
	typedef TsgcAMQPFramePayload_Method_Connection inherited;
	
private:
	System::UnicodeString FLocales;
	System::UnicodeString FMechanisms;
	System::Classes::TStringList* FServerProperties;
	System::Byte FVersionMajor;
	System::Byte FVersionMinor;
	System::Classes::TStringList* __fastcall GetServerProperties();
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_ConnectionStart();
	__fastcall virtual ~TsgcAMQPFramePayload_Method_ConnectionStart();
	__property System::UnicodeString Locales = {read=FLocales, write=FLocales};
	__property System::UnicodeString Mechanisms = {read=FMechanisms, write=FMechanisms};
	__property System::Classes::TStringList* ServerProperties = {read=GetServerProperties, write=FServerProperties};
	__property System::Byte VersionMajor = {read=FVersionMajor, write=FVersionMajor, nodefault};
	__property System::Byte VersionMinor = {read=FVersionMinor, write=FVersionMinor, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_ConnectionStartOk : public TsgcAMQPFramePayload_Method_Connection
{
	typedef TsgcAMQPFramePayload_Method_Connection inherited;
	
private:
	System::Classes::TStringList* FClientProperties;
	System::UnicodeString FLocale;
	System::UnicodeString FMechanism;
	System::UnicodeString FPassword;
	System::UnicodeString FUser;
	System::Classes::TStringList* __fastcall GetClientProperties();
	TsgcAMQPAuthentication __fastcall GetMechanismType();
	
protected:
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_ConnectionStartOk();
	__fastcall virtual ~TsgcAMQPFramePayload_Method_ConnectionStartOk();
	__property System::Classes::TStringList* ClientProperties = {read=GetClientProperties, write=FClientProperties};
	__property System::UnicodeString Locale = {read=FLocale, write=FLocale};
	__property System::UnicodeString Mechanism = {read=FMechanism, write=FMechanism};
	__property TsgcAMQPAuthentication MechanismType = {read=GetMechanismType, nodefault};
	__property System::UnicodeString Password = {read=FPassword, write=FPassword};
	__property System::UnicodeString User = {read=FUser, write=FUser};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_ConnectionSecure : public TsgcAMQPFramePayload_Method_Connection
{
	typedef TsgcAMQPFramePayload_Method_Connection inherited;
	
private:
	System::UnicodeString FChallenge;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_ConnectionSecure();
	__property System::UnicodeString Challenge = {read=FChallenge, write=FChallenge};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_ConnectionSecure() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_ConnectionSecureOk : public TsgcAMQPFramePayload_Method_Connection
{
	typedef TsgcAMQPFramePayload_Method_Connection inherited;
	
private:
	System::UnicodeString FChallenge;
	
protected:
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_ConnectionSecureOk();
	__property System::UnicodeString Challenge = {read=FChallenge, write=FChallenge};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_ConnectionSecureOk() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_ConnectionTune : public TsgcAMQPFramePayload_Method_Connection
{
	typedef TsgcAMQPFramePayload_Method_Connection inherited;
	
private:
	System::Word FChannelMax;
	unsigned FFrameMax;
	System::Word FHeartBeat;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_ConnectionTune();
	__property System::Word ChannelMax = {read=FChannelMax, write=FChannelMax, nodefault};
	__property unsigned FrameMax = {read=FFrameMax, write=FFrameMax, nodefault};
	__property System::Word HeartBeat = {read=FHeartBeat, write=FHeartBeat, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_ConnectionTune() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_ConnectionTuneOk : public TsgcAMQPFramePayload_Method_Connection
{
	typedef TsgcAMQPFramePayload_Method_Connection inherited;
	
private:
	System::Word FChannelMax;
	unsigned FFrameMax;
	System::Word FHeartBeat;
	
protected:
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_ConnectionTuneOk();
	__property System::Word ChannelMax = {read=FChannelMax, write=FChannelMax, nodefault};
	__property unsigned FrameMax = {read=FFrameMax, write=FFrameMax, nodefault};
	__property System::Word HeartBeat = {read=FHeartBeat, write=FHeartBeat, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_ConnectionTuneOk() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_ConnectionOpen : public TsgcAMQPFramePayload_Method_Connection
{
	typedef TsgcAMQPFramePayload_Method_Connection inherited;
	
private:
	System::UnicodeString FReserved1;
	bool FReserved2;
	System::UnicodeString FVirtualHost;
	
protected:
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_ConnectionOpen();
	__property System::UnicodeString VirtualHost = {read=FVirtualHost, write=FVirtualHost};
	__property System::UnicodeString Reserved1 = {read=FReserved1, write=FReserved1};
	__property bool Reserved2 = {read=FReserved2, write=FReserved2, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_ConnectionOpen() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_ConnectionOpenOk : public TsgcAMQPFramePayload_Method_Connection
{
	typedef TsgcAMQPFramePayload_Method_Connection inherited;
	
private:
	System::UnicodeString FReserved1;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_ConnectionOpenOk();
	__property System::UnicodeString Reserved1 = {read=FReserved1, write=FReserved1};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_ConnectionOpenOk() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_ConnectionClose : public TsgcAMQPFramePayload_Method_Connection
{
	typedef TsgcAMQPFramePayload_Method_Connection inherited;
	
private:
	TsgcAMQPClass FFailClassId;
	TsgcAMQPMethod FFailMethodId;
	System::Word FReplyCode;
	System::UnicodeString FReplyText;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_ConnectionClose();
	__property System::Word ReplyCode = {read=FReplyCode, write=FReplyCode, nodefault};
	__property System::UnicodeString ReplyText = {read=FReplyText, write=FReplyText};
	__property TsgcAMQPClass FailClassId = {read=FFailClassId, write=FFailClassId, nodefault};
	__property TsgcAMQPMethod FailMethodId = {read=FFailMethodId, write=FFailMethodId, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_ConnectionClose() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_ConnectionCloseOk : public TsgcAMQPFramePayload_Method_Connection
{
	typedef TsgcAMQPFramePayload_Method_Connection inherited;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_ConnectionCloseOk();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_ConnectionCloseOk() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_Channel : public TsgcAMQPFramePayload_Method
{
	typedef TsgcAMQPFramePayload_Method inherited;
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_Channel();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_Channel() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_ChannelOpen : public TsgcAMQPFramePayload_Method_Channel
{
	typedef TsgcAMQPFramePayload_Method_Channel inherited;
	
private:
	System::UnicodeString FReserved1;
	
protected:
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_ChannelOpen();
	__property System::UnicodeString Reserved1 = {read=FReserved1, write=FReserved1};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_ChannelOpen() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_ChannelOpenOk : public TsgcAMQPFramePayload_Method_Channel
{
	typedef TsgcAMQPFramePayload_Method_Channel inherited;
	
private:
	System::UnicodeString FReserved1;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_ChannelOpenOk();
	__property System::UnicodeString Reserved1 = {read=FReserved1, write=FReserved1};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_ChannelOpenOk() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_ChannelFlow : public TsgcAMQPFramePayload_Method_Channel
{
	typedef TsgcAMQPFramePayload_Method_Channel inherited;
	
private:
	bool FActive;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_ChannelFlow();
	__property bool Active = {read=FActive, write=FActive, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_ChannelFlow() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_ChannelFlowOk : public TsgcAMQPFramePayload_Method_Channel
{
	typedef TsgcAMQPFramePayload_Method_Channel inherited;
	
private:
	bool FActive;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_ChannelFlowOk();
	__property bool Active = {read=FActive, write=FActive, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_ChannelFlowOk() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_ChannelClose : public TsgcAMQPFramePayload_Method_Channel
{
	typedef TsgcAMQPFramePayload_Method_Channel inherited;
	
private:
	TsgcAMQPClass FFailClassId;
	TsgcAMQPMethod FFailMethodId;
	System::Word FReplyCode;
	System::UnicodeString FReplyText;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_ChannelClose();
	__property System::Word ReplyCode = {read=FReplyCode, write=FReplyCode, nodefault};
	__property System::UnicodeString ReplyText = {read=FReplyText, write=FReplyText};
	__property TsgcAMQPClass FailClassId = {read=FFailClassId, write=FFailClassId, nodefault};
	__property TsgcAMQPMethod FailMethodId = {read=FFailMethodId, write=FFailMethodId, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_ChannelClose() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_ChannelCloseOk : public TsgcAMQPFramePayload_Method_Channel
{
	typedef TsgcAMQPFramePayload_Method_Channel inherited;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_ChannelCloseOk();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_ChannelCloseOk() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_Exchange : public TsgcAMQPFramePayload_Method
{
	typedef TsgcAMQPFramePayload_Method inherited;
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_Exchange();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_Exchange() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_ExchangeDeclare : public TsgcAMQPFramePayload_Method_Exchange
{
	typedef TsgcAMQPFramePayload_Method_Exchange inherited;
	
private:
	System::UnicodeString FArguments;
	bool FDurable;
	System::UnicodeString FExchange;
	bool FNoWait;
	bool FPassive;
	System::Word FReserved1;
	bool FAutoDelete;
	bool FInternal;
	System::UnicodeString FExchangeType;
	
protected:
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_ExchangeDeclare();
	__property System::Word Reserved1 = {read=FReserved1, write=FReserved1, nodefault};
	__property System::UnicodeString Exchange = {read=FExchange, write=FExchange};
	__property System::UnicodeString ExchangeType = {read=FExchangeType, write=FExchangeType};
	__property bool Passive = {read=FPassive, write=FPassive, nodefault};
	__property bool Durable = {read=FDurable, write=FDurable, nodefault};
	__property bool AutoDelete = {read=FAutoDelete, write=FAutoDelete, nodefault};
	__property bool Internal = {read=FInternal, write=FInternal, nodefault};
	__property bool NoWait = {read=FNoWait, write=FNoWait, nodefault};
	__property System::UnicodeString Arguments = {read=FArguments, write=FArguments};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_ExchangeDeclare() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_ExchangeDeclareOk : public TsgcAMQPFramePayload_Method_Exchange
{
	typedef TsgcAMQPFramePayload_Method_Exchange inherited;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_ExchangeDeclareOk();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_ExchangeDeclareOk() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_ExchangeDelete : public TsgcAMQPFramePayload_Method_Exchange
{
	typedef TsgcAMQPFramePayload_Method_Exchange inherited;
	
private:
	System::UnicodeString FExchange;
	bool FIfUnused;
	bool FNoWait;
	System::Word FReserved1;
	
protected:
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_ExchangeDelete();
	__property System::Word Reserved1 = {read=FReserved1, write=FReserved1, nodefault};
	__property System::UnicodeString Exchange = {read=FExchange, write=FExchange};
	__property bool IfUnused = {read=FIfUnused, write=FIfUnused, nodefault};
	__property bool NoWait = {read=FNoWait, write=FNoWait, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_ExchangeDelete() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_ExchangeDeleteOk : public TsgcAMQPFramePayload_Method_Exchange
{
	typedef TsgcAMQPFramePayload_Method_Exchange inherited;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_ExchangeDeleteOk();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_ExchangeDeleteOk() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_Queue : public TsgcAMQPFramePayload_Method
{
	typedef TsgcAMQPFramePayload_Method inherited;
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_Queue();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_Queue() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_QueueDeclare : public TsgcAMQPFramePayload_Method_Queue
{
	typedef TsgcAMQPFramePayload_Method_Queue inherited;
	
private:
	System::UnicodeString FArguments;
	bool FDurable;
	System::UnicodeString FQueue;
	bool FNoWait;
	bool FPassive;
	System::Word FReserved1;
	bool FAutoDelete;
	bool FExclusive;
	
protected:
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_QueueDeclare();
	__property System::Word Reserved1 = {read=FReserved1, write=FReserved1, nodefault};
	__property System::UnicodeString Queue = {read=FQueue, write=FQueue};
	__property bool Passive = {read=FPassive, write=FPassive, nodefault};
	__property bool Durable = {read=FDurable, write=FDurable, nodefault};
	__property bool Exclusive = {read=FExclusive, write=FExclusive, nodefault};
	__property bool AutoDelete = {read=FAutoDelete, write=FAutoDelete, nodefault};
	__property bool NoWait = {read=FNoWait, write=FNoWait, nodefault};
	__property System::UnicodeString Arguments = {read=FArguments, write=FArguments};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_QueueDeclare() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_QueueDeclareOk : public TsgcAMQPFramePayload_Method_Queue
{
	typedef TsgcAMQPFramePayload_Method_Queue inherited;
	
private:
	int FConsumerCount;
	int FMessageCount;
	System::UnicodeString FQueue;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_QueueDeclareOk();
	__property System::UnicodeString Queue = {read=FQueue, write=FQueue};
	__property int MessageCount = {read=FMessageCount, write=FMessageCount, nodefault};
	__property int ConsumerCount = {read=FConsumerCount, write=FConsumerCount, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_QueueDeclareOk() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_QueueBind : public TsgcAMQPFramePayload_Method_Queue
{
	typedef TsgcAMQPFramePayload_Method_Queue inherited;
	
private:
	System::UnicodeString FArguments;
	System::UnicodeString FQueue;
	bool FNoWait;
	System::UnicodeString FRoutingKey;
	System::Word FReserved1;
	System::UnicodeString FExchange;
	
protected:
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_QueueBind();
	__property System::Word Reserved1 = {read=FReserved1, write=FReserved1, nodefault};
	__property System::UnicodeString Queue = {read=FQueue, write=FQueue};
	__property System::UnicodeString Exchange = {read=FExchange, write=FExchange};
	__property System::UnicodeString RoutingKey = {read=FRoutingKey, write=FRoutingKey};
	__property bool NoWait = {read=FNoWait, write=FNoWait, nodefault};
	__property System::UnicodeString Arguments = {read=FArguments, write=FArguments};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_QueueBind() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_QueueBindOk : public TsgcAMQPFramePayload_Method_Queue
{
	typedef TsgcAMQPFramePayload_Method_Queue inherited;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_QueueBindOk();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_QueueBindOk() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_QueueUnBind : public TsgcAMQPFramePayload_Method_Queue
{
	typedef TsgcAMQPFramePayload_Method_Queue inherited;
	
private:
	System::UnicodeString FArguments;
	System::UnicodeString FQueue;
	System::UnicodeString FRoutingKey;
	System::Word FReserved1;
	System::UnicodeString FExchange;
	
protected:
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_QueueUnBind();
	__property System::Word Reserved1 = {read=FReserved1, write=FReserved1, nodefault};
	__property System::UnicodeString Queue = {read=FQueue, write=FQueue};
	__property System::UnicodeString Exchange = {read=FExchange, write=FExchange};
	__property System::UnicodeString RoutingKey = {read=FRoutingKey, write=FRoutingKey};
	__property System::UnicodeString Arguments = {read=FArguments, write=FArguments};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_QueueUnBind() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_QueueUnBindOk : public TsgcAMQPFramePayload_Method_Queue
{
	typedef TsgcAMQPFramePayload_Method_Queue inherited;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_QueueUnBindOk();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_QueueUnBindOk() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_QueuePurge : public TsgcAMQPFramePayload_Method_Queue
{
	typedef TsgcAMQPFramePayload_Method_Queue inherited;
	
private:
	System::UnicodeString FQueue;
	bool FNoWait;
	System::Word FReserved1;
	
protected:
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_QueuePurge();
	__property System::Word Reserved1 = {read=FReserved1, write=FReserved1, nodefault};
	__property System::UnicodeString Queue = {read=FQueue, write=FQueue};
	__property bool NoWait = {read=FNoWait, write=FNoWait, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_QueuePurge() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_QueuePurgeOk : public TsgcAMQPFramePayload_Method_Queue
{
	typedef TsgcAMQPFramePayload_Method_Queue inherited;
	
private:
	int FMessageCount;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_QueuePurgeOk();
	__property int MessageCount = {read=FMessageCount, write=FMessageCount, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_QueuePurgeOk() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_QueueDelete : public TsgcAMQPFramePayload_Method_Queue
{
	typedef TsgcAMQPFramePayload_Method_Queue inherited;
	
private:
	System::UnicodeString FQueue;
	bool FNoWait;
	bool FIfEmpty;
	System::Word FReserved1;
	bool FIfUnused;
	
protected:
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_QueueDelete();
	__property System::Word Reserved1 = {read=FReserved1, write=FReserved1, nodefault};
	__property System::UnicodeString Queue = {read=FQueue, write=FQueue};
	__property bool IfUnused = {read=FIfUnused, write=FIfUnused, nodefault};
	__property bool IfEmpty = {read=FIfEmpty, write=FIfEmpty, nodefault};
	__property bool NoWait = {read=FNoWait, write=FNoWait, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_QueueDelete() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_QueueDeleteOk : public TsgcAMQPFramePayload_Method_Queue
{
	typedef TsgcAMQPFramePayload_Method_Queue inherited;
	
private:
	int FMessageCount;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_QueueDeleteOk();
	__property int MessageCount = {read=FMessageCount, write=FMessageCount, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_QueueDeleteOk() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_Basic : public TsgcAMQPFramePayload_Method
{
	typedef TsgcAMQPFramePayload_Method inherited;
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_Basic();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_Basic() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_BasicQoS : public TsgcAMQPFramePayload_Method_Basic
{
	typedef TsgcAMQPFramePayload_Method_Basic inherited;
	
private:
	bool FGlobal;
	System::Word FPrefetchCount;
	unsigned FPrefetchSize;
	
protected:
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_BasicQoS();
	__property unsigned PrefetchSize = {read=FPrefetchSize, write=FPrefetchSize, nodefault};
	__property System::Word PrefetchCount = {read=FPrefetchCount, write=FPrefetchCount, nodefault};
	__property bool Global = {read=FGlobal, write=FGlobal, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_BasicQoS() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_BasicQoSOk : public TsgcAMQPFramePayload_Method_Basic
{
	typedef TsgcAMQPFramePayload_Method_Basic inherited;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_BasicQoSOk();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_BasicQoSOk() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_BasicConsume : public TsgcAMQPFramePayload_Method_Basic
{
	typedef TsgcAMQPFramePayload_Method_Basic inherited;
	
private:
	System::UnicodeString FArguments;
	System::UnicodeString FConsumerTag;
	bool FExclusive;
	bool FNoAck;
	bool FNoLocal;
	bool FNoWait;
	System::UnicodeString FQueue;
	System::Word FReserved1;
	
protected:
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_BasicConsume();
	__property System::Word Reserved1 = {read=FReserved1, write=FReserved1, nodefault};
	__property System::UnicodeString Queue = {read=FQueue, write=FQueue};
	__property System::UnicodeString ConsumerTag = {read=FConsumerTag, write=FConsumerTag};
	__property bool NoLocal = {read=FNoLocal, write=FNoLocal, nodefault};
	__property bool NoAck = {read=FNoAck, write=FNoAck, nodefault};
	__property bool Exclusive = {read=FExclusive, write=FExclusive, nodefault};
	__property bool NoWait = {read=FNoWait, write=FNoWait, nodefault};
	__property System::UnicodeString Arguments = {read=FArguments, write=FArguments};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_BasicConsume() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_BasicConsumeOk : public TsgcAMQPFramePayload_Method_Basic
{
	typedef TsgcAMQPFramePayload_Method_Basic inherited;
	
private:
	System::UnicodeString FConsumerTag;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_BasicConsumeOk();
	__property System::UnicodeString ConsumerTag = {read=FConsumerTag, write=FConsumerTag};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_BasicConsumeOk() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_BasicCancel : public TsgcAMQPFramePayload_Method_Basic
{
	typedef TsgcAMQPFramePayload_Method_Basic inherited;
	
private:
	System::UnicodeString FConsumerTag;
	bool FNoWait;
	
protected:
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_BasicCancel();
	__property System::UnicodeString ConsumerTag = {read=FConsumerTag, write=FConsumerTag};
	__property bool NoWait = {read=FNoWait, write=FNoWait, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_BasicCancel() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_BasicCancelOk : public TsgcAMQPFramePayload_Method_Basic
{
	typedef TsgcAMQPFramePayload_Method_Basic inherited;
	
private:
	System::UnicodeString FConsumerTag;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_BasicCancelOk();
	__property System::UnicodeString ConsumerTag = {read=FConsumerTag, write=FConsumerTag};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_BasicCancelOk() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_BasicPublish : public TsgcAMQPFramePayload_Method_Basic
{
	typedef TsgcAMQPFramePayload_Method_Basic inherited;
	
private:
	System::UnicodeString FExchange;
	bool FImmediate;
	bool FMandatory;
	System::Word FReserved1;
	System::UnicodeString FRoutingKey;
	
protected:
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_BasicPublish();
	__property System::Word Reserved1 = {read=FReserved1, write=FReserved1, nodefault};
	__property System::UnicodeString Exchange = {read=FExchange, write=FExchange};
	__property System::UnicodeString RoutingKey = {read=FRoutingKey, write=FRoutingKey};
	__property bool Mandatory = {read=FMandatory, write=FMandatory, nodefault};
	__property bool Immediate = {read=FImmediate, write=FImmediate, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_BasicPublish() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_BasicReturn : public TsgcAMQPFramePayload_Method_Basic
{
	typedef TsgcAMQPFramePayload_Method_Basic inherited;
	
private:
	System::UnicodeString FExchange;
	System::Word FReplyCode;
	System::UnicodeString FReplyText;
	System::UnicodeString FRoutingKey;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_BasicReturn();
	__property System::Word ReplyCode = {read=FReplyCode, write=FReplyCode, nodefault};
	__property System::UnicodeString ReplyText = {read=FReplyText, write=FReplyText};
	__property System::UnicodeString Exchange = {read=FExchange, write=FExchange};
	__property System::UnicodeString RoutingKey = {read=FRoutingKey, write=FRoutingKey};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_BasicReturn() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_BasicDeliver : public TsgcAMQPFramePayload_Method_Basic
{
	typedef TsgcAMQPFramePayload_Method_Basic inherited;
	
private:
	bool FRedelivered;
	System::UnicodeString FConsumerTag;
	unsigned __int64 FDeliveryTag;
	System::UnicodeString FExchange;
	System::UnicodeString FRoutingKey;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_BasicDeliver();
	__property System::UnicodeString ConsumerTag = {read=FConsumerTag, write=FConsumerTag};
	__property unsigned __int64 DeliveryTag = {read=FDeliveryTag, write=FDeliveryTag};
	__property bool Redelivered = {read=FRedelivered, write=FRedelivered, nodefault};
	__property System::UnicodeString Exchange = {read=FExchange, write=FExchange};
	__property System::UnicodeString RoutingKey = {read=FRoutingKey, write=FRoutingKey};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_BasicDeliver() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_BasicGet : public TsgcAMQPFramePayload_Method_Basic
{
	typedef TsgcAMQPFramePayload_Method_Basic inherited;
	
private:
	bool FNoAck;
	System::Word FReserved1;
	System::UnicodeString FQueue;
	
protected:
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_BasicGet();
	__property System::Word Reserved1 = {read=FReserved1, write=FReserved1, nodefault};
	__property System::UnicodeString Queue = {read=FQueue, write=FQueue};
	__property bool NoAck = {read=FNoAck, write=FNoAck, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_BasicGet() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_BasicGetOk : public TsgcAMQPFramePayload_Method_Basic
{
	typedef TsgcAMQPFramePayload_Method_Basic inherited;
	
private:
	bool FRedelivered;
	int FMessageCount;
	unsigned __int64 FDeliveryTag;
	System::UnicodeString FExchange;
	System::UnicodeString FRoutingKey;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_BasicGetOk();
	__property unsigned __int64 DeliveryTag = {read=FDeliveryTag, write=FDeliveryTag};
	__property bool Redelivered = {read=FRedelivered, write=FRedelivered, nodefault};
	__property System::UnicodeString Exchange = {read=FExchange, write=FExchange};
	__property System::UnicodeString RoutingKey = {read=FRoutingKey, write=FRoutingKey};
	__property int MessageCount = {read=FMessageCount, write=FMessageCount, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_BasicGetOk() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_BasicGetEmpty : public TsgcAMQPFramePayload_Method_Basic
{
	typedef TsgcAMQPFramePayload_Method_Basic inherited;
	
private:
	System::Word FReserved1;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_BasicGetEmpty();
	__property System::Word Reserved1 = {read=FReserved1, write=FReserved1, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_BasicGetEmpty() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_BasicAck : public TsgcAMQPFramePayload_Method_Basic
{
	typedef TsgcAMQPFramePayload_Method_Basic inherited;
	
private:
	unsigned __int64 FDeliveryTag;
	bool FMultiple;
	
protected:
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_BasicAck();
	__property unsigned __int64 DeliveryTag = {read=FDeliveryTag, write=FDeliveryTag};
	__property bool Multiple = {read=FMultiple, write=FMultiple, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_BasicAck() { }
	
};


class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_BasicReject : public TsgcAMQPFramePayload_Method_Basic
{
	typedef TsgcAMQPFramePayload_Method_Basic inherited;
	
private:
	unsigned __int64 FDeliveryTag;
	bool FRequeue;
	
protected:
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_BasicReject();
	__property unsigned __int64 DeliveryTag = {read=FDeliveryTag, write=FDeliveryTag};
	__property bool Requeue = {read=FRequeue, write=FRequeue, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_BasicReject() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_BasicRecoverAsync : public TsgcAMQPFramePayload_Method_Basic
{
	typedef TsgcAMQPFramePayload_Method_Basic inherited;
	
private:
	bool FRequeue;
	
protected:
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_BasicRecoverAsync();
	__property bool Requeue = {read=FRequeue, write=FRequeue, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_BasicRecoverAsync() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_BasicRecover : public TsgcAMQPFramePayload_Method_Basic
{
	typedef TsgcAMQPFramePayload_Method_Basic inherited;
	
private:
	bool FRequeue;
	
protected:
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_BasicRecover();
	__property bool Requeue = {read=FRequeue, write=FRequeue, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_BasicRecover() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_BasicRecoverOk : public TsgcAMQPFramePayload_Method_Basic
{
	typedef TsgcAMQPFramePayload_Method_Basic inherited;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_BasicRecoverOk();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_BasicRecoverOk() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_Tx : public TsgcAMQPFramePayload_Method
{
	typedef TsgcAMQPFramePayload_Method inherited;
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_Tx();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_Tx() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_TxSelect : public TsgcAMQPFramePayload_Method_Tx
{
	typedef TsgcAMQPFramePayload_Method_Tx inherited;
	
protected:
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_TxSelect();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_TxSelect() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_TxSelectOk : public TsgcAMQPFramePayload_Method_Tx
{
	typedef TsgcAMQPFramePayload_Method_Tx inherited;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_TxSelectOk();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_TxSelectOk() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_TxCommit : public TsgcAMQPFramePayload_Method_Tx
{
	typedef TsgcAMQPFramePayload_Method_Tx inherited;
	
protected:
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_TxCommit();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_TxCommit() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_TxCommitOk : public TsgcAMQPFramePayload_Method_Tx
{
	typedef TsgcAMQPFramePayload_Method_Tx inherited;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_TxCommitOk();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_TxCommitOk() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_TxRollback : public TsgcAMQPFramePayload_Method_Tx
{
	typedef TsgcAMQPFramePayload_Method_Tx inherited;
	
protected:
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_TxRollback();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_TxRollback() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPFramePayload_Method_TxRollbackOk : public TsgcAMQPFramePayload_Method_Tx
{
	typedef TsgcAMQPFramePayload_Method_Tx inherited;
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	
public:
	__fastcall virtual TsgcAMQPFramePayload_Method_TxRollbackOk();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayload_Method_TxRollbackOk() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcAMQPFramePayloadType_Method : public TsgcAMQPFramePayloadType
{
	typedef TsgcAMQPFramePayloadType inherited;
	
private:
	TsgcAMQPClass FClassId;
	TsgcAMQPMethod FMethodId;
	TsgcAMQPFramePayload_Method* FPayload;
	void __fastcall SetPayload(TsgcAMQPFramePayload_Method* const Value);
	
public:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	__fastcall virtual ~TsgcAMQPFramePayloadType_Method();
	__property TsgcAMQPClass ClassId = {read=FClassId, nodefault};
	__property TsgcAMQPMethod MethodId = {read=FMethodId, nodefault};
	__property TsgcAMQPFramePayload_Method* Payload = {read=FPayload, write=SetPayload};
public:
	/* TsgcAMQPFramePayloadType.Create */ inline __fastcall virtual TsgcAMQPFramePayloadType_Method() : TsgcAMQPFramePayloadType() { }
	
};


class PASCALIMPLEMENTATION TsgcAMQPFramePayloadType_ContentHeader : public TsgcAMQPFramePayloadType
{
	typedef TsgcAMQPFramePayloadType inherited;
	
private:
	int FOffset;
	unsigned __int64 FBodySize;
	TsgcAMQPClass FClassId;
	System::Word FWeight;
	TsgcAMQPBasicProperties* FHeader;
	TsgcAMQPBasicProperties* __fastcall GetHeader();
	void __fastcall SetHeader(TsgcAMQPBasicProperties* const Value);
	
public:
	__fastcall virtual ~TsgcAMQPFramePayloadType_ContentHeader();
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	__property TsgcAMQPClass ClassId = {read=FClassId, write=FClassId, nodefault};
	__property System::Word Weight = {read=FWeight, write=FWeight, nodefault};
	__property unsigned __int64 BodySize = {read=FBodySize, write=FBodySize};
	__property TsgcAMQPBasicProperties* Header = {read=GetHeader, write=SetHeader};
public:
	/* TsgcAMQPFramePayloadType.Create */ inline __fastcall virtual TsgcAMQPFramePayloadType_ContentHeader() : TsgcAMQPFramePayloadType() { }
	
};


class PASCALIMPLEMENTATION TsgcAMQPFramePayloadType_ContentBody : public TsgcAMQPFramePayloadType
{
	typedef TsgcAMQPFramePayloadType inherited;
	
private:
	System::Classes::TStream* FData;
	bool FFreeData;
	__int64 FDataSize;
	System::UnicodeString __fastcall GetAsString();
	System::Classes::TStream* __fastcall GetData();
	
protected:
	virtual void __fastcall DoRead(const System::DynamicArray<System::Byte> aBytes);
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	__fastcall virtual ~TsgcAMQPFramePayloadType_ContentBody();
	__property System::UnicodeString AsString = {read=GetAsString};
	__property System::Classes::TStream* Data = {read=GetData, write=FData};
	__property __int64 DataSize = {read=FDataSize, write=FDataSize};
public:
	/* TsgcAMQPFramePayloadType.Create */ inline __fastcall virtual TsgcAMQPFramePayloadType_ContentBody() : TsgcAMQPFramePayloadType() { }
	
};


class PASCALIMPLEMENTATION TsgcAMQPFramePayloadType_HeartBeat : public TsgcAMQPFramePayloadType
{
	typedef TsgcAMQPFramePayloadType inherited;
	
public:
	/* TsgcAMQPFramePayloadType.Create */ inline __fastcall virtual TsgcAMQPFramePayloadType_HeartBeat() : TsgcAMQPFramePayloadType() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPFramePayloadType_HeartBeat() { }
	
};


class PASCALIMPLEMENTATION TsgcAMQPFrame : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TsgcAMQPFramePayloadType_Method* FPayload_Method;
	TsgcAMQPFramePayloadType_ContentHeader* FPayload_ContentHeader;
	TsgcAMQPFramePayloadType_ContentBody* FPayload_ContentBody;
	TsgcAMQPFramePayloadType_HeartBeat* FPayload_HeartBeat;
	TsgcAMQPFramePayloadType_Method* __fastcall GetPayload_Method();
	TsgcAMQPFramePayloadType_ContentHeader* __fastcall GetPayload_ContentHeader();
	TsgcAMQPFramePayloadType_ContentBody* __fastcall GetPayload_ContentBody();
	TsgcAMQPFramePayloadType_HeartBeat* __fastcall GetPayload_HeartBeat();
	
public:
	__property TsgcAMQPFramePayloadType_ContentBody* Payload_ContentBody = {read=GetPayload_ContentBody, write=FPayload_ContentBody};
	__property TsgcAMQPFramePayloadType_ContentHeader* Payload_ContentHeader = {read=GetPayload_ContentHeader, write=FPayload_ContentHeader};
	__property TsgcAMQPFramePayloadType_HeartBeat* Payload_HeartBeat = {read=GetPayload_HeartBeat, write=FPayload_HeartBeat};
	__property TsgcAMQPFramePayloadType_Method* Payload_Method = {read=GetPayload_Method, write=FPayload_Method};
	
protected:
	virtual void __fastcall OnReadFramePayloadEvent(System::TObject* Sender);
	virtual void __fastcall DoReadPayload();
	System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	System::DynamicArray<System::Byte> __fastcall Write();
	
private:
	TsgcAMQPHeader* FHeader;
	TsgcAMQPPayload* FPayload;
	TsgcAMQPHeader* __fastcall GetHeader();
	TsgcAMQPPayload* __fastcall GetPayload();
	
protected:
	virtual void __fastcall DoValidateFrame(const System::DynamicArray<System::Byte> aBytes, int aOffset);
	
public:
	void __fastcall Clear();
	int __fastcall Read(const System::DynamicArray<System::Byte> aBytes, int aOffset = 0x0);
	__fastcall virtual TsgcAMQPFrame();
	__fastcall virtual ~TsgcAMQPFrame();
	__property TsgcAMQPHeader* Header = {read=GetHeader, write=FHeader};
	__property TsgcAMQPPayload* Payload = {read=GetPayload, write=FPayload};
	
private:
	int FMaxSize;
	
public:
	__property int MaxSize = {read=FMaxSize, write=FMaxSize, nodefault};
	
private:
	System::Classes::TNotifyEvent FOnReadFrame;
	
public:
	__property System::Classes::TNotifyEvent OnReadFrame = {read=FOnReadFrame, write=FOnReadFrame};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPMessageContent : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TsgcAMQPFramePayloadType_ContentHeader* FHeader;
	TsgcAMQPFramePayloadType_ContentBody* FBody;
	TsgcAMQPFramePayload_Method_Basic* FPayload;
	TsgcAMQPFramePayloadType_ContentBody* __fastcall GetBody();
	TsgcAMQPFramePayloadType_ContentHeader* __fastcall GetHeader();
	void __fastcall SetBody(TsgcAMQPFramePayloadType_ContentBody* const Value);
	void __fastcall SetHeader(TsgcAMQPFramePayloadType_ContentHeader* const Value);
	void __fastcall SetPayload(TsgcAMQPFramePayload_Method_Basic* const Value);
	
protected:
	virtual void __fastcall DoClear();
	
public:
	__fastcall virtual ~TsgcAMQPMessageContent();
	void __fastcall Clear();
	__property TsgcAMQPFramePayloadType_ContentHeader* Header = {read=GetHeader, write=SetHeader};
	__property TsgcAMQPFramePayloadType_ContentBody* Body = {read=GetBody, write=SetBody};
	__property TsgcAMQPFramePayload_Method_Basic* Payload = {read=FPayload, write=SetPayload};
public:
	/* TObject.Create */ inline __fastcall TsgcAMQPMessageContent() : System::TObject() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcAMQPChannelThread : public System::Classes::TThread
{
	typedef System::Classes::TThread inherited;
	
public:
	__fastcall virtual ~TsgcAMQPChannelThread();
	virtual void __fastcall Execute();
	
private:
	Sgcwebsocket_classes_queues::TsgcQueue* FFrames;
	Sgcwebsocket_classes_queues::TsgcQueue* __fastcall GetFrames();
	
protected:
	__property Sgcwebsocket_classes_queues::TsgcQueue* Frames = {read=GetFrames, write=FFrames};
	
public:
	void __fastcall AddFrame(TsgcAMQPFrame* const aFrame);
	
private:
	TsgcAMQPThreadExceptionEvent FOnException;
	
public:
	__property TsgcAMQPThreadExceptionEvent OnException = {read=FOnException, write=FOnException};
	
private:
	System::Classes::TNotifyEvent FOnAfterReadFrame;
	
public:
	__property System::Classes::TNotifyEvent OnAfterReadFrame = {read=FOnAfterReadFrame, write=FOnAfterReadFrame};
public:
	/* TThread.Create */ inline __fastcall TsgcAMQPChannelThread()/* overload */ : System::Classes::TThread() { }
	/* TThread.Create */ inline __fastcall TsgcAMQPChannelThread(bool CreateSuspended)/* overload */ : System::Classes::TThread(CreateSuspended) { }
	/* TThread.Create */ inline __fastcall TsgcAMQPChannelThread(bool CreateSuspended, NativeUInt ReservedStackSize)/* overload */ : System::Classes::TThread(CreateSuspended, ReservedStackSize) { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPChannelThreadWaitRequest : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TsgcAMQPMethod FMethod;
	TsgcAMQPMethods FMethods;
	bool FTerminated;
	
public:
	__fastcall virtual TsgcAMQPChannelThreadWaitRequest();
	__fastcall virtual ~TsgcAMQPChannelThreadWaitRequest();
	void __fastcall Initialize();
	__property TsgcAMQPMethod Method = {read=FMethod, write=FMethod, nodefault};
	__property TsgcAMQPMethods Methods = {read=FMethods, write=FMethods};
	__property bool Terminated = {read=FTerminated, write=FTerminated, nodefault};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcAMQPChannelThreadRequest : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FExchangeDeclare;
	System::UnicodeString FExchangeDelete;
	System::UnicodeString FQueueBindExchange;
	System::UnicodeString FQueueBindQueue;
	System::UnicodeString FQueueDelete;
	System::UnicodeString FQueuePurge;
	System::UnicodeString FQueueUnBindExchange;
	System::UnicodeString FQueueUnBindQueue;
	
public:
	__property System::UnicodeString ExchangeDeclare = {read=FExchangeDeclare, write=FExchangeDeclare};
	__property System::UnicodeString ExchangeDelete = {read=FExchangeDelete, write=FExchangeDelete};
	__property System::UnicodeString QueueBindExchange = {read=FQueueBindExchange, write=FQueueBindExchange};
	__property System::UnicodeString QueueBindQueue = {read=FQueueBindQueue, write=FQueueBindQueue};
	__property System::UnicodeString QueueDelete = {read=FQueueDelete, write=FQueueDelete};
	__property System::UnicodeString QueuePurge = {read=FQueuePurge, write=FQueuePurge};
	__property System::UnicodeString QueueUnBindExchange = {read=FQueueUnBindExchange, write=FQueueUnBindExchange};
	__property System::UnicodeString QueueUnBindQueue = {read=FQueueUnBindQueue, write=FQueueUnBindQueue};
public:
	/* TObject.Create */ inline __fastcall TsgcAMQPChannelThreadRequest() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcAMQPChannelThreadRequest() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcAMQPChannelThreadItem : public Sgcwebsocket_classes_queues::TsgcQueueItemBase
{
	typedef Sgcwebsocket_classes_queues::TsgcQueueItemBase inherited;
	
private:
	System::UnicodeString FChannel;
	TsgcAMQPChannelThread* FThread;
	TsgcAMQPFramePayload_Method_ChannelClose* FChannelClose;
	TsgcAMQPFramePayload_Method_BasicQoS* FQoS;
	bool FFlow;
	TsgcAMQPMethod FMethodId;
	TsgcAMQPChannelThreadWaitRequest* FWaitRequest;
	TsgcAMQPChannelThreadRequest* FRequest;
	TsgcAMQPFramePayload_Method_ChannelClose* __fastcall GetChannelClose();
	TsgcAMQPFramePayload_Method_BasicQoS* __fastcall GetQoS();
	void __fastcall SetChannelClose(TsgcAMQPFramePayload_Method_ChannelClose* const Value);
	void __fastcall SetQoS(TsgcAMQPFramePayload_Method_BasicQoS* const Value);
	TsgcAMQPChannelThreadWaitRequest* __fastcall GetWaitRequest();
	TsgcAMQPChannelThreadRequest* __fastcall GetRequest();
	
protected:
	virtual void __fastcall OnThreadExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	virtual void __fastcall OnThreadAfterReadFrameEvent(System::TObject* Sender);
	
public:
	__property System::UnicodeString Channel = {read=FChannel, write=FChannel};
	__property TsgcAMQPFramePayload_Method_ChannelClose* ChannelClose = {read=GetChannelClose, write=SetChannelClose};
	__property bool Flow = {read=FFlow, write=FFlow, nodefault};
	__property TsgcAMQPMethod MethodId = {read=FMethodId, write=FMethodId, nodefault};
	__property TsgcAMQPFramePayload_Method_BasicQoS* QoS = {read=GetQoS, write=SetQoS};
	__property TsgcAMQPChannelThreadWaitRequest* WaitRequest = {read=GetWaitRequest, write=FWaitRequest};
	__property TsgcAMQPChannelThreadRequest* Request = {read=GetRequest, write=FRequest};
	
private:
	TsgcAMQPMessageContent* FMessageContent;
	TsgcAMQPMessageContent* __fastcall GetMessageContent();
	
public:
	__property TsgcAMQPMessageContent* MessageContent = {read=GetMessageContent, write=FMessageContent};
	__fastcall virtual TsgcAMQPChannelThreadItem();
	__fastcall virtual ~TsgcAMQPChannelThreadItem();
	void __fastcall AddFrame(TsgcAMQPFrame* const aFrame);
	void __fastcall Terminate();
	
private:
	TsgcAMQPThreadExceptionEvent FOnException;
	System::Classes::TNotifyEvent FOnTerminate;
	
public:
	__property TsgcAMQPThreadExceptionEvent OnException = {read=FOnException, write=FOnException};
	__property System::Classes::TNotifyEvent OnTerminate = {read=FOnTerminate, write=FOnTerminate};
};


class PASCALIMPLEMENTATION TsgcAMQPChannelThreads : public Sgcwebsocket_classes_queues::TsgcQueue
{
	typedef Sgcwebsocket_classes_queues::TsgcQueue inherited;
	
private:
	Sgcwebsocket_classes_syncobjs::TsgcThreadSafeInteger* FId;
	System::Word __fastcall GetNewChannelId();
	
protected:
	virtual void __fastcall OnThreadTerminateEvent(System::TObject* Sender);
	virtual void __fastcall OnThreadExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	
public:
	__fastcall virtual TsgcAMQPChannelThreads();
	__fastcall virtual ~TsgcAMQPChannelThreads();
	void __fastcall AddFrame(TsgcAMQPFrame* const aFrame);
	System::Word __fastcall AddChannel(const System::UnicodeString aChannel);
	TsgcAMQPChannelThreadItem* __fastcall GetChannel(const System::UnicodeString aChannel)/* overload */;
	TsgcAMQPChannelThreadItem* __fastcall GetChannel(const System::Word aId)/* overload */;
	
private:
	TsgcAMQPThreadExceptionEvent FOnException;
	
public:
	__property TsgcAMQPThreadExceptionEvent OnException = {read=FOnException, write=FOnException};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcamqp_classes */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCAMQP_CLASSES)
using namespace Sgcamqp_classes;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcamqp_classesHPP
