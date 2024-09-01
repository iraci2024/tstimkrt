// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcP2P_STUN_Classes.pas' rev: 35.00 (Windows)

#ifndef Sgcp2p_stun_classesHPP
#define Sgcp2p_stun_classesHPP

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
#include <sgcBase_Classes.hpp>
#include <sgcBase_Helpers.hpp>
#include <sgcUDP_Classes.hpp>
#include <sgcP2P_STUN_Types.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcSocket_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcp2p_stun_classes
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcSTUNExceptionInvalidRequest;
class DELPHICLASS TsgcSTUNExceptionSilent;
class DELPHICLASS TsgcSTUN_Options;
class DELPHICLASS TsgcSTUNLogFile;
class DELPHICLASS TsgcSTUN_Component;
class DELPHICLASS TsgcSTUN_ResponseBinding;
class DELPHICLASS TsgcSTUN_ResponseError;
class DELPHICLASS TsgcSTUN_Header;
class DELPHICLASS TsgcSTUN_Attribute;
class DELPHICLASS TsgcSTUN_Attribute_None;
class DELPHICLASS TsgcSTUN_Attribute_MAPPED_ADDRESS;
class DELPHICLASS TsgcSTUN_Attribute_XOR_MAPPED_ADDRESS;
class DELPHICLASS TsgcSTUN_Attribute_USERNAME;
class DELPHICLASS TsgcSTUN_Attribute_MESSAGE_INTEGRITY;
class DELPHICLASS TsgcSTUN_Attribute_MESSAGE_INTEGRITY_SHA256;
class DELPHICLASS TsgcSTUN_Attribute_FINGERPRINT;
class DELPHICLASS TsgcSTUN_Attribute_ERROR_CODE;
class DELPHICLASS TsgcSTUN_Attribute_REALM;
class DELPHICLASS TsgcSTUN_Attribute_NONCE;
class DELPHICLASS TsgcSTUN_Attribute_UNKNOWN_ATTRIBUTES;
class DELPHICLASS TsgcSTUN_Attribute_SOFTWARE;
class DELPHICLASS TsgcSTUN_Attribute_ALTERNATE_SERVER;
class DELPHICLASS TsgcSTUN_Attribute_CHANGE_REQUEST;
class DELPHICLASS TsgcSTUN_Attribute_RESPONSE_PORT;
class DELPHICLASS TsgcSTUN_Attribute_PADDING;
class DELPHICLASS TsgcSTUN_Attribute_CACHE_TIMEOUT;
class DELPHICLASS TsgcSTUN_Attribute_RESPONSE_ORIGIN;
class DELPHICLASS TsgcSTUN_Attribute_OTHER_ADDRESS;
class DELPHICLASS TsgcSTUN_Attribute_SOURCE_ADDRESS;
class DELPHICLASS TsgcSTUN_Attribute_CHANGED_ADDRESS;
class DELPHICLASS TsgcSTUN_Attribute_PASSWORD_ALGORITHM;
class DELPHICLASS TsgcSTUN_Attribute_USERHASH;
class DELPHICLASS TsgcSTUN_Attribute_PASSWORD_ALGORITHMS;
class DELPHICLASS TsgcSTUN_Attribute_ALTERNATE_DOMAIN;
class DELPHICLASS TsgcSTUN_Attribute_CHANNEL_NUMBER;
class DELPHICLASS TsgcSTUN_Attribute_LIFETIME;
class DELPHICLASS TsgcSTUN_Attribute_XOR_PEER_ADDRESS;
class DELPHICLASS TsgcSTUN_Attribute_DATA;
class DELPHICLASS TsgcSTUN_Attribute_XOR_RELAYED_ADDRESS;
class DELPHICLASS TsgcSTUN_Attribute_REQUESTED_ADDRESS_FAMILY;
class DELPHICLASS TsgcSTUN_Attribute_EVEN_PORT;
class DELPHICLASS TsgcSTUN_Attribute_REQUESTED_TRANSPORT;
class DELPHICLASS TsgcSTUN_Attribute_DONT_FRAGMENT;
class DELPHICLASS TsgcSTUN_Attribute_RESERVATION_TOKEN;
class DELPHICLASS TsgcSTUN_Attribute_ADDITIONAL_ADDRESS_FAMILY;
class DELPHICLASS TsgcSTUN_Attribute_ADDRESS_ERROR_CODE;
class DELPHICLASS TsgcSTUN_Attribute_ICMP;
class DELPHICLASS TsgcSTUN_Attribute_PRIORITY;
class DELPHICLASS TsgcSTUN_Attribute_USE_CANDIDATE;
class DELPHICLASS TsgcSTUN_Attribute_ICE_CONTROL_BASE;
class DELPHICLASS TsgcSTUN_Attribute_ICE_CONTROLLED;
class DELPHICLASS TsgcSTUN_Attribute_ICE_CONTROLLING;
class DELPHICLASS TsgcSTUN_AttributeList;
class DELPHICLASS TsgcSTUN_Message;
//-- type declarations -------------------------------------------------------
#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUNExceptionInvalidRequest : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall TsgcSTUNExceptionInvalidRequest(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall TsgcSTUNExceptionInvalidRequest(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall TsgcSTUNExceptionInvalidRequest(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall TsgcSTUNExceptionInvalidRequest(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall TsgcSTUNExceptionInvalidRequest(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall TsgcSTUNExceptionInvalidRequest(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall TsgcSTUNExceptionInvalidRequest(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall TsgcSTUNExceptionInvalidRequest(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall TsgcSTUNExceptionInvalidRequest(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall TsgcSTUNExceptionInvalidRequest(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall TsgcSTUNExceptionInvalidRequest(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall TsgcSTUNExceptionInvalidRequest(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~TsgcSTUNExceptionInvalidRequest() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUNExceptionSilent : public System::Sysutils::Exception
{
	typedef System::Sysutils::Exception inherited;
	
public:
	/* Exception.Create */ inline __fastcall TsgcSTUNExceptionSilent(const System::UnicodeString Msg) : System::Sysutils::Exception(Msg) { }
	/* Exception.CreateFmt */ inline __fastcall TsgcSTUNExceptionSilent(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High) : System::Sysutils::Exception(Msg, Args, Args_High) { }
	/* Exception.CreateRes */ inline __fastcall TsgcSTUNExceptionSilent(NativeUInt Ident)/* overload */ : System::Sysutils::Exception(Ident) { }
	/* Exception.CreateRes */ inline __fastcall TsgcSTUNExceptionSilent(System::PResStringRec ResStringRec)/* overload */ : System::Sysutils::Exception(ResStringRec) { }
	/* Exception.CreateResFmt */ inline __fastcall TsgcSTUNExceptionSilent(NativeUInt Ident, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High) { }
	/* Exception.CreateResFmt */ inline __fastcall TsgcSTUNExceptionSilent(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High) { }
	/* Exception.CreateHelp */ inline __fastcall TsgcSTUNExceptionSilent(const System::UnicodeString Msg, int AHelpContext) : System::Sysutils::Exception(Msg, AHelpContext) { }
	/* Exception.CreateFmtHelp */ inline __fastcall TsgcSTUNExceptionSilent(const System::UnicodeString Msg, const System::TVarRec *Args, const int Args_High, int AHelpContext) : System::Sysutils::Exception(Msg, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall TsgcSTUNExceptionSilent(NativeUInt Ident, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, AHelpContext) { }
	/* Exception.CreateResHelp */ inline __fastcall TsgcSTUNExceptionSilent(System::PResStringRec ResStringRec, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall TsgcSTUNExceptionSilent(System::PResStringRec ResStringRec, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(ResStringRec, Args, Args_High, AHelpContext) { }
	/* Exception.CreateResFmtHelp */ inline __fastcall TsgcSTUNExceptionSilent(NativeUInt Ident, const System::TVarRec *Args, const int Args_High, int AHelpContext)/* overload */ : System::Sysutils::Exception(Ident, Args, Args_High, AHelpContext) { }
	/* Exception.Destroy */ inline __fastcall virtual ~TsgcSTUNExceptionSilent() { }
	
};

#pragma pack(pop)

typedef void __fastcall (__closure *TsgcSTUNExceptionEvent)(System::TObject* Sender, System::Sysutils::Exception* E);

typedef System::DynamicArray<System::Byte> _dt_Sgcp2p_stun_classes_1;
typedef void __fastcall (__closure *TsgcSTUNRequestMessageIntegrityKeyEvent)(System::TObject* Sender, const System::UnicodeString aTransactionId, _dt_Sgcp2p_stun_classes_1 &MessageIntegrityKey);

typedef void __fastcall (__closure *TsgcSTUNRequestMessageIntegrityPasswordEvent)(System::TObject* Sender, const System::UnicodeString aUsername, const System::UnicodeString aRealm, System::UnicodeString &Password);

typedef void __fastcall (__closure *TsgcICEValidateMessageIntegrityEvent)(System::TObject* Sender, const System::UnicodeString aUsername, System::UnicodeString &Password);

typedef void __fastcall (__closure *TsgcICERequestMessageIntegrityPasswordEvent)(System::TObject* Sender, TsgcSTUN_Message* const aMessage, System::UnicodeString &Password);

typedef void __fastcall (__closure *TsgcICERequestBindingEvent)(System::TObject* Sender, Sgcsocket_classes::TsgcSocketConnection* const aSocket, TsgcSTUN_Message* const aMessage);

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FFingerprint;
	bool FSoftware;
	
public:
	__fastcall virtual TsgcSTUN_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Fingerprint = {read=FFingerprint, write=FFingerprint, nodefault};
	__property bool Software = {read=FSoftware, write=FSoftware, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcSTUN_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUNLogFile : public Sgcsocket_classes::TsgcSocketLogFile
{
	typedef Sgcsocket_classes::TsgcSocketLogFile inherited;
	
public:
	/* TsgcSocketLogFile.Create */ inline __fastcall virtual TsgcSTUNLogFile() : Sgcsocket_classes::TsgcSocketLogFile() { }
	
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcSTUNLogFile() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcSTUN_Component : public Sgcbase_classes::TsgcComponent_Base
{
	typedef Sgcbase_classes::TsgcComponent_Base inherited;
	
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcSTUN_Component(System::Classes::TComponent* AOwner) : Sgcbase_classes::TsgcComponent_Base(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcSTUN_Component() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_ResponseBinding : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Sgcwebsocket_types::TwsIPVersion FIPVersion;
	System::UnicodeString FLocalIP;
	System::Word FLocalPort;
	System::UnicodeString FRemoteIP;
	System::Word FRemotePort;
	
public:
	__property Sgcwebsocket_types::TwsIPVersion IPVersion = {read=FIPVersion, write=FIPVersion, nodefault};
	__property System::UnicodeString LocalIP = {read=FLocalIP, write=FLocalIP};
	__property System::Word LocalPort = {read=FLocalPort, write=FLocalPort, nodefault};
	__property System::UnicodeString RemoteIP = {read=FRemoteIP, write=FRemoteIP};
	__property System::Word RemotePort = {read=FRemotePort, write=FRemotePort, nodefault};
public:
	/* TObject.Create */ inline __fastcall TsgcSTUN_ResponseBinding() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_ResponseBinding() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_ResponseError : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Word FCode;
	System::UnicodeString FReason;
	
public:
	__property System::Word Code = {read=FCode, write=FCode, nodefault};
	__property System::UnicodeString Reason = {read=FReason, write=FReason};
public:
	/* TObject.Create */ inline __fastcall TsgcSTUN_ResponseError() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_ResponseError() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Header : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	unsigned FMagicCookie;
	Sgcp2p_stun_types::TsgcStunMessageClass FMessageType_Class;
	System::Word FMessageLength;
	System::Word FMessageType;
	Sgcp2p_stun_types::TsgcStunMessageMethod FMessageType_Method;
	System::DynamicArray<System::Byte> FTransactionId;
	System::UnicodeString __fastcall GetTransactionIdString();
	
public:
	__fastcall virtual TsgcSTUN_Header();
	void __fastcall Read(const System::DynamicArray<System::Byte> aBytes);
	System::DynamicArray<System::Byte> __fastcall Write();
	__property Sgcp2p_stun_types::TsgcStunMessageClass MessageType_Class = {read=FMessageType_Class, write=FMessageType_Class, nodefault};
	__property Sgcp2p_stun_types::TsgcStunMessageMethod MessageType_Method = {read=FMessageType_Method, write=FMessageType_Method, nodefault};
	__property unsigned MagicCookie = {read=FMagicCookie, write=FMagicCookie, nodefault};
	__property System::Word MessageLength = {read=FMessageLength, write=FMessageLength, nodefault};
	__property System::Word MessageType = {read=FMessageType, write=FMessageType, nodefault};
	__property System::DynamicArray<System::Byte> TransactionId = {read=FTransactionId, write=FTransactionId};
	__property System::UnicodeString TransactionIdString = {read=GetTransactionIdString};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Header() { }
	
};

#pragma pack(pop)

typedef System::TMetaClass* TsgcSTUN_Attribute_Class;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FAddress;
	System::Word FFamily;
	System::Word FPort;
	Sgcp2p_stun_types::TsgcStunMessageAttribute FAttributeType;
	System::Word FAttributeLength;
	int FOffset;
	void __fastcall IncOffset(System::Word Value = (System::Word)(0x1));
	
protected:
	virtual System::DynamicArray<System::Byte> __fastcall GetPayload() = 0 ;
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value) = 0 ;
	virtual System::Word __fastcall GetPayloadLength();
	
public:
	__fastcall virtual TsgcSTUN_Attribute();
	virtual void __fastcall Read(System::DynamicArray<System::Byte> Value, int &aOffset);
	virtual System::DynamicArray<System::Byte> __fastcall Write();
	__property Sgcp2p_stun_types::TsgcStunMessageAttribute AttributeType = {read=FAttributeType, write=FAttributeType, nodefault};
	__property System::Word AttributeLength = {read=FAttributeLength, write=FAttributeLength, nodefault};
	__property System::UnicodeString Address = {read=FAddress, write=FAddress};
	__property System::Word Family = {read=FFamily, write=FFamily, nodefault};
	__property System::Word Port = {read=FPort, write=FPort, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute() { }
	
};

#pragma pack(pop)

typedef System::DynamicArray<TsgcSTUN_Attribute*> TsgcSTUN_Attributes;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_None : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
protected:
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
public:
	/* TsgcSTUN_Attribute.Create */ inline __fastcall virtual TsgcSTUN_Attribute_None() : TsgcSTUN_Attribute() { }
	
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_None() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_MAPPED_ADDRESS : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
protected:
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
	virtual System::DynamicArray<System::Byte> __fastcall GetPayload();
	
public:
	__fastcall virtual TsgcSTUN_Attribute_MAPPED_ADDRESS();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_MAPPED_ADDRESS() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_XOR_MAPPED_ADDRESS : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
private:
	System::DynamicArray<System::Byte> FTransactionId;
	void __fastcall SetTransactionId(const System::DynamicArray<System::Byte> Value);
	
protected:
	virtual System::DynamicArray<System::Byte> __fastcall GetPayload();
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
	
public:
	__fastcall virtual TsgcSTUN_Attribute_XOR_MAPPED_ADDRESS();
	__property System::DynamicArray<System::Byte> TransactionId = {read=FTransactionId, write=SetTransactionId};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_XOR_MAPPED_ADDRESS() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_USERNAME : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
private:
	System::UnicodeString FUsername;
	
public:
	__fastcall virtual TsgcSTUN_Attribute_USERNAME();
	
protected:
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
	virtual System::DynamicArray<System::Byte> __fastcall GetPayload();
	
public:
	__property System::UnicodeString Username = {read=FUsername, write=FUsername};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_USERNAME() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_MESSAGE_INTEGRITY : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
private:
	System::DynamicArray<System::Byte> FKey;
	System::DynamicArray<System::Byte> FPayLoad;
	void __fastcall SetKey(const System::DynamicArray<System::Byte> Value);
	void __fastcall SetPayLoad(const System::DynamicArray<System::Byte> Value);
	
public:
	__fastcall virtual TsgcSTUN_Attribute_MESSAGE_INTEGRITY();
	
protected:
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
	virtual System::DynamicArray<System::Byte> __fastcall GetPayload();
	
public:
	__property System::DynamicArray<System::Byte> Key = {read=FKey, write=SetKey};
	__property System::DynamicArray<System::Byte> PayLoad = {read=FPayLoad, write=SetPayLoad};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_MESSAGE_INTEGRITY() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_MESSAGE_INTEGRITY_SHA256 : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
private:
	System::DynamicArray<System::Byte> FKey;
	System::DynamicArray<System::Byte> FPayLoad;
	void __fastcall SetKey(const System::DynamicArray<System::Byte> Value);
	void __fastcall SetPayLoad(const System::DynamicArray<System::Byte> Value);
	
public:
	__fastcall virtual TsgcSTUN_Attribute_MESSAGE_INTEGRITY_SHA256();
	
protected:
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
	virtual System::DynamicArray<System::Byte> __fastcall GetPayload();
	
public:
	__property System::DynamicArray<System::Byte> Key = {read=FKey, write=SetKey};
	__property System::DynamicArray<System::Byte> PayLoad = {read=FPayLoad, write=SetPayLoad};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_MESSAGE_INTEGRITY_SHA256() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_FINGERPRINT : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
private:
	unsigned FFingerprint;
	
public:
	__fastcall virtual TsgcSTUN_Attribute_FINGERPRINT();
	
protected:
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
	virtual System::DynamicArray<System::Byte> __fastcall GetPayload();
	
public:
	__property unsigned Fingerprint = {read=FFingerprint, write=FFingerprint, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_FINGERPRINT() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_ERROR_CODE : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
private:
	System::Word FCode;
	System::UnicodeString FReason;
	
protected:
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
	virtual System::DynamicArray<System::Byte> __fastcall GetPayload();
	
public:
	__fastcall virtual TsgcSTUN_Attribute_ERROR_CODE();
	__property System::UnicodeString Reason = {read=FReason, write=FReason};
	__property System::Word Code = {read=FCode, write=FCode, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_ERROR_CODE() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_REALM : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
private:
	System::UnicodeString FRealm;
	
public:
	__fastcall virtual TsgcSTUN_Attribute_REALM();
	
protected:
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
	virtual System::DynamicArray<System::Byte> __fastcall GetPayload();
	
public:
	__property System::UnicodeString Realm = {read=FRealm, write=FRealm};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_REALM() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_NONCE : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
private:
	System::UnicodeString FNonce;
	
public:
	__fastcall virtual TsgcSTUN_Attribute_NONCE();
	
protected:
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
	virtual System::DynamicArray<System::Byte> __fastcall GetPayload();
	
public:
	__property System::UnicodeString Nonce = {read=FNonce, write=FNonce};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_NONCE() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_UNKNOWN_ATTRIBUTES : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
private:
	Sgcp2p_stun_types::TsgcStunMessageAttributes FUnknownAttributes;
	
public:
	__fastcall virtual TsgcSTUN_Attribute_UNKNOWN_ATTRIBUTES();
	
protected:
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
	virtual System::DynamicArray<System::Byte> __fastcall GetPayload();
	
public:
	__property Sgcp2p_stun_types::TsgcStunMessageAttributes UnknownAttributes = {read=FUnknownAttributes, write=FUnknownAttributes};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_UNKNOWN_ATTRIBUTES() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_SOFTWARE : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
private:
	System::UnicodeString FSoftware;
	
public:
	__fastcall virtual TsgcSTUN_Attribute_SOFTWARE();
	
protected:
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
	virtual System::DynamicArray<System::Byte> __fastcall GetPayload();
	
public:
	__property System::UnicodeString Software = {read=FSoftware, write=FSoftware};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_SOFTWARE() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_ALTERNATE_SERVER : public TsgcSTUN_Attribute_MAPPED_ADDRESS
{
	typedef TsgcSTUN_Attribute_MAPPED_ADDRESS inherited;
	
public:
	__fastcall virtual TsgcSTUN_Attribute_ALTERNATE_SERVER();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_ALTERNATE_SERVER() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_CHANGE_REQUEST : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
public:
	__fastcall virtual TsgcSTUN_Attribute_CHANGE_REQUEST();
	
protected:
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
	virtual System::DynamicArray<System::Byte> __fastcall GetPayload();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_CHANGE_REQUEST() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_RESPONSE_PORT : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
public:
	__fastcall virtual TsgcSTUN_Attribute_RESPONSE_PORT();
	
protected:
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
	virtual System::DynamicArray<System::Byte> __fastcall GetPayload();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_RESPONSE_PORT() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_PADDING : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
public:
	__fastcall virtual TsgcSTUN_Attribute_PADDING();
	
protected:
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
	virtual System::DynamicArray<System::Byte> __fastcall GetPayload();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_PADDING() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_CACHE_TIMEOUT : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
public:
	__fastcall virtual TsgcSTUN_Attribute_CACHE_TIMEOUT();
	
protected:
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
	virtual System::DynamicArray<System::Byte> __fastcall GetPayload();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_CACHE_TIMEOUT() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_RESPONSE_ORIGIN : public TsgcSTUN_Attribute_MAPPED_ADDRESS
{
	typedef TsgcSTUN_Attribute_MAPPED_ADDRESS inherited;
	
public:
	__fastcall virtual TsgcSTUN_Attribute_RESPONSE_ORIGIN();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_RESPONSE_ORIGIN() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_OTHER_ADDRESS : public TsgcSTUN_Attribute_MAPPED_ADDRESS
{
	typedef TsgcSTUN_Attribute_MAPPED_ADDRESS inherited;
	
public:
	__fastcall virtual TsgcSTUN_Attribute_OTHER_ADDRESS();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_OTHER_ADDRESS() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_SOURCE_ADDRESS : public TsgcSTUN_Attribute_MAPPED_ADDRESS
{
	typedef TsgcSTUN_Attribute_MAPPED_ADDRESS inherited;
	
public:
	__fastcall virtual TsgcSTUN_Attribute_SOURCE_ADDRESS();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_SOURCE_ADDRESS() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_CHANGED_ADDRESS : public TsgcSTUN_Attribute_MAPPED_ADDRESS
{
	typedef TsgcSTUN_Attribute_MAPPED_ADDRESS inherited;
	
public:
	__fastcall virtual TsgcSTUN_Attribute_CHANGED_ADDRESS();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_CHANGED_ADDRESS() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_PASSWORD_ALGORITHM : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
public:
	__fastcall virtual TsgcSTUN_Attribute_PASSWORD_ALGORITHM();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_PASSWORD_ALGORITHM() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_USERHASH : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
public:
	__fastcall virtual TsgcSTUN_Attribute_USERHASH();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_USERHASH() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_PASSWORD_ALGORITHMS : public TsgcSTUN_Attribute_MAPPED_ADDRESS
{
	typedef TsgcSTUN_Attribute_MAPPED_ADDRESS inherited;
	
public:
	__fastcall virtual TsgcSTUN_Attribute_PASSWORD_ALGORITHMS();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_PASSWORD_ALGORITHMS() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_ALTERNATE_DOMAIN : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
private:
	System::UnicodeString FDomain;
	
protected:
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
	virtual System::DynamicArray<System::Byte> __fastcall GetPayload();
	
public:
	__fastcall virtual TsgcSTUN_Attribute_ALTERNATE_DOMAIN();
	__property System::UnicodeString Domain = {read=FDomain, write=FDomain};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_ALTERNATE_DOMAIN() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_CHANNEL_NUMBER : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
private:
	System::Word FChannel;
	
protected:
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
	virtual System::DynamicArray<System::Byte> __fastcall GetPayload();
	
public:
	__fastcall virtual TsgcSTUN_Attribute_CHANNEL_NUMBER();
	__property System::Word Channel = {read=FChannel, write=FChannel, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_CHANNEL_NUMBER() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_LIFETIME : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
private:
	unsigned FLifetime;
	
protected:
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
	virtual System::DynamicArray<System::Byte> __fastcall GetPayload();
	
public:
	__fastcall virtual TsgcSTUN_Attribute_LIFETIME();
	__property unsigned Lifetime = {read=FLifetime, write=FLifetime, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_LIFETIME() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_XOR_PEER_ADDRESS : public TsgcSTUN_Attribute_XOR_MAPPED_ADDRESS
{
	typedef TsgcSTUN_Attribute_XOR_MAPPED_ADDRESS inherited;
	
public:
	__fastcall virtual TsgcSTUN_Attribute_XOR_PEER_ADDRESS();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_XOR_PEER_ADDRESS() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_DATA : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
private:
	System::DynamicArray<System::Byte> FData;
	void __fastcall SetData(const System::DynamicArray<System::Byte> Value);
	
protected:
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
	virtual System::DynamicArray<System::Byte> __fastcall GetPayload();
	
public:
	__fastcall virtual TsgcSTUN_Attribute_DATA();
	__property System::DynamicArray<System::Byte> Data = {read=FData, write=SetData};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_DATA() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_XOR_RELAYED_ADDRESS : public TsgcSTUN_Attribute_XOR_MAPPED_ADDRESS
{
	typedef TsgcSTUN_Attribute_XOR_MAPPED_ADDRESS inherited;
	
public:
	__fastcall virtual TsgcSTUN_Attribute_XOR_RELAYED_ADDRESS();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_XOR_RELAYED_ADDRESS() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_REQUESTED_ADDRESS_FAMILY : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
private:
	System::Byte FFamily;
	
protected:
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
	virtual System::DynamicArray<System::Byte> __fastcall GetPayload();
	
public:
	__fastcall virtual TsgcSTUN_Attribute_REQUESTED_ADDRESS_FAMILY();
	__property System::Byte Family = {read=FFamily, write=FFamily, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_REQUESTED_ADDRESS_FAMILY() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_EVEN_PORT : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
private:
	bool FEven;
	
protected:
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
	virtual System::DynamicArray<System::Byte> __fastcall GetPayload();
	
public:
	__fastcall virtual TsgcSTUN_Attribute_EVEN_PORT();
	__property bool Even = {read=FEven, write=FEven, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_EVEN_PORT() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_REQUESTED_TRANSPORT : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
private:
	System::Byte FProtocol;
	
protected:
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
	virtual System::DynamicArray<System::Byte> __fastcall GetPayload();
	
public:
	__fastcall virtual TsgcSTUN_Attribute_REQUESTED_TRANSPORT();
	__property System::Byte Protocol = {read=FProtocol, write=FProtocol, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_REQUESTED_TRANSPORT() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_DONT_FRAGMENT : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
protected:
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
	
public:
	__fastcall virtual TsgcSTUN_Attribute_DONT_FRAGMENT();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_DONT_FRAGMENT() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_RESERVATION_TOKEN : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
private:
	System::DynamicArray<System::Byte> FToken;
	
protected:
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
	virtual System::DynamicArray<System::Byte> __fastcall GetPayload();
	
public:
	__fastcall virtual TsgcSTUN_Attribute_RESERVATION_TOKEN();
	__property System::DynamicArray<System::Byte> Token = {read=FToken, write=FToken};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_RESERVATION_TOKEN() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_ADDITIONAL_ADDRESS_FAMILY : public TsgcSTUN_Attribute_REQUESTED_ADDRESS_FAMILY
{
	typedef TsgcSTUN_Attribute_REQUESTED_ADDRESS_FAMILY inherited;
	
public:
	__fastcall virtual TsgcSTUN_Attribute_ADDITIONAL_ADDRESS_FAMILY();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_ADDITIONAL_ADDRESS_FAMILY() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_ADDRESS_ERROR_CODE : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
private:
	System::Word FCode;
	System::Byte FFamily;
	System::UnicodeString FReason;
	
protected:
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
	virtual System::DynamicArray<System::Byte> __fastcall GetPayload();
	
public:
	__fastcall virtual TsgcSTUN_Attribute_ADDRESS_ERROR_CODE();
	__property System::Word Code = {read=FCode, write=FCode, nodefault};
	__property System::Byte Family = {read=FFamily, write=FFamily, nodefault};
	__property System::UnicodeString Reason = {read=FReason, write=FReason};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_ADDRESS_ERROR_CODE() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_ICMP : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
private:
	unsigned FErrorData;
	System::Word FICMPCode;
	System::Byte FICMPType;
	
protected:
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
	virtual System::DynamicArray<System::Byte> __fastcall GetPayload();
	
public:
	__fastcall virtual TsgcSTUN_Attribute_ICMP();
	__property unsigned ErrorData = {read=FErrorData, write=FErrorData, nodefault};
	__property System::Word ICMPCode = {read=FICMPCode, write=FICMPCode, nodefault};
	__property System::Byte ICMPType = {read=FICMPType, write=FICMPType, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_ICMP() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_PRIORITY : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
private:
	unsigned FPriority;
	
protected:
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
	virtual System::DynamicArray<System::Byte> __fastcall GetPayload();
	
public:
	__fastcall virtual TsgcSTUN_Attribute_PRIORITY();
	__property unsigned Priority = {read=FPriority, write=FPriority, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_PRIORITY() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_USE_CANDIDATE : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
protected:
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
	virtual System::DynamicArray<System::Byte> __fastcall GetPayload();
	
public:
	__fastcall virtual TsgcSTUN_Attribute_USE_CANDIDATE();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_USE_CANDIDATE() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_ICE_CONTROL_BASE : public TsgcSTUN_Attribute
{
	typedef TsgcSTUN_Attribute inherited;
	
private:
	System::DynamicArray<System::Byte> FTieBreaker;
	void __fastcall SetTieBreaker(const System::DynamicArray<System::Byte> Value);
	
protected:
	virtual void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Value);
	virtual System::DynamicArray<System::Byte> __fastcall GetPayload();
	
public:
	__fastcall virtual TsgcSTUN_Attribute_ICE_CONTROL_BASE();
	__property System::DynamicArray<System::Byte> TieBreaker = {read=FTieBreaker, write=SetTieBreaker};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_ICE_CONTROL_BASE() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_ICE_CONTROLLED : public TsgcSTUN_Attribute_ICE_CONTROL_BASE
{
	typedef TsgcSTUN_Attribute_ICE_CONTROL_BASE inherited;
	
public:
	__fastcall virtual TsgcSTUN_Attribute_ICE_CONTROLLED();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_ICE_CONTROLLED() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_Attribute_ICE_CONTROLLING : public TsgcSTUN_Attribute_ICE_CONTROL_BASE
{
	typedef TsgcSTUN_Attribute_ICE_CONTROL_BASE inherited;
	
public:
	__fastcall virtual TsgcSTUN_Attribute_ICE_CONTROLLING();
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcSTUN_Attribute_ICE_CONTROLLING() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcSTUN_AttributeList : public System::Classes::TList
{
	typedef System::Classes::TList inherited;
	
public:
	__fastcall virtual ~TsgcSTUN_AttributeList();
	TsgcSTUN_Attribute* __fastcall GetAttribute(const Sgcp2p_stun_types::TsgcStunMessageAttribute aType);
	System::DynamicArray<System::Byte> __fastcall Write();
public:
	/* TObject.Create */ inline __fastcall TsgcSTUN_AttributeList() : System::Classes::TList() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcSTUN_Message : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TsgcSTUN_AttributeList* FAttributes;
	Sgcp2p_stun_types::TsgcStunFingerprintState FFingerprint;
	Sgcp2p_stun_types::TsgcStunCredentials FCredentials;
	TsgcSTUN_Header* FHeader;
	Sgcp2p_stun_types::TsgcStunMessageIntegrityState FMessageIntegrity;
	Sgcp2p_stun_types::TsgcStunMessageIntegrityState FMessageIntegritySHA256;
	Sgcp2p_stun_types::TsgcStunMessageType FMessageType;
	TsgcSTUN_AttributeList* __fastcall GetAttributes();
	TsgcSTUN_Header* __fastcall GetHeader();
	
public:
	__fastcall virtual TsgcSTUN_Message();
	__fastcall virtual ~TsgcSTUN_Message();
	
private:
	bool __fastcall IsFingerprintValid(TsgcSTUN_Attribute* const aAttribute, const System::DynamicArray<System::Byte> aBytes);
	void __fastcall DoValidateMessageIntegrity(TsgcSTUN_Attribute* const aAttribute, const System::DynamicArray<System::Byte> aBytes, bool aICE);
	void __fastcall DoComputeFingerprint();
	void __fastcall DoComputeCredentials();
	void __fastcall DoComputeHeaderMessageLength();
	void __fastcall SetMessageType(const Sgcp2p_stun_types::TsgcStunMessageType Value);
	
protected:
	void __fastcall DoReadHeader(System::DynamicArray<System::Byte> Bytes);
	void __fastcall DoReadPayload(System::DynamicArray<System::Byte> Bytes);
	virtual System::DynamicArray<System::Byte> __fastcall DoWrite();
	
public:
	void __fastcall Read(System::DynamicArray<System::Byte> Bytes);
	System::DynamicArray<System::Byte> __fastcall Write();
	__property TsgcSTUN_AttributeList* Attributes = {read=GetAttributes, write=FAttributes};
	__property TsgcSTUN_Header* Header = {read=GetHeader, write=FHeader};
	__property Sgcp2p_stun_types::TsgcStunFingerprintState Fingerprint = {read=FFingerprint, write=FFingerprint, nodefault};
	__property Sgcp2p_stun_types::TsgcStunMessageIntegrityState MessageIntegrity = {read=FMessageIntegrity, write=FMessageIntegrity, nodefault};
	__property Sgcp2p_stun_types::TsgcStunMessageIntegrityState MessageIntegritySHA256 = {read=FMessageIntegritySHA256, write=FMessageIntegritySHA256, nodefault};
	__property Sgcp2p_stun_types::TsgcStunCredentials Credentials = {read=FCredentials, write=FCredentials, nodefault};
	__property Sgcp2p_stun_types::TsgcStunMessageType MessageType = {read=FMessageType, write=SetMessageType, nodefault};
	
private:
	System::UnicodeString FNonce;
	System::UnicodeString FPassword;
	System::UnicodeString FRealm;
	System::UnicodeString FUsername;
	
public:
	__property System::UnicodeString Nonce = {read=FNonce, write=FNonce};
	__property System::UnicodeString Password = {read=FPassword, write=FPassword};
	__property System::UnicodeString Realm = {read=FRealm, write=FRealm};
	__property System::UnicodeString Username = {read=FUsername, write=FUsername};
	
private:
	TsgcSTUNRequestMessageIntegrityKeyEvent FOnRequestMessageIntegrityKey;
	TsgcSTUNRequestMessageIntegrityPasswordEvent FOnRequestMessageIntegrityPassword;
	TsgcICEValidateMessageIntegrityEvent FOnICEValidateMessageIntegrity;
	TsgcICERequestMessageIntegrityPasswordEvent FOnICERequestMessageIntegrityPassword;
	
public:
	__property TsgcSTUNRequestMessageIntegrityKeyEvent OnRequestMessageIntegrityKey = {read=FOnRequestMessageIntegrityKey, write=FOnRequestMessageIntegrityKey};
	__property TsgcSTUNRequestMessageIntegrityPasswordEvent OnRequestMessageIntegrityPassword = {read=FOnRequestMessageIntegrityPassword, write=FOnRequestMessageIntegrityPassword};
	__property TsgcICEValidateMessageIntegrityEvent OnICEValidateMessageIntegrity = {read=FOnICEValidateMessageIntegrity, write=FOnICEValidateMessageIntegrity};
	__property TsgcICERequestMessageIntegrityPasswordEvent OnICERequestMessageIntegrityPassword = {read=FOnICERequestMessageIntegrityPassword, write=FOnICERequestMessageIntegrityPassword};
};


//-- var, const, procedure ---------------------------------------------------
static const System::Word CS_STUN_DEFAULT_PORT = System::Word(0xd96);
static const int CS_STUN_MAGIC_COOKIE = int(0x2112a442);
static const int CS_STUN_FINGERPRINT_XOR = int(0x5354554e);
static const System::Int8 CS_STUN_ATTRIBUTE_REQUIRED_MAPPED_ADDRESS = System::Int8(0x1);
static const System::Int8 CS_STUN_ATTRIBUTE_REQUIRED_USERNAME = System::Int8(0x6);
static const System::Int8 CS_STUN_ATTRIBUTE_REQUIRED_MESSAGE_INTEGRITY = System::Int8(0x8);
static const System::Int8 CS_STUN_ATTRIBUTE_REQUIRED_ERROR_CODE = System::Int8(0x9);
static const System::Int8 CS_STUN_ATTRIBUTE_REQUIRED_UNKNOWN_ATTRIBUTES = System::Int8(0xa);
static const System::Int8 CS_STUN_ATTRIBUTE_REQUIRED_REALM = System::Int8(0x14);
static const System::Int8 CS_STUN_ATTRIBUTE_REQUIRED_NONCE = System::Int8(0x15);
static const System::Int8 CS_STUN_ATTRIBUTE_REQUIRED_XOR_MAPPED_ADDRESS = System::Int8(0x20);
static const System::Word CS_STUN_ATTRIBUTE_OPTIONAL_SOFTWARE = System::Word(0x8022);
static const System::Word CS_STUN_ATTRIBUTE_OPTIONAL_ALTERNATE_SERVER = System::Word(0x8023);
static const System::Word CS_STUN_ATTRIBUTE_OPTIONAL_FINGERPRINT = System::Word(0x8028);
static const System::Int8 CS_STUN_ATTRIBUTE_CHANGE_REQUEST = System::Int8(0x3);
static const System::Int8 CS_STUN_ATTRIBUTE_RESPONSE_PORT = System::Int8(0x27);
static const System::Int8 CS_STUN_ATTRIBUTE_PADDING = System::Int8(0x26);
static const System::Word CS_STUN_ATTRIBUTE_CACHE_TIMEOUT = System::Word(0x8027);
static const System::Word CS_STUN_ATTRIBUTE_RESPONSE_ORIGIN = System::Word(0x802b);
static const System::Word CS_STUN_ATTRIBUTE_OTHER_ADDRESS = System::Word(0x802c);
static const System::Int8 CS_STUN_ATTRIBUTE_SOURCE_ADDRESS = System::Int8(0x4);
static const System::Int8 CS_STUN_ATTRIBUTE_CHANGED_ADDRESS = System::Int8(0x5);
static const System::Int8 CS_STUN_ATTRIBUTE_REQUIRED_MESSAGE_INTEGRITY_SHA256 = System::Int8(0x1c);
static const System::Int8 CS_STUN_ATTRIBUTE_REQUIRED_PASSWORD_ALGORITHM = System::Int8(0x1d);
static const System::Int8 CS_STUN_ATTRIBUTE_REQUIRED_USERHASH = System::Int8(0x1e);
static const System::Word CS_STUN_ATTRIBUTE_OPTIONAL_PASSWORD_ALGORITHMS = System::Word(0x8002);
static const System::Word CS_STUN_ATTRIBUTE_OPTIONAL_ALTERNATE_DOMAIN = System::Word(0x8003);
static const System::Int8 CS_STUN_ATTRIBUTE_CHANNEL_NUMBER = System::Int8(0xc);
static const System::Int8 CS_STUN_ATTRIBUTE_LIFETIME = System::Int8(0xd);
static const System::Int8 CS_STUN_ATTRIBUTE_XOR_PEER_ADDRESS = System::Int8(0x12);
static const System::Int8 CS_STUN_ATTRIBUTE_DATA = System::Int8(0x13);
static const System::Int8 CS_STUN_ATTRIBUTE_XOR_RELAYED_ADDRESS = System::Int8(0x16);
static const System::Int8 CS_STUN_ATTRIBUTE_REQUESTED_ADDRESS_FAMILY = System::Int8(0x17);
static const System::Int8 CS_STUN_ATTRIBUTE_EVENT_PORT = System::Int8(0x18);
static const System::Int8 CS_STUN_ATTRIBUTE_REQUESTED_TRANSPORT = System::Int8(0x19);
static const System::Int8 CS_STUN_ATTRIBUTE_DONT_FRAGMENT = System::Int8(0x1a);
static const System::Int8 CS_STUN_ATTRIBUTE_RESERVATION_TOKEN = System::Int8(0x22);
static const System::Word CS_STUN_ATTRIBUTE_ADDITIONAL_ADDRESS_FAMILY = System::Word(0x8000);
static const System::Word CS_STUN_ATTRIBUTE_ADDRESS_ERROR_CODE = System::Word(0x8001);
static const System::Word CS_STUN_ATTRIBUTE_ICMP = System::Word(0x8004);
static const System::Int8 CS_STUN_ATTRIBUTE_PRIORITY = System::Int8(0x24);
static const System::Int8 CS_STUN_ATTRIBUTE_USE_CANDIDATE = System::Int8(0x25);
static const System::Word CS_STUN_ATTRIBUTE_ICE_CONTROLLED = System::Word(0x8029);
static const System::Word CS_STUN_ATTRIBUTE_ICE_CONTROLLING = System::Word(0x802a);
static const System::Int8 CS_STUN_HEADER_LENGTH = System::Int8(0x14);
static const System::Int8 CS_STUN_ATTRIBUTE_HEADER_LENGTH = System::Int8(0x4);
static const System::Int8 CS_STUN_ATTRIBUTE_FINGERPRINT_LENGTH = System::Int8(0x4);
static const System::Int8 CS_STUN_ATTRIBUTE_MESSAGE_INTEGRITY_LENGTH = System::Int8(0x14);
static const System::Int8 CS_STUN_ATTRIBUTE_MESSAGE_INTEGRITY_SHA256_LENGTH = System::Int8(0x20);
#define CS_STUN_RELAY L"Relay"
extern DELPHI_PACKAGE System::ResourceString _S_STUN_INVALID_HEADER_FORMAT;
#define Sgcp2p_stun_classes_S_STUN_INVALID_HEADER_FORMAT System::LoadResourceString(&Sgcp2p_stun_classes::_S_STUN_INVALID_HEADER_FORMAT)
extern DELPHI_PACKAGE System::ResourceString _S_STUN_INVALID_MESSAGE_CLASS;
#define Sgcp2p_stun_classes_S_STUN_INVALID_MESSAGE_CLASS System::LoadResourceString(&Sgcp2p_stun_classes::_S_STUN_INVALID_MESSAGE_CLASS)
extern DELPHI_PACKAGE System::ResourceString _S_STUN_INVALID_FINGERPRINT;
#define Sgcp2p_stun_classes_S_STUN_INVALID_FINGERPRINT System::LoadResourceString(&Sgcp2p_stun_classes::_S_STUN_INVALID_FINGERPRINT)
extern DELPHI_PACKAGE System::ResourceString _S_STUN_INVALID_MESSAGE_INTEGRITY;
#define Sgcp2p_stun_classes_S_STUN_INVALID_MESSAGE_INTEGRITY System::LoadResourceString(&Sgcp2p_stun_classes::_S_STUN_INVALID_MESSAGE_INTEGRITY)
extern DELPHI_PACKAGE System::ResourceString _S_STUN_UKNOWN_ATTRIBUTES;
#define Sgcp2p_stun_classes_S_STUN_UKNOWN_ATTRIBUTES System::LoadResourceString(&Sgcp2p_stun_classes::_S_STUN_UKNOWN_ATTRIBUTES)
extern DELPHI_PACKAGE System::ResourceString _S_STUN_ATTRIBUTE_NOT_FOUND;
#define Sgcp2p_stun_classes_S_STUN_ATTRIBUTE_NOT_FOUND System::LoadResourceString(&Sgcp2p_stun_classes::_S_STUN_ATTRIBUTE_NOT_FOUND)
extern DELPHI_PACKAGE System::ResourceString _S_STUN_INVALID_FORMAT_ATTRIBUTE;
#define Sgcp2p_stun_classes_S_STUN_INVALID_FORMAT_ATTRIBUTE System::LoadResourceString(&Sgcp2p_stun_classes::_S_STUN_INVALID_FORMAT_ATTRIBUTE)
extern DELPHI_PACKAGE System::ResourceString _S_STUN_INVALID_REQUEST;
#define Sgcp2p_stun_classes_S_STUN_INVALID_REQUEST System::LoadResourceString(&Sgcp2p_stun_classes::_S_STUN_INVALID_REQUEST)
extern DELPHI_PACKAGE System::ResourceString _S_STUN_UNAUTHORIZED;
#define Sgcp2p_stun_classes_S_STUN_UNAUTHORIZED System::LoadResourceString(&Sgcp2p_stun_classes::_S_STUN_UNAUTHORIZED)
extern DELPHI_PACKAGE System::ResourceString _S_STUN_STALE_NONCE;
#define Sgcp2p_stun_classes_S_STUN_STALE_NONCE System::LoadResourceString(&Sgcp2p_stun_classes::_S_STUN_STALE_NONCE)
extern DELPHI_PACKAGE System::ResourceString _S_STUN_UNKNOWN_ATTRIBUTE;
#define Sgcp2p_stun_classes_S_STUN_UNKNOWN_ATTRIBUTE System::LoadResourceString(&Sgcp2p_stun_classes::_S_STUN_UNKNOWN_ATTRIBUTE)
extern DELPHI_PACKAGE System::ResourceString _S_STUN_TRY_ALTERNATE;
#define Sgcp2p_stun_classes_S_STUN_TRY_ALTERNATE System::LoadResourceString(&Sgcp2p_stun_classes::_S_STUN_TRY_ALTERNATE)
extern DELPHI_PACKAGE System::ResourceString _S_STUN_FORBIDDEN;
#define Sgcp2p_stun_classes_S_STUN_FORBIDDEN System::LoadResourceString(&Sgcp2p_stun_classes::_S_STUN_FORBIDDEN)
extern DELPHI_PACKAGE System::ResourceString _S_STUN_ALLOCATION_MISMATCH;
#define Sgcp2p_stun_classes_S_STUN_ALLOCATION_MISMATCH System::LoadResourceString(&Sgcp2p_stun_classes::_S_STUN_ALLOCATION_MISMATCH)
extern DELPHI_PACKAGE System::ResourceString _S_STUN_ADDRESS_FAMILY_NOT_SUPPORTED;
#define Sgcp2p_stun_classes_S_STUN_ADDRESS_FAMILY_NOT_SUPPORTED System::LoadResourceString(&Sgcp2p_stun_classes::_S_STUN_ADDRESS_FAMILY_NOT_SUPPORTED)
extern DELPHI_PACKAGE System::ResourceString _S_STUN_WRONG_CREDENTIALS;
#define Sgcp2p_stun_classes_S_STUN_WRONG_CREDENTIALS System::LoadResourceString(&Sgcp2p_stun_classes::_S_STUN_WRONG_CREDENTIALS)
extern DELPHI_PACKAGE System::ResourceString _S_STUN_UNSUPPORTED_TRANSPORT_PROTOCOL;
#define Sgcp2p_stun_classes_S_STUN_UNSUPPORTED_TRANSPORT_PROTOCOL System::LoadResourceString(&Sgcp2p_stun_classes::_S_STUN_UNSUPPORTED_TRANSPORT_PROTOCOL)
extern DELPHI_PACKAGE System::ResourceString _S_STUN_PEER_ADDRESS_FAMILY_MISMATCH;
#define Sgcp2p_stun_classes_S_STUN_PEER_ADDRESS_FAMILY_MISMATCH System::LoadResourceString(&Sgcp2p_stun_classes::_S_STUN_PEER_ADDRESS_FAMILY_MISMATCH)
extern DELPHI_PACKAGE System::ResourceString _S_STUN_ALLOCATION_QUOTA_REACHED;
#define Sgcp2p_stun_classes_S_STUN_ALLOCATION_QUOTA_REACHED System::LoadResourceString(&Sgcp2p_stun_classes::_S_STUN_ALLOCATION_QUOTA_REACHED)
extern DELPHI_PACKAGE System::ResourceString _S_STUN_INSUFFICIENT_CAPACITY;
#define Sgcp2p_stun_classes_S_STUN_INSUFFICIENT_CAPACITY System::LoadResourceString(&Sgcp2p_stun_classes::_S_STUN_INSUFFICIENT_CAPACITY)
extern DELPHI_PACKAGE System::ResourceString _S_STUN_ICE_ROLE_CONFLICT;
#define Sgcp2p_stun_classes_S_STUN_ICE_ROLE_CONFLICT System::LoadResourceString(&Sgcp2p_stun_classes::_S_STUN_ICE_ROLE_CONFLICT)
extern DELPHI_PACKAGE Sgcp2p_stun_types::TsgcStunMessageAttribute __fastcall GetAttributeTypeFromValue(System::Word aValue);
extern DELPHI_PACKAGE System::Word __fastcall GetAttributeTypeFromType(Sgcp2p_stun_types::TsgcStunMessageAttribute aValue);
extern DELPHI_PACKAGE TsgcSTUN_Attribute_Class __fastcall GetAttributeClassFromType(Sgcp2p_stun_types::TsgcStunMessageAttribute aValue);
extern DELPHI_PACKAGE void __fastcall GetErrorResponseCode(Sgcp2p_stun_types::TsgcStunErrorResponseCodes aValue, System::Word &Code, System::UnicodeString &Reason);
}	/* namespace Sgcp2p_stun_classes */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCP2P_STUN_CLASSES)
using namespace Sgcp2p_stun_classes;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcp2p_stun_classesHPP
