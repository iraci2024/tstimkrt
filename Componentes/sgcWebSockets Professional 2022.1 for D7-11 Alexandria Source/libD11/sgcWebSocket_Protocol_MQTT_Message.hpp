// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_MQTT_Message.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_mqtt_messageHPP
#define Sgcwebsocket_protocol_mqtt_messageHPP

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
#include <IdGlobal.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcBase_Classes.hpp>
#include <sgcBase_Helpers.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_mqtt_message
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSTopic;
class DELPHICLASS TsgcWSTopics;
class DELPHICLASS TsgcWSSUBACK;
class DELPHICLASS TsgcWSSUBACKS;
class DELPHICLASS TsgcWSUNSUBACK;
class DELPHICLASS TsgcWSUNSUBACKS;
class DELPHICLASS TsgcWSMQTTFixedHeader;
class DELPHICLASS TsgcWSMQTTVariableHeader;
class DELPHICLASS TsgcWSMQTTPayLoadHeader;
class DELPHICLASS TsgcWSMQTTVariableHeaderPacketIdentifier;
class DELPHICLASS TsgcWSMQTTReadProperties;
class DELPHICLASS TsgcWSMQTTCONNACKProperties;
class DELPHICLASS TsgcWSMQTTCONNACK;
class DELPHICLASS TsgcWSMQTTSUBACKProperties;
class DELPHICLASS TsgcWSMQTTSUBACK;
class DELPHICLASS TsgcWSMQTTPUBSUBACKProperties;
class DELPHICLASS TsgcWSMQTTUNSUBACKProperties;
class DELPHICLASS TsgcWSMQTTUNSUBACK;
class DELPHICLASS TsgcWSMQTTPUBACKProperties;
class DELPHICLASS TsgcWSMQTTPUBACK;
class DELPHICLASS TsgcWSMQTTPUBRECProperties;
class DELPHICLASS TsgcWSMQTTPUBREC;
class DELPHICLASS TsgcWSMQTTPUBRELProperties;
class DELPHICLASS TsgcWSMQTTPUBREL;
class DELPHICLASS TsgcWSMQTTPUBCOMPProperties;
class DELPHICLASS TsgcWSMQTTPUBCOMP;
class DELPHICLASS TsgcWSMQTTDisconnect_Properties;
class DELPHICLASS TsgcWSMQTTDISCONNECTProperties;
class DELPHICLASS TsgcWSMQTTDISCONNECT;
class DELPHICLASS TsgcWSMQTTConnectProperties;
class DELPHICLASS TsgcWSMQTTConnect_Properties;
class DELPHICLASS TsgcWSMQTTWillProperties;
class DELPHICLASS TsgcWSMQTTWill_Properties;
class DELPHICLASS TsgcWSMQTTPublishProperties;
class DELPHICLASS TsgcWSMQTTPublish_Properties;
class DELPHICLASS TsgcWSMQTTPUBLISH;
class DELPHICLASS TsgcWSMQTTSubscribeProperties;
class DELPHICLASS TsgcWSMQTTSubscribe_Properties;
class DELPHICLASS TsgcWSMQTTUnSubscribeProperties;
class DELPHICLASS TsgcWSMQTTUnsubscribe_Properties;
class DELPHICLASS TsgcWSMQTTAuth_Properties;
class DELPHICLASS TsgcWSMQTTAUTHProperties;
class DELPHICLASS TsgcWSMQTTAUTH;
class DELPHICLASS TsgcWSMQTTReadPacketBuffer;
class DELPHICLASS TsgcWSMQTTMessage;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TwsMQTTPayloadFormat : unsigned char { mqpfUnspecified, mqpfUTF8 };

enum DECLSPEC_DENUM TwsMQTTReadState : unsigned char { mqrsFixedHeader, mqrsMessage };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSTopic : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	Sgcwebsocket_types::TmqttQoS FQoS;
	System::UnicodeString FTopic;
	
public:
	__property Sgcwebsocket_types::TmqttQoS QoS = {read=FQoS, write=FQoS, nodefault};
	__property System::UnicodeString Topic = {read=FTopic, write=FTopic};
public:
	/* TObject.Create */ inline __fastcall TsgcWSTopic() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSTopic() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSTopics : public System::Contnrs::TObjectList
{
	typedef System::Contnrs::TObjectList inherited;
	
public:
	/* TObjectList.Create */ inline __fastcall TsgcWSTopics()/* overload */ : System::Contnrs::TObjectList() { }
	/* TObjectList.Create */ inline __fastcall TsgcWSTopics(bool AOwnsObjects)/* overload */ : System::Contnrs::TObjectList(AOwnsObjects) { }
	
public:
	/* TList.Destroy */ inline __fastcall virtual ~TsgcWSTopics() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSSUBACK : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FReasonCode;
	System::UnicodeString FReasonName;
	
public:
	__property int ReasonCode = {read=FReasonCode, write=FReasonCode, nodefault};
	__property System::UnicodeString ReasonName = {read=FReasonName, write=FReasonName};
public:
	/* TObject.Create */ inline __fastcall TsgcWSSUBACK() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSSUBACK() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSSUBACKS : public System::Contnrs::TObjectList
{
	typedef System::Contnrs::TObjectList inherited;
	
private:
	HIDESBASE TsgcWSSUBACK* __fastcall GetItem(int i);
	HIDESBASE void __fastcall SetItem(int i, TsgcWSSUBACK* const Value);
	
public:
	__property TsgcWSSUBACK* Item[int i] = {read=GetItem, write=SetItem};
public:
	/* TObjectList.Create */ inline __fastcall TsgcWSSUBACKS()/* overload */ : System::Contnrs::TObjectList() { }
	/* TObjectList.Create */ inline __fastcall TsgcWSSUBACKS(bool AOwnsObjects)/* overload */ : System::Contnrs::TObjectList(AOwnsObjects) { }
	
public:
	/* TList.Destroy */ inline __fastcall virtual ~TsgcWSSUBACKS() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSUNSUBACK : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FReasonCode;
	System::UnicodeString FReasonName;
	
public:
	__property int ReasonCode = {read=FReasonCode, write=FReasonCode, nodefault};
	__property System::UnicodeString ReasonName = {read=FReasonName, write=FReasonName};
public:
	/* TObject.Create */ inline __fastcall TsgcWSUNSUBACK() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSUNSUBACK() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSUNSUBACKS : public System::Contnrs::TObjectList
{
	typedef System::Contnrs::TObjectList inherited;
	
private:
	HIDESBASE TsgcWSUNSUBACK* __fastcall GetItem(int i);
	HIDESBASE void __fastcall SetItem(int i, TsgcWSUNSUBACK* const Value);
	
public:
	__property TsgcWSUNSUBACK* Item[int i] = {read=GetItem, write=SetItem};
public:
	/* TObjectList.Create */ inline __fastcall TsgcWSUNSUBACKS()/* overload */ : System::Contnrs::TObjectList() { }
	/* TObjectList.Create */ inline __fastcall TsgcWSUNSUBACKS(bool AOwnsObjects)/* overload */ : System::Contnrs::TObjectList(AOwnsObjects) { }
	
public:
	/* TList.Destroy */ inline __fastcall virtual ~TsgcWSUNSUBACKS() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSMQTTFixedHeader : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Sgcwebsocket_types::TmqttControlPacket FControlPacket;
	bool FDuplicate;
	bool FRetain;
	Sgcwebsocket_types::TmqttQoS FQoS;
	int FRemainingLength;
	Idglobal::TIdBytes __fastcall GetBytes();
	int __fastcall GetSize();
	
public:
	__fastcall virtual TsgcWSMQTTFixedHeader(System::Classes::TComponent* aOwner);
	__property Idglobal::TIdBytes Bytes = {read=GetBytes};
	__property int Size = {read=GetSize, nodefault};
	__property Sgcwebsocket_types::TmqttControlPacket ControlPacket = {read=FControlPacket, write=FControlPacket, nodefault};
	__property bool Duplicate = {read=FDuplicate, write=FDuplicate, nodefault};
	__property bool Retain = {read=FRetain, write=FRetain, nodefault};
	__property Sgcwebsocket_types::TmqttQoS QoS = {read=FQoS, write=FQoS, nodefault};
	__property int RemainingLength = {read=FRemainingLength, write=FRemainingLength, nodefault};
public:
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcWSMQTTFixedHeader() { }
	
};


class PASCALIMPLEMENTATION TsgcWSMQTTVariableHeader : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Idglobal::TIdBytes FBytes;
	int __fastcall GetSize();
	
public:
	void __fastcall AddByte(System::Byte aByte);
	void __fastcall AddBytes(Idglobal::TIdBytes aBytes);
	void __fastcall AddInteger(int aValue);
	void __fastcall AddWord(System::Word aValue);
	void __fastcall AddString(const System::UnicodeString aText, bool aHeader = true);
	void __fastcall AddBoolean(bool aValue);
	void __fastcall AddVarInteger(int aValue);
	virtual void __fastcall Clear();
	__property Idglobal::TIdBytes Bytes = {read=FBytes};
	__property int Size = {read=GetSize, nodefault};
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcWSMQTTVariableHeader(System::Classes::TComponent* AOwner) : System::Classes::TComponent(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcWSMQTTVariableHeader() { }
	
};


class PASCALIMPLEMENTATION TsgcWSMQTTPayLoadHeader : public TsgcWSMQTTVariableHeader
{
	typedef TsgcWSMQTTVariableHeader inherited;
	
public:
	void __fastcall AddStream(System::Classes::TStream* const aStream);
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcWSMQTTPayLoadHeader(System::Classes::TComponent* AOwner) : TsgcWSMQTTVariableHeader(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcWSMQTTPayLoadHeader() { }
	
};


class PASCALIMPLEMENTATION TsgcWSMQTTVariableHeaderPacketIdentifier : public TsgcWSMQTTVariableHeader
{
	typedef TsgcWSMQTTVariableHeader inherited;
	
private:
	int FPacketIdentifier;
	
public:
	virtual void __fastcall Clear();
	__property int PacketIdentifier = {read=FPacketIdentifier, write=FPacketIdentifier, nodefault};
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcWSMQTTVariableHeaderPacketIdentifier(System::Classes::TComponent* AOwner) : TsgcWSMQTTVariableHeader(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcWSMQTTVariableHeaderPacketIdentifier() { }
	
};


class PASCALIMPLEMENTATION TsgcWSMQTTReadProperties : public TsgcWSMQTTVariableHeader
{
	typedef TsgcWSMQTTVariableHeader inherited;
	
protected:
	int __fastcall GetInteger(const Idglobal::TIdBytes aBytes, int aLength, int &aOffset);
	bool __fastcall GetBoolean(const Idglobal::TIdBytes aBytes, int &aOffset);
	System::UnicodeString __fastcall GetString(const Idglobal::TIdBytes aBytes, int &aOffset);
	virtual void __fastcall DoReadBytes(const Idglobal::TIdBytes aBytes) = 0 ;
	
public:
	void __fastcall ReadBytes(const Idglobal::TIdBytes aBytes);
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcWSMQTTReadProperties(System::Classes::TComponent* AOwner) : TsgcWSMQTTVariableHeader(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcWSMQTTReadProperties() { }
	
};


class PASCALIMPLEMENTATION TsgcWSMQTTCONNACKProperties : public TsgcWSMQTTReadProperties
{
	typedef TsgcWSMQTTReadProperties inherited;
	
private:
	System::UnicodeString FAssignedClientIdentifier;
	System::UnicodeString FAuthenticationData;
	System::UnicodeString FAuthenticationMethod;
	int FMaximumPacketSize;
	Sgcwebsocket_types::TmqttQoS FMaximumQoS;
	System::UnicodeString FReasonString;
	int FReceiveMaximum;
	System::UnicodeString FResponseInformation;
	bool FRetainAvailable;
	int FServerKeepAlive;
	System::UnicodeString FServerReference;
	int FSessionExpiryInterval;
	bool FSharedSubscriptionAvailable;
	bool FSubscriptionIdentifiersAvailable;
	int FTopicAliasMaximum;
	System::Classes::TStringList* FUserProperties;
	bool FWildcardSubscriptionAvailable;
	System::Classes::TStringList* __fastcall GetUserProperties();
	void __fastcall SetSubscriptionIdentifiersAvailable(const bool Value);
	
protected:
	virtual void __fastcall DoReadBytes(const Idglobal::TIdBytes aBytes);
	
public:
	__fastcall virtual TsgcWSMQTTCONNACKProperties(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSMQTTCONNACKProperties();
	virtual void __fastcall Clear();
	__property System::UnicodeString AssignedClientIdentifier = {read=FAssignedClientIdentifier, write=FAssignedClientIdentifier};
	__property System::UnicodeString AuthenticationData = {read=FAuthenticationData, write=FAuthenticationData};
	__property System::UnicodeString AuthenticationMethod = {read=FAuthenticationMethod, write=FAuthenticationMethod};
	__property int MaximumPacketSize = {read=FMaximumPacketSize, write=FMaximumPacketSize, nodefault};
	__property Sgcwebsocket_types::TmqttQoS MaximumQoS = {read=FMaximumQoS, write=FMaximumQoS, nodefault};
	__property System::UnicodeString ReasonString = {read=FReasonString, write=FReasonString};
	__property int ReceiveMaximum = {read=FReceiveMaximum, write=FReceiveMaximum, nodefault};
	__property System::UnicodeString ResponseInformation = {read=FResponseInformation, write=FResponseInformation};
	__property bool RetainAvailable = {read=FRetainAvailable, write=FRetainAvailable, nodefault};
	__property int ServerKeepAlive = {read=FServerKeepAlive, write=FServerKeepAlive, nodefault};
	__property System::UnicodeString ServerReference = {read=FServerReference, write=FServerReference};
	__property int SessionExpiryInterval = {read=FSessionExpiryInterval, write=FSessionExpiryInterval, nodefault};
	__property bool SharedSubscriptionAvailable = {read=FSharedSubscriptionAvailable, write=FSharedSubscriptionAvailable, nodefault};
	__property bool SubscriptionIdentifiersAvailable = {read=FSubscriptionIdentifiersAvailable, write=SetSubscriptionIdentifiersAvailable, nodefault};
	__property int TopicAliasMaximum = {read=FTopicAliasMaximum, write=FTopicAliasMaximum, nodefault};
	__property System::Classes::TStringList* UserProperties = {read=GetUserProperties, write=FUserProperties};
	__property bool WildcardSubscriptionAvailable = {read=FWildcardSubscriptionAvailable, write=FWildcardSubscriptionAvailable, nodefault};
};


class PASCALIMPLEMENTATION TsgcWSMQTTCONNACK : public TsgcWSMQTTVariableHeader
{
	typedef TsgcWSMQTTVariableHeader inherited;
	
private:
	TsgcWSMQTTCONNACKProperties* FCONNACKProperties;
	int FReasonCode;
	System::UnicodeString FReasonName;
	bool FSession;
	TsgcWSMQTTCONNACKProperties* __fastcall GetCONNACKProperties();
	
public:
	__fastcall virtual ~TsgcWSMQTTCONNACK();
	virtual void __fastcall Clear();
	__property TsgcWSMQTTCONNACKProperties* CONNACKProperties = {read=GetCONNACKProperties, write=FCONNACKProperties};
	__property int ReasonCode = {read=FReasonCode, write=FReasonCode, nodefault};
	__property System::UnicodeString ReasonName = {read=FReasonName, write=FReasonName};
	__property bool Session = {read=FSession, write=FSession, nodefault};
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcWSMQTTCONNACK(System::Classes::TComponent* AOwner) : TsgcWSMQTTVariableHeader(AOwner) { }
	
};


class PASCALIMPLEMENTATION TsgcWSMQTTSUBACKProperties : public TsgcWSMQTTReadProperties
{
	typedef TsgcWSMQTTReadProperties inherited;
	
private:
	System::UnicodeString FReasonString;
	System::Classes::TStringList* FUserProperties;
	void __fastcall SetUserProperties(System::Classes::TStringList* const Value);
	
protected:
	virtual void __fastcall DoReadBytes(const Idglobal::TIdBytes aBytes);
	
public:
	__fastcall virtual TsgcWSMQTTSUBACKProperties(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSMQTTSUBACKProperties();
	virtual void __fastcall Clear();
	__property System::UnicodeString ReasonString = {read=FReasonString, write=FReasonString};
	__property System::Classes::TStringList* UserProperties = {read=FUserProperties, write=SetUserProperties};
};


class PASCALIMPLEMENTATION TsgcWSMQTTSUBACK : public TsgcWSMQTTVariableHeaderPacketIdentifier
{
	typedef TsgcWSMQTTVariableHeaderPacketIdentifier inherited;
	
private:
	TsgcWSSUBACKS* FCodes;
	TsgcWSMQTTSUBACKProperties* FSUBACKProperties;
	TsgcWSSUBACKS* __fastcall GetCodes();
	TsgcWSMQTTSUBACKProperties* __fastcall GetSUBACKProperties();
	
public:
	virtual void __fastcall Clear();
	__fastcall virtual ~TsgcWSMQTTSUBACK();
	__property TsgcWSSUBACKS* Codes = {read=GetCodes, write=FCodes};
	__property TsgcWSMQTTSUBACKProperties* SUBACKProperties = {read=GetSUBACKProperties, write=FSUBACKProperties};
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcWSMQTTSUBACK(System::Classes::TComponent* AOwner) : TsgcWSMQTTVariableHeaderPacketIdentifier(AOwner) { }
	
};


class PASCALIMPLEMENTATION TsgcWSMQTTPUBSUBACKProperties : public TsgcWSMQTTReadProperties
{
	typedef TsgcWSMQTTReadProperties inherited;
	
private:
	System::UnicodeString FReasonString;
	System::Classes::TStringList* FUserProperties;
	void __fastcall SetUserProperties(System::Classes::TStringList* const Value);
	
protected:
	virtual void __fastcall DoReadBytes(const Idglobal::TIdBytes aBytes);
	
public:
	__fastcall virtual TsgcWSMQTTPUBSUBACKProperties(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSMQTTPUBSUBACKProperties();
	virtual void __fastcall Clear();
	__property System::UnicodeString ReasonString = {read=FReasonString, write=FReasonString};
	__property System::Classes::TStringList* UserProperties = {read=FUserProperties, write=SetUserProperties};
};


class PASCALIMPLEMENTATION TsgcWSMQTTUNSUBACKProperties : public TsgcWSMQTTPUBSUBACKProperties
{
	typedef TsgcWSMQTTPUBSUBACKProperties inherited;
	
public:
	/* TsgcWSMQTTPUBSUBACKProperties.Create */ inline __fastcall virtual TsgcWSMQTTUNSUBACKProperties(System::Classes::TComponent* aOwner) : TsgcWSMQTTPUBSUBACKProperties(aOwner) { }
	/* TsgcWSMQTTPUBSUBACKProperties.Destroy */ inline __fastcall virtual ~TsgcWSMQTTUNSUBACKProperties() { }
	
};


class PASCALIMPLEMENTATION TsgcWSMQTTUNSUBACK : public TsgcWSMQTTVariableHeaderPacketIdentifier
{
	typedef TsgcWSMQTTVariableHeaderPacketIdentifier inherited;
	
private:
	TsgcWSUNSUBACKS* FCodes;
	TsgcWSMQTTUNSUBACKProperties* FUNSUBACKProperties;
	TsgcWSUNSUBACKS* __fastcall GetCodes();
	TsgcWSMQTTUNSUBACKProperties* __fastcall GetUNSUBACKProperties();
	
public:
	virtual void __fastcall Clear();
	__property TsgcWSUNSUBACKS* Codes = {read=GetCodes, write=FCodes};
	__property TsgcWSMQTTUNSUBACKProperties* UNSUBACKProperties = {read=GetUNSUBACKProperties, write=FUNSUBACKProperties};
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcWSMQTTUNSUBACK(System::Classes::TComponent* AOwner) : TsgcWSMQTTVariableHeaderPacketIdentifier(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcWSMQTTUNSUBACK() { }
	
};


class PASCALIMPLEMENTATION TsgcWSMQTTPUBACKProperties : public TsgcWSMQTTPUBSUBACKProperties
{
	typedef TsgcWSMQTTPUBSUBACKProperties inherited;
	
public:
	/* TsgcWSMQTTPUBSUBACKProperties.Create */ inline __fastcall virtual TsgcWSMQTTPUBACKProperties(System::Classes::TComponent* aOwner) : TsgcWSMQTTPUBSUBACKProperties(aOwner) { }
	/* TsgcWSMQTTPUBSUBACKProperties.Destroy */ inline __fastcall virtual ~TsgcWSMQTTPUBACKProperties() { }
	
};


class PASCALIMPLEMENTATION TsgcWSMQTTPUBACK : public TsgcWSMQTTVariableHeaderPacketIdentifier
{
	typedef TsgcWSMQTTVariableHeaderPacketIdentifier inherited;
	
private:
	TsgcWSMQTTPUBACKProperties* FPUBACKProperties;
	int FReasonCode;
	System::UnicodeString FReasonName;
	TsgcWSMQTTPUBACKProperties* __fastcall GetPUBACKProperties();
	
public:
	__fastcall virtual TsgcWSMQTTPUBACK(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSMQTTPUBACK();
	virtual void __fastcall Clear();
	__property TsgcWSMQTTPUBACKProperties* PUBACKProperties = {read=GetPUBACKProperties, write=FPUBACKProperties};
	__property int ReasonCode = {read=FReasonCode, write=FReasonCode, nodefault};
	__property System::UnicodeString ReasonName = {read=FReasonName, write=FReasonName};
};


class PASCALIMPLEMENTATION TsgcWSMQTTPUBRECProperties : public TsgcWSMQTTPUBSUBACKProperties
{
	typedef TsgcWSMQTTPUBSUBACKProperties inherited;
	
public:
	/* TsgcWSMQTTPUBSUBACKProperties.Create */ inline __fastcall virtual TsgcWSMQTTPUBRECProperties(System::Classes::TComponent* aOwner) : TsgcWSMQTTPUBSUBACKProperties(aOwner) { }
	/* TsgcWSMQTTPUBSUBACKProperties.Destroy */ inline __fastcall virtual ~TsgcWSMQTTPUBRECProperties() { }
	
};


class PASCALIMPLEMENTATION TsgcWSMQTTPUBREC : public TsgcWSMQTTVariableHeaderPacketIdentifier
{
	typedef TsgcWSMQTTVariableHeaderPacketIdentifier inherited;
	
private:
	TsgcWSMQTTPUBRECProperties* FPUBRECProperties;
	int FReasonCode;
	System::UnicodeString FReasonName;
	TsgcWSMQTTPUBRECProperties* __fastcall GetPUBRECProperties();
	
public:
	__fastcall virtual TsgcWSMQTTPUBREC(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSMQTTPUBREC();
	virtual void __fastcall Clear();
	__property TsgcWSMQTTPUBRECProperties* PUBRECProperties = {read=GetPUBRECProperties, write=FPUBRECProperties};
	__property int ReasonCode = {read=FReasonCode, write=FReasonCode, nodefault};
	__property System::UnicodeString ReasonName = {read=FReasonName, write=FReasonName};
};


class PASCALIMPLEMENTATION TsgcWSMQTTPUBRELProperties : public TsgcWSMQTTPUBSUBACKProperties
{
	typedef TsgcWSMQTTPUBSUBACKProperties inherited;
	
public:
	/* TsgcWSMQTTPUBSUBACKProperties.Create */ inline __fastcall virtual TsgcWSMQTTPUBRELProperties(System::Classes::TComponent* aOwner) : TsgcWSMQTTPUBSUBACKProperties(aOwner) { }
	/* TsgcWSMQTTPUBSUBACKProperties.Destroy */ inline __fastcall virtual ~TsgcWSMQTTPUBRELProperties() { }
	
};


class PASCALIMPLEMENTATION TsgcWSMQTTPUBREL : public TsgcWSMQTTVariableHeaderPacketIdentifier
{
	typedef TsgcWSMQTTVariableHeaderPacketIdentifier inherited;
	
private:
	TsgcWSMQTTPUBRELProperties* FPUBRELProperties;
	int FReasonCode;
	System::UnicodeString FReasonName;
	TsgcWSMQTTPUBRELProperties* __fastcall GetPUBRELProperties();
	
public:
	__fastcall virtual TsgcWSMQTTPUBREL(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSMQTTPUBREL();
	virtual void __fastcall Clear();
	__property TsgcWSMQTTPUBRELProperties* PUBRELProperties = {read=GetPUBRELProperties, write=FPUBRELProperties};
	__property int ReasonCode = {read=FReasonCode, write=FReasonCode, nodefault};
	__property System::UnicodeString ReasonName = {read=FReasonName, write=FReasonName};
};


class PASCALIMPLEMENTATION TsgcWSMQTTPUBCOMPProperties : public TsgcWSMQTTPUBSUBACKProperties
{
	typedef TsgcWSMQTTPUBSUBACKProperties inherited;
	
public:
	/* TsgcWSMQTTPUBSUBACKProperties.Create */ inline __fastcall virtual TsgcWSMQTTPUBCOMPProperties(System::Classes::TComponent* aOwner) : TsgcWSMQTTPUBSUBACKProperties(aOwner) { }
	/* TsgcWSMQTTPUBSUBACKProperties.Destroy */ inline __fastcall virtual ~TsgcWSMQTTPUBCOMPProperties() { }
	
};


class PASCALIMPLEMENTATION TsgcWSMQTTPUBCOMP : public TsgcWSMQTTVariableHeaderPacketIdentifier
{
	typedef TsgcWSMQTTVariableHeaderPacketIdentifier inherited;
	
private:
	TsgcWSMQTTPUBCOMPProperties* FPUBCOMPProperties;
	int FReasonCode;
	System::UnicodeString FReasonName;
	TsgcWSMQTTPUBCOMPProperties* __fastcall GetPUBCOMPProperties();
	
public:
	__fastcall virtual TsgcWSMQTTPUBCOMP(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSMQTTPUBCOMP();
	virtual void __fastcall Clear();
	__property TsgcWSMQTTPUBCOMPProperties* PUBCOMPProperties = {read=GetPUBCOMPProperties, write=FPUBCOMPProperties};
	__property int ReasonCode = {read=FReasonCode, write=FReasonCode, nodefault};
	__property System::UnicodeString ReasonName = {read=FReasonName, write=FReasonName};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSMQTTDisconnect_Properties : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	System::UnicodeString FReasonString;
	System::UnicodeString FServerReference;
	int FSessionExpiryInterval;
	System::Classes::TStringList* FUserProperties;
	int __fastcall GetSessionExpiryInterval();
	void __fastcall SetUserProperties(System::Classes::TStringList* const Value);
	
public:
	__fastcall virtual TsgcWSMQTTDisconnect_Properties();
	__fastcall virtual ~TsgcWSMQTTDisconnect_Properties();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property System::UnicodeString ReasonString = {read=FReasonString, write=FReasonString};
	__property System::UnicodeString ServerReference = {read=FServerReference, write=FServerReference};
	__property int SessionExpiryInterval = {read=GetSessionExpiryInterval, write=FSessionExpiryInterval, nodefault};
	__property System::Classes::TStringList* UserProperties = {read=FUserProperties, write=SetUserProperties};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSMQTTDISCONNECTProperties : public TsgcWSMQTTReadProperties
{
	typedef TsgcWSMQTTReadProperties inherited;
	
private:
	System::UnicodeString FReasonString;
	System::UnicodeString FServerReference;
	int FSessionExpiryInterval;
	System::Classes::TStringList* FUserProperties;
	int __fastcall GetSessionExpiryInterval();
	System::Classes::TStringList* __fastcall GetUserProperties();
	
protected:
	virtual void __fastcall DoReadBytes(const Idglobal::TIdBytes aBytes);
	
public:
	__fastcall virtual TsgcWSMQTTDISCONNECTProperties(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSMQTTDISCONNECTProperties();
	virtual void __fastcall Clear();
	__property System::UnicodeString ReasonString = {read=FReasonString, write=FReasonString};
	__property System::UnicodeString ServerReference = {read=FServerReference, write=FServerReference};
	__property int SessionExpiryInterval = {read=GetSessionExpiryInterval, write=FSessionExpiryInterval, nodefault};
	__property System::Classes::TStringList* UserProperties = {read=GetUserProperties, write=FUserProperties};
};


class PASCALIMPLEMENTATION TsgcWSMQTTDISCONNECT : public TsgcWSMQTTVariableHeaderPacketIdentifier
{
	typedef TsgcWSMQTTVariableHeaderPacketIdentifier inherited;
	
private:
	TsgcWSMQTTDISCONNECTProperties* FDISCONNECTProperties;
	int FReasonCode;
	System::UnicodeString FReasonName;
	TsgcWSMQTTDISCONNECTProperties* __fastcall GetDISCONNECTProperties();
	
public:
	__fastcall virtual TsgcWSMQTTDISCONNECT(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSMQTTDISCONNECT();
	virtual void __fastcall Clear();
	__property TsgcWSMQTTDISCONNECTProperties* DISCONNECTProperties = {read=GetDISCONNECTProperties, write=FDISCONNECTProperties};
	__property int ReasonCode = {read=FReasonCode, write=FReasonCode, nodefault};
	__property System::UnicodeString ReasonName = {read=FReasonName, write=FReasonName};
};


class PASCALIMPLEMENTATION TsgcWSMQTTConnectProperties : public TsgcWSMQTTVariableHeader
{
	typedef TsgcWSMQTTVariableHeader inherited;
	
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcWSMQTTConnectProperties(System::Classes::TComponent* AOwner) : TsgcWSMQTTVariableHeader(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcWSMQTTConnectProperties() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSMQTTConnect_Properties : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FAuthenticationMethod;
	bool FEnabled;
	int FMaximumPacketSize;
	int FReceiveMaximum;
	bool FRequestProblemInformation;
	bool FRequestResponseInformation;
	int FSessionExpiryInterval;
	int FTopicAliasMaximum;
	System::Classes::TStringList* FUserProperties;
	int __fastcall GetMaximumPacketSize();
	int __fastcall GetReceiveMaximum();
	int __fastcall GetSessionExpiryInterval();
	int __fastcall GetTopicAliasMaximum();
	void __fastcall SetUserProperties(System::Classes::TStringList* const Value);
	
public:
	__fastcall virtual TsgcWSMQTTConnect_Properties();
	__fastcall virtual ~TsgcWSMQTTConnect_Properties();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString AuthenticationMethod = {read=FAuthenticationMethod, write=FAuthenticationMethod};
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property int MaximumPacketSize = {read=GetMaximumPacketSize, write=FMaximumPacketSize, nodefault};
	__property int ReceiveMaximum = {read=GetReceiveMaximum, write=FReceiveMaximum, nodefault};
	__property bool RequestProblemInformation = {read=FRequestProblemInformation, write=FRequestProblemInformation, nodefault};
	__property bool RequestResponseInformation = {read=FRequestResponseInformation, write=FRequestResponseInformation, nodefault};
	__property int SessionExpiryInterval = {read=GetSessionExpiryInterval, write=FSessionExpiryInterval, nodefault};
	__property int TopicAliasMaximum = {read=GetTopicAliasMaximum, write=FTopicAliasMaximum, nodefault};
	__property System::Classes::TStringList* UserProperties = {read=FUserProperties, write=SetUserProperties};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSMQTTWillProperties : public TsgcWSMQTTVariableHeader
{
	typedef TsgcWSMQTTVariableHeader inherited;
	
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcWSMQTTWillProperties(System::Classes::TComponent* AOwner) : TsgcWSMQTTVariableHeader(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcWSMQTTWillProperties() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSMQTTWill_Properties : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FContentType;
	System::UnicodeString FCorrelationData;
	bool FEnabled;
	int FMessageExpiryInterval;
	TwsMQTTPayloadFormat FPayloadFormat;
	System::UnicodeString FResponseTopic;
	System::Classes::TStringList* FUserProperties;
	int FWillDelayInterval;
	int __fastcall GetMessageExpiryInterval();
	int __fastcall GetWillDelayInterval();
	void __fastcall SetUserProperties(System::Classes::TStringList* const Value);
	
public:
	__fastcall virtual TsgcWSMQTTWill_Properties();
	__fastcall virtual ~TsgcWSMQTTWill_Properties();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	
__published:
	__property System::UnicodeString ContentType = {read=FContentType, write=FContentType};
	__property System::UnicodeString CorrelationData = {read=FCorrelationData, write=FCorrelationData};
	__property int MessageExpiryInterval = {read=GetMessageExpiryInterval, write=FMessageExpiryInterval, nodefault};
	__property TwsMQTTPayloadFormat PayloadFormat = {read=FPayloadFormat, write=FPayloadFormat, nodefault};
	__property System::UnicodeString ResponseTopic = {read=FResponseTopic, write=FResponseTopic};
	__property int WillDelayInterval = {read=GetWillDelayInterval, write=FWillDelayInterval, nodefault};
	__property System::Classes::TStringList* UserProperties = {read=FUserProperties, write=SetUserProperties};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSMQTTPublishProperties : public TsgcWSMQTTReadProperties
{
	typedef TsgcWSMQTTReadProperties inherited;
	
private:
	System::UnicodeString FContentType;
	System::UnicodeString FCorrelationData;
	bool FEnabled;
	int FMessageExpiryInterval;
	TwsMQTTPayloadFormat FPayloadFormat;
	System::UnicodeString FResponseTopic;
	int FSubscriptionIdentifier;
	int FTopicAlias;
	System::Classes::TStringList* FUserProperties;
	System::Classes::TStringList* __fastcall GetUserProperties();
	
protected:
	virtual void __fastcall DoReadBytes(const Idglobal::TIdBytes aBytes);
	
public:
	__fastcall virtual TsgcWSMQTTPublishProperties(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSMQTTPublishProperties();
	virtual void __fastcall Clear();
	
__published:
	__property System::UnicodeString ContentType = {read=FContentType, write=FContentType};
	__property System::UnicodeString CorrelationData = {read=FCorrelationData, write=FCorrelationData};
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property int MessageExpiryInterval = {read=FMessageExpiryInterval, write=FMessageExpiryInterval, nodefault};
	__property TwsMQTTPayloadFormat PayloadFormat = {read=FPayloadFormat, write=FPayloadFormat, nodefault};
	__property System::UnicodeString ResponseTopic = {read=FResponseTopic, write=FResponseTopic};
	__property int SubscriptionIdentifier = {read=FSubscriptionIdentifier, write=FSubscriptionIdentifier, nodefault};
	__property int TopicAlias = {read=FTopicAlias, write=FTopicAlias, nodefault};
	__property System::Classes::TStringList* UserProperties = {read=GetUserProperties, write=FUserProperties};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSMQTTPublish_Properties : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FContentType;
	System::UnicodeString FCorrelationData;
	bool FEnabled;
	int FMessageExpiryInterval;
	TwsMQTTPayloadFormat FPayloadFormat;
	System::UnicodeString FResponseTopic;
	int FSubscriptionIdentifier;
	System::Classes::TStringList* FUserProperties;
	int FTopicAlias;
	int __fastcall GetMessageExpiryInterval();
	int __fastcall GetSubscriptionIdentifier();
	int __fastcall GetTopicAlias();
	void __fastcall SetUserProperties(System::Classes::TStringList* const Value);
	
public:
	__fastcall virtual TsgcWSMQTTPublish_Properties();
	__fastcall virtual ~TsgcWSMQTTPublish_Properties();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString ContentType = {read=FContentType, write=FContentType};
	__property System::UnicodeString CorrelationData = {read=FCorrelationData, write=FCorrelationData};
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property int MessageExpiryInterval = {read=GetMessageExpiryInterval, write=FMessageExpiryInterval, nodefault};
	__property TwsMQTTPayloadFormat PayloadFormat = {read=FPayloadFormat, write=FPayloadFormat, nodefault};
	__property System::UnicodeString ResponseTopic = {read=FResponseTopic, write=FResponseTopic};
	__property int SubscriptionIdentifier = {read=GetSubscriptionIdentifier, write=FSubscriptionIdentifier, nodefault};
	__property int TopicAlias = {read=GetTopicAlias, write=FTopicAlias, nodefault};
	__property System::Classes::TStringList* UserProperties = {read=FUserProperties, write=SetUserProperties};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSMQTTPUBLISH : public TsgcWSMQTTVariableHeader
{
	typedef TsgcWSMQTTVariableHeader inherited;
	
private:
	int FPacketIdentifier;
	TsgcWSMQTTPublishProperties* FPUBLISHProperties;
	System::UnicodeString FText;
	System::UnicodeString FTopic;
	TsgcWSMQTTPublishProperties* __fastcall GetPUBLISHProperties();
	
public:
	virtual void __fastcall Clear();
	__property int PacketIdentifier = {read=FPacketIdentifier, write=FPacketIdentifier, nodefault};
	__property TsgcWSMQTTPublishProperties* PUBLISHProperties = {read=GetPUBLISHProperties, write=FPUBLISHProperties};
	__property System::UnicodeString Text = {read=FText, write=FText};
	__property System::UnicodeString Topic = {read=FTopic, write=FTopic};
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcWSMQTTPUBLISH(System::Classes::TComponent* AOwner) : TsgcWSMQTTVariableHeader(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcWSMQTTPUBLISH() { }
	
};


class PASCALIMPLEMENTATION TsgcWSMQTTSubscribeProperties : public TsgcWSMQTTVariableHeader
{
	typedef TsgcWSMQTTVariableHeader inherited;
	
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcWSMQTTSubscribeProperties(System::Classes::TComponent* AOwner) : TsgcWSMQTTVariableHeader(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcWSMQTTSubscribeProperties() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSMQTTSubscribe_Properties : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	int FSubscriptionIdentifier;
	System::Classes::TStringList* FUserProperties;
	int __fastcall GetSubscriptionIdentifier();
	void __fastcall SetUserProperties(System::Classes::TStringList* const Value);
	
public:
	__fastcall virtual TsgcWSMQTTSubscribe_Properties();
	__fastcall virtual ~TsgcWSMQTTSubscribe_Properties();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property int SubscriptionIdentifier = {read=GetSubscriptionIdentifier, write=FSubscriptionIdentifier, nodefault};
	__property System::Classes::TStringList* UserProperties = {read=FUserProperties, write=SetUserProperties};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSMQTTUnSubscribeProperties : public TsgcWSMQTTVariableHeader
{
	typedef TsgcWSMQTTVariableHeader inherited;
	
public:
	/* TComponent.Create */ inline __fastcall virtual TsgcWSMQTTUnSubscribeProperties(System::Classes::TComponent* AOwner) : TsgcWSMQTTVariableHeader(AOwner) { }
	/* TComponent.Destroy */ inline __fastcall virtual ~TsgcWSMQTTUnSubscribeProperties() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSMQTTUnsubscribe_Properties : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	System::Classes::TStringList* FUserProperties;
	void __fastcall SetUserProperties(System::Classes::TStringList* const Value);
	
public:
	__fastcall virtual TsgcWSMQTTUnsubscribe_Properties();
	__fastcall virtual ~TsgcWSMQTTUnsubscribe_Properties();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property System::Classes::TStringList* UserProperties = {read=FUserProperties, write=SetUserProperties};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSMQTTAuth_Properties : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	System::UnicodeString FReasonString;
	System::UnicodeString FAuthenticationMethod;
	System::UnicodeString FAuthenticationData;
	System::Classes::TStringList* FUserProperties;
	void __fastcall SetUserProperties(System::Classes::TStringList* const Value);
	
public:
	__fastcall virtual TsgcWSMQTTAuth_Properties();
	__fastcall virtual ~TsgcWSMQTTAuth_Properties();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property System::UnicodeString ReasonString = {read=FReasonString, write=FReasonString};
	__property System::UnicodeString AuthenticationMethod = {read=FAuthenticationMethod, write=FAuthenticationMethod};
	__property System::UnicodeString AuthenticationData = {read=FAuthenticationData, write=FAuthenticationData};
	__property System::Classes::TStringList* UserProperties = {read=FUserProperties, write=SetUserProperties};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSMQTTAUTHProperties : public TsgcWSMQTTReadProperties
{
	typedef TsgcWSMQTTReadProperties inherited;
	
private:
	System::UnicodeString FReasonString;
	System::UnicodeString FAuthenticationMethod;
	System::UnicodeString FAuthenticationData;
	System::Classes::TStringList* FUserProperties;
	System::Classes::TStringList* __fastcall GetUserProperties();
	
protected:
	virtual void __fastcall DoReadBytes(const Idglobal::TIdBytes aBytes);
	
public:
	__fastcall virtual TsgcWSMQTTAUTHProperties(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSMQTTAUTHProperties();
	virtual void __fastcall Clear();
	__property System::UnicodeString ReasonString = {read=FReasonString, write=FReasonString};
	__property System::UnicodeString AuthenticationMethod = {read=FAuthenticationMethod, write=FAuthenticationMethod};
	__property System::UnicodeString AuthenticationData = {read=FAuthenticationData, write=FAuthenticationData};
	__property System::Classes::TStringList* UserProperties = {read=GetUserProperties, write=FUserProperties};
};


class PASCALIMPLEMENTATION TsgcWSMQTTAUTH : public TsgcWSMQTTVariableHeaderPacketIdentifier
{
	typedef TsgcWSMQTTVariableHeaderPacketIdentifier inherited;
	
private:
	TsgcWSMQTTAUTHProperties* FAUTHProperties;
	int FReasonCode;
	System::UnicodeString FReasonName;
	TsgcWSMQTTAUTHProperties* __fastcall GetAUTHProperties();
	
public:
	__fastcall virtual TsgcWSMQTTAUTH(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSMQTTAUTH();
	__property TsgcWSMQTTAUTHProperties* AUTHProperties = {read=GetAUTHProperties, write=FAUTHProperties};
	__property int ReasonCode = {read=FReasonCode, write=FReasonCode, nodefault};
	__property System::UnicodeString ReasonName = {read=FReasonName, write=FReasonName};
};


class PASCALIMPLEMENTATION TsgcWSMQTTReadPacketBuffer : public System::Classes::TComponent
{
	typedef System::Classes::TComponent inherited;
	
private:
	Sgcwebsocket_types::TmqttControlPacket FControlPacket;
	int FFixedHeaderLength;
	System::Classes::TMemoryStream* FStreamBuffer;
	TwsMQTTReadState FReadPacketState;
	int FRemainingLength;
	System::Classes::TMemoryStream* __fastcall GetStreamBuffer();
	void __fastcall SetReadPacketState(const TwsMQTTReadState Value);
	
public:
	__fastcall virtual TsgcWSMQTTReadPacketBuffer(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSMQTTReadPacketBuffer();
	bool __fastcall Read(System::Classes::TStream* const aStream);
	__property Sgcwebsocket_types::TmqttControlPacket ControlPacket = {read=FControlPacket, write=FControlPacket, nodefault};
	__property int FixedHeaderLength = {read=FFixedHeaderLength, write=FFixedHeaderLength, nodefault};
	__property System::Classes::TMemoryStream* StreamBuffer = {read=GetStreamBuffer, write=FStreamBuffer};
	__property TwsMQTTReadState ReadPacketState = {read=FReadPacketState, write=SetReadPacketState, nodefault};
	__property int RemainingLength = {read=FRemainingLength, write=FRemainingLength, nodefault};
};


class PASCALIMPLEMENTATION TsgcWSMQTTMessage : public Sgcbase_classes::TsgcThreadSafeComponent
{
	typedef Sgcbase_classes::TsgcThreadSafeComponent inherited;
	
private:
	System::UnicodeString FConnectionId;
	System::Classes::TStream* FStreamRead;
	unsigned FThreadId;
	TsgcWSMQTTReadPacketBuffer* FStreamReadPacketBuffer;
	TsgcWSMQTTReadPacketBuffer* __fastcall GetStreamReadPacketBuffer();
	
protected:
	__property TsgcWSMQTTReadPacketBuffer* StreamReadPacketBuffer = {read=GetStreamReadPacketBuffer, write=FStreamReadPacketBuffer};
	
public:
	__fastcall virtual TsgcWSMQTTMessage(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSMQTTMessage();
	void __fastcall Clear();
	void __fastcall ClearStreamReadTCP();
	__property System::Classes::TStream* StreamRead = {read=FStreamRead, write=FStreamRead};
	
private:
	System::Byte __fastcall GetByte(bool aBit7, bool aBit6, bool aBit5, bool aBit4, bool aBit3, bool aBit2, bool aBit1, bool aBit0);
	Idglobal::TIdBytes __fastcall GetRemainingLength();
	System::Byte __fastcall GetConnectFlags(bool aAuthentication, System::UnicodeString aUserName, System::UnicodeString aPassword, bool aWillFlag, Sgcwebsocket_types::TmqttQoS aQoS, bool aRetain);
	Idglobal::TIdBytes __fastcall GetMSBLSB(int aValue);
	void __fastcall DoConnectProperties();
	void __fastcall DoWillProperties();
	void __fastcall DoPublishProperties();
	void __fastcall DoSubscribeProperties();
	void __fastcall DoUnsubscribeProperties();
	void __fastcall DoDisconnectProperties();
	void __fastcall DoAuthProperties();
	
protected:
	System::DynamicArray<System::Byte> __fastcall WriteBytes();
	
private:
	void __fastcall DoReadFixedHeader();
	
public:
	virtual void __fastcall Read(System::Classes::TStream* const aStream);
	virtual bool __fastcall ReadTCP(System::Classes::TStream* const aStream);
	virtual System::DynamicArray<System::Byte> __fastcall DoConnect(bool aAuthentication, System::UnicodeString aAuthUser, System::UnicodeString aAuthPass, bool aWillFlag, System::UnicodeString aWillTopic, System::UnicodeString aWillText, Sgcwebsocket_types::TmqttQoS aWillQoS, bool aWillRetain, bool aKeepAliveEnabled, int aKeepAliveInterval);
	virtual System::DynamicArray<System::Byte> __fastcall DoPing();
	virtual System::DynamicArray<System::Byte> __fastcall DoPublish(System::Word aPacketIdentifier, const System::UnicodeString aTopic, const System::UnicodeString aText, Sgcwebsocket_types::TmqttQoS aQoS, bool aRetain, bool aDuplicate)/* overload */;
	virtual System::DynamicArray<System::Byte> __fastcall DoPublish(System::Word aPacketIdentifier, const System::UnicodeString aTopic, System::Classes::TStream* aStream, Sgcwebsocket_types::TmqttQoS aQoS, bool aRetain, bool aDuplicate)/* overload */;
	virtual System::DynamicArray<System::Byte> __fastcall DoSubscribe(System::Word aPacketIdentifier, TsgcWSTopics* aTopics);
	virtual System::DynamicArray<System::Byte> __fastcall DoUnSubscribe(System::Word aPacketIdentifier, TsgcWSTopics* aTopics);
	virtual System::DynamicArray<System::Byte> __fastcall DoPubAck(System::Word aPacketIdentifier);
	virtual System::DynamicArray<System::Byte> __fastcall DoPubRec(System::Word aPacketIdentifier);
	virtual System::DynamicArray<System::Byte> __fastcall DoPubRel(System::Word aPacketIdentifier);
	virtual System::DynamicArray<System::Byte> __fastcall DoPubComp(System::Word aPacketIdentifier);
	virtual System::DynamicArray<System::Byte> __fastcall DoDisconnect(int aReasonCode = 0x0);
	virtual System::DynamicArray<System::Byte> __fastcall DoAuth(bool aReAuthenticate);
	__property System::UnicodeString ConnectionId = {read=FConnectionId, write=FConnectionId};
	__property unsigned ThreadId = {read=FThreadId, write=FThreadId, nodefault};
	
private:
	TsgcWSMQTTFixedHeader* FFixedHeader;
	TsgcWSMQTTFixedHeader* __fastcall GetFixedHeader();
	
public:
	__property TsgcWSMQTTFixedHeader* FixedHeader = {read=GetFixedHeader, write=FFixedHeader};
	
private:
	TsgcWSMQTTVariableHeader* FVariableHeader;
	TsgcWSMQTTVariableHeader* __fastcall GetVariableHeader();
	
public:
	__property TsgcWSMQTTVariableHeader* VariableHeader = {read=GetVariableHeader, write=FVariableHeader};
	
private:
	TsgcWSMQTTPayLoadHeader* FPayLoadHeader;
	TsgcWSMQTTPayLoadHeader* __fastcall GetPayLoadHeader();
	
public:
	__property TsgcWSMQTTPayLoadHeader* PayLoadHeader = {read=GetPayLoadHeader, write=FPayLoadHeader};
	
private:
	TsgcWSMQTTCONNACK* FCONNACK;
	TsgcWSMQTTPUBLISH* FPUBLISH;
	TsgcWSMQTTPUBACK* FPUBACK;
	TsgcWSMQTTPUBREC* FPUBREC;
	TsgcWSMQTTPUBREL* FPUBREL;
	TsgcWSMQTTPUBCOMP* FPUBCOMP;
	TsgcWSMQTTSUBACK* FSUBACK;
	TsgcWSMQTTUNSUBACK* FUNSUBACK;
	TsgcWSMQTTDISCONNECT* FDISCONNECT;
	TsgcWSMQTTAUTH* FAUTH;
	TsgcWSMQTTCONNACK* __fastcall GetCONNACK();
	TsgcWSMQTTPUBLISH* __fastcall GetPUBLISH();
	TsgcWSMQTTPUBACK* __fastcall GetPUBACK();
	TsgcWSMQTTPUBREC* __fastcall GetPUBREC();
	TsgcWSMQTTPUBREL* __fastcall GetPUBREL();
	TsgcWSMQTTPUBCOMP* __fastcall GetPUBCOMP();
	TsgcWSMQTTSUBACK* __fastcall GetSUBACK();
	TsgcWSMQTTUNSUBACK* __fastcall GetUNSUBACK();
	TsgcWSMQTTDISCONNECT* __fastcall GetDISCONNECT();
	TsgcWSMQTTAUTH* __fastcall GetAUTH();
	
public:
	__property TsgcWSMQTTCONNACK* CONNACK = {read=GetCONNACK, write=FCONNACK};
	__property TsgcWSMQTTPUBLISH* PUBLISH = {read=GetPUBLISH, write=FPUBLISH};
	__property TsgcWSMQTTPUBACK* PUBACK = {read=GetPUBACK, write=FPUBACK};
	__property TsgcWSMQTTPUBREC* PUBREC = {read=GetPUBREC, write=FPUBREC};
	__property TsgcWSMQTTPUBREL* PUBREL = {read=GetPUBREL, write=FPUBREL};
	__property TsgcWSMQTTPUBCOMP* PUBCOMP = {read=GetPUBCOMP, write=FPUBCOMP};
	__property TsgcWSMQTTSUBACK* SUBACK = {read=GetSUBACK, write=FSUBACK};
	__property TsgcWSMQTTUNSUBACK* UNSUBACK = {read=GetUNSUBACK, write=FUNSUBACK};
	__property TsgcWSMQTTDISCONNECT* DISCONNECT = {read=GetDISCONNECT, write=FDISCONNECT};
	__property TsgcWSMQTTAUTH* AUTH = {read=GetAUTH, write=FAUTH};
	
private:
	void __fastcall DoReadCONNACK(const Idglobal::TIdBytes aBytes);
	void __fastcall DoReadPUBLISH(const Idglobal::TIdBytes aBytes);
	void __fastcall DoReadPUBACK(const Idglobal::TIdBytes aBytes);
	void __fastcall DoReadPUBREC(const Idglobal::TIdBytes aBytes);
	void __fastcall DoReadPUBREL(const Idglobal::TIdBytes aBytes);
	void __fastcall DoReadPUBCOMP(const Idglobal::TIdBytes aBytes);
	void __fastcall DoReadSUBACK(const Idglobal::TIdBytes aBytes);
	void __fastcall DoReadUNSUBACK(const Idglobal::TIdBytes aBytes);
	void __fastcall DoReadDISCONNECT(const Idglobal::TIdBytes aBytes);
	void __fastcall DoReadAUTH(const Idglobal::TIdBytes aBytes);
	bool FCleanSession;
	System::UnicodeString FClientId;
	Sgcwebsocket_types::TwsMQTTVersion FMQTTVersion;
	System::UnicodeString __fastcall GetClientId();
	
public:
	__property bool CleanSession = {read=FCleanSession, write=FCleanSession, nodefault};
	__property System::UnicodeString ClientId = {read=GetClientId, write=FClientId};
	__property Sgcwebsocket_types::TwsMQTTVersion MQTTVersion = {read=FMQTTVersion, write=FMQTTVersion, nodefault};
	
private:
	TsgcWSMQTTConnect_Properties* FConnectProperties;
	TsgcWSMQTTWill_Properties* FWillProperties;
	TsgcWSMQTTPublish_Properties* FPUBLISHProperties;
	TsgcWSMQTTSubscribe_Properties* FSubscribeProperties;
	TsgcWSMQTTUnsubscribe_Properties* FUnsubscribeProperties;
	TsgcWSMQTTDisconnect_Properties* FDISCONNECTProperties;
	TsgcWSMQTTAuth_Properties* FAUTHProperties;
	void __fastcall SetConnectProperties(TsgcWSMQTTConnect_Properties* const Value);
	void __fastcall SetWillProperties(TsgcWSMQTTWill_Properties* const Value);
	void __fastcall SetPublishProperties(TsgcWSMQTTPublish_Properties* const Value);
	void __fastcall SetSubscribeProperties(TsgcWSMQTTSubscribe_Properties* const Value);
	void __fastcall SetUnsubscribeProperties(TsgcWSMQTTUnsubscribe_Properties* const Value);
	void __fastcall SetDisconnectProperties(TsgcWSMQTTDisconnect_Properties* const Value);
	void __fastcall SetAuthProperties(TsgcWSMQTTAuth_Properties* const Value);
	
public:
	__property TsgcWSMQTTConnect_Properties* ConnectProperties = {read=FConnectProperties, write=SetConnectProperties};
	__property TsgcWSMQTTWill_Properties* WillProperties = {read=FWillProperties, write=SetWillProperties};
	__property TsgcWSMQTTPublish_Properties* PUBLISHProperties = {read=FPUBLISHProperties, write=SetPublishProperties};
	__property TsgcWSMQTTSubscribe_Properties* SubscribeProperties = {read=FSubscribeProperties, write=SetSubscribeProperties};
	__property TsgcWSMQTTUnsubscribe_Properties* UnsubscribeProperties = {read=FUnsubscribeProperties, write=SetUnsubscribeProperties};
	__property TsgcWSMQTTDisconnect_Properties* DISCONNECTProperties = {read=FDISCONNECTProperties, write=SetDisconnectProperties};
	__property TsgcWSMQTTAuth_Properties* AUTHProperties = {read=FAUTHProperties, write=SetAuthProperties};
};


//-- var, const, procedure ---------------------------------------------------
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetReasonCode(Sgcwebsocket_types::TmqttControlPacket aPacket, int aCode);
extern DELPHI_PACKAGE System::UnicodeString __fastcall GetReasonCodeMQTT311(int aCode);
}	/* namespace Sgcwebsocket_protocol_mqtt_message */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_MQTT_MESSAGE)
using namespace Sgcwebsocket_protocol_mqtt_message;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_mqtt_messageHPP
