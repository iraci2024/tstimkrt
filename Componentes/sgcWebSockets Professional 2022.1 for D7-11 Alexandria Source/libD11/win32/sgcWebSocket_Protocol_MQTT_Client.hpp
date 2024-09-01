// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_Protocol_MQTT_Client.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_protocol_mqtt_clientHPP
#define Sgcwebsocket_protocol_mqtt_clientHPP

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
#include <sgcBase_Classes.hpp>
#include <sgcJSON.hpp>
#include <sgcWebSocket_Protocol_MQTT_Message.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Protocol_Base_Client.hpp>
#include <sgcWebSocket_HTTPResponse.hpp>
#include <sgcWebSocket_Classes_SyncObjs.hpp>
#include <sgcBase_Helpers.hpp>
#include <sgcSocket_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_protocol_mqtt_client
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSComponent_HeartBeat;
class DELPHICLASS TsgcWSMQTTQoSItem;
class DELPHICLASS TsgcWSMQTTQoSList;
class DELPHICLASS TsgcWSMQTTQoS_Options;
class DELPHICLASS TsgcWSMQTTAuthentication_Options;
class DELPHICLASS TsgcWSMQTTLWT_Options;
class DELPHICLASS TsgcWSMQTTHeartBeat_Options;
class DELPHICLASS TsgcWSProtocol_MQTT_Client;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TsgcWSMQTTItemType : unsigned char { mqttText, mqttStream };

typedef void __fastcall (__closure *TsgcWSMQTTBeforeConnectEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, bool &aCleanSession, System::UnicodeString &aClientIdentifier);

typedef void __fastcall (__closure *TsgcWSMQTTConnectEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, const bool Session, const int ReasonCode, const System::UnicodeString ReasonName, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTCONNACKProperties* const ConnectProperties);

typedef void __fastcall (__closure *TsgcWSMQTTPingEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection);

typedef void __fastcall (__closure *TsgcWSMQTTPublishEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString aTopic, System::UnicodeString aText, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTPublishProperties* PublishProperties);

typedef void __fastcall (__closure *TsgcWSMQTTPubAckEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::Word aPacketIdentifier, int ReasonCode, const System::UnicodeString ReasonName, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTPUBACKProperties* PubAckProperties);

typedef void __fastcall (__closure *TsgcWSMQTTPubRecEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::Word aPacketIdentifier, int ReasonCode, const System::UnicodeString ReasonName, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTPUBRECProperties* PubRecProperties);

typedef void __fastcall (__closure *TsgcWSMQTTPubRelEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::Word aPacketIdentifier, int ReasonCode, const System::UnicodeString ReasonName, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTPUBRELProperties* PubRelProperties);

typedef void __fastcall (__closure *TsgcWSMQTTPubCompEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::Word aPacketIdentifier, int ReasonCode, const System::UnicodeString ReasonName, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTPUBCOMPProperties* PubCompProperties);

typedef void __fastcall (__closure *TsgcWSMQTTSubscribeEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::Word aPacketIdentifier, Sgcwebsocket_protocol_mqtt_message::TsgcWSSUBACKS* aCodes, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTSUBACKProperties* SubscribeProperties);

typedef void __fastcall (__closure *TsgcWSMQTTUnSubscribeEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::Word aPacketIdentifier, Sgcwebsocket_protocol_mqtt_message::TsgcWSUNSUBACKS* aCodes, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTUNSUBACKProperties* UnsubscribeProperties);

typedef void __fastcall (__closure *TsgcWSMQTTDisconnectEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, int ReasonCode, const System::UnicodeString ReasonName, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTDISCONNECTProperties* DisconnectProperties);

typedef void __fastcall (__closure *TsgcWSMQTTAuthEvent)(Sgcwebsocket_classes::TsgcWSConnection* Connection, int ReasonCode, const System::UnicodeString ReasonName, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTAUTHProperties* AuthProperties);

class PASCALIMPLEMENTATION TsgcWSComponent_HeartBeat : public Sgcwebsocket_classes::TsgcWSComponent
{
	typedef Sgcwebsocket_classes::TsgcWSComponent inherited;
	
public:
	/* TsgcWSComponent.Create */ inline __fastcall virtual TsgcWSComponent_HeartBeat(System::Classes::TComponent* aOwner) : Sgcwebsocket_classes::TsgcWSComponent(aOwner) { }
	/* TsgcWSComponent.Destroy */ inline __fastcall virtual ~TsgcWSComponent_HeartBeat() { }
	
};


class PASCALIMPLEMENTATION TsgcWSMQTTQoSItem : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::TDateTime FDate;
	TsgcWSMQTTItemType FItemType;
	System::Word FPacketIdentifier;
	Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTPublish_Properties* FPublishProperties;
	Sgcwebsocket_types::TmqttQoS FQoS;
	bool FRetain;
	System::Classes::TStream* FStream;
	System::UnicodeString FText;
	System::UnicodeString FTopic;
	void __fastcall SetItemType(const TsgcWSMQTTItemType Value);
	void __fastcall SetPublishProperties(Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTPublish_Properties* const Value);
	void __fastcall SetStream(System::Classes::TStream* const Value);
	
public:
	__fastcall TsgcWSMQTTQoSItem();
	__fastcall virtual ~TsgcWSMQTTQoSItem();
	__property System::TDateTime Date = {read=FDate, write=FDate};
	__property TsgcWSMQTTItemType ItemType = {read=FItemType, write=SetItemType, nodefault};
	__property System::Word PacketIdentifier = {read=FPacketIdentifier, write=FPacketIdentifier, nodefault};
	__property Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTPublish_Properties* PublishProperties = {read=FPublishProperties, write=SetPublishProperties};
	__property Sgcwebsocket_types::TmqttQoS QoS = {read=FQoS, write=FQoS, nodefault};
	__property bool Retain = {read=FRetain, write=FRetain, nodefault};
	__property System::Classes::TStream* Stream = {read=FStream, write=SetStream};
	__property System::UnicodeString Text = {read=FText, write=FText};
	__property System::UnicodeString Topic = {read=FTopic, write=FTopic};
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSMQTTQoSList : public System::Contnrs::TObjectList
{
	typedef System::Contnrs::TObjectList inherited;
	
public:
	/* TObjectList.Create */ inline __fastcall TsgcWSMQTTQoSList()/* overload */ : System::Contnrs::TObjectList() { }
	/* TObjectList.Create */ inline __fastcall TsgcWSMQTTQoSList(bool AOwnsObjects)/* overload */ : System::Contnrs::TObjectList(AOwnsObjects) { }
	
public:
	/* TList.Destroy */ inline __fastcall virtual ~TsgcWSMQTTQoSList() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSMQTTQoS_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	Sgcwebsocket_types::TmqttQoS FLevel;
	int FInterval;
	int FTimeout;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property Sgcwebsocket_types::TmqttQoS Level = {read=FLevel, write=FLevel, nodefault};
	__property int Interval = {read=FInterval, write=FInterval, nodefault};
	__property int Timeout = {read=FTimeout, write=FTimeout, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSMQTTQoS_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSMQTTQoS_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSMQTTAuthentication_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	System::UnicodeString FPassword;
	System::UnicodeString FUserName;
	
public:
	__fastcall virtual TsgcWSMQTTAuthentication_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property System::UnicodeString Password = {read=FPassword, write=FPassword};
	__property System::UnicodeString UserName = {read=FUserName, write=FUserName};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSMQTTAuthentication_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSMQTTLWT_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	Sgcwebsocket_types::TmqttQoS FQoS;
	bool FRetain;
	System::UnicodeString FText;
	System::UnicodeString FTopic;
	Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTWill_Properties* FWillProperties;
	void __fastcall SetWillProperties(Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTWill_Properties* const Value);
	
public:
	__fastcall virtual TsgcWSMQTTLWT_Options();
	__fastcall virtual ~TsgcWSMQTTLWT_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property Sgcwebsocket_types::TmqttQoS QoS = {read=FQoS, write=FQoS, nodefault};
	__property bool Retain = {read=FRetain, write=FRetain, nodefault};
	__property System::UnicodeString Text = {read=FText, write=FText};
	__property System::UnicodeString Topic = {read=FTopic, write=FTopic};
	__property Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTWill_Properties* WillProperties = {read=FWillProperties, write=SetWillProperties};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSMQTTHeartBeat_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	int FInterval;
	int FTimeout;
	
public:
	__fastcall virtual TsgcWSMQTTHeartBeat_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property int Interval = {read=FInterval, write=FInterval, nodefault};
	__property int Timeout = {read=FTimeout, write=FTimeout, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSMQTTHeartBeat_Options() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWSProtocol_MQTT_Client : public Sgcwebsocket_protocol_base_client::TsgcWSProtocol_Client_Base
{
	typedef Sgcwebsocket_protocol_base_client::TsgcWSProtocol_Client_Base inherited;
	
private:
	System::Word FPacketIdentifier;
	System::Word __fastcall GetPacketIdentifier();
	System::UnicodeString FWSMessageId;
	
protected:
	Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTMessage* FWSMessage;
	Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTMessage* __fastcall GetWSMessage();
	Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTMessage* __fastcall GetWSMessageByConnection(Sgcwebsocket_classes::TsgcWSConnection* const aConnection);
	__property Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTMessage* WSMessage = {read=GetWSMessage, write=FWSMessage};
	
private:
	Sgcwebsocket_classes::TsgcWSConnection* FWSConnection;
	
protected:
	virtual void __fastcall DoNotifyBinary(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual void __fastcall DoEventBinary(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, System::Classes::TMemoryStream* Data)/* overload */;
	virtual void __fastcall DoEventDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int Code);
	virtual void __fastcall DoEventException(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Error, System::Sysutils::Exception* aException);
	virtual void __fastcall DoInitialize(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoFinalize(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoClear(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	
private:
	System::TDateTime FFirstPing;
	System::TDateTime FLastPing;
	System::TDateTime FLastPong;
	
protected:
	virtual bool __fastcall DoKeepAlive();
	virtual void __fastcall DoWriteRawData(const System::UnicodeString aText);
	
public:
	virtual void __fastcall WriteData(const System::UnicodeString aText)/* overload */;
	
protected:
	virtual System::UnicodeString __fastcall GetWSMessageText();
	virtual void __fastcall DoWriteMessageText();
	
public:
	__fastcall virtual TsgcWSProtocol_MQTT_Client(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWSProtocol_MQTT_Client();
	
private:
	int FLastPUBACK;
	int FLastPUBCOMP;
	bool FWaitResponse;
	bool __fastcall DoPublishAndWait(System::Word aPacketIdentifier, const System::UnicodeString aTopic, const System::UnicodeString aText, System::Classes::TStream* const aStream, Sgcwebsocket_types::TmqttQoS aQoS, bool aRetain, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTPublish_Properties* aPublishProperties = (Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTPublish_Properties*)(0x0), const int aTimeout = 0x2710);
	void __fastcall DoWriteBytes(const System::DynamicArray<System::Byte> aBytes);
	void __fastcall DoConnect();
	void __fastcall DoPing();
	void __fastcall DoPublish(System::Word aPacketIdentifier, const System::UnicodeString aTopic, const System::UnicodeString aText, Sgcwebsocket_types::TmqttQoS aQoS, bool aRetain, const bool aDup, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTPublish_Properties* aPublishProperties = (Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTPublish_Properties*)(0x0))/* overload */;
	void __fastcall DoPublish(System::Word aPacketIdentifier, const System::UnicodeString aTopic, System::Classes::TStream* aStream, Sgcwebsocket_types::TmqttQoS aQoS, bool aRetain, const bool aDup, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTPublish_Properties* aPublishProperties = (Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTPublish_Properties*)(0x0))/* overload */;
	void __fastcall DoPubAck(System::Word aPacketIdentifier);
	void __fastcall DoPubRec(System::Word aPacketIdentifier);
	void __fastcall DoPubRel(System::Word aPacketIdentifier);
	void __fastcall DoPubComp(System::Word aPacketIdentifier);
	void __fastcall DoSubscribe(int aPacketIdentifier, Sgcwebsocket_protocol_mqtt_message::TsgcWSTopics* aTopics, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTSubscribe_Properties* const aSubscribeProperties);
	void __fastcall DoUnSubscribe(int aPacketIdentifier, Sgcwebsocket_protocol_mqtt_message::TsgcWSTopics* aTopics);
	void __fastcall DoDisconnect(int aReasonCode = 0x0, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTDisconnect_Properties* aDisconnectProperties = (Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTDisconnect_Properties*)(0x0));
	void __fastcall DoAuth(bool aReAuthenticate, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTAuth_Properties* aAuthProperties = (Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTAuth_Properties*)(0x0));
	
public:
	void __fastcall Connect();
	void __fastcall Ping();
	System::Word __fastcall Publish(const System::UnicodeString aTopic, const System::UnicodeString aText, Sgcwebsocket_types::TmqttQoS aQoS = (Sgcwebsocket_types::TmqttQoS)(0x0), bool aRetain = false, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTPublish_Properties* const aPublishProperties = (Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTPublish_Properties*)(0x0))/* overload */;
	System::Word __fastcall Publish(const System::UnicodeString aTopic, System::Classes::TStream* const aStream, Sgcwebsocket_types::TmqttQoS aQoS = (Sgcwebsocket_types::TmqttQoS)(0x0), bool aRetain = false, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTPublish_Properties* const aPublishProperties = (Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTPublish_Properties*)(0x0))/* overload */;
	bool __fastcall PublishAndWait(const System::UnicodeString aTopic, const System::UnicodeString aText, Sgcwebsocket_types::TmqttQoS aQoS = (Sgcwebsocket_types::TmqttQoS)(0x0), bool aRetain = false, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTPublish_Properties* const aPublishProperties = (Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTPublish_Properties*)(0x0), const int aTimeout = 0x2710)/* overload */;
	bool __fastcall PublishAndWait(const System::UnicodeString aTopic, System::Classes::TStream* const aStream, Sgcwebsocket_types::TmqttQoS aQoS = (Sgcwebsocket_types::TmqttQoS)(0x0), bool aRetain = false, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTPublish_Properties* const aPublishProperties = (Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTPublish_Properties*)(0x0), const int aTimeout = 0x2710)/* overload */;
	System::Word __fastcall Subscribe(const System::UnicodeString aTopic, Sgcwebsocket_types::TmqttQoS aQoS = (Sgcwebsocket_types::TmqttQoS)(0x0), Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTSubscribe_Properties* const aSubscribeProperties = (Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTSubscribe_Properties*)(0x0))/* overload */;
	System::Word __fastcall Subscribe(Sgcwebsocket_protocol_mqtt_message::TsgcWSTopics* aTopics)/* overload */;
	System::Word __fastcall UnSubscribe(const System::UnicodeString aTopic, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTUnsubscribe_Properties* aUnsubscribeProperties = (Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTUnsubscribe_Properties*)(0x0))/* overload */;
	System::Word __fastcall UnSubscribe(Sgcwebsocket_protocol_mqtt_message::TsgcWSTopics* aTopics)/* overload */;
	void __fastcall Disconnect()/* overload */;
	void __fastcall Disconnect(int aReasonCode, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTDisconnect_Properties* aDisconnectProperties = (Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTDisconnect_Properties*)(0x0))/* overload */;
	void __fastcall Auth(bool aReAuthenticate, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTAuth_Properties* aAuthProperties = (Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTAuth_Properties*)(0x0));
	
private:
	Sgcwebsocket_types::TwsMQTTVersion FMQTTVersion;
	
protected:
	virtual void __fastcall SetMQTTVersion(const Sgcwebsocket_types::TwsMQTTVersion Value);
	
public:
	__property Sgcwebsocket_types::TwsMQTTVersion MQTTVersion = {read=FMQTTVersion, write=SetMQTTVersion, nodefault};
	
private:
	Sgcbase_classes::TsgcTimer* FQoSTimer;
	TsgcWSMQTTQoS_Options* FQoS;
	TsgcWSMQTTQoSList* FQoSList;
	TsgcWSMQTTQoSList* __fastcall GetQoSList();
	
protected:
	virtual void __fastcall DoStartQoS();
	virtual void __fastcall DoStopQoS();
	virtual void __fastcall DoQoSList();
	virtual void __fastcall DoDeleteStoredMessage(const System::Word aId);
	virtual void __fastcall DoQueuePublish(const System::Word aPacketIdentifier, const System::UnicodeString aTopic, const System::UnicodeString aText, Sgcwebsocket_types::TmqttQoS aQoS, bool aRetain, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTPublish_Properties* aPublishProperties)/* overload */;
	virtual void __fastcall DoQueuePublish(const System::Word aPacketIdentifier, const System::UnicodeString aTopic, System::Classes::TStream* aStream, Sgcwebsocket_types::TmqttQoS aQoS, bool aRetain, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTPublish_Properties* aPublishProperties)/* overload */;
	virtual void __fastcall OnQoSEvent(System::TObject* Sender);
	virtual void __fastcall OnQoSExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	void __fastcall SetQoS(TsgcWSMQTTQoS_Options* const Value);
	__property TsgcWSMQTTQoSList* QoSList = {read=GetQoSList};
	
public:
	__property TsgcWSMQTTQoS_Options* QoS = {read=FQoS, write=SetQoS};
	
private:
	TsgcWSMQTTLWT_Options* FLastWillTestament;
	void __fastcall SetLastWillTestament(TsgcWSMQTTLWT_Options* const Value);
	
public:
	__property TsgcWSMQTTLWT_Options* LastWillTestament = {read=FLastWillTestament, write=SetLastWillTestament};
	
private:
	TsgcWSMQTTAuthentication_Options* FAuthentication;
	void __fastcall SetAuthentication(TsgcWSMQTTAuthentication_Options* const Value);
	
public:
	__property TsgcWSMQTTAuthentication_Options* Authentication = {read=FAuthentication, write=SetAuthentication};
	
private:
	TsgcWSMQTTHeartBeat_Options* FHeartBeat;
	void __fastcall SetHeartBeat(TsgcWSMQTTHeartBeat_Options* const Value);
	
public:
	__property TsgcWSMQTTHeartBeat_Options* HeartBeat = {read=FHeartBeat, write=SetHeartBeat};
	
private:
	Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTConnect_Properties* FConnectProperties;
	void __fastcall SetConnectProperties(Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTConnect_Properties* const Value);
	
public:
	__property Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTConnect_Properties* ConnectProperties = {read=FConnectProperties, write=SetConnectProperties};
	
private:
	TsgcWSMQTTBeforeConnectEvent FOnMQTTBeforeConnect;
	TsgcWSMQTTConnectEvent FOnMQTTConnect;
	TsgcWSMQTTPingEvent FOnMQTTPing;
	TsgcWSMQTTPublishEvent FOnMQTTPublish;
	TsgcWSMQTTPubAckEvent FOnMQTTPubAck;
	TsgcWSMQTTPubRecEvent FOnMQTTPubRec;
	TsgcWSMQTTPubRelEvent FOnMQTTPubRel;
	TsgcWSMQTTPubCompEvent FOnMQTTPubComp;
	TsgcWSMQTTSubscribeEvent FOnMQTTSubscribe;
	TsgcWSMQTTUnSubscribeEvent FOnMQTTUnSubscribe;
	TsgcWSMQTTDisconnectEvent FOnMQTTDisconnect;
	TsgcWSMQTTAuthEvent FOnMQTTAuth;
	
protected:
	virtual void __fastcall DoEventMQTTBeforeConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventMQTTConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventMQTTPing(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventMQTTPublish(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventMQTTPubAck(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventMQTTPubRec(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventMQTTPubRel(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventMQTTPubComp(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventMQTTSubscribe(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventMQTTUnSubscribe(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventMQTTDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventMQTTAuth(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	
public:
	__property TsgcWSMQTTBeforeConnectEvent OnMQTTBeforeConnect = {read=FOnMQTTBeforeConnect, write=FOnMQTTBeforeConnect};
	__property TsgcWSMQTTConnectEvent OnMQTTConnect = {read=FOnMQTTConnect, write=FOnMQTTConnect};
	__property TsgcWSMQTTPingEvent OnMQTTPing = {read=FOnMQTTPing, write=FOnMQTTPing};
	__property TsgcWSMQTTPublishEvent OnMQTTPublish = {read=FOnMQTTPublish, write=FOnMQTTPublish};
	__property TsgcWSMQTTPubAckEvent OnMQTTPubAck = {read=FOnMQTTPubAck, write=FOnMQTTPubAck};
	__property TsgcWSMQTTPubRecEvent OnMQTTPubRec = {read=FOnMQTTPubRec, write=FOnMQTTPubRec};
	__property TsgcWSMQTTPubRelEvent OnMQTTPubRel = {read=FOnMQTTPubRel, write=FOnMQTTPubRel};
	__property TsgcWSMQTTPubCompEvent OnMQTTPubComp = {read=FOnMQTTPubComp, write=FOnMQTTPubComp};
	__property TsgcWSMQTTSubscribeEvent OnMQTTSubscribe = {read=FOnMQTTSubscribe, write=FOnMQTTSubscribe};
	__property TsgcWSMQTTUnSubscribeEvent OnMQTTUnSubscribe = {read=FOnMQTTUnSubscribe, write=FOnMQTTUnSubscribe};
	__property TsgcWSMQTTDisconnectEvent OnMQTTDisconnect = {read=FOnMQTTDisconnect, write=FOnMQTTDisconnect};
	__property TsgcWSMQTTAuthEvent OnMQTTAuth = {read=FOnMQTTAuth, write=FOnMQTTAuth};
	/* Hoisted overloads: */
	
public:
	inline void __fastcall  WriteData(System::Classes::TStream* aStream, int aSize = 0x0, const Sgcwebsocket_types::TwsStreaming aStreaming = (Sgcwebsocket_types::TwsStreaming)(0x0)){ Sgcwebsocket_protocol_base_client::TsgcWSProtocol_Client_Base::WriteData(aStream, aSize, aStreaming); }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_protocol_mqtt_client */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_PROTOCOL_MQTT_CLIENT)
using namespace Sgcwebsocket_protocol_mqtt_client;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_protocol_mqtt_clientHPP
