// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcIoT_MQTT_Client.pas' rev: 35.00 (Windows)

#ifndef Sgciot_mqtt_clientHPP
#define Sgciot_mqtt_clientHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <sgcBase_Helpers.hpp>
#include <sgcIoT_Client.hpp>
#include <sgcWebSocket_Client.hpp>
#include <sgcWebSocket_Protocol_MQTT_Client.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcWebSocket_Protocol_MQTT_Message.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgciot_mqtt_client
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcIoTMQTT_Client;
//-- type declarations -------------------------------------------------------
typedef Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTBeforeConnectEvent TsgcIoTMQTTBeforeConnectEvent;

typedef Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTConnectEvent TsgcIoTMQTTConnectEvent;

typedef Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTPingEvent TsgcIoTMQTTPingEvent;

typedef Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTPublishEvent TsgcIoTMQTTPublishEvent;

typedef Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTPubAckEvent TsgcIoTMQTTPubAckEvent;

typedef Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTPubRecEvent TsgcIoTMQTTPubRecEvent;

typedef Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTPubCompEvent TsgcIoTMQTTPubCompEvent;

typedef Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTSubscribeEvent TsgcIoTMQTTSubscribeEvent;

typedef Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTUnSubscribeEvent TsgcIoTMQTTUnSubscribeEvent;

typedef Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTDisconnectEvent TsgcIoTMQTTDisconnectEvent;

class PASCALIMPLEMENTATION TsgcIoTMQTT_Client : public Sgciot_client::TsgcIoTComponent_Client
{
	typedef Sgciot_client::TsgcIoTComponent_Client inherited;
	
private:
	Sgcwebsocket_protocol_mqtt_client::TsgcWSProtocol_MQTT_Client* FMQTT;
	Sgcwebsocket_protocol_mqtt_client::TsgcWSProtocol_MQTT_Client* __fastcall GetMQTT();
	
protected:
	__property Sgcwebsocket_protocol_mqtt_client::TsgcWSProtocol_MQTT_Client* MQTT = {read=GetMQTT, write=FMQTT};
	
private:
	Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTAuthentication_Options* FMQTTAuthentication;
	Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTHeartBeat_Options* FMQTTHeartBeat;
	void __fastcall SetMQTTAuthentication(Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTAuthentication_Options* const Value);
	void __fastcall SetMQTTHeartBeat(Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTHeartBeat_Options* const Value);
	
protected:
	__property Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTAuthentication_Options* MQTTAuthentication = {read=FMQTTAuthentication, write=SetMQTTAuthentication};
	__property Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTHeartBeat_Options* MQTTHeartBeat = {read=FMQTTHeartBeat, write=SetMQTTHeartBeat};
	
public:
	__fastcall virtual TsgcIoTMQTT_Client(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcIoTMQTT_Client();
	
protected:
	virtual System::Word __fastcall DoSubscribe(const System::UnicodeString aTopic, Sgcwebsocket_types::TmqttQoS aQoS = (Sgcwebsocket_types::TmqttQoS)(0x0));
	virtual System::Word __fastcall DoUnSubscribe(const System::UnicodeString aTopic);
	virtual System::Word __fastcall DoPublish(const System::UnicodeString aTopic, const System::UnicodeString aText, Sgcwebsocket_types::TmqttQoS aQoS = (Sgcwebsocket_types::TmqttQoS)(0x0), bool aRetain = false);
	virtual bool __fastcall DoPublishAndWait(const System::UnicodeString aTopic, const System::UnicodeString aText, Sgcwebsocket_types::TmqttQoS aQoS = (Sgcwebsocket_types::TmqttQoS)(0x0), bool aRetain = false, const int aTimeout = 0x2710);
	
public:
	void __fastcall Subscribe(const System::UnicodeString aTopic);
	void __fastcall UnSubscribe(const System::UnicodeString aTopic);
	int __fastcall Publish(const System::UnicodeString aTopic, const System::UnicodeString aText, Sgcwebsocket_types::TmqttQoS aQoS = (Sgcwebsocket_types::TmqttQoS)(0x0), bool aRetain = false);
	bool __fastcall PublishAndWait(const System::UnicodeString aTopic, const System::UnicodeString aText, Sgcwebsocket_types::TmqttQoS aQoS = (Sgcwebsocket_types::TmqttQoS)(0x0), bool aRetain = false, const int aTimeout = 0x2710);
	
protected:
	Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTBeforeConnectEvent FOnMQTTBeforeConnect;
	Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTConnectEvent FOnMQTTConnect;
	Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTPingEvent FOnMQTTPing;
	Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTPublishEvent FOnMQTTPublish;
	Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTPubAckEvent FOnMQTTPubAck;
	Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTPubRecEvent FOnMQTTPubRec;
	Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTPubCompEvent FOnMQTTPubComp;
	Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTSubscribeEvent FOnMQTTSubscribe;
	Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTUnSubscribeEvent FOnMQTTUnSubscribe;
	Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTDisconnectEvent FOnMQTTDisconnect;
	virtual void __fastcall OnProtocolErrorEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, const System::UnicodeString Error);
	virtual void __fastcall OnProtocolExceptionEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::Sysutils::Exception* E);
	virtual void __fastcall OnProtocolConnectEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection);
	virtual void __fastcall OnProtocolDisconnectEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, int Code);
	virtual void __fastcall DoBeforeConnect();
	virtual void __fastcall OnMQTTBeforeConnectEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, bool &aCleanSession, System::UnicodeString &aClientIdentifier);
	virtual void __fastcall OnMQTTConnectEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, const bool Session, const int ReturnCode, const System::UnicodeString ReturnReason, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTCONNACKProperties* const ConnectProperties);
	virtual void __fastcall OnMQTTDisconnectEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, int ReturnCode, const System::UnicodeString ReturnReason, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTDISCONNECTProperties* DisconnectProperties);
	virtual void __fastcall OnMQTTPublishEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::UnicodeString aTopic, System::UnicodeString aText, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTPublishProperties* PublishProperties);
	virtual void __fastcall OnMQTTSubscribeEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::Word aPacketIdentifier, Sgcwebsocket_protocol_mqtt_message::TsgcWSSUBACKS* aCodes, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTSUBACKProperties* SubscribeProperties);
	virtual void __fastcall OnMQTTUnSubscribeEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::Word aPacketIdentifier, Sgcwebsocket_protocol_mqtt_message::TsgcWSUNSUBACKS* aCodes, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTUNSUBACKProperties* UnsubscribeProperties);
	virtual void __fastcall OnMQTTPubAckEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::Word aPacketIdentifier, int ReasonCode, const System::UnicodeString ReasonName, Sgcwebsocket_protocol_mqtt_message::TsgcWSMQTTPUBACKProperties* PubAckProperties);
	
public:
	__property Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTBeforeConnectEvent OnMQTTBeforeConnect = {read=FOnMQTTBeforeConnect, write=FOnMQTTBeforeConnect};
	__property Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTConnectEvent OnMQTTConnect = {read=FOnMQTTConnect, write=FOnMQTTConnect};
	__property Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTPingEvent OnMQTTPing = {read=FOnMQTTPing, write=FOnMQTTPing};
	__property Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTPublishEvent OnMQTTPublish = {read=FOnMQTTPublish, write=FOnMQTTPublish};
	__property Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTPubAckEvent OnMQTTPubAck = {read=FOnMQTTPubAck, write=FOnMQTTPubAck};
	__property Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTPubRecEvent OnMQTTPubRec = {read=FOnMQTTPubRec, write=FOnMQTTPubRec};
	__property Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTPubCompEvent OnMQTTPubComp = {read=FOnMQTTPubComp, write=FOnMQTTPubComp};
	__property Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTSubscribeEvent OnMQTTSubscribe = {read=FOnMQTTSubscribe, write=FOnMQTTSubscribe};
	__property Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTUnSubscribeEvent OnMQTTUnSubscribe = {read=FOnMQTTUnSubscribe, write=FOnMQTTUnSubscribe};
	__property Sgcwebsocket_protocol_mqtt_client::TsgcWSMQTTDisconnectEvent OnMQTTDisconnect = {read=FOnMQTTDisconnect, write=FOnMQTTDisconnect};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgciot_mqtt_client */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCIOT_MQTT_CLIENT)
using namespace Sgciot_mqtt_client;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgciot_mqtt_clientHPP
