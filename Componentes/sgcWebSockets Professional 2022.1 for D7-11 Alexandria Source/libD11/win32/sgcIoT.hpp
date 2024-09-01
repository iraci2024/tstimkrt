// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcIoT.pas' rev: 35.00 (Windows)

#ifndef SgciotHPP
#define SgciotHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <sgcIoT_Amazon_MQTT_Client.hpp>
#include <sgcIoT_Azure_MQTT_Client.hpp>
#include <sgcIoT_MQTT_Client.hpp>
#include <sgcIoT_Client.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>
#include <sgcWebSocket_Protocol_MQTT_Client.hpp>
#include <sgcWebSocket_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgciot
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcIoTAmazon_MQTT_Client;
class DELPHICLASS TsgcIoTAzure_MQTT_Client;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcIoTAmazon_MQTT_Client : public Sgciot_amazon_mqtt_client::TsgcIoT_Amazon_MQTT_Client
{
	typedef Sgciot_amazon_mqtt_client::TsgcIoT_Amazon_MQTT_Client inherited;
	
__published:
	__property Active;
	__property Certificate;
	__property Amazon;
	__property MQTTHeartBeat;
	__property MQTTAuthentication;
	__property WatchDog;
	__property LogFile;
	__property SignatureV4;
	__property CustomAuthentication;
	__property OnConnect;
	__property OnDisconnect;
	__property OnException;
	__property OnError;
	__property OnMQTTBeforeConnect;
	__property OnMQTTConnect;
	__property OnMQTTPublish;
	__property OnMQTTPubAck;
	__property OnMQTTSubscribe;
	__property OnMQTTUnSubscribe;
	__property OnMQTTDisconnect;
	__property Version = {default=0};
public:
	/* TsgcIoT_Amazon_MQTT_Client.Create */ inline __fastcall virtual TsgcIoTAmazon_MQTT_Client(System::Classes::TComponent* aOwner) : Sgciot_amazon_mqtt_client::TsgcIoT_Amazon_MQTT_Client(aOwner) { }
	/* TsgcIoT_Amazon_MQTT_Client.Destroy */ inline __fastcall virtual ~TsgcIoTAmazon_MQTT_Client() { }
	
};


class PASCALIMPLEMENTATION TsgcIoTAzure_MQTT_Client : public Sgciot_azure_mqtt_client::TsgcIoT_Azure_MQTT_Client
{
	typedef Sgciot_azure_mqtt_client::TsgcIoT_Azure_MQTT_Client inherited;
	
__published:
	__property Active;
	__property Certificate;
	__property SAS;
	__property Azure;
	__property MQTTHeartBeat;
	__property WatchDog;
	__property LogFile;
	__property OnConnect;
	__property OnDisconnect;
	__property OnException;
	__property OnError;
	__property OnMQTTBeforeConnect;
	__property OnMQTTConnect;
	__property OnMQTTPublish;
	__property OnMQTTPubAck;
	__property OnMQTTSubscribe;
	__property OnMQTTUnSubscribe;
	__property OnMQTTDisconnect;
	__property Version = {default=0};
public:
	/* TsgcIoT_Azure_MQTT_Client.Create */ inline __fastcall virtual TsgcIoTAzure_MQTT_Client(System::Classes::TComponent* aOwner) : Sgciot_azure_mqtt_client::TsgcIoT_Azure_MQTT_Client(aOwner) { }
	/* TsgcIoT_Azure_MQTT_Client.Destroy */ inline __fastcall virtual ~TsgcIoTAzure_MQTT_Client() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgciot */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCIOT)
using namespace Sgciot;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// SgciotHPP
