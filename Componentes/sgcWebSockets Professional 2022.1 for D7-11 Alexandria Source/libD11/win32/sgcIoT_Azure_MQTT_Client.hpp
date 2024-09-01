// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcIoT_Azure_MQTT_Client.pas' rev: 35.00 (Windows)

#ifndef Sgciot_azure_mqtt_clientHPP
#define Sgciot_azure_mqtt_clientHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <sgcIoT_MQTT_Client.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcIoT_Client.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgciot_azure_mqtt_client
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcIoT_Azure_MQTT_Client_Options;
class DELPHICLASS TsgcIoT_Azure_MQTT_Client;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TazuIoTQoS : unsigned char { azuIoTQoS0, azuIoTQoS1 };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcIoT_Azure_MQTT_Client_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FDeviceId;
	System::UnicodeString FIoTHub;
	System::UnicodeString FModuleId;
	bool FWebSockets;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString DeviceId = {read=FDeviceId, write=FDeviceId};
	__property System::UnicodeString IoTHub = {read=FIoTHub, write=FIoTHub};
	__property System::UnicodeString ModuleId = {read=FModuleId, write=FModuleId};
	__property bool WebSockets = {read=FWebSockets, write=FWebSockets, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcIoT_Azure_MQTT_Client_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcIoT_Azure_MQTT_Client_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcIoT_Azure_MQTT_Client : public Sgciot_mqtt_client::TsgcIoTMQTT_Client
{
	typedef Sgciot_mqtt_client::TsgcIoTMQTT_Client inherited;
	
protected:
	virtual void __fastcall OnMQTTBeforeConnectEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, bool &aCleanSession, System::UnicodeString &aClientIdentifier);
	virtual void __fastcall DoBeforeConnect();
	
private:
	System::UnicodeString __fastcall GetPassword_SAS();
	System::UnicodeString __fastcall GetPassword_X509();
	System::UnicodeString __fastcall GetTopicDeviceToCloud(System::Classes::TStrings* const aProperties = (System::Classes::TStrings*)(0x0));
	System::UnicodeString __fastcall GetTopicCloudToDevice();
	System::UnicodeString __fastcall GetTopicDeviceTwins();
	System::UnicodeString __fastcall GetTopicDeviceTwinsProperties(const System::UnicodeString aRequestId);
	System::UnicodeString __fastcall GetTopicDeviceTwinsUpdateProperties(const System::UnicodeString aRequestId);
	System::UnicodeString __fastcall GetTopicDirectMethod();
	System::UnicodeString __fastcall GetTopicRespondPublicMethod(const System::UnicodeString aRequestId, int aStatus);
	TsgcIoT_Azure_MQTT_Client_Options* FAzure;
	TsgcIoT_Azure_MQTT_Client_Options* __fastcall GetAzure();
	void __fastcall SetAzure(TsgcIoT_Azure_MQTT_Client_Options* const Value);
	
public:
	__property TsgcIoT_Azure_MQTT_Client_Options* Azure = {read=GetAzure, write=SetAzure};
	__fastcall virtual TsgcIoT_Azure_MQTT_Client(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcIoT_Azure_MQTT_Client();
	int __fastcall Subscribe_CloudToDevice();
	int __fastcall UnSubscribe_CloudToDevice();
	int __fastcall Send_DeviceToCloud(const System::UnicodeString aText, TazuIoTQoS aQoS = (TazuIoTQoS)(0x0))/* overload */;
	int __fastcall Send_DeviceToCloud(const System::UnicodeString aText, System::Classes::TStrings* const aProperties, TazuIoTQoS aQoS = (TazuIoTQoS)(0x0))/* overload */;
	bool __fastcall SendAndWait_DeviceToCloud(const System::UnicodeString aText, const int aTimeout = 0x2710)/* overload */;
	bool __fastcall SendAndWait_DeviceToCloud(const System::UnicodeString aText, System::Classes::TStrings* const aProperties, const int aTimeout = 0x2710)/* overload */;
	int __fastcall Subscribe_DeviceTwins();
	int __fastcall UnSubscribe_DeviceTwins();
	void __fastcall Get_DeviceTwinsProperties(const System::UnicodeString aRequestId, TazuIoTQoS aQoS = (TazuIoTQoS)(0x0));
	bool __fastcall GetAndWait_DeviceTwinsProperties(const System::UnicodeString aRequestId, const int aTimeout = 0x2710);
	int __fastcall Set_DeviceTwinsProperties(const System::UnicodeString aRequestId, System::UnicodeString aText, TazuIoTQoS aQoS = (TazuIoTQoS)(0x0));
	bool __fastcall SetAndWait_DeviceTwinsProperties(const System::UnicodeString aRequestId, System::UnicodeString aText, const int aTimeout = 0x2710);
	int __fastcall Subscribe_DirectMethod();
	int __fastcall UnSubscribe_DirectMethod();
	int __fastcall RespondPublicMethod(const System::UnicodeString aRequestId, int aStatus, const System::UnicodeString aText, TazuIoTQoS aQoS = (TazuIoTQoS)(0x0));
	bool __fastcall RespondAndWaitPublicMethod(const System::UnicodeString aRequestId, int aStatus, const System::UnicodeString aText, const int aTimeout = 0x2710);
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgciot_azure_mqtt_client */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCIOT_AZURE_MQTT_CLIENT)
using namespace Sgciot_azure_mqtt_client;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgciot_azure_mqtt_clientHPP
