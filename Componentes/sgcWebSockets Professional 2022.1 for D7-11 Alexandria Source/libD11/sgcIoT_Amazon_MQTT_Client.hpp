// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcIoT_Amazon_MQTT_Client.pas' rev: 35.00 (Windows)

#ifndef Sgciot_amazon_mqtt_clientHPP
#define Sgciot_amazon_mqtt_clientHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcIoT_MQTT_Client.hpp>
#include <sgcIoT_Client.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgciot_amazon_mqtt_client
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcIoT_Amazon_MQTT_Client_Options;
class DELPHICLASS TsgcIoT_Amazon_MQTT_Signature_V4_Options;
class DELPHICLASS TsgcIoT_Amazon_MQTT_Custom_Authentication_Options;
class DELPHICLASS TsgcIoT_Amazon_MQTT_Client;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TawsIoTQoS : unsigned char { awsIoTQoS0, awsIoTQoS1 };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcIoT_Amazon_MQTT_Client_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FClientId;
	System::UnicodeString FEndpoint;
	int FPort;
	
public:
	__fastcall virtual TsgcIoT_Amazon_MQTT_Client_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString ClientId = {read=FClientId, write=FClientId};
	__property System::UnicodeString Endpoint = {read=FEndpoint, write=FEndpoint};
	__property int Port = {read=FPort, write=FPort, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcIoT_Amazon_MQTT_Client_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcIoT_Amazon_MQTT_Signature_V4_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FAccessKey;
	bool FEnabled;
	System::UnicodeString FRegion;
	System::UnicodeString FSecretKey;
	System::UnicodeString FSessionToken;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString AccessKey = {read=FAccessKey, write=FAccessKey};
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property System::UnicodeString Region = {read=FRegion, write=FRegion};
	__property System::UnicodeString SecretKey = {read=FSecretKey, write=FSecretKey};
	__property System::UnicodeString SessionToken = {read=FSessionToken, write=FSessionToken};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcIoT_Amazon_MQTT_Signature_V4_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcIoT_Amazon_MQTT_Signature_V4_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcIoT_Amazon_MQTT_Custom_Authentication_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	System::Classes::TStringList* FHeaders;
	System::UnicodeString FParameters;
	bool FWebSockets;
	void __fastcall SetHeaders(System::Classes::TStringList* const Value);
	
public:
	__fastcall virtual TsgcIoT_Amazon_MQTT_Custom_Authentication_Options();
	__fastcall virtual ~TsgcIoT_Amazon_MQTT_Custom_Authentication_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property System::Classes::TStringList* Headers = {read=FHeaders, write=SetHeaders};
	__property System::UnicodeString Parameters = {read=FParameters, write=FParameters};
	__property bool WebSockets = {read=FWebSockets, write=FWebSockets, nodefault};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcIoT_Amazon_MQTT_Client : public Sgciot_mqtt_client::TsgcIoTMQTT_Client
{
	typedef Sgciot_mqtt_client::TsgcIoTMQTT_Client inherited;
	
protected:
	virtual void __fastcall DoBeforeConnect();
	virtual void __fastcall OnMQTTBeforeConnectEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, bool &aCleanSession, System::UnicodeString &aClientIdentifier);
	
private:
	TsgcIoT_Amazon_MQTT_Client_Options* FAmazon;
	TsgcIoT_Amazon_MQTT_Client_Options* __fastcall GetAmazon();
	void __fastcall SetAmazon(TsgcIoT_Amazon_MQTT_Client_Options* const Value);
	
public:
	__property TsgcIoT_Amazon_MQTT_Client_Options* Amazon = {read=GetAmazon, write=SetAmazon};
	
private:
	TsgcIoT_Amazon_MQTT_Signature_V4_Options* FSignatureV4;
	void __fastcall SetSignatureV4(TsgcIoT_Amazon_MQTT_Signature_V4_Options* const Value);
	System::UnicodeString __fastcall GetSignatureV4QueryString();
	
public:
	__property TsgcIoT_Amazon_MQTT_Signature_V4_Options* SignatureV4 = {read=FSignatureV4, write=SetSignatureV4};
	
private:
	TsgcIoT_Amazon_MQTT_Custom_Authentication_Options* FCustomAuthentication;
	void __fastcall SetCustomAuthentication(TsgcIoT_Amazon_MQTT_Custom_Authentication_Options* const Value);
	
protected:
	virtual void __fastcall OnHandshakeEvent(Sgcwebsocket_classes::TsgcWSConnection* Connection, System::Classes::TStringList* &Headers);
	
public:
	__property TsgcIoT_Amazon_MQTT_Custom_Authentication_Options* CustomAuthentication = {read=FCustomAuthentication, write=SetCustomAuthentication};
	__fastcall virtual TsgcIoT_Amazon_MQTT_Client(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcIoT_Amazon_MQTT_Client();
	
protected:
	virtual void __fastcall DoSubscribe_ClientId(const System::UnicodeString aTopic, const System::UnicodeString aClientId);
	
public:
	void __fastcall Subscribe_ClientConnected(const System::UnicodeString aClientId);
	void __fastcall Subscribe_ClientDisconnected(const System::UnicodeString aClientId);
	void __fastcall Subscribe_ClientSubscribed(const System::UnicodeString aClientId);
	void __fastcall Subscribe_ClientUnSubscribed(const System::UnicodeString aClientId);
	void __fastcall Publish_Rule(const System::UnicodeString aRuleName, const System::UnicodeString aText);
	
protected:
	virtual void __fastcall DoSubscribe_ThingName(const System::UnicodeString aTopic, const System::UnicodeString aThingName);
	virtual void __fastcall DoPublish_ThingName(const System::UnicodeString aTopic, const System::UnicodeString aThingName, const System::UnicodeString aText);
	
public:
	void __fastcall Publish_DeleteShadow(const System::UnicodeString aThingName, const System::UnicodeString aText);
	void __fastcall Subscribe_DeleteShadow(const System::UnicodeString aThingName);
	void __fastcall Subscribe_ShadowDeleted(const System::UnicodeString aThingName);
	void __fastcall Subscribe_ShadowRejected(const System::UnicodeString aThingName);
	void __fastcall Publish_ShadowGet(const System::UnicodeString aThingName, const System::UnicodeString aText);
	void __fastcall Subscribe_ShadowGet(const System::UnicodeString aThingName);
	void __fastcall Subscribe_ShadowGetAccepted(const System::UnicodeString aThingName);
	void __fastcall Subscribe_ShadowGetRejected(const System::UnicodeString aThingName);
	void __fastcall Publish_ShadowUpdate(const System::UnicodeString aThingName, const System::UnicodeString aText);
	void __fastcall Subscribe_ShadowUpdateAccepted(const System::UnicodeString aThingName);
	void __fastcall Subscribe_ShadowUpdateRejected(const System::UnicodeString aThingName);
	void __fastcall Subscribe_ShadowUpdateDelta(const System::UnicodeString aThingName);
	void __fastcall Subscribe_ShadowUpdateDocuments(const System::UnicodeString aThingName);
	HIDESBASE void __fastcall Subscribe(const System::UnicodeString aTopic, TawsIoTQoS aQoS = (TawsIoTQoS)(0x0));
	HIDESBASE void __fastcall Publish(const System::UnicodeString aTopic, const System::UnicodeString aText, TawsIoTQoS aQoS = (TawsIoTQoS)(0x0));
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgciot_amazon_mqtt_client */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCIOT_AMAZON_MQTT_CLIENT)
using namespace Sgciot_amazon_mqtt_client;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgciot_amazon_mqtt_clientHPP
