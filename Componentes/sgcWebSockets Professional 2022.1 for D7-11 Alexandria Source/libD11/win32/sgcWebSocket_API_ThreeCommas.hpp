// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_API_ThreeCommas.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_api_threecommasHPP
#define Sgcwebsocket_api_threecommasHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <System.StrUtils.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcJSON.hpp>
#include <sgcHTTP_API_ThreeCommas.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_api_threecommas
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSThreeCommasLog_Options;
class DELPHICLASS TsgcWSThreeCommas_Options;
class DELPHICLASS TsgcWS_API_ThreeCommas;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSThreeCommasConnectEvent)(System::TObject* Sender, const System::UnicodeString aRawMessage);

typedef void __fastcall (__closure *TsgcWSThreeCommasSubscribedEvent)(System::TObject* Sender, const System::UnicodeString aChannel, const System::UnicodeString aRawMessage);

typedef void __fastcall (__closure *TsgcWSThreeCommasRejectSubscriptionEvent)(System::TObject* Sender, const System::UnicodeString aChannel, const System::UnicodeString aRawMessage);

typedef void __fastcall (__closure *TsgcWSThreeCommasMessageEvent)(System::TObject* Sender, const System::UnicodeString aType, const System::UnicodeString aRawMessage);

typedef void __fastcall (__closure *TsgcWSThreeCommasPingEvent)(System::TObject* Sender, const System::UnicodeString aRawMessage);

typedef void __fastcall (__closure *TsgcWSThreeCommasHTTPExceptionEvent)(System::TObject* Sender, System::Sysutils::Exception* E);

enum DECLSPEC_DENUM TsgcWSThreeCommasChannels : unsigned char { r3cmcSmartTradesChannel, r3cmDealsChannel };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSThreeCommasLog_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	System::UnicodeString FFileName;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property System::UnicodeString FileName = {read=FFileName, write=FFileName};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSThreeCommasLog_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSThreeCommasLog_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSThreeCommas_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FApiKey;
	System::UnicodeString FApiSecret;
	TsgcWSThreeCommasLog_Options* FHTTPLogOptions;
	void __fastcall SetHTTPLogOptions(TsgcWSThreeCommasLog_Options* const Value);
	
public:
	__fastcall virtual TsgcWSThreeCommas_Options();
	__fastcall virtual ~TsgcWSThreeCommas_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString ApiKey = {read=FApiKey, write=FApiKey};
	__property System::UnicodeString ApiSecret = {read=FApiSecret, write=FApiSecret};
	__property TsgcWSThreeCommasLog_Options* HTTPLogOptions = {read=FHTTPLogOptions, write=SetHTTPLogOptions};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWS_API_ThreeCommas : public Sgcwebsocket_classes::TsgcWSAPI_client
{
	typedef Sgcwebsocket_classes::TsgcWSAPI_client inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetURL();
	virtual void __fastcall DoBeforeConnect();
	virtual void __fastcall DoEventMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	
private:
	Sgcjson::TsgcJSON* FJSON;
	Sgcjson::TsgcJSON* __fastcall GetJSON();
	
protected:
	System::UnicodeString __fastcall GetJSONChannel();
	System::UnicodeString __fastcall GetJSONCode();
	System::UnicodeString __fastcall GetJSONMessage();
	virtual void __fastcall DoReadJSONType(System::UnicodeString aRawMessage);
	__property Sgcjson::TsgcJSON* JSON = {read=GetJSON, write=FJSON};
	
private:
	TsgcWSThreeCommas_Options* FThreeCommas;
	Sgchttp_api_threecommas::TsgcHTTP_API_3Commas_Rest* FREST_API;
	Sgchttp_api_threecommas::TsgcHTTP_API_3Commas_Rest* __fastcall GetREST_API();
	void __fastcall SetThreeCommas(TsgcWSThreeCommas_Options* const Value);
	
protected:
	virtual void __fastcall OnHTTPAPIExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	
public:
	__property TsgcWSThreeCommas_Options* ThreeCommas = {read=FThreeCommas, write=SetThreeCommas};
	__property Sgchttp_api_threecommas::TsgcHTTP_API_3Commas_Rest* REST_API = {read=GetREST_API, write=FREST_API};
	__fastcall virtual TsgcWS_API_ThreeCommas(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWS_API_ThreeCommas();
	
protected:
	virtual void __fastcall DoWriteData(const System::UnicodeString aText);
	virtual void __fastcall DoSubscribe(TsgcWSThreeCommasChannels aChannel);
	
public:
	void __fastcall SubscribeSmartTrades();
	void __fastcall SubscribeDeals();
	
private:
	TsgcWSThreeCommasMessageEvent FOnThreeCommasMessage;
	TsgcWSThreeCommasSubscribedEvent FOnThreeCommasConfirmSubscription;
	TsgcWSThreeCommasRejectSubscriptionEvent FOnThreeCommasRejectSubscription;
	TsgcWSThreeCommasPingEvent FOnThreeCommasPing;
	TsgcWSThreeCommasConnectEvent FOnThreeCommasConnect;
	
protected:
	virtual void __fastcall DoConfirmSubscriptionEvent(const System::UnicodeString aRawMessage);
	virtual void __fastcall DoRejectSubscriptionEvent(const System::UnicodeString aRawMessage);
	virtual void __fastcall DoPingEvent(const System::UnicodeString aRawMessage);
	virtual void __fastcall DoMessageEvent(const System::UnicodeString aType, const System::UnicodeString aRawMessage);
	virtual void __fastcall DoConnectEvent(const System::UnicodeString aRawMessage);
	
public:
	__property TsgcWSThreeCommasMessageEvent OnThreeCommasMessage = {read=FOnThreeCommasMessage, write=FOnThreeCommasMessage};
	__property TsgcWSThreeCommasSubscribedEvent OnThreeCommasConfirmSubscription = {read=FOnThreeCommasConfirmSubscription, write=FOnThreeCommasConfirmSubscription};
	__property TsgcWSThreeCommasRejectSubscriptionEvent OnThreeCommasRejectSubscription = {read=FOnThreeCommasRejectSubscription, write=FOnThreeCommasRejectSubscription};
	__property TsgcWSThreeCommasPingEvent OnThreeCommasPing = {read=FOnThreeCommasPing, write=FOnThreeCommasPing};
	__property TsgcWSThreeCommasConnectEvent OnThreeCommasConnect = {read=FOnThreeCommasConnect, write=FOnThreeCommasConnect};
	
private:
	TsgcWSThreeCommasHTTPExceptionEvent FOnThreeCommasHTTPException;
	
protected:
	virtual void __fastcall Do3CommasHTTPExceptionEvent(System::Sysutils::Exception* E);
	
public:
	__property TsgcWSThreeCommasHTTPExceptionEvent OnThreeCommasHTTPException = {read=FOnThreeCommasHTTPException, write=FOnThreeCommasHTTPException};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_api_threecommas */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_API_THREECOMMAS)
using namespace Sgcwebsocket_api_threecommas;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_api_threecommasHPP
