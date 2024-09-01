// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_API_Huobi.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_api_huobiHPP
#define Sgcwebsocket_api_huobiHPP

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
#include <sgcJSON.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_api_huobi
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSHuobi_Options;
class DELPHICLASS TsgcWS_API_Huobi;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSHuobiErrorEvent)(System::TObject* Sender, System::UnicodeString Id, System::UnicodeString Code, System::UnicodeString Msg, System::UnicodeString Ts);

typedef void __fastcall (__closure *TsgcWSHuobiSubscribedEvent)(System::TObject* Sender, System::UnicodeString Topic, System::UnicodeString Ts);

typedef void __fastcall (__closure *TsgcWSHuobiUnSubscribedEvent)(System::TObject* Sender, System::UnicodeString Topic, System::UnicodeString Ts);

typedef void __fastcall (__closure *TsgcWSHuobiUpdateEvent)(System::TObject* Sender, System::UnicodeString Data);

enum DECLSPEC_DENUM TsgcWSHuobiPeriods : unsigned char { hup1Min, hup5Min, hup15Min, hup30Min, hup60Min, hup1Day, hup1Mon, hup1Week, hup1Year };

enum DECLSPEC_DENUM TsgcWSHuobiDepths : unsigned char { hudStep0, hudStep1, hudStep2, hudStep3, hudStep4, hudStep5 };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSHuobi_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FKey;
	System::UnicodeString FSecret;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString Key = {read=FKey, write=FKey};
	__property System::UnicodeString Secret = {read=FSecret, write=FSecret};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSHuobi_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSHuobi_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWS_API_Huobi : public Sgcwebsocket_classes::TsgcWSAPI_client
{
	typedef Sgcwebsocket_classes::TsgcWSAPI_client inherited;
	
protected:
	virtual void __fastcall DoEventConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventBinary(Sgcwebsocket_classes::TsgcWSConnection* const aConnection, System::Classes::TMemoryStream* Data);
	virtual System::UnicodeString __fastcall GetURL();
	virtual bool __fastcall DoKeepAlive();
	
private:
	Sgcjson::TsgcJSON* FJSON;
	Sgcjson::TsgcJSON* __fastcall GetJSON();
	
protected:
	__property Sgcjson::TsgcJSON* JSON = {read=GetJSON, write=FJSON};
	
private:
	TsgcWSHuobi_Options* FHuobi;
	bool FAuthenticated;
	
public:
	__property TsgcWSHuobi_Options* Huobi = {read=FHuobi, write=FHuobi};
	__property bool Authenticated = {read=FAuthenticated, nodefault};
	__fastcall virtual TsgcWS_API_Huobi(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWS_API_Huobi();
	void __fastcall Ping();
	
private:
	System::UnicodeString __fastcall GetPeriod(const TsgcWSHuobiPeriods aPeriod);
	System::UnicodeString __fastcall GetDepth(const TsgcWSHuobiDepths aDepth);
	System::UnicodeString __fastcall DoDecodeGZIP(System::Classes::TStream* const aStream);
	System::UnicodeString __fastcall GetRandomId();
	void __fastcall DoWriteDataSubscription(const System::UnicodeString aMethod, const System::UnicodeString aTopic, const System::UnicodeString aId = System::UnicodeString());
	
protected:
	void __fastcall DoWriteData(const System::UnicodeString aText);
	virtual void __fastcall DoSubscribe(const System::UnicodeString aTopic, const System::UnicodeString aId = System::UnicodeString());
	virtual void __fastcall DoUnSubscribe(const System::UnicodeString aTopic, const System::UnicodeString aId = System::UnicodeString());
	
public:
	void __fastcall SubscribeKLine(const System::UnicodeString aSymbol, TsgcWSHuobiPeriods aPeriod, const System::UnicodeString aId = System::UnicodeString());
	void __fastcall UnSubscribeKLine(const System::UnicodeString aSymbol, TsgcWSHuobiPeriods aPeriod, const System::UnicodeString aId = System::UnicodeString());
	void __fastcall SubscribeMarketDepth(const System::UnicodeString aSymbol, TsgcWSHuobiDepths aDepth, const System::UnicodeString aId = System::UnicodeString());
	void __fastcall UnSubscribeMarketDepth(const System::UnicodeString aSymbol, TsgcWSHuobiDepths aDepth, const System::UnicodeString aId = System::UnicodeString());
	void __fastcall SubscribeTradeDetail(const System::UnicodeString aSymbol, const System::UnicodeString aId = System::UnicodeString());
	void __fastcall UnSubscribeTradeDetail(const System::UnicodeString aSymbol, const System::UnicodeString aId = System::UnicodeString());
	void __fastcall SubscribeMarketDetail(const System::UnicodeString aSymbol, const System::UnicodeString aId = System::UnicodeString());
	void __fastcall UnSubscribeMarketDetail(const System::UnicodeString aSymbol, const System::UnicodeString aId = System::UnicodeString());
	void __fastcall SubscribeMarketTickers(const System::UnicodeString aId = System::UnicodeString());
	void __fastcall UnSubscribeMarketTickers(const System::UnicodeString aId = System::UnicodeString());
	
private:
	void __fastcall DoAuthentication();
	void __fastcall DoWriteDataAuthentication(const System::UnicodeString aTopic, const System::UnicodeString aId = System::UnicodeString());
	
protected:
	virtual void __fastcall DoRequest(const System::UnicodeString aTopic, const System::UnicodeString aId = System::UnicodeString());
	
public:
	void __fastcall GetAccounts(const System::UnicodeString aId = System::UnicodeString());
	void __fastcall GetOrders(const System::UnicodeString aSymbol, const System::UnicodeString aId = System::UnicodeString());
	void __fastcall GetAccountsList(const System::UnicodeString aId = System::UnicodeString());
	void __fastcall GetOrdersList(const System::UnicodeString aId = System::UnicodeString());
	void __fastcall GetOrdersDetail(const System::UnicodeString aId = System::UnicodeString());
	
private:
	TsgcWSHuobiErrorEvent FOnHuobiError;
	TsgcWSHuobiSubscribedEvent FOnHuobiSubscribed;
	TsgcWSHuobiUnSubscribedEvent FOnHuobiUnSubscribed;
	TsgcWSHuobiUpdateEvent FOnHuobiUpdate;
	
protected:
	virtual void __fastcall DoEventHuobiSubscribed();
	virtual void __fastcall DoEventHuobiUnSubscribed();
	virtual void __fastcall DoEventHuobiUpdate(const System::UnicodeString aData);
	virtual void __fastcall DoEventHuobiError();
	
public:
	__property TsgcWSHuobiSubscribedEvent OnHuobiSubscribed = {read=FOnHuobiSubscribed, write=FOnHuobiSubscribed};
	__property TsgcWSHuobiUnSubscribedEvent OnHuobiUnSubscribed = {read=FOnHuobiUnSubscribed, write=FOnHuobiUnSubscribed};
	__property TsgcWSHuobiUpdateEvent OnHuobiUpdate = {read=FOnHuobiUpdate, write=FOnHuobiUpdate};
	__property TsgcWSHuobiErrorEvent OnHuobiError = {read=FOnHuobiError, write=FOnHuobiError};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_api_huobi */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_API_HUOBI)
using namespace Sgcwebsocket_api_huobi;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_api_huobiHPP
