// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_API_Bitmex.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_api_bitmexHPP
#define Sgcwebsocket_api_bitmexHPP

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
#include <sgcHTTP_API_Bitmex.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_api_bitmex
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSBitmexLog_Options;
class DELPHICLASS TsgcWSBitmex_Options;
class DELPHICLASS TsgcWS_API_Bitmex;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TwsBitmexTopics : unsigned char { btmAnnouncement, btmChat, btmConnected, btmFunding, btmInstrument, btmInsurance, btmLiquidation, btmOrderBookL2_25, btmOrderBookL2, btmOrderBook10, btmPublicNotifications, btmQuote, btmQuoteBin1m, btmQuoteBin5m, btmQuoteBin1h, btmQuoteBin1d, btmSettlement, btmTrade, btmTradeBin1m, btmTradeBin5m, btmTradeBin1h, btmTradeBin1d, btmAffiliate, btmExecution, btmOrder, btmMargin, btmPosition, btmPrivateNotifications, btmTransact, btmWallet };

typedef void __fastcall (__closure *TsgcWSBitmexConnectEvent)(System::TObject* Sender, const System::UnicodeString aMessage);

typedef void __fastcall (__closure *TsgcWSBitmexSubscribedEvent)(System::TObject* Sender, const System::UnicodeString aChannel);

typedef void __fastcall (__closure *TsgcWSBitmexUnsubscribedEvent)(System::TObject* Sender, const System::UnicodeString aChannel);

typedef void __fastcall (__closure *TsgcWSBitmexMessageEvent)(System::TObject* Sender, const TwsBitmexTopics aTopic, const System::UnicodeString aMessage);

typedef void __fastcall (__closure *TsgcWSBitmexErrorEvent)(System::TObject* Sender, const System::UnicodeString aMessage);

typedef void __fastcall (__closure *TsgcWSBitmexAuthenticatedEvent)(System::TObject* Sender);

typedef void __fastcall (__closure *TsgcWSBitmexHTTPExceptionEvent)(System::TObject* Sender, System::Sysutils::Exception* E);

typedef System::StaticArray<System::UnicodeString, 30> Sgcwebsocket_api_bitmex__1;

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSBitmexLog_Options : public System::Classes::TPersistent
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
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSBitmexLog_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSBitmexLog_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSBitmex_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FApiKey;
	System::UnicodeString FApiSecret;
	TsgcWSBitmexLog_Options* FHTTPLogOptions;
	bool FTestNet;
	void __fastcall SetHTTPLogOptions(TsgcWSBitmexLog_Options* const Value);
	
public:
	__fastcall virtual TsgcWSBitmex_Options();
	__fastcall virtual ~TsgcWSBitmex_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString ApiKey = {read=FApiKey, write=FApiKey};
	__property System::UnicodeString ApiSecret = {read=FApiSecret, write=FApiSecret};
	__property TsgcWSBitmexLog_Options* HTTPLogOptions = {read=FHTTPLogOptions, write=SetHTTPLogOptions};
	__property bool TestNet = {read=FTestNet, write=FTestNet, nodefault};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWS_API_Bitmex : public Sgcwebsocket_classes::TsgcWSAPI_client
{
	typedef Sgcwebsocket_classes::TsgcWSAPI_client inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetURL();
	virtual void __fastcall DoEventMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual bool __fastcall DoKeepAlive();
	
private:
	Sgcjson::TsgcJSON* FJSON;
	Sgcjson::TsgcJSON* __fastcall GetJSON();
	
protected:
	__property Sgcjson::TsgcJSON* JSON = {read=GetJSON, write=FJSON};
	
private:
	TsgcWSBitmex_Options* FBitmex;
	Sgchttp_api_bitmex::TsgcHTTP_API_Bitmex_Rest* FREST_API;
	void __fastcall SetBitmex(TsgcWSBitmex_Options* const Value);
	Sgchttp_api_bitmex::TsgcHTTP_API_Bitmex_Rest* __fastcall GetREST_API();
	
protected:
	virtual void __fastcall OnHTTPAPIExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	
public:
	__property TsgcWSBitmex_Options* Bitmex = {read=FBitmex, write=SetBitmex};
	__property Sgchttp_api_bitmex::TsgcHTTP_API_Bitmex_Rest* REST_API = {read=GetREST_API, write=FREST_API};
	__fastcall virtual TsgcWS_API_Bitmex(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWS_API_Bitmex();
	
private:
	int __fastcall GetTimeStamp();
	bool __fastcall IsSymbolTopic(TwsBitmexTopics aTopic);
	System::UnicodeString __fastcall GetTopicString(TwsBitmexTopics aTopic);
	TwsBitmexTopics __fastcall GetTopicFromString(System::UnicodeString aTopic);
	
protected:
	virtual void __fastcall DoWriteData(const System::UnicodeString aText);
	virtual void __fastcall DoPing();
	virtual void __fastcall DoRequest(const System::UnicodeString aMethod, TwsBitmexTopics aTopic, const System::UnicodeString aSymbol);
	
public:
	void __fastcall Subscribe(TwsBitmexTopics aTopic, const System::UnicodeString aSymbol = System::UnicodeString());
	void __fastcall Unsubscribe(TwsBitmexTopics aTopic, const System::UnicodeString aSymbol = System::UnicodeString());
	void __fastcall Authenticate();
	
private:
	TsgcWSBitmexAuthenticatedEvent FOnBitmexAuthenticated;
	TsgcWSBitmexConnectEvent FOnBitmexConnect;
	TsgcWSBitmexErrorEvent FOnBitmexError;
	TsgcWSBitmexMessageEvent FOnBitmexMessage;
	TsgcWSBitmexSubscribedEvent FOnBitmexSubscribed;
	TsgcWSBitmexUnsubscribedEvent FOnBitmexUnsubscribed;
	
protected:
	virtual void __fastcall DoBitmexConnectEvent(const System::UnicodeString aMessage);
	virtual void __fastcall DoBitmexSubscribedEvent(const System::UnicodeString aChannel);
	virtual void __fastcall DoBitmexUnsubscribedEvent(const System::UnicodeString aChannel);
	virtual void __fastcall DoBitmexMessageEvent(const System::UnicodeString aTopic, const System::UnicodeString aMessage);
	virtual void __fastcall DoBitmexErrorEvent(const System::UnicodeString aMessage);
	virtual void __fastcall DoBitmexAuthenticatedEvent();
	
public:
	__property TsgcWSBitmexAuthenticatedEvent OnBitmexAuthenticated = {read=FOnBitmexAuthenticated, write=FOnBitmexAuthenticated};
	__property TsgcWSBitmexConnectEvent OnBitmexConnect = {read=FOnBitmexConnect, write=FOnBitmexConnect};
	__property TsgcWSBitmexErrorEvent OnBitmexError = {read=FOnBitmexError, write=FOnBitmexError};
	__property TsgcWSBitmexMessageEvent OnBitmexMessage = {read=FOnBitmexMessage, write=FOnBitmexMessage};
	__property TsgcWSBitmexSubscribedEvent OnBitmexSubscribed = {read=FOnBitmexSubscribed, write=FOnBitmexSubscribed};
	__property TsgcWSBitmexUnsubscribedEvent OnBitmexUnsubscribed = {read=FOnBitmexUnsubscribed, write=FOnBitmexUnsubscribed};
	
private:
	TsgcWSBitmexHTTPExceptionEvent FOnBitmexHTTPException;
	
protected:
	virtual void __fastcall DoBitmexHTTPExceptionEvent(System::Sysutils::Exception* E);
	
public:
	__property TsgcWSBitmexHTTPExceptionEvent OnBitmexHTTPException = {read=FOnBitmexHTTPException, write=FOnBitmexHTTPException};
};


//-- var, const, procedure ---------------------------------------------------
#define CS_BITMEX_ANNOUNCEMENT L"announcement"
#define CS_BITMEX_CHAT L"chat"
#define CS_BITMEX_CONNECTED L"connected"
#define CS_BITMEX_FUNDING L"funding"
#define CS_BITMEX_INSTRUMENT L"instrument"
#define CS_BITMEX_INSURANCE L"insurance"
#define CS_BITMEX_LIQUIDATION L"liquidation"
#define CS_BITMEX_ORDERBOOKL2_25 L"orderBookL2_25"
#define CS_BITMEX_ORDERBOOKL2 L"orderBookL2"
#define CS_BITMEX_ORDERBOOK10 L"orderBook10"
#define CS_BITMEX_PUBLICNOTIFICATIONS L"publicNotifications"
#define CS_BITMEX_QUOTE L"quote"
#define CS_BITMEX_QUOTEBIN1M L"quoteBin1m"
#define CS_BITMEX_QUOTEBIN5M L"quoteBin5m"
#define CS_BITMEX_QUOTEBIN1H L"quoteBin1h"
#define CS_BITMEX_QUOTEBIN1D L"quoteBin1d"
#define CS_BITMEX_SETTLEMENT L"settlement"
#define CS_BITMEX_TRADE L"trade"
#define CS_BITMEX_TRADEBIN1M L"tradeBin1m"
#define CS_BITMEX_TRADEBIN5M L"tradeBin5m"
#define CS_BITMEX_TRADEBIN1H L"tradeBin1h"
#define CS_BITMEX_TRADEBIN1D L"tradeBin1d"
#define CS_BITMEX_AFFILIATE L"affiliate"
#define CS_BITMEX_EXECUTION L"execution"
#define CS_BITMEX_ORDER L"order"
#define CS_BITMEX_MARGIN L"margin"
#define CS_BITMEX_POSITION L"position"
#define CS_BITMEX_PRIVATENOTIFICATIONS L"privateNotifications"
#define CS_BITMEX_TRANSACT L"transact"
#define CS_BITMEX_WALLET L"wallet"
extern DELPHI_PACKAGE Sgcwebsocket_api_bitmex__1 TwsBitmexTopics_String;
}	/* namespace Sgcwebsocket_api_bitmex */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_API_BITMEX)
using namespace Sgcwebsocket_api_bitmex;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_api_bitmexHPP
