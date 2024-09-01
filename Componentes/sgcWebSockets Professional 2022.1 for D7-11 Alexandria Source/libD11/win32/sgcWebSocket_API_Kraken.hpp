// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_API_Kraken.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_api_krakenHPP
#define Sgcwebsocket_api_krakenHPP

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
#include <sgcHTTP_API_Kraken.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_api_kraken
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSKrakenOrder;
class DELPHICLASS TsgcWSKrakenLog_Options;
class DELPHICLASS TsgcWSKraken_Options_Base;
class DELPHICLASS TsgcWSKraken_Options;
class DELPHICLASS TsgcWSKrakenFutures_Options;
class DELPHICLASS TsgcWS_API_Kraken_Base;
class DELPHICLASS TsgcWS_API_Kraken;
class DELPHICLASS TsgcWS_API_Kraken_Futures;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSKrakenHTTPExceptionEvent)(System::TObject* Sender, System::Sysutils::Exception* E);

enum DECLSPEC_DENUM TsgcWSKrakenInterval : unsigned char { kin1min, kin5min, kin15min, kin30min, kin60min, kin240min, kin1440min, kin10080min, kin21600min };

enum DECLSPEC_DENUM TsgcWSKrakenDepth : unsigned char { kde10, kde25, kde100, kde500, kde1000 };

enum DECLSPEC_DENUM TsgcWSKrakenOrderType : unsigned char { kotNone, kotMarket, kotLimit, kotStopLoss, kotTakeProfit, kotStopLossProfit, kotStopLossProfitLimit, kotStopLossLimit, kotTakeProfitLimit, kotTrailingStop, kotTrailingStopLimit, kotStopLossAndLimit };

enum DECLSPEC_DENUM TsgcWSKrakenOrderSide : unsigned char { kosNone, kosBuy, kosSell };

typedef void __fastcall (__closure *TsgcWSKrakenConnectEvent)(System::TObject* Sender, System::UnicodeString ConnectionId, System::UnicodeString Status, System::UnicodeString Version);

typedef void __fastcall (__closure *TsgcWSKrakenSystemStatusEvent)(System::TObject* Sender, System::UnicodeString ConnectionId, System::UnicodeString Status, System::UnicodeString Version);

typedef void __fastcall (__closure *TsgcWSKrakenSubscribedEvent)(System::TObject* Sender, int ChannelId, System::UnicodeString Pair, System::UnicodeString Subscription, System::UnicodeString ChannelName, int ReqID);

typedef void __fastcall (__closure *TsgcWSKrakenUnSubscribedEvent)(System::TObject* Sender, int ChannelId, System::UnicodeString Pair, System::UnicodeString Subscription, int ReqID);

typedef void __fastcall (__closure *TsgcWSKrakenSubscriptionError)(System::TObject* Sender, System::UnicodeString ErrorMessage, System::UnicodeString Pair, System::UnicodeString Subscription, int ReqID);

typedef void __fastcall (__closure *TsgcWSKrakenDataEvent)(System::TObject* Sender, System::UnicodeString aData);

typedef void __fastcall (__closure *TsgcWSKrakenFuturesConnectEvent)(System::TObject* Sender, System::UnicodeString Version);

typedef void __fastcall (__closure *TsgcWSKrakenFuturesSubscribedEvent)(System::TObject* Sender, System::UnicodeString Feed, System::UnicodeString ProductId);

typedef void __fastcall (__closure *TsgcWSKrakenFuturesUnSubscribedEvent)(System::TObject* Sender, System::UnicodeString Feed, System::UnicodeString ProductId);

typedef void __fastcall (__closure *TsgcWSKrakenFuturesErrorEvent)(System::TObject* Sender, System::UnicodeString Error);

class PASCALIMPLEMENTATION TsgcWSKrakenOrder : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TsgcWSKrakenOrderType FClose_OrderType;
	System::Currency FClose_Price;
	System::Currency FClose_Price2;
	System::UnicodeString FExpireTm;
	System::UnicodeString FLeverage;
	System::UnicodeString FOFlags;
	TsgcWSKrakenOrderType FOrderType;
	System::UnicodeString FPair;
	System::Currency FPrice;
	System::Currency FPrice2;
	System::UnicodeString FStartTm;
	System::UnicodeString FTradingAgreement;
	System::UnicodeString FUserRef;
	bool FValidate;
	System::Currency FVolume;
	TsgcWSKrakenOrderSide F_Type;
	
public:
	__fastcall virtual TsgcWSKrakenOrder();
	__property System::UnicodeString Pair = {read=FPair, write=FPair};
	__property TsgcWSKrakenOrderSide _Type = {read=F_Type, write=F_Type, nodefault};
	__property TsgcWSKrakenOrderType OrderType = {read=FOrderType, write=FOrderType, nodefault};
	__property System::Currency Price = {read=FPrice, write=FPrice};
	__property System::Currency Price2 = {read=FPrice2, write=FPrice2};
	__property System::Currency Volume = {read=FVolume, write=FVolume};
	__property System::UnicodeString Leverage = {read=FLeverage, write=FLeverage};
	__property System::UnicodeString OFlags = {read=FOFlags, write=FOFlags};
	__property System::UnicodeString StartTm = {read=FStartTm, write=FStartTm};
	__property System::UnicodeString ExpireTm = {read=FExpireTm, write=FExpireTm};
	__property System::UnicodeString UserRef = {read=FUserRef, write=FUserRef};
	__property bool Validate = {read=FValidate, write=FValidate, nodefault};
	__property TsgcWSKrakenOrderType Close_OrderType = {read=FClose_OrderType, write=FClose_OrderType, nodefault};
	__property System::Currency Close_Price = {read=FClose_Price, write=FClose_Price};
	__property System::Currency Close_Price2 = {read=FClose_Price2, write=FClose_Price2};
	__property System::UnicodeString TradingAgreement = {read=FTradingAgreement, write=FTradingAgreement};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcWSKrakenOrder() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSKrakenLog_Options : public System::Classes::TPersistent
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
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSKrakenLog_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSKrakenLog_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSKraken_Options_Base : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FApiKey;
	System::UnicodeString FApiSecret;
	TsgcWSKrakenLog_Options* FHTTPLogOptions;
	void __fastcall SetHTTPLogOptions(TsgcWSKrakenLog_Options* const Value);
	
public:
	__fastcall virtual TsgcWSKraken_Options_Base();
	__fastcall virtual ~TsgcWSKraken_Options_Base();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString ApiKey = {read=FApiKey, write=FApiKey};
	__property System::UnicodeString ApiSecret = {read=FApiSecret, write=FApiSecret};
	__property TsgcWSKrakenLog_Options* HTTPLogOptions = {read=FHTTPLogOptions, write=SetHTTPLogOptions};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSKraken_Options : public TsgcWSKraken_Options_Base
{
	typedef TsgcWSKraken_Options_Base inherited;
	
private:
	bool FBeta;
	
public:
	__fastcall virtual TsgcWSKraken_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Beta = {read=FBeta, write=FBeta, nodefault};
public:
	/* TsgcWSKraken_Options_Base.Destroy */ inline __fastcall virtual ~TsgcWSKraken_Options() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSKrakenFutures_Options : public TsgcWSKraken_Options_Base
{
	typedef TsgcWSKraken_Options_Base inherited;
	
private:
	bool FDemo;
	
public:
	__fastcall virtual TsgcWSKrakenFutures_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Demo = {read=FDemo, write=FDemo, nodefault};
public:
	/* TsgcWSKraken_Options_Base.Destroy */ inline __fastcall virtual ~TsgcWSKrakenFutures_Options() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWS_API_Kraken_Base : public Sgcwebsocket_classes::TsgcWSAPI_client
{
	typedef Sgcwebsocket_classes::TsgcWSAPI_client inherited;
	
private:
	System::UnicodeString FFormatCurrency;
	System::UnicodeString __fastcall GetCurrencyString(System::Currency aValue);
	
public:
	__property System::UnicodeString FormatCurrency = {read=FFormatCurrency, write=FFormatCurrency};
	
protected:
	virtual void __fastcall DoEventMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual bool __fastcall DoKeepAlive();
	
private:
	Sgcjson::TsgcJSON* FJSON;
	Sgcjson::TsgcJSON* __fastcall GetJSON();
	
protected:
	__property Sgcjson::TsgcJSON* JSON = {read=GetJSON, write=FJSON};
	
public:
	__fastcall virtual TsgcWS_API_Kraken_Base(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWS_API_Kraken_Base();
	
protected:
	virtual void __fastcall DoReadEvent(const System::UnicodeString aEvent, const System::UnicodeString aText) = 0 ;
	virtual void __fastcall DoWriteData(const System::UnicodeString aText);
	virtual void __fastcall DoPing();
	
public:
	void __fastcall Ping();
	
private:
	int __fastcall GetInterval(TsgcWSKrakenInterval aInterval);
	int __fastcall GetDepth(TsgcWSKrakenDepth aDepth);
	System::UnicodeString __fastcall GetOrderType(TsgcWSKrakenOrderType aOrderType);
	System::UnicodeString __fastcall GetOrderSide(TsgcWSKrakenOrderSide aOrderSide);
	TsgcWSKrakenHTTPExceptionEvent FOnKrakenHTTPException;
	
protected:
	virtual void __fastcall DoKrakenHTTPExceptionEvent(System::Sysutils::Exception* E);
	
public:
	__property TsgcWSKrakenHTTPExceptionEvent OnKrakenHTTPException = {read=FOnKrakenHTTPException, write=FOnKrakenHTTPException};
	
private:
	TsgcWSKrakenDataEvent FOnKrakenData;
	
protected:
	virtual void __fastcall DoEventKrakenData(System::UnicodeString aData);
	
public:
	__property TsgcWSKrakenDataEvent OnKrakenData = {read=FOnKrakenData, write=FOnKrakenData};
};


class PASCALIMPLEMENTATION TsgcWS_API_Kraken : public TsgcWS_API_Kraken_Base
{
	typedef TsgcWS_API_Kraken_Base inherited;
	
protected:
	virtual void __fastcall DoBeforeConnect();
	virtual void __fastcall DoEventConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int Code);
	virtual System::UnicodeString __fastcall GetURL();
	virtual void __fastcall DoReadEvent(const System::UnicodeString aEvent, const System::UnicodeString aText);
	
public:
	__fastcall virtual TsgcWS_API_Kraken(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWS_API_Kraken();
	
private:
	TsgcWSKraken_Options* FKraken;
	Sgchttp_api_kraken::TsgcHTTP_API_Kraken_Rest* FREST_API;
	Sgchttp_api_kraken::TsgcHTTP_API_Kraken_Rest* __fastcall GetREST_API();
	void __fastcall SetKraken(TsgcWSKraken_Options* const Value);
	
protected:
	virtual void __fastcall OnHTTPAPIExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	
public:
	__property TsgcWSKraken_Options* Kraken = {read=FKraken, write=SetKraken};
	__property Sgchttp_api_kraken::TsgcHTTP_API_Kraken_Rest* REST_API = {read=GetREST_API, write=FREST_API};
	
private:
	bool FFirstStatus;
	
protected:
	virtual void __fastcall DoSubscription(const System::UnicodeString aEvent, const System::TVarRec *aPairs, const int aPairs_High, const System::UnicodeString aSubscription, int aInterval, int aDepth, int aReqId);
	
public:
	void __fastcall SubscribeTicker(const System::TVarRec *aPairs, const int aPairs_High, int aReqId = 0x0);
	void __fastcall UnSubscribeTicker(const System::TVarRec *aPairs, const int aPairs_High, int aReqId = 0x0);
	void __fastcall SubscribeOHLC(const System::TVarRec *aPairs, const int aPairs_High, TsgcWSKrakenInterval aInterval, int aReqId = 0x0);
	void __fastcall UnSubscribeOHLC(const System::TVarRec *aPairs, const int aPairs_High, TsgcWSKrakenInterval aInterval, int aReqId = 0x0);
	void __fastcall SubscribeTrade(const System::TVarRec *aPairs, const int aPairs_High, int aReqId = 0x0);
	void __fastcall UnSubscribeTrade(const System::TVarRec *aPairs, const int aPairs_High, int aReqId = 0x0);
	void __fastcall SubscribeBook(const System::TVarRec *aPairs, const int aPairs_High, TsgcWSKrakenDepth aDepth, int aReqId = 0x0);
	void __fastcall UnSubscribeBook(const System::TVarRec *aPairs, const int aPairs_High, TsgcWSKrakenDepth aDepth, int aReqId = 0x0);
	void __fastcall SubscribeSpread(const System::TVarRec *aPairs, const int aPairs_High, int aReqId = 0x0);
	void __fastcall UnSubscribeSpread(const System::TVarRec *aPairs, const int aPairs_High, int aReqId = 0x0);
	void __fastcall SubscribeAll(const System::TVarRec *aPairs, const int aPairs_High, int aReqId = 0x0);
	void __fastcall UnSubscribeAll(const System::TVarRec *aPairs, const int aPairs_High, int aReqId = 0x0);
	void __fastcall UnSubscribe(int aChannelId);
	
private:
	System::UnicodeString FWebSocketsToken;
	System::UnicodeString __fastcall GetWebSocketsToken();
	
protected:
	virtual void __fastcall DoPrivateSubscription(const System::UnicodeString aEvent, const System::UnicodeString aSubscription, int aReqId);
	
public:
	void __fastcall SubscribeOwnTrades(int aReqId = 0x0);
	void __fastcall UnSubscribeOwnTrades(int aReqId = 0x0);
	void __fastcall SubscribeOpenOrders(int aReqId = 0x0);
	void __fastcall UnSubscribeOpenOrders(int aReqId = 0x0);
	void __fastcall AddOrder(TsgcWSKrakenOrder* aOrder, int aReqId = 0x0);
	void __fastcall CancelOrder(const System::UnicodeString aOrderId, int aReqId = 0x0);
	
private:
	TsgcWSKrakenSystemStatusEvent FOnKrakenSystemStatus;
	TsgcWSKrakenConnectEvent FOnKrakenConnect;
	TsgcWSKrakenSubscribedEvent FOnKrakenSubscribed;
	TsgcWSKrakenSubscriptionError FOnKrakenSubscriptionError;
	TsgcWSKrakenUnSubscribedEvent FOnKrakenUnSubscribed;
	
protected:
	virtual void __fastcall DoEventKrakenSystemStatus(System::UnicodeString aConnectionId, System::UnicodeString aStatus, System::UnicodeString aVersion);
	virtual void __fastcall DoEventKrakenConnect(System::UnicodeString aConnectionId, System::UnicodeString aStatus, System::UnicodeString aVersion);
	virtual void __fastcall DoEventKrakenSubscribed(int aChannelId, System::UnicodeString aPair, System::UnicodeString aSubscription, System::UnicodeString aChannelName, int aReqId);
	virtual void __fastcall DoEventKrakenUnSubscribed(int aChannelId, System::UnicodeString aPair, System::UnicodeString aSubscription, int aReqId);
	virtual void __fastcall DoEventKrakenSubscriptionError(System::UnicodeString aErrorMessage, System::UnicodeString aPair, System::UnicodeString aSubscription, int aReqId);
	
public:
	__property TsgcWSKrakenSystemStatusEvent OnKrakenSystemStatus = {read=FOnKrakenSystemStatus, write=FOnKrakenSystemStatus};
	__property TsgcWSKrakenConnectEvent OnKrakenConnect = {read=FOnKrakenConnect, write=FOnKrakenConnect};
	__property TsgcWSKrakenSubscribedEvent OnKrakenSubscribed = {read=FOnKrakenSubscribed, write=FOnKrakenSubscribed};
	__property TsgcWSKrakenUnSubscribedEvent OnKrakenUnSubscribed = {read=FOnKrakenUnSubscribed, write=FOnKrakenUnSubscribed};
	__property TsgcWSKrakenSubscriptionError OnKrakenSubscriptionError = {read=FOnKrakenSubscriptionError, write=FOnKrakenSubscriptionError};
};


class PASCALIMPLEMENTATION TsgcWS_API_Kraken_Futures : public TsgcWS_API_Kraken_Base
{
	typedef TsgcWS_API_Kraken_Base inherited;
	
protected:
	virtual void __fastcall DoEventConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection, int Code);
	virtual System::UnicodeString __fastcall GetURL();
	virtual void __fastcall DoReadEvent(const System::UnicodeString aEvent, const System::UnicodeString aText);
	
public:
	__fastcall virtual TsgcWS_API_Kraken_Futures(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWS_API_Kraken_Futures();
	
private:
	TsgcWSKrakenFutures_Options* FKraken;
	Sgchttp_api_kraken::TsgcHTTP_API_Kraken_Futures_Rest* FREST_API;
	Sgchttp_api_kraken::TsgcHTTP_API_Kraken_Futures_Rest* __fastcall GetREST_API();
	void __fastcall SetKraken(TsgcWSKrakenFutures_Options* const Value);
	
protected:
	virtual void __fastcall OnHTTPAPIExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	
public:
	__property TsgcWSKrakenFutures_Options* Kraken = {read=FKraken, write=SetKraken};
	__property Sgchttp_api_kraken::TsgcHTTP_API_Kraken_Futures_Rest* REST_API = {read=GetREST_API, write=FREST_API};
	
private:
	void __fastcall DoRequestChallenge();
	
protected:
	virtual void __fastcall DoSubscription(const System::UnicodeString aEvent, const System::UnicodeString aFeed, const System::TVarRec *aProductIds, const int aProductIds_High);
	
public:
	void __fastcall SubscribeTicker(const System::TVarRec *aProductIds, const int aProductIds_High);
	void __fastcall UnSubscribeTicker(const System::TVarRec *aProductIds, const int aProductIds_High);
	void __fastcall SubscribeTrade(const System::TVarRec *aProductIds, const int aProductIds_High);
	void __fastcall UnSubscribeTrade(const System::TVarRec *aProductIds, const int aProductIds_High);
	void __fastcall SubscribeHeartbeat();
	void __fastcall UnSubscribeHeartbeat();
	void __fastcall SubscribeTickerLite(const System::TVarRec *aProductIds, const int aProductIds_High);
	void __fastcall UnSubscribeTickerLite(const System::TVarRec *aProductIds, const int aProductIds_High);
	void __fastcall SubscribeBook(const System::TVarRec *aProductIds, const int aProductIds_High);
	void __fastcall UnSubscribeBook(const System::TVarRec *aProductIds, const int aProductIds_High);
	
private:
	System::UnicodeString FChallenge;
	System::UnicodeString __fastcall GetSignedChallenge();
	
protected:
	virtual void __fastcall DoSubscriptionSigned(const System::UnicodeString aEvent, const System::UnicodeString aFeed, const System::TVarRec *aProductIds, const int aProductIds_High);
	
public:
	void __fastcall SubscribeOpenOrdersVerbose();
	void __fastcall UnSubscribeOpenOrdersVerbose();
	void __fastcall SubscribeOpenPositions();
	void __fastcall UnSubscribeOpenPositions();
	void __fastcall SubscribeAccountLog();
	void __fastcall UnSubscribeAccountLog();
	void __fastcall SubscribeFills();
	void __fastcall UnSubscribeFills();
	void __fastcall SubscribeOpenOrders();
	void __fastcall UnSubscribeOpenOrders();
	void __fastcall SubscribeAccountBalanceAndMargins();
	void __fastcall UnSubscribeAccountBalanceAndMargins();
	void __fastcall SubscribeNotifications();
	void __fastcall UnSubscribeNotifications();
	
private:
	TsgcWSKrakenFuturesConnectEvent FOnKrakenFuturesConnect;
	TsgcWSKrakenFuturesSubscribedEvent FOnKrakenFuturesSubscribed;
	TsgcWSKrakenFuturesErrorEvent FOnKrakenFuturesError;
	TsgcWSKrakenFuturesUnSubscribedEvent FOnKrakenFuturesUnSubscribed;
	
protected:
	virtual void __fastcall DoEventKrakenFuturesConnect(const System::UnicodeString aVersion);
	virtual void __fastcall DoEventKrakenFuturesSubscribed(const System::UnicodeString aFeed, System::UnicodeString aProductId);
	virtual void __fastcall DoEventKrakenFuturesUnSubscribed(const System::UnicodeString aFeed, System::UnicodeString aProductId);
	virtual void __fastcall DoEventKrakenFuturesError(const System::UnicodeString aError);
	
public:
	__property TsgcWSKrakenFuturesConnectEvent OnKrakenFuturesConnect = {read=FOnKrakenFuturesConnect, write=FOnKrakenFuturesConnect};
	__property TsgcWSKrakenFuturesSubscribedEvent OnKrakenFuturesSubscribed = {read=FOnKrakenFuturesSubscribed, write=FOnKrakenFuturesSubscribed};
	__property TsgcWSKrakenFuturesErrorEvent OnKrakenFuturesError = {read=FOnKrakenFuturesError, write=FOnKrakenFuturesError};
	__property TsgcWSKrakenFuturesUnSubscribedEvent OnKrakenFuturesUnSubscribed = {read=FOnKrakenFuturesUnSubscribed, write=FOnKrakenFuturesUnSubscribed};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_api_kraken */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_API_KRAKEN)
using namespace Sgcwebsocket_api_kraken;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_api_krakenHPP
