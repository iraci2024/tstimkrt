// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcHTTP_API_Kraken.pas' rev: 35.00 (Windows)

#ifndef Sgchttp_api_krakenHPP
#define Sgchttp_api_krakenHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <sgcHTTP_API.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgchttp_api_kraken
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcHTTPKrakenOrder;
class DELPHICLASS TsgcHTTPKrakenFuturesOrder;
class DELPHICLASS TsgcHTTPKrakenLog_Options;
class DELPHICLASS TsgcHTTPKraken_Options;
class DELPHICLASS TsgcHTTPKrakenFutures_Options;
class DELPHICLASS TsgcHTTP_API_Kraken;
class DELPHICLASS TsgcHTTP_API_Kraken_Rest;
class DELPHICLASS TsgcHTTP_API_Kraken_Futures_Rest;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TsgcHTTPKrakenInterval : unsigned char { kinh1min, kinh5min, kinh15min, kinh30min, kinh60min, kinh240min, kinh1440min, kinh10080min, kinh21600min };

enum DECLSPEC_DENUM TsgcHTTPKrakenOrderType : unsigned char { kothNone, kothMarket, kothLimit, kothStopLoss, kothTakeProfit, kothStopLossProfit, kothStopLossProfitLimit, kothStopLossLimit, kothTakeProfitLimit, kothTrailingStop, kothTrailingStopLimit, kothStopLossAndLimit };

enum DECLSPEC_DENUM TsgcHTTPKrakenOrderSide : unsigned char { kosNone, koshBuy, koshSell };

class PASCALIMPLEMENTATION TsgcHTTPKrakenOrder : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TsgcHTTPKrakenOrderType FClose_OrderType;
	System::Extended FClose_Price;
	System::Extended FClose_Price2;
	System::UnicodeString FExpireTm;
	System::UnicodeString FLeverage;
	System::UnicodeString FOFlags;
	TsgcHTTPKrakenOrderType FOrderType;
	System::UnicodeString FPair;
	System::Extended FPrice;
	System::Extended FPrice2;
	System::UnicodeString FStartTm;
	System::UnicodeString FUserRef;
	bool FValidate;
	System::Extended FVolume;
	TsgcHTTPKrakenOrderSide F_Type;
	
public:
	__fastcall virtual TsgcHTTPKrakenOrder();
	__property System::UnicodeString Pair = {read=FPair, write=FPair};
	__property TsgcHTTPKrakenOrderSide _Type = {read=F_Type, write=F_Type, nodefault};
	__property TsgcHTTPKrakenOrderType OrderType = {read=FOrderType, write=FOrderType, nodefault};
	__property System::Extended Price = {read=FPrice, write=FPrice};
	__property System::Extended Price2 = {read=FPrice2, write=FPrice2};
	__property System::Extended Volume = {read=FVolume, write=FVolume};
	__property System::UnicodeString Leverage = {read=FLeverage, write=FLeverage};
	__property System::UnicodeString OFlags = {read=FOFlags, write=FOFlags};
	__property System::UnicodeString StartTm = {read=FStartTm, write=FStartTm};
	__property System::UnicodeString ExpireTm = {read=FExpireTm, write=FExpireTm};
	__property System::UnicodeString UserRef = {read=FUserRef, write=FUserRef};
	__property bool Validate = {read=FValidate, write=FValidate, nodefault};
	__property TsgcHTTPKrakenOrderType Close_OrderType = {read=FClose_OrderType, write=FClose_OrderType, nodefault};
	__property System::Extended Close_Price = {read=FClose_Price, write=FClose_Price};
	__property System::Extended Close_Price2 = {read=FClose_Price2, write=FClose_Price2};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcHTTPKrakenOrder() { }
	
};


enum DECLSPEC_DENUM TsgcHTTPKrakenFuturesOrderType : unsigned char { kotfNone, kotfLMT, kotfPOST, kotfMKT, kotfSTP, kotfTAKE_PROFIT, kotfIOC };

enum DECLSPEC_DENUM TsgcHTTPKrakenFuturesOrderSide : unsigned char { kosfNone, kosfBuy, kosfSell };

enum DECLSPEC_DENUM TsgcHTTPKrakenFuturesOrderTriggerSignal : unsigned char { kotsNone, kotsMark, kotsIndex, kotsLast };

class PASCALIMPLEMENTATION TsgcHTTPKrakenFuturesOrder : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FCliOrderId;
	double FLimitPrice;
	TsgcHTTPKrakenFuturesOrderType FOrderType;
	bool FReduceOnly;
	TsgcHTTPKrakenFuturesOrderSide FSide;
	int FSize;
	double FStopPrice;
	System::UnicodeString FSymbol;
	TsgcHTTPKrakenFuturesOrderTriggerSignal FTriggerSignal;
	
public:
	__property System::UnicodeString CliOrderId = {read=FCliOrderId, write=FCliOrderId};
	__property double LimitPrice = {read=FLimitPrice, write=FLimitPrice};
	__property TsgcHTTPKrakenFuturesOrderType OrderType = {read=FOrderType, write=FOrderType, nodefault};
	__property bool ReduceOnly = {read=FReduceOnly, write=FReduceOnly, nodefault};
	__property TsgcHTTPKrakenFuturesOrderSide Side = {read=FSide, write=FSide, nodefault};
	__property int Size = {read=FSize, write=FSize, nodefault};
	__property double StopPrice = {read=FStopPrice, write=FStopPrice};
	__property System::UnicodeString Symbol = {read=FSymbol, write=FSymbol};
	__property TsgcHTTPKrakenFuturesOrderTriggerSignal TriggerSignal = {read=FTriggerSignal, write=FTriggerSignal, nodefault};
public:
	/* TObject.Create */ inline __fastcall TsgcHTTPKrakenFuturesOrder() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcHTTPKrakenFuturesOrder() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTPKrakenLog_Options : public System::Classes::TPersistent
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
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcHTTPKrakenLog_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcHTTPKrakenLog_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTPKraken_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FApiKey;
	System::UnicodeString FApiSecret;
	TsgcHTTPKrakenLog_Options* FLogOptions;
	void __fastcall SetLogOptions(TsgcHTTPKrakenLog_Options* const Value);
	
public:
	__fastcall virtual TsgcHTTPKraken_Options();
	__fastcall virtual ~TsgcHTTPKraken_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString ApiKey = {read=FApiKey, write=FApiKey};
	__property System::UnicodeString ApiSecret = {read=FApiSecret, write=FApiSecret};
	__property TsgcHTTPKrakenLog_Options* LogOptions = {read=FLogOptions, write=SetLogOptions};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTPKrakenFutures_Options : public TsgcHTTPKraken_Options
{
	typedef TsgcHTTPKraken_Options inherited;
	
private:
	bool FDemo;
	
public:
	__fastcall virtual TsgcHTTPKrakenFutures_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Demo = {read=FDemo, write=FDemo, nodefault};
public:
	/* TsgcHTTPKraken_Options.Destroy */ inline __fastcall virtual ~TsgcHTTPKrakenFutures_Options() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcHTTP_API_Kraken : public Sgchttp_api::TsgcHTTPAPI_client
{
	typedef Sgchttp_api::TsgcHTTPAPI_client inherited;
	
private:
	System::UnicodeString FFormatExtended;
	System::UnicodeString __fastcall GetFormatExtended(System::Extended aValue);
	
public:
	__property System::UnicodeString FormatExtended = {read=FFormatExtended, write=FFormatExtended};
	
private:
	virtual void __fastcall DoInitializeLog() = 0 ;
	
protected:
	virtual System::UnicodeString __fastcall GetEndpoint(const System::UnicodeString aMethod) = 0 ;
	virtual System::UnicodeString __fastcall DoHTTP_GET(const System::UnicodeString aMethod, const System::UnicodeString aParameters = System::UnicodeString(), System::Classes::TStrings* const aHeaders = (System::Classes::TStrings*)(0x0));
	virtual System::UnicodeString __fastcall DoHTTP_POST(const System::UnicodeString aMethod, const System::UnicodeString aBody = System::UnicodeString(), const System::UnicodeString aParameters = System::UnicodeString(), System::Classes::TStrings* const aHeaders = (System::Classes::TStrings*)(0x0));
public:
	/* TsgcHTTPAPI_client.Create */ inline __fastcall virtual TsgcHTTP_API_Kraken(System::Classes::TComponent* aOwner) : Sgchttp_api::TsgcHTTPAPI_client(aOwner) { }
	/* TsgcHTTPAPI_client.Destroy */ inline __fastcall virtual ~TsgcHTTP_API_Kraken() { }
	
};


class PASCALIMPLEMENTATION TsgcHTTP_API_Kraken_Rest : public TsgcHTTP_API_Kraken
{
	typedef TsgcHTTP_API_Kraken inherited;
	
private:
	int __fastcall GetInterval(TsgcHTTPKrakenInterval aInterval);
	
protected:
	virtual void __fastcall DoInitializeLog();
	virtual System::UnicodeString __fastcall GetEndpoint(const System::UnicodeString aMethod);
	virtual System::UnicodeString __fastcall DoHTTP_POST_PRIVATE(const System::UnicodeString aMethod, const System::UnicodeString aBody = System::UnicodeString(), const System::UnicodeString aParameters = System::UnicodeString());
	
private:
	TsgcHTTPKraken_Options* FKrakenOptions;
	void __fastcall SetKrakenOptions(TsgcHTTPKraken_Options* const Value);
	
public:
	__property TsgcHTTPKraken_Options* KrakenOptions = {read=FKrakenOptions, write=SetKrakenOptions};
	__fastcall virtual TsgcHTTP_API_Kraken_Rest(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcHTTP_API_Kraken_Rest();
	System::UnicodeString __fastcall GetWebSocketsToken();
	System::UnicodeString __fastcall GetServerTime();
	System::UnicodeString __fastcall GetAssets(const System::UnicodeString aInfo = System::UnicodeString(), const System::UnicodeString aClass = System::UnicodeString(), const System::UnicodeString aAsset = System::UnicodeString());
	System::UnicodeString __fastcall GetAssetPairs(const System::UnicodeString *aPairs, const int aPairs_High, const System::UnicodeString aInfo = System::UnicodeString(), const System::UnicodeString aLeverage = System::UnicodeString(), const System::UnicodeString aFees = System::UnicodeString(), const System::UnicodeString aMargin = System::UnicodeString());
	System::UnicodeString __fastcall GetTicker(const System::UnicodeString *aPairs, const int aPairs_High);
	System::UnicodeString __fastcall GetOHLC(const System::UnicodeString aPair, TsgcHTTPKrakenInterval aInterval = (TsgcHTTPKrakenInterval)(0x0), System::UnicodeString aSince = System::UnicodeString());
	System::UnicodeString __fastcall GetOrderBook(const System::UnicodeString aPair, int aCount = 0x0);
	System::UnicodeString __fastcall GetTrades(const System::UnicodeString aPair, System::UnicodeString aSince = System::UnicodeString());
	System::UnicodeString __fastcall GetSpread(const System::UnicodeString aPair, System::UnicodeString aSince = System::UnicodeString());
	System::UnicodeString __fastcall GetAccountBalance();
	System::UnicodeString __fastcall GetTradeBalance(const System::UnicodeString aClass = System::UnicodeString(), const System::UnicodeString aAsset = System::UnicodeString());
	System::UnicodeString __fastcall GetOpenOrders(const bool aTrades = false, const System::UnicodeString aUserRef = System::UnicodeString());
	System::UnicodeString __fastcall GetClosedOrders(const bool aTrades = false, const System::UnicodeString aUserRef = System::UnicodeString(), const System::UnicodeString aStart = System::UnicodeString(), const System::UnicodeString aEnd = System::UnicodeString(), const System::UnicodeString aOfs = System::UnicodeString(), const System::UnicodeString aCloseTime = System::UnicodeString());
	System::UnicodeString __fastcall QueryOrders(const System::UnicodeString aTxId, const bool aTrades = false, const System::UnicodeString aUserRef = System::UnicodeString());
	System::UnicodeString __fastcall GetTradesHistory(const System::UnicodeString aType = L"all", const bool aTrades = false, const System::UnicodeString aStart = System::UnicodeString(), const System::UnicodeString aEnd = System::UnicodeString(), const System::UnicodeString aOfs = System::UnicodeString());
	System::UnicodeString __fastcall QueryTrades(const System::UnicodeString aTxId, const bool aTrades = false);
	System::UnicodeString __fastcall GetOpenPositions(const System::UnicodeString aTxId, const bool aDocalcs = false, const System::UnicodeString aConsolidation = System::UnicodeString());
	System::UnicodeString __fastcall GetLedgers(const System::UnicodeString aClass = System::UnicodeString(), const System::UnicodeString aAsset = System::UnicodeString(), const System::UnicodeString aType = L"all", const System::UnicodeString aStart = System::UnicodeString(), const System::UnicodeString aEnd = System::UnicodeString(), const System::UnicodeString aOfs = System::UnicodeString());
	System::UnicodeString __fastcall QueryLedgers(const System::UnicodeString aId);
	System::UnicodeString __fastcall GetTradeVolume(const System::UnicodeString aPair = System::UnicodeString(), const System::UnicodeString aFeeInfo = System::UnicodeString());
	System::UnicodeString __fastcall AddExport(const System::UnicodeString aDescription, const System::UnicodeString aReport = L"trades", const System::UnicodeString aFormat = L"CSV", const System::UnicodeString aFields = L"all", const System::UnicodeString aAsset = L"all", const System::UnicodeString aStartTm = System::UnicodeString(), const System::UnicodeString aEndTm = System::UnicodeString());
	System::UnicodeString __fastcall ExportStatus(const System::UnicodeString aReport = L"trades");
	System::UnicodeString __fastcall RetrieveExport(const System::UnicodeString aId);
	System::UnicodeString __fastcall RemoveExport(const System::UnicodeString aId, const System::UnicodeString aType = L"delete");
	
private:
	System::UnicodeString __fastcall GetOrderType(TsgcHTTPKrakenOrderType aOrderType);
	System::UnicodeString __fastcall GetOrderSide(TsgcHTTPKrakenOrderSide aOrderSide);
	
public:
	System::UnicodeString __fastcall AddOrder(TsgcHTTPKrakenOrder* const aOrder);
	System::UnicodeString __fastcall CancelOrder(const System::UnicodeString aTxId);
};


class PASCALIMPLEMENTATION TsgcHTTP_API_Kraken_Futures_Rest : public TsgcHTTP_API_Kraken
{
	typedef TsgcHTTP_API_Kraken inherited;
	
protected:
	virtual void __fastcall DoInitializeLog();
	virtual System::UnicodeString __fastcall GetEndpoint(const System::UnicodeString aMethod);
	virtual System::UnicodeString __fastcall DoHTTP_REQUEST_PRIVATE(const System::UnicodeString aRequest, const System::UnicodeString aMethod, const System::UnicodeString aParameters = System::UnicodeString(), const System::UnicodeString aBody = System::UnicodeString());
	virtual System::UnicodeString __fastcall DoHTTP_GET_PRIVATE(const System::UnicodeString aMethod, const System::UnicodeString aParameters = System::UnicodeString());
	virtual System::UnicodeString __fastcall DoHTTP_POST_PRIVATE(const System::UnicodeString aMethod, const System::UnicodeString aParameters = System::UnicodeString(), const System::UnicodeString aBody = System::UnicodeString());
	
private:
	TsgcHTTPKrakenFutures_Options* FKrakenOptions;
	void __fastcall SetKrakenOptions(TsgcHTTPKrakenFutures_Options* const Value);
	
public:
	__property TsgcHTTPKrakenFutures_Options* KrakenOptions = {read=FKrakenOptions, write=SetKrakenOptions};
	__fastcall virtual TsgcHTTP_API_Kraken_Futures_Rest(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcHTTP_API_Kraken_Futures_Rest();
	System::UnicodeString __fastcall GetFeeSchedules();
	System::UnicodeString __fastcall GetOrderBook(const System::UnicodeString aSymbol);
	System::UnicodeString __fastcall GetTickers();
	System::UnicodeString __fastcall GetInstruments();
	System::UnicodeString __fastcall GetHistory(const System::UnicodeString aSymbol, System::UnicodeString aLastTime = System::UnicodeString());
	
private:
	System::UnicodeString __fastcall GetOrderType(TsgcHTTPKrakenFuturesOrderType aOrderType);
	System::UnicodeString __fastcall GetSide(TsgcHTTPKrakenFuturesOrderSide aSide);
	System::UnicodeString __fastcall GetTriggerSignal(TsgcHTTPKrakenFuturesOrderTriggerSignal aTriggerSignal);
	
public:
	System::UnicodeString __fastcall EditOrderByOrderId(const System::UnicodeString aOrderId, int aSize = 0x0, double aLimitPrice = 0.000000E+00, double aStopPrice = 0.000000E+00);
	System::UnicodeString __fastcall EditOrderByCliOrderId(const System::UnicodeString aCliOrderId, int aSize = 0x0, double aLimitPrice = 0.000000E+00, double aStopPrice = 0.000000E+00);
	System::UnicodeString __fastcall SendOrder(TsgcHTTPKrakenFuturesOrder* const aOrder);
	System::UnicodeString __fastcall SendMarketOrder(TsgcHTTPKrakenFuturesOrderSide aSide, const System::UnicodeString aSymbol, int aSize);
	System::UnicodeString __fastcall SendLimitOrder(TsgcHTTPKrakenFuturesOrderSide aSide, const System::UnicodeString aSymbol, int aSize, double aLimitPrice);
	System::UnicodeString __fastcall SendStopOrder(TsgcHTTPKrakenFuturesOrderSide aSide, const System::UnicodeString aSymbol, int aSize, double aStopPrice, double aLimitPrice);
	System::UnicodeString __fastcall SendTakeProfit(TsgcHTTPKrakenFuturesOrderSide aSide, const System::UnicodeString aSymbol, int aSize, double aStopPrice, double aLimitPrice);
	System::UnicodeString __fastcall CancelOrderByOrderId(const System::UnicodeString aOrderId);
	System::UnicodeString __fastcall CancelOrderByCliOrderId(const System::UnicodeString aCliOrderId);
	System::UnicodeString __fastcall GetFills(const System::UnicodeString aLastFillDate = System::UnicodeString());
	System::UnicodeString __fastcall Transfer(const System::UnicodeString aFromAccount, const System::UnicodeString aToAccount, const System::UnicodeString aUnit, double aAmount);
	System::UnicodeString __fastcall GetOpenPositions();
	System::UnicodeString __fastcall GetNotifications();
	System::UnicodeString __fastcall GetAccounts();
	System::UnicodeString __fastcall CancelAllOrders(const System::UnicodeString aSymbol = System::UnicodeString());
	System::UnicodeString __fastcall CancelAllOrdersAfter(int aTimeout = 0x0);
	System::UnicodeString __fastcall GetOpenOrders();
	System::UnicodeString __fastcall GetHistoricalOrders(System::TDateTime aSince, System::TDateTime aBefore = 0.000000E+00, System::UnicodeString aSort = L"desc", System::UnicodeString aContinuationToken = System::UnicodeString());
	System::UnicodeString __fastcall GetHistoricalTriggers(System::TDateTime aSince, System::TDateTime aBefore = 0.000000E+00, System::UnicodeString aSort = L"desc", System::UnicodeString aContinuationToken = System::UnicodeString());
	System::UnicodeString __fastcall GetHistoricalExecutions(System::TDateTime aSince, System::TDateTime aBefore = 0.000000E+00, System::UnicodeString aSort = L"desc", System::UnicodeString aContinuationToken = System::UnicodeString());
	System::UnicodeString __fastcall WithdrawalToSpotWallet(const System::UnicodeString aCurrency, double aAmount);
	System::UnicodeString __fastcall GetFeeScheduleVolumes();
	System::UnicodeString __fastcall GetAccountLogCSV();
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgchttp_api_kraken */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCHTTP_API_KRAKEN)
using namespace Sgchttp_api_kraken;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgchttp_api_krakenHPP
