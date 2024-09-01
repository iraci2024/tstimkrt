// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcHTTP_API_Binance.pas' rev: 35.00 (Windows)

#ifndef Sgchttp_api_binanceHPP
#define Sgchttp_api_binanceHPP

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

namespace Sgchttp_api_binance
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcHTTPBinanceLog_Options;
class DELPHICLASS TsgcHTTPBinance_Options;
class DELPHICLASS TsgcHTTP_API_Binance;
class DELPHICLASS TsgcHTTP_API_Binance_Rest_Base;
class DELPHICLASS TsgcHTTP_API_Binance_Rest;
class DELPHICLASS TsgcHTTP_API_Binance_Futures_Rest;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TsgcHTTPBinanceChartIntervals : unsigned char { bcih1m, bcih3m, bcih5m, bcih15m, bcih30m, bcih1h, bcih2h, bcih4h, bcih6h, bcih8h, bcih12h, bcih1d, bcih3d, bcih1w, bcih1Mo };

enum DECLSPEC_DENUM TsgcHTTPBinanceFutOpenInterestPeriods : unsigned char { oip5m, oip15m, oip30m, oip1h, oip2h, oip4h, oip6h, oip12h, oip1d };

enum DECLSPEC_DENUM TsgcHTTPBinanceOrderSide : unsigned char { bosNone, bosBuy, bosSell };

enum DECLSPEC_DENUM TsgcHTTPBinanceFuturesContracts : unsigned char { bfchUSDT, bfchCOIN };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTPBinanceLog_Options : public System::Classes::TPersistent
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
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcHTTPBinanceLog_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcHTTPBinanceLog_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTPBinance_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FApiKey;
	System::UnicodeString FApiSecret;
	bool FBinanceUS;
	TsgcHTTPBinanceLog_Options* FLogOptions;
	bool FTestNet;
	void __fastcall SetLogOptions(TsgcHTTPBinanceLog_Options* const Value);
	
public:
	__fastcall virtual TsgcHTTPBinance_Options();
	__fastcall virtual ~TsgcHTTPBinance_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString ApiKey = {read=FApiKey, write=FApiKey};
	__property System::UnicodeString ApiSecret = {read=FApiSecret, write=FApiSecret};
	__property bool BinanceUS = {read=FBinanceUS, write=FBinanceUS, nodefault};
	__property TsgcHTTPBinanceLog_Options* LogOptions = {read=FLogOptions, write=SetLogOptions};
	__property bool TestNet = {read=FTestNet, write=FTestNet, nodefault};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcHTTP_API_Binance : public Sgchttp_api::TsgcHTTPAPI_client
{
	typedef Sgchttp_api::TsgcHTTPAPI_client inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetEndpointBase() = 0 ;
	
private:
	System::UnicodeString __fastcall GetIntervalString(TsgcHTTPBinanceChartIntervals aInterval);
	__int64 __fastcall GetTimeStamp();
	void __fastcall DoInitializeLog();
	
protected:
	virtual System::UnicodeString __fastcall DoHTTP_GET(const System::UnicodeString aMethod, const System::UnicodeString aParameters = System::UnicodeString(), const System::UnicodeString aHeader = System::UnicodeString());
	virtual System::UnicodeString __fastcall DoHTTP_POST(const System::UnicodeString aMethod, const System::UnicodeString aParameters = System::UnicodeString(), const System::UnicodeString aHeader = System::UnicodeString());
	virtual System::UnicodeString __fastcall DoHTTP_DELETE(const System::UnicodeString aMethod, const System::UnicodeString aParameters = System::UnicodeString(), const System::UnicodeString aHeader = System::UnicodeString());
	virtual System::UnicodeString __fastcall DoHTTP_GET_HMAC256(const System::UnicodeString aMethod, const System::UnicodeString aParameters = System::UnicodeString());
	virtual System::UnicodeString __fastcall DoHTTP_POST_HMAC256(const System::UnicodeString aMethod, const System::UnicodeString aParameters = System::UnicodeString(), bool aQueryString = false);
	virtual System::UnicodeString __fastcall DoHTTP_DELETE_HMAC256(const System::UnicodeString aMethod, const System::UnicodeString aParameters = System::UnicodeString());
	
public:
	__fastcall virtual TsgcHTTP_API_Binance(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcHTTP_API_Binance();
	
private:
	TsgcHTTPBinance_Options* FBinanceOptions;
	void __fastcall SetBinanceOptions(TsgcHTTPBinance_Options* const Value);
	
public:
	__property TsgcHTTPBinance_Options* BinanceOptions = {read=FBinanceOptions, write=SetBinanceOptions};
};


class PASCALIMPLEMENTATION TsgcHTTP_API_Binance_Rest_Base : public TsgcHTTP_API_Binance
{
	typedef TsgcHTTP_API_Binance inherited;
	
protected:
	System::UnicodeString __fastcall GetOrderSideString(TsgcHTTPBinanceOrderSide aValue);
	virtual System::UnicodeString __fastcall GetDoubleString(double aValue);
	virtual System::UnicodeString __fastcall GetEndpointPing() = 0 ;
	virtual System::UnicodeString __fastcall GetEndpointServerTime() = 0 ;
	virtual System::UnicodeString __fastcall GetEndpointExchangeInformation() = 0 ;
	virtual System::UnicodeString __fastcall GetEndpointOrderBook() = 0 ;
	virtual System::UnicodeString __fastcall GetEndpointTrades() = 0 ;
	virtual System::UnicodeString __fastcall GetEndpointAggregateTrades() = 0 ;
	virtual System::UnicodeString __fastcall GetEndpointKLines() = 0 ;
	virtual System::UnicodeString __fastcall GetEndpoint24HRTicker() = 0 ;
	virtual System::UnicodeString __fastcall GetEndpointPriceTicker() = 0 ;
	virtual System::UnicodeString __fastcall GetEndpointBookTicker() = 0 ;
	
public:
	bool __fastcall Ping();
	System::UnicodeString __fastcall GetServerTime();
	System::UnicodeString __fastcall GetExchangeInformation();
	System::UnicodeString __fastcall GetOrderBook(const System::UnicodeString aSymbol, int aLimit = 0x64);
	System::UnicodeString __fastcall GetTrades(const System::UnicodeString aSymbol, int aLimit = 0x1f4);
	System::UnicodeString __fastcall GetHistoricalTrades(const System::UnicodeString aSymbol, int aLimit = 0x1f4, int aFromId = 0x0);
	System::UnicodeString __fastcall GetAggregateTrades(const System::UnicodeString aSymbol, int aFromId = 0x0, __int64 aStartTime = 0LL, __int64 aEndTime = 0LL, int aLimit = 0x1f4);
	System::UnicodeString __fastcall GetKLines(const System::UnicodeString aSymbol, TsgcHTTPBinanceChartIntervals aInterval, __int64 aStartTime = 0LL, __int64 aEndTime = 0LL, int aLimit = 0x1f4);
	System::UnicodeString __fastcall Get24hrTicker(const System::UnicodeString aSymbol);
	System::UnicodeString __fastcall GetPriceTicker(const System::UnicodeString aSymbol = System::UnicodeString());
	System::UnicodeString __fastcall GetBookTicker(const System::UnicodeString aSymbol = System::UnicodeString());
public:
	/* TsgcHTTP_API_Binance.Create */ inline __fastcall virtual TsgcHTTP_API_Binance_Rest_Base(System::Classes::TComponent* aOwner) : TsgcHTTP_API_Binance(aOwner) { }
	/* TsgcHTTP_API_Binance.Destroy */ inline __fastcall virtual ~TsgcHTTP_API_Binance_Rest_Base() { }
	
};


class PASCALIMPLEMENTATION TsgcHTTP_API_Binance_Rest : public TsgcHTTP_API_Binance_Rest_Base
{
	typedef TsgcHTTP_API_Binance_Rest_Base inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetEndpointBase();
	virtual System::UnicodeString __fastcall GetEndpointPing();
	virtual System::UnicodeString __fastcall GetEndpointServerTime();
	virtual System::UnicodeString __fastcall GetEndpointExchangeInformation();
	virtual System::UnicodeString __fastcall GetEndpointOrderBook();
	virtual System::UnicodeString __fastcall GetEndpointTrades();
	virtual System::UnicodeString __fastcall GetEndpointAggregateTrades();
	virtual System::UnicodeString __fastcall GetEndpointKLines();
	virtual System::UnicodeString __fastcall GetEndpoint24HRTicker();
	virtual System::UnicodeString __fastcall GetEndpointPriceTicker();
	virtual System::UnicodeString __fastcall GetEndpointBookTicker();
	
public:
	System::UnicodeString __fastcall GetAveragePrice(const System::UnicodeString aSymbol);
	System::UnicodeString __fastcall NewOrder(const System::UnicodeString aSymbol, const System::UnicodeString aSide, const System::UnicodeString aType, System::UnicodeString aTimeInForce = System::UnicodeString(), double aQuantity = 0.000000E+00, double aQuoteOrderQty = 0.000000E+00, double aPrice = 0.000000E+00, System::UnicodeString aNewClientOrderId = System::UnicodeString(), double aStopPrice = 0.000000E+00, double aIcebergQty = 0.000000E+00, System::UnicodeString aNewOrderRespType = System::UnicodeString());
	System::UnicodeString __fastcall PlaceMarketOrder(TsgcHTTPBinanceOrderSide aSide, const System::UnicodeString aSymbol, double aQuantity);
	System::UnicodeString __fastcall PlaceLimitOrder(TsgcHTTPBinanceOrderSide aSide, const System::UnicodeString aSymbol, double aQuantity, double aLimitPrice);
	System::UnicodeString __fastcall PlaceStopOrder(TsgcHTTPBinanceOrderSide aSide, const System::UnicodeString aSymbol, double aQuantity, double aStopPrice, double aLimitPrice);
	System::UnicodeString __fastcall TestNewOrder(const System::UnicodeString aSymbol, const System::UnicodeString aSide, const System::UnicodeString aType, System::UnicodeString aTimeInForce = System::UnicodeString(), double aQuantity = 0.000000E+00, double aQuoteOrderQty = 0.000000E+00, double aPrice = 0.000000E+00, System::UnicodeString aNewClientOrderId = System::UnicodeString(), double aStopPrice = 0.000000E+00, double aIcebergQty = 0.000000E+00, System::UnicodeString aNewOrderRespType = System::UnicodeString());
	System::UnicodeString __fastcall QueryOrder(const System::UnicodeString aSymbol, __int64 aOrderId = 0LL, const System::UnicodeString aOrigClientOrderId = System::UnicodeString());
	System::UnicodeString __fastcall CancelOrder(const System::UnicodeString aSymbol, __int64 aOrderId = 0LL, const System::UnicodeString aOrigClientOrderId = System::UnicodeString());
	System::UnicodeString __fastcall CancelAllOpenOrders(const System::UnicodeString aSymbol);
	System::UnicodeString __fastcall GetOpenOrders(const System::UnicodeString aSymbol = System::UnicodeString());
	System::UnicodeString __fastcall GetAllOrders(const System::UnicodeString aSymbol, __int64 aOrderId = 0LL, __int64 aStartTime = 0LL, __int64 aEndTime = 0LL, int aLimit = 0x1f4);
	System::UnicodeString __fastcall NewOCO(const System::UnicodeString aSymbol, const System::UnicodeString aSide, double aQuantity, double aPrice, double aStopPrice, const System::UnicodeString aListClientOrderId = System::UnicodeString(), System::UnicodeString aLimitClientOrderId = System::UnicodeString(), double aLimitIcebergQty = 0.000000E+00, System::UnicodeString aStopClientOrderId = System::UnicodeString(), double aStopLimitPrice = 0.000000E+00, double aStopIcebergQty = 0.000000E+00, System::UnicodeString aStopLimitTimeInForce = System::UnicodeString(), System::UnicodeString aNewOrderRespType = System::UnicodeString());
	System::UnicodeString __fastcall CancelOCO(const System::UnicodeString aSymbol, int aOrderListId = 0x0, const System::UnicodeString aListClientOrderId = System::UnicodeString(), System::UnicodeString aNewClientOrderId = System::UnicodeString());
	System::UnicodeString __fastcall QueryOCO(const System::UnicodeString aSymbol, int aOrderListId = 0x0, const System::UnicodeString aOrigClientOrderId = System::UnicodeString());
	System::UnicodeString __fastcall GetAllOCO(int aFromId = 0x0, __int64 aStartTime = 0LL, __int64 aEndTime = 0LL, int aLimit = 0x1f4);
	System::UnicodeString __fastcall GetOpenOCO();
	System::UnicodeString __fastcall GetAccountInformation();
	System::UnicodeString __fastcall GetAccountTradeList(const System::UnicodeString aSymbol, __int64 aStartTime = 0LL, __int64 aEndTime = 0LL, int aFromId = 0x0, int aLimit = 0x1f4);
public:
	/* TsgcHTTP_API_Binance.Create */ inline __fastcall virtual TsgcHTTP_API_Binance_Rest(System::Classes::TComponent* aOwner) : TsgcHTTP_API_Binance_Rest_Base(aOwner) { }
	/* TsgcHTTP_API_Binance.Destroy */ inline __fastcall virtual ~TsgcHTTP_API_Binance_Rest() { }
	
};


class PASCALIMPLEMENTATION TsgcHTTP_API_Binance_Futures_Rest : public TsgcHTTP_API_Binance_Rest_Base
{
	typedef TsgcHTTP_API_Binance_Rest_Base inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetEndpointBase();
	virtual System::UnicodeString __fastcall GetEndpointPing();
	virtual System::UnicodeString __fastcall GetEndpointServerTime();
	virtual System::UnicodeString __fastcall GetEndpointExchangeInformation();
	virtual System::UnicodeString __fastcall GetEndpointOrderBook();
	virtual System::UnicodeString __fastcall GetEndpointAggregateTrades();
	virtual System::UnicodeString __fastcall GetEndpointTrades();
	virtual System::UnicodeString __fastcall GetEndpointKLines();
	virtual System::UnicodeString __fastcall GetEndpoint24HRTicker();
	virtual System::UnicodeString __fastcall GetEndpointPriceTicker();
	virtual System::UnicodeString __fastcall GetEndpointBookTicker();
	
private:
	System::UnicodeString __fastcall GetOpenInterestPeriod(TsgcHTTPBinanceFutOpenInterestPeriods aValue);
	
public:
	System::UnicodeString __fastcall GetMarkPrice(const System::UnicodeString aSymbol = System::UnicodeString());
	System::UnicodeString __fastcall GetFundingRateHistory(const System::UnicodeString aSymbol, __int64 aStartTime = 0LL, __int64 aEndTime = 0LL, int aLimit = 0x64);
	System::UnicodeString __fastcall GetOpenInterest(const System::UnicodeString aSymbol = System::UnicodeString());
	System::UnicodeString __fastcall GetOpenInterestStatistics(const System::UnicodeString aSymbol, const TsgcHTTPBinanceFutOpenInterestPeriods aPeriod, int aLimit = 0x1e, __int64 aStartTime = 0LL, __int64 aEndTime = 0LL);
	System::UnicodeString __fastcall GetTopTraderAccountRatio(const System::UnicodeString aSymbol, const TsgcHTTPBinanceFutOpenInterestPeriods aPeriod, int aLimit = 0x1e, __int64 aStartTime = 0LL, __int64 aEndTime = 0LL);
	System::UnicodeString __fastcall GetTopTraderPositionRatio(const System::UnicodeString aSymbol, const TsgcHTTPBinanceFutOpenInterestPeriods aPeriod, int aLimit = 0x1e, __int64 aStartTime = 0LL, __int64 aEndTime = 0LL);
	System::UnicodeString __fastcall GetGlobalAccountRatio(const System::UnicodeString aSymbol, const TsgcHTTPBinanceFutOpenInterestPeriods aPeriod, int aLimit = 0x1e, __int64 aStartTime = 0LL, __int64 aEndTime = 0LL);
	System::UnicodeString __fastcall GetTakerVolume(const System::UnicodeString aSymbol, const TsgcHTTPBinanceFutOpenInterestPeriods aPeriod, int aLimit = 0x1e, __int64 aStartTime = 0LL, __int64 aEndTime = 0LL);
	System::UnicodeString __fastcall ChangePositionMode(bool aDualPosition);
	System::UnicodeString __fastcall GetCurrentPositionMode();
	System::UnicodeString __fastcall NewOrder(const System::UnicodeString aSymbol, const System::UnicodeString aSide, System::UnicodeString aPositionSide, const System::UnicodeString aType, System::UnicodeString aTimeInForce = System::UnicodeString(), double aQuantity = 0.000000E+00, System::UnicodeString aReduceOnly = System::UnicodeString(), double aPrice = 0.000000E+00, System::UnicodeString aNewClientOrderId = System::UnicodeString(), double aStopPrice = 0.000000E+00, System::UnicodeString aClosePosition = System::UnicodeString(), double aActivationPrice = 0.000000E+00, double aCallbackRate = 0.000000E+00, System::UnicodeString aWorkingType = System::UnicodeString(), System::UnicodeString aNewOrderRespType = System::UnicodeString());
	System::UnicodeString __fastcall PlaceMarketOrder(TsgcHTTPBinanceOrderSide aSide, const System::UnicodeString aSymbol, double aQuantity);
	System::UnicodeString __fastcall PlaceLimitOrder(TsgcHTTPBinanceOrderSide aSide, const System::UnicodeString aSymbol, double aQuantity, double aLimitPrice);
	System::UnicodeString __fastcall PlaceStopOrder(TsgcHTTPBinanceOrderSide aSide, const System::UnicodeString aSymbol, double aQuantity, double aStopPrice, double aLimitPrice);
	System::UnicodeString __fastcall PlaceTrailingStopOrder(TsgcHTTPBinanceOrderSide aSide, const System::UnicodeString aSymbol, double aQuantity, double aActivationPrice, double aCallbackRate);
	System::UnicodeString __fastcall QueryOrder(const System::UnicodeString aSymbol, __int64 aOrderId = 0LL, const System::UnicodeString aOrigClientOrderId = System::UnicodeString());
	System::UnicodeString __fastcall CancelOrder(const System::UnicodeString aSymbol, __int64 aOrderId = 0LL, const System::UnicodeString aOrigClientOrderId = System::UnicodeString());
	System::UnicodeString __fastcall CancelAllOpenOrders(const System::UnicodeString aSymbol);
	System::UnicodeString __fastcall AutoCancelAllOpenOrders(const System::UnicodeString aSymbol, int aCountdownTime);
	System::UnicodeString __fastcall QueryCurrentOpenOrder(const System::UnicodeString aSymbol, __int64 aOrderId = 0LL, const System::UnicodeString aOrigClientOrderId = System::UnicodeString());
	System::UnicodeString __fastcall GetOpenOrders(const System::UnicodeString aSymbol = System::UnicodeString());
	System::UnicodeString __fastcall GetAllOrders(const System::UnicodeString aSymbol, __int64 aOrderId = 0LL, __int64 aStartTime = 0LL, __int64 aEndTime = 0LL, int aLimit = 0x64);
	System::UnicodeString __fastcall GetAccountBalance();
	System::UnicodeString __fastcall GetAccountInformation();
	System::UnicodeString __fastcall ChangeInitialLeverage(const System::UnicodeString aSymbol, int aLeverage);
	System::UnicodeString __fastcall ChangeMarginType(const System::UnicodeString aSymbol, const System::UnicodeString aMarginType);
	System::UnicodeString __fastcall ModifyIsolatedPositionMargin(const System::UnicodeString aSymbol, double aAmount, int aType, System::UnicodeString aPositionSide = System::UnicodeString());
	System::UnicodeString __fastcall GetPositionMarginChangeHistory(const System::UnicodeString aSymbol, int aType = 0x0, __int64 aStartTime = 0LL, __int64 aEndTime = 0LL, int aLimit = 0x1f4);
	System::UnicodeString __fastcall GetPositionInformation(const System::UnicodeString aSymbol = System::UnicodeString());
	System::UnicodeString __fastcall GetAccountTradeList(const System::UnicodeString aSymbol, __int64 aStartTime = 0LL, __int64 aEndTime = 0LL, int aFromId = 0x0, int aLimit = 0x1f4);
	System::UnicodeString __fastcall GetIncomeHistory(const System::UnicodeString aSymbol = System::UnicodeString(), const System::UnicodeString aIncomeType = System::UnicodeString(), __int64 aStartTime = 0LL, __int64 aEndTime = 0LL, int aLimit = 0x64);
	System::UnicodeString __fastcall GetNotionalLeverageBracket(const System::UnicodeString aSymbol = System::UnicodeString());
	
private:
	TsgcHTTPBinanceFuturesContracts FFuturesContracts;
	
public:
	__property TsgcHTTPBinanceFuturesContracts FuturesContracts = {read=FFuturesContracts, write=FFuturesContracts, nodefault};
public:
	/* TsgcHTTP_API_Binance.Create */ inline __fastcall virtual TsgcHTTP_API_Binance_Futures_Rest(System::Classes::TComponent* aOwner) : TsgcHTTP_API_Binance_Rest_Base(aOwner) { }
	/* TsgcHTTP_API_Binance.Destroy */ inline __fastcall virtual ~TsgcHTTP_API_Binance_Futures_Rest() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgchttp_api_binance */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCHTTP_API_BINANCE)
using namespace Sgchttp_api_binance;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgchttp_api_binanceHPP
