// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_API_Binance.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_api_binanceHPP
#define Sgcwebsocket_api_binanceHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <sgcBase_Classes.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Types.hpp>
#include <sgcHTTP_API_Binance.hpp>
#include <sgcSocket_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_api_binance
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSBinanceRestLog_Options;
class DELPHICLASS TsgcWSBinanceLog_Options;
class DELPHICLASS TsgcWSBinance_Options;
class DELPHICLASS TsgcWS_API_Binance_Base;
class DELPHICLASS TsgcWS_API_Binance;
class DELPHICLASS TsgcWS_API_Binance_Futures;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSBinanceHTTPExceptionEvent)(System::TObject* Sender, System::Sysutils::Exception* E);

enum DECLSPEC_DENUM TwsBinanceStreams : unsigned char { bncAggregateTrade, bncTrade, bncKline, bncMiniTicker, bncAllMiniTickers, bncTicker, bncAllMarketTickers, bncBookTicker, bncAllBookTickers, bncPartialBookDepth, bncDiffDepth, bncMarkPrice, bncAllMarketMarkPrice, bncLiquidationOrder, bncAllMarketLiquidationOrder };

enum DECLSPEC_DENUM TsgcWSBinanceChartIntervals : unsigned char { bci1m, bci3m, bci5m, bci15m, bci30m, bci1h, bci2h, bci4h, bci6h, bci8h, bci12h, bci1d, bci3d, bci1w, bci1Mo };

enum DECLSPEC_DENUM TsgcWSBinanceDepthLevels : unsigned char { bde5, bde10, bde20 };

enum DECLSPEC_DENUM TsgcWSBinanceFuturesContracts : unsigned char { bfcUSDT, bfcCOIN };

enum DECLSPEC_DENUM TsgcWSBinanceListenKeyMethod : unsigned char { blkmGetListenKey, blkmDeleteListenKey, blkmRefreshListenKey };

enum DECLSPEC_DENUM TsgcWSBinanceListenKeyOnDisconnect : unsigned char { blkodDeleteListenKey, blkodClearListenKey, blkodDoNothing };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSBinanceRestLog_Options : public System::Classes::TPersistent
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
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSBinanceRestLog_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSBinanceRestLog_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSBinanceLog_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	System::UnicodeString FFileName;
	TsgcWSBinanceRestLog_Options* FREST;
	void __fastcall SetREST(TsgcWSBinanceRestLog_Options* const Value);
	
public:
	__fastcall virtual TsgcWSBinanceLog_Options();
	__fastcall virtual ~TsgcWSBinanceLog_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property System::UnicodeString FileName = {read=FFileName, write=FFileName};
	__property TsgcWSBinanceRestLog_Options* REST = {read=FREST, write=SetREST};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSBinance_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FApiKey;
	System::UnicodeString FApiSecret;
	bool FBinanceUS;
	TsgcWSBinanceListenKeyOnDisconnect FListenKeyOnDisconnect;
	TsgcWSBinanceLog_Options* FHTTPLogOptions;
	bool FTestNet;
	bool FUseCombinedStreams;
	bool FUserStream;
	void __fastcall SetHTTPLogOptions(TsgcWSBinanceLog_Options* const Value);
	
public:
	__fastcall virtual TsgcWSBinance_Options();
	__fastcall virtual ~TsgcWSBinance_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString ApiKey = {read=FApiKey, write=FApiKey};
	__property System::UnicodeString ApiSecret = {read=FApiSecret, write=FApiSecret};
	__property bool BinanceUS = {read=FBinanceUS, write=FBinanceUS, nodefault};
	__property TsgcWSBinanceListenKeyOnDisconnect ListenKeyOnDisconnect = {read=FListenKeyOnDisconnect, write=FListenKeyOnDisconnect, nodefault};
	__property TsgcWSBinanceLog_Options* HTTPLogOptions = {read=FHTTPLogOptions, write=SetHTTPLogOptions};
	__property bool TestNet = {read=FTestNet, write=FTestNet, nodefault};
	__property bool UseCombinedStreams = {read=FUseCombinedStreams, write=FUseCombinedStreams, nodefault};
	__property bool UserStream = {read=FUserStream, write=FUserStream, nodefault};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWS_API_Binance_Base : public Sgcwebsocket_classes::TsgcWSAPI_client
{
	typedef Sgcwebsocket_classes::TsgcWSAPI_client inherited;
	
protected:
	virtual void __fastcall DoNotifyDisconnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoBeforeConnect();
	virtual bool __fastcall IsInternalMessage(const System::UnicodeString aMessage);
	int FId;
	int __fastcall GetNewId();
	virtual System::UnicodeString __fastcall GetURL_BaseAPI() = 0 ;
	virtual System::UnicodeString __fastcall GetURL_ListenKey() = 0 ;
	virtual System::UnicodeString __fastcall GetDepthString(TsgcWSBinanceDepthLevels aDepth);
	virtual System::UnicodeString __fastcall GetIntervalString(TsgcWSBinanceChartIntervals aInterval);
	virtual System::UnicodeString __fastcall GetStream(const TwsBinanceStreams aStream, const System::UnicodeString aSymbol = System::UnicodeString(), const System::UnicodeString aInterval = System::UnicodeString());
	virtual int __fastcall DoSubscribe(const TwsBinanceStreams aStream, const System::UnicodeString aSymbol = System::UnicodeString(), const System::UnicodeString aInterval = System::UnicodeString());
	virtual int __fastcall DoUnSubscribe(const TwsBinanceStreams aStream, const System::UnicodeString aSymbol = System::UnicodeString(), const System::UnicodeString aInterval = System::UnicodeString());
	
public:
	__fastcall virtual TsgcWS_API_Binance_Base(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWS_API_Binance_Base();
	
private:
	TsgcWSBinance_Options* FBinance;
	void __fastcall SetBinance(TsgcWSBinance_Options* const Value);
	
public:
	__property TsgcWSBinance_Options* Binance = {read=FBinance, write=SetBinance};
	
private:
	System::UnicodeString FListenKey;
	Sgcbase_classes::TsgcTimer* FTimerUserStream;
	System::UnicodeString __fastcall DoRequestListenKey(const TsgcWSBinanceListenKeyMethod aMethod);
	void __fastcall DoDeleteListenKey();
	System::UnicodeString __fastcall DoGetListenKey();
	void __fastcall DoRequestNewListenKey();
	void __fastcall OnKeepAliveUserStream(System::TObject* Sender);
	void __fastcall OnKeepAliveUserStreamException(System::TObject* Sender, System::Sysutils::Exception* E);
	
protected:
	virtual void __fastcall DoStartKeepAliveUserStream();
	virtual void __fastcall DoStopKeepAliveUserStream();
	
public:
	__property System::UnicodeString ListenKey = {read=FListenKey};
	
private:
	TsgcWSBinanceHTTPExceptionEvent FOnBinanceHTTPException;
	
protected:
	virtual void __fastcall DoBinanceHTTPExceptionEvent(System::Sysutils::Exception* E);
	
public:
	__property TsgcWSBinanceHTTPExceptionEvent OnBinanceHTTPException = {read=FOnBinanceHTTPException, write=FOnBinanceHTTPException};
};


class PASCALIMPLEMENTATION TsgcWS_API_Binance : public TsgcWS_API_Binance_Base
{
	typedef TsgcWS_API_Binance_Base inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetURL();
	virtual System::UnicodeString __fastcall GetURL_BaseAPI();
	virtual System::UnicodeString __fastcall GetURL_ListenKey();
	
public:
	__fastcall virtual ~TsgcWS_API_Binance();
	int __fastcall SubscribeAggregateTrades(const System::UnicodeString aSymbol);
	int __fastcall UnSubscribeAggregateTrades(const System::UnicodeString aSymbol);
	int __fastcall SubscribeTrades(const System::UnicodeString aSymbol);
	int __fastcall UnSubscribeTrades(const System::UnicodeString aSymbol);
	int __fastcall SubscribeKLine(const System::UnicodeString aSymbol, const TsgcWSBinanceChartIntervals aInterval);
	int __fastcall UnSubscribeKLine(const System::UnicodeString aSymbol, const TsgcWSBinanceChartIntervals aInterval);
	int __fastcall SubscribeMiniTicker(const System::UnicodeString aSymbol);
	int __fastcall UnSubscribeMiniTicker(const System::UnicodeString aSymbol);
	int __fastcall SubscribeAllMiniTickers();
	int __fastcall UnSubscribeAllMiniTickers();
	int __fastcall SubscribeTicker(const System::UnicodeString aSymbol);
	int __fastcall UnSubscribeTicker(const System::UnicodeString aSymbol);
	int __fastcall SubscribeAllMarketTickers();
	int __fastcall UnSubscribeAllMarketTickers();
	int __fastcall SubscribeBookTicker(const System::UnicodeString aSymbol);
	int __fastcall UnSubscribeBookTicker(const System::UnicodeString aSymbol);
	int __fastcall SubscribeAllBookTickers();
	int __fastcall UnSubscribeAllBookTickers();
	int __fastcall SubscribePartialBookDepth(const System::UnicodeString aSymbol, const TsgcWSBinanceDepthLevels aDepth, System::UnicodeString aUpdateSpeed = System::UnicodeString());
	int __fastcall UnSubscribePartialBookDepth(const System::UnicodeString aSymbol, const TsgcWSBinanceDepthLevels aDepth);
	int __fastcall SubscribeDiffDepth(const System::UnicodeString aSymbol);
	int __fastcall UnSubscribeDiffDepth(const System::UnicodeString aSymbol);
	int __fastcall ListSubscriptions();
	
private:
	Sgchttp_api_binance::TsgcHTTP_API_Binance_Rest* FREST_API;
	Sgchttp_api_binance::TsgcHTTP_API_Binance_Rest* __fastcall GetREST_API();
	
protected:
	virtual void __fastcall OnHTTPAPIExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	
public:
	__property Sgchttp_api_binance::TsgcHTTP_API_Binance_Rest* REST_API = {read=GetREST_API, write=FREST_API};
public:
	/* TsgcWS_API_Binance_Base.Create */ inline __fastcall virtual TsgcWS_API_Binance(System::Classes::TComponent* aOwner) : TsgcWS_API_Binance_Base(aOwner) { }
	
};


class PASCALIMPLEMENTATION TsgcWS_API_Binance_Futures : public TsgcWS_API_Binance_Base
{
	typedef TsgcWS_API_Binance_Base inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetURL();
	virtual System::UnicodeString __fastcall GetURL_BaseAPI();
	virtual System::UnicodeString __fastcall GetURL_ListenKey();
	
public:
	int __fastcall SubscribeAggregateTrades(const System::UnicodeString aSymbol);
	int __fastcall UnSubscribeAggregateTrades(const System::UnicodeString aSymbol);
	int __fastcall SubscribeMarkPrice(const System::UnicodeString aSymbol, const System::UnicodeString aUpdateSpeed = L"@1s");
	int __fastcall UnSubscribeMarkPrice(const System::UnicodeString aSymbol);
	int __fastcall SubscribeMarkPriceAllMarket(const System::UnicodeString aSymbol, const System::UnicodeString aUpdateSpeed = L"@1s");
	int __fastcall UnSubscribeMarkPriceAllMarket(const System::UnicodeString aSymbol);
	int __fastcall SubscribeKLine(const System::UnicodeString aSymbol, const TsgcWSBinanceChartIntervals aInterval);
	int __fastcall UnSubscribeKLine(const System::UnicodeString aSymbol, const TsgcWSBinanceChartIntervals aInterval);
	int __fastcall SubscribeMiniTicker(const System::UnicodeString aSymbol);
	int __fastcall UnSubscribeMiniTicker(const System::UnicodeString aSymbol);
	int __fastcall SubscribeAllMiniTickers();
	int __fastcall UnSubscribeAllMiniTickers();
	int __fastcall SubscribeTicker(const System::UnicodeString aSymbol);
	int __fastcall UnSubscribeTicker(const System::UnicodeString aSymbol);
	int __fastcall SubscribeAllMarketTickers();
	int __fastcall UnSubscribeAllMarketTickers();
	int __fastcall SubscribeBookTicker(const System::UnicodeString aSymbol);
	int __fastcall UnSubscribeBookTicker(const System::UnicodeString aSymbol);
	int __fastcall SubscribeAllBookTickers();
	int __fastcall UnSubscribeAllBookTickers();
	int __fastcall SubscribeLiquidationOrders(const System::UnicodeString aSymbol);
	int __fastcall UnSubscribeLiquidationOrders(const System::UnicodeString aSymbol);
	int __fastcall SubscribeAllMarketLiquidationOrders();
	int __fastcall UnSubscribeAllMarketLiquidationOrders();
	int __fastcall SubscribePartialBookDepth(const System::UnicodeString aSymbol, const TsgcWSBinanceDepthLevels aDepth, const System::UnicodeString aUpdateSpeed = System::UnicodeString());
	int __fastcall UnSubscribePartialBookDepth(const System::UnicodeString aSymbol, const TsgcWSBinanceDepthLevels aDepth);
	int __fastcall SubscribeDiffDepth(const System::UnicodeString aSymbol, const System::UnicodeString aUpdateSpeed = System::UnicodeString());
	int __fastcall UnSubscribeDiffDepth(const System::UnicodeString aSymbol);
	
private:
	TsgcWSBinanceFuturesContracts FFuturesContracts;
	Sgchttp_api_binance::TsgcHTTP_API_Binance_Futures_Rest* FREST_API;
	Sgchttp_api_binance::TsgcHTTP_API_Binance_Futures_Rest* __fastcall GetREST_API();
	
protected:
	virtual void __fastcall OnHTTPAPIExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	
public:
	__property TsgcWSBinanceFuturesContracts FuturesContracts = {read=FFuturesContracts, write=FFuturesContracts, nodefault};
	__property Sgchttp_api_binance::TsgcHTTP_API_Binance_Futures_Rest* REST_API = {read=GetREST_API, write=FREST_API};
public:
	/* TsgcWS_API_Binance_Base.Create */ inline __fastcall virtual TsgcWS_API_Binance_Futures(System::Classes::TComponent* aOwner) : TsgcWS_API_Binance_Base(aOwner) { }
	/* TsgcWS_API_Binance_Base.Destroy */ inline __fastcall virtual ~TsgcWS_API_Binance_Futures() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_api_binance */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_API_BINANCE)
using namespace Sgcwebsocket_api_binance;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_api_binanceHPP
