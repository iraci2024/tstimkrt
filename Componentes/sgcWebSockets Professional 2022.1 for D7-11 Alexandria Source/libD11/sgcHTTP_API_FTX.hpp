// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcHTTP_API_FTX.pas' rev: 35.00 (Windows)

#ifndef Sgchttp_api_ftxHPP
#define Sgchttp_api_ftxHPP

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

namespace Sgchttp_api_ftx
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcHTTPFTXOrder;
class DELPHICLASS TsgcHTTPFTXTriggerOrder;
class DELPHICLASS TsgcHTTPFTXLog_Options;
class DELPHICLASS TsgcHTTPFTX_Options;
class DELPHICLASS TsgcHTTP_API_FTX;
class DELPHICLASS TsgcHTTP_API_FTX_Rest;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TsgcHTTPFTXResolution : unsigned char { ftxr15, ftxr60, ftxr300, ftxr900, ftxr3600, ftxr14400, ftxr86400 };

enum DECLSPEC_DENUM TsgcHTTPFTXTriggerOrderType : unsigned char { fttotNone, fttotStop, fttotTrailing_Stop, fttotTake_Profit };

enum DECLSPEC_DENUM TsgcHTTPFTXOrderSide : unsigned char { ftosNone, ftosBuy, ftosSell };

enum DECLSPEC_DENUM TsgcHTTPFTXOrderType : unsigned char { ftotNone, ftotMarket, ftotLimit };

class PASCALIMPLEMENTATION TsgcHTTPFTXOrder : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FClientId;
	bool FIOC;
	System::UnicodeString FMarket;
	bool FPostOnly;
	System::Extended FPrice;
	bool FReduceOnly;
	TsgcHTTPFTXOrderSide FSide;
	System::Extended FSize;
	TsgcHTTPFTXOrderType F_Type;
	
protected:
	virtual System::UnicodeString __fastcall GetJSON();
	
public:
	__fastcall virtual TsgcHTTPFTXOrder();
	__property System::UnicodeString ClientId = {read=FClientId, write=FClientId};
	__property bool IOC = {read=FIOC, write=FIOC, nodefault};
	__property System::UnicodeString Market = {read=FMarket, write=FMarket};
	__property bool PostOnly = {read=FPostOnly, write=FPostOnly, nodefault};
	__property System::Extended Price = {read=FPrice, write=FPrice};
	__property bool ReduceOnly = {read=FReduceOnly, write=FReduceOnly, nodefault};
	__property TsgcHTTPFTXOrderSide Side = {read=FSide, write=FSide, nodefault};
	__property System::Extended Size = {read=FSize, write=FSize};
	__property TsgcHTTPFTXOrderType _Type = {read=F_Type, write=F_Type, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcHTTPFTXOrder() { }
	
};


class PASCALIMPLEMENTATION TsgcHTTPFTXTriggerOrder : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FMarket;
	System::Extended FOrderPrice;
	bool FReduceOnly;
	bool FRetryUntilFilled;
	TsgcHTTPFTXOrderSide FSide;
	System::Extended FSize;
	System::Extended FTrailValue;
	System::Extended FTriggerPrice;
	TsgcHTTPFTXTriggerOrderType F_Type;
	void __fastcall SetOrderPrice(const System::Extended Value);
	void __fastcall SetTrailValue(const System::Extended Value);
	
protected:
	virtual System::UnicodeString __fastcall GetJSON();
	
public:
	__fastcall virtual TsgcHTTPFTXTriggerOrder();
	__property System::UnicodeString Market = {read=FMarket, write=FMarket};
	__property System::Extended OrderPrice = {read=FOrderPrice, write=SetOrderPrice};
	__property bool ReduceOnly = {read=FReduceOnly, write=FReduceOnly, nodefault};
	__property bool RetryUntilFilled = {read=FRetryUntilFilled, write=FRetryUntilFilled, nodefault};
	__property TsgcHTTPFTXOrderSide Side = {read=FSide, write=FSide, nodefault};
	__property System::Extended Size = {read=FSize, write=FSize};
	__property System::Extended TrailValue = {read=FTrailValue, write=SetTrailValue};
	__property System::Extended TriggerPrice = {read=FTriggerPrice, write=FTriggerPrice};
	__property TsgcHTTPFTXTriggerOrderType _Type = {read=F_Type, write=F_Type, nodefault};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcHTTPFTXTriggerOrder() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTPFTXLog_Options : public System::Classes::TPersistent
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
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcHTTPFTXLog_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcHTTPFTXLog_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTPFTX_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FApiKey;
	System::UnicodeString FApiSecret;
	TsgcHTTPFTXLog_Options* FLogOptions;
	System::UnicodeString FSubAccount;
	void __fastcall SetLogOptions(TsgcHTTPFTXLog_Options* const Value);
	
public:
	__fastcall virtual TsgcHTTPFTX_Options();
	__fastcall virtual ~TsgcHTTPFTX_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString ApiKey = {read=FApiKey, write=FApiKey};
	__property System::UnicodeString ApiSecret = {read=FApiSecret, write=FApiSecret};
	__property TsgcHTTPFTXLog_Options* LogOptions = {read=FLogOptions, write=SetLogOptions};
	__property System::UnicodeString SubAccount = {read=FSubAccount, write=FSubAccount};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcHTTP_API_FTX : public Sgchttp_api::TsgcHTTPAPI_client
{
	typedef Sgchttp_api::TsgcHTTPAPI_client inherited;
	
private:
	void __fastcall DoInitializeLog();
	System::UnicodeString __fastcall GetBaseURL();
	
protected:
	virtual System::UnicodeString __fastcall DoHTTP_GET(const System::UnicodeString aMethod, const System::UnicodeString aParameters = System::UnicodeString(), System::Classes::TStrings* const aHeaders = (System::Classes::TStrings*)(0x0));
	virtual System::UnicodeString __fastcall DoHTTP_POST(const System::UnicodeString aMethod, const System::UnicodeString aBody = System::UnicodeString(), System::Classes::TStrings* const aHeaders = (System::Classes::TStrings*)(0x0));
	virtual System::UnicodeString __fastcall DoHTTP_DELETE(const System::UnicodeString aMethod, const System::UnicodeString aParameters = System::UnicodeString(), System::Classes::TStrings* const aHeaders = (System::Classes::TStrings*)(0x0), const System::UnicodeString aBody = System::UnicodeString());
	virtual void __fastcall GetAuthHeaders(const System::UnicodeString aMethod, const System::UnicodeString aRequestPath, const System::UnicodeString aBody, System::Classes::TStringList* &Headers);
	virtual System::UnicodeString __fastcall DoHTTP_GET_PRIVATE(const System::UnicodeString aRequestPath);
	virtual System::UnicodeString __fastcall DoHTTP_POST_PRIVATE(const System::UnicodeString aRequestPath, const System::UnicodeString aBody);
	virtual System::UnicodeString __fastcall DoHTTP_DELETE_PRIVATE(const System::UnicodeString aRequestPath, System::UnicodeString aBody = System::UnicodeString());
	
public:
	__fastcall virtual TsgcHTTP_API_FTX(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcHTTP_API_FTX();
	
private:
	TsgcHTTPFTX_Options* FFTXOptions;
	void __fastcall SetFTXOptions(TsgcHTTPFTX_Options* const Value);
	
public:
	__property TsgcHTTPFTX_Options* FTXOptions = {read=FFTXOptions, write=SetFTXOptions};
};


class PASCALIMPLEMENTATION TsgcHTTP_API_FTX_Rest : public TsgcHTTP_API_FTX
{
	typedef TsgcHTTP_API_FTX inherited;
	
public:
	System::UnicodeString __fastcall GetMarkets();
	System::UnicodeString __fastcall GetMarket(const System::UnicodeString aMarket);
	System::UnicodeString __fastcall GetOrderbook(const System::UnicodeString aMarket, System::Word aDepth = (System::Word)(0x14));
	System::UnicodeString __fastcall GetTrades(const System::UnicodeString aMarket, System::Word aLimit = (System::Word)(0x14), __int64 aStartTime = 0LL, __int64 aEndTime = 0LL);
	System::UnicodeString __fastcall GetHistoricalPrices(const System::UnicodeString aMarket, TsgcHTTPFTXResolution aResolution = (TsgcHTTPFTXResolution)(0x2), System::Word aLimit = (System::Word)(0x23), __int64 aStartTime = 0LL, __int64 aEndTime = 0LL);
	System::UnicodeString __fastcall GetFutures();
	System::UnicodeString __fastcall GetFuture(const System::UnicodeString aFuture);
	System::UnicodeString __fastcall GetFutureStats(const System::UnicodeString aFuture);
	System::UnicodeString __fastcall GetFundingRates(const System::UnicodeString aFuture = System::UnicodeString(), __int64 aStartTime = 0LL, __int64 aEndTime = 0LL);
	System::UnicodeString __fastcall GetIndexWeights(const System::UnicodeString aFuture);
	System::UnicodeString __fastcall GetExpiredFutures();
	System::UnicodeString __fastcall GetHistoricalIndex(const System::UnicodeString aFuture, TsgcHTTPFTXResolution aResolution = (TsgcHTTPFTXResolution)(0x2), System::Word aLimit = (System::Word)(0x23), __int64 aStartTime = 0LL, __int64 aEndTime = 0LL);
	System::UnicodeString __fastcall GetAccount();
	System::UnicodeString __fastcall GetPositions();
	System::UnicodeString __fastcall ChangeAccountLeverage(System::Word aLeverage);
	System::UnicodeString __fastcall GetAllSubaccounts();
	System::UnicodeString __fastcall CreateSubaccount(const System::UnicodeString aNickname);
	System::UnicodeString __fastcall ChangeSubaccountName(const System::UnicodeString aOldNickname, const System::UnicodeString aNewNickname);
	System::UnicodeString __fastcall DeleteSubaccount(const System::UnicodeString aNickname);
	System::UnicodeString __fastcall GetSubaccountBalances(const System::UnicodeString aNickname);
	System::UnicodeString __fastcall TransferBetweenSubaccounts(const System::UnicodeString aCoin, System::Extended aSize, const System::UnicodeString aSource, const System::UnicodeString aDestination);
	
private:
	System::UnicodeString __fastcall GetJSONValueNull(const System::UnicodeString aValue);
	System::UnicodeString __fastcall GetJSONValueBoolean(const bool aValue);
	
public:
	System::UnicodeString __fastcall GetCoins();
	System::UnicodeString __fastcall GetBalances();
	System::UnicodeString __fastcall GetBalancesAllAccounts();
	System::UnicodeString __fastcall GetDepositAddress(const System::UnicodeString aCoin, const System::UnicodeString aMethod);
	System::UnicodeString __fastcall GetDepositHistory(__int64 aStartTime = 0LL, __int64 aEndTime = 0LL);
	System::UnicodeString __fastcall GetWithdrawalHistory(__int64 aStartTime = 0LL, __int64 aEndTime = 0LL);
	System::UnicodeString __fastcall RequestWithdrawal(const System::UnicodeString aCoin, System::Extended aSize, const System::UnicodeString aAddress, const System::UnicodeString aTag = System::UnicodeString(), const System::UnicodeString aPassword = System::UnicodeString(), const System::UnicodeString aCode = System::UnicodeString());
	System::UnicodeString __fastcall GetAirDrops(__int64 aStartTime = 0LL, __int64 aEndTime = 0LL);
	System::UnicodeString __fastcall GetWithdrawalFees(const System::UnicodeString aCoin, System::Extended aSize, const System::UnicodeString aAddress, const System::UnicodeString aTag = System::UnicodeString());
	System::UnicodeString __fastcall GetSavedAddresses(const System::UnicodeString aCoin = System::UnicodeString());
	System::UnicodeString __fastcall CreateSavedAddresses(const System::UnicodeString aCoin, const System::UnicodeString aAddress, const System::UnicodeString aAddressName, const bool aIsPrimetrust, const System::UnicodeString aTag);
	System::UnicodeString __fastcall DeleteSavedAddresses(int aSavedAddressId);
	System::UnicodeString __fastcall GetOpenOrders(const System::UnicodeString aMarket);
	System::UnicodeString __fastcall GetOrderHistory(const System::UnicodeString aMarket, System::Word aLimit = (System::Word)(0x64), __int64 aStartTime = 0LL, __int64 aEndTime = 0LL);
	System::UnicodeString __fastcall GetOpenTriggerOrders(const System::UnicodeString aMarket, TsgcHTTPFTXTriggerOrderType aTriggerOrder);
	System::UnicodeString __fastcall GetTriggerOrderTriggers(__int64 aOrderId);
	System::UnicodeString __fastcall GetTriggerOrderHistory(const System::UnicodeString aMarket = System::UnicodeString(), __int64 aStartTime = 0LL, __int64 aEndTime = 0LL, TsgcHTTPFTXOrderSide aSide = (TsgcHTTPFTXOrderSide)(0x0), TsgcHTTPFTXTriggerOrderType aType = (TsgcHTTPFTXTriggerOrderType)(0x0), TsgcHTTPFTXOrderType aOrderType = (TsgcHTTPFTXOrderType)(0x0), System::Word aLimit = (System::Word)(0x64));
	System::UnicodeString __fastcall PlaceOrder(TsgcHTTPFTXOrder* const aOrder);
	System::UnicodeString __fastcall PlaceMarketOrder(const System::UnicodeString aMarket, TsgcHTTPFTXOrderSide aSide, System::Extended aSize, const System::UnicodeString aClientId = System::UnicodeString());
	System::UnicodeString __fastcall PlaceLimitOrder(const System::UnicodeString aMarket, TsgcHTTPFTXOrderSide aSide, System::Extended aSize, System::Extended aPrice, const System::UnicodeString aClientId = System::UnicodeString());
	System::UnicodeString __fastcall PlaceTriggerOrder(TsgcHTTPFTXTriggerOrder* const aOrder);
	System::UnicodeString __fastcall PlaceTriggerStopOrder(const System::UnicodeString aMarket, TsgcHTTPFTXOrderSide aSide, System::Extended aSize, System::Extended aTriggerPrice, System::Extended aOrderPrice);
	System::UnicodeString __fastcall PlaceTriggerTrailingStopOrder(const System::UnicodeString aMarket, TsgcHTTPFTXOrderSide aSide, System::Extended aSize, System::Extended aTrailValue);
	System::UnicodeString __fastcall PlaceTriggerTakeProfitOrder(const System::UnicodeString aMarket, TsgcHTTPFTXOrderSide aSide, System::Extended aSize, System::Extended aTriggerPrice, System::Extended aOrderPrice);
	System::UnicodeString __fastcall ModifyOrder(__int64 aOrderId, System::Extended aPrice, System::Extended aSize, System::UnicodeString aClientId = System::UnicodeString());
	System::UnicodeString __fastcall ModifyOrderByClientId(System::UnicodeString aClientOrderId, System::Extended aPrice, System::Extended aSize, System::UnicodeString aClientId = System::UnicodeString());
	System::UnicodeString __fastcall ModifyTriggerOrder_StopLoss(__int64 aOrderId, System::Extended aSize, System::Extended aTriggerPrice, System::Extended aOrderPrice);
	System::UnicodeString __fastcall ModifyTriggerOrder_TakeProfit(__int64 aOrderId, System::Extended aSize, System::Extended aTriggerPrice, System::Extended aOrderPrice);
	System::UnicodeString __fastcall ModifyTriggerOrder_TrailingStop(__int64 aOrderId, System::Extended aSize, System::Extended aTrailValue);
	System::UnicodeString __fastcall GetOrderStatus(__int64 aOrderId);
	System::UnicodeString __fastcall GetOrderStatusByClientId(const System::UnicodeString aClientOrderId);
	System::UnicodeString __fastcall CancelOrder(__int64 aOrderId);
	System::UnicodeString __fastcall CancelOrderByClientId(const System::UnicodeString aClientOrderId, const System::UnicodeString aClientOrderId1);
	System::UnicodeString __fastcall CancelOpenTriggerOrder(__int64 aOrderId);
	System::UnicodeString __fastcall CancelAllOrders(const System::UnicodeString aMarket = System::UnicodeString(), bool aConditionalOrdersOnly = false, bool aLimitOrdersOnly = false);
	System::UnicodeString __fastcall GetFills(const System::UnicodeString aMarket, System::Word aLimit = (System::Word)(0x14), __int64 aStartTime = 0LL, __int64 aEndTime = 0LL, bool aAscending = false, const System::UnicodeString aOrderId = System::UnicodeString());
public:
	/* TsgcHTTP_API_FTX.Create */ inline __fastcall virtual TsgcHTTP_API_FTX_Rest(System::Classes::TComponent* aOwner) : TsgcHTTP_API_FTX(aOwner) { }
	/* TsgcHTTP_API_FTX.Destroy */ inline __fastcall virtual ~TsgcHTTP_API_FTX_Rest() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgchttp_api_ftx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCHTTP_API_FTX)
using namespace Sgchttp_api_ftx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgchttp_api_ftxHPP
