// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcHTTP_API_Coinbase.pas' rev: 35.00 (Windows)

#ifndef Sgchttp_api_coinbaseHPP
#define Sgchttp_api_coinbaseHPP

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

namespace Sgchttp_api_coinbase
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcHTTPCoinbaseOrder;
class DELPHICLASS TsgcHTTPCoinbaseLog_Options;
class DELPHICLASS TsgcHTTPCoinbase_Options;
class DELPHICLASS TsgcHTTP_API_Coinbase;
class DELPHICLASS TsgcHTTP_API_Coinbase_Rest;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TsgcHTTPCoinbaseOrderType : unsigned char { coioLimit, coioMarket };

enum DECLSPEC_DENUM TsgcHTTPCoinbaseOrderSide : unsigned char { coisBuy, coisSell };

enum DECLSPEC_DENUM TsgcHTTPCoinbaseOrderStop : unsigned char { costNone, costLoss, costEntry };

enum DECLSPEC_DENUM TsgcHTTPCoinbaseTimeInForce : unsigned char { cbtfNone, cbtfGTC, cbtfGTT, cbtfIOC, cbtfFOK };

enum DECLSPEC_DENUM TsgcHTTPCoinbaseSelfTradePrevention : unsigned char { stpNone, stpDC, stpCO, stpCN, stpCB };

enum DECLSPEC_DENUM TsgcHTTPCoinbaseSizeType : unsigned char { csztNone, csztUseSize, csztUseFunds };

enum DECLSPEC_DENUM TsgcHTTPCoinbaseOrderStatus : unsigned char { cbosOpen, cbosPending, cboActive };

typedef System::Set<TsgcHTTPCoinbaseOrderStatus, TsgcHTTPCoinbaseOrderStatus::cbosOpen, TsgcHTTPCoinbaseOrderStatus::cboActive> TsgcHTTPCoinbaseOrderStatusSet;

class PASCALIMPLEMENTATION TsgcHTTPCoinbaseOrder : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FCancel_after;
	System::UnicodeString FClient_oid;
	bool FPost_only;
	System::Extended FPrice;
	System::UnicodeString FProduct_id;
	TsgcHTTPCoinbaseOrderSide FSide;
	System::Extended FSize;
	TsgcHTTPCoinbaseSizeType FSizeType;
	TsgcHTTPCoinbaseOrderStop FStop;
	System::Extended FStop_price;
	TsgcHTTPCoinbaseSelfTradePrevention FStp;
	TsgcHTTPCoinbaseTimeInForce FTimeInForce;
	TsgcHTTPCoinbaseOrderType F_Type;
	System::UnicodeString __fastcall GetOrderTypeString(TsgcHTTPCoinbaseOrderType aType);
	System::UnicodeString __fastcall GetOrderSideString(TsgcHTTPCoinbaseOrderSide aSide);
	System::UnicodeString __fastcall GetOrderStopString(TsgcHTTPCoinbaseOrderStop aOrderStop);
	System::UnicodeString __fastcall GetTimeInForceString(TsgcHTTPCoinbaseTimeInForce aTimeInForce);
	System::UnicodeString __fastcall GetSelfTradePreventionString(TsgcHTTPCoinbaseSelfTradePrevention aSelfTradePrevention);
	System::UnicodeString __fastcall GetOrderSizeTypeString(TsgcHTTPCoinbaseSizeType aSizeType);
	
protected:
	virtual System::UnicodeString __fastcall GetJSON();
	
public:
	__property System::UnicodeString Cancel_after = {read=FCancel_after, write=FCancel_after};
	__property System::UnicodeString Client_oid = {read=FClient_oid, write=FClient_oid};
	__property bool Post_only = {read=FPost_only, write=FPost_only, nodefault};
	__property System::Extended Price = {read=FPrice, write=FPrice};
	__property System::UnicodeString Product_id = {read=FProduct_id, write=FProduct_id};
	__property TsgcHTTPCoinbaseOrderSide Side = {read=FSide, write=FSide, nodefault};
	__property System::Extended Size = {read=FSize, write=FSize};
	__property TsgcHTTPCoinbaseSizeType SizeType = {read=FSizeType, write=FSizeType, nodefault};
	__property TsgcHTTPCoinbaseOrderStop Stop = {read=FStop, write=FStop, nodefault};
	__property System::Extended Stop_price = {read=FStop_price, write=FStop_price};
	__property TsgcHTTPCoinbaseSelfTradePrevention Stp = {read=FStp, write=FStp, nodefault};
	__property TsgcHTTPCoinbaseTimeInForce TimeInForce = {read=FTimeInForce, write=FTimeInForce, nodefault};
	__property TsgcHTTPCoinbaseOrderType _Type = {read=F_Type, write=F_Type, nodefault};
public:
	/* TObject.Create */ inline __fastcall TsgcHTTPCoinbaseOrder() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcHTTPCoinbaseOrder() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTPCoinbaseLog_Options : public System::Classes::TPersistent
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
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcHTTPCoinbaseLog_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcHTTPCoinbaseLog_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTPCoinbase_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FApiKey;
	System::UnicodeString FApiSecret;
	TsgcHTTPCoinbaseLog_Options* FLogOptions;
	System::UnicodeString FPassphrase;
	bool FSandBox;
	void __fastcall SetLogOptions(TsgcHTTPCoinbaseLog_Options* const Value);
	
public:
	__fastcall virtual TsgcHTTPCoinbase_Options();
	__fastcall virtual ~TsgcHTTPCoinbase_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString ApiKey = {read=FApiKey, write=FApiKey};
	__property System::UnicodeString ApiSecret = {read=FApiSecret, write=FApiSecret};
	__property TsgcHTTPCoinbaseLog_Options* LogOptions = {read=FLogOptions, write=SetLogOptions};
	__property System::UnicodeString Passphrase = {read=FPassphrase, write=FPassphrase};
	__property bool SandBox = {read=FSandBox, write=FSandBox, nodefault};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcHTTP_API_Coinbase : public Sgchttp_api::TsgcHTTPAPI_client
{
	typedef Sgchttp_api::TsgcHTTPAPI_client inherited;
	
private:
	void __fastcall DoInitializeLog();
	System::UnicodeString __fastcall GetBaseURL();
	
protected:
	virtual System::UnicodeString __fastcall DoHTTP_GET(const System::UnicodeString aMethod, const System::UnicodeString aParameters = System::UnicodeString(), System::Classes::TStrings* const aHeaders = (System::Classes::TStrings*)(0x0));
	virtual System::UnicodeString __fastcall DoHTTP_POST(const System::UnicodeString aMethod, const System::UnicodeString aBody = System::UnicodeString(), System::Classes::TStrings* const aHeaders = (System::Classes::TStrings*)(0x0));
	virtual System::UnicodeString __fastcall DoHTTP_DELETE(const System::UnicodeString aMethod, const System::UnicodeString aParameters = System::UnicodeString(), System::Classes::TStrings* const aHeaders = (System::Classes::TStrings*)(0x0));
	virtual void __fastcall GetAuthHeaders(const System::UnicodeString aMethod, const System::UnicodeString aRequestPath, const System::UnicodeString aBody, System::Classes::TStringList* &Headers);
	virtual System::UnicodeString __fastcall DoHTTP_GET_PRIVATE(const System::UnicodeString aRequestPath);
	virtual System::UnicodeString __fastcall DoHTTP_POST_PRIVATE(const System::UnicodeString aRequestPath, const System::UnicodeString aBody);
	virtual System::UnicodeString __fastcall DoHTTP_DELETE_PRIVATE(const System::UnicodeString aRequestPath);
	
public:
	__fastcall virtual TsgcHTTP_API_Coinbase(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcHTTP_API_Coinbase();
	
private:
	TsgcHTTPCoinbase_Options* FCoinbaseOptions;
	void __fastcall SetCoinbaseOptions(TsgcHTTPCoinbase_Options* const Value);
	
public:
	__property TsgcHTTPCoinbase_Options* CoinbaseOptions = {read=FCoinbaseOptions, write=SetCoinbaseOptions};
};


class PASCALIMPLEMENTATION TsgcHTTP_API_Coinbase_Rest : public TsgcHTTP_API_Coinbase
{
	typedef TsgcHTTP_API_Coinbase inherited;
	
public:
	System::UnicodeString __fastcall ListAccounts();
	System::UnicodeString __fastcall GetAccount(const System::UnicodeString aAccountId);
	System::UnicodeString __fastcall GetAccountHistory(const System::UnicodeString aAccountId);
	System::UnicodeString __fastcall GetHolds(const System::UnicodeString aAccountId);
	
protected:
	virtual System::UnicodeString __fastcall DoPlaceNewOrder(TsgcHTTPCoinbaseOrder* const aOrder);
	
public:
	System::UnicodeString __fastcall PlaceNewOrder(TsgcHTTPCoinbaseOrder* const aOrder);
	System::UnicodeString __fastcall PlaceMarketOrder(const TsgcHTTPCoinbaseOrderSide aSide, const System::UnicodeString aProductId, const System::Extended aAmount, bool aUseFunds = false, System::UnicodeString aClient_oid = System::UnicodeString());
	System::UnicodeString __fastcall PlaceLimitOrder(const TsgcHTTPCoinbaseOrderSide aSide, const System::UnicodeString aProductId, System::Extended aSize, System::Extended aLimitPrice, TsgcHTTPCoinbaseTimeInForce aTimeInForce = (TsgcHTTPCoinbaseTimeInForce)(0x0), bool aPostOnly = false, System::UnicodeString aClient_oid = System::UnicodeString());
	System::UnicodeString __fastcall PlaceStopOrder(const TsgcHTTPCoinbaseOrderSide aSide, const System::UnicodeString aProductId, System::Extended aSize, System::Extended aStopPrice, System::Extended aLimitPrice, TsgcHTTPCoinbaseOrderStop aStop, System::UnicodeString aClient_oid = System::UnicodeString());
	System::UnicodeString __fastcall CancelOrder(const System::UnicodeString aOrderId);
	System::UnicodeString __fastcall CancelOrdersClient(const System::UnicodeString aClient_oid);
	System::UnicodeString __fastcall CancelAllOrders();
	System::UnicodeString __fastcall ListOrders(const System::UnicodeString aProductId = System::UnicodeString(), TsgcHTTPCoinbaseOrderStatusSet aStatus = (TsgcHTTPCoinbaseOrderStatusSet() << TsgcHTTPCoinbaseOrderStatus::cbosOpen << TsgcHTTPCoinbaseOrderStatus::cbosPending << TsgcHTTPCoinbaseOrderStatus::cboActive ));
	System::UnicodeString __fastcall GetOrder(const System::UnicodeString aOrderId);
	System::UnicodeString __fastcall GetOrdersClient(const System::UnicodeString aClient_oid);
	System::UnicodeString __fastcall GetFillsByOrderId(const System::UnicodeString aOrderId);
	System::UnicodeString __fastcall GetFillsByProductId(const System::UnicodeString aProductId);
	System::UnicodeString __fastcall GetCurrentExchangeLimits();
	System::UnicodeString __fastcall ListDeposits(const System::UnicodeString aProfileId = System::UnicodeString(), const System::TDateTime aBefore = 0.000000E+00, const System::TDateTime aAfter = 0.000000E+00, const int aLimit = 0x64);
	System::UnicodeString __fastcall GetDeposit(const System::UnicodeString aTransferId);
	System::UnicodeString __fastcall DepositPaymentMethod(double aAmount, const System::UnicodeString aCurrency, const System::UnicodeString aPaymentMethodId);
	System::UnicodeString __fastcall DepositCoinbase(double aAmount, const System::UnicodeString aCurrency, const System::UnicodeString aCoinbaseAccountId);
	System::UnicodeString __fastcall DepositGenerateAddress(const System::UnicodeString aCoinbaseAccountId);
	System::UnicodeString __fastcall ListWithdrawals(const System::UnicodeString aProfileId = System::UnicodeString(), const System::TDateTime aBefore = 0.000000E+00, const System::TDateTime aAfter = 0.000000E+00, const int aLimit = 0x64);
	System::UnicodeString __fastcall GetWithdrawal(const System::UnicodeString aTransferId);
	System::UnicodeString __fastcall WithdrawalPaymentMethod(double aAmount, const System::UnicodeString aCurrency, const System::UnicodeString aPaymentMethodId);
	System::UnicodeString __fastcall WithdrawalCoinbase(double aAmount, const System::UnicodeString aCurrency, const System::UnicodeString aCoinbaseAccountId);
	System::UnicodeString __fastcall WithdrawalCrypto(double aAmount, const System::UnicodeString aCurrency, const System::UnicodeString aCryptoAddress);
	System::UnicodeString __fastcall GetWithdrawalFeeEstimate(const System::UnicodeString aCurrency, const System::UnicodeString aCryptoAddress);
	System::UnicodeString __fastcall CreateConversion(const System::UnicodeString aFromCurrencyId, const System::UnicodeString aToCurrencyId, double aAmount);
	System::UnicodeString __fastcall ListPaymentMethods();
	System::UnicodeString __fastcall CoinbaseListAccounts();
	System::UnicodeString __fastcall GetFees();
	System::UnicodeString __fastcall CreateReportFills(System::TDateTime aStartDate, System::TDateTime aEndDate, const System::UnicodeString aProductId, const System::UnicodeString aFormat = L"pdf", const System::UnicodeString aEmail = System::UnicodeString());
	System::UnicodeString __fastcall CreateReportAccount(System::TDateTime aStartDate, System::TDateTime aEndDate, const System::UnicodeString aAccountId, const System::UnicodeString aFormat = L"pdf", const System::UnicodeString aEmail = System::UnicodeString());
	System::UnicodeString __fastcall GetReportStatus(const System::UnicodeString aReportId);
	System::UnicodeString __fastcall ListProfiles(const bool aOnlyActive = false);
	System::UnicodeString __fastcall GetProfile(const System::UnicodeString aProfileId);
	System::UnicodeString __fastcall CreateProfileTransfer(const System::UnicodeString aFromProfileId, const System::UnicodeString aToProfileId, const System::UnicodeString aCurrency, double aAmount);
	System::UnicodeString __fastcall GetMarginProfileInformation(const System::UnicodeString aProductId);
	System::UnicodeString __fastcall GetMarginBuyingPower(const System::UnicodeString aProductId);
	System::UnicodeString __fastcall GetMarginWithdrawalPower(const System::UnicodeString aCurrency);
	System::UnicodeString __fastcall GetMarginAllWithdrawalPowers();
	System::UnicodeString __fastcall GetMarginExitPlan();
	System::UnicodeString __fastcall GetMarginListLiquidationHistory(System::TDateTime aAfterDate = 0.000000E+00);
	System::UnicodeString __fastcall GetMarginPositionRefreshAmounts();
	System::UnicodeString __fastcall GetMarginStatus();
	System::UnicodeString __fastcall GetOracle();
	System::UnicodeString __fastcall GetProducts();
	System::UnicodeString __fastcall GetSingleProduct(const System::UnicodeString aProductId);
	System::UnicodeString __fastcall GetProductOrderBook(const System::UnicodeString aProductId);
	System::UnicodeString __fastcall GetProductTicker(const System::UnicodeString aProductId);
	System::UnicodeString __fastcall GetTrades(const System::UnicodeString aProductId);
	System::UnicodeString __fastcall GetHistoricRates(const System::UnicodeString aProductId);
	System::UnicodeString __fastcall Get24hrStats(const System::UnicodeString aProductId);
	System::UnicodeString __fastcall GetCurrencies();
	System::UnicodeString __fastcall GetCurrency(const System::UnicodeString aProductId);
	System::UnicodeString __fastcall GetTime();
public:
	/* TsgcHTTP_API_Coinbase.Create */ inline __fastcall virtual TsgcHTTP_API_Coinbase_Rest(System::Classes::TComponent* aOwner) : TsgcHTTP_API_Coinbase(aOwner) { }
	/* TsgcHTTP_API_Coinbase.Destroy */ inline __fastcall virtual ~TsgcHTTP_API_Coinbase_Rest() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgchttp_api_coinbase */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCHTTP_API_COINBASE)
using namespace Sgchttp_api_coinbase;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgchttp_api_coinbaseHPP
