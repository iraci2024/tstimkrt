// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcHTTP_API_ThreeCommas.pas' rev: 35.00 (Windows)

#ifndef Sgchttp_api_threecommasHPP
#define Sgchttp_api_threecommasHPP

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

namespace Sgchttp_api_threecommas
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcHTTPThreeCommasOrderTrailing;
class DELPHICLASS TsgcHTTPThreeCommasOrderPrice;
class DELPHICLASS TsgcHTTPThreeCommasOrderConditional;
class DELPHICLASS TsgcHTTPThreeCommasOrderPosition;
class DELPHICLASS TsgcHTTPThreeCommasOrderLeverage;
class DELPHICLASS TsgcHTTPThreeCommasOrder;
class DELPHICLASS TsgcHTTPThreeCommasLog_Options;
class DELPHICLASS TsgcHTTPThreeCommas_Options;
class DELPHICLASS TsgcHTTP_API_3Commas;
class DELPHICLASS TsgcHTTP_API_3Commas_Rest;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TsgcHTTPThreeCommasOrderSide : unsigned char { os3cNone, os3cBuy, os3cSell };

enum DECLSPEC_DENUM TsgcHTTPThreeCommasOrderType : unsigned char { ot3cNone, ot3cMarket, ot3cLimit, ot3cConditional };

enum DECLSPEC_DENUM TsgcHTTPThreeCommasPriceType : unsigned char { pt3cNone, pt3cBid, pt3cAsk, pt3cLast };

enum DECLSPEC_DENUM TsgcHTTPThreeCommasLeverageType : unsigned char { lt3cNonce, lt3cCustom, lt3cCross };

class PASCALIMPLEMENTATION TsgcHTTPThreeCommasOrderTrailing : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	bool FEnabled;
	System::Extended FPercent;
	
public:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property System::Extended Percent = {read=FPercent, write=FPercent};
public:
	/* TObject.Create */ inline __fastcall TsgcHTTPThreeCommasOrderTrailing() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcHTTPThreeCommasOrderTrailing() { }
	
};


class PASCALIMPLEMENTATION TsgcHTTPThreeCommasOrderPrice : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Extended FPercent;
	System::Extended FValue;
	TsgcHTTPThreeCommasPriceType F_Type;
	
public:
	__property System::Extended Value = {read=FValue, write=FValue};
	__property TsgcHTTPThreeCommasPriceType _Type = {read=F_Type, write=F_Type, nodefault};
	__property System::Extended Percent = {read=FPercent, write=FPercent};
public:
	/* TObject.Create */ inline __fastcall TsgcHTTPThreeCommasOrderPrice() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcHTTPThreeCommasOrderPrice() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTPThreeCommasOrderConditional : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TsgcHTTPThreeCommasOrderType FOrderType;
	TsgcHTTPThreeCommasOrderPrice* FPrice;
	TsgcHTTPThreeCommasOrderTrailing* FTrailing;
	TsgcHTTPThreeCommasOrderPrice* __fastcall GetPrice();
	TsgcHTTPThreeCommasOrderTrailing* __fastcall GetTrailing();
	
public:
	__fastcall virtual ~TsgcHTTPThreeCommasOrderConditional();
	__property TsgcHTTPThreeCommasOrderPrice* Price = {read=GetPrice, write=FPrice};
	__property TsgcHTTPThreeCommasOrderType OrderType = {read=FOrderType, write=FOrderType, nodefault};
	__property TsgcHTTPThreeCommasOrderTrailing* Trailing = {read=GetTrailing, write=FTrailing};
public:
	/* TObject.Create */ inline __fastcall TsgcHTTPThreeCommasOrderConditional() : System::TObject() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcHTTPThreeCommasOrderPosition : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	TsgcHTTPThreeCommasOrderConditional* FConditional;
	TsgcHTTPThreeCommasOrderType FOrderType;
	TsgcHTTPThreeCommasOrderPrice* FPrice;
	System::Extended FUnits;
	TsgcHTTPThreeCommasOrderSide F_Type;
	TsgcHTTPThreeCommasOrderConditional* __fastcall GetConditional();
	TsgcHTTPThreeCommasOrderPrice* __fastcall GetPrice();
	
public:
	__fastcall virtual ~TsgcHTTPThreeCommasOrderPosition();
	__property TsgcHTTPThreeCommasOrderConditional* Conditional = {read=GetConditional, write=FConditional};
	__property TsgcHTTPThreeCommasOrderSide _Type = {read=F_Type, write=F_Type, nodefault};
	__property System::Extended Units = {read=FUnits, write=FUnits};
	__property TsgcHTTPThreeCommasOrderPrice* Price = {read=GetPrice, write=FPrice};
	__property TsgcHTTPThreeCommasOrderType OrderType = {read=FOrderType, write=FOrderType, nodefault};
public:
	/* TObject.Create */ inline __fastcall TsgcHTTPThreeCommasOrderPosition() : System::TObject() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTPThreeCommasOrderLeverage : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	bool FEnabled;
	int FValue;
	TsgcHTTPThreeCommasLeverageType F_Type;
	
public:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property TsgcHTTPThreeCommasLeverageType _Type = {read=F_Type, write=F_Type, nodefault};
	__property int Value = {read=FValue, write=FValue, nodefault};
public:
	/* TObject.Create */ inline __fastcall TsgcHTTPThreeCommasOrderLeverage() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcHTTPThreeCommasOrderLeverage() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTPThreeCommasOrder : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	int FAccountId;
	bool FInstant;
	TsgcHTTPThreeCommasOrderLeverage* FLeverage;
	System::UnicodeString FPair;
	TsgcHTTPThreeCommasOrderPosition* FPosition;
	TsgcHTTPThreeCommasOrderLeverage* __fastcall GetLeverage();
	TsgcHTTPThreeCommasOrderPosition* __fastcall GetPosition();
	
protected:
	virtual System::UnicodeString __fastcall GetJSON();
	
public:
	__fastcall virtual TsgcHTTPThreeCommasOrder();
	__fastcall virtual ~TsgcHTTPThreeCommasOrder();
	__property int AccountId = {read=FAccountId, write=FAccountId, nodefault};
	__property System::UnicodeString Pair = {read=FPair, write=FPair};
	__property bool Instant = {read=FInstant, write=FInstant, nodefault};
	__property TsgcHTTPThreeCommasOrderLeverage* Leverage = {read=GetLeverage, write=FLeverage};
	__property TsgcHTTPThreeCommasOrderPosition* Position = {read=GetPosition, write=FPosition};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTPThreeCommasLog_Options : public System::Classes::TPersistent
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
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcHTTPThreeCommasLog_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcHTTPThreeCommasLog_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTPThreeCommas_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FApiKey;
	System::UnicodeString FApiSecret;
	TsgcHTTPThreeCommasLog_Options* FLogOptions;
	System::UnicodeString FSubAccount;
	void __fastcall SetLogOptions(TsgcHTTPThreeCommasLog_Options* const Value);
	
public:
	__fastcall virtual TsgcHTTPThreeCommas_Options();
	__fastcall virtual ~TsgcHTTPThreeCommas_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString ApiKey = {read=FApiKey, write=FApiKey};
	__property System::UnicodeString ApiSecret = {read=FApiSecret, write=FApiSecret};
	__property TsgcHTTPThreeCommasLog_Options* LogOptions = {read=FLogOptions, write=SetLogOptions};
	__property System::UnicodeString SubAccount = {read=FSubAccount, write=FSubAccount};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcHTTP_API_3Commas : public Sgchttp_api::TsgcHTTPAPI_client
{
	typedef Sgchttp_api::TsgcHTTPAPI_client inherited;
	
private:
	void __fastcall DoInitializeLog();
	System::UnicodeString __fastcall GetBaseURL();
	
protected:
	virtual System::UnicodeString __fastcall DoHTTP_GET(const System::UnicodeString aMethod, const System::UnicodeString aParameters = System::UnicodeString(), System::Classes::TStrings* const aHeaders = (System::Classes::TStrings*)(0x0));
	virtual System::UnicodeString __fastcall DoHTTP_POST(const System::UnicodeString aMethod, const System::UnicodeString aBody = System::UnicodeString(), System::Classes::TStrings* const aHeaders = (System::Classes::TStrings*)(0x0));
	virtual System::UnicodeString __fastcall DoHTTP_DELETE(const System::UnicodeString aMethod, const System::UnicodeString aParameters = System::UnicodeString(), System::Classes::TStrings* const aHeaders = (System::Classes::TStrings*)(0x0), const System::UnicodeString aBody = System::UnicodeString());
	virtual void __fastcall GetAuthHeaders(const System::UnicodeString aRequestPath, const System::UnicodeString aBody, System::Classes::TStringList* &Headers);
	virtual System::UnicodeString __fastcall DoHTTP_GET_PRIVATE(const System::UnicodeString aRequestPath);
	virtual System::UnicodeString __fastcall DoHTTP_POST_PRIVATE(const System::UnicodeString aRequestPath, const System::UnicodeString aBody);
	virtual System::UnicodeString __fastcall DoHTTP_DELETE_PRIVATE(const System::UnicodeString aRequestPath, System::UnicodeString aBody = System::UnicodeString());
	
public:
	__fastcall virtual TsgcHTTP_API_3Commas(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcHTTP_API_3Commas();
	
private:
	TsgcHTTPThreeCommas_Options* FThreeCommas;
	void __fastcall SetThreeCommas(TsgcHTTPThreeCommas_Options* const Value);
	
public:
	__property TsgcHTTPThreeCommas_Options* ThreeCommas = {read=FThreeCommas, write=SetThreeCommas};
};


class PASCALIMPLEMENTATION TsgcHTTP_API_3Commas_Rest : public TsgcHTTP_API_3Commas
{
	typedef TsgcHTTP_API_3Commas inherited;
	
public:
	System::UnicodeString __fastcall GetPing();
	System::UnicodeString __fastcall GetServerTime();
	System::UnicodeString __fastcall GetAccounts();
	System::UnicodeString __fastcall GetMarketList();
	System::UnicodeString __fastcall GetMarketPairs(const System::UnicodeString aMarketCode = System::UnicodeString());
	System::UnicodeString __fastcall GetCurrencyRatesWithLeverageData(const System::UnicodeString aMarketCode, const System::UnicodeString aPair);
	System::UnicodeString __fastcall GetCurrencyRates(const System::UnicodeString aMarketCode, const System::UnicodeString aPair);
	System::UnicodeString __fastcall GetBalances(int aAccountId);
	System::UnicodeString __fastcall GetAccountTableData(int aAccountId);
	System::UnicodeString __fastcall GetAccountLeverage(int aAccountId, const System::UnicodeString aPair);
	System::UnicodeString __fastcall GetAccountInfo(int aAccountId);
	System::UnicodeString __fastcall GetSmartTradeHistory();
	System::UnicodeString __fastcall CreateSmartTrade(TsgcHTTPThreeCommasOrder* const aOrder);
	System::UnicodeString __fastcall PlaceMarketOrder(int aAccountId, TsgcHTTPThreeCommasOrderSide aOrderSide, const System::UnicodeString aPair, System::Extended aQuantity);
	System::UnicodeString __fastcall PlaceLimitOrder(int aAccountId, TsgcHTTPThreeCommasOrderSide aOrderSide, const System::UnicodeString aPair, System::Extended aQuantity, System::Extended aPrice);
	System::UnicodeString __fastcall GetSmartTrade(const int aId);
	System::UnicodeString __fastcall CancelSmartTrade(const int aId);
	System::UnicodeString __fastcall CloseByMarketSmartTrade(const int aId);
public:
	/* TsgcHTTP_API_3Commas.Create */ inline __fastcall virtual TsgcHTTP_API_3Commas_Rest(System::Classes::TComponent* aOwner) : TsgcHTTP_API_3Commas(aOwner) { }
	/* TsgcHTTP_API_3Commas.Destroy */ inline __fastcall virtual ~TsgcHTTP_API_3Commas_Rest() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgchttp_api_threecommas */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCHTTP_API_THREECOMMAS)
using namespace Sgchttp_api_threecommas;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgchttp_api_threecommasHPP
