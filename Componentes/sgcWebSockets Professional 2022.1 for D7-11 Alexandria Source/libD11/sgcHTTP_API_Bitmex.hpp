// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcHTTP_API_Bitmex.pas' rev: 35.00 (Windows)

#ifndef Sgchttp_api_bitmexHPP
#define Sgchttp_api_bitmexHPP

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

namespace Sgchttp_api_bitmex
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcHTTPBitmexOrder;
class DELPHICLASS TsgcHTTPBitmexAmendOrder;
class DELPHICLASS TsgcHTTPBitmexLog_Options;
class DELPHICLASS TsgcHTTPBitmex_Options;
class DELPHICLASS TsgcHTTP_API_Bitmex;
class DELPHICLASS TsgcHTTP_API_Bitmex_Rest;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TsgcHTTPBitmexOrderSide : unsigned char { bmosNone, bmosBuy, bmosSell };

enum DECLSPEC_DENUM TsgcHTTPBitmexOrderType : unsigned char { bmotNone, bmotMarket, bmotLimit, bmotStop, bmotStopLimit, bmotMarketIfTouched, bmotLimitIfTouched, bmotPegged };

enum DECLSPEC_DENUM TsgcHTTPBitmexPegPriceType : unsigned char { bmpptNone, bmpptMarketPeg, bmpptPrimaryPeg, bmpptTrailingStopPeg };

enum DECLSPEC_DENUM TsgcHTTPBitmexTimeInForce : unsigned char { bmtifNone, bmtifDay, bmtifGoodTillCancel, bmtifImmediateOrCancel, bmtifFillOrKill };

enum DECLSPEC_DENUM TsgcHTTPBitmexExecInst : unsigned char { bmeiNone, bmeiParticipateDoNotInitiate, bmeiAllOrNone, bmeiMarkPrice, bmeiIndexPrice, bmeiLastPrice, bmeiClose, bmeiReduceOnly, bmeiFixed, bmeiLastWithinMark };

class PASCALIMPLEMENTATION TsgcHTTPBitmexOrder : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FClOrdId;
	System::Extended FDisplayQty;
	TsgcHTTPBitmexTimeInForce FTimeInForce;
	System::UnicodeString FSymbol;
	TsgcHTTPBitmexExecInst FExecInst;
	System::Extended FPrice;
	System::UnicodeString FText;
	TsgcHTTPBitmexOrderSide FSide;
	System::Extended FOrderQty;
	System::Extended FPegOffsetValue;
	TsgcHTTPBitmexPegPriceType FPegPriceType;
	System::Extended FStopPx;
	TsgcHTTPBitmexOrderType FOrdType;
	
protected:
	virtual System::UnicodeString __fastcall GetJSON();
	
public:
	__fastcall virtual TsgcHTTPBitmexOrder();
	__property System::UnicodeString Symbol = {read=FSymbol, write=FSymbol};
	__property TsgcHTTPBitmexOrderSide Side = {read=FSide, write=FSide, nodefault};
	__property System::Extended OrderQty = {read=FOrderQty, write=FOrderQty};
	__property System::Extended Price = {read=FPrice, write=FPrice};
	__property System::Extended DisplayQty = {read=FDisplayQty, write=FDisplayQty};
	__property System::Extended StopPx = {read=FStopPx, write=FStopPx};
	__property System::UnicodeString ClOrdId = {read=FClOrdId, write=FClOrdId};
	__property System::Extended PegOffsetValue = {read=FPegOffsetValue, write=FPegOffsetValue};
	__property TsgcHTTPBitmexPegPriceType PegPriceType = {read=FPegPriceType, write=FPegPriceType, nodefault};
	__property TsgcHTTPBitmexOrderType OrdType = {read=FOrdType, write=FOrdType, nodefault};
	__property TsgcHTTPBitmexTimeInForce TimeInForce = {read=FTimeInForce, write=FTimeInForce, nodefault};
	__property TsgcHTTPBitmexExecInst ExecInst = {read=FExecInst, write=FExecInst, nodefault};
	__property System::UnicodeString Text = {read=FText, write=FText};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcHTTPBitmexOrder() { }
	
};


class PASCALIMPLEMENTATION TsgcHTTPBitmexAmendOrder : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FClOrdId;
	System::Extended FLeavesQty;
	System::UnicodeString FOrderId;
	System::Extended FPrice;
	System::UnicodeString FText;
	System::UnicodeString FOrigClOrdID;
	System::Extended FOrderQty;
	System::Extended FPegOffsetValue;
	System::Extended FStopPx;
	
protected:
	virtual System::UnicodeString __fastcall GetJSON();
	
public:
	__fastcall virtual TsgcHTTPBitmexAmendOrder();
	__property System::UnicodeString OrderId = {read=FOrderId, write=FOrderId};
	__property System::UnicodeString OrigClOrdID = {read=FOrigClOrdID, write=FOrigClOrdID};
	__property System::UnicodeString ClOrdId = {read=FClOrdId, write=FClOrdId};
	__property System::Extended OrderQty = {read=FOrderQty, write=FOrderQty};
	__property System::Extended LeavesQty = {read=FLeavesQty, write=FLeavesQty};
	__property System::Extended Price = {read=FPrice, write=FPrice};
	__property System::Extended StopPx = {read=FStopPx, write=FStopPx};
	__property System::Extended PegOffsetValue = {read=FPegOffsetValue, write=FPegOffsetValue};
	__property System::UnicodeString Text = {read=FText, write=FText};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcHTTPBitmexAmendOrder() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTPBitmexLog_Options : public System::Classes::TPersistent
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
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcHTTPBitmexLog_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcHTTPBitmexLog_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTPBitmex_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FApiKey;
	System::UnicodeString FApiSecret;
	int FExpires;
	TsgcHTTPBitmexLog_Options* FLogOptions;
	bool FTestNet;
	void __fastcall SetLogOptions(TsgcHTTPBitmexLog_Options* const Value);
	
public:
	__fastcall virtual TsgcHTTPBitmex_Options();
	__fastcall virtual ~TsgcHTTPBitmex_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString ApiKey = {read=FApiKey, write=FApiKey};
	__property System::UnicodeString ApiSecret = {read=FApiSecret, write=FApiSecret};
	__property int Expires = {read=FExpires, write=FExpires, nodefault};
	__property TsgcHTTPBitmexLog_Options* LogOptions = {read=FLogOptions, write=SetLogOptions};
	__property bool TestNet = {read=FTestNet, write=FTestNet, nodefault};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcHTTP_API_Bitmex : public Sgchttp_api::TsgcHTTPAPI_client
{
	typedef Sgchttp_api::TsgcHTTPAPI_client inherited;
	
private:
	void __fastcall DoInitializeLog();
	System::UnicodeString __fastcall GetBaseURL();
	
protected:
	virtual System::UnicodeString __fastcall DoHTTP_GET(const System::UnicodeString aMethod, const System::UnicodeString aParameters = System::UnicodeString(), System::Classes::TStrings* const aHeaders = (System::Classes::TStrings*)(0x0));
	virtual System::UnicodeString __fastcall DoHTTP_POST(const System::UnicodeString aMethod, const System::UnicodeString aBody = System::UnicodeString(), System::Classes::TStrings* const aHeaders = (System::Classes::TStrings*)(0x0));
	virtual System::UnicodeString __fastcall DoHTTP_PUT(const System::UnicodeString aMethod, const System::UnicodeString aBody = System::UnicodeString(), System::Classes::TStrings* const aHeaders = (System::Classes::TStrings*)(0x0));
	virtual System::UnicodeString __fastcall DoHTTP_DELETE(const System::UnicodeString aMethod, const System::UnicodeString aParameters = System::UnicodeString(), System::Classes::TStrings* const aHeaders = (System::Classes::TStrings*)(0x0), const System::UnicodeString aBody = System::UnicodeString());
	virtual void __fastcall GetAuthHeaders(const System::UnicodeString aMethod, const System::UnicodeString aRequestPath, const System::UnicodeString aBody, System::Classes::TStringList* &Headers);
	virtual System::UnicodeString __fastcall DoHTTP_GET_PRIVATE(const System::UnicodeString aRequestPath);
	virtual System::UnicodeString __fastcall DoHTTP_POST_PRIVATE(const System::UnicodeString aRequestPath, const System::UnicodeString aBody = System::UnicodeString());
	virtual System::UnicodeString __fastcall DoHTTP_DELETE_PRIVATE(const System::UnicodeString aRequestPath, System::UnicodeString aBody = System::UnicodeString());
	
public:
	__fastcall virtual TsgcHTTP_API_Bitmex(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcHTTP_API_Bitmex();
	
private:
	TsgcHTTPBitmex_Options* FBitmexOptions;
	void __fastcall SetBitmexOptions(TsgcHTTPBitmex_Options* const Value);
	
public:
	__property TsgcHTTPBitmex_Options* BitmexOptions = {read=FBitmexOptions, write=SetBitmexOptions};
};


class PASCALIMPLEMENTATION TsgcHTTP_API_Bitmex_Rest : public TsgcHTTP_API_Bitmex
{
	typedef TsgcHTTP_API_Bitmex inherited;
	
private:
	System::UnicodeString __fastcall DoGetData(const System::UnicodeString aRequestPath, const System::UnicodeString aSymbol = System::UnicodeString(), const System::UnicodeString aFilter = System::UnicodeString(), const System::UnicodeString aColumns = System::UnicodeString(), int aCount = 0x64, __int64 aStart = 0LL, bool aReverse = false, __int64 aStartTime = 0LL, __int64 aEndTime = 0LL);
	
public:
	System::UnicodeString __fastcall GetExecutions(const System::UnicodeString aSymbol = System::UnicodeString(), const System::UnicodeString aFilter = System::UnicodeString(), const System::UnicodeString aColumns = System::UnicodeString(), int aCount = 0x64, __int64 aStart = 0LL, bool aReverse = false, __int64 aStartTime = 0LL, __int64 aEndTime = 0LL);
	System::UnicodeString __fastcall GetExecutionsTradeHistory(const System::UnicodeString aSymbol = System::UnicodeString(), const System::UnicodeString aFilter = System::UnicodeString(), const System::UnicodeString aColumns = System::UnicodeString(), int aCount = 0x64, __int64 aStart = 0LL, bool aReverse = false, __int64 aStartTime = 0LL, __int64 aEndTime = 0LL);
	System::UnicodeString __fastcall GetInstruments(const System::UnicodeString aSymbol = System::UnicodeString(), const System::UnicodeString aFilter = System::UnicodeString(), const System::UnicodeString aColumns = System::UnicodeString(), int aCount = 0x64, __int64 aStart = 0LL, bool aReverse = false, __int64 aStartTime = 0LL, __int64 aEndTime = 0LL);
	System::UnicodeString __fastcall GetOrders(const System::UnicodeString aSymbol, const System::UnicodeString aFilter = System::UnicodeString(), const System::UnicodeString aColumns = System::UnicodeString(), int aCount = 0x64, __int64 aStart = 0LL, bool aReverse = false, __int64 aStartTime = 0LL, __int64 aEndTime = 0LL);
	System::UnicodeString __fastcall PlaceOrder(TsgcHTTPBitmexOrder* const aOrder);
	System::UnicodeString __fastcall PlaceMarketOrder(TsgcHTTPBitmexOrderSide aSide, const System::UnicodeString aSymbol, System::Extended aQuantity);
	System::UnicodeString __fastcall PlaceLimitOrder(TsgcHTTPBitmexOrderSide aSide, const System::UnicodeString aSymbol, System::Extended aQuantity, System::Extended aPrice);
	System::UnicodeString __fastcall PlaceStopOrder(TsgcHTTPBitmexOrderSide aSide, const System::UnicodeString aSymbol, System::Extended aQuantity, System::Extended aStopPrice);
	System::UnicodeString __fastcall PlaceStopLimitOrder(TsgcHTTPBitmexOrderSide aSide, const System::UnicodeString aSymbol, System::Extended aQuantity, System::Extended aStopPrice, System::Extended aLimitPrice);
	System::UnicodeString __fastcall AmendOrder(TsgcHTTPBitmexAmendOrder* const aOrder);
	System::UnicodeString __fastcall CancelOrder(const System::UnicodeString aOrderId, const System::UnicodeString aClOrderId, const System::UnicodeString aText = System::UnicodeString());
	System::UnicodeString __fastcall CancelAllOrders(const System::UnicodeString aSymbol = System::UnicodeString(), const System::UnicodeString aFilter = System::UnicodeString(), const System::UnicodeString aText = System::UnicodeString());
	System::UnicodeString __fastcall CancelAllOrdersAfter(int aTimeout);
	System::UnicodeString __fastcall ClosePosition(const System::UnicodeString aSymbol, System::Extended aPrice = 0.000000E+00);
	System::UnicodeString __fastcall GetOrderBook(const System::UnicodeString aSymbol, int aDepth = 0x19);
	System::UnicodeString __fastcall GetPosition(const System::UnicodeString aColumns = System::UnicodeString(), int aCount = 0x64);
	System::UnicodeString __fastcall SetPositionIsolate(const System::UnicodeString aSymbol, bool aEnabled);
	System::UnicodeString __fastcall SetPositionLeverage(const System::UnicodeString aSymbol, double aLeverage);
	System::UnicodeString __fastcall SetPositionRiskLimit(const System::UnicodeString aSymbol, double aRiskLimit);
	System::UnicodeString __fastcall SetPositionTransferMargin(const System::UnicodeString aSymbol, System::Extended aAmount);
	System::UnicodeString __fastcall GetQuotes(const System::UnicodeString aSymbol, const System::UnicodeString aFilter = System::UnicodeString(), const System::UnicodeString aColumns = System::UnicodeString(), int aCount = 0x64, __int64 aStart = 0LL, bool aReverse = false, __int64 aStartTime = 0LL, __int64 aEndTime = 0LL);
	System::UnicodeString __fastcall GetTrades(const System::UnicodeString aSymbol, const System::UnicodeString aFilter = System::UnicodeString(), const System::UnicodeString aColumns = System::UnicodeString(), int aCount = 0x64, __int64 aStart = 0LL, bool aReverse = false, __int64 aStartTime = 0LL, __int64 aEndTime = 0LL);
public:
	/* TsgcHTTP_API_Bitmex.Create */ inline __fastcall virtual TsgcHTTP_API_Bitmex_Rest(System::Classes::TComponent* aOwner) : TsgcHTTP_API_Bitmex(aOwner) { }
	/* TsgcHTTP_API_Bitmex.Destroy */ inline __fastcall virtual ~TsgcHTTP_API_Bitmex_Rest() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgchttp_api_bitmex */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCHTTP_API_BITMEX)
using namespace Sgchttp_api_bitmex;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgchttp_api_bitmexHPP
