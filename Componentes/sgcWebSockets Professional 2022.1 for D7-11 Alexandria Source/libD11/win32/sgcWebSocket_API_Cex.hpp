// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_API_Cex.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_api_cexHPP
#define Sgcwebsocket_api_cexHPP

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

namespace Sgcwebsocket_api_cex
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSCex_Options;
class DELPHICLASS TsgcWS_API_Cex;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSCexConnectEvent)(System::TObject* Sender);

typedef void __fastcall (__closure *TsgcWSCexErrorEvent)(System::TObject* Sender, System::UnicodeString Error);

typedef void __fastcall (__closure *TsgcWSCexSubscribedEvent)(System::TObject* Sender, System::UnicodeString Channel);

typedef void __fastcall (__closure *TsgcWSCexMessageEvent)(System::TObject* Sender, System::UnicodeString Event, System::UnicodeString Msg);

typedef void __fastcall (__closure *TsgcWSCexAuthenticatedEvent)(System::TObject* Sender);

typedef void __fastcall (__closure *TsgcWSCexDisconnectingEvent)(System::TObject* Sender, System::UnicodeString Reason, System::UnicodeString Time);

typedef void __fastcall (__closure *TsgcWSCexDisconnectEvent)(System::TObject* Sender);

enum DECLSPEC_DENUM TsgcWSCexPeriods : unsigned char { cxp1Min, cxp3Min, cxp5Min, cxp15Min, cxp30Min, cxp1Hour, cxp2Hour, cxp4Hour, cxp6Hour, cxp12Hour, cxp1Day, cxp3Day, cxp1Week };

enum DECLSPEC_DENUM TsgcWSCexOrderType : unsigned char { cxoBuy, cxoSell };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSCex_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FApiKey;
	System::UnicodeString FApiSecret;
	
public:
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString ApiKey = {read=FApiKey, write=FApiKey};
	__property System::UnicodeString ApiSecret = {read=FApiSecret, write=FApiSecret};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSCex_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSCex_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWS_API_Cex : public Sgcwebsocket_classes::TsgcWSAPI_client
{
	typedef Sgcwebsocket_classes::TsgcWSAPI_client inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetURL();
	virtual void __fastcall DoEventMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual bool __fastcall DoKeepAlive();
	
private:
	System::UnicodeString __fastcall GetPeriod(const TsgcWSCexPeriods aPeriod);
	System::UnicodeString __fastcall GetOrderType(const TsgcWSCexOrderType aOrderType);
	Sgcjson::TsgcJSON* FJSON;
	Sgcjson::TsgcJSON* __fastcall GetJSON();
	
protected:
	__property Sgcjson::TsgcJSON* JSON = {read=GetJSON, write=FJSON};
	
private:
	TsgcWSCex_Options* FCex;
	void __fastcall SetCex(TsgcWSCex_Options* const Value);
	
public:
	__property TsgcWSCex_Options* Cex = {read=FCex, write=SetCex};
	__fastcall virtual TsgcWS_API_Cex(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWS_API_Cex();
	
private:
	int FRequestNum;
	System::UnicodeString __fastcall GetTimeStamp();
	
protected:
	virtual void __fastcall DoReadEvent(const System::UnicodeString aEvent, const System::UnicodeString aText);
	virtual void __fastcall DoWriteData(const System::UnicodeString aText);
	virtual void __fastcall DoPing();
	void __fastcall DoAuthenticate();
	virtual void __fastcall DoSubscribe(const System::UnicodeString aRoom);
	virtual void __fastcall DoUnSubscribe(const System::UnicodeString aRoom);
	virtual void __fastcall DoRequest(const System::UnicodeString aEvent, const System::UnicodeString aSymbolPair1 = System::UnicodeString(), const System::UnicodeString aSymbolPair2 = System::UnicodeString(), const System::UnicodeString aOptions = System::UnicodeString());
	
public:
	void __fastcall Ping();
	void __fastcall Authenticate();
	void __fastcall SubscribeTickers();
	void __fastcall UnSubscribeTickers();
	void __fastcall SubscribePair(const System::UnicodeString aSymbolPair1, const System::UnicodeString aSymbolPair2);
	void __fastcall UnSubscribePair(const System::UnicodeString aSymbolPair1, const System::UnicodeString aSymbolPair2);
	void __fastcall SubscribeChart(TsgcWSCexPeriods aPeriod, const System::UnicodeString aSymbolPair1, const System::UnicodeString aSymbolPair2);
	void __fastcall GetTicker(const System::UnicodeString aSymbolPair1, const System::UnicodeString aSymbolPair2);
	void __fastcall GetBalance();
	void __fastcall SubscribeOrderBook(const System::UnicodeString aSymbolPair1, const System::UnicodeString aSymbolPair2, bool aSubscribe = true, const int aDepth = 0xffffffff);
	void __fastcall UnSubscribeOrderBook(const System::UnicodeString aSymbolPair1, const System::UnicodeString aSymbolPair2);
	void __fastcall GetOpenOrders(const System::UnicodeString aSymbolPair1, const System::UnicodeString aSymbolPair2);
	void __fastcall PlaceOrder(const System::UnicodeString aSymbolPair1, const System::UnicodeString aSymbolPair2, System::Currency aAmount, System::Currency aPrice, TsgcWSCexOrderType aType);
	void __fastcall CancelReplaceOrder(const System::UnicodeString aOrderId, const System::UnicodeString aSymbolPair1, const System::UnicodeString aSymbolPair2, System::Currency aAmount, System::Currency aPrice, TsgcWSCexOrderType aType);
	void __fastcall GetOrderRequest(const System::UnicodeString aOrderId);
	void __fastcall CancelOrderRequest(const System::UnicodeString aOrderId);
	void __fastcall GetArchivedOrders(const System::UnicodeString aSymbolPair1, const System::UnicodeString aSymbolPair2, const int aLimit = 0xa);
	void __fastcall OpenPosition(const System::UnicodeString aSymbolPair1, const System::UnicodeString aSymbolPair2, const System::UnicodeString aSymbol, int aAmount, int aLeverage, bool aAnySlippage, System::Currency aEOPrice, System::Currency aStopLossPrice);
	void __fastcall GetPosition(const System::UnicodeString aId);
	void __fastcall GetOpenPositions(const System::UnicodeString aSymbolPair1, const System::UnicodeString aSymbolPair2);
	void __fastcall ClosePosition(const System::UnicodeString aId, const System::UnicodeString aSymbolPair1, const System::UnicodeString aSymbolPair2);
	
private:
	TsgcWSCexAuthenticatedEvent FOnCexAuthenticated;
	TsgcWSCexConnectEvent FOnCexConnect;
	TsgcWSCexDisconnectEvent FOnCexDisconnect;
	TsgcWSCexDisconnectingEvent FOnCexDisconnecting;
	TsgcWSCexErrorEvent FOnCexError;
	TsgcWSCexMessageEvent FOnCexMessage;
	TsgcWSCexSubscribedEvent FOnCexSubscribed;
	
protected:
	virtual void __fastcall DoEventCexConnect();
	virtual void __fastcall DoEventCexSubscribed(const System::UnicodeString aChannel);
	virtual void __fastcall DoEventCexMessage(const System::UnicodeString aEvent, const System::UnicodeString aMessage);
	virtual void __fastcall DoEventCexAuthenticated();
	virtual void __fastcall DoEventCexError(const System::UnicodeString aError);
	virtual void __fastcall DoEventCexDisconnecting(const System::UnicodeString aReason, const System::UnicodeString aTime);
	virtual void __fastcall DoEventCexDisconnect();
	
public:
	__property TsgcWSCexConnectEvent OnCexConnect = {read=FOnCexConnect, write=FOnCexConnect};
	__property TsgcWSCexSubscribedEvent OnCexSubscribed = {read=FOnCexSubscribed, write=FOnCexSubscribed};
	__property TsgcWSCexMessageEvent OnCexMessage = {read=FOnCexMessage, write=FOnCexMessage};
	__property TsgcWSCexAuthenticatedEvent OnCexAuthenticated = {read=FOnCexAuthenticated, write=FOnCexAuthenticated};
	__property TsgcWSCexErrorEvent OnCexError = {read=FOnCexError, write=FOnCexError};
	__property TsgcWSCexDisconnectEvent OnCexDisconnect = {read=FOnCexDisconnect, write=FOnCexDisconnect};
	__property TsgcWSCexDisconnectingEvent OnCexDisconnecting = {read=FOnCexDisconnecting, write=FOnCexDisconnecting};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_api_cex */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_API_CEX)
using namespace Sgcwebsocket_api_cex;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_api_cexHPP
