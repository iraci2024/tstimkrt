// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcHTTP_API_Cryptohopper.pas' rev: 35.00 (Windows)

#ifndef Sgchttp_api_cryptohopperHPP
#define Sgchttp_api_cryptohopperHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <IdGlobal.hpp>
#include <IdURI.hpp>
#include <IdContext.hpp>
#include <IdCustomHTTPServer.hpp>
#include <sgcHTTP_API.hpp>
#include <sgcHTTP_OAuth2_Client.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcWebSocket_Server.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgchttp_api_cryptohopper
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcHTTPCTHOrder;
class DELPHICLASS TsgcHTTPCTHSignal;
class DELPHICLASS TsgcHTTPCTHopper;
class DELPHICLASS TsgcHTTPCryptohopperLog_Options;
class DELPHICLASS TsgcHTTPCryptohopperOAuth2_Options;
class DELPHICLASS TsgcHTTPCryptohopperWebhook_Options;
class DELPHICLASS TsgcHTTPCryptohopper_Options;
class DELPHICLASS TsgcHTTP_API_Cryptohopper;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcCryptohopperWebhookEvent)(System::TObject* Sender, const System::UnicodeString aBody, const System::UnicodeString aSignature);

enum DECLSPEC_DENUM TsgcHTTPCTHOrderSide : unsigned char { cthosNone, cthosBuy, cthosSell };

class PASCALIMPLEMENTATION TsgcHTTPCTHOrder : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::Extended FAmount;
	System::UnicodeString FCoin;
	int FMarketOrder;
	TsgcHTTPCTHOrderSide FOrderSide;
	System::Extended FPctProfit;
	System::Extended FPrice;
	int FTrailingBuy;
	System::Extended FTrailingBuyPct;
	
protected:
	virtual System::UnicodeString __fastcall GetJSON();
	
public:
	__fastcall virtual TsgcHTTPCTHOrder();
	__property System::Extended Amount = {read=FAmount, write=FAmount};
	__property System::UnicodeString Coin = {read=FCoin, write=FCoin};
	__property int MarketOrder = {read=FMarketOrder, write=FMarketOrder, nodefault};
	__property TsgcHTTPCTHOrderSide OrderSide = {read=FOrderSide, write=FOrderSide, nodefault};
	__property System::Extended PctProfit = {read=FPctProfit, write=FPctProfit};
	__property System::Extended Price = {read=FPrice, write=FPrice};
	__property int TrailingBuy = {read=FTrailingBuy, write=FTrailingBuy, nodefault};
	__property System::Extended TrailingBuyPct = {read=FTrailingBuyPct, write=FTrailingBuyPct};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcHTTPCTHOrder() { }
	
};


class PASCALIMPLEMENTATION TsgcHTTPCTHSignal : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FAPI_Key;
	System::UnicodeString FAPI_Secret;
	System::UnicodeString FExchange;
	System::UnicodeString FMarket;
	System::Extended FPercentageProfit;
	System::UnicodeString FSignalId;
	System::Extended FStopLoss;
	System::Extended FTrailingArmPercentage;
	System::Extended FTrailingPercentage;
	TsgcHTTPCTHOrderSide F_Type;
	
public:
	System::UnicodeString __fastcall GetQueryString();
	__property System::UnicodeString API_Key = {read=FAPI_Key, write=FAPI_Key};
	__property System::UnicodeString API_Secret = {read=FAPI_Secret, write=FAPI_Secret};
	__property System::UnicodeString SignalId = {read=FSignalId, write=FSignalId};
	__property System::UnicodeString Exchange = {read=FExchange, write=FExchange};
	__property System::UnicodeString Market = {read=FMarket, write=FMarket};
	__property TsgcHTTPCTHOrderSide _Type = {read=F_Type, write=F_Type, nodefault};
	__property System::Extended PercentageProfit = {read=FPercentageProfit, write=FPercentageProfit};
	__property System::Extended StopLoss = {read=FStopLoss, write=FStopLoss};
	__property System::Extended TrailingPercentage = {read=FTrailingPercentage, write=FTrailingPercentage};
	__property System::Extended TrailingArmPercentage = {read=FTrailingArmPercentage, write=FTrailingArmPercentage};
public:
	/* TObject.Create */ inline __fastcall TsgcHTTPCTHSignal() : System::TObject() { }
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcHTTPCTHSignal() { }
	
};


#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTPCTHopper : public System::TObject
{
	typedef System::TObject inherited;
	
private:
	System::UnicodeString FAPI_Key;
	System::UnicodeString FAPI_Secret;
	System::UnicodeString FBase_Currency;
	int FBuying_Enabled;
	int FEnabled;
	System::UnicodeString FExchange;
	System::UnicodeString FHopperName;
	int FPaper_Trading;
	int FSelling_Enabled;
	System::UnicodeString FStrategy;
	
protected:
	virtual System::UnicodeString __fastcall GetJSON();
	
public:
	__fastcall virtual TsgcHTTPCTHopper();
	__property System::UnicodeString API_Key = {read=FAPI_Key, write=FAPI_Key};
	__property System::UnicodeString API_Secret = {read=FAPI_Secret, write=FAPI_Secret};
	__property System::UnicodeString Base_Currency = {read=FBase_Currency, write=FBase_Currency};
	__property int Buying_Enabled = {read=FBuying_Enabled, write=FBuying_Enabled, nodefault};
	__property int Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property System::UnicodeString Exchange = {read=FExchange, write=FExchange};
	__property System::UnicodeString HopperName = {read=FHopperName, write=FHopperName};
	__property int Paper_Trading = {read=FPaper_Trading, write=FPaper_Trading, nodefault};
	__property int Selling_Enabled = {read=FSelling_Enabled, write=FSelling_Enabled, nodefault};
	__property System::UnicodeString Strategy = {read=FStrategy, write=FStrategy};
public:
	/* TObject.Destroy */ inline __fastcall virtual ~TsgcHTTPCTHopper() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTPCryptohopperLog_Options : public System::Classes::TPersistent
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
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcHTTPCryptohopperLog_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcHTTPCryptohopperLog_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTPCryptohopperOAuth2_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FClientId;
	System::UnicodeString FClientSecret;
	System::UnicodeString FLocalIP;
	int FLocalPort;
	System::UnicodeString FRedirectURL;
	System::Classes::TStringList* FScope;
	void __fastcall SetScope(System::Classes::TStringList* const Value);
	
public:
	__fastcall virtual TsgcHTTPCryptohopperOAuth2_Options();
	__fastcall virtual ~TsgcHTTPCryptohopperOAuth2_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString ClientId = {read=FClientId, write=FClientId};
	__property System::UnicodeString ClientSecret = {read=FClientSecret, write=FClientSecret};
	__property System::UnicodeString LocalIP = {read=FLocalIP, write=FLocalIP};
	__property int LocalPort = {read=FLocalPort, write=FLocalPort, nodefault};
	__property System::UnicodeString RedirectURL = {read=FRedirectURL, write=FRedirectURL};
	__property System::Classes::TStringList* Scope = {read=FScope, write=SetScope};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTPCryptohopperWebhook_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	bool FEnabled;
	System::UnicodeString FHost;
	int FPort;
	Sgcwebsocket_classes::TsgcWSSSL_Options* FSSLOptions;
	System::UnicodeString FValidationCode;
	void __fastcall SetSSLOptions(Sgcwebsocket_classes::TsgcWSSSL_Options* const Value);
	
public:
	__fastcall virtual TsgcHTTPCryptohopperWebhook_Options();
	__fastcall virtual ~TsgcHTTPCryptohopperWebhook_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property bool Enabled = {read=FEnabled, write=FEnabled, nodefault};
	__property System::UnicodeString Host = {read=FHost, write=FHost};
	__property int Port = {read=FPort, write=FPort, nodefault};
	__property Sgcwebsocket_classes::TsgcWSSSL_Options* SSLOptions = {read=FSSLOptions, write=SetSSLOptions};
	__property System::UnicodeString ValidationCode = {read=FValidationCode, write=FValidationCode};
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcHTTPCryptohopper_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	TsgcHTTPCryptohopperLog_Options* FLogOptions;
	TsgcHTTPCryptohopperOAuth2_Options* FOAuth2;
	TsgcHTTPCryptohopperWebhook_Options* FWebhook;
	void __fastcall SetLogOptions(TsgcHTTPCryptohopperLog_Options* const Value);
	void __fastcall SetOAuth2(TsgcHTTPCryptohopperOAuth2_Options* const Value);
	void __fastcall SetWebhook(TsgcHTTPCryptohopperWebhook_Options* const Value);
	
public:
	__fastcall virtual TsgcHTTPCryptohopper_Options();
	__fastcall virtual ~TsgcHTTPCryptohopper_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property TsgcHTTPCryptohopperLog_Options* LogOptions = {read=FLogOptions, write=SetLogOptions};
	__property TsgcHTTPCryptohopperOAuth2_Options* OAuth2 = {read=FOAuth2, write=SetOAuth2};
	__property TsgcHTTPCryptohopperWebhook_Options* Webhook = {read=FWebhook, write=SetWebhook};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcHTTP_API_Cryptohopper : public Sgchttp_api::TsgcHTTPAPI_client
{
	typedef Sgchttp_api::TsgcHTTPAPI_client inherited;
	
private:
	Sgchttp_oauth2_client::TsgcHTTPComponentClient_OAuth2* FOAuth2;
	System::UnicodeString FOAuth2Token;
	System::TDateTime FOAuth2Expires;
	Sgchttp_oauth2_client::TsgcHTTPComponentClient_OAuth2* __fastcall GetOAuth2();
	
protected:
	virtual void __fastcall OnAfterAccessTokenEvent(System::TObject* Sender, const System::UnicodeString Access_Token, const System::UnicodeString Token_Type, const System::UnicodeString Expires_In, const System::UnicodeString Refresh_Token, const System::UnicodeString Scope, const System::UnicodeString RawParams, bool &Handled);
	virtual void __fastcall OnErrorAccessTokenEvent(System::TObject* Sender, const System::UnicodeString Error, const System::UnicodeString Error_Description, const System::UnicodeString Error_URI, const System::UnicodeString RawParams);
	__property Sgchttp_oauth2_client::TsgcHTTPComponentClient_OAuth2* OAuth2 = {read=GetOAuth2, write=FOAuth2};
	
private:
	Sgcwebsocket_server::TsgcWSHTTPServer* FHTTPServer;
	
protected:
	Sgcwebsocket_server::TsgcWSHTTPServer* __fastcall GetHTTPServer();
	virtual void __fastcall OnCommandGetEvent(Idcontext::TIdContext* AContext, Idcustomhttpserver::TIdHTTPRequestInfo* ARequestInfo, Idcustomhttpserver::TIdHTTPResponseInfo* AResponseInfo);
	virtual void __fastcall DoWebhookEvent(const System::UnicodeString aBody, const System::UnicodeString aSignature);
	__property Sgcwebsocket_server::TsgcWSHTTPServer* HTTPServer = {read=GetHTTPServer, write=FHTTPServer};
	
public:
	void __fastcall StartWebhook();
	void __fastcall StopWebhook();
	
private:
	void __fastcall DoInitialize();
	System::UnicodeString __fastcall GetBaseURL();
	
protected:
	virtual System::UnicodeString __fastcall DoHTTP_DELETE(const System::UnicodeString aMethod, const System::UnicodeString aParameters = System::UnicodeString(), System::Classes::TStrings* const aHeaders = (System::Classes::TStrings*)(0x0), const System::UnicodeString aBody = System::UnicodeString());
	virtual System::UnicodeString __fastcall DoHTTP_GET(const System::UnicodeString aMethod, const System::UnicodeString aParameters = System::UnicodeString(), System::Classes::TStrings* const aHeaders = (System::Classes::TStrings*)(0x0));
	virtual System::UnicodeString __fastcall DoHTTP_POST(const System::UnicodeString aMethod, const System::UnicodeString aBody = System::UnicodeString(), System::Classes::TStrings* const aHeaders = (System::Classes::TStrings*)(0x0));
	virtual System::UnicodeString __fastcall DoHTTP_PATCH(const System::UnicodeString aMethod, const System::UnicodeString aBody = System::UnicodeString(), System::Classes::TStrings* const aHeaders = (System::Classes::TStrings*)(0x0));
	virtual void __fastcall GetAuthHeaders(const System::UnicodeString aMethod, const System::UnicodeString aRequestPath, const System::UnicodeString aBody, System::Classes::TStringList* &Headers);
	virtual System::UnicodeString __fastcall DoHTTP_GET_PRIVATE(const System::UnicodeString aRequestPath);
	virtual System::UnicodeString __fastcall DoHTTP_POST_PRIVATE(const System::UnicodeString aRequestPath, const System::UnicodeString aBody);
	virtual System::UnicodeString __fastcall DoHTTP_DELETE_PRIVATE(const System::UnicodeString aRequestPath, System::UnicodeString aBody = System::UnicodeString());
	virtual System::UnicodeString __fastcall DoHTTP_PATCH_PRIVATE(const System::UnicodeString aRequestPath, const System::UnicodeString aBody);
	
public:
	__fastcall virtual TsgcHTTP_API_Cryptohopper(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcHTTP_API_Cryptohopper();
	
private:
	TsgcHTTPCryptohopper_Options* FCryptohopperOptions;
	void __fastcall SetCryptohopperOptions(TsgcHTTPCryptohopper_Options* const Value);
	
public:
	__property TsgcHTTPCryptohopper_Options* CryptohopperOptions = {read=FCryptohopperOptions, write=SetCryptohopperOptions};
	System::UnicodeString __fastcall GetHoppers();
	System::UnicodeString __fastcall CreateHopper(const System::UnicodeString aBody);
	System::UnicodeString __fastcall GetHopper(const System::UnicodeString aId)/* overload */;
	bool __fastcall GetHopper(const System::UnicodeString aId, TsgcHTTPCTHopper* &aHopper)/* overload */;
	System::UnicodeString __fastcall DeleteHopper(const System::UnicodeString aId);
	System::UnicodeString __fastcall UpdateHopper(const System::UnicodeString aId, const System::UnicodeString aBody)/* overload */;
	System::UnicodeString __fastcall UpdateHopper(const System::UnicodeString aId, TsgcHTTPCTHopper* const aHopper)/* overload */;
	System::UnicodeString __fastcall GetOpenOrders(const System::UnicodeString aId);
	System::UnicodeString __fastcall CreateNewOrder(const System::UnicodeString aId, TsgcHTTPCTHOrder* const aOrder);
	System::UnicodeString __fastcall PlaceMarketOrder(const System::UnicodeString aId, TsgcHTTPCTHOrderSide aOrderSide, const System::UnicodeString aCoin, System::Extended aAmount);
	System::UnicodeString __fastcall PlaceLimitOrder(const System::UnicodeString aId, TsgcHTTPCTHOrderSide aOrderSide, const System::UnicodeString aCoin, System::Extended aAmount, System::Extended aPrice);
	System::UnicodeString __fastcall DeleteOrder(const System::UnicodeString aId, const System::UnicodeString aOrderId);
	System::UnicodeString __fastcall DeleteAllOrders(const System::UnicodeString aId);
	System::UnicodeString __fastcall GetOpenOrder(const System::UnicodeString aId, const System::UnicodeString aOrderId);
	System::UnicodeString __fastcall CancelOrder(const System::UnicodeString aId, const System::UnicodeString aOrderId);
	System::UnicodeString __fastcall GetPosition(const System::UnicodeString aId);
	System::UnicodeString __fastcall GetTradeHistory(const System::UnicodeString aId);
	System::UnicodeString __fastcall GetTradeHistoryById(const System::UnicodeString aId, const System::UnicodeString aTradeId);
	System::UnicodeString __fastcall GetExchange();
	System::UnicodeString __fastcall GetAllTickers(const System::UnicodeString aExchange);
	System::UnicodeString __fastcall GetMarketTicker(const System::UnicodeString aExchange, const System::UnicodeString aPair);
	System::UnicodeString __fastcall GetOrderBook(const System::UnicodeString aExchange, const System::UnicodeString aPair, const System::UnicodeString aDepth);
	System::UnicodeString __fastcall CreateWebhook(const System::UnicodeString aURL, System::UnicodeString aMessageTypes = System::UnicodeString());
	System::UnicodeString __fastcall DeleteWebhook(const System::UnicodeString aURL);
	
protected:
	System::UnicodeString __fastcall DoSendSignal(TsgcHTTPCTHSignal* const aSignal, bool aTest);
	
public:
	System::UnicodeString __fastcall SendSignal(TsgcHTTPCTHSignal* const aSignal);
	System::UnicodeString __fastcall SendTestSignal(TsgcHTTPCTHSignal* const aSignal);
	System::UnicodeString __fastcall GetSignalStats(const System::UnicodeString aSignalId, const System::UnicodeString aExchange = System::UnicodeString());
	
private:
	TsgcCryptohopperWebhookEvent FOnWebhook;
	
public:
	__property TsgcCryptohopperWebhookEvent OnWebhook = {read=FOnWebhook, write=FOnWebhook};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgchttp_api_cryptohopper */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCHTTP_API_CRYPTOHOPPER)
using namespace Sgchttp_api_cryptohopper;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgchttp_api_cryptohopperHPP
