// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_API_FTX.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_api_ftxHPP
#define Sgcwebsocket_api_ftxHPP

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
#include <sgcHTTP_API_FTX.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_api_ftx
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSFTXLog_Options;
class DELPHICLASS TsgcWSFTX_Options;
class DELPHICLASS TsgcWS_API_FTX;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSFTXSubscribedEvent)(System::TObject* Sender, System::UnicodeString aChannel, System::UnicodeString aMarket, System::UnicodeString aRawMessage);

typedef void __fastcall (__closure *TsgcWSFTXUnsubscribedEvent)(System::TObject* Sender, System::UnicodeString aChannel, System::UnicodeString aMarket, System::UnicodeString aRawMessage);

typedef void __fastcall (__closure *TsgcWSFTXMessageEvent)(System::TObject* Sender, System::UnicodeString aType, System::UnicodeString aRawMessage);

typedef void __fastcall (__closure *TsgcWSFTXErrorEvent)(System::TObject* Sender, System::UnicodeString aCode, System::UnicodeString aMessage, System::UnicodeString aRawMessage);

typedef void __fastcall (__closure *TsgcWSFTXHTTPExceptionEvent)(System::TObject* Sender, System::Sysutils::Exception* E);

enum DECLSPEC_DENUM TsgcWSFTXRequest : unsigned char { ftrSubscribe, ftrUnsubscribe };

enum DECLSPEC_DENUM TsgcWSFTXChannels : unsigned char { ftcTicker, ftcMarkets, ftcTrades, ftcOrderbook, ftcOrderbookGrouped, ftcFills, ftcOrders };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSFTXLog_Options : public System::Classes::TPersistent
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
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSFTXLog_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSFTXLog_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSFTX_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FApiKey;
	System::UnicodeString FApiSecret;
	TsgcWSFTXLog_Options* FHTTPLogOptions;
	System::UnicodeString FSubAccount;
	void __fastcall SetHTTPLogOptions(TsgcWSFTXLog_Options* const Value);
	
public:
	__fastcall virtual TsgcWSFTX_Options();
	__fastcall virtual ~TsgcWSFTX_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString ApiKey = {read=FApiKey, write=FApiKey};
	__property System::UnicodeString ApiSecret = {read=FApiSecret, write=FApiSecret};
	__property TsgcWSFTXLog_Options* HTTPLogOptions = {read=FHTTPLogOptions, write=SetHTTPLogOptions};
	__property System::UnicodeString SubAccount = {read=FSubAccount, write=FSubAccount};
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWS_API_FTX : public Sgcwebsocket_classes::TsgcWSAPI_client
{
	typedef Sgcwebsocket_classes::TsgcWSAPI_client inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetURL();
	virtual void __fastcall DoBeforeConnect();
	virtual void __fastcall DoEventConnect(Sgcwebsocket_classes::TsgcWSConnection* aConnection);
	virtual void __fastcall DoEventMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual bool __fastcall DoKeepAlive();
	
private:
	Sgcjson::TsgcJSON* FJSON;
	Sgcjson::TsgcJSON* __fastcall GetJSON();
	
protected:
	System::UnicodeString __fastcall GetJSONChannel();
	System::UnicodeString __fastcall GetJSONMarket();
	System::UnicodeString __fastcall GetJSONCode();
	System::UnicodeString __fastcall GetJSONMessage();
	virtual void __fastcall DoReadJSONType(System::UnicodeString aRawMessage);
	__property Sgcjson::TsgcJSON* JSON = {read=GetJSON, write=FJSON};
	
private:
	TsgcWSFTX_Options* FFTX;
	Sgchttp_api_ftx::TsgcHTTP_API_FTX_Rest* FREST_API;
	void __fastcall SetFTX(TsgcWSFTX_Options* const Value);
	Sgchttp_api_ftx::TsgcHTTP_API_FTX_Rest* __fastcall GetREST_API();
	
protected:
	virtual void __fastcall OnHTTPAPIExceptionEvent(System::TObject* Sender, System::Sysutils::Exception* E);
	
public:
	__property TsgcWSFTX_Options* FTX = {read=FFTX, write=SetFTX};
	__property Sgchttp_api_ftx::TsgcHTTP_API_FTX_Rest* REST_API = {read=GetREST_API, write=FREST_API};
	__fastcall virtual TsgcWS_API_FTX(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWS_API_FTX();
	
private:
	System::UnicodeString __fastcall GetChannel(TsgcWSFTXChannels aChannel);
	
protected:
	virtual void __fastcall DoWriteData(const System::UnicodeString aText);
	virtual void __fastcall DoRequest(TsgcWSFTXRequest aRequest, TsgcWSFTXChannels aChannel, const System::UnicodeString aMarket = System::UnicodeString(), int aGrouping = 0x0, bool aAuthenticate = false);
	virtual void __fastcall DoLogin();
	virtual void __fastcall DoPing();
	
public:
	void __fastcall Ping();
	void __fastcall SubscribeTicker(const System::UnicodeString aMarket);
	void __fastcall UnSubscribeTicker(const System::UnicodeString aMarket);
	void __fastcall SubscribeMarkets();
	void __fastcall UnSubscribeMarkets();
	void __fastcall SubscribeTrades(const System::UnicodeString aMarket);
	void __fastcall UnSubscribeTrades(const System::UnicodeString aMarket);
	void __fastcall SubscribeOrderbooks(const System::UnicodeString aMarket);
	void __fastcall UnSubscribeOrderbooks(const System::UnicodeString aMarket);
	void __fastcall SubscribeGroupedOrderbooks(const System::UnicodeString aMarket, int aGrouping = 0x1f4);
	void __fastcall UnSubscribeGroupedOrderbooks(const System::UnicodeString aMarket, int aGrouping = 0x1f4);
	void __fastcall SubscribeFills();
	void __fastcall UnSubscribeFills();
	void __fastcall SubscribeOrders();
	void __fastcall UnSubscribeOrders();
	
private:
	TsgcWSFTXErrorEvent FOnFTXError;
	TsgcWSFTXMessageEvent FOnFTXMessage;
	TsgcWSFTXSubscribedEvent FOnFTXSubscribed;
	TsgcWSFTXUnsubscribedEvent FOnFTXUnsubscribed;
	
protected:
	virtual void __fastcall DoSubscribedEvent(const System::UnicodeString aRawMessage);
	virtual void __fastcall DoUnSubscribedEvent(const System::UnicodeString aRawMessage);
	virtual void __fastcall DoMessageEvent(const System::UnicodeString aType, const System::UnicodeString aRawMessage);
	virtual void __fastcall DoErrorEvent(const System::UnicodeString aRawMessage);
	
public:
	__property TsgcWSFTXErrorEvent OnFTXError = {read=FOnFTXError, write=FOnFTXError};
	__property TsgcWSFTXMessageEvent OnFTXMessage = {read=FOnFTXMessage, write=FOnFTXMessage};
	__property TsgcWSFTXSubscribedEvent OnFTXSubscribed = {read=FOnFTXSubscribed, write=FOnFTXSubscribed};
	__property TsgcWSFTXUnsubscribedEvent OnFTXUnsubscribed = {read=FOnFTXUnsubscribed, write=FOnFTXUnsubscribed};
	
private:
	TsgcWSFTXHTTPExceptionEvent FOnFTXHTTPException;
	
protected:
	virtual void __fastcall DoFTXHTTPExceptionEvent(System::Sysutils::Exception* E);
	
public:
	__property TsgcWSFTXHTTPExceptionEvent OnFTXHTTPException = {read=FOnFTXHTTPException, write=FOnFTXHTTPException};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_api_ftx */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_API_FTX)
using namespace Sgcwebsocket_api_ftx;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_api_ftxHPP
