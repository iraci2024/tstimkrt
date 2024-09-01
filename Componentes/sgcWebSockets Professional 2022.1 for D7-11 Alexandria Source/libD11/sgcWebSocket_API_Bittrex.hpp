// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_API_Bittrex.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_api_bittrexHPP
#define Sgcwebsocket_api_bittrexHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <sgcWebSocket_API_SignalR.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_api_bittrex
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSBittrex_Options;
class DELPHICLASS TsgcWS_API_Bittrex;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSBittrexConnectEvent)(System::TObject* Sender);

typedef void __fastcall (__closure *TsgcWSBittrexErrorEvent)(System::TObject* Sender, System::UnicodeString Error);

typedef void __fastcall (__closure *TsgcWSBittrexSubscribedEvent)(System::TObject* Sender, System::UnicodeString Channel);

typedef void __fastcall (__closure *TsgcWSBittrexUnSubscribedEvent)(System::TObject* Sender, System::UnicodeString Channel);

typedef void __fastcall (__closure *TsgcWSBittrexMessageEvent)(System::TObject* Sender, System::UnicodeString CallBack, System::UnicodeString PayLoad);

typedef void __fastcall (__closure *TsgcWSBittrexDisconnectEvent)(System::TObject* Sender, int aCloseCode);

typedef void __fastcall (__closure *TsgcWSBittrexAuthenticatedEvent)(System::TObject* Sender);

typedef void __fastcall (__closure *TsgcWSBittrexHeartBeatEvent)(System::TObject* Sender);

enum DECLSPEC_DENUM TsgcWSBittrexCandleInterval : unsigned char { btxc1Min, btxc5Min, btxc5Hour, btxc1Day };

enum DECLSPEC_DENUM TsgcWSBittrexOrderBookDepth : unsigned char { btxd1, btxd25, btxd500 };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSBittrex_Options : public System::Classes::TPersistent
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
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSBittrex_Options() { }
	
public:
	/* TObject.Create */ inline __fastcall TsgcWSBittrex_Options() : System::Classes::TPersistent() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWS_API_Bittrex : public Sgcwebsocket_api_signalr::TsgcWS_API_SignalR
{
	typedef Sgcwebsocket_api_signalr::TsgcWS_API_SignalR inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetURL();
	virtual void __fastcall DoEventSignalRConnect(const System::UnicodeString aMessageId, const System::UnicodeString aData);
	virtual void __fastcall DoEventSignalRResult(const System::UnicodeString aInvocationId, const System::UnicodeString aResult, const System::UnicodeString aError);
	virtual void __fastcall DoEventSignalRMessage(const System::UnicodeString aMessageId, const System::UnicodeString aData);
	virtual void __fastcall DoEventSignalRError(const System::UnicodeString aError);
	virtual void __fastcall DoEventSignalRDisconnect(const int aCloseCode);
	
private:
	TsgcWSBittrex_Options* FBittrex;
	void __fastcall SetBittrex(TsgcWSBittrex_Options* const Value);
	
public:
	__property TsgcWSBittrex_Options* Bittrex = {read=FBittrex, write=SetBittrex};
	__fastcall virtual TsgcWS_API_Bittrex(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWS_API_Bittrex();
	
private:
	System::UnicodeString __fastcall DoDecodeGZIP(const System::UnicodeString aText);
	System::UnicodeString __fastcall GetHMACSHA512(const System::UnicodeString aValue, const System::UnicodeString aSecret);
	void __fastcall DoWriteData(const System::UnicodeString aAction, const System::UnicodeString aMethod);
	void __fastcall DoSubscribe(const System::UnicodeString aMethod);
	void __fastcall DoUnSubscribe(const System::UnicodeString aMethod);
	__int64 __fastcall GetTimeStamp();
	System::UnicodeString __fastcall GetAuthentication();
	bool FIsAuthenticated;
	bool FResponseIsAuthenticated;
	
public:
	bool __fastcall IsAuthenticated(int aTimeOut = 0x2710);
	void __fastcall Authenticate();
	void __fastcall SubscribeBalance();
	void __fastcall UnSubscribeBalance();
	void __fastcall SubscribeHeartBeat();
	void __fastcall UnSubscribeHeartBeat();
	
private:
	System::UnicodeString __fastcall GetCandleString(const System::UnicodeString aSymbol, TsgcWSBittrexCandleInterval aInterval);
	System::UnicodeString __fastcall GetCandleIntervalString(TsgcWSBittrexCandleInterval aInterval);
	
public:
	void __fastcall SubscribeCandle(const System::UnicodeString aSymbol, TsgcWSBittrexCandleInterval aInterval);
	void __fastcall UnSubscribeCandle(const System::UnicodeString aSymbol, TsgcWSBittrexCandleInterval aInterval);
	void __fastcall SubscribeDeposit();
	void __fastcall UnSubscribeDeposit();
	void __fastcall SubscribeMarketSummaries();
	void __fastcall UnSubscribeMarketSummaries();
	void __fastcall SubscribeMarketSummary(const System::UnicodeString aSymbol);
	void __fastcall UnSubscribeMarketSummary(const System::UnicodeString aSymbol);
	void __fastcall SubscribeOrder();
	void __fastcall UnSubscribeOrder();
	
private:
	System::UnicodeString __fastcall GetOrderBookString(const System::UnicodeString aSymbol, TsgcWSBittrexOrderBookDepth aDepth);
	System::UnicodeString __fastcall GetOrderBookDepthString(TsgcWSBittrexOrderBookDepth aDepth);
	
public:
	void __fastcall SubscribeOrderBook(const System::UnicodeString aSymbol, TsgcWSBittrexOrderBookDepth aDepth);
	void __fastcall UnSubscribeOrderBook(const System::UnicodeString aSymbol, TsgcWSBittrexOrderBookDepth aDepth);
	void __fastcall SubscribeTickers();
	void __fastcall UnSubscribeTickers();
	void __fastcall SubscribeTicker(const System::UnicodeString aSymbol);
	void __fastcall UnSubscribeTicker(const System::UnicodeString aSymbol);
	void __fastcall SubscribeTrade(const System::UnicodeString aSymbol);
	void __fastcall UnSubscribeTrade(const System::UnicodeString aSymbol);
	
private:
	TsgcWSBittrexAuthenticatedEvent FOnBittrexAuthenticated;
	TsgcWSBittrexConnectEvent FOnBittrexConnect;
	TsgcWSBittrexDisconnectEvent FOnBittrexDisconnect;
	TsgcWSBittrexErrorEvent FOnBittrexError;
	TsgcWSBittrexHeartBeatEvent FOnBittrexHeartBeat;
	TsgcWSBittrexMessageEvent FOnBittrexMessage;
	TsgcWSBittrexSubscribedEvent FOnBittrexSubscribed;
	TsgcWSBittrexUnSubscribedEvent FOnBittrexUnSubscribed;
	
protected:
	virtual void __fastcall DoEventBittrexConnect();
	virtual void __fastcall DoEventBittrexSubscribed(const System::UnicodeString aChannel);
	virtual void __fastcall DoEventBittrexUnSubscribed(const System::UnicodeString aChannel);
	virtual void __fastcall DoEventBittrexMessage(const System::UnicodeString aCallBack, const System::UnicodeString aPayLoad);
	virtual void __fastcall DoEventBittrexError(const System::UnicodeString aError);
	virtual void __fastcall DoEventBittrexDisconnect(const int aCloseCode);
	virtual void __fastcall DoEventBittrexAuthenticated();
	virtual void __fastcall DoEventBittrexHeartBeat();
	__property TsgcWSBittrexConnectEvent OnBittrexConnect = {read=FOnBittrexConnect, write=FOnBittrexConnect};
	__property TsgcWSBittrexSubscribedEvent OnBittrexSubscribed = {read=FOnBittrexSubscribed, write=FOnBittrexSubscribed};
	__property TsgcWSBittrexUnSubscribedEvent OnBittrexUnSubscribed = {read=FOnBittrexUnSubscribed, write=FOnBittrexUnSubscribed};
	__property TsgcWSBittrexMessageEvent OnBittrexMessage = {read=FOnBittrexMessage, write=FOnBittrexMessage};
	__property TsgcWSBittrexErrorEvent OnBittrexError = {read=FOnBittrexError, write=FOnBittrexError};
	__property TsgcWSBittrexDisconnectEvent OnBittrexDisconnect = {read=FOnBittrexDisconnect, write=FOnBittrexDisconnect};
	__property TsgcWSBittrexAuthenticatedEvent OnBittrexAuthenticated = {read=FOnBittrexAuthenticated, write=FOnBittrexAuthenticated};
	__property TsgcWSBittrexHeartBeatEvent OnBittrexHeartBeat = {read=FOnBittrexHeartBeat, write=FOnBittrexHeartBeat};
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_api_bittrex */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_API_BITTREX)
using namespace Sgcwebsocket_api_bittrex;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_api_bittrexHPP
