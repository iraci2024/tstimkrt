// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_API_Bitfinex.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_api_bitfinexHPP
#define Sgcwebsocket_api_bitfinexHPP

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
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_api_bitfinex
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWS_API_Bitfinex;
//-- type declarations -------------------------------------------------------
typedef void __fastcall (__closure *TsgcWSBitfinexConnectEvent)(System::TObject* Sender, System::UnicodeString Version);

typedef void __fastcall (__closure *TsgcWSBifinexInfoMessageEvent)(System::TObject* Sender, System::UnicodeString Code, System::UnicodeString Msg);

typedef void __fastcall (__closure *TsgcWSBitfinexAuthEvent)(System::TObject* Sender, System::UnicodeString Status, int ChanId, int Code, System::UnicodeString Msg);

typedef void __fastcall (__closure *TsgcWSBitfinexUnauthEvent)(System::TObject* Sender, System::UnicodeString Status, int ChanId);

typedef void __fastcall (__closure *TsgcWSBitfinexSubscribedEvent)(System::TObject* Sender, System::UnicodeString Channel, int ChanId, System::UnicodeString Symbol, System::UnicodeString Pair, System::UnicodeString Key);

typedef void __fastcall (__closure *TsgcWSBitfinexUnSubscribedEvent)(System::TObject* Sender, System::UnicodeString Status, int ChanId);

typedef void __fastcall (__closure *TsgcWSBitfinexUpdate)(System::TObject* Sender, System::UnicodeString Data);

typedef void __fastcall (__closure *TsgcWSBitfinexErrorEvent)(System::TObject* Sender, System::UnicodeString Status, int ChanId, int Code);

enum DECLSPEC_DENUM TsgcWSBitfinexPrecision : unsigned char { P0, P1, P2, P3 };

enum DECLSPEC_DENUM TsgcWSBitfinexFrequency : unsigned char { F0, F1 };

class PASCALIMPLEMENTATION TsgcWS_API_Bitfinex : public Sgcwebsocket_classes::TsgcWSAPI_client
{
	typedef Sgcwebsocket_classes::TsgcWSAPI_client inherited;
	
protected:
	virtual void __fastcall DoEventMessage(Sgcwebsocket_classes::TsgcWSConnection* aConnection, const System::UnicodeString Text);
	virtual System::UnicodeString __fastcall GetURL();
	
private:
	Sgcjson::TsgcJSON* FJSON;
	Sgcjson::TsgcJSON* __fastcall GetJSON();
	
protected:
	__property Sgcjson::TsgcJSON* JSON = {read=GetJSON, write=FJSON};
	
private:
	System::UnicodeString __fastcall GetPrecision(TsgcWSBitfinexPrecision aPrecision);
	System::UnicodeString __fastcall GetFrequency(TsgcWSBitfinexFrequency aFrequency);
	
protected:
	void __fastcall DoReadEvent(const System::UnicodeString aEvent);
	void __fastcall DoPong(const System::UnicodeString aCid);
	
public:
	void __fastcall Ping();
	void __fastcall Configuration(int aFlags);
	void __fastcall Authenticate(System::UnicodeString aAPIKey, System::UnicodeString aAuthPayload, System::UnicodeString aAuthNonce, System::UnicodeString aAuthSig);
	void __fastcall UnAuthenticate();
	void __fastcall SubscribeTicker(const System::UnicodeString aSymbol);
	void __fastcall UnSubscribeTicker(const int aChanId);
	void __fastcall SubscribeTrades(const System::UnicodeString aSymbol);
	void __fastcall UnSubscribeTrades(const int aChanId);
	void __fastcall SubscribeOrderBook(const System::UnicodeString aPair, TsgcWSBitfinexPrecision aPrecision = (TsgcWSBitfinexPrecision)(0x0), TsgcWSBitfinexFrequency aFrequency = (TsgcWSBitfinexFrequency)(0x0), int aLength = 0x19);
	void __fastcall UnSubscribeOrderBook(int aChanId);
	void __fastcall SubscribeRawOrderBook(const System::UnicodeString aPair);
	void __fastcall UnSubscribeRawOrderBook(int aChanId);
	void __fastcall SubscribeCandles(const System::UnicodeString aKey);
	void __fastcall UnSubscribeCandles(int aChanId);
	
private:
	TsgcWSBitfinexAuthEvent FOnBitfinexAuth;
	TsgcWSBitfinexConnectEvent FOnBitfinexConnect;
	TsgcWSBitfinexErrorEvent FOnBitfinexError;
	TsgcWSBifinexInfoMessageEvent FOnBitfinexInfoMsg;
	TsgcWSBitfinexSubscribedEvent FOnBitfinexSubscribed;
	TsgcWSBitfinexUnauthEvent FOnBitfinexUnauth;
	TsgcWSBitfinexUnSubscribedEvent FOnBitfinexUnSubscribed;
	TsgcWSBitfinexUpdate FOnBitfinexUpdate;
	
protected:
	virtual void __fastcall DoEventConnected();
	virtual void __fastcall DoEventAuthentication();
	virtual void __fastcall DoEventUnauthentication();
	virtual void __fastcall DoEventInfoMessage();
	virtual void __fastcall DoEventSubscribed();
	virtual void __fastcall DoEventUnSubscribed();
	virtual void __fastcall DoEventUpdate(System::UnicodeString aData);
	virtual void __fastcall DoEventErr();
	
public:
	__property TsgcWSBifinexInfoMessageEvent OnBitfinexInfoMsg = {read=FOnBitfinexInfoMsg, write=FOnBitfinexInfoMsg};
	__property TsgcWSBitfinexSubscribedEvent OnBitfinexSubscribed = {read=FOnBitfinexSubscribed, write=FOnBitfinexSubscribed};
	__property TsgcWSBitfinexUnSubscribedEvent OnBitfinexUnSubscribed = {read=FOnBitfinexUnSubscribed, write=FOnBitfinexUnSubscribed};
	__property TsgcWSBitfinexUpdate OnBitfinexUpdate = {read=FOnBitfinexUpdate, write=FOnBitfinexUpdate};
	__property TsgcWSBitfinexAuthEvent OnBitfinexAuth = {read=FOnBitfinexAuth, write=FOnBitfinexAuth};
	__property TsgcWSBitfinexUnauthEvent OnBitfinexUnauth = {read=FOnBitfinexUnauth, write=FOnBitfinexUnauth};
	__property TsgcWSBitfinexErrorEvent OnBitfinexError = {read=FOnBitfinexError, write=FOnBitfinexError};
	__property TsgcWSBitfinexConnectEvent OnBitfinexConnect = {read=FOnBitfinexConnect, write=FOnBitfinexConnect};
public:
	/* TsgcWSAPI.Create */ inline __fastcall virtual TsgcWS_API_Bitfinex(System::Classes::TComponent* aOwner) : Sgcwebsocket_classes::TsgcWSAPI_client(aOwner) { }
	
public:
	/* TsgcWSComponent_Base.Destroy */ inline __fastcall virtual ~TsgcWS_API_Bitfinex() { }
	
};


//-- var, const, procedure ---------------------------------------------------
static const System::Int8 CS_DEC_S = System::Int8(0x8);
static const System::Int8 CS_TIME_S = System::Int8(0x20);
static const int CS_SEQ_ALL = int(0x10000);
static const int CHECKSUM = int(0x20000);
}	/* namespace Sgcwebsocket_api_bitfinex */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_API_BITFINEX)
using namespace Sgcwebsocket_api_bitfinex;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_api_bitfinexHPP
