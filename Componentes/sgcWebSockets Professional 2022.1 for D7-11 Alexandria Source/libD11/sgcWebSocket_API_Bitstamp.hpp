// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_API_Bitstamp.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_api_bitstampHPP
#define Sgcwebsocket_api_bitstampHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <sgcWebSocket_API_Pusher.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_api_bitstamp
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWS_API_BitstampV1;
class DELPHICLASS TsgcWS_API_Bitstamp;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcWS_API_BitstampV1 : public Sgcwebsocket_api_pusher::TsgcWS_API_Pusher
{
	typedef Sgcwebsocket_api_pusher::TsgcWS_API_Pusher inherited;
	
public:
	__fastcall virtual TsgcWS_API_BitstampV1(System::Classes::TComponent* aOwner);
	void __fastcall SubscribeTicker(const System::UnicodeString aCurrency = System::UnicodeString());
	void __fastcall UnSubscribeTicker(const System::UnicodeString aCurrency = System::UnicodeString());
	void __fastcall SubscribeOrderBook(const System::UnicodeString aCurrency = System::UnicodeString());
	void __fastcall UnSubscribeOrderBook(const System::UnicodeString aCurrency = System::UnicodeString());
	void __fastcall SubscribeFullOrderBook(const System::UnicodeString aCurrency = System::UnicodeString());
	void __fastcall UnSubscribeFullOrderBook(const System::UnicodeString aCurrency = System::UnicodeString());
	void __fastcall SubscribeOrders(const System::UnicodeString aCurrency = System::UnicodeString());
	void __fastcall UnSubscribeOrders(const System::UnicodeString aCurrency = System::UnicodeString());
public:
	/* TsgcWS_API_Pusher.Destroy */ inline __fastcall virtual ~TsgcWS_API_BitstampV1() { }
	
};


class PASCALIMPLEMENTATION TsgcWS_API_Bitstamp : public Sgcwebsocket_classes::TsgcWSAPI_client
{
	typedef Sgcwebsocket_classes::TsgcWSAPI_client inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetURL();
	virtual void __fastcall DoWriteData(const System::UnicodeString aText);
	virtual void __fastcall DoSendBitstampMessage(const System::UnicodeString aEvent, const System::UnicodeString aChannel);
	virtual void __fastcall DoSubscribe(const System::UnicodeString aChannel);
	virtual void __fastcall DoUnSubscribe(const System::UnicodeString aChannel);
	
public:
	void __fastcall SubscribeLiveTicker(const System::UnicodeString aCurrencyPair);
	void __fastcall UnSubscribeLiveTicker(const System::UnicodeString aCurrencyPair);
	void __fastcall SubscribeLiveOrders(const System::UnicodeString aCurrencyPair);
	void __fastcall UnSubscribeLiveOrders(const System::UnicodeString aCurrencyPair);
	void __fastcall SubscribeLiveOrderBook(const System::UnicodeString aCurrencyPair);
	void __fastcall UnSubscribeLiveOrderBook(const System::UnicodeString aCurrencyPair);
	void __fastcall SubscribeLiveDetailOrderBook(const System::UnicodeString aCurrencyPair);
	void __fastcall UnSubscribeLiveDetailOrderBook(const System::UnicodeString aCurrencyPair);
	void __fastcall SubscribeLiveFullOrderBook(const System::UnicodeString aCurrencyPair);
	void __fastcall UnSubscribeLiveFullOrderBook(const System::UnicodeString aCurrencyPair);
public:
	/* TsgcWSAPI.Create */ inline __fastcall virtual TsgcWS_API_Bitstamp(System::Classes::TComponent* aOwner) : Sgcwebsocket_classes::TsgcWSAPI_client(aOwner) { }
	
public:
	/* TsgcWSComponent_Base.Destroy */ inline __fastcall virtual ~TsgcWS_API_Bitstamp() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_api_bitstamp */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_API_BITSTAMP)
using namespace Sgcwebsocket_api_bitstamp;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_api_bitstampHPP
