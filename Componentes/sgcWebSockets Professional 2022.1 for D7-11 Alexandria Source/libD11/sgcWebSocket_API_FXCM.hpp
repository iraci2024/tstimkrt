// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_API_FXCM.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_api_fxcmHPP
#define Sgcwebsocket_api_fxcmHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <System.SysUtils.hpp>
#include <sgcBase_Helpers.hpp>
#include <sgcWebSocket_API_SocketIO.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_api_fxcm
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSFXCM_Options;
class DELPHICLASS TsgcWS_API_FXCM;
//-- type declarations -------------------------------------------------------
enum DECLSPEC_DENUM TsgcWSFXCMHTTPMethods : unsigned char { httpGET, httpPOST };

enum DECLSPEC_DENUM TsgcWSFXCMTableModels : unsigned char { tmoOffer, tmoOpenPosition, tmoClosedPosition, tmoOrder, tmoAccount, tmoSummary, tmoLeverageProfile, tmoProperties };

enum DECLSPEC_DENUM TsgcWSFXCMTradingMethod : unsigned char { tmeOpenTrade, tmeCloseTrade, tmeChangeOrder, tmeDeleteOrder, tmeCreateEntryOrder, tmeSimpleEco, tmeAddToOco, tmeRemoveFromOco, tmeEditOco, tmeChangeTradeStopLimit, tmeChangeOrderStopLimit, tmeCloseAllForSymbol };

enum DECLSPEC_DENUM TsgcWSFXCMTimeFrame : unsigned char { tmfMin1, tmfMin5, tmfMin15, tmfMin30, tmfHour1, tmfHour2, tmfHour3, tmfHour4, tmfHour6, tmfHour8, tmfDay1, tmfWeek1, tmfMonth1 };

#pragma pack(push,4)
class PASCALIMPLEMENTATION TsgcWSFXCM_Options : public System::Classes::TPersistent
{
	typedef System::Classes::TPersistent inherited;
	
private:
	System::UnicodeString FAccessToken;
	System::UnicodeString FHost;
	int FPort;
	bool FTLS;
	
public:
	__fastcall virtual TsgcWSFXCM_Options();
	virtual void __fastcall Assign(System::Classes::TPersistent* aSource);
	
__published:
	__property System::UnicodeString AccessToken = {read=FAccessToken, write=FAccessToken};
	__property System::UnicodeString Host = {read=FHost, write=FHost};
	__property int Port = {read=FPort, write=FPort, nodefault};
	__property bool TLS = {read=FTLS, write=FTLS, nodefault};
public:
	/* TPersistent.Destroy */ inline __fastcall virtual ~TsgcWSFXCM_Options() { }
	
};

#pragma pack(pop)

class PASCALIMPLEMENTATION TsgcWS_API_FXCM : public Sgcwebsocket_api_socketio::TsgcWS_API_SocketIO
{
	typedef Sgcwebsocket_api_socketio::TsgcWS_API_SocketIO inherited;
	
protected:
	virtual System::UnicodeString __fastcall GetURL();
	virtual System::UnicodeString __fastcall DoGetSessionURL();
	
private:
	TsgcWSFXCM_Options* FFXCM;
	void __fastcall SetFXCM(TsgcWSFXCM_Options* const Value);
	
public:
	__property TsgcWSFXCM_Options* FXCM = {read=FFXCM, write=SetFXCM};
	
private:
	System::UnicodeString __fastcall GetSymbolsString(const System::UnicodeString aSymbol1, const System::UnicodeString aSymbol2);
	System::UnicodeString __fastcall GetPairs(const System::UnicodeString aSymbol1, const System::UnicodeString aSymbol2);
	System::UnicodeString __fastcall GetModelString(TsgcWSFXCMTableModels aModel);
	System::UnicodeString __fastcall GetTradingMethod(TsgcWSFXCMTradingMethod aMethod);
	System::UnicodeString __fastcall GetTimeFrame(TsgcWSFXCMTimeFrame aTimeFrame);
	
protected:
	virtual System::UnicodeString __fastcall DoRequest(const System::UnicodeString aURI, const System::UnicodeString aBody, const TsgcWSFXCMHTTPMethods aMethod);
	
public:
	System::UnicodeString __fastcall GetSymbols();
	System::UnicodeString __fastcall SubscribeMarketData(const System::UnicodeString aSymbol1, const System::UnicodeString aSymbol2);
	System::UnicodeString __fastcall UnSubscribeMarketData(const System::UnicodeString aSymbol1, const System::UnicodeString aSymbol2);
	System::UnicodeString __fastcall SubscribeTradingTables(const TsgcWSFXCMTableModels aModel);
	System::UnicodeString __fastcall UnSubscribeTradingTables(const TsgcWSFXCMTableModels aModel);
	System::UnicodeString __fastcall SnapshotTradingTables(const TsgcWSFXCMTableModels aModel);
	System::UnicodeString __fastcall UpdateSubscriptions(const System::UnicodeString aSymbol1, const System::UnicodeString aSymbol2, bool aVisible);
	System::UnicodeString __fastcall TradingOrder(const TsgcWSFXCMTradingMethod aTradingMethod, const System::UnicodeString aParameters);
	System::UnicodeString __fastcall GetHistoricalData(int aOfferId, TsgcWSFXCMTimeFrame aTimeFrame, int aCandels);
	__fastcall virtual TsgcWS_API_FXCM(System::Classes::TComponent* aOwner);
	__fastcall virtual ~TsgcWS_API_FXCM();
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_api_fxcm */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_API_FXCM)
using namespace Sgcwebsocket_api_fxcm;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_api_fxcmHPP
