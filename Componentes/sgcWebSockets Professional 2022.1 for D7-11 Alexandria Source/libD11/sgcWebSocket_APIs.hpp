// CodeGear C++Builder
// Copyright (c) 1995, 2022 by Embarcadero Technologies, Inc.
// All rights reserved

// (DO NOT EDIT: machine generated header) 'sgcWebSocket_APIs.pas' rev: 35.00 (Windows)

#ifndef Sgcwebsocket_apisHPP
#define Sgcwebsocket_apisHPP

#pragma delphiheader begin
#pragma option push
#pragma option -w-      // All warnings off
#pragma option -Vx      // Zero-length empty class member 
#pragma pack(push,8)
#include <System.hpp>
#include <SysInit.hpp>
#include <System.Classes.hpp>
#include <sgcWebSocket_API_Bitfinex.hpp>
#include <sgcWebSocket_API_Blockchain.hpp>
#include <sgcWebSocket_API_Pusher.hpp>
#include <sgcWebSocket_API_SignalR.hpp>
#include <sgcWebSocket_API_Bittrex.hpp>
#include <sgcWebSocket_API_Binance.hpp>
#include <sgcWebSocket_API_SocketIO.hpp>
#include <sgcWebSocket_API_Bitstamp.hpp>
#include <sgcWebSocket_API_Huobi.hpp>
#include <sgcWebSocket_API_Cex.hpp>
#include <sgcWebSocket_API_Bitmex.hpp>
#include <sgcWebSocket_API_SignalRCore.hpp>
#include <sgcWebSocket_API_FXCM.hpp>
#include <sgcWebSocket_API_Kraken.hpp>
#include <sgcWebSocket_API_Coinbase.hpp>
#include <sgcWebSocket_API_FTX.hpp>
#include <sgcWebSocket_API_Discord.hpp>
#include <sgcWebSocket_API_ThreeCommas.hpp>
#include <sgcWebSocket_Classes.hpp>
#include <sgcSocket_Classes.hpp>
#include <sgcBase_Classes.hpp>

//-- user supplied -----------------------------------------------------------

namespace Sgcwebsocket_apis
{
//-- forward type declarations -----------------------------------------------
class DELPHICLASS TsgcWSAPI_Bitfinex;
class DELPHICLASS TsgcWSAPI_Blockchain;
class DELPHICLASS TsgcWSAPI_Pusher;
class DELPHICLASS TsgcWSAPI_SignalR;
class DELPHICLASS TsgcWSAPI_SignalRCore;
class DELPHICLASS TsgcWSAPI_Bittrex;
class DELPHICLASS TsgcWSAPI_Binance;
class DELPHICLASS TsgcWSAPI_Binance_Futures;
class DELPHICLASS TsgcWSAPI_SocketIO;
class DELPHICLASS TsgcWSAPI_Bitstamp;
class DELPHICLASS TsgcWSAPI_Huobi;
class DELPHICLASS TsgcWSAPI_Cex;
class DELPHICLASS TsgcWSAPI_Bitmex;
class DELPHICLASS TsgcWSAPI_FXCM;
class DELPHICLASS TsgcWSAPI_Kraken;
class DELPHICLASS TsgcWSAPI_Kraken_Futures;
class DELPHICLASS TsgcWSAPI_Discord;
class DELPHICLASS TsgcWSAPI_Coinbase;
class DELPHICLASS TsgcWSAPI_FTX;
class DELPHICLASS TsgcWSAPI_ThreeCommas;
//-- type declarations -------------------------------------------------------
class PASCALIMPLEMENTATION TsgcWSAPI_Bitfinex : public Sgcwebsocket_api_bitfinex::TsgcWS_API_Bitfinex
{
	typedef Sgcwebsocket_api_bitfinex::TsgcWS_API_Bitfinex inherited;
	
__published:
	__property OnConnect;
	__property OnDisconnect;
	__property OnMessage;
	__property OnError;
	__property OnException;
	__property OnBitfinexConnect;
	__property OnBitfinexAuth;
	__property OnBitfinexUnauth;
	__property OnBitfinexInfoMsg;
	__property OnBitfinexSubscribed;
	__property OnBitfinexUnSubscribed;
	__property OnBitfinexUpdate;
	__property OnBitfinexError;
	__property Client;
	__property RawMessages;
	__property Version = {default=0};
public:
	/* TsgcWSAPI.Create */ inline __fastcall virtual TsgcWSAPI_Bitfinex(System::Classes::TComponent* aOwner) : Sgcwebsocket_api_bitfinex::TsgcWS_API_Bitfinex(aOwner) { }
	
public:
	/* TsgcWSComponent_Base.Destroy */ inline __fastcall virtual ~TsgcWSAPI_Bitfinex() { }
	
};


class PASCALIMPLEMENTATION TsgcWSAPI_Blockchain : public Sgcwebsocket_api_blockchain::TsgcWS_API_Blockchain
{
	typedef Sgcwebsocket_api_blockchain::TsgcWS_API_Blockchain inherited;
	
__published:
	__property OnConnect;
	__property OnDisconnect;
	__property OnMessage;
	__property OnError;
	__property OnException;
	__property OnBlockchainNewBlock;
	__property OnBlockchainNewTransaction;
	__property OnBlockchainPong;
	__property Client;
	__property RawMessages;
	__property Version = {default=0};
public:
	/* TsgcWS_API_Blockchain.Create */ inline __fastcall virtual TsgcWSAPI_Blockchain(System::Classes::TComponent* aOwner) : Sgcwebsocket_api_blockchain::TsgcWS_API_Blockchain(aOwner) { }
	
public:
	/* TsgcWSComponent_Base.Destroy */ inline __fastcall virtual ~TsgcWSAPI_Blockchain() { }
	
};


class PASCALIMPLEMENTATION TsgcWSAPI_Pusher : public Sgcwebsocket_api_pusher::TsgcWS_API_Pusher
{
	typedef Sgcwebsocket_api_pusher::TsgcWS_API_Pusher inherited;
	
__published:
	__property OnConnect;
	__property OnDisconnect;
	__property OnMessage;
	__property OnError;
	__property OnException;
	__property OnPusherConnect;
	__property OnPusherError;
	__property OnPusherEvent;
	__property OnPusherSubscribe;
	__property OnPusherAuthentication;
	__property Client;
	__property Pusher;
	__property RawMessages;
	__property Version = {default=0};
public:
	/* TsgcWS_API_Pusher.Create */ inline __fastcall virtual TsgcWSAPI_Pusher(System::Classes::TComponent* aOwner) : Sgcwebsocket_api_pusher::TsgcWS_API_Pusher(aOwner) { }
	/* TsgcWS_API_Pusher.Destroy */ inline __fastcall virtual ~TsgcWSAPI_Pusher() { }
	
};


class PASCALIMPLEMENTATION TsgcWSAPI_SignalR : public Sgcwebsocket_api_signalr::TsgcWS_API_SignalR
{
	typedef Sgcwebsocket_api_signalr::TsgcWS_API_SignalR inherited;
	
__published:
	__property OnSignalRConnect;
	__property OnSignalRMessage;
	__property OnSignalRKeepAlive;
	__property OnSignalRResult;
	__property OnSignalRError;
	__property OnSignalRDisconnect;
	__property Client;
	__property SignalR;
	__property RawMessages;
	__property Version = {default=0};
public:
	/* TsgcWS_API_SignalR.Create */ inline __fastcall virtual TsgcWSAPI_SignalR(System::Classes::TComponent* aOwner) : Sgcwebsocket_api_signalr::TsgcWS_API_SignalR(aOwner) { }
	/* TsgcWS_API_SignalR.Destroy */ inline __fastcall virtual ~TsgcWSAPI_SignalR() { }
	
};


class PASCALIMPLEMENTATION TsgcWSAPI_SignalRCore : public Sgcwebsocket_api_signalrcore::TsgcWS_API_SignalRCore
{
	typedef Sgcwebsocket_api_signalrcore::TsgcWS_API_SignalRCore inherited;
	
__published:
	__property OnSignalRCoreBeforeConnect;
	__property OnSignalRCoreConnect;
	__property OnSignalRCoreInvocation;
	__property OnSignalRCoreStreamInvocation;
	__property OnSignalRCoreStreamItem;
	__property OnSignalRCoreCompletion;
	__property OnSignalRCoreCancelInvocation;
	__property OnSignalRCoreKeepAlive;
	__property OnSignalRCoreError;
	__property OnSignalRCoreClose;
	__property OnSignalRCoreDisconnect;
	__property Client;
	__property SignalRCore;
	__property RawMessages;
	__property Version = {default=0};
public:
	/* TsgcWS_API_SignalRCore.Create */ inline __fastcall virtual TsgcWSAPI_SignalRCore(System::Classes::TComponent* aOwner) : Sgcwebsocket_api_signalrcore::TsgcWS_API_SignalRCore(aOwner) { }
	/* TsgcWS_API_SignalRCore.Destroy */ inline __fastcall virtual ~TsgcWSAPI_SignalRCore() { }
	
};


class PASCALIMPLEMENTATION TsgcWSAPI_Bittrex : public Sgcwebsocket_api_bittrex::TsgcWS_API_Bittrex
{
	typedef Sgcwebsocket_api_bittrex::TsgcWS_API_Bittrex inherited;
	
__published:
	__property OnBittrexConnect;
	__property OnBittrexSubscribed;
	__property OnBittrexUnSubscribed;
	__property OnBittrexMessage;
	__property OnBittrexAuthenticated;
	__property OnBittrexHeartBeat;
	__property OnBittrexError;
	__property OnBittrexDisconnect;
	__property Client;
	__property SignalR;
	__property Bittrex;
	__property RawMessages;
	__property Version = {default=0};
public:
	/* TsgcWS_API_Bittrex.Create */ inline __fastcall virtual TsgcWSAPI_Bittrex(System::Classes::TComponent* aOwner) : Sgcwebsocket_api_bittrex::TsgcWS_API_Bittrex(aOwner) { }
	/* TsgcWS_API_Bittrex.Destroy */ inline __fastcall virtual ~TsgcWSAPI_Bittrex() { }
	
};


class PASCALIMPLEMENTATION TsgcWSAPI_Binance : public Sgcwebsocket_api_binance::TsgcWS_API_Binance
{
	typedef Sgcwebsocket_api_binance::TsgcWS_API_Binance inherited;
	
__published:
	__property OnBinanceHTTPException;
	__property Client;
	__property Binance;
	__property Version = {default=0};
public:
	/* TsgcWS_API_Binance.Destroy */ inline __fastcall virtual ~TsgcWSAPI_Binance() { }
	
public:
	/* TsgcWS_API_Binance_Base.Create */ inline __fastcall virtual TsgcWSAPI_Binance(System::Classes::TComponent* aOwner) : Sgcwebsocket_api_binance::TsgcWS_API_Binance(aOwner) { }
	
};


class PASCALIMPLEMENTATION TsgcWSAPI_Binance_Futures : public Sgcwebsocket_api_binance::TsgcWS_API_Binance_Futures
{
	typedef Sgcwebsocket_api_binance::TsgcWS_API_Binance_Futures inherited;
	
__published:
	__property OnBinanceHTTPException;
	__property Client;
	__property Binance;
	__property FuturesContracts;
	__property Version = {default=0};
public:
	/* TsgcWS_API_Binance_Base.Create */ inline __fastcall virtual TsgcWSAPI_Binance_Futures(System::Classes::TComponent* aOwner) : Sgcwebsocket_api_binance::TsgcWS_API_Binance_Futures(aOwner) { }
	/* TsgcWS_API_Binance_Base.Destroy */ inline __fastcall virtual ~TsgcWSAPI_Binance_Futures() { }
	
};


class PASCALIMPLEMENTATION TsgcWSAPI_SocketIO : public Sgcwebsocket_api_socketio::TsgcWS_API_SocketIO
{
	typedef Sgcwebsocket_api_socketio::TsgcWS_API_SocketIO inherited;
	
__published:
	__property Client;
	__property SocketIO;
	__property OnHTTPConnectionSSL;
	__property OnHTTPRequest;
	__property OnAfterConnect;
	__property Version = {default=0};
public:
	/* TsgcWS_API_SocketIO.Create */ inline __fastcall virtual TsgcWSAPI_SocketIO(System::Classes::TComponent* aOwner) : Sgcwebsocket_api_socketio::TsgcWS_API_SocketIO(aOwner) { }
	/* TsgcWS_API_SocketIO.Destroy */ inline __fastcall virtual ~TsgcWSAPI_SocketIO() { }
	
};


class PASCALIMPLEMENTATION TsgcWSAPI_Bitstamp : public Sgcwebsocket_api_bitstamp::TsgcWS_API_Bitstamp
{
	typedef Sgcwebsocket_api_bitstamp::TsgcWS_API_Bitstamp inherited;
	
__published:
	__property OnConnect;
	__property OnDisconnect;
	__property OnMessage;
	__property OnError;
	__property OnException;
	__property Client;
	__property RawMessages;
	__property Version = {default=0};
public:
	/* TsgcWSAPI.Create */ inline __fastcall virtual TsgcWSAPI_Bitstamp(System::Classes::TComponent* aOwner) : Sgcwebsocket_api_bitstamp::TsgcWS_API_Bitstamp(aOwner) { }
	
public:
	/* TsgcWSComponent_Base.Destroy */ inline __fastcall virtual ~TsgcWSAPI_Bitstamp() { }
	
};


class PASCALIMPLEMENTATION TsgcWSAPI_Huobi : public Sgcwebsocket_api_huobi::TsgcWS_API_Huobi
{
	typedef Sgcwebsocket_api_huobi::TsgcWS_API_Huobi inherited;
	
__published:
	__property OnConnect;
	__property OnDisconnect;
	__property OnError;
	__property OnException;
	__property OnHuobiSubscribed;
	__property OnHuobiUnSubscribed;
	__property OnHuobiUpdate;
	__property OnHuobiError;
	__property Client;
	__property RawMessages;
	__property Version = {default=0};
public:
	/* TsgcWS_API_Huobi.Create */ inline __fastcall virtual TsgcWSAPI_Huobi(System::Classes::TComponent* aOwner) : Sgcwebsocket_api_huobi::TsgcWS_API_Huobi(aOwner) { }
	/* TsgcWS_API_Huobi.Destroy */ inline __fastcall virtual ~TsgcWSAPI_Huobi() { }
	
};


class PASCALIMPLEMENTATION TsgcWSAPI_Cex : public Sgcwebsocket_api_cex::TsgcWS_API_Cex
{
	typedef Sgcwebsocket_api_cex::TsgcWS_API_Cex inherited;
	
__published:
	__property OnConnect;
	__property OnCexConnect;
	__property OnCexSubscribed;
	__property OnCexMessage;
	__property OnCexAuthenticated;
	__property OnCexError;
	__property OnCexDisconnecting;
	__property OnCexDisconnect;
	__property OnDisconnect;
	__property Client;
	__property Cex;
	__property RawMessages;
	__property Version = {default=0};
public:
	/* TsgcWS_API_Cex.Create */ inline __fastcall virtual TsgcWSAPI_Cex(System::Classes::TComponent* aOwner) : Sgcwebsocket_api_cex::TsgcWS_API_Cex(aOwner) { }
	/* TsgcWS_API_Cex.Destroy */ inline __fastcall virtual ~TsgcWSAPI_Cex() { }
	
};


class PASCALIMPLEMENTATION TsgcWSAPI_Bitmex : public Sgcwebsocket_api_bitmex::TsgcWS_API_Bitmex
{
	typedef Sgcwebsocket_api_bitmex::TsgcWS_API_Bitmex inherited;
	
__published:
	__property OnConnect;
	__property OnBitmexConnect;
	__property OnBitmexSubscribed;
	__property OnBitmexUnsubscribed;
	__property OnBitmexMessage;
	__property OnBitmexAuthenticated;
	__property OnBitmexError;
	__property OnDisconnect;
	__property OnBitmexHTTPException;
	__property Client;
	__property Bitmex;
	__property RawMessages;
	__property Version = {default=0};
public:
	/* TsgcWS_API_Bitmex.Create */ inline __fastcall virtual TsgcWSAPI_Bitmex(System::Classes::TComponent* aOwner) : Sgcwebsocket_api_bitmex::TsgcWS_API_Bitmex(aOwner) { }
	/* TsgcWS_API_Bitmex.Destroy */ inline __fastcall virtual ~TsgcWSAPI_Bitmex() { }
	
};


class PASCALIMPLEMENTATION TsgcWSAPI_FXCM : public Sgcwebsocket_api_fxcm::TsgcWS_API_FXCM
{
	typedef Sgcwebsocket_api_fxcm::TsgcWS_API_FXCM inherited;
	
__published:
	__property OnConnect;
	__property OnDisconnect;
	__property OnMessage;
	__property OnError;
	__property OnException;
	__property Client;
	__property SocketIO;
	__property FXCM;
	__property OnHTTPConnectionSSL;
	__property RawMessages;
	__property Version = {default=0};
public:
	/* TsgcWS_API_FXCM.Create */ inline __fastcall virtual TsgcWSAPI_FXCM(System::Classes::TComponent* aOwner) : Sgcwebsocket_api_fxcm::TsgcWS_API_FXCM(aOwner) { }
	/* TsgcWS_API_FXCM.Destroy */ inline __fastcall virtual ~TsgcWSAPI_FXCM() { }
	
};


class PASCALIMPLEMENTATION TsgcWSAPI_Kraken : public Sgcwebsocket_api_kraken::TsgcWS_API_Kraken
{
	typedef Sgcwebsocket_api_kraken::TsgcWS_API_Kraken inherited;
	
__published:
	__property OnConnect;
	__property OnKrakenConnect;
	__property OnKrakenSystemStatus;
	__property OnKrakenSubscribed;
	__property OnKrakenUnSubscribed;
	__property OnKrakenSubscriptionError;
	__property OnKrakenData;
	__property OnDisconnect;
	__property OnKrakenHTTPException;
	__property Client;
	__property Kraken;
	__property RawMessages;
	__property Version = {default=0};
public:
	/* TsgcWS_API_Kraken.Create */ inline __fastcall virtual TsgcWSAPI_Kraken(System::Classes::TComponent* aOwner) : Sgcwebsocket_api_kraken::TsgcWS_API_Kraken(aOwner) { }
	/* TsgcWS_API_Kraken.Destroy */ inline __fastcall virtual ~TsgcWSAPI_Kraken() { }
	
};


class PASCALIMPLEMENTATION TsgcWSAPI_Kraken_Futures : public Sgcwebsocket_api_kraken::TsgcWS_API_Kraken_Futures
{
	typedef Sgcwebsocket_api_kraken::TsgcWS_API_Kraken_Futures inherited;
	
__published:
	__property OnConnect;
	__property OnKrakenFuturesConnect;
	__property OnKrakenFuturesSubscribed;
	__property OnKrakenFuturesUnSubscribed;
	__property OnKrakenFuturesError;
	__property OnKrakenData;
	__property OnDisconnect;
	__property OnKrakenHTTPException;
	__property Client;
	__property Kraken;
	__property RawMessages;
	__property Version = {default=0};
public:
	/* TsgcWS_API_Kraken_Futures.Create */ inline __fastcall virtual TsgcWSAPI_Kraken_Futures(System::Classes::TComponent* aOwner) : Sgcwebsocket_api_kraken::TsgcWS_API_Kraken_Futures(aOwner) { }
	/* TsgcWS_API_Kraken_Futures.Destroy */ inline __fastcall virtual ~TsgcWSAPI_Kraken_Futures() { }
	
};


class PASCALIMPLEMENTATION TsgcWSAPI_Discord : public Sgcwebsocket_api_discord::TsgcWS_API_Discord
{
	typedef Sgcwebsocket_api_discord::TsgcWS_API_Discord inherited;
	
__published:
	__property OnConnect;
	__property OnDiscordBeforeReconnect;
	__property OnDiscordReady;
	__property OnDiscordResumed;
	__property OnDiscordDispatch;
	__property OnDiscordEvent;
	__property OnDisconnect;
	__property Client;
	__property DiscordOptions;
	__property RawMessages;
	__property Version = {default=0};
public:
	/* TsgcWS_API_Discord.Create */ inline __fastcall virtual TsgcWSAPI_Discord(System::Classes::TComponent* aOwner) : Sgcwebsocket_api_discord::TsgcWS_API_Discord(aOwner) { }
	/* TsgcWS_API_Discord.Destroy */ inline __fastcall virtual ~TsgcWSAPI_Discord() { }
	
};


class PASCALIMPLEMENTATION TsgcWSAPI_Coinbase : public Sgcwebsocket_api_coinbase::TsgcWS_API_Coinbase
{
	typedef Sgcwebsocket_api_coinbase::TsgcWS_API_Coinbase inherited;
	
__published:
	__property OnConnect;
	__property OnCoinbaseSubscriptions;
	__property OnCoinbaseMessage;
	__property OnCoinbaseError;
	__property OnCoinbaseHTTPException;
	__property OnDisconnect;
	__property Client;
	__property Coinbase;
	__property RawMessages;
	__property Version = {default=0};
public:
	/* TsgcWS_API_Coinbase.Create */ inline __fastcall virtual TsgcWSAPI_Coinbase(System::Classes::TComponent* aOwner) : Sgcwebsocket_api_coinbase::TsgcWS_API_Coinbase(aOwner) { }
	/* TsgcWS_API_Coinbase.Destroy */ inline __fastcall virtual ~TsgcWSAPI_Coinbase() { }
	
};


class PASCALIMPLEMENTATION TsgcWSAPI_FTX : public Sgcwebsocket_api_ftx::TsgcWS_API_FTX
{
	typedef Sgcwebsocket_api_ftx::TsgcWS_API_FTX inherited;
	
__published:
	__property OnConnect;
	__property OnFTXSubscribed;
	__property OnFTXUnsubscribed;
	__property OnFTXMessage;
	__property OnFTXError;
	__property OnFTXHTTPException;
	__property OnDisconnect;
	__property Client;
	__property FTX;
	__property RawMessages;
	__property Version = {default=0};
public:
	/* TsgcWS_API_FTX.Create */ inline __fastcall virtual TsgcWSAPI_FTX(System::Classes::TComponent* aOwner) : Sgcwebsocket_api_ftx::TsgcWS_API_FTX(aOwner) { }
	/* TsgcWS_API_FTX.Destroy */ inline __fastcall virtual ~TsgcWSAPI_FTX() { }
	
};


class PASCALIMPLEMENTATION TsgcWSAPI_ThreeCommas : public Sgcwebsocket_api_threecommas::TsgcWS_API_ThreeCommas
{
	typedef Sgcwebsocket_api_threecommas::TsgcWS_API_ThreeCommas inherited;
	
__published:
	__property OnConnect;
	__property OnThreeCommasConnect;
	__property OnThreeCommasConfirmSubscription;
	__property OnThreeCommasRejectSubscription;
	__property OnThreeCommasMessage;
	__property OnThreeCommasPing;
	__property OnThreeCommasHTTPException;
	__property OnDisconnect;
	__property Client;
	__property ThreeCommas;
	__property RawMessages;
	__property Version = {default=0};
public:
	/* TsgcWS_API_ThreeCommas.Create */ inline __fastcall virtual TsgcWSAPI_ThreeCommas(System::Classes::TComponent* aOwner) : Sgcwebsocket_api_threecommas::TsgcWS_API_ThreeCommas(aOwner) { }
	/* TsgcWS_API_ThreeCommas.Destroy */ inline __fastcall virtual ~TsgcWSAPI_ThreeCommas() { }
	
};


//-- var, const, procedure ---------------------------------------------------
}	/* namespace Sgcwebsocket_apis */
#if !defined(DELPHIHEADER_NO_IMPLICIT_NAMESPACE_USE) && !defined(NO_USING_NAMESPACE_SGCWEBSOCKET_APIS)
using namespace Sgcwebsocket_apis;
#endif
#pragma pack(pop)
#pragma option pop

#pragma delphiheader end.
//-- end unit ----------------------------------------------------------------
#endif	// Sgcwebsocket_apisHPP
