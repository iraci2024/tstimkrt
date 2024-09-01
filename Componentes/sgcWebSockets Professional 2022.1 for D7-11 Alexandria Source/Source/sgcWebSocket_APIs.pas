{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }
unit sgcWebSocket_APIs;

interface

{$I sgcVer.inc}
{$IFDEF SGC_APIS}

uses
  Classes,
  sgcWebSocket_API_Bitfinex, sgcWebSocket_API_Blockchain,
  sgcWebSocket_API_Pusher, sgcWebSocket_API_SignalR,
  sgcWebSocket_API_Bittrex, sgcWebSocket_API_Binance,
  sgcWebSocket_API_SocketIO, sgcWebSocket_API_Bitstamp,
  sgcWebSocket_API_Huobi, sgcWebSocket_API_Cex,
  sgcWebSocket_API_Bitmex, sgcWebSocket_API_SignalRCore,
  sgcWebSocket_API_FXCM, sgcWebSocket_API_Kraken,
  sgcWebSocket_API_Coinbase, sgcWebSocket_API_FTX,
  sgcWebSocket_API_Discord, sgcWebSocket_API_ThreeCommas
  {$IFDEF SGC_KUCOIN}
  , sgcWebSocket_API_Kucoin
  {$ENDIF}
  ;

type
  TsgcWSAPI_Bitfinex = class(TsgcWS_API_Bitfinex)
  published
    property OnConnect;
    property OnDisconnect;
    property OnMessage;
    // property OnBinary;
    // property OnFragmented;
    property OnError;
    property OnException;

    property OnBitfinexConnect;
    property OnBitfinexAuth;
    property OnBitfinexUnauth;
    property OnBitfinexInfoMsg;
    property OnBitfinexSubscribed;
    property OnBitfinexUnSubscribed;
    property OnBitfinexUpdate;
    property OnBitfinexError;

    property Client;

    property RawMessages;

    property Version;
  end;

  TsgcWSAPI_Blockchain = class(TsgcWS_API_Blockchain)
  published
    property OnConnect;
    property OnDisconnect;
    property OnMessage;
    // property OnBinary;
    // property OnFragmented;
    property OnError;
    property OnException;

    property OnBlockchainNewBlock;
    property OnBlockchainNewTransaction;
    property OnBlockchainPong;

    property Client;

    property RawMessages;

    property Version;
  end;

  TsgcWSAPI_Pusher = class(TsgcWS_API_Pusher)
  published
    property OnConnect;
    property OnDisconnect;
    property OnMessage;
    // property OnBinary;
    // property OnFragmented;
    property OnError;
    property OnException;

    property OnPusherConnect;
    property OnPusherError;
    property OnPusherEvent;
    property OnPusherSubscribe;
    property OnPusherAuthentication;
    property Client;

    property Pusher;

    property RawMessages;

    property Version;
  end;

  TsgcWSAPI_SignalR = class(TsgcWS_API_SignalR)
  published
    property OnSignalRConnect;
    property OnSignalRMessage;
    property OnSignalRKeepAlive;
    property OnSignalRResult;
    property OnSignalRError;
    property OnSignalRDisconnect;

    property Client;

    property SignalR;

    property RawMessages;

    property Version;
  end;

  TsgcWSAPI_SignalRCore = class(TsgcWS_API_SignalRCore)
  published
    property OnSignalRCoreBeforeConnect;
    property OnSignalRCoreConnect;
    property OnSignalRCoreInvocation;
    property OnSignalRCoreStreamInvocation;
    property OnSignalRCoreStreamItem;
    property OnSignalRCoreCompletion;
    property OnSignalRCoreCancelInvocation;
    property OnSignalRCoreKeepAlive;
    property OnSignalRCoreError;
    property OnSignalRCoreClose;
    property OnSignalRCoreDisconnect;

    property Client;

    property SignalRCore;

    property RawMessages;

    property Version;
  end;

  TsgcWSAPI_Bittrex = class(TsgcWS_API_Bittrex)
  published
    property OnBittrexConnect;
    property OnBittrexSubscribed;
    property OnBittrexUnSubscribed;
    property OnBittrexMessage;
    property OnBittrexAuthenticated;
    property OnBittrexHeartBeat;
    property OnBittrexError;
    property OnBittrexDisconnect;

    property Client;

    property SignalR;
    property Bittrex;

    property RawMessages;

    property Version;
  end;

  TsgcWSAPI_Binance = class(TsgcWS_API_Binance)
  published
    property OnBinanceHTTPException;

    property Client;

    property Binance;

    // property RawMessages;

    property Version;
  end;

  TsgcWSAPI_Binance_Futures = class(TsgcWS_API_Binance_Futures)
  published
    property OnBinanceHTTPException;

    property Client;

    property Binance;
    property FuturesContracts;

    // property RawMessages;

    property Version;
  end;

  TsgcWSAPI_SocketIO = class(TsgcWS_API_SocketIO)
  published
    property Client;

    property SocketIO;

    // property RawMessages;

    property OnHTTPConnectionSSL;
    property OnHTTPRequest;
    property OnAfterConnect;

    property Version;
  end;

  TsgcWSAPI_Bitstamp = class(TsgcWS_API_Bitstamp)
  published
    property OnConnect;
    property OnDisconnect;
    property OnMessage;
    // property OnBinary;
    // property OnFragmented;
    property OnError;
    property OnException;

    property Client;

    property RawMessages;

    property Version;
  end;

  TsgcWSAPI_Huobi = class(TsgcWS_API_Huobi)
  published
    property OnConnect;
    property OnDisconnect;
    // property OnMessage;
    // property OnBinary;
    // property OnFragmented;
    property OnError;
    property OnException;

    property OnHuobiSubscribed;
    property OnHuobiUnSubscribed;
    property OnHuobiUpdate;
    property OnHuobiError;

    property Client;

    property RawMessages;

    property Version;
  end;

  TsgcWSAPI_Cex = class(TsgcWS_API_Cex)
  published
    property OnConnect;
    property OnCexConnect;
    property OnCexSubscribed;
    property OnCexMessage;
    property OnCexAuthenticated;
    property OnCexError;
    property OnCexDisconnecting;
    property OnCexDisconnect;
    property OnDisconnect;

    property Client;

    property Cex;

    property RawMessages;

    property Version;
  end;

  TsgcWSAPI_Bitmex = class(TsgcWS_API_Bitmex)
  published
    property OnConnect;
    property OnBitmexConnect;
    property OnBitmexSubscribed;
    property OnBitmexUnsubscribed;
    property OnBitmexMessage;
    property OnBitmexAuthenticated;
    property OnBitmexError;
    property OnDisconnect;

    property OnBitmexHTTPException;

    property Client;

    property Bitmex;

    property RawMessages;

    property Version;
  end;

  TsgcWSAPI_FXCM = class(TsgcWS_API_FXCM)
  published
    property OnConnect;
    property OnDisconnect;
    property OnMessage;
    // property OnBinary;
    // property OnFragmented;
    property OnError;
    property OnException;

    property Client;

    property SocketIO;
    property FXCM;

    property OnHTTPConnectionSSL;

    property RawMessages;

    property Version;
  end;

  TsgcWSAPI_Kraken = class(TsgcWS_API_Kraken)
  published
    property OnConnect;
    property OnKrakenConnect;
    property OnKrakenSystemStatus;
    property OnKrakenSubscribed;
    property OnKrakenUnSubscribed;
    property OnKrakenSubscriptionError;
    property OnKrakenData;
    property OnDisconnect;

    property OnKrakenHTTPException;

    property Client;

    property Kraken;

    property RawMessages;

    property Version;
  end;

  TsgcWSAPI_Kraken_Futures = class(TsgcWS_API_Kraken_Futures)
  published
    property OnConnect;
    property OnKrakenFuturesConnect;
    property OnKrakenFuturesSubscribed;
    property OnKrakenFuturesUnSubscribed;
    property OnKrakenFuturesError;
    property OnKrakenData;
    property OnDisconnect;

    property OnKrakenHTTPException;

    property Client;

    property Kraken;

    property RawMessages;

    property Version;
  end;

  TsgcWSAPI_Discord = class(TsgcWS_API_Discord)
  published
    property OnConnect;
    property OnDiscordBeforeReconnect;
    property OnDiscordReady;
    property OnDiscordResumed;
    property OnDiscordDispatch;
    property OnDiscordEvent;
    property OnDisconnect;

    property Client;

    property DiscordOptions;

    property RawMessages;

    property Version;
  end;

  TsgcWSAPI_Coinbase = class(TsgcWS_API_Coinbase)
  published
    property OnConnect;
    property OnCoinbaseSubscriptions;
    property OnCoinbaseMessage;
    property OnCoinbaseError;
    property OnCoinbaseHTTPException;
    property OnDisconnect;

    property Client;
    property Coinbase;

    property RawMessages;

    property Version;
  end;

  TsgcWSAPI_FTX = class(TsgcWS_API_FTX)
  published
    property OnConnect;
    property OnFTXSubscribed;
    property OnFTXUnsubscribed;
    property OnFTXMessage;
    property OnFTXError;
    property OnFTXHTTPException;
    property OnDisconnect;

    property Client;
    property FTX;

    property RawMessages;

    property Version;
  end;


  TsgcWSAPI_ThreeCommas = class(TsgcWS_API_ThreeCommas)
  published
    property OnConnect;
    property OnThreeCommasConnect;
    property OnThreeCommasConfirmSubscription;
    property OnThreeCommasRejectSubscription;
    property OnThreeCommasMessage;
    property OnThreeCommasPing;
    property OnThreeCommasHTTPException;
    property OnDisconnect;

    property Client;
    property ThreeCommas;

    property RawMessages;

    property Version;
  end;

  {$IFDEF SGC_KUCOIN}
  TsgcWSAPI_Kucoin = class(TsgcWS_API_Kucoin)
  published
    property OnKucoinHTTPException;

    property Client;

    property Kucoin;

    // property RawMessages;

    property Version;
  end;

  TsgcWSAPI_Kucoin_Futures = class(TsgcWS_API_Kucoin_Futures)
  published
    property OnKucoinHTTPException;

    property Client;

    property Kucoin;

    // property RawMessages;

    property Version;
  end;
  {$ENDIF}

{$ENDIF}


implementation

end.
