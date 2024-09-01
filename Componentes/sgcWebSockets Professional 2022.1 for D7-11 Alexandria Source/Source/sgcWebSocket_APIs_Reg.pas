{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcWebSocket_APIs_Reg;

interface

{$I sgcVer.inc}

{$IFDEF SGC_APIS}

uses
  Classes,
  {$IFDEF LAZARUS}
  PropEdits, ComponentEditors,
  {$ELSE}
  {$IFNDEF APPMETHOD}
  {$IFDEF SGC_DESIGN_PACKAGE}
  DesignEditors, DesignIntf,
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  sgcWebSocket, sgcLibs,
  sgcWebSocket_APIs
  {$IFDEF SGC_EDT_PRO}
  , sgcWebSocket_Server_APIs
  {$ENDIF};

  procedure Register;

{$ENDIF}

implementation

{$IFDEF SGC_APIS}

procedure Register;
begin
  Classes.RegisterComponents('SGC WebSockets APIs',
    [
      TsgcWSAPI_Bitfinex,
      TsgcWSAPI_Blockchain,
      TsgcWSAPI_Pusher,
      TsgcWSAPI_SignalR,
      TsgcWSAPI_Bittrex,
      TsgcWSAPI_Binance,
      TsgcWSAPI_Binance_Futures,
      TsgcWSAPI_SocketIO,
      TsgcWSAPI_Bitstamp,
      TsgcWSAPI_Huobi,
      TsgcWSAPI_Cex,
      TsgcWSAPI_Bitmex,
      TsgcWSAPI_SignalRCore,
      TsgcWSAPI_FXCM,
      TsgcWSAPI_Kraken,
      TsgcWSAPI_Kraken_Futures,
      TsgcWSAPI_Coinbase,
      TsgcWSAPI_FTX,
      {$IFDEF SGC_TELEGRAM}
      TsgcTDLib_Telegram,
      {$ENDIF}
      {$IFDEF SGC_EDT_PRO}
      TsgcHTTP_Cryptohopper,
      TsgcWSAPIServer_RTCMultiConnection,
      {$ENDIF}
      TsgcWSAPI_Discord,
      TsgcLib_RCON,
      TsgcWSAPI_ThreeCommas
      {$IFDEF SGC_KUCOIN}
      ,TsgcWSAPI_Kucoin
      ,TsgcWSAPI_Kucoin_Futures
      {$ENDIF}
    ]
  );
end;

{$ENDIF}

end.
