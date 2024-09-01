{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_API_Bitstamp;

interface

{$I sgcVer.inc}
{$IFDEF SGC_APIS}

uses
  Classes, SysUtils,
  // sgc
  sgcWebSocket_API_Pusher,
  // v2
  sgcWebSocket_Classes;

Type

  TsgcWS_API_BitstampV1 = class(TsgcWS_API_Pusher)
  public
    constructor Create(aOwner: TComponent); override;
  public
    procedure SubscribeTicker(const aCurrency: String = '');
    procedure UnSubscribeTicker(const aCurrency: String = '');
  public
    procedure SubscribeOrderBook(const aCurrency: String = '');
    procedure UnSubscribeOrderBook(const aCurrency: String = '');
  public
    procedure SubscribeFullOrderBook(const aCurrency: String = '');
    procedure UnSubscribeFullOrderBook(const aCurrency: String = '');
  public
    procedure SubscribeOrders(const aCurrency: String = '');
    procedure UnSubscribeOrders(const aCurrency: String = '');
  end;

  TsgcWS_API_Bitstamp = class(TsgcWSAPI_client)
    { from TsgcWSAPI_Client }
  protected
    function GetURL: String; override;
    { from TsgcWSAPI_Client }

    { internal methods }
  protected
    procedure DoWriteData(const aText: string); virtual;
  protected
    procedure DoSendBitstampMessage(const aEvent, aChannel: String); virtual;
  protected
    procedure DoSubscribe(const aChannel: string); virtual;
    procedure DoUnSubscribe(const aChannel: string); virtual;
    { internal methods }

    { public methods }
  public
    procedure SubscribeLiveTicker(const aCurrencyPair: string);
    procedure UnSubscribeLiveTicker(const aCurrencyPair: string);
  public
    procedure SubscribeLiveOrders(const aCurrencyPair: string);
    procedure UnSubscribeLiveOrders(const aCurrencyPair: string);
  public
    procedure SubscribeLiveOrderBook(const aCurrencyPair: string);
    procedure UnSubscribeLiveOrderBook(const aCurrencyPair: string);
  public
    procedure SubscribeLiveDetailOrderBook(const aCurrencyPair: string);
    procedure UnSubscribeLiveDetailOrderBook(const aCurrencyPair: string);
  public
    procedure SubscribeLiveFullOrderBook(const aCurrencyPair: string);
    procedure UnSubscribeLiveFullOrderBook(const aCurrencyPair: string);
    { public methods }
  end;

{$ENDIF}

implementation

uses
  sgcWebSocket_Const;

const
  CS_BITSTAMP_V1_TICKER = 'live_trades';
  CS_BITSTAMP_V1_ORDERS = 'live_orders';
  CS_BITSTAMP_V1_ORDERBOOK = 'order_book';
  CS_BITSTAMP_V1_FULLORDERBOOK = 'diff_order_book';
  CS_BITSTAMP_V1_KEY = 'de504dc5763aeef9ff52';

const
  CS_BITSTAMP_V2_SUBSCRIBE = 'bts:subscribe';
  CS_BITSTAMP_V2_UNSUBSCRIBE = 'bts:unsubscribe';
const
  CS_CHANNEL_V2_LIVE_TICKER = 'live_trades_';
  CS_CHANNEL_V2_LIVE_ORDERS = 'live_orders_';
  CS_CHANNEL_V2_LIVE_ORDER_BOOK = 'order_book_';
  CS_CHANNEL_V2_LIVE_DETAIL_ORDER_BOOK = 'detail_order_book_';
  CS_CHANNEL_V2_LIVE_FULL_ORDER_BOOK = 'diff_order_book_';



{$IFDEF SGC_APIS}

constructor TsgcWS_API_BitstampV1.Create(aOwner: TComponent);
begin
  inherited;
  Pusher.Key := CS_BITSTAMP_V1_KEY;
end;

procedure TsgcWS_API_BitstampV1.SubscribeFullOrderBook(const aCurrency: String =
    '');
begin
  if aCurrency <> '' then
    Subscribe(CS_BITSTAMP_V1_FULLORDERBOOK + '_' + aCurrency)
  else
    Subscribe(CS_BITSTAMP_V1_FULLORDERBOOK);
end;

procedure TsgcWS_API_BitstampV1.UnSubscribeFullOrderBook(const aCurrency: String
    = '');
begin
  if aCurrency <> '' then
    UnSubscribe(CS_BITSTAMP_V1_FULLORDERBOOK + '_' + aCurrency)
  else
    UnSubscribe(CS_BITSTAMP_V1_FULLORDERBOOK);
end;

procedure TsgcWS_API_BitstampV1.SubscribeOrderBook(const aCurrency: String = '');
begin
  if aCurrency <> '' then
    Subscribe(CS_BITSTAMP_V1_ORDERBOOK + '_' + aCurrency)
  else
    Subscribe(CS_BITSTAMP_V1_ORDERBOOK);
end;

procedure TsgcWS_API_BitstampV1.UnSubscribeOrderBook(const aCurrency: String =
    '');
begin
  if aCurrency <> '' then
    UnSubscribe(CS_BITSTAMP_V1_ORDERBOOK + '_' + aCurrency)
  else
    UnSubscribe(CS_BITSTAMP_V1_ORDERBOOK);
end;

procedure TsgcWS_API_BitstampV1.SubscribeOrders(const aCurrency: String = '');
begin
  if aCurrency <> '' then
    Subscribe(CS_BITSTAMP_V1_ORDERS + '_' + aCurrency)
  else
    Subscribe(CS_BITSTAMP_V1_ORDERS);
end;

procedure TsgcWS_API_BitstampV1.UnSubscribeOrders(const aCurrency: String = '');
begin
  if aCurrency <> '' then
    UnSubscribe(CS_BITSTAMP_V1_ORDERS + '_' + aCurrency)
  else
    UnSubscribe(CS_BITSTAMP_V1_ORDERS);
end;

procedure TsgcWS_API_BitstampV1.SubscribeTicker(const aCurrency: String = '');
begin
  if aCurrency <> '' then
    Subscribe(CS_BITSTAMP_V1_TICKER + '_' + aCurrency)
  else
    Subscribe(CS_BITSTAMP_V1_TICKER);
end;

procedure TsgcWS_API_BitstampV1.UnSubscribeTicker(const aCurrency: String = '');
begin
  if aCurrency <> '' then
    UnSubscribe(CS_BITSTAMP_V1_TICKER + '_' + aCurrency)
  else
    UnSubscribe(CS_BITSTAMP_V1_TICKER);
end;

procedure TsgcWS_API_Bitstamp.DoSendBitstampMessage(const aEvent, aChannel:
    String);
begin
  DoWriteData(
    Format(
      '{' +
        '"event": "%s",' +
        '"data": {' +
          '"channel": "%s"' +
        '}' +
      '}', [aEvent, aChannel])
    );
end;

procedure TsgcWS_API_Bitstamp.DoSubscribe(const aChannel: string);
begin
  DoSendBitstampMessage(CS_BITSTAMP_V2_SUBSCRIBE, aChannel);
end;

procedure TsgcWS_API_Bitstamp.DoUnSubscribe(const aChannel: string);
begin
  DoSendBitstampMessage(CS_BITSTAMP_V2_UNSUBSCRIBE, aChannel);
end;

procedure TsgcWS_API_Bitstamp.DoWriteData(const aText: string);
begin
  if Assigned(FClient) then
    FClient.WriteData(aText);
end;

function TsgcWS_API_Bitstamp.GetURL: String;
begin
  Result := 'wss://ws.bitstamp.net';
end;

procedure TsgcWS_API_Bitstamp.SubscribeLiveDetailOrderBook(const aCurrencyPair:
    string);
begin
  DoSubscribe(CS_CHANNEL_V2_LIVE_DETAIL_ORDER_BOOK + aCurrencyPair)
end;

procedure TsgcWS_API_Bitstamp.SubscribeLiveFullOrderBook(const aCurrencyPair:
    string);
begin
  DoSubscribe(CS_CHANNEL_V2_LIVE_FULL_ORDER_BOOK + aCurrencyPair)
end;

procedure TsgcWS_API_Bitstamp.SubscribeLiveOrderBook(const aCurrencyPair:
    string);
begin
  DoSubscribe(CS_CHANNEL_V2_LIVE_ORDER_BOOK + aCurrencyPair)
end;

procedure TsgcWS_API_Bitstamp.SubscribeLiveOrders(const aCurrencyPair: string);
begin
  DoSubscribe(CS_CHANNEL_V2_LIVE_ORDERS + aCurrencyPair)
end;

procedure TsgcWS_API_Bitstamp.SubscribeLiveTicker(const aCurrencyPair: string);
begin
  DoSubscribe(CS_CHANNEL_V2_LIVE_TICKER + aCurrencyPair);
end;

procedure TsgcWS_API_Bitstamp.UnSubscribeLiveDetailOrderBook(const
    aCurrencyPair: string);
begin
  DoUnSubscribe(CS_CHANNEL_V2_LIVE_DETAIL_ORDER_BOOK + aCurrencyPair)
end;

procedure TsgcWS_API_Bitstamp.UnSubscribeLiveFullOrderBook(const aCurrencyPair:
    string);
begin
  DoUnSubscribe(CS_CHANNEL_V2_LIVE_FULL_ORDER_BOOK + aCurrencyPair)
end;

procedure TsgcWS_API_Bitstamp.UnSubscribeLiveOrderBook(const aCurrencyPair:
    string);
begin
  DoUnSubscribe(CS_CHANNEL_V2_LIVE_ORDER_BOOK + aCurrencyPair)
end;

procedure TsgcWS_API_Bitstamp.UnSubscribeLiveOrders(const aCurrencyPair:
    string);
begin
  DouNSubscribe(CS_CHANNEL_V2_LIVE_ORDERS + aCurrencyPair)
end;

procedure TsgcWS_API_Bitstamp.UnSubscribeLiveTicker(const aCurrencyPair:
    string);
begin
  DoUnSubscribe(CS_CHANNEL_V2_LIVE_TICKER + aCurrencyPair);
end;

{$ENDIF}

end.
