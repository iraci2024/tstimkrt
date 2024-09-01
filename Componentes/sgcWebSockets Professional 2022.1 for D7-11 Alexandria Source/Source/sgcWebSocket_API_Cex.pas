{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_API_Cex;

interface

{$I sgcVer.inc}
{$IFDEF SGC_APIS}

uses
  Classes, SysUtils,
  // sgc
  sgcWebSocket_Classes, sgcJSON;

Type
  TsgcWSCexConnectEvent = procedure(Sender: TObject) of object;
  TsgcWSCexErrorEvent = procedure(Sender: TObject; Error: String) of object;
  TsgcWSCexSubscribedEvent = procedure(Sender: TObject; Channel: String)
    of object;
  TsgcWSCexMessageEvent = procedure(Sender: TObject; Event, Msg: String)
    of object;
  TsgcWSCexAuthenticatedEvent = procedure(Sender: TObject) of object;
  TsgcWSCexDisconnectingEvent = procedure(Sender: TObject; Reason: String;
    Time: String) of object;
  TsgcWSCexDisconnectEvent = procedure(Sender: TObject) of object;

  TsgcWSCexPeriods = (cxp1Min, cxp3Min, cxp5Min, cxp15Min, cxp30Min, cxp1Hour,
    cxp2Hour, cxp4Hour, cxp6Hour, cxp12Hour, cxp1Day, cxp3Day, cxp1Week);
  TsgcWSCexOrderType = (cxoBuy, cxoSell);

  TsgcWSCex_Options = class(TPersistent)
  private
    FApiKey: String;
    FApiSecret: String;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property ApiKey: String read FApiKey write FApiKey;
    property ApiSecret: String read FApiSecret write FApiSecret;
  end;

  TsgcWS_API_Cex = class(TsgcWSAPI_client)
    { from TsgcWSAPI_Client }
  protected
    function GetURL: String; override;
    { from TsgcWSAPI_Client }

    { from TsgcWSComponent_Base }
  protected
    procedure DoEventMessage(aConnection: TsgcWSConnection;
      const Text: string); override;
    { from TsgcWSComponent_Base }

    { from TsgcWSAPI_client }
  protected
    function DoKeepAlive: Boolean; override;
    { from TsgcWSAPI_client }

    { helpers }
  private
    function GetPeriod(const aPeriod: TsgcWSCexPeriods): String;
    function GetOrderType(const aOrderType: TsgcWSCexOrderType): String;
    { helpers }

    { JSON }
  private
    FJSON: TsgcJSON;
    function GetJSON: TsgcJSON;
  protected
    property JSON: TsgcJSON read GetJSON write FJSON;
    { JSON }

    { properties }
  private
    FCex: TsgcWSCex_Options;
    procedure SetCex(const Value: TsgcWSCex_Options);
  public
    property Cex: TsgcWSCex_Options read FCex write SetCex;
    { properties }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { methods }
  private
    FRequestNum: Integer;
  private
    function GetTimeStamp: String;
  protected
    procedure DoReadEvent(const aEvent, aText: String); virtual;
  protected
    procedure DoWriteData(const aText: String); virtual;
    procedure DoPing; virtual;
  protected
    procedure DoAuthenticate;
  protected
    procedure DoSubscribe(const aRoom: String); virtual;
    procedure DoUnSubscribe(const aRoom: String); virtual;
  protected
    procedure DoRequest(const aEvent: String; const aSymbolPair1: String = ''; const
        aSymbolPair2: String = ''; const aOptions: String = ''); virtual;
  public
    procedure Ping;
    // ... public channels
  public
    procedure Authenticate;
  public
    procedure SubscribeTickers;
    procedure UnSubscribeTickers;
  public
    procedure SubscribePair(const aSymbolPair1, aSymbolPair2: String);
    procedure UnSubscribePair(const aSymbolPair1, aSymbolPair2: String);
  public
    procedure SubscribeChart(aPeriod: TsgcWSCexPeriods;
      const aSymbolPair1, aSymbolPair2: String);
    // ... private channels
  public
    procedure GetTicker(const aSymbolPair1, aSymbolPair2: String);
    procedure GetBalance;
    procedure SubscribeOrderBook(const aSymbolPair1, aSymbolPair2: String; aSubscribe:
        Boolean = True; const aDepth: Integer = -1);
    procedure UnSubscribeOrderBook(const aSymbolPair1, aSymbolPair2: String);
    procedure GetOpenOrders(const aSymbolPair1, aSymbolPair2: String);
    procedure PlaceOrder(const aSymbolPair1, aSymbolPair2: String; aAmount, aPrice:
        Currency; aType: TsgcWSCexOrderType);
    procedure CancelReplaceOrder(const aOrderId, aSymbolPair1, aSymbolPair2: String;
        aAmount, aPrice: Currency; aType: TsgcWSCexOrderType);
    procedure GetOrderRequest(const aOrderId: String);
    procedure CancelOrderRequest(const aOrderId: String);
    procedure GetArchivedOrders(const aSymbolPair1, aSymbolPair2: String; const aLimit:
        Integer = 10);
    procedure OpenPosition(const aSymbolPair1, aSymbolPair2, aSymbol: String;
        aAmount, aLeverage: Integer; aAnySlippage: Boolean; aEOPrice,
        aStopLossPrice: Currency);
    procedure GetPosition(const aId: String);
    procedure GetOpenPositions(const aSymbolPair1, aSymbolPair2: String);
    procedure ClosePosition(const aId, aSymbolPair1, aSymbolPair2: String);
    { methods }

    { events }
  private
    FOnCexAuthenticated: TsgcWSCexAuthenticatedEvent;
    FOnCexConnect: TsgcWSCexConnectEvent;
    FOnCexDisconnect: TsgcWSCexDisconnectEvent;
    FOnCexDisconnecting: TsgcWSCexDisconnectingEvent;
    FOnCexError: TsgcWSCexErrorEvent;
    FOnCexMessage: TsgcWSCexMessageEvent;
    FOnCexSubscribed: TsgcWSCexSubscribedEvent;
  protected
    procedure DoEventCexConnect; virtual;
    procedure DoEventCexSubscribed(const aChannel: String); virtual;
    procedure DoEventCexMessage(const aEvent, aMessage: String); virtual;
    procedure DoEventCexAuthenticated; virtual;
    procedure DoEventCexError(const aError: String); virtual;
    procedure DoEventCexDisconnecting(const aReason, aTime: String); virtual;
    procedure DoEventCexDisconnect; virtual;
  public
    property OnCexConnect: TsgcWSCexConnectEvent read FOnCexConnect
      write FOnCexConnect;
    property OnCexSubscribed: TsgcWSCexSubscribedEvent read FOnCexSubscribed
      write FOnCexSubscribed;
    property OnCexMessage: TsgcWSCexMessageEvent read FOnCexMessage
      write FOnCexMessage;
    property OnCexAuthenticated: TsgcWSCexAuthenticatedEvent
      read FOnCexAuthenticated write FOnCexAuthenticated;
    property OnCexError: TsgcWSCexErrorEvent read FOnCexError write FOnCexError;
    property OnCexDisconnect: TsgcWSCexDisconnectEvent read FOnCexDisconnect
      write FOnCexDisconnect;
    property OnCexDisconnecting: TsgcWSCexDisconnectingEvent
      read FOnCexDisconnecting write FOnCexDisconnecting;
    { events }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_APIS}

uses
  DateUtils,
  // sgc
  sgcBase_Helpers, sgcWebSocket_Helpers;

const
  CS_CLOSE_POSITION = 'close-position';
  CS_OPEN_POSITIONS = 'open-positions';
  CS_GET_POSITION = 'get-position';
  CS_ARCHIVED_ORDERS = 'archived-orders';
  CS_PLACE_ORDER = 'place-order';
  CS_CANCEL_REPLACE_ORDER = 'cancel-replace-order';
  CS_TICKER = 'ticker';
  CS_ORDER_BOOK_SUBSCRIBE = 'order-book-subscribe';
  CS_ORDER_BOOK_UNSUBSCRIBE = 'order-book-unsubscribe';
  CS_OPEN_ORDERS = 'open-orders';
  CS_GET_ORDER = 'get-order';
  CS_CANCEL_ORDER = 'cancel-order';
  CS_OPEN_POSITION = 'open-position';

constructor TsgcWS_API_Cex.Create(aOwner: TComponent);
begin
  inherited;
  FCex := TsgcWSCex_Options.Create;
  FRequestNum := 0;
end;

destructor TsgcWS_API_Cex.Destroy;
begin
  sgcFree(FCex);
  inherited;
end;

procedure TsgcWS_API_Cex.Authenticate;
begin
  DoAuthenticate;
end;

procedure TsgcWS_API_Cex.CancelOrderRequest(const aOrderId: String);
var
  vOptions: String;
begin
  vOptions := '"data": {"order_id": "' + aOrderId + '"},';

  DoRequest(CS_CANCEL_ORDER, '', '', vOptions);
end;

procedure TsgcWS_API_Cex.CancelReplaceOrder(const aOrderId, aSymbolPair1, aSymbolPair2:
    String; aAmount, aPrice: Currency; aType: TsgcWSCexOrderType);
var
  vOptions: String;
  vFS: TFormatSettings;
begin
  vFS.DecimalSeparator := '.';
  vFS.ThousandSeparator := ',';

  vOptions :=
    '"order_id": "' + aOrderId + '",' +
    '"amount": ' + FormatCurr('0.00', aAmount, vFS) + ',' +
    '"price": "' + FormatCurr('0.0000', aPrice, vFS) + '",' +
    '"type": "' + GetOrderType(aType) + '"';

  DoRequest(CS_CANCEL_REPLACE_ORDER, aSymbolPair1, aSymbolPair2, vOptions);
end;

procedure TsgcWS_API_Cex.ClosePosition(const aId, aSymbolPair1, aSymbolPair2:
    String);
begin
  DoRequest(CS_CLOSE_POSITION, aSymbolPair1, aSymbolPair2, '"id": "' + aId + '"');
end;

procedure TsgcWS_API_Cex.DoAuthenticate;
var
  vTimestamp: String;
begin
  vTimestamp := GetTimeStamp;

  DoWriteData(
    '{' +
      '"e": "auth", ' +
      '"auth": {' +
        '"key": "' + Cex.ApiKey + '",' +
        '"signature": "' + GetHMACSHA256(vTimestamp + Cex.ApiKey, Cex.ApiSecret) + '",' +
        '"timestamp": ' + vTimestamp +
      '}' +
    '}');
end;

procedure TsgcWS_API_Cex.DoEventCexAuthenticated;
begin
  if Assigned(FOnCexAuthenticated) then
    FOnCexAuthenticated(self);
end;

procedure TsgcWS_API_Cex.DoEventCexConnect;
begin
  FRequestNum := 0;

  if Assigned(FOnCexConnect) then
    FOnCexConnect(self);
end;

procedure TsgcWS_API_Cex.DoEventCexDisconnect;
begin
  if Assigned(FOnCexDisconnect) then
    FOnCexDisconnect(self);
end;

procedure TsgcWS_API_Cex.DoEventCexDisconnecting(const aReason, aTime: String);
begin
  if Assigned(FOnCexDisconnecting) then
    FOnCexDisconnecting(self, aReason, aTime);
end;

procedure TsgcWS_API_Cex.DoEventCexError(const aError: String);
begin
  if Assigned(FOnCexError) then
    FOnCexError(self, aError);
end;

procedure TsgcWS_API_Cex.DoEventCexMessage(const aEvent, aMessage: String);
begin
  if Assigned(FOnCexMessage) then
    FOnCexMessage(self, aEvent, aMessage);
end;

procedure TsgcWS_API_Cex.DoEventCexSubscribed(const aChannel: String);
begin
  if Assigned(FOnCexSubscribed) then
    FOnCexSubscribed(self, aChannel);
end;

procedure TsgcWS_API_Cex.DoEventMessage(aConnection: TsgcWSConnection;
  const Text: string);
begin
  if RawMessages then
    inherited
  else
  begin
    JSON.Clear;
    JSON.Read(Text);

    if JSON.Node['e'] <> nil then
      DoReadEvent(JSON.Node['e'].Value, Text)
    else
      inherited;
  end;
end;

function TsgcWS_API_Cex.DoKeepAlive: Boolean;
begin
  Result := True;

  DoPing;
end;

procedure TsgcWS_API_Cex.DoPing;
begin
  DoWriteData('{"e": "ping", "time": ' + GetTimeStamp + '}');
end;

procedure TsgcWS_API_Cex.DoReadEvent(const aEvent, aText: String);
begin
  if aEvent = 'ping' then
    DoWriteData('{"e": "pong"}')
  else if aEvent = 'auth' then
  begin
    if JSON.Node['ok'].Value = 'ok' then
      DoEventCexAuthenticated
    else
      DoEventCexError(JSON.Node['data'].Node['error'].Value)
  end
  else if aEvent = 'connected' then
    DoEventCexConnect
  else if aEvent = 'disconnected' then
    DoEventCexDisconnect
  else if aEvent = 'disconnecting' then
    DoEventCexDisconnecting(JSON.Node['reason'].Value, JSON.Node['time'].Value)
  else
    DoEventCexMessage(aEvent, aText);
end;

procedure TsgcWS_API_Cex.DoRequest(const aEvent: String; const aSymbolPair1: String
    = ''; const aSymbolPair2: String = ''; const aOptions: String = '');
var
  vJSON: String;
begin
  FRequestNum := FRequestNum + 1;

  vJSON :=
    '{' +
      '"e": "' + aEvent + '",';
  if (aSymbolPair1 <> '') and (aSymbolPair2 <> '') then
  begin
    if aEvent = CS_TICKER then
      vJSON := vJSON +
        '"data": [' +
          '"' + aSymbolPair1 + '",' +
          '"' + aSymbolPair2 + '"' +
        '],'
    else
    begin
      vJSON := vJSON +
        '"data": {' +
          '"pair": [' +
            '"' + aSymbolPair1 + '",' +
            '"' + aSymbolPair2 + '"' +
          ']';

      if aOptions <> '' then
        vJSON := vJSON + ',' + aOptions;

      vJSON := vJSON + '},';
    end;
  end
  else if aOptions <> '' then
    vJSON := vJSON + aOptions;

  vJSON := vJSON +
      '"oid": "' + GetTimeStamp + '_' + IntToStr(FRequestNum) + '_' + aEvent + '"' +
    '}';

  DoWriteData(vJSON);
end;

procedure TsgcWS_API_Cex.DoSubscribe(const aRoom: String);
begin
  DoWriteData(
    '{' +
      '"e": "subscribe",' +
      '"rooms": [' +
        '"' + aRoom + '"' +
      ']' +
    '}');
end;

procedure TsgcWS_API_Cex.DoUnSubscribe(const aRoom: String);
begin
  DoWriteData(
    '{' +
      '"e": "unsubscribe",' +
      '"rooms": [' + '"' +
        aRoom + '"' +
      ']' +
    '}');
end;

procedure TsgcWS_API_Cex.DoWriteData(const aText: String);
begin
  if Assigned(FClient) then
    FClient.WriteData(aText);
end;

procedure TsgcWS_API_Cex.GetArchivedOrders(const aSymbolPair1, aSymbolPair2: String;
    const aLimit: Integer = 10);
var
  vOptions: String;
begin
  vOptions := '"limit": ' + IntToStr(aLimit);

  DoRequest(CS_ARCHIVED_ORDERS, aSymbolPair1, aSymbolPair2, vOptions);
end;

procedure TsgcWS_API_Cex.GetBalance;
begin
  DoRequest('get-balance');
end;

function TsgcWS_API_Cex.GetJSON: TsgcJSON;
begin
  if not Assigned(FJSON) then
    FJSON := TsgcJSON.Create(self);
  Result := FJSON;
end;

procedure TsgcWS_API_Cex.GetOpenOrders(const aSymbolPair1, aSymbolPair2: String);
begin
  DoRequest(CS_OPEN_ORDERS, aSymbolPair1, aSymbolPair2);
end;

procedure TsgcWS_API_Cex.GetOrderRequest(const aOrderId: String);
var
  vOptions: String;
begin
  vOptions := '"data": {"order_id": "' + aOrderId + '"},';

  DoRequest(CS_GET_ORDER, '', '', vOptions);
end;

function TsgcWS_API_Cex.GetOrderType(const aOrderType: TsgcWSCexOrderType):
    String;
begin
  result := '';

  case aOrderType of
    cxoBuy: result := 'buy';
    cxoSell: result := 'sell';
  end;
end;

function TsgcWS_API_Cex.GetPeriod(const aPeriod: TsgcWSCexPeriods): String;
begin
  case aPeriod of
    cxp1Min:
      Result := '1m';
    cxp3Min:
      Result := '3m';
    cxp5Min:
      Result := '5m';
    cxp15Min:
      Result := '15m';
    cxp30Min:
      Result := '30m';
    cxp1Hour:
      Result := '1h';
    cxp2Hour:
      Result := '2h';
    cxp4Hour:
      Result := '4h';
    cxp6Hour:
      Result := '6h';
    cxp12Hour:
      Result := '12h';
    cxp1Day:
      Result := '1d';
    cxp3Day:
      Result := '3d';
    cxp1Week:
      Result := '1w';
  else
    Result := '';
  end;
end;

procedure TsgcWS_API_Cex.GetPosition(const aId: String);
begin
  DoRequest(CS_GET_POSITION, '', '', '"data": {"id": ' + aId + '},');
end;

procedure TsgcWS_API_Cex.GetTicker(const aSymbolPair1, aSymbolPair2: String);
begin
  DoRequest(CS_TICKER, aSymbolPair1, aSymbolPair2);
end;

function TsgcWS_API_Cex.GetTimeStamp: String;
begin
  {$IFDEF LAZARUS}
  Result := IntToStr(DateTimeToUnix(LocalTimeToUniversal(Now)));
  {$ELSE}
  Result := IntToStr(DateTimeToUnix(Now{$IFDEF DXE6}, False{$ENDIF}));
  {$ENDIF}
end;

function TsgcWS_API_Cex.GetURL: String;
begin
  Result := 'wss://ws.cex.io/ws';
end;

procedure TsgcWS_API_Cex.OpenPosition(const aSymbolPair1, aSymbolPair2,
    aSymbol: String; aAmount, aLeverage: Integer; aAnySlippage: Boolean;
    aEOPrice, aStopLossPrice: Currency);
var
  vOptions: String;
  vFS: TFormatSettings;
  vAnySlippage: String;
begin
  vFS.DecimalSeparator := '.';
  vFS.ThousandSeparator := ',';

  if aAnySlippage then
    vAnySlippage := 'true'
  else
    vAnySlippage := 'false';

  vOptions :=
    '"amount": "' + IntToStr(aAmount) + '",' +
    '"symbol": "' + aSymbol + '",' +
    '"leverage": "' + IntToStr(aLeverage) + '",' +
    '"ptype": "long",' +
    '"anySlippage": "' + vAnySlippage + '",' +
    '"eoprice": "' + FormatCurr('0.0000', aEOPrice, vFS) + '",' +
    '"stopLossPrice": "' + FormatCurr('0.0000', aStopLossPrice, vFS) + '"';

  DoRequest(CS_OPEN_POSITION, aSymbolPair1, aSymbolPair2, vOptions);
end;

procedure TsgcWS_API_Cex.GetOpenPositions(const aSymbolPair1, aSymbolPair2:
    String);
begin
  DoRequest(CS_OPEN_POSITIONS, aSymbolPair1, aSymbolPair2);
end;

procedure TsgcWS_API_Cex.Ping;
begin
  DoPing;
end;

procedure TsgcWS_API_Cex.PlaceOrder(const aSymbolPair1, aSymbolPair2: String; aAmount,
    aPrice: Currency; aType: TsgcWSCexOrderType);
var
  vOptions: String;
  vFS: TFormatSettings;
begin
  vFS.DecimalSeparator := '.';
  vFS.ThousandSeparator := ',';

  vOptions :=
    '"amount": ' + FormatCurr('0.00', aAmount, vFS) + ',' +
    '"price": "' + FormatCurr('0.0000', aPrice, vFS) + '",' +
    '"type": "' + GetOrderType(aType) + '"';

  DoRequest(CS_PLACE_ORDER, aSymbolPair1, aSymbolPair2, vOptions);
end;

procedure TsgcWS_API_Cex.SetCex(const Value: TsgcWSCex_Options);
begin
  FCex.Assign(Value);
end;

procedure TsgcWS_API_Cex.SubscribeChart(aPeriod: TsgcWSCexPeriods;
  const aSymbolPair1, aSymbolPair2: String);
begin
  DoWriteData(
    '{' +
      '"e": "init-ohlcv",' +
      '"i": "' + GetPeriod(aPeriod) + '",' +
      '"rooms": [' +
        '"pair-' + aSymbolPair1 + '-' + aSymbolPair2 + '"' +
      ']' +
    '}');
end;

procedure TsgcWS_API_Cex.SubscribeOrderBook(const aSymbolPair1, aSymbolPair2: String;
    aSubscribe: Boolean = True; const aDepth: Integer = -1);
var
  vOptions: String;
begin
  vOptions := '"subscribe": ';
  if aSubscribe then
    vOptions := vOptions + 'true'
  else
    vOptions := vOptions + 'false';
  vOptions := vOptions + ', "depth": ' + IntToStr(aDepth);

  DoRequest(CS_ORDER_BOOK_SUBSCRIBE, aSymbolPair1, aSymbolPair2, vOptions);
end;

procedure TsgcWS_API_Cex.SubscribePair(const aSymbolPair1, aSymbolPair2: String);
begin
  DoSubscribe('pair-' + aSymbolPair1 + '-' + aSymbolPair2);
end;

procedure TsgcWS_API_Cex.SubscribeTickers;
begin
  DoSubscribe('tickers');
end;

procedure TsgcWS_API_Cex.UnSubscribeOrderBook(const aSymbolPair1, aSymbolPair2: String);
begin
  DoRequest(CS_ORDER_BOOK_UNSUBSCRIBE, aSymbolPair1, aSymbolPair2);
end;

procedure TsgcWS_API_Cex.UnSubscribePair(const aSymbolPair1, aSymbolPair2: String);
begin
  DoUnSubscribe('pair-' + aSymbolPair1 + '-' + aSymbolPair2);
end;

procedure TsgcWS_API_Cex.UnSubscribeTickers;
begin
  DoUnSubscribe('tickers');
end;

procedure TsgcWSCex_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSCex_Options then
  begin
    ApiKey := TsgcWSCex_Options(aSource).ApiKey;
    ApiSecret := TsgcWSCex_Options(aSource).ApiSecret;
  end
  else
    inherited Assign(aSource);
end;

{$ENDIF}

end.
