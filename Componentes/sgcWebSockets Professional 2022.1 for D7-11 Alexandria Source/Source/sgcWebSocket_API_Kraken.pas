{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_API_Kraken;

interface

{$I sgcVer.inc}
{$IFDEF SGC_APIS}

uses
  Classes, SysUtils, StrUtils,
  // sgc
  sgcWebSocket_Classes, sgcJSON, sgcHTTP_API_Kraken;

type
  TsgcWSKrakenHTTPExceptionEvent = procedure(Sender: TObject; E: Exception)
    of object;

  TsgcWSKrakenInterval = (kin1min, kin5min, kin15min, kin30min, kin60min,
    kin240min, kin1440min, kin10080min, kin21600min);
  TsgcWSKrakenDepth = (kde10, kde25, kde100, kde500, kde1000);
  TsgcWSKrakenOrderType = (kotNone, kotMarket, kotLimit, kotStopLoss,
    kotTakeProfit, kotStopLossProfit, kotStopLossProfitLimit, kotStopLossLimit,
    kotTakeProfitLimit, kotTrailingStop, kotTrailingStopLimit,
    kotStopLossAndLimit);
  TsgcWSKrakenOrderSide = (kosNone, kosBuy, kosSell);

  TsgcWSKrakenConnectEvent = procedure(Sender: TObject; ConnectionId: String;
    Status: String; Version: String) of object;
  TsgcWSKrakenSystemStatusEvent = procedure(Sender: TObject;
    ConnectionId: String; Status: String; Version: String) of object;
  TsgcWSKrakenSubscribedEvent = procedure(Sender: TObject; ChannelId: Integer;
    Pair: String; Subscription: String; ChannelName: String; ReqID: Integer)
    of object;
  TsgcWSKrakenUnSubscribedEvent = procedure(Sender: TObject; ChannelId: Integer;
    Pair, Subscription: String; ReqID: Integer) of object;
  TsgcWSKrakenSubscriptionError = procedure(Sender: TObject;
    ErrorMessage: String; Pair: String; Subscription: String; ReqID: Integer)
    of object;
  TsgcWSKrakenDataEvent = procedure(Sender: TObject; aData: String) of object;

  // futures
  TsgcWSKrakenFuturesConnectEvent = procedure(Sender: TObject; Version: String)
    of object;
  TsgcWSKrakenFuturesSubscribedEvent = procedure(Sender: TObject;
    Feed, ProductId: String) of object;
  TsgcWSKrakenFuturesUnSubscribedEvent = procedure(Sender: TObject;
    Feed, ProductId: String) of object;
  TsgcWSKrakenFuturesErrorEvent = procedure(Sender: TObject; Error: String)
    of object;

type
  TsgcWSKrakenOrder = class
  private
    FClose_OrderType: TsgcWSKrakenOrderType;
    FClose_Price: Currency;
    FClose_Price2: Currency;
    FExpireTm: String;
    FLeverage: String;
    FOFlags: String;
    FOrderType: TsgcWSKrakenOrderType;
    FPair: String;
    FPrice: Currency;
    FPrice2: Currency;
    FStartTm: String;
    FTradingAgreement: String;
    FUserRef: String;
    FValidate: Boolean;
    FVolume: Currency;
    F_Type: TsgcWSKrakenOrderSide;
  public
    constructor Create; virtual;
  public
    property Pair: String read FPair write FPair;
    property _Type: TsgcWSKrakenOrderSide read F_Type write F_Type;
    property OrderType: TsgcWSKrakenOrderType read FOrderType write FOrderType;
    property Price: Currency read FPrice write FPrice;
    property Price2: Currency read FPrice2 write FPrice2;
    property Volume: Currency read FVolume write FVolume;
    property Leverage: String read FLeverage write FLeverage;
    property OFlags: String read FOFlags write FOFlags;
    property StartTm: String read FStartTm write FStartTm;
    property ExpireTm: String read FExpireTm write FExpireTm;
    property UserRef: String read FUserRef write FUserRef;
    property Validate: Boolean read FValidate write FValidate;
    property Close_OrderType: TsgcWSKrakenOrderType read FClose_OrderType
      write FClose_OrderType;
    property Close_Price: Currency read FClose_Price write FClose_Price;
    property Close_Price2: Currency read FClose_Price2 write FClose_Price2;
    property TradingAgreement: String read FTradingAgreement
      write FTradingAgreement;
  end;

  TsgcWSKrakenLog_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FFileName: String;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property FileName: String read FFileName write FFileName;
  end;

  TsgcWSKraken_Options_Base = class(TPersistent)
  private
    FApiKey: String;
    FApiSecret: String;
    FHTTPLogOptions: TsgcWSKrakenLog_Options;
    procedure SetHTTPLogOptions(const Value: TsgcWSKrakenLog_Options);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property ApiKey: String read FApiKey write FApiKey;
    property ApiSecret: String read FApiSecret write FApiSecret;
    property HTTPLogOptions: TsgcWSKrakenLog_Options read FHTTPLogOptions
      write SetHTTPLogOptions;
  end;

  TsgcWSKraken_Options = class(TsgcWSKraken_Options_Base)
  private
    FBeta: Boolean;
  public
    constructor Create; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Beta: Boolean read FBeta write FBeta;
  end;

  TsgcWSKrakenFutures_Options = class(TsgcWSKraken_Options_Base)
  private
    FDemo: Boolean;
  public
    constructor Create; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Demo: Boolean read FDemo write FDemo;
  end;

  TsgcWS_API_Kraken_Base = class(TsgcWSAPI_client)
    { currency }
  private
    FFormatCurrency: String;
    function GetCurrencyString(aValue: Currency): String;
  public
    property FormatCurrency: String read FFormatCurrency write FFormatCurrency;
    { currency }

    { from TsgcWSComponent_Base }
  protected
    procedure DoEventMessage(aConnection: TsgcWSConnection;
      const Text: string); override;
    { from TsgcWSComponent_Base }

    { from TsgcWSAPI_client }
  protected
    function DoKeepAlive: Boolean; override;
    { from TsgcWSAPI_client }

    { JSON }
  private
    FJSON: TsgcJSON;
    function GetJSON: TsgcJSON;
  protected
    property JSON: TsgcJSON read GetJSON write FJSON;
    { JSON }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { public methods }
  protected
    procedure DoReadEvent(const aEvent, aText: String); virtual; abstract;
  protected
    procedure DoWriteData(const aText: String); virtual;
    procedure DoPing; virtual;
  public
    procedure Ping;
  private
    function GetInterval(aInterval: TsgcWSKrakenInterval): Integer;
    function GetDepth(aDepth: TsgcWSKrakenDepth): Integer;
    { public methods }

    { private methods }
  private
    function GetOrderType(aOrderType: TsgcWSKrakenOrderType): String;
    function GetOrderSide(aOrderSide: TsgcWSKrakenOrderSide): String;
    { private methods }

    { http events }
  private
    FOnKrakenHTTPException: TsgcWSKrakenHTTPExceptionEvent;
  protected
    procedure DoKrakenHTTPExceptionEvent(E: Exception); virtual;
  public
    property OnKrakenHTTPException: TsgcWSKrakenHTTPExceptionEvent
      read FOnKrakenHTTPException write FOnKrakenHTTPException;
    { http events }

    { events }
  private
    FOnKrakenData: TsgcWSKrakenDataEvent;
  protected
    procedure DoEventKrakenData(aData: String); virtual;
  public
    property OnKrakenData: TsgcWSKrakenDataEvent read FOnKrakenData
      write FOnKrakenData;
  end;

  TsgcWS_API_Kraken = class(TsgcWS_API_Kraken_Base)
    { from TsgcWSComponent_Base }
  protected
    procedure DoBeforeConnect; override;
  protected
    procedure DoEventConnect(aConnection: TsgcWSConnection); override;
    procedure DoEventDisconnect(aConnection: TsgcWSConnection;
      Code: Integer); override;
    { from TsgcWSComponent_Base }

    { from TsgcWSAPI_Client }
  protected
    function GetURL: String; override;
    { from TsgcWSAPI_Client }

    { read }
  protected
    procedure DoReadEvent(const aEvent, aText: String); override;
    { read }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { properties }
  private
    FKraken: TsgcWSKraken_Options;
    FREST_API: TsgcHTTP_API_Kraken_Rest;
    function GetREST_API: TsgcHTTP_API_Kraken_Rest;
    procedure SetKraken(const Value: TsgcWSKraken_Options);
  protected
    procedure OnHTTPAPIExceptionEvent(Sender: TObject; E: Exception); virtual;
  public
    property Kraken: TsgcWSKraken_Options read FKraken write SetKraken;
    property REST_API: TsgcHTTP_API_Kraken_Rest read GetREST_API
      write FREST_API;
    { properties }

    { public methods }
  private
    FFirstStatus: Boolean;
  protected
    procedure DoSubscription(const aEvent: String; const aPairs: Array of Const;
      const aSubscription: String; aInterval, aDepth, aReqId: Integer); virtual;
  public
    procedure SubscribeTicker(const aPairs: Array of Const;
      aReqId: Integer = 0);
    procedure UnSubscribeTicker(const aPairs: Array of Const;
      aReqId: Integer = 0);
  public
    procedure SubscribeOHLC(const aPairs: Array of Const;
      aInterval: TsgcWSKrakenInterval; aReqId: Integer = 0);
    procedure UnSubscribeOHLC(const aPairs: Array of Const;
      aInterval: TsgcWSKrakenInterval; aReqId: Integer = 0);
  public
    procedure SubscribeTrade(const aPairs: Array of Const; aReqId: Integer = 0);
    procedure UnSubscribeTrade(const aPairs: Array of Const;
      aReqId: Integer = 0);
  public
    procedure SubscribeBook(const aPairs: Array of Const;
      aDepth: TsgcWSKrakenDepth; aReqId: Integer = 0);
    procedure UnSubscribeBook(const aPairs: Array of Const;
      aDepth: TsgcWSKrakenDepth; aReqId: Integer = 0);
  public
    procedure SubscribeSpread(const aPairs: Array of Const;
      aReqId: Integer = 0);
    procedure UnSubscribeSpread(const aPairs: Array of Const;
      aReqId: Integer = 0);
  public
    procedure SubscribeAll(const aPairs: Array of Const; aReqId: Integer = 0);
    procedure UnSubscribeAll(const aPairs: Array of Const; aReqId: Integer = 0);
  public
    procedure UnSubscribe(aChannelId: Integer);
    { public methods }

    { private methods }
  private
    FWebSocketsToken: String;
    function GetWebSocketsToken: String;
  protected
    procedure DoPrivateSubscription(const aEvent, aSubscription: String;
      aReqId: Integer); virtual;
  public
    procedure SubscribeOwnTrades(aReqId: Integer = 0);
    procedure UnSubscribeOwnTrades(aReqId: Integer = 0);
  public
    procedure SubscribeOpenOrders(aReqId: Integer = 0);
    procedure UnSubscribeOpenOrders(aReqId: Integer = 0);
  public
    procedure AddOrder(aOrder: TsgcWSKrakenOrder; aReqId: Integer = 0);
    procedure CancelOrder(const aOrderId: String; aReqId: Integer = 0);
    { private methods }

    { events }
  private
    FOnKrakenSystemStatus: TsgcWSKrakenSystemStatusEvent;
    FOnKrakenConnect: TsgcWSKrakenConnectEvent;
    FOnKrakenSubscribed: TsgcWSKrakenSubscribedEvent;
    FOnKrakenSubscriptionError: TsgcWSKrakenSubscriptionError;
    FOnKrakenUnSubscribed: TsgcWSKrakenUnSubscribedEvent;
  protected
    procedure DoEventKrakenSystemStatus(aConnectionId, aStatus,
      aVersion: String); virtual;
    procedure DoEventKrakenConnect(aConnectionId, aStatus,
      aVersion: String); virtual;
    procedure DoEventKrakenSubscribed(aChannelId: Integer;
      aPair, aSubscription, aChannelName: String; aReqId: Integer); virtual;
    procedure DoEventKrakenUnSubscribed(aChannelId: Integer;
      aPair, aSubscription: String; aReqId: Integer); virtual;
    procedure DoEventKrakenSubscriptionError(aErrorMessage: String;
      aPair, aSubscription: string; aReqId: Integer); virtual;
  public
    property OnKrakenSystemStatus: TsgcWSKrakenSystemStatusEvent
      read FOnKrakenSystemStatus write FOnKrakenSystemStatus;
    property OnKrakenConnect: TsgcWSKrakenConnectEvent read FOnKrakenConnect
      write FOnKrakenConnect;
    property OnKrakenSubscribed: TsgcWSKrakenSubscribedEvent
      read FOnKrakenSubscribed write FOnKrakenSubscribed;
    property OnKrakenUnSubscribed: TsgcWSKrakenUnSubscribedEvent
      read FOnKrakenUnSubscribed write FOnKrakenUnSubscribed;
    property OnKrakenSubscriptionError: TsgcWSKrakenSubscriptionError
      read FOnKrakenSubscriptionError write FOnKrakenSubscriptionError;
    { events }
  end;

  TsgcWS_API_Kraken_Futures = class(TsgcWS_API_Kraken_Base)
    { from TsgcWSComponent_Base }
  protected
    procedure DoEventConnect(aConnection: TsgcWSConnection); override;
    procedure DoEventDisconnect(aConnection: TsgcWSConnection;
      Code: Integer); override;
    { from TsgcWSComponent_Base }

    { from TsgcWSAPI_Client }
  protected
    function GetURL: String; override;
    { from TsgcWSAPI_Client }

    { read }
  protected
    procedure DoReadEvent(const aEvent, aText: String); override;
    { read }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { properties }
  private
    FKraken: TsgcWSKrakenFutures_Options;
    FREST_API: TsgcHTTP_API_Kraken_Futures_Rest;
    function GetREST_API: TsgcHTTP_API_Kraken_Futures_Rest;
    procedure SetKraken(const Value: TsgcWSKrakenFutures_Options);
  protected
    procedure OnHTTPAPIExceptionEvent(Sender: TObject; E: Exception); virtual;
  public
    property Kraken: TsgcWSKrakenFutures_Options read FKraken write SetKraken;
    property REST_API: TsgcHTTP_API_Kraken_Futures_Rest read GetREST_API
      write FREST_API;
    { properties }

    { public methods }
  private
    procedure DoRequestChallenge;
  protected
    procedure DoSubscription(const aEvent: String; const aFeed: String;
      const aProductIds: Array of Const); virtual;
  public
    procedure SubscribeTicker(const aProductIds: Array of Const);
    procedure UnSubscribeTicker(const aProductIds: Array of Const);
  public
    procedure SubscribeTrade(const aProductIds: Array of Const);
    procedure UnSubscribeTrade(const aProductIds: Array of Const);
  public
    procedure SubscribeHeartbeat;
    procedure UnSubscribeHeartbeat;
  public
    procedure SubscribeTickerLite(const aProductIds: Array of Const);
    procedure UnSubscribeTickerLite(const aProductIds: Array of Const);
  public
    procedure SubscribeBook(const aProductIds: Array of Const);
    procedure UnSubscribeBook(const aProductIds: Array of Const);
    { public methods }

    { private methods }
  private
    FChallenge: String;
    function GetSignedChallenge: string;
  protected
    procedure DoSubscriptionSigned(const aEvent: String; const aFeed: String;
      const aProductIds: Array of Const); virtual;
  public
    procedure SubscribeOpenOrdersVerbose;
    procedure UnSubscribeOpenOrdersVerbose;
  public
    procedure SubscribeOpenPositions;
    procedure UnSubscribeOpenPositions;
  public
    procedure SubscribeAccountLog;
    procedure UnSubscribeAccountLog;
  public
    procedure SubscribeFills;
    procedure UnSubscribeFills;
  public
    procedure SubscribeOpenOrders;
    procedure UnSubscribeOpenOrders;
  public
    procedure SubscribeAccountBalanceAndMargins;
    procedure UnSubscribeAccountBalanceAndMargins;
  public
    procedure SubscribeNotifications;
    procedure UnSubscribeNotifications;
    { private methods }

    { events }
  private
    FOnKrakenFuturesConnect: TsgcWSKrakenFuturesConnectEvent;
    FOnKrakenFuturesSubscribed: TsgcWSKrakenFuturesSubscribedEvent;
    FOnKrakenFuturesError: TsgcWSKrakenFuturesErrorEvent;
    FOnKrakenFuturesUnSubscribed: TsgcWSKrakenFuturesUnSubscribedEvent;
  protected
    procedure DoEventKrakenFuturesConnect(const aVersion: string); virtual;
    procedure DoEventKrakenFuturesSubscribed(const aFeed: String;
      aProductId: String); virtual;
    procedure DoEventKrakenFuturesUnSubscribed(const aFeed: String;
      aProductId: String); virtual;
    procedure DoEventKrakenFuturesError(const aError: string); virtual;
  public
    property OnKrakenFuturesConnect: TsgcWSKrakenFuturesConnectEvent
      read FOnKrakenFuturesConnect write FOnKrakenFuturesConnect;
    property OnKrakenFuturesSubscribed: TsgcWSKrakenFuturesSubscribedEvent
      read FOnKrakenFuturesSubscribed write FOnKrakenFuturesSubscribed;
    property OnKrakenFuturesError: TsgcWSKrakenFuturesErrorEvent
      read FOnKrakenFuturesError write FOnKrakenFuturesError;
    property OnKrakenFuturesUnSubscribed: TsgcWSKrakenFuturesUnSubscribedEvent
      read FOnKrakenFuturesUnSubscribed write FOnKrakenFuturesUnSubscribed;
    { events }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_APIS}

uses
  sgcBase_Helpers, sgcWebSocket_Types;

const
  CS_KRAKEN_SUBSCRIPTIONSTATUS = 'subscriptionStatus';
  CS_KRAKEN_PING = 'ping';
  CS_KRAKEN_PONG = 'pong';
  CS_KRAKEN_SYSTEMSTATUS = 'systemStatus';
  CS_KRAKEN_HEARTBEAT = 'heartbeat';
  CS_KRAKEN_SUBSCRIBE = 'subscribe';
  CS_KRAKEN_UNSUBSCRIBE = 'unsubscribe';
  CS_KRAKEN_TICKER = 'ticker';
  CS_KRAKEN_TICKER_LITE = 'ticker_lite';
  CS_KRAKEN_OHLC = 'ohlc';
  CS_KRAKEN_TRADE = 'trade';
  CS_KRAKEN_BOOK = 'book';
  CS_KRAKEN_SPREAD = 'spread';
  CS_KRAKEN_ALL = '*';
  CS_KRAKEN_SUBSCRIBED = 'subscribed';
  CS_KRAKEN_UNSUBSCRIBED = 'unsubscribed';
  CS_KRAKEN_ERROR = 'error';
  CS_KRAKEN_INFO = 'info';
  CS_KRAKEN_CHALLENGE = 'challenge';

const
  CS_KRAKEN_PRIVATE_OWNTRADES = 'ownTrades';
  CS_KRAKEN_PRIVATE_OPENORDERS = 'openOrders';
  CS_KRAKEN_PRIVATE_ADDORDER = 'addOrder';
  CS_KRAKEN_PRIVATE_CANCELORDER = 'cancelOrder';

const
  CS_KRAKEN_PRIVATE_OPEN_ORDERS_VERBOSE = 'open_orders_verbose';
  CS_KRAKEN_PRIVATE_OPEN_POSITIONS = 'open_positions';
  CS_KRAKEN_PRIVATE_ACCOUNT_LOG = 'account_log';
  CS_KRAKEN_PRIVATE_FILLS = 'fills';
  CS_KRAKEN_PRIVATE_OPEN_ORDERS = 'open_orders';
  CS_KRAKEN_PRIVATE_ACCOUNT_BALANCE_AND_MARGINS =
    'account_balances_and_margins';
  CS_KRAKEN_PRIVATE_NOTIFICATIONS = 'notifications_auth';

constructor TsgcWS_API_Kraken_Base.Create(aOwner: TComponent);
begin
  inherited;
  FFormatCurrency := '0.000000';
end;

destructor TsgcWS_API_Kraken_Base.Destroy;
begin
  inherited;
end;

procedure TsgcWS_API_Kraken.AddOrder(aOrder: TsgcWSKrakenOrder;
  aReqId: Integer = 0);
var
  vJSON: String;
begin
  vJSON := '{';
  vJSON := vJSON + '"event": "addOrder"';
  vJSON := vJSON + ', "token": "' + GetWebSocketsToken + '"';
  if aReqId > 0 then
    vJSON := vJSON + ', "reqid": ' + IntToStr(aReqId) + '';
  vJSON := vJSON + ', "ordertype": "' + GetOrderType(aOrder.OrderType) + '"';
  vJSON := vJSON + ', "type": "' + GetOrderSide(aOrder._Type) + '"';
  vJSON := vJSON + ', "pair": "' + aOrder.Pair + '"';
  if aOrder.Price <> 0 then
    vJSON := vJSON + ', "price": "' + GetCurrencyString(aOrder.Price) + '"';
  if aOrder.Price2 <> 0 then
    vJSON := vJSON + ', "price2": "' + GetCurrencyString(aOrder.Price2) + '"';
  vJSON := vJSON + ', "volume": "' + GetCurrencyString(aOrder.Volume) + '"';
  if aOrder.Leverage <> '' then
    vJSON := vJSON + ', "leverage": "' + aOrder.Leverage + '"';
  if aOrder.OFlags <> '' then
    vJSON := vJSON + ', "oflags": "' + aOrder.OFlags + '"';
  if aOrder.StartTm <> '' then
    vJSON := vJSON + ', "starttm": "' + aOrder.StartTm + '"';
  if aOrder.ExpireTm <> '' then
    vJSON := vJSON + ', "expiretm": "' + aOrder.ExpireTm + '"';
  if aOrder.UserRef <> '' then
    vJSON := vJSON + ', "userref": "' + aOrder.UserRef + '"';
  if aOrder.Validate then
    vJSON := vJSON + ', "validate": "true"';
  if aOrder.Close_OrderType <> kotNone then
    vJSON := vJSON + ', "close[ordertype]": "' +
      GetOrderType(aOrder.Close_OrderType) + '"';
  if aOrder.Close_Price <> 0 then
    vJSON := vJSON + ', "close[price]": "' + GetCurrencyString
      (aOrder.Close_Price) + '"';
  if aOrder.Close_Price2 <> 0 then
    vJSON := vJSON + ', "close[price2]": "' + GetCurrencyString
      (aOrder.Close_Price2) + '"';
  if aOrder.TradingAgreement <> '' then
    vJSON := vJSON + ', "trading_agreement": "' + aOrder.TradingAgreement + '"';
  vJSON := vJSON + '}';

  DoWriteData(vJSON);
end;

procedure TsgcWS_API_Kraken.CancelOrder(const aOrderId: String;
  aReqId: Integer = 0);
begin
  DoWriteData(Format('{"event": "cancelOrder","token": "%s", "txid": ["%s"]}',
    [GetWebSocketsToken, aOrderId]));
end;

procedure TsgcWS_API_Kraken.DoEventConnect(aConnection: TsgcWSConnection);
begin
  inherited;
  FWebSocketsToken := '';
  FFirstStatus := True;
end;

procedure TsgcWS_API_Kraken.DoEventDisconnect(aConnection: TsgcWSConnection;
  Code: Integer);
begin
  FWebSocketsToken := '';
  FFirstStatus := True;
  inherited;
end;

procedure TsgcWS_API_Kraken.DoEventKrakenSystemStatus(aConnectionId, aStatus,
  aVersion: String);
begin
  if Assigned(FOnKrakenSystemStatus) then
    FOnKrakenSystemStatus(self, aConnectionId, aStatus, aVersion);
end;

procedure TsgcWS_API_Kraken.DoEventKrakenConnect(aConnectionId, aStatus,
  aVersion: String);
begin
  if Assigned(FOnKrakenConnect) then
    FOnKrakenConnect(self, aConnectionId, aStatus, aVersion);
end;

procedure TsgcWS_API_Kraken_Base.DoEventKrakenData(aData: String);
begin
  if Assigned(FOnKrakenData) then
    FOnKrakenData(self, aData);
end;

procedure TsgcWS_API_Kraken.DoEventKrakenSubscribed(aChannelId: Integer;
  aPair, aSubscription, aChannelName: String; aReqId: Integer);
begin
  if Assigned(FOnKrakenSubscribed) then
    FOnKrakenSubscribed(self, aChannelId, aPair, aSubscription,
      aChannelName, aReqId);
end;

procedure TsgcWS_API_Kraken.DoEventKrakenSubscriptionError
  (aErrorMessage: String; aPair, aSubscription: string; aReqId: Integer);
begin
  if Assigned(FOnKrakenSubscriptionError) then
    FOnKrakenSubscriptionError(self, aErrorMessage, aPair,
      aSubscription, aReqId);
end;

procedure TsgcWS_API_Kraken.DoEventKrakenUnSubscribed(aChannelId: Integer;
  aPair, aSubscription: String; aReqId: Integer);
begin
  if Assigned(FOnKrakenUnSubscribed) then
    FOnKrakenUnSubscribed(self, aChannelId, aPair, aSubscription, aReqId);
end;

procedure TsgcWS_API_Kraken_Base.DoEventMessage(aConnection: TsgcWSConnection;
  const Text: string);
begin
  if RawMessages then
    inherited
  else
  begin
    JSON.Clear;
    JSON.Read(Text);

    if JSON.Node['event'] <> nil then
      DoReadEvent(JSON.Node['event'].Value, Text)
    else
      DoEventKrakenData(Text);
  end;
end;

function TsgcWS_API_Kraken_Base.DoKeepAlive: Boolean;
begin
  Result := True;

  DoPing;
end;

procedure TsgcWS_API_Kraken_Base.DoKrakenHTTPExceptionEvent(E: Exception);
begin
  if Assigned(FOnKrakenHTTPException) then
    FOnKrakenHTTPException(self, E);
end;

procedure TsgcWS_API_Kraken_Base.DoPing;
begin
  DoWriteData('{"event": "' + CS_KRAKEN_PING + '", "reqid": ' +
    FormatDateTime('hhnnsszzz', Now) + '}');
end;

procedure TsgcWS_API_Kraken.DoPrivateSubscription(const aEvent,
  aSubscription: String; aReqId: Integer);
var
  vJSON: string;
  vToken: string;
begin
  vToken := GetWebSocketsToken;

  vJSON := '{"event": "' + aEvent + '",';
  if aReqId > 0 then
    vJSON := '"reqId": ' + IntToStr(aReqId) + ',';
  vJSON := vJSON + '"subscription": {';
  vJSON := vJSON + '"name": "' + aSubscription + '", ';
  vJSON := vJSON + '"token": "' + vToken + '"';
  vJSON := vJSON + '}}';

  DoWriteData(vJSON);
end;

procedure TsgcWS_API_Kraken.DoSubscription(const aEvent: String;
  const aPairs: Array of Const; const aSubscription: String;
  aInterval, aDepth, aReqId: Integer);
var
  i: Integer;
  vJSON: string;
begin
  vJSON := '{"event": "' + aEvent + '",';
  if aReqId > 0 then
    vJSON := '"reqId": ' + IntToStr(aReqId) + ',';
  vJSON := vJSON + '"pair": [';
  for i := 0 to Length(aPairs) - 1 do
  begin
    if i > 0 then
      vJSON := vJSON + ',';
    vJSON := vJSON + '"' + String(aPairs[i].VWideString) + '"';
  end;
  vJSON := vJSON + '],';
  vJSON := vJSON + '"subscription": {';
  vJSON := vJSON + '"name": "' + aSubscription + '"';
  if aInterval > 0 then
    vJSON := vJSON + ',"interval": ' + IntToStr(aInterval);
  if aDepth > 0 then
    vJSON := vJSON + ',"depth": ' + IntToStr(aDepth);
  vJSON := vJSON + '}}';

  DoWriteData(vJSON);
end;

procedure TsgcWS_API_Kraken_Base.DoWriteData(const aText: String);
begin
  if Assigned(FClient) then
    FClient.WriteData(aText);
end;

function TsgcWS_API_Kraken_Base.GetCurrencyString(aValue: Currency): String;
begin
  Result := FormatCurr(FormatCurrency, aValue);
end;

function TsgcWS_API_Kraken_Base.GetDepth(aDepth: TsgcWSKrakenDepth): Integer;
begin
  case aDepth of
    kde10:
      Result := 10;
    kde25:
      Result := 25;
    kde100:
      Result := 100;
    kde500:
      Result := 500;
    kde1000:
      Result := 1000;
  else
    Result := 0;
  end;
end;

function TsgcWS_API_Kraken_Base.GetInterval
  (aInterval: TsgcWSKrakenInterval): Integer;
begin
  case aInterval of
    kin1min:
      Result := 1;
    kin5min:
      Result := 5;
    kin15min:
      Result := 15;
    kin30min:
      Result := 30;
    kin60min:
      Result := 60;
    kin240min:
      Result := 240;
    kin1440min:
      Result := 1440;
    kin10080min:
      Result := 10080;
    kin21600min:
      Result := 21600;
  else
    Result := 0;
  end;
end;

function TsgcWS_API_Kraken_Base.GetJSON: TsgcJSON;
begin
  if not Assigned(FJSON) then
    FJSON := TsgcJSON.Create(self);
  Result := FJSON;
end;

function TsgcWS_API_Kraken_Base.GetOrderSide(aOrderSide
  : TsgcWSKrakenOrderSide): String;
begin
  case aOrderSide of
    kosBuy:
      Result := 'buy';
    kosSell:
      Result := 'sell';
  else
    Result := '';
  end;
end;

function TsgcWS_API_Kraken_Base.GetOrderType(aOrderType
  : TsgcWSKrakenOrderType): String;
begin
  case aOrderType of
    kotMarket:
      Result := 'market';
    kotLimit:
      Result := 'limit';
    kotStopLoss:
      Result := 'stop_loss';
    kotTakeProfit:
      Result := 'take_profit';
    kotStopLossProfit:
      Result := 'stop_loss_profit';
    kotStopLossProfitLimit:
      Result := 'stop_loss_profit_limit';
    kotStopLossLimit:
      Result := 'stop_loss_limit';
    kotTakeProfitLimit:
      Result := 'take_profit_limit';
    kotTrailingStop:
      Result := 'trailing_stop';
    kotTrailingStopLimit:
      Result := 'trailing_stop_limit';
    kotStopLossAndLimit:
      Result := 'stop_loss_and_limit';
  else
    Result := '';
  end;
end;

procedure TsgcWS_API_Kraken_Base.Ping;
begin
  DoPing;
end;

procedure TsgcWS_API_Kraken.SubscribeTicker(const aPairs: Array of Const;
  aReqId: Integer = 0);
begin
  DoSubscription(CS_KRAKEN_SUBSCRIBE, aPairs, CS_KRAKEN_TICKER, 0, 0, aReqId);
end;

procedure TsgcWS_API_Kraken.SubscribeOHLC(const aPairs: Array of Const;
  aInterval: TsgcWSKrakenInterval; aReqId: Integer = 0);
begin
  DoSubscription(CS_KRAKEN_SUBSCRIBE, aPairs, CS_KRAKEN_OHLC,
    GetInterval(aInterval), 0, aReqId);
end;

procedure TsgcWS_API_Kraken.SubscribeTrade(const aPairs: Array of Const;
  aReqId: Integer = 0);
begin
  DoSubscription(CS_KRAKEN_SUBSCRIBE, aPairs, CS_KRAKEN_TRADE, 0, 0, aReqId);
end;

procedure TsgcWS_API_Kraken.SubscribeBook(const aPairs: Array of Const;
  aDepth: TsgcWSKrakenDepth; aReqId: Integer = 0);
begin
  DoSubscription(CS_KRAKEN_SUBSCRIBE, aPairs, CS_KRAKEN_BOOK, 0,
    GetDepth(aDepth), aReqId);
end;

procedure TsgcWS_API_Kraken.SubscribeSpread(const aPairs: Array of Const;
  aReqId: Integer = 0);
begin
  DoSubscription(CS_KRAKEN_SUBSCRIBE, aPairs, CS_KRAKEN_SPREAD, 0, 0, aReqId);
end;

procedure TsgcWS_API_Kraken.SubscribeAll(const aPairs: Array of Const;
  aReqId: Integer = 0);
begin
  DoSubscription(CS_KRAKEN_SUBSCRIBE, aPairs, CS_KRAKEN_ALL, 0, 0, aReqId);
end;

procedure TsgcWS_API_Kraken.SubscribeOpenOrders(aReqId: Integer = 0);
begin
  DoPrivateSubscription(CS_KRAKEN_SUBSCRIBE,
    CS_KRAKEN_PRIVATE_OPENORDERS, aReqId);
end;

procedure TsgcWS_API_Kraken.SubscribeOwnTrades(aReqId: Integer = 0);
begin
  DoPrivateSubscription(CS_KRAKEN_SUBSCRIBE,
    CS_KRAKEN_PRIVATE_OWNTRADES, aReqId);
end;

procedure TsgcWS_API_Kraken.UnSubscribe(aChannelId: Integer);
begin
  DoWriteData('{"event": "unsubscribe", "channelID": ' +
    IntToStr(aChannelId) + '}');
end;

procedure TsgcWS_API_Kraken.UnSubscribeAll(const aPairs: Array of Const;
  aReqId: Integer = 0);
begin
  DoSubscription(CS_KRAKEN_UNSUBSCRIBE, aPairs, CS_KRAKEN_ALL, 0, 0, aReqId);
end;

procedure TsgcWS_API_Kraken.UnSubscribeSpread(const aPairs: Array of Const;
  aReqId: Integer = 0);
begin
  DoSubscription(CS_KRAKEN_UNSUBSCRIBE, aPairs, CS_KRAKEN_SPREAD, 0, 0, aReqId);
end;

procedure TsgcWS_API_Kraken.UnSubscribeBook(const aPairs: Array of Const;
  aDepth: TsgcWSKrakenDepth; aReqId: Integer = 0);
begin
  DoSubscription(CS_KRAKEN_UNSUBSCRIBE, aPairs, CS_KRAKEN_BOOK, 0,
    GetDepth(aDepth), aReqId);
end;

procedure TsgcWS_API_Kraken.UnSubscribeTrade(const aPairs: Array of Const;
  aReqId: Integer = 0);
begin
  DoSubscription(CS_KRAKEN_UNSUBSCRIBE, aPairs, CS_KRAKEN_TRADE, 0, 0, aReqId);
end;

procedure TsgcWS_API_Kraken.UnSubscribeOHLC(const aPairs: Array of Const;
  aInterval: TsgcWSKrakenInterval; aReqId: Integer = 0);
begin
  DoSubscription(CS_KRAKEN_UNSUBSCRIBE, aPairs, CS_KRAKEN_OHLC,
    GetInterval(aInterval), 0, aReqId);
end;

procedure TsgcWS_API_Kraken.UnSubscribeOpenOrders(aReqId: Integer = 0);
begin
  DoPrivateSubscription(CS_KRAKEN_UNSUBSCRIBE,
    CS_KRAKEN_PRIVATE_OPENORDERS, aReqId);
end;

procedure TsgcWS_API_Kraken.UnSubscribeOwnTrades;
begin
  DoPrivateSubscription(CS_KRAKEN_UNSUBSCRIBE,
    CS_KRAKEN_PRIVATE_OWNTRADES, aReqId);
end;

procedure TsgcWS_API_Kraken.UnSubscribeTicker(const aPairs: Array of Const;
  aReqId: Integer = 0);
begin
  DoSubscription(CS_KRAKEN_UNSUBSCRIBE, aPairs, CS_KRAKEN_TICKER, 0, 0, aReqId);
end;

constructor TsgcWSKraken_Options_Base.Create;
begin
  inherited;
  FHTTPLogOptions := TsgcWSKrakenLog_Options.Create;
end;

destructor TsgcWSKraken_Options_Base.Destroy;
begin
  sgcFree(FHTTPLogOptions);
  inherited;
end;

procedure TsgcWSKraken_Options_Base.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSKraken_Options_Base then
  begin
    ApiKey := TsgcWSKraken_Options_Base(aSource).ApiKey;
    ApiSecret := TsgcWSKraken_Options_Base(aSource).ApiSecret;
    HTTPLogOptions.Assign(TsgcWSKraken_Options_Base(aSource).HTTPLogOptions);
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSKraken_Options_Base.SetHTTPLogOptions
  (const Value: TsgcWSKrakenLog_Options);
begin
  if Assigned(FHTTPLogOptions) then
    FHTTPLogOptions.Assign(Value);
end;

procedure TsgcWSKrakenLog_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSKrakenLog_Options then
  begin
    Enabled := TsgcWSKrakenLog_Options(aSource).Enabled;
    FileName := TsgcWSKrakenLog_Options(aSource).FileName;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcWSKrakenOrder.Create;
begin
  inherited;
  FClose_OrderType := kotNone;
  FClose_Price := 0;
  FClose_Price2 := 0;
  FExpireTm := '';
  FLeverage := '';
  FOFlags := '';
  FOrderType := kotNone;
  FPair := '';
  FPrice := 0;
  FPrice2 := 0;
  FStartTm := '';
  FTradingAgreement := '';
  FUserRef := '';
  FValidate := false;
  FVolume := 0;
  F_Type := kosNone;
end;

constructor TsgcWS_API_Kraken.Create(aOwner: TComponent);
begin
  inherited;
  FKraken := TsgcWSKraken_Options.Create;
end;

destructor TsgcWS_API_Kraken.Destroy;
begin
  sgcFree(FKraken);
  sgcFree(FREST_API);
  inherited;
end;

procedure TsgcWS_API_Kraken.DoBeforeConnect;
begin
  inherited;
  if Assigned(FClient) then
  begin
    if not FClient.HeartBeat.Enabled then
    begin
      FClient.HeartBeat.Interval := 30;
      FClient.HeartBeat.Enabled := True;
    end;
  end;
end;

procedure TsgcWS_API_Kraken.DoReadEvent(const aEvent, aText: String);
var
  vChannelId: Integer;
  vChannelName: string;
  vErrorMessage: string;
  vPair: string;
  vSubscription: string;
  vReqId: Integer;
begin
  inherited;
  if aEvent = CS_KRAKEN_HEARTBEAT then
  begin
    // nothing
  end
  else if aEvent = CS_KRAKEN_PING then
    DoWriteData(sgcStringReplace(aText, '"' + CS_KRAKEN_PING + '"',
      '"' + CS_KRAKEN_PONG + '"'))
  else if aEvent = CS_KRAKEN_PONG then
  begin
    // nothing
  end
  else if aEvent = CS_KRAKEN_SUBSCRIPTIONSTATUS then
  begin
    if JSON.Node['pair'] <> nil then
      vPair := JSON.Node['pair'].Value;
    if JSON.Node['subscription'] <> nil then
      vSubscription := JSON.Node['subscription'].Node['name'].Value;
    vReqId := 0;
    if JSON.Node['reqID'] <> nil then
      vReqId := JSON.Node['reqID'].Value;

    if JSON.Node['errorMessage'] <> nil then
    begin
      vErrorMessage := JSON.Node['errorMessage'].Value;

      DoEventKrakenSubscriptionError(vErrorMessage, vPair,
        vSubscription, vReqId);
    end
    else if JSON.Node['status'] <> nil then
    begin
      vChannelId := JSON.Node['channelID'].Value;

      if JSON.Node['status'].Value = CS_KRAKEN_SUBSCRIBED then
      begin
        vChannelName := JSON.Node['channelName'].Value;

        DoEventKrakenSubscribed(vChannelId, vPair, vSubscription,
          vChannelName, vReqId)
      end
      else if JSON.Node['status'].Value = CS_KRAKEN_UNSUBSCRIBED then
        DoEventKrakenUnSubscribed(vChannelId, vPair, vSubscription, vReqId);
    end
  end
  else if aEvent = CS_KRAKEN_SYSTEMSTATUS then
  begin
    if FFirstStatus then
    begin
      DoEventKrakenConnect(JSON.Node['connectionID'].Value,
        JSON.Node['status'].Value, JSON.Node['version'].Value);
      FFirstStatus := True;
    end
    else
      DoEventKrakenSystemStatus(JSON.Node['connectionID'].Value,
        JSON.Node['status'].Value, JSON.Node['version'].Value);
  end;
end;

function TsgcWS_API_Kraken.GetREST_API: TsgcHTTP_API_Kraken_Rest;
begin
  if not Assigned(FREST_API) then
    FREST_API := TsgcHTTP_API_Kraken_Rest.Create(nil);
  FREST_API.KrakenOptions.ApiKey := Kraken.ApiKey;
  FREST_API.KrakenOptions.ApiSecret := Kraken.ApiSecret;
  if FREST_API.KrakenOptions.LogOptions.Enabled <> Kraken.HTTPLogOptions.Enabled
  then
  begin
    FREST_API.KrakenOptions.LogOptions.Enabled := Kraken.HTTPLogOptions.Enabled;
    FREST_API.KrakenOptions.LogOptions.FileName :=
      Kraken.HTTPLogOptions.FileName;
  end;
  if Assigned(FOnKrakenHTTPException) then
    FREST_API.OnHTTPAPIException := OnHTTPAPIExceptionEvent
  else
    FREST_API.OnHTTPAPIException := nil;
  Result := FREST_API;
end;

function TsgcWS_API_Kraken.GetURL: String;
begin
  if Kraken.Beta then
    Result := 'wss://beta-ws.kraken.com'
  else
  begin
    if Kraken.ApiKey <> '' then
      Result := 'wss://ws-auth.kraken.com'
    else
      Result := 'wss://ws.kraken.com';
  end;

  if Assigned(Client) then
    Client.TLSOptions.Version := tls1_2;
end;

function TsgcWS_API_Kraken.GetWebSocketsToken: String;
var
  oJSON: TsgcJSON;
begin
  if FWebSocketsToken = '' then
  begin
    FWebSocketsToken := REST_API.GetWebSocketsToken;
    if FWebSocketsToken <> '' then
    begin
      oJSON := TsgcJSON.Create(nil);
      Try
        oJSON.Read(FWebSocketsToken);
        if oJSON.Node['error'] <> nil then
        begin
          if oJSON.Node['error'].Value = '[]' then
          begin
            if oJSON.Node['result'] <> nil then
            begin
              if oJSON.Node['result'].Node['token'] <> nil then
                FWebSocketsToken := oJSON.Node['result'].Node['token'].Value;
            end
            else
              DoException(nil, FWebSocketsToken);
          end
          else
            DoException(nil, oJSON.Node['error'].Value);
        end;
      Finally
        sgcFree(oJSON);
      End;
    end;
  end;
  Result := FWebSocketsToken;
end;

procedure TsgcWS_API_Kraken.OnHTTPAPIExceptionEvent(Sender: TObject;
  E: Exception);
begin
  DoKrakenHTTPExceptionEvent(E);
end;

procedure TsgcWS_API_Kraken.SetKraken(const Value: TsgcWSKraken_Options);
begin
  if Assigned(FKraken) then
    FKraken.Assign(Value);
end;

constructor TsgcWS_API_Kraken_Futures.Create(aOwner: TComponent);
begin
  inherited;
  FKraken := TsgcWSKrakenFutures_Options.Create;
end;

destructor TsgcWS_API_Kraken_Futures.Destroy;
begin
  sgcFree(FKraken);
  sgcFree(FREST_API);
  inherited;
end;

procedure TsgcWS_API_Kraken_Futures.DoEventConnect
  (aConnection: TsgcWSConnection);
begin
  inherited;
  FChallenge := '';
end;

procedure TsgcWS_API_Kraken_Futures.DoEventDisconnect
  (aConnection: TsgcWSConnection; Code: Integer);
begin
  inherited;
  FChallenge := '';
end;

procedure TsgcWS_API_Kraken_Futures.DoEventKrakenFuturesConnect
  (const aVersion: string);
begin
  if Assigned(FOnKrakenFuturesConnect) then
    FOnKrakenFuturesConnect(self, aVersion);
end;

procedure TsgcWS_API_Kraken_Futures.DoEventKrakenFuturesSubscribed
  (const aFeed: String; aProductId: String);
begin
  if Assigned(FOnKrakenFuturesSubscribed) then
    FOnKrakenFuturesSubscribed(self, aFeed, aProductId);
end;

procedure TsgcWS_API_Kraken_Futures.DoEventKrakenFuturesError
  (const aError: string);
begin
  if Assigned(FOnKrakenFuturesError) then
    FOnKrakenFuturesError(self, aError);
end;

procedure TsgcWS_API_Kraken_Futures.DoEventKrakenFuturesUnSubscribed
  (const aFeed: String; aProductId: String);
begin
  if Assigned(FOnKrakenFuturesUnSubscribed) then
    FOnKrakenFuturesUnSubscribed(self, aFeed, aProductId);
end;

procedure TsgcWS_API_Kraken_Futures.DoReadEvent(const aEvent, aText: String);
var
  vFeed, vProductId, vMessage, vVersion: string;
begin
  inherited;
  if aEvent = CS_KRAKEN_HEARTBEAT then
  begin
    // nothing
  end
  else if (aEvent = CS_KRAKEN_SUBSCRIBED) or (aEvent = CS_KRAKEN_UNSUBSCRIBED)
  then
  begin
    if JSON.Node['feed'] <> nil then
      vFeed := JSON.Node['feed'].Value;
    if JSON.Node['product_ids'] <> nil then
    begin
      if Assigned(JSON.Node['product_ids'].JSONObject) then
        vProductId := JSON.Node['product_ids'].JSONObject.Item[0].Value
    end;
    if aEvent = CS_KRAKEN_SUBSCRIBED then
      DoEventKrakenFuturesSubscribed(vFeed, vProductId)
    else if aEvent = CS_KRAKEN_UNSUBSCRIBED then
      DoEventKrakenFuturesUnSubscribed(vFeed, vProductId);
  end
  else if aEvent = CS_KRAKEN_ERROR then
  begin
    if JSON.Node['message'] <> nil then
      vMessage := JSON.Node['message'].Value;
    DoEventKrakenFuturesError(vMessage);
  end
  else if aEvent = CS_KRAKEN_CHALLENGE then
  begin
    if JSON.Node['message'] <> nil then
      FChallenge := JSON.Node['message'].Value;
  end
  else if aEvent = CS_KRAKEN_INFO then
  begin
    if JSON.Node['version'] <> nil then
      vVersion := JSON.Node['version'].Value;
    DoEventKrakenFuturesConnect(vVersion);
    if Kraken.ApiKey <> '' then
      DoRequestChallenge;
  end;
end;

procedure TsgcWS_API_Kraken_Futures.DoRequestChallenge;
begin
  DoWriteData(Format('{"event":"%s","api_key":"%s"}', [CS_KRAKEN_CHALLENGE,
    Kraken.ApiKey]));
end;

procedure TsgcWS_API_Kraken_Futures.DoSubscription(const aEvent: String;
  const aFeed: String; const aProductIds: Array of Const);
var
  i: Integer;
  vJSON: string;
begin
  vJSON := '{"event": "' + aEvent + '",';
  vJSON := vJSON + '"feed":"' + aFeed + '"';
  if Length(aProductIds) > 0 then
  begin
    vJSON := vJSON + ', "product_ids": [';
    for i := 0 to Length(aProductIds) - 1 do
    begin
      if i > 0 then
        vJSON := vJSON + ',';
      vJSON := vJSON + '"' + String(aProductIds[i].VWideString) + '"';
    end;
    vJSON := vJSON + ']';
  end;
  vJSON := vJSON + '}';

  DoWriteData(vJSON);
end;

procedure TsgcWS_API_Kraken_Futures.DoSubscriptionSigned(const aEvent: String;
  const aFeed: String; const aProductIds: Array of Const);
var
  i: Integer;
  vJSON: string;
begin
  vJSON := '{"event": "' + aEvent + '",';
  vJSON := vJSON + '"feed":"' + aFeed + '"';
  if Length(aProductIds) > 0 then
  begin
    vJSON := vJSON + ', "product_ids": [';
    for i := 0 to Length(aProductIds) - 1 do
    begin
      if i > 0 then
        vJSON := vJSON + ',';
      vJSON := vJSON + '"' + String(aProductIds[i].VWideString) + '"';
    end;
    vJSON := vJSON + ']';
  end;
  vJSON := vJSON + ', "api_key":"' + Kraken.ApiKey + '"';
  vJSON := vJSON + ', "original_challenge":"' + FChallenge + '"';
  vJSON := vJSON + ', "signed_challenge":"' + GetSignedChallenge + '"';
  vJSON := vJSON + '}';

  DoWriteData(vJSON);
end;

function TsgcWS_API_Kraken_Futures.GetREST_API
  : TsgcHTTP_API_Kraken_Futures_Rest;
begin
  if not Assigned(FREST_API) then
    FREST_API := TsgcHTTP_API_Kraken_Futures_Rest.Create(nil);
  FREST_API.KrakenOptions.ApiKey := Kraken.ApiKey;
  FREST_API.KrakenOptions.ApiSecret := Kraken.ApiSecret;
  FREST_API.KrakenOptions.Demo := Kraken.Demo;
  if FREST_API.KrakenOptions.LogOptions.Enabled <> Kraken.HTTPLogOptions.Enabled
  then
  begin
    FREST_API.KrakenOptions.LogOptions.Enabled := Kraken.HTTPLogOptions.Enabled;
    FREST_API.KrakenOptions.LogOptions.FileName :=
      Kraken.HTTPLogOptions.FileName;
  end;
  if Assigned(FOnKrakenHTTPException) then
    FREST_API.OnHTTPAPIException := OnHTTPAPIExceptionEvent
  else
    FREST_API.OnHTTPAPIException := nil;
  Result := FREST_API;
end;

function TsgcWS_API_Kraken_Futures.GetSignedChallenge: string;
var
  oHash: TBytes;
  oBytes: TBytes;
begin
  Result := '';
  oHash := nil;
  
  if FChallenge <> '' then
  begin
    oHash := GetHashSHA256(FChallenge);
    DecodeBase64(Kraken.ApiSecret, oBytes);
    Result := GetHMACSHA512(oHash, oBytes, True);
  end
  else
    DoEventKrakenFuturesError('Challenge is empty.');
end;

function TsgcWS_API_Kraken_Futures.GetURL: String;
begin
  if Kraken.Demo then
    Result := 'wss://demo-futures.kraken.com/ws/v1'
  else
    Result := 'wss://futures.kraken.com/ws/v1';

  if Assigned(Client) then
    Client.TLSOptions.Version := tls1_2;
end;

procedure TsgcWS_API_Kraken_Futures.OnHTTPAPIExceptionEvent(Sender: TObject;
  E: Exception);
begin
  DoKrakenHTTPExceptionEvent(E);
end;

procedure TsgcWS_API_Kraken_Futures.SetKraken(const Value
  : TsgcWSKrakenFutures_Options);
begin
  if Assigned(FKraken) then
    FKraken.Assign(Value);
end;

procedure TsgcWS_API_Kraken_Futures.SubscribeAccountLog;
begin
  DoSubscriptionSigned(CS_KRAKEN_SUBSCRIBE, CS_KRAKEN_PRIVATE_ACCOUNT_LOG, []);
end;

procedure TsgcWS_API_Kraken_Futures.SubscribeAccountBalanceAndMargins;
begin
  DoSubscriptionSigned(CS_KRAKEN_SUBSCRIBE,
    CS_KRAKEN_PRIVATE_ACCOUNT_BALANCE_AND_MARGINS, []);
end;

procedure TsgcWS_API_Kraken_Futures.SubscribeBook(const aProductIds
  : Array of Const);
begin
  DoSubscription(CS_KRAKEN_SUBSCRIBE, CS_KRAKEN_BOOK, aProductIds);
end;

procedure TsgcWS_API_Kraken_Futures.SubscribeFills;
begin
  DoSubscriptionSigned(CS_KRAKEN_SUBSCRIBE, CS_KRAKEN_PRIVATE_FILLS, []);
end;

procedure TsgcWS_API_Kraken_Futures.SubscribeHeartbeat;
begin
  DoSubscription(CS_KRAKEN_SUBSCRIBE, CS_KRAKEN_HEARTBEAT, []);
end;

procedure TsgcWS_API_Kraken_Futures.SubscribeNotifications;
begin
  DoSubscriptionSigned(CS_KRAKEN_SUBSCRIBE,
    CS_KRAKEN_PRIVATE_NOTIFICATIONS, []);
end;

procedure TsgcWS_API_Kraken_Futures.SubscribeOpenOrders;
begin
  DoSubscriptionSigned(CS_KRAKEN_SUBSCRIBE, CS_KRAKEN_PRIVATE_OPEN_ORDERS, []);
end;

procedure TsgcWS_API_Kraken_Futures.SubscribeOpenOrdersVerbose;
begin
  DoSubscriptionSigned(CS_KRAKEN_SUBSCRIBE,
    CS_KRAKEN_PRIVATE_OPEN_ORDERS_VERBOSE, []);
end;

procedure TsgcWS_API_Kraken_Futures.SubscribeOpenPositions;
begin
  DoSubscriptionSigned(CS_KRAKEN_SUBSCRIBE,
    CS_KRAKEN_PRIVATE_OPEN_POSITIONS, []);
end;

procedure TsgcWS_API_Kraken_Futures.SubscribeTicker(const aProductIds
  : Array of Const);
begin
  DoSubscription(CS_KRAKEN_SUBSCRIBE, CS_KRAKEN_TICKER, aProductIds);
end;

procedure TsgcWS_API_Kraken_Futures.SubscribeTickerLite(const aProductIds
  : Array of Const);
begin
  DoSubscription(CS_KRAKEN_SUBSCRIBE, CS_KRAKEN_TICKER_LITE, aProductIds);
end;

procedure TsgcWS_API_Kraken_Futures.SubscribeTrade(const aProductIds
  : Array of Const);
begin
  DoSubscription(CS_KRAKEN_SUBSCRIBE, CS_KRAKEN_TRADE, aProductIds);
end;

procedure TsgcWS_API_Kraken_Futures.UnSubscribeAccountLog;
begin
  DoSubscriptionSigned(CS_KRAKEN_UNSUBSCRIBE,
    CS_KRAKEN_PRIVATE_ACCOUNT_LOG, []);
end;

procedure TsgcWS_API_Kraken_Futures.UnSubscribeAccountBalanceAndMargins;
begin
  DoSubscriptionSigned(CS_KRAKEN_UNSUBSCRIBE,
    CS_KRAKEN_PRIVATE_ACCOUNT_BALANCE_AND_MARGINS, []);
end;

procedure TsgcWS_API_Kraken_Futures.UnSubscribeBook(const aProductIds
  : Array of Const);
begin
  DoSubscription(CS_KRAKEN_UNSUBSCRIBE, CS_KRAKEN_BOOK, aProductIds);
end;

procedure TsgcWS_API_Kraken_Futures.UnSubscribeFills;
begin
  DoSubscriptionSigned(CS_KRAKEN_UNSUBSCRIBE, CS_KRAKEN_PRIVATE_FILLS, []);
end;

procedure TsgcWS_API_Kraken_Futures.UnSubscribeHeartbeat;
begin
  DoSubscription(CS_KRAKEN_UNSUBSCRIBE, CS_KRAKEN_HEARTBEAT, []);
end;

procedure TsgcWS_API_Kraken_Futures.UnSubscribeNotifications;
begin
  DoSubscriptionSigned(CS_KRAKEN_UNSUBSCRIBE,
    CS_KRAKEN_PRIVATE_NOTIFICATIONS, []);
end;

procedure TsgcWS_API_Kraken_Futures.UnSubscribeOpenOrders;
begin
  DoSubscriptionSigned(CS_KRAKEN_UNSUBSCRIBE,
    CS_KRAKEN_PRIVATE_OPEN_ORDERS, []);
end;

procedure TsgcWS_API_Kraken_Futures.UnSubscribeOpenOrdersVerbose;
begin
  DoSubscriptionSigned(CS_KRAKEN_UNSUBSCRIBE,
    CS_KRAKEN_PRIVATE_OPEN_ORDERS_VERBOSE, []);
end;

procedure TsgcWS_API_Kraken_Futures.UnSubscribeOpenPositions;
begin
  DoSubscriptionSigned(CS_KRAKEN_UNSUBSCRIBE,
    CS_KRAKEN_PRIVATE_OPEN_POSITIONS, []);
end;

procedure TsgcWS_API_Kraken_Futures.UnSubscribeTicker(const aProductIds
  : Array of Const);
begin
  DoSubscription(CS_KRAKEN_UNSUBSCRIBE, CS_KRAKEN_TICKER, aProductIds);
end;

procedure TsgcWS_API_Kraken_Futures.UnSubscribeTickerLite(const aProductIds
  : Array of Const);
begin
  DoSubscription(CS_KRAKEN_UNSUBSCRIBE, CS_KRAKEN_TICKER_LITE, aProductIds);
end;

procedure TsgcWS_API_Kraken_Futures.UnSubscribeTrade(const aProductIds
  : Array of Const);
begin
  DoSubscription(CS_KRAKEN_UNSUBSCRIBE, CS_KRAKEN_TRADE, aProductIds);
end;

constructor TsgcWSKraken_Options.Create;
begin
  inherited;
  Beta := false;
end;

procedure TsgcWSKraken_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSKraken_Options then
  begin
    Beta := TsgcWSKraken_Options(aSource).Beta;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcWSKrakenFutures_Options.Create;
begin
  inherited;
  Demo := false;
end;

procedure TsgcWSKrakenFutures_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSKrakenFutures_Options then
  begin
    Demo := TsgcWSKrakenFutures_Options(aSource).Demo;
  end
  else
    inherited Assign(aSource);
end;

{$ENDIF}

end.
