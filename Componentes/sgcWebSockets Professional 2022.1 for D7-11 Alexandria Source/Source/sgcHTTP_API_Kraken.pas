{ ***************************************************************************
  sgcHTTP API component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcHTTP_API_Kraken;

interface

{$I sgcVer.inc}
{$IFDEF SGC_HTTP}

uses
  Classes, SysUtils,
  // sgc
  sgcHTTP_API;

type

  TsgcHTTPKrakenInterval = (kinh1min, kinh5min, kinh15min, kinh30min, kinh60min,
    kinh240min, kinh1440min, kinh10080min, kinh21600min);

  TsgcHTTPKrakenOrderType = (kothNone, kothMarket, kothLimit, kothStopLoss, kothTakeProfit,
    kothStopLossProfit, kothStopLossProfitLimit, kothStopLossLimit,
    kothTakeProfitLimit, kothTrailingStop, kothTrailingStopLimit,
    kothStopLossAndLimit);
  TsgcHTTPKrakenOrderSide = (kosNone, koshBuy, koshSell);

  TsgcHTTPKrakenOrder = class
  private
    FClose_OrderType: TsgcHTTPKrakenOrderType;
    FClose_Price: Extended;
    FClose_Price2: Extended;
    FExpireTm: String;
    FLeverage: String;
    FOFlags: String;
    FOrderType: TsgcHTTPKrakenOrderType;
    FPair: String;
    FPrice: Extended;
    FPrice2: Extended;
    FStartTm: String;
    FUserRef: String;
    FValidate: Boolean;
    FVolume: Extended;
    F_Type: TsgcHTTPKrakenOrderSide;
  public
    constructor Create; virtual;
  public
    property Pair: String read FPair write FPair;
    property _Type: TsgcHTTPKrakenOrderSide read F_Type write F_Type;
    property OrderType: TsgcHTTPKrakenOrderType read FOrderType write FOrderType;
    property Price: Extended read FPrice write FPrice;
    property Price2: Extended read FPrice2 write FPrice2;
    property Volume: Extended read FVolume write FVolume;
    property Leverage: String read FLeverage write FLeverage;
    property OFlags: String read FOFlags write FOFlags;
    property StartTm: String read FStartTm write FStartTm;
    property ExpireTm: String read FExpireTm write FExpireTm;
    property UserRef: String read FUserRef write FUserRef;
    property Validate: Boolean read FValidate write FValidate;
    property Close_OrderType: TsgcHTTPKrakenOrderType read FClose_OrderType write
        FClose_OrderType;
    property Close_Price: Extended read FClose_Price write FClose_Price;
    property Close_Price2: Extended read FClose_Price2 write FClose_Price2;
  end;

  TsgcHTTPKrakenFuturesOrderType = (kotfNone, kotfLMT, kotfPOST, kotfMKT, kotfSTP, kotfTAKE_PROFIT, kotfIOC);
  TsgcHTTPKrakenFuturesOrderSide = (kosfNone, kosfBuy, kosfSell);
  TsgcHTTPKrakenFuturesOrderTriggerSignal = (kotsNone, kotsMark, kotsIndex, kotsLast);

  TsgcHTTPKrakenFuturesOrder = class
  private
    FCliOrderId: string;
    FLimitPrice: Double;
    FOrderType: TsgcHTTPKrakenFuturesOrderType;
    FReduceOnly: Boolean;
    FSide: TsgcHTTPKrakenFuturesOrderSide;
    FSize: Integer;
    FStopPrice: Double;
    FSymbol: string;
    FTriggerSignal: TsgcHTTPKrakenFuturesOrderTriggerSignal;
  public
    property CliOrderId: string read FCliOrderId write FCliOrderId;
    property LimitPrice: Double read FLimitPrice write FLimitPrice;
    property OrderType: TsgcHTTPKrakenFuturesOrderType read FOrderType write
        FOrderType;
    property ReduceOnly: Boolean read FReduceOnly write FReduceOnly;
    property Side: TsgcHTTPKrakenFuturesOrderSide read FSide write FSide;
    property Size: Integer read FSize write FSize;
    property StopPrice: Double read FStopPrice write FStopPrice;
    property Symbol: string read FSymbol write FSymbol;
    property TriggerSignal: TsgcHTTPKrakenFuturesOrderTriggerSignal read
        FTriggerSignal write FTriggerSignal;
  end;

  TsgcHTTPKrakenLog_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FFileName: String;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property FileName: String read FFileName write FFileName;
  end;

  TsgcHTTPKraken_Options = class(TPersistent)
  private
    FApiKey: String;
    FApiSecret: string;
    FLogOptions: TsgcHTTPKrakenLog_Options;
    procedure SetLogOptions(const Value: TsgcHTTPKrakenLog_Options);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property ApiKey: String read FApiKey write FApiKey;
    property ApiSecret: string read FApiSecret write FApiSecret;
    property LogOptions: TsgcHTTPKrakenLog_Options read FLogOptions write
        SetLogOptions;
  end;

  TsgcHTTPKrakenFutures_Options = class(TsgcHTTPKraken_Options)
  private
    FDemo: Boolean;
  public
    constructor Create; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Demo: Boolean read FDemo write FDemo;
  end;


  TsgcHTTP_API_Kraken = class(TsgcHTTPAPI_Client)
  { helpers }
  private
    FFormatExtended: String;
    function GetFormatExtended(aValue: Extended): String;
  public
    property FormatExtended: String read FFormatExtended write FFormatExtended;
  { helpers }

    { http requests }
  private
    procedure DoInitializeLog; virtual; abstract;
  protected
    function GetEndpoint(const aMethod: String): string; virtual; abstract;
  protected
    function DoHTTP_GET(const aMethod: String; const aParameters: String = '';
        const aHeaders: TStrings = nil): string; virtual;
    function DoHTTP_POST(const aMethod: String; const aBody: String = ''; const
        aParameters: String = ''; const aHeaders: TStrings = nil): string; virtual;
    { http requests }
  end;

  TsgcHTTP_API_Kraken_Rest = class(TsgcHTTP_API_Kraken)
  { helpers }
  private
    function GetInterval(aInterval: TsgcHTTPKrakenInterval): Integer;
  protected
    procedure DoInitializeLog; override;
  { helpers }

  { endpoint }
  protected
    function GetEndpoint(const aMethod: String): string; override;
  { endpoint }

  { requests }
  protected
    function DoHTTP_POST_PRIVATE(const aMethod: String; const aBody: String = '';
        const aParameters: String = ''): string; virtual;
  { requests }

  { properties }
  private
    FKrakenOptions: TsgcHTTPKraken_Options;
    procedure SetKrakenOptions(const Value: TsgcHTTPKraken_Options);
  public
    property KrakenOptions: TsgcHTTPKraken_Options read FKrakenOptions write
        SetKrakenOptions;
  { properties }

  { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  { constructor / destructor }

  { websockets }
  public
    function GetWebSocketsToken: String;
  { websockets }

  { public methods }
  public
    function GetServerTime: String;
    function GetAssets(const aInfo: String = ''; const aClass: String = ''; const
        aAsset: String = ''): String;
    function GetAssetPairs(const aPairs: Array of String; const aInfo: String = '';
        const aLeverage: String = ''; const aFees: String = ''; const aMargin:
        String = ''): String;
    function GetTicker(const aPairs: Array of String): String;
    function GetOHLC(const aPair: String; aInterval: TsgcHTTPKrakenInterval =
        kinh1min; aSince: String = ''): String;
    function GetOrderBook(const aPair: String; aCount: Integer = 0): String;
    function GetTrades(const aPair: String; aSince: String = ''): String;
    function GetSpread(const aPair: String; aSince: String = ''): String;
  { public methods }

  { private methods }
  public
    function GetAccountBalance: String;
    function GetTradeBalance(const aClass: String = ''; const aAsset: String = ''):
        String;
    function GetOpenOrders(const aTrades: Boolean = False; const aUserRef: String =
        ''): String;
    function GetClosedOrders(const aTrades: Boolean = False; const aUserRef: String
        = ''; const aStart: String = ''; const aEnd: String = ''; const aOfs:
        String = ''; const aCloseTime: String = ''): String;
    function QueryOrders(const aTxId: String; const aTrades: Boolean = False; const
        aUserRef: String = ''): String;
    function GetTradesHistory(const aType: String = 'all'; const aTrades: Boolean =
        false; const aStart: String = ''; const aEnd: String = ''; const aOfs:
        String = ''): String;
    function QueryTrades(const aTxId: String; const aTrades: Boolean = False):
        String;
    function GetOpenPositions(const aTxId: String; const aDocalcs: Boolean = false;
        const aConsolidation: String = ''): String;
    function GetLedgers(const aClass: String = ''; const aAsset: String = ''; const
        aType: String = 'all'; const aStart: String = ''; const aEnd: String = '';
        const aOfs: String = ''): String;
    function QueryLedgers(const aId: String): String;
    function GetTradeVolume(const aPair: String = ''; const aFeeInfo: String = ''):
        String;
    function AddExport(const aDescription: String; const aReport: String =
        'trades'; const aFormat: String = 'CSV'; const aFields: String = 'all';
        const aAsset: String = 'all'; const aStartTm: String = ''; const aEndTm:
        String = ''): String;
    function ExportStatus(const aReport: String = 'trades'): String;
    function RetrieveExport(const aId: String): String;
    function RemoveExport(const aId: String; const aType: String = 'delete'):
        String;
  { private methods }

  { private trading }
  private
    function GetOrderType(aOrderType: TsgcHTTPKrakenOrderType): String;
    function GetOrderSide(aOrderSide: TsgcHTTPKrakenOrderSide): String;
  public
    function AddOrder(const aOrder: TsgcHTTPKrakenOrder): String;
    function CancelOrder(const aTxId: String): String;
  { private trading }
  end;

  TsgcHTTP_API_Kraken_Futures_Rest = class(TsgcHTTP_API_Kraken)
  { helpers }
  protected
    procedure DoInitializeLog; override;
  { helpers }

  { endpoint }
  protected
    function GetEndpoint(const aMethod: String): string; override;
  { endpoint }

  { requests }
    function DoHTTP_REQUEST_PRIVATE(const aRequest, aMethod: string; const
        aParameters: String = ''; const aBody: String = ''): string; virtual;
  protected
    function DoHTTP_GET_PRIVATE(const aMethod: String; const aParameters: String =
        ''): string; virtual;
    function DoHTTP_POST_PRIVATE(const aMethod: String; const aParameters: String =
        ''; const aBody: String = ''): string; virtual;
  { requests }

  { properties }
  private
    FKrakenOptions: TsgcHTTPKrakenFutures_Options;
    procedure SetKrakenOptions(const Value: TsgcHTTPKrakenFutures_Options);
  public
    property KrakenOptions: TsgcHTTPKrakenFutures_Options read FKrakenOptions write
        SetKrakenOptions;
  { properties }

  { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  { constructor / destructor }

  { public methods }
  public
    function GetFeeSchedules: string;
    function GetOrderBook(const aSymbol: string): string;
    function GetTickers: string;
    function GetInstruments: string;
    function GetHistory(const aSymbol: string; aLastTime: String = ''): string;
  { public methods }

  { private methods }
  private
    function GetOrderType(aOrderType: TsgcHTTPKrakenFuturesOrderType): string;
    function GetSide(aSide: TsgcHTTPKrakenFuturesOrderSide): string;
    function GetTriggerSignal(aTriggerSignal:
        TsgcHTTPKrakenFuturesOrderTriggerSignal): string;
  public
    function EditOrderByOrderId(const aOrderId: string; aSize: Integer = 0;
        aLimitPrice: Double = 0; aStopPrice: Double = 0): string;
    function EditOrderByCliOrderId(const aCliOrderId: string; aSize: Integer = 0;
        aLimitPrice: Double = 0; aStopPrice: Double = 0): string;
    function SendOrder(const aOrder: TsgcHTTPKrakenFuturesOrder): string;
    function SendMarketOrder(aSide: TsgcHTTPKrakenFuturesOrderSide; const aSymbol:
        string; aSize: Integer): string;
    function SendLimitOrder(aSide: TsgcHTTPKrakenFuturesOrderSide; const aSymbol:
        string; aSize: Integer; aLimitPrice: Double): string;
    function SendStopOrder(aSide: TsgcHTTPKrakenFuturesOrderSide; const aSymbol:
        string; aSize: Integer; aStopPrice, aLimitPrice: Double): string;
    function SendTakeProfit(aSide: TsgcHTTPKrakenFuturesOrderSide; const aSymbol:
        string; aSize: Integer; aStopPrice, aLimitPrice: Double): string;
    function CancelOrderByOrderId(const aOrderId: string): string;
    function CancelOrderByCliOrderId(const aCliOrderId: string): string;
    function GetFills(const aLastFillDate: string = ''): string;
    function Transfer(const aFromAccount, aToAccount, aUnit: string; aAmount:
        Double): string;
    function GetOpenPositions: string;
    function GetNotifications: string;
    function GetAccounts: string;
    function CancelAllOrders(const aSymbol: string = ''): string;
    function CancelAllOrdersAfter(aTimeout: Integer = 0): string;
    function GetOpenOrders: string;
    function GetHistoricalOrders(aSince: TDateTime; aBefore: TDateTime = 0; aSort:
        string = 'desc'; aContinuationToken: string = ''): string;
    function GetHistoricalTriggers(aSince: TDateTime; aBefore: TDateTime = 0;
        aSort: string = 'desc'; aContinuationToken: string = ''): string;
    function GetHistoricalExecutions(aSince: TDateTime; aBefore: TDateTime = 0;
        aSort: string = 'desc'; aContinuationToken: string = ''): string;
    function WithdrawalToSpotWallet(const aCurrency: string; aAmount: Double):
        string;
    function GetFeeScheduleVolumes: string;
    function GetAccountLogCSV: string;
  { private methods }
  end;


{$ENDIF}

implementation

{$IFDEF SGC_HTTP}

uses
  DateUtils, StrUtils,
  // sgc
  sgcWebSocket_Helpers, sgcBase_Helpers;


const
  CS_KRAKEN_ENDPOINT_BASE = 'https://api.kraken.com';
  CS_KRAKEN_FUTURES_ENDPOINT_BASE = 'https://futures.kraken.com/derivatives';
  CS_KRAKEN_FUTURES_DEMO_ENDPOINT_BASE = 'https://demo-futures.kraken.com/derivatives';
  CS_KRAKEN_DELIMITER = '&';
  CS_KRAKEN_GET = 'GET';
  CS_KRAKEN_POST = 'POST';

const
  CS_KRAKEN_ENDPOINT_GET_WEBSOCKETS_TOKEN = '/0/private/GetWebSocketsToken';

const
  CS_KRAKEN_ENDPOINT_GET_SERVER_TIME = '/0/public/Time';
  CS_KRAKEN_ENDPOINT_GET_ASSETS = '/0/public/Assets';
  CS_KRAKEN_ENDPOINT_GET_ASSET_PAIRS = '/0/public/AssetPairs';
  CS_KRAKEN_ENDPOINT_GET_TICKER = '/0/public/Ticker';
  CS_KRAKEN_ENDPOINT_GET_OHLC = '/0/public/OHLC';
  CS_KRAKEN_ENDPOINT_GET_ORDER_BOOK = '/0/public/Depth';
  CS_KRAKEN_ENDPOINT_GET_TRADES = '/0/public/Trades';
  CS_KRAKEN_ENDPOINT_GET_SPREAD = '/0/public/Spread';

const
  CS_KRAKEN_ENDPOINT_GET_ACCOUNT_BALANCE = '/0/private/Balance';
  CS_KRAKEN_ENDPOINT_GET_TRADE_BALANCE = '/0/private/TradeBalance';
  CS_KRAKEN_ENDPOINT_GET_OPEN_ORDERS = '/0/private/OpenOrders';
  CS_KRAKEN_ENDPOINT_GET_CLOSED_ORDERS = '/0/private/ClosedOrders';
  CS_KRAKEN_ENDPOINT_GET_QUERY_ORDERS = '/0/private/QueryOrders';
  CS_KRAKEN_ENDPOINT_QUERY_ORDERS = '/0/private/QueryOrders';
  CS_KRAKEN_ENDPOINT_GET_TRADES_HISTORY = '/0/private/TradesHistory';
  CS_KRAKEN_ENDPOINT_QUERY_TRADES = '/0/private/QueryTrades';
  CS_KRAKEN_ENDPOINT_GET_OPEN_POSITIONS = '/0/private/OpenPositions';
  CS_KRAKEN_ENDPOINT_GET_LEDGERS = '/0/private/Ledgers';
  CS_KRAKEN_ENDPOINT_QUERY_LEDGERS = '/0/private/QueryLedgers';
  CS_KRAKEN_ENDPOINT_GET_TRADE_VOLUME = '/0/private/TradeVolume';
  CS_KRAKEN_ENDPOINT_ADD_EXPORT = '/0/private/AddExport';
  CS_KRAKEN_ENDPOINT_EXPORT_STATUS = '/0/private/ExportStatus';
  CS_KRAKEN_ENDPOINT_RETRIEVE_EXPORT = '/0/private/RetrieveExport';
  CS_KRAKEN_ENDPOINT_REMOVE_EXPORT = '/0/private/RemoveExport';

const
  CS_KRAKEN_ENDPOINT_ADD_ORDER = '/0/private/AddOrder';
  CS_KRAKEN_ENDPOINT_CANCEL_ORDER = '/0/private/CancelOrder';

const
  CS_KRAKEN_FUTURES_ENDPOINT_GET_FEE_SCHEDULES = '/api/v3/feeschedules';
  CS_KRAKEN_FUTURES_ENDPOINT_GET_ORDER_BOOK = '/api/v3/orderbook';
  CS_KRAKEN_FUTURES_ENDPOINT_GET_TICKERS = '/api/v3/tickers';
  CS_KRAKEN_FUTURES_ENDPOINT_GET_INSTRUMENTS = '/api/v3/instruments';
  CS_KRAKEN_FUTURES_ENDPOINT_GET_HISTORY = '/api/v3/history';

const
  CS_KRAKEN_FUTURES_ENDPOINT_EDIT_ORDER = '/api/v3/editorder';
  CS_KRAKEN_FUTURES_ENDPOINT_SEND_ORDER = '/api/v3/sendorder';
  CS_KRAKEN_FUTURES_ENDPOINT_CANCEL_ORDER = '/api/v3/cancelorder';
  CS_KRAKEN_FUTURES_ENDPOINT_GET_FILLS = '/api/v3/fills';
  CS_KRAKEN_FUTURES_ENDPOINT_TRANSFER = '/api/v3/transfer';
  CS_KRAKEN_FUTURES_ENDPOINT_GET_OPEN_POSITIONS = '/api/v3/openpositions';
  CS_KRAKEN_FUTURES_ENDPOINT_GET_NOTIFICATIONS = '/api/v3/notifications';
  CS_KRAKEN_FUTURES_ENDPOINT_GET_ACCOUNTS = '/api/v3/accounts';
  CS_KRAKEN_FUTURES_ENDPOINT_CANCEL_ALL_ORDERS = '/api/v3/cancelallorders';
  CS_KRAKEN_FUTURES_ENDPOINT_CANCEL_ALL_ORDERS_AFTER = '/api/v3/cancelallordersafter';
  CS_KRAKEN_FUTURES_ENDPOINT_GET_OPEN_ORDERS = '/api/v3/openorders';
  CS_KRAKEN_FUTURES_ENDPOINT_GET_HISTORICAL_ORDERS = '/api/history/v2/orders';
  CS_KRAKEN_FUTURES_ENDPOINT_GET_HISTORICAL_TRIGGERS = '/api/history/v2/triggers';
  CS_KRAKEN_FUTURES_ENDPOINT_GET_HISTORICAL_EXECUTIONS = '/api/history/v2/executions';
  CS_KRAKEN_FUTURES_ENDPOINT_GET_RECENT_ORDERS = '/api/v3/recentorders';
  CS_KRAKEN_FUTURES_ENDPOINT_WITHDRAWAL_TO_SPOT_WALLET = '/api/v3/withdrawal';
  CS_KRAKEN_FUTURES_ENDPOINT_GET_FEE_SCHEDULE_VOLUMES = '/api/v3/feeschedules/volumes';
  CS_KRAKEN_FUTURES_ENDPOINT_GET_ACCOUNT_LOG_CSV = '/api/history/v2/accountlogcsv';

procedure TsgcHTTPKrakenLog_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTPKrakenLog_Options then
  begin
    Enabled := TsgcHTTPKrakenLog_Options(aSource).Enabled;
    FileName := TsgcHTTPKrakenLog_Options(aSource).FileName;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcHTTPKraken_Options.Create;
begin
  inherited;
  FLogOptions := TsgcHTTPKrakenLog_Options.Create;
end;

destructor TsgcHTTPKraken_Options.Destroy;
begin
  sgcFree(FLogOptions);
  inherited;
end;

procedure TsgcHTTPKraken_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTPKraken_Options then
  begin
    ApiKey := TsgcHTTPKraken_Options(aSource).ApiKey;
    ApiSecret := TsgcHTTPKraken_Options(aSource).ApiSecret;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcHTTPKraken_Options.SetLogOptions(const Value:
    TsgcHTTPKrakenLog_Options);
begin
  if Assigned(FLogOptions) then
    FLogOptions.Assign(Value);
end;

function TsgcHTTP_API_Kraken.DoHTTP_GET(const aMethod: String; const
    aParameters: String = ''; const aHeaders: TStrings = nil): string;
var
  vURL: String;
begin
  DoInitializeLog;

  vURL := GetEndpoint(aMethod) + aMethod;
  if aParameters <> '' then
    vURL := vURL + '?' + aParameters;
  Result := Get(vURL, aHeaders);
end;

function TsgcHTTP_API_Kraken.DoHTTP_POST(const aMethod: String; const aBody:
    String = ''; const aParameters: String = ''; const aHeaders: TStrings =
    nil): string;
var
  vURL: String;
begin
  DoInitializeLog;

  vURL := GetEndpoint(aMethod) + aMethod;
  if aParameters <> '' then
    vURL := vURL + '?' + aParameters;
  Result := Post(vURL, aBody, aHeaders);
end;

constructor TsgcHTTP_API_Kraken_Rest.Create(aOwner: TComponent);
begin
  inherited;
  FKrakenOptions := TsgcHTTPKraken_Options.Create;
  FFormatExtended := '0.0000000000';
end;

destructor TsgcHTTP_API_Kraken_Rest.Destroy;
begin
  sgcFree(FKrakenOptions);
  inherited;
end;

function TsgcHTTP_API_Kraken_Rest.AddExport(const aDescription: String; const
    aReport: String = 'trades'; const aFormat: String = 'CSV'; const aFields:
    String = 'all'; const aAsset: String = 'all'; const aStartTm: String = '';
    const aEndTm: String = ''): String;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    oParameters.Add('description=' + aDescription);
    if aReport <> '' then
      oParameters.Add('report=' + aReport);
    if aFormat <> '' then
      oParameters.Add('format=' + aFormat);
    if aFields <> '' then
      oParameters.Add('fields=' + aFields);
    if aAsset <> '' then
      oParameters.Add('asset=' + aAsset);
    if aStartTm <> '' then
      oParameters.Add('starttm=' + aStartTm);
    if aEndTm <> '' then
      oParameters.Add('endtm=' + aEndTm);
    Result := DoHTTP_POST_PRIVATE(CS_KRAKEN_ENDPOINT_ADD_EXPORT, '', oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken_Rest.AddOrder(const aOrder: TsgcHTTPKrakenOrder):
    String;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    oParameters.Add('ordertype=' + GetOrderType(aOrder.OrderType));
    oParameters.Add('type=' + GetOrderSide(aOrder._Type));
    oParameters.Add('pair=' + aOrder.Pair);
    if aOrder.Price <> 0 then
      oParameters.Add('price=' + GetFormatExtended(aOrder.Price));
    if aOrder.Price2 <> 0 then
      oParameters.Add('price2=' + GetFormatExtended(aOrder.Price2));
    oParameters.Add('volume=' + GetFormatExtended(aOrder.Volume));
    if aOrder.Leverage <> '' then
      oParameters.Add('leverage=' + aOrder.Leverage);
    if aOrder.OFlags <> '' then
      oParameters.Add('oflags=' + aOrder.OFlags);
    if aOrder.StartTm <> '' then
      oParameters.Add('starttm=' + aOrder.StartTm);
    if aOrder.ExpireTm <> '' then
      oParameters.Add('expiretm=' + aOrder.ExpireTm);
    if aOrder.UserRef <> '' then
      oParameters.Add('userref=' + aOrder.UserRef);
    if aOrder.Validate then
      oParameters.Add('validate=true');
    if aOrder.Close_OrderType <> kothNone then
      oParameters.Add('close[ordertype]=' + GetOrderType(aOrder.Close_OrderType));
    if aOrder.Close_Price <> 0 then
      oParameters.Add('close[price]=' + GetFormatExtended(aOrder.Close_Price));
    if aOrder.Close_Price2 <> 0 then
      oParameters.Add('close[price2]=' + GetFormatExtended(aOrder.Close_Price2));
    Result := DoHTTP_POST_PRIVATE(CS_KRAKEN_ENDPOINT_ADD_ORDER, '', oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken_Rest.CancelOrder(const aTxId: String): String;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    oParameters.Add('txid=' + aTxId);
    Result := DoHTTP_POST_PRIVATE(CS_KRAKEN_ENDPOINT_CANCEL_ORDER, '', oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken_Rest.DoHTTP_POST_PRIVATE(const aMethod: String;
    const aBody: String = ''; const aParameters: String = ''): string;
var
  vNonce, vBody, vSignature: String;
  oBytes, oBytesMethod, oBytesSHA256, oSecret: TBytes;
  i: Integer;
  j: Integer;
  oHeaders: TStringList;
  vURL: String;
begin
  // ... nonce
  vNonce := IntToStr(DateToMilliseconds(Now));
  if aParameters <> '' then
    vBody := 'nonce=' + vNonce + CS_KRAKEN_DELIMITER + aParameters
  else
    vBody := 'nonce=' + vNonce;

  // ... method to bytes
  oBytesMethod := TBytes(StringTosgcArrayOfBytes(aMethod));
  // ... sha256 nonce + body
  oBytesSHA256 := GetHashSHA256(vNonce + vBody);
  // ... create 1 array with previous bytes
  SetLength(oBytes, Length(oBytesMethod) + Length(oBytesSHA256));
  for i := 0 to Length(oBytesMethod) - 1 do
    oBytes[i] := oBytesMethod[i];
  i := Length(oBytesMethod);
  for j := 0 to Length(oBytesSHA256) - 1 do
    oBytes[i + j] := oBytesSHA256[j];

  // ... get decoded secret
  DecodeBase64(KrakenOptions.ApiSecret, oSecret);
  // ... get signature
  vSignature := GetHMACSHA512(oBytes, oSecret, True);
  // ... call post
  oHeaders := TStringList.Create;
  Try
    oHeaders.Add('API-Key: ' + KrakenOptions.ApiKey);
    oHeaders.Add('API-Sign: ' + vSignature);
    vURL := aMethod;
    Result := DoHTTP_POST(aMethod, vBody, '', oHeaders);
  Finally
    sgcFree(oHeaders);
  End;
end;

procedure TsgcHTTP_API_Kraken_Rest.DoInitializeLog;
begin
  inherited;
  if Log <> KrakenOptions.LogOptions.Enabled then
  begin
    LogFileName := KrakenOptions.LogOptions.FileName;
    Log := KrakenOptions.LogOptions.Enabled;
  end;
end;

function TsgcHTTP_API_Kraken_Rest.ExportStatus(const aReport: String =
    'trades'): String;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    if aReport <> '' then
      oParameters.Add('report=' + aReport);
    Result := DoHTTP_POST_PRIVATE(CS_KRAKEN_ENDPOINT_EXPORT_STATUS, '', oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken_Rest.GetAssets(const aInfo: String = ''; const
    aClass: String = ''; const aAsset: String = ''): String;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    if aInfo <> '' then
      oParameters.Add('info=' + aInfo);
    if aClass <> '' then
      oParameters.Add('class=' + aClass);
    if aAsset <> '' then
      oParameters.Add('asset=' + aAsset);
    Result := DoHTTP_GET(CS_KRAKEN_ENDPOINT_GET_ASSETS, oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken_Rest.GetInterval(aInterval:
    TsgcHTTPKrakenInterval): Integer;
begin
  case aInterval of
    kinh1min:
      Result := 1;
    kinh5min:
      Result := 5;
    kinh15min:
      Result := 15;
    kinh30min:
      Result := 30;
    kinh60min:
      Result := 60;
    kinh240min:
      Result := 240;
    kinh1440min:
      Result := 1440;
    kinh10080min:
      Result := 10080;
    kinh21600min:
      Result := 21600;
  else
    Result := 0;
  end;
end;

function TsgcHTTP_API_Kraken_Rest.GetOHLC(const aPair: String; aInterval:
    TsgcHTTPKrakenInterval = kinh1min; aSince: String = ''): String;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    oParameters.Add('pair=' + aPair);
    if aInterval <> kinh1min then
      oParameters.Add('interval=' + IntToStr(GetInterval(aInterval)));
    if aSince <> '' then
      oParameters.Add('since=' + aSince);
    Result := DoHTTP_GET(CS_KRAKEN_ENDPOINT_GET_OHLC, oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken_Rest.GetAssetPairs(const aPairs: Array of String;
    const aInfo: String = ''; const aLeverage: String = ''; const aFees: String
    = ''; const aMargin: String = ''): String;
var
  i: Integer;
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    if aInfo <> '' then
      oParameters.Add('info=' + aInfo);
    if aLeverage <> '' then
      oParameters.Add('leverage=' + aLeverage);
    if aFees <> '' then
      oParameters.Add('fees=' + aFees);
    if aMargin <> '' then
      oParameters.Add('margin=' + aMargin);
    for i := Low(aPairs) to High(aPairs) do
      oParameters.Add('pair=' + aPairs[i]);
    Result := DoHTTP_GET(CS_KRAKEN_ENDPOINT_GET_ASSET_PAIRS, oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken_Rest.GetAccountBalance: String;
begin
  Result := DoHTTP_POST_PRIVATE(CS_KRAKEN_ENDPOINT_GET_ACCOUNT_BALANCE);
end;

function TsgcHTTP_API_Kraken_Rest.GetOpenOrders(const aTrades: Boolean = False;
    const aUserRef: String = ''): String;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    if aTrades then
      oParameters.Add('trades=true');
    if aUserRef <> '' then
      oParameters.Add('userref=' + aUserRef);
    Result := DoHTTP_POST_PRIVATE(CS_KRAKEN_ENDPOINT_GET_OPEN_ORDERS, '', oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken_Rest.GetClosedOrders(const aTrades: Boolean =
    False; const aUserRef: String = ''; const aStart: String = ''; const aEnd:
    String = ''; const aOfs: String = ''; const aCloseTime: String = ''):
    String;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    if aTrades then
      oParameters.Add('trades=true');
    if aUserRef <> '' then
      oParameters.Add('userref=' + aUserRef);
    if aStart <> '' then
      oParameters.Add('start=' + aStart);
    if aEnd <> '' then
      oParameters.Add('end=' + aEnd);
    if aOfs <> '' then
      oParameters.Add('ofs=' + aOfs);
    if aCloseTime <> '' then
      oParameters.Add('closetime=' + aCloseTime);
    Result := DoHTTP_POST_PRIVATE(CS_KRAKEN_ENDPOINT_GET_CLOSED_ORDERS, '', oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken.GetFormatExtended(aValue: Extended): String;
var
  vFS: TFormatSettings;
begin
  vFS.DecimalSeparator := '.';
  vFS.ThousandSeparator := ',';
  Result := FormatCurr(FormatExtended, aValue, vFS);
end;

function TsgcHTTP_API_Kraken_Rest.GetEndpoint(const aMethod: String): string;
begin
  Result := CS_KRAKEN_ENDPOINT_BASE;
end;

function TsgcHTTP_API_Kraken_Rest.GetLedgers(const aClass: String = ''; const
    aAsset: String = ''; const aType: String = 'all'; const aStart: String =
    ''; const aEnd: String = ''; const aOfs: String = ''): String;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    if aClass <> '' then
      oParameters.Add('class=' + aClass);
    if aAsset <> '' then
      oParameters.Add('asset=' + aAsset);
    if aType <> '' then
      oParameters.Add('type=' + aType);
    if aStart <> '' then
      oParameters.Add('start=' + aStart);
    if aEnd <> '' then
      oParameters.Add('end=' + aEnd);
    if aOfs <> '' then
      oParameters.Add('ofs=' + aOfs);
    Result := DoHTTP_POST_PRIVATE(CS_KRAKEN_ENDPOINT_GET_LEDGERS, '', oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken_Rest.GetOpenPositions(const aTxId: String; const
    aDocalcs: Boolean = false; const aConsolidation: String = ''): String;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    oParameters.Add('txid=' + aTxId);
    if aDocalcs then
      oParameters.Add('docalcs=true');
    if aConsolidation <> '' then
      oParameters.Add('consolidation=' + aConsolidation);
    Result := DoHTTP_POST_PRIVATE(CS_KRAKEN_ENDPOINT_GET_OPEN_POSITIONS, '', oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken_Rest.GetTradeBalance(const aClass: String = '';
    const aAsset: String = ''): String;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    if aClass <> '' then
      oParameters.Add('aclass=' + aclass);
    if aAsset <> '' then
      oParameters.Add('asset=' + aAsset);
    Result := DoHTTP_POST_PRIVATE(CS_KRAKEN_ENDPOINT_GET_TRADE_BALANCE, '', oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken_Rest.GetOrderBook(const aPair: String; aCount:
    Integer = 0): String;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    oParameters.Add('pair=' + aPair);
    if aCount > 0 then
      oParameters.Add('count=' + IntToStr(aCount));
    Result := DoHTTP_GET(CS_KRAKEN_ENDPOINT_GET_ORDER_BOOK, oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken_Rest.GetOrderSide(aOrderSide:
    TsgcHTTPKrakenOrderSide): String;
begin
  case aOrderSide of
    koshBuy: result := 'buy';
    koshSell: result := 'sell';
    else
      result := '';
  end;
end;

function TsgcHTTP_API_Kraken_Rest.GetOrderType(aOrderType:
    TsgcHTTPKrakenOrderType): String;
begin
  case aOrderType of
    kothMarket: result := 'market';
    kothLimit: result := 'limit';
    kothStopLoss: result := 'stop_loss';
    kothTakeProfit: result := 'take_profit';
    kothStopLossProfit: result := 'stop_loss_profit';
    kothStopLossProfitLimit: result := 'stop_loss_profit_limit';
    kothStopLossLimit: result := 'stop_loss_limit';
    kothTakeProfitLimit: result := 'take_profit_limit';
    kothTrailingStop: result := 'trailing_stop';
    kothTrailingStopLimit: result := 'trailing_stop_limit';
    kothStopLossAndLimit: result := 'stop_loss_and_limit';
    else
      result := '';
  end;
end;

function TsgcHTTP_API_Kraken_Rest.GetTrades(const aPair: String; aSince: String
    = ''): String;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    oParameters.Add('pair=' + aPair);
    if aSince <> '' then
      oParameters.Add('since=' + aSince);
    Result := DoHTTP_GET(CS_KRAKEN_ENDPOINT_GET_TRADES, oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken_Rest.GetServerTime: String;
begin
  Result := DoHTTP_GET(CS_KRAKEN_ENDPOINT_GET_SERVER_TIME);
end;

function TsgcHTTP_API_Kraken_Rest.GetTicker(const aPairs: Array of String):
    String;
var
  i: Integer;
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    for i := Low(aPairs) to High(aPairs) do
      oParameters.Add('pair=' + aPairs[i]);
    Result := DoHTTP_GET(CS_KRAKEN_ENDPOINT_GET_TICKER, oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken_Rest.GetSpread(const aPair: String; aSince: String
    = ''): String;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    oParameters.Add('pair=' + aPair);
    if aSince <> '' then
      oParameters.Add('since=' + aSince);
    Result := DoHTTP_GET(CS_KRAKEN_ENDPOINT_GET_SPREAD, oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken_Rest.GetTradesHistory(const aType: String = 'all';
    const aTrades: Boolean = false; const aStart: String = ''; const aEnd:
    String = ''; const aOfs: String = ''): String;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    if aType <> '' then
      oParameters.Add('type=' + aType);
    if aTrades then
      oParameters.Add('trades=true');
    if aStart <> '' then
      oParameters.Add('start=' + aStart);
    if aEnd <> '' then
      oParameters.Add('end=' + aEnd);
    if aOfs <> '' then
      oParameters.Add('ofs=' + aOfs);
    Result := DoHTTP_POST_PRIVATE(CS_KRAKEN_ENDPOINT_GET_TRADES_HISTORY, '', oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken_Rest.GetTradeVolume(const aPair: String = '';
    const aFeeInfo: String = ''): String;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    if aPair <> '' then
      oParameters.Add('pair=' + aPair);
    if aFeeInfo <> '' then
      oParameters.Add('fee-info=' + aFeeInfo);
    Result := DoHTTP_POST_PRIVATE(CS_KRAKEN_ENDPOINT_GET_TRADE_VOLUME, '', oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken_Rest.GetWebSocketsToken: String;
begin
  Result := DoHTTP_POST_PRIVATE(CS_KRAKEN_ENDPOINT_GET_WEBSOCKETS_TOKEN);
end;

function TsgcHTTP_API_Kraken_Rest.QueryLedgers(const aId: String): String;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    oParameters.Add('id=' + aId);
    Result := DoHTTP_POST_PRIVATE(CS_KRAKEN_ENDPOINT_QUERY_LEDGERS, '', oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken_Rest.QueryOrders(const aTxId: String; const
    aTrades: Boolean = False; const aUserRef: String = ''): String;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    oParameters.Add('txid=' + aTxId);
    if aTrades then
      oParameters.Add('trades=true');
    if aUserRef <> '' then
      oParameters.Add('userref=' + aUserRef);
    Result := DoHTTP_POST_PRIVATE(CS_KRAKEN_ENDPOINT_QUERY_ORDERS, '', oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken_Rest.QueryTrades(const aTxId: String; const
    aTrades: Boolean = False): String;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    oParameters.Add('txid=' + aTxId);
    if aTrades then
      oParameters.Add('trades=true');
    Result := DoHTTP_POST_PRIVATE(CS_KRAKEN_ENDPOINT_QUERY_TRADES, '', oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken_Rest.RemoveExport(const aId: String; const aType:
    String = 'delete'): String;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    oParameters.Add('type=' + aType);
    oParameters.Add('id=' + aId);
    Result := DoHTTP_POST_PRIVATE(CS_KRAKEN_ENDPOINT_REMOVE_EXPORT, '', oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken_Rest.RetrieveExport(const aId: String): String;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    oParameters.Add('id=' + aId);
    Result := DoHTTP_POST_PRIVATE(CS_KRAKEN_ENDPOINT_RETRIEVE_EXPORT, '', oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

procedure TsgcHTTP_API_Kraken_Rest.SetKrakenOptions(const Value:
    TsgcHTTPKraken_Options);
begin
  if Assigned(FKrakenOptions) then
    FKrakenOptions.Assign(Value);
end;

constructor TsgcHTTPKrakenOrder.Create;
begin
  inherited;
  FClose_OrderType := kothNone;
  FClose_Price := 0;
  FClose_Price2 := 0;
  FExpireTm := '';
  FLeverage := '';
  FOFlags := '';
  FOrderType := kothNone;
  FPair := '';
  FPrice := 0;
  FPrice2 := 0;
  FStartTm := '';
  FUserRef := '';
  FValidate := false;
  FVolume := 0;
  F_Type := kosNone;
end;

constructor TsgcHTTP_API_Kraken_Futures_Rest.Create(aOwner: TComponent);
begin
  inherited;
  FKrakenOptions := TsgcHTTPKrakenFutures_Options.Create;
end;

destructor TsgcHTTP_API_Kraken_Futures_Rest.Destroy;
begin
  sgcFree(FKrakenOptions);
  inherited;
end;

function TsgcHTTP_API_Kraken_Futures_Rest.CancelAllOrders(const aSymbol: string
    = ''): string;
var
  vParameters: string;
begin
  vParameters := '';
  if aSymbol <> '' then
    vParameters := 'symbol=' + aSymbol;
  Result := DoHTTP_GET_PRIVATE(CS_KRAKEN_FUTURES_ENDPOINT_CANCEL_ALL_ORDERS, vParameters);
end;

function TsgcHTTP_API_Kraken_Futures_Rest.CancelAllOrdersAfter(aTimeout:
    Integer = 0): string;
begin
  Result := DoHTTP_GET_PRIVATE(CS_KRAKEN_FUTURES_ENDPOINT_CANCEL_ALL_ORDERS_AFTER, 'timeout=' + IntToStr(aTimeout));
end;

function TsgcHTTP_API_Kraken_Futures_Rest.CancelOrderByCliOrderId(const
    aCliOrderId: string): string;
begin
  Result := DoHTTP_POST_PRIVATE(CS_KRAKEN_FUTURES_ENDPOINT_CANCEL_ORDER, 'cliOrdId=' + aCliOrderId);
end;

function TsgcHTTP_API_Kraken_Futures_Rest.CancelOrderByOrderId(const aOrderId:
    string): string;
begin
  Result := DoHTTP_POST_PRIVATE(CS_KRAKEN_FUTURES_ENDPOINT_CANCEL_ORDER, 'order_id=' + aOrderId);
end;

function TsgcHTTP_API_Kraken_Futures_Rest.DoHTTP_GET_PRIVATE(const aMethod:
    String; const aParameters: String = ''): string;
begin
  result := DoHTTP_REQUEST_PRIVATE(CS_KRAKEN_GET, aMethod, aParameters);
end;

function TsgcHTTP_API_Kraken_Futures_Rest.DoHTTP_POST_PRIVATE(const aMethod:
    String; const aParameters: String = ''; const aBody: String = ''): string;
begin
  result := DoHTTP_REQUEST_PRIVATE(CS_KRAKEN_GET, aMethod, aParameters, aBody);
end;

function TsgcHTTP_API_Kraken_Futures_Rest.DoHTTP_REQUEST_PRIVATE(const
    aRequest, aMethod: string; const aParameters: String = ''; const aBody:
    String = ''): string;
var
  vNonce, vMessage, vSignature: String;
  oBytesSHA256, oSecret: TBytes;
  oHeaders: TStringList;
begin
  // ... nonce
  vNonce := IntToStr(DateToMilliseconds(Now));
  // ... parameters + nonce + method
  vMessage := aParameters + vNonce + aMethod;
  // ... sha256 nonce + body
  oBytesSHA256 := GetHashSHA256(vMessage);
  // ... get decoded secret
  DecodeBase64(KrakenOptions.ApiSecret, oSecret);
  // ... get signature
  vSignature := GetHMACSHA512(oBytesSHA256, oSecret, True);
  // ... call post
  oHeaders := TStringList.Create;
  Try
    oHeaders.Add('APIKey: ' + KrakenOptions.ApiKey);
    oHeaders.Add('Nonce: ' + vNonce);
    oHeaders.Add('Authent: ' + vSignature);
    if aRequest = CS_KRAKEN_GET then
      Result := DoHTTP_GET(aMethod, aParameters, oHeaders)
    else if aRequest = CS_KRAKEN_POST then
      Result := DoHTTP_POST(aMethod, aParameters, aBody, oHeaders)
  Finally
    sgcFree(oHeaders);
  End;
end;

procedure TsgcHTTP_API_Kraken_Futures_Rest.DoInitializeLog;
begin
  inherited;
  if Log <> KrakenOptions.LogOptions.Enabled then
  begin
    LogFileName := KrakenOptions.LogOptions.FileName;
    Log := KrakenOptions.LogOptions.Enabled;
  end;
end;

function TsgcHTTP_API_Kraken_Futures_Rest.EditOrderByCliOrderId(const
    aCliOrderId: string; aSize: Integer = 0; aLimitPrice: Double = 0;
    aStopPrice: Double = 0): string;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    oParameters.Add('cliOrdId=' + aCliOrderId);
    if aSize > 0 then
      oParameters.Add('size=' + IntToStr(aSize));
    if aLimitPrice > 0 then
      oParameters.Add('limitPrice=' + GetFormatExtended(aLimitPrice));
    if aStopPrice > 0 then
      oParameters.Add('stopPrice=' + GetFormatExtended(aStopPrice));

    Result := DoHTTP_POST_PRIVATE(CS_KRAKEN_FUTURES_ENDPOINT_EDIT_ORDER, oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken_Futures_Rest.EditOrderByOrderId(const aOrderId:
    string; aSize: Integer = 0; aLimitPrice: Double = 0; aStopPrice: Double =
    0): string;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    oParameters.Add('aOrderId=' + aOrderId);
    if aSize > 0 then
      oParameters.Add('size=' + IntToStr(aSize));
    if aLimitPrice > 0 then
      oParameters.Add('limitPrice=' + GetFormatExtended(aLimitPrice));
    if aStopPrice > 0 then
      oParameters.Add('stopPrice=' + GetFormatExtended(aStopPrice));

    Result := DoHTTP_POST_PRIVATE(CS_KRAKEN_FUTURES_ENDPOINT_EDIT_ORDER, oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken_Futures_Rest.GetAccountLogCSV: string;
begin
  Result := DoHTTP_GET_PRIVATE(CS_KRAKEN_FUTURES_ENDPOINT_GET_ACCOUNT_LOG_CSV);
end;

function TsgcHTTP_API_Kraken_Futures_Rest.GetAccounts: string;
begin
  Result := DoHTTP_GET_PRIVATE(CS_KRAKEN_FUTURES_ENDPOINT_GET_ACCOUNTS);
end;

function TsgcHTTP_API_Kraken_Futures_Rest.GetEndpoint(const aMethod: String):
    string;
begin
  if KrakenOptions.Demo then
    Result := CS_KRAKEN_FUTURES_DEMO_ENDPOINT_BASE
  else
    Result := CS_KRAKEN_FUTURES_ENDPOINT_BASE;

  if LeftStr(aMethod, Length('/api/history/')) = '/api/history/' then
    Result := StringReplace(Result, '/derivatives', '', []);
end;

function TsgcHTTP_API_Kraken_Futures_Rest.GetFeeSchedules: string;
begin
  Result := DoHTTP_GET(CS_KRAKEN_FUTURES_ENDPOINT_GET_FEE_SCHEDULES);
end;

function TsgcHTTP_API_Kraken_Futures_Rest.GetFeeScheduleVolumes: string;
begin
  Result := DoHTTP_GET_PRIVATE(CS_KRAKEN_FUTURES_ENDPOINT_GET_FEE_SCHEDULE_VOLUMES);
end;

function TsgcHTTP_API_Kraken_Futures_Rest.GetFills(const aLastFillDate: string
    = ''): string;
var
  vParameters: string;
begin
  vParameters := '';
  if aLastFillDate <> '' then
    vParameters := 'lastFillTime=' + aLastFillDate;

  Result := DoHTTP_GET_PRIVATE(CS_KRAKEN_FUTURES_ENDPOINT_GET_FILLS, vParameters);
end;

function TsgcHTTP_API_Kraken_Futures_Rest.GetHistoricalExecutions(aSince:
    TDateTime; aBefore: TDateTime = 0; aSort: string = 'desc';
    aContinuationToken: string = ''): string;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    oParameters.Add('since=' + GetDateTimeUnix(aSince, False));
    if aBefore > 0 then
      oParameters.Add('before=' + GetDateTimeUnix(aBefore, False));
    if aSort <> '' then
      oParameters.Add('sort=' + aSort);
    if aContinuationToken <> '' then
      oParameters.Add('continuationToken=' + aContinuationToken);

    Result := DoHTTP_GET_PRIVATE(CS_KRAKEN_FUTURES_ENDPOINT_GET_HISTORICAL_EXECUTIONS, oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken_Futures_Rest.GetHistoricalOrders(aSince:
    TDateTime; aBefore: TDateTime = 0; aSort: string = 'desc';
    aContinuationToken: string = ''): string;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    oParameters.Add('since=' + GetDateTimeUnix(aSince, False));
    if aBefore > 0 then
      oParameters.Add('before=' + GetDateTimeUnix(aBefore, False));
    if aSort <> '' then
      oParameters.Add('sort=' + aSort);
    if aContinuationToken <> '' then
      oParameters.Add('continuationToken=' + aContinuationToken);

    Result := DoHTTP_GET_PRIVATE(CS_KRAKEN_FUTURES_ENDPOINT_GET_HISTORICAL_ORDERS, oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken_Futures_Rest.GetHistoricalTriggers(aSince:
    TDateTime; aBefore: TDateTime = 0; aSort: string = 'desc';
    aContinuationToken: string = ''): string;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    oParameters.Add('since=' + GetDateTimeUnix(aSince, False));
    if aBefore > 0 then
      oParameters.Add('before=' + GetDateTimeUnix(aBefore, False));
    if aSort <> '' then
      oParameters.Add('sort=' + aSort);
    if aContinuationToken <> '' then
      oParameters.Add('continuationToken=' + aContinuationToken);

    Result := DoHTTP_GET_PRIVATE(CS_KRAKEN_FUTURES_ENDPOINT_GET_HISTORICAL_TRIGGERS, oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken_Futures_Rest.GetHistory(const aSymbol: string;
    aLastTime: String = ''): string;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    oParameters.Add('symbol=' + aSymbol);
    if aLastTime <> '' then
      oParameters.Add('lastTime=' + aLastTime);
    Result := DoHTTP_GET(CS_KRAKEN_FUTURES_ENDPOINT_GET_HISTORY, oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken_Futures_Rest.GetInstruments: string;
begin
  Result := DoHTTP_GET(CS_KRAKEN_FUTURES_ENDPOINT_GET_INSTRUMENTS);
end;

function TsgcHTTP_API_Kraken_Futures_Rest.GetNotifications: string;
begin
  Result := DoHTTP_GET_PRIVATE(CS_KRAKEN_FUTURES_ENDPOINT_GET_NOTIFICATIONS);
end;

function TsgcHTTP_API_Kraken_Futures_Rest.GetOpenOrders: string;
begin
  Result := DoHTTP_GET_PRIVATE(CS_KRAKEN_FUTURES_ENDPOINT_GET_OPEN_ORDERS);
end;

function TsgcHTTP_API_Kraken_Futures_Rest.GetOpenPositions: string;
begin
  Result := DoHTTP_GET_PRIVATE(CS_KRAKEN_FUTURES_ENDPOINT_GET_OPEN_POSITIONS);
end;

function TsgcHTTP_API_Kraken_Futures_Rest.GetOrderBook(const aSymbol: string):
    string;
begin
  Result := DoHTTP_GET(CS_KRAKEN_FUTURES_ENDPOINT_GET_ORDER_BOOK, 'symbol=' + aSymbol);
end;

function TsgcHTTP_API_Kraken_Futures_Rest.GetOrderType(aOrderType:
    TsgcHTTPKrakenFuturesOrderType): string;
begin
  case aOrderType of
    kotfLMT: result := 'lmt';
    kotfPOST: result := 'post';
    kotfMKT: result := 'mkt';
    kotfSTP: result := 'stp';
    kotfTAKE_PROFIT: result := 'take_profit';
    kotfIOC: result := 'ioc';
    else
      result := '';
  end;
end;

function TsgcHTTP_API_Kraken_Futures_Rest.GetSide(aSide:
    TsgcHTTPKrakenFuturesOrderSide): string;
begin
  case aSide of
    kosfBuy: result := 'buy';
    kosfSell: result := 'sell';
    else
      result := '';
  end;
end;

function TsgcHTTP_API_Kraken_Futures_Rest.GetTickers: string;
begin
  Result := DoHTTP_GET(CS_KRAKEN_FUTURES_ENDPOINT_GET_TICKERS);
end;

function TsgcHTTP_API_Kraken_Futures_Rest.GetTriggerSignal(aTriggerSignal:
    TsgcHTTPKrakenFuturesOrderTriggerSignal): string;
begin
  case aTriggerSignal of
    kotsMark: result := 'mark';
    kotsIndex: result := 'index';
    kotsLast: result := 'last';
    else
      result := '';
  end;
end;

function TsgcHTTP_API_Kraken_Futures_Rest.SendMarketOrder(aSide:
    TsgcHTTPKrakenFuturesOrderSide; const aSymbol: string; aSize: Integer):
    string;
var
  oOrder: TsgcHTTPKrakenFuturesOrder;
begin
  oOrder := TsgcHTTPKrakenFuturesOrder.Create;
  Try
    oOrder.OrderType := kotfMKT;
    oOrder.Side := aSide;
    oOrder.Symbol := aSymbol;
    oOrder.Size := aSize;
    Result := SendOrder(oOrder);
  Finally
    sgcFree(oOrder);
  End;
end;

function TsgcHTTP_API_Kraken_Futures_Rest.SendLimitOrder(aSide:
    TsgcHTTPKrakenFuturesOrderSide; const aSymbol: string; aSize: Integer;
    aLimitPrice: Double): string;
var
  oOrder: TsgcHTTPKrakenFuturesOrder;
begin
  oOrder := TsgcHTTPKrakenFuturesOrder.Create;
  Try
    oOrder.OrderType := kotfLMT;
    oOrder.Side := aSide;
    oOrder.Symbol := aSymbol;
    oOrder.Size := aSize;
    oOrder.LimitPrice := aLimitPrice;
    Result := SendOrder(oOrder);
  Finally
    sgcFree(oOrder);
  End;
end;

function TsgcHTTP_API_Kraken_Futures_Rest.SendOrder(const aOrder:
    TsgcHTTPKrakenFuturesOrder): string;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    oParameters.Add('orderType=' + GetOrderType(aOrder.OrderType));
    oParameters.Add('symbol=' + aOrder.Symbol);
    oParameters.Add('side=' + GetSide(aOrder.Side));
    oParameters.Add('size=' + IntToStr(aOrder.Size));
    oParameters.Add('limitPrice=' + GetFormatExtended(aOrder.LimitPrice));
    if aOrder.StopPrice > 0 then
      oParameters.Add('stopPrice=' + GetFormatExtended(aOrder.StopPrice));
    if aOrder.TriggerSignal <> kotsNone then
      oParameters.Add('triggerSignal=' + GetTriggerSignal(aOrder.TriggerSignal));
    if aOrder.CliOrderId <> '' then
      oParameters.Add('cliOrdId=' + aOrder.CliOrderId);
    if aOrder.ReduceOnly then
      oParameters.Add('reduceOnly=true');

    Result := DoHTTP_POST_PRIVATE(CS_KRAKEN_FUTURES_ENDPOINT_SEND_ORDER, oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken_Futures_Rest.SendStopOrder(aSide:
    TsgcHTTPKrakenFuturesOrderSide; const aSymbol: string; aSize: Integer;
    aStopPrice, aLimitPrice: Double): string;
var
  oOrder: TsgcHTTPKrakenFuturesOrder;
begin
  oOrder := TsgcHTTPKrakenFuturesOrder.Create;
  Try
    oOrder.OrderType := kotfSTP;
    oOrder.Side := aSide;
    oOrder.Symbol := aSymbol;
    oOrder.Size := aSize;
    oOrder.LimitPrice := aLimitPrice;
    oOrder.StopPrice := aStopPrice;
    Result := SendOrder(oOrder);
  Finally
    sgcFree(oOrder);
  End;
end;

function TsgcHTTP_API_Kraken_Futures_Rest.SendTakeProfit(aSide:
    TsgcHTTPKrakenFuturesOrderSide; const aSymbol: string; aSize: Integer;
    aStopPrice, aLimitPrice: Double): string;
var
  oOrder: TsgcHTTPKrakenFuturesOrder;
begin
  oOrder := TsgcHTTPKrakenFuturesOrder.Create;
  Try
    oOrder.OrderType := kotfTAKE_PROFIT;
    oOrder.Side := aSide;
    oOrder.Symbol := aSymbol;
    oOrder.Size := aSize;
    oOrder.LimitPrice := aLimitPrice;
    oOrder.StopPrice := aStopPrice;
    Result := SendOrder(oOrder);
  Finally
    sgcFree(oOrder);
  End;
end;

procedure TsgcHTTP_API_Kraken_Futures_Rest.SetKrakenOptions(const Value:
    TsgcHTTPKrakenFutures_Options);
begin

end;

function TsgcHTTP_API_Kraken_Futures_Rest.Transfer(const aFromAccount,
    aToAccount, aUnit: string; aAmount: Double): string;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    oParameters.Add('fromAccount=' + aFromAccount);
    oParameters.Add('toAccount=' + aToAccount);
    oParameters.Add('unit=' + aUnit);
    oParameters.Add('amount=' + GetFormatExtended(aAmount));

    Result := DoHTTP_POST_PRIVATE(CS_KRAKEN_FUTURES_ENDPOINT_TRANSFER, oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Kraken_Futures_Rest.WithdrawalToSpotWallet(const
    aCurrency: string; aAmount: Double): string;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_KRAKEN_DELIMITER;
    oParameters.Add('Extended=' + aCurrency);
    oParameters.Add('amount=' + GetFormatExtended(aAmount));

    Result := DoHTTP_POST_PRIVATE(CS_KRAKEN_FUTURES_ENDPOINT_WITHDRAWAL_TO_SPOT_WALLET, oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

constructor TsgcHTTPKrakenFutures_Options.Create;
begin
  inherited;
  Demo := False;
end;

procedure TsgcHTTPKrakenFutures_Options.Assign(aSource: TPersistent);
begin
   Assign(aSource);
  if aSource is TsgcHTTPKrakenFutures_Options then
  begin
    Demo := TsgcHTTPKrakenFutures_Options(aSource).Demo;
  end;
end;

{$ENDIF}

end.
