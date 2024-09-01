{ ***************************************************************************
  sgcHTTP API component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcHTTP_API_Binance;

interface

{$I sgcVer.inc}
{$IFDEF SGC_HTTP}

uses
  Classes, SysUtils,
  // sgc
  sgcHTTP_API;

type

  TsgcHTTPBinanceChartIntervals = (bcih1m, bcih3m, bcih5m, bcih15m, bcih30m,
    bcih1h, bcih2h, bcih4h, bcih6h, bcih8h, bcih12h, bcih1d, bcih3d,
    bcih1w, bcih1Mo);

  TsgcHTTPBinanceFutOpenInterestPeriods = (oip5m, oip15m, oip30m, oip1h, oip2h,
    oip4h, oip6h, oip12h, oip1d);

  TsgcHTTPBinanceOrderSide = (bosNone, bosBuy, bosSell);
  TsgcHTTPBinanceFuturesContracts = (bfchUSDT, bfchCOIN);

  TsgcHTTPBinanceLog_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FFileName: String;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property FileName: String read FFileName write FFileName;
  end;

  TsgcHTTPBinance_Options = class(TPersistent)
  private
    FApiKey: String;
    FApiSecret: string;
    FBinanceUS: Boolean;
    FLogOptions: TsgcHTTPBinanceLog_Options;
    FTestNet: Boolean;
    procedure SetLogOptions(const Value: TsgcHTTPBinanceLog_Options);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property ApiKey: String read FApiKey write FApiKey;
    property ApiSecret: string read FApiSecret write FApiSecret;
    property BinanceUS: Boolean read FBinanceUS write FBinanceUS;
    property LogOptions: TsgcHTTPBinanceLog_Options read FLogOptions
      write SetLogOptions;
    property TestNet: Boolean read FTestNet write FTestNet;
  end;

  TsgcHTTP_API_Binance = class(TsgcHTTPAPI_Client)
    { endpoints }
  protected
    function GetEndpointBase: String; virtual; abstract;
    { endpoints }

    { helpers }
  private
    function GetIntervalString(aInterval
      : TsgcHTTPBinanceChartIntervals): String;
    function GetTimeStamp: Int64;
    { helpers }

    { http requests }
  private
    procedure DoInitializeLog;
  protected
    function DoHTTP_GET(const aMethod: String; const aParameters: String = '';
      const aHeader: String = ''): string; virtual;
    function DoHTTP_POST(const aMethod: String; const aParameters: String = '';
      const aHeader: String = ''): string; virtual;
    function DoHTTP_DELETE(const aMethod: String;
      const aParameters: String = ''; const aHeader: String = '')
      : string; virtual;
  protected
    function DoHTTP_GET_HMAC256(const aMethod: String;
      const aParameters: String = ''): string; virtual;
    function DoHTTP_POST_HMAC256(const aMethod: String;
      const aParameters: String = ''; aQueryString: Boolean = False)
      : string; virtual;
    function DoHTTP_DELETE_HMAC256(const aMethod: String;
      const aParameters: String = ''): string; virtual;
    { http requests }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { properties }
  private
    FBinanceOptions: TsgcHTTPBinance_Options;
    procedure SetBinanceOptions(const Value: TsgcHTTPBinance_Options);
  public
    property BinanceOptions: TsgcHTTPBinance_Options read FBinanceOptions
      write SetBinanceOptions;
    { properties }
  end;

  TsgcHTTP_API_Binance_Rest_Base = class(TsgcHTTP_API_Binance)
    { helpers }
  protected
    function GetOrderSideString(aValue: TsgcHTTPBinanceOrderSide): string;
    function GetDoubleString(aValue: Double): string; virtual;
    { helpers }

    { endpoints }
  protected
    function GetEndpointPing: String; virtual; abstract;
    function GetEndpointServerTime: String; virtual; abstract;
    function GetEndpointExchangeInformation: String; virtual; abstract;
  protected
    function GetEndpointOrderBook: String; virtual; abstract;
    function GetEndpointTrades: String; virtual; abstract;
    function GetEndpointAggregateTrades: String; virtual; abstract;
    function GetEndpointKLines: String; virtual; abstract;
    function GetEndpoint24HRTicker: String; virtual; abstract;
    function GetEndpointPriceTicker: String; virtual; abstract;
    function GetEndpointBookTicker: String; virtual; abstract;
    { endpoints }

    { general endpoints }
  public
    function Ping: Boolean;
    function GetServerTime: String;
    function GetExchangeInformation: String;
    { general endpoints }

    { market data endpoints }
  public
    function GetOrderBook(const aSymbol: String; aLimit: Integer = 100): string;
    function GetTrades(const aSymbol: String; aLimit: Integer = 500): string;
    function GetHistoricalTrades(const aSymbol: String; aLimit: Integer = 500;
      aFromId: Integer = 0): string;
    function GetAggregateTrades(const aSymbol: String; aFromId: Integer = 0;
        aStartTime: Int64 = 0; aEndTime: Int64 = 0; aLimit: Integer = 500): String;
    function GetKLines(const aSymbol: String; aInterval:
        TsgcHTTPBinanceChartIntervals; aStartTime: Int64 = 0; aEndTime: Int64 = 0;
        aLimit: Integer = 500): String;
    function Get24hrTicker(const aSymbol: String): string;
    function GetPriceTicker(const aSymbol: String = ''): string;
    function GetBookTicker(const aSymbol: String = ''): string;
    { market data endpoints }
  end;

  TsgcHTTP_API_Binance_Rest = class(TsgcHTTP_API_Binance_Rest_Base)
    { endpoints }
  protected
    function GetEndpointBase: String; override;
  protected
    function GetEndpointPing: String; override;
    function GetEndpointServerTime: String; override;
    function GetEndpointExchangeInformation: String; override;
  protected
    function GetEndpointOrderBook: String; override;
    function GetEndpointTrades: String; override;
    function GetEndpointAggregateTrades: String; override;
    function GetEndpointKLines: String; override;
    function GetEndpoint24HRTicker: String; override;
    function GetEndpointPriceTicker: String; override;
    function GetEndpointBookTicker: String; override;
    { endpoints }

    { market data endpoints }
  public
    function GetAveragePrice(const aSymbol: String): string;
    { market data endpoints }

    { user data }
  public
    function NewOrder(const aSymbol, aSide, aType: String;
      aTimeInForce: String = ''; aQuantity: Double = 0;
      aQuoteOrderQty: Double = 0; aPrice: Double = 0;
      aNewClientOrderId: String = ''; aStopPrice: Double = 0;
      aIcebergQty: Double = 0; aNewOrderRespType: String = ''): String;
    function PlaceMarketOrder(aSide: TsgcHTTPBinanceOrderSide;
      const aSymbol: string; aQuantity: Double): string;
    function PlaceLimitOrder(aSide: TsgcHTTPBinanceOrderSide;
      const aSymbol: string; aQuantity: Double; aLimitPrice: Double): string;
    function PlaceStopOrder(aSide: TsgcHTTPBinanceOrderSide; const aSymbol: string;
        aQuantity, aStopPrice, aLimitPrice: Double): string;
    function TestNewOrder(const aSymbol, aSide, aType: String;
      aTimeInForce: String = ''; aQuantity: Double = 0;
      aQuoteOrderQty: Double = 0; aPrice: Double = 0;
      aNewClientOrderId: String = ''; aStopPrice: Double = 0;
      aIcebergQty: Double = 0; aNewOrderRespType: String = ''): String;
    function QueryOrder(const aSymbol: String; aOrderId: Int64 = 0;
      const aOrigClientOrderId: String = ''): string;
    function CancelOrder(const aSymbol: String; aOrderId: Int64 = 0;
      const aOrigClientOrderId: String = ''): string;
    function CancelAllOpenOrders(const aSymbol: String): string;
    function GetOpenOrders(const aSymbol: String = ''): string;
    function GetAllOrders(const aSymbol: String; aOrderId: Int64 = 0; aStartTime:
        Int64 = 0; aEndTime: Int64 = 0; aLimit: Integer = 500): string;
    function NewOCO(const aSymbol, aSide: String;
      aQuantity, aPrice, aStopPrice: Double;
      const aListClientOrderId: String = ''; aLimitClientOrderId: string = '';
      aLimitIcebergQty: Double = 0; aStopClientOrderId: string = '';
      aStopLimitPrice: Double = 0; aStopIcebergQty: Double = 0;
      aStopLimitTimeInForce: String = '';
      aNewOrderRespType: String = ''): String;
    function CancelOCO(const aSymbol: String; aOrderListId: Integer = 0;
      const aListClientOrderId: String = '';
      aNewClientOrderId: String = ''): string;
    function QueryOCO(const aSymbol: String; aOrderListId: Integer = 0;
      const aOrigClientOrderId: String = ''): string;
    function GetAllOCO(aFromId: Integer = 0; aStartTime: Int64 = 0; aEndTime: Int64
        = 0; aLimit: Integer = 500): string;
    function GetOpenOCO: string;
    function GetAccountInformation: String;
    function GetAccountTradeList(const aSymbol: String; aStartTime: Int64 = 0;
        aEndTime: Int64 = 0; aFromId: Integer = 0; aLimit: Integer = 500): String;
    { user data }
  end;

  TsgcHTTP_API_Binance_Futures_Rest = class(TsgcHTTP_API_Binance_Rest_Base)
    { endpoints }
  protected
    function GetEndpointBase: String; override;
  protected
    function GetEndpointPing: String; override;
    function GetEndpointServerTime: String; override;
    function GetEndpointExchangeInformation: String; override;
  protected
    function GetEndpointOrderBook: String; override;
    function GetEndpointAggregateTrades: String; override;
    function GetEndpointTrades: String; override;
    function GetEndpointKLines: String; override;
    function GetEndpoint24HRTicker: String; override;
    function GetEndpointPriceTicker: String; override;
    function GetEndpointBookTicker: String; override;
    { endpoints }

    { market data endpoints }
  private
    function GetOpenInterestPeriod
      (aValue: TsgcHTTPBinanceFutOpenInterestPeriods): String;
  public
    function GetMarkPrice(const aSymbol: String = ''): string;
    function GetFundingRateHistory(const aSymbol: String; aStartTime: Int64 = 0;
        aEndTime: Int64 = 0; aLimit: Integer = 100): string;
    function GetOpenInterest(const aSymbol: String = ''): string;
    function GetOpenInterestStatistics(const aSymbol: String; const aPeriod:
        TsgcHTTPBinanceFutOpenInterestPeriods; aLimit: Integer = 30; aStartTime:
        Int64 = 0; aEndTime: Int64 = 0): string;
    function GetTopTraderAccountRatio(const aSymbol: String; const aPeriod:
        TsgcHTTPBinanceFutOpenInterestPeriods; aLimit: Integer = 30; aStartTime:
        Int64 = 0; aEndTime: Int64 = 0): string;
    function GetTopTraderPositionRatio(const aSymbol: String; const aPeriod:
        TsgcHTTPBinanceFutOpenInterestPeriods; aLimit: Integer = 30; aStartTime:
        Int64 = 0; aEndTime: Int64 = 0): string;
    function GetGlobalAccountRatio(const aSymbol: String; const aPeriod:
        TsgcHTTPBinanceFutOpenInterestPeriods; aLimit: Integer = 30; aStartTime:
        Int64 = 0; aEndTime: Int64 = 0): string;
    function GetTakerVolume(const aSymbol: String; const aPeriod:
        TsgcHTTPBinanceFutOpenInterestPeriods; aLimit: Integer = 30; aStartTime:
        Int64 = 0; aEndTime: Int64 = 0): string;
    { market data endpoints }

    { user data }
  public
    function ChangePositionMode(aDualPosition: Boolean): string;
    function GetCurrentPositionMode: string;
    function NewOrder(const aSymbol, aSide: String; aPositionSide: String;
      const aType: String; aTimeInForce: String = ''; aQuantity: Double = 0;
      aReduceOnly: string = ''; aPrice: Double = 0;
      aNewClientOrderId: String = ''; aStopPrice: Double = 0;
      aClosePosition: string = ''; aActivationPrice: Double = 0;
      aCallbackRate: Double = 0; aWorkingType: String = '';
      aNewOrderRespType: String = ''): String;
    function PlaceMarketOrder(aSide: TsgcHTTPBinanceOrderSide; const aSymbol:
        string; aQuantity: Double): string;
    function PlaceLimitOrder(aSide: TsgcHTTPBinanceOrderSide; const aSymbol:
        string; aQuantity: Double; aLimitPrice: Double): string;
    function PlaceStopOrder(aSide: TsgcHTTPBinanceOrderSide; const aSymbol: string;
        aQuantity, aStopPrice, aLimitPrice: Double): string;
    function PlaceTrailingStopOrder(aSide: TsgcHTTPBinanceOrderSide; const aSymbol:
        string; aQuantity, aActivationPrice, aCallbackRate: Double): string;
    function QueryOrder(const aSymbol: String; aOrderId: Int64 = 0;
      const aOrigClientOrderId: String = ''): string;
    function CancelOrder(const aSymbol: String; aOrderId: Int64 = 0;
      const aOrigClientOrderId: String = ''): string;
    function CancelAllOpenOrders(const aSymbol: String): string;
    function AutoCancelAllOpenOrders(const aSymbol: String;
      aCountdownTime: Integer): string;
    function QueryCurrentOpenOrder(const aSymbol: String; aOrderId: Int64 = 0;
      const aOrigClientOrderId: String = ''): string;
    function GetOpenOrders(const aSymbol: String = ''): string;
    function GetAllOrders(const aSymbol: String; aOrderId: Int64 = 0; aStartTime:
        Int64 = 0; aEndTime: Int64 = 0; aLimit: Integer = 100): string;
  public
    function GetAccountBalance: string;
    function GetAccountInformation: string;
    function ChangeInitialLeverage(const aSymbol: String;
      aLeverage: Integer): string;
    function ChangeMarginType(const aSymbol, aMarginType: String): string;
    function ModifyIsolatedPositionMargin(const aSymbol: String;
      aAmount: Double; aType: Integer; aPositionSide: String = ''): string;
    function GetPositionMarginChangeHistory(const aSymbol: String; aType: Integer =
        0; aStartTime: Int64 = 0; aEndTime: Int64 = 0; aLimit: Integer = 500):
        string;
    function GetPositionInformation(const aSymbol: String = ''): string;
    function GetAccountTradeList(const aSymbol: String; aStartTime: Int64 = 0;
        aEndTime: Int64 = 0; aFromId: Integer = 0; aLimit: Integer = 500): String;
    function GetIncomeHistory(const aSymbol: String = ''; const aIncomeType: String
        = ''; aStartTime: Int64 = 0; aEndTime: Int64 = 0; aLimit: Integer = 100):
        string;
    function GetNotionalLeverageBracket(const aSymbol: String = ''): string;
    { user data }

    { properties }
  private
    FFuturesContracts: TsgcHTTPBinanceFuturesContracts;
  public
    property FuturesContracts: TsgcHTTPBinanceFuturesContracts
      read FFuturesContracts write FFuturesContracts;
    { properties }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_HTTP}

uses
  DateUtils,
  // sgc
  sgcBase_Helpers;

resourcestring
  S_BINANCE_ERROR_NOT_SUPPORTED_BINANCE_US = 'Method Not supported by Binance.US';

// spot
const
  CS_BNC_ENDPOINT_BASE = 'https://api.binance.com';
  CS_BNC_US_ENDPOINT_BASE = 'https://api.binance.us';
  CS_BNC_TESTNET_ENDPOINT_BASE = 'https://testnet.binance.vision';

const
  CS_BNC_ENDPOINT_PING = '/api/v3/ping';
  CS_BNC_ENDPOINT_SERVER_TIME = '/api/v3/time';
  CS_BNC_ENDPOINT_EXCHANGE_INFORMATION = '/api/v3/exchangeInfo';

const
  CS_BNC_ENDPOINT_ORDER_BOOK = '/api/v3/depth';
  CS_BNC_ENDPOINT_TRADES = '/api/v3/trades';
  CS_BNC_ENDPOINT_AGGREGATE_TRADES = '/api/v3/aggTrades';
  CS_BNC_ENDPOINT_KLINES = '/api/v3/klines';
  CS_BNC_ENDPOINT_AVERAGE_PRICE = '/api/v3/avgPrice';
  CS_BNC_ENDPOINT_24HR_TICKER = '/api/v3/ticker/24hr';
  CS_BNC_ENDPOINT_PRICE_TICKER = '/api/v3/ticker/price';
  CS_BNC_ENDPOINT_BOOK_TICKER = '/api/v3/ticker/bookTicker';

const
  CS_BNC_ENDPOINT_NEW_ORDER = '/api/v3/order';
  CS_BNC_ENDPOINT_TEST_NEW_ORDER = '/api/v3/order/test';
  CS_BNC_ENDPOINT_QUERY_ORDER = '/api/v3/order';
  CS_BNC_ENDPOINT_CANCEL_ORDER = '/api/v3/order';
  CS_BNC_ENDPOINT_OPEN_ORDERS = '/api/v3/openOrders';
  CS_BNC_ENDPOINT_ALL_ORDERS = '/api/v3/allOrders';
  CS_BNC_ENDPOINT_NEW_OCO = '/api/v3/order/oco';
  CS_BNC_ENDPOINT_CANCEL_OCO = '/api/v3/orderList';
  CS_BNC_ENDPOINT_QUERY_OCO = '/api/v3/orderList';
  CS_BNC_ENDPOINT_ALL_OCO = '/api/v3/allOrderList';
  CS_BNC_ENDPOINT_OPEN_OCO = '/api/v3/openOrderList';
  CS_BNC_ENDPOINT_ACCOUNT_INFORMATION = '/api/v3/account';
  CS_BNC_ENDPOINT_ACCOUNT_TRADE_LIST = '/api/v3/myTrades';

  // futures
const
  CS_BNC_ENDPOINT_FUT_USDT_BASE = 'https://fapi.binance.com/fapi';
  CS_BNC_ENDPOINT_TESTNET_FUT_USDT_BASE = 'https://testnet.binancefuture.com/fapi';

  CS_BNC_ENDPOINT_FUT_COIN_BASE = 'https://dapi.binance.com/dapi';
  CS_BNC_ENDPOINT_TESTNET_FUT_COIN_BASE = 'https://testnet.binancefuture.com/dapi';

const
  CS_BNC_ENDPOINT_FUT_PING = '/v1/ping';
  CS_BNC_ENDPOINT_FUT_SERVER_TIME = '/v1/time';
  CS_BNC_ENDPOINT_FUT_EXCHANGE_INFORMATION = '/v1/exchangeInfo';

const
  CS_BNC_ENDPOINT_FUT_ORDER_BOOK = '/v1/depth';
  CS_BNC_ENDPOINT_FUT_TRADES = '/v1/trades';
  CS_BNC_ENDPOINT_FUT_AGGREGATE_TRADES = '/v1/aggTrades';
  CS_BNC_ENDPOINT_FUT_KLINES = '/v1/klines';
  CS_BNC_ENDPOINT_FUT_MARK_PRICE = '/v1/premiumIndex';
  CS_BNC_ENDPOINT_FUT_FUNDING_RATE_HISTORY = '/v1/fundingRate';
  CS_BNC_ENDPOINT_FUT_24HR_TICKER = '/v1/ticker/24hr';
  CS_BNC_ENDPOINT_FUT_PRICE_TICKER = '/v1/ticker/price';
  CS_BNC_ENDPOINT_FUT_BOOK_TICKER = '/v1/ticker/bookTicker';
  CS_BNC_ENDPOINT_FUT_OPEN_INTEREST = '/v1/openInterest';
  CS_BNC_ENDPOINT_FUT_OPEN_INTEREST_STATISTICS =
    '/futures/data/openInterestHist';
  CS_BNC_ENDPOINT_FUT_TOP_LONG_SHORT_ACCOUNT_RATIO =
    '/futures/data/topLongShortAccountRatio';
  CS_BNC_ENDPOINT_FUT_TOP_LONG_SHORT_POSITION_RATIO =
    '/futures/data/topLongShortPositionRatio';
  CS_BNC_ENDPOINT_FUT_GLOBAL_LONG_SHORT_ACCOUNT_RATIO =
    '/futures/data/globalLongShortAccountRatio';
  CS_BNC_ENDPOINT_FUT_TAKER_LONG_SHORT_RATIO =
    '/futures/data/takerlongshortRatio';

const
  CS_BNC_ENDPOINT_FUT_CHANGE_POSITION_MODE = '/v1/positionSide/dual';
  CS_BNC_ENDPOINT_FUT_CURRENT_POSITION_MODE = '/v1/positionSide/dual';
  CS_BNC_ENDPOINT_FUT_NEW_ORDER = '/v1/order';
  CS_BNC_ENDPOINT_FUT_QUERY_ORDER = '/v1/order';
  CS_BNC_ENDPOINT_FUT_CANCEL_ORDER = '/v1/order';
  CS_BNC_ENDPOINT_FUT_CANCEL_ALL_OPEN_ORDERS = '/v1/allOpenOrders';
  CS_BNC_ENDPOINT_FUT_AUTOCANCEL_ALL_OPEN_ORDERS = '/v1/allOpenOrders';
  CS_BNC_ENDPOINT_FUT_QUERY_CURRENT_OPEN_ORDER = '/v1/openOrder';
  CS_BNC_ENDPOINT_FUT_OPEN_ORDERS = '/v1/openOrders';
  CS_BNC_ENDPOINT_FUT_ALL_ORDERS = '/v1/allOrders';
  CS_BNC_ENDPOINT_FUT_USDT_ACCOUNT_BALANCE = '/v2/balance';
  CS_BNC_ENDPOINT_FUT_COIN_ACCOUNT_BALANCE = '/v1/balance';
  CS_BNC_ENDPOINT_FUT_USDT_ACCOUNT_INFORMATION = '/v2/account';
  CS_BNC_ENDPOINT_FUT_COIN_ACCOUNT_INFORMATION = '/v1/account';
  CS_BNC_ENDPOINT_FUT_CHANGE_INITIAL_LEVERAGE = '/v1/leverage';
  CS_BNC_ENDPOINT_FUT_CHANGE_MARGIN_TYPE = '/v1/marginType';
  CS_BNC_ENDPOINT_FUT_MODIFY_ISOLATED_POSITION_MARGIN = '/v1/positionMargin';
  CS_BNC_ENDPOINT_FUT_GET_POSITION_MARGIN_CHANGE_HISTORY =
    '/v1/positionMargin/history';
  CS_BNC_ENDPOINT_FUT_USDT_POSITION_INFORMATION = '/v2/positionRisk';
  CS_BNC_ENDPOINT_FUT_COIN_POSITION_INFORMATION = '/v1/positionRisk';
  CS_BNC_ENDPOINT_FUT_ACCOUNT_TRADE_LIST = '/v1/userTrades';
  CS_BNC_ENDPOINT_FUT_GET_INCOME_HISTORY = '/v1/income';
  CS_BNC_ENDPOINT_FUT_NOTIONAL_LEVERAGE_BRACKET = '/v1/leverageBracket';

const
  CS_BNC_MASK_CURRENCY = '0.00000000';

function TsgcHTTP_API_Binance_Rest_Base.Get24hrTicker(const aSymbol
  : String): string;
begin
  Result := DoHTTP_GET(GetEndpoint24HRTicker, Format('symbol=%s', [aSymbol]));
end;

function TsgcHTTP_API_Binance_Rest_Base.GetAggregateTrades(const aSymbol:
    String; aFromId: Integer = 0; aStartTime: Int64 = 0; aEndTime: Int64 = 0;
    aLimit: Integer = 500): String;
var
  vParameters: String;
begin
  vParameters := Format('symbol=%s&limit=%d', [aSymbol, aLimit]);
  if aFromId > 0 then
    vParameters := vParameters + '&fromId=' + IntToStr(aFromId);
  if aStartTime > 0 then
    vParameters := vParameters + '&startTime=' + IntToStr(aStartTime);
  if aEndTime > 0 then
    vParameters := vParameters + '&endTime=' + IntToStr(aEndTime);

  Result := DoHTTP_GET(GetEndpointAggregateTrades, vParameters);
end;

function TsgcHTTP_API_Binance_Rest_Base.GetBookTicker(const aSymbol
  : String = ''): string;
begin
  if aSymbol <> '' then
    Result := DoHTTP_GET(GetEndpointBookTicker, Format('symbol=%s', [aSymbol]))
  else
    Result := DoHTTP_GET(GetEndpointBookTicker);
end;

function TsgcHTTP_API_Binance_Rest_Base.GetDoubleString(aValue: Double): string;
var
  vFS: TFormatSettings;
begin
  vFS.DecimalSeparator := '.';
  vFS.ThousandSeparator := ',';
  Result := FormatFloat(CS_BNC_MASK_CURRENCY, aValue, vFS);
end;

function TsgcHTTP_API_Binance_Rest_Base.GetExchangeInformation: String;
begin
  Result := DoHTTP_GET(GetEndpointExchangeInformation);
end;

function TsgcHTTP_API_Binance_Rest_Base.GetHistoricalTrades
  (const aSymbol: String; aLimit: Integer = 500; aFromId: Integer = 0): string;
var
  vParameters: String;
begin
  if aFromId > 0 then
    vParameters := Format('symbol=%s&limit=%d&fromId=%d',
      [aSymbol, aLimit, aFromId])
  else
    vParameters := Format('symbol=%s&limit=%d', [aSymbol, aLimit]);
  Result := DoHTTP_GET(GetEndpointTrades, vParameters);
end;

function TsgcHTTP_API_Binance_Rest_Base.GetKLines(const aSymbol: String;
    aInterval: TsgcHTTPBinanceChartIntervals; aStartTime: Int64 = 0; aEndTime:
    Int64 = 0; aLimit: Integer = 500): String;
var
  vParameters: String;
begin
  vParameters := Format('symbol=%s&interval=%s&limit=%d',
    [aSymbol, GetIntervalString(aInterval), aLimit]);
  if aStartTime > 0 then
    vParameters := vParameters + '&startTime=' + IntToStr(aStartTime);
  if aEndTime > 0 then
    vParameters := vParameters + '&endTime=' + IntToStr(aEndTime);

  Result := DoHTTP_GET(GetEndpointKLines, vParameters);
end;

function TsgcHTTP_API_Binance_Rest_Base.GetOrderBook(const aSymbol: String;
  aLimit: Integer = 100): string;
begin
  Result := DoHTTP_GET(GetEndpointOrderBook, Format('symbol=%s&limit=%d',
    [aSymbol, aLimit]));
end;

function TsgcHTTP_API_Binance_Rest_Base.GetOrderSideString
  (aValue: TsgcHTTPBinanceOrderSide): string;
begin
  case aValue of
    bosBuy:
      Result := 'BUY';
    bosSell:
      Result := 'SELL';
  else
    Result := '';
  end;
end;

function TsgcHTTP_API_Binance_Rest_Base.GetPriceTicker(const aSymbol
  : String = ''): string;
begin
  if aSymbol <> '' then
    Result := DoHTTP_GET(GetEndpointPriceTicker, Format('symbol=%s', [aSymbol]))
  else
    Result := DoHTTP_GET(GetEndpointPriceTicker);
end;

function TsgcHTTP_API_Binance_Rest_Base.GetServerTime: String;
begin
  Result := DoHTTP_GET(GetEndpointServerTime);
end;

function TsgcHTTP_API_Binance_Rest_Base.GetTrades(const aSymbol: String;
  aLimit: Integer = 500): string;
begin
  Result := DoHTTP_GET(GetEndpointTrades, Format('symbol=%s&limit=%d',
    [aSymbol, aLimit]));
end;

function TsgcHTTP_API_Binance_Rest_Base.Ping: Boolean;
begin
  Result := DoHTTP_GET(GetEndpointPing) = '{}';
end;

constructor TsgcHTTP_API_Binance.Create(aOwner: TComponent);
begin
  inherited;
  FBinanceOptions := TsgcHTTPBinance_Options.Create;
end;

destructor TsgcHTTP_API_Binance.Destroy;
begin
  sgcFree(FBinanceOptions);
  inherited;
end;

function TsgcHTTP_API_Binance.DoHTTP_DELETE(const aMethod: String;
  const aParameters: String = ''; const aHeader: String = ''): string;
var
  vURL: String;
  oHeaders: TStringList;
begin
  DoInitializeLog;

  vURL := GetEndpointBase + aMethod;
  if aParameters <> '' then
    vURL := vURL + '?' + aParameters;
  oHeaders := TStringList.Create;
  Try
    if aHeader <> '' then
      oHeaders.Add(aHeader);
    Result := Delete(vURL, oHeaders);
  Finally
    sgcFree(oHeaders);
  End;
end;

function TsgcHTTP_API_Binance.DoHTTP_DELETE_HMAC256(const aMethod: String;
  const aParameters: String = ''): string;
var
  vParameters, vSignature: string;
begin
  vParameters := aParameters;
  vSignature := GetHMACSHA256(vParameters, BinanceOptions.ApiSecret);
  vParameters := vParameters + '&signature=' + vSignature;

  Result := DoHTTP_DELETE(aMethod, vParameters,
    'X-MBX-APIKEY: ' + BinanceOptions.ApiKey);
end;

function TsgcHTTP_API_Binance.DoHTTP_GET(const aMethod: String;
  const aParameters: String = ''; const aHeader: String = ''): string;
var
  oHeaders: TStringList;
  vURL: String;
begin
  DoInitializeLog;

  vURL := GetEndpointBase + aMethod;
  if aParameters <> '' then
    vURL := vURL + '?' + aParameters;
  oHeaders := TStringList.Create;
  Try
    if aHeader <> '' then
      oHeaders.Add(aHeader);
    Result := Get(vURL, oHeaders);
  Finally
    sgcFree(oHeaders);
  End;
end;

function TsgcHTTP_API_Binance.DoHTTP_GET_HMAC256(const aMethod: String;
  const aParameters: String = ''): string;
var
  vParameters, vSignature: string;
begin
  vParameters := aParameters;
  vSignature := GetHMACSHA256(vParameters, BinanceOptions.ApiSecret);
  vParameters := vParameters + '&signature=' + vSignature;

  Result := DoHTTP_GET(aMethod, vParameters, 'X-MBX-APIKEY: ' +
    BinanceOptions.ApiKey);
end;

function TsgcHTTP_API_Binance.DoHTTP_POST(const aMethod: String;
  const aParameters: String = ''; const aHeader: String = ''): string;
var
  oHeaders: TStringList;
begin
  DoInitializeLog;

  oHeaders := TStringList.Create;
  Try
    if aHeader <> '' then
      oHeaders.Add(aHeader);
    Result := Post(GetEndpointBase + aMethod, aParameters, oHeaders);
  Finally
    sgcFree(oHeaders);
  End;
end;

function TsgcHTTP_API_Binance.DoHTTP_POST_HMAC256(const aMethod: String;
  const aParameters: String = ''; aQueryString: Boolean = False): string;
var
  vParameters, vSignature: string;
begin
  vParameters := aParameters;
  vSignature := GetHMACSHA256(vParameters, BinanceOptions.ApiSecret);
  vParameters := vParameters + '&signature=' + vSignature;

  if aQueryString then
    Result := DoHTTP_POST(aMethod + '?' + vParameters, '',
      'X-MBX-APIKEY: ' + BinanceOptions.ApiKey)
  else
  begin
    Result := DoHTTP_POST(aMethod, vParameters,
      'X-MBX-APIKEY: ' + BinanceOptions.ApiKey);
  end;
end;

procedure TsgcHTTP_API_Binance.DoInitializeLog;
begin
  if Log <> BinanceOptions.LogOptions.Enabled then
  begin
    LogFileName := BinanceOptions.LogOptions.FileName;
    Log := BinanceOptions.LogOptions.Enabled;
  end;
end;

function TsgcHTTP_API_Binance.GetIntervalString
  (aInterval: TsgcHTTPBinanceChartIntervals): String;
begin
  Result := '';
  case aInterval of
    bcih1m:
      Result := '1m';
    bcih3m:
      Result := '3m';
    bcih5m:
      Result := '5m';
    bcih15m:
      Result := '15m';
    bcih30m:
      Result := '30m';
    bcih1h:
      Result := '1h';
    bcih2h:
      Result := '2h';
    bcih4h:
      Result := '4h';
    bcih6h:
      Result := '6h';
    bcih8h:
      Result := '8h';
    bcih12h:
      Result := '12h';
    bcih1d:
      Result := '1d';
    bcih3d:
      Result := '3d';
    bcih1w:
      Result := '1w';
    bcih1Mo:
      Result := '1M';
  end;
end;

function TsgcHTTP_API_Binance.GetTimeStamp: Int64;
begin
{$IFDEF LAZARUS}
  Result := DateTimeToUnix(LocalTimeToUniversal(Now)) * 1000;
{$ELSE}
  Result := DateTimeToUnix(Now{$IFDEF DXE6}, False{$ENDIF}) * 1000;
{$ENDIF}
end;

procedure TsgcHTTP_API_Binance.SetBinanceOptions(const Value
  : TsgcHTTPBinance_Options);
begin
  if Assigned(FBinanceOptions) then
    FBinanceOptions.Assign(Value);
end;

constructor TsgcHTTPBinance_Options.Create;
begin
  inherited;
  FLogOptions := TsgcHTTPBinanceLog_Options.Create;
  BinanceUS := False;
end;

destructor TsgcHTTPBinance_Options.Destroy;
begin
  sgcFree(FLogOptions);
  inherited;
end;

procedure TsgcHTTPBinance_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTPBinance_Options then
  begin
    ApiKey := TsgcHTTPBinance_Options(aSource).ApiKey;
    ApiSecret := TsgcHTTPBinance_Options(aSource).ApiSecret;
    LogOptions := TsgcHTTPBinance_Options(aSource).LogOptions;
    TestNet := TsgcHTTPBinance_Options(aSource).TestNet;
    BinanceUS := TsgcHTTPBinance_Options(aSource).BinanceUS;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcHTTPBinance_Options.SetLogOptions(const Value
  : TsgcHTTPBinanceLog_Options);
begin
  if Assigned(FLogOptions) then
    FLogOptions.Assign(Value);
end;

procedure TsgcHTTPBinanceLog_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTPBinanceLog_Options then
  begin
    Enabled := TsgcHTTPBinanceLog_Options(aSource).Enabled;
    FileName := TsgcHTTPBinanceLog_Options(aSource).FileName;
  end
  else
    inherited Assign(aSource);
end;

function TsgcHTTP_API_Binance_Rest.CancelOCO(const aSymbol: String;
  aOrderListId: Integer = 0; const aListClientOrderId: String = '';
  aNewClientOrderId: String = ''): string;
var
  vParameters: String;
begin
  vParameters := Format('symbol=%s&timestamp=%d', [aSymbol, GetTimeStamp]);
  if aOrderListId > 0 then
    vParameters := vParameters + '&orderListId=' + IntToStr(aOrderListId);
  if aListClientOrderId <> '' then
    vParameters := vParameters + '&listClientOrderId=' + aListClientOrderId;
  if aNewClientOrderId <> '' then
    vParameters := vParameters + '&newClientOrderId=' + aNewClientOrderId;

  Result := DoHTTP_DELETE_HMAC256(CS_BNC_ENDPOINT_CANCEL_OCO, vParameters);
end;

function TsgcHTTP_API_Binance_Rest.CancelAllOpenOrders(const aSymbol
  : String): string;
var
  vParameters: String;
begin
  vParameters := Format('symbol=%s&timestamp=%d', [aSymbol, GetTimeStamp]);

  Result := DoHTTP_DELETE_HMAC256(CS_BNC_ENDPOINT_OPEN_ORDERS, vParameters);
end;

function TsgcHTTP_API_Binance_Rest.CancelOrder(const aSymbol: String;
  aOrderId: Int64 = 0; const aOrigClientOrderId: String = ''): string;
var
  vParameters: String;
begin
  vParameters := Format('symbol=%s&timestamp=%d', [aSymbol, GetTimeStamp]);
  if aOrderId > 0 then
    vParameters := vParameters + '&orderId=' + IntToStr(aOrderId);
  if aOrigClientOrderId <> '' then
    vParameters := vParameters + '&origClientOrderId=' + aOrigClientOrderId;

  Result := DoHTTP_DELETE_HMAC256(CS_BNC_ENDPOINT_CANCEL_ORDER, vParameters);
end;

function TsgcHTTP_API_Binance_Rest.GetAccountInformation: String;
begin
  Result := DoHTTP_GET_HMAC256(CS_BNC_ENDPOINT_ACCOUNT_INFORMATION,
    Format('timestamp=%d', [GetTimeStamp]));
end;

function TsgcHTTP_API_Binance_Rest.GetAccountTradeList(const aSymbol: String;
    aStartTime: Int64 = 0; aEndTime: Int64 = 0; aFromId: Integer = 0; aLimit:
    Integer = 500): String;
var
  vParameters: String;
begin
  vParameters := Format('symbol=%s&timestamp=%d&limit=%d',
    [aSymbol, GetTimeStamp, aLimit]);
  if aFromId > 0 then
    vParameters := vParameters + '&fromId=' + IntToStr(aFromId);
  if aStartTime > 0 then
    vParameters := vParameters + '&startTime=' + IntToStr(aStartTime);
  if aEndTime > 0 then
    vParameters := vParameters + '&endTime=' + IntToStr(aEndTime);

  Result := DoHTTP_GET_HMAC256(CS_BNC_ENDPOINT_ACCOUNT_TRADE_LIST, vParameters);
end;

function TsgcHTTP_API_Binance_Rest.GetAllOCO(aFromId: Integer = 0; aStartTime:
    Int64 = 0; aEndTime: Int64 = 0; aLimit: Integer = 500): string;
var
  vParameters: String;
begin
  vParameters := Format('limit=%d&timestamp=%d', [aLimit, GetTimeStamp]);
  if aFromId > 0 then
    vParameters := vParameters + '&fromId	=' + IntToStr(aFromId);
  if aStartTime > 0 then
    vParameters := vParameters + '&startTime=' + IntToStr(aStartTime);
  if aEndTime > 0 then
    vParameters := vParameters + '&endTime=' + IntToStr(aEndTime);

  Result := DoHTTP_GET_HMAC256(CS_BNC_ENDPOINT_ALL_OCO, vParameters);
end;

function TsgcHTTP_API_Binance_Rest.GetAllOrders(const aSymbol: String;
    aOrderId: Int64 = 0; aStartTime: Int64 = 0; aEndTime: Int64 = 0; aLimit:
    Integer = 500): string;
var
  vParameters: String;
begin
  vParameters := Format('symbol=%s&limit=%d&timestamp=%d',
    [aSymbol, aLimit, GetTimeStamp]);
  if aOrderId > 0 then
    vParameters := vParameters + '&orderId=' + IntToStr(aOrderId);
  if aStartTime > 0 then
    vParameters := vParameters + '&startTime=' + IntToStr(aStartTime);
  if aEndTime > 0 then
    vParameters := vParameters + '&endTime=' + IntToStr(aEndTime);

  Result := DoHTTP_GET_HMAC256(CS_BNC_ENDPOINT_ALL_ORDERS, vParameters);
end;

function TsgcHTTP_API_Binance_Rest.GetAveragePrice(const aSymbol
  : String): string;
begin
  Result := DoHTTP_GET(CS_BNC_ENDPOINT_AVERAGE_PRICE,
    Format('symbol=%s', [aSymbol]));
end;

function TsgcHTTP_API_Binance_Rest.GetEndpoint24HRTicker: String;
begin
  Result := CS_BNC_ENDPOINT_24HR_TICKER;
end;

function TsgcHTTP_API_Binance_Rest.GetEndpointAggregateTrades: String;
begin
  Result := CS_BNC_ENDPOINT_AGGREGATE_TRADES;
end;

function TsgcHTTP_API_Binance_Rest.GetEndpointBase: String;
begin
  if BinanceOptions.BinanceUS then
  begin
    if BinanceOptions.TestNet then
      raise Exception.Create(S_BINANCE_ERROR_NOT_SUPPORTED_BINANCE_US)
    else
      Result := CS_BNC_US_ENDPOINT_BASE;
  end
  else
  begin
    if BinanceOptions.TestNet then
      Result := CS_BNC_TESTNET_ENDPOINT_BASE
    else
      Result := CS_BNC_ENDPOINT_BASE;
  end;
end;

function TsgcHTTP_API_Binance_Rest.GetEndpointBookTicker: String;
begin
  Result := CS_BNC_ENDPOINT_BOOK_TICKER;
end;

function TsgcHTTP_API_Binance_Rest.GetEndpointExchangeInformation: String;
begin
  Result := CS_BNC_ENDPOINT_EXCHANGE_INFORMATION;
end;

function TsgcHTTP_API_Binance_Rest.GetEndpointKLines: String;
begin
  Result := CS_BNC_ENDPOINT_KLINES;
end;

function TsgcHTTP_API_Binance_Rest.GetEndpointOrderBook: String;
begin
  Result := CS_BNC_ENDPOINT_ORDER_BOOK;
end;

function TsgcHTTP_API_Binance_Rest.GetEndpointPing: String;
begin
  Result := CS_BNC_ENDPOINT_PING;
end;

function TsgcHTTP_API_Binance_Rest.GetEndpointPriceTicker: String;
begin
  Result := CS_BNC_ENDPOINT_PRICE_TICKER;
end;

function TsgcHTTP_API_Binance_Rest.GetEndpointServerTime: String;
begin
  Result := CS_BNC_ENDPOINT_SERVER_TIME;
end;

function TsgcHTTP_API_Binance_Rest.GetEndpointTrades: String;
begin
  Result := CS_BNC_ENDPOINT_TRADES;
end;

function TsgcHTTP_API_Binance_Rest.GetOpenOCO: string;
var
  vParameters: string;
begin
  vParameters := Format('timestamp=%d', [GetTimeStamp]);

  Result := DoHTTP_GET_HMAC256(CS_BNC_ENDPOINT_OPEN_OCO, vParameters);
end;

function TsgcHTTP_API_Binance_Rest.GetOpenOrders(const aSymbol
  : String = ''): string;
var
  vParameters: string;
begin
  vParameters := Format('timestamp=%d', [GetTimeStamp]);
  if aSymbol <> '' then
    vParameters := vParameters + '&symbol=' + aSymbol;

  Result := DoHTTP_GET_HMAC256(CS_BNC_ENDPOINT_OPEN_ORDERS, vParameters);
end;

function TsgcHTTP_API_Binance_Rest.NewOCO(const aSymbol, aSide: String;
  aQuantity, aPrice, aStopPrice: Double; const aListClientOrderId: String = '';
  aLimitClientOrderId: string = ''; aLimitIcebergQty: Double = 0;
  aStopClientOrderId: string = ''; aStopLimitPrice: Double = 0;
  aStopIcebergQty: Double = 0; aStopLimitTimeInForce: String = '';
  aNewOrderRespType: String = ''): String;
var
  vParameters: String;
begin
  vParameters :=
    Format('symbol=%s&side=%s&quantity=%s&price=%s&stopPrice=%s&timestamp=%d',
    [aSymbol, aSide, GetDoubleString(aQuantity), GetDoubleString(aPrice),
    GetDoubleString(aStopPrice), GetTimeStamp]);
  if aListClientOrderId <> '' then
    vParameters := vParameters + '&listClientOrderId=' + aListClientOrderId;
  if aLimitClientOrderId <> '' then
    vParameters := vParameters + '&limitClientOrderId=' + aLimitClientOrderId;
  if aLimitIcebergQty <> 0 then
    vParameters := vParameters + '&limitIcebergQty=' +
      GetDoubleString(aLimitIcebergQty);
  if aStopClientOrderId <> '' then
    vParameters := vParameters + '&stopClientOrderId=' + aStopClientOrderId;
  if aStopLimitPrice <> 0 then
    vParameters := vParameters + '&stopLimitPrice=' +
      GetDoubleString(aStopLimitPrice);
  if aStopIcebergQty <> 0 then
    vParameters := vParameters + '&stopIcebergQty=' +
      GetDoubleString(aStopIcebergQty);
  if aStopLimitTimeInForce <> '' then
    vParameters := vParameters + '&stopLimitTimeInForce=' +
      aStopLimitTimeInForce;
  if aNewOrderRespType <> '' then
    vParameters := vParameters + '&newOrderRespType=' + aNewOrderRespType;

  Result := DoHTTP_POST_HMAC256(CS_BNC_ENDPOINT_NEW_OCO, vParameters, True);
end;

function TsgcHTTP_API_Binance_Rest.NewOrder(const aSymbol, aSide, aType: String;
  aTimeInForce: String = ''; aQuantity: Double = 0; aQuoteOrderQty: Double = 0;
  aPrice: Double = 0; aNewClientOrderId: String = ''; aStopPrice: Double = 0;
  aIcebergQty: Double = 0; aNewOrderRespType: String = ''): String;
var
  vParameters: String;
begin
  vParameters := Format('symbol=%s&side=%s&type=%s&timestamp=%d',
    [aSymbol, aSide, aType, GetTimeStamp]);
  if aTimeInForce <> '' then
    vParameters := vParameters + '&timeInForce=' + aTimeInForce;
  if aQuantity <> 0 then
    vParameters := vParameters + '&quantity=' + GetDoubleString(aQuantity);
  if aQuoteOrderQty <> 0 then
    vParameters := vParameters + '&quoteOrderQty=' +
      GetDoubleString(aQuoteOrderQty);
  if aPrice <> 0 then
    vParameters := vParameters + '&price=' + GetDoubleString(aPrice);
  if aNewClientOrderId <> '' then
    vParameters := vParameters + '&newClientOrderId=' + aNewClientOrderId;
  if aStopPrice <> 0 then
    vParameters := vParameters + '&stopPrice=' + GetDoubleString(aStopPrice);
  if aIcebergQty <> 0 then
    vParameters := vParameters + '&icebergQty=' + GetDoubleString(aIcebergQty);
  if aNewOrderRespType <> '' then
    vParameters := vParameters + '&newOrderRespType=' + aNewOrderRespType;

  Result := DoHTTP_POST_HMAC256(CS_BNC_ENDPOINT_NEW_ORDER, vParameters, True);
end;

function TsgcHTTP_API_Binance_Rest.PlaceLimitOrder
  (aSide: TsgcHTTPBinanceOrderSide; const aSymbol: string; aQuantity: Double;
  aLimitPrice: Double): string;
begin
  Result := NewOrder(aSymbol, GetOrderSideString(aSide), 'LIMIT', 'GTC',
    aQuantity, 0, aLimitPrice);
end;

function TsgcHTTP_API_Binance_Rest.PlaceMarketOrder
  (aSide: TsgcHTTPBinanceOrderSide; const aSymbol: string;
  aQuantity: Double): string;
begin
  Result := NewOrder(aSymbol, GetOrderSideString(aSide), 'MARKET', '',
    aQuantity);
end;

function TsgcHTTP_API_Binance_Rest.PlaceStopOrder(aSide:
    TsgcHTTPBinanceOrderSide; const aSymbol: string; aQuantity, aStopPrice,
    aLimitPrice: Double): string;
begin
  if aLimitPrice > 0 then
    Result := NewOrder(aSymbol, GetOrderSideString(aSide), 'STOP_LOSS_LIMIT', 'GTC',
      aQuantity, 0, aLimitPrice, '', aStopPrice)
  else
    Result := NewOrder(aSymbol, GetOrderSideString(aSide), 'STOP_LOSS', '',
      aQuantity, 0, 0, '', aStopPrice);
end;

function TsgcHTTP_API_Binance_Rest.QueryOCO(const aSymbol: String;
  aOrderListId: Integer = 0; const aOrigClientOrderId: String = ''): string;
var
  vParameters: String;
begin
  vParameters := Format('symbol=%s&timestamp=%d', [aSymbol, GetTimeStamp]);
  if aOrderListId > 0 then
    vParameters := vParameters + '&orderListId=' + IntToStr(aOrderListId);
  if aOrigClientOrderId <> '' then
    vParameters := vParameters + '&origClientOrderId=' + aOrigClientOrderId;

  Result := DoHTTP_GET_HMAC256(CS_BNC_ENDPOINT_QUERY_OCO, vParameters);
end;

function TsgcHTTP_API_Binance_Rest.QueryOrder(const aSymbol: String;
  aOrderId: Int64 = 0; const aOrigClientOrderId: String = ''): string;
var
  vParameters: String;
begin
  vParameters := Format('symbol=%s&timestamp=%d', [aSymbol, GetTimeStamp]);
  if aOrderId > 0 then
    vParameters := vParameters + '&orderId=' + IntToStr(aOrderId);
  if aOrigClientOrderId <> '' then
    vParameters := vParameters + '&origClientOrderId=' + aOrigClientOrderId;

  Result := DoHTTP_GET_HMAC256(CS_BNC_ENDPOINT_QUERY_ORDER, vParameters);
end;

function TsgcHTTP_API_Binance_Rest.TestNewOrder(const aSymbol, aSide,
  aType: String; aTimeInForce: String = ''; aQuantity: Double = 0;
  aQuoteOrderQty: Double = 0; aPrice: Double = 0;
  aNewClientOrderId: String = ''; aStopPrice: Double = 0;
  aIcebergQty: Double = 0; aNewOrderRespType: String = ''): String;
var
  vParameters: String;
begin
  vParameters := Format('symbol=%s&side=%s&type=%s&timestamp=%d',
    [aSymbol, aSide, aType, GetTimeStamp]);
  if aTimeInForce <> '' then
    vParameters := vParameters + '&timeInForce=' + aTimeInForce;
  if aQuantity <> 0 then
    vParameters := vParameters + '&quantity=' + GetDoubleString(aQuantity);
  if aQuoteOrderQty <> 0 then
    vParameters := vParameters + '&quoteOrderQty=' +
      GetDoubleString(aQuoteOrderQty);
  if aPrice <> 0 then
    vParameters := vParameters + '&price=' + GetDoubleString(aPrice);
  if aNewClientOrderId <> '' then
    vParameters := vParameters + '&newClientOrderId=' + aNewClientOrderId;
  if aStopPrice <> 0 then
    vParameters := vParameters + '&stopPrice=' + GetDoubleString(aStopPrice);
  if aIcebergQty <> 0 then
    vParameters := vParameters + '&icebergQty=' + GetDoubleString(aIcebergQty);
  if aNewOrderRespType <> '' then
    vParameters := vParameters + '&newOrderRespType=' + aNewOrderRespType;

  Result := DoHTTP_POST_HMAC256(CS_BNC_ENDPOINT_TEST_NEW_ORDER,
    vParameters, True);
end;

function TsgcHTTP_API_Binance_Futures_Rest.CancelOrder(const aSymbol: String;
  aOrderId: Int64 = 0; const aOrigClientOrderId: String = ''): string;
var
  vParameters: String;
begin
  vParameters := Format('symbol=%s&timestamp=%d', [aSymbol, GetTimeStamp]);
  if aOrderId > 0 then
    vParameters := vParameters + '&orderId=' + IntToStr(aOrderId);
  if aOrigClientOrderId <> '' then
    vParameters := vParameters + '&origClientOrderId=' + aOrigClientOrderId;

  Result := DoHTTP_DELETE_HMAC256(CS_BNC_ENDPOINT_FUT_CANCEL_ORDER,
    vParameters);
end;

function TsgcHTTP_API_Binance_Futures_Rest.CancelAllOpenOrders
  (const aSymbol: String): string;
var
  vParameters: String;
begin
  vParameters := Format('symbol=%s&timestamp=%d', [aSymbol, GetTimeStamp]);

  Result := DoHTTP_DELETE_HMAC256(CS_BNC_ENDPOINT_FUT_CANCEL_ALL_OPEN_ORDERS,
    vParameters);
end;

function TsgcHTTP_API_Binance_Futures_Rest.AutoCancelAllOpenOrders
  (const aSymbol: String; aCountdownTime: Integer): string;
var
  vParameters: String;
begin
  vParameters := Format('symbol=%s&countdownTime=%d&timestamp=%d',
    [aSymbol, aCountdownTime, GetTimeStamp]);

  Result := DoHTTP_POST_HMAC256(CS_BNC_ENDPOINT_FUT_AUTOCANCEL_ALL_OPEN_ORDERS,
    vParameters);
end;

function TsgcHTTP_API_Binance_Futures_Rest.ChangeInitialLeverage
  (const aSymbol: String; aLeverage: Integer): string;
var
  vParameters: String;
begin
  vParameters := Format('symbol=%s&leverage=%d&timestamp=%d',
    [aSymbol, aLeverage, GetTimeStamp]);

  Result := DoHTTP_POST_HMAC256(CS_BNC_ENDPOINT_FUT_CHANGE_INITIAL_LEVERAGE,
    vParameters, True); // pass in the querystring, binance returns an error if false
end;

function TsgcHTTP_API_Binance_Futures_Rest.GetPositionInformation
  (const aSymbol: String = ''): string;
var
  vParameters: String;
begin
  vParameters := Format('timestamp=%d', [GetTimeStamp]);
  if aSymbol <> '' then
    vParameters := vParameters + '&symbol=' + aSymbol;

  case FuturesContracts of
    bfchUSDT:
      Result := DoHTTP_GET_HMAC256
        (CS_BNC_ENDPOINT_FUT_USDT_POSITION_INFORMATION, vParameters);
    bfchCOIN:
      Result := DoHTTP_GET_HMAC256
        (CS_BNC_ENDPOINT_FUT_COIN_POSITION_INFORMATION, vParameters);
  end;
end;

function TsgcHTTP_API_Binance_Futures_Rest.ChangeMarginType(const aSymbol,
  aMarginType: String): string;
var
  vParameters: String;
begin
  vParameters := Format('symbol=%s&marginType=%s&timestamp=%d',
    [aSymbol, aMarginType, GetTimeStamp]);

  Result := DoHTTP_POST_HMAC256(CS_BNC_ENDPOINT_FUT_CHANGE_MARGIN_TYPE,
    vParameters, True); // pass in the querystring, binance returns an error if false
end;

function TsgcHTTP_API_Binance_Futures_Rest.GetPositionMarginChangeHistory(const
    aSymbol: String; aType: Integer = 0; aStartTime: Int64 = 0; aEndTime: Int64
    = 0; aLimit: Integer = 500): string;
var
  vParameters: String;
begin
  vParameters := Format('symbol=%s&limit=%d&timestamp=%d',
    [aSymbol, aLimit, GetTimeStamp]);
  if aType <> 0 then
    vParameters := vParameters + '&type=' + IntToStr(aType);
  if aStartTime <> 0 then
    vParameters := vParameters + '&startTime=' + IntToStr(aStartTime);
  if aEndTime <> 0 then
    vParameters := vParameters + '&endTime=' + IntToStr(aEndTime);

  Result := DoHTTP_GET_HMAC256
    (CS_BNC_ENDPOINT_FUT_GET_POSITION_MARGIN_CHANGE_HISTORY, vParameters);
end;

function TsgcHTTP_API_Binance_Futures_Rest.ModifyIsolatedPositionMargin
  (const aSymbol: String; aAmount: Double; aType: Integer;
  aPositionSide: String = ''): string;
var
  vParameters: String;
begin
  vParameters := Format('symbol=%s&amount=%s&type=%d&timestamp=%d',
    [aSymbol, GetDoubleString(aAmount), aType, GetTimeStamp]);
  if aPositionSide <> '' then
    vParameters := vParameters + '&positionSide=' + aPositionSide;

  Result := DoHTTP_POST_HMAC256
    (CS_BNC_ENDPOINT_FUT_MODIFY_ISOLATED_POSITION_MARGIN, vParameters);
end;

function TsgcHTTP_API_Binance_Futures_Rest.ChangePositionMode
  (aDualPosition: Boolean): string;
var
  vParameters: String;
  vDualPosition: String;
begin
  if aDualPosition then
    vDualPosition := 'true'
  else
    vDualPosition := 'false';
  vParameters := Format('dualSidePosition=%s&timestamp=%d',
    [vDualPosition, GetTimeStamp]);

  Result := DoHTTP_GET_HMAC256(CS_BNC_ENDPOINT_FUT_CHANGE_POSITION_MODE,
    vParameters);
end;

function TsgcHTTP_API_Binance_Futures_Rest.GetAccountBalance: string;
var
  vParameters: string;
begin
  vParameters := Format('timestamp=%d', [GetTimeStamp]);

  case FuturesContracts of
    bfchUSDT:
      Result := DoHTTP_GET_HMAC256(CS_BNC_ENDPOINT_FUT_USDT_ACCOUNT_BALANCE,
        vParameters);
    bfchCOIN:
      Result := DoHTTP_GET_HMAC256(CS_BNC_ENDPOINT_FUT_COIN_ACCOUNT_BALANCE,
        vParameters);
  end;
end;

function TsgcHTTP_API_Binance_Futures_Rest.GetAccountInformation: string;
var
  vParameters: string;
begin
  vParameters := Format('timestamp=%d', [GetTimeStamp]);

  case FuturesContracts of
    bfchUSDT:
      Result := DoHTTP_GET_HMAC256(CS_BNC_ENDPOINT_FUT_USDT_ACCOUNT_INFORMATION,
        vParameters);
    bfchCOIN:
      Result := DoHTTP_GET_HMAC256(CS_BNC_ENDPOINT_FUT_COIN_ACCOUNT_INFORMATION,
        vParameters);
  end;
end;

function TsgcHTTP_API_Binance_Futures_Rest.GetAccountTradeList(const aSymbol:
    String; aStartTime: Int64 = 0; aEndTime: Int64 = 0; aFromId: Integer = 0;
    aLimit: Integer = 500): String;
var
  vParameters: String;
begin
  vParameters := Format('symbol=%s&timestamp=%d&limit=%d',
    [aSymbol, GetTimeStamp, aLimit]);
  if aFromId > 0 then
    vParameters := vParameters + '&fromId=' + IntToStr(aFromId);
  if aStartTime > 0 then
    vParameters := vParameters + '&startTime=' + IntToStr(aStartTime);
  if aEndTime > 0 then
    vParameters := vParameters + '&endTime=' + IntToStr(aEndTime);

  Result := DoHTTP_GET_HMAC256(CS_BNC_ENDPOINT_FUT_ACCOUNT_TRADE_LIST,
    vParameters);
end;

function TsgcHTTP_API_Binance_Futures_Rest.GetEndpoint24HRTicker: String;
begin
  Result := CS_BNC_ENDPOINT_FUT_24HR_TICKER;
end;

function TsgcHTTP_API_Binance_Futures_Rest.GetEndpointAggregateTrades: String;
begin
  Result := CS_BNC_ENDPOINT_FUT_AGGREGATE_TRADES;
end;

function TsgcHTTP_API_Binance_Futures_Rest.GetEndpointBase: String;
begin
  if BinanceOptions.BinanceUS then
    raise Exception.Create(S_BINANCE_ERROR_NOT_SUPPORTED_BINANCE_US);

  case FuturesContracts of
    bfchUSDT:
      begin
        if BinanceOptions.TestNet then
          Result := CS_BNC_ENDPOINT_TESTNET_FUT_USDT_BASE
        else
          Result := CS_BNC_ENDPOINT_FUT_USDT_BASE;
      end;
    bfchCOIN:
      begin
        if BinanceOptions.TestNet then
          Result := CS_BNC_ENDPOINT_TESTNET_FUT_COIN_BASE
        else
          Result := CS_BNC_ENDPOINT_FUT_COIN_BASE;
      end;
  end;
end;

function TsgcHTTP_API_Binance_Futures_Rest.GetEndpointBookTicker: String;
begin
  Result := CS_BNC_ENDPOINT_FUT_BOOK_TICKER;
end;

function TsgcHTTP_API_Binance_Futures_Rest.
  GetEndpointExchangeInformation: String;
begin
  Result := CS_BNC_ENDPOINT_FUT_EXCHANGE_INFORMATION;
end;

function TsgcHTTP_API_Binance_Futures_Rest.GetEndpointKLines: String;
begin
  Result := CS_BNC_ENDPOINT_FUT_KLINES;
end;

function TsgcHTTP_API_Binance_Futures_Rest.GetEndpointOrderBook: String;
begin
  Result := CS_BNC_ENDPOINT_FUT_ORDER_BOOK;
end;

function TsgcHTTP_API_Binance_Futures_Rest.GetEndpointPing: String;
begin
  Result := CS_BNC_ENDPOINT_FUT_PING;
end;

function TsgcHTTP_API_Binance_Futures_Rest.GetEndpointPriceTicker: String;
begin
  Result := CS_BNC_ENDPOINT_FUT_PRICE_TICKER;
end;

function TsgcHTTP_API_Binance_Futures_Rest.GetEndpointServerTime: String;
begin
  Result := CS_BNC_ENDPOINT_FUT_SERVER_TIME;
end;

function TsgcHTTP_API_Binance_Futures_Rest.GetEndpointTrades: String;
begin
  Result := CS_BNC_ENDPOINT_FUT_TRADES;
end;

function TsgcHTTP_API_Binance_Futures_Rest.GetFundingRateHistory(const aSymbol:
    String; aStartTime: Int64 = 0; aEndTime: Int64 = 0; aLimit: Integer = 100):
    string;
var
  vParameters: String;
begin
  vParameters := Format('symbol=%s&limit=%d', [aSymbol, aLimit]);
  if aStartTime > 0 then
    vParameters := vParameters + '&startTime=' + IntToStr(aStartTime);
  if aEndTime > 0 then
    vParameters := vParameters + '&endTime=' + IntToStr(aEndTime);

  Result := DoHTTP_GET(CS_BNC_ENDPOINT_FUT_FUNDING_RATE_HISTORY, vParameters);
end;

function TsgcHTTP_API_Binance_Futures_Rest.GetAllOrders(const aSymbol: String;
    aOrderId: Int64 = 0; aStartTime: Int64 = 0; aEndTime: Int64 = 0; aLimit:
    Integer = 100): string;
var
  vParameters: String;
begin
  vParameters := Format('symbol=%s&limit=%d&timestamp=%d',
    [aSymbol, aLimit, GetTimeStamp]);
  if aOrderId > 0 then
    vParameters := vParameters + '&orderId=' + IntToStr(aOrderId);
  if aStartTime > 0 then
    vParameters := vParameters + '&startTime=' + IntToStr(aStartTime);
  if aEndTime > 0 then
    vParameters := vParameters + '&endTime=' + IntToStr(aEndTime);

  Result := DoHTTP_GET_HMAC256(CS_BNC_ENDPOINT_FUT_ALL_ORDERS, vParameters);
end;

function TsgcHTTP_API_Binance_Futures_Rest.GetCurrentPositionMode: string;
var
  vParameters: String;
begin
  vParameters := Format('timestamp=%d', [GetTimeStamp]);

  Result := DoHTTP_GET_HMAC256(CS_BNC_ENDPOINT_FUT_CURRENT_POSITION_MODE,
    vParameters);
end;

function TsgcHTTP_API_Binance_Futures_Rest.GetOpenInterestStatistics(const
    aSymbol: String; const aPeriod: TsgcHTTPBinanceFutOpenInterestPeriods;
    aLimit: Integer = 30; aStartTime: Int64 = 0; aEndTime: Int64 = 0): string;
var
  vParameters: String;
begin
  vParameters := Format('symbol=%s&period=%s&limit=%d',
    [aSymbol, GetOpenInterestPeriod(aPeriod), aLimit]);
  if aStartTime > 0 then
    vParameters := vParameters + '&startTime=' + IntToStr(aStartTime);
  if aEndTime > 0 then
    vParameters := vParameters + '&endTime=' + IntToStr(aEndTime);

  Result := DoHTTP_GET(CS_BNC_ENDPOINT_FUT_OPEN_INTEREST_STATISTICS,
    vParameters);
end;

function TsgcHTTP_API_Binance_Futures_Rest.GetMarkPrice(const aSymbol
  : String = ''): string;
var
  vParameters: string;
begin
  vParameters := '';
  if aSymbol <> '' then
    vParameters := Format('symbol=%s', [aSymbol]);
  Result := DoHTTP_GET(CS_BNC_ENDPOINT_FUT_MARK_PRICE, vParameters);
end;

function TsgcHTTP_API_Binance_Futures_Rest.GetOpenInterest
  (const aSymbol: String = ''): string;
var
  vParameters: string;
begin
  vParameters := '';
  if aSymbol <> '' then
    vParameters := Format('symbol=%s', [aSymbol]);
  Result := DoHTTP_GET(CS_BNC_ENDPOINT_FUT_OPEN_INTEREST, vParameters);
end;

function TsgcHTTP_API_Binance_Futures_Rest.GetOpenInterestPeriod
  (aValue: TsgcHTTPBinanceFutOpenInterestPeriods): String;
begin
  case aValue of
    oip5m:
      Result := '5m';
    oip15m:
      Result := '15m';
    oip30m:
      Result := '30m';
    oip1h:
      Result := '1h';
    oip2h:
      Result := '2h';
    oip4h:
      Result := '4h';
    oip6h:
      Result := '6h';
    oip12h:
      Result := '12h';
    oip1d:
      Result := '1d';
  end;
end;

function TsgcHTTP_API_Binance_Futures_Rest.GetTopTraderAccountRatio(const
    aSymbol: String; const aPeriod: TsgcHTTPBinanceFutOpenInterestPeriods;
    aLimit: Integer = 30; aStartTime: Int64 = 0; aEndTime: Int64 = 0): string;
var
  vParameters: String;
begin
  vParameters := Format('symbol=%s&period=%s&limit=%d',
    [aSymbol, GetOpenInterestPeriod(aPeriod), aLimit]);
  if aStartTime > 0 then
    vParameters := vParameters + '&startTime=' + IntToStr(aStartTime);
  if aEndTime > 0 then
    vParameters := vParameters + '&endTime=' + IntToStr(aEndTime);

  Result := DoHTTP_GET(CS_BNC_ENDPOINT_FUT_TOP_LONG_SHORT_ACCOUNT_RATIO,
    vParameters);
end;

function TsgcHTTP_API_Binance_Futures_Rest.GetTopTraderPositionRatio(const
    aSymbol: String; const aPeriod: TsgcHTTPBinanceFutOpenInterestPeriods;
    aLimit: Integer = 30; aStartTime: Int64 = 0; aEndTime: Int64 = 0): string;
var
  vParameters: String;
begin
  vParameters := Format('symbol=%s&period=%s&limit=%d',
    [aSymbol, GetOpenInterestPeriod(aPeriod), aLimit]);
  if aStartTime > 0 then
    vParameters := vParameters + '&startTime=' + IntToStr(aStartTime);
  if aEndTime > 0 then
    vParameters := vParameters + '&endTime=' + IntToStr(aEndTime);

  Result := DoHTTP_GET(CS_BNC_ENDPOINT_FUT_TOP_LONG_SHORT_POSITION_RATIO,
    vParameters);
end;

function TsgcHTTP_API_Binance_Futures_Rest.GetGlobalAccountRatio(const aSymbol:
    String; const aPeriod: TsgcHTTPBinanceFutOpenInterestPeriods; aLimit:
    Integer = 30; aStartTime: Int64 = 0; aEndTime: Int64 = 0): string;
var
  vParameters: String;
begin
  vParameters := Format('symbol=%s&period=%s&limit=%d',
    [aSymbol, GetOpenInterestPeriod(aPeriod), aLimit]);
  if aStartTime > 0 then
    vParameters := vParameters + '&startTime=' + IntToStr(aStartTime);
  if aEndTime > 0 then
    vParameters := vParameters + '&endTime=' + IntToStr(aEndTime);

  Result := DoHTTP_GET(CS_BNC_ENDPOINT_FUT_GLOBAL_LONG_SHORT_ACCOUNT_RATIO,
    vParameters);
end;

function TsgcHTTP_API_Binance_Futures_Rest.GetIncomeHistory(const aSymbol:
    String = ''; const aIncomeType: String = ''; aStartTime: Int64 = 0;
    aEndTime: Int64 = 0; aLimit: Integer = 100): string;
var
  vParameters: String;
begin
  vParameters := Format('timestamp=%d&limit=%d', [GetTimeStamp, aLimit]);
  if aSymbol <> '' then
    vParameters := vParameters + '&symbol=' + aSymbol;
  if aIncomeType <> '' then
    vParameters := vParameters + '&incomeType=' + aIncomeType;
  if aStartTime > 0 then
    vParameters := vParameters + '&startTime=' + IntToStr(aStartTime);
  if aEndTime > 0 then
    vParameters := vParameters + '&endTime=' + IntToStr(aEndTime);

  Result := DoHTTP_GET_HMAC256(CS_BNC_ENDPOINT_FUT_GET_INCOME_HISTORY,
    vParameters);
end;

function TsgcHTTP_API_Binance_Futures_Rest.GetOpenOrders(const aSymbol
  : String = ''): string;
var
  vParameters: string;
begin
  vParameters := Format('timestamp=%d', [GetTimeStamp]);
  if aSymbol <> '' then
    vParameters := vParameters + '&symbol=' + aSymbol;

  Result := DoHTTP_GET_HMAC256(CS_BNC_ENDPOINT_FUT_OPEN_ORDERS, vParameters);
end;

function TsgcHTTP_API_Binance_Futures_Rest.GetNotionalLeverageBracket
  (const aSymbol: String = ''): string;
var
  vParameters: String;
begin
  vParameters := Format('timestamp=%d', [GetTimeStamp]);
  if aSymbol <> '' then
    vParameters := vParameters + '&symbol=' + aSymbol;

  Result := DoHTTP_GET_HMAC256(CS_BNC_ENDPOINT_FUT_NOTIONAL_LEVERAGE_BRACKET,
    vParameters);
end;

function TsgcHTTP_API_Binance_Futures_Rest.GetTakerVolume(const aSymbol:
    String; const aPeriod: TsgcHTTPBinanceFutOpenInterestPeriods; aLimit:
    Integer = 30; aStartTime: Int64 = 0; aEndTime: Int64 = 0): string;
var
  vParameters: String;
begin
  vParameters := Format('symbol=%s&period=%s&limit=%d',
    [aSymbol, GetOpenInterestPeriod(aPeriod), aLimit]);
  if aStartTime > 0 then
    vParameters := vParameters + '&startTime=' + IntToStr(aStartTime);
  if aEndTime > 0 then
    vParameters := vParameters + '&endTime=' + IntToStr(aEndTime);

  Result := DoHTTP_GET(CS_BNC_ENDPOINT_FUT_TAKER_LONG_SHORT_RATIO, vParameters);
end;

function TsgcHTTP_API_Binance_Futures_Rest.NewOrder(const aSymbol,
  aSide: String; aPositionSide: String; const aType: String;
  aTimeInForce: String = ''; aQuantity: Double = 0; aReduceOnly: string = '';
  aPrice: Double = 0; aNewClientOrderId: String = ''; aStopPrice: Double = 0;
  aClosePosition: string = ''; aActivationPrice: Double = 0;
  aCallbackRate: Double = 0; aWorkingType: String = '';
  aNewOrderRespType: String = ''): String;
var
  vParameters: String;
begin
  vParameters := Format('symbol=%s&side=%s&type=%s&timestamp=%d',
    [aSymbol, aSide, aType, GetTimeStamp]);
  if aPositionSide <> '' then
    vParameters := vParameters + '&positionSide=' + aPositionSide;
  if aTimeInForce <> '' then
    vParameters := vParameters + '&timeInForce=' + aTimeInForce;
  if aQuantity <> 0 then
    vParameters := vParameters + '&quantity=' + GetDoubleString(aQuantity);
  if aReduceOnly <> '' then
    vParameters := vParameters + '&reduceOnly=' + aReduceOnly;
  if aPrice <> 0 then
    vParameters := vParameters + '&price=' + GetDoubleString(aPrice);
  if aNewClientOrderId <> '' then
    vParameters := vParameters + '&newClientOrderId=' + aNewClientOrderId;
  if aStopPrice <> 0 then
    vParameters := vParameters + '&stopPrice=' + GetDoubleString(aStopPrice);
  if aClosePosition <> '' then
    vParameters := vParameters + '&closePosition=' + aClosePosition;
  if aActivationPrice <> 0 then
    vParameters := vParameters + '&activationPrice=' +
      GetDoubleString(aActivationPrice);
  if aCallbackRate <> 0 then
    vParameters := vParameters + '&callbackRate=' +
      GetDoubleString(aCallbackRate);
  if aWorkingType <> '' then
    vParameters := vParameters + '&workingType=' + aWorkingType;
  if aNewOrderRespType <> '' then
    vParameters := vParameters + '&newOrderRespType=' + aNewOrderRespType;

  Result := DoHTTP_POST_HMAC256(CS_BNC_ENDPOINT_FUT_NEW_ORDER,
    vParameters, True);
end;

function TsgcHTTP_API_Binance_Futures_Rest.PlaceLimitOrder(aSide:
    TsgcHTTPBinanceOrderSide; const aSymbol: string; aQuantity: Double;
    aLimitPrice: Double): string;
begin
  Result := NewOrder(aSymbol, GetOrderSideString(aSide), 'BOTH', 'LIMIT', 'GTC',
    aQuantity, 'false', aLimitPrice);
end;

function TsgcHTTP_API_Binance_Futures_Rest.PlaceMarketOrder(aSide:
    TsgcHTTPBinanceOrderSide; const aSymbol: string; aQuantity: Double): string;
begin
  Result := NewOrder(aSymbol, GetOrderSideString(aSide), 'BOTH', 'MARKET', '',
    aQuantity);
end;

function TsgcHTTP_API_Binance_Futures_Rest.PlaceStopOrder(aSide:
    TsgcHTTPBinanceOrderSide; const aSymbol: string; aQuantity, aStopPrice,
    aLimitPrice: Double): string;
begin
  if aLimitPrice > 0 then
    Result := NewOrder(aSymbol, GetOrderSideString(aSide), 'BOTH', 'STOP', 'GTC',
      aQuantity, 'false', aLimitPrice, '', aStopPrice)
  else
    Result := NewOrder(aSymbol, GetOrderSideString(aSide), 'BOTH', 'STOP_MARKET', '',
      aQuantity, 'false', 0, '', aStopPrice);
end;

function TsgcHTTP_API_Binance_Futures_Rest.PlaceTrailingStopOrder(aSide:
    TsgcHTTPBinanceOrderSide; const aSymbol: string; aQuantity,
    aActivationPrice, aCallbackRate: Double): string;
begin
  Result := NewOrder(aSymbol, GetorderSideString(aSide), 'BOTH', 'TRAILING_STOP_MARKET', '',
    aQuantity, 'false', 0, '', 0, '', aActivationPrice, aCallbackRate);
end;

function TsgcHTTP_API_Binance_Futures_Rest.QueryOrder(const aSymbol: String;
  aOrderId: Int64 = 0; const aOrigClientOrderId: String = ''): string;
var
  vParameters: String;
begin
  vParameters := Format('symbol=%s&timestamp=%d', [aSymbol, GetTimeStamp]);
  if aOrderId > 0 then
    vParameters := vParameters + '&orderId=' + IntToStr(aOrderId);
  if aOrigClientOrderId <> '' then
    vParameters := vParameters + '&origClientOrderId=' + aOrigClientOrderId;

  Result := DoHTTP_GET_HMAC256(CS_BNC_ENDPOINT_FUT_QUERY_ORDER, vParameters);
end;

function TsgcHTTP_API_Binance_Futures_Rest.QueryCurrentOpenOrder
  (const aSymbol: String; aOrderId: Int64 = 0;
  const aOrigClientOrderId: String = ''): string;
var
  vParameters: String;
begin
  vParameters := Format('symbol=%s&timestamp=%d', [aSymbol, GetTimeStamp]);
  if aOrderId > 0 then
    vParameters := vParameters + '&orderId=' + IntToStr(aOrderId);
  if aOrigClientOrderId <> '' then
    vParameters := vParameters + '&origClientOrderId=' + aOrigClientOrderId;

  Result := DoHTTP_GET_HMAC256(CS_BNC_ENDPOINT_FUT_QUERY_CURRENT_OPEN_ORDER,
    vParameters);
end;

{$ENDIF}

end.
