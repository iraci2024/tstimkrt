{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_API_Binance;

interface

{$I sgcVer.inc}
{$IFDEF SGC_APIS}

uses
  Classes, SysUtils,
  // sgc
  sgcBase_Classes, sgcWebSocket_Classes, sgcWebSocket_Types,
  sgcHTTP_API_Binance;

Type
  TsgcWSBinanceHTTPExceptionEvent = procedure(Sender: TObject; E: Exception)
    of object;

  TwsBinanceStreams = (bncAggregateTrade, bncTrade, bncKline, bncMiniTicker,
    bncAllMiniTickers, bncTicker, bncAllMarketTickers, bncBookTicker,
    bncAllBookTickers, bncPartialBookDepth, bncDiffDepth, bncMarkPrice,
    bncAllMarketMarkPrice, bncLiquidationOrder, bncAllMarketLiquidationOrder);

  TsgcWSBinanceChartIntervals = (bci1m, bci3m, bci5m, bci15m, bci30m, bci1h,
    bci2h, bci4h, bci6h, bci8h, bci12h, bci1d, bci3d, bci1w, bci1Mo);

  TsgcWSBinanceDepthLevels = (bde5, bde10, bde20);

  TsgcWSBinanceFuturesContracts = (bfcUSDT, bfcCOIN);

  TsgcWSBinanceListenKeyMethod = (blkmGetListenKey, blkmDeleteListenKey,
    blkmRefreshListenKey);

  TsgcWSBinanceListenKeyOnDisconnect = (blkodDeleteListenKey,
    blkodClearListenKey, blkodDoNothing);

  TsgcWSBinanceRestLog_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FFileName: String;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property FileName: String read FFileName write FFileName;
  end;

  TsgcWSBinanceLog_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FFileName: String;
    FREST: TsgcWSBinanceRestLog_Options;
    procedure SetREST(const Value: TsgcWSBinanceRestLog_Options);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property FileName: String read FFileName write FFileName;
    property REST: TsgcWSBinanceRestLog_Options read FREST write SetREST;
  end;

  TsgcWSBinance_Options = class(TPersistent)
  private
    FApiKey: String;
    FApiSecret: String;
    FBinanceUS: Boolean;
    FListenKeyOnDisconnect: TsgcWSBinanceListenKeyOnDisconnect;
    FHTTPLogOptions: TsgcWSBinanceLog_Options;
    FTestNet: Boolean;
    FUseCombinedStreams: Boolean;
    FUserStream: Boolean;
    procedure SetHTTPLogOptions(const Value: TsgcWSBinanceLog_Options);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property ApiKey: String read FApiKey write FApiKey;
    property ApiSecret: String read FApiSecret write FApiSecret;
    property BinanceUS: Boolean read FBinanceUS write FBinanceUS;
    property ListenKeyOnDisconnect: TsgcWSBinanceListenKeyOnDisconnect
      read FListenKeyOnDisconnect write FListenKeyOnDisconnect;
    property HTTPLogOptions: TsgcWSBinanceLog_Options read FHTTPLogOptions
      write SetHTTPLogOptions;
    property TestNet: Boolean read FTestNet write FTestNet;
    property UseCombinedStreams: Boolean read FUseCombinedStreams write
        FUseCombinedStreams;
    property UserStream: Boolean read FUserStream write FUserStream;
  end;

  TsgcWS_API_Binance_Base = class(TsgcWSAPI_Client)
    { from TsgcWSAPI_client }
  protected
    procedure DoNotifyDisconnect(aConnection: TsgcWSConnection); override;
    { from TsgcWSAPI_client }

    { id }
  protected
    procedure DoBeforeConnect; override;
    function IsInternalMessage(const aMessage: String): Boolean; override;
  protected
    FId: Integer;
    function GetNewId: Integer;
    { id }

    { url }
  protected
    function GetURL_BaseAPI: String; virtual; abstract;
    function GetURL_ListenKey: String; virtual; abstract;
    { url }

    { helpers }
  protected
    function GetDepthString(aDepth: TsgcWSBinanceDepthLevels): String; virtual;
    function GetIntervalString(aInterval: TsgcWSBinanceChartIntervals)
      : String; virtual;
  protected
    function GetStream(const aStream: TwsBinanceStreams;
      const aSymbol: String = ''; const aInterval: String = '')
      : String; virtual;
  protected
    function DoSubscribe(const aStream: TwsBinanceStreams;
      const aSymbol: String = ''; const aInterval: String = '')
      : Integer; virtual;
    function DoUnSubscribe(const aStream: TwsBinanceStreams;
      const aSymbol: String = ''; const aInterval: String = '')
      : Integer; virtual;
    { helpers }

    { constructor/destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor/destructor }

    { options }
  private
    FBinance: TsgcWSBinance_Options;
    procedure SetBinance(const Value: TsgcWSBinance_Options);
  public
    property Binance: TsgcWSBinance_Options read FBinance write SetBinance;
    { options }

    { user stream }
  private
    FListenKey: String;
    FTimerUserStream: TsgcTimer;
    function DoRequestListenKey(const aMethod
      : TsgcWSBinanceListenKeyMethod): string;
  private
    procedure DoDeleteListenKey;
    function DoGetListenKey: string;
    procedure DoRequestNewListenKey;
    procedure OnKeepAliveUserStream(Sender: TObject);
    procedure OnKeepAliveUserStreamException(Sender: TObject; E: Exception);
  protected
    procedure DoStartKeepAliveUserStream; virtual;
    procedure DoStopKeepAliveUserStream; virtual;
  public
    property ListenKey: string read FListenKey;
    { user stream }

    { events }
  private
    FOnBinanceHTTPException: TsgcWSBinanceHTTPExceptionEvent;
  protected
    procedure DoBinanceHTTPExceptionEvent(E: Exception); virtual;
  public
    property OnBinanceHTTPException: TsgcWSBinanceHTTPExceptionEvent
      read FOnBinanceHTTPException write FOnBinanceHTTPException;
  end;

  TsgcWS_API_Binance = class(TsgcWS_API_Binance_Base)
    { from TsgcWSAPI_client }
  protected
    function GetURL: String; override;
    { from TsgcWSAPI_client }

    { url }
  protected
    function GetURL_BaseAPI: String; override;
    function GetURL_ListenKey: String; override;
    { url }

    { constructor/destructor }
  public
    destructor Destroy; override;
    { constructor/destructor }

    { subscribe methods }
  public
    function SubscribeAggregateTrades(const aSymbol: String): Integer;
    function UnSubscribeAggregateTrades(const aSymbol: String): Integer;
  public
    function SubscribeTrades(const aSymbol: String): Integer;
    function UnSubscribeTrades(const aSymbol: String): Integer;
  public
    function SubscribeKLine(const aSymbol: String;
      const aInterval: TsgcWSBinanceChartIntervals): Integer;
    function UnSubscribeKLine(const aSymbol: String;
      const aInterval: TsgcWSBinanceChartIntervals): Integer;
  public
    function SubscribeMiniTicker(const aSymbol: String): Integer;
    function UnSubscribeMiniTicker(const aSymbol: String): Integer;
  public
    function SubscribeAllMiniTickers: Integer;
    function UnSubscribeAllMiniTickers: Integer;
  public
    function SubscribeTicker(const aSymbol: String): Integer;
    function UnSubscribeTicker(const aSymbol: String): Integer;
  public
    function SubscribeAllMarketTickers: Integer;
    function UnSubscribeAllMarketTickers: Integer;
  public
    function SubscribeBookTicker(const aSymbol: String): Integer;
    function UnSubscribeBookTicker(const aSymbol: String): Integer;
  public
    function SubscribeAllBookTickers: Integer;
    function UnSubscribeAllBookTickers: Integer;
  public
    function SubscribePartialBookDepth(const aSymbol: String;
      const aDepth: TsgcWSBinanceDepthLevels;
      aUpdateSpeed: string = ''): Integer;
    function UnSubscribePartialBookDepth(const aSymbol: String;
      const aDepth: TsgcWSBinanceDepthLevels): Integer;
  public
    function SubscribeDiffDepth(const aSymbol: String): Integer;
    function UnSubscribeDiffDepth(const aSymbol: String): Integer;
    { subscribe methods }

    { methods }
  public
    function ListSubscriptions: Integer;
    { methods }

    { properties }
  private
    FREST_API: TsgcHTTP_API_Binance_Rest;
    function GetREST_API: TsgcHTTP_API_Binance_Rest;
  protected
    procedure OnHTTPAPIExceptionEvent(Sender: TObject; E: Exception); virtual;
  public
    property REST_API: TsgcHTTP_API_Binance_Rest read GetREST_API
      write FREST_API;
    { properties }
  end;

  TsgcWS_API_Binance_Futures = class(TsgcWS_API_Binance_Base)
    { from TsgcWSAPI_client }
  protected
    function GetURL: String; override;
    { from TsgcWSAPI_client }

    { url }
  protected
    function GetURL_BaseAPI: String; override;
    function GetURL_ListenKey: String; override;
    { url }

    { methods }
  public
    function SubscribeAggregateTrades(const aSymbol: String): Integer;
    function UnSubscribeAggregateTrades(const aSymbol: String): Integer;
  public
    function SubscribeMarkPrice(const aSymbol: String;
      const aUpdateSpeed: String = '@1s'): Integer;
    function UnSubscribeMarkPrice(const aSymbol: String): Integer;
  public
    function SubscribeMarkPriceAllMarket(const aSymbol: String;
      const aUpdateSpeed: String = '@1s'): Integer;
    function UnSubscribeMarkPriceAllMarket(const aSymbol: String): Integer;
  public
    function SubscribeKLine(const aSymbol: String;
      const aInterval: TsgcWSBinanceChartIntervals): Integer;
    function UnSubscribeKLine(const aSymbol: String;
      const aInterval: TsgcWSBinanceChartIntervals): Integer;
  public
    function SubscribeMiniTicker(const aSymbol: String): Integer;
    function UnSubscribeMiniTicker(const aSymbol: String): Integer;
  public
    function SubscribeAllMiniTickers: Integer;
    function UnSubscribeAllMiniTickers: Integer;
  public
    function SubscribeTicker(const aSymbol: String): Integer;
    function UnSubscribeTicker(const aSymbol: String): Integer;
  public
    function SubscribeAllMarketTickers: Integer;
    function UnSubscribeAllMarketTickers: Integer;
  public
    function SubscribeBookTicker(const aSymbol: String): Integer;
    function UnSubscribeBookTicker(const aSymbol: String): Integer;
  public
    function SubscribeAllBookTickers: Integer;
    function UnSubscribeAllBookTickers: Integer;
  public
    function SubscribeLiquidationOrders(const aSymbol: String): Integer;
    function UnSubscribeLiquidationOrders(const aSymbol: String): Integer;
  public
    function SubscribeAllMarketLiquidationOrders: Integer;
    function UnSubscribeAllMarketLiquidationOrders: Integer;
  public
    function SubscribePartialBookDepth(const aSymbol: String;
      const aDepth: TsgcWSBinanceDepthLevels;
      const aUpdateSpeed: string = ''): Integer;
    function UnSubscribePartialBookDepth(const aSymbol: String;
      const aDepth: TsgcWSBinanceDepthLevels): Integer;
  public
    function SubscribeDiffDepth(const aSymbol: String;
      const aUpdateSpeed: string = ''): Integer;
    function UnSubscribeDiffDepth(const aSymbol: String): Integer;
    { methods }

    { properties }
  private
    FFuturesContracts: TsgcWSBinanceFuturesContracts;
    FREST_API: TsgcHTTP_API_Binance_Futures_Rest;
    function GetREST_API: TsgcHTTP_API_Binance_Futures_Rest;
  protected
    procedure OnHTTPAPIExceptionEvent(Sender: TObject; E: Exception); virtual;
  public
    property FuturesContracts: TsgcWSBinanceFuturesContracts
      read FFuturesContracts write FFuturesContracts;
    property REST_API: TsgcHTTP_API_Binance_Futures_Rest read GetREST_API
      write FREST_API;
    { properties }
  end;
{$ENDIF}

implementation

{$IFDEF SGC_APIS}

uses
  StrUtils,
{$IFDEF SGC_INDY}sgcIdHTTP{$ELSE}IdHTTP{$ENDIF},
  // sgc
  sgcWebSocket_Helpers, sgcWebSocket_Const, sgcHTTP_Client, sgcJSON;

resourcestring
  S_BINANCE_ERROR_NOT_SUPPORTED_BINANCE_US =
    'Method Not supported by Binance.US';

Const
  CS_BNC_WS_URL = 'wss://stream.binance.com:9443';
  CS_BNC_US_WS_URL = 'wss://stream.binance.us:9443';
  CS_BNC_BASE_API = 'https://api.binance.com';
  CS_BNC_US_BASE_API = 'https://api.binance.us';
  CS_BNC_METHOD_LISTENKEY = '/api/v3/userDataStream';

Const
  CS_BNC_TESTNET_WS_URL = 'wss://testnet.binance.vision';
  CS_BNC_TESTNET_BASE_API = 'https://testnet.binance.vision';
  CS_BNC_TESTNET_METHOD_LISTENKEY = '/api/v3/userDataStream';

Const
  CS_BNC_FUTURES_USDT_WS_URL = 'wss://fstream.binance.com/ws';
  CS_BNC_FUTURES_USDT_BASE_API = 'https://fapi.binance.com';
  CS_BNC_FUTURES_USDT_METHOD_LISTENKEY = '/fapi/v1/listenKey';

Const
  CS_BNC_FUTURES_TESTNET_USDT_WS_URL = 'wss://stream.binancefuture.com';
  CS_BNC_FUTURES_TESTNET_USDT_BASE_API = 'https://testnet.binancefuture.com';
  CS_BNC_FUTURES_TESTNET_USDT_METHOD_LISTENKEY = '/fapi/v1/listenKey';

Const
  CS_BNC_FUTURES_COIN_WS_URL = 'wss://dstream.binance.com';
  CS_BNC_FUTURES_COIN_BASE_API = 'https://dapi.binance.com';
  CS_BNC_FUTURES_COIN_METHOD_LISTENKEY = '/dapi/v1/listenKey';

Const
  CS_BNC_FUTURES_TESTNET_COIN_WS_URL = 'wss://dstream.binancefuture.com';
  CS_BNC_FUTURES_TESTNET_COIN_BASE_API = 'https://testnet.binancefuture.com';
  CS_BNC_FUTURES_TESTNET_COIN_METHOD_LISTENKEY = '/dapi/v1/listenKey';

Const
  CS_BNC_AGGREGATE_TRADE = 'aggTrade';
  CS_BNC_TRADE = 'trade';
  CS_BNC_KLINE = 'kline_';
  CS_BNC_MINITICKER = 'miniTicker';
  CS_BNC_ALL_MINITICKERS = '!miniTicker@arr';
  CS_BNC_TICKER = 'ticker';
  CS_BNC_ALL_MARKET_TICKERS = '!ticker@arr';
  CS_BNC_BOOK_TICKER = 'bookTicker';
  CS_BNC_ALL_BOOK_TICKERS = '!bookTicker';
  CS_BNC_DEPTH = 'depth';
  CS_BNC_MARK_PRICE = 'markPrice';
  CS_BNC_ALL_MARKET_MARK_PRICE = '!markPrice@arr';
  CS_BNC_LIQUIDATION_ORDER = 'forceOrder';
  CS_BNC_ALL_MARKET_LIQUIDATION_ORDER = '!forceOrder@arr';

Const
  TwsBinanceStreams_String: array [0 .. 13] of string = (CS_BNC_AGGREGATE_TRADE,
    CS_BNC_TRADE, CS_BNC_KLINE, CS_BNC_MINITICKER, CS_BNC_ALL_MINITICKERS,
    CS_BNC_TICKER, CS_BNC_ALL_MARKET_TICKERS, CS_BNC_BOOK_TICKER,
    CS_BNC_ALL_BOOK_TICKERS, CS_BNC_DEPTH, CS_BNC_MARK_PRICE,
    CS_BNC_ALL_MARKET_MARK_PRICE, CS_BNC_LIQUIDATION_ORDER,
    CS_BNC_ALL_MARKET_LIQUIDATION_ORDER);

destructor TsgcWS_API_Binance.Destroy;
begin
  sgcFree(FREST_API);
  inherited;
end;

function TsgcWS_API_Binance.GetREST_API: TsgcHTTP_API_Binance_Rest;
begin
  if not Assigned(FREST_API) then
  begin
    FREST_API := TsgcHTTP_API_Binance_Rest.Create(nil);
    if Assigned(FCLient) then
    begin
      FREST_API.TLSOptions.OpenSSL_Options.APIVersion :=
        FCLient.TLSOptions.OpenSSL_Options.APIVersion;
      FREST_API.TLSOptions.IOHandler := FCLient.TLSOptions.IOHandler;
      FREST_API.TLSOptions.Version := FCLient.TLSOptions.Version;
    end;
  end;
  FREST_API.BinanceOptions.ApiKey := Binance.ApiKey;
  FREST_API.BinanceOptions.ApiSecret := Binance.ApiSecret;
  FREST_API.BinanceOptions.TestNet := Binance.TestNet;
  if FREST_API.BinanceOptions.LogOptions.Enabled <> Binance.HTTPLogOptions.REST.Enabled
  then
  begin
    FREST_API.BinanceOptions.LogOptions.Enabled :=
      Binance.HTTPLogOptions.REST.Enabled;
    FREST_API.BinanceOptions.LogOptions.FileName :=
      Binance.HTTPLogOptions.REST.FileName;
  end;
  if Assigned(FOnBinanceHTTPException) then
    FREST_API.OnHTTPAPIException := OnHTTPAPIExceptionEvent
  else
    FREST_API.OnHTTPAPIException := nil;
  Result := FREST_API;
end;

function TsgcWS_API_Binance.GetURL: String;
begin
  if Binance.BinanceUS then
  begin
    if Binance.TestNet then
      raise Exception.Create(S_BINANCE_ERROR_NOT_SUPPORTED_BINANCE_US)
    else
      Result := CS_BNC_US_WS_URL;
  end
  else
  begin
    if Binance.TestNet then
      Result := CS_BNC_TESTNET_WS_URL
    else
      Result := CS_BNC_WS_URL;
  end;
  if Binance.UseCombinedStreams then
    Result := Result + '/stream'
  else
    Result := Result + '/ws'
end;

function TsgcWS_API_Binance.GetURL_BaseAPI: String;
begin
  if Binance.BinanceUS then
  begin
    if Binance.TestNet then
      raise Exception.Create(S_BINANCE_ERROR_NOT_SUPPORTED_BINANCE_US)
    else
      Result := CS_BNC_US_BASE_API;
  end
  else
  begin
    if Binance.TestNet then
      Result := CS_BNC_TESTNET_BASE_API
    else
      Result := CS_BNC_BASE_API;
  end;
end;

function TsgcWS_API_Binance.GetURL_ListenKey: String;
begin
  if Binance.TestNet then
    Result := CS_BNC_TESTNET_METHOD_LISTENKEY
  else
    Result := CS_BNC_METHOD_LISTENKEY;
end;

function TsgcWS_API_Binance.ListSubscriptions: Integer;
begin
  Result := 0;
  if Assigned(FCLient) then
  begin
    Result := GetNewId;
    FCLient.WriteData(Format('{"method": "LIST_SUBSCRIPTIONS", "id": %d}',
      [Result]));
  end;
end;

procedure TsgcWS_API_Binance.OnHTTPAPIExceptionEvent(Sender: TObject;
  E: Exception);
begin
  DoBinanceHTTPExceptionEvent(E);
end;

function TsgcWS_API_Binance.SubscribeAggregateTrades(const aSymbol
  : String): Integer;
begin
  Result := DoSubscribe(bncAggregateTrade, aSymbol);
end;

function TsgcWS_API_Binance.SubscribeAllBookTickers: Integer;
begin
  Result := DoSubscribe(bncAllBookTickers);
end;

function TsgcWS_API_Binance.SubscribeAllMarketTickers: Integer;
begin
  Result := DoSubscribe(bncAllMarketTickers);
end;

function TsgcWS_API_Binance.SubscribeAllMiniTickers: Integer;
begin
  Result := DoSubscribe(bncAllMiniTickers);
end;

function TsgcWS_API_Binance.SubscribeBookTicker(const aSymbol: String): Integer;
begin
  Result := DoSubscribe(bncBookTicker, aSymbol);
end;

function TsgcWS_API_Binance.SubscribeDiffDepth(const aSymbol: String): Integer;
begin
  Result := DoSubscribe(bncDiffDepth, aSymbol);
end;

function TsgcWS_API_Binance.SubscribeKLine(const aSymbol: String;
  const aInterval: TsgcWSBinanceChartIntervals): Integer;
begin
  Result := DoSubscribe(bncKline, aSymbol, GetIntervalString(aInterval));
end;

function TsgcWS_API_Binance.SubscribeMiniTicker(const aSymbol: String): Integer;
begin
  Result := DoSubscribe(bncMiniTicker, aSymbol);
end;

function TsgcWS_API_Binance.SubscribePartialBookDepth(const aSymbol: String;
  const aDepth: TsgcWSBinanceDepthLevels; aUpdateSpeed: string = ''): Integer;
begin
  Result := DoSubscribe(bncPartialBookDepth, aSymbol, GetDepthString(aDepth) +
    aUpdateSpeed);
end;

function TsgcWS_API_Binance.SubscribeTicker(const aSymbol: String): Integer;
begin
  Result := DoSubscribe(bncTicker, aSymbol);
end;

function TsgcWS_API_Binance.SubscribeTrades(const aSymbol: String): Integer;
begin
  Result := DoSubscribe(bncTrade, aSymbol);
end;

function TsgcWS_API_Binance.UnSubscribeAggregateTrades(const aSymbol
  : String): Integer;
begin
  Result := DoUnSubscribe(bncAggregateTrade, aSymbol);
end;

function TsgcWS_API_Binance.UnSubscribeAllBookTickers: Integer;
begin
  Result := DoUnSubscribe(bncAllBookTickers);
end;

function TsgcWS_API_Binance.UnSubscribeAllMarketTickers: Integer;
begin
  Result := DoUnSubscribe(bncAllMarketTickers);
end;

function TsgcWS_API_Binance.UnSubscribeAllMiniTickers: Integer;
begin
  Result := DoUnSubscribe(bncAllMiniTickers);
end;

function TsgcWS_API_Binance.UnSubscribeBookTicker(const aSymbol
  : String): Integer;
begin
  Result := DoUnSubscribe(bncBookTicker, aSymbol);
end;

function TsgcWS_API_Binance.UnSubscribeDiffDepth(const aSymbol: String)
  : Integer;
begin
  Result := DoUnSubscribe(bncDiffDepth, aSymbol);
end;

function TsgcWS_API_Binance.UnSubscribeKLine(const aSymbol: String;
  const aInterval: TsgcWSBinanceChartIntervals): Integer;
begin
  Result := DoUnSubscribe(bncKline, aSymbol,
    '_' + GetIntervalString(aInterval));
end;

function TsgcWS_API_Binance.UnSubscribeMiniTicker(const aSymbol
  : String): Integer;
begin
  Result := DoUnSubscribe(bncMiniTicker, aSymbol);
end;

function TsgcWS_API_Binance.UnSubscribePartialBookDepth(const aSymbol: String;
  const aDepth: TsgcWSBinanceDepthLevels): Integer;
begin
  Result := DoUnSubscribe(bncPartialBookDepth, aSymbol, GetDepthString(aDepth));
end;

function TsgcWS_API_Binance.UnSubscribeTicker(const aSymbol: String): Integer;
begin
  Result := DoUnSubscribe(bncTicker, aSymbol);
end;

function TsgcWS_API_Binance.UnSubscribeTrades(const aSymbol: String): Integer;
begin
  Result := DoUnSubscribe(bncTrade, aSymbol);
end;

constructor TsgcWSBinance_Options.Create;
begin
  inherited;
  FHTTPLogOptions := TsgcWSBinanceLog_Options.Create;
  TestNet := False;
  UserStream := True;
  BinanceUS := False;
  ListenKeyOnDisconnect := blkodDeleteListenKey;
  UseCombinedStreams := False;
end;

destructor TsgcWSBinance_Options.Destroy;
begin
  sgcFree(FHTTPLogOptions);
  inherited;
end;

procedure TsgcWSBinance_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSBinance_Options then
  begin
    ApiKey := TsgcWSBinance_Options(aSource).ApiKey;
    ApiSecret := TsgcWSBinance_Options(aSource).ApiSecret;
    HTTPLogOptions := TsgcWSBinance_Options(aSource).HTTPLogOptions;
    TestNet := TsgcWSBinance_Options(aSource).TestNet;
    UserStream := TsgcWSBinance_Options(aSource).UserStream;
    BinanceUS := TsgcWSBinance_Options(aSource).BinanceUS;
    ListenKeyOnDisconnect := TsgcWSBinance_Options(aSource)
      .ListenKeyOnDisconnect;
    UseCombinedStreams := TsgcWSBinance_Options(aSource).UseCombinedStreams;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSBinance_Options.SetHTTPLogOptions
  (const Value: TsgcWSBinanceLog_Options);
begin
  if Assigned(FHTTPLogOptions) then
    FHTTPLogOptions.Assign(Value);
end;

constructor TsgcWSBinanceLog_Options.Create;
begin
  inherited;
  FREST := TsgcWSBinanceRestLog_Options.Create;
end;

destructor TsgcWSBinanceLog_Options.Destroy;
begin
  sgcFree(FREST);
  inherited;
end;

procedure TsgcWSBinanceLog_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSBinanceLog_Options then
  begin
    Enabled := TsgcWSBinanceLog_Options(aSource).Enabled;
    FileName := TsgcWSBinanceLog_Options(aSource).FileName;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSBinanceLog_Options.SetREST(const Value
  : TsgcWSBinanceRestLog_Options);
begin
  if Assigned(FREST) then
    FREST.Assign(Value);
end;

procedure TsgcWSBinanceRestLog_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSBinanceRestLog_Options then
  begin
    Enabled := TsgcWSBinanceRestLog_Options(aSource).Enabled;
    FileName := TsgcWSBinanceRestLog_Options(aSource).FileName;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcWS_API_Binance_Base.Create(aOwner: TComponent);
begin
  inherited;
  FBinance := TsgcWSBinance_Options.Create;
  RawMessages := True;
  FListenKey := '';
end;

destructor TsgcWS_API_Binance_Base.Destroy;
begin
  sgcFree(FBinance);
  inherited;
end;

procedure TsgcWS_API_Binance_Base.DoBeforeConnect;
begin
  inherited;
  FId := 0;
  // ... assign again URL because UseCombinedStreams can be different
  FClient.URL := GetURL;
  // ... request new listening key
  DoRequestNewListenKey;
end;

procedure TsgcWS_API_Binance_Base.DoBinanceHTTPExceptionEvent(E: Exception);
begin
  if Assigned(FOnBinanceHTTPException) then
    FOnBinanceHTTPException(self, E);
end;

procedure TsgcWS_API_Binance_Base.DoDeleteListenKey;
begin
  DoRequestListenKey(blkmDeleteListenKey);
end;

function TsgcWS_API_Binance_Base.DoGetListenKey: string;
begin
  Result := DoRequestListenKey(blkmGetListenKey);
end;

procedure TsgcWS_API_Binance_Base.DoNotifyDisconnect
  (aConnection: TsgcWSConnection);
begin
  if not IsDestroying then
  begin
    if FListenKey <> '' then
    begin
      case Binance.ListenKeyOnDisconnect of
        blkodDeleteListenKey:
          DoDeleteListenKey;
        blkodClearListenKey:
          FListenKey := '';
      end;
    end;
  end;
  DoStopKeepAliveUserStream;
  inherited;
end;

function TsgcWS_API_Binance_Base.DoRequestListenKey(const aMethod
  : TsgcWSBinanceListenKeyMethod): string;
var
  oHTTP: TsgcIdHTTP;
  oRequest, oResponse: TStringStream;
  oJSON: TsgcJSON;
begin
  Result := '';

  oHTTP := TsgcIdHTTP.Create(nil);
  Try
    Try
      if Assigned(FCLient) then
      begin
        oHTTP.TLSOptions.OpenSSL_Options.APIVersion :=
          FCLient.TLSOptions.OpenSSL_Options.APIVersion;
        oHTTP.TLSOptions.IOHandler := FCLient.TLSOptions.IOHandler;
        oHTTP.TLSOptions.Version := FCLient.TLSOptions.Version;
      end
      else
        oHTTP.TLSOptions.Version := tls1_2;
      if Binance.HTTPLogOptions.Enabled then
      begin
        oHTTP.LogOptions.FileName := Binance.HTTPLogOptions.FileName;
        oHTTP.Log := True;
      end;
      oHTTP.Request.CustomHeaders.Add('X-MBX-APIKEY: ' + Binance.ApiKey);
      case aMethod of
        // ... get listen key
        blkmGetListenKey:
          begin
            oRequest := TStringStream.Create('');
            oResponse := TStringStream.Create('');
            Try
              oHTTP.Post(GetURL_BaseAPI + GetURL_ListenKey, oRequest,
                oResponse);
              if oResponse.DataString <> '' then
              begin
                oJSON := TsgcJSON.Create(nil);
                Try
                  oJSON.Read(oResponse.DataString);
                  if oJSON.Node['listenKey'] <> nil then
                  begin
                    Result := oJSON.Node['listenKey'].Value;
                    DoStartKeepAliveUserStream;
                  end;
                Finally
                  sgcFree(oJSON);
                End;
              end;
            Finally
              sgcFree(oRequest);
              begin
              end;
              sgcFree(oResponse);
            End;
          end;
        // ... delete listen key
        blkmDeleteListenKey:
          begin
            oResponse := TStringStream.Create('');
            Try
{$IFDEF INDY10_6_0_5122}
              oHTTP.Delete(GetURL_BaseAPI + GetURL_ListenKey + '?listenKey=' +
                FListenKey, oResponse);
              if oResponse.DataString = '{}' then
                FListenKey := '';
{$ELSE}
{$IFDEF INDY10_5_5}
              oHTTP.Delete(GetURL_BaseAPI + GetURL_ListenKey + '?listenKey=' +
                FListenKey);
              FListenKey := '';
{$ENDIF}
{$ENDIF}
            Finally
              sgcFree(oResponse);
            End;
          end;
        // ... refresh listen eky
        blkmRefreshListenKey:
          begin
            oRequest := TStringStream.Create('');
            Try
              oHTTP.Put(GetURL_BaseAPI + GetURL_ListenKey + '?listenKey=' +
                FListenKey, oRequest);
            Finally
              sgcFree(oRequest);
            End;
          end;
      end;
    Finally
      sgcFree(oHTTP);
    End;
  Except
    On E: EIdHTTPProtocolException Do
    begin
      E.Message := E.Message + '. ' + E.ErrorMessage;
      DoBinanceHTTPExceptionEvent(E);
    end;
    On E: Exception do
      DoBinanceHTTPExceptionEvent(E);
  end;
end;

procedure TsgcWS_API_Binance_Base.DoRequestNewListenKey;
begin
  if Binance.UserStream and (Binance.ApiKey <> '') then
  begin
    FListenKey := DoGetListenKey;
    if FListenKey = '' then
      raise Exception.Create('ListenKey cannot be empty.')
    else
    begin
      if Binance.UseCombinedStreams then
        FClient.URL := GetURL + '?streams=' + FListenKey
      else
        FCLient.URL := GetURL + '/' + FListenKey;
    end;
  end;
end;

procedure TsgcWS_API_Binance_Base.DoStartKeepAliveUserStream;
begin
  FTimerUserStream := TsgcTimer.Create;
  FTimerUserStream.DebugName := 'UserStream';
  FTimerUserStream.OnTimer := OnKeepAliveUserStream;
  FTimerUserStream.OnException := OnKeepAliveUserStreamException;
  FTimerUserStream.Interval := 10 * 60 * 1000; // 10 minutes keep-alive
  FTimerUserStream.Enabled := True;
end;

procedure TsgcWS_API_Binance_Base.DoStopKeepAliveUserStream;
begin
  sgcThreadFree(FTimerUserStream);
end;

function TsgcWS_API_Binance_Base.DoSubscribe(const aStream: TwsBinanceStreams;
  const aSymbol: String = ''; const aInterval: String = ''): Integer;
begin
  Result := 0;
  if Assigned(FCLient) then
  begin
    Result := GetNewId;
    FCLient.WriteData
      (Format('{"method": "SUBSCRIBE", "params":["%s"], "id": %d}',
      [GetStream(aStream, aSymbol, aInterval), Result]));
  end;
end;

function TsgcWS_API_Binance_Base.DoUnSubscribe(const aStream: TwsBinanceStreams;
  const aSymbol: String = ''; const aInterval: String = ''): Integer;
begin
  Result := 0;
  if Assigned(FCLient) then
  begin
    Result := GetNewId;
    FCLient.WriteData
      (Format('{"method": "UNSUBSCRIBE", "params":["%s"], "id": %d}',
      [GetStream(aStream, aSymbol, aInterval), Result]));
  end;
end;

function TsgcWS_API_Binance_Base.GetDepthString
  (aDepth: TsgcWSBinanceDepthLevels): String;
begin
  Result := '';
  case aDepth of
    bde5:
      Result := '5';
    bde10:
      Result := '10';
    bde20:
      Result := '20';
  end;
end;

function TsgcWS_API_Binance_Base.GetIntervalString
  (aInterval: TsgcWSBinanceChartIntervals): String;
begin
  Result := '';
  case aInterval of
    bci1m:
      Result := '1m';
    bci3m:
      Result := '3m';
    bci5m:
      Result := '5m';
    bci15m:
      Result := '15m';
    bci30m:
      Result := '30m';
    bci1h:
      Result := '1h';
    bci2h:
      Result := '2h';
    bci4h:
      Result := '4h';
    bci6h:
      Result := '6h';
    bci8h:
      Result := '8h';
    bci12h:
      Result := '12h';
    bci1d:
      Result := '1d';
    bci3d:
      Result := '3d';
    bci1w:
      Result := '1w';
    bci1Mo:
      Result := '1M';
  end;
end;

function TsgcWS_API_Binance_Base.GetNewId: Integer;
begin
  FId := FId + 1;
  Result := FId;
end;

function TsgcWS_API_Binance_Base.GetStream(const aStream: TwsBinanceStreams;
  const aSymbol: String = ''; const aInterval: String = ''): String;
begin
  case aStream of
    bncAggregateTrade:
      Result := aSymbol + '@' + CS_BNC_AGGREGATE_TRADE;
    bncTrade:
      Result := aSymbol + '@' + CS_BNC_TRADE;
    bncKline:
      Result := aSymbol + '@' + CS_BNC_KLINE + aInterval;
    bncMiniTicker:
      Result := aSymbol + '@' + CS_BNC_MINITICKER;
    bncAllMiniTickers:
      Result := CS_BNC_ALL_MINITICKERS;
    bncTicker:
      Result := aSymbol + '@' + CS_BNC_TICKER;
    bncAllMarketTickers:
      Result := CS_BNC_ALL_MARKET_TICKERS;
    bncBookTicker:
      Result := aSymbol + '@' + CS_BNC_BOOK_TICKER;
    bncAllBookTickers:
      Result := CS_BNC_ALL_BOOK_TICKERS;
    bncPartialBookDepth:
      Result := aSymbol + '@' + CS_BNC_DEPTH + aInterval;
    bncDiffDepth:
      Result := aSymbol + '@' + CS_BNC_DEPTH;
    bncMarkPrice:
      Result := aSymbol + '@' + CS_BNC_MARK_PRICE + aInterval;
    bncAllMarketMarkPrice:
      Result := CS_BNC_ALL_MARKET_MARK_PRICE;
    bncLiquidationOrder:
      Result := aSymbol + '@' + CS_BNC_LIQUIDATION_ORDER;
    bncAllMarketLiquidationOrder:
      Result := CS_BNC_ALL_MARKET_LIQUIDATION_ORDER
  end;
end;

function TsgcWS_API_Binance_Base.IsInternalMessage(const aMessage
  : String): Boolean;
begin
  // ... if listen key has expired request a new one
  if LeftStr(aMessage, Length('{"e": "listenKeyExpired"')) = '{"e": "listenKeyExpired"'
  then
  begin
    Result := True;
    DoRequestNewListenKey;
  end
  else
    Result := False;
end;

procedure TsgcWS_API_Binance_Base.OnKeepAliveUserStream(Sender: TObject);
begin
  DoRequestListenKey(blkmRefreshListenKey);
end;

procedure TsgcWS_API_Binance_Base.OnKeepAliveUserStreamException
  (Sender: TObject; E: Exception);
begin
  DoBinanceHTTPExceptionEvent(E);
end;

procedure TsgcWS_API_Binance_Base.SetBinance(const Value
  : TsgcWSBinance_Options);
begin
  if Assigned(FBinance) then
    FBinance.Assign(Value);
end;

function TsgcWS_API_Binance_Futures.GetREST_API
  : TsgcHTTP_API_Binance_Futures_Rest;
begin
  if not Assigned(FREST_API) then
    FREST_API := TsgcHTTP_API_Binance_Futures_Rest.Create(nil);
  FREST_API.BinanceOptions.ApiKey := Binance.ApiKey;
  FREST_API.BinanceOptions.ApiSecret := Binance.ApiSecret;
  FREST_API.BinanceOptions.TestNet := Binance.TestNet;
  if FREST_API.BinanceOptions.LogOptions.Enabled <> Binance.HTTPLogOptions.REST.Enabled
  then
  begin
    FREST_API.BinanceOptions.LogOptions.Enabled :=
      Binance.HTTPLogOptions.REST.Enabled;
    FREST_API.BinanceOptions.LogOptions.FileName :=
      Binance.HTTPLogOptions.REST.FileName;
  end;
  case FuturesContracts of
    bfcUSDT:
      FREST_API.FuturesContracts := bfchUSDT;
    bfcCOIN:
      FREST_API.FuturesContracts := bfchCOIN;
  end;
  if Assigned(FOnBinanceHTTPException) then
    FREST_API.OnHTTPAPIException := OnHTTPAPIExceptionEvent
  else
    FREST_API.OnHTTPAPIException := nil;

  Result := FREST_API;
end;

function TsgcWS_API_Binance_Futures.GetURL: String;
begin
  if Binance.BinanceUS then
    raise Exception.Create(S_BINANCE_ERROR_NOT_SUPPORTED_BINANCE_US);

  case FuturesContracts of
    bfcUSDT:
      begin
        if Binance.TestNet then
          Result := CS_BNC_FUTURES_TESTNET_USDT_WS_URL
        else
          Result := CS_BNC_FUTURES_USDT_WS_URL;
      end;
    bfcCOIN:
      begin
        if Binance.TestNet then
          Result := CS_BNC_FUTURES_TESTNET_COIN_WS_URL
        else
          Result := CS_BNC_FUTURES_COIN_WS_URL;
      end;
  end;

  if Binance.UseCombinedStreams then
    Result := Result + '/stream'
  else
    Result := Result + '/ws';
end;

function TsgcWS_API_Binance_Futures.GetURL_BaseAPI: String;
begin
  case FuturesContracts of
    bfcUSDT:
      begin
        if Binance.TestNet then
          Result := CS_BNC_FUTURES_TESTNET_USDT_BASE_API
        else
          Result := CS_BNC_FUTURES_USDT_BASE_API;
      end;
    bfcCOIN:
      begin
        if Binance.TestNet then
          Result := CS_BNC_FUTURES_TESTNET_COIN_BASE_API
        else
          Result := CS_BNC_FUTURES_COIN_BASE_API;
      end;
  end;
end;

function TsgcWS_API_Binance_Futures.GetURL_ListenKey: String;
begin
  if Binance.BinanceUS then
    raise Exception.Create(S_BINANCE_ERROR_NOT_SUPPORTED_BINANCE_US);

  case FuturesContracts of
    bfcUSDT:
      begin
        if Binance.TestNet then
          Result := CS_BNC_FUTURES_TESTNET_USDT_METHOD_LISTENKEY
        else
          Result := CS_BNC_FUTURES_USDT_METHOD_LISTENKEY
      end;
    bfcCOIN:
      begin
        if Binance.TestNet then
          Result := CS_BNC_FUTURES_TESTNET_COIN_METHOD_LISTENKEY
        else
          Result := CS_BNC_FUTURES_COIN_METHOD_LISTENKEY;
      end;
  end;
end;

procedure TsgcWS_API_Binance_Futures.OnHTTPAPIExceptionEvent(Sender: TObject;
  E: Exception);
begin
  DoBinanceHTTPExceptionEvent(E);
end;

function TsgcWS_API_Binance_Futures.SubscribeAggregateTrades
  (const aSymbol: String): Integer;
begin
  Result := DoSubscribe(bncAggregateTrade, aSymbol);
end;

function TsgcWS_API_Binance_Futures.SubscribeAllBookTickers: Integer;
begin
  Result := DoSubscribe(bncAllBookTickers);
end;

function TsgcWS_API_Binance_Futures.SubscribeAllMarketLiquidationOrders
  : Integer;
begin
  Result := DoSubscribe(bncAllMarketLiquidationOrder);
end;

function TsgcWS_API_Binance_Futures.SubscribeAllMarketTickers: Integer;
begin
  Result := DoSubscribe(bncAllMarketTickers);
end;

function TsgcWS_API_Binance_Futures.SubscribeAllMiniTickers: Integer;
begin
  Result := DoSubscribe(bncAllMiniTickers);
end;

function TsgcWS_API_Binance_Futures.SubscribeBookTicker(const aSymbol
  : String): Integer;
begin
  Result := DoSubscribe(bncBookTicker, aSymbol);
end;

function TsgcWS_API_Binance_Futures.SubscribeDiffDepth(const aSymbol: String;
  const aUpdateSpeed: string = ''): Integer;
begin
  Result := DoSubscribe(bncDiffDepth, aSymbol + aUpdateSpeed);
end;

function TsgcWS_API_Binance_Futures.SubscribeKLine(const aSymbol: String;
  const aInterval: TsgcWSBinanceChartIntervals): Integer;
begin
  Result := DoSubscribe(bncKline, aSymbol, GetIntervalString(aInterval));
end;

function TsgcWS_API_Binance_Futures.SubscribeLiquidationOrders
  (const aSymbol: String): Integer;
begin
  Result := DoSubscribe(bncLiquidationOrder, aSymbol);
end;

function TsgcWS_API_Binance_Futures.SubscribeMarkPrice(const aSymbol: String;
  const aUpdateSpeed: String = '@1s'): Integer;
begin
  Result := DoSubscribe(bncMarkPrice, aSymbol, aUpdateSpeed);
end;

function TsgcWS_API_Binance_Futures.SubscribeMarkPriceAllMarket
  (const aSymbol: String; const aUpdateSpeed: String = '@1s'): Integer;
begin
  Result := DoSubscribe(bncAllMarketMarkPrice, '', aUpdateSpeed);
end;

function TsgcWS_API_Binance_Futures.SubscribeMiniTicker(const aSymbol
  : String): Integer;
begin
  Result := DoSubscribe(bncMiniTicker, aSymbol);
end;

function TsgcWS_API_Binance_Futures.SubscribePartialBookDepth
  (const aSymbol: String; const aDepth: TsgcWSBinanceDepthLevels;
  const aUpdateSpeed: string = ''): Integer;
begin
  Result := DoSubscribe(bncPartialBookDepth, aSymbol, GetDepthString(aDepth) +
    aUpdateSpeed);
end;

function TsgcWS_API_Binance_Futures.SubscribeTicker(const aSymbol
  : String): Integer;
begin
  Result := DoSubscribe(bncTicker, aSymbol);
end;

function TsgcWS_API_Binance_Futures.UnSubscribeAggregateTrades
  (const aSymbol: String): Integer;
begin
  Result := DoUnSubscribe(bncAggregateTrade, aSymbol);
end;

function TsgcWS_API_Binance_Futures.UnSubscribeAllBookTickers: Integer;
begin
  Result := DoUnSubscribe(bncAllBookTickers);
end;

function TsgcWS_API_Binance_Futures.
  UnSubscribeAllMarketLiquidationOrders: Integer;
begin
  Result := DoUnSubscribe(bncAllMarketLiquidationOrder);
end;

function TsgcWS_API_Binance_Futures.UnSubscribeAllMarketTickers: Integer;
begin
  Result := DoUnSubscribe(bncAllMarketTickers);
end;

function TsgcWS_API_Binance_Futures.UnSubscribeAllMiniTickers: Integer;
begin
  Result := DoUnSubscribe(bncAllMiniTickers);
end;

function TsgcWS_API_Binance_Futures.UnSubscribeBookTicker
  (const aSymbol: String): Integer;
begin
  Result := DoUnSubscribe(bncBookTicker, aSymbol);
end;

function TsgcWS_API_Binance_Futures.UnSubscribeDiffDepth(const aSymbol
  : String): Integer;
begin
  Result := DoUnSubscribe(bncDiffDepth, aSymbol);
end;

function TsgcWS_API_Binance_Futures.UnSubscribeKLine(const aSymbol: String;
  const aInterval: TsgcWSBinanceChartIntervals): Integer;
begin
  Result := DoUnSubscribe(bncKline, aSymbol,
    '_' + GetIntervalString(aInterval));
end;

function TsgcWS_API_Binance_Futures.UnSubscribeLiquidationOrders
  (const aSymbol: String): Integer;
begin
  Result := DoUnSubscribe(bncLiquidationOrder, aSymbol);
end;

function TsgcWS_API_Binance_Futures.UnSubscribeMarkPrice(const aSymbol
  : String): Integer;
begin
  Result := DoUnSubscribe(bncMarkPrice, aSymbol);
end;

function TsgcWS_API_Binance_Futures.UnSubscribeMarkPriceAllMarket
  (const aSymbol: String): Integer;
begin
  Result := DoUnSubscribe(bncAllMarketMarkPrice);
end;

function TsgcWS_API_Binance_Futures.UnSubscribeMiniTicker
  (const aSymbol: String): Integer;
begin
  Result := DoUnSubscribe(bncMiniTicker, aSymbol);
end;

function TsgcWS_API_Binance_Futures.UnSubscribePartialBookDepth
  (const aSymbol: String; const aDepth: TsgcWSBinanceDepthLevels): Integer;
begin
  Result := DoUnSubscribe(bncPartialBookDepth, aSymbol, GetDepthString(aDepth));
end;

function TsgcWS_API_Binance_Futures.UnSubscribeTicker(const aSymbol
  : String): Integer;
begin
  Result := DoUnSubscribe(bncTicker, aSymbol);
end;
{$ENDIF}

end.
