{ ***************************************************************************
  sgcHTTP API component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcHTTP_API_FTX;

interface

{$I sgcVer.inc}
{$IFDEF SGC_HTTP}

uses
  Classes, SysUtils,
  // sgc
  sgcHTTP_API;

type

  TsgcHTTPFTXResolution = (ftxr15, ftxr60, ftxr300, ftxr900, ftxr3600,
    ftxr14400, ftxr86400);
  TsgcHTTPFTXTriggerOrderType = (fttotNone, fttotStop, fttotTrailing_Stop,
    fttotTake_Profit);
  TsgcHTTPFTXOrderSide = (ftosNone, ftosBuy, ftosSell);
  TsgcHTTPFTXOrderType = (ftotNone, ftotMarket, ftotLimit);

  TsgcHTTPFTXOrder = class
  private
    FClientId: string;
    FIOC: Boolean;
    FMarket: string;
    FPostOnly: Boolean;
    FPrice: Extended;
    FReduceOnly: Boolean;
    FSide: TsgcHTTPFTXOrderSide;
    FSize: Extended;
    F_Type: TsgcHTTPFTXOrderType;
  protected
    function GetJSON: string; virtual;
  public
    constructor Create; virtual;
  public
    property ClientId: string read FClientId write FClientId;
    property IOC: Boolean read FIOC write FIOC;
    property Market: string read FMarket write FMarket;
    property PostOnly: Boolean read FPostOnly write FPostOnly;
    property Price: Extended read FPrice write FPrice;
    property ReduceOnly: Boolean read FReduceOnly write FReduceOnly;
    property Side: TsgcHTTPFTXOrderSide read FSide write FSide;
    property Size: Extended read FSize write FSize;
    property _Type: TsgcHTTPFTXOrderType read F_Type write F_Type;
  end;

  TsgcHTTPFTXTriggerOrder = class
  private
    FMarket: string;
    FOrderPrice: Extended;
    FReduceOnly: Boolean;
    FRetryUntilFilled: Boolean;
    FSide: TsgcHTTPFTXOrderSide;
    FSize: Extended;
    FTrailValue: Extended;
    FTriggerPrice: Extended;
    F_Type: TsgcHTTPFTXTriggerOrderType;
    procedure SetOrderPrice(const Value: Extended);
    procedure SetTrailValue(const Value: Extended);
  protected
    function GetJSON: string; virtual;
  public
    constructor Create; virtual;
  public
    property Market: string read FMarket write FMarket;
    property OrderPrice: Extended read FOrderPrice write SetOrderPrice;
    property ReduceOnly: Boolean read FReduceOnly write FReduceOnly;
    property RetryUntilFilled: Boolean read FRetryUntilFilled
      write FRetryUntilFilled;
    property Side: TsgcHTTPFTXOrderSide read FSide write FSide;
    property Size: Extended read FSize write FSize;
    property TrailValue: Extended read FTrailValue write SetTrailValue;
    property TriggerPrice: Extended read FTriggerPrice write FTriggerPrice;
    property _Type: TsgcHTTPFTXTriggerOrderType read F_Type write F_Type;
  end;

  TsgcHTTPFTXLog_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FFileName: String;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property FileName: String read FFileName write FFileName;
  end;

  TsgcHTTPFTX_Options = class(TPersistent)
  private
    FApiKey: String;
    FApiSecret: string;
    FLogOptions: TsgcHTTPFTXLog_Options;
    FSubAccount: string;
    procedure SetLogOptions(const Value: TsgcHTTPFTXLog_Options);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property ApiKey: String read FApiKey write FApiKey;
    property ApiSecret: string read FApiSecret write FApiSecret;
    property LogOptions: TsgcHTTPFTXLog_Options read FLogOptions
      write SetLogOptions;
    property SubAccount: string read FSubAccount write FSubAccount;
  end;

  TsgcHTTP_API_FTX = class(TsgcHTTPAPI_Client)

    { http requests }
  private
    procedure DoInitializeLog;
  private
    function GetBaseURL: string;
  protected
    function DoHTTP_GET(const aMethod: String; const aParameters: String = '';
      const aHeaders: TStrings = nil): string; virtual;
    function DoHTTP_POST(const aMethod: String; const aBody: String = '';
      const aHeaders: TStrings = nil): string; virtual;
    function DoHTTP_DELETE(const aMethod: String;
      const aParameters: String = ''; const aHeaders: TStrings = nil;
      const aBody: string = ''): string; virtual;
  protected
    procedure GetAuthHeaders(const aMethod, aRequestPath, aBody: String;
      var Headers: TStringList); virtual;
  protected
    function DoHTTP_GET_PRIVATE(const aRequestPath: String): string; virtual;
    function DoHTTP_POST_PRIVATE(const aRequestPath, aBody: String)
      : string; virtual;
    function DoHTTP_DELETE_PRIVATE(const aRequestPath: String;
      aBody: string = ''): string; virtual;
    { http requests }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { properties }
  private
    FFTXOptions: TsgcHTTPFTX_Options;
    procedure SetFTXOptions(const Value: TsgcHTTPFTX_Options);
  public
    property FTXOptions: TsgcHTTPFTX_Options read FFTXOptions
      write SetFTXOptions;
    { properties }
  end;

  TsgcHTTP_API_FTX_Rest = class(TsgcHTTP_API_FTX)
    { markets }
  public
    function GetMarkets: string;
    function GetMarket(const aMarket: string): string;
    function GetOrderbook(const aMarket: string; aDepth: Word = 20): string;
    function GetTrades(const aMarket: string; aLimit: Word = 20;
      aStartTime: Int64 = 0; aEndTime: Int64 = 0): string;
    function GetHistoricalPrices(const aMarket: string;
      aResolution: TsgcHTTPFTXResolution = ftxr300; aLimit: Word = 35;
      aStartTime: Int64 = 0; aEndTime: Int64 = 0): string;
    { markets }

    { futures }
  public
    function GetFutures: string;
    function GetFuture(const aFuture: string): string;
    function GetFutureStats(const aFuture: string): string;
    function GetFundingRates(const aFuture: string = ''; aStartTime: Int64 = 0;
      aEndTime: Int64 = 0): string;
    function GetIndexWeights(const aFuture: string): string;
    function GetExpiredFutures: string;
    function GetHistoricalIndex(const aFuture: string;
      aResolution: TsgcHTTPFTXResolution = ftxr300; aLimit: Word = 35;
      aStartTime: Int64 = 0; aEndTime: Int64 = 0): string;
    { futures }

    { account }
  public
    function GetAccount: string;
    function GetPositions: string;
    function ChangeAccountLeverage(aLeverage: Word): string;
    { account }

    { subaccounts }
  public
    function GetAllSubaccounts: string;
    function CreateSubaccount(const aNickname: string): string;
    function ChangeSubaccountName(const aOldNickname,
      aNewNickname: string): string;
    function DeleteSubaccount(const aNickname: string): string;
    function GetSubaccountBalances(const aNickname: string): string;
    function TransferBetweenSubaccounts(const aCoin: string; aSize: Extended;
      const aSource, aDestination: string): string;
    { subaccounts }

    { wallet }
  private
    function GetJSONValueNull(const aValue: string): string;
    function GetJSONValueBoolean(const aValue: Boolean): string;
  public
    function GetCoins: string;
    function GetBalances: string;
    function GetBalancesAllAccounts: string;
    function GetDepositAddress(const aCoin: string;
      const aMethod: string): string;
    function GetDepositHistory(aStartTime: Int64 = 0;
      aEndTime: Int64 = 0): string;
    function GetWithdrawalHistory(aStartTime: Int64 = 0;
      aEndTime: Int64 = 0): string;
    function RequestWithdrawal(const aCoin: string; aSize: Extended;
      const aAddress: string; const aTag: string = '';
      const aPassword: string = ''; const aCode: string = ''): string;
    function GetAirDrops(aStartTime: Int64 = 0; aEndTime: Int64 = 0): string;
    function GetWithdrawalFees(const aCoin: string; aSize: Extended;
      const aAddress: string; const aTag: string = ''): string;
    function GetSavedAddresses(const aCoin: string = ''): string;
    function CreateSavedAddresses(const aCoin: string; const aAddress: string;
      const aAddressName: string; const aIsPrimetrust: Boolean;
      const aTag: string): string;
    function DeleteSavedAddresses(aSavedAddressId: Integer): string;
    { wallet }

    { orders }
  public
    function GetOpenOrders(const aMarket: string): string;
    function GetOrderHistory(const aMarket: string; aLimit: Word = 100;
      aStartTime: Int64 = 0; aEndTime: Int64 = 0): string;
    function GetOpenTriggerOrders(const aMarket: string;
      aTriggerOrder: TsgcHTTPFTXTriggerOrderType): string;
    function GetTriggerOrderTriggers(aOrderId: Int64): string;
    function GetTriggerOrderHistory(const aMarket: string = '';
      aStartTime: Int64 = 0; aEndTime: Int64 = 0;
      aSide: TsgcHTTPFTXOrderSide = ftosNone;
      aType: TsgcHTTPFTXTriggerOrderType = fttotNone;
      aOrderType: TsgcHTTPFTXOrderType = ftotNone; aLimit: Word = 100): string;
    function PlaceOrder(const aOrder: TsgcHTTPFTXOrder): string;
    function PlaceMarketOrder(const aMarket: string;
      aSide: TsgcHTTPFTXOrderSide; aSize: Extended;
      const aClientId: string = ''): string;
    function PlaceLimitOrder(const aMarket: string; aSide: TsgcHTTPFTXOrderSide;
      aSize, aPrice: Extended; const aClientId: string = ''): string;
    function PlaceTriggerOrder(const aOrder: TsgcHTTPFTXTriggerOrder): string;
    function PlaceTriggerStopOrder(const aMarket: string; aSide:
        TsgcHTTPFTXOrderSide; aSize, aTriggerPrice, aOrderPrice: Extended): string;
    function PlaceTriggerTrailingStopOrder(const aMarket: string; aSide:
        TsgcHTTPFTXOrderSide; aSize, aTrailValue: Extended): string;
    function PlaceTriggerTakeProfitOrder(const aMarket: string; aSide:
        TsgcHTTPFTXOrderSide; aSize, aTriggerPrice, aOrderPrice: Extended): string;
    function ModifyOrder(aOrderId: Int64; aPrice: Extended; aSize: Extended;
      aClientId: string = ''): string;
    function ModifyOrderByClientId(aClientOrderId: string;
      aPrice, aSize: Extended; aClientId: string = ''): string;
    function ModifyTriggerOrder_StopLoss(aOrderId: Int64; aSize: Extended;
      aTriggerPrice: Extended; aOrderPrice: Extended): string;
    function ModifyTriggerOrder_TakeProfit(aOrderId: Int64; aSize: Extended;
      aTriggerPrice: Extended; aOrderPrice: Extended): string;
    function ModifyTriggerOrder_TrailingStop(aOrderId: Int64;
      aSize, aTrailValue: Extended): string;
    function GetOrderStatus(aOrderId: Int64): string;
    function GetOrderStatusByClientId(const aClientOrderId: string): string;
    function CancelOrder(aOrderId: Int64): string;
    function CancelOrderByClientId(const aClientOrderId, aClientOrderId1
      : string): string;
    function CancelOpenTriggerOrder(aOrderId: Int64): string;
    function CancelAllOrders(const aMarket: string = '';
      aConditionalOrdersOnly: Boolean = False;
      aLimitOrdersOnly: Boolean = False): string;
    { orders }

    { fills }
  public
    function GetFills(const aMarket: string; aLimit: Word = 20;
      aStartTime: Int64 = 0; aEndTime: Int64 = 0; aAscending: Boolean = False;
      const aOrderId: string = ''): string;
    { fills }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_HTTP}

uses
  DateUtils,
{$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
{$IFDEF SGC_INDY}sgcIdUri{$ELSE}IdUri{$ENDIF},
  // sgc
  sgcWebSocket_Helpers, sgcBase_Helpers, sgcJSON;

const
  CS_FTX_ENDPOINT_BASE = 'https://ftx.com';
  CS_FTX_DELIMITER = '&';

const
  CS_FTX_ENDPOINT_MARKETS = '/api/markets';
  CS_FTX_ENDPOINT_MARKET = '/api/markets/%s';
  CS_FTX_ENDPOINT_ORDERBOOK = '/api/markets/%s/orderbook';
  CS_FTX_ENDPOINT_TRADES = '/api/markets/%s/orderbook';
  CS_FTX_ENDPOINT_HISTORICAL_PRICES = '/api/markets/%s/candles';

const
  CS_FTX_ENDPOINT_FUTURES = '/api/futures';
  CS_FTX_ENDPOINT_FUTURE = '/api/futures/%s';
  CS_FTX_ENDPOINT_FUTURE_STATS = '/api/futures/%s/stats';
  CS_FTX_ENDPOINT_FUNDING_RATES = '/api/funding_rates';
  CS_FTX_ENDPOINT_INDEX_WEIGHTS = '/api/indexes/%s/weights';
  CS_FTX_ENDPOINT_EXPIRED_FUTURES = '/api/expired_futures';
  CS_FTX_ENDPOINT_HISTORICAL_INDEX = '/api/indexes/%s/candles';

const
  CS_FTX_ENDPOINT_ACCOUNT = '/api/account';
  CS_FTX_ENDPOINT_POSITIONS = '/api/positions';
  CS_FTX_ENDPOINT_ACCOUNT_LEVERAGE = '/api/account/leverage';

const
  CS_FTX_ENDPOINT_SUBACCOUNTS = '/api/subaccounts';
  CS_FTX_ENDPOINT_SUBACCOUNTS_UPDATE_NAME = '/api/subaccounts/update_name';
  CS_FTX_ENDPOINT_SUBACCOUNTS_BALANCES = '/api/subaccounts/%s/balances';
  CS_FTX_ENDPOINT_SUBACCOUNTS_TRANSFER = '/api/subaccounts/transfer';

const
  CS_FTX_ENDPOINT_WALLET_COINS = '/api/wallet/coins';
  CS_FTX_ENDPOINT_WALLET_BALANCES = '/api/wallet/balances';
  CS_FTX_ENDPOINT_WALLET_ALL_BALANCES = '/api/wallet/all_balances';
  CS_FTX_ENDPOINT_WALLET_DEPOSIT_ADDRESS =
    '/wallet/deposit_address/%s?method=%s';
  CS_FTX_ENDPOINT_WALLET_DEPOSITS = '/api/wallet/deposits';
  CS_FTX_ENDPOINT_WALLET_WITHDRAWALS = '/api/wallet/withdrawals';
  CS_FTX_ENDPOINT_WALLET_AIRDROPS = '/api/wallet/airdrops';
  CS_FTX_ENDPOINT_WALLET_WITHDRAWAL_FEE = '/api/wallet/withdrawal_fee';
  CS_FTX_ENDPOINT_WALLET_SAVED_ADDRESSES = '/api/wallet/saved_addresses';

const
  CS_FTX_ENDPOINT_ORDERS = '/api/orders';
  CS_FTX_ENDPOINT_ORDERS_HISTORY = '/api/orders/history';
  CS_FTX_ENDPOINT_CONDITIONAL_ORDERS = '/api/conditional_orders';
  CS_FTX_ENDPOINT_CONDITIONAL_ORDERS_TRIGGERS =
    '/api/conditional_orders/%d/triggers';
  CS_FTX_ENDPOINT_CONDITIONAL_ORDERS_HISTORY =
    '/api/conditional_orders/history';
  CS_FTX_ENDPOINT_ORDER_MODIFY = '/api/orders/%d/modify';
  CS_FTX_ENDPOINT_ORDER_MODIFY_CLIENT_ID = '/api/orders/by_client_id/%s/modify';
  CS_FTX_ENDPOINT_CONDITIONAL_ORDERS_MODIFY =
    '/api/conditional_orders/%d/modify';
  CS_FTX_ENDPOINT_ORDERS_STATUS = '/api/orders/%d';
  CS_FTX_ENDPOINT_ORDERS_STATUS_CLIENT_ID = '/api/orders/by_client_id/%s';
  CS_FTX_ENDPOINT_ORDERS_CANCEL = '/api/orders/%d';
  CS_FTX_ENDPOINT_ORDERS_CANCEL_CLIENT_ID = '/api/orders/by_client_id/%s';
  CS_FTX_ENDPOINT_CONDITIONAL_ORDERS_CANCEL = '/conditional_orders/%d';

const
  CS_FTX_ENDPOINT_FILLS = '/api/fills';

function GetFormatBoolean(aValue: Boolean): string;
begin
  if aValue then
    Result := 'true'
  else
    Result := 'false';
end;

function GetFormatExtended(aValue: Extended): string;
var
  vFS: TFormatSettings;
begin
  vFS.DecimalSeparator := '.';
  vFS.ThousandSeparator := ',';
  Result := FormatFloat('0.0000000000', aValue, vFS);
end;

function GetResolution(aValue: TsgcHTTPFTXResolution): string;
begin
  case aValue of
    ftxr15:
      Result := '15';
    ftxr60:
      Result := '60';
    ftxr300:
      Result := '300';
    ftxr900:
      Result := '900';
    ftxr3600:
      Result := '3600';
    ftxr14400:
      Result := '14400';
    ftxr86400:
      Result := '86400';
  else
    Result := '';
  end;
end;

function GetTriggerOrder(aValue: TsgcHTTPFTXTriggerOrderType): string;
begin
  case aValue of
    fttotStop:
      Result := 'stop';
    fttotTrailing_Stop:
      Result := 'trailing_stop';
    fttotTake_Profit:
      Result := 'take_profit';
  else
    Result := '';
  end;
end;

function GetOrderSide(aValue: TsgcHTTPFTXOrderSide): string;
begin
  case aValue of
    ftosBuy:
      Result := 'buy';
    ftosSell:
      Result := 'sell';
  else
    Result := '';
  end;
end;

function GetOrderType(aValue: TsgcHTTPFTXOrderType): string;
begin
  case aValue of
    ftotMarket:
      Result := 'market';
    ftotLimit:
      Result := 'limit';
  else
    Result := '';
  end;
end;

procedure TsgcHTTPFTXLog_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTPFTXLog_Options then
  begin
    Enabled := TsgcHTTPFTXLog_Options(aSource).Enabled;
    FileName := TsgcHTTPFTXLog_Options(aSource).FileName;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcHTTPFTX_Options.Create;
begin
  inherited;
  FLogOptions := TsgcHTTPFTXLog_Options.Create;
end;

destructor TsgcHTTPFTX_Options.Destroy;
begin
  sgcFree(FLogOptions);
  inherited;
end;

procedure TsgcHTTPFTX_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTPFTX_Options then
  begin
    ApiKey := TsgcHTTPFTX_Options(aSource).ApiKey;
    ApiSecret := TsgcHTTPFTX_Options(aSource).ApiSecret;
    SubAccount := TsgcHTTPFTX_Options(aSource).SubAccount;
    LogOptions := TsgcHTTPFTX_Options(aSource).LogOptions;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcHTTPFTX_Options.SetLogOptions(const Value
  : TsgcHTTPFTXLog_Options);
begin
  if Assigned(FLogOptions) then
    FLogOptions.Assign(Value);
end;

constructor TsgcHTTP_API_FTX.Create(aOwner: TComponent);
begin
  inherited;
  FFTXOptions := TsgcHTTPFTX_Options.Create;
end;

destructor TsgcHTTP_API_FTX.Destroy;
begin
  sgcFree(FFTXOptions);
  inherited;
end;

function TsgcHTTP_API_FTX.DoHTTP_GET(const aMethod: String;
  const aParameters: String = ''; const aHeaders: TStrings = nil): string;
var
  vURL: String;
begin
  DoInitializeLog;

  vURL := GetBaseURL + aMethod;
  if aParameters <> '' then
    vURL := vURL + '?' + aParameters;
  Result := Get(vURL, aHeaders);
end;

function TsgcHTTP_API_FTX.DoHTTP_POST(const aMethod: String;
  const aBody: String = ''; const aHeaders: TStrings = nil): string;
begin
  DoInitializeLog;

  Result := Post(GetBaseURL + aMethod, aBody, aHeaders);
end;

function TsgcHTTP_API_FTX.DoHTTP_GET_PRIVATE(const aRequestPath
  : String): string;
var
  oHeaders: TStringList;
begin
  oHeaders := TStringList.Create;
  Try
    oHeaders.Add('content-type: application/json');
    GetAuthHeaders('GET', aRequestPath, '', oHeaders);
    Result := DoHTTP_GET(aRequestPath, '', oHeaders);
  Finally
    sgcFree(oHeaders);
  End;
end;

procedure TsgcHTTP_API_FTX.DoInitializeLog;
begin
  if Log <> FTXOptions.LogOptions.Enabled then
  begin
    LogFileName := FTXOptions.LogOptions.FileName;
    Log := FTXOptions.LogOptions.Enabled;
  end;
end;

procedure TsgcHTTP_API_FTX.GetAuthHeaders(const aMethod, aRequestPath,
  aBody: String; var Headers: TStringList);
var
  vTimestamp: string;
  oSecret: TsgcStringStream;
  vSignature: string;
begin
  // get signature
  vTimestamp := GetDateTimeUnix(Now, False) + '000';
  oSecret := TsgcStringStream.Create(FTXOptions.ApiSecret);
  Try
    vSignature := GetHMACSHA256(vTimestamp + aMethod + aRequestPath + aBody,
      TIdBytes(oSecret.Bytes), False);
  Finally
    sgcFree(oSecret);
  end;

  // set signature headers
  Headers.Add('FTX-KEY: ' + FTXOptions.ApiKey);
  Headers.Add('FTX-TS: ' + vTimestamp);
  Headers.Add('FTX-SIGN: ' + vSignature);
  if FTXOptions.SubAccount <> '' then
    Headers.Add('FTX-SUBACCOUNT: ' + TIdURI.ParamsEncode(FTXOptions.SubAccount))
end;

function TsgcHTTP_API_FTX.GetBaseURL: string;
begin
  Result := CS_FTX_ENDPOINT_BASE;
end;

function TsgcHTTP_API_FTX.DoHTTP_DELETE(const aMethod: String;
  const aParameters: String = ''; const aHeaders: TStrings = nil;
  const aBody: string = ''): string;
var
  vURL: String;
begin
  DoInitializeLog;

  vURL := GetBaseURL + aMethod;
  if aParameters <> '' then
    vURL := vURL + '?' + aParameters;
  Result := Delete(vURL, aHeaders, aBody);
end;

function TsgcHTTP_API_FTX.DoHTTP_DELETE_PRIVATE(const aRequestPath: String;
  aBody: string = ''): string;
var
  oHeaders: TStringList;
begin
  oHeaders := TStringList.Create;
  Try
    oHeaders.Add('content-type: application/json');
    GetAuthHeaders('DELETE', aRequestPath, aBody, oHeaders);
    Result := DoHTTP_DELETE(aRequestPath, '', oHeaders, aBody);
  Finally
    sgcFree(oHeaders);
  End;
end;

function TsgcHTTP_API_FTX.DoHTTP_POST_PRIVATE(const aRequestPath,
  aBody: String): string;
var
  oHeaders: TStringList;
begin
  oHeaders := TStringList.Create;
  Try
    oHeaders.Add('content-type: application/json');
    GetAuthHeaders('POST', aRequestPath, aBody, oHeaders);
    Result := DoHTTP_POST(aRequestPath, aBody, oHeaders);
  Finally
    sgcFree(oHeaders);
  End;
end;

procedure TsgcHTTP_API_FTX.SetFTXOptions(const Value: TsgcHTTPFTX_Options);
begin
  if Assigned(FFTXOptions) then
    FFTXOptions.Assign(Value);
end;

constructor TsgcHTTPFTXOrder.Create;
begin
  inherited;
  FMarket := '';
  FSide := ftosNone;
  FPrice := 0;
  F_Type := ftotNone;
  FSize := 0;
  FReduceOnly := False;
  FIOC := False;
  FPostOnly := False;
  FClientId := '';
end;

function TsgcHTTPFTXOrder.GetJSON: string;
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    // ... common fields mandatories
    oJSON.AddPair('market', Market);
    oJSON.AddPair('side', GetOrderSide(Side));
    oJSON.AddPair('type', GetOrderType(_Type));
    oJSON.AddPair('size', GetFormatExtended(Size));

    // ... limit orders
    if _Type = ftotLimit then
      oJSON.AddPair('price', GetFormatExtended(Price))
    else if _Type = ftotMarket then
      oJSON.AddPair('price');

    // ... optionals
    if ReduceOnly then
      oJSON.AddPair('reduceOnly', GetFormatBoolean(ReduceOnly));
    if IOC then
      oJSON.AddPair('ioc', GetFormatBoolean(IOC));
    if PostOnly then
      oJSON.AddPair('postOnly', GetFormatBoolean(PostOnly));
    if ClientId <> '' then
      oJSON.AddPair('clientId', ClientId)
    else
      oJSON.AddPair('clientId');

    // ... result
    Result := oJSON.Text;
  Finally
    sgcFree(oJSON);
  End;
end;

function TsgcHTTP_API_FTX_Rest.CancelAllOrders(const aMarket: string = '';
  aConditionalOrdersOnly: Boolean = False;
  aLimitOrdersOnly: Boolean = False): string;
var
  oJSON: TsgcJSON;
  vBody: string;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    if aMarket <> '' then
      oJSON.AddPair('market', aMarket);
    if aConditionalOrdersOnly then
      oJSON.AddPair('conditionalOrdersOnly',
        GetFormatBoolean(aConditionalOrdersOnly));
    if aLimitOrdersOnly then
      oJSON.AddPair('limitOrdersOnly', GetFormatBoolean(aLimitOrdersOnly));

    vBody := '';
    if oJSON.Count > 0 then
      vBody := oJSON.Text;

    Result := DoHTTP_DELETE_PRIVATE(CS_FTX_ENDPOINT_ORDERS, vBody);
  Finally
    sgcFree(oJSON);
  End;
end;

function TsgcHTTP_API_FTX_Rest.CancelOrder(aOrderId: Int64): string;
begin
  Result := DoHTTP_DELETE_PRIVATE(Format(CS_FTX_ENDPOINT_ORDERS_CANCEL,
    [aOrderId]));
end;

function TsgcHTTP_API_FTX_Rest.CancelOrderByClientId(const aClientOrderId,
  aClientOrderId1: string): string;
begin
  Result := DoHTTP_DELETE_PRIVATE
    (Format(CS_FTX_ENDPOINT_ORDERS_CANCEL_CLIENT_ID, [aClientOrderId]));
end;

function TsgcHTTP_API_FTX_Rest.CancelOpenTriggerOrder(aOrderId: Int64): string;
begin
  Result := DoHTTP_DELETE_PRIVATE
    (Format(CS_FTX_ENDPOINT_CONDITIONAL_ORDERS_CANCEL, [aOrderId]));
end;

function TsgcHTTP_API_FTX_Rest.ChangeAccountLeverage(aLeverage: Word): string;
begin
  Result := DoHTTP_POST_PRIVATE(CS_FTX_ENDPOINT_ACCOUNT_LEVERAGE,
    Format('{"leverage": %d}', [aLeverage]));
end;

function TsgcHTTP_API_FTX_Rest.ChangeSubaccountName(const aOldNickname,
  aNewNickname: string): string;
begin
  Result := DoHTTP_POST_PRIVATE(CS_FTX_ENDPOINT_SUBACCOUNTS_UPDATE_NAME,
    Format('{"nickname": "%s", "newNickname": "%s"}',
    [aOldNickname, aNewNickname]));
end;

function TsgcHTTP_API_FTX_Rest.CreateSavedAddresses(const aCoin: string;
  const aAddress: string; const aAddressName: string;
  const aIsPrimetrust: Boolean; const aTag: string): string;
begin
  Result := DoHTTP_POST_PRIVATE(CS_FTX_ENDPOINT_WALLET_SAVED_ADDRESSES,
    Format('{"coin": "%s", "address": "%s", "addressName": "%s", "isPrimetrust": %s, "tag": %s}',
    [aCoin, aAddress, aAddressName, GetJSONValueBoolean(aIsPrimetrust),
    GetJSONValueNull(aTag)]));
end;

function TsgcHTTP_API_FTX_Rest.CreateSubaccount(const aNickname
  : string): string;
begin
  Result := DoHTTP_POST_PRIVATE(CS_FTX_ENDPOINT_SUBACCOUNTS,
    Format('{"nickname": "%s"}', [aNickname]));
end;

function TsgcHTTP_API_FTX_Rest.DeleteSavedAddresses(aSavedAddressId
  : Integer): string;
begin
  Result := DoHTTP_DELETE_PRIVATE(Format(CS_FTX_ENDPOINT_WALLET_SAVED_ADDRESSES
    + '/%d', [aSavedAddressId]));
end;

function TsgcHTTP_API_FTX_Rest.DeleteSubaccount(const aNickname
  : string): string;
begin
  Result := DoHTTP_DELETE_PRIVATE(CS_FTX_ENDPOINT_SUBACCOUNTS,
    Format('{"nickname": "%s"}', [aNickname]));
end;

function TsgcHTTP_API_FTX_Rest.GetAccount: string;
begin
  Result := DoHTTP_GET_PRIVATE(CS_FTX_ENDPOINT_ACCOUNT);
end;

function TsgcHTTP_API_FTX_Rest.GetAirDrops(aStartTime: Int64 = 0;
  aEndTime: Int64 = 0): string;
var
  oParameters: TStringList;
  vRequestPath: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_FTX_DELIMITER;
    if aStartTime > 0 then
      oParameters.Add('start_time=' + IntToStr(aStartTime));
    if aEndTime > 0 then
      oParameters.Add('end_time=' + IntToStr(aEndTime));
    vRequestPath := CS_FTX_ENDPOINT_WALLET_AIRDROPS;
    if oParameters.Count > 0 then
      vRequestPath := vRequestPath + '?' + oParameters.DelimitedText;
    Result := DoHTTP_GET_PRIVATE(vRequestPath);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_FTX_Rest.GetAllSubaccounts: string;
begin
  Result := DoHTTP_GET_PRIVATE(CS_FTX_ENDPOINT_SUBACCOUNTS);
end;

function TsgcHTTP_API_FTX_Rest.GetBalances: string;
begin
  Result := DoHTTP_GET_PRIVATE(CS_FTX_ENDPOINT_WALLET_BALANCES);
end;

function TsgcHTTP_API_FTX_Rest.GetBalancesAllAccounts: string;
begin
  Result := DoHTTP_GET_PRIVATE(CS_FTX_ENDPOINT_WALLET_ALL_BALANCES);
end;

function TsgcHTTP_API_FTX_Rest.GetCoins: string;
begin
  Result := DoHTTP_GET(CS_FTX_ENDPOINT_WALLET_COINS);
end;

function TsgcHTTP_API_FTX_Rest.GetDepositAddress(const aCoin: string;
  const aMethod: string): string;
begin
  Result := DoHTTP_GET_PRIVATE(Format(CS_FTX_ENDPOINT_WALLET_DEPOSIT_ADDRESS,
    [aCoin, aMethod]));
end;

function TsgcHTTP_API_FTX_Rest.GetDepositHistory(aStartTime: Int64 = 0;
  aEndTime: Int64 = 0): string;
var
  oParameters: TStringList;
  vRequestPath: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_FTX_DELIMITER;
    if aStartTime > 0 then
      oParameters.Add('start_time=' + IntToStr(aStartTime));
    if aEndTime > 0 then
      oParameters.Add('end_time=' + IntToStr(aEndTime));
    vRequestPath := CS_FTX_ENDPOINT_WALLET_DEPOSITS;
    if oParameters.Count > 0 then
      vRequestPath := vRequestPath + '?' + oParameters.DelimitedText;
    Result := DoHTTP_GET_PRIVATE(vRequestPath);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_FTX_Rest.GetExpiredFutures: string;
begin
  Result := DoHTTP_GET(CS_FTX_ENDPOINT_EXPIRED_FUTURES);
end;

function TsgcHTTP_API_FTX_Rest.GetFills(const aMarket: string;
  aLimit: Word = 20; aStartTime: Int64 = 0; aEndTime: Int64 = 0;
  aAscending: Boolean = False; const aOrderId: string = ''): string;
var
  oParameters: TStringList;
  vRequestPath: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_FTX_DELIMITER;
    if aMarket <> '' then
      oParameters.Add('market=' + aMarket);
    if aLimit <> 20 then
      oParameters.Add('limit=' + IntToStr(aLimit));
    if aStartTime > 0 then
      oParameters.Add('start_time=' + IntToStr(aStartTime));
    if aEndTime > 0 then
      oParameters.Add('end_time=' + IntToStr(aEndTime));
    if aAscending then
      oParameters.Add('order=asc');
    if aOrderId <> '' then
      oParameters.Add('orderId=' + aOrderId);

    vRequestPath := CS_FTX_ENDPOINT_FILLS;
    if oParameters.Count > 0 then
      vRequestPath := vRequestPath + '?' + oParameters.DelimitedText;
    Result := DoHTTP_GET_PRIVATE(vRequestPath);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_FTX_Rest.GetFundingRates(const aFuture: string = '';
  aStartTime: Int64 = 0; aEndTime: Int64 = 0): string;
var
  oParameters: TStringList;
  vRequestPath: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_FTX_DELIMITER;
    if aFuture <> '' then
      oParameters.Add('future=' + aFuture);
    if aStartTime > 0 then
      oParameters.Add('start_time=' + IntToStr(aStartTime));
    if aEndTime > 0 then
      oParameters.Add('end_time=' + IntToStr(aEndTime));
    vRequestPath := CS_FTX_ENDPOINT_FUNDING_RATES;
    if oParameters.Count > 0 then
      vRequestPath := vRequestPath + '?' + oParameters.DelimitedText;
    Result := DoHTTP_GET(vRequestPath);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_FTX_Rest.GetFuture(const aFuture: string): string;
begin
  Result := DoHTTP_GET(Format(CS_FTX_ENDPOINT_FUTURE, [aFuture]));
end;

function TsgcHTTP_API_FTX_Rest.GetFutureStats(const aFuture: string): string;
begin
  Result := DoHTTP_GET(Format(CS_FTX_ENDPOINT_FUTURE_STATS, [aFuture]));
end;

function TsgcHTTP_API_FTX_Rest.GetHistoricalIndex(const aFuture: string;
  aResolution: TsgcHTTPFTXResolution = ftxr300; aLimit: Word = 35;
  aStartTime: Int64 = 0; aEndTime: Int64 = 0): string;
var
  oParameters: TStringList;
  vRequestPath: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_FTX_DELIMITER;
    oParameters.Add('limit=' + IntToStr(aLimit));
    oParameters.Add('resolution=' + GetResolution(aResolution));
    if aStartTime > 0 then
      oParameters.Add('start_time=' + IntToStr(aStartTime));
    if aEndTime > 0 then
      oParameters.Add('end_time=' + IntToStr(aEndTime));
    vRequestPath := CS_FTX_ENDPOINT_HISTORICAL_INDEX;
    if oParameters.Count > 0 then
      vRequestPath := vRequestPath + '?' + oParameters.DelimitedText;
    Result := DoHTTP_GET(Format(vRequestPath, [aFuture]));
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_FTX_Rest.GetHistoricalPrices(const aMarket: string;
  aResolution: TsgcHTTPFTXResolution = ftxr300; aLimit: Word = 35;
  aStartTime: Int64 = 0; aEndTime: Int64 = 0): string;
var
  oParameters: TStringList;
  vRequestPath: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_FTX_DELIMITER;
    oParameters.Add('limit=' + IntToStr(aLimit));
    oParameters.Add('resolution=' + GetResolution(aResolution));
    if aStartTime > 0 then
      oParameters.Add('start_time=' + IntToStr(aStartTime));
    if aEndTime > 0 then
      oParameters.Add('end_time=' + IntToStr(aEndTime));
    vRequestPath := CS_FTX_ENDPOINT_HISTORICAL_PRICES;
    if oParameters.Count > 0 then
      vRequestPath := vRequestPath + '?' + oParameters.DelimitedText;
    Result := DoHTTP_GET(Format(vRequestPath, [aMarket]));
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_FTX_Rest.GetIndexWeights(const aFuture: string): string;
begin
  Result := DoHTTP_GET(Format(CS_FTX_ENDPOINT_INDEX_WEIGHTS, [aFuture]));
end;

function TsgcHTTP_API_FTX_Rest.GetMarket(const aMarket: string): string;
begin
  Result := DoHTTP_GET(Format(CS_FTX_ENDPOINT_MARKET, [aMarket]));
end;

function TsgcHTTP_API_FTX_Rest.GetMarkets: string;
begin
  Result := DoHTTP_GET(CS_FTX_ENDPOINT_MARKETS);
end;

function TsgcHTTP_API_FTX_Rest.GetOrderbook(const aMarket: string;
  aDepth: Word = 20): string;
begin
  Result := DoHTTP_GET(Format(CS_FTX_ENDPOINT_ORDERBOOK + '?depth=%d',
    [aMarket, aDepth]));
end;

function TsgcHTTP_API_FTX_Rest.GetPositions: string;
begin
  Result := DoHTTP_GET_PRIVATE(CS_FTX_ENDPOINT_POSITIONS);
end;

function TsgcHTTP_API_FTX_Rest.GetTrades(const aMarket: string;
  aLimit: Word = 20; aStartTime: Int64 = 0; aEndTime: Int64 = 0): string;
var
  oParameters: TStringList;
  vRequestPath: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_FTX_DELIMITER;
    oParameters.Add('limit=' + IntToStr(aLimit));
    if aStartTime > 0 then
      oParameters.Add('start_time=' + IntToStr(aStartTime));
    if aEndTime > 0 then
      oParameters.Add('end_time=' + IntToStr(aEndTime));
    vRequestPath := CS_FTX_ENDPOINT_TRADES;
    if oParameters.Count > 0 then
      vRequestPath := vRequestPath + '?' + oParameters.DelimitedText;
    Result := DoHTTP_GET(Format(vRequestPath, [aMarket]));
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_FTX_Rest.GetFutures: string;
begin
  Result := DoHTTP_GET(CS_FTX_ENDPOINT_FUTURES);
end;

function TsgcHTTP_API_FTX_Rest.GetJSONValueBoolean(const aValue
  : Boolean): string;
begin
  if aValue then
    Result := 'true'
  else
    Result := 'false';
end;

function TsgcHTTP_API_FTX_Rest.GetJSONValueNull(const aValue: string): string;
begin
  if aValue <> '' then
    Result := '"' + aValue + '"'
  else
    Result := 'null';
end;

function TsgcHTTP_API_FTX_Rest.GetOpenOrders(const aMarket: string): string;
begin
  Result := DoHTTP_GET_PRIVATE(CS_FTX_ENDPOINT_ORDERS + '?market=' + aMarket);
end;

function TsgcHTTP_API_FTX_Rest.GetOpenTriggerOrders(const aMarket: string;
  aTriggerOrder: TsgcHTTPFTXTriggerOrderType): string;
var
  oParameters: TStringList;
  vRequestPath: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_FTX_DELIMITER;
    oParameters.Add('market=' + aMarket);
    if aTriggerOrder <> fttotNone then
      oParameters.Add('type=' + GetTriggerOrder(aTriggerOrder));
    vRequestPath := CS_FTX_ENDPOINT_CONDITIONAL_ORDERS;
    if oParameters.Count > 0 then
      vRequestPath := vRequestPath + '?' + oParameters.DelimitedText;
    Result := DoHTTP_GET_PRIVATE(vRequestPath);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_FTX_Rest.GetOrderHistory(const aMarket: string;
  aLimit: Word = 100; aStartTime: Int64 = 0; aEndTime: Int64 = 0): string;
var
  oParameters: TStringList;
  vRequestPath: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_FTX_DELIMITER;
    oParameters.Add('market=' + aMarket);
    if aLimit <> 100 then
      oParameters.Add('limit=' + IntToStr(aLimit));
    if aStartTime > 0 then
      oParameters.Add('start_time=' + IntToStr(aStartTime));
    if aEndTime > 0 then
      oParameters.Add('end_time=' + IntToStr(aEndTime));
    vRequestPath := CS_FTX_ENDPOINT_ORDERS_HISTORY;
    if oParameters.Count > 0 then
      vRequestPath := vRequestPath + '?' + oParameters.DelimitedText;
    Result := DoHTTP_GET_PRIVATE(vRequestPath);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_FTX_Rest.GetOrderStatus(aOrderId: Int64): string;
begin
  Result := DoHTTP_GET_PRIVATE(Format(CS_FTX_ENDPOINT_ORDERS_STATUS,
    [aOrderId]));
end;

function TsgcHTTP_API_FTX_Rest.GetOrderStatusByClientId(const aClientOrderId
  : string): string;
begin
  Result := DoHTTP_GET_PRIVATE(Format(CS_FTX_ENDPOINT_ORDERS_STATUS_CLIENT_ID,
    [aClientOrderId]));
end;

function TsgcHTTP_API_FTX_Rest.GetSavedAddresses(const aCoin
  : string = ''): string;
var
  oParameters: TStringList;
  vRequestPath: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_FTX_DELIMITER;
    if aCoin <> '' then
      oParameters.Add('coin=' + aCoin);
    vRequestPath := CS_FTX_ENDPOINT_WALLET_SAVED_ADDRESSES;
    if oParameters.Count > 0 then
      vRequestPath := vRequestPath + '?' + oParameters.DelimitedText;
    Result := DoHTTP_GET_PRIVATE(vRequestPath);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_FTX_Rest.GetSubaccountBalances(const aNickname
  : string): string;
begin
  Result := DoHTTP_GET_PRIVATE(Format(CS_FTX_ENDPOINT_SUBACCOUNTS_BALANCES,
    [aNickname]));
end;

function TsgcHTTP_API_FTX_Rest.GetTriggerOrderHistory(const aMarket
  : string = ''; aStartTime: Int64 = 0; aEndTime: Int64 = 0;
  aSide: TsgcHTTPFTXOrderSide = ftosNone;
  aType: TsgcHTTPFTXTriggerOrderType = fttotNone;
  aOrderType: TsgcHTTPFTXOrderType = ftotNone; aLimit: Word = 100): string;
var
  oParameters: TStringList;
  vRequestPath: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_FTX_DELIMITER;
    if aMarket <> '' then
      oParameters.Add('market=' + aMarket);
    if aStartTime > 0 then
      oParameters.Add('start_time=' + IntToStr(aStartTime));
    if aEndTime > 0 then
      oParameters.Add('end_time=' + IntToStr(aEndTime));
    if aSide <> ftosNone then
      oParameters.Add('side=' + GetOrderSide(aSide));
    if aType <> fttotNone then
      oParameters.Add('type=' + GetTriggerOrder(aType));
    if aOrderType <> ftotNone then
      oParameters.Add('orderType=' + GetOrderType(aOrderType));
    if aLimit <> 100 then
      oParameters.Add('limit=' + IntToStr(aLimit));

    vRequestPath := CS_FTX_ENDPOINT_CONDITIONAL_ORDERS_HISTORY;
    if oParameters.Count > 0 then
      vRequestPath := vRequestPath + '?' + oParameters.DelimitedText;
    Result := DoHTTP_GET_PRIVATE(vRequestPath);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_FTX_Rest.GetTriggerOrderTriggers(aOrderId: Int64): string;
begin
  Result := DoHTTP_GET_PRIVATE
    (Format(CS_FTX_ENDPOINT_CONDITIONAL_ORDERS_TRIGGERS, [aOrderId]));
end;

function TsgcHTTP_API_FTX_Rest.GetWithdrawalFees(const aCoin: string;
  aSize: Extended; const aAddress: string; const aTag: string = ''): string;
begin
  Result := DoHTTP_POST_PRIVATE(CS_FTX_ENDPOINT_WALLET_WITHDRAWAL_FEE,
    Format('{"coin": "%s", "size": %s, "address": "%s", "tag": %s}',
    [aCoin, GetFormatExtended(aSize), aAddress, GetJSONValueNull(aTag)]));
end;

function TsgcHTTP_API_FTX_Rest.GetWithdrawalHistory(aStartTime: Int64 = 0;
  aEndTime: Int64 = 0): string;
var
  oParameters: TStringList;
  vRequestPath: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_FTX_DELIMITER;
    if aStartTime > 0 then
      oParameters.Add('start_time=' + IntToStr(aStartTime));
    if aEndTime > 0 then
      oParameters.Add('end_time=' + IntToStr(aEndTime));
    vRequestPath := CS_FTX_ENDPOINT_WALLET_WITHDRAWALS;
    if oParameters.Count > 0 then
      vRequestPath := vRequestPath + '?' + oParameters.DelimitedText;
    Result := DoHTTP_GET_PRIVATE(vRequestPath);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_FTX_Rest.ModifyOrder(aOrderId: Int64; aPrice: Extended;
  aSize: Extended; aClientId: string = ''): string;
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    if aPrice > 0 then
      oJSON.AddPair('price', GetFormatExtended(aPrice));
    if aSize > 0 then
      oJSON.AddPair('size', GetFormatExtended(aSize));
    if aClientId <> '' then
      oJSON.AddPair('clientId', aClientId);

    Result := DoHTTP_POST_PRIVATE(Format(CS_FTX_ENDPOINT_ORDER_MODIFY,
      [aOrderId]), oJSON.Text);
  Finally
    sgcFree(oJSON);
  End;
end;

function TsgcHTTP_API_FTX_Rest.ModifyOrderByClientId(aClientOrderId: string;
  aPrice, aSize: Extended; aClientId: string = ''): string;
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    if aPrice > 0 then
      oJSON.AddPair('price', GetFormatExtended(aPrice));
    if aSize > 0 then
      oJSON.AddPair('size', GetFormatExtended(aSize));
    if aClientId <> '' then
      oJSON.AddPair('clientId', aClientId);

    Result := DoHTTP_POST_PRIVATE(Format(CS_FTX_ENDPOINT_ORDER_MODIFY_CLIENT_ID,
      [aClientOrderId]), oJSON.Text);
  Finally
    sgcFree(oJSON);
  End;
end;

function TsgcHTTP_API_FTX_Rest.ModifyTriggerOrder_StopLoss(aOrderId: Int64;
  aSize: Extended; aTriggerPrice: Extended; aOrderPrice: Extended): string;
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.AddPair('size', GetFormatExtended(aSize));
    oJSON.AddPair('triggerPrice', GetFormatExtended(aTriggerPrice));
    oJSON.AddPair('orderPrice', GetFormatExtended(aOrderPrice));

    Result := DoHTTP_POST_PRIVATE
      (Format(CS_FTX_ENDPOINT_CONDITIONAL_ORDERS_MODIFY, [aOrderId]),
      oJSON.Text);
  Finally
    sgcFree(oJSON);
  End;
end;

function TsgcHTTP_API_FTX_Rest.ModifyTriggerOrder_TakeProfit(aOrderId: Int64;
  aSize: Extended; aTriggerPrice: Extended; aOrderPrice: Extended): string;
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.AddPair('size', GetFormatExtended(aSize));
    oJSON.AddPair('triggerPrice', GetFormatExtended(aTriggerPrice));
    oJSON.AddPair('orderPrice', GetFormatExtended(aOrderPrice));

    Result := DoHTTP_POST_PRIVATE
      (Format(CS_FTX_ENDPOINT_CONDITIONAL_ORDERS_MODIFY, [aOrderId]),
      oJSON.Text);
  Finally
    sgcFree(oJSON);
  End;
end;

function TsgcHTTP_API_FTX_Rest.ModifyTriggerOrder_TrailingStop(aOrderId: Int64;
  aSize, aTrailValue: Extended): string;
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.AddPair('size', GetFormatExtended(aSize));
    oJSON.AddPair('trailValue', GetFormatExtended(aTrailValue));

    Result := DoHTTP_POST_PRIVATE
      (Format(CS_FTX_ENDPOINT_CONDITIONAL_ORDERS_MODIFY, [aOrderId]),
      oJSON.Text);
  Finally
    sgcFree(oJSON);
  End;
end;

function TsgcHTTP_API_FTX_Rest.PlaceLimitOrder(const aMarket: string;
  aSide: TsgcHTTPFTXOrderSide; aSize, aPrice: Extended;
  const aClientId: string = ''): string;
var
  oOrder: TsgcHTTPFTXOrder;
begin
  oOrder := TsgcHTTPFTXOrder.Create;
  Try
    oOrder._Type := ftotLimit;
    oOrder.Market := aMarket;
    oOrder.Side := aSide;
    oOrder.Size := aSize;
    oOrder.Price := aPrice;
    oOrder.ClientId := aClientId;

    Result := PlaceOrder(oOrder);
  Finally
    sgcFree(oOrder);
  End;
end;

function TsgcHTTP_API_FTX_Rest.PlaceMarketOrder(const aMarket: string;
  aSide: TsgcHTTPFTXOrderSide; aSize: Extended;
  const aClientId: string = ''): string;
var
  oOrder: TsgcHTTPFTXOrder;
begin
  oOrder := TsgcHTTPFTXOrder.Create;
  Try
    oOrder._Type := ftotMarket;
    oOrder.Market := aMarket;
    oOrder.Side := aSide;
    oOrder.Size := aSize;
    oOrder.ClientId := aClientId;

    Result := PlaceOrder(oOrder);
  Finally
    sgcFree(oOrder);
  End;
end;

function TsgcHTTP_API_FTX_Rest.PlaceOrder(const aOrder
  : TsgcHTTPFTXOrder): string;
begin
  Result := DoHTTP_POST_PRIVATE(CS_FTX_ENDPOINT_ORDERS, aOrder.GetJSON);
end;

function TsgcHTTP_API_FTX_Rest.PlaceTriggerOrder(const aOrder
  : TsgcHTTPFTXTriggerOrder): string;
begin
  Result := DoHTTP_POST_PRIVATE(CS_FTX_ENDPOINT_CONDITIONAL_ORDERS,
    aOrder.GetJSON);
end;

function TsgcHTTP_API_FTX_Rest.PlaceTriggerStopOrder(const aMarket: string;
    aSide: TsgcHTTPFTXOrderSide; aSize, aTriggerPrice, aOrderPrice: Extended):
    string;
var
  oOrder: TsgcHTTPFTXTriggerOrder;
begin
  oOrder := TsgcHTTPFTXTriggerOrder.Create;
  Try
    oOrder._Type := fttotStop;
    oOrder.Market := aMarket;
    oOrder.Side := aSide;
    oOrder.Size := aSize;
    oOrder.TriggerPrice := aTriggerPrice;
    oOrder.OrderPrice := aOrderPrice;

    Result := PlaceTriggerOrder(oOrder);
  Finally
    sgcFree(oOrder);
  End;
end;

function TsgcHTTP_API_FTX_Rest.PlaceTriggerTakeProfitOrder(const aMarket:
    string; aSide: TsgcHTTPFTXOrderSide; aSize, aTriggerPrice, aOrderPrice:
    Extended): string;
var
  oOrder: TsgcHTTPFTXTriggerOrder;
begin
  oOrder := TsgcHTTPFTXTriggerOrder.Create;
  Try
    oOrder._Type := fttotTake_Profit;
    oOrder.Market := aMarket;
    oOrder.Side := aSide;
    oOrder.Size := aSize;
    oOrder.TriggerPrice := aTriggerPrice;
    oOrder.OrderPrice := aOrderPrice;

    Result := PlaceTriggerOrder(oOrder);
  Finally
    sgcFree(oOrder);
  End;
end;

function TsgcHTTP_API_FTX_Rest.PlaceTriggerTrailingStopOrder(const aMarket:
    string; aSide: TsgcHTTPFTXOrderSide; aSize, aTrailValue: Extended): string;
var
  oOrder: TsgcHTTPFTXTriggerOrder;
begin
  oOrder := TsgcHTTPFTXTriggerOrder.Create;
  Try
    oOrder._Type := fttotTrailing_Stop;
    oOrder.Market := aMarket;
    oOrder.Side := aSide;
    oOrder.Size := aSize;
    oOrder.TrailValue := aTrailValue;

    Result := PlaceTriggerOrder(oOrder);
  Finally
    sgcFree(oOrder);
  End;
end;

function TsgcHTTP_API_FTX_Rest.RequestWithdrawal(const aCoin: string;
  aSize: Extended; const aAddress: string; const aTag: string = '';
  const aPassword: string = ''; const aCode: string = ''): string;
begin
  Result := DoHTTP_POST_PRIVATE(CS_FTX_ENDPOINT_WALLET_WITHDRAWALS,
    Format('{"coin": "%s", "size": %s, "address": "%s", "tag": %s, "password": %s, "code": %s}',
    [aCoin, GetFormatExtended(aSize), aAddress, GetJSONValueNull(aTag),
    GetJSONValueNull(aPassword), GetJSONValueNull(aCode)]));
end;

function TsgcHTTP_API_FTX_Rest.TransferBetweenSubaccounts(const aCoin: string;
  aSize: Extended; const aSource, aDestination: string): string;
begin
  Result := DoHTTP_POST_PRIVATE(CS_FTX_ENDPOINT_SUBACCOUNTS_UPDATE_NAME,
    Format('{"coin": "%s", "size": %s, "source": "%s", "destination": "%s"}',
    [aCoin, GetFormatExtended(aSize), aSource, aDestination]));
end;

constructor TsgcHTTPFTXTriggerOrder.Create;
begin
  inherited;
  FMarket := '';
  FSide := ftosNone;
  F_Type := fttotStop;
  FSize := 0;
  FReduceOnly := False;
  FRetryUntilFilled := False;
  TriggerPrice := 0;
  OrderPrice := 0;
  TrailValue := 0;
end;

function TsgcHTTPFTXTriggerOrder.GetJSON: string;
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    // ... common fields mandatories
    oJSON.AddPair('market', Market);
    oJSON.AddPair('side', GetOrderSide(Side));
    oJSON.AddPair('type', GetTriggerOrder(_Type));
    oJSON.AddPair('size', GetFormatExtended(Size));

    // ... optionals
    if ReduceOnly then
      oJSON.AddPair('reduceOnly', 'true');
    if RetryUntilFilled then
      oJSON.AddPair('retryUntilFilled', 'true');

    // ... additional fields
    if (_Type = fttotStop) or (_Type = fttotTake_Profit) then
    begin
      oJSON.AddPair('triggerPrice', GetFormatExtended(TriggerPrice));
      // ... if value is zero, is a limit order. If it's zero, is a market order
      if OrderPrice > 0 then
        oJSON.AddPair('orderPrice', GetFormatExtended(OrderPrice));
    end
    else if _Type = fttotTrailing_Stop then
      oJSON.AddPair('trailValue', GetFormatExtended(TrailValue));

    // ... result
    Result := oJSON.Text;
  Finally
    sgcFree(oJSON);
  End;
end;

procedure TsgcHTTPFTXTriggerOrder.SetOrderPrice(const Value: Extended);
begin
  FOrderPrice := Value;
end;

procedure TsgcHTTPFTXTriggerOrder.SetTrailValue(const Value: Extended);
begin
  FTrailValue := Value;
end;

{$ENDIF}

end.
