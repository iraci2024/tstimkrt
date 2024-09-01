{ ***************************************************************************
  sgcHTTP API component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcHTTP_API_Coinbase;

interface

{$I sgcVer.inc}
{$IFDEF SGC_HTTP}

uses
  Classes, SysUtils,
  // sgc
  sgcHTTP_API;

type

  TsgcHTTPCoinbaseOrderType = (coioLimit, coioMarket);
  TsgcHTTPCoinbaseOrderSide = (coisBuy, coisSell);
  TsgcHTTPCoinbaseOrderStop = (costNone, costLoss, costEntry);
  TsgcHTTPCoinbaseTimeInForce = (cbtfNone, cbtfGTC, cbtfGTT, cbtfIOC, cbtfFOK);
  TsgcHTTPCoinbaseSelfTradePrevention = (stpNone, stpDC, stpCO, stpCN, stpCB);
  TsgcHTTPCoinbaseSizeType = (csztNone, csztUseSize, csztUseFunds);
  TsgcHTTPCoinbaseOrderStatus = (cbosOpen, cbosPending, cboActive);
  TsgcHTTPCoinbaseOrderStatusSet = set of TsgcHTTPCoinbaseOrderStatus;

  TsgcHTTPCoinbaseOrder = class
  private
    FCancel_after: string;
    FClient_oid: string;
    FPost_only: Boolean;
    FPrice: Extended;
    FProduct_id: string;
    FSide: TsgcHTTPCoinbaseOrderSide;
    FSize: Extended;
    FSizeType: TsgcHTTPCoinbaseSizeType;
    FStop: TsgcHTTPCoinbaseOrderStop;
    FStop_price: Extended;
    FStp: TsgcHTTPCoinbaseSelfTradePrevention;
    FTimeInForce: TsgcHTTPCoinbaseTimeInForce;
    F_Type: TsgcHTTPCoinbaseOrderType;
  private
    function GetOrderTypeString(aType: TsgcHTTPCoinbaseOrderType): string;
    function GetOrderSideString(aSide: TsgcHTTPCoinbaseOrderSide): string;
    function GetOrderStopString(aOrderStop: TsgcHTTPCoinbaseOrderStop): string;
    function GetTimeInForceString(aTimeInForce
      : TsgcHTTPCoinbaseTimeInForce): string;
    function GetSelfTradePreventionString(aSelfTradePrevention
      : TsgcHTTPCoinbaseSelfTradePrevention): string;
    function GetOrderSizeTypeString(aSizeType
      : TsgcHTTPCoinbaseSizeType): string;
  protected
    function GetJSON: string; virtual;
  public
    property Cancel_after: string read FCancel_after write FCancel_after;
    property Client_oid: string read FClient_oid write FClient_oid;
    property Post_only: Boolean read FPost_only write FPost_only;
    property Price: Extended read FPrice write FPrice;
    property Product_id: string read FProduct_id write FProduct_id;
    property Side: TsgcHTTPCoinbaseOrderSide read FSide write FSide;
    property Size: Extended read FSize write FSize;
    property SizeType: TsgcHTTPCoinbaseSizeType read FSizeType write FSizeType;
    property Stop: TsgcHTTPCoinbaseOrderStop read FStop write FStop;
    property Stop_price: Extended read FStop_price write FStop_price;
    property Stp: TsgcHTTPCoinbaseSelfTradePrevention read FStp write FStp;
    property TimeInForce: TsgcHTTPCoinbaseTimeInForce read FTimeInForce
      write FTimeInForce;
    property _Type: TsgcHTTPCoinbaseOrderType read F_Type write F_Type;
  end;

  TsgcHTTPCoinbaseLog_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FFileName: String;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property FileName: String read FFileName write FFileName;
  end;

  TsgcHTTPCoinbase_Options = class(TPersistent)
  private
    FApiKey: String;
    FApiSecret: string;
    FLogOptions: TsgcHTTPCoinbaseLog_Options;
    FPassphrase: string;
    FSandBox: Boolean;
    procedure SetLogOptions(const Value: TsgcHTTPCoinbaseLog_Options);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property ApiKey: String read FApiKey write FApiKey;
    property ApiSecret: string read FApiSecret write FApiSecret;
    property LogOptions: TsgcHTTPCoinbaseLog_Options read FLogOptions
      write SetLogOptions;
    property Passphrase: string read FPassphrase write FPassphrase;
    property SandBox: Boolean read FSandBox write FSandBox;
  end;

  TsgcHTTP_API_Coinbase = class(TsgcHTTPAPI_Client)

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
      const aParameters: String = ''; const aHeaders: TStrings = nil)
      : string; virtual;
  protected
    procedure GetAuthHeaders(const aMethod, aRequestPath, aBody: String;
      var Headers: TStringList); virtual;
  protected
    function DoHTTP_GET_PRIVATE(const aRequestPath: String): string; virtual;
    function DoHTTP_POST_PRIVATE(const aRequestPath, aBody: String)
      : string; virtual;
    function DoHTTP_DELETE_PRIVATE(const aRequestPath: String): string; virtual;

    { http requests }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { properties }
  private
    FCoinbaseOptions: TsgcHTTPCoinbase_Options;
    procedure SetCoinbaseOptions(const Value: TsgcHTTPCoinbase_Options);
  public
    property CoinbaseOptions: TsgcHTTPCoinbase_Options read FCoinbaseOptions
      write SetCoinbaseOptions;
    { properties }
  end;

  TsgcHTTP_API_Coinbase_Rest = class(TsgcHTTP_API_Coinbase)
    { private }
  public
    function ListAccounts: string;
    function GetAccount(const aAccountId: string): string;
    function GetAccountHistory(const aAccountId: string): string;
    function GetHolds(const aAccountId: string): string;
  protected
    function DoPlaceNewOrder(const aOrder: TsgcHTTPCoinbaseOrder)
      : string; virtual;
  public
    // place order
    function PlaceNewOrder(const aOrder: TsgcHTTPCoinbaseOrder): string;
    function PlaceMarketOrder(const aSide: TsgcHTTPCoinbaseOrderSide;
      const aProductId: string; const aAmount: Extended;
      aUseFunds: Boolean = False; aClient_oid: string = ''): string;
    function PlaceLimitOrder(const aSide: TsgcHTTPCoinbaseOrderSide;
      const aProductId: string; aSize: Extended; aLimitPrice: Extended;
      aTimeInForce: TsgcHTTPCoinbaseTimeInForce = cbtfNone;
      aPostOnly: Boolean = False; aClient_oid: string = ''): string;
    function PlaceStopOrder(const aSide: TsgcHTTPCoinbaseOrderSide; const
        aProductId: string; aSize, aStopPrice, aLimitPrice: Extended; aStop:
        TsgcHTTPCoinbaseOrderStop; aClient_oid: string = ''): string;
    // cancel order
    function CancelOrder(const aOrderId: string): string;
    function CancelOrdersClient(const aClient_oid: string): string;
    function CancelAllOrders: string;
    // list orders
    function ListOrders(const aProductId: String = '';
      aStatus: TsgcHTTPCoinbaseOrderStatusSet = [cbosOpen, cbosPending,
      cboActive]): string;
    function GetOrder(const aOrderId: string): string;
    function GetOrdersClient(const aClient_oid: string): string;
  public
    // fills
    function GetFillsByOrderId(const aOrderId: string): string;
    function GetFillsByProductId(const aProductId: string): string;
  public
    // limits
    function GetCurrentExchangeLimits: string;
  public
    // deposits
    function ListDeposits(const aProfileId: string = '';
      const aBefore: TDateTime = 0; const aAfter: TDateTime = 0;
      const aLimit: integer = 100): string;
    function GetDeposit(const aTransferId: string): string;
    function DepositPaymentMethod(aAmount: Double; const aCurrency: string;
      const aPaymentMethodId: string): string;
    function DepositCoinbase(aAmount: Double; const aCurrency: string;
      const aCoinbaseAccountId: string): string;
    function DepositGenerateAddress(const aCoinbaseAccountId: string): string;
  public
    // withdrawals
    function ListWithdrawals(const aProfileId: string = '';
      const aBefore: TDateTime = 0; const aAfter: TDateTime = 0;
      const aLimit: integer = 100): string;
    function GetWithdrawal(const aTransferId: string): string;
    function WithdrawalPaymentMethod(aAmount: Double; const aCurrency: string;
      const aPaymentMethodId: string): string;
    function WithdrawalCoinbase(aAmount: Double; const aCurrency: string;
      const aCoinbaseAccountId: string): string;
    function WithdrawalCrypto(aAmount: Double;
      const aCurrency, aCryptoAddress: string): string;
    function GetWithdrawalFeeEstimate(const aCurrency: string;
      const aCryptoAddress: string): string;
  public
    // conversions
    function CreateConversion(const aFromCurrencyId, aToCurrencyId: string;
      aAmount: Double): string;
  public
    // payment methods
    function ListPaymentMethods: string;
  public
    // accounts
    function CoinbaseListAccounts: string;
  public
    // fees
    function GetFees: string;
  public
    // reports
    function CreateReportFills(aStartDate, aEndDate: TDateTime;
      const aProductId: string; const aFormat: string = 'pdf';
      const aEmail: string = ''): string;
    function CreateReportAccount(aStartDate, aEndDate: TDateTime;
      const aAccountId: string; const aFormat: string = 'pdf';
      const aEmail: string = ''): string;
    function GetReportStatus(const aReportId: string): string;
  public
    // profiles
    function ListProfiles(const aOnlyActive: Boolean = False): string;
    function GetProfile(const aProfileId: string): string;
    function CreateProfileTransfer(const aFromProfileId, aToProfileId: string;
      const aCurrency: string; aAmount: Double): string;
  public
    // margin
    function GetMarginProfileInformation(const aProductId: string): string;
    function GetMarginBuyingPower(const aProductId: string): string;
    function GetMarginWithdrawalPower(const aCurrency: string): string;
    function GetMarginAllWithdrawalPowers: string;
    function GetMarginExitPlan: string;
    function GetMarginListLiquidationHistory(aAfterDate: TDateTime = 0): string;
    function GetMarginPositionRefreshAmounts: string;
    function GetMarginStatus: string;
  public
    // oracle
    function GetOracle: string;
    { private }

    { market data }
  public
    function GetProducts: string;
    function GetSingleProduct(const aProductId: string): string;
    function GetProductOrderBook(const aProductId: string): string;
    function GetProductTicker(const aProductId: string): string;
    function GetTrades(const aProductId: string): string;
    function GetHistoricRates(const aProductId: string): string;
    function Get24hrStats(const aProductId: string): string;
  public
    function GetCurrencies: string;
    function GetCurrency(const aProductId: string): string;
  public
    function GetTime: string;
    { market data }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_HTTP}

uses
  DateUtils,
{$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
  // sgc
  sgcWebSocket_Helpers, sgcBase_Helpers, sgcJSON;

const
  CS_COINBASE_ENDPOINT_BASE = 'https://api.pro.coinbase.com';
  CS_COINBASE_ENDPOINT_SANDBOX_BASE =
    'https://api-public.sandbox.pro.coinbase.com';
  CS_COINBASE_DELIMITER = '&';

  // private
const
  CS_COINBASE_ENDPOINT_LIST_ACCOUNTS = '/accounts';
  CS_COINBASE_ENDPOINT_GET_ACCOUNT = '/accounts/%s';
  CS_COINBASE_ENDPOINT_GET_ACCOUNT_HISTORY = '/accounts/%s/ledger';
  CS_COINBASE_ENDPOINT_GET_HOLDS = '/accounts/%s/holds';

const
  CS_COINBASE_ENDPOINT_POST_ORDERS = '/orders';
  CS_COINBASE_ENDPOINT_DELETE_ORDER = '/orders/%s';
  CS_COINBASE_ENDPOINT_DELETE_ORDERS_CLIENT = '/orders/client:%s';
  CS_COINBASE_ENDPOINT_DELETE_ORDERS = '/orders';
  CS_COINBASE_ENDPOINT_LIST_ORDERS = '/orders';
  CS_COINBASE_ENDPOINT_GET_ORDER = '/orders/%s';
  CS_COINBASE_ENDPOINT_GET_ORDERS_CLIENT = '/orders/client:%s';

const
  CS_COINBASE_ENDPOINT_GET_FILLS = '/fills';

const
  CS_COINBASE_ENDPOINT_GET_CURRENT_EXCHANGE_LIMITS =
    '/users/self/exchange-limits';

const
  CS_COINBASE_ENDPOINT_LIST_DEPOSITS = '/transfers';
  CS_COINBASE_ENDPOINT_GET_DEPOSIT = '/transfers/%s';
  CS_COINBASE_ENDPOINT_POST_DEPOSIT_PAYMENT_METHOD = '/deposits/payment-method';
  CS_COINBASE_ENDPOINT_POST_DEPOSIT_COINBASE = '/deposits/coinbase-account';
  CS_COINBASE_ENDPOINT_POST_GENERATE_DEPOSIT_ADDRESS =
    '/coinbase-accounts/%s/addresses';

const
  CS_COINBASE_ENDPOINT_LIST_WITHDRAWALS = '/transfers';
  CS_COINBASE_ENDPOINT_GET_WITHDRAWAL = '/transfers/%s';
  CS_COINBASE_ENDPOINT_POST_WITHDRAWAL_PAYMENT_METHOD =
    '/withdrawals/payment-method';
  CS_COINBASE_ENDPOINT_POST_WITHDRAWAL_COINBASE =
    '/withdrawals/coinbase-account';
  CS_COINBASE_ENDPOINT_POST_WITHDRAWAL_CRYPTO = '/withdrawals/crypto';
  CS_COINBASE_ENDPOINT_GET_WITHDRAWAL_FEE_ESTIMATE =
    '/withdrawals/fee-estimate';

const
  CS_COINBASE_ENDPOINT_POST_CONVERSION = '/conversions';

const
  CS_COINBASE_ENDPOINT_GET_PAYMENT_METHODS = '/payment-methods';

const
  CS_COINBASE_ENDPOINT_GET_COINBASE_LIST_ACCOUNTS = '/coinbase-accounts';

const
  CS_COINBASE_ENDPOINT_GET_FEES = '/fees';

const
  CS_COINBASE_ENDPOINT_POST_REPORTS = '/reports';
  CS_COINBASE_ENDPOINT_GET_REPORT_STATUS = '/reports/%s';

const
  CS_COINBASE_ENDPOINT_LIST_PROFILES = '/profiles';
  CS_COINBASE_ENDPOINT_GET_PROFILE = '/profiles/%s';
  CS_COINBASE_ENDPOINT_POST_PROFILE_TRANSFER = '/profiles/transfer';

const
  CS_COINBASE_ENDPOINT_GET_MARGIN_PROFILE_INFORMATION =
    '/margin/profile_information';
  CS_COINBASE_ENDPOINT_GET_MARGIN_BUYING_POWER = '/margin/buying_power';
  CS_COINBASE_ENDPOINT_GET_MARGIN_WITHDRAWAL_POWER = '/margin/withdrawal_power';
  CS_COINBASE_ENDPOINT_GET_MARGIN_ALL_WITHDRAWAL_POWERS =
    '/margin/withdrawal_power_all';
  CS_COINBASE_ENDPOINT_GET_MARGIN_EXIT_PLAN = '/margin/exit_plan';
  CS_COINBASE_ENDPOINT_GET_MARGIN_LIQUIDATION_HISTORY =
    '/margin/liquidation_history';
  CS_COINBASE_ENDPOINT_GET_MARGIN_POSITION_REFRESH_AMOUNTS =
    '/margin/position_refresh_amounts';
  CS_COINBASE_ENDPOINT_GET_MARGIN_STATUS = '/margin/status';

const
  CS_COINBASE_ENDPOINT_GET_ORACLE = '/oracle';

  // market data
const
  CS_COINBASE_ENDPOINT_GET_PRODUCTS = '/products';
  CS_COINBASE_ENDPOINT_GET_PRODUCT = '/products/%s';
  CS_COINBASE_ENDPOINT_GET_PRODUCT_ORDER_BOOK = '/products/%s/book';
  CS_COINBASE_ENDPOINT_GET_PRODUCT_TICKER = '/products/%s/ticker';
  CS_COINBASE_ENDPOINT_GET_TRADES = '/products/%s/trades';
  CS_COINBASE_ENDPOINT_GET_HISTORIC_RATES = '/products/%s/candles';
  CS_COINBASE_ENDPOINT_GET_24HR_STATS = '/products/%s/stats';

const
  CS_COINBASE_ENDPOINT_GET_CURRENCIES = '/currencies';
  CS_COINBASE_ENDPOINT_GET_CURRENCY = '/currencies/%s';

const
  CS_COINBASE_ENDPOINT_GET_TIME = '/time';

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

procedure TsgcHTTPCoinbaseLog_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTPCoinbaseLog_Options then
  begin
    Enabled := TsgcHTTPCoinbaseLog_Options(aSource).Enabled;
    FileName := TsgcHTTPCoinbaseLog_Options(aSource).FileName;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcHTTPCoinbase_Options.Create;
begin
  inherited;
  FLogOptions := TsgcHTTPCoinbaseLog_Options.Create;
end;

destructor TsgcHTTPCoinbase_Options.Destroy;
begin
  sgcFree(FLogOptions);
  inherited;
end;

procedure TsgcHTTPCoinbase_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTPCoinbase_Options then
  begin
    ApiKey := TsgcHTTPCoinbase_Options(aSource).ApiKey;
    ApiSecret := TsgcHTTPCoinbase_Options(aSource).ApiSecret;
    Passphrase := TsgcHTTPCoinbase_Options(aSource).Passphrase;
    SandBox := TsgcHTTPCoinbase_Options(aSource).SandBox;
    LogOptions := TsgcHTTPCoinbase_Options(aSource).LogOptions;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcHTTPCoinbase_Options.SetLogOptions(const Value
  : TsgcHTTPCoinbaseLog_Options);
begin
  if Assigned(FLogOptions) then
    FLogOptions.Assign(Value);
end;

constructor TsgcHTTP_API_Coinbase.Create(aOwner: TComponent);
begin
  inherited;
  FCoinbaseOptions := TsgcHTTPCoinbase_Options.Create;
end;

destructor TsgcHTTP_API_Coinbase.Destroy;
begin
  sgcFree(FCoinbaseOptions);
  inherited;
end;

function TsgcHTTP_API_Coinbase.DoHTTP_GET(const aMethod: String;
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

function TsgcHTTP_API_Coinbase.DoHTTP_POST(const aMethod: String;
  const aBody: String = ''; const aHeaders: TStrings = nil): string;
begin
  DoInitializeLog;

  Result := Post(GetBaseURL + aMethod, aBody, aHeaders);
end;

function TsgcHTTP_API_Coinbase.DoHTTP_GET_PRIVATE(const aRequestPath
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

procedure TsgcHTTP_API_Coinbase.DoInitializeLog;
begin
  if Log <> CoinbaseOptions.LogOptions.Enabled then
  begin
    LogFileName := CoinbaseOptions.LogOptions.FileName;
    Log := CoinbaseOptions.LogOptions.Enabled;
  end;
end;

function TsgcHTTP_API_Coinbase_Rest.Get24hrStats(const aProductId
  : string): string;
begin
  Result := DoHTTP_GET(Format(CS_COINBASE_ENDPOINT_GET_24HR_STATS,
    [aProductId]));
end;

function TsgcHTTP_API_Coinbase_Rest.GetAccount(const aAccountId
  : string): string;
begin
  Result := DoHTTP_GET_PRIVATE(Format(CS_COINBASE_ENDPOINT_GET_ACCOUNT,
    [aAccountId]));
end;

function TsgcHTTP_API_Coinbase_Rest.GetAccountHistory(const aAccountId
  : string): string;
begin
  Result := DoHTTP_GET_PRIVATE(Format(CS_COINBASE_ENDPOINT_GET_ACCOUNT_HISTORY,
    [aAccountId]));
end;

procedure TsgcHTTP_API_Coinbase.GetAuthHeaders(const aMethod, aRequestPath,
  aBody: String; var Headers: TStringList);
var
  vTimestamp: string;
  oSecret: TsgcStringStream;
  vSignature: string;
begin
  // get signature
  vTimestamp := GetDateTimeUnix(Now, False);
  oSecret := TsgcStringStream.Create('');
  Try
    DecodeBase64(CoinbaseOptions.ApiSecret, TMemoryStream(oSecret));
    oSecret.Size := 64;
    vSignature := GetHMACSHA256(vTimestamp + aMethod + aRequestPath + aBody,
      TIdBytes(oSecret.Bytes), True);
  Finally
    sgcFree(oSecret);
  end;

  // set signature headers
  Headers.Add('CB-ACCESS-KEY: ' + CoinbaseOptions.ApiKey);
  Headers.Add('CB-ACCESS-SIGN: ' + vSignature);
  Headers.Add('CB-ACCESS-TIMESTAMP: ' + vTimestamp);
  Headers.Add('CB-ACCESS-PASSPHRASE: ' + CoinbaseOptions.Passphrase);
end;

function TsgcHTTP_API_Coinbase.GetBaseURL: string;
begin
  if CoinbaseOptions.SandBox then
    Result := CS_COINBASE_ENDPOINT_SANDBOX_BASE
  else
    Result := CS_COINBASE_ENDPOINT_BASE;
end;

function TsgcHTTP_API_Coinbase_Rest.CancelAllOrders: string;
begin
  Result := DoHTTP_DELETE_PRIVATE(CS_COINBASE_ENDPOINT_DELETE_ORDERS);
end;

function TsgcHTTP_API_Coinbase_Rest.CancelOrder(const aOrderId: string): string;
begin
  Result := DoHTTP_DELETE_PRIVATE(Format(CS_COINBASE_ENDPOINT_DELETE_ORDER,
    [aOrderId]));
end;

function TsgcHTTP_API_Coinbase_Rest.CancelOrdersClient(const aClient_oid
  : string): string;
begin
  Result := DoHTTP_DELETE_PRIVATE
    (Format(CS_COINBASE_ENDPOINT_DELETE_ORDERS_CLIENT, [aClient_oid]));
end;

function TsgcHTTP_API_Coinbase_Rest.CoinbaseListAccounts: string;
begin
  Result := DoHTTP_GET_PRIVATE(CS_COINBASE_ENDPOINT_GET_COINBASE_LIST_ACCOUNTS);
end;

function TsgcHTTP_API_Coinbase_Rest.CreateConversion(const aFromCurrencyId,
  aToCurrencyId: string; aAmount: Double): string;
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.AddPair('from', aFromCurrencyId);
    oJSON.AddPair('to', aToCurrencyId);
    oJSON.AddPair('amount', GetFormatExtended(aAmount));

    Result := DoHTTP_POST_PRIVATE(CS_COINBASE_ENDPOINT_POST_CONVERSION,
      oJSON.Text);
  Finally
    sgcFree(oJSON);
  End;
end;

function TsgcHTTP_API_Coinbase_Rest.CreateProfileTransfer(const aFromProfileId,
  aToProfileId: string; const aCurrency: string; aAmount: Double): string;
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.AddPair('from', aFromProfileId);
    oJSON.AddPair('to', aToProfileId);
    oJSON.AddPair('currency', aCurrency);
    oJSON.AddPair('amount', GetFormatExtended(aAmount));

    Result := DoHTTP_POST_PRIVATE(CS_COINBASE_ENDPOINT_POST_PROFILE_TRANSFER,
      oJSON.Text);
  Finally
    sgcFree(oJSON);
  End;
end;

function TsgcHTTP_API_Coinbase_Rest.CreateReportAccount(aStartDate,
  aEndDate: TDateTime; const aAccountId: string; const aFormat: string = 'pdf';
  const aEmail: string = ''): string;
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.AddPair('type', 'account');
    oJSON.AddPair('start_date', sgcGetUTCString(aStartDate));
    oJSON.AddPair('end_date', sgcGetUTCString(aEndDate));
    oJSON.AddPair('account_id', aAccountId);
    oJSON.AddPair('format', aFormat);
    if aEmail <> '' then
      oJSON.AddPair('email', aEmail);

    Result := DoHTTP_POST_PRIVATE(CS_COINBASE_ENDPOINT_POST_REPORTS,
      oJSON.Text);
  Finally
    sgcFree(oJSON);
  End;
end;

function TsgcHTTP_API_Coinbase_Rest.CreateReportFills(aStartDate,
  aEndDate: TDateTime; const aProductId: string; const aFormat: string = 'pdf';
  const aEmail: string = ''): string;
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.AddPair('type', 'fills');
    oJSON.AddPair('start_date', sgcGetUTCString(aStartDate));
    oJSON.AddPair('end_date', sgcGetUTCString(aEndDate));
    oJSON.AddPair('product_id', aProductId);
    oJSON.AddPair('format', aFormat);
    if aEmail <> '' then
      oJSON.AddPair('email', aEmail);

    Result := DoHTTP_POST_PRIVATE(CS_COINBASE_ENDPOINT_POST_REPORTS,
      oJSON.Text);
  Finally
    sgcFree(oJSON);
  End;
end;

function TsgcHTTP_API_Coinbase_Rest.DepositCoinbase(aAmount: Double;
  const aCurrency: string; const aCoinbaseAccountId: string): string;
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.AddPair('amount', GetFormatExtended(aAmount));
    oJSON.AddPair('currency', aCurrency);
    oJSON.AddPair('coinbase_account_id', aCoinbaseAccountId);

    Result := DoHTTP_POST_PRIVATE(CS_COINBASE_ENDPOINT_POST_DEPOSIT_COINBASE,
      oJSON.Text);
  Finally
    sgcFree(oJSON);
  End;
end;

function TsgcHTTP_API_Coinbase_Rest.DepositGenerateAddress
  (const aCoinbaseAccountId: string): string;
begin
  Result := DoHTTP_POST_PRIVATE
    (Format(CS_COINBASE_ENDPOINT_POST_GENERATE_DEPOSIT_ADDRESS,
    [aCoinbaseAccountId]), '');
end;

function TsgcHTTP_API_Coinbase_Rest.DepositPaymentMethod(aAmount: Double;
  const aCurrency: string; const aPaymentMethodId: string): string;
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.AddPair('amount', GetFormatExtended(aAmount));
    oJSON.AddPair('currency', aCurrency);
    oJSON.AddPair('payment_method_id', aPaymentMethodId);

    Result := DoHTTP_POST_PRIVATE
      (CS_COINBASE_ENDPOINT_POST_DEPOSIT_PAYMENT_METHOD, oJSON.Text);
  Finally
    sgcFree(oJSON);
  End;
end;

function TsgcHTTP_API_Coinbase_Rest.DoPlaceNewOrder(const aOrder
  : TsgcHTTPCoinbaseOrder): string;
begin
  Result := DoHTTP_POST_PRIVATE(CS_COINBASE_ENDPOINT_POST_ORDERS,
    aOrder.GetJSON);
end;

function TsgcHTTP_API_Coinbase_Rest.GetMarginBuyingPower(const aProductId
  : string): string;
begin
  Result := DoHTTP_GET_PRIVATE(CS_COINBASE_ENDPOINT_GET_MARGIN_BUYING_POWER +
    '?product_id=' + aProductId);
end;

function TsgcHTTP_API_Coinbase_Rest.GetCurrencies: string;
begin
  Result := DoHTTP_GET(CS_COINBASE_ENDPOINT_GET_CURRENCIES);
end;

function TsgcHTTP_API_Coinbase_Rest.GetCurrency(const aProductId
  : string): string;
begin
  Result := DoHTTP_GET(Format(CS_COINBASE_ENDPOINT_GET_CURRENCY, [aProductId]));
end;

function TsgcHTTP_API_Coinbase_Rest.GetCurrentExchangeLimits: string;
begin
  Result := DoHTTP_GET_PRIVATE
    (CS_COINBASE_ENDPOINT_GET_CURRENT_EXCHANGE_LIMITS);
end;

function TsgcHTTP_API_Coinbase_Rest.GetDeposit(const aTransferId
  : string): string;
begin
  Result := DoHTTP_GET_PRIVATE(Format(CS_COINBASE_ENDPOINT_GET_DEPOSIT,
    [aTransferId]));
end;

function TsgcHTTP_API_Coinbase_Rest.GetFees: string;
begin
  Result := DoHTTP_GET_PRIVATE(CS_COINBASE_ENDPOINT_GET_FEES);
end;

function TsgcHTTP_API_Coinbase_Rest.GetWithdrawal(const aTransferId
  : string): string;
begin
  Result := DoHTTP_GET_PRIVATE(Format(CS_COINBASE_ENDPOINT_GET_WITHDRAWAL,
    [aTransferId]));
end;

function TsgcHTTP_API_Coinbase_Rest.GetFillsByOrderId(const aOrderId
  : string): string;
begin
  Result := DoHTTP_GET_PRIVATE(CS_COINBASE_ENDPOINT_GET_FILLS + '?order_id=' +
    aOrderId);
end;

function TsgcHTTP_API_Coinbase_Rest.GetFillsByProductId(const aProductId
  : string): string;
begin
  Result := DoHTTP_GET_PRIVATE(CS_COINBASE_ENDPOINT_GET_FILLS + '?product_id=' +
    aProductId);
end;

function TsgcHTTP_API_Coinbase_Rest.GetHistoricRates(const aProductId
  : string): string;
begin
  Result := DoHTTP_GET(Format(CS_COINBASE_ENDPOINT_GET_HISTORIC_RATES,
    [aProductId]));
end;

function TsgcHTTP_API_Coinbase_Rest.GetHolds(const aAccountId: string): string;
begin
  Result := DoHTTP_GET_PRIVATE(Format(CS_COINBASE_ENDPOINT_GET_HOLDS,
    [aAccountId]));
end;

function TsgcHTTP_API_Coinbase_Rest.GetMarginAllWithdrawalPowers: string;
begin
  Result := DoHTTP_GET_PRIVATE
    (CS_COINBASE_ENDPOINT_GET_MARGIN_ALL_WITHDRAWAL_POWERS);
end;

function TsgcHTTP_API_Coinbase_Rest.GetMarginExitPlan: string;
begin
  Result := DoHTTP_GET_PRIVATE(CS_COINBASE_ENDPOINT_GET_MARGIN_EXIT_PLAN);
end;

function TsgcHTTP_API_Coinbase_Rest.GetMarginListLiquidationHistory
  (aAfterDate: TDateTime = 0): string;
var
  vRequestPath: string;
begin
  vRequestPath := CS_COINBASE_ENDPOINT_GET_MARGIN_LIQUIDATION_HISTORY;
  if aAfterDate > 0 then
    vRequestPath := vRequestPath + '?after=' + sgcGetUTCString(aAfterDate);
  Result := DoHTTP_GET_PRIVATE(vRequestPath);
end;

function TsgcHTTP_API_Coinbase_Rest.GetMarginPositionRefreshAmounts: string;
begin
  Result := DoHTTP_GET_PRIVATE
    (CS_COINBASE_ENDPOINT_GET_MARGIN_POSITION_REFRESH_AMOUNTS);
end;

function TsgcHTTP_API_Coinbase_Rest.GetMarginProfileInformation(const aProductId
  : string): string;
begin
  Result := DoHTTP_GET_PRIVATE
    (CS_COINBASE_ENDPOINT_GET_MARGIN_PROFILE_INFORMATION + '?product_id=' +
    aProductId);
end;

function TsgcHTTP_API_Coinbase_Rest.GetMarginStatus: string;
begin
  Result := DoHTTP_GET_PRIVATE(CS_COINBASE_ENDPOINT_GET_MARGIN_STATUS);
end;

function TsgcHTTP_API_Coinbase_Rest.GetMarginWithdrawalPower(const aCurrency
  : string): string;
begin
  Result := DoHTTP_GET_PRIVATE(CS_COINBASE_ENDPOINT_GET_MARGIN_WITHDRAWAL_POWER
    + '?currency=' + aCurrency);
end;

function TsgcHTTP_API_Coinbase_Rest.GetOracle: string;
begin
  Result := DoHTTP_GET_PRIVATE(CS_COINBASE_ENDPOINT_GET_ORACLE);;
end;

function TsgcHTTP_API_Coinbase_Rest.GetOrder(const aOrderId: string): string;
begin
  Result := DoHTTP_GET_PRIVATE(Format(CS_COINBASE_ENDPOINT_GET_ORDER,
    [aOrderId]));
end;

function TsgcHTTP_API_Coinbase_Rest.GetOrdersClient(const aClient_oid
  : string): string;
begin
  Result := DoHTTP_GET_PRIVATE(Format(CS_COINBASE_ENDPOINT_GET_ORDERS_CLIENT,
    [aClient_oid]));
end;

function TsgcHTTP_API_Coinbase_Rest.GetProductOrderBook(const aProductId
  : string): string;
begin
  Result := DoHTTP_GET(Format(CS_COINBASE_ENDPOINT_GET_PRODUCT_ORDER_BOOK,
    [aProductId]));
end;

function TsgcHTTP_API_Coinbase_Rest.GetProducts: string;
begin
  Result := DoHTTP_GET(CS_COINBASE_ENDPOINT_GET_PRODUCTS);
end;

function TsgcHTTP_API_Coinbase_Rest.GetProductTicker(const aProductId
  : string): string;
begin
  Result := DoHTTP_GET(Format(CS_COINBASE_ENDPOINT_GET_PRODUCT_TICKER,
    [aProductId]));
end;

function TsgcHTTP_API_Coinbase_Rest.GetProfile(const aProfileId
  : string): string;
begin
  Result := DoHTTP_GET_PRIVATE(Format(CS_COINBASE_ENDPOINT_GET_PROFILE,
    [aProfileId]));
end;

function TsgcHTTP_API_Coinbase_Rest.GetReportStatus(const aReportId
  : string): string;
begin
  Result := DoHTTP_GET_PRIVATE(Format(CS_COINBASE_ENDPOINT_GET_REPORT_STATUS,
    [aReportId]));
end;

function TsgcHTTP_API_Coinbase_Rest.GetSingleProduct(const aProductId
  : string): string;
begin
  Result := DoHTTP_GET(Format(CS_COINBASE_ENDPOINT_GET_PRODUCT, [aProductId]));
end;

function TsgcHTTP_API_Coinbase_Rest.GetTime: string;
begin
  Result := DoHTTP_GET(CS_COINBASE_ENDPOINT_GET_TIME);
end;

function TsgcHTTP_API_Coinbase_Rest.GetTrades(const aProductId: string): string;
begin
  Result := DoHTTP_GET(Format(CS_COINBASE_ENDPOINT_GET_TRADES, [aProductId]));
end;

function TsgcHTTP_API_Coinbase_Rest.GetWithdrawalFeeEstimate(const aCurrency
  : string; const aCryptoAddress: string): string;
var
  oParameters: TStringList;
  vRequestPath: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_COINBASE_DELIMITER;
    oParameters.Add('currency=' + aCurrency);
    oParameters.Add('crypto_address=' + aCryptoAddress);
    vRequestPath := CS_COINBASE_ENDPOINT_GET_WITHDRAWAL_FEE_ESTIMATE;
    if oParameters.Count > 0 then
      vRequestPath := vRequestPath + '?' + oParameters.DelimitedText;
    Result := DoHTTP_GET_PRIVATE(vRequestPath);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Coinbase_Rest.ListAccounts: string;
begin
  Result := DoHTTP_GET_PRIVATE(CS_COINBASE_ENDPOINT_LIST_ACCOUNTS);
end;

function TsgcHTTP_API_Coinbase_Rest.ListDeposits(const aProfileId: string = '';
  const aBefore: TDateTime = 0; const aAfter: TDateTime = 0;
  const aLimit: integer = 100): string;
var
  oParameters: TStringList;
  vRequestPath: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_COINBASE_DELIMITER;
    oParameters.Add('type=deposit');
    if aProfileId <> '' then
      oParameters.Add('profile_id=' + aProfileId);
    if aBefore > 0 then
      oParameters.Add('before=' + GetDateTimeUnix(aBefore, False));
    if aAfter > 0 then
      oParameters.Add('after=' + GetDateTimeUnix(aAfter, False));
    if aLimit < 100 then
      oParameters.Add('limit=' + IntToStr(aLimit));
    vRequestPath := CS_COINBASE_ENDPOINT_LIST_DEPOSITS;
    if oParameters.Count > 0 then
      vRequestPath := vRequestPath + '?' + oParameters.DelimitedText;
    Result := DoHTTP_GET_PRIVATE(vRequestPath);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Coinbase_Rest.ListOrders(const aProductId: String = '';
  aStatus: TsgcHTTPCoinbaseOrderStatusSet = [cbosOpen, cbosPending,
  cboActive]): string;
var
  oParameters: TStringList;
  vRequestPath: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_COINBASE_DELIMITER;
    if aProductId <> '' then
      oParameters.Add('product_id=' + aProductId);
    if aStatus = [cbosOpen, cbosPending, cboActive] then
      oParameters.Add('status=all')
    else
    begin
      if cbosOpen in aStatus then
        oParameters.Add('status=open');
      if cbosPending in aStatus then
        oParameters.Add('status=pending');
      if cboActive in aStatus then
        oParameters.Add('status=active');
    end;
    vRequestPath := CS_COINBASE_ENDPOINT_LIST_ORDERS;
    if oParameters.Count > 0 then
      vRequestPath := vRequestPath + '?' + oParameters.DelimitedText;
    Result := DoHTTP_GET_PRIVATE(vRequestPath);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Coinbase_Rest.ListPaymentMethods: string;
begin
  Result := DoHTTP_GET_PRIVATE(CS_COINBASE_ENDPOINT_GET_PAYMENT_METHODS);
end;

function TsgcHTTP_API_Coinbase_Rest.ListProfiles(const aOnlyActive
  : Boolean = False): string;
var
  vRequestPath: string;
begin
  vRequestPath := CS_COINBASE_ENDPOINT_LIST_PROFILES;
  if aOnlyActive then
    vRequestPath := vRequestPath + '?active=true';
  Result := DoHTTP_GET_PRIVATE(vRequestPath);
end;

function TsgcHTTP_API_Coinbase_Rest.ListWithdrawals(const aProfileId
  : string = ''; const aBefore: TDateTime = 0; const aAfter: TDateTime = 0;
  const aLimit: integer = 100): string;
var
  oParameters: TStringList;
  vRequestPath: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_COINBASE_DELIMITER;
    oParameters.Add('type=withdraw');
    if aProfileId <> '' then
      oParameters.Add('profile_id=' + aProfileId);
    if aBefore > 0 then
      oParameters.Add('before=' + GetDateTimeUnix(aBefore, False));
    if aAfter > 0 then
      oParameters.Add('after=' + GetDateTimeUnix(aAfter, False));
    if aLimit < 100 then
      oParameters.Add('limit=' + IntToStr(aLimit));
    vRequestPath := CS_COINBASE_ENDPOINT_LIST_WITHDRAWALS;
    if oParameters.Count > 0 then
      vRequestPath := vRequestPath + '?' + oParameters.DelimitedText;
    Result := DoHTTP_GET_PRIVATE(vRequestPath);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Coinbase_Rest.PlaceLimitOrder
  (const aSide: TsgcHTTPCoinbaseOrderSide; const aProductId: string;
  aSize: Extended; aLimitPrice: Extended;
  aTimeInForce: TsgcHTTPCoinbaseTimeInForce = cbtfNone;
  aPostOnly: Boolean = False; aClient_oid: string = ''): string;
var
  oOrder: TsgcHTTPCoinbaseOrder;
begin
  oOrder := TsgcHTTPCoinbaseOrder.Create;
  Try
    oOrder._Type := coioLimit;
    oOrder.Side := aSide;
    oOrder.Product_id := aProductId;
    oOrder.Size := aSize;
    oOrder.Price := aLimitPrice;
    oOrder.TimeInForce := aTimeInForce;
    oOrder.Post_only := aPostOnly;
    oOrder.Client_oid := aClient_oid;
    Result := DoPlaceNewOrder(oOrder);
  Finally
    sgcFree(oOrder);
  End;
end;

function TsgcHTTP_API_Coinbase_Rest.PlaceMarketOrder
  (const aSide: TsgcHTTPCoinbaseOrderSide; const aProductId: string;
  const aAmount: Extended; aUseFunds: Boolean = False;
  aClient_oid: string = ''): string;
var
  oOrder: TsgcHTTPCoinbaseOrder;
begin
  oOrder := TsgcHTTPCoinbaseOrder.Create;
  Try
    oOrder._Type := coioMarket;
    oOrder.Side := aSide;
    oOrder.Product_id := aProductId;
    oOrder.Size := aAmount;
    if aUseFunds then
      oOrder.SizeType := csztUseFunds
    else
      oOrder.SizeType := csztUseSize;
    oOrder.Client_oid := aClient_oid;
    Result := DoPlaceNewOrder(oOrder);
  Finally
    sgcFree(oOrder);
  End;
end;

function TsgcHTTP_API_Coinbase_Rest.PlaceNewOrder(const aOrder
  : TsgcHTTPCoinbaseOrder): string;
begin
  Result := DoPlaceNewOrder(aOrder);
end;

function TsgcHTTP_API_Coinbase_Rest.PlaceStopOrder(const aSide:
    TsgcHTTPCoinbaseOrderSide; const aProductId: string; aSize, aStopPrice,
    aLimitPrice: Extended; aStop: TsgcHTTPCoinbaseOrderStop; aClient_oid:
    string = ''): string;
var
  oOrder: TsgcHTTPCoinbaseOrder;
begin
  oOrder := TsgcHTTPCoinbaseOrder.Create;
  Try
    oOrder._Type := coioLimit;
    oOrder.Side := aSide;
    oOrder.Product_id := aProductId;
    oOrder.Size := aSize;
    oOrder.Stop_price := aStopPrice;
    oOrder.Stop := aStop;
    oOrder.Price := aLimitPrice;
    oOrder.Client_oid := aClient_oid;
    Result := DoPlaceNewOrder(oOrder);
  Finally
    sgcFree(oOrder);
  End;
end;

function TsgcHTTP_API_Coinbase_Rest.WithdrawalCoinbase(aAmount: Double;
  const aCurrency: string; const aCoinbaseAccountId: string): string;
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.AddPair('amount', GetFormatExtended(aAmount));
    oJSON.AddPair('currency', aCurrency);
    oJSON.AddPair('coinbase_account_id', aCoinbaseAccountId);

    Result := DoHTTP_POST_PRIVATE(CS_COINBASE_ENDPOINT_POST_WITHDRAWAL_COINBASE,
      oJSON.Text);
  Finally
    sgcFree(oJSON);
  End;
end;

function TsgcHTTP_API_Coinbase_Rest.WithdrawalCrypto(aAmount: Double;
  const aCurrency, aCryptoAddress: string): string;
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.AddPair('amount', GetFormatExtended(aAmount));
    oJSON.AddPair('currency', aCurrency);
    oJSON.AddPair('crypto_address', aCryptoAddress);

    Result := DoHTTP_POST_PRIVATE(CS_COINBASE_ENDPOINT_POST_WITHDRAWAL_CRYPTO,
      oJSON.Text);
  Finally
    sgcFree(oJSON);
  End;
end;

function TsgcHTTP_API_Coinbase_Rest.WithdrawalPaymentMethod(aAmount: Double;
  const aCurrency: string; const aPaymentMethodId: string): string;
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.AddPair('amount', GetFormatExtended(aAmount));
    oJSON.AddPair('currency', aCurrency);
    oJSON.AddPair('payment_method_id', aPaymentMethodId);

    Result := DoHTTP_POST_PRIVATE
      (CS_COINBASE_ENDPOINT_POST_WITHDRAWAL_PAYMENT_METHOD, oJSON.Text);
  Finally
    sgcFree(oJSON);
  End;
end;

function TsgcHTTP_API_Coinbase.DoHTTP_DELETE(const aMethod: String;
  const aParameters: String = ''; const aHeaders: TStrings = nil): string;
var
  vURL: String;
begin
  DoInitializeLog;

  vURL := GetBaseURL + aMethod;
  if aParameters <> '' then
    vURL := vURL + '?' + aParameters;
  Result := Delete(vURL, aHeaders);
end;

function TsgcHTTP_API_Coinbase.DoHTTP_DELETE_PRIVATE(const aRequestPath
  : String): string;
var
  oHeaders: TStringList;
begin
  oHeaders := TStringList.Create;
  Try
    oHeaders.Add('content-type: application/json');
    GetAuthHeaders('DELETE', aRequestPath, '', oHeaders);
    Result := DoHTTP_DELETE(aRequestPath, '', oHeaders);
  Finally
    sgcFree(oHeaders);
  End;
end;

function TsgcHTTP_API_Coinbase.DoHTTP_POST_PRIVATE(const aRequestPath,
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

procedure TsgcHTTP_API_Coinbase.SetCoinbaseOptions
  (const Value: TsgcHTTPCoinbase_Options);
begin
  if Assigned(FCoinbaseOptions) then
    FCoinbaseOptions.Assign(Value);
end;

function TsgcHTTPCoinbaseOrder.GetJSON: string;
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    // ... common fields mandatories
    oJSON.AddPair('type', GetOrderTypeString(_Type));
    oJSON.AddPair('side', GetOrderSideString(Side));
    oJSON.AddPair('product_id', Product_id);
    // ... common fields optionals
    if Client_oid <> '' then
      oJSON.AddPair('client_oid', Client_oid);
    if Stp <> stpNone then
      oJSON.AddPair('stp', GetSelfTradePreventionString(Stp));
    if Stop <> costNone then
    begin
      oJSON.AddPair('stop', GetOrderStopString(Stop));
      oJSON.AddPair('stop_price', GetFormatExtended(Stop_price));
    end;

    // ... limit orders
    if _Type = coioLimit then
    begin
      oJSON.AddPair('price', GetFormatExtended(Price));
      oJSON.AddPair('size', GetFormatExtended(Size));
      if TimeInForce <> cbtfNone then
        oJSON.AddPair('time_in_force', GetTimeInForceString(TimeInForce));
      if (TimeInForce = cbtfGTT) and (Cancel_after <> '') then
        oJSON.AddPair('cancel_after', Cancel_after);
      if (TimeInForce <> cbtfIOC) and (TimeInForce <> cbtfFOK) then
        oJSON.AddPair('post_only', GetFormatBoolean(Post_only));
    end
    // ... market orders
    else if _Type = coioMarket then
      oJSON.AddPair(GetOrderSizeTypeString(SizeType), GetFormatExtended(Size));

    Result := oJSON.Text;
  Finally
    sgcFree(oJSON);
  End;
end;

function TsgcHTTPCoinbaseOrder.GetOrderSideString
  (aSide: TsgcHTTPCoinbaseOrderSide): string;
begin
  case aSide of
    coisBuy:
      Result := 'buy';
    coisSell:
      Result := 'sell';
  else
    Result := '';
  end;
end;

function TsgcHTTPCoinbaseOrder.GetOrderSizeTypeString
  (aSizeType: TsgcHTTPCoinbaseSizeType): string;
begin
  case aSizeType of
    csztUseSize:
      Result := 'size';
    csztUseFunds:
      Result := 'funds';
  else
    Result := '';
  end;
end;

function TsgcHTTPCoinbaseOrder.GetOrderStopString
  (aOrderStop: TsgcHTTPCoinbaseOrderStop): string;
begin
  case aOrderStop of
    costLoss:
      Result := 'loss';
    costEntry:
      Result := 'entry';
  else
    Result := '';
  end;
end;

function TsgcHTTPCoinbaseOrder.GetOrderTypeString
  (aType: TsgcHTTPCoinbaseOrderType): string;
begin
  case aType of
    coioLimit:
      Result := 'limit';
    coioMarket:
      Result := 'market';
  else
    Result := '';
  end;
end;

function TsgcHTTPCoinbaseOrder.GetSelfTradePreventionString(aSelfTradePrevention
  : TsgcHTTPCoinbaseSelfTradePrevention): string;
begin
  case aSelfTradePrevention of
    stpDC:
      Result := 'dc';
    stpCO:
      Result := 'co';
    stpCN:
      Result := 'cn';
    stpCB:
      Result := 'cb';
  else
    Result := '';
  end;
end;

function TsgcHTTPCoinbaseOrder.GetTimeInForceString(aTimeInForce
  : TsgcHTTPCoinbaseTimeInForce): string;
begin
  case aTimeInForce of
    cbtfGTC:
      Result := 'GTC';
    cbtfGTT:
      Result := 'GTT';
    cbtfIOC:
      Result := 'IOC';
    cbtfFOK:
      Result := 'FOK';
  else
    Result := '';
  end;
end;

{$ENDIF}

end.
