{ ***************************************************************************
  sgcHTTP API component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcHTTP_API_ThreeCommas;

interface

{$I sgcVer.inc}
{$IFDEF SGC_HTTP}

uses
  Classes, SysUtils,
  // sgc
  sgcHTTP_API;

type

  TsgcHTTPThreeCommasOrderSide = (os3cNone, os3cBuy, os3cSell);
  TsgcHTTPThreeCommasOrderType = (ot3cNone, ot3cMarket, ot3cLimit, ot3cConditional);
  TsgcHTTPThreeCommasPriceType = (pt3cNone, pt3cBid, pt3cAsk, pt3cLast);
  TsgcHTTPThreeCommasLeverageType = (lt3cNonce, lt3cCustom, lt3cCross);

  TsgcHTTPThreeCommasOrderTrailing = class
  private
    FEnabled: Boolean;
    FPercent: Extended;
  public
    property Enabled: Boolean read FEnabled write FEnabled;
    property Percent: Extended read FPercent write FPercent;
  end;

  TsgcHTTPThreeCommasOrderPrice = class
  private
    FPercent: Extended;
    FValue: Extended;
    F_Type: TsgcHTTPThreeCommasPriceType;
  public
    property Value: Extended read FValue write FValue;
    property _Type: TsgcHTTPThreeCommasPriceType read F_Type write F_Type;
    property Percent: Extended read FPercent write FPercent;
  end;

  TsgcHTTPThreeCommasOrderConditional = class
  private
    FOrderType: TsgcHTTPThreeCommasOrderType;
    FPrice: TsgcHTTPThreeCommasOrderPrice;
    FTrailing: TsgcHTTPThreeCommasOrderTrailing;
    function GetPrice: TsgcHTTPThreeCommasOrderPrice;
    function GetTrailing: TsgcHTTPThreeCommasOrderTrailing;
  public
    destructor Destroy; override;
  public
    property Price: TsgcHTTPThreeCommasOrderPrice read GetPrice write FPrice;
    property OrderType: TsgcHTTPThreeCommasOrderType read FOrderType
      write FOrderType;
    property Trailing: TsgcHTTPThreeCommasOrderTrailing read GetTrailing
      write FTrailing;
  end;

  TsgcHTTPThreeCommasOrderPosition = class
  private
    FConditional: TsgcHTTPThreeCommasOrderConditional;
    FOrderType: TsgcHTTPThreeCommasOrderType;
    FPrice: TsgcHTTPThreeCommasOrderPrice;
    FUnits: Extended;
    F_Type: TsgcHTTPThreeCommasOrderSide;
    function GetConditional: TsgcHTTPThreeCommasOrderConditional;
    function GetPrice: TsgcHTTPThreeCommasOrderPrice;
  public
    destructor Destroy; override;
  public
    property Conditional: TsgcHTTPThreeCommasOrderConditional read GetConditional
      write FConditional;
    property _Type: TsgcHTTPThreeCommasOrderSide read F_Type write F_Type;
    property Units: Extended read FUnits write FUnits;
    property Price: TsgcHTTPThreeCommasOrderPrice read GetPrice write FPrice;
    property OrderType: TsgcHTTPThreeCommasOrderType read FOrderType
      write FOrderType;
  end;

  TsgcHTTPThreeCommasOrderLeverage = class
  private
    FEnabled: Boolean;
    FValue: Integer;
    F_Type: TsgcHTTPThreeCommasLeverageType;
  public
    property Enabled: Boolean read FEnabled write FEnabled;
    property _Type: TsgcHTTPThreeCommasLeverageType read F_Type write F_Type;
    property Value: Integer read FValue write FValue;
  end;

  TsgcHTTPThreeCommasOrder = class
  private
    FAccountId: Integer;
    FInstant: Boolean;
    FLeverage: TsgcHTTPThreeCommasOrderLeverage;
    FPair: string;
    FPosition: TsgcHTTPThreeCommasOrderPosition;
    function GetLeverage: TsgcHTTPThreeCommasOrderLeverage;
    function GetPosition: TsgcHTTPThreeCommasOrderPosition;
  protected
    function GetJSON: string; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    property AccountId: Integer read FAccountId write FAccountId;
    property Pair: string read FPair write FPair;
    property Instant: Boolean read FInstant write FInstant;
    property Leverage: TsgcHTTPThreeCommasOrderLeverage read GetLeverage
      write FLeverage;
    property Position: TsgcHTTPThreeCommasOrderPosition read GetPosition
      write FPosition;
  end;

  TsgcHTTPThreeCommasLog_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FFileName: String;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property FileName: String read FFileName write FFileName;
  end;

  TsgcHTTPThreeCommas_Options = class(TPersistent)
  private
    FApiKey: String;
    FApiSecret: string;
    FLogOptions: TsgcHTTPThreeCommasLog_Options;
    FSubAccount: string;
    procedure SetLogOptions(const Value: TsgcHTTPThreeCommasLog_Options);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property ApiKey: String read FApiKey write FApiKey;
    property ApiSecret: string read FApiSecret write FApiSecret;
    property LogOptions: TsgcHTTPThreeCommasLog_Options read FLogOptions
      write SetLogOptions;
    property SubAccount: string read FSubAccount write FSubAccount;
  end;

  TsgcHTTP_API_3Commas = class(TsgcHTTPAPI_Client)

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
    procedure GetAuthHeaders(const aRequestPath, aBody: String;
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
    FThreeCommas: TsgcHTTPThreeCommas_Options;
    procedure SetThreeCommas(const Value: TsgcHTTPThreeCommas_Options);
  public
    property ThreeCommas: TsgcHTTPThreeCommas_Options read FThreeCommas write
        SetThreeCommas;
    { properties }

  end;

  TsgcHTTP_API_3Commas_Rest = class(TsgcHTTP_API_3Commas)
    { test connectivity }
  public
    function GetPing: string;
    function GetServerTime: string;
    { test connectivity }

    { account }
  public
    function GetAccounts: string;
    function GetMarketList: string;
    function GetMarketPairs(const aMarketCode: string = ''): string;
    function GetCurrencyRatesWithLeverageData(const aMarketCode: string;
      const aPair: string): string;
    function GetCurrencyRates(const aMarketCode: string;
      const aPair: string): string;
    function GetBalances(aAccountId: Integer): string;
    function GetAccountTableData(aAccountId: Integer): string;
    function GetAccountLeverage(aAccountId: Integer;
      const aPair: string): string;
    function GetAccountInfo(aAccountId: Integer): string;
    { account }

    { smart trades }
  public
    function GetSmartTradeHistory: string;
    function CreateSmartTrade(const aOrder: TsgcHTTPThreeCommasOrder): string;
    function PlaceMarketOrder(aAccountId: Integer; aOrderSide:
        TsgcHTTPThreeCommasOrderSide; const aPair: string; aQuantity: Extended): string;
    function PlaceLimitOrder(aAccountId: Integer; aOrderSide:
        TsgcHTTPThreeCommasOrderSide; const aPair: string; aQuantity, aPrice:
        Extended): string;
    function GetSmartTrade(const aId: Integer): string;
    function CancelSmartTrade(const aId: Integer): string;
    function CloseByMarketSmartTrade(const aId: Integer): string;
    { smart trades }
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
  CS_3Commas_ENDPOINT_BASE = 'https://api.3commas.io/public/api';
  CS_3Commas_DELIMITER = '&';

const
  CS_3Commas_ENDPOINT_PING = '/ver1/ping';
  CS_3Commas_ENDPOINT_TIME = '/ver1/time';

const
  CS_3Commas_ENDPOINT_ACCOUNTS = '/ver1/accounts';
  CS_3Commas_ENDPOINT_ACCOUNTS_MARKET_LIST = '/ver1/accounts/market_list';
  CS_3Commas_ENDPOINT_ACCOUNTS_MARKET_PAIRS = '/ver1/accounts/market_pairs';
  CS_3Commas_ENDPOINT_ACCOUNTS_CURRENCY_RATES_WITH_LEVERAGE_DATA =
    '/ver1/accounts/currency_rates_with_leverage_data';
  CS_3Commas_ENDPOINT_ACCOUNTS_CURRENCY_RATES = '/ver1/accounts/currency_rates';
  CS_3Commas_ENDPOINT_ACCOUNTS_LOAD_BALANCES =
    '/ver1/accounts/%d/load_balances';
  CS_3Commas_ENDPOINT_ACCOUNTS_ACCOUNT_TABLE_DATA =
    '/ver1/accounts/%d/account_table_data';
  CS_3Commas_ENDPOINT_ACCOUNTS_ACCOUNT_LEVERAGE =
    '/ver1/accounts/%d/account_leverage';
  CS_3Commas_ENDPOINT_ACCOUNTS_ACCOUNT_INFO = '/ver1/accounts/%d';

const
  CS_3Commas_ENDPOINT_SMART_TRADES = '/v2/smart_trades';
  CS_3Commas_ENDPOINT_SMART_TRADES_BY_ID = '/v2/smart_trades/%d';
  CS_3Commas_ENDPOINT_SMART_TRADES_CLOSE_BY_MARKET = '/v2/smart_trades/%d/close_by_market';

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

function GetOrderSide(aValue: TsgcHTTPThreeCommasOrderSide): string;
begin
  case aValue of
    os3cBuy:
      Result := 'buy';
    os3cSell:
      Result := 'sell';
  else
    Result := '';
  end;
end;

function GetOrderType(aValue: TsgcHTTPThreeCommasOrderType): string;
begin
  case aValue of
    ot3cMarket:
      Result := 'market';
    ot3cLimit:
      Result := 'limit';
    ot3cConditional:
      Result := 'conditional';
  else
    Result := '';
  end;
end;

function GetOrderLeverage(aValue: TsgcHTTPThreeCommasLeverageType): string;
begin
  case aValue of
    lt3cCustom:
      Result := 'custom';
    lt3cCross:
      Result := 'cross';
  else
    Result := '';
  end;
end;

function GetOrderPriceType(aValue: TsgcHTTPThreeCommasPriceType): string;
begin
  case aValue of
    pt3cBid:
      Result := 'bid';
    pt3cAsk:
      Result := 'ask';
    pt3cLast:
      Result := 'last';
  else
    Result := '';
  end;
end;

procedure TsgcHTTPThreeCommasLog_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTPThreeCommasLog_Options then
  begin
    Enabled := TsgcHTTPThreeCommasLog_Options(aSource).Enabled;
    FileName := TsgcHTTPThreeCommasLog_Options(aSource).FileName;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcHTTPThreeCommas_Options.Create;
begin
  inherited;
  FLogOptions := TsgcHTTPThreeCommasLog_Options.Create;
end;

destructor TsgcHTTPThreeCommas_Options.Destroy;
begin
  sgcFree(FLogOptions);
  inherited;
end;

procedure TsgcHTTPThreeCommas_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTPThreeCommas_Options then
  begin
    ApiKey := TsgcHTTPThreeCommas_Options(aSource).ApiKey;
    ApiSecret := TsgcHTTPThreeCommas_Options(aSource).ApiSecret;
    SubAccount := TsgcHTTPThreeCommas_Options(aSource).SubAccount;
    LogOptions := TsgcHTTPThreeCommas_Options(aSource).LogOptions;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcHTTPThreeCommas_Options.SetLogOptions(const Value
  : TsgcHTTPThreeCommasLog_Options);
begin
  if Assigned(FLogOptions) then
    FLogOptions.Assign(Value);
end;

constructor TsgcHTTP_API_3Commas.Create(aOwner: TComponent);
begin
  inherited;
  FThreeCommas := TsgcHTTPThreeCommas_Options.Create;
end;

destructor TsgcHTTP_API_3Commas.Destroy;
begin
  sgcFree(FThreeCommas);
  inherited;
end;

function TsgcHTTP_API_3Commas_Rest.CreateSmartTrade(const aOrder:
    TsgcHTTPThreeCommasOrder): string;
begin
  Result := DoHTTP_POST_PRIVATE(CS_3Commas_ENDPOINT_SMART_TRADES, aOrder.GetJSON);
end;

function TsgcHTTP_API_3Commas.DoHTTP_GET(const aMethod: String;
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

function TsgcHTTP_API_3Commas.DoHTTP_POST(const aMethod: String;
  const aBody: String = ''; const aHeaders: TStrings = nil): string;
begin
  DoInitializeLog;

  Result := Post(GetBaseURL + aMethod, aBody, aHeaders);
end;

function TsgcHTTP_API_3Commas.DoHTTP_GET_PRIVATE(const aRequestPath
  : String): string;
var
  oHeaders: TStringList;
begin
  oHeaders := TStringList.Create;
  Try
    oHeaders.Add('content-type: application/json');
    GetAuthHeaders(aRequestPath, '', oHeaders);
    Result := DoHTTP_GET(aRequestPath, '', oHeaders);
  Finally
    sgcFree(oHeaders);
  End;
end;

procedure TsgcHTTP_API_3Commas.DoInitializeLog;
begin
  if Log <> ThreeCommas.LogOptions.Enabled then
  begin
    LogFileName := ThreeCommas.LogOptions.FileName;
    Log := ThreeCommas.LogOptions.Enabled;
  end;
end;

procedure TsgcHTTP_API_3Commas.GetAuthHeaders(const aRequestPath, aBody: String;
  var Headers: TStringList);
var
  oSecret: TsgcStringStream;
  vValue, vSignature: string;
begin
  // get signature
  oSecret := TsgcStringStream.Create(ThreeCommas.ApiSecret);
  Try
    vValue := '/public/api' + aRequestPath + aBody;
    vSignature := GetHMACSHA256(vValue, TIdBytes(oSecret.Bytes), False);
  Finally
    sgcFree(oSecret);
  end;

  // set signature headers
  Headers.Add('APIKEY: ' + ThreeCommas.ApiKey);
  Headers.Add('Signature: ' + vSignature);
end;

function TsgcHTTP_API_3Commas.GetBaseURL: string;
begin
  Result := CS_3Commas_ENDPOINT_BASE;
end;

function TsgcHTTP_API_3Commas.DoHTTP_DELETE(const aMethod: String;
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

function TsgcHTTP_API_3Commas.DoHTTP_DELETE_PRIVATE(const aRequestPath: String;
  aBody: string = ''): string;
var
  oHeaders: TStringList;
begin
  oHeaders := TStringList.Create;
  Try
    oHeaders.Add('content-type: application/json');
    GetAuthHeaders(aRequestPath, aBody, oHeaders);
    Result := DoHTTP_DELETE(aRequestPath, '', oHeaders, aBody);
  Finally
    sgcFree(oHeaders);
  End;
end;

function TsgcHTTP_API_3Commas.DoHTTP_POST_PRIVATE(const aRequestPath,
  aBody: String): string;
var
  oHeaders: TStringList;
begin
  oHeaders := TStringList.Create;
  Try
    oHeaders.Add('content-type: application/json');
    GetAuthHeaders(aRequestPath, aBody, oHeaders);
    Result := DoHTTP_POST(aRequestPath, aBody, oHeaders);
  Finally
    sgcFree(oHeaders);
  End;
end;

function TsgcHTTP_API_3Commas_Rest.CancelSmartTrade(const aId: Integer): string;
begin
  Result := DoHTTP_DELETE_PRIVATE(Format(CS_3Commas_ENDPOINT_SMART_TRADES_BY_ID, [aId]));
end;

function TsgcHTTP_API_3Commas_Rest.CloseByMarketSmartTrade(const aId: Integer):
    string;
begin
  Result := DoHTTP_POST_PRIVATE(Format(CS_3Commas_ENDPOINT_SMART_TRADES_CLOSE_BY_MARKET, [aId]), '');
end;

function TsgcHTTP_API_3Commas_Rest.GetAccountInfo(aAccountId: Integer): string;
begin
  Result := DoHTTP_GET_PRIVATE(Format(CS_3Commas_ENDPOINT_ACCOUNTS_ACCOUNT_INFO,
    [aAccountId]));
end;

function TsgcHTTP_API_3Commas_Rest.GetAccountLeverage(aAccountId: Integer;
  const aPair: string): string;
begin
  Result := DoHTTP_GET_PRIVATE
    (Format(CS_3Commas_ENDPOINT_ACCOUNTS_ACCOUNT_LEVERAGE + '?pair=%s',
    [aAccountId, aPair]));
end;

function TsgcHTTP_API_3Commas_Rest.GetAccounts: string;
begin
  Result := DoHTTP_GET_PRIVATE(CS_3Commas_ENDPOINT_ACCOUNTS);
end;

function TsgcHTTP_API_3Commas_Rest.GetAccountTableData(aAccountId: Integer): string;
begin
  Result := DoHTTP_POST_PRIVATE
    (Format(CS_3Commas_ENDPOINT_ACCOUNTS_ACCOUNT_TABLE_DATA, [aAccountId]), '');
end;

function TsgcHTTP_API_3Commas_Rest.GetBalances(aAccountId: Integer): string;
begin
  Result := DoHTTP_POST_PRIVATE
    (Format(CS_3Commas_ENDPOINT_ACCOUNTS_LOAD_BALANCES, [aAccountId]), '');
end;

function TsgcHTTP_API_3Commas_Rest.GetCurrencyRates(const aMarketCode: string;
  const aPair: string): string;
var
  oParameters: TStringList;
  vRequestPath: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_3Commas_DELIMITER;
    oParameters.Add('market_code=' + aMarketCode);
    oParameters.Add('pair=' + aPair);
    vRequestPath := CS_3Commas_ENDPOINT_ACCOUNTS_CURRENCY_RATES;
    if oParameters.Count > 0 then
      vRequestPath := vRequestPath + '?' + oParameters.DelimitedText;
    Result := DoHTTP_GET(vRequestPath);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_3Commas_Rest.GetCurrencyRatesWithLeverageData(const aMarketCode
  : string; const aPair: string): string;
var
  oParameters: TStringList;
  vRequestPath: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_3Commas_DELIMITER;
    oParameters.Add('market_code=' + aMarketCode);
    oParameters.Add('pair=' + aPair);
    vRequestPath :=
      CS_3Commas_ENDPOINT_ACCOUNTS_CURRENCY_RATES_WITH_LEVERAGE_DATA;
    if oParameters.Count > 0 then
      vRequestPath := vRequestPath + '?' + oParameters.DelimitedText;
    Result := DoHTTP_GET(vRequestPath);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_3Commas_Rest.GetMarketList: string;
begin
  Result := DoHTTP_GET(CS_3Commas_ENDPOINT_ACCOUNTS_MARKET_LIST);
end;

function TsgcHTTP_API_3Commas_Rest.GetMarketPairs(const aMarketCode
  : string = ''): string;
var
  vRequestPath: string;
begin
  vRequestPath := CS_3Commas_ENDPOINT_ACCOUNTS_MARKET_PAIRS;
  if aMarketCode <> '' then
    vRequestPath := vRequestPath + '?market_code=' + aMarketCode;
  Result := DoHTTP_GET(vRequestPath);
end;

function TsgcHTTP_API_3Commas_Rest.GetPing: string;
begin
  Result := DoHTTP_GET(CS_3Commas_ENDPOINT_PING);
end;

function TsgcHTTP_API_3Commas_Rest.GetServerTime: string;
begin
  Result := DoHTTP_GET(CS_3Commas_ENDPOINT_TIME);
end;

function TsgcHTTP_API_3Commas_Rest.GetSmartTrade(const aId: Integer): string;
begin
  Result := DoHTTP_GET_PRIVATE(Format(CS_3Commas_ENDPOINT_SMART_TRADES_BY_ID, [aId]));
end;

function TsgcHTTP_API_3Commas_Rest.GetSmartTradeHistory: string;
begin
  Result := DoHTTP_GET_PRIVATE(CS_3Commas_ENDPOINT_SMART_TRADES);
end;

function TsgcHTTP_API_3Commas_Rest.PlaceLimitOrder(aAccountId: Integer;
    aOrderSide: TsgcHTTPThreeCommasOrderSide; const aPair: string; aQuantity,
    aPrice: Extended): string;
var
  oOrder: TsgcHTTPThreeCommasOrder;
begin
  oOrder := TsgcHTTPThreeCommasOrder.Create;
  Try
    oOrder.AccountId := aAccountId;
    oOrder.Pair := aPair;
    oOrder.Instant := True;
    oOrder.Position._Type := aOrderSide;
    oOrder.Position.Units := aQuantity;
    oOrder.Position.Price.Value := aPrice;
    oOrder.Position.OrderType := ot3cLimit;

    Result := CreateSmartTrade(oOrder);
  Finally
    sgcFree(oOrder);
  End;
end;

function TsgcHTTP_API_3Commas_Rest.PlaceMarketOrder(aAccountId: Integer;
    aOrderSide: TsgcHTTPThreeCommasOrderSide; const aPair: string; aQuantity:
    Extended): string;
var
  oOrder: TsgcHTTPThreeCommasOrder;
begin
  oOrder := TsgcHTTPThreeCommasOrder.Create;
  Try
    oOrder.AccountId := aAccountId;
    oOrder.Pair := aPair;
    oOrder.Instant := True;
    oOrder.Position._Type := aOrderSide;
    oOrder.Position.Units := aQuantity;
    oOrder.Position.OrderType := ot3cMarket;

    Result := CreateSmartTrade(oOrder);
  Finally
    sgcFree(oOrder);
  End;
end;

procedure TsgcHTTP_API_3Commas.SetThreeCommas(const Value:
    TsgcHTTPThreeCommas_Options);
begin
  if Assigned(FThreeCommas) then
    FThreeCommas.Assign(Value);
end;

constructor TsgcHTTPThreeCommasOrder.Create;
begin
  inherited;
end;

destructor TsgcHTTPThreeCommasOrder.Destroy;
begin
  sgcFree(FLeverage);
  sgcFree(FPosition);
  inherited;
end;

function TsgcHTTPThreeCommasOrder.GetJSON: string;
var
  oJSON: TsgcJSON;
  oObject, oObject2, oObject3: IsgcObjectJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    // ... common fields mandatories
    oJSON.AddPair('account_id', AccountId);
    oJSON.AddPair('pair', Pair);
    oJSON.AddPair('instant', GetFormatBoolean(Instant));
    // ... leverage
    if Leverage.Enabled then
    begin
      oObject := oJSON.AddObject('leverage');
      oObject.JSONObject.AddPair('enabled', GetFormatBoolean(Leverage.Enabled));
      oObject.JSONObject.AddPair('type', GetOrderLeverage(Leverage._Type));
      oObject.JSONObject.AddPair('value', Leverage.Value);
    end;
    // ... position
    oObject := oJSON.AddObject('position');
    oObject.JSONObject.AddPair('type', GetOrderSide(Position._Type));
    oObject2 := oObject.JSONObject.AddObject('units');
    oObject2.JSONObject.AddPair('value', GetFormatExtended(Position.Units));
    if Position.Price.Value > 0 then
    begin
      oObject2 := oObject.JSONObject.AddObject('price');
      oObject2.JSONObject.AddPair('value',
        GetFormatExtended(Position.Price.Value));
    end;
    oObject.JSONObject.AddPair('order_type', GetOrderType(Position.OrderType));
    // ... conditional
    if Position.OrderType = ot3cConditional then
    begin
      oObject2 := oObject.JSONObject.AddObject('conditional');
      // price
      oObject3 := oObject2.JSONObject.AddObject('price');
      oObject3.JSONObject.AddPair('value', Position.Conditional.Price.Value);
      oObject3.JSONObject.AddPair('type',
        GetOrderPriceType(Position.Conditional.Price._Type));
      // order_type
      oObject2.JSONObject.AddPair('order_type',
        GetOrderType(Position.Conditional.OrderType));
      // trailing
      if Position.Conditional.Trailing.Enabled then
      begin
        oObject3 := oObject2.JSONObject.AddObject('trailing');
        oObject3.JSONObject.AddPair('enabled',
          GetFormatBoolean(Position.Conditional.Trailing.Enabled));
        oObject3.JSONObject.AddPair('percent',
          GetFormatExtended(Position.Conditional.Trailing.Percent));
      end;
    end;

    result := oJSON.Text;
  Finally
    sgcFree(oJSON);
  end;
end;

function TsgcHTTPThreeCommasOrder.GetLeverage: TsgcHTTPThreeCommasOrderLeverage;
begin
  if not Assigned(FLeverage) then
    FLeverage := TsgcHTTPThreeCommasOrderLeverage.Create;
  Result := FLeverage;
end;

function TsgcHTTPThreeCommasOrder.GetPosition: TsgcHTTPThreeCommasOrderPosition;
begin
  if not Assigned(FPosition) then
    FPosition := TsgcHTTPThreeCommasOrderPosition.Create;
  Result := FPosition;
end;

destructor TsgcHTTPThreeCommasOrderPosition.Destroy;
begin
  sgcFree(FConditional);
  sgcFree(FPrice);
  inherited;
end;

function TsgcHTTPThreeCommasOrderPosition.GetConditional
  : TsgcHTTPThreeCommasOrderConditional;
begin
  if not Assigned(FConditional) then
    FConditional := TsgcHTTPThreeCommasOrderConditional.Create;
  Result := FConditional;
end;

function TsgcHTTPThreeCommasOrderPosition.GetPrice: TsgcHTTPThreeCommasOrderPrice;
begin
  if not Assigned(FPrice) then
    FPrice := TsgcHTTPThreeCommasOrderPrice.Create;
  Result := FPrice;
end;

destructor TsgcHTTPThreeCommasOrderConditional.Destroy;
begin
  sgcFree(FPrice);
  sgcFree(FTrailing);
  inherited;
end;

function TsgcHTTPThreeCommasOrderConditional.GetPrice: TsgcHTTPThreeCommasOrderPrice;
begin
  if not Assigned(FPrice) then
    FPrice := TsgcHTTPThreeCommasOrderPrice.Create;
  Result := FPrice;
end;

function TsgcHTTPThreeCommasOrderConditional.GetTrailing
  : TsgcHTTPThreeCommasOrderTrailing;
begin
  if not Assigned(FTrailing) then
    FTrailing := TsgcHTTPThreeCommasOrderTrailing.Create;
  Result := FTrailing;
end;

{$ENDIF}

end.
