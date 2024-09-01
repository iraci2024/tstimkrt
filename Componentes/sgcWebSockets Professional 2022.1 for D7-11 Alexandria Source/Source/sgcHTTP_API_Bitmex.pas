{ ***************************************************************************
  sgcHTTP API component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcHTTP_API_Bitmex;

interface

{$I sgcVer.inc}
{$IFDEF SGC_HTTP}

uses
  Classes, SysUtils,
  // sgc
  sgcHTTP_API;

type

  TsgcHTTPBitmexOrderSide = (bmosNone, bmosBuy, bmosSell);
  TsgcHTTPBitmexOrderType = (bmotNone, bmotMarket, bmotLimit, bmotStop,
    bmotStopLimit, bmotMarketIfTouched, bmotLimitIfTouched, bmotPegged);
  TsgcHTTPBitmexPegPriceType = (bmpptNone, bmpptMarketPeg, bmpptPrimaryPeg,
    bmpptTrailingStopPeg);
  TsgcHTTPBitmexTimeInForce = (bmtifNone, bmtifDay, bmtifGoodTillCancel,
    bmtifImmediateOrCancel, bmtifFillOrKill);
  TsgcHTTPBitmexExecInst = (bmeiNone, bmeiParticipateDoNotInitiate,
    bmeiAllOrNone, bmeiMarkPrice, bmeiIndexPrice, bmeiLastPrice, bmeiClose,
    bmeiReduceOnly, bmeiFixed, bmeiLastWithinMark);

  TsgcHTTPBitmexOrder = class
  private
    FClOrdId: string;
    FDisplayQty: Extended;
    FTimeInForce: TsgcHTTPBitmexTimeInForce;
    FSymbol: string;
    FExecInst: TsgcHTTPBitmexExecInst;
    FPrice: Extended;
    FText: string;
    FSide: TsgcHTTPBitmexOrderSide;
    FOrderQty: Extended;
    FPegOffsetValue: Extended;
    FPegPriceType: TsgcHTTPBitmexPegPriceType;
    FStopPx: Extended;
    FOrdType: TsgcHTTPBitmexOrderType;
  protected
    function GetJSON: string; virtual;
  public
    constructor Create; virtual;
  public
    property Symbol: string read FSymbol write FSymbol;
    property Side: TsgcHTTPBitmexOrderSide read FSide write FSide;
    property OrderQty: Extended read FOrderQty write FOrderQty;
    property Price: Extended read FPrice write FPrice;
    property DisplayQty: Extended read FDisplayQty write FDisplayQty;
    property StopPx: Extended read FStopPx write FStopPx;
    property ClOrdId: string read FClOrdId write FClOrdId;
    property PegOffsetValue: Extended read FPegOffsetValue
      write FPegOffsetValue;
    property PegPriceType: TsgcHTTPBitmexPegPriceType read FPegPriceType
      write FPegPriceType;
    property OrdType: TsgcHTTPBitmexOrderType read FOrdType write FOrdType;
    property TimeInForce: TsgcHTTPBitmexTimeInForce read FTimeInForce
      write FTimeInForce;
    property ExecInst: TsgcHTTPBitmexExecInst read FExecInst write FExecInst;
    property Text: string read FText write FText;
  end;

  TsgcHTTPBitmexAmendOrder = class
  private
    FClOrdId: string;
    FLeavesQty: Extended;
    FOrderId: string;
    FPrice: Extended;
    FText: string;
    FOrigClOrdID: string;
    FOrderQty: Extended;
    FPegOffsetValue: Extended;
    FStopPx: Extended;
  protected
    function GetJSON: string; virtual;
  public
    constructor Create; virtual;
  public
    property OrderId: string read FOrderId write FOrderId;
    property OrigClOrdID: string read FOrigClOrdID write FOrigClOrdID;
    property ClOrdId: string read FClOrdId write FClOrdId;
    property OrderQty: Extended read FOrderQty write FOrderQty;
    property LeavesQty: Extended read FLeavesQty write FLeavesQty;
    property Price: Extended read FPrice write FPrice;
    property StopPx: Extended read FStopPx write FStopPx;
    property PegOffsetValue: Extended read FPegOffsetValue
      write FPegOffsetValue;
    property Text: string read FText write FText;
  end;

  TsgcHTTPBitmexLog_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FFileName: String;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property FileName: String read FFileName write FFileName;
  end;

  TsgcHTTPBitmex_Options = class(TPersistent)
  private
    FApiKey: String;
    FApiSecret: string;
    FExpires: Integer;
    FLogOptions: TsgcHTTPBitmexLog_Options;
    FTestNet: Boolean;
    procedure SetLogOptions(const Value: TsgcHTTPBitmexLog_Options);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property ApiKey: String read FApiKey write FApiKey;
    property ApiSecret: string read FApiSecret write FApiSecret;
    property Expires: Integer read FExpires write FExpires;
    property LogOptions: TsgcHTTPBitmexLog_Options read FLogOptions
      write SetLogOptions;
    property TestNet: Boolean read FTestNet write FTestNet;
  end;

  TsgcHTTP_API_Bitmex = class(TsgcHTTPAPI_Client)
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
    function DoHTTP_PUT(const aMethod: String; const aBody: String = ''; const
        aHeaders: TStrings = nil): string; virtual;
    function DoHTTP_DELETE(const aMethod: String;
      const aParameters: String = ''; const aHeaders: TStrings = nil;
      const aBody: string = ''): string; virtual;
  protected
    procedure GetAuthHeaders(const aMethod, aRequestPath, aBody: String;
      var Headers: TStringList); virtual;
  protected
    function DoHTTP_GET_PRIVATE(const aRequestPath: String): string; virtual;
    function DoHTTP_POST_PRIVATE(const aRequestPath: String;
      const aBody: String = ''): string; virtual;
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
    FBitmexOptions: TsgcHTTPBitmex_Options;
    procedure SetBitmexOptions(const Value: TsgcHTTPBitmex_Options);
  public
    property BitmexOptions: TsgcHTTPBitmex_Options read FBitmexOptions
      write SetBitmexOptions;
    { properties }
  end;

  TsgcHTTP_API_Bitmex_Rest = class(TsgcHTTP_API_Bitmex)
  private
    function DoGetData(const aRequestPath: string; const aSymbol: string = '';
      const aFilter: string = ''; const aColumns: string = '';
      aCount: Integer = 100; aStart: Int64 = 0; aReverse: Boolean = False;
      aStartTime: Int64 = 0; aEndTime: Int64 = 0): string;
    // ... execution
  public
    function GetExecutions(const aSymbol: string = '';
      const aFilter: string = ''; const aColumns: string = '';
      aCount: Integer = 100; aStart: Int64 = 0; aReverse: Boolean = False;
      aStartTime: Int64 = 0; aEndTime: Int64 = 0): string;
    function GetExecutionsTradeHistory(const aSymbol: string = '';
      const aFilter: string = ''; const aColumns: string = '';
      aCount: Integer = 100; aStart: Int64 = 0; aReverse: Boolean = False;
      aStartTime: Int64 = 0; aEndTime: Int64 = 0): string;
    // ... instrument
  public
    function GetInstruments(const aSymbol: string = '';
      const aFilter: string = ''; const aColumns: string = '';
      aCount: Integer = 100; aStart: Int64 = 0; aReverse: Boolean = False;
      aStartTime: Int64 = 0; aEndTime: Int64 = 0): string;
    // ... order
  public
    function GetOrders(const aSymbol: string; const aFilter: string = '';
      const aColumns: string = ''; aCount: Integer = 100; aStart: Int64 = 0;
      aReverse: Boolean = False; aStartTime: Int64 = 0;
      aEndTime: Int64 = 0): string;
    function PlaceOrder(const aOrder: TsgcHTTPBitmexOrder): string;
    function PlaceMarketOrder(aSide: TsgcHTTPBitmexOrderSide; const aSymbol:
        string; aQuantity: Extended): string;
    function PlaceLimitOrder(aSide: TsgcHTTPBitmexOrderSide; const aSymbol: string;
        aQuantity, aPrice: Extended): string;
    function PlaceStopOrder(aSide: TsgcHTTPBitmexOrderSide; const aSymbol: string;
        aQuantity, aStopPrice: Extended): string;
    function PlaceStopLimitOrder(aSide: TsgcHTTPBitmexOrderSide; const aSymbol:
        string; aQuantity, aStopPrice, aLimitPrice: Extended): string;
    function AmendOrder(const aOrder: TsgcHTTPBitmexAmendOrder): string;
    function CancelOrder(const aOrderId, aClOrderId: string; const aText: string =
        ''): string;
    function CancelAllOrders(const aSymbol: string = ''; const aFilter: string =
        ''; const aText: string = ''): string;
    function CancelAllOrdersAfter(aTimeout: Integer): string;
    function ClosePosition(const aSymbol: string; aPrice: Extended = 0): string;
    // ... order book
  public
    function GetOrderBook(const aSymbol: string; aDepth: Integer = 25): string;
    // ... position
  public
    function GetPosition(const aColumns: string = '';
      aCount: Integer = 100): string;
    function SetPositionIsolate(const aSymbol: string;
      aEnabled: Boolean): string;
    function SetPositionLeverage(const aSymbol: string;
      aLeverage: Double): string;
    function SetPositionRiskLimit(const aSymbol: string;
      aRiskLimit: Double): string;
    function SetPositionTransferMargin(const aSymbol: string;
      aAmount: Extended): string;
    // ... quote
  public
    function GetQuotes(const aSymbol: string; const aFilter: string = '';
      const aColumns: string = ''; aCount: Integer = 100; aStart: Int64 = 0;
      aReverse: Boolean = False; aStartTime: Int64 = 0;
      aEndTime: Int64 = 0): string;
    // ... trade
  public
    function GetTrades(const aSymbol: string; const aFilter: string = '';
      const aColumns: string = ''; aCount: Integer = 100; aStart: Int64 = 0;
      aReverse: Boolean = False; aStartTime: Int64 = 0;
      aEndTime: Int64 = 0): string;
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
  CS_BITMEX_ENDPOINT_BASE = 'https://www.bitmex.com';
  CS_BITMEX_ENDPOINT_TESTNET_BASE = 'https://testnet.bitmex.com';
  CS_BITMEX_DELIMITER = '&';

const
  CS_BITMEX_ENDPOINT_EXECUTION = '/api/v1/execution';
  CS_BITMEX_ENDPOINT_EXECUTION_TRADE_HISTORY = '/api/v1/execution/tradeHistory';

const
  CS_BITMEX_ENDPOINT_INSTRUMENT = '/api/v1/instrument';

const
  CS_BITMEX_ENDPOINT_ORDER = '/api/v1/order';
  CS_BITMEX_ENDPOINT_ORDER_ALL = '/api/v1/order/all';
  CS_BITMEX_ENDPOINT_ORDER_CANCELALLAFTER = '/api/v1/order/cancelAllAfter';
  CS_BITMEX_ENDPOINT_ORDER_CLOSEPOSITION = '/api/v1/order/closePosition';

const
  CS_BITMEX_ENDPOINT_ORDER_BOOK = '/api/v1/orderBook/L2';

const
  CS_BITMEX_ENDPOINT_POSITION = '/api/v1/position';
  CS_BITMEX_ENDPOINT_POSITION_ISOLATE = '/api/v1/position/isolate';
  CS_BITMEX_ENDPOINT_POSITION_LEVERAGE = '/api/v1/position/leverage';
  CS_BITMEX_ENDPOINT_POSITION_RISKLIMIT = '/api/v1/position/riskLimit';
  CS_BITMEX_ENDPOINT_POSITION_TRANSFERMARGIN =
    '/api/v1/position/transferMargin';

const
  CS_BITMEX_ENDPOINT_QUOTE = '/api/v1/quote';

const
  CS_BITMEX_ENDPOINT_TRADE = '/api/v1/trade';

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
  Result := FormatFloat('0.000000', aValue, vFS);
end;

function GetFormatDouble(aValue: Double): string;
var
  vFS: TFormatSettings;
begin
  vFS.DecimalSeparator := '.';
  vFS.ThousandSeparator := ',';
  Result := FormatFloat('0.00', aValue, vFS);
end;

function GetOrderSide(aValue: TsgcHTTPBitmexOrderSide): string;
begin
  case aValue of
    bmosBuy:
      Result := 'Buy';
    bmosSell:
      Result := 'Sell';
  else
    Result := '';
  end;
end;

function GetOrderPegPriceType(aValue: TsgcHTTPBitmexPegPriceType): string;
begin
  case aValue of
    bmpptMarketPeg:
      Result := 'MarketPeg';
    bmpptPrimaryPeg:
      Result := 'PrimaryPeg';
    bmpptTrailingStopPeg:
      Result := 'TrailingStopPeg';
  else
    Result := '';
  end;
end;

function GetOrderType(aValue: TsgcHTTPBitmexOrderType): string;
begin
  case aValue of
    bmotMarket:
      Result := 'Market';
    bmotLimit:
      Result := 'Limit';
    bmotStop:
      Result := 'Stop';
    bmotStopLimit:
      Result := 'StopLimit';
    bmotMarketIfTouched:
      Result := 'MarketIfTouched';
    bmotLimitIfTouched:
      Result := 'LimitIfTouched';
    bmotPegged:
      Result := 'Pegged';
  else
    Result := '';
  end;
end;

function GetOrderTimeInForce(aValue: TsgcHTTPBitmexTimeInForce): string;
begin
  case aValue of
    bmtifDay:
      Result := 'Day';
    bmtifGoodTillCancel:
      Result := 'GoodTillCancel';
    bmtifImmediateOrCancel:
      Result := 'ImmediateOrCancel';
    bmtifFillOrKill:
      Result := 'FillOrKill';
  else
    Result := '';
  end;
end;

function GetOrderExecInst(aValue: TsgcHTTPBitmexExecInst): string;
begin
  case aValue of
    bmeiParticipateDoNotInitiate:
      Result := 'ParticipateDoNotInitiate';
    bmeiAllOrNone:
      Result := 'AllOrNone';
    bmeiMarkPrice:
      Result := 'MarkPrice';
    bmeiIndexPrice:
      Result := 'IndexPrice';
    bmeiLastPrice:
      Result := 'LastPrice';
    bmeiClose:
      Result := 'Close';
    bmeiReduceOnly:
      Result := 'ReduceOnly';
    bmeiFixed:
      Result := 'Fixed';
    bmeiLastWithinMark:
      Result := 'LastWithinMark';
  else
    Result := '';
  end;
end;

procedure TsgcHTTPBitmexLog_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTPBitmexLog_Options then
  begin
    Enabled := TsgcHTTPBitmexLog_Options(aSource).Enabled;
    FileName := TsgcHTTPBitmexLog_Options(aSource).FileName;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcHTTPBitmex_Options.Create;
begin
  inherited;
  FLogOptions := TsgcHTTPBitmexLog_Options.Create;
  FTestNet := False;
  FExpires := 30;
end;

destructor TsgcHTTPBitmex_Options.Destroy;
begin
  sgcFree(FLogOptions);
  inherited;
end;

procedure TsgcHTTPBitmex_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTPBitmex_Options then
  begin
    ApiKey := TsgcHTTPBitmex_Options(aSource).ApiKey;
    ApiSecret := TsgcHTTPBitmex_Options(aSource).ApiSecret;
    Expires := TsgcHTTPBitmex_Options(aSource).Expires;
    TestNet := TsgcHTTPBitmex_Options(aSource).TestNet;
    LogOptions := TsgcHTTPBitmex_Options(aSource).LogOptions;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcHTTPBitmex_Options.SetLogOptions(const Value
  : TsgcHTTPBitmexLog_Options);
begin
  if Assigned(FLogOptions) then
    FLogOptions.Assign(Value);
end;

constructor TsgcHTTP_API_Bitmex.Create(aOwner: TComponent);
begin
  inherited;
  FBitmexOptions := TsgcHTTPBitmex_Options.Create;
end;

destructor TsgcHTTP_API_Bitmex.Destroy;
begin
  sgcFree(FBitmexOptions);
  inherited;
end;

function TsgcHTTP_API_Bitmex.DoHTTP_GET(const aMethod: String;
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

function TsgcHTTP_API_Bitmex.DoHTTP_POST(const aMethod: String;
  const aBody: String = ''; const aHeaders: TStrings = nil): string;
begin
  DoInitializeLog;

  Result := Post(GetBaseURL + aMethod, aBody, aHeaders);
end;

function TsgcHTTP_API_Bitmex.DoHTTP_GET_PRIVATE(const aRequestPath
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

procedure TsgcHTTP_API_Bitmex.DoInitializeLog;
begin
  if Log <> BitmexOptions.LogOptions.Enabled then
  begin
    LogFileName := BitmexOptions.LogOptions.FileName;
    Log := BitmexOptions.LogOptions.Enabled;
  end;
end;

procedure TsgcHTTP_API_Bitmex.GetAuthHeaders(const aMethod, aRequestPath,
  aBody: String; var Headers: TStringList);
var
  vTimestamp: string;
  oSecret: TsgcStringStream;
  vSignature: string;
begin
  // get signature
  vTimestamp := GetDateTimeUnix(IncSecond(Now, BitmexOptions.Expires),
    False) + '000';
  oSecret := TsgcStringStream.Create(BitmexOptions.ApiSecret);
  Try
    vSignature := GetHMACSHA256(aMethod + aRequestPath + vTimestamp + aBody,
      TIdBytes(oSecret.Bytes), False);
  Finally
    sgcFree(oSecret);
  end;

  // set signature headers
  Headers.Add('api-expires: ' + vTimestamp);
  Headers.Add('api-key: ' + BitmexOptions.ApiKey);
  Headers.Add('api-signature: ' + vSignature);
end;

function TsgcHTTP_API_Bitmex.GetBaseURL: string;
begin
  if BitmexOptions.TestNet then
    Result := CS_BITMEX_ENDPOINT_TESTNET_BASE
  else
    Result := CS_BITMEX_ENDPOINT_BASE;
end;

function TsgcHTTP_API_Bitmex.DoHTTP_DELETE(const aMethod: String;
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

function TsgcHTTP_API_Bitmex.DoHTTP_DELETE_PRIVATE(const aRequestPath: String;
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

function TsgcHTTP_API_Bitmex.DoHTTP_POST_PRIVATE(const aRequestPath: String;
  const aBody: String = ''): string;
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

function TsgcHTTP_API_Bitmex.DoHTTP_PUT(const aMethod: String; const aBody:
    String = ''; const aHeaders: TStrings = nil): string;
begin
  DoInitializeLog;

  Result := Put(GetBaseURL + aMethod, aBody, aHeaders);
end;

procedure TsgcHTTP_API_Bitmex.SetBitmexOptions(const Value
  : TsgcHTTPBitmex_Options);
begin
  if Assigned(FBitmexOptions) then
    FBitmexOptions.Assign(Value);
end;

constructor TsgcHTTPBitmexOrder.Create;
begin
  inherited;
  FSymbol := '';
  FSide := bmosNone;
  FOrderQty := 0;
  FPrice := 0;
  FDisplayQty := 0;
  FStopPx := 0;
  FClOrdId := '';
  FPegOffsetValue := 0;
  FPegPriceType := bmpptNone;
  FOrdType := bmotNone;
  FTimeInForce := bmtifNone;
  FExecInst := bmeiNone;
  FText := '';
end;

function TsgcHTTPBitmexOrder.GetJSON: string;
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    // mandatory
    oJSON.AddPair('symbol', Symbol);
    oJSON.AddPair('side', GetOrderSide(Side));
    oJSON.AddPair('orderQty', GetFormatExtended(OrderQty));
    oJSON.AddPair('ordType', GetOrderType(OrdType));

    // optional
    case OrdType of
      bmotLimit, bmotStopLimit, bmotLimitIfTouched:
        oJSON.AddPair('price', GetFormatExtended(Price));
    end;
    if DisplayQty = 0 then
      oJSON.AddPair('displayQty', 0)
    else
      oJSON.AddPair('displayQty', GetFormatExtended(DisplayQty));
    case OrdType of
      bmotStop, bmotStopLimit, bmotMarketIfTouched:
        oJSON.AddPair('stopPx', GetFormatExtended(StopPx));
    end;
    if ClOrdId <> '' then
      oJSON.AddPair('clOrdID', ClOrdId);
    case OrdType of
      bmotStop, bmotStopLimit, bmotMarketIfTouched, bmotLimitIfTouched:
        oJSON.AddPair('pegOffsetValue', GetFormatExtended(PegOffsetValue));
    end;
    if (OrdType = bmotPegged) and (PegPriceType <> bmpptNone) then
      oJSON.AddPair('pegPriceType', GetOrderPegPriceType(PegPriceType));
    if TimeInForce = bmtifNone then
    begin
      case OrdType of
        bmotLimit, bmotStopLimit, bmotLimitIfTouched:
          oJSON.AddPair('timeInForce',
            GetOrderTimeInForce(bmtifGoodTillCancel));
      else
        oJSON.AddPair('timeInForce', GetOrderTimeInForce(bmtifDay));
      end;
    end
    else
      oJSON.AddPair('timeInForce', GetOrderTimeInForce(TimeInForce));
    case ExecInst of
      bmeiAllOrNone:
        begin
          if DisplayQty = 0 then
            oJSON.AddPair('execInst', GetOrderExecInst(ExecInst));
        end;
      bmeiMarkPrice, bmeiIndexPrice, bmeiLastPrice:
        begin
          case OrdType of
            bmotStop, bmotStopLimit, bmotMarketIfTouched, bmotLimitIfTouched:
              oJSON.AddPair('execInst', GetOrderExecInst(ExecInst));
          end;
        end;
    else
      begin
        if ExecInst <> bmeiNone then
          oJSON.AddPair('execInst', GetOrderExecInst(ExecInst));
      end;
    end;
    if Text <> '' then
      oJSON.AddPair('text', Text);

    // ... result
    Result := oJSON.Text;
  Finally
    sgcFree(oJSON);
  End;
end;

function TsgcHTTP_API_Bitmex_Rest.AmendOrder(const aOrder:
    TsgcHTTPBitmexAmendOrder): string;
begin
  Result := DoHTTP_PUT(CS_BITMEX_ENDPOINT_ORDER, aOrder.GetJSON);
end;

function TsgcHTTP_API_Bitmex_Rest.CancelAllOrders(const aSymbol: string = '';
    const aFilter: string = ''; const aText: string = ''): string;
var
  oParameters: TStringList;
  vRequestPath: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_BITMEX_DELIMITER;
    if aSymbol <> '' then
      oParameters.Add('symbol=' + aSymbol);
    if aFilter <> '' then
      oParameters.Add('filter=' + aFilter);
    if aText <> '' then
      oParameters.Add('text=' + aText);
    vRequestPath := CS_BITMEX_ENDPOINT_ORDER_ALL;
    if oParameters.Count > 0 then
      vRequestPath := vRequestPath + '?' + oParameters.DelimitedText;

    Result := DoHTTP_DELETE_PRIVATE(vRequestPath);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Bitmex_Rest.CancelOrder(const aOrderId, aClOrderId:
    string; const aText: string = ''): string;
var
  oParameters: TStringList;
  vRequestPath: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_BITMEX_DELIMITER;
    oParameters.Add('orderID=' + aOrderId);
    oParameters.Add('clOrdID=' + aClOrderId);
    if aText <> '' then
      oParameters.Add('text=' + aText);
    vRequestPath := CS_BITMEX_ENDPOINT_ORDER + '?' +
      oParameters.DelimitedText;

    Result := DoHTTP_DELETE_PRIVATE(vRequestPath);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Bitmex_Rest.GetExecutions(const aSymbol: string = '';
  const aFilter: string = ''; const aColumns: string = '';
  aCount: Integer = 100; aStart: Int64 = 0; aReverse: Boolean = False;
  aStartTime: Int64 = 0; aEndTime: Int64 = 0): string;
begin
  Result := DoGetData(CS_BITMEX_ENDPOINT_EXECUTION, aSymbol, aFilter, aColumns,
    aCount, aStart, aReverse, aStartTime, aEndTime);
end;

function TsgcHTTP_API_Bitmex_Rest.GetExecutionsTradeHistory
  (const aSymbol: string = ''; const aFilter: string = '';
  const aColumns: string = ''; aCount: Integer = 100; aStart: Int64 = 0;
  aReverse: Boolean = False; aStartTime: Int64 = 0;
  aEndTime: Int64 = 0): string;
begin
  Result := DoGetData(CS_BITMEX_ENDPOINT_EXECUTION_TRADE_HISTORY, aSymbol,
    aFilter, aColumns, aCount, aStart, aReverse, aStartTime, aEndTime);
end;

function TsgcHTTP_API_Bitmex_Rest.GetOrderBook(const aSymbol: string;
  aDepth: Integer = 25): string;
var
  oParameters: TStringList;
  vRequestPath: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_BITMEX_DELIMITER;
    oParameters.Add('symbol=' + aSymbol);
    if aDepth <> 25 then
      oParameters.Add('depth=' + IntToStr(aDepth));
    vRequestPath := CS_BITMEX_ENDPOINT_ORDER_BOOK + '?' +
      oParameters.DelimitedText;

    Result := DoHTTP_GET_PRIVATE(vRequestPath);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Bitmex_Rest.DoGetData(const aRequestPath: string;
  const aSymbol: string = ''; const aFilter: string = '';
  const aColumns: string = ''; aCount: Integer = 100; aStart: Int64 = 0;
  aReverse: Boolean = False; aStartTime: Int64 = 0;
  aEndTime: Int64 = 0): string;
var
  oParameters: TStringList;
  vRequestPath: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_BITMEX_DELIMITER;
    if aSymbol <> '' then
      oParameters.Add('symbol=' + aSymbol);
    if aFilter <> '' then
      oParameters.Add('filter=' + aFilter);
    if aColumns <> '' then
      oParameters.Add('columns=' + aColumns);
    if aCount <> 100 then
      oParameters.Add('count=' + IntToStr(aCount));
    if aStart > 0 then
      oParameters.Add('start=' + IntToStr(aStart));
    if aReverse then
      oParameters.Add('reverse=true');
    if aStartTime > 0 then
      oParameters.Add('startTime=' + IntToStr(aStartTime));
    if aEndTime > 0 then
      oParameters.Add('endTime=' + IntToStr(aEndTime));
    vRequestPath := aRequestPath;
    if oParameters.Count > 0 then
      vRequestPath := vRequestPath + '?' + oParameters.DelimitedText;

    Result := DoHTTP_GET_PRIVATE(vRequestPath);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Bitmex_Rest.GetQuotes(const aSymbol: string;
  const aFilter: string = ''; const aColumns: string = '';
  aCount: Integer = 100; aStart: Int64 = 0; aReverse: Boolean = False;
  aStartTime: Int64 = 0; aEndTime: Int64 = 0): string;
begin
  Result := DoGetData(CS_BITMEX_ENDPOINT_QUOTE, aSymbol, aFilter, aColumns,
    aCount, aStart, aReverse, aStartTime, aEndTime);
end;

function TsgcHTTP_API_Bitmex_Rest.GetInstruments(const aSymbol: string = '';
  const aFilter: string = ''; const aColumns: string = '';
  aCount: Integer = 100; aStart: Int64 = 0; aReverse: Boolean = False;
  aStartTime: Int64 = 0; aEndTime: Int64 = 0): string;
begin
  Result := DoGetData(CS_BITMEX_ENDPOINT_INSTRUMENT, aSymbol, aFilter, aColumns,
    aCount, aStart, aReverse, aStartTime, aEndTime);
end;

function TsgcHTTP_API_Bitmex_Rest.GetOrders(const aSymbol: string;
  const aFilter: string = ''; const aColumns: string = '';
  aCount: Integer = 100; aStart: Int64 = 0; aReverse: Boolean = False;
  aStartTime: Int64 = 0; aEndTime: Int64 = 0): string;
begin
  Result := DoGetData(CS_BITMEX_ENDPOINT_ORDER, aSymbol, aFilter, aColumns,
    aCount, aStart, aReverse, aStartTime, aEndTime);
end;

function TsgcHTTP_API_Bitmex_Rest.GetPosition(const aColumns: string = '';
  aCount: Integer = 100): string;
var
  oParameters: TStringList;
  vRequestPath: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_BITMEX_DELIMITER;
    if aColumns <> '' then
      oParameters.Add('columns=' + aColumns);
    if aCount > 0 then
      oParameters.Add('count=' + IntToStr(aCount));
    vRequestPath := CS_BITMEX_ENDPOINT_POSITION + '?' +
      oParameters.DelimitedText;

    Result := DoHTTP_GET_PRIVATE(vRequestPath);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Bitmex_Rest.GetTrades(const aSymbol: string;
  const aFilter: string = ''; const aColumns: string = '';
  aCount: Integer = 100; aStart: Int64 = 0; aReverse: Boolean = False;
  aStartTime: Int64 = 0; aEndTime: Int64 = 0): string;
begin
  Result := DoGetData(CS_BITMEX_ENDPOINT_TRADE, aSymbol, aFilter, aColumns,
    aCount, aStart, aReverse, aStartTime, aEndTime);
end;

function TsgcHTTP_API_Bitmex_Rest.PlaceMarketOrder(aSide:
    TsgcHTTPBitmexOrderSide; const aSymbol: string; aQuantity: Extended):
    string;
var
  oOrder: TsgcHTTPBitmexOrder;
begin
  oOrder := TsgcHTTPBitmexOrder.Create;
  Try
    oOrder.OrdType := bmotMarket;
    oOrder.Symbol := aSymbol;
    oOrder.Side := aSide;
    oOrder.OrderQty := aQuantity;

    Result := PlaceOrder(oOrder);
  Finally
    sgcFree(oOrder);
  End;
end;

function TsgcHTTP_API_Bitmex_Rest.PlaceLimitOrder(aSide:
    TsgcHTTPBitmexOrderSide; const aSymbol: string; aQuantity, aPrice:
    Extended): string;
var
  oOrder: TsgcHTTPBitmexOrder;
begin
  oOrder := TsgcHTTPBitmexOrder.Create;
  Try
    oOrder.OrdType := bmotLimit;
    oOrder.Symbol := aSymbol;
    oOrder.Side := aSide;
    oOrder.OrderQty := aQuantity;
    oOrder.Price := aPrice;

    Result := PlaceOrder(oOrder);
  Finally
    sgcFree(oOrder);
  End;
end;

function TsgcHTTP_API_Bitmex_Rest.PlaceStopOrder(aSide:
    TsgcHTTPBitmexOrderSide; const aSymbol: string; aQuantity, aStopPrice:
    Extended): string;
var
  oOrder: TsgcHTTPBitmexOrder;
begin
  oOrder := TsgcHTTPBitmexOrder.Create;
  Try
    oOrder.OrdType := bmotStop;
    oOrder.Symbol := aSymbol;
    oOrder.Side := aSide;
    oOrder.OrderQty := aQuantity;
    oOrder.StopPx := aStopPrice;

    Result := PlaceOrder(oOrder);
  Finally
    sgcFree(oOrder);
  End;
end;

function TsgcHTTP_API_Bitmex_Rest.PlaceOrder(const aOrder:
    TsgcHTTPBitmexOrder): string;
begin
  Result := DoHTTP_POST_PRIVATE(CS_BITMEX_ENDPOINT_ORDER, aOrder.GetJSON);
end;

function TsgcHTTP_API_Bitmex_Rest.PlaceStopLimitOrder(aSide:
    TsgcHTTPBitmexOrderSide; const aSymbol: string; aQuantity, aStopPrice,
    aLimitPrice: Extended): string;
var
  oOrder: TsgcHTTPBitmexOrder;
begin
  oOrder := TsgcHTTPBitmexOrder.Create;
  Try
    oOrder.OrdType := bmotStopLimit;
    oOrder.Symbol := aSymbol;
    oOrder.Side := aSide;
    oOrder.OrderQty := aQuantity;
    oOrder.StopPx := aStopPrice;
    oOrder.Price := aLimitPrice;

    Result := PlaceOrder(oOrder);
  Finally
    sgcFree(oOrder);
  End;
end;

function TsgcHTTP_API_Bitmex_Rest.CancelAllOrdersAfter(aTimeout: Integer):
    string;
begin
  Result := DoHTTP_POST_PRIVATE(Format(CS_BITMEX_ENDPOINT_ORDER_CANCELALLAFTER + '?timeout=%d', [aTimeout]));
end;

function TsgcHTTP_API_Bitmex_Rest.ClosePosition(const aSymbol: string; aPrice:
    Extended = 0): string;
var
  oParameters: TStringList;
  vRequestPath: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_BITMEX_DELIMITER;
    oParameters.Add('symbol=' + aSymbol);
    if aPrice <> 0 then
      oParameters.Add('price=' + GetFormatExtended(aPrice));
    vRequestPath := CS_BITMEX_ENDPOINT_ORDER_CLOSEPOSITION + '?' + oParameters.DelimitedText;

    Result := DoHTTP_POST_PRIVATE(vRequestPath);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Bitmex_Rest.SetPositionIsolate(const aSymbol: string;
  aEnabled: Boolean): string;
var
  oParameters: TStringList;
  vRequestPath: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_BITMEX_DELIMITER;
    oParameters.Add('symbol=' + aSymbol);
    if aEnabled then
      oParameters.Add('enabled=true')
    else
      oParameters.Add('enabled=false');
    vRequestPath := CS_BITMEX_ENDPOINT_POSITION_ISOLATE + '?' +
      oParameters.DelimitedText;

    Result := DoHTTP_POST_PRIVATE(vRequestPath);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Bitmex_Rest.SetPositionLeverage(const aSymbol: string;
  aLeverage: Double): string;
var
  oParameters: TStringList;
  vRequestPath: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_BITMEX_DELIMITER;
    oParameters.Add('symbol=' + aSymbol);
    oParameters.Add('leverage=' + GetFormatDouble(aLeverage));
    vRequestPath := CS_BITMEX_ENDPOINT_POSITION_LEVERAGE + '?' +
      oParameters.DelimitedText;

    Result := DoHTTP_POST_PRIVATE(vRequestPath);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Bitmex_Rest.SetPositionRiskLimit(const aSymbol: string;
  aRiskLimit: Double): string;
var
  oParameters: TStringList;
  vRequestPath: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_BITMEX_DELIMITER;
    oParameters.Add('symbol=' + aSymbol);
    oParameters.Add('riskLimit=' + GetFormatDouble(aRiskLimit));
    vRequestPath := CS_BITMEX_ENDPOINT_POSITION_RISKLIMIT + '?' +
      oParameters.DelimitedText;

    Result := DoHTTP_POST_PRIVATE(vRequestPath);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Bitmex_Rest.SetPositionTransferMargin
  (const aSymbol: string; aAmount: Extended): string;
var
  oParameters: TStringList;
  vRequestPath: string;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := CS_BITMEX_DELIMITER;
    oParameters.Add('symbol=' + aSymbol);
    oParameters.Add('amount=' + GetFormatExtended(aAmount));
    vRequestPath := CS_BITMEX_ENDPOINT_POSITION_TRANSFERMARGIN + '?' +
      oParameters.DelimitedText;

    Result := DoHTTP_POST_PRIVATE(vRequestPath);
  Finally
    sgcFree(oParameters);
  End;
end;

constructor TsgcHTTPBitmexAmendOrder.Create;
begin
  inherited;
  FOrderId := '';
  FOrigClOrdID := '';
  FOrderQty := 0;
  FPrice := 0;
  FLeavesQty := 0;
  FStopPx := 0;
  FClOrdId := '';
  FPegOffsetValue := 0;
  FText := '';
end;

function TsgcHTTPBitmexAmendOrder.GetJSON: string;
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    // mandatory
    oJSON.AddPair('OrderId', OrderId);
    oJSON.AddPair('origClOrdID', OrigClOrdID);

    // optional
    if ClOrdID <> '' then
      oJSON.AddPair('clOrdID', ClOrdID);
    if OrderQty <> 0 then
      oJSON.AddPair('orderQty', GetFormatExtended(OrderQty));
    if leavesQty <> 0 then
      oJSON.AddPair('leavesQty', GetFormatExtended(LeavesQty));
    if Price <> 0 then
      oJSON.AddPair('price', GetFormatExtended(Price));
    if StopPx <> 0 then
      oJSON.AddPair('stopPx', GetFormatExtended(StopPx));
    if PegOffsetValue <> 0 then
      oJSON.AddPair('pegOffsetValue', GetFormatExtended(PegOffsetValue));
    if Text <> '' then
      oJSON.AddPair('text', Text);

    // ... result
    Result := oJSON.Text;
  Finally
    sgcFree(oJSON);
  End;
end;

{$ENDIF}

end.
