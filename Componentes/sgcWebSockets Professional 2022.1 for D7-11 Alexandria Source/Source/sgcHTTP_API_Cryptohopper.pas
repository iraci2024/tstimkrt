{ ***************************************************************************
  sgcHTTP API component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcHTTP_API_Cryptohopper;

interface

{$I sgcVer.inc}
{$IFDEF SGC_HTTP}

uses
  Classes, SysUtils,
{$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
{$IFDEF SGC_INDY}sgcIdUri{$ELSE}IdUri{$ENDIF},
{$IFDEF SGC_INDY}sgcIdContext{$ELSE}IdContext{$ENDIF},
{$IFDEF SGC_INDY}sgcIdCustomHTTPServer{$ELSE}IdCustomHTTPServer{$ENDIF},
  // sgc
  sgcHTTP_API, sgcHTTP_OAuth2_Client, sgcWebSocket_Classes, sgcWebSocket_Server;

type
  TsgcCryptohopperWebhookEvent = procedure(Sender: TObject; const aBody: string;
    const aSignature: string) of object;

  TsgcHTTPCTHOrderSide = (cthosNone, cthosBuy, cthosSell);

  TsgcHTTPCTHOrder = class
  private
    FAmount: Extended;
    FCoin: string;
    FMarketOrder: Integer;
    FOrderSide: TsgcHTTPCTHOrderSide;
    FPctProfit: Extended;
    FPrice: Extended;
    FTrailingBuy: Integer;
    FTrailingBuyPct: Extended;
  protected
    function GetJSON: string; virtual;
  public
    constructor Create; virtual;
  public
    property Amount: Extended read FAmount write FAmount;
    property Coin: string read FCoin write FCoin;
    property MarketOrder: Integer read FMarketOrder write FMarketOrder;
    property OrderSide: TsgcHTTPCTHOrderSide read FOrderSide write FOrderSide;
    property PctProfit: Extended read FPctProfit write FPctProfit;
    property Price: Extended read FPrice write FPrice;
    property TrailingBuy: Integer read FTrailingBuy write FTrailingBuy;
    property TrailingBuyPct: Extended read FTrailingBuyPct
      write FTrailingBuyPct;
  end;

  TsgcHTTPCTHSignal = class

  private
    FAPI_Key: string;
    FAPI_Secret: string;
    FExchange: string;
    FMarket: string;
    FPercentageProfit: Extended;
    FSignalId: string;
    FStopLoss: Extended;
    FTrailingArmPercentage: Extended;
    FTrailingPercentage: Extended;
    F_Type: TsgcHTTPCTHOrderSide;
  public
    function GetQueryString: string;
  public
    property API_Key: string read FAPI_Key write FAPI_Key;
    property API_Secret: string read FAPI_Secret write FAPI_Secret;
    property SignalId: string read FSignalId write FSignalId;
    property Exchange: string read FExchange write FExchange;
    property Market: string read FMarket write FMarket;
    property _Type: TsgcHTTPCTHOrderSide read F_Type write F_Type;
    property PercentageProfit: Extended read FPercentageProfit write
        FPercentageProfit;
    property StopLoss: Extended read FStopLoss write FStopLoss;
    property TrailingPercentage: Extended read FTrailingPercentage write
        FTrailingPercentage;
    property TrailingArmPercentage: Extended read FTrailingArmPercentage write
        FTrailingArmPercentage;
  end;

  TsgcHTTPCTHopper = class
  private
    FAPI_Key: string;
    FAPI_Secret: string;
    FBase_Currency: string;
    FBuying_Enabled: Integer;
    FEnabled: Integer;
    FExchange: string;
    FHopperName: string;
    FPaper_Trading: Integer;
    FSelling_Enabled: Integer;
    FStrategy: string;
  protected
    function GetJSON: string; virtual;
  public
    constructor Create; virtual;
  public
    property API_Key: string read FAPI_Key write FAPI_Key;
    property API_Secret: string read FAPI_Secret write FAPI_Secret;
    property Base_Currency: string read FBase_Currency write FBase_Currency;
    property Buying_Enabled: Integer read FBuying_Enabled write FBuying_Enabled;
    property Enabled: Integer read FEnabled write FEnabled;
    property Exchange: string read FExchange write FExchange;
    property HopperName: string read FHopperName write FHopperName;
    property Paper_Trading: Integer read FPaper_Trading write FPaper_Trading;
    property Selling_Enabled: Integer read FSelling_Enabled
      write FSelling_Enabled;
    property Strategy: string read FStrategy write FStrategy;
  end;

  TsgcHTTPCryptohopperLog_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FFileName: String;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property FileName: String read FFileName write FFileName;
  end;

  TsgcHTTPCryptohopperOAuth2_Options = class(TPersistent)
  private
    FClientId: String;
    FClientSecret: String;
    FLocalIP: String;
    FLocalPort: Integer;
    FRedirectURL: String;
    FScope: TStringList;
    procedure SetScope(const Value: TStringList);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property ClientId: String read FClientId write FClientId;
    property ClientSecret: String read FClientSecret write FClientSecret;
    property LocalIP: String read FLocalIP write FLocalIP;
    property LocalPort: Integer read FLocalPort write FLocalPort;
    property RedirectURL: String read FRedirectURL write FRedirectURL;
    property Scope: TStringList read FScope write SetScope;
  end;

  TsgcHTTPCryptohopperWebhook_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FHost: string;
    FPort: Integer;
    FSSLOptions: TsgcWSSSL_Options;
    FValidationCode: string;
    procedure SetSSLOptions(const Value: TsgcWSSSL_Options);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property Host: string read FHost write FHost;
    property Port: Integer read FPort write FPort;
    property SSLOptions: TsgcWSSSL_Options read FSSLOptions write SetSSLOptions;
    property ValidationCode: string read FValidationCode write FValidationCode;
  end;

  TsgcHTTPCryptohopper_Options = class(TPersistent)
  private
    FLogOptions: TsgcHTTPCryptohopperLog_Options;
    FOAuth2: TsgcHTTPCryptohopperOAuth2_Options;
    FWebhook: TsgcHTTPCryptohopperWebhook_Options;
    procedure SetLogOptions(const Value: TsgcHTTPCryptohopperLog_Options);
    procedure SetOAuth2(const Value: TsgcHTTPCryptohopperOAuth2_Options);
    procedure SetWebhook(const Value: TsgcHTTPCryptohopperWebhook_Options);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property LogOptions: TsgcHTTPCryptohopperLog_Options read FLogOptions
      write SetLogOptions;
    property OAuth2: TsgcHTTPCryptohopperOAuth2_Options read FOAuth2
      write SetOAuth2;
    property Webhook: TsgcHTTPCryptohopperWebhook_Options read FWebhook
      write SetWebhook;
  end;

  TsgcHTTP_API_Cryptohopper = class(TsgcHTTPAPI_Client)
    { oauth2 }
  private
    FOAuth2: TsgcHTTPComponentClient_OAuth2;
    FOAuth2Token: string;
    FOAuth2Expires: TDateTime;
    function GetOAuth2: TsgcHTTPComponentClient_OAuth2;
  protected
    procedure OnAfterAccessTokenEvent(Sender: TObject;
      const Access_Token, Token_Type, Expires_In, Refresh_Token, Scope,
      RawParams: String; var Handled: Boolean); virtual;
    procedure OnErrorAccessTokenEvent(Sender: TObject;
      const Error, Error_Description, Error_URI, RawParams: String); virtual;
  protected
    property OAuth2: TsgcHTTPComponentClient_OAuth2 read GetOAuth2
      write FOAuth2;
    { oauth2 }

    { HTTP server }
  private
    FHTTPServer: TsgcWSHTTPServer;
  protected
    function GetHTTPServer: TsgcWSHTTPServer;
    procedure OnCommandGetEvent(AContext: TIdContext;
      ARequestInfo: TIdHTTPRequestInfo;
      AResponseInfo: TIdHTTPResponseInfo); virtual;
  protected
    procedure DoWebhookEvent(const aBody: string;
      const aSignature: string); virtual;
  protected
    property HTTPServer: TsgcWSHTTPServer read GetHTTPServer write FHTTPServer;
  public
    procedure StartWebhook;
    procedure StopWebhook;
    { HTTP server }

    { http requests }
  private
    procedure DoInitialize;
  private
    function GetBaseURL: string;
  protected
    function DoHTTP_DELETE(const aMethod: String;
      const aParameters: String = ''; const aHeaders: TStrings = nil;
      const aBody: string = ''): string; virtual;
    function DoHTTP_GET(const aMethod: String; const aParameters: String = '';
      const aHeaders: TStrings = nil): string; virtual;
    function DoHTTP_POST(const aMethod: String; const aBody: String = '';
      const aHeaders: TStrings = nil): string; virtual;
    function DoHTTP_PATCH(const aMethod: String; const aBody: String = '';
      const aHeaders: TStrings = nil): string; virtual;
  protected
    procedure GetAuthHeaders(const aMethod, aRequestPath, aBody: String;
      var Headers: TStringList); virtual;
  protected
    function DoHTTP_GET_PRIVATE(const aRequestPath: String): string; virtual;
    function DoHTTP_POST_PRIVATE(const aRequestPath, aBody: String)
      : string; virtual;
    function DoHTTP_DELETE_PRIVATE(const aRequestPath: String;
      aBody: string = ''): string; virtual;
    function DoHTTP_PATCH_PRIVATE(const aRequestPath, aBody: String)
      : string; virtual;
    { http requests }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { properties }
  private
    FCryptohopperOptions: TsgcHTTPCryptohopper_Options;
    procedure SetCryptohopperOptions(const Value: TsgcHTTPCryptohopper_Options);
  public
    property CryptohopperOptions: TsgcHTTPCryptohopper_Options
      read FCryptohopperOptions write SetCryptohopperOptions;
    { properties }

    { hopper }
  public
    function GetHoppers: string;
    function CreateHopper(const aBody: string): string;
    function GetHopper(const aId: string): string; overload;
    function GetHopper(const aId: string; var aHopper: TsgcHTTPCTHopper)
      : Boolean; overload;
    function DeleteHopper(const aId: string): string;
    function UpdateHopper(const aId: string; const aBody: string)
      : string; overload;
    function UpdateHopper(const aId: string; const aHopper: TsgcHTTPCTHopper)
      : string; overload;
    { hopper }

    { orders }
  public
    function GetOpenOrders(const aId: string): string;
    function CreateNewOrder(const aId: string;
      const aOrder: TsgcHTTPCTHOrder): string;
    function PlaceMarketOrder(const aId: string;
      aOrderSide: TsgcHTTPCTHOrderSide; const aCoin: string;
      aAmount: Extended): string;
    function PlaceLimitOrder(const aId: string;
      aOrderSide: TsgcHTTPCTHOrderSide; const aCoin: string; aAmount: Extended;
      aPrice: Extended): string;
    function DeleteOrder(const aId: string; const aOrderId: string): string;
    function DeleteAllOrders(const aId: string): string;
    function GetOpenOrder(const aId: string; const aOrderId: string): string;
    function CancelOrder(const aId: string; const aOrderId: string): string;
    { orders }

    { position }
  public
    function GetPosition(const aId: string): string;
    { position }

    { trade }
  public
    function GetTradeHistory(const aId: string): string;
    function GetTradeHistoryById(const aId: string;
      const aTradeId: string): string;
    { trade }

    { exchange }
  public
    function GetExchange: string;
    function GetAllTickers(const aExchange: string): string;
    function GetMarketTicker(const aExchange: string;
      const aPair: string): string;
    function GetOrderBook(const aExchange: string; const aPair: string;
      const aDepth: string): string;
    { exchange }

    { webhooks }
  public
    function CreateWebhook(const aURL: string;
      aMessageTypes: string = ''): string;
    function DeleteWebhook(const aURL: string): string;
    { webhooks }

    { signals }
  protected
    function DoSendSignal(const aSignal: TsgcHTTPCTHSignal; aTest: Boolean): string;
  public
    function SendSignal(const aSignal: TsgcHTTPCTHSignal): string;
    function SendTestSignal(const aSignal: TsgcHTTPCTHSignal): string;
  public
    function GetSignalStats(const aSignalId: String; const aExchange: String = ''):
        string;
    { signals }

    { events }
  private
    FOnWebhook: TsgcCryptohopperWebhookEvent;
  public
    property OnWebhook: TsgcCryptohopperWebhookEvent read FOnWebhook
      write FOnWebhook;
    { events }

  end;

{$ENDIF}

implementation

{$IFDEF SGC_HTTP}

uses
  DateUtils,
  sgcBase_Helpers, sgcJSON, sgcWebSocket_Types;

const
  CS_CRYPTOHOPPER_ENDPOINT_BASE = 'https://api.cryptohopper.com/v1';

const
  CS_CRYPTOHOPPER_DELIMITER = '&';

const
  CS_CRYPTOHOPPER_OAUTH2_AUTH = 'https://www.cryptohopper.com/oauth2/authorize';
  CS_CRYPTOHOPPER_OAUTH2_TOKEN = 'https://www.cryptohopper.com/oauth2/token';
  CS_CYRPTOHOPPER_OAUTH2_SCOPE = 'read,notifications,manage,trade';

const
  CS_CRYPTOHOPPER_ENDPOINT_HOPPER = '/hopper';
  CS_CRYPTOHOPPER_ENDPOINT_HOPPER_ID = '/hopper/%s';

const
  CS_CRYPTOHOPPER_ENDPOINT_ORDER = '/hopper/%s/order';
  CS_CRYPTOHOPPER_ENDPOINT_ORDER_ALL = '/hopper/%s/order/all';
  CS_CRYPTOHOPPER_ENDPOINT_ORDER_ID = '/hopper/%s/order/%s';

const
  CS_CRYPTOHOPPER_ENDPOINT_POSITION = '/hopper/%s/position';

const
  CS_CRYPTOHOPPER_ENDPOINT_TRADE = '/hopper/%s/trade';
  CS_CRYPTOHOPPER_ENDPOINT_TRADE_ID = '/hopper/%s/trade/%s';

const
  CS_CRYPTOHOPPER_ENDPOINT_EXCHANGE = '/exchange';
  CS_CRYPTOHOPPER_ENDPOINT_EXCHANGE_TICKER = '/exchange/%s/ticker';
  CS_CRYPTOHOPPER_ENDPOINT_EXCHANGE_TICKER_PAIR = '/exchange/%s/ticker/%s';
  CS_CRYPTOHOPPER_ENDPOINT_EXCHANGE_ORDERBOOK = '/exchange/%s/%s/orderbook/%s';

const
  CS_CRYPTOHOPPER_ENDPOINT_WEBHOOKS = '/api/webhooks';

function GetOrderSide(aValue: TsgcHTTPCTHOrderSide): string;
begin
  case aValue of
    cthosBuy:
      Result := 'buy';
    cthosSell:
      Result := 'sell';
  else
    Result := '';
  end;
end;

constructor TsgcHTTP_API_Cryptohopper.Create(aOwner: TComponent);
begin
  inherited;
  FCryptohopperOptions := TsgcHTTPCryptohopper_Options.Create;
  FOAuth2Token := '';
  FOAuth2Expires := 0;
end;

destructor TsgcHTTP_API_Cryptohopper.Destroy;
begin
  sgcFree(FHTTPServer);
  sgcFree(FCryptohopperOptions);
  inherited;
end;

function TsgcHTTP_API_Cryptohopper.CancelOrder(const aId: string;
  const aOrderId: string): string;
begin
  Result := DoHTTP_DELETE_PRIVATE(Format(CS_CRYPTOHOPPER_ENDPOINT_ORDER_ID,
    [aId, aOrderId]));
end;

function TsgcHTTP_API_Cryptohopper.CreateHopper(const aBody: string): string;
begin
  Result := DoHTTP_POST_PRIVATE(CS_CRYPTOHOPPER_ENDPOINT_HOPPER, aBody);
end;

function TsgcHTTP_API_Cryptohopper.CreateNewOrder(const aId: string;
  const aOrder: TsgcHTTPCTHOrder): string;
begin
  Result := DoHTTP_POST_PRIVATE(Format(CS_CRYPTOHOPPER_ENDPOINT_ORDER, [aId]),
    aOrder.GetJSON);
end;

function TsgcHTTP_API_Cryptohopper.CreateWebhook(const aURL: string;
  aMessageTypes: string = ''): string;
begin
  if aMessageTypes = '' then
    aMessageTypes := 'config_error,on_first_start,on_panic_end,on_panic_start,'
      + 'order_cancelled,trade_completed,trade_error,order_placed,order_trigger';

  Result := DoHTTP_POST_PRIVATE(CS_CRYPTOHOPPER_ENDPOINT_WEBHOOKS,
    Format('{"webhook_url": "%s", "message_types": "%s"}',
    [aURL, aMessageTypes]));
end;

function TsgcHTTP_API_Cryptohopper.DeleteAllOrders(const aId: string): string;
begin
  Result := DoHTTP_DELETE_PRIVATE
    (Format(CS_CRYPTOHOPPER_ENDPOINT_ORDER_ALL, [aId]));
end;

function TsgcHTTP_API_Cryptohopper.DeleteHopper(const aId: string): string;
begin
  Result := DoHTTP_DELETE_PRIVATE
    (Format(CS_CRYPTOHOPPER_ENDPOINT_HOPPER_ID, [aId]));
end;

function TsgcHTTP_API_Cryptohopper.DeleteOrder(const aId: string;
  const aOrderId: string): string;
begin
  Result := DoHTTP_DELETE_PRIVATE(Format(CS_CRYPTOHOPPER_ENDPOINT_ORDER, [aId]),
    '{"order_id": ' + aOrderId + '}');
end;

function TsgcHTTP_API_Cryptohopper.DeleteWebhook(const aURL: string): string;
begin
  Result := DoHTTP_DELETE_PRIVATE(CS_CRYPTOHOPPER_ENDPOINT_WEBHOOKS,
    Format('{"webhook_url": "%s"}', [aURL]));
end;

function TsgcHTTP_API_Cryptohopper.DoHTTP_DELETE(const aMethod: String;
  const aParameters: String = ''; const aHeaders: TStrings = nil;
  const aBody: string = ''): string;
var
  vURL: String;
begin
  DoInitialize;

  vURL := GetBaseURL + aMethod;
  if aParameters <> '' then
    vURL := vURL + '?' + aParameters;
  Result := Delete(vURL, aHeaders, aBody);
end;

function TsgcHTTP_API_Cryptohopper.DoHTTP_DELETE_PRIVATE(const aRequestPath
  : String; aBody: string = ''): string;
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

function TsgcHTTP_API_Cryptohopper.DoHTTP_GET(const aMethod: String;
  const aParameters: String = ''; const aHeaders: TStrings = nil): string;
var
  vURL: String;
begin
  DoInitialize;

  vURL := GetBaseURL + aMethod;
  if aParameters <> '' then
    vURL := vURL + '?' + aParameters;
  Result := Get(vURL, aHeaders);
end;

function TsgcHTTP_API_Cryptohopper.DoHTTP_GET_PRIVATE(const aRequestPath
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

function TsgcHTTP_API_Cryptohopper.DoHTTP_PATCH(const aMethod: String;
  const aBody: String = ''; const aHeaders: TStrings = nil): string;
begin
  DoInitialize;

  Result := Patch(GetBaseURL + aMethod, aBody, aHeaders);
end;

function TsgcHTTP_API_Cryptohopper.DoHTTP_PATCH_PRIVATE(const aRequestPath,
  aBody: String): string;
var
  oHeaders: TStringList;
begin
  oHeaders := TStringList.Create;
  Try
    oHeaders.Add('content-type: application/json');
    GetAuthHeaders('PATCH', aRequestPath, aBody, oHeaders);
    Result := DoHTTP_PATCH(aRequestPath, aBody, oHeaders);
  Finally
    sgcFree(oHeaders);
  End;
end;

function TsgcHTTP_API_Cryptohopper.DoHTTP_POST(const aMethod: String;
  const aBody: String = ''; const aHeaders: TStrings = nil): string;
begin
  DoInitialize;

  Result := Post(GetBaseURL + aMethod, aBody, aHeaders);
end;

function TsgcHTTP_API_Cryptohopper.DoHTTP_POST_PRIVATE(const aRequestPath,
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

procedure TsgcHTTP_API_Cryptohopper.DoInitialize;
begin
  // ... log
  if Log <> CryptohopperOptions.LogOptions.Enabled then
  begin
    LogFileName := CryptohopperOptions.LogOptions.FileName;
    Log := FCryptohopperOptions.LogOptions.Enabled;
  end;

  // ... http server
  if HTTPServer.Active <> CryptohopperOptions.Webhook.Enabled then
  begin
    if CryptohopperOptions.Webhook.Enabled then
    begin
      HTTPServer.Bindings.Clear;
      With HTTPServer.Bindings.Add do
      begin
        IP := CryptohopperOptions.Webhook.Host;
        Port := CryptohopperOptions.Webhook.Port;
      end;
      HTTPServer.Port := CryptohopperOptions.Webhook.Port;
      HTTPServer.SSLOptions := CryptohopperOptions.Webhook.SSLOptions;
      HTTPServer.SSLOptions.Port := CryptohopperOptions.Webhook.Port;
    end
    else
      HTTPServer.Stop;
  end;
end;

function TsgcHTTP_API_Cryptohopper.DoSendSignal(const aSignal:
    TsgcHTTPCTHSignal; aTest: Boolean): string;
var
  oHeaders: TStringList;
  vSignature: string;
  vURL, vValue: string;
begin
  if aTest then
    vValue := '/testsignal.php'
  else
    vValue := '/signal.php';

  vValue := vValue + '?' + aSignal.GetQueryString;
  vURL := 'https://www.cryptohopper.com' + vValue;
  vSignature := GetHMACSHA512(sgcGetBytesFromUTF8String(vValue), sgcGetBytesFromUTF8String(aSignal.API_Secret));
  oHeaders := TStringList.Create;
  Try
    oHeaders.Add('content-type: application/json');
    oHeaders.Add('X-Hub-Signature:' + vSignature);
    Result := Get(vURL, oHeaders);
  Finally
    sgcFree(oHeaders);
  End;
end;

procedure TsgcHTTP_API_Cryptohopper.DoWebhookEvent(const aBody: string;
  const aSignature: string);
begin
  if (aBody <> '') and (aSignature <> '') then
  begin
    if Assigned(FOnWebhook) then
      FOnWebhook(self, aBody, aSignature);
  end;
end;

function TsgcHTTP_API_Cryptohopper.GetAllTickers(const aExchange
  : string): string;
begin
  Result := DoHTTP_GET(Format(CS_CRYPTOHOPPER_ENDPOINT_EXCHANGE_TICKER,
    [aExchange]));
end;

procedure TsgcHTTP_API_Cryptohopper.GetAuthHeaders(const aMethod, aRequestPath,
  aBody: String; var Headers: TStringList);
var
  vStart: Cardinal;
  vTimeout: Integer;
begin
  if FOAuth2Expires < Now then
  begin
    // ... Wait OAuth2 Token
    vTimeout := 10000;
    vStart := sgcGetTicks;
    OAuth2.Start;
    Try
      repeat
        if (FOAuth2Expires > Now) and (FOAuth2Token <> '') then
          break;
        sleep(1);
      until sgcGetTickDiff(vStart, sgcGetTicks) >= Cardinal(vTimeout);
    Finally
      OAuth2.Stop;
    End;

    if FOAuth2Expires < Now then
      raise Exception.Create('OAuth2 Timeout: Cannot Get Access Token.');
  end;

  // ... Add Access Token
  Headers.Add('access-token: ' + FOAuth2Token);
end;

function TsgcHTTP_API_Cryptohopper.GetBaseURL: string;
begin
  Result := CS_CRYPTOHOPPER_ENDPOINT_BASE;
end;

function TsgcHTTP_API_Cryptohopper.GetExchange: string;
begin
  Result := DoHTTP_GET(CS_CRYPTOHOPPER_ENDPOINT_EXCHANGE);
end;

function TsgcHTTP_API_Cryptohopper.GetHopper(const aId: string): string;
begin
  Result := DoHTTP_GET_PRIVATE
    (Format(CS_CRYPTOHOPPER_ENDPOINT_HOPPER_ID, [aId]));
end;

function TsgcHTTP_API_Cryptohopper.GetHopper(const aId: string;
  var aHopper: TsgcHTTPCTHopper): Boolean;
var
  oJSON: TsgcJSON;
  vResponse: string;
  oNode: IsgcObjectJSON;
begin
  Result := False;
  vResponse := GetHopper(aId);
  if vResponse <> '' then
  begin
    oJSON := TsgcJSON.Create(nil);
    Try
      oJSON.Read(vResponse);
      if oJSON.Node['data'] <> nil then
      begin
        if oJSON.Node['data'].Node['hopper'] <> nil then
        begin
          oNode := oJSON.Node['data'].Node['hopper'];
          if oNode.Node['name'] <> nil then
            aHopper.HopperName := oNode.Node['name'].Value;
          if oNode.Node['enabled'] <> nil then
            aHopper.Enabled := oNode.Node['enabled'].Value;
          if oNode.Node['exchange'] <> nil then
            aHopper.Exchange := oNode.Node['exchange'].Value;
          if oNode.Node['base_currency'] <> nil then
            aHopper.Base_Currency := oNode.Node['base_currency'].Value;
          if oNode.Node['buying_enabled'] <> nil then
            aHopper.Buying_Enabled := oNode.Node['buying_enabled'].Value;
          if oNode.Node['selling_enabled'] <> nil then
            aHopper.Selling_Enabled := oNode.Node['selling_enabled'].Value;
          if oNode.Node['paper_trading'] <> nil then
            aHopper.Paper_Trading := oNode.Node['paper_trading'].Value;

          Result := True;
        end;
      end;
    Finally
      sgcFree(oJSON);
    End;
  end;
end;

function TsgcHTTP_API_Cryptohopper.GetHoppers: string;
begin
  Result := DoHTTP_GET_PRIVATE(CS_CRYPTOHOPPER_ENDPOINT_HOPPER);
end;

function TsgcHTTP_API_Cryptohopper.GetHTTPServer: TsgcWSHTTPServer;
begin
  if not Assigned(FHTTPServer) then
  begin
    FHTTPServer := TsgcWSHTTPServer.Create(nil);
    FHTTPServer.OnCommandGet := OnCommandGetEvent;
    FHTTPServer.SSL := True;
    FHTTPServer.SSLOptions.Version := tls1_2;
  end;
  Result := FHTTPServer;
end;

function TsgcHTTP_API_Cryptohopper.GetMarketTicker(const aExchange: string;
  const aPair: string): string;
begin
  Result := DoHTTP_GET(Format(CS_CRYPTOHOPPER_ENDPOINT_EXCHANGE_TICKER_PAIR,
    [aExchange, aPair]));
end;

function TsgcHTTP_API_Cryptohopper.GetOAuth2: TsgcHTTPComponentClient_OAuth2;
begin
  if not Assigned(FOAuth2) then
  begin
    FOAuth2 := TsgcHTTPComponentClient_OAuth2.Create(nil);
    FOAuth2.AuthorizationServerOptions.AuthURL := CS_CRYPTOHOPPER_OAUTH2_AUTH;
    FOAuth2.AuthorizationServerOptions.TokenURL := CS_CRYPTOHOPPER_OAUTH2_TOKEN;
    FOAuth2.HTTPClientOptions.TLSOptions.Version := tls1_2;
    FOAuth2.OAuth2Options.GrantType := auth2ClientCredentials;

    FOAuth2.OnAfterAccessToken := OnAfterAccessTokenEvent;
    FOAuth2.OnErrorAccessToken := OnErrorAccessTokenEvent;
  end;
  FOAuth2.AuthorizationServerOptions.Scope.Text :=
    CryptohopperOptions.OAuth2.Scope.Text;
  FOAuth2.OAuth2Options.ClientId := CryptohopperOptions.OAuth2.ClientId;
  FOAuth2.OAuth2Options.ClientSecret := CryptohopperOptions.OAuth2.ClientSecret;
  FOAuth2.LocalServerOptions.IP := CryptohopperOptions.OAuth2.LocalIP;
  FOAuth2.LocalServerOptions.Port := CryptohopperOptions.OAuth2.LocalPort;
  FOAuth2.LocalServerOptions.RedirectURL :=
    CryptohopperOptions.OAuth2.RedirectURL;

  Result := FOAuth2;
end;

function TsgcHTTP_API_Cryptohopper.GetOpenOrder(const aId: string;
  const aOrderId: string): string;
begin
  Result := DoHTTP_GET_PRIVATE(Format(CS_CRYPTOHOPPER_ENDPOINT_ORDER_ID,
    [aId, aOrderId]));
end;

function TsgcHTTP_API_Cryptohopper.GetOpenOrders(const aId: string): string;
begin
  Result := DoHTTP_GET_PRIVATE(Format(CS_CRYPTOHOPPER_ENDPOINT_ORDER, [aId]));
end;

function TsgcHTTP_API_Cryptohopper.GetOrderBook(const aExchange: string;
  const aPair: string; const aDepth: string): string;
begin
  Result := DoHTTP_GET_PRIVATE
    (Format(CS_CRYPTOHOPPER_ENDPOINT_EXCHANGE_ORDERBOOK, [aExchange, aPair,
    aDepth]));
end;

function TsgcHTTP_API_Cryptohopper.GetPosition(const aId: string): string;
begin
  Result := DoHTTP_GET_PRIVATE
    (Format(CS_CRYPTOHOPPER_ENDPOINT_POSITION, [aId]));
end;

function TsgcHTTP_API_Cryptohopper.GetTradeHistory(const aId: string): string;
begin
  Result := DoHTTP_GET_PRIVATE(Format(CS_CRYPTOHOPPER_ENDPOINT_TRADE, [aId]));
end;

function TsgcHTTP_API_Cryptohopper.GetTradeHistoryById(const aId: string;
  const aTradeId: string): string;
begin
  Result := DoHTTP_GET_PRIVATE(Format(CS_CRYPTOHOPPER_ENDPOINT_TRADE_ID,
    [aId, aTradeId]));
end;

procedure TsgcHTTP_API_Cryptohopper.OnAfterAccessTokenEvent(Sender: TObject;
  const Access_Token, Token_Type, Expires_In, Refresh_Token, Scope,
  RawParams: String; var Handled: Boolean);
begin
  FOAuth2Token := Access_Token;
  if Expires_In <> '' then
    FOAuth2Expires := IncSecond(Now, Trunc(StrToInt(Expires_In) / 2))
  else
    FOAuth2Expires := IncSecond(Now, 60);
end;

procedure TsgcHTTP_API_Cryptohopper.OnCommandGetEvent(AContext: TIdContext;
  ARequestInfo: TIdHTTPRequestInfo; AResponseInfo: TIdHTTPResponseInfo);
var
  oStream: TStringStream;
  vBody: string;
  vSignature: string;
  vType: string;
  oJSON: TsgcJSON;
begin
  if CryptohopperOptions.Webhook.Enabled and (ARequestInfo.PostStream <> nil)
  then
  begin
    oStream := TStringStream.Create('');
    Try
      oStream.CopyFrom(ARequestInfo.PostStream, ARequestInfo.PostStream.Size);
      vBody := oStream.DataString;
      vSignature := ARequestInfo.RawHeaders.Values['X-HUB-SIGNATURE'];

      oJSON := TsgcJSON.Create(nil);
      Try
        vType := '';
        oJSON.Read(vBody);
        if oJSON.Node['type'] <> nil then
          vType := oJSON.Node['type'].Value;

        if vType = 'validate' then
        begin
          AResponseInfo.ContentText :=
            CryptohopperOptions.Webhook.ValidationCode;
          AResponseInfo.ResponseNo := 200;
          AResponseInfo.ContentType := 'text/html';
        end
        else if vType = 'messages' then
        begin
          AResponseInfo.ContentText := '';
          AResponseInfo.ResponseNo := 200;
          AResponseInfo.ContentType := 'text/html';

          DoWebhookEvent(vBody, vSignature);
        end
        else
          AResponseInfo.ResponseNo := 404;
      Finally
        sgcFree(oJSON);
      End;

    Finally
      sgcFree(oStream);
    End;
  end
  else
    AResponseInfo.ResponseNo := 404;
end;

procedure TsgcHTTP_API_Cryptohopper.OnErrorAccessTokenEvent(Sender: TObject;
  const Error, Error_Description, Error_URI, RawParams: String);
begin
  raise Exception.Create('OAuth2 Error: ' + Error + ': ' + Error_Description);
end;

function TsgcHTTP_API_Cryptohopper.PlaceLimitOrder(const aId: string;
  aOrderSide: TsgcHTTPCTHOrderSide; const aCoin: string; aAmount: Extended;
  aPrice: Extended): string;
var
  oOrder: TsgcHTTPCTHOrder;
begin
  oOrder := TsgcHTTPCTHOrder.Create;
  Try
    oOrder.OrderSide := aOrderSide;
    oOrder.Coin := aCoin;
    oOrder.Amount := aAmount;
    oOrder.Price := aPrice;
    Result := CreateNewOrder(aId, oOrder);
  Finally
    sgcFree(oOrder);
  End;
end;

function TsgcHTTP_API_Cryptohopper.PlaceMarketOrder(const aId: string;
  aOrderSide: TsgcHTTPCTHOrderSide; const aCoin: string;
  aAmount: Extended): string;
var
  oOrder: TsgcHTTPCTHOrder;
begin
  oOrder := TsgcHTTPCTHOrder.Create;
  Try
    oOrder.OrderSide := aOrderSide;
    oOrder.Coin := aCoin;
    oOrder.Amount := aAmount;
    Result := CreateNewOrder(aId, oOrder);
  Finally
    sgcFree(oOrder);
  End;
end;

function TsgcHTTP_API_Cryptohopper.SendSignal(const aSignal:
    TsgcHTTPCTHSignal): string;
begin
  Result := DoSendSignal(aSignal, False);
end;

function TsgcHTTP_API_Cryptohopper.GetSignalStats(const aSignalId: String;
    const aExchange: String = ''): string;
var
  oParameters: TStringList;
begin
  oParameters := TStringList.Create;
  Try
    oParameters.Delimiter := '&';
    oParameters.Add('signal_id=' + aSignalId);
    if aExchange <> '' then
      oParameters.Add('exchange=' + aExchange);
    Result := Get('https://www.cryptohopper.com/signalstats.php?' + oParameters.DelimitedText);
  Finally
    sgcFree(oParameters);
  End;
end;

function TsgcHTTP_API_Cryptohopper.SendTestSignal(const aSignal:
    TsgcHTTPCTHSignal): string;
begin
  Result := DoSendSignal(aSignal, True);
end;


procedure TsgcHTTP_API_Cryptohopper.SetCryptohopperOptions
  (const Value: TsgcHTTPCryptohopper_Options);
begin
  if Assigned(FCryptohopperOptions) then
    FCryptohopperOptions.Assign(Value);
end;

procedure TsgcHTTP_API_Cryptohopper.StartWebhook;
begin
  DoInitialize;

  HTTPServer.Start;
end;

procedure TsgcHTTP_API_Cryptohopper.StopWebhook;
begin
  HTTPServer.Stop;
end;

function TsgcHTTP_API_Cryptohopper.UpdateHopper(const aId: string;
  const aBody: string): string;
begin
  Result := DoHTTP_PATCH_PRIVATE(Format(CS_CRYPTOHOPPER_ENDPOINT_HOPPER_ID,
    [aId]), aBody);
end;

function TsgcHTTP_API_Cryptohopper.UpdateHopper(const aId: string;
  const aHopper: TsgcHTTPCTHopper): string;
begin
  Result := UpdateHopper(aId, aHopper.GetJSON);
end;

constructor TsgcHTTPCryptohopper_Options.Create;
begin
  inherited;
  FOAuth2 := TsgcHTTPCryptohopperOAuth2_Options.Create;
  FLogOptions := TsgcHTTPCryptohopperLog_Options.Create;
  FWebhook := TsgcHTTPCryptohopperWebhook_Options.Create;
end;

destructor TsgcHTTPCryptohopper_Options.Destroy;
begin
  sgcFree(FWebhook);
  sgcFree(FOAuth2);
  sgcFree(FLogOptions);
  inherited;
end;

procedure TsgcHTTPCryptohopper_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTPCryptohopper_Options then
  begin
    LogOptions := TsgcHTTPCryptohopper_Options(aSource).LogOptions;
    OAuth2 := TsgcHTTPCryptohopper_Options(aSource).OAuth2;
    Webhook := TsgcHTTPCryptohopper_Options(aSource).Webhook;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcHTTPCryptohopper_Options.SetLogOptions
  (const Value: TsgcHTTPCryptohopperLog_Options);
begin
  if Assigned(FLogOptions) then
    FLogOptions.Assign(Value);
end;

procedure TsgcHTTPCryptohopper_Options.SetOAuth2(const Value
  : TsgcHTTPCryptohopperOAuth2_Options);
begin
  if Assigned(FOAuth2) then
    FOAuth2.Assign(Value);
end;

procedure TsgcHTTPCryptohopper_Options.SetWebhook
  (const Value: TsgcHTTPCryptohopperWebhook_Options);
begin
  if Assigned(FWebhook) then
    FWebhook.Assign(Value);
end;

procedure TsgcHTTPCryptohopperLog_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTPCryptohopperLog_Options then
  begin
    Enabled := TsgcHTTPCryptohopperLog_Options(aSource).Enabled;
    FileName := TsgcHTTPCryptohopperLog_Options(aSource).FileName;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcHTTPCTHOrder.Create;
begin
  inherited;
  FAmount := -1;
  FCoin := '';
  FMarketOrder := -1;
  FOrderSide := cthosNone;
  FPctProfit := -1;
  FPrice := -1;
  FTrailingBuy := -1;
  FTrailingBuyPct := -1;
end;

function TsgcHTTPCTHOrder.GetJSON: string;
var
  oJSON: TsgcJSON;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    if OrderSide <> cthosNone then
      oJSON.AddPair('order_type', GetOrderSide(OrderSide));
    oJSON.AddPair('coin', Coin);
    if Amount > -1 then
      oJSON.AddPair('amount', Amount);
    if Price > -1 then
      oJSON.AddPair('price', Price);
    if MarketOrder > -1 then
      oJSON.AddPair('market_order', MarketOrder);
    if TrailingBuy > -1 then
      oJSON.AddPair('trailing_buy', TrailingBuy);
    if TrailingBuyPct > -1 then
      oJSON.AddPair('trailing_buy_pct', TrailingBuyPct);
    if PctProfit > -1 then
      oJSON.AddPair('pct_profit', PctProfit);

    // ... result
    Result := oJSON.Text;
  Finally
    sgcFree(oJSON);
  End;
end;

constructor TsgcHTTPCryptohopperOAuth2_Options.Create;
begin
  inherited;
  LocalIP := '127.0.0.1';
  LocalPort := 8080;
  RedirectURL := '';
  FScope := TStringList.Create;
  FScope.Text := CS_CYRPTOHOPPER_OAUTH2_SCOPE;
end;

destructor TsgcHTTPCryptohopperOAuth2_Options.Destroy;
begin
  sgcFree(FScope);
  inherited;
end;

procedure TsgcHTTPCryptohopperOAuth2_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTPCryptohopperOAuth2_Options then
  begin
    ClientId := TsgcHTTPCryptohopperOAuth2_Options(aSource).ClientId;
    ClientSecret := TsgcHTTPCryptohopperOAuth2_Options(aSource).ClientSecret;
    Scope.Text := TsgcHTTPCryptohopperOAuth2_Options(aSource).Scope.Text;
    LocalIP := TsgcHTTPCryptohopperOAuth2_Options(aSource).LocalIP;
    LocalPort := TsgcHTTPCryptohopperOAuth2_Options(aSource).LocalPort;
    RedirectURL := TsgcHTTPCryptohopperOAuth2_Options(aSource).RedirectURL;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcHTTPCryptohopperOAuth2_Options.SetScope(const Value: TStringList);
begin
  if Assigned(FScope) then
    FScope.Assign(Value);
end;

constructor TsgcHTTPCTHopper.Create;
begin
  inherited;
  FBase_Currency := '';
  FBuying_Enabled := -1;
  FEnabled := -1;
  FExchange := '';
  FHopperName := '';
  FPaper_Trading := -1;
  FSelling_Enabled := -1;
  FAPI_Key := '';
  FAPI_Secret := '';
  FStrategy := '';
end;

function TsgcHTTPCTHopper.GetJSON: string;
var
  oJSON: TsgcJSON;
  oConfig: IsgcObjectJSON;

  function GetConfig: IsgcJSON;
  begin
    if oConfig = nil then
      oConfig := oJSON.AddObject('config');
    Result := oConfig.JSONObject;
  end;

begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oConfig := nil;

    if HopperName <> '' then
      oJSON.AddPair('name', HopperName);
    if Enabled > -1 then
      oJSON.AddPair('enabled', Enabled);

    if Base_Currency <> '' then
      GetConfig.AddPair('base_currency', Base_Currency);
    if Exchange <> '' then
      GetConfig.AddPair('exchange', Exchange);
    if Paper_Trading > -1 then
      GetConfig.AddPair('paper_trading', Paper_Trading);
    if Buying_Enabled > -1 then
      GetConfig.AddPair('buying_enabled', Buying_Enabled);
    if Selling_Enabled > -1 then
      GetConfig.AddPair('selling_enabled', Selling_Enabled);
    if API_Key <> '' then
      GetConfig.AddPair('api_key', API_Key);
    if API_Secret <> '' then
      GetConfig.AddPair('api_secret', API_Secret);
    if Strategy <> '' then
      GetConfig.AddPair('strategy', Strategy);

    // ... result
    Result := oJSON.Text;
  Finally
    sgcFree(oJSON);
  End;
end;

constructor TsgcHTTPCryptohopperWebhook_Options.Create;
begin
  inherited;
  FSSLOptions := TsgcWSSSL_Options.Create;
  Enabled := False;
  Host := '127.0.0.1';
  Port := 443;
  ValidationCode := '1234';
end;

destructor TsgcHTTPCryptohopperWebhook_Options.Destroy;
begin
  sgcFree(FSSLOptions);
  inherited;
end;

procedure TsgcHTTPCryptohopperWebhook_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcHTTPCryptohopperWebhook_Options then
  begin
    Enabled := TsgcHTTPCryptohopperWebhook_Options(aSource).Enabled;
    Host := TsgcHTTPCryptohopperWebhook_Options(aSource).Host;
    Port := TsgcHTTPCryptohopperWebhook_Options(aSource).Port;
    ValidationCode := TsgcHTTPCryptohopperWebhook_Options(aSource)
      .ValidationCode;
    SSLOptions := TsgcHTTPCryptohopperWebhook_Options(aSource).SSLOptions;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcHTTPCryptohopperWebhook_Options.SetSSLOptions
  (const Value: TsgcWSSSL_Options);
begin
  if Assigned(FSSLOptions) then
    FSSLOptions.Assign(Value);
end;

function TsgcHTTPCTHSignal.GetQueryString: string;
var
  oQueryString: TStringList;
  vFS: TFormatSettings;
begin
  vFS.DecimalSeparator := '.';
  vFS.ThousandSeparator := ',';

  oQueryString := TStringList.Create;
  Try
    oQueryString.Delimiter := '&';
    oQueryString.Add('api_key=' + API_Key);
    oQueryString.Add('signal_id=' + SignalId);
    oQueryString.Add('exchange=' + Exchange);
    oQueryString.Add('market=' + Market);
    oQueryString.Add('type=' + GetOrderSide(_Type));
    if PercentageProfit <> 0 then
      oQueryString.Add('percentage_profit=' + FormatFloat('0.0000', PercentageProfit, vFS));
    if StopLoss <> 0 then
      oQueryString.Add('stop_loss=' + FormatFloat('0.0000', StopLoss, vFS));
    if TrailingPercentage <> 0 then
      oQueryString.Add('trailing_percentage=' + FormatFloat('0.0000', TrailingPercentage, vFS));
    if TrailingArmPercentage <> 0 then
      oQueryString.Add('trailing_arm_percentage=' + FormatFloat('0.0000', TrailingArmPercentage, vFS));

    result := oQueryString.DelimitedText;
  Finally
    sgcFree(oQueryString);
  End;
end;

{$ENDIF}

end.
