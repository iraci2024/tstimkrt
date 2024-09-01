{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_API_FXCM;

interface

{$I sgcVer.inc}
{$IFDEF SGC_APIS}

uses
  Classes, SysUtils,
  // sgc
  sgcBase_Helpers, sgcWebSocket_API_SocketIO;

type
  TsgcWSFXCMHTTPMethods = (httpGET, httpPOST);
  TsgcWSFXCMTableModels = (tmoOffer, tmoOpenPosition, tmoClosedPosition,
    tmoOrder, tmoAccount, tmoSummary, tmoLeverageProfile, tmoProperties);
  TsgcWSFXCMTradingMethod = (tmeOpenTrade, tmeCloseTrade, tmeChangeOrder,
    tmeDeleteOrder, tmeCreateEntryOrder, tmeSimpleEco, tmeAddToOco,
    tmeRemoveFromOco, tmeEditOco, tmeChangeTradeStopLimit,
    tmeChangeOrderStopLimit, tmeCloseAllForSymbol);
  TsgcWSFXCMTimeFrame = (tmfMin1, tmfMin5, tmfMin15, tmfMin30, tmfHour1,
    tmfHour2, tmfHour3, tmfHour4, tmfHour6, tmfHour8, tmfDay1, tmfWeek1,
    tmfMonth1);

  TsgcWSFXCM_Options = class(TPersistent)
  private
    FAccessToken: string;
    FHost: String;
    FPort: Integer;
    FTLS: Boolean;
  public
    constructor Create; virtual;
    procedure Assign(aSource: TPersistent); override;
  published
    property AccessToken: string read FAccessToken write FAccessToken;
    property Host: String read FHost write FHost;
    property Port: Integer read FPort write FPort;
    property TLS: Boolean read FTLS write FTLS;
  end;

  TsgcWS_API_FXCM = class(TsgcWS_API_SocketIO)
    { from TsgcWSAPI_Client }
  protected
    function GetURL: String; override;
    { from TsgcWSAPI_Client }

    { from TsgcWS_API_SocketIO }
  protected
    function DoGetSessionURL: string; override;
    { from TsgcWS_API_SocketIO }

    { FXCM }
  private
    FFXCM: TsgcWSFXCM_Options;
    procedure SetFXCM(const Value: TsgcWSFXCM_Options);
  public
    property FXCM: TsgcWSFXCM_Options read FFXCM write SetFXCM;
    { FXCM }

    { HTTP Requests }
  private
    function GetSymbolsString(const aSymbol1, aSymbol2: String): String;
    function GetPairs(const aSymbol1, aSymbol2: String): String;
    function GetModelString(aModel: TsgcWSFXCMTableModels): String;
    function GetTradingMethod(aMethod: TsgcWSFXCMTradingMethod): String;
    function GetTimeFrame(aTimeFrame: TsgcWSFXCMTimeFrame): String;
  protected
    function DoRequest(const aURI, aBody: String;
      const aMethod: TsgcWSFXCMHTTPMethods): string; virtual;
  public
    function GetSymbols: string;
  public
    function SubscribeMarketData(const aSymbol1, aSymbol2: String): string;
    function UnSubscribeMarketData(const aSymbol1, aSymbol2: String): string;
  public
    function SubscribeTradingTables(const aModel
      : TsgcWSFXCMTableModels): string;
    function UnSubscribeTradingTables(const aModel
      : TsgcWSFXCMTableModels): string;
  public
    function SnapshotTradingTables(const aModel: TsgcWSFXCMTableModels): string;
  public
    function UpdateSubscriptions(const aSymbol1, aSymbol2: String; aVisible:
        Boolean): string;
  public
    function TradingOrder(const aTradingMethod: TsgcWSFXCMTradingMethod;
      const aParameters: String): string;
  public
    function GetHistoricalData(aOfferId: Integer;
      aTimeFrame: TsgcWSFXCMTimeFrame; aCandels: Integer): string;
    { HTTP Requests }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }
  end;
{$ENDIF}

implementation

{$IFDEF SGC_APIS}

uses
  // sgc
  sgcHTTP_Client, sgcWebSocket_Types;

const
  CS_FXCM_UPDATE_SUBSCRIPTIONS = '/trading/update_subscriptions';
  CS_FXCM_SUBSCRIBE = '/subscribe';
  CS_FXCM_UNSUBSCRIBE = '/unsubscribe';
  CS_FXCM_GET_SYMBOLS = '/trading/get_instruments/?';
  CS_FXCM_SUBSCRIBE_TRADING_TABLES = '/trading/subscribe';
  CS_FXCM_UNSUBSCRIBE_TRADING_TABLES = '/trading/unsubscribe';
  CS_FXCM_SNAPSHOT_TRADING_TABLES = '/trading/get_model';

constructor TsgcWSFXCM_Options.Create;
begin
  inherited;
  AccessToken := '';
  Host := 'api-demo.fxcm.com';
  Port := 443;
  TLS := True;
end;

procedure TsgcWSFXCM_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSFXCM_Options then
  begin
    AccessToken := TsgcWSFXCM_Options(aSource).AccessToken;
    Host := TsgcWSFXCM_Options(aSource).Host;
    Port := TsgcWSFXCM_Options(aSource).Port;
    TLS := TsgcWSFXCM_Options(aSource).TLS;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcWS_API_FXCM.Create(aOwner: TComponent);
begin
  inherited;
  FFXCM := TsgcWSFXCM_Options.Create;
end;

destructor TsgcWS_API_FXCM.Destroy;
begin
  sgcFree(FFXCM);
  inherited;
end;

function TsgcWS_API_FXCM.DoGetSessionURL: string;
begin
  result := inherited DoGetSessionURL;
  if FXCM.AccessToken <> '' then
    result := result + '&access_token=' + FXCM.AccessToken;
end;

function TsgcWS_API_FXCM.DoRequest(const aURI, aBody: String;
  const aMethod: TsgcWSFXCMHTTPMethods): string;
var
  oHTTP: TsgcIdHTTP;
  oStream, oResponse: TStringStream;
  vURL: String;
begin
  oHTTP := TsgcIdHTTP.Create(nil);
  oStream := TStringStream.Create(aBody);
  Try
    if Assigned(FClient) then
      oHTTP.TLSOptions.Assign(FClient.TLSOptions);
    oHTTP.ReadTimeout := 10000;

    // ... custom headers
    oHTTP.Request.UserAgent := Version;
    oHTTP.Request.Accept := 'application/json';
    oHTTP.Request.ContentType := 'application/x-www-form-urlencoded';
    {$IFDEF INDY10_5_5}
    oHTTP.Request.CustomHeaders.AddValue('Authorization',
      'Bearer ' + IO_SessionId + FXCM.AccessToken);
    {$ELSE}
    oHTTP.Request.CustomHeaders.Add('Authorization: Bearer ' + IO_SessionId +
      FXCM.AccessToken);
    {$ENDIF}
    // ... URL
    vURL := 'http';
    if Client.TLS then
      vURL := vURL + 's';
    vURL := vURL + '://' + FXCM.Host + ':' + IntToStr(FXCM.Port);
    // ... HTTP Request
    Case aMethod of
      httpGET:
        begin
          oHTTP.Get(vURL + aURI, oStream);
          result := oStream.DataString;
        end;
      httpPOST:
        begin
          oResponse := TStringStream.Create('');
          Try
            oHTTP.Post(vURL + aURI, oStream, oResponse);
            result := oResponse.DataString;
          Finally
            sgcFree(oResponse);
          End;
        end;
    end;
  Finally
    sgcFree(oStream);
    sgcFree(oHTTP);
  End;
end;

function TsgcWS_API_FXCM.GetHistoricalData(aOfferId: Integer;
  aTimeFrame: TsgcWSFXCMTimeFrame; aCandels: Integer): string;
begin
  result := DoRequest('/candles/' + IntToStr(aOfferId) + '/' +
    GetTimeFrame(aTimeFrame) + '/?num=' + IntToStr(aCandels), '', httpGET);
end;

function TsgcWS_API_FXCM.GetModelString(aModel: TsgcWSFXCMTableModels): String;
begin
  result := '';

  case aModel of
    tmoOffer:
      result := 'Offer';
    tmoOpenPosition:
      result := 'OpenPosition';
    tmoClosedPosition:
      result := 'ClosedPosition';
    tmoOrder:
      result := 'Order';
    tmoAccount:
      result := 'Account';
    tmoSummary:
      result := 'Summary';
    tmoLeverageProfile:
      result := 'LeverageProfile';
    tmoProperties:
      result := 'Properties';
  end;
end;

function TsgcWS_API_FXCM.GetPairs(const aSymbol1, aSymbol2: String): String;
begin
  result := 'pairs=' + GetSymbolsString(aSymbol1, aSymbol2);
end;

function TsgcWS_API_FXCM.GetSymbols: string;
begin
  result := DoRequest(CS_FXCM_GET_SYMBOLS, '', httpGET);
end;

function TsgcWS_API_FXCM.GetSymbolsString(const aSymbol1,
  aSymbol2: String): String;
begin
  result := URIEncode(aSymbol1 + '/' + aSymbol2);
end;

function TsgcWS_API_FXCM.GetTimeFrame(aTimeFrame: TsgcWSFXCMTimeFrame): String;
begin
  result := '';

  case aTimeFrame of
    tmfMin1:
      result := 'm1';
    tmfMin5:
      result := 'm5';
    tmfMin15:
      result := 'm15';
    tmfMin30:
      result := 'm30';
    tmfHour1:
      result := 'H1';
    tmfHour2:
      result := 'H2';
    tmfHour3:
      result := 'H3';
    tmfHour4:
      result := 'H4';
    tmfHour6:
      result := 'H6';
    tmfHour8:
      result := 'H8';
    tmfDay1:
      result := 'D1';
    tmfWeek1:
      result := 'W1';
    tmfMonth1:
      result := 'M1';
  end;
end;

function TsgcWS_API_FXCM.GetTradingMethod
  (aMethod: TsgcWSFXCMTradingMethod): String;
begin
  result := '';

  case aMethod of
    tmeOpenTrade:
      result := '/trading/open_trade';
    tmeCloseTrade:
      result := '/trading/close_trade';
    tmeChangeOrder:
      result := '/trading/change_order';
    tmeDeleteOrder:
      result := '/trading/delete_order';
    tmeCreateEntryOrder:
      result := '/trading/create_entry_order';
    tmeSimpleEco:
      result := '/trading/simple_eco';
    tmeAddToOco:
      result := '/trading/add_to_oco';
    tmeRemoveFromOco:
      result := '/trading/remove_from_oco';
    tmeEditOco:
      result := '/trading/edit_oco';
    tmeChangeTradeStopLimit:
      result := '/trading/change_trade_stop_limit';
    tmeChangeOrderStopLimit:
      result := '/trading/change_order_stop_limit';
    tmeCloseAllForSymbol:
      result := '/trading/close_all_for_symbol';
  end;
end;

function TsgcWS_API_FXCM.GetURL: String;
begin
  result := 'ws';
  if FXCM.TLS then
    result := result + 's';
  result := result + '://' + FXCM.Host + ':' + IntToStr(FXCM.Port);

  if Assigned(Client) then
    Client.TLSOptions.Version := tls1_2;
end;

procedure TsgcWS_API_FXCM.SetFXCM(const Value: TsgcWSFXCM_Options);
begin
  FFXCM.Assign(Value);
end;

function TsgcWS_API_FXCM.SnapshotTradingTables(const aModel
  : TsgcWSFXCMTableModels): string;
begin
  result := DoRequest(CS_FXCM_SNAPSHOT_TRADING_TABLES + '/?models=' +
    GetModelString(aModel), '', httpGET);
end;

function TsgcWS_API_FXCM.SubscribeMarketData(const aSymbol1,
  aSymbol2: String): string;
begin
  result := DoRequest(CS_FXCM_SUBSCRIBE, GetPairs(aSymbol1, aSymbol2),
    httpPOST);
end;

function TsgcWS_API_FXCM.SubscribeTradingTables(const aModel
  : TsgcWSFXCMTableModels): string;
begin
  result := DoRequest(CS_FXCM_SUBSCRIBE_TRADING_TABLES,
    'models=' + GetModelString(aModel), httpPOST);
end;

function TsgcWS_API_FXCM.TradingOrder(const aTradingMethod
  : TsgcWSFXCMTradingMethod; const aParameters: String): string;
begin
  result := DoRequest(GetTradingMethod(aTradingMethod), aParameters, httpPOST);
end;

function TsgcWS_API_FXCM.UnSubscribeMarketData(const aSymbol1,
  aSymbol2: String): string;
begin
  result := DoRequest(CS_FXCM_UNSUBSCRIBE, GetPairs(aSymbol1, aSymbol2),
    httpPOST);
end;

function TsgcWS_API_FXCM.UnSubscribeTradingTables(const aModel
  : TsgcWSFXCMTableModels): string;
begin
  result := DoRequest(CS_FXCM_UNSUBSCRIBE_TRADING_TABLES,
    'models=' + GetModelString(aModel), httpPOST);
end;

function TsgcWS_API_FXCM.UpdateSubscriptions(const aSymbol1, aSymbol2: String;
    aVisible: Boolean): string;
var
  vVisible: string;
  vBody: string;
begin
  // ... visible
  if aVisible then
    vVisible := 'true'
  else
    vVisible := 'false';
  // ... body
  vBody := 'symbol=' + GetSymbolsString(aSymbol1, aSymbol2) + '&visible=' + vVisible;
  // ... request
  result := DoRequest(CS_FXCM_UPDATE_SUBSCRIPTIONS, vBody, httpGET);
end;

{$ENDIF}

end.
