{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_API_FTX;

interface

{$I sgcVer.inc}
{$IFDEF SGC_APIS}

uses
  Classes, SysUtils, StrUtils,
  // sgc
  sgcWebSocket_Classes, sgcJSON, sgcHTTP_API_FTX;

type
  TsgcWSFTXSubscribedEvent = procedure(Sender: TObject; aChannel: string;
    aMarket: string; aRawMessage: string) of object;
  TsgcWSFTXUnsubscribedEvent = procedure(Sender: TObject; aChannel: string;
    aMarket: string; aRawMessage: string) of object;

  TsgcWSFTXMessageEvent = procedure(Sender: TObject; aType: string;
    aRawMessage: string) of object;
  TsgcWSFTXErrorEvent = procedure(Sender: TObject;
    aCode, aMessage, aRawMessage: string) of object;
  TsgcWSFTXHTTPExceptionEvent = procedure(Sender: TObject; E: Exception)
    of object;

  TsgcWSFTXRequest = (ftrSubscribe, ftrUnsubscribe);
  TsgcWSFTXChannels = (ftcTicker, ftcMarkets, ftcTrades, ftcOrderbook,
    ftcOrderbookGrouped, ftcFills, ftcOrders);

  TsgcWSFTXLog_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FFileName: String;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property FileName: String read FFileName write FFileName;
  end;

  TsgcWSFTX_Options = class(TPersistent)
  private
    FApiKey: String;
    FApiSecret: String;
    FHTTPLogOptions: TsgcWSFTXLog_Options;
    FSubAccount: string;
    procedure SetHTTPLogOptions(const Value: TsgcWSFTXLog_Options);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property ApiKey: String read FApiKey write FApiKey;
    property ApiSecret: String read FApiSecret write FApiSecret;
    property HTTPLogOptions: TsgcWSFTXLog_Options read FHTTPLogOptions
      write SetHTTPLogOptions;
    property SubAccount: string read FSubAccount write FSubAccount;
  end;

  TsgcWS_API_FTX = class(TsgcWSAPI_client)
    { from TsgcWSAPI_Client }
  protected
    function GetURL: String; override;
    { from TsgcWSAPI_Client }

    { from TsgcWSComponent_Base }
  protected
    procedure DoBeforeConnect; override;
  protected
    procedure DoEventConnect(aConnection: TsgcWSConnection); override;
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
    function GetJSONChannel: string;
    function GetJSONMarket: string;
    function GetJSONCode: string;
    function GetJSONMessage: string;
  protected
    procedure DoReadJSONType(aRawMessage: string); virtual;
  protected
    property JSON: TsgcJSON read GetJSON write FJSON;
    { JSON }

    { options }
  private
    FFTX: TsgcWSFTX_Options;
    FREST_API: TsgcHTTP_API_FTX_Rest;
    procedure SetFTX(const Value: TsgcWSFTX_Options);
    function GetREST_API: TsgcHTTP_API_FTX_Rest;
  protected
    procedure OnHTTPAPIExceptionEvent(Sender: TObject; E: Exception); virtual;
  public
    property FTX: TsgcWSFTX_Options read FFTX write SetFTX;
    property REST_API: TsgcHTTP_API_FTX_Rest read GetREST_API write FREST_API;
    { options }

    { constructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor }

    { methods }
  private
    function GetChannel(aChannel: TsgcWSFTXChannels): string;
  protected
    procedure DoWriteData(const aText: string); virtual;
  protected
    procedure DoRequest(aRequest: TsgcWSFTXRequest; aChannel: TsgcWSFTXChannels;
      const aMarket: String = ''; aGrouping: Integer = 0;
      aAuthenticate: Boolean = False); virtual;
    procedure DoLogin; virtual;
  protected
    procedure DoPing; virtual;
  public
    procedure Ping;
    // public methods
  public
    procedure SubscribeTicker(const aMarket: string);
    procedure UnSubscribeTicker(const aMarket: string);
  public
    procedure SubscribeMarkets;
    procedure UnSubscribeMarkets;
  public
    procedure SubscribeTrades(const aMarket: string);
    procedure UnSubscribeTrades(const aMarket: string);
  public
    procedure SubscribeOrderbooks(const aMarket: string);
    procedure UnSubscribeOrderbooks(const aMarket: string);
  public
    procedure SubscribeGroupedOrderbooks(const aMarket: string;
      aGrouping: Integer = 500);
    procedure UnSubscribeGroupedOrderbooks(const aMarket: string;
      aGrouping: Integer = 500);
    // private methods
  public
    procedure SubscribeFills;
    procedure UnSubscribeFills;
  public
    procedure SubscribeOrders;
    procedure UnSubscribeOrders;
    { methods }

    { events }
  private
    FOnFTXError: TsgcWSFTXErrorEvent;
    FOnFTXMessage: TsgcWSFTXMessageEvent;
    FOnFTXSubscribed: TsgcWSFTXSubscribedEvent;
    FOnFTXUnsubscribed: TsgcWSFTXUnsubscribedEvent;
  protected
    procedure DoSubscribedEvent(const aRawMessage: string); virtual;
    procedure DoUnSubscribedEvent(const aRawMessage: string); virtual;
    procedure DoMessageEvent(const aType: string;
      const aRawMessage: string); virtual;
    procedure DoErrorEvent(const aRawMessage: string); virtual;
  public
    property OnFTXError: TsgcWSFTXErrorEvent read FOnFTXError write FOnFTXError;
    property OnFTXMessage: TsgcWSFTXMessageEvent read FOnFTXMessage
      write FOnFTXMessage;
    property OnFTXSubscribed: TsgcWSFTXSubscribedEvent read FOnFTXSubscribed
      write FOnFTXSubscribed;
    property OnFTXUnsubscribed: TsgcWSFTXUnsubscribedEvent
      read FOnFTXUnsubscribed write FOnFTXUnsubscribed;
    { events }

    { http events }
  private
    FOnFTXHTTPException: TsgcWSFTXHTTPExceptionEvent;
  protected
    procedure DoFTXHTTPExceptionEvent(E: Exception); virtual;
  public
    property OnFTXHTTPException: TsgcWSFTXHTTPExceptionEvent
      read FOnFTXHTTPException write FOnFTXHTTPException;
    { http events }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_APIS}

uses
{$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
{$IFDEF SGC_INDY}sgcIdUri{$ELSE}IdUri{$ENDIF},
  sgcBase_Helpers, sgcWebSocket_Types, sgcWebSocket_Helpers;

const
  CS_FTX_SUBSCRIBE = 'subscribe';
  CS_FTX_UNSUBSCRIBE = 'unsubscribe';
  CS_FTX_TICKER = 'ticker';
  CS_FTX_TRADES = 'trades';
  CS_FTX_ORDERBOOK = 'orderbook';
  CS_FTX_MARKETS = 'markets';
  CS_FTX_ORDERBOOK_GROUPED = 'orderbookGrouped';
const
  CS_FTX_FILLS = 'fills';
  CS_FTX_ORDERS = 'orders';

const
  CS_FTX_ERROR = 'error';
  CS_FTX_PONG = 'pong';
  CS_FTX_SUBSCRIBED = 'subscribed';
  CS_FTX_UNSUBSCRIBED = 'unsubscribed';

constructor TsgcWS_API_FTX.Create(aOwner: TComponent);
begin
  inherited;
  FFTX := TsgcWSFTX_Options.Create;
end;

destructor TsgcWS_API_FTX.Destroy;
begin
  sgcFree(FFTX);
  sgcFree(FREST_API);
  inherited;
end;

procedure TsgcWS_API_FTX.DoBeforeConnect;
begin
  inherited;
  if Assigned(FClient) then
  begin
    if not FClient.HeartBeat.Enabled then
    begin
      FClient.HeartBeat.Interval := 15;
      FClient.HeartBeat.Enabled := True;
    end;
  end;
end;

procedure TsgcWS_API_FTX.DoFTXHTTPExceptionEvent(E: Exception);
begin
  if Assigned(FOnFTXHTTPException) then
    FOnFTXHTTPException(self, E);
end;

procedure TsgcWS_API_FTX.DoErrorEvent(const aRawMessage: string);
begin
  if Assigned(FOnFTXError) then
    FOnFTXError(self, GetJSONCode, GetJSONMessage, aRawMessage);
end;

procedure TsgcWS_API_FTX.DoEventConnect(aConnection: TsgcWSConnection);
begin
  inherited;
  if (FTX.ApiKey <> '') and (FTX.ApiSecret <> '') then
    DoLogin;
end;

procedure TsgcWS_API_FTX.DoEventMessage(aConnection: TsgcWSConnection;
  const Text: string);
begin
  if RawMessages then
    inherited
  else
  begin
    JSON.Clear;
    JSON.Read(Text);
    if JSON.Node['type'] <> nil then
      DoReadJSONType(Text)
    else
      inherited;
  end;
end;

function TsgcWS_API_FTX.DoKeepAlive: Boolean;
begin
  Result := True;

  DoPing;
end;

procedure TsgcWS_API_FTX.DoLogin;
var
  oSecret: TsgcStringStream;
  vSignature: string;
  vTimestamp: string;
  vJSON: string;
begin
  vTimestamp := GetDateTimeUnix(Now, False) + '000';

  oSecret := TsgcStringStream.Create(FTX.ApiSecret);
  Try
    vSignature := GetHMACSHA256(vTimestamp + 'websocket_login',
      TIdBytes(oSecret.Bytes), False);

    if FTX.SubAccount <> '' then
      vJSON := Format
        ('{"args": {"key": "%s", "sign": "%s", "time": %s, "subaccount": "%s"}, "op": "login"}',
        [FTX.ApiKey, vSignature, vTimestamp, TIdURI.ParamsEncode(FTX.SubAccount)])
    else
      vJSON := Format
        ('{"args": {"key": "%s", "sign": "%s", "time": %s}, "op": "login"}',
        [FTX.ApiKey, vSignature, vTimestamp]);

    DoWriteData(vJSON);
  Finally
    sgcFree(oSecret);
  end;
end;

procedure TsgcWS_API_FTX.DoMessageEvent(const aType: string;
  const aRawMessage: string);
begin
  if Assigned(FOnFTXMessage) then
    FOnFTXMessage(self, aType, aRawMessage);
end;

procedure TsgcWS_API_FTX.DoPing;
begin
  DoWriteData('{"op": "ping"}');
end;

procedure TsgcWS_API_FTX.DoReadJSONType(aRawMessage: string);
var
  vType: string;
begin
  vType := JSON.Node['type'].Value;

  if vType = CS_FTX_SUBSCRIBED then
    DoSubscribedEvent(aRawMessage)
  else if vType = CS_FTX_UNSUBSCRIBED then
    DoUnSubscribedEvent(aRawMessage)
  else if vType = CS_FTX_ERROR then
    DoErrorEvent(aRawMessage)
  else if vType = CS_FTX_PONG then
  begin
    // nothing
  end
  else
    DoMessageEvent(vType, aRawMessage);
end;

procedure TsgcWS_API_FTX.DoRequest(aRequest: TsgcWSFTXRequest;
  aChannel: TsgcWSFTXChannels; const aMarket: String = '';
  aGrouping: Integer = 0; aAuthenticate: Boolean = False);
var
  vMethod: string;
  vJSON: string;
begin
  case aRequest of
    ftrSubscribe:
      vMethod := CS_FTX_SUBSCRIBE;
    ftrUnsubscribe:
      vMethod := CS_FTX_UNSUBSCRIBE;
  end;

  if aMarket = '' then
    vJSON := Format('{"channel": "%s", "op": "%s"}',
      [GetChannel(aChannel), vMethod])
  else
  begin
    if aChannel = ftcOrderbookGrouped then
      vJSON := Format
        ('{"channel": "%s", "market": "%s", "op": "%s", "grouping": %d}',
        [GetChannel(aChannel), aMarket, vMethod, aGrouping])
    else
      vJSON := Format('{"channel": "%s", "market": "%s", "op": "%s"}',
        [GetChannel(aChannel), aMarket, vMethod]);
  end;

  DoWriteData(vJSON);
end;

procedure TsgcWS_API_FTX.DoSubscribedEvent(const aRawMessage: string);
begin
  if Assigned(FOnFTXSubscribed) then
    FOnFTXSubscribed(self, GetJSONChannel, GetJSONMarket, aRawMessage);
end;

procedure TsgcWS_API_FTX.DoUnSubscribedEvent(const aRawMessage: string);
begin
  if Assigned(FOnFTXUnsubscribed) then
    FOnFTXUnsubscribed(self, GetJSONChannel, GetJSONMarket, aRawMessage);
end;

procedure TsgcWS_API_FTX.DoWriteData(const aText: string);
begin
  if Assigned(FClient) then
    FClient.WriteData(aText);
end;

function TsgcWS_API_FTX.GetChannel(aChannel: TsgcWSFTXChannels): string;
begin
  case aChannel of
    ftcTicker:
      Result := CS_FTX_TICKER;
    ftcMarkets:
      Result := CS_FTX_MARKETS;
    ftcTrades:
      Result := CS_FTX_TRADES;
    ftcOrderbook:
      Result := CS_FTX_ORDERBOOK;
    ftcOrderbookGrouped:
      Result := CS_FTX_ORDERBOOK_GROUPED;
    ftcFills:
      Result := CS_FTX_FILLS;
    ftcOrders:
      Result := CS_FTX_ORDERS;
  end;
end;

function TsgcWS_API_FTX.GetJSON: TsgcJSON;
begin
  if not Assigned(FJSON) then
    FJSON := TsgcJSON.Create(self);
  Result := FJSON;
end;

function TsgcWS_API_FTX.GetJSONChannel: string;
begin
  Result := '';
  if JSON.Node['channel'] <> nil then
    Result := JSON.Node['channel'].Value;
end;

function TsgcWS_API_FTX.GetJSONCode: string;
begin
  Result := '';
  if JSON.Node['code'] <> nil then
    Result := JSON.Node['code'].Value;
end;

function TsgcWS_API_FTX.GetJSONMessage: string;
begin
  Result := '';
  if JSON.Node['msg'] <> nil then
    Result := JSON.Node['msg'].Value;
end;

function TsgcWS_API_FTX.GetJSONMarket: string;
begin
  Result := '';
  if JSON.Node['market'] <> nil then
    Result := JSON.Node['market'].Value;
end;

function TsgcWS_API_FTX.GetREST_API: TsgcHTTP_API_FTX_Rest;
begin
  if not Assigned(FREST_API) then
  begin
    FREST_API := TsgcHTTP_API_FTX_Rest.Create(nil);
    if Assigned(FCLient) then
    begin
      FREST_API.TLSOptions.OpenSSL_Options.APIVersion :=
        FCLient.TLSOptions.OpenSSL_Options.APIVersion;
      FREST_API.TLSOptions.IOHandler := FClient.TLSOptions.IOHandler;
      FREST_API.TLSOptions.Version := FClient.TLSOptions.Version;
    end;
  end;

  if Assigned(FOnFTXHTTPException) then
    FREST_API.OnHTTPAPIException := OnHTTPAPIExceptionEvent
  else
    FREST_API.OnHTTPAPIException := nil;
  FREST_API.FTXOptions.ApiKey := FTX.ApiKey;
  FREST_API.FTXOptions.ApiSecret := FTX.ApiSecret;
  FREST_API.FTXOptions.SubAccount := FTX.SubAccount;
  if FREST_API.FTXOptions.LogOptions.Enabled <> FTX.HTTPLogOptions.Enabled then
  begin
    FREST_API.FTXOptions.LogOptions.Enabled := FTX.HTTPLogOptions.Enabled;
    FREST_API.FTXOptions.LogOptions.FileName := FTX.HTTPLogOptions.FileName;
  end;
  Result := FREST_API;
end;

function TsgcWS_API_FTX.GetURL: String;
begin
  Result := 'wss://ftx.com/ws';

  if Assigned(Client) then
    Client.TLSOptions.Version := tls1_2;
end;

procedure TsgcWS_API_FTX.OnHTTPAPIExceptionEvent(Sender: TObject; E: Exception);
begin
  DoFTXHTTPExceptionEvent(E);
end;

procedure TsgcWS_API_FTX.Ping;
begin
  DoPing;
end;

procedure TsgcWS_API_FTX.SetFTX(const Value: TsgcWSFTX_Options);
begin
  if Assigned(FFTX) then
    FFTX.Assign(Value);
end;

procedure TsgcWS_API_FTX.SubscribeFills;
begin
  DoRequest(ftrSubscribe, ftcFills);
end;

procedure TsgcWS_API_FTX.SubscribeGroupedOrderbooks(const aMarket: string;
  aGrouping: Integer = 500);
begin
  DoRequest(ftrSubscribe, ftcOrderbookGrouped, aMarket, aGrouping);
end;

procedure TsgcWS_API_FTX.SubscribeMarkets;
begin
  DoRequest(ftrSubscribe, ftcMarkets);
end;

procedure TsgcWS_API_FTX.SubscribeOrderbooks(const aMarket: string);
begin
  DoRequest(ftrSubscribe, ftcOrderbook, aMarket);
end;

procedure TsgcWS_API_FTX.SubscribeOrders;
begin
  DoRequest(ftrSubscribe, ftcOrders);
end;

procedure TsgcWS_API_FTX.SubscribeTicker(const aMarket: string);
begin
  DoRequest(ftrSubscribe, ftcTicker, aMarket);
end;

procedure TsgcWS_API_FTX.SubscribeTrades(const aMarket: string);
begin
  DoRequest(ftrSubscribe, ftcTrades, aMarket);
end;

procedure TsgcWS_API_FTX.UnSubscribeFills;
begin
  DoRequest(ftrUnSubscribe, ftcFills);
end;

procedure TsgcWS_API_FTX.UnSubscribeGroupedOrderbooks(const aMarket: string;
  aGrouping: Integer = 500);
begin
  DoRequest(ftrUnsubscribe, ftcOrderbookGrouped, aMarket, aGrouping);
end;

procedure TsgcWS_API_FTX.UnSubscribeMarkets;
begin
  DoRequest(ftrUnsubscribe, ftcMarkets);
end;

procedure TsgcWS_API_FTX.UnSubscribeOrderbooks(const aMarket: string);
begin
  DoRequest(ftrUnsubscribe, ftcOrderbook, aMarket);
end;

procedure TsgcWS_API_FTX.UnSubscribeOrders;
begin
  DoRequest(ftrUnSubscribe, ftcOrders);
end;

procedure TsgcWS_API_FTX.UnSubscribeTicker(const aMarket: string);
begin
  DoRequest(ftrUnsubscribe, ftcTicker, aMarket);
end;

procedure TsgcWS_API_FTX.UnSubscribeTrades(const aMarket: string);
begin
  DoRequest(ftrUnsubscribe, ftcTrades, aMarket);
end;

constructor TsgcWSFTX_Options.Create;
begin
  inherited;
  FHTTPLogOptions := TsgcWSFTXLog_Options.Create;
end;

destructor TsgcWSFTX_Options.Destroy;
begin
  sgcFree(FHTTPLogOptions);
  inherited;
end;

procedure TsgcWSFTX_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSFTX_Options then
  begin
    ApiKey := TsgcWSFTX_Options(aSource).ApiKey;
    ApiSecret := TsgcWSFTX_Options(aSource).ApiSecret;
    SubAccount := TsgcWSFTX_Options(aSource).SubAccount;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSFTX_Options.SetHTTPLogOptions(const Value
  : TsgcWSFTXLog_Options);
begin
  if Assigned(FHTTPLogOptions) then
    FHTTPLogOptions.Assign(Value);
end;

procedure TsgcWSFTXLog_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSFTXLog_Options then
  begin
    Enabled := TsgcWSFTXLog_Options(aSource).Enabled;
    FileName := TsgcWSFTXLog_Options(aSource).FileName;
  end
  else
    inherited Assign(aSource);
end;

{$ENDIF}

end.
