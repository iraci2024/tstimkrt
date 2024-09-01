{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_API_Bitmex;

interface

{$I sgcVer.inc}
{$IFDEF SGC_APIS}

uses
  Classes, SysUtils,
  // sgc
  sgcWebSocket_Classes, sgcJSON, sgcHTTP_API_Bitmex;

Type
  TwsBitmexTopics = (btmAnnouncement, btmChat, btmConnected, btmFunding,
    btmInstrument, btmInsurance, btmLiquidation, btmOrderBookL2_25,
    btmOrderBookL2, btmOrderBook10, btmPublicNotifications, btmQuote,
    btmQuoteBin1m, btmQuoteBin5m, btmQuoteBin1h, btmQuoteBin1d, btmSettlement,
    btmTrade, btmTradeBin1m, btmTradeBin5m, btmTradeBin1h, btmTradeBin1d,
    // authenticate
    btmAffiliate, btmExecution, btmOrder, btmMargin, btmPosition,
    btmPrivateNotifications, btmTransact, btmWallet);

Type
  TsgcWSBitmexConnectEvent = procedure(Sender: TObject; const aMessage: String)
    of object;
  TsgcWSBitmexSubscribedEvent = procedure(Sender: TObject;
    const aChannel: String) of object;
  TsgcWSBitmexUnsubscribedEvent = procedure(Sender: TObject;
    const aChannel: String) of object;
  TsgcWSBitmexMessageEvent = procedure(Sender: TObject;
    const aTopic: TwsBitmexTopics; const aMessage: String) of object;
  TsgcWSBitmexErrorEvent = procedure(Sender: TObject; const aMessage: String)
    of object;
  TsgcWSBitmexAuthenticatedEvent = procedure(Sender: TObject) of object;
  TsgcWSBitmexHTTPExceptionEvent = procedure(Sender: TObject; E: Exception)
    of object;

const
  CS_BITMEX_ANNOUNCEMENT = 'announcement';
  CS_BITMEX_CHAT = 'chat';
  CS_BITMEX_CONNECTED = 'connected';
  CS_BITMEX_FUNDING = 'funding';
  CS_BITMEX_INSTRUMENT = 'instrument';
  CS_BITMEX_INSURANCE = 'insurance';
  CS_BITMEX_LIQUIDATION = 'liquidation';
  CS_BITMEX_ORDERBOOKL2_25 = 'orderBookL2_25';
  CS_BITMEX_ORDERBOOKL2 = 'orderBookL2';
  CS_BITMEX_ORDERBOOK10 = 'orderBook10';
  CS_BITMEX_PUBLICNOTIFICATIONS = 'publicNotifications';
  CS_BITMEX_QUOTE = 'quote';
  CS_BITMEX_QUOTEBIN1M = 'quoteBin1m';
  CS_BITMEX_QUOTEBIN5M = 'quoteBin5m';
  CS_BITMEX_QUOTEBIN1H = 'quoteBin1h';
  CS_BITMEX_QUOTEBIN1D = 'quoteBin1d';
  CS_BITMEX_SETTLEMENT = 'settlement';
  CS_BITMEX_TRADE = 'trade';
  CS_BITMEX_TRADEBIN1M = 'tradeBin1m';
  CS_BITMEX_TRADEBIN5M = 'tradeBin5m';
  CS_BITMEX_TRADEBIN1H = 'tradeBin1h';
  CS_BITMEX_TRADEBIN1D = 'tradeBin1d';
  CS_BITMEX_AFFILIATE = 'affiliate';
  CS_BITMEX_EXECUTION = 'execution';
  CS_BITMEX_ORDER = 'order';
  CS_BITMEX_MARGIN = 'margin';
  CS_BITMEX_POSITION = 'position';
  CS_BITMEX_PRIVATENOTIFICATIONS = 'privateNotifications';
  CS_BITMEX_TRANSACT = 'transact';
  CS_BITMEX_WALLET = 'wallet';

var
  TwsBitmexTopics_String: Array [0 .. 29] of String = (
    CS_BITMEX_ANNOUNCEMENT,
    CS_BITMEX_CHAT,
    CS_BITMEX_CONNECTED,
    CS_BITMEX_FUNDING,
    CS_BITMEX_INSTRUMENT,
    CS_BITMEX_INSURANCE,
    CS_BITMEX_LIQUIDATION,
    CS_BITMEX_ORDERBOOKL2_25,
    CS_BITMEX_ORDERBOOKL2,
    CS_BITMEX_ORDERBOOK10,
    CS_BITMEX_PUBLICNOTIFICATIONS,
    CS_BITMEX_QUOTE,
    CS_BITMEX_QUOTEBIN1M,
    CS_BITMEX_QUOTEBIN5M,
    CS_BITMEX_QUOTEBIN1H,
    CS_BITMEX_QUOTEBIN1D,
    CS_BITMEX_SETTLEMENT,
    CS_BITMEX_TRADE,
    CS_BITMEX_TRADEBIN1M,
    CS_BITMEX_TRADEBIN5M,
    CS_BITMEX_TRADEBIN1H,
    CS_BITMEX_TRADEBIN1D,
    // authenticate
    CS_BITMEX_AFFILIATE,
    CS_BITMEX_EXECUTION,
    CS_BITMEX_ORDER,
    CS_BITMEX_MARGIN,
    CS_BITMEX_POSITION,
    CS_BITMEX_PRIVATENOTIFICATIONS,
    CS_BITMEX_TRANSACT,
    CS_BITMEX_WALLET
  );

Type
  TsgcWSBitmexLog_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FFileName: String;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property FileName: String read FFileName write FFileName;
  end;

  TsgcWSBitmex_Options = class(TPersistent)
  private
    FApiKey: String;
    FApiSecret: String;
    FHTTPLogOptions: TsgcWSBitmexLog_Options;
    FTestNet: Boolean;
    procedure SetHTTPLogOptions(const Value: TsgcWSBitmexLog_Options);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property ApiKey: String read FApiKey write FApiKey;
    property ApiSecret: String read FApiSecret write FApiSecret;
    property HTTPLogOptions: TsgcWSBitmexLog_Options read FHTTPLogOptions write
        SetHTTPLogOptions;
    property TestNet: Boolean read FTestNet write FTestNet;
  end;

  TsgcWS_API_Bitmex = class(TsgcWSAPI_client)
    { from TsgcWSAPI_Client }
  protected
    function GetURL: String; override;
    { from TsgcWSAPI_Client }

    { from TsgcWSComponent_Base }
  protected
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
    property JSON: TsgcJSON read GetJSON write FJSON;
    { JSON }

    { properties }
  private
    FBitmex: TsgcWSBitmex_Options;
    FREST_API: TsgcHTTP_API_Bitmex_Rest;
    procedure SetBitmex(const Value: TsgcWSBitmex_Options);
    function GetREST_API: TsgcHTTP_API_Bitmex_Rest;
  protected
    procedure OnHTTPAPIExceptionEvent(Sender: TObject; E: Exception); virtual;
  public
    property Bitmex: TsgcWSBitmex_Options read FBitmex write SetBitmex;
    property REST_API: TsgcHTTP_API_Bitmex_Rest read GetREST_API write FREST_API;
    { properties }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { methods }
  private
    function GetTimeStamp: Integer;
  private
    function IsSymbolTopic(aTopic: TwsBitmexTopics): Boolean;
  private
    function GetTopicString(aTopic: TwsBitmexTopics): String;
    function GetTopicFromString(aTopic: String): TwsBitmexTopics;
  protected
    procedure DoWriteData(const aText: String); virtual;
    procedure DoPing; virtual;
  protected
    procedure DoRequest(const aMethod: String; aTopic: TwsBitmexTopics;
      const aSymbol: String); virtual;
  public
    procedure Subscribe(aTopic: TwsBitmexTopics; const aSymbol: String = '');
    procedure Unsubscribe(aTopic: TwsBitmexTopics; const aSymbol: String = '');
  public
    procedure Authenticate;
    { methods }

    { events }
  private
    FOnBitmexAuthenticated: TsgcWSBitmexAuthenticatedEvent;
    FOnBitmexConnect: TsgcWSBitmexConnectEvent;
    FOnBitmexError: TsgcWSBitmexErrorEvent;
    FOnBitmexMessage: TsgcWSBitmexMessageEvent;
    FOnBitmexSubscribed: TsgcWSBitmexSubscribedEvent;
    FOnBitmexUnsubscribed: TsgcWSBitmexUnsubscribedEvent;
  protected
    procedure DoBitmexConnectEvent(const aMessage: String); virtual;
    procedure DoBitmexSubscribedEvent(const aChannel: String); virtual;
    procedure DoBitmexUnsubscribedEvent(const aChannel: String); virtual;
    procedure DoBitmexMessageEvent(const aTopic: String;
      const aMessage: String); virtual;
    procedure DoBitmexErrorEvent(const aMessage: String); virtual;
    procedure DoBitmexAuthenticatedEvent; virtual;
  public
    property OnBitmexAuthenticated: TsgcWSBitmexAuthenticatedEvent
      read FOnBitmexAuthenticated write FOnBitmexAuthenticated;
    property OnBitmexConnect: TsgcWSBitmexConnectEvent read FOnBitmexConnect
      write FOnBitmexConnect;
    property OnBitmexError: TsgcWSBitmexErrorEvent read FOnBitmexError
      write FOnBitmexError;
    property OnBitmexMessage: TsgcWSBitmexMessageEvent read FOnBitmexMessage
      write FOnBitmexMessage;
    property OnBitmexSubscribed: TsgcWSBitmexSubscribedEvent
      read FOnBitmexSubscribed write FOnBitmexSubscribed;
    property OnBitmexUnsubscribed: TsgcWSBitmexUnsubscribedEvent
      read FOnBitmexUnsubscribed write FOnBitmexUnsubscribed;
    { events }

    { http events }
  private
    FOnBitmexHTTPException: TsgcWSBitmexHTTPExceptionEvent;
  protected
    procedure DoBitmexHTTPExceptionEvent(E: Exception); virtual;
  public
    property OnBitmexHTTPException: TsgcWSBitmexHTTPExceptionEvent
      read FOnBitmexHTTPException write FOnBitmexHTTPException;
    { http events }
  end;


{$ENDIF}

implementation

{$IFDEF SGC_APIS}

uses
  DateUtils,
  // sgc
  sgcBase_Helpers, sgcWebSocket_Helpers;

Const
  CS_AUTH_KEY_EXPIRES = 'authKeyExpires';

constructor TsgcWS_API_Bitmex.Create(aOwner: TComponent);
begin
  inherited;
  FBitmex := TsgcWSBitmex_Options.Create;
end;

destructor TsgcWS_API_Bitmex.Destroy;
begin
  sgcFree(FBitmex);
  sgcFree(FREST_API);
  inherited;
end;

procedure TsgcWS_API_Bitmex.Authenticate;
var
  oJSON: TsgcJSON;
  vTimestamp: Integer;
begin
  vTimestamp := GetTimeStamp;

  oJSON := TsgcJSON.Create(nil);
  Try
    // ... method
    oJSON.AddPair('op', CS_AUTH_KEY_EXPIRES);
    // ... arguments
    oJSON.AddArray('args', '');
    oJSON.Node['args'].JSONObject.AddPair('0', Bitmex.ApiKey);
    oJSON.Node['args'].JSONObject.AddPair('1', vTimestamp);
    oJSON.Node['args'].JSONObject.AddPair('2',
      GetHMACSHA256('GET/realtime' + IntToStr(vTimestamp), Bitmex.ApiSecret));
    // ... write data
    DoWriteData(oJSON.Text);
  Finally
    sgcFree(oJSON);
  End;
end;

procedure TsgcWS_API_Bitmex.DoBitmexAuthenticatedEvent;
begin
  if Assigned(FOnBitmexAuthenticated) then
    FOnBitmexAuthenticated(self);
end;

procedure TsgcWS_API_Bitmex.DoBitmexConnectEvent(const aMessage: String);
begin
  if Assigned(FOnBitmexConnect) then
    FOnBitmexConnect(self, aMessage);
end;

procedure TsgcWS_API_Bitmex.DoBitmexErrorEvent(const aMessage: String);
begin
  if Assigned(FOnBitmexError) then
    FOnBitmexError(self, aMessage);
end;

procedure TsgcWS_API_Bitmex.DoBitmexHTTPExceptionEvent(E: Exception);
begin
  if Assigned(FOnBitmexHTTPException) then
    FOnBitmexHTTPException(self, E);
end;

procedure TsgcWS_API_Bitmex.DoBitmexMessageEvent(const aTopic: String;
  const aMessage: String);
begin
  if Assigned(FOnBitmexMessage) then
    FOnBitmexMessage(self, GetTopicFromString(aTopic), aMessage);
end;

procedure TsgcWS_API_Bitmex.DoBitmexSubscribedEvent(const aChannel: String);
begin
  if Assigned(FOnBitmexSubscribed) then
    FOnBitmexSubscribed(self, aChannel);
end;

procedure TsgcWS_API_Bitmex.DoBitmexUnsubscribedEvent(const aChannel: String);
begin
  if Assigned(FOnBitmexUnsubscribed) then
    FOnBitmexUnsubscribed(self, aChannel);
end;

procedure TsgcWS_API_Bitmex.DoEventMessage(aConnection: TsgcWSConnection;
  const Text: string);
begin
  if RawMessages then
    inherited
  else
  begin
    if Text = 'pong' then
      exit;

    JSON.Clear;
    JSON.Read(Text);

    if JSON.Node['table'] <> nil then
      DoBitmexMessageEvent(JSON.Node['table'].Value, Text)
    else if JSON.Node['subscribe'] <> nil then
    begin
      if JSON.Node['success'].Value = 'true' then
        DoBitmexSubscribedEvent(JSON.Node['subscribe'].Value)
    end
    else if JSON.Node['unsubscribe'] <> nil then
    begin
      if JSON.Node['success'].Value = 'true' then
        DoBitmexUnsubscribedEvent(JSON.Node['unsubscribe'].Value)
    end
    else if JSON.Node['error'] <> nil then
      DoBitmexErrorEvent(JSON.Node['error'].Value)
    else if JSON.Node['info'] <> nil then
      DoBitmexConnectEvent(Text)
    else if JSON.Node['request'] <> nil then
    begin
      if JSON.Node['success'].Value = 'true' then
      begin
        if JSON.Node['request'].JSONObject.Node['op'] <> nil then
        begin
          if JSON.Node['request'].JSONObject.Node['op'].Value = CS_AUTH_KEY_EXPIRES
          then
            DoBitmexAuthenticatedEvent;
        end
      end
    end
    else
      inherited;
  end;
end;

function TsgcWS_API_Bitmex.DoKeepAlive: Boolean;
begin
  Result := True;

  DoPing;
end;

procedure TsgcWS_API_Bitmex.DoPing;
begin
  DoWriteData('ping');
end;

procedure TsgcWS_API_Bitmex.DoRequest(const aMethod: String;
  aTopic: TwsBitmexTopics; const aSymbol: String);
var
  oJSON: TsgcJSON;
  vTopic: string;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    // ... write message
    oJSON.AddPair('op', aMethod);
    // ... arguments
    vTopic := GetTopicString(aTopic);
    if (aSymbol <> '') and IsSymbolTopic(aTopic) then
      vTopic := vTopic + ':' + aSymbol;
    oJSON.AddArray('args', '');
    oJSON.Node['args'].JSONObject.AddPair('0', vTopic);

    // ... write data
    DoWriteData(oJSON.Text);
  Finally
    sgcFree(oJSON);
  End;
end;

procedure TsgcWS_API_Bitmex.DoWriteData(const aText: String);
begin
  if Assigned(FClient) then
    FClient.WriteData(aText);
end;

function TsgcWS_API_Bitmex.GetJSON: TsgcJSON;
begin
  if not Assigned(FJSON) then
    FJSON := TsgcJSON.Create(self);
  Result := FJSON;
end;

function TsgcWS_API_Bitmex.GetREST_API: TsgcHTTP_API_Bitmex_Rest;
begin
  if not Assigned(FREST_API) then
  begin
    FREST_API := TsgcHTTP_API_Bitmex_Rest.Create(nil);
    if Assigned(FCLient) then
    begin
      FREST_API.TLSOptions.OpenSSL_Options.APIVersion :=
        FCLient.TLSOptions.OpenSSL_Options.APIVersion;
      FREST_API.TLSOptions.IOHandler := FClient.TLSOptions.IOHandler;
      FREST_API.TLSOptions.Version := FClient.TLSOptions.Version;
    end;
  end;

  if Assigned(FOnBitmexHTTPException) then
    FREST_API.OnHTTPAPIException := OnHTTPAPIExceptionEvent
  else
    FREST_API.OnHTTPAPIException := nil;
  FREST_API.BitmexOptions.ApiKey := Bitmex.ApiKey;
  FREST_API.BitmexOptions.ApiSecret := Bitmex.ApiSecret;
  FREST_API.BitmexOptions.TestNet := Bitmex.TestNet;
  if FREST_API.BitmexOptions.LogOptions.Enabled <> Bitmex.HTTPLogOptions.Enabled
  then
  begin
    FREST_API.BitmexOptions.LogOptions.Enabled :=
      Bitmex.HTTPLogOptions.Enabled;
    FREST_API.BitmexOptions.LogOptions.FileName :=
      Bitmex.HTTPLogOptions.FileName;
  end;
  Result := FREST_API;
end;

function TsgcWS_API_Bitmex.GetTimeStamp: Integer;
begin
  {$IFDEF LAZARUS}
  Result := DateTimeToUnix(LocalTimeToUniversal(Now)) + 10;
  {$ELSE}
  Result := DateTimeToUnix(Now{$IFDEF DXE6}, False{$ENDIF}) + 10;
  {$ENDIF}
end;

function TsgcWS_API_Bitmex.GetTopicFromString(aTopic: String): TwsBitmexTopics;
var
  i: Integer;
begin
  Result := TwsBitmexTopics(0);

  for i := 0 to Length(TwsBitmexTopics_String) - 1 do
  begin
    if TwsBitmexTopics_String[i] = aTopic then
    begin
      Result := TwsBitmexTopics(i);
      break;
    end;
  end;
end;

function TsgcWS_API_Bitmex.GetTopicString(aTopic: TwsBitmexTopics): String;
begin
  Result := TwsBitmexTopics_String[Ord(aTopic)];
end;

function TsgcWS_API_Bitmex.GetURL: String;
begin
  if Bitmex.TestNet then
    Result := 'wss://testnet.bitmex.com/realtime'
  else
    Result := 'wss://www.bitmex.com/realtime';
end;

function TsgcWS_API_Bitmex.IsSymbolTopic(aTopic: TwsBitmexTopics): Boolean;
begin
  case aTopic of
    btmFunding, btmInstrument, btmLiquidation, btmOrderBookL2_25,
      btmOrderBookL2, btmOrderBook10, btmQuote, btmQuoteBin1m, btmQuoteBin5m,
      btmQuoteBin1h, btmQuoteBin1d, btmSettlement, btmTrade, btmTradeBin1m,
      btmTradeBin5m, btmTradeBin1h, btmTradeBin1d:
      Result := True
  else
    Result := False
  end;
end;

procedure TsgcWS_API_Bitmex.OnHTTPAPIExceptionEvent(Sender: TObject; E:
    Exception);
begin
  DoBitmexHTTPExceptionEvent(E);
end;

procedure TsgcWS_API_Bitmex.SetBitmex(const Value: TsgcWSBitmex_Options);
begin
  FBitmex.Assign(Value);
end;

procedure TsgcWS_API_Bitmex.Subscribe(aTopic: TwsBitmexTopics;
  const aSymbol: String = '');
begin
  DoRequest('subscribe', aTopic, aSymbol);
end;

procedure TsgcWS_API_Bitmex.Unsubscribe(aTopic: TwsBitmexTopics;
  const aSymbol: String = '');
begin
  DoRequest('unsubscribe', aTopic, aSymbol);
end;

constructor TsgcWSBitmex_Options.Create;
begin
  inherited;
  FHTTPLogOptions := TsgcWSBitmexLog_Options.Create;
end;

destructor TsgcWSBitmex_Options.Destroy;
begin
  sgcFree(FHTTPLogOptions);
  inherited;
end;

procedure TsgcWSBitmex_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSBitmex_Options then
  begin
    ApiKey := TsgcWSBitmex_Options(aSource).ApiKey;
    ApiSecret := TsgcWSBitmex_Options(aSource).ApiSecret;
    TestNet := TsgcWSBitmex_Options(aSource).TestNet;
    HTTPLogOptions := TsgcWSBitmex_Options(aSource).HTTPLogOptions;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSBitmex_Options.SetHTTPLogOptions(const Value:
    TsgcWSBitmexLog_Options);
begin
  if Assigned(FHTTPLogOptions) then
    FHTTPLogOptions.Assign(Value);
end;

procedure TsgcWSBitmexLog_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSBitmexLog_Options then
  begin
    Enabled := TsgcWSBitmexLog_Options(aSource).Enabled;
    FileName := TsgcWSBitmexLog_Options(aSource).FileName;
  end
  else
    inherited Assign(aSource);
end;

{$ENDIF}

end.
