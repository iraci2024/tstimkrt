{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_API_Coinbase;

interface

{$I sgcVer.inc}
{$IFDEF SGC_APIS}

uses
  Classes, SysUtils, StrUtils,
  // sgc
  sgcWebSocket_Classes, sgcJSON, sgcHTTP_API_Coinbase;

type
  TsgcWSCoinbaseSubscriptionsEvent = procedure(Sender: TObject;
    aChannels: string; aProductIds: string; aRawMessage: string) of object;
  TsgcWSCoinbaseMessageEvent = procedure(Sender: TObject; aType: string;
    aRawMessage: string) of object;
  TsgcWSCoinbaseErrorEvent = procedure(Sender: TObject;
    aError, aReason, aRawMessage: string) of object;
  TsgcWSCoinbaseHTTPExceptionEvent = procedure(Sender: TObject; E: Exception)
    of object;


  TsgcWSCoinbaseRequest = (cbrSubscribe, cbrUnsubscribe);
  TsgcWSCoinbaseChannels = (cbcHeartBeat, cbcStatus, cbcTicker, cbcLevel2,
    cbcMatches, cbcFull, cbcUser);

  TsgcWSCoinbaseLog_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FFileName: String;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property FileName: String read FFileName write FFileName;
  end;

  TsgcWSCoinbase_Options = class(TPersistent)
  private
    FApiKey: String;
    FApiSecret: String;
    FHTTPLogOptions: TsgcWSCoinbaseLog_Options;
    FPassphrase: string;
    FSandBox: Boolean;
    procedure SetHTTPLogOptions(const Value: TsgcWSCoinbaseLog_Options);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property ApiKey: String read FApiKey write FApiKey;
    property ApiSecret: String read FApiSecret write FApiSecret;
    property HTTPLogOptions: TsgcWSCoinbaseLog_Options read FHTTPLogOptions write
        SetHTTPLogOptions;
    property Passphrase: string read FPassphrase write FPassphrase;
    property SandBox: Boolean read FSandBox write FSandBox;
  end;

  TsgcWS_API_Coinbase = class(TsgcWSAPI_client)
    { from TsgcWSAPI_Client }
  protected
    function GetURL: String; override;
    { from TsgcWSAPI_Client }

    { from TsgcWSComponent_Base }
  protected
    procedure DoBeforeConnect; override;
  protected
    procedure DoEventMessage(aConnection: TsgcWSConnection;
      const Text: string); override;
    { from TsgcWSComponent_Base }

    { JSON }
  private
    FJSON: TsgcJSON;
    function GetJSON: TsgcJSON;
  protected
    function GetJSONChannels: string;
    function GetJSONProductIds: string;
    function GetJSONError: string;
    function GetJSONErrorReason: string;
  protected
    procedure DoReadJSONType(aRawMessage: string); virtual;
  protected
    property JSON: TsgcJSON read GetJSON write FJSON;
    { JSON }

    { options }
  private
    FCoinbase: TsgcWSCoinbase_Options;
    FREST_API: TsgcHTTP_API_Coinbase_Rest;
    procedure SetCoinbase(const Value: TsgcWSCoinbase_Options);
    function GetREST_API: TsgcHTTP_API_Coinbase_Rest;
  protected
    procedure OnHTTPAPIExceptionEvent(Sender: TObject; E: Exception); virtual;
  public
    property Coinbase: TsgcWSCoinbase_Options read FCoinbase write SetCoinbase;
    property REST_API: TsgcHTTP_API_Coinbase_Rest read GetREST_API write FREST_API;
    { options }

    { constructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor }

    { methods }
  private
    function GetChannel(aChannel: TsgcWSCoinbaseChannels): string;
  protected
    procedure DoWriteData(const aText: string); virtual;
  protected
    procedure DoRequest(aRequest: TsgcWSCoinbaseRequest;
      aChannel: TsgcWSCoinbaseChannels; const aProductId: String = '';
      aAuthenticate: Boolean = False); virtual;
  public
    procedure SubscribeHeartBeat(const aProductId: String;
      aAuthenticate: Boolean = False);
    procedure UnSubscribeHeartBeat(const aProductId: String);
  public
    procedure SubscribeStatus(aAuthenticate: Boolean = False);
    procedure UnSubscribeStatus;
  public
    procedure SubscribeTicker(const aProductId: string;
      aAuthenticate: Boolean = False);
    procedure UnSubscribeTicker(const aProductId: string);
  public
    procedure SubscribeLevel2(const aProductId: string;
      aAuthenticate: Boolean = False);
    procedure UnSubscribeLevel2(const aProductId: string);
  public
    procedure SubscribeMatches(const aProductId: string;
      aAuthenticate: Boolean = False);
    procedure UnSubscribeMatches(const aProductId: string);
  public
    procedure SubscribeFull(const aProductId: string;
      aAuthenticate: Boolean = False);
    procedure UnSubscribeFull(const aProductId: string);
  public
    procedure SubscribeUser(const aProductId: string);
    procedure UnSubscribeUser(const aProductId: string);
    { methods }

    { events }
  private
    FOnCoinbaseError: TsgcWSCoinbaseErrorEvent;
    FOnCoinbaseMessage: TsgcWSCoinbaseMessageEvent;
    FOnCoinbaseSubscriptions: TsgcWSCoinbaseSubscriptionsEvent;
  protected
    procedure DoSubscriptionsEvent(const aRawMessage: string); virtual;
    procedure DoMessageEvent(const aType: string;
      const aRawMessage: string); virtual;
    procedure DoErrorEvent(const aRawMessage: string); virtual;
  public
    property OnCoinbaseError: TsgcWSCoinbaseErrorEvent read FOnCoinbaseError
      write FOnCoinbaseError;
    property OnCoinbaseMessage: TsgcWSCoinbaseMessageEvent
      read FOnCoinbaseMessage write FOnCoinbaseMessage;
    property OnCoinbaseSubscriptions: TsgcWSCoinbaseSubscriptionsEvent
      read FOnCoinbaseSubscriptions write FOnCoinbaseSubscriptions;
    { events }

    { http events }
  private
    FOnCoinbaseHTTPException: TsgcWSCoinbaseHTTPExceptionEvent;
  protected
    procedure DoCoinbaseHTTPExceptionEvent(E: Exception); virtual;
  public
    property OnCoinbaseHTTPException: TsgcWSCoinbaseHTTPExceptionEvent
      read FOnCoinbaseHTTPException write FOnCoinbaseHTTPException;
    { http events }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_APIS}

uses
{$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
  sgcBase_Helpers, sgcWebSocket_Types, sgcWebSocket_Helpers;

const
  CS_COINBASE_SUBSCRIBE = 'subscribe';
  CS_COINBASE_SUBSCRIPTIONS = 'subscriptions';
  CS_COINBASE_UNSUBSCRIBE = 'unsubscribe';
  CS_COINBASE_HEARTBEAT = 'heartbeat';
  CS_COINBASE_STATUS = 'status';
  CS_COINBASE_TICKER = 'ticker';
  CS_COINBASE_LEVEL2 = 'level2';
  CS_COINBASE_MATCHES = 'matches';
  CS_COINBASE_FULL = 'full';
  CS_COINBASE_USER = 'user';
  CS_COINBASE_ERROR = 'error';

constructor TsgcWS_API_Coinbase.Create(aOwner: TComponent);
begin
  inherited;
  FCoinbase := TsgcWSCoinbase_Options.Create;
end;

destructor TsgcWS_API_Coinbase.Destroy;
begin
  sgcFree(FCoinbase);
  sgcFree(FREST_API);
  inherited;
end;

procedure TsgcWS_API_Coinbase.DoBeforeConnect;
begin
  inherited;
  if Assigned(FClient) then
  begin
    if not FClient.HeartBeat.Enabled then
    begin
      FClient.HeartBeat.Interval := 30;
      FClient.HeartBeat.Enabled := True;
    end;
  end;
end;

procedure TsgcWS_API_Coinbase.DoCoinbaseHTTPExceptionEvent(E: Exception);
begin
  if Assigned(FOnCoinbaseHTTPException) then
    FOnCoinbaseHTTPException(self, E);
end;

procedure TsgcWS_API_Coinbase.DoErrorEvent(const aRawMessage: string);
begin
  if Assigned(FOnCoinbaseError) then
    FOnCoinbaseError(self, GetJSONError, GetJSONErrorReason, aRawMessage);
end;

procedure TsgcWS_API_Coinbase.DoEventMessage(aConnection: TsgcWSConnection;
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

procedure TsgcWS_API_Coinbase.DoMessageEvent(const aType: string;
  const aRawMessage: string);
begin
  if Assigned(FOnCoinbaseMessage) then
    FOnCoinbaseMessage(self, aType, aRawMessage);
end;

procedure TsgcWS_API_Coinbase.DoReadJSONType(aRawMessage: string);
var
  vType: string;
begin
  vType := JSON.Node['type'].Value;

  if vType = CS_COINBASE_SUBSCRIPTIONS then
    DoSubscriptionsEvent(aRawMessage)
  else if vType = CS_COINBASE_ERROR then
    DoErrorEvent(aRawMessage)
  else
    DoMessageEvent(vType, aRawMessage);
end;

procedure TsgcWS_API_Coinbase.DoRequest(aRequest: TsgcWSCoinbaseRequest;
  aChannel: TsgcWSCoinbaseChannels; const aProductId: String = '';
  aAuthenticate: Boolean = False);
var
  oSecret: TsgcStringStream;
  vMethod: string;
  vJSON: string;
  vAuthenticate, vSignature, vTimestamp: string;
begin
  case aRequest of
    cbrSubscribe:
      vMethod := CS_COINBASE_SUBSCRIBE;
    cbrUnsubscribe:
      vMethod := CS_COINBASE_UNSUBSCRIBE;
  end;

  vAuthenticate := '';
  if aAuthenticate then
  begin
    vTimestamp := GetDateTimeUnix(Now, False);
    oSecret := TsgcStringStream.Create('');
    Try
      DecodeBase64(Coinbase.ApiSecret, TMemoryStream(oSecret));
      oSecret.Size := 64;
      vSignature := GetHMACSHA256(vTimestamp + 'GET/users/self/verify',
        TIdBytes(oSecret.Bytes), True);
    Finally
      sgcFree(oSecret);
    end;

    vAuthenticate :=
      Format(', "signature": "%s", "key": "%s", "passphrase": "%s", "timestamp": "%s"',
      [vSignature, Coinbase.ApiKey, Coinbase.Passphrase, vTimestamp]);
  end;

  case aChannel of
    cbcHeartBeat, cbcTicker, cbcLevel2, cbcMatches, cbcFull, cbcUser:
      vJSON := Format
        ('{"type": "%s", "channels":[{"name":"%s","product_ids":["%s"]}]%s}',
        [vMethod, GetChannel(aChannel), aProductId, vAuthenticate]);
    cbcStatus:
      vJSON := Format('{"type": "%s", "channels":["%s"]%s}',
        [vMethod, GetChannel(aChannel), vAuthenticate]);
  end;

  DoWriteData(vJSON);
end;

procedure TsgcWS_API_Coinbase.DoSubscriptionsEvent(const aRawMessage: string);
begin
  if Assigned(FOnCoinbaseSubscriptions) then
    FOnCoinbaseSubscriptions(self, GetJSONChannels, GetJSONProductIds,
      aRawMessage);
end;

procedure TsgcWS_API_Coinbase.DoWriteData(const aText: string);
begin
  if Assigned(FClient) then
    FClient.WriteData(aText);
end;

function TsgcWS_API_Coinbase.GetChannel
  (aChannel: TsgcWSCoinbaseChannels): string;
begin
  case aChannel of
    cbcHeartBeat:
      result := CS_COINBASE_HEARTBEAT;
    cbcStatus:
      result := CS_COINBASE_STATUS;
    cbcTicker:
      result := CS_COINBASE_TICKER;
    cbcLevel2:
      result := CS_COINBASE_LEVEL2;
    cbcMatches:
      result := CS_COINBASE_MATCHES;
    cbcFull:
      result := CS_COINBASE_FULL;
    cbcUser:
      result := CS_COINBASE_USER;
  end;
end;

function TsgcWS_API_Coinbase.GetJSON: TsgcJSON;
begin
  if not Assigned(FJSON) then
    FJSON := TsgcJSON.Create(self);
  result := FJSON;
end;

function TsgcWS_API_Coinbase.GetJSONChannels: string;
begin
  result := '';
  if JSON.Node['channels'] <> nil then
  begin
    if JSON.Node['channels'].JSONObject <> nil then
    begin
      if JSON.Node['channels'].JSONObject.Count > 0 then
      begin
        if JSON.Node['channels'].JSONObject.Item[0].Node['name'] <> nil then
          result := JSON.Node['channels'].JSONObject.Item[0].Node['name'].Value;
      end;
    end;
  end;
end;

function TsgcWS_API_Coinbase.GetJSONError: string;
begin
  result := '';
  if JSON.Node['message'] <> nil then
    result := JSON.Node['message'].Value;
end;

function TsgcWS_API_Coinbase.GetJSONErrorReason: string;
begin
  result := '';
  if JSON.Node['reason'] <> nil then
    result := JSON.Node['reason'].Value;
end;

function TsgcWS_API_Coinbase.GetJSONProductIds: string;
begin
  result := '';
  if JSON.Node['channels'] <> nil then
  begin
    if JSON.Node['channels'].JSONObject <> nil then
    begin
      if JSON.Node['channels'].JSONObject.Count > 0 then
      begin
        if JSON.Node['channels'].JSONObject.Item[0].Node['name'] <> nil then
          result := JSON.Node['channels'].JSONObject.Item[0].Node
            ['product_ids'].Value;
      end;
    end;
  end;
end;

function TsgcWS_API_Coinbase.GetREST_API: TsgcHTTP_API_Coinbase_Rest;
begin
  if not Assigned(FREST_API) then
  begin
    FREST_API := TsgcHTTP_API_Coinbase_Rest.Create(nil);
    if Assigned(FCLient) then
    begin
      FREST_API.TLSOptions.OpenSSL_Options.APIVersion :=
        FCLient.TLSOptions.OpenSSL_Options.APIVersion;
      FREST_API.TLSOptions.IOHandler := FClient.TLSOptions.IOHandler;
      FREST_API.TLSOptions.Version := FClient.TLSOptions.Version;
    end;
  end;

  if Assigned(FOnCoinbaseHTTPException) then
    FREST_API.OnHTTPAPIException := OnHTTPAPIExceptionEvent
  else
    FREST_API.OnHTTPAPIException := nil;
  FREST_API.CoinbaseOptions.ApiKey := Coinbase.ApiKey;
  FREST_API.CoinbaseOptions.ApiSecret := Coinbase.ApiSecret;
  FREST_API.CoinbaseOptions.Passphrase := Coinbase.Passphrase;
  FREST_API.CoinbaseOptions.SandBox := Coinbase.SandBox;
  if FREST_API.CoinbaseOptions.LogOptions.Enabled <> Coinbase.HTTPLogOptions.Enabled
  then
  begin
    FREST_API.CoinbaseOptions.LogOptions.Enabled :=
      Coinbase.HTTPLogOptions.Enabled;
    FREST_API.CoinbaseOptions.LogOptions.FileName :=
      Coinbase.HTTPLogOptions.FileName;
  end;
  Result := FREST_API;
end;

function TsgcWS_API_Coinbase.GetURL: String;
begin
  if Coinbase.SandBox then
    result := 'wss://ws-feed-public.sandbox.pro.coinbase.com'
  else
    result := 'wss://ws-feed.pro.coinbase.com';

  if Assigned(Client) then
    Client.TLSOptions.Version := tls1_2;
end;

procedure TsgcWS_API_Coinbase.OnHTTPAPIExceptionEvent(Sender: TObject; E:
    Exception);
begin
  DoCoinbaseHTTPExceptionEvent(E);
end;

procedure TsgcWS_API_Coinbase.SetCoinbase(const Value: TsgcWSCoinbase_Options);
begin
  if Assigned(FCoinbase) then
    FCoinbase.Assign(Value);
end;

procedure TsgcWS_API_Coinbase.SubscribeFull(const aProductId: string;
  aAuthenticate: Boolean = False);
begin
  DoRequest(cbrSubscribe, cbcFull, aProductId, aAuthenticate);
end;

procedure TsgcWS_API_Coinbase.SubscribeHeartBeat(const aProductId: String;
  aAuthenticate: Boolean = False);
begin
  DoRequest(cbrSubscribe, cbcHeartBeat, aProductId, aAuthenticate);
end;

procedure TsgcWS_API_Coinbase.SubscribeLevel2(const aProductId: string;
  aAuthenticate: Boolean = False);
begin
  DoRequest(cbrSubscribe, cbcLevel2, aProductId, aAuthenticate);
end;

procedure TsgcWS_API_Coinbase.SubscribeMatches(const aProductId: string;
  aAuthenticate: Boolean = False);
begin
  DoRequest(cbrSubscribe, cbcMatches, aProductId, aAuthenticate);
end;

procedure TsgcWS_API_Coinbase.SubscribeStatus(aAuthenticate: Boolean = False);
begin
  DoRequest(cbrSubscribe, cbcStatus, '', aAuthenticate);
end;

procedure TsgcWS_API_Coinbase.SubscribeTicker(const aProductId: string;
  aAuthenticate: Boolean = False);
begin
  DoRequest(cbrSubscribe, cbcTicker, aProductId, aAuthenticate);
end;

procedure TsgcWS_API_Coinbase.SubscribeUser(const aProductId: string);
begin
  DoRequest(cbrSubscribe, cbcUser, aProductId, True);
end;

procedure TsgcWS_API_Coinbase.UnSubscribeFull(const aProductId: string);
begin
  DoRequest(cbrUnsubscribe, cbcFull, aProductId);
end;

procedure TsgcWS_API_Coinbase.UnSubscribeHeartBeat(const aProductId: String);
begin
  DoRequest(cbrUnsubscribe, cbcHeartBeat, aProductId);
end;

procedure TsgcWS_API_Coinbase.UnSubscribeLevel2(const aProductId: string);
begin
  DoRequest(cbrUnsubscribe, cbcLevel2, aProductId);
end;

procedure TsgcWS_API_Coinbase.UnSubscribeMatches(const aProductId: string);
begin
  DoRequest(cbrUnsubscribe, cbcMatches, aProductId);
end;

procedure TsgcWS_API_Coinbase.UnSubscribeStatus;
begin
  DoRequest(cbrUnsubscribe, cbcStatus);
end;

procedure TsgcWS_API_Coinbase.UnSubscribeTicker(const aProductId: string);
begin
  DoRequest(cbrUnsubscribe, cbcTicker, aProductId);
end;

procedure TsgcWS_API_Coinbase.UnSubscribeUser(const aProductId: string);
begin
  DoRequest(cbrUnsubscribe, cbcUser, aProductId);
end;

constructor TsgcWSCoinbase_Options.Create;
begin
  inherited;
  FHTTPLogOptions := TsgcWSCoinbaseLog_Options.Create;
end;

destructor TsgcWSCoinbase_Options.Destroy;
begin
  sgcFree(FHTTPLogOptions);
  inherited;
end;

procedure TsgcWSCoinbase_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSCoinbase_Options then
  begin
    ApiKey := TsgcWSCoinbase_Options(aSource).ApiKey;
    ApiSecret := TsgcWSCoinbase_Options(aSource).ApiSecret;
    Passphrase := TsgcWSCoinbase_Options(aSource).Passphrase;
    SandBox := TsgcWSCoinbase_Options(aSource).SandBox;
    HTTPLogOptions := TsgcWSCoinbase_Options(aSource).HTTPLogOptions;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSCoinbase_Options.SetHTTPLogOptions(const Value :
    TsgcWSCoinbaseLog_Options);
begin
  if Assigned(FHTTPLogOptions) then
    FHTTPLogOptions.Assign(Value);
end;

procedure TsgcWSCoinbaseLog_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSCoinbaseLog_Options then
  begin
    Enabled := TsgcWSCoinbaseLog_Options(aSource).Enabled;
    FileName := TsgcWSCoinbaseLog_Options(aSource).FileName;
  end
  else
    inherited Assign(aSource);
end;

{$ENDIF}

end.
