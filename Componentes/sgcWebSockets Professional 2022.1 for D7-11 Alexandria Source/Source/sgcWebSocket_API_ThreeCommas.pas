{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_API_ThreeCommas;

interface

{$I sgcVer.inc}
{$IFDEF SGC_APIS}

uses
  Classes, SysUtils, StrUtils,
  // sgc
  sgcWebSocket_Classes, sgcJSON, sgcHTTP_API_ThreeCommas;

type
  TsgcWSThreeCommasConnectEvent = procedure(Sender: TObject; const aRawMessage:
      string) of object;
  TsgcWSThreeCommasSubscribedEvent = procedure(Sender: TObject; const aChannel,
      aRawMessage: string) of object;
  TsgcWSThreeCommasRejectSubscriptionEvent = procedure(Sender: TObject; const
      aChannel, aRawMessage: string) of object;
  TsgcWSThreeCommasMessageEvent = procedure(Sender: TObject; const aType,
      aRawMessage: string) of object;
  TsgcWSThreeCommasPingEvent = procedure(Sender: TObject; const aRawMessage: string)
      of object;
  TsgcWSThreeCommasHTTPExceptionEvent = procedure(Sender: TObject; E: Exception)
    of object;

  TsgcWSThreeCommasChannels = (r3cmcSmartTradesChannel, r3cmDealsChannel);

  TsgcWSThreeCommasLog_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FFileName: String;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property FileName: String read FFileName write FFileName;
  end;

  TsgcWSThreeCommas_Options = class(TPersistent)
  private
    FApiKey: String;
    FApiSecret: String;
    FHTTPLogOptions: TsgcWSThreeCommasLog_Options;
    procedure SetHTTPLogOptions(const Value: TsgcWSThreeCommasLog_Options);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property ApiKey: String read FApiKey write FApiKey;
    property ApiSecret: String read FApiSecret write FApiSecret;
    property HTTPLogOptions: TsgcWSThreeCommasLog_Options read FHTTPLogOptions
      write SetHTTPLogOptions;
  end;

  TsgcWS_API_ThreeCommas = class(TsgcWSAPI_client)
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
    function GetJSONChannel: string;
    function GetJSONCode: string;
    function GetJSONMessage: string;
  protected
    procedure DoReadJSONType(aRawMessage: string); virtual;
  protected
    property JSON: TsgcJSON read GetJSON write FJSON;
    { JSON }

    { options }
  private
    FThreeCommas: TsgcWSThreeCommas_Options;
    FREST_API: TsgcHTTP_API_3Commas_Rest;
    function GetREST_API: TsgcHTTP_API_3Commas_Rest;
    procedure SetThreeCommas(const Value: TsgcWSThreeCommas_Options);
  protected
    procedure OnHTTPAPIExceptionEvent(Sender: TObject; E: Exception); virtual;
  public
    property ThreeCommas: TsgcWSThreeCommas_Options read FThreeCommas write
        SetThreeCommas;
    property REST_API: TsgcHTTP_API_3Commas_Rest read GetREST_API
      write FREST_API;
    { options }

    { constructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor }

    { methods }
  protected
    procedure DoWriteData(const aText: string); virtual;
  protected
    procedure DoSubscribe(aChannel: TsgcWSThreeCommasChannels); virtual;
    // streams
  public
    procedure SubscribeSmartTrades;
    procedure SubscribeDeals;
    { methods }

    { events }
  private
    FOnThreeCommasMessage: TsgcWSThreeCommasMessageEvent;
    FOnThreeCommasConfirmSubscription: TsgcWSThreeCommasSubscribedEvent;
    FOnThreeCommasRejectSubscription: TsgcWSThreeCommasRejectSubscriptionEvent;
    FOnThreeCommasPing: TsgcWSThreeCommasPingEvent;
    FOnThreeCommasConnect: TsgcWSThreeCommasConnectEvent;
  protected
    procedure DoConfirmSubscriptionEvent(const aRawMessage: string); virtual;
    procedure DoRejectSubscriptionEvent(const aRawMessage: string); virtual;
    procedure DoPingEvent(const aRawMessage: string); virtual;
    procedure DoMessageEvent(const aType: string;
      const aRawMessage: string); virtual;
    procedure DoConnectEvent(const aRawMessage: string); virtual;
  public
    property OnThreeCommasMessage: TsgcWSThreeCommasMessageEvent read FOnThreeCommasMessage
      write FOnThreeCommasMessage;
    property OnThreeCommasConfirmSubscription: TsgcWSThreeCommasSubscribedEvent read
        FOnThreeCommasConfirmSubscription write FOnThreeCommasConfirmSubscription;
    property OnThreeCommasRejectSubscription: TsgcWSThreeCommasRejectSubscriptionEvent read
        FOnThreeCommasRejectSubscription write FOnThreeCommasRejectSubscription;
    property OnThreeCommasPing: TsgcWSThreeCommasPingEvent read FOnThreeCommasPing write
        FOnThreeCommasPing;
    property OnThreeCommasConnect: TsgcWSThreeCommasConnectEvent read FOnThreeCommasConnect write
        FOnThreeCommasConnect;
    { events }

    { http events }
  private
    FOnThreeCommasHTTPException: TsgcWSThreeCommasHTTPExceptionEvent;
  protected
    procedure Do3CommasHTTPExceptionEvent(E: Exception); virtual;
  public
    property OnThreeCommasHTTPException: TsgcWSThreeCommasHTTPExceptionEvent
      read FOnThreeCommasHTTPException write FOnThreeCommasHTTPException;
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
  CS_3Commas_SUBSCRIBE = 'subscribe';
  CS_3Commas_CHANNEL_SMART_TRADES = 'SmartTradesChannel';
  CS_3Commas_CHANNEL_DEALS = 'DealsChannel';

const
  CS_3Commas_REQUEST_SMART_TRADES = '/smart_trades';
  CS_3Commas_REQUEST_DEALS = '/deals';

const
  CS_3Commas_CONFIRM_SUBSCRIPTION = 'confirm_subscription';
  CS_3Commas_REJECT_SUBSCRIPTION = 'reject_subscription';
  CS_3Commas_PING = 'ping';
  CS_3Commas_CONNECT = 'welcome';

constructor TsgcWS_API_ThreeCommas.Create(aOwner: TComponent);
begin
  inherited;
  FThreeCommas := TsgcWSThreeCommas_Options.Create;
end;

destructor TsgcWS_API_ThreeCommas.Destroy;
begin
  sgcFree(FThreeCommas);
  sgcFree(FREST_API);
  inherited;
end;

procedure TsgcWS_API_ThreeCommas.DoBeforeConnect;
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

procedure TsgcWS_API_ThreeCommas.Do3CommasHTTPExceptionEvent(E: Exception);
begin
  if Assigned(FOnThreeCommasHTTPException) then
    FOnThreeCommasHTTPException(self, E);
end;

procedure TsgcWS_API_ThreeCommas.DoEventMessage(aConnection: TsgcWSConnection;
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

procedure TsgcWS_API_ThreeCommas.DoMessageEvent(const aType: string;
  const aRawMessage: string);
begin
  if Assigned(FOnThreeCommasMessage) then
    FOnThreeCommasMessage(self, aType, aRawMessage);
end;

procedure TsgcWS_API_ThreeCommas.DoReadJSONType(aRawMessage: string);
var
  vType: string;
begin
  vType := JSON.Node['type'].Value;

  if vType = CS_3Commas_CONFIRM_SUBSCRIPTION then
    DoConfirmSubscriptionEvent(aRawMessage)
  else if vType = CS_3Commas_REJECT_SUBSCRIPTION then
    DoRejectSubscriptionEvent(aRawMessage)
  else if vType = CS_3Commas_PING then
    DoPingEvent(aRawMessage)
  else if vtype = CS_3COMMAS_CONNECT then
    DoConnectEvent(aRawMessage)
  else
    DoMessageEvent(vType, aRawMessage);
end;

procedure TsgcWS_API_ThreeCommas.DoRejectSubscriptionEvent(const aRawMessage:
    string);
begin
  if Assigned(FOnThreeCommasRejectSubscription) then
    FOnThreeCommasRejectSubscription(self, GetJSONChannel, aRawMessage);
end;

procedure TsgcWS_API_ThreeCommas.DoSubscribe(aChannel: TsgcWSThreeCommasChannels);
var
  vChannel, vRequest: string;
begin
  case aChannel of
    r3cmcSmartTradesChannel:
      begin
        vChannel := CS_3Commas_CHANNEL_SMART_TRADES;
        vRequest := CS_3Commas_REQUEST_SMART_TRADES
      end;
    r3cmDealsChannel:
      begin
        vChannel := CS_3Commas_CHANNEL_DEALS;
        vRequest := CS_3Commas_REQUEST_DEALS;
      end;
  end;

  DoWriteData
    (Format('{"identifier":"{\"channel\":\"%s\", \"users\": [{\"api_key\":\"%s\",\"signature\":\"%s\"}]}","command": "%s"}',
    [vChannel, ThreeCommas.ApiKey, GetHMACSHA256(vRequest,
    ThreeCommas.ApiSecret, False), CS_3Commas_SUBSCRIBE]));
end;

procedure TsgcWS_API_ThreeCommas.DoConfirmSubscriptionEvent(const aRawMessage:
    string);
begin
  if Assigned(FOnThreeCommasConfirmSubscription) then
    FOnThreeCommasConfirmSubscription(self, GetJSONChannel, aRawMessage);
end;

procedure TsgcWS_API_ThreeCommas.DoConnectEvent(const aRawMessage: string);
begin
  if Assigned(FOnThreeCommasConnect) then
    FOnThreeCommasConnect(self, aRawMessage);
end;

procedure TsgcWS_API_ThreeCommas.DoPingEvent(const aRawMessage: string);
begin
  if Assigned(FOnThreeCommasPing) then
    FOnThreeCommasPing(self, aRawMessage);
end;

procedure TsgcWS_API_ThreeCommas.DoWriteData(const aText: string);
begin
  if Assigned(FClient) then
    FClient.WriteData(aText);
end;

function TsgcWS_API_ThreeCommas.GetJSON: TsgcJSON;
begin
  if not Assigned(FJSON) then
    FJSON := TsgcJSON.Create(self);
  Result := FJSON;
end;

function TsgcWS_API_ThreeCommas.GetJSONChannel: string;
var
  oJSON: TsgcJSON;
begin
  Result := '';
  if JSON.Node['identifier'] <> nil then
  begin
    oJSON := TsgcJSON.Create(nil);
    Try
      oJSON.Read(sgcStringReplace(JSON.Node['identifier'].Value, '\"', '"'));
      if oJSON.Node['channel'] <> nil then
        result := oJSON.Node['channel'].Value;
    Finally
      sgcFree(oJSON);
    End;
  end;
end;

function TsgcWS_API_ThreeCommas.GetJSONCode: string;
begin
  Result := '';
  if JSON.Node['code'] <> nil then
    Result := JSON.Node['code'].Value;
end;

function TsgcWS_API_ThreeCommas.GetJSONMessage: string;
begin
  Result := '';
  if JSON.Node['msg'] <> nil then
    Result := JSON.Node['msg'].Value;
end;

function TsgcWS_API_ThreeCommas.GetREST_API: TsgcHTTP_API_3Commas_Rest;
begin
  if not Assigned(FREST_API) then
  begin
    FREST_API := TsgcHTTP_API_3Commas_Rest.Create(nil);
    if Assigned(FClient) then
    begin
      FREST_API.TLSOptions.OpenSSL_Options.APIVersion :=
        FCLient.TLSOptions.OpenSSL_Options.APIVersion;
      FREST_API.TLSOptions.IOHandler := FClient.TLSOptions.IOHandler;
      FREST_API.TLSOptions.Version := FClient.TLSOptions.Version;
    end;
  end;

  if Assigned(FOnThreeCommasHTTPException) then
    FREST_API.OnHTTPAPIException := OnHTTPAPIExceptionEvent
  else
    FREST_API.OnHTTPAPIException := nil;
  FREST_API.ThreeCommas.ApiKey := ThreeCommas.ApiKey;
  FREST_API.ThreeCommas.ApiSecret := ThreeCommas.ApiSecret;
  if FREST_API.ThreeCommas.LogOptions.Enabled <>
    ThreeCommas.HTTPLogOptions.Enabled then
  begin
    FREST_API.ThreeCommas.LogOptions.Enabled :=
      ThreeCommas.HTTPLogOptions.Enabled;
    FREST_API.ThreeCommas.LogOptions.FileName :=
      ThreeCommas.HTTPLogOptions.FileName;
  end;
  Result := FREST_API;
end;

function TsgcWS_API_ThreeCommas.GetURL: String;
begin
  Result := 'wss://ws.3commas.io/websocket';

  if Assigned(Client) then
    Client.TLSOptions.Version := tls1_2;
end;

procedure TsgcWS_API_ThreeCommas.OnHTTPAPIExceptionEvent(Sender: TObject;
  E: Exception);
begin
  Do3CommasHTTPExceptionEvent(E);
end;

procedure TsgcWS_API_ThreeCommas.SetThreeCommas(const Value:
    TsgcWSThreeCommas_Options);
begin
  if Assigned(FThreeCommas) then
    FThreeCommas.Assign(Value);
end;

procedure TsgcWS_API_ThreeCommas.SubscribeDeals;
begin
  DoSubscribe(r3cmDealsChannel);
end;

procedure TsgcWS_API_ThreeCommas.SubscribeSmartTrades;
begin
  DoSubscribe(r3cmcSmartTradesChannel);
end;

constructor TsgcWSThreeCommas_Options.Create;
begin
  inherited;
  FHTTPLogOptions := TsgcWSThreeCommasLog_Options.Create;
end;

destructor TsgcWSThreeCommas_Options.Destroy;
begin
  sgcFree(FHTTPLogOptions);
  inherited;
end;

procedure TsgcWSThreeCommas_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSThreeCommas_Options then
  begin
    ApiKey := TsgcWSThreeCommas_Options(aSource).ApiKey;
    ApiSecret := TsgcWSThreeCommas_Options(aSource).ApiSecret;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSThreeCommas_Options.SetHTTPLogOptions
  (const Value: TsgcWSThreeCommasLog_Options);
begin
  if Assigned(FHTTPLogOptions) then
    FHTTPLogOptions.Assign(Value);
end;

procedure TsgcWSThreeCommasLog_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSThreeCommasLog_Options then
  begin
    Enabled := TsgcWSThreeCommasLog_Options(aSource).Enabled;
    FileName := TsgcWSThreeCommasLog_Options(aSource).FileName;
  end
  else
    inherited Assign(aSource);
end;

{$ENDIF}

end.
