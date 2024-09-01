{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_API_Pusher;

interface

{$I sgcVer.inc}
{$IFDEF SGC_APIS}

uses
  Classes, SysUtils, StrUtils,
  // sgc
  sgcWebSocket_Classes, sgcJSON;

type
  TsgcWSPusherRequestAuthentication = class;
  TsgcWSPusherResponseAuthentication = class;

  TsgcWSPusherConnectEvent = procedure(Sender: TObject; Socket_id: String;
    Timeout: Integer) of object;
  TsgcWSPusherErrorEvent = procedure(Sender: TObject; Code: Integer;
    Msg: String) of object;
  TsgcWSPusherSubscribeEvent = procedure(Sender: TObject; Channel, Data: String)
    of object;
  TsgcWSPusherEventEvent = procedure(Sender: TObject;
    Event, Channel, Data: String) of object;
  TsgcWSPusherAuthenticationEvent = procedure(Sender: TObject;
    AuthRequest: TsgcWSPusherRequestAuthentication;
    AuthResponse: TsgcWSPusherResponseAuthentication) of object;

  TsgcWSPusherChannels = (pscPublicChannel, pscPrivateChannel,
    pscPresenceChannel);

  TsgcWSPusherRequestAuthentication = class
  private
    FChannel: string;
    FData: string;
    FKey: string;
    FSocketID: string;
  public
    property Channel: string read FChannel;
    property Data: string read FData;
    property Key: string read FKey;
    property SocketID: string read FSocketID;
  end;

  TsgcWSPusherResponseAuthentication = class
  private
    FSecret: string;
    FSignature: string;
  public
    property Secret: string read FSecret write FSecret;
    property Signature: string read FSignature write FSignature;
  end;

  TsgcWSPusher_Options = class(TPersistent)
  private
    FAppId: string;
    FCluster: String;
    FKey: String;
    FName: string;
    FSecret: String;
    FTLS: Boolean;
    FVersion: String;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property AppId: string read FAppId write FAppId;
    property Cluster: String read FCluster write FCluster;
    property Key: String read FKey write FKey;
    property Name: string read FName write FName;
    property Secret: String read FSecret write FSecret;
    property TLS: Boolean read FTLS write FTLS;
    property Version: String read FVersion write FVersion;
  end;

  TsgcWS_API_Pusher = class(TsgcWSAPI_client)
    { from TsgcWSComponent_Base }
  protected
    procedure DoEventMessage(aConnection: TsgcWSConnection;
      const Text: string); override;
    { from TsgcWSComponent_Base }

    { from TsgcWSAPI_client }
  protected
    function GetURL: String; override;
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

    { helpers }
  private
    function SerializeJSON(const aText: String): String;
    function DoubleEncodedJSON(const aText: String): String;
  private
    function GetChannelName(const aChannel: String;
      aChannelType: TsgcWSPusherChannels): String;
  private
    { helpers }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { internal procedures }
  protected
    procedure DoSendPong;
    procedure DoReceivePong;
    procedure DoReadEvent(const aEvent: string); virtual;
    { internal procedures }

    { websocket methods }
  private
    function GetPusherSignature(const aKey, aSecret, aSocketID,
      aChannelName: string; const aData: string = ''): string;
  public
    procedure Ping(const aData: string = '{}');
  public
    procedure Subscribe(const aChannel: String;
      aChannelType: TsgcWSPusherChannels = pscPublicChannel;
      aData: String = '');
    procedure UnSubscribe(const aChannel: String;
      aChannelType: TsgcWSPusherChannels = pscPublicChannel);
  public
    procedure Publish(const aEvent, aChannel: string;
      aChannelType: TsgcWSPusherChannels = pscPublicChannel;
      const aData: string = '{}');
    { websocket methods }

    { rest methods }
  private
    function GetEndpoint: string;
    function GetSignature(const aHTTPMethod, aRequestPath,
      aQueryParams: string): string;
  protected
    function DoHTTP_Request(const aHTTPMethod, aRequestPath: string;
      aBody: string = ''): string;
    function DoHTTP_GET_Request(const aRequestPath: string): string;
    function DoHTTP_POST_Request(const aRequestPath, aBody: string): string;
  public
    function TriggerEvent(const aEventName, aChannel, aData: string): string;
  public
    function GetChannels: string;
    function GetChannel(const aChannel: string): string;
  public
    function GetUsers(const aChannel: string): string;
    { rest methods }

    { properties }
  private
    FSocket_id: String;
  private
    FPusher: TsgcWSPusher_Options;
    procedure SetPusher(const Value: TsgcWSPusher_Options);
  public
    property Pusher: TsgcWSPusher_Options read FPusher write SetPusher;
    { properties }

    { events }
  private
    FOnPusherAuthentication: TsgcWSPusherAuthenticationEvent;
    FOnPusherConnect: TsgcWSPusherConnectEvent;
    FOnPusherError: TsgcWSPusherErrorEvent;
    FOnPusherEvent: TsgcWSPusherEventEvent;
    FOnPusherSubscribe: TsgcWSPusherSubscribeEvent;
  protected
    procedure DoEventPusherConnect;
    procedure DoEventPusherError;
    procedure DoEventPusherSubscribe;
    procedure DoEventPusherEvent;
  public
    property OnPusherAuthentication: TsgcWSPusherAuthenticationEvent
      read FOnPusherAuthentication write FOnPusherAuthentication;
    property OnPusherConnect: TsgcWSPusherConnectEvent read FOnPusherConnect
      write FOnPusherConnect;
    property OnPusherError: TsgcWSPusherErrorEvent read FOnPusherError
      write FOnPusherError;
    property OnPusherEvent: TsgcWSPusherEventEvent read FOnPusherEvent
      write FOnPusherEvent;
    property OnPusherSubscribe: TsgcWSPusherSubscribeEvent
      read FOnPusherSubscribe write FOnPusherSubscribe;
    { events }
  end;
{$ENDIF}

implementation

{$IFDEF SGC_APIS}

uses
  // sgc
  sgcBase_Const, sgcBase_Helpers, sgcWebSocket_Helpers, sgcWebSocket_Const,
  sgcHTTP_API, sgcWebSocket_Types;

Const
  CS_PUSHER_PROTOCOL = '7';

Const
  CS_PUSHER_PRIVATE = 'private-';
  CS_PUSHER_PRESENCE = 'presence-';

constructor TsgcWS_API_Pusher.Create(aOwner: TComponent);
begin
  inherited;
  FPusher := TsgcWSPusher_Options.Create;
  FPusher.Version := CS_VERSION;
  FPusher.Name := CS_APPLICATION_NAME;
end;

destructor TsgcWS_API_Pusher.Destroy;
begin
  sgcFree(FPusher);
  inherited;
end;

procedure TsgcWS_API_Pusher.DoEventPusherConnect;
var
  vTimeout: Integer;
  oJSON: TsgcJSON;
begin
  if JSON.Node['data'] <> nil then
  begin
    oJSON := TsgcJSON.Create(nil);
    Try
      oJSON.Read(SerializeJSON(JSON.Node['data'].Value));
      FSocket_id := oJSON.Node['socket_id'].Value;
      TryStrToInt(oJSON.Node['activity_timeout'].Value, vTimeout);

      if vTimeout > 0 then
      begin
        TsgcWSComponent_WSClient_API(FClient).DoStopHeartBeat;
        FClient.HeartBeat.Interval := vTimeout;
        FClient.HeartBeat.Enabled := True;
        TsgcWSComponent_WSClient_API(FClient).DoStartHeartBeat;
      end;
    Finally
      sgcFree(oJSON);
    End;
  end;

  if Assigned(FOnPusherConnect) then
    FOnPusherConnect(self, FSocket_id, vTimeout);
end;

procedure TsgcWS_API_Pusher.DoEventPusherError;
var
  vMsg: String;
  vCode: Integer;
  oJSON: TsgcJSON;
begin
  if Assigned(FOnPusherError) then
  begin
    if JSON.Node['data'] <> nil then
    begin
      oJSON := TsgcJSON.Create(nil);
      Try
        oJSON.Read(JSON.Node['data'].Value);
        TryStrToInt(oJSON.Node['code'].Value, vCode);
        vMsg := oJSON.Node['message'].Value;
      Finally
        sgcFree(oJSON);
      End;
    end;

    FOnPusherError(self, vCode, vMsg);
  end;
end;

function TsgcWS_API_Pusher.GetJSON: TsgcJSON;
begin
  if not Assigned(FJSON) then
    FJSON := TsgcJSON.Create(self);
  Result := FJSON;
end;

procedure TsgcWS_API_Pusher.DoEventMessage(aConnection: TsgcWSConnection;
  const Text: string);
begin
  if RawMessages then
    inherited
  else
  begin
    JSON.Clear;
    JSON.Read(Text);

    if JSON.Node['event'] <> nil then
      DoReadEvent(JSON.Node['event'].Value)
    else
      inherited;
  end;
end;

procedure TsgcWS_API_Pusher.DoEventPusherEvent;
var
  vEvent, vChannel, vData: String;
begin
  if Assigned(FOnPusherEvent) then
  begin
    vEvent := JSON.Node['event'].Value;
    if JSON.Node['channel'] <> nil then
      vChannel := JSON.Node['channel'].Value;
    if JSON.Node['data'] <> nil then
      vData := JSON.Node['data'].Value;

    FOnPusherEvent(self, vEvent, vChannel, vData);
  end;
end;

procedure TsgcWS_API_Pusher.DoEventPusherSubscribe;
var
  vChannel, vData: String;
begin
  if Assigned(FOnPusherSubscribe) then
  begin
    vChannel := JSON.Node['channel'].Value;
    vData := SerializeJSON(JSON.Node['data'].Value);

    FOnPusherSubscribe(self, vChannel, vData);
  end;
end;

function TsgcWS_API_Pusher.DoHTTP_GET_Request(const aRequestPath
  : string): string;
begin
  Result := DoHTTP_Request('GET', aRequestPath);
end;

function TsgcWS_API_Pusher.DoHTTP_POST_Request(const aRequestPath,
  aBody: string): string;
begin
  Result := DoHTTP_Request('POST', aRequestPath, aBody);
end;

function TsgcWS_API_Pusher.DoHTTP_Request(const aHTTPMethod,
  aRequestPath: string; aBody: string = ''): string;
var
  oClient: TsgcHTTPAPI_Client;
  vQueryParams, vSignature: string;
  oHeaders: TStringList;
begin
  oClient := TsgcHTTPAPI_Client.Create(nil);
  Try
    oClient.TLSOptions.Version := tls1_2;
    vQueryParams := 'auth_key=' + Pusher.Key + '&auth_timestamp=' +
      GetDateTimeUnix(Now, False) + '&auth_version=1.0';

    if aBody <> '' then
      vQueryParams := vQueryParams + '&body_md5=' +
        LowerCase(GetMD5(sgcStringToHex(aBody)));
    vSignature := GetSignature(aHTTPMethod, aRequestPath, vQueryParams);
    if aHTTPMethod = 'POST' then
    begin
      oHeaders := TStringList.Create;
      Try
        oHeaders.Add('Content-Type: application/json');
        Result := oClient.Post(GetEndpoint + aRequestPath + '?' + vQueryParams +
          '&auth_signature=' + vSignature, aBody, oHeaders)
      Finally
        sgcFree(oHeaders);
      End;
    end
    else
      Result := oClient.Get(GetEndpoint + aRequestPath + '?' + vQueryParams +
        '&auth_signature=' + vSignature);
  Finally
    sgcFree(oClient);
  End;
end;

function TsgcWS_API_Pusher.DoKeepAlive: Boolean;
begin
  Result := True;

  Ping;
end;

procedure TsgcWS_API_Pusher.DoSendPong;
begin
  FClient.WriteData('{"event":"pusher:pong", "data":"' + JSON.Node['data']
    .Value + '"}');
end;

procedure TsgcWS_API_Pusher.DoReadEvent(const aEvent: string);
begin
  if aEvent = 'pusher:connection_established' then
    DoEventPusherConnect
  else if aEvent = 'pusher:error' then
    DoEventPusherError
  else if aEvent = 'pusher_internal:subscription_succeeded' then
    DoEventPusherSubscribe
  else if aEvent = 'puserh:ping' then
    DoSendPong
  else if aEvent = 'pusher:pong' then
    DoReceivePong
  else
    DoEventPusherEvent;

end;

procedure TsgcWS_API_Pusher.DoReceivePong;
begin
  // nothing
end;

function TsgcWS_API_Pusher.GetChannelName(const aChannel: String;
  aChannelType: TsgcWSPusherChannels): String;
begin
  Result := aChannel;

  case aChannelType of
    pscPrivateChannel:
      Result := CS_PUSHER_PRIVATE + Result;
    pscPresenceChannel:
      Result := CS_PUSHER_PRESENCE + Result;
  end;
end;

function TsgcWS_API_Pusher.GetURL: String;
var
  vProtocol: string;
  vHost: string;
begin
  vProtocol := 'ws';
  if Pusher.TLS then
    vProtocol := 'wss';
  if Pusher.Cluster <> '' then
    vHost := 'ws-' + Pusher.Cluster + '.pusher.com'
  else
    vHost := 'ws.pusherapp.com';

  Result := vProtocol + '://' + vHost + '/app/' + Pusher.Key + '?client=' +
    Pusher.Name + '&version=' + Pusher.Version + '&protocol=' +
    CS_PUSHER_PROTOCOL + '&flash=false';
end;

procedure TsgcWS_API_Pusher.Publish(const aEvent, aChannel: string;
  aChannelType: TsgcWSPusherChannels = pscPublicChannel;
  const aData: string = '{}');
begin
  case aChannelType of
    pscPublicChannel:
      TriggerEvent(aEvent, aChannel, aData);
    pscPrivateChannel, pscPresenceChannel:
      begin
        FClient.WriteData('{' + '"event": "client-' + aEvent + '", ' +
          '"channel": "' + GetChannelName(aChannel, aChannelType) + '", ' +
          '"data": "' + DoubleEncodedJSON(aData) + '"' + '}');
      end;
  end;
end;

function TsgcWS_API_Pusher.SerializeJSON(const aText: String): String;
begin
  Result := sgcStringReplace(aText, '{\', '{');
  Result := sgcStringReplace(Result, '\"', '"');
end;

procedure TsgcWS_API_Pusher.Subscribe(const aChannel: String;
  aChannelType: TsgcWSPusherChannels = pscPublicChannel; aData: String = '');
var
  oRequestAuth: TsgcWSPusherRequestAuthentication;
  oResponseAuth: TsgcWSPusherResponseAuthentication;
  vSignature: string;
begin
  case aChannelType of
    pscPublicChannel:
      begin
        FClient.WriteData('{' + '"event": "pusher:subscribe", ' + '"data": { ' +
          '"channel":"' + GetChannelName(aChannel, aChannelType) + '"}' + '}');
      end;
    pscPrivateChannel, pscPresenceChannel:
      begin
        oResponseAuth := TsgcWSPusherResponseAuthentication.Create;
        Try
          oResponseAuth.Secret := Pusher.Secret;
          oResponseAuth.Signature := '';
          if Assigned(FOnPusherAuthentication) then
          begin
            oRequestAuth := TsgcWSPusherRequestAuthentication.Create;
            Try
              oRequestAuth.FChannel := GetChannelName(aChannel, aChannelType);
              oRequestAuth.FSocketID := FSocket_id;
              oRequestAuth.FData := aData;
              oRequestAuth.FKey := Pusher.Key;

              FOnPusherAuthentication(self, oRequestAuth, oResponseAuth);
            Finally
              sgcFree(oRequestAuth);
            End;
          end;

          // if signature is not provided, calculate
          vSignature := oResponseAuth.Signature;
          if vSignature = '' then
            vSignature := GetPusherSignature(Pusher.Key, oResponseAuth.Secret,
              FSocket_id, GetChannelName(aChannel, aChannelType), aData);

          if aChannelType = pscPrivateChannel then
            FClient.WriteData('{' + '"event": "pusher:subscribe", ' +
              '"data": { ' + '"channel":"' + GetChannelName(aChannel,
              aChannelType) + '", ' + '"auth":"' + vSignature + '"}' + '}')
          else if aChannelType = pscPresenceChannel then
            FClient.WriteData('{' + '"event": "pusher:subscribe", ' +
              '"data": { ' + '"channel":"' + GetChannelName(aChannel,
              aChannelType) + '", ' + '"auth":"' + vSignature + '", ' +
              '"channel_data":"' + DoubleEncodedJSON(aData) + '"}' + '}');
        Finally
          sgcFree(oResponseAuth);
        End;
      end;
  end;
end;

function TsgcWS_API_Pusher.DoubleEncodedJSON(const aText: String): String;
begin
  Result := aText;
  if LeftStr(aText, 1) = '{' then
    Result := sgcStringReplace(aText, '"', '\"');
end;

function TsgcWS_API_Pusher.GetChannel(const aChannel: string): string;
begin
  Result := DoHTTP_GET_Request('/apps/' + Pusher.AppId + '/channels/' +
    aChannel);
end;

function TsgcWS_API_Pusher.GetChannels: string;
begin
  Result := DoHTTP_GET_Request('/apps/' + Pusher.AppId + '/channels');
end;

function TsgcWS_API_Pusher.GetEndpoint: string;
begin
  Result := 'https://api-' + Pusher.Cluster + '.pusher.com';
end;

function TsgcWS_API_Pusher.GetPusherSignature(const aKey, aSecret, aSocketID,
  aChannelName: string; const aData: string = ''): string;
begin
  Result := aSocketID + ':' + aChannelName;
  if aData <> '' then
    Result := Result + ':' + aData;

  Result := GetHMACSHA256(Result, aSecret);

  Result := aKey + ':' + Result;
end;

function TsgcWS_API_Pusher.GetSignature(const aHTTPMethod, aRequestPath,
  aQueryParams: string): string;
begin
  Result := GetHMACSHA256(aHTTPMethod + CHR(10) + aRequestPath + CHR(10) +
    aQueryParams, Pusher.Secret, False);
end;

function TsgcWS_API_Pusher.GetUsers(const aChannel: string): string;
begin
  Result := DoHTTP_GET_Request('/apps/' + Pusher.AppId + '/channels/presence-' +
    aChannel + '/users');;
end;

procedure TsgcWS_API_Pusher.Ping(const aData: string = '{}');
begin
  FClient.WriteData('{"event":"pusher:ping", "data":"' + aData + '"}');
end;

function TsgcWS_API_Pusher.TriggerEvent(const aEventName, aChannel,
  aData: string): string;
var
  vBody: string;
begin
  vBody := Format('{"name": "%s", "channels": ["%s"], "data": "%s"}',
    [aEventName, aChannel, DoubleEncodedJSON(aData)]);
  Result := DoHTTP_POST_Request('/apps/' + Pusher.AppId + '/events', vBody);
end;

procedure TsgcWS_API_Pusher.SetPusher(const Value: TsgcWSPusher_Options);
begin
  if Assigned(FPusher) then
    FPusher.Assign(Value);
end;

procedure TsgcWS_API_Pusher.UnSubscribe(const aChannel: String;
  aChannelType: TsgcWSPusherChannels = pscPublicChannel);
begin
  FClient.WriteData('{' + '"event": "pusher:unsubscribe", ' + '"data": { ' +
    '"channel":"' + GetChannelName(aChannel, aChannelType) + '"}' + '}');
end;

procedure TsgcWSPusher_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSPusher_Options then
  begin
    AppId := TsgcWSPusher_Options(aSource).AppId;
    Key := TsgcWSPusher_Options(aSource).Key;
    Name := TsgcWSPusher_Options(aSource).Name;
    Secret := TsgcWSPusher_Options(aSource).Secret;
    Version := TsgcWSPusher_Options(aSource).Version;
    Cluster := TsgcWSPusher_Options(aSource).Cluster;
    TLS := TsgcWSPusher_Options(aSource).TLS;
  end
  else
    inherited Assign(aSource);
end;

{$ENDIF}

end.
