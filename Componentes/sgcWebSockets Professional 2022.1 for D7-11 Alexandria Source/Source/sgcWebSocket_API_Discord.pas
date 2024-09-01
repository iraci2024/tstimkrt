{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_API_Discord;

interface

{$I sgcVer.inc}
{$IFDEF SGC_APIS}

uses
  Classes, SysUtils,
  // sgc
  sgcWebSocket_Classes, sgcHTTP_Client, sgcJSON, sgcHTTP_OAuth2_Client;

type

  TsgcWSDiscordEvents = (disDispatch, disHeartBeat, disIdentify,
    disStatusUpdate, disVoiceStateUpdate, disVoiceServerPing, disResume,
    disReconnect, disRequestGuildMembers, disInvalidSession, disHello,
    disHeartBeatACK);

  TsgcWSDiscordEvent = procedure(Sender: TObject; Event: TsgcWSDiscordEvents;
    const RawData: String) of object;
  TsgcWSDiscordReadyEvent = procedure(Sender: TObject; const RawData: String)
    of object;
  TsgcWSDiscordResumedEvent = procedure(Sender: TObject; const RawData: String)
    of object;
  TsgcWSDiscordDispatchEvent = procedure(Sender: TObject;
    const Event, RawData: String) of object;
  TsgcWSDiscordDBeforeReconnectEvent = procedure(Sender: TObject;
    var Token, Session_Id: String; var Seq: Integer; var Reconnect: Boolean)
    of object;

  TsgcWSDiscordBot_Options = class(TPersistent)
  private
    FBotName: String;
    FBotURL: String;
    FToken: String;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property BotName: String read FBotName write FBotName;
    property BotURL: String read FBotURL write FBotURL;
    property Token: String read FToken write FToken;
  end;

  TsgcWSDiscord_Options = class(TPersistent)
  private
    FBot: Boolean;
    FBotOptions: TsgcWSDiscordBot_Options;
    FClientId: String;
    FClientSecret: String;
    FIntents: Integer;
    FVersion: Integer;
    procedure SetBotOptions(const Value: TsgcWSDiscordBot_Options);
  public
    procedure Assign(aSource: TPersistent); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    property Bot: Boolean read FBot write FBot;
    property ClientId: String read FClientId write FClientId;
    property ClientSecret: String read FClientSecret write FClientSecret;
  published
    property BotOptions: TsgcWSDiscordBot_Options read FBotOptions
      write SetBotOptions;
    property Intents: Integer read FIntents write FIntents;
    property Version: Integer read FVersion write FVersion;
  end;

  TsgcWS_API_Discord = class(TsgcWSAPI_client)
    { from TsgcWSComponent_Base }
  protected
    procedure DoEventConnect(aConnection: TsgcWSConnection); override;
    procedure DoEventMessage(aConnection: TsgcWSConnection;
      const Text: string); override;
    procedure DoEventDisconnect(aConnection: TsgcWSConnection;
      Code: Integer); override;
    { from TsgcWSComponent_Base }

    { from TsgcWSAPI }
  protected
    procedure DoBeforeConnect; override;
  protected
    function DoKeepAlive: Boolean; override;
    { from TsgcWSAPI }

    { from TsgcWSAPI_client }
  private
    function GetDiscordBotURL: String;
    function GetDiscordURL: String;
  protected
    { from TsgcWSAPI_client }

    { http client }
  private
    FHTTPClient: TsgcIdHTTP;
    function GetHTTPClient: TsgcIdHTTP;
  protected
    property HTTPClient: TsgcIdHTTP read GetHTTPClient write FHTTPClient;
    { http client }

    { OAuth2 }
  private
    FOAuth2: TsgcHTTPComponentClient_OAuth2;
    function GetOAuth2: TsgcHTTPComponentClient_OAuth2;
  protected
    property OAuth2: TsgcHTTPComponentClient_OAuth2 read GetOAuth2 write FOAuth2;
    { OAuth2 }

    { JSON }
  private
    FJSON: TsgcJSON;
    function GetJSON: TsgcJSON;
  protected
    property JSON: TsgcJSON read GetJSON write FJSON;
    { JSON }

    { options }
  private
    FDiscordOptions: TsgcWSDiscord_Options;
    procedure SetDiscordOptions(const Value: TsgcWSDiscord_Options);
  public
    property DiscordOptions: TsgcWSDiscord_Options read FDiscordOptions
      write SetDiscordOptions;
    { options }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { methods }
  private
    FSession_id: String;
    FSequence: Integer;
    FReadSequence: Integer;
    FLastReadSequence: Integer;
    FSessionResumed: Boolean;
  private
    function GetUserAgent: string;
  private
    procedure DoDiscordHELLO(const aRawData: String);
    procedure DoDiscordHEARTBEAT;
    procedure DoDiscordDISPATCH(const aRawData: String);
    procedure DoDiscordRECONNECT(const aSession_id: String);
  private
    procedure OnAfterAccessTokenEvent(Sender: TObject;
      const Access_Token, Token_Type, Expires_In, Refresh_Token, Scope,
      RawParams: String; var Handled: Boolean);
    procedure DoAuthenticateUser;
    procedure DoIdentifyUser(const aToken: String);
    procedure DoReadSequence;
  protected
    procedure DoReadEvent(const aEvent, aRawData: String); virtual;
  protected
    procedure DoHTTPRequest;
    function DoGET_Request(const aPath: String): string;
    function DoPOST_Request(const aPath, aMessage: String): string;
    function DoPUT_Request(const aPath, aMessage: String): string;
    function DoPATCH_Request(const aPath, aMessage: String): string;
    function DoDELETE_Request(const aPath: String): string;
  public
    procedure Ping;
  public
    function GET_Request(const aPath: String): string;
    function POST_Request(const aPath, aMessage: String): string;
    function PUT_Request(const aPath, aMessage: String): string;
    function PATCH_Request(const aPath, aMessage: String): string;
    function DELETE_Request(const aPath: String): string;
    { methods }

    { properties }
  public
    property Session_Id: String read FSession_id;
    property Sequence: Integer read FSequence;
    { properties }

    { events }
  private
    FOnDiscordDispatch: TsgcWSDiscordDispatchEvent;
    FOnDiscordEvent: TsgcWSDiscordEvent;
    FOnDiscordReady: TsgcWSDiscordReadyEvent;
    FOnDiscordResumed: TsgcWSDiscordResumedEvent;
    FOnDiscordBeforeReconnect: TsgcWSDiscordDBeforeReconnectEvent;
  protected
    procedure DoDiscordEvent(aEvent: TsgcWSDiscordEvents;
      const aRawData: String); virtual;
    procedure DoDiscordReadyEvent(const aRawData: String); virtual;
    procedure DoDiscordResumedEvent(const aRawData: String); virtual;
    procedure DoDiscordDispatchEvent(const aEvent, aRawData: String); virtual;
    function DoDiscordBeforeReconnectEvent(var aToken, aSession_id: String;
      var aSeq: Integer): Boolean; virtual;
  public
    property OnDiscordEvent: TsgcWSDiscordEvent read FOnDiscordEvent
      write FOnDiscordEvent;
    property OnDiscordReady: TsgcWSDiscordReadyEvent read FOnDiscordReady
      write FOnDiscordReady;
    property OnDiscordResumed: TsgcWSDiscordResumedEvent read FOnDiscordResumed
      write FOnDiscordResumed;
    property OnDiscordDispatch: TsgcWSDiscordDispatchEvent
      read FOnDiscordDispatch write FOnDiscordDispatch;
    property OnDiscordBeforeReconnect: TsgcWSDiscordDBeforeReconnectEvent
      read FOnDiscordBeforeReconnect write FOnDiscordBeforeReconnect;
    { events }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_APIS}

uses
  sgcBase_Helpers, sgcBase_Const;

const
  CS_DISCORD_AUTHORIZATION_BOT = 'Authorization: Bot ';
  CS_DISCORD_VERSION = 6;
  CS_DISCORD_EVENT_READY = 'READY';
  CS_DISCORD_EVENT_RESUMED = 'RESUMED';
  CS_DISCORD_URL = 'https://discordapp.com/api';
  CS_OAUTH2_DISCORD_AUTHORIZE_URL =
    'https://discordapp.com/api/oauth2/authorize';
  CS_OAUTH2_DISCORD_TOKEN_URL = 'https://discordapp.com/api/oauth2/token';

constructor TsgcWS_API_Discord.Create(aOwner: TComponent);
begin
  inherited;
  FDiscordOptions := TsgcWSDiscord_Options.Create;
  FSession_id := '';
end;

destructor TsgcWS_API_Discord.Destroy;
begin
  sgcFree(FOAuth2);
  sgcFree(FDiscordOptions);
  sgcFree(FHTTPClient);
  inherited;
end;

function TsgcWS_API_Discord.DELETE_Request(const aPath: String): string;
begin
  Result := DoDELETE_Request(aPath);
end;

procedure TsgcWS_API_Discord.DoAuthenticateUser;
begin
  if DiscordOptions.Bot then
    DoIdentifyUser(DiscordOptions.BotOptions.Token)
  else
  begin
    OAuth2.OAuth2Options.ClientId := DiscordOptions.ClientId;
    OAuth2.OAuth2Options.ClientSecret := DiscordOptions.ClientSecret;
    OAuth2.AuthorizationServerOptions.AuthURL :=
      CS_OAUTH2_DISCORD_AUTHORIZE_URL;
    OAuth2.AuthorizationServerOptions.TokenURL := CS_OAUTH2_DISCORD_TOKEN_URL;
    OAuth2.AuthorizationServerOptions.Scope.Clear;
    OAuth2.AuthorizationServerOptions.Scope.Add('connections');

    OAuth2.Start;
  end;
end;

procedure TsgcWS_API_Discord.DoBeforeConnect;
begin
  inherited;
  if DiscordOptions.Bot then
    FClient.URL := GetDiscordBotURL
  else
    FClient.URL := GetDiscordURL;
end;

function TsgcWS_API_Discord.DoDELETE_Request(const aPath: String): string;
begin
  DoHTTPRequest;

{$IFDEF INDY10_6_0_5122}
  Result := HTTPClient.Delete(CS_DISCORD_URL + aPath);
{$ELSE}
{$IFDEF INDY10_5_5}
  HTTPClient.Delete(CS_DISCORD_URL + aPath);
{$ENDIF}
{$ENDIF}
end;

function TsgcWS_API_Discord.DoDiscordBeforeReconnectEvent(var aToken,
  aSession_id: String; var aSeq: Integer): Boolean;
begin
  Result := (aToken <> '') and (aSession_id <> '') and (aSeq > 0);
  if Assigned(FOnDiscordBeforeReconnect) then
    FOnDiscordBeforeReconnect(self, aToken, aSession_id, aSeq, Result);
end;

procedure TsgcWS_API_Discord.DoDiscordDISPATCH(const aRawData: String);
var
  vEvent: String;
begin
  if JSON.Node['t'] <> nil then
  begin
    vEvent := JSON.Node['t'].Value;
    if vEvent = CS_DISCORD_EVENT_READY then
    begin
      DoDiscordReadyEvent(aRawData);
      if JSON.Node['d'] <> nil then
      begin
        if JSON.Node['d'].Node['session_id'] <> nil then
          FSession_id := JSON.Node['d'].Node['session_id'].Value;
      end
    end
    else if vEvent = CS_DISCORD_EVENT_RESUMED then
      DoDiscordResumedEvent(aRawData)
    else
      DoDiscordDispatchEvent(vEvent, aRawData);
  end;
end;

procedure TsgcWS_API_Discord.DoDiscordDispatchEvent(const aEvent,
  aRawData: String);
begin
  if Assigned(FOnDiscordDispatch) then
    FOnDiscordDispatch(self, aEvent, aRawData);
end;

procedure TsgcWS_API_Discord.DoDiscordEvent(aEvent: TsgcWSDiscordEvents;
  const aRawData: String);
begin
  if Assigned(FOnDiscordEvent) then
    FOnDiscordEvent(self, aEvent, aRawData);
end;

procedure TsgcWS_API_Discord.DoDiscordHEARTBEAT;
begin
  FClient.WriteData('{"op": 11}');
end;

procedure TsgcWS_API_Discord.DoDiscordHELLO(const aRawData: String);
var
  vInterval: Integer;
begin
  // heartbeat
  if JSON.Node['d'] <> nil then
  begin
    if JSON.Node['d'].Node['heartbeat_interval'] <> nil then
    begin
      vInterval := JSON.Node['d'].Node['heartbeat_interval'].Value;
      if vInterval > 1 then
      begin
        if not FClient.HeartBeat.Enabled then
        begin
          FClient.HeartBeat.Interval := Trunc(vInterval / 1000);
          FClient.HeartBeat.Enabled := True;
          TsgcWSComponent_WSClient_API(FClient).DoStartHeartBeat;
        end;
      end;
    end;
  end;
end;

procedure TsgcWS_API_Discord.DoDiscordReadyEvent(const aRawData: String);
begin
  if Assigned(FOnDiscordReady) then
    FOnDiscordReady(self, aRawData);
end;

procedure TsgcWS_API_Discord.DoDiscordRECONNECT(const aSession_id: String);
var
  vToken, vSession_id: string;
  vSequence: Integer;
begin
  vToken := DiscordOptions.BotOptions.Token;
  vSession_id := aSession_id;
  vSequence := FLastReadSequence;
  FSessionResumed := False;
  if DoDiscordBeforeReconnectEvent(vToken, vSession_id, vSequence) then
  begin
    FClient.WriteData
      (Format('{"op": 6, "d": {"token": "%s", "session_id":"%s", "seq": %d}}',
      [vToken, vSession_id, vSequence]));
    FSessionResumed := True;
  end
  else
    FSession_id := '';
end;

procedure TsgcWS_API_Discord.DoDiscordResumedEvent(const aRawData: String);
begin
  if Assigned(FOnDiscordResumed) then
    FOnDiscordResumed(self, aRawData);
end;

procedure TsgcWS_API_Discord.DoEventConnect(aConnection: TsgcWSConnection);
begin
  inherited;
  if FSession_id <> '' then
    DoDiscordRECONNECT(FSession_id)
end;

procedure TsgcWS_API_Discord.DoEventDisconnect(aConnection: TsgcWSConnection;
  Code: Integer);
begin
  inherited;
  FLastReadSequence := FReadSequence;
end;

procedure TsgcWS_API_Discord.DoEventMessage(aConnection: TsgcWSConnection;
  const Text: string);
begin
  if RawMessages then
    inherited
  else
  begin
    JSON.Clear;
    JSON.Read(Text);

    if JSON.Node['op'] <> nil then
      DoReadEvent(JSON.Node['op'].Value, Text)
    else
      inherited;
  end;
end;

function TsgcWS_API_Discord.DoGET_Request(const aPath: String): string;
begin
  DoHTTPRequest;

  Result := HTTPClient.Get(CS_DISCORD_URL + aPath);
end;

procedure TsgcWS_API_Discord.DoHTTPRequest;
begin
  HTTPClient.Request.CustomHeaders.Clear;
  HTTPClient.Request.CustomHeaders.Add(CS_DISCORD_AUTHORIZATION_BOT +
    DiscordOptions.BotOptions.Token);
  HTTPClient.Request.ContentType := 'application/json';
  HTTPClient.Request.UserAgent := GetUserAgent;
end;

procedure TsgcWS_API_Discord.DoIdentifyUser(const aToken: String);
begin
  FClient.WriteData
    (Format('{"op": 2,"d": {"token": "%s", "properties": {"$os": "%s", "$browser": "%s", "$device": "%s"}}, "intents": %d}',
    [aToken, 'windows', GetUserAgent, CS_APPLICATION_NAME + ' ' + CS_VERSION, DiscordOptions.Intents]));
end;

function TsgcWS_API_Discord.DoKeepAlive: Boolean;
begin
  Result := True;

  Ping;
end;

function TsgcWS_API_Discord.DoPATCH_Request(const aPath,
  aMessage: String): string;
var
  oRequest, oResponse: TStringStream;
begin
  DoHTTPRequest;

  oRequest := TStringStream.Create(aMessage{$IFDEF D2009}, TEncoding.UTF8{$ENDIF});
  Try
    oResponse := TStringStream.Create('');
    Try
{$IFDEF INDY10_6_0_5122}
      HTTPClient.Patch(CS_DISCORD_URL + aPath, oRequest, oResponse);
{$ENDIF}
      Result := oResponse.DataString;
    Finally
      sgcFree(oResponse);
    End;
  Finally
    sgcFree(oRequest);
  End;
end;

function TsgcWS_API_Discord.DoPOST_Request(const aPath,
  aMessage: String): string;
var
  oRequest, oResponse: TStringStream;
begin
  DoHTTPRequest;

  oRequest := TStringStream.Create(aMessage{$IFDEF D2010}, TEncoding.UTF8{$ENDIF});
  Try
    oResponse := TStringStream.Create('');
    Try
      HTTPClient.Post(CS_DISCORD_URL + aPath, oRequest, oResponse);
      Result := oResponse.DataString;
    Finally
      sgcFree(oResponse);
    End;
  Finally
    sgcFree(oRequest);
  End;
end;

function TsgcWS_API_Discord.DoPUT_Request(const aPath,
  aMessage: String): string;
var
  oRequest, oResponse: TStringStream;
begin
  DoHTTPRequest;

  oRequest := TStringStream.Create(aMessage{$IFDEF D2010}, TEncoding.UTF8{$ENDIF});
  Try
    oResponse := TStringStream.Create('');
    Try
      HTTPClient.Put(CS_DISCORD_URL + aPath, oRequest, oResponse);
      Result := oResponse.DataString;
    Finally
      sgcFree(oResponse);
    End;
  Finally
    sgcFree(oRequest);
  End;
end;

procedure TsgcWS_API_Discord.DoReadEvent(const aEvent, aRawData: String);
var
  vEvent: Integer;
  vDiscordEvent: TsgcWSDiscordEvents;
begin
  TryStrToInt(aEvent, vEvent);

  DoReadSequence;

  vDiscordEvent := disDispatch;
  case vEvent of
    0:
      begin
        vDiscordEvent := disDispatch;
        DoDiscordDISPATCH(aRawData);
      end;
    1:
      begin
        vDiscordEvent := disHeartBeat;
        DoDiscordHEARTBEAT;
      end;
    2:
      vDiscordEvent := disIdentify;
    3:
      vDiscordEvent := disStatusUpdate;
    4:
      vDiscordEvent := disVoiceStateUpdate;
    5:
      vDiscordEvent := disVoiceServerPing;
    6:
      vDiscordEvent := disResume;
    7:
      vDiscordEvent := disReconnect;
    8:
      vDiscordEvent := disRequestGuildMembers;
    9:
      begin
        vDiscordEvent := disInvalidSession;
        DoIdentifyUser(DiscordOptions.BotOptions.Token);
      end;
    10:
      begin
        vDiscordEvent := disHello;
        DoDiscordHELLO(aRawData);
        if not FSessionResumed then
          DoAuthenticateUser;
      end;
    11:
      vDiscordEvent := disHeartBeatACK;
  end;

  DoDiscordEvent(vDiscordEvent, aRawData);
end;

procedure TsgcWS_API_Discord.DoReadSequence;
begin
  if JSON.Node['s'] <> nil then
  begin
    if JSON.Node['s'].Value = 'null' then
      FSequence := 1
    else
      FSequence := JSON.Node['s'].Value;
    FReadSequence := FSequence;
  end;
end;

function TsgcWS_API_Discord.GetDiscordBotURL: String;
var
  oJSON: TsgcJSON;
  vJSON: string;
begin
  Result := '';

  HTTPClient.Request.CustomHeaders.Clear;
  HTTPClient.Request.CustomHeaders.Add('Authorization: Bot ' +
    DiscordOptions.BotOptions.Token);
  HTTPClient.Request.UserAgent := GetUserAgent;
  vJSON := HTTPClient.Get(CS_DISCORD_URL + '/gateway/bot');

  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.Read(vJSON);
    if oJSON.Node['url'] <> nil then
      Result := oJSON.Node['url'].Value;
  Finally
    sgcFree(oJSON);
  End;
end;

function TsgcWS_API_Discord.GetDiscordURL: String;
var
  oJSON: TsgcJSON;
  vJSON: string;
begin
  Result := '';

  vJSON := HTTPClient.Get(CS_DISCORD_URL + '/gateway');

  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.Read(vJSON);
    if oJSON.Node['url'] <> nil then
      Result := oJSON.Node['url'].Value;
  Finally
    sgcFree(oJSON);
  End;
end;

function TsgcWS_API_Discord.GetHTTPClient: TsgcIdHTTP;
begin
  if not Assigned(FHTTPClient) then
  begin
    FHTTPClient := TsgcIdHTTP.Create(nil);
    if Assigned(Client) then
      FHTTPClient.TLSOptions.Assign(Client.TLSOptions);
  end;
  Result := FHTTPClient;
end;

function TsgcWS_API_Discord.GetJSON: TsgcJSON;
begin
  if not Assigned(FJSON) then
    FJSON := TsgcJSON.Create(self);
  Result := FJSON;
end;

function TsgcWS_API_Discord.GetOAuth2: TsgcHTTPComponentClient_OAuth2;
begin
  if not Assigned(FOAuth2) then
  begin
    FOAuth2 := TsgcHTTPComponentClient_OAuth2.Create(nil);
    FOAuth2.OnAfterAccessToken := OnAfterAccessTokenEvent;
  end;
  Result := FOAuth2;
end;

function TsgcWS_API_Discord.GET_Request(const aPath: String): string;
begin
  Result := DoGET_Request(aPath);
end;

function TsgcWS_API_Discord.GetUserAgent: string;
begin
  Result := DiscordOptions.BotOptions.BotName + ' (' +
    DiscordOptions.BotOptions.BotURL + ',' +
    IntToStr(DiscordOptions.Version) + ')';
end;

procedure TsgcWS_API_Discord.OnAfterAccessTokenEvent(Sender: TObject;
  const Access_Token, Token_Type, Expires_In, Refresh_Token, Scope,
  RawParams: String; var Handled: Boolean);
begin
  DoIdentifyUser(Access_Token);
end;

function TsgcWS_API_Discord.PATCH_Request(const aPath,
  aMessage: String): string;
begin
  Result := DoPATCH_Request(aPath, aMessage);
end;

procedure TsgcWS_API_Discord.Ping;
begin
  FClient.WriteData('{"op": 1,"d": ' + IntToStr(FSequence) + '}');
  FSequence := FSequence + 1;
end;

function TsgcWS_API_Discord.POST_Request(const aPath, aMessage: String): string;
begin
  Result := DoPOST_Request(aPath, aMessage);
end;

function TsgcWS_API_Discord.PUT_Request(const aPath, aMessage: String): string;
begin
  Result := DoPUT_Request(aPath, aMessage);
end;

procedure TsgcWS_API_Discord.SetDiscordOptions(const Value
  : TsgcWSDiscord_Options);
begin
  if Assigned(FDiscordOptions) then
    FDiscordOptions.Assign(Value);
end;

constructor TsgcWSDiscord_Options.Create;
begin
  inherited;
  FBotOptions := TsgcWSDiscordBot_Options.Create;
  Bot := True;
  Version := CS_DISCORD_VERSION;
  Intents := 0;
end;

destructor TsgcWSDiscord_Options.Destroy;
begin
  sgcFree(FBotOptions);
  inherited;
end;

procedure TsgcWSDiscord_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSDiscord_Options then
  begin
    Bot := TsgcWSDiscord_Options(aSource).Bot;
    BotOptions := TsgcWSDiscord_Options(aSource).BotOptions;
    Version := TsgcWSDiscord_Options(aSource).Version;
    ClientId := TsgcWSDiscord_Options(aSource).ClientId;
    ClientSecret := TsgcWSDiscord_Options(aSource).ClientSecret;
    Intents := TsgcWSDiscord_Options(aSource).Intents;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSDiscord_Options.SetBotOptions(const Value
  : TsgcWSDiscordBot_Options);
begin
  if Assigned(FBotOptions) then
    FBotOptions.Assign(Value);
end;

constructor TsgcWSDiscordBot_Options.Create;
begin
  inherited;
  BotName := 'DiscordBot';
  BotURL := 'none';
end;

procedure TsgcWSDiscordBot_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSDiscordBot_Options then
  begin
    Token := TsgcWSDiscordBot_Options(aSource).Token;
    BotName := TsgcWSDiscordBot_Options(aSource).BotName;
    BotURL := TsgcWSDiscordBot_Options(aSource).BotURL;
  end
  else
    inherited Assign(aSource);
end;

{$ENDIF}

end.
