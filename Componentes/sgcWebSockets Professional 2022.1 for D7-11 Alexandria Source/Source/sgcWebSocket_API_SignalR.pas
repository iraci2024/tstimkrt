{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_API_SignalR;

interface

{$I sgcVer.inc}
{$IFDEF SGC_APIS}

uses
  Classes, SysUtils, StrUtils,
  // sgc
  sgcWebSocket_Classes, sgcWebSocket_Types, sgcJSON;

type

  TsgcWSSignalRConnectEvent = procedure(Sender: TObject; MessageId: String;
    aData: String) of object;
  TsgcWSSignalRMessageEvent = procedure(Sender: TObject; MessageId: String;
    aData: String) of object;
  TsgcWSSignalRResultEvent = procedure(Sender: TObject; InvocationId: String;
    Result: String; Error: String) of object;
  TsgcWSSignalRKeepAliveEvent = procedure(Sender: TObject) of object;
  TsgcWSSignalRErrorEvent = procedure(Sender: TObject; Error: String) of object;
  TsgcWSSignalRDisconnectEvent = procedure(Sender: TObject; aCloseCode: Integer)
      of object;


  TsgcWSSignalR_Options = class(TPersistent)
  private
    FHubs: TStringList;
    FProtocolVersion: TwsSignalRProtocolVersions;
    FUserAgent: String;
    function GetConnectionData: String;
    function GetJSONHubs: String;
    procedure SetHubs(const Value: TStringList);
  public
    procedure Assign(aSource: TPersistent); override;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    property ConnectionData: String read GetConnectionData;
  published
    property Hubs: TStringList read FHubs write SetHubs;
    property ProtocolVersion
      : TwsSignalRProtocolVersions read FProtocolVersion write
      FProtocolVersion;
    property UserAgent: String read FUserAgent write FUserAgent;
  end;

  TsgcWSSignalR_Negotiation = class(TPersistent)
  private
    FConnectionId: string;
    FConnectionTimeout: string;
    FConnectionToken: string;
    FDisconnectTimeout: string;
    FKeepAliveTimeout: string;
    FLongPollDelay: string;
    FProtocolVersion: string;
    FTransportConnectTimeout: string;
    FTryWebSockets: Boolean;
    FURL: String;
    procedure SetConnectionToken(const Value: string);
  public
    procedure Assign(aSource: TPersistent); override;
  public
    property ConnectionId: string read FConnectionId write FConnectionId;
    property ConnectionTimeout
      : string read FConnectionTimeout write FConnectionTimeout;
    property ConnectionToken
      : string read FConnectionToken write SetConnectionToken;
    property DisconnectTimeout
      : string read FDisconnectTimeout write FDisconnectTimeout;
    property KeepAliveTimeout
      : string read FKeepAliveTimeout write FKeepAliveTimeout;
    property LongPollDelay: string read FLongPollDelay write FLongPollDelay;
    property ProtocolVersion
      : string read FProtocolVersion write FProtocolVersion;
    property TransportConnectTimeout
      : string read FTransportConnectTimeout write
      FTransportConnectTimeout;
    property TryWebSockets: Boolean read FTryWebSockets write FTryWebSockets;
    property URL: String read FURL write FURL;
  end;

  TsgcWS_API_SignalR = class(TsgcWSAPI_client)
    { from TsgcWSComponent_Base }
  protected
    procedure DoNotifyConnect(aConnection: TsgcWSConnection); override;
    procedure DoNotifyMessage(aConnection: TsgcWSConnection); override;
    procedure DoNotifyError(aConnection: TsgcWSConnection); override;
    procedure DoNotifyException(aConnection: TsgcWSConnection); override;
    procedure DoNotifyDisconnect(aConnection: TsgcWSConnection); override;
    { from TsgcWSComponent_Base }

    { from TsgcWSAPI }
  protected
    procedure DoBeforeConnect; override;
    { from TsgcWSAPI }

    { negotiation }
  private
    function GetProtocolVersion: String;
  private
    function DoHTTP(const aMethod: String; const aQuery: String = ''): String;
  private
    procedure DoHTTPNegotiation;
    procedure DoWebSocketURL;
    procedure DoHTTPStart;
    { negotiation }

    { json }
  private
    FJSON: TsgcJSON;
    function GetJSON: TsgcJSON;
    procedure SetNegotiation(const Value: TsgcWSSignalR_Negotiation);
    procedure SetSignalR(const Value: TsgcWSSignalR_Options);
  protected
    function GetJSONData: string;
    function GetJSONMessageId: String;
    function GetJSONError: string;
    function GetJSONInvocationId: string;
    function GetJSONResult: string;
  protected
    property JSON: TsgcJSON read GetJSON write FJSON;
    { json }

    { properties }
  private
    FSignalR: TsgcWSSignalR_Options;
    FNegotiation: TsgcWSSignalR_Negotiation;
    FRawParams: string;
  public
    property SignalR: TsgcWSSignalR_Options read FSignalR write SetSignalR;
    property Negotiation: TsgcWSSignalR_Negotiation read FNegotiation write
        SetNegotiation;
    { properties }

    { constructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor }

    { methods }
  public
    procedure WriteData(const aText: String);
    { methods }

    { events }
  private
    FOnSignalRConnect: TsgcWSSignalRConnectEvent;
    FOnSignalRDisconnect: TsgcWSSignalRDisconnectEvent;
    FOnSignalRError: TsgcWSSignalRErrorEvent;
    FOnSignalRMessage: TsgcWSSignalRMessageEvent;
    FOnSignalRKeepAlive: TsgcWSSignalRKeepAliveEvent;
    FOnSignalRResult: TsgcWSSignalRResultEvent;
  protected
    procedure DoEventSignalRConnect(const aMessageId, aData: string); virtual;
    procedure DoEventSignalRMessage(const aMessageId, aData: string); virtual;
    procedure DoEventSignalRKeepAlive; virtual;
    procedure DoEventSignalRResult(const aInvocationId, aResult, aError: string);
        virtual;
    procedure DoEventSignalRError(const aError: String); virtual;
    procedure DoEventSignalRDisconnect(const aCloseCode: Integer); virtual;
  protected
    property OnSignalRConnect
      : TsgcWSSignalRConnectEvent read FOnSignalRConnect write
      FOnSignalRConnect;
    property OnSignalRMessage
      : TsgcWSSignalRMessageEvent read FOnSignalRMessage write
      FOnSignalRMessage;
    property OnSignalRKeepAlive
      : TsgcWSSignalRKeepAliveEvent read FOnSignalRKeepAlive write
      FOnSignalRKeepAlive;
    property OnSignalRResult
      : TsgcWSSignalRResultEvent read FOnSignalRResult write
      FOnSignalRResult;
    property OnSignalRError: TsgcWSSignalRErrorEvent read FOnSignalRError write
        FOnSignalRError;
    property OnSignalRDisconnect: TsgcWSSignalRDisconnectEvent read
        FOnSignalRDisconnect write FOnSignalRDisconnect;
    { events }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_APIS}

uses
  // indy
  {$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
  // sgc
  sgcWebSocket_Helpers, sgcBase_Helpers, sgcWebSocket_Const,
  sgcHTTP_Client, sgcWebSocket_Client;

function URIEncode(const aValue: String): string;
var
  i, j: integer;
  oBytes: TIdbytes;
begin
  result := '';
  {$IFDEF NEXTGEN}
  oBytes := TIdBytes(TEncoding.UTF8.GetBytes(aValue));
  for i := 0 to high(oBytes) do begin
  {$ELSE}
  {$IFDEF INDY10_5_5}
  oBytes := ToBytes(aValue, {$IFDEF INDY10_6}IndyTextEncoding_UTF8{$ELSE}TIdTextEncoding.UTF8{$ENDIF});
  {$ELSE}
  oBytes := ToBytes(UTF8Encode(aValue));
  {$ENDIF}
  for i := 0 to length(oBytes) - 1 do begin
  {$ENDIF}
    j := ord(oBytes[i]);
    case j of
    ord('0')..ord('9'),ord('a')..ord('z'),ord('A')..ord('Z'),
    ord('_'),ord('-'),ord('.'),ord('~'):
      Result := Result + WideChar(oBytes[i]);
    else
      Result := Result + '%' + IntToHex(oBytes[i], 2);
    end;
  end;
end;

constructor TsgcWS_API_SignalR.Create(aOwner: TComponent);
begin
  inherited;
  FSignalR := TsgcWSSignalR_Options.Create;
  FNegotiation := TsgcWSSignalR_Negotiation.Create;
end;

destructor TsgcWS_API_SignalR.Destroy;
begin
  sgcFree(FJSON);
  sgcFree(FNegotiation);
  sgcFree(FSignalR);
  inherited;
end;

procedure TsgcWS_API_SignalR.DoBeforeConnect;
begin
  inherited;
  FRawParams := '/';
  if Client.InheritsFrom(TsgcWSClient) then
  begin
    FRawParams := TsgcWSClient(Client).Options.Parameters;
    if RightStr(FRawParams, 1) <> '/' then
      FRawParams := FRawParams + '/';
  end;

  DoHTTPNegotiation;
  DoWebSocketURL;
end;

procedure TsgcWS_API_SignalR.DoEventSignalRConnect(const aMessageId, aData:
    string);
begin
  if Assigned(FOnSignalRConnect) then
    FOnSignalRConnect(self, aMessageId, aData);
end;

procedure TsgcWS_API_SignalR.DoEventSignalRDisconnect(const aCloseCode:
    Integer);
begin
  if Assigned(FOnSignalRDisconnect) then
    FOnSignalRDisconnect(self, aCloseCode);
end;

procedure TsgcWS_API_SignalR.DoEventSignalRError(const aError: String);
begin
  if Assigned(FOnSignalRError) then
    FOnSignalRError(self, aError);
end;

procedure TsgcWS_API_SignalR.DoEventSignalRKeepAlive;
begin
  if Assigned(FOnSignalRKeepAlive) then
    FOnSignalRKeepAlive(self);
end;

procedure TsgcWS_API_SignalR.DoEventSignalRMessage(const aMessageId, aData:
    string);
begin
  if Assigned(FOnSignalRMessage) then
    FOnSignalRMessage(self, aMessageId, aData);
end;

procedure TsgcWS_API_SignalR.DoEventSignalRResult(const aInvocationId, aResult,
    aError: string);
begin
  if Assigned(FOnSignalRResult) then
    FOnSignalRResult(self, aInvocationId, aResult, aError);
end;

function TsgcWS_API_SignalR.DoHTTP(const aMethod: String;
  const aQuery: String = ''): String;
var
  oHTTP: TsgcIdHTTP;
  oStream: TStringStream;
  vURL: String;
begin
  oHTTP := TsgcIdHTTP.Create(nil);
  oHTTP.HandleRedirects := True;
  oStream := TStringStream.Create('');
  Try
    vURL := 'http://';
    if Client.TLS then
      vURL := 'https://';
    vURL := vURL + Client.Host + ':' + IntToStr(Client.Port)
      + FRawParams + 'signalr/' + aMethod;
    if aQuery <> '' then
      vURL := vURL + '?' + aQuery;

    if Assigned(Client) then
      oHTTP.TLSOptions.Assign(Client.TLSOptions);
    oHTTP.ReadTimeout := 10000;
    if SignalR.UserAgent <> '' then
      oHTTP.Request.UserAgent := SignalR.UserAgent;
    oHTTP.Get(vURL, oStream);
    Result := oStream.DataString Finally sgcFree(oStream);
    sgcFree(oHTTP);
  End;
end;

procedure TsgcWS_API_SignalR.DoNotifyConnect(aConnection: TsgcWSConnection);
begin
  if Negotiation.ProtocolVersion > '1.2' then
    DoHTTPStart;
  inherited;
end;

procedure TsgcWS_API_SignalR.DoHTTPNegotiation;
var
  oJSON: TsgcJSON;
  vResponse: String;

  function GetJSONValue(const aNode: String): Variant;
  begin
    Result := '';
    if oJSON.Node[aNode] <> nil then
      Result := oJSON.Node[aNode].Value;
  end;

begin
  vResponse := DoHTTP('negotiate', 'clientProtocol=' + GetProtocolVersion);

  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.Read(vResponse);

    Negotiation.ConnectionId := GetJSONValue('ConnectionId');
    Negotiation.ConnectionTimeout := GetJSONValue('ConnectionTimeout');
    Negotiation.ConnectionToken := GetJSONValue('ConnectionToken');
    Negotiation.DisconnectTimeout := GetJSONValue('DisconnectTimeout');
    Negotiation.KeepAliveTimeout := GetJSONValue('KeepAliveTimeout');
    Negotiation.LongPollDelay := GetJSONValue('LongPollDelay');
    Negotiation.ProtocolVersion := GetJSONValue('ProtocolVersion');
    Negotiation.TransportConnectTimeout := GetJSONValue
      ('TransportConnectTimeout');
    if oJSON.Node['TryWebSockets'] <> nil then
      Negotiation.TryWebSockets := GetJSONValue('TryWebSockets');
    Negotiation.URL := GetJSONValue('URL');

    if not Negotiation.TryWebSockets then
      raise TsgcWSException.Create(S_ERROR_WEBSOCKETS_PROTOCOL_NOT_SUPPORTED);
  Finally
    sgcFree(oJSON);
  End;
end;

procedure TsgcWS_API_SignalR.DoHTTPStart;
var
  oJSON: TsgcJSON;
  vResponse: String;
begin
  vResponse := DoHTTP('start',
    'transport=webSockets' + '&clientProtocol=' +
      Negotiation.ProtocolVersion + '&connectionToken=' +
      Negotiation.ConnectionToken + '&connectionData=' +
      SignalR.ConnectionData) + '&_=' + FormatDateTime
    ('yyyymmddhhnnsszzz', Now);

  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.Read(vResponse);

    if oJSON.Node['Response'] <> nil then
    begin
      if oJSON.Node['Response'].Value <> 'started' then
        raise TsgcWSException.Create(S_ERROR_WEBSOCKETS_PROTOCOL_NOT_SUPPORTED);
    end;
  Finally
    sgcFree(oJSON);
  End;
end;

procedure TsgcWS_API_SignalR.DoNotifyDisconnect(aConnection: TsgcWSConnection);
begin
  if Assigned(FClient) then
    if FClient.InheritsFrom(TsgcWSClient) then
      TsgcWSClient(Client).Options.Parameters := FRawParams;
  inherited;
  DoEventSignalRDisconnect(aConnection.CloseCode);
end;

procedure TsgcWS_API_SignalR.DoNotifyError(aConnection: TsgcWSConnection);
begin
  DoEventSignalRError(aConnection.MsgError);
end;

procedure TsgcWS_API_SignalR.DoNotifyException(aConnection: TsgcWSConnection);
begin
  DoEventSignalRError(aConnection.MsgException);
end;

procedure TsgcWS_API_SignalR.DoNotifyMessage(aConnection: TsgcWSConnection);
begin
  if not Assigned(aConnection) then
    exit;

  if aConnection.MsgReceived <> '' then
  begin
    if aConnection.MsgReceived = '{}' then
      DoEventSignalRKeepAlive
    else
    begin
      JSON.Read(aConnection.MsgReceived);

      if JSON.Node['S'] <> nil then
        DoEventSignalRConnect(GetJSONMessageId, GetJSONData)
      else if JSON.Node['I'] <> nil then
        DoEventSignalRResult(GetJSONInvocationId, GetJSONResult, GetJSONError)
      else if JSON.Node['M'] <> nil then
        DoEventSignalRMessage(GetJSONMessageId, GetJSONData)
      else
        inherited;
    end;
  end;
end;

procedure TsgcWS_API_SignalR.DoWebSocketURL;
var
  vURL: String;
begin
  vURL := 'ws';
  if Client.TLS then
    vURL := 'wss';

  vURL := vURL + '://' + Client.Host + ':' + IntToStr(Client.Port) +
    FRawParams + 'signalr/connect?transport=webSockets' + '&clientProtocol=' +
    Negotiation.ProtocolVersion + '&connectionToken=' +
    Negotiation.ConnectionToken + '&connectionData=' +
    SignalR.ConnectionData + '&tid=100';
  Client.URL := vURL;
end;

function TsgcWS_API_SignalR.GetJSON: TsgcJSON;
begin
  if not Assigned(FJSON) then
    FJSON := TsgcJSON.Create(nil);
  Result := FJSON;
end;

function TsgcWS_API_SignalR.GetJSONData: string;
begin
  Result := '';
  if JSON.Node['M'] <> nil then
    Result := JSON.Node['M'].Value;
end;

function TsgcWS_API_SignalR.GetJSONError: string;
begin
  Result := '';
  if JSON.Node['E'] <> nil then
    Result := JSON.Node['E'].Value;
end;

function TsgcWS_API_SignalR.GetJSONInvocationId: string;
begin
  Result := '';
  if JSON.Node['I'] <> nil then
    Result := JSON.Node['I'].Value;
end;

function TsgcWS_API_SignalR.GetJSONMessageId: String;
begin
  Result := '';
  if JSON.Node['C'] <> nil then
    Result := JSON.Node['C'].Value;
end;

function TsgcWS_API_SignalR.GetJSONResult: string;
begin
  Result := '';
  if JSON.Node['R'] <> nil then
    Result := JSON.Node['R'].Value;
end;

function TsgcWS_API_SignalR.GetProtocolVersion: String;
begin
  case self.SignalR.ProtocolVersion of
    srpt1_2:
      Result := '1.2';
    srpt1_3:
      Result := '1.3';
    srpt1_4:
      Result := '1.4';
    srpt1_5:
      Result := '1.5';
    srpt2_1:
      Result := '2.1';
  end;
end;

procedure TsgcWS_API_SignalR.SetNegotiation(const Value:
    TsgcWSSignalR_Negotiation);
begin
  FNegotiation.Assign(Value);
end;

procedure TsgcWS_API_SignalR.SetSignalR(const Value: TsgcWSSignalR_Options);
begin
  FSignalR.Assign(Value);
end;

procedure TsgcWS_API_SignalR.WriteData(const aText: String);
begin
  FClient.WriteData(aText);
end;

constructor TsgcWSSignalR_Options.Create;
begin
  inherited;
  FHubs := TStringList.Create;
  ProtocolVersion := srpt1_2;
end;

destructor TsgcWSSignalR_Options.Destroy;
begin
  sgcFree(FHubs);
  inherited;
end;

procedure TsgcWSSignalR_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSSignalR_Options then
  begin
    Hubs := TsgcWSSignalR_Options(aSource).Hubs;
    ProtocolVersion := TsgcWSSignalR_Options(aSource).ProtocolVersion;
    UserAgent := TsgcWSSignalR_Options(aSource).UserAgent;
  end
  else
    inherited Assign(aSource);
end;

function TsgcWSSignalR_Options.GetConnectionData: String;
begin
  Result := URIEncode(GetJSONHubs);
end;

function TsgcWSSignalR_Options.GetJSONHubs: String;
var
  i: Integer;
  oJSON: TsgcJSON;
begin
  Result := '';
  if Hubs.count > 0 then
  begin
    oJSON := TsgcJSON.Create(nil);
    Try
      oJSON.IsArray := True;
      for i := 0 to Hubs.count - 1 do
        oJSON.AddObject(Hubs[i]).JSONObject.AddPair('name', Hubs[i]);
      Result := oJSON.Text;
    Finally
      sgcFree(oJSON);
    End;
  end;
end;

procedure TsgcWSSignalR_Options.SetHubs(const Value: TStringList);
begin
  FHubs.Assign(Value);
end;

procedure TsgcWSSignalR_Negotiation.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSSignalR_Negotiation then
  begin
    ConnectionId := TsgcWSSignalR_Negotiation(aSource).ConnectionId;
    ConnectionTimeout := TsgcWSSignalR_Negotiation(aSource).ConnectionTimeout;
    ConnectionToken := TsgcWSSignalR_Negotiation(aSource).ConnectionToken;
    DisconnectTimeout := TsgcWSSignalR_Negotiation(aSource).DisconnectTimeout;
    KeepAliveTimeout := TsgcWSSignalR_Negotiation(aSource).KeepAliveTimeout;
    LongPollDelay := TsgcWSSignalR_Negotiation(aSource).LongPollDelay;
    ProtocolVersion := TsgcWSSignalR_Negotiation(aSource).ProtocolVersion;
    TransportConnectTimeout := TsgcWSSignalR_Negotiation(aSource)
      .TransportConnectTimeout;
    TryWebSockets := TsgcWSSignalR_Negotiation(aSource).TryWebSockets;
    URL := TsgcWSSignalR_Negotiation(aSource).URL;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSSignalR_Negotiation.SetConnectionToken(const Value: string);
begin
  FConnectionToken := Value;

  FConnectionToken := sgcStringReplace(FConnectionToken, '/', '%2F');
  FConnectionToken := sgcStringReplace(FConnectionToken, '+', '%2B');
end;

{$ENDIF}

end.
