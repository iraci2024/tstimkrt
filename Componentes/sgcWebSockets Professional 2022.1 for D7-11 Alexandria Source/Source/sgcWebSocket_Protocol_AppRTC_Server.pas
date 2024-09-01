{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Protocol_AppRTC_Server;

interface

{$I sgcVer.inc}
{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils,
{$IFDEF MSWINDOWS}Windows,{$ENDIF}
{$IFDEF NEXTGEN}System.Generics.Collections, {$ELSE}Contnrs, {$ENDIF}
  // sgc
  sgcJSON, sgcWebSocket_Classes, sgcWebSocket_Protocol_Base_Server,
  sgcWebSocket_HTTPResponse;

type

  TsgcMessages = class(TStringList)
  private
    function GetJSONList: String;
  public
    property JSONList: String read GetJSONList;
  end;

  TsgcRoomClient = class
  private
    FActive: Boolean;
    FClientId: string;
    FGUID: String;
    FInitiator: Boolean;
    FMessages: TsgcMessages;
    function GetMessages: TsgcMessages;
  public
    destructor Destroy; override;
  public
    property Active: Boolean read FActive write FActive;
    property ClientId: string read FClientId write FClientId;
    property GUID: String read FGUID write FGUID;
    property Initiator: Boolean read FInitiator write FInitiator;
    property Messages: TsgcMessages read GetMessages write FMessages;
  end;

  TsgcRoom = class
  private
    FRoomId: string;
    FClient1: TsgcRoomClient;
    FClient2: TsgcRoomClient;
    function GetClient1: TsgcRoomClient;
    function GetClient2: TsgcRoomClient;
  public
    destructor Destroy; override;
  public
    property RoomId: string read FRoomId write FRoomId;
    property Client1: TsgcRoomClient read GetClient1 write FClient1;
    property Client2: TsgcRoomClient read GetClient2 write FClient2;
  end;

  TsgcRoomList = class(TsgcQueueList)
  end;

  TsgcWSAppRTCAudio = class(TPersistent)
  private
    FEnabled: Boolean;
  public
    constructor Create;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

  TsgcWSAppRTCVideo = class(TPersistent)
  private
    FEnabled: Boolean;
    FMinHeight: Integer;
    FMinWidth: Integer;
  public
    constructor Create;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property MinHeight: Integer read FMinHeight write FMinHeight;
    property MinWidth: Integer read FMinWidth write FMinWidth;
  end;

  TsgcWSAppRTCMediaConstraints = class(TPersistent)
  private
    FAudio: TsgcWSAppRTCAudio;
    FVideo: TsgcWSAppRTCVideo;
    procedure SetAudio(const Value: TsgcWSAppRTCAudio);
    procedure SetVideo(const Value: TsgcWSAppRTCVideo);
  private
    function GetJSONText: string;
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Audio: TsgcWSAppRTCAudio read FAudio write SetAudio;
    property Video: TsgcWSAppRTCVideo read FVideo write SetVideo;
  end;

  TsgcWSAppRTC_Options = class(TPersistent)
  private
    FIceServers: TStringList;
    FMediaConstraints: TsgcWSAppRTCMediaConstraints;
    FRoomLink: String;
    FWebSocketURL: String;
    procedure SetIceServers(const Value: TStringList);
    procedure SetMediaConstraints(const Value: TsgcWSAppRTCMediaConstraints);
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property IceServers: TStringList read FIceServers write SetIceServers;
    property MediaConstraints: TsgcWSAppRTCMediaConstraints
      read FMediaConstraints write SetMediaConstraints;
    property RoomLink: String read FRoomLink write FRoomLink;
    property WebSocketURL: String read FWebSocketURL write FWebSocketURL;
  end;

  TsgcWSProtocol_AppRTC_Server = class(TsgcWSProtocol_Subscription_Server_Base)

    { from TsgcWSComponent }
  protected
    procedure DoEventConnect(aConnection: TsgcWSConnection); override;
    procedure DoEventMessage(aConnection: TsgcWSConnection;
      const Text: string); override;
    procedure DoEventDisconnect(aConnection: TsgcWSConnection;
      Code: Integer); override;
    { from TsgcWSComponent }

    { http request }
  protected
    function GetHTTPResponse(const aConnection: TsgcWSConnection;
      const aPath, aBody: String): String; override;
    { http request }

    { JSON }
  private
    FJSON: TsgcJSON;
    function GetJSON: TsgcJSON;
  protected
    property JSON: TsgcJSON read GetJSON write FJSON;
    { JSON }

    { properties }
  private
    FAppRTC: TsgcWSAppRTC_Options;
    procedure SetAppRTC(const Value: TsgcWSAppRTC_Options);
  public
    property AppRTC: TsgcWSAppRTC_Options read FAppRTC write SetAppRTC;
    { properties }

    { webrtc subscriptions }
  private
    FRooms: TsgcRoomList;
    function GetRooms: TsgcRoomList;
  protected
    function GetRoomById(const aRoomId: string): TsgcRoom; virtual;
    function NewRoomById(const aRoomId: string): TsgcRoom; virtual;
  protected
    function GetGUIDByPeerGUID(aConnectionGUID: String): string; virtual;
    procedure ClearRoomClientByGUID(aConnectionGUID: String);
    procedure ClearMessagesRoomClientByGUID(aConnectionGUID: String);
  public
    property Rooms: TsgcRoomList read GetRooms write FRooms;
    { webrtc subscriptions }

    { methods }
  protected
    procedure DoRegister(aGuid, aRoomId, aClientId: String);
    procedure DoSend(aGuid, aText: String);
  protected
    function DoJoin(const aRoomId: String): String;
    function DoMessage(const aRoomId, aClientId, aBody: String): String;
    { methods }

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TsgcWSProtocol_JS_AppRTC = class(TsgcWSHTTPResponse_Base)
  protected
    function GetResponse: string; override;
  public
    class function GetFileName: string; override;
  end;

  TsgcWSProtocol_CSS_AppRTC = class(TsgcWSHTTPResponse_Base)
  protected
    function GetResponse: string; override;
  public
    class function GetFileName: string; override;
  end;

{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses
  sgcWebSocket_Const, sgcBase_Helpers, sgcWebSocket_Helpers, sgcWebSocket_Resources,
  sgcWebSocket_Server, sgcWebSocket_Classes_SyncObjs;

constructor TsgcWSProtocol_AppRTC_Server.Create(aOwner: TComponent);
begin
  inherited;
  FProtocol := CS_PROTOCOL_APPRTC;
  FAppRTC := TsgcWSAppRTC_Options.Create;
end;

destructor TsgcWSProtocol_AppRTC_Server.Destroy;
begin
  sgcFree(FAppRTC);
  sgcFree(FRooms);
  inherited;
end;

procedure TsgcWSProtocol_AppRTC_Server.ClearMessagesRoomClientByGUID(
    aConnectionGUID: String);
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  oList := Rooms.LockList;
  Try
    for i := 0 to oList.Count - 1 do
    begin
      if TsgcRoom(oList[i]).Client1.GUID = aConnectionGUID then
      begin
        TsgcRoom(oList[i]).Client1.Messages.Clear;
        break;
      end
      else if TsgcRoom(oList[i]).Client2.GUID = aConnectionGUID then
      begin
        TsgcRoom(oList[i]).Client2.Messages.Clear;
        break;
      end;
    end;
  Finally
    Rooms.UnlockList;
  End;
end;

procedure TsgcWSProtocol_AppRTC_Server.DoEventConnect
  (aConnection: TsgcWSConnection);
begin
  inherited;

end;

procedure TsgcWSProtocol_AppRTC_Server.DoEventDisconnect
  (aConnection: TsgcWSConnection; Code: Integer);
begin
  inherited;
  // ... Unsubscribe
end;

procedure TsgcWSProtocol_AppRTC_Server.DoEventMessage
  (aConnection: TsgcWSConnection; const Text: string);
begin
  if DoRawMessage(aConnection, Text) then
    exit;

  JSON.Read(Text);

  if JSON.Node['cmd'] <> nil then
  begin
    if JSON.Node['cmd'].Value = 'register' then
      DoRegister(aConnection.GUID, JSON.Node['roomid'].Value,
        JSON.Node['clientid'].Value)
    else if JSON.Node['cmd'].Value = 'send' then
      DoSend(aConnection.GUID, Text);
  end;
end;

function TsgcWSProtocol_AppRTC_Server.DoJoin(const aRoomId: String): String;
var
  oRoom: TsgcRoom;
  vInitiator: String;
  vClientId: String;
  vMessages: String;
begin
  oRoom := GetRoomById(aRoomId);
  if not Assigned(oRoom) then
    oRoom := NewRoomById(aRoomId);

  vClientId := FormatDateTime('hhnnsszzz', Now);
  if not oRoom.Client1.Initiator then
  begin
    if oRoom.Client2.Active = False then
      vInitiator := 'true'
    else
      vInitiator := 'false';
    oRoom.Client1.ClientId := vClientId;
    oRoom.Client1.Active := True;
    oRoom.Client1.Initiator := True;
    if oRoom.Client2.Active then
      vMessages := oRoom.Client2.Messages.JSONList
    else
      vMessages := '[]';
  end
  else
  begin
    vInitiator := 'false';
    oRoom.Client2.ClientId := vClientId;
    oRoom.Client2.Active := True;
    if oRoom.Client1.Active then
      vMessages := oRoom.Client1.Messages.JSONList
    else
      vMessages := '[]';
  end;

  result := '{"params": {' + '"is_initiator": "' + vInitiator + '", ' +
    '"room_link": "' + AppRTC.RoomLink + aRoomId + '", ' +
    '"version_info": "{\"gitHash\": \"20cdd7652d58c9cf47ef92ba0190a5505760dc05\", '
    + '\"branch\": \"master\", ' +
    '\"time\": \"Fri Mar 9 17:06:42 2019 +0100\"}", ' + '"messages": ' +
    vMessages + ', ' + '"error_messages": [], ' + '"client_id": "' + vClientId +
    '", ' + '"ice_server_transports": "", ' +
    '"bypass_join_confirmation": "false", ' + '"wss_url": "' +
    AppRTC.WebSocketURL + '", ' + '"media_constraints": "' +
    AppRTC.MediaConstraints.GetJSONText + '", ' + '"include_loopback_js": "", '
    + '"is_loopback": "false", ' + '"offer_options": "{}", ' +
    '"pc_constraints": "{\"optional\": []}", "pc_config": "{\"rtcpMuxPolicy\": \"require\", \"bundlePolicy\": \"max-bundle\", \"iceServers\": []}", '
    + '"wss_post_url": "", ' + '"ice_server_url": "/iceservers", ' +
    '"warning_messages": [], ' + '"room_id": "' + aRoomId + '", ' +
    '"include_rtstats_js": ""}, ' + '"result": "SUCCESS"}'
end;

function TsgcWSProtocol_AppRTC_Server.DoMessage(const aRoomId, aClientId,
  aBody: String): String;
var
  oRoom: TsgcRoom;
  oClient: TsgcRoomClient;
begin
  oClient := nil;
  oRoom := GetRoomById(aRoomId);

  if Assigned(oRoom) then
  begin
    if oRoom.Client1.ClientId = aClientId then
      oClient := oRoom.Client1
    else if oRoom.Client2.ClientId = aClientId then
      oClient := oRoom.Client2;
    if Assigned(oClient) then
      oClient.Messages.Add(aBody);
  end;
  result := '{"result": "SUCCESS"}';
end;

procedure TsgcWSProtocol_AppRTC_Server.DoSend(aGuid, aText: String);
var
  vGUID: String;
begin
  vGUID := GetGUIDByPeerGUID(aGuid);

  if vGUID <> '' then
  begin
    if aText = '{"cmd":"send","msg":"{\"type\":\"bye\"}"}' then
    begin
      WriteData(vGUID, '{"msg":"{\"type\":\"bye\"}","error":""}');
      ClearRoomClientByGUID(aGuid);
      ClearMessagesRoomClientByGUID(vGUID);
    end
    else
      WriteData(vGUID, aText);
  end;
end;

procedure TsgcWSProtocol_AppRTC_Server.DoRegister(aGuid, aRoomId,
  aClientId: String);
var
  oRoom: TsgcRoom;
begin
  oRoom := GetRoomById(aRoomId);
  if not Assigned(oRoom) then
    oRoom := NewRoomById(aRoomId);

  if oRoom.Client1.ClientId = aClientId then
    oRoom.Client1.GUID := aGuid
  else if oRoom.Client2.ClientId = aClientId then
    oRoom.Client2.GUID := aGuid;
end;

function TsgcWSProtocol_AppRTC_Server.GetHTTPResponse(const aConnection
  : TsgcWSConnection; const aPath, aBody: String): String;
var
  oParams: TStringList;
begin
  oParams := TStringList.Create;
  Try
    oParams.Delimiter := '/';
    oParams.DelimitedText := aPath;
    if oParams.Count > 0 then
    begin
      if oParams[0] = 'iceservers' then
        result := AppRTC.IceServers.Text
      else if oParams[0] = 'join' then
        result := DoJoin(oParams[1])
      else if oParams[0] = 'message' then
        result := DoMessage(oParams[1], oParams[2], aBody);
    end;
  Finally
    sgcFree(oParams);
  End;
end;

function TsgcWSProtocol_AppRTC_Server.GetJSON: TsgcJSON;
begin
  if not Assigned(FJSON) then
    FJSON := TsgcJSON.Create(self);
  result := FJSON;
end;

function TsgcWSProtocol_AppRTC_Server.GetGUIDByPeerGUID(aConnectionGUID
  : String): string;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  result := '';

  oList := Rooms.LockList;
  Try
    for i := 0 to oList.Count - 1 do
    begin
      if TsgcRoom(oList[i]).Client2.GUID = aConnectionGUID then
      begin
        result := TsgcRoom(oList[i]).Client1.GUID;
        break;
      end
      else if TsgcRoom(oList[i]).Client1.GUID = aConnectionGUID then
      begin
        result := TsgcRoom(oList[i]).Client2.GUID;
        break;
      end;
    end;
  Finally
    Rooms.UnlockList;
  End;
end;

function TsgcWSProtocol_AppRTC_Server.GetRoomById(const aRoomId: string)
  : TsgcRoom;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  result := nil;

  oList := Rooms.LockList;
  Try
    for i := 0 to oList.Count - 1 do
    begin
      if TsgcRoom(oList[i]).RoomId = aRoomId then
      begin
        result := TsgcRoom(oList[i]);
        break;
      end;
    end;
  Finally
    Rooms.UnlockList;
  End;
end;

procedure TsgcWSProtocol_AppRTC_Server.ClearRoomClientByGUID
  (aConnectionGUID: String);
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  oList := Rooms.LockList;
  Try
    for i := 0 to oList.Count - 1 do
    begin
      if TsgcRoom(oList[i]).Client1.GUID = aConnectionGUID then
      begin
        {$IFNDEF NEXTGEN}
        TsgcRoom(oList[i]).Client1.Free;
        {$ENDIF}
        TsgcRoom(oList[i]).Client1 := nil;
        break;
      end
      else if TsgcRoom(oList[i]).Client2.GUID = aConnectionGUID then
      begin
        {$IFNDEF NEXTGEN}
        TsgcRoom(oList[i]).Client2.Free;
        {$ENDIF}
        TsgcRoom(oList[i]).Client2 := nil;
        break;
      end;
    end;
  Finally
    Rooms.UnlockList;
  End;
end;

function TsgcWSProtocol_AppRTC_Server.GetRooms: TsgcRoomList;
begin
  if not Assigned(FRooms) then
    FRooms := TsgcRoomList.Create;
  result := FRooms;
end;

function TsgcWSProtocol_AppRTC_Server.NewRoomById(const aRoomId: string)
  : TsgcRoom;
var
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  result := TsgcRoom.Create;
  result.RoomId := aRoomId;

  oList := Rooms.LockList;
  Try
    oList.Add(result);
  Finally
    Rooms.UnlockList;
  End;
end;

procedure TsgcWSProtocol_AppRTC_Server.SetAppRTC(const Value
  : TsgcWSAppRTC_Options);
begin
  if Assigned(FAppRTC) then
    FAppRTC.Assign(Value);
end;

function TsgcWSProtocol_JS_AppRTC.GetResponse: string;
begin
  result := GetResourceString(CS_SGC_JS_APPRTC_ESEGECE_COM);
end;

class function TsgcWSProtocol_JS_AppRTC.GetFileName: string;
begin
  result := CS_JS_APPRTC_ESEGECE_COM;
end;

class function TsgcWSProtocol_CSS_AppRTC.GetFileName: string;
begin
  result := CS_CSS_APPRTC_ESEGECE_COM;;
end;

function TsgcWSProtocol_CSS_AppRTC.GetResponse: string;
begin
  result := GetResourceString(CS_SGC_CSS_APPRTC_ESEGECE_COM);
end;

destructor TsgcRoom.Destroy;
begin
  sgcFree(FClient1);
  sgcFree(FClient2);
  inherited;
end;

function TsgcRoom.GetClient1: TsgcRoomClient;
begin
  if not Assigned(FClient1) then
    FClient1 := TsgcRoomClient.Create;
  result := FClient1;
end;

function TsgcRoom.GetClient2: TsgcRoomClient;
begin
  if not Assigned(FClient2) then
    FClient2 := TsgcRoomClient.Create;
  result := FClient2;
end;

destructor TsgcRoomClient.Destroy;
begin
  sgcFree(FMessages);
  inherited;
end;

function TsgcRoomClient.GetMessages: TsgcMessages;
begin
  if not Assigned(FMessages) then
    FMessages := TsgcMessages.Create;
  result := FMessages;
end;

function TsgcMessages.GetJSONList: String;
var
  i: Integer;
  vMessage: String;
begin
  result := '[';

  for i := 0 to Count - 1 do
  begin
    vMessage := self[i];
    vMessage := sgcStringReplace(vMessage, '\', '\\');
    vMessage := sgcStringReplace(vMessage, '"', '\"');

    if result = '[' then
      result := result + '"' + vMessage + '"'
    else
      result := result + ',' + '"' + vMessage + '"'
  end;
  result := result + ']';
end;

constructor TsgcWSAppRTC_Options.Create;
begin
  inherited;
  FIceServers := TStringList.Create;
  FMediaConstraints := TsgcWSAppRTCMediaConstraints.Create;
end;

destructor TsgcWSAppRTC_Options.Destroy;
begin
  sgcFree(FMediaConstraints);
  sgcFree(FIceServers);
  inherited;
end;

procedure TsgcWSAppRTC_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSAppRTC_Options then
  begin
    RoomLink := TsgcWSAppRTC_Options(aSource).RoomLink;
    WebSocketURL := TsgcWSAppRTC_Options(aSource).WebSocketURL;
    IceServers := TsgcWSAppRTC_Options(aSource).IceServers;
    MediaConstraints := TsgcWSAppRTC_Options(aSource).MediaConstraints;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSAppRTC_Options.SetIceServers(const Value: TStringList);
begin
  FIceServers.Assign(Value);
end;

procedure TsgcWSAppRTC_Options.SetMediaConstraints
  (const Value: TsgcWSAppRTCMediaConstraints);
begin
  FMediaConstraints.Assign(Value);
end;

constructor TsgcWSAppRTCMediaConstraints.Create;
begin
  inherited;
  FAudio := TsgcWSAppRTCAudio.Create;
  FVideo := TsgcWSAppRTCVideo.Create;
end;

destructor TsgcWSAppRTCMediaConstraints.Destroy;
begin
  sgcFree(FVideo);
  sgcFree(FAudio);
  inherited;
end;

procedure TsgcWSAppRTCMediaConstraints.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSAppRTCMediaConstraints then
  begin
    Audio := TsgcWSAppRTCMediaConstraints(aSource).Audio;
    Video := TsgcWSAppRTCMediaConstraints(aSource).Video;
  end
  else
    inherited Assign(aSource);
end;

function TsgcWSAppRTCMediaConstraints.GetJSONText: string;

  function GetBoolText(const aBool: Boolean): string;
  begin
    if aBool then
      result := 'true'
    else
      result := 'false';
  end;

begin
  result := '{' + '\"audio\": ' + GetBoolText(Audio.Enabled) + ', ' +
    '\"video\": ' + GetBoolText(Video.Enabled);
  if Video.Enabled then
    result := result + ',' + '{\"optional\": [{\"minWidth\": \"' +
      IntToStr(Video.MinWidth) + '\"}, {\"minHeight\": \"' +
      IntToStr(Video.MinHeight) + '\"}], \"mandatory\": {}}';
  result := result + '}';
end;

procedure TsgcWSAppRTCMediaConstraints.SetAudio(const Value: TsgcWSAppRTCAudio);
begin
  FAudio.Assign(Value);
end;

procedure TsgcWSAppRTCMediaConstraints.SetVideo(const Value: TsgcWSAppRTCVideo);
begin
  FVideo.Assign(Value);
end;

constructor TsgcWSAppRTCAudio.Create;
begin
  inherited;
  Enabled := True;
end;

procedure TsgcWSAppRTCAudio.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSAppRTCAudio then
    Enabled := TsgcWSAppRTCAudio(aSource).Enabled
  else
    inherited Assign(aSource);
end;

constructor TsgcWSAppRTCVideo.Create;
begin
  inherited;
  Enabled := True;
  MinHeight := 720;
  MinWidth := 1280;
end;

procedure TsgcWSAppRTCVideo.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSAppRTCVideo then
  begin
    Enabled := TsgcWSAppRTCVideo(aSource).Enabled;
    MinHeight := TsgcWSAppRTCVideo(aSource).MinHeight;
    MinWidth := TsgcWSAppRTCVideo(aSource).MinWidth;
  end
  else
    inherited Assign(aSource);
end;

initialization

Classes.RegisterClass(TsgcWSProtocol_AppRTC_Server);
TsgcWSHTTPResponse.RegisterfileName(TsgcWSProtocol_JS_AppRTC);
TsgcWSHTTPResponse.RegisterfileName(TsgcWSProtocol_CSS_AppRTC);

{$ENDIF}

end.
