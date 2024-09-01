{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Server_API_RTCMultiConnection;

interface

{$I sgcVer.inc}
{$IFDEF SGC_APIS}

uses
  Classes, SysUtils, StrUtils,
{$IFDEF NEXTGEN}System.Generics.Collections, {$ELSE}Contnrs, {$ENDIF}
  // sgc
  sgcWebSocket_Classes_SyncObjs, sgcWebSocket_Server_API_Base, sgcJSON,
  sgcWebSocket_Classes, sgcWebSocket_Classes_Queues,
  sgcWebSocket_Server_API_SocketIO;

type

  TsgcWSRTCMultiConnectionVideoResolution = (rtcvrSD, rtcvr360p, rtcvr480p,
    rtcvrHDReady, rtcvrFullHD, rtcvrUltraHD, rtcvrCustom);

  TsgcWSServerApiData_RTCMultiConnection = class(TsgcWSServerApiData)
  private
    FRoom: string;
    FUserId: string;
  public
    property Room: string read FRoom write FRoom;
    property UserId: string read FUserId write FUserId;
  end;

  TsgcWSRTCMultiConnectionUser = class(TsgcQueueItemBase)
  private
    FConnection: TsgcWSConnection;
    function GetUserId: string;
    procedure SetConnection(const Value: TsgcWSConnection);
    procedure SetUserId(const Value: string);
  public
    property Connection: TsgcWSConnection read FConnection write SetConnection;
    property UserId: string read GetUserId write SetUserId;
  end;

  TsgcWSRTCMultiConnectionUsers = class(TsgcQueue)
  protected
    procedure DoBroadcastUserUpdate(const aMethod: string;
      const aUserId: string);
  public
    function GetUser(const aUserId: string): TsgcWSRTCMultiConnectionUser;
    procedure AddUser(const aConnection: TsgcWSConnection;
      const aUserId: string);
    function DeleteUser(const aUserId: string): Boolean;
  end;

  TsgcWSRTCMultiConnectionRoom = class(TsgcQueueItemBase)
  private
    FMessageEventName: string;
    FUsers: TsgcWSRTCMultiConnectionUsers;
    function GetRoom: string;
    function GetUsers: TsgcWSRTCMultiConnectionUsers;
    procedure SetRoom(const Value: string);
  protected
    procedure DoNewParticipationRequest(const aConnection: TsgcWSConnection;
      const aJSON: TsgcJSON); virtual;
    procedure DoForwardMessageToRemoteUser(const aRemoteUserId: string;
      const aJSON: TsgcJSON); virtual;
  public
    destructor Destroy; override;
  public
    property MessageEventName: string read FMessageEventName
      write FMessageEventName;
    property Room: string read GetRoom write SetRoom;
    property Users: TsgcWSRTCMultiConnectionUsers read GetUsers write FUsers;
  end;

  TsgcWSRTCMultiConnectionRooms = class(TsgcQueue)
  end;

  TsgcWSRTCMultiConnectionXHR = class(TsgcQueueItemBase)
  private
    FRequest: string;
  public
    property Request: string read FRequest write FRequest;
  end;

  TsgcWSRTCMultiConnectionXHRs = class(TsgcQueue)
  end;

  TsgcWSRTCMultiConnection_HTMLDocuments = class(TPersistent)
  private
    FScreenSharing: string;
    FVideoBroadcasting: string;
    FVideoConferencing: string;
  protected
    function ProcessDocument(const aDocument: string): Boolean; virtual;
  public
    constructor Create;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property ScreenSharing: string read FScreenSharing write FScreenSharing;
    property VideoBroadcasting: string read FVideoBroadcasting
      write FVideoBroadcasting;
    property VideoConferencing: string read FVideoConferencing
      write FVideoConferencing;
  end;

  TsgcWSRTCMultiConnection_ServerOptions = class(TPersistent)
  private
    FHost: String;
    FPort: Integer;
  public
    constructor Create;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Host: String read FHost write FHost;
    property Port: Integer read FPort write FPort;
  end;

  TsgcWSRTCMultiConnection_VideoResolutionOptions = class(TPersistent)
  private
    FFrameRate: Integer;
    FHeight: Integer;
    FWidth: Integer;
    FResolution: TsgcWSRTCMultiConnectionVideoResolution;
    procedure SetFrameRate(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    procedure SetResolution(const Value
      : TsgcWSRTCMultiConnectionVideoResolution);
    procedure SetWidth(const Value: Integer);
  public
    constructor Create;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property FrameRate: Integer read FFrameRate write SetFrameRate;
    property Height: Integer read FHeight write SetHeight;
    property Width: Integer read FWidth write SetWidth;
    property Resolution: TsgcWSRTCMultiConnectionVideoResolution
      read FResolution write SetResolution;
  end;

  TsgcWSRTCMultiConnection_Options = class(TPersistent)
  private
    FHTMLDocuments: TsgcWSRTCMultiConnection_HTMLDocuments;
    FFooter: string;
    FIceServers: TStringList;
    FMessageEventName: string;
    FServer: TsgcWSRTCMultiConnection_ServerOptions;
    FVideoResolution: TsgcWSRTCMultiConnection_VideoResolutionOptions;
    procedure SetHTMLDocuments(const Value
      : TsgcWSRTCMultiConnection_HTMLDocuments);
    procedure SetIceServers(const Value: TStringList);
    procedure SetServer(const Value: TsgcWSRTCMultiConnection_ServerOptions);
    procedure SetVideoResolution(const Value
      : TsgcWSRTCMultiConnection_VideoResolutionOptions);
  public
    constructor Create;
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  public
    property Footer: string read FFooter write FFooter;
    property MessageEventName: string read FMessageEventName
      write FMessageEventName;
  published
    property HTMLDocuments: TsgcWSRTCMultiConnection_HTMLDocuments
      read FHTMLDocuments write SetHTMLDocuments;
    property IceServers: TStringList read FIceServers write SetIceServers;
    property Server: TsgcWSRTCMultiConnection_ServerOptions read FServer
      write SetServer;
    property VideoResolution: TsgcWSRTCMultiConnection_VideoResolutionOptions
      read FVideoResolution write SetVideoResolution;
  end;

  TsgcWSServer_API_RTCMultiConnection = class(TsgcWSServer_API_SocketIO)
    { from TsgcWSComponent }
  protected
    procedure Loaded; override;
  protected
    procedure DoEventConnect(aConnection: TsgcWSConnection); override;
    procedure DoEventDisconnect(aConnection: TsgcWSConnection;
      Code: Integer); override;
    { from TsgcWSComponent }

    { from TsgcWSAPI_server }
  protected
    function DoHTTPRequestApi(const aRequest: TsgcHTTPRequest;
      const aResponse: TsgcHTTPResponse): Boolean; override;
    procedure DoHTTPBeforeResourceResponse(const aRequest: TsgcHTTPRequest;
      const aResponse: TsgcHTTPResponse); override;
    { from TsgcWSAPI_server }

    { from TsgcWSServer_API_SocketIO }
  protected
    procedure DoSocketIOJSONMessageEvent(const aConnection: TsgcWSConnection;
      const aHeader, aJSON, aRawMessage: string); override;
    { from TsgcWSServer_API_SocketIO }

    { rooms }
  private
    FRooms: TsgcWSRTCMultiConnectionRooms;
    function GetRooms: TsgcWSRTCMultiConnectionRooms;
  protected
    function GetRoom(const aRoom: string): TsgcWSRTCMultiConnectionRoom;
    function GetRoomByUser(const aUserId: string): TsgcWSRTCMultiConnectionRoom;
    procedure AddRoom(const aRoom: string);
    function AddUser(const aRoom: string;
      const aConnection: TsgcWSConnection): Boolean;
    function DeleteRoom(const aRoom: string): Boolean;
  protected
    property Rooms: TsgcWSRTCMultiConnectionRooms read GetRooms write FRooms;
    { rooms }

    { xhr }
  private
    FXHRs: TsgcWSRTCMultiConnectionXHRs;
    function GetXHRs: TsgcWSRTCMultiConnectionXHRs;
  protected
    procedure AddXHR(const aSessionId, aRequest: string);
    function GetXHR(const aSessionId: string): string;
    function DeleteXHR(const aSessionId: string): Boolean;
  protected
    property XHRs: TsgcWSRTCMultiConnectionXHRs read GetXHRs write FXHRs;
    { xhr }

    { resources }
  public
    procedure LoadResources;
    { resources }

    { properties }
  private
    function GetServerURL: string;
  private
    FRTCMultiConnection: TsgcWSRTCMultiConnection_Options;
    procedure SetRTCMultiConnection(const Value
      : TsgcWSRTCMultiConnection_Options);
  public
    property RTCMultiConnection: TsgcWSRTCMultiConnection_Options
      read FRTCMultiConnection write SetRTCMultiConnection;
    { properties }

    { methods }
  protected
    procedure DoExtraDataUpdated(const aConnection: TsgcWSConnection;
      const aJSON: string); virtual;
    procedure DoOpenRoom(const aConnection: TsgcWSConnection;
      const aJSON: string); virtual;
    procedure DoJoinRoom(const aConnection: TsgcWSConnection;
      const aJSON: string); virtual;
  protected
    procedure DoVideoConference(const aConnection: TsgcWSConnection;
      const aJSON: string); virtual;
    { methods }

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

{$ENDIF}

implementation

{$IFDEF SGC_APIS}

uses
{$IFDEF SGC_INDY}sgcIdURI{$ELSE}IdURI{$ENDIF},
  sgcBase_Helpers;

type
  TsgcWSConnection_Hack = class(TsgcWSConnection);

function GetRTCMultiConnectionApiData(const aConnection: TsgcWSConnection)
  : TsgcWSServerApiData_RTCMultiConnection;
begin
  result := TsgcWSServerApiData_RTCMultiConnection
    (TsgcWSConnection_Hack(aConnection).ApiData);
end;

constructor TsgcWSServer_API_RTCMultiConnection.Create(aOwner: TComponent);
begin
  inherited;
  FRTCMultiConnection := TsgcWSRTCMultiConnection_Options.Create;
end;

destructor TsgcWSServer_API_RTCMultiConnection.Destroy;
begin
  sgcFree(FXHRs);
  sgcFree(FRTCMultiConnection);
  sgcFree(FRooms);
  inherited;
end;

procedure TsgcWSServer_API_RTCMultiConnection.AddRoom(const aRoom: string);
var
  oRoom: TsgcWSRTCMultiConnectionRoom;
begin
  oRoom := TsgcWSRTCMultiConnectionRoom.Create;
  oRoom.MessageEventName := RTCMultiConnection.MessageEventName;
  oRoom.Room := aRoom;

  Rooms.AddItem(oRoom);
end;

function TsgcWSServer_API_RTCMultiConnection.AddUser(const aRoom: string;
  const aConnection: TsgcWSConnection): Boolean;
var
  oRoom: TsgcWSRTCMultiConnectionRoom;
  vUserId: string;
begin
  result := False;

  oRoom := GetRoom(aRoom);
  if Assigned(oRoom) then
  begin
    vUserId := GetRTCMultiConnectionApiData(aConnection).UserId;
    if not Assigned(oRoom.Users.GetUser(vUserId)) then
    begin
      oRoom.Users.AddUser(aConnection, vUserId);
      result := True;
    end;
  end;
end;

procedure TsgcWSServer_API_RTCMultiConnection.AddXHR(const aSessionId,
  aRequest: string);
var
  oXHR: TsgcWSRTCMultiConnectionXHR;
begin
  oXHR := TsgcWSRTCMultiConnectionXHR.Create;
  oXHR.ID := aSessionId;
  oXHR.Request := MidStr(aRequest, Pos('[', aRequest), Length(aRequest));

  XHRs.AddItem(oXHR);
end;

function TsgcWSServer_API_RTCMultiConnection.DeleteRoom
  (const aRoom: string): Boolean;
begin
  result := Rooms.DeleteItem(aRoom)
end;

function TsgcWSServer_API_RTCMultiConnection.DeleteXHR(const aSessionId
  : string): Boolean;
begin
  result := XHRs.DeleteItem(aSessionId);
end;

procedure TsgcWSServer_API_RTCMultiConnection.DoEventConnect
  (aConnection: TsgcWSConnection);
var
  oParams: TStringList;
  oData: TsgcWSServerApiData_RTCMultiConnection;
begin
  inherited;
  oData := TsgcWSServerApiData_RTCMultiConnection.Create;

  oParams := TStringList.Create;
  Try
    oParams.CommaText := sgcStringReplace(sgcStringReplace(aConnection.URL,
      '/socket.io/?', ''), '&', ',');
    oData.UserId := oParams.Values['userid'];
    oData.Room := oParams.Values['sessionid'];
  Finally
    sgcFree(oParams);
  End;

  TsgcWSConnection_Hack(aConnection).ApiData := oData;
end;

procedure TsgcWSServer_API_RTCMultiConnection.DoEventDisconnect
  (aConnection: TsgcWSConnection; Code: Integer);
var
  oData: TObject;
  oRoom: TsgcWSRTCMultiConnectionRoom;
  vRoom: string;
begin
  // ... delete user
  vRoom := GetRTCMultiConnectionApiData(aConnection).Room;
  oRoom := GetRoom(vRoom);
  if Assigned(oRoom) then
  begin
    oRoom.Users.DeleteUser(GetRTCMultiConnectionApiData(aConnection).UserId);
    if oRoom.Users.Count = 0 then
      DeleteRoom(vRoom);
  end;

  // ... destroy ApiData
  oData := TsgcWSConnection_Hack(aConnection).ApiData;
  sgcFree(oData);
  inherited;
end;

procedure TsgcWSServer_API_RTCMultiConnection.DoExtraDataUpdated
  (const aConnection: TsgcWSConnection; const aJSON: string);
begin

end;

procedure TsgcWSServer_API_RTCMultiConnection.DoHTTPBeforeResourceResponse
  (const aRequest: TsgcHTTPRequest; const aResponse: TsgcHTTPResponse);
begin
  inherited;
  if RTCMultiConnection.HTMLDocuments.ProcessDocument(aRequest.Document) then
  begin
    aResponse.Content := sgcStringReplace(aResponse.Content, '<#socket_url>',
      GetServerURL);
    aResponse.Content := sgcStringReplace(aResponse.Content,
      '<#socket_message_event>', RTCMultiConnection.MessageEventName);
    aResponse.Content := sgcStringReplace(aResponse.Content, '<#ice_servers>',
      RTCMultiConnection.IceServers.Text);
    aResponse.Content := sgcStringReplace(aResponse.Content, '<#footer>',
      RTCMultiConnection.Footer);
    aResponse.Content := sgcStringReplace(aResponse.Content,
      '<#resolution_width>',
      IntToStr(RTCMultiConnection.VideoResolution.Width));
    aResponse.Content := sgcStringReplace(aResponse.Content,
      '<#resolution_height>',
      IntToStr(RTCMultiConnection.VideoResolution.Height));
    aResponse.Content := sgcStringReplace(aResponse.Content,
      '<#resolution_framerate>',
      IntToStr(RTCMultiConnection.VideoResolution.FrameRate));
  end;
end;

function TsgcWSServer_API_RTCMultiConnection.DoHTTPRequestApi
  (const aRequest: TsgcHTTPRequest; const aResponse: TsgcHTTPResponse): Boolean;
var
  oJSON: TsgcJSON;
  oParams: TStringList;
  vSID: string;
  vRoom: string;
begin
  // ... read params
  oParams := TStringList.Create;
  Try
    oParams.CommaText := sgcStringReplace(aRequest.QueryParams, '&', ',');
    vSID := oParams.Values['sid'];
  Finally
    sgcFree(oParams);
  End;

  if (vSID <> '') and (aRequest.Method = 'POST') and (aRequest.Content <> '')
  then
  begin
    AddXHR(vSID, aRequest.Content);

    result := True;

    aResponse.Code := 200;
    aResponse.Content := 'ok';
    aResponse.ContentType := 'text/html';
  end
  else if (vSID <> '') and (aRequest.Method = 'GET') and (GetXHR(vSID) <> '')
  then
  begin
    oJSON := TsgcJSON.Create(nil);
    Try
      oJSON.Read(GetXHR(vSID));
      if oJSON.Count > 1 then
      begin
        if oJSON.Item[0].Value = 'check-presence' then
          vRoom := oJSON.Item[1].Value;
      end;
    Finally
      sgcFree(oJSON);
    End;

    if vRoom <> '' then
    begin
      aResponse.Code := 200;
      aResponse.Content :=
        Format('430[true,"%s",{"_room":{"isFull":false}}]', [vRoom]);
      aResponse.Content := IntToStr(Length(aResponse.Content)) + ':' +
        aResponse.Content;
      aResponse.ContentType := 'text/plain; charset=UTF-8';
      result := True;
    end
    else
    begin
      aResponse.Code := 404;
      result := False;
    end;
  end
  else
    result := inherited DoHTTPRequestApi(aRequest, aResponse);
end;

procedure TsgcWSServer_API_RTCMultiConnection.DoJoinRoom(const aConnection
  : TsgcWSConnection; const aJSON: string);
var
  oJSON: TsgcJSON;
  vRoom: string;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.Read(aJSON);
    if oJSON.Node['sessionid'] <> nil then
    begin
      vRoom := oJSON.Node['sessionid'].Value;
      if Assigned(GetRoom(vRoom)) then
      begin
        // ... update room because when websocket starts, the session id is not set to room
        GetRTCMultiConnectionApiData(aConnection).Room := vRoom;

        AddUser(vRoom, aConnection);
        aConnection.WriteData('431[true]');
      end;
    end;
  Finally
    sgcFree(oJSON);
  End;
end;

procedure TsgcWSServer_API_RTCMultiConnection.LoadResources;
begin
  ClearResources;
  AddResource('/sgcRTCMultiConnection.js', 'SGC_JS_RTCMULTICONNECTION');
  AddResource(RTCMultiConnection.HTMLDocuments.VideoConferencing,
    'SGC_HTML_RTCMULTICONNECTION_VIDEOCONFERENCING');
  AddResource(RTCMultiConnection.HTMLDocuments.ScreenSharing,
    'SGC_HTML_RTCMULTICONNECTION_SCREENSHARING');
  AddResource(RTCMultiConnection.HTMLDocuments.VideoBroadcasting,
    'SGC_HTML_RTCMULTICONNECTION_VIDEOBROADCASTING');
  AddResource('/sgcRTCMultiConnection.css', 'SGC_CSS_RTCMULTICONNECTION');
end;

procedure TsgcWSServer_API_RTCMultiConnection.DoOpenRoom(const aConnection
  : TsgcWSConnection; const aJSON: string);
var
  oJSON: TsgcJSON;
  vRoom: string;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.Read(aJSON);
    if oJSON.Node['sessionid'] <> nil then
    begin
      vRoom := oJSON.Node['sessionid'].Value;
      if not Assigned(GetRoom(vRoom)) then
      begin
        AddRoom(vRoom);
        AddUser(vRoom, aConnection);
        aConnection.WriteData('430[true]');
      end;
    end;
  Finally
    sgcFree(oJSON);
  End;
end;

procedure TsgcWSServer_API_RTCMultiConnection.DoSocketIOJSONMessageEvent
  (const aConnection: TsgcWSConnection;
  const aHeader, aJSON, aRawMessage: string);
var
  oJSON: TsgcJSON;
  vMethod: string;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.Read(aJSON);
    vMethod := oJSON.Item[0].Value;
    if vMethod = 'extra-data-updated' then
      DoExtraDataUpdated(aConnection, oJSON.Item[1].Value)
    else if vMethod = 'open-room' then
      DoOpenRoom(aConnection, oJSON.Item[1].Value)
    else if vMethod = 'join-room' then
      DoJoinRoom(aConnection, oJSON.Item[1].Value)
    else if vMethod = RTCMultiConnection.MessageEventName then
      DoVideoConference(aConnection, oJSON.Item[1].Value);
  Finally
    sgcFree(oJSON);
  End;
  inherited;
end;

procedure TsgcWSServer_API_RTCMultiConnection.DoVideoConference
  (const aConnection: TsgcWSConnection; const aJSON: string);
var
  oJSON: TsgcJSON;
  vRemoteUserId: string;
  oRoom: TsgcWSRTCMultiConnectionRoom;
begin
  oJSON := TsgcJSON.Create(nil);
  Try
    oJSON.Read(aJSON);
    if oJSON.Node['remoteUserId'] <> nil then
    begin
      vRemoteUserId := oJSON.Node['remoteUserId'].Value;
      oRoom := GetRoom(vRemoteUserId);
      if Assigned(oRoom) then
        oRoom.DoNewParticipationRequest(aConnection, oJSON)
      else
      begin
        oRoom := GetRoomByUser(vRemoteUserId);
        if Assigned(oRoom) then
          oRoom.DoForwardMessageToRemoteUser(vRemoteUserId, oJSON);
      end;
    end;
  Finally
    sgcFree(oJSON);
  End;
end;

function TsgcWSServer_API_RTCMultiConnection.GetRooms
  : TsgcWSRTCMultiConnectionRooms;
begin
  if not Assigned(FRooms) then
  begin
    FRooms := TsgcWSRTCMultiConnectionRooms.Create;
    FRooms.OwnObjects := True;
  end;
  result := FRooms;
end;

function TsgcWSServer_API_RTCMultiConnection.GetRoom(const aRoom: string)
  : TsgcWSRTCMultiConnectionRoom;
begin
  result := TsgcWSRTCMultiConnectionRoom(Rooms.GetItem(aRoom));
end;

function TsgcWSServer_API_RTCMultiConnection.GetRoomByUser
  (const aUserId: string): TsgcWSRTCMultiConnectionRoom;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  result := nil;

  oList := Rooms.LockList;
  Try
    for i := 0 to oList.Count - 1 do
    begin
      if TsgcWSRTCMultiConnectionRoom(oList[i]).Users.GetUser(aUserId) <> nil
      then
      begin
        result := TsgcWSRTCMultiConnectionRoom(oList[i]);
        break;
      end;
    end;
  Finally
    Rooms.UnlockList;
  End;
end;

function TsgcWSServer_API_RTCMultiConnection.GetServerURL: string;
begin
  result := 'https://' + RTCMultiConnection.Server.Host;
  if RTCMultiConnection.Server.Port <> 443 then
    result := result + ':' + IntToStr(RTCMultiConnection.Server.Port);
  result := result + '/';
end;

function TsgcWSServer_API_RTCMultiConnection.GetXHR(const aSessionId
  : string): string;
var
  oXHR: TsgcWSRTCMultiConnectionXHR;
  i: Integer;
begin
  result := '';

  // ... wait post thread is processed
  for i := 1 to 100 do
  begin
    oXHR := TsgcWSRTCMultiConnectionXHR(XHRs.GetItem(aSessionId));
    if Assigned(oXHR) then
    begin
      result := oXHR.Request;
      break;
    end;
    sleep(1);
  end;
end;

function TsgcWSServer_API_RTCMultiConnection.GetXHRs
  : TsgcWSRTCMultiConnectionXHRs;
begin
  if not Assigned(FXHRs) then
    FXHRs := TsgcWSRTCMultiConnectionXHRs.Create;
  result := FXHRs;
end;

procedure TsgcWSServer_API_RTCMultiConnection.Loaded;
begin
  inherited;
  LoadResources;
end;

procedure TsgcWSServer_API_RTCMultiConnection.SetRTCMultiConnection
  (const Value: TsgcWSRTCMultiConnection_Options);
begin
  if Assigned(FRTCMultiConnection) then
    FRTCMultiConnection.Assign(Value);
end;

constructor TsgcWSRTCMultiConnection_Options.Create;
begin
  inherited;
  FServer := TsgcWSRTCMultiConnection_ServerOptions.Create;
  FVideoResolution := TsgcWSRTCMultiConnection_VideoResolutionOptions.Create;
  FHTMLDocuments := TsgcWSRTCMultiConnection_HTMLDocuments.Create;
  FIceServers := TStringList.Create;
  MessageEventName := 'sgcRTCMultiConnection';
  Footer := '<div align="right"><small>sgcWebSockets WebRTC Demo - based on ' +
    '<a href="https://github.com/muaz-khan/RTCMultiConnection">RTCMultiConnection</a> project</small></div>';
end;

destructor TsgcWSRTCMultiConnection_Options.Destroy;
begin
  sgcFree(FHTMLDocuments);
  sgcFree(FVideoResolution);
  sgcFree(FServer);
  sgcFree(FIceServers);
  inherited;
end;

procedure TsgcWSRTCMultiConnection_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSRTCMultiConnection_Options then
  begin
    Server := TsgcWSRTCMultiConnection_Options(aSource).Server;
    IceServers := TsgcWSRTCMultiConnection_Options(aSource).IceServers;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSRTCMultiConnection_Options.SetHTMLDocuments
  (const Value: TsgcWSRTCMultiConnection_HTMLDocuments);
begin
  if Assigned(FHTMLDocuments) then
    FHTMLDocuments.Assign(Value);
end;

procedure TsgcWSRTCMultiConnection_Options.SetIceServers
  (const Value: TStringList);
begin
  FIceServers.Assign(Value);
end;

procedure TsgcWSRTCMultiConnection_Options.SetServer
  (const Value: TsgcWSRTCMultiConnection_ServerOptions);
begin
  if Assigned(FServer) then
    FServer.Assign(Value);
end;

procedure TsgcWSRTCMultiConnection_Options.SetVideoResolution
  (const Value: TsgcWSRTCMultiConnection_VideoResolutionOptions);
begin
  if Assigned(FVideoResolution) then
    FVideoResolution.Assign(Value);
end;

destructor TsgcWSRTCMultiConnectionRoom.Destroy;
begin
  sgcFree(FUsers);
  inherited;
end;

procedure TsgcWSRTCMultiConnectionRoom.DoForwardMessageToRemoteUser
  (const aRemoteUserId: string; const aJSON: TsgcJSON);
var
  oUser: TsgcWSRTCMultiConnectionUser;
begin
  oUser := Users.GetUser(aRemoteUserId);
  if Assigned(oUser) then
  begin
    aJSON.AddObject('extra').JSONObject.AddObject('_room')
      .JSONObject.AddPair('isFull', False);
    oUser.Connection.WriteData(Format('42["%s", %s]', [MessageEventName,
      aJSON.Text]));
  end;
end;

procedure TsgcWSRTCMultiConnectionRoom.DoNewParticipationRequest
  (const aConnection: TsgcWSConnection; const aJSON: TsgcJSON);
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
  oUser: TsgcWSRTCMultiConnectionUser;
begin
  oList := Users.LockList;
  Try
    for i := 0 to oList.Count - 1 do
    begin
      oUser := TsgcWSRTCMultiConnectionUser(oList[i]);
      if oUser.Connection.Guid <> aConnection.Guid then
      begin
        aJSON.Node['remoteUserId'].Value := TsgcWSRTCMultiConnectionUser
          (oList[i]).UserId;
        oUser.Connection.WriteData(Format('42["%s", %s]', [MessageEventName,
          aJSON.Text]));
      end;
    end;
  Finally
    Users.UnlockList;
  End;
end;

function TsgcWSRTCMultiConnectionRoom.GetRoom: string;
begin
  result := ID;
end;

function TsgcWSRTCMultiConnectionRoom.GetUsers: TsgcWSRTCMultiConnectionUsers;
begin
  if not Assigned(FUsers) then
  begin
    FUsers := TsgcWSRTCMultiConnectionUsers.Create;
    FUsers.OwnObjects := True;
  end;
  result := FUsers;
end;

procedure TsgcWSRTCMultiConnectionRoom.SetRoom(const Value: string);
begin
  ID := Value;
end;

procedure TsgcWSRTCMultiConnectionUsers.AddUser(const aConnection
  : TsgcWSConnection; const aUserId: string);
var
  oUser: TsgcWSRTCMultiConnectionUser;
begin
  oUser := TsgcWSRTCMultiConnectionUser.Create;
  oUser.UserId := aUserId;
  oUser.Connection := aConnection;

  AddItem(oUser);

  DoBroadcastUserUpdate('connected', aUserId);
end;

function TsgcWSRTCMultiConnectionUsers.DeleteUser(const aUserId
  : string): Boolean;
begin
  result := DeleteItem(aUserId);

  DoBroadcastUserUpdate('disconnected', aUserId);
end;

procedure TsgcWSRTCMultiConnectionUsers.DoBroadcastUserUpdate
  (const aMethod: string; const aUserId: string);
var
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
  i: Integer;
begin
  oList := LockList;
  Try
    for i := 0 to oList.Count - 1 do
    begin
      if TsgcWSRTCMultiConnectionUser(oList[i]).UserId <> aUserId then
        TsgcWSRTCMultiConnectionUser(oList[i]).Connection.WriteData
          (Format('42["user-%s","%s"]', [aMethod, aUserId]));
    end;
  Finally
    UnlockList;
  End;
end;

function TsgcWSRTCMultiConnectionUsers.GetUser(const aUserId: string)
  : TsgcWSRTCMultiConnectionUser;
begin
  result := TsgcWSRTCMultiConnectionUser(GetItem(aUserId));
end;

function TsgcWSRTCMultiConnectionUser.GetUserId: string;
begin
  result := ID;
end;

procedure TsgcWSRTCMultiConnectionUser.SetConnection
  (const Value: TsgcWSConnection);
begin
  FConnection := Value;
end;

procedure TsgcWSRTCMultiConnectionUser.SetUserId(const Value: string);
begin
  ID := Value;
end;

constructor TsgcWSRTCMultiConnection_ServerOptions.Create;
begin
  inherited;
  Host := '127.0.0.1';
  Port := 443;
end;

procedure TsgcWSRTCMultiConnection_ServerOptions.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSRTCMultiConnection_ServerOptions then
  begin
    Host := TsgcWSRTCMultiConnection_ServerOptions(aSource).Host;
    Port := TsgcWSRTCMultiConnection_ServerOptions(aSource).Port;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcWSRTCMultiConnection_VideoResolutionOptions.Create;
begin
  inherited;
  Resolution := rtcvrHDReady;
end;

procedure TsgcWSRTCMultiConnection_VideoResolutionOptions.Assign
  (aSource: TPersistent);
begin
  if aSource is TsgcWSRTCMultiConnection_VideoResolutionOptions then
  begin
    Resolution := TsgcWSRTCMultiConnection_VideoResolutionOptions(aSource)
      .Resolution;
    Width := TsgcWSRTCMultiConnection_VideoResolutionOptions(aSource).Width;
    Height := TsgcWSRTCMultiConnection_VideoResolutionOptions(aSource).Height;
    FrameRate := TsgcWSRTCMultiConnection_VideoResolutionOptions(aSource)
      .FrameRate;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSRTCMultiConnection_VideoResolutionOptions.SetFrameRate
  (const Value: Integer);
begin
  FFrameRate := Value;
end;

procedure TsgcWSRTCMultiConnection_VideoResolutionOptions.SetHeight
  (const Value: Integer);
begin
  FHeight := Value;
end;

procedure TsgcWSRTCMultiConnection_VideoResolutionOptions.SetResolution
  (const Value: TsgcWSRTCMultiConnectionVideoResolution);
begin
  FResolution := Value;

  case FResolution of
    rtcvrSD:
      begin
        FWidth := 352;
        FHeight := 240;
        FFrameRate := 30;
      end;
    rtcvr360p:
      begin
        FWidth := 480;
        FHeight := 360;
        FFrameRate := 30;
      end;
    rtcvr480p:
      begin
        FWidth := 858;
        FHeight := 480;
        FFrameRate := 30;
      end;
    rtcvrHDReady:
      begin
        FWidth := 1280;
        FHeight := 720;
        FFrameRate := 30;
      end;
    rtcvrFullHD:
      begin
        FWidth := 1920;
        FHeight := 1080;
        FFrameRate := 30;
      end;
    rtcvrUltraHD:
      begin
        FWidth := 3860;
        FHeight := 2160;
        FFrameRate := 30;
      end;
  end;
end;

procedure TsgcWSRTCMultiConnection_VideoResolutionOptions.SetWidth
  (const Value: Integer);
begin
  FWidth := Value;
end;

constructor TsgcWSRTCMultiConnection_HTMLDocuments.Create;
begin
  inherited;
  FVideoConferencing := '/RTCMultiConnection-VideoConferencing.html';
  FScreenSharing := '/RTCMultiConnection-ScreenSharing.html';
  FVideoBroadcasting := '/RTCMultiConnection-VideoBroadcasting.html';
end;

procedure TsgcWSRTCMultiConnection_HTMLDocuments.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSRTCMultiConnection_HTMLDocuments then
  begin
    VideoConferencing := TsgcWSRTCMultiConnection_HTMLDocuments(aSource)
      .VideoConferencing;
    ScreenSharing := TsgcWSRTCMultiConnection_HTMLDocuments(aSource)
      .ScreenSharing;
    VideoBroadcasting := TsgcWSRTCMultiConnection_HTMLDocuments(aSource)
      .VideoBroadcasting;
  end
  else
    inherited Assign(aSource);
end;

function TsgcWSRTCMultiConnection_HTMLDocuments.ProcessDocument(const aDocument
  : string): Boolean;
begin
  result := (UpperCase(aDocument) = UpperCase(VideoConferencing)) or
    (UpperCase(aDocument) = UpperCase(ScreenSharing)) or
    (UpperCase(aDocument) = UpperCase(VideoBroadcasting));
end;

{$ENDIF}

end.
