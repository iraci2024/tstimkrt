{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Protocol_Presence_Message;

interface

{$I sgcVer.inc}
{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils,
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}
  {$IFDEF NEXTGEN}System.Generics.Collections, {$ELSE}Contnrs, {$ENDIF}
  // sgc
  sgcWebSocket_Protocol_Base_Message, sgcWebSocket_Classes, sgcJSON,
  sgcWebSocket_Types, sgcWebSocket_Classes_SyncObjs;

type
  TsgcWSPresenceMember = class;
  TsgcWSPresenceChannel = class;
  TsgcWSPresenceMsg = class;
  TsgcWSPresenceError = class;

  TsgcWSPresenceNewMemberEvent = procedure(aConnection: TsgcWSConnection;
    const aMember: TsgcWSPresenceMember) of object;
  TsgcWSPresenceRemoveMemberEvent = procedure(aConnection: TsgcWSConnection;
    aMember: TsgcWSPresenceMember) of object;
  TsgcWSPresenceNewMemberChannelEvent = procedure(Connection: TsgcWSConnection;
    const aChannel: TsgcWSPresenceChannel; const aMember: TsgcWSPresenceMember)
    of object;
  TsgcWSPresenceRemoveMemberChannelEvent = procedure
    (Connection: TsgcWSConnection; const aChannel: TsgcWSPresenceChannel;
    const aMember: TsgcWSPresenceMember) of object;
  TsgcWSPresenceMemberChannelErrorEvent = procedure
    (Connection: TsgcWSConnection; const aError: TsgcWSPresenceError;
    const aChannel: TsgcWSPresenceChannel; const aMember: TsgcWSPresenceMember)
    of object;
  TsgcWSPresencePublishMsgErrorEvent = procedure(Connection: TsgcWSConnection;
    const aError: TsgcWSPresenceError; const aMsg: TsgcWSPresenceMsg;
    const aChannel: TsgcWSPresenceChannel; const aMember: TsgcWSPresenceMember)
    of object;

  TsgcWSPresenceError = class(TPersistent)
  private
    FCode: Integer;
    FText: String;
  protected
    procedure Clear; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  public
    property Code: Integer read FCode write FCode;
    property Text: String read FText write FText;
  end;

  TsgcWSPresenceMember = class(TPersistent)
  private
    FConnection: TsgcWSConnection;
    FData: TObject;
    FID: String;
    FInfo: String;
    FName: String;
  protected
    procedure Clear; virtual;
  public
    destructor Destroy; override;
  public
    procedure Assign(aSource: TPersistent); override;
  public
    property Connection: TsgcWSConnection read FConnection write FConnection;
    property ID: String read FID write FID;
    property Name: String read FName write FName;
    property Info: String read FInfo write FInfo;
    property Data: TObject read FData write FData;
  end;

  TsgcWSPresenceMemberList = class
    (TsgcThreadList{$IFDEF NEXTGEN}<TsgcWSPresenceMember>{$ENDIF})
  private
    FOwnObjects: Boolean;
    function GetCount: Integer;
  protected
    function GetMemberByIndex(Index: Integer): TsgcWSPresenceMember; virtual;
  public
    destructor Destroy; override;
  public
    procedure Clear; override;
  public
    procedure AddMember(const aMember: TsgcWSPresenceMember);
    function DeleteMember(const aID: String): Boolean;
    function GetMember(const aID: String): TsgcWSPresenceMember;
  public
    property Member[Index: Integer]: TsgcWSPresenceMember read
        GetMemberByIndex; default;
  public
    property Count: Integer read GetCount;
    property OwnObjects: Boolean read FOwnObjects write FOwnObjects;
  end;

  TsgcWSPresenceChannel = class(TPersistent)
  private
    FMembers: TsgcWSPresenceMemberList;
    FName: String;
    function GetMembers: TsgcWSPresenceMemberList;
  public
    destructor Destroy; override;
  protected
    procedure Clear; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  public
    property Members: TsgcWSPresenceMemberList read GetMembers;
    property Name: String read FName write FName;
  end;

  TsgcWSPresenceMsg = class(TPersistent)
  private
    FMsgType: TwsMessageType;
    FText: String;
    FStream: TMemoryStream;
  public
    constructor Create; virtual;
  public
    procedure Clear;
    procedure Assign(aSource: TPersistent); override;
  public
    property MsgType: TwsMessageType read FMsgType write FMsgType;
  public
    property Text: String read FText write FText;
  end;

  TsgcWSPresenceChannelList = class(TsgcThreadList{$IFDEF NEXTGEN}<TsgcWSPresenceChannel>{$ENDIF})
  public
    destructor Destroy; override;
  public
    procedure Clear; override;
  public
    procedure DeleteMember(const aID: String);
  public
    procedure AddChannel(const aChannel: TsgcWSPresenceChannel);
    function DeleteChannel(const aName: String): Boolean;
    function GetChannel(const aName: String): TsgcWSPresenceChannel;
  end;

  TsgcWSPresenceMessage = class(TsgcWSMessage_Base)
  private
    FID: string;
    Fmethod: string;
    Fversion: String;
    FMember: TsgcWSPresenceMember;
    FChannel: TsgcWSPresenceChannel;
    FMsg: TsgcWSPresenceMsg;
    FError: TsgcWSPresenceError;
    FMembers: TsgcWSPresenceMemberList;
    function GetMember: TsgcWSPresenceMember;
    function GetChannel: TsgcWSPresenceChannel;
    function GetMsg: TsgcWSPresenceMsg;
    function GetError: TsgcWSPresenceError;
    function GetMembers: TsgcWSPresenceMemberList;
  protected
    function DoReadJSONValue(const aField: string; aJSONObject: IsgcObjectJSON =
        nil): Variant; overload; override;
    function DoReadJSONValue(aItem: Integer; aJSONObject: TsgcObjectJSON = nil):
        Variant; overload; override;
  protected
    procedure DoWriteJSONValue(const aName, aValue: string; aJSONObject:
        IsgcObjectJSON = nil); overload; override;
  public
    constructor Create(aOwner: TCOmponent); override;
    destructor Destroy; override;
  public
    procedure Clear(aForceClear: Boolean = False); override;
  public
    procedure Read(const aMessage: String); override;
    function Write: string; override;
  private
    FBroadcast: Boolean;
    FEncodeBase64: Boolean;
  public
    property Broadcast: Boolean read FBroadcast write FBroadcast;
    property EncodeBase64: Boolean read FEncodeBase64 write FEncodeBase64;
    { custom }
  public
    property Members: TsgcWSPresenceMemberList read GetMembers write FMembers;
    property Member: TsgcWSPresenceMember read GetMember write FMember;
    property Channel: TsgcWSPresenceChannel read GetChannel write FChannel;
    property Msg: TsgcWSPresenceMsg read GetMsg write FMsg;
    property Error: TsgcWSPresenceError read GetError write FError;
    { custom }

    { json-rpc }
  public
    property ID: string read FID write FID;
    property method: string read Fmethod write Fmethod;
    property version: String read Fversion;
    { json-rpc }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses
  sgcWebSocket_Const, sgcWebSocket_Helpers;

constructor TsgcWSPresenceMessage.Create(aOwner: TCOmponent);
begin
  inherited;
  Fversion := CS_JSONRPC_VERSION;
  FBroadcast := False;
  FEncodeBase64 := False;
end;

destructor TsgcWSPresenceMessage.Destroy;
begin
  sgcFree(FMembers);
  sgcFree(FMember);
  sgcFree(FChannel);
  sgcFree(FMsg);
  sgcFree(FError);
  inherited;
end;

procedure TsgcWSPresenceMessage.Clear(aForceClear: Boolean = False);
begin
  if not FBroadcast then
  begin
    if (not FIsWriting and not FIsReading) or (aForceClear = True) then
    begin
      ID := '';
      method := '';
      Member.Clear;
      Channel.Clear;
      Msg.Clear;
      Error.Clear;
      Members.Clear;
      inherited;
    end;
  end;
end;

function TsgcWSPresenceMessage.DoReadJSONValue(const aField: string;
    aJSONObject: IsgcObjectJSON = nil): Variant;
begin
  Result := GetDecodedString(inherited DoReadJSONValue(aField, aJSONObject));
end;

function TsgcWSPresenceMessage.DoReadJSONValue(aItem: Integer; aJSONObject:
    TsgcObjectJSON = nil): Variant;
begin
  Result := GetDecodedString(inherited DoReadJSONValue(aItem, aJSONObject));
end;

procedure TsgcWSPresenceMessage.DoWriteJSONValue(const aName, aValue: string;
    aJSONObject: IsgcObjectJSON = nil);
var
  vValue: string;
begin
  vValue := aValue;
  if EncodeBase64 and (aName <> CS_JSON_RPC) and (aValue <> '') then
    vValue := GetEncodedString(aValue);
  inherited DoWriteJSONValue(aName, vValue, aJSONObject);
end;

function TsgcWSPresenceMessage.GetChannel: TsgcWSPresenceChannel;
begin
  if not Assigned(FChannel) then
    FChannel := TsgcWSPresenceChannel.Create;
  Result := FChannel;
end;

function TsgcWSPresenceMessage.GetError: TsgcWSPresenceError;
begin
  if not Assigned(FError) then
    FError := TsgcWSPresenceError.Create;
  Result := FError;
end;

function TsgcWSPresenceMessage.GetMember: TsgcWSPresenceMember;
begin
  if not Assigned(FMember) then
    FMember := TsgcWSPresenceMember.Create;
  Result := FMember;
end;

function TsgcWSPresenceMessage.GetMembers: TsgcWSPresenceMemberList;
begin
  if not Assigned(FMembers) then
  begin
    FMembers := TsgcWSPresenceMemberList.Create;
    FMembers.ListType := tdlThreadDictionary;
  end;
  Result := FMembers;
end;

function TsgcWSPresenceMessage.GetMsg: TsgcWSPresenceMsg;
begin
  if not Assigned(FMsg) then
    FMsg := TsgcWSPresenceMsg.Create;
  Result := FMsg;
end;

procedure TsgcWSPresenceMessage.Read(const aMessage: String);
var
  i: Integer;
  oMember: TsgcWSPresenceMember;
begin
  Try
    DoEnterRead(aMessage);
  Except
    On E: Exception do
    begin
      DoLeaveRead;
      raise;
    end;
  End;

  if DoReadJSONValue(CS_JSON_RPC) <> CS_JSONRPC_VERSION then
    raise Exception.Create(S_ERROR_DECODING_MESSAGE);

  method := DoReadJSONValue(CS_METHOD);

  if JSON.Node[CS_MEMBER] <> nil then
  begin
    Member.ID := DoReadJSONValue(CS_ID, JSON.Node[CS_MEMBER]);
    Member.Name := DoReadJSONValue(CS_NAME, JSON.Node[CS_MEMBER]);
    Member.Info := DoReadJSONValue(CS_INFO, JSON.Node[CS_MEMBER]);
  end;

  if JSON.Node[CS_CHANNEL] <> nil then
  begin
    Channel.Name := DoReadJSONValue(CS_NAME, JSON.Node[CS_CHANNEL]);
  end;

  if JSON.Node[CS_MESSAGE] <> nil then
  begin
    Msg.Text := DoReadJSONValue(CS_TEXT, JSON.Node[CS_MESSAGE]);
  end;

  if JSON.Node[CS_ERROR] <> nil then
  begin
    Error.Code := DoReadJSONValue(CS_CODE, JSON.Node[CS_ERROR]);
    Error.Text := DoReadJSONValue(CS_TEXT, JSON.Node[CS_ERROR]);
  end;

  if JSON.Node[CS_MEMBERS] <> nil then
  begin
    Members.Clear;
    JSON.Node[CS_MEMBERS].JSONObject.IsArray := True;
    for i := 0 to JSON.Node[CS_MEMBERS].JSONObject.Count - 1 do
    begin
      oMember := TsgcWSPresenceMember.Create;
      if JSON.Node[CS_MEMBERS].JSONObject.Item[i].Node[CS_ID] <> nil then
        oMember.ID := JSON.Node[CS_MEMBERS].JSONObject.Item[i].Node[CS_ID].Value;
      if JSON.Node[CS_MEMBERS].JSONObject.Item[i].Node[CS_NAME] <> nil then
        oMember.Name := JSON.Node[CS_MEMBERS].JSONObject.Item[i].Node[CS_NAME].Value;
      if JSON.Node[CS_MEMBERS].JSONObject.Item[i].Node[CS_INFO] <> nil then
        oMember.Info := JSON.Node[CS_MEMBERS].JSONObject.Item[i].Node[CS_INFO].Value;
      Members.AddMember(oMember);
    end;
  end;

  ID := DoReadJSONValue(CS_ID);
  inherited;
end;

function TsgcWSPresenceMessage.Write: string;
var
  i: Integer;
  oJSON, oJSON2: IsgcObjectJSON;
  oList: TList{$IFDEF NEXTGEN}<TsgcWSPresenceMember>{$ENDIF};
begin
  DoEnterWrite;

  DoWriteJSONValue(CS_JSON_RPC, version);
  // ... error
  if method = CS_PRESENCE_ERROR_MEMBER_CHANNEL then
  begin
    DoWriteJSONValue(CS_METHOD, method);

    oJSON := DoAddJSONObject(CS_ERROR);
    DoWriteJSONValue(CS_CODE, Error.Code, oJSON);
    DoWriteJSONValue(CS_TEXT, Error.Text, oJSON);

    oJSON := DoAddJSONObject(CS_CHANNEL);
    DoWriteJSONValue(CS_NAME, Channel.Name, oJSON);

    oJSON := DoAddJSONObject(CS_MEMBER);
    DoWriteJSONValue(CS_ID, Member.ID, oJSON);
    DoWriteJSONValue(CS_NAME, Member.Name, oJSON);
    DoWriteJSONValue(CS_INFO, Member.Info, oJSON);
  end
  // ... session
  else if method = CS_PRESENCE_SESSION then
  begin
    DoWriteJSONValue(CS_METHOD, method);

    oJSON := DoAddJSONObject(CS_MESSAGE);
    DoWriteJSONValue(CS_TEXT, Msg.Text, oJSON);
  end
  // ... member
  else if (method = CS_PRESENCE_NEW_MEMBER) then
  begin
    DoWriteJSONValue(CS_METHOD, method);

    oJSON := DoAddJSONObject(CS_MEMBER);
    DoWriteJSONValue(CS_ID, Member.ID, oJSON);
    DoWriteJSONValue(CS_NAME, Member.Name, oJSON);
    DoWriteJSONValue(CS_INFO, Member.Info, oJSON);
  end
  else if (method = CS_PRESENCE_REMOVE_MEMBER) then
  begin
    DoWriteJSONValue(CS_METHOD, method);

    oJSON := DoAddJSONObject(CS_MEMBER);
    DoWriteJSONValue(CS_ID, Member.ID, oJSON);
    DoWriteJSONValue(CS_NAME, Member.Name, oJSON);
    DoWriteJSONValue(CS_INFO, Member.Info, oJSON);
  end
  else if (method = CS_PRESENCE_NEW_CHANNEL_MEMBER) or
    (method = CS_PRESENCE_REMOVE_CHANNEL_MEMBER) then
  begin
    DoWriteJSONValue(CS_METHOD, method);

    oJSON := DoAddJSONObject(CS_MEMBER);
    DoWriteJSONValue(CS_ID, Member.ID, oJSON);
    DoWriteJSONValue(CS_NAME, Member.Name, oJSON);
    DoWriteJSONValue(CS_INFO, Member.Info, oJSON);

    oJSON := DoAddJSONObject(CS_CHANNEL);
    DoWriteJSONValue(CS_NAME, Channel.Name, oJSON);
  end
  else if (method = CS_PRESENCE_PUBLISH_MSG) then
  begin
    DoWriteJSONValue(CS_METHOD, method);

    oJSON := DoAddJSONObject(CS_MESSAGE);
    DoWriteJSONValue(CS_TEXT, Msg.Text, oJSON);

    oJSON := DoAddJSONObject(CS_CHANNEL);
    DoWriteJSONValue(CS_NAME, Channel.Name, oJSON);

    oJSON := DoAddJSONObject(CS_MEMBER);
    DoWriteJSONValue(CS_ID, Member.ID, oJSON);
    DoWriteJSONValue(CS_NAME, Member.Name, oJSON);
    DoWriteJSONValue(CS_INFO, Member.Info, oJSON);
  end
  else if (method = CS_PRESENCE_GET_MEMBERS) then
  begin
    DoWriteJSONValue(CS_METHOD, method);

    oJSON := DoAddJSONObject(CS_CHANNEL);
    DoWriteJSONValue(CS_NAME, Channel.Name, oJSON);

    oJSON := DoAddJSONObject(CS_MEMBER);
    DoWriteJSONValue(CS_ID, Member.ID, oJSON);
    DoWriteJSONValue(CS_NAME, Member.Name, oJSON);
    DoWriteJSONValue(CS_INFO, Member.Info, oJSON);
  end
  else if (method = CS_PRESENCE_SEND_MEMBERS) then
  begin
    DoWriteJSONValue(CS_METHOD, method);

    oJSON := DoAddJSONObject(CS_CHANNEL);
    DoWriteJSONValue(CS_NAME, Channel.Name, oJSON);

    oJSON := DoAddJSONObject(CS_MEMBER);
    DoWriteJSONValue(CS_ID, Member.ID, oJSON);
    DoWriteJSONValue(CS_NAME, Member.Name, oJSON);
    DoWriteJSONValue(CS_INFO, Member.Info, oJSON);

    oList := Members.LockList;
    Try
      oJSON := DoAddJSONObject(CS_MEMBERS);
      oJSON.JSONObject.IsArray := True;
      for i := 0 to oList.Count - 1 do
      begin
        oJSON2 := oJSON.JSONObject.AddPair(IntToStr(i), '');
        DoWriteJSONValue(CS_ID, TsgcWSPresenceMember(oList[i]).ID, oJSON2);
        DoWriteJSONValue(CS_NAME, TsgcWSPresenceMember(oList[i]).Name, oJSON2);
        DoWriteJSONValue(CS_INFO, TsgcWSPresenceMember(oList[i]).Info, oJSON2);
      end;
    Finally
      Members.UnlockList;
    End;
  end
  else if (method = CS_PRESENCE_CHANNEL_INVITATION) then
  begin
    DoWriteJSONValue(CS_METHOD, method);

    oJSON := DoAddJSONObject(CS_CHANNEL);
    DoWriteJSONValue(CS_NAME, Channel.Name, oJSON);

    oJSON := DoAddJSONObject(CS_MEMBER);
    DoWriteJSONValue(CS_ID, Member.ID, oJSON);
    DoWriteJSONValue(CS_NAME, Member.Name, oJSON);
    DoWriteJSONValue(CS_INFO, Member.Info, oJSON);

    oList := Members.LockList;
    Try
      oJSON := DoAddJSONObject(CS_MEMBERS);
      oJSON.JSONObject.IsArray := True;
      for i := 0 to oList.Count - 1 do
      begin
        oJSON2 := oJSON.JSONObject.AddPair(IntToStr(i), '');
        DoWriteJSONValue(CS_ID, TsgcWSPresenceMember(oList[i]).ID, oJSON2);
        DoWriteJSONValue(CS_NAME, TsgcWSPresenceMember(oList[i]).Name, oJSON2);
        DoWriteJSONValue(CS_INFO, TsgcWSPresenceMember(oList[i]).Info, oJSON2);
      end;
    Finally
      Members.UnlockList;
    End;
  end;

  if ID <> '' then
    DoWriteJSONValue(CS_ID, ID);

  Result := inherited Write;
end;

destructor TsgcWSPresenceMemberList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TsgcWSPresenceMemberList.AddMember(const aMember:
    TsgcWSPresenceMember);
begin
  AddKey(aMember, aMember.ID);
end;

procedure TsgcWSPresenceMemberList.Clear;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TsgcWSPresenceMember>{$ENDIF};
  oPresenceMember: TsgcWSPresenceMember;
begin
  oList := LockList;
  Try
    for i := oList.Count - 1 Downto 0 do
    begin
      oPresenceMember := TsgcWSPresenceMember(oList.Items[i]);
      DeleteKey(i, oPresenceMember.ID);
      if OwnObjects then
        sgcFree(oPresenceMember);
    end;
  Finally
    UnlockList;
  End;
  inherited;
end;

function TsgcWSPresenceMemberList.DeleteMember(const aID: String): Boolean;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TsgcWSPresenceMember>{$ENDIF};
  oPresenceMember: TsgcWSPresenceMember;
begin
  Result := False;

  oList := LockList;
  Try
    for i := 0 to oList.Count - 1 do
    begin
      if TsgcWSPresenceMember(oList[i]).ID = aID then
      begin
        Result := True;
        if OwnObjects then
        begin
          oPresenceMember := TsgcWSPresenceMember(oList.Items[i]);
          sgcFree(oPresenceMember);
        end;
        DeleteKey(i, aID);
        break;
      end;
    end;
  Finally
    UnlockList;
  End;
end;

function TsgcWSPresenceMemberList.GetCount: Integer;
var
  oList: TList{$IFDEF NEXTGEN}<TsgcWSPresenceMember>{$ENDIF};
begin
  oList := LockList;
  Try
    result := oList.Count;
  Finally
    UnlockList;
  End;
end;

function TsgcWSPresenceMemberList.GetMember(const aID: String)
  : TsgcWSPresenceMember;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TsgcWSPresenceMember>{$ENDIF};
begin
  Result := nil;

  case ListType of
    tdlThreadDictionary, tdlOnlyDictionary:
      begin
        result := TsgcWSPresenceMember(GetValue(aID));
      end;
    tdlOnlyThread:
      begin
        oList := LockList;
        Try
          for i := 0 to oList.Count - 1 do
          begin
            if TsgcWSPresenceMember(oList[i]).ID = aID then
            begin
              Result := TsgcWSPresenceMember(oList[i]);
              break;
            end;
          end;
        Finally
          UnlockList;
        End;
      end;
  end;
end;

function TsgcWSPresenceMemberList.GetMemberByIndex(Index: Integer):
    TsgcWSPresenceMember;
var
  oList: TList{$IFDEF NEXTGEN}<TsgcWSPresenceMember>{$ENDIF};
begin
  Result := nil;

  oList := LockList;
  Try
    if Index < oList.Count then
      Result := TsgcWSPresenceMember(oList[Index]);
  Finally
    UnlockList;
  End;
end;

destructor TsgcWSPresenceChannel.Destroy;
begin
  sgcFree(FMembers);
  inherited;
end;

procedure TsgcWSPresenceChannel.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSPresenceChannel then
  begin
    Name := TsgcWSPresenceChannel(aSource).Name;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSPresenceChannel.Clear;
begin
  if Assigned(FMembers) then
    FMembers.Clear;
  FName := '';
end;

function TsgcWSPresenceChannel.GetMembers: TsgcWSPresenceMemberList;
begin
  if not Assigned(FMembers) then
  begin
    FMembers := TsgcWSPresenceMemberList.Create;
    FMembers.OwnObjects := False; // instance of Members
  end;
  Result := FMembers;
end;

destructor TsgcWSPresenceChannelList.Destroy;
begin
  Clear;
  inherited;
end;

procedure TsgcWSPresenceChannelList.Clear;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TsgcWSPresenceChannel>{$ENDIF};
  oPresenceChannel: TsgcWSPresenceChannel;
begin
  oList := LockList;
  Try
    for i := oList.Count - 1 Downto 0 do
    begin
      oPresenceChannel := TsgcWSPresenceChannel(oList.Items[i]);
      sgcFree(oPresenceChannel);
    end;
  Finally
    UnlockList;
  End;
  inherited;
end;

function TsgcWSPresenceChannelList.DeleteChannel(const aName: String): Boolean;
begin
  Result := False;
end;

procedure TsgcWSPresenceChannelList.DeleteMember(const aID: String);
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TsgcWSPresenceChannel>{$ENDIF};
begin
  oList := LockList;
  Try
    for i := oList.Count - 1 Downto 0 do
      TsgcWSPresenceChannel(oList.Items[i]).Members.DeleteMember(aID);
  Finally
    UnlockList;
  End;
end;

function TsgcWSPresenceChannelList.GetChannel(const aName: String)
  : TsgcWSPresenceChannel;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TsgcWSPresenceChannel>{$ENDIF};
begin
  Result := nil;

  case ListType of
    tdlThreadDictionary, tdlOnlyDictionary:
      begin
        result := TsgcWSPresenceChannel(GetValue(aName));
      end;
    tdlOnlyThread:
      begin
        oList := LockList;
        Try
          for i := 0 to oList.Count - 1 do
          begin
            if TsgcWSPresenceChannel(oList.Items[i]).Name = aName then
            begin
              Result := TsgcWSPresenceChannel(oList.Items[i]);
              break;
            end;
          end;
        Finally
          UnlockList;
        End;
      end;
  end;
end;

procedure TsgcWSPresenceChannelList.AddChannel(const aChannel:
    TsgcWSPresenceChannel);
begin
  AddKey(aChannel, aChannel.Name);
end;

destructor TsgcWSPresenceMember.Destroy;
begin
  Clear;
  inherited;
end;

procedure TsgcWSPresenceMember.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSPresenceMember then
  begin
    ID := TsgcWSPresenceMember(aSource).ID;
    Name := TsgcWSPresenceMember(aSource).Name;
    Info := TsgcWSPresenceMember(aSource).Info;
    Data := TsgcWSPresenceMember(aSource).Data;
    Connection := TsgcWSPresenceMember(aSource).Connection;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSPresenceMember.Clear;
begin
  FConnection := nil;
  FID := '';
  FName := '';
  FInfo := '';
  FData := nil;
end;

constructor TsgcWSPresenceMsg.Create;
begin
  inherited;
  FMsgType := mtText;
end;

procedure TsgcWSPresenceMsg.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSPresenceMsg then
  begin
    Text := TsgcWSPresenceMsg(aSource).Text;
    MsgType := TsgcWSPresenceMsg(aSource).MsgType;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSPresenceMsg.Clear;
begin
  if Assigned(FStream) then
    FStream.Size := 0;
  Text := '';
end;

procedure TsgcWSPresenceError.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSPresenceError then
  begin
    Code := TsgcWSPresenceError(aSource).Code;
    Text := TsgcWSPresenceError(aSource).Text;
  end
  else
    inherited Assign(aSource);
end;

procedure TsgcWSPresenceError.Clear;
begin
  Code := 0;
  Text := '';
end;

{$ENDIF}

end.
