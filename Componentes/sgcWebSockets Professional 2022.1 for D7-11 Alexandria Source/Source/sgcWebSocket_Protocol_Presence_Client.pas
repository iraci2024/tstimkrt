{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcWebSocket_Protocol_Presence_Client;

interface

{$I sgcVer.inc}

{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils,
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  {$IFDEF NEXTGEN}System.Generics.Collections, {$ELSE}Contnrs, {$ENDIF}
  // sgc
  sgcJSON, sgcWebSocket_Protocol_Presence_Message, sgcWebSocket_Types,
  sgcWebSocket_Classes, sgcWebSocket_Protocol_Base_Client,
  sgcWebSocket_HTTPResponse, sgcWebSocket_Protocol_Presence_Server,
  sgcWebSocket_Protocol_Base_Message, sgcWebSocket_Classes_SyncObjs;

type

  TsgcWSPresenceSessionEvent = procedure(Connection: TsgcWSConnection; Session:
      string) of object;
  TsgcWSPresencePublishMsgEvent = procedure(Connection: TsgcWSConnection; const
      aMsg: TsgcWSPresenceMsg; const aChannel: TsgcWSPresenceChannel; const
      aMember: TsgcWSPresenceMember) of object;
  TsgcWSPresenceGetMembersEvent = procedure(Connection: TsgcWSConnection; const
      aMembers: TsgcWSPresenceMemberList; const aChannel: TsgcWSPresenceChannel)
      of object;
  TsgcWSPresenceChannelInvitationEvent = procedure(Connection: TsgcWSConnection;
      const aMember: TsgcWSPresenceMember; const aChannel: TsgcWSPresenceChannel;
      var Accept: Boolean) of object;

  TsgcWSPresenceMember_Options = class(TPersistent)
  private
    FInfo: TStringList;
    FName: String;
    procedure SetInfo(const Value: TStringList);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property Name: String read FName write FName;
    property Info: TStringList read FInfo write SetInfo;
  end;


  TsgcWSProtocol_Presence_Client = class(TsgcWSProtocol_Acknowledgment_Client_Base)
  { wsmessage }
  private
    FWSMessageId: String;
  protected
    FWSMessage: TsgcWSPresenceMessage;
    function GetWSMessage: TsgcWSPresenceMessage;
    function GetWSMessageByConnection(const aConnection: TsgcWSConnection):
        TsgcWSPresenceMessage;
  protected
    property WSMessage: TsgcWSPresenceMessage read GetWSMessage write FWSMessage;
  { wsmessage }

  { from TsgcWSComponent }
  private
    FWSConnection: TsgcWSConnection;
  protected
    procedure DoEventConnect(aConnection: TsgcWSConnection); override;
    procedure DoEventMessage(aConnection: TsgcWSConnection; const Text: string);
        override;
    procedure DoEventDisconnect(aConnection: TsgcWSConnection; Code: Integer);
        override;
  { from TsgcWSComponent }

  { from TsgcWSProtocol }
  protected
    procedure DoInitialize(aConnection: TsgcWSConnection); override;
    procedure DoFinalize(aConnection: TsgcWSConnection); override;
    procedure DoClear(aConnection: TsgcWSConnection); override;
  { from TsgcWSProtocol }


  { from TsgcWSProtocol_Client_Base }
  protected
    procedure DoWriteRawData(const aText: String); virtual;
  public
    procedure WriteData(const aText: String); override;
  { from TsgcWSProtocol_Client_Base }

  { from  TsgcWSProtocol_Acknowledgment_Client_Base }
  protected
    procedure OnAcknowledgmentMessageEvent(aConnection: TsgcWSConnection; const
        aText: String); override;
  { from  TsgcWSProtocol_Acknowledgment_Client_Base }

  protected
    procedure DoWriteMessageText(const aText: String); virtual;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

  { presence }
  private
    FPresence: TsgcWSPresenceMember_Options;
    procedure SetPresence(const Value: TsgcWSPresenceMember_Options);
  public
    property Presence: TsgcWSPresenceMember_Options read FPresence write
        SetPresence;
  { presence }

  { procedures }
  private
    FSession: String;
  protected
    procedure DoWriteDataWithAck(const aMessage: TsgcWSPresenceMessage);
  protected
    procedure DoAcknowledgment(const aID: string); virtual;
  protected
    procedure DoProcessAcknowledgment(aID: String); virtual;
  protected
    procedure DoNewMember; virtual;
    procedure DoRemoveMember; virtual;
  public
    procedure Subscribe(const aChannel: String);
    procedure UnSubscribe(const aChannel: String);
  public
    procedure Invite(const aChannel, aMemberID: String);
  public
    procedure GetMembers; overload;
    procedure GetMembers(const aChannel: String); overload;
  public
    procedure Publish(const aText: String; const aChannel: String = '');
  { procedures }

    { properties }
  private
    FEncodeBase64: Boolean;
  public
    property EncodeBase64: Boolean read FEncodeBase64 write FEncodeBase64;
    { properties }

  { events }
  private
    FOnChannelInvitation: TsgcWSPresenceChannelInvitationEvent;
    FOnNewChannelMember: TsgcWSPresenceNewMemberChannelEvent;
    FOnNewMember: TsgcWSPresenceNewMemberEvent;
    FOnErrorMemberChannel: TsgcWSPresenceMemberChannelErrorEvent;
    FOnErrorPublishMsg: TsgcWSPresencePublishMsgErrorEvent;
    FOnGetMembers: TsgcWSPresenceGetMembersEvent;
    FOnPublishMsg: TsgcWSPresencePublishMsgEvent;
    FOnRemoveChannelMember: TsgcWSPresenceRemoveMemberChannelEvent;
    FOnRemoveMember: TsgcWSPresenceRemoveMemberEvent;
    FOnSession: TsgcWSPresenceSessionEvent;
  protected
    procedure DoNewMemberEvent(aConnection: TsgcWSConnection; const aMember:
        TsgcWSPresenceMember); virtual;
    procedure DoRemoveMemberEvent(aConnection: TsgcWSConnection; aMember:
        TsgcWSPresenceMember); virtual;
    procedure DoSessionEvent(aConnection: TsgcWSConnection; const aMessage:
        TsgcWSPresenceMessage); virtual;
    procedure DoNewChannelMemberEvent(aConnection: TsgcWSConnection; aChannel:
        TsgcWSPresenceChannel; aMember: TsgcWSPresenceMember); virtual;
    procedure DoRemoveChannelMemberEvent(aConnection: TsgcWSConnection; aChannel:
        TsgcWSPresenceChannel; aMember: TsgcWSPresenceMember); virtual;
    procedure DoPublishMsgEvent(aConnection: TsgcWSConnection; aMsg:
        TsgcWSPresenceMsg; aChannel: TsgcWSPresenceChannel; aMember:
        TsgcWSPresenceMember); virtual;
    procedure DoGetMembersEvent(aConnection: TsgcWSConnection; const aMembers:
        TsgcWSPresenceMemberList; const aChannel: TsgcWSPresenceChannel); virtual;
    procedure DoErrorMemberChannelEvent(aConnection: TsgcWSConnection; const
        aError: TsgcWSPresenceError; const aChannel: TsgcWSPresenceChannel; const
        aMember: TsgcWSPresenceMember); virtual;
    procedure DoErrorPublishMsgEvent(aConnection: TsgcWSConnection; const aError:
        TsgcWSPresenceError; const aMsg: TsgcWSPresenceMsg; const aChannel:
        TsgcWSPresenceChannel; const aMember: TsgcWSPresenceMember);
    procedure DoChannelInvitationEvent(aConnection: TsgcWSConnection; const
        aMember: TsgcWSPresenceMember; const aChannel: TsgcWSPresenceChannel);
        virtual;
  public
    property OnChannelInvitation: TsgcWSPresenceChannelInvitationEvent read
        FOnChannelInvitation write FOnChannelInvitation;
    property OnNewChannelMember: TsgcWSPresenceNewMemberChannelEvent read
        FOnNewChannelMember write FOnNewChannelMember;
    property OnNewMember: TsgcWSPresenceNewMemberEvent read FOnNewMember write
        FOnNewMember;
    property OnErrorMemberChannel: TsgcWSPresenceMemberChannelErrorEvent read
        FOnErrorMemberChannel write FOnErrorMemberChannel;
    property OnErrorPublishMsg: TsgcWSPresencePublishMsgErrorEvent read
        FOnErrorPublishMsg write FOnErrorPublishMsg;
    property OnGetMembers: TsgcWSPresenceGetMembersEvent read FOnGetMembers write
        FOnGetMembers;
    property OnRemoveChannelMember: TsgcWSPresenceRemoveMemberChannelEvent read
        FOnRemoveChannelMember write FOnRemoveChannelMember;
    property OnRemoveMember: TsgcWSPresenceRemoveMemberEvent read FOnRemoveMember
        write FOnRemoveMember;
    property OnSession: TsgcWSPresenceSessionEvent read FOnSession write FOnSession;
    property OnPublishMsg: TsgcWSPresencePublishMsgEvent read FOnPublishMsg write
        FOnPublishMsg;
  { events }
  end;


{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses sgcWebSocket_Const, sgcBase_Helpers;

const
  CS_QOS_LIST = 1;
  CS_RPC_LIST = 0;

constructor TsgcWSProtocol_Presence_Client.Create(aOwner: TComponent);
begin
  inherited;
  FWSMessageId := NewGuid;
  FProtocol := CS_PROTOCOL_PRESENCE;
  FPresence := TsgcWSPresenceMember_Options.Create;
  FEncodeBase64 := False;
end;

destructor TsgcWSProtocol_Presence_Client.Destroy;
begin
  sgcFree(FPresence);
  inherited;
end;

procedure TsgcWSProtocol_Presence_Client.DoEventMessage(aConnection:
    TsgcWSConnection; const Text: string);
var
  oMessage: TsgcWSPresenceMessage;
begin
  {$IFDEF SGC_DEBUG}
  DoLog(self, aConnection, 'DoEventMessage', '[Text]: ' + Text);
  {$ENDIF}

  if DoRawMessage(aConnection, Text) then exit;

  oMessage := WSMessage;
  oMessage.Read(Text);

  // ...acknowledgment
  if (oMessage.id <> '') and (oMessage.method <> CS_PRESENCE_ACKNOWLEDGMENT) then
    DoAcknowledgment(oMessage.id);

  if oMessage.method = CS_PRESENCE_SESSION then
    DoSessionEvent(aConnection, oMessage)
  else if oMessage.method = CS_PRESENCE_NEW_MEMBER then
    DoNewMemberEvent(aConnection, oMessage.Member)
  else if oMessage.method = CS_PRESENCE_REMOVE_MEMBER then
    DoRemoveMemberEvent(aConnection, oMessage.Member)
  else if oMessage.method = CS_PRESENCE_NEW_CHANNEL_MEMBER then
    DoNewChannelMemberEvent(aConnection, oMessage.Channel, oMessage.Member)
  else if oMessage.method = CS_PRESENCE_REMOVE_CHANNEL_MEMBER then
    DoRemoveChannelMemberEvent(aConnection, oMessage.Channel, oMessage.Member)
  else if oMessage.method = CS_PRESENCE_PUBLISH_MSG then
    DoPublishMsgEvent(aConnection, oMessage.Msg, oMessage.Channel, oMessage.Member)
  else if oMessage.method = CS_PRESENCE_ERROR_MEMBER_CHANNEL then
    DoErrorMemberChannelEvent(aConnection, oMessage.Error, oMessage.Channel, oMessage.Member)
  else if oMessage.method = CS_PRESENCE_ACKNOWLEDGMENT then
    DoProcessAcknowledgment(oMessage.id)
  else if oMessage.method = CS_PRESENCE_CHANNEL_INVITATION then
    DoChannelInvitationEvent(aConnection, oMessage.Member, oMessage.Channel)
  else if oMessage.method = CS_PRESENCE_SEND_MEMBERS then
  begin
    DoGetMembersEvent(aConnection, oMessage.Members, oMessage.Channel);
    oMessage.Members.OwnObjects := True; // destroy members objects
    oMessage.Clear(True);
  end
  else
    inherited;
end;

function TsgcWSProtocol_Presence_Client.GetWSMessage: TsgcWSPresenceMessage;
begin
  result := GetWSMessageByConnection(FWSConnection);

  if not Assigned(result) then
  begin
    if not Assigned(FWSMessage) then
    begin
      FWSMessage := TsgcWSPresenceMessage.Create(self);
      FWSMessage.EncodeBase64 := EncodeBase64;
    end;
    Result := FWSMessage;
  end;
end;

procedure TsgcWSProtocol_Presence_Client.DoAcknowledgment(const aID: string);
var
  oJSON: IsgcJSON;
begin
  oJSON := GetJSONInstance;
  Try
    oJSON.AddPair(CS_JSON_RPC, CS_JSONRPC_VERSION);
    oJSON.AddPair(CS_METHOD, CS_PRESENCE_ACKNOWLEDGMENT);
    oJSON.AddPair(CS_ID, aID);

    DoWriteMessageText(oJSON.Text);
  Finally
    FreeJSONInstance(oJSON);
  End;
end;

procedure TsgcWSProtocol_Presence_Client.DoChannelInvitationEvent(aConnection:
    TsgcWSConnection; const aMember: TsgcWSPresenceMember; const aChannel:
    TsgcWSPresenceChannel);
var
  vAccept: Boolean;
begin
  vAccept := False;
  if Assigned(FOnChannelInvitation) then
  begin
    FOnChannelInvitation(aConnection, aMember, aChannel, vAccept);
    if vAccept then
      Subscribe(aChannel.Name);
  end;
end;

procedure TsgcWSProtocol_Presence_Client.DoClear(aConnection: TsgcWSConnection);
begin
  inherited;
  DoRemoveMember;
end;

procedure TsgcWSProtocol_Presence_Client.DoErrorMemberChannelEvent(aConnection:
    TsgcWSConnection; const aError: TsgcWSPresenceError; const aChannel:
    TsgcWSPresenceChannel; const aMember: TsgcWSPresenceMember);
begin
  if Assigned(FOnErrorMemberChannel) then
    FOnErrorMemberChannel(aConnection, aError, aChannel, aMember);
end;

procedure TsgcWSProtocol_Presence_Client.DoErrorPublishMsgEvent(aConnection:
    TsgcWSConnection; const aError: TsgcWSPresenceError; const aMsg:
    TsgcWSPresenceMsg; const aChannel: TsgcWSPresenceChannel; const aMember:
    TsgcWSPresenceMember);
begin
  if Assigned(FOnErrorPublishMsg) then
    FOnErrorPublishMsg(aConnection, aError, aMsg, aChannel, aMember);
end;

procedure TsgcWSProtocol_Presence_Client.DoEventConnect(aConnection:
    TsgcWSConnection);
begin
  inherited;
  FWSConnection := aConnection;
end;

procedure TsgcWSProtocol_Presence_Client.DoEventDisconnect(aConnection:
    TsgcWSConnection; Code: Integer);
var
  vId: String;
begin
  vId := aConnection.Guid;
  if Assigned(FWSMessage) then
    FWSMessage.Clear(True);
  inherited;
  FWSConnection := nil;
end;

procedure TsgcWSProtocol_Presence_Client.DoSessionEvent(aConnection:
    TsgcWSConnection; const aMessage: TsgcWSPresenceMessage);
begin
  FSession := aMessage.Msg.Text;
  // ... new member
  DoNewMember;
  // ... session event
  if Assigned(FOnSession) then
    FOnSession(aConnection, FSession);
end;

procedure TsgcWSProtocol_Presence_Client.DoFinalize(aConnection:
    TsgcWSConnection);
begin
  inherited;
  DoRemoveMember;

  if Assigned(FWSMessage) then
    FWSMessage.Clear(True);
  FWSConnection := nil;
end;

procedure TsgcWSProtocol_Presence_Client.DoGetMembersEvent(aConnection:
    TsgcWSConnection; const aMembers: TsgcWSPresenceMemberList; const aChannel:
    TsgcWSPresenceChannel);
begin
  if Assigned(FOnGetMembers) then
    FOnGetMembers(aConnection, aMembers, aChannel);
end;

procedure TsgcWSProtocol_Presence_Client.DoInitialize(aConnection:
    TsgcWSConnection);
begin
  inherited;
  FWSConnection := aConnection;
end;

procedure TsgcWSProtocol_Presence_Client.DoNewChannelMemberEvent(aConnection:
    TsgcWSConnection; aChannel: TsgcWSPresenceChannel; aMember:
    TsgcWSPresenceMember);
begin
  if Assigned(FOnNewChannelMember) then
    FOnNewChannelMember(aConnection, aChannel, aMember);
end;

procedure TsgcWSProtocol_Presence_Client.DoWriteMessageText(const aText:
    String);
begin
  inherited WriteData(aText);
end;

procedure TsgcWSProtocol_Presence_Client.WriteData(const aText: String);
begin

end;

procedure TsgcWSProtocol_Presence_Client.DoWriteRawData(const aText: String);
begin
  inherited WriteData(aText);
end;

procedure TsgcWSProtocol_Presence_Client.DoNewMember;
var
  oMessage: TsgcWSPresenceMessage;
begin
  oMessage := TsgcWSPresenceMessage.Create(nil);
  Try
    oMessage.DoEnterWrite;
    Try
      oMessage.EncodeBase64 := EncodeBase64;
      oMessage.Method := CS_PRESENCE_NEW_MEMBER;
      oMessage.Member.Name := Presence.Name;
      oMessage.Member.ID := FSession;
      oMessage.Member.Info := Presence.Info.Text;

      DoWriteDataWithAck(oMessage);
    Finally
      oMessage.DoLeaveWrite;
    End;
  Finally
    sgcFree(oMessage);
  End;
end;

procedure TsgcWSProtocol_Presence_Client.DoNewMemberEvent(aConnection:
    TsgcWSConnection; const aMember: TsgcWSPresenceMember);
begin
  if Assigned(FOnNewMember) then
    FOnNewMember(aConnection, aMember);
end;

procedure TsgcWSProtocol_Presence_Client.DoProcessAcknowledgment(aID: String);
begin
  AcknowledgmentList.Delete(aID);
end;

procedure TsgcWSProtocol_Presence_Client.DoPublishMsgEvent(aConnection:
    TsgcWSConnection; aMsg: TsgcWSPresenceMsg; aChannel: TsgcWSPresenceChannel;
    aMember: TsgcWSPresenceMember);
begin
  if Assigned(FonPublishMsg) then
    FOnPublishMsg(aConnection, aMsg, aChannel, aMember);
end;

procedure TsgcWSProtocol_Presence_Client.DoRemoveChannelMemberEvent(
    aConnection: TsgcWSConnection; aChannel: TsgcWSPresenceChannel; aMember:
    TsgcWSPresenceMember);
begin
  if Assigned(FOnRemoveChannelMember) then
    FOnRemoveChannelMember(aConnection, aChannel, aMember);
end;

procedure TsgcWSProtocol_Presence_Client.DoRemoveMember;
var
  oMessage: TsgcWSPresenceMessage;
begin
  oMessage := TsgcWSPresenceMessage.Create(nil);
  Try
    Try
      oMessage.EncodeBase64 := EncodeBase64;
      oMessage.Method := CS_PRESENCE_REMOVE_MEMBER;
      oMessage.Member.ID := FSession;

      DoWriteDataWithAck(oMessage);
    Finally
      oMessage.DoLeaveWrite;
    End;
  Finally
    sgcFree(oMessage);
  End;
end;

procedure TsgcWSProtocol_Presence_Client.DoRemoveMemberEvent(aConnection:
    TsgcWSConnection; aMember: TsgcWSPresenceMember);
begin
  if Assigned(FOnRemoveMember) then
    FOnRemoveMember(aConnection, aMember);
end;

procedure TsgcWSProtocol_Presence_Client.DoWriteDataWithAck(const aMessage:
    TsgcWSPresenceMessage);
var
  oMessage: TsgcWSAckMessage;
  vText: String;
begin
  // ... add id
  if Acknowledgment.Enabled then
    aMessage.ID := NewGuid;

  vText := aMessage.Write;

  // ... queue message
  if Acknowledgment.Enabled then
  begin
    oMessage := TsgcWSAckMessage.Create;
    if Acknowledgment.Enabled then
      oMessage.ID := aMessage.ID;
    oMessage.Connection := FWSConnection;
    oMessage.Text := vText;
    AcknowledgmentList.Add(oMessage);
  end;

  // ... writedata to client
  DoWriteMessageText(vText);
end;

procedure TsgcWSProtocol_Presence_Client.GetMembers;
var
  oMessage: TsgcWSPresenceMessage;
begin
  oMessage := TsgcWSPresenceMessage.Create(nil);
  Try
    oMessage.DoEnterWrite;
    Try
      oMessage.EncodeBase64 := EncodeBase64;
      oMessage.Method := CS_PRESENCE_GET_MEMBERS;
      oMessage.Member.ID := FSession;
      DoWriteDataWithAck(oMessage);
    Finally
      oMessage.DoLeaveWrite;
    End;
  Finally
    sgcFree(oMessage);
  End;
end;

procedure TsgcWSProtocol_Presence_Client.GetMembers(const aChannel: String);
var
  oMessage: TsgcWSPresenceMessage;
begin
  oMessage := TsgcWSPresenceMessage.Create(nil);
  Try
    oMessage.DoEnterWrite;
    Try
      oMessage.EncodeBase64 := EncodeBase64;
      oMessage.Method := CS_PRESENCE_GET_MEMBERS;
      oMessage.Member.ID := FSession;
      oMessage.Channel.Name := aChannel;
      DoWriteDataWithAck(oMessage);
    Finally
      oMessage.DoLeaveWrite;
    End;
  Finally
    sgcFree(oMessage);
  End;
end;

function TsgcWSProtocol_Presence_Client.GetWSMessageByConnection(const
    aConnection: TsgcWSConnection): TsgcWSPresenceMessage;
var
  oItem: TsgcQueueItemProtocolMessage;
begin
  Result := nil;

  if Assigned(aConnection) then
  begin
    oItem := GetProtocolDataItem(aConnection, FWSMessageId);
    if not Assigned(oItem) then
    begin
      oItem := TsgcQueueItemProtocolMessage.Create;
      oItem.WSMessage := TsgcWSPresenceMessage.Create(nil);
      TsgcWSPresenceMessage(oItem.WSMessage).EncodeBase64 := EncodeBase64;
      AddProtocolDataItem(aConnection, FWSMessageId, oItem);
    end;

    result := TsgcWSPresenceMessage(oItem.WSMessage);
  end;
end;

procedure TsgcWSProtocol_Presence_Client.Invite(const aChannel, aMemberID:
    String);
var
  oMessage: TsgcWSPresenceMessage;
  oMember: TsgcWSPresenceMember;
begin
  oMessage := TsgcWSPresenceMessage.Create(nil);
  Try
    oMessage.DoEnterWrite;
    Try
      oMessage.EncodeBase64 := EncodeBase64;
      oMessage.Method := CS_PRESENCE_CHANNEL_INVITATION;
      oMessage.Channel.Name := aChannel;
      oMember := TsgcWSPresenceMember.Create;
      oMember.ID := aMemberID;

      oMessage.Members.AddMember(oMember);

      oMessage.Member.Name := Presence.Name;
      oMessage.Member.ID := FSession;
      oMessage.Member.Info := Presence.Info.Text;

      DoWriteDataWithAck(oMessage);
    Finally
      oMessage.DoLeaveWrite;
    End;
  Finally
    sgcFree(oMessage);
  End;
end;

procedure TsgcWSProtocol_Presence_Client.OnAcknowledgmentMessageEvent(
    aConnection: TsgcWSConnection; const aText: String);
begin
  inherited;
  inherited WriteData(aText);
end;

procedure TsgcWSProtocol_Presence_Client.Publish(const aText: String; const
    aChannel: String = '');
var
  oMessage: TsgcWSPresenceMessage;
begin
  oMessage := TsgcWSPresenceMessage.Create(nil);
  Try
    oMessage.DoEnterWrite;
    Try
      oMessage.EncodeBase64 := EncodeBase64;
      oMessage.Method := CS_PRESENCE_PUBLISH_MSG;
      oMessage.Channel.Name := aChannel;
      oMessage.Msg.Text := aText;
      oMessage.Msg.MsgType := mtText;
      oMessage.Member.ID := FSession;

      DoWriteDataWithAck(oMessage);
    Finally
      oMessage.DoLeaveWrite;
    End;
  Finally
    sgcFree(oMessage);
  End;
end;

procedure TsgcWSProtocol_Presence_Client.SetPresence(const Value:
    TsgcWSPresenceMember_Options);
begin
  FPresence.Assign(Value);
end;

procedure TsgcWSProtocol_Presence_Client.Subscribe(const aChannel: String);
var
  oMessage: TsgcWSPresenceMessage;
begin
  oMessage := TsgcWSPresenceMessage.Create(nil);
  Try
    oMessage.DoEnterWrite;
    Try
      oMessage.EncodeBase64 := EncodeBase64;
      oMessage.Method := CS_PRESENCE_NEW_CHANNEL_MEMBER;
      oMessage.Channel.Name := aChannel;
      oMessage.Member.ID := FSession;

      DoWriteDataWithAck(oMessage);
    Finally
      oMessage.DoLeaveWrite;
    End;
  Finally
    sgcFree(oMessage);
  End;
end;

procedure TsgcWSProtocol_Presence_Client.UnSubscribe(const aChannel: String);
var
  oMessage: TsgcWSPresenceMessage;
begin
  oMessage := TsgcWSPresenceMessage.Create(nil);
  Try
    oMessage.DoEnterWrite;
    Try
      oMessage.EncodeBase64 := EncodeBase64;
      oMessage.Method := CS_PRESENCE_REMOVE_CHANNEL_MEMBER;
      oMessage.Channel.Name := aChannel;
      oMessage.Member.ID := FSession;

      DoWriteDataWithAck(oMessage);
    Finally
      oMessage.DoLeaveWrite;
    End;
  Finally
    sgcFree(oMessage);
  End;
end;

constructor TsgcWSPresenceMember_Options.Create;
begin
  inherited;
  FInfo := TStringList.Create;
end;

destructor TsgcWSPresenceMember_Options.Destroy;
begin
  sgcFree(FInfo);
  inherited;
end;

procedure TsgcWSPresenceMember_Options.SetInfo(const Value: TStringList);
begin
  FInfo.Assign(Value);
end;

{$ENDIF}

end.
