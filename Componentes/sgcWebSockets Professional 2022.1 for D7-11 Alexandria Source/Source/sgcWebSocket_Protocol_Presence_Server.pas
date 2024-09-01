{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Protocol_Presence_Server;

interface

{$I sgcVer.inc}
{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils,
{$IFDEF MSWINDOWS}Windows, {$ENDIF}
{$IFDEF NEXTGEN}System.Generics.Collections, {$ELSE}Contnrs, {$ENDIF}
  // sgc
  sgcJSON, sgcWebSocket_Protocol_Presence_Message, sgcWebSocket_Types,
  sgcWebSocket_Classes, sgcWebSocket_Protocol_Base_Server, sgcWebSocket_Helpers,
  sgcWebSocket_HTTPResponse, sgcWebSocket_Classes_SyncObjs;

type

  TsgcWSPresenceBeforeNewMember = procedure(aConnection: TsgcWSConnection;
      aMember: TsgcWSPresenceMember; var Accept: Boolean) of object;
  TsgcWSPresenceBeforeNewChannel = procedure(Connection: TsgcWSConnection;
    const aChannel: TsgcWSPresenceChannel; const aMember: TsgcWSPresenceMember; var Accept: Boolean) of object;
  TsgcWSPresenceNewChannel = procedure(Connection: TsgcWSConnection;
    const aChannel: TsgcWSPresenceChannel) of object;
  TsgcWSPresenceBeforeNewMemberChannel = procedure(Connection: TsgcWSConnection;
    const aChannel: TsgcWSPresenceChannel; const aMember: TsgcWSPresenceMember;
    var Accept: Boolean) of object;
  TsgcWSPresenceBeforePublishMsgEvent = procedure(Connection: TsgcWSConnection;
      const aMsg: TsgcWSPresenceMsg; const aChannel: TsgcWSPresenceChannel; const
      aMember: TsgcWSPresenceMember; var Accept: Boolean) of object;
  TsgcWSPresenceBeforeSendMembersEvent = procedure(Connection: TsgcWSConnection;
      const aChannel: TsgcWSPresenceChannel; const aMember: TsgcWSPresenceMember;
      var Accept: Boolean) of object;


  TsgcWSProtocol_Presence_Server = class(TsgcWSProtocol_Acknowledgment_Server_Base)
    { wsmessage }
  private
    FWSMessageId: String;
  protected
    function GetWSMessageByConnection(aConnection: TsgcWSConnection):
        TsgcWSPresenceMessage;
    { wsmessage }

    { from TsgcWSComponent }
  protected
    procedure DoEventConnect(aConnection: TsgcWSConnection); override;
    procedure DoEventMessage(aConnection: TsgcWSConnection;
      const Text: string); override;
    procedure DoEventDisconnect(aConnection: TsgcWSConnection;
      Code: Integer); override;
    { from TsgcWSComponent }

    { from TsgcWSProtocol }
  protected
    procedure DoInitialize(aConnection: TsgcWSConnection); override;
    procedure DoFinalize(aConnection: TsgcWSConnection); override;
    procedure DoClear(aConnection: TsgcWSConnection); override;
    { from TsgcWSProtocol }

    { from TsgcWSProtocol_Acknowledgment_Server_Base }
  protected
    procedure OnAcknowledgmentMessageEvent(aConnection: TsgcWSConnection; const
        aText: String); override;
    { from TsgcWSProtocol_Acknowledgment_Server_Base }

    { constructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor }

    { channels }
  private
    FChannels: TsgcWSPresenceChannelList;
    FMembers: TsgcWSPresenceMemberList;
    function GetChannels: TsgcWSPresenceChannelList;
    function GetMembers: TsgcWSPresenceMemberList;
  protected
    property Channels: TsgcWSPresenceChannelList read GetChannels;
    property Members: TsgcWSPresenceMemberList read GetMembers write FMembers;
    { channels }

    { procedures }
  private
    function GetErrorDescription(aCode: Integer): string;
  protected
    procedure DoWriteDataWithAck(aConnection: TsgcWSConnection; aMessage:
        TsgcWSPresenceMessage; aClearMessage: Boolean = True);
  protected
    procedure DoProcessAcknowledgment(aID: String);
  protected
    function DoWriteMessageText(aGuid, aMessage: string): Boolean;
  protected
    procedure DoSession(aConnection: TsgcWSConnection); virtual;
    procedure DoAcknowledgment(aConnection: TsgcWSConnection; aID: string); virtual;
  protected
    procedure DoNewMember(aConnection: TsgcWSConnection); virtual;
    procedure DoRemoveMember(aConnection: TsgcWSConnection; aMemberId: String =
        ''); virtual;
  protected
    function DoNewChannel(aConnection: TsgcWSConnection; aChannel: String; aMember:
        TsgcWSPresenceMember): TsgcWSPresenceChannel; virtual;
  protected
    procedure DoNewChannelMember(aConnection: TsgcWSConnection; aChannel: String);
        virtual;
    procedure DoRemoveChannelMember(aConnection: TsgcWSConnection; aChannel:
        String; aMemberId: String = ''); virtual;
    procedure DoRemoveAllChannelsMember(aConnection: TsgcWSConnection; aMember:
        TsgcWSPresenceMember); virtual;
  protected
    procedure DoPublishMsg(aConnection: TsgcWSConnection; aMessage:
        TsgcWSPresenceMessage); virtual;
  protected
    procedure DoSendMembers(aConnection: TsgcWSConnection; aMember:
        TsgcWSPresenceMember; aChannel: TsgcWSPresenceChannel); virtual;
    procedure DoSendMembersChannel(aConnection: TsgcWSConnection; aMember:
        TsgcWSPresenceMember; aChannel: TsgcWSPresenceChannel); virtual;
    procedure DoSendAllMembers(aConnection: TsgcWSConnection; aMember:
        TsgcWSPresenceMember); virtual;
  protected
    procedure DoReceiveChannelInvitation(aConnection: TsgcWSConnection; aMember:
        TsgcWSPresenceMember; aChannel: TsgcWSPresenceChannel; aMembers:
        TsgcWSPresenceMemberList); virtual;
    procedure DoSendChannelInvitation(aConnection: TsgcWSConnection; aMember:
        TsgcWSPresenceMember; aChannel: TsgcWSPresenceChannel);
  protected
    procedure DoBroadcastMessage(aMessage: TsgcWSPresenceMessage; aChannel:
        TsgcWSPresenceChannel = nil); virtual;
  protected
    procedure DoErrorMemberChannel(aConnection: TsgcWSConnection; aCode: Integer;
        aChannel: TsgcWSPresenceChannel = nil; aMember: TsgcWSPresenceMember =
        nil); virtual;
    procedure DoErrorPublishMsg(aConnection: TsgcWSConnection; aCode: Integer;
        aMsg: TsgcWSPresenceMsg; aChannel: TsgcWSPresenceChannel = nil; aMember:
        TsgcWSPresenceMember = nil);
    { procedures }

    { properties }
  private
    FEncodeBase64: Boolean;
  public
    property EncodeBase64: Boolean read FEncodeBase64 write FEncodeBase64;
    { properties }

    { events }
  private
    FOnBeforeNewChannel: TsgcWSPresenceBeforeNewChannel;
    FOnBeforeNewChannelMember: TsgcWSPresenceBeforeNewMemberChannel;
    FOnBeforeNewMember: TsgcWSPresenceBeforeNewMember;
    FOnBeforePublishMsg: TsgcWSPresenceBeforePublishMsgEvent;
    FOnBeforeSendMembers: TsgcWSPresenceBeforeSendMembersEvent;
    FOnErrorMemberChannel: TsgcWSPresenceMemberChannelErrorEvent;
    FOnErrorPublishMsg: TsgcWSPresencePublishMsgErrorEvent;
    FOnNewChannel: TsgcWSPresenceNewChannel;
    FOnNewChannelMember: TsgcWSPresenceNewMemberChannelEvent;
    FOnNewMember: TsgcWSPresenceNewMemberEvent;
    FOnRemoveChannelMember: TsgcWSPresenceRemoveMemberChannelEvent;
    FOnRemoveMember: TsgcWSPresenceRemoveMemberEvent;
  protected
    function DoBeforeNewMemberEvent(aConnection: TsgcWSConnection; aMember:
        TsgcWSPresenceMember): Boolean; virtual;
    function DoBeforeNewChannelMemberEvent(aConnection: TsgcWSConnection; aChannel:
        TsgcWSPresenceChannel; aMember: TsgcWSPresenceMember): Boolean; virtual;
    function DoBeforeNewChannelEvent(Connection: TsgcWSConnection; aChannel:
        TsgcWSPresenceChannel; aMember: TsgcWSPresenceMember): Boolean; virtual;
    function DoBeforeSendMembersEvent(Connection: TsgcWSConnection; aChannel:
        TsgcWSPresenceChannel; aMember: TsgcWSPresenceMember): Boolean; virtual;
    procedure DoNewChannelEvent(Connection: TsgcWSConnection; var aChannel:
        TsgcWSPresenceChannel); virtual;
    procedure DoNewChannelMemberEvent(aConnection: TsgcWSConnection; aChannel:
        TsgcWSPresenceChannel; aMember: TsgcWSPresenceMember); virtual;
    procedure DoNewMemberEvent(aConnection: TsgcWSConnection; aMember:
        TsgcWSPresenceMember); virtual;
    procedure DoRemoveChannelMemberEvent(aConnection: TsgcWSConnection; aChannel:
        TsgcWSPresenceChannel; aMember: TsgcWSPresenceMember); virtual;
    procedure DoRemoveMemberEvent(aConnection: TsgcWSConnection; aMember:
        TsgcWSPresenceMember); virtual;
    function DoBeforePublishMsgEvent(aConnection: TsgcWSConnection; aMsg:
        TsgcWSPresenceMsg; aChannel: TsgcWSPresenceChannel; aMember:
        TsgcWSPresenceMember): Boolean; virtual;
    procedure DoErrorMemberChannelEvent(aConnection: TsgcWSConnection; aError:
        TsgcWSPresenceError; aChannel: TsgcWSPresenceChannel; aMember:
        TsgcWSPresenceMember); virtual;
    procedure DoErrorPublishMsgEvent(aConnection: TsgcWSConnection; aError:
        TsgcWSPresenceError; aMsg: TsgcWSPresenceMsg; aChannel:
        TsgcWSPresenceChannel; aMember: TsgcWSPresenceMember); virtual;
  public
    property OnBeforeNewChannelMember: TsgcWSPresenceBeforeNewMemberChannel read
        FOnBeforeNewChannelMember write FOnBeforeNewChannelMember;
    property OnBeforeNewMember: TsgcWSPresenceBeforeNewMember read
        FOnBeforeNewMember write FOnBeforeNewMember;
    property OnBeforePublishMsg: TsgcWSPresenceBeforePublishMsgEvent read
        FOnBeforePublishMsg write FOnBeforePublishMsg;
    property OnBeforeNewChannel: TsgcWSPresenceBeforeNewChannel read
        FOnBeforeNewChannel write FOnBeforeNewChannel;
    property OnBeforeSendMembers: TsgcWSPresenceBeforeSendMembersEvent read
        FOnBeforeSendMembers write FOnBeforeSendMembers;
    property OnErrorMemberChannel: TsgcWSPresenceMemberChannelErrorEvent read
        FOnErrorMemberChannel write FOnErrorMemberChannel;
    property OnErrorPublishMsg: TsgcWSPresencePublishMsgErrorEvent read
        FOnErrorPublishMsg write FOnErrorPublishMsg;
    property OnNewChannel: TsgcWSPresenceNewChannel read FOnNewChannel write
        FOnNewChannel;
    property OnNewChannelMember: TsgcWSPresenceNewMemberChannelEvent read
        FOnNewChannelMember write FOnNewChannelMember;
    property OnNewMember: TsgcWSPresenceNewMemberEvent read FOnNewMember write
        FOnNewMember;
    property OnRemoveChannelMember: TsgcWSPresenceRemoveMemberChannelEvent read
        FOnRemoveChannelMember write FOnRemoveChannelMember;
    property OnRemoveMember: TsgcWSPresenceRemoveMemberEvent read FOnRemoveMember
        write FOnRemoveMember;
    { events }
  end;

  TsgcWSProtocol_JS_Response_Presence = class(TsgcWSHTTPResponse_Base)
  protected
    function GetResponse: string; override;
  public
    class function GetFileName: string; override;
  end;

  TsgcWSProtocol_HTML_Response_Presence = class(TsgcWSHTTPResponse_Base)
  protected
    function GetResponse: string; override;
  public
    class function GetFileName: string; override;
  end;

{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses
  sgcWebSocket_Const, sgcWebSocket_Resources, sgcWebSocket_Server,
  sgcWebSocket_Protocol_Base_Message, sgcBase_Helpers;

Const
  CS_MEMBER_ALREADY_EXISTS = 1;
  CS_NEW_MEMBER_DENIED = 2;
  CS_MEMBER_NOT_EXISTS = 3;

  CS_CHANNEL_NOT_EXISTS = 5;
  CS_NEW_CHANNEL_DENIED = 6;

  CS_NEW_SUBSCRIPTON_DENIED = 10;
  CS_SUBSCRIPTON_ALREADY_EXISTS = 11;
  CS_SUBSCRIPTION_NOT_EXISTS = 12;

  CS_PUBLISH_MSG_DENIED = 20;

  CS_SEND_MEMBERS_DENIED = 30;

resourcestring
  S_MEMBER_ALREADY_EXISTS = 'Member already exists.';
  S_NEW_MEMBER_DENIED = 'Create New Member request has been denied.';
  S_MEMBER_NOT_EXISTS = 'Member not exists.';

  S_CHANNEL_NOT_EXISTS = 'Channel not exists';
  S_NEW_CHANNEL_DENIED = 'Create New Channel request has been denied.';

  S_NEW_SUBSCRIPTON_DENIED = 'Create Subscription request has been denied.';
  S_SUBSCRIPTON_ALREADY_EXISTS = 'Subscription already exists.';
  S_SUBSCRIPTION_NOT_EXISTS = 'Subscription not exists.';

  S_PUBLISH_MSG_DENIED = 'Publish Message request has been denied.';

  S_SEND_MEMBERS_DENIED = 'Send Members request has been denied.';

const
  CS_QOS_LEVEL2_LIST = 1;

constructor TsgcWSProtocol_Presence_Server.Create(aOwner: TComponent);
begin
  inherited;
  FProtocol := CS_PROTOCOL_PRESENCE;
  FWSMessageId := NewGuid;
  FEncodeBase64 := False;
end;

destructor TsgcWSProtocol_Presence_Server.Destroy;
begin
  sgcFree(FMembers);
  sgcFree(FChannels);
  inherited;
end;

procedure TsgcWSProtocol_Presence_Server.DoEventConnect
  (aConnection: TsgcWSConnection);
begin
  // ... send session id
  DoSession(aConnection);
  // ... notify connection only if not Assigned Broker
  if not Assigned(Broker) then
    inherited;
end;

procedure TsgcWSProtocol_Presence_Server.DoEventMessage
  (aConnection: TsgcWSConnection; const Text: string);
var
  oMessage: TsgcWSPresenceMessage;
begin
{$IFDEF SGC_DEBUG}
  DoLog(self, aConnection, 'DoEventMessage', '[Message]: ' + Text);
{$ENDIF}
  if DoRawMessage(aConnection, Text) then
    exit;

  oMessage := GetWSMessageByConnection(aConnection);
  oMessage.Read(Text);

  // ...acknowledgment
  if (oMessage.id <> '') and (oMessage.method <> CS_PRESENCE_ACKNOWLEDGMENT) then
    DoAcknowledgment(aConnection, oMessage.id);

  // ... methods
  if oMessage.method = CS_PRESENCE_NEW_MEMBER then
    DoNewMember(aConnection)
  else if oMessage.Method = CS_PRESENCE_REMOVE_MEMBER then
    DoRemoveMember(aConnection, oMessage.Member.ID)
  else if oMessage.method = CS_PRESENCE_NEW_CHANNEL_MEMBER then
    DoNewChannelMember(aConnection, oMessage.Channel.Name)
  else if oMessage.method = CS_PRESENCE_REMOVE_CHANNEL_MEMBER then
    DoRemoveChannelMember(aConnection, oMessage.Channel.Name, oMessage.Member.ID)
  else if oMessage.method = CS_PRESENCE_PUBLISH_MSG then
    DoPublishMsg(aConnection, oMessage)
  else if oMessage.method = CS_PRESENCE_ACKNOWLEDGMENT then
    DoProcessAcknowledgment(oMessage.id)
  else if oMessage.method = CS_PRESENCE_GET_MEMBERS then
    DoSendMembers(aConnection, oMessage.Member, oMessage.Channel)
  else if oMessage.method = CS_PRESENCE_CHANNEL_INVITATION then
    DoReceiveChannelInvitation(aConnection, oMessage.Member, oMessage.Channel, oMessage.Members);
end;

procedure TsgcWSProtocol_Presence_Server.DoSession(aConnection:
    TsgcWSConnection);
var
  oMessage: TsgcWSPresenceMessage;
begin
  oMessage := TsgcWSPresenceMessage.Create(nil);
  Try
    oMessage.DoEnterWrite(True);
    Try
      oMessage.EncodeBase64 := EncodeBase64;
      // ... session id
      oMessage.method := CS_PRESENCE_SESSION;
      oMessage.Msg.Text := aConnection.Guid;

      DoWriteDataWithAck(aConnection, oMessage);
    Finally
      oMessage.DoLeaveWrite;
    End;
  Finally
    sgcFree(oMessage);
  End;
end;

function TsgcWSProtocol_JS_Response_Presence.GetResponse: string;
begin
  Result := GetResourceString(CS_SGC_JS_PRESENCE_ESEGECE_COM);
end;

class function TsgcWSProtocol_JS_Response_Presence.GetFileName: string;
begin
  Result := CS_JS_PRESENCE_ESEGECE_COM;
end;

class function TsgcWSProtocol_HTML_Response_Presence.GetFileName: string;
begin
  Result := CS_HTML_PRESENCE_ESEGECE_COM;
end;

function TsgcWSProtocol_HTML_Response_Presence.GetResponse: string;
begin
  Result := GetResourceString(CS_SGC_HTML_PRESENCE_ESEGECE_COM);
end;

procedure TsgcWSProtocol_Presence_Server.DoAcknowledgment(aConnection:
    TsgcWSConnection; aID: string);
var
  oJSON: IsgcJSON;
begin
  oJSON := GetJSONInstance;
  Try
    oJSON.AddPair(CS_JSON_RPC, CS_JSONRPC_VERSION);
    oJSON.AddPair(CS_METHOD, CS_PRESENCE_ACKNOWLEDGMENT);
    oJSON.AddPair(CS_ID, aID);

    WriteData(aConnection, oJSON.Text);
  Finally
    FreeJSONInstance(oJSON);
  End;
end;

function TsgcWSProtocol_Presence_Server.DoBeforeNewChannelEvent(Connection:
    TsgcWSConnection; aChannel: TsgcWSPresenceChannel; aMember:
    TsgcWSPresenceMember): Boolean;
begin
  result := True;
  if Assigned(FOnBeforeNewChannel) then
    FOnBeforeNewChannel(Connection, aChannel, aMember, result);
end;

function TsgcWSProtocol_Presence_Server.DoBeforeNewChannelMemberEvent(
    aConnection: TsgcWSConnection; aChannel: TsgcWSPresenceChannel; aMember:
    TsgcWSPresenceMember): Boolean;
begin
  result := True;

  if Assigned(FOnBeforeNewChannelMember) then
    FOnBeforeNewChannelMember(aConnection, aChannel, aMember, result);
end;

function TsgcWSProtocol_Presence_Server.DoBeforeNewMemberEvent(aConnection:
    TsgcWSConnection; aMember: TsgcWSPresenceMember): Boolean;
begin
  result := True;

  if Assigned(FOnBeforeNewMember) then
    FOnBeforeNewMember(aConnection, aMember, result);
end;

function TsgcWSProtocol_Presence_Server.DoBeforePublishMsgEvent(aConnection:
    TsgcWSConnection; aMsg: TsgcWSPresenceMsg; aChannel: TsgcWSPresenceChannel;
    aMember: TsgcWSPresenceMember): Boolean;
begin
  Result := True;
  if Assigned(FOnBeforePublishMsg) then
    FOnBeforePublishMsg(aConnection, aMsg, aChannel, aMember, Result);
end;

function TsgcWSProtocol_Presence_Server.DoBeforeSendMembersEvent(Connection:
    TsgcWSConnection; aChannel: TsgcWSPresenceChannel; aMember:
    TsgcWSPresenceMember): Boolean;
begin
  Result := True;
  if Assigned(FOnBeforeSendMembers) then
    FOnBeforeSendMembers(Connection, aChannel, aMember, Result);
end;

procedure TsgcWSProtocol_Presence_Server.DoBroadcastMessage(aMessage:
    TsgcWSPresenceMessage; aChannel: TsgcWSPresenceChannel = nil);
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TsgcWSPresenceMember>{$ENDIF};
  oConnection: TsgcWSConnection;
begin
  if Assigned(aChannel) then
    oList := aChannel.Members.LockList
  else
    oList := Members.LockList;
  Try
    aMessage.Broadcast := True;
    Try
      for i := 0 to oList.Count - 1 do
      begin
        oConnection := TsgcWSPresenceMember(oList[i]).Connection;
        if (oConnection.Disconnected = False) and (oConnection.CloseCode = 0) then
          DoWriteDataWithAck(oConnection, aMessage, False);
      end;
    Finally
      aMessage.Broadcast := False;
    End;
  Finally
    if Assigned(aChannel) then
      aChannel.Members.UnlockList
    else
      Members.UnlockList;
  End;
end;

procedure TsgcWSProtocol_Presence_Server.DoReceiveChannelInvitation(
    aConnection: TsgcWSConnection; aMember: TsgcWSPresenceMember; aChannel:
    TsgcWSPresenceChannel; aMembers: TsgcWSPresenceMemberList);
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TsgcWSPresenceMember>{$ENDIF};
  oConnection: TsgcWSConnection;
  oMember: TsgcWSPresenceMember;
begin
  oList := aMembers.LockList;
  Try
    for i := 0 to oList.Count - 1 do
    begin
      oMember := Members.GetMember(TsgcWSPresenceMember(oList[i]).ID);
      if Assigned(oMember) then
      begin
        oConnection := oMember.Connection;
        if Assigned(oConnection) then
          DoSendChannelInvitation(oConnection, aMember, aChannel);
      end;
    end;
  Finally
    aMembers.UnlockList;
  End;
end;

procedure TsgcWSProtocol_Presence_Server.DoClear(aConnection: TsgcWSConnection);
begin
  // ... remove member
  DoRemoveMember(aConnection);
  inherited;
end;

procedure TsgcWSProtocol_Presence_Server.DoErrorMemberChannel(aConnection:
    TsgcWSConnection; aCode: Integer; aChannel: TsgcWSPresenceChannel = nil;
    aMember: TsgcWSPresenceMember = nil);
var
  oMessage: TsgcWSPresenceMessage;
begin
  oMessage := TsgcWSPresenceMessage.Create(nil);
  Try
    oMessage.DoEnterWrite(True);
    Try
      oMessage.EncodeBase64 := EncodeBase64;
      // ... session id
      oMessage.method := CS_PRESENCE_ERROR_MEMBER_CHANNEL;
      if Assigned(aMember) then
        oMessage.Member.Assign(aMember);
      if Assigned(aChannel) then
        oMessage.Channel.Assign(aChannel);
      oMessage.Error.Code := aCode;
      oMessage.Error.Text := GetErrorDescription(aCode);

      // ... call event
      DoErrorMemberChannelEvent(aConnection, oMessage.Error, aChannel, aMember);

      // ... send message to client
      DoWriteDataWithAck(aConnection, oMessage);
    Finally
      oMessage.DoLeaveWrite;
    End;
  Finally
    sgcFree(oMessage);
  End;
end;

procedure TsgcWSProtocol_Presence_Server.DoErrorMemberChannelEvent(aConnection:
    TsgcWSConnection; aError: TsgcWSPresenceError; aChannel:
    TsgcWSPresenceChannel; aMember: TsgcWSPresenceMember);
begin
  if Assigned(FOnErrorMemberChannel) then
    FOnErrorMemberChannel(aConnection, aError, aChannel, aMember);
end;

procedure TsgcWSProtocol_Presence_Server.DoErrorPublishMsg(aConnection:
    TsgcWSConnection; aCode: Integer; aMsg: TsgcWSPresenceMsg; aChannel:
    TsgcWSPresenceChannel = nil; aMember: TsgcWSPresenceMember = nil);
var
  oMessage: TsgcWSPresenceMessage;
begin
  oMessage := TsgcWSPresenceMessage.Create(nil);
  Try
    oMessage.DoEnterWrite(True);
    Try
      oMessage.EncodeBase64 := EncodeBase64;
      // ... session id
      oMessage.method := CS_PRESENCE_ERROR_MEMBER_CHANNEL;
      if Assigned(aMember) then
        oMessage.Member.Assign(aMember);
      if Assigned(aChannel) then
        oMessage.Channel.Assign(aChannel);
      if Assigned(aMsg) then
        oMessage.Msg.Assign(aMsg);
      oMessage.Error.Code := aCode;
      oMessage.Error.Text := GetErrorDescription(aCode);

      // ... call event
      DoErrorPublishMsgEvent(aConnection, oMessage.Error, aMsg, aChannel, aMember);

      // ... send message to client
      DoWriteDataWithAck(aConnection, oMessage);
    Finally
      oMessage.DoLeaveWrite;
    End;
  Finally
    sgcFree(oMessage);
  End;
end;

procedure TsgcWSProtocol_Presence_Server.DoErrorPublishMsgEvent(aConnection:
    TsgcWSConnection; aError: TsgcWSPresenceError; aMsg: TsgcWSPresenceMsg;
    aChannel: TsgcWSPresenceChannel; aMember: TsgcWSPresenceMember);
begin
  if Assigned(FOnErrorPublishMsg) then
    FOnErrorPublishMsg(aConnection, aError, aMsg, aChannel, aMember);
end;

procedure TsgcWSProtocol_Presence_Server.DoEventDisconnect
  (aConnection: TsgcWSConnection; Code: Integer);
begin
  // ... Unsubscribe
  DoRemoveMember(aConnection);
  inherited;
end;

procedure TsgcWSProtocol_Presence_Server.DoFinalize(aConnection:
    TsgcWSConnection);
begin
  inherited;
  // ... Unsubscribe
  DoRemoveMember(aConnection);
end;

procedure TsgcWSProtocol_Presence_Server.DoInitialize(aConnection:
    TsgcWSConnection);
begin
  inherited;
  // ... send session id
  DoSession(aConnection);
end;

function TsgcWSProtocol_Presence_Server.DoNewChannel(aConnection:
    TsgcWSConnection; aChannel: String; aMember: TsgcWSPresenceMember):
    TsgcWSPresenceChannel;
var
  oChannel: TsgcWSPresenceChannel;

begin
  result := nil;

  oChannel := TsgcWSPresenceChannel.Create;
  oChannel.Name := aChannel;
  if DoBeforeNewChannelEvent(aConnection, oChannel, aMember) then
  begin
    Channels.AddChannel(oChannel);
    DoNewChannelEvent(aConnection, oChannel);
    result := oChannel;
  end
  else
  begin
    DoErrorMemberChannel(aConnection, CS_NEW_CHANNEL_DENIED, oChannel, aMember);
    sgcFree(oChannel);
  end;
end;

procedure TsgcWSProtocol_Presence_Server.DoNewChannelEvent(Connection:
    TsgcWSConnection; var aChannel: TsgcWSPresenceChannel);
begin
  if Assigned(FOnNewChannel) then
    FOnNewChannel(Connection, aChannel);
end;

procedure TsgcWSProtocol_Presence_Server.DoNewChannelMember(aConnection:
    TsgcWSConnection; aChannel: String);
var
  oMember: TsgcWSPresenceMember;
  oMessage: TsgcWSPresenceMessage;
  oChannel: TsgcWSPresenceChannel;
begin
  oMessage := GetWSMessageByConnection(aConnection);
  // ... get member
  oMember := Members.GetMember(oMessage.Member.ID);
  if Assigned(oMember) then
  begin
    // ... copy member values
    oMessage.Member.Assign(oMember);
    // ... get channel
    oChannel := Channels.GetChannel(aChannel);
    if not Assigned(oChannel) then
      oChannel := DoNewChannel(aConnection, aChannel, oMember);
    if Assigned(oChannel) then
    begin
      // ... check if already exists subscription
      if  oChannel.Members.GetMember(oMember.ID) = nil then
      begin
        // ... ask if a new member can be added
        if DoBeforeNewChannelMemberEvent(aConnection, oChannel, oMember) then
        begin
          oChannel.Members.AddMember(oMember);

          DoNewChannelMemberEvent(aConnection, oChannel, oMember);
          DoBroadcastMessage(oMessage, oChannel);
        end
        else
        begin
          DoErrorMemberChannel(aConnection, CS_NEW_SUBSCRIPTON_DENIED, oChannel, oMember);
          sgcFree(oMember);
        end;
      end
      else
        DoErrorMemberChannel(aConnection, CS_SUBSCRIPTON_ALREADY_EXISTS, oChannel, oMember);
    end;
  end
  else
    DoErrorMemberChannel(aConnection, CS_MEMBER_NOT_EXISTS, nil, oMember);
end;

procedure TsgcWSProtocol_Presence_Server.DoNewChannelMemberEvent(aConnection:
    TsgcWSConnection; aChannel: TsgcWSPresenceChannel; aMember:
    TsgcWSPresenceMember);
begin
  if Assigned(FOnNewChannelMember) then
    FOnNewChannelMember(aConnection, aChannel, aMember);
end;

procedure TsgcWSProtocol_Presence_Server.DoNewMember(aConnection:
    TsgcWSConnection);
var
  oMember: TsgcWSPresenceMember;
  oMessage: TsgcWSPresenceMessage;
begin
  oMessage := GetWSMessageByConnection(aConnection);
  oMember := Members.GetMember(oMessage.Member.ID);
  if not Assigned(oMember) then
  begin
    oMember := TsgcWSPresenceMember.Create;
    oMessage.Member.Connection := aConnection;
    oMember.Assign(oMessage.Member);

    if DoBeforeNewMemberEvent(aConnection, oMember) then
    begin
      Members.AddMember(oMember);
      DoNewMemberEvent(aConnection, oMember);
      DoBroadcastMessage(oMessage);
    end
    else
    begin
      DoErrorMemberChannel(aConnection, CS_NEW_MEMBER_DENIED, nil, oMember);
      sgcFree(oMember);
    end;
  end
  else
    DoErrorMemberChannel(aConnection, CS_MEMBER_ALREADY_EXISTS, nil, oMember);
end;

procedure TsgcWSProtocol_Presence_Server.DoNewMemberEvent(aConnection:
    TsgcWSConnection; aMember: TsgcWSPresenceMember);
begin
  if Assigned(FOnNewMember) then
    FOnNewMember(aConnection, aMember);
end;

procedure TsgcWSProtocol_Presence_Server.DoProcessAcknowledgment(aID: String);
begin
  AcknowledgmentList.Delete(aID);
end;

procedure TsgcWSProtocol_Presence_Server.DoPublishMsg(aConnection:
    TsgcWSConnection; aMessage: TsgcWSPresenceMessage);
var
  oMember: TsgcWSPresenceMember;
  oChannel: TsgcWSPresenceChannel;
  vId, vChannel: String;
begin
  vId := aMessage.Member.ID;
  vChannel := aMessage.Channel.Name;
  oMember := Members.GetMember(vId);
  if Assigned(oMember) then
  begin
    if vChannel <> '' then
    begin
      oChannel := Channels.GetChannel(vChannel);
      if Assigned(oChannel) then
      begin
        oMember := oChannel.Members.GetMember(vId);
        if Assigned(oMember) then
        begin
          aMessage.Member.Assign(oMember);
          aMessage.Channel.Assign(oChannel);
          if DoBeforePublishMsgEvent(aConnection, aMessage.Msg, oChannel, aMessage.Member) then
            DoBroadcastMessage(aMessage, oChannel)
          else
            DoErrorPublishMsg(aConnection, CS_PUBLISH_MSG_DENIED, aMessage.Msg, oChannel, oMember);
        end
        else
        begin
          oMember := Members.GetMember(vId);
          DoErrorPublishMsg(aConnection, CS_SUBSCRIPTION_NOT_EXISTS, aMessage.Msg, oChannel, oMember);
        end;
      end
      else
        DoErrorPublishMsg(aConnection, CS_CHANNEL_NOT_EXISTS, aMessage.Msg, nil, oMember);
    end
    else
    begin
      if DoBeforePublishMsgEvent(aConnection, aMessage.Msg, nil, aMessage.Member) then
        DoBroadcastMessage(aMessage)
    end;
  end
  else
    DoErrorPublishMsg(aConnection, CS_MEMBER_NOT_EXISTS, aMessage.Msg, nil, nil);
end;

procedure TsgcWSProtocol_Presence_Server.DoRemoveAllChannelsMember(aConnection:
    TsgcWSConnection; aMember: TsgcWSPresenceMember);
var
  i: Integer;
  oMessage: TsgcWSPresenceMessage;
  oList: TList{$IFDEF NEXTGEN}<TsgcWSPresenceChannel>{$ENDIF};
begin
  oList := Channels.LockList;
  Try
    for i := 0 to oList.Count - 1 do
    begin
      if TsgcWSPresenceChannel(oList[i]).Members.GetMember(aMember.ID) <> nil then
      begin
        // ... remove member from channel
        TsgcWSPresenceChannel(oList[i]).Members.DeleteMember(aMember.ID);
        // ... broadcast message all subscribers
        oMessage := TsgcWSPresenceMessage.Create(nil);
        Try
          oMessage.EncodeBase64 := EncodeBase64;
          oMessage.DoEnterWrite;
          oMessage.method := CS_PRESENCE_REMOVE_CHANNEL_MEMBER;
          oMessage.Member.Assign(aMember);
          oMessage.Channel.Assign(TsgcWSPresenceChannel(oList[i]));
          DoBroadcastMessage(oMessage, TsgcWSPresenceChannel(oList[i]));
        Finally
          sgcFree(oMessage);
        End;
      end;
    end;
  Finally
    Channels.UnlockList;
  End;
end;

procedure TsgcWSProtocol_Presence_Server.DoRemoveChannelMember(aConnection:
    TsgcWSConnection; aChannel: String; aMemberId: String = '');
var
  oChannel: TsgcWSPresenceChannel;
  oMember: TsgcWSPresenceMember;
  oMessage: TsgcWSPresenceMessage;
  vId: string;
begin
  if Assigned(aConnection) then
  begin
    if aMemberId <> '' then
      vId := aMemberId
    else
      vId := aConnection.Guid;
    oChannel := Channels.GetChannel(aChannel);
    if Assigned(oChannel) then
    begin
      oMember := oChannel.Members.GetMember(vId);
      if Assigned(oMember) then
      begin
        // ... remove member event
        DoRemoveChannelMemberEvent(aConnection, oChannel, oMember);
        // ... broadcast remove member
        oMessage := TsgcWSPresenceMessage.Create(nil);
        Try
          oMessage.EncodeBase64 := EncodeBase64;
          oMessage.DoEnterWrite;
          oMessage.method := CS_PRESENCE_REMOVE_CHANNEL_MEMBER;
          oMessage.Member.Assign(oMember);
          oMessage.Channel.Assign(oChannel);
          // ... remove member
          oChannel.Members.DeleteMember(vId);
          // ... broadcast
          DoBroadcastMessage(oMessage);
        Finally
          sgcFree(oMessage);
        End;
      end
      else
      begin
        oMember := Members.GetMember(vId);
        DoErrorMemberChannel(aConnection, CS_SUBSCRIPTION_NOT_EXISTS, oChannel, oMember);
      end;
    end
    else
    begin
      oChannel := TsgcWSPresenceChannel.Create;
      Try
        oChannel.Name := aChannel;
        oMember := oChannel.Members.GetMember(vId);
        DoErrorMemberChannel(aConnection, CS_CHANNEL_NOT_EXISTS, oChannel, oMember);
      Finally
        sgcFree(oChannel);
      End;
    end;
  end;
end;

procedure TsgcWSProtocol_Presence_Server.DoRemoveChannelMemberEvent(
    aConnection: TsgcWSConnection; aChannel: TsgcWSPresenceChannel; aMember:
    TsgcWSPresenceMember);
begin
  if Assigned(FOnRemoveChannelMember) then
    FOnRemoveChannelMember(aConnection, aChannel, aMember);
end;

procedure TsgcWSProtocol_Presence_Server.DoRemoveMember(aConnection:
    TsgcWSConnection; aMemberId: String = '');
var
  oMember: TsgcWSPresenceMember;
  oMessage: TsgcWSPresenceMessage;
  vId: string;
begin
  if Assigned(aConnection) then
  begin
    if aMemberId <> '' then
      vId := aMemberId
    else
      vId := aConnection.Guid;
    oMember := Members.GetMember(vId);
    if Assigned(oMember) then
    begin
      // ... remove member event
      DoRemoveMemberEvent(aConnection, oMember);
      // ... broadcast remove member
      oMessage := TsgcWSPresenceMessage.Create(nil);
      Try
        oMessage.EncodeBase64 := EncodeBase64;
        oMessage.DoEnterWrite;
        oMessage.method := CS_PRESENCE_REMOVE_MEMBER;
        oMessage.Member.Assign(oMember);
        // ... remove member
        DoRemoveAllChannelsMember(aConnection, oMember);
        Members.DeleteMember(vId);
        // ... broadcast
        DoBroadcastMessage(oMessage);
      Finally
        sgcFree(oMessage);
      End;
    end
    else if (aMemberId <> '') then // only raise exception if deleting a member id, not if disconnected
      DoErrorMemberChannel(aConnection, CS_MEMBER_NOT_EXISTS, nil, oMember);
  end;
end;

procedure TsgcWSProtocol_Presence_Server.DoRemoveMemberEvent(aConnection:
    TsgcWSConnection; aMember: TsgcWSPresenceMember);
begin
  if Assigned(FOnRemoveMember) then
    FOnRemoveMember(aConnection, aMember);
end;

procedure TsgcWSProtocol_Presence_Server.DoSendAllMembers(aConnection:
    TsgcWSConnection; aMember: TsgcWSPresenceMember);
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TsgcWSPresenceMember>{$ENDIF};
  oMessage: TsgcWSPresenceMessage;
begin
  oList := Members.LockList;
  Try
    oMessage := TsgcWSPresenceMessage.Create(nil);
    Try
      oMessage.EncodeBase64 := EncodeBase64;
      oMessage.DoEnterWrite;
      oMessage.method := CS_PRESENCE_SEND_MEMBERS;
      oMessage.Member.Assign(aMember);
      for i := 0 to oList.Count - 1 do
        oMessage.Members.AddMember(TsgcWSPresenceMember(oList[i]));
      DoWriteDataWithAck(aConnection, oMessage);
    Finally
      sgcFree(oMessage);
    End;
  Finally
    Members.UnlockList;
  End;
end;

procedure TsgcWSProtocol_Presence_Server.DoSendChannelInvitation(aConnection:
    TsgcWSConnection; aMember: TsgcWSPresenceMember; aChannel:
    TsgcWSPresenceChannel);
var
  oMessage: TsgcWSPresenceMessage;
begin
  oMessage := TsgcWSPresenceMessage.Create(nil);
  Try
    oMessage.EncodeBase64 := EncodeBase64;
    oMessage.DoEnterWrite;
    oMessage.method := CS_PRESENCE_CHANNEL_INVITATION;
    oMessage.Member.Assign(aMember);
    oMessage.Channel.Assign(aChannel);
    DoWriteDataWithAck(aConnection, oMessage);
  Finally
    sgcFree(oMessage);
  End;
end;

procedure TsgcWSProtocol_Presence_Server.DoSendMembers(aConnection:
    TsgcWSConnection; aMember: TsgcWSPresenceMember; aChannel:
    TsgcWSPresenceChannel);
var
  oChannel: TsgcWSPresenceChannel;
  oMember: TsgcWSPresenceMember;
begin
  oMember := Members.GetMember(aMember.ID);
  if Assigned(oMember) then
  begin
    if DoBeforeSendMembersEvent(aConnection, aChannel, oMember) then
    begin
      if aChannel.Name <> '' then
      begin
        oChannel := Channels.GetChannel(aChannel.Name);
        if Assigned(oChannel) then
          DoSendMembersChannel(aConnection, oMember, oChannel)
        else
          DoErrorMemberChannel(aConnection, CS_CHANNEL_NOT_EXISTS, aChannel, oMember)
      end
      else
        DoSendAllMembers(aConnection, oMember)
    end
    else
      DoErrorMemberChannel(aConnection, CS_SEND_MEMBERS_DENIED, aChannel, oMember);
  end
  else
    DoErrorMemberChannel(aConnection, CS_MEMBER_NOT_EXISTS, nil, aMember);
end;

procedure TsgcWSProtocol_Presence_Server.DoSendMembersChannel(aConnection:
    TsgcWSConnection; aMember: TsgcWSPresenceMember; aChannel:
    TsgcWSPresenceChannel);
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TsgcWSPresenceMember>{$ENDIF};
  oMessage: TsgcWSPresenceMessage;
begin
  oList := aChannel.Members.LockList;
  Try
    oMessage := TsgcWSPresenceMessage.Create(nil);
    Try
      oMessage.EncodeBase64 := EncodeBase64;
      oMessage.DoEnterWrite;
      oMessage.method := CS_PRESENCE_SEND_MEMBERS;
      oMessage.Member.Assign(aMember);
      oMessage.Channel.Assign(aChannel);
      for i := 0 to oList.Count - 1 do
        oMessage.Members.AddMember(TsgcWSPresenceMember(oList[i]));
      DoWriteDataWithAck(aConnection, oMessage);
    Finally
      sgcFree(oMessage);
    End;
  Finally
    aChannel.Members.UnlockList;
  End;
end;

procedure TsgcWSProtocol_Presence_Server.DoWriteDataWithAck(aConnection:
    TsgcWSConnection; aMessage: TsgcWSPresenceMessage; aClearMessage: Boolean =
    True);
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
    oMessage.Connection := aConnection;
    oMessage.Text := vText;
    AcknowledgmentList.Add(oMessage);
  end;

  // ... writedata to client
  WriteData(aConnection, vText);
end;

function TsgcWSProtocol_Presence_Server.DoWriteMessageText(aGuid, aMessage:
    string): Boolean;
begin
  Result := inherited WriteData(aGuid, aMessage);
end;

function TsgcWSProtocol_Presence_Server.GetChannels: TsgcWSPresenceChannelList;
begin
  if not Assigned(FChannels) then
  begin
    FChannels := TsgcWSPresenceChannelList.Create;
    FChannels.ListType := tdlThreadDictionary;
  end;
  Result := FChannels;
end;

function TsgcWSProtocol_Presence_Server.GetErrorDescription(aCode: Integer):
    string;
begin
  result := '';
  case aCode of
    CS_MEMBER_ALREADY_EXISTS: result := S_MEMBER_ALREADY_EXISTS;
    CS_NEW_MEMBER_DENIED: result := S_NEW_MEMBER_DENIED;
    CS_MEMBER_NOT_EXISTS: result := S_MEMBER_NOT_EXISTS;
    CS_CHANNEL_NOT_EXISTS: result := S_CHANNEL_NOT_EXISTS;
    CS_NEW_SUBSCRIPTON_DENIED: result := S_NEW_SUBSCRIPTON_DENIED;
    CS_SUBSCRIPTON_ALREADY_EXISTS: result := S_SUBSCRIPTON_ALREADY_EXISTS;
    CS_SUBSCRIPTION_NOT_EXISTS: result := S_SUBSCRIPTION_NOT_EXISTS;
    CS_PUBLISH_MSG_DENIED: result := S_PUBLISH_MSG_DENIED;
  end;
end;

function TsgcWSProtocol_Presence_Server.GetMembers: TsgcWSPresenceMemberList;
begin
  if not Assigned(FMembers) then
  begin
    FMembers := TsgcWSPresenceMemberList.Create;
    FMembers.OwnObjects := True;
  end;
  Result := FMembers;
end;

function TsgcWSProtocol_Presence_Server.GetWSMessageByConnection(aConnection:
    TsgcWSConnection): TsgcWSPresenceMessage;
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

procedure TsgcWSProtocol_Presence_Server.OnAcknowledgmentMessageEvent(
    aConnection: TsgcWSConnection; const aText: String);
begin
  inherited;
  WriteData(aConnection, aText)
end;

initialization

Classes.RegisterClass(TsgcWSProtocol_Presence_Server);
TsgcWSHTTPResponse.RegisterfileName(TsgcWSProtocol_JS_Response_Presence);
TsgcWSHTTPResponse.RegisterfileName(TsgcWSProtocol_HTML_Response_Presence);

finalization

{$ENDIF}

end.
