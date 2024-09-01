{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Protocol_sgc_Client;

interface

{$I sgcVer.inc}
{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils,
{$IFDEF MSWINDOWS}Windows, {$ENDIF}
{$IFDEF NEXTGEN}System.Generics.Collections, {$ELSE}Contnrs, {$ENDIF}
  // sgcBase
  sgcBase_Classes,
  // sgcWebSocket
  sgcJSON, sgcWebSocket_Protocol_sgc_Message, sgcWebSocket_Types,
  sgcWebSocket_Classes, sgcWebSocket_Protocol_Base_Client,
  sgcWebSocket_HTTPResponse,
  sgcWebSocket_Classes_SyncObjs;

type

  TsgcWSSessionEvent = procedure(Connection: TsgcWSConnection; Guid: string)
    of object;
  TsgcWSRPCResultEvent = procedure(Connection: TsgcWSConnection;
    Id, Result: string) of object;
  TsgcWSRPCErrorEvent = procedure(Connection: TsgcWSConnection; Id: string;
    ErrorCode: integer; ErrorMessage, ErrorData: string) of object;
  TsgcWSCustomEvent = procedure(Connection: TsgcWSConnection;
    const Channel, Text: string) of object;

  TsgcWSRPCItem = class
  private
    FDate: TDateTime;
    FId: string;
    FMethod: String;
    FParams: string;
  public
    property Date: TDateTime read FDate write FDate;
    property Id: string read FId write FId;
    property Method: String read FMethod write FMethod;
    property Params: string read FParams write FParams;
  end;

  TsgcWSRPCList = class({$IFDEF NEXTGEN}TList<TsgcWSRPCItem>{$ELSE}TObjectList{$ENDIF})

  end;

  TsgcWSProtocol_sgc_Client = class(TsgcWSProtocol_Subscription_Client_Base)
    { wsmessage }
  protected
    FWSMessageId: String;
    FWSMessage: TsgcWSMessage;
  protected
    function GetWSMessage: TsgcWSMessage;
    function GetWSMessageByConnection(const aConnection: TsgcWSConnection):
        TsgcWSMessage;
  public
    property WSMessage: TsgcWSMessage read GetWSMessage write FWSMessage;
    { wsmessage }

    { from TsgcWSComponent }
  private
    FWSConnection: TsgcWSConnection;
  protected
    procedure DoEventConnect(aConnection: TsgcWSConnection); override;
    procedure DoEventMessage(aConnection: TsgcWSConnection;
      const Text: string); override;
    procedure DoEventDisconnect(aConnection: TsgcWSConnection;
      Code: integer); override;
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

  protected
    function GetWSMessageText(const aMessage: TsgcWSMessage): String; virtual;
    procedure DoWriteMessageText(const aMessage: TsgcWSMessage); virtual;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    { procedures }
  protected
    procedure DoAcknowledgment(const aId: string); virtual;
  public
    procedure Subscribe(const aChannel: String;
      const aGuid: String = ''); override;
    procedure UnSubscribe(const aChannel: String;
      const aGuid: String = ''); override;
    procedure UnSubscribeAll(const aGuid: String = '');
    procedure Broadcast(const aText: String; const aChannel: String = '';
      const aGuid: String = '');
    procedure RPC(const aId, aMethod: string; const aParams: string = '';
      const aGuid: String = ''; const aQueue: TwsQueue = queueLevel0);
    procedure Notify(const aMethod: string; const aParams: string = '';
      const aGuid: String = ''; const aQueue: TwsQueue = queueLevel0);
    procedure Publish(const aText, aChannel: String; const aGuid: String = '';
      const aQueue: TwsQueue = queueLevel0);
    procedure GetSession;
    procedure StartTransaction(const aChannel: string = '');
    procedure Commit(const aChannel: string = '');
    procedure RollBack(const aChannel: string = '');
    { procedures }

    { events }
  private
    FOnAcknowledgment: TsgcWSAcknowledgment;
    FOnEvent: TsgcWSCustomEvent;
    FOnSession: TsgcWSSessionEvent;
    FOnRPCError: TsgcWSRPCErrorEvent;
    FOnRPCResult: TsgcWSRPCResultEvent;
  protected
    procedure DoEventRPCResult(aConnection: TsgcWSConnection;
      const aMessage: TsgcWSMessage); virtual;
    procedure DoEventRPCError(aConnection: TsgcWSConnection;
      const aMessage: TsgcWSMessage); virtual;
    procedure DoEventAcknowledgment(aConnection: TsgcWSConnection;
      const aMessage: TsgcWSMessage); virtual;
    procedure DoEventEvent(aConnection: TsgcWSConnection;
      const aMessage: TsgcWSMessage); virtual;
    procedure DoEventSession(aConnection: TsgcWSConnection;
      const aMessage: TsgcWSMessage); virtual;
  public
    property OnEvent: TsgcWSCustomEvent read FOnEvent write FOnEvent;
    property OnRPCError: TsgcWSRPCErrorEvent read FOnRPCError write FOnRPCError;
    property OnRPCResult: TsgcWSRPCResultEvent read FOnRPCResult
      write FOnRPCResult;
    property OnAcknowledgment: TsgcWSAcknowledgment read FOnAcknowledgment
      write FOnAcknowledgment;
    property OnSession: TsgcWSSessionEvent read FOnSession write FOnSession;
    { events }

    { rpc }
  private
    FRPCList: TsgcWSRPCList;
    function GetRPCList: TsgcWSRPCList;
  protected
    procedure DoQueueRPC(const aId, aMethod, aParams: string); virtual;
    procedure DoDeleteRPC(const aId: String); virtual;
  protected
    property RPCList: TsgcWSRPCList read GetRPCList;
  public
    function GetRPCMethodById(const aId: String): string;
    function GetRPCParamsById(const aId: String): string;
    { rpc }

    { QoS }
  private
    FQoSTimer: TsgcTimer;
    FQoS: TsgcWSQoS_Options;
    FQoSList: TsgcWSQoSList;
    function GetQoSList: TsgcWSQoSList;
  protected
    procedure DoStartQoS; virtual;
    procedure DoStopQoS; virtual;
  protected
    procedure DoQoSList; virtual;
  protected
    procedure DoProcessAcknowledgment(const aId: String); virtual;
    procedure DoProcessPubRec(aId: String); virtual;
    procedure DoQueueQoS(const aId, aText: String); virtual;
  protected
    procedure OnQoSEvent(Sender: TObject); virtual;
    procedure OnQoSExceptionEvent(Sender: TObject; E: Exception); virtual;
  protected
    procedure SetQoS(const Value: TsgcWSQoS_Options);
  protected
    property QoSList: TsgcWSQoSList read GetQoSList;
  public
    property QoS: TsgcWSQoS_Options read FQoS write SetQoS;
    { QoS }

  end;

{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses sgcWebSocket_Const, sgcBase_Helpers;

const
  CS_QOS_LIST = 1;
  CS_RPC_LIST = 0;

type
  THackComponent_Client = class(TsgcWSComponent_Client);

constructor TsgcWSProtocol_sgc_Client.Create(aOwner: TComponent);
begin
  inherited;
  FWSMessageId := NewGuid;
  FProtocol := CS_PROTOCOL_SGC;
  FQoS := TsgcWSQoS_Options.Create;
  FQoS.Level := qosLevel0;
  FQoS.Interval := 60;
  FQoS.Timeout := 300;
end;

destructor TsgcWSProtocol_sgc_Client.Destroy;
begin
  sgcFree(FQoSTimer);
  sgcFree(FRPCList);
  sgcFree(FQoS);
  sgcFree(FQoSList);
  inherited;
end;

procedure TsgcWSProtocol_sgc_Client.DoEventMessage
  (aConnection: TsgcWSConnection; const Text: string);
var
  oMessage: TsgcWSMessage;
begin
{$IFDEF SGC_DEBUG}
  DoLog(self, aConnection, 'DoEventMessage', '[Text]: ' + Text);
{$ENDIF}
  if DoRawMessage(aConnection, Text) then
    exit;

  oMessage := WSMessage;
  oMessage.Read(Text);

  if oMessage.Method = CS_SGC_SESSION then
  begin
    DoAcknowledgment(oMessage.Id);
    DoEventSession(aConnection, oMessage);
  end
  else if oMessage.Method = CS_SGC_EVENT then
    DoEventEvent(aConnection, oMessage)
  else if oMessage.Method = CS_SGC_MESSAGE then
    inherited DoEventMessage(aConnection, oMessage.Text)
  else if oMessage.Method = CS_SGC_SUBSCRIBE then
  begin
    aConnection.DoSubscribe(aConnection.Guid + '_' + oMessage.Channel);
    aConnection.LastSubscription := oMessage.Channel;
    DoNotifySubscription(aConnection);
  end
  else if oMessage.Method = CS_SGC_UNSUBSCRIBE then
  begin
    aConnection.DoUnSubscribe(aConnection.Guid + '_' + oMessage.Channel);
    aConnection.LastUnSubscription := oMessage.Channel;
    DoNotifyUnSubscription(aConnection)
  end
  else if oMessage.Method = CS_SGC_ACKNOWLEDGMENT then
  begin
    DoProcessAcknowledgment(oMessage.Id);
    DoEventAcknowledgment(aConnection, oMessage);
  end
  else if oMessage.Method = CS_SGC_PUBREC then
    DoProcessPubRec(oMessage.Id)
  else if oMessage.ErrorCode <> 0 then
  begin
    DoAcknowledgment(oMessage.Id);
    DoEventRPCError(aConnection, oMessage);
    DoDeleteRPC(oMessage.Id);
  end
  else if oMessage.Result <> '' then
  begin
    DoAcknowledgment(oMessage.Id);
    DoEventRPCResult(aConnection, oMessage);
    DoDeleteRPC(oMessage.Id);
  end
  else
    inherited;
end;

procedure TsgcWSProtocol_sgc_Client.RPC(const aId, aMethod: string;
  const aParams: string = ''; const aGuid: String = '';
  const aQueue: TwsQueue = queueLevel0);
var
  vId: String;
  oMessage: TsgcWSMessage;
begin
  oMessage := TsgcWSMessage.Create(nil);
  Try
    oMessage.DoEnterWrite;
    vId := aId;
    if vId = '' then
      vId := NewGuid;

    oMessage.Id := vId;
    oMessage.Queue := TwsQueue_String[Ord(aQueue)];
    oMessage.Method := aMethod;
    oMessage.Params := aParams;
    oMessage.Guid := aGuid;

    DoQueueRPC(vId, aMethod, aParams);

    DoWriteMessageText(oMessage);
  Finally
    sgcFree(oMessage);
  End;
end;

procedure TsgcWSProtocol_sgc_Client.Subscribe(const aChannel: String;
  const aGuid: String = '');
var
  oMessage: TsgcWSMessage;
begin
  oMessage := TsgcWSMessage.Create(nil);
  Try
    oMessage.DoEnterWrite;
    oMessage.Method := CS_SGC_SUBSCRIBE;
    oMessage.Channel := aChannel;
    oMessage.Guid := aGuid;

    DoWriteMessageText(oMessage);
  Finally
    sgcFree(oMessage);
  End;
end;

procedure TsgcWSProtocol_sgc_Client.UnSubscribe(const aChannel: String;
  const aGuid: String = '');
var
  oMessage: TsgcWSMessage;
begin
  oMessage := TsgcWSMessage.Create(nil);
  Try
    oMessage.DoEnterWrite;
    oMessage.Method := CS_SGC_UNSUBSCRIBE;
    oMessage.Channel := aChannel;
    oMessage.Guid := aGuid;

    DoWriteMessageText(oMessage);
  Finally
    sgcFree(oMessage);
  End;
end;

procedure TsgcWSProtocol_sgc_Client.Broadcast(const aText: String;
  const aChannel: String = ''; const aGuid: String = '');
var
  oMessage: TsgcWSMessage;
begin
  oMessage := TsgcWSMessage.Create(nil);
  Try
    oMessage.DoEnterWrite;
    oMessage.Method := CS_SGC_BROADCAST;
    oMessage.Text := aText;
    oMessage.Channel := aChannel;
    oMessage.Guid := aGuid;

    DoWriteMessageText(oMessage);
  Finally
    sgcFree(oMessage);
  End;
end;

procedure TsgcWSProtocol_sgc_Client.Commit(const aChannel: string = '');
var
  oMessage: TsgcWSMessage;
begin
  oMessage := TsgcWSMessage.Create(nil);
  Try
    oMessage.DoEnterWrite;
    oMessage.Method := CS_SGC_COMMIT;
    oMessage.Channel := aChannel;

    DoWriteMessageText(oMessage);
  Finally
    sgcFree(oMessage);
  End;
end;

procedure TsgcWSProtocol_sgc_Client.DoAcknowledgment(const aId: string);
var
  oJSON: IsgcJSON;
  oObject: IsgcObjectJSON;
begin
  oJSON := GetJSONInstance;
  Try
    oJSON.AddPair(CS_JSON_RPC, CS_JSONRPC_VERSION);
    oObject := oJSON.AddObject(CS_RESULT);
    oObject.JSONObject.AddPair(CS_METHOD, CS_SGC_ACKNOWLEDGMENT);
    oJSON.AddPair(CS_ID, aId);

    inherited WriteData(oJSON.Text);
  Finally
    FreeJSONInstance(oJSON);
  End;
end;

procedure TsgcWSProtocol_sgc_Client.DoClear(aConnection: TsgcWSConnection);
begin
  inherited;
end;

procedure TsgcWSProtocol_sgc_Client.DoDeleteRPC(const aId: String);
var
  i: integer;
begin
  DoEnterCS(CS_RPC_LIST);
  Try
    for i := RPCList.Count - 1 Downto 0 do
    begin
      if TsgcWSRPCItem(RPCList.Items[i]).Id = aId then
        RPCList.Delete(i);
    end;
  Finally
    DoLeaveCS(CS_RPC_LIST);
  End;
end;

procedure TsgcWSProtocol_sgc_Client.DoEventAcknowledgment
  (aConnection: TsgcWSConnection; const aMessage: TsgcWSMessage);
var
  vId: String;
begin
  if Assigned(FOnAcknowledgment) then
  begin
    vId := aMessage.Id;

    FOnAcknowledgment(aConnection, vId);
  end;
end;

procedure TsgcWSProtocol_sgc_Client.DoEventConnect
  (aConnection: TsgcWSConnection);
begin
  inherited;
  FWSConnection := aConnection;

  DoStartQoS;
end;

procedure TsgcWSProtocol_sgc_Client.DoEventDisconnect
  (aConnection: TsgcWSConnection; Code: integer);
var
  vId: String;
begin
  vId := aConnection.Guid;
  if Assigned(FWSMessage) then
    FWSMessage.Clear(True);
  DoStopQoS;
  FWSConnection := nil;
  inherited;
end;

procedure TsgcWSProtocol_sgc_Client.DoEventEvent(aConnection: TsgcWSConnection;
  const aMessage: TsgcWSMessage);
var
  vChannel: string;
  vText: string;
begin
  if Assigned(FOnEvent) then
  begin
    vChannel := aMessage.Channel;
    vText := aMessage.Text;

    FOnEvent(aConnection, vChannel, vText);
  end;
end;

procedure TsgcWSProtocol_sgc_Client.DoEventRPCError
  (aConnection: TsgcWSConnection; const aMessage: TsgcWSMessage);
var
  vId: String;
  vErrorCode: integer;
  vErrorMessage: String;
  vErrorData: String;
begin
  if Assigned(FOnRPCError) then
  begin
    vId := aMessage.Id;
    vErrorCode := aMessage.ErrorCode;
    vErrorMessage := aMessage.ErrorMessage;
    vErrorData := aMessage.ErrorData;

    FOnRPCError(aConnection, vId, vErrorCode, vErrorMessage, vErrorData);
  end;
end;

procedure TsgcWSProtocol_sgc_Client.DoEventRPCResult
  (aConnection: TsgcWSConnection; const aMessage: TsgcWSMessage);
var
  vId: String;
  vResult: String;
begin
  if Assigned(FOnRPCResult) then
  begin
    vId := aMessage.Id;
    vResult := aMessage.Result;

    FOnRPCResult(aConnection, vId, vResult);
  end;
end;

procedure TsgcWSProtocol_sgc_Client.DoEventSession
  (aConnection: TsgcWSConnection; const aMessage: TsgcWSMessage);
var
  vText: string;
begin
  if Assigned(FOnSession) then
  begin
    vText := aMessage.Text;

    FOnSession(aConnection, vText);
  end;
end;

procedure TsgcWSProtocol_sgc_Client.DoFinalize(aConnection: TsgcWSConnection);
begin
  inherited;
  DoUnSubscriptions(aConnection);

  if Assigned(FWSMessage) then
    FWSMessage.Clear(True);
  FWSConnection := nil;
  DoStopQoS;
end;

procedure TsgcWSProtocol_sgc_Client.DoInitialize(aConnection: TsgcWSConnection);
begin
  inherited;
  FWSConnection := aConnection;

  DoStartQoS;
end;

procedure TsgcWSProtocol_sgc_Client.DoProcessAcknowledgment(const aId: String);
var
  i: integer;
begin
  if QoS.Level <> qosLevel0 then
  begin
    DoEnterCS(CS_QOS_LIST);
    Try
      for i := QoSList.Count - 1 Downto 0 do
      begin
        if TsgcWSQoSItem(QoSList.Items[i]).Id = aId then
          QoSList.Delete(i);
      end;
    Finally
      DoLeaveCS(CS_QOS_LIST);
    End;
  end;
end;

procedure TsgcWSProtocol_sgc_Client.DoProcessPubRec(aId: String);
var
  oMessage: TsgcWSMessage;
begin
  oMessage := TsgcWSMessage.Create(nil);
  Try
    oMessage.DoEnterWrite;
    oMessage.Method := CS_SGC_PUBREL;
    oMessage.Id := aId;

    DoWriteMessageText(oMessage);
  Finally
    sgcFree(oMessage);
  End;
end;

procedure TsgcWSProtocol_sgc_Client.DoQoSList;
var
  i: integer;
begin
  for i := 0 to QoSList.Count - 1 do
  begin
    if Trunc((Now - TsgcWSQoSItem(QoSList.Items[i]).Date) * 86400) > QoS.Timeout
    then
      inherited WriteData(TsgcWSQoSItem(QoSList.Items[i]).Text);
  end;
end;

procedure TsgcWSProtocol_sgc_Client.DoQueueQoS(const aId, aText: String);
var
  oItem: TsgcWSQoSItem;
begin
  if aId = '' then
    exit;

  case QoS.Level of
    qosLevel1, qosLevel2:
      begin
        DoEnterCS(CS_QOS_LIST);
        Try
          oItem := TsgcWSQoSItem.Create;
          oItem.Id := aId;
          oItem.Text := aText;
          oItem.Date := Now;
          QoSList.Add(oItem)
        Finally
          DoLeaveCS(CS_QOS_LIST);
        End;
      end;
  end;
end;

procedure TsgcWSProtocol_sgc_Client.DoQueueRPC(const aId, aMethod,
  aParams: string);
var
  oItem: TsgcWSRPCItem;
begin
  if aId = '' then
    exit;

  DoEnterCS(CS_RPC_LIST);
  Try
    oItem := TsgcWSRPCItem.Create;
    oItem.Id := aId;
    oItem.Method := aMethod;
    oItem.Params := aParams;
    oItem.Date := Now;
    RPCList.Add(oItem)
  Finally
    DoLeaveCS(CS_RPC_LIST);
  End;
end;

procedure TsgcWSProtocol_sgc_Client.DoStartQoS;
begin
  if QoS.Level <> qosLevel0 then
  begin
    if not Assigned(FQoSTimer) then
    begin
      FQoSTimer := TsgcTimer.Create;
      FQoSTimer.DebugName := 'QoS Timer';
      if Assigned(Client) then
        FQoSTimer.NotifyEvents := THackComponent_Client(Client).NotifyEvents;
      FQoSTimer.Interval := FQoS.Interval;
      FQoSTimer.OnTimer := OnQoSEvent;
      FQoSTimer.OnException := OnQoSExceptionEvent;
      FQoSTimer.Interval := QoS.Interval * 1000;
    end;

    if FQoSTimer.Interval > 0 then
    begin
      if not FQoSTimer.Enabled then
        FQoSTimer.Enabled := True;
    end;
  end;
end;

procedure TsgcWSProtocol_sgc_Client.DoStopQoS;
begin
  sgcThreadFree(FQoSTimer);
end;

procedure TsgcWSProtocol_sgc_Client.DoWriteMessageText(const aMessage:
    TsgcWSMessage);
begin
  inherited WriteData(GetWSMessageText(aMessage));
end;

procedure TsgcWSProtocol_sgc_Client.GetSession;
var
  oMessage: TsgcWSMessage;
begin
  oMessage := TsgcWSMessage.Create(nil);
  Try
    oMessage.DoEnterWrite;
    oMessage.Id := '';
    oMessage.Method := CS_SGC_SESSION;
    oMessage.Params := '';
    oMessage.Guid := '';

    DoWriteMessageText(oMessage);
  Finally
    sgcFree(oMessage);
  End;
end;

function TsgcWSProtocol_sgc_Client.GetQoSList: TsgcWSQoSList;
begin
  if not Assigned(FQoSList) then
    FQoSList := TsgcWSQoSList.Create;
  Result := FQoSList;
end;

function TsgcWSProtocol_sgc_Client.GetRPCList: TsgcWSRPCList;
begin
  if not Assigned(FRPCList) then
    FRPCList := TsgcWSRPCList.Create;
  Result := FRPCList;
end;

function TsgcWSProtocol_sgc_Client.GetRPCMethodById(const aId: String): string;
var
  i: integer;
begin
  Result := '';

  for i := 0 to RPCList.Count - 1 do
  begin
    if TsgcWSRPCItem(RPCList.Items[i]).Id = aId then
    begin
      Result := TsgcWSRPCItem(RPCList.Items[i]).Method;
      break;
    end;
  end;
end;

function TsgcWSProtocol_sgc_Client.GetRPCParamsById(const aId: String): string;
var
  i: integer;
begin
  Result := '';

  for i := 0 to RPCList.Count - 1 do
  begin
    if TsgcWSRPCItem(RPCList.Items[i]).Id = aId then
    begin
      Result := TsgcWSRPCItem(RPCList.Items[i]).Params;
      break;
    end;
  end;
end;

function TsgcWSProtocol_sgc_Client.GetWSMessageText(const aMessage:
    TsgcWSMessage): String;
var
  vId, vMessage: string;
begin
  if Assigned(aMessage) then
  begin
    aMessage.QoS := TwsQoS_String[Ord(QoS.Level)];

    vId := aMessage.Id;
    vMessage := aMessage.Write;

    Result := vMessage;

    DoQueueQoS(vId, vMessage);
  end;
end;

procedure TsgcWSProtocol_sgc_Client.Notify(const aMethod: string;
  const aParams: string = ''; const aGuid: String = '';
  const aQueue: TwsQueue = queueLevel0);
var
  oMessage: TsgcWSMessage;
begin
  oMessage := TsgcWSMessage.Create(nil);
  Try
    oMessage.DoEnterWrite;
    oMessage.Id := '';
    oMessage.Method := aMethod;
    oMessage.Params := aParams;
    oMessage.Guid := aGuid;

    DoWriteMessageText(oMessage);
  Finally
    sgcFree(oMessage);
  End;
end;

procedure TsgcWSProtocol_sgc_Client.OnQoSEvent(Sender: TObject);
begin
  DoQoSList;
end;

procedure TsgcWSProtocol_sgc_Client.Publish(const aText, aChannel: String;
  const aGuid: String = ''; const aQueue: TwsQueue = queueLevel0);
var
  oMessage: TsgcWSMessage;
begin
  oMessage := TsgcWSMessage.Create(nil);
  Try
    oMessage.DoEnterWrite;
    if QoS.Level = qosLevel0 then
      oMessage.Id := ''
    else
      oMessage.Id := NewGuid;
    oMessage.Queue := TwsQueue_String[Ord(aQueue)];
    oMessage.Method := CS_SGC_PUBLISH;
    oMessage.Text := aText;
    oMessage.Channel := aChannel;
    oMessage.Guid := aGuid;

    DoWriteMessageText(oMessage);
  Finally
    sgcFree(oMessage);
  End;
end;

procedure TsgcWSProtocol_sgc_Client.RollBack(const aChannel: string = '');
var
  oMessage: TsgcWSMessage;
begin
  oMessage := TsgcWSMessage.Create(nil);
  Try
    oMessage.DoEnterWrite;
    oMessage.Method := CS_SGC_ROLLBACK;
    oMessage.Channel := aChannel;

    DoWriteMessageText(oMessage);
  Finally
    sgcFree(oMessage);
  End;
end;

procedure TsgcWSProtocol_sgc_Client.SetQoS(const Value: TsgcWSQoS_Options);
begin
  FQoS.Assign(Value);
end;

procedure TsgcWSProtocol_sgc_Client.StartTransaction(const aChannel
  : string = '');
var
  oMessage: TsgcWSMessage;
begin
  oMessage := TsgcWSMessage.Create(nil);
  Try
    oMessage.DoEnterWrite;
    oMessage.Method := CS_SGC_TRANSACTION;
    oMessage.Channel := aChannel;

    DoWriteMessageText(oMessage);
  Finally
    sgcFree(oMessage);
  End;
end;

procedure TsgcWSProtocol_sgc_Client.UnSubscribeAll(const aGuid: String = '');
var
  oMessage: TsgcWSMessage;
begin
  oMessage := TsgcWSMessage.Create(nil);
  Try
    oMessage.DoEnterWrite;
    oMessage.Method := CS_SGC_UNSUBSCRIBE_ALL;
    oMessage.Channel := '';
    oMessage.Guid := aGuid;

    DoWriteMessageText(oMessage);
  Finally
    sgcFree(oMessage);
  End;
end;

procedure TsgcWSProtocol_sgc_Client.WriteData(const aText: String);
var
  oMessage: TsgcWSMessage;
begin
  oMessage := TsgcWSMessage.Create(nil);
  Try
    oMessage.DoEnterWrite;
    oMessage.Method := CS_SGC_MESSAGE;
    oMessage.Text := aText;
    oMessage.Channel := '';
    oMessage.Guid := '';

    DoWriteMessageText(oMessage);
  Finally
    sgcFree(oMessage);
  End;
end;

procedure TsgcWSProtocol_sgc_Client.DoWriteRawData(const aText: String);
begin
  inherited WriteData(aText);
end;

function TsgcWSProtocol_sgc_Client.GetWSMessage: TsgcWSMessage;
begin
  result := GetWSMessageByConnection(FWSConnection);

  if not Assigned(result) then
  begin
    if not Assigned(FWSMessage) then
      FWSMessage := TsgcWSMessage.Create(self);
    Result := FWSMessage;
  end;
end;

function TsgcWSProtocol_sgc_Client.GetWSMessageByConnection(const aConnection:
    TsgcWSConnection): TsgcWSMessage;
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
      oItem.WSMessage := TsgcWSMessage.Create(nil);
      AddProtocolDataItem(aConnection, FWSMessageId, oItem);
    end;

    result := TsgcWSMessage(oItem.WSMessage);
  end;
end;

procedure TsgcWSProtocol_sgc_Client.OnQoSExceptionEvent(Sender: TObject;
  E: Exception);
begin
  DoEventException(FWSConnection, E.Message, E);
end;

{$ENDIF}

end.
