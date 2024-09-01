{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcWebSocket_Protocol_sgc_Server;

interface

{$I sgcVer.inc}

{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils,
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  {$IFDEF NEXTGEN}System.Generics.Collections, {$ELSE}Contnrs, {$ENDIF}
  // sgcbase
  sgcBase_Classes, sgcBase_Helpers,
  // sgcWebSockets
  sgcJSON, sgcWebSocket_Protocol_sgc_Message, sgcWebSocket_Types,
  sgcWebSocket_Classes, sgcWebSocket_Protocol_Base_Server,
  sgcWebSocket_HTTPResponse, sgcWebSocket_Classes_SyncObjs;

type

  TsgcWSRPCEvent = procedure(Connection: TsgcWSConnection;
    const ID, Method, Params: string) of object;
  TsgcWSNotificationEvent = procedure(Connection: TsgcWSConnection; const Method,
      Params: string) of object;
  TsgcWSRPCAuthenticationEvent = procedure(Connection: TsgcWSConnection;
    const Method, User, Password: string; var Authenticated: Boolean)
    of object;
  TsgcWSBeforePublish = procedure(Connection: TsgcWSConnection; const aChannel:
      String; var aText: String; var Accept: Boolean) of object;


  TsgcThreadListMethodsId = class(TsgcThreadList{$IFDEF NEXTGEN}<TObject>{$ENDIF})
  end;

  TsgcWSMethodId = class
  private
    FConnection: TsgcWSConnection;
    FId: String;
  public
    property Connection: TsgcWSConnection read FConnection write FConnection;
    property Id: String read FId write FId;
  end;

  TsgcWSTransaction = class
  private
    FActive: Boolean;
    FID: String;
    FQueue: TsgcWSObjectList;
    function GetQueue: TsgcWSObjectList;
    procedure SetActive(const Value: Boolean);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  public
    property Active: Boolean read FActive write SetActive;
    property ID: String read FID write FID;
    property Queue: TsgcWSObjectList read GetQueue write FQueue;
  end;


  TsgcWSTransactions = class({$IFDEF NEXTGEN}TList<TsgcWSTransaction>{$ELSE}TObjectList{$ENDIF})
  public
    procedure Add(const ID: string); overload;
  end;

  TsgcWSQueuedMessage = class
  private
    FID: String;
    FQueue: TsgcWSObjectList;
    function GetQueue: TsgcWSObjectList;
  public
    destructor Destroy; override;
  public
    property ID: String read FID write FID;
    property Queue: TsgcWSObjectList read GetQueue write FQueue;
  end;


  TsgcWSQueuedMessages = class({$IFDEF NEXTGEN}TList<TsgcWSQueuedMessage>{$ELSE}TObjectList{$ENDIF})
  public
    function Add(const ID: string): TsgcWSQueuedMessage; overload;
  end;


  TsgcWSQoSLevel2 = class
  private
    FID: String;
    FNotifyObject: TsgcWSNotifyObject;
    function GetNotifyObject: TsgcWSNotifyObject;
  public
    destructor Destroy; override;
  public
    property ID: String read FID write FID;
    property NotifyObject: TsgcWSNotifyObject read GetNotifyObject write
        FNotifyObject;
  end;


  TsgcWSQoSLevel2List = class({$IFDEF NEXTGEN}TList<TsgcWSQoSLevel2>{$ELSE}TObjectList{$ENDIF})
  public
    function Add(const aID: string; const aConnection: TsgcWSConnection; const
        aText: String): TsgcWSQoSLevel2; overload;
    function Item(const aId: string): Integer;
    procedure Delete(const aID: string);
  end;


  TsgcWSAuthentication_Methods = class(TPersistent)
  private
    FMethods: TStringList;
    FEnabled: Boolean;
    procedure SetMethods(const Value: TStringList);
  public
    constructor Create; virtual;
    destructor Destroy; override;
  published
    property Methods: TStringList read FMethods write SetMethods;
    property Enabled: Boolean read FEnabled write FEnabled;
  end;

  TsgcWSProtocol_sgc_Server = class(TsgcWSProtocol_Subscription_Server_Base)
    { from TComponent }
  protected
    procedure Loaded; override;
    { from TComponent }

    { wsmessage }
  private
    FWSMessageId: String;
  protected
    function GetWSMessageByConnection(const aConnection: TsgcWSConnection):
        TsgcWSMessage; virtual;
    { wsmessage }

    { from TsgcWSComponent }
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

    { from TsgcWSProtocol_Server_Base }
  public
    function WriteData(aGuid, aMessage: string): Boolean; overload; override;
    { from TsgcWSProtocol_Server_Base }

    { broadcast }
  protected
    procedure DoBroadcastAcknowledgment(aMessage: TsgcWSMessage; const aChannel:
        string = ''; const Exclude: String = ''; const Include: String = '');
        overload; virtual;
  protected
    procedure DoBroadCast(aMessage: TsgcWSMessage; const aChannel: string = '';
        const Exclude: String = ''; const Include: String = ''); overload; virtual;
    procedure DoBroadCast(aStream: TStream; const aChannel: string = ''; const
        Exclude: String = ''; const Include: String = ''; const aSize: Integer = 0;
        const aStreaming: TwsStreaming = stmNone); overload; virtual;
  public
    procedure Broadcast(aMessage: string; aChannel: string = ''; Exclude: String =
        ''; Include: String = ''); overload; override;
    procedure Broadcast(aStream: TStream; aChannel: string = ''; Exclude: String =
        ''; Include: String = ''; aSize: Integer = 0; aStreaming: TwsStreaming =
        stmNone); overload; override;
    { broadcast }

    { constructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor }

    { transactions }
  private
    FTransactions: TsgcWSTransactions;
    function GetTransactions: TsgcWSTransactions;
  protected
    function GetTransaction(const aId: String): TsgcWSTransaction; virtual;
    function InTransaction(const aId: String): Boolean; virtual;
    procedure DoStartTransaction(const aId: String); virtual;
    procedure DoDeleteTransaction(const aId: String); virtual;
    procedure DoCommit(const aMessage: TsgcWSMessage; const aId: String); virtual;
    procedure DoRollBack(const aId: String); virtual;
    function DoQueueTransaction(aConnection: TsgcWSConnection; aText: String;
        aMessage: TsgcWSMessage): Boolean;
  protected
    property Transactions: TsgcWSTransactions read GetTransactions;
    { transactions }


    { queued Messages }
  private
    FQueuedMessages: TsgcWSQueuedMessages;
    function GetQueuedMessages: TsgcWSQueuedMessages;
  protected
    function GetQueuedMessage(const aId: String): TsgcWSQueuedMessage; virtual;
    procedure DoProcessQueuedMessages(const aId: String; aConnection:
        TsgcWSConnection); virtual;
    function DoQueueMessageLevel1(aConnection: TsgcWSConnection; aText, aChannel:
        String): Boolean;
    function DoQueueMessageLevel2(aConnection: TsgcWSConnection; aText, aChannel:
        String): Boolean;
  protected
    property QueuedMessages: TsgcWSQueuedMessages read GetQueuedMessages;
  public
    function ClearQueue(const aChannel: String): Boolean;
    { queued Messages }

    { QoS }
  private
    FQoSLevel2List: TsgcWSQoSLevel2List;
    function GetQoSLevel2List: TsgcWSQoSLevel2List;
  protected
    procedure DoQoSLevel2Publish(const aId: String; aConnection: TsgcWSConnection;
        const aText: String); virtual;
    procedure DoQoSLevel2PubRec(const aConnection: TsgcWSConnection; const aID:
        String);
    procedure DoQoSLevel2PubRel(const aConnection: TsgcWSConnection; const aId:
        String);
  protected
    property QoSLevel2List: TsgcWSQoSLevel2List read GetQoSLevel2List;
    { QoS }

  { procedures }
  protected
    function DoWriteMessageText(const aGuid, aMessage: string): Boolean; overload;
    function DoWriteMessageText(const aConnection: TsgcWSConnection;const aMessage:
        string): Boolean; overload;
  protected
    procedure DoProcess(const aConnection: TsgcWSConnection; const aText: String);
    procedure DoProcessMessage(aConnection: TsgcWSConnection; const aText: String);
        virtual;
  protected
    procedure DoSession(const aConnection: TsgcWSConnection); virtual;
    procedure DoAcknowledgment(const aConnection: TsgcWSConnection; const aId:
        string); virtual;
  protected
    procedure DoPublish(const aMessage, aChannel, aExclude, aInclude: String; const
        aQueue: TwsQueue);
  public
    procedure Publish(const aMessage, aChannel: String; const aExclude: String =
        ''; const aInclude: String = ''; const aQueue: TwsQueue = queueLevel0);
        overload;
    procedure Publish(const aMessage, aChannel: String; const aQueue: TwsQueue);
        overload;
    procedure RPCResult(aId, aResult: String);
    procedure RPCError(aId: String; aCode: Integer; aMessage: String; aData: String
        = '');
  { procedures }

  { method id }
  private
    FMethodsId: TsgcThreadListMethodsId;
  protected
    function GetMethodsId: TsgcThreadListMethodsId;
  protected
    function AddMethodID(const aConnection: TsgcWSConnection; const aId: string):
        Boolean;
    function GetMethodId(const aId: string): TsgcWSConnection;
    function RemoveMethodId(const aId: string): Boolean;
  { method id }

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
    procedure DoProcessAcknowledgment(const aGuid, aId: String); virtual;
    procedure DoQueueQoS(const aGuid, aId, aText: string); virtual;
    procedure DoDeleteQoSByGuid(const aGuid: String); virtual;
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

  { RPCAuthentication }
  private
    FRPCAuthentication: TsgcWSAuthentication_Methods;
  protected
    function DoRPCAuthentication(aConnection: TsgcWSConnection;
      const aMethod: string): Boolean;
  protected
    procedure SetRPCAuthentication(const Value: TsgcWSAuthentication_Methods);
  public
    property RPCAuthentication: TsgcWSAuthentication_Methods read
        FRPCAuthentication write SetRPCAuthentication;
  { RPCAuthentication }

  { events }
  private
    FOnAcknowledgment: TsgcWSAcknowledgment;
    FOnNotification: TsgcWSNotificationEvent;
    FOnRPC: TsgcWSRPCEvent;
    FOnRPCAuthentication: TsgcWSRPCAuthenticationEvent;
    FOnBeforePublish: TsgcWSBeforePublish;
  protected
    procedure DoEventNotification(aConnection: TsgcWSConnection; const aMessage:
        TsgcWSMessage); virtual;
    procedure DoEventRPC(aConnection: TsgcWSConnection; const aMessage:
        TsgcWSMessage); virtual;
    procedure DoEventRPCAuthentication(aConnection: TsgcWSConnection; const
        aMethod: string; var Result: Boolean); virtual;
    procedure DoEventBeforePublish(aConnection: TsgcWSConnection; const aChannel:
        String; var aText: String; var Accept: Boolean); virtual;
    procedure DoEventAcknowledgment(aConnection: TsgcWSConnection; const Id:
        String); virtual;
  public
    property OnAcknowledgment: TsgcWSAcknowledgment read FOnAcknowledgment write
        FOnAcknowledgment;
    property OnNotification: TsgcWSNotificationEvent read FOnNotification write
        FOnNotification;
    property OnRPC: TsgcWSRPCEvent read FOnRPC write FOnRPC;
    property OnRPCAuthentication
      : TsgcWSRPCAuthenticationEvent read FOnRPCAuthentication write
      FOnRPCAuthentication;
    property OnBeforePublish: TsgcWSBeforePublish read FOnBeforePublish write FOnBeforePublish;
  { events }
  end;

  TsgcWSProtocol_JS_sgc = class(TsgcWSHTTPResponse_Base)
  protected
    function GetResponse: string; override;
  public
    class function GetFileName: string; override;
  end;

  TsgcWSProtocol_HTML_sgc = class(TsgcWSHTTPResponse_Base)
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
  sgcWebSocket_Protocol_Base_Message;

const
  CS_TRANSACTIONS = 0;
  CS_QOS_LEVEL2_LIST = 1;
  CS_QOS_LIST = 2;

type
  THackComponent_Server = class(TsgcWSComponent_Server);

constructor TsgcWSProtocol_sgc_Server.Create(aOwner: TComponent);
begin
  inherited;
  FProtocol := CS_PROTOCOL_SGC;
  FWSMessageId := NewGuid;
  FRPCAuthentication := TsgcWSAuthentication_Methods.Create;
  FQoS := TsgcWSQoS_Options.Create;
  FQoS.Level := qosLevel0;
  FQoS.Interval := 60;
  FQoS.Timeout := 300;
end;

destructor TsgcWSProtocol_sgc_Server.Destroy;
begin
  DoStopQoS;
  sgcFree(FQoSTimer);
  sgcFree(FQueuedMessages);
  sgcFree(FQoSLevel2List);
  sgcFree(FTransactions);
  sgcFree(FMethodsId);
  sgcFree(FRPCAuthentication);
  sgcFree(FQoS);
  sgcFree(FQoSList);
  inherited;
end;

function TsgcWSProtocol_sgc_Server.AddMethodID(const aConnection:
    TsgcWSConnection; const aId: string): Boolean;
var
  oMethod: TsgcWSMethodId;
begin
  Result := False;
  if GetMethodId(aId) = nil then
  begin
    oMethod := TsgcWSMethodId.Create;
    oMethod.Connection := aConnection;
    oMethod.Id := aId;

    GetMethodsId.AddKey(oMethod, oMethod.Id);
  end;
end;

procedure TsgcWSProtocol_sgc_Server.Broadcast(aMessage: string; aChannel:
    string = ''; Exclude: String = ''; Include: String = '');
var
  oMessage: TsgcWSMessage;
begin
  oMessage := TsgcWSMessage.Create(nil);
  Try
    oMessage.DoEnterWrite(True);
    Try
      oMessage.method := CS_SGC_MESSAGE;
      oMessage.result := CS_SGC_MESSAGE;
      oMessage.Text := aMessage;
      oMessage.Channel := aChannel;
      oMessage.params := ''; // ... prevent params from other message
      DoBroadCast(oMessage, aChannel, Exclude, Include);
    Finally
      oMessage.DoLeaveWrite;
    End;
  Finally
    sgcFree(oMessage);
  End;
end;

procedure TsgcWSProtocol_sgc_Server.Broadcast(aStream: TStream; aChannel:
    string = ''; Exclude: String = ''; Include: String = ''; aSize: Integer =
    0; aStreaming: TwsStreaming = stmNone);
begin
  DoBroadCast(aStream, aChannel, Exclude, Include, aSize, aStreaming);
end;

procedure TsgcWSProtocol_sgc_Server.DoBroadCast(aMessage: TsgcWSMessage; const
    aChannel: string = ''; const Exclude: String = ''; const Include: String =
    '');
var
  vChannel: string;
begin
  if aChannel <> '' then
  begin
    vChannel := guid + '_' + aChannel;
    if QoS.Level <> qosLevel0 then
      DoBroadcastAcknowledgment(aMessage, vChannel, Exclude, Include)
    else
      inherited Broadcast(aMessage.Write, vChannel, Exclude, Include)
  end
  else
  begin
    if QoS.Level <> qosLevel0 then
      DoBroadcastAcknowledgment(aMessage, '', Exclude, Include)
    else
      inherited Broadcast(aMessage.Write, '', Exclude, Include);
  end;
end;

procedure TsgcWSProtocol_sgc_Server.DoBroadCast(aStream: TStream; const
    aChannel: string = ''; const Exclude: String = ''; const Include: String =
    ''; const aSize: Integer = 0; const aStreaming: TwsStreaming = stmNone);
var
  vChannel: string;
begin
  if aChannel <> '' then
  begin
    vChannel := guid + '_' + aChannel;
    inherited Broadcast(aStream, vChannel, Exclude, Include, aSize, aStreaming)
  end
  else
    inherited Broadcast(aStream, '', Exclude, Include, aSize, aStreaming);
end;

procedure TsgcWSProtocol_sgc_Server.DoCommit(const aMessage: TsgcWSMessage;
    const aId: String);
var
  oTransaction: TsgcWSTransaction;
  oList: TList{$IFDEF NEXTGEN}<TsgcWSNotifyObject>{$ENDIF};
  oObject: TObject;
begin
  oTransaction := GetTransaction(aId);

  if Assigned(oTransaction) then
  begin
    oTransaction.Active := False;

    oList := oTransaction.Queue.LockList;
    Try
      while oList.Count > 0 do
      begin
        aMessage.Read(TsgcWSNotifyObject(oList.Items[0]).Text);
        DoProcessMessage(TsgcWSNotifyObject(oList.Items[0]).Connection,
          TsgcWSNotifyObject(oList.Items[0]).Text);

        oObject := TObject(oList.Items[0]);
        sgcFree(oObject);
        oList.Delete(0);
      end;
    Finally
      oTransaction.Queue.UnlockList;
    End;

    DoDeleteTransaction(aId);
  end;
end;

procedure TsgcWSProtocol_sgc_Server.DoEventConnect(aConnection:
    TsgcWSConnection);
var
  vGuid: String;
begin
  vGuid := aConnection.Guid;
  // ... send session id
  DoSession(aConnection);
  // ... process queued messages
  DoProcessQueuedMessages('', aConnection);
  // ... notify connection only if not Assigned Broker
  if not Assigned(Broker) then
    inherited;
end;

procedure TsgcWSProtocol_sgc_Server.DoEventMessage(aConnection:
    TsgcWSConnection; const Text: string);
var
  oMessage: TsgcWSMessage;
  vId: String;
begin
  {$IFDEF SGC_DEBUG}
  DoLog(self, aConnection, 'DoEventMessage', '[Message]: ' + Text);
  {$ENDIF}

  if DoRawMessage(aConnection, Text) then exit;

  oMessage := GetWSMessageByConnection(aConnection);
  oMessage.Read(Text);
  vId := oMessage.id;

  if (oMessage.QoS = CS_QOS_LEVEL2) and (vId <> '') then
  begin
    if oMessage.method = CS_SGC_PUBREL then
      DoQoSLevel2PubRel(aConnection, vId)
    else if oMessage.method <> CS_SGC_ACKNOWLEDGMENT  then
      DoQoSLevel2Publish(vId, aConnection, Text)
    else if oMessage.method = CS_SGC_ACKNOWLEDGMENT then
      DoProcessAcknowledgment(aConnection.Guid, vId);
  end
  else
    DoProcess(aConnection, Text);
end;

procedure TsgcWSProtocol_sgc_Server.DoRollBack(const aId: String);
var
  oTransaction: TsgcWSTransaction;
  oList: TList{$IFDEF NEXTGEN}<TsgcWSNotifyObject>{$ENDIF};
  oObject: TObject;
begin
  oTransaction := GetTransaction(aId);

  if Assigned(oTransaction) then
  begin
    oList := oTransaction.Queue.LockList;
    Try
      while oList.Count > 0 do
      begin
        oObject := TObject(oList.Items[0]);
        sgcFree(oObject);
        oList.Delete(0);
      end;
    Finally
      oTransaction.Queue.UnlockList;
    End;

    DoDeleteTransaction(aId);
  end;
end;

function TsgcWSProtocol_sgc_Server.DoRPCAuthentication
  (aConnection: TsgcWSConnection; const aMethod: string): Boolean;
begin
  Result := False;

  if RPCAuthentication.Enabled then
  begin
    if RPCAuthentication.Methods.IndexOf(aMethod) <> -1 then
      Result := True
    else if RPCAuthentication.Methods.IndexOf
      (aMethod + '=' + TsgcWSConnectionServer(aConnection)
        .Authentication.User) <> -1 then
      Result := True;
  end
  else
    Result := True;

  if not result then
    DoEventRPCAuthentication(aConnection, aMethod, Result);
end;

procedure TsgcWSProtocol_sgc_Server.DoSession(const aConnection:
    TsgcWSConnection);
var
  oMessage: TsgcWSMessage;
  vText: string;
begin
  oMessage := TsgcWSMessage.Create(nil);
  Try
    oMessage.DoEnterWrite(True);
    Try
      // ... session id
      oMessage.id := aConnection.Guid;
      oMessage.method := CS_SGC_SESSION;
      oMessage.result := CS_SGC_SESSION;
      oMessage.Text := aConnection.Guid;
      oMessage.QoS := TwsQoS_String[Ord(QoS.Level)];

      vText := oMessage.Write;
      DoWriteMessageText(aConnection, vText);
      DoQueueQoS(aConnection.Guid, aConnection.Guid, vText);
    Finally
      oMessage.DoLeaveWrite;
    End;
  Finally
    sgcFree(oMessage);
  End;
end;

procedure TsgcWSProtocol_sgc_Server.DoStartTransaction(const aId: String);
begin
  if not InTransaction(aId) then
    Transactions.Add(aId);
end;

function TsgcWSProtocol_sgc_Server.GetMethodId(const aId: string):
    TsgcWSConnection;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
  oMethodId: TsgcWSMethodId;
begin
  result := nil;

  case GetMethodsId.ListType of
    tdlThreadDictionary, tdlOnlyDictionary:
      begin
        oMethodId := TsgcWSMethodId(GetMethodsId.GetValue(aId));
        if Assigned(oMethodId) then
          result := oMethodId.Connection;
      end;
    tdlOnlyThread:
      begin
        oList := GetMethodsId.LockList;
        Try
          for i := 0 to oList.Count - 1 do
          begin
            if TsgcWSMethodId(oList[i]).Id = aId then
            begin
              result := TsgcWSMethodId(oList[i]).Connection;
              break;
            end;
          end;
        Finally
          GetMethodsId.UnlockList;
        End;
      end;
  end;
end;

function TsgcWSProtocol_sgc_Server.GetMethodsId: TsgcThreadListMethodsId;
begin
  if not Assigned(FMethodsId) then
  begin
    FMethodsId := TsgcThreadListMethodsId.Create;
    FMethodsId.ListType := tdlThreadDictionary;
  end;
  Result := FMethodsId;
end;


function TsgcWSProtocol_sgc_Server.GetTransactions: TsgcWSTransactions;
begin
  if not Assigned(FTransactions) then
    FTransactions := TsgcWSTransactions.Create;
  Result := FTransactions;
end;

function TsgcWSProtocol_sgc_Server.InTransaction(const aId: String): Boolean;
var
  oTransaction: TsgcWSTransaction;
begin
  result := False;

  oTransaction := GetTransaction(aId);
  if Assigned(oTransaction) then
    result := oTransaction.Active;
end;

procedure TsgcWSProtocol_sgc_Server.Publish(const aMessage, aChannel: String;
    const aExclude: String = ''; const aInclude: String = ''; const aQueue:
    TwsQueue = queueLevel0);
begin
  DoPublish(aMessage, aChannel, aExclude, aInclude, aQueue);
end;

function TsgcWSProtocol_sgc_Server.RemoveMethodId(const aId: string): Boolean;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
  oMethodId: TsgcWSMethodId;
begin
  result := False;

  oList := GetMethodsId.LockList;
  Try
    for i := 0 to oList.Count - 1 do
    begin
      if TsgcWSMethodId(oList[i]).Id = aId then
      begin
        result := True;
        oMethodId := TsgcWSMethodId(oList[i]);
        GetMethodsId.DeleteKey(i, oMethodId.ID);
        sgcFree(oMethodId);
        break;
      end;
    end;
  Finally
    GetMethodsId.UnlockList;
  End;
end;

procedure TsgcWSProtocol_sgc_Server.RPCError(aId: String; aCode: Integer;
    aMessage: String; aData: String = '');
var
  oMessage: TsgcWSMessage;
  oConnection: TsgcWSConnection;
  vText: String;
begin
  oConnection := GetMethodId(aId);
  if Assigned(oConnection) then
  begin
    RemoveMethodId(aId);

    oMessage := TsgcWSMessage.Create(nil);
    Try
      oMessage.DoEnterWrite(True);
      Try
        oMessage.ID := aId;
        oMessage.errorcode := aCode;
        oMessage.errormessage := aMessage;
        oMessage.errordata := aData;
        oMessage.QoS := TwsQoS_String[Ord(QoS.Level)];

        vText := oMessage.Write;
        DoWriteMessageText(oConnection, vText);
        DoQueueQoS(oConnection.Guid, aId, vText);
      Finally
        oMessage.DoLeaveWrite;
      End;
    Finally
      sgcFree(oMessage);
    End;
  end;
end;

procedure TsgcWSProtocol_sgc_Server.RPCResult(aId, aResult: String);
var
  oMessage: TsgcWSMessage;
  oConnection: TsgcWSConnection;
  vText: String;
begin
  oConnection := GetMethodId(aId);
  if Assigned(oConnection) then
  begin
    RemoveMethodId(aId);

    oMessage := TsgcWSMessage.Create(nil);
    Try
      oMessage.DoEnterWrite(True);
      Try
        oMessage.ID := aId;
        oMessage.Result := aResult;
        oMessage.QoS := TwsQoS_String[Ord(QoS.Level)];

        vText := oMessage.Write;
        DoWriteMessageText(oConnection, vText);
        DoQueueQoS(oConnection.Guid, aId, vText);
      Finally
        oMessage.DoLeaveWrite;
      End;
    Finally
      sgcFree(oMessage);
    End;
  end;
end;

procedure TsgcWSProtocol_sgc_Server.SetRPCAuthentication(const Value:
    TsgcWSAuthentication_Methods);
begin
  FRPCAuthentication := Value;
end;

function TsgcWSProtocol_JS_sgc.GetResponse: string;
begin
  Result := GetResourceString(CS_SGC_JS_ESEGECE_COM);
end;

class function TsgcWSProtocol_JS_sgc.GetFileName: string;
begin
  Result := CS_JS_ESEGECE_COM;
end;

class function TsgcWSProtocol_HTML_sgc.GetFileName: string;
begin
  Result := CS_HTML_ESEGECE_COM;
end;

function TsgcWSProtocol_HTML_sgc.GetResponse: string;
begin
  Result := GetResourceString(CS_SGC_HTML_ESEGECE_COM);
end;

constructor TsgcWSAuthentication_Methods.Create;
begin
  inherited;
  FMethods := TStringList.Create;
end;

destructor TsgcWSAuthentication_Methods.Destroy;
begin
  sgcFree(FMethods);
  inherited;
end;

procedure TsgcWSAuthentication_Methods.SetMethods(const Value: TStringList);
begin
  FMethods.Assign(Value);
end;

constructor TsgcWSTransaction.Create;
begin
  inherited;
  Active := True;
end;

destructor TsgcWSTransaction.Destroy;
begin
  sgcFree(FQueue);
  inherited;
end;

function TsgcWSTransaction.GetQueue: TsgcWSObjectList;
begin
  if not Assigned(FQueue) then
    FQueue := TsgcWSObjectList.Create;
  Result := FQueue;
end;

procedure TsgcWSTransaction.SetActive(const Value: Boolean);
begin
  FActive := Value;
end;

procedure TsgcWSTransactions.Add(const ID: string);
var
  oTransaction: TsgcWSTransaction;
begin
  oTransaction := TsgcWSTransaction.Create;
  oTransaction.ID := ID;
  Add(oTransaction);
end;

function TsgcWSProtocol_sgc_Server.ClearQueue(const aChannel: String): Boolean;
var
  oQueuedMessage: TsgcWSQueuedMessage;
begin
  result := False;

  oQueuedMessage := GetQueuedMessage(aChannel);
  if Assigned(oQueuedMessage) then
  begin
    oQueuedMessage.Queue.DeleteAll;
    result := True;
  end;
end;

procedure TsgcWSProtocol_sgc_Server.DoAcknowledgment(const aConnection:
    TsgcWSConnection; const aId: string);
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

    DoWriteMessageText(aConnection, oJSON.Text);
  Finally
    FreeJSONInstance(oJSON);
  End;
end;

procedure TsgcWSProtocol_sgc_Server.DoBroadcastAcknowledgment(aMessage:
    TsgcWSMessage; const aChannel: string = ''; const Exclude: String = '';
    const Include: String = '');
var
  i: Integer;
  oConnection: TsgcWSConnection;
  oConnections: TList{$IFDEF NEXTGEN}<TsgcWSConnection>{$ENDIF};
  vId: String;
  vMessage: String;
begin
  oConnection := nil;
  vMessage := aMessage.Write;
  vId := aMessage.id;

  oConnections := TList{$IFDEF NEXTGEN}<TsgcWSConnection>{$ENDIF}.Create;
  Try
    GetBroadcastConnections(aChannel, Exclude, Include, oConnections);
    for i := 0 to oConnections.Count - 1 do
    begin
      Try
        oConnection := TsgcWSConnection(oConnections[i]);
        if Assigned(oConnection) then
        begin
          if oConnection.Disconnected = False then
          begin
            DoWriteMessageText(oConnection, vMessage);
            DoQueueQoS(oConnection.Guid, vId, vMessage);
          end;
        end;
      Except
        On E: Exception do
          DoError(oConnection, E.Message);
      end;
    end;
  Finally
    sgcFree(oConnections);
  End;
end;

procedure TsgcWSProtocol_sgc_Server.DoClear(aConnection: TsgcWSConnection);
begin
  inherited;
  // ... delete qos by connection.guid
  DoDeleteQoSByGuid(aConnection.Guid);
end;

procedure TsgcWSProtocol_sgc_Server.DoDeleteQoSByGuid(const aGuid: String);
var
  i: Integer;
begin
  case QoS.Level of
    qosLevel1, qosLevel2:
      begin
        DoEnterCS(CS_QOS_LIST);
        Try
          for i := QoSList.Count - 1 Downto 0 do
          begin
            if TsgcWSQoSItem(QoSList[i]).Guid = aGuid then
              QoSList.Delete(i);
          end;
        Finally
          DoLeaveCS(CS_QOS_LIST);
        End;
      end;
  end;
end;

procedure TsgcWSProtocol_sgc_Server.DoDeleteTransaction(const aId: String);
var
  i: Integer;
begin
  DoEnterCS(CS_TRANSACTIONS);
  Try
    for i := 0 to Transactions.Count - 1 do
    begin
      if TsgcWSTransaction(Transactions.Items[i]).ID = aId then
      begin
        Transactions.Delete(i);
        break;
      end;
    end;
  Finally
    DoLeaveCS(CS_TRANSACTIONS);
  End;
end;

procedure TsgcWSProtocol_sgc_Server.DoEventAcknowledgment(aConnection:
    TsgcWSConnection; const Id: String);
begin
  if Assigned(FOnAcknowledgment) then
    FOnAcknowledgment(aConnection, Id);
end;

procedure TsgcWSProtocol_sgc_Server.DoEventBeforePublish(aConnection:
    TsgcWSConnection; const aChannel: String; var aText: String; var Accept:
    Boolean);
begin
  if Assigned(FOnBeforePublish) then
    FOnBeforePublish(aConnection, aChannel, aText, Accept);
end;

procedure TsgcWSProtocol_sgc_Server.DoEventDisconnect(aConnection:
    TsgcWSConnection; Code: Integer);
begin
  inherited;
  // ... delete qos by connection.guid
  DoDeleteQoSByGuid(aConnection.Guid);
  // ... Unsubscribe
  DoUnSubscriptions(aConnection);
end;

procedure TsgcWSProtocol_sgc_Server.DoEventNotification(aConnection:
    TsgcWSConnection; const aMessage: TsgcWSMessage);
var
  vMethod: string;
  vParams: string;
begin
  if Assigned(FOnNotification) then
  begin
    vMethod := aMessage.Method;
    vParams := aMessage.Params;

    FOnNotification(aConnection, vMethod, vParams);
  end;
end;

procedure TsgcWSProtocol_sgc_Server.DoEventRPC(aConnection: TsgcWSConnection;
    const aMessage: TsgcWSMessage);
var
  vId: String;
  vMethod: String;
  vParams: String;
begin
  if Assigned(FOnRPC) then
  begin
    vId := aMessage.Id;
    vMethod := aMessage.Method;
    vParams := aMessage.Params;

    FOnRPC(aConnection, vId, vMethod, vParams);
  end;
end;

procedure TsgcWSProtocol_sgc_Server.DoEventRPCAuthentication(aConnection:
    TsgcWSConnection; const aMethod: string; var Result: Boolean);
var
  vMethod: string;
  vPassword: string;
  vUser: string;
begin
  if Assigned(FOnRPCAuthentication) then
  begin
    vMethod := aMethod;
    vUser := TsgcWSConnectionServer(aConnection).Authentication.User;
    vPassword := TsgcWSConnectionServer(aConnection).Authentication.Password;

    FOnRPCAuthentication(aConnection, aMethod, vUser, vPassword, Result);
  end;
end;

procedure TsgcWSProtocol_sgc_Server.DoFinalize(aConnection: TsgcWSConnection);
begin
  inherited;
  // ... Unsubscribe
  DoUnSubscriptions(aConnection);
end;

procedure TsgcWSProtocol_sgc_Server.DoInitialize(aConnection: TsgcWSConnection);
var
  vGuid: string;
begin
  inherited;
  vGuid := aConnection.Guid;

  // ... send session id
  DoSession(aConnection);

  // ... process queued messages
  DoProcessQueuedMessages('', aConnection);
end;

procedure TsgcWSProtocol_sgc_Server.DoProcessMessage(aConnection:
    TsgcWSConnection; const aText: String);
var
  i: Integer;
  vAccept: Boolean;
  vChannel: string;
  vText: string;
  oMessage: TsgcWSMessage;
  oChannels: TStringList;
  vMethod: string;
  vParams: string;
  vId: String;
begin
  oMessage := GetWSMessageByConnection(aConnection);
  oMessage.Read(aText);

  vId := oMessage.id;
  vText := oMessage.Text;
  vChannel := oMessage.Channel;
  // ... methods
  if oMessage.method = CS_SGC_MESSAGE then
    inherited DoEventMessage(aConnection, vText)
  else if oMessage.method = CS_SGC_TRANSACTION then
    DoStartTransaction(aConnection.Guid + '_' + vChannel)
  else if oMessage.Method = CS_SGC_SESSION then
    DoSession(aConnection)
  else if oMessage.method = CS_SGC_ACKNOWLEDGMENT then
  begin
    DoProcessAcknowledgment(aConnection.Guid, vId);
    DoEventAcknowledgment(aConnection, vId);
  end
  else if oMessage.Method = CS_SGC_PUBLISH then
  begin
    // ... OnBeforeSubscription
    vAccept := True;
    DoEventBeforePublish(aConnection, vChannel, vText, vAccept);
    if vAccept then
    begin
      if oMessage.Queue = CS_QUEUE_LEVEL1 then
        DoPublish(vText, vChannel, '', '', queueLevel1)
      else if oMessage.Queue = CS_QUEUE_LEVEL2 then
        DoPublish(vText, vChannel, '', '', queueLevel2)
      else
        DoPublish(vText, vChannel, '', '', queueLevel0);
    end;
  end
  else if oMessage.Method = CS_SGC_BROADCAST then
  begin
    oMessage.Result := CS_SGC_BROADCAST;
    DoBroadCast(oMessage, vChannel)
  end
  else if oMessage.Method = CS_SGC_PROTOCOL then
    DoWriteMessageText(aConnection, aConnection.Protocol)
  else if oMessage.Method = CS_SGC_SUBSCRIBE then
  begin
    vMethod := oMessage.method;
    vParams := oMessage.params;
    // ... get channels
    oChannels := TStringList.Create;
    Try
      GetChannelsFromSubscriptions(vChannel, oChannels);
      // ... process oChannels
      for i := 0 to oChannels.Count - 1 do
      begin
        vChannel := oChannels[i];

        oMessage.method := vMethod;
        oMessage.Channel := vChannel;
        oMessage.params := vParams;
        // ... OnBeforeSubscription
        vAccept := True;
        DoEventBeforeSubscription(aConnection, vChannel, vAccept);
        if vAccept then
        begin
          // ... subscribe
          aConnection.DoSubscribe(guid + '_' + vChannel);
          if aConnection.Subscribed(guid + '_' + vChannel) then
          begin
            aConnection.LastSubscription := vChannel;
            DoNotifySubscription(aConnection);
            oMessage.Result := CS_SGC_SUBSCRIBE;

            DoWriteMessageText(aConnection, oMessage.Write);
          end;
          // ... process queued messages
          DoProcessQueuedMessages(vChannel, aConnection);
        end;
      end;
    Finally
      sgcFree(oChannels);
    End;
  end
  else if oMessage.Method = CS_SGC_UNSUBSCRIBE_ALL then
    DoUnSubscriptions(aConnection)
  else if oMessage.Method = CS_SGC_UNSUBSCRIBE then
  begin
    vMethod := oMessage.method;
    vParams := oMessage.params;
    // ... get channels
    oChannels := TStringList.Create;
    Try
      GetChannelsFromSubscriptions(vChannel, oChannels);
      // ... process oChannels
      for i := 0 to oChannels.Count - 1 do
      begin
        vChannel := oChannels[i];
        oMessage.method := vMethod;
        oMessage.Channel := vChannel;
        oMessage.params := vParams;
        // ... unsubscribe
        aConnection.DoUnsubscribe(guid + '_' + vChannel);
        if not aConnection.Subscribed(guid + '_' + vChannel) then
        begin
          aConnection.LastUnSubscription := vChannel;
          DoNotifyUnSubscription(aConnection);
          oMessage.Result := CS_SGC_UNSUBSCRIBE;

          DoWriteMessageText(aConnection, oMessage.Write);
        end;
      end;
    Finally
      sgcFree(oChannels);
    End;
  end
  else
  begin
    // ... notification
    if vId = '' then
      DoEventNotification(aConnection, oMessage)
    else
    // ... rpc
    begin
      vMethod := oMessage.method;
      // ... process
      AddMethodID(aConnection, vId);
      if DoRPCAuthentication(aConnection, vMethod) then
        DoEventRPC(aConnection, oMessage)
      else
        RPCError(oMessage.ID, CS_JSONRPC_METHOD_NOT_FOUND,
          S_JSONRPC_METHOD_NOT_FOUND);
    end;
  end;
end;

procedure TsgcWSProtocol_sgc_Server.DoProcess(const aConnection:
    TsgcWSConnection; const aText: String);
var
  oMessage: TsgcWSMessage;
begin
  oMessage := GetWSMessageByConnection(aConnection);
  oMessage.Read(aText);

  if ((oMessage.id <> '') and (oMessage.method <> CS_SGC_ACKNOWLEDGMENT)) then
    DoAcknowledgment(aConnection, oMessage.id);

  // ... transactions
  if oMessage.method = CS_SGC_COMMIT then
    DoCommit(oMessage, aConnection.Guid + '_' + oMessage.Channel)
  else if oMessage.method = CS_SGC_ROLLBACK then
    DoRollBack(aConnection.Guid + '_' + oMessage.Channel)
  else if DoQueueTransaction(aConnection, aText, oMessage) then
    exit;

  DoProcessMessage(aConnection, aText);
end;

procedure TsgcWSProtocol_sgc_Server.DoProcessAcknowledgment(const aGuid, aId:
    String);
var
  i: Integer;
begin
  if QoS.Level <> qosLevel0 then
  begin
    DoEnterCS(CS_QOS_LIST);
    Try
      for i := QoSList.Count - 1 Downto 0 do
      begin
        if ((TsgcWSQoSItem(QoSList.Items[i]).guid = aGuid) and (TsgcWSQoSItem(QoSList.Items[i]).ID = aId)) then
          QoSList.Delete(i);
      end;
    Finally
      DoLeaveCS(CS_QOS_LIST);
    End;
  end;
end;

procedure TsgcWSProtocol_sgc_Server.DoProcessQueuedMessages(const aId: String;
    aConnection: TsgcWSConnection);
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TsgcWSNotifyObject>{$ENDIF};
  oQueuedMessage: TsgcWSQueuedMessage;
  oMessage: TsgcWSMessage;
begin
  oQueuedMessage := GetQueuedMessage(aId);

  if Assigned(oQueuedMessage) then
  begin
    oList := oQueuedMessage.Queue.LockList;
    Try
      for i := 0 to oList.Count - 1 do
      begin
        oMessage := GetWSMessageByConnection(aConnection);
        oMessage.Read(TsgcWSNotifyObject(oList.Items[i]).Text);
        oMessage.method := CS_SGC_EVENT;
        oMessage.Result := CS_SGC_EVENT;
        DoBroadCast(oMessage, oMessage.Channel, '', aConnection.Guid);
      end;
    Finally
      oQueuedMessage.Queue.UnlockList;
    End;
  end;
end;

procedure TsgcWSProtocol_sgc_Server.DoPublish(const aMessage, aChannel,
    aExclude, aInclude: String; const aQueue: TwsQueue);
var
  i: Integer;
  oChannels: TStringList;
  oMessage: TsgcWSMessage;
begin
  oChannels := TStringList.Create;
  Try
    GetChannelsFromSubscriptions(aChannel, oChannels);
    // ... process oChannels
    for i := 0 to oChannels.Count - 1 do
    begin
      oMessage := TsgcWSMessage.Create(nil);
      Try
        oMessage.DoEnterWrite(True);
        Try
          // ... queue messages if needed
          oMessage.method := CS_SGC_EVENT;
          oMessage.result := CS_SGC_EVENT;
          oMessage.Text := aMessage;
          oMessage.Channel := oChannels[i];
          case aQueue of
            queueLevel1: DoQueueMessageLevel1(nil, oMessage.Write, oChannels[i]);
            queueLevel2: DoQueueMessageLevel2(nil, oMessage.Write, oChannels[i]);
          end;

          // ... broadcast
          oMessage.method := CS_SGC_EVENT;
          oMessage.result := CS_SGC_EVENT;
          oMessage.Text := aMessage;
          oMessage.Channel := oChannels[i];
          DoBroadCast(oMessage, oChannels[i], aExclude, aInclude);
        Finally
          oMessage.DoLeaveWrite;
        End;
      Finally
        sgcFree(oMessage);
      End;
    end;
  Finally
    sgcFree(oChannels);
  End;
end;

procedure TsgcWSProtocol_sgc_Server.DoQoSLevel2Publish(const aId: String;
    aConnection: TsgcWSConnection; const aText: String);
begin
  DoEnterCS(CS_QOS_LEVEL2_LIST);
  Try
    // ... delete if already exists
    QosLevel2List.Delete(aId);

    QoSLevel2List.Add(aId, aConnection, aText);
  Finally
    DoLeaveCS(CS_QOS_LEVEL2_LIST);
  End;

  // ... response
  DoQoSLevel2PubRec(aConnection, aId);
end;

procedure TsgcWSProtocol_sgc_Server.DoQoSLevel2PubRec(const aConnection:
    TsgcWSConnection; const aID: String);
var
  oMessage: TsgcWSMessage;
begin
  oMessage := TsgcWSMessage.Create(nil);
  Try
    oMessage.DoEnterWrite(True);
    Try
      // ... pubrec
      oMessage.method := CS_SGC_PUBREC;
      oMessage.result := CS_SGC_PUBREC;
      oMessage.Guid := aConnection.Guid;
      oMessage.id := aId;

      DoWriteMessageText(aConnection, oMessage.Write);
    Finally
      oMessage.DoLeaveWrite;
    End;
  Finally
    sgcFree(oMessage);
  End;
end;

procedure TsgcWSProtocol_sgc_Server.DoQoSLevel2PubRel(const aConnection:
    TsgcWSConnection; const aId: String);
var
  i: Integer;
  oLevel2: TsgcWSQoSLevel2;
begin
  DoEnterCS(CS_QOS_LEVEL2_LIST);
  Try
    i := QoSLevel2List.Item(aId);
    if i > -1 then
    begin
      oLevel2 := TsgcWSQoSLevel2(QoSLevel2List.Items[i]);
      DoProcess(oLevel2.NotifyObject.Connection, oLevel2.NotifyObject.Text);
      QoSLevel2List.Delete(aId);
    end;
  Finally
    DoLeaveCS(CS_QOS_LEVEL2_LIST);
  End;

  if i = -1 then
    DoAcknowledgment(aConnection, aId);
end;

procedure TsgcWSProtocol_sgc_Server.DoQoSList;
var
  i: integer;
begin
  for i := 0 to QoSList.Count - 1 do
  begin
    if Trunc((Now - TsgcWSQoSItem(QoSList.Items[i]).Date) * 86400) > QoS.Timeout then
      inherited WriteData(TsgcWSQoSItem(QoSList.Items[i]).Guid, TsgcWSQoSItem(QoSList.Items[i]).Text);
  end;
end;

function TsgcWSProtocol_sgc_Server.DoQueueMessageLevel1(aConnection:
    TsgcWSConnection; aText, aChannel: String): Boolean;
var
  oQueuedMessage: TsgcWSQueuedMessage;
begin
  result := False;

  oQueuedMessage := GetQueuedMessage(aChannel);
  if Assigned(oQueuedMessage) then
  begin
    oQueuedMessage.Queue.DeleteAll;

    oQueuedMessage.Queue.AddNotifyObject(aConnection, aText);
    result := True;
  end;
end;

function TsgcWSProtocol_sgc_Server.DoQueueMessageLevel2(aConnection:
    TsgcWSConnection; aText, aChannel: String): Boolean;
var
  oQueuedMessage: TsgcWSQueuedMessage;
begin
  result := False;

  oQueuedMessage := GetQueuedMessage(aChannel);
  if Assigned(oQueuedMessage) then
  begin
    oQueuedMessage.Queue.AddNotifyObject(aConnection, aText);
    result := True;
  end;
end;

procedure TsgcWSProtocol_sgc_Server.DoQueueQoS(const aGuid, aId, aText: string);
var
  oItem: TsgcWSQoSItem;
begin
  if aId = '' then exit;

  case QoS.Level of
    qosLevel1, qosLevel2:
      begin
        DoEnterCS(CS_QOS_LIST);
        Try
          oItem := TsgcWSQoSItem.Create;
          oItem.Guid := aGuid;
          oItem.ID := aId;
          oItem.Text := aText;
          oItem.Date := Now;
          QoSList.Add(oItem)
        Finally
          DoLeaveCS(CS_QOS_LIST);
        End;
      end;
  end;
end;

function TsgcWSProtocol_sgc_Server.DoQueueTransaction(aConnection:
    TsgcWSConnection; aText: String; aMessage: TsgcWSMessage): Boolean;
var
  oTransaction: TsgcWSTransaction;
begin
  result := False;

  oTransaction := GetTransaction(aConnection.Guid + '_' + aMessage.Channel);
  if Assigned(oTransaction) then
  begin
    if oTransaction.Active then
    begin
      oTransaction.Queue.AddNotifyObject(aConnection, aText);
      result := True;
    end;
  end;
end;

procedure TsgcWSProtocol_sgc_Server.DoStartQoS;
begin
  if QoS.Level <> qosLevel0 then
  begin
    if not Assigned(FQoSTimer) then
    begin
      FQoSTimer := TsgcTimer.Create;
      FQoSTimer.DebugName := 'QoS Timer';
      if Assigned(Server) then
        FQoSTimer.NotifyEvents := THackComponent_Server(Server).NotifyEvents;
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

procedure TsgcWSProtocol_sgc_Server.DoStopQoS;
begin
  sgcThreadFree(FQoSTimer);
end;

function TsgcWSProtocol_sgc_Server.DoWriteMessageText(const aGuid, aMessage:
    string): Boolean;
begin
  result := inherited WriteData(aGuid, aMessage);
end;

function TsgcWSProtocol_sgc_Server.DoWriteMessageText(const aConnection:
    TsgcWSConnection;const aMessage: string): Boolean;
begin
  Result := inherited WriteData(aConnection, aMessage);
end;

function TsgcWSProtocol_sgc_Server.GetQoSLevel2List: TsgcWSQoSLevel2List;
begin
  if not Assigned(FQoSLevel2List) then
    FQoSLevel2List := TsgcWSQoSLevel2List.Create;
  Result := FQoSLevel2List;
end;

function TsgcWSProtocol_sgc_Server.GetQoSList: TsgcWSQoSList;
begin
  if not Assigned(FQoSList) then
    FQoSList := TsgcWSQoSList.Create;
  Result := FQoSList;
end;

function TsgcWSProtocol_sgc_Server.GetQueuedMessage(const aId: String):
    TsgcWSQueuedMessage;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to QueuedMessages.Count - 1 do
  begin
    if TsgcWSQueuedMessage(QueuedMessages.Items[i]).ID = aId then
    begin
      result := TsgcWSQueuedMessage(QueuedMessages.Items[i]);
      break;
    end;
  end;

  if not Assigned(result) then
    result := QueuedMessages.Add(aId);
end;

function TsgcWSProtocol_sgc_Server.GetQueuedMessages: TsgcWSQueuedMessages;
begin
  if not Assigned(FQueuedMessages) then
    FQueuedMessages := TsgcWSQueuedMessages.Create;
  Result := FQueuedMessages;
end;

function TsgcWSProtocol_sgc_Server.GetTransaction(const aId: String):
    TsgcWSTransaction;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to Transactions.Count - 1 do
  begin
    if TsgcWSTransaction(Transactions.Items[i]).ID = aId then
    begin
      result := TsgcWSTransaction(Transactions.Items[i]);
      break;
    end;
  end;
end;

function TsgcWSProtocol_sgc_Server.GetWSMessageByConnection(const aConnection:
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


procedure TsgcWSProtocol_sgc_Server.Loaded;
begin
  inherited;
  DoStartQoS;
end;

procedure TsgcWSProtocol_sgc_Server.OnQoSEvent(Sender: TObject);
begin
  DoQoSList;
end;

procedure TsgcWSProtocol_sgc_Server.OnQoSExceptionEvent(Sender: TObject; E:
    Exception);
begin
  DoNotifyException(E.Message, E);
end;

procedure TsgcWSProtocol_sgc_Server.Publish(const aMessage, aChannel: String;
    const aQueue: TwsQueue);
begin
  DoPublish(aMessage, aChannel, '', '', aQueue);
end;

procedure TsgcWSProtocol_sgc_Server.SetQoS(const Value: TsgcWSQoS_Options);
begin
  FQoS.Assign(Value);
end;

function TsgcWSProtocol_sgc_Server.WriteData(aGuid, aMessage: string): Boolean;
var
  oMessage: TsgcWSMessage;
begin
  oMessage := TsgcWSMessage.Create(nil);
  Try
    oMessage.DoEnterWrite(True);
    Try
      // ... pubrec
      oMessage.method := CS_SGC_MESSAGE;
      oMessage.Text := aMessage;
      oMessage.Guid := aGuid;
      oMessage.params := ''; // ... prevent params from other message
      result := DoWriteMessageText(aGuid, oMessage.Write);
    Finally
      oMessage.DoLeaveWrite;
    End;
  Finally
    sgcFree(oMessage);
  End;
end;


function TsgcWSQoSLevel2List.Add(const aID: string; const aConnection:
    TsgcWSConnection; const aText: String): TsgcWSQoSLevel2;
begin
  result := TsgcWSQoSLevel2.Create;
  result.ID := aID;
  result.NotifyObject.Connection := aConnection;
  result.NotifyObject.Text := aText;
  Add(result);
end;

procedure TsgcWSQoSLevel2List.Delete(const aID: string);
var
  i: Integer;
begin
  for i := Count - 1 downto 0 do
  begin
    if TsgcWSQosLevel2(Items[i]).ID = aId then
    begin
      inherited Delete(i);
      break;
    end;
  end;
end;

function TsgcWSQoSLevel2List.Item(const aId: string): Integer;
var
  i: Integer;
begin
  result := -1;
  
  for i := 0 to Count - 1 do
  begin
    if TsgcWSQosLevel2(Items[i]).ID = aId then
    begin
      result := i;
      break;
    end;
  end;
end;

destructor TsgcWSQoSLevel2.Destroy;
begin
  sgcFree(FNotifyObject);
  inherited;
end;

function TsgcWSQoSLevel2.GetNotifyObject: TsgcWSNotifyObject;
begin
  if not Assigned(FNotifyObject) then
    FNotifyObject := TsgcWSNotifyObject.Create;
  Result := FNotifyObject;
end;

destructor TsgcWSQueuedMessage.Destroy;
begin
  sgcFree(FQueue);
  inherited;
end;

function TsgcWSQueuedMessage.GetQueue: TsgcWSObjectList;
begin
  if not Assigned(FQueue) then
    FQueue := TsgcWSObjectList.Create;
  Result := FQueue;
end;

function TsgcWSQueuedMessages.Add(const ID: string): TsgcWSQueuedMessage;
begin
  result := TsgcWSQueuedMessage.Create;
  result.ID := ID;
  Add(result);
end;

initialization
Classes.RegisterClass(TsgcWSProtocol_sgc_Server);
TsgcWSHTTPResponse.RegisterfileName(TsgcWSProtocol_JS_sgc);
TsgcWSHTTPResponse.RegisterfileName(TsgcWSProtocol_HTML_sgc);

finalization;

{$ENDIF}

end.
