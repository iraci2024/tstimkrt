{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcWebSocket_Protocol_WAMP_Server;

interface

{$I sgcVer.inc}

{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils,
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  {$IFDEF NEXTGEN}System.Generics.Collections, {$ELSE}Contnrs, {$ENDIF}
  // sgc
  sgcJSON, sgcWebSocket_Protocol_WAMP_Message,
  sgcWebSocket_Classes, sgcWebSocket_Protocol_Base_Server,
  sgcWebSocket_HTTPResponse, sgcWebSocket_Classes_SyncObjs;

type

  TsgcWSPrefixEvent = procedure(Connection: TsgcWSConnection; const Prefix, URI:
      string) of object;
  TsgcWSCallEvent = procedure(Connection: TsgcWSConnection; const CallId, ProcUri, Arguments:
      string) of object;
  TsgcWSBeforeCancelCallEvent = procedure(Connection: TsgcWSConnection; const
      CallId: string; var Cancel: Boolean) of object;

  TsgcWSCallId = class
  private
    FGuid: String;
    FId: String;
  public
    property Guid: String read FGuid write FGuid;
    property Id: String read FId write FId;
  end;

  TsgcThreadListCallsId = class(TsgcThreadList{$IFDEF NEXTGEN}<TObject>{$ENDIF})
  end;

  TsgcWSProtocol_WAMP_Server = class(TsgcWSProtocol_Subscription_Server_Base)
    { wsmessage }
  private
    FWSMessageId: String;
  protected
    function GetWSMessageByConnection(const aConnection: TsgcWSConnection):
        TsgcWSMessageWAMP; virtual;
    { wsmessage }

    { from TsgcWSProtocol }
  protected
    procedure DoInitialize(aConnection: TsgcWSConnection); override;
    procedure DoFinalize(aConnection: TsgcWSConnection); override;
    procedure DoClear(aConnection: TsgcWSConnection); override;
    { from TsgcWSProtocol }

  { from TsgcWSComponent }
  protected
    procedure DoEventConnect(aConnection: TsgcWSConnection); override;
    procedure DoEventMessage(aConnection: TsgcWSConnection; const Text: string);
        override;
    procedure DoEventDisconnect(aConnection: TsgcWSConnection; Code: Integer);
        override;
  { from TsgcWSComponent }

  { call }
  private
    FCallId: TsgcThreadListCallsId;
  protected
    function GetCallsId: TsgcThreadListCallsId;
    function AddCallId(const aGuid, aCallId: string): Boolean;
    function RemoveCallId(const aCallId: string): Boolean;
    function GetCallGuid(const aCallId: string): String;
    function GetNewCallId(const aGuid, aCallId: String): String;
    function GetOriginalCallId(const aGuid, aCallId: String): String;
  { call }

  { procedures }
  protected
    procedure DoWelcome(const aGuid: string); virtual;
  public
    procedure CallResult(const aCallId: String; const aResult: String = '');
    procedure CallProgressResult(const aCallId: String; const aResult: String = '');
    procedure CallError(const aCallId, aErrorURI, aErrorDesc: String; const
        aErrorDetails: String = '');
    procedure Event(const aTopicURI: String; const aEvent: String = '');
  { procedures }

  { constructor }
  public
    constructor Create(aOwner: TComponent); override;
  { constructor }

  { events }
  private
    FOnCall: TsgcWSCallEvent;
    FOnBeforeCancelCall: TsgcWSBeforeCancelCallEvent;
    FOnPrefix: TsgcWSPrefixEvent;
  private
    function DoBeforeCancelCall(const aConnection: TsgcWSConnection; const aCallId:
        String): Boolean;
  public
    property OnCall: TsgcWSCallEvent read FOnCall write FOnCall;
    property OnBeforeCancelCall: TsgcWSBeforeCancelCallEvent read
        FOnBeforeCancelCall write FOnBeforeCancelCall;
    property OnPrefix: TsgcWSPrefixEvent read FOnPrefix write FOnPrefix;
  { events }
  end;


  TsgcWSProtocol_JS_WAMP = class(TsgcWSHTTPResponse_Base)
  protected
    function GetResponse: string; override;
  public
    class function GetFileName: string; override;
  end;


  TsgcWSProtocol_HTML_WAMP = class(TsgcWSHTTPResponse_Base)
  protected
    function GetResponse: string; override;
  public
    class function GetFileName: string; override;
  end;

{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses
  sgcWebSocket_Const, sgcWebSocket_Helpers, sgcBase_Helpers,
  sgcWebSocket_Resources;

const
  CS_CALL_ID = 0;

constructor TsgcWSProtocol_WAMP_Server.Create(aOwner: TComponent);
begin
  inherited;
  FWSMessageId := NewGuid;
  FProtocol := CS_PROTOCOL_WAMP;
end;

procedure TsgcWSProtocol_WAMP_Server.CallResult(const aCallId: String; const
    aResult: String = '');
var
  vGUID: String;
  oMessage: TsgcWSMessageWAMP;
begin
  vGUID := GetCallGuid(aCallId);

  if vGUID <> '' then
  begin
    oMessage := TsgcWSMessageWAMP.Create(nil);
    Try
      oMessage.DoEnterWrite;

      oMessage.TypeId := CS_WAMP_CALLRESULT;
      oMessage.CallId := GetOriginalCallId(vGUID, aCallId);
      oMessage.CallResult := aResult;

      WriteData(vGUID, oMessage.Write);

      RemoveCallId(aCallId);
    Finally
      sgcFree(oMessage);
    End;
  end;
end;

function TsgcWSProtocol_WAMP_Server.AddCallId(const aGuid, aCallId: string):
    Boolean;
var
  oCallId: TsgcWSCallId;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  Result := False;
  if GetCallGuid(aCallId) = '' then
  begin
    oCallId := TsgcWSCallId.Create;
    oCallId.Id := aCallId;
    oCallId.Guid := aGuid;
    oList := GetCallsId.LockList;
    Try
      oList.Add(oCallId);
    Finally
      GetCallsId.UnlockList;
    End;
  end;
end;

procedure TsgcWSProtocol_WAMP_Server.CallError(const aCallId, aErrorURI,
    aErrorDesc: String; const aErrorDetails: String = '');
var
  vGUID: String;
  oMessage: TsgcWSMessageWAMP;
begin
  vGUID := GetCallGuid(aCallId);

  if vGUID <> '' then
  begin
    oMessage := TsgcWSMessageWAMP.Create(nil);
    Try
      oMessage.DoEnterWrite;

      oMessage.TypeId := CS_WAMP_CALLERROR;
      oMessage.CallId := GetOriginalCallId(vGUID, aCallId);;
      oMessage.ErrorURI := aErrorURI;
      oMessage.ErrorDesc := aErrorDesc;
      oMessage.ErrorDetails := aErrorDetails;

      WriteData(vGUID, oMessage.Write);

      RemoveCallId(aCallId);
    Finally
      sgcFree(oMessage);
    End;
  end;
end;

procedure TsgcWSProtocol_WAMP_Server.CallProgressResult(const aCallId: String;
    const aResult: String = '');
var
  vGUID: String;
  oMessage: TsgcWSMessageWAMP;
begin
  vGUID := GetCallGuid(aCallId);

  if vGUID <> '' then
  begin
    oMessage := TsgcWSMessageWAMP.Create(nil);
    Try
      oMessage.DoEnterWrite;

      oMessage.TypeId := CS_WAMP_CALL_PROGRESS_RESULT;
      oMessage.CallId := GetOriginalCallId(vGUID, aCallId);
      oMessage.CallResult := aResult;

      WriteData(vGUID, oMessage.Write);
    Finally
      sgcFree(oMessage);
    End;
  end;
end;

function TsgcWSProtocol_WAMP_Server.DoBeforeCancelCall(const aConnection:
    TsgcWSConnection; const aCallId: String): Boolean;
begin
  result := True;
  if Assigned(FOnBeforeCancelCall) then
    FOnBeforeCancelCall(aConnection, aCallId, result);
end;

procedure TsgcWSProtocol_WAMP_Server.DoClear(aConnection: TsgcWSConnection);
begin
  inherited;
end;

procedure TsgcWSProtocol_WAMP_Server.DoEventConnect(aConnection:
    TsgcWSConnection);
begin
  // ... welcome
  DoWelcome(aConnection.guid);
  // ... notify connection only if not Assigned Broker
  if not Assigned(Broker) then
    inherited;
end;

procedure TsgcWSProtocol_WAMP_Server.DoEventDisconnect(aConnection:
    TsgcWSConnection; Code: Integer);
begin
  inherited;
  // ... Unsubscribe
  DoUnSubscriptions(aConnection);
end;

procedure TsgcWSProtocol_WAMP_Server.DoEventMessage(aConnection:
    TsgcWSConnection; const Text: string);
var
  vAccept: Boolean;
  vCallId: string;
  vTopicURI: String;
  oMessage: TsgcWSMessageWAMP;
begin
  {$IFDEF SGC_DEBUG}
  DoLog(self, aConnection, 'DoEventMessage', '[Text]: ' + Text);
  {$ENDIF}

  if DoRawMessage(aConnection, Text) then exit;

  oMessage := GetWSMessageByConnection(aConnection);
  oMessage.Read(Text);

  case oMessage.TypeId of
    CS_WAMP_PREFIX:
      begin
        if Assigned(FOnPrefix) then
          FOnPrefix(aConnection, oMessage.Prefix, oMessage.PrefixURI);
      end;
    CS_WAMP_CALL:
      begin
        vCallId := GetNewCallId(aConnection.Guid, oMessage.CallId);
        AddCallId(aConnection.Guid, vCallId);
        if Assigned(FOnCall) then
          FOnCall(aConnection, vCallId, oMessage.ProcUri, oMessage.Arguments);
      end;
    CS_WAMP_SUBSCRIBE:
      begin
        // ... OnBeforeSubscription
        vAccept := True;
        DoEventBeforeSubscription(aConnection, oMessage.TopicURI, vAccept);
        if vAccept then
        begin
          // ... subscribe
          aConnection.DoSubscribe(oMessage.TopicURI);
          if aConnection.Subscribed(oMessage.TopicURI) then
          begin
            aConnection.LastSubscription := oMessage.TopicURI;
            DoEventSubscription(aConnection, oMessage.TopicURI);
          end;
        end;
      end;
    CS_WAMP_UNSUBSCRIBE:
      begin
        aConnection.DoUnSubscribe(oMessage.TopicURI);
        if not aConnection.Subscribed(oMessage.TopicURI) then
        begin
          aConnection.LastUnSubscription := oMessage.TopicURI;
          DoEventUnSubscription(aConnection, oMessage.TopicURI);
        end;
      end;
    CS_WAMP_PUBLISH:
      begin
        oMessage.TypeId := CS_WAMP_EVENT;
        vTopicURI := oMessage.TopicUri;
        Broadcast(oMessage.Write, vTopicURI)
      end;
    CS_WAMP_CALL_CANCEL:
      begin
        if GetCallGuid(aConnection.Guid + '_' + oMessage.CallId) <> '' then
        begin
          if DoBeforeCancelCall(aConnection, oMessage.CallId) then
            RemoveCallId(aConnection.Guid + '_' + oMessage.CallId);
        end
      end
    else
      inherited;
  end;
end;

procedure TsgcWSProtocol_WAMP_Server.DoFinalize(aConnection: TsgcWSConnection);
begin
  inherited;
  // ... Unsubscribe
  DoUnSubscriptions(aConnection);
end;

procedure TsgcWSProtocol_WAMP_Server.DoInitialize(aConnection:
    TsgcWSConnection);
begin
  inherited;
  // ... welcome
  DoWelcome(aConnection.guid);
end;

procedure TsgcWSProtocol_WAMP_Server.DoWelcome(const aGuid: string);
var
  oMessage: TsgcWSMessageWAMP;
begin
  oMessage := TsgcWSMessageWAMP.Create(nil);
  Try
    oMessage.DoEnterWrite;
    // ... welcome message
    oMessage.TypeId := CS_WAMP_WELCOME;
    oMessage.SessionId := aGuid;
    WriteData(aGuid, oMessage.Write);
  Finally
    sgcFree(oMessage);
  End;
end;

function TsgcWSProtocol_WAMP_Server.GetCallGuid(const aCallId: string): String;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  result := '';

  oList := GetCallsId.LockList;
  Try
    for i := 0 to oList.Count - 1 do
    begin
      if TsgcWSCallId(oList[i]).Id = aCallId then
      begin
        result := TsgcWSCallId(oList[i]).Guid;
        break;
      end;
    end;
  Finally
    GetCallsId.UnlockList;
  End;
end;

function TsgcWSProtocol_WAMP_Server.GetCallsId: TsgcThreadListCallsId;
begin
  if not Assigned(FCallId) then
    FCallId := TsgcThreadListCallsId.Create;
  Result := FCallId;
end;

function TsgcWSProtocol_WAMP_Server.RemoveCallId(const aCallId: string):
    Boolean;
var
  i: Integer;
  oCallId: TsgcWSCallId;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  result := False;

  oList := GetCallsId.LockList;
  Try
    for i := 0 to oList.Count - 1 do
    begin
      if TsgcWSCallId(oList[i]).Id = aCallId then
      begin
        result := True;
        oCallId := TsgcWSCallId(oList[i]);
        sgcFree(oCallId);
        oList.Delete(i);
        break;
      end;
    end;
  Finally
    GetCallsId.UnlockList;
  End;
end;


procedure TsgcWSProtocol_WAMP_Server.Event(const aTopicURI: String; const
    aEvent: String = '');
var
  oMessage: TsgcWSMessageWAMP;
begin
  oMessage := TsgcWSMessageWAMP.Create(nil);
  Try
    oMessage.DoEnterWrite;
    oMessage.TypeId := CS_WAMP_EVENT;
    oMessage.TopicUri := aTopicURI;
    oMessage.Event := aEvent;

    BroadCast(oMessage.Write, aTopicURI);
  Finally
    sgcFree(oMessage);
  End;
end;

function TsgcWSProtocol_WAMP_Server.GetNewCallId(const aGuid, aCallId: String):
    String;
begin
  Result := aGuid + '_' + aCallId;
end;

function TsgcWSProtocol_WAMP_Server.GetOriginalCallId(const aGuid, aCallId:
    String): String;
begin
  result := StringReplace(aCallId, aGUID + '_', '', [rfIgnoreCase]);
end;

function TsgcWSProtocol_WAMP_Server.GetWSMessageByConnection(const aConnection:
    TsgcWSConnection): TsgcWSMessageWAMP;
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
      oItem.WSMessage := TsgcWSMessageWAMP.Create(nil);
      AddProtocolDataItem(aConnection, FWSMessageId, oItem);
    end;

    result := TsgcWSMessageWAMP(oItem.WSMessage);
  end;
end;

function TsgcWSProtocol_JS_WAMP.GetResponse: string;
begin
  result := GetResourceString(CS_SGC_JS_WAMP_ESEGECE_COM);
end;

class function TsgcWSProtocol_JS_WAMP.GetFileName: string;
begin
  Result := CS_JS_WAMP_ESEGECE_COM;
end;

class function TsgcWSProtocol_HTML_WAMP.GetFileName: string;
begin
  Result := CS_HTML_WAMP_ESEGECE_COM;
end;

function TsgcWSProtocol_HTML_WAMP.GetResponse: string;
begin
  result := GetResourceString(CS_SGC_HTML_WAMP_ESEGECE_COM);
end;


initialization
  Classes.RegisterClass(TsgcWSProtocol_WAMP_Server);
  TsgcWSHTTPResponse.RegisterfileName(TsgcWSProtocol_JS_WAMP);
  TsgcWSHTTPResponse.RegisterfileName(TsgcWSProtocol_HTML_WAMP);

{$ENDIF}

end.
