{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Protocol_WAMP_Client;

interface

{$I sgcVer.inc}

{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils,
{$IFDEF MSWINDOWS} Windows, {$ENDIF}
  // sgc
  sgcJSON,
  sgcWebSocket_Classes, sgcWebSocket_Protocol_Base_Client,
  sgcWebSocket_Protocol_WAMP_Message, sgcWebSocket_Classes_SyncObjs;

type

  TsgcWSWelcomeEvent = procedure(Connection: TsgcWSConnection;
    SessionId, ProtocolVersion, ServerIdent: string) of object;
  TsgcWSCallResultEvent = procedure(Connection: TsgcWSConnection;
    CallId, Result: string) of object;
  TsgcWSCallProgresslResultEvent = procedure(Connection: TsgcWSConnection;
      CallId, Result: string) of object;
  TsgcWSCallErrorEvent = procedure(Connection: TsgcWSConnection;
    CallId, ErrorURI, ErrorDesc, ErrorDetails: string) of object;

  TsgcWSEvent = procedure(Connection: TsgcWSConnection;
    TopicURI, Event: string) of object;

  TsgcWSProtocol_WAMP_Client = class(TsgcWSProtocol_Client_Base)
    { WSMessageWAMP }
  private
    FWSMessageWAMPId: String;
  protected
    FWSMessageWAMP: TsgcWSMessageWAMP;
    function GetWSMessageWAMP: TsgcWSMessageWAMP;
    function GetWSMessageByConnection(const aConnection: TsgcWSConnection):
        TsgcWSMessageWAMP;
  protected
    property WSMessageWAMP
      : TsgcWSMessageWAMP read GetWSMessageWAMP write FWSMessageWAMP;
    { WSMessageWAMP }

    { from TsgcWSComponent }
  private
    FWSConnection: TsgcWSConnection;
  protected
    procedure DoEventConnect(aConnection: TsgcWSConnection); override;
    procedure DoEventMessage(aConnection: TsgcWSConnection;
      const Text: string); override;
    procedure DoEventDisconnect(aConnection: TsgcWSConnection; Code: Integer);
        override;
    { from TsgcWSComponent }

  { from TsgcWSProtocol }
  protected
    procedure DoInitialize(aConnection: TsgcWSConnection); override;
    procedure DoFinalize(aConnection: TsgcWSConnection); override;
    procedure DoClear(aConnection: TsgcWSConnection); override;
  { from TsgcWSProtocol }


    { constructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor }

    { write }
  protected
    procedure DoWrite; virtual;
    { write }

    { procedures }
  public
    procedure Prefix(const aPrefix, aURI: String);
    procedure Subscribe(const aTopicURI: String);
    procedure UnSubscribe(const aTopicURI: String);
    procedure Call(const aCallId, aProcURI: String; aArguments: String = '');
    procedure CancelCall(const aCallId: string);
    procedure Publish(const aTopicURI, aEvent: string;
      const aExclude: string = ''; const aEligible: String = '');
    { procedures }

    { events }
  private
    FOnCallError: TsgcWSCallErrorEvent;
    FOnCallResult: TsgcWSCallResultEvent;
    FOnCallProgressResult: TsgcWSCallProgresslResultEvent;
    FOnEvent: TsgcWSEvent;
    FOnWelcome: TsgcWSWelcomeEvent;
  published
    property OnCallError
      : TsgcWSCallErrorEvent read FOnCallError write FOnCallError;
    property OnCallResult
      : TsgcWSCallResultEvent read FOnCallResult write FOnCallResult;
    property OnCallProgressResult: TsgcWSCallProgresslResultEvent read
        FOnCallProgressResult write FOnCallProgressResult;
    property OnEvent: TsgcWSEvent read FOnEvent write FOnEvent;
    property OnWelcome: TsgcWSWelcomeEvent read FOnWelcome write FOnWelcome;
    { events }
  end;

{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses
  sgcWebSocket_Const, sgcBase_Helpers;

constructor TsgcWSProtocol_WAMP_Client.Create(aOwner: TComponent);
begin
  inherited;
  FWSMessageWAMPId := NewGuid;
  FProtocol := CS_PROTOCOL_WAMP;
end;

destructor TsgcWSProtocol_WAMP_Client.Destroy;
begin
  inherited;
end;

procedure TsgcWSProtocol_WAMP_Client.Call(const aCallId, aProcURI: String;
  aArguments: String = '');
var
  oMessage: TsgcWSMessageWAMP;
begin
  oMessage := WSMessageWAMP;

  oMessage.DoEnterWrite;
  Try
    oMessage.TypeId := CS_WAMP_CALL;
    oMessage.CallId := aCallId;
    oMessage.ProcUri := aProcURI;
    oMessage.Arguments := aArguments;

    DoWrite;
  Finally
    oMessage.DoLeaveWrite
  End;
end;

procedure TsgcWSProtocol_WAMP_Client.CancelCall(const aCallId: string);
var
  oMessage: TsgcWSMessageWAMP;
begin
  oMessage := WSMessageWAMP;

  oMessage.DoEnterWrite;
  Try
    oMessage.TypeId := CS_WAMP_CALL_CANCEL;
    oMessage.CallId := aCallId;

    DoWrite;
  Finally
    oMessage.DoLeaveWrite
  End;
end;

procedure TsgcWSProtocol_WAMP_Client.DoClear(aConnection: TsgcWSConnection);
begin
  inherited;
end;

procedure TsgcWSProtocol_WAMP_Client.DoEventConnect(aConnection:
    TsgcWSConnection);
begin
  inherited;
  FWSConnection := aConnection;
end;

procedure TsgcWSProtocol_WAMP_Client.DoEventDisconnect(aConnection:
    TsgcWSConnection; Code: Integer);
begin
  if Assigned(FWSMessageWAMP) then
    FWSMessageWAMP.Clear(True);
  FWSConnection := nil;
  inherited;
end;

procedure TsgcWSProtocol_WAMP_Client.DoEventMessage
  (aConnection: TsgcWSConnection; const Text: string);
var
  oMessage: TsgcWSMessageWAMP;
begin
  {$IFDEF SGC_DEBUG}
  DoLog(self, aConnection, 'DoEventMessage', '[Text]: ' + Text);
  {$ENDIF}

  if DoRawMessage(aConnection, Text) then
    exit;

  oMessage := WSMessageWAMP;
  oMessage.Read(Text);

  case oMessage.TypeId of
    CS_WAMP_WELCOME:
      begin
        if Assigned(FOnWelcome) then
          FOnWelcome(aConnection, oMessage.SessionId,
            oMessage.ProtocolVersion, oMessage.ServerIdent);
      end;
    CS_WAMP_CALLRESULT:
      begin
        if Assigned(FOnCallResult) then
          FOnCallResult(aConnection, oMessage.CallId,
            oMessage.CallResult);
      end;
    CS_WAMP_CALL_PROGRESS_RESULT:
      begin
        if Assigned(FOnCallProgressResult) then
          FOnCallProgressResult(aConnection, oMessage.CallId,
            oMessage.CallResult);
      end;
    CS_WAMP_CALLERROR:
      begin
        if Assigned(FOnCallError) then
          FOnCallError(aConnection, oMessage.CallId,
            oMessage.ErrorURI, oMessage.ErrorDesc,
            oMessage.ErrorDetails);
      end;
    CS_WAMP_EVENT:
      begin
        if Assigned(FOnEvent) then
          FOnEvent(aConnection, oMessage.TopicURI, oMessage.Event);
      end;
  else
    inherited;
  end;
end;

procedure TsgcWSProtocol_WAMP_Client.DoFinalize(aConnection: TsgcWSConnection);
begin
  inherited;
  if Assigned(FWSMessageWAMP) then
    FWSMessageWAMP.Clear(True);
  FWSConnection := nil;
end;

procedure TsgcWSProtocol_WAMP_Client.DoInitialize(aConnection:
    TsgcWSConnection);
begin
  inherited;
  FWSConnection := aConnection;
end;

procedure TsgcWSProtocol_WAMP_Client.DoWrite;
begin
  WriteData(WSMessageWAMP.Write);
end;

function TsgcWSProtocol_WAMP_Client.GetWSMessageByConnection(const aConnection:
    TsgcWSConnection): TsgcWSMessageWAMP;
var
  oItem: TsgcQueueItemProtocolMessage;
begin
  Result := nil;

  if Assigned(aConnection) then
  begin
    oItem := GetProtocolDataItem(aConnection, FWSMessageWAMPId);
    if not Assigned(oItem) then
    begin
      oItem := TsgcQueueItemProtocolMessage.Create;
      oItem.WSMessage := TsgcWSMessageWAMP.Create(nil);
      AddProtocolDataItem(aConnection, FWSMessageWAMPId, oItem);
    end;

    result := TsgcWSMessageWAMP(oItem.WSMessage);
  end;
end;

function TsgcWSProtocol_WAMP_Client.GetWSMessageWAMP: TsgcWSMessageWAMP;
begin
  result := nil;

  if Assigned(FWSConnection) then
    result := GetWSMessageByConnection(FWSConnection);

  if not Assigned(result) then
  begin
    if not Assigned(FWSMessageWAMP) then
      FWSMessageWAMP := TsgcWSMessageWAMP.Create(self);
    Result := FWSMessageWAMP;
  end;
end;

procedure TsgcWSProtocol_WAMP_Client.Prefix(const aPrefix, aURI: String);
var
  oMessage: TsgcWSMessageWAMP;
begin
  oMessage := WSMessageWAMP;

  oMessage.DoEnterWrite;
  Try
    oMessage.TypeId := CS_WAMP_PREFIX;
    oMessage.Prefix := aPrefix;
    oMessage.PrefixURI := aURI;

    DoWrite;
  Finally
    oMessage.DoLeaveWrite;
  End;
end;

procedure TsgcWSProtocol_WAMP_Client.Publish(const aTopicURI, aEvent: string;
  const aExclude: string = ''; const aEligible: String = '');
var
  oMessage: TsgcWSMessageWAMP;
begin
  oMessage := WSMessageWAMP;

  oMessage.DoEnterWrite;
  Try
    oMessage.TypeId := CS_WAMP_PUBLISH;
    oMessage.TopicURI := aTopicURI;
    oMessage.Event := aEvent;
    oMessage.Exclude := aExclude;
    oMessage.Eligible := aEligible;

    DoWrite;
  Finally
    oMessage.DoLeaveWrite;
  End;
end;

procedure TsgcWSProtocol_WAMP_Client.Subscribe(const aTopicURI: String);
var
  oMessage: TsgcWSMessageWAMP;
begin
  oMessage := WSMessageWAMP;

  oMessage.DoEnterWrite;
  Try
    oMessage.TypeId := CS_WAMP_SUBSCRIBE;
    oMessage.TopicURI := aTopicURI;

    DoWrite;
  Finally
    oMessage.DoLeaveWrite;
  End;
end;

procedure TsgcWSProtocol_WAMP_Client.UnSubscribe(const aTopicURI: String);
var
  oMessage: TsgcWSMessageWAMP;
begin
  oMessage := WSMessageWAMP;

  oMessage.DoEnterWrite;
  Try
    oMessage.TypeId := CS_WAMP_UNSUBSCRIBE;
    oMessage.TopicURI := aTopicURI;

    DoWrite;
  Finally
    oMessage.DoLeaveWrite;
  End;
end;

{$ENDIF}

end.
