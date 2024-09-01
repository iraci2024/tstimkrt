{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Protocol_WAMP2_Client;

interface

{$I sgcVer.inc}

{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils,
{$IFDEF MSWINDOWS} Windows, {$ENDIF}
  // sgc
  sgcJSON,
  sgcWebSocket_Classes, sgcWebSocket_Protocol_Base_Client,
  sgcWebSocket_Protocol_WAMP2_Message, sgcWebSocket_Classes_SyncObjs;

{$IFDEF BCB}
{$IFNDEF DXE}
  {$DEFINE _YIELD} // Bug C2010
{$ENDIF}
{$ENDIF}

type

  TsgcWSSessionEvent = procedure(Connection: TsgcWSConnection;
    var aRealm, aDetails: String) of object;
  TsgcWSWelcomeEvent = procedure(Connection: TsgcWSConnection;
    SessionId: Int64; Details: string) of object;
  TsgcWSChallengeEvent = procedure(Connection: TsgcWSConnection;
    AuthMethod: String; Details: string; var Secret: String) of object;
  TsgcWSSubscribedEvent = procedure(Connection: TsgcWSConnection;
    RequestId, SubscriptionId: Int64) of object;
  TsgcWSUnsubscribedEvent = procedure(Connection: TsgcWSConnection;
    RequestId: Int64) of object;
  TsgcWSEventEvent = procedure(Connection: TsgcWSConnection;
    SubscriptionId: Int64; PublicationId: Int64; Details, Arguments,
    ArgumentsKw: String) of object;
  TsgcWSPublishedEvent = procedure(Connection: TsgcWSConnection;
    RequestId, PublicationId: Int64) of object;
  TsgcWSResultEvent = procedure(Connection: TsgcWSConnection; RequestId: Int64;
    Details, Arguments, ArgumentsKw: String) of object;
  TsgcWSErrorEvent = procedure(Connection: TsgcWSConnection; MethodId: Integer;
    RequestId: Int64; Details, Error, Arguments, ArgumentsKw: String) of object;
  TsgcWSAbortEvent = procedure(Connection: TsgcWSConnection;
    Details, Reason: String) of object;
  TsgcWSGoodByeEvent = procedure(Connection: TsgcWSConnection;
    Details, Reason: String) of object;
  TsgcWSRegisteredEvent = procedure(Connection: TsgcWSConnection;
    RequestId, RegistrationId: Int64) of object;
  TsgcWSUnRegisteredEvent = procedure(Connection: TsgcWSConnection;
    RequestId, RegistrationId: Int64) of object;

  TsgcWSProtocol_WAMP2_Client = class(TsgcWSProtocol_Client_Base)
    { WSMessageWAMP }
  private
    FWSMessageWAMPId: String;
  protected
    FWSMessageWAMP: TsgcWSMessageWAMP2;
    function GetWSMessageWAMP: TsgcWSMessageWAMP2;
    function GetWSMessageByConnection(const aConnection: TsgcWSConnection):
        TsgcWSMessageWAMP2;
  protected
    property WSMessageWAMP
      : TsgcWSMessageWAMP2 read GetWSMessageWAMP write FWSMessageWAMP;
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
  protected
    procedure DoReplyGoodBye; virtual;
    procedure DoHello(const aRealm, aDetails: String); virtual;
    procedure DoAuthenticate(const aSecret: String); virtual;
  public
    procedure Subscribe(const aTopic: String; const aOptions: String = '{}'; const
        aRequestId: Int64 = 0);
    procedure UnSubscribe(const aSubscriptionId: Int64;
      const aRequestId: Int64 = 0);
    procedure Abort(const aDetails: String; const aReason: String);
    procedure GoodBye(const aDetails: String; const aReason: String);
    procedure Publish(const aTopic: String; const aArguments: String = '';
      const aArgumentsKw: String = ''; const aOptions: String = '{}';
      const aRequestId: Int64 = 0);
    procedure Call(const aProcedure: String; const aArguments: String = '';
      const aArgumentsKw: String = ''; const aOptions: String = '{}';
      const aRequestId: Int64 = 0);
    procedure RegisterCall(const aProcedure: String;
      const aOptions: String = '{}'; const aRequestId: Int64 = 0);
    procedure UnRegisterCall(const aRegistrationId: Int64;
      const aRequestId: Int64 = 0);
    procedure Invocation(const aRegistrationId: Int64; const aDetails: String = '';
        const aArguments: String = ''; const aArgumentsKw: String = ''; const
        aRequestId: Int64 = 0);
    procedure InvocationError(const aRequestId: Int64; const aDetails: String =
        '{}'; const aTopic: String = ''; const aArguments: String = ''; const
        aArgumentsKw: String = '');
    procedure {$IFDEF _YIELD}_Yield{$ELSE}Yield{$ENDIF}(const aOptions: String = '{}'; const aArguments: String = '';
        const aArgumentsKw: String = ''; const aRequestId: Int64 = 0);
    { procedures }

    { events }
  private
    FOnWAMPSession: TsgcWSSessionEvent;
    FOnWAMPSubscribed: TsgcWSSubscribedEvent;
    FOnWAMPUnsubscribed: TsgcWSUnsubscribedEvent;
    FOnWAMPWelcome: TsgcWSWelcomeEvent;
    FOnWAMPError: TsgcWSErrorEvent;
    FOnWAMPAbort: TsgcWSAbortEvent;
    FOnWAMPGoodBye: TsgcWSGoodByeEvent;
    FOnWAMPEvent: TsgcWSEventEvent;
    FOnWAMPPublished: TsgcWSPublishedEvent;
    FOnWAMPRegistered: TsgcWSRegisteredEvent;
    FOnWAMPResult: TsgcWSResultEvent;
    FOnWAMPUnRegistered: TsgcWSUnRegisteredEvent;
    FOnWAMPChallenge: TsgcWSChallengeEvent;
  protected
    procedure DoSessionEvent(aConnection: TsgcWSConnection);
  published
    property OnWAMPSession
      : TsgcWSSessionEvent read FOnWAMPSession write FOnWAMPSession;
    property OnWAMPWelcome
      : TsgcWSWelcomeEvent read FOnWAMPWelcome write FOnWAMPWelcome;
    property OnWAMPAbort: TsgcWSAbortEvent read FOnWAMPAbort write FOnWAMPAbort;
    property OnWAMPGoodBye
      : TsgcWSGoodByeEvent read FOnWAMPGoodBye write FOnWAMPGoodBye;
    property OnWAMPSubscribed
      : TsgcWSSubscribedEvent read FOnWAMPSubscribed write
      FOnWAMPSubscribed;
    property OnWAMPUnsubscribed
      : TsgcWSUnsubscribedEvent read FOnWAMPUnsubscribed write
      FOnWAMPUnsubscribed;
    property OnWAMPPublished: TsgcWSPublishedEvent read FOnWAMPPublished write
      FOnWAMPPublished;
    property OnWAMPRegistered
      : TsgcWSRegisteredEvent read FOnWAMPRegistered write
      FOnWAMPRegistered;
    property OnWAMPUnRegistered
      : TsgcWSUnRegisteredEvent read FOnWAMPUnRegistered write
      FOnWAMPUnRegistered;
    property OnWAMPEvent: TsgcWSEventEvent read FOnWAMPEvent write FOnWAMPEvent;
    property OnWAMPError: TsgcWSErrorEvent read FOnWAMPError write FOnWAMPError;
    property OnWAMPResult
      : TsgcWSResultEvent read FOnWAMPResult write FOnWAMPResult;
    property OnWAMPChallenge: TsgcWSChallengeEvent read FOnWAMPChallenge write
        FOnWAMPChallenge;
    { events }
  end;
{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses
  sgcWebSocket_Const, sgcBase_Helpers;

constructor TsgcWSProtocol_WAMP2_Client.Create(aOwner: TComponent);
begin
  inherited;
  FWSMessageWAMPId := NewGuid;
  FProtocol := CS_PROTOCOL_WAMP2;
end;

destructor TsgcWSProtocol_WAMP2_Client.Destroy;
begin
  inherited;
end;

procedure TsgcWSProtocol_WAMP2_Client.Abort(const aDetails: String;
  const aReason: String);
var
  oMessage: TsgcWSMessageWAMP2;
begin
  oMessage := WSMessageWAMP;

  oMessage.DoEnterWrite;
  Try
    oMessage.MethodId := CS_WAMP2_ABORT;
    oMessage.Details := aDetails;
    oMessage.Reason := aReason;

    DoWrite;
  Finally
    oMessage.DoLeaveWrite;
  End;
end;

procedure TsgcWSProtocol_WAMP2_Client.Call(const aProcedure: String;
  const aArguments: String = ''; const aArgumentsKw: String = '';
  const aOptions: String = '{}'; const aRequestId: Int64 = 0);
var
  oMessage: TsgcWSMessageWAMP2;
begin
  oMessage := WSMessageWAMP;

  oMessage.DoEnterWrite;
  Try
    oMessage.MethodId := CS_WAMP2_CALL;
    oMessage.RequestId := aRequestId;
    oMessage.TopicUri := aProcedure;
    oMessage.Options := aOptions;
    oMessage.Arguments := aArguments;
    oMessage.ArgumentsKw := aArgumentsKw;

    DoWrite;
  Finally
    oMessage.DoLeaveWrite;
  End;
end;

procedure TsgcWSProtocol_WAMP2_Client.DoAuthenticate(const aSecret: String);
var
  oMessage: TsgcWSMessageWAMP2;
begin
  oMessage := WSMessageWAMP;

  oMessage.DoEnterWrite;
  Try
    oMessage.MethodId := CS_WAMP2_AUTHENTICATE;
    oMessage.Authentication := aSecret;
    oMessage.Details := '{}';

    DoWrite;
  Finally
    oMessage.DoLeaveWrite;
  End;
end;

procedure TsgcWSProtocol_WAMP2_Client.DoSessionEvent
  (aConnection: TsgcWSConnection);
var
  vRealm: string;
  vDetails: string;
begin
  vRealm := '';
  vDetails := '';
  if Assigned(FOnWAMPSession) then
    FOnWAMPSession(aConnection, vRealm, vDetails);
  DoHello(vRealm, vDetails);
end;

procedure TsgcWSProtocol_WAMP2_Client.DoClear(aConnection: TsgcWSConnection);
begin
  inherited;
end;

procedure TsgcWSProtocol_WAMP2_Client.DoEventConnect
  (aConnection: TsgcWSConnection);
begin
  inherited;
  FWSConnection := aConnection;

  DoSessionEvent(aConnection);
end;

procedure TsgcWSProtocol_WAMP2_Client.DoEventDisconnect
  (aConnection: TsgcWSConnection; Code: Integer);
var
  vId: String;
begin
  vId := aConnection.Guid;
  if Assigned(FWSMessageWAMP) then
    FWSMessageWAMP.Clear(True);
  FWSConnection := nil;
  inherited;
end;

procedure TsgcWSProtocol_WAMP2_Client.DoEventMessage
  (aConnection: TsgcWSConnection; const Text: string);
var
  oMessage: TsgcWSMessageWAMP2;
  vSecret: String;
begin
{$IFDEF SGC_DEBUG}
  DoLog(self, aConnection, 'DoEventMessage', '[Text]: ' + Text);
{$ENDIF}
  if DoRawMessage(aConnection, Text) then
    exit;

  oMessage := WSMessageWAMP;
  oMessage.Read(Text);

  case oMessage.MethodId of
    CS_WAMP2_WELCOME:
      begin
        if Assigned(FOnWAMPWelcome) then
          FOnWAMPWelcome(aConnection, oMessage.SessionId,
            oMessage.Details);
      end;
    CS_WAMP2_CHALLENGE:
      begin
        if Assigned(FOnWAMPChallenge) then
        begin
          vSecret := '';
          FOnWAMPChallenge(aConnection, oMessage.Authentication,
            oMessage.Details, vSecret);
          if vSecret <> '' then
            DoAuthenticate(vSecret);
        end;
      end;
    CS_WAMP2_ABORT:
      begin
        if Assigned(FOnWAMPAbort) then
          FOnWAMPAbort(aConnection, oMessage.Details,
            oMessage.Reason);
      end;
    CS_WAMP2_GOODBYE:
      begin
        if Assigned(FOnWAMPGoodBye) then
          FOnWAMPGoodBye(aConnection, oMessage.Details,
            oMessage.Reason);
        DoReplyGoodBye;
      end;
    CS_WAMP2_PUBLISHED:
      begin
        if Assigned(FOnWAMPPublished) then
          FOnWAMPPublished(aConnection, oMessage.RequestId,
            oMessage.PublicationId);
      end;
    CS_WAMP2_SUBSCRIBED:
      begin
        if Assigned(FOnWAMPSubscribed) then
          FOnWAMPSubscribed(aConnection, oMessage.RequestId,
            oMessage.SubscriptionId);
      end;
    CS_WAMP2_UNSUBSCRIBED:
      begin
        if Assigned(FOnWAMPUnsubscribed) then
          FOnWAMPUnsubscribed(aConnection, oMessage.RequestId);
      end;
    CS_WAMP2_EVENT:
      begin
        if Assigned(FOnWAMPEvent) then
          FOnWAMPEvent(aConnection, oMessage.SubscriptionId,
            oMessage.PublicationId, oMessage.Details,
            oMessage.Arguments, oMessage.ArgumentsKw);
      end;
    CS_WAMP2_REGISTERED:
      begin
        if Assigned(FOnWAMPRegistered) then
          FOnWAMPRegistered(aConnection, oMessage.RequestId,
            oMessage.SubscriptionId);
      end;
    CS_WAMP2_UNREGISTERED:
      begin
        if Assigned(FOnWAMPUnRegistered) then
          FOnWAMPUnRegistered(aConnection, oMessage.RequestId,
            oMessage.SubscriptionId);
      end;
    CS_WAMP2_ERROR:
      begin
        if Assigned(FOnWAMPError) then
          FOnWAMPError(aConnection, oMessage.ErrorMethodId,
            oMessage.ErrorRequestId, oMessage.ErrorDetails,
            oMessage.ErrorURI, '', '');
      end;
    CS_WAMP2_RESULT:
      begin
        if Assigned(FOnWAMPResult) then
          FOnWAMPResult(aConnection, oMessage.RequestId,
            oMessage.Details, oMessage.Arguments,
            oMessage.ArgumentsKw);
      end;
  else
    inherited;
  end;
end;

procedure TsgcWSProtocol_WAMP2_Client.DoFinalize(aConnection: TsgcWSConnection);
begin
  inherited;
  if Assigned(FWSMessageWAMP) then
    FWSMessageWAMP.Clear(True);
  FWSConnection := nil;
end;

procedure TsgcWSProtocol_WAMP2_Client.DoHello(const aRealm, aDetails: String);
var
  oMessage: TsgcWSMessageWAMP2;
begin
  oMessage := WSMessageWAMP;

  oMessage.DoEnterWrite;
  Try
    oMessage.MethodId := CS_WAMP2_HELLO;
    oMessage.TopicUri := aRealm;
    oMessage.Details := aDetails;

    DoWrite;
  Finally
    oMessage.DoLeaveWrite;
  End;
end;

procedure TsgcWSProtocol_WAMP2_Client.DoInitialize(aConnection:
    TsgcWSConnection);
begin
  inherited;
  FWSConnection := aConnection;

  DoSessionEvent(aConnection);
end;

procedure TsgcWSProtocol_WAMP2_Client.DoReplyGoodBye;
var
  oMessage: TsgcWSMessageWAMP2;
begin
  oMessage := WSMessageWAMP;

  oMessage.DoEnterWrite;
  Try
    oMessage.MethodId := CS_WAMP2_GOODBYE;
    oMessage.Details := '{}';
    oMessage.Reason := CS_WAMP2_GOODBYE_REPLY;

    DoWrite;
  Finally
    oMessage.DoLeaveWrite;
  End;
end;

procedure TsgcWSProtocol_WAMP2_Client.DoWrite;
begin
  WriteData(WSMessageWAMP.Write);
end;

function TsgcWSProtocol_WAMP2_Client.GetWSMessageByConnection(const
    aConnection: TsgcWSConnection): TsgcWSMessageWAMP2;
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
      oItem.WSMessage := TsgcWSMessageWAMP2.Create(nil);
      AddProtocolDataItem(aConnection, FWSMessageWAMPId, oItem);
    end;

    result := TsgcWSMessageWAMP2(oItem.WSMessage);
  end;
end;

function TsgcWSProtocol_WAMP2_Client.GetWSMessageWAMP: TsgcWSMessageWAMP2;
begin
  result := nil;

  if Assigned(FWSConnection) then
    result := GetWSMessageByConnection(FWSConnection);

  if not Assigned(result) then
  begin
    if not Assigned(FWSMessageWAMP) then
      FWSMessageWAMP := TsgcWSMessageWAMP2.Create(self);
    Result := FWSMessageWAMP;
  end;
end;

procedure TsgcWSProtocol_WAMP2_Client.GoodBye(const aDetails: String;
  const aReason: String);
var
  oMessage: TsgcWSMessageWAMP2;
begin
  oMessage := WSMessageWAMP;

  oMessage.DoEnterWrite;
  Try
    oMessage.MethodId := CS_WAMP2_GOODBYE;
    oMessage.Details := aDetails;
    oMessage.Reason := aReason;

    DoWrite;
  Finally
    oMessage.DoLeaveWrite;
  End;
end;

procedure TsgcWSProtocol_WAMP2_Client.Invocation(const aRegistrationId: Int64;
    const aDetails: String = ''; const aArguments: String = ''; const
    aArgumentsKw: String = ''; const aRequestId: Int64 = 0);
var
  oMessage: TsgcWSMessageWAMP2;
begin
  oMessage := WSMessageWAMP;

  oMessage.DoEnterWrite;
  Try
    oMessage.MethodId := CS_WAMP2_INVOCATION;
    oMessage.RequestId := aRequestId;
    oMessage.Details := aDetails;
    oMessage.Arguments := aArguments;
    oMessage.ArgumentsKw := aArgumentsKw;

    DoWrite;
  Finally
    oMessage.DoLeaveWrite;
  End;
end;

procedure TsgcWSProtocol_WAMP2_Client.InvocationError(const aRequestId: Int64;
    const aDetails: String = '{}'; const aTopic: String = ''; const aArguments:
    String = ''; const aArgumentsKw: String = '');
var
  oMessage: TsgcWSMessageWAMP2;
begin
  oMessage := WSMessageWAMP;

  oMessage.DoEnterWrite;
  Try
    oMessage.MethodId := CS_WAMP2_INVOCATION_ERROR;
    oMessage.RequestId := aRequestId;
    oMessage.Details := aDetails;
    oMessage.TopicUri := aTopic;
    oMessage.Arguments := aArguments;
    oMessage.ArgumentsKw := aArgumentsKw;

    DoWrite;
  Finally
    oMessage.DoLeaveWrite;
  End;
end;

procedure TsgcWSProtocol_WAMP2_Client.Publish(const aTopic: String;
  const aArguments: String = ''; const aArgumentsKw: String = '';
  const aOptions: String = '{}'; const aRequestId: Int64 = 0);
var
  oMessage: TsgcWSMessageWAMP2;
begin
  oMessage := WSMessageWAMP;

  oMessage.DoEnterWrite;
  Try
    oMessage.MethodId := CS_WAMP2_PUBLISH;
    oMessage.RequestId := aRequestId;
    oMessage.TopicUri := aTopic;
    oMessage.Options := aOptions;
    oMessage.Arguments := aArguments;
    oMessage.ArgumentsKw := aArgumentsKw;

    DoWrite;
  Finally
    oMessage.DoLeaveWrite;
  End;
end;

procedure TsgcWSProtocol_WAMP2_Client.RegisterCall(const aProcedure: String;
  const aOptions: String = '{}'; const aRequestId: Int64 = 0);
var
  oMessage: TsgcWSMessageWAMP2;
begin
  oMessage := WSMessageWAMP;

  oMessage.DoEnterWrite;
  Try
    oMessage.MethodId := CS_WAMP2_REGISTER;
    oMessage.RequestId := aRequestId;
    oMessage.TopicUri := aProcedure;
    oMessage.Options := aOptions;

    DoWrite;
  Finally
    oMessage.DoLeaveWrite;
  End;
end;

procedure TsgcWSProtocol_WAMP2_Client.Subscribe(const aTopic: String; const
    aOptions: String = '{}'; const aRequestId: Int64 = 0);
var
  oMessage: TsgcWSMessageWAMP2;
begin
  oMessage := WSMessageWAMP;

  oMessage.DoEnterWrite;
  Try
    oMessage.MethodId := CS_WAMP2_SUBSCRIBE;
    oMessage.RequestId := aRequestId;
    oMessage.Options := aOptions;
    oMessage.TopicUri := aTopic;

    DoWrite;
  Finally
    oMessage.DoLeaveWrite;
  End;
end;

procedure TsgcWSProtocol_WAMP2_Client.UnRegisterCall
  (const aRegistrationId: Int64; const aRequestId: Int64 = 0);
var
  oMessage: TsgcWSMessageWAMP2;
begin
  oMessage := WSMessageWAMP;

  oMessage.DoEnterWrite;
  Try
    oMessage.MethodId := CS_WAMP2_UNREGISTER;
    oMessage.RequestId := aRequestId;
    oMessage.SubscriptionId := aRegistrationId;

    DoWrite;
  Finally
    oMessage.DoLeaveWrite;
  End;
end;

procedure TsgcWSProtocol_WAMP2_Client.UnSubscribe(const aSubscriptionId: Int64;
  const aRequestId: Int64 = 0);
var
  oMessage: TsgcWSMessageWAMP2;
begin
  oMessage := WSMessageWAMP;

  oMessage.DoEnterWrite;
  Try
    oMessage.MethodId := CS_WAMP2_UNSUBSCRIBE;
    oMessage.RequestId := aRequestId;
    oMessage.SubscriptionId := aSubscriptionId;

    DoWrite;
  Finally
    oMessage.DoLeaveWrite;
  End;
end;

procedure TsgcWSProtocol_WAMP2_Client.{$IFDEF _YIELD}_Yield{$ELSE}Yield{$ENDIF}(const aOptions: String = '{}';
    const aArguments: String = ''; const aArgumentsKw: String = ''; const
    aRequestId: Int64 = 0);
var
  oMessage: TsgcWSMessageWAMP2;
begin
  oMessage := WSMessageWAMP;

  oMessage.DoEnterWrite;
  Try
    oMessage.MethodId := CS_WAMP2_YIELD;
    oMessage.RequestId := aRequestId;
    oMessage.Options := aOptions;
    oMessage.Arguments := aArguments;
    oMessage.ArgumentsKw := aArgumentsKw;

    DoWrite;
  Finally
    oMessage.DoLeaveWrite;
  End;
end;

{$ENDIF}

end.
