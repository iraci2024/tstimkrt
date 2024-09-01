{ ***************************************************************************
  sgcWebSocket component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcWebSocket_Protocol_Base_Client;

interface

{$I sgcVer.inc}
{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils,
{$IFDEF NEXTGEN}System.Generics.Collections, {$ELSE}Contnrs, {$ENDIF}
{$IFDEF MSWINDOWS}Windows, {$ENDIF}
  // sgc
  sgcWebSocket_Classes, sgcWebSocket_Protocol_Broker_Client,
  sgcWebSocket_Types, sgcWebSocket_Protocol_Base_Message;

type
  TsgcWSProtocol_Client_Base = class;
  TsgcWSProtocol_Subscription_Client_Base = class;
  TsgcWSProtocol_Acknowledgment_Client_Base = class;

  TsgcWSProtocol_Client_Base = Class(TsgcWSProtocol_Client)
    { from TComponent }
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    { from TComponent }

    { from TsgcWSProtocol_Client }
  protected
    procedure Loaded; override;
    { from TsgcWSProtocol_Client }

    { broker component }
  private
    FBroker: TsgcWSProtocol_Broker_Client;
  protected
    procedure SetBroker(const Value: TsgcWSProtocol_Broker_Client); virtual;
  public
    property Broker: TsgcWSProtocol_Broker_Client read FBroker write SetBroker;
    { broker component }

    { default client }
  protected
    function GetWSClient: TsgcWSComponent_WSClient;
    { default client }

    { constructor }
  public
    constructor Create(aOwner: TComponent); override;
    { constructor }

    { send message }
  public
    procedure WriteData(const aText: String); overload; virtual;
    procedure WriteData(aStream: TStream; aSize: Integer = 0;
      const aStreaming: TwsStreaming = stmNone); overload; virtual;
    { send message }
  End;

  TsgcWSProtocol_Subscription_Client_Base = class(TsgcWSProtocol_Client_Base)
    { from TsgcWSComponent }
  protected
    procedure DoEventDisconnect(aConnection: TsgcWSConnection;
      aCode: Integer); override;
    { from TsgcWSComponent }

    { subscription }
  private
    FOnSubscription: TsgcWSSubscriptionEvent;
    FOnUnSubscription: TsgcWSSubscriptionEvent;
{$IFNDEF SGC_EVENT_DISPATCH}
  private
    FNotifySubscription: TsgcWSObjectList;
    FNotifyUnSubscription: TsgcWSObjectList;
  private
    FAsyncSubscription: Boolean;
    FAsyncUnSubscription: Boolean;
    procedure DoAsyncSubscription;
    procedure DoAsyncUnSubscription;
  private
    function GetNotifySubscription: TsgcWSObjectList;
    function GetNotifyUnSubscription: TsgcWSObjectList;
  private
    property NotifySubscription: TsgcWSObjectList read GetNotifySubscription;
    property NotifyUnSubscription: TsgcWSObjectList
      read GetNotifyUnSubscription;
{$ENDIF}
  protected
    procedure DoNotifySubscription(aConnection: TsgcWSConnection); virtual;
    procedure DoNotifyUnSubscription(aConnection: TsgcWSConnection); virtual;
  protected
    procedure DoEventSubscription(aConnection: TsgcWSConnection;
      const aSubscription: String);
    procedure DoEventUnSubscription(aConnection: TsgcWSConnection;
      const aSubscription: String);
  public
    destructor Destroy; override;
  private
    FSubscriptions: TStringList;
    function GetSubscriptions: TStringList;
  protected
    procedure DoSubscribe(const aChannel: String); virtual;
    procedure DoUnSubscribe(const aChannel: String); virtual;
  protected
    procedure DoUnSubscriptions(aConnection: TsgcWSConnection); virtual;
  public
    property Subscriptions: TStringList read GetSubscriptions
      write FSubscriptions;
  public
    procedure Subscribe(const aChannel: String; const aGuid: String = '');
      virtual; abstract;
    procedure UnSubscribe(const aChannel: String; const aGuid: String = '');
      virtual; abstract;
  public
    property OnSubscription: TsgcWSSubscriptionEvent read FOnSubscription
      write FOnSubscription;
    property OnUnSubscription: TsgcWSSubscriptionEvent read FOnUnSubscription
      write FOnUnSubscription;
    { subscription }
  End;

  TsgcWSAckClient_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FInterval: Integer;
    FTimeout: Integer;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property Interval: Integer read FInterval write FInterval;
    property Timeout: Integer read FTimeout write FTimeout;
  end;

  TsgcWSProtocol_Acknowledgment_Client_Base = class(TsgcWSProtocol_Client_Base)

    { from TsgcWSComponent }
  private
    FWSConnection: TsgcWSConnection;
  protected
    procedure DoEventConnect(aConnection: TsgcWSConnection); override;
    procedure DoEventDisconnect(aConnection: TsgcWSConnection;
      Code: Integer); override;
    { from TsgcWSComponent }

    { from TsgcWSProtocol }
  protected
    procedure DoInitialize(aConnection: TsgcWSConnection); override;
    procedure DoFinalize(aConnection: TsgcWSConnection); override;
    { from TsgcWSProtocol }

    { AcknowledgmentList }
  private
    FAcknowledgmentList: TsgcWSAckMessageList;
    function GetAcknowledgmentList: TsgcWSAckMessageList;
  protected
    property AcknowledgmentList: TsgcWSAckMessageList read GetAcknowledgmentList
      write FAcknowledgmentList;
    { AcknowledgmentList }

    { Acknowledgment }
  private
    FAcknowledgment: TsgcWSAckClient_Options;
    procedure SetAcknowledgment(const Value: TsgcWSAckClient_Options);
  protected
    property Acknowledgment: TsgcWSAckClient_Options read FAcknowledgment
      write SetAcknowledgment;
    { Acknowledgment }

    { events }
  protected
    procedure OnExceptionEvent(Sender: TObject; E: Exception); virtual;
    procedure OnAcknowledgmentMessageEvent(aConnection: TsgcWSConnection;
      const aText: String); virtual; abstract;
    { events }

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;

{$ENDIF}

implementation

{$IFDEF SGC_PROTOCOLS}

uses
  sgcWebSocket_Helpers, sgcWebSocket_Protocol_Broker_Message,
  sgcWebSocket_Classes_SyncObjs;

const
  CS_SUBSCRIPTIONS = 0;

procedure TsgcWSProtocol_Client_Base.WriteData(const aText: String);
var
  oBroker: TsgcWSBrokerMessage;
begin
  if GetWSClient <> nil then
  begin
    if Assigned(Broker) then
    begin
      oBroker := Broker.WSBrokerMessageWrite;
      oBroker.ID := Guid + '.' + Protocol;
      oBroker.Text := aText;

      GetWSClient.WriteData(oBroker.Write);
    end
    else
      GetWSClient.WriteData(aText);
  end;
end;

destructor TsgcWSProtocol_Subscription_Client_Base.Destroy;
begin
{$IFNDEF SGC_EVENT_DISPATCH}
  sgcFree(FNotifySubscription);
  sgcFree(FNotifyUnSubscription);
{$ENDIF}
  sgcFree(FSubscriptions);
  inherited;
end;

{$IFNDEF SGC_EVENT_DISPATCH}

procedure TsgcWSProtocol_Subscription_Client_Base.DoAsyncSubscription;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TsgcWSNotifyObject>{$ENDIF};
  oObject: TObject;
begin
  if IsDestroying then
    exit;
  if not Assigned(FNotifySubscription) then
    exit;

  oList := FNotifySubscription.LockList;
  Try
    if FAsyncSubscription then
      exit;
    FAsyncSubscription := True;
    Try
      for i := oList.Count - 1 Downto 0 do
      begin
        DoEventSubscription(TsgcWSNotifyObject(oList.Items[i]).Connection,
          TsgcWSNotifyObject(oList.Items[i]).Text);
        if not Assigned(FNotifySubscription) then
          exit;
        oObject := TObject(oList.Items[i]);
        sgcFree(oObject);
        oList.Delete(i);
      end;
    Finally
      FAsyncSubscription := False;
    End;
  Finally
    if Assigned(FNotifySubscription) then
      FNotifySubscription.UnlockList;
  End;
end;

procedure TsgcWSProtocol_Subscription_Client_Base.DoAsyncUnSubscription;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TsgcWSNotifyObject>{$ENDIF};
  oObject: TObject;
begin
  if IsDestroying then
    exit;
  if not Assigned(FNotifyUnSubscription) then
    exit;

  oList := FNotifyUnSubscription.LockList;
  Try
    if FAsyncUnSubscription then
      exit;
    FAsyncUnSubscription := True;
    Try
      for i := oList.Count - 1 Downto 0 do
      begin
        DoEventUnSubscription(TsgcWSNotifyObject(oList.Items[i]).Connection,
          TsgcWSNotifyObject(oList.Items[i]).Text);
        if not Assigned(FNotifyUnSubscription) then
          exit;
        oObject := TObject(oList.Items[i]);
        sgcFree(oObject);
        oList.Delete(i);
      end;
    Finally
      FAsyncUnSubscription := False;
    End;
  Finally
    if Assigned(FNotifyUnSubscription) then
      FNotifyUnSubscription.UnlockList;
  End;
end;

function TsgcWSProtocol_Subscription_Client_Base.GetNotifySubscription
  : TsgcWSObjectList;
begin
  if not Assigned(FNotifySubscription) then
    FNotifySubscription := TsgcWSObjectList.Create;
  Result := FNotifySubscription;
end;

function TsgcWSProtocol_Subscription_Client_Base.GetNotifyUnSubscription
  : TsgcWSObjectList;
begin
  if not Assigned(FNotifyUnSubscription) then
    FNotifyUnSubscription := TsgcWSObjectList.Create;
  Result := FNotifyUnSubscription;
end;
{$ENDIF}

procedure TsgcWSProtocol_Subscription_Client_Base.DoEventDisconnect
  (aConnection: TsgcWSConnection; aCode: Integer);
begin
  DoUnSubscriptions(aConnection);
  inherited;
end;

procedure TsgcWSProtocol_Subscription_Client_Base.DoEventSubscription
  (aConnection: TsgcWSConnection; const aSubscription: String);
begin
{$IFDEF SGC_DEBUG}
  DoLog(self, aConnection, 'DoEventSubscription', '[Subscription]: ' +
    aSubscription);
{$ENDIF}
  if Assigned(FOnSubscription) then
    FOnSubscription(aConnection, aSubscription);
end;

procedure TsgcWSProtocol_Subscription_Client_Base.DoEventUnSubscription
  (aConnection: TsgcWSConnection; const aSubscription: String);
begin
{$IFDEF SGC_DEBUG}
  DoLog(self, aConnection, 'DoEventUnSubscription',
    '[Subscription]: ' + aSubscription);
{$ENDIF}
  if Assigned(FOnUnSubscription) then
    FOnUnSubscription(aConnection, aSubscription);
end;

procedure TsgcWSProtocol_Subscription_Client_Base.DoNotifySubscription
  (aConnection: TsgcWSConnection);
var
  vSubscription: string;
begin
  if IsDestroying then
    exit;

  if Assigned(aConnection) then
  begin
    vSubscription := aConnection.LastSubscription;
    DoSubscribe(vSubscription);

    case NotifyEvents of
      neNoSync:
        DoEventSubscription(aConnection, vSubscription);
      neAsynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Queue(nil,
            procedure
            begin
              DoEventSubscription(aConnection, vSubscription);
            end);
{$ELSE}
          NotifySubscription.AddNotifyObject(aConnection, vSubscription);
          NotifyMethod(DoAsyncSubscription);
{$ENDIF}
        end;
      neSynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Synchronize(nil,
            procedure
            begin
              DoEventSubscription(aConnection, vSubscription);
            end);
{$ELSE}
          NotifySubscription.AddNotifyObject(aConnection, vSubscription);
          SynchronizeMethod(DoAsyncSubscription);
{$ENDIF}
        end;
    end;
  end;
end;

procedure TsgcWSProtocol_Subscription_Client_Base.DoNotifyUnSubscription
  (aConnection: TsgcWSConnection);
var
  vUnSubscription: string;
begin
  if IsDestroying then
    exit;

  if Assigned(aConnection) then
  begin
    vUnSubscription := aConnection.LastUnSubscription;
    DoUnSubscribe(vUnSubscription);
    case NotifyEvents of
      neNoSync:
        DoEventUnSubscription(aConnection, vUnSubscription);
      neAsynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Queue(nil,
            procedure
            begin
              DoEventUnSubscription(aConnection, vUnSubscription);
            end);
{$ELSE}
          NotifyUnSubscription.AddNotifyObject(aConnection, vUnSubscription);
          NotifyMethod(DoAsyncUnSubscription);
{$ENDIF}
        end;
      neSynchronous:
        begin
{$IFDEF SGC_EVENT_DISPATCH}
          TThread.Synchronize(nil,
            procedure
            begin
              DoEventUnSubscription(aConnection, vUnSubscription);
            end);
{$ELSE}
          NotifyUnSubscription.AddNotifyObject(aConnection, vUnSubscription);
          SynchronizeMethod(DoAsyncUnSubscription);
{$ENDIF}
        end;
    end;
  end;
end;

procedure TsgcWSProtocol_Subscription_Client_Base.DoSubscribe
  (const aChannel: String);
begin
  DoEnterCS(CS_SUBSCRIPTIONS);
  Try
    if Subscriptions.IndexOf(aChannel) = -1 then
      Subscriptions.Add(aChannel);
  Finally
    DoLeaveCS(CS_SUBSCRIPTIONS);
  End;
end;

procedure TsgcWSProtocol_Subscription_Client_Base.DoUnSubscribe
  (const aChannel: String);
var
  i: Integer;
begin
  DoEnterCS(CS_SUBSCRIPTIONS);
  Try
    i := Subscriptions.IndexOf(aChannel);
    if i <> -1 then
      Subscriptions.Delete(i);
  Finally
    DoLeaveCS(CS_SUBSCRIPTIONS);
  End;
end;

procedure TsgcWSProtocol_Subscription_Client_Base.DoUnSubscriptions
  (aConnection: TsgcWSConnection);
var
  i: Integer;
begin
  if Subscriptions.Count > 0 then
  begin
    DoEnterCS(CS_SUBSCRIPTIONS);
    Try
      for i := Subscriptions.Count - 1 DownTo 0 do
      begin
        if Assigned(FOnUnSubscription) then
          FOnUnSubscription(aConnection, Subscriptions[i]);
        DoUnSubscribe(Subscriptions[i]);
      end;
    Finally
      DoLeaveCS(CS_SUBSCRIPTIONS);
    End;
  end;
end;

function TsgcWSProtocol_Subscription_Client_Base.GetSubscriptions: TStringList;
begin
  if not Assigned(FSubscriptions) then
    FSubscriptions := TStringList.Create;
  Result := FSubscriptions;
end;

constructor TsgcWSProtocol_Client_Base.Create(aOwner: TComponent);
begin
  inherited;
  FBroker := nil;
end;

function TsgcWSProtocol_Client_Base.GetWSClient: TsgcWSComponent_WSClient;
begin
  if Assigned(Broker) then
    Result := Broker.Client
  else
    Result := Client;
end;

procedure TsgcWSProtocol_Client_Base.Loaded;
begin
  inherited;
  if Assigned(Broker) then
    Broker.DoRegisterProtocol(self);
end;

procedure TsgcWSProtocol_Client_Base.Notification(AComponent: TComponent;
Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FBroker) then
    FBroker := nil;
end;

procedure TsgcWSProtocol_Client_Base.SetBroker(const Value
  : TsgcWSProtocol_Broker_Client);
begin
  if Assigned(FBroker) then
    FBroker.RemoveFreeNotification(self);

  if [csDesigning, csLoading] * ComponentState = [] then
  begin
    if Assigned(FBroker) then
    begin
      if not Assigned(Value) then
      begin
        FBroker.DoUnRegisterProtocol(self);
        FBroker := Value;
      end
      else if FBroker <> Value then
      begin
        FBroker := Value;
        FBroker.DoRegisterProtocol(self);
      end;
    end
    else
    begin
      FBroker := Value;
      if Assigned(Value) then
        FBroker.DoRegisterProtocol(self);
    end;
  end
  else
    FBroker := Value;

  if Assigned(FBroker) then
    FBroker.FreeNotification(self);
end;

procedure TsgcWSProtocol_Client_Base.WriteData(aStream: TStream;
aSize: Integer = 0; const aStreaming: TwsStreaming = stmNone);
var
  oBroker: TsgcWSBrokerBinary;
begin
  if GetWSClient <> nil then
  begin
    if Assigned(Broker) then
    begin
      oBroker := Broker.WSBrokerBinaryWrite;
      oBroker.ID := Guid + '.' + Protocol;
      oBroker.Write(TMemoryStream(aStream));

      GetWSClient.WriteData(aStream, aSize, aStreaming);
    end
    else
      GetWSClient.WriteData(aStream, aSize, aStreaming);
  end;
end;

constructor TsgcWSProtocol_Acknowledgment_Client_Base.Create
  (aOwner: TComponent);
begin
  inherited;
  FAcknowledgment := TsgcWSAckClient_Options.Create;
end;

destructor TsgcWSProtocol_Acknowledgment_Client_Base.Destroy;
begin
  sgcFree(FAcknowledgment);
  sgcFree(FAcknowledgmentList);
  inherited;
end;

procedure TsgcWSProtocol_Acknowledgment_Client_Base.DoEventConnect
  (aConnection: TsgcWSConnection);
begin
  FWSConnection := aConnection;
  inherited;
  if Acknowledgment.Enabled then
  begin
    AcknowledgmentList.Interval := Acknowledgment.Interval;
    AcknowledgmentList.Timeout := Acknowledgment.Timeout;
    AcknowledgmentList.Start;
  end;
end;

procedure TsgcWSProtocol_Acknowledgment_Client_Base.DoEventDisconnect
  (aConnection: TsgcWSConnection; Code: Integer);
begin
  AcknowledgmentList.Stop;
  inherited;
  FWSConnection := nil;
end;

procedure TsgcWSProtocol_Acknowledgment_Client_Base.DoFinalize
  (aConnection: TsgcWSConnection);
begin
  AcknowledgmentList.Stop;
  inherited;
  FWSConnection := nil;
end;

procedure TsgcWSProtocol_Acknowledgment_Client_Base.DoInitialize
  (aConnection: TsgcWSConnection);
begin
  inherited;
  AcknowledgmentList.Start;
end;

function TsgcWSProtocol_Acknowledgment_Client_Base.GetAcknowledgmentList
  : TsgcWSAckMessageList;
begin
  if not Assigned(FAcknowledgmentList) then
  begin
    FAcknowledgmentList := TsgcWSAckMessageList.Create;
    FAcknowledgmentList.ListType := tdlThreadDictionary;
    FAcknowledgmentList.OnException := OnExceptionEvent;
    FAcknowledgmentList.OnMessage := OnAcknowledgmentMessageEvent;
  end;
  Result := FAcknowledgmentList;
end;

procedure TsgcWSProtocol_Acknowledgment_Client_Base.OnExceptionEvent
  (Sender: TObject; E: Exception);
begin
  DoEventException(FWSConnection, E.Message, E);
end;

procedure TsgcWSProtocol_Acknowledgment_Client_Base.SetAcknowledgment
  (const Value: TsgcWSAckClient_Options);
begin
  FAcknowledgment.Assign(Value);
end;

constructor TsgcWSAckClient_Options.Create;
begin
  inherited;
  Enabled := False;
  Interval := 300;
  Timeout := 240;
end;

procedure TsgcWSAckClient_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSAckClient_Options then
  begin
    Enabled := TsgcWSAckClient_Options(aSource).Enabled;
    Interval := TsgcWSAckClient_Options(aSource).Interval;
    Timeout := TsgcWSAckClient_Options(aSource).Timeout;
  end
  else
    inherited Assign(aSource);
end;

{$ENDIF}

end.
