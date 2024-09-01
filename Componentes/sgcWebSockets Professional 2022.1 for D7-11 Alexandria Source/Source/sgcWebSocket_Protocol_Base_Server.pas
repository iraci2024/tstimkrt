{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcWebSocket_Protocol_Base_Server;

interface

{$I sgcVer.inc}

{$IFDEF SGC_PROTOCOLS}

uses
  Classes, SysUtils, StrUtils,
  {$IFDEF NEXTGEN}System.Generics.Collections, {$ELSE}Contnrs, {$ENDIF}
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}
  // indy
  {$IFDEF SGC_INDY}sgcIdContext{$ELSE}IdContext{$ENDIF},
  // sgc
  sgcWebSocket_Protocol_Base_Message, sgcWebSocket_HTTPResponse,
  sgcWebSocket_Classes, sgcWebSocket_Types,
  sgcWebSocket_Protocol_Broker_Server, sgcWebSocket_Helpers;

type

  TsgcWSProtocol_Server_Base = class;
  TsgcWSProtocol_Subscription_Server_Base = class;
  TsgcWSProtocol_Acknowledgment_Server_Base = class;

  TsgcWSProtocol_JS_Base = class(TsgcWSHTTPResponse_Base)
  protected
    function GetResponse: string; override;
  public
    class function GetFileName: string; override;
  end;

  TsgcWSProtocol_JS_Flash = class(TsgcWSHTTPResponse_Base)
  protected
    function GetResponse: string; override;
  public
    class function GetFileName: string; override;
  end;

  TsgcWSProtocol_JS_EventSource = class(TsgcWSHTTPResponse_Base)
  protected
    function GetResponse: string; override;
  public
    class function GetFileName: string; override;
  end;


  TsgcWSProtocol_HTML_Base = class(TsgcWSHTTPResponse_Base)
  protected
    function GetResponse: string; override;
  public
    class function GetFileName: string; override;
  end;

  TsgcWSProtocol_Server_Base = Class(TsgcWSProtocol_Server)
  { from TComponent }
  protected
    procedure Notification(AComponent : TComponent; Operation : TOperation);
        override;
  { from TComponent }

  { from TsgcWSProtocol_Server }
  protected
    procedure Loaded; override;
  { from TsgcWSProtocol_Server }

  { broker component }
  private
    FBroker: TsgcWSProtocol_Broker_Server;
  protected
    procedure SetBroker(const Value: TsgcWSProtocol_Broker_Server); virtual;
  public
    property Broker: TsgcWSProtocol_Broker_Server read FBroker write SetBroker;
  { broker component }

  { constructor }
  public
    constructor Create(aOwner: TComponent); override;
  { constructor }

  { default server }
  protected
    function GetWSServer: TsgcWSComponent_Server;
  { default server }

  { send messages }
  protected
    function BroadcastConnection(aConnection: TsgcWSConnection; aChannel: string;
        aExclude, aInclude: TsgcDelimitedStringList): Boolean; virtual;
    procedure GetBroadcastConnections(aChannel, Exclude, Include: string; var
        Connections: TList{$IFDEF NEXTGEN}<TsgcWSConnection>{$ENDIF}); virtual;
  protected
    procedure Broadcast(aMessage: string; aChannel: string = ''; Exclude: String =
        ''; Include: String = ''); overload; virtual;
    procedure Broadcast(aStream: TStream; aChannel: string = ''; Exclude: String =
        ''; Include: String = ''; aSize: Integer = 0; aStreaming: TwsStreaming =
        stmNone); overload; virtual;
    function WriteData(aGuid, aMessage: string): Boolean; overload; virtual;
    function WriteData(aGuid: string; aStream: TStream; aSize: Integer = 0;
        aStreaming: TwsStreaming = stmNone): Boolean; overload; virtual;
    function WriteData(aConnection: TsgcWSConnection; aMessage: String): Boolean;
        overload; virtual;
    procedure WriteData(aConnection: TsgcWSConnection; aStream: TStream); overload;
        virtual;
  { send message }
  End;

  TsgcWSProtocol_Subscription_Server_Base = class(TsgcWSProtocol_Server_Base)
  { from TsgcWSComponent }
  protected
      procedure DoEventDisconnect(aConnection: TsgcWSConnection; aCode: Integer);
          override;
  { from TsgcWSComponent }

  { subscription }
  private
    FOnSubscription: TsgcWSSubscriptionEvent;
    FOnUnSubscription: TsgcWSSubscriptionEvent;
  private
    FOnBeforeSubscription: TsgcWSBeforeSubscriptionEvent;
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
    property NotifyUnSubscription: TsgcWSObjectList read GetNotifyUnSubscription;
  {$ENDIF}
  protected
    procedure DoEventBeforeSubscription(aConnection: TsgcWSConnection; const
        Subscription: String; var Accept: Boolean); virtual;
    procedure DoEventSubscription(aConnection: TsgcWSConnection; const
        Subscription: String); virtual;
    procedure DoEventUnSubscription(aConnection: TsgcWSConnection; const
        Subscription: String); virtual;
  protected
    procedure DoNotifySubscription(aConnection: TsgcWSConnection); virtual;
    procedure DoNotifyUnSubscription(aConnection: TsgcWSConnection); virtual;
  private
    FSubscriptions: TsgcQueueListChannels;
      FUseMatchesMask: Boolean;
    function GetSubscriptions: TsgcQueueListChannels;
  protected
    procedure DoSubscribe(aChannel: String; aConnection: TsgcWSConnection); virtual;
    procedure DoUnSubscribe(aChannel: String; aConnection: TsgcWSConnection);
        virtual;
  protected
    procedure DoUnSubscriptions(aConnection: TsgcWSConnection); virtual;
  protected
    procedure GetChannelsFromSubscriptions(const aChannel: String; var Channels:
        TStringList); virtual;
  public
    property Subscriptions: TsgcQueueListChannels read GetSubscriptions write
        FSubscriptions;
    property UseMatchesMask: Boolean read FUseMatchesMask write FUseMatchesMask;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  public
    property OnBeforeSubscription: TsgcWSBeforeSubscriptionEvent read
        FOnBeforeSubscription write FOnBeforeSubscription;
     property OnSubscription: TsgcWSSubscriptionEvent read FOnSubscription write
        FOnSubscription;
    property OnUnSubscription: TSgcWSSubscriptionEvent read FOnUnSubscription write
        FOnUnSubscription;
  End;

  TsgcWSAckServer_Options = class(TPersistent)
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

  TsgcWSProtocol_Acknowledgment_Server_Base = class(TsgcWSProtocol_Server_Base)
  { from TComponent }
  protected
    procedure Loaded; override;
  { from TComponent }

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
    FAcknowledgment: TsgcWSAckServer_Options;
    procedure SetAcknowledgment(const Value: TsgcWSAckServer_Options);
  protected
    property Acknowledgment: TsgcWSAckServer_Options read FAcknowledgment write
        SetAcknowledgment;
  { Acknowledgment }

  { events }
  protected
    procedure OnExceptionEvent(Sender: TObject; E: Exception); virtual;
    procedure OnAcknowledgmentMessageEvent(aConnection: TsgcWSConnection; const
        aText: String); virtual; abstract;
  { events }

  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  end;


{$ENDIF}


implementation

{$IFDEF SGC_PROTOCOLS}

uses
  sgcWebSocket_Protocol_Broker_Message, sgcWebSocket_Classes_SyncObjs,
  sgcWebSocket_Const, sgcWebSocket_Resources, sgcWebSocket_Server_Base;

{$R sgcResources.RES}

Type
  TsgcHackWSComponent_Server = class(TsgcWSComponent_Server);

function TsgcWSProtocol_JS_Base.GetResponse: string;
begin
  result := GetResourceString(CS_SGC_JS_SGCWEBSOCKETS);
end;

class function TsgcWSProtocol_JS_Base.GetFileName: string;
begin
  Result := CS_JS_SGCWEBSOCKETS;
end;

constructor TsgcWSProtocol_Server_Base.Create(aOwner: TComponent);
begin
  inherited;
  FBroker := nil;
end;

procedure TsgcWSProtocol_Server_Base.Broadcast(aMessage: string; aChannel:
    string = ''; Exclude: String = ''; Include: String = '');
var
  oBroker: TsgcWSBrokerMessage;
begin
  if GetWSServer <> nil then
  begin
    if Assigned(Broker) then
    begin
      oBroker := TsgcWSBrokerMessage.Create(nil);
      Try
        oBroker.ID := Guid + '.' + Protocol;
        oBroker.Text := aMessage;

        GetWSServer.Broadcast(oBroker.Write, aChannel, CS_PROTOCOL_BROKER, Exclude, Include);
      Finally
        sgcFree(oBroker);
      End;
    end
    else
    begin
      if FGuid <> '' then
        GetWSServer.Broadcast(aMessage, aChannel, FGuid + '.' + FProtocol, Exclude, Include)
      else
        GetWSServer.Broadcast(aMessage, aChannel, FProtocol, Exclude, Include);
    end;
  end;
end;

procedure TsgcWSProtocol_Server_Base.Broadcast(aStream: TStream; aChannel:
    string = ''; Exclude: String = ''; Include: String = ''; aSize: Integer =
    0; aStreaming: TwsStreaming = stmNone);
var
  oBroker: TsgcWSBrokerBinary;
begin
  if GetWSServer <> nil then
  begin
    if Assigned(Broker) then
    begin
      oBroker := TsgcWSBrokerBinary.Create(nil);
      Try
        oBroker.ID := Guid + '.' + Protocol;
        oBroker.Write(TMemoryStream(aStream));
        GetWSServer.Broadcast(aStream, aChannel, CS_PROTOCOL_BROKER, Exclude, Include, aSize, aStreaming);
      Finally
        sgcFree(oBroker);
      End;
    end
    else
    begin
      if FGuid <> '' then
        GetWSServer.Broadcast(aStream, aChannel, FGuid + '.' + FProtocol, Exclude, Include, aSize, aStreaming)
      else
        GetWSServer.Broadcast(aStream, aChannel, FProtocol, Exclude, Include, aSize, aStreaming);
    end;
  end;
end;

function TsgcWSProtocol_Server_Base.BroadcastConnection(aConnection:
    TsgcWSConnection; aChannel: string; aExclude, aInclude:
    TsgcDelimitedStringList): Boolean;
var
  vExclude, vInclude: Boolean;
  vChannel: String;

begin
  Result := False;
  Try
    if Assigned(aConnection) then
    begin
      if aConnection.Enabled then
      begin
        // ... exclude
        if Assigned(aExclude) then
          vExclude := aExclude.IndexOf(aConnection.guid) <> -1
        else
          vExclude := False;
        // ... include
        if Assigned(aInclude) then
          vInclude := aInclude.IndexOf(aConnection.guid) <> -1
        else
          vInclude := True;
        // ... broadcast
        if (vExclude = False) and (vInclude = True) then
        begin
          vChannel := aChannel;
          if (vChannel <> '') and (LeftStr(vChannel, 1) <> '_') then
            vChannel := guid + '_' + vChannel;
          if Assigned(Broker) then
          begin
            if UpperCase(aConnection.Protocol) = UpperCase(CS_PROTOCOL_BROKER)
            then
            begin
              if aConnection.Subscribed(vChannel) then
                Result := True;
            end;
          end
          else
          begin
            if UpperCase(aConnection.Protocol) = UpperCase(Protocol) then
            begin
              if aConnection.Subscribed(vChannel) then
                Result := True;
            end;
          end;
        end;
      end;
    end;
  Except
    On E: Exception do
      DoError(aConnection, E.Message);
  end;
end;

procedure TsgcWSProtocol_Server_Base.GetBroadcastConnections(aChannel, Exclude,
    Include: string; var Connections:
    TList{$IFDEF NEXTGEN}<TsgcWSConnection>{$ENDIF});
var
  i: Integer;
  oConnection: TsgcWSConnection;
  oListIndy: TList{$IFDEF NEXTGEN}<TIdContext>{$ENDIF};
  oListBase: TList{$IFDEF NEXTGEN}<TsgcWSConnection>{$ENDIF};
  oExclude: TsgcDelimitedStringList;
  oInclude: TsgcDelimitedStringList;
begin
  oExclude := nil;
  oInclude := nil;

  // ... exclude list
  if Exclude <> '' then
  begin
    oExclude := TsgcDelimitedStringList.Create;
    oExclude.DelimitedText := Exclude;
  end;
  // ... include list
  if Include <> '' then
  begin
    oInclude := TsgcDelimitedStringList.Create;
    oInclude.DelimitedText := Include;
  end;
  // ... broadcast
  Try
    // server base
    if Server.InheritsFrom(TsgcWSServer_Base) then
    begin
      oListBase := TsgcWSServer_Base(Server).LockList;
      Try
        for i := 0 to oListBase.Count - 1 do
        begin
          oConnection := TsgcWSConnection(oListBase[i]);
          if BroadcastConnection(oConnection, aChannel, oExclude, oInclude)
          then
            Connections.Add(oConnection);
        end;
      Finally
        Server.UnlockList;
      End;
    end
    else
    // indy server
    begin
      oListIndy := Server.LockList;
      Try
        for i := 0 to oListIndy.Count - 1 do
        begin
          oConnection := TsgcWSConnection(TIdContext(oListIndy[i])
            .{$IFDEF NEXTGEN}DataObject{$ELSE}Data{$ENDIF});
          if BroadcastConnection(oConnection, aChannel, oExclude, oInclude)
          then
            Connections.Add(oConnection);
        end;
      Finally
        Server.UnlockList;
      End;
    end;
  Finally
    sgcFree(oExclude);
    sgcFree(oInclude);
  End;
end;

function TsgcWSProtocol_Server_Base.GetWSServer: TsgcWSComponent_Server;
begin
  if Assigned(Broker) then
    result := Broker.Server
  else
    result := Server;
end;

procedure TsgcWSProtocol_Server_Base.Loaded;
begin
  inherited;
  if Assigned(Broker) then
    Broker.DoRegisterProtocol(self);
end;

procedure TsgcWSProtocol_Server_Base.Notification(AComponent : TComponent;
    Operation : TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FBroker) then
    FBroker := nil;
end;

procedure TsgcWSProtocol_Server_Base.SetBroker(const Value:
    TsgcWSProtocol_Broker_Server);
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

function TsgcWSProtocol_Server_Base.WriteData(aGuid, aMessage: string): Boolean;
var
  oBroker: TsgcWSBrokerMessage;
  oConnection: TsgcWSConnection;
begin
  result := False;
  if GetWSServer <> nil then
  begin
    if Assigned(Broker) then
    begin
      oBroker := TsgcWSBrokerMessage.Create(nil);
      Try
        oBroker.ID := Guid + '.' + Protocol;
        oBroker.Text := aMessage;

        oConnection := nil;
        if TsgcHackWSComponent_Server(GetWSServer).Optimizations.Connections.Enabled then
          oConnection := TsgcHackWSComponent_Server(GetWSServer).GetConnections.GetConnection(aGuid);
        if Assigned(oConnection) then
        begin
          if oConnection.Protocol = CS_PROTOCOL_BROKER then // verify protocol is broker
            oConnection.WriteData(oBroker.Write)
          else
            oConnection.WriteData(aMessage);
          result := True;
        end
        else
          result := GetWSServer.WriteData(aGuid, oBroker.Write);
      Finally
        sgcFree(oBroker);
      End;
    end
    else
      result := GetWSServer.WriteData(aGuid, aMessage);
  end;
end;

function TsgcWSProtocol_Server_Base.WriteData(aGuid: string; aStream: TStream;
    aSize: Integer = 0; aStreaming: TwsStreaming = stmNone): Boolean;
var
  oBroker: TsgcWSBrokerBinary;
  oConnection: TsgcWSConnection;
begin
  result := False;
  if GetWSServer <> nil then
  begin
    if Assigned(Broker) then
    begin
      oBroker := TsgcWSBrokerBinary.Create(nil);
      Try
        oBroker.ID := Guid + '.' + Protocol;

        oConnection := nil;
        if TsgcHackWSComponent_Server(GetWSServer).Optimizations.Connections.Enabled then
          oConnection := TsgcHackWSComponent_Server(GetWSServer).GetConnections.GetConnection(aGuid);
        if Assigned(oConnection) then
        begin
          if oConnection.Protocol = CS_PROTOCOL_BROKER then  // verify protocol is broker
          begin
            oBroker.Write(TMemoryStream(aStream));
            oConnection.WriteData(aStream, aSize, aStreaming)
          end
          else
            oConnection.WriteData(aStream, aSize, aStreaming);
          result := True;
        end
        else
        begin
          oBroker.Write(TMemoryStream(aStream));
          result := GetWSServer.WriteData(aGuid, aStream, aSize, aStreaming);
        end;
      Finally
        sgcFree(oBroker);
      End;
    end
    else
      result := GetWSServer.WriteData(aGuid, aStream, aSize, aStreaming);
  end;
end;

function TsgcWSProtocol_Server_Base.WriteData(aConnection: TsgcWSConnection;
    aMessage: String): Boolean;
var
  oBroker: TsgcWSBrokerMessage;
begin
  result := False;
  if Assigned(aConnection) then
  begin
    result := True;
    if Assigned(Broker) and (aConnection.Protocol = CS_PROTOCOL_BROKER) then
    begin
      oBroker := TsgcWSBrokerMessage.Create(nil);
      Try
        oBroker.ID := Guid + '.' + Protocol;
        oBroker.Text := aMessage;

        aConnection.WriteData(oBroker.Write);
      Finally
        sgcFree(oBroker);
      End;
    end
    else
      aConnection.WriteData(aMessage);
  end;
end;

procedure TsgcWSProtocol_Server_Base.WriteData(aConnection: TsgcWSConnection;
    aStream: TStream);
var
  oBroker: TsgcWSBrokerBinary;
begin
  if Assigned(aConnection) then
  begin
    if Assigned(Broker) and (aConnection.Protocol = CS_PROTOCOL_BROKER) then
    begin
      oBroker := TsgcWSBrokerBinary.Create(nil);
      Try
        oBroker.ID := Guid + '.' + Protocol;
        oBroker.Write(TMemoryStream(aStream));
        aConnection.WriteData(aStream)
      Finally
        sgcFree(oBroker);
      End;
    end
    else
      aConnection.WriteData(aStream);
  end;
end;

class function TsgcWSProtocol_HTML_Base.GetFileName: string;
begin
  Result := CS_HTML_SGCWEBSOCKETS;
end;

function TsgcWSProtocol_HTML_Base.GetResponse: string;
begin
  result := GetResourceString(CS_SGC_HTML_SGCWEBSOCKETS);
end;

constructor TsgcWSProtocol_Subscription_Server_Base.Create(aOwner: TComponent);
begin
  inherited;
  NotifyEvents := neNoSync;
  UseMatchesMask := False;
end;

destructor TsgcWSProtocol_Subscription_Server_Base.Destroy;
begin
  {$IFNDEF SGC_EVENT_DISPATCH}
  sgcFree(FNotifySubscription);
  sgcFree(FNotifyUnSubscription);
  {$ENDIF}
  sgcFree(FSubscriptions);
  inherited;
end;

{$IFNDEF SGC_EVENT_DISPATCH}
procedure TsgcWSProtocol_Subscription_Server_Base.DoAsyncSubscription;
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

procedure TsgcWSProtocol_Subscription_Server_Base.DoAsyncUnSubscription;
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

function TsgcWSProtocol_Subscription_Server_Base.GetNotifySubscription:
    TsgcWSObjectList;
begin
  if not Assigned(FNotifySubscription) then
    FNotifySubscription := TsgcWSObjectList.Create;
  Result := FNotifySubscription;
end;

function TsgcWSProtocol_Subscription_Server_Base.GetNotifyUnSubscription:
    TsgcWSObjectList;
begin
  if not Assigned(FNotifyUnSubscription) then
    FNotifyUnSubscription := TsgcWSObjectList.Create;
  Result := FNotifyUnSubscription;
end;
{$ENDIF}

procedure TsgcWSProtocol_Subscription_Server_Base.DoEventBeforeSubscription(
    aConnection: TsgcWSConnection; const Subscription: String; var Accept:
    Boolean);
begin
  {$IFDEF SGC_DEBUG}
  DoLog(self, aConnection, 'DoEventBeforeSubscription', '[Subscription]: ' + Subscription);
  {$ENDIF}

  if Assigned(FOnBeforeSubscription) then
    FOnBeforeSubscription(aConnection, Subscription, Accept);
end;

procedure TsgcWSProtocol_Subscription_Server_Base.DoEventDisconnect(
    aConnection: TsgcWSConnection; aCode: Integer);
begin
  DoUnSubscriptions(aConnection);
  inherited;
end;

procedure TsgcWSProtocol_Subscription_Server_Base.DoEventSubscription(
    aConnection: TsgcWSConnection; const Subscription: String);
begin
  {$IFDEF SGC_DEBUG}
  DoLog(self, aConnection, 'DoEventSubscription', '[Subscription]: ' + Subscription);
  {$ENDIF}

  DoSubscribe(Subscription, aConnection);

  if Assigned(FOnSubscription) then
    FOnSubscription(aConnection, Subscription);
end;

procedure TsgcWSProtocol_Subscription_Server_Base.DoEventUnSubscription(
    aConnection: TsgcWSConnection; const Subscription: String);
begin
  {$IFDEF SGC_DEBUG}
  DoLog(self, aConnection, 'DoEventUnSubscription', '[Subscription]: ' + Subscription);
  {$ENDIF}

  DoUnSubscribe(Subscription, aConnection);

  if Assigned(FOnUnSubscription) then
    FOnUnSubscription(aConnection, Subscription);
end;

procedure TsgcWSProtocol_Subscription_Server_Base.DoNotifySubscription(
    aConnection: TsgcWSConnection);
var
  vSubscription: string;
begin
  if IsDestroying then
    exit;

  if Assigned(aConnection) then
  begin
    vSubscription := aConnection.LastSubscription;
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

procedure TsgcWSProtocol_Subscription_Server_Base.DoNotifyUnSubscription(
    aConnection: TsgcWSConnection);
var
  vUnSubscription: string;
begin
  if Assigned(aConnection) then
  begin
    vUnSubscription := aConnection.LastUnSubscription;
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

procedure TsgcWSProtocol_Subscription_Server_Base.DoSubscribe(aChannel: String;
    aConnection: TsgcWSConnection);
var
  oItem: TsgcQueueItemChannel;
begin
  if Assigned(aConnection) then
  begin
    oItem := TsgcQueueItemChannel.Create;
    oItem.Connection := aConnection;
    oItem.Queue := aChannel;
    if not Subscriptions.AddItem(oItem) then
      sgcFree(oItem);
  end;
end;

procedure TsgcWSProtocol_Subscription_Server_Base.DoUnSubscribe(aChannel:
    String; aConnection: TsgcWSConnection);
begin
  if Assigned(aConnection) then
    Subscriptions.DeleteItem(aChannel, aConnection.Guid);
end;

procedure TsgcWSProtocol_Subscription_Server_Base.DoUnSubscriptions(
    aConnection: TsgcWSConnection);
var
  i: Integer;
  oItem: TsgcQueueItemChannel;
  oList: TList{$IFDEF NEXTGEN}<TsgcQueueItemChannel>{$ENDIF};
begin
  if Assigned(aConnection) then
  begin
    oList := TList{$IFDEF NEXTGEN}<TsgcQueueItemChannel>{$ENDIF}.Create;
    Try
      Subscriptions.GetChannels(aConnection.Guid, oList);
      for i := oList.Count - 1 DownTo 0 do
      begin
        oItem := TsgcQueueItemChannel(oList[i]);
        if Assigned(oItem) then
        begin
          if Assigned(FOnUnSubscription) then
            FOnUnSubscription(aConnection, oItem.Queue);
          aConnection.DoUnSubscribe(oItem.ID + '_' + oItem.Queue);
          DoUnSubscribe(oItem.Queue, oItem.Connection);
        end;
      end;
    Finally
      sgcFree(oList);
    End;
  end;
end;

procedure TsgcWSProtocol_Subscription_Server_Base.GetChannelsFromSubscriptions(
    const aChannel: String; var Channels: TStringList);
var
  i: Integer;
  vChannel: string;
  oList: TStringList;
begin
  if UseMatchesMask then
  begin
    oList := TStringList.Create;
    Try
      oList.delimitedText := Subscriptions.GetQueuesDelimitedText;
      for i := oList.Count - 1 Downto 0 do
      begin
        vChannel := oList[i];
        if sgcMatchesMask(vChannel, aChannel) then
        begin
          if Channels.IndexOf(vChannel) = -1 then
            Channels.Add(vChannel);
        end;
      end;
    Finally
      sgcFree(oList);
    End;
  end;

  if Channels.Count = 0 then
  begin
    if Pos(CS_DELIMITER, aChannel) > 0 then
    begin
      oList := TStringList.Create;
      Try
        oList.DelimitedText := aChannel;
        Channels.Assign(oList);
      Finally
        sgcFree(oList);
      End;
    end
    else
      Channels.Add(aChannel);
  end;
end;

function TsgcWSProtocol_Subscription_Server_Base.GetSubscriptions:
    TsgcQueueListChannels;
begin
  if not Assigned(FSubscriptions) then
    FSubscriptions := TsgcQueueListChannels.Create;
  Result := FSubscriptions;
end;

class function TsgcWSProtocol_JS_Flash.GetFileName: string;
begin
  Result := CS_JS_SGCWEBSOCKETSFLASH;
end;

function TsgcWSProtocol_JS_Flash.GetResponse: string;
begin
  result := GetResourceString(CS_SGC_JS_SGCWEBSOCKETSFLASH);
end;

class function TsgcWSProtocol_JS_EventSource.GetFileName: string;
begin
  Result := CS_JS_SGCWEBSOCKETSEVENTSOURCE;
end;

function TsgcWSProtocol_JS_EventSource.GetResponse: string;
begin
  result := GetResourceString(CS_SGC_JS_SGCWEBSOCKETSEVENTSOURCE);
end;

constructor TsgcWSProtocol_Acknowledgment_Server_Base.Create(aOwner:
    TComponent);
begin
  inherited;
  FAcknowledgment := TsgcWSAckServer_Options.Create;
end;

destructor TsgcWSProtocol_Acknowledgment_Server_Base.Destroy;
begin
  AcknowledgmentList.Stop;
  sgcFree(FAcknowledgment);
  sgcFree(FAcknowledgmentList);
  inherited;
end;

function TsgcWSProtocol_Acknowledgment_Server_Base.GetAcknowledgmentList:
    TsgcWSAckMessageList;
begin
  if not Assigned(FAcknowledgmentList) then
  begin
    FAcknowledgmentList := TsgcWSAckMessageList.Create;
    FAcknowledgmentList.ListType := tdlThreadDictionary;
    FAcknowledgmentList.OnMessage := OnAcknowledgmentMessageEvent;
    FAcknowledgmentList.OnException := OnExceptionEvent;
  end;
  Result := FAcknowledgmentList;
end;

procedure TsgcWSProtocol_Acknowledgment_Server_Base.Loaded;
begin
  inherited;
  if Acknowledgment.Enabled then
  begin
    AcknowledgmentList.Interval := Acknowledgment.Interval;
    AcknowledgmentList.Timeout := Acknowledgment.Timeout;
    AcknowledgmentList.Start;
  end;
end;

procedure TsgcWSProtocol_Acknowledgment_Server_Base.OnExceptionEvent(Sender:
    TObject; E: Exception);
begin
  DoNotifyException(E.Message, E);
end;

procedure TsgcWSProtocol_Acknowledgment_Server_Base.SetAcknowledgment(const
    Value: TsgcWSAckServer_Options);
begin
  FAcknowledgment.Assign(Value);
end;

constructor TsgcWSAckServer_Options.Create;
begin
  inherited;
  Enabled := False;
  Interval := 300;
  Timeout := 240;
end;

procedure TsgcWSAckServer_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSAckServer_Options then
  begin
    Enabled := TsgcWSAckServer_Options(aSource).Enabled;
    Interval := TsgcWSAckServer_Options(aSource).Interval;
    Timeout := TsgcWSAckServer_Options(aSource).Timeout;
  end
  else
    inherited Assign(aSource);
end;

initialization
  TsgcWSHTTPResponse.RegisterfileName(TsgcWSProtocol_JS_Base);
  TsgcWSHTTPResponse.RegisterfileName(TsgcWSProtocol_JS_Flash);
  TsgcWSHTTPResponse.RegisterfileName(TsgcWSProtocol_JS_EventSource);
  TsgcWSHTTPResponse.RegisterfileName(TsgcWSProtocol_HTML_Base);

{$ENDIF}

end.

