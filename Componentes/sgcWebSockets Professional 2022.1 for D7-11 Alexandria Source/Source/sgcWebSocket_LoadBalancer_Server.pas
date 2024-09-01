{***************************************************************************
 sgcWebSocket component

 written by eSeGeCe
            copyright © 2022
            Email : info@esegece.com
            Web : http://www.esegece.com
***************************************************************************}

unit sgcWebSocket_LoadBalancer_Server;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils, StrUtils,
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  {$IFDEF NEXTGEN}System.Generics.Collections, {$ELSE}Contnrs, {$ENDIF}
  // indy
  {$IFDEF SGC_INDY}sgcIdContext{$ELSE}IdContext{$ENDIF},
  // websocket
  sgcWebSocket_Server, sgcWebSocket_Classes, sgcWebSocket_LoadBalancer_Message,
  sgcWebSocket_Types, sgcWebSocket_Classes_SyncObjs;

type
  TsgcWSLBServerConnection = class;

  TsgcWSLBServerConnectEvent = procedure(Connection: TsgcWSConnection) of object;
  TsgcWSLBServerDisconnectEvent = procedure(Connection: TsgcWSConnection; Code: Integer) of object;
  TsgcWSLBClientConnectionEvent = procedure(ServerConnection: TsgcWSConnection;
      ClientConnection: TsgcWSLoadBalancerClientConnection) of object;
  TsgcWSLBServerMessageEvent = procedure(Connection: TsgcWSConnection; Text:
      String; var Handled: Boolean) of object;
  TsgcWSLBServerBinaryEvent = procedure(Connection: TsgcWSConnection; Data:
      TMemoryStream; var Handled: Boolean) of object;
  TsgcWSLBBeforeSendServerHostEvent = procedure(Connection: TsgcWSConnection; var
      Binding: TsgcWSLoadBalancerServerBinding) of object;
  TsgcWSLBServerReadyEvent = procedure(Server: TsgcWSLBServerConnection) of
      object;

  TsgcThreadListBindings = Class(TsgcThreadList{$IFDEF NEXTGEN}<TObject>{$ENDIF})
  End;

  TsgcThreadListClientList = Class(TsgcThreadList{$IFDEF NEXTGEN}<TObject>{$ENDIF})
  End;

  TsgcThreadListServerList = Class(TsgcThreadList{$IFDEF NEXTGEN}<TObject>{$ENDIF})
  End;

  TsgcWSLoadBalancer_Options = class(TPersistent)
  private
    FLoadBalancing: TwsLoadBalancing;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property LoadBalancing: TwsLoadBalancing read FLoadBalancing write
        FLoadBalancing;
  end;

  TsgcWSLBServerConnection = class(TComponent)
  private
    FBindings: TsgcThreadListBindings;
    FClientList: TsgcThreadListClientList;
    FReady: Boolean;
    FConnection: TsgcWSConnection;
    FData: TsgcWSLoadBalancerServerData;
    function GetBindings: TsgcThreadListBindings;
    function GetClientList: TsgcThreadListClientList;
    function GetData: TsgcWSLoadBalancerServerData;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  public
    procedure AddClient(const aConnection: TsgcWSLoadBalancerClientConnection);
    procedure DeleteClient(const aConnection: TsgcWSLoadBalancerClientConnection);
  public
    procedure AddBinding(const aBinding: TsgcWSLoadBalancerServerBinding);
  public
    property Bindings: TsgcThreadListBindings read GetBindings write FBindings;
    property ClientList: TsgcThreadListClientList read GetClientList write FClientList;
    property Ready: Boolean read FReady write FReady;
    property Connection: TsgcWSConnection read FConnection write FConnection;
    property Data: TsgcWSLoadBalancerServerData read GetData write FData;
  end;

  TsgcWSLoadBalancerServer = class(TsgcWSServer)
  { connections }
  private
    FServerList: TsgcThreadListServerList;
  protected
    function GetServerList: TsgcThreadListServerList;
  protected
    procedure DoAddConnectionToServersList(aConnection: TsgcWSConnection); virtual;
    procedure DoDeleteConnectionFromServersList(aConnection: TsgcWSConnection); virtual;
    function GetServerByConnection(aConnection: TsgcWSConnection):
        TsgcWSLBServerConnection;
  protected
    procedure DoProcessServerData(const aConnection: TsgcWSConnection; const aText:
        string); virtual;
    procedure DoProcessClientConnection(const aConnection: TsgcWSConnection; const
        aText: string); virtual;
    procedure DoProcessServerBinding(const aConnection: TsgcWSConnection; const
        aText: string); virtual;
    procedure DoProcessServerReady(const aConnection: TsgcWSConnection); virtual;
  public
    property ServerList: TsgcThreadListServerList read GetServerList write FServerList;
  { connections }

  { from TsgcWSComponent_Base }
  protected
    procedure DoEventConnect(aConnection: TsgcWSConnection); override;
    procedure DoEventDisconnect(aConnection: TsgcWSConnection; Code: Integer);
        override;
    procedure DoEventMessage(aConnection: TsgcWSConnection; const Text: string);
        override;
    procedure DoEventBinary(const aConnection: TsgcWSConnection; Data:
        TMemoryStream); override;
    procedure DoEventFragmented(const aConnection: TsgcWSConnection; Data:
        TMemoryStream; OpCode: TOpCode; Fin: Boolean); override;
  { from TsgcWSComponent_Base }

  { LoadBalancer }
  private
    FLoadBalancer: TsgcWSLoadBalancer_Options;
  protected
    procedure SetLoadBalancer(const Value: TsgcWSLoadBalancer_Options);
  public
    property LoadBalancer: TsgcWSLoadBalancer_Options read FLoadBalancer write
        SetLoadBalancer;
  { LoadBalancer }

  { responses }
  private
    function GetServerLessConnections(const aList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF}): Integer;
  private
    function GetServerBindingText: String;
  protected
    function DoSendServerBinding(const aConnection: TsgcWSConnectionServer):
        Boolean; virtual;
  { responses }

  { from TsgcWSServer_Base }
  protected
    function DoBuiltInLibraries(aConnection: TsgcWSConnectionServer; const aText:
        String; aDisconnect: Boolean; aStreamIdentifier: Integer = 0): Boolean;
        override;
  { from TsgcWSServer_Base }

  { message text }
  private
    FMessageReadText: TsgcWSLoadBalancerMessage;
    FMessageWriteText: TsgcWSLoadBalancerMessage;
  protected
    function GetMessageReadText: TsgcWSLoadBalancerMessage;
    function GetMessageWriteText: TsgcWSLoadBalancerMessage;
  public
    property MessageReadText: TsgcWSLoadBalancerMessage read GetMessageReadText
        write FMessageReadText;
    property MessageWriteText: TsgcWSLoadBalancerMessage read GetMessageWriteText
        write FMessageWriteText;
  { message text }

  { message binary }
  private
    FMessageReadBinary: TsgcWSLoadBalancerBinary;
    FMessageWriteBinary: TsgcWSLoadBalancerBinary;
  protected
    function GetMessageReadBinary: TsgcWSLoadBalancerBinary;
    function GetMessageWriteBinary: TsgcWSLoadBalancerBinary;
  public
    property MessageReadBinary: TsgcWSLoadBalancerBinary read GetMessageReadBinary
        write FMessageReadBinary;
    property MessageWriteBinary: TsgcWSLoadBalancerBinary read
        GetMessageWriteBinary write FMessageWriteBinary;
  { message binary }

  { constructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
  { constructor }

  { events }
  private
    FOnRawMessage: TsgcWSRawMessageEvent;
  private
    FOnClientBinary: TsgcWSLBServerBinaryEvent;
    FOnClientConnect: TsgcWSLBClientConnectionEvent;
    FOnClientDisconnect: TsgcWSLBClientConnectionEvent;
    FOnClientFragmented: TsgcWSLBServerBinaryEvent;
    FOnClientMessage: TsgcWSLBServerMessageEvent;
  private
    FOnServerConnect: TsgcWSLBServerConnectEvent;
    FOnServerDisconnect: TsgcWSLBServerDisconnectEvent;
    FOnServerReady: TsgcWSLBServerReadyEvent;
  private
    FOnBeforeSendServerBinding: TsgcWSLBBeforeSendServerHostEvent;
  public
    property OnRawMessage: TsgcWSRawMessageEvent read FOnRawMessage write
        FOnRawMessage;
  public
    property OnClientConnect: TsgcWSLBClientConnectionEvent read FOnClientConnect
        write FOnClientConnect;
    property OnClientDisconnect: TsgcWSLBClientConnectionEvent read
        FOnClientDisconnect write FOnClientDisconnect;
    property OnClientBinary: TsgcWSLBServerBinaryEvent read FOnClientBinary write
        FOnClientBinary;
    property OnClientFragmented: TsgcWSLBServerBinaryEvent read FOnClientFragmented
        write FOnClientFragmented;
    property OnClientMessage: TsgcWSLBServerMessageEvent read FOnClientMessage
        write FOnClientMessage;
  public
    property OnServerConnect: TsgcWSLBServerConnectEvent read FOnServerConnect
        write FOnServerConnect;
    property OnServerDisconnect: TsgcWSLBServerDisconnectEvent read
        FOnServerDisconnect write FOnServerDisconnect;
    property OnServerReady: TsgcWSLBServerReadyEvent read FOnServerReady write
        FOnServerReady;
  public
    property OnBeforeSendServerBinding: TsgcWSLBBeforeSendServerHostEvent read
        FOnBeforeSendServerBinding write FOnBeforeSendServerBinding;
  { events }
  end;

implementation

uses
  Math,
  sgcWebSocket_Const, sgcWebSocket_Helpers, sgcBase_Helpers;

constructor TsgcWSLoadBalancerServer.Create(aOwner: TComponent);
begin
  inherited;
  NotifyEvents := neNoSync;
  FLoadBalancer := TsgcWSLoadBalancer_Options.Create;
end;

destructor TsgcWSLoadBalancerServer.Destroy;
begin
  sgcFree(FLoadBalancer);
  sgcFree(FServerList);
  inherited;
end;

procedure TsgcWSLoadBalancerServer.DoAddConnectionToServersList(aConnection:
    TsgcWSConnection);
var
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
  oServer: TsgcWSLBServerConnection;
begin
  oList := ServerList.LockList;
  Try
    oServer := TsgcWSLBServerConnection.Create(nil);
    oServer.Connection := aConnection;
    oList.Add(oServer);
  Finally
    ServerList.UnlockList;
  End;
end;

procedure TsgcWSLoadBalancerServer.DoDeleteConnectionFromServersList(
    aConnection: TsgcWSConnection);
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
  oServerConnection: TsgcWSLBServerConnection;
begin
  oList := ServerList.LockList;
  Try
    for i := oList.Count - 1 Downto 0 do
    begin
      if TsgcWSLBServerConnection(oList[i]).Connection = aConnection then
      begin
        oServerConnection := TsgcWSLBServerConnection(oList[i]);
        sgcFree(oServerConnection);
        oList.Delete(i);
        break;
      end;
    end;
  Finally
    ServerList.UnlockList;
  End;
end;

procedure TsgcWSLoadBalancerServer.DoEventBinary(const aConnection:
    TsgcWSConnection; Data: TMemoryStream);
var
  vHandled: Boolean;
begin
  inherited;
  if aConnection.URL = CS_LB_CLIENT then
  begin
    vHandled := False;
    if Assigned(FOnClientBinary) then
      FOnClientBinary(aConnection, Data, vHandled);
    if not vHandled then    
      BroadCast(Data);
  end
  else
    inherited;
end;

procedure TsgcWSLoadBalancerServer.DoEventConnect(aConnection:
    TsgcWSConnection);
begin
  if aConnection.URL = CS_LB_CLIENT then
  begin
    // ... delete / add to list
    DoDeleteConnectionFromServersList(aConnection);
    DoAddConnectionToServersList(aConnection);
    // ... event
    if Assigned(FOnServerConnect) then
      FOnServerConnect(aConnection);
  end
  else
    inherited;
end;

procedure TsgcWSLoadBalancerServer.DoEventDisconnect(aConnection:
    TsgcWSConnection; Code: Integer);
begin
  if aConnection.URL = CS_LB_CLIENT then
  begin
    // ... delete from list
    DoDeleteConnectionFromServersList(aConnection);
    // ... event
    if Assigned(FOnServerDisconnect) then
      FOnServerDisconnect(aConnection, Code);
  end
  else
    inherited;
end;

procedure TsgcWSLoadBalancerServer.DoEventFragmented(const aConnection:
    TsgcWSConnection; Data: TMemoryStream; OpCode: TOpCode; Fin: Boolean);
var
  vHandled: Boolean;
begin
  if aConnection.URL = CS_LB_CLIENT then
  begin
    vHandled := False;
    if Assigned(FOnClientFragmented) then
      FOnClientFragmented(aConnection, Data, vHandled);
    if not vHandled then
      BroadCast(Data);
  end
  else
    inherited;
end;


procedure TsgcWSLoadBalancerServer.DoEventMessage(aConnection:
    TsgcWSConnection; const Text: string);
var
  vHandled: Boolean;
begin
  vHandled := False;
  if Assigned(FOnRawMessage) then
  begin
    FOnRawMessage(aConnection, Text, vHandled);
    if vHandled then
      exit;
  end;

  if aConnection.URL = CS_LB_CLIENT then
  begin
    if sgcContainsText(Text, CS_LB_SERVER_DATA) then
      DoProcessServerData(aConnection, Text)
    else if sgcContainsText(Text, CS_LB_CLIENT_CONNECTION) then
      DoProcessClientConnection(aConnection, Text)
    else if sgcContainsText(Text, CS_LB_SERVER_BINDING) then
      DoProcessServerBinding(aConnection, Text)
    else if sgcContainsText(Text, CS_LB_SERVER_READY) then
      DoProcessServerReady(aConnection)
    else
    begin
      vHandled := False;
      if Assigned(FOnClientMessage) then
        FOnClientMessage(aConnection, Text, vHandled);
      if not vHandled then      
        Broadcast(Text);
    end;
  end
  else
    inherited;
end;

procedure TsgcWSLoadBalancerServer.DoProcessClientConnection(const aConnection:
    TsgcWSConnection; const aText: string);
var
  oConnection: TsgcWSLoadBalancerClientConnection;
  oServer: TsgcWSLBServerConnection;
begin
  oServer := GetServerByConnection(aConnection);
  if Assigned(oServer) then
  begin
    oConnection := TsgcWSLoadBalancerClientConnection.Create(nil);
    Try
      oConnection.Read(aText);
      if oConnection.Active then
      begin
        // ... add client
        oServer.AddClient(oConnection);
        // ... event connect
        if Assigned(FOnClientConnect) then
          FOnClientConnect(aConnection, oConnection);
      end
      else
      begin
        // ... delete cliente
        oServer.DeleteClient(oConnection);
        // ... event disconnect
        if Assigned(FOnClientDisconnect) then
          FOnClientDisconnect(aConnection, oConnection);
      end;
    Finally
      sgcFree(oConnection);
    End;
  end;
end;

procedure TsgcWSLoadBalancerServer.DoProcessServerBinding(const aConnection:
    TsgcWSConnection; const aText: string);
var
  oBinding: TsgcWSLoadBalancerServerBinding;
  oServer: TsgcWSLBServerConnection;
begin
  oServer := GetServerByConnection(aConnection);
  if Assigned(oServer) then
  begin
    oBinding := TsgcWSLoadBalancerServerBinding.Create(nil);
    Try
      oBinding.Read(aText);
      oServer.AddBinding(oBinding)
    Finally
      sgcFree(oBinding);
    End;
  end;
end;

function TsgcWSLoadBalancerServer.GetMessageReadBinary:
    TsgcWSLoadBalancerBinary;
begin
  if not Assigned(FMessageReadBinary) then
    FMessageReadBinary := TsgcWSLoadBalancerBinary.Create(self);
  Result := FMessageReadBinary;
end;

function TsgcWSLoadBalancerServer.GetMessageReadText: TsgcWSLoadBalancerMessage;
begin
  if not Assigned(FMessageReadText) then
    FMessageReadText := TsgcWSLoadBalancerMessage.Create(self);
  Result := FMessageReadText;
end;

function TsgcWSLoadBalancerServer.GetMessageWriteBinary:
    TsgcWSLoadBalancerBinary;
begin
  if not Assigned(FMessageWriteBinary) then
    FMessageWriteBinary := TsgcWSLoadBalancerBinary.Create(self);
  Result := FMessageWriteBinary;
end;

function TsgcWSLoadBalancerServer.GetMessageWriteText:
    TsgcWSLoadBalancerMessage;
begin
  if not Assigned(FMessageWriteText) then
    FMessageWriteText := TsgcWSLoadBalancerMessage.Create(self);
  Result := FMessageWriteText;
end;

function TsgcWSLoadBalancerServer.GetServerByConnection(aConnection:
    TsgcWSConnection): TsgcWSLBServerConnection;
var
  i: Integer;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  result := nil;

  oList := ServerList.LockList;
  Try
    for i := oList.Count - 1 Downto 0 do
    begin
      if TsgcWSLBServerConnection(oList[i]).Connection = aConnection then
      begin
        result := TsgcWSLBServerConnection(oList[i]);
        break;
      end;
    end;
  Finally
    ServerList.UnlockList;
  End;
end;

function TsgcWSLoadBalancerServer.GetServerList: TsgcThreadListServerList;
begin
  if not Assigned(FServerList) then
    FServerList := TsgcThreadListServerList.Create;
  Result := FServerList;
end;

function TsgcWSLoadBalancerServer.GetServerBindingText: String;
var
  i: integer;
  j: integer;
  n: integer;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
  oList2: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
  vLoaded: Boolean;
begin
  vLoaded := False;
  n := 1;

  oList := ServerList.LockList;
  Try
    if oList.Count > 0 then
    begin
      repeat
        // ... get server
        case LoadBalancer.LoadBalancing of
          lbRandom: i := RandomRange(0, oList.Count);
          lbConnections: i := GetServerLessConnections(oList);
          else
            i := 0;
        end;
        // ... get binding by server random
        if TsgcWSLBServerConnection(oList[i]).Ready then
        begin
          oList2 := TsgcWSLBServerConnection(oList[i]).Bindings.LockList;
          Try
            if oList2.Count > 0 then
            begin
              j := RandomRange(0, oList2.Count);
              // ... write binding
              result := TsgcWSLoadBalancerServerBinding(oList2[j]).Write;
              vLoaded := True;
            end;
          Finally
            TsgcWSLBServerConnection(oList[i]).Bindings.UnlockList;
          End;
        end;
        inc(n);
      until
        vLoaded or (n > 10);
    end;
  Finally
    ServerList.UnlockList;
  End;
end;

function TsgcWSLoadBalancerServer.DoBuiltInLibraries(aConnection:
    TsgcWSConnectionServer; const aText: String; aDisconnect: Boolean;
    aStreamIdentifier: Integer = 0): Boolean;
begin
  result := inherited DoBuiltInLibraries(aConnection, aText, aDisconnect, aStreamIdentifier);

  if not result then
  begin
    if sgcMatchesMask(aText, 'GET /' + CS_LB_CLIENT_GET_BINDING + ' *') then
      result := DoSendServerBinding(aConnection);
  end;
end;

procedure TsgcWSLoadBalancerServer.DoProcessServerData(const aConnection:
    TsgcWSConnection; const aText: string);
var
  oData: TsgcWSLoadBalancerServerData;
  oServer: TsgcWSLBServerConnection;
begin
  oServer := GetServerByConnection(aConnection);
  if Assigned(oServer) then
  begin
    oData := TsgcWSLoadBalancerServerData.Create(nil);
    Try
      oData.Read(aText);
      oServer.Data.Assign(oData);
    Finally
      sgcFree(oData);
    End;
  end;
end;

procedure TsgcWSLoadBalancerServer.DoProcessServerReady(const aConnection:
    TsgcWSConnection);
var
  oServer: TsgcWSLBServerConnection;
begin
  oServer := GetServerByConnection(aConnection);
  if Assigned(oServer) then
  begin
    oServer.Ready := True;
    if Assigned(FOnServerReady) then
      FOnServerReady(oServer);
  end;
end;

function TsgcWSLoadBalancerServer.DoSendServerBinding(const aConnection:
    TsgcWSConnectionServer): Boolean;
var
  vText: String;
  oBinding: TsgcWSLoadBalancerServerBinding;
begin
  result := False;
  vText := GetServerBindingText;
  if vText <> '' then
  begin
    // ... event
    if Assigned(FOnBeforeSendServerBinding) then
    begin
      oBinding := TsgcWSLoadBalancerServerBinding.Create(nil);
      Try
        if vText <> '' then
          oBinding.Read(vText);
        FOnBeforeSendServerBinding(aConnection, oBinding);
        vText := oBinding.Write;
      Finally
        sgcFree(oBinding);
      End;
    end;
    // ... send response
    aConnection.SendResponseHTTP(vText, 'text/html');
    result := True;
  end;
end;

function TsgcWSLoadBalancerServer.GetServerLessConnections(const aList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF}):
    Integer;
var
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
  i: Integer;
  n: Integer;
begin
  result := 0;
  n := MaxInt;
  for i := 0 to aList.Count - 1 do
  begin
    oList := TsgcWSLBServerConnection(aList[i]).ClientList.LockList;
    Try
      if oList.Count < n then
      begin
        n := oList.Count;
        result := i;
      end;
    Finally
      TsgcWSLBServerConnection(aList[i]).ClientList.UnlockList;
    End;
  end;
end;

procedure TsgcWSLoadBalancerServer.SetLoadBalancer(const Value:
    TsgcWSLoadBalancer_Options);
begin
  FLoadBalancer.Assign(Value);
end;

constructor TsgcWSLBServerConnection.Create(aOwner: TComponent);
begin
  inherited;
  FReady := False;
end;

destructor TsgcWSLBServerConnection.Destroy;
begin
  FConnection := nil;
  sgcFree(FBindings);
  sgcFree(FClientList);
  inherited;
end;

procedure TsgcWSLBServerConnection.AddClient(const aConnection:
    TsgcWSLoadBalancerClientConnection);
var
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
  oConnection: TsgcWSLoadBalancerClientConnection;
begin
  oList := ClientList.LockList;
  Try
    oConnection := TsgcWSLoadBalancerClientConnection.Create(nil);
    oConnection.Assign(aConnection);
    oList.Add(oConnection)
  Finally
    ClientList.UnlockList;
  End;
end;

procedure TsgcWSLBServerConnection.AddBinding(const aBinding:
    TsgcWSLoadBalancerServerBinding);
var
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
  oBinding: TsgcWSLoadBalancerServerBinding;
begin
  oList := Bindings.LockList;
  Try
    oBinding := TsgcWSLoadBalancerServerBinding.Create(nil);
    oBinding.Assign(aBinding);
    oList.Add(oBinding)
  Finally
    Bindings.UnlockList;
  End;
end;

procedure TsgcWSLBServerConnection.DeleteClient(const aConnection:
    TsgcWSLoadBalancerClientConnection);
var
  i: Integer;
  oLBClientConnection: TsgcWSLoadBalancerClientConnection;
  oList: TList{$IFDEF NEXTGEN}<TObject>{$ENDIF};
begin
  oList := ClientList.LockList;
  Try
    for i := oList.Count - 1 Downto 0 do
    begin
      if TsgcWSLoadBalancerClientConnection(oList[i]).Guid = aConnection.Guid then
      begin
        oLBClientConnection := TsgcWSLoadBalancerClientConnection(oList[i]);
        sgcFree(oLBClientConnection);
        oList.Delete(i);
        break;
      end;
    end;
  Finally
    ClientList.UnlockList;
  End;
end;

function TsgcWSLBServerConnection.GetBindings: TsgcThreadListBindings;
begin
  if not Assigned(FBindings) then
    FBindings := TsgcThreadListBindings.Create;
  Result := FBindings;
end;

function TsgcWSLBServerConnection.GetClientList: TsgcThreadListClientList;
begin
  if not Assigned(FClientList) then
    FClientList := TsgcThreadListClientList.Create;
  Result := FClientList;
end;

function TsgcWSLBServerConnection.GetData: TsgcWSLoadBalancerServerData;
begin
  if not Assigned(FData) then
    FData := TsgcWSLoadBalancerServerData.Create(self);
  Result := FData;
end;

constructor TsgcWSLoadBalancer_Options.Create;
begin
  inherited;
  LoadBalancing := lbRandom;
end;

procedure TsgcWSLoadBalancer_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcWSLoadBalancer_Options then
  begin
    FLoadBalancing := TsgcWSLoadBalancer_Options(aSource).LoadBalancing;
  end
  else
    inherited
end;


end.
