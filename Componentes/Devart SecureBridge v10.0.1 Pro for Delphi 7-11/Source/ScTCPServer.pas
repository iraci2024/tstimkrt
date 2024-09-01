
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScTCPServer;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
{$IFDEF POSIX}
  Posix.SysSocket,
{$ENDIF}
  SysUtils, SyncObjs, Classes,
  ScTypes, ScFunctions, ScUtils, ScConsts,
  ScVio, ScVioTcp;

const
  DefValListenBacklog = 5;
  DefValTimeout = 60;

type
  TScAfterAcceptEvent = procedure (Sender: TObject; Vio: TCRVioTcp) of object;
  TScAcceptConnectionEvent = procedure (Sender: TObject; Connection: TScTCPConnection; var Cancel: boolean) of object;
  TScCheckIfStopListenEvent = procedure (Sender: TObject; var NeedStop: boolean) of object;

  TScTCPServerThreadEvents = record
    BeforeBind: TNotifyEvent;
    BeforeExecute: TNotifyEvent;
    AfterExecute: TNotifyEvent;
    AfterAccept: TScAfterAcceptEvent;
    OnCheckIfStopListen: TScCheckIfStopListenEvent;
    OnException: TScErrorEvent;
  end;

  TScTCPServerThread = class(TThread)
  private
    FStartWaiter: TEvent;
    FBeforeBind: TNotifyEvent;
    FBeforeExecute: TNotifyEvent;
    FAfterExecute: TNotifyEvent;
    FAfterAccept: TScAfterAcceptEvent;
    FOnException: TScErrorEvent;
    FOnCheckIfStopListen: TScCheckIfStopListenEvent;
    FMaxAcceptedConnections: integer;
    FAcceptedCount: integer;
    FListenSocket: TCRVioTcp;
    FFinished: boolean;

    procedure PrepareListen(const ListenAddress: string; const Port: integer; const IPVersion: TIPVersion;
      Timeout: integer; TCPKeepAlive: boolean; ListenBacklog: integer);
    function IsStopped: boolean;

  protected
    procedure Execute; override;
  public
    constructor Create(const ThreadEvents: TScTCPServerThreadEvents;
      const ListenAddress: string; const Port: integer; const IPVersion: TIPVersion;
      Timeout: integer; TCPKeepAlive: boolean; ListenBacklog: integer;
      MaxAcceptedConnections: integer);
    destructor Destroy; override;

    procedure StartListen;

    property Finished: boolean read FFinished;
    property ListenSocket: TCRVioTcp read FListenSocket;

    property MaxAcceptedConnections: integer read FMaxAcceptedConnections;
    property BeforeExecute: TNotifyEvent read FBeforeExecute;
    property AfterExecute: TNotifyEvent read FAfterExecute;
    property AfterAccept: TScAfterAcceptEvent read FAfterAccept;
    property OnException: TScErrorEvent read FOnException;
    property OnCheckIfStopListen: TScCheckIfStopListenEvent read FOnCheckIfStopListen;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TScTCPServer = class(TComponent)
  private
    FListenThread: TScTCPServerThread;
    FStreamedActive: boolean;

    FBindAddress: string;
    FPort: integer;
    FIPVersion: TIPVersion;
    FListenBacklog: integer;
    FTimeout: integer;
    FMaxAcceptedConnections: integer;
    FMaxOpenedConnections: integer;

    FConnectionList: TCRList;

    FBeforeBind: TNotifyEvent;
    FBeforeListenerRun: TNotifyEvent;
    FAfterListenerEnd: TNotifyEvent;
    FOnCheckIfStopListen: TScCheckIfStopListenEvent;
    FOnAcceptConnection: TScAcceptConnectionEvent;
    FOnException: TScErrorEvent;

    procedure SetBindAddress(const Value: string);
  {$HPPEMIT '#ifdef SetPort'}
  {$HPPEMIT '#undef SetPort'}
  {$HPPEMIT '#endif'}
    procedure SetPort(const Value: integer);
    procedure SetIPVersion(const Value: TIPVersion);
    procedure SetListenBacklog(const Value: integer);
    procedure SetTimeout(Value: integer);

    procedure DoBeforeBind(Sender: TObject);
    procedure DoBeforeListenerRun(Sender: TObject);
    procedure DoAfterListenerEnd(Sender: TObject);
    procedure DoOnAcceptConnection(Sender: TObject; Vio: TCRVioTcp);
    procedure DoOnCheckIfStopListen(Sender: TObject; var NeedStop: boolean);
    procedure DoOnException(Sender: TObject; E: Exception);

    procedure DoOnCloseConnection(Sender: TObject);

    function GetConnection(Index: integer): TScTCPConnection;
    function GetConnectionCount: integer;

  protected
    procedure Loaded; override;
    procedure AssignTo(Dest: TPersistent); override;

    procedure CheckInactive;
    function GetActive: boolean;
    procedure SetActive(Value: boolean);

    procedure DoStart; virtual;
    procedure DoStop; virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;

    property Connections[Index: integer]: TScTCPConnection read GetConnection;
    property ConnectionCount: integer read GetConnectionCount;

  published
    property Active: boolean read GetActive write SetActive default False;

    property BindAddress: string read FBindAddress write SetBindAddress;
    property Port: integer read FPort write SetPort default 0;
    property IPVersion: TIPVersion read FIPVersion write SetIPVersion default DefValIPVersion;
    property ListenBacklog: integer read FListenBacklog write SetListenBacklog default DefValListenBacklog;
    property Timeout: integer read FTimeout write SetTimeout default DefValTimeout;
    property MaxAcceptedConnections: integer read FMaxAcceptedConnections write FMaxAcceptedConnections default 0;
    property MaxOpenedConnections: integer read FMaxOpenedConnections write FMaxOpenedConnections default 0;

    property BeforeBind: TNotifyEvent read FBeforeBind write FBeforeBind;
    property BeforeListenerRun: TNotifyEvent read FBeforeListenerRun write FBeforeListenerRun;
    property AfterListenerEnd: TNotifyEvent read FAfterListenerEnd write FAfterListenerEnd;
    property OnAcceptConnection: TScAcceptConnectionEvent read FOnAcceptConnection write FOnAcceptConnection;
    property OnCheckIfStopListen: TScCheckIfStopListenEvent read FOnCheckIfStopListen write FOnCheckIfStopListen;
    property OnException: TScErrorEvent read FOnException write FOnException;
  end;

implementation

uses
{$IFDEF VER17P}
  Types,
{$ENDIF}
{$IFDEF MSWINDOWS}
  WinSock;
{$ENDIF}
{$IFDEF POSIX}
  Posix.SysTime, Posix.Time, Posix.UTime,
  Posix.NetinetIn, Posix.Errno;
{$ENDIF}
{$IFDEF UNIX}
  Sockets, baseunix, unix, unixutil, netdb;
{$ENDIF}

{ TScTCPServerThread }

constructor TScTCPServerThread.Create(const ThreadEvents: TScTCPServerThreadEvents;
  const ListenAddress: string; const Port: integer; const IPVersion: TIPVersion;
  Timeout: integer; TCPKeepAlive: boolean; ListenBacklog: integer;
  MaxAcceptedConnections: integer);
begin
  FStartWaiter := CreateEvent;

  FBeforeBind := ThreadEvents.BeforeBind;
  FBeforeExecute := ThreadEvents.BeforeExecute;
  FAfterExecute := ThreadEvents.AfterExecute;
  FAfterAccept := ThreadEvents.AfterAccept;
  FOnException := ThreadEvents.OnException;
  FOnCheckIfStopListen := ThreadEvents.OnCheckIfStopListen;
  FMaxAcceptedConnections := MaxAcceptedConnections;
  FAcceptedCount := 0;

  PrepareListen(ListenAddress, Port, IPVersion, Timeout, TCPKeepAlive, ListenBacklog);

  inherited Create(False); /// Input\Output error(5) for non-Windows platforms
end;

destructor TScTCPServerThread.Destroy;
begin
  inherited;

  FListenSocket.Free;
  FStartWaiter.Free;
end;

procedure TScTCPServerThread.StartListen;
begin
  FStartWaiter.SetEvent;
end;

procedure TScTCPServerThread.PrepareListen(const ListenAddress: string; const Port: integer;
  const IPVersion: TIPVersion; Timeout: integer; TCPKeepAlive: boolean; ListenBacklog: integer);
var
  opt: integer;
begin
  FListenSocket := TCRVioTcp.Create;
  FListenSocket.BindAddress := ListenAddress;
  FListenSocket.Port := Port;
  FListenSocket.IPVersion := IPVersion;
  FListenSocket.ReceiveTimeout := Timeout;

  if Assigned(FBeforeBind) then
    FBeforeBind(Self);

  FListenSocket.Bind;

  if TCPKeepAlive then
    opt := 1
  else
    opt := 0;
  FListenSocket.SetSocketOption(SOL_SOCKET, SO_KEEPALIVE, opt);

  FListenSocket.Listen(ListenBacklog); // recommended that a much lower value be used
end;

function TScTCPServerThread.IsStopped: boolean;
begin
  if (FMaxAcceptedConnections > 0) and (FAcceptedCount >= FMaxAcceptedConnections) then begin
    Result := True;
    Exit;
  end;

  Result := False;
  if Assigned(FOnCheckIfStopListen) then
    FOnCheckIfStopListen(Self, Result);
end;

procedure TScTCPServerThread.Execute;
var
  Vio: TCRVioTcp;
  NewSd: NativeInt;
  From: PSockAddr;
begin
  FStartWaiter.WaitFor(cardinal(-1));
  try
    try
      if Assigned(FBeforeExecute) then
        FBeforeExecute(Self);

      while not Terminated and not IsStopped do begin
      {$IFDEF ANDROID}
        while not FListenSocket.WaitForData(1000) and not Terminated do ;
        if Terminated then
          Continue;
      {$ELSE}
        if not FListenSocket.WaitForData or Terminated then
          Continue;
      {$ENDIF}

        try
          Inc(FAcceptedCount);
          FListenSocket.Accept(NewSd, From);
          Vio := FListenSocket.CreateNew(NewSd, From);

          if Assigned(FAfterAccept) then
            FAfterAccept(Self, Vio)
          else
            Vio.Free;
        except
          on E: Exception do
            if Assigned(FOnException) then
              FOnException(Self, E);
        end;
      end;

      FListenSocket.Close;
    finally
      if Assigned(FAfterExecute) then
        FAfterExecute(Self);
    end;
  finally
    FFinished := True;
  end;
end;

{ TScTCPServer }

constructor TScTCPServer.Create(AOwner: TComponent);
begin
  inherited;

  FConnectionList := TCRList.Create;

  FIPVersion := DefValIPVersion;
  FListenBacklog := DefValListenBacklog;
  FTimeout := DefValTimeout;
end;

destructor TScTCPServer.Destroy;
begin
  DoStop;

  FListenThread.Free;
  FConnectionList.Free;

  inherited;
end;

procedure TScTCPServer.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScTCPServer) then begin
    TScTCPServer(Dest).FBindAddress := FBindAddress;
    TScTCPServer(Dest).FPort := FPort;
    TScTCPServer(Dest).FIPVersion := FIPVersion;
    TScTCPServer(Dest).FListenBacklog := FListenBacklog;
    TScTCPServer(Dest).FTimeout := FTimeout;
    TScTCPServer(Dest).FMaxAcceptedConnections := FMaxAcceptedConnections;
    TScTCPServer(Dest).FMaxOpenedConnections := FMaxOpenedConnections;
  end
  else
    inherited;
end;

procedure TScTCPServer.Loaded;
begin
  inherited;

  try
    try
      if FStreamedActive then
        SetActive(True);
    except
      on E: Exception do
        if csDesigning in ComponentState then
          ShowException(E, ExceptAddr)
        else
          raise;
    end;
  finally
    FStreamedActive := False;
  end;
end;

function TScTCPServer.GetActive: boolean;
begin
  Result := (FListenThread <> nil) and not FListenThread.Finished;
end;

procedure TScTCPServer.SetActive(Value: boolean);
begin
  if (csReading in ComponentState) and Value then
    FStreamedActive := True
  else begin
    if Value = GetActive then
      Exit;

    if Value then
      DoStart
    else
      DoStop;
  end;
end;

procedure TScTCPServer.Start;
begin
  SetActive(True);
end;

procedure TScTCPServer.Stop;
begin
  SetActive(False);
end;

procedure TScTCPServer.DoStart;
var
  ThreadEvents: TScTCPServerThreadEvents;
begin
  FreeAndNil(FListenThread);

  ThreadEvents.BeforeBind := DoBeforeBind;
  ThreadEvents.BeforeExecute := DoBeforeListenerRun;
  ThreadEvents.AfterExecute := DoAfterListenerEnd;
  ThreadEvents.AfterAccept := DoOnAcceptConnection;
  ThreadEvents.OnCheckIfStopListen := DoOnCheckIfStopListen;
  ThreadEvents.OnException := DoOnException;

  FListenThread := TScTCPServerThread.Create(ThreadEvents, FBindAddress, FPort,
    FIPVersion, FTimeout, True, FListenBacklog, FMaxAcceptedConnections);
  FListenThread.StartListen;
end;

procedure TScTCPServer.DoStop;
begin
  if FListenThread <> nil then begin
    FListenThread.Terminate;
    FListenThread.ListenSocket.Close;
    FreeAndNil(FListenThread);
  end;
end;

procedure TScTCPServer.DoOnAcceptConnection(Sender: TObject; Vio: TCRVioTcp);
var
  TCPConnection: TScTCPConnection;
  Cancel: boolean;
begin
  if (FMaxOpenedConnections > 0) and (GetConnectionCount >= FMaxOpenedConnections) then
    Cancel := True
  else
    Cancel := False;

  if not Assigned(OnAcceptConnection) and Cancel then begin
    Vio.Free;
    Exit;
  end;

  TCPConnection := TScTCPConnection.Create(Vio, True);
  try
    if Assigned(OnAcceptConnection) then
      OnAcceptConnection(Self, TCPConnection, Cancel);
  finally
    if Cancel then
      TCPConnection.Free
    else begin
      TCPConnection.OnInternalClose := DoOnCloseConnection;
      FConnectionList.Add(TCPConnection);
    end;
  end;
end;

procedure TScTCPServer.DoOnCloseConnection(Sender: TObject);
begin
  FConnectionList.Remove(Sender);
end;

function TScTCPServer.GetConnection(Index: Integer): TScTCPConnection;
begin
  Result := TObject(FConnectionList.Items[Index]) as TScTCPConnection;
end;

function TScTCPServer.GetConnectionCount: Integer;
begin
  Result := FConnectionList.Count;
end;

procedure TScTCPServer.DoOnCheckIfStopListen(Sender: TObject; var NeedStop: boolean);
begin
  if Assigned(OnCheckIfStopListen) then
    OnCheckIfStopListen(Self, NeedStop);
end;

procedure TScTCPServer.DoOnException(Sender: TObject; E: Exception);
begin
  if Assigned(OnException) then
    OnException(Self, E);
end;

procedure TScTCPServer.DoBeforeBind(Sender: TObject);
begin
  if Assigned(BeforeBind) then
    BeforeBind(Self);
end;

procedure TScTCPServer.DoBeforeListenerRun(Sender: TObject);
begin
  if Assigned(BeforeListenerRun) then
    BeforeListenerRun(Self);
end;

procedure TScTCPServer.DoAfterListenerEnd(Sender: TObject);
begin
  if Assigned(AfterListenerEnd) then
    AfterListenerEnd(Self);
end;

procedure TScTCPServer.CheckInactive;
begin
  if Active then
    raise EScError.Create(seServerOpened);
end;

procedure TScTCPServer.SetBindAddress(const Value: string);
begin
  if Value <> FBindAddress then begin
    CheckInactive;
    FBindAddress := Trim(Value);
  end;
end;

procedure TScTCPServer.SetPort(const Value: Integer);
begin
  if Value <> FPort then begin
    CheckInactive;
    FPort := Value;
  end;
end;

procedure TScTCPServer.SetIPVersion(const Value: TIPVersion);
begin
  if Value <> FIPVersion then begin
    CheckInactive;
    FIPVersion := Value;
  end;
end;

procedure TScTCPServer.SetListenBacklog(const Value: integer);
begin
  if Value <> FListenBacklog then begin
    CheckInactive;
    FListenBacklog := Value;
  end;
end;

procedure TScTCPServer.SetTimeout(Value: integer);
begin
  if Value <> FTimeout then begin
    CheckInactive;
    FTimeout := Value;
  end;
end;

end.
