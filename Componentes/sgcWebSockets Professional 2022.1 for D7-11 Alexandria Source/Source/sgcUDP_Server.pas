{ ***************************************************************************
  sgcUDP component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcUDP_Server;

interface

{$I sgcVer.inc}

{$IFDEF SGC_UDP}

uses
  Classes, SysUtils,
{$IFDEF NEXTGEN}System.Generics.Collections, {$ELSE}Contnrs, {$ENDIF}
  // indy
{$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
{$IFDEF SGC_INDY}sgcIdUDPServer{$ELSE}IdUDPServer{$ENDIF},
{$IFDEF SGC_INDY}sgcIdUDPBase{$ELSE}IdUDPBase{$ENDIF},
{$IFDEF SGC_INDY}sgcIdSocketHandle{$ELSE}IdSocketHandle{$ENDIF},
  // sgc
  sgcUDP_Classes, sgcBase_Classes, sgcSocket_Classes_Indy,
  sgcWebSocket_Types, sgcUDP_Classes_Indy, sgcBase_Helpers;

type
  TsgcUDPServer_Base = class;

  TsgcUDPOnBeforeWatchDogEvent = procedure(Sender: TObject;
    var Handled: Boolean) of object;

  TsgcIdUDPServer = class(TIdUDPServer)
  protected
    function GetSocketBinding(const aIPAddress: string; aPort: Integer):
        TIdSocketHandle; virtual;
  public
    function AddBinding(const aIPAddress: string; aPort: Integer;
        const aName: string = ''): TIdSocketHandle;
    function RemoveBinding(const aIPAddress: string; aPort: Integer): Boolean;
  protected
    function GetActive: Boolean; override;
  end;

  TsgcUDPWatchDogMonitorServer_Options = class(TPersistent)
  private
    FEnabled: Boolean;
    FTimeOut: Integer;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Enabled: Boolean read FEnabled write FEnabled;
    property TimeOut: Integer read FTimeOut write FTimeOut;
  end;

  TsgcUDPWatchDogServer_Options = class(TsgcUDPWatchDog_Options)
  private
    FMonitor: TsgcUDPWatchDogMonitorServer_Options;
    procedure SetMonitor(const Value: TsgcUDPWatchDogMonitorServer_Options);
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property Monitor: TsgcUDPWatchDogMonitorServer_Options read FMonitor
      write SetMonitor;
  end;

  TsgcUDPSocketServer = class(TsgcUDPSocket_Indy)
    { from TsgcUDPSocket }
  public
    procedure Assign(const aSource: TsgcUDPSocket); override;
    { from TsgcUDPSocket }

    { UDPServer }
  private
    FServer: TsgcUDPServer_Base;
  protected
    procedure DoInitialize(const aServer: TsgcUDPServer_Base; const aPeerIp:
        string; aPeerPort: Integer; aLocalIp: string; aLocalPort: Integer;
        aIPVersion: TwsIPVersion); virtual;
    { UDPServer }

  protected
    procedure DoWriteData(const aValue: string); override;
    procedure DoWriteData(const aBytes: TBytes); override;
  end;

  TsgcUDPServer_Base = class(TsgcUDPComponent)
    { from TComponent }
  protected
    procedure Loaded; override;
    { from TComponent }

    { UDP server }
  private
    FUDPServer: TsgcIdUDPServer;
    function GetUDPServer: TsgcIdUDPServer;
  protected
    property UDPServer: TsgcIdUDPServer read GetUDPServer write FUDPServer;
  public
    function AddBinding(const aIPAddress: string; aPort: Integer;
        const aName: string = ''): TIdSocketHandle;
    function RemoveBinding(const aIPAddress: string; aPort: Integer): Boolean;
    function GetBinding(const aIPAddress: string; aPort: Integer): TIdSocketHandle;
    { UDP server }

    { heartbeat }
  private
    FHeartBeatTimer: TsgcTimer;
    FHeartBeat: TsgcUDPHeartBeat_Options;
    procedure SetHeartBeat(const Value: TsgcUDPHeartBeat_Options);
  protected
    procedure OnHeartBeatEvent(Sender: TObject); virtual;
    procedure OnHeartBeatExceptionEvent(Sender: TObject; E: Exception); virtual;
    procedure DoStartHeartBeat; virtual;
    procedure DoStopHeartBeat; virtual;
  protected
    property HeartBeat: TsgcUDPHeartBeat_Options read FHeartBeat
      write SetHeartBeat;
    { heartbeat }

    { events }
  protected
    function DoCreateUDPSocket(const ABinding: TIdSocketHandle)
      : TsgcUDPSocketServer; virtual;
  protected
{$IFNDEF INDY10_2}
    procedure OnClientReadEvent(Sender: TObject; AData: TIdBytes;
      ABinding: TIdSocketHandle); virtual;
    procedure OnClientExceptionEvent(Sender: TObject; ABinding: TIdSocketHandle;
      const AMessage: String; const AExceptionClass: TClass); virtual;
{$ELSE}
    procedure OnClientReadEvent(AThread: TIdUDPListenerThread;
{$IFDEF INDY10_6}const {$ENDIF} AData:
{$IFDEF INDY10_5_9}{$IFNDEF INDY10_6} Array of Byte{$ELSE}TIdBytes{$ENDIF}{$ELSE}TIdBytes{$ENDIF}; ABinding: TIdSocketHandle); virtual;
    procedure OnClientExceptionEvent(AThread: TIdUDPListenerThread;
      ABinding: TIdSocketHandle; const AMessage: String;
      const AExceptionClass: TClass); virtual;
{$ENDIF}
  protected
    procedure OnSocketExceptionEvent(Sender: TObject;
      const E: Exception); virtual;
    { events }

    { LogFile }
  private
    FLogFile: TsgcUDPLogFile;
    FIdLogFile: TsgcIdUDPLogFile;
  protected
    function GetIdLogFile: TsgcIdUDPLogFile;
    procedure SetLogFile(const Value: TsgcUDPLogFile);
  public
    property LogFile: TsgcUDPLogFile read FLogFile write SetLogFile;
    { LogFile }

    { WatchDog }
  private
    FWatchDog: TsgcUDPWatchDogServer_Options;
    FWatchDogMonitorSecret: string;
    FWatchDogMonitorStart: Cardinal;
    FWatchDogMonitorWaiting: Boolean;
  private
    FOnBeforeWatchDog: TsgcUDPOnBeforeWatchDogEvent;
  protected
    FWatchDogAttempts: Integer;
    FWatchDogTimer: TsgcTimer;
    FWatchDogEnabled: Boolean;
  protected
    procedure DoStartWatchDog; virtual;
    procedure DoStopWatchDog; virtual;
  protected
    procedure DoWatchDogMonitor; virtual;
    function DoWatchDogMonitorRead(const Bytes: TBytes): Boolean; virtual;
  protected
    procedure OnWatchDogEvent(Sender: TObject); virtual;
    procedure OnWatchDogExceptionEvent(Sender: TObject; E: Exception); virtual;
  protected
    procedure SetWatchDog(const Value: TsgcUDPWatchDogServer_Options); virtual;
  public
    property WatchDog: TsgcUDPWatchDogServer_Options read FWatchDog
      write SetWatchDog;
  public
    property OnBeforeWatchDog: TsgcUDPOnBeforeWatchDogEvent
      read FOnBeforeWatchDog write FOnBeforeWatchDog;
    { WatchDog }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { properties }
  private
    FHost: String;
    FPort: Integer;
    FTLS: Boolean;
    FTLSOptions: TsgcUDPTLS_Options;
    function GetBindings: TIdSocketHandles;
    procedure SetBindings(const Value: TIdSocketHandles);
  protected
    procedure SetTLSOptions(const Value: TsgcUDPTLS_Options); virtual;
  public
    property Host: String read FHost write FHost;
    property Port: Integer read FPort write FPort;
    property Bindings: TIdSocketHandles read GetBindings write SetBindings;
    property TLS: Boolean read FTLS write FTLS;
    property TLSOptions: TsgcUDPTLS_Options read FTLSOptions
      write SetTLSOptions;
    { properties }

    { start / stop }
  public
    procedure Start;
    procedure Stop;
    procedure ReStart;
    { start / stop }

    { properties }
  private
    FUseNagle: Boolean;
    FIPVersion: TIdIPVersion;
    FConnectTimeout: Integer;
    FReadTimeout: Integer;
    FWriteTimeout: Integer;
  public
    property UseNagle: Boolean read FUseNagle write FUseNagle stored True;
    property IPVersion: TIdIPVersion read FIPVersion write FIPVersion;
    property ConnectTimeout: Integer read FConnectTimeout write FConnectTimeout;
    property WriteTimeout: Integer read FWriteTimeout write FWriteTimeout;
    property ReadTimeout: Integer read FReadTimeout write FReadTimeout;
    { properties }

    { Active }
  private
    FActive: Boolean;
  protected
    function GetActive: Boolean; virtual;
    procedure SetActive(const Value: Boolean); virtual;
  public
    property Active: Boolean read GetActive write SetActive;
    { Active }

    { write data }
  protected
    procedure DoWriteData(const aIP: string; aPort: Word; const aBytes: TBytes;
        const aSourceIP: string = ''; aSourcePort: Word = 0); virtual;
  public
    procedure WriteData(const aIP: string; aPort: Word; const aValue: string);
        overload;
    procedure WriteData(const aIP: string; aPort: Word; const aBytes: TBytes);
        overload;
  public
    procedure WriteData(const aSourceIP: string; const aSourcePort: Word;
        const aIP: string; aPort: Word; const aValue: string); overload;
    procedure WriteData(const aSourceIP: string; const aSourcePort: Word;
        const aIP: string; aPort: Word; const aBytes: TBytes); overload;
    { write data }

    { events }
  private
    FOnShutdown: TNotifyEvent;
    FOnStartup: TNotifyEvent;
  protected
    procedure DoStartupEvent; virtual;
    procedure DoShutdownEvent; virtual;
  protected
    property OnStartup: TNotifyEvent read FOnStartup write FOnStartup;
    property OnShutdown: TNotifyEvent read FOnShutdown write FOnShutdown;
    { events }
  end;

  TUDPServerThread = class(TThread)
  private
    FServer: TsgcUDPServer_Base;
    FMethod: string;
  protected
    procedure Execute; override;
  public
    constructor Create(const aMethod: string;
      const aServer: TsgcUDPServer_Base);
    class procedure Start(const aServer: TsgcUDPServer_Base);
    class procedure Stop(const aServer: TsgcUDPServer_Base);
    class procedure ReStart(const aServer: TsgcUDPServer_Base);
  end;

  TsgcUDPSocketServerClass = class of TsgcUDPSocketServer;

{$ENDIF}

implementation

{$IFDEF SGC_UDP}

uses
{$IFDEF SGC_INDY}sgcIdStackConsts, {$ELSE}IdStackConsts, {$ENDIF}
  sgcSocket_Const, sgcWebSocket_Helpers, sgcUDP_Client;

type
  TsgcUDPCLient_Base_Hack = class(TsgcUDPCLient_Base)
  end;

constructor TsgcUDPServer_Base.Create(aOwner: TComponent);
begin
  inherited;
  NotifyEvents := neNoSync;
  Port := CS_DEFAULT_PORT;
  TLS := False;
  FTLSOptions := TsgcUDPTLS_Options.Create;
  FWatchDog := TsgcUDPWatchDogServer_Options.Create;
  FWatchDog.Enabled := False;
  FWatchDog.Interval := 10;
  FConnectTimeout := 0;
  FReadTimeout := -1;
  FHeartBeat := TsgcUDPHeartBeat_Options.Create;
  FLogFile := TsgcUDPLogFile.Create;
end;

destructor TsgcUDPServer_Base.Destroy;
begin
  Active := False;
  sgcFree(FHeartBeat);
  sgcFree(FUDPServer);
  sgcFree(FLogFile);
  sgcFree(FWatchDog);
  sgcFree(FIdLogFile);
  inherited;
end;

function TsgcUDPServer_Base.AddBinding(const aIPAddress: string; aPort: Integer;
    const aName: string = ''): TIdSocketHandle;
begin
  Result := UDPServer.AddBinding(aIPAddress, aPort, aName);
end;

function TsgcUDPServer_Base.DoCreateUDPSocket(const ABinding: TIdSocketHandle)
  : TsgcUDPSocketServer;
begin
  result := nil;
  Try
    result := TsgcUDPSocketServer.Create;
    result.Transport := trpUDP;
    result.OnException := OnSocketExceptionEvent;
    TsgcUDPSocketServer(result).FGuid :=
      EncodeBase64(ABinding.PeerIP + ':' + IntToStr(ABinding.PeerPort));
    if LogFile.Enabled and (LogFile.FileName <> '') then
      result.FLogFile := GetIdLogFile;
    if ABinding.IPVersion = Id_IPv6 then
      result.DoInitialize(self, ABinding.PeerIP, ABinding.PeerPort, ABinding.IP, ABinding.Port, ipV6)
    else
      result.DoInitialize(self, ABinding.PeerIP, ABinding.PeerPort, ABinding.IP, ABinding.Port, ipV4)
  Except
    On E: Exception do
      DoException(result, E);
  end;
end;

procedure TsgcUDPServer_Base.DoStartHeartBeat;
begin
  if HeartBeat.Enabled then
  begin
    if not Assigned(FHeartBeatTimer) then
    begin
      FHeartBeatTimer := TsgcTimer.Create;
      FHeartBeatTimer.DebugName := 'HeartBeat';
      FHeartBeatTimer.NotifyEvents := NotifyEvents;
      FHeartBeatTimer.OnTimer := OnHeartBeatEvent;
      FHeartBeatTimer.OnException := OnHeartBeatExceptionEvent;
    end;
    FHeartBeatTimer.Interval := HeartBeat.Interval * 1000;

    if FHeartBeatTimer.Interval > 0 then
    begin
      if not FHeartBeatTimer.Enabled then
        FHeartBeatTimer.Enabled := True;
    end;
  end;
end;

procedure TsgcUDPServer_Base.DoStartWatchDog;
begin
  if WatchDog.Enabled then
  begin
    if not Assigned(FWatchDogTimer) then
    begin
      FWatchDogTimer := TsgcTimer.Create;
      FWatchDogTimer.DebugName := 'WatchDog';
      FWatchDogTimer.NotifyEvents := NotifyEvents;
      FWatchDogTimer.OnTimer := OnWatchDogEvent;
      FWatchDogTimer.OnException := OnWatchDogExceptionEvent;
    end;
    FWatchDogTimer.Interval := WatchDog.Interval * 1000;

    if FWatchDogTimer.Interval > 0 then
    begin
      if not FWatchDogTimer.Enabled then
        FWatchDogTimer.Enabled := True;
    end;
  end;
end;

procedure TsgcUDPServer_Base.DoStopHeartBeat;
begin
  if Assigned(FHeartBeatTimer) then
    FHeartBeatTimer.Enabled := False;
end;

procedure TsgcUDPServer_Base.DoStopWatchDog;
begin
  sgcThreadFree(FWatchDogTimer);
end;

procedure TsgcUDPServer_Base.DoWriteData(const aIP: string; aPort: Word;
    const aBytes: TBytes; const aSourceIP: string = ''; aSourcePort: Word = 0);
var
  oBinding: TIdSocketHandle;
begin
  // ... protect binding property from multiple thread access
  DoEnterCS;
  Try
    // ... get binding
    oBinding := nil;
    if aSourceIP <> '' then
      oBinding := UDPServer.GetSocketBinding(aSourceIP, aSourcePort);
    if not Assigned(oBinding) then
      oBinding := UDPServer.Binding;

    // ... send data
    if sgcContainsText(aIP, ':') then
    begin
      oBinding.SetPeer(aIP, aPort, Id_IPv6);
      oBinding.SendTo(aIP, aPort, TIdBytes(aBytes), Id_IpV6);
    end
    else
    begin
      oBinding.SetPeer(aIP, aPort, Id_IPv4);
      oBinding.SendTo(aIP, aPort, TIdBytes(aBytes), Id_IpV4);
    end;
  Finally
    DoLeaveCS;
  End;
end;

function TsgcUDPServer_Base.GetActive: Boolean;
begin
  if not IsDesigning and not IsLoading then
    result := UDPServer.Active
  else
    result := FActive;
end;

function TsgcUDPServer_Base.GetUDPServer: TsgcIdUDPServer;
begin
  if not Assigned(FUDPServer) then
  begin
    FUDPServer := TsgcIdUDPServer.Create(nil);
    FUDPServer.ThreadedEvent := True; // prevent indy synchronization
    FUDPServer.OnUDPRead := OnClientReadEvent;
    FUDPServer.OnUDPException := OnClientExceptionEvent;
  end;
  result := FUDPServer;
end;

procedure TsgcUDPServer_Base.Loaded;
begin
  inherited;
  if Active <> FActive then
    Active := FActive;
end;

procedure TsgcUDPServer_Base.DoShutdownEvent;
begin
  FWatchDogMonitorSecret := '';
  DoStopWatchDog;

  if Assigned(FOnShutdown) then
    FOnShutdown(self);
end;

procedure TsgcUDPServer_Base.DoStartupEvent;
begin
  FWatchDogMonitorSecret := NewGuid;

  if FWatchDogEnabled then
    DoStartWatchDog;

  if Assigned(FOnStartup) then
    FOnStartup(self);
end;

procedure TsgcUDPServer_Base.DoWatchDogMonitor;
var
  oClient: TsgcUDPCLient_Base;
begin
  if FWatchDogMonitorSecret = '' then
    exit;

  if FWatchDogMonitorWaiting then
  begin
    if sgcGetTickDiff(FWatchDogMonitorStart, sgcGetTicks) >=
      Cardinal(WatchDog.Monitor.TimeOut * 1000) then
    begin
      FWatchDogMonitorWaiting := False;
      ReStart;
      exit;
    end;
  end;

  oClient := TsgcUDPCLient_Base.Create(nil);
  Try
    TsgcUDPCLient_Base_Hack(oClient).NotifyEvents := neNoSync;
    if Bindings.Count > 0 then
      oClient.Host := Bindings[0].IP;
    if (oClient.Host = '0.0.0.0') or (oClient.Host = '0:0:0:0:0:0:0:0') or
      (oClient.Host = '') then
      oClient.Host := '127.0.0.1';
    oClient.Port := Port;
    oClient.WriteData(FWatchDogMonitorSecret);
    if not FWatchDogMonitorWaiting then
      FWatchDogMonitorStart := sgcGetTicks;
    FWatchDogMonitorWaiting := True;
  Finally
    sgcFree(oClient);
  End;
end;

function TsgcUDPServer_Base.DoWatchDogMonitorRead(const Bytes: TBytes): Boolean;
begin
  result := False;
  if WatchDog.Monitor.Enabled then
  begin
    if sgcGetUTF8StringFromBytes(Bytes) = FWatchDogMonitorSecret then
    begin
      FWatchDogMonitorWaiting := False;
      result := True;
    end;
  end;
end;

function TsgcUDPServer_Base.GetBinding(const aIPAddress: string; aPort: Integer)
    : TIdSocketHandle;
begin
  Result := UDPServer.GetSocketBinding(aIPAddress, aPort);
end;

function TsgcUDPServer_Base.GetBindings: TIdSocketHandles;
begin
  result := UDPServer.Bindings;
end;

function TsgcUDPServer_Base.GetIdLogFile: TsgcIdUDPLogFile;
begin
  if not Assigned(FIdLogFile) then
    FIdLogFile := TsgcIdUDPLogFile.Create(nil);
  FIdLogFile.FileName := LogFile.FileName;
  result := FIdLogFile;
end;

{$IFNDEF INDY10_2}

procedure TsgcUDPServer_Base.OnClientExceptionEvent(Sender: TObject;
  ABinding: TIdSocketHandle; const AMessage: String;
  const AExceptionClass: TClass);
{$ELSE}

procedure TsgcUDPServer_Base.OnClientExceptionEvent
  (AThread: TIdUDPListenerThread; ABinding: TIdSocketHandle;
  const AMessage: String; const AExceptionClass: TClass);
{$ENDIF}
var
  oSocket: TsgcUDPSocket;
begin
  if IsDestroying then
    exit;

  Try
    oSocket := DoCreateUDPSocket(ABinding);
    Try
      oSocket.RawException := Exception.Create(AMessage);
      DoNotifyUDPException(oSocket);
    Finally
      sgcFree(oSocket);
    End;
  Except
    On E: Exception do
      DoException(oSocket, E);
  End;
end;

{$IFNDEF INDY10_2}

procedure TsgcUDPServer_Base.OnClientReadEvent(Sender: TObject; AData: TIdBytes;
  ABinding: TIdSocketHandle);
{$ELSE}

procedure TsgcUDPServer_Base.OnClientReadEvent(AThread: TIdUDPListenerThread;
{$IFDEF INDY10_6}const {$ENDIF} AData:
{$IFDEF INDY10_5_9}{$IFNDEF INDY10_6} Array of Byte{$ELSE}TIdBytes{$ENDIF}{$ELSE}TIdBytes{$ENDIF}; ABinding: TIdSocketHandle);
{$ENDIF}
var
  oSocket: TsgcUDPSocket;
  oBytes: TBytes;
begin
  if IsDestroying then
    exit;

  Try

{$IFDEF INDY10_5_9}
{$IFNDEF INDY_10_6}
    SetLength(oBytes, Length(AData));
    Move(AData[0], oBytes[0], Length(AData));
{$ELSE}
    oBytes := TBytes(AData);
{$ENDIF}
{$ELSE}
    oBytes := TBytes(AData);
{$ENDIF}
    if DoWatchDogMonitorRead(oBytes) then
      exit;

    oSocket := DoCreateUDPSocket(ABinding);
    Try
      oSocket.LogData(oBytes);
      oSocket.MsgRead := oBytes;
      DoNotifyUDPRead(oSocket);
    Finally
      sgcFree(oSocket);
    End;
  Except
    On E: Exception do
      DoException(oSocket, E);
  End;
end;

procedure TsgcUDPServer_Base.OnHeartBeatEvent(Sender: TObject);
begin
  // not implemented
end;

procedure TsgcUDPServer_Base.OnHeartBeatExceptionEvent(Sender: TObject; E:
    Exception);
begin
  // not implemented
end;

procedure TsgcUDPServer_Base.OnSocketExceptionEvent(Sender: TObject;
  const E: Exception);
begin
  DoException(TsgcUDPSocket(Sender), E);
end;

procedure TsgcUDPServer_Base.OnWatchDogEvent(Sender: TObject);
var
  vHandled: Boolean;
begin
  if IsDestroying then
    exit;

  vHandled := False;
  if Assigned(FOnBeforeWatchDog) then
    FOnBeforeWatchDog(self, vHandled);
  if not vHandled then
  begin
    case NotifyEvents of
      neNoSync:
        begin
          if not Active then
            Start;
        end
    else
      begin
        if not Active then
          NotifyMethod(Start)
        else if WatchDog.Monitor.Enabled then
          NotifyMethod(DoWatchDogMonitor);
      end;
    end;

    if WatchDog.Attempts > 0 then
    begin
      FWatchDogAttempts := FWatchDogAttempts + 1;

      if FWatchDogAttempts >= WatchDog.Attempts then
        DoStopWatchDog;
    end;
  end;
end;

procedure TsgcUDPServer_Base.OnWatchDogExceptionEvent(Sender: TObject;
  E: Exception);
begin
  DoException(nil, E);
end;

function TsgcUDPServer_Base.RemoveBinding(const aIPAddress: string; aPort:
    Integer): Boolean;
begin
  Result := UDPServer.RemoveBinding(aIPAddress, aPort);
end;

procedure TsgcUDPServer_Base.ReStart;
begin
  TUDPServerThread.ReStart(self);
end;

procedure TsgcUDPServer_Base.SetActive(const Value: Boolean);
var
  oSocketHandle: TIdSocketHandle;
begin
  Try
    if not IsDesigning and not IsLoading then
    begin
      if Value then
      begin
        if not UDPServer.Active then
        begin
          // ... properties
          UDPServer.Host := Host;
          UDPServer.DefaultPort := Port;
          UDPServer.IPVersion := IPVersion;
          // ... WatchDog
          FWatchDogEnabled := WatchDog.Enabled;
          // ... binding
          if (Host <> '') and (Bindings.Count = 0) then
          begin
            oSocketHandle := Bindings.Add;
            oSocketHandle.IP := Host;
            oSocketHandle.Port := Port;
          end;
          // ... Start
          UDPServer.Active := True;
          // ... event
          DoStartupEvent;
        end;
      end
      else
      begin
        FWatchDogEnabled := False;
        UDPServer.Active := False;
        // ... event
        DoShutdownEvent;
      end;
    end
    else
      FActive := Value;
  Except
    On E: Exception do
    begin
      // ... exception
      // DoException(Connection, E);
      // ... watchdog
      if FWatchDogEnabled then
        DoStartWatchDog;
    end;
  End;
end;

procedure TsgcUDPServer_Base.SetBindings(const Value: TIdSocketHandles);
begin
  UDPServer.Bindings := Value;
end;

procedure TsgcUDPServer_Base.SetHeartBeat(const Value
  : TsgcUDPHeartBeat_Options);
begin
  if Assigned(FHeartBeat) then
    FHeartBeat.Assign(Value);
end;

procedure TsgcUDPServer_Base.SetLogFile(const Value: TsgcUDPLogFile);
begin
  if Assigned(FLogFile) then
    FLogFile.Assign(Value);
end;

procedure TsgcUDPServer_Base.SetTLSOptions(const Value: TsgcUDPTLS_Options);
begin
  FTLSOptions.Assign(Value);
end;

procedure TsgcUDPServer_Base.SetWatchDog(const Value
  : TsgcUDPWatchDogServer_Options);
begin
  FWatchDog.Assign(Value);
end;

procedure TsgcUDPServer_Base.Start;
begin
  TUDPServerThread.Start(self);
end;

procedure TsgcUDPServer_Base.Stop;
begin
  TUDPServerThread.Stop(self);
end;

procedure TsgcUDPServer_Base.WriteData(const aIP: string; aPort: Word; const
    aValue: string);
begin
  WriteData(aIP, aPort, sgcGetBytesFromUTF8String(aValue));
end;

procedure TsgcUDPServer_Base.WriteData(const aIP: string; aPort: Word; const
    aBytes: TBytes);
begin
  DoWriteData(aIP, aPort, aBytes);
end;

procedure TsgcUDPServer_Base.WriteData(const aSourceIP: string;
    const aSourcePort: Word; const aIP: string; aPort: Word; const aValue:
    string);
begin
  WriteData(aSourceIP, aSourcePort, aIP, aPort, sgcGetBytesFromUTF8String(aValue));
end;

procedure TsgcUDPServer_Base.WriteData(const aSourceIP: string;
    const aSourcePort: Word; const aIP: string; aPort: Word; const aBytes:
    TBytes);
begin
  DoWriteData(aIP, aPort, aBytes, aSourceIP, aSourcePort);
end;

procedure TsgcUDPSocketServer.Assign(const aSource: TsgcUDPSocket);
begin
  inherited;
  FServer := TsgcUDPSocketServer(aSource).FServer;
end;

procedure TsgcUDPSocketServer.DoInitialize(const aServer: TsgcUDPServer_Base;
    const aPeerIp: string; aPeerPort: Integer; aLocalIp: string; aLocalPort:
    Integer; aIPVersion: TwsIPVersion);
begin
  FServer := aServer;
  FIp := aPeerIp;
  FLocalIp := aLocalIp;
  FPort := aPeerPort;
  FLocalPort := aLocalPort;
  FIPVersion := aIPVersion;
end;

procedure TsgcUDPSocketServer.DoWriteData(const aValue: string);
begin
  inherited;
  DoWriteData(TBytes(ToBytes(aValue)));
end;

procedure TsgcUDPSocketServer.DoWriteData(const aBytes: TBytes);
begin
  inherited;
  if Assigned(FServer) then
    FServer.WriteData(FIp, FPort, aBytes)
end;

constructor TUDPServerThread.Create(const aMethod: string;
  const aServer: TsgcUDPServer_Base);
begin
  FMethod := aMethod;
  FServer := aServer;
  FreeOnTerminate := True;
  inherited Create(False);
end;

class procedure TUDPServerThread.Start(const aServer: TsgcUDPServer_Base);
begin
  Create(CS_START, aServer);
end;

class procedure TUDPServerThread.Stop(const aServer: TsgcUDPServer_Base);
begin
  Create(CS_STOP, aServer);
end;

procedure TUDPServerThread.Execute;
begin
  inherited;
  if Assigned(FServer) then
  begin
    // ... Start
    if FMethod = CS_START then
    begin
      if FServer.DoAssignedCS then
      begin
        FServer.EnterCS;
        Try
          FServer.Active := True
        Finally
          FServer.LeaveCS;
        End;
      end;
    end
    // ... Stop
    else if FMethod = CS_STOP then
    begin
      if FServer.DoAssignedCS then
      begin
        FServer.EnterCS;
        Try
          FServer.Active := False;
        Finally
          FServer.LeaveCS;
        End;
      end;
    end
    // ... Restart
    else if FMethod = CS_RESTART then
    begin
      if FServer.DoAssignedCS then
      begin
        if FServer.Active then
          Stop(FServer);
        repeat
          sleep(1);
        until (not FServer.Active);
        Start(FServer);
      end;
    end;
    Terminate;
  end;
end;

class procedure TUDPServerThread.ReStart(const aServer: TsgcUDPServer_Base);
begin
  Create(CS_RESTART, aServer);
end;

constructor TsgcUDPWatchDogMonitorServer_Options.Create;
begin
  inherited;
  FEnabled := False;
  FTimeOut := 10;
end;

procedure TsgcUDPWatchDogMonitorServer_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcUDPWatchDogMonitorServer_Options then
  begin
    Enabled := TsgcUDPWatchDogMonitorServer_Options(aSource).Enabled;
    TimeOut := TsgcUDPWatchDogMonitorServer_Options(aSource).TimeOut;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcUDPWatchDogServer_Options.Create;
begin
  inherited;
  FMonitor := TsgcUDPWatchDogMonitorServer_Options.Create;
end;

destructor TsgcUDPWatchDogServer_Options.Destroy;
begin
  sgcFree(FMonitor);
  inherited;
end;

procedure TsgcUDPWatchDogServer_Options.SetMonitor
  (const Value: TsgcUDPWatchDogMonitorServer_Options);
begin
  FMonitor := Value;
end;

function TsgcIdUDPServer.AddBinding(const aIPAddress: string; aPort: Integer;
    const aName: string = ''): TIdSocketHandle;
{$IFDEF INDY10_2}
var
  oThread: TIdUDPListenerThread;
{$ENDIF}
begin
{$IFDEF INDY10_2}
  result := GetSocketBinding(aIPAddress, aPort);
  if result = nil then
  begin
    result := Bindings.Add;
    result.IP := aIPAddress;
    result.Port  := aPort;
    {$IFDEF SGC_INDY_LIB}
    result.Name := aName;
    {$ENDIF}
{$IFDEF LINUX}
    result.AllocateSocket(Integer(Id_SOCK_DGRAM));
{$ELSE}
    result.AllocateSocket(Id_SOCK_DGRAM);
{$ENDIF}
    // do not overwrite if the default. This allows ReuseSocket to be set per binding
    {$IFDEF INDY10_5_9}
    if FReuseSocket <> rsOSDependent then
      result.ReuseSocket := FReuseSocket;
    {$ENDIF}
    DoBeforeBind(result);
    result.Bind;

    DoAfterBind;

    oThread := FThreadClass.Create(Self, result);
    oThread.Name := Name + ' Listener #' + IntToStr(aPort);
    {$IFDEF WINDOWS}
    {$IFNDEF LAZARUS}
    oThread.Priority := tpListener;
    {$ENDIF}
    {$ENDIF}
    FListenerThreads.Add(oThread);
    oThread.Start;
  end;
{$ELSE}
  raise Exception.Create('Indy version is too old.');
{$ENDIF}
end;

function TsgcIdUDPServer.GetActive: Boolean;
begin
  Result := Assigned(FCurrentBinding);
end;

function TsgcIdUDPServer.GetSocketBinding(const aIPAddress: string; aPort:
    Integer): TIdSocketHandle;
var
  i: Integer;
begin
  result := nil;

  for i := Bindings.Count - 1 Downto 0 do
  begin
    if (Bindings[i].IP = aIPAddress) and (Bindings[i].Port = aPort) then
    begin
      result := Bindings[i];
      break;
    end;
  end;
end;

function TsgcIdUDPServer.RemoveBinding(const aIPAddress: string; aPort:
    Integer): Boolean;
{$IFDEF INDY10_2}
var
  i, j: Integer;
  oList: TList{$IFDEF NEXTGEN}<TIdUDPListenerThread>{$ENDIF};
  oThread: TIdUDPListenerThread;
{$ENDIF}
begin
{$IFDEF INDY10_2}
  result := False;

  for i := Bindings.Count - 1 Downto 0 do
  begin
    if (Bindings[i].IP = aIPAddress) and (Bindings[i].Port = aPort) then
    begin
      // delete thread
      if Assigned(FListenerThreads) then
      begin
        oList := FListenerThreads.LockList;
        Try
          for j := oList.Count - 1 Downto 0 do
          begin
            oThread := {$IFDEF NEXTGEN}oList[j]{$ELSE}TIdUDPListenerThread(oList[j]){$ENDIF};
            if oThread.Binding = Bindings[i] then
            begin
              oThread.Stop;
              oThread.Binding.CloseSocket;
              oThread.WaitFor;
              oThread.Free;
              oList.Delete(j);
              break;
            end;
          end;
        Finally
          FListenerThreads.UnlockList;
        End;
      end;

      // delete binding
      Bindings.Delete(i);

      result := True;
    end;
  end;
{$ELSE}
  raise Exception.Create('Indy version is too old.');
{$ENDIF}
end;

{$ENDIF}

end.
