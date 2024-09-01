{ ***************************************************************************
  sgcUDP component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcUDP_Client;

interface

{$I sgcVer.inc}
{$IFDEF SGC_UDP}

uses
  Classes, SysUtils,
{$IFDEF MSWINDOWS}Windows{$ELSE}SyncObjs{$ENDIF},
{$IFDEF NEXTGEN}System.Generics.Collections, {$ELSE}Contnrs, {$ENDIF}
  // indy
{$IFDEF SGC_INDY}sgcIdGlobal{$ELSE}IdGlobal{$ENDIF},
{$IFDEF SGC_INDY}sgcIdUDPClient{$ELSE}IdUDPClient{$ENDIF},
{$IFDEF SGC_INDY}sgcIdException{$ELSE}IdException{$ENDIF},
{$IFDEF SGC_INDY}sgcIdSocks{$ELSE}IdSocks{$ENDIF},
  // sgcWebSocket
  sgcBase_Classes, sgcBase_Helpers, sgcSocket_Classes_Indy,
  sgcUDP_Classes, sgcUDP_Classes_Indy, sgcWebSocket_Classes_SyncObjs,
  sgcWebSocket_Classes_Queues, sgcWebSocket_Types, sgcWebSocket_Helpers;

type

  TsgcUDPReadThread = class;
  TsgcIdUDPClient = class;

  TsgcReadThreadExceptionEvent = procedure(aException: Exception) of object;
  TsgcReadThreadMessageEvent = procedure(Sender: TObject; const aPeerIP: string;
    aPeerPort: Word) of object;
  TsgcUDPReadEvent = procedure(Sender: TObject; const Bytes: TBytes;
    const aPeerIP: string; aPeerPort: Word) of object;

  TsgcUDPCommands = (tcmNone, tcmActive, tcmNotActive, tcmWriteBytes);
  TsgcUDPMode = (udpmDefault, udpmCommands);

  TsgcQueueCommand = class(TsgcQueueItemBase)
  private
    FBytes: TIdBytes;
    FCommand: TsgcUDPCommands;
    procedure SetBytes(const Value: TIdBytes);
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    property Bytes: TIdBytes read FBytes write SetBytes;
    property Command: TsgcUDPCommands read FCommand write FCommand;
  end;

  TsgcQueueCommands = class(TsgcQueue)
  public
    procedure AddCommand(const aCommand: TsgcUDPCommands);
    procedure AddBytes(const ABytes: TIdBytes);
  end;

  TsgcUDPReadThread = class(TsgcIdThread)
  private
    FReadSocket: Boolean;
  private
    FClient: TsgcIdUDPClient;
  private
    FOnReadMessage: TsgcReadThreadMessageEvent;
    FOnReadThread: TNotifyEvent;
    FOnReadException: TsgcReadThreadExceptionEvent;
  protected
    procedure DoOnExceptionEvent(const E: Exception); virtual;
  private
    FExecuted: Boolean;
    FUDPMode: TsgcUDPMode;
  protected
    procedure Run; override;
  public
    procedure Terminate; override;
  public
    constructor Create(aClient: TsgcIdUDPClient); reintroduce;
    destructor Destroy; override;
  public
    property OnReadThread: TNotifyEvent read FOnReadThread write FOnReadThread;
    property OnReadMessage: TsgcReadThreadMessageEvent read FOnReadMessage
      write FOnReadMessage;
    property OnReadException: TsgcReadThreadExceptionEvent read FOnReadException
      write FOnReadException;
  public
    property ReadSocket: Boolean read FReadSocket write FReadSocket;
    property UDPMode: TsgcUDPMode read FUDPMode write FUDPMode;
  End;

  TsgcIdUDPClient = class(TIdUDPClient)
  private
    FOnThread: TNotifyEvent;
    FOnRead: TsgcUDPReadEvent;
    FOnReadException: TsgcReadThreadExceptionEvent;
    FUDPRead: TsgcUDPReadThread;
    FUDPMode: TsgcUDPMode;
  protected
    FReadBuffer: TBytes;
    FReadBufferSize: Integer;
  protected
    procedure OnReadThreadEvent(Sender: TObject); virtual;
    procedure OnReadEvent(Sender: TObject; const aPeerIP: string;
      aPeerPort: Word); virtual;
    procedure OnReadExceptionEvent(aException: Exception); virtual;
  public
  protected
    procedure InitComponent; override;
  private
    FCS: TsgcCriticalSection;
  protected
    procedure DoEnterCS; virtual;
    procedure DoLeaveCS; virtual;
  public
    procedure EnterCS;
    procedure LeaveCS;
  private
    function GetUDPRead: TsgcUDPReadThread;
  public
    property UDPRead: TsgcUDPReadThread read GetUDPRead;
  public
    destructor Destroy; override;
  public
    property UDPMode: TsgcUDPMode read FUDPMode write FUDPMode;
  published
    property OnThread: TNotifyEvent read FOnThread write FOnThread;
    property OnRead: TsgcUDPReadEvent read FOnRead write FOnRead;
    property OnReadException: TsgcReadThreadExceptionEvent read FOnReadException
      write FOnReadException;
  end;

  TsgcUDPRetransmissionClient_Options = class(TsgcUDPRetransmission_Options)

  end;

  TsgcUDPSocketClient = class(TsgcUDPSocket_Indy)
    { from TsgcUDPSocket }
  public
    procedure Assign(const aSource: TsgcUDPSocket); override;
    { from TsgcUDPSocket }

    { initialize }
  private
    FClient: TsgcIdUDPClient;
  protected
    procedure SetClient(const aValue: TsgcIdUDPClient);
    { initialize }

    { write data }
  protected
    procedure DoWriteData(const aIPAddress: string; aPort: Word;
      const aValue: string); overload; override;
    procedure DoWriteData(const aIPAddress: string; aPort: Word;
      const aValue: TBytes); overload; override;
  protected
    procedure DoWriteData(const aValue: string); overload; override;
    procedure DoWriteData(const aValue: TBytes); overload; override;
    { write data }
  end;

  TsgcUDPCLient_Base = class(TsgcUDPComponent)
    { UDP client }
  private
    FUDPClient: TsgcIdUDPClient;
    function GetUDPClient: TsgcIdUDPClient;
  protected
    procedure DoStopReadThread; virtual;
  protected
    property UDPClient: TsgcIdUDPClient read GetUDPClient write FUDPClient;
    { UDP client }

    { properties }
  private
    FReadingThread: Boolean;
    { properties }

    { from thread }
  protected
    procedure OnReadEvent(Sender: TObject; const ABytes: TBytes;
      const aPeerIP: string; aPeerPort: Word); virtual;
    procedure OnReadExceptionEvent(aException: Exception); virtual;
    procedure OnThreadEvent(Sender: TObject); virtual;
  protected
    procedure DoReadData(const ABytes: TBytes); virtual;
    { from thread }

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

    { RetransmissionOptions }
  private
    FRetransmissionOptions: TsgcUDPRetransmissionClient_Options;
    procedure SetRetransmissionOptions(const Value
      : TsgcUDPRetransmissionClient_Options);
  private
    FRetransmissions: TsgcUDPRetransmissions;
    function GetRetransmissions: TsgcUDPRetransmissions;
  protected
    procedure OnRetransmissionRetryEvent(Sender: TObject; const ABytes: TBytes;
      const aIPAddress: string; aPort: Word); virtual;
    procedure OnRetransmissionTimeoutEvent(Sender: TObject;
      const ABytes: TBytes; const aIPAddress: string; aPort: Word); virtual;
    procedure OnRetransmissionExceptionEvent(Sender: TObject;
      E: Exception); virtual;
  protected
    property Retransmissions: TsgcUDPRetransmissions read GetRetransmissions
      write FRetransmissions;
  protected
    property RetransmissionOptions: TsgcUDPRetransmissionClient_Options
      read FRetransmissionOptions write SetRetransmissionOptions;
    { RetransmissionOptions }

    { commands }
  private
    FCommands: TsgcQueueCommands;
    function GetCommands: TsgcQueueCommands;
  protected
    property Commands: TsgcQueueCommands read GetCommands write FCommands;
  protected
    procedure OnCommandExecute(const aCommand: TsgcUDPCommands); virtual;
    procedure DoCommands; virtual;
  public
    procedure AddCommand(const aCommand: TsgcUDPCommands);
    procedure AddBytes(const ABytes: TIdBytes);
    { commands }

    { socket }
  protected
    FUDPSocket: TsgcUDPSocketClient;
    function GetUDPSocket: TsgcUDPSocketClient; virtual;
  protected
    property UDPSocket: TsgcUDPSocketClient read GetUDPSocket;
    { socket }

    { events }
  protected
    procedure OnClientConnectEvent(Sender: TObject); virtual;
    procedure OnClientDisconnectEvent(Sender: TObject); virtual;
  protected
    procedure OnSocketExceptionEvent(Sender: TObject;
      const E: Exception); virtual;
    { events }

    { proxy }
  private
    FProxy: TsgcUDPProxy_Options;
    FProxySocks: TIdSocksInfo;
  protected
    procedure SetProxy(const Value: TsgcUDPProxy_Options); virtual;
    function GetProxySocks: TIdSocksInfo;
  public
    property Proxy: TsgcUDPProxy_Options read FProxy write SetProxy;
    { proxy }

    { LogFile }
  private
    FLogFile: TsgcUDPLogFile;
    FIdLogFile: TsgcIdLogFileClient;
  protected
    function GetIdLogFile: TsgcIdLogFileClient;
    procedure SetLogFile(const Value: TsgcUDPLogFile); virtual;
  public
    property LogFile: TsgcUDPLogFile read FLogFile write SetLogFile;
    { LogFile }

    { constructor / destructor }
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { host }
  private
    FTLS: Boolean;
    FTLSOptions: TsgcUDPTLS_Options;
  protected
    procedure SetTLSOptions(const Value: TsgcUDPTLS_Options); virtual;
    function GetHost: String;
    function GetPort: Integer;
    function GetIPVersion: TIdIPVersion;
    procedure SetHost(const Value: String);
    procedure _SetPort(const Value: Integer);
    procedure SetIPVersion(const Value: TIdIPVersion);
  public
    property Host: String read GetHost write SetHost;
    property Port: Integer read GetPort write _SetPort;
    property IPVersion: TIdIPVersion read GetIPVersion write SetIPVersion;
    property TLS: Boolean read FTLS write FTLS;
    property TLSOptions: TsgcUDPTLS_Options read FTLSOptions
      write SetTLSOptions;
    { host }

    { properties }
  private
    FUseNagle: Boolean;
    FUDPMode: TsgcUDPMode;
  public
    property UseNagle: Boolean read FUseNagle write FUseNagle stored True;
    property UDPMode: TsgcUDPMode read FUDPMode write FUDPMode;
    { properties }

    { write data }
  protected
    procedure DoWriteData(const aIPAddress: string; aPort: Word;
      const aValue: string); overload; virtual;
    procedure DoWriteData(const aIPAddress: string; aPort: Word;
      const aValue: TBytes); overload; virtual;
  protected
    procedure DoWriteData(const aValue: string); overload; virtual;
    procedure DoWriteData(const aValue: TBytes); overload; virtual;
  public
    procedure WriteData(const aIPAddress: string; aPort: Word;
      const aValue: string); overload;
    procedure WriteData(const aIPAddress: string; aPort: Word;
      const aValue: TBytes); overload;
  public
    procedure WriteData(const aValue: string); overload;
    procedure WriteData(const aValue: TBytes); overload;
    { write data }

    { events }
  private
    FOnRetransmissionTimeout: TsgcUDPRetransmissionTimeoutEvent;
  protected
    property OnRetranmissionTimeout: TsgcUDPRetransmissionTimeoutEvent
      read FOnRetransmissionTimeout write FOnRetransmissionTimeout;
    { events }
  end;

  TsgcUDPSocketClientClass = class of TsgcUDPSocketClient;

{$ENDIF}

implementation

{$IFDEF SGC_UDP}

uses
{$IFDEF SGC_INDY}sgcIdStackConsts, {$ELSE}IdStackConsts, {$ENDIF}
  sgcSocket_Const;

constructor TsgcUDPReadThread.Create(aClient: TsgcIdUDPClient);
begin
{$IFDEF SGC_DEBUG}
  DoLog(self, nil, 'UDPRead_Create_Start');
{$ENDIF}
  FClient := aClient;
  FExecuted := False;
  FReadSocket := False;
  inherited Create(False);
{$IFDEF SGC_DEBUG}
  DoLog(self, nil, 'UDPRead_Create_End');
{$ENDIF}
end;

destructor TsgcUDPReadThread.Destroy;
begin
{$IFDEF SGC_DEBUG}
  DoLog(self, nil, 'UDPRead_Destroy_Start');
{$ENDIF}
  inherited;
{$IFDEF SGC_DEBUG}
  DoLog(self, nil, 'UDPRead_Destroy_End');
{$ENDIF}
end;

procedure TsgcUDPReadThread.DoOnExceptionEvent(const E: Exception);
begin
  if Assigned(FOnReadException) then
  begin
    FIsExecuting := True;
    Try
      FOnReadException(E);
    Finally
      FIsExecuting := False;
    End;
  end;
end;

procedure TsgcUDPReadThread.Run;
var
  vPeerIP: string;
  vPeerPort: Word;
begin
  inherited;

  Try
    FExecuted := True;

    if not Terminated then
      if Assigned(FOnReadThread) then
        FOnReadThread(self);

    vPeerIP := '';
    vPeerPort := 0;
    if ReadSocket then
    begin
      if not Terminated then
      begin
        if Assigned(FClient) then
        begin
          FClient.EnterCS;
          Try
            SetLength(FClient.FReadBuffer, FClient.BufferSize);
            FClient.FReadBufferSize :=
{$IFNDEF INDY10_2}
              FClient.ReceiveBuffer(TIdBytes(FClient.FReadBuffer), 10);
{$ELSE}
              FClient.ReceiveBuffer(TIdBytes(FClient.FReadBuffer), vPeerIP,
              vPeerPort, 10);
{$ENDIF}
            if FClient.FReadBufferSize > 0 then
            begin
              if Assigned(FOnReadMessage) then
                FOnReadMessage(self, vPeerIP, vPeerPort);
            end;
          Finally
            FClient.LeaveCS;
          End;
        end;
      end;
    end
    else
      sleep(0);
  Except
    On E: Exception do
      DoOnExceptionEvent(E);
  End;
end;

procedure TsgcUDPReadThread.Terminate;
begin
  case UDPMode of
    udpmCommands:
      inherited
  else
    begin
      // ... only call terminate if thread has been executed
      if FExecuted then
        inherited;
    end;
  end;
end;

destructor TsgcIdUDPClient.Destroy;
begin
  sgcThreadFree(FUDPRead);
  sgcFree(FCS);
  inherited;
end;

procedure TsgcIdUDPClient.DoEnterCS;
begin
  FCS.Acquire;
end;

procedure TsgcIdUDPClient.DoLeaveCS;
begin
  FCS.Release;
end;

procedure TsgcIdUDPClient.EnterCS;
begin
  DoEnterCS;
end;

function TsgcIdUDPClient.GetUDPRead: TsgcUDPReadThread;
begin
  if not Assigned(FUDPRead) then
  begin
    FUDPRead := TsgcUDPReadThread.Create(self);
    FUDPRead.OnReadMessage := OnReadEvent;
    FUDPRead.OnReadException := OnReadExceptionEvent;
    FUDPRead.OnReadThread := OnReadThreadEvent;
    FUDPRead.UDPMode := UDPMode;
  end;
  Result := FUDPRead;
end;

procedure TsgcIdUDPClient.InitComponent;
begin
  FCS := TsgcCriticalSection.Create;
  inherited;
end;

procedure TsgcIdUDPClient.LeaveCS;
begin
  DoLeaveCS;
end;

procedure TsgcIdUDPClient.OnReadEvent(Sender: TObject; const aPeerIP: string;
  aPeerPort: Word);
var
  oBytes: TBytes;
begin
  if Assigned(FOnRead) then
  begin
    SetLength(oBytes, FReadBufferSize);
    sgcMove(FReadBuffer[0], oBytes[0], FReadBufferSize);
    FOnRead(self, oBytes, aPeerIP, aPeerPort);
  end;
end;

procedure TsgcIdUDPClient.OnReadExceptionEvent(aException: Exception);
begin
  if Assigned(FOnReadException) then
    FOnReadException(aException);
end;

procedure TsgcIdUDPClient.OnReadThreadEvent(Sender: TObject);
begin
  if Assigned(FOnThread) then
    FOnThread(self);
end;

constructor TsgcUDPCLient_Base.Create(aOwner: TComponent);
begin
  inherited;
{$IFDEF CONSOLE}
  NotifyEvents := neNoSync;
{$ELSE}
{$IFDEF MSWINDOWS}
  if IsLibrary then
    NotifyEvents := neNoSync
  else
    NotifyEvents := neAsynchronous;
{$ELSE}
  NotifyEvents := neAsynchronous;
{$ENDIF}
{$ENDIF}
  FProxy := TsgcUDPProxy_Options.Create;
  FProxy.ProxyType := pxySocks5;
  Host := CS_DEFAULT_HOST;
  Port := CS_DEFAULT_PORT;
  TLS := False;
  FTLSOptions := TsgcUDPTLS_Options.Create;
  FUDPMode := udpmDefault;
  FHeartBeat := TsgcUDPHeartBeat_Options.Create;
  FLogFile := TsgcUDPLogFile.Create;
  FRetransmissionOptions := TsgcUDPRetransmissionClient_Options.Create;
end;

destructor TsgcUDPCLient_Base.Destroy;
begin
  DoStopReadThread;
  sgcFree(FRetransmissions);
  sgcFree(FRetransmissionOptions);
  sgcFree(FHeartBeat);
  sgcFree(FUDPClient);
  sgcFree(FUDPSocket);
  sgcFree(FCommands);
  sgcFree(FProxy);
  sgcFree(FLogFile);
  sgcFree(FProxySocks);
  sgcFree(FIdLogFile);
  inherited;
end;

procedure TsgcUDPCLient_Base.AddBytes(const ABytes: TIdBytes);
begin
  Commands.AddBytes(ABytes);
end;

procedure TsgcUDPCLient_Base.AddCommand(const aCommand: TsgcUDPCommands);
begin
  case aCommand of
    tcmNone:
      ;
    tcmActive:
      ;
    tcmNotActive:
      ;
    tcmWriteBytes:
      ;
  end;
  Commands.AddCommand(aCommand)
end;

procedure TsgcUDPCLient_Base.DoCommands;
var
  oCommand: TsgcQueueCommand;
begin
  if Assigned(FCommands) then
  begin
    while Commands.Count > 0 do
    begin
      oCommand := TsgcQueueCommand(Commands.Item[0]);
      Try
        Try
          case oCommand.Command of
            tcmNone:
              ;
            tcmActive:
              ;
            tcmNotActive:
              ;
            tcmWriteBytes:
              begin
                if Assigned(UDPClient) then
                begin
                  if Assigned(UDPClient) then
                  begin
                    UDPClient.SendBuffer(TIdBytes(oCommand.Bytes));
                    OnCommandExecute(oCommand.Command);
                  end;
                end;
              end;
          end;
        Except
          On E: Exception do
            DoException(FUDPSocket, E);
        End;
      Finally
        Commands.DeleteItem(0);
      End;
    end;
  end;
end;

procedure TsgcUDPCLient_Base.DoReadData(const ABytes: TBytes);
begin
  // copy bytes
  SetLength(FUDPSocket.FMsgRead, Length(ABytes));
  sgcMove(ABytes[0], FUDPSocket.FMsgRead[0], Length(ABytes));
  // notify read
  DoNotifyUDPRead(UDPSocket);
end;

procedure TsgcUDPCLient_Base.DoStartHeartBeat;
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

procedure TsgcUDPCLient_Base.DoStopHeartBeat;
begin
  if Assigned(FHeartBeatTimer) then
    FHeartBeatTimer.Enabled := False;
end;

procedure TsgcUDPCLient_Base.DoStopReadThread;
begin
  if IsDestroying then
    exit;

  case UDPMode of
    udpmCommands:
      begin
        if Assigned(FUDPClient) then
          if Assigned(FUDPClient.FUDPRead) then
            FUDPClient.UDPRead.ReadSocket := False;
      end;
  else
    begin
      if Assigned(FUDPClient) then
      begin
        if Assigned(FUDPClient.FUDPRead) then
        begin
          if not UDPClient.UDPRead.Terminated then
          begin
            UDPClient.UDPRead.Terminate;
            if not FReadingThread then
            begin
              if not(TLS and (TLSOptions.IOHandler = iohSChannel)) then
                UDPClient.UDPRead.WaitFor;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TsgcUDPCLient_Base.DoWriteData(const aValue: string);
begin
  UDPSocket.DoWriteData(aValue);
end;

procedure TsgcUDPCLient_Base.DoWriteData(const aValue: TBytes);
begin
  UDPSocket.DoWriteData(aValue);
end;

procedure TsgcUDPCLient_Base.DoWriteData(const aIPAddress: string; aPort: Word;
  const aValue: TBytes);
begin
  UDPSocket.DoWriteData(aIPAddress, aPort, aValue);
end;

procedure TsgcUDPCLient_Base.DoWriteData(const aIPAddress: string; aPort: Word;
  const aValue: string);
begin
  UDPSocket.DoWriteData(aIPAddress, aPort, aValue);
end;

function TsgcUDPCLient_Base.GetCommands: TsgcQueueCommands;
begin
  if not Assigned(FCommands) then
  begin
    FCommands := TsgcQueueCommands.Create;
    FCommands.OwnObjects := True;
  end;
  Result := FCommands;
end;

function TsgcUDPCLient_Base.GetHost: String;
begin
  Result := UDPClient.Host;
end;

function TsgcUDPCLient_Base.GetIdLogFile: TsgcIdLogFileClient;
begin
  if not Assigned(FIdLogFile) then
    FIdLogFile := TsgcIdLogFileClient.Create(nil);
  if FIdLogFile.Filename <> LogFile.Filename then
    FIdLogFile.Filename := LogFile.Filename;
  Result := FIdLogFile;
end;

function TsgcUDPCLient_Base.GetUDPSocket: TsgcUDPSocketClient;
begin
  if not Assigned(FUDPSocket) then
  begin
    FUDPSocket := TsgcUDPSocketClient.Create;
    FUDPSocket.Transport := trpUDP;
    FUDPSocket.OnException := OnSocketExceptionEvent;
  end;
  FUDPSocket.SetClient(UDPClient);
  FUDPSocket.FGuid := EncodeBase64(Host + ':' + IntToStr(Port));
  if LogFile.Enabled and (LogFile.Filename <> '') then
    FUDPSocket.FLogFile := TsgcIdUDPLogFile(GetIdLogFile)
  else
    FUDPSocket.FLogFile := nil;
  Result := FUDPSocket;
end;

function TsgcUDPCLient_Base.GetIPVersion: TIdIPVersion;
begin
  Result := UDPClient.IPVersion;;
end;

function TsgcUDPCLient_Base.GetPort: Integer;
begin
  Result := UDPClient.Port;
end;

function TsgcUDPCLient_Base.GetProxySocks: TIdSocksInfo;
begin
  if not Assigned(FProxySocks) then
    FProxySocks := TIdSocksInfo.Create(self);
  FProxySocks.Host := Proxy.Host;
  FProxySocks.Port := Proxy.Port;
  if Proxy.Username <> '' then
    FProxySocks.Authentication := saUsernamePassword
  else
    FProxySocks.Authentication := saNoAuthentication;
  FProxySocks.Username := Proxy.Username;
  FProxySocks.Password := Proxy.Password;
  case Proxy.ProxyType of
    pxySocks4:
      FProxySocks.Version := svSocks4;
    pxySocks4A:
      FProxySocks.Version := svSocks4A;
    pxySocks5:
      FProxySocks.Version := svSocks5;
  end;
  Result := FProxySocks;
end;

function TsgcUDPCLient_Base.GetRetransmissions: TsgcUDPRetransmissions;
begin
  if not Assigned(FRetransmissions) then
  begin
    FRetransmissions := TsgcUDPRetransmissions.Create;
    FRetransmissions.OnRetry := OnRetransmissionRetryEvent;
    FRetransmissions.OnTimeout := OnRetransmissionTimeoutEvent;
    FRetransmissions.OnException := OnRetransmissionExceptionEvent;
    FRetransmissions.MaxRetries := RetransmissionOptions.MaxRetries;
    FRetransmissions.RTO := RetransmissionOptions.RTO;
    FRetransmissions.Enabled := RetransmissionOptions.Enabled;
  end;
  Result := FRetransmissions;
end;

function TsgcUDPCLient_Base.GetUDPClient: TsgcIdUDPClient;
begin
  if not Assigned(FUDPClient) then
  begin
    FUDPClient := TsgcIdUDPClient.Create(nil);

    FUDPClient.OnConnected := OnClientConnectEvent;
    FUDPClient.OnDisconnected := OnClientDisconnectEvent;

    FUDPClient.OnThread := OnThreadEvent;
    FUDPClient.OnRead := OnReadEvent;
    FUDPClient.OnReadException := OnReadExceptionEvent;
    FUDPClient.UDPRead.ReadSocket := True;

    FUDPClient.UDPMode := UDPMode;
  end;
  if Proxy.Enabled then
    FUDPClient.TransparentProxy := GetProxySocks
  else
    FUDPClient.TransparentProxy := nil;

  Result := FUDPClient;
end;

procedure TsgcUDPCLient_Base.OnClientConnectEvent(Sender: TObject);
begin
  if IsDestroying then
    exit;

  Try
    DoNotifyUDPConnect(UDPSocket);
  Except
    On E: Exception do
      DoException(UDPSocket, E);
  End;
end;

procedure TsgcUDPCLient_Base.OnClientDisconnectEvent(Sender: TObject);
begin
  if IsDestroying then
    exit;

  Try
    DoNotifyUDPDisconnect(UDPSocket);
  Except
    On E: Exception do
      DoException(UDPSocket, E);
  end;
end;

procedure TsgcUDPCLient_Base.OnCommandExecute(const aCommand: TsgcUDPCommands);
begin

end;

procedure TsgcUDPCLient_Base.OnHeartBeatEvent(Sender: TObject);
begin

end;

procedure TsgcUDPCLient_Base.OnHeartBeatExceptionEvent(Sender: TObject;
  E: Exception);
begin

end;

procedure TsgcUDPCLient_Base.OnReadEvent(Sender: TObject; const ABytes: TBytes;
  const aPeerIP: string; aPeerPort: Word);
begin
  if IsDestroying then
    exit;

  Try
    if Assigned(UDPSocket) then
    begin
      UDPSocket.IP := aPeerIP;
      UDPSocket.Port := aPeerPort;

      UDPSocket.LogData(ABytes);
      if not UDPSocket.Disconnected then
      begin
        FReadingThread := True;
        Try
          DoReadData(ABytes);
        Finally
          FReadingThread := False;
        End;
      end;
    end;
  Except
    On E: Exception do
      DoException(UDPSocket, E);
  end;
end;

procedure TsgcUDPCLient_Base.OnReadExceptionEvent(aException: Exception);
begin
  if IsDestroying then
    exit;

  DoException(UDPSocket, aException);
end;

procedure TsgcUDPCLient_Base.OnRetransmissionExceptionEvent(Sender: TObject;
  E: Exception);
begin
  DoException(UDPSocket, E);
end;

procedure TsgcUDPCLient_Base.OnRetransmissionRetryEvent(Sender: TObject;
  const ABytes: TBytes; const aIPAddress: string; aPort: Word);
begin
  if aIPAddress <> '' then
    UDPSocket.WriteData(aIPAddress, aPort, ABytes)
  else
    UDPSocket.WriteData(ABytes);
end;

procedure TsgcUDPCLient_Base.OnRetransmissionTimeoutEvent(Sender: TObject;
  const ABytes: TBytes; const aIPAddress: string; aPort: Word);
begin
  if Assigned(FOnRetransmissionTimeout) then
    FOnRetransmissionTimeout(self, ABytes, aIPAddress, aPort);
end;

procedure TsgcUDPCLient_Base.OnSocketExceptionEvent(Sender: TObject;
  const E: Exception);
begin
  DoException(TsgcUDPSocket(Sender), E);
end;

procedure TsgcUDPCLient_Base.OnThreadEvent(Sender: TObject);
begin
  case UDPMode of
    udpmCommands:
      DoCommands;
  end;
end;

procedure TsgcUDPCLient_Base.SetHeartBeat(const Value
  : TsgcUDPHeartBeat_Options);
begin
  if Assigned(FHeartBeat) then
    FHeartBeat.Assign(Value);
end;

procedure TsgcUDPCLient_Base.SetHost(const Value: String);
begin
  UDPClient.Host := Value;
end;

procedure TsgcUDPCLient_Base.SetIPVersion(const Value: TIdIPVersion);
begin
  UDPClient.IPVersion := Value;
end;

procedure TsgcUDPCLient_Base.SetLogFile(const Value: TsgcUDPLogFile);
begin
  if Assigned(FLogFile) then
    FLogFile.Assign(Value);
end;

procedure TsgcUDPCLient_Base.SetProxy(const Value: TsgcUDPProxy_Options);
begin
  if Assigned(FProxy) then
    FProxy.Assign(Value);
end;

procedure TsgcUDPCLient_Base.SetRetransmissionOptions
  (const Value: TsgcUDPRetransmissionClient_Options);
begin
  if Assigned(FRetransmissionOptions) then
    FRetransmissionOptions.Assign(Value);
end;

procedure TsgcUDPCLient_Base.SetTLSOptions(const Value: TsgcUDPTLS_Options);
begin
  FTLSOptions.Assign(Value);
end;

procedure TsgcUDPCLient_Base.WriteData(const aValue: string);
begin
  DoWriteData(aValue);
end;

procedure TsgcUDPCLient_Base.WriteData(const aValue: TBytes);
begin
  DoWriteData(aValue);
end;

procedure TsgcUDPCLient_Base.WriteData(const aIPAddress: string; aPort: Word;
  const aValue: string);
begin
  DoWriteData(aIPAddress, aPort, aValue);
end;

procedure TsgcUDPCLient_Base.WriteData(const aIPAddress: string; aPort: Word;
  const aValue: TBytes);
begin
  DoWriteData(aIPAddress, aPort, aValue);
end;

procedure TsgcUDPCLient_Base._SetPort(const Value: Integer);
begin
  UDPClient.Port := Value;
end;

constructor TsgcQueueCommand.Create;
begin
  inherited;
  Command := tcmNone;
end;

destructor TsgcQueueCommand.Destroy;
begin
  inherited;
end;

procedure TsgcQueueCommand.SetBytes(const Value: TIdBytes);
begin
  SetLength(FBytes, Length(Value));
  sgcMove(Value[0], FBytes[0], Length(FBytes));
end;

procedure TsgcQueueCommands.AddBytes(const ABytes: TIdBytes);
var
  oCommand: TsgcQueueCommand;
begin
  oCommand := TsgcQueueCommand.Create;
  oCommand.ID := NewGuid;
  oCommand.Command := tcmWriteBytes;
  oCommand.FBytes := ABytes;

  AddItem(oCommand);
end;

procedure TsgcQueueCommands.AddCommand(const aCommand: TsgcUDPCommands);
var
  oCommand: TsgcQueueCommand;
begin
  oCommand := TsgcQueueCommand.Create;
  oCommand.ID := NewGuid;
  oCommand.Command := aCommand;

  AddItem(oCommand);
end;

procedure TsgcUDPSocketClient.Assign(const aSource: TsgcUDPSocket);
begin
  inherited;
  FClient := TsgcUDPSocketClient(aSource).FClient;
end;

procedure TsgcUDPSocketClient.DoWriteData(const aValue: string);
begin
  inherited;
  if Assigned(FClient) then
  begin
    DoEnterCS;
    Try
      Try
        FClient.Send(aValue);
      Except
        On E: Exception do
          DoException(E);
      End;
    Finally
      DoLeaveCS;
    End;
  end;
end;

procedure TsgcUDPSocketClient.DoWriteData(const aValue: TBytes);
begin
  inherited;
  if Assigned(FClient) then
  begin
    DoEnterCS;
    Try
      Try
        FClient.SendBuffer(TIdBytes(aValue));
      Except
        On E: Exception do
          DoException(E);
      End;
    Finally
      DoLeaveCS;
    End;
  end;
end;

procedure TsgcUDPSocketClient.DoWriteData(const aIPAddress: string; aPort: Word;
  const aValue: string);
begin
  DoWriteData(aIPAddress, aPort, sgcGetBytesFromUTF8String(aValue));
end;

procedure TsgcUDPSocketClient.DoWriteData(const aIPAddress: string; aPort: Word;
  const aValue: TBytes);
begin
  if Assigned(FClient) then
  begin
    DoEnterCS;
    Try
      Try
        if sgcContainsText(aIPAddress, ':') then
          FClient.SendBuffer(aIPAddress, aPort, Id_IpV6, TIdBytes(aValue))
        else
          FClient.SendBuffer(aIPAddress, aPort, TIdBytes(aValue));
      Except
        On E: Exception do
          DoException(E);
      End;
    Finally
      DoLeaveCS;
    End;
  end;

end;

procedure TsgcUDPSocketClient.SetClient(const aValue: TsgcIdUDPClient);
begin
  FClient := aValue;
  Socket := FClient.Binding;
end;

{$ENDIF}

end.
