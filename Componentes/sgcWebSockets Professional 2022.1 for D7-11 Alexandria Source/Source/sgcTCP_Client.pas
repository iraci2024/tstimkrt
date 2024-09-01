{ ***************************************************************************
  sgcTCP component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcTCP_Client;

interface

{$I sgcVer.inc}

uses
  Classes, SysUtils,
{$IFDEF MSWINDOWS} Windows {$ELSE} SyncObjs {$ENDIF},
{$IFDEF NEXTGEN} System.Generics.Collections, {$ELSE} Contnrs, {$ENDIF}
  // indy
{$IFDEF SGC_INDY} sgcIdTCPConnection {$ELSE} IdTCPConnection {$ENDIF},
{$IFDEF SGC_INDY} sgcIdGlobal {$ELSE} IdGlobal {$ENDIF},
{$IFDEF SGC_INDY} sgcIdTCPClient {$ELSE} IdTCPClient {$ENDIF},
{$IFDEF SGC_INDY} sgcIdThread {$ELSE} IdThread {$ENDIF},
{$IFDEF SGC_INDY} sgcIdException {$ELSE} IdException {$ENDIF},
{$IFDEF SGC_INDY} sgcIdIntercept {$ELSE} IdIntercept {$ENDIF},
{$IFDEF SGC_INDY} sgcIdIOHandler {$ELSE} IdIOHandler {$ENDIF},
{$IFDEF SGC_INDY} sgcIdIOHandlerSocket {$ELSE} IdIOHandlerSocket {$ENDIF},
{$IFDEF SGC_INDY} sgcIdIOHandlerStack {$ELSE} IdIOHandlerStack {$ENDIF},
{$IFDEF SGC_INDY} sgcIdConnectThroughHttpProxy
{$ELSE} IdConnectThroughHttpProxy {$ENDIF},
{$IFDEF SGC_INDY} sgcIdSocks {$ELSE} IdSocks {$ENDIF},
{$IFDEF SGC_INDY} sgcIdSSL {$ELSE} IdSSL {$ENDIF},
{$IFDEF SGC_INDY} sgcIdSSLOpenSSL {$ELSE} IdSSLOpenSSL {$ENDIF},
{$IFDEF SGC_INDY} sgcIdSSLOpenSSLHeaders {$ELSE} IdSSLOpenSSLHeaders {$ENDIF},
{$IFDEF SGC_INDY} sgcIdComponent {$ELSE} IdComponent {$ENDIF},
{$IFNDEF LAZARUS}
{$IFDEF SGC_INDY} sgcIdInterceptThrottler {$ELSE} IdInterceptThrottler {$ENDIF},
{$ENDIF}
  // sgcWebSocket
  sgcBase_Classes, sgcBase_Helpers, sgcSocket_Classes_Indy,
  sgcTCP_Classes, sgcTCP_Classes_Indy, sgcWebSocket_Classes_SyncObjs,
  sgcWebSocket_Classes_Queues, sgcWebSocket_Types, sgcWebSocket_Helpers;

type

  TsgcTCPOnSSLGetHandler = procedure(Sender: TObject; aType: TwsSSLHandler;
    var aSSLHandler: TIdSSLIOHandlerSocketBase) of object;
  TsgcTCPOnSSLAfterCreateHandler = procedure(Sender: TObject;
    aType: TwsSSLHandler; aSSLHandler: TIdSSLIOHandlerSocketBase) of object;

  TsgcTCPReadThread = class;
  TsgcIdTCPClient = class;

  TsgcReadThreadExceptionEvent = procedure(aException: Exception) of object;

  TsgcTCPCommands = (tcmNone, tcmActive, tcmNotActive, tcmWriteBytes);
  TsgcTCPMode = (tcpmDefault, tcpmCommands);

  TsgcQueueCommand = class(TsgcQueueItemBase)
  private
    FBytes: TIdBytes;
    FCommand: TsgcTCPCommands;
    procedure SetBytes(const Value: TIdBytes);
  public
    constructor Create; override;
    destructor Destroy; override;
  public
    property Bytes: TIdBytes read FBytes write SetBytes;
    property Command: TsgcTCPCommands read FCommand write FCommand;
  end;

  TsgcQueueCommands = class(TsgcQueue)
  public
    procedure AddCommand(const aCommand: TsgcTCPCommands);
    procedure AddBytes(const ABytes: TIdBytes);
  end;

  TsgcTCPReadThread = class(TsgcIdThread)
  private
    FReadSocket: Boolean;
  private
    FClient: TsgcIdTCPClient;
  private
    FOnReadMessage: TNotifyEvent;
    FOnReadThread: TNotifyEvent;
    FOnReadException: TsgcReadThreadExceptionEvent;
  protected
    procedure DoOnMessageEvent; virtual;
    procedure DoOnExceptionEvent(const E: Exception); virtual;
  private
    FExecuted: Boolean;
    FTCPMode: TsgcTCPMode;
  protected
    procedure Run; override;
  public
    procedure Terminate; override;
  public
    constructor Create(aClient: TsgcIdTCPClient); reintroduce;
    destructor Destroy; override;
  public
    property OnReadThread: TNotifyEvent read FOnReadThread write FOnReadThread;
    property OnReadMessage: TNotifyEvent read FOnReadMessage
      write FOnReadMessage;
    property OnReadException: TsgcReadThreadExceptionEvent read FOnReadException
      write FOnReadException;
  public
    property ReadSocket: Boolean read FReadSocket write FReadSocket;
    property TCPMode: TsgcTCPMode read FTCPMode write FTCPMode;
  End;

  TsgcIdTCPClient = class(TIdTCPClient)
  private
    FOnThread: TNotifyEvent;
    FOnRead: TNotifyEvent;
    FOnReadException: TsgcReadThreadExceptionEvent;
    FTCPRead: TsgcTCPReadThread;
    FTCPMode: TsgcTCPMode;
  protected
    procedure OnReadThreadEvent(Sender: TObject); virtual;
    procedure OnReadEvent(Sender: TObject); virtual;
    procedure OnReadExceptionEvent(aException: Exception); virtual;
  public
    function Connected: Boolean; override;
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
    function GetTCPRead: TsgcTCPReadThread;
  public
    property TCPRead: TsgcTCPReadThread read GetTCPRead;
  public
    destructor Destroy; override;
  public
    procedure Connect; override;
    procedure Disconnect(ANotifyPeer: Boolean); override;
  public
    property TCPMode: TsgcTCPMode read FTCPMode write FTCPMode;
  published
    property OnThread: TNotifyEvent read FOnThread write FOnThread;
    property OnRead: TNotifyEvent read FOnRead write FOnRead;
    property OnReadException: TsgcReadThreadExceptionEvent read FOnReadException
      write FOnReadException;
  end;

type
  TsgcIdSSLIOHandlerSocketOpenSSL = class(TIdSSLIOHandlerSocketOpenSSL)
  private
    FCS: TsgcCriticalSection;
  protected
    procedure DoEnterCS; virtual;
    procedure DoLeaveCS; virtual;
  protected
    procedure InitComponent; override;
  protected
    function RecvEnc(var VBuffer: TIdBytes): Integer; override;
    function SendEnc(const ABuffer: TIdBytes
{$IFDEF INDY10_2}; const AOffset, ALength: Integer
{$ENDIF}): Integer; override;
{$IFDEF INDY10_5_7}
    function CheckForError(ALastResult: Integer): Integer; override;
  public
    destructor Destroy; override;
    function Readable(AMSec: Integer = IdTimeoutDefault): Boolean; override;
{$ENDIF}
  end;

type
  TsgcTCPTLSClient_Options = class(TsgcTCPTLS_Options)

  end;

type

  TsgcTCPConnectionClient = class(TsgcTCPConnection_Indy)
    { read data }
  protected
    procedure DoReadData_TCP; virtual;
  protected
    procedure ReadData; virtual;
    { read data }

    { alpn }
  private
    function GetALPNProtocol: string;
  public
    property ALPNProtocol: string read GetALPNProtocol;
    { alpn }

    { initialize }
  protected
    procedure DoInitialize(aConnection: TIdTCPConnection); virtual;
    { initialize }

    { events }
  private
    FOnBinary: TsgcTCPBinaryEvent;
  protected
    procedure DoBinaryEvent(const aData: TMemoryStream); virtual;
  public
    property OnBinary: TsgcTCPBinaryEvent read FOnBinary write FOnBinary;
    { events }
  end;

  TsgcTCPCLient_Common = class(TsgcTCPComponent)
    { from TComponent }
  protected
    procedure Loaded; override;
    { from TComponent }

    { from TsgcWSComponent }
  protected
    procedure DoNotifyDisconnect(const aConnection: TsgcTCPConnection);
      override;
    { from TsgcWSComponent

      { tcp client }
  private
    FTCPClient: TsgcIdTCPClient;
    function GetTCPClient: TsgcIdTCPClient;
  protected
    procedure DoStopReadThread; virtual;
  protected
    property TCPClient: TsgcIdTCPClient read GetTCPClient write FTCPClient;
    { tcp client }

    { keepalive }
  private
    FTCPKeepAlive: TsgcTCPKeepAlive;
    procedure SetTCPKeepAlive(const Value: TsgcTCPKeepAlive);
  public
    property TCPKeepAlive: TsgcTCPKeepAlive read FTCPKeepAlive
      write SetTCPKeepAlive;
    { keepalive }

    { properties }
  private
    FReadingThread: Boolean;
    { properties }

    { from thread }
  protected
    procedure OnReadEvent(Sender: TObject); virtual;
    procedure OnReadExceptionEvent(aException: Exception); virtual;
    procedure OnThreadEvent(Sender: TObject); virtual;
    { from thread }

    { heartbeat }
  private
    FHeartBeatTimer: TsgcTimer;
    FHeartBeat: TsgcTCPHeartBeat_Options;
    procedure SetHeartBeat(const Value: TsgcTCPHeartBeat_Options);
  protected
    procedure OnHeartBeatEvent(Sender: TObject); virtual; abstract;
    procedure OnHeartBeatExceptionEvent(Sender: TObject; E: Exception);
      virtual; abstract;
    procedure DoStartHeartBeat; virtual;
    procedure DoStopHeartBeat; virtual;
  protected
    property HeartBeat: TsgcTCPHeartBeat_Options read FHeartBeat
      write SetHeartBeat;
    { heartbeat }

    { commands }
  private
    FCommands: TsgcQueueCommands;
    function GetCommands: TsgcQueueCommands;
  protected
    property Commands: TsgcQueueCommands read GetCommands write FCommands;
  protected
    procedure OnCommandExecute(const aCommand: TsgcTCPCommands);
      virtual; abstract;
    procedure DoCommands; virtual;
  public
    procedure AddCommand(const aCommand: TsgcTCPCommands);
    procedure AddBytes(const ABytes: TIdBytes);
    { commands }

    { connection }
  protected
    FTCPConnection: TsgcTCPConnectionClient;
    FDisconnecting: Boolean;
    function GetConnection: TsgcTCPConnection;
  public
    property Connection: TsgcTCPConnection read GetConnection;
    { connection }

    { events }
  protected
    procedure DoCreateTCPConnection; virtual;
  protected
    procedure OnConnectionBeforeDisconnect(Sender: TObject); virtual;
  protected
    procedure OnClientConnectEvent(Sender: TObject); virtual;
    procedure OnClientDisconnectEvent(Sender: TObject); virtual;
    procedure OnClientStatusEvent(ASender: TObject; const AStatus: TIdStatus;
      const AStatusText: string); virtual;
  protected
    procedure OnClientBinaryEvent(aConnection: TsgcTCPConnection;
      const aStream: TMemoryStream); virtual;
    { events }

    { LogFile }
  private
    FLogFile: TsgcTCPLogFile;
  protected
    function GetLogFile: TsgcTCPLogFile; virtual;
    procedure SetLogFile(const Value: TsgcTCPLogFile); virtual;
  public
    property LogFile: TsgcTCPLogFile read GetLogFile write SetLogFile;
    { LogFile }

    { Throttle }
  private
    FThrottle: TsgcTCPThrottle;
  protected
    function GetThrottle: TsgcTCPThrottle; virtual;
    procedure SetThrottle(const Value: TsgcTCPThrottle); virtual;
  public
    property Throttle: TsgcTCPThrottle read GetThrottle write SetThrottle;
    { Throttle }
{$IFNDEF LAZARUS}
    { Throttle }
  private
    FInterceptThrottle: TIdInterceptThrottler;
    function GetInterceptThrottle: TIdInterceptThrottler;
  protected
    property InterceptThrottle: TIdInterceptThrottler read GetInterceptThrottle
      write FInterceptThrottle;
    { Throttle }
{$ENDIF}
    { intercepts }
  protected
    function GetConnectionIntercept: TIdConnectionIntercept;
    { intercepts }

    { helpers }
  protected
    procedure DoStopThreads; virtual;
    { helpers }

    { LogFile }
  private
    FInterceptLogFile: TsgcIdLogFileClient;
    function GetInterceptLogFile: TsgcIdLogFileClient;
  protected
    property InterceptLogFile: TsgcIdLogFileClient read GetInterceptLogFile
      write FInterceptLogFile;
    { LogFile }

    { proxy }
  private
    FProxy: TsgcTCPProxy_Options;
    FProxyIOHandler: TIdIOHandlerStack;
    FProxyHTTP: TIdConnectThroughHttpProxy;
    FProxySocks: TIdSocksInfo;
  private
    function GetProxyIOHandler: TIdIOHandlerStack;
    function GetProxyHTTP: TIdConnectThroughHttpProxy;
    function GetProxySocks: TIdSocksInfo;
  protected
    procedure SetProxy(const Value: TsgcTCPProxy_Options); virtual;
  public
    property Proxy: TsgcTCPProxy_Options read FProxy write SetProxy;
    { proxy }

    { ssl }
  private
    FHandlerSSL: TIdSSLIOHandlerSocketBase;
  protected
    FOnSSLGetHandler: TsgcTCPOnSSLGetHandler;
    FOnSSLAfterCreateHandler: TsgcTCPOnSSLAfterCreateHandler;
  protected
    property HandlerSSL: TIdSSLIOHandlerSocketBase read FHandlerSSL;
  protected
    procedure GetHandlerSSLEvent(const aSSLType: TwsSSLHandler;
      var aSSLHandler: TIdSSLIOHandlerSocketBase); virtual;
    procedure DoSSL; virtual;
  protected
    procedure OnGetPasswordEvent(var Password:
{$IFDEF INDY10_6}String{$ELSE} AnsiString {$ENDIF}); virtual;
  public
    property OnSSLGetHandler: TsgcTCPOnSSLGetHandler read FOnSSLGetHandler
      write FOnSSLGetHandler;
    property OnSSLAfterCreateHandler: TsgcTCPOnSSLAfterCreateHandler
      read FOnSSLAfterCreateHandler write FOnSSLAfterCreateHandler;
    { ssl }

    { WatchDog }
  private
    FWatchDog: TsgcTCPWatchDog_Options;
  protected
    FWatchDogAttempts: Integer;
    FWatchDogTimer: TsgcTimer;
    FWatchDogEnabled: Boolean;
  protected
    procedure DoStartWatchDog; virtual;
    procedure DoStopWatchDog; virtual;
  protected
    procedure OnWatchDogEvent(Sender: TObject); virtual;
    procedure OnWatchDogExceptionEvent(Sender: TObject; E: Exception);
      virtual; abstract;
  protected
    procedure SetWatchDog(const Value: TsgcTCPWatchDog_Options); virtual;
  public
    property WatchDog: TsgcTCPWatchDog_Options read FWatchDog write SetWatchDog;
    { WatchDog }

    { constructor / destructor }
  private
    procedure DoClear;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    { constructor / destructor }

    { host }
  private
    FHost: String;
    FPort: Integer;
    FTLS: Boolean;
    FTLSOptions: TsgcTCPTLS_Options;
  protected
    procedure SetTLSOptions(const Value: TsgcTCPTLS_Options); virtual;
  public
    property Host: String read FHost write FHost;
    property Port: Integer read FPort write FPort;
    property TLS: Boolean read FTLS write FTLS;
    property TLSOptions: TsgcTCPTLS_Options read FTLSOptions
      write SetTLSOptions;
    { host }

    { properties }
  private
    FUseNagle: Boolean;
    FIPVersion: TIdIPVersion;
    FConnectTimeout: Integer;
    FReadTimeout: Integer;
    FTCPMode: TsgcTCPMode;
    FWriteTimeout: Integer;
    FBoundPortMax: TIdPort;
    FBoundPortMin: TIdPort;
    FLingerState: Integer;
  public
    property UseNagle: Boolean read FUseNagle write FUseNagle stored True;
    property IPVersion: TIdIPVersion read FIPVersion write FIPVersion;
    property ConnectTimeout: Integer read FConnectTimeout write FConnectTimeout;
    property WriteTimeout: Integer read FWriteTimeout write FWriteTimeout;
    property ReadTimeout: Integer read FReadTimeout write FReadTimeout;
    property TCPMode: TsgcTCPMode read FTCPMode write FTCPMode;
    property BoundPortMax: TIdPort read FBoundPortMax write FBoundPortMax;
    property BoundPortMin: TIdPort read FBoundPortMin write FBoundPortMin;
    property LingerState: Integer read FLingerState write FLingerState;
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

    { connect/disconnect and wait }
  private
    FConnecting: Boolean;
  public
    function Connect(const aTimeout: Integer = 10000): Boolean;
    function Disconnect(const aTimeout: Integer = 10000): Boolean;
    { connect/disconnect and wait }
  end;

  TsgcTCPCLient_Base = class(TsgcTCPCLient_Common)
    { from TsgcTCPCLient_Common }
  protected
    procedure OnHeartBeatEvent(Sender: TObject); override;
    procedure OnHeartBeatExceptionEvent(Sender: TObject; E: Exception);
      override;
  protected
    procedure OnCommandExecute(const aCommand: TsgcTCPCommands); override;
  protected
    procedure OnWatchDogExceptionEvent(Sender: TObject; E: Exception); override;
    { from TsgcTCPCLient_Common }

    { write data }
  protected
    procedure DoWriteData(const aValue: TBytes); overload; virtual;
    procedure DoWriteData(const aValue: string); overload; virtual;
  public
    procedure WriteData(const aValue: TBytes); overload;
    procedure WriteData(const aValue: string); overload;
    { write data }
  end;

  TsgcTCPCLient = class(TsgcTCPCLient_Base)
  public
    property NotifyEvents;
  public
    property OnConnect;
    property OnDisconnect;
    property OnMessage;
    property OnBinary;
    property OnException;
  end;

implementation

uses
{$IFDEF SGC_INDY} sgcIdStackConsts, {$ELSE} IdStackConsts, {$ENDIF}
{$IFDEF SGC_SCHANNEL} sgcSSL_SChannel_Indy, {$ENDIF}
  sgcSocket_Const;

constructor TsgcTCPReadThread.Create(aClient: TsgcIdTCPClient);
begin
{$IFDEF SGC_DEBUG}
  DoLog(self, nil, 'TCPRead_Create_Start');
{$ENDIF}
  FClient := aClient;
  FExecuted := False;
  FReadSocket := False;
  inherited Create(False);
{$IFDEF SGC_DEBUG}
  DoLog(self, nil, 'TCPRead_Create_End');
{$ENDIF}
end;

destructor TsgcTCPReadThread.Destroy;
begin
{$IFDEF SGC_DEBUG}
  DoLog(self, nil, 'TCPRead_Destroy_Start');
{$ENDIF}
  inherited;
{$IFDEF SGC_DEBUG}
  DoLog(self, nil, 'TCPRead_Destroy_End');
{$ENDIF}
end;

procedure TsgcTCPReadThread.DoOnExceptionEvent(const E: Exception);
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

procedure TsgcTCPReadThread.DoOnMessageEvent;
begin
  if Assigned(FOnReadMessage) then
  begin
    FIsExecuting := True;
    Try
      FOnReadMessage(self);
    Finally
      FIsExecuting := False;
    End;
  end;
end;

procedure TsgcTCPReadThread.Run;
begin
  inherited;
  // -->start sgc_trial
{$IFDEF SGC_TRIAL}
  if (Now > EncodeDate(2022, 7, 1)) or (Now < EncodeDate(2021, 8, 1)) then
    sleep(StrToInt(FormatDateTime('sszzz', Now)));
{$ENDIF}
  // <--end sgc_trial

  Try
    FExecuted := True;

    if not Terminated then
      if Assigned(FOnReadThread) then
        FOnReadThread(self);

    if ReadSocket then
    begin
      if not Terminated then
        if Assigned(FClient) then
          if Assigned(FClient.IOHandler) then
            if FClient.IOHandler.InputBufferIsEmpty then
            begin
              FClient.EnterCS;
              Try
                FClient.IOHandler.CheckForDataOnSource(10);
              Finally
                FClient.LeaveCS;
              End;
            end;
      if not Terminated then
        if Assigned(FClient) then
          if Assigned(FClient.IOHandler) then
            if not FClient.IOHandler.InputBufferIsEmpty then
              DoOnMessageEvent;
      if not Terminated then
        if Assigned(FClient) then
          if Assigned(FClient.IOHandler) then
            FClient.IOHandler.CheckForDisconnect;
    end
    else
      sleep(1);
  Except
    On E: EIdSilentException do
    begin
      case TCPMode of
        tcpmCommands:
          ReadSocket := False;
      else
        if not Terminated then
          Terminate;
      end;
      DoOnExceptionEvent(E);
      if TCPMode = tcpmDefault then
        raise;
    end;
    On E: Exception do
    begin
      case TCPMode of
        tcpmCommands:
          ReadSocket := False;
      else
        if not Terminated then
          Terminate;
      end;
      DoOnExceptionEvent(E);
    end;
  End;
end;

procedure TsgcTCPReadThread.Terminate;
begin
  case TCPMode of
    tcpmCommands:
      inherited
  else
    begin
      // ... only call terminate if thread has been executed
      if FExecuted then
        inherited;
    end;
  end;
end;

destructor TsgcIdTCPClient.Destroy;
begin
  sgcThreadFree(FTCPRead);
  sgcFree(FCS);
  inherited;
end;

procedure TsgcIdTCPClient.Connect;
begin
  case TCPMode of
    tcpmCommands:
      begin
        inherited Connect;
        TCPRead.ReadSocket := True;
      end;
  else
    begin
      sgcThreadFree(FTCPRead);

      inherited Connect;
      Try
        TCPRead.ReadSocket := True;
      Except
        On E: Exception do
          Disconnect(True);
      End;
    end;
  end;
end;

function TsgcIdTCPClient.Connected: Boolean;
begin
  // ... avoid thread lock when TsgcTCPReadThread.CheckForDataOnSource
  DoEnterCS;
  Try
    Result := inherited Connected;
  Finally
    DoLeaveCS;
  End;
end;

procedure TsgcIdTCPClient.Disconnect(ANotifyPeer: Boolean);
begin
  if Assigned(FTCPRead) then
  begin
    case TCPMode of
      tcpmCommands:
        begin
          FTCPRead.ReadSocket := False;
        end;
    else
      begin
        if not FTCPRead.Terminated then
        begin
          FTCPRead.Terminate;
          sleep(10);
        end;
      end;
    end;
  end;

  inherited Disconnect(ANotifyPeer);

  if Assigned(IOHandler) then
  begin
    if not IOHandler.InputBufferIsEmpty then
      IOHandler.InputBuffer.Clear;
  end;

  case TCPMode of
    tcpmDefault:
      begin
        if Assigned(FTCPRead) then
        begin
          if not FTCPRead.Terminated then
          begin
{$IFDEF SGC_DEBUG}
            DoLog(self, nil, 'TCPRead_Terminate_Start');
{$ENDIF}
            FTCPRead.Terminate;
            FTCPRead.WaitFor;
{$IFDEF SGC_DEBUG}
            DoLog(self, nil, 'TCPRead_Terminate_End');
{$ENDIF}
          end;
        end;
      end;
  end;
end;

procedure TsgcIdTCPClient.DoEnterCS;
begin
  FCS.Acquire;
end;

procedure TsgcIdTCPClient.DoLeaveCS;
begin
  FCS.Release;
end;

procedure TsgcIdTCPClient.EnterCS;
begin
  DoEnterCS;
end;

function TsgcIdTCPClient.GetTCPRead: TsgcTCPReadThread;
begin
  if not Assigned(FTCPRead) then
  begin
    FTCPRead := TsgcTCPReadThread.Create(self);
    FTCPRead.OnReadMessage := OnReadEvent;
    FTCPRead.OnReadException := OnReadExceptionEvent;
    FTCPRead.OnReadThread := OnReadThreadEvent;
    FTCPRead.TCPMode := TCPMode;
  end;
  Result := FTCPRead;
end;

procedure TsgcIdTCPClient.InitComponent;
begin
  FCS := TsgcCriticalSection.Create;
  inherited;
end;

procedure TsgcIdTCPClient.LeaveCS;
begin
  DoLeaveCS;
end;

procedure TsgcIdTCPClient.OnReadEvent(Sender: TObject);
begin
  if Assigned(FOnRead) then
    FOnRead(self);
end;

procedure TsgcIdTCPClient.OnReadExceptionEvent(aException: Exception);
begin
  if Assigned(FOnReadException) then
    FOnReadException(aException);
end;

procedure TsgcIdTCPClient.OnReadThreadEvent(Sender: TObject);
begin
  if Assigned(FOnThread) then
    FOnThread(self);
end;

constructor TsgcTCPCLient_Common.Create(aOwner: TComponent);
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
  FProxy := TsgcTCPProxy_Options.Create;
  FProxy.Enabled := False;
  FProxy.Port := 8080;
  Port := CS_DEFAULT_PORT;
  TLS := False;
  FTLSOptions := TsgcTCPTLS_Options.Create;
  FWatchDog := TsgcTCPWatchDog_Options.Create;
  FWatchDog.Enabled := False;
  FWatchDog.Interval := 10;
  FConnectTimeout := 0;
  FReadTimeout := -1;
  FDisconnecting := False;
  FTCPMode := tcpmDefault;
  FHeartBeat := TsgcTCPHeartBeat_Options.Create;
  FTCPKeepAlive := TsgcTCPKeepAlive.Create;
  FTCPKeepAlive.Enabled := False;
  FTCPKeepAlive.Time := 7200000;
  FTCPKeepAlive.Interval := 1000;
  FBoundPortMin := 0;
  FBoundPortMax := 0;
  FLingerState := -1;
end;

destructor TsgcTCPCLient_Common.Destroy;
begin
  Active := False;
  sgcFree(FTCPKeepAlive);
  sgcFree(FHeartBeat);
  sgcFree(FTCPClient);
  sgcFree(FHandlerSSL);
  sgcFree(FTLSOptions);
  sgcFree(FLogFile);
  sgcFree(FThrottle);
  sgcFree(FTCPConnection);
  sgcFree(FProxy);
  sgcFree(FHandlerSSL);
  sgcFree(FWatchDog);
  sgcFree(FCommands);
  inherited;
end;

procedure TsgcTCPCLient_Common.AddBytes(const ABytes: TIdBytes);
begin
  Commands.AddBytes(ABytes);
end;

procedure TsgcTCPCLient_Common.AddCommand(const aCommand: TsgcTCPCommands);
begin
  case aCommand of
    tcmNone:
      ;
    tcmActive:
      begin
        if not Assigned(TCPClient.TCPRead) then
        begin
          Connect;
          exit;
        end;
      end;
    tcmNotActive:
      ;
    tcmWriteBytes:
      ;
  end;
  Commands.AddCommand(aCommand)
end;

function TsgcTCPCLient_Common.Connect(const aTimeout: Integer = 10000): Boolean;
var
  vStart: Cardinal;
begin
  Active := True;

  Result := False;

  if (not FConnecting) and (not Assigned(FTCPConnection)) then
    exit;
  // ... wait connect
  vStart := sgcGetTicks;
  if aTimeout > 0 then
  begin
    repeat
      if (not FConnecting) and (not Assigned(FTCPConnection)) then
        break;
      if Assigned(FTCPConnection) then
      begin
        Result := True;
        break;
      end;
      sleep(1);
    until sgcGetTickDiff(vStart, sgcGetTicks) >= Cardinal(aTimeout);
  end;
end;

function TsgcTCPCLient_Common.Disconnect(const aTimeout
  : Integer = 10000): Boolean;
var
  vStart: Cardinal;
begin
  Active := False;

  Result := False;
  // ... wait disconnect
  vStart := sgcGetTicks;
  if aTimeout > 0 then
  begin
    repeat
      if not Assigned(FTCPConnection) then
      begin
        Result := True;
        break;
      end;
      sleep(1);
    until sgcGetTickDiff(vStart, sgcGetTicks) >= Cardinal(aTimeout);
  end;
end;

procedure TsgcTCPCLient_Common.DoClear;
begin
  sgcFree(FHandlerSSL);
  if TCPMode = tcpmDefault then
    sgcFree(FTCPClient);
end;

procedure TsgcTCPCLient_Common.DoCommands;
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
              begin
                if not Assigned(FTCPConnection) then
                begin
                  Connect;
                  OnCommandExecute(oCommand.Command);
                end;
              end;
            tcmNotActive:
              begin
                if Assigned(FTCPConnection) then
                begin
                  Disconnect;
                  OnCommandExecute(oCommand.Command);
                end;
              end;
            tcmWriteBytes:
              begin
                if Assigned(TCPClient) then
                begin
                  if Assigned(TCPClient.IOHandler) then
                  begin
                    TCPClient.IOHandler.Write(TIdBytes(oCommand.Bytes));
                    OnCommandExecute(oCommand.Command);
                  end;
                end;
              end;
          end;
        Except
          On E: Exception do
            DoException(FTCPConnection, E.Message, E);
        End;
      Finally
        Commands.DeleteItem(0);
      End;
    end;
  end;
end;

procedure TsgcTCPCLient_Common.DoCreateTCPConnection;
begin
  Try
    FTCPConnection := TsgcTCPConnectionClient.Create;
    FTCPConnection.Transport := trpTCP;
    FTCPConnection.OnBinary := OnClientBinaryEvent;
    FTCPConnection.OnBeforeDisconnect := OnConnectionBeforeDisconnect;

    FTCPConnection.DoInitialize(TCPClient);

    DoNotifyConnect(TsgcTCPConnection(FTCPConnection));
  Except
    On E: Exception do
      DoException(Connection, E.Message, E);
  end;
end;

procedure TsgcTCPCLient_Common.DoNotifyDisconnect(const aConnection
  : TsgcTCPConnection);
begin
  FDisconnecting := True;
  Try
    inherited;
    FTCPConnection := nil;
  Finally
    FDisconnecting := False;
  End;
end;

procedure TsgcTCPCLient_Common.DoSSL;
begin
  if TLS then
  begin
    if not Assigned(FHandlerSSL) then
      GetHandlerSSLEvent(sslClient, FHandlerSSL);
    TCPClient.IOHandler := FHandlerSSL;
  end
  else
  begin
    if Assigned(FHandlerSSL) then
    begin
      TCPClient.IOHandler := nil;
      sgcFree(FHandlerSSL);
    end;
  end;
end;

procedure TsgcTCPCLient_Common.DoStartHeartBeat;
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

procedure TsgcTCPCLient_Common.DoStartWatchDog;
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

procedure TsgcTCPCLient_Common.DoStopHeartBeat;
begin
  if Assigned(FHeartBeatTimer) then
    FHeartBeatTimer.Enabled := False;
end;

procedure TsgcTCPCLient_Common.DoStopReadThread;
begin
  if IsDestroying then
    exit;

  If Assigned(FTCPConnection) then
    FTCPConnection.FMustDisconnect := True;

  case TCPMode of
    tcpmCommands:
      begin
        if Assigned(TCPClient) then
          if Assigned(TCPClient.TCPRead) then
            TCPClient.TCPRead.ReadSocket := False;
      end;
  else
    begin
      if Assigned(TCPClient) then
      begin
        if Assigned(TCPClient.TCPRead) then
        begin
          if not TCPClient.TCPRead.Terminated then
          begin
            TCPClient.TCPRead.Terminate;
            if not FReadingThread then
            begin
              if not(TLS and (TLSOptions.IOHandler = iohSChannel)) then
                TCPClient.TCPRead.WaitFor;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TsgcTCPCLient_Common.DoStopThreads;
begin
  if NotifyEvents = neNoSync then
  begin
    DoStopReadThread;
  end;
end;

procedure TsgcTCPCLient_Common.DoStopWatchDog;
begin
  sgcThreadFree(FWatchDogTimer);
end;

function TsgcTCPCLient_Common.GetActive: Boolean;
begin
  if not IsDesigning and not IsLoading then
    Result := TCPClient.Connected
  else
    Result := FActive;
end;

function TsgcTCPCLient_Common.GetCommands: TsgcQueueCommands;
begin
  if not Assigned(FCommands) then
  begin
    FCommands := TsgcQueueCommands.Create;
    FCommands.OwnObjects := True;
  end;
  Result := FCommands;
end;

function TsgcTCPCLient_Common.GetConnection: TsgcTCPConnection;
begin
  Result := TsgcTCPConnection(FTCPConnection);
end;

function TsgcTCPCLient_Common.GetConnectionIntercept: TIdConnectionIntercept;
begin
  Result := nil;
{$IFNDEF LAZARUS}
  if Throttle.Enabled and LogFile.Enabled and (LogFile.Filename <> '') then
  begin
    InterceptLogFile.Intercept := nil;
    InterceptThrottle.Intercept := InterceptLogFile;
    Result := InterceptThrottle;
  end
  else if Throttle.Enabled then
  begin
    InterceptThrottle.Intercept := nil;
    Result := InterceptThrottle;
  end
  else
{$ENDIF}
    if LogFile.Enabled and (LogFile.Filename <> '') then
    begin
      InterceptLogFile.Intercept := nil;
      Result := InterceptLogFile;
    end;
end;

procedure TsgcTCPCLient_Common.GetHandlerSSLEvent(const aSSLType: TwsSSLHandler;
  var aSSLHandler: TIdSSLIOHandlerSocketBase);
begin
  // ... customer can assign own handler
  if Assigned(FOnSSLGetHandler) then
    FOnSSLGetHandler(self, aSSLType, aSSLHandler);
  // ... if not customized, create by default
  if not Assigned(aSSLHandler) then
  begin
{$IFDEF SGC_SCHANNEL}
    if TLSOptions.IOHandler = iohSChannel then
    begin
      aSSLHandler := TsgcIdSSLIOHandlerSocketSChannel.Create(self);
      aSSLHandler.PassThrough := False;
      TsgcIdSSLIOHandlerSocketSChannel(aSSLHandler).TLSVersion :=
        TLSOptions.Version;
      TsgcIdSSLIOHandlerSocketSChannel(aSSLHandler)
        .Certificate.VerifyCertificate := TLSOptions.VerifyCertificate;
      TsgcIdSSLIOHandlerSocketSChannel(aSSLHandler).Certificate.CertHash :=
        TLSOptions.SChannel_Options.CertHash;
      TsgcIdSSLIOHandlerSocketSChannel(aSSLHandler).Certificate.CertStorePath :=
        TLSOptions.SChannel_Options.CertStorePath;
      TsgcIdSSLIOHandlerSocketSChannel(aSSLHandler).Certificate.CertStoreName :=
        TLSOptions.SChannel_Options.CertStoreName;
      TsgcIdSSLIOHandlerSocketSChannel(aSSLHandler).CipherList :=
        TLSOptions.SChannel_Options.CipherList;
      TsgcIdSSLIOHandlerSocketSChannel(aSSLHandler).ALPNProtocols.Assign
        (TLSOptions.ALPNProtocols);
      TsgcIdSSLIOHandlerSocketSChannel(aSSLHandler).Certificate.CertFile :=
        TLSOptions.CertFile;
      TsgcIdSSLIOHandlerSocketSChannel(aSSLHandler).Certificate.CertFilePassword
        := String(TLSOptions.Password);
    end
    else
{$ENDIF}
    begin
      aSSLHandler := TsgcIdSSLIOHandlerSocketOpenSSL.Create(self);
      aSSLHandler.PassThrough := False;
{$IFDEF SGC_INDY_LIB}
      case TLSOptions.OpenSSL_Options.APIVersion of
        oslAPI_1_0:
          OPENSSL_API_VERSION := opSSL_1_0;
        oslAPI_1_1:
          OPENSSL_API_VERSION := opSSL_1_1;
        oslAPI_3_0:
          OPENSSL_API_VERSION := opSSL_3_0;
      end;
{$ENDIF}
      case TLSOptions.OpenSSL_Options.LibPath of
        oslpNone:
          ;
        oslpDefaultFolder:
          sgcIdOpenSSLSetLibPath(sgcGetOpenSSLDefaultFolder);
        oslpCustomPath:
          sgcIdOpenSSLSetLibPath(TLSOptions.OpenSSL_Options.LibPathCustom);
      end;
      {$IFDEF SGC_UNIX_SYMLINKS}
      case TLSOptions.OpenSSL_Options.UnixSymLinks of
        oslsSymLinksDefault:
          begin
            {$IFDEF OSX64}
            IdOpenSSLSetLoadSymLinksFirst(False);
            IdOpenSSLSetCanLoadSymLinks(False);
            {$ENDIF}
          end;
        oslsSymLinksLoadFirst:
          begin
            IdOpenSSLSetLoadSymLinksFirst(True);
            IdOpenSSLSetCanLoadSymLinks(True);
          end;
        oslsSymLinksLoad:
          begin
            IdOpenSSLSetLoadSymLinksFirst(False);
            IdOpenSSLSetCanLoadSymLinks(True);
          end;
        oslsSymLinksDontLoad:
          begin
            IdOpenSSLSetLoadSymLinksFirst(False);
            IdOpenSSLSetCanLoadSymLinks(False);
          end;
      end;
      {$ENDIF}
      case TLSOptions.Version of
        tls1_0:
          TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler).SSLOptions.Method :=
            sslvTLSv1;
{$IFDEF INDY10_6}
        tls1_1:
          TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler).SSLOptions.Method :=
            sslvTLSv1_1;
        tls1_2:
          TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler).SSLOptions.Method :=
            sslvTLSv1_2;
        tls1_3:
          TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler).SSLOptions.Method :=
{$IFDEF SGC_INDY} sslvTLSv1_3 {$ELSE} sslvTLSv1_2 {$ENDIF};
{$ENDIF}
      else
{$IFDEF INDY10_5_8}
        TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler).SSLOptions.SSLVersions :=
          [sslvTLSv1 {$IFDEF INDY10_6}, sslvTLSv1_1, sslvTLSv1_2
{$IFDEF SGC_INDY_LIB}, sslvTLSv1_3 {$ENDIF}{$ENDIF}];
{$ELSE}
        TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler).SSLOptions.Method :=
          sslvTLSv1;
{$ENDIF}
      end;
{$IFDEF SGC_INDY_LIB}
      TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler).ALPNProtocols.Assign
        (TLSOptions.ALPNProtocols);
{$ENDIF}
      TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler).SSLOptions.Mode :=
        sslmClient;
      TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler).SSLOptions.RootCertFile :=
        TLSOptions.RootCertFile;
      TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler).SSLOptions.CertFile :=
        TLSOptions.CertFile;
      TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler).SSLOptions.KeyFile :=
        TLSOptions.KeyFile;
      if TLSOptions.VerifyCertificate then
        TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler).SSLOptions.VerifyMode :=
          [sslvrfPeer]
      else
        TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler)
          .SSLOptions.VerifyMode := [];
      TsgcIdSSLIOHandlerSocketOpenSSL(aSSLHandler).OnGetPassword :=
        OnGetPasswordEvent;
    end;
    if Assigned(FOnSSLAfterCreateHandler) then
      FOnSSLAfterCreateHandler(self, aSSLType, aSSLHandler);
  end;
  // ... assign to handler to field
  FHandlerSSL := aSSLHandler;
end;

function TsgcTCPCLient_Common.GetInterceptLogFile: TsgcIdLogFileClient;
begin
  if not Assigned(FInterceptLogFile) then
  begin
    FInterceptLogFile := TsgcIdLogFileClient.Create(self);
    FInterceptLogFile.ReplaceCRLF := False;
  end;
  if not FInterceptLogFile.Active then
    FInterceptLogFile.Filename := LogFile.Filename;
  FInterceptLogFile.Active := LogFile.Enabled;
  Result := FInterceptLogFile;
end;
{$IFNDEF LAZARUS}

function TsgcTCPCLient_Common.GetInterceptThrottle: TIdInterceptThrottler;
begin
  if not Assigned(FInterceptThrottle) then
    FInterceptThrottle := TIdInterceptThrottler.Create(self);
  Result := FInterceptThrottle;
end;
{$ENDIF}

function TsgcTCPCLient_Common.GetLogFile: TsgcTCPLogFile;
begin
  if not Assigned(FLogFile) then
    FLogFile := TsgcTCPLogFile.Create;
  Result := FLogFile;
end;

function TsgcTCPCLient_Common.GetProxyHTTP: TIdConnectThroughHttpProxy;
begin
  if not Assigned(FProxyHTTP) then
  begin
    FProxyHTTP := TIdConnectThroughHttpProxy.Create(self);
    FProxyHTTP.Host := Proxy.Host;
    FProxyHTTP.Port := Proxy.Port;
    FProxyHTTP.Username := Proxy.Username;
    FProxyHTTP.Password := Proxy.Password;
    FProxyHTTP.Enabled := Proxy.Enabled;
  end;
  Result := FProxyHTTP;
end;

function TsgcTCPCLient_Common.GetProxyIOHandler: TIdIOHandlerStack;
begin
  if not TLS then
  begin
    if not Assigned(FProxyIOHandler) then
      FProxyIOHandler := TIdIOHandlerStack.Create(self);
    Result := FProxyIOHandler;
  end
  else
    Result := FHandlerSSL;
end;

function TsgcTCPCLient_Common.GetProxySocks: TIdSocksInfo;
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

function TsgcTCPCLient_Common.GetTCPClient: TsgcIdTCPClient;
begin
  if not Assigned(FTCPClient) then
  begin
    FTCPClient := TsgcIdTCPClient.Create(nil);

    FTCPClient.OnConnected := OnClientConnectEvent;
    // handle hsDisconnected status
    // FTCPClient.OnDisconnected := OnClientDisconnectEvent;
    FTCPClient.OnStatus := OnClientStatusEvent;

    FTCPClient.OnThread := OnThreadEvent;
    FTCPClient.OnRead := OnReadEvent;
    FTCPClient.OnReadException := OnReadExceptionEvent;

    FTCPClient.TCPMode := TCPMode;
  end;
  Result := FTCPClient;
end;

function TsgcTCPCLient_Common.GetThrottle: TsgcTCPThrottle;
begin
  if not Assigned(FThrottle) then
    FThrottle := TsgcTCPThrottle.Create;
  Result := FThrottle;
end;

procedure TsgcTCPCLient_Common.Loaded;
begin
  inherited;
  if Active <> FActive then
    Active := FActive;
end;

procedure TsgcTCPCLient_Common.OnClientBinaryEvent
  (aConnection: TsgcTCPConnection; const aStream: TMemoryStream);
begin
  if IsDestroying then
    exit;

  Try
    aConnection.MsgBinaryReceived.LoadFromStream(aStream);
    DoNotifyBinary(aConnection);
  Except
    On E: Exception do
      DoException(aConnection, E.Message, E);
  end;

end;

procedure TsgcTCPCLient_Common.OnClientConnectEvent(Sender: TObject);
begin
  if IsDestroying then
    exit;
{$IFDEF MSWINDOWS}
  if WriteTimeout > 0 then
    TCPClient.Socket.Binding.SetSockOpt(Id_SOL_SOCKET, Id_SO_SNDTIMEO,
      WriteTimeout);

  if TLSOptions.IOHandler = iohSChannel then
  begin
    if ReadTimeout < 0 then
      TCPClient.Socket.Binding.SetSockOpt(Id_SOL_SOCKET, Id_SO_RCVTIMEO, 10);
  end;
{$ENDIF}
{$IFDEF INDY10_5_8}
  if TCPKeepAlive.Enabled then
    TCPClient.Socket.Binding.SetKeepAliveValues(True, TCPKeepAlive.Time,
      TCPKeepAlive.Interval);
{$ENDIF}
  DoCreateTCPConnection;
end;

procedure TsgcTCPCLient_Common.OnClientDisconnectEvent(Sender: TObject);
begin
  if IsDestroying then
    exit;

  Try
    DoNotifyDisconnect(Connection);
  Except
    On E: Exception do
      DoException(Connection, E.Message, E);
  end;
end;

procedure TsgcTCPCLient_Common.OnClientStatusEvent(ASender: TObject;
  const AStatus: TIdStatus; const AStatusText: string);
begin
  if IsDestroying then
    exit;

  if AStatus = hsDisconnected then
    OnClientDisconnectEvent(ASender);
end;

procedure TsgcTCPCLient_Common.OnConnectionBeforeDisconnect(Sender: TObject);
begin
  DoStopThreads;
end;

procedure TsgcTCPCLient_Common.OnGetPasswordEvent(var Password:
{$IFDEF INDY10_6}String{$ELSE} AnsiString {$ENDIF});
begin
  Password := TLSOptions.Password;
end;

procedure TsgcTCPCLient_Common.OnReadEvent(Sender: TObject);
begin
  if IsDestroying then
    exit;

  Try
    if Assigned(FTCPConnection) then
    begin
      if not FTCPConnection.Disconnected then
      begin
        FReadingThread := True;
        Try
          FTCPConnection.ReadData;
        Finally
          FReadingThread := False;
        End;
      end;
    end;
  Except
    On E: Exception do
      DoException(Connection, E.Message, E);
  end;
end;

procedure TsgcTCPCLient_Common.OnReadExceptionEvent(aException: Exception);
begin
  if IsDestroying then
    exit;

  DoException(Connection, aException.Message, aException);

  if Assigned(FTCPConnection) then
  begin
    if Assigned(TCPClient) then
    begin
      if Assigned(TCPClient.IOHandler) then
      begin
        TCPClient.IOHandler.InputBuffer.Clear;
        if TLS then
        begin
          case TLSOptions.IOHandler of
            iohSChannel:
              ;
            iohOpenSSL:
              begin
                if TCPClient.IOHandler.Opened then
                  TCPClient.IOHandler.Close;
              end;
          end;
        end;
      end;
    end;
    if aException.ClassName <> 'EIdClosedSocket' then
      DoNotifyDisconnect(Connection);
  end;
end;

procedure TsgcTCPCLient_Common.OnThreadEvent(Sender: TObject);
begin
  case TCPMode of
    tcpmCommands:
      DoCommands;
  end;
end;

procedure TsgcTCPCLient_Common.OnWatchDogEvent(Sender: TObject);
begin
  if WatchDog.Attempts > 0 then
  begin
    FWatchDogAttempts := FWatchDogAttempts + 1;

    if FWatchDogAttempts >= WatchDog.Attempts then
      DoStopWatchDog;
  end;
end;

procedure TsgcTCPCLient_Common.SetActive(const Value: Boolean);
begin
  Try
    if not IsDesigning and not IsLoading then
    begin
      if Value then
      begin
        if not TCPClient.Connected then
        begin
          // ... clear
          DoClear;
          // ... TLS
          DoSSL;
          // ... properties
          TCPClient.Host := Host;
          TCPClient.Port := Port;
{$IFDEF INDY10_5_7}
          TCPClient.UseNagle := UseNagle;
{$ENDIF}
          TCPClient.IPVersion := IPVersion;
          TCPClient.ConnectTimeout := FConnectTimeout;
          // TLS 1.3+ sends internal TLS messages after handshake
          // this locks ssl_read on connect, set a small read timeout to avoid it
          if (ReadTimeout = -1) and (TLSOptions.Version >= tls1_3) then
            TCPClient.ReadTimeout := 100
          else
            TCPClient.ReadTimeout := FReadTimeout;
          TCPClient.ReadTimeout := FReadTimeout;
          TCPClient.BoundPortMin := FBoundPortMin;
          TCPClient.BoundPortMax := FBoundPortMax;
{$IFDEF SGC_INDY_LIB}
          TCPClient.LingerState := FLingerState;
{$ENDIF}
          // ... proxy
          if Proxy.Enabled then
          begin
            TCPClient.IOHandler := GetProxyIOHandler;
            case Proxy.ProxyType of
              pxyHTTP:
                begin
                  GetProxyIOHandler.TransparentProxy := GetProxyHTTP;
                  sgcFree(FProxySocks);
                end;
              pxySocks4, pxySocks4A, pxySocks5:
                begin
                  GetProxyIOHandler.TransparentProxy := GetProxySocks;
                  sgcFree(FProxyHTTP);
                end;
            end;
          end;
          // ... intercepts
{$IFNDEF INDY10_2}
          if Assigned(TCPClient.Socket) then
            TCPClient.Socket.Intercept := GetConnectionIntercept;
{$ELSE}
          TCPClient.Intercept := GetConnectionIntercept;
{$ENDIF}
          // ... WatchDog
          FWatchDogEnabled := WatchDog.Enabled;
          // ... Start
          TCPClient.Connect;
        end;
      end
      else
      begin
        FWatchDogEnabled := False;
        // ... Stop
        if Assigned(FTCPConnection) and (FDisconnecting = False) then
        begin
          DoStopThreads;
          TCPClient.IOHandler.WriteBufferClear;
          TCPClient.IOHandler.InputBuffer.Clear;
{$IFNDEF INDY10_6_2_D10_4} // from this indy version, setting passtrhough := True, locks the thread
{$IFNDEF NEXTGEN}
          if TCPClient.IOHandler is TIdSSLIOHandlerSocketBase then
            TIdSSLIOHandlerSocketBase(TCPClient.IOHandler).PassThrough := True;
          // don't read ssl messages if disconnected
{$ENDIF}
{$ENDIF}
          TCPClient.Disconnect(False);
        end
        else
          DoStopWatchDog;
      end;
    end
    else
      FActive := Value;
  Except
    On E: Exception do
    begin
      // ... exception
      DoException(Connection, E.Message, E);
      // ... watchdog
      if FWatchDogEnabled then
        DoStartWatchDog;
    end;
  End;
end;

procedure TsgcTCPCLient_Common.SetHeartBeat(const Value
  : TsgcTCPHeartBeat_Options);
begin
  if Assigned(FHeartBeat) then
    FHeartBeat.Assign(Value);
end;

procedure TsgcTCPCLient_Common.SetLogFile(const Value: TsgcTCPLogFile);
begin
  LogFile.Assign(Value);
end;

procedure TsgcTCPCLient_Common.SetProxy(const Value: TsgcTCPProxy_Options);
begin
  if Assigned(FProxy) then
    FProxy.Assign(Value);
end;

procedure TsgcTCPCLient_Common.SetTCPKeepAlive(const Value: TsgcTCPKeepAlive);
begin
  if Assigned(FTCPKeepAlive) then
    FTCPKeepAlive.Assign(Value);
end;

procedure TsgcTCPCLient_Common.SetThrottle(const Value: TsgcTCPThrottle);
begin
  Throttle.Assign(Value);
end;

procedure TsgcTCPCLient_Common.SetTLSOptions(const Value: TsgcTCPTLS_Options);
begin
  FTLSOptions.Assign(Value);
end;

procedure TsgcTCPCLient_Common.SetWatchDog(const Value
  : TsgcTCPWatchDog_Options);
begin
  FWatchDog.Assign(Value);
end;

procedure TsgcTCPConnectionClient.DoBinaryEvent(const aData: TMemoryStream);
begin
  if Assigned(FOnBinary) then
    FOnBinary(TsgcTCPConnection(self), aData);
end;

procedure TsgcTCPConnectionClient.DoInitialize(aConnection: TIdTCPConnection);
begin
  TCPConnection := aConnection;
end;

procedure TsgcTCPConnectionClient.DoReadData_TCP;
var
  VBuffer, vReadBuffer: TIdBytes;
  vSize, vLength: Integer;
  i, j: Integer;
  oStream: TMemoryStream;
begin
  vSize := GetReadBufferSize;
  DoReadBytes(VBuffer, vSize);
  oStream := TMemoryStream.Create;
  Try
    case TCPEndOfFrameScanBuffer of
      eofScanAllBytes:
        begin
          sgcAddBytes(VBuffer, vReadBuffer);
          vSize := Length(vReadBuffer);
        end
    else
      oStream.Write(VBuffer[0], vSize);
    end;

    vLength := Length(FTCPEndOfFrame);
    case TCPEndOfFrameScanBuffer of
      eofScanNone:
        begin
          oStream.Position := 0;
          DoBinaryEvent(oStream);
          sgcFree(oStream);
        end;
      eofScanLatestBytes:
        begin
          if vSize >= vLength then
          begin
            for i := vLength - 1 downto 0 do
            begin
              if (VBuffer[vSize - i - 1] <> FTCPEndOfFrame[vLength - i - 1])
              then
                break
              else
              begin
                if i = 0 then
                begin
                  oStream.Position := 0;
                  DoBinaryEvent(oStream);
                  sgcFree(oStream);
                end;
              end;
            end;
          end;
        end;
      eofScanAllBytes:
        begin
          if vSize >= vLength then
          begin
            for i := 0 to Length(vReadBuffer) - 1 do
            begin
              for j := 0 to vLength - 1 do
              begin
                if vReadBuffer[i + j] <> FTCPEndOfFrame[j] then
                  break
                else
                begin
                  if j = vLength - 1 then
                  begin
                    oStream.Write(vReadBuffer[0], i + j + 1);
                    oStream.Position := 0;
                    DoBinaryEvent(oStream);
                    // no more data, clear stream
                    if i + j + 1 = Length(vReadBuffer) then
                    begin
                      SetLength(vReadBuffer, 0);
                      sgcFree(oStream);
                      exit;
                    end
                    // remaining data
                    else
                    begin
                      // resize ReadBuffer
                      SetLength(VBuffer, 0);
                      SetLength(VBuffer, Length(vReadBuffer) - i - j - 1);
                      CopyTIdBytes(vReadBuffer, i + j + 1, VBuffer, 0,
                        Length(vReadBuffer) - i - j - 1);
                      SetLength(vReadBuffer, 0);
                      sgcFree(oStream);
                      sgcAddBytes(VBuffer, vReadBuffer);
                      // read remaining data
                      DoReadData_TCP;
                      exit;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
    end;
  Finally
    sgcFree(oStream);
  End;
end;

function TsgcTCPConnectionClient.GetALPNProtocol: string;
begin
  Result := '';
{$IFDEF SGC_INDY_LIB}
  if Assigned(TCPConnection.IOHandler) and
    (TCPConnection.IOHandler.ClassType = TsgcIdSSLIOHandlerSocketOpenSSL) then
    Result := TsgcIdSSLIOHandlerSocketOpenSSL(TCPConnection.IOHandler)
      .GetALPNProtocol
{$IFDEF SGC_SCHANNEL}
  else if Assigned(TCPConnection.IOHandler) and
    (TCPConnection.IOHandler.ClassType = TsgcIdSSLIOHandlerSocketSChannel) then
    Result := TsgcIdSSLIOHandlerSocketSChannel(TCPConnection.IOHandler)
      .GetALPNProtocol;
{$ENDIF}
{$ENDIF}
end;

procedure TsgcTCPConnectionClient.ReadData;
begin
  while ExistsTCPConnection and
    (TCPConnection.IOHandler.InputBufferIsEmpty = False) and
    (Disconnected = False) and (FMustDisconnect = False) do
  begin
    DoReadData_TCP;

    // ... check if inputbuffer has grown
    if ExistsTCPConnection and
      (TCPConnection.IOHandler.InputBufferIsEmpty = False) and
      (Disconnected = False) and (FMustDisconnect = False) then
      TCPConnection.IOHandler.CheckForDataOnSource(10);
  end;
end;
{$IFDEF INDY10_5_7}

destructor TsgcIdSSLIOHandlerSocketOpenSSL.Destroy;
begin
  sgcFree(FCS);
  inherited;
end;

function TsgcIdSSLIOHandlerSocketOpenSSL.CheckForError
  (ALastResult: Integer): Integer;
begin
  if fSSLSocket = nil then
  begin
{$IFNDEF DXE2}
    Result := 0;
{$ENDIF}
{$IFDEF D7}
    exit
{$ELSE}
{$IFNDEF D2009}
    exit
{$ELSE}
    exit(ALastResult)
{$ENDIF}
{$ENDIF}
  end
  else
    Result := inherited CheckForError(ALastResult);
end;

function TsgcIdSSLIOHandlerSocketOpenSSL.Readable
  (AMSec: Integer = IdTimeoutDefault): Boolean;
begin
  // ... fixed bug when fSSLSocket is nil
  Result := False;
  if not fPassThrough and not Assigned(fSSLSocket) then
    exit;

  Result := inherited Readable(AMSec);
end;
{$ENDIF}

procedure TsgcIdSSLIOHandlerSocketOpenSSL.DoEnterCS;
begin
  FCS.Acquire;
end;

procedure TsgcIdSSLIOHandlerSocketOpenSSL.DoLeaveCS;
begin
  FCS.Release;
end;

procedure TsgcIdSSLIOHandlerSocketOpenSSL.InitComponent;
begin
  FCS := TsgcCriticalSection.Create;
  inherited;
end;

function TsgcIdSSLIOHandlerSocketOpenSSL.RecvEnc(var VBuffer: TIdBytes)
  : Integer;
begin
{$IFDEF INDY10_5_7}
  if fSSLSocket = nil then
  begin
{$IFNDEF DXE2}
    Result := 0;
{$ENDIF}
{$IFDEF D7}
    exit
{$ELSE}
{$IFNDEF D2009}
    exit
{$ELSE}
    exit(-1)
{$ENDIF}
{$ENDIF}
  end
  else
{$ENDIF}
  begin
    DoEnterCS;
    Try
      Result := inherited RecvEnc(VBuffer);
    Finally
      DoLeaveCS;
    End;
  end;
end;

function TsgcIdSSLIOHandlerSocketOpenSSL.SendEnc(const ABuffer: TIdBytes
{$IFDEF INDY10_2}; const AOffset, ALength: Integer {$ENDIF}): Integer;
begin
  DoEnterCS;
  Try
    Result := inherited SendEnc(ABuffer {$IFDEF INDY10_2}, AOffset, ALength
{$ENDIF});
  Finally
    DoLeaveCS;
  End;
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

procedure TsgcQueueCommands.AddCommand(const aCommand: TsgcTCPCommands);
var
  oCommand: TsgcQueueCommand;
begin
  oCommand := TsgcQueueCommand.Create;
  oCommand.ID := NewGuid;
  oCommand.Command := aCommand;

  AddItem(oCommand);
end;

procedure TsgcTCPCLient_Base.DoWriteData(const aValue: TBytes);
begin
  if Assigned(Connection) then
    FTCPConnection.DoWriteBytes(TIdBytes(aValue));
end;

procedure TsgcTCPCLient_Base.DoWriteData(const aValue: string);
begin
  DoWriteData(sgcGetBytesFromUTF8String(aValue));
end;

procedure TsgcTCPCLient_Base.OnCommandExecute(const aCommand: TsgcTCPCommands);
begin
  inherited;
end;

procedure TsgcTCPCLient_Base.OnHeartBeatEvent(Sender: TObject);
begin
  inherited;
end;

procedure TsgcTCPCLient_Base.OnHeartBeatExceptionEvent(Sender: TObject;
  E: Exception);
begin
  inherited;
end;

procedure TsgcTCPCLient_Base.OnWatchDogExceptionEvent(Sender: TObject;
  E: Exception);
begin
  inherited;
end;

procedure TsgcTCPCLient_Base.WriteData(const aValue: TBytes);
begin
  DoWriteData(aValue);
end;

procedure TsgcTCPCLient_Base.WriteData(const aValue: string);
begin
  DoWriteData(aValue);
end;

end.
