
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScSSHChannel;

interface

uses
{$IFDEF MSWINDOWS}
  {$IFNDEF FPC}
  {$IFNDEF BCB}
  {$NOINCLUDE winsock}
  {$HPPEMIT '#include <winsock2.h>'}
  {$ENDIF}
  {$ENDIF}
  WinSock,
  Windows, ActiveX,
  {$IFNDEF BCB}{$IFNDEF FPC}
    {$NOINCLUDE ActiveX}
  {$ENDIF}{$ENDIF}
{$ENDIF}
{$IFDEF POSIX}
  System.Types, Posix.SysSocket,
{$ENDIF}
{$IFDEF UNIX}
  Types, Sockets,
{$ENDIF}
  Classes, SysUtils,
  ScCLRClasses, ScUtils, ScConsts, ScVio, ScVioTcp,
  ScSSHClient, ScClient, ScSSH2Channel,
  ScBridge, ScSSHUtils, ScSSHSocket, ScTypes, ScTCPServer;

{$HPPEMIT '#ifdef MANAGED_INTERFACE_OPERATORS'}
{$HPPEMIT '#undef MANAGED_INTERFACE_OPERATORS'}
{$HPPEMIT '#endif'}

type
  TScSSHChannel = class;
  TScSSHStream = class;
  TListRWFlows = class;

  // Create logical connection (channel) over SSH-tunnel
  TScSSHCustomChannel = class(TComponent, ISequentialStream)
  private
    FClient: TScSSHClient;
    FTimeout: integer;
    FEventsCallMode: TScEventCallMode;
    FEnvironment: TStrings;
    FChannelRequest: TChannelRequest;
    FChannelInfo: TScSSHChannelInfo;

    FOnAsyncReceive: TScAsyncReceiveEvent;
    FOnAsyncError: TScErrorEvent;
    FOnConnect: TNotifyEvent;
    FOnDisconnect: TNotifyEvent;
    FOnDataFromClient: TScData;
    FOnDataToClient: TScData;
    FOnChannelError: TScChannelError;
    FBeforeChannelConnect: TScBeforeChannelConnect;
    FAfterChannelDisconnect: TScAfterChannelDisconnect;

    procedure SetClient(Value: TScSSHClient);
    procedure SetTimeout(const Value: integer);
    procedure SetEnvironment(Value: TStrings);

    function InternalReadBuffer(const Buffer: TValueArr; const Offset, Count: integer): integer;
    function InternalReadNoWait(const Buffer: TValueArr; const Offset, Count: integer): integer; overload;
    function InternalReadNoWait(Stream: TStream): integer; overload;
    function InternalWriteBuffer(const Buffer: TValueArr; const Offset, Count: integer): integer;
    function GetExitStatusCode: integer;


  protected
    FStreamedConnected: boolean;
    FStream: TSsh2Channel;
    FUseUnicode: boolean;

    procedure CheckInactive;
    procedure CheckReadingAbility; virtual;
    procedure CheckWritingAbility; virtual;
    function DecodeBytes(const bytes: TBytes; index, count: Integer): string;
    function EncodeString(const str: string): TBytes;
    function Readable(const DataLen: Integer; const MillisecondsTimeout: Integer = MaxInt): boolean;
    function DirectWrite(const Buffer: TValueArr; Offset, Count: integer): integer;
    function ReadAllData(var Buffer: TBytes): integer;
    procedure CloseIfDisconnected;

    procedure AssignTo(Dest: TPersistent); override;
    procedure Loaded; override;

    procedure OpenChannel(const ToHost: string = ''; const ToPort: Integer = -1);
    procedure DoConnect; virtual;
    procedure DoDisconnect; virtual;
    function GetConnected: boolean; virtual;
    procedure SetConnected(Value: boolean); virtual;
    procedure DoAfterChannelDisconnect(Sender: TObject);

    procedure SetEventsCallMode(Value: TScEventCallMode);
    function GetInCount: Integer;
    function GetOutCount: Integer;
    procedure DoReceiveData(Sender: TObject);
    procedure NotifyOnAsyncReceive;
    procedure DoAsyncError(E: Exception; var Fail: boolean);

    property Environment: TStrings read FEnvironment write SetEnvironment;
    property OnDataFromClient: TScData read FOnDataFromClient write FOnDataFromClient;
    property OnDataToClient: TScData read FOnDataToClient write FOnDataToClient;

  public
  {$IFNDEF FPC}
    {$IFDEF VER22P}
    function Read(pv: IntPtr; cb: FixedUInt; pcbRead: PFixedUInt): HResult; stdcall;
    {$ELSE}
    function Read(pv: IntPtr; cb: Longint; pcbRead: PLongint): HResult; stdcall;
    {$ENDIF}
  {$ELSE}
    function Read(pv: Pointer; cb: DWord; pcbRead: PDWord): HRESULT; stdcall;
  {$ENDIF}
  {$IFNDEF FPC}
    {$IFDEF VER22P}
    function Write(pv: IntPtr; cb: FixedUInt; pcbWritten: PFixedUInt): HResult; stdcall;
    {$ELSE}
    function Write(pv: IntPtr; cb: Longint; pcbWritten: PLongint): HResult; stdcall;
    {$ENDIF}
  {$ELSE}
    function Write(pv: Pointer; cb: DWord; pcbWritten: PDWord): HRESULT; stdcall;
  {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Connect; virtual;
    procedure Disconnect; virtual;

    function ReadBuffer(var Buffer; const Count: integer): integer; overload;
    function ReadBuffer(var Buffer: TBytes; const Offset, Count: integer): integer; overload;
    function ReadNoWait(var Buffer; const Count: integer): integer; overload;
    function ReadNoWait(var Buffer: TBytes; const Offset, Count: integer): integer; overload;
    function ReadNoWait(Stream: TStream): integer; overload;
    function ReadString: string;
    function SkipBuffer(Count: integer): integer;

    function WriteBuffer(const Buffer; const Count: integer): integer; overload;
    function WriteBuffer(const Buffer: TBytes; const Offset, Count: integer): integer; overload;
    procedure WriteString(const Buffer: string);

    function GetRemainedWindowSize: integer;

    property ChannelInfo: TScSSHChannelInfo read FChannelInfo;
    property InCount: Integer read GetInCount;
    property OutCount: Integer read GetOutCount;

    property Connected: boolean read GetConnected write SetConnected default False;
    property Timeout: integer read FTimeout write SetTimeout default DEFAULT_TIMEOUT;
    property UseUnicode: boolean read FUseUnicode write FUseUnicode default False;
    property ExitStatusCode: integer read GetExitStatusCode;

    property OnConnect: TNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect: TNotifyEvent read FOnDisconnect write FOnDisconnect;

  published
    property Client: TScSSHClient read FClient write SetClient;
    property EventsCallMode: TScEventCallMode read FEventsCallMode write SetEventsCallMode default ecAsynchronous;

    property OnAsyncReceive: TScAsyncReceiveEvent read FOnAsyncReceive write FOnAsyncReceive;
    property OnAsyncError: TScErrorEvent read FOnAsyncError write FOnAsyncError;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TScSSHChannel = class(TScSSHCustomChannel)
  private
    FSSHStream: TScSSHStream;
    FSourcePort: integer;
    FDestHost: string;
    FDestPort: integer;
    FGatewayPorts: boolean;
    FRemote: boolean;
    FDirect: boolean;
    FDynamic: boolean;
    FRemoteConnected: boolean;
    FIPVersion: TIPVersion;
    FOnError: TScErrorEvent;
    FOnSocketConnect: TScSocketEvent;
    FOnSocketDisconnect: TScSocketEvent;
    FListLPFThreads: TListRWFlows;

    procedure SetSourcePort(const Value: integer);
    procedure SetDestHost(const Value: string);
    procedure SetDestPort(const Value: integer);
    procedure SetGatewayPorts(const Value: boolean);
    procedure SetRemote(const Value: boolean);
    procedure SetDirect(const Value: boolean);
    procedure SetDynamic(const Value: boolean);
    procedure SetIPVersion(const Value: TIPVersion);
    function GetSSHStream: TScSSHStream;
    procedure DoSocketConnect(Sender: TObject; const SockAddr: PSockAddr);
    procedure DoSocketDisconnect(Sender: TObject; const SockAddr: PSockAddr);

  protected
    FListenLPFThread: TScTCPServerThread;

    procedure AssignTo(Dest: TPersistent); override;
    procedure CheckReadingAbility; override;
    procedure CheckWritingAbility; override;
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function GetConnected: boolean; override;
    procedure DoError(Sender: TObject; E: Exception);
    procedure AcceptSocketConnection(Sender: TObject; Vio: TCRVioTcp);
    procedure AfterLPFThreadExecute(Sender: TObject);
    procedure CheckIfLPFStopped(Sender: TObject; var NeedStop: boolean);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetListenedPort: integer;

    property SSHStream: TScSSHStream read GetSSHStream;

  published
    property SourcePort: integer read FSourcePort write SetSourcePort default 0;
    property DestHost: string read FDestHost write SetDestHost;
    property DestPort: integer read FDestPort write SetDestPort default 0;
    property GatewayPorts: boolean read FGatewayPorts write SetGatewayPorts default False;
    property Remote: boolean read FRemote write SetRemote default False;
    property Direct: boolean read FDirect write SetDirect default False;
    property Dynamic: boolean read FDynamic write SetDynamic default False;
    property IPVersion: TIPVersion read FIPVersion write SetIPVersion default DefValIPVersion;

    property Connected;
    property Timeout;
    property OnConnect;
    property OnDisconnect;

    property OnError: TScErrorEvent read FOnError write FOnError;
    property OnSocketConnect: TScSocketEvent read FOnSocketConnect write FOnSocketConnect;
    property OnSocketDisconnect: TScSocketEvent read FOnSocketDisconnect write FOnSocketDisconnect;
  end;

  TScSSHStream = class(TStream)
  private
    FChannel: TScSSHChannel;
  protected
    procedure SetSize(NewSize: longint); override;

  public
    constructor Create(Channel: TScSSHChannel);
    function Read(var Buffer; Count: longint): longint; override;
    function Write(const Buffer; Count: longint): longint; override;
    function Seek(Offset: longint; Origin: Word): longint; override;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TScSSHShell = class(TScSSHCustomChannel)
  private
    FExecutingCommand: boolean;
    FNonBlocking: boolean;
    FTerminalInfo: TScTerminalInfo;
    procedure SetTerminalInfo(Value: TScTerminalInfo);

  protected
    procedure SendTerminalInfo;
    procedure DoConnect; override;
    function GetConnected: boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function ExecuteCommand(const Command: string): string;
    procedure Resize;

  published
    property NonBlocking: boolean read FNonBlocking write FNonBlocking default False;
    property Environment;
    property Connected;
    property Timeout;
    property UseUnicode;
    property TerminalInfo: TScTerminalInfo read FTerminalInfo write SetTerminalInfo;
    property OnConnect;
    property OnDisconnect;
  end;

  TScSSHSubSystem = class(TScSSHCustomChannel)
  private
    FSubsystem: string;
    procedure SetSubsystem(const Value: string);
  protected
    procedure DoConnect; override;
    function GetConnected: boolean; override;
  public
  published
    property Subsystem: string read FSubsystem write SetSubsystem;
    property Connected;
    property Timeout;
    property OnConnect;
    property OnDisconnect;
  end;

  TScSSHSubSystemProcessor = class
  protected
    FChannel: TScSSHChannel;
  public
    constructor Create(Channel: TScSSHChannel);
    procedure ProcessRequest; virtual; abstract;
    procedure Init; virtual; abstract;
    procedure Close; virtual; abstract;
  end;

  TListRWFlows = class
  private
    FChannelToSocketThreads: TCRList;
    FRWThreads: TCRList;

    procedure FreeThreads(List: TCRList);
    procedure FreeFinishedThreads(List: TCRList);

  public
    constructor Create;
    destructor Destroy; override;
    procedure CreateSocketToChannelTransferring(aChannel: TScSSHChannel; vio: TCRVioTcp);
    procedure CreateReadChannelThread(aChannel: TScSSHChannel);
    procedure CreateSubSystemThread(aChannel: TScSSHChannel; aSubSystemProcessor: TScSSHSubSystemProcessor);
  {$IFDEF MSWINDOWS}
    procedure CreateShellToChannelTransferring(aChannel: TScSSHChannel; TerminalInfo: TScTerminalInfo;
      const Username, Domain, Password: string; OSAuthentication: boolean;
      const Command: string);
  {$ENDIF}
    procedure FreeAllThreads;
  end;

  TScSSHChannelUtils = class
  public
    class procedure SetChannelRequest(Obj: TScSSHCustomChannel; const ChannelRequest: TChannelRequest);
    class procedure SetOnDataFromClient(Obj: TScSSHCustomChannel; OnDataFromClient: TScData);
    class procedure SetOnDataToClient(Obj: TScSSHCustomChannel; OnDataToClient: TScData);
    class procedure SetOnChannelError(Obj: TScSSHChannel; OnChannelError: TScChannelError);
    class procedure SetBeforeChannelConnect(Obj: TScSSHChannel; BeforeChannelConnect: TScBeforeChannelConnect);
    class procedure SetAfterChannelDisconnect(Obj: TScSSHChannel; AfterChannelDisconnect: TScAfterChannelDisconnect);
    class function Readable(Obj: TScSSHCustomChannel; const DataLen: Integer; const MillisecondsTimeout: Integer): boolean;
    class procedure CloseIfDisconnected(Obj: TScSSHCustomChannel);
    class function GetStream(Obj: TScSSHCustomChannel): TSsh2Channel;
  end;

{$IFDEF MSWINDOWS}
type
  TCreateProcessWithLogonW = function (
    lpUsername: LPCWSTR;
    lpDomain: LPCWSTR;
    lpPassword: LPCWSTR;
    dwLogonFlags: DWORD;
    lpApplicationName: LPCWSTR;
    lpCommandLine: LPWSTR;
    dwCreationFlags: DWORD;
    lpEnvironment: Pointer;
    lpCurrentDirectory: LPCWSTR;
    const lpStartupInfo: TStartupInfo;
    var lpProcessInformation: TProcessInformation
  ): Boolean; stdcall;

  TCreateEnvironmentBlock = function (
    var lpEnvironment: LPWSTR; hToken: THandle; bInherit: Boolean
  ): Boolean; stdcall;

  TDestroyEnvironmentBlock = function (
    lpEnvironment: LPWSTR
  ): Boolean; stdcall;

var
  CreateProcessWithLogonW: TCreateProcessWithLogonW;
  CreateEnvironmentBlock: TCreateEnvironmentBlock;
  DestroyEnvironmentBlock: TDestroyEnvironmentBlock;
{$ENDIF}

var
  MAX_CONNECTIONS: integer = 32;

implementation

uses
{$IFDEF POSIX}
  Posix.ArpaInet, Posix.NetinetIn,
{$ENDIF}
{$IFDEF UNIX}
  baseunix, unix, netdb,
{$ENDIF}
  Math, SyncObjs,
  ScFunctions, ScThread, ScVioSocket, ScSSH2Connection;

const
  BUF_SIZE = 32 * 1024;

{ TScSSHCustomChannel }

constructor TScSSHCustomChannel.Create(AOwner: TComponent);
begin
  inherited;

  FTimeout := DEFAULT_TIMEOUT;
  FEnvironment := TStringList.Create;
  FEventsCallMode := ecAsynchronous;

  FChannelInfo := TScSSHChannelInfo.Create;
  TScSSHChannelInfoUtils.SetSSHChannel(FChannelInfo, Self);
end;

destructor TScSSHCustomChannel.Destroy;
begin
  try
    DoDisconnect;
  except
  end;

  DisposeAsyncEvent(NotifyOnAsyncReceive);

  FreeAndNil(FStream);
  FChannelInfo.Free;

  if Assigned(Client) then
    TScClientUtils.UnregisterClient(Client, Self);

  FEnvironment.Free;
  inherited;
end;

procedure TScSSHCustomChannel.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScSSHCustomChannel) then begin
    TScSSHCustomChannel(Dest).Client := Client;
    TScSSHCustomChannel(Dest).Timeout := Timeout;
    TScSSHCustomChannel(Dest).EventsCallMode := EventsCallMode;
    TScSSHCustomChannel(Dest).UseUnicode := UseUnicode;

    TScSSHCustomChannel(Dest).FEnvironment.Assign(FEnvironment);
  end
  else
    inherited;
end;

function TScSSHCustomChannel.DecodeBytes(const bytes: TBytes; index, count: Integer): string;
begin
  if FUseUnicode then
    Result := string(Encoding.UTF8.{$IFNDEF NEXTGEN}GetWideString{$ELSE}GetString{$ENDIF}(bytes, index, count))
  else
    Result := string(Encoding.Default.GetString(bytes, index, count))
end;

function TScSSHCustomChannel.EncodeString(const str: string): TBytes;
var
  wstr: WideString;
begin
  SetLength(Result, 0);

  if Length(str) > 0 then begin
    if FUseUnicode then begin
      wstr := WideString(str);
      Result := Encoding.UTF8.GetBytes(wstr);
    end
    else
      Result := Encoding.Default.GetBytes(str);
  end;
end;

function TScSSHCustomChannel.GetConnected: boolean;
begin
  Result := False;
end;

procedure TScSSHCustomChannel.SetConnected(Value: boolean);
begin
  if (csReading in ComponentState) and Value then
    FStreamedConnected := True
  else begin
    if Value and GetConnected then
      Exit;

    if Value then
      DoConnect
    else
      DoDisconnect;
  end;
end;

procedure TScSSHCustomChannel.Loaded;
begin
  inherited;

  try
    try
      if FStreamedConnected then
        SetConnected(True);
    except
      on E: Exception do
        if csDesigning in ComponentState then
          ShowException(E, ExceptAddr)
        else
          raise;
    end;
  finally
    FStreamedConnected := False;
  end;
end;

procedure TScSSHCustomChannel.OpenChannel(const ToHost: string = ''; const ToPort: Integer = -1);
var
  Connection: TSsh2Connection;
  Env, EnvName, EnvValue: string;
  i, EqPos: Integer;
  Direct: boolean;
begin
  Connection := TScClientUtils.GetConnection(Client);
  if Connection = nil then
    raise EScError.Create(seConnectionNotDefined);

  DoDisconnect;
  FreeAndNil(FStream);

  TScSSHChannelInfoUtils.SetClientInfo(FChannelInfo, Client.ClientInfo);
  TScSSHChannelInfoUtils.SetDestHost(FChannelInfo, ToHost);
  TScSSHChannelInfoUtils.SetDestPort(FChannelInfo, ToPort);

  TScSSHChannelInfoUtils.SetRemote(FChannelInfo, FChannelRequest.ChannelType = ctForwardedRemoteToLocal);
  TScSSHChannelInfoUtils.SetIsSession(FChannelInfo, (FChannelRequest.ChannelType = ctSession) or ((ToHost = '') and (ToPort = -1)));
  TScSSHChannelInfoUtils.SetDirect(FChannelInfo, False);
  if Assigned(FBeforeChannelConnect) then begin
    Direct := False;
    FBeforeChannelConnect(Self, FChannelInfo, Direct);
    TScSSHChannelInfoUtils.SetDirect(FChannelInfo, Direct);
  end;

  if FEventsCallMode = ecAsynchronous then
    CheckIfAsyncEventProcessorStarted;

  if FChannelRequest.ChannelType <> ctForwardedLocalToRemote then
    FStream := Connection.ConfirmChannel(FChannelRequest)
  else
  if (ToHost = '') and (ToPort = -1) then
    FStream := Connection.OpenSession
  else
    FStream := Connection.ForwardPort(ToHost, ToPort);

  FStream.OnReceiveData := DoReceiveData;
  FStream.OnDisconnect := DoAfterChannelDisconnect;

  if (Int64(Timeout) * 1000) > MaxInt then
    FStream.Timeout := MaxInt
  else
    FStream.Timeout := Timeout * 1000;

  // wait while channel becomes connected
  if not FStream.WaitForReady(FStream.Timeout) then
    raise EScError.Create(seConnectionTimeout);

  if not FStream.Ready then
    raise EScError.CreateFmt(FStream.LastErrorMessage, [], seOpenChannelError);

  for i := 0 to FEnvironment.Count - 1 do begin
    Env := FEnvironment[i];
    EqPos := Pos('=', Env);
    if EqPos > 0 then begin
      EnvName := Trim(Copy(Env, 1, EqPos - 1));
      EnvValue := Trim(Copy(Env, EqPos + 1, Length(Env)));
    end
    else begin
      EnvName := Trim(Env);
      EnvValue := '';
    end;

    if EnvName <> '' then
      FStream.SendEnvironmentVariable(EnvName, EnvValue);
  end;
end;

procedure TScSSHCustomChannel.DoConnect;
begin
  if Assigned(OnConnect) then
    OnConnect(Self);

  if Client = nil then
    raise EScError.Create(seClientNotDefined);

  Client.Connect;
end;

procedure TScSSHCustomChannel.DoDisconnect;
begin
  if FStream = nil then
    Exit;

  try
    FStream.Close;
    FStream.Dispose;
  except
    on e: Exception do
      if e.ClassName <> SocketException.ClassName then
        raise;
  end;
end;

procedure TScSSHCustomChannel.Connect;
begin
  SetConnected(True);
end;

procedure TScSSHCustomChannel.Disconnect;
begin
  SetConnected(False);
end;

function TScSSHCustomChannel.Readable(const DataLen: Integer; const MillisecondsTimeout: Integer = MaxInt): boolean;
begin
  Result := False;
  if FStream = nil then
    Exit;

  Result := FStream.Readable(DataLen, MillisecondsTimeout);
end;

function TScSSHCustomChannel.GetRemainedWindowSize: integer;
begin
  if FStream = nil then
    Result := 0
  else
    Result := FStream.GetRemainedWindowSize;
end;

function TScSSHCustomChannel.GetExitStatusCode: integer;
begin
  if FStream = nil then
    Result := 0
  else
    Result := FStream.ExitStatusCode;
end;

procedure TScSSHCustomChannel.CloseIfDisconnected;
begin
  if FStream = nil then
    Exit;

  if FStream.IsEOF and (GetInCount = 0) then
    DoDisconnect;
end;

function TScSSHCustomChannel.DirectWrite(const Buffer: TValueArr; Offset, Count: integer): integer;
begin
  if Buffer = nil then
    raise EScError.Create(seInvalidInputArgs);

  Result := 0;
  if not GetConnected then
    Exit;

  if not FStream.Ready then
    Exit; //channel not ready

  if Assigned(OnDataToClient) then
    OnDataToClient(Self, FChannelInfo, TBytes(Buffer), Offset, Count);

  Result := FStream.Transmit(Buffer, Offset, Count);
end;

procedure TScSSHCustomChannel.CheckReadingAbility;
begin
  // None
end;

procedure TScSSHCustomChannel.CheckWritingAbility;
begin
  // None
end;

function TScSSHCustomChannel.InternalReadBuffer(const Buffer: TValueArr; const Offset, Count: integer): integer;
begin
  CheckReadingAbility;

  if Buffer = nil then
    raise EScError.Create(seInvalidInputArgs);

  Result := 0;
  if (FStream = nil) or (Count = 0) then
    Exit;

  Readable(Count, FStream.Timeout);
  if GetInCount > 0 then begin
    Result := FStream.Read(Buffer, Offset, Count);
    if Assigned(OnDataFromClient) then
      OnDataFromClient(Self, FChannelInfo, TBytes(Buffer), Offset, Result);
  end;
end;

function TScSSHCustomChannel.InternalReadNoWait(const Buffer: TValueArr; const Offset, Count: integer): integer;
begin
  CheckReadingAbility;

  if Buffer = nil then
    raise EScError.Create(seInvalidInputArgs);

  Result := 0;
  if (FStream = nil) or (Count = 0) then
    Exit;

  Readable(1, FStream.Timeout);
  if GetInCount > 0 then begin
    Result := FStream.Read(Buffer, Offset, Count);
    if Assigned(OnDataFromClient) then
      OnDataFromClient(Self, FChannelInfo, TBytes(Buffer), Offset, Result);
  end;
end;

function TScSSHCustomChannel.InternalReadNoWait(Stream: TStream): integer;
begin
  CheckReadingAbility;

  if Stream = nil then
    raise EScError.Create(seInvalidInputArgs);

  Result := 0;
  if FStream = nil then
    Exit;

  Readable(1, FStream.Timeout);
  if GetInCount > 0 then begin
    Result := FStream.Read(Stream);
    if Assigned(OnDataFromClient) then
      OnDataFromClient(Self, FChannelInfo, nil, Stream.Position - Result, Result);
  end;
end;

function TScSSHCustomChannel.InternalWriteBuffer(const Buffer: TValueArr; const Offset, Count: integer): integer;
begin
  CheckWritingAbility;
  Result := DirectWrite(Buffer, Offset, Count);
end;

function TScSSHCustomChannel.ReadBuffer(var Buffer; const Count: integer): integer;
begin
  Result := InternalReadBuffer(@Buffer, 0, Count);
end;

function TScSSHCustomChannel.ReadBuffer(var Buffer: TBytes; const Offset, Count: integer): integer;
begin
  Result := InternalReadBuffer(TValueArr(Buffer), Offset, Count);
end;

function TScSSHCustomChannel.ReadNoWait(var Buffer; const Count: integer): integer;
begin
  Result := InternalReadNoWait(@Buffer, 0, Count);
end;

function TScSSHCustomChannel.ReadNoWait(var Buffer: TBytes; const Offset, Count: integer): integer;
begin
  Result := InternalReadNoWait(TValueArr(Buffer), Offset, Count);
end;

function TScSSHCustomChannel.ReadNoWait(Stream: TStream): integer;
begin
  Result := InternalReadNoWait(Stream);
end;

function TScSSHCustomChannel.ReadAllData(var Buffer: TBytes): integer;
begin
  CheckReadingAbility;

  Result := 0;
  if FStream = nil then
    Exit;

  Readable(1);
  if GetInCount > 0 then begin
    Result := FStream.Read(TValueArr(Buffer), 0, Min(GetInCount, Length(Buffer)));
    if Assigned(OnDataFromClient) then
      OnDataFromClient(Self, FChannelInfo, Buffer, 0, Result);
  end;
end;

function TScSSHCustomChannel.SkipBuffer(Count: integer): integer;
begin
  CheckReadingAbility;

  Result := 0;
  if (FStream = nil) or (Count = 0) then
    Exit;

  Readable(1, FStream.Timeout);
  if GetInCount > 0 then
    Result := FStream.Read(nil, 0, Count);
end;

function TScSSHCustomChannel.ReadString: string;
var
  buf: TBytes;
  Read: Integer;
begin
  if GetInCount = 0 then begin
    Result := '';
    Exit;
  end;

  SetLength(buf, GetInCount);
  Read := ReadBuffer(buf, 0, Length(buf));
  Result := DecodeBytes(buf, 0, Read);
end;

function TScSSHCustomChannel.WriteBuffer(const Buffer; const Count: integer): integer;
begin
  Result := InternalWriteBuffer(@Buffer, 0, Count);
end;

function TScSSHCustomChannel.WriteBuffer(const Buffer: TBytes; const Offset, Count: integer): integer;
begin
  Result := InternalWriteBuffer(TValueArr(Buffer), Offset, Count);
end;

procedure TScSSHCustomChannel.WriteString(const Buffer: string);
var
  buf: TBytes;
  Wrote: Integer;
begin
  buf := EncodeString(Buffer);
  Wrote := WriteBuffer(buf, 0, Length(buf));
  if Wrote <> Length(buf) then
    raise EScError.Create(seCannotSendData);
end;

{$IFNDEF FPC}
{$IFDEF VER22P}
function TScSSHCustomChannel.Read(pv: IntPtr; cb: FixedUInt; pcbRead: PFixedUInt): HResult;
{$ELSE}
function TScSSHCustomChannel.Read(pv: IntPtr; cb: Longint; pcbRead: PLongint): HResult;
{$ENDIF}
{$ELSE}
function TScSSHCustomChannel.Read(pv: Pointer; cb: DWord; pcbRead: PDWord): HRESULT;
{$ENDIF}
var
  cbReadBytes: integer;
begin
  try
    cbReadBytes := ReadBuffer(pv^, cb);
    if pcbRead <> nil then
      Marshal.WriteInt32(pcbRead, cbReadBytes);
    Result := S_OK;
  except
    Result := S_FALSE;
  end;
end;

{$IFNDEF FPC}
{$IFDEF VER22P}
function TScSSHCustomChannel.Write(pv: IntPtr; cb: FixedUInt; pcbWritten: PFixedUInt): HResult;
{$ELSE}
function TScSSHCustomChannel.Write(pv: IntPtr; cb: Longint; pcbWritten: PLongint): HResult;
{$ENDIF}
{$ELSE}
function TScSSHCustomChannel.Write(pv: Pointer; cb: DWord; pcbWritten: PDWord): HRESULT;
{$ENDIF}
var
  cbWriteBytes: integer;
begin
  try
    cbWriteBytes := WriteBuffer(pv^, cb);
    if pcbWritten <> nil then
      Marshal.WriteInt32(pcbWritten, cbWriteBytes);
    Result := S_OK;
  except
    Result := S_FALSE;
  end;
end;

procedure TScSSHCustomChannel.CheckInactive;
begin
  if Connected then
    raise EScError.Create(seClientOpened);
end;

procedure TScSSHCustomChannel.SetClient(Value: TScSSHClient);
begin
  if Value <> FClient then begin
    if FClient <> nil then
      TScClientUtils.UnregisterClient(FClient, Self);

    SetConnected(False);
    FClient := Value;

    if FClient <> nil then
      TScClientUtils.RegisterClient(Value, Self, nil);
  end;
end;

procedure TScSSHCustomChannel.SetTimeout(const Value: integer);
begin
  if Value <> FTimeout then begin
    FTimeout := Value;
    if FStream <> nil then
      if (Int64(Value) * 1000) > MaxInt then
        FStream.Timeout := MaxInt
      else
        FStream.Timeout := Value * 1000;
  end;
end;

function TScSSHCustomChannel.GetInCount: Integer;
begin
  CheckReadingAbility;

  if FStream <> nil then
    Result := FStream.DataLength
  else
    Result := 0;
end;

function TScSSHCustomChannel.GetOutCount: Integer;
begin
  CheckWritingAbility;

  Result := 0;
end;

procedure TScSSHCustomChannel.DoReceiveData(Sender: TObject);
begin
  if Assigned(OnAsyncReceive) then
    case FEventsCallMode of
      ecDirectly:
        NotifyOnAsyncReceive;
      ecAsynchronous:
        HandleEventAsync(NotifyOnAsyncReceive);
      ecSynchronous:
        SynchronizeWithMainThread(NotifyOnAsyncReceive);
    else
      Assert(False);
    end;
end;

procedure TScSSHCustomChannel.NotifyOnAsyncReceive;
var
  Fail: boolean;
begin
  try
    if Assigned(FOnAsyncReceive) then
      FOnAsyncReceive(Self);
  except
    on E: Exception do
      if not (E is EAbort) then begin
        Fail := True;
        DoAsyncError(E, Fail);
        if Fail then
          raise;
      end;
  end;
end;

procedure TScSSHCustomChannel.DoAsyncError(E: Exception; var Fail: boolean);
begin
  Fail := False;
  if Assigned(FOnAsyncError) then
    FOnAsyncError(Self, E);
end;

procedure TScSSHCustomChannel.DoAfterChannelDisconnect(Sender: TObject);
begin
  if Assigned(FOnDisconnect) then
    FOnDisconnect(Self);
  if Assigned(FAfterChannelDisconnect) then
    FAfterChannelDisconnect(Self, FChannelInfo);
end;

procedure TScSSHCustomChannel.SetEnvironment(Value: TStrings);
begin
  if not Value.Equals(FEnvironment) then begin
    CheckInactive;
    FEnvironment.Assign(Value);
  end;
end;

procedure TScSSHCustomChannel.SetEventsCallMode(Value: TScEventCallMode);
begin
  if Value <> FEventsCallMode then begin
    CheckInactive;
    FEventsCallMode := Value;
  end;
end;

{ TScSSHChannel }

constructor TScSSHChannel.Create(AOwner: TComponent);
begin
  inherited;

  FIPVersion := DefValIPVersion;
  FListLPFThreads := TListRWFlows.Create;
end;

destructor TScSSHChannel.Destroy;
begin
  inherited;

  FListLPFThreads.Free;
end;

procedure TScSSHChannel.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScSSHChannel) then begin
    TScSSHChannel(Dest).FSourcePort := FSourcePort;
    TScSSHChannel(Dest).FDestHost := FDestHost;
    TScSSHChannel(Dest).FDestPort := FDestPort;
    TScSSHChannel(Dest).FRemote := FRemote;
    TScSSHChannel(Dest).FDirect := FDirect;
    TScSSHChannel(Dest).FDynamic := FDynamic;
    TScSSHChannel(Dest).FGatewayPorts := FGatewayPorts;
    TScSSHChannel(Dest).FIPVersion := FIPVersion;
  end;

  inherited;
end;

function TScSSHChannel.GetConnected: boolean;
begin
  if FDirect then begin
    Result := (FStream <> nil) and (not FStream.IsClosed);
    if Result and not FStream.Ready then
      if not FStream.WaitForReady(FStream.Timeout) or not FStream.Ready then
        Result := False;
  end
  else begin
    if not FRemote then
      Result := (FListenLPFThread <> nil) and
        not FListenLPFThread.Suspended and
        not FListenLPFThread.Finished
    else
      Result := FRemoteConnected;
  end;
end;

procedure TScSSHChannel.DoConnect;

  procedure LocalPFConnect;
  var
    LocalHost: string;
    ThreadEvents: TScTCPServerThreadEvents;
  begin
    if GatewayPorts then
      LocalHost := '' // will be set in FListenLPFThread on Binding
    else
      LocalHost := LOCAL_HOST;

    FreeAndNil(FListenLPFThread);

    ThreadEvents.BeforeBind := nil;
    ThreadEvents.BeforeExecute := nil;
    ThreadEvents.AfterExecute := AfterLPFThreadExecute;
    ThreadEvents.AfterAccept := AcceptSocketConnection;
    ThreadEvents.OnCheckIfStopListen := CheckIfLPFStopped;
    ThreadEvents.OnException := DoError;
    FListenLPFThread := TScTCPServerThread.Create(ThreadEvents, LocalHost, SourcePort,
      FIPVersion, Timeout, True, 3, 0);
    FListenLPFThread.StartListen;

    while not GetConnected do;
  end;

  procedure RemotePFConnect;
  var
    Connection: TSsh2Connection;
    LocalHost: string;
  begin
    Connection := TScClientUtils.GetConnection(Client);
    if Connection = nil then
      raise EScError.Create(seConnectionNotDefined);

    if FIPVersion = ivIPv6 then
      LocalHost := '::'
    else
      LocalHost := '0.0.0.0';

    if not Connection.OpenForwardedPort(LocalHost, SourcePort, DestHost, DestPort, Timeout * 1000) then
      raise EScError.Create(seCannotListenPF);

    FRemoteConnected := True;
  end;

begin
  inherited;

  if FDirect and FRemote then
    raise EScError.Create(seRemoteAndDirectSet);
  if FDirect and FDynamic then
    raise EScError.Create(seDynamicAndDirectSet);
  if FRemote and FDynamic then
    raise EScError.Create(seRemoteAndDynamicSet);

  if FDirect then
    OpenChannel(FDestHost, FDestPort)
  else
  if not FRemote then
    LocalPFConnect
  else
    RemotePFConnect;
end;

procedure TScSSHChannel.DoDisconnect;
var
  Connection: TSsh2Connection;
begin
  if FDirect then begin
    inherited DoDisconnect;
  end
  else begin
    if not FRemote then begin
      if FListenLPFThread <> nil then begin
        FListenLPFThread.Terminate;
        FListenLPFThread.ListenSocket.Close;
        FreeAndNil(FListenLPFThread);
        DoAfterChannelDisconnect(Self);
      end;
    end
    else begin
      if FRemoteConnected then begin
        FRemoteConnected := False;

        if Client <> nil then begin
          Connection := TScClientUtils.GetConnection(Client);
          if Connection <> nil then
            Connection.CancelForwardedPort(Client.HostName, SourcePort);
        end;
        DoAfterChannelDisconnect(Self);
      end;
    end;
  end;
end;

procedure TScSSHChannel.AcceptSocketConnection(Sender: TObject; Vio: TCRVioTcp);
var
  NewChannel: TScSSHChannel;
  Hostname: string;
  Port: integer;
begin
  FListLPFThreads.FreeFinishedThreads(FListLPFThreads.FChannelToSocketThreads);
  NewChannel := nil;

  try
    DoSocketConnect(Self, Vio.RemoteSockAddr);

    if (FListLPFThreads.FChannelToSocketThreads.Count >= MAX_CONNECTIONS) or not Client.Connected then begin
      try
        DoSocketDisconnect(Self, Vio.RemoteSockAddr);
      finally
        Vio.Free;
        Vio := nil;
      end;
      Exit;
    end;

    if FDynamic then begin
      Vio.ReadSocksRequest(Hostname, Port);
    end
    else begin
      Hostname := DestHost;
      Port := DestPort;
    end;

    NewChannel := Client.CreateSSHChannel as TScSSHChannel;
    NewChannel.Direct := True;
    NewChannel.DestHost := Hostname;
    NewChannel.DestPort := Port;
    NewChannel.IPVersion := IPVersion;
    NewChannel.Timeout := Timeout;
    NewChannel.Client := Client;
    NewChannel.FOnChannelError := FOnChannelError;
    NewChannel.Connected := True;
    NewChannel.OnSocketDisconnect := DoSocketDisconnect;

    if FDynamic then
      Vio.WriteSocksResponse(Hostname, Port);
  except
    DoSocketDisconnect(Self, Vio.RemoteSockAddr);

    Vio.Free;
    NewChannel.Free;
    raise;
  end;

  FListLPFThreads.CreateSocketToChannelTransferring(NewChannel, Vio);
end;

procedure TScSSHChannel.AfterLPFThreadExecute(Sender: TObject);
begin
  FListLPFThreads.FreeAllThreads;
end;

procedure TScSSHChannel.CheckIfLPFStopped(Sender: TObject; var NeedStop: boolean);
begin
  NeedStop := not Client.Connected;
end;

function TScSSHChannel.GetListenedPort: integer;
begin
  if FListenLPFThread = nil then
    Result := -1
  else
    Result := FListenLPFThread.ListenSocket.GetLocalPort;
end;

procedure TScSSHChannel.CheckReadingAbility;
begin
  if not Direct then
    raise EScError.Create(seNotDirectChannel);
end;

procedure TScSSHChannel.CheckWritingAbility;
begin
  if not Direct then
    raise EScError.Create(seNotDirectChannel);
end;

function TScSSHChannel.GetSSHStream: TScSSHStream;
begin
  if not Direct then
    raise EScError.Create(seNotDirectChannel);

  if FSSHStream = nil then
    FSSHStream := TScSSHStream.Create(Self);

  Result := FSSHStream;
end;

procedure TScSSHChannel.SetSourcePort(const Value: integer);
begin
  if Value <> FSourcePort then begin
    CheckInactive;
    FSourcePort := Value;
  end;
end;

procedure TScSSHChannel.SetDestHost(const Value: string);
begin
  if Value <> FDestHost then begin
    CheckInactive;
    FDestHost := Value;
  end;
end;

procedure TScSSHChannel.SetDestPort(const Value: integer);
begin
  if Value <> FDestPort then begin
    CheckInactive;
    FDestPort := Value;
  end;
end;

procedure TScSSHChannel.SetRemote(const Value: boolean);
begin
  if Value <> FRemote then begin
    CheckInactive;
    FRemote := Value;
  end;
end;

procedure TScSSHChannel.SetDirect(const Value: boolean);
begin
  if Value <> FDirect then begin
    CheckInactive;
    FDirect := Value;
  end;
end;

procedure TScSSHChannel.SetDynamic(const Value: boolean);
begin
  if Value <> FDynamic then begin
    CheckInactive;
    FDynamic := Value;
  end;
end;

procedure TScSSHChannel.SetGatewayPorts(const Value: boolean);
begin
  if Value <> FGatewayPorts then begin
    CheckInactive;
    FGatewayPorts := Value;
  end;
end;

procedure TScSSHChannel.SetIPVersion(const Value: TIPVersion);
begin
  if Value <> FIPVersion then begin
    CheckInactive;
    FIPVersion := Value;
  end;
end;

procedure TScSSHChannel.DoError(Sender: TObject; E: Exception);
begin
  if Assigned(FOnChannelError) then
    FOnChannelError(Self, FChannelInfo, E)
  else
  if Assigned(FOnError) then
    FOnError(Self, E);
end;

procedure TScSSHChannel.DoSocketConnect(Sender: TObject; const SockAddr: PSockAddr);
begin
  if Assigned(FOnSocketConnect) then
    FOnSocketConnect(Self, SockAddr);
end;

procedure TScSSHChannel.DoSocketDisconnect(Sender: TObject; const SockAddr: PSockAddr);
begin
  if Assigned(FOnSocketDisconnect) then
    FOnSocketDisconnect(Self, SockAddr);
end;

{ TScSSHStream }

constructor TScSSHStream.Create(Channel: TScSSHChannel);
begin
  inherited Create;

  if (Channel = nil) or (Channel.Direct = False) then
    raise EScError.Create(seNotDirectChannel);

  FChannel := Channel;
end;

function TScSSHStream.Read(var Buffer; Count: longint): longint;
begin
  Result := FChannel.ReadBuffer(Buffer, Count);
end;

function TScSSHStream.Write(const Buffer; Count: longint): longint;
begin
  Result := FChannel.WriteBuffer(Buffer, Count);
end;

function TScSSHStream.Seek(Offset: longint; Origin: Word): longint;
begin
  raise Exception.Create('Seek not implemented');
{$IFDEF FPC}
  Result := 0;
{$ENDIF}
end;

procedure TScSSHStream.SetSize(NewSize: longint);
begin
  raise Exception.Create('SetSize not implemented');
end;

{ TScSSHShell }

constructor TScSSHShell.Create(AOwner: TComponent);
begin
  inherited;
  FTerminalInfo := TScTerminalInfo.Create;
end;

destructor TScSSHShell.Destroy;
begin
  FTerminalInfo.Free;
  inherited;
end;

function TScSSHShell.GetConnected: boolean;
begin
  Result := (FStream <> nil) and (not FStream.IsClosed);
end;

procedure TScSSHShell.DoConnect;
begin
  inherited;

  OpenChannel;
  SendTerminalInfo;

  if not FExecutingCommand then
    FStream.SendShell;
end;

procedure TScSSHShell.SendTerminalInfo;
begin
  if FTerminalInfo.TerminalType <> '' then
    FStream.SendTerminalReq(FTerminalInfo);
end;

procedure TScSSHShell.Resize;
begin
  if Connected then
    FStream.SendWindowChangeReq(FTerminalInfo);
end;

function TScSSHShell.ExecuteCommand(const Command: string): string;
var
  ms: MemoryStream;
  Buf: TBytes;
  Read: Integer;
begin
  SetConnected(False);

  FExecutingCommand := True;
  try
    DoConnect;
  finally
    FExecutingCommand := False;
  end;

  FStream.SendExec(Command);

  if FNonBlocking then begin
    Result := '';
  end
  else begin
    SetLength(Buf, 1024);
    ms := MemoryStream.Create(0);
    try
      while not FStream.IsClosed or (Readable(1) and (GetInCount > 0)) do begin
        Read := ReadAllData(Buf);
        ms.Write(Buf, 0, Read);
      end;

      Result := DecodeBytes(ms.ToArray, 0, ms.Position);
    finally
      ms.Free;
    end;
  end;
end;

procedure TScSSHShell.SetTerminalInfo(Value: TScTerminalInfo);
begin
  if Value <> FTerminalInfo then begin
    FTerminalInfo.Assign(Value);

    if Connected then
      SendTerminalInfo;
  end;
end;

{ TScSSHSubSystem }

function TScSSHSubSystem.GetConnected: boolean;
begin
  Result := (FStream <> nil) and (not FStream.IsClosed);
end;

procedure TScSSHSubSystem.DoConnect;
begin
  inherited;

  OpenChannel;
  FStream.SendSubsystem(FSubsystem);
end;

procedure TScSSHSubSystem.SetSubsystem(const Value: string);
begin
  if Value <> FSubsystem then begin
    CheckInactive;
    FSubsystem := Value;
  end;
end;

{ TScSSHSubSystemProcessor }

constructor TScSSHSubSystemProcessor.Create(Channel: TScSSHChannel);
begin
  inherited Create;
  FChannel := Channel;
end;

{$IFDEF MSWINDOWS}
{ TConsoleApp }

type
  TConsoleApp = class
  private
    FhProcess: cardinal;
    FhStdInRead: THandle;
    FhStdInWrite: THandle;
    FhStdOutRead: THandle;
    FhStdOutWrite: THandle;
    FhStdErrRead: THandle;
    FhStdErrWrite: THandle;
    FhToken: THandle;
    FExitCode: cardinal;

    FStartDir: string;
    FOldDir: string;
    FCommandLine: WideString;
    FUserName: WideString;
    FDomain: WideString;
    FPassword: WideString;
    FOSAuthentication: boolean;
    FIsShell: boolean;

    procedure CreatePipes;
  public
    destructor Destroy; override;

    procedure Open(TerminalInfo: TScTerminalInfo);
    function IsActive: boolean;
    procedure Close;

    function HasPendingOutput: boolean;
    function HasPendingErr: boolean;
    function ReadOutput(var Buffer: TBytes; MaxCount: cardinal): cardinal;
    function ReadErr(var Buffer: TBytes; MaxCount: cardinal): cardinal;

    function HasPendingInput: boolean;
    function Write(const Buffer: TBytes; Count: integer): cardinal;
    procedure GetExitCode(out ExitCode: cardinal);
    procedure WaitForClose;

    property IsShell: boolean read FIsShell write FIsShell;
    property CommandLine: WideString read FCommandLine write FCommandLine;
    property UserName: WideString read FUserName write FUserName;
    property Domain: WideString read FDomain write FDomain;
    property Password: WideString read FPassword write FPassword;
    property OSAuthentication: boolean read FOSAuthentication write FOSAuthentication;
    property StartDir: string read FStartDir write FStartDir;
  end;
{$ENDIF}

type
  TRWHandler = class;

  TRWThread = class(TThread)
  private
    FLinkedThread: TRWThread;
    FHandler: TRWHandler;

  protected
    procedure Execute; override;
  public
    constructor Create(Handler: TRWHandler; LinkedThread: TRWThread);
    destructor Destroy; override;
  end;

  TRWHandler = class
  protected
    FChannel: TScSSHChannel;
    FBuffer: TBytes;

    procedure CheckObjects; virtual;
    procedure Initialize; virtual;
    function Connected: boolean; virtual;
    function WaitForData: boolean; virtual;
    function ReadData: integer; virtual; abstract;
    procedure WriteData(Count: integer); virtual; abstract;
    procedure ProcessError(const E: Exception); virtual;
    procedure Close; virtual;
    procedure TryTerminate; virtual;
    procedure Disconnect; virtual;

  public
    constructor Create(aChannel: TScSSHChannel);
  end;

  TSocketToChannelTransfer = class(TRWHandler)
  private
    FClosed: boolean;
    FVio: TCRVioTcp;

  protected
    procedure CheckObjects; override;
    function Connected: boolean; override;
    function WaitForData: boolean; override;
    function ReadData: integer; override;
    procedure WriteData(Count: integer); override;
    procedure ProcessError(const E: Exception); override;
    procedure TryTerminate; override;
    procedure Disconnect; override;

  public
    constructor Create(aChannel: TScSSHChannel; vio: TCRVioTcp);
  end;

  TChannelToSocketTransfer = class(TRWHandler)
  private
    FVio: TCRVioTcp;

  protected
    procedure CheckObjects; override;
    function Connected: boolean; override;
    function ReadData: integer; override;
    procedure WriteData(Count: integer); override;
    procedure TryTerminate; override;

  public
    constructor Create(aChannel: TScSSHChannel; vio: TCRVioTcp);
    destructor Destroy; override;
  end;

  TReadChannelHandler = class(TRWHandler)
  protected
    function Connected: boolean; override;
    function ReadData: integer; override;
    procedure WriteData(Count: integer); override;
    procedure TryTerminate; override;

  public
    destructor Destroy; override;
  end;

  TSubSystemHandler = class(TRWHandler)
  private
    FSubSystemProcessor: TScSSHSubSystemProcessor;

  protected
    procedure CheckObjects; override;
    procedure Initialize; override;
    function Connected: boolean; override;
    function WaitForData: boolean; override;
    function ReadData: integer; override;
    procedure WriteData(Count: integer); override;
    procedure TryTerminate; override;
    procedure Disconnect; override;

  public
    constructor Create(aChannel: TScSSHChannel; aSubSystemProcessor: TScSSHSubSystemProcessor);
    destructor Destroy; override;
  end;

{$IFDEF MSWINDOWS}
  TShellToChannelTransfer = class(TRWHandler)
  private
    FShell: TConsoleApp;
    FTerminalInfo: TScTerminalInfo;

  protected
    procedure CheckObjects; override;
    procedure Initialize; override;
    function Connected: boolean; override;
    function WaitForData: boolean; override;
    function ReadData: integer; override;
    procedure WriteData(Count: integer); override;
    procedure Close; override;
    procedure TryTerminate; override;
    procedure Disconnect; override;

  public
    constructor Create(aChannel: TScSSHChannel; aShell: TConsoleApp; aTerminalInfo: TScTerminalInfo);
    destructor Destroy; override;
  end;

  TChannelToShellTransfer = class(TRWHandler)
  private
    FShell: TConsoleApp;

  protected
    procedure CheckObjects; override;
    function Connected: boolean; override;
    function ReadData: integer; override;
    procedure WriteData(Count: integer); override;
    procedure TryTerminate; override;
    procedure Disconnect; override;

  public
    constructor Create(aChannel: TScSSHChannel; aShell: TConsoleApp);
  end;
{$ENDIF}

{$IFDEF MSWINDOWS}
{ TConsoleApp }

destructor TConsoleApp.Destroy;
begin
  Close;

  inherited;
end;

procedure TConsoleApp.Open(TerminalInfo: TScTerminalInfo);
var
  StartupInfo: TStartupInfo;
  ProcessInformation: TProcessInformation;
  pUserName, pDomain, pPassword, pCommandLine: IntPtr;
  Env: LPWSTR;
begin
  if IsActive then
    Exit;

  CreatePipes;
  FillChar(StartupInfo, SizeOf(StartupInfo), 0);

  with StartupInfo do begin
    cb := SizeOf(TStartupInfo);
    lpReserved := nil;
    lpDesktop := nil;
    lpTitle := nil;
    dwX := 0;
    dwY := 0;
    dwXCountChars := TerminalInfo.Cols;
    dwYCountChars := TerminalInfo.Rows;
    dwXSize := TerminalInfo.Width;
    dwYSize := TerminalInfo.Height;
    dwFillAttribute := 0;

    if (dwXCountChars = 0) or (dwYCountChars = 0) then
      dwFlags := STARTF_USESIZE
    else
      dwFlags := STARTF_USECOUNTCHARS;

    dwFlags := dwFlags + STARTF_USESHOWWINDOW + STARTF_USESTDHANDLES;
    wShowWindow := 0;
    cbReserved2 := 0;
    lpReserved2 := nil;
    hStdInput := FhStdInRead;
    hStdOutput := FhStdOutWrite;
    hStdError := FhStdErrWrite;
  end;

  if FStartDir <> '' then begin
    FOldDir := GetCurrentDir;
    Win32Check(SetCurrentDir(FStartDir));
  end
  else
    FOldDir := '';

  FhToken := 0;

  if Domain = '' then
    pDomain := nil
  else
    pDomain := Marshal.StringToHGlobalUni(Domain);
  pUserName := Marshal.StringToHGlobalUni(UserName);
  pPassword := Marshal.StringToHGlobalUni(Password);
  pCommandLine := Marshal.StringToHGlobalUni(FCommandLine);
  try
    Env := nil;

    if not OSAuthentication then
      Win32Check(CreateProcessW(nil, pCommandLine, nil, nil, True, // must be true, because pipe handles must be inherited!
        CREATE_UNICODE_ENVIRONMENT or CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
        Env, nil, {$IFDEF FPC}@{$ELSE}{$IFDEF VER10P}{$IFNDEF VER12P}TStartupInfoW{$ENDIF}{$ENDIF}{$ENDIF}(StartupInfo),
        {$IFDEF FPC}@{$ENDIF}ProcessInformation))
    else
    if not Assigned(CreateProcessWithLogonW) then begin
      Win32Check(LogonUserW(pUserName, pDomain, pPassword,
        LOGON32_LOGON_INTERACTIVE, LOGON32_PROVIDER_DEFAULT, FhToken));

      if Assigned(CreateEnvironmentBlock) then
        Win32Check(CreateEnvironmentBlock(Env, FhToken, True));

      try
        Win32Check(CreateProcessAsUserW(FhToken, nil, pCommandLine, nil, nil, True, // must be true, because pipe handles must be inherited!
          CREATE_UNICODE_ENVIRONMENT or CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
          Env, nil, {$IFDEF FPC}@{$ENDIF}StartupInfo, {$IFDEF FPC}@{$ENDIF}ProcessInformation));
      finally
        if Assigned(DestroyEnvironmentBlock) and (Env <> nil) then
          DestroyEnvironmentBlock(Env);
      end;
    end
    else begin
      Win32Check(CreateProcessWithLogonW(pUserName, pDomain, pPassword, 0,
        nil, pCommandLine,
        CREATE_UNICODE_ENVIRONMENT or CREATE_NEW_CONSOLE or NORMAL_PRIORITY_CLASS,
        nil, nil, StartupInfo, ProcessInformation));
    end;
  finally
    Marshal.FreeCoTaskMem(pDomain);
    Marshal.FreeCoTaskMem(pUserName);
    Marshal.FreeCoTaskMem(pPassword);
    Marshal.FreeCoTaskMem(pCommandLine);
    ZeroMemory(IntPtr(Password), Length(Password) * sizeof(WideChar));
  end;

  FhProcess := ProcessInformation.hProcess;
  FExitCode := 0;
end;

procedure TConsoleApp.CreatePipes;
var
  Attrs: TSecurityAttributes;
  hPipe: THandle;
begin
  Attrs.nLength := SizeOf(TSecurityAttributes);
  Attrs.lpSecurityDescriptor := nil;
  Attrs.bInheritHandle := true;

  // create stdout pipe
  if not CreatePipe(hPipe, FhStdOutWrite, @Attrs, 0) then
    raise Exception.Create('Cannot create STDOUT pipe');

  // create noninheritable STDOUT read handle and close inheritable original one
  if not DuplicateHandle(GetCurrentProcess, hPipe, GetCurrentProcess,
    @FhStdOutRead, 0, False, DUPLICATE_SAME_ACCESS)
  then
    raise Exception.Create('Cannot create STDOUT pipe');
  CloseHandle(hPipe);

  // create stdErr pipe
  if not CreatePipe(hPipe, FhStdErrWrite, @Attrs, 0) then
    raise Exception.Create('Cannot create STDErr pipe');

  // create noninheritable STDErr read handle and close inheritable original one
  if not DuplicateHandle(GetCurrentProcess, hPipe, GetCurrentProcess,
    @FhStdErrRead, 0, False, DUPLICATE_SAME_ACCESS)
  then
    raise Exception.Create('Cannot create STDErr pipe');
  CloseHandle(hPipe);

  // create stdin pipe
  if not CreatePipe(FhStdInRead, hPipe, @Attrs, 0) then
    raise Exception.Create('Cannot create STDIN pipe');

  // create noninheritable STDIN write handle and close inheritable original one
  if not DuplicateHandle(GetCurrentProcess, hPipe, GetCurrentProcess,
    @FhStdInWrite, 0, False, DUPLICATE_SAME_ACCESS)
  then
    raise Exception.Create('Cannot create STDIN pipe');
  CloseHandle(hPipe);
end;

function TConsoleApp.IsActive: boolean;
var
  ExitCode: cardinal;
begin
  Result := (FhProcess <> 0) and GetExitCodeProcess(FhProcess, ExitCode) and
    (ExitCode = STILL_ACTIVE);
end;

procedure TConsoleApp.Close;
begin
  if FhProcess = 0 then
    Exit;

  GetExitCodeProcess(FhProcess, FExitCode);
  if IsActive then
    TerminateProcess(FhProcess, 0);

  CloseHandle(FhProcess);
  FhProcess := 0;

  if FhToken <> 0 then
    CloseHandle(FhToken);

  if FOldDir <> '' then begin
    Win32Check(SetCurrentDir(FOldDir));
    FOldDir := '';
  end;

  // close stdin handles
  if FhStdInRead <> 0 then begin
    CloseHandle(FhStdInRead);
    FhStdInRead := 0;
    CloseHandle(FhStdInWrite);
    FhStdInWrite := 0;
  end;

  // close stdout handles
  if FhStdOutRead <> 0 then begin
    CloseHandle(FhStdOutRead);
    FhStdOutRead := 0;
    CloseHandle(FhStdOutWrite);
    FhStdOutWrite := 0;
  end;

  // close stdErr handles
  if FhStdErrRead <> 0 then begin
    CloseHandle(FhStdErrRead);
    FhStdErrRead := 0;
    CloseHandle(FhStdErrWrite);
    FhStdErrWrite := 0;
  end;
end;

function TConsoleApp.HasPendingOutput: boolean;
var
  Size: cardinal;
begin
  Size := GetFileSize(FhStdOutRead, nil);
  Result := (Size > 0) and (Size < $FFFFFFFF);
end;

function TConsoleApp.HasPendingErr: boolean;
var
  Size: cardinal;
begin
  Size := GetFileSize(FhStdErrRead, nil);
  Result := (Size > 0) and (Size < $FFFFFFFF);
end;

function TConsoleApp.ReadOutput(var Buffer: TBytes; MaxCount: cardinal): cardinal;
var
  Size: cardinal;
begin
  Size := GetFileSize(FhStdOutRead, nil);
  Size := Min(Size, MaxCount);
  Result := 0;
  if Size > 0 then
    ReadFile(FhStdOutRead, Buffer[0], Size, Result, nil);
end;

function TConsoleApp.ReadErr(var Buffer: TBytes; MaxCount: cardinal): cardinal;
var
  Size: cardinal;
begin
  Size := GetFileSize(FhStdErrRead, nil);
  Size := Min(Size, MaxCount);
  Result := 0;
  if Size > 0 then
    ReadFile(FhStdErrRead, Buffer[0], Size, Result, nil);
end;

function TConsoleApp.HasPendingInput: boolean;
var
  Size: cardinal;
begin
  Size := GetFileSize(FhStdInWrite, nil);
  Result := (Size > 0) and (Size < $FFFFFFFF);
end;

function TConsoleApp.Write(const Buffer: TBytes; Count: integer): cardinal;
begin
  Result := 0;
  WriteFile(FhStdInWrite, Buffer[0], Count, Result, nil);
end;

procedure TConsoleApp.GetExitCode(out ExitCode: cardinal);
begin
  ExitCode := FExitCode;
end;

procedure TConsoleApp.WaitForClose;
begin
  if FIsShell then
    Exit;

  while IsActive or HasPendingOutput or HasPendingErr do
    sleep(50);
end;

{$ENDIF}

{ TRWThread }

constructor TRWThread.Create(Handler: TRWHandler; LinkedThread: TRWThread);
begin
  FLinkedThread := LinkedThread;
  FHandler := Handler;

  inherited Create(False); /// Input\Output error(5) for non-Windows platforms
end;

destructor TRWThread.Destroy;
begin
  inherited;

  FLinkedThread.Free;
  FHandler.Free;
end;

procedure TRWThread.Execute;
var
  Count: integer;
begin
  Assert(FHandler <> nil);

  FHandler.CheckObjects;

  try
    try
      FHandler.Initialize;

      while not Terminated and FHandler.Connected do begin
        if FHandler.WaitForData and not Terminated then begin
          Count := FHandler.ReadData;
          FHandler.WriteData(Count);
        end;
      end;

      FHandler.Close;
    except
      on E: Exception do
        FHandler.ProcessError(E);
    end;

  finally
    try
      Terminate;
      FHandler.Disconnect;
    finally
      if FLinkedThread <> nil then begin
        FLinkedThread.Terminate;
        FLinkedThread.FHandler.TryTerminate;
        FLinkedThread.WaitFor;
      end;
    end;
  end;
end;

{ TRWHandler }

constructor TRWHandler.Create(aChannel: TScSSHChannel);
begin
  inherited Create;
  FChannel := aChannel;
end;

procedure TRWHandler.CheckObjects;
begin
  Assert(FChannel <> nil);
end;

procedure TRWHandler.Initialize;
begin
  SetLength(FBuffer, BUF_SIZE);
end;

function TRWHandler.Connected: boolean;
begin
  Result := True;
end;

function TRWHandler.WaitForData: boolean;
begin
  Result := True;
end;

procedure TRWHandler.ProcessError(const E: Exception);
begin
  FChannel.DoError(Self, E);
end;

procedure TRWHandler.Close;
begin
end;

procedure TRWHandler.TryTerminate;
begin
end;

procedure TRWHandler.Disconnect;
begin
end;

{ TSocketToChannelTransfer }

constructor TSocketToChannelTransfer.Create(aChannel: TScSSHChannel; vio: TCRVioTcp);
begin
  inherited Create(aChannel);
  FVio := vio;
end;

procedure TSocketToChannelTransfer.CheckObjects;
begin
  inherited;
  Assert(FVio <> nil);
end;

function TSocketToChannelTransfer.Connected: boolean;
begin
  Result := FVio.Connected and not FClosed;
end;

function TSocketToChannelTransfer.WaitForData: boolean;
begin
  Result := FVio.WaitForData;
end;

function TSocketToChannelTransfer.ReadData: integer;
begin
  Result := FVio.ReadNoWait(TValueArr(FBuffer), 0, Length(FBuffer));
  if Result <= 0 then begin
    FClosed := True;
    if FVio.LastError <> '' then
      raise SocketException.Create(FVio.LastError, FVio.LastErrorCode);
  end;
end;

procedure TSocketToChannelTransfer.WriteData(Count: integer);
begin
  if Count > 0 then
    if FChannel.WriteBuffer(FBuffer[0], Count) <> Count then
      raise EScError.Create(seCannotSendData);
end;

procedure TSocketToChannelTransfer.ProcessError(const E: Exception);
begin
  if (E is SocketException) and (SocketException(E).ErrorCode = 10038) then
    Exit;

  inherited;
end;

procedure TSocketToChannelTransfer.TryTerminate;
begin
  FChannel.DoSocketDisconnect(Self, FVio.RemoteSockAddr);
  FVio.Close;
end;

procedure TSocketToChannelTransfer.Disconnect;
begin
  FChannel.DoDisconnect;
end;

{ TChannelToSocketTransfer }

constructor TChannelToSocketTransfer.Create(aChannel: TScSSHChannel; vio: TCRVioTcp);
begin
  inherited Create(aChannel);
  FVio := vio;
end;

destructor TChannelToSocketTransfer.Destroy;
begin
  FVio.Free;
  FChannel.Free;
  inherited;
end;

procedure TChannelToSocketTransfer.CheckObjects;
begin
  inherited;
  Assert(FVio <> nil);
end;

function TChannelToSocketTransfer.Connected: boolean;
begin
  Result := FChannel.Connected or (FChannel.GetInCount > 0);
end;

function TChannelToSocketTransfer.ReadData: integer;
begin
  Result := FChannel.ReadAllData(FBuffer);
  if Result <= 0 then
    FChannel.CloseIfDisconnected;
end;

procedure TChannelToSocketTransfer.WriteData(Count: integer);
begin
  if Count > 0 then
    if FVio.Write(TValueArr(FBuffer), 0, Count) <> Count then
      raise SocketException.Create(FVio.LastError, FVio.LastErrorCode);
end;

procedure TChannelToSocketTransfer.TryTerminate;
begin
  FChannel.DoDisconnect;
end;

{ TReadChannelHandler }

destructor TReadChannelHandler.Destroy;
begin
  FChannel.Free;
  inherited;
end;

function TReadChannelHandler.Connected: boolean;
begin
  Result := FChannel.Connected or (FChannel.GetInCount > 0);
end;

function TReadChannelHandler.ReadData: integer;
begin
  Result := FChannel.ReadAllData(FBuffer);
end;

procedure TReadChannelHandler.WriteData(Count: integer);
begin
end;

procedure TReadChannelHandler.TryTerminate;
begin
  FChannel.DoDisconnect;
end;

{ TSubSystemHandler }

constructor TSubSystemHandler.Create(aChannel: TScSSHChannel; aSubSystemProcessor: TScSSHSubSystemProcessor);
begin
  inherited Create(aChannel);
  FSubSystemProcessor := aSubSystemProcessor;
end;

destructor TSubSystemHandler.Destroy;
begin
  FSubSystemProcessor.Free;
  FChannel.Free;
  inherited;
end;

procedure TSubSystemHandler.CheckObjects;
begin
  inherited;
  Assert(FSubSystemProcessor <> nil);
end;

procedure TSubSystemHandler.Initialize;
begin
  FSubSystemProcessor.Init;
end;

function TSubSystemHandler.Connected: boolean;
begin
  Result := FChannel.Connected or (FChannel.GetInCount > 0);
end;

function TSubSystemHandler.WaitForData: boolean;
begin
  FChannel.Readable(1);
  Result := FChannel.GetInCount > 0;
end;

function TSubSystemHandler.ReadData: integer;
begin
  Result := 0;
end;

procedure TSubSystemHandler.WriteData(Count: integer);
begin
  FSubSystemProcessor.ProcessRequest;
end;

procedure TSubSystemHandler.TryTerminate;
begin
  FChannel.DoDisconnect;
end;

procedure TSubSystemHandler.Disconnect;
begin
  FSubSystemProcessor.Close;
end;

{$IFDEF MSWINDOWS}

{ TShellToChannelTransfer }

constructor TShellToChannelTransfer.Create(aChannel: TScSSHChannel; aShell: TConsoleApp; aTerminalInfo: TScTerminalInfo);
begin
  inherited Create(aChannel);
  FShell := aShell;
  FTerminalInfo := aTerminalInfo;
end;

destructor TShellToChannelTransfer.Destroy;
begin
  FChannel.Free;
  FShell.Free;
  inherited;
end;

procedure TShellToChannelTransfer.CheckObjects;
begin
  inherited;
  Assert(FShell <> nil);
  Assert(FTerminalInfo <> nil);
end;

procedure TShellToChannelTransfer.Initialize;
begin
  try
    FShell.Open(FTerminalInfo);
  except
    on e: Exception do begin
      FChannel.WriteString(e.Message);
      raise;
    end;
  end;

  inherited;
end;

function TShellToChannelTransfer.Connected: boolean;
begin
  Result := FShell.IsActive or FShell.HasPendingOutput or FShell.HasPendingErr;
end;

function TShellToChannelTransfer.WaitForData: boolean;
begin
  Result := FShell.HasPendingOutput or FShell.HasPendingErr;
  if not Result then begin
    sleep(50);
    Result := FShell.HasPendingOutput or FShell.HasPendingErr;
  end;
end;

function TShellToChannelTransfer.ReadData: integer;
begin
  Result := FShell.ReadErr(FBuffer, Length(FBuffer));
  if Result <= 0 then
    Result := FShell.ReadOutput(FBuffer, Length(FBuffer));
end;

procedure TShellToChannelTransfer.WriteData(Count: integer);
begin
  if Count > 0 then
    if FChannel.WriteBuffer(FBuffer[0], Count) <> Count then
      raise EScError.Create(seCannotSendData);
end;

procedure TShellToChannelTransfer.Close;
begin
  FShell.Close;
  if FChannel.GetConnected then
    FChannel.FStream.SendExitStatus(integer(FShell.FExitCode));
end;

procedure TShellToChannelTransfer.TryTerminate;
begin
  FShell.Close;
end;

procedure TShellToChannelTransfer.Disconnect;
begin
  FChannel.DoDisconnect;
end;

{ TChannelToShellTransfer }

constructor TChannelToShellTransfer.Create(aChannel: TScSSHChannel; aShell: TConsoleApp);
begin
  inherited Create(aChannel);
  FShell := aShell;
end;

procedure TChannelToShellTransfer.CheckObjects;
begin
  inherited;
  Assert(FShell <> nil);
end;

function TChannelToShellTransfer.Connected: boolean;
begin
  Result := FChannel.Connected or (FChannel.GetInCount > 0);
end;

function TChannelToShellTransfer.ReadData: integer;
begin
  Result := FChannel.ReadAllData(FBuffer);
end;

procedure TChannelToShellTransfer.WriteData(Count: integer);
var
  Written: integer;
begin
  if Count > 0 then begin
    if FBuffer[Count - 1] = 13 then begin
      FBuffer[Count] := 10;
      Inc(Count);
    end;

    if FShell.IsShell then begin
      Written := FChannel.WriteBuffer(FBuffer, 0, Count);
      if Count <> Written then
        raise EScError.Create(seCannotSendData);
    end;

    Written := FShell.Write(FBuffer, Count);
    if Count <> Written then
      raise EScError.Create(seCannotSendData);
  end;
end;

procedure TChannelToShellTransfer.TryTerminate;
begin
  FChannel.DoDisconnect;
end;

procedure TChannelToShellTransfer.Disconnect;
begin
  FShell.Close;
end;
{$ENDIF}

{ TListRWFlows }

constructor TListRWFlows.Create;
begin
  inherited;
  FChannelToSocketThreads := TCRList.Create;
  FRWThreads := TCRList.Create;
end;

destructor TListRWFlows.Destroy;
begin
  FChannelToSocketThreads.Free;
  FRWThreads.Free;
  inherited;
end;

procedure TListRWFlows.FreeThreads(List: TCRList);
var
  Thread: TRWThread;
begin
  while List.Count > 0 do begin
    Thread := TRWThread(List.Last);
    try
      if not Thread.Terminated then begin
        Thread.Terminate;
        Thread.FHandler.TryTerminate;
        Thread.WaitFor;
      end;
    finally
      Thread.Free;
      List.Delete(List.Count - 1);
    end;
  end;
end;

procedure TListRWFlows.FreeAllThreads;
begin
  FreeThreads(FChannelToSocketThreads);
  FreeThreads(FRWThreads);
end;

procedure TListRWFlows.FreeFinishedThreads(List: TCRList);
var
  i: integer;
begin
  for i := List.Count - 1 downto 0 do begin
    if TRWThread(List[i]).Terminated then begin
    {$IFNDEF AUTOREFCOUNT}
      TRWThread(List[i]).Free;
    {$ENDIF}
      List.Delete(i);
    end;
  end;
end;

procedure TListRWFlows.CreateSocketToChannelTransferring(aChannel: TScSSHChannel; vio: TCRVioTcp);
var
  SocketToChannelTransfer: TSocketToChannelTransfer;
  ChannelToSocketTransfer: TChannelToSocketTransfer;
  SocketToChannelThread: TRWThread;
  ChannelToSocketThread: TRWThread;
begin
  SocketToChannelThread := nil;
  ChannelToSocketThread := nil;
  try
    SocketToChannelTransfer := TSocketToChannelTransfer.Create(aChannel, vio);
    SocketToChannelThread := TRWThread.Create(SocketToChannelTransfer, nil);

    ChannelToSocketTransfer := TChannelToSocketTransfer.Create(aChannel, vio);
    ChannelToSocketThread := TRWThread.Create(ChannelToSocketTransfer, SocketToChannelThread);

    FChannelToSocketThreads.Add(ChannelToSocketThread);
  except
    SocketToChannelThread.Free;
    ChannelToSocketThread.Free;
    raise;
  end;

  FreeFinishedThreads(FChannelToSocketThreads);
end;

procedure TListRWFlows.CreateReadChannelThread(aChannel: TScSSHChannel);
var
  ReadChannelHandler: TReadChannelHandler;
  ReadChannelThread: TRWThread;
begin
  ReadChannelThread := nil;
  try
    ReadChannelHandler := TReadChannelHandler.Create(aChannel);
    ReadChannelThread := TRWThread.Create(ReadChannelHandler, nil);

    FRWThreads.Add(ReadChannelThread);
  except
    ReadChannelThread.Free;
    raise;
  end;

  FreeFinishedThreads(FRWThreads);
end;

procedure TListRWFlows.CreateSubSystemThread(aChannel: TScSSHChannel; aSubSystemProcessor: TScSSHSubSystemProcessor);
var
  SubSystemHandler: TSubSystemHandler;
  SubSystemThread: TRWThread;
begin
  SubSystemThread := nil;
  try
    SubSystemHandler := TSubSystemHandler.Create(aChannel, aSubSystemProcessor);
    SubSystemThread := TRWThread.Create(SubSystemHandler, nil);

    FRWThreads.Add(SubSystemThread);
  except
    SubSystemThread.Free;
    raise;
  end;

  FreeFinishedThreads(FRWThreads);
end;

{$IFDEF MSWINDOWS}
procedure TListRWFlows.CreateShellToChannelTransferring(aChannel: TScSSHChannel; TerminalInfo: TScTerminalInfo;
  const Username, Domain, Password: string; OSAuthentication: boolean;
  const Command: string);
var
  ShellToChannelTransfer: TShellToChannelTransfer;
  ChannelToShellTransfer: TChannelToShellTransfer;
  ShellToChannelThread: TRWThread;
  ChannelToShellThread: TRWThread;
  Shell: TConsoleApp;
begin
  ShellToChannelThread := nil;
  ChannelToShellThread := nil;

  Shell := TConsoleApp.Create;
  try
    Shell.CommandLine := 'cmd.exe /q';
    Shell.IsShell := Command = '';
    if not Shell.IsShell then
      Shell.CommandLine := Shell.CommandLine + ' /c ' + WideString(Command);

    Shell.UserName := WideString(Username);
    Shell.Domain := WideString(Domain);
    Shell.Password := WideString(Password);
    Shell.OSAuthentication := OSAuthentication;

    ChannelToShellTransfer := TChannelToShellTransfer.Create(aChannel, Shell);
    ChannelToShellThread := TRWThread.Create(ChannelToShellTransfer, nil);

    ShellToChannelTransfer := TShellToChannelTransfer.Create(aChannel, Shell, TerminalInfo);
    ShellToChannelThread := TRWThread.Create(ShellToChannelTransfer, ChannelToShellThread);

    FRWThreads.Add(ShellToChannelThread);
  except
    ShellToChannelThread.Free;
    ChannelToShellThread.Free;
    Shell.Free;
    raise;
  end;

  aChannel.FStream.OnWaitForClose := Shell.WaitForClose;
  aChannel.FStream.OnGetExitCode := Shell.GetExitCode;

  FreeFinishedThreads(FRWThreads);
end;
{$ENDIF}

{ TScSSHChannelUtils }

class procedure TScSSHChannelUtils.SetChannelRequest(Obj: TScSSHCustomChannel; const ChannelRequest: TChannelRequest);
begin
  Obj.FChannelRequest := ChannelRequest;
end;

class procedure TScSSHChannelUtils.SetOnDataFromClient(Obj: TScSSHCustomChannel; OnDataFromClient: TScData);
begin
  Obj.OnDataFromClient := OnDataFromClient;
end;

class procedure TScSSHChannelUtils.SetOnDataToClient(Obj: TScSSHCustomChannel; OnDataToClient: TScData);
begin
  Obj.OnDataToClient := OnDataToClient;
end;

class procedure TScSSHChannelUtils.SetOnChannelError(Obj: TScSSHChannel; OnChannelError: TScChannelError);
begin
  Obj.FOnChannelError := OnChannelError;
end;

class procedure TScSSHChannelUtils.SetBeforeChannelConnect(Obj: TScSSHChannel; BeforeChannelConnect: TScBeforeChannelConnect);
begin
  Obj.FBeforeChannelConnect := BeforeChannelConnect;
end;

class procedure TScSSHChannelUtils.SetAfterChannelDisconnect(Obj: TScSSHChannel; AfterChannelDisconnect: TScAfterChannelDisconnect);
begin
  Obj.FAfterChannelDisconnect := AfterChannelDisconnect;
end;

class function TScSSHChannelUtils.Readable(Obj: TScSSHCustomChannel; const DataLen: Integer; const MillisecondsTimeout: Integer): boolean;
begin
  Result := Obj.Readable(DataLen, MillisecondsTimeout);
end;

class procedure TScSSHChannelUtils.CloseIfDisconnected(Obj: TScSSHCustomChannel);
begin
  Obj.CloseIfDisconnected;
end;

class function TScSSHChannelUtils.GetStream(Obj: TScSSHCustomChannel): TSsh2Channel;
begin
  Result := Obj.FStream;
end;

{$IFDEF MSWINDOWS}
var
  hAdvapi32Lib: HMODULE;
  hUserenvLib: HMODULE;

procedure LoadCryptoLib;
begin
  hAdvapi32Lib := LoadLibrary(PChar(ADVAPI32));
  if hAdvapi32Lib > 0 then begin
    CreateProcessWithLogonW := GetProcAddress(hAdvapi32Lib, 'CreateProcessWithLogonW');
  end;

  hUserenvLib := LoadLibrary('userenv.dll');
  if hUserenvLib > 0 then begin
    CreateEnvironmentBlock := GetProcAddress(hUserenvLib, 'CreateEnvironmentBlock');
    DestroyEnvironmentBlock := GetProcAddress(hUserenvLib, 'DestroyEnvironmentBlock');
  end;
end;

procedure FreeCryptoLib;
begin
  if hAdvapi32Lib > 0 then begin
    FreeLibrary(hAdvapi32Lib);
    hAdvapi32Lib := 0;
  end;

  if hUserenvLib > 0 then begin
    FreeLibrary(hUserenvLib);
    hUserenvLib := 0;
  end;
end;

initialization
  CreateProcessWithLogonW := nil;
  CreateEnvironmentBlock := nil;
  DestroyEnvironmentBlock := nil;
  LoadCryptoLib;

finalization
  FreeCryptoLib;

{$ENDIF}

end.
