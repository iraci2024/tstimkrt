
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScSSHServer;

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
  Windows,
{$ENDIF}
{$IFDEF POSIX}
  Posix.NetinetIn, Posix.SysSocket, Posix.Unistd, Posix.Stdio,
{$ENDIF}
{$IFDEF UNIX}
  Sockets,
{$ENDIF}
  Classes, SysUtils,
  ScTypes, ScUtils, ScVio, ScVioTcp,
  ScTCPServer, ScSSHUtils, ScBridge,
  ScClient, ScSSHConnectionParameter, ScSSHClient, ScSFTPServer;

const
  DefValKeyExchangeAlgorithms = [keDHGroup1Sha1, keDHGroup14Sha1, keDHExchSha1, keDHExchSha256,
    keECDHSha2Nistp256, keECDHSha2Nistp384, keECDHSha2Nistp521, keCurve25519Sha256];
  DefValCiphers = [saTripleDES_cbc, saBlowfish_cbc, saAES128_cbc, saAES192_cbc, saAES256_cbc, saCast128_cbc, saTripleDES_ctr, saBlowfish_ctr, saAES128_ctr, saAES192_ctr, saAES256_ctr, saCast128_ctr];
  DefValHMACs = [hmacSHA1, hmacSHA2_256, hmacSHA2_512, hmacSHA2_224, hmacSHA2_384];
  DefValHostKeyAlgorithms = [aaRSA, aaEC];
  DefValAuthentications = [atPublicKey, atPassword];
  DefValAllowCompression = True;
  DefValPort = 22;
  DefValTimeout = 60;
  DefValAllowEmptyPassword = False;
  DefValClientAliveCountMax = 3;
  DefValClientAliveInterval = 0;
  DefValTCPKeepAlive = True;
  DefValMaxStartups = 20;
  DefValMaxConnections = 0;
  DefValListenBacklog = 5;

type
  TScSSHServerOptions = class(TPersistent)
  private
    FAllowEmptyPassword: boolean;
    FBanner: string;
    FClientAliveCountMax: integer;
    FClientAliveInterval: integer;
    FRekeyLimit: string;
    FListenAddress: string;
    FMaxStartups: integer;
    FMaxConnections: integer;
    FTCPKeepAlive: boolean;
    FListenBacklog: integer;
    FIPVersion: TIPVersion;
    FServerVersion: string;

    procedure SetRekeyLimit(Value: string);
    function NotDefaultVersion: boolean;

  protected
    procedure AssignTo(Dest: TPersistent); override;

  public
    constructor Create;

  published
    property AllowEmptyPassword: boolean read FAllowEmptyPassword write FAllowEmptyPassword default DefValAllowEmptyPassword; // To permit connecting with empty passwords
    property Banner: string read FBanner write FBanner; // text are sent to the remote user before authentication is allowed
    property ClientAliveCountMax: integer read FClientAliveCountMax write FClientAliveCountMax default DefValClientAliveCountMax;
    property ClientAliveInterval: integer read FClientAliveInterval write FClientAliveInterval default DefValClientAliveInterval;
    property TCPKeepAlive: boolean read FTCPKeepAlive write FTCPKeepAlive default DefValTCPKeepAlive;
    property RekeyLimit: string read FRekeyLimit write SetRekeyLimit;
    property ListenAddress: string read FListenAddress write FListenAddress; // useful on systems with more than one IP.
    property MaxStartups: integer read FMaxStartups write FMaxStartups default DefValMaxStartups; // max connections count
    property MaxConnections: integer read FMaxConnections write FMaxConnections default DefValMaxConnections;
    property IPVersion: TIPVersion read FIPVersion write FIPVersion default DefValIPVersion;
    property ListenBacklog: integer read FListenBacklog write FListenBacklog default DefValListenBacklog;
    property ServerVersion: string read FServerVersion write FServerVersion stored NotDefaultVersion;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TScSSHServer = class(TComponent)
  private
    FStreamedActive: boolean;
    FListenThread: TScTCPServerThread;
    FPort: Integer;

    FKeyExchangeAlgorithms: TScKeyExchangeAlgorithms;
    FCiphers: TScSymmetricAlgorithms;
    FHMACs: TScHMACAlgorithms;
    FHostKeyAlgorithms: TScAsymmetricAlgorithms;
    FAuthentications: TScSSHAuthentications; // Lists the acceptable authentication methods.
    FAllowCompression: boolean;
    FOptions: TScSSHServerOptions;
    FKeyNameRSA: string;
    FKeyNameDSA: string;
    FKeyNameEC: string;
    FStorage: TScStorage;
    FTimeout: integer;
    FSFTPServer: TScSFTPServer;
    FConnParams: TSshConnectionParameters;

    FConnectingThreadCount: integer;
    FInClear: boolean;
    FServerConnections: TCRThreadList;
    FDoConnectThreads: TCRThreadList;

    FOnError: TScErrorEvent;
    FBeforeClientConnect: TScBeforeClientConnectEvent;
    FAfterClientConnect: TScClientEvent;
    FAfterClientDisconnect: TScClientEvent;
    FOnClientError: TScClientError;
    FBeforeChannelConnect: TScBeforeChannelConnect;
    FAfterChannelDisconnect: TScAfterChannelDisconnect;
    FOnChannelError: TScChannelError;
    FBeforeShellConnect: TScBeforeShellConnect;
    FAfterShellDisconnect: TScAfterShellDisconnect;
    FOnRemotePortForwardingRequest: TScOnRemotePortForwardingRequest;
    FOnCancelRemotePortForwardingRequest: TScOnCancelRemotePortForwardingRequest;
    FOnDataFromClient: TScData;
    FOnDataToClient: TScData;
    FClientInfoList: TCRList;
    FChannelInfoList: TCRList;

    function GetClientInfo(Index: Integer): TScSSHClientInfo;
    function GetClientInfoCount: Integer;
    function GetChannelInfo(Index: Integer): TScSSHChannelInfo;
    function GetChannelInfoCount: Integer;
    procedure DoOnFreeClientInfo(Sender: TObject);
    procedure DoOnFreeChannelInfo(Sender: TObject);
    procedure CheckInactive;

  {$HPPEMIT '#ifdef SetPort'}
  {$HPPEMIT '#undef SetPort'}
  {$HPPEMIT '#endif'}
    procedure SetPort(const Value: Integer);
    procedure SetKeyExchangeAlgorithms(const Value: TScKeyExchangeAlgorithms);
    procedure SetCiphers(const Value: TScSymmetricAlgorithms);
    procedure SetHMACs(const Value: TScHMACAlgorithms);
    procedure SetHostKeyAlgorithms(const Value: TScAsymmetricAlgorithms);
    procedure SetAuthentications(const Value: TScSSHAuthentications);
    procedure SetAllowCompression(Value: boolean);
    procedure SetOptions(Value: TScSSHServerOptions);
    procedure SetKeyNameRSA(const Value: string);
    procedure SetKeyNameDSA(const Value: string);
    procedure SetKeyNameEC(const Value: string);
    procedure SetStorage(Value: TScStorage);
    procedure SetSFTPServer(Value: TScSFTPServer);
    procedure SetTimeout(Value: integer);
    procedure DoError(Sender: TObject; E: Exception);
    procedure DoBeforeClientConnect(const SockAddr: PSockAddr; var Cancel: boolean);
    procedure DoAfterClientConnect(Sender: TObject; ClientInfo: TScSSHClientInfo);
    procedure DoAfterClientDisconnect(Sender: TObject; ClientInfo: TScSSHClientInfo);
    procedure DoClientError(Sender: TObject; ClientInfo: TScSSHClientInfo; E: Exception);
    procedure DoBeforeChannelConnect(Sender: TObject; ChannelInfo: TScSSHChannelInfo; var Direct: boolean);
    procedure DoAfterChannelDisconnect(Sender: TObject; ChannelInfo: TScSSHChannelInfo);
    procedure AfterChannelClosed(Sender: TObject; ChannelInfo: TScSSHChannelInfo);
    procedure DoChannelError(Sender: TObject; ChannelInfo: TScSSHChannelInfo; E: Exception);
    procedure DoDataFromClient(Sender: TObject; ChannelInfo: TScSSHChannelInfo;
      const Buffer: TBytes; const Offset, Count: integer);
    procedure DoDataToClient(Sender: TObject; ChannelInfo: TScSSHChannelInfo;
      const Buffer: TBytes; const Offset, Count: integer);
    procedure DoBeforeShellConnect(Sender: TObject; ClientInfo: TScSSHClientInfo);
    procedure DoAfterShellDisconnect(Sender: TObject; ClientInfo: TScSSHClientInfo);
    procedure DoRemotePortForwardingRequest(Sender: TObject; ClientInfo: TScSSHClientInfo;
      const Host: string; const Port: Integer; var Allow: Boolean);
    procedure DoCancelRemotePortForwardingRequest(Sender: TObject;
      ClientInfo: TScSSHClientInfo; const Host: string; const Port: Integer);

    procedure CreateServerConnection(Vio: TCRVioTcp);
    procedure ClearClosedConnectionsAndThreads;
    procedure CloseAllConnectionsAndThreads;
    procedure AcceptSocketConnection(Sender: TObject; Vio: TCRVioTcp);
    procedure AfterThreadExecute(Sender: TObject);

  protected
    procedure Notification(Component: TComponent; Operation: TOperation); override;

    function GetActive: boolean; virtual;
    procedure SetActive(Value: boolean);
    procedure Loaded; override;
    procedure DoConnect; virtual;
    procedure DoDisconnect; virtual;

    procedure FillServerCiphers(Dest: TScSSHCiphers);
    procedure FillHMACAlgorithms(Dest: TScSSHHMacAlgorithms);
    procedure DoCheckUserPass(Sender: TObject; ClientInfo: TScSSHClientInfo; const Password: string; var Accept: boolean);
    procedure DoCheckUserKey(Sender: TObject; ClientInfo: TScSSHClientInfo; Key: TScKey; var Accept: boolean);

    property ConnectionParameters: TSshConnectionParameters read FConnParams;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SendToClient(ChannelInfo: TScSSHChannelInfo; const Buffer; const Count: integer); overload;
    procedure SendToClient(ChannelInfo: TScSSHChannelInfo; const Buffer: TBytes; const Offset, Count: integer); overload;

    property ClientInfos[Index: Integer]: TScSSHClientInfo read GetClientInfo;
    property ClientInfoCount: Integer read GetClientInfoCount;
    property ChannelInfos[Index: Integer]: TScSSHChannelInfo read GetChannelInfo;
    property ChannelInfoCount: Integer read GetChannelInfoCount;

  published
    property Active: boolean read GetActive write SetActive default False;
    property Port: integer read FPort write SetPort default DefValPort;

    // Keys
    property KeyNameRSA: string read FKeyNameRSA write SetKeyNameRSA;
    property KeyNameDSA: string read FKeyNameDSA write SetKeyNameDSA;
    property KeyNameEC: string read FKeyNameEC write SetKeyNameEC;

    property KeyExchangeAlgorithms: TScKeyExchangeAlgorithms read FKeyExchangeAlgorithms write SetKeyExchangeAlgorithms default DefValKeyExchangeAlgorithms;
    property Ciphers: TScSymmetricAlgorithms read FCiphers write SetCiphers default DefValCiphers;
    property HMACs: TScHMACAlgorithms read FHMACs write SetHMACs default DefValHMACs;
    property HostKeyAlgorithms: TScAsymmetricAlgorithms read FHostKeyAlgorithms write SetHostKeyAlgorithms default DefValHostKeyAlgorithms;
    property Authentications: TScSSHAuthentications read FAuthentications write SetAuthentications default DefValAuthentications; // Lists the acceptable authentication methods.
    property AllowCompression: boolean read FAllowCompression write SetAllowCompression default DefValAllowCompression;
    property Options: TScSSHServerOptions read FOptions write SetOptions;
    property Timeout: integer read FTimeout write SetTimeout default DefValTimeout;
    property Storage: TScStorage read FStorage write SetStorage;
    property SFTPServer: TScSFTPServer read FSFTPServer write SetSFTPServer;

    property OnError: TScErrorEvent read FOnError write FOnError;
    property BeforeClientConnect: TScBeforeClientConnectEvent read FBeforeClientConnect write FBeforeClientConnect;
    property AfterClientConnect: TScClientEvent read FAfterClientConnect write FAfterClientConnect;
    property AfterClientDisconnect: TScClientEvent read FAfterClientDisconnect write FAfterClientDisconnect;
    property OnClientError: TScClientError read FOnClientError write FOnClientError;
    property BeforeChannelConnect: TScBeforeChannelConnect read FBeforeChannelConnect write FBeforeChannelConnect;
    property AfterChannelDisconnect: TScAfterChannelDisconnect read FAfterChannelDisconnect write FAfterChannelDisconnect;
    property OnChannelError: TScChannelError read FOnChannelError write FOnChannelError;
    property BeforeShellConnect: TScBeforeShellConnect read FBeforeShellConnect write FBeforeShellConnect;
    property AfterShellDisconnect: TScAfterShellDisconnect read FAfterShellDisconnect write FAfterShellDisconnect;
    property OnRemotePortForwardingRequest: TScOnRemotePortForwardingRequest
      read FOnRemotePortForwardingRequest write FOnRemotePortForwardingRequest;
    property OnCancelRemotePortForwardingRequest: TScOnCancelRemotePortForwardingRequest
      read FOnCancelRemotePortForwardingRequest write FOnCancelRemotePortForwardingRequest;

    property OnDataFromClient: TScData read FOnDataFromClient write FOnDataFromClient;
    property OnDataToClient: TScData read FOnDataToClient write FOnDataToClient;
  end;

  TScSSHServerUtils = class
  public
    class procedure CheckUserPass(Obj: TScSSHServer; ClientInfo: TScSSHClientInfo; const Password: string; var Accept: boolean);
    class procedure CheckUserKey(Obj: TScSSHServer; ClientInfo: TScSSHClientInfo; Key: TScKey; var Accept: boolean);
  end;

implementation

uses
  ScCLRClasses,
{$IFDEF VER17P}
  Types,
{$ENDIF}
  TypInfo, ScFunctions, ScSSHSocket, ScAlgorithmSupport,
  ScSSH2ServerConnection, ScConsts,
  ScSSHChannel, ScCertificateConsts;

type
  TScSSHServerConnection = class(TScSSHClient)
  private
    FParentSSHServer: TScSSHServer;
    FChannels: TCRThreadList;

  protected
    procedure DoAfterConnect(Sender: TObject);
    procedure DoAfterDisconnect(Sender: TObject);
    procedure OnError(Sender: TObject; Error: Exception);
    procedure DoOnPortForwardingConnect(Sender: TObject; ChannelInfo: TScSSHChannelInfo; var Direct: boolean);
    function NewSSHChannel(const ChannelRequest: TChannelRequest): TObject; override;
    procedure EstablishPortForwarding(const ChannelRequest: TChannelRequest); override;
    procedure EstablishRemotePortForwarding(const Host: string; const Port: Integer);
    procedure CancelRemotePortForwarding(const Host: string; const Port: Integer);
    function ExtractChannel(Stream: TObject): TScSSHChannel;
    procedure EstablishSession(const ChannelRequest: TChannelRequest);
    procedure OpenSession(Stream: TObject; TerminalInfo: TScTerminalInfo; const Request: string; const Command: string);
    procedure ClearThreads;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TDoConnectThread = class(TThread)
  protected
    FSSHServer: TScSSHServer;
    FVio: TCRVioTcp;

    procedure Execute; override;
  public
    constructor Create(SSHServer: TScSSHServer; Vio: TCRVioTcp);
  end;

{ TDoConnectThread }

constructor TDoConnectThread.Create(SSHServer: TScSSHServer; Vio: TCRVioTcp);
begin
  FSSHServer := SSHServer;
  FVio := Vio;

  inherited Create(False); /// Input\Output error(5) for non-Windows platforms
end;

procedure TDoConnectThread.Execute;
begin
  FSSHServer.CreateServerConnection(FVio);
end;

{ TScSSHServerOptions }

constructor TScSSHServerOptions.Create;
begin
  inherited;

  FClientAliveCountMax := DefValClientAliveCountMax;
  FClientAliveInterval := DefValClientAliveInterval;
  FTCPKeepAlive := DefValTCPKeepAlive;
  FMaxStartups := DefValMaxStartups;
  FMaxConnections := DefValMaxConnections;
  FListenBacklog := DefValListenBacklog;
  FIPVersion := DefValIPVersion;
  FServerVersion := TSshUtils.ServerVersionString(pSSH2);
end;

procedure TScSSHServerOptions.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScSSHServerOptions) then begin
    TScSSHServerOptions(Dest).FAllowEmptyPassword := FAllowEmptyPassword;
    TScSSHServerOptions(Dest).FBanner := FBanner;
    TScSSHServerOptions(Dest).FClientAliveCountMax := FClientAliveCountMax;
    TScSSHServerOptions(Dest).FClientAliveInterval := FClientAliveInterval;
    TScSSHServerOptions(Dest).FRekeyLimit := FRekeyLimit;
    TScSSHServerOptions(Dest).FListenAddress := FListenAddress;
    TScSSHServerOptions(Dest).FMaxStartups := FMaxStartups;
    TScSSHServerOptions(Dest).FMaxConnections := FMaxConnections;
    TScSSHServerOptions(Dest).FTCPKeepAlive := FTCPKeepAlive;
    TScSSHServerOptions(Dest).FIPVersion := FIPVersion;
    TScSSHServerOptions(Dest).FListenBacklog := FListenBacklog;
    TScSSHServerOptions(Dest).FServerVersion := FServerVersion;
  end
  else
    inherited;
end;

procedure TScSSHServerOptions.SetRekeyLimit(Value: string);
begin
  if Value <> FRekeyLimit then begin
    Value := UpperCase(Trim(Value));
    GetRekeyLimitSize(Value);
    FRekeyLimit := Value;
  end;
end;

function TScSSHServerOptions.NotDefaultVersion: boolean;
begin
  Result := not (FServerVersion = TSshUtils.ServerVersionString(pSSH2));
end;

{ TScSSHServer }

constructor TScSSHServer.Create(AOwner: TComponent);
begin
  inherited;

  FKeyExchangeAlgorithms := DefValKeyExchangeAlgorithms;
  FCiphers := DefValCiphers;
  FHMACs := DefValHMACs;
  FHostKeyAlgorithms := DefValHostKeyAlgorithms;
  FAuthentications := DefValAuthentications;
  FAllowCompression := DefValAllowCompression;
  FPort := DefValPort;
  FTimeout := DefValTimeout;

  FOptions := TScSSHServerOptions.Create;
  FClientInfoList := TCRList.Create;
  FChannelInfoList := TCRList.Create;
  FConnParams := TSshConnectionParameters.Create;
  FDoConnectThreads := TCRThreadList.Create;
  FServerConnections := TCRThreadList.Create;
end;

destructor TScSSHServer.Destroy;
begin
  DoDisconnect;

  FOptions.Free;
  FClientInfoList.Free;
  FChannelInfoList.Free;
  FConnParams.Free;
  FDoConnectThreads.Free;
  FServerConnections.Free;

  inherited;
end;

function TScSSHServer.GetActive: boolean;
begin
  Result := (FListenThread <> nil) and not FListenThread.Finished;
end;

procedure TScSSHServer.SetActive(Value: boolean);
begin
  if (csReading in ComponentState) and Value then
    FStreamedActive := True
  else begin
    if Value = GetActive then
      Exit;

    if Value then
      DoConnect
    else
      DoDisconnect;
  end;
end;

procedure TScSSHServer.Loaded;
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

procedure TScSSHServer.DoConnect;

  function GetKeyExchangeAlgorithms: string;
  var
    Alg: TScKeyExchangeAlgorithm;
  begin
    Result := '';

    for Alg := Low(TScKeyExchangeAlgorithm) to High(TScKeyExchangeAlgorithm) do
      if Alg in KeyExchangeAlgorithms then begin
        if Result <> '' then
          Result := Result + ',';
        Result := Result + CipherFactory.KeyExchangeAlgorithmToSSH2Name(Alg);
      end;
  end;

  function GetHostKeyAlgorithms: string;
  var
    ECKey: TScKey;
  begin
    Result := '';

    if FConnParams.HostKeys[aaRSA] <> nil then
      Result := Result + CipherFactory.PublicKeyAlgorithmToSSH2ListName(aaRSA);

    if FConnParams.HostKeys[aaDSA] <> nil then begin
      if Result <> '' then
        Result := Result + ',';
      Result := Result + CipherFactory.PublicKeyAlgorithmToSSH2ListName(aaDSA);
    end;

    ECKey := FConnParams.HostKeys[aaEC];
    if ECKey <> nil then begin
      if Result <> '' then
        Result := Result + ',';

      ECKey.Ready := True;
      if ECKey.ECData.ECCryptography = nil then
        raise EScError.Create(seInternalError);

      Result := Result + CipherFactory.PublicKeyAlgorithmToSSH2FullName(aaEC, ECKey.ECData.ECName);
    end;
  end;

var
  CurKeyNameRSA, CurKeyNameDSA, CurKeyNameEC: string;
  S: string;
  ThreadEvents: TScTCPServerThreadEvents;
begin
  if Storage = nil then
    raise EScError.Create(seStorageNoSet);

  if KeyNameRSA = '' then
    CurKeyNameRSA := RSA_TYPE_HEADER
  else
    CurKeyNameRSA := KeyNameRSA;
  if KeyNameDSA = '' then
    CurKeyNameDSA := DSA_TYPE_HEADER
  else
    CurKeyNameDSA := KeyNameDSA;
  if KeyNameEC = '' then
    CurKeyNameEC := ECDSA_TYPE_HEADER
  else
    CurKeyNameEC := KeyNameEC;

  if aaRSA in HostKeyAlgorithms then
    FConnParams.HostKeys[aaRSA] := Storage.Keys.FindKey(CurKeyNameRSA);
  if aaDSA in HostKeyAlgorithms then
    FConnParams.HostKeys[aaDSA] := Storage.Keys.FindKey(CurKeyNameDSA);
  if aaEC in HostKeyAlgorithms then
    FConnParams.HostKeys[aaEC] := Storage.Keys.FindKey(CurKeyNameEC);

  if (FConnParams.HostKeys[aaRSA] = nil) and (FConnParams.HostKeys[aaDSA] = nil) and (FConnParams.HostKeys[aaEC] = nil) then
    raise EScError.Create(seHostKeysNotFound);

  if Random = nil then
    raise Exception.Create(SInternalError);
  FConnParams.Random := Random;
  FConnParams.OnCheckUserPass := DoCheckUserPass;
  FConnParams.OnCheckUserKey := DoCheckUserKey;

  FConnParams.KeyExchangeAlgorithms := GetKeyExchangeAlgorithms;
  FConnParams.PreferableHostKeyAlgorithms := GetHostKeyAlgorithms;
  FConnParams.ServerAuthenticationsType := Authentications;
{$IFDEF HAVE_COMPRESS_INTERNAL}
  if AllowCompression then
    FConnParams.CompressionClientAlgorithms := SNone + ',' + SZLib + ',' + SZLibOpenSSH
  else
{$ENDIF}
    FConnParams.CompressionClientAlgorithms := SNone;
  FConnParams.CompressionServerAlgorithms := FConnParams.CompressionClientAlgorithms;

  S := Options.RekeyLimit;
  FConnParams.RekeyLimitSize := GetRekeyLimitSize(S);
  FConnParams.AliveCountMax := Options.ClientAliveCountMax;
  FConnParams.AliveInterval := Options.ClientAliveInterval;
  FConnParams.IPVersion := Options.IPVersion;
  FConnParams.Banner := Options.Banner;
  FConnParams.ServerVersion := Options.ServerVersion;
  FConnParams.OnRemotePortForwardingRequest := DoRemotePortForwardingRequest;
  FConnParams.OnCancelRemotePortForwardingRequest := DoCancelRemotePortForwardingRequest;

  FreeAndNil(FListenThread);

  ThreadEvents.BeforeBind := nil;
  ThreadEvents.BeforeExecute := nil;
  ThreadEvents.AfterExecute := AfterThreadExecute;
  ThreadEvents.AfterAccept := AcceptSocketConnection;
  ThreadEvents.OnCheckIfStopListen := nil;
  ThreadEvents.OnException := DoError;
  FListenThread := TScTCPServerThread.Create(ThreadEvents, Trim(Options.ListenAddress), Port,
    Options.IPVersion, Timeout, Options.TCPKeepAlive, Options.ListenBacklog, 0);
  FListenThread.StartListen;
end;

procedure TScSSHServer.DoDisconnect;
begin
  if FListenThread <> nil then begin
    FListenThread.Terminate;
    FListenThread.ListenSocket.Close;
    FreeAndNil(FListenThread);
  end;
end;

procedure TScSSHServer.CreateServerConnection(Vio: TCRVioTcp);
var
  Client: TScSSHServerConnection;
  TmpConnParams: TSshConnectionParameters;
  Conn: TSsh2ServerConnection;
begin
  InterlockedIncrement(FConnectingThreadCount);

  TmpConnParams := TSshConnectionParameters.Create;
  try
    TmpConnParams.Assign(FConnParams);

    Client := TScSSHServerConnection.Create(nil);
    try
      Client.FParentSSHServer := Self;

      TmpConnParams.AfterConnect := Client.DoAfterConnect;
      TmpConnParams.AfterDisconnect := Client.DoAfterDisconnect;
      TmpConnParams.OnError := Client.OnError;
      TmpConnParams.OnEstablishPortForwarding := Client.EstablishPortForwarding;
      TmpConnParams.OnEstablishRemotePortForwarding := Client.EstablishRemotePortForwarding;
      TmpConnParams.OnCancelRemotePortForwarding := Client.CancelRemotePortForwarding;
      TmpConnParams.OnEstablishSession := Client.EstablishSession;
      TmpConnParams.OnOpenSession := Client.OpenSession;

      Client.Options.IPVersion := TmpConnParams.IPVersion;
      Client.Timeout := Timeout;
      FillServerCiphers(Client.CiphersClient);
      FillHMACAlgorithms(Client.HMACAlgorithms);
      TScSSHConnectionInfoUtils.SetCiphersClient(Client.ClientInfo, Client.CiphersClient);
      TScSSHConnectionInfoUtils.SetCiphersServer(Client.ClientInfo, Client.CiphersClient);
      TScSSHConnectionInfoUtils.SetHMACAlgorithms(Client.ClientInfo, Client.HMACAlgorithms);
      TScSSHConnectionInfoUtils.SetTCPConnection(Client.ClientInfo, Vio);
      TmpConnParams.ConnectionInfo := Client.ClientInfo;

      try
        TScClientUtils.SetVio(Client, Vio);
        Vio.ReceiveTimeout := Client.Timeout;

        Conn := TSsh2ServerConnection.Create(TmpConnParams);
        try
          TScClientUtils.SetConnection(Client, Conn); // set before connection - Conn can be used immediately after this
          Conn.Connect(Vio);
        except
          TScClientUtils.SetConnection(Client, nil);
          Conn.Free;
          raise;
        end;

      except
        on E: Exception do begin
          DoClientError(Client, Client.ClientInfo, E);
          raise;
        end;
      end;

      FServerConnections.Add(Client);
    except
      Client.Free;
    end;

    ClearClosedConnectionsAndThreads;
  finally
    InterlockedDecrement(FConnectingThreadCount);
    TmpConnParams.Free;
  end;
end;

procedure TScSSHServer.ClearClosedConnectionsAndThreads;
var
  ConnList, ThreadList: TCRList;
  Conn: TScSSHClient;
  i: Integer;
begin
  if FInClear then
    Exit;

  FServerConnections.LockList;
  try
    if FInClear then
      Exit;
    FInClear := True;
  finally
    FServerConnections.UnlockList;
  end;

  ThreadList := FDoConnectThreads.LockList;
  try
    i := 0;
    while i < ThreadList.Count do begin
      if TDoConnectThread(ThreadList[i]).ThreadID <> GetCurrentThreadID then begin
        TDoConnectThread(ThreadList[i]).WaitFor;
      {$IFNDEF AUTOREFCOUNT}
        TObject(ThreadList[i]).Free;
      {$ELSE}
        ThreadList[i] := nil;
      {$ENDIF}
        ThreadList.Delete(i);
      end
      else
        Inc(i);
    end;
  finally
    FDoConnectThreads.UnlockList;
  end;

  ConnList := FServerConnections.LockList;
  try
    i := 0;
    while i < ConnList.Count do begin
      Conn := TScSSHClient(ConnList[i]);
      if not Conn.Connected then begin
        Conn.Free;
        ConnList.Delete(i);
      end
      else
        Inc(i);
    end;

  finally
    FInClear := False;
    FServerConnections.UnlockList;
  end;
end;

procedure TScSSHServer.CloseAllConnectionsAndThreads;
var
  ThreadList, ConnList: TCRList;
  i: Integer;
begin
  FServerConnections.LockList;
  FInClear := True;
  FServerConnections.UnlockList;

  ThreadList := FDoConnectThreads.LockList;
  try
    for i := ThreadList.Count - 1 downto 0 do begin
      TDoConnectThread(ThreadList[i]).WaitFor;
    {$IFNDEF AUTOREFCOUNT}
      TObject(ThreadList[i]).Free;
    {$ELSE}
      ThreadList[i] := nil;
    {$ENDIF}
    end;

    ThreadList.Clear;
  finally
    FDoConnectThreads.UnlockList;
  end;

  ConnList := FServerConnections.LockList;
  try
    for i := ConnList.Count - 1 downto 0 do
      try
      {$IFNDEF AUTOREFCOUNT}
        TObject(ConnList[i]).Free;
      {$ELSE}
        ConnList[i] := nil;
      {$ENDIF}
      except
        on E: Exception do
          DoError(Self, E);
      end;

    ConnList.Clear;
  finally
    FServerConnections.UnlockList;
  end;
end;

procedure TScSSHServer.AcceptSocketConnection(Sender: TObject; Vio: TCRVioTcp);
var
  DoConnectThread: TThread;
  Cancel: boolean;
begin
  if ((Options.MaxStartups > 0) and (FConnectingThreadCount >= Options.MaxStartups)) or
    ((Options.MaxConnections > 0) and (GetClientInfoCount >= Options.MaxConnections))
  then
    Cancel := True
  else
    Cancel := False;

  try
    DoBeforeClientConnect(Vio.RemoteSockAddr, Cancel);
  finally
    if Cancel then
      Vio.Free
    else begin
      DoConnectThread := TDoConnectThread.Create(Self, Vio);
      FDoConnectThreads.Add(DoConnectThread);
    end;
  end;
end;

procedure TScSSHServer.AfterThreadExecute(Sender: TObject);
begin
  CloseAllConnectionsAndThreads;
end;

procedure TScSSHServer.FillServerCiphers(Dest: TScSSHCiphers);
var
  sa: TScSymmetricAlgorithm;
begin
  Dest.Clear;

  for sa := Low(TScSymmetricAlgorithm) to High(TScSymmetricAlgorithm) do begin
    if sa in FCiphers then
      TScSSHCipherItem(Dest.Add).Algorithm := sa;
  end;
end;

procedure TScSSHServer.FillHMACAlgorithms(Dest: TScSSHHMacAlgorithms);
var
  HMACAlg: TScHMACAlgorithm;
begin
  Dest.Clear;

  for HMACAlg := Low(TScHMACAlgorithm) to High(TScHMACAlgorithm) do begin
    if HMACAlg in FHMACs then
      TScSSHHMacAlgorithmItem(Dest.Add).Algorithm := HMACAlg;
  end;
end;

function TScSSHServer.GetClientInfo(Index: Integer): TScSSHClientInfo;
begin
  Result := TObject(FClientInfoList.Items[Index]) as TScSSHClientInfo;
end;

function TScSSHServer.GetClientInfoCount: Integer;
begin
  Result := FClientInfoList.Count;
end;

function TScSSHServer.GetChannelInfo(Index: Integer): TScSSHChannelInfo;
begin
  Result := TObject(FChannelInfoList.Items[Index]) as TScSSHChannelInfo;
end;

function TScSSHServer.GetChannelInfoCount: Integer;
begin
  Result := FChannelInfoList.Count;
end;

procedure TScSSHServer.DoOnFreeClientInfo(Sender: TObject);
begin
  FClientInfoList.Remove(Sender);
end;

procedure TScSSHServer.DoOnFreeChannelInfo(Sender: TObject);
begin
  FChannelInfoList.Remove(Sender);
end;

procedure TScSSHServer.CheckInactive;
begin
  if Active then
    raise EScError.Create(seServerOpened);
end;

procedure TScSSHServer.SetPort(const Value: Integer);
begin
  if Value <> FPort then begin
    CheckInactive;
    FPort := Value;
  end;
end;

procedure TScSSHServer.SetKeyExchangeAlgorithms(const Value: TScKeyExchangeAlgorithms);
begin
  if Value <> FKeyExchangeAlgorithms then begin
    CheckInactive;
    FKeyExchangeAlgorithms := Value;
  end;
end;

procedure TScSSHServer.SetCiphers(const Value: TScSymmetricAlgorithms);
begin
  if Value <> FCiphers then begin
    CheckInactive;
    FCiphers := Value;
  end;
end;

procedure TScSSHServer.SetHMACs(const Value: TScHMACAlgorithms);
begin
  if Value <> FHMACs then begin
    CheckInactive;
    FHMACs := Value;
  end;
end;

procedure TScSSHServer.SetHostKeyAlgorithms(const Value: TScAsymmetricAlgorithms);
begin
  if Value <> FHostKeyAlgorithms then begin
    CheckInactive;
    FHostKeyAlgorithms := Value;
  end;
end;

procedure TScSSHServer.SetAuthentications(const Value: TScSSHAuthentications);
begin
  if Value <> FAuthentications then begin
    CheckInactive;
    FAuthentications := Value;
  end;
end;

procedure TScSSHServer.SetAllowCompression(Value: boolean);
begin
  if Value <> FAllowCompression then begin
    CheckInactive;
    FAllowCompression := Value;
  end;
end;

procedure TScSSHServer.SetOptions(Value: TScSSHServerOptions);
begin
  if Value <> FOptions then begin
    CheckInactive;
    FOptions.Assign(Value);
  end;
end;

procedure TScSSHServer.Notification(Component: TComponent; Operation: TOperation);
begin
  if (Component = FStorage) and (Operation = opRemove) then
    Storage := nil;
  if (Component = FSFTPServer) and (Operation = opRemove) then
    SFTPServer := nil;

  inherited;
end;

procedure TScSSHServer.SetKeyNameRSA(const Value: string);
begin
  if Value <> FKeyNameRSA then begin
    CheckInactive;
    FKeyNameRSA := Value;
  end;
end;

procedure TScSSHServer.SetKeyNameDSA(const Value: string);
begin
  if Value <> FKeyNameDSA then begin
    CheckInactive;
    FKeyNameDSA := Value;
  end;
end;

procedure TScSSHServer.SetKeyNameEC(const Value: string);
begin
  if Value <> FKeyNameEC then begin
    CheckInactive;
    FKeyNameEC := Value;
  end;
end;

procedure TScSSHServer.SetStorage(Value: TScStorage);
begin
  if FStorage <> Value then begin
    if FStorage <> nil then
      FStorage.RemoveFreeNotification(Self);

    SetActive(False);
    FStorage := Value;

    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

procedure TScSSHServer.SetSFTPServer(Value: TScSFTPServer);
begin
  if FSFTPServer <> Value then begin
    if FSFTPServer <> nil then
      FSFTPServer.RemoveFreeNotification(Self);

    SetActive(False);
    FSFTPServer := Value;

    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

procedure TScSSHServer.SetTimeout(Value: integer);
begin
  if Value <> FTimeout then begin
    CheckInactive;
    FTimeout := Value;
  end;
end;

procedure TScSSHServer.SendToClient(ChannelInfo: TScSSHChannelInfo; const Buffer; const Count: integer);
begin
  SendToClient(ChannelInfo, TBytes(@Buffer), 0, Count);
end;

procedure TScSSHServer.SendToClient(ChannelInfo: TScSSHChannelInfo; const Buffer: TBytes; const Offset, Count: integer);
var
  SSHChannel: TScSSHCustomChannel;
begin
  if ChannelInfo = nil then
    raise EScError.Create(seChannelNotDefined);

  SSHChannel := TScSSHCustomChannel(TScSSHChannelInfoUtils.GetSSHChannel(ChannelInfo));
  if SSHChannel = nil then
    raise EScError.Create(seChannelNotDefined);

  SSHChannel.WriteBuffer(Buffer, Offset, Count);
end;

procedure TScSSHServer.DoCheckUserPass(Sender: TObject; ClientInfo: TScSSHClientInfo; const Password: string; var Accept: boolean);
begin
  if Storage = nil then
    raise EScError.Create(seStorageNoSet);

  if not Options.AllowEmptyPassword and (Password = '') then
    Accept := False
  else
    Accept := True;
  TScStorageUtils.CheckUserPass(Storage, ClientInfo, Password, Accept);
end;

procedure TScSSHServer.DoCheckUserKey(Sender: TObject; ClientInfo: TScSSHClientInfo; Key: TScKey; var Accept: boolean);
begin
  if Storage = nil then
    raise EScError.Create(seStorageNoSet);

  Accept := True;
  TScStorageUtils.CheckUserKey(Storage, ClientInfo, Key, Accept);
end;

procedure TScSSHServer.DoError(Sender: TObject; E: Exception);
begin
  if Assigned(FOnError) then
    FOnError(Self, E);
end;

procedure TScSSHServer.DoBeforeClientConnect(const SockAddr: PSockAddr; var Cancel: boolean);
begin
  if Assigned(FBeforeClientConnect) then
    FBeforeClientConnect(Self, SockAddr, Cancel);
end;

procedure TScSSHServer.DoAfterClientConnect(Sender: TObject; ClientInfo: TScSSHClientInfo);
begin
  TScSSHConnectionInfoUtils.SetOnFree(ClientInfo, DoOnFreeClientInfo);
  FClientInfoList.Add(ClientInfo);

  if Assigned(FAfterClientConnect) then
    FAfterClientConnect(Self, ClientInfo);
end;

procedure TScSSHServer.DoAfterClientDisconnect(Sender: TObject; ClientInfo: TScSSHClientInfo);
begin
  try
    if Assigned(FAfterClientDisconnect) then
      FAfterClientDisconnect(Self, ClientInfo);
  finally
    FClientInfoList.Remove(ClientInfo);
    TScSSHConnectionInfoUtils.SetOnFree(ClientInfo, nil);
  end;
end;

procedure TScSSHServer.DoClientError(Sender: TObject; ClientInfo: TScSSHClientInfo; E: Exception);
begin
  if Assigned(FOnClientError) then
    FOnClientError(Self, ClientInfo, E);
end;

procedure TScSSHServer.DoBeforeChannelConnect(Sender: TObject; ChannelInfo: TScSSHChannelInfo; var Direct: boolean);
begin
  TScSSHChannelInfoUtils.SetOnFree(ChannelInfo, DoOnFreeChannelInfo);
  FChannelInfoList.Add(ChannelInfo);

  if Assigned(FBeforeChannelConnect) then
    FBeforeChannelConnect(Self, ChannelInfo, Direct);
end;

procedure TScSSHServer.DoAfterChannelDisconnect(Sender: TObject; ChannelInfo: TScSSHChannelInfo);
begin
  try
    if Assigned(FAfterChannelDisconnect) then
      FAfterChannelDisconnect(Self, ChannelInfo);
  finally
    FChannelInfoList.Remove(ChannelInfo);
    TScSSHChannelInfoUtils.SetOnFree(ChannelInfo, nil);
  end;
end;

procedure TScSSHServer.AfterChannelClosed(Sender: TObject; ChannelInfo: TScSSHChannelInfo);
begin
  try
    if TScSSHChannelInfoUtils.GetIsShell(ChannelInfo) then
      DoAfterShellDisconnect(Sender, ChannelInfo.Client);
  finally
    DoAfterChannelDisconnect(Sender, ChannelInfo);
  end;
end;

procedure TScSSHServer.DoChannelError(Sender: TObject; ChannelInfo: TScSSHChannelInfo; E: Exception);
begin
  if Assigned(FOnChannelError) then
    FOnChannelError(Self, ChannelInfo, E);
end;

procedure TScSSHServer.DoDataFromClient(Sender: TObject; ChannelInfo: TScSSHChannelInfo;
  const Buffer: TBytes; const Offset, Count: integer);
begin
  if Assigned(FOnDataFromClient) then
    FOnDataFromClient(Self, ChannelInfo, Buffer, Offset, Count);
end;

procedure TScSSHServer.DoDataToClient(Sender: TObject; ChannelInfo: TScSSHChannelInfo;
  const Buffer: TBytes; const Offset, Count: integer);
begin
  if Assigned(FOnDataToClient) then
    FOnDataToClient(Self, ChannelInfo, Buffer, Offset, Count);
end;

procedure TScSSHServer.DoBeforeShellConnect(Sender: TObject; ClientInfo: TScSSHClientInfo);
begin
  if Assigned(FBeforeShellConnect) then
    FBeforeShellConnect(Self, ClientInfo);
end;

procedure TScSSHServer.DoAfterShellDisconnect(Sender: TObject; ClientInfo: TScSSHClientInfo);
begin
  if Assigned(FAfterShellDisconnect) then
    FAfterShellDisconnect(Self, ClientInfo);
end;

procedure TScSSHServer.DoRemotePortForwardingRequest(Sender: TObject; ClientInfo: TScSSHClientInfo;
  const Host: string; const Port: Integer; var Allow: Boolean);
begin
  if Assigned(FOnRemotePortForwardingRequest) then
    FOnRemotePortForwardingRequest(Self, ClientInfo, Host, Port, Allow);
end;

procedure TScSSHServer.DoCancelRemotePortForwardingRequest(Sender: TObject;
  ClientInfo: TScSSHClientInfo; const Host: string; const Port: Integer);
begin
  if Assigned(FOnCancelRemotePortForwardingRequest) then
    FOnCancelRemotePortForwardingRequest(Self, ClientInfo, Host, Port);
end;

{ TScSSHServerConnection }

constructor TScSSHServerConnection.Create(AOwner: TComponent);
begin
  inherited;

  FChannels := TCRThreadList.Create;
end;

destructor TScSSHServerConnection.Destroy;
begin
  ClearThreads;
  FChannels.Free;

  inherited;
end;

procedure TScSSHServerConnection.ClearThreads;
var
  Threads: TCRList;
  i: Integer;
begin
  Threads := FChannels.LockList;
  try
    for i := Threads.Count - 1 downto 0 do begin
    {$IFNDEF AUTOREFCOUNT}
      TObject(Threads[i]).Free;
    {$ENDIF}
      Threads.Delete(i);
    end;
  finally
    FChannels.UnlockList;
  end;

  inherited;
end;

procedure TScSSHServerConnection.DoAfterConnect(Sender: TObject);
begin
  FParentSSHServer.DoAfterClientConnect(Sender, ClientInfo);
end;

procedure TScSSHServerConnection.DoAfterDisconnect(Sender: TObject);
begin
  FParentSSHServer.DoAfterClientDisconnect(Sender, ClientInfo);
end;

procedure TScSSHServerConnection.OnError(Sender: TObject; Error: Exception);
begin
  FParentSSHServer.DoClientError(Sender, ClientInfo, Error);
end;

procedure TScSSHServerConnection.DoOnPortForwardingConnect(Sender: TObject; ChannelInfo: TScSSHChannelInfo; var Direct: boolean);
var
  Vio: TCRVioTcp;
begin
  FParentSSHServer.DoBeforeChannelConnect(Sender, ChannelInfo, Direct);

  if not Direct then begin
    Vio := TScSSHChannelInfoUtils.GetTmpObj(ChannelInfo) as TCRVioTcp;
    if Vio <> nil then
      Vio.Connect;
  end;
end;

function TScSSHServerConnection.NewSSHChannel(const ChannelRequest: TChannelRequest): TObject;
begin
  Result := inherited NewSSHChannel(ChannelRequest);

  TScSSHChannelUtils.SetBeforeChannelConnect(TScSSHChannel(Result), FParentSSHServer.DoBeforeChannelConnect);
  TScSSHChannelUtils.SetAfterChannelDisconnect(TScSSHChannel(Result), FParentSSHServer.AfterChannelClosed);
  TScSSHChannelUtils.SetOnChannelError(TScSSHChannel(Result), FParentSSHServer.DoChannelError);
end;

procedure TScSSHServerConnection.EstablishPortForwarding(const ChannelRequest: TChannelRequest);
var
  NewChannel: TScSSHChannel;
  NewVio: TCRVioTcp;
begin
  NewVio := nil;
  NewChannel := TScSSHChannel(NewSSHChannel(ChannelRequest));

  try
    NewVio := TCRVioTcp.Create;
    NewVio.Host := ChannelRequest.Host;
    NewVio.Port := ChannelRequest.Port;
    NewVio.ConnectionTimeout := Timeout;
    NewVio.IPVersion := Options.IPVersion;

    TScSSHChannelInfoUtils.SetTmpObj(NewChannel.ChannelInfo, NewVio);
    TScSSHChannelUtils.SetBeforeChannelConnect(NewChannel, DoOnPortForwardingConnect); // to connect NewVio if not Direct mode
    TScSSHChannelUtils.SetOnDataFromClient(NewChannel, FParentSSHServer.DoDataFromClient);
    TScSSHChannelUtils.SetOnDataToClient(NewChannel, FParentSSHServer.DoDataToClient);
    NewChannel.Connect;
  except
    NewChannel.Free;
    NewVio.Free;
    raise;
  end;

  if not NewChannel.ChannelInfo.Direct then begin
    Assert(NewVio <> nil);
    TListRWFlows(FListFlows).CreateSocketToChannelTransferring(NewChannel, NewVio);
  end
  else begin
    NewVio.Free;
    TListRWFlows(FListFlows).CreateReadChannelThread(NewChannel);
  end;
end;

procedure TScSSHServerConnection.EstablishRemotePortForwarding(const Host: string; const Port: Integer);
var
  NewChannel: TScSSHChannel;
begin
  NewChannel := TScSSHChannel.Create(nil);
  try
    NewChannel.Client := Self;

    NewChannel.OnError := OnError;
    TScSSHChannelUtils.SetOnChannelError(NewChannel, FParentSSHServer.DoChannelError);

    NewChannel.Direct := False;
    NewChannel.DestHost := Host;
    NewChannel.DestPort := Port;
    if (Host = '0.0.0.0') or (Host = '::') then
      NewChannel.GatewayPorts := True;
    NewChannel.SourcePort := Port;
    NewChannel.Timeout := Timeout;
    NewChannel.IPVersion := Options.IPVersion;

    while not Connected do;

    NewChannel.Connect;
  except
    NewChannel.Free;
    raise;
  end;

  FChannels.Add(NewChannel);
end;

procedure TScSSHServerConnection.CancelRemotePortForwarding(const Host: string; const Port: Integer);
var
  Threads: TCRList;
  i: Integer;
begin
  Threads := FChannels.LockList;
  try
    for i := Threads.Count - 1 downto 0 do begin
      if TScSSHChannel(Threads[i]).SourcePort = Port then begin
        try
          TScSSHChannel(Threads[i]).Disconnect;
        finally
        {$IFNDEF AUTOREFCOUNT}
          TScSSHChannel(Threads[i]).Free;
        {$ENDIF}
          Threads.Delete(i);
        end;
      end;
    end;
  finally
    FChannels.UnLockList;
  end;
end;

function TScSSHServerConnection.ExtractChannel(Stream: TObject): TScSSHChannel;
var
  Threads: TCRList;
  i: Integer;
begin
  Result := nil;

  Threads := FChannels.LockList;
  try
    for i := Threads.Count - 1 downto 0 do begin
      if TScSSHChannelUtils.GetStream(TScSSHChannel(Threads[i])) = Stream then begin
        Result := TScSSHChannel(Threads[i]);
        Threads.Delete(i);
        Exit;
      end;
    end;
  finally
    FChannels.UnLockList;
  end;
end;

procedure TScSSHServerConnection.EstablishSession(const ChannelRequest: TChannelRequest);
var
  NewChannel: TScSSHChannel;
begin
  NewChannel := TScSSHChannel(NewSSHChannel(ChannelRequest));

  try
    NewChannel.Connect;
  except
    NewChannel.Free;
    raise;
  end;

  FChannels.Add(NewChannel);
end;

procedure TScSSHServerConnection.OpenSession(Stream: TObject; TerminalInfo: TScTerminalInfo;
  const Request: string; const Command: string);
var
  NewChannel: TScSSHChannel;
  SFTPServerHandler: TScSFTPServerHandler;
begin
  if not ((Request = 'shell') or (Request = 'exec') or (Request = 'sftp')) then
    raise EScError.Create(seUnexpectedPacketType);

  if Request = 'sftp' then begin
    if FParentSSHServer.SFTPServer = nil then
      raise EScError.Create(seServerNotSupportSFTP);

    NewChannel := ExtractChannel(Stream);
    Assert(NewChannel <> nil);

    SFTPServerHandler := TScSFTPServerHandler.Create(NewChannel, FParentSSHServer.SFTPServer, ClientInfo);
    TListRWFlows(FListFlows).CreateSubSystemThread(NewChannel, SFTPServerHandler);
  end
  else begin
    if not FConnection.Params.AllowedShell then
      raise EScError.Create(seServerNotSupportShell);

    NewChannel := ExtractChannel(Stream);
    Assert(NewChannel <> nil);

    try
      FParentSSHServer.DoBeforeShellConnect(Self, ClientInfo);
      TScSSHChannelInfoUtils.SetIsShell(NewChannel.ChannelInfo, True);

      if NewChannel.ChannelInfo.Direct then begin
        TScSSHChannelUtils.SetOnDataFromClient(NewChannel, FParentSSHServer.DoDataFromClient);
        TScSSHChannelUtils.SetOnDataToClient(NewChannel, FParentSSHServer.DoDataToClient);
        TListRWFlows(FListFlows).CreateReadChannelThread(NewChannel);
      end
      else begin
      {$IFDEF MSWINDOWS}
        TListRWFlows(FListFlows).CreateShellToChannelTransferring(NewChannel,
          TerminalInfo, FConnection.Params.UserName, ClientInfo.Domain, FConnection.Params.Password,
          TScSSHConnectionInfoUtils.GetOSAuthentication(ClientInfo), Command);
      {$ELSE}
        raise EScError.Create(seInvalidMessage);
      {$ENDIF}
      end;
    except
      FChannels.Add(NewChannel);
      raise;
    end;
  end;
end;

{ TScSSHServerUtils }

class procedure TScSSHServerUtils.CheckUserPass(Obj: TScSSHServer; ClientInfo: TScSSHClientInfo; const Password: string; var Accept: boolean);
begin
  Obj.DoCheckUserPass(Obj, ClientInfo, Password, Accept);
end;

class procedure TScSSHServerUtils.CheckUserKey(Obj: TScSSHServer; ClientInfo: TScSSHClientInfo; Key: TScKey; var Accept: boolean);
begin
  Obj.DoCheckUserKey(Obj, ClientInfo, Key, Accept);
end;

end.
