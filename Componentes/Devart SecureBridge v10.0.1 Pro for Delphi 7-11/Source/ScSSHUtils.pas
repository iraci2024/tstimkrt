
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScSSHUtils;

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
{$ENDIF}
  {$IFDEF VER16P}System.Types,{$ELSE}Types,{$ENDIF}
{$IFDEF POSIX}
  Posix.SysSocket, Posix.Unistd, Posix.Stdio,
{$ENDIF}
{$IFDEF UNIX}
  Sockets,
{$ENDIF}
  Classes, SysUtils, ScVio, ScVioTcp,
  ScCLRClasses, ScConsts, ScUtils, ScTypes,
  ScBigInteger, ScRNG;

type
  TScSSHProtocol = (pSSH1, pSSH2);

  TScChannelType = (ctForwardedLocalToRemote, ctForwardedRemoteToLocal, ctSession);

  TScSSHAuthentication = (atPublicKey, atPassword, atKeyboardInteractive);
  TScSSHAuthentications = set of TScSSHAuthentication;

  TScSSHCipherItem = class(TScCollectionItem)
  private
    FAlgorithm: TScSymmetricAlgorithm;
  protected
    function GetAsString: string; override;
    procedure SetAsString(const Value: string); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    property AsString: string read GetAsString write SetAsString;
  published
    property Algorithm: TScSymmetricAlgorithm read FAlgorithm write FAlgorithm;
  end;

  TScSSHCiphers = class(TScCollection)
  public
    constructor Create(AOwner: TPersistent);
  end;

  TScSSHHMacAlgorithmItem = class(TScCollectionItem)
  private
    FAlgorithm: TScHMACAlgorithm;
  protected
    function GetAsString: string; override;
    procedure SetAsString(const Value: string); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    property AsString: string read GetAsString write SetAsString;
  published
    property Algorithm: TScHMACAlgorithm read FAlgorithm write FAlgorithm;
  end;

  TScSSHHMacAlgorithms = class(TScCollection)
  public
    constructor Create(AOwner: TPersistent);
  end;

  TScSSHKeyExchangeAlgorithmItem = class(TScCollectionItem)
  private
    FAlgorithm: TScKeyExchangeAlgorithm;
  protected
    function GetAsString: string; override;
    procedure SetAsString(const Value: string); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    property AsString: string read GetAsString write SetAsString;
  published
    property Algorithm: TScKeyExchangeAlgorithm read FAlgorithm write FAlgorithm;
  end;

  TScSSHKeyExchangeAlgorithms = class(TscCollection)
  public
    constructor Create(AOwner: TPersistent);
  end;

  TScSSHHostKeyAlgorithmItem = class(TScCollectionItem)
  private
    FAlgorithm: TScAsymmetricAlgorithm;
  protected
    function GetAsString: string; override;
    procedure SetAsString(const Value: string); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    property AsString: string read GetAsString write SetAsString;
  published
    property Algorithm: TScAsymmetricAlgorithm read FAlgorithm write FAlgorithm;
  end;

  TScSSHHostKeyAlgorithms = class(TscCollection)
  public
    constructor Create(AOwner: TPersistent);
  end;

  // Established connection information
  TScSSHConnectionInfo = class
  protected
    FUserExtData: string;

    FVersion: string;
    FUser: string;
    FDomain: string;
    FOSAuthentication: boolean;
    FHomePath: string;
    FKeyExchangeAlgorithm: TScKeyExchangeAlgorithm;
    FHostKeyAlgorithm: TScAsymmetricAlgorithm;
    FHostKeyAlgorithmName: string;
    FCiphersClient: TScSSHCiphers;
    FCiphersServer: TScSSHCiphers;
    FCipherClient: TScSymmetricAlgorithm;
    FCipherServer: TScSymmetricAlgorithm;
    FHMACAlgorithms: TScSSHHMacAlgorithms;
    FHMACClient: TScHMACAlgorithm;
    FHMACServer: TScHMACAlgorithm;
    FCompressionClient: TScCompressionAlgorithm;
    FCompressionServer: TScCompressionAlgorithm;
    FSSHChannelPermissions: TScSSHChannelPermissions;
    FOnFree: TNotifyEvent;
    FTCPConnection: TScTCPConnection;

    procedure SetCiphersClient(Value: TScSSHCiphers);
    procedure SetCiphersServer(Value: TScSSHCiphers);
    procedure SetHMACAlgorithms(Value: TScSSHHMacAlgorithms);
    function GetTCPConnection: TScTCPConnection;

  public
    constructor Create;
    destructor Destroy; override;

    property UserExtData: string read FUserExtData;

    property Version: string read FVersion; // application version of other side
    property User: string read FUser;
    property Domain: string read FDomain;

    property KeyExchangeAlgorithm: TScKeyExchangeAlgorithm read FKeyExchangeAlgorithm;
    property HostKeyAlgorithm: TScAsymmetricAlgorithm read FHostKeyAlgorithm;
    property HostKeyAlgorithmName: string read FHostKeyAlgorithmName;
    property CiphersClient: TScSSHCiphers read FCiphersClient;
    property CiphersServer: TScSSHCiphers read FCiphersServer;
    property CipherClient: TScSymmetricAlgorithm read FCipherClient;
    property CipherServer: TScSymmetricAlgorithm read FCipherServer;
    property HMACAlgorithms: TScSSHHMacAlgorithms read FHMACAlgorithms;
    property HMACClient: TScHMACAlgorithm read FHMACClient;
    property HMACServer: TScHMACAlgorithm read FHMACServer;
    property CompressionClient: TScCompressionAlgorithm read FCompressionClient;
    property CompressionServer: TScCompressionAlgorithm read FCompressionServer;
    property SSHChannelPermissions: TScSSHChannelPermissions read FSSHChannelPermissions;
    property TCPConnection: TScTCPConnection read GetTCPConnection;
  end;

  TScSSHClientInfo = class(TScSSHConnectionInfo)
  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FData: TObject;
  public
    property Data: TObject read FData write FData; // Users object
  end;

  TScSSHChannelInfo = class
  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FData: TObject;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FTmpObj: TObject;
    FClient: TScSSHClientInfo;
    FDirect: boolean;
    FRemote: boolean;
    FDestHost: string;
    FDestPort: integer;
    FSSHChannel: TObject;
    FIsSession: boolean;
    FIsShell: boolean;
    FOnFree: TNotifyEvent;

  public
    destructor Destroy; override;

    property Client: TScSSHClientInfo read FClient;
    property Direct: boolean read FDirect;
    property Remote: boolean read FRemote;
    property IsSession: boolean read FIsSession;
    property DestHost: string read FDestHost;
    property DestPort: integer read FDestPort;

    property Data: TObject read FData write FData; // Users object
  end;

  TScSFTPSessionInfo = class
  protected
    FData: TObject;
    FClient: TScSSHClientInfo;
    FHomePath: string;
    FUseUnicode: boolean;
    FVersion: integer;
    FEOL: string;

  public
    constructor Create(Client: TScSSHClientInfo);

    property Client: TScSSHClientInfo read FClient;
    property HomePath: string read FHomePath write FHomePath;
    property UseUnicode: boolean read FUseUnicode write FUseUnicode;
    property Version: integer read FVersion;
  {$HPPEMIT '#ifdef EOL'}
  {$HPPEMIT '#undef EOL'}
  {$HPPEMIT '#endif'}
    property EOL: string read FEOL write FEOL;

    property Data: TObject read FData write FData; // Users object
  end;

  // TScTerminalMode = type of byte;

  TScTerminalInfo = class(TPersistent)
  private
    FCols: integer;
    FRows: integer;
    FHeight: integer;
    FWidth: integer;
    FTerminalType: string;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadTerminalType(Reader: TReader);
    procedure WriteTerminalType(Writer: TWriter);
  public
    constructor Create;

  published
    property Cols: integer read FCols write FCols default 80; // characters. The Cols/Rows dimensions override the pixel dimensions (when nonzero)
    property Rows: integer read FRows write FRows default 25; // rows count
    property Height: integer read FHeight write FHeight default 480; // pixels
    property Width: integer read FWidth write FWidth default 640; // pixels
    property TerminalType: string read FTerminalType write FTerminalType stored False; // TERM environment variable value
    // property TerminalModes[Index: TScTerminalMode]: UInt32;
  end;

  TScSSHConnectionInfoUtils = class
  public
    class function GetOSAuthentication(Obj: TScSSHConnectionInfo): boolean;
    class procedure SetVersion(Obj: TScSSHConnectionInfo; const Version: string);
    class procedure SetUser(Obj: TScSSHConnectionInfo; const User: string);
    class procedure SetDomain(Obj: TScSSHConnectionInfo; const Domain: string);
    class procedure SetOSAuthentication(Obj: TScSSHConnectionInfo; const OSAuthentication: boolean);
    class procedure SetHomePath(Obj: TScSSHConnectionInfo; const HomePath: string);
    class procedure SetUserExtData(Obj: TScSSHConnectionInfo; const UserExtData: string);
    class procedure SetKeyExchangeAlgorithm(Obj: TScSSHConnectionInfo; const KeyExchangeAlgorithm: TScKeyExchangeAlgorithm);
    class procedure SetHostKeyAlgorithm(Obj: TScSSHConnectionInfo; const HostKeyAlgorithm: TScAsymmetricAlgorithm);
    class procedure SetHostKeyAlgorithmName(Obj: TScSSHConnectionInfo; const HostKeyAlgorithmName: string);
    class procedure SetCiphersClient(Obj: TScSSHConnectionInfo; CiphersClient: TScSSHCiphers);
    class procedure SetCiphersServer(Obj: TScSSHConnectionInfo; CiphersServer: TScSSHCiphers);
    class procedure SetHMACAlgorithms(Obj: TScSSHConnectionInfo; HMacAlgorithms: TScSSHHMacAlgorithms);
    class procedure SetCipherClient(Obj: TScSSHConnectionInfo; const CipherClient: TScSymmetricAlgorithm);
    class procedure SetCipherServer(Obj: TScSSHConnectionInfo; const CipherServer: TScSymmetricAlgorithm);
    class procedure SetHMACClient(Obj: TScSSHConnectionInfo; const HMACClient: TScHMACAlgorithm);
    class procedure SetHMACServer(Obj: TScSSHConnectionInfo; const HMACServer: TScHMACAlgorithm);
    class procedure SetCompressionClient(Obj: TScSSHConnectionInfo; const CompressionClient: TScCompressionAlgorithm);
    class procedure SetCompressionServer(Obj: TScSSHConnectionInfo; const CompressionServer: TScCompressionAlgorithm);
    class procedure SetSSHChannelPermissions(Obj: TScSSHConnectionInfo; const SSHChannelPermissions: TScSSHChannelPermissions);
    class procedure SetTCPConnection(Obj: TScSSHConnectionInfo; Vio: TCRVioTcp);
    class procedure SetOnFree(Obj: TScSSHConnectionInfo; OnFree: TNotifyEvent);
  end;

  TScSSHChannelInfoUtils = class
  public
    class procedure SetClientInfo(Obj: TScSSHChannelInfo; ClientInfo: TScSSHClientInfo);
    class procedure SetDestHost(Obj: TScSSHChannelInfo; const DestHost: string);
    class procedure SetDestPort(Obj: TScSSHChannelInfo; DestPort: integer);
    class procedure SetDirect(Obj: TScSSHChannelInfo; Direct: boolean);
    class procedure SetRemote(Obj: TScSSHChannelInfo; Remote: boolean);
    class procedure SetIsSession(Obj: TScSSHChannelInfo; IsSession: boolean);
    class procedure SetIsShell(Obj: TScSSHChannelInfo; IsShell: boolean);
    class function GetIsShell(Obj: TScSSHChannelInfo): boolean;
    class procedure SetSSHChannel(Obj: TScSSHChannelInfo; SSHChannel: TObject);
    class function GetSSHChannel(Obj: TScSSHChannelInfo): TObject;
    class procedure SetOnFree(Obj: TScSSHChannelInfo; OnFree: TNotifyEvent);
    class procedure SetTmpObj(Obj: TScSSHChannelInfo; Value: TObject);
    class function GetTmpObj(Obj: TScSSHChannelInfo): TObject;
  end;

  TScSFTPSessionInfoUtils = class
  public
    class procedure SetVersion(Obj: TScSFTPSessionInfo; Version: integer);
  end;

  TSshUtils = class
  public
    class function ClientVersionString(p: TScSSHProtocol): string;
    class function ServerVersionString(p: TScSSHProtocol): string;
  end;

  TChannelRequest = record
    ChannelType: TScChannelType;
    Host: string;
    Port: Integer;
    RemoteChannelID: integer;
    RemoteWindowSize: cardinal;
    RemoteMaxPacketSize: cardinal;
  end;

  TScBeforeClientConnectEvent = procedure(Sender: TObject; const SockAddr: PSockAddr; var Cancel: boolean) of object;
  TScClientEvent = procedure(Sender: TObject; ClientInfo: TScSSHClientInfo) of object;
  TScClientError = procedure(Sender: TObject; ClientInfo: TScSSHClientInfo; E: Exception) of object;
  TScBeforeChannelConnect = procedure(Sender: TObject; ChannelInfo: TScSSHChannelInfo; var Direct: boolean) of object;
  TScAfterChannelDisconnect = procedure(Sender: TObject; ChannelInfo: TScSSHChannelInfo) of object;
  TScChannelError = procedure(Sender: TObject; ChannelInfo: TScSSHChannelInfo; E: Exception) of object;
  TScBeforeShellConnect = procedure(Sender: TObject; ClientInfo: TScSSHClientInfo) of object;
  TScAfterShellDisconnect = procedure(Sender: TObject; ClientInfo: TScSSHClientInfo) of object;
  TScAuthenticationPromptEvent = procedure(Sender: TObject; const Name, Instruction: string;
    const Prompts: TStringDynArray; var Responses: TStringDynArray) of object; //keyboard-interactive only

  TScOnGetDHEGroup = procedure (Sender: TObject; MinBits, ReqBits, MaxBits: integer;
    aRandom: IScRandom; out PrimeSafe, Generator: TBigInteger) of object;

  TScOnRemotePortForwardingRequest = procedure(Sender: TObject; ClientInfo: TScSSHClientInfo;
    const Host: string; const Port: Integer; var Allow: Boolean) of object;
  TScOnCancelRemotePortForwardingRequest = procedure(Sender: TObject;
    ClientInfo: TScSSHClientInfo; const Host: string; const Port: Integer) of object;
  TScOnEstablishRemotePortForwarding = procedure (const Host: string; const Port: Integer) of object;
  TScOnEstablishPortForwarding = procedure (const ChannelRequest: TChannelRequest) of object;
  TScOnEstablishSession = procedure (const ChannelRequest: TChannelRequest) of object;
  TScOnOpenSession = procedure (Channel: TObject; TerminalInfo: TScTerminalInfo; const Request: string; const Command: string) of object;

  TScSocketEvent = procedure(Sender: TObject; const SockAddr: PSockAddr) of object;
  TScData = procedure(Sender: TObject; ChannelInfo: TScSSHChannelInfo;
    const Buffer: TBytes; const Offset, Count: integer) of object;

const
  SSH_CHANNEL_PACKET_SIZE = 128 * 1024;
  SSH_MAX_PACKET_SIZE = SSH_CHANNEL_PACKET_SIZE + 2048;

const
  ListReKeyLimit: array[0..3] of string = ('256M', '512M', '1G', '2G');

implementation

uses
  ScFunctions, ScAlgorithmSupport;

{ TScSSHCipherItem }

procedure TScSSHCipherItem.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScSSHCipherItem) then begin
    TScSSHCipherItem(Dest).FAlgorithm := FAlgorithm;
  end
  else
    inherited;
end;

function TScSSHCipherItem.GetAsString: string;
begin
  Result := CipherFactory.CipherAlgorithmToSSH2Name(FAlgorithm);
end;

procedure TScSSHCipherItem.SetAsString(const Value: string);
begin
  FAlgorithm := CipherFactory.SSH2NameToCipherAlgorithm(LowerCase(Trim(Value)));
end;

{ TScSSHCiphers }

constructor TScSSHCiphers.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TScSSHCipherItem);
end;

{ TScSSHHMacAlgorithmItem }

procedure TScSSHHMacAlgorithmItem.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScSSHHMacAlgorithmItem) then begin
    TScSSHHMacAlgorithmItem(Dest).FAlgorithm := FAlgorithm;
  end
  else
    inherited;
end;

function TScSSHHMacAlgorithmItem.GetAsString: string;
begin
  Result := CipherFactory.HMACAlgorithmToSSH2Name(FAlgorithm);
end;

procedure TScSSHHMacAlgorithmItem.SetAsString(const Value: string);
begin
  FAlgorithm := CipherFactory.SSH2NameToHMACAlgorithm(LowerCase(Trim(Value)));
end;

{ TScSSHHMacAlgorithms }

constructor TScSSHHMacAlgorithms.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TScSSHHMacAlgorithmItem);
end;

{ TScSSHKeyExchangeAlgorithmItem }

procedure TScSSHKeyExchangeAlgorithmItem.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScSSHKeyExchangeAlgorithmItem) then begin
    TScSSHKeyExchangeAlgorithmItem(Dest).FAlgorithm := FAlgorithm;
  end
  else
    inherited;
end;

function TScSSHKeyExchangeAlgorithmItem.GetAsString: string;
begin
  Result := CipherFactory.KeyExchangeAlgorithmToSSH2Name(FAlgorithm);
end;

procedure TScSSHKeyExchangeAlgorithmItem.SetAsString(const Value: string);
begin
  FAlgorithm := CipherFactory.SSH2NameToKeyExchangeAlgorithm(LowerCase(Trim(Value)));
end;

{ TScSSHKeyExchangeAlgorithms }

constructor TScSSHKeyExchangeAlgorithms.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TScSSHKeyExchangeAlgorithmItem);
end;

{ TScSSHHostKeyAlgorithmItem }

procedure TScSSHHostKeyAlgorithmItem.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScSSHHostKeyAlgorithmItem) then begin
    TScSSHHostKeyAlgorithmItem(Dest).FAlgorithm := FAlgorithm;
  end
  else
    inherited;
end;

function TScSSHHostKeyAlgorithmItem.GetAsString: string;
begin
  Result := CipherFactory.PublicKeyAlgorithmToSSH2Name(FAlgorithm);
end;

procedure TScSSHHostKeyAlgorithmItem.SetAsString(const Value: string);
begin
  FAlgorithm := CipherFactory.SSH2NameToPublicKeyAlgorithm(LowerCase(Trim(Value)));
end;

{ TScSSHHostKeyAlgorithms }

constructor TScSSHHostKeyAlgorithms.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TScSSHHostKeyAlgorithmItem);
end;

{ TScSSHConnectionInfo }

constructor TScSSHConnectionInfo.Create;
begin
  inherited;

  FSSHChannelPermissions := [cpAllowLocalForwarding, cpAllowRemoteForwarding, cpAllowShell, cpAllowSFTP];
  FCiphersClient := TScSSHCiphers.Create(nil);
  FCiphersServer := TScSSHCiphers.Create(nil);
  FHMACAlgorithms := TScSSHHMacAlgorithms.Create(nil);
end;

destructor TScSSHConnectionInfo.Destroy;
begin
  if Assigned(FOnFree) then
    FOnFree(Self);

  FCiphersClient.Free;
  FCiphersServer.Free;
  FHMACAlgorithms.Free;
  FTCPConnection.Free;

  inherited;
end;

function TScSSHConnectionInfo.GetTCPConnection: TScTCPConnection;
begin
  if FTCPConnection = nil then
    FTCPConnection := TScTCPConnection.Create;
  Result := FTCPConnection;
end;

procedure TScSSHConnectionInfo.SetCiphersClient(Value: TScSSHCiphers);
begin
  if FCiphersClient <> Value then
    FCiphersClient.Assign(Value);
end;

procedure TScSSHConnectionInfo.SetCiphersServer(Value: TScSSHCiphers);
begin
  if FCiphersServer <> Value then
    FCiphersServer.Assign(Value);
end;

procedure TScSSHConnectionInfo.SetHMACAlgorithms(Value: TScSSHHMacAlgorithms);
begin
  if FHMACAlgorithms <> Value then
    FHMACAlgorithms.Assign(Value);
end;

{ TScSSHChannelInfo }

destructor TScSSHChannelInfo.Destroy;
begin
  if Assigned(FOnFree) then
    FOnFree(Self);

  inherited;
end;

{ TScSFTPSessionInfo }

constructor TScSFTPSessionInfo.Create(Client: TScSSHClientInfo);
begin
  inherited Create;

  FClient := Client;
  FHomePath := Client.FHomePath;
end;

{ TScTerminalInfo }

constructor TScTerminalInfo.Create;
begin
  inherited;

  FCols := 80;
  FRows := 25;
  FWidth := 640;
  FHeight := 480;
  FTerminalType := 'vt100';
end;

procedure TScTerminalInfo.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScTerminalInfo) then begin
    TScTerminalInfo(Dest).FCols := FCols;
    TScTerminalInfo(Dest).FRows := FRows;
    TScTerminalInfo(Dest).FHeight := FHeight;
    TScTerminalInfo(Dest).FWidth := FWidth;
    TScTerminalInfo(Dest).FTerminalType := FTerminalType;
  end
  else
    inherited;
end;

procedure TScTerminalInfo.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);

  Filer.DefineProperty('TerminalType', ReadTerminalType, WriteTerminalType, FTerminalType <> 'vt100');
end;

procedure TScTerminalInfo.ReadTerminalType(Reader: TReader);
begin
  FTerminalType := Reader.ReadString;
end;

procedure TScTerminalInfo.WriteTerminalType(Writer: TWriter);
begin
  Writer.WriteString(FTerminalType);
end;

{ TScSSHConnectionInfoUtils }

class function TScSSHConnectionInfoUtils.GetOSAuthentication(Obj: TScSSHConnectionInfo): boolean;
begin
  Result := Obj.FOSAuthentication;
end;

class procedure TScSSHConnectionInfoUtils.SetVersion(Obj: TScSSHConnectionInfo; const Version: string);
begin
  Obj.FVersion := Version;
end;

class procedure TScSSHConnectionInfoUtils.SetUser(Obj: TScSSHConnectionInfo; const User: string);
begin
  Obj.FUser := User;
end;

class procedure TScSSHConnectionInfoUtils.SetDomain(Obj: TScSSHConnectionInfo; const Domain: string);
begin
  Obj.FDomain := Domain;
end;

class procedure TScSSHConnectionInfoUtils.SetOSAuthentication(Obj: TScSSHConnectionInfo; const OSAuthentication: boolean);
begin
  Obj.FOSAuthentication := OSAuthentication;
end;

class procedure TScSSHConnectionInfoUtils.SetHomePath(Obj: TScSSHConnectionInfo; const HomePath: string);
begin
  Obj.FHomePath := HomePath;
end;

class procedure TScSSHConnectionInfoUtils.SetUserExtData(Obj: TScSSHConnectionInfo; const UserExtData: string);
begin
  Obj.FUserExtData := UserExtData;
end;

class procedure TScSSHConnectionInfoUtils.SetKeyExchangeAlgorithm(Obj: TScSSHConnectionInfo; const KeyExchangeAlgorithm: TScKeyExchangeAlgorithm);
begin
  Obj.FKeyExchangeAlgorithm := KeyExchangeAlgorithm;
end;

class procedure TScSSHConnectionInfoUtils.SetHostKeyAlgorithm(Obj: TScSSHConnectionInfo; const HostKeyAlgorithm: TScAsymmetricAlgorithm);
begin
  Obj.FHostKeyAlgorithm := HostKeyAlgorithm;
end;

class procedure TScSSHConnectionInfoUtils.SetHostKeyAlgorithmName(Obj: TScSSHConnectionInfo; const HostKeyAlgorithmName: string);
begin
  Obj.FHostKeyAlgorithmName := HostKeyAlgorithmName;
end;

class procedure TScSSHConnectionInfoUtils.SetCiphersClient(Obj: TScSSHConnectionInfo; CiphersClient: TScSSHCiphers);
begin
  Obj.SetCiphersClient(CiphersClient);
end;

class procedure TScSSHConnectionInfoUtils.SetCiphersServer(Obj: TScSSHConnectionInfo; CiphersServer: TScSSHCiphers);
begin
  Obj.SetCiphersServer(CiphersServer);
end;

class procedure TScSSHConnectionInfoUtils.SetHMACAlgorithms(Obj: TScSSHConnectionInfo; HMacAlgorithms: TScSSHHMacAlgorithms);
begin
  Obj.SetHMACAlgorithms(HMacAlgorithms);
end;

class procedure TScSSHConnectionInfoUtils.SetCipherClient(Obj: TScSSHConnectionInfo; const CipherClient: TScSymmetricAlgorithm);
begin
  Obj.FCipherClient := CipherClient;
end;

class procedure TScSSHConnectionInfoUtils.SetCipherServer(Obj: TScSSHConnectionInfo; const CipherServer: TScSymmetricAlgorithm);
begin
  Obj.FCipherServer := CipherServer;
end;

class procedure TScSSHConnectionInfoUtils.SetHMACClient(Obj: TScSSHConnectionInfo; const HMACClient: TScHMACAlgorithm);
begin
  Obj.FHMACClient := HMACClient;
end;

class procedure TScSSHConnectionInfoUtils.SetHMACServer(Obj: TScSSHConnectionInfo; const HMACServer: TScHMACAlgorithm);
begin
  Obj.FHMACServer := HMACServer;
end;

class procedure TScSSHConnectionInfoUtils.SetCompressionClient(Obj: TScSSHConnectionInfo; const CompressionClient: TScCompressionAlgorithm);
begin
  Obj.FCompressionClient := CompressionClient;
end;

class procedure TScSSHConnectionInfoUtils.SetCompressionServer(Obj: TScSSHConnectionInfo; const CompressionServer: TScCompressionAlgorithm);
begin
  Obj.FCompressionServer := CompressionServer;
end;

class procedure TScSSHConnectionInfoUtils.SetSSHChannelPermissions(Obj: TScSSHConnectionInfo; const SSHChannelPermissions: TScSSHChannelPermissions);
begin
  Obj.FSSHChannelPermissions := SSHChannelPermissions;
end;

class procedure TScSSHConnectionInfoUtils.SetTCPConnection(Obj: TScSSHConnectionInfo; Vio: TCRVioTcp);
begin
  FreeAndNil(Obj.FTCPConnection);
  Obj.FTCPConnection := TScTCPConnection.Create(Vio, False);
end;

class procedure TScSSHConnectionInfoUtils.SetOnFree(Obj: TScSSHConnectionInfo; OnFree: TNotifyEvent);
begin
  Obj.FOnFree := OnFree;
end;

{ TScSSHChannelInfoUtils }

class procedure TScSSHChannelInfoUtils.SetClientInfo(Obj: TScSSHChannelInfo; ClientInfo: TScSSHClientInfo);
begin
  Obj.FClient := ClientInfo;
end;

class procedure TScSSHChannelInfoUtils.SetDestHost(Obj: TScSSHChannelInfo; const DestHost: string);
begin
  Obj.FDestHost := DestHost;
end;

class procedure TScSSHChannelInfoUtils.SetDestPort(Obj: TScSSHChannelInfo; DestPort: integer);
begin
  Obj.FDestPort := DestPort;
end;

class procedure TScSSHChannelInfoUtils.SetDirect(Obj: TScSSHChannelInfo; Direct: boolean);
begin
  Obj.FDirect := Direct;
end;

class procedure TScSSHChannelInfoUtils.SetRemote(Obj: TScSSHChannelInfo; Remote: boolean);
begin
  Obj.FRemote := Remote;
end;

class procedure TScSSHChannelInfoUtils.SetIsSession(Obj: TScSSHChannelInfo; IsSession: boolean);
begin
  Obj.FIsSession := IsSession;
end;

class procedure TScSSHChannelInfoUtils.SetIsShell(Obj: TScSSHChannelInfo; IsShell: boolean);
begin
  Obj.FIsShell := IsShell;
end;

class function TScSSHChannelInfoUtils.GetIsShell(Obj: TScSSHChannelInfo): boolean;
begin
  Result := Obj.FIsShell;
end;

class procedure TScSSHChannelInfoUtils.SetSSHChannel(Obj: TScSSHChannelInfo; SSHChannel: TObject);
begin
  Obj.FSSHChannel := SSHChannel;
end;

class function TScSSHChannelInfoUtils.GetSSHChannel(Obj: TScSSHChannelInfo): TObject;
begin
  Result := Obj.FSSHChannel;
end;

class procedure TScSSHChannelInfoUtils.SetOnFree(Obj: TScSSHChannelInfo; OnFree: TNotifyEvent);
begin
  Obj.FOnFree := OnFree;
end;

class procedure TScSSHChannelInfoUtils.SetTmpObj(Obj: TScSSHChannelInfo; Value: TObject);
begin
  Obj.FTmpObj := Value;
end;

class function TScSSHChannelInfoUtils.GetTmpObj(Obj: TScSSHChannelInfo): TObject;
begin
  Result := Obj.FTmpObj;
end;

{ TScSFTPSessionInfoUtils }

class procedure TScSFTPSessionInfoUtils.SetVersion(Obj: TScSFTPSessionInfo; Version: integer);
begin
  Obj.FVersion := Version;
end;

{ TSshUtils }

class function TSshUtils.ClientVersionString(p: TScSSHProtocol): string;
begin
  if p = pSSH1 then
    Result := S_Devart15
  else
    Result := S_Devart20;
end;

class function TSshUtils.ServerVersionString(p: TScSSHProtocol): string;
begin
  if p = pSSH1 then
    Result := S_Devart15
  else
    Result := S_Devart20;
end;

end.

