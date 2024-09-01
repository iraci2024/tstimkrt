
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScSSHClient;

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  {$IFDEF VER16P}System.Types,{$ELSE}Types,{$ENDIF}
  Classes, SyncObjs,
{$IFNDEF FPC}
  ScTypes,
{$ENDIF}
  ScCLRClasses, ScFunctions, ScVio, ScConsts,
  ScClient, ScBridge, ScSSHUtils, ScUtils, ScSSH2Connection;

type
  TScSSHClient = class;

  TScSSHClientOptions = class(TPersistent)
  private
    FClient: TScSSHClient;
    FRekeyLimit: string;
    FServerAliveCountMax: integer;
    FServerAliveInterval: integer;
    FBindAddress: string;
    FTCPKeepAlive: boolean;
    FIPVersion: TIPVersion;
    FClientVersion: string;
    FMsgIgnoreRate: Integer;
    FSocketReceiveBufferSize: integer;
    FSocketSendBufferSize: integer;

    procedure SetRekeyLimit(Value: string);
    procedure SetServerAliveCountMax(Value: integer);
    procedure SetServerAliveInterval(Value: integer);
    procedure SetBindAddress(const Value: string);
    procedure SetTCPKeepAlive(Value: boolean);
    procedure SetIPVersion(const Value: TIPVersion);
    procedure SetClientVersion(const Value: string);
    function NotDefaultVersion: boolean;
    procedure SetMsgIgnoreRate(Value: Integer);
    procedure SetSocketReceiveBufferSize(Value: Integer);
    procedure SetSocketSendBufferSize(Value: Integer);

  protected
    procedure AssignTo(Dest: TPersistent); override;

  public
    constructor Create(Client: TScSSHClient);

  published
    property RekeyLimit: string read FRekeyLimit write SetRekeyLimit; // '2G', '1G', '512M', '128M'
    property ServerAliveCountMax: integer read FServerAliveCountMax write SetServerAliveCountMax default 3;
    property ServerAliveInterval: integer read FServerAliveInterval write SetServerAliveInterval default 0;
    property BindAddress: string read FBindAddress write SetBindAddress; // useful on systems with more than one IP.
    property TCPKeepAlive: boolean read FTCPKeepAlive write SetTCPKeepAlive default True;
    property IPVersion: TIPVersion read FIPVersion write SetIPVersion default DefValIPVersion;
    property ClientVersion: string read FClientVersion write SetClientVersion stored NotDefaultVersion;
    property MsgIgnoreRate: Integer read FMsgIgnoreRate write SetMsgIgnoreRate default 0;
    property SocketReceiveBufferSize: integer read FSocketReceiveBufferSize write SetSocketReceiveBufferSize default DEFAULT_SOCKET_BUFFER_SIZE;
    property SocketSendBufferSize: integer read FSocketSendBufferSize write SetSocketSendBufferSize default DEFAULT_SOCKET_BUFFER_SIZE;
  end;

  // Create SSH-connection from client to server. Multiple channels are multiplexed into a single connection.
{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TScSSHClient = class(TComponent)
  private
    FClients: TThreadList;
    FConnectEvents: TList;
    FStreamedConnected: boolean;
    FAfterConnect: TNotifyEvent;
    FAfterDisconnect: TNotifyEvent;
    FBeforeConnect: TNotifyEvent;
    FBeforeDisconnect: TNotifyEvent;
    FOnBanner: TScBannerEvent;
    FOnServerKeyValidate: TScServerKeyValidationEvent;
    FOnAuthenticationPrompt: TScAuthenticationPromptEvent;
    FCiphersClient: TScSSHCiphers;
    FCiphersServer: TScSSHCiphers;
    FHMACAlgorithms: TScSSHHMacAlgorithms;
    FCompressionClient: TScCompression;
    FCompressionServer: TScCompression;
    FKeyExchangeAlgorithms: TScSSHKeyExchangeAlgorithms;
    FHostKeyAlgorithms: TScSSHHostKeyAlgorithms;
    FAuthentication: TScSSHAuthentication;
    FHostName: string;
    FPort: integer;
    FUser: string;
    FPassword: string;
    FTimeout: integer;
    FClientInfo: TScSSHClientInfo;
    FOptions: TScSSHClientOptions;
    FHttpOptions: THttpOptions;
    FProxyOptions: TProxyOptions;
    FProviderName: string;
    FServerPublicKeyChanged: boolean;
    FNewServerKey: TScKey;
    FPrivateKeyName: string;
    FHostKeyName: string;
    FKeyStorage: TScStorage;
    FLockConnection: TCriticalSection;

    function GetChannelCount: Integer;
    procedure OnPropertyChanged(Sender: TObject);
    procedure SetCiphersClient(Value: TScSSHCiphers);
    procedure SetCiphersServer(Value: TScSSHCiphers);
    procedure SetHMACAlgorithms(Value: TScSSHHMacAlgorithms);
    procedure SetCompressionClient(const Value: TScCompression);
    procedure SetCompressionServer(const Value: TScCompression);
    procedure SetKeyExchangeAlgorithms(Value: TScSSHKeyExchangeAlgorithms);
    procedure SetHostKeyAlgorithms(Value: TScSSHHostKeyAlgorithms);
    procedure SetAuthentication(const Value: TScSSHAuthentication);
    procedure SetHostName(const Value: string);
  {$HPPEMIT '#ifdef SetPort'}
  {$HPPEMIT '#undef SetPort'}
  {$HPPEMIT '#endif'}
    procedure SetPort(const Value: integer);
    procedure SetUser(const Value: string);
    procedure SetPassword(const Value: string);
    procedure SetTimeout(const Value: integer);
    procedure SetOptions(Value: TScSSHClientOptions);
    procedure SetHttpOptions(Value: THttpOptions);
    procedure SetProxyOptions(Value: TProxyOptions);
    procedure SetSocketReceiveBufferSize(Value: Integer);
    procedure SetSocketSendBufferSize(Value: Integer);

    function GetServerVersion: string;
    function NotDefaultCiphersClient: boolean;
    function NotDefaultCiphersServer: boolean;
    function NotDefaultKeyExchangeAlgorithms: boolean;
    function NotDefaultHostKeyAlgorithms: boolean;
    function NotDefaultHMACAlgorithms: boolean;
    procedure SetPrivateKeyName(const Value: string);
    procedure SetHostKeyName(const Value: string);
    procedure SetKeyStorage(Value: TScStorage);
    procedure DoBanner(Sender: TObject; const Banner: string);
    procedure DoAfterDisconnect(Sender: TObject);
    procedure DoAuthenticationPrompt(Sender: TObject; const Name, Instruction: string;
      const Prompts: TStringDynArray; var Responses: TStringDynArray);

  protected
    FVio: TCRVio;
    FConnection: TSsh2Connection;
    FListFlows: TObject;

    procedure Loaded; override;
    procedure Notification(Component: TComponent; Operation: TOperation); override;
    procedure AssignTo(Dest: TPersistent); override;

    procedure CheckInactive;
    function GetConnected: boolean; virtual;
    procedure SetConnected(Value: boolean); virtual;

    procedure CreateVio; virtual;

    procedure DoConnect; virtual;
    procedure DoDisconnect; virtual;
    procedure SendConnectEvent(Connecting: Boolean);
    procedure RegisterClient(Client: TObject; Event: TScConnectChangeEvent);
    procedure UnRegisterClient(Client: TObject);
    procedure DoServerKeyValidation(Sender: TObject; NewServerKey: TScKey; var Accept: boolean);

    function NewSSHChannel(const ChannelRequest: TChannelRequest): TObject; virtual;
    procedure EstablishPortForwarding(const ChannelRequest: TChannelRequest); virtual;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Connect;
    procedure Disconnect;

    function CreateSSHChannel: TObject; virtual;

    property ClientInfo: TScSSHClientInfo read FClientInfo;
//    property Channels[Index: Integer]: TScSSHChannel read GetChannel;
    property ChannelCount: Integer read GetChannelCount;
    property ServerVersion: string read GetServerVersion;

  published
    property Connected: boolean read GetConnected write SetConnected default False;

    property CiphersClient: TScSSHCiphers read FCiphersClient write SetCiphersClient stored NotDefaultCiphersClient; // Lists the acceptable symmetric encryption algorithms in order of preference.
    property CiphersServer: TScSSHCiphers read FCiphersServer write SetCiphersServer stored NotDefaultCiphersServer;
    property HMACAlgorithms: TScSSHHMacAlgorithms read FHMACAlgorithms write SetHMACAlgorithms stored NotDefaultHMACAlgorithms; // Lists the acceptable MAC algorithms in order of preference.
    property KeyExchangeAlgorithms: TScSSHKeyExchangeAlgorithms read FKeyExchangeAlgorithms write SetKeyExchangeAlgorithms stored NotDefaultKeyExchangeAlgorithms; // List of the algorithms supported for the exchange keys.
    property HostKeyAlgorithms: TScSSHHostKeyAlgorithms read FHostKeyAlgorithms write SetHostKeyAlgorithms stored NotDefaultHostKeyAlgorithms; // List of the algorithms supported for the server host key. 'Ssh-rsa,Ssh-dss'
    property CompressionClient: TScCompression read FCompressionClient write SetCompressionClient default csAllowed;
    property CompressionServer: TScCompression read FCompressionServer write SetCompressionServer default csAllowed;

    property Authentication: TScSSHAuthentication read FAuthentication write SetAuthentication default atPassword; // Authentication methods 'gssapi-with-mic, hostbased, publickey, keyboard-interactive, password'
    property HostName: string read FHostName write SetHostName; // SSH-server name
    property Port: integer read FPort write SetPort default SSH_DEFAULT_PORT; // SSH-server port
    property User: string read FUser write SetUser;
    property Password: string read FPassword write SetPassword;
    property Timeout: integer read FTimeout write SetTimeout default DEFAULT_TIMEOUT;
    property Options: TScSSHClientOptions read FOptions write SetOptions;
    property HttpOptions: THttpOptions read FHttpOptions write SetHttpOptions;
    property ProxyOptions: TProxyOptions read FProxyOptions write SetProxyOptions;

    // Keys
    property KeyStorage: TScStorage read FKeyStorage write SetKeyStorage;
    property PrivateKeyName: string read FPrivateKeyName write SetPrivateKeyName;
    property HostKeyName: string read FHostKeyName write SetHostKeyName;

    property AfterConnect: TNotifyEvent read FAfterConnect write FAfterConnect;
    property BeforeConnect: TNotifyEvent read FBeforeConnect write FBeforeConnect;
    property AfterDisconnect: TNotifyEvent read FAfterDisconnect write FAfterDisconnect;
    property BeforeDisconnect: TNotifyEvent read FBeforeDisconnect write FBeforeDisconnect;
    property OnBanner: TScBannerEvent read FOnBanner write FOnBanner;
    // Occur when client have no server key in KeyStorage or when key not verified.
    property OnServerKeyValidate: TScServerKeyValidationEvent read FOnServerKeyValidate write FOnServerKeyValidate;
    property OnAuthenticationPrompt: TScAuthenticationPromptEvent read FOnAuthenticationPrompt write FOnAuthenticationPrompt;
  end;

  TScClientUtils = class
    class procedure RegisterClient(Obj: TScSSHClient; Client: TObject; Event: TScConnectChangeEvent);
    class procedure UnRegisterClient(Obj: TScSSHClient; Client: TObject);

    class function GetConnection(Obj: TScSSHClient): TSsh2Connection;
    class procedure SetConnection(Obj: TScSSHClient; SshConnection: TSsh2Connection);
    class procedure SetVio(Obj: TScSSHClient; vio: TCRVio);
  end;

  function GetRekeyLimitSize(var RekeyLimit: string): Int64;

var
  ShowDlgYesOrNo: function(const Msg: string): boolean = nil;

implementation

uses
{$IFDEF MSWINDOWS}
  WinSock,
{$ENDIF}
{$IFDEF POSIX}
  Posix.NetinetIn, Posix.SysSocket,
{$ENDIF}
{$IFDEF UNIX}
  Sockets,
{$ENDIF}
  SysUtils, ScVioTcp, ScVioHttp, ScSSHConnectionParameter, ScAlgorithmSupport,
  ScSSHSocket, ScSSHChannel;

{ TScSSHClient }

constructor TScSSHClient.Create(AOwner: TComponent);
begin
  inherited;

  FClients := TThreadList.Create;
  FConnectEvents := TList.Create;
  FOptions := TScSSHClientOptions.Create(Self);
  FHttpOptions := THttpOptions.Create;
  FProxyOptions := TProxyOptions.Create;
  FHttpOptions.ProxyOptions := FProxyOptions;

  FCiphersClient := TScSSHCiphers.Create(Self);
  (FCiphersClient.Add as TScSSHCipherItem).Algorithm := saAES256_ctr;
  (FCiphersClient.Add as TScSSHCipherItem).Algorithm := saAES192_ctr;
  (FCiphersClient.Add as TScSSHCipherItem).Algorithm := saAES128_ctr;
  (FCiphersClient.Add as TScSSHCipherItem).Algorithm := saBlowfish_cbc;
  (FCiphersClient.Add as TScSSHCipherItem).Algorithm := saAES256_cbc;
  (FCiphersClient.Add as TScSSHCipherItem).Algorithm := saAES192_cbc;
  (FCiphersClient.Add as TScSSHCipherItem).Algorithm := saAES128_cbc;
  (FCiphersClient.Add as TScSSHCipherItem).Algorithm := saCast128_cbc;
  (FCiphersClient.Add as TScSSHCipherItem).Algorithm := saTripleDES_cbc;
  FCiphersClient.OnChanged := OnPropertyChanged;

  FCiphersServer := TScSSHCiphers.Create(Self);
  (FCiphersServer.Add as TScSSHCipherItem).Algorithm := saAES256_ctr;
  (FCiphersServer.Add as TScSSHCipherItem).Algorithm := saAES192_ctr;
  (FCiphersServer.Add as TScSSHCipherItem).Algorithm := saAES128_ctr;
  (FCiphersServer.Add as TScSSHCipherItem).Algorithm := saBlowfish_cbc;
  (FCiphersServer.Add as TScSSHCipherItem).Algorithm := saAES256_cbc;
  (FCiphersServer.Add as TScSSHCipherItem).Algorithm := saAES192_cbc;
  (FCiphersServer.Add as TScSSHCipherItem).Algorithm := saAES128_cbc;
  (FCiphersServer.Add as TScSSHCipherItem).Algorithm := saCast128_cbc;
  (FCiphersServer.Add as TScSSHCipherItem).Algorithm := saTripleDES_cbc;
  FCiphersServer.OnChanged := OnPropertyChanged;

  FHMACAlgorithms := TScSSHHMacAlgorithms.Create(Self);
  (FHMACAlgorithms.Add as TScSSHHMacAlgorithmItem).Algorithm := hmacSHA2_256;
  (FHMACAlgorithms.Add as TScSSHHMacAlgorithmItem).Algorithm := hmacSHA2_512;
  (FHMACAlgorithms.Add as TScSSHHMacAlgorithmItem).Algorithm := hmacSHA1;
  FHMACAlgorithms.OnChanged := OnPropertyChanged;

  FKeyExchangeAlgorithms := TScSSHKeyExchangeAlgorithms.Create(Self);
  (FKeyExchangeAlgorithms.Add as TScSSHKeyExchangeAlgorithmItem).Algorithm := keCurve25519Sha256;
  (FKeyExchangeAlgorithms.Add as TScSSHKeyExchangeAlgorithmItem).Algorithm := keECDHSha2Nistp521;
  (FKeyExchangeAlgorithms.Add as TScSSHKeyExchangeAlgorithmItem).Algorithm := keECDHSha2Nistp384;
  (FKeyExchangeAlgorithms.Add as TScSSHKeyExchangeAlgorithmItem).Algorithm := keECDHSha2Nistp256;
  (FKeyExchangeAlgorithms.Add as TScSSHKeyExchangeAlgorithmItem).Algorithm := keDHExchSha256;
  (FKeyExchangeAlgorithms.Add as TScSSHKeyExchangeAlgorithmItem).Algorithm := keDHExchSha1;
  (FKeyExchangeAlgorithms.Add as TScSSHKeyExchangeAlgorithmItem).Algorithm := keDHGroup14Sha1;
  (FKeyExchangeAlgorithms.Add as TScSSHKeyExchangeAlgorithmItem).Algorithm := keDHGroup1Sha1;
  FKeyExchangeAlgorithms.OnChanged := OnPropertyChanged;

  FHostKeyAlgorithms := TScSSHHostKeyAlgorithms.Create(Self);
  (FHostKeyAlgorithms.Add as TScSSHHostKeyAlgorithmItem).Algorithm := aaRSA;
  (FHostKeyAlgorithms.Add as TScSSHHostKeyAlgorithmItem).Algorithm := aaEC;
  FHostKeyAlgorithms.OnChanged := OnPropertyChanged;

  FCompressionClient := csAllowed;
  FCompressionServer := csAllowed;
  FAuthentication := atPassword;
  FPort := SSH_DEFAULT_PORT;
  FTimeout := DEFAULT_TIMEOUT;

  FClientInfo := TScSSHClientInfo.Create;
  FListFlows := TListRWFlows.Create;
  FLockConnection := TCriticalSection.Create;
end;

destructor TScSSHClient.Destroy;
  procedure ClearRefs;
  var
    Clients: TList;
    Obj: TObject;
  begin
    Clients := FClients.LockList;
    FClients.UnlockList; // to prevent hanging on closing of daughterly channels by local portforwarding

    while Clients.Count > 0 do begin
      Obj := Clients[0];
      if Obj is TScSSHCustomChannel then
        TScSSHCustomChannel(Obj).Client := nil
      else
        Clients.Remove(Obj);
    end;
  end;

begin
  ClearRefs;
  DoDisconnect;

  FNewServerKey.Free;
  FClientInfo.Free;
  FCiphersClient.Free;
  FCiphersServer.Free;
  FHMACAlgorithms.Free;
  FKeyExchangeAlgorithms.Free;
  FHostKeyAlgorithms.Free;
  FOptions.Free;
  FHttpOptions.Free;
  FProxyOptions.Free;
  FListFlows.Free;

  inherited;
  FLockConnection.Free;
  FConnectEvents.Free;
  FClients.Free;
end;

function TScSSHClient.NotDefaultCiphersClient: boolean;
begin
  Result := not ((FCiphersClient.Count = 9) and
    ((FCiphersClient.Items[0] as TScSSHCipherItem).Algorithm = saAES256_ctr) and
    ((FCiphersClient.Items[1] as TScSSHCipherItem).Algorithm = saAES192_ctr) and
    ((FCiphersClient.Items[2] as TScSSHCipherItem).Algorithm = saAES128_ctr) and
    ((FCiphersClient.Items[3] as TScSSHCipherItem).Algorithm = saBlowfish_cbc) and
    ((FCiphersClient.Items[4] as TScSSHCipherItem).Algorithm = saAES256_cbc) and
    ((FCiphersClient.Items[5] as TScSSHCipherItem).Algorithm = saAES192_cbc) and
    ((FCiphersClient.Items[6] as TScSSHCipherItem).Algorithm = saAES128_cbc) and
    ((FCiphersClient.Items[7] as TScSSHCipherItem).Algorithm = saCast128_cbc) and
    ((FCiphersClient.Items[8] as TScSSHCipherItem).Algorithm = saTripleDES_cbc));
end;

function TScSSHClient.NotDefaultCiphersServer: boolean;
begin
  Result := not ((FCiphersServer.Count = 9) and
    ((FCiphersServer.Items[0] as TScSSHCipherItem).Algorithm = saAES256_ctr) and
    ((FCiphersServer.Items[1] as TScSSHCipherItem).Algorithm = saAES192_ctr) and
    ((FCiphersServer.Items[2] as TScSSHCipherItem).Algorithm = saAES128_ctr) and
    ((FCiphersServer.Items[3] as TScSSHCipherItem).Algorithm = saBlowfish_cbc) and
    ((FCiphersServer.Items[4] as TScSSHCipherItem).Algorithm = saAES256_cbc) and
    ((FCiphersServer.Items[5] as TScSSHCipherItem).Algorithm = saAES192_cbc) and
    ((FCiphersServer.Items[6] as TScSSHCipherItem).Algorithm = saAES128_cbc) and
    ((FCiphersServer.Items[7] as TScSSHCipherItem).Algorithm = saCast128_cbc) and
    ((FCiphersServer.Items[8] as TScSSHCipherItem).Algorithm = saTripleDES_cbc));
end;

function TScSSHClient.NotDefaultHMACAlgorithms: boolean;
begin
  Result := not ((FHMACAlgorithms.Count = 3) and
    ((FHMACAlgorithms.Items[0] as TScSSHHMacAlgorithmItem).Algorithm = hmacSHA2_256) and
    ((FHMACAlgorithms.Items[1] as TScSSHHMacAlgorithmItem).Algorithm = hmacSHA2_512) and
    ((FHMACAlgorithms.Items[2] as TScSSHHMacAlgorithmItem).Algorithm = hmacSHA1));
end;

function TScSSHClient.NotDefaultKeyExchangeAlgorithms: boolean;
begin
  Result := not ((FKeyExchangeAlgorithms.Count = 8) and
    ((FKeyExchangeAlgorithms.Items[0] as TScSSHKeyExchangeAlgorithmItem).Algorithm = keCurve25519Sha256) and
    ((FKeyExchangeAlgorithms.Items[1] as TScSSHKeyExchangeAlgorithmItem).Algorithm = keECDHSha2Nistp521) and
    ((FKeyExchangeAlgorithms.Items[2] as TScSSHKeyExchangeAlgorithmItem).Algorithm = keECDHSha2Nistp384) and
    ((FKeyExchangeAlgorithms.Items[3] as TScSSHKeyExchangeAlgorithmItem).Algorithm = keECDHSha2Nistp256) and
    ((FKeyExchangeAlgorithms.Items[4] as TScSSHKeyExchangeAlgorithmItem).Algorithm = keDHExchSha256) and
    ((FKeyExchangeAlgorithms.Items[5] as TScSSHKeyExchangeAlgorithmItem).Algorithm = keDHExchSha1) and
    ((FKeyExchangeAlgorithms.Items[6] as TScSSHKeyExchangeAlgorithmItem).Algorithm = keDHGroup14Sha1) and
    ((FKeyExchangeAlgorithms.Items[7] as TScSSHKeyExchangeAlgorithmItem).Algorithm = keDHGroup1Sha1));
end;

function TScSSHClient.NotDefaultHostKeyAlgorithms: boolean;
begin
  Result := not ((FHostKeyAlgorithms.Count = 2) and
    ((FHostKeyAlgorithms.Items[0] as TScSSHHostKeyAlgorithmItem).Algorithm = aaRSA) and
    ((FHostKeyAlgorithms.Items[1] as TScSSHHostKeyAlgorithmItem).Algorithm = aaEC));
end;

procedure TScSSHClient.CreateVio;
var
  CurHostName: string;
begin
  if HostName = '' then
    CurHostName := LOCAL_HOST
  else
    CurHostName := HostName;

  if FHttpOptions.Enabled then
    FVio := TCRVioHttp.Create(nil, FHttpOptions, FProxyOptions, CurHostName, FPort, Options.IPVersion)
  else
    FVio := TCRVioTcp.Create(FProxyOptions, FProviderName, CurHostName, FPort, Options.IPVersion);
end;

function TScSSHClient.CreateSSHChannel: TObject;
begin
  Result := TScSSHChannel.Create(nil);
end;

procedure TScSSHClient.DoConnect;

  function CompressionToString(Compression: TScCompression): string;
  begin
  {$IFDEF HAVE_COMPRESS_INTERNAL}
    case Compression of
      csNone:
        Result := SNone;
      csAllowed:
        Result := SNone + ',' + SZLib + ',' + SZLibOpenSSH;
      csRequired:
        Result := SZLib + ',' + SZLibOpenSSH;
    end;
  {$ELSE}
    Result := SNone;
  {$ENDIF}
  end;

var
  CurHostName, CurHostKeyName, CurPrivateKeyName: string;
  Params: TSshConnectionParameters;
  s: string;
  opt: integer;
  i: integer;
begin
  if HostName = '' then
    CurHostName := LOCAL_HOST
  else
    CurHostName := HostName;

  if FPort = 0 then
    FPort := SSH_DEFAULT_PORT;

  if KeyStorage = nil then
    raise EScError.Create(seStorageNoSet);

  if User = '' then
    raise EScError.Create(seEmptyUsername);

  lock(FLockConnection);
  try
    FreeAndNil(FVio);
    FreeAndNil(FConnection);

    CreateVio;
    try
      FVio.Timeout := Timeout;

      if FVio is TCRVioTcp then begin
        if Options.BindAddress <> '' then begin
          TCRVioTcp(FVio).BindAddress := Options.BindAddress;
          TCRVioTcp(FVio).Bind;
        end;
      end;

      FVio.Connect;

      if FVio is TCRVioTcp then begin
        if Options.TCPKeepAlive then
          opt := 1
        else
          opt := 0;
        TCRVioTcp(FVio).SetSocketOption(SOL_SOCKET, SO_KEEPALIVE, opt);

        if Options.SocketReceiveBufferSize > 0 then
          TCRVioTcp(FVio).SetSocketOption(SOL_SOCKET, SO_RCVBUF, Options.SocketReceiveBufferSize);
        if Options.SocketSendBufferSize > 0 then
          TCRVioTcp(FVio).SetSocketOption(SOL_SOCKET, SO_SNDBUF, Options.SocketSendBufferSize);
      end;

      Params := TSshConnectionParameters.Create;
      try
        TScSSHConnectionInfoUtils.SetCiphersClient(FClientInfo, CiphersClient);
        TScSSHConnectionInfoUtils.SetCiphersServer(FClientInfo, CiphersServer);
        TScSSHConnectionInfoUtils.SetHMACAlgorithms(FClientInfo, HMACAlgorithms);
        TScSSHConnectionInfoUtils.SetUser(FClientInfo, User);
        if FVio is TCRVioTcp then
          TScSSHConnectionInfoUtils.SetTCPConnection(FClientInfo, TCRVioTcp(FVio));
        Params.ConnectionInfo := FClientInfo;

        s := '';
        for i := 0 to HostKeyAlgorithms.Count - 1 do begin
          if i > 0 then
            s := s + ',';
          s := s + CipherFactory.PublicKeyAlgorithmToSSH2ListName(TScSSHHostKeyAlgorithmItem(HostKeyAlgorithms.Items[i]).Algorithm);
        end;
        Params.PreferableHostKeyAlgorithms := s;

        if Random = nil then
          raise Exception.Create(SInternalError);
        Params.Random := Random;
        Params.KeyExchangeAlgorithms := KeyExchangeAlgorithms.AsString;
        Params.CompressionClientAlgorithms := CompressionToString(CompressionClient);
        Params.CompressionServerAlgorithms := CompressionToString(CompressionServer);
        Params.AuthenticationType := Authentication;
        Params.UserName := User;
        Params.Password := Password;

        if HostKeyName = '' then
          CurHostKeyName := CurHostName
        else
          CurHostKeyName := HostKeyName;

        if PrivateKeyName = '' then
          CurPrivateKeyName := User
        else
          CurPrivateKeyName := PrivateKeyName;

        if FServerPublicKeyChanged then
          Params.ServerKey := FNewServerKey
        else
          Params.ServerKey := KeyStorage.Keys.FindKey(CurHostKeyName);
        Params.ClientKey := KeyStorage.Keys.FindKey(CurPrivateKeyName);

        s := Options.RekeyLimit;
        Params.RekeyLimitSize := GetRekeyLimitSize(s);
        Params.AliveCountMax := Options.ServerAliveCountMax;
        Params.AliveInterval := Options.ServerAliveInterval;
        Params.ClientVersion := Options.ClientVersion;
        Params.MsgIgnoreRate := Options.MsgIgnoreRate;
        Params.IPVersion := Options.IPVersion;

        Params.OnBanner := DoBanner;
        Params.OnServerKeyValidation := DoServerKeyValidation;
        Params.AfterDisconnect := DoAfterDisconnect;
        Params.OnEstablishPortForwarding := EstablishPortForwarding;
        if Assigned(FOnAuthenticationPrompt) then
          Params.OnAuthenticationPrompt := DoAuthenticationPrompt;

        FConnection := TSsh2Connection.Create(Params);
        try
          FConnection.Connect(FVio);
        except
          FreeAndNil(FConnection);
          raise;
        end;
      finally
        Params.Free;
      end;
    except
      FreeAndNil(FVio);
      raise;
    end;
  finally
    unlock(FLockConnection);
  end;
end;

procedure TScSSHClient.DoDisconnect;
var
  TmpConnection: TSsh2Connection;
  TmpVio: TCRVio;
begin
  lock(FLockConnection);
  try
    TmpConnection := FConnection;
    TmpVio := FVio;
    FConnection := nil;
    FVio := nil;

  {$IFDEF AUTOREFCOUNT}
    if TmpConnection <> nil then
      TmpConnection.Dispose;
    if TmpVio <> nil then
      TmpVio.Close;
  {$ELSE}
    TmpConnection.Free;
    TmpVio.Free;
  {$ENDIF}

    TListRWFlows(FListFlows).FreeAllThreads;
  finally
    unlock(FLockConnection);
  end;
end;

function TScSSHClient.GetConnected: boolean;
begin
  lock(FLockConnection);
  try
    Result := (FConnection <> nil) and not FConnection.IsClosed;
  finally
    unlock(FLockConnection);
  end;
end;

procedure TScSSHClient.SetConnected(Value: boolean);
begin
  if (csReading in ComponentState) and Value then
    FStreamedConnected := True
  else begin
    lock(FLockConnection);
    try
      if Value = GetConnected then
        Exit;

      if Value then begin
        if Assigned(BeforeConnect) then
          BeforeConnect(Self);

        FServerPublicKeyChanged := False;
        try
          DoConnect;
        except
          on E: EScError do
            if not GetConnected and FServerPublicKeyChanged and (E.ErrorCode <> seAuthenticationFailed) then
              DoConnect // Try to reconnect using new key
            else
              raise;
        end;

        SendConnectEvent(True);

        if Assigned(AfterConnect) then
          AfterConnect(Self);
      end
      else begin
        if Assigned(BeforeDisconnect) then
          BeforeDisconnect(Self);

        SendConnectEvent(False);
        DoDisconnect;
      end;
    finally
      unlock(FLockConnection);
    end;
  end;
end;

procedure TScSSHClient.Connect;
begin
  SetConnected(True);
end;

procedure TScSSHClient.Disconnect;
begin
  SetConnected(False);
end;

procedure TScSSHClient.Loaded;
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

procedure TScSSHClient.SendConnectEvent(Connecting: Boolean);
var
  Clients: TList;
  ConnectEvent: TScConnectChangeEvent;
  i: Integer;
begin
  Clients := FClients.LockList;
  try
    for i := Clients.Count - 1 downto 0 do
      if FConnectEvents[i] <> nil then begin
        TMethod(ConnectEvent).Data := Clients[i];
        TMethod(ConnectEvent).Code := FConnectEvents[i];
        ConnectEvent(Self, Connecting);
      end;
  finally
    FClients.UnlockList;
  end;
end;

procedure TScSSHClient.RegisterClient(Client: TObject; Event: TScConnectChangeEvent);
var
  Clients: TList;
begin
  Clients := FClients.LockList;
  try
    Clients.Add(Client);
    FConnectEvents.Add(TMethod(Event).Code);
  finally
    FClients.UnlockList;
  end;
end;

procedure TScSSHClient.UnRegisterClient(Client: TObject);
var
  Clients: TList;
  Index: Integer;
begin
  Clients := FClients.LockList;
  try
    Index := Clients.IndexOf(Client);
    if Index <> -1 then begin
      Clients.Delete(Index);
      FConnectEvents.Delete(Index);
    end;
  finally
    FClients.UnlockList;
  end;
end;

function TScSSHClient.GetChannelCount: Integer;
var
  Clients: TList;
begin
  Clients := FClients.LockList;
  try
    Result := Clients.Count;
  finally
    FClients.UnlockList;
  end;
end;

procedure TScSSHClient.DoServerKeyValidation(Sender: TObject; NewServerKey: TScKey; var Accept: boolean);
{$IFNDEF FPC}
var
  CurHostKeyName: string;
{$ENDIF}
begin
{$IFNDEF FPC}
  if csDesigning in ComponentState then begin
    if HostKeyName = '' then
      CurHostKeyName := HostName
    else
      CurHostKeyName := HostKeyName;

    if KeyStorage.Keys.FindKey(CurHostKeyName) = nil then
      if Assigned(ShowDlgYesOrNo) and ShowDlgYesOrNo(SMsgAcceptServerKey) then begin
        NewServerKey.KeyName := CurHostKeyName;
        KeyStorage.Keys.Add(NewServerKey);
        Accept := True;
      end;
  end
  else
{$ENDIF}
  begin
    if Assigned(FOnServerKeyValidate) then
      FOnServerKeyValidate(Self, NewServerKey, Accept);

    if Accept then begin
      FServerPublicKeyChanged := True;
      if FNewServerKey = nil then
        FNewServerKey := TScKey.Create(nil);
      FNewServerKey.Assign(NewServerKey);
    end;
  end;
end;

procedure TScSSHClient.CheckInactive;
begin
  if Connected then
    raise EScError.Create(seClientOpened);
end;

function TScSSHClient.GetServerVersion: string;
begin
  if FConnection <> nil then
    Result := FConnection.Params.ServerVersion
  else
    Result := '';
end;

procedure TScSSHClient.OnPropertyChanged(Sender: TObject);
begin
  CheckInactive;
end;

procedure TScSSHClient.SetCiphersClient(Value: TScSSHCiphers);
begin
  if Value <> FCiphersClient then begin
    CheckInactive;
    FCiphersClient.Assign(Value);
  end;
end;

procedure TScSSHClient.SetCiphersServer(Value: TScSSHCiphers);
begin
  if Value <> FCiphersServer then begin
    CheckInactive;
    FCiphersServer.Assign(Value);
  end;
end;

procedure TScSSHClient.SetHMACAlgorithms(Value: TScSSHHMacAlgorithms);
begin
  if Value <> FHMACAlgorithms then begin
    CheckInactive;
    FHMACAlgorithms.Assign(Value);
  end;
end;

procedure TScSSHClient.SetCompressionClient(const Value: TScCompression);
begin
  if Value <> FCompressionClient then begin
    CheckInactive;
    FCompressionClient := Value;
  end;
end;

procedure TScSSHClient.SetCompressionServer(const Value: TScCompression);
begin
  if Value <> FCompressionServer then begin
    CheckInactive;
    FCompressionServer := Value;
  end;
end;

procedure TScSSHClient.SetKeyExchangeAlgorithms(Value: TScSSHKeyExchangeAlgorithms);
begin
  if Value <> FKeyExchangeAlgorithms then begin
    CheckInactive;
    FKeyExchangeAlgorithms.Assign(Value);
  end;
end;

procedure TScSSHClient.SetHostKeyAlgorithms(Value: TScSSHHostKeyAlgorithms);
begin
  if Value <> FHostKeyAlgorithms then begin
    CheckInactive;
    FHostKeyAlgorithms.Assign(Value);
  end;
end;

procedure TScSSHClient.SetAuthentication(const Value: TScSSHAuthentication);
begin
  if Value <> FAuthentication then begin
    CheckInactive;
    FAuthentication := Value;
  end;
end;

procedure TScSSHClient.SetHostName(const Value: string);
begin
  if Value <> FHostName then begin
    CheckInactive;
    FHostName := Trim(Value);
  end;
end;

procedure TScSSHClient.SetPort(const Value: integer);
begin
  if Value <> FPort then begin
    CheckInactive;
    FPort := Value;
  end;
end;

procedure TScSSHClient.SetUser(const Value: string);
begin
  if Value <> FUser then begin
    CheckInactive;
    FUser := Trim(Value);
  end;
end;

procedure TScSSHClient.SetPassword(const Value: string);
begin
  if Value <> FPassword then begin
    CheckInactive;
    FPassword := Trim(Value);
  end;
end;

procedure TScSSHClient.SetTimeout(const Value: integer);
begin
  if Value <> FTimeout then begin
    CheckInactive;
    FTimeout := Value;
  end;
end;

procedure TScSSHClient.SetOptions(Value: TScSSHClientOptions);
begin
  if Value <> FOptions then begin
    CheckInactive;
    FOptions.Assign(Value);
  end;
end;

procedure TScSSHClient.SetHttpOptions(Value: THttpOptions);
begin
  if Value <> FHttpOptions then begin
    CheckInactive;
    FHttpOptions.Assign(Value);
  end;
end;

procedure TScSSHClient.SetProxyOptions(Value: TProxyOptions);
begin
  if Value <> FProxyOptions then begin
    CheckInactive;
    FProxyOptions.Assign(Value);
  end;
end;

procedure TScSSHClient.SetSocketReceiveBufferSize(Value: Integer);
begin
  if (FVio is TCRVioTcp) and GetConnected and (Value > 0) then
    TCRVioTcp(FVio).SetSocketOption(SOL_SOCKET, SO_RCVBUF, Value);
end;

procedure TScSSHClient.SetSocketSendBufferSize(Value: Integer);
begin
  if (FVio is TCRVioTcp) and GetConnected and (Value > 0) then
    TCRVioTcp(FVio).SetSocketOption(SOL_SOCKET, SO_SNDBUF, Value);
end;

procedure TScSSHClient.Notification(Component: TComponent; Operation: TOperation);
begin
  if (Component = FKeyStorage) and (Operation = opRemove) then
    KeyStorage := nil;

  inherited;
end;

procedure TScSSHClient.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScSSHClient) then begin
    TScSSHClient(Dest).CiphersClient := CiphersClient;
    TScSSHClient(Dest).CiphersServer := CiphersServer;
    TScSSHClient(Dest).HMACAlgorithms := HMACAlgorithms;
    TScSSHClient(Dest).KeyExchangeAlgorithms := KeyExchangeAlgorithms;
    TScSSHClient(Dest).HostKeyAlgorithms := HostKeyAlgorithms;
    TScSSHClient(Dest).Authentication := Authentication;
    TScSSHClient(Dest).Options := Options;
    TScSSHClient(Dest).HttpOptions := HttpOptions;
    TScSSHClient(Dest).ProxyOptions := ProxyOptions;

    TScSSHClient(Dest).HostName := HostName;
    TScSSHClient(Dest).Port := Port;
    TScSSHClient(Dest).User := User;
    TScSSHClient(Dest).Password := Password;
    TScSSHClient(Dest).Timeout := Timeout;

    TScSSHClient(Dest).KeyStorage := KeyStorage;
    TScSSHClient(Dest).PrivateKeyName := PrivateKeyName;
    TScSSHClient(Dest).HostKeyName := HostKeyName;
  end
  else
    inherited;
end;

procedure TScSSHClient.SetPrivateKeyName(const Value: string);
begin
  if Value <> FPrivateKeyName then begin
    CheckInactive;
    FPrivateKeyName := Value;
  end;
end;

procedure TScSSHClient.SetHostKeyName(const Value: string);
begin
  if Value <> FHostKeyName then begin
    CheckInactive;
    FHostKeyName := Value;
  end;
end;

procedure TScSSHClient.SetKeyStorage(Value: TScStorage);
begin
  if FKeyStorage <> Value then begin
    if FKeyStorage <> nil then
      FKeyStorage.RemoveFreeNotification(Self);

    SetConnected(False);
    FKeyStorage := Value;

    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

procedure TScSSHClient.DoBanner(Sender: TObject; const Banner: string);
begin
  if Assigned(FOnBanner) then
    FOnBanner(Self, Banner);
end;

procedure TScSSHClient.DoAfterDisconnect(Sender: TObject);
begin
  if Assigned(FAfterDisconnect) then
    FAfterDisconnect(Self);
end;

procedure TScSSHClient.DoAuthenticationPrompt(Sender: TObject; const Name, Instruction: string;
  const Prompts: TStringDynArray; var Responses: TStringDynArray);
begin
  if Assigned(FOnAuthenticationPrompt) then
    FOnAuthenticationPrompt(Self, Name, Instruction, Prompts, Responses);
end;

function TScSSHClient.NewSSHChannel(const ChannelRequest: TChannelRequest): TObject;
var
  NewChannel: TScSSHChannel;
begin
  NewChannel := CreateSSHChannel as TScSSHChannel;
  try
    NewChannel.Client := Self;
    NewChannel.Direct := True;
    NewChannel.Timeout := Timeout;
    NewChannel.DestHost := ChannelRequest.Host;
    NewChannel.DestPort := ChannelRequest.Port;

    TScSSHChannelUtils.SetChannelRequest(NewChannel, ChannelRequest);
  except
    NewChannel.Free;
    raise;
  end;

  Result := NewChannel;
end;

procedure TScSSHClient.EstablishPortForwarding(const ChannelRequest: TChannelRequest);
var
  NewVio: TCRVioTcp;
  NewChannel: TScSSHChannel;
begin
  NewVio := nil;
  NewChannel := TScSSHChannel(NewSSHChannel(ChannelRequest));
  try
    NewVio := TCRVioTcp.Create(ChannelRequest.Host, ChannelRequest.Port, Options.IPVersion);
    NewVio.ConnectionTimeout := Timeout;
    NewVio.Connect;
    NewChannel.Connect;
  except
    NewChannel.Free;
    NewVio.Free;
    raise;
  end;

  TListRWFlows(FListFlows).CreateSocketToChannelTransferring(NewChannel, NewVio);
end;

{ TScSSHClientOptions }

constructor TScSSHClientOptions.Create(Client: TScSSHClient);
begin
  inherited Create;

  Assert(Client <> nil);
  FClient := Client;
  FServerAliveCountMax := 3;
  FServerAliveInterval := 0;
  FTCPKeepAlive := True;
  FIPVersion := DefValIPVersion;
  FClientVersion := TSshUtils.ClientVersionString(pSSH2);
  FSocketReceiveBufferSize := DEFAULT_SOCKET_BUFFER_SIZE;
  FSocketSendBufferSize := DEFAULT_SOCKET_BUFFER_SIZE;
end;

procedure TScSSHClientOptions.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScSSHClientOptions) then
  begin
    TScSSHClientOptions(Dest).FRekeyLimit := FRekeyLimit;
    TScSSHClientOptions(Dest).FServerAliveCountMax := FServerAliveCountMax;
    TScSSHClientOptions(Dest).FServerAliveInterval := FServerAliveInterval;
    TScSSHClientOptions(Dest).FBindAddress := FBindAddress;
    TScSSHClientOptions(Dest).FTCPKeepAlive := FTCPKeepAlive;
    TScSSHClientOptions(Dest).FIPVersion := FIPVersion;
    TScSSHClientOptions(Dest).FClientVersion := FClientVersion;
    TScSSHClientOptions(Dest).FMsgIgnoreRate := FMsgIgnoreRate;
    TScSSHClientOptions(Dest).FSocketReceiveBufferSize := FSocketReceiveBufferSize;
    TScSSHClientOptions(Dest).FSocketSendBufferSize := FSocketSendBufferSize;
  end
  else
    inherited;
end;

procedure TScSSHClientOptions.SetRekeyLimit(Value: string);
begin
  if Value <> FRekeyLimit then begin
    Value := UpperCase(Trim(Value));
    GetRekeyLimitSize(Value);
    FClient.CheckInactive;
    FRekeyLimit := Value;
  end;
end;

procedure TScSSHClientOptions.SetServerAliveCountMax(Value: integer);
begin
  if Value <> FServerAliveCountMax then begin
    FClient.CheckInactive;
    FServerAliveCountMax := Value;
  end;
end;

procedure TScSSHClientOptions.SetServerAliveInterval(Value: integer);
begin
  if Value <> FServerAliveInterval then begin
    FClient.CheckInactive;
    FServerAliveInterval := Value;
  end;
end;

procedure TScSSHClientOptions.SetBindAddress(const Value: string);
begin
  if Value <> FBindAddress then begin
    FClient.CheckInactive;
    FBindAddress := Trim(Value);
  end;
end;

procedure TScSSHClientOptions.SetTCPKeepAlive(Value: boolean);
begin
  if Value <> FTCPKeepAlive then begin
    FClient.CheckInactive;
    FTCPKeepAlive := Value;
  end;
end;

procedure TScSSHClientOptions.SetIPVersion(const Value: TIPVersion);
begin
  if Value <> FIPVersion then begin
    FClient.CheckInactive;
    FIPVersion := Value;
  end;
end;

procedure TScSSHClientOptions.SetClientVersion(const Value: string);
begin
  if Value <> FClientVersion then begin
    FClient.CheckInactive;
    FClientVersion := Value;
  end;
end;

function TScSSHClientOptions.NotDefaultVersion: boolean;
begin
  Result := not (FClientVersion = TSshUtils.ClientVersionString(pSSH2));
end;

procedure TScSSHClientOptions.SetMsgIgnoreRate(Value: Integer);
begin
  if Value <> FMsgIgnoreRate then begin
    FClient.CheckInactive;
    FMsgIgnoreRate := Abs(Value) mod 101;
  end;
end;

procedure TScSSHClientOptions.SetSocketReceiveBufferSize(Value: Integer);
begin
  if Value <> FSocketReceiveBufferSize then begin
    FSocketReceiveBufferSize := Value;
    FClient.SetSocketReceiveBufferSize(Value);
  end;
end;

procedure TScSSHClientOptions.SetSocketSendBufferSize(Value: Integer);
begin
  if Value <> FSocketSendBufferSize then begin
    FSocketSendBufferSize := Value;
    FClient.SetSocketSendBufferSize(Value);
  end;
end;

function GetRekeyLimitSize(var RekeyLimit: string): Int64;
var
  Ind: Integer;
  Size: string;
  Koef: Int64;
begin
  if RekeyLimit = '' then begin
    Result := 0;
    Exit;
  end;

  Ind := Pos('G', RekeyLimit);
  if Ind > 0 then begin
    Size := Copy(RekeyLimit, 1, Ind - 1);
    RekeyLimit := Copy(RekeyLimit, 1, Ind);
    Koef := 1024 * 1024 * 1024;
  end
  else begin
    Ind := Pos('M', RekeyLimit);
    if Ind > 0 then begin
      Size := Copy(RekeyLimit, 1, Ind - 1);
      RekeyLimit := Copy(RekeyLimit, 1, Ind);
      Koef := 1024 * 1024;
    end
    else begin
      Ind := Pos('K', RekeyLimit);
      if Ind > 0 then begin
        Size := Copy(RekeyLimit, 1, Ind - 1);
        RekeyLimit := Copy(RekeyLimit, 1, Ind);
        Koef := 1024;
      end
      else begin
        Size := RekeyLimit;
        Koef := 1024;
      end;
    end;
  end;

  Result := Round(StrToFloat(Trim(Size)) * Koef);
end;

{ TScClientUtils }

class procedure TScClientUtils.RegisterClient(Obj: TScSSHClient; Client: TObject; Event: TScConnectChangeEvent);
begin
  Obj.RegisterClient(Client, Event);
end;

class procedure TScClientUtils.UnRegisterClient(Obj: TScSSHClient; Client: TObject);
begin
  Obj.UnRegisterClient(Client);
end;

class function TScClientUtils.GetConnection(Obj: TScSSHClient): TSsh2Connection;
begin
  Result := Obj.FConnection;
end;

class procedure TScClientUtils.SetConnection(Obj: TScSSHClient; SshConnection: TSsh2Connection);
begin
  Obj.FConnection := SshConnection;
end;

class procedure TScClientUtils.SetVio(Obj: TScSSHClient; vio: TCRVio);
begin
  Obj.FVio := vio;
end;

end.
