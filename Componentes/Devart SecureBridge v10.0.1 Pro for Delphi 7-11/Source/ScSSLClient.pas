
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF DAC}
{$I SB.inc}
unit ScSSLClient;
{$ENDIF}

interface

uses
  Classes, SysUtils,
{$IFNDEF SBRIDGE}
  CLRClasses, CRTypes, CRVio, CRVioTcp,
  TdsSSLConsts, TdsUtils, TdsSSLTypes, TdsReceiveBuffer, TdsBridge,
  TdsSecureSocket, TdsCertificateExts, TdsSSLExtensions;
{$ELSE}
  ScCLRClasses, ScTypes, ScVio, ScVioTcp,
  ScConsts, ScUtils, ScSSLTypes, ScReceiveBuffer, ScBridge,
  ScSecureSocket, ScCertificateExts, ScSSLExtensions;
{$ENDIF}


const
  DefValCompression = csNone;
  DefValEventsCallMode = ecAsynchronous;
  DefValProtocols = [spTls11, spTls12];

type
  // Used for Ftps, Https, Smtps, and etc.
  TScSSLClientOptions = class(TPersistent)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FOwner: TComponent;
    FCipherSuites: TScSSLCipherSuites;
    FProtocols: TScSSLProtocols;
    FStorage: TScStorage;
    FClientCertificateName: string;
    FCACertificateName: string;

    FAllowLoadCRLByHttp: boolean;
    FDisableCRLValidation: boolean;
    FIgnoreServerCertificateConstraints: boolean;
    FIgnoreServerCertificateInsecurity: boolean;
    FIgnoreServerCertificateValidity: boolean;
    FTrustSelfSignedCertificate: boolean;
    FTrustServerCertificate: boolean;
    FTrustStorageCertificates: boolean;
    FUseClientInitiatedRenegotiation: boolean;
    FIdentityDNSName: string;
    FUseSecureSessionResumption: boolean;
    FDisableInsertEmptyFragment: boolean;
    FClientHelloExtensions: TTLSHelloExtensions;
    FOnServerCertificateValidation: TScRemoteCertificateValidationEvent;
    FOnObtainCRL: TScObtainCRLEvent;

    function CheckDefaultCipherSuites: boolean;
    procedure SetCipherSuites(Value: TScSSLCipherSuites);
    procedure SetStorage(Value: TScStorage);

  protected
    procedure AssignTo(Dest: TPersistent); override;

  public
    constructor Create(Owner: TComponent);
    destructor Destroy; override;

    property ClientHelloExtensions: TTLSHelloExtensions read FClientHelloExtensions;
    property IdentityDNSName: string read FIdentityDNSName write FIdentityDNSName;
    property UseSecureSessionResumption: boolean read FUseSecureSessionResumption write FUseSecureSessionResumption default False;
    property DisableInsertEmptyFragment: boolean read FDisableInsertEmptyFragment write FDisableInsertEmptyFragment default False;

  published
    property CipherSuites: TScSSLCipherSuites read FCipherSuites write SetCipherSuites stored CheckDefaultCipherSuites;
    property Protocols: TScSSLProtocols read FProtocols write FProtocols default DefValProtocols;
    property Storage: TScStorage read FStorage write SetStorage;
    property ClientCertificateName: string read FClientCertificateName write FClientCertificateName;
    property CACertificateName: string read FCACertificateName write FCACertificateName;

    property AllowLoadCRLByHttp: boolean read FAllowLoadCRLByHttp write FAllowLoadCRLByHttp default True;
    property DisableCRLValidation: boolean read FDisableCRLValidation write FDisableCRLValidation default False;
    property IgnoreServerCertificateConstraints: boolean read FIgnoreServerCertificateConstraints write FIgnoreServerCertificateConstraints default False;
    property IgnoreServerCertificateInsecurity: boolean read FIgnoreServerCertificateInsecurity write FIgnoreServerCertificateInsecurity default False;
    property IgnoreServerCertificateValidity: boolean read FIgnoreServerCertificateValidity write FIgnoreServerCertificateValidity default False;
    property TrustSelfSignedCertificate: boolean read FTrustSelfSignedCertificate write FTrustSelfSignedCertificate default False;
    property TrustServerCertificate: boolean read FTrustServerCertificate write FTrustServerCertificate default False;
    property TrustStorageCertificates: boolean read FTrustStorageCertificates write FTrustStorageCertificates default False;
    property UseClientInitiatedRenegotiation: boolean read FUseClientInitiatedRenegotiation write FUseClientInitiatedRenegotiation default False;

    property OnServerCertificateValidation: TScRemoteCertificateValidationEvent read FOnServerCertificateValidation write FOnServerCertificateValidation;
    property OnObtainCRL: TScObtainCRLEvent read FOnObtainCRL write FOnObtainCRL;
  end;

  TScSSLClient = class;

  TScSSLSecurityOptions = class(TPersistent)
  private
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FHelloExtensions: TTLSHelloExtensions;

    FAllowLoadCRLByHttp: boolean;
    FDisableCRLValidation: boolean;
    FIgnoreServerCertificateConstraints: boolean;
    FIgnoreServerCertificateInsecurity: boolean;
    FIgnoreServerCertificateValidity: boolean;
    FTrustSelfSignedCertificate: boolean;
    FTrustServerCertificate: boolean;
    FTrustStorageCertificates: boolean;
    FUseClientInitiatedRenegotiation: boolean;
    FIdentityDNSName: string;
    FServerCertDN: string;

    procedure Init;
    function GetUseExtendedMasterSecret: boolean;
    procedure SetUseExtendedMasterSecret(Value: boolean);
    function GetUseSecureSessionResumption: boolean;
    procedure SetUseSecureSessionResumption(Value: boolean);
    function GetUseSecureRenegotiation: boolean;
    procedure SetUseSecureRenegotiation(Value: boolean);
    function GetUseSignatureAlgorithmsExtension: boolean;
    procedure SetUseSignatureAlgorithmsExtension(Value: boolean);

  protected
    procedure AssignTo(Dest: TPersistent); override;

  public
    constructor Create(Owner: TScSSLClient); overload;
    constructor Create(HelloExtensions: TTLSHelloExtensions); overload;

    property IdentityDNSName: string read FIdentityDNSName write FIdentityDNSName;
    property ServerCertDN: string read FServerCertDN write FServerCertDN;

  published
    property AllowLoadCRLByHttp: boolean read FAllowLoadCRLByHttp write FAllowLoadCRLByHttp default True;
    property DisableCRLValidation: boolean read FDisableCRLValidation write FDisableCRLValidation default False;
    property IgnoreServerCertificateConstraints: boolean read FIgnoreServerCertificateConstraints write FIgnoreServerCertificateConstraints default False;
    property IgnoreServerCertificateInsecurity: boolean read FIgnoreServerCertificateInsecurity write FIgnoreServerCertificateInsecurity default False;
    property IgnoreServerCertificateValidity: boolean read FIgnoreServerCertificateValidity write FIgnoreServerCertificateValidity default False;
    property TrustSelfSignedCertificate: boolean read FTrustSelfSignedCertificate write FTrustSelfSignedCertificate default False;
    property TrustServerCertificate: boolean read FTrustServerCertificate write FTrustServerCertificate default False;
    property TrustStorageCertificates: boolean read FTrustStorageCertificates write FTrustStorageCertificates default False;
    property UseClientInitiatedRenegotiation: boolean read FUseClientInitiatedRenegotiation write FUseClientInitiatedRenegotiation default False;

    property UseExtendedMasterSecret: boolean read GetUseExtendedMasterSecret write SetUseExtendedMasterSecret default True;
    property UseSecureSessionResumption: boolean read GetUseSecureSessionResumption write SetUseSecureSessionResumption default False;
    property UseSecureRenegotiation: boolean read GetUseSecureRenegotiation write SetUseSecureRenegotiation default True;
    property UseSignatureAlgorithmsExtension: boolean read GetUseSignatureAlgorithmsExtension write SetUseSignatureAlgorithmsExtension default True;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TScSSLClient = class(TComponent)
  private
    FStreamedConnected: boolean;
    FBindAddress: string;
    FHostName: string;
    FPort: integer;
    FTimeout: integer;
    FHttpOptions: THttpOptions;
    FProxyOptions: TProxyOptions;
    FProviderName: string;

    FAssignedSessionInfo: TScSSLSessionInfo;
    FCertName: string;
    FCACertName: string;
    FStorage: TScStorage;
    FCipherSuites: TScSSLCipherSuites;
    FCompression: TScCompression;
    FProtocols: TScSSLProtocols;
    FSecureSocket: TSecureSocket;
    FDisableInsertEmptyFragment: boolean;
    FDisableCloseSocketOnShutdownAlert: boolean;
    FMinDHEKeyLength: integer;
    FSecurityOptions: TScSSLSecurityOptions;
    FClientHelloExtensions: TTLSHelloExtensions;
    FServerHelloExtensions: TTLSHelloExtensions;

    FBeforeConnect: TNotifyEvent;
    FAfterConnect: TNotifyEvent;
    FBeforeDisconnect: TNotifyEvent;
    FAfterDisconnect: TNotifyEvent;
    FOnServerCertificateValidation: TScRemoteCertificateValidationEvent;
    FOnObtainCRL: TScObtainCRLEvent;

    FOnAsyncReceive: TScAsyncReceiveEvent;
    FOnAsyncError: TScErrorEvent;
    FEventsCallMode: TScEventCallMode;

    FIPVersion: TIPVersion;
    procedure SetIPVersion(const Value: TIPVersion);

    procedure OnCipherPropertyChanged(Sender: TObject);
    procedure SetBindAddress(const Value: string);
    procedure SetHostName(const Value: string);

    function GetSessionInfo: TScSSLSessionInfo;

  {$HPPEMIT '#ifdef SetPort'}
  {$HPPEMIT '#undef SetPort'}
  {$HPPEMIT '#endif'}
    procedure SetPort(const Value: integer);
    procedure SetTimeout(const Value: integer);
    procedure SetHttpOptions(Value: THttpOptions);
    procedure SetProxyOptions(Value: TProxyOptions);
    procedure SetSecurityOptions(Value: TScSSLSecurityOptions);

    function GetIsSecure: boolean;
    procedure SetIsSecure(Value: boolean);

    procedure SetCertName(const Value: string);
    procedure SetCACertName(const Value: string);
    procedure SetStorage(Value: TScStorage);
    procedure SetServerNameExtension;

    function CheckDefaultCipherSuites: boolean;
    procedure SetCipherSuites(Value: TScSSLCipherSuites);
    procedure SetCompression(const Value: TScCompression);
    procedure SetProtocols(const Value: TScSSLProtocols);
    procedure SetOnAsyncReceive(Value: TScAsyncReceiveEvent);
    procedure SetEventsCallMode(Value: TScEventCallMode);
    procedure SetDisableInsertEmptyFragment(Value: boolean);
    procedure SetDisableCloseSocketOnShutdownAlert(Value: boolean);
    procedure SetMinDHEKeyLength(Value: integer);
    procedure DoAfterDisconnect(Sender: TObject);
    function IsClosed: boolean;

  protected
    procedure Loaded; override;
    procedure Notification(Component: TComponent; Operation: TOperation); override;
    procedure AssignTo(Dest: TPersistent); override;

    procedure CheckInactive;
    procedure CheckSecured;
    procedure SetVio(Vio: TCRVioTcp);
    function GetConnected: boolean; virtual;
    procedure SetConnected(Value: boolean); virtual;
    procedure DoConnect; virtual;
    procedure DoDisconnect; virtual;

    function GetInCount: integer;
    function GetOutCount: integer;
    procedure DoReceiveData(Sender: TObject);
    procedure NotifyOnAsyncReceive;
    procedure DoAsyncError(E: Exception; var Fail: boolean);

    procedure DoCertificateRequest(Acceptable: TScDistinguishedNameList; CertList: TCRList);
    procedure DoServerCertificateValidation(Sender: TObject; ServerCertificate: TScCertificate;
      CertificateList: TCRList; var Errors: TScCertificateStatusSet);
    procedure DoObtainCRL(Sender: TObject; DistributionPointName: TScGeneralNames; Update: boolean; out CRL: TScCRL);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AssignOptions(Options: TScSSLClientOptions);

    procedure Connect;
    procedure Disconnect;
    procedure Renegotiate;
    procedure AssignSession(Source: TScSSLClient); overload;
    procedure AssignSession(SourceSessionInfo: TScSSLSessionInfo); overload;

    procedure SetSocketOption(OptionLevel, OptionName, OptionValue: integer);
    function ReadBuffer(var Buffer; const Count: integer): integer; overload;
    function ReadBuffer(var Buffer: TBytes; const Offset, Count: integer): integer; overload;
    function ReadNoWait(var Buffer; const Count: integer): integer; overload;
    function ReadNoWait(var Buffer: TBytes; const Offset, Count: integer): integer; overload;
    function WriteBuffer(const Buffer; const Count: integer): integer; overload;
    function WriteBuffer(const Buffer: TBytes; const Offset, Count: integer): integer; overload;
    function WaitForData(MillisecondsTimeout: integer = -1): boolean;

    function GetLastException: Exception;
    function GetLocalIP: string;
    function GetLocalPort: integer;
    function GetRemoteIP: string;
    function GetRemotePort: integer;

    property IsSecure: boolean read GetIsSecure write SetIsSecure default False;

    property MinDHEKeyLength: integer read FMinDHEKeyLength write SetMinDHEKeyLength;
    property ClientHelloExtensions: TTLSHelloExtensions read FClientHelloExtensions;
    property ServerHelloExtensions: TTLSHelloExtensions read FServerHelloExtensions;

    property SessionInfo: TScSSLSessionInfo read GetSessionInfo;
    property InCount: integer read GetInCount;
    property OutCount: integer read GetOutCount;

  published
    property Connected: boolean read GetConnected write SetConnected default False;
    property BindAddress: string read FBindAddress write SetBindAddress; // useful on systems with more than one IP.
    property HostName: string read FHostName write SetHostName; // server name
    property Port: integer read FPort write SetPort default 0; // server port
    property Timeout: integer read FTimeout write SetTimeout default DEFAULT_TIMEOUT;
    property IPVersion: TIPVersion read FIPVersion write SetIPVersion default DefValIPVersion;
    property HttpOptions: THttpOptions read FHttpOptions write SetHttpOptions;
    property ProxyOptions: TProxyOptions read FProxyOptions write SetProxyOptions;
    property SecurityOptions: TScSSLSecurityOptions read FSecurityOptions write SetSecurityOptions;

    property CipherSuites: TScSSLCipherSuites read FCipherSuites write SetCipherSuites stored CheckDefaultCipherSuites;
    property Compression: TScCompression read FCompression write SetCompression default DefValCompression;
    property Protocols: TScSSLProtocols read FProtocols write SetProtocols default DefValProtocols;

    /// To avoid a certain CBC IV attack, the client sends an empty message after the handshake and before the actual application payload.
    /// Unfortunately, some broken implementations do not support empty packets, so sending this empty packet can be turned off
    /// by specifying the DisableInsertEmptyFragment option.
    property DisableInsertEmptyFragment: boolean read FDisableInsertEmptyFragment write SetDisableInsertEmptyFragment default False;
    property DisableCloseSocketOnShutdownAlert: boolean read FDisableCloseSocketOnShutdownAlert write SetDisableCloseSocketOnShutdownAlert default False;

    property Storage: TScStorage read FStorage write SetStorage;
    property CertName: string read FCertName write SetCertName;
    property CACertName: string read FCACertName write SetCACertName;

    property EventsCallMode: TScEventCallMode read FEventsCallMode write SetEventsCallMode default DefValEventsCallMode;

    property BeforeConnect: TNotifyEvent read FBeforeConnect write FBeforeConnect;
    property AfterConnect: TNotifyEvent read FAfterConnect write FAfterConnect;
    property BeforeDisconnect: TNotifyEvent read FBeforeDisconnect write FBeforeDisconnect;
    property AfterDisconnect: TNotifyEvent read FAfterDisconnect write FAfterDisconnect;
    property OnServerCertificateValidation: TScRemoteCertificateValidationEvent read FOnServerCertificateValidation write FOnServerCertificateValidation;
    property OnObtainCRL: TScObtainCRLEvent read FOnObtainCRL write FOnObtainCRL;

    property OnAsyncReceive: TScAsyncReceiveEvent read FOnAsyncReceive write SetOnAsyncReceive;
    property OnAsyncError: TScErrorEvent read FOnAsyncError write FOnAsyncError;
  end;

  TScSSLClientUtils = class
  public
    class procedure SetVio(Obj: TScSSLClient; AVio: TCRVioTcp);
  end;

  procedure _InitDefaultCipherSuites(CipherSuites: TScSSLCipherSuites; StrongMode: boolean);
  function _CheckDefaultCipherSuites(CipherSuites: TScSSLCipherSuites; StrongMode: boolean): boolean;


implementation

uses
{$IFDEF MSWINDOWS}
  Windows, WinSock,
{$ENDIF}
{$IFDEF VER7P}
  StrUtils,
{$ENDIF}
{$IFDEF POSIX}
  Posix.ArpaInet, Posix.NetinetIn, Posix.SysSocket, Posix.NetinetTCP,
{$ENDIF}
{$IFDEF UNIX}
  Sockets,
{$ENDIF}
{$IFDEF VER17P}
  Types,
{$ENDIF}
  SyncObjs,
{$IFNDEF SBRIDGE}
  CRFunctions, TdsThread, TdsCipherSuites;
{$ELSE}
  ScFunctions, ScThread, ScCipherSuites;
{$ENDIF}

procedure _InitDefaultCipherSuites(CipherSuites: TScSSLCipherSuites; StrongMode: boolean);
begin
  (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caAES_256_GCM_SHA384;
  (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caAES_128_GCM_SHA256;
  (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caECDHE_RSA_AES_256_GCM_SHA384;
  (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caECDHE_RSA_AES_128_GCM_SHA256;
  (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caECDHE_ECDSA_AES_256_GCM_SHA384;
  (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caECDHE_ECDSA_AES_128_GCM_SHA256;
  (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caDHE_RSA_AES_256_GCM_SHA384;
  (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caDHE_RSA_AES_128_GCM_SHA256;

  if not StrongMode then begin
    (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caECDHE_RSA_AES_256_CBC_SHA;
    (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caECDHE_RSA_AES_128_CBC_SHA;
    (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caECDHE_RSA_AES_256_CBC_SHA384;
    (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caECDHE_RSA_AES_128_CBC_SHA256;
    (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caECDHE_RSA_3DES_168_CBC_SHA;
    (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caECDHE_ECDSA_AES_256_CBC_SHA;
    (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caECDHE_ECDSA_AES_128_CBC_SHA;
    (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caECDHE_ECDSA_AES_256_CBC_SHA384;
    (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caECDHE_ECDSA_AES_128_CBC_SHA256;
    (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caECDHE_ECDSA_3DES_168_CBC_SHA;
    (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caDHE_RSA_AES_256_CBC_SHA;
    (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caDHE_RSA_AES_128_CBC_SHA;
    (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caDHE_RSA_AES_256_CBC_SHA256;
    (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caDHE_RSA_AES_128_CBC_SHA256;
    (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caDHE_RSA_3DES_168_CBC_SHA;
    (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caRSA_AES_256_GCM_SHA384;
    (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caRSA_AES_128_GCM_SHA256;
    (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caRSA_AES_256_CBC_SHA;
    (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caRSA_AES_128_CBC_SHA;
    (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caRSA_AES_256_CBC_SHA256;
    (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caRSA_AES_128_CBC_SHA256;
    (CipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := caRSA_3DES_168_CBC_SHA;
  end;
end;

function _CheckDefaultCipherSuites(CipherSuites: TScSSLCipherSuites; StrongMode: boolean): boolean;
var
  TestCipherSuites: TScSSLCipherSuites;
  i: integer;
begin
  TestCipherSuites := TScSSLCipherSuites.Create(nil);
  try
    _InitDefaultCipherSuites(TestCipherSuites, StrongMode);

    Result := CipherSuites.Count = TestCipherSuites.Count;
    if not Result then
      Exit;

    for i := 0 to CipherSuites.Count - 1 do begin
      if (CipherSuites.Items[i] as TScSSLCipherSuiteItem).CipherAlgorithm <> (TestCipherSuites.Items[i] as TScSSLCipherSuiteItem).CipherAlgorithm then begin
        Result := False;
        Exit;
      end;
    end;
  finally
    TestCipherSuites.Free;
  end;
end;

{ TScSSLClientOptions }

constructor TScSSLClientOptions.Create(Owner: TComponent);
begin
  inherited Create;

  FOwner := Owner;
  FCipherSuites := TScSSLCipherSuites.Create(FOwner);
  _InitDefaultCipherSuites(FCipherSuites, False);
  FClientHelloExtensions := TTLSHelloExtensions.Create;

  FProtocols := DefValProtocols;
  FAllowLoadCRLByHttp := True;
end;

destructor TScSSLClientOptions.Destroy;
begin
  FCipherSuites.Free;
  FClientHelloExtensions.Free;

  inherited;
end;

procedure TScSSLClientOptions.AssignTo(Dest: TPersistent);
var
  DestCO: TScSSLClientOptions;
begin
  if Dest is TScSSLClientOptions then begin
    DestCO := TScSSLClientOptions(Dest);
    DestCO.CipherSuites.Assign(FCipherSuites);
    DestCO.ClientHelloExtensions.Assign(FClientHelloExtensions);
    DestCO.Storage := FStorage;
    DestCO.FProtocols := FProtocols;
    DestCO.FClientCertificateName := FClientCertificateName;
    DestCO.FCACertificateName := FCACertificateName;

    DestCO.FAllowLoadCRLByHttp := FAllowLoadCRLByHttp;
    DestCO.FDisableCRLValidation := FDisableCRLValidation;
    DestCO.FIgnoreServerCertificateConstraints := FIgnoreServerCertificateConstraints;
    DestCO.FIgnoreServerCertificateInsecurity := FIgnoreServerCertificateInsecurity;
    DestCO.FIgnoreServerCertificateValidity := FIgnoreServerCertificateValidity;
    DestCO.FTrustSelfSignedCertificate := FTrustSelfSignedCertificate;
    DestCO.FTrustServerCertificate := FTrustServerCertificate;
    DestCO.FTrustStorageCertificates := FTrustStorageCertificates;
    DestCO.FUseClientInitiatedRenegotiation := FUseClientInitiatedRenegotiation;
    DestCO.FIdentityDNSName := FIdentityDNSName;
    DestCO.FUseSecureSessionResumption := FUseSecureSessionResumption;
    DestCO.FDisableInsertEmptyFragment := FDisableInsertEmptyFragment;
    DestCO.FOnServerCertificateValidation := FOnServerCertificateValidation;
    DestCO.FOnObtainCRL := FOnObtainCRL;
  end
  else
    inherited;
end;

function TScSSLClientOptions.CheckDefaultCipherSuites: boolean;
begin
  Result := not _CheckDefaultCipherSuites(FCipherSuites, False);
end;

procedure TScSSLClientOptions.SetCipherSuites(Value: TScSSLCipherSuites);
begin
  if Value <> FCipherSuites then
    FCipherSuites.Assign(Value);
end;

procedure TScSSLClientOptions.SetStorage(Value: TScStorage);
begin
  if FStorage <> Value then begin
    if (FStorage <> nil) and (FOwner <> nil) then
      FStorage.RemoveFreeNotification(FOwner);

    FStorage := Value;

    if (Value <> nil) and (FOwner <> nil) then
      Value.FreeNotification(FOwner);
  end;
end;

{ TScSSLSecurityOptions }

constructor TScSSLSecurityOptions.Create(Owner: TScSSLClient);
begin
  inherited Create;

  if (Owner = nil) or (Owner.ClientHelloExtensions = nil) then
    raise EScError.Create(seInvalidInputArgs);

  FHelloExtensions := Owner.ClientHelloExtensions;
  Init;
end;

constructor TScSSLSecurityOptions.Create(HelloExtensions: TTLSHelloExtensions);
begin
  inherited Create;

  if HelloExtensions = nil then
    raise EScError.Create(seInvalidInputArgs);

  FHelloExtensions := HelloExtensions;
  Init;
end;

procedure TScSSLSecurityOptions.Init;
begin
  SetUseExtendedMasterSecret(True);
  SetUseSecureSessionResumption(False);
  SetUseSecureRenegotiation(True);
  SetUseSignatureAlgorithmsExtension(True);
  FAllowLoadCRLByHttp := True;
end;

procedure TScSSLSecurityOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TScSSLSecurityOptions then begin
    TScSSLSecurityOptions(Dest).UseExtendedMasterSecret := UseExtendedMasterSecret;
    TScSSLSecurityOptions(Dest).UseSecureSessionResumption := UseSecureSessionResumption;
    TScSSLSecurityOptions(Dest).UseSecureRenegotiation := UseSecureRenegotiation;
    TScSSLSecurityOptions(Dest).UseSignatureAlgorithmsExtension := UseSignatureAlgorithmsExtension;

    TScSSLSecurityOptions(Dest).FAllowLoadCRLByHttp := FAllowLoadCRLByHttp;
    TScSSLSecurityOptions(Dest).FDisableCRLValidation := FDisableCRLValidation;
    TScSSLSecurityOptions(Dest).FIgnoreServerCertificateConstraints := FIgnoreServerCertificateConstraints;
    TScSSLSecurityOptions(Dest).FIgnoreServerCertificateInsecurity := FIgnoreServerCertificateInsecurity;
    TScSSLSecurityOptions(Dest).FIgnoreServerCertificateValidity := FIgnoreServerCertificateValidity;
    TScSSLSecurityOptions(Dest).FTrustSelfSignedCertificate := FTrustSelfSignedCertificate;
    TScSSLSecurityOptions(Dest).FTrustServerCertificate := FTrustServerCertificate;
    TScSSLSecurityOptions(Dest).FTrustStorageCertificates := FTrustStorageCertificates;
    TScSSLSecurityOptions(Dest).FUseClientInitiatedRenegotiation := FUseClientInitiatedRenegotiation;

    TScSSLSecurityOptions(Dest).FIdentityDNSName := FIdentityDNSName;
    TScSSLSecurityOptions(Dest).FServerCertDN := FServerCertDN;
  end
  else
    inherited;
end;

function TScSSLSecurityOptions.GetUseExtendedMasterSecret: boolean;
begin
  Result := FHelloExtensions.Find(TTLSExtendedMasterSecretExtension) <> nil;
end;

procedure TScSSLSecurityOptions.SetUseExtendedMasterSecret(Value: boolean);
begin
  if Value then
    FHelloExtensions.FindOrCreate(TTLSExtendedMasterSecretExtension)
  else
    FHelloExtensions.RemoveIfExists(TTLSExtendedMasterSecretExtension);
end;

function TScSSLSecurityOptions.GetUseSecureSessionResumption: boolean;
begin
  Result := FHelloExtensions.Find(TTLSSessionTicketExtension) <> nil;
end;

procedure TScSSLSecurityOptions.SetUseSecureSessionResumption(Value: boolean);
begin
  if Value then
    FHelloExtensions.FindOrCreate(TTLSSessionTicketExtension)
  else
    FHelloExtensions.RemoveIfExists(TTLSSessionTicketExtension);
end;

function TScSSLSecurityOptions.GetUseSecureRenegotiation: boolean;
begin
  Result := FHelloExtensions.Find(TTLSRenegotiationIndicationExtension) <> nil;
end;

procedure TScSSLSecurityOptions.SetUseSecureRenegotiation(Value: boolean);
begin
  if Value then
    FHelloExtensions.FindOrCreate(TTLSRenegotiationIndicationExtension)
  else
    FHelloExtensions.RemoveIfExists(TTLSRenegotiationIndicationExtension);
end;

function TScSSLSecurityOptions.GetUseSignatureAlgorithmsExtension: boolean;
begin
  Result := FHelloExtensions.Find(TTLSSignatureAlgorithmsExtension) <> nil;
end;

procedure TScSSLSecurityOptions.SetUseSignatureAlgorithmsExtension(Value: boolean);
var
  SignatureAlgorithmsExtension: TTLSSignatureAlgorithmsExtension;
begin
  if Value then begin
    SignatureAlgorithmsExtension := TTLSSignatureAlgorithmsExtension(FHelloExtensions.Find(TTLSSignatureAlgorithmsExtension));
    if SignatureAlgorithmsExtension = nil then begin
      SignatureAlgorithmsExtension := TTLSSignatureAlgorithmsExtension.CreateDefault;
      FHelloExtensions.Add(SignatureAlgorithmsExtension);
    end;
  end
  else
    FHelloExtensions.RemoveIfExists(TTLSSignatureAlgorithmsExtension);
end;

{ TScSSLClientUtils }

class procedure TScSSLClientUtils.SetVio(Obj: TScSSLClient; AVio: TCRVioTcp);
begin
  Obj.SetVio(AVio);
end;

{ TScSSLClient }

constructor TScSSLClient.Create(AOwner: TComponent);
begin
  inherited;

  FHttpOptions := THttpOptions.Create;
  FProxyOptions := TProxyOptions.Create;
  FHttpOptions.ProxyOptions := FProxyOptions;

  FClientHelloExtensions := TTLSHelloExtensions.Create;
  FServerHelloExtensions := TTLSHelloExtensions.Create;

  FCipherSuites := TScSSLCipherSuites.Create(Self);
  _InitDefaultCipherSuites(FCipherSuites, False);
  FCipherSuites.OnChanged := OnCipherPropertyChanged;

  FCompression := DefValCompression;
  FProtocols := DefValProtocols;
  FPort := 0;
  FTimeout := DEFAULT_TIMEOUT;
  FIPVersion := DefValIPVersion;
  FEventsCallMode := DefValEventsCallMode;
  FMinDHEKeyLength := 1024;

  FSecurityOptions := TScSSLSecurityOptions.Create(Self);
  FSecureSocket := nil;
end;

destructor TScSSLClient.Destroy;
begin
  try
    DoDisconnect;
  except
  end;

  DisposeAsyncEvent(NotifyOnAsyncReceive);

  FSecureSocket.Free;
  FHttpOptions.Free;
  FProxyOptions.Free;
  FClientHelloExtensions.Free;
  FServerHelloExtensions.Free;
  FCipherSuites.Free;
  FSecurityOptions.Free;
  FAssignedSessionInfo.Free;

  inherited;
end;

function TScSSLClient.CheckDefaultCipherSuites: boolean;
begin
  Result := not _CheckDefaultCipherSuites(FCipherSuites, False);
end;

procedure TScSSLClient.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScSSLClient) then begin
    TScSSLClient(Dest).BindAddress := BindAddress;
    TScSSLClient(Dest).HostName := HostName;
    TScSSLClient(Dest).Port := Port;
    TScSSLClient(Dest).Timeout := Timeout;
    TScSSLClient(Dest).IPVersion := IPVersion;
    TScSSLClient(Dest).CipherSuites := CipherSuites;
    TScSSLClient(Dest).Compression := Compression;
    TScSSLClient(Dest).Protocols := Protocols;
    TScSSLClient(Dest).DisableInsertEmptyFragment := DisableInsertEmptyFragment;
    TScSSLClient(Dest).DisableCloseSocketOnShutdownAlert := DisableCloseSocketOnShutdownAlert;
    TScSSLClient(Dest).MinDHEKeyLength := MinDHEKeyLength;
    TScSSLClient(Dest).EventsCallMode := EventsCallMode;

    TScSSLClient(Dest).Storage := Storage;
    TScSSLClient(Dest).CertName := CertName;
    TScSSLClient(Dest).CACertName := CACertName;

    TScSSLClient(Dest).FHttpOptions.Assign(FHttpOptions);
    TScSSLClient(Dest).FProxyOptions.Assign(FProxyOptions);
    TScSSLClient(Dest).FSecurityOptions.Assign(SecurityOptions);
  end
  else
    inherited;
end;

procedure TScSSLClient.AssignOptions(Options: TScSSLClientOptions);
var
  SrcHelloExtension, DstHelloExtension: TTLSHelloExtension;
  i: integer;
begin
  CheckSecured;
  CipherSuites := Options.CipherSuites;
  Protocols := Options.Protocols;
  Storage := Options.Storage;
  CertName := Options.ClientCertificateName;
  CACertName := Options.CACertificateName;

  FSecurityOptions.AllowLoadCRLByHttp := Options.AllowLoadCRLByHttp;
  FSecurityOptions.DisableCRLValidation := Options.DisableCRLValidation;
  FSecurityOptions.IgnoreServerCertificateConstraints := Options.IgnoreServerCertificateConstraints;
  FSecurityOptions.IgnoreServerCertificateInsecurity := Options.IgnoreServerCertificateInsecurity;
  FSecurityOptions.IgnoreServerCertificateValidity := Options.IgnoreServerCertificateValidity;
  FSecurityOptions.TrustSelfSignedCertificate := Options.TrustSelfSignedCertificate;
  FSecurityOptions.TrustServerCertificate := Options.TrustServerCertificate;
  FSecurityOptions.TrustStorageCertificates := Options.TrustStorageCertificates;
  FSecurityOptions.UseClientInitiatedRenegotiation := Options.UseClientInitiatedRenegotiation;
  FSecurityOptions.IdentityDNSName := Options.IdentityDNSName;
  FSecurityOptions.UseSecureSessionResumption := Options.UseSecureSessionResumption;

  FDisableInsertEmptyFragment := Options.FDisableInsertEmptyFragment;
  FOnServerCertificateValidation := Options.FOnServerCertificateValidation;
  FOnObtainCRL := Options.FOnObtainCRL;

  for i := 0 to Options.FClientHelloExtensions.Count - 1 do begin
    SrcHelloExtension := Options.FClientHelloExtensions[i];
    DstHelloExtension := FClientHelloExtensions.FindOrCreate(TTLSHelloExtensionClass(SrcHelloExtension.ClassType));
    DstHelloExtension.Assign(SrcHelloExtension);
  end;
end;

function TScSSLClient.GetLastException: Exception;
begin
  if FSecureSocket <> nil then
    Result := FSecureSocket.GetLastException
  else
    Result := nil;
end;

procedure TScSSLClient.SetVio(Vio: TCRVioTcp);
begin
  if FEventsCallMode = ecAsynchronous then
    CheckIfAsyncEventProcessorStarted;

  FreeAndNil(FSecureSocket);
  FSecureSocket := TSecureSocket.Create(Vio, True);
  FSecureSocket.OnClose := DoAfterDisconnect;
  FSecureSocket.Timeout := FTimeout;
  if Assigned(OnAsyncReceive) then
    FSecureSocket.OnAsyncReceive := DoReceiveData;
end;

procedure TScSSLClient.DoConnect;
var
  CACertificate, ClientCertificate: TScCertificate;
begin
  if FHostname = '' then
    raise EScError.Create(seWrongHostname);

  if FPort = 0 then
    raise EScError.Create(seWrongPort);

  if CACertName <> '' then begin
    if Storage = nil then
      raise EScError.Create(seStorageNoSet);

    CACertificate := Storage.Certificates.FindCertificate(CACertName);
    if CACertificate <> nil then
      CACertificate.Ready := True;
  end;

  if CertName <> '' then begin
    if Storage = nil then
      raise EScError.Create(seStorageNoSet);

    ClientCertificate := Storage.Certificates.FindCertificate(CertName);
    if ClientCertificate <> nil then begin
      ClientCertificate.Ready := True;

      if not ClientCertificate.Key.IsPrivate then
        raise EScError.Create(seCertificateMustBePrivate);
    end;
  end;

  if FEventsCallMode = ecAsynchronous then
    CheckIfAsyncEventProcessorStarted;

  FreeAndNil(FSecureSocket);
  FSecureSocket := TSecureSocket.Create(FHttpOptions, FProxyOptions,
    FProviderName, FHostname, FPort, FIPVersion);
  try
    FSecureSocket.OnClose := DoAfterDisconnect;
    FSecureSocket.Timeout := FTimeout;

    if FBindAddress <> '' then
      FSecureSocket.Bind(FBindAddress);

    FSecureSocket.Connect;

    if Assigned(OnAsyncReceive) then
      FSecureSocket.OnAsyncReceive := DoReceiveData;
  except
    FreeAndNil(FSecureSocket);
    raise;
  end;
end;

procedure TScSSLClient.DoDisconnect;
begin
  if GetConnected then
    try
      FSecureSocket.Shutdown;
    except
    end;

  if FSecureSocket <> nil then
    FSecureSocket.Close;
end;

function TScSSLClient.IsClosed: boolean;
begin
  if FSecureSocket = nil then
    Result := True
  else
    Result := not FSecureSocket.Connected and not FSecureSocket.HasData;
end;

function TScSSLClient.GetConnected: boolean;
begin
  Result := (FSecureSocket <> nil) and FSecureSocket.Connected;
end;

procedure TScSSLClient.SetConnected(Value: boolean);
begin
  if (csReading in ComponentState) and Value then
    FStreamedConnected := True
  else begin
    if Value = GetConnected then
      Exit;

    if Value then begin
      if Assigned(BeforeConnect) then
        BeforeConnect(Self);

      DoConnect;

      if Assigned(AfterConnect) then
        AfterConnect(Self);
    end
    else begin
      if Assigned(BeforeDisconnect) then
        BeforeDisconnect(Self);

      DoDisconnect;
    end;
  end;
end;

procedure TScSSLClient.Connect;
begin
  SetConnected(True);
end;

procedure TScSSLClient.Disconnect;
begin
  SetConnected(False);
end;

procedure TScSSLClient.Loaded;
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

procedure TScSSLClient.AssignSession(Source: TScSSLClient);
var
  SrcSessionTicketExtension: TTLSSessionTicketExtension;
begin
  if Source = nil then
    raise EScError.Create(seInvalidInputArgs);

  if Source.GetSessionInfo = nil then
    raise EScError.Create(seSourceConnectionNotSecure);

  if Source.GetSessionInfo.Protocol = spTls13 then begin
    if Source.GetSessionInfo.NewSessionTickets.Count = 0 then
      raise EScError.Create(seSourceSessionTicketAbsent);
  end
  else begin
    SrcSessionTicketExtension := TTLSSessionTicketExtension(Source.ServerHelloExtensions.Find(TTLSSessionTicketExtension));
    if (SrcSessionTicketExtension = nil) and (Length(Source.GetSessionInfo.SessionID) = 0) then
      raise EScError.Create(seSourceSessionTicketAbsent);
  end;

  AssignSession(Source.GetSessionInfo);
end;

procedure TScSSLClient.AssignSession(SourceSessionInfo: TScSSLSessionInfo);
var
  DstSessionTicketExtension: TTLSSessionTicketExtension;
begin
  if SourceSessionInfo = nil then
    raise EScError.Create(seInvalidInputArgs);

  if FAssignedSessionInfo = nil then
    FAssignedSessionInfo := TScSSLSessionInfo.Create;

  FAssignedSessionInfo.Assign(SourceSessionInfo);
  FAssignedSessionInfo.Initialized := True;

  // set options from source
  FCipherSuites.Clear;
  (FCipherSuites.Add as TScSSLCipherSuiteItem).CipherAlgorithm := FAssignedSessionInfo.CipherAlgorithm;
  FCompression := FAssignedSessionInfo.Compression;
  FProtocols := [FAssignedSessionInfo.Protocol];
  FDisableInsertEmptyFragment := FAssignedSessionInfo.DisableInsertEmptyFragment;

  if FAssignedSessionInfo.Protocol <> spTls13 then begin
    FSecurityOptions.UseSecureSessionResumption := True;

    DstSessionTicketExtension := TTLSSessionTicketExtension(ClientHelloExtensions.Find(TTLSSessionTicketExtension));
    if DstSessionTicketExtension = nil then
      raise EScError.Create(seMissingExtension);

    if FAssignedSessionInfo.NewSessionTickets.Count > 0 then
      DstSessionTicketExtension.Ticket := TScNewSessionTicket(FAssignedSessionInfo.NewSessionTickets[0]).Ticket
    else
      DstSessionTicketExtension.Ticket := nil;
  end;
end;

procedure TScSSLClient.SetServerNameExtension;
var
  ServerNameExtension: TTLSServerNameExtension;
begin
  if FSecurityOptions.FIdentityDNSName <> '' then begin
    ServerNameExtension := TTLSServerNameExtension(FClientHelloExtensions.FindOrCreate(TTLSServerNameExtension));

    if ServerNameExtension.ServerNames.IndexOf(FSecurityOptions.FIdentityDNSName) = -1 then
      ServerNameExtension.ServerNames.Add(FSecurityOptions.FIdentityDNSName);
  end;
end;

function TScSSLClient.GetIsSecure: boolean;
begin
  if FSecureSocket <> nil then
    Result := FSecureSocket.IsSecure
  else
    Result := False;
end;

procedure TScSSLClient.SetIsSecure(Value: boolean);
var
  SecOptions: TScSecurityOptions;
  Certificate: TScCertificate;
begin
  if FSecureSocket = nil then
    raise EScError.Create(seClientClosed);

  if FSecureSocket.IsSecure <> Value then begin
    if Value then begin
      SecOptions := TScSecurityOptions.Create;
      try
        SetServerNameExtension;

        // ApplicationLayerProtocolNegotiationExtension := TTLSApplicationLayerProtocolNegotiationExtension.Create;
        // ApplicationLayerProtocolNegotiationExtension.Add('h2');
        // ApplicationLayerProtocolNegotiationExtension.Add('http/1.1');
        // FOptions.ClientHelloExtensions.Add(ApplicationLayerProtocolNegotiationExtension);

        // create a security options for secure connection
        SecOptions.Protocols := Protocols;
        SecOptions.CipherAlgorithms := FCipherSuites.GetCipherSuites;
        SecOptions.Compression := FCompression;
        SecOptions.IgnoreRemoteCertificateConstraints := FSecurityOptions.IgnoreServerCertificateConstraints;
        SecOptions.IgnoreRemoteCertificateInsecurity := FSecurityOptions.IgnoreServerCertificateInsecurity;
        SecOptions.IgnoreRemoteCertificateValidity := FSecurityOptions.IgnoreServerCertificateValidity;
        SecOptions.TrustSelfSignedCertificate := FSecurityOptions.TrustSelfSignedCertificate;
        SecOptions.TrustRemoteCertificate := FSecurityOptions.TrustServerCertificate;
        SecOptions.UseClientInitiatedRenegotiation := FSecurityOptions.UseClientInitiatedRenegotiation;
        SecOptions.ClientHelloExtensions := FClientHelloExtensions;
        SecOptions.ServerHelloExtensions := FServerHelloExtensions;
        SecOptions.DisableInsertEmptyFragment := FDisableInsertEmptyFragment;
        SecOptions.DisableCloseSocketOnShutdownAlert := FDisableCloseSocketOnShutdownAlert;
        SecOptions.MinDHEKeyLength := FMinDHEKeyLength;

        if (Int64(FTimeout) * 1000) > MaxInt then
          SecOptions.Timeout := MaxInt
        else
          SecOptions.Timeout := FTimeout * 1000;

        SecOptions.AssignedSessionInfo := FAssignedSessionInfo;
        if FAssignedSessionInfo <> nil then begin
          if (FAssignedSessionInfo.Protocol <> spTls13) and not FSecurityOptions.UseSecureSessionResumption then
            FAssignedSessionInfo.Initialized := False;
        end;

        if Storage <> nil then begin
          if CertName <> '' then begin
            Certificate := Storage.Certificates.FindCertificate(CertName);
            if (Certificate <> nil) and not Certificate.Key.IsPrivate then
              raise EScError.Create(seCertificateMustBePrivate);
          end;

          SecOptions.OnCertificateRequest := DoCertificateRequest;
          SecOptions.CACertificate := Storage.Certificates.FindCertificate(CACertName);

          if FSecurityOptions.TrustStorageCertificates then
            SecOptions.CAStorage := Storage;
        end;

        SecOptions.OnRemoteCertificateValidation := DoServerCertificateValidation;
        if not FSecurityOptions.DisableCRLValidation then
          SecOptions.OnObtainCRL := DoObtainCRL;

        FSecureSocket.Options := SecOptions;
      finally
        SecOptions.Free;
      end;
    end;

    FSecureSocket.IsSecure := Value;
  end;
end;

procedure TScSSLClient.SetSocketOption(OptionLevel, OptionName, OptionValue: integer);
begin
  if Connected then
    FSecureSocket.SetSocketOption(OptionLevel, OptionName, OptionValue);
end;

procedure TScSSLClient.Renegotiate;
begin
  if Connected and IsSecure then
    FSecureSocket.QueueRenegotiate;
end;

function TScSSLClient.ReadBuffer(var Buffer; const Count: integer): integer;
begin
  if not IsClosed then
    Result := FSecureSocket.Read(@Buffer, 0, Count)
  else
    Result := 0;
end;

function TScSSLClient.ReadBuffer(var Buffer: TBytes; const Offset, Count: integer): integer;
begin
  if not IsClosed then
    Result := FSecureSocket.Read(TValueArr(Buffer), Offset, Count)
  else
    Result := 0;
end;

function TScSSLClient.ReadNoWait(var Buffer; const Count: integer): integer;
begin
  if not IsClosed then
    Result := FSecureSocket.ReadNoWait(@Buffer, 0, Count)
  else
    Result := 0;
end;

function TScSSLClient.ReadNoWait(var Buffer: TBytes; const Offset, Count: integer): integer;
begin
  if not IsClosed then
    Result := FSecureSocket.ReadNoWait(TValueArr(Buffer), Offset, Count)
  else
    Result := 0;
end;

function TScSSLClient.WriteBuffer(const Buffer; const Count: integer): integer;
begin
  if Connected then
    Result := FSecureSocket.Send(@Buffer, 0, Count)
  else
    Result := 0;
end;

function TScSSLClient.WriteBuffer(const Buffer: TBytes; const Offset, Count: integer): integer;
begin
  if Connected then
    Result := FSecureSocket.Send(TValueArr(Buffer), Offset, Count)
  else
    Result := 0;
end;

function TScSSLClient.WaitForData(MillisecondsTimeout: integer = -1): boolean;
begin
  if not IsClosed then
    Result := FSecureSocket.WaitForData(MillisecondsTimeout)
  else
    Result := False;
end;

function TScSSLClient.GetLocalIP: string;
begin
  if FSecureSocket <> nil then
    Result := FSecureSocket.GetLocalIP
  else
    Result := '';
end;

function TScSSLClient.GetLocalPort: integer;
begin
  if FSecureSocket <> nil then
    Result := FSecureSocket.GetLocalPort
  else
    Result := 0;
end;

function TScSSLClient.GetRemoteIP: string;
begin
  if FSecureSocket <> nil then
    Result := FSecureSocket.GetRemoteIP
  else
    Result := '';
end;

function TScSSLClient.GetRemotePort: integer;
begin
  if FSecureSocket <> nil then
    Result := FSecureSocket.GetRemotePort
  else
    Result := 0;
end;

procedure TScSSLClient.CheckInactive;
begin
  if Connected then
    raise EScError.Create(seClientOpened);
end;

procedure TScSSLClient.CheckSecured;
begin
  if Connected and IsSecure then
    raise EScError.Create(seClientOpened);
end;

function TScSSLClient.GetInCount: integer;
begin
  if not IsClosed then
    Result := FSecureSocket.Available
  else
    Result := 0;
end;

function TScSSLClient.GetOutCount: integer;
begin
  Result := 0;
end;

procedure TScSSLClient.SetOnAsyncReceive(Value: TScAsyncReceiveEvent);
begin
  if @Value <> @FOnAsyncReceive then begin
    FOnAsyncReceive := Value;

    if FSecureSocket <> nil then begin
      if Assigned(Value) then
        FSecureSocket.OnAsyncReceive := DoReceiveData
      else
        FSecureSocket.OnAsyncReceive := nil; // to make sure, that DoReceiveData will not be called more
    end;
  end;
end;

procedure TScSSLClient.DoReceiveData(Sender: TObject);
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

procedure TScSSLClient.NotifyOnAsyncReceive;
var
  Fail: boolean;
begin
  try
    if Assigned(FOnAsyncReceive) then
      FOnAsyncReceive(Self);
  except
    on E: Exception do
      if not (E is EAbort) then begin
        Fail := FEventsCallMode = ecDirectly;
        DoAsyncError(E, Fail);
        if Fail then
          raise;
      end;
  end;
end;

procedure TScSSLClient.DoAsyncError(E: Exception; var Fail: boolean);
begin
  if Assigned(FOnAsyncError) then
    FOnAsyncError(Self, E);
end;

procedure TScSSLClient.DoAfterDisconnect(Sender: TObject);
begin
  if Assigned(FAfterDisconnect) then
    FAfterDisconnect(Self);
end;

procedure TScSSLClient.SetEventsCallMode(Value: TScEventCallMode);
begin
  if Value <> FEventsCallMode then begin
    CheckInactive;
    FEventsCallMode := Value;
  end;
end;

function TScSSLClient.GetSessionInfo: TScSSLSessionInfo;
begin
  if FSecureSocket <> nil then
    Result := FSecureSocket.SessionInfo
  else
    Result := nil;
end;

procedure TScSSLClient.Notification(Component: TComponent; Operation: TOperation);
begin
  if (Component = FStorage) and (Operation = opRemove) then
    Storage := nil;

  inherited;
end;

procedure TScSSLClient.SetCertName(const Value: string);
begin
  if Value <> FCertName then begin
    CheckSecured;
    FCertName := Value;
  end;
end;

procedure TScSSLClient.SetCACertName(const Value: string);
begin
  if Value <> FCACertName then begin
    CheckSecured;
    FCACertName := Value;
  end;
end;

procedure TScSSLClient.SetStorage(Value: TScStorage);
begin
  if FStorage <> Value then begin
    if FStorage <> nil then
      FStorage.RemoveFreeNotification(Self);

    SetConnected(False);
    FStorage := Value;

    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

procedure TScSSLClient.DoCertificateRequest(Acceptable: TScDistinguishedNameList; CertList: TCRList);
var
  DN: TScDistinguishedName;
  Certificate: TScCertificate;
  TmpCert: TScCertificate;
  i, c: integer;
begin
  Certificate := nil;

  if Storage <> nil then begin
    if CertName <> '' then begin
      Certificate := Storage.Certificates.FindCertificate(CertName);
      if (Certificate <> nil) and not Certificate.Key.IsPrivate then
        Certificate := nil;
    end
    else begin
      for i := 0 to Acceptable.Count - 1 do begin
        DN := Acceptable[i];

        for c := 0 to Storage.Certificates.Count - 1 do begin
          TmpCert := Storage.Certificates[c];
          if DN.Equals(TmpCert.IssuerName) and TmpCert.Key.IsPrivate then begin
            Certificate := TmpCert;
            Break;
          end;
        end;

        if Certificate <> nil then
          Break;
      end;
    end;
  end;

  if Certificate <> nil then
    CertList.Add(Certificate);
end;

procedure TScSSLClient.DoServerCertificateValidation(Sender: TObject; ServerCertificate: TScCertificate;
  CertificateList: TCRList; var Errors: TScCertificateStatusSet);
begin
  if Assigned(FOnServerCertificateValidation) then
    FOnServerCertificateValidation(Self, ServerCertificate, CertificateList, Errors);
end;

procedure TScSSLClient.DoObtainCRL(Sender: TObject; DistributionPointName: TScGeneralNames; Update: boolean; out CRL: TScCRL);
begin
  TScCRLList.DoObtainCRL(Self, Storage, FSecurityOptions.AllowLoadCRLByHttp,
    FOnObtainCRL, DistributionPointName, Update, CRL);
end;

procedure TScSSLClient.OnCipherPropertyChanged(Sender: TObject);
begin
  CheckSecured;
end;

procedure TScSSLClient.SetHttpOptions(Value: THttpOptions);
begin
  CheckInactive;
  FHttpOptions.Assign(Value);
end;

procedure TScSSLClient.SetProxyOptions(Value: TProxyOptions);
begin
  CheckInactive;
  FProxyOptions.Assign(Value);
end;

procedure TScSSLClient.SetSecurityOptions(Value: TScSSLSecurityOptions);
begin
  CheckSecured;
  FSecurityOptions.Assign(Value);
end;

procedure TScSSLClient.SetCipherSuites(Value: TScSSLCipherSuites);
begin
  if Value <> FCipherSuites then begin
    CheckSecured;
    FCipherSuites.Assign(Value);
  end;
end;

procedure TScSSLClient.SetCompression(const Value: TScCompression);
begin
  if Value <> FCompression then begin
    CheckSecured;
    FCompression := Value;
  end;
end;

procedure TScSSLClient.SetProtocols(const Value: TScSSLProtocols);
begin
  if Value <> FProtocols then begin
    CheckSecured;
    FProtocols := Value;
  end;
end;

procedure TScSSLClient.SetBindAddress(const Value: string);
begin
  if Value <> FBindAddress then begin
    CheckInactive;
    FBindAddress := Trim(Value);
  end;
end;

procedure TScSSLClient.SetHostName(const Value: string);
begin
  if Value <> FHostName then begin
    CheckInactive;
    FHostName := Trim(Value);
  end;
end;

procedure TScSSLClient.SetPort(const Value: integer);
begin
  if Value <> FPort then begin
    CheckInactive;
    FPort := Value;
  end;
end;

procedure TScSSLClient.SetIPVersion(const Value: TIPVersion);
begin
  if Value <> FIPVersion then begin
    CheckInactive;
    FIPVersion := Value;
  end;
end;

procedure TScSSLClient.SetTimeout(const Value: integer);
begin
  if Value <> FTimeout then begin
    FTimeout := Value;

    if Connected then
      FSecureSocket.Timeout := FTimeout;
  end;
end;

procedure TScSSLClient.SetDisableInsertEmptyFragment(Value: boolean);
begin
  if Value <> FDisableInsertEmptyFragment then begin
    CheckSecured;
    FDisableInsertEmptyFragment := Value;
  end;
end;

procedure TScSSLClient.SetDisableCloseSocketOnShutdownAlert(Value: boolean);
begin
  if Value <> FDisableCloseSocketOnShutdownAlert then begin
    FDisableCloseSocketOnShutdownAlert := Value;
    if Assigned(FSecureSocket) then
      FSecureSocket.Options.DisableCloseSocketOnShutdownAlert := Value;
  end;
end;

procedure TScSSLClient.SetMinDHEKeyLength(Value: integer);
begin
  if Value <> FMinDHEKeyLength then begin
    CheckSecured;
    FMinDHEKeyLength := Value;
  end;
end;

end.
