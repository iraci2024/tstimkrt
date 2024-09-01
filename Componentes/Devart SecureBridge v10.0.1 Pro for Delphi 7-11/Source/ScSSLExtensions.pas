
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF DAC}
{$I SB.inc}
unit ScSSLExtensions;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, SyncObjs, Classes,
{$IFNDEF SBRIDGE}
  CRTypes, CRFunctions, CLRClasses, CRDECUtil, CRBigInteger,
{$IFNDEF UNIDACPRO}
  TdsUtils, TdsSSLConsts, TdsBridge, TdsCertificateExts,
  TdsSSLTypes, TdsSSLMessages;
{$ELSE}
  TdsUtilsUni, TdsSSLConstsUni, TdsBridgeUni, TdsCertificateExtsUni,
  TdsSSLTypesUni, TdsSSLMessagesUni;
{$ENDIF}
{$ELSE}
  ScTypes, ScFunctions, ScCLRClasses, ScDECUtil, ScBigInteger,
  ScUtils, ScConsts, ScBridge, ScCertificateExts,
  ScSSLTypes, ScSSLMessages;
{$ENDIF}

type
  TTLSHelloExtensions = class;
  TScNewSessionTicketExt = class;

  TScCreateNewSessionTicketEvent = procedure (Sender: TObject; SessionInfo: TScSSLSessionInfo;
    NewSessionTicket: TScNewSessionTicket) of object;
  TScDecodeTicketEvent = procedure (Sender: TObject; NewSessionTicket: TScNewSessionTicket;
    NewSessionInfo: TScSSLSessionInfo; var IsValid: boolean) of object;

  /// Represents the security options that should be used when connecting to a secure server, or when accepting secure connections.
  TScSecurityOptions = class(TPersistent)
  private
    FCipherAlgorithms: TScSSLCipherAlgorithmArray;
    FCompression: TScCompression;
    FProtocols: TScSSLProtocols;
    FEntity: TScSSLConnectionEnd;
    FOnCertificateRequest: TScOnCertificateRequest;
    FOnRemoteCertificateValidation: TScRemoteCertificateValidationEvent;
    FOnObtainCRL: TScObtainCRLEvent;
    FOnNeedVerifyCertificate: TScOnNeedVerifyCertificate;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FCACertificate: TScCertificate;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FCAStorage: TScStorage;
    FIgnoreRemoteCertificateConstraints: boolean;
    FIgnoreRemoteCertificateInsecurity: boolean;
    FIgnoreRemoteCertificateValidity: boolean;
    FTrustSelfSignedCertificate: boolean;
    FTrustRemoteCertificate: boolean;
    FIdentityDNSName: string;
    FCertificateListIsUnsorted: boolean;
    FDisableInsertEmptyFragment: boolean;
    FDisableCloseSocketOnShutdownAlert: boolean;
    FDisableCloseOnDestroy: boolean;
    FAsyncStartTLS: boolean;
    FMinDHEKeyLength: integer;
    FUseClientInitiatedRenegotiation: boolean;
    FTimeout: integer;
    FExtendedMasterSecretMode: TScExtendedMasterSecretMode;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FClientHelloExtensions: TTLSHelloExtensions;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FServerHelloExtensions: TTLSHelloExtensions;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FAssignedSessionInfo: TScSSLSessionInfo;
    FClientInitiatedRenegotiationIsAllowed: boolean;
    FRecordSizeLimit: integer;
    FRequestDNList: TScDistinguishedNameList;
    FIsClientCertificateRequired: boolean;
    FNewSessionTicketLifetime: integer;
    FNewSessionTicketDistributedCount: integer;
    FSupportedKExNamedGroups: TScKExNamedGroupTypes;
    FAfterClientHello: TScAfterClientHelloMessageEvent;
    FOnCreateNewSessionTicket: TScCreateNewSessionTicketEvent;
    FOnDecodeTicket: TScDecodeTicketEvent;
    FOnGetTicketNameAndPassword: TScGetTicketNameAndPasswordEvent;
    FOnGetPasswordByTicketName: TScGetPasswordByTicketNameEvent;

    procedure SetCipherAlgorithms(const Value: TScSSLCipherAlgorithmArray);

  protected
    procedure AssignTo(Dest: TPersistent); override;

  public
    constructor Create;

    property Entity: TScSSLConnectionEnd read FEntity write FEntity;
    property CipherAlgorithms: TScSSLCipherAlgorithmArray read FCipherAlgorithms write SetCipherAlgorithms;
    property Compression: TScCompression read FCompression write FCompression;
    property Protocols: TScSSLProtocols read FProtocols write FProtocols;
    property OnCertificateRequest: TScOnCertificateRequest read FOnCertificateRequest write FOnCertificateRequest;
    property OnRemoteCertificateValidation: TScRemoteCertificateValidationEvent read FOnRemoteCertificateValidation write FOnRemoteCertificateValidation;
    property OnObtainCRL: TScObtainCRLEvent read FOnObtainCRL write FOnObtainCRL;
    property OnNeedVerifyCertificate: TScOnNeedVerifyCertificate read FOnNeedVerifyCertificate write FOnNeedVerifyCertificate;
    property CACertificate: TScCertificate read FCACertificate write FCACertificate;
    property CAStorage: TScStorage read FCAStorage write FCAStorage;
    property IgnoreRemoteCertificateConstraints: boolean read FIgnoreRemoteCertificateConstraints write FIgnoreRemoteCertificateConstraints;
    property IgnoreRemoteCertificateInsecurity: boolean read FIgnoreRemoteCertificateInsecurity write FIgnoreRemoteCertificateInsecurity;
    property IgnoreRemoteCertificateValidity: boolean read FIgnoreRemoteCertificateValidity write FIgnoreRemoteCertificateValidity;
    property TrustSelfSignedCertificate: boolean read FTrustSelfSignedCertificate write FTrustSelfSignedCertificate;
    property TrustRemoteCertificate: boolean read FTrustRemoteCertificate write FTrustRemoteCertificate;
    property IdentityDNSName: string read FIdentityDNSName write FIdentityDNSName;
    property CertificateListIsUnsorted: boolean read FCertificateListIsUnsorted write FCertificateListIsUnsorted;
    property Timeout: integer read FTimeout write FTimeout;
    property ClientHelloExtensions: TTLSHelloExtensions read FClientHelloExtensions write FClientHelloExtensions;
    property ServerHelloExtensions: TTLSHelloExtensions read FServerHelloExtensions write FServerHelloExtensions;
    property DisableCloseSocketOnShutdownAlert: boolean read FDisableCloseSocketOnShutdownAlert write FDisableCloseSocketOnShutdownAlert;
    property DisableCloseOnDestroy: boolean read FDisableCloseOnDestroy write FDisableCloseOnDestroy;

  // Client options
    property AssignedSessionInfo: TScSSLSessionInfo read FAssignedSessionInfo write FAssignedSessionInfo;
    property DisableInsertEmptyFragment: boolean read FDisableInsertEmptyFragment write FDisableInsertEmptyFragment;
    property MinDHEKeyLength: integer read FMinDHEKeyLength write FMinDHEKeyLength;
    property UseClientInitiatedRenegotiation: boolean read FUseClientInitiatedRenegotiation write FUseClientInitiatedRenegotiation;

  // Server options
    property AsyncStartTLS: boolean read FAsyncStartTLS write FAsyncStartTLS;
    property ExtendedMasterSecretMode: TScExtendedMasterSecretMode read FExtendedMasterSecretMode write FExtendedMasterSecretMode;
    property ClientInitiatedRenegotiationIsAllowed: boolean read FClientInitiatedRenegotiationIsAllowed write FClientInitiatedRenegotiationIsAllowed;
    property RecordSizeLimit: integer read FRecordSizeLimit write FRecordSizeLimit;
    property RequestDNList: TScDistinguishedNameList read FRequestDNList write FRequestDNList;
    property IsClientCertificateRequired: boolean read FIsClientCertificateRequired write FIsClientCertificateRequired;
    property NewSessionTicketLifetime: integer read FNewSessionTicketLifetime write FNewSessionTicketLifetime;
    property NewSessionTicketDistributedCount: integer read FNewSessionTicketDistributedCount write FNewSessionTicketDistributedCount;
    property SupportedKExNamedGroups: TScKExNamedGroupTypes read FSupportedKExNamedGroups write FSupportedKExNamedGroups;
    property AfterClientHello: TScAfterClientHelloMessageEvent read FAfterClientHello write FAfterClientHello;
    property OnCreateNewSessionTicket: TScCreateNewSessionTicketEvent read FOnCreateNewSessionTicket write FOnCreateNewSessionTicket;
    property OnDecodeTicket: TScDecodeTicketEvent read FOnDecodeTicket write FOnDecodeTicket;
    property OnGetTicketNameAndPassword: TScGetTicketNameAndPasswordEvent read FOnGetTicketNameAndPassword write FOnGetTicketNameAndPassword;
    property OnGetPasswordByTicketName: TScGetPasswordByTicketNameEvent read FOnGetPasswordByTicketName write FOnGetPasswordByTicketName;
  end;

  TScNewSessionTicketExt = class(TScNewSessionTicket)
  private
    FExtensions: TTLSHelloExtensions;

  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property Extensions: TTLSHelloExtensions read FExtensions;
  end;

  TTLSExtensionType = (
    etUnknown,
    etServerName, etMaxFragmentLength, etRecordSizeLimit,
    etExtendedMasterSecret,
    etSessionTicket, etSignatureAlgorithms, etCertificateStatusRequest,
    etSignedCertificateTimestamp, etApplicationLayerProtocolNegotiation,
    etTLSChannelId,
    etSupportedGroups, etEllipticCurvePointFormats,
    etRenegotiationIndication, etClientHelloPadding,
    etSupportedVersions,
    etPskKeyExchangeModes, etPreSharedKey, etEarlyData,
    etCookie, etSignatureAlgorithmsCert, etKeyShare,
    etPostHandshakeAuth
  );

  TTLSHelloExtension = class(TPersistent)
  protected
    FEntity: TScSSLConnectionEnd;
    function GetTypeCode: word; virtual;
    function GetType: TTLSExtensionType; virtual; abstract;
    procedure Encode(Message: THandshakeMessage); virtual; abstract;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    procedure Parse(Message: THandshakeMessage); virtual; abstract;
  end;

  TTLSHelloExtensionClass = class of TTLSHelloExtension;

  TTLSUnknownExtension = class(TTLSHelloExtension)
  private
    FTypeCode: word;
    FData: TBytes;

  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetTypeCode: word; override;
    function GetType: TTLSExtensionType; override;
    procedure Encode(Message: THandshakeMessage); override;

  public
    procedure Parse(Message: THandshakeMessage); override;

    property TypeCode: word read FTypeCode;
  end;

  // https://tools.ietf.org/html/rfc5746
  TTLSRenegotiationIndicationExtension = class(TTLSHelloExtension)
  private
    FIsRenegotiation: boolean;
    FLocalClientFinishedMessage: TBytes;
    FLocalServerFinishedMessage: TBytes;
    FRemoteClientFinishedMessage: TBytes;
    FRemoteServerFinishedMessage: TBytes;
    FIsRemoteSupport: boolean;

  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetType: TTLSExtensionType; override;
    procedure Encode(Message: THandshakeMessage); override;
  public
    constructor Create; override;
    procedure Parse(Message: THandshakeMessage); override;

    procedure Clear;
    procedure Renegotiate;
    procedure Check(ReceivedExtension: TTLSRenegotiationIndicationExtension);
    procedure SetClientFinishedMessage(const Value: TBytes; Offset, Size: integer);
    procedure SetServerFinishedMessage(const Value: TBytes; Offset, Size: integer);

    property IsRemoteSupport: boolean read FIsRemoteSupport;
  end;

  // https://tools.ietf.org/search/rfc4366#section-3.1
  TTLSServerNameExtension = class(TTLSHelloExtension)
  private
    FServerNames: TStringList;

  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetType: TTLSExtensionType; override;
    procedure Encode(Message: THandshakeMessage); override;

  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Parse(Message: THandshakeMessage); override;

    property ServerNames: TStringList read FServerNames;
  end;

  // https://tools.ietf.org/html/rfc6066#page-8
  TScMaxFragmentLengthType = (mfl_2_9, mfl_2_10, mfl_2_11, mfl_2_12);

  TTLSMaxFragmentLengthExtension = class(TTLSHelloExtension)
  private
    FMaxFragmentLength: TScMaxFragmentLengthType;
    function GetMaxFragmentLengthInByte: integer;

  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetType: TTLSExtensionType; override;
    procedure Encode(Message: THandshakeMessage); override;

  public
    procedure Parse(Message: THandshakeMessage); override;

    property MaxFragmentLength: TScMaxFragmentLengthType read FMaxFragmentLength write FMaxFragmentLength;
    property MaxFragmentLengthInByte: integer read GetMaxFragmentLengthInByte;
  end;

  // https://tools.ietf.org/html/rfc8449
  TTLSRecordSizeLimitExtension = class(TTLSHelloExtension)
  private
    FRecordSizeLimit: word;
    procedure SetRecordSizeLimit(Value: word);

  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetType: TTLSExtensionType; override;
    procedure Encode(Message: THandshakeMessage); override;

  public
    constructor Create; override;
    procedure Parse(Message: THandshakeMessage); override;

    property RecordSizeLimit: word read FRecordSizeLimit write SetRecordSizeLimit;
  end;

  // https://tools.ietf.org/search/rfc7627
  TTLSExtendedMasterSecretExtension = class(TTLSHelloExtension)
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetType: TTLSExtensionType; override;
    procedure Encode(Message: THandshakeMessage); override;
  public
    procedure Parse(Message: THandshakeMessage); override;
  end;

  // https://tools.ietf.org/html/rfc5077
  TTLSSessionTicketExtension = class(TTLSHelloExtension)
  private
    FTicket: TBytes;

  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetType: TTLSExtensionType; override;
    procedure Encode(Message: THandshakeMessage); override;
  public
    procedure Parse(Message: THandshakeMessage); override;

    property Ticket: TBytes read FTicket write FTicket;
  end;

  TTLSSignatureAndHashAlgorithm = record
    Hash: TScHashAlgorithm;
    Signature: TScSSLSignatureAlgorithm;
    Padding: TScPaddingMode;
    // https://tools.ietf.org/html/rfc8446#section-4.2.3
    // RSASSA-PSS RSAE  vs  RSASSA-PSS PSS
    KeyPadding: TScPaddingMode;
  end;

  // https://tools.ietf.org/html/rfc5246#section-7.4.1.4.1
  // https://tools.ietf.org/html/rfc8446#section-4.2.3
  TTLSSignatureAlgorithmsExtension = class(TTLSHelloExtension)
  private
    FSignatureSchemes: array of TScSSLSignatureScheme;
    FCount: integer;

    function GetSignatureScheme(Index: integer): TScSSLSignatureScheme;

  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetType: TTLSExtensionType; override;
    procedure Encode(Message: THandshakeMessage); override;

  public
    constructor Create; override;
    procedure Parse(Message: THandshakeMessage); override;

    class function CreateDefault: TTLSSignatureAlgorithmsExtension;

    procedure Add(Hash: TScHashAlgorithm; Signature: TScSSLSignatureAlgorithm); overload;
    procedure Add(SignatureScheme: TScSSLSignatureScheme); overload;
    procedure Clear;
    property Count: integer read FCount;
    property SignatureSchemes[Index: integer]: TScSSLSignatureScheme read GetSignatureScheme;
  end;

  TTLSSignatureAlgorithmsCertExtension = class(TTLSSignatureAlgorithmsExtension)
  protected
    function GetType: TTLSExtensionType; override;
  end;

  // http://www.networksorcery.com/enp/rfc/rfc4366.txt
  TTLSCertificateStatusRequestExtension = class(TTLSHelloExtension)
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetType: TTLSExtensionType; override;
    procedure Encode(Message: THandshakeMessage); override;
  public
    procedure Parse(Message: THandshakeMessage); override;
  end;

  // https://tools.ietf.org/html/rfc6962
  TTLSSignedCertificateTimestampExtension = class(TTLSHelloExtension)
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetType: TTLSExtensionType; override;
    procedure Encode(Message: THandshakeMessage); override;
  public
    procedure Parse(Message: THandshakeMessage); override;
  end;

  // https://tools.ietf.org/html/rfc7301
  TTLSApplicationLayerProtocolNegotiationExtension = class(TTLSHelloExtension)
  private
    FProtocolNames: TStringList;

  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetType: TTLSExtensionType; override;
    procedure Encode(Message: THandshakeMessage); override;

  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Parse(Message: THandshakeMessage); override;

    property ProtocolNames: TStringList read FProtocolNames;
  end;

  // only for Chrome
  TTLSChannelIdExtension = class(TTLSHelloExtension)
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetType: TTLSExtensionType; override;
    procedure Encode(Message: THandshakeMessage); override;
  public
    procedure Parse(Message: THandshakeMessage); override;
  end;

  // https://tools.ietf.org/search/rfc4492
  TScECPointFormat = (
    ecpfUncompressed, ecpfAnsiX962CompressedPrime, ecpfAnsiX962CompressedChar2);
  TScECPointFormats = set of TScECPointFormat;

  TTLSEllipticCurvePointFormatsExtension = class(TTLSHelloExtension)
  private
    FFormats: TScECPointFormats;

  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetType: TTLSExtensionType; override;
    procedure Encode(Message: THandshakeMessage); override;
  public
    constructor Create; override;
    procedure Parse(Message: THandshakeMessage); override;

    property Formats: TScECPointFormats read FFormats write FFormats;
  end;

  // https://tools.ietf.org/search/rfc4492#section-5.1.1
  // https://tools.ietf.org/html/rfc8446#section-4.2.7
  TTLSSupportedGroupsExtension = class(TTLSHelloExtension)
  private
    FKExNamedGroupList: array of TScKExNamedGroupType;
    FCount: integer;

    function GetKExNamedGroup(Index: integer): TScKExNamedGroupType;

  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetType: TTLSExtensionType; override;
    procedure Encode(Message: THandshakeMessage); override;
  public
    constructor Create; override;
    procedure Parse(Message: THandshakeMessage); override;

    procedure Add(const ECName: TScECName); overload;
    procedure Add(const FFGType: TScFiniteFieldGroupType); overload;
    procedure Add(const KExNamedGroup: TScKExNamedGroupType); overload;
    procedure Clear;

    property Count: integer read FCount;
    property KExNamedGroups[Index: integer]: TScKExNamedGroupType read GetKExNamedGroup;
  end;

  // https://tools.ietf.org/html/rfc8446#section-4.2.8
  TScKeyShareEntry = record
    KExNamedGroup: TScKExNamedGroupType;
    PrivateECKey: TScKey;
    PublicECPoint: TScCustomECPoint;
    PrivateDHX: TBigInteger;
    PublicDHY: TBigInteger;
  end;

  TTLSKeyShareExtension = class(TTLSHelloExtension)
  private
    FEntries: array of TScKeyShareEntry;
    FCount: integer;
    FOnlyRequest: boolean;

    function GetKExNamedGroup(Index: integer): TScKExNamedGroupType;
    function GetPrivateECKeys(Index: integer): TScKey;
    function GetPublicECPoints(Index: integer): TScCustomECPoint;
    function GetPrivateDHXs(Index: integer): TBigInteger;
    function GetPublicDHYs(Index: integer): TBigInteger;

  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetType: TTLSExtensionType; override;
    procedure Encode(Message: THandshakeMessage); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Parse(Message: THandshakeMessage); override;

    procedure Add(const ECName: TScECName); overload;
    procedure Add(const FFGType: TScFiniteFieldGroupType); overload;
    procedure Add(const KExNamedGroup: TScKExNamedGroupType); overload;
    procedure Clear;

    property OnlyRequest: boolean read FOnlyRequest write FOnlyRequest;
    property Count: integer read FCount;
    property KExNamedGroups[Index: integer]: TScKExNamedGroupType read GetKExNamedGroup;
    property PrivateECKeys[Index: integer]: TScKey read GetPrivateECKeys;
    property PublicECPoints[Index: integer]: TScCustomECPoint read GetPublicECPoints;
    property PrivateXs[Index: integer]: TBigInteger read GetPrivateDHXs;
    property PublicYs[Index: integer]: TBigInteger read GetPublicDHYs;
  end;

  TScPskKeyExchangeMode = (kemPskOnly, kemPskWithECDHE);
  TScPskKeyExchangeModes = set of TScPskKeyExchangeMode;

  // TLS 1.3
  TTLSPskKeyExchangeModesExtension = class(TTLSHelloExtension)
  private
    FModes: TScPskKeyExchangeModes;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetType: TTLSExtensionType; override;
    procedure Encode(Message: THandshakeMessage); override;
  public
    constructor Create; override;
    procedure Parse(Message: THandshakeMessage); override;

    property Modes: TScPskKeyExchangeModes read FModes write FModes;
  end;

  // TLS 1.3
  TTLSPreSharedKeyExtension = class(TTLSHelloExtension)
  private
    FNewSessionTicketList: TScNewSessionTicketList;
    FSelectedIdentity: integer;
    FHashAlgorithm: TScHashAlgorithm;

    function GetCount: integer;
    function GetNewSessionTicket(Index: integer): TScNewSessionTicket;
    function GetIdentity(Index: integer): TBytes;

  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetType: TTLSExtensionType; override;
    procedure Encode(Message: THandshakeMessage); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure Parse(Message: THandshakeMessage); override;

    procedure Add(NewSessionTicket: TScNewSessionTicket);
    procedure Clear;

    property SelectedIdentity: integer read FSelectedIdentity write FSelectedIdentity;
    property Count: integer read GetCount;
    property NewSessionTickets[Index: integer]: TScNewSessionTicket read GetNewSessionTicket;
    property Identities[Index: integer]: TBytes read GetIdentity;
    property HashAlgorithm: TScHashAlgorithm read FHashAlgorithm write FHashAlgorithm;
  end;

  // https://tools.ietf.org/html/rfc7685
  TTLSClientHelloPaddingExtension = class(TTLSHelloExtension)
  private
    FPadding: TBytes;

  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetType: TTLSExtensionType; override;
    procedure Encode(Message: THandshakeMessage); override;

  public
    procedure Parse(Message: THandshakeMessage); override;

    property Padding: TBytes read FPadding write FPadding;
  end;

  // TLS 1.3
  TTLSEarlyDataExtension = class(TTLSHelloExtension)
  private
    FMaxEarlyDataSize: cardinal;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetType: TTLSExtensionType; override;
    procedure Encode(Message: THandshakeMessage); override;
  public
    procedure Parse(Message: THandshakeMessage); override;

    property MaxEarlyDataSize: cardinal read FMaxEarlyDataSize write FMaxEarlyDataSize;
  end;

  // TLS 1.3
  TTLSSupportedVersionsExtension = class(TTLSHelloExtension)
  private
    FSelectedVersion: TScSSLProtocol;
    FVersions: TScSSLProtocols;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetType: TTLSExtensionType; override;
    procedure Encode(Message: THandshakeMessage); override;
  public
    procedure Parse(Message: THandshakeMessage); override;

    property SelectedVersion: TScSSLProtocol read FSelectedVersion write FSelectedVersion;
    property Versions: TScSSLProtocols read FVersions write FVersions;
  end;

  // TLS 1.3
  TTLSCookieExtension = class(TTLSHelloExtension)
  private
    FCookie: TBytes;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetType: TTLSExtensionType; override;
    procedure Encode(Message: THandshakeMessage); override;
  public
    procedure Parse(Message: THandshakeMessage); override;

    property Cookie: TBytes read FCookie write FCookie;
  end;

  // https://tools.ietf.org/html/rfc8446#section-4.2.6
  TTLSPostHandshakeAuthExtension = class(TTLSHelloExtension)
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetType: TTLSExtensionType; override;
    procedure Encode(Message: THandshakeMessage); override;
  public
    procedure Parse(Message: THandshakeMessage); override;
  end;

  TTLSHelloExtensions = class(TCRObjectList)
  private
    function GetExtension(Index: integer): TTLSHelloExtension;
    procedure SetExtension(Index: integer; Value: TTLSHelloExtension);

  public
    function Add(Item: TTLSHelloExtension): integer;
    procedure Assign(SrcList: TTLSHelloExtensions);
    procedure Encode(Message: THandshakeMessage; Entity: TScSSLConnectionEnd; Protocol: TScSSLProtocol = spTls12);
    procedure Parse(Message: THandshakeMessage; Entity: TScSSLConnectionEnd);
    function Find(AClass: TTLSHelloExtensionClass): TTLSHelloExtension;
    function FindOrCreate(AClass: TTLSHelloExtensionClass): TTLSHelloExtension;
    procedure RemoveIfExists(AClass: TTLSHelloExtensionClass);
    class function GetExtensionClass(ExtTypeCode: word): TTLSHelloExtensionClass;

    property Extensions[Index: integer]: TTLSHelloExtension read GetExtension write SetExtension; default;
  end;

const
  SCHEME_ALGORITHMS: array [TScSSLSignatureScheme] of TTLSSignatureAndHashAlgorithm = (
    (Hash: haSHA2_256; Signature: aaRSA; Padding: pmPKCS1; KeyPadding: pmNone), {ssRSA_PKCS1_SHA256}
    (Hash: haSHA2_384; Signature: aaRSA; Padding: pmPKCS1; KeyPadding: pmNone), {ssRSA_PKCS1_SHA384}
    (Hash: haSHA2_512; Signature: aaRSA; Padding: pmPKCS1; KeyPadding: pmNone), {ssRSA_PKCS1_SHA512}
    (Hash: haSHA2_256; Signature: aaEC; Padding: pmNone; KeyPadding: pmNone),   {ssECDSA_SECP256r1_SHA256}
    (Hash: haSHA2_384; Signature: aaEC; Padding: pmNone; KeyPadding: pmNone),   {ssECDSA_SECP384r1_SHA384}
    (Hash: haSHA2_512; Signature: aaEC; Padding: pmNone; KeyPadding: pmNone),   {ssECDSA_SECP521r1_SHA512}
    (Hash: haSHA2_256; Signature: aaRSA; Padding: pmPSS; KeyPadding: pmPKCS1),  {ssRSA_PSS_RSAE_SHA256}
    (Hash: haSHA2_384; Signature: aaRSA; Padding: pmPSS; KeyPadding: pmPKCS1),  {ssRSA_PSS_RSAE_SHA384}
    (Hash: haSHA2_512; Signature: aaRSA; Padding: pmPSS; KeyPadding: pmPKCS1),  {ssRSA_PSS_RSAE_SHA512}
    (Hash: haSHA2_256; Signature: aaRSA; Padding: pmPSS; KeyPadding: pmPSS),    {ssRSA_PSS_PSS_SHA256}
    (Hash: haSHA2_384; Signature: aaRSA; Padding: pmPSS; KeyPadding: pmPSS),    {ssRSA_PSS_PSS_SHA384}
    (Hash: haSHA2_512; Signature: aaRSA; Padding: pmPSS; KeyPadding: pmPSS),    {ssRSA_PSS_PSS_SHA512}
    (Hash: haNone; Signature: aaEC; Padding: pmNone; KeyPadding: pmNone),       {ssEd25519}
    (Hash: haSHA1; Signature: aaRSA; Padding: pmPKCS1; KeyPadding: pmNone),     {ssRSA_PKCS1_SHA1}
    (Hash: haSHA1; Signature: aaEC; Padding: pmNone; KeyPadding: pmNone)        {ssECDSA_SHA1}
  );

  EXTENSION_TYPE_CODES: array [TTLSExtensionType] of word = (
    $FFFF {etUnknown},
    $0000 {etServerName}, $0001 {etMaxFragmentLength}, $001C {etRecordSizeLimit},
    $0017 {etExtendedMasterSecret},
    $0023 {etSessionTicket}, $000D {etSignatureAlgorithms}, $0005 {etCertificateStatusRequest},
    $0012 {etSignedCertificateTimestamp}, $0010 {etApplicationLayerProtocolNegotiation},
    $7550 {etTLSChannelId},
    $000A {etSupportedGroups}, $000B {etEllipticCurvePointFormats},
    $FF01 {etRenegotiationIndication}, $0015 {etClientHelloPadding},
    $002B {etSupportedVersions},
    $002D {etPskKeyExchangeModes}, $0029 {etPreSharedKey}, $002A {etEarlyData},
    $002C {etCookie}, $0032 {etSignatureAlgorithmsCert}, $0033 {etKeyShare},
    $0031 {etPostHandshakeAuth}
  );

  EXTENSION_TYPE_CLASSES: array [TTLSExtensionType] of TTLSHelloExtensionClass = (
    TTLSUnknownExtension,
    TTLSServerNameExtension, TTLSMaxFragmentLengthExtension, TTLSRecordSizeLimitExtension,
    TTLSExtendedMasterSecretExtension,
    TTLSSessionTicketExtension, TTLSSignatureAlgorithmsExtension, TTLSCertificateStatusRequestExtension,
    TTLSSignedCertificateTimestampExtension, TTLSApplicationLayerProtocolNegotiationExtension,
    TTLSChannelIdExtension,
    TTLSSupportedGroupsExtension, TTLSEllipticCurvePointFormatsExtension,
    TTLSRenegotiationIndicationExtension, TTLSClientHelloPaddingExtension,
    TTLSSupportedVersionsExtension,
    TTLSPskKeyExchangeModesExtension, TTLSPreSharedKeyExtension, TTLSEarlyDataExtension,
    TTLSCookieExtension, TTLSSignatureAlgorithmsCertExtension, TTLSKeyShareExtension,
    TTLSPostHandshakeAuthExtension
  );

implementation

uses
{$IFNDEF SBRIDGE}
{$IFNDEF UNIDACPRO}
  TdsCipherSuites, TdsAlgorithmSupport;
{$ELSE}
  TdsCipherSuitesUni, TdsAlgorithmSupportUni;
{$ENDIF}
{$ELSE}
  ScCipherSuites, ScAlgorithmSupport;
{$ENDIF}

var
  KExNamedGroupType_COUNT: integer;
  SSLSignatureScheme_COUNT: integer;

{ TScSecurityOptions }

constructor TScSecurityOptions.Create;
begin
  inherited Create;

  FProtocols := [];
  FCompression := csNone;
  FEntity := ceClient;
  FOnCertificateRequest := nil;
  FOnRemoteCertificateValidation := nil;
  FOnObtainCRL := nil;
  FOnNeedVerifyCertificate := nil;
  FCACertificate := nil;
  FCAStorage := nil;

  FIgnoreRemoteCertificateConstraints := False;
  FIgnoreRemoteCertificateInsecurity := False;
  FIgnoreRemoteCertificateValidity := False;
  FTrustSelfSignedCertificate := False;
  FTrustRemoteCertificate := False;
  FCertificateListIsUnsorted := False;
  FDisableInsertEmptyFragment := False;
  FDisableCloseSocketOnShutdownAlert := False;
  FDisableCloseOnDestroy := False;
  FAsyncStartTLS := False;
  FMinDHEKeyLength := 1024;
  FUseClientInitiatedRenegotiation := False;
  FExtendedMasterSecretMode := emsmAllow;
  FClientInitiatedRenegotiationIsAllowed := False;
  FIsClientCertificateRequired := False;
  FRecordSizeLimit := 0;
  FNewSessionTicketLifetime := DEFAULT_NEW_SESSION_TICKET_LIFETIME;

  FSupportedKExNamedGroups := [
    kex_x25519,
    kex_secp256r1, kex_secp256k1, kex_secp384r1, kex_secp521r1,
    kex_sect283r1, kex_sect283k1, kex_sect409r1, kex_sect409k1,
    kex_sect571r1, kex_sect571k1,
    kex_ffDHE2048, kex_ffDHE3072, kex_ffDHE4096,
    kex_ffDHE6144, kex_ffDHE8192
  ];
end;

procedure TScSecurityOptions.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScSecurityOptions) then begin
    TScSecurityOptions(Dest).CipherAlgorithms := CipherAlgorithms;
    TScSecurityOptions(Dest).FCompression := FCompression;
    TScSecurityOptions(Dest).FProtocols := FProtocols;
    TScSecurityOptions(Dest).FEntity := FEntity;
    TScSecurityOptions(Dest).FOnCertificateRequest := FOnCertificateRequest;
    TScSecurityOptions(Dest).FOnRemoteCertificateValidation := FOnRemoteCertificateValidation;
    TScSecurityOptions(Dest).FOnObtainCRL := FOnObtainCRL;
    TScSecurityOptions(Dest).FOnNeedVerifyCertificate := FOnNeedVerifyCertificate;
    TScSecurityOptions(Dest).FCACertificate := FCACertificate;
    TScSecurityOptions(Dest).FCAStorage := FCAStorage;
    TScSecurityOptions(Dest).FIgnoreRemoteCertificateConstraints := FIgnoreRemoteCertificateConstraints;
    TScSecurityOptions(Dest).FIgnoreRemoteCertificateInsecurity := FIgnoreRemoteCertificateInsecurity;
    TScSecurityOptions(Dest).FIgnoreRemoteCertificateValidity := FIgnoreRemoteCertificateValidity;
    TScSecurityOptions(Dest).FTrustSelfSignedCertificate := FTrustSelfSignedCertificate;
    TScSecurityOptions(Dest).FTrustRemoteCertificate := FTrustRemoteCertificate;
    TScSecurityOptions(Dest).FIdentityDNSName := FIdentityDNSName;
    TScSecurityOptions(Dest).FCertificateListIsUnsorted := FCertificateListIsUnsorted;
    TScSecurityOptions(Dest).FDisableInsertEmptyFragment := FDisableInsertEmptyFragment;
    TScSecurityOptions(Dest).FDisableCloseSocketOnShutdownAlert := FDisableCloseSocketOnShutdownAlert;
    TScSecurityOptions(Dest).FDisableCloseOnDestroy := FDisableCloseOnDestroy;
    TScSecurityOptions(Dest).FAsyncStartTLS := FAsyncStartTLS;
    TScSecurityOptions(Dest).FMinDHEKeyLength := FMinDHEKeyLength;
    TScSecurityOptions(Dest).FUseClientInitiatedRenegotiation := FUseClientInitiatedRenegotiation;
    TScSecurityOptions(Dest).FTimeout := FTimeout;
    TScSecurityOptions(Dest).FExtendedMasterSecretMode := FExtendedMasterSecretMode;
    TScSecurityOptions(Dest).FClientHelloExtensions := FClientHelloExtensions;
    TScSecurityOptions(Dest).FServerHelloExtensions := FServerHelloExtensions;
    TScSecurityOptions(Dest).FAssignedSessionInfo := FAssignedSessionInfo;
    TScSecurityOptions(Dest).FClientInitiatedRenegotiationIsAllowed := FClientInitiatedRenegotiationIsAllowed;
    TScSecurityOptions(Dest).FRecordSizeLimit := FRecordSizeLimit;
    TScSecurityOptions(Dest).FRequestDNList := FRequestDNList;
    TScSecurityOptions(Dest).FIsClientCertificateRequired := FIsClientCertificateRequired;
    TScSecurityOptions(Dest).FNewSessionTicketLifetime := FNewSessionTicketLifetime;
    TScSecurityOptions(Dest).FNewSessionTicketDistributedCount := FNewSessionTicketDistributedCount;
    TScSecurityOptions(Dest).FSupportedKExNamedGroups := FSupportedKExNamedGroups;
    TScSecurityOptions(Dest).FAfterClientHello := FAfterClientHello;
    TScSecurityOptions(Dest).FOnCreateNewSessionTicket := FOnCreateNewSessionTicket;
    TScSecurityOptions(Dest).FOnDecodeTicket := FOnDecodeTicket;
    TScSecurityOptions(Dest).FOnGetTicketNameAndPassword := FOnGetTicketNameAndPassword;
    TScSecurityOptions(Dest).FOnGetPasswordByTicketName := FOnGetPasswordByTicketName;
  end
  else
    inherited;
end;

procedure TScSecurityOptions.SetCipherAlgorithms(const Value: TScSSLCipherAlgorithmArray);
begin
  SetLength(FCipherAlgorithms, Length(Value));
  if Length(Value) > 0 then
    Move(Value[0], FCipherAlgorithms[0], Length(Value) * sizeof(TScSSLCipherAlgorithm));
end;

{ TScNewSessionTicketExt }

constructor TScNewSessionTicketExt.Create;
begin
  inherited;
  FExtensions := TTLSHelloExtensions.Create;
end;

destructor TScNewSessionTicketExt.Destroy;
begin
  FExtensions.Free;
  inherited;
end;

procedure TScNewSessionTicketExt.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScNewSessionTicketExt) then
    TScNewSessionTicketExt(Dest).FExtensions.Assign(FExtensions);

  inherited;
end;

{ TTLSHelloExtensions }

function TTLSHelloExtensions.GetExtension(Index: integer): TTLSHelloExtension;
begin
  Result := TObject(inherited Items[Index]) as TTLSHelloExtension;
end;

procedure TTLSHelloExtensions.SetExtension(Index: integer; Value: TTLSHelloExtension);
begin
  inherited Items[Index] := Value;
end;

function TTLSHelloExtensions.Add(Item: TTLSHelloExtension): integer;
var
  Ext: TTLSHelloExtension;
  i: integer;
begin
  for i := 0 to Count - 1 do begin
    Ext := TObject(Items[i]) as TTLSHelloExtension;

    if Item.GetType = etUnknown then begin
      if Item.GetTypeCode = Ext.GetTypeCode then
        raise EScError.CreateFmt(SExtensionClassDuplicated, [Item.ClassName], seExtensionClassDuplicated);
    end
    else
      if Item.GetType = Ext.GetType then
        raise EScError.CreateFmt(SExtensionClassDuplicated, [Item.ClassName], seExtensionClassDuplicated);
  end;

  Result := inherited Add(Item);
end;

procedure TTLSHelloExtensions.Assign(SrcList: TTLSHelloExtensions);
var
  SrcHelloExtension, DstHelloExtension: TTLSHelloExtension;
  i: integer;
begin
  Clear;

  for i := 0 to SrcList.Count - 1 do begin
    SrcHelloExtension := TObject(SrcList.Items[i]) as TTLSHelloExtension;
    DstHelloExtension := TTLSHelloExtensionClass(SrcHelloExtension.ClassType).Create;
    DstHelloExtension.Assign(SrcHelloExtension);
    Add(DstHelloExtension);
  end;
end;

procedure TTLSHelloExtensions.Encode(Message: THandshakeMessage;
  Entity: TScSSLConnectionEnd; Protocol: TScSSLProtocol = spTls12);
const
  SERVER_HELLO_EXT_TYPES = [etSupportedVersions, etKeyShare, etPreSharedKey, etCookie];
var
  Extension: TTLSHelloExtension;
  TypeCode: word;
  LenOffset, ExtLenOffset: integer;
  i: integer;
begin
  if Count = 0 then
    Exit;

  LenOffset := Message.WriteOffset;
  Message.SkipWrite(2); // Len

  for i := 0 to Count - 1 do begin
    Extension := TObject(Items[i]) as TTLSHelloExtension;
    Extension.FEntity := Entity;

    if Protocol = spTls13 then begin
      if Message.HandshakeType = htServerHello then begin
        if not (Extension.GetType in SERVER_HELLO_EXT_TYPES) then
          continue;
      end
      else
      if Message.HandshakeType = htEncryptedExtensions then begin
        if Extension.GetType in SERVER_HELLO_EXT_TYPES then
          continue;
      end;
    end;

    TypeCode := Extension.GetTypeCode;
    Message.WriteInt16(TypeCode);

    ExtLenOffset := Message.WriteOffset;
    Message.SkipWrite(2); // Len

    Extension.Encode(Message);
    Message.WriteInt16ByOffset(Message.WriteOffset - ExtLenOffset - 2, ExtLenOffset);
  end;

  Message.WriteInt16ByOffset(Message.WriteOffset - LenOffset - 2, LenOffset);
end;

procedure TTLSHelloExtensions.Parse(Message: THandshakeMessage; Entity: TScSSLConnectionEnd);
var
  ExtsLen, ExtLen, EndOffset: integer;
  ExtTypeCode: word;
  ExtensionClass: TTLSHelloExtensionClass;
  Extension: TTLSHelloExtension;
begin
  if Message.RestCount = 0 then
    Exit;

  ExtsLen := Message.ReadInt16;
  while ExtsLen > 0 do begin
    ExtTypeCode := Message.ReadInt16;
    ExtLen := Message.ReadInt16;
    Dec(ExtsLen, ExtLen + 4);

    ExtensionClass := GetExtensionClass(ExtTypeCode);
    if ExtensionClass = nil then
      ExtensionClass := TTLSUnknownExtension;

    Extension := ExtensionClass.Create;
    try
      Extension.FEntity := Entity;

      EndOffset := Message.WriteOffset;
      Message.WriteOffset := Message.ReadOffset + ExtLen;
      try
        Extension.Parse(Message);
      finally
        Message.WriteOffset := EndOffset;
      end;

      if ExtensionClass = TTLSUnknownExtension then
        TTLSUnknownExtension(Extension).FTypeCode := ExtTypeCode;
      Add(Extension);
    except
      Extension.Free;
      raise;
    end;
  end;
end;

function TTLSHelloExtensions.Find(AClass: TTLSHelloExtensionClass): TTLSHelloExtension;
var
  i: integer;
begin
  for i := 0 to Count - 1 do begin
    Result := TObject(Items[i]) as TTLSHelloExtension;
    if Result is AClass then
      Exit;
  end;

  Result := nil;
end;

function TTLSHelloExtensions.FindOrCreate(AClass: TTLSHelloExtensionClass): TTLSHelloExtension;
begin
  Result := Find(AClass);
  if Result = nil then begin
    Result := AClass.Create;
    Add(Result);
  end;
end;

procedure TTLSHelloExtensions.RemoveIfExists(AClass: TTLSHelloExtensionClass);
var
  HelloExtension: TTLSHelloExtension;
  i: integer;
begin
  for i := 0 to Count - 1 do begin
    HelloExtension := TObject(Items[i]) as TTLSHelloExtension;
    if HelloExtension is AClass then begin
      Delete(i);
      Exit;
    end;
  end;
end;

class function TTLSHelloExtensions.GetExtensionClass(ExtTypeCode: word): TTLSHelloExtensionClass;
var
  ExtType: TTLSExtensionType;
begin
  for ExtType := Low(TTLSExtensionType) to High(TTLSExtensionType) do
    if EXTENSION_TYPE_CODES[ExtType] = ExtTypeCode then begin
      Result := EXTENSION_TYPE_CLASSES[ExtType];
      Exit;
    end;

  Result := nil;
end;

{ TTLSHelloExtension }

constructor TTLSHelloExtension.Create;
begin
  inherited;
end;

procedure TTLSHelloExtension.AssignTo(Dest: TPersistent);
begin
  inherited;
end;

function TTLSHelloExtension.GetTypeCode: word;
begin
  Result := EXTENSION_TYPE_CODES[GetType];
end;

{ TTLSUnknownExtension }

procedure TTLSUnknownExtension.AssignTo(Dest: TPersistent);
begin
  if Dest is TTLSUnknownExtension then begin
    SetLength(TTLSUnknownExtension(Dest).FData, Length(FData));
    if Length(FData) > 0 then
      Move(FData[0], TTLSUnknownExtension(Dest).FData[0], Length(FData));
  end
  else
    inherited;
end;

function TTLSUnknownExtension.GetTypeCode: word;
begin
  Result := FTypeCode;
end;

function TTLSUnknownExtension.GetType: TTLSExtensionType;
begin
  Result := etUnknown;
end;

procedure TTLSUnknownExtension.Encode(Message: THandshakeMessage);
begin
  Message.WriteBuf0(FData);
end;

procedure TTLSUnknownExtension.Parse(Message: THandshakeMessage);
begin
  FData := Message.Read(Message.RestCount);
end;

{ TTLSRenegotiationIndicationExtension }

constructor TTLSRenegotiationIndicationExtension.Create;
begin
  inherited;
  FIsRenegotiation := False;
  FIsRemoteSupport := False;
end;

procedure TTLSRenegotiationIndicationExtension.AssignTo(Dest: TPersistent);
begin
  if Dest is TTLSRenegotiationIndicationExtension then begin
    TTLSRenegotiationIndicationExtension(Dest).FIsRenegotiation := FIsRenegotiation;
    TTLSRenegotiationIndicationExtension(Dest).FLocalClientFinishedMessage := FLocalClientFinishedMessage;
    TTLSRenegotiationIndicationExtension(Dest).FLocalServerFinishedMessage := FLocalServerFinishedMessage;
    TTLSRenegotiationIndicationExtension(Dest).FRemoteClientFinishedMessage := FRemoteClientFinishedMessage;
    TTLSRenegotiationIndicationExtension(Dest).FRemoteServerFinishedMessage := FRemoteServerFinishedMessage;
    TTLSRenegotiationIndicationExtension(Dest).FIsRemoteSupport := FIsRemoteSupport;
  end
  else
    inherited;
end;

function TTLSRenegotiationIndicationExtension.GetType: TTLSExtensionType;
begin
  Result := etRenegotiationIndication;
end;

procedure TTLSRenegotiationIndicationExtension.Encode(Message: THandshakeMessage);
begin
  if not FIsRenegotiation then
    Message.WriteInt8(0) // Len
  else begin
    if FEntity = ceClient then
      Message.WriteBuf8(FLocalClientFinishedMessage)
    else begin
      Message.WriteInt8(Length(FLocalClientFinishedMessage) * 2);
      Message.WriteBuf0(FLocalClientFinishedMessage);
      Message.WriteBuf0(FLocalServerFinishedMessage);
    end;
  end;
end;

procedure TTLSRenegotiationIndicationExtension.Parse(Message: THandshakeMessage);
var
  HalfLen, Len: integer;
begin
  if FEntity = ceClient then begin
    HalfLen := Message.ReadInt8 div 2;
    FRemoteClientFinishedMessage := Message.Read(HalfLen);
    FRemoteServerFinishedMessage := Message.Read(HalfLen);
  end
  else begin
    Len := Message.ReadInt8;
    FRemoteClientFinishedMessage := Message.Read(Len);
  end;
end;

procedure TTLSRenegotiationIndicationExtension.Clear;
begin
  SetLength(FLocalClientFinishedMessage, 0);
  SetLength(FLocalServerFinishedMessage, 0);
  SetLength(FRemoteClientFinishedMessage, 0);
  SetLength(FRemoteServerFinishedMessage, 0);
end;

procedure TTLSRenegotiationIndicationExtension.Renegotiate;
begin
  FIsRenegotiation := True;
end;

procedure TTLSRenegotiationIndicationExtension.Check(ReceivedExtension: TTLSRenegotiationIndicationExtension);
begin
  FRemoteClientFinishedMessage := ReceivedExtension.FRemoteClientFinishedMessage;
  FRemoteServerFinishedMessage := ReceivedExtension.FRemoteServerFinishedMessage;

  if not FIsRenegotiation then begin
    if Length(FRemoteClientFinishedMessage) > 0 then
      raise EScError.Create(seInvalidMessage);

    FIsRemoteSupport := True;
  end
  else begin
    if not FIsRemoteSupport then
      raise EScError.Create(seRenegotiationDenied);

    if Length(FRemoteClientFinishedMessage) = 0 then
      raise EScError.Create(seInvalidMessage);

    if Length(FLocalClientFinishedMessage) <> Length(FRemoteClientFinishedMessage) then
      raise EScError.Create(seInvalidRenegotiationInfo);

    if Length(FLocalClientFinishedMessage) <> 0 then
      if MemCompare(@FLocalClientFinishedMessage[0], @FRemoteClientFinishedMessage[0], Length(FLocalClientFinishedMessage)) <> 0 then
        raise EScError.Create(seInvalidRenegotiationInfo);

    if FEntity = ceClient then begin
      if Length(FLocalServerFinishedMessage) <> Length(FRemoteServerFinishedMessage) then
        raise EScError.Create(seInvalidRenegotiationInfo);

      if Length(FLocalServerFinishedMessage) <> 0 then
        if MemCompare(@FLocalServerFinishedMessage[0], @FRemoteServerFinishedMessage[0], Length(FLocalServerFinishedMessage)) <> 0 then
          raise EScError.Create(seInvalidRenegotiationInfo);
    end;
  end;
end;

procedure TTLSRenegotiationIndicationExtension.SetClientFinishedMessage(const Value: TBytes; Offset, Size: integer);
begin
  SetLength(FLocalClientFinishedMessage, Size);
  if Size > 0 then
    Move(Value[Offset], FLocalClientFinishedMessage[0], Size);

  FRemoteClientFinishedMessage := nil;
end;

procedure TTLSRenegotiationIndicationExtension.SetServerFinishedMessage(const Value: TBytes; Offset, Size: integer);
begin
  SetLength(FLocalServerFinishedMessage, Size);
  if Size > 0 then
    Move(Value[Offset], FLocalServerFinishedMessage[0], Size);

  FRemoteServerFinishedMessage := nil;
end;

{ TTLSServerNameExtension }

constructor TTLSServerNameExtension.Create;
begin
  inherited;
  FServerNames := TStringList.Create;
end;

destructor TTLSServerNameExtension.Destroy;
begin
  FServerNames.Free;
  inherited;
end;

procedure TTLSServerNameExtension.AssignTo(Dest: TPersistent);
begin
  if Dest is TTLSServerNameExtension then begin
    TTLSServerNameExtension(Dest).FServerNames.Assign(FServerNames);
  end
  else
    inherited;
end;

function TTLSServerNameExtension.GetType: TTLSExtensionType;
begin
  Result := etServerName;
end;

procedure TTLSServerNameExtension.Encode(Message: THandshakeMessage);
var
  StrBuf: TBytes;
  LenOffset: integer;
  i: integer;
begin
  if FEntity = ceClient then begin
    SetLength(StrBuf, 0);
    if FServerNames.Count = 0 then
      FServerNames.Add('');

    LenOffset := Message.WriteOffset;
    Message.SkipWrite(2); // Len

    for i := 0 to FServerNames.Count - 1 do begin
      StrBuf := Encoding.UTF8.GetBytes(FServerNames[i]);
      Message.WriteInt8(0); // NameType = host_name
      Message.WriteBuf16(StrBuf);
    end;

    Message.WriteInt16ByOffset(Message.WriteOffset - LenOffset - 2, LenOffset);
  end;
end;

procedure TTLSServerNameExtension.Parse(Message: THandshakeMessage);
var
  RecLen, StrLen: integer;
begin
  FServerNames.Clear;
  if Message.RestCount = 0 then
    Exit;

  RecLen := Message.ReadInt16;
  while RecLen > 0 do begin
    if Message.ReadInt8 <> 0 then // NameType = host_name
      raise EScError.Create(seWrongExtensionData);

    StrLen := Message.ReadInt16;
    FServerNames.Add(Encoding.UTF8.GetString(Message.Read(StrLen)));
    Dec(RecLen, StrLen + 3);
  end;
end;

{ TTLSMaxFragmentLengthExtension }

procedure TTLSMaxFragmentLengthExtension.AssignTo(Dest: TPersistent);
begin
  if Dest is TTLSMaxFragmentLengthExtension then begin
    TTLSMaxFragmentLengthExtension(Dest).FMaxFragmentLength := FMaxFragmentLength;
  end
  else
    inherited;
end;

function TTLSMaxFragmentLengthExtension.GetType: TTLSExtensionType;
begin
  Result := etMaxFragmentLength;
end;

function TTLSMaxFragmentLengthExtension.GetMaxFragmentLengthInByte: integer;
begin
  case FMaxFragmentLength of
    mfl_2_9:
      Result := 1 shl 9;
    mfl_2_10:
      Result := 1 shl 10;
    mfl_2_11:
      Result := 1 shl 11;
    mfl_2_12:
      Result := 1 shl 12;
  else
    Result := 1 shl 14;
  end;
end;

procedure TTLSMaxFragmentLengthExtension.Encode(Message: THandshakeMessage);
begin
  case FMaxFragmentLength of
    mfl_2_9:
      Message.WriteInt8(1);
    mfl_2_10:
      Message.WriteInt8(2);
    mfl_2_11:
      Message.WriteInt8(3);
    mfl_2_12:
      Message.WriteInt8(4);
  else
    raise EScError.Create(seInvalidInputArgs);
  end;
end;

procedure TTLSMaxFragmentLengthExtension.Parse(Message: THandshakeMessage);
begin
  case Message.ReadInt8 of
    1:
      FMaxFragmentLength := mfl_2_9;
    2:
      FMaxFragmentLength := mfl_2_10;
    3:
      FMaxFragmentLength := mfl_2_11;
    4:
      FMaxFragmentLength := mfl_2_12;
  else
    raise EScError.Create(seWrongExtensionData);
  end;
end;

{ TTLSRecordSizeLimitExtension }

constructor TTLSRecordSizeLimitExtension.Create;
begin
  inherited;

  FRecordSizeLimit := MAX_RECORD_LENGTH;
end;

procedure TTLSRecordSizeLimitExtension.AssignTo(Dest: TPersistent);
begin
  if Dest is TTLSRecordSizeLimitExtension then begin
    TTLSRecordSizeLimitExtension(Dest).FRecordSizeLimit := FRecordSizeLimit;
  end
  else
    inherited;
end;

function TTLSRecordSizeLimitExtension.GetType: TTLSExtensionType;
begin
  Result := etRecordSizeLimit;
end;

procedure TTLSRecordSizeLimitExtension.SetRecordSizeLimit(Value: word);
begin
  if (Value < 64) or (Value > MAX_RECORD_LENGTH) then
    raise EScError.Create(seWrongExtensionData);

  FRecordSizeLimit := Value;
end;

procedure TTLSRecordSizeLimitExtension.Encode(Message: THandshakeMessage);
begin
  Message.WriteInt16(FRecordSizeLimit);
end;

procedure TTLSRecordSizeLimitExtension.Parse(Message: THandshakeMessage);
var
  Value: word;
begin
  FRecordSizeLimit := 0;

  Value := Message.ReadInt16;
  SetRecordSizeLimit(Value);
end;

{ TTLSExtendedMasterSecretExtension }

procedure TTLSExtendedMasterSecretExtension.AssignTo(Dest: TPersistent);
begin
  if Dest is TTLSExtendedMasterSecretExtension then begin
    // none
  end
  else
    inherited;
end;

function TTLSExtendedMasterSecretExtension.GetType: TTLSExtensionType;
begin
  Result := etExtendedMasterSecret;
end;

procedure TTLSExtendedMasterSecretExtension.Encode(Message: THandshakeMessage);
begin
  // none
end;

procedure TTLSExtendedMasterSecretExtension.Parse(Message: THandshakeMessage);
begin
  if Message.RestCount <> 0 then
    raise EScError.Create(seWrongExtensionData);
end;

{ TTLSSessionTicketExtension }

procedure TTLSSessionTicketExtension.AssignTo(Dest: TPersistent);
begin
  if Dest is TTLSSessionTicketExtension then begin
    SetLength(TTLSSessionTicketExtension(Dest).FTicket, Length(FTicket));
    if Length(FTicket) > 0 then
      Move(FTicket[0], TTLSSessionTicketExtension(Dest).FTicket[0], Length(FTicket));
  end
  else
    inherited;
end;

function TTLSSessionTicketExtension.GetType: TTLSExtensionType;
begin
  Result := etSessionTicket;
end;

procedure TTLSSessionTicketExtension.Encode(Message: THandshakeMessage);
begin
  Message.WriteBuf0(FTicket);
end;

procedure TTLSSessionTicketExtension.Parse(Message: THandshakeMessage);
begin
  FTicket := Message.Read(Message.RestCount);
end;

{ TTLSSignatureAlgorithmsExtension }

constructor TTLSSignatureAlgorithmsExtension.Create;
begin
  inherited;

  SetLength(FSignatureSchemes, SSLSignatureScheme_COUNT);
  FCount := 0;
end;

procedure TTLSSignatureAlgorithmsExtension.AssignTo(Dest: TPersistent);
begin
  if Dest is TTLSSignatureAlgorithmsExtension then begin
    TTLSSignatureAlgorithmsExtension(Dest).FCount := FCount;
    Move(FSignatureSchemes[0], TTLSSignatureAlgorithmsExtension(Dest).FSignatureSchemes[0], SSLSignatureScheme_COUNT * sizeof(TScSSLSignatureScheme));
  end
  else
    inherited;
end;

function TTLSSignatureAlgorithmsExtension.GetType: TTLSExtensionType;
begin
  Result := etSignatureAlgorithms;
end;

procedure TTLSSignatureAlgorithmsExtension.Add(Hash: TScHashAlgorithm; Signature: TScSSLSignatureAlgorithm);
var
  ss: TScSSLSignatureScheme;
begin
  for ss := Low(TScSSLSignatureScheme) to High(TScSSLSignatureScheme) do
    if (Hash = SCHEME_ALGORITHMS[ss].Hash) and (SCHEME_ALGORITHMS[ss].Signature = Signature) then begin
      Add(ss);
      Exit;
    end;

  raise EScError.Create(seInvalidSignatureSchemeAlgorithm);
end;

procedure TTLSSignatureAlgorithmsExtension.Add(SignatureScheme: TScSSLSignatureScheme);
var
  i: integer;
begin
  for i := 0 to FCount - 1 do begin
    if FSignatureSchemes[i] = SignatureScheme then
      raise EScError.Create(seDuplicateKExNamedGroup);
  end;

  Assert(FCount < Length(FSignatureSchemes));
  FSignatureSchemes[FCount] := SignatureScheme;
  Inc(FCount);
end;

procedure TTLSSignatureAlgorithmsExtension.Clear;
begin
  FCount := 0;
end;

function TTLSSignatureAlgorithmsExtension.GetSignatureScheme(Index: integer): TScSSLSignatureScheme;
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt(SListIndexError, [Index]);

  Result := FSignatureSchemes[Index];
end;

procedure TTLSSignatureAlgorithmsExtension.Encode(Message: THandshakeMessage);
var
  i: integer;
begin
  Message.WriteInt16(FCount * 2); // Len

  for i := 0 to FCount - 1 do
    Message.WriteInt16(SIGNATURE_SCHEME_CODES[FSignatureSchemes[i]]);
end;

procedure TTLSSignatureAlgorithmsExtension.Parse(Message: THandshakeMessage);
var
  RecLen: integer;
  SS: TScSSLSignatureScheme;
  SSVal: word;
  i: integer;
begin
  FCount := 0;
  if Message.RestCount = 0 then
    Exit;

  RecLen := Message.ReadInt16 div 2;
  for i := 0 to RecLen - 1 do begin
    SSVal := Message.ReadInt16;

    for SS := Low(TScSSLSignatureScheme) to High(TScSSLSignatureScheme) do begin
      if SSVal = SIGNATURE_SCHEME_CODES[ss] then begin
        FSignatureSchemes[FCount] := SS;
        Inc(FCount);
        if FCount = Length(FSignatureSchemes) then
          Exit;

        break;
      end;
    end;
  end;
end;

class function TTLSSignatureAlgorithmsExtension.CreateDefault: TTLSSignatureAlgorithmsExtension;
begin
  Result := TTLSSignatureAlgorithmsExtension.Create;
  Result.FSignatureSchemes[0] := ssRSA_PKCS1_SHA256;
  Result.FSignatureSchemes[1] := ssRSA_PKCS1_SHA384;
  Result.FSignatureSchemes[2] := ssRSA_PKCS1_SHA512;
  Result.FSignatureSchemes[3] := ssECDSA_SECP256r1_SHA256;
  Result.FSignatureSchemes[4] := ssECDSA_SECP384r1_SHA384;
  Result.FSignatureSchemes[5] := ssECDSA_SECP521r1_SHA512;
  Result.FSignatureSchemes[6] := ssRSA_PSS_RSAE_SHA256;
  Result.FSignatureSchemes[7] := ssRSA_PSS_RSAE_SHA384;
  Result.FSignatureSchemes[8] := ssRSA_PSS_RSAE_SHA512;
  Result.FSignatureSchemes[9] := ssRSA_PSS_PSS_SHA256;
  Result.FSignatureSchemes[10] := ssRSA_PSS_PSS_SHA384;
  Result.FSignatureSchemes[11] := ssRSA_PSS_PSS_SHA512;
  Result.FSignatureSchemes[12] := ssRSA_PKCS1_SHA1;
  Result.FSignatureSchemes[13] := ssECDSA_SHA1;
  Result.FSignatureSchemes[14] := ssEd25519;
  Result.FCount := 15;
end;

{ TTLSSignatureAlgorithmsCertExtension }

function TTLSSignatureAlgorithmsCertExtension.GetType: TTLSExtensionType;
begin
  Result := etSignatureAlgorithmsCert;
end;

{ TTLSCertificateStatusRequestExtension }

procedure TTLSCertificateStatusRequestExtension.AssignTo(Dest: TPersistent);
begin
  if Dest is TTLSCertificateStatusRequestExtension then begin
    // none
  end
  else
    inherited;
end;

function TTLSCertificateStatusRequestExtension.GetType: TTLSExtensionType;
begin
  Result := etCertificateStatusRequest;
end;

procedure TTLSCertificateStatusRequestExtension.Encode(Message: THandshakeMessage);
begin
  Message.WriteInt8(1); // OCSPStatusRequest = ocsp
  Message.WriteInt32(0);
end;

procedure TTLSCertificateStatusRequestExtension.Parse(Message: THandshakeMessage);
begin

end;

{ TTLSSignedCertificateTimestampExtension }

procedure TTLSSignedCertificateTimestampExtension.AssignTo(Dest: TPersistent);
begin
  if Dest is TTLSSignedCertificateTimestampExtension then begin
    // none
  end
  else
    inherited;
end;

function TTLSSignedCertificateTimestampExtension.GetType: TTLSExtensionType;
begin
  Result := etSignedCertificateTimestamp;
end;

procedure TTLSSignedCertificateTimestampExtension.Encode(Message: THandshakeMessage);
begin

end;

procedure TTLSSignedCertificateTimestampExtension.Parse(Message: THandshakeMessage);
begin

end;

{ TTLSApplicationLayerProtocolNegotiationExtension }

constructor TTLSApplicationLayerProtocolNegotiationExtension.Create;
begin
  inherited;
  FProtocolNames := TStringList.Create;
end;

destructor TTLSApplicationLayerProtocolNegotiationExtension.Destroy;
begin
  FProtocolNames.Free;
  inherited;
end;

procedure TTLSApplicationLayerProtocolNegotiationExtension.AssignTo(Dest: TPersistent);
begin
  if Dest is TTLSApplicationLayerProtocolNegotiationExtension then begin
    TTLSApplicationLayerProtocolNegotiationExtension(Dest).FProtocolNames.Assign(FProtocolNames);
  end
  else
    inherited;
end;

function TTLSApplicationLayerProtocolNegotiationExtension.GetType: TTLSExtensionType;
begin
  Result := etApplicationLayerProtocolNegotiation;
end;

procedure TTLSApplicationLayerProtocolNegotiationExtension.Encode(Message: THandshakeMessage);
var
  StrBuf: TBytes;
  LenOffset: integer;
  i: integer;
begin
  SetLength(StrBuf, 0);
  LenOffset := Message.WriteOffset;
  Message.SkipWrite(2); // Len

  for i := 0 to FProtocolNames.Count - 1 do begin
    StrBuf := Encoding.ASCII.GetBytes(FProtocolNames[i]);
    Message.WriteBuf8(StrBuf);
  end;

  Message.WriteInt16ByOffset(Message.WriteOffset - LenOffset - 2, LenOffset);
end;

procedure TTLSApplicationLayerProtocolNegotiationExtension.Parse(Message: THandshakeMessage);
var
  RecLen, StrLen: integer;
begin
  FProtocolNames.Clear;
  if Message.RestCount = 0 then
    Exit;

  RecLen := Message.ReadInt16;
  while RecLen > 0 do begin
    StrLen := Message.ReadInt8;
    FProtocolNames.Add(Encoding.Default.GetString(Message.Read(StrLen)));
    Dec(RecLen, StrLen + 1);
  end;
end;

{ TTLSChannelIdExtension }

procedure TTLSChannelIdExtension.AssignTo(Dest: TPersistent);
begin
  if Dest is TTLSChannelIdExtension then begin
    // none
  end
  else
    inherited;
end;

function TTLSChannelIdExtension.GetType: TTLSExtensionType;
begin
  Result := etTLSChannelId;
end;

procedure TTLSChannelIdExtension.Encode(Message: THandshakeMessage);
begin

end;

procedure TTLSChannelIdExtension.Parse(Message: THandshakeMessage);
begin

end;

{ TTLSEllipticCurvePointFormatsExtension }

constructor TTLSEllipticCurvePointFormatsExtension.Create;
begin
  inherited;
  FFormats := [ecpfUncompressed];
end;

procedure TTLSEllipticCurvePointFormatsExtension.AssignTo(Dest: TPersistent);
begin
  if Dest is TTLSEllipticCurvePointFormatsExtension then begin
    TTLSEllipticCurvePointFormatsExtension(Dest).FFormats := FFormats;
  end
  else
    inherited;
end;

function TTLSEllipticCurvePointFormatsExtension.GetType: TTLSExtensionType;
begin
  Result := etEllipticCurvePointFormats;
end;

procedure TTLSEllipticCurvePointFormatsExtension.Encode(Message: THandshakeMessage);
const
  POINT_FORMAT_CODES: array[TScECPointFormat] of byte =
    (0, 1, 2);
begin
  if FFormats = [] then
    FFormats := [ecpfUncompressed];

  Message.WriteInt8(1); // Len
  Message.WriteInt8(POINT_FORMAT_CODES[ecpfUncompressed]);
end;

procedure TTLSEllipticCurvePointFormatsExtension.Parse(Message: THandshakeMessage);
var
  RecLen: integer;
  i: integer;
begin
  FFormats := [];
  if Message.RestCount = 0 then
    Exit;

  RecLen := Message.ReadInt8;
  for i := 1 to RecLen do begin
    case Message.ReadInt8 of
      0:
        Include(FFormats, ecpfUncompressed);
      1:
        Include(FFormats, ecpfAnsiX962CompressedPrime);
      2:
        Include(FFormats, ecpfAnsiX962CompressedChar2);
    end;
  end;
end;

{ TTLSSupportedGroupsExtension }

constructor TTLSSupportedGroupsExtension.Create;
begin
  inherited;

  SetLength(FKExNamedGroupList, KExNamedGroupType_COUNT);
  FCount := 0;
end;

procedure TTLSSupportedGroupsExtension.AssignTo(Dest: TPersistent);
begin
  if Dest is TTLSSupportedGroupsExtension then begin
    TTLSSupportedGroupsExtension(Dest).FCount := FCount;
    Move(FKExNamedGroupList[0], TTLSSupportedGroupsExtension(Dest).FKExNamedGroupList[0], KExNamedGroupType_COUNT * sizeof(TScKExNamedGroupType));
  end
  else
    inherited;
end;

function TTLSSupportedGroupsExtension.GetType: TTLSExtensionType;
begin
  Result := etSupportedGroups;
end;

procedure TTLSSupportedGroupsExtension.Add(const ECName: TScECName);
var
  i: integer;
begin
  for i := 0 to FCount - 1 do begin
    if FKExNamedGroupList[i] = TScKExNamedGroupType(byte(ECName)) then
      raise EScError.Create(seDuplicateKExNamedGroup);
  end;

  Assert(FCount < Length(FKExNamedGroupList));
  FKExNamedGroupList[FCount] := TScKExNamedGroupType(byte(ECName));
  Inc(FCount);
end;

procedure TTLSSupportedGroupsExtension.Add(const FFGType: TScFiniteFieldGroupType);
var
  KExNamedGroup: TScKExNamedGroupType;
  i: integer;
begin
  KExNamedGroup := TScKExNamedGroupType(integer(FFGType) + integer(High(TScECName)) + 1);
  for i := 0 to FCount - 1 do begin
    if FKExNamedGroupList[i] = KExNamedGroup then
      raise EScError.Create(seDuplicateKExNamedGroup);
  end;

  Assert(FCount < Length(FKExNamedGroupList));
  FKExNamedGroupList[FCount] := KExNamedGroup;
  Inc(FCount);
end;

procedure TTLSSupportedGroupsExtension.Add(const KExNamedGroup: TScKExNamedGroupType);
var
  i: integer;
begin
  for i := 0 to FCount - 1 do begin
    if FKExNamedGroupList[i] = KExNamedGroup then
      raise EScError.Create(seDuplicateKExNamedGroup);
  end;

  Assert(FCount < Length(FKExNamedGroupList));
  FKExNamedGroupList[FCount] := KExNamedGroup;
  Inc(FCount);
end;

procedure TTLSSupportedGroupsExtension.Clear;
begin
  FCount := 0;
end;

function TTLSSupportedGroupsExtension.GetKExNamedGroup(Index: integer): TScKExNamedGroupType;
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt(SListIndexError, [Index]);

  Result := FKExNamedGroupList[Index];
end;

procedure TTLSSupportedGroupsExtension.Encode(Message: THandshakeMessage);
var
  i: integer;
begin
  Message.WriteInt16(FCount * 2);

  for i := 0 to FCount - 1 do
    Message.WriteInt16(KEX_NAMED_GROUP_CODES[FKExNamedGroupList[i]]);
end;

procedure TTLSSupportedGroupsExtension.Parse(Message: THandshakeMessage);
var
  KExNamedGroup: TScKExNamedGroupType;
  KExNamedGroupVal: word;
  RecLen: integer;
  i: integer;
begin
  FCount := 0;
  if Message.RestCount = 0 then
    Exit;

  RecLen := Message.ReadInt16 div 2;
  for i := 0 to RecLen - 1 do begin
    KExNamedGroupVal := Message.ReadInt16;

    for KExNamedGroup := Low(TScKExNamedGroupType) to High(TScKExNamedGroupType) do begin
      if KExNamedGroupVal = KEX_NAMED_GROUP_CODES[KExNamedGroup] then begin
        FKExNamedGroupList[FCount] := KExNamedGroup;
        Inc(FCount);
        if FCount = Length(FKExNamedGroupList) then
          Exit;

        break;
      end;
    end;
  end;
end;

{ TTLSKeyShareExtension }

constructor TTLSKeyShareExtension.Create;
begin
  inherited;

  SetLength(FEntries, KExNamedGroupType_COUNT);
  FCount := 0;
end;

destructor TTLSKeyShareExtension.Destroy;
begin
  Clear;
  inherited;
end;

procedure TTLSKeyShareExtension.AssignTo(Dest: TPersistent);
var
  i: integer;
begin
  if Dest is TTLSKeyShareExtension then begin
    TTLSKeyShareExtension(Dest).Clear; // to free all objects

    TTLSKeyShareExtension(Dest).FCount := FCount;
    for i := 0 to FCount - 1 do
      TTLSKeyShareExtension(Dest).FEntries[i].KExNamedGroup := FEntries[i].KExNamedGroup;
  end
  else
    inherited;
end;

function TTLSKeyShareExtension.GetType: TTLSExtensionType;
begin
  Result := etKeyShare;
end;

procedure TTLSKeyShareExtension.Clear;
var
  i: integer;
begin
  for i := 0 to Length(FEntries) - 1 do begin
    FEntries[i].PrivateECKey.Free;
    FEntries[i].PublicECPoint.Free;
    FEntries[i].PrivateDHX.Free;
    FEntries[i].PublicDHY.Free;
  end;

  FCount := 0;
end;

procedure TTLSKeyShareExtension.Add(const ECName: TScECName);
var
  i: integer;
begin
  for i := 0 to FCount - 1 do begin
    if FEntries[i].KExNamedGroup = TScKExNamedGroupType(byte(ECName)) then
      raise EScError.Create(seDuplicateKExNamedGroup);
  end;

  Assert(FCount < Length(FEntries));

  FEntries[FCount].KExNamedGroup := TScKExNamedGroupType(byte(ECName));
  FEntries[FCount].PrivateECKey := TScKey.Create(nil);
  FEntries[FCount].PrivateECKey.GenerateEC(ECName);
  Inc(FCount);
end;

procedure TTLSKeyShareExtension.Add(const FFGType: TScFiniteFieldGroupType);
var
  KExNamedGroup: TScKExNamedGroupType;
  RandomBuf: TBytes;
  P: TBigInteger;
  i: integer;
begin
  KExNamedGroup := TScKExNamedGroupType(integer(FFGType) + integer(High(TScECName)) + 1);

  for i := 0 to FCount - 1 do begin
    if FEntries[i].KExNamedGroup = KExNamedGroup then
      raise EScError.Create(seDuplicateKExNamedGroup);
  end;

  Assert(FCount < Length(FEntries));

  FEntries[FCount].KExNamedGroup := KExNamedGroup;
  P := FFDHE_PRIME(FFGType);
  SetLength(RandomBuf, P.BitCount div (8 * 2)); // div 8 - convert to bytes, div 2 - reduce by 2
  Random.Random(RandomBuf, 0, Length(RandomBuf));
  FEntries[FCount].PrivateDHX := TBigInteger.Create(RandomBuf);
  FEntries[FCount].PublicDHY := FFDHE_G.ModPow(FEntries[FCount].PrivateDHX, P);
  Inc(FCount);
end;

procedure TTLSKeyShareExtension.Add(const KExNamedGroup: TScKExNamedGroupType);
var
  FFGType: TScFiniteFieldGroupType;
  RandomBuf: TBytes;
  P: TBigInteger;
  i: integer;
begin
  for i := 0 to FCount - 1 do begin
    if FEntries[i].KExNamedGroup = KExNamedGroup then
      raise EScError.Create(seDuplicateKExNamedGroup);
  end;

  Assert(FCount < Length(FEntries));

  FEntries[FCount].KExNamedGroup := KExNamedGroup;
  if integer(KExNamedGroup) <= integer(High(TScECName)) then begin
    FEntries[FCount].PrivateECKey := TScKey.Create(nil);
    FEntries[FCount].PrivateECKey.GenerateEC(TScECName(byte(KExNamedGroup)));
  end
  else begin
    FFGType := TScFiniteFieldGroupType(integer(KExNamedGroup) - integer(High(TScECName)) - 1);
    P := FFDHE_PRIME(FFGType);

    SetLength(RandomBuf, P.BitCount div (8 * 2)); // div 8 - convert to bytes, div 2 - reduce by 2
    Random.Random(RandomBuf, 0, Length(RandomBuf));
    FEntries[FCount].PrivateDHX := TBigInteger.Create(RandomBuf);
    FEntries[FCount].PublicDHY := FFDHE_G.ModPow(FEntries[FCount].PrivateDHX, P);
  end;

  Inc(FCount);
end;

function TTLSKeyShareExtension.GetKExNamedGroup(Index: integer): TScKExNamedGroupType;
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt(SListIndexError, [Index]);

  Result := FEntries[Index].KExNamedGroup;
end;

function TTLSKeyShareExtension.GetPrivateECKeys(Index: integer): TScKey;
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt(SListIndexError, [Index]);

  Result := FEntries[Index].PrivateECKey;
end;

function TTLSKeyShareExtension.GetPublicECPoints(Index: integer): TScCustomECPoint;
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt(SListIndexError, [Index]);

  Result := FEntries[Index].PublicECPoint;
end;

function TTLSKeyShareExtension.GetPrivateDHXs(Index: integer): TBigInteger;
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt(SListIndexError, [Index]);

  Result := FEntries[Index].PrivateDHX;
end;

function TTLSKeyShareExtension.GetPublicDHYs(Index: integer): TBigInteger;
begin
  if (Index < 0) or (Index >= FCount) then
    raise Exception.CreateFmt(SListIndexError, [Index]);

  Result := FEntries[Index].PublicDHY;
end;

procedure TTLSKeyShareExtension.Encode(Message: THandshakeMessage);
var
  RandomBuf: TBytes;
  KeyShares: array of TBytes;
  P: TBigInteger;
  FFGType: TScFiniteFieldGroupType;
  KeySharesLen: integer;
  i: integer;
begin
  if Random = nil then
    raise Exception.Create(SInternalError);

  SetLength(KeyShares, FCount);
  KeySharesLen := 0;

  if not FOnlyRequest then begin
    for i := 0 to FCount - 1 do begin
      if integer(FEntries[i].KExNamedGroup) <= integer(High(TScECName)) then begin
        if FEntries[i].PrivateECKey = nil then begin
          FEntries[i].PrivateECKey := TScKey.Create(nil);
          FEntries[i].PrivateECKey.GenerateEC(TScECName(byte(FEntries[i].KExNamedGroup)));
        end;

        KeyShares[i] := FEntries[i].PrivateECKey.ECData.ECCryptography.EncodePointToOctetString(FEntries[i].PrivateECKey.ECData.PublicPoint);
        Inc(KeySharesLen, Length(KeyShares[i]));
      end
      else begin
        FFGType := TScFiniteFieldGroupType(integer(FEntries[i].KExNamedGroup) - integer(High(TScECName)) - 1);
        P := FFDHE_PRIME(FFGType);

        if FEntries[i].PrivateDHX = nil then begin
          SetLength(RandomBuf, P.BitCount div (8 * 2)); // div 8 - convert to bytes, div 2 - reduce by 2
          Random.Random(RandomBuf, 0, Length(RandomBuf));
          FEntries[i].PrivateDHX := TBigInteger.Create(RandomBuf);
          FEntries[i].PublicDHY := FFDHE_G.ModPow(FEntries[i].PrivateDHX, P);
        end;

        KeyShares[i] := FEntries[i].PublicDHY.GetBytes(P.BitCount shr 3); // div 8
        Inc(KeySharesLen, Length(KeyShares[i]));
      end;

      if FEntity = ceServer then
        Break;
    end;
  end;

  if FEntity = ceClient then
    Message.WriteInt16((FCount * 4) + KeySharesLen);

  for i := 0 to FCount - 1 do begin
    Message.WriteInt16(KEX_NAMED_GROUP_CODES[FEntries[i].KExNamedGroup]);

    if not FOnlyRequest then
      Message.WriteBuf16(KeyShares[i]);

    if FEntity = ceServer then
      Break;
  end;
end;

procedure TTLSKeyShareExtension.Parse(Message: THandshakeMessage);
var
  KExNamedGroup: TScKExNamedGroupType;
  KExNamedGroupVal: word;
  FFGType: TScFiniteFieldGroupType;
  P: TBigInteger;
  KeySharesLen: integer;
  IsFound: boolean;
  i, RecLen: integer;
begin
  Clear;
  if Message.RestCount = 0 then
    Exit;

  if FEntity = ceClient then
    RecLen := Message.RestCount
  else
    RecLen := Message.ReadInt16;

  FCount := 0;
  while RecLen > 0 do begin
    KExNamedGroupVal := Message.ReadInt16;
    Dec(RecLen, 2);
    IsFound := False;

    for KExNamedGroup := Low(TScKExNamedGroupType) to High(TScKExNamedGroupType) do begin
      if KExNamedGroupVal = KEX_NAMED_GROUP_CODES[KExNamedGroup] then begin
        for i := 0 to FCount - 1 do begin
          if FEntries[i].KExNamedGroup = KExNamedGroup then
            raise EScError.Create(seDuplicateKExNamedGroup);
        end;

        Assert(FCount < Length(FEntries));
        FEntries[FCount].KExNamedGroup := KExNamedGroup;

        if RecLen > 2 then begin
          KeySharesLen := Message.ReadInt16;
          Dec(RecLen, 2);
          if KeySharesLen > RecLen then
            raise EScError.Create(seWrongExtensionData);

          if integer(KExNamedGroup) <= integer(High(TScECName)) then begin
            FEntries[FCount].PrivateECKey := TScKey.Create(nil);
            FEntries[FCount].PrivateECKey.GenerateEC(TScECName(byte(KExNamedGroup)));
            FEntries[FCount].PublicECPoint := FEntries[FCount].PrivateECKey.ECData.ECCryptography.DecodePointFromOctetString(Message.Fragment, Message.ReadOffset, KeySharesLen);
            Message.SkipRead(KeySharesLen);

            if (FEntries[FCount].PublicECPoint is TScECPointBI) and (TScECPointBI(FEntries[FCount].PublicECPoint).Z.BitCount = 0) then
              raise EScError.Create(seWrongECPointFormat);
          end
          else begin
            FFGType := TScFiniteFieldGroupType(integer(KExNamedGroup) - integer(High(TScECName)) - 1);
            P := FFDHE_PRIME(FFGType);

            if (P.BitCount shr 3) <> KeySharesLen then
              raise EScError.Create(seWrongExtensionData);

            // https://tools.ietf.org/html/rfc7919#section-5.1
            // https://tools.ietf.org/html/rfc8446#section-4.2.8.1
            FEntries[FCount].PublicDHY := TBigInteger.Create(Message.Fragment, Message.ReadOffset, KeySharesLen);
            Message.SkipRead(KeySharesLen);

            if (FEntries[FCount].PublicDHY.BitCount <= 1) or FEntries[FCount].PublicDHY.GreaterOrEqual(P) then
              raise EScError.Create(seWrongExtensionData);
          end;

          Dec(RecLen, KeySharesLen);
        end;

        Inc(FCount);
        IsFound := True;
        break;
      end;
    end;

    if (FEntity = ceClient) and not IsFound then
      raise EScError.Create(seInvalidEllipticCurveName);
  end;

  if (FEntity = ceClient) and (FCount <> 1) then
    raise EScError.Create(seInvalidEllipticCurveName);
end;

{ TTLSPskKeyExchangeModesExtension }

constructor TTLSPskKeyExchangeModesExtension.Create;
begin
  inherited;

  FModes := [kemPskOnly, kemPskWithECDHE];
end;

procedure TTLSPskKeyExchangeModesExtension.AssignTo(Dest: TPersistent);
begin
  if Dest is TTLSPskKeyExchangeModesExtension then begin
    TTLSPskKeyExchangeModesExtension(Dest).FModes := FModes;
  end
  else
    inherited;
end;

function TTLSPskKeyExchangeModesExtension.GetType: TTLSExtensionType;
begin
  Result := etPskKeyExchangeModes;
end;

procedure TTLSPskKeyExchangeModesExtension.Encode(Message: THandshakeMessage);
const
  PSK_KEY_EXCHANGE_CODES: array[TScPskKeyExchangeMode] of byte =
    (0, 1);
begin
  if (kemPskOnly in FModes) and (kemPskWithECDHE in FModes) then begin
    Message.WriteInt8(2); // Len
    Message.WriteInt8(PSK_KEY_EXCHANGE_CODES[kemPskOnly]);
    Message.WriteInt8(PSK_KEY_EXCHANGE_CODES[kemPskWithECDHE]);
  end
  else
  if FModes = [kemPskWithECDHE] then begin
    Message.WriteInt8(1); // Len
    Message.WriteInt8(PSK_KEY_EXCHANGE_CODES[kemPskWithECDHE]);
  end
  else begin
    Message.WriteInt8(1); // Len
    Message.WriteInt8(PSK_KEY_EXCHANGE_CODES[kemPskOnly]);
  end;
end;

procedure TTLSPskKeyExchangeModesExtension.Parse(Message: THandshakeMessage);
var
  RecLen: integer;
  i: integer;
begin
  FModes := [];
  if Message.RestCount = 0 then
    Exit;

  RecLen := Message.ReadInt8;
  for i := 1 to RecLen do begin
    case Message.ReadInt8 of
      0:
        Include(FModes, kemPskOnly);
      1:
        Include(FModes, kemPskWithECDHE);
    end;
  end;
end;

{ TTLSPreSharedKeyExtension }

constructor TTLSPreSharedKeyExtension.Create;
begin
  inherited;

  FHashAlgorithm := haSHA2_256;
  FNewSessionTicketList := TScNewSessionTicketList.Create;
end;

destructor TTLSPreSharedKeyExtension.Destroy;
begin
  FNewSessionTicketList.Free;

  inherited;
end;

procedure TTLSPreSharedKeyExtension.AssignTo(Dest: TPersistent);
begin
  if Dest is TTLSPreSharedKeyExtension then begin
    TTLSPreSharedKeyExtension(Dest).FNewSessionTicketList.Assign(FNewSessionTicketList);
    TTLSPreSharedKeyExtension(Dest).FSelectedIdentity := FSelectedIdentity;
    TTLSPreSharedKeyExtension(Dest).FHashAlgorithm := FHashAlgorithm;
  end
  else
    inherited;
end;

function TTLSPreSharedKeyExtension.GetType: TTLSExtensionType;
begin
  Result := etPreSharedKey;
end;

function TTLSPreSharedKeyExtension.GetCount: integer;
begin
  Result := FNewSessionTicketList.Count;
end;

procedure TTLSPreSharedKeyExtension.Add(NewSessionTicket: TScNewSessionTicket);
var
  CopyNewSessionTicket: TScNewSessionTicket;
begin
  CopyNewSessionTicket := TScNewSessionTicketClass(NewSessionTicket.ClassType).Create;
  FNewSessionTicketList.Add(CopyNewSessionTicket);
  CopyNewSessionTicket.Assign(NewSessionTicket);
end;

procedure TTLSPreSharedKeyExtension.Clear;
begin
  FNewSessionTicketList.Clear;
end;

function TTLSPreSharedKeyExtension.GetNewSessionTicket(Index: integer): TScNewSessionTicket;
begin
  Result := FNewSessionTicketList.NewSessionTickets[Index];
end;

function TTLSPreSharedKeyExtension.GetIdentity(Index: integer): TBytes;
begin
  Result := FNewSessionTicketList.NewSessionTickets[Index].Ticket;
end;

procedure TTLSPreSharedKeyExtension.Encode(Message: THandshakeMessage);
var
  NewSessionTicket: TScNewSessionTicket;
  TicketAge, ObfTicketAge: cardinal;
  LenOffset, HashLen: integer;
  i: integer;
begin
  if FEntity = ceClient then begin
    LenOffset := Message.WriteOffset;
    Message.SkipWrite(2); // Len

    for i := 0 to FNewSessionTicketList.Count - 1 do begin
      NewSessionTicket := FNewSessionTicketList[i];
      TicketAge := GetUnixTime - NewSessionTicket.CreateTime;
      if TicketAge > NewSessionTicket.Lifetime then
        raise EScError.Create(seSesionTicketTimeExpired);

      ObfTicketAge := cardinal(TicketAge * 1000 + NewSessionTicket.AgeAdd);
      Message.WriteBuf16(NewSessionTicket.Ticket);
      Message.WriteInt32(ObfTicketAge);
    end;

    Message.WriteInt16ByOffset(Message.WriteOffset - LenOffset - 2, LenOffset);

    LenOffset := Message.WriteOffset;
    Message.SkipWrite(2); // Len

    for i := 0 to FNewSessionTicketList.Count - 1 do begin
      HashLen := CipherFactory.GetHashSize(FHashAlgorithm);
      Message.WriteInt8(HashLen);
      Message.SkipWrite(HashLen);
    end;

    Message.WriteInt16ByOffset(Message.WriteOffset - LenOffset - 2, LenOffset);
  end
  else begin
    Message.WriteInt16(FSelectedIdentity);
  end;
end;

procedure TTLSPreSharedKeyExtension.Parse(Message: THandshakeMessage);
var
  NewSessionTicket: TScNewSessionTicket;
  RecLen, Size: integer;
  No: integer;
begin
  if FEntity = ceServer then begin
    FNewSessionTicketList.Clear;

    RecLen := Message.ReadInt16;
    while RecLen > 0 do begin
      NewSessionTicket := TScNewSessionTicketExt.Create;
      FNewSessionTicketList.Add(NewSessionTicket);

      Size := Message.ReadInt16;
      NewSessionTicket.Ticket := Message.Read(Size);
      Dec(RecLen, 2 + Size);
      NewSessionTicket.Lifetime := Message.ReadInt32;
      Dec(RecLen, 4);
    end;

    No := 0;
    RecLen := Message.ReadInt16;
    while RecLen > 0 do begin
      NewSessionTicket := FNewSessionTicketList[No];
      Inc(No);

      Size := Message.ReadInt8;
      NewSessionTicket.Hash := Message.Read(Size);
      Dec(RecLen, 1 + Size);
    end;
  end
  else begin
    FSelectedIdentity := Message.ReadInt16;
  end;
end;

{ TTLSClientHelloPaddingExtension }

procedure TTLSClientHelloPaddingExtension.AssignTo(Dest: TPersistent);
begin
  if Dest is TTLSClientHelloPaddingExtension then begin
    TTLSClientHelloPaddingExtension(Dest).FPadding := FPadding;
  end
  else
    inherited;
end;

function TTLSClientHelloPaddingExtension.GetType: TTLSExtensionType;
begin
  Result := etClientHelloPadding;
end;

procedure TTLSClientHelloPaddingExtension.Encode(Message: THandshakeMessage);
begin
  Message.WriteBuf0(FPadding);
end;

procedure TTLSClientHelloPaddingExtension.Parse(Message: THandshakeMessage);
begin
  FPadding := Message.Read(Message.RestCount);
end;

{ TTLSEarlyDataExtension }

procedure TTLSEarlyDataExtension.AssignTo(Dest: TPersistent);
begin
  if Dest is TTLSEarlyDataExtension then begin
    TTLSEarlyDataExtension(Dest).FMaxEarlyDataSize := FMaxEarlyDataSize;
  end
  else
    inherited;
end;

function TTLSEarlyDataExtension.GetType: TTLSExtensionType;
begin
  Result := etEarlyData;
end;

procedure TTLSEarlyDataExtension.Encode(Message: THandshakeMessage);
begin
  if FMaxEarlyDataSize > 0 then
    Message.WriteInt32(FMaxEarlyDataSize);
end;

procedure TTLSEarlyDataExtension.Parse(Message: THandshakeMessage);
var
  Count: integer;
begin
  Count := Message.RestCount;

  if Count = 4 then
    FMaxEarlyDataSize := Message.ReadInt32
  else
  if Count = 0 then
    FMaxEarlyDataSize := 0
  else
    raise EScError.Create(seWrongExtensionData);
end;

{ TTLSSupportedVersionsExtension }

procedure TTLSSupportedVersionsExtension.AssignTo(Dest: TPersistent);
begin
  if Dest is TTLSSupportedVersionsExtension then begin
    TTLSSupportedVersionsExtension(Dest).FVersions := FVersions;
    TTLSSupportedVersionsExtension(Dest).FSelectedVersion := FSelectedVersion;
  end
  else
    inherited;
end;

function TTLSSupportedVersionsExtension.GetType: TTLSExtensionType;
begin
  Result := etSupportedVersions;
end;

procedure TTLSSupportedVersionsExtension.Encode(Message: THandshakeMessage);
var
  LenOffset: integer;
begin
  if FEntity = ceClient then begin
    LenOffset := Message.WriteOffset;
    Message.SkipWrite(1); // Len

    if spTls13 in FVersions then begin
      // https://tools.ietf.org/html/rfc8446#section-4.2.1
      Message.WriteInt8(3);
      Message.WriteInt8(4);
      Message.WriteInt8(127);
      Message.WriteInt8(28);
    end;

    if spTls12 in FVersions then begin
      Message.WriteInt8(3);
      Message.WriteInt8(3);
    end;
    // don't support prior versions

    Message.WriteInt8ByOffset(Message.WriteOffset - LenOffset - 1, LenOffset);
  end
  else begin
    case FSelectedVersion of
      spTls13: begin
        Message.WriteInt8(3);
        Message.WriteInt8(4);
      end;
      else begin
        Message.WriteInt8(3);
        Message.WriteInt8(4);
      end;
    end;
  end;
end;

procedure TTLSSupportedVersionsExtension.Parse(Message: THandshakeMessage);
var
  ProtocolCount: integer;
  Major, Minor: byte;
  i: integer;
begin
  FVersions := [];
  FSelectedVersion := spSsl3;

  if FEntity = ceClient then begin
    Major := Message.ReadInt8;
    Minor := Message.ReadInt8;
    if ((Major = 3) and (Minor = 4)) or ((Major = 127) and (Minor = 28)) then
      FSelectedVersion := spTls13
    else
      raise EScError.Create(seWrongExtensionData);
  end
  else begin
    ProtocolCount := Message.ReadInt8 div 2;
    for i := 0 to ProtocolCount - 1 do begin
      Major := Message.ReadInt8;
      Minor := Message.ReadInt8;

      if Major = 3 then begin
        if Minor = 4 then
          Include(FVersions, spTls13)
        else
        if Minor = 3 then
          Include(FVersions, spTls12)
        else
        if Minor = 2 then
          Include(FVersions, spTls11)
        else
        if Minor = 1 then
          Include(FVersions, spTls1);
      end;
    end;
  end;
end;

{ TTLSCookieExtension }

procedure TTLSCookieExtension.AssignTo(Dest: TPersistent);
begin
  if Dest is TTLSCookieExtension then begin
    TTLSCookieExtension(Dest).FCookie := FCookie;
  end
  else
    inherited;
end;

function TTLSCookieExtension.GetType: TTLSExtensionType;
begin
  Result := etCookie;
end;

procedure TTLSCookieExtension.Encode(Message: THandshakeMessage);
begin
  Message.WriteBuf16(FCookie);
end;

procedure TTLSCookieExtension.Parse(Message: THandshakeMessage);
var
  Len: integer;
begin
  Len := Message.ReadInt16;
  FCookie := Message.Read(Len);
end;

{ TTLSPostHandshakeAuthExtension }

procedure TTLSPostHandshakeAuthExtension.AssignTo(Dest: TPersistent);
begin
  if Dest is TTLSPostHandshakeAuthExtension then begin
    // none
  end
  else
    inherited;
end;

function TTLSPostHandshakeAuthExtension.GetType: TTLSExtensionType;
begin
  Result := etPostHandshakeAuth;
end;

procedure TTLSPostHandshakeAuthExtension.Encode(Message: THandshakeMessage);
begin
  // none
end;

procedure TTLSPostHandshakeAuthExtension.Parse(Message: THandshakeMessage);
begin
  if Message.RestCount <> 0 then
    raise EScError.Create(seWrongExtensionData);
end;

initialization
  KExNamedGroupType_COUNT := integer(High(TScKExNamedGroupType)) + 1;
  SSLSignatureScheme_COUNT := integer(High(TScSSLSignatureScheme)) + 1;

finalization

end.
