
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF DAC}
{$I SB.inc}
unit ScSSLTypes;
{$ENDIF}

interface

uses
  SysUtils, SyncObjs, Classes,
{$IFNDEF SBRIDGE}
  CRTypes, CRFunctions, CLRClasses, CRBigInteger,
{$IFNDEF UNIDACPRO}
  TdsUtils, TdsSSLConsts, TdsBridge, TdsCertificateExts;
{$ELSE}
  TdsUtilsUni, TdsSSLConstsUni, TdsBridgeUni, TdsCertificateExtsUni;
{$ENDIF}
{$ELSE}
  ScTypes, ScFunctions, ScCLRClasses, ScBigInteger,
  ScUtils, ScConsts, ScBridge, ScCertificateExts;
{$ENDIF}

type
  TScSSLProtocol = (spSsl3, spTls1, spTls11, spTls12, spTls13);
  TScSSLProtocols = set of TScSSLProtocol;

  TScSSLCipherAlgorithm = (
    caNone,
    caAnon_RC4_128_MD5,
    caAnon_3DES_168_CBC_SHA,
    caAnon_AES_128_CBC_SHA,
    caAnon_AES_256_CBC_SHA,
    caAnon_AES_128_CBC_SHA256,
    caAnon_AES_256_CBC_SHA256,
    caRSA_RC4_128_MD5,
    caRSA_RC4_128_SHA,
    caRSA_DES_56_CBC_SHA,
    caRSA_3DES_168_CBC_SHA,
    caRSA_AES_128_CBC_SHA,
    caRSA_AES_256_CBC_SHA,
    caRSA_AES_128_CBC_SHA256,
    caRSA_AES_256_CBC_SHA256,
    caRSA_AES_128_GCM_SHA256,
    caRSA_AES_256_GCM_SHA384,
    caDHE_RSA_DES_56_CBC_SHA,
    caDHE_RSA_3DES_168_CBC_SHA,
    caDHE_RSA_AES_128_CBC_SHA,
    caDHE_RSA_AES_256_CBC_SHA,
    caDHE_RSA_AES_128_CBC_SHA256,
    caDHE_RSA_AES_256_CBC_SHA256,
    caDHE_RSA_AES_128_GCM_SHA256,
    caDHE_RSA_AES_256_GCM_SHA384,
    caECDHE_RSA_RC4_128_SHA,
    caECDHE_RSA_3DES_168_CBC_SHA,
    caECDHE_RSA_AES_128_CBC_SHA,
    caECDHE_RSA_AES_256_CBC_SHA,
    caECDHE_RSA_AES_128_CBC_SHA256,
    caECDHE_RSA_AES_256_CBC_SHA384,
    caECDHE_RSA_AES_128_GCM_SHA256,
    caECDHE_RSA_AES_256_GCM_SHA384,
    caECDHE_ECDSA_RC4_128_SHA,
    caECDHE_ECDSA_3DES_168_CBC_SHA,
    caECDHE_ECDSA_AES_128_CBC_SHA,
    caECDHE_ECDSA_AES_256_CBC_SHA,
    caECDHE_ECDSA_AES_128_CBC_SHA256,
    caECDHE_ECDSA_AES_256_CBC_SHA384,
    caECDHE_ECDSA_AES_128_GCM_SHA256,
    caECDHE_ECDSA_AES_256_GCM_SHA384,
    caAES_128_GCM_SHA256,
    caAES_256_GCM_SHA384
  );

  TScSSLCipherAlgorithmArray = array of TScSSLCipherAlgorithm;

  TScSSLSignatureAlgorithm = TScAsymmetricAlgorithm;

   TScSSLSignatureScheme = (
    ssRSA_PKCS1_SHA256, ssRSA_PKCS1_SHA384, ssRSA_PKCS1_SHA512,
    ssECDSA_SECP256r1_SHA256, ssECDSA_SECP384r1_SHA384, ssECDSA_SECP521r1_SHA512,
    ssRSA_PSS_RSAE_SHA256, ssRSA_PSS_RSAE_SHA384, ssRSA_PSS_RSAE_SHA512,
    ssRSA_PSS_PSS_SHA256, ssRSA_PSS_PSS_SHA384, ssRSA_PSS_PSS_SHA512,
    ssEd25519,
    ssRSA_PKCS1_SHA1,
    ssECDSA_SHA1
  );

  TScECurveDomainType = (ecdtExplicitPrime, ecdtExplicitChar2, ecdtNamedCurve);

  TScFiniteFieldGroupType = (
    ffDHE2048, ffDHE3072, ffDHE4096,
    ffDHE6144, ffDHE8192
  );

  // !!! Synchronize with TScECName and TScFiniteFieldGroupType
  TScKExNamedGroupType = (
    kex_x25519,
    kex_secp160r1, kex_secp160r2, kex_secp160k1,
    kex_secp192r1, kex_secp192k1, kex_secp224r1, kex_secp224k1,
    kex_secp256r1, kex_secp256k1, kex_secp384r1, kex_secp521r1,
    kex_sect163r1, kex_sect163r2, kex_sect163k1,
    kex_sect193r1, kex_sect193r2, kex_sect233r1, kex_sect233k1, kex_sect239k1,
    kex_sect283r1, kex_sect283k1, kex_sect409r1, kex_sect409k1,
    kex_sect571r1, kex_sect571k1,
    kex_ffDHE2048, kex_ffDHE3072, kex_ffDHE4096,
    kex_ffDHE6144, kex_ffDHE8192
  );

  TScKExNamedGroupTypes = set of TScKExNamedGroupType;

  TScSSLStatus = (ssOK, ssContinueNeeded, ssMessageIncomplete, ssShuttedDown);
  TScSSLConnectionEnd = (ceClient, ceServer);

  TScExtendedMasterSecretMode = (emsmDisable, emsmAllow, emsmRequire);

  /// Specifies the method used to verify the remote credential.
  TScCredentialVerification = (
    cvManual, /// The remote certificate will be manually verified. When an incoming connection is accepted, the SecureSocket will raise a CertVerification event. This is the recommended credential verification method.
    cvAuto, /// The remote certificate will be automatically verified by the crypto API.
    // cvAutoWithoutCName, /// The remote certificate will be automatically verified by the crypto API, but the common name of the server will not be checked.
    cvNone /// The remote certificate will not be verified. This method is not secure and should only be used for debugging purposes.
  );

  /// References the method to be called when the SecureSocket receives a Certificate request from the peer.
  TScOnCertificateRequest = procedure(Acceptable: TScDistinguishedNameList; CertList: TCRList) of object;
  TScOnNeedVerifyCertificate = procedure(out NeedVerify: boolean) of object;

const
  AnonAlgorithms = [caAnon_RC4_128_MD5, caAnon_3DES_168_CBC_SHA,
    caAnon_AES_128_CBC_SHA, caAnon_AES_256_CBC_SHA,
    caAnon_AES_128_CBC_SHA256, caAnon_AES_256_CBC_SHA256];

  DHEAlgorithms = [caDHE_RSA_DES_56_CBC_SHA, caDHE_RSA_3DES_168_CBC_SHA,
    caDHE_RSA_AES_128_CBC_SHA, caDHE_RSA_AES_256_CBC_SHA,
    caDHE_RSA_AES_128_CBC_SHA256, caDHE_RSA_AES_256_CBC_SHA256,
    caDHE_RSA_AES_128_GCM_SHA256, caDHE_RSA_AES_256_GCM_SHA384];

  ECDHEAlgorithms = [caECDHE_RSA_RC4_128_SHA, caECDHE_RSA_3DES_168_CBC_SHA,
    caECDHE_RSA_AES_128_CBC_SHA, caECDHE_RSA_AES_256_CBC_SHA,
    caECDHE_RSA_AES_128_CBC_SHA256, caECDHE_RSA_AES_256_CBC_SHA384,
    caECDHE_RSA_AES_128_GCM_SHA256, caECDHE_RSA_AES_256_GCM_SHA384,
    caECDHE_ECDSA_RC4_128_SHA, caECDHE_ECDSA_3DES_168_CBC_SHA,
    caECDHE_ECDSA_AES_128_CBC_SHA, caECDHE_ECDSA_AES_256_CBC_SHA,
    caECDHE_ECDSA_AES_128_CBC_SHA256, caECDHE_ECDSA_AES_256_CBC_SHA384,
    caECDHE_ECDSA_AES_128_GCM_SHA256, caECDHE_ECDSA_AES_256_GCM_SHA384
  ];

const
  NONE = 'NONE';
  Anon_RC4_128_MD5 = 'Anon_RC4_128_MD5';
  Anon_3DES_168_CBC_SHA = 'Anon_3DES_168_CBC_SHA';
  Anon_AES_128_CBC_SHA = 'Anon_AES_128_CBC_SHA';
  Anon_AES_256_CBC_SHA = 'Anon_AES_256_CBC_SHA';
  Anon_AES_128_CBC_SHA256 = 'Anon_AES_128_CBC_SHA256';
  Anon_AES_256_CBC_SHA256 = 'Anon_AES_256_CBC_SHA256';
  RSA_RC4_40_MD5 = 'RSA_RC4_40_MD5';
  RSA_RC4_128_MD5 = 'RSA_RC4_128_MD5';
  RSA_RC4_128_SHA = 'RSA_RC4_128_SHA';
  RSA_DES_56_CBC_SHA = 'RSA_DES_56_CBC_SHA';
  RSA_3DES_168_CBC_SHA = 'RSA_3DES_168_CBC_SHA';
  RSA_DES_40_CBC_SHA = 'RSA_DES_40_CBC_SHA';
  RSA_AES_128_CBC_SHA = 'RSA_AES_128_CBC_SHA';
  RSA_AES_256_CBC_SHA = 'RSA_AES_256_CBC_SHA';
  RSA_AES_128_CBC_SHA256 = 'RSA_AES_128_CBC_SHA256';
  RSA_AES_256_CBC_SHA256 = 'RSA_AES_256_CBC_SHA256';
  RSA_AES_128_GCM_SHA256 = 'RSA_AES_128_GCM_SHA256';
  RSA_AES_256_GCM_SHA384 = 'RSA_AES_256_GCM_SHA384';
  DHE_RSA_DES_56_CBC_SHA = 'DHE_RSA_DES_56_CBC_SHA';
  DHE_RSA_3DES_168_CBC_SHA = 'DHE_RSA_3DES_168_CBC_SHA';
  DHE_RSA_AES_128_CBC_SHA = 'DHE_RSA_AES_128_CBC_SHA';
  DHE_RSA_AES_256_CBC_SHA = 'DHE_RSA_AES_256_CBC_SHA';
  DHE_RSA_AES_128_CBC_SHA256 = 'DHE_RSA_AES_128_CBC_SHA256';
  DHE_RSA_AES_256_CBC_SHA256 = 'DHE_RSA_AES_256_CBC_SHA256';
  DHE_RSA_AES_128_GCM_SHA256 = 'DHE_RSA_AES_128_GCM_SHA256';
  DHE_RSA_AES_256_GCM_SHA384 = 'DHE_RSA_AES_256_GCM_SHA384';
  ECDHE_RSA_RC4_128_SHA = 'ECDHE_RSA_RC4_128_CBC_SHA';
  ECDHE_RSA_3DES_168_CBC_SHA = 'ECDHE_RSA_3DES_168_CBC_SHA';
  ECDHE_RSA_AES_128_CBC_SHA = 'ECDHE_RSA_AES_128_CBC_SHA';
  ECDHE_RSA_AES_256_CBC_SHA = 'ECDHE_RSA_AES_256_CBC_SHA';
  ECDHE_RSA_AES_128_CBC_SHA256 = 'ECDHE_RSA_AES_128_CBC_SHA256';
  ECDHE_RSA_AES_256_CBC_SHA384 = 'ECDHE_RSA_AES_256_CBC_SHA384';
  ECDHE_RSA_AES_128_GCM_SHA256 = 'ECDHE_RSA_AES_128_GCM_SHA256';
  ECDHE_RSA_AES_256_GCM_SHA384 = 'ECDHE_RSA_AES_256_GCM_SHA384';
  ECDHE_ECDSA_RC4_128_SHA = 'ECDHE_ECDSA_RC4_128_SHA';
  ECDHE_ECDSA_3DES_168_CBC_SHA = 'ECDHE_ECDSA_3DES_168_CBC_SHA';
  ECDHE_ECDSA_AES_128_CBC_SHA = 'ECDHE_ECDSA_AES_128_CBC_SHA';
  ECDHE_ECDSA_AES_256_CBC_SHA = 'ECDHE_ECDSA_AES_256_CBC_SHA';
  ECDHE_ECDSA_AES_128_CBC_SHA256 = 'ECDHE_ECDSA_AES_128_CBC_SHA256';
  ECDHE_ECDSA_AES_256_CBC_SHA384 = 'ECDHE_ECDSA_AES_256_CBC_SHA384';
  ECDHE_ECDSA_AES_128_GCM_SHA256 = 'ECDHE_ECDSA_AES_128_GCM_SHA256';
  ECDHE_ECDSA_AES_256_GCM_SHA384 = 'ECDHE_ECDSA_AES_256_GCM_SHA384';
  AES_128_GCM_SHA256 = 'AES_128_GCM_SHA256';
  AES_256_GCM_SHA384 = 'AES_256_GCM_SHA384';

type
  TScContentType = (
    ctChangeCipherSpec,
    ctAlert,
    ctHandshake,
    ctApplicationData,
    ctHeartBeat,
    ctUnknown
  );

const
  CONTENT_TYPE_CODES: array[TScContentType] of byte = (
    20{ctChangeCipherSpec},
    21{ctAlert},
    22{ctHandshake},
    23{ctApplicationData},
    24{ctHeartBeat},
    0 {ctUnknown}
  );

type
  TScHandshakeType = (
    htHelloRequest,
    htClientHello,
    htServerHello,
    htNewSessionTicket,
    htEndOfEarlyData,
    htEncryptedExtensions,
    htCertificate,
    htServerKeyExchange,
    htCertificateRequest,
    htServerHelloDone,
    htCertificateVerify,
    htClientKeyExchange,
    htFinished,
    htKeyUpdate,
    htMessageHash,
    /// not part of TLS standard, used internally in SecureBridge
    htShuttdownSent,
    htShuttdownRecv,
    htShuttedDown,
    htChangeCipherSpec,
    htNothing
  );

const
  // https://tools.ietf.org/html/rfc8446#section-4
  HANDSHAKE_TYPE_CODES: array[TScHandshakeType] of byte = (
    0{htHelloRequest}, 1{htClientHello}, 2{htServerHello},
    4{htNewSessionTicket}, 5{htEndOfEarlyData}, 8{htEncryptedExtensions},
    11{htCertificate}, 12{htServerKeyExchange}, 13{htCertificateRequest},
    14{htServerHelloDone}, 15{htCertificateVerify}, 16{htClientKeyExchange},
    20{htFinished}, 24{htKeyUpdate}, 254{htMessageHash},
  /// not part of TLS standard, used internally in SecureBridge
    201{htShuttdownSent}, 202{htShuttdownRecv}, 203{htShuttedDown},
    204{htChangeCipherSpec}, 255{htNothing}
  );

type
  TScAlertLevel = (
    alWarning,
    alFatal
  );

  TScAlertDescription = (
    adCloseNotify,
    adUnexpectedMessage,
    adBadRecordMac,
    adDecryptionFailed,
    adRecordOverflow,
    adDecompressionFailure,
    adHandshakeFailure,
    adBadCertificate,
    adUnsupportedCertificate,
    adCertificateRevoked,
    adCertificateExpired,
    adCertificateUnknown,
    adIllegalParameter,
    adUnknownCa,
    adAccessDenied,
    adDecodeError,
    adDecryptError,
    adExportRestriction,
    adProtocolVersion,
    adInsufficientSecurity,
    adInternalError,
    adInappropriateFallback,
    adUserCanceled,
    adNoRenegotiation,
    adMissingExtension,
    adUnsupportedExtension,
    adUnrecognizedName,
    adBadCertificateStatusResponse,
    adUnknownPskIdentity,
    adCertificateRequired,
    adNoApplicationProtocol
  );

const
  ALERT_LEVEL_CODES: array[TScAlertLevel] of byte = (
    1{alWarning}, 2{alFatal}
  );

  ALERT_DESCRIPTION_CODES: array[TScAlertDescription] of byte = (
    0{adCloseNotify}, 10{adUnexpectedMessage}, 20{adBadRecordMac},
    21{adDecryptionFailed}, 22{adRecordOverflow}, 30{adDecompressionFailure},
    40{adHandshakeFailure}, 42{adBadCertificate}, 43{adUnsupportedCertificate},
    44{adCertificateRevoked}, 45{adCertificateExpired}, 46{adCertificateUnknown},
    47{adIllegalParameter}, 48{adUnknownCa}, 49{adAccessDenied},
    50{adDecodeError}, 51{adDecryptError}, 60{adExportRestriction},
    70{adProtocolVersion}, 71{adInsufficientSecurity}, 80{adInternalError},
    86{adInappropriateFallback}, 90{adUserCanceled}, 100{adNoRenegotiation},
    109{adMissingExtension}, 110{adUnsupportedExtension}, 112{adUnrecognizedName},
    113{adBadCertificateStatusResponse}, 115{adUnknownPskIdentity},
    116{adCertificateRequired}, 120{adNoApplicationProtocol}
  );

  WARNING_ALERT_DESCRIPTIONS = [
    adBadCertificate, adUnsupportedCertificate,
    adCertificateRevoked, adCertificateExpired, adCertificateUnknown,
    adUserCanceled, adNoRenegotiation
  ];

type
  TScClientCertificateType = type Byte;

const
  ctRsaSign = TScClientCertificateType(1);
  ctDssSign = TScClientCertificateType(2);
  ctRsaFixedDh = TScClientCertificateType(3);
  ctDssFixedDh = TScClientCertificateType(4);
  ctRsaEphemeralDh = TScClientCertificateType(5);
  ctDssEphemeralDh = TScClientCertificateType(6);
  ctFortezzaDms = TScClientCertificateType(20);
  ctECdsaSign = TScClientCertificateType(64);
  ctRsaFixedEcdh = TScClientCertificateType(65);
  ctECdsaFixedEcdh = TScClientCertificateType(66);

type
  TScKeyUpdateRequest = (kurUpdateNotRequested, kurUpdateRequested);

const
  KEY_UPDATE_REQUEST_CODES: array[TScKeyUpdateRequest] of byte = (
    0{kurUpdateNotRequested},
    1{kurUpdateRequested}
  );

type
  TScSSLVersionRec = record
    Major: Byte;
    Minor: Byte;
  end;

  TScSSLVersionHelper = class
  public
    class function CheckMaxSupportedProtocol(const SupportedProtocols: TScSSLProtocols;
      const MaxProtocol: TScSSLProtocol; out ResProtocol: TScSSLProtocol): boolean;
    class function GetMaxSupportedProtocol(const SupportedProtocols: TScSSLProtocols;
      const MaxProtocol: TScSSLProtocol): TScSSLProtocol;
    class function GetProtocol(const Version: TScSSLVersionRec): TScSSLProtocol;
    class function GetVersion(const Protocol: TScSSLProtocol): TScSSLVersionRec;
    class function GetMinProtocol(const Protocols: TScSSLProtocols): TScSSLProtocol;
    class function GetMaxProtocol(const Protocols: TScSSLProtocols): TScSSLProtocol;
  end;

  TScCompressionHelper = class
  public
    class function ToCompression(Value: byte): TScCompression;
    class function CompressionToBuf(const Compression: TScCompression): TBytes;
  end;

  TScSSLCipherSuiteItem = class(TScCollectionItem)
  private
    FCipherAlgorithm: TScSSLCipherAlgorithm;
  protected
    function GetAsString: string; override;
    procedure SetAsString(const Value: string); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    property AsString: string read GetAsString write SetAsString;
  published
    property CipherAlgorithm: TScSSLCipherAlgorithm read FCipherAlgorithm write FCipherAlgorithm;
  end;

  TScSSLCipherSuites = class(TScCollection)
  public
    constructor Create(AOwner: TPersistent);
    function GetCipherSuites: TScSSLCipherAlgorithmArray;
  end;

  TScAsyncResult = class(TInterfacedObject, IScAsyncResult)
  private
    FCompleted: boolean;
    FStateObject: TObject;
    FOwner: TCriticalSection;
    FWaitHandle: TEvent;
    FAsyncException: Exception;
    FCallback: AsyncCallback;

  public
    constructor Create(Callback: AsyncCallback; StateObject: TObject; Owner: TCriticalSection);
    destructor Destroy; override;

    function get_AsyncState: TObject;
    function get_AsyncWaitHandle: TEvent;
    function get_IsCompleted: boolean;

    procedure Notify(e: Exception); overload;
    procedure Notify; overload;
    procedure RaiseLastError;

    property AsyncState: TObject read get_AsyncState;
    property AsyncWaitHandle: TEvent read get_AsyncWaitHandle;
    property IsCompleted: boolean read get_IsCompleted;
    property Callback: AsyncCallback read FCallback;
    property AsyncException: Exception write FAsyncException;
  end;

  TScNewSessionTicketClass = class of TScNewSessionTicket;

  TScNewSessionTicket = class(TPersistent)
  protected
    FCreateTime: cardinal;
    FLifetime: cardinal;
    FAgeAdd: cardinal;
    FNonce: TBytes;
    FTicket: TBytes;
    FHash: TBytes;

    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;

    property CreateTime: cardinal read FCreateTime write FCreateTime;
    property Lifetime: cardinal read FLifetime write FLifetime;
    property AgeAdd: cardinal read FAgeAdd write FAgeAdd;
    property Nonce: TBytes read FNonce write FNonce;
    property Ticket: TBytes read FTicket write FTicket;
    property Hash: TBytes read FHash write FHash;
  end;

  TScNewSessionTicketList = class(TCRObjectList)
  private
    function GetNewSessionTicket(Index: integer): TScNewSessionTicket;
    procedure SetNewSessionTicket(Index: integer; Value: TScNewSessionTicket);

  public
    function Add(Item: TScNewSessionTicket): integer;
    procedure Assign(SrcList: TScNewSessionTicketList);

    property NewSessionTickets[Index: integer]: TScNewSessionTicket read GetNewSessionTicket write SetNewSessionTicket; default;
  end;

  TScSSLSessionInfo = class(TPersistent)
  private
    FInitialized: boolean;
    FCipherAlgorithm: TScSSLCipherAlgorithm;
    FCompression: TScCompression;
    FProtocol: TScSSLProtocol;
    FRemoteCertificate: TScCertificate;
    FDisableInsertEmptyFragment: boolean;
    FSessionID: TBytes;
    FMasterSecret: TBytes;
    FUseExtendedMasterSecret: boolean;
    FTicketNonce: TBytes;
    FNewSessionTickets: TScNewSessionTicketList;
    FNewSessionTicketCount: integer;

    procedure SetInitialized(Value: boolean);
    procedure SetMasterSecret(const Value: TBytes);

  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Clear;

  public
    constructor Create;
    destructor Destroy; override;

    property Initialized: boolean read FInitialized write SetInitialized;

    property CipherAlgorithm: TScSSLCipherAlgorithm read FCipherAlgorithm write FCipherAlgorithm;
    property Compression: TScCompression read FCompression write FCompression;
    property Protocol: TScSSLProtocol read FProtocol write FProtocol;
    property RemoteCertificate: TScCertificate read FRemoteCertificate write FRemoteCertificate;
    property DisableInsertEmptyFragment: boolean read FDisableInsertEmptyFragment write FDisableInsertEmptyFragment;
    property SessionID: TBytes read FSessionID write FSessionID;
    property MasterSecret: TBytes read FMasterSecret write SetMasterSecret;
    property UseExtendedMasterSecret: boolean read FUseExtendedMasterSecret write FUseExtendedMasterSecret;
    property TicketNonce: TBytes read FTicketNonce write FTicketNonce;
    property NewSessionTickets: TScNewSessionTicketList read FNewSessionTickets;
    property NewSessionTicketCount: integer read FNewSessionTicketCount write FNewSessionTicketCount;
  end;

  TScLayersHelper = class
    class function FindContentType(Value: byte): TScContentType;
    class function FindHandshakeType(Value: byte): TScHandshakeType;
  end;

const
  DEFAULT_NEW_SESSION_TICKET_LIFETIME = 600;

const
  TLS11Protection: array[0..7] of byte = ($44, $4F, $57, $4E, $47, $52, $44, $00);
  TLS12Protection: array[0..7] of byte = ($44, $4F, $57, $4E, $47, $52, $44, $01);
  HelloRetryRequestID: array[0..31] of byte = (
    $CF, $21, $AD, $74, $E5, $9A, $61, $11, $BE, $1D, $8C, $02, $1E, $65, $B8, $91,
    $C2, $A2, $11, $16, $7A, $BB, $8C, $5E, $07, $9E, $09, $E2, $C8, $A8, $33, $9C
  );

const
  SIGNATURE_SCHEME_CODES: array [TScSSLSignatureScheme] of word = (
    $0401 {ssRSA_PKCS1_SHA256}, $0501 {ssRSA_PKCS1_SHA384}, $0601 {ssRSA_PKCS1_SHA512},
    $0403 {ssECDSA_SECP256r1_SHA256}, $0503 {ssECDSA_SECP384r1_SHA384}, $0603 {ssECDSA_SECP521r1_SHA512},
    $0804 {ssRSA_PSS_RSAE_SHA256}, $0805 {ssRSA_PSS_RSAE_SHA384}, $0806 {ssRSA_PSS_RSAE_SHA512},
    $0809 {ssRSA_PSS_PSS_SHA256}, $080A {ssRSA_PSS_PSS_SHA384}, $080B {ssRSA_PSS_PSS_SHA512},
    $0807 {ssEd25519},
    $0201 {ssRSA_PKCS1_SHA1},
    $0203 {ssECDSA_SHA1}
  );

function FFDHE_PRIME(FFGType: TScFiniteFieldGroupType): TBigInteger;

var
  FFDHE_G,
  FFDHE_PRIME_2048, FFDHE_PRIME_3072, FFDHE_PRIME_4096,
  FFDHE_PRIME_6144, FFDHE_PRIME_8192: TBigInteger;

implementation

uses
{$IFNDEF SBRIDGE}
{$IFNDEF UNIDACPRO}
  TdsCipherSuites;
{$ELSE}
  TdsCipherSuitesUni;
{$ENDIF}
{$ELSE}
  ScCipherSuites;
{$ENDIF}

{ TScSSLVersionHelper }

class function TScSSLVersionHelper.CheckMaxSupportedProtocol(const SupportedProtocols: TScSSLProtocols;
  const MaxProtocol: TScSSLProtocol; out ResProtocol: TScSSLProtocol): boolean;
begin
  Result := True;

  case MaxProtocol of
    spTls13: begin
      if spTls13 in SupportedProtocols then
        ResProtocol := spTls13
      else
      if spTls12 in SupportedProtocols then
        ResProtocol := spTls12
      else
      if spTls11 in SupportedProtocols then
        ResProtocol := spTls11
      else
      if spTls1 in SupportedProtocols then
        ResProtocol := spTls1
      else
      if spSsl3 in SupportedProtocols then
        ResProtocol := spSsl3
      else
        Result := False;
    end;

    spTls12: begin
      if spTls12 in SupportedProtocols then
        ResProtocol := spTls12
      else
      if spTls11 in SupportedProtocols then
        ResProtocol := spTls11
      else
      if spTls1 in SupportedProtocols then
        ResProtocol := spTls1
      else
      if spSsl3 in SupportedProtocols then
        ResProtocol := spSsl3
      else
        Result := False;
    end;

    spTls11: begin
      if spTls11 in SupportedProtocols then
        ResProtocol := spTls11
      else
      if spTls1 in SupportedProtocols then
        ResProtocol := spTls1
      else
      if spSsl3 in SupportedProtocols then
        ResProtocol := spSsl3
      else
        Result := False;
    end;

    spTls1: begin
      if spTls1 in SupportedProtocols then
        ResProtocol := spTls1
      else
      if spSsl3 in SupportedProtocols then
        ResProtocol := spSsl3
      else
        Result := False;
    end;

    else begin
      if spSsl3 in SupportedProtocols then
        ResProtocol := spSsl3
      else
        Result := False;
    end;
  end;
end;

class function TScSSLVersionHelper.GetMaxSupportedProtocol(const SupportedProtocols: TScSSLProtocols;
  const MaxProtocol: TScSSLProtocol): TScSSLProtocol;
begin
  if not CheckMaxSupportedProtocol(SupportedProtocols, MaxProtocol, Result) then
    raise EscError.Create(seNotAgreeOnProtocol);
end;

class function TScSSLVersionHelper.GetProtocol(const Version: TScSSLVersionRec): TScSSLProtocol;
var
  Ver: integer;
begin
  Ver := (Version.Major * 10 + Version.Minor);
  if Ver >= 33 then
    Result := spTls12
  else
  if Ver = 32 then
    Result := spTls11
  else
  if Ver = 31 then
    Result := spTls1
  else
    Result := spSsl3;
end;

class function TScSSLVersionHelper.GetVersion(const Protocol: TScSSLProtocol): TScSSLVersionRec;
begin
  case Protocol of
    spTls13: begin
      Result.Major := 3;
      Result.Minor := 3;
    end;
    spTls12: begin
      Result.Major := 3;
      Result.Minor := 3;
    end;
    spTls11: begin
      Result.Major := 3;
      Result.Minor := 2;
    end;
    spTls1: begin
      Result.Major := 3;
      Result.Minor := 1;
    end;
    spSsl3: begin
      Result.Major := 3;
      Result.Minor := 0;
    end;
    else begin
      Result.Major := 3;
      Result.Minor := 3;
    end;
  end;
end;

class function TScSSLVersionHelper.GetMinProtocol(const Protocols: TScSSLProtocols): TScSSLProtocol;
begin
  if spSsl3 in Protocols then
    Result := spSsl3
  else
  if spTls1 in Protocols then
    Result := spTls1
  else
  if spTls11 in Protocols then
    Result := spTls11
  else
  if spTls12 in Protocols then
    Result := spTls12
  else
  if spTls13 in Protocols then
    Result := spTls13
  else
    Result := spSsl3;
end;

class function TScSSLVersionHelper.GetMaxProtocol(const Protocols: TScSSLProtocols): TScSSLProtocol;
begin
  if spTls13 in Protocols then
    Result := spTls13
  else
  if spTls12 in Protocols then
    Result := spTls12
  else
  if spTls11 in Protocols then
    Result := spTls11
  else
  if spTls1 in Protocols then
    Result := spTls1
  else
  if spSsl3 in Protocols then
    Result := spSsl3
  else
    Result := spSsl3; // if empty - not supported anything
end;

{ TScCompressionHelper }

class function TScCompressionHelper.ToCompression(Value: byte): TScCompression;
begin
  case Value of
    0:
      Result := csNone;
    1:
      Result := csRequired;
  else
    raise EScError.Create(seInvalidCompressionAlgorithm);
  end;
end;

class function TScCompressionHelper.CompressionToBuf(const Compression: TScCompression): TBytes;
begin
  case Compression of
    csNone: begin
      SetLength(Result, 1);
      Result[0] := 0;
    end;
    csAllowed, csRequired: begin
      SetLength(Result, 2);
      Result[0] := 0;
      Result[1] := 1;
    end;
  end;
end;

{ TScSSLCipherSuiteItem }

procedure TScSSLCipherSuiteItem.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScSSLCipherSuiteItem) then
    TScSSLCipherSuiteItem(Dest).FCipherAlgorithm := FCipherAlgorithm
  else
    inherited;
end;

function TScSSLCipherSuiteItem.GetAsString: string;
begin
  Result := TCipherSuites.CipherAlgorithmToName(FCipherAlgorithm);
end;

procedure TScSSLCipherSuiteItem.SetAsString(const Value: string);
begin
  FCipherAlgorithm := TCipherSuites.NameToCipherAlgorithm(UpperCase(Trim(Value)));
end;

{ TScSSLCipherSuites }

constructor TScSSLCipherSuites.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TScSSLCipherSuiteItem);
end;

function TScSSLCipherSuites.GetCipherSuites: TScSSLCipherAlgorithmArray;
var
  i: integer;
begin
  SetLength(Result, Count);
  for i := 0 to Count - 1 do
    Result[i] := TScSSLCipherSuiteItem(Items[i]).FCipherAlgorithm;
end;

{ TScAsyncResult }

constructor TScAsyncResult.Create(Callback: AsyncCallback; StateObject: TObject; Owner: TCriticalSection);
begin
  inherited Create;

  FStateObject := StateObject;
  FCompleted := False;
  FOwner := Owner;
  FAsyncException := nil;
  FCallback := Callback;
end;

destructor TScAsyncResult.Destroy;
begin
  FreeAndNil(FWaitHandle);
  FreeAndNil(FAsyncException);

  inherited;
end;

procedure TScAsyncResult.RaiseLastError;
var
  e: Exception;
begin
  if FAsyncException <> nil then begin
    e := FAsyncException;
    FAsyncException := nil;
    raise e;
  end;
end;

procedure TScAsyncResult.Notify(e: Exception);
begin
  if not FCompleted then begin
    FAsyncException := e;
    FCompleted := True;

    if Assigned(FCallback) then begin
      if FOwner <> nil then // exit the synchronization lock, if necessary
        unlock(FOwner); // Monitor.Exit(FOwner);
      try
        Callback(Self);
      finally
        if FOwner <> nil then // acquire the synchronization lock, if necessary
          lock(FOwner); // Monitor.Enter(FOwner);
      end;
    end;

    if FWaitHandle <> nil then
      FWaitHandle.SetEvent;
  end;
end;

procedure TScAsyncResult.Notify;
begin
  Notify(Self.FAsyncException);
end;

function TScAsyncResult.get_AsyncState: TObject;
begin
  Result := FStateObject;
end;

function TScAsyncResult.get_IsCompleted: boolean;
begin
  Result := FCompleted;
end;

function TScAsyncResult.get_AsyncWaitHandle: TEvent;
begin
  if FWaitHandle = nil then
    FWaitHandle := CreateEvent(FCompleted);

  if FCompleted then
    FWaitHandle.SetEvent;

  Result := FWaitHandle;
end;

{ TScNewSessionTicket }

constructor TScNewSessionTicket.Create;
begin
  inherited;
end;

procedure TScNewSessionTicket.AssignTo(Dest: TPersistent);
var
  DstTicket: TScNewSessionTicket;
begin
  if IsClass(Dest, TScNewSessionTicket) then begin
    DstTicket := TScNewSessionTicket(Dest);

    DstTicket.FCreateTime := FCreateTime;
    DstTicket.FLifetime := FLifetime;
    DstTicket.FAgeAdd := FAgeAdd;
    DstTicket.FNonce := FNonce;
    DstTicket.FTicket := FTicket;
    DstTicket.FHash := FHash;
  end
  else
    inherited;
end;

{ TScNewSessionTicketList }

function TScNewSessionTicketList.GetNewSessionTicket(Index: integer): TScNewSessionTicket;
begin
  Result := TObject(inherited Items[Index]) as TScNewSessionTicket;
end;

procedure TScNewSessionTicketList.SetNewSessionTicket(Index: integer; Value: TScNewSessionTicket);
begin
  inherited Items[Index] := Value;
end;

function TScNewSessionTicketList.Add(Item: TScNewSessionTicket): integer;
begin
  Result := inherited Add(Item);
end;

procedure TScNewSessionTicketList.Assign(SrcList: TScNewSessionTicketList);
var
  SrcNewSessionTicket, DstNewSessionTicket: TScNewSessionTicket;
  i: integer;
begin
  Clear;

  for i := 0 to SrcList.Count - 1 do begin
    SrcNewSessionTicket := TObject(SrcList.Items[i]) as TScNewSessionTicket;
    DstNewSessionTicket := TScNewSessionTicketClass(SrcNewSessionTicket.ClassType).Create;
    DstNewSessionTicket.Assign(SrcNewSessionTicket);
    Add(DstNewSessionTicket);
  end;
end;

{ TScSSLSessionInfo }

constructor TScSSLSessionInfo.Create;
begin
  inherited;
  FNewSessionTickets := TScNewSessionTicketList.Create;
end;

destructor TScSSLSessionInfo.Destroy;
begin
  FNewSessionTickets.Free;
  inherited;
end;

procedure TScSSLSessionInfo.AssignTo(Dest: TPersistent);
var
  DstSessionInfo: TScSSLSessionInfo;
begin
  if IsClass(Dest, TScSSLSessionInfo) then begin
    DstSessionInfo := TScSSLSessionInfo(Dest);

    DstSessionInfo.FInitialized := FInitialized;
    DstSessionInfo.FCipherAlgorithm := FCipherAlgorithm;
    DstSessionInfo.FCompression := FCompression;
    DstSessionInfo.FProtocol := FProtocol;
    DstSessionInfo.FRemoteCertificate := FRemoteCertificate;
    DstSessionInfo.FDisableInsertEmptyFragment := FDisableInsertEmptyFragment;
    DstSessionInfo.FSessionID := FSessionID;
    DstSessionInfo.FUseExtendedMasterSecret := FUseExtendedMasterSecret;
    DstSessionInfo.FTicketNonce := FTicketNonce;
    DstSessionInfo.FNewSessionTicketCount := FNewSessionTicketCount;
    DstSessionInfo.SetMasterSecret(FMasterSecret);
    DstSessionInfo.FNewSessionTickets.Assign(FNewSessionTickets);
  end
  else
    inherited;
end;

procedure TScSSLSessionInfo.Clear;
begin
  if Length(FMasterSecret) > 0 then begin
    FillChar(FMasterSecret[0], Length(FMasterSecret), 0);
    SetLength(FMasterSecret, 0);
  end;

  FUseExtendedMasterSecret := False;
  FNewSessionTickets.Clear;
end;

procedure TScSSLSessionInfo.SetInitialized(Value: boolean);
begin
  FInitialized := Value;
  if not Value then
    Clear;
end;

procedure TScSSLSessionInfo.SetMasterSecret(const Value: TBytes);
begin
  SetLength(FMasterSecret, Length(Value));
  if Length(Value) > 0 then
    Move(Value[0], FMasterSecret[0], Length(Value));
end;

{ TScLayersHelper }

class function TScLayersHelper.FindContentType(Value: byte): TScContentType;
begin
  // sorted by frequency of use
  if Value = CONTENT_TYPE_CODES[ctApplicationData] then
    Result := ctApplicationData
  else
  if Value = CONTENT_TYPE_CODES[ctHandshake] then
    Result := ctHandshake
  else
  if Value = CONTENT_TYPE_CODES[ctChangeCipherSpec] then
    Result := ctChangeCipherSpec
  else
  if Value = CONTENT_TYPE_CODES[ctAlert] then
    Result := ctAlert
  else
  if Value = CONTENT_TYPE_CODES[ctHeartBeat] then
    Result := ctHeartBeat
  else
    Result := ctUnknown;
end;

class function TScLayersHelper.FindHandshakeType(Value: byte): TScHandshakeType;
const
  HANDSHAKE_TYPES: array[0..255] of TScHandshakeType = (
    htHelloRequest{0}, htClientHello{1}, htServerHello{2}, htNothing{3}, htNewSessionTicket{4},
    htEndOfEarlyData{5}, htNothing, htNothing, htEncryptedExtensions{8}, htNothing,
    htNothing, htCertificate{11}, htServerKeyExchange{12}, htCertificateRequest{13}, htServerHelloDone{14},
    htCertificateVerify{15}, htClientKeyExchange{16}, htNothing, htNothing, htNothing,
    htFinished{20}, htNothing, htNothing, htNothing, htKeyUpdate{24},
    htNothing, htNothing, htNothing, htNothing, htNothing,                                                        //25-29
    htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, //30-39
    htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, //40-49
    htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, //50-59
    htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, //60-69
    htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, //70-79
    htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, //80-89
    htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, //90-99
    htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, //100-109
    htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, //110-119
    htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, //120-129
    htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, //130-139
    htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, //140-149
    htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, //150-159
    htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, //160-169
    htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, //170-179
    htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, //180-189
    htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, //190-199
    htNothing, htShuttdownSent{201}, htShuttdownRecv{202}, htShuttedDown{203}, htChangeCipherSpec{204},           //200-204
    htNothing, htNothing, htNothing, htNothing, htNothing,                                                        //205-209
    htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, //210-219
    htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, //220-229
    htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, //230-239
    htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, htNothing, //240-249
    htNothing, htNothing, htNothing, htNothing, htMessageHash{254}, htNothing //250-255
  );
begin
  Result := HANDSHAKE_TYPES[Value];
end;

{ FFDHE PRIME}

function FFDHE_PRIME(FFGType: TScFiniteFieldGroupType): TBigInteger;
begin
  case FFGType of
    ffDHE2048:
      Result := FFDHE_PRIME_2048;
    ffDHE3072:
      Result := FFDHE_PRIME_3072;
    ffDHE4096:
      Result := FFDHE_PRIME_4096;
    ffDHE6144:
      Result := FFDHE_PRIME_6144;
    ffDHE8192:
      Result := FFDHE_PRIME_8192;
    else begin
      Result := FFDHE_PRIME_2048;
      Assert(False);
    end;
  end;
end;

procedure Init_FFDHE_PRIME;
var
  sb: StringBuilder;
begin
  FFDHE_G := TBigInteger.Create(2);

  sb := StringBuilder.Create;
  try
    sb.Append('FFFFFFFFFFFFFFFFADF85458A2BB4A9AAFDC5620273D3CF1');
    sb.Append('D8B9C583CE2D3695A9E13641146433FBCC939DCE249B3EF9');
    sb.Append('7D2FE363630C75D8F681B202AEC4617AD3DF1ED5D5FD6561');
    sb.Append('2433F51F5F066ED0856365553DED1AF3B557135E7F57C935');
    sb.Append('984F0C70E0E68B77E2A689DAF3EFE8721DF158A136ADE735');
    sb.Append('30ACCA4F483A797ABC0AB182B324FB61D108A94BB2C8E3FB');
    sb.Append('B96ADAB760D7F4681D4F42A3DE394DF4AE56EDE76372BB19');
    sb.Append('0B07A7C8EE0A6D709E02FCE1CDF7E2ECC03404CD28342F61');
    sb.Append('9172FE9CE98583FF8E4F1232EEF28183C3FE3B1B4C6FAD73');
    sb.Append('3BB5FCBC2EC22005C58EF1837D1683B2C6F34A26C1B2EFFA');
    sb.Append('886B423861285C97FFFFFFFFFFFFFFFF');

    FFDHE_PRIME_2048 := TBigInteger.Create(string(sb.ToString), 16);
  finally
    sb.Free;
  end;

  sb := StringBuilder.Create;
  try
    sb.Append('FFFFFFFFFFFFFFFFADF85458A2BB4A9AAFDC5620273D3CF1');
    sb.Append('D8B9C583CE2D3695A9E13641146433FBCC939DCE249B3EF9');
    sb.Append('7D2FE363630C75D8F681B202AEC4617AD3DF1ED5D5FD6561');
    sb.Append('2433F51F5F066ED0856365553DED1AF3B557135E7F57C935');
    sb.Append('984F0C70E0E68B77E2A689DAF3EFE8721DF158A136ADE735');
    sb.Append('30ACCA4F483A797ABC0AB182B324FB61D108A94BB2C8E3FB');
    sb.Append('B96ADAB760D7F4681D4F42A3DE394DF4AE56EDE76372BB19');
    sb.Append('0B07A7C8EE0A6D709E02FCE1CDF7E2ECC03404CD28342F61');
    sb.Append('9172FE9CE98583FF8E4F1232EEF28183C3FE3B1B4C6FAD73');
    sb.Append('3BB5FCBC2EC22005C58EF1837D1683B2C6F34A26C1B2EFFA');
    sb.Append('886B4238611FCFDCDE355B3B6519035BBC34F4DEF99C0238');
    sb.Append('61B46FC9D6E6C9077AD91D2691F7F7EE598CB0FAC186D91C');
    sb.Append('AEFE130985139270B4130C93BC437944F4FD4452E2D74DD3');
    sb.Append('64F2E21E71F54BFF5CAE82AB9C9DF69EE86D2BC522363A0D');
    sb.Append('ABC521979B0DEADA1DBF9A42D5C4484E0ABCD06BFA53DDEF');
    sb.Append('3C1B20EE3FD59D7C25E41D2B66C62E37FFFFFFFFFFFFFFFF');

    FFDHE_PRIME_3072 := TBigInteger.Create(string(sb.ToString), 16);
  finally
    sb.Free;
  end;

  sb := StringBuilder.Create;
  try
    sb.Append('FFFFFFFFFFFFFFFFADF85458A2BB4A9AAFDC5620273D3CF1');
    sb.Append('D8B9C583CE2D3695A9E13641146433FBCC939DCE249B3EF9');
    sb.Append('7D2FE363630C75D8F681B202AEC4617AD3DF1ED5D5FD6561');
    sb.Append('2433F51F5F066ED0856365553DED1AF3B557135E7F57C935');
    sb.Append('984F0C70E0E68B77E2A689DAF3EFE8721DF158A136ADE735');
    sb.Append('30ACCA4F483A797ABC0AB182B324FB61D108A94BB2C8E3FB');
    sb.Append('B96ADAB760D7F4681D4F42A3DE394DF4AE56EDE76372BB19');
    sb.Append('0B07A7C8EE0A6D709E02FCE1CDF7E2ECC03404CD28342F61');
    sb.Append('9172FE9CE98583FF8E4F1232EEF28183C3FE3B1B4C6FAD73');
    sb.Append('3BB5FCBC2EC22005C58EF1837D1683B2C6F34A26C1B2EFFA');
    sb.Append('886B4238611FCFDCDE355B3B6519035BBC34F4DEF99C0238');
    sb.Append('61B46FC9D6E6C9077AD91D2691F7F7EE598CB0FAC186D91C');
    sb.Append('AEFE130985139270B4130C93BC437944F4FD4452E2D74DD3');
    sb.Append('64F2E21E71F54BFF5CAE82AB9C9DF69EE86D2BC522363A0D');
    sb.Append('ABC521979B0DEADA1DBF9A42D5C4484E0ABCD06BFA53DDEF');
    sb.Append('3C1B20EE3FD59D7C25E41D2B669E1EF16E6F52C3164DF4FB');
    sb.Append('7930E9E4E58857B6AC7D5F42D69F6D187763CF1D55034004');
    sb.Append('87F55BA57E31CC7A7135C886EFB4318AED6A1E012D9E6832');
    sb.Append('A907600A918130C46DC778F971AD0038092999A333CB8B7A');
    sb.Append('1A1DB93D7140003C2A4ECEA9F98D0ACC0A8291CDCEC97DCF');
    sb.Append('8EC9B55A7F88A46B4DB5A851F44182E1C68A007E5E655F6A');
    sb.Append('FFFFFFFFFFFFFFFF');

    FFDHE_PRIME_4096 := TBigInteger.Create(string(sb.ToString), 16);
  finally
    sb.Free;
  end;

  sb := StringBuilder.Create;
  try
    sb.Append('FFFFFFFFFFFFFFFFADF85458A2BB4A9AAFDC5620273D3CF1');
    sb.Append('D8B9C583CE2D3695A9E13641146433FBCC939DCE249B3EF9');
    sb.Append('7D2FE363630C75D8F681B202AEC4617AD3DF1ED5D5FD6561');
    sb.Append('2433F51F5F066ED0856365553DED1AF3B557135E7F57C935');
    sb.Append('984F0C70E0E68B77E2A689DAF3EFE8721DF158A136ADE735');
    sb.Append('30ACCA4F483A797ABC0AB182B324FB61D108A94BB2C8E3FB');
    sb.Append('B96ADAB760D7F4681D4F42A3DE394DF4AE56EDE76372BB19');
    sb.Append('0B07A7C8EE0A6D709E02FCE1CDF7E2ECC03404CD28342F61');
    sb.Append('9172FE9CE98583FF8E4F1232EEF28183C3FE3B1B4C6FAD73');
    sb.Append('3BB5FCBC2EC22005C58EF1837D1683B2C6F34A26C1B2EFFA');
    sb.Append('886B4238611FCFDCDE355B3B6519035BBC34F4DEF99C0238');
    sb.Append('61B46FC9D6E6C9077AD91D2691F7F7EE598CB0FAC186D91C');
    sb.Append('AEFE130985139270B4130C93BC437944F4FD4452E2D74DD3');
    sb.Append('64F2E21E71F54BFF5CAE82AB9C9DF69EE86D2BC522363A0D');
    sb.Append('ABC521979B0DEADA1DBF9A42D5C4484E0ABCD06BFA53DDEF');
    sb.Append('3C1B20EE3FD59D7C25E41D2B669E1EF16E6F52C3164DF4FB');
    sb.Append('7930E9E4E58857B6AC7D5F42D69F6D187763CF1D55034004');
    sb.Append('87F55BA57E31CC7A7135C886EFB4318AED6A1E012D9E6832');
    sb.Append('A907600A918130C46DC778F971AD0038092999A333CB8B7A');
    sb.Append('1A1DB93D7140003C2A4ECEA9F98D0ACC0A8291CDCEC97DCF');
    sb.Append('8EC9B55A7F88A46B4DB5A851F44182E1C68A007E5E0DD902');
    sb.Append('0BFD64B645036C7A4E677D2C38532A3A23BA4442CAF53EA6');
    sb.Append('3BB454329B7624C8917BDD64B1C0FD4CB38E8C334C701C3A');
    sb.Append('CDAD0657FCCFEC719B1F5C3E4E46041F388147FB4CFDB477');
    sb.Append('A52471F7A9A96910B855322EDB6340D8A00EF092350511E3');
    sb.Append('0ABEC1FFF9E3A26E7FB29F8C183023C3587E38DA0077D9B4');
    sb.Append('763E4E4B94B2BBC194C6651E77CAF992EEAAC0232A281BF6');
    sb.Append('B3A739C1226116820AE8DB5847A67CBEF9C9091B462D538C');
    sb.Append('D72B03746AE77F5E62292C311562A846505DC82DB854338A');
    sb.Append('E49F5235C95B91178CCF2DD5CACEF403EC9D1810C6272B04');
    sb.Append('5B3B71F9DC6B80D63FDD4A8E9ADB1E6962A69526D43161C1');
    sb.Append('A41D570D7938DAD4A40E329CD0E40E65FFFFFFFFFFFFFFFF');

    FFDHE_PRIME_6144 := TBigInteger.Create(string(sb.ToString), 16);
  finally
    sb.Free;
  end;

  sb := StringBuilder.Create;
  try
    sb.Append('FFFFFFFFFFFFFFFFADF85458A2BB4A9AAFDC5620273D3CF1');
    sb.Append('D8B9C583CE2D3695A9E13641146433FBCC939DCE249B3EF9');
    sb.Append('7D2FE363630C75D8F681B202AEC4617AD3DF1ED5D5FD6561');
    sb.Append('2433F51F5F066ED0856365553DED1AF3B557135E7F57C935');
    sb.Append('984F0C70E0E68B77E2A689DAF3EFE8721DF158A136ADE735');
    sb.Append('30ACCA4F483A797ABC0AB182B324FB61D108A94BB2C8E3FB');
    sb.Append('B96ADAB760D7F4681D4F42A3DE394DF4AE56EDE76372BB19');
    sb.Append('0B07A7C8EE0A6D709E02FCE1CDF7E2ECC03404CD28342F61');
    sb.Append('9172FE9CE98583FF8E4F1232EEF28183C3FE3B1B4C6FAD73');
    sb.Append('3BB5FCBC2EC22005C58EF1837D1683B2C6F34A26C1B2EFFA');
    sb.Append('886B4238611FCFDCDE355B3B6519035BBC34F4DEF99C0238');
    sb.Append('61B46FC9D6E6C9077AD91D2691F7F7EE598CB0FAC186D91C');
    sb.Append('AEFE130985139270B4130C93BC437944F4FD4452E2D74DD3');
    sb.Append('64F2E21E71F54BFF5CAE82AB9C9DF69EE86D2BC522363A0D');
    sb.Append('ABC521979B0DEADA1DBF9A42D5C4484E0ABCD06BFA53DDEF');
    sb.Append('3C1B20EE3FD59D7C25E41D2B669E1EF16E6F52C3164DF4FB');
    sb.Append('7930E9E4E58857B6AC7D5F42D69F6D187763CF1D55034004');
    sb.Append('87F55BA57E31CC7A7135C886EFB4318AED6A1E012D9E6832');
    sb.Append('A907600A918130C46DC778F971AD0038092999A333CB8B7A');
    sb.Append('1A1DB93D7140003C2A4ECEA9F98D0ACC0A8291CDCEC97DCF');
    sb.Append('8EC9B55A7F88A46B4DB5A851F44182E1C68A007E5E0DD902');
    sb.Append('0BFD64B645036C7A4E677D2C38532A3A23BA4442CAF53EA6');
    sb.Append('3BB454329B7624C8917BDD64B1C0FD4CB38E8C334C701C3A');
    sb.Append('CDAD0657FCCFEC719B1F5C3E4E46041F388147FB4CFDB477');
    sb.Append('A52471F7A9A96910B855322EDB6340D8A00EF092350511E3');
    sb.Append('0ABEC1FFF9E3A26E7FB29F8C183023C3587E38DA0077D9B4');
    sb.Append('763E4E4B94B2BBC194C6651E77CAF992EEAAC0232A281BF6');
    sb.Append('B3A739C1226116820AE8DB5847A67CBEF9C9091B462D538C');
    sb.Append('D72B03746AE77F5E62292C311562A846505DC82DB854338A');
    sb.Append('E49F5235C95B91178CCF2DD5CACEF403EC9D1810C6272B04');
    sb.Append('5B3B71F9DC6B80D63FDD4A8E9ADB1E6962A69526D43161C1');
    sb.Append('A41D570D7938DAD4A40E329CCFF46AAA36AD004CF600C838');
    sb.Append('1E425A31D951AE64FDB23FCEC9509D43687FEB69EDD1CC5E');
    sb.Append('0B8CC3BDF64B10EF86B63142A3AB8829555B2F747C932665');
    sb.Append('CB2C0F1CC01BD70229388839D2AF05E454504AC78B758282');
    sb.Append('2846C0BA35C35F5C59160CC046FD8251541FC68C9C86B022');
    sb.Append('BB7099876A460E7451A8A93109703FEE1C217E6C3826E52C');
    sb.Append('51AA691E0E423CFC99E9E31650C1217B624816CDAD9A95F9');
    sb.Append('D5B8019488D9C0A0A1FE3075A577E23183F81D4A3F2FA457');
    sb.Append('1EFC8CE0BA8A4FE8B6855DFE72B0A66EDED2FBABFBE58A30');
    sb.Append('FAFABE1C5D71A87E2F741EF8C1FE86FEA6BBFDE530677F0D');
    sb.Append('97D11D49F7A8443D0822E506A9F4614E011E2A94838FF88C');
    sb.Append('D68C8BB7C5C6424CFFFFFFFFFFFFFFFF');

    FFDHE_PRIME_8192 := TBigInteger.Create(string(sb.ToString), 16);
  finally
    sb.Free;
  end;
end;

initialization
{$IFDEF DEBUG}
  Assert((integer(High(TScECName)) + 1 + integer(High(TScFiniteFieldGroupType)) + 1) = integer(High(TScKExNamedGroupType)) + 1);
{$ENDIF}
  Init_FFDHE_PRIME;

finalization
  FFDHE_G.Free;
  FFDHE_PRIME_2048.Free;
  FFDHE_PRIME_3072.Free;
  FFDHE_PRIME_4096.Free;
  FFDHE_PRIME_6144.Free;
  FFDHE_PRIME_8192.Free;

end.
