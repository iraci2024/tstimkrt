
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF DAC}
{$I SB.inc}
unit ScBridge;
{$ENDIF}

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
  Windows, Registry,
{$ENDIF}
  {$IFDEF VER16P}System.Types,{$ELSE}Types,{$ENDIF}
{$IFDEF POSIX}
  Posix.SysSocket, Posix.Unistd, Posix.Stdio,
{$ENDIF}
{$IFDEF UNIX}
  Sockets,
{$ENDIF}
  {$IFDEF NEXTGEN}Generics.Collections,{$ELSE}Contnrs,{$ENDIF}
  Classes, SysUtils, SyncObjs,
{$IFNDEF SBRIDGE}
  CLRClasses, CRTypes, CRFunctions,
  CRHashAlgorithm, CRSymmetricAlgorithm, CRHMAC, CRCipher, CRRNG, CRBigInteger,
{$IFNDEF UNIDACPRO}
  TdsSSLConsts, TdsUtils,
  TdsASN1, TdsOids, TdsCertificateConsts, TdsCertificateExts, TdsEC25519;
{$ELSE}
  TdsSSLConstsUni, TdsUtilsUni,
  TdsASN1Uni, TdsOidsUni, TdsCertificateConstsUni, TdsCertificateExtsUni, TdsEC25519Uni;
{$ENDIF}
{$ELSE}
  ScCLRClasses, ScTypes, ScFunctions,
  ScHashAlgorithm, ScSymmetricAlgorithm, ScHMAC, ScCipher, ScRNG, ScBigInteger,
  ScConsts, ScSSHUtils, ScUtils,
  ScASN1, ScOids, ScCertificateConsts, ScCertificateExts, ScEC25519;
{$ENDIF}

{$IFNDEF DAC}
{$I SecureBridgeVer.inc}
{$ENDIF}

{$IFDEF FPC}
{$IFDEF MSWINDOWS}
const
  HKEY_CURRENT_USER = NativeUint($80000001);
{$ENDIF}
{$ENDIF}

const
  Def_Path = '.';
  Def_KeyExt = 'key';
  Def_UserExt = 'usr';
  Def_CertificateExt = 'crt';
  Def_CRLExt = 'crl';

type
  TKeyPurpose = (kpEncKey, kpIV, kpMacKey);

type
  // Diffie-Hellman parameters
  TScDHData = record
    P: TBigInteger;
    G: TBigInteger;
    Y: TBigInteger; // y = g^X mod p; X - private, Y - public
    X: TBigInteger;
  end;

  // DSA Key parameters
  TScDSAData = record
    // private Key
    X: TBigInteger;

    // public Key
    G: TBigInteger;
    P: TBigInteger;
    Q: TBigInteger;
    Y: TBigInteger;
  end;

  // RSA Key parameters
  TScRSAData = record
    // private Key
    D: TBigInteger; // private_exponent
    P: TBigInteger;
    Q: TBigInteger;
    Qinv: TBigInteger;
    DP: TBigInteger;
    DQ: TBigInteger;

    // public Key
    PublicExponent: TBigInteger; //e
    PublicModulus: TBigInteger;  //n
  end;

  TSizeAndPosBlobRSA = record
    ExponentLen: cardinal;
    ModulusLen: cardinal;
    Prime1Len: cardinal;
    Prime2Len: cardinal;
    Exponent1Len: cardinal;
    Exponent2Len: cardinal;
    CoefficientLen: cardinal;
    PrivateExponentLen: cardinal;

    ExponentPos: cardinal;
    ModulusPos: cardinal;
    Prime1Pos: cardinal;
    Prime2Pos: cardinal;
    Exponent1Pos: cardinal;
    Exponent2Pos: cardinal;
    CoefficientPos: cardinal;
    PrivateExponentPos: cardinal;

    PrivByteLen: cardinal;
    PubByteLen: cardinal;
  end;

  TSizeAndPosBlobDSA = record
    PLen: cardinal;
    QLen: cardinal;
    GLen: cardinal;
    XLen: cardinal; // private
    YLen: cardinal; // public
    PgenCounterLen: cardinal;
    SeedLen: cardinal;

    PPos: cardinal;
    QPos: cardinal;
    GPos: cardinal;
    XYPos: cardinal; // private & public
    XPgenCounterPos: cardinal;
    YPgenCounterPos: cardinal;
    XSeedPos: cardinal;
    YSeedPos: cardinal;

    PrivByteLen: cardinal;
    PubByteLen: cardinal;
  end;

  // Elliptic Curves

  TScCustomECCryptography = class;
  TScECCryptographyClass = class of TScCustomECCryptography;

  TScECParameters = record
    ECName: TScECName;
    CryptographyClass: TScECCryptographyClass;
    Size: integer;
    p: string;
    a, b: string;
    Gx, Gy: string;
    n: string;
    OID: string;
    SSHName: string;
  end;

  T32BytesField = class(TPersistent)
  private
    BytesField: T32Bytes;
  public
    constructor Create; overload;
    constructor Create(const Data: TBytes); overload;

    procedure Assign(Source: TPersistent); override;
    function Equal(Value: T32BytesField): boolean;

    function GetBytes: TBytes;
  end;

  TScECPointClass = class of TScCustomECPoint;

  TScCustomECPoint = class
  public
    procedure Assign(Source: TScCustomECPoint); virtual; abstract;
    function Equals(ECPoint: TScCustomECPoint): boolean; reintroduce; virtual; abstract;
  end;

  TScECPointBI = class (TScCustomECPoint)
  public
    X, Y, Z: TBigInteger;
    destructor Destroy; override;
    procedure Assign(Source: TScCustomECPoint); override;
    function Equals(ECPoint: TScCustomECPoint): boolean; reintroduce; override;
  end;

  TScECPointX25519 = class(TScCustomECPoint)
  public
    PointField: T32Bytes;
    procedure Assign(Source: TScCustomECPoint); override;
    function Equals(ECPoint: TScCustomECPoint): boolean; reintroduce; override;
  end;

  // EC Key  parameters
  TScECData = record
    ECName: TScECName;
    ECCryptography: TScCustomECCryptography;
    PublicPoint: TScCustomECPoint;
    PrivateKoef: TPersistent;
  end;

  TScCustomECCryptography = class
  protected
    FIsNamed: boolean;
    FSize: integer;
    FIsSSHFormat: boolean;

    function BigIntToHexBuf(bi: TBigInteger): TBytes;
    procedure GeneratePrivateKey(var ECData: TScECData; ARandom: IScRandom = nil); virtual; abstract;
    function GetBitCount: integer; virtual; abstract;

  public
    constructor Create; overload; virtual;
    constructor Create(const ECParameters: TScECParameters); overload; virtual;

    procedure Assign(Source: TScCustomECCryptography); virtual;
    function Equals(ECCryptography: TScCustomECCryptography): boolean; reintroduce; virtual; abstract;
    function PublicPointClass: TScECPointClass; virtual; abstract;
    function PrivateKoefClass: TPersistentClass; virtual; abstract;

    function MulPoint(P1: TScCustomECPoint; Koef: TObject): TScCustomECPoint; virtual; abstract;
    function DecodePointFromOctetString(const Buf: TBytes; Offset, Count: integer): TScCustomECPoint; virtual; abstract;
    function EncodePointToOctetString(P: TScCustomECPoint): TBytes; virtual; abstract;

    function SignHash(const Hash: TBytes; PrivateKoef: TObject; PublicPoint: TScCustomECPoint): TBytes; virtual; abstract;
    function VerifyHash(const Hash, Sign: TBytes; PublicPoint: TScCustomECPoint): boolean; virtual; abstract;

    property Size: integer read FSize;
    property BitCount: integer read GetBitCount;
    property IsSSHFormat: boolean read FIsSSHFormat write FIsSSHFormat;
  end;

  TScECCryptographyBI = class(TScCustomECCryptography)
  private
    FModP: TBigInteger;
    FA: TBigInteger;
    FB: TBigInteger;
    FN: TBigInteger;
    FG: TScECPointBI;
    FSeed: TBytes;
    FCofactor: integer;

    class function Compute_wNAF(Scalar: TBigInteger): TIntArr;
    procedure ClonePoint(P: TScECPointBI; Res: TScECPointBI);
    procedure SetToInfinity(P: TScECPointBI);

  protected
    function GenerateRandomKoef(ARandom: IScRandom = nil): TBigInteger;
    procedure GeneratePrivateKey(var ECData: TScECData; ARandom: IScRandom = nil); override;
    function GetBitCount: integer; override;

    procedure Prepare; virtual; abstract;
    procedure Invert(P: TScECPointBI); virtual; abstract;
    procedure AddPoints(P1, P2: TScECPointBI; Res: TScECPointBI); virtual; abstract;
    procedure DblPoint(P: TScECPointBI; Res: TScECPointBI); virtual; abstract;
    procedure ToAffineCoord(P: TScECPointBI); virtual; abstract;
    function NeedConvertToAffineCoordOnPrecomp: boolean; virtual;

    function IsPointBelongTo(Point: TScECPointBI): boolean; virtual; abstract;

  public
    constructor Create; overload; override;
    constructor Create(const ECParameters: TScECParameters); overload; override;
    destructor Destroy; override;

    procedure Assign(Source: TScCustomECCryptography); override;
    function Equals(ECCryptography: TScCustomECCryptography): boolean; reintroduce; override;
    function PublicPointClass: TScECPointClass; override;
    function PrivateKoefClass: TPersistentClass; override;

    function MulPoint(P1: TScCustomECPoint; Koef: TObject): TScCustomECPoint; override;
    function DecodePointFromOctetString(const Buf: TBytes; Offset, Count: integer): TScCustomECPoint; override;
    function EncodePointToOctetString(P: TScCustomECPoint): TBytes; override;

    function SignHash(const Hash: TBytes; PrivateKoef: TObject; PublicPoint: TScCustomECPoint): TBytes; override;
    function VerifyHash(const Hash, Sign: TBytes; PublicPoint: TScCustomECPoint): boolean; override;
  end;

  TScECCryptographyFp = class(TScECCryptographyBI)
  protected
    procedure Prepare; override;
    procedure Invert(P: TScECPointBI); override;
    procedure AddPoints(P1, P2: TScECPointBI; Res: TScECPointBI); override;
    procedure DblPoint(P: TScECPointBI; Res: TScECPointBI); override;
    procedure ToAffineCoord(P: TScECPointBI); override;
    function IsPointBelongTo(Point: TScECPointBI): boolean; override;
  end;

  TScECCryptographyF2m = class(TScECCryptographyBI)
  private
    FABitCount: integer;

    procedure AddPointsGF2m(P1, P2: TScECPointBI; Res: TScECPointBI);
    procedure DblPointGF2m(P: TScECPointBI; Res: TScECPointBI);
    procedure AddPointsGF2mLD(P1, P2: TScECPointBI; Res: TScECPointBI);
    procedure DblPointGF2mLD(P: TScECPointBI; Res: TScECPointBI);

  protected
    procedure Prepare; override;
    procedure Invert(P: TScECPointBI); override;
    procedure AddPoints(P1, P2: TScECPointBI; Res: TScECPointBI); override;
    procedure DblPoint(P: TScECPointBI; Res: TScECPointBI); override;
    procedure ToAffineCoord(P: TScECPointBI); override;
    function NeedConvertToAffineCoordOnPrecomp: boolean; override;
    function IsPointBelongTo(Point: TScECPointBI): boolean; override;
  end;

  TScX25519PreHashType = (phPure, phHash, phCtx);

  TScECCryptographyX25519 = class(TScCustomECCryptography)
  protected
    FPreHashType: TScX25519PreHashType;

    function PublicFromPrivate(Koef: T32BytesField): TScECPointX25519;
    procedure GeneratePrivateKey(var ECData: TScECData; ARandom: IScRandom = nil); override;
    function GetBitCount: integer; override;

  public
    constructor Create; overload; override;
    constructor Create(const ECParameters: TScECParameters); overload; override;

    procedure Assign(Source: TScCustomECCryptography); override;
    function Equals(ECCryptography: TScCustomECCryptography): boolean; reintroduce; override;
    function PublicPointClass: TScECPointClass; override;
    function PrivateKoefClass: TPersistentClass; override;

    function MulPoint(P1: TScCustomECPoint; Koef: TObject): TScCustomECPoint; override;
    function DecodePointFromOctetString(const Buf: TBytes; Offset, Count: integer): TScCustomECPoint; override;
    function EncodePointToOctetString(P: TScCustomECPoint): TBytes; override;

    function SignHash(const Data: TBytes; PrivateKoef: TObject; PublicPoint: TScCustomECPoint): TBytes; override;
    function VerifyHash(const Data, Sign: TBytes; PublicPoint: TScCustomECPoint): boolean; override;
  end;

  TScKeyDerivationProcessor = class
  private
    // function BCrypt(const Password: string; const Salt: TBytes; Iterations: integer): TBytes;
    class function BCryptHash(const Key, Salt: TBytes): TBytes;
    class function BCryptGenBlock(const HashedPassphrase, Salt: TBytes; Counter: integer): TBytes;
  public
    class function PBKDF1FromPassword(const HashAlgorithm: TScHashAlgorithm;
      const Password: string; const Salt: TBytes;
      KeyLen: integer; Iterations: integer; IETFFormat: boolean): TBytes;
    class function PBKDF2FromPassword(const Password: string; const Salt: TBytes;
      KeyLen: integer; Iterations: integer): TBytes;
    class function PKCS12KeyDeriveFromPassword(const HashAlgorithm: TScHashAlgorithm;
      const Password: string; const Salt: TBytes;
      KeyPurpose: TKeyPurpose; Iterations: integer;
      KeyLen: integer): TBytes;

    class function OpenSSHBCrypt(const Password: string; const Salt: TBytes; Iterations: integer; OutSize: integer): TBytes;

    class function CreateCipherByOId(const OId: string; const Params: TBytes;
      const Password: string): TSymmetricAlgorithm;
  end;

type
  TScKeyFormat = (kfDefault, kfDER, kfPKCS8, kfPKCS8enc, kfIETF, kfPKCS1);
  TScKeyExFormat = (kefDefault, kefPKCS8, kefOpenSSL, kefPVK);
  TScCertificateEncoding = (cfCER, cfPEM);

  TScStorage = class;
  TScStorageClass = class of TScStorage;
  TScStorageItem = class;
  TScStorageItemClass = class of TScStorageItem;
  TScStorageList = class;
  TScKeyList = class;
  TScUserList = class;
  TScCertificateList = class;
  TScCRLList = class;
  TScPKCS12Processor = class;

  TScCertificate = class;
  TScCRL = class;

  TScStorageItem = class(TPersistent)
  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FStorageList: TScStorageList;
    FLock: TCriticalSection;
    FItemName: string;
    FReady: boolean;
    FLoading: boolean;

    procedure SetReady(const Value: boolean);
    procedure SetItemName(const Value: string); virtual;
    procedure SetStorageList(Value: TScStorageList); virtual;
    procedure SaveToStorageIfPossible; virtual;

    procedure InternalAssign(Source: TScStorageItem); virtual;
    procedure FreeData; virtual;
    function Clone: TScStorageItem;
    procedure CheckAvailable;

    property ItemName: string read FItemName write SetItemName;

  public
    constructor Create(AStorageList: TScStorageList = nil); virtual;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    function Equals(Item: TScStorageItem): boolean; reintroduce; virtual;

    property StorageList: TScStorageList read FStorageList write SetStorageList;
    property Ready: boolean read FReady write SetReady; // Item is loaded
  end;

  // TScKey is the base class type that can store, load and save public or private Key (RSA, DSA, EC)
  TScKey = class(TScStorageItem)
  private
    FList: TCRList;
    FAlgorithm: TScAsymmetricAlgorithm;
    FPSSParams: TScPSSParams;
    FOAEPParams: TScOAEPParams;

    function GetKeyList: TScKeyList;
    procedure SetKeyList(Value: TScKeyList);

    procedure RaiseError;
    procedure CheckReady;
    class procedure FindRandomStrongPrime(PrimeBits, OrderBits: integer;
      ARandom: IScRandom; var Prime, Order: TBigInteger);
    class function FindRandomGenerator(Order, Modulo: TBigInteger;
      ARandom: IScRandom): TBigInteger;
    procedure GenerateDSA(const BitCount: integer; ARandom: IScRandom);
    procedure GenerateRSA(const BitCount: integer; ARandom: IScRandom);
    procedure CalcY;
    procedure Calc_DP_DQ;
    function RSAPrivateModPow(const Data: TBytes): TBytes;
    function RSAPublicModPow(const Data: TBytes): TBytes;
    procedure GetAlgAsnId(const HashAlg: TScHashAlgorithm; out AsnId: PByteArray; out AsnIdLen: integer);

    procedure ParsePuTTYKey(KeyStrings: TStringList; const Password: string; out Comment: string);
    function ParsePEMKey(KeyStrings: TStringList; const Footer: string; out Comment: string): TBytes;
    function ParsePrivatePEMKey(KeyStrings: TStringList; const Password: string; const Footer: string; out Comment: string): TBytes;
    procedure CryptPrivatePEMKey(Stream: TStream; const Password: string;
      Algorithm: TScSymmetricAlgorithm; var KeyData: TBytes);
    function ParseOpenSSHPublicKey(const Key: string; out Comment: string): TBytes;
    procedure WriteOpenSSHPublicKey(Stream: TStream; const KeyData: TBytes; const Comment: string);

    function DetectPKCS1Format(const KeyData: TBytes; out Alg: TScAsymmetricAlgorithm; out PublicKeyOnly: boolean): boolean;
    procedure DecodeKeyFromPKCS1Format(const KeyData: TBytes; Alg: TScAsymmetricAlgorithm; PublicKeyOnly: boolean);
    procedure DecodePublicKeyFromOpenSSHFormat(const KeyData: TBytes);
    procedure DecodePrivateKeyFromOpenSSHFormat(const KeyData: TBytes; const Password: string);
    procedure DecodePrivateKeyFromPKCS8Format(const KeyData: TBytes);
    procedure DecodePrivateKeyFromPKCS8EncFormat(const KeyData: TBytes; const Password: string);
    procedure DecodePrivateKeyFromIETFFormat(const KeyData: TBytes; const Password: string);
    procedure DecodePublicKeyFromIETFFormat(const KeyData: TBytes);
    procedure DecodeKeyFromECFormat(const KeyData, ECParamsData: TBytes);
    procedure DecodeKeyFromX25519Format(const KeyData: TBytes; PreHashType: TScX25519PreHashType);
    procedure DecodeX25519Params(const Data: TBytes; PreHashType: TScX25519PreHashType);
    procedure DecodeECParams(const Data: TBytes); overload;
    procedure DecodeECParams(ParamsLexemInfo: TScLexemInfo); overload;
    function EncodeX25519Params: TBytes;
    function EncodeECParams: TBytes;
    function EncodeKeyToPKCS1Format(PublicKeyOnly: boolean): TBytes;
    function EncodePublicKeyToOpenSSHFormat: TBytes;
    function EncodePrivateKeyToPKCS8Format: TBytes;
    function EncodePrivateKeyToPKCS8EncFormat(const Password: string; Algo: TScSymmetricAlgorithm): TBytes;
    function EncodePrivateKeyToIETFFormat(const Password: string): TBytes;
    function EncodePublicKeyToIETFFormat: TBytes;
    procedure DecodeKeyFromPVKFormat(const Data: TBytes);
    function SetSizeAndPositionPVKBlobRSA(BitLen: cardinal): TSizeAndPosBlobRSA;
    function SetSizeAndPositionPVKBlobDSA(BitLen: cardinal): TSizeAndPosBlobDSA;
    procedure DecodeKeyFromOpenSSLFormat(const KeyData: TBytes; PublicKeyOnly: boolean);
    procedure DecodeDSAKeyFromCertificate(const PublicKey, KeyParameters: TBytes);
    procedure DecodeECKeyFromCertificate(const PublicKey, KeyParameters: TBytes);
    procedure DecodeX25519KeyFromCertificate(const PublicKey: TBytes; PreHashType: TScX25519PreHashType);

  {$IFDEF MSWINDOWS}
    function EncodeRSAKeyToPVKFormat: TBytes;
    function EncodeDSAKeyToPVKFormat: TBytes;
  {$ENDIF}

    class function MGF1(const Seed: TBytes; MaskLen: integer; HashAlg: TScHashAlgorithm): TBytes;
    function EncodeWithPad(const Input: TBytes; Padding: TScPaddingMode): TBytes;
    function EncodeHashWithPad(const Hash: TBytes; HashAlg: TScHashAlgorithm; Padding: TScPaddingMode): TBytes;
    function VerifyHashWithPad(const Hash, Sign: TBytes; HashAlg: TScHashAlgorithm; Padding: TScPaddingMode): boolean;
    function DecodePad(const Input: TBytes; Padding: TScPaddingMode): TBytes;
    function DSASignHash(const Hash: TBytes): TBytes;
    function DSAVerifyHash(const Hash, Sign: TBytes): boolean;
    function RSASignHash(const Hash: TBytes; HashAlg: TScHashAlgorithm; Padding: TScPaddingMode): TBytes;
    function RSAVerifyHash(const Hash, Sign: TBytes; HashAlg: TScHashAlgorithm; Padding: TScPaddingMode): boolean;
    function ECSignHash(const Hash: TBytes): TBytes;
    function ECVerifyHash(const Hash, Sign: TBytes): boolean;

    class function CompareDSAData(const d1, d2: TScDSAData; PublicKeyOnly: boolean): boolean;
    class function CompareRSAData(const d1, d2: TScRSAData; PublicKeyOnly: boolean): boolean;
    class function CompareECData(const d1, d2: TScECData; PublicKeyOnly: boolean): boolean;
    function GetAlgorithm: TScAsymmetricAlgorithm;
    function GetKeySize: integer; // Byte count
    function GetBitCount: integer;
    function GetIsPrivate: boolean;
    function GetSSHKeyData: TBytes;
    procedure SetSSHKeyData(const Value: TBytes);
    procedure SetPSSParams(Value: TScPSSParams);
    procedure SetOAEPParams(Value: TScOAEPParams);

  protected
    FDSAData: TScDSAData;
    FRSAData: TScRSAData;
    FECData: TScECData;

    procedure RegisterClient(Client: TObject);
    procedure UnRegisterClient(Client: TObject);

    procedure SetStorageList(Value: TScStorageList); override;
    procedure InternalAssign(Source: TScStorageItem); override;
    procedure FreeData; override;

    procedure LoadKeyFromCryptoAPIFormat(const Data: TBytes;
      const KeyExFormat: TScKeyExFormat; const Alg: TScAsymmetricAlgorithm; const IsPublicKey: boolean);
    function SaveKeyToCryptoAPIFormat: TBytes;
    procedure LoadKeyFromPVKFormat(const Data: TBytes);

    procedure SaveToStorageIfPossible; override;

  public
    constructor Create(AStorageList: TScStorageList = nil); override;
    destructor Destroy; override;

    procedure ImportFrom(const FileName: string; const Password: string; out Comment: string); overload;
    procedure ImportFrom(const FileName: string; const Password: string = ''); overload;
    procedure ImportFrom(Stream: TStream; const Password: string; out Comment: string); overload;
    procedure ImportFrom(Stream: TStream; const Password: string = ''); overload;
    procedure ExportTo(const FileName: string; const PublicKeyOnly: boolean; const Password: string;
      const Algorithm: TScSymmetricAlgorithm = saTripleDES_cbc; const KeyFormat: TScKeyFormat = kfDefault; const Comment: string = ''); overload;
    procedure ExportTo(Stream: TStream; const PublicKeyOnly: boolean; const Password: string;
      const Algorithm: TScSymmetricAlgorithm = saTripleDES_cbc; const KeyFormat: TScKeyFormat = kfDefault; const Comment: string = ''); overload;

    procedure Generate(const Algorithm: TScAsymmetricAlgorithm; const BitCount: integer; ARandom: IScRandom = nil);
    procedure GenerateEC(const ECName: TScECName; ARandom: IScRandom = nil);

    procedure GetFingerprint(const HashAlg: TScHashAlgorithm; out Fingerprint: TBytes); overload;
    procedure GetFingerprint(const HashAlg: TScHashAlgorithm; out Fingerprint: string); overload;

    function Equals(Item: TScStorageItem): boolean; reintroduce; override;
    function Sign(const Data: TBytes; HashAlg: TScHashAlgorithm = haSHA1; Padding: TScPaddingMode = pmPKCS1): TBytes;
    function SignHash(const Hash: TBytes; HashAlg: TScHashAlgorithm = haSHA1; Padding: TScPaddingMode = pmPKCS1): TBytes;
    function VerifySign(const Data, Sign: TBytes; HashAlg: TScHashAlgorithm = haSHA1; Padding: TScPaddingMode = pmPKCS1): boolean;
    function VerifyHashSign(const Hash, Sign: TBytes; HashAlg: TScHashAlgorithm = haSHA1; Padding: TScPaddingMode = pmPKCS1): boolean;
    function Encrypt(const Data: TBytes; Padding: TScPaddingMode = pmPKCS2): TBytes;
    function Decrypt(const Data: TBytes; Padding: TScPaddingMode = pmPKCS2): TBytes;

    property Algorithm: TScAsymmetricAlgorithm read GetAlgorithm; // (RSA, DSA, EC)
    property BitCount: integer read GetBitCount;
    property IsPrivate: boolean read GetIsPrivate;
    property KeyName: string read FItemName write SetItemName;
    property KeyList: TScKeyList read GetKeyList write SetKeyList;

    property ECData: TScECData read FECData;
    property DSAData: TScDSAData read FDSAData;
    property RSAData: TScRSAData read FRSAData;
    property PSSParams: TScPSSParams read FPSSParams write SetPSSParams;
    property OAEPParams: TScOAEPParams read FOAEPParams write SetOAEPParams;
  end;

  TScStorageList = class
  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FStorage: TScStorage;
    FLock: TCriticalSection;
    FList: TStringList;

    function GetCount: integer;
    procedure ClearList;
    function GetItemClassType: TScStorageItemClass; virtual; abstract;
    function GetItemDefName: string; virtual; abstract;

    procedure SaveToStorage(Item: TScStorageItem);
    procedure LoadFromStorage(Item: TScStorageItem);
    procedure DeleteFromStorage(Item: TScStorageItem);
    procedure RenameInStorageIfExists(Item: TScStorageItem; const NewName: string);
    procedure RenameInStorage(Item: TScStorageItem; const NewName: string);

    procedure CheckItemName(const ItemName: string);
    function GetItem(Index: integer): TScStorageItem;
    procedure SetItem(Index: integer; Value: TScStorageItem);
    procedure RenameItem(Item: TScStorageItem; const NewName: string);
    function FindItem(const ItemName: string): TScStorageItem;
    function ItemByName(const ItemName: string): TScStorageItem;
    procedure GetItemNames(List: TStrings);

    function FindEqualItem(Item: TScStorageItem): boolean;

    property Items[Index: integer]: TScStorageItem read GetItem write SetItem;

  public
    constructor Create(AStorage: TScStorage); virtual;
    destructor Destroy; override;

    procedure Add(Item: TScStorageItem);
    function IndexOf(Item: TScStorageItem): integer;
    procedure Remove(Item: TScStorageItem);

    procedure Clear;
    procedure Refresh; // Reload from storage
    procedure Flush; virtual;

    property Count: integer read GetCount;
    property Storage: TScStorage read FStorage;
  end;

  TScKeyList = class(TScStorageList)
  protected
    function GetKey(Index: integer): TScKey;
    procedure SetKey(Index: integer; Value: TScKey);
    function GetItemClassType: TScStorageItemClass; override;
    function GetItemDefName: string; override;

  public
    procedure CheckKeyName(const KeyName: string);
    function FindKey(const KeyName: string): TScKey;
    function KeyByName(const KeyName: string): TScKey;
    procedure GetKeyNames(List: TStrings);

    property Keys[Index: integer]: TScKey read GetKey write SetKey; default;
  end;

{$IFDEF SBRIDGE}
  TScCheckUserPass = procedure(Sender: TObject; ClientInfo: TScSSHClientInfo; const Password: string; var Accept: boolean) of object;
  TScCheckUserKey = procedure(Sender: TObject; ClientInfo: TScSSHClientInfo; Key: TScKey; var Accept: boolean) of object;
{$ENDIF}

  TScStorage = class(TComponent)
  private
    FLock: TCriticalSection;
    FKeys: TScKeyList;
    FUsers: TScUserList;
    FCertificates: TScCertificateList;
    FCRLs: TScCRLList;
  {$IFDEF SBRIDGE}
    FOnCheckUserPass: TScCheckUserPass;
    FOnCheckUserKey: TScCheckUserKey;
  {$ENDIF}
    class function CreateCipher(const Algorithm: TScSymmetricAlgorithm; const Password: string): TSymmetricAlgorithm;

  protected
    FReadOnly: boolean;
    FStoreUserPassword: boolean;
    procedure SetReadOnly(const Value: Boolean);
    function GetKeys: TScKeyList; virtual;
    function GetUsers: TScUserList; virtual;
    function GetCertificates: TScCertificateList; virtual;
    function GetCRLs: TScCRLList; virtual;
    procedure Invalidate; virtual;

    procedure CheckReadOnly;
    procedure Load(Item: TScStorageItem);
    procedure InternalLoad(Item: TScStorageItem); virtual; abstract;
    procedure Save(Item: TScStorageItem);
    procedure InternalSave(Item: TScStorageItem); virtual; abstract;
    procedure Delete(Item: TScStorageItem);
    procedure InternalDelete(Item: TScStorageItem); virtual; abstract;
    procedure Rename(Item: TScStorageItem; const NewName: string; CheckIfExists: boolean = True);
    procedure InternalRename(Item: TScStorageItem; const NewName: string; CheckIfExists: boolean); virtual; abstract;
    procedure GetNames(const ItemClass: TScStorageItemClass; List: TStrings);
    procedure InternalGetNames(const ItemClass: TScStorageItemClass; List: TStrings); virtual; abstract;
    procedure InternalDeleteStorage; virtual; abstract;
    procedure Flush; virtual;

    class procedure LoadObj(Stream: TStream; Item: TScStorageItem; const Algorithm: TScSymmetricAlgorithm; const Password: string);
    class procedure SaveObj(Stream: TStream; Item: TScStorageItem; const Algorithm: TScSymmetricAlgorithm; const Password: string);

  {$IFDEF SBRIDGE}
    procedure CheckUserPass(ClientInfo: TScSSHClientInfo; const APassword: string; var Accept: boolean);
    procedure CheckUserKey(ClientInfo: TScSSHClientInfo; AKey: TScKey; var Accept: boolean);
  {$ENDIF}

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DeleteStorage;

    procedure ImportFromPKCS12(const FileName: string; const Password: string = ''); overload;
    procedure ImportFromPKCS12(Stream: TStream; const Password: string = ''); overload;
    procedure ImportFromPKCS12(PKCS12Processor: TScPKCS12Processor); overload;

    property Keys: TScKeyList read GetKeys;
    property Users: TScUserList read GetUsers;
    property Certificates: TScCertificateList read GetCertificates;
    property CRLs: TScCRLList read GetCRLs;

  published
    property ReadOnly: boolean read FReadOnly write SetReadOnly default False;
    property StoreUserPassword: boolean read FStoreUserPassword write FStoreUserPassword default True;
  {$IFDEF SBRIDGE}
    property OnCheckUserPass: TScCheckUserPass read FOnCheckUserPass write FOnCheckUserPass;
    property OnCheckUserKey: TScCheckUserKey read FOnCheckUserKey write FOnCheckUserKey;
  {$ENDIF}
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TScMemoryStorage = class(TScStorage)
  protected
    procedure InternalLoad(Item: TScStorageItem); override;
    procedure InternalSave(Item: TScStorageItem); override;
    procedure InternalDelete(Item: TScStorageItem); override;
    procedure InternalRename(Item: TScStorageItem; const NewName: string; CheckIfExists: boolean); override;
    procedure InternalGetNames(const ItemClass: TScStorageItemClass; List: TStrings); override;
    procedure InternalDeleteStorage; override;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TScFileStorage = class(TScStorage)
  private
    FPassword: string;
    FAlgorithm: TScSymmetricAlgorithm;
    FPath: string;
    FKeyExt: string;
    FUserExt: string;
    FCertificateExt: string;
    FCRLExt: string;

    procedure SetPath(const Value: string);
    procedure SetKeyExt(const Value: string);
    procedure SetUserExt(const Value: string);
    procedure SetCertificateExt(const Value: string);
    procedure SetCRLExt(const Value: string);

    function GetFullPath: string;
    function GetItemExt(ItemClass: TScStorageItemClass): string;
    procedure GetItemFileName(Item: TScStorageItem; out FileName, Ext: string);
    function GetFileName(Item: TScStorageItem; CreateDir: boolean = False): string;

  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadPath(Reader: TReader);
    procedure WritePath(Writer: TWriter);
    procedure ReadKeyExt(Reader: TReader);
    procedure WriteKeyExt(Writer: TWriter);
    procedure ReadUserExt(Reader: TReader);
    procedure WriteUserExt(Writer: TWriter);
    procedure ReadCertificateExt(Reader: TReader);
    procedure WriteCertificateExt(Writer: TWriter);
    procedure ReadCRLExt(Reader: TReader);
    procedure WriteCRLExt(Writer: TWriter);

    procedure InternalLoad(Item: TScStorageItem); override;
    procedure InternalSave(Item: TScStorageItem); override;
    procedure InternalDelete(Item: TScStorageItem); override;
    procedure InternalRename(Item: TScStorageItem; const NewName: string; CheckIfExists: boolean); override;
    procedure InternalGetNames(const ItemClass: TScStorageItemClass; List: TStrings); override;
    procedure InternalDeleteStorage; override;

  public
    constructor Create(AOwner: TComponent); override;
    procedure ChangePassword(const NewPassword: string);

  published
    property Algorithm: TScSymmetricAlgorithm read FAlgorithm write FAlgorithm default saTripleDES_cbc;
    property Password: string read FPassword write FPassword;

    property Path: string read FPath write SetPath stored False;
    property KeyExt: string read FKeyExt write SetKeyExt stored False;
    property UserExt: string read FUserExt write SetUserExt stored False;
    property CertificateExt: string read FCertificateExt write SetCertificateExt stored False;
    property CRLExt: string read FCRLExt write SetCRLExt stored False;
  end;

{$IFDEF MSWINDOWS}
{$IFDEF VER16P}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}
  TScRegStorage = class(TScStorage)
  private
    FAlgorithm: TScSymmetricAlgorithm;
    FPassword: string;
    FKeyPath: string;
    FRegistry: TRegistry;
    procedure SetKeyPath(const Value: string);
    procedure SetRootKey(const Value: NativeUint);
    function GetRootKey: NativeUint;

  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadKeyPath(Reader: TReader);
    procedure WriteKeyPath(Writer: TWriter);

    function GetItemDir(ItemClass: TScStorageItemClass): string;
    procedure GetItemKeyPath(Item: TScStorageItem; out ARegPath, ARegName: string);
    procedure InternalLoad(Item: TScStorageItem); override;
    procedure InternalSave(Item: TScStorageItem); override;
    procedure InternalDelete(Item: TScStorageItem); override;
    procedure InternalRename(Item: TScStorageItem; const NewName: string; CheckIfExists: boolean); override;
    procedure InternalGetNames(const ItemClass: TScStorageItemClass; List: TStrings); override;
    procedure InternalDeleteStorage; override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ChangePassword(const NewPassword: string);

  published
    property Algorithm: TScSymmetricAlgorithm read FAlgorithm write FAlgorithm default saTripleDES_cbc;
    property Password: string read FPassword write FPassword;

  {$HPPEMIT '#ifdef KeyPath'}
  {$HPPEMIT '#undef KeyPath'}
  {$HPPEMIT '#endif'}
    property KeyPath: string read FKeyPath write SetKeyPath stored False;
    property RootKey: NativeUint read GetRootKey write SetRootKey default HKEY_CURRENT_USER;
  end;
{$ENDIF}

  TScUserAuthentication = (uaPublicKey, uaPassword, uaOSAuthentication);
  TScUserAuthentications = set of TScUserAuthentication;

  // Information about user
  TScUser = class(TScStorageItem)
  private
    FDomain: string;
    FHomePath: string;
    FExtData: string;
    FSSHChannelPermissions: TScSSHChannelPermissions;

    FAuthentications: TScUserAuthentications;
    FPassword: string;
    FHashPassword: string;
    FKey: TScKey;

    function GetUserList: TScUserList;
    procedure SetUserList(Value: TScUserList);

    procedure SetDomain(const Value: string);
    procedure SetHomePath(const Value: string);
    procedure SetExtData(const Value: string);
    procedure SetSSHChannelPermissions(const Value: TScSSHChannelPermissions);

    procedure SetPassword(const Value: string);
    procedure SetAuthentications(const Value: TScUserAuthentications);
    procedure SetKey(Value: TScKey);
    procedure CheckAuthenticationKey;

  protected
    procedure InternalAssign(Source: TScStorageItem); override;
    procedure SetItemName(const Value: string); override;

    procedure LoadKeyFromBytes(const Data: TBytes);
    function SaveKeyToBytes: TBytes;
    procedure LoadFromBytes(const Data: TBytes);
    function SaveToBytes: TBytes;

  {$IFDEF SBRIDGE}
    procedure CheckUserPass(ClientInfo: TScSSHClientInfo; const APassword: string; var Accept: boolean);
    procedure CheckUserKey(ClientInfo: TScSSHClientInfo; AKey: TScKey; var Accept: boolean);
  {$ENDIF}

  public
    constructor Create(AStorageList: TScStorageList = nil); override;
    destructor Destroy; override;

    function Equals(Item: TScStorageItem): boolean; reintroduce; override;

    property UserName: string read FItemName write SetItemName;
    property Domain: string read FDomain write SetDomain; // if uaOSAuthentication in Authentications
    property HomePath: string read FHomePath write SetHomePath;

    property SSHChannelPermissions: TScSSHChannelPermissions read FSSHChannelPermissions write SetSSHChannelPermissions;
    property ExtData: string read FExtData write SetExtData;

    property Authentications: TScUserAuthentications read FAuthentications write SetAuthentications;
    property HashPassword: string read FHashPassword;
    property Password: string read FPassword write SetPassword; // if uaPassword in Authentications
    property Key: TScKey read FKey write SetKey; // nil, if not (uaPublicKey in Authentications)
    property UserList: TScUserList read GetUserList write SetUserList;
  end;

  TScUserList = class(TScStorageList)
  protected
    function GetUser(Index: integer): TScUser;
    procedure SetUser(Index: integer; Value: TScUser);
    function GetItemClassType: TScStorageItemClass; override;
    function GetItemDefName: string; override;

  public
    procedure CheckUserName(const UserName: string);
    function FindUser(const UserName: string): TScUser;
    function UserByName(const UserName: string): TScUser;
    procedure GetUserNames(List: TStrings);

    property Users[Index: integer]: TScUser read GetUser write SetUser; default;
  end;

  // !!! Synchronize with CERTIFICATE_STATUS_ERROR_CODE
  TScCertificateStatus = (
    csValid,
    csOtherError, csExpired,
    csInsecureSignature, csForbiddenSignature, csInvalidPolicies,
    csUnknownCriticalExtension,
    csCRLNotFound, csCRLIsNotValid, csCertificateIsRevoked,
    csInvalidBasicConstraints, csInvalidKeyUsage, csIssuerNotEqualSubject,
    csInvalidSubjectName, csUntrustedRoot, csInvalidSignature
  );
  TScCertificateStatusSet = set of TScCertificateStatus;

  TScObtainCRLEvent = procedure (Sender: TObject;
    DistributionPointName: TScGeneralNames; Update: boolean; out CRL: TScCRL) of object;

  TScFreeHandleEvent = procedure (Handle: IntPtr) of object;

  TScCertificate = class(TScStorageItem)
  private
    FASN1Compiler: TScASN1Compiler;
    FRawData: TBytes;
    FHandle: IntPtr;
    FIssuerDN: TScDistinguishedName;
    FSubjectDN: TScDistinguishedName;
    FExtensions: TScExtensions;
    FCRLDistributionPointsExtension: TScCertCRLDistributionPointsExtension;
    FKey: TScKey;
    FOrigKey: TScKey;
    FSignatureAlgorithm: TScSignatureAlgorithmIdentifier;
    FCRLReason: TScCRLReason;
    FOnFreeHandle: TScFreeHandleEvent;
    FIsSelfSigned: integer;

    function GetCertificateList: TScCertificateList;
    procedure SetCertificateList(Value: TScCertificateList);

    class function ParsePEMString(const Cert, Delimiter: string): string;
    function GetIssuer: string;
    function GetSubject: string;
    procedure CheckIssuerName;
    procedure CheckSubjectName;
    class function GetDisplayName(DN: TScDistinguishedName): string;
    function GetIssuerName: TScDistinguishedName;
    function GetSubjectName: TScDistinguishedName;
    function GetNotAfter: TDateTime;
    function GetNotBefore: TDateTime;
    function GetSerialNumber: string;
    function GetSignatureAlgorithm: TScSignatureAlgorithmIdentifier;
    function GetSignature: TBytes;
    function GetVersion: integer;
    procedure CheckExtensions;
    function GetExtensions: TScExtensions;
    function GetSubjectKeyIdentifier: string;
    function GetKey: TScKey;
    function GetIsSelfSigned: boolean;

  protected
    procedure SetHandle(Handle: IntPtr; OnFreeHandle: TScFreeHandleEvent);
    procedure InternalAssign(Source: TScStorageItem); override;
    procedure FreeData; override;
    procedure KeyChanged;

    class procedure GetSignAndPadAlg(SignatureAlgorithm: TScSignatureAlgorithmIdentifier;
      out HashAlgorithm: TScHashAlgorithm; out PaddingMode: TScPaddingMode);
    function CheckSignature(ParentCertificate: TScCertificate): boolean;
    function CheckPolicies(ChildPolicies, ParentPolicies: TScCertPoliciesExtension): boolean;

    procedure VerifyCertificateByDeltaCRL(OnObtainCRL: TScObtainCRLEvent; DistributionPointName: TScGeneralNames;
      CRL: TScCRL; ParentCertificate: TScCertificate; var StatusSet: TScCertificateStatusSet);

    procedure CheckCRLDistributionPointsExtension;

  public
    constructor Create(AStorageList: TScStorageList = nil); override;

    procedure SetRawData(Handle: IntPtr; OnFreeHandle: TScFreeHandleEvent; const RawData: TBytes);
    function GetRawData: TBytes;
    function Equals(Item: TScStorageItem): boolean; reintroduce; override;
    procedure GetFingerprint(const HashAlg: TScHashAlgorithm; out Fingerprint: TBytes); overload; // Returns the hash value for the X.509v3 certificate as an array of bytes.
    procedure GetFingerprint(const HashAlg: TScHashAlgorithm; out Fingerprint: string); overload; // Returns the hash value for the X.509v3 certificate as a string.

    procedure ImportFrom(const FileName: string; const Password: string = ''); overload;
    procedure ImportFrom(Stream: TStream; const Password: string = ''); overload;
    procedure ExportTo(const FileName: string; const CertEncoding: TScCertificateEncoding = cfPEM); overload;
    procedure ExportTo(Stream: TStream; const CertEncoding: TScCertificateEncoding = cfPEM); overload;

    function Sign(const Data: TBytes; HashAlg: TScHashAlgorithm = haSHA1; Padding: TScPaddingMode = pmPKCS1): TBytes;
    function SignHash(const Hash: TBytes; HashAlg: TScHashAlgorithm = haSHA1; Padding: TScPaddingMode = pmPKCS1): TBytes;
    function VerifySign(const Data, Sign: TBytes; HashAlg: TScHashAlgorithm = haSHA1; Padding: TScPaddingMode = pmPKCS1): boolean;
    function VerifyHashSign(const Hash, Sign: TBytes; HashAlg: TScHashAlgorithm = haSHA1; Padding: TScPaddingMode = pmPKCS1): boolean;
    function Encrypt(const Data: TBytes): TBytes;
    function Decrypt(const Data: TBytes): TBytes;

    procedure VerifyCertificateChain(OnObtainCRL: TScObtainCRLEvent; ParentCertificate: TScCertificate; var StatusSet: TScCertificateStatusSet);
    procedure VerifyCRLRevocation(OnObtainCRL: TScObtainCRLEvent; ParentCertificate: TScCertificate; var StatusSet: TScCertificateStatusSet);

    property Issuer: string read GetIssuer;
    property IssuerName: TScDistinguishedName read GetIssuerName; // Returns a list of attributes of the X.509v3 certificate.
    property Subject: string read GetSubject;
    property SubjectName: TScDistinguishedName read GetSubjectName; // Returns a list of attributes of the X.509v3 certificate.
    property NotAfter: TDateTime read GetNotAfter; // Retrieves the ending date for the validity of the certificate.
    property NotBefore: TDateTime read GetNotBefore; // Retrieves the beginning date for the validity of the certificate.
    property SerialNumber: string read GetSerialNumber; // Returns the serial number of the X.509v3 certificate.
    property SignatureAlgorithm: TScSignatureAlgorithmIdentifier read GetSignatureAlgorithm;
    property Signature: TBytes read GetSignature;
    property Version: integer read GetVersion;
    property Extensions: TScExtensions read GetExtensions; // Returns a list of extensions of the X.509v3 certificate.
    property SubjectKeyIdentifier: string read GetSubjectKeyIdentifier;
    property CRLReason: TScCRLReason read FCRLReason;
    property IsSelfSigned: boolean read GetIsSelfSigned;

    property Handle: IntPtr read FHandle;
    property Key: TScKey read GetKey;
    property CertName: string read FItemName write SetItemName;
    property CertificateList: TScCertificateList read GetCertificateList write SetCertificateList;
  end;

  TScCertificateList = class(TScStorageList)
  protected
    function GetCertificate(Index: integer): TScCertificate;
    procedure SetCertificate(Index: integer; Value: TScCertificate);
    function GetItemClassType: TScStorageItemClass; override;
    function GetItemDefName: string; override;

  public
    procedure CheckCertificateName(const CertName: string);
    function FindCertificate(const CertName: string): TScCertificate;
    function CertificateByName(const CertName: string): TScCertificate;
    procedure GetCertificateNames(List: TStrings);

    function FindCertificateBySubject(DistinguishedName: TScDistinguishedName): TScCertificate;

    property Certificates[Index: integer]: TScCertificate read GetCertificate write SetCertificate; default;
  end;

  TScCertificateChain = class
  public
    class procedure VerifyChain(OnObtainCRL: TScObtainCRLEvent; CertificateList: TCRList; out StatusSet: TScCertificateStatusSet);
  end;

  TScRevokedCertificate = class(TScPersistent)
  private
    FSerialNumber: string;
    FRevocationDate: TDateTime;
    FExtensions: TScExtensions;
    FExtensionsEncodedData: TBytes;
    function GetExtensions: TScExtensions;

  protected
    function Clone: TScPersistent; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TScPersistent); override;

    property SerialNumber: string read FSerialNumber;
    property RevocationDate: TDateTime read FRevocationDate;
    property Extensions: TScExtensions read GetExtensions;
  end;

  TScRevokedCertificates = class(TScPersistentObjectList)
  private
    function GetRevokedCertificate(Index: integer): TScRevokedCertificate;
    procedure SetRevokedCertificate(Index: integer; Item: TScRevokedCertificate);
  protected
    function GetItemClassType: TScPersistentClass; override;

  public
    property RevokedCertificates[Index: integer]: TScRevokedCertificate read GetRevokedCertificate write SetRevokedCertificate; default;
  end;

  TScCRL = class(TScStorageItem)
  private
    FASN1Compiler: TScASN1Compiler;
    FRawData: TBytes;
    FIsPartialParsed: boolean;
    FSignatureAlgorithm: TScSignatureAlgorithmIdentifier;
    FIssuerDN: TScDistinguishedName;
    FExtensions: TScExtensions;
    FRevokedCertificates: TScRevokedCertificates;
    FRevokedSN: TStringList;

    function GetCRLList: TScCRLList;
    procedure SetCRLList(Value: TScCRLList);

    function GetVersion: integer;
    function GetSignature: TBytes;
    function GetSignatureAlgorithm: TScSignatureAlgorithmIdentifier;
    function GetIssuer: string;
    function GetIssuerDN: TScDistinguishedName;
    function GetThisUpdate: TDateTime;
    function GetNextUpdate: TDateTime;
    function GetExtensions: TScExtensions;
    function GetRevokedCertificates: TScRevokedCertificates;
    procedure CheckExtensions;
    procedure CheckRevokedCertificates;

  protected
    procedure InternalAssign(Source: TScStorageItem); override;
    procedure FreeData; override;
    function CheckSignature(ParentCertificate: TScCertificate): boolean;
    procedure SetRawData(const RawData: TBytes);

  public
    constructor Create(AStorageList: TScStorageList = nil); override;

    function Equals(Item: TScStorageItem): boolean; reintroduce; override;

    function CheckCompliance(Cert: TScCertificate; CertDistributionPoint: TScCRLDistributionPoint): boolean; overload;
    function CheckCompliance(DeltaCRL: TScCRL): boolean; overload;
    function FindCertificate(Cert: TScCertificate; out Reason: TScCRLReason): boolean;
    procedure VerifyCRLChain(ParentCertificate: TScCertificate; var StatusSet: TScCertificateStatusSet);

    procedure ImportFrom(const FileName: string; const Password: string = ''); overload;
    procedure ImportFrom(Stream: TStream; const Password: string = ''); overload;
    procedure ExportTo(const FileName: string; const CertEncoding: TScCertificateEncoding = cfPEM); overload;
    procedure ExportTo(Stream: TStream; const CertEncoding: TScCertificateEncoding = cfPEM); overload;

    class function LoadByHttp(const CRLUrl: string): TScCRL;

    property Version: integer read GetVersion;
    property Signature: TBytes read GetSignature;
    property SignatureAlgorithm: TScSignatureAlgorithmIdentifier read GetSignatureAlgorithm;
    property Issuer: string read GetIssuer;
    property IssuerName: TScDistinguishedName read GetIssuerDN;
    property ThisUpdate: TDateTime read GetThisUpdate;
    property NextUpdate: TDateTime read GetNextUpdate;
    property Extensions: TScExtensions read GetExtensions;
    property RevokedCertificates: TScRevokedCertificates read GetRevokedCertificates;

    property CRLName: string read FItemName write SetItemName;
    property CRLList: TScCRLList read GetCRLList write SetCRLList;
  end;

  TScCRLList = class(TScStorageList)
  protected
    function GetCRL(Index: integer): TScCRL;
    procedure SetCRL(Index: integer; Value: TScCRL);
    function GetItemClassType: TScStorageItemClass; override;
    function GetItemDefName: string; override;

  public
    class procedure DoObtainCRL(Sender: TObject; Storage: TScStorage;
      AllowLoadCRLByHttp: boolean; OnObtainCRL: TScObtainCRLEvent;
      DistributionPointName: TScGeneralNames; Update: boolean; out CRL: TScCRL);
    class function GetHashedName(const FullName: string): string;
    procedure CheckCRLName(const CRLName: string);
    function FindCRL(const CRLName: string): TScCRL;
    function CRLByName(const CRLName: string): TScCRL;
    procedure GetCRLNames(List: TStrings);

    property CRLs[Index: integer]: TScCRL read GetCRL write SetCRL; default;
  end;

  TScPKCS12Bag = class
  private
    FItem: TScStorageItem;
    FFriendlyName: string;
    FLocalKeyID: TBytes;
    FTypeID: string;
    FRawData: TBytes;
  public
    destructor Destroy; override;

    property Item: TScStorageItem read FItem;
    property FriendlyName: string read FFriendlyName;
    property LocalKeyID: TBytes read FLocalKeyID;
    property TypeID: string read FTypeID;
    property RawData: TBytes read FRawData;
  end;

  TScPKCS12Processor = class
  private
    FKeyList: TCRList;
    FCertList: TCRList;
    FCRLList: TCRList;
    FCustomList: TCRList;
    FCertificateEnc: TScCertificate;
    FCertificateSign: TScCertificate;

    procedure ParseBags(const Content: TBytes; const Password: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    class function ImportPasswordFromSSO(Stream: TStream): string;
    procedure ImportFrom(const FileName: string; const Password: string = ''); overload;
    procedure ImportFrom(Stream: TStream; const Password: string = ''); overload;

    property CertificateEnc: TScCertificate read FCertificateEnc write FCertificateEnc;
    property CertificateSign: TScCertificate read FCertificateSign write FCertificateSign;

    property KeyList: TCRList read FKeyList;
    property CertList: TCRList read FCertList;
    property CRLList: TCRList read FCRLList;
    property CustomList: TCRList read FCustomList;
  end;

  TScKeyUtils = class
  public
    class procedure SetSSHKeyData(Key: TScKey; const SSHKeyData: TBytes);
    class function GetSSHKeyData(Key: TScKey): TBytes;
    class procedure SetRSAData(Key: TScKey; const RSAData: TScRSAData);
    class procedure LoadKeyFromCryptoAPIFormat(Key: TScKey; const Data: TBytes;
      const KeyExFormat: TScKeyExFormat; const Alg: TScAsymmetricAlgorithm; const IsPublicKey: boolean);
    class function SaveKeyToCryptoAPIFormat(Key: TScKey): TBytes;
    class procedure LoadKeyFromPVKFormat(Key: TScKey; const Data: TBytes);
  end;

{$IFDEF SBRIDGE}
  TScStorageUtils = class
  public
    class procedure CheckUserPass(Obj: TScStorage; ClientInfo: TScSSHClientInfo; const Password: string; var Accept: boolean);
    class procedure CheckUserKey(Obj: TScStorage; ClientInfo: TScSSHClientInfo; Key: TScKey; var Accept: boolean);
    class procedure Invalidate(Obj: TScStorage);
  end;
{$ENDIF}

  TScCertificateUtils = class
  public
    class procedure SetHandle(Obj: TScCertificate; Handle: IntPtr; OnFreeHandle: TScFreeHandleEvent);
  end;

  TScServerKeyValidationEvent = procedure(Sender: TObject;
    NewServerKey: TScKey; // Key from server
    var Accept: boolean   // allowed to accept NewServerKey
  ) of object;

  TScRemoteCertificateValidationEvent = procedure(Sender: TObject;
    RemoteCertificate: TScCertificate; CertificateList: TCRList;
    var Errors: TScCertificateStatusSet
  ) of object;

const
  PEM_LINE_LENGTH = 64;
  DER_LINE_LENGTH = 48; // = PEM_LINE_LENGTH div 4 * 3

const
  CERTIFICATE_STATUS_ERROR_CODE: array [TScCertificateStatus] of TScErrorCode = (
    seSuccess,
    seCertificateNotValid, seCertNotValidityPeriod,
    seInsecureSignature, seForbiddenSignature, seInvalidPolicies,
    seUnknownCriticalExtension,
    seCRLNotFound, seCRLIsNotValid, seCertificateIsRevoked,
    seCertCANotValid, seCertKeyUsageNotValid, seIssuerNotEqualSubject,
    seInvalidSubjectName, seCertNotTrusted, seCertSignatureNotValid
  );

var
  CurrentProjectOutputDir: string;
  Random: IScRandom;
  CRLStorage: TScStorage = nil;

implementation

uses
  StrUtils, Math,
{$IFNDEF SBRIDGE}
  CRBase64, CRHash, CRDECUtil, CRCryptoTransformIntf,
  CRHttp,
{$IFNDEF UNIDACPRO}
  TdsReaderWriter, TdsAlgorithmSupport, TdsCipherSuites;
{$ELSE}
  TdsReaderWriterUni, TdsAlgorithmSupportUni, TdsCipherSuitesUni;
{$ENDIF}
{$ELSE}
  ScBase64, ScHash, ScDECUtil, ScCryptoTransformIntf,
  ScReaderWriter, ScAlgorithmSupport, ScCipherSuites,
  ScHttp, ScCMS;
{$ENDIF}

const
  KeyFormatVersion = 1;
  ECKeyFormatVersion = 11;
  UserFormatVersion1 = 1;
  UserFormatVersion2 = 2;
  UserFormatVersion3 = 3;
  UserFormatVersion4 = 4;
  StorageVersion1 = '1';
  StorageVersion2 = '2';
  STORAGE_KEY_PATH = '\SOFTWARE\Devart\SecureBridge\';

  SSHPERMS_PROP_NAME = 'SSHPERMS';
  EXTDATA_PROP_NAME = 'EXTDATA';

  TMP_NAME = '_tmp';
  REGISTRY_KEYS = '\Keys';
  REGISTRY_USERS = '\Users';
  REGISTRY_CERTIFICATES = '\Certificates';
  REGISTRY_CRLS = '\CRLs';

const
  STORAGE_ITEM_CLASSES: array[0..3] of TScStorageItemClass = (
    TScKey, TScUser, TScCertificate, TScCRL
  );

const
  SSH_CHANNEL_PERMISSION_CODES: array [TScSSHChannelPermission] of integer = (
    1 {cpAllowLocalForwarding},
    2 {cpAllowRemoteForwarding},
    4 {cpAllowShell},
    8 {cpAllowSFTP}
  );

const
  wNAFSize = 4;

type
  TPrimeSieve = class
  private
    FTable: array of cardinal;
  public
    constructor Create(x: integer);
    function AvailablePrimes: cardinal;
    function GetNextPrime(x: integer): integer;
  end;

const
  BitCounts: array[0..255] of integer = (
    0,1,1,2,1,2,2,3,1,2,2,3,2,3,3,4,1,2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,1,
    2,2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,1,2,
    2,3,2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,2,3,3,
    4,3,4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,1,2,2,3,
    2,3,3,4,2,3,3,4,3,4,4,5,2,3,3,4,3,4,4,5,3,4,4,5,4,5,5,6,2,3,3,4,3,
    4,4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,2,3,3,4,3,4,
    4,5,3,4,4,5,4,5,5,6,3,4,4,5,4,5,5,6,4,5,5,6,5,6,6,7,3,4,4,5,4,5,5,
    6,4,5,5,6,5,6,6,7,4,5,5,6,5,6,6,7,5,6,6,7,6,7,7,8
  );

{ TPrimeSieve }

constructor TPrimeSieve.Create(x: integer);
var
  Len, Max, Stop: integer;
  i, j, k: integer;
begin
  inherited Create;

  if x < 4 then
    x := 4;
  Len := (x - 3) div (32 * 2);
  SetLength(FTable, Len);

  Max := Len * 32;
  Stop := Round(sqrt(Max)) + 1;

  for i := 0 to Stop - 1 do begin
    if (FTable[i div 32] and (1 shl (i and (32 - 1)))) = 0 then begin
      k := 3 + i * 2;
      j := i + k;
      while  j < Max do begin
        FTable[j div 32] := FTable[j div 32] or (cardinal(1) shl (j and (32 - 1)));
        j := j + k;
      end;
    end;
  end;
end;

function TPrimeSieve.AvailablePrimes: cardinal;
var
  Bits, w, Primes: cardinal;
  i: integer;
begin
  Primes := 2;

  for i := 0 to Length(FTable) - 1 do begin
    w := FTable[i];
    Bits := 0;
    while w <> 0 do begin
      Bits := Bits + cardinal(BitCounts[w and $FF]);
      w := w shr 8;
    end;

    Primes := Primes + (32 - Bits);
  end;

  Result := Primes;
end;

function TPrimeSieve.GetNextPrime(x: integer): integer;
var
  p: integer;
begin
  p := ((x - 3) div 2) + 1;
  case x of
    // Trivial cases
    0: Result := 2;
    1: Result := 2;
    2: Result := 3;
  else
    // Cases above 2 are handled with the Table
    while True do begin
      if (p div 32) >= Length(FTable) then begin
        Result := 0;
        Exit;
      end;

      if (FTable[p div 32] and (1 shl (p and (32 - 1)))) = 0 then begin
        Result := p * 2 + 3;
        Exit;
      end;

      Inc(p);
    end;
  end;
end;

{ T32BytesField }

constructor T32BytesField.Create;
begin
  inherited;
end;

constructor T32BytesField.Create(const Data: TBytes);
begin
  inherited Create;

  if Length(Data) >= sizeof(T32Bytes) then
    Move(Data[0], BytesField[0], sizeof(T32Bytes));
end;

procedure T32BytesField.Assign(Source: TPersistent);
begin
  if IsClass(Source, T32BytesField) then begin
    Move(T32BytesField(Source).BytesField[0], BytesField[0], sizeof(T32Bytes));
  end
  else
    inherited Assign(Source);
end;

function T32BytesField.Equal(Value: T32BytesField): boolean;
begin
  Result := MemCompare(@BytesField[0], @Value.BytesField[0], sizeof(T32Bytes)) = 0;
end;

function T32BytesField.GetBytes: TBytes;
begin
  SetLength(Result, sizeof(T32Bytes));
  Move(BytesField[0], Result[0], sizeof(T32Bytes));
end;

{ TScECPointBI }

destructor TScECPointBI.Destroy;
begin
  X.Free;
  Y.Free;
  Z.Free;

  inherited;
end;

procedure TScECPointBI.Assign(Source: TScCustomECPoint);
begin
  if IsClass(Source, TScECPointBI) then begin
    FreeAndNil(X);
    FreeAndNil(Y);
    FreeAndNil(Z);

    if TScECPointBI(Source).X <> nil then
      X := TBigInteger.Create(TScECPointBI(Source).X);

    if TScECPointBI(Source).Y <> nil then
      Y := TBigInteger.Create(TScECPointBI(Source).Y);

    if TScECPointBI(Source).Z <> nil then
      Z := TBigInteger.Create(TScECPointBI(Source).Z);
  end
  else
  if Source <> nil then
    raise EConvertError.CreateFmt(SAssignError, [Source.ClassName, ClassName]);
end;

function TScECPointBI.Equals(ECPoint: TScCustomECPoint): boolean;
begin
  Result := IsClass(ECPoint, TScECPointBI) and
    ((X = nil) or X.Equal(TScECPointBI(ECPoint).X)) and
    ((Y = nil) or Y.Equal(TScECPointBI(ECPoint).Y)) and
    ((Z = nil) or Z.Equal(TScECPointBI(ECPoint).Z));
end;

{ TScECPointX25519 }

procedure TScECPointX25519.Assign(Source: TScCustomECPoint);
begin
  if IsClass(Source, TScECPointX25519) then begin
    Move(TScECPointX25519(Source).PointField[0], PointField[0], sizeof(T32Bytes));
  end
  else
  if Source <> nil then
    raise EConvertError.CreateFmt(SAssignError, [Source.ClassName, ClassName]);
end;

function TScECPointX25519.Equals(ECPoint: TScCustomECPoint): boolean;
begin
  Result := IsClass(ECPoint, TScECPointX25519) and
    (MemCompare(@PointField[0], @TScECPointX25519(ECPoint).PointField[0], sizeof(T32Bytes)) = 0);
end;

{ TScCustomECCryptography }

constructor TScCustomECCryptography.Create;
begin
  inherited Create;
  FIsNamed := False;
end;

constructor TScCustomECCryptography.Create(const ECParameters: TScECParameters);
begin
  inherited Create;

  FIsNamed := True;
  FSize := ECParameters.Size;
end;

procedure TScCustomECCryptography.Assign(Source: TScCustomECCryptography);
begin
  if Source = nil then
    Exit;

  if ClassType <> Source.ClassType then
    raise EConvertError.CreateFmt(SAssignError, [Source.ClassName, ClassName]);

  FIsNamed := Source.FIsNamed;
  FSize := Source.FSize;
end;

function TScCustomECCryptography.BigIntToHexBuf(bi: TBigInteger): TBytes;
var
  s: string;
  Len: integer;
begin
  s := bi.ToString(16);
  Result := HexToBytes(s);
  Len := Length(Result);
  if Len < FSize then begin
    SetLength(Result, FSize);
    Move(Result[0], Result[FSize - Len], Len);
    FillChar(Result[0], FSize - Len, 0);
  end;
end;

{ TScECCryptographyBI }

constructor TScECCryptographyBI.Create;
begin
  inherited Create;
end;

constructor TScECCryptographyBI.Create(const ECParameters: TScECParameters);
begin
  inherited Create(ECParameters);

  FModP := TBigInteger.Create(ECParameters.p, 16);
  FA := TBigInteger.Create(ECParameters.a, 16);
  FB := TBigInteger.Create(ECParameters.b, 16);
  FN := TBigInteger.Create(ECParameters.n, 16);

  FG := TScECPointBI.Create;
  FG.X := TBigInteger.Create(ECParameters.Gx, 16);
  FG.Y := TBigInteger.Create(ECParameters.Gy, 16);
  FG.Z := TBigInteger.Create(1);

  Prepare;
end;

destructor TScECCryptographyBI.Destroy;
begin
  FModP.Free;
  FA.Free;
  FB.Free;
  FN.Free;
  FG.Free;

  inherited;
end;

function TScECCryptographyBI.Equals(ECCryptography: TScCustomECCryptography): boolean;
begin
  Result := (ClassType = ECCryptography.ClassType) and
    (FSize = ECCryptography.FSize) and
    FModP.Equal(TScECCryptographyBI(ECCryptography).FModP) and
    FA.Equal(TScECCryptographyBI(ECCryptography).FA) and
    FB.Equal(TScECCryptographyBI(ECCryptography).FB) and
    FN.Equal(TScECCryptographyBI(ECCryptography).FN) and
    FG.Equals(TScECCryptographyBI(ECCryptography).FG);
end;

procedure TScECCryptographyBI.Assign(Source: TScCustomECCryptography);
var
  SrcBI: TScECCryptographyBI;
begin
  if Source = nil then
    Exit;

  inherited Assign(Source);

  SrcBI := TScECCryptographyBI(Source);
  FSeed := SrcBI.FSeed;
  FCofactor := SrcBI.FCofactor;

  FreeAndNil(FModP);
  if SrcBI.FModP <> nil then
    FModP := TBigInteger.Create(SrcBI.FModP);

  FreeAndNil(FA);
  if SrcBI.FA <> nil then
    FA := TBigInteger.Create(SrcBI.FA);

  FreeAndNil(FB);
  if SrcBI.FB <> nil then
    FB := TBigInteger.Create(SrcBI.FB);

  FreeAndNil(FN);
  if SrcBI.FN <> nil then
    FN := TBigInteger.Create(SrcBI.FN);

  FreeAndNil(FG);
  if SrcBI.FG <> nil then begin
    FG := TScECPointBI.Create;
    FG.Assign(SrcBI.FG);
  end;

  Prepare;
end;

function TScECCryptographyBI.PublicPointClass: TScECPointClass;
begin
  Result := TScECPointBI;
end;

function TScECCryptographyBI.PrivateKoefClass: TPersistentClass;
begin
  Result := TBigInteger;
end;

function TScECCryptographyBI.GetBitCount: integer;
begin
  Result := FModP.BitCount;
end;

function TScECCryptographyBI.GenerateRandomKoef(ARandom: IScRandom = nil): TBigInteger;
var
  RandomBuf: TBytes;
begin
  if ARandom = nil then
    ARandom := Random;
  if ARandom = nil then
    raise Exception.Create(SInternalError);

  SetLength(RandomBuf, FSize);
  ARandom.Random(RandomBuf, 0, Length(RandomBuf));

  Result := TBigInteger.Create(RandomBuf);
  try
    if Result.GreaterOrEqual(FN) and (RandomBuf[0] > 1) then begin
      RandomBuf[0] := 1;
      FreeAndNil(Result);
      Result := TBigInteger.Create(RandomBuf);
    end;

    if Result.GreaterOrEqual(FN) then begin
      RandomBuf[0] := 0;
      RandomBuf[1] := 1;
      FreeAndNil(Result);
      Result := TBigInteger.Create(RandomBuf);
    end;

    FillChar(RandomBuf[0], Length(RandomBuf), 0);
  except
    Result.Free;
    raise;
  end;
end;

procedure TScECCryptographyBI.GeneratePrivateKey(var ECData: TScECData; ARandom: IScRandom = nil);
begin
  ECData.PrivateKoef := GenerateRandomKoef(ARandom);
  ECData.PublicPoint := MulPoint(FG, ECData.PrivateKoef);

  if not IsPointBelongTo(TScECPointBI(ECData.PublicPoint)) then
    raise EScError.Create(seWrongEllipticCurvesParameters);
end;

class function TScECCryptographyBI.Compute_wNAF(Scalar: TBigInteger): TIntArr;
var
  WindowValue: integer;
  Bit, NextBit, Len: integer;
  Sign, Digit: integer;
  Mask: cardinal;
  j: integer;
begin
  Bit := 1 shl wNAFSize; // at most 128
  NextBit := Bit shl 1;  // at most 256
  Mask := NextBit - 1;   // at most 255

  if Scalar.IsNegative then
    Sign := -1
  else
    Sign := 1;

  Len := Scalar.BitCount;
  SetLength(Result, Len + 1); // modified wNAF may be one digit longer than binary representation

  WindowValue := Scalar.IntValue and Mask;
  j := 0;
  while (WindowValue <> 0) or ((j + wNAFSize + 1) < Len) do begin // if j+w+1 >= Len, WindowValue will not increase
    Digit := 0;

    // 0 <= WindowValue <= 2^(w+1)
    if (WindowValue and 1) <> 0 then begin
      // 0 < WindowValue < 2^(w+1)
      if (WindowValue and Bit) <> 0 then begin
        Digit := WindowValue - NextBit; // -2^w < Digit < 0

        if (j + wNAFSize + 1) >= Len then begin
          // special case for generating modified wNAFs:
          // no new bits will be added into WindowValue,
          // so using a positive digit here will decrease the total length of the representation
          Digit := WindowValue and (Mask shr 1); // 0 < Digit < 2^w
        end;
      end
      else
        Digit := WindowValue; // 0 < Digit < 2^w

      if (Digit <= -Bit) or (Digit >= Bit) or ((Digit and 1) = 0) then
        raise EScError.Create(sewNAFError);

      WindowValue := WindowValue - Digit;

      // now WindowValue is 0 or 2^(w+1) in standard wNAF generation;
      // for modified window NAFs, it may also be 2^w
      if (WindowValue <> 0) and (WindowValue <> NextBit) and (WindowValue <> Bit) then
        raise EScError.Create(sewNAFError);
    end;

    if j >= (Len + 1) then
      raise EScError.Create(sewNAFError);
    Result[j] := Sign * Digit;
    Inc(j);

    WindowValue := WindowValue shr 1;
    WindowValue := WindowValue + (Bit * Scalar.GetBit(j + wNAFSize));

    if WindowValue > NextBit then
      raise EScError.Create(sewNAFError);
  end;

  SetLength(Result, j);
end;

function TScECCryptographyBI.MulPoint(P1: TScCustomECPoint; Koef: TObject): TScCustomECPoint;
var
  wNAF: TIntArr;
  Digit: integer;
  PreValues: array of TScECPointBI;
  R: TScECPointBI;
  IsAtInfinity, IsInverted, IsNeg: boolean;
  i: integer;
begin
  if not IsClass(P1, TScECPointBI) then
    raise EScError.Create(seInvalidInputArgs);

  if not IsClass(Koef, TBigInteger) then
    raise EScError.Create(seInvalidInputArgs);

  if TScECPointBI(P1).Z.BitCount <> 1 then
    raise EScError.Create(seInvalidInputArgs);

  R := TScECPointBI.Create;
  try
    wNAF := Compute_wNAF(TBigInteger(Koef));

    // Length will be the total number of temporarily precomputed points
    SetLength(PreValues, 1 shl (wNAFSize - 1));
    for i := 0 to Length(PreValues) - 1 do
      PreValues[i] := TScECPointBI.Create;

    // prepare precomputed values:
    //  PreValues[0] :=     point
    //  PreValues[1] := 3 * point
    //  PreValues[2] := 5 * point
    //  ...
    ClonePoint(TScECPointBI(P1), PreValues[0]);

    DblPoint(PreValues[0], R);
    for i := 1 to Length(PreValues) - 1 do begin
      AddPoints(R, PreValues[i - 1], PreValues[i]);
      if NeedConvertToAffineCoordOnPrecomp then
        // to optimaze calculations in AddPoints
        ToAffineCoord(PreValues[i]);
    end;

    IsAtInfinity := True;
    IsInverted := False;

    for i := Length(wNAF) - 1 downto 0 do begin
      if not IsAtInfinity then
        DblPoint(R, R);

      Digit := wNAF[i];

      if Digit <> 0 then begin
        IsNeg := Digit < 0;

        if IsNeg then
          Digit := -Digit;

        if IsNeg <> IsInverted then begin
          if not IsAtInfinity then
            Invert(R);

          IsInverted := not IsInverted;
        end;

        // Digit > 0
        if IsAtInfinity then begin
          ClonePoint(PreValues[Digit shr 1], R);
          IsAtInfinity := False;
        end
        else
          AddPoints(R, PreValues[Digit shr 1], R);
      end;
    end;

    if IsAtInfinity then
      SetToInfinity(R)
    else
    if IsInverted then
      Invert(R);

    ToAffineCoord(R);
    Result := R;
    R := nil;
  finally
    for i := 0 to Length(PreValues) - 1 do
      PreValues[i].Free;

    R.Free;
  end;
end;

function TScECCryptographyBI.NeedConvertToAffineCoordOnPrecomp: boolean;
begin
  Result := False;
end;

procedure TScECCryptographyBI.SetToInfinity(P: TScECPointBI);
begin
  if P.Z <> nil then
    P.Z.SetToZero
  else
    P.Z := TBigInteger.Create(0);
end;

procedure TScECCryptographyBI.ClonePoint(P: TScECPointBI; Res: TScECPointBI);
begin
  if P = Res then
    Exit;

  FreeAndNil(Res.X);
  FreeAndNil(Res.Y);
  FreeAndNil(Res.Z);
  Res.X := TBigInteger.Create(P.X);
  Res.Y := TBigInteger.Create(P.Y);
  Res.Z := TBigInteger.Create(P.Z);
end;

function TScECCryptographyBI.DecodePointFromOctetString(const Buf: TBytes; Offset, Count: integer): TScCustomECPoint;
var
  Res: TScECPointBI;
begin
  Res := TScECPointBI.Create;
  Result := Res;
  try
    if (Count = 1) and (Buf[Offset] = 0) then begin // infinity
      Res.X := TBigInteger.Create(0);
      Res.Y := TBigInteger.Create(0);
      Res.Z := TBigInteger.Create(0);
      Exit;
    end;

    if Count <> (1 + FSize * 2) then
      raise EScError.Create(seWrongECPointFormat);

    // only uncompressed form may be used, because compressed is protected by patent
    if Buf[Offset] <> 4 then
      raise EScError.Create(seWrongECPointFormat);

    Res.X := TBigInteger.Create(Buf, 1 + Offset, FSize);
    Res.Y := TBigInteger.Create(Buf, 1 + Offset + FSize, FSize);
    if (Res.X.BitCount = 0) and (Res.Y.BitCount = 0) then // infinity
      Res.Z := TBigInteger.Create(0)
    else
      Res.Z := TBigInteger.Create(1);

    // https://www.iacr.org/archive/crypto2000/18800131/18800131.pdf (Section 4.1)
    if not IsPointBelongTo(Res) then
      raise EScError.Create(seWrongECPointFormat);
  except
    Res.Free;
    raise;
  end;
end;

function TScECCryptographyBI.EncodePointToOctetString(P: TScCustomECPoint): TBytes;
var
  sx: TBytes;
begin
  if not IsClass(P, TScECPointBI) then
    raise EScError.Create(seInvalidInputArgs);

  if TScECPointBI(P).Z.BitCount = 0 then begin // infinity
    SetLength(Result, 1);
    Result[0] := 0; // 0 - infinity
    Exit;
  end;

  // only uncompressed form may be used, because compressed is protected by patent
  SetLength(Result, 1 + FSize * 2); // compressed flag, X, Y

  SetLength(sx, 0);
  Result[0] := 4; // 4 - uncompressed form
  sx := TScECPointBI(P).X.GetBytes(FSize);
  Move(sx[0], Result[1], Length(sx));
  sx := TScECPointBI(P).Y.GetBytes(FSize);
  Move(sx[0], Result[1 + FSize], Length(sx));
end;

function TScECCryptographyBI.SignHash(const Hash: TBytes; PrivateKoef: TObject; PublicPoint: TScCustomECPoint): TBytes;
var
  Input: TBigInteger;
  r, k, s, dr, edr, tmp: TBigInteger;
  rPoint: TScECPointBI;
  ASN1Compiler: TScASN1Compiler;
  Writer: TSSH2DataStream;
begin
  if not IsClass(PrivateKoef, TBigInteger) then
    raise EScError.Create(seInvalidInputArgs);

  k := nil;
  r := nil;
  s := nil;
  dr := nil;
  edr := nil;
  tmp := nil;

  if Length(Hash) > FSize then
    Input := TBigInteger.Create(Hash, 0, FSize)
  else
    Input := TBigInteger.Create(Hash);

  try
    repeat
      repeat
        FreeAndNil(k);
        FreeAndNil(r);

        k := GenerateRandomKoef;
        rPoint := TScECPointBI(MulPoint(FG, k));
        try
          r := rPoint.X.Mod_(FN);
        finally
          FreeAndNil(rPoint);
        end;
      until r.BitCount <> 0;

      FreeAndNil(dr);
      FreeAndNil(edr);
      FreeAndNil(s);

      dr := TBigInteger(PrivateKoef).Mul(r);
      tmp := dr.Mod_(FN);
      edr := tmp.Add(Input);
      FreeAndNil(tmp);

      tmp := k;
      k := k.ModInverse(FN);
      FreeAndNil(tmp);

      tmp := k.Mul(edr);
      s := tmp.Mod_(FN);
      FreeAndNil(tmp);
    until s.BitCount <> 0;

    if IsSSHFormat then begin
      Writer := TSSH2DataStream.Create;
      try
        Writer.WriteAsBigInteger(r.GetBytes);
        Writer.WriteAsBigInteger(s.GetBytes);
        Result := Writer.ToBytes;
      finally
        Writer.Free;
      end;
    end
    else begin
      ASN1Compiler := TScASN1Compiler.Create;
      try
        if not ASN1Compiler.Parse(asn1_EC_SIGN_DESC) then
          raise EScError.Create(seWrongDataFormat);

        ASN1Compiler['R'].AsBigInteger := r.GetBytes;
        ASN1Compiler['S'].AsBigInteger := s.GetBytes;

        Result := ASN1Compiler.Build;
      finally
        ASN1Compiler.Free;
      end;
    end;

  finally
    Input.Free;
    k.Free;
    r.Free;
    s.Free;
    dr.Free;
    edr.Free;
    tmp.Free;
  end;
end;

function TScECCryptographyBI.VerifyHash(const Hash, Sign: TBytes; PublicPoint: TScCustomECPoint): boolean;
var
  ASN1Compiler: TScASN1Compiler;
  Input: TBigInteger;
  r, s, w, u1, u2, v, tmp: TBigInteger;
  P, P1, P2: TScECPointBI;
  Reader: TSSH2DataReader;
begin
  Result := False;

  r := nil;
  s := nil;
  w := nil;
  u1 := nil;
  u2 := nil;
  v := nil;
  tmp := nil;
  P1 := nil;
  P2 := nil;
  P := nil;

  if Length(Hash) > FSize then
    Input := TBigInteger.Create(Hash, 0, FSize)
  else
    Input := TBigInteger.Create(Hash);

  try
    if IsSSHFormat then begin
      Reader := TSSH2DataReader.Create(Sign);
      try
        r := Reader.ReadAsBigInteger;
        s := Reader.ReadAsBigInteger;
      finally
        Reader.Free;
      end;
    end
    else begin
      ASN1Compiler := TScASN1Compiler.Create;
      try
        if not ASN1Compiler.Parse(asn1_EC_SIGN_DESC, Sign) then
          raise EScError.Create(seWrongDataFormat);

        r := TBigInteger.Create(ASN1Compiler['R'].AsBigInteger);
        s := TBigInteger.Create(ASN1Compiler['S'].AsBigInteger);
      finally
        ASN1Compiler.Free;
      end;
    end;

    if r.GreaterOrEqual(FN) or r.IsNegative or (r.BitCount = 0) or
       s.GreaterOrEqual(FN) or s.IsNegative or (s.BitCount = 0) then
      Exit;

    w := s.ModInverse(FN);
    tmp := Input.Mul(w);
    u1 := tmp.Mod_(FN);
    FreeAndNil(tmp);

    tmp := r.Mul(w);
    u2 := tmp.Mod_(FN);
    FreeAndNil(tmp);

    P1 := TScECPointBI(MulPoint(FG, u1));
    if P1.Z.BitCount = 0 then
      Exit;
    P2 := TScECPointBI(MulPoint(PublicPoint, u2));
    if P2.Z.BitCount = 0 then
      Exit;

    P := TScECPointBI.Create;
    AddPoints(P1, P2, P);
    if (P.Z = nil) or (P.Z.BitCount = 0) then
      Exit;

    ToAffineCoord(P);
    v := P.X.Mod_(FN);

    Result := v.Equal(r);
  finally
    Input.Free;
    r.Free;
    s.Free;
    w.Free;
    u1.Free;
    u2.Free;
    v.Free;
    tmp.Free;

    P1.Free;
    P2.Free;
    P.Free;
  end;
end;

{ TScECCryptographyFp }

procedure TScECCryptographyFp.Prepare;
begin
  if FModP = nil then
    raise EScError.Create(seECCryptographyNotInitialized);

  FModP.PrepareForBarrettReduction;
end;

procedure TScECCryptographyFp.Invert(P: TScECPointBI);
var
  tmp: TBigInteger;
begin
	if (P.Y.BitCount = 0) or (P.Z.BitCount = 0) then
    Exit;

  tmp := P.Y;
  P.Y := FModP.Minus(P.Y);
  tmp.Free;
end;

procedure TScECCryptographyFp.AddPoints(P1, P2: TScECPointBI; Res: TScECPointBI);
var
  Equal: boolean;
  tmp, n0, n1, n2, n3, n4, n5, n5_2, n6, n7, n8: TBigInteger;
begin
  Equal := (P1 = P2) or P1.Equals(P2);
  if Equal then begin
    DblPoint(P1, Res);
    Exit;
  end;

  if P1.Z.BitCount = 0 then begin
    ClonePoint(P2, Res);
    Exit;
  end;

  if P2.Z.BitCount = 0 then begin
    ClonePoint(P1, Res);
    Exit;
  end;

  tmp := nil;
  n0 := nil;
  n1 := nil;
  n2 := nil;
  n3 := nil;
  n4 := nil;
  n5 := nil;
  n5_2 := nil;
  n6 := nil;
  n7 := nil;
  n8 := nil;
  try
    if P2.Z.BitCount = 1 then begin
      n1 := TBigInteger.Create(P1.X);
      n2 := TBigInteger.Create(P1.Y);
      // n1 = X_a
      // n2 = Y_a
    end
    else begin
      FreeAndNil(n0);
      n0 := P2.Z.ModMul(P2.Z, FModP);
      FreeAndNil(n1);
      n1 := P1.X.ModMul(n0, FModP);
      // n1 = X_a * Z_b^2

      FreeAndNil(tmp);
      tmp := n0;
      n0 := n0.ModMul(P2.Z, FModP);
      FreeAndNil(n2);
      n2 := P1.Y.ModMul(n0, FModP);
      // n2 = Y_a * Z_b^3
    end;

    if P1.Z.BitCount = 1 then begin
      n3 := TBigInteger.Create(P2.X);
      n4 := TBigInteger.Create(P2.Y);
      // n3 = X_b
      // n4 = Y_b
    end
    else begin
      FreeAndNil(n0);
      n0 := P1.Z.ModMul(P1.Z, FModP);
      FreeAndNil(n3);
      n3 := P2.X.ModMul(n0, FModP);
      // n3 = X_b * Z_a^2

      FreeAndNil(tmp);
      tmp := n0;
      n0 := n0.ModMul(P1.Z, FModP);
      FreeAndNil(n4);
      n4 := P2.Y.ModMul(n0, FModP);
      // n4 = Y_b * Z_a^3
    end;

    FreeAndNil(n5);
    n5 := n1.Minus(n3);
    if n5.IsNegative then begin
      FreeAndNil(tmp);
      tmp := n5;
      n5 := n5.Add(FModP);
    end;
    // n5 = n1 - n3

    FreeAndNil(n6);
    n6 := n2.Minus(n4);
    if n6.IsNegative then begin
      FreeAndNil(tmp);
      tmp := n6;
      n6 := n6.Add(FModP);
    end;
    // n6 = n2 - n4

    if n5.BitCount = 0 then begin
      if n6.BitCount = 0 then
        // a is the same point as b
        DblPoint(P1, Res)
      else
        // a is the inverse of b
        SetToInfinity(Res);
      Exit;
    end;

    n7 := n1.Add(n3);
    // n7 = n1 + n3
    n8 := n2.Add(n4);
    // n8 = n2 + n4

    // Z_r
    if (P1.Z.BitCount = 1) and (P2.Z.BitCount = 1) then begin
      FreeAndNil(Res.Z);
      Res.Z := TBigInteger.Create(n5);
    end
    else begin
      FreeAndNil(n0);
      if P1.Z.BitCount = 1 then
        n0 := TBigInteger.Create(P2.Z)
      else
      if P2.Z.BitCount = 1 then
        n0 := TBigInteger.Create(P1.Z)
      else
        n0 := P1.Z.ModMul(P2.Z, FModP);

      FreeAndNil(Res.Z);
      Res.Z := n0.ModMul(n5, FModP);
    end;
    // Z_r = Z_a * Z_b * n5

    // X_r
    FreeAndNil(n0);
    n0 := n6.ModMul(n6, FModP);
    // n0 = n6^2
    n5_2 := n5.ModMul(n5, FModP);
    // n5_2 = n5^2
    FreeAndNil(n3);
    n3 := n7.ModMul(n5_2, FModP);
    // n3 = n5^2 * n7
    FreeAndNil(Res.X);
    Res.X := n0.Minus(n3);
    // X_r = n6^2 - n5^2 * n7
    if Res.X.IsNegative then begin
      FreeAndNil(tmp);
      tmp := Res.X;
      Res.X := Res.X.Add(FModP);
    end;

    FreeAndNil(n0);
    n0 := Res.X.Shl_(1);
    FreeAndNil(tmp);
    tmp := n0;
    n0 := n0.BarrettReduction(FModP);
    // n0 = 2 * X_r

    FreeAndNil(tmp);
    tmp := n0;
    n0 := n3.Minus(n0);
    if n0.IsNegative then begin
      FreeAndNil(tmp);
      tmp := n0;
      n0 := n0.Add(FModP);
    end;
    // n9 = n5^2 * n7 - 2 * X_r = n3 - 2 * X_r

    // Y_r
    FreeAndNil(tmp);
    tmp := n0;
    n0 := n0.ModMul(n6, FModP);
    FreeAndNil(tmp);
    tmp := n5;
    n5 := n5_2.ModMul(n5, FModP);
    // n5 = n5^3
    FreeAndNil(n1);
    n1 := n8.ModMul(n5, FModP);
    FreeAndNil(tmp);
    tmp := n0;
    n0 := n0.Minus(n1);
    if n0.IsNegative then begin
      FreeAndNil(tmp);
      tmp := n0;
      n0 := n0.Add(FModP);
    end;

    if n0.IsOdd then begin
      FreeAndNil(tmp);
      tmp := n0;
      n0 := n0.Add(FModP);
    end;
    // now 0 <= n0 < 2*p, and n0 is even

    FreeAndNil(Res.Y);
    Res.Y := n0.Shr_(1);
    // Y_r = (n6 * n9 - n8 * n5^3) / 2
  finally
    n0.Free;
    n1.Free;
    n2.Free;
    n3.Free;
    n4.Free;
    n5.Free;
    n5_2.Free;
    n6.Free;
    n7.Free;
    n8.Free;
    tmp.Free;
  end;
end;

procedure TScECCryptographyFp.DblPoint(P: TScECPointBI; Res: TScECPointBI);
var
  tmp, n0, n1, n2, n3: TBigInteger;
begin
  if P.Z.BitCount = 0 then begin
    SetToInfinity(Res);
    Exit;
  end;

  tmp := nil;
  n0 := nil;
  n1 := nil;
  n2 := nil;
  n3 := nil;
  try
    if P.Z.BitCount = 1 then begin
      FreeAndNil(n0);
      n0 := P.X.ModMul(P.X, FModP);
      FreeAndNil(n1);
      n1 := n0.Shl_(1);
      FreeAndNil(tmp);
      tmp := n0;
      n0 := n0.Add(n1);
      FreeAndNil(n1);
      n1 := n0.Add(FA);
      FreeAndNil(tmp);
      tmp := n1;
      n1 := n1.BarrettReduction(FModP);
      // n1 = 3 * X_a^2 + a_curve
    end
    else begin
      FreeAndNil(n0);
      n0 := P.X.ModMul(P.X, FModP);
      FreeAndNil(n1);
      n1 := n0.Shl_(1);
      FreeAndNil(tmp);
      tmp := n0;
      n0 := n0.Add(n1);
      FreeAndNil(n1);
      n1 := P.Z.ModMul(P.Z, FModP);
      FreeAndNil(tmp);
      tmp := n1;
      n1 := n1.ModMul(n1, FModP);
      FreeAndNil(tmp);
      tmp := n1;
      n1 := n1.ModMul(FA, FModP);
      FreeAndNil(tmp);
      tmp := n1;
      n1 := n1.Add(n0);
      FreeAndNil(tmp);
      tmp := n1;
      n1 := n1.BarrettReduction(FModP);
      // n1 = 3 * X_a^2 + a_curve * Z_a^4
    end;

    // Z_r
    FreeAndNil(n0);
    if P.Z.BitCount = 1 then
      n0 := TBigInteger.Create(P.Y)
    else
      n0 := P.Y.ModMul(P.Z, FModP);

    FreeAndNil(Res.Z);
    Res.Z := n0.Shl_(1);
    FreeAndNil(tmp);
    tmp := Res.Z;
    Res.Z := Res.Z.BarrettReduction(FModP);
    // Z_r = 2 * Y_a * Z_a

    n3 := P.Y.ModMul(P.Y, FModP);
    n2 := P.X.ModMul(n3, FModP);
    FreeAndNil(tmp);
    tmp := n2;
    n2 := n2.Shl_(2);
    FreeAndNil(tmp);
    tmp := n2;
    n2 := n2.BarrettReduction(FModP);
    // n2 = 4 * X_a * Y_a^2

    // X_r
    FreeAndNil(n0);
    n0 := n2.Shl_(1);
    FreeAndNil(tmp);
    tmp := n1.ModMul(n1, FModP);

    FreeAndNil(Res.X);
    Res.X := tmp.Minus(n0);
    FreeAndNil(tmp);
    tmp := Res.X;
    Res.X := Res.X.Mod_(FModP); // can be negative
    // X_r = n1^2 - 2 * n2

    FreeAndNil(n0);
    n0 := n3.ModMul(n3, FModP);
    FreeAndNil(n3);
    n3 := n0.Shl_(3);
    FreeAndNil(tmp);
    tmp := n3;
    n3 := n3.BarrettReduction(FModP);
    // n3 = 8 * Y_a^4

    // Y_r
    FreeAndNil(n0);
    n0 := n2.Minus(Res.X);
    if n0.IsNegative then begin
      FreeAndNil(tmp);
      tmp := n0;
      n0 := n0.Add(FModP);
    end;

    FreeAndNil(tmp);
    tmp := n1.ModMul(n0, FModP);

    FreeAndNil(Res.Y);
    Res.Y := tmp.Minus(n3);
    if Res.Y.IsNegative then begin
      FreeAndNil(tmp);
      tmp := Res.Y;
      Res.Y := Res.Y.Add(FModP);
    end;
    // Y_r = n1 * (n2 - X_r) - n3
  finally
    n0.Free;
    n1.Free;
    n2.Free;
    n3.Free;
    tmp.Free;
  end;
end;

function TScECCryptographyFp.IsPointBelongTo(Point: TScECPointBI): boolean;
var
  Three: TBigInteger;
  XCube, AX, X1, X2, X3, Ysqr: TBigInteger;
begin
  Assert(FModP <> nil);
  Assert(FA <> nil);
  Assert(FB <> nil);
  Assert(Point.X <> nil);
  Assert(Point.Y <> nil);

  XCube := nil;
  AX := nil;
  X1 := nil;
  X2 := nil;
  X3 := nil;
  Ysqr := nil;
  Three := TBigInteger.Create(3);
  try
    XCube := Point.X.ModPow(Three, FModP); // X^3 mod P
    AX := FA.ModMul(Point.X, FModP);       // AX mod P
    X1 := XCube.Add(AX);                   // X^3 + AX
    X2 := X1.Add(FB);                      // X^3 + AX + B
    X3 := X2.Mod_(FModP);                  // (X^3 + AX + B) mod P
    Ysqr := Point.Y.ModMul(Point.Y, FModP);// Y^2 mod P

    Result := X3.Equal(Ysqr);
  finally
    Three.Free;
    XCube.Free;
    AX.Free;
    X1.Free;
    X2.Free;
    X3.Free;
    Ysqr.Free;
  end;
end;

procedure TScECCryptographyFp.ToAffineCoord(P: TScECPointBI);
var
  Z1, Z2, Z3, tmp: TBigInteger;
begin
  if (P.Z = nil) or (P.Z.BitCount = 0) then
    raise EScError.Create(sePointInfinitive);

  // transform  (X, Y, Z)  into  (x, y) := (X/Z^2, Y/Z^3)
  if P.Z.BitCount = 1 then
    Exit;

  Z2 := nil;
  Z3 := nil;
  tmp := nil;
  Z1 := P.Z.ModInverse(FModP);
  try
    Z2 := Z1.ModMul(Z1, FModP);
    tmp := P.X;
    P.X := P.X.ModMul(Z2, FModP);
    FreeAndNil(tmp);

    Z3 := Z2.ModMul(Z1, FModP);
    tmp := P.Y;
    P.Y := P.Y.ModMul(Z3, FModP);
    FreeAndNil(tmp);

    FreeAndNil(P.Z);
    P.Z := TBigInteger.Create(1);
  finally
    Z1.Free;
    Z2.Free;
    Z3.Free;
    tmp.Free;
  end;
end;

{ TScECCryptographyF2m }

procedure TScECCryptographyF2m.Prepare;
begin
  if (FModP = nil) or (FA = nil) then
    raise EScError.Create(seECCryptographyNotInitialized);

  FModP.PrepareForGF2mCalc;
  FABitCount := FA.BitCount;
end;

procedure TScECCryptographyF2m.Invert(P: TScECPointBI);
begin
  if P.Z.BitCount = 0 then
    Exit;

  if P.Z.BitCount > 1 then
    ToAffineCoord(P);

  P.Y.XorSelf(P.X);
end;

procedure TScECCryptographyF2m.AddPoints(P1, P2: TScECPointBI; Res: TScECPointBI);
begin
  if (FABitCount = 0) or (FABitCount = 1) then
    AddPointsGF2mLD(P1, P2, Res)
  else
    AddPointsGF2m(P1, P2, Res);
end;

procedure TScECCryptographyF2m.DblPoint(P: TScECPointBI; Res: TScECPointBI);
begin
  if (FABitCount = 0) or (FABitCount = 1) then
    DblPointGF2mLD(P, Res)
  else
    DblPointGF2m(P, Res);
end;

procedure TScECCryptographyF2m.AddPointsGF2m(P1, P2: TScECPointBI; Res: TScECPointBI);
var
  x1, y1, x12, y12, x1Sqr, x3, y3, l, tmp: TBigInteger;
begin
  if P1.Z.BitCount = 0 then begin
    ClonePoint(P2, Res);
    Exit;
  end;

  if P2.Z.BitCount = 0 then begin
    ClonePoint(P1, Res);
    Exit;
  end;

  x12 := nil;
  y12 := nil;
  x1Sqr := nil;
  x3 := nil;
  y3 := nil;
  l := nil;
  tmp := nil;
  x1 := TBigInteger.Create(P1.X);
  y1 := TBigInteger.Create(P1.Y);
  try
    if not x1.Equal(P2.X) then begin
      x12 := x1.Xor_(P2.X);
      y12 := y1.Xor_(P2.Y);
      l := y12.ModDiv_GF2m(x12, FModP); // l = (y1 + y2)/(x1 + x2)

      x3 := l.ModSqr_GF2m(FModP);
      x3.XorSelf(l);                    // x3 = l^2 + l
      x3.XorSelf(FA);                   // x3 = l^2 + l + a
      FreeAndNil(Res.X);
      Res.X := x3.Xor_(x12);            // x3 = l^2 + l + a + x1 + x2

      y3 := x1.Xor_(Res.X);
      tmp := y3;
      y3 := y3.ModMul_GF2m(l, FModP);   // y3 = l(x1 + x3)
      FreeAndNil(tmp);
      y3.XorSelf(Res.X);                // y3 = l(x1 + x3) + x3

      FreeAndNil(Res.Y);
      Res.Y := y3.Xor_(y1);             // y3 = l(x1 + x3) + x3 + y1
    end
    else begin
      if not y1.Equal(P2.Y) or (P2.X.BitCount = 0) then begin
        SetToInfinity(Res);
        Exit;
      end;

      l := y1.ModDiv_GF2m(x1, FModP);
      l.XorSelf(x1);                    // l = y1/x1 + x1

      x3 := l.ModSqr_GF2m(FModP);
      x3.XorSelf(l);                    // x3 = l^2 + l
      FreeAndNil(Res.X);
      Res.X := x3.Xor_(FA);             // x3 = l^2 + l + a

      y3 := Res.X.ModMul_GF2m(l, FModP);
      y3.XorSelf(Res.X);                // y3 = l(x3) + x3
      x1Sqr := x1.ModSqr_GF2m(FModP);

      FreeAndNil(Res.Y);
      Res.Y := y3.Xor_(x1Sqr);          // y3 = l(x3) + x3 + x1^2
    end;

    FreeAndNil(Res.Z);
    Res.Z := TBigInteger.Create(1);
  finally
    x1.Free;
    y1.Free;
    x12.Free;
    y12.Free;
    x1Sqr.Free;
    x3.Free;
    y3.Free;
    l.Free;
    tmp.Free;
  end;
end;

procedure TScECCryptographyF2m.DblPointGF2m(P: TScECPointBI; Res: TScECPointBI);
begin
  AddPointsGF2m(P, P, Res);
end;

procedure TScECCryptographyF2m.AddPointsGF2mLD(P1, P2: TScECPointBI; Res: TScECPointBI);
var
  x2, y2, T1, T2, A1, B1, C1, D1, E1, Z3Sql: TBigInteger;
begin
  // in Lopez-Dahab coordinates
  // http://crypto.cs.mcgill.ca/~simonpie/webdav/ipad/EBook/Crypto/Guide to Elliptic Curve Cryptography - D. Hankerson, A. Menezes, S. Vanstone.pdf

  if P1.Z.BitCount = 0 then begin
    ClonePoint(P2, Res);
    Exit;
  end;

  if P2.Z.BitCount = 0 then begin
    ClonePoint(P1, Res);
    Exit;
  end;

  if P2.Z.BitCount <> 1 then
    raise EScError.Create(seInvalidInputArgs);

  x2 := nil;
  y2 := nil;
  T1 := nil;
  T2 := nil;
  A1 := nil;
  B1 := nil;
  C1 := nil;
  D1 := nil;
  E1 := nil;
  Z3Sql := nil;
  try
    T1 := P1.Z.ModMul_GF2m(P2.X, FModP);  // X2*Z1
    B1 := P1.X.Xor_(T1);                  // B = X2*Z1 + X1
    FreeAndNil(T1);
    T1 := P1.Z.ModSqr_GF2m(FModP);        // Z1^2
    C1 := P1.Z.ModMul_GF2m(B1, FModP);    // C = Z1*B
    T2 := T1.ModMul_GF2m(P2.Y, FModP);    // Y2*Z1^2
    A1 := P1.Y.Xor_(T2);                  // A = Y2*Z1^2 + Y1
    if B1.BitCount = 0 then begin
      if A1.BitCount = 0 then
        DblPointGF2mLD(P2, Res)
      else
        SetToInfinity(Res);
      Exit;
    end;

    x2 := TBigInteger.Create(P2.X);
    y2 := TBigInteger.Create(P2.Y);

    FreeAndNil(Res.Z);
    Res.Z := C1.ModSqr_GF2m(FModP);       // C^2

    E1 := C1.ModMul_GF2m(A1, FModP);      // E = A*C
    if FA.BitCount = 1 then
      C1.XorSelf(T1);                     // C + a*Z1^2

    FreeAndNil(T1);
    T1 := B1.ModSqr_GF2m(FModP);          // B^2
    D1 := T1.ModMul_GF2m(C1, FModP);      // D = B^2*(C + a*Z1^2)
    FreeAndNil(T1);
    T1 := A1.ModSqr_GF2m(FModP);          // A^2

    FreeAndNil(Res.X);
    Res.X := D1.Xor_(T1);                 // A^2 + D
    Res.X.XorSelf(E1);                    // A^2 + D + E

    FreeAndNil(T1);
    T1 := x2.ModMul_GF2m(Res.Z, FModP);   // X2*Z3
    T1.XorSelf(Res.X);                    // F = X3 + X2*Z3
    E1.XorSelf(Res.Z);                    // E + Z3

    FreeAndNil(Res.Y);
    Res.Y := E1.ModMul_GF2m(T1, FModP);   // (E + Z3)*F
    FreeAndNil(T1);
    T1 := x2.Xor_(y2);                    // X2 + Y2
    Z3Sql := Res.Z.ModSqr_GF2m(FModP);    // Z3^2
    FreeAndNil(T2);
    T2 := Z3Sql.ModMul_GF2m(T1, FModP);   // (X2 + Y2)*Z3^2
    Res.Y.XorSelf(T2);                    // (E + Z3)*F + G
  finally
    x2.Free;
    y2.Free;
    T1.Free;
    T2.Free;
    A1.Free;
    B1.Free;
    C1.Free;
    D1.Free;
    E1.Free;
    Z3Sql.Free;
  end;
end;

procedure TScECCryptographyF2m.DblPointGF2mLD(P: TScECPointBI; Res: TScECPointBI);
var
  T1, T2, T3: TBigInteger;
begin
  // in Lopez-Dahab coordinates

  if P.Z.BitCount = 0 then begin
    SetToInfinity(Res);
    Exit;
  end;

  T1 := nil;
  T2 := nil;
  T3 := nil;
  try
    T1 := P.Z.ModSqr_GF2m(FModP);       // Z1^2
    T2 := P.X.ModSqr_GF2m(FModP);       // X1^2
    FreeAndNil(Res.Z);
    Res.Z := T1.ModMul_GF2m(T2, FModP); // X1^2 * 2

    T3 := T2.ModSqr_GF2m(FModP);        // X1^4
    FreeAndNil(T2);
    T2 := T1;
    T1 := T1.ModSqr_GF2m(FModP);        // Z1^4
    FreeAndNil(T2);
    T2 := T1.ModMul_GF2m(FB, FModP);    // b*Z1^4

    FreeAndNil(Res.X);
    Res.X := T3.Xor_(T2);               // X1^4 + b*Z1^4

    FreeAndNil(T1);
    T1 := P.Y.ModSqr_GF2m(FModP);       // Y1^2
    if FA.BitCount = 1 then
      T1.XorSelf(Res.Z);                // a*Z3 + Y1^2
    T1.XorSelf(T2);                     // a*Z3 + Y1^2 + b*Z1^4
    FreeAndNil(T3);
    T3 := Res.X.ModMul_GF2m(T1, FModP); // X3*(a*Z3 + Y1^2 + b*Z1^4)
    FreeAndNil(T1);
    T1 := T2.ModMul_GF2m(Res.Z, FModP); // b*Z1^4*Z3

    FreeAndNil(Res.Y);
    Res.Y := T3.Xor_(T1);               // b*Z1^4*Z3 + X3*(a*Z3 + Y1^2 + b*Z1^4)
  finally
    T1.Free;
    T2.Free;
    T3.Free;
  end;
end;

function TScECCryptographyF2m.IsPointBelongTo(Point: TScECPointBI): boolean;
var
  Ysqr, f, tmp: TBigInteger;
begin
  Assert(FModP <> nil);
  Assert(FA <> nil);
  Assert(FB <> nil);
  Assert(Point.X <> nil);
  Assert(Point.Y <> nil);

  f := nil;
  Ysqr := nil;
  tmp := nil;
  try
    // We have a curve defined by a Weierstrass equation
    // y^2 + x*y = x^3 + a*x^2 + b.
    // <=> x^3 + a*x^2 + x*y + b + y^2 = 0
    // <=> ((x + a) * x + y ) * x + b + y^2 = 0

    f := Point.X.Xor_(FA);              // x + a
    tmp := f;
    f := f.ModMul_GF2m(Point.X, FModP); // (x + a) * x
    FreeAndNil(tmp);
    f.XorSelf(Point.Y);                 // (x + a) * x + y
    tmp := f;
    f := f.ModMul_GF2m(Point.X, FModP); // ((x + a) * x + y) * x
    FreeAndNil(tmp);
    f.XorSelf(FB);                      // ((x + a) * x + y) * x + b
    Ysqr := Point.Y.ModSqr_GF2m(FModP);
    f.XorSelf(Ysqr);                    // ((x + a) * x + y) * x + b + y^2
    Result := f.BitCount = 0;
  finally
    f.Free;
    Ysqr.Free;
    tmp.Free;
  end;
end;

procedure TScECCryptographyF2m.ToAffineCoord(P: TScECPointBI);
var
  Z1, Z2, tmp: TBigInteger;
begin
  if (P.Z = nil) or (P.Z.BitCount = 0) then
    raise EScError.Create(sePointInfinitive);

  // transform  (X, Y, Z)  into  (x, y) := (X/Z, Y/Z^2)
  if P.Z.BitCount = 1 then
    Exit;

  Z2 := nil;
  tmp := nil;
  Z1 := P.Z.ModInv_GF2m(FModP);
  try
    tmp := P.X;
    P.X := P.X.ModMul_GF2m(Z1, FModP);
    FreeAndNil(tmp);

    Z2 := Z1.ModMul_GF2m(Z1, FModP);
    tmp := P.Y;
    P.Y := P.Y.ModMul_GF2m(Z2, FModP);
    FreeAndNil(tmp);

    FreeAndNil(P.Z);
    P.Z := TBigInteger.Create(1);
  finally
    Z1.Free;
    Z2.Free;
    tmp.Free;
  end;
end;

function TScECCryptographyF2m.NeedConvertToAffineCoordOnPrecomp: boolean;
begin
  // to optimaze calculations in AddPoints
  Result := True;
end;

{ TScECCryptographyX25519 }

constructor TScECCryptographyX25519.Create;
begin
  inherited Create;

  FSize := 32;
  FPreHashType := phPure;
end;

constructor TScECCryptographyX25519.Create(const ECParameters: TScECParameters);
begin
  Create;
end;

function TScECCryptographyX25519.Equals(ECCryptography: TScCustomECCryptography): boolean;
begin
  Result := (ClassType = ECCryptography.ClassType) and
    (FSize = ECCryptography.FSize);
end;

procedure TScECCryptographyX25519.Assign(Source: TScCustomECCryptography);
begin
  if Source = nil then
    Exit;

  inherited Assign(Source);
end;

function TScECCryptographyX25519.PublicPointClass: TScECPointClass;
begin
  Result := TScECPointX25519;
end;

function TScECCryptographyX25519.PrivateKoefClass: TPersistentClass;
begin
  Result := T32BytesField;
end;

function TScECCryptographyX25519.GetBitCount: integer;
begin
  Result := FSize * 8;
end;

procedure TScECCryptographyX25519.GeneratePrivateKey(var ECData: TScECData; ARandom: IScRandom = nil);
begin
  if ARandom = nil then
    ARandom := Random;
  if ARandom = nil then
    raise Exception.Create(SInternalError);

  ECData.PrivateKoef := T32BytesField.Create;
  ARandom.Random(@T32BytesField(ECData.PrivateKoef).BytesField[0], sizeof(T32Bytes));

  ECData.PublicPoint := PublicFromPrivate(T32BytesField(ECData.PrivateKoef));
end;

function TScECCryptographyX25519.PublicFromPrivate(Koef: T32BytesField): TScECPointX25519;
const
  GPoint: T32Bytes = (9, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0);
begin
  Result := TScECPointX25519.Create;
  try
    case FPreHashType of
      phPure:
        X25519ScalarMult(Result.PointField, Koef.BytesField, GPoint);
      phHash:
        X25519PublicFromPrivate(Koef.BytesField, Result.PointField);
    else
      raise EScError.Create(sePreHashTypeNotSupported);
    end;
  except
    Result.Free;
    raise;
  end;
end;

function TScECCryptographyX25519.MulPoint(P1: TScCustomECPoint; Koef: TObject): TScCustomECPoint;
begin
  if not IsClass(P1, TScECPointX25519) or not IsClass(Koef, T32BytesField) then
    raise EScError.Create(seInvalidInputArgs);

  Result := TScECPointX25519.Create;
  try
    X25519ScalarMult(TScECPointX25519(Result).PointField, T32BytesField(Koef).BytesField, TScECPointX25519(P1).PointField);
  except
    Result.Free;
    raise;
  end;
end;

function TScECCryptographyX25519.DecodePointFromOctetString(const Buf: TBytes; Offset, Count: integer): TScCustomECPoint;
begin
  if Count <> FSize then
    raise EScError.Create(seWrongECPointFormat);

  Result := TScECPointX25519.Create;
  try
    Move(Buf[Offset], TScECPointX25519(Result).PointField[0], Count);
  except
    Result.Free;
    raise;
  end;
end;

function TScECCryptographyX25519.EncodePointToOctetString(P: TScCustomECPoint): TBytes;
begin
  if not IsClass(P, TScECPointX25519) then
    raise EScError.Create(seInvalidInputArgs);

  SetLength(Result, FSize);
  Move(TScECPointX25519(P).PointField[0], Result[0], FSize);
end;

function TScECCryptographyX25519.SignHash(const Data: TBytes; PrivateKoef: TObject; PublicPoint: TScCustomECPoint): TBytes;
begin
  if not IsClass(PublicPoint, TScECPointX25519) or not IsClass(PrivateKoef, T32BytesField) then
    raise EScError.Create(seInvalidInputArgs);

  Result := X25519Sign(Data, T32BytesField(PrivateKoef).BytesField, TScECPointX25519(PublicPoint).PointField);
end;

function TScECCryptographyX25519.VerifyHash(const Data, Sign: TBytes; PublicPoint: TScCustomECPoint): boolean;
begin
  if not IsClass(PublicPoint, TScECPointX25519) then
    raise EScError.Create(seInvalidInputArgs);

  Result := X25519VerifySign(Data, Sign, TScECPointX25519(PublicPoint).PointField);
end;

{ TScKeyDerivationProcessor }

class function TScKeyDerivationProcessor.PBKDF1FromPassword(const HashAlgorithm: TScHashAlgorithm;
  const Password: string; const Salt: TBytes;
  KeyLen: integer; Iterations: integer; IETFFormat: boolean): TBytes;
var
  csp: THashAlgorithm;
  PassBuf, InitBuf, HashBuf, TmpBuf: TBytes;
  SaltLen, HashLen, Offset: integer;
  i: integer;
begin
  csp := CipherFactory.CreateHash(HashAlgorithm);
  try
    PassBuf := Encoding.UTF8.GetBytes(Password);
    SaltLen := Min(Length(Salt), 8);
    SetLength(InitBuf, Length(PassBuf) + SaltLen);
    if Length(PassBuf) > 0 then
      Move(PassBuf[0], InitBuf[0], Length(PassBuf));
    if SaltLen > 0 then
      Move(Salt[0], InitBuf[Length(PassBuf)], SaltLen);

    HashLen := csp.HashSize;
    SetLength(Result, KeyLen);

    HashBuf := InitBuf;
    for i := 0 to Iterations - 1 do
      HashBuf := csp.ComputeHash(HashBuf);
    Move(HashBuf[0], Result[0], Min(KeyLen, HashLen));

    if HashLen < KeyLen then begin
      SetLength(TmpBuf, Length(InitBuf) + HashLen);
      if Length(InitBuf) > 0 then
        if IETFFormat then
          Move(InitBuf[0], TmpBuf[0], Length(InitBuf))
        else
          Move(InitBuf[0], TmpBuf[HashLen], Length(InitBuf));

      Offset := HashLen;
      while Offset < KeyLen do begin
        if IETFFormat then
          Move(HashBuf[0], TmpBuf[Length(InitBuf)], HashLen)
        else
          Move(HashBuf[0], TmpBuf[0], HashLen);

        HashBuf := TmpBuf;
        for i := 0 to Iterations - 1 do
          HashBuf := csp.ComputeHash(HashBuf);
        Move(HashBuf[0], Result[Offset], Min(KeyLen - Offset, HashLen));
        Offset := Offset + HashLen;
      end;
    end;
  finally
    csp.Free;
  end;

  if Length(InitBuf) > 0 then
    FillChar(InitBuf[0], Length(InitBuf), 0);
  if Length(PassBuf) > 0 then
    FillChar(PassBuf[0], Length(PassBuf), 0);
  FillChar(HashBuf[0], Length(HashBuf), 0);
end;

class function TScKeyDerivationProcessor.PBKDF2FromPassword(const Password: string; const Salt: TBytes;
  KeyLen: integer; Iterations: integer): TBytes;
var
  HMac: THMAC;
  InitBuf, HashBuf: TBytes;
  SaltLen, HashLen, Offset: integer;
  n: cardinal;
  i, j: integer;
begin
  SaltLen := Length(Salt);
  SetLength(InitBuf, SaltLen + 4);
  Buffer.BlockCopy(Salt, 0, InitBuf, 0, SaltLen);

  SetLength(Result, KeyLen);
  SetLength(HashBuf, 0);
  Offset := 0;

  HMac := THMAC.Create(THash_SHA1, Encoding.UTF8.GetBytes(Password));
  try
    n := 1;
    HashLen := HMac.HashSize;
    while KeyLen > 0 do begin
      if KeyLen < HashLen then
        HashLen := KeyLen;

      InitBuf[SaltLen] := byte(n shr 24);
      InitBuf[SaltLen + 1] := byte(n shr 16);
      InitBuf[SaltLen + 2] := byte(n shr 8);
      InitBuf[SaltLen + 3] := byte(n);
      HashBuf := HMac.ComputeHash(InitBuf);
      Buffer.BlockCopy(HashBuf, 0, Result, Offset, HashLen);

      for i := 1 to Iterations - 1 do begin
        HashBuf := HMac.ComputeHash(HashBuf);
        for j := 0 to HashLen - 1 do
          Result[Offset + j] := Result[Offset + j] xor HashBuf[j];
      end;

      KeyLen := KeyLen - HashLen;
      Offset := Offset + HashLen;
      Inc(n);
    end;
  finally
    HMac.Free;
  end;
end;

class function TScKeyDerivationProcessor.PKCS12KeyDeriveFromPassword(const HashAlgorithm: TScHashAlgorithm;
  const Password: string; const Salt: TBytes;
  KeyPurpose: TKeyPurpose; Iterations: integer;
  KeyLen: integer): TBytes;
var
  csp: THashAlgorithm;
  uSize, vSize: integer;
  D: byte;
  Divers: TBytes;
  PassBuf, IBuf: TBytes;
  PassLen, SaltLen, PLen, SLen, ILen: integer;
  Offset, ResOffset: integer;
  Ai, B, tmp: TBytes;
  Bi, Bi1, Ij, IjBi: TBigInteger;
  i, j: integer;
begin
  // https://tools.ietf.org/html/rfc7292#appendix-B.2

  csp := CipherFactory.CreateHash(HashAlgorithm);
  try
    uSize := csp.HashSize;
    if uSize > 32 then
      vSize := 128  // 1024 bit
    else
      vSize := 64; // 512 bit

    case KeyPurpose of
      kpEncKey:
        D := 1;
      kpIV:
        D := 2;
      kpMacKey:
        D := 3;
    else
      raise ArgumentException.Create('KeyPurpose');
    end;

  {$IFNDEF VER10P}
    SetLength(tmp, 0);
    SetLength(Ai, 0);
    SetLength(PassBuf, 0);
  {$ENDIF}

    SetLength(Divers, vSize);
    FillChar(Divers[0], Length(Divers), D);

    SaltLen := Length(Salt);
    SLen := ((SaltLen + vSize - 1) div vSize) * vSize;

    PassBuf := Encoding.BigEndianUnicode.GetBytes(Password);
    PassLen := Length(PassBuf);
    if PassLen > 0 then begin
      Inc(PassLen, 2{#0});
      PLen := ((PassLen + vSize - 1) div vSize) * vSize;
    end
    else
      PLen := 0;

    ILen := SLen + PLen;
    SetLength(IBuf, ILen);

    Offset := 0;
    if SaltLen > 0 then begin
      while (Offset + SaltLen) <= SLen do begin
        Move(Salt[0], IBuf[Offset], SaltLen);
        Inc(Offset, SaltLen);
      end;
      if SLen - Offset > 0 then
        Move(Salt[0], IBuf[Offset], SLen - Offset);
      Offset := SLen;
    end;

    if PassLen > 0 then begin
      while (Offset + PassLen) <= ILen do begin
        Move(PassBuf[0], IBuf[Offset], Length(PassBuf));
        Inc(Offset, PassLen);
      end;
      if ILen - Offset > 0 then
        Move(PassBuf[0], IBuf[Offset], ILen - Offset);
    end;

    SetLength(B, vSize);
    SetLength(Result, KeyLen);

    ResOffset := 0;
    while True do begin
      csp.TransformBlock(Divers, 0, vSize);
      csp.TransformFinalBlock(IBuf, 0, ILen);
      Ai := csp.Hash;
      csp.Initialize;
      for i := 0 to Iterations - 2 do
        Ai := csp.ComputeHash(Ai);

      Move(Ai[0], Result[ResOffset], Min(KeyLen, uSize));
      if uSize >= KeyLen then
        Break;

      Dec(KeyLen, uSize);
      Inc(ResOffset, uSize);

      Offset := 0;
      while (Offset + uSize) <= vSize do begin
        Move(Ai[0], B[Offset], uSize);
        Inc(Offset, uSize);
      end;
      if vSize - Offset > 0 then
        Move(Ai[0], B[Offset], vSize - Offset);

      Bi1 := nil;
      Ij := nil;
      IjBi := nil;
      Bi := TBigInteger.Create(B);
      try
        Bi1 := Bi.Add(1);

        j := 0;
        while j < ILen do begin
          FreeAndNil(Ij);
          Ij := TBigInteger.Create(IBuf, j, vSize);
          FreeAndNil(IjBi);
          IjBi := Ij.Add(Bi1);
          tmp := IjBi.GetBytes;

          if Length(tmp) > vSize then
            Move(tmp[1], IBuf[j], vSize)
          else
          if Length(tmp) < vSize then begin
            FillChar(IBuf[j], vSize - Length(tmp), 0);
            Move(tmp[0], IBuf[j + vSize - Length(tmp)], Length(tmp));
          end
          else
            Move(tmp[0], IBuf[j], vSize);

          Inc(j, vSize);
        end;
      finally
        Bi.Free;
        Bi1.Free;
        Ij.Free;
        IjBi.Free;
      end;
    end;
  finally
    csp.Free;
  end;

  if Length(PassBuf) > 0 then
    FillChar(PassBuf[0], Length(PassBuf), 0);
  if Length(IBuf) > 0 then
    FillChar(IBuf[0], Length(IBuf), 0);
end;

(*
class function TScKeyDerivationProcessor.BCrypt(const Password: string; const Salt: TBytes; Iterations: integer): TBytes;
const
  // 'OrpheanBeholderScryDoubt'
  CText: array [0..23] of byte =
    ($4F, $72, $70, $68, $65, $61, $6E, $42, $65, $68, $6F, $6C,
     $64, $65, $72, $53, $63, $72, $79, $44, $6F, $75, $62, $74);

var
  Cipher: TCipher_Blowfish;
  Key, CData: TBytes;
  Rounds: integer;
  i: integer;
begin
  if not (Iterations in [4..31]) or (Length(Salt) <> 16) then
    raise EScError.Create(seUnsupportedBCryptParameters);

  Cipher := TCipher_Blowfish.Create;
  try
    Cipher.Mode := cmECB;

    Key := Encoding.UTF8.GetBytes(Password);
    Cipher.InitEx(Key, Salt);

    Rounds := 1 shl Iterations; // 2^Iterations
    for i := 1 to Rounds do begin
      Cipher.InitEx(Key);
      Cipher.InitEx(Salt);
    end;

    SetLength(CData, Length(CText));
    Move(CText[0], CData[0], Length(CText));

    for i := 1 to 64 do
      Cipher.EncodeBuffer(@CData[0], 0, Length(CData), @CData[0], 0);

    SetLength(Result, Length(CData));
    Move(CData[0], Result[0], Length(CData));
  finally
    Cipher.Free;
  end;
end;
*)

class function TScKeyDerivationProcessor.BCryptHash(const Key, Salt: TBytes): TBytes;
const
  // 'cyxOmorhcitawolBhsiftawSanyDetim'
  CText: array [0..31] of byte =
    ($63, $79, $78, $4F, $6D, $6F, $72, $68, $63, $69, $74, $61, $77, $6F, $6C, $42,
     $68, $73, $69, $66, $74, $61, $77, $53, $61, $6E, $79, $44, $65, $74, $69, $6D);

var
  Cipher: TCipher_BlowfishLE;
  i: integer;
begin
  Cipher := TCipher_BlowfishLE.Create;
  try
    Cipher.Mode := cmECB;

    Cipher.InitEx(Key, Salt);

    // Original bcrypt replaces this fixed loop count with the variable cost.
    // OpenSSH instead iterates the whole thing more than once if it wants extra rounds.
    for i := 1 to 64 do begin
      Cipher.InitEx(Salt);
      Cipher.InitEx(Key);
    end;

    SetLength(Result, Length(CText));
    Move(CText[0], Result[0], Length(CText));

    for i := 1 to 64 do
      Cipher.EncodeBuffer(@Result[0], 0, Length(Result), @Result[0], 0);
  finally
    Cipher.Free;
  end;
end;

class function TScKeyDerivationProcessor.BCryptGenBlock(const HashedPassphrase, Salt: TBytes; Counter: integer): TBytes;
var
  csp: THashAlgorithm;
  TmpBuf: array[0..3] of byte;
begin
  csp := THash_SHA2_512.Create;
  try
    if Counter <> 0 then begin
      csp.TransformBlock(Salt, 0, Length(Salt));

      PutIntBE(Counter, TValueArr(@TmpBuf), 0);
      csp.TransformFinalBlock(TValueArr(@TmpBuf), 4);
    end
    else
      csp.ComputeHash(Salt);

    Result := BCryptHash(HashedPassphrase, csp.Hash);
  finally
    csp.Free;
  end;
end;

class function TScKeyDerivationProcessor.OpenSSHBCrypt(const Password: string; const Salt: TBytes; Iterations: integer; OutSize: integer): TBytes;
var
  csp: THashAlgorithm;
  HashedPassphrase, OutBlock, ThisSalt, Block: TBytes;
  Modulus, Residue, Round: integer;
  i, j: integer;
begin
  csp := THash_SHA2_512.Create;
  try
    csp.ComputeHash(Encoding.UTF8.GetBytes(Password));
    SetLength(HashedPassphrase, Length(csp.Hash));
    Buffer.BlockCopy(csp.Hash, 0, HashedPassphrase, 0, Length(csp.Hash));
  finally
    csp.Free;
  end;

  SetLength(Result, OutSize);

  SetLength(Block, 0);
  SetLength(ThisSalt, 0);

  SetLength(OutBlock, 32);
  Modulus := (OutSize + 31) div 32;
  for Residue := 0 to Modulus - 1 do begin
    // Our output block of data is the XOR of all blocks generated by bcrypt in the following loop
    FillChar(OutBlock[0], 32, 0);

    ThisSalt := Salt;
    for Round := 0 to Iterations - 1 do begin
      if Round = 0 then
        Block := BCryptGenBlock(HashedPassphrase, ThisSalt, Residue + 1)
      else
        Block := BCryptGenBlock(HashedPassphrase, ThisSalt, 0);

      // Each subsequent bcrypt call reuses the previous one's output as its salt
      ThisSalt := Block;
      for i := 0 to 31 do
        OutBlock[i] := OutBlock[i] xor Block[i];
    end;

    i := Residue;
    j := 0;
    while i < OutSize do begin
      Result[i] := OutBlock[j];
      Inc(i, Modulus);
      Inc(j);
    end
  end;

  FillChar(HashedPassphrase[0], Length(HashedPassphrase), 0);
end;

class function TScKeyDerivationProcessor.CreateCipherByOId(const OId: string;
  const Params: TBytes; const Password: string): TSymmetricAlgorithm;
var
  ASN1Compiler: TScASN1Compiler;
  Key, Salt, IV: TBytes;
  Iterations: integer;
  Algo: TScSymmetricAlgorithm;
  AlgoEx: TScSymmetricAlgorithmEx;
  CipherMode: TCipherMode;
  KeySize, IVSize: integer;
  HashAlgorithm: TScHashAlgorithm;
begin
  Result := nil;
  SetLength(Key, 0);
  SetLength(Salt, 0);
  SetLength(IV, 0);

  ASN1Compiler := TScASN1Compiler.Create;
  try
    if OId = OID_PBES2 then begin
      if not ASN1Compiler.Parse(asn1_PBES2_PARAMS_DESC, Params) then
        Exit;

      Salt := ASN1Compiler['Salt'].AsBytes;
      Iterations := ASN1Compiler['Iters'].AsInteger;
      IV := ASN1Compiler['IV'].AsBytes;
      Algo := CipherFactory.OidToEncryptionAlgorithm(ASN1Compiler['Algo'].AsOID);

      // With a decrypted PBKDF2 PrivateKeyInfo blob
      Key := PBKDF2FromPassword(Password, Salt, CipherFactory.GetKeySize(Algo), Iterations);
      Result := CipherFactory.CreateCipher(Algo, Key, IV);
    end
    else
    if Copy(OId, 1, Length(OID_PKCS5)) = OID_PKCS5 then begin
      if OId = OID_pbeWithMD2AndDES_CBC then begin
        HashAlgorithm := haMD2;
        AlgoEx := sa_DES;
      end
      else
      if OId = OID_pbeWithMD2AndRC2_CBC then begin
        HashAlgorithm := haMD2;
        AlgoEx := sa_RC2;
      end
      else
      if OId = OID_pbeWithMD5AndDES_CBC then begin
        HashAlgorithm := haMD5;
        AlgoEx := sa_DES;
      end
      else
      if OId = OID_pbeWithMD5AndRC2_CBC then begin
        HashAlgorithm := haMD5;
        AlgoEx := sa_RC2;
      end
      else
      if OId = OID_pbeWithSHA1AndDES_CBC then begin
        HashAlgorithm := haSHA1;
        AlgoEx := sa_DES;
      end
      else
      if OId = OID_pbeWithSHA1AndRC2_CBC then begin
        HashAlgorithm := haSHA1;
        AlgoEx := sa_RC2;
      end
      else
        Exit;

      if not ASN1Compiler.Parse(asn1_PBES1_PARAMS_DESC, Params) then
        Exit;

      Salt := ASN1Compiler['Salt'].AsBytes;
      Iterations := ASN1Compiler['Iters'].AsInteger;

      Key := PBKDF1FromPassword(HashAlgorithm, Password, Salt, 16, Iterations, False);
      SetLength(IV, 8);
      Move(Key[8], IV[0], 8);
      SetLength(Key, 8);

      Result := CipherFactory.CreateCipherEx(AlgoEx, cmCBC);
      Result.Key := Key;
      Result.IV := IV;
    end
    else
    if Copy(OId, 1, Length(OID_PKCS12_PBE_IDS)) = OID_PKCS12_PBE_IDS then begin
      if OId = OID_pbeWithSHAAnd128BitRC4 then begin
        AlgoEx := sa_RC4;
        CipherMode := cmECB;
        KeySize := 16;
        IVSize := 0;
      end
      else
      if OId = OID_pbeWithSHAAnd40BitRC4 then begin
        AlgoEx := sa_RC4;
        CipherMode := cmECB;
        KeySize := 5;
        IVSize := 0;
      end
      else
      if OId = OID_pbeWithSHAAnd3KeyTripleDES_CBC then begin
        AlgoEx := sa_TripleDES;
        CipherMode := cmCBC;
        KeySize := 24;
        IVSize := 8;
      end
      else
      if OId = OID_pbeWithSHAAnd2KeyTripleDES_CBC then begin
        AlgoEx := sa_TripleDES;
        CipherMode := cmCBC;
        KeySize := 16;
        IVSize := 8;
      end
      else
      if OId = OID_pbeWithSHAAnd128BitRC2_CBC then begin
        AlgoEx := sa_RC2;
        CipherMode := cmCBC;
        KeySize := 16;
        IVSize := 8;
      end
      else
      if OId = OID_pbeWithSHAAnd40BitRC2_CBC then begin
        AlgoEx := sa_RC2;
        CipherMode := cmCBC;
        KeySize := 5;
        IVSize := 8;
      end
      else
        Exit;

      if not ASN1Compiler.Parse(asn1_PKCS12_PBE_PARAMS_DESC, Params) then
        Exit;

      Salt := ASN1Compiler['Salt'].AsBytes;
      Iterations := ASN1Compiler['Iters'].AsInteger;

      Key := PKCS12KeyDeriveFromPassword(haSHA1, Password, Salt, kpEncKey, Iterations, KeySize);
      if IVSize > 0 then
        IV := PKCS12KeyDeriveFromPassword(haSHA1, Password, Salt, kpIV, Iterations, IVSize);

      Result := CipherFactory.CreateCipherEx(AlgoEx, CipherMode);
      Result.Key := Key;
      Result.IV := IV;
    end;

    if Length(Key) > 0 then
      FillChar(Key[0], Length(Key), 0); // to protect
  finally
    ASN1Compiler.Free;
  end;
end;

{ TScKey }

procedure WriteBlockInPEMStyle(Stream: TStream; const Data: TBytes; LineLen: integer; EscapeNeeded: Boolean);

  procedure WriteBuffer(const Data: TBytes; Offset, Count: integer);
  begin
    if (Count <> 0) and (Stream.Write(Data[Offset], Count) <> Count) then
      raise EWriteError.Create('Stream write error');
  end;

const
  LF: byte = 10;
  BS: byte = 92; // '\'
var
  Cursor: integer;
begin
  Cursor := 0;
  while Cursor < Length(Data) do begin
    if LineLen >= (Length(Data) - Cursor) then begin
      WriteBuffer(Data, Cursor, Length(Data) - Cursor);
    end
    else
    if EscapeNeeded then begin
      WriteBuffer(Data, Cursor, LineLen - 1);
      Stream.WriteBuffer(BS, 1);
      Dec(Cursor);
    end
    else begin
      WriteBuffer(Data, Cursor, LineLen);
    end;

    Stream.WriteBuffer(LF, 1);
    Cursor := Cursor + LineLen;
  end;
end;

constructor TScKey.Create(AStorageList: TScStorageList = nil);
begin
  inherited Create(AStorageList);

  FPSSParams := TScPSSParams.Create;
  FOAEPParams := TScOAEPParams.Create;
  FList := TCRList.Create;
  FAlgorithm := aaRSA;
end;

destructor TScKey.Destroy;
begin
  FPSSParams.Free;
  FOAEPParams.Free;

  inherited;
  FList.Free; // used in inherited
end;

procedure TScKey.RaiseError;
begin
  raise EScError.Create(seBrokenKey);
end;

procedure TScKey.CheckReady;
begin
  if not FReady then
    RaiseError;
end;

procedure TScKey.RegisterClient(Client: TObject);
begin
  if IsClass(Client, TScUser) or IsClass(Client, TScCertificate) then
    FList.Add(Client);
end;

procedure TScKey.UnRegisterClient(Client: TObject);
begin
  FList.Remove(Client);
end;

procedure TScKey.SaveToStorageIfPossible;
var
  Obj: TObject;
  i: integer;
begin
  inherited;

  for i := 0 to FList.Count - 1 do begin
    Obj := TObject(FList[i]);
    if IsClass(Obj, TScUser) then
      TScUser(Obj).SaveToStorageIfPossible
    else
    if IsClass(Obj, TScCertificate) then
      TScCertificate(Obj).KeyChanged;
  end;
end;

procedure TScKey.FreeData; // to protect
begin
  lock(FLock);
  try
    FreeAndNil(FDSAData.X);
    FreeAndNil(FDSAData.G);
    FreeAndNil(FDSAData.P);
    FreeAndNil(FDSAData.Q);
    FreeAndNil(FDSAData.Y);

    FreeAndNil(FRSAData.D);
    FreeAndNil(FRSAData.P);
    FreeAndNil(FRSAData.Q);
    FreeAndNil(FRSAData.Qinv);
    FreeAndNil(FRSAData.DP);
    FreeAndNil(FRSAData.DQ);
    FreeAndNil(FRSAData.PublicExponent);
    FreeAndNil(FRSAData.PublicModulus);

    FreeAndNil(FECData.ECCryptography);
    FreeAndNil(FECData.PublicPoint);
    FreeAndNil(FECData.PrivateKoef);
  finally
    unlock(FLock);
  end;
end;

procedure TScKey.InternalAssign(Source: TScStorageItem);

  procedure SetDSAData(const Source: TScDSAData);
  begin
    if Source.G <> nil then
      FDSAData.G := TBigInteger.Create(Source.G);

    if Source.P <> nil then
      FDSAData.P := TBigInteger.Create(Source.P);

    if Source.Q <> nil then
      FDSAData.Q := TBigInteger.Create(Source.Q);

    if Source.Y <> nil then
      FDSAData.Y := TBigInteger.Create(Source.Y);

    if Source.X <> nil then
      FDSAData.X := TBigInteger.Create(Source.X);
  end;

  procedure SetRSAData(const Source: TScRSAData);
  begin
    if Source.PublicExponent <> nil then
      FRSAData.PublicExponent := TBigInteger.Create(Source.PublicExponent);

    if Source.PublicModulus <> nil then
      FRSAData.PublicModulus := TBigInteger.Create(Source.PublicModulus);

    if Source.D <> nil then
      FRSAData.D := TBigInteger.Create(Source.D);

    if Source.P <> nil then
      FRSAData.P := TBigInteger.Create(Source.P);

    if Source.Q <> nil then
      FRSAData.Q := TBigInteger.Create(Source.Q);

    if Source.Qinv <> nil then
      FRSAData.Qinv := TBigInteger.Create(Source.Qinv);

    if Source.DP <> nil then
      FRSAData.DP := TBigInteger.Create(Source.DP);

    if Source.DQ <> nil then
      FRSAData.DQ := TBigInteger.Create(Source.DQ);
  end;

  procedure SetECData(const Source: TScECData);
  begin
    FECData.ECName := Source.ECName;
    if Source.ECCryptography <> nil then begin
      FECData.ECCryptography := TScECCryptographyClass(Source.ECCryptography.ClassType).Create;
      FECData.ECCryptography.Assign(Source.ECCryptography);

      if Source.PublicPoint <> nil then begin
        FECData.PublicPoint := FECData.ECCryptography.PublicPointClass.Create;
        FECData.PublicPoint.Assign(Source.PublicPoint);
      end;

      if Source.PrivateKoef <> nil then begin
        FECData.PrivateKoef := FECData.ECCryptography.PrivateKoefClass.Create;
        FECData.PrivateKoef.Assign(Source.PrivateKoef);
      end;
    end;
  end;

begin
  if not IsClass(Source, TScKey) then
    raise EScError.CreateFmt(SInvalidObjectClass, [TScKey.ClassName, Source.ClassName], seInvalidObjectClass);

  FreeData;

  // FKeyStorage := Source.FKeyStorage;
  // FKeyName := Source.FKeyName; // Commented to allow changing KeyName
  FAlgorithm := TScKey(Source).FAlgorithm;

  case FAlgorithm of
    aaRSA:
      SetRSAData(TScKey(Source).FRSAData);
    aaDSA:
      SetDSAData(TScKey(Source).FDSAData);
    aaEC:
      SetECData(TScKey(Source).FECData);
  else
    Assert(False);
  end;

  FReady := Source.FReady;
end;

procedure TScKey.GenerateRSA(const BitCount: integer; ARandom: IScRandom);
var
  one, tmp: TBigInteger;
  e, p, q, p_1, q_1, phi: TBigInteger;
begin
  if ARandom = nil then
    ARandom := Random;
  if ARandom = nil then
    raise Exception.Create(SInternalError);

  p := nil;
  q := nil;
  tmp := nil;
  p_1 := nil;
  q_1 := nil;
  phi := nil;
  one := TBigInteger.Create(1);
  e := TBigInteger.Create(65537);

  try
    while True do begin
      while True do begin
        FreeAndNil(p);
        FreeAndNil(q);
        p := TBigInteger.GenPseudoPrime(BitCount shr 1, 64, ARandom);
        q := TBigInteger.GenPseudoPrime(BitCount shr 1, 64, ARandom);

        FreeAndNil(tmp);
        tmp := p.gcd(q);
        if tmp.Equal(one) then
          Break;
      end;

      FreeAndNil(p_1);
      FreeAndNil(q_1);
      FreeAndNil(phi);
      p_1 := p.Minus(one);
      q_1 := q.Minus(one);
      phi := p_1.Mul(q_1);

      FreeAndNil(tmp);
      tmp := e.gcd(phi);
      if tmp.Equal(one) then
        break;
    end;

    FRSAData.PublicExponent := TBigInteger.Create(e);
    FRSAData.PublicModulus := p.Mul(q);
    FRSAData.D := e.ModInverse(phi); // private_exponent
    FRSAData.Qinv := q.ModInverse(p);
    FRSAData.DP := FRSAData.D.Mod_(p_1);
    FRSAData.DQ := FRSAData.D.Mod_(q_1);

    FRSAData.P := p;
    p := nil;
    FRSAData.Q := q;
    q := nil;

    FAlgorithm := aaRSA;
    FReady := True;
  finally
    one.Free;
    p.Free;
    q.Free;
    tmp.Free;
    p_1.Free;
    q_1.Free;
    phi.Free;
    e.Free;
  end;
end;

procedure TScKey.Calc_DP_DQ;
var
  one: TBigInteger;
  p_1, q_1: TBigInteger;
  TmpQinv, tmp: TBigInteger;
begin
  TmpQinv := FRSAData.Q.ModInverse(FRSAData.P);
  try
    if not TmpQinv.Equal(FRSAData.Qinv) then begin
      TmpQinv.Free;
      TmpQinv := FRSAData.P.ModInverse(FRSAData.Q);
      if not TmpQinv.Equal(FRSAData.Qinv) then
        RaiseError;
      tmp := FRSAData.Q;
      FRSAData.Q := FRSAData.P;
      FRSAData.P := tmp;
    end;
  finally
    TmpQinv.Free;
  end;

  p_1 := nil;
  q_1 := nil;
  one := TBigInteger.Create(1);
  try
    p_1 := FRSAData.P.Minus(one);
    q_1 := FRSAData.Q.Minus(one);

    FRSAData.DP := FRSAData.D.Mod_(p_1);
    FRSAData.DQ := FRSAData.D.Mod_(q_1);
  finally
    one.Free;
    p_1.Free;
    q_1.Free;
  end;
end;

class procedure TScKey.FindRandomStrongPrime(PrimeBits, OrderBits: integer;
  ARandom: IScRandom; var Prime, Order: TBigInteger);
var
  one, u, aux, aux2, t, temp: TBigInteger;
  Sieve: TPrimeSieve;
  Table_q, Table_u, Prime_Table: array of Int64;
  cur_p, Value: Int64;
  Table_Count: cardinal;
  Flag: boolean;
  i, j, pN: integer;
begin
  Sieve := TPrimeSieve.Create(16000);
  try
    Table_Count := Sieve.AvailablePrimes - 1;
    SetLength(Prime_Table, Table_Count);
    SetLength(Table_q, Table_Count);
    SetLength(Table_u, Table_Count);

    i := 0;
    pN := 2;
    while pN <> 0 do begin
      Prime_Table[i] := pN;
      pN := Sieve.GetNextPrime(pN);
      Inc(i);
    end;
  finally
    Sieve.Free;
  end;

  u := nil;
  aux := nil;
  aux2 := nil;
  one := TBigInteger.Create(1);
  try
    Order := TBigInteger.GenPseudoPrime(OrderBits, 50, ARandom);
    for i := 0 to Table_Count - 1 do begin
      t := Order.Mod_(Prime_Table[i]);
      try
        Table_q[i] := ({$IFDEF VER25P}Int64{$ENDIF}(t.LongValue) * 2) mod Prime_Table[i];
      finally
        t.Free;
      end;
    end;

    while True do begin
      u := TBigInteger.Create;
      u.GenRandomBits(PrimeBits, ARandom);

      u.SetBit(PrimeBits - 1);
      aux := Order.Shl_(1);
      aux2 := u.Mod_(aux);
      if not u.Equal(aux2) then begin
        temp := u;
        u := u.Minus(aux2);
        temp.Free;
      end;
      temp := u;
      u := u.Add(one);
      temp.Free;
      FreeAndNil(aux);
      FreeAndNil(aux2);

      if u.BitCount <= (PrimeBits - 1) then
        Continue;

      for j := 0  to Table_Count - 1 do begin
        t := u.Mod_(Prime_Table[j]);
        try
          Table_u[j] := t.LongValue;
        finally
          t.Free;
        end;
      end;

      aux2 := Order.Shl_(1);
      i := 0;
      while i < (1 shl 24) do begin
        Flag := True;

        for j := 1 to Table_Count - 1 do begin
          cur_p := Prime_Table[j];
          Value := Table_u[j];
          if Value >= cur_p then
            Value := Value - cur_p;
          if Value = 0 then
            Flag := False;

          Table_u[j] := Value + Table_q[j];
        end;

        if not Flag then begin
          Inc(i);
          Continue;
        end;

        aux := aux2.Mul(i);
        FreeAndNil(Prime);
        Prime := u.Add(aux);
        FreeAndNil(aux);

        if Prime.BitCount > PrimeBits then begin
          Inc(i);
          Continue;
        end;

        if Prime.IsProbablePrime(50) then
          break;

        Inc(i);
      end;
      FreeAndNil(aux2);
      FreeAndNil(u);

      if i < (1 shl 24) then
        break;
    end;
  finally
    one.Free;
    u.Free;
    aux.Free;
    aux2.Free;
  end;
end;

class function TScKey.FindRandomGenerator(Order, Modulo: TBigInteger; // q, p
  ARandom: IScRandom): TBigInteger;
var
  one, aux, t, temp, temp2: TBigInteger;
  Generator: TBigInteger;
begin
  aux := nil;
  t := nil;
  Generator := nil;
  one := TBigInteger.Create(1);
  try
    aux := Modulo.Minus(one);
    t := aux.Mod_(Order);

    if t.LongValue <> 0 then begin
      Result := TBigInteger.Create(0);
      Exit;
    end;
    FreeAndNil(t);

    t := aux.Div_(Order);
    while True do begin
      temp2 := nil;
      temp := TBigInteger.Create;
      try
        temp.GenRandomBits(Modulo.BitCount, ARandom);
        temp2 := temp.Mod_(Modulo);
        FreeAndNil(Generator);
        Generator := temp2.ModPow(t, Modulo);
      finally
        temp.Free;
        temp2.Free;
      end;

      if Generator.Greater(one) then
        break;
    end;

    FreeAndNil(aux);
    aux := Generator.ModPow(Order, Modulo);
    if aux.NotEqual(one) then begin
      Result := TBigInteger.Create(0);
      Exit;
    end;

    Result := Generator;
    Generator := nil;
  finally
    one.Free;
    aux.Free;
    t.Free;
    Generator.Free;
  end;
end;

procedure TScKey.GenerateDSA(const BitCount: integer; ARandom: IScRandom);
var
  one, p, q, q_1: TBigInteger;
begin
  if ARandom = nil then
    ARandom := Random;
  if ARandom = nil then
    raise Exception.Create(SInternalError);

  p := nil;
  q := nil;
  q_1 := nil;
  one := TBigInteger.Create(1);
  try
    FindRandomStrongPrime(BitCount, 160, ARandom, p, q);
    FDSAData.G := FindRandomGenerator(q, p, ARandom);

    q_1 := q.Minus(one);
    FDSAData.X := TBigInteger.Create;
    repeat
      FDSAData.X.GenRandomBits(q.BitCount, ARandom);
    until FDSAData.X.Greater(one) and FDSAData.X.Less(q_1);

    FDSAData.Y := FDSAData.G.ModPow(FDSAData.X, p);

    FDSAData.P := p;
    p := nil;
    FDSAData.Q := q;
    q := nil;

    FAlgorithm := aaDSA;
    FReady := True;
  finally
    one.Free;
    p.Free;
    q.Free;
    q_1.Free;
  end;
end;

procedure TScKey.CalcY;
begin
  FDSAData.Y := FDSAData.G.ModPow(FDSAData.X, FDSAData.P);
end;

procedure TScKey.Generate(const Algorithm: TScAsymmetricAlgorithm; const BitCount: integer; ARandom: IScRandom = nil);
begin
  if not (Algorithm in [aaRSA, aaDSA]) then
    raise EScError.Create(seUseGenerateECMethod);

  if (BitCount <= 0) or ((BitCount mod 128) <> 0) then
    raise EScError.Create(seBitCountMultiple);

  lock(FLock);
  try
    Ready := False;
    case Algorithm of
      aaRSA:
        GenerateRSA(BitCount, ARandom);
      aaDSA:
        GenerateDSA(BitCount, ARandom);
    else
      Assert(False);
    end;
  finally
    unlock(FLock);
  end;

  SaveToStorageIfPossible;
end;

procedure TScKey.GenerateEC(const ECName: TScECName; ARandom: IScRandom = nil);
begin
  lock(FLock);
  try
    Ready := False;

    Assert(FECData.ECCryptography = nil);
    FECData.ECName := ECName;
    FECData.ECCryptography := EllipticCurvesParameters[ECName].CryptographyClass.Create(EllipticCurvesParameters[ECName]);
    try
      FECData.ECCryptography.GeneratePrivateKey(FECData, ARandom);
    except
      FreeData;
      raise;
    end;

    FAlgorithm := aaEC;
    FReady := True;
  finally
    unlock(FLock);
  end;

  SaveToStorageIfPossible;
end;

class function TScKey.CompareDSAData(const d1, d2: TScDSAData; PublicKeyOnly: boolean): boolean;
begin
  Result := d1.G.Equal(d2.G) and d1.P.Equal(d2.P) and d1.Q.Equal(d2.Q) and d1.Y.Equal(d2.Y);

  if not PublicKeyOnly and Result then
    Result := d1.X.Equal(d2.X);
end;

class function TScKey.CompareRSAData(const d1, d2: TScRSAData; PublicKeyOnly: boolean): boolean;
begin
  Result := d1.PublicExponent.Equal(d2.PublicExponent) and d1.PublicModulus.Equal(d2.PublicModulus);

  if not PublicKeyOnly and Result then
    Result := d1.D.Equal(d2.D) and d1.P.Equal(d2.P) and d1.Q.Equal(d2.Q) and d1.Qinv.Equal(d2.Qinv);
end;

class function TScKey.CompareECData(const d1, d2: TScECData; PublicKeyOnly: boolean): boolean;
begin
  Result := (d1.ECCryptography <> nil) and (d2.ECCryptography <> nil) and
    (d1.PublicPoint <> nil) and (d2.PublicPoint <> nil) and
    d1.ECCryptography.Equals(d2.ECCryptography) and
    d1.PublicPoint.Equals(d2.PublicPoint);

  if not PublicKeyOnly and Result then begin
    Result := (d1.PrivateKoef <> nil) and (d2.PrivateKoef <> nil) and (d1.PrivateKoef.ClassType = d2.PrivateKoef.ClassType);
    if Result then
      if IsClass(d1.PrivateKoef, TBigInteger) then
        Result := TBigInteger(d1.PrivateKoef).Equal(TBigInteger(d2.PrivateKoef))
      else
      if IsClass(d1.PrivateKoef, T32BytesField) then
        Result := T32BytesField(d1.PrivateKoef).Equal(T32BytesField(d2.PrivateKoef))
      else
        Result := False;
  end;
end;

function TScKey.Equals(Item: TScStorageItem): boolean;
var
  Key: TScKey;
begin
  Result := IsClass(Item, TScKey);
  if not Result then
    Exit;

  Key := TScKey(Item);
  CheckAvailable;
  Key.CheckAvailable;
  Result := Algorithm = Key.Algorithm;

  if Result then
    case Algorithm of
      aaRSA:
        Result := CompareRSAData(FRSAData, Key.FRSAData, not (IsPrivate and Key.IsPrivate));
      aaDSA:
        Result := CompareDSAData(FDSAData, Key.FDSAData, not (IsPrivate and Key.IsPrivate));
      aaEC:
        Result := CompareECData(FECData, Key.FECData, not (IsPrivate and Key.IsPrivate));
    else
      Assert(False);
    end;
end;

procedure TScKey.GetFingerprint(const HashAlg: TScHashAlgorithm; out Fingerprint: TBytes);
var
  csp: THashAlgorithm;
begin
  CheckAvailable;

  csp := CipherFactory.CreateHash(HashAlg);
  try
    csp.ComputeHash(EncodePublicKeyToOpenSSHFormat);
    SetLength(Fingerprint, Length(csp.Hash));
    Buffer.BlockCopy(csp.Hash, 0, Fingerprint, 0, Length(csp.Hash));
  finally
    csp.Free;
  end;
end;

procedure TScKey.GetFingerprint(const HashAlg: TScHashAlgorithm; out Fingerprint: string);
var
  FPData: TBytes;
begin
  GetFingerprint(HashAlg, FPData);
  Fingerprint := BytesToHexStr(FPData, ':');
end;

procedure TScKey.ParsePuTTYKey(KeyStrings: TStringList; const Password: string; out Comment: string);
var
  i, j, k: Integer;
  s: string;
  AName, AValue: string;
  PuTTYEncryption{, PuTTYPrivateMAC}: string;
  PuTTYPublicKey, PuTTYPrivateKey: string;
  Data, TmpBuf, PassBuf, PassHash, IV: TBytes;
  csp: THash_SHA1;
  Cipher: TSymmetricAlgorithm;
  Reader: TSSH2DataReader;
begin
  Comment := '';
  PuTTYEncryption := '';
  PuTTYPublicKey := '';
  PuTTYPrivateKey := '';
//  PuTTYPrivateMAC := '';
  SetLength(Data, 0); // due warning
  SetLength(PassBuf, 0); // due warning

  i := 0;
  while i < KeyStrings.Count do begin
    s := Trim(KeyStrings.Strings[i]);
    Inc(i);

    j := Pos(':', s);
    if j <= 0 then
      Continue;

    AName := Trim(Copy(s, 1, j - 1));
    AValue := Trim(Copy(s, j + 1, Length(s) - j));

    if SameText(AName, 'PuTTY-User-Key-File-2') then begin
      if SameText(AValue, DSA_TYPE_HEADER) then
        FAlgorithm := aaDSA
      else
      if SameText(AValue, RSA_TYPE_HEADER) then
        FAlgorithm := aaRSA
      else
      if SameText(AValue, ED25519_TYPE_HEADER) or
        SameText(LeftStr(AValue, Length(ECDSA_TYPE_HEADER)), ECDSA_TYPE_HEADER) then
        FAlgorithm := aaEC
      else
        raise EScError.CreateFmt(SUnknownAlgorithmS, [AValue], seUnknownAlgorithm);
    end
    else
    if SameText(AName, 'Encryption') then
      PuTTYEncryption := AValue
    else
    if SameText(AName, 'Comment') then
      Comment := AValue
    else
    if SameText(AName, 'Public-Lines') then begin
      k := StrToIntDef(AValue, -1);
      if k <= 0 then
        raise EScError.Create(seBrokenKey);

      PuTTYPublicKey := '';
      while (k > 0) and (i < KeyStrings.Count) do begin
        PuTTYPublicKey := PuTTYPublicKey + KeyStrings.Strings[i];
        Inc(i);
        Dec(k);
      end;

      if k <> 0 then
        raise EScError.Create(seBrokenKey);
    end
    else
    if SameText(AName, 'Private-Lines') then begin
      k := StrToIntDef(AValue, -1);
      if k <= 0 then
        raise EScError.Create(seBrokenKey);

      PuTTYPrivateKey := '';
      while (k > 0) and (i < KeyStrings.Count) do begin
        PuTTYPrivateKey := PuTTYPrivateKey + KeyStrings.Strings[i];
        Inc(i);
        Dec(k);
      end;

      if k <> 0 then
        raise EScError.Create(seBrokenKey);
    end;
//    else
//    if SameText( AName, 'Private-MAC') then
//      PuTTYPrivateMAC := AValue;
  end;

  if PuTTYPublicKey <> '' then begin
    Data := TBase64.Decode(Encoding.Default.GetBytes(PuTTYPublicKey));
    DecodePublicKeyFromIETFFormat(Data);
  end;

  CheckReady;

  if PuTTYPrivateKey <> '' then begin
    Data := TBase64.Decode(Encoding.Default.GetBytes(PuTTYPrivateKey));

    if not SameText(PuTTYEncryption, 'none') then begin
      if Password = '' then
        raise EScError.Create(sePasswordNotSpecified);

      csp := THash_SHA1.Create;
      try
        PassBuf := Encoding.Default.GetBytes(Password);
        SetLength(TmpBuf, 4 + Length(PassBuf));
        SetLength(PassHash, 2 * csp.HashSize);  // 2 * 20

        TmpBuf[0] := 0;
        TmpBuf[1] := 0;
        TmpBuf[2] := 0;
        TmpBuf[3] := 0;
        Move(PassBuf[0], TmpBuf[4], Length(PassBuf));

        csp.ComputeHash(TmpBuf);
        Move(csp.Hash[0], PassHash[0], csp.HashSize);

        TmpBuf[3] := 1;
        csp.ComputeHash(TmpBuf);
        Move(csp.Hash[0], PassHash[csp.HashSize], csp.HashSize);
      finally
        csp.Free;
      end;

      SetLength(PassHash, 32);  // 32 * 8 = 256b
      SetLength(IV, 16);
      FillChar(IV[0], Length(IV), 0);

      Cipher := CipherFactory.CreateCipher(CipherFactory.SSH2NameToCipherAlgorithm(PuTTYEncryption), PassHash, IV);
      Cipher.CreateDecryptor.TransformBlock(@Data[0], 0, Length(Data), @Data[0], 0);
    end;

    Reader := TSSH2DataReader.Create(Data);
    try
      case FAlgorithm of
        aaRSA: begin
          FRSAData.D := Reader.ReadAsBigInteger;
          FRSAData.P := Reader.ReadAsBigInteger;
          FRSAData.Q := Reader.ReadAsBigInteger;
          FRSAData.Qinv := Reader.ReadAsBigInteger;
          Calc_DP_DQ;
        end;
        aaDSA: begin
          FDSAData.X := Reader.ReadAsBigInteger;
        end;
        aaEC: begin
          if IsClass(FECData.ECCryptography, TScECCryptographyX25519) then begin
            TmpBuf := Reader.ReadString;
            FECData.PrivateKoef := T32BytesField.Create(TmpBuf);
          end
          else
            FECData.PrivateKoef := Reader.ReadAsBigInteger;
        end;
      else
        raise EScError.CreateFmt(SUnknownAlgorithmS, ['EC'], seUnknownAlgorithm);
      end;
    finally
      Reader.Free;
    end;
  end;

  FReady := True;
end;

function TScKey.ParsePEMKey(KeyStrings: TStringList; const Footer: string; out Comment: string): TBytes;
var
  l: string;
  InfoStr: string;
  sb: StringBuilder;
  i: integer;
begin
  Comment := '';

  sb := StringBuilder.Create;
  try
    i := 1;
    while i < KeyStrings.Count do begin
      l := Trim(KeyStrings[i]);

      if SameText(LeftStr(l, Length(Footer)), Footer) then
        Break;

      if Pos(':', l) = 0 then
        sb.Append(l)
      else begin
        InfoStr := l;
        while (i < KeyStrings.Count - 1) and (l[Length(l)] = '\') do begin
          Delete(l, Length(l), 1);
          InfoStr := InfoStr + l;
          Inc(i);
          l := KeyStrings[i];
        end;

        if SameText(LeftStr(InfoStr, Length(COMMENT_LINE)), COMMENT_LINE) then
          Comment := Trim(Copy(l, Length(COMMENT_LINE) + 1, Length(l)));
      end;

      Inc(i);
    end;

    if i >= KeyStrings.Count then
      RaiseError;

    Result := TBase64.Decode(Encoding.Default.GetBytes(sb.ToString));
  finally
    sb.Free;
  end;
end;

function TScKey.ParsePrivatePEMKey(KeyStrings: TStringList; const Password: string; const Footer: string; out Comment: string): TBytes;
var
  l, AlgStr, SaltStr: string;
  Key, Salt: TBytes;
  Cipher: TSymmetricAlgorithm;
  Algo: TScSymmetricAlgorithm;
  i, CommaPos: integer;
begin
  // read PEM encryption info lines and extract Salt
  if KeyStrings.Count > 3 then
    l := KeyStrings[1]
  else
    l := '';

  if LeftStr(l, Length(PEM_PROC_TYPE_LINE)) = PEM_PROC_TYPE_LINE then begin
    l := KeyStrings[2];

    SetLength(Key, 0);
    SetLength(Salt, 0);
    if LeftStr(l, Length(PEM_INFO_LINE)) = PEM_INFO_LINE then begin
      CommaPos := Pos(',', l);
      if CommaPos = 0 then
        RaiseError;

      AlgStr := Copy(l, Length(PEM_INFO_LINE) + 1, CommaPos - Length(PEM_INFO_LINE) - 1);
      SaltStr := Trim(Copy(l, CommaPos + 1, Length(l)));
      SetLength(Salt, Length(SaltStr) div 2);

      for i := 0 to Length(Salt) - 1 do
        Salt[i] := StrToInt('$' + SaltStr[i * 2 + 1] + SaltStr[i * 2 + 2]);
    end;

    if AlgStr = PEM_3DESCBC_ALG then
      Algo := saTripleDES_cbc
    else
    if AlgStr = PEM_AES128CBC_ALG then
      Algo := saAES128_cbc
    else
    if AlgStr = PEM_AES192CBC_ALG then
      Algo := saAES192_cbc
    else
    if AlgStr = PEM_AES256CBC_ALG then
      Algo := saAES256_cbc
    else
    if AlgStr = PEM_AES128CTR_ALG then
      Algo := saAES128_ctr
    else
    if AlgStr = PEM_AES192CTR_ALG then
      Algo := saAES192_ctr
    else
    if AlgStr = PEM_AES256CTR_ALG then
      Algo := saAES256_ctr
    else
      raise EScError.Create(seCipherNotSupported);

    Key := TScKeyDerivationProcessor.PBKDF1FromPassword(haMD5, Password, Salt, CipherFactory.GetKeySize(Algo), 1, False);
    Cipher := CipherFactory.CreateCipher(Algo, Key, Salt);
    FillChar(Key[0], Length(Key), 0); // to protect

    Result := ParsePEMKey(KeyStrings, Footer, Comment);
    Cipher.CreateDecryptor.TransformBlock(Result, 0, Length(Result));
  end
  else
    Result := ParsePEMKey(KeyStrings, Footer, Comment);
end;

procedure TScKey.CryptPrivatePEMKey(Stream: TStream; const Password: string;
  Algorithm: TScSymmetricAlgorithm; var KeyData: TBytes);
var
  Key: TBytes;
  Cipher: TSymmetricAlgorithm;
  Salt: TBytes;
  LenKeyData, PaddingLen, BlockSize: integer;
  AlgStr, PEMInfo: string;
begin
  case Algorithm of
    saTripleDES_cbc:
      AlgStr := PEM_3DESCBC_ALG;
    saAES128_cbc:
      AlgStr := PEM_AES128CBC_ALG;
    saAES192_cbc:
      AlgStr := PEM_AES192CBC_ALG;
    saAES256_cbc:
      AlgStr := PEM_AES256CBC_ALG;
    saAES128_ctr:
      AlgStr := PEM_AES128CTR_ALG;
    saAES192_ctr:
      AlgStr := PEM_AES192CTR_ALG;
    saAES256_ctr:
      AlgStr := PEM_AES256CTR_ALG;
  else
    raise EScError.Create(seCipherNotSupported);
  end;

  PEMInfo := PEM_INFO_LINE + AlgStr + ',';

  TStreamUtils.WriteLine(Stream, PEM_PROC_TYPE_LINE);
  Stream.WriteBuffer(Encoding.Default.GetBytes(PEMInfo)[0], Length(PEMInfo));

  LenKeyData := Length(KeyData);
  BlockSize := CipherFactory.GetBlockSize(Algorithm);

  PaddingLen := LenKeyData mod BlockSize;
  if PaddingLen > 0 then begin
    PaddingLen := BlockSize - PaddingLen;

    if PaddingLen > 0 then begin
      SetLength(KeyData, LenKeyData + PaddingLen);
      System.FillChar(KeyData[LenKeyData], PaddingLen, PaddingLen);
    end;
  end;

  SetLength(Salt, BlockSize);
  if Random = nil then
    raise Exception.Create(SInternalError);
  Random.Random(Salt, 0, Length(Salt));
  TStreamUtils.WriteLine(Stream, BytesToHexStr(Salt));
  TStreamUtils.WriteLine(Stream, '');

  Key := TScKeyDerivationProcessor.PBKDF1FromPassword(haMD5, Password, Salt, CipherFactory.GetKeySize(Algorithm), 1, False);
  Cipher := CipherFactory.CreateCipher(Algorithm, Key, Salt);
  FillChar(Key[0], Length(Key), 0); // to protect
  Cipher.CreateEncryptor.TransformBlock(KeyData, 0, Length(KeyData));
end;

function TScKey.ParseOpenSSHPublicKey(const Key: string; out Comment: string): TBytes;
var
  Len: integer;
  Pos1, Pos2: integer;
begin
  Len := Length(Key);
  Pos1 := 1;
  while (Pos1 <= Len) and (Key[Pos1] <> ' ') do
    Inc(Pos1);

  Pos2 := Pos1 + 1;
  while (Pos2 <= Len) and (Key[Pos2] <> ' ') do
    Inc(Pos2);

  if Pos2 > Len then
    Comment := ''
  else
    Comment := Copy(Key, Pos2 + 1, Length(Key));

  Result := TBase64.Decode(Encoding.Default.GetBytes(Copy(Key, Pos1 + 1, Pos2 - Pos1 - 1)));
end;

procedure TScKey.ImportFrom(Stream: TStream; const Password: string; out Comment: string);
var
  FirstLine: string;
  StreamBeginPos: Int64;
  Str: string;
  KeyStrings: TStringList;
  KeyData, ECParamsData: TBytes;
  BeginPos: integer;
  Alg: TScAsymmetricAlgorithm;
  PublicKeyOnly: boolean;
begin
  lock(FLock);
  try
    Ready := False;
    Comment := '';
    if Stream.Position = Stream.Size then
      raise EScError.CreateFmt(SBrokenKey + ': Stream.Position = Stream.Size', [], seBrokenKey);

    StreamBeginPos := Stream.Position;

    SetLength(KeyData, Min(Stream.Size - Stream.Position, 1024 * 1024));
    Stream.Read(KeyData[0], Length(KeyData));
    Str := Encoding.Default.GetString(KeyData);

    BeginPos := Pos('-----BEGIN ', Str);
    if BeginPos = 0 then
      BeginPos := Pos('---- BEGIN ', Str);
    if BeginPos > 1 then
      Delete(Str, 1, BeginPos - 1);

    KeyStrings := TStringList.Create;
    try
      try
        KeyStrings.Text := Str;
        if KeyStrings.Count > 0 then
          FirstLine := KeyStrings[0]
        else
          FirstLine := '';

        if SameText(LeftStr(FirstLine, Length(PUTTY_KEY_HEADER_2)), PUTTY_KEY_HEADER_2) then begin
          ParsePuTTYKey(KeyStrings, Password, Comment);
        end
        else
        if SameText(LeftStr(FirstLine, Length(RSA_TYPE_HEADER)), RSA_TYPE_HEADER) or
          SameText(LeftStr(FirstLine, Length(DSA_TYPE_HEADER)), DSA_TYPE_HEADER) or
          SameText(LeftStr(FirstLine, Length(ECDSA_TYPE_HEADER)), ECDSA_TYPE_HEADER) or
          SameText(LeftStr(FirstLine, Length(ED25519_TYPE_HEADER)), ED25519_TYPE_HEADER) or
          SameText(LeftStr(FirstLine, Length(SCurve25519_Sha256)), SCurve25519_Sha256) then begin
          KeyData := ParseOpenSSHPublicKey(FirstLine, Comment);
          DecodePublicKeyFromOpenSSHFormat(KeyData);
        end
        else
        if FirstLine = PKCS1_RSA_PRIV_HEADER then begin
          KeyData := ParsePrivatePEMKey(KeyStrings, Password, PKCS1_RSA_PRIV_FOOTER, Comment);
          DecodeKeyFromPKCS1Format(KeyData, aaRSA, False);
        end
        else
        if FirstLine = PKCS1_RSA_PUB_HEADER then begin
          KeyData := ParsePEMKey(KeyStrings, PKCS1_RSA_PUB_FOOTER, Comment);
          DecodeKeyFromPKCS1Format(KeyData, aaRSA, True);
        end
        else
        if FirstLine = PKCS1_DSA_PRIV_HEADER then begin
          KeyData := ParsePrivatePEMKey(KeyStrings, Password, PKCS1_DSA_PRIV_FOOTER, Comment);
          DecodeKeyFromPKCS1Format(KeyData, aaDSA, False);
        end
        else
        if FirstLine = PKCS1_DSA_PUB_HEADER then begin
          KeyData := ParsePEMKey(KeyStrings, PKCS1_DSA_PUB_FOOTER, Comment);
          DecodeKeyFromPKCS1Format(KeyData, aaDSA, True);
        end
        else
        if FirstLine = EC_PRIV_HEADER then begin
          KeyData := ParsePrivatePEMKey(KeyStrings, Password, EC_PRIV_FOOTER, Comment);
          DecodeKeyFromPKCS1Format(KeyData, aaEC, False);
        end
        else
        if FirstLine = OPENSSH_PRIV_HEADER then begin
          KeyData := ParsePrivatePEMKey(KeyStrings, Password, OPENSSH_PRIV_FOOTER, Comment);
          DecodePrivateKeyFromOpenSSHFormat(KeyData, Password);
        end
        else
        if FirstLine = EC_PARAM_HEADER then begin
          SetLength(ECParamsData, 0);
          ECParamsData := ParsePEMKey(KeyStrings, EC_PARAM_FOOTER, Comment);
          while KeyStrings.Count > 0 do begin
            if SameText(KeyStrings[0], EC_PARAM_FOOTER) then begin
              KeyStrings.Delete(0);
              Break;
            end;
            KeyStrings.Delete(0);
          end;

          if (KeyStrings.Count = 0) or (KeyStrings[0] <> EC_PRIV_HEADER) then
            RaiseError;

          KeyData := ParsePrivatePEMKey(KeyStrings, Password, EC_PRIV_FOOTER, Comment);
          DecodeKeyFromECFormat(KeyData, ECParamsData);
        end
        else
        if FirstLine = PKCS1_PUB_HEADER then begin
          KeyData := ParsePEMKey(KeyStrings, PKCS1_PUB_FOOTER, Comment);
          DecodeKeyFromOpenSSLFormat(KeyData, True);

          if not FReady then
            DecodeKeyFromPKCS1Format(KeyData, aaRSA, True);
          if not FReady then
            DecodeKeyFromPKCS1Format(KeyData, aaDSA, True);
          if not FReady then
            DecodeKeyFromPKCS1Format(KeyData, aaEC, True);
        end
        else
        if FirstLine = PKCS8_HEADER then begin
          KeyData := ParsePEMKey(KeyStrings, PKCS8_FOOTER, Comment);
          DecodePrivateKeyFromPKCS8Format(KeyData);
        end
        else
        if FirstLine = PKCS8_ENC_HEADER then begin
          KeyData := ParsePEMKey(KeyStrings, PKCS8_ENC_FOOTER, Comment);
          DecodePrivateKeyFromPKCS8EncFormat(KeyData, Password);
        end
        else
        if FirstLine = IETF_PRIV_HEADER then begin
          KeyData := ParsePEMKey(KeyStrings, IETF_PRIV_FOOTER, Comment);
          DecodePrivateKeyFromIETFFormat(KeyData, Password);
        end
        else
        if FirstLine = IETF_PUB_HEADER then begin
          KeyData := ParsePEMKey(KeyStrings, IETF_PUB_FOOTER, Comment);
          DecodePublicKeyFromIETFFormat(KeyData);
        end
        else begin
          if DetectPKCS1Format(KeyData, Alg, PublicKeyOnly) then
            try
              DecodeKeyFromPKCS1Format(KeyData, Alg, PublicKeyOnly);
            except
            end;

          try
            if not FReady then
              DecodePrivateKeyFromPKCS8Format(KeyData);
          except
          end;

          if not FReady then begin // Try loading from own old format
            Stream.Position := StreamBeginPos;
            TScStorage.LoadObj(Stream, Self, saTripleDES_cbc, ''); // raise exception if error
          end;
        end;
      except
        FreeData;
        raise;
      end;
    finally
      KeyStrings.Free;
    end;
  finally
    unlock(FLock);
  end;

  CheckReady;
  SaveToStorageIfPossible;
end;

procedure TScKey.ImportFrom(Stream: TStream; const Password: string = '');
var
  Comment: string;
begin
  ImportFrom(Stream, Password, Comment);
end;

procedure TScKey.ImportFrom(const FileName: string; const Password: string; out Comment: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    ImportFrom(Stream, Password, Comment);
  finally
    Stream.Free;
  end;
end;

procedure TScKey.ImportFrom(const FileName: string; const Password: string = '');
var
  Comment: string;
begin
  ImportFrom(FileName, Password, Comment);
end;

function TScKey.DetectPKCS1Format(const KeyData: TBytes; out Alg: TScAsymmetricAlgorithm; out PublicKeyOnly: boolean): boolean;
var
  ASN1Compiler: TScASN1Compiler;
begin
  ASN1Compiler := TScASN1Compiler.Create;
  try
    Result := True;

    // don't change the order

    if ASN1Compiler.Parse(asn1_RSA_PRIVATE_KEY_PKCS1_DESC, KeyData) then begin
      Alg := aaRSA;
      PublicKeyOnly := False;
      Exit;
    end;

    if ASN1Compiler.Parse(asn1_DSA_PRIVATE_KEY_PKCS1_DESC, KeyData) then begin
      Alg := aaDSA;
      PublicKeyOnly := False;
      Exit;
    end;

    if ASN1Compiler.Parse(asn1_DSA_PUBLIC_KEY_PKCS1_DESC, KeyData) then begin
      Alg := aaDSA;
      PublicKeyOnly := True;
      Exit;
    end;

    if ASN1Compiler.Parse(asn1_RSA_PUBLIC_KEY_PKCS1_DESC, KeyData) then begin
      Alg := aaRSA;
      PublicKeyOnly := True;
      Exit;
    end;

    if ASN1Compiler.Parse(asn1_EC_PRIVATE_KEY_DESC, KeyData) then begin
      Alg := aaEC;
      PublicKeyOnly := False;
      Exit;
    end;

    if ASN1Compiler.Parse(asn1_EC_PUBLIC_KEY_DESC, KeyData) then begin
      Alg := aaEC;
      PublicKeyOnly := True;
      Exit;
    end;

    Result := False;
  finally
    ASN1Compiler.Free;
  end;
end;

procedure TScKey.DecodeKeyFromPKCS1Format(const KeyData: TBytes; Alg: TScAsymmetricAlgorithm; PublicKeyOnly: boolean);
var
  ASN1Compiler: TScASN1Compiler;
  AlgorithmOID: string;
  PublicKeyData: TBytes;
begin
  ASN1Compiler := TScASN1Compiler.Create;
  try
    FAlgorithm := Alg;

    case FAlgorithm of
      aaRSA: begin
        if PublicKeyOnly then begin
          if not ASN1Compiler.Parse(asn1_RSA_PUBLIC_KEY_PKCS1_DESC, KeyData) then
            Exit;

          FreeAndNil(FRSAData.D);
          FreeAndNil(FRSAData.P);
          FreeAndNil(FRSAData.Q);
          FreeAndNil(FRSAData.Qinv);
        end
        else begin
          if not ASN1Compiler.Parse(asn1_RSA_PRIVATE_KEY_PKCS1_DESC, KeyData) then
            Exit;

          FRSAData.D := TBigInteger.Create(ASN1Compiler['D'].AsBigInteger);
          FRSAData.P := TBigInteger.Create(ASN1Compiler['P'].AsBigInteger);
          FRSAData.Q := TBigInteger.Create(ASN1Compiler['Q'].AsBigInteger);
          FRSAData.Qinv := TBigInteger.Create(ASN1Compiler['U'].AsBigInteger);
          Calc_DP_DQ;
        end;

        FRSAData.PublicModulus := TBigInteger.Create(ASN1Compiler['PubMod'].AsBigInteger);
        FRSAData.PublicExponent := TBigInteger.Create(ASN1Compiler['PubExp'].AsBigInteger);
      end;

      aaDSA: begin
        if PublicKeyOnly then begin
          if not ASN1Compiler.Parse(asn1_DSA_PUBLIC_KEY_PKCS1_DESC, KeyData) then
            Exit;

          FreeAndNil(FDSAData.X);
        end
        else begin
          if not ASN1Compiler.Parse(asn1_DSA_PRIVATE_KEY_PKCS1_DESC, KeyData) then
            Exit;

          FDSAData.X := TBigInteger.Create(ASN1Compiler['X'].AsBigInteger);
        end;

        FDSAData.P := TBigInteger.Create(ASN1Compiler['P'].AsBigInteger);
        FDSAData.Q := TBigInteger.Create(ASN1Compiler['Q'].AsBigInteger);
        FDSAData.G := TBigInteger.Create(ASN1Compiler['G'].AsBigInteger);
        FDSAData.Y := TBigInteger.Create(ASN1Compiler['Y'].AsBigInteger);
      end;

      aaEC: begin
        if not PublicKeyOnly then begin
          DecodeKeyFromECFormat(KeyData, nil);
          if not FReady then
            DecodeKeyFromX25519Format(KeyData, phPure);

          if not FReady then
            Exit;
        end
        else begin
          if not ASN1Compiler.Parse(asn1_EC_PUBLIC_KEY_DESC, KeyData) then
            Exit;

          SetLength(PublicKeyData, 0);
          AlgorithmOID := ASN1Compiler['Algorithm']['Algorithm'].AsString;
          PublicKeyData := ASN1Compiler['SubjectPublicKey'].AsBytes;

          if AlgorithmOID = OID_EC_PUBLIC_KEY then
            DecodeECParams(ASN1Compiler['Algorithm']['Parameters'].EncodedData)
          else
          if AlgorithmOID = OID_EC_PUBLIC_KEY_RESTRICTED then begin
            if not ASN1Compiler.Parse(asn1_EC_PK_RESTRICTIONS_DESC, ASN1Compiler['Algorithm']['Parameters'].EncodedData) then
              raise EScError.Create(seUnknownECParametersFormat);

            DecodeECParams(ASN1Compiler['EcDomain'].AsBytes);
          end
          else
          if AlgorithmOID = OID_X25519_PUBLICKEY then
            DecodeX25519Params(ASN1Compiler['Algorithm']['Parameters'].EncodedData, phPure)
          else
          if AlgorithmOID = OID_X25519 then
            DecodeX25519Params(ASN1Compiler['Algorithm']['Parameters'].EncodedData, phPure)
          else
          if AlgorithmOID = OID_Ed25519 then
            DecodeX25519Params(ASN1Compiler['Algorithm']['Parameters'].EncodedData, phHash)
          else
            raise EScError.Create(seUnknownECParametersFormat);

          Assert(FECData.ECCryptography <> nil);
          FECData.PublicPoint := FECData.ECCryptography.DecodePointFromOctetString(PublicKeyData, 0, Length(PublicKeyData));
        end;
      end;
    else
      Assert(False);
    end;

    FReady := True;
  finally
    ASN1Compiler.Free;
  end;
end;

procedure TScKey.DecodePublicKeyFromOpenSSHFormat(const KeyData: TBytes);
var
  Reader: TSSH2DataReader;
  AlgStr, ECIdentifier: string;
  PublicKeyData: TBytes;
begin
  Reader := TSSH2DataReader.Create(KeyData);
  try
    AlgStr := Encoding.Default.GetString(Reader.ReadString);
    FAlgorithm := CipherFactory.SSH2NameToPublicKeyAlgorithm(AlgStr);

    case FAlgorithm of
      aaRSA: begin
        FRSAData.PublicExponent := Reader.ReadAsBigInteger;
        FRSAData.PublicModulus := Reader.ReadAsBigInteger;
        FreeAndNil(FRSAData.D);
        FreeAndNil(FRSAData.P);
        FreeAndNil(FRSAData.Q);
        FreeAndNil(FRSAData.Qinv);
      end;

      aaDSA: begin
        FDSAData.P := Reader.ReadAsBigInteger;
        FDSAData.Q := Reader.ReadAsBigInteger;
        FDSAData.G := Reader.ReadAsBigInteger;
        FDSAData.Y := Reader.ReadAsBigInteger;
        FreeAndNil(FDSAData.X);
      end;

      aaEC: begin
        if SameText(AlgStr, ED25519_TYPE_HEADER) then begin
          FECData.ECName := x25519;
          FECData.ECCryptography := TScECCryptographyX25519.Create;
          TScECCryptographyX25519(FECData.ECCryptography).FPreHashType := phHash;
        end
        else begin
          ECIdentifier := Encoding.Default.GetString(Reader.ReadString);
          FECData.ECName := TCipherSuites.SshIdToECName(ECIdentifier);
          FECData.ECCryptography := EllipticCurvesParameters[FECData.ECName].CryptographyClass.Create(EllipticCurvesParameters[FECData.ECName]);
        end;

        SetLength(PublicKeyData, 0);
        PublicKeyData := Reader.ReadString;
        FECData.PublicPoint := FECData.ECCryptography.DecodePointFromOctetString(PublicKeyData, 0, Length(PublicKeyData));
      end;
    else
      Assert(False);
    end;

    if Reader.Rest <> 0 then
      RaiseError;

    FReady := True;
  finally
    Reader.Free;
  end;
end;

procedure TScKey.DecodePrivateKeyFromOpenSSHFormat(const KeyData: TBytes; const Password: string);
var
  Reader: TSSH2DataReader;
  Version, CipherName, KdfName: string;
  KdfOptions: TBytes;
  PrivateKeyBuf: TBytes;
  KeyBuf: TBytes;
  Num, Iters: integer;
  AlgStr, ECIdentifier: string;
  Cipher: TSymmetricAlgorithm;
  Key, Salt, IV: TBytes;
begin
  // https://cvsweb.openbsd.org/src/usr.bin/ssh/PROTOCOL.key?annotate=HEAD
  // https://peterlyons.com/problog/2017/12/openssh-ed25519-private-key-file-format

{$IFNDEF VER9P}
  SetLength(PrivateKeyBuf, 0);
  SetLength(KdfOptions, 0);
  SetLength(Salt, 0);
  SetLength(KeyBuf, 0);
{$ENDIF}

  Reader := TSSH2DataReader.Create(KeyData);
  try
    Version := Encoding.Default.GetString(Reader.Read(Length(OPENSSH_AUTH_MAGIC)));
    if not SameText(Version, OPENSSH_AUTH_MAGIC) then
      raise EScError.Create(seWrongDataFormat);

    CipherName := Encoding.Default.GetString(Reader.ReadString);
    KdfName := Encoding.Default.GetString(Reader.ReadString);
    KdfOptions := Reader.ReadString;

    Num := Reader.ReadInt32;
    if Num <> 1 then
      raise EScError.Create(seWrongDataFormat);

    Reader.ReadString; // PublicKeyBuf
    PrivateKeyBuf := Reader.ReadString;
  finally
    Reader.Free;
  end;

  if SameText(KdfName, 'bcrypt') then begin
    if Password = '' then
      raise EScError.Create(sePasswordNotSpecified);

    Reader := TSSH2DataReader.Create(KdfOptions);
    try
      Salt := Reader.ReadString;
      Iters := Reader.ReadInt32;
    finally
      Reader.Free;
    end;

    Key := TScKeyDerivationProcessor.OpenSSHBCrypt(Password, Salt, Iters, 32{Key} + 16{IV});
    SetLength(IV, 16);
    Move(Key[32], IV[0], 16);
    SetLength(Key, 32);

    Cipher := CipherFactory.CreateCipher(CipherFactory.SSH2NameToCipherAlgorithm(CipherName), Key, IV);
    Cipher.CreateDecryptor.TransformBlock(@PrivateKeyBuf[0], 0, Length(PrivateKeyBuf), @PrivateKeyBuf[0], 0);
  end
  else
  if not SameText(KdfName, 'none') then
    raise EScError.Create(seUnsupportedKeyEncryption);

  Reader := TSSH2DataReader.Create(PrivateKeyBuf);
  try
    if Reader.ReadInt32 <> Reader.ReadInt32 then // checkint
      raise EScError.Create(seWrongDataFormat);

    AlgStr := Encoding.Default.GetString(Reader.ReadString);
    if SameText(AlgStr, RSA_TYPE_HEADER) then begin
      FAlgorithm := aaRSA;

      FRSAData.PublicModulus := Reader.ReadAsBigInteger;
      FRSAData.PublicExponent := Reader.ReadAsBigInteger;
      FRSAData.D := Reader.ReadAsBigInteger;
      FRSAData.Qinv := Reader.ReadAsBigInteger;
      FRSAData.P := Reader.ReadAsBigInteger;
      FRSAData.Q := Reader.ReadAsBigInteger;
      Calc_DP_DQ;
    end
    else
    if SameText(AlgStr, ED25519_TYPE_HEADER) then begin
      FAlgorithm := aaEC;
      KeyBuf := Reader.ReadString;
      if Length(KeyBuf) <> 32 then
        raise EScError.Create(seWrongDataFormat);

      FECData.ECName := x25519;
      FECData.ECCryptography := TScECCryptographyX25519.Create;
      TScECCryptographyX25519(FECData.ECCryptography).FPreHashType := phHash;
      FECData.PublicPoint := FECData.ECCryptography.DecodePointFromOctetString(KeyBuf, 0, Length(KeyBuf));

      KeyBuf := Reader.ReadString;
      if Length(KeyBuf) <> 64 then
        raise EScError.Create(seWrongDataFormat);

      FECData.PrivateKoef := T32BytesField.Create(KeyBuf);
    end
    else
    if SameText(Copy(AlgStr, 1, Length(ECDSA_SHA2_TYPE_HEADER)), ECDSA_SHA2_TYPE_HEADER) then begin
      FAlgorithm := aaEC;
      ECIdentifier := Encoding.Default.GetString(Reader.ReadString);
      FECData.ECName := TCipherSuites.SshIdToECName(ECIdentifier);
      FECData.ECCryptography := EllipticCurvesParameters[FECData.ECName].CryptographyClass.Create(EllipticCurvesParameters[FECData.ECName]);

      KeyBuf := Reader.ReadString;
      FECData.PublicPoint := FECData.ECCryptography.DecodePointFromOctetString(KeyBuf, 0, Length(KeyBuf));
      FECData.PrivateKoef := Reader.ReadAsBigInteger;
    end
    else
      raise EScError.Create(seWrongDataFormat);

    FReady := True;
  finally
    Reader.Free;
  end;
end;

procedure TScKey.DecodeKeyFromOpenSSLFormat(const KeyData: TBytes; PublicKeyOnly: boolean);
var
  ASN1Compiler: TScASN1Compiler;
begin
  ASN1Compiler := TScASN1Compiler.Create;
  try
    if PublicKeyOnly then begin
      if ASN1Compiler.Parse(asn1_RSA_PUBLIC_KEY_OPENSSL_DESC, KeyData) then
        FAlgorithm := aaRSA
      else
      if ASN1Compiler.Parse(asn1_RSA_PUBLIC_KEY_OPENSSL_DESC2, KeyData) then
        FAlgorithm := aaRSA
      else
      if ASN1Compiler.Parse(asn1_DSA_PUBLIC_KEY_OPENSSL_DESC, KeyData) then
        FAlgorithm := aaDSA
      else
        Exit;
    end
    else begin
      if ASN1Compiler.Parse(asn1_RSA_PRIVATE_KEY_OPENSSL_DESC, KeyData) then
        FAlgorithm := aaRSA
      else
      if ASN1Compiler.Parse(asn1_DSA_PRIVATE_KEY_OPENSSL_DESC, KeyData) then
        FAlgorithm := aaDSA
      else
        Exit;
    end;

    case FAlgorithm of
      aaRSA: begin
        if PublicKeyOnly then begin
          FreeAndNil(FRSAData.D);
          FreeAndNil(FRSAData.P);
          FreeAndNil(FRSAData.Q);
          FreeAndNil(FRSAData.Qinv);
        end
        else begin
          FRSAData.D := TBigInteger.Create(ASN1Compiler['D'].AsBigInteger);
          FRSAData.P := TBigInteger.Create(ASN1Compiler['P'].AsBigInteger);
          FRSAData.Q := TBigInteger.Create(ASN1Compiler['Q'].AsBigInteger);
          FRSAData.Qinv := TBigInteger.Create(ASN1Compiler['U'].AsBigInteger);
          Calc_DP_DQ;
        end;

        FRSAData.PublicModulus := TBigInteger.Create(ASN1Compiler['PubMod'].AsBigInteger);
        FRSAData.PublicExponent := TBigInteger.Create(ASN1Compiler['PubExp'].AsBigInteger);
      end;

      aaDSA: begin
        FDSAData.P := TBigInteger.Create(ASN1Compiler['P'].AsBigInteger);
        FDSAData.Q := TBigInteger.Create(ASN1Compiler['Q'].AsBigInteger);
        FDSAData.G := TBigInteger.Create(ASN1Compiler['G'].AsBigInteger);
        if PublicKeyOnly then begin
          FreeAndNil(FDSAData.X);
          FDSAData.Y := TBigInteger.Create(ASN1Compiler['Y'].AsBigInteger);
        end
        else begin
          FDSAData.X := TBigInteger.Create(ASN1Compiler['X'].AsBigInteger);
          CalcY;
        end;
      end;

      aaEC:
        raise EScError.Create(seWrongDataFormat);
    else
      Assert(False);
    end;

    FReady := True;
  finally
    ASN1Compiler.Free;
  end;
end;

procedure TScKey.DecodeDSAKeyFromCertificate(const PublicKey, KeyParameters: TBytes);
var
  ASN1Compiler: TScASN1Compiler;
begin
  ASN1Compiler := TScASN1Compiler.Create;
  try
    FAlgorithm := aaDSA;

    if not ASN1Compiler.Parse(asn1_DSA_CERT_PUBLIC_KEY_DESC, PublicKey) then
      RaiseError;
    FDSAData.Y := TBigInteger.Create(ASN1Compiler['Y'].AsBigInteger);

    if not ASN1Compiler.Parse(asn1_DSA_TRUNC_PUBLIC_KEY_PKCS1_DESC, KeyParameters) then
      RaiseError;
    FDSAData.P := TBigInteger.Create(ASN1Compiler['P'].AsBigInteger);
    FDSAData.Q := TBigInteger.Create(ASN1Compiler['Q'].AsBigInteger);
    FDSAData.G := TBigInteger.Create(ASN1Compiler['G'].AsBigInteger);

    FReady := True;
  finally
    ASN1Compiler.Free;
  end;
end;

procedure TScKey.DecodeECKeyFromCertificate(const PublicKey, KeyParameters: TBytes);
begin
  FAlgorithm := aaEC;
  DecodeECParams(KeyParameters);
  Assert(FECData.ECCryptography <> nil);
  FECData.PublicPoint := FECData.ECCryptography.DecodePointFromOctetString(PublicKey, 0, Length(PublicKey));

  FReady := True;
end;

procedure TScKey.DecodeX25519KeyFromCertificate(const PublicKey: TBytes; PreHashType: TScX25519PreHashType);
begin
  FAlgorithm := aaEC;
  FECData.ECName := x25519;
  FECData.ECCryptography := TScECCryptographyX25519.Create;
  TScECCryptographyX25519(FECData.ECCryptography).FPreHashType := PreHashType;
  FECData.PublicPoint := FECData.ECCryptography.DecodePointFromOctetString(PublicKey, 0, Length(PublicKey));

  FReady := True;
end;

procedure TScKey.DecodePrivateKeyFromPKCS8Format(const KeyData: TBytes);
var
  ASN1Compiler: TScASN1Compiler;
  PrivateKeyAlgorithmOID: string;
  PrivateKeyAlgorithmData, PrivateKeyData: TBytes;
  IsEd25519: boolean;
  PreHashType: TScX25519PreHashType;
begin
  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_PKCS8_KEY_DESC, KeyData) then
      Exit;

    IsEd25519 := False;
    PreHashType := phPure;
    PrivateKeyAlgorithmOID := ASN1Compiler['PrivateKeyAlgorithm']['Algorithm'].AsString;
    if PrivateKeyAlgorithmOID = OID_RSA_ENCRYPTION then
      FAlgorithm := aaRSA
    else
    if PrivateKeyAlgorithmOID = OID_DSA_ENCRYPTION then
      FAlgorithm := aaDSA
    else
    if PrivateKeyAlgorithmOID = OID_EC_PUBLIC_KEY then
      FAlgorithm := aaEC
    else
    if PrivateKeyAlgorithmOID = OID_X25519 then begin
      FAlgorithm := aaEC;
      IsEd25519 := True;
      PreHashType := phPure;
    end
    else
    if PrivateKeyAlgorithmOID = OID_Ed25519 then begin
      FAlgorithm := aaEC;
      IsEd25519 := True;
      PreHashType := phHash;
    end
    else
      Exit;

    SetLength(PrivateKeyData, 0);
    SetLength(PrivateKeyAlgorithmData, 0);
    PrivateKeyAlgorithmData := ASN1Compiler['PrivateKeyAlgorithm']['Parameters'].EncodedData;
    PrivateKeyData := ASN1Compiler['PrivateKey'].AsBytes;

    case FAlgorithm of
      aaRSA: begin
        if not ASN1Compiler.Parse(asn1_RSA_PRIVATE_KEY_PKCS1_DESC, PrivateKeyData) then
          Exit;

        FRSAData.PublicModulus := TBigInteger.Create(ASN1Compiler['PubMod'].AsBigInteger);
        FRSAData.PublicExponent := TBigInteger.Create(ASN1Compiler['PubExp'].AsBigInteger);
        FRSAData.D := TBigInteger.Create(ASN1Compiler['D'].AsBigInteger);
        FRSAData.P := TBigInteger.Create(ASN1Compiler['P'].AsBigInteger);
        FRSAData.Q := TBigInteger.Create(ASN1Compiler['Q'].AsBigInteger);
        FRSAData.Qinv := TBigInteger.Create(ASN1Compiler['U'].AsBigInteger);
        Calc_DP_DQ;
      end;

      aaDSA: begin
        if not ASN1Compiler.Parse(asn1_DSA_TRUNC_PUBLIC_KEY_PKCS1_DESC, PrivateKeyAlgorithmData) then
          Exit;

        FDSAData.P := TBigInteger.Create(ASN1Compiler['P'].AsBigInteger);
        FDSAData.Q := TBigInteger.Create(ASN1Compiler['Q'].AsBigInteger);
        FDSAData.G := TBigInteger.Create(ASN1Compiler['G'].AsBigInteger);

        if not ASN1Compiler.Parse(asn1_DSA_PRIVATE_KEY_PKCS8_DESC, PrivateKeyData) then
          Exit;

        FDSAData.X := TBigInteger.Create(ASN1Compiler['X'].AsBigInteger);
        CalcY;
      end;

      aaEC: begin
        if IsEd25519 then
          DecodeKeyFromX25519Format(PrivateKeyData, PreHashType)
        else
          DecodeKeyFromECFormat(PrivateKeyData, PrivateKeyAlgorithmData);
        if not FReady then
          Exit;
      end;
    else
      Assert(False);
    end;

    FReady := True;
  finally
    ASN1Compiler.Free;
  end;
end;

procedure TScKey.DecodePrivateKeyFromPKCS8EncFormat(const KeyData: TBytes; const Password: string);
var
  ASN1Compiler: TScASN1Compiler;
  EncryptionAlgorithmOID: string;
  EncryptionAlgorithmParams: TBytes;
  EncryptedData: TBytes;
  Cipher: TSymmetricAlgorithm;
begin
  SetLength(EncryptionAlgorithmParams, 0);
  SetLength(EncryptedData, 0);

  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_PRIVATE_KEY_PKCS8ENC_DESC, KeyData) then
      Exit;

    EncryptionAlgorithmOID := ASN1Compiler['EncryptionAlgorithm']['Algorithm'].AsString;
    EncryptedData := ASN1Compiler['EncryptedData'].AsBytes;
    EncryptionAlgorithmParams := ASN1Compiler['EncryptionAlgorithm']['Parameters'].EncodedData;

    Cipher := TScKeyDerivationProcessor.CreateCipherByOId(EncryptionAlgorithmOID, EncryptionAlgorithmParams, Password);
    if Cipher = nil then
      Exit;

    Cipher.CreateDecryptor.TransformBlock(EncryptedData, 0, Length(EncryptedData));
    DecodePrivateKeyFromPKCS8Format(EncryptedData);
  finally
    ASN1Compiler.Free;
  end;
end;

procedure TScKey.DecodePrivateKeyFromIETFFormat(const KeyData: TBytes; const Password: string);
var
  Reader: TSSH2DataReader;
  Data, IETFFKey: TBytes;
  AlgStr: string;
  num: integer;
  Cipher: TSymmetricAlgorithm;
  Algo: TScSymmetricAlgorithm;
  Key, IV: TBytes;
begin
  Reader := TSSH2DataReader.Create(KeyData);
  try
    num := Reader.ReadInt32;
    if num <> IETF_MAGIC_VAL then
      Exit;

    Reader.ReadInt32; // Length

    SetLength(IETFFKey, 0);
    SetLength(Data, 0);
    Data := Reader.ReadString;
    AlgStr := Encoding.Default.GetString(Data);
    if AlgStr = IETF_RSA then
      FAlgorithm := aaRSA
    else
    if AlgStr = IETF_DSA then
      FAlgorithm := aaDSA
    else
      Exit;

    Data := Reader.ReadString;
    AlgStr := Encoding.Default.GetString(Data);
    Algo := CipherFactory.SSH2NameToCipherAlgorithm(STripleDES_cbc);

    if AlgStr = STripleDES_cbc then begin
      SetLength(IV, CipherFactory.GetBlockSize(Algo));
      FillChar(IV[0], Length(IV), 0);
      SetLength(Key, 0);
      Key := TScKeyDerivationProcessor.PBKDF1FromPassword(haMD5, Password, nil, CipherFactory.GetKeySize(Algo), 1, True);
      Cipher := CipherFactory.CreateCipher(Algo, Key, IV);
      FillChar(Key[0], Length(Key), 0); // to protect

      IETFFKey := Reader.ReadString;
      Cipher.CreateDecryptor.TransformBlock(IETFFKey, 0, Length(IETFFKey));
    end
    else
    if AlgStr = SNone then
      IETFFKey := Reader.ReadString
    else
      Exit;

  finally
    Reader.Free;
  end;

  Reader := TSSH2DataReader.Create(IETFFKey);
  try
    Reader.ReadInt32; // Length

    case FAlgorithm of
      aaRSA: begin
        FRSAData.PublicExponent := Reader.ReadBigIntWithBits;
        FRSAData.D := Reader.ReadBigIntWithBits;
        FRSAData.PublicModulus := Reader.ReadBigIntWithBits;
        FRSAData.Qinv := Reader.ReadBigIntWithBits;
        FRSAData.Q := Reader.ReadBigIntWithBits;
        FRSAData.P := Reader.ReadBigIntWithBits;
        Calc_DP_DQ;
      end;

      aaDSA: begin
        Reader.ReadInt32;
        FDSAData.P := Reader.ReadBigIntWithBits;
        FDSAData.G := Reader.ReadBigIntWithBits;
        FDSAData.Q := Reader.ReadBigIntWithBits;
        FDSAData.Y := Reader.ReadBigIntWithBits;
        FDSAData.X := Reader.ReadBigIntWithBits;
      end;

      aaEC:
        raise EScError.Create(seWrongDataFormat);
    else
      Assert(False);
    end;
  finally
    Reader.Free;
  end;

  FReady := True;
end;

procedure TScKey.DecodePublicKeyFromIETFFormat(const KeyData: TBytes);
begin
  DecodePublicKeyFromOpenSSHFormat(KeyData);
end;

procedure TScKey.DecodeKeyFromECFormat(const KeyData, ECParamsData: TBytes);
var
  ASN1Compiler: TScASN1Compiler;
  ParamsLexemInfo: TScLexemInfo;
  s: string;
begin
  ASN1Compiler := TScASN1Compiler.Create;
  try
    FAlgorithm := aaEC;

    if not ASN1Compiler.Parse(asn1_EC_PRIVATE_KEY_DESC, KeyData) then
      Exit;

    ParamsLexemInfo := ASN1Compiler['Params'].GetChoiceLexem;
    if not ParamsLexemInfo.IsNull then
      DecodeECParams(ParamsLexemInfo)
    else
    if Length(ECParamsData) > 0 then
      DecodeECParams(ECParamsData)
    else
      raise EScError.Create(seUnknownECParametersFormat);

    if not (FECData.ECCryptography is TScECCryptographyBI) then
      raise EScError.Create(seUnknownECParametersFormat);

    s := BytesToHexStr(ASN1Compiler['PrivateKey'].AsBytes);
    FECData.PrivateKoef := TBigInteger.Create(s, 16);

    Assert(FECData.ECCryptography <> nil);
    FECData.PublicPoint := FECData.ECCryptography.MulPoint(TScECCryptographyBI(FECData.ECCryptography).FG, FECData.PrivateKoef);

    FReady := True;
  finally
    ASN1Compiler.Free;
  end;
end;

procedure TScKey.DecodeKeyFromX25519Format(const KeyData: TBytes; PreHashType: TScX25519PreHashType);
var
  ASN1Compiler: TScASN1Compiler;
  KeyBuf: TBytes;
begin
  ASN1Compiler := TScASN1Compiler.Create;
  try
    FAlgorithm := aaEC;

    if not ASN1Compiler.Parse(asn1_EC_POINT_DESC, KeyData) then
      Exit;

    SetLength(KeyBuf, 0);
    KeyBuf := ASN1Compiler.Root.AsBytes;
    if Length(KeyBuf) <> 32 then
      Exit;

    FECData.ECName := x25519;
    FECData.ECCryptography := TScECCryptographyX25519.Create;
    TScECCryptographyX25519(FECData.ECCryptography).FPreHashType := PreHashType;

    FECData.PrivateKoef := T32BytesField.Create(KeyBuf);
    FECData.PublicPoint := TScECCryptographyX25519(FECData.ECCryptography).PublicFromPrivate(T32BytesField(FECData.PrivateKoef));
    FReady := True;
  finally
    ASN1Compiler.Free;
  end;
end;

procedure TScKey.DecodeX25519Params(const Data: TBytes; PreHashType: TScX25519PreHashType);
var
  ASN1Compiler: TScASN1Compiler;
begin
  FECData.ECName := x25519;
  FECData.ECCryptography := TScECCryptographyX25519.Create;
  TScECCryptographyX25519(FECData.ECCryptography).FPreHashType := PreHashType;

  if Length(Data) = 0 then
    Exit;

  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_X25519_PUBLIC_KEY_PARAMS_DESC, Data) then
      raise EScError.Create(seUnknownECParametersFormat);

    case ASN1Compiler['Parameters'].AsInteger of
      1:
        TScECCryptographyX25519(FECData.ECCryptography).FPreHashType := phPure;
      2:
        TScECCryptographyX25519(FECData.ECCryptography).FPreHashType := phHash;
    else
      raise EScError.Create(seUnknownECParametersFormat);
    end;
  finally
    ASN1Compiler.Free;
  end;
end;

function TScKey.EncodeX25519Params: TBytes;
var
  ASN1Compiler: TScASN1Compiler;
begin
  Assert(FECData.ECCryptography <> nil);

  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_X25519_PUBLIC_KEY_PARAMS_DESC) then
      raise EScError.Create(seUnsupportedECParameters);

    case TScECCryptographyX25519(FECData.ECCryptography).FPreHashType of
      phPure:
        ASN1Compiler['Parameters'].AsInteger := 1;
      phHash:
        ASN1Compiler['Parameters'].AsInteger := 2;
    else
      raise EScError.Create(seUnsupportedECParameters);
    end;

    Result := ASN1Compiler.Build;
  finally
    ASN1Compiler.Free;
  end;
end;

procedure TScKey.DecodeECParams(const Data: TBytes);
var
  ASN1Compiler: TScASN1Compiler;
begin
  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_EC_DOMAIN_PARAMS_DESC, Data) then
      raise EScError.Create(seUnknownECParametersFormat);

    DecodeECParams(ASN1Compiler.Root.GetChoiceLexem);
  finally
    ASN1Compiler.Free;
  end;
end;

procedure TScKey.DecodeECParams(ParamsLexemInfo: TScLexemInfo);
var
  FieldIDLexemInfo, CurveLexemInfo: TScLexemInfo;
  FieldIDParamsLexemInfo, CharacteristicTwoParamsLexemInfo: TScLexemInfo;
  CryptographyBI: TScECCryptographyBI;
  buf: TBytes;
  s: string;
  k0, k1, k2, k3: integer;
begin
  Assert(FECData.ECCryptography = nil);

  if SameText(ParamsLexemInfo.Name, 'Specified') then begin
    FieldIDLexemInfo := ParamsLexemInfo['FieldID'];
    FieldIDParamsLexemInfo := FieldIDLexemInfo['Params'];

    if FieldIDLexemInfo['FieldType'].AsOID = OID_PRIME_FIELD then begin
      if not SameText(FieldIDParamsLexemInfo.Name, 'PrimeP') then
        raise EScError.Create(seUnknownECParametersFormat);

      CryptographyBI := TScECCryptographyFp.Create;
      FECData.ECCryptography := CryptographyBI;
      s := BytesToHexStr(FieldIDParamsLexemInfo.AsBytes);
      CryptographyBI.FModP := TBigInteger.Create(s, 16);
    end
    else
    if FieldIDLexemInfo['FieldType'].AsOID = OID_CHARACTERISTIC_TWO_FIELD then begin
      if not SameText(FieldIDParamsLexemInfo.Name, 'CharacteristicTwo') then
        raise EScError.Create(seUnknownECParametersFormat);

      CryptographyBI := TScECCryptographyF2m.Create;
      FECData.ECCryptography := CryptographyBI;
      k0 := FieldIDParamsLexemInfo['M'].AsInteger;
      CryptographyBI.FModP := TBigInteger.Create(1);
      CryptographyBI.FModP.SetBit(k0);

      CharacteristicTwoParamsLexemInfo := FieldIDParamsLexemInfo['Params'].GetChoiceLexem;
      if SameText(CharacteristicTwoParamsLexemInfo.Name, 'Trinomial') then begin
        k1 := CharacteristicTwoParamsLexemInfo.AsInteger;
        CryptographyBI.FModP.SetBit(k1);
      end
      else
      if SameText(CharacteristicTwoParamsLexemInfo.Name, 'Pentanomial') then begin
        k1 := CharacteristicTwoParamsLexemInfo['k1'].AsInteger;
        CryptographyBI.FModP.SetBit(k1);
        k2 := CharacteristicTwoParamsLexemInfo['k2'].AsInteger;
        CryptographyBI.FModP.SetBit(k2);
        k3 := CharacteristicTwoParamsLexemInfo['k3'].AsInteger;
        CryptographyBI.FModP.SetBit(k3);
      end
      else
        raise EScError.Create(seUnknownECParametersFormat);
    end
    else
      raise EScError.Create(seUnknownECParametersFormat);

    CryptographyBI.FSize := CryptographyBI.FModP.BitCount shr 3;
    if (CryptographyBI.FModP.BitCount and 7) > 0 then
      Inc(CryptographyBI.FSize);

    CurveLexemInfo := ParamsLexemInfo['Curve'];
    s := BytesToHexStr(CurveLexemInfo['A'].AsBytes);
    CryptographyBI.FA := TBigInteger.Create(s, 16);
    s := BytesToHexStr(CurveLexemInfo['B'].AsBytes);
    CryptographyBI.FB := TBigInteger.Create(s, 16);

    CryptographyBI.FSeed := CurveLexemInfo['Seed'].AsBytes;

    s := BytesToHexStr(ParamsLexemInfo['Order'].AsBytes);
    CryptographyBI.FN := TBigInteger.Create(s, 16);

    CryptographyBI.FCofactor := ParamsLexemInfo['Cofactor'].AsInteger;

    CryptographyBI.Prepare; // call before DecodePointFromOctetString

    SetLength(buf, 0);
    buf := ParamsLexemInfo['Base'].AsBytes;
    CryptographyBI.FG := TScECPointBI(CryptographyBI.DecodePointFromOctetString(buf, 0, Length(buf)));
  end
  else
  if SameText(ParamsLexemInfo.Name, 'Named') then begin
    FECData.ECName := TCipherSuites.OidToECName(ParamsLexemInfo.AsOID);
    FECData.ECCryptography := EllipticCurvesParameters[FECData.ECName].CryptographyClass.Create(EllipticCurvesParameters[FECData.ECName]);
  end
  else
    raise EScError.Create(seUnknownECParametersFormat);
end;

function TScKey.EncodeECParams: TBytes;
var
  ASN1Compiler: TScASN1Compiler;
  SpecifiedLexemInfo, FieldIDLexemInfo, CurveLexemInfo: TScLexemInfo;
  FieldIDParamsLexemInfo, CharacteristicTwoParamsLexemInfo: TScLexemInfo;
  CryptographyBI: TScECCryptographyBI;
  BitsArray: TIntArr;
  s: string;
begin
  Assert(FECData.ECCryptography <> nil);

  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_EC_DOMAIN_PARAMS_DESC) then
      raise EScError.Create(seUnknownECParametersFormat);

    if not FECData.ECCryptography.FIsNamed then begin
      CryptographyBI := TScECCryptographyBI(FECData.ECCryptography);
      ASN1Compiler.Root.SetChoiceLexem('Specified');
      SpecifiedLexemInfo := ASN1Compiler['Specified'];
      SpecifiedLexemInfo['Version'].AsInteger := 1;

      FieldIDLexemInfo := SpecifiedLexemInfo['FieldID'];

      if IsClass(FECData.ECCryptography, TScECCryptographyFp) then begin
        FieldIDLexemInfo['FieldType'].AsOID := OID_PRIME_FIELD;
        FieldIDLexemInfo['Params'].SetChoiceLexem('PrimeP');
        FieldIDLexemInfo['PrimeP'].AsBytes := CryptographyBI.BigIntToHexBuf(CryptographyBI.FModP);
      end
      else
      if IsClass(FECData.ECCryptography, TScECCryptographyF2m) then begin
        FieldIDLexemInfo['FieldType'].AsOID := OID_CHARACTERISTIC_TWO_FIELD;
        FieldIDLexemInfo['Params'].SetChoiceLexem('CharacteristicTwo');
        FieldIDParamsLexemInfo := FieldIDLexemInfo['Params'];

        SetLength(BitsArray, 0);
        BitsArray := CryptographyBI.FModP.GetSetBitsArray;
        FieldIDParamsLexemInfo['M'].AsInteger := BitsArray[0];

        if Length(BitsArray) = 3 then begin
          FieldIDParamsLexemInfo['Basis'].AsOID := OID_CHARACTERISTIC_TWO_BASIS_2;
          FieldIDParamsLexemInfo['Params'].SetChoiceLexem('Trinomial');

          CharacteristicTwoParamsLexemInfo := FieldIDParamsLexemInfo['Trinomial'];
          CharacteristicTwoParamsLexemInfo.AsInteger := BitsArray[1];
        end
        else
        if Length(BitsArray) = 5 then begin
          FieldIDParamsLexemInfo['Basis'].AsOID := OID_CHARACTERISTIC_TWO_BASIS_3;
          FieldIDParamsLexemInfo['Params'].SetChoiceLexem('Pentanomial');

          CharacteristicTwoParamsLexemInfo := FieldIDParamsLexemInfo['Pentanomial'];
          CharacteristicTwoParamsLexemInfo['k1'].AsInteger := BitsArray[3];
          CharacteristicTwoParamsLexemInfo['k2'].AsInteger := BitsArray[2];
          CharacteristicTwoParamsLexemInfo['k3'].AsInteger := BitsArray[1];
        end
        else
          raise EScError.Create(seUnsupportedECParameters);
      end
      else
      if IsClass(FECData.ECCryptography, TScECCryptographyX25519) then begin
        raise EScError.Create(seUnsupportedECParameters);
      end
      else
        raise EScError.Create(seUnsupportedECParameters);

      CurveLexemInfo := SpecifiedLexemInfo['Curve'];
      s := CryptographyBI.FA.ToString(16);
      CurveLexemInfo['A'].AsBytes := HexToBytes(s);
      s := CryptographyBI.FB.ToString(16);
      CurveLexemInfo['B'].AsBytes := HexToBytes(s);

      if Length(CryptographyBI.FSeed) > 0 then
        CurveLexemInfo['Seed'].AsBytes := CryptographyBI.FSeed;

      s := CryptographyBI.FN.ToString(16);
      SpecifiedLexemInfo['Order'].AsBytes := HexToBytes(s);

      SpecifiedLexemInfo['Cofactor'].AsInteger := CryptographyBI.FCofactor;
      SpecifiedLexemInfo['Base'].AsBytes := CryptographyBI.EncodePointToOctetString(CryptographyBI.FG);
    end
    else begin
      ASN1Compiler.Root.SetChoiceLexem('Named');
      ASN1Compiler['Named'].AsOID := TCipherSuites.ECNameToOid(FECData.ECName);
    end;

    Result := ASN1Compiler.Build;
  finally
    ASN1Compiler.Free;
  end;
end;

function TScKey.EncodeKeyToPKCS1Format(PublicKeyOnly: boolean): TBytes;
var
  ASN1Compiler: TScASN1Compiler;
begin
  Assert(FReady = True);

  ASN1Compiler := TScASN1Compiler.Create;
  try
    case FAlgorithm of
      aaRSA: begin
        if PublicKeyOnly then begin
          if not ASN1Compiler.Parse(asn1_RSA_PUBLIC_KEY_PKCS1_DESC) then
            raise EScError.Create(seWrongDataFormat);
        end
        else begin
          if not ASN1Compiler.Parse(asn1_RSA_PRIVATE_KEY_PKCS1_DESC) then
            raise EScError.Create(seWrongDataFormat);

          ASN1Compiler['Version'].AsInteger := 0;

          ASN1Compiler['D'].AsBigInteger := FRSAData.D.GetBytes;
          ASN1Compiler['P'].AsBigInteger := FRSAData.P.GetBytes;
          ASN1Compiler['Q'].AsBigInteger := FRSAData.Q.GetBytes;
          ASN1Compiler['U'].AsBigInteger := FRSAData.Qinv.GetBytes;
          ASN1Compiler['DP'].AsBigInteger := FRSAData.DP.GetBytes;
          ASN1Compiler['DQ'].AsBigInteger := FRSAData.DQ.GetBytes;
        end;

        ASN1Compiler['PubMod'].AsBigInteger := FRSAData.PublicModulus.GetBytes;
        ASN1Compiler['PubExp'].AsBigInteger := FRSAData.PublicExponent.GetBytes;
      end;

      aaDSA: begin
        if PublicKeyOnly then begin
          if not ASN1Compiler.Parse(asn1_DSA_PUBLIC_KEY_PKCS1_DESC) then
            raise EScError.Create(seWrongDataFormat);
        end
        else begin
          if not ASN1Compiler.Parse(asn1_DSA_PRIVATE_KEY_PKCS1_DESC) then
            raise EScError.Create(seWrongDataFormat);

          ASN1Compiler['Version'].AsInteger := 0;
          ASN1Compiler['X'].AsBigInteger := FDSAData.X.GetBytes;
        end;

        ASN1Compiler['P'].AsBigInteger := FDSAData.P.GetBytes;
        ASN1Compiler['Q'].AsBigInteger := FDSAData.Q.GetBytes;
        ASN1Compiler['G'].AsBigInteger := FDSAData.G.GetBytes;
        ASN1Compiler['Y'].AsBigInteger := FDSAData.Y.GetBytes;
      end;

      aaEC: begin
        Assert(FECData.ECCryptography <> nil);

        if PublicKeyOnly then begin
          if not ASN1Compiler.Parse(asn1_EC_PUBLIC_KEY_DESC) then
            raise EScError.Create(seWrongDataFormat);

          if IsClass(FECData.ECCryptography, TScECCryptographyX25519) then begin
            ASN1Compiler['Algorithm']['Algorithm'].AsOID := OID_X25519_PUBLICKEY;
            ASN1Compiler['Algorithm']['Parameters'].EncodedData := EncodeX25519Params;
            ASN1Compiler['SubjectPublicKey'].AsBytes := FECData.ECCryptography.EncodePointToOctetString(FECData.PublicPoint);
          end
          else begin
            ASN1Compiler['Algorithm']['Algorithm'].AsOID := OID_EC_PUBLIC_KEY;
            ASN1Compiler['Algorithm']['Parameters'].EncodedData := EncodeECParams;
            ASN1Compiler['SubjectPublicKey'].AsBytes := FECData.ECCryptography.EncodePointToOctetString(FECData.PublicPoint);
          end;
        end
        else begin
          if IsClass(FECData.ECCryptography, TScECCryptographyX25519) then begin
            if not ASN1Compiler.Parse(asn1_EC_POINT_DESC) then
              raise EScError.Create(seWrongDataFormat);

            ASN1Compiler.Root.AsBytes := T32BytesField(FECData.PrivateKoef).GetBytes;
          end
          else begin
            if not ASN1Compiler.Parse(asn1_EC_PRIVATE_KEY_DESC) then
              raise EScError.Create(seWrongDataFormat);

            ASN1Compiler['Version'].AsInteger := 1;
            ASN1Compiler['PrivateKey'].AsBytes := FECData.ECCryptography.BigIntToHexBuf(FECData.PrivateKoef as TBigInteger);
            ASN1Compiler['Params'].EncodedData := EncodeECParams;
            ASN1Compiler['PublicKey'].AsBytes := FECData.ECCryptography.EncodePointToOctetString(FECData.PublicPoint);
          end;
        end;
      end;
    else
      Assert(False);
    end;

    Result := ASN1Compiler.Build;
  finally
    ASN1Compiler.Free;
  end;
end;

function TScKey.EncodePrivateKeyToPKCS8Format: TBytes;
var
  ASN1Compiler: TScASN1Compiler;
  PrivateKeyLexemInfo: TScLexemInfo;
begin
  Assert(FReady = True);

  ASN1Compiler := TScASN1Compiler.Create;
  try
    case FAlgorithm of
      aaRSA: begin
        if not ASN1Compiler.Parse(asn1_RSA_PKCS8_KEY_DESC) then
          raise EScError.Create(seWrongDataFormat);

        ASN1Compiler['Version'].AsInteger := 0;
        ASN1Compiler['PrivateKey']['Version'].AsInteger := 0;
        ASN1Compiler['PubMod'].AsBigInteger := FRSAData.PublicModulus.GetBytes;
        ASN1Compiler['PubExp'].AsBigInteger := FRSAData.PublicExponent.GetBytes;

        ASN1Compiler['D'].AsBigInteger := FRSAData.D.GetBytes;
        ASN1Compiler['P'].AsBigInteger := FRSAData.P.GetBytes;
        ASN1Compiler['Q'].AsBigInteger := FRSAData.Q.GetBytes;
        ASN1Compiler['U'].AsBigInteger := FRSAData.Qinv.GetBytes;
        ASN1Compiler['DP'].AsBigInteger := FRSAData.DP.GetBytes;
        ASN1Compiler['DQ'].AsBigInteger := FRSAData.DQ.GetBytes;
      end;

      aaDSA: begin
        if not ASN1Compiler.Parse(asn1_DSA_PKCS8_KEY_DESC) then
          raise EScError.Create(seWrongDataFormat);

        ASN1Compiler['Version'].AsInteger := 0;
        ASN1Compiler['P'].AsBigInteger := FDSAData.P.GetBytes;
        ASN1Compiler['Q'].AsBigInteger := FDSAData.Q.GetBytes;
        ASN1Compiler['G'].AsBigInteger := FDSAData.G.GetBytes;
        ASN1Compiler['X'].AsBigInteger := FDSAData.X.GetBytes;
      end;

      aaEC: begin
        Assert(FECData.ECCryptography <> nil);

        if IsClass(FECData.ECCryptography, TScECCryptographyX25519) then begin
          if not ASN1Compiler.Parse(asn1_Ed25519_PKCS8_KEY_DESC) then
            raise EScError.Create(seWrongDataFormat);

          ASN1Compiler['Version'].AsInteger := 0;
          ASN1Compiler['PrivateKeyPoint'].AsBytes := T32BytesField(FECData.PrivateKoef).GetBytes;
        end
        else begin
          if not ASN1Compiler.Parse(asn1_EC_PKCS8_KEY_DESC) then
            raise EScError.Create(seWrongDataFormat);

          ASN1Compiler['Version'].AsInteger := 0;
          ASN1Compiler['PrivateKeyAlgorithm']['Parameters'].EncodedData := EncodeECParams;

          PrivateKeyLexemInfo := ASN1Compiler['PrivateKey'];
          PrivateKeyLexemInfo['Version'].AsInteger := 1;
          PrivateKeyLexemInfo['PrivateKey'].AsBytes := FECData.ECCryptography.BigIntToHexBuf(FECData.PrivateKoef as TBigInteger);
          PrivateKeyLexemInfo['PublicKey'].AsBytes := FECData.ECCryptography.EncodePointToOctetString(FECData.PublicPoint);
        end;
      end;
    else
      Assert(False);
    end;

    Result := ASN1Compiler.Build;
  finally
    ASN1Compiler.Free;
  end;
end;

function TScKey.EncodePrivateKeyToPKCS8EncFormat(const Password: string; Algo: TScSymmetricAlgorithm): TBytes;
var
  ASN1Compiler: TScASN1Compiler;
  PKCS8KeyData: TBytes;
  Key: TBytes;
  LenKeyData, PaddingLen, BlockSize: integer;
  Salt, IV: TBytes;
  Iterations: Word;
  OID: string;
  Cipher: TSymmetricAlgorithm;
begin
  PKCS8KeyData := EncodePrivateKeyToPKCS8Format;

  OID := CipherFactory.EncryptionAlgorithmToOid(Algo);
  LenKeyData := Length(PKCS8KeyData);
  BlockSize := CipherFactory.GetBlockSize(Algo);

  PaddingLen := LenKeyData mod BlockSize;
  if PaddingLen > 0 then begin
    PaddingLen := BlockSize - PaddingLen;

    if PaddingLen > 0 then begin
      SetLength(PKCS8KeyData, LenKeyData + PaddingLen);
      System.FillChar(PKCS8KeyData[LenKeyData], PaddingLen, PaddingLen);
    end;
  end;

  if Random = nil then
    raise Exception.Create(SInternalError);

  Iterations := 2048;
  SetLength(Salt, BlockSize);
  Random.Random(Salt, 0, Length(Salt));
  SetLength(IV, BlockSize);
  Random.Random(IV, 0, Length(IV));
  Key := TScKeyDerivationProcessor.PBKDF2FromPassword(Password, Salt, CipherFactory.GetKeySize(Algo), Iterations);
  Cipher := CipherFactory.CreateCipher(Algo, Key, IV);
  FillChar(Key[0], Length(Key), 0); // to protect
  Cipher.CreateEncryptor.TransformBlock(PKCS8KeyData, 0, Length(PKCS8KeyData));

  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_PRIVATE_KEY_PKCS8_PBES2ENC_DESC) then
      raise EScError.Create(seWrongDataFormat);

    ASN1Compiler['Algo'].AsOID := OID;
    ASN1Compiler['Iters'].AsInteger := Iterations;
    ASN1Compiler['Salt'].AsBytes := Salt;
    ASN1Compiler['IV'].AsBytes := IV;
    ASN1Compiler['EncryptedData'].AsBytes := PKCS8KeyData;

    Result := ASN1Compiler.Build;
  finally
    ASN1Compiler.Free;
  end;
end;

function TScKey.EncodePrivateKeyToIETFFormat(const Password: string): TBytes;
var
  wr: TSSH2DataStream;
  PaddingLen: integer;
  Padding: TBytes;
  Cipher: TSymmetricAlgorithm;
  Algo: TScSymmetricAlgorithm;
  Key, IV, EncryptedBody: TBytes;
begin
  wr := TSSH2DataStream.Create;
  try
    // step1 Key Body
    wr.WriteInt32(0); // this field is filled later

    case FAlgorithm of
      aaRSA: begin
        wr.WriteBigIntWithBits(FRSAData.PublicExponent);
        wr.WriteBigIntWithBits(FRSAData.D);
        wr.WriteBigIntWithBits(FRSAData.PublicModulus);
        wr.WriteBigIntWithBits(FRSAData.Qinv);
        wr.WriteBigIntWithBits(FRSAData.Q);
        wr.WriteBigIntWithBits(FRSAData.P);
      end;

      aaDSA: begin
        wr.WriteInt32(0);
        wr.WriteBigIntWithBits(FDSAData.P);
        wr.WriteBigIntWithBits(FDSAData.G);
        wr.WriteBigIntWithBits(FDSAData.Q);
        wr.WriteBigIntWithBits(FDSAData.Y);
        wr.WriteBigIntWithBits(FDSAData.X);
      end;

      aaEC:
        raise EScError.Create(seNotCompatibleFormatWithECKey);
    else
      Assert(False);
    end;

    Algo := saTripleDES_cbc;
    PaddingLen := 0;
    if Password <> '' then begin
      PaddingLen := wr.DataLength mod CipherFactory.GetBlockSize(Algo);

      if PaddingLen > 0 then begin
        PaddingLen := CipherFactory.GetBlockSize(Algo) - PaddingLen;
        if PaddingLen > 0 then begin
          SetLength(Padding, PaddingLen);
          System.FillChar(Padding[0], PaddingLen, PaddingLen);
          wr.WriteBuf(Padding);
        end;
      end;
    end;

    SetLength(EncryptedBody, 0);
    EncryptedBody := wr.ToBytes;
    PutIntBE(Length(EncryptedBody) - PaddingLen - 4, TValueArr(EncryptedBody), 0);

    // Encrypt if necessary
    if Password <> '' then begin
      SetLength(IV, CipherFactory.GetBlockSize(Algo));
      FillChar(IV[0], Length(IV), 0);
      SetLength(Key, 0);
      Key := TScKeyDerivationProcessor.PBKDF1FromPassword(haMD5, Password, nil, CipherFactory.GetKeySize(Algo), 1, True);
      Cipher := CipherFactory.CreateCipher(Algo, Key, IV);
      FillChar(Key[0], Length(Key), 0); // to protect
      Cipher.CreateEncryptor.TransformBlock(EncryptedBody, 0, Length(EncryptedBody));
    end;

    // step2 make binary Key Data
    wr.Clear;
    wr.WriteInt32(integer(IETF_MAGIC_VAL));
    wr.WriteInt32(0); // For total Size

    case FAlgorithm of
      aaRSA:
        wr.WriteAStr(IETF_RSA);
      aaDSA:
        wr.WriteAStr(IETF_DSA);
      aaEC:
        raise EScError.Create(seNotCompatibleFormatWithECKey);
    else
      Assert(False);
    end;

    if Password = '' then
      wr.WriteAStr(SNone)
    else
      wr.WriteAStr(STripleDES_cbc);
    wr.WriteAsString(EncryptedBody);

    Result := wr.ToBytes;
    PutIntBE(Length(Result), TValueArr(Result), 4); // fix total length
  finally
    wr.Free;
  end;
end;

function TScKey.EncodePublicKeyToIETFFormat: TBytes;
begin
  Result := EncodePublicKeyToOpenSSHFormat;
end;

function TScKey.EncodePublicKeyToOpenSSHFormat: TBytes;
var
  wr: TSSH2DataStream;
begin
  wr := TSSH2DataStream.Create;
  try
    wr.WriteAStr(CipherFactory.PublicKeyAlgorithmToSSH2FullName(FAlgorithm, FECData.ECName));

    case FAlgorithm of
      aaRSA: begin
        wr.WriteAsBigInteger(FRSAData.PublicExponent.GetBytes);
        wr.WriteAsBigInteger(FRSAData.PublicModulus.GetBytes);
      end;

      aaDSA: begin
        wr.WriteAsBigInteger(FDSAData.P.GetBytes);
        wr.WriteAsBigInteger(FDSAData.Q.GetBytes);
        wr.WriteAsBigInteger(FDSAData.G.GetBytes);
        wr.WriteAsBigInteger(FDSAData.Y.GetBytes);
      end;

      aaEC: begin
        Assert(FECData.ECCryptography <> nil);
        if not IsClass(FECData.ECCryptography, TScECCryptographyX25519) then
          wr.WriteAStr(TCipherSuites.ECNameToSshId(FECData.ECName));

        wr.WriteAsString(FECData.ECCryptography.EncodePointToOctetString(FECData.PublicPoint));
      end;
    else
      Assert(False);
    end;

    Result := wr.ToBytes;
  finally
    wr.Free;
  end;
end;

procedure TScKey.WriteOpenSSHPublicKey(Stream: TStream; const KeyData: TBytes; const Comment: string);
var
  str: string;
  buf: TBytes;
begin
  str := CipherFactory.PublicKeyAlgorithmToSSH2FullName(FAlgorithm, FECData.ECName) + ' ';
  Stream.WriteBuffer(Encoding.Default.GetBytes(str)[0], Length(str));

  Assert(Length(KeyData) > 0);
  buf := TBase64.Encode(KeyData);
  Stream.WriteBuffer(buf[0], Length(buf));

  str := ' ' + Comment + #10;
  Stream.WriteBuffer(Encoding.Default.GetBytes(str)[0], Length(str));
end;

// PVK format
const
  PUBLIC_KEY_BLOB = $6;
  PRIVATE_KEY_BLOB = $7;
  ALG_CLASS_SIGNATURE = $2000;
  ALG_CLASS_KEY_EXCHANGE = $A000;
  ALG_TYPE_DSS = $200;
  ALG_TYPE_RSA = $400;
  CALG_RSA_SIGN = (ALG_CLASS_SIGNATURE or ALG_TYPE_RSA); // $2400
  CALG_DSS_SIGN = (ALG_CLASS_SIGNATURE or ALG_TYPE_DSS); // $2200
  CALG_RSA_KEYX = (ALG_CLASS_KEY_EXCHANGE or ALG_TYPE_RSA); // $A400

type
  TPVKKeyHeader = record
    BlobType: byte;
    BlobVersion: byte;
    Reserved: word;
    KeyAlg: cardinal;
  end;

function TScKey.SetSizeAndPositionPVKBlobRSA(BitLen: cardinal): TSizeAndPosBlobRSA;
var
  PksLen, RpkLen, RpkPos: cardinal;
begin
  // size = 8 12 128 64 64 64 64 64 128 = 596
  PksLen := 8;
  RpkLen := 12;

  with Result do begin
    ExponentLen := 4;
    ModulusLen := BitLen shr 3; //128
    Prime1Len := BitLen shr 4; //64  // div 16
    Prime2Len := BitLen shr 4; //64
    Exponent1Len := BitLen shr 4; //64
    Exponent2Len := BitLen shr 4; //64
    CoefficientLen := BitLen shr 4; //64
    PrivateExponentLen := BitLen shr 3; //128

    PrivByteLen := PksLen + RpkLen + ModulusLen + Prime1Len + Prime2Len + Exponent1Len + Exponent2Len + CoefficientLen + PrivateExponentLen; //596
    PubByteLen := PksLen + RpkLen + ModulusLen; //148

    // 1024 = 16 20 148 212 276 340 404 468
    RpkPos := PksLen; //8
    ExponentPos := RpkPos + 8; //16
    ModulusPos := RpkPos + RpkLen; //20
    Prime1Pos := ModulusPos + ModulusLen; //148
    Prime2Pos := Prime1Pos + Prime1Len; //212
    Exponent1Pos := Prime2Pos + Prime2Len; //276
    Exponent2Pos := Exponent1Pos + Exponent1Len; //340
    CoefficientPos := Exponent2Pos + Exponent2Len; //404
    PrivateExponentPos := CoefficientPos + CoefficientLen; //468
  end;
end;

function TScKey.SetSizeAndPositionPVKBlobDSA(BitLen: cardinal): TSizeAndPosBlobDSA;
var
  PksLen, DpkLen, DpkPos: cardinal;
begin
  // size = 8 8 128 20 128 (20 128) 4 20
  PksLen := 8;
  DpkLen := 8;

  with Result do begin
    PLen := BitLen shr 3; //128
    QLen := 20;
    GLen := BitLen shr 3; //128
    XLen := 20;
    YLen := BitLen shr 3; //128
    PgenCounterLen := 4;
    SeedLen := 20;

    PrivByteLen := PksLen + DpkLen + PLen + QLen + GLen + XLen + PgenCounterLen + SeedLen;
    PubByteLen := PksLen + DpkLen + PLen + QLen + GLen + YLen + PgenCounterLen + SeedLen;

    // 1024 = 8 16 144 164 292 292 (312 420) (316 424)
    DpkPos := PksLen; //8
    PPos := DpkPos + DpkLen; //16
    QPos := PPos + PLen; //144
    GPos := QPos + QLen; //164
    XYPos := GPos + GLen; //292

    XPgenCounterPos := XYPos + XLen; //312
    YPgenCounterPos := XYPos + YLen; //420
    XSeedPos := XPgenCounterPos + PgenCounterLen; //316
    YSeedPos := YPgenCounterPos + PgenCounterLen; //424
  end;
end;

procedure TScKey.DecodeKeyFromPVKFormat(const Data: TBytes);

  procedure PVKBlob2RSA(BitLen: integer; IsPrivate: Boolean);
  var
    SizePos: TSizeAndPosBlobRSA;
  begin
    SizePos := SetSizeAndPositionPVKBlobRSA(BitLen);

    // public
    FRSAData.PublicModulus := TBigInteger.Create(TBigInteger.GetBigIntegerLE(Data, SizePos.ModulusPos, SizePos.ModulusLen));
    FRSAData.PublicExponent := TBigInteger.Create(TBigInteger.GetBigIntegerLE(Data, SizePos.ExponentPos, SizePos.ExponentLen));

    // private
    if IsPrivate then begin
      FRSAData.P := TBigInteger.Create(TBigInteger.GetBigIntegerLE(Data, SizePos.Prime1Pos, SizePos.Prime1Len));
      FRSAData.Q := TBigInteger.Create(TBigInteger.GetBigIntegerLE(Data, SizePos.Prime2Pos, SizePos.Prime2Len));
      // FRSAData.DP := TBigInteger.Create(TBigInteger.GetBigIntegerLE(Data, SizePos.Exponent1Pos, SizePos.Exponent1Len));
      // FRSAData.DQ := TBigInteger.Create(TBigInteger.GetBigIntegerLE(Data, SizePos.Exponent2Pos, SizePos.Exponent2Len));
      FRSAData.Qinv := TBigInteger.Create(TBigInteger.GetBigIntegerLE(Data, SizePos.CoefficientPos, SizePos.CoefficientLen));
      FRSAData.D := TBigInteger.Create(TBigInteger.GetBigIntegerLE(Data, SizePos.PrivateExponentPos, SizePos.PrivateExponentLen));
      Calc_DP_DQ;
    end
    else begin
      FreeAndNil(FRSAData.D);
      FreeAndNil(FRSAData.P);
      FreeAndNil(FRSAData.Q);
      FreeAndNil(FRSAData.Qinv);
    end;
  end;

  procedure PVKBlob2DSA(BitLen: integer; IsPrivate: Boolean);
  var
    // Seed, PgenCounter: TBytes;
    SizePos: TSizeAndPosBlobDSA;
  begin
    SizePos := SetSizeAndPositionPVKBlobDSA(BitLen);

    // public
    FDSAData.P := TBigInteger.Create(TBigInteger.GetBigIntegerLE(Data, SizePos.PPos, SizePos.PLen));
    FDSAData.Q := TBigInteger.Create(TBigInteger.GetBigIntegerLE(Data, SizePos.QPos, SizePos.QLen));
    FDSAData.G := TBigInteger.Create(TBigInteger.GetBigIntegerLE(Data, SizePos.GPos, SizePos.GLen));

    // private
    if IsPrivate then begin
      FDSAData.X := TBigInteger.Create(TBigInteger.GetBigIntegerLE(Data, SizePos.XYPos, SizePos.XLen));
      // PgenCounter := TBigInteger.GetBigIntegerLE(Data, SizePos.XPgenCounterPos, SizePos.PgenCounterLen);
      // Seed := TBigInteger.GetBigIntegerLE(Data, SizePos.XSeedPos, SizePos.SeedLen);

      CalcY;
    end
    else begin
      FDSAData.Y := TBigInteger.Create(TBigInteger.GetBigIntegerLE(Data, SizePos.XYPos, SizePos.YLen));
      // PgenCounter := TBigInteger.GetBigIntegerLE(Data, SizePos.YPgenCounterPos, SizePos.PgenCounterLen);
      // Seed := TBigInteger.GetBigIntegerLE(Data, SizePos.YSeedPos, SizePos.SeedLen);
      FreeAndNil(FDSAData.X);
    end;
  end;

var
  pkh: TPVKKeyHeader;
  Magic: cardinal;
  BitLen: cardinal;
begin
  if Length(Data) < 16 then
    RaiseError;

  pkh.BlobType := Data[0]; //7
  pkh.BlobVersion := Data[1]; //2
  pkh.Reserved := BitConverter.ToInt16(Data, 2); //0
  pkh.KeyAlg := BitConverter.ToInt32(Data, 4); // $2400 RSA_SIGN  // $2200 DSS_SIGN

  if (pkh.BlobType <> PUBLIC_KEY_BLOB) and (pkh.BlobType <> PRIVATE_KEY_BLOB) then
    raise EScError.Create(seWrongDataFormat);

  // PRIV DSA. This must always be set to $32535344, the ASCII encoding of DSS2.
  // PUB DSA. This must always be set to $31535344, the ASCII encoding of DSS1.
  // PRIV RSA. This must always be set to $32415352, the ASCII encoding of RSA2.
  // PUB RSA. This must always be set to $31415352, the ASCII encoding of RSA1.
  Magic := BitConverter.ToUInt32(Data, 8);
  BitLen := BitConverter.ToUInt32(Data, 12); //1024 // DSA. Number of bits in the DSS key BLOB's prime, P.

  if (pkh.KeyAlg = CALG_RSA_SIGN) or (pkh.KeyAlg = CALG_RSA_KEYX) then begin
    if ((pkh.BlobType = PUBLIC_KEY_BLOB) and (Magic = $31415352)) or
       ((pkh.BlobType = PRIVATE_KEY_BLOB) and (Magic = $32415352)) then
      FAlgorithm := aaRSA
    else
      RaiseError;
  end
  else
  if pkh.KeyAlg = CALG_DSS_SIGN then begin
    if ((pkh.BlobType = PUBLIC_KEY_BLOB) and (Magic = $31535344)) or
       ((pkh.BlobType = PRIVATE_KEY_BLOB) and (Magic = $32535344)) then
      FAlgorithm := aaDSA
    else
      RaiseError;
  end
  else
    raise EScError.Create(seWrongDataFormat);

  case FAlgorithm of
    aaRSA:
      PVKBlob2RSA(BitLen, pkh.BlobType = PRIVATE_KEY_BLOB);
    aaDSA:
      PVKBlob2DSA(BitLen, pkh.BlobType = PRIVATE_KEY_BLOB);
    aaEC:
      raise EScError.Create(seWrongDataFormat);
  else
    Assert(False);
  end;

  FReady := True;
end;

{$IFDEF MSWINDOWS}
procedure FillBytes(var Target: TBytes; Offset, Count: integer; const Data: TBytes);
begin
  Buffer.BlockCopy(Data, 0, Target, Offset, Length(Data));
  if Count > Length(Data) then
    FillChar(Target[Offset + Length(Data)], Count - Length(Data), 0);
end;

function TScKey.EncodeRSAKeyToPVKFormat: TBytes;
var
  rsaMagic: TBytes;
  BitLen: cardinal;
  caSize: integer;
  kb: Byte;
  SizePos: TSizeAndPosBlobRSA;
begin
  if not Ready then
    raise EScError.Create(seKeyNotReady);

  BitLen := GetBitCount;
  SizePos := SetSizeAndPositionPVKBlobRSA(BitLen);

  if IsPrivate then begin
    kb := PRIVATE_KEY_BLOB;
    caSize := 20 + (9 * (BitLen shr 4)); // div 16
    rsaMagic := Encoding.Default.GetBytes('RSA2');
  end
  else begin
    kb := PUBLIC_KEY_BLOB;
    caSize := 20 + (BitLen shr 3); // div 8
    rsaMagic := Encoding.Default.GetBytes('RSA1');
  end;
  SetLength(Result, caSize);

  // TPVKKeyHeader
  Result[0] := kb;// BlobType
  Result[1] := 2; // BlobVersion
  Result[2] := 0; // Reserved
  Result[3] := 0; // Reserved
  PutIntLE(CALG_RSA_KEYX, Result, 4); // KeyAlg

  // RSAPUBKEY
  Buffer.BlockCopy(rsaMagic, 0, Result, 8, 4);
  PutIntLE(BitLen, Result, 12);

  if Length(FRSAData.PublicExponent.GetBytes) > 4 then
    raise EScError.Create(seLargeExpLength);

  // public
  FillBytes(Result, SizePos.ModulusPos, SizePos.ModulusLen, FRSAData.PublicModulus.GetBytesLE);
  FillBytes(Result, SizePos.ExponentPos, SizePos.ExponentLen, FRSAData.PublicExponent.GetBytesLE);

  // private
  if IsPrivate then begin
    FillBytes(Result, SizePos.Prime1Pos, SizePos.Prime1Len, FRSAData.P.GetBytesLE);
    FillBytes(Result, SizePos.Prime2Pos, SizePos.Prime2Len, FRSAData.Q.GetBytesLE);
    FillBytes(Result, SizePos.Exponent1Pos, SizePos.Exponent1Len, FRSAData.DP.GetBytesLE);
    FillBytes(Result, SizePos.Exponent2Pos, SizePos.Exponent2Len, FRSAData.DQ.GetBytesLE);
    FillBytes(Result, SizePos.CoefficientPos, SizePos.CoefficientLen, FRSAData.Qinv.GetBytesLE);
    FillBytes(Result, SizePos.PrivateExponentPos, SizePos.PrivateExponentLen, FRSAData.D.GetBytesLE);
  end;
end;

function TScKey.EncodeDSAKeyToPVKFormat: TBytes;
var
  dsaMagic: TBytes;
  BitLen: cardinal;
  caSize: integer;
  kb: Byte;
  SizePos: TSizeAndPosBlobDSA;
begin
  if not Ready then
    raise EScError.Create(seKeyNotReady);

  BitLen := GetBitCount;
  SizePos := SetSizeAndPositionPVKBlobDSA(BitLen);

  if IsPrivate then begin
    kb := PRIVATE_KEY_BLOB;
    caSize := SizePos.PrivByteLen;
    dsaMagic := Encoding.Default.GetBytes('DSS2');
  end
  else begin
    kb := PUBLIC_KEY_BLOB;
    caSize := SizePos.PubByteLen;
    dsaMagic := Encoding.Default.GetBytes('DSS1');
  end;
  SetLength(Result, caSize);

  // TPVKKeyHeader
  Result[0] := kb;// BlobType
  Result[1] := 2; // BlobVersion
  Result[2] := 0; // Reserved
  Result[3] := 0; // Reserved
  PutIntLE(CALG_DSS_SIGN, Result, 4); // KeyAlg

  // DSSPUBKEY
  Buffer.BlockCopy(dsaMagic, 0, Result, 8, 4);
  PutIntLE(BitLen, Result, 12);

  FillBytes(Result, SizePos.PPos, SizePos.PLen, FDSAData.P.GetBytesLE);
  FillBytes(Result, SizePos.QPos, SizePos.QLen, FDSAData.Q.GetBytesLE);
  FillBytes(Result, SizePos.GPos, SizePos.GLen, FDSAData.G.GetBytesLE);

  if IsPrivate then begin
    FillBytes(Result, SizePos.XYPos, SizePos.XLen, FDSAData.X.GetBytesLE);
    PutIntLE($FFFFFFFF, Result, SizePos.XPgenCounterPos);
    FillChar(Result[SizePos.XSeedPos], SizePos.SeedLen, 0);
  end
  else begin // public
    FillBytes(Result, SizePos.XYPos, SizePos.YLen, FDSAData.Y.GetBytesLE);
    PutIntLE($FFFFFFFF, Result, SizePos.YPgenCounterPos);
    FillChar(Result[SizePos.YSeedPos], SizePos.SeedLen, 0);
  end;
end;
{$ENDIF MSWINDOWS}

procedure TScKey.ExportTo(const FileName: string; const PublicKeyOnly: boolean; const Password: string;
  const Algorithm: TScSymmetricAlgorithm = saTripleDES_cbc; const KeyFormat: TScKeyFormat = kfDefault; const Comment: string = '');
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    ExportTo(Stream, PublicKeyOnly, Password, Algorithm, KeyFormat, Comment);
  finally
    Stream.Free;
  end;
end;

procedure TScKey.ExportTo(Stream: TStream; const PublicKeyOnly: boolean; const Password: string;
  const Algorithm: TScSymmetricAlgorithm = saTripleDES_cbc; const KeyFormat: TScKeyFormat = kfDefault; const Comment: string = '');
var
  BegStr, EndStr: string;
  KeyData: TBytes;
  LineLen: integer;
begin
  if not Ready then
    raise EScError.Create(seKeyNotReady);

  if PublicKeyOnly then begin
    if not (KeyFormat in [kfDefault, kfIETF, kfPKCS1]) then
      raise EScError.Create(seNotCompatibleFormatWithPublicKeyOnly);

    if Password <> '' then
      raise EScError.Create(seNotCompatibleFormatWithPassphrase);
  end
  else
    if not IsPrivate then
      raise EScError.Create(seKeyNotPrivate);

  if (Password <> '') and (KeyFormat in [kfDER, kfPKCS8]) then
    raise EScError.Create(seNotCompatibleFormatWithPassphrase);

  if (Comment <> '') and (KeyFormat <> kfIETF) and not PublicKeyOnly then
    raise EScError.Create(seNotCompatibleFormatWithComment);

  SetLength(KeyData, 0);
  case KeyFormat of
    kfDefault, kfPKCS1: begin
      if PublicKeyOnly then begin
        if KeyFormat = kfPKCS1 then begin
          case FAlgorithm of
            aaRSA: begin
              BegStr := PKCS1_RSA_PUB_HEADER;
              EndStr := PKCS1_RSA_PUB_FOOTER;
            end;
            aaDSA: begin
              BegStr := PKCS1_DSA_PUB_HEADER;
              EndStr := PKCS1_DSA_PUB_FOOTER;
            end;
            aaEC: begin
              BegStr := PKCS1_PUB_HEADER;
              EndStr := PKCS1_PUB_FOOTER;
            end;
          else
            Assert(False);
          end;

          KeyData := EncodeKeyToPKCS1Format(True);
        end
        else begin
          KeyData := EncodePublicKeyToOpenSSHFormat;
          WriteOpenSSHPublicKey(Stream, KeyData, Comment);
          Exit;
        end;
      end
      else begin
        case FAlgorithm of
          aaRSA: begin
            BegStr := PKCS1_RSA_PRIV_HEADER;
            EndStr := PKCS1_RSA_PRIV_FOOTER;
          end;
          aaDSA: begin
            BegStr := PKCS1_DSA_PRIV_HEADER;
            EndStr := PKCS1_DSA_PRIV_FOOTER;
          end;
          aaEC: begin
            BegStr := EC_PRIV_HEADER;
            EndStr := EC_PRIV_FOOTER;
          end;
        else
          Assert(False);
        end;

        KeyData := EncodeKeyToPKCS1Format(False);
      end;
    end;

    kfDER, kfPKCS8: begin
      BegStr := PKCS8_HEADER;
      EndStr := PKCS8_FOOTER;
      KeyData := EncodePrivateKeyToPKCS8Format;
    end;

    kfPKCS8enc: begin
      BegStr := PKCS8_ENC_HEADER;
      EndStr := PKCS8_ENC_FOOTER;
      KeyData := EncodePrivateKeyToPKCS8EncFormat(Password, Algorithm);
    end;

    kfIETF: begin
      if PublicKeyOnly then begin
        BegStr := IETF_PUB_HEADER;
        EndStr := IETF_PUB_FOOTER;
      end
      else begin
        if FAlgorithm = aaEC then
          raise EScError.Create(seNotCompatibleFormatWithECKey);

        BegStr := IETF_PRIV_HEADER;
        EndStr := IETF_PRIV_FOOTER;
      end;

      if PublicKeyOnly then
        KeyData := EncodePublicKeyToIETFFormat
      else begin
        if (Password <> '') and (Algorithm <> saTripleDES_cbc) then
          raise EScError.Create(seCipherNotSupported);
        KeyData := EncodePrivateKeyToIETFFormat(Password);
      end;
    end;
  else
    Assert(False);
  end;

  Assert(Length(KeyData) > 0);
  if KeyFormat = kfDER then begin
    Stream.WriteBuffer(KeyData[0], Length(KeyData));
  end
  else begin
    TStreamUtils.WriteLine(Stream, BegStr);

    if (KeyFormat in [kfDefault, kfPKCS1]) and not PublicKeyOnly and (Password <> '') then
      CryptPrivatePEMKey(Stream, Password, Algorithm, KeyData); // result is written in the Stream

    if (KeyFormat = kfIETF) and not PublicKeyOnly then
      LineLen := 70
    else
      LineLen := PEM_LINE_LENGTH;

    if Comment <> '' then
      WriteBlockInPEMStyle(Stream, Encoding.Default.GetBytes('Comment: ' + Comment), LineLen, True);

    WriteBlockInPEMStyle(Stream, TBase64.Encode(KeyData), LineLen, False);
    TStreamUtils.WriteLine(Stream, EndStr);
  end;
end;

procedure TScKey.LoadKeyFromPVKFormat(const Data: TBytes);
begin
  Ready := False;
  DecodeKeyFromPVKFormat(Data);
end;

procedure TScKey.LoadKeyFromCryptoAPIFormat(const Data: TBytes;
  const KeyExFormat: TScKeyExFormat; const Alg: TScAsymmetricAlgorithm; const IsPublicKey: boolean);
begin
  Ready := False;

  case KeyExFormat of
    kefPVK:
      DecodeKeyFromPVKFormat(Data);
    kefDefault:
      DecodeKeyFromPKCS1Format(Data, Alg, IsPublicKey);
    kefPKCS8:
      DecodePrivateKeyFromPKCS8Format(Data);
  {$IFNDEF MSWINDOWS}
    kefOpenSSL:
      DecodeKeyFromOpenSSLFormat(Data, IsPublicKey);
  {$ENDIF}
  else
    raise EScError.Create(seWrongDataFormat);
  end;

  CheckReady;
end;

function TScKey.SaveKeyToCryptoAPIFormat: TBytes;
begin
  if not Ready then
    raise EScError.Create(seKeyNotReady);

  SetLength(Result, 0);

{$IFDEF MSWINDOWS}
  case FAlgorithm of
    aaRSA:
      Result := EncodeRSAKeyToPVKFormat;
    aaDSA:
      Result := EncodeDSAKeyToPVKFormat;
    aaEC:
      raise EScError.Create(seNotCompatibleFormatWithECKey);
  else
    Assert(False);
  end;
{$ELSE}
  Result := EncodeKeyToPKCS1Format(not IsPrivate);
{$ENDIF}
end;

class function TScKey.MGF1(const Seed: TBytes; MaskLen: integer; HashAlg: TScHashAlgorithm): TBytes;
var
  csp: THashAlgorithm;
  ResLen, HashLen, SeedLen, Counter: integer;
  Buf, Hash: TBytes;
begin
  Result := nil;

  csp := CipherFactory.CreateHash(HashAlg);
  try
    SeedLen := Length(Seed);
    SetLength(Buf, SeedLen + 4);
    Move(Seed[0], Buf[0], SeedLen);
    SetLength(Hash, 0);
    HashLen := csp.HashSize;

    ResLen := 0;
    Counter := 0;
    while ResLen < MaskLen do begin
      PutIntBE(Counter, TValueArr(Buf), SeedLen);
      Hash := csp.ComputeHash(Buf);
      if Result = nil then
        Result := Hash
      else begin
        SetLength(Result, ResLen + HashLen);
        Move(Hash[0], Result[ResLen], HashLen);
      end;
      Inc(ResLen, HashLen);
      Inc(Counter);
    end;

    SetLength(Result, MaskLen);
  finally
    csp.Free;
  end;
end;

function TScKey.EncodeWithPad(const Input: TBytes; Padding: TScPaddingMode): TBytes;
var
  MLen, PaddingLen: integer;
  KeySize, HashLen, DBLen: integer;
  EmptyStrHash: TBytes;
  MaskedSeed, MaskedDB: TBytes;
  SeedMask, DBMask, Seed, DB: TBytes;
  i: integer;
begin
  if Random = nil then
    raise Exception.Create(SInternalError);

  KeySize := GetKeySize;

  case Padding of
    pmPKCS2: begin
      MLen := Length(Input);
      PaddingLen := KeySize - MLen;
      if PaddingLen < 11 then
        raise EScError.CreateFmt(SErrorEncryptingData, [SMessageTooLong], seErrorEncryptingData);

      SetLength(Result, KeySize);
      Result[0] := 0;
      Result[1] := 2;
      Result[PaddingLen - 1] := 0;
      Move(Input[0], Result[PaddingLen], MLen);

      Random.Random(@Result[2], PaddingLen - 3);
      for i := 2 to PaddingLen - 2 do
        while Result[i] = 0 do
          Random.Random(@Result[i], 1);
    end;

    pmOAEP: begin
      MLen := Length(Input);
      PaddingLen := KeySize - MLen;
      if PaddingLen < (2 + 2 * CipherFactory.GetHashSize(FOAEPParams.HashAlgorithm)) then
        raise EScError.CreateFmt(SErrorEncryptingData, [SMessageTooLong], seErrorEncryptingData);

      SetLength(EmptyStrHash, 0);
      EmptyStrHash := FOAEPParams.GetEmptyStrHash;
      HashLen := Length(EmptyStrHash);

      DBLen := KeySize - HashLen - 1;
      SetLength(DB, DBLen);
      Move(EmptyStrHash[0], DB[0], HashLen);
      FillChar(DB[HashLen], DBLen - MLen - HashLen - 1, 0);
      DB[DBLen - MLen - 1] := 1;
      Move(Input[0], DB[DBLen - MLen], MLen);

      SetLength(Seed, HashLen);
      Random.Random(@Seed[0], HashLen);

      SetLength(DBMask, 0);
      DBMask := MGF1(Seed, DBLen, FOAEPParams.MaskGenHashAlgorithm);

      SetLength(MaskedDB, DBLen);
      for i := 0 to DBLen - 1 do
        MaskedDB[i] := DB[i] xor DBMask[i];

      SetLength(SeedMask, 0);
      SeedMask := MGF1(MaskedDB, HashLen, FOAEPParams.MaskGenHashAlgorithm);

      SetLength(MaskedSeed, HashLen);
      for i := 0 to HashLen - 1 do
        MaskedSeed[i] := Seed[i] xor SeedMask[i];

      SetLength(Result, KeySize);
      Result[0] := 0;
      Move(MaskedSeed[0], Result[1], HashLen);
      Move(MaskedDB[0], Result[1 + HashLen], DBLen);
    end;

    pmNone: begin
      MLen := Length(Input);
      if MLen <> KeySize then
        raise EScError.CreateFmt(SErrorEncryptingData, [SInvalidLength], seErrorEncryptingData);

      Result := Input;
    end;
  else
    raise EScError.Create(seInvalidInputArgs);
  end;
end;

function TScKey.EncodeHashWithPad(const Hash: TBytes; HashAlg: TScHashAlgorithm; Padding: TScPaddingMode): TBytes;
var
  MLen, PaddingLen: integer;
  KeySize, HashLen, DBLen: integer;
  MaxBits: integer;
  csp: THashAlgorithm;
  AlgAsnIdLen: integer;
  AlgAsnId: PByteArray;
  M, SeedHash: TBytes;
  MaskedDB: TBytes;
  DBMask, Seed, DB: TBytes;
  Mask: byte;
  i: integer;
begin
  if Random = nil then
    raise Exception.Create(SInternalError);

  KeySize := GetKeySize;
  HashLen := Length(Hash);

  case Padding of
    pmPKCS1: begin
      if HashAlg = haNone then
        M := Hash
      else begin
        GetAlgAsnId(HashAlg, AlgAsnId, AlgAsnIdLen);
        SetLength(M, AlgAsnIdLen + HashLen);
        if AlgAsnId <> nil then
          Move(AlgAsnId^, M[0], AlgAsnIdLen);
        Move(Hash[0], M[AlgAsnIdLen], HashLen);
      end;

      MLen := Length(M);
      PaddingLen := KeySize - MLen;
      if PaddingLen < 11 then
        raise EScError.CreateFmt(SErrorEncryptingData, [SMessageTooLong], seErrorEncryptingData);

      SetLength(Result, KeySize);
      Result[0] := 0;
      Result[1] := 1;
      Result[PaddingLen - 1] := 0;
      FillChar(Result[2], PaddingLen - 3, $FF);
      Move(M[0], Result[PaddingLen], MLen);
    end;

    pmPSS: begin
      MaxBits := FRSAData.PublicModulus.BitCount - 1;
      if KeySize < HashLen + FPSSParams.SaltLength + 2 then
        raise EScError.CreateFmt(SErrorEncryptingData, [SInvalidSaltLength], seErrorEncryptingData);

      if FPSSParams.SaltLength > 0 then begin
        SetLength(Seed, FPSSParams.SaltLength);
        Random.Random(@Seed[0], FPSSParams.SaltLength);
      end;

      SetLength(M, HashLen + FPSSParams.SaltLength + 8);
      FillChar(M[0], 8, 0);
      Move(Hash[0], M[8], HashLen);
      if FPSSParams.SaltLength > 0 then
        Move(Seed[0], M[8 + HashLen], FPSSParams.SaltLength);

      SetLength(SeedHash, 0);
      csp := CipherFactory.CreateHash(HashAlg);
      try
        SeedHash := csp.ComputeHash(M);
      finally
        csp.Free;
      end;

      PaddingLen := KeySize - FPSSParams.SaltLength - HashLen - 2;
      DBLen := PaddingLen + FPSSParams.SaltLength + 1;
      SetLength(DB, DBLen);
      FillChar(DB[0], PaddingLen, 0);
      DB[PaddingLen] := 1;
      if FPSSParams.SaltLength > 0 then
        Move(Seed[0], DB[PaddingLen + 1], FPSSParams.SaltLength);

      SetLength(DBMask, 0);
      DBMask := MGF1(SeedHash, DBLen, FPSSParams.MaskGenHashAlgorithm);

      SetLength(MaskedDB, DBLen);
      for i := 0 to DBLen - 1 do
        MaskedDB[i] := DB[i] xor DBMask[i];

      for i := 0 to (KeySize * 8 - MaxBits) - 1 do begin
        Mask := $7F shr i;
        MaskedDB[0] := MaskedDB[0] and Mask;
      end;

      SetLength(Result, KeySize);
      Move(MaskedDB[0], Result[0], DBLen);
      Move(SeedHash[0], Result[DBLen], HashLen);
      Result[KeySize - 1] := $bc;
    end;
  else
    raise EScError.Create(seInvalidInputArgs);
  end;
end;

function TScKey.VerifyHashWithPad(const Hash, Sign: TBytes; HashAlg: TScHashAlgorithm; Padding: TScPaddingMode): boolean;
var
  SignLen: integer;
  csp: THashAlgorithm;
  AlgAsnIdLen: integer;
  AlgAsnId: PByteArray;
  M, SignHash, SeedHash: TBytes;
  MaskedDB: TBytes;
  DBMask, DB: TBytes;
  HashLen, DBLen, PaddingLen: integer;
  MaxBits: integer;
  Mask: byte;
  i: integer;
begin
  Result := False;
  HashLen := Length(Hash);
  SignLen := Length(Sign);

  case Padding of
    pmPKCS1: begin
      if (SignLen <= 11) or (Sign[0] <> 0) or (Sign[1] <> 1) then
        raise EScError.Create(seIPCorruptData);

      i := 2;
      while i < SignLen do begin
        if Sign[i] = 0 then
          break;
        if Sign[i] <> $FF then
          raise EScError.Create(seIPCorruptData);

        Inc(i);
      end;

      if i = SignLen then
        raise EScError.Create(seIPCorruptData);
      Inc(i);

      GetAlgAsnId(HashAlg, AlgAsnId, AlgAsnIdLen);

      if (SignLen - i) <> (AlgAsnIdLen + HashLen) then
        Exit
      else begin
        if AlgAsnId <> nil then
          if MemCompare(AlgAsnId, @Sign[i], AlgAsnIdLen) <> 0 then
            Exit;

        if MemCompare(@Hash[0], @Sign[i + AlgAsnIdLen], HashLen) <> 0 then
          Exit;
      end;

      Result := True;
    end;

    pmPSS: begin
      MaxBits := FRSAData.PublicModulus.BitCount - 1;
      if SignLen < HashLen + FPSSParams.SaltLength + 2 then
        raise EScError.Create(seIPCorruptData);

      if Sign[SignLen - 1] <> $bc then
        Exit;

      DBLen := SignLen - HashLen - 1;
      SetLength(MaskedDB, DBLen);
      Move(Sign[0], MaskedDB[0], DBLen);
      SetLength(SignHash, HashLen);
      Move(Sign[DBLen], SignHash[0], HashLen);

      for i := 0 to (SignLen * 8 - MaxBits) - 1 do begin
        Mask := 128 shr i;
        if (MaskedDB[0] and Mask) <> 0 then
          Exit;
      end;

      SetLength(DBMask, 0);
      DBMask := MGF1(SignHash, DBLen, FPSSParams.MaskGenHashAlgorithm);
      SetLength(DB, DBLen);
      for i := 0 to DBLen - 1 do
        DB[i] := MaskedDB[i] xor DBMask[i];

      for i := 0 to (SignLen * 8 - MaxBits) - 1 do begin
        Mask := $7F shr i;
        DB[0] := DB[0] and Mask;
      end;

      PaddingLen := SignLen - HashLen - FPSSParams.SaltLength - 2;
      for i := 0 to PaddingLen - 1 do begin
        if DB[i] <> 0 then
          Exit;
      end;

      if DB[PaddingLen] <> 1 then
        Exit;

      SetLength(M, HashLen + FPSSParams.SaltLength + 8);
      FillChar(M[0], 8, 0);
      Move(Hash[0], M[8], HashLen);
      if FPSSParams.SaltLength > 0 then
        Move(DB[PaddingLen + 1], M[8 + HashLen], FPSSParams.SaltLength);

      SetLength(SeedHash, 0);
      csp := CipherFactory.CreateHash(HashAlg);
      try
        SeedHash := csp.ComputeHash(M);
      finally
        csp.Free;
      end;

      if MemCompare(@SignHash[0], @SeedHash[0], HashLen) <> 0 then
        Exit;

      Result := True;
    end;

    pmNone: begin
      Result := (Length(Hash) = SignLen) and (MemCompare(@Hash[0], @Sign[0], SignLen) = 0);
    end;
  else
    raise EScError.Create(seInvalidInputArgs);
  end;
end;

function TScKey.DecodePad(const Input: TBytes; Padding: TScPaddingMode): TBytes;
var
  InputLen: integer;
  EmptyStrHash: TBytes;
  MaskedSeed, MaskedDB: TBytes;
  SeedMask, DBMask, Seed, DB: TBytes;
  HashLen, DBLen: integer;
  i: integer;
begin
  InputLen := Length(Input);

  case Padding of
    pmPKCS2: begin
      if (InputLen <= 11) or (Input[0] <> 0) or (Input[1] <> 2) then
        raise EScError.Create(seIPCorruptData);

      i := 2;
      while i < InputLen do begin
        if Input[i] = 0 then
          break;

        Inc(i);
      end;

      if i = InputLen then
        raise EScError.Create(seIPCorruptData);
      Inc(i);

      SetLength(Result, InputLen - i);
      Move(Input[i], Result[0], Length(Result));
    end;

    pmOAEP: begin
      if (InputLen <= (2 + 2 * CipherFactory.GetHashSize(FOAEPParams.HashAlgorithm))) or
        (Input[0] <> 0) then
        raise EScError.Create(seIPCorruptData);

      SetLength(EmptyStrHash, 0);
      EmptyStrHash := FOAEPParams.GetEmptyStrHash;
      HashLen := Length(EmptyStrHash);

      DBLen := InputLen - HashLen - 1;
      SetLength(MaskedSeed, HashLen);
      Move(Input[1], MaskedSeed[0], HashLen);
      SetLength(MaskedDB, DBLen);
      Move(Input[1 + Length(MaskedSeed)], MaskedDB[0], DBLen);

      SetLength(SeedMask, 0);
      SeedMask := MGF1(MaskedDB, HashLen, FOAEPParams.MaskGenHashAlgorithm);
      SetLength(Seed, HashLen);
      for i := 0 to HashLen - 1 do
        Seed[i] := MaskedSeed[i] xor SeedMask[i];

      SetLength(DBMask, 0);
      DBMask := MGF1(Seed, DBLen, FOAEPParams.MaskGenHashAlgorithm);
      SetLength(DB, DBLen);
      for i := 0 to DBLen - 1 do
        DB[i] := MaskedDB[i] xor DBMask[i];

      if (HashLen >= DBLen) or (MemCompare(@EmptyStrHash[0], @DB[0], HashLen) <> 0) then
        raise EScError.Create(seIPCorruptData);

      i := HashLen;
      while i < DBLen do begin
        if DB[i] = 1 then
          break;

        if DB[i] <> 0 then
          raise EScError.Create(seIPCorruptData);

        Inc(i);
      end;

      if i = DBLen then
        raise EScError.Create(seIPCorruptData);
      Inc(i);

      SetLength(Result, DBLen - i);
      Move(DB[i], Result[0], Length(Result));
    end;

    pmNone: begin
      Result := Input;
    end;
  else
    raise EScError.Create(seInvalidInputArgs);
  end;
end;

function TScKey.RSAPrivateModPow(const Data: TBytes): TBytes;
var
  Input, m1, m2, h, Res, temp: TBigInteger;
begin
  Res := nil;
  m1 := nil;
  m2 := nil;
  h := nil;

  Input := TBigInteger.Create(Data);
  try
    m1 := Input.ModPow(FRSAData.DP, FRSAData.P);
    m2 := Input.ModPow(FRSAData.DQ, FRSAData.Q);
    h := m1.Minus(m2);

    if h.IsNegative then begin
      temp := h;
      h := h.Add(FRSAData.P);
      temp.Free;
    end;

    Res := FRSAData.Qinv.Mul(h);

    temp := Res;
    Res := Res.Mod_(FRSAData.P);
    temp.Free;

    temp := Res;
    Res := Res.Mul(FRSAData.Q);
    temp.Free;

    temp := Res;
    Res := Res.Add(m2);
    temp.Free;

    Result := Res.GetBytes(GetKeySize);
  finally
    Input.Free;
    Res.Free;
    m1.Free;
    m2.Free;
    h.Free;
  end;
end;

function TScKey.RSAPublicModPow(const Data: TBytes): TBytes;
var
  Input, Res: TBigInteger;
begin
  Res := nil;
  Input := TBigInteger.Create(Data);
  try
    Res := Input.ModPow(FRSAData.PublicExponent, FRSAData.PublicModulus);
    Result := Res.GetBytes(GetKeySize);
  finally
    Input.Free;
    Res.Free;
  end;
end;

procedure TScKey.GetAlgAsnId(const HashAlg: TScHashAlgorithm; out AsnId: PByteArray; out AsnIdLen: integer);
begin
  case HashAlg of
    haNone: begin
      AsnIdLen := 0;
      AsnId := nil;
    end;
    haMD2: begin
      AsnIdLen := Length(MD2_ASN_ID);
      AsnId := @MD2_ASN_ID;
    end;
    haMD4: begin
      AsnIdLen := Length(MD4_ASN_ID);
      AsnId := @MD4_ASN_ID;
    end;
    haMD5: begin
      AsnIdLen := Length(MD5_ASN_ID);
      AsnId := @MD5_ASN_ID;
    end;
    haSHA1: begin
      AsnIdLen := Length(SHA1_ASN_ID);
      AsnId := @SHA1_ASN_ID;
    end;
    haSHA2_256: begin
      AsnIdLen := Length(SHA256_ASN_ID);
      AsnId := @SHA256_ASN_ID;
    end;
    haSHA2_512: begin
      AsnIdLen := Length(SHA512_ASN_ID);
      AsnId := @SHA512_ASN_ID;
    end;
    haSHA2_224: begin
      AsnIdLen := Length(SHA224_ASN_ID);
      AsnId := @SHA224_ASN_ID;
    end;
    haSHA2_384: begin
      AsnIdLen := Length(SHA384_ASN_ID);
      AsnId := @SHA384_ASN_ID;
    end;
  else
    raise EScError.Create(seInvalidHashAlgorithm);
  end;
end;

function TScKey.DSASignHash(const Hash: TBytes): TBytes;
var
  Input, r, s, tmp, temp: TBigInteger;
  br, bs: TBytes;
begin
  r := nil;
  s := nil;
  tmp := nil;
  Input := TBigInteger.Create(Hash);
  try
    r := FDSAData.Y.Mod_(FDSAData.Q); // r = (g^k mod p) mod q

    tmp := FDSAData.X.Mul(r);
    temp := tmp;
    tmp := Input.Add(tmp);
    temp.Free;
    s := FDSAData.X.ModInverse(FDSAData.Q);
    temp := s;
    s := s.Mul(tmp);
    temp.Free;
    temp := s;
    s := s.Mod_(FDSAData.Q); // s = k^-1 * (Hash + x*r) mod q
    temp.Free;

    br := r.GetBytes;
    bs := s.GetBytes;
    SetLength(Result, Length(Hash) * 2);
    Move(br[0], Result[Length(Hash) - Length(br)], Length(br));
    Move(bs[0], Result[Length(Hash) * 2 - Length(bs)], Length(bs));
  finally
    Input.Free;
    r.Free;
    s.Free;
    tmp.Free;
  end;
end;

function TScKey.DSAVerifyHash(const Hash, Sign: TBytes): boolean;
var
  Input, r, s, w, u1, u2, v, temp: TBigInteger;
  First, Second: TBytes;
begin
  r := nil;
  s := nil;
  w := nil;
  u1 := nil;
  u2 := nil;
  v := nil;
  Input := TBigInteger.Create(Hash);
  try
    SetLength(First, Length(Sign) div 2);
    SetLength(Second, Length(Sign) div 2);
    Move(Sign[0], First[0], Length(First));
    Move(Sign[Length(First)], Second[0], Length(Second));
    r := TBigInteger.Create(First);
    s := TBigInteger.Create(Second);

    w := s.ModInverse(FDSAData.Q);
    u1 := Input.Mul(w);
    temp := u1;
    u1 := u1.Mod_(FDSAData.Q);
    temp.Free;
    u2 := r.Mul(w);
    temp := u2;
    u2 := u2.Mod_(FDSAData.Q);
    temp.Free;
    temp := u1;
    u1 := FDSAData.G.modPow(u1, FDSAData.P);
    temp.Free;
    temp := u2;
    u2 := FDSAData.Y.modPow(u2, FDSAData.P);
    temp.Free;
    v := u1.Mul(u2);
    temp := v;
    v := v.Mod_(FDSAData.P);
    temp.Free;
    temp := v;
    v := v.Mod_(FDSAData.Q);
    temp.Free;

    Result := v.Equal(r);
  finally
    Input.Free;
    r.Free;
    s.Free;
    w.Free;
    u1.Free;
    u2.Free;
    v.Free;
  end;
end;

function TScKey.RSASignHash(const Hash: TBytes; HashAlg: TScHashAlgorithm; Padding: TScPaddingMode): TBytes;
var
  PaddedData: TBytes;
begin
  if not (Padding in [pmPKCS1, pmPSS]) then
    raise EScError.Create(seImproperPaddingMode);

  PaddedData := EncodeHashWithPad(Hash, HashAlg, Padding);
  Result := RSAPrivateModPow(PaddedData);

  // ! Factoring RSA Keys !
  // https://tools.ietf.org/html/rfc8446#appendix-C.3
  // https://pdfs.semanticscholar.org/bb97/42830fb24de49f6ae6a4d84d44270a7458f8.pdf
  if not RSAVerifyHash(Hash, Result, HashAlg, Padding) then
    raise EScError.Create(seErrorSigningData);
end;

function TScKey.RSAVerifyHash(const Hash, Sign: TBytes; HashAlg: TScHashAlgorithm; Padding: TScPaddingMode): boolean;
var
  vSign: TBytes;
begin
  if not (Padding in [pmPKCS1, pmPSS]) then
    raise EScError.Create(seImproperPaddingMode);

  vSign := RSAPublicModPow(Sign);
  try
    Result := VerifyHashWithPad(Hash, vSign, HashAlg, Padding);
  except
    Result := False;
  end;
end;

function TScKey.ECSignHash(const Hash: TBytes): TBytes;
begin
  Assert(FECData.ECCryptography <> nil);
  Result := FECData.ECCryptography.SignHash(Hash, FECData.PrivateKoef, FECData.PublicPoint);
end;

function TScKey.ECVerifyHash(const Hash, Sign: TBytes): boolean;
begin
  Assert(FECData.ECCryptography <> nil);
  Result := FECData.ECCryptography.VerifyHash(Hash, Sign, FECData.PublicPoint);
end;

function TScKey.Sign(const Data: TBytes; HashAlg: TScHashAlgorithm = haSHA1; Padding: TScPaddingMode = pmPKCS1): TBytes;
var
  csp: THashAlgorithm;
  Hash: TBytes;
begin
  if HashAlg = haNone then
    Hash := Data
  else begin
    csp := CipherFactory.CreateHash(HashAlg);
    try
      Hash := csp.ComputeHash(Data);
    finally
      csp.Free;
    end;
  end;

  Result := SignHash(Hash, HashAlg, Padding);
end;

function TScKey.SignHash(const Hash: TBytes; HashAlg: TScHashAlgorithm = haSHA1; Padding: TScPaddingMode = pmPKCS1): TBytes;
begin
  CheckAvailable;
  if not IsPrivate then
    raise EScError.Create(seCannotSignData);

  lock(FLock);
  try
    case FAlgorithm of
      aaRSA:
        Result := RSASignHash(Hash, HashAlg, Padding);
      aaDSA:
        Result := DSASignHash(Hash);
      aaEC:
        Result := ECSignHash(Hash);
    else
      Assert(False);
    end;
  finally
    unlock(FLock);
  end;
end;

function TScKey.VerifySign(const Data, Sign: TBytes; HashAlg: TScHashAlgorithm = haSHA1; Padding: TScPaddingMode = pmPKCS1): boolean;
var
  csp: THashAlgorithm;
  Hash: TBytes;
begin
  if HashAlg = haNone then
    Hash := Data
  else begin
    csp := CipherFactory.CreateHash(HashAlg);
    try
      Hash := csp.ComputeHash(Data);
    finally
      csp.Free;
    end;
  end;

  Result := VerifyHashSign(Hash, Sign, HashAlg, Padding);
end;

function TScKey.VerifyHashSign(const Hash, Sign: TBytes; HashAlg: TScHashAlgorithm = haSHA1; Padding: TScPaddingMode = pmPKCS1): boolean;
begin
  CheckAvailable;

  lock(FLock);
  try
    case FAlgorithm of
      aaRSA:
        Result := RSAVerifyHash(Hash, Sign, HashAlg, Padding);
      aaDSA:
        Result := DSAVerifyHash(Hash, Sign);
      aaEC:
        Result := ECVerifyHash(Hash, Sign);
    else
      Result := False;
      Assert(False);
    end;
  finally
    unlock(FLock);
  end;
end;

function TScKey.Encrypt(const Data: TBytes; Padding: TScPaddingMode = pmPKCS2): TBytes;
var
  PaddedData: TBytes;
begin
  CheckAvailable;

  if FAlgorithm = aaDSA then
    raise EScError.Create(seDSACannotEncrypt);
  if FAlgorithm = aaEC then
    raise EScError.Create(seECCannotEncrypt);

  if not (Padding in [pmNone, pmPKCS2, pmOAEP]) then
    raise EScError.Create(seImproperPaddingMode);

  lock(FLock);
  try
    PaddedData := EncodeWithPad(Data, Padding);
    Result := RSAPublicModPow(PaddedData);
  finally
    unlock(FLock);
  end;
end;

function TScKey.Decrypt(const Data: TBytes; Padding: TScPaddingMode = pmPKCS2): TBytes;
var
  PaddedData: TBytes;
begin
  CheckAvailable;

  if FAlgorithm = aaDSA then
    raise EScError.Create(seDSACannotEncrypt);
  if FAlgorithm = aaEC then
    raise EScError.Create(seECCannotEncrypt);

  if not IsPrivate then
    raise EScError.Create(seCannotDecryptData);
  if not (Padding in [pmNone, pmPKCS2, pmOAEP]) then
    raise EScError.Create(seImproperPaddingMode);

  if Length(Data) <> GetKeySize then
    raise EScError.CreateFmt(SErrorEncryptingData, [SInvalidLength], seErrorEncryptingData);

  lock(FLock);
  try
    PaddedData := RSAPrivateModPow(Data);
    Result := DecodePad(PaddedData, Padding);
  finally
    unlock(FLock);
  end;
end;

function TScKey.GetSSHKeyData: TBytes;
begin
  CheckAvailable;
  Result := EncodePublicKeyToOpenSSHFormat;
end;

procedure TScKey.SetSSHKeyData(const Value: TBytes);
begin
  lock(FLock);
  try
    Ready := False;
    DecodePublicKeyFromOpenSSHFormat(Value);
  finally
    unlock(FLock);
  end;

  SaveToStorageIfPossible;
end;

function TScKey.GetAlgorithm: TScAsymmetricAlgorithm;
begin
  Result := FAlgorithm;
end;

function TScKey.GetKeySize: integer; // Byte count
begin
  if not Ready then
    Result := 0
  else
    case FAlgorithm of
      aaRSA:
        Result := ((FRSAData.PublicModulus.BitCount + 7) shr 3);
      aaDSA:
        Result := ((FDSAData.P.BitCount + 7) shr 3);
      aaEC:
        Result := FECData.ECCryptography.FSize;
    else
      Result := 0;
      Assert(False);
    end;
end;

function TScKey.GetBitCount: integer;
begin
  if not Ready then
    Result := 0
  else
    case FAlgorithm of
      aaRSA:
        Result := ((FRSAData.PublicModulus.BitCount + 7) shr 3) shl 3; // to round to byte
      aaDSA:
        Result := ((FDSAData.P.BitCount + 7) shr 3) shl 3;
      aaEC:
        Result := FECData.ECCryptography.BitCount;
    else
      Result := 0;
      Assert(False);
    end;
end;

function TScKey.GetIsPrivate: boolean;
begin
  if not Ready then
    Result := False
  else
    case FAlgorithm of
      aaRSA: begin
        Result := FRSAData.D <> nil;
        if Result then begin
          Assert(FRSAData.P <> nil);
          Assert(FRSAData.Q <> nil);
          Assert(FRSAData.Qinv <> nil);
          Assert(FRSAData.DP <> nil);
          Assert(FRSAData.DQ <> nil);
        end;
      end;
      aaDSA: begin
        Result := FDSAData.X <> nil;
      end;
      aaEC: begin
        Result := FECData.PrivateKoef <> nil;
      end;
    else
      Result := False;
      Assert(False);
    end;
end;

procedure TScKey.SetPSSParams(Value: TScPSSParams);
begin
  FPSSParams.Assign(Value);
end;

procedure TScKey.SetOAEPParams(Value: TScOAEPParams);
begin
  FOAEPParams.Assign(Value);
end;

procedure TScKey.SetStorageList(Value: TScStorageList);
var
  i: integer;
begin
  if FStorageList <> Value then
    if FList <> nil then begin
      for i := 0 to FList.Count - 1 do begin
        if IsClass(TObject(FList[i]), TScCertificate) then
          raise EScError.Create(seCertStorageCannotBeChanged);
      end;
    end;

  inherited;
end;

function TScKey.GetKeyList: TScKeyList;
begin
  Result := TScKeyList(StorageList);
end;

procedure TScKey.SetKeyList(Value: TScKeyList);
begin
  StorageList := Value;
end;

{ TScStorageItem }

constructor TScStorageItem.Create(AStorageList: TScStorageList = nil);
begin
  inherited Create;

  FReady := False;
  FLock := TCriticalSection.Create;
  FStorageList := AStorageList;
  if FStorageList <> nil then
    FStorageList.FList.AddObject('', Self);
end;

destructor TScStorageItem.Destroy;
begin
  FreeData;

  StorageList := nil;
  FLock.Free;

  inherited;
end;

procedure TScStorageItem.FreeData;
begin
  // none
end;

procedure TScStorageItem.InternalAssign(Source: TScStorageItem);
begin
  // none
end;

procedure TScStorageItem.Assign(Source: TPersistent);
begin
  if IsClass(Source, ClassType) then begin
    Ready := False;
    InternalAssign(TScStorageItem(Source));
    SaveToStorageIfPossible;
  end
  else
    inherited Assign(Source);
end;

function TScStorageItem.Equals(Item: TScStorageItem): boolean;
begin
  Result := IsClass(Item, ClassType);
end;

function TScStorageItem.Clone: TScStorageItem;
begin
  Result := TScStorageItemClass(ClassType).Create(nil);
  Result.FItemName := FItemName;
  Result.InternalAssign(Self);
end;

procedure TScStorageItem.CheckAvailable;
begin
  Ready := True;

  if not Ready then
    raise EScError.CreateFmt(SObjNotReady, [ClassName], seObjNotReady);
end;

procedure TScStorageItem.SetReady(const Value: boolean);
begin
  lock(FLock);
  try
    if FReady <> Value then begin
      if Value then begin
        if FStorageList = nil then
          raise EScError.Create(seStorageNoSet)
        else begin
          FStorageList.LoadFromStorage(Self);
          if not FReady then
            raise EScError.Create(seStorageNoSet);
        end;
      end
      else begin
        FreeData;
        FReady := False;
      end;
    end;
  finally
    unlock(FLock);
  end;
end;

procedure TScStorageItem.SaveToStorageIfPossible;
begin
  if not FLoading and Ready and (StorageList <> nil) and (ItemName <> '') then
    StorageList.SaveToStorage(Self);
end;

procedure TScStorageItem.SetItemName(const Value: string);
begin
  if FItemName <> Value then begin
    if FStorageList <> nil then
      FStorageList.RenameItem(Self, Value);

    FItemName := Value;
  end;
end;

procedure TScStorageItem.SetStorageList(Value: TScStorageList);
var
  Index: integer;
begin
  if FStorageList <> Value then begin
    if Value <> nil then
      Value.Add(Self)
    else begin
      lock(FLock);
      try
        if FStorageList <> nil then begin
          lock(FStorageList.FLock);
          try
            Index := FStorageList.IndexOf(Self);
            if Index > -1 then
              FStorageList.FList.Objects[Index] := nil;
          finally
            unlock(FStorageList.FLock);
          end;
        end;

        FStorageList := nil;
      finally
        unlock(FLock);
      end;
    end;
  end;
end;

{ TScStorageList }

constructor TScStorageList.Create(AStorage: TScStorage);
begin
  inherited Create;

//  if AStorage = nil then
//    raise EScError.Create(seStorageNoSet);

  FStorage := AStorage;
  FLock := TCriticalSection.Create;
  FList := TStringList.Create;
end;

destructor TScStorageList.Destroy;
begin
  ClearList;

  FList.Free;
  FLock.Free;
  inherited;
end;

procedure TScStorageList.ClearList;
begin
  lock(FLock);
  try
    while FList.Count > 0 do begin
    {$IFNDEF AUTOREFCOUNT}
      FList.Objects[FList.Count - 1].Free;
    {$ELSE}
      FList.Objects[FList.Count - 1] := nil;
    {$ENDIF}
      FList.Delete(FList.Count - 1);
    end;
  finally
    unlock(FLock);
  end;
end;

function TScStorageList.GetCount: integer;
begin
  lock(FLock);
  try
    Result := FList.Count;
  finally
    unlock(FLock);
  end;
end;

procedure TScStorageList.Add(Item: TScStorageItem);
var
  Index: integer;
begin
  if not IsClass(Item, GetItemClassType) then
    raise EScError.CreateFmt(SInvalidObjectClass, [GetItemClassType.ClassName, Item.ClassName], seInvalidObjectClass);

  lock(FLock);
  try
    CheckItemName(Item.ItemName);
    Item.Ready := True;
    SaveToStorage(Item);

    lock(Item.FLock);
    try
      if Item.FStorageList <> nil then begin
        lock(Item.FStorageList.FLock);
        try
          Index := Item.FStorageList.IndexOf(Item);
          if Index > -1 then
            Item.FStorageList.FList.Objects[Index] := Item.Clone;
        finally
          unlock(Item.FStorageList.FLock);
        end;
      end;

      Item.FStorageList := Self;
    finally
      unlock(Item.FLock);
    end;

    FList.AddObject(Item.ItemName, Item);
  finally
    unlock(FLock);
  end;
end;

procedure TScStorageList.CheckItemName(const ItemName: string);
begin
  if (ItemName = '') and (FStorage <> nil) then
    raise EScError.Create(seObjNameMissing);

  if FindItem(ItemName) <> nil then
    raise EScError.Create(seDuplicateObjName);
end;

function TScStorageList.GetItem(Index: integer): TScStorageItem;
begin
  lock(FLock);
  try
    Result := TScStorageItem(FList.Objects[Index]);

    if Result = nil then begin
      Result := GetItemClassType.Create(nil);
      Result.FStorageList := Self;
      Result.FItemName := FList[Index];
      FList.Objects[Index] := Result;
      LoadFromStorage(Result);
    end
    else
      Result.FStorageList := Self;
  finally
    unlock(FLock);
  end;
end;

procedure TScStorageList.SetItem(Index: integer; Value: TScStorageItem);
begin
  if not IsClass(Value, GetItemClassType) then
    raise EScError.CreateFmt(SInvalidObjectClass, [GetItemClassType.ClassName, Value.ClassName], seInvalidObjectClass);

  GetItem(Index).Assign(Value);
end;

procedure TScStorageList.RenameItem(Item: TScStorageItem; const NewName: string);
var
  Index: integer;
begin
  if Item = nil then
    raise EScError.Create(seInvalidInputArgs);

  lock(FLock);
  try
    CheckItemName(NewName);

    Index := FList.IndexOfObject(Item);
    if Index < 0 then begin
      if Item.Ready then begin
        Item.FItemName := NewName;
        SaveToStorage(Item);
      end;
      FList.AddObject(NewName, Item);
    end
    else begin
      if Item.FItemName <> '' then begin
        if Item.Ready then
          RenameInStorage(Item, NewName)
        else
          RenameInStorageIfExists(Item, NewName);
      end
      else
        if Item.Ready then begin
          Item.FItemName := NewName;
          SaveToStorage(Item);
        end;

      FList[Index] := NewName;
    end;
  finally
    unlock(FLock);
  end;
end;

function TScStorageList.FindItem(const ItemName: string): TScStorageItem;
var
  Index: integer;
begin
  lock(FLock);
  try
    Index := FList.IndexOf(ItemName);
    if Index >= 0 then
      Result := GetItem(Index)
    else
      Result := nil;
  finally
    unlock(FLock);
  end;
end;

function TScStorageList.ItemByName(const ItemName: string): TScStorageItem;
begin
  Result := FindItem(ItemName);
  if Result = nil then
    raise EScError.CreateFmt(SObjNameNotFound, [ItemName], seObjectNotFound);
end;

procedure TScStorageList.GetItemNames(List: TStrings);
var
  i: integer;
begin
  lock(FLock);
  List.BeginUpdate;
  try
    List.Clear;
    for i := 0 to FList.Count - 1 do
      List.Add(FList[i]);
  finally
    List.EndUpdate;
    unlock(FLock);
  end;
end;

function TScStorageList.IndexOf(Item: TScStorageItem): integer;
begin
  lock(FLock);
  try
    Result := FList.IndexOfObject(Item);
  finally
    unlock(FLock);
  end;
end;

procedure TScStorageList.Remove(Item: TScStorageItem);
var
  Index: integer;
begin
  if Item = nil then
    Exit;

  lock(FLock);
  try
    Index := FList.IndexOfObject(Item);
    if Index > -1 then begin
      DeleteFromStorage(Item);
      FList.Delete(Index);
    end;
    Item.FStorageList := nil;
  finally
    unlock(FLock);
  end;
end;

procedure TScStorageList.Clear;
var
  Item: TScStorageItem;
begin
  lock(FLock);
  try
    while FList.Count > 0 do begin
      Item := GetItem(FList.Count - 1);
      Remove(Item);
      Item.Free;
    end;
  finally
    unlock(FLock);
  end;
end;

procedure TScStorageList.Refresh;
var
  NewList: TStringList;
  i: integer;
begin
  if (FStorage = nil) or IsClass(FStorage, TScMemoryStorage) then
    Exit;

  ClearList;

  NewList := TStringList.Create;
  try
    FStorage.GetNames(GetItemClassType, NewList);

    lock(FLock);
    try
      for i := 0 to NewList.Count - 1 do
        FList.AddObject(NewList[i], NewList.Objects[i]);
    finally
      unlock(FLock);
    end;
  finally
    NewList.Free;
  end;
end;

function TScStorageList.FindEqualItem(Item: TScStorageItem): boolean;
var
  i: integer;
begin
  lock(FLock);
  try
    for i := 0 to FList.Count - 1 do begin
      if GetItem(i).Equals(Item) then begin
        Result := True;
        Exit;
      end;
    end;

    Result := False;
  finally
    unlock(FLock);
  end;
end;

procedure TScStorageList.SaveToStorage(Item: TScStorageItem);
begin
  if (FStorage <> nil) and (Item <> nil) then
    FStorage.Save(Item);
end;

procedure TScStorageList.LoadFromStorage(Item: TScStorageItem);
begin
  if (FStorage <> nil) and (Item <> nil) then
    FStorage.Load(Item);
end;

procedure TScStorageList.DeleteFromStorage(Item: TScStorageItem);
begin
  if (FStorage <> nil) and (Item <> nil) then
    FStorage.Delete(Item);
end;

procedure TScStorageList.RenameInStorageIfExists(Item: TScStorageItem; const NewName: string);
begin
  if (FStorage <> nil) and (Item <> nil) then
    FStorage.Rename(Item, NewName, False);
end;

procedure TScStorageList.RenameInStorage(Item: TScStorageItem; const NewName: string);
begin
  if (FStorage <> nil) and (Item <> nil) then
    FStorage.Rename(Item, NewName, True);
end;

procedure TScStorageList.Flush;
begin
  if FStorage <> nil then
    FStorage.Flush;
end;

{ TScKeyList }

function TScKeyList.GetKey(Index: integer): TScKey;
begin
  Result := TScKey(GetItem(Index));
end;

procedure TScKeyList.SetKey(Index: integer; Value: TScKey);
begin
  SetItem(Index, Value);
end;

procedure TScKeyList.CheckKeyName(const KeyName: string);
begin
  CheckItemName(KeyName);
end;

function TScKeyList.FindKey(const KeyName: string): TScKey;
begin
  Result := TScKey(FindItem(KeyName));
end;

function TScKeyList.KeyByName(const KeyName: string): TScKey;
begin
  Result := TScKey(ItemByName(KeyName));
end;

procedure TScKeyList.GetKeyNames(List: TStrings);
begin
  GetItemNames(List);
end;

function TScKeyList.GetItemClassType: TScStorageItemClass;
begin
  Result := TScKey;
end;

function TScKeyList.GetItemDefName: string;
begin
  Result := 'key';
end;

{ TScStorage }

constructor TScStorage.Create(AOwner: TComponent);
begin
  inherited;

  FStoreUserPassword := True;
  FLock := TCriticalSection.Create;
end;

destructor TScStorage.Destroy;
begin
  Invalidate;
  FLock.Free;

  inherited;
end;

procedure TScStorage.Invalidate;
begin
  lock(FLock);
  try
    FreeAndNil(FKeys);
    FreeAndNil(FUsers);
    FreeAndNil(FCertificates);
    FreeAndNil(FCRLs);
  finally
    unlock(FLock);
  end;
end;

function TScStorage.GetKeys: TScKeyList;
begin
  lock(FLock);
  try
    if FKeys = nil then begin
      FKeys := TScKeyList.Create(Self);
      FKeys.Refresh;
    end;
  finally
    unlock(FLock);
  end;

  Result := FKeys;
end;

function TScStorage.GetUsers: TScUserList;
begin
  lock(FLock);
  try
    if FUsers = nil then begin
      FUsers := TScUserList.Create(Self);
      FUsers.Refresh;
    end;
  finally
    unlock(FLock);
  end;

  Result := FUsers;
end;

function TScStorage.GetCertificates: TScCertificateList;
begin
  lock(FLock);
  try
    if FCertificates = nil then begin
      FCertificates := TScCertificateList.Create(Self);
      FCertificates.Refresh;
    end;
  finally
    unlock(FLock);
  end;

  Result := FCertificates;
end;

function TScStorage.GetCRLs: TScCRLList;
begin
  lock(FLock);
  try
    if FCRLs = nil then begin
      FCRLs := TScCRLList.Create(Self);
      FCRLs.Refresh;
    end;
  finally
    unlock(FLock);
  end;

  Result := FCRLs;
end;

procedure TScStorage.CheckReadOnly;
begin
  if FReadOnly then
    raise EScError.Create(seChangingReadOnlyStorage);
end;

procedure TScStorage.Load(Item: TScStorageItem);
begin
  lock(FLock);
  try
    InternalLoad(Item);
  finally
    unlock(FLock);
  end;
end;

procedure TScStorage.Save(Item: TScStorageItem);
begin
  CheckReadOnly;

  lock(FLock);
  try
    InternalSave(Item);
  finally
    unlock(FLock);
  end;
end;

procedure TScStorage.Delete(Item: TScStorageItem);
begin
  CheckReadOnly;

  lock(FLock);
  try
    InternalDelete(Item);
  finally
    unlock(FLock);
  end;
end;

procedure TScStorage.Rename(Item: TScStorageItem; const NewName: string; CheckIfExists: boolean = True);
begin
  CheckReadOnly;

  lock(FLock);
  try
    InternalRename(Item, NewName, CheckIfExists);
  finally
    unlock(FLock);
  end;
end;

procedure TScStorage.GetNames(const ItemClass: TScStorageItemClass; List: TStrings);
begin
  lock(FLock);
  try
    List.BeginUpdate;
    try
      List.Clear;
      InternalGetNames(ItemClass, List);
    finally
      List.EndUpdate;
    end;
  finally
    unlock(FLock);
  end;
end;

procedure TScStorage.DeleteStorage;
begin
  CheckReadOnly;

  lock(FLock);
  try
    InternalDeleteStorage;
  finally
    unlock(FLock);
  end;
end;

procedure TScStorage.Flush;
begin
  CheckReadOnly;
end;

procedure TScStorage.SetReadOnly(const Value: Boolean);
begin
  if Value <> FReadOnly then begin
    FReadOnly := Value;
    Invalidate;
  end;
end;

{$IFDEF SBRIDGE}
procedure TScStorage.CheckUserPass(ClientInfo: TScSSHClientInfo; const APassword: string; var Accept: boolean);
var
  User: TScUser;
begin
  lock(FLock);
  try
    if Accept = True then begin
      User := Users.FindUser(ClientInfo.User);

      if User = nil then
        Accept := False
      else
        User.CheckUserPass(ClientInfo, APassword, Accept);
    end;

    if Assigned(FOnCheckUserPass) then
      FOnCheckUserPass(Self, ClientInfo, APassword, Accept);
  finally
    unlock(FLock);
  end;
end;

procedure TScStorage.CheckUserKey(ClientInfo: TScSSHClientInfo; AKey: TScKey; var Accept: boolean);
var
  User: TScUser;
begin
  lock(FLock);
  try
    if Accept = True then begin
      User := Users.FindUser(ClientInfo.User);

      if User = nil then
        Accept := False
      else
        User.CheckUserKey(ClientInfo, AKey, Accept);
    end;

    if Assigned(FOnCheckUserKey) then
      FOnCheckUserKey(Self, ClientInfo, AKey, Accept);
  finally
    unlock(FLock);
  end;
end;
{$ENDIF}

class function TScStorage.CreateCipher(const Algorithm: TScSymmetricAlgorithm; const Password: string): TSymmetricAlgorithm;
var
  csp: THash_SHA1;
  Key, IV: TBytes;
  KeySize: integer;
begin
  csp := THash_SHA1.Create;
  try
    Key := csp.ComputeHash(Encoding.Default.GetBytes(Password));
  finally
    csp.Free;
  end;

  KeySize := CipherFactory.GetKeySize(Algorithm);
  if KeySize <> Length(Key) then
    SetLength(Key, KeySize);

  SetLength(IV, CipherFactory.GetBlockSize(Algorithm));
  Result := CipherFactory.CreateCipher(Algorithm, Key, IV);
  FillChar(Key[0], Length(Key), 0); // to protect
end;

class procedure TScStorage.LoadObj(Stream: TStream; Item: TScStorageItem; const Algorithm: TScSymmetricAlgorithm; const Password: string);
var
  csp: THash_SHA1;
  Data, Hash: TBytes;
  Cipher: TSymmetricAlgorithm;
  Version: string;
  HashLen, DataLen: integer;
  Reader: TSSH2DataReader;
  BeginPos: Int64;
begin
  if (Stream.Size - Stream.Position) <= 4 then
    raise EScError.Create(seWrongDataFormat);

  if Item.FLoading then
    Exit;

  Item.FLoading := True;
  try
    if IsClass(Item, TScCertificate) then begin
      TScCertificate(Item).ImportFrom(Stream, Password);
    end
    else
    if IsClass(Item, TScCRL) then begin
      TScCRL(Item).ImportFrom(Stream, Password);
    end
    else begin
      BeginPos := Stream.Position;
      SetLength(Data, Stream.Size - Stream.Position);
      Stream.Read(Data[0], Length(Data));

      Version := Encoding.Default.GetString(Data, 0, 4); // read from non decoded data

      if ((Version <> ('SBE' + StorageVersion1)) and (Version <> ('SBP' + StorageVersion1)) and
          (Version <> ('SBE' + StorageVersion2)) and (Version <> ('SBP' + StorageVersion2))) then begin
        if IsClass(Item, TScKey) then begin
          Stream.Position := BeginPos;
          TScKey(Item).ImportFrom(Stream, Password); // New behaviour - support any format
          Exit;
        end
        else
          raise EScError.Create(seWrongDataFormat);
      end;

      csp := THash_SHA1.Create;
      try
        HashLen := csp.HashSize;
        Data := TBase64.Decode(Data, 4, Length(Data) - 4);
        if (Length(Data) <= (4 + HashLen)) then
          raise EScError.Create(seWrongDataFormat);

        SetLength(Hash, 0);
        DataLen := Length(Data) - HashLen;
        if Version[4] = StorageVersion1 then begin
          Hash := csp.ComputeHash(TValueArr(Data), 0, DataLen);
          if MemCompare(@Data[DataLen], @Hash[0], HashLen) <> 0 then
            raise EScError.Create(seWrongDataFormat);
        end;

        if (Length(Password) > 0) and (Version[3] = 'E') then begin
          Cipher := CreateCipher(Algorithm, Password);
          Cipher.CreateDecryptor.TransformBlock(Data, 0, DataLen);
        end;

        if Version[4] = StorageVersion2 then begin
          Hash := csp.ComputeHash(TValueArr(Data), 0, DataLen);
          if MemCompare(@Data[DataLen], @Hash[0], HashLen) <> 0 then
            raise EScError.Create(seWrongDataFormat);
        end;

        SetLength(Data, DataLen);
      finally
        csp.Free;
      end;

      if IsClass(Item, TScKey) then begin // Old format version
        Item.Ready := False;

        Reader := TSSH2DataReader.Create(Data);
        try
          if Reader.ReadByte <> KeyFormatVersion then
            raise EScError.Create(seWrongDataFormat);

          if Reader.ReadBool then // IsPrivate
            TScKey(Item).DecodePrivateKeyFromIETFFormat(Reader.ReadAll, '')
          else
            TScKey(Item).DecodePublicKeyFromIETFFormat(Reader.ReadAll);

          TScKey(Item).CheckReady;
        finally
          Reader.Free;
        end;
      end
      else
      if IsClass(Item, TScUser) then
        TScUser(Item).LoadFromBytes(Data)
      else
        raise EScError.Create(seInvalidInputArgs);
    end;
  finally
    Item.FLoading := False;
  end;
end;

class procedure TScStorage.SaveObj(Stream: TStream; Item: TScStorageItem; const Algorithm: TScSymmetricAlgorithm; const Password: string);
var
  csp: THash_SHA1;
  Cipher: TSymmetricAlgorithm;
  Hash, Data, ResData: TBytes;
  HashLen, DataLen: integer;
  Version: TBytes;
begin
  if not Item.Ready then
    raise EScError.CreateFmt(SObjNotReady, [Item.ClassName], seObjNotReady);

  if IsClass(Item, TScKey) then begin
    if TScKey(Item).IsPrivate then
      TScKey(Item).ExportTo(Stream, False, Password, Algorithm, kfPKCS8enc)
    else
      TScKey(Item).ExportTo(Stream, True, '', Algorithm, kfPKCS1);
  end
  else
  if IsClass(Item, TScCertificate) then begin
    TScCertificate(Item).ExportTo(Stream, cfPEM);
  end
  else
  if IsClass(Item, TScCRL) then begin
    TScCRL(Item).ExportTo(Stream, cfPEM);
  end
  else
  if IsClass(Item, TScUser) then begin
    SetLength(Hash, 0);
    Data := TScUser(Item).SaveToBytes;

    csp := THash_SHA1.Create;
    try
      Hash := csp.ComputeHash(Data);
    finally
      csp.Free;
    end;

    HashLen := Length(Hash);
    DataLen := Length(Data);
    SetLength(Version, 0);
    if Length(Password) > 0 then begin
      Cipher := CreateCipher(Algorithm, Password);
      Cipher.CreateEncryptor.TransformBlock(Data, 0, DataLen);
      Version := Encoding.Default.GetBytes('SBE' + StorageVersion2);
    end
    else
      Version := Encoding.Default.GetBytes('SBP' + StorageVersion2);

    SetLength(Data, DataLen + HashLen);
    Buffer.BlockCopy(Hash, 0, Data, DataLen, HashLen);

    Data := TBase64.Encode(Data);

    SetLength(ResData, Length(Version) + Length(Data));
    Buffer.BlockCopy(Version, 0, ResData, 0, Length(Version));
    Buffer.BlockCopy(Data, 0, ResData, Length(Version), Length(Data));

    WriteBlockInPEMStyle(Stream, ResData, 64, False);
  end
  else
    raise EScError.Create(seInvalidInputArgs);
end;

procedure TScStorage.ImportFromPKCS12(const FileName: string; const Password: string = '');
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    ImportFromPKCS12(Stream, Password);
  finally
    Stream.Free;
  end;
end;

procedure TScStorage.ImportFromPKCS12(Stream: TStream; const Password: string = '');
var
  PKCS12Processor: TScPKCS12Processor;
begin
  PKCS12Processor := TScPKCS12Processor.Create;
  try
    PKCS12Processor.ImportFrom(Stream, Password);
    ImportFromPKCS12(PKCS12Processor);
  finally
    PKCS12Processor.Free;
  end;
end;

procedure TScStorage.ImportFromPKCS12(PKCS12Processor: TScPKCS12Processor);

  procedure AssignList(List: TCRList; StorageItemClass: TScStorageItemClass);
  var
    PKCS12Bag: TScPKCS12Bag;
    StorageList: TScStorageList;
    ItemName: string;
    i, j: integer;
  begin
    if StorageItemClass = TScKey then
      StorageList := GetKeys
    else
    if StorageItemClass = TScCertificate then
      StorageList := GetCertificates
    else
    if StorageItemClass = TScCRL then
      StorageList := GetCRLs
    else
      raise Exception.Create(SInternalError);

    for i := 0 to List.Count - 1 do begin
      PKCS12Bag := TScPKCS12Bag(List[i]);
      if PKCS12Bag.Item = nil then
        Continue;

      if StorageList.FindEqualItem(PKCS12Bag.Item) then
        Continue;

      if PKCS12Bag.FriendlyName <> '' then
        ItemName := PKCS12Bag.FriendlyName
      else
        ItemName := StorageList.GetItemDefName + '0';

      j := 0;
      while StorageList.FindItem(ItemName) <> nil do begin
        Inc(j);
        ItemName := StorageList.GetItemDefName + IntToStr(j);
      end;

      PKCS12Bag.Item.ItemName := Trim(ItemName);
      StorageList.Add(PKCS12Bag.Item);
      PKCS12Bag.FItem := nil; // that not to free when PKCS12Processor is released
    end;
  end;

begin
  AssignList(PKCS12Processor.KeyList, TScKey);
  AssignList(PKCS12Processor.CertList, TScCertificate);
  AssignList(PKCS12Processor.CRLList, TScCRL);
end;

{ TScMemoryStorage }

procedure TScMemoryStorage.InternalLoad(Item: TScStorageItem);
begin
  raise EScError.Create(seObjectNotFound);
end;

procedure TScMemoryStorage.InternalSave(Item: TScStorageItem);
begin
end;

procedure TScMemoryStorage.InternalDelete(Item: TScStorageItem);
begin
end;

procedure TScMemoryStorage.InternalRename(Item: TScStorageItem; const NewName: string; CheckIfExists: boolean);
begin
end;

procedure TScMemoryStorage.InternalGetNames(const ItemClass: TScStorageItemClass; List: TStrings);
begin
end;

procedure TScMemoryStorage.InternalDeleteStorage;
begin
  Keys.Clear;
  Users.Clear;
  Certificates.Clear;
  CRLs.Clear;
end;

{ TScFileStorage }

constructor TScFileStorage.Create(AOwner: TComponent);
begin
  inherited;

  FAlgorithm := saTripleDES_cbc;

  FPath := Def_Path;
  FKeyExt := Def_KeyExt;
  FUserExt := Def_UserExt;
  FCertificateExt := Def_CertificateExt;
  FCRLExt := Def_CRLExt;
end;

procedure TScFileStorage.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScFileStorage) then begin
    TScFileStorage(Dest).FReadOnly := FReadOnly;
    TScFileStorage(Dest).FAlgorithm := FAlgorithm;
    TScFileStorage(Dest).FPassword := FPassword;
    TScFileStorage(Dest).Path := Path;
    TScFileStorage(Dest).KeyExt := KeyExt;
    TScFileStorage(Dest).UserExt := UserExt;
    TScFileStorage(Dest).CertificateExt := CertificateExt;
    TScFileStorage(Dest).CRLExt := CRLExt;
  end
  else
    inherited;
end;

procedure TScFileStorage.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);

  Filer.DefineProperty('Path', ReadPath, WritePath, FPath <> Def_Path);
  Filer.DefineProperty('KeyExt', ReadKeyExt, WriteKeyExt, FKeyExt <> Def_KeyExt);
  Filer.DefineProperty('UserExt', ReadUserExt, WriteUserExt, FUserExt <> Def_UserExt);
  Filer.DefineProperty('CertificateExt', ReadCertificateExt, WriteCertificateExt, FCertificateExt <> Def_CertificateExt);
  Filer.DefineProperty('CRLExt', ReadCRLExt, WriteCRLExt, FCRLExt <> Def_CRLExt);
end;

procedure TScFileStorage.ReadPath(Reader: TReader);
begin
  FPath := Reader.ReadString;
end;

procedure TScFileStorage.WritePath(Writer: TWriter);
begin
  Writer.WriteString(FPath);
end;

procedure TScFileStorage.ReadKeyExt(Reader: TReader);
begin
  FKeyExt := Reader.ReadString;
end;

procedure TScFileStorage.WriteKeyExt(Writer: TWriter);
begin
  Writer.WriteString(FKeyExt);
end;

procedure TScFileStorage.ReadUserExt(Reader: TReader);
begin
  FUserExt := Reader.ReadString;
end;

procedure TScFileStorage.WriteUserExt(Writer: TWriter);
begin
  Writer.WriteString(FUserExt);
end;

procedure TScFileStorage.ReadCertificateExt(Reader: TReader);
begin
  FCertificateExt := Reader.ReadString;
end;

procedure TScFileStorage.WriteCertificateExt(Writer: TWriter);
begin
  Writer.WriteString(FCertificateExt);
end;

procedure TScFileStorage.ReadCRLExt(Reader: TReader);
begin
  FCRLExt := Reader.ReadString;
end;

procedure TScFileStorage.WriteCRLExt(Writer: TWriter);
begin
  Writer.WriteString(FCRLExt);
end;

function TScFileStorage.GetItemExt(ItemClass: TScStorageItemClass): string;
begin
  if ItemClass = TScKey then
    Result := FKeyExt
  else
  if ItemClass = TScUser then
    Result := FUserExt
  else
  if ItemClass = TScCertificate then
    Result := FCertificateExt
  else
  if ItemClass = TScCRL then
    Result := FCRLExt
  else
    Assert(False);
end;

procedure TScFileStorage.GetItemFileName(Item: TScStorageItem; out FileName, Ext: string);
begin
  FileName := Item.ItemName;
  Ext := GetItemExt(TScStorageItemClass(Item.ClassType));
end;

function TScFileStorage.GetFileName(Item: TScStorageItem; CreateDir: boolean = False): string;
var
  Dir, FileName, Ext: string;
begin
  GetItemFileName(Item, FileName, Ext);
  if FileName = '' then
    raise EScError.Create(seNullName);

  Dir := GetFullPath;
  Assert(Ext <> '');
  Result := IncludeTrailingBackslash(Dir) + FileName + '.' + Ext;

  if CreateDir then begin
    Dir := ExcludeTrailingBackslash(Dir);
    if (Dir <> '') and (Dir[Length(Dir)] = '.') then
      Dir := ExcludeTrailingBackslash(ExtractFilePath(Dir));

    if (Dir <> '') and not ForceDirectories(Dir) then
      RaiseLastOSError;
  end;
end;

procedure TScFileStorage.InternalLoad(Item: TScStorageItem);
var
  Stream: TFileStream;
begin
  Stream := nil;
  try
    try
      Stream := TFileStream.Create(GetFileName(Item), fmOpenRead or fmShareDenyWrite);
    except
      on e: EFOpenError do
        raise EScError.Create(seObjectNotFound);
    end;

    if Stream.Size = 0 then
      raise EScError.Create(seObjectNotFound);

    LoadObj(Stream, Item, Algorithm, Password);
  finally
    Stream.Free;
  end;
end;

procedure TScFileStorage.InternalSave(Item: TScStorageItem);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(GetFileName(Item, True), fmCreate);
  try
    SaveObj(Stream, Item, Algorithm, Password);
  finally
    Stream.Free;
  end;
end;

procedure TScFileStorage.InternalGetNames(const ItemClass: TScStorageItemClass; List: TStrings);
{$IFDEF FPC}
const
  faVolumeId = $00000008;
{$ENDIF}
var
  SearchRec: TSearchRec;
  FullPath: string;
  FileNames, FileExt: string;
  fn: string;
begin
  FullPath := GetFullPath;
  if (FullPath <> '') and not DirectoryExists(FullPath) then
    Exit;

  FileExt := '.' + GetItemExt(ItemClass);
  FileNames := '*' + FileExt;

  try
    if FindFirst(IncludeTrailingBackslash(ExpandFileName(IncludeTrailingBackslash(FullPath))) + FileNames, faAnyFile, SearchRec) = 0 then
      repeat
        if (SearchRec.Name <> '.') and
          (SearchRec.Attr and faVolumeID <> faVolumeID) and
          (SearchRec.Attr and faDirectory <> faDirectory) then
          if LowerCase(ExtractFileExt(SearchRec.Name)) = FileExt then begin
            fn := ChangeFileExt(SearchRec.Name, '');
            if List.IndexOf(fn) < 0 then
              List.Add(fn);
          end;
      until FindNext(SearchRec) <> 0;
  finally
    FindClose(SearchRec);
  end;
end;

procedure TScFileStorage.SetPath(const Value: string);
begin
  if Value <> FPath then begin
    FPath := Trim(Value);
    Invalidate;
  end;
end;

procedure TScFileStorage.SetKeyExt(const Value: string);
begin
  if Value <> FKeyExt then begin
    FKeyExt := Trim(Value);
    Invalidate;
  end;
end;

procedure TScFileStorage.SetUserExt(const Value: string);
begin
  if Value <> FUserExt then begin
    FUserExt := Trim(Value);
    Invalidate;
  end;
end;

procedure TScFileStorage.SetCertificateExt(const Value: string);
begin
  if Value <> FCertificateExt then begin
    FCertificateExt := Trim(Value);
    Invalidate;
  end;
end;

procedure TScFileStorage.SetCRLExt(const Value: string);
begin
  if Value <> FCRLExt then begin
    FCRLExt := Trim(Value);
    Invalidate;
  end;
end;

function TScFileStorage.GetFullPath: string;
begin
  Result := FPath;
  if //(csDeSigning in ComponentState) and
     (Result <> '') and (Result[1] = '.') and  // Path = '.'; '..'; '.\'; '..\'
     (CurrentProjectOutputDir <> '')
   then
     Result := IncludeTrailingBackslash(CurrentProjectOutputDir) + Result;
end;

procedure TScFileStorage.InternalDelete(Item: TScStorageItem);
var
  FileName: string;
begin
  FileName := GetFileName(Item);
  if FileExists(FileName) then
    if not DeleteFile(FileName) then
      RaiseLastOSError;
end;

procedure TScFileStorage.InternalRename(Item: TScStorageItem; const NewName: string; CheckIfExists: boolean);
var
  Dir: string;
  OldName, Ext: string;
  OldFileName, NewFileName: string;
begin
  GetItemFileName(Item, OldName, Ext);

  if (OldName = '') or (NewName = '') then
    raise EScError.Create(seNullName);

  Assert(Ext <> '');
  Dir := IncludeTrailingBackslash(GetFullPath);

  OldFileName := Dir + OldName + '.' + Ext;
  if not FileExists(OldFileName) then begin
    if CheckIfExists then
      raise EScError.Create(seObjectNotFound)
    else
      Exit;
  end;

  NewFileName := Dir + NewName + '.' + Ext;
  if FileExists(NewFileName) then
    raise EScError.CreateFmt(SObjectAlreadyExists, [NewName], seObjectAlreadyExists);

  RenameFile(OldFileName, NewFileName);
end;

procedure TScFileStorage.ChangePassword(const NewPassword: string);
var
  List: TStringList;
  Dir, TmpFileName: string;

  procedure ChangeItemPassword(Item: TScStorageItem);
  var
    FileName, Ext: string;
    OldFileName: string;
  begin
    Load(Item);

    GetItemFileName(Item, FileName, Ext);
    OldFileName := Dir + FileName + '.' + Ext;
    if FileExists(TmpFileName) then
      if not DeleteFile(TmpFileName) then
        RaiseLastOSError;
    RenameFile(OldFileName, TmpFileName);

    FPassword := NewPassword;
    try
      Save(Item);
    except
      DeleteFile(OldFileName);
      RenameFile(TmpFileName, OldFileName);
      raise;
    end;
  end;

var
  Key: TScKey;
  User: TScUser;
  OldPassword: string;
  i: integer;
begin
  CheckReadOnly;
  if FPassword = NewPassword then
    Exit;

  lock(FLock);
  try
    Key := nil;
    User := nil;
    List := TStringList.Create;

    try
      OldPassword := FPassword;
      Dir := IncludeTrailingBackslash(GetFullPath);

      try
        TmpFileName := Dir + TMP_NAME + '.obj';
        i := 0;
        while FileExists(TmpFileName) do begin
          Inc(i);
          TmpFileName := Dir + TMP_NAME + IntToStr(i) + '.obj';
        end;

        try
          Key := TScKey.Create(nil);
          GetNames(TScKey, List);

          for i := 0 to List.Count - 1 do begin
            FPassword := OldPassword;
            Key.KeyName := List[i];
            ChangeItemPassword(Key);
          end;

          User := TScUser.Create(nil);
          GetNames(TScUser, List);

          for i := 0 to List.Count - 1 do begin
            FPassword := OldPassword;
            User.UserName := List[i];
            ChangeItemPassword(User);
          end;

        finally
          DeleteFile(TmpFileName);
        end;
      except
        FPassword := OldPassword;
        raise;
      end;

      FPassword := NewPassword;
    finally
      List.Free;
      Key.Free;
      User.Free;
    end;
  finally
    unlock(FLock);
  end;
end;

procedure TScFileStorage.InternalDeleteStorage;
var
  List: TStringList;
  Dir: string;
  i, c: integer;
begin
  Dir := IncludeTrailingBackslash(GetFullPath);
  List := TStringList.Create;
  try
    for c := Low(STORAGE_ITEM_CLASSES) to High(STORAGE_ITEM_CLASSES) do begin
      GetNames(STORAGE_ITEM_CLASSES[c], List);

      for i := 0 to List.Count - 1 do
        DeleteFile(Dir + List[i] + '.' + GetItemExt(STORAGE_ITEM_CLASSES[c]));
    end;
  finally
    List.Free;
  end;

  RemoveDir(GetFullPath);
  Invalidate;
end;

{$IFDEF MSWINDOWS}
{ TScRegStorage }

constructor TScRegStorage.Create(AOwner: TComponent);
begin
  inherited;

  FKeyPath := STORAGE_KEY_PATH;
  FAlgorithm := saTripleDES_cbc;
  FRegistry := TRegistry.Create;
end;

destructor TScRegStorage.Destroy;
begin
  FRegistry.Free;

  inherited;
end;

procedure TScRegStorage.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScRegStorage) then begin
    TScRegStorage(Dest).FReadOnly := FReadOnly;
    TScRegStorage(Dest).FAlgorithm := FAlgorithm;
    TScRegStorage(Dest).FPassword := FPassword;
    TScRegStorage(Dest).RootKey := RootKey;
    TScRegStorage(Dest).KeyPath := KeyPath;
  end
  else
    inherited;
end;

procedure TScRegStorage.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);

  Filer.DefineProperty('KeyPath', ReadKeyPath, WriteKeyPath, KeyPath <> STORAGE_KEY_PATH);
end;

procedure TScRegStorage.ReadKeyPath(Reader: TReader);
begin
  FKeyPath := Reader.ReadString;
end;

procedure TScRegStorage.WriteKeyPath(Writer: TWriter);
begin
  Writer.WriteString(FKeyPath);
end;

function TScRegStorage.GetItemDir(ItemClass: TScStorageItemClass): string;
begin
  if ItemClass = TScKey then
    Result := FKeyPath + REGISTRY_KEYS
  else
  if ItemClass = TScUser then
    Result := FKeyPath + REGISTRY_USERS
  else
  if ItemClass = TScCertificate then
    Result := FKeyPath + REGISTRY_CERTIFICATES
  else
  if ItemClass = TScCRL then
    Result := FKeyPath + REGISTRY_CRLS
  else
    Assert(False);
end;

procedure TScRegStorage.GetItemKeyPath(Item: TScStorageItem; out ARegPath, ARegName: string);
begin
  ARegName := Item.ItemName;
  ARegPath := GetItemDir(TScStorageItemClass(Item.ClassType));
end;

procedure TScRegStorage.InternalLoad(Item: TScStorageItem);
var
  CurRegPath, RegName: string;
  Data: TBytes;
  Stream: TMemoryStream;
begin
  GetItemKeyPath(Item, CurRegPath, RegName);

  if not FRegistry.OpenKeyReadOnly(CurRegPath) then
    raise EScError.Create(seKeyPathNotFound);
  try
    if FRegistry.ValueExists(RegName) then begin
      if FRegistry.GetDataType(RegName) = rdString then
        Data := Encoding.Default.GetBytes(FRegistry.ReadString(RegName))
      else begin
        SetLength(Data, FRegistry.GetDataSize(RegName));
        if Length(Data) > 0 then
          FRegistry.ReadBinaryData(RegName, Data[0], Length(Data));
      end;

      Stream := TMemoryStream.Create;
      try
        Stream.WriteBuffer(Data[0], Length(Data));
        Stream.Position := 0;
        LoadObj(Stream, Item, Algorithm, Password);
      finally
        Stream.Free;
      end;
    end
    else
      raise EScError.Create(seObjectNotFound);
  finally
    FRegistry.CloseKey;
  end;
end;

procedure TScRegStorage.InternalSave(Item: TScStorageItem);
var
  CurRegPath, RegName: string;
  Data: TBytes;
  Stream: TMemoryStream;
begin
  GetItemKeyPath(Item, CurRegPath, RegName);

  Stream := TMemoryStream.Create;
  try
    SaveObj(Stream, Item, Algorithm, Password);
    Stream.Position := 0;
    SetLength(Data, Stream.Size);
    Stream.ReadBuffer(Data[0], Length(Data));
  finally
    Stream.Free;
  end;

  FRegistry.Access := KEY_ALL_ACCESS;
  Win32Check(FRegistry.OpenKey(CurRegPath, True));
  try
    FRegistry.WriteBinaryData(RegName, Data[0], Length(Data));
  finally
    FRegistry.CloseKey;
  end;
end;

procedure TScRegStorage.InternalDelete(Item: TScStorageItem);
var
  CurRegPath, RegName: string;
begin
  GetItemKeyPath(Item, CurRegPath, RegName);

  FRegistry.Access := KEY_ALL_ACCESS;
  if FRegistry.OpenKey(CurRegPath, False) then begin
    FRegistry.DeleteValue(RegName);
    FRegistry.CloseKey;
  end;
end;

procedure TScRegStorage.InternalRename(Item: TScStorageItem; const NewName: string; CheckIfExists: boolean);
var
  OldName, CurRegPath: string;
begin
  GetItemKeyPath(Item, CurRegPath, OldName);

  if (OldName = '') or (NewName = '') then
    raise EScError.Create(seNullName);

  FRegistry.Access := KEY_ALL_ACCESS;
  if not FRegistry.OpenKey(CurRegPath, False) then begin
    if CheckIfExists then
      raise EScError.Create(seKeyPathNotFound)
    else
      Exit;
  end;

  try
    if not FRegistry.ValueExists(OldName) then begin
      if CheckIfExists then
        raise EScError.Create(seObjectNotFound)
      else
        Exit;
    end;

    if FRegistry.ValueExists(NewName) then
      raise EScError.CreateFmt(SObjectAlreadyExists, [NewName], seObjectAlreadyExists);

    FRegistry.RenameValue(OldName, NewName);
  finally
    FRegistry.CloseKey;
  end;
end;

procedure TScRegStorage.ChangePassword(const NewPassword: string);
var
  CurRegistry: TRegistry;
  List: TStringList;
  TmpName: string;

  procedure ChangeItemPassword(Item: TScStorageItem);
  var
    CurPath, ItemName: string;
  begin
    Load(Item);
    GetItemKeyPath(Item, CurPath, ItemName);

    if CurRegistry.ValueExists(TmpName) then
      CurRegistry.DeleteValue(TmpName);
    CurRegistry.RenameValue(ItemName, TmpName);

    FPassword := NewPassword;
    try
      Save(Item);
    except
      CurRegistry.DeleteValue(ItemName);
      CurRegistry.RenameValue(TmpName, ItemName);
      raise;
    end;
  end;

var
  Key: TScKey;
  User: TScUser;
  OldPassword: string;
  i: integer;
begin
  CheckReadOnly;
  if FPassword = NewPassword then
    Exit;

  lock(FLock);
  try
    Key := nil;
    User := nil;
    List := TStringList.Create;

    try
      OldPassword := FPassword;

      try
        Key := TScKey.Create(nil);
        GetNames(TScKey, List);

        if List.Count > 0 then begin
          CurRegistry := TRegistry.Create;
          try
            CurRegistry.RootKey := FRegistry.RootKey;
            CurRegistry.Access := KEY_ALL_ACCESS;
            if not CurRegistry.OpenKey(FKeyPath + REGISTRY_KEYS, False) then
              raise EScError.Create(seKeyPathNotFound);

            TmpName := TMP_NAME;
            i := 0;
            while CurRegistry.ValueExists(TmpName) do begin
              Inc(i);
              TmpName := TMP_NAME + IntToStr(i);
            end;

            try
              for i := 0 to List.Count - 1 do begin
                FPassword := OldPassword;
                Key.KeyName := List[i];
                ChangeItemPassword(Key);
              end;
            finally
              CurRegistry.DeleteValue(TmpName);
            end;
          finally
            CurRegistry.Free;
          end;
        end;

        User := TScUser.Create(nil);
        GetNames(TScUser, List);

        if List.Count > 0 then begin
          CurRegistry := TRegistry.Create;
          try
            CurRegistry.RootKey := FRegistry.RootKey;
            CurRegistry.Access := KEY_ALL_ACCESS;
            if not CurRegistry.OpenKey(FKeyPath + REGISTRY_USERS, False) then
              raise EScError.Create(seKeyPathNotFound);

            TmpName := TMP_NAME;
            i := 0;
            while CurRegistry.ValueExists(TmpName) do begin
              Inc(i);
              TmpName := TMP_NAME + IntToStr(i);
            end;

            try
              for i := 0 to List.Count - 1 do begin
                FPassword := OldPassword;
                User.UserName := List[i];
                ChangeItemPassword(User);
              end;
            finally
              CurRegistry.DeleteValue(TmpName);
            end;
          finally
            CurRegistry.Free;
          end;
        end;
      except
        FPassword := OldPassword;
        raise;
      end;

      FPassword := NewPassword;
    finally
      List.Free;
      Key.Free;
      User.Free;
    end;
  finally
    unlock(FLock);
  end;
end;

procedure TScRegStorage.InternalGetNames(const ItemClass: TScStorageItemClass; List: TStrings);
var
  CurRegPath: string;
begin
  try
    CurRegPath := GetItemDir(ItemClass);
    if FRegistry.OpenKeyReadOnly(CurRegPath) then
      FRegistry.GetValueNames(List);
  finally
    FRegistry.CloseKey;
  end;
end;

procedure TScRegStorage.SetKeyPath(const Value: string);
begin
  if Value <> FKeyPath then begin
    FKeyPath := Trim(Value);
    Invalidate;
  end;
end;

procedure TScRegStorage.SetRootKey(const Value: NativeUint);
begin
  if Value <> FRegistry.RootKey then begin
    FRegistry.RootKey := Value;
    Invalidate;
  end;
end;

function TScRegStorage.GetRootKey: NativeUint;
begin
  Result := FRegistry.RootKey;
end;

procedure TScRegStorage.InternalDeleteStorage;
var
  c: integer;
begin
  for c := Low(STORAGE_ITEM_CLASSES) to High(STORAGE_ITEM_CLASSES) do
    FRegistry.DeleteKey(GetItemDir(STORAGE_ITEM_CLASSES[c]));

  Invalidate;
end;
{$ENDIF}

{ TScUser }

constructor TScUser.Create(AStorageList: TScStorageList = nil);
begin
  inherited Create(AStorageList);

  FSSHChannelPermissions := [cpAllowLocalForwarding, cpAllowRemoteForwarding, cpAllowShell, cpAllowSFTP];
  FExtData := '';
  FAuthentications := [];
end;

destructor TScUser.Destroy;
begin
  FKey.Free;

  inherited;
  Password := ''; // to protect
end;

function TScUser.Equals(Item: TScStorageItem): boolean;
var
  User: TScUser;
begin
  Result := IsClass(Item, TScUser);
  if not Result then
    Exit;

  User := TScUser(Item);
  Result := UserName = User.UserName;
end;

procedure TScUser.InternalAssign(Source: TScStorageItem);
begin
  if not IsClass(Source, TScUser) then
    raise EScError.CreateFmt(SInvalidObjectClass, [TScUser.ClassName, Source.ClassName], seInvalidObjectClass);

  FAuthentications := TScUser(Source).FAuthentications;
  FDomain := TScUser(Source).FDomain;
  FHomePath := TScUser(Source).FHomePath;
  FSSHChannelPermissions := TScUser(Source).FSSHChannelPermissions;
  FExtData := TScUser(Source).FExtData;
  FPassword := TScUser(Source).FPassword;
  FHashPassword := TScUser(Source).FHashPassword;

  CheckAuthenticationKey;
  if FKey <> nil then
    FKey.Assign(TScUser(Source).Key);
  UserName := TScUser(Source).UserName; // renaming
end;

{$IFDEF SBRIDGE}
procedure TScUser.CheckUserPass(ClientInfo: TScSSHClientInfo; const APassword: string; var Accept: boolean);
{$IFDEF MSWINDOWS}
const
  LOGON_TYPE = LOGON32_LOGON_NETWORK; // LOGON32_LOGON_INTERACTIVE
  LOGON_PROVIDER = LOGON32_PROVIDER_DEFAULT;
{$ENDIF}
var
{$IFDEF MSWINDOWS}
  phToken: THandle;
{$ENDIF}
  CalcPassword: string;
  csp: THash_SHA1;
begin
  TScSSHConnectionInfoUtils.SetDomain(ClientInfo, FDomain);
  TScSSHConnectionInfoUtils.SetOSAuthentication(ClientInfo, uaOSAuthentication in FAuthentications);
  TScSSHConnectionInfoUtils.SetHomePath(ClientInfo, FHomePath);
  TScSSHConnectionInfoUtils.SetSSHChannelPermissions(ClientInfo, FSSHChannelPermissions);
  TScSSHConnectionInfoUtils.SetUserExtData(ClientInfo, FExtData);

  if uaPassword in FAuthentications then begin
    if (FPassword = '') and (FHashPassword <> '') then begin
      csp := THash_SHA1.Create;
      try
        CalcPassword := BytesToHexStr(csp.ComputeHash(Encoding.UTF8.GetBytes(APassword)));
      finally
        csp.Free;
      end;
      Accept := FHashPassword = CalcPassword;
    end
    else
      Accept := FPassword = APassword;
  end
  else
    Accept := False;

{$IFDEF MSWINDOWS}
  if not Accept and (uaOSAuthentication in FAuthentications) then begin
    Accept := LogonUser(PChar(FItemName), PChar(FDomain), PChar(APassword), LOGON_TYPE, LOGON_PROVIDER, phToken);
    if Accept then
      CloseHandle(phToken);
  end;
{$ENDIF}
end;

procedure TScUser.CheckUserKey(ClientInfo: TScSSHClientInfo; AKey: TScKey; var Accept: boolean);
begin
  TScSSHConnectionInfoUtils.SetHomePath(ClientInfo, FHomePath);
  TScSSHConnectionInfoUtils.SetSSHChannelPermissions(ClientInfo, FSSHChannelPermissions);
  TScSSHConnectionInfoUtils.SetUserExtData(ClientInfo, FExtData);

  if uaPublicKey in FAuthentications then begin
    Assert(FKey <> nil);
    Accept := FKey.Equals(AKey);
  end
  else
    Accept := False;
end;
{$ENDIF}

procedure TScUser.SetItemName(const Value: string);
begin
  if FItemName <> Value then begin
    inherited;

    FReady := True;
    SaveToStorageIfPossible; // to reset Username
  end;
end;

procedure TScUser.SetDomain(const Value: string);
begin
  if FDomain <> Value then begin
    FDomain := Value;
    SaveToStorageIfPossible;
  end;
end;

procedure TScUser.SetHomePath(const Value: string);
begin
  if FHomePath <> Value then begin
    FHomePath := Value;
    SaveToStorageIfPossible;
  end;
end;

procedure TScUser.SetExtData(const Value: string);
begin
  if FExtData <> Value then begin
    FExtData := Value;
    SaveToStorageIfPossible;
  end;
end;

procedure TScUser.SetSSHChannelPermissions(const Value: TScSSHChannelPermissions);
begin
  if FSSHChannelPermissions <> Value then begin
    FSSHChannelPermissions := Value;
    SaveToStorageIfPossible;
  end;
end;

procedure TScUser.SetPassword(const Value: string);
var
  csp: THash_SHA1;
begin
  if FPassword <> Value then begin
    // to protect
    if Length(FPassword) > 0 then
      FillChar(FPassword[1], Length(FPassword) * sizeof(Char), 0);
    if Length(FHashPassword) > 0 then
      FillChar(FHashPassword[1], Length(FHashPassword) * sizeof(Char), 0);

    FPassword := Value;
    if Value <> '' then begin
      csp := THash_SHA1.Create;
      try
        FHashPassword := BytesToHexStr(csp.ComputeHash(Encoding.UTF8.GetBytes(Value)));
      finally
        csp.Free;
      end;
    end
    else
      FHashPassword := '';

    SaveToStorageIfPossible;
  end;
end;

procedure TScUser.CheckAuthenticationKey;
begin
  if (uaPublicKey in FAuthentications) and (FKey = nil) then begin
    FKey := TScKey.Create(nil);
    FKey.RegisterClient(Self);
  end
  else
  if not (uaPublicKey in FAuthentications) and (FKey <> nil) then
    FreeAndNil(FKey);
end;

procedure TScUser.SetAuthentications(const Value: TScUserAuthentications);
begin
  if FAuthentications <> Value then begin
    FAuthentications := Value;

    CheckAuthenticationKey;
    SaveToStorageIfPossible;
  end;
end;

procedure TScUser.SetKey(Value: TScKey);
begin
  if FKey <> Value then begin
    if FKey = nil then
      raise EScError.Create(seKeyAuthNotSupported);

    FKey.Assign(Value);
    SaveToStorageIfPossible;
  end;
end;

procedure TScUser.LoadKeyFromBytes(const Data: TBytes);
var
  Reader: TSSH2DataReader;
  bKeyFormatVersion: byte;
begin
  Key.Ready := False;

  Reader := TSSH2DataReader.Create(Data);
  try
    bKeyFormatVersion := Reader.ReadByte;
    if (bKeyFormatVersion <> KeyFormatVersion) and (bKeyFormatVersion <> ECKeyFormatVersion) then
      raise EScError.Create(seWrongDataFormat);

    if Reader.ReadBool then begin // IsPrivate
      if bKeyFormatVersion = ECKeyFormatVersion then
        Key.DecodePrivateKeyFromPKCS8Format(Reader.ReadAll)
      else
        Key.DecodePrivateKeyFromIETFFormat(Reader.ReadAll, '');
    end
    else
      Key.DecodePublicKeyFromIETFFormat(Reader.ReadAll);
  finally
    Reader.Free;
  end;

  Key.CheckReady;
end;

function TScUser.SaveKeyToBytes: TBytes;
var
  wr: TSSH2DataStream;
  KeyData: TBytes;
  bKeyFormatVersion: byte;
begin
  if not Key.Ready then
    raise EScError.Create(seKeyNotReady);

  bKeyFormatVersion := KeyFormatVersion;

  if Key.IsPrivate then begin
    if Key.Algorithm = aaEC then begin
      bKeyFormatVersion := ECKeyFormatVersion;
      KeyData := Key.EncodePrivateKeyToPKCS8Format;
    end
    else
      KeyData := Key.EncodePrivateKeyToIETFFormat('');
  end
  else
    KeyData := Key.EncodePublicKeyToIETFFormat;

  wr := TSSH2DataStream.Create;
  try
    wr.WriteByte(bKeyFormatVersion);
    wr.WriteBool(Key.IsPrivate);
    wr.WriteBuf(KeyData);
    Result := wr.ToBytes;
  finally
    wr.Free;
  end;
end;

procedure TScUser.LoadFromBytes(const Data: TBytes);
var
  Reader: TSSH2DataReader;
  Version: byte;
  buf: TBytes;
  PropName: string;
  cp: TScSSHChannelPermission;
  SSHChannelPermissionsCode: integer;
begin
  SetLength(buf, 0);
  FReady := False;

  Reader := TSSH2DataReader.Create(Data);
  try
    Version := Reader.ReadByte;
    if not (Version in [UserFormatVersion1, UserFormatVersion2, UserFormatVersion3, UserFormatVersion4]) then
      raise EScError.Create(seWrongDataFormat);

    if FItemName <> string(Encoding.UTF8.{$IFNDEF NEXTGEN}GetWideString{$ELSE}GetString{$ENDIF}(Reader.ReadString)) then
      raise EScError.Create(seWrongDataFormat);

    FAuthentications := [];
    if Reader.ReadBool then
      Include(FAuthentications, uaPublicKey);
    if Reader.ReadBool then
      Include(FAuthentications, uaPassword);
    if Reader.ReadBool then
      Include(FAuthentications, uaOSAuthentication);
    CheckAuthenticationKey;

    if Version >= UserFormatVersion2 then
      FDomain := string(Encoding.UTF8.{$IFNDEF NEXTGEN}GetWideString{$ELSE}GetString{$ENDIF}(Reader.ReadString));

    if Version >= UserFormatVersion3 then
      FHomePath := string(Encoding.UTF8.{$IFNDEF NEXTGEN}GetWideString{$ELSE}GetString{$ENDIF}(Reader.ReadString));

    Password := string(Encoding.UTF8.{$IFNDEF NEXTGEN}GetWideString{$ELSE}GetString{$ENDIF}(Reader.ReadString));
    if Version >= UserFormatVersion2 then
      FHashPassword := string(Encoding.Default.GetString(Reader.ReadString));

    if Reader.ReadBool then begin
      buf := Reader.ReadString;
      if uaPublicKey in Authentications then
        LoadKeyFromBytes(buf);
    end;

    FExtData := '';
    FSSHChannelPermissions := [cpAllowLocalForwarding, cpAllowRemoteForwarding, cpAllowShell, cpAllowSFTP];

    while Reader.Rest > 0 do begin // due compatibility with older format
      try
        PropName := string(Encoding.UTF8.{$IFNDEF NEXTGEN}GetWideString{$ELSE}GetString{$ENDIF}(Reader.ReadString));

        if SameText(PropName, SSHPERMS_PROP_NAME) then begin
          FSSHChannelPermissions := [];
          SSHChannelPermissionsCode := Reader.ReadInt32;
          for cp := Low(TScSSHChannelPermission) to High(TScSSHChannelPermission) do begin
            if (SSH_CHANNEL_PERMISSION_CODES[cp] and SSHChannelPermissionsCode) <> 0 then
              Include(FSSHChannelPermissions, cp);
          end;
        end
        else
        if SameText(PropName, EXTDATA_PROP_NAME) then
          FExtData := string(Encoding.UTF8.{$IFNDEF NEXTGEN}GetWideString{$ELSE}GetString{$ENDIF}(Reader.ReadString))
      except
      end;
    end;

    FReady := True;
  finally
    Reader.Free;
  end;
end;

function TScUser.SaveToBytes: TBytes;
var
  wr: TSSH2DataStream;
  cp: TScSSHChannelPermission;
  SSHChannelPermissionsCode: integer;
begin
  SetLength(Result, 0);
  wr := TSSH2DataStream.Create;
  try
    wr.WriteByte(UserFormatVersion4);
    wr.WriteWStr(WideString(FItemName));
    wr.WriteBool(uaPublicKey in FAuthentications);
    wr.WriteBool(uaPassword in FAuthentications);
    wr.WriteBool(uaOSAuthentication in FAuthentications);
    wr.WriteWStr(WideString(FDomain));
    wr.WriteWStr(WideString(FHomePath));

    if (FStorageList <> nil) and (FStorageList.Storage <> nil) and not FStorageList.Storage.StoreUserPassword then
      wr.WriteAStr('')
    else
      wr.WriteWStr(WideString(FPassword));

    wr.WriteAStr(FHashPassword);

    if (uaPublicKey in FAuthentications) and Key.Ready then begin
      wr.WriteBool(True);
      wr.WriteAsString(SaveKeyToBytes);
    end
    else
      wr.WriteBool(False);

    SSHChannelPermissionsCode := 0;
    for cp := Low(TScSSHChannelPermission) to High(TScSSHChannelPermission) do begin
      if cp in FSSHChannelPermissions then
        Inc(SSHChannelPermissionsCode, SSH_CHANNEL_PERMISSION_CODES[cp]);
    end;

    wr.WriteWStr(WideString(SSHPERMS_PROP_NAME));
    wr.WriteInt32(SSHChannelPermissionsCode);

    wr.WriteWStr(WideString(EXTDATA_PROP_NAME));
    wr.WriteWStr(WideString(FExtData));

    Result := wr.ToBytes;
  finally
    wr.Free;
  end;
end;

function TScUser.GetUserList: TScUserList;
begin
  Result := TScUserList(StorageList);
end;

procedure TScUser.SetUserList(Value: TScUserList);
begin
  StorageList := Value;
end;

{ TScUSerList }

function TScUserList.GetUser(Index: integer): TScUser;
begin
  Result := TScUser(GetItem(Index));
end;

procedure TScUserList.SetUser(Index: integer; Value: TScUser);
begin
  SetItem(Index, Value);
end;

procedure TScUserList.CheckUserName(const UserName: string);
begin
  CheckItemName(UserName);
end;

function TScUserList.FindUser(const UserName: string): TScUser;
begin
  Result := TScUser(FindItem(UserName));
end;

function TScUserList.UserByName(const UserName: string): TScUser;
begin
  Result := TScUser(ItemByName(UserName));
end;

procedure TScUserList.GetUserNames(List: TStrings);
begin
  GetItemNames(List);
end;

function TScUserList.GetItemClassType: TScStorageItemClass;
begin
  Result := TScUser;
end;

function TScUserList.GetItemDefName: string;
begin
  Result := 'usr';
end;

{ TScCertificate }

constructor TScCertificate.Create(AStorageList: TScStorageList = nil);
begin
  inherited Create(AStorageList);

  FIsSelfSigned := -1;
end;

procedure TScCertificate.FreeData;
begin
  lock(FLock);
  try
    SetHandle(nil, nil);
    FCRLDistributionPointsExtension := nil;
    FreeAndNil(FASN1Compiler);
    FreeAndNil(FIssuerDN);
    FreeAndNil(FSubjectDN);
    FreeAndNil(FExtensions);
    FreeAndNil(FSignatureAlgorithm);
    FreeAndNil(FKey);
    FreeAndNil(FOrigKey);
    FIsSelfSigned := -1;
  finally
    unlock(FLock);
  end;
end;

function TScCertificate.GetRawData: TBytes;
begin
  Result := FRawData;
end;

procedure TScCertificate.InternalAssign(Source: TScStorageItem);
begin
  if not IsClass(Source, TScCertificate) then
    raise EScError.CreateFmt(SInvalidObjectClass, [TScCertificate.ClassName, Source.ClassName], seInvalidObjectClass);

  SetRawData(nil, nil, TScCertificate(Source).FRawData);
  FReady := Source.FReady;
end;

class function TScCertificate.ParsePEMString(const Cert, Delimiter: string): string;
var
  BeginPos, EndPos: integer;
  BeginStr, EndStr: string;
begin
  Result := '';

  BeginStr := '-----BEGIN ' + Delimiter + '-----';
  BeginPos := Pos(BeginStr, Cert);
  if BeginPos = 0 then
    Exit;

  EndStr := '-----END ' + Delimiter + '-----';
  EndPos := PosEx(EndStr, Cert, BeginPos);
  if EndPos = 0 then
    EndPos := Length(Cert);

  BeginPos := BeginPos + Length(BeginStr);
  Result := Trim(Copy(Cert, BeginPos, EndPos - BeginPos));
end;

procedure TScCertificate.SetHandle(Handle: IntPtr; OnFreeHandle: TScFreeHandleEvent);
begin
  if Assigned(FHandle) and Assigned(FOnFreeHandle) then
    FOnFreeHandle(FHandle);

  FHandle := Handle;
  FOnFreeHandle := OnFreeHandle;
end;

procedure TScCertificate.SetRawData(Handle: IntPtr; OnFreeHandle: TScFreeHandleEvent; const RawData: TBytes);
begin
  lock(FLock);
  try
    Ready := False;

    FHandle := Handle;
    FOnFreeHandle := OnFreeHandle;
    FRawData := RawData;

    Assert(FASN1Compiler = nil);
    FASN1Compiler := TScASN1Compiler.Create;
    try
      if (FRawData = nil) or not FASN1Compiler.Parse(asn1_CERTIFICATE_DESC, FRawData) then
        raise EScError.Create(seWrongCertContext);
    except
      FreeAndNil(FASN1Compiler);
      raise;
    end;

    FReady := True;
  finally
    unlock(FLock);
  end;
end;

class procedure TScCertificate.GetSignAndPadAlg(SignatureAlgorithm: TScSignatureAlgorithmIdentifier;
  out HashAlgorithm: TScHashAlgorithm; out PaddingMode: TScPaddingMode);
begin
  PaddingMode := SignatureAlgorithm.PaddingMode;
  if PaddingMode = pmPSS then
    HashAlgorithm := SignatureAlgorithm.PSSParams.HashAlgorithm
  else
    HashAlgorithm := SignatureAlgorithm.HashAlgorithm;
end;

function TScCertificate.CheckSignature(ParentCertificate: TScCertificate): boolean;
var
  TBSData, Sign: TBytes;
  HashAlgorithm: TScHashAlgorithm;
  PaddingMode: TScPaddingMode;
begin
  Assert(ParentCertificate <> nil);

  GetSignAndPadAlg(GetSignatureAlgorithm, HashAlgorithm, PaddingMode);
  ParentCertificate.Key.PSSParams.Assign(GetSignatureAlgorithm.PSSParams);

  TBSData := FASN1Compiler['TBSCertificate'].EncodedData;
  Sign := FASN1Compiler['SignatureValue'].AsBytes;
  Result := ParentCertificate.Key.VerifySign(TBSData, Sign, HashAlgorithm, PaddingMode);
end;

function TScCertificate.CheckPolicies(ChildPolicies, ParentPolicies: TScCertPoliciesExtension): boolean;
var
  Identifier: string;
  Found: boolean;
  i, j: integer;
begin
  Result := False;
  if (ChildPolicies = nil) or (ParentPolicies = nil) then
    Exit;

  for j := 0 to ParentPolicies.Policies.Count - 1 do
    if ParentPolicies.Policies[j].Identifier = OID_CE_CERTIFICATE_ANY_POLICY then begin
      Result := True;
      Exit;
    end;

  for i := 0 to ChildPolicies.Policies.Count - 1 do begin
    Identifier := ChildPolicies.Policies[i].Identifier;
    Found := False;
    for j := 0 to ParentPolicies.Policies.Count - 1 do begin
      if Identifier = ParentPolicies.Policies[j].Identifier then begin
        Found := True;
        break;
      end;
    end;
    if not Found then
      Exit;
  end;

  Result := True;
end;

procedure TScCertificate.CheckCRLDistributionPointsExtension;
begin
  if FCRLDistributionPointsExtension = nil then
    FCRLDistributionPointsExtension := TScCertCRLDistributionPointsExtension(Extensions.FindExtensionByClass(TScCertCRLDistributionPointsExtension));
end;

procedure TScCertificate.VerifyCertificateChain(OnObtainCRL: TScObtainCRLEvent;
  ParentCertificate: TScCertificate; var StatusSet: TScCertificateStatusSet);
var
  dt: TDateTime;
  Ext: TScCertificateExtension;
  ExtClassType: TClass;
  PoliciesExtension: TScCertPoliciesExtension;
  CAExists: boolean;
  HashAlgorithm: TScHashAlgorithm;
  PaddingMode: TScPaddingMode;
  i: integer;
begin
  dt := Now;
  if (dt < GetNotBefore) or (dt > GetNotAfter) then
    Include(StatusSet, csExpired);

  if ParentCertificate = nil then begin
    if GetIsSelfSigned then begin
      Exclude(StatusSet, csUntrustedRoot);
      Exit;
    end;
  end
  else begin
    if not IssuerName.Equals(ParentCertificate.SubjectName) then
      Include(StatusSet, csIssuerNotEqualSubject);

    PoliciesExtension := TScCertPoliciesExtension(Extensions.FindExtensionByClass(TScCertPoliciesExtension));
    if (PoliciesExtension <> nil) and not PoliciesExtension.Critical then
      PoliciesExtension := nil;

    CAExists := False;
    for i := 0 to ParentCertificate.Extensions.Count - 1 do begin
      Ext := ParentCertificate.Extensions[i];
      if IsClass(Ext, TScCertBasicConstraintsExtension) then begin
        CAExists := True;
        if not TScCertBasicConstraintsExtension(Ext).CertificateAuthority then
          Include(StatusSet, csInvalidBasicConstraints);
      end
      else
      if IsClass(Ext, TScCertKeyUsageExtension) then begin
        if not (kfKeyCertSign in TScCertKeyUsageExtension(Ext).KeyUsages) then
          Include(StatusSet, csInvalidKeyUsage);
      end
      else
      if IsClass(Ext, TScCertPoliciesExtension) then begin
        if (PoliciesExtension <> nil) and not CheckPolicies(PoliciesExtension, TScCertPoliciesExtension(Ext)) then
          Include(StatusSet, csInvalidPolicies);
        PoliciesExtension := nil;
      end;
    end;

    if not CAExists then
      Include(StatusSet, csInvalidBasicConstraints);

    if PoliciesExtension <> nil then
      if not ParentCertificate.GetIsSelfSigned and not ParentCertificate.IssuerName.Equals(ParentCertificate.SubjectName) then // is Root certificate
        Include(StatusSet, csInvalidPolicies);

    if not CheckSignature(ParentCertificate) then
      Include(StatusSet, csInvalidSignature);
  end;

  if Key.Algorithm = aaRSA then begin
    GetSignAndPadAlg(GetSignatureAlgorithm, HashAlgorithm, PaddingMode);
    if HashAlgorithm in [haNone, haMD5, haMD4, haMD2] then
      Include(StatusSet, csForbiddenSignature)
    else
    if HashAlgorithm = haSHA1 then
      Include(StatusSet, csInsecureSignature);
  end;

  if Assigned(OnObtainCRL) then
    VerifyCRLRevocation(OnObtainCRL, ParentCertificate, StatusSet);

  for i := 0 to Extensions.Count - 1 do begin
    Ext := Extensions[i];
    ExtClassType := Ext.ClassType;
    if Ext.Critical and
      ((ExtClassType <> TScCertBasicConstraintsExtension) and
       (ExtClassType <> TScCertKeyUsageExtension) and
       (ExtClassType <> TScCertExtendedKeyUsageExtension) and
       (ExtClassType <> TScCertSubjectAlternativeNameExtension) and
       (ExtClassType <> TScCertPoliciesExtension) and
       (ExtClassType <> TScCertCRLDistributionPointsExtension))
    then
      Include(StatusSet, csUnknownCriticalExtension);
  end;
end;

procedure TScCertificate.VerifyCRLRevocation(OnObtainCRL: TScObtainCRLEvent;
  ParentCertificate: TScCertificate; var StatusSet: TScCertificateStatusSet);
var
  CertFreshestCRLExt, CRLFreshestCRLExt: TScCertFreshestCRLExtension;
  DistributionPoint, DeltaDistributionPoint: TScCRLDistributionPoint;
  CRL, FirstCRL: TScCRL;
  AReason: TScCRLReason;
  i, j: integer;
begin
  // https://tools.ietf.org/html/rfc5280#section-6.3.3

  if not Assigned(OnObtainCRL) then
    raise EScError.Create(seInvalidInputArgs);

  CheckCRLDistributionPointsExtension;
  if FCRLDistributionPointsExtension = nil then
    Exit;

  CertFreshestCRLExt := TScCertFreshestCRLExtension(Extensions.FindExtensionByClass(TScCertFreshestCRLExtension));
  FCRLReason := TScCRLReason(-1);
  FirstCRL := nil;

  for i := 0 to FCRLDistributionPointsExtension.CRLDistributionPoints.Count - 1 do begin
    FCRLReason := TScCRLReason(-1);
    DistributionPoint := FCRLDistributionPointsExtension.CRLDistributionPoints[i];

    if DistributionPoint.DistributionPointName.Count = 0 then
      Continue;

    CRL := nil;
    try
      OnObtainCRL(Self, DistributionPoint.DistributionPointName, False, CRL);
    except
      Continue;
    end;

    if CRL = nil then
      Continue;

    if Now > CRL.NextUpdate then begin
      try
        OnObtainCRL(Self, DistributionPoint.DistributionPointName, True, CRL);
        // try updating, but if not updated, use delta CRL
      except
        Continue;
      end;

      if CRL = nil then
        Continue;
    end;

    FirstCRL := CRL;

    if not CRL.CheckCompliance(Self, DistributionPoint) then
      Include(StatusSet, csCRLIsNotValid);

    if (CertFreshestCRLExt <> nil) and (CertFreshestCRLExt.CRLDistributionPoints.Count > i) then begin
      DeltaDistributionPoint := CertFreshestCRLExt.CRLDistributionPoints[i];
      if DeltaDistributionPoint.DistributionPointName.Count = 0 then begin
        Include(StatusSet, csCRLIsNotValid);
        Continue;
      end;

      VerifyCertificateByDeltaCRL(OnObtainCRL, DeltaDistributionPoint.DistributionPointName, CRL, ParentCertificate, StatusSet);
    end;

    CRLFreshestCRLExt := TScCertFreshestCRLExtension(CRL.Extensions.FindExtensionByClass(TScCertFreshestCRLExtension));
    if CRLFreshestCRLExt <> nil then begin
      for j := 0 to CRLFreshestCRLExt.CRLDistributionPoints.Count - 1 do begin
        DeltaDistributionPoint := CRLFreshestCRLExt.CRLDistributionPoints[j];
        if DeltaDistributionPoint.DistributionPointName.Count = 0 then begin
          Include(StatusSet, csCRLIsNotValid);
          Continue;
        end;

        VerifyCertificateByDeltaCRL(OnObtainCRL, DeltaDistributionPoint.DistributionPointName, CRL, ParentCertificate, StatusSet);
      end;
    end;

    if ParentCertificate <> nil then
      CRL.VerifyCRLChain(ParentCertificate, StatusSet);

    if CRL.FindCertificate(Self, AReason) then
      if FCRLReason <> crRemoveFromCRL then
        FCRLReason := AReason;

    if (FCRLReason <> TScCRLReason(-1)) and (FCRLReason <> crRemoveFromCRL) then begin
      Include(StatusSet, csCertificateIsRevoked);
      Exit;
    end;
  end;

  if (FirstCRL = nil) and (FCRLDistributionPointsExtension.CRLDistributionPoints.Count > 0) then
    Include(StatusSet, csCRLNotFound);
end;

procedure TScCertificate.VerifyCertificateByDeltaCRL(OnObtainCRL: TScObtainCRLEvent;
  DistributionPointName: TScGeneralNames; CRL: TScCRL; ParentCertificate: TScCertificate;
  var StatusSet: TScCertificateStatusSet);
var
  DeltaCRL: TScCRL;
  AReason: TScCRLReason;
begin
  if (CRL = nil) or not Assigned(OnObtainCRL) then
    raise EScError.Create(seInvalidInputArgs);

  DeltaCRL := nil;
  try
    OnObtainCRL(Self, DistributionPointName, False, DeltaCRL);
  except
    Exit;
  end;

  if DeltaCRL = nil then
    Exit;

  if Now > DeltaCRL.NextUpdate then begin
    try
      OnObtainCRL(Self, DistributionPointName, True, DeltaCRL);
    except
      Include(StatusSet, csCRLNotFound);
      Exit;
    end;
  end;

  if DeltaCRL = nil then begin
    Include(StatusSet, csCRLNotFound);
    Exit;
  end;

  if Now > DeltaCRL.NextUpdate then
    Include(StatusSet, csCRLIsNotValid);

  if not CRL.CheckCompliance(DeltaCRL) then
    Include(StatusSet, csCRLIsNotValid);

  if ParentCertificate <> nil then
    DeltaCRL.VerifyCRLChain(ParentCertificate, StatusSet);

  if DeltaCRL.FindCertificate(Self, AReason) then
    if FCRLReason <> crRemoveFromCRL then
      FCRLReason := AReason;
end;

procedure TScCertificate.ImportFrom(const FileName: string; const Password: string = '');
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    ImportFrom(Stream, Password);
  finally
    Stream.Free;
  end;
end;

procedure TScCertificate.ImportFrom(Stream: TStream; const Password: string = '');
var
  Data, DecodedData: TBytes;
  DataStr, ContentStr: string;
begin
  SetLength(Data, Stream.Size - Stream.Position);
  if Length(Data) > 0 then
    Stream.Read(Data[0], Length(Data));

  DataStr := Encoding.ANSI.GetString(Data);
  ContentStr := ParsePEMString(DataStr, 'CERTIFICATE');
  if ContentStr = '' then
    ContentStr := ParsePEMString(DataStr, 'X509 CERTIFICATE');
  if ContentStr = '' then
    ContentStr := ParsePEMString(DataStr, 'TRUSTED CERTIFICATE');

  if ContentStr <> '' then
    DecodedData := TBase64.Decode(Encoding.Default.GetBytes(ContentStr))
  else
    DecodedData := Data;

  SetRawData(nil, nil, DecodedData);

  SaveToStorageIfPossible;
end;

procedure TScCertificate.ExportTo(const FileName: string; const CertEncoding: TScCertificateEncoding = cfPEM);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    ExportTo(Stream, CertEncoding);
  finally
    Stream.Free;
  end;
end;

procedure TScCertificate.ExportTo(Stream: TStream; const CertEncoding: TScCertificateEncoding = cfPEM);
begin
  if not Ready then
    raise EScError.Create(seCertificateNotReady);

  if CertEncoding = cfPEM then begin
    TStreamUtils.WriteLine(Stream, CERT_HEADER);
    WriteBlockInPEMStyle(Stream, TBase64.Encode(FRawData), 64, False);
    TStreamUtils.WriteLine(Stream, CERT_FOOTER);
  end
  else
    Stream.WriteBuffer(FRawData[0], Length(FRawData));
end;

function TScCertificate.Equals(Item: TScStorageItem): boolean;
var
  Certificate: TScCertificate;
begin
  Result := IsClass(Item, TScCertificate);
  if not Result then
    Exit;

  Certificate := TScCertificate(Item);
  CheckAvailable;
  Certificate.CheckAvailable;

  Result := Length(FRawData) = Length(Certificate.FRawData);
  if Result then begin
    Result := MemCompare(@FRawData[0], @Certificate.FRawData[0], Length(FRawData)) = 0;
    if Result then
      Result := Certificate.Key.IsPrivate = Key.IsPrivate;
  end;
end;

function TScCertificate.Sign(const Data: TBytes; HashAlg: TScHashAlgorithm = haSHA1; Padding: TScPaddingMode = pmPKCS1): TBytes;
begin
  CheckAvailable;
  Result := Key.Sign(Data, HashAlg, Padding);
end;

function TScCertificate.SignHash(const Hash: TBytes; HashAlg: TScHashAlgorithm = haSHA1; Padding: TScPaddingMode = pmPKCS1): TBytes;
begin
  CheckAvailable;
  Result := Key.SignHash(Hash, HashAlg, Padding);
end;

function TScCertificate.VerifySign(const Data, Sign: TBytes; HashAlg: TScHashAlgorithm = haSHA1; Padding: TScPaddingMode = pmPKCS1): boolean;
begin
  CheckAvailable;
  Result := Key.VerifySign(Data, Sign, HashAlg, Padding);
end;

function TScCertificate.VerifyHashSign(const Hash, Sign: TBytes; HashAlg: TScHashAlgorithm = haSHA1; Padding: TScPaddingMode = pmPKCS1): boolean;
begin
  CheckAvailable;
  Result := Key.VerifyHashSign(Hash, Sign, HashAlg, Padding);
end;

function TScCertificate.Encrypt(const Data: TBytes): TBytes;
begin
  CheckAvailable;
  Result := Key.Encrypt(Data);
end;

function TScCertificate.Decrypt(const Data: TBytes): TBytes;
begin
  CheckAvailable;
  Result := Key.Decrypt(Data);
end;

procedure TScCertificate.GetFingerprint(const HashAlg: TScHashAlgorithm; out Fingerprint: TBytes);
var
  ha: THashAlgorithm;
begin
  CheckAvailable;

  ha := CipherFactory.CreateHash(HashAlg);
  try
    Fingerprint := ha.ComputeHash(FRawData);
  finally
    ha.Free;
  end;
end;

procedure TScCertificate.GetFingerprint(const HashAlg: TScHashAlgorithm; out Fingerprint: string);
var
  FPData: TBytes;
begin
  GetFingerprint(HashAlg, FPData);
  Fingerprint := BytesToHexStr(FPData, ':');
end;

procedure TScCertificate.CheckIssuerName;
begin
  CheckAvailable;

  lock(FLock);
  try
    if FIssuerDN = nil then begin
      FIssuerDN := TScDistinguishedName.Create;
      TScASNUtils.Parse(FIssuerDN, FASN1Compiler['Issuer']);
    end;
  finally
    unlock(FLock);
  end;
end;

procedure TScCertificate.CheckSubjectName;
begin
  CheckAvailable;

  lock(FLock);
  try
    if FSubjectDN = nil then begin
      FSubjectDN := TScDistinguishedName.Create;
      TScASNUtils.Parse(FSubjectDN, FASN1Compiler['Subject']);
    end;
  finally
    unlock(FLock);
  end;
end;

function TScCertificate.GetIssuerName: TScDistinguishedName;
begin
  CheckIssuerName;
  Result := FIssuerDN;
end;

function TScCertificate.GetSubjectName: TScDistinguishedName;
begin
  CheckSubjectName;
  Result := FSubjectDN;
end;

class function TScCertificate.GetDisplayName(DN: TScDistinguishedName): string;
var
  idx: integer;
begin
  if DN.Count = 0 then begin
    Result := '';
    Exit;
  end;

  // MSDN - CertGetNameString function
  idx := DN.IndexOfOId(OID_COMMON_NAME);
  if idx < 0 then begin
    idx := DN.IndexOfOId(OID_ORGANIZATIONAL_UNIT_NAME);
    if idx < 0 then begin
      idx := DN.IndexOfOId(OID_ORGANIZATION_NAME);
      if idx < 0 then begin
        idx := DN.IndexOfOId(OID_RSA_emailAddr);
        if idx < 0 then
          idx := 0;
      end;
    end;
  end;

  Result := DN.ValueFromIndex[idx];
end;

function TScCertificate.GetIssuer: string;
begin
  CheckIssuerName;
  Result := GetDisplayName(FIssuerDN);
end;

function TScCertificate.GetSubject: string;
begin
  CheckSubjectName;
  Result := GetDisplayName(FSubjectDN);
end;

function TScCertificate.GetNotAfter: TDateTime;
begin
  CheckAvailable;
  Result := FASN1Compiler['NotAfter'].AsDateTime;
end;

function TScCertificate.GetNotBefore: TDateTime;
begin
  CheckAvailable;
  Result := FASN1Compiler['NotBefore'].AsDateTime;
end;

function TScCertificate.GetSerialNumber: string;
begin
  CheckAvailable;
  Result := FASN1Compiler['SerialNumber'].AsString;
end;

function TScCertificate.GetSignatureAlgorithm: TScSignatureAlgorithmIdentifier;
begin
  CheckAvailable;

  lock(FLock);
  try
    if FSignatureAlgorithm = nil then begin
      FSignatureAlgorithm := TScSignatureAlgorithmIdentifier.Create;
      TScASNUtils.Parse(FSignatureAlgorithm, FASN1Compiler['Signature']);
    end;
  finally
    unlock(FLock);
  end;

  Result := FSignatureAlgorithm;
end;

function TScCertificate.GetSignature: TBytes;
begin
  CheckAvailable;
  Result := FASN1Compiler['SignatureValue'].AsBytes;
end;

function TScCertificate.GetVersion: integer;
begin
  CheckAvailable;
  Result := FASN1Compiler['Version'].AsInteger + 1;
end;

procedure TScCertificate.CheckExtensions;
begin
  CheckAvailable;

  lock(FLock);
  try
    if FExtensions = nil then begin
      FExtensions := TScExtensions.Create;
      TScASNUtils.Parse(FExtensions, FASN1Compiler['Extensions']);
    end;
  finally
    unlock(FLock);
  end;
end;

function TScCertificate.GetExtensions: TScExtensions;
begin
  CheckExtensions;
  Result := FExtensions;
end;

function TScCertificate.GetSubjectKeyIdentifier: string;
var
  Extension: TScCertificateExtension;
  i: integer;
begin
  CheckExtensions;

  Result := '';
  for i := 0 to FExtensions.Count - 1 do begin
    Extension := FExtensions[i];
    if IsClass(Extension, TScCertSubjectKeyIdExtension) then begin
      Result := TScCertSubjectKeyIdExtension(Extension).SubjectKeyIdentifier;
      Exit;
    end;
  end;
end;

function TScCertificate.GetKey: TScKey;
var
  AlgOID: string;
  SubjectPublicKeyInfoLexem: TScLexemInfo;
begin
  lock(FLock);
  try
    CheckAvailable;

    if (FKey <> nil) and not FKey.Ready then begin
      FreeAndNil(FKey);
      FreeAndNil(FOrigKey);
    end;

    if FKey = nil then begin
      FKey := TScKey.Create(nil);
      FOrigKey := TScKey.Create(nil);
      FKey.RegisterClient(Self);

      SubjectPublicKeyInfoLexem := FASN1Compiler['SubjectPublicKeyInfo'];
      AlgOID := SubjectPublicKeyInfoLexem['Algorithm']['Algorithm'].AsOID;

      if (AlgOID = OID_RSA_ENCRYPTION) or (AlgOID = OID_PKCS1) then
        FKey.LoadKeyFromCryptoAPIFormat(SubjectPublicKeyInfoLexem['SubjectPublicKey'].AsBytes, kefDefault, aaRSA, True)
      else
      if AlgOID = OID_DSA_ENCRYPTION then
        FKey.DecodeDSAKeyFromCertificate(SubjectPublicKeyInfoLexem['SubjectPublicKey'].AsBytes, SubjectPublicKeyInfoLexem['Algorithm']['Parameters'].EncodedData)
      else
      if AlgOID = OID_EC_PUBLIC_KEY then
        FKey.DecodeECKeyFromCertificate(SubjectPublicKeyInfoLexem['SubjectPublicKey'].AsBytes, SubjectPublicKeyInfoLexem['Algorithm']['Parameters'].EncodedData)
      else
      if AlgOID = OID_X25519 then
        FKey.DecodeX25519KeyFromCertificate(SubjectPublicKeyInfoLexem['SubjectPublicKey'].AsBytes, phPure)
      else
      if AlgOID = OID_Ed25519 then
        FKey.DecodeX25519KeyFromCertificate(SubjectPublicKeyInfoLexem['SubjectPublicKey'].AsBytes, phHash)
      else
        raise EScError.Create(seInvalidPublicKeyAlgorithm);

      FOrigKey.Assign(FKey);
    end;
  finally
    unlock(FLock);
  end;

  Result := FKey;
end;

function TScCertificate.GetIsSelfSigned: boolean;
begin
  CheckAvailable;

  if FIsSelfSigned = -1 then begin
    try
      if IssuerName.Equals(SubjectName) and CheckSignature(Self) then
        FIsSelfSigned := 1
      else
        FIsSelfSigned := 0;
    except
      FIsSelfSigned := 0;
    end;
  end;

  Result := FIsSelfSigned = 1;
end;

procedure TScCertificate.KeyChanged;
begin
  if FKey <> nil then begin
    try
      Assert(FOrigKey <> nil);
      if not FKey.IsPrivate then
        raise EScError.Create(seCertKeyCannotBeChanged);
      if not FKey.Equals(FOrigKey) then
        raise EScError.Create(seCertKeyCannotBeChanged);

      if not FOrigKey.IsPrivate then
        FOrigKey.InternalAssign(FKey);
    except
      FKey.InternalAssign(FOrigKey);
      raise;
    end;
  end;
end;

function TScCertificate.GetCertificateList: TScCertificateList;
begin
  Result := TScCertificateList(StorageList);
end;

procedure TScCertificate.SetCertificateList(Value: TScCertificateList);
begin
  StorageList := Value;
end;

{ TScCertificateList }

function TScCertificateList.GetCertificate(Index: integer): TScCertificate;
begin
  Result := TScCertificate(GetItem(Index));
end;

procedure TScCertificateList.SetCertificate(Index: integer; Value: TScCertificate);
begin
  SetItem(Index, Value);
end;

procedure TScCertificateList.CheckCertificateName(const CertName: string);
begin
  CheckItemName(CertName);
end;

function TScCertificateList.FindCertificate(const CertName: string): TScCertificate;
begin
  Result := TScCertificate(FindItem(CertName));
end;

function TScCertificateList.CertificateByName(const CertName: string): TScCertificate;
begin
  Result := TScCertificate(ItemByName(CertName));
end;

procedure TScCertificateList.GetCertificateNames(List: TStrings);
begin
  GetItemNames(List);
end;

function TScCertificateList.FindCertificateBySubject(DistinguishedName: TScDistinguishedName): TScCertificate;
var
  i: integer;
begin
  lock(FLock);
  try
    for i := 0 to FList.Count - 1 do begin
      Result := Certificates[i];
      if DistinguishedName.Equals(Result.SubjectName) then
        Exit;
    end;

    Result := nil;
  finally
    unlock(FLock);
  end;
end;

function TScCertificateList.GetItemClassType: TScStorageItemClass;
begin
  Result := TScCertificate;
end;

function TScCertificateList.GetItemDefName: string;
begin
  Result := 'crt';
end;

{ TScCertificateChain }

class procedure TScCertificateChain.VerifyChain(OnObtainCRL: TScObtainCRLEvent;
  CertificateList: TCRList; out StatusSet: TScCertificateStatusSet);
var
  i: integer;
begin
  StatusSet := [csUntrustedRoot];
  if CertificateList.Count = 0 then
    Exit;

  for i := 0 to CertificateList.Count - 2 do
    TScCertificate(CertificateList[i]).VerifyCertificateChain(OnObtainCRL, TScCertificate(CertificateList[i + 1]), StatusSet);

  TScCertificate(CertificateList[CertificateList.Count - 1]).VerifyCertificateChain(OnObtainCRL, nil, StatusSet);
end;

{ TScRevokedCertificate }

constructor TScRevokedCertificate.Create;
begin
  inherited Create;

  FExtensions := TScExtensions.Create;
end;

destructor TScRevokedCertificate.Destroy;
begin
  FExtensions.Free;

  inherited;
end;

function TScRevokedCertificate.GetExtensions: TScExtensions;
var
  ASN1Compiler: TScASN1Compiler;
begin
  if Length(FExtensionsEncodedData) > 0 then begin
    ASN1Compiler := TScASN1Compiler.Create;
    try
      if not ASN1Compiler.Parse(asn1_EXTENSIONS_DESC, FExtensionsEncodedData) then
        raise EScError.Create(seWrongDataFormat);
      TScASNUtils.Parse(FExtensions, ASN1Compiler.Root);
      SetLength(FExtensionsEncodedData, 0);
    finally
      ASN1Compiler.Free;
    end;
  end;

  Result := FExtensions;
end;

function TScRevokedCertificate.Clone: TScPersistent;
begin
  Result := TScRevokedCertificate.Create;
  Result.Assign(Self);
end;

procedure TScRevokedCertificate.Assign(Source: TScPersistent);
var
  Src: TScRevokedCertificate;
begin
  if (Source = nil) or not IsClass(Source, TScRevokedCertificate) then
    RaiseAssignError(Source);

  if Self <> Source then begin
    Src := TScRevokedCertificate(Source);
    FSerialNumber := Src.FSerialNumber;
    FRevocationDate := Src.FRevocationDate;
    FExtensions.Assign(Src.FExtensions);
  end;
end;

{ TScRevokedCertificates }

function TScRevokedCertificates.GetItemClassType: TScPersistentClass;
begin
  Result := TScRevokedCertificate;
end;

function TScRevokedCertificates.GetRevokedCertificate(Index: integer): TScRevokedCertificate;
begin
  Result := TObject(Items[Index]) as TScRevokedCertificate;
end;

procedure TScRevokedCertificates.SetRevokedCertificate(Index: integer; Item: TScRevokedCertificate);
begin
  Items[Index] := Item;
end;

{ TScCRL }

constructor TScCRL.Create(AStorageList: TScStorageList = nil);
begin
  inherited Create(AStorageList);
end;

procedure TScCRL.FreeData;
begin
  lock(FLock);
  try
    FreeAndNil(FSignatureAlgorithm);
    FreeAndNil(FIssuerDN);
    FreeAndNil(FExtensions);
    FreeAndNil(FRevokedCertificates);
    FreeAndNil(FRevokedSN);

    FreeAndNil(FASN1Compiler);
  finally
    unlock(FLock);
  end;
end;

procedure TScCRL.InternalAssign(Source: TScStorageItem);
begin
  if not IsClass(Source, TScCRL) then
    raise EScError.CreateFmt(SInvalidObjectClass, [TScCRL.ClassName, Source.ClassName], seInvalidObjectClass);

  lock(FLock);
  try
    Assert(FASN1Compiler = nil);

    FRawData := TScCRL(Source).FRawData;

    try
      FSignatureAlgorithm := TScSignatureAlgorithmIdentifier.Create;
      FSignatureAlgorithm.Assign(TScCRL(Source).FSignatureAlgorithm);

      FIssuerDN := TScDistinguishedName.Create;
      FIssuerDN.Assign(TScCRL(Source).FIssuerDN);

      FIsPartialParsed := True;
      FASN1Compiler := TScASN1Compiler.Create;

      if (FRawData = nil) or not FASN1Compiler.Parse(asn1_PARTIAL_CERTIFICATE_LIST_DESC, FRawData) then
        raise EScError.Create(seWrongCRLContext);
    except
      FreeAndNil(FASN1Compiler);
      FreeAndNil(FSignatureAlgorithm);
      FreeAndNil(FIssuerDN);
      raise;
    end;

    FReady := Source.FReady;
  finally
    unlock(FLock);
  end;
end;

function TScCRL.Equals(Item: TScStorageItem): boolean;
var
  CRL: TScCRL;
begin
  Result := IsClass(Item, TScCRL);
  if not Result then
    Exit;

  CRL := TScCRL(Item);
  CheckAvailable;
  CRL.CheckAvailable;

  Result := Length(FRawData) = Length(CRL.FRawData);
  if Result then
    Result := MemCompare(@FRawData[0], @CRL.FRawData[0], Length(FRawData)) = 0;
end;

function TScCRL.CheckCompliance(Cert: TScCertificate; CertDistributionPoint: TScCRLDistributionPoint): boolean;
var
  CertDPName, CrlDPName: TScGeneralName;
  CrlIDPExt: TScCRLIssuingDistributionPointExtension;
  BasicConstraintsExt: TScCertBasicConstraintsExtension;
  NameFound: boolean;
  i, j, k: integer;
begin
  // https://tools.ietf.org/html/rfc5280#section-6.3.3
  // paragraph (b)

  if (Cert = nil) or (CertDistributionPoint = nil) then
    raise EScError.Create(seInvalidInputArgs);

  CheckExtensions;
  Result := False;

  CrlIDPExt := TScCRLIssuingDistributionPointExtension(FExtensions.FindExtensionByClass(TScCRLIssuingDistributionPointExtension));
  if CrlIDPExt = nil then begin
    if not Cert.IssuerName.Equals(FIssuerDN) then
      Exit;

    Result := True;
    Exit;
  end;

  // paragraph (b)(1) - Check CRLIssuer
  if CrlIDPExt.IndirectCRL and (CertDistributionPoint.CRLIssuer.Count > 0) then begin
    if not CertDistributionPoint.CRLIssuer[0].DirectoryName.Equals(FIssuerDN) then
      Exit;
  end
  else
    if not Cert.IssuerName.Equals(FIssuerDN) then
      Exit;

  // paragraph (b)(2)(ii) - Check OnlyContainsUserCerts
  if CrlIDPExt.OnlyContainsUserCerts then begin
    BasicConstraintsExt := TScCertBasicConstraintsExtension(Cert.Extensions.FindExtensionByClass(TScCertBasicConstraintsExtension));
    if (BasicConstraintsExt <> nil) and BasicConstraintsExt.CertificateAuthority then
      Exit;
  end;

  // paragraph (b)(2)(iii) - Check OnlyContainsCACerts
  if CrlIDPExt.OnlyContainsCACerts then begin
    BasicConstraintsExt := TScCertBasicConstraintsExtension(Cert.Extensions.FindExtensionByClass(TScCertBasicConstraintsExtension));
    if (BasicConstraintsExt = nil) or not BasicConstraintsExt.CertificateAuthority then
      Exit;
  end;

  // paragraph (b)(2)(iv)
  if CrlIDPExt.OnlyContainsAttributeCerts then
    Exit;

  // paragraph (b)(2)(i) - Check DistributionPointName
  NameFound := False;
  for i := 0 to CrlIDPExt.DistributionPointName.Count - 1 do begin
    CrlDPName := CrlIDPExt.DistributionPointName[i];

    for j := 0 to Cert.FCRLDistributionPointsExtension.CRLDistributionPoints.Count - 1 do begin
      CertDistributionPoint := Cert.FCRLDistributionPointsExtension.CRLDistributionPoints[i];

      if CertDistributionPoint.DistributionPointName.Count > 0 then begin
        for k := 0 to CertDistributionPoint.DistributionPointName.Count - 1 do begin
          CertDPName := CertDistributionPoint.DistributionPointName[k];
          if SameText(CrlDPName.Value, CertDPName.Value) then begin
            NameFound := True;
            break;
          end;
        end;
      end
      else
        for k := 0 to CertDistributionPoint.CRLIssuer.Count - 1 do begin
          CertDPName := CertDistributionPoint.CRLIssuer[k];
          if SameText(CrlDPName.Value, CertDPName.Value) then begin
            NameFound := True;
            break;
          end;
        end;

      if NameFound then
        break;
    end;

    if NameFound then
      break;
  end;

  if not NameFound and (CrlIDPExt.DistributionPointName.Count > 0) then
    Exit;

  Result := True;
end;

function TScCRL.CheckCompliance(DeltaCRL: TScCRL): boolean;
var
  CrlIDPExt, DeltaCrlIDPExt: TScCRLIssuingDistributionPointExtension;
  AuthKeyIdExt, DeltaAuthKeyIdExt: TScCertAuthorityKeyIdExtension;
begin
  // https://tools.ietf.org/html/rfc5280#section-6.3.3
  // paragraph (c)

  if DeltaCRL = nil then
    raise EScError.Create(seInvalidInputArgs);

  CheckExtensions;
  DeltaCRL.CheckExtensions;
  Result := False;

  if not FIssuerDN.Equals(DeltaCRL.FIssuerDN) then
    Exit;

  CrlIDPExt := TScCRLIssuingDistributionPointExtension(FExtensions.FindExtensionByClass(TScCRLIssuingDistributionPointExtension));
  DeltaCrlIDPExt := TScCRLIssuingDistributionPointExtension(DeltaCRL.FExtensions.FindExtensionByClass(TScCRLIssuingDistributionPointExtension));

  if (CrlIDPExt <> nil) or (DeltaCrlIDPExt <> nil) then begin
    if (CrlIDPExt = nil) or (DeltaCrlIDPExt = nil) then
      Exit;

    if not CrlIDPExt.DistributionPointName.Equals(DeltaCrlIDPExt.DistributionPointName) or
      (CrlIDPExt.OnlySomeReasons <> DeltaCrlIDPExt.OnlySomeReasons) or
      (CrlIDPExt.OnlyContainsUserCerts <> DeltaCrlIDPExt.OnlyContainsUserCerts) or
      (CrlIDPExt.OnlyContainsCACerts <> DeltaCrlIDPExt.OnlyContainsCACerts) or
      (CrlIDPExt.IndirectCRL <> DeltaCrlIDPExt.IndirectCRL) or
      (CrlIDPExt.OnlyContainsAttributeCerts <> DeltaCrlIDPExt.OnlyContainsAttributeCerts)
    then
      Exit;
  end;

  AuthKeyIdExt := TScCertAuthorityKeyIdExtension(FExtensions.FindExtensionByClass(TScCertAuthorityKeyIdExtension));
  DeltaAuthKeyIdExt := TScCertAuthorityKeyIdExtension(DeltaCRL.FExtensions.FindExtensionByClass(TScCertAuthorityKeyIdExtension));

  if (AuthKeyIdExt <> nil) or (DeltaAuthKeyIdExt <> nil) then begin
    if (AuthKeyIdExt = nil) or (DeltaAuthKeyIdExt = nil) then
      Exit;

    if not SameText(AuthKeyIdExt.KeyIdentifier, DeltaAuthKeyIdExt.KeyIdentifier) or
      not SameText(AuthKeyIdExt.CertSerialNumber, DeltaAuthKeyIdExt.CertSerialNumber) or
      not AuthKeyIdExt.CertIssuers.Equals(DeltaAuthKeyIdExt.CertIssuers)
    then
      Exit;
  end;

  Result := True;
end;

function TScCRL.FindCertificate(Cert: TScCertificate; out Reason: TScCRLReason): boolean;
var
  ASN1Compiler: TScASN1Compiler;
  RevokedCertificate: TScRevokedCertificate;
  CertIssuerExt, TmpCertIssuerExt: TScCRLCertificateIssuerExtension;
  S, CertSerialNumber: string;
  ReasonCodeExt: TScCRLReasonCodeExtension;
  RevokedCertificatesLexem, RevokedCertLexem: TScLexemInfo;
  i: integer;
begin
  // https://tools.ietf.org/html/rfc5280#section-6.3.3
  // paragraph (d)

  if Cert = nil then
    raise EScError.Create(seInvalidInputArgs);

  CheckAvailable;

  Result := False;
  CertSerialNumber := Cert.SerialNumber;
  Reason := crUnspecified;

  if FRevokedCertificates <> nil then begin
    CertIssuerExt := nil;

    for i := 0 to FRevokedCertificates.Count - 1 do begin
      RevokedCertificate := FRevokedCertificates[i];

      TmpCertIssuerExt := TScCRLCertificateIssuerExtension(RevokedCertificate.Extensions.FindExtensionByClass(TScCRLCertificateIssuerExtension));
      if TmpCertIssuerExt <> nil then
        CertIssuerExt := TmpCertIssuerExt;

      if SameText(RevokedCertificate.SerialNumber, CertSerialNumber) then
        if CertIssuerExt <> nil then
          Result := CertIssuerExt.CertificateIssuer.HasEqual(Cert.IssuerName)
        else
          Result := True;

      if Result then begin
        ReasonCodeExt := TScCRLReasonCodeExtension(RevokedCertificate.Extensions.FindExtensionByClass(TScCRLReasonCodeExtension));
        if ReasonCodeExt <> nil then
          Reason := ReasonCodeExt.CRLReason;

        Break;
      end;
    end;
  end
  else
  if FRevokedSN <> nil then begin
    if FRevokedSN.IndexOf(CertSerialNumber) > -1 then
      Result := True;
  end
  else begin
    lock(FLock);
    try
      ASN1Compiler := TScASN1Compiler.Create;
      try
        if not ASN1Compiler.Parse(asn1_CERTIFICATE_LIST_DESC, FRawData) then
          raise EScError.Create(seWrongCRLContext);

        FRevokedSN := TStringList.Create;

        RevokedCertificatesLexem := ASN1Compiler['RevokedCertificates'];
        for i := 0 to RevokedCertificatesLexem.ValueCount - 1 do begin
          RevokedCertLexem := RevokedCertificatesLexem.Values[i];
          S := RevokedCertLexem['UserCertificate'].AsString;
          FRevokedSN.Add(S);
          if not Result and SameText(S, CertSerialNumber) then
            Result := True;
        end;
      finally
        ASN1Compiler.Free;
      end;
    finally
      unlock(FLock);
    end;
  end;
end;

function TScCRL.CheckSignature(ParentCertificate: TScCertificate): boolean;
var
  TBSData, Sign: TBytes;
  HashAlgorithm: TScHashAlgorithm;
  PaddingMode: TScPaddingMode;
begin
  if ParentCertificate = nil then
    raise EScError.Create(seInvalidInputArgs);

  TScCertificate.GetSignAndPadAlg(GetSignatureAlgorithm, HashAlgorithm, PaddingMode);
  ParentCertificate.Key.PSSParams.Assign(FSignatureAlgorithm.PSSParams);

  TBSData := FASN1Compiler['TBSCertList'].EncodedData;
  Sign := FASN1Compiler['SignatureValue'].AsBytes;
  Result := ParentCertificate.Key.VerifySign(TBSData, Sign, HashAlgorithm, PaddingMode);
end;

procedure TScCRL.VerifyCRLChain(ParentCertificate: TScCertificate; var StatusSet: TScCertificateStatusSet);
var
  Ext: TScCertificateExtension;
begin
  if ParentCertificate = nil then
    raise EScError.Create(seInvalidInputArgs);

  CheckAvailable;

  if not FIssuerDN.Equals(ParentCertificate.SubjectName) then
    Include(StatusSet, csIssuerNotEqualSubject);

  Ext := ParentCertificate.Extensions.FindExtensionByClass(TScCertKeyUsageExtension);
  if Ext <> nil then
    if not (kfCRLSign in TScCertKeyUsageExtension(Ext).KeyUsages) then
      Include(StatusSet, csInvalidKeyUsage);

  if not CheckSignature(ParentCertificate) then
    Include(StatusSet, csInvalidSignature);
end;

procedure TScCRL.ImportFrom(const FileName: string; const Password: string = '');
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    ImportFrom(Stream, Password);
  finally
    Stream.Free;
  end;
end;

procedure TScCRL.ImportFrom(Stream: TStream; const Password: string = '');
var
  Data, DecodedData: TBytes;
  DataStr, ContentStr: string;
begin
  SetLength(Data, Stream.Size - Stream.Position);
  if Length(Data) > 0 then
    Stream.Read(Data[0], Length(Data));

  DataStr := Encoding.ANSI.GetString(Data);
  ContentStr := TScCertificate.ParsePEMString(DataStr, 'X509 CRL');
  if ContentStr = '' then
    ContentStr := TScCertificate.ParsePEMString(DataStr, 'CRL');

  if ContentStr <> '' then
    DecodedData := TBase64.Decode(Encoding.Default.GetBytes(ContentStr))
  else
    DecodedData := Data;

  SetRawData(DecodedData);

  SaveToStorageIfPossible;
end;

procedure TScCRL.SetRawData(const RawData: TBytes);
begin
  lock(FLock);
  try
    Ready := False;

    FRawData := RawData;
    FIsPartialParsed := True;

    Assert(FASN1Compiler = nil);
    FASN1Compiler := TScASN1Compiler.Create;
    try
      if (FRawData = nil) or not FASN1Compiler.Parse(asn1_PARTIAL_CERTIFICATE_LIST_DESC, FRawData) then
        raise EScError.Create(seWrongCRLContext);

      FSignatureAlgorithm := TScSignatureAlgorithmIdentifier.Create;
      TScASNUtils.Parse(FSignatureAlgorithm, FASN1Compiler['SignatureAlgorithm']);

      FIssuerDN := TScDistinguishedName.Create;
      TScASNUtils.Parse(FIssuerDN, FASN1Compiler['Issuer']);
    except
      FreeAndNil(FASN1Compiler);
      FreeAndNil(FSignatureAlgorithm);
      FreeAndNil(FIssuerDN);
      raise;
    end;

    FReady := True;
  finally
    unlock(FLock);
  end;
end;

procedure TScCRL.ExportTo(const FileName: string; const CertEncoding: TScCertificateEncoding = cfPEM);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  try
    ExportTo(Stream, CertEncoding);
  finally
    Stream.Free;
  end;
end;

procedure TScCRL.ExportTo(Stream: TStream; const CertEncoding: TScCertificateEncoding = cfPEM);
begin
  if not Ready then
    raise EScError.Create(seCRLNotReady);

  if CertEncoding = cfPEM then begin
    TStreamUtils.WriteLine(Stream, CRL_HEADER);
    WriteBlockInPEMStyle(Stream, TBase64.Encode(FRawData), 64, False);
    TStreamUtils.WriteLine(Stream, CRL_FOOTER);
  end
  else
    Stream.WriteBuffer(FRawData[0], Length(FRawData));
end;

function TScCRL.GetVersion: integer;
begin
  CheckAvailable;
  Result := FASN1Compiler['Version'].AsInteger + 1;
end;

function TScCRL.GetSignature: TBytes;
begin
  CheckAvailable;
  Result := FASN1Compiler['SignatureValue'].AsBytes;
end;

function TScCRL.GetSignatureAlgorithm: TScSignatureAlgorithmIdentifier;
begin
  CheckAvailable;
  Result := FSignatureAlgorithm;
end;

function TScCRL.GetIssuer: string;
begin
  CheckAvailable;
  Result := TScCertificate.GetDisplayName(FIssuerDN);
end;

function TScCRL.GetIssuerDN: TScDistinguishedName;
begin
  CheckAvailable;
  Result := FIssuerDN;
end;

function TScCRL.GetThisUpdate: TDateTime;
begin
  CheckAvailable;
  Result := FASN1Compiler['ThisUpdate'].AsDateTime;
end;

function TScCRL.GetNextUpdate: TDateTime;
begin
  CheckAvailable;
  Result := FASN1Compiler['NextUpdate'].AsDateTime;
end;

procedure TScCRL.CheckExtensions;
begin
  CheckAvailable;

  lock(FLock);
  try
    if FExtensions = nil then begin
      FExtensions := TScExtensions.Create;
      TScASNUtils.Parse(FExtensions, FASN1Compiler['CrlExtensions']);
    end;
  finally
    unlock(FLock);
  end;
end;

function TScCRL.GetExtensions: TScExtensions;
begin
  CheckExtensions;
  Result := FExtensions;
end;

procedure TScCRL.CheckRevokedCertificates;
var
  RevokedCertificatesLexem, RevokedCertLexem: TScLexemInfo;
  RevokedCertificate: TScRevokedCertificate;
  List: TScRevokedCertificates;
  i: integer;
begin
  CheckAvailable;

  lock(FLock);
  try
    if FIsPartialParsed then begin
      if not FASN1Compiler.Parse(asn1_CERTIFICATE_LIST_DESC, FRawData) then
        raise EScError.Create(seWrongCRLContext);
      FIsPartialParsed := False;
    end;

    if FRevokedCertificates = nil then begin
      RevokedCertificatesLexem := FASN1Compiler['RevokedCertificates'];
      List := TScRevokedCertificates.Create;

      for i := 0 to RevokedCertificatesLexem.ValueCount - 1 do begin
        RevokedCertificate := TScRevokedCertificate.Create;
        List.Add(RevokedCertificate);

        RevokedCertLexem := RevokedCertificatesLexem.Values[i];
        RevokedCertificate.FSerialNumber := RevokedCertLexem['UserCertificate'].AsString;
        RevokedCertificate.FRevocationDate := RevokedCertLexem['RevocationDate'].AsDateTime;
        RevokedCertificate.FExtensionsEncodedData := RevokedCertLexem['CrlEntryExtensions'].EncodedData;
      end;

      FRevokedCertificates := List;
    end;
  finally
    unlock(FLock);
  end;
end;

function TScCRL.GetRevokedCertificates: TScRevokedCertificates;
begin
  CheckRevokedCertificates;
  Result := FRevokedCertificates;
end;

function TScCRL.GetCRLList: TScCRLList;
begin
  Result := TScCRLList(StorageList);
end;

procedure TScCRL.SetCRLList(Value: TScCRLList);
begin
  StorageList := Value;
end;

class function TScCRL.LoadByHttp(const CRLUrl: string): TScCRL;
var
  HttpWebRequest: {$IFNDEF SBRIDGE}TCRHttpWebRequest{$ELSE}TScHttpWebRequest{$ENDIF};
  HttpWebResponse: {$IFNDEF SBRIDGE}TCRHttpWebResponse{$ELSE}TScHttpWebResponse{$ENDIF};
  ms: TMemoryStream;
begin
  HttpWebResponse := nil;
  HttpWebRequest := {$IFNDEF SBRIDGE}TCRHttpWebRequest{$ELSE}TScHttpWebRequest{$ENDIF}.Create(CRLUrl);
  try
    HttpWebResponse := HttpWebRequest.GetResponse;

    ms := TMemoryStream.Create;
    try
      HttpWebResponse.ReadToStream(ms);
      ms.Position := 0;

      Result := TScCRL.Create(nil);
      try
        Result.CRLName := TScCRLList.GetHashedName(CRLUrl);
        Result.ImportFrom(ms);
      except
        Result.Free;
        raise;
      end;
    finally
      ms.Free;
    end;
  finally
    HttpWebRequest.Free;
    HttpWebResponse.Free;
  end;
end;

{ TScCRLList }

class procedure TScCRLList.DoObtainCRL(Sender: TObject; Storage: TScStorage;
  AllowLoadCRLByHttp: boolean; OnObtainCRL: TScObtainCRLEvent;
  DistributionPointName: TScGeneralNames; Update: boolean; out CRL: TScCRL);
var
  LocalCRL, GlobalCRL: TScCRL;
  CRLName, CRLHashedName: string;
  i: integer;
begin
  CRL := nil;

  CRLName := '';
  for i := 0 to DistributionPointName.Count - 1 do begin
    CRLName := DistributionPointName.GeneralNames[i].Value;
    if Copy(CRLName, 1, 4) = 'http' then
      Break
    else
      CRLName := '';
  end;

  if CRLName <> '' then
    CRLHashedName := TScCRLList.GetHashedName(CRLName)
  else
    CRLHashedName := '';

  if CRLHashedName <> '' then begin
    if CRLStorage <> nil then
      GlobalCRL := CRLStorage.CRLs.FindCRL(CRLHashedName)
    else
      GlobalCRL := nil;

    if Storage <> nil then
      LocalCRL := Storage.CRLs.FindCRL(CRLHashedName)
    else
      LocalCRL := nil;

    if not Update then begin
      if GlobalCRL = nil then
        CRL := LocalCRL
      else
      if LocalCRL = nil then
        CRL := GlobalCRL
      else
      if GlobalCRL.NextUpdate > LocalCRL.NextUpdate then
        CRL := GlobalCRL
      else
        CRL := LocalCRL;

      if CRL <> nil then
        Exit;
    end;

    try
      if AllowLoadCRLByHttp then begin
        CRL := TScCRL.LoadByHttp(CRLName);

        // add first to local storage, then in global storage will be saved the parsed CLR
        if (Storage <> nil) and (LocalCRL <> nil) {and ((CRLStorage = nil) or (Update and (LocalCRL <> nil)))} then begin
          Storage.CRLs.Remove(LocalCRL);
          LocalCRL.Free;
          Storage.CRLs.Add(CRL);
        end;

        if CRLStorage <> nil then begin
          CRLStorage.CRLs.Remove(GlobalCRL);
          GlobalCRL.Free;
          CRLStorage.CRLs.Add(CRL);
        end;

        if CRL <> nil then
          Exit;
      end;
    except
    end;
  end;

  if Assigned(OnObtainCRL) then
    OnObtainCRL(Sender, DistributionPointName, Update, CRL);
end;

class function TScCRLList.GetHashedName(const FullName: string): string;
var
  csp: THash_SHA1;
begin
  csp := THash_SHA1.Create;
  try
    Result := BytesToHexStr(csp.ComputeHash(Encoding.UTF8.GetBytes(FullName)));
  finally
    csp.Free;
  end;
end;

function TScCRLList.GetCRL(Index: integer): TScCRL;
begin
  Result := TScCRL(GetItem(Index));
end;

procedure TScCRLList.SetCRL(Index: integer; Value: TScCRL);
begin
  SetItem(Index, Value);
end;

procedure TScCRLList.CheckCRLName(const CRLName: string);
begin
  CheckItemName(CRLName);
end;

function TScCRLList.FindCRL(const CRLName: string): TScCRL;
begin
  Result := TScCRL(FindItem(CRLName));
end;

function TScCRLList.CRLByName(const CRLName: string): TScCRL;
begin
  Result := TScCRL(ItemByName(CRLName));
end;

procedure TScCRLList.GetCRLNames(List: TStrings);
begin
  GetItemNames(List);
end;

function TScCRLList.GetItemClassType: TScStorageItemClass;
begin
  Result := TScCRL;
end;

function TScCRLList.GetItemDefName: string;
begin
  Result := 'crl';
end;

{ TScPKCS12Bag }

destructor TScPKCS12Bag.Destroy;
begin
  FItem.Free;

  inherited;
end;

{ TScPKCS12Processor }

constructor TScPKCS12Processor.Create;
begin
  inherited;

  FKeyList := TCRList.Create;
  FCertList := TCRList.Create;
  FCRLList := TCRList.Create;
  FCustomList := TCRList.Create;
end;

destructor TScPKCS12Processor.Destroy;
begin
  Clear;
  FKeyList.Free;
  FCertList.Free;
  FCRLList.Free;
  FCustomList.Free;

  inherited;
end;

procedure TScPKCS12Processor.Clear;

  procedure ClearList(List: TCRList);
  begin
    while List.Count > 0 do begin
    {$IFNDEF AUTOREFCOUNT}
      TObject(List[List.Count - 1]).Free;
    {$ELSE}
      List[List.Count - 1] := nil;
    {$ENDIF}
      List.Delete(List.Count - 1);
    end;
  end;

begin
  ClearList(FKeyList);
  ClearList(FCertList);
  ClearList(FCRLList);
  ClearList(FCustomList);
end;

class function TScPKCS12Processor.ImportPasswordFromSSO(Stream: TStream): string;
const
  SSO_HEADER: array[0..10] of byte =
    ($A1, $F8, $4E, $36{ $37}, 0, 0, 0, 6, 0, 0, 0);

  ASE_SALT: array[0..15] of byte =
    (192, 52, 216, 49, 28, 2, 206, 248, 81, 240, 20, 75, 129, 237, 75, 242);

var
  i, Pad: integer;
  Header, Key, Salt, Pass: TBytes;
  Cipher: TSymmetricAlgorithm;
begin
  if Stream.Size < 46 then
    raise EScError.Create(sePKCS12DataBroken);

  SetLength(Header, 13);
  Stream.Read(Header[0], 13);
  if MemCompare(@Header[0], @SSO_HEADER[0], 3) <> 0 then
    raise EScError.Create(sePKCS12DataBroken);
  if MemCompare(@Header[4], @SSO_HEADER[4], 7) <> 0 then
    raise EScError.Create(sePKCS12DataBroken);
  if (Header[3] <> $36) and (Header[3] <> $37) then
    raise EScError.Create(sePKCS12DataBroken);

  if (Header[11] = 33) and (Header[12] = 6) then begin
    SetLength(Key, 16);
    SetLength(Salt, 16);
    SetLength(Pass, 16);
    Stream.Read(Key[0], 16);
    Stream.Read(Pass[0], 16);
    Move(ASE_SALT[0], Salt[0], 16);

    Cipher := TCipher_Rijndael.Create;
    Cipher.Mode := cmCBC;
    Cipher.Key := Key;
    Cipher.IV := Salt;
    Cipher.CreateDecryptor.TransformBlock(Pass, 0, Length(Pass));
    Result := Encoding.Default.GetString(Pass);
  end
  else
  if (Header[11] = 65) and (Header[12] = 53) then begin
    SetLength(Key, 16);
    SetLength(Pass, 48);
    Stream.Read(Key[0], 16);
    Stream.Read(Pass[0], 48);

    Key := HexToBytes(Encoding.Default.GetString(Key));
    Pass := HexToBytes(Encoding.Default.GetString(Pass));

    Cipher := TCipher_1DES.Create;
    Cipher.Mode := cmCBC;
    Cipher.Key := Key;
    Cipher.CreateDecryptor.TransformBlock(Pass, 0, Length(Pass));

    // delete Padding PKCS#7
    Pad := Pass[Length(Pass) - 1];
    if Pad > 8 then
      raise EScError.Create(sePKCS12DataBroken);

    for i := 1 to Pad do
      if Pass[Length(Pass) - i] <> Pad then
        raise EScError.Create(sePKCS12DataBroken);

    Result := Encoding.Default.GetString(Pass, 0, Length(Pass) - Pad);
  end
  else
    raise EScError.Create(sePKCS12DataBroken);

  if Stream.Position = Stream.Size then
    raise EScError.Create(sePKCS12DataBroken);
end;

procedure TScPKCS12Processor.ImportFrom(const FileName: string; const Password: string = '');
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    ImportFrom(Stream, Password);
  finally
    Stream.Free;
  end;
end;

procedure TScPKCS12Processor.ImportFrom(Stream: TStream; const Password: string = '');
var
  ASN1Compiler: TScASN1Compiler;
  ASN1CompilerEnc: TScASN1Compiler;
  AuthSafeLexem, AuthSafesLexem: TScLexemInfo;
  MacDataLexem: TScLexemInfo;
  ContentInfoLexem, EncryptedContentInfoLexem: TScLexemInfo;
  DigestAlgorithmOID: string;
  ContentType: string;
  MacSalt, MacDigest, ContentDigest, KeyBuf: TBytes;
  Content: TBytes;
  ContentEncryptionAlgorithm: TScASN1AlgorithmIdentifier;
  Cipher: TSymmetricAlgorithm;
  Iters: integer;
  HashAlg: TScHashAlgorithm;
  HMac: THMAC;
{$IFDEF SBRIDGE}
  SignedData: TScCMSSignedData;
  EnvelopedData: TScCMSEnvelopedData;
{$ENDIF}
  KeyBag, CertBag: TScPKCS12Bag;
  i, j: integer;
begin
  // https://tools.ietf.org/html/rfc7292

  Clear;
  ContentEncryptionAlgorithm := nil;
  ASN1CompilerEnc := nil;
  ASN1Compiler := TScASN1Compiler.Create;
  try
    if Stream.Position = Stream.Size then
      Stream.Position := 0;

    if not ASN1Compiler.Parse(asn1_PFX_DESC, Stream) then
      raise EScError.Create(seWrongPKCS12Context);

    if ASN1Compiler['Version'].AsInteger <> 3 then
      raise EScError.Create(seWrongPKCS12Context);

    AuthSafeLexem := ASN1Compiler['AuthSafe'];
    ContentType := AuthSafeLexem['ContentType'].AsString;
    if ContentType = OID_DATA_TYPE then
      Content := AuthSafeLexem['Content'].AsBytes
  {$IFDEF SBRIDGE}
    else
    if ContentType = OID_SIGNED_DATA_TYPE then begin
      if FCertificateSign = nil then
        raise EScError.Create(seCertificateSignNotSet);

      SignedData := TScCMSSignedData.Create;
      try
        SignedData.Decode(AuthSafeLexem.EncodedData);
        SignedData.CheckSignature(FCertificateSign);
        Content := SignedData.ContentInfo.GetContentData;
      finally
        SignedData.Free;
      end;
    end
  {$ENDIF}
    else
      raise EScError.Create(seWrongPKCS12Context);

    MacDataLexem := ASN1Compiler['MacData'];
    SetLength(MacSalt, 0);
    SetLength(MacDigest, 0);
    if not MacDataLexem.IsNull then begin
      DigestAlgorithmOID := MacDataLexem['DigestAlgorithm']['Algorithm'].AsString;
      MacSalt := MacDataLexem['MacSalt'].AsBytes;
      MacDigest := MacDataLexem['Digest'].AsBytes;
      if MacDataLexem['Iters'].IsNull then
        Iters := 1
      else
        Iters := MacDataLexem['Iters'].AsInteger;
    end
    else begin
      DigestAlgorithmOID := '';
      Iters := 0;
    end;

    if not ASN1Compiler.Parse(asn1_AUTH_SAFE_DESC_EX, Content) then
      raise EScError.Create(seWrongPKCS12Context);

  {$IFNDEF VER10P}
    SetLength(Content, 0);
    SetLength(ContentDigest, 0);
    SetLength(KeyBuf, 0);
  {$ENDIF}

    AuthSafesLexem := ASN1Compiler['AuthSafes'];
    if DigestAlgorithmOID <> '' then begin
      HashAlg := CipherFactory.OidToHashAlgorithm(DigestAlgorithmOID);
      KeyBuf := TScKeyDerivationProcessor.PKCS12KeyDeriveFromPassword(HashAlg, Password,
        MacSalt, kpMacKey, Iters, CipherFactory.HashClass(HashAlg).GetHashSize);

      HMac := THMAC.Create(CipherFactory.HashClass(HashAlg), KeyBuf);
      try
        ContentDigest := HMac.ComputeHash(AuthSafesLexem.EncodedData);
      finally
        HMac.Free;
      end;

      if (Length(MacDigest) <> Length(ContentDigest)) or
         (MemCompare(@MacDigest[0], @ContentDigest[0], Length(ContentDigest)) <> 0) then
        raise EScError.Create(sePKCS12DataBroken);
    end;

    for i := 0 to AuthSafesLexem.ValueCount - 1 do begin
      ContentInfoLexem := AuthSafesLexem.Values[i];

      ContentType := ContentInfoLexem['ContentType'].AsString;
      if ContentType = OID_DATA_TYPE then begin
        Content := ContentInfoLexem['Content'].AsBytes;
      end
      else
      if ContentType = OID_ENCRYPTED_DATA_TYPE then begin
        if ASN1CompilerEnc = nil then
          ASN1CompilerEnc := TScASN1Compiler.Create;

        if not ASN1CompilerEnc.Parse(asn1_ENCRYPTED_DATA_DESC, ContentInfoLexem['Content'].AsBytes) then
          raise EScError.Create(seWrongPKCS12Context);

        EncryptedContentInfoLexem := ASN1CompilerEnc['EncryptedContentInfo'];
        if ContentEncryptionAlgorithm = nil then
          ContentEncryptionAlgorithm := TScASN1AlgorithmIdentifier.Create;

        TScASNUtils.Parse(ContentEncryptionAlgorithm, EncryptedContentInfoLexem['ContentEncryptionAlgorithm']);

        Cipher := TScKeyDerivationProcessor.CreateCipherByOId(ContentEncryptionAlgorithm.Algorithm.Value,
          ContentEncryptionAlgorithm.Parameters, Password);
        if Cipher = nil then
          raise EScError.Create(seWrongPKCS12Context);

        ContentType := EncryptedContentInfoLexem['ContentType'].AsString;
        Content := EncryptedContentInfoLexem['EncryptedContent'].AsBytes;
        Cipher.CreateDecryptor.TransformBlock(Content, 0, Length(Content));
      end
    {$IFDEF SBRIDGE}
      else
      if ContentType = OID_ENVELOPED_DATA_TYPE then begin
        if FCertificateEnc = nil then
          raise EScError.Create(seCertificateEncNotSet);

        EnvelopedData := TScCMSEnvelopedData.Create;
        try
          EnvelopedData.Decode(ContentInfoLexem['AuthSafei'].EncodedData);
          Content := EnvelopedData.Decrypt(FCertificateEnc);
        finally
          EnvelopedData.Free;
        end;
      end
    {$ENDIF}
      else
        raise EScError.Create(seWrongPKCS12Context);

      ParseBags(Content, Password);
    end;
  finally
    ASN1Compiler.Free;
    ASN1CompilerEnc.Free;
    ContentEncryptionAlgorithm.Free;
  end;

  for i := 0 to FCertList.Count - 1 do begin
    CertBag := TScPKCS12Bag(FCertList[i]);

    for j := 0 to FKeyList.Count - 1 do begin
      KeyBag := TScPKCS12Bag(FKeyList[j]);
      if (Length(CertBag.FLocalKeyID) = Length(KeyBag.FLocalKeyID)) and
         (MemCompare(@CertBag.FLocalKeyID[0], @KeyBag.FLocalKeyID[0], Length(CertBag.FLocalKeyID)) = 0) then begin
        (CertBag.Item as TScCertificate).Key.Assign(KeyBag.Item);
        break;
      end;
    end;
  end;
end;

procedure TScPKCS12Processor.ParseBags(const Content: TBytes; const Password: string);
var
  ASN1Compiler: TScASN1Compiler;
  ASN1CompilerValue: TScASN1Compiler;
  SafeBagsLexem, SafeBagLexem: TScLexemInfo;
  BagAttributes, BagAttribute, AttrValues: TScLexemInfo;
  BagID, AttrID, TypeID: string;
  BagValue: TBytes;
  PKCS12Bag: TScPKCS12Bag;
  Key: TScKey;
  Cert: TScCertificate;
  CRL: TScCRL;
  i, j: integer;
begin
  SetLength(BagValue, 0);
  ASN1CompilerValue := nil;
  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_SAFE_CONTENTS_DESC_EX, Content) then
      raise EScError.Create(seWrongPKCS12Context);

    SafeBagsLexem := ASN1Compiler['SafeBags'];

    for i := 0 to SafeBagsLexem.ValueCount - 1 do begin
      SafeBagLexem := SafeBagsLexem.Values[i];
      BagID := SafeBagLexem['BagID'].AsString;
      BagValue := SafeBagLexem['BagValue'].AsBytes;
      BagAttributes := SafeBagLexem['BagAttributes'];

      PKCS12Bag := TScPKCS12Bag.Create;
      try
        for j := 0 to BagAttributes.ValueCount - 1 do begin
          BagAttribute := BagAttributes.Values[j];
          AttrID := BagAttribute['AttrId'].AsString;
          AttrValues := BagAttribute['AttrValues'];
          if AttrValues.ValueCount > 0 then begin
            if AttrID = OID_PKCS12_ATTR_FriendlyName then
              PKCS12Bag.FFriendlyName := AttrValues.Values[0]['AttrValue'].AsString
            else
            if AttrID = OID_PKCS12_ATTR_LocalKeyID then
              PKCS12Bag.FLocalKeyID := AttrValues.Values[0]['AttrValue'].AsBytes;
          end;
        end;

        if BagID = OID_KEY_BAG then begin
          Key := TScKey.Create;
          PKCS12Bag.FItem := Key;
          Key.DecodePrivateKeyFromPKCS8Format(BagValue);
          Key.CheckReady;
          FKeyList.Add(PKCS12Bag);
        end
        else
        if BagID = OID_PKCS8_ENC_KEY_BAG then begin
          Key := TScKey.Create;
          PKCS12Bag.FItem := Key;
          Key.DecodePrivateKeyFromPKCS8EncFormat(BagValue, Password);
          Key.CheckReady;
          FKeyList.Add(PKCS12Bag);
        end
        else
        if BagID = OID_CERT_BAG then begin
          if ASN1CompilerValue = nil then
            ASN1CompilerValue := TScASN1Compiler.Create;

          if not ASN1CompilerValue.Parse(asn1_SAFE_BAG_VALUE_DESC, BagValue) then
            raise EScError.Create(seWrongPKCS12Context);

          Cert := TScCertificate.Create;
          PKCS12Bag.FItem := Cert;
          TypeID := ASN1CompilerValue['TypeID'].AsString;
          if TypeID = OID_X509_CERTIFICATE_BAG_TYPE then
            Cert.SetRawData(nil, nil, ASN1CompilerValue['Value'].AsBytes)
          else
          if TypeID = OID_SDSI_CERTIFICATE_BAG_TYPE then
            Cert.SetRawData(nil, nil, TBase64.Decode(ASN1CompilerValue['Value'].AsBytes))
          else
            raise EScError.Create(seWrongPKCS12Context);

          FCertList.Add(PKCS12Bag);
        end
        else
        if BagID = OID_CRL_BAG then begin
          if ASN1CompilerValue = nil then
            ASN1CompilerValue := TScASN1Compiler.Create;

          if not ASN1CompilerValue.Parse(asn1_SAFE_BAG_VALUE_DESC, BagValue) then
            raise EScError.Create(seWrongPKCS12Context);

          CRL := TScCRL.Create;
          PKCS12Bag.FItem := CRL;
          TypeID := ASN1CompilerValue['TypeID'].AsString;
          if TypeID = OID_X509CRL_BAG_TYPE then
            CRL.SetRawData(ASN1CompilerValue['Value'].AsBytes)
          else
            raise EScError.Create(seWrongPKCS12Context);

          FCRLList.Add(PKCS12Bag);
        end
        else
        if BagID = OID_SECRET_BAG then begin
          if ASN1CompilerValue = nil then
            ASN1CompilerValue := TScASN1Compiler.Create;

          if not ASN1CompilerValue.Parse(asn1_SAFE_BAG_VALUE_DESC, BagValue) then
            raise EScError.Create(seWrongPKCS12Context);

          PKCS12Bag.FTypeID := ASN1CompilerValue['TypeID'].AsString;
          PKCS12Bag.FRawData := ASN1CompilerValue['Value'].AsBytes;
          FCustomList.Add(PKCS12Bag);
        end
        else
        if BagID = OID_SAFE_CONTENTS_BAG then begin
          FreeAndNil(PKCS12Bag);
          ParseBags(BagValue, Password);
        end;

      except
        PKCS12Bag.Free;
        raise;
      end;
    end;
  finally
    ASN1Compiler.Free;
    ASN1CompilerValue.Free;
  end;
end;

{ TScKeyUtils }

class procedure TScKeyUtils.SetSSHKeyData(Key: TScKey; const SSHKeyData: TBytes);
begin
  Key.SetSSHKeyData(SSHKeyData);
end;

class function TScKeyUtils.GetSSHKeyData(Key: TScKey): TBytes;
begin
  Result := Key.GetSSHKeyData;
end;

class procedure TScKeyUtils.SetRSAData(Key: TScKey; const RSAData: TScRSAData);
begin
  Key.FreeData;
  Key.FRSAData := RSAData;
  Key.FAlgorithm := aaRSA;
  Key.FReady := True;
end;

class procedure TScKeyUtils.LoadKeyFromCryptoAPIFormat(Key: TScKey; const Data: TBytes;
  const KeyExFormat: TScKeyExFormat; const Alg: TScAsymmetricAlgorithm; const IsPublicKey: boolean);
begin
  Key.LoadKeyFromCryptoAPIFormat(Data, KeyExFormat, Alg, IsPublicKey);
end;

class function TScKeyUtils.SaveKeyToCryptoAPIFormat(Key: TScKey): TBytes;
begin
  Result := Key.SaveKeyToCryptoAPIFormat;
end;

class procedure TScKeyUtils.LoadKeyFromPVKFormat(Key: TScKey; const Data: TBytes);
begin
  Key.LoadKeyFromPVKFormat(Data);
end;

{$IFDEF SBRIDGE}
{ TScStorageUtils }

class procedure TScStorageUtils.CheckUserPass(Obj: TScStorage; ClientInfo: TScSSHClientInfo; const Password: string; var Accept: boolean);
begin
  Obj.CheckUserPass(ClientInfo, Password, Accept);
end;

class procedure TScStorageUtils.CheckUserKey(Obj: TScStorage; ClientInfo: TScSSHClientInfo; Key: TScKey; var Accept: boolean);
begin
  Obj.CheckUserKey(ClientInfo, Key, Accept);
end;

class procedure TScStorageUtils.Invalidate(Obj: TScStorage);
begin
  Obj.Invalidate;
end;
{$ENDIF}

{ TScCertificateUtils }

class procedure TScCertificateUtils.SetHandle(Obj: TScCertificate; Handle: IntPtr; OnFreeHandle: TScFreeHandleEvent);
begin
  Obj.SetHandle(Handle, OnFreeHandle);
end;

initialization
  CurrentProjectOutputDir := GetCurrentDir;
  Random := TScRandomLFSR.Create;

  CRLStorage := TScFileStorage.Create(nil);
  TScFileStorage(CRLStorage).Path := GetHomePath + PathDelim {$IFDEF IOS} + 'Documents' + PathDelim{$ENDIF} + 'CRL';

finalization
  Random := nil;
  FreeAndNil(CRLStorage);

end.
