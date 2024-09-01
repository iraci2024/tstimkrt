
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF DAC}
{$I SB.inc}
unit ScUtils;
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
{$IFDEF POSIX}
  Posix.SysSocket,
{$ENDIF}
{$IFDEF MSWINDOWS}
  {$IFNDEF FPC}
  {$IFNDEF BCB}
  {$NOINCLUDE winsock}
  {$HPPEMIT '#include <winsock2.h>'}
  {$ENDIF}
  {$ENDIF}
  WinSock,
{$ENDIF}
{$IFDEF UNIX}
  sockets, baseunix, unix, netdb,
{$ENDIF}
{$IFDEF NEXTGEN}
  Generics.Collections,
{$ENDIF}
{$IFDEF HAVE_COMPRESS_INTERNAL}
  ZLib, ZLibConst,
{$ENDIF}
  SysUtils, Classes, SyncObjs,
{$IFNDEF FPC}
{$IFNDEF MSWINDOWS}
  System.IOUtils,
{$ENDIF}
{$ENDIF}
{$IFNDEF SBRIDGE}
  CLRClasses, CRTypes, CRFunctions, CRVio, CRVioTcp, CRBigInteger,
{$IFNDEF UNIDACPRO}
  TdsSSLConsts;
{$ELSE}
  TdsSSLConstsUni;
{$ENDIF}
{$ELSE}
  ScCLRClasses, ScTypes, ScFunctions, ScVio, ScVioTcp, ScBigInteger,
  ScConsts;
{$ENDIF}

{$IFNDEF SBRIDGE}
  {$UNDEF TRIAL}
  {$UNDEF TRIAL_BPLCALL}
{$ENDIF}


type
  TScSymmetricAlgorithm = (
    saTripleDES_cbc,
    saBlowfish_cbc,
    saAES128_cbc,
    saAES192_cbc,
    saAES256_cbc,
    saCast128_cbc,
    saTripleDES_ctr,
    saBlowfish_ctr,
    saAES128_ctr,
    saAES192_ctr,
    saAES256_ctr,
    saCast128_ctr
  );
  TScSymmetricAlgorithms = set of TScSymmetricAlgorithm;

  TScSymmetricAlgorithmEx = (
    sa_DES,
    sa_TripleDES,
    sa_Blowfish,
    sa_AES128,
    sa_AES192,
    sa_AES256,
    sa_Cast128,
    sa_RC2,
    sa_RC4
  );

  TScHashAlgorithm = (haNone, haSHA1, haSHA2_256, haSHA2_512, haSHA2_224, haSHA2_384, haMD5, haMD4, haMD2);
  TScHashAlgorithms = set of TScHashAlgorithm;

  TScHMACAlgorithm = (hmacSHA1, hmacSHA2_256, hmacSHA2_512, hmacSHA2_224, hmacSHA2_384, hmacMD5);
  TScHMACAlgorithms = set of TScHMACAlgorithm;

  TScKeyExchangeAlgorithm = (
    keDHGroup1Sha1, keDHGroup14Sha1,
    keDHExchSha1, keDHExchSha256,
    keECDHSha2Nistp256, keECDHSha2Nistp384, keECDHSha2Nistp521,
    keCurve25519Sha256);
  TScKeyExchangeAlgorithms = set of TScKeyExchangeAlgorithm;

  TScAsymmetricAlgorithm = (aaDSA, aaRSA, aaEC);
  TScAsymmetricAlgorithms = set of TScAsymmetricAlgorithm;

  TScSignatureAlgorithm = (saUnknown, saRSA_Encryption, saRSA_PSS_Encryption,
    saSHA1_RSA_Encryption, saSHA256_RSA_Encryption, saSHA224_RSA_Encryption,
    saSHA512_RSA_Encryption, saSHA384_RSA_Encryption,
    saMD5_RSA_Encryption, saMD4_RSA_Encryption, saMD2_RSA_Encryption,
    saDSA_Encryption, saDSA_SHA1, saDSA_SHA256, saDSA_SHA224,
    saEC_SHA1, saEC_SHA224, saEC_SHA256, saEC_SHA384, saEC_SHA512);

  // !!! Synchronize with TScKExNamedGroupType, EllipticCurvesParameters and KEX_NAMED_GROUP_CODES
  TScECName = (
    x25519,
    secp160r1, secp160r2, secp160k1,
    secp192r1, secp192k1, secp224r1, secp224k1,
    secp256r1, secp256k1, secp384r1, secp521r1,
    sect163r1, sect163r2, sect163k1,
    sect193r1, sect193r2, sect233r1, sect233k1, sect239k1,
    sect283r1, sect283k1, sect409r1, sect409k1,
    sect571r1, sect571k1
  );

  TScCompressionAlgorithm = (csaNone, csaZLib, csaZLibOpenSSH);
  TScCompressionAlgorithms = set of TScCompressionAlgorithm;

  TScCompression = (csNone, csAllowed, csRequired);
  TScCompressionArray = array of TScCompression;

  TScSSHChannelPermission = (cpAllowLocalForwarding, cpAllowRemoteForwarding, cpAllowShell, cpAllowSFTP);
  TScSSHChannelPermissions = set of TScSSHChannelPermission;

  TScTLSMode = (tmDisableTLS, tmImplicitTLS, tmRequireExplicitTLS, tmAllowExplicitTLS);

  TScEventCallMode = (ecAsynchronous, ecSynchronous, ecDirectly);

type
  EScError = class(Exception)
  protected
    FErrorCode: TScErrorCode;
  public
    constructor Create(const Msg: string); overload;
    constructor Create(ErrorCode: TScErrorCode); overload;
    constructor CreateFmt(const Msg: string; const Args: array of const; ErrorCode: TScErrorCode);

    function Clone: EScError;
    property ErrorCode: TScErrorCode read FErrorCode;
  end;

  TScCollectionItem = class(TCollectionItem)
  protected
    function GetAsString: string; virtual;
    procedure SetAsString(const Value: string); virtual;
  public
    property AsString: string read GetAsString write SetAsString;
  end;

  TScCollectionItemClass = class of TScCollectionItem;

  TScCollection = class(TOwnedCollection)
  private
    FOnChanged: TNotifyEvent;
    function GetAsString: string;
    procedure SetAsString(const Value: string);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    procedure Assign(Source: TPersistent); override;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TScCollectionItemClass);
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
    property AsString: string read GetAsString write SetAsString;
  end;

  TScStreamInfoRec = record
    Stream: TStream;
    Position: Int64;
    Count: Int64;
  end;

  TScStreamInfo = class
  private
    FStream: TStream;
    FPosition: Int64;
    FCount: Int64;
  public
    constructor Create(Stream: TStream; Position, Count: Int64);
    procedure Init(Stream: TStream; Position, Count: Int64);
    procedure Assign(Source: TScStreamInfo); overload;
    procedure Assign(const Source: TScStreamInfoRec); overload;

    property Stream: TStream read FStream;
    property Position: Int64 read FPosition;
    property Count: Int64 read FCount;
  end;

  TScPersistent = class
  protected
    procedure RaiseAssignError(Source: TScPersistent);
    function Clone: TScPersistent; virtual; abstract;
  public
    procedure Assign(Source: TScPersistent); virtual; abstract;
  end;

  TScPersistentClass = class of TScPersistent;

  TScPersistentObjectList = class
  private
    FList: TCRObjectList;

    function GetCount: integer;
    function Get(Index: integer): TScPersistent;
    procedure Put(Index: integer; Item: TScPersistent);
  protected
    procedure CheckType(Item: TScPersistent);
    function GetItemClassType: TScPersistentClass; virtual;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TScPersistentObjectList); virtual;

    function Add(Item: TScPersistent): integer;
    procedure Clear;
    procedure Delete(Index: integer);
    function IndexOf(Item: TScPersistent): integer;
    procedure Insert(Index: integer; Item: TScPersistent);
    function Remove(Item: TScPersistent): integer;

    property Count: integer read GetCount;
    property Items[Index: integer]: TScPersistent read Get write Put;
  end;

  TScWriteDataToStreamEvent = procedure (Stream: TStream; const Data: TBytes; Count: integer) of object;
  TScReadDataFromStreamEvent = function (Stream: TStream; var Data: TBytes; Count: integer): integer of object;

  TScAfterClientHelloMessageEvent = procedure (Sender: TObject; var Cancel: boolean) of object;
  TScGetTicketNameAndPasswordEvent = procedure (Sender: TObject; out TicketName, Password: TBytes) of object;
  TScGetPasswordByTicketNameEvent = procedure (Sender: TObject; const Ticket: TBytes; out Password: TBytes) of object;

  TScAsyncReceiveEvent = procedure (Sender: TObject) of object;
  TScErrorEvent = procedure (Sender: TObject; E: Exception) of object;
  TScConnectChangeEvent = procedure (Sender: TObject; Connecting: boolean) of object;
  TScBannerEvent = procedure (Sender: TObject; const Banner: string) of object;

  TScSendLineEvent = procedure (Sender: TObject; const Line: string) of object;
  TScReadLineEvent = procedure (Sender: TObject; const Line: string) of object;

  IScCompression = interface
  ['{94EC0E52-388C-49DA-A1C6-3DEF7B27C117}']
    procedure Compress(const InBuffer: TValueArr; InOffset, InCount: cardinal;
      const OutBuffer: TValueArr; OutOffset: cardinal; var OutCount: cardinal; Flush: boolean = True);
    function Decompress(const InBuffer: TValueArr; var InOffset, InCount: cardinal;
      const OutBuffer: TValueArr; OutOffset: cardinal; var OutCount: cardinal): boolean;
    procedure CompressStream(InStream: TStream; OnWriteData: TScWriteDataToStreamEvent);
    procedure DecompressToStream(OutStream: TStream; OnReadData: TScReadDataFromStreamEvent);
  end;

  TScZCompression = class(TInterfacedObject, IScCompression)
  private
  {$IFDEF HAVE_COMPRESS_INTERNAL}
    FZCRec: TZStreamRec;
    FZDRec: TZStreamRec;
    FCInited: Boolean;
    FDInited: Boolean;
    FInBuffer: TBytes;
    FOutBuffer: TBytes;
    procedure InitC;
    procedure InitD;
    function CCheck(code: Integer): Integer;
    function DCheck(code: Integer): Integer;
  {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;

    procedure Compress(const InBuffer: TValueArr; InOffset, InCount: cardinal;
      const OutBuffer: TValueArr; OutOffset: cardinal; var OutCount: cardinal; Flush: boolean = True);
    function Decompress(const InBuffer: TValueArr; var InOffset, InCount: cardinal;
      const OutBuffer: TValueArr; OutOffset: cardinal; var OutCount: cardinal): boolean;
    procedure CompressStream(InStream: TStream; OnWriteData: TScWriteDataToStreamEvent);
    procedure DecompressToStream(OutStream: TStream; OnReadData: TScReadDataFromStreamEvent);
  end;

  TSimpleSemaphore = class
  private
    FCountLock: TCriticalSection;
    FWaiter: TEvent;
    FCurrentCount: integer;
    FInThreadCount: integer;
    FThreadId: TThreadID;

  public
    constructor Create;
    destructor Destroy; override;

    function WaitFor(Timeout: cardinal = INFINITE): boolean;
    procedure Release;

    property CurrentCount: integer read FCurrentCount;
  end;

  TStreamUtils = class
  public
    class function ReadLine(Stream: TStream; out Line: string; MaxLineLength: integer = -1;
      AEncoding: Encoding = nil): boolean;
    class procedure WriteLine(Stream: TStream; const Str: string);
  end;

  TScTCPConnection = class
  private
    FVio: TCRVioTcp;
    FOwner: boolean;
    FOnClose: TNotifyEvent;
    FOnInternalClose: TNotifyEvent;

    function GetBindAddress: string;
    procedure SetBindAddress(const Value: string);
    function GetHost: string;
    procedure SetHost(const Value: string);
    function GetPort: integer;
    procedure SetPort(const Value: integer);
    function GetIPVersion: TIPVersion;
    procedure SetIPVersion(const Value: TIPVersion);
    function GetConnected: boolean;
    function GetConnectionTimeout: integer;
    procedure SetConnectionTimeout(const Value: integer);
    function GetSendTimeout: integer;
    procedure SetSendTimeout(const Value: integer);
    function GetReceiveTimeout: integer;
    procedure SetReceiveTimeout(const Value: integer);
    function GetLocalSockAddr: PSockAddr;
    function GetRemoteSockAddr: PSockAddr;
    procedure DoOnClose(Sender: TObject);

  public
    constructor Create; overload;
    constructor Create(VioTcp: TCRVioTcp; Owner: boolean); overload;
    destructor Destroy; override;

    procedure Bind;
    procedure Connect;
    procedure Close;

    function Read(var Buffer; Count: integer): integer; overload;
    function Read(var Buffer: TBytes; Offset, Count: integer): integer; overload;
    function Write(const Buffer; Count: integer): integer; overload;
    function Write(const Buffer: TBytes; Offset, Count: integer): integer; overload;
    function WaitForData(MillisecondsTimeout: integer = -1): boolean;

    function GetLocalIP: string;
    function GetLocalPort: integer;
    function GetRemoteIP: string;
    function GetRemotePort: integer;

    property LocalSockAddr: PSockAddr read GetLocalSockAddr;
    property RemoteSockAddr: PSockAddr read GetRemoteSockAddr;

    property BindAddress: string read GetBindAddress write SetBindAddress;
    property Host: string read GetHost write SetHost;
    property Port: integer read GetPort write SetPort;
    property IPVersion: TIPVersion read GetIPVersion write SetIPVersion;

    property Connected: boolean read GetConnected;
    property ConnectionTimeout: integer read GetConnectionTimeout write SetConnectionTimeout;
    property SendTimeout: integer read GetSendTimeout write SetSendTimeout;
    property ReceiveTimeout: integer read GetReceiveTimeout write SetReceiveTimeout;

    property Vio: TCRVioTcp read FVio;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
    property OnInternalClose: TNotifyEvent read FOnInternalClose write FOnInternalClose;
  end;

  TScLogMessageEvent = procedure (Sender: TObject; const Message: string) of object;
  TScLogErrorEvent = procedure (Sender: TObject; const Message: string; E: Exception) of object;

  TScLogger = class
  private
    FLogLock: TCriticalSection;

    FOnLogDebug: TScLogMessageEvent;
    FOnLogInformation: TScLogMessageEvent;
    FOnLogWarning: TScLogMessageEvent;
    FOnLogError: TScLogErrorEvent;

  protected
    procedure LogDebug(const Message: string); virtual;
    procedure LogInformation(const Message: string); virtual;
    procedure LogWarning(const Message: string); virtual;
    procedure LogError(const Message: string; E: Exception); virtual;

  public
    constructor Create;
    destructor Destroy; override;

    property OnLogDebug: TScLogMessageEvent read FOnLogDebug write FOnLogDebug;
    property OnLogInformation: TScLogMessageEvent read FOnLogInformation write FOnLogInformation;
    property OnLogWarning: TScLogMessageEvent read FOnLogWarning write FOnLogWarning;
    property OnLogError: TScLogErrorEvent read FOnLogError write FOnLogError;
  end;

  Log = class
  public
    class procedure LogDebug(Logger: TScLogger; const Message: string); overload;
    class procedure LogDebug(Logger: TScLogger; const Message: string; const Args: array of const); overload;
    class procedure LogInformation(Logger: TScLogger; const Message: string); overload;
    class procedure LogInformation(Logger: TScLogger; const Message: string; const Args: array of const); overload;
    class procedure LogWarning(Logger: TScLogger; const Message: string); overload;
    class procedure LogWarning(Logger: TScLogger; const Message: string; const Args: array of const); overload;
    class procedure LogError(Logger: TScLogger; const Message: string; E: Exception); overload;
    class procedure LogError(Logger: TScLogger; const Message: string; const Args: array of const; E: Exception); overload;
  end;

function EncodingByCharset(const Charset: string): Encoding;
function Split(const Str: string; const Separator: Char): TStringList; overload;
procedure Split(Strings: TStrings; const Str: string; const Separator: Char; ATrim: Boolean); overload;
function DelSpaces(const s: string): string;
function BytesToHexStr(const Buffer: TBytes): string; overload;
function BytesToHexStr(const Buffer: TBytes; const Separator: string): string; overload;
function HexToBytes(const Text: string): TBytes;
function IsAlpha(const Ch: Char): Boolean;
function IncludeBrackets(const Str: string): string;
function UnquoteStr(const Str: string): string;
function PosOfAnyFromSet(const AlowedChars, ValueStr: string; StartPos: integer = 1): integer;
function PosOfAnyExceptSet(const ExceptChars, ValueStr: string; StartPos: integer = 1): integer;
function ScMatchesMask(const Filename, Mask: string): boolean;
function GetUnixTime: cardinal;  // in seconds
function GetLocalTimeZoneOffset: TDateTime;
function GetUniqueFileName(Path: string = ''): string;

procedure Lock(obj: TCriticalSection);
procedure Unlock(obj: TCriticalSection);

function CloneException(E: Exception): Exception;
function GetSocketError: integer;

{$IFDEF VER16P}
{$IFNDEF MSWINDOWS}
function GetCurrentThreadID: TThreadID;
{$ENDIF}
{$ENDIF}

{$IFNDEF VER16P}
function GetHomePath: string;
{$ENDIF}

procedure LogMessage(const Message: string);

{$IFDEF POSIX}
type
  TSockAddr = sockaddr;
  {$EXTERNALSYM TSockAddr}
{$ENDIF}

{$IFNDEF VER7P}
const
  HoursPerDay = 24;
  MinsPerHour = 60;
  SecsPerMin  = 60;

  { Days between TDateTime basis (12/31/1899) and Unix time_t basis (1/1/1970) }
  UnixDateDelta = 25569;
{$ENDIF}

var
  DefaultLogger: TScLogger;
  LockLog: TCriticalSection;

var
  DH_PRIME_GROUP_1024, DH_PRIME_GROUP_1536, DH_PRIME_GROUP_2048, DH_PRIME_GROUP_3072,
  DH_PRIME_GROUP_4096, DH_PRIME_GROUP_6144, DH_PRIME_GROUP_8192: TBigInteger;

const
  DEFAULT_ZBUFFER_SIZE = 32 * 1024;

const
  DACProductName = 'SecureBridge';

implementation

uses
  StrUtils, Types, Math,
{$IFDEF SBRIDGE}
{$IFNDEF ODBC_DRIVER}
  ScHttp, ScWebSocketClient, ScSignalRProtocol,
{$ENDIF}
{$ENDIF}
{$IFDEF MSWINDOWS}
  SysConst, Registry;
{$ENDIF}
{$IFDEF POSIX}
  Posix.SysTime, Posix.Time, Posix.UTime,
  Posix.NetinetIn, Posix.Errno;
{$ENDIF}
{$IFDEF UNIX}
  unixutil;
{$ENDIF}

resourcestring
  SInvalidMask = '''%s'' is an invalid mask at (%d)';

const
  TwoHexLookup: packed array[0..255] of array[1..2] of Char =
  ('00','01','02','03','04','05','06','07','08','09','0a','0b','0c','0d','0e','0f',
   '10','11','12','13','14','15','16','17','18','19','1a','1b','1c','1d','1e','1f',
   '20','21','22','23','24','25','26','27','28','29','2a','2b','2c','2d','2e','2f',
   '30','31','32','33','34','35','36','37','38','39','3a','3b','3c','3d','3e','3f',
   '40','41','42','43','44','45','46','47','48','49','4a','4b','4c','4d','4e','4f',
   '50','51','52','53','54','55','56','57','58','59','5a','5b','5c','5d','5e','5f',
   '60','61','62','63','64','65','66','67','68','69','6a','6b','6c','6d','6e','6f',
   '70','71','72','73','74','75','76','77','78','79','7a','7b','7c','7d','7e','7f',
   '80','81','82','83','84','85','86','87','88','89','8a','8b','8c','8d','8e','8f',
   '90','91','92','93','94','95','96','97','98','99','9a','9b','9c','9d','9e','9f',
   'a0','a1','a2','a3','a4','a5','a6','a7','a8','a9','aa','ab','ac','ad','ae','af',
   'b0','b1','b2','b3','b4','b5','b6','b7','b8','b9','ba','bb','bc','bd','be','bf',
   'c0','c1','c2','c3','c4','c5','c6','c7','c8','c9','ca','cb','cc','cd','ce','cf',
   'd0','d1','d2','d3','d4','d5','d6','d7','d8','d9','da','db','dc','dd','de','df',
   'e0','e1','e2','e3','e4','e5','e6','e7','e8','e9','ea','eb','ec','ed','ee','ef',
   'f0','f1','f2','f3','f4','f5','f6','f7','f8','f9','fa','fb','fc','fd','fe','ff');


{$IFDEF VER16P}
{$IFNDEF MSWINDOWS}
function GetCurrentThreadID: TThreadID;
begin
  Result := TThread.CurrentThread.ThreadID;
end;
{$ENDIF}
{$ENDIF}

{$IFNDEF VER16P}
function GetHomePath: string;
begin
  Result := GetEnvironmentVariable('HOME');
end;
{$ENDIF}

function GetSocketError: integer;
begin
{$IFDEF MSWINDOWS}
  Result := WSAGetLastError;
{$ELSE}
  Result := errno;
{$ENDIF}
end;

type
  TScCharsetSpec = record
    Name: string;
    CodePage: cardinal;
  end;

const
  SC_CHARSETS: array[0..404] of TScCharsetSpec = (
    (Name: ''; CodePage: 0),
    (Name: 'US-ASCII'; CodePage: 20127),
    (Name: 'ANSI_X3.4-1968'; CodePage: 20127),
    (Name: 'iso-ir-6'; CodePage: 20127),
    (Name: 'ANSI_X3.4-1986'; CodePage: 20127),
    (Name: 'ISO_646.irv:1991'; CodePage: 20127),
    (Name: 'ASCII'; CodePage: 20127),
    (Name: 'ISO646-US'; CodePage: 20127),
    (Name: 'us'; CodePage: 20127),
    (Name: 'IBM367'; CodePage: 20127),
    (Name: 'cp367'; CodePage: 20127),
    (Name: 'csASCII'; CodePage: 20127),
    (Name: 'KS_C_5601-1987'; CodePage: 949),
    (Name: 'iso-ir-149'; CodePage: 949),
    (Name: 'KS_C_5601-1989'; CodePage: 949),
    (Name: 'KSC_5601'; CodePage: 949),
    (Name: 'korean'; CodePage: 949),
    (Name: 'csKSC56011987'; CodePage: 949),
    (Name: 'ISO-2022-KR'; CodePage: 50225),
    (Name: 'csISO2022KR'; CodePage: 50225),
    (Name: 'EUC-KR'; CodePage: 51949),
    (Name: 'csEUCKR'; CodePage: 51949),
    (Name: 'ISO-2022-JP'; CodePage: 50220),
    (Name: 'csISO2022JP'; CodePage: 50221),
    (Name: 'GB_2312-80'; CodePage: 936),
    (Name: 'iso-ir-58'; CodePage: 936),
    (Name: 'chinese'; CodePage: 936),
    (Name: 'csISO58GB231280'; CodePage: 936),
    (Name: 'ISO-8859-1'; CodePage: 28591),
    (Name: 'ISO_8859-1:1987'; CodePage: 28591),
    (Name: 'iso-ir-100'; CodePage: 28591),
    (Name: 'ISO_8859-1'; CodePage: 28591),
    (Name: 'latin1'; CodePage: 28591),
    (Name: 'l1'; CodePage: 28591),
    (Name: 'IBM819'; CodePage: 28591),
    (Name: 'CP819'; CodePage: 28591),
    (Name: 'csISOLatin1'; CodePage: 28591),
    (Name: 'ISO-8859-2'; CodePage: 28592),
    (Name: 'ISO_8859-2:1987'; CodePage: 28592),
    (Name: 'iso-ir-101'; CodePage: 28592),
    (Name: 'ISO_8859-2'; CodePage: 28592),
    (Name: 'latin2'; CodePage: 28592),
    (Name: 'l2'; CodePage: 28592),
    (Name: 'csISOLatin2'; CodePage: 28592),
    (Name: 'ISO-8859-3'; CodePage: 28593),
    (Name: 'ISO_8859-3:1988'; CodePage: 28593),
    (Name: 'iso-ir-109'; CodePage: 28593),
    (Name: 'ISO_8859-3'; CodePage: 28593),
    (Name: 'latin3'; CodePage: 28593),
    (Name: 'l3'; CodePage: 28593),
    (Name: 'csISOLatin3'; CodePage: 28593),
    (Name: 'ISO-8859-4'; CodePage: 28594),
    (Name: 'ISO_8859-4:1988'; CodePage: 28594),
    (Name: 'iso-ir-110'; CodePage: 28594),
    (Name: 'ISO_8859-4'; CodePage: 28594),
    (Name: 'latin4'; CodePage: 28594),
    (Name: 'l4'; CodePage: 28594),
    (Name: 'csISOLatin4'; CodePage: 28594),
    (Name: 'ISO-8859-6'; CodePage: 28596),
    (Name: 'ISO_8859-6:1987'; CodePage: 708),
    (Name: 'iso-ir-127'; CodePage: 708),
    (Name: 'ISO_8859-6'; CodePage: 708),
    (Name: 'ECMA-114'; CodePage: 708),
    (Name: 'ASMO-708'; CodePage: 708),
    (Name: 'arabic'; CodePage: 708),
    (Name: 'csISOLatinArabic'; CodePage: 708),
    (Name: 'ISO-8859-7'; CodePage: 28597),
    (Name: 'ISO_8859-7:1987'; CodePage: 28597),
    (Name: 'iso-ir-126'; CodePage: 28597),
    (Name: 'ISO_8859-7'; CodePage: 28597),
    (Name: 'ELOT_928'; CodePage: 28597),
    (Name: 'ECMA-118'; CodePage: 28597),
    (Name: 'greek'; CodePage: 28597),
    (Name: 'greek8'; CodePage: 28597),
    (Name: 'csISOLatinGreek'; CodePage: 28597),
    (Name: 'ISO-8859-8'; CodePage: 28598),
    (Name: 'ISO_8859-8:1988'; CodePage: 28598),
    (Name: 'iso-ir-138'; CodePage: 28598),
    (Name: 'ISO_8859-8'; CodePage: 28598),
    (Name: 'hebrew'; CodePage: 28598),
    (Name: 'csISOLatinHebrew'; CodePage: 28598),
    (Name: 'ISO-8859-8-I'; CodePage: 38598),
    (Name: 'ISO_8859-8-I'; CodePage: 38598),
    (Name: 'csISO88598I'; CodePage: 38598),
    (Name: 'ISO-8859-5'; CodePage: 28595),
    (Name: 'ISO_8859-5:1988'; CodePage: 28595),
    (Name: 'iso-ir-144'; CodePage: 28595),
    (Name: 'ISO_8859-5'; CodePage: 28595),
    (Name: 'cyrillic'; CodePage: 28595),
    (Name: 'csISOLatinCyrillic'; CodePage: 28595),
    (Name: 'ISO-8859-9'; CodePage: 28599),
    (Name: 'ISO_8859-9:1989'; CodePage: 28599),
    (Name: 'iso-ir-148'; CodePage: 28599),
    (Name: 'ISO_8859-9'; CodePage: 28599),
    (Name: 'latin5'; CodePage: 28599),
    (Name: 'l5'; CodePage: 28599),
    (Name: 'csISOLatin5'; CodePage: 28599),
    (Name: 'macintosh'; CodePage: 10000),
    (Name: 'mac'; CodePage: 10000),
    (Name: 'csMacintosh'; CodePage: 10000),
    (Name: 'IBM037'; CodePage: 37),
    (Name: 'cp037'; CodePage: 37),
    (Name: 'ebcdic-cp-us'; CodePage: 37),
    (Name: 'ebcdic-cp-ca'; CodePage: 37),
    (Name: 'ebcdic-cp-wt'; CodePage: 37),
    (Name: 'ebcdic-cp-nl'; CodePage: 37),
    (Name: 'csIBM037'; CodePage: 37),
    (Name: 'IBM273'; CodePage: 20273),
    (Name: 'CP273'; CodePage: 20273),
    (Name: 'csIBM273'; CodePage: 20273),
    (Name: 'IBM277'; CodePage: 20277),
    (Name: 'EBCDIC-CP-DK'; CodePage: 20277),
    (Name: 'EBCDIC-CP-NO'; CodePage: 20277),
    (Name: 'csIBM277'; CodePage: 20277),
    (Name: 'IBM278'; CodePage: 20278),
    (Name: 'CP278'; CodePage: 20278),
    (Name: 'ebcdic-cp-fi'; CodePage: 20278),
    (Name: 'ebcdic-cp-se'; CodePage: 20278),
    (Name: 'csIBM278'; CodePage: 20278),
    (Name: 'IBM280'; CodePage: 20280),
    (Name: 'CP280'; CodePage: 20280),
    (Name: 'ebcdic-cp-it'; CodePage: 20280),
    (Name: 'csIBM280'; CodePage: 20280),
    (Name: 'IBM284'; CodePage: 20284),
    (Name: 'CP284'; CodePage: 20284),
    (Name: 'ebcdic-cp-es'; CodePage: 20284),
    (Name: 'csIBM284'; CodePage: 20284),
    (Name: 'IBM285'; CodePage: 20285),
    (Name: 'CP285'; CodePage: 20285),
    (Name: 'ebcdic-cp-gb'; CodePage: 20285),
    (Name: 'csIBM285'; CodePage: 20285),
    (Name: 'IBM290'; CodePage: 20290),
    (Name: 'cp290'; CodePage: 20290),
    (Name: 'EBCDIC-JP-kana'; CodePage: 20290),
    (Name: 'csIBM290'; CodePage: 20290),
    (Name: 'IBM297'; CodePage: 20297),
    (Name: 'cp297'; CodePage: 20297),
    (Name: 'ebcdic-cp-fr'; CodePage: 20297),
    (Name: 'csIBM297'; CodePage: 20297),
    (Name: 'IBM420'; CodePage: 20420),
    (Name: 'cp420'; CodePage: 20420),
    (Name: 'ebcdic-cp-ar1'; CodePage: 20420),
    (Name: 'csIBM420'; CodePage: 20420),
    (Name: 'IBM423'; CodePage: 20423),
    (Name: 'cp423'; CodePage: 20423),
    (Name: 'ebcdic-cp-gr'; CodePage: 20423),
    (Name: 'csIBM423'; CodePage: 20423),
    (Name: 'IBM424'; CodePage: 20424),
    (Name: 'cp424'; CodePage: 20424),
    (Name: 'ebcdic-cp-he'; CodePage: 20424),
    (Name: 'csIBM424'; CodePage: 20424),
    (Name: 'IBM437'; CodePage: 437),
    (Name: 'cp437'; CodePage: 437),
    (Name: '437'; CodePage: 437),
    (Name: 'csPC8CodePage437'; CodePage: 437),
    (Name: 'IBM500'; CodePage: 500),
    (Name: 'CP500'; CodePage: 500),
    (Name: 'ebcdic-cp-be'; CodePage: 500),
    (Name: 'ebcdic-cp-ch'; CodePage: 500),
    (Name: 'csIBM500'; CodePage: 500),
    (Name: 'IBM775'; CodePage: 775),
    (Name: 'cp775'; CodePage: 775),
    (Name: 'csPC775Baltic'; CodePage: 775),
    (Name: 'IBM850'; CodePage: 850),
    (Name: 'cp850'; CodePage: 850),
    (Name: '850'; CodePage: 850),
    (Name: 'csPC850Multilingual'; CodePage: 850),
    (Name: 'IBM852'; CodePage: 852),
    (Name: 'cp852'; CodePage: 852),
    (Name: '852'; CodePage: 852),
    (Name: 'csPCp852'; CodePage: 852),
    (Name: 'IBM855'; CodePage: 855),
    (Name: 'cp855'; CodePage: 855),
    (Name: '855'; CodePage: 855),
    (Name: 'csIBM855'; CodePage: 855),
    (Name: 'IBM857'; CodePage: 857),
    (Name: 'cp857'; CodePage: 857),
    (Name: '857'; CodePage: 857),
    (Name: 'csIBM857'; CodePage: 857),
    (Name: 'IBM860'; CodePage: 860),
    (Name: 'cp860'; CodePage: 860),
    (Name: '860'; CodePage: 860),
    (Name: 'csIBM860'; CodePage: 860),
    (Name: 'IBM861'; CodePage: 861),
    (Name: 'cp861'; CodePage: 861),
    (Name: '861'; CodePage: 861),
    (Name: 'cp-is'; CodePage: 861),
    (Name: 'csIBM861'; CodePage: 861),
    (Name: 'IBM863'; CodePage: 863),
    (Name: 'cp863'; CodePage: 863),
    (Name: '863'; CodePage: 863),
    (Name: 'csIBM863'; CodePage: 863),
    (Name: 'IBM864'; CodePage: 864),
    (Name: 'cp864'; CodePage: 864),
    (Name: 'csIBM864'; CodePage: 864),
    (Name: 'IBM865'; CodePage: 865),
    (Name: 'cp865'; CodePage: 865),
    (Name: '865'; CodePage: 865),
    (Name: 'csIBM865'; CodePage: 865),
    (Name: 'IBM866'; CodePage: 866),
    (Name: 'cp866'; CodePage: 866),
    (Name: '866'; CodePage: 866),
    (Name: 'csIBM866'; CodePage: 866),
    (Name: 'IBM869'; CodePage: 869),
    (Name: 'cp869'; CodePage: 869),
    (Name: '869'; CodePage: 869),
    (Name: 'cp-gr'; CodePage: 869),
    (Name: 'csIBM869'; CodePage: 869),
    (Name: 'IBM870'; CodePage: 870),
    (Name: 'CP870'; CodePage: 870),
    (Name: 'ebcdic-cp-roece'; CodePage: 870),
    (Name: 'ebcdic-cp-yu'; CodePage: 870),
    (Name: 'csIBM870'; CodePage: 870),
    (Name: 'IBM871'; CodePage: 20871),
    (Name: 'CP871'; CodePage: 20871),
    (Name: 'ebcdic-cp-is'; CodePage: 20871),
    (Name: 'csIBM871'; CodePage: 20871),
    (Name: 'IBM880'; CodePage: 20880),
    (Name: 'cp880'; CodePage: 20880),
    (Name: 'EBCDIC-Cyrillic'; CodePage: 20880),
    (Name: 'csIBM880'; CodePage: 20880),
    (Name: 'IBM905'; CodePage: 20905),
    (Name: 'CP905'; CodePage: 20905),
    (Name: 'ebcdic-cp-tr'; CodePage: 20905),
    (Name: 'csIBM905'; CodePage: 20905),
    (Name: 'IBM1026'; CodePage: 1026),
    (Name: 'CP1026'; CodePage: 1026),
    (Name: 'csIBM1026'; CodePage: 1026),
    (Name: 'KOI8-R'; CodePage: 20866),
    (Name: 'csKOI8R'; CodePage: 20866),
    (Name: 'KOI8-U'; CodePage: 21866),
    (Name: 'IBM00858'; CodePage: 858),
    (Name: 'CCSID00858'; CodePage: 858),
    (Name: 'CP00858'; CodePage: 858),
    (Name: 'PC-Multilingual-850+euro'; CodePage: 858),
    (Name: 'IBM00924'; CodePage: 20924),
    (Name: 'CCSID00924'; CodePage: 20924),
    (Name: 'CP00924'; CodePage: 20924),
    (Name: 'ebcdic-Latin9--euro'; CodePage: 20924),
    (Name: 'IBM01140'; CodePage: 1140),
    (Name: 'CCSID01140'; CodePage: 1140),
    (Name: 'CP01140'; CodePage: 1140),
    (Name: 'ebcdic-us-37+euro'; CodePage: 1140),
    (Name: 'IBM01141'; CodePage: 1141),
    (Name: 'CCSID01141'; CodePage: 1141),
    (Name: 'CP01141'; CodePage: 1141),
    (Name: 'ebcdic-de-273+euro'; CodePage: 1141),
    (Name: 'IBM01142'; CodePage: 1142),
    (Name: 'CCSID01142'; CodePage: 1142),
    (Name: 'CP01142'; CodePage: 1142),
    (Name: 'ebcdic-dk-277+euro'; CodePage: 1142),
    (Name: 'ebcdic-no-277+euro'; CodePage: 1142),
    (Name: 'IBM01143'; CodePage: 1143),
    (Name: 'CCSID01143'; CodePage: 1143),
    (Name: 'CP01143'; CodePage: 1143),
    (Name: 'ebcdic-fi-278+euro'; CodePage: 1143),
    (Name: 'ebcdic-se-278+euro'; CodePage: 1143),
    (Name: 'IBM01144'; CodePage: 1144),
    (Name: 'CCSID01144'; CodePage: 1144),
    (Name: 'CP01144'; CodePage: 1144),
    (Name: 'ebcdic-it-280+euro'; CodePage: 1144),
    (Name: 'IBM01145'; CodePage: 1145),
    (Name: 'CCSID01145'; CodePage: 1145),
    (Name: 'CP01145'; CodePage: 1145),
    (Name: 'ebcdic-es-284+euro'; CodePage: 1145),
    (Name: 'IBM01146'; CodePage: 1146),
    (Name: 'CCSID01146'; CodePage: 1146),
    (Name: 'CP01146'; CodePage: 1146),
    (Name: 'ebcdic-gb-285+euro'; CodePage: 1146),
    (Name: 'IBM01147'; CodePage: 1147),
    (Name: 'CCSID01147'; CodePage: 1147),
    (Name: 'CP01147'; CodePage: 1147),
    (Name: 'ebcdic-fr-297+euro'; CodePage: 1147),
    (Name: 'IBM01148'; CodePage: 1148),
    (Name: 'CCSID01148'; CodePage: 1148),
    (Name: 'CP01148'; CodePage: 1148),
    (Name: 'ebcdic-international-500+euro'; CodePage: 1148),
    (Name: 'IBM01149'; CodePage: 1149),
    (Name: 'CCSID01149'; CodePage: 1149),
    (Name: 'CP01149'; CodePage: 1149),
    (Name: 'ebcdic-is-871+euro'; CodePage: 1149),
    (Name: 'UTF-16BE'; CodePage: 1201),
    (Name: 'UTF-16LE'; CodePage: 1200),
    (Name: 'UTF-16'; CodePage: 1200),
    (Name: 'UTF-32'; CodePage: 12000),
    (Name: 'UTF-32BE'; CodePage: 12001),
    (Name: 'UTF-32LE'; CodePage: 12000),
    (Name: 'UTF-8'; CodePage: 65001),
    (Name: 'ISO-8859-13'; CodePage: 28603),
    (Name: 'ISO-8859-15'; CodePage: 28605),
    (Name: 'ISO_8859-15'; CodePage: 28605),
    (Name: 'Latin-9'; CodePage: 28605),
    (Name: 'GBK'; CodePage: 936),
    (Name: 'CP936'; CodePage: 936),
    (Name: 'MS936'; CodePage: 936),
    (Name: 'windows-936'; CodePage: 936),
    (Name: 'GB18030'; CodePage: 54936),
    (Name: 'Shift_JIS'; CodePage: 932),
    (Name: 'MS_Kanji'; CodePage: 932),
    (Name: 'csShiftJIS'; CodePage: 932),
    (Name: 'EUC-JP'; CodePage: 20932),
    (Name: 'Extended_UNIX_Code_Packed_Format_for_Japanese'; CodePage: 20932),
    (Name: 'csEUCPkdFmtJapanese'; CodePage: 20932),
    (Name: 'DOS-862'; CodePage: 862),
    (Name: 'windows-874'; CodePage: 874),
    (Name: 'cp875'; CodePage: 875),
    (Name: 'IBM01047'; CodePage: 1047),
    (Name: 'unicodeFFFE'; CodePage: 1201),
    (Name: 'Johab'; CodePage: 1361),
    (Name: 'x-mac-japanese'; CodePage: 10001),
    (Name: 'x-mac-chinesetrad'; CodePage: 10002),
    (Name: 'x-mac-korean'; CodePage: 10003),
    (Name: 'x-mac-arabic'; CodePage: 10004),
    (Name: 'x-mac-hebrew'; CodePage: 10005),
    (Name: 'x-mac-greek'; CodePage: 10006),
    (Name: 'x-mac-cyrillic'; CodePage: 10007),
    (Name: 'x-mac-chinesesimp'; CodePage: 10008),
    (Name: 'x-mac-romanian'; CodePage: 10010),
    (Name: 'x-mac-ukrainian'; CodePage: 10017),
    (Name: 'x-mac-thai'; CodePage: 10021),
    (Name: 'x-mac-ce'; CodePage: 10029),
    (Name: 'x-mac-icelandic'; CodePage: 10079),
    (Name: 'x-mac-turkish'; CodePage: 10081),
    (Name: 'x-mac-croatian'; CodePage: 10082),
    (Name: 'x-Chinese-CNS'; CodePage: 20000),
    (Name: 'x-cp20001'; CodePage: 20001),
    (Name: 'x-Chinese-Eten'; CodePage: 20002),
    (Name: 'x-cp20003'; CodePage: 20003),
    (Name: 'x-cp20004'; CodePage: 20004),
    (Name: 'x-cp20005'; CodePage: 20005),
    (Name: 'x-IA5'; CodePage: 20105),
    (Name: 'x-IA5-German'; CodePage: 20106),
    (Name: 'x-IA5-Swedish'; CodePage: 20107),
    (Name: 'x-IA5-Norwegian'; CodePage: 20108),
    (Name: 'x-cp20261'; CodePage: 20261),
    (Name: 'x-cp20269'; CodePage: 20269),
    (Name: 'x-EBCDIC-KoreanExtended'; CodePage: 20833),
    (Name: 'x-cp20936'; CodePage: 20936),
    (Name: 'x-cp20949'; CodePage: 20949),
    (Name: 'cp1025'; CodePage: 21025),
    (Name: 'x-Europa'; CodePage: 29001),
    (Name: 'x-cp50227'; CodePage: 50227),
    (Name: 'EUC-CN'; CodePage: 51936),
    (Name: 'x-iscii-de'; CodePage: 57002),
    (Name: 'x-iscii-be'; CodePage: 57003),
    (Name: 'x-iscii-ta'; CodePage: 57004),
    (Name: 'x-iscii-te'; CodePage: 57005),
    (Name: 'x-iscii-as'; CodePage: 57006),
    (Name: 'x-iscii-or'; CodePage: 57007),
    (Name: 'x-iscii-ka'; CodePage: 57008),
    (Name: 'x-iscii-ma'; CodePage: 57009),
    (Name: 'x-iscii-gu'; CodePage: 57010),
    (Name: 'x-iscii-pa'; CodePage: 57011),
    (Name: 'x-EBCDIC-Arabic'; CodePage: 20420),
    (Name: 'x-EBCDIC-CyrillicRussian'; CodePage: 20880),
    (Name: 'x-EBCDIC-CyrillicSerbianBulgarian'; CodePage: 21025),
    (Name: 'x-EBCDIC-DenmarkNorway'; CodePage: 20277),
    (Name: 'x-ebcdic-denmarknorway-euro'; CodePage: 1142),
    (Name: 'x-EBCDIC-FinlandSweden'; CodePage: 20278),
    (Name: 'x-ebcdic-finlandsweden-euro'; CodePage: 1143),
    (Name: 'X-EBCDIC-France'; CodePage: 1143),
    (Name: 'x-ebcdic-france-euro'; CodePage: 1147),
    (Name: 'x-EBCDIC-Germany'; CodePage: 20273),
    (Name: 'x-ebcdic-germany-euro'; CodePage: 1141),
    (Name: 'x-EBCDIC-GreekModern'; CodePage: 875),
    (Name: 'x-EBCDIC-Greek'; CodePage: 20423),
    (Name: 'x-EBCDIC-Hebrew'; CodePage: 20424),
    (Name: 'x-EBCDIC-Icelandic'; CodePage: 20871),
    (Name: 'x-ebcdic-icelandic-euro'; CodePage: 1149),
    (Name: 'x-ebcdic-international-euro'; CodePage: 1148),
    (Name: 'x-EBCDIC-Italy'; CodePage: 20280),
    (Name: 'x-ebcdic-italy-euro'; CodePage: 1144),
    (Name: 'x-EBCDIC-JapaneseAndKana'; CodePage: 50930),
    (Name: 'x-EBCDIC-JapaneseAndJapaneseLatin'; CodePage: 50939),
    (Name: 'x-EBCDIC-JapaneseAndUSCanada'; CodePage: 50931),
    (Name: 'x-EBCDIC-JapaneseKatakana'; CodePage: 20290),
    (Name: 'x-EBCDIC-KoreanAndKoreanExtended'; CodePage: 50933),
    (Name: 'x-EBCDIC-SimplifiedChinese'; CodePage: 50935),
    (Name: 'X-EBCDIC-Spain'; CodePage: 20284),
    (Name: 'x-ebcdic-spain-euro'; CodePage: 1145),
    (Name: 'x-EBCDIC-Thai'; CodePage: 20838),
    (Name: 'x-EBCDIC-TraditionalChinese'; CodePage: 50937),
    (Name: 'x-EBCDIC-Turkish'; CodePage: 20905),
    (Name: 'x-EBCDIC-UK'; CodePage: 20285),
    (Name: 'x-ebcdic-uk-euro'; CodePage: 1146),
    (Name: 'x-ebcdic-cp-us-euro'; CodePage: 1140),
    (Name: 'UTF-7'; CodePage: 65000),
    (Name: 'IBM-Thai'; CodePage: 20838),
    (Name: 'csIBMThai'; CodePage: 20838),
    (Name: 'GB2312'; CodePage: 936),
    (Name: 'csGB2312'; CodePage: 936),
    (Name: 'Big5'; CodePage: 950),
    (Name: 'csBig5'; CodePage: 950),
    (Name: 'HZ-GB-2312'; CodePage: 52936),
    (Name: 'windows-1250'; CodePage: 1250),
    (Name: 'windows-1251'; CodePage: 1251),
    (Name: 'windows-1252'; CodePage: 1252),
    (Name: 'windows-1253'; CodePage: 1253),
    (Name: 'windows-1254'; CodePage: 1254),
    (Name: 'windows-1255'; CodePage: 1255),
    (Name: 'windows-1256'; CodePage: 1256),
    (Name: 'windows-1257'; CodePage: 1257),
    (Name: 'windows-1258'; CodePage: 1258),
    (Name: 'DOS-720'; CodePage: 720),
    (Name: 'ibm737'; CodePage: 737)
  );

function EncodingByCharset(const Charset: string): Encoding;
var
  CodePage: cardinal;
  cp: integer;
begin
  Result := nil;

  if Charset <> '' then begin
    CodePage := 0;
    for cp := Low(SC_CHARSETS) to High(SC_CHARSETS) do begin
      if AnsiSameText(SC_CHARSETS[cp].Name, Charset) then begin
        CodePage := SC_CHARSETS[cp].CodePage;
        Break;
      end;
    end;

    if CodePage <> 0 then
      Result := {$IFDEF NEXTGEN}Encoding{$ENDIF}(Encoding.GetEncoding(CodePage));
  end;

  if not Assigned(Result) then
    Result := Encoding.Default;
end;

function CloneException(E: Exception): Exception;
begin
  Result := nil;
  if E <> nil then
    if E.ClassName = EScError.ClassName then
      Result := EScError(E).Clone
    else
    if E.ClassName = SocketException.ClassName then
      Result := SocketException.Create(E.Message, SocketException(E).ErrorCode)
    else
  {$IFDEF SBRIDGE}
  {$IFNDEF ODBC_DRIVER}
    if E.ClassName = HttpException.ClassName then
      Result := HttpException.Create(HttpException(E).Code, E.Message, HttpException(E).ServerMessage)
    else
    if E.ClassName = WebSocketException.ClassName then
      Result := WebSocketException.Create(E.Message)
    else
    if E.ClassName = HubException.ClassName then
      Result := HubException.Create(E.Message)
    else
  {$ENDIF}
  {$ENDIF}
    if E.ClassName = ArgumentException.ClassName then
      Result := ArgumentException.Create(E.Message)
    else
    if E.ClassName = InvalidDataException.ClassName then
      Result := InvalidDataException.Create(E.Message)
    else
    if E.ClassName = InvalidOperationException.ClassName then
      Result := InvalidOperationException.Create(E.Message)
    else
    if E.ClassName = JSONException.ClassName then
      Result := JSONException.Create(E.Message)
    else
    if E.ClassName = OperationCanceledException.ClassName then
      Result := OperationCanceledException.Create(E.Message)
    else
      Result := Exception.Create(E.Message);
end;

procedure Lock(obj: TCriticalSection);
begin
  obj.Enter;
end;

procedure Unlock(obj: TCriticalSection);
begin
  obj.Leave;
end;

function Split(const Str: string; const Separator: Char): TStringList;
begin
  Result := TStringList.Create;
  Split(Result, Str, Separator, False);
end;

procedure Split(Strings: TStrings; const Str: string; const Separator: Char; ATrim: Boolean);
var
  TStr, Val: string;
  Pos1, Pos2: integer;
begin
  if Strings = nil then
    raise EScError.Create(seInvalidInputArgs);

  Strings.Clear;

  if ATrim then begin
    TStr := Trim(Str);
    if TStr = '' then
      Exit;
  end
  else
    TStr := Str;

  Pos1 := 1;

  for Pos2 := 1 to Length(TStr) do begin
    if TStr[Pos2] = Separator then begin
      if ATrim then
        Val := Trim(Copy(TStr, Pos1, Pos2 - Pos1))
      else
        Val := Copy(TStr, Pos1, Pos2 - Pos1);
      if Val <> '' then
        Strings.Add(Val);
      Pos1 := Pos2 + 1;
    end;
  end;

  if ATrim then
    Val := Trim(Copy(TStr, Pos1, Length(Str)))
  else
    Val := Copy(TStr, Pos1, Length(Str));
  if Val <> '' then
    Strings.Add(Val);
end;

function DelSpaces(const s: string): string;
var
  p: integer;
begin
  Result := s;
  p := Pos(' ', Result);
  while p > 0 do begin
    Result := Copy(Result, 1, p - 1) + Copy(Result, p + 1, Length(Result));
    p := Pos(' ', Result);
  end;
end;

function BytesToHexStr(const Buffer: TBytes): string;
var
  i, p: integer;
begin
  SetLength(Result, Length(Buffer) * 2);

  p := 1;
  for i := 0 to Length(Buffer) - 1 do begin
    Result[p] := TwoHexLookup[Buffer[i]][1];
    Result[p + 1] := TwoHexLookup[Buffer[i]][2];
    Inc(p, 2);
  end;
end;

function BytesToHexStr(const Buffer: TBytes; const Separator: string): string;
var
  i, p: integer;
begin
  if Length(Buffer) = 0 then begin
    Result := '';
    Exit;
  end;

  SetLength(Result, Length(Buffer) * 3 - 1);
  Result[1] := TwoHexLookup[Buffer[0]][1];
  Result[2] := TwoHexLookup[Buffer[0]][2];

  p := 3;
  for i := 1 to Length(Buffer) - 1 do begin
    Result[p] := Separator[1];
    Result[p + 1] := TwoHexLookup[Buffer[i]][1];
    Result[p + 2] := TwoHexLookup[Buffer[i]][2];
    Inc(p, 3);
  end;
end;

function HexToBytes(const Text: string): TBytes;
const
  Convert: array['0'..'f'] of Byte =
    ( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 0, 0, 0, 0, 0, 0,
      0,10,11,12,13,14,15, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
      0,10,11,12,13,14,15);
var
  i, j: integer;
  Size: integer;
begin
  if (Length(Text) and 1) = 1 then begin
    Size := (Length(Text) shr 1) + 1;
    SetLength(Result, Size);

    Result[0] := Convert[Text[1]];
    i := 1;
    j := 2;
  end
  else begin
    Size := Length(Text) shr 1;
    SetLength(Result, Size);

    i := 0;
    j := 1;
  end;

  while i < Size do begin
    if not CharInSet(Text[j], ['0'..'f']) or not CharInSet(Text[j + 1], ['0'..'f']) then
      Break;

    Result[i] := (Convert[Text[j]] shl 4) + Convert[Text[j + 1]];
    Inc(i);
    Inc(j, 2);
  end;
end;

function IsAlpha(const Ch: Char): Boolean;
begin
  Result := ((Ch >= 'a') and (Ch <= 'z')) or ((Ch >= 'A') and (Ch <= 'Z'));
end;

function IncludeBrackets(const Str: string): string;
begin
  Result := Str;
  if Result = '' then
    Exit;

  if Result[1] <> '<' then
    Result := '<' + Result;
  if Result[Length(Result)] <> '>' then
    Result := Result + '>';
end;

function UnquoteStr(const Str: string): string;
var
  i: integer;
begin
  Result := Str;
  if (Result = '') or (Result[1] <> '"') then
    Exit;

  for i := 2 to Length(Result) do begin
    if Result[i] = '"' then begin
      Result := Copy(Str, 2, i - 2);
      Exit;
    end;
  end;

  System.Delete(Result, 1, 1); // delete first '"'
end;

function PosOfAnyFromSet(const AlowedChars, ValueStr: string; StartPos: integer = 1): integer;
var
  CharsLen, ValueLen: integer;
  i, j: integer;
begin
  Result := 0;
  if AlowedChars = '' then
    Exit;

  ValueLen := Max(Length(ValueStr) - StartPos + 1, 0);
  if ValueLen = 0 then
    Exit;

  CharsLen := Length(AlowedChars);
  for i := 0 to ValueLen - 1 do begin
    for j := 1 to CharsLen do begin
      if ValueStr[StartPos] = AlowedChars[j] then begin
        Result := StartPos;
        Exit;
      end;
    end;

    Inc(StartPos);
  end;
end;

function PosOfAnyExceptSet(const ExceptChars, ValueStr: string; StartPos: integer = 1): integer;
var
  CharsLen, ValueLen: integer;
  i, j: integer;
  Found: boolean;
begin
  Result := 0;
  ValueLen := Max(Length(ValueStr) - StartPos + 1, 0);
  if ValueLen = 0 then
    Exit;

  if ExceptChars = '' then begin
    Result := StartPos;
    Exit;
  end;

  CharsLen := Length(ExceptChars);
  for i := 0 to ValueLen - 1 do begin
    Found := False;
    for j := 1 to CharsLen do begin
      if ValueStr[StartPos] = ExceptChars[j] then begin
        Found := True;
        Break;
      end;
    end;

    if not Found then begin
      Result := StartPos;
      Exit;
    end;

    Inc(StartPos);
  end;
end;

function GetLocalTimeZoneOffset: TDateTime;
{$IFDEF MSWINDOWS}
var
  TZI: TTimeZoneInformation;
  DL: integer;
  Bias: integer;
{$ENDIF}
{$IFDEF POSIX}
var
  T: time_t;
  TV: timeval;
  UT: tm;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  DL := GetTimeZoneInformation(TZI);
  Bias := TZI.Bias;
  if DL = 2 then
    Bias := Bias + TZI.DaylightBias;

  Result := EncodeTime(Abs(Bias) div 60, Abs(Bias) mod 60, 0, 0);
  if Bias > 0 then
    Result := 0.0 - Result;
{$ENDIF}
{$IFDEF POSIX}
  gettimeofday(TV, nil);
  T := TV.tv_sec;
  localtime_r(T, UT);
  Result := UT.tm_gmtoff / (60 * 60 * 24);
{$ENDIF}
{$IFDEF UNIX}
  Result := Tzseconds / (60 * 60 * 24);
{$ENDIF}
end;

function GetUnixTime: cardinal; // in seconds
var
  t: TDateTime;
  Hour, Min, Sec, MSec: Word;
  Days: integer;
begin
  t := Now - UnixDateDelta; {EncodeDate(1970, 1, 1)}
  Days := integer(Trunc(t));
  DecodeTime(t, Hour, Min, Sec, MSec);
  Result := Sec + Min * SecsPerMin + Hour * MinsPerHour * SecsPerMin + Days * SecsPerDay;
end;

function GetUniqueFileName(Path: string = ''): string;
const
  FILE_PREFIX = 'SBridge';
{$IFNDEF FPC}
var
  Filename: string;
  Ticks: UInt64;
{$IFDEF MSWINDOWS}
  Len: integer;
{$ENDIF}
{$ENDIF}
begin
{$IFDEF FPC}
  Result := GetTempFileName(Path, FILE_PREFIX);
{$ELSE}

{$IFDEF MSWINDOWS}
  if Path = '' then begin
    SetLength(Path, MAX_PATH);
    Len := GetTempPath(MAX_PATH, PChar(Path));
    if Len > 0 then begin
      SetLength(Path, Len);
      Path := IncludeTrailingPathDelimiter(Path);
    end
    else
      Result := '';
  end;
{$ELSE}
  if Path = '' then
    Path := System.IOUtils.TPath.GetTempPath;
{$ENDIF}

  if (Path <> '') and DirectoryExists(Path) then
    Filename := IncludeTrailingPathDelimiter(Path) + FILE_PREFIX
  else
    Filename := FILE_PREFIX;

  Ticks := UInt64({$IFDEF FPC}GetTickCount64{$ELSE}GetTickCount{$ENDIF});
  repeat
    Result := Filename + IntToHex(Ticks, 8) + {$IFNDEF UNIX}'.tmp'{$ENDIF};
    if not FileExists(Result) then
      Break;
    Inc(Ticks);
  until False;
{$ENDIF}
end;

{ EScError }

constructor EScError.Create(const Msg: string);
begin
  inherited Create(Msg);
  FErrorCode := seInternalError;
end;

constructor EScError.Create(ErrorCode: TScErrorCode);
begin
  inherited Create(ScErrorMessages[ErrorCode]);
  FErrorCode := ErrorCode;
end;

constructor EScError.CreateFmt(const Msg: string; const Args: array of const; ErrorCode: TScErrorCode);
begin
  inherited CreateFmt(Msg, Args);
  FErrorCode := ErrorCode;
end;

function EScError.Clone: EScError;
begin
  Result := EScError.Create(Self.Message);
  Result.FErrorCode := Self.FErrorCode;
end;

{ TScCollectionItem }

function TScCollectionItem.GetAsString: string;
begin
  Result := '';
end;

procedure TScCollectionItem.SetAsString(const Value: string);
begin
end;

{ TScCollection }

constructor TScCollection.Create(AOwner: TPersistent; ItemClass: TScCollectionItemClass);
begin
  inherited Create(AOwner, ItemClass);
end;

procedure TScCollection.Update(Item: TCollectionItem);
begin
  if Assigned(OnChanged) then OnChanged(Self);
end;

procedure TScCollection.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

function TScCollection.GetAsString: string;
var
  b: StringBuilder;
  i: Integer;
begin
  b := StringBuilder.Create;
  try
    for i := 0 to Count - 1 do begin
      if i > 0 then
        b.Append(',');
      b.Append(TScCollectionItem(Items[i]).AsString);
    end;

    Result := b.ToString;
  finally
    b.Free;
  end;
end;

procedure TScCollection.SetAsString(const Value: string);
var
  ind1, ind2: Integer;
  len: Integer;
begin
  if AsString <> Value then begin
    Clear;
    len := Length(TrimRight(Value));
    if len = 0 then
      Exit;

    ind2 := 0;
    while ind2 < len do begin
      ind1 := ind2 + 1;
      ind2 := PosEx(',', Value, ind1);
      if ind2 = 0 then
        ind2 := len + 1;

      TScCollectionItem(Add).AsString := Trim(Copy(Value, ind1, ind2 - ind1));
    end;
  end;
end;

{ TScStreamInfo }

constructor TScStreamInfo.Create(Stream: TStream; Position, Count: Int64);
begin
  inherited Create;
  Init(Stream, Position, Count);
end;

procedure TScStreamInfo.Init(Stream: TStream; Position, Count: Int64);
begin
  FStream := Stream;
  FPosition := Position;
  FCount := Count;
end;

procedure TScStreamInfo.Assign(Source: TScStreamInfo);
begin
  if Source = nil then
    raise EScError.Create(seInvalidInputArgs);

  FStream := Source.FStream;
  FPosition := Source.FPosition;
  FCount := Source.FCount;
end;

procedure TScStreamInfo.Assign(const Source: TScStreamInfoRec);
begin
  FStream := Source.Stream;
  FPosition := Source.Position;
  FCount := Source.Count;
end;

{ TStreamUtils }

class function TStreamUtils.ReadLine(Stream: TStream; out Line: string; MaxLineLength: integer = -1;
  AEncoding: Encoding = nil): boolean;
const
  BLOCK_SIZE = 4096;
var
  LineLen, ReadOffset, ReadCount: integer;
  ReadBuffer: TBytes;
  StreamPos, StreamSize: NativeInt;
  EOLFound: boolean;
  i: integer;
begin
  Assert(Stream <> nil);

  if MaxLineLength < 0 then
    MaxLineLength := MaxInt;

  StreamPos := Stream.Position;
  StreamSize := Stream.Size;

  if StreamPos >= StreamSize then begin
    Line := '';
    Result := False;
    Exit;
  end;

  SetLength(ReadBuffer, 0);
  EOLFound := False;
  ReadOffset := 0;
  LineLen := 0;

  while StreamPos < StreamSize do begin
    if (ReadOffset + BLOCK_SIZE) < Length(ReadBuffer) then
      SetLength(ReadBuffer, ReadOffset + BLOCK_SIZE);

    ReadCount := Stream.Read(ReadBuffer[ReadOffset], Min(BLOCK_SIZE, MaxLineLength));
    if ReadCount <= 0 then
      Break;

    LineLen := ReadOffset + ReadCount; // Use all if EOL not found
    i := 0;
    while i < ReadCount do begin
      case ReadBuffer[i + ReadOffset] of
        10: begin
          EOLFound := True;
          LineLen := i + ReadOffset;
          ReadCount := i + 1;
          Break;
        end;
        13: begin
          EOLFound := True;
          LineLen := i + ReadOffset;
          Inc(i);
          if (i < ReadCount) and (ReadBuffer[i + ReadOffset] = 10) then
            ReadCount := i + 1
          else
            ReadCount := i;
          Break;
        end;
      end;
      Inc(i);
    end;


    Inc(StreamPos, ReadCount);
    Inc(ReadOffset, ReadCount);
    Dec(MaxLineLength, ReadCount);

    if EOLFound then
      Break;
  end;

  Stream.Position := StreamPos;

  if AEncoding  <> nil then
    Line := AEncoding.GetString(ReadBuffer, 0, LineLen)
  else
    Line := Encoding.Default.GetString(ReadBuffer, 0, LineLen);

  Result := True;
end;

class procedure TStreamUtils.WriteLine(Stream: TStream; const Str: string);
var
  ch: Byte;
begin
  if Str <> '' then
    Stream.WriteBuffer(Encoding.Default.GetBytes(Str)[0], Length(Str));

  ch := $0A;
  Stream.WriteBuffer(ch, 1);
end;

{ TScPersistent }

procedure TScPersistent.RaiseAssignError(Source: TScPersistent);
var
  SourceName: string;
begin
  if Source <> nil then
    SourceName := Source.ClassName
  else
    SourceName := 'nil';
  raise EScError.CreateFmt(SAssignError, [SourceName, ClassName], seAssignError);
end;

{ TScPersistentObjectList }

constructor TScPersistentObjectList.Create;
begin
  inherited;
  FList := TCRObjectList.Create;
end;

destructor TScPersistentObjectList.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TScPersistentObjectList.Assign(Source: TScPersistentObjectList);
var
  Item: TScPersistent;
  i: integer;
begin
  if Source = nil then
    raise EScError.Create(seInvalidInputArgs);

  if Self = Source then
    Exit;

  FList.Clear;

  for i := 0 to Source.FList.Count - 1 do begin
    if Source.FList[i] <> nil then begin
      Item := TScPersistent(Source.FList[i]).Clone;
      FList.Add(Item);
    end;
  end;
end;

function TScPersistentObjectList.GetItemClassType: TScPersistentClass;
begin
  Result := TScPersistent;
end;

procedure TScPersistentObjectList.CheckType(Item: TScPersistent);
begin
  if not (Item is GetItemClassType) then
    raise EScError.CreateFmt(SInvalidObjectClass, [GetItemClassType.ClassName, Item.ClassName], seInvalidObjectClass);
end;

function TScPersistentObjectList.Add(Item: TScPersistent): integer;
begin
  CheckType(Item);
  Result := FList.Add(Item);
end;

procedure TScPersistentObjectList.Clear;
begin
  FList.Clear;
end;

procedure TScPersistentObjectList.Delete(Index: integer);
begin
  FList.Delete(Index);
end;

function TScPersistentObjectList.IndexOf(Item: TScPersistent): integer;
begin
  Result := FList.IndexOf(Item);
end;

procedure TScPersistentObjectList.Insert(Index: integer; Item: TScPersistent);
begin
  CheckType(Item);
  FList.Insert(Index, Item);
end;

function TScPersistentObjectList.Remove(Item: TScPersistent): integer;
begin
  Result := Flist.Remove(Item);
end;

function TScPersistentObjectList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TScPersistentObjectList.Get(Index: integer): TScPersistent;
begin
  Result := TScPersistent(FList.Items[Index]);
end;

procedure TScPersistentObjectList.Put(Index: integer; Item: TScPersistent);
begin
  CheckType(Item);
  FList.Items[Index] := Item;
end;

{ TScZCompression }

constructor TScZCompression.Create;
begin
  inherited;

{$IFDEF HAVE_COMPRESS_INTERNAL}
  SetLength(FInBuffer, DEFAULT_ZBUFFER_SIZE);
  SetLength(FOutBuffer, DEFAULT_ZBUFFER_SIZE + 1024);
{$ENDIF}
end;

destructor TScZCompression.Destroy;
begin
{$IFDEF HAVE_COMPRESS_INTERNAL}
  if FCInited then
    deflateEnd(FZCRec);
  if FDInited then
    inflateEnd(FZDRec);
{$ENDIF}

  inherited;
end;

{$IFDEF HAVE_COMPRESS_INTERNAL}
procedure TScZCompression.InitC;
begin
  FillChar(FZCRec, sizeof(FZCRec), 0);
  FZCRec.zalloc := zlibAllocMem;
  FZCRec.zfree := zlibFreeMem;
  CCheck(deflateInit_(FZCRec, Z_DEFAULT_COMPRESSION{Z_BEST_COMPRESSION}, ZLIB_VERSION, sizeof(FZCRec)));
  FCInited := True;
end;

procedure TScZCompression.InitD;
begin
  FillChar(FZDRec, sizeof(FZDRec), 0);
  FZDRec.zalloc := zlibAllocMem;
  FZDRec.zfree := zlibFreeMem;
  DCheck(inflateInit_(FZDRec, ZLIB_VERSION, sizeof(FZDRec)));
  FDInited := True;
end;

function TScZCompression.CCheck(code: Integer): Integer;
begin
  Result := code;
  if code < 0 then
    raise {$IFDEF VER12P}EZCompressionError{$ELSE}ECompressionError{$ENDIF}.Create(sError);
end;

function TScZCompression.DCheck(code: Integer): Integer;
begin
  Result := code;
  if code < 0 then
    raise {$IFDEF VER12P}EZDecompressionError{$ELSE}EDecompressionError{$ENDIF}.Create(sError);
end;
{$ENDIF}

procedure TScZCompression.Compress(const InBuffer: TValueArr; InOffset, InCount: cardinal;
  const OutBuffer: TValueArr; OutOffset: cardinal; var OutCount: cardinal; Flush: boolean = True);
{$IFDEF HAVE_COMPRESS_INTERNAL}
var
  ZRes: integer;
{$ENDIF}
begin
{$IFDEF HAVE_COMPRESS_INTERNAL}
  if not FCInited then
    InitC;

  FZCRec.next_in := PtrOffset(InBuffer, InOffset);
  FZCRec.avail_in := InCount;
  FZCRec.next_out := PtrOffset(OutBuffer, OutOffset);
  FZCRec.avail_out := OutCount;
  FZCRec.total_out := 0;
  try
    if Flush then
      ZRes := CCheck(deflate(FZCRec, Z_PARTIAL_FLUSH{Z_FINISH}))
    else
      ZRes := CCheck(deflate(FZCRec, Z_NO_FLUSH));
    if ((ZRes <> Z_STREAM_END) and (ZRes <> Z_OK)) or (FZCRec.avail_in > 0) then
      raise EZlibError.CreateRes(@sTargetBufferTooSmall);

    OutCount := FZCRec.total_out;
  except
    CCheck(deflateEnd(FZCRec));
    InitC;
    raise;
  end;
{$ELSE}
  OutCount := 0;
{$ENDIF}
end;

function TScZCompression.Decompress(const InBuffer: TValueArr; var InOffset, InCount: cardinal;
  const OutBuffer: TValueArr; OutOffset: cardinal; var OutCount: cardinal): boolean;
{$IFDEF HAVE_COMPRESS_INTERNAL}
var
  ZRes: integer;
{$ENDIF}
begin
{$IFDEF HAVE_COMPRESS_INTERNAL}
  if not FDInited then
    InitD;

  FZDRec.next_in := PtrOffset(InBuffer, InOffset);
  FZDRec.avail_in := InCount;
  FZDRec.next_out := PtrOffset(OutBuffer, OutOffset);
  FZDRec.avail_out := OutCount;
  FZDRec.total_out := 0;
  try
    ZRes := DCheck(inflate(FZDRec, Z_PARTIAL_FLUSH{Z_FINISH}));
    if ((ZRes <> Z_STREAM_END) and (ZRes <> Z_OK)) {or (FZDRec.avail_in > 0) } then
      raise EZlibError.CreateRes(@sTargetBufferTooSmall);

    InOffset := InOffset + InCount - cardinal(FZDRec.avail_in);
    InCount := FZDRec.avail_in;
    OutCount := FZDRec.total_out;
    Result := FZDRec.avail_in = 0;
  except
    DCheck(inflateEnd(FZDRec));
    InitD;
    raise;
  end;
{$ELSE}
  Result := True;
  OutCount := 0;
{$ENDIF}
end;

procedure TScZCompression.CompressStream(InStream: TStream; OnWriteData: TScWriteDataToStreamEvent);
{$IFDEF HAVE_COMPRESS_INTERNAL}
var
  ZCRec: TZStreamRec;
  InCount: integer;
  OutOffset, OutCount: integer;
  ZRes: integer;
{$ENDIF}
begin
{$IFDEF HAVE_COMPRESS_INTERNAL}
  FillChar(ZCRec, sizeof(ZCRec), 0);
  ZCRec.zalloc := zlibAllocMem;
  ZCRec.zfree := zlibFreeMem;
  CCheck(deflateInit_(ZCRec, Z_DEFAULT_COMPRESSION, ZLIB_VERSION, sizeof(ZCRec)));

  OutOffset := 0;
  InCount := InStream.Read(FInBuffer, Length(FInBuffer));

  while InCount > 0 do begin
    ZCRec.next_in := @FInBuffer[0];
    ZCRec.avail_in := InCount;

    repeat
      ZCRec.next_out := @FOutBuffer[OutOffset];
      ZCRec.avail_out := Length(FOutBuffer) - OutOffset;

      CCheck(deflate(ZCRec, Z_NO_FLUSH));
      OutCount := Length(FOutBuffer) - integer(ZCRec.avail_out);
      if OutCount >= 1024 then begin // for performance optimization
        OnWriteData(InStream, FOutBuffer, OutCount);
        OutOffset := 0;
      end
      else
        OutOffset := OutCount;
    until (ZCRec.avail_in = 0) and (ZCRec.avail_out > 0);

    InCount := InStream.Read(FInBuffer, Length(FInBuffer));
  end;

  repeat
    ZCRec.next_out := @FOutBuffer[OutOffset];
    ZCRec.avail_out := Length(FOutBuffer) - OutOffset;

    ZRes := CCheck(deflate(ZCRec, Z_FINISH));
    OutCount := Length(FOutBuffer) - integer(ZCRec.avail_out);
    if OutCount > 0 then begin
      OnWriteData(InStream, FOutBuffer, OutCount);
      OutOffset := 0;
    end;
  until (ZRes = Z_STREAM_END) and (ZCRec.avail_out > 0);

  CCheck(deflateEnd(ZCRec));
{$ELSE}
  raise Exception.Create('Operation is not implemented');
{$ENDIF}
end;

procedure TScZCompression.DecompressToStream(OutStream: TStream; OnReadData: TScReadDataFromStreamEvent);
{$IFDEF HAVE_COMPRESS_INTERNAL}
var
  ZDRec: TZStreamRec;
  InCount, OutCount: integer;
  ZRes: integer;
{$ENDIF}
begin
{$IFDEF HAVE_COMPRESS_INTERNAL}
  FillChar(ZDRec, sizeof(ZDRec), 0);
  ZDRec.zalloc := zlibAllocMem;
  ZDRec.zfree := zlibFreeMem;
  DCheck(inflateInit_(ZDRec, ZLIB_VERSION, sizeof(ZDRec)));

  InCount := OnReadData(OutStream, FInBuffer, Length(FInBuffer));
  while InCount > 0 do begin
    ZDRec.next_in := @FInBuffer[0];
    ZDRec.avail_in := InCount;

    repeat
      ZDRec.next_out := @FOutBuffer[0];
      ZDRec.avail_out := Length(FOutBuffer);

      DCheck(inflate(ZDRec, Z_NO_FLUSH));
      OutCount := Length(FOutBuffer) - integer(ZDRec.avail_out);
      if OutCount <> 0 then
        OutStream.Write(FOutBuffer, OutCount);
    until (ZDRec.avail_in = 0) and (ZDRec.avail_out > 0);

    InCount := OnReadData(OutStream, FInBuffer, Length(FInBuffer));
  end;

  repeat
    ZDRec.next_out := @FOutBuffer[0];
    ZDRec.avail_out := Length(FOutBuffer);

    ZRes := inflate(ZDRec, Z_FINISH);
    if ZRes <> Z_BUF_ERROR then
      ZRes := DCheck(ZRes);

    OutCount := Length(FOutBuffer) - integer(ZDRec.avail_out);
    if OutCount <> 0 then
      OutStream.Write(FOutBuffer, OutCount);
  until ((ZRes = Z_STREAM_END) and (ZDRec.avail_out > 0)) or (ZRes = Z_BUF_ERROR);

  DCheck(inflateEnd(ZDRec));
{$ELSE}
  raise Exception.Create('Operation is not implemented');
{$ENDIF}
end;

{ TSimpleSemaphore }

constructor TSimpleSemaphore.Create;
const
  MaxCurrentCount = 1;
begin
  inherited;

  FCountLock := TCriticalSection.Create;
  FWaiter := CreateEvent;
  FCurrentCount := MaxCurrentCount;
  FInThreadCount := 0;
  FThreadId := {$IFDEF DARWIN}nil{$ELSE}0{$ENDIF};
end;

destructor TSimpleSemaphore.Destroy;
begin
  FCountLock.Free;
  FWaiter.Free;

  inherited;
end;

function TSimpleSemaphore.WaitFor(Timeout: cardinal = INFINITE): boolean;
var
  Ticks, BeginTickCount: {$IFDEF FPC}UInt64{$ELSE}cardinal{$ENDIF};
  CurrentThreadId: TThreadID;
begin
  FCountLock.Enter;
  try
    CurrentThreadId := GetCurrentThreadId;
    if FThreadId = CurrentThreadId then begin
      Inc(FInThreadCount);
      Result := True;
      Exit;
    end;

    Result := FCurrentCount > 0;
    if Result then begin
      FThreadId := CurrentThreadId;
      Inc(FInThreadCount);
      Dec(FCurrentCount);
      Exit;
    end;

    FWaiter.ResetEvent;
  finally
    FCountLock.Leave;
  end;

  while Timeout > 0 do begin
    BeginTickCount := {$IFDEF FPC}GetTickCount64{$ELSE}GetTickCount{$ENDIF};
    FWaiter.WaitFor(Timeout);

    FCountLock.Enter;
    try
      Result := FCurrentCount > 0;
      if Result then begin
        FThreadId := CurrentThreadId;
        Inc(FInThreadCount);
        Dec(FCurrentCount);
        Exit;
      end;

      FWaiter.ResetEvent;
    finally
      FCountLock.Leave;
    end;

    Ticks := GetTickInterval(BeginTickCount, {$IFDEF FPC}GetTickCount64{$ELSE}GetTickCount{$ENDIF});
    if Timeout > Ticks then
      Timeout := Timeout - Ticks
    else
      Timeout := 0;
  end;
end;

procedure TSimpleSemaphore.Release;
begin
  FCountLock.Enter;
  try
    Dec(FInThreadCount);
    if FInThreadCount = 0 then begin
      Inc(FCurrentCount);
      FThreadId := {$IFDEF DARWIN}nil{$ELSE}0{$ENDIF};
      FWaiter.SetEvent;
    end;
  finally
    FCountLock.Leave;
  end;
end;

{ TScTCPConnection }

constructor TScTCPConnection.Create;
begin
  inherited Create;

  FVio := TCRVioTcp.Create;
  FVio.OnClose := DoOnClose;
  FOwner := True;
end;

constructor TScTCPConnection.Create(VioTcp: TCRVioTcp; Owner: boolean);
begin
  inherited Create;

  Assert(VioTcp <> nil);
  FVio := VioTcp;
  FVio.OnClose := DoOnClose;
  FOwner := Owner;
end;

destructor TScTCPConnection.Destroy;
begin
  if FOwner then
    FVio.Free;

  inherited;
end;

procedure TScTCPConnection.DoOnClose(Sender: TObject);
begin
  try
    if Assigned(OnInternalClose) then
      OnInternalClose(Self);
  finally
    if Assigned(OnClose) then
      OnClose(Self);
  end;
end;

procedure TScTCPConnection.Bind;
begin
  FVio.Bind;
end;

procedure TScTCPConnection.Connect;
begin
  FVio.Connect;
end;

procedure TScTCPConnection.Close;
begin
  FVio.Close;
end;

function TScTCPConnection.Read(var Buffer; Count: integer): integer;
begin
  Result := FVio.Read(@Buffer, 0, Count);
end;

function TScTCPConnection.Read(var Buffer: TBytes; Offset, Count: integer): integer;
begin
  Result := FVio.Read(TValueArr(Buffer), Offset, Count);
end;

function TScTCPConnection.Write(const Buffer; Count: integer): integer;
begin
  Result := FVio.Write(@Buffer, 0, Count);
end;

function TScTCPConnection.Write(const Buffer: TBytes; Offset, Count: integer): integer;
begin
  Result := FVio.Write(TValueArr(Buffer), Offset, Count);
end;

function TScTCPConnection.WaitForData(MillisecondsTimeout: integer = -1): boolean;
begin
  Result := FVio.WaitForData(MillisecondsTimeout);
end;

function TScTCPConnection.GetLocalIP: string;
begin
  Result := FVio.GetLocalIP;
end;

function TScTCPConnection.GetLocalPort: integer;
begin
  Result := FVio.GetLocalPort;
end;

function TScTCPConnection.GetRemoteIP: string;
begin
  Result := FVio.GetRemoteIP;
end;

function TScTCPConnection.GetRemotePort: integer;
begin
  Result := FVio.GetRemotePort;
end;

function TScTCPConnection.GetBindAddress: string;
begin
  Result := FVio.BindAddress;
end;

procedure TScTCPConnection.SetBindAddress(const Value: string);
begin
  FVio.BindAddress := Value;
end;

function TScTCPConnection.GetHost: string;
begin
  Result := FVio.Host;
end;

procedure TScTCPConnection.SetHost(const Value: string);
begin
  FVio.Host := Value;
end;

function TScTCPConnection.GetPort: integer;
begin
  Result := FVio.Port;
end;

procedure TScTCPConnection.SetPort(const Value: integer);
begin
  FVio.Port := Value;
end;

function TScTCPConnection.GetIPVersion: TIPVersion;
begin
  Result := FVio.IPVersion;
end;

procedure TScTCPConnection.SetIPVersion(const Value: TIPVersion);
begin
  FVio.IPVersion := Value;
end;

function TScTCPConnection.GetConnected: boolean;
begin
  Result := FVio.Connected;
end;

function TScTCPConnection.GetConnectionTimeout: integer;
begin
  Result := FVio.ConnectionTimeout;
end;

procedure TScTCPConnection.SetConnectionTimeout(const Value: integer);
begin
  FVio.ConnectionTimeout := Value;
end;

function TScTCPConnection.GetSendTimeout: integer;
begin
  Result := FVio.SendTimeout;
end;

procedure TScTCPConnection.SetSendTimeout(const Value: integer);
begin
  FVio.SendTimeout := Value;
end;

function TScTCPConnection.GetReceiveTimeout: integer;
begin
  Result := FVio.ReceiveTimeout;
end;

procedure TScTCPConnection.SetReceiveTimeout(const Value: integer);
begin
  FVio.ReceiveTimeout := Value;
end;

function TScTCPConnection.GetLocalSockAddr: PSockAddr;
begin
  Result := FVio.LocalSockAddr;
end;

function TScTCPConnection.GetRemoteSockAddr: PSockAddr;
begin
  Result := FVio.RemoteSockAddr;
end;

{ Log }

class procedure Log.LogDebug(Logger: TScLogger; const Message: string);
begin
  if Assigned(Logger) then
    Logger.LogDebug(Message);
end;

class procedure Log.LogDebug(Logger: TScLogger; const Message: string; const Args: array of const);
begin
  if Assigned(Logger) then
    Logger.LogDebug(Format(Message, Args));
end;

class procedure Log.LogInformation(Logger: TScLogger; const Message: string);
begin
  if Assigned(Logger) then
    Logger.LogInformation(Message);
end;

class procedure Log.LogInformation(Logger: TScLogger; const Message: string; const Args: array of const);
begin
  if Assigned(Logger) then
    Logger.LogInformation(Format(Message, Args));
end;

class procedure Log.LogWarning(Logger: TScLogger; const Message: string);
begin
  if Assigned(Logger) then
    Logger.LogWarning(Message);
end;

class procedure Log.LogWarning(Logger: TScLogger; const Message: string; const Args: array of const);
begin
  if Assigned(Logger) then
    Logger.LogWarning(Format(Message, Args));
end;

class procedure Log.LogError(Logger: TScLogger; const Message: string; E: Exception);
begin
  if Assigned(Logger) then
    if E <> nil then
      Logger.LogError(Message + #13#10 + E.Message, E)
    else
      Logger.LogError(Message, nil);
end;

class procedure Log.LogError(Logger: TScLogger; const Message: string; const Args: array of const; E: Exception);
begin
  if Assigned(Logger) then
    if E <> nil then
      Logger.LogError(Format(Message, Args) + #13#10 + E.Message, E)
    else
      Logger.LogError(Format(Message, Args), nil);
end;

{ TScLogger }

constructor TScLogger.Create;
begin
  inherited;

  FLogLock := TCriticalSection.Create;
end;

destructor TScLogger.Destroy;
begin
  FLogLock.Free;

  inherited;
end;

procedure TScLogger.LogDebug(const Message: string);
begin
  FLogLock.Enter;
  try
    if Assigned(FOnLogDebug) then
      FOnLogDebug(Self, Message);
  finally
    FLogLock.Leave;
  end;
end;

procedure TScLogger.LogInformation(const Message: string);
begin
  FLogLock.Enter;
  try
    if Assigned(FOnLogInformation) then
      FOnLogInformation(Self, Message);
  finally
    FLogLock.Leave;
  end;
end;

procedure TScLogger.LogWarning(const Message: string);
begin
  FLogLock.Enter;
  try
    if Assigned(FOnLogWarning) then
      FOnLogWarning(Self, Message);
  finally
    FLogLock.Leave;
  end;
end;

procedure TScLogger.LogError(const Message: string; E: Exception);
begin
  FLogLock.Enter;
  try
    if Assigned(FOnLogError) then
      FOnLogError(Self, Message, E);
  finally
    FLogLock.Leave;
  end;
end;

function GetLogFileName: string;
begin
{$IFDEF ANDROID}
  Result := TPath.GetPublicPath + PathDelim;
{$ELSE}
  Result := '.\';
{$ENDIF}
  Result := Result + 'log.log';
end;

procedure LogMessage(const Message: string);
var
  fs: TFileStream;
  Buf: TBytes;
begin
  if LockLog = nil then
    LockLog := TCriticalSection.Create;

  try
    LockLog.Enter;
    try
      try
        fs := TFileStream.Create(GetLogFileName, fmOpenReadWrite + fmShareDenyNone);
      except
        fs := TFileStream.Create(GetLogFileName, fmCreate);
      end;

      try
        SetLength(Buf, 0);
        fs.Seek(0, {$IFDEF VER16P}TSeekOrigin.{$ENDIF}soEnd);
        Buf := Encoding.UTF8.GetBytes(DateTimeToStr(Now) + #9 + Message + #13#10);
        fs.Write(Buf[0], Length(Buf));
      finally
        fs.Free;
      end;
    finally
      LockLog.Leave;
    end;
  except
  end;
end;


type
{$IFDEF VER12P}
  {$WARN WIDECHAR_REDUCED OFF}
{$ENDIF}
  // WideChar Reduced to ByteChar in set expressions.
  TMaskSet = set of Char;
  PMaskSet = ^TMaskSet;
  TMaskStates = (msLiteral, msAny, msSet, msMBCSLiteral);
  TMaskState = record
    SkipTo: Boolean;
    case State: TMaskStates of
      msLiteral: (Literal: Char);
      msAny: ();
      msSet: (
        Negate: Boolean;
        CharSet: PMaskSet);
      msMBCSLiteral: (LeadByte, TrailByte: Char);
  end;

  TScMask = class
  private
    FMaskStates: array of TMaskState;

  protected
    function InitMaskStates(const Mask: string): Integer;
    procedure DoneMaskStates;
    function MatchesMaskStates(const Filename: string): Boolean;

  public
    constructor Create(const MaskValue: string);
    destructor Destroy; override;
    function Matches(const Filename: string): Boolean;
  end;

const
  MaxCards = 30;

function TScMask.InitMaskStates(const Mask: string): Integer;
var
  I: Integer;
  SkipTo: Boolean;
  Literal: Char;
  LeadByte, TrailByte: Char;
  P: PChar;
  Negate: Boolean;
  CharSet: TMaskSet;
  Cards: Integer;

  procedure InvalidMask;
  begin
    raise Exception.CreateResFmt(@SInvalidMask, [Mask, P - PChar(Mask) + 1]);
  end;

  procedure Reset;
  begin
    SkipTo := False;
    Negate := False;
    CharSet := [];
  end;

  procedure WriteScan(MaskState: TMaskStates);
  begin
    if I <= High(FMaskStates) then
    begin
      if SkipTo then
      begin
        Inc(Cards);
        if Cards > MaxCards then InvalidMask;
      end;
      FMaskStates[I].SkipTo := SkipTo;
      FMaskStates[I].State := MaskState;
      case MaskState of
        msLiteral: FMaskStates[I].Literal := UpCase(Literal);
        msSet:
          begin
            FMaskStates[I].Negate := Negate;
            New(FMaskStates[I].CharSet);
            FMaskStates[I].CharSet^ := CharSet;
          end;
        msMBCSLiteral:
          begin
            FMaskStates[I].LeadByte := LeadByte;
            FMaskStates[I].TrailByte := TrailByte;
          end;
      end;
    end;
    Inc(I);
    Reset;
  end;

  procedure ScanSet;
  var
    LastChar: Char;
    C: Char;
  begin
    Inc(P);
    if P^ = '!' then
    begin
      Negate := True;
      Inc(P);
    end;
    LastChar := #0;
    while not (P^ in [#0, ']']) do
    begin
      // MBCS characters not supported in msSet!
      if {$IFDEF VER14P}IsLeadChar(P^){$ELSE}CharInSet(P^, LeadBytes){$ENDIF} then
         Inc(P)
      else
      case P^ of
        '-':
          if LastChar = #0 then InvalidMask
          else
          begin
            Inc(P);
            for C := LastChar to UpCase(P^) do
              CharSet := CharSet + [C];
          end;
      else
        LastChar := UpCase(P^);
        CharSet := CharSet + [LastChar];
      end;
      Inc(P);
    end;
    if (P^ <> ']') or (CharSet = []) then InvalidMask;
    WriteScan(msSet);
  end;

begin
  P := PChar(Mask);
  I := 0;
  Cards := 0;
  Reset;
  while P^ <> #0 do
  begin
    case P^ of
      '*': SkipTo := True;
      '?': if not SkipTo then WriteScan(msAny);
      '[':  ScanSet;
    else
      if {$IFDEF VER14P}IsLeadChar(P^){$ELSE}CharInSet(P^, LeadBytes){$ENDIF} then
      begin
        LeadByte := P^;
        Inc(P);
        TrailByte := P^;
        WriteScan(msMBCSLiteral);
      end
      else
      begin
        Literal := P^;
        WriteScan(msLiteral);
      end;
    end;
    Inc(P);
  end;
  Literal := #0;
  WriteScan(msLiteral);
  Result := I;
end;

function TScMask.MatchesMaskStates(const Filename: string): Boolean;
type
  TStackRec = record
    sP: PChar;
    sI: Integer;
  end;
var
  T: Integer;
  S: array of TStackRec;
  I: Integer;
  P: PChar;

  procedure Push(P: PChar; I: Integer);
  begin
    S[T].sP := P;
    S[T].sI := I;
    Inc(T);
  end;

  function Pop(var P: PChar; var I: Integer): Boolean;
  begin
    if T = 0 then
      Result := False
    else
    begin
      Dec(T);
      P := S[T].sP;
      I := S[T].sI;
      Result := True;
    end;
  end;

  function Matches(P: PChar; Start: Integer): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := Start to High(FMaskStates) do
    begin
      if FMaskStates[I].SkipTo then
      begin
        case FMaskStates[I].State of
          msLiteral:
            while (P^ <> #0) and (UpCase(P^) <> FMaskStates[I].Literal) do Inc(P);
          msSet:
            while (P^ <> #0) and not (FMaskStates[I].Negate xor (UpCase(P^) in FMaskStates[I].CharSet^)) do Inc(P);
          msMBCSLiteral:
            while (P^ <> #0) do
            begin
              if (P^ <> FMaskStates[I].LeadByte) then Inc(P, 2)
              else
              begin
                Inc(P);
                if (P^ = FMaskStates[I].TrailByte) then Break;
                Inc(P);
              end;
            end;
        end;
        if P^ <> #0 then
          Push(@P[1], I);
      end;
      case FMaskStates[I].State of
        msLiteral: if UpCase(P^) <> FMaskStates[I].Literal then Exit;
        msSet: if not (FMaskStates[I].Negate xor (UpCase(P^) in FMaskStates[I].CharSet^)) then Exit;
        msMBCSLiteral:
          begin
            if P^ <> FMaskStates[I].LeadByte then Exit;
            Inc(P);
            if P^ <> FMaskStates[I].TrailByte then Exit;
          end;
        msAny:
          if P^ = #0 then
          begin
            Result := False;
            Exit;
          end;
      end;
      Inc(P);
    end;
    Result := True;
  end;

begin
  SetLength(S, MaxCards);
  Result := True;
  T := 0;
  P := PChar(Filename);
  I := Low(FMaskStates);
  repeat
    if Matches(P, I) then Exit;
  until not Pop(P, I);
  Result := False;
end;

procedure TScMask.DoneMaskStates;
var
  I: Integer;
begin
  for I := Low(FMaskStates) to High(FMaskStates) do
    if FMaskStates[I].State = msSet then Dispose(FMaskStates[I].CharSet);
end;

constructor TScMask.Create(const MaskValue: string);
var
  Size: Integer;
begin
  inherited Create;
  SetLength(FMaskStates, 1);
  Size := InitMaskStates(MaskValue);
  DoneMaskStates;

  SetLength(FMaskStates, Size);
  InitMaskStates(MaskValue);
end;

destructor TScMask.Destroy;
begin
  DoneMaskStates;
  SetLength(FMaskStates, 0);
  inherited;
end;

function TScMask.Matches(const Filename: string): Boolean;
begin
  Result := MatchesMaskStates(Filename);
end;

function ScMatchesMask(const Filename, Mask: string): Boolean;
var
  CMask: TScMask;
begin
  CMask := TScMask.Create(Mask);
  try
    Result := CMask.Matches(Filename);
  finally
    CMask.Free;
  end;
end;

procedure Init_DH_PRIMEs;
var
  sb: StringBuilder;
begin
  sb := StringBuilder.Create;
  try
    sb.Append('FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD1');
    sb.Append('29024E088A67CC74020BBEA63B139B22514A08798E3404DD');
    sb.Append('EF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245');
    sb.Append('E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7ED');
    sb.Append('EE386BFB5A899FA5AE9F24117C4B1FE649286651ECE65381');
    sb.Append('FFFFFFFFFFFFFFFF');

    DH_PRIME_GROUP_1024 := TBigInteger.Create(string(sb.ToString), 16);
  finally
    sb.Free;
  end;

  sb := StringBuilder.Create;
  try
    sb.Append('FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD1');
    sb.Append('29024E088A67CC74020BBEA63B139B22514A08798E3404DD');
    sb.Append('EF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245');
    sb.Append('E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7ED');
    sb.Append('EE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3D');
    sb.Append('C2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F');
    sb.Append('83655D23DCA3AD961C62F356208552BB9ED529077096966D');
    sb.Append('670C354E4ABC9804F1746C08CA237327FFFFFFFFFFFFFFFF');

    DH_PRIME_GROUP_1536 := TBigInteger.Create(string(sb.ToString), 16);
  finally
    sb.Free;
  end;

  sb := StringBuilder.Create;
  try
    sb.Append('FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD1');
    sb.Append('29024E088A67CC74020BBEA63B139B22514A08798E3404DD');
    sb.Append('EF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245');
    sb.Append('E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7ED');
    sb.Append('EE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3D');
    sb.Append('C2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F');
    sb.Append('83655D23DCA3AD961C62F356208552BB9ED529077096966D');
    sb.Append('670C354E4ABC9804F1746C08CA18217C32905E462E36CE3B');
    sb.Append('E39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9');
    sb.Append('DE2BCBF6955817183995497CEA956AE515D2261898FA0510');
    sb.Append('15728E5A8AACAA68FFFFFFFFFFFFFFFF');

    DH_PRIME_GROUP_2048 := TBigInteger.Create(string(sb.ToString), 16);
  finally
    sb.Free;
  end;

  sb := StringBuilder.Create;
  try
    sb.Append('FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD1');
    sb.Append('29024E088A67CC74020BBEA63B139B22514A08798E3404DD');
    sb.Append('EF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245');
    sb.Append('E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7ED');
    sb.Append('EE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3D');
    sb.Append('C2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F');
    sb.Append('83655D23DCA3AD961C62F356208552BB9ED529077096966D');
    sb.Append('670C354E4ABC9804F1746C08CA18217C32905E462E36CE3B');
    sb.Append('E39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9');
    sb.Append('DE2BCBF6955817183995497CEA956AE515D2261898FA0510');
    sb.Append('15728E5A8AAAC42DAD33170D04507A33A85521ABDF1CBA64');
    sb.Append('ECFB850458DBEF0A8AEA71575D060C7DB3970F85A6E1E4C7');
    sb.Append('ABF5AE8CDB0933D71E8C94E04A25619DCEE3D2261AD2EE6B');
    sb.Append('F12FFA06D98A0864D87602733EC86A64521F2B18177B200C');
    sb.Append('BBE117577A615D6C770988C0BAD946E208E24FA074E5AB31');
    sb.Append('43DB5BFCE0FD108E4B82D120A93AD2CAFFFFFFFFFFFFFFFF');

    DH_PRIME_GROUP_3072 := TBigInteger.Create(string(sb.ToString), 16);
  finally
    sb.Free;
  end;

  sb := StringBuilder.Create;
  try
    sb.Append('FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD1');
    sb.Append('29024E088A67CC74020BBEA63B139B22514A08798E3404DD');
    sb.Append('EF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245');
    sb.Append('E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7ED');
    sb.Append('EE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3D');
    sb.Append('C2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F');
    sb.Append('83655D23DCA3AD961C62F356208552BB9ED529077096966D');
    sb.Append('670C354E4ABC9804F1746C08CA18217C32905E462E36CE3B');
    sb.Append('E39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9');
    sb.Append('DE2BCBF6955817183995497CEA956AE515D2261898FA0510');
    sb.Append('15728E5A8AAAC42DAD33170D04507A33A85521ABDF1CBA64');
    sb.Append('ECFB850458DBEF0A8AEA71575D060C7DB3970F85A6E1E4C7');
    sb.Append('ABF5AE8CDB0933D71E8C94E04A25619DCEE3D2261AD2EE6B');
    sb.Append('F12FFA06D98A0864D87602733EC86A64521F2B18177B200C');
    sb.Append('BBE117577A615D6C770988C0BAD946E208E24FA074E5AB31');
    sb.Append('43DB5BFCE0FD108E4B82D120A92108011A723C12A787E6D7');
    sb.Append('88719A10BDBA5B2699C327186AF4E23C1A946834B6150BDA');
    sb.Append('2583E9CA2AD44CE8DBBBC2DB04DE8EF92E8EFC141FBECAA6');
    sb.Append('287C59474E6BC05D99B2964FA090C3A2233BA186515BE7ED');
    sb.Append('1F612970CEE2D7AFB81BDD762170481CD0069127D5B05AA9');
    sb.Append('93B4EA988D8FDDC186FFB7DC90A6C08F4DF435C934063199');
    sb.Append('FFFFFFFFFFFFFFFF');

    DH_PRIME_GROUP_4096 := TBigInteger.Create(string(sb.ToString), 16);
  finally
    sb.Free;
  end;

  sb := StringBuilder.Create;
  try
    sb.Append('FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD129024E08');
    sb.Append('8A67CC74020BBEA63B139B22514A08798E3404DDEF9519B3CD3A431B');
    sb.Append('302B0A6DF25F14374FE1356D6D51C245E485B576625E7EC6F44C42E9');
    sb.Append('A637ED6B0BFF5CB6F406B7EDEE386BFB5A899FA5AE9F24117C4B1FE6');
    sb.Append('49286651ECE45B3DC2007CB8A163BF0598DA48361C55D39A69163FA8');
    sb.Append('FD24CF5F83655D23DCA3AD961C62F356208552BB9ED529077096966D');
    sb.Append('670C354E4ABC9804F1746C08CA18217C32905E462E36CE3BE39E772C');
    sb.Append('180E86039B2783A2EC07A28FB5C55DF06F4C52C9DE2BCBF695581718');
    sb.Append('3995497CEA956AE515D2261898FA051015728E5A8AAAC42DAD33170D');
    sb.Append('04507A33A85521ABDF1CBA64ECFB850458DBEF0A8AEA71575D060C7D');
    sb.Append('B3970F85A6E1E4C7ABF5AE8CDB0933D71E8C94E04A25619DCEE3D226');
    sb.Append('1AD2EE6BF12FFA06D98A0864D87602733EC86A64521F2B18177B200C');
    sb.Append('BBE117577A615D6C770988C0BAD946E208E24FA074E5AB3143DB5BFC');
    sb.Append('E0FD108E4B82D120A92108011A723C12A787E6D788719A10BDBA5B26');
    sb.Append('99C327186AF4E23C1A946834B6150BDA2583E9CA2AD44CE8DBBBC2DB');
    sb.Append('04DE8EF92E8EFC141FBECAA6287C59474E6BC05D99B2964FA090C3A2');
    sb.Append('233BA186515BE7ED1F612970CEE2D7AFB81BDD762170481CD0069127');
    sb.Append('D5B05AA993B4EA988D8FDDC186FFB7DC90A6C08F4DF435C934028492');
    sb.Append('36C3FAB4D27C7026C1D4DCB2602646DEC9751E763DBA37BDF8FF9406');
    sb.Append('AD9E530EE5DB382F413001AEB06A53ED9027D831179727B0865A8918');
    sb.Append('DA3EDBEBCF9B14ED44CE6CBACED4BB1BDB7F1447E6CC254B33205151');
    sb.Append('2BD7AF426FB8F401378CD2BF5983CA01C64B92ECF032EA15D1721D03');
    sb.Append('F482D7CE6E74FEF6D55E702F46980C82B5A84031900B1C9E59E7C97F');
    sb.Append('BEC7E8F323A97A7E36CC88BE0F1D45B7FF585AC54BD407B22B4154AA');
    sb.Append('CC8F6D7EBF48E1D814CC5ED20F8037E0A79715EEF29BE32806A1D58B');
    sb.Append('B7C5DA76F550AA3D8A1FBFF0EB19CCB1A313D55CDA56C9EC2EF29632');
    sb.Append('387FE8D76E3C0468043E8F663F4860EE12BF2D5B0B7474D6E694F91E');
    sb.Append('6DCC4024FFFFFFFFFFFFFFFF');

    DH_PRIME_GROUP_6144 := TBigInteger.Create(string(sb.ToString), 16);
  finally
    sb.Free;
  end;

  sb := StringBuilder.Create;
  try
    sb.Append('FFFFFFFFFFFFFFFFC90FDAA22168C234C4C6628B80DC1CD1');
    sb.Append('29024E088A67CC74020BBEA63B139B22514A08798E3404DD');
    sb.Append('EF9519B3CD3A431B302B0A6DF25F14374FE1356D6D51C245');
    sb.Append('E485B576625E7EC6F44C42E9A637ED6B0BFF5CB6F406B7ED');
    sb.Append('EE386BFB5A899FA5AE9F24117C4B1FE649286651ECE45B3D');
    sb.Append('C2007CB8A163BF0598DA48361C55D39A69163FA8FD24CF5F');
    sb.Append('83655D23DCA3AD961C62F356208552BB9ED529077096966D');
    sb.Append('670C354E4ABC9804F1746C08CA18217C32905E462E36CE3B');
    sb.Append('E39E772C180E86039B2783A2EC07A28FB5C55DF06F4C52C9');
    sb.Append('DE2BCBF6955817183995497CEA956AE515D2261898FA0510');
    sb.Append('15728E5A8AAAC42DAD33170D04507A33A85521ABDF1CBA64');
    sb.Append('ECFB850458DBEF0A8AEA71575D060C7DB3970F85A6E1E4C7');
    sb.Append('ABF5AE8CDB0933D71E8C94E04A25619DCEE3D2261AD2EE6B');
    sb.Append('F12FFA06D98A0864D87602733EC86A64521F2B18177B200C');
    sb.Append('BBE117577A615D6C770988C0BAD946E208E24FA074E5AB31');
    sb.Append('43DB5BFCE0FD108E4B82D120A92108011A723C12A787E6D7');
    sb.Append('88719A10BDBA5B2699C327186AF4E23C1A946834B6150BDA');
    sb.Append('2583E9CA2AD44CE8DBBBC2DB04DE8EF92E8EFC141FBECAA6');
    sb.Append('287C59474E6BC05D99B2964FA090C3A2233BA186515BE7ED');
    sb.Append('1F612970CEE2D7AFB81BDD762170481CD0069127D5B05AA9');
    sb.Append('93B4EA988D8FDDC186FFB7DC90A6C08F4DF435C934028492');
    sb.Append('36C3FAB4D27C7026C1D4DCB2602646DEC9751E763DBA37BD');
    sb.Append('F8FF9406AD9E530EE5DB382F413001AEB06A53ED9027D831');
    sb.Append('179727B0865A8918DA3EDBEBCF9B14ED44CE6CBACED4BB1B');
    sb.Append('DB7F1447E6CC254B332051512BD7AF426FB8F401378CD2BF');
    sb.Append('5983CA01C64B92ECF032EA15D1721D03F482D7CE6E74FEF6');
    sb.Append('D55E702F46980C82B5A84031900B1C9E59E7C97FBEC7E8F3');
    sb.Append('23A97A7E36CC88BE0F1D45B7FF585AC54BD407B22B4154AA');
    sb.Append('CC8F6D7EBF48E1D814CC5ED20F8037E0A79715EEF29BE328');
    sb.Append('06A1D58BB7C5DA76F550AA3D8A1FBFF0EB19CCB1A313D55C');
    sb.Append('DA56C9EC2EF29632387FE8D76E3C0468043E8F663F4860EE');
    sb.Append('12BF2D5B0B7474D6E694F91E6DBE115974A3926F12FEE5E4');
    sb.Append('38777CB6A932DF8CD8BEC4D073B931BA3BC832B68D9DD300');
    sb.Append('741FA7BF8AFC47ED2576F6936BA424663AAB639C5AE4F568');
    sb.Append('3423B4742BF1C978238F16CBE39D652DE3FDB8BEFC848AD9');
    sb.Append('22222E04A4037C0713EB57A81A23F0C73473FC646CEA306B');
    sb.Append('4BCBC8862F8385DDFA9D4B7FA2C087E879683303ED5BDD3A');
    sb.Append('062B3CF5B3A278A66D2A13F83F44F82DDF310EE074AB6A36');
    sb.Append('4597E899A0255DC164F31CC50846851DF9AB48195DED7EA1');
    sb.Append('B1D510BD7EE74D73FAF36BC31ECFA268359046F4EB879F92');
    sb.Append('4009438B481C6CD7889A002ED5EE382BC9190DA6FC026E47');
    sb.Append('9558E4475677E9AA9E3050E2765694DFC81F56E880B96E71');
    sb.Append('60C980DD98EDD3DFFFFFFFFFFFFFFFFF');

    DH_PRIME_GROUP_8192 := TBigInteger.Create(string(sb.ToString), 16);
  finally
    sb.Free;
  end;
end;

initialization
  DefaultLogger := TScLogger.Create;
  Init_DH_PRIMEs;

finalization
  DefaultLogger.Free;
  LockLog.Free;
  DH_PRIME_GROUP_1024.Free;
  DH_PRIME_GROUP_1536.Free;
  DH_PRIME_GROUP_2048.Free;
  DH_PRIME_GROUP_3072.Free;
  DH_PRIME_GROUP_4096.Free;
  DH_PRIME_GROUP_6144.Free;
  DH_PRIME_GROUP_8192.Free;

end.
