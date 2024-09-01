{ ***************************************************************************
  sgcP2P component

  written by eSeGeCe
  copyright © 2022
  Email : info@esegece.com
  Web : http://www.esegece.com
  *************************************************************************** }

unit sgcP2P_STUN_Classes;

interface

{$I sgcVer.inc}
{$IFDEF SGC_STUN}

uses
  Classes, SysUtils,
{$IFDEF NEXTGEN}System.Generics.Collections, {$ELSE}Contnrs, {$ENDIF}
  // sgc
  sgcBase_Classes, sgcBase_Helpers, sgcUDP_Classes, sgcP2P_STUN_Types,
  sgcWebSocket_Types, sgcSocket_Classes;

type
  TsgcSTUN_Message = class;

  TsgcSTUNExceptionInvalidRequest = class(Exception);
  TsgcSTUNExceptionSilent = class(Exception);

  TsgcSTUNExceptionEvent = procedure(Sender: TObject; E: Exception) of object;
  TsgcSTUNRequestMessageIntegrityKeyEvent = procedure(Sender: TObject;
    const aTransactionId: String; var MessageIntegrityKey: TBytes) of object;
  TsgcSTUNRequestMessageIntegrityPasswordEvent = procedure(Sender: TObject;
    const aUsername, aRealm: String; var Password: string) of object;

  TsgcICEValidateMessageIntegrityEvent = procedure(Sender: TObject;
    const aUsername: String; var Password: string) of object;
  TsgcICERequestMessageIntegrityPasswordEvent = procedure(Sender: TObject;
    const aMessage: TsgcSTUN_Message; var Password: string) of object;
  TsgcICERequestBindingEvent = procedure(Sender: TObject;
    const aSocket: TsgcSocketConnection; const aMessage: TsgcSTUN_Message)
    of object;

  TsgcSTUN_Options = class(TPersistent)
  private
    FFingerprint: Boolean;
    FSoftware: Boolean;
  public
    constructor Create; virtual;
  public
    procedure Assign(aSource: TPersistent); override;
  published
    property Fingerprint: Boolean read FFingerprint write FFingerprint;
    property Software: Boolean read FSoftware write FSoftware;
  end;

  TsgcSTUNLogFile = class(TsgcSocketLogFile)

  end;

  TsgcSTUN_Component = class(TsgcComponent_Base)

  end;

  TsgcSTUN_ResponseBinding = class
  private
    FIPVersion: TwsIPVersion;
    FLocalIP: string;
    FLocalPort: Word;
    FRemoteIP: string;
    FRemotePort: Word;
  public
    property IPVersion: TwsIPVersion read FIPVersion write FIPVersion;
    property LocalIP: string read FLocalIP write FLocalIP;
    property LocalPort: Word read FLocalPort write FLocalPort;
    property RemoteIP: string read FRemoteIP write FRemoteIP;
    property RemotePort: Word read FRemotePort write FRemotePort;
  end;

  TsgcSTUN_ResponseError = class
  private
    FCode: Word;
    FReason: string;
  public
    property Code: Word read FCode write FCode;
    property Reason: string read FReason write FReason;
  end;

  TsgcSTUN_Header = class
  private
    FMagicCookie: Cardinal;
    FMessageType_Class: TsgcStunMessageClass;
    FMessageLength: Word;
    FMessageType: Word;
    FMessageType_Method: TsgcStunMessageMethod;
    FTransactionId: TBytes;
    function GetTransactionIdString: string;
  public
    constructor Create; virtual;
  public
    procedure Read(const aBytes: TBytes);
    function Write: TBytes;
  public
    property MessageType_Class: TsgcStunMessageClass read FMessageType_Class
      write FMessageType_Class;
    property MessageType_Method: TsgcStunMessageMethod read FMessageType_Method
      write FMessageType_Method;
  public
    property MagicCookie: Cardinal read FMagicCookie write FMagicCookie;
    property MessageLength: Word read FMessageLength write FMessageLength;
    property MessageType: Word read FMessageType write FMessageType;
    property TransactionId: TBytes read FTransactionId write FTransactionId;
  public
    property TransactionIdString: String read GetTransactionIdString;
  end;

  TsgcSTUN_Attribute_Class = class of TsgcSTUN_Attribute;

  TsgcSTUN_Attribute = class
  private
    FAddress: string;
    FFamily: Word;
    FPort: Word;
  private
    FAttributeType: TsgcStunMessageAttribute;
    FAttributeLength: Word;
  private
    FOffset: Integer;
    procedure IncOffset(Value: Word = 1);
  protected
    function GetPayload: TBytes; virtual; abstract;
    procedure DoReadPayload(Value: TBytes); virtual; abstract;
    function GetPayloadLength: Word; virtual;
  public
    constructor Create; virtual;
  public
    procedure Read(Value: TBytes; var aOffset: Integer); virtual;
    function Write: TBytes; virtual;
  public
    property AttributeType: TsgcStunMessageAttribute read FAttributeType
      write FAttributeType;
    property AttributeLength: Word read FAttributeLength write FAttributeLength;
  public
    property Address: string read FAddress write FAddress;
    property Family: Word read FFamily write FFamily;
    property Port: Word read FPort write FPort;
  end;

  TsgcSTUN_Attributes = Array of TsgcSTUN_Attribute;

  TsgcSTUN_Attribute_None = class(TsgcSTUN_Attribute)
  protected
    procedure DoReadPayload(Value: TBytes); override;
  end;

  TsgcSTUN_Attribute_MAPPED_ADDRESS = class(TsgcSTUN_Attribute)
  protected
    procedure DoReadPayload(Value: TBytes); override;
    function GetPayload: TBytes; override;
  public
    constructor Create; override;
  end;

  TsgcSTUN_Attribute_XOR_MAPPED_ADDRESS = class(TsgcSTUN_Attribute)
  private
    FTransactionId: TBytes;
    procedure SetTransactionId(const Value: TBytes);
  protected
    function GetPayload: TBytes; override;
    procedure DoReadPayload(Value: TBytes); override;
  public
    constructor Create; override;
  public
    property TransactionId: TBytes read FTransactionId write SetTransactionId;
  end;

  TsgcSTUN_Attribute_USERNAME = class(TsgcSTUN_Attribute)
  private
    FUsername: string;
  public
    constructor Create; override;
  protected
    procedure DoReadPayload(Value: TBytes); override;
    function GetPayload: TBytes; override;
  public
    property Username: string read FUsername write FUsername;
  end;

  TsgcSTUN_Attribute_MESSAGE_INTEGRITY = class(TsgcSTUN_Attribute)
  private
    FKey: TBytes;
    FPayLoad: TBytes;
    procedure SetKey(const Value: TBytes);
    procedure SetPayLoad(const Value: TBytes);
  public
    constructor Create; override;
  protected
    procedure DoReadPayload(Value: TBytes); override;
    function GetPayload: TBytes; override;
  public
    property Key: TBytes read FKey write SetKey;
    property PayLoad: TBytes read FPayLoad write SetPayLoad;
  end;

  TsgcSTUN_Attribute_MESSAGE_INTEGRITY_SHA256 = class(TsgcSTUN_Attribute)
  private
    FKey: TBytes;
    FPayLoad: TBytes;
    procedure SetKey(const Value: TBytes);
    procedure SetPayLoad(const Value: TBytes);
  public
    constructor Create; override;
  protected
    procedure DoReadPayload(Value: TBytes); override;
    function GetPayload: TBytes; override;
  public
    property Key: TBytes read FKey write SetKey;
    property PayLoad: TBytes read FPayLoad write SetPayLoad;
  end;

  TsgcSTUN_Attribute_FINGERPRINT = class(TsgcSTUN_Attribute)
  private
    FFingerprint: Cardinal;
  public
    constructor Create; override;
  protected
    procedure DoReadPayload(Value: TBytes); override;
    function GetPayload: TBytes; override;
  public
    property Fingerprint: Cardinal read FFingerprint write FFingerprint;
  end;

  TsgcSTUN_Attribute_ERROR_CODE = class(TsgcSTUN_Attribute)
  private
    FCode: Word;
    FReason: string;
  protected
    procedure DoReadPayload(Value: TBytes); override;
    function GetPayload: TBytes; override;
  public
    constructor Create; override;
  public
    property Reason: string read FReason write FReason;
    property Code: Word read FCode write FCode;
  end;

  TsgcSTUN_Attribute_REALM = class(TsgcSTUN_Attribute)
  private
    FRealm: String;
  public
    constructor Create; override;
  protected
    procedure DoReadPayload(Value: TBytes); override;
    function GetPayload: TBytes; override;
  public
    property Realm: String read FRealm write FRealm;
  end;

  TsgcSTUN_Attribute_NONCE = class(TsgcSTUN_Attribute)
  private
    FNonce: String;
  public
    constructor Create; override;
  protected
    procedure DoReadPayload(Value: TBytes); override;
    function GetPayload: TBytes; override;
  public
    property Nonce: String read FNonce write FNonce;
  end;

  TsgcSTUN_Attribute_UNKNOWN_ATTRIBUTES = class(TsgcSTUN_Attribute)
  private
    FUnknownAttributes: TsgcStunMessageAttributes;
  public
    constructor Create; override;
  protected
    procedure DoReadPayload(Value: TBytes); override;
    function GetPayload: TBytes; override;
  public
    property UnknownAttributes: TsgcStunMessageAttributes
      read FUnknownAttributes write FUnknownAttributes;
  end;

  TsgcSTUN_Attribute_SOFTWARE = class(TsgcSTUN_Attribute)
  private
    FSoftware: string;
  public
    constructor Create; override;
  protected
    procedure DoReadPayload(Value: TBytes); override;
    function GetPayload: TBytes; override;
  public
    property Software: string read FSoftware write FSoftware;
  end;

  TsgcSTUN_Attribute_ALTERNATE_SERVER = class(TsgcSTUN_Attribute_MAPPED_ADDRESS)
  public
    constructor Create; override;
  end;

  TsgcSTUN_Attribute_CHANGE_REQUEST = class(TsgcSTUN_Attribute)
  public
    constructor Create; override;
  protected
    procedure DoReadPayload(Value: TBytes); override;
    function GetPayload: TBytes; override;
  end;

  TsgcSTUN_Attribute_RESPONSE_PORT = class(TsgcSTUN_Attribute)
  public
    constructor Create; override;
  protected
    procedure DoReadPayload(Value: TBytes); override;
    function GetPayload: TBytes; override;
  end;

  TsgcSTUN_Attribute_PADDING = class(TsgcSTUN_Attribute)
  public
    constructor Create; override;
  protected
    procedure DoReadPayload(Value: TBytes); override;
    function GetPayload: TBytes; override;
  end;

  TsgcSTUN_Attribute_CACHE_TIMEOUT = class(TsgcSTUN_Attribute)
  public
    constructor Create; override;
  protected
    procedure DoReadPayload(Value: TBytes); override;
    function GetPayload: TBytes; override;
  end;

  TsgcSTUN_Attribute_RESPONSE_ORIGIN = class(TsgcSTUN_Attribute_MAPPED_ADDRESS)
  public
    constructor Create; override;
  end;

  TsgcSTUN_Attribute_OTHER_ADDRESS = class(TsgcSTUN_Attribute_MAPPED_ADDRESS)
  public
    constructor Create; override;
  end;

  TsgcSTUN_Attribute_SOURCE_ADDRESS = class(TsgcSTUN_Attribute_MAPPED_ADDRESS)
  public
    constructor Create; override;
  end;

  TsgcSTUN_Attribute_CHANGED_ADDRESS = class(TsgcSTUN_Attribute_MAPPED_ADDRESS)
  public
    constructor Create; override;
  end;

  TsgcSTUN_Attribute_PASSWORD_ALGORITHM = class(TsgcSTUN_Attribute)
  public
    constructor Create; override;
  end;

  TsgcSTUN_Attribute_USERHASH = class(TsgcSTUN_Attribute)
  public
    constructor Create; override;
  end;

  TsgcSTUN_Attribute_PASSWORD_ALGORITHMS = class
    (TsgcSTUN_Attribute_MAPPED_ADDRESS)
  public
    constructor Create; override;
  end;

  TsgcSTUN_Attribute_ALTERNATE_DOMAIN = class(TsgcSTUN_Attribute)
  private
    FDomain: string;
  protected
    procedure DoReadPayload(Value: TBytes); override;
    function GetPayload: TBytes; override;
  public
    constructor Create; override;
  public
    property Domain: string read FDomain write FDomain;
  end;

  TsgcSTUN_Attribute_CHANNEL_NUMBER = class(TsgcSTUN_Attribute)
  private
    FChannel: Word;
  protected
    procedure DoReadPayload(Value: TBytes); override;
    function GetPayload: TBytes; override;
  public
    constructor Create; override;
  public
    property Channel: Word read FChannel write FChannel;
  end;

  TsgcSTUN_Attribute_LIFETIME = class(TsgcSTUN_Attribute)
  private
    FLifetime: Cardinal;
  protected
    procedure DoReadPayload(Value: TBytes); override;
    function GetPayload: TBytes; override;
  public
    constructor Create; override;
  public
    property Lifetime: Cardinal read FLifetime write FLifetime;
  end;

  TsgcSTUN_Attribute_XOR_PEER_ADDRESS = class
    (TsgcSTUN_Attribute_XOR_MAPPED_ADDRESS)
  public
    constructor Create; override;
  end;

  TsgcSTUN_Attribute_DATA = class(TsgcSTUN_Attribute)
  private
    FData: TBytes;
    procedure SetData(const Value: TBytes);
  protected
    procedure DoReadPayload(Value: TBytes); override;
    function GetPayload: TBytes; override;
  public
    constructor Create; override;
  public
    property Data: TBytes read FData write SetData;
  end;

  TsgcSTUN_Attribute_XOR_RELAYED_ADDRESS = class
    (TsgcSTUN_Attribute_XOR_MAPPED_ADDRESS)
  public
    constructor Create; override;
  end;

  TsgcSTUN_Attribute_REQUESTED_ADDRESS_FAMILY = class(TsgcSTUN_Attribute)
  private
    FFamily: Byte;
  protected
    procedure DoReadPayload(Value: TBytes); override;
    function GetPayload: TBytes; override;
  public
    constructor Create; override;
  public
    property Family: Byte read FFamily write FFamily;
  end;

  TsgcSTUN_Attribute_EVEN_PORT = class(TsgcSTUN_Attribute)
  private
    FEven: Boolean;
  protected
    procedure DoReadPayload(Value: TBytes); override;
    function GetPayload: TBytes; override;
  public
    constructor Create; override;
  public
    property Even: Boolean read FEven write FEven;
  end;

  TsgcSTUN_Attribute_REQUESTED_TRANSPORT = class(TsgcSTUN_Attribute)
  private
    FProtocol: Byte;
  protected
    procedure DoReadPayload(Value: TBytes); override;
    function GetPayload: TBytes; override;
  public
    constructor Create; override;
  public
    property Protocol: Byte read FProtocol write FProtocol;
  end;

  TsgcSTUN_Attribute_DONT_FRAGMENT = class(TsgcSTUN_Attribute)
  protected
    procedure DoReadPayload(Value: TBytes); override;
  public
    constructor Create; override;
  end;

  TsgcSTUN_Attribute_RESERVATION_TOKEN = class(TsgcSTUN_Attribute)
  private
    FToken: TBytes;
  protected
    procedure DoReadPayload(Value: TBytes); override;
    function GetPayload: TBytes; override;
  public
    constructor Create; override;
  public
    property Token: TBytes read FToken write FToken;
  end;

  TsgcSTUN_Attribute_ADDITIONAL_ADDRESS_FAMILY = class
    (TsgcSTUN_Attribute_REQUESTED_ADDRESS_FAMILY)
  public
    constructor Create; override;
  end;

  TsgcSTUN_Attribute_ADDRESS_ERROR_CODE = class(TsgcSTUN_Attribute)
  private
    FCode: Word;
    FFamily: Byte;
    FReason: String;
  protected
    procedure DoReadPayload(Value: TBytes); override;
    function GetPayload: TBytes; override;
  public
    constructor Create; override;
  public
    property Code: Word read FCode write FCode;
    property Family: Byte read FFamily write FFamily;
    property Reason: String read FReason write FReason;
  end;

  TsgcSTUN_Attribute_ICMP = class(TsgcSTUN_Attribute)
  private
    FErrorData: Cardinal;
    FICMPCode: Word;
    FICMPType: Byte;
  protected
    procedure DoReadPayload(Value: TBytes); override;
    function GetPayload: TBytes; override;
  public
    constructor Create; override;
  public
    property ErrorData: Cardinal read FErrorData write FErrorData;
    property ICMPCode: Word read FICMPCode write FICMPCode;
    property ICMPType: Byte read FICMPType write FICMPType;
  end;

  TsgcSTUN_Attribute_PRIORITY = class(TsgcSTUN_Attribute)
  private
    FPriority: Cardinal;
  protected
    procedure DoReadPayload(Value: TBytes); override;
    function GetPayload: TBytes; override;
  public
    constructor Create; override;
  public
    property Priority: Cardinal read FPriority write FPriority;
  end;

  TsgcSTUN_Attribute_USE_CANDIDATE = class(TsgcSTUN_Attribute)
  protected
    procedure DoReadPayload(Value: TBytes); override;
    function GetPayload: TBytes; override;
  public
    constructor Create; override;
  end;

  TsgcSTUN_Attribute_ICE_CONTROL_BASE = class(TsgcSTUN_Attribute)
  private
    FTieBreaker: TBytes;
    procedure SetTieBreaker(const Value: TBytes);
  protected
    procedure DoReadPayload(Value: TBytes); override;
    function GetPayload: TBytes; override;
  public
    constructor Create; override;
  public
    property TieBreaker: TBytes read FTieBreaker write SetTieBreaker;
  end;

  TsgcSTUN_Attribute_ICE_CONTROLLED = class(TsgcSTUN_Attribute_ICE_CONTROL_BASE)
  public
    constructor Create; override;
  end;

  TsgcSTUN_Attribute_ICE_CONTROLLING = class
    (TsgcSTUN_Attribute_ICE_CONTROL_BASE)
  public
    constructor Create; override;
  end;

  TsgcSTUN_AttributeList = class
    (TList{$IFDEF NEXTGEN}<TsgcSTUN_Attribute>{$ENDIF})
  public
    destructor Destroy; override;
  public
    function GetAttribute(const aType: TsgcStunMessageAttribute)
      : TsgcSTUN_Attribute;
  public
    function Write: TBytes;
  end;

  TsgcSTUN_Message = class
  private
    FAttributes: TsgcSTUN_AttributeList;
    FFingerprint: TsgcStunFingerprintState;
    FCredentials: TsgcStunCredentials;
    FHeader: TsgcSTUN_Header;
    FMessageIntegrity: TsgcStunMessageIntegrityState;
    FMessageIntegritySHA256: TsgcStunMessageIntegrityState;
    FMessageType: TsgcStunMessageType;
    function GetAttributes: TsgcSTUN_AttributeList;
    function GetHeader: TsgcSTUN_Header;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  private
    function IsFingerprintValid(const aAttribute: TsgcSTUN_Attribute;
      const aBytes: TBytes): Boolean;
    procedure DoValidateMessageIntegrity(const aAttribute: TsgcSTUN_Attribute;
      const aBytes: TBytes; aICE: Boolean);
    procedure DoComputeFingerprint;
    procedure DoComputeCredentials;
    procedure DoComputeHeaderMessageLength;
    procedure SetMessageType(const Value: TsgcStunMessageType);
  protected
    procedure DoReadHeader(Bytes: TBytes);
    procedure DoReadPayload(Bytes: TBytes);
  protected
    function DoWrite: TBytes; virtual;
  public
    procedure Read(Bytes: TBytes);
    function Write: TBytes;
  public
    property Attributes: TsgcSTUN_AttributeList read GetAttributes
      write FAttributes;
    property Header: TsgcSTUN_Header read GetHeader write FHeader;
  public
    property Fingerprint: TsgcStunFingerprintState read FFingerprint
      write FFingerprint;
    property MessageIntegrity: TsgcStunMessageIntegrityState
      read FMessageIntegrity write FMessageIntegrity;
    property MessageIntegritySHA256: TsgcStunMessageIntegrityState
      read FMessageIntegritySHA256 write FMessageIntegritySHA256;
    property Credentials: TsgcStunCredentials read FCredentials
      write FCredentials;
    property MessageType: TsgcStunMessageType read FMessageType
      write SetMessageType;

    { properties }
  private
    FNonce: string;
    FPassword: string;
    FRealm: string;
    FUsername: string;
  public
    property Nonce: string read FNonce write FNonce;
    property Password: string read FPassword write FPassword;
    property Realm: string read FRealm write FRealm;
    property Username: string read FUsername write FUsername;
    { properties }

    { events }
  private
    FOnRequestMessageIntegrityKey: TsgcSTUNRequestMessageIntegrityKeyEvent;
    FOnRequestMessageIntegrityPassword
      : TsgcSTUNRequestMessageIntegrityPasswordEvent;
    FOnICEValidateMessageIntegrity: TsgcICEValidateMessageIntegrityEvent;
    FOnICERequestMessageIntegrityPassword
      : TsgcICERequestMessageIntegrityPasswordEvent;
  public
    property OnRequestMessageIntegrityKey
      : TsgcSTUNRequestMessageIntegrityKeyEvent
      read FOnRequestMessageIntegrityKey write FOnRequestMessageIntegrityKey;
    property OnRequestMessageIntegrityPassword
      : TsgcSTUNRequestMessageIntegrityPasswordEvent
      read FOnRequestMessageIntegrityPassword
      write FOnRequestMessageIntegrityPassword;
    property OnICEValidateMessageIntegrity: TsgcICEValidateMessageIntegrityEvent
      read FOnICEValidateMessageIntegrity write FOnICEValidateMessageIntegrity;
    property OnICERequestMessageIntegrityPassword
      : TsgcICERequestMessageIntegrityPasswordEvent
      read FOnICERequestMessageIntegrityPassword
      write FOnICERequestMessageIntegrityPassword;
    { events }
  end;

function GetAttributeTypeFromValue(aValue: Word): TsgcStunMessageAttribute;
function GetAttributeTypeFromType(aValue: TsgcStunMessageAttribute): Word;
function GetAttributeClassFromType(aValue: TsgcStunMessageAttribute)
  : TsgcSTUN_Attribute_Class;

procedure GetErrorResponseCode(aValue: TsgcStunErrorResponseCodes;
  var Code: Word; var Reason: String);

const
  CS_STUN_DEFAULT_PORT = 3478;

const
  CS_STUN_MAGIC_COOKIE = $2112A442;
  CS_STUN_FINGERPRINT_XOR = $5354554E;

  // ... stun attributes
const
  CS_STUN_ATTRIBUTE_REQUIRED_MAPPED_ADDRESS = $0001;
  CS_STUN_ATTRIBUTE_REQUIRED_USERNAME = $0006;
  CS_STUN_ATTRIBUTE_REQUIRED_MESSAGE_INTEGRITY = $0008;
  CS_STUN_ATTRIBUTE_REQUIRED_ERROR_CODE = $0009;
  CS_STUN_ATTRIBUTE_REQUIRED_UNKNOWN_ATTRIBUTES = $000A;
  CS_STUN_ATTRIBUTE_REQUIRED_REALM = $0014;
  CS_STUN_ATTRIBUTE_REQUIRED_NONCE = $0015;
  CS_STUN_ATTRIBUTE_REQUIRED_XOR_MAPPED_ADDRESS = $0020;

const
  CS_STUN_ATTRIBUTE_OPTIONAL_SOFTWARE = $8022;
  CS_STUN_ATTRIBUTE_OPTIONAL_ALTERNATE_SERVER = $8023;
  CS_STUN_ATTRIBUTE_OPTIONAL_FINGERPRINT = $8028;

const
  CS_STUN_ATTRIBUTE_CHANGE_REQUEST = $0003;
  CS_STUN_ATTRIBUTE_RESPONSE_PORT = $0027;
  CS_STUN_ATTRIBUTE_PADDING = $0026;
  CS_STUN_ATTRIBUTE_CACHE_TIMEOUT = $8027;
  CS_STUN_ATTRIBUTE_RESPONSE_ORIGIN = $802B;
  CS_STUN_ATTRIBUTE_OTHER_ADDRESS = $802C;

const
  CS_STUN_ATTRIBUTE_SOURCE_ADDRESS = $0004;
  CS_STUN_ATTRIBUTE_CHANGED_ADDRESS = $0005;

  { rfc 8489 }
const
  CS_STUN_ATTRIBUTE_REQUIRED_MESSAGE_INTEGRITY_SHA256 = $001C;
  CS_STUN_ATTRIBUTE_REQUIRED_PASSWORD_ALGORITHM = $001D;
  CS_STUN_ATTRIBUTE_REQUIRED_USERHASH = $001E;

const
  CS_STUN_ATTRIBUTE_OPTIONAL_PASSWORD_ALGORITHMS = $8002;
  CS_STUN_ATTRIBUTE_OPTIONAL_ALTERNATE_DOMAIN = $8003;
  { rfc 8489 }

  // ... turn attributes
const
  CS_STUN_ATTRIBUTE_CHANNEL_NUMBER = $000C;
  CS_STUN_ATTRIBUTE_LIFETIME = $000D;
  CS_STUN_ATTRIBUTE_XOR_PEER_ADDRESS = $0012;
  CS_STUN_ATTRIBUTE_DATA = $0013;
  CS_STUN_ATTRIBUTE_XOR_RELAYED_ADDRESS = $0016;
  CS_STUN_ATTRIBUTE_REQUESTED_ADDRESS_FAMILY = $0017;
  CS_STUN_ATTRIBUTE_EVENT_PORT = $0018;
  CS_STUN_ATTRIBUTE_REQUESTED_TRANSPORT = $0019;
  CS_STUN_ATTRIBUTE_DONT_FRAGMENT = $001A;
  CS_STUN_ATTRIBUTE_RESERVATION_TOKEN = $0022;
  CS_STUN_ATTRIBUTE_ADDITIONAL_ADDRESS_FAMILY = $8000;
  CS_STUN_ATTRIBUTE_ADDRESS_ERROR_CODE = $8001;
  CS_STUN_ATTRIBUTE_ICMP = $8004;

  // ... ice attriubtes
const
  CS_STUN_ATTRIBUTE_PRIORITY = $0024;
  CS_STUN_ATTRIBUTE_USE_CANDIDATE = $0025;
  CS_STUN_ATTRIBUTE_ICE_CONTROLLED = $8029;
  CS_STUN_ATTRIBUTE_ICE_CONTROLLING = $802A;

  // ... other values
const
  CS_STUN_HEADER_LENGTH = 20;
  CS_STUN_ATTRIBUTE_HEADER_LENGTH = 4;
  CS_STUN_ATTRIBUTE_FINGERPRINT_LENGTH = 4;
  CS_STUN_ATTRIBUTE_MESSAGE_INTEGRITY_LENGTH = 20;
  CS_STUN_ATTRIBUTE_MESSAGE_INTEGRITY_SHA256_LENGTH = 32;

const
  CS_STUN_RELAY = 'Relay';

resourcestring
  S_STUN_INVALID_HEADER_FORMAT = 'Invalid Header Format';
  S_STUN_INVALID_MESSAGE_CLASS = 'Invalid Message Class Identifier';
  S_STUN_INVALID_FINGERPRINT = 'Invalid Fingerprint';
  S_STUN_INVALID_MESSAGE_INTEGRITY = 'Invalid Message Integrity';
  S_STUN_UKNOWN_ATTRIBUTES = 'Unknown Attributes';
  S_STUN_ATTRIBUTE_NOT_FOUND = 'Attribute %s not found.';
  S_STUN_INVALID_FORMAT_ATTRIBUTE = 'Invalid Attribute: %s Format';

resourcestring
  S_STUN_INVALID_REQUEST = 'Invalid Request';
  S_STUN_UNAUTHORIZED = 'Unauthorized';
  S_STUN_STALE_NONCE = 'Stale Nonce';
  S_STUN_UNKNOWN_ATTRIBUTE = 'Unknown Attribute';
  S_STUN_TRY_ALTERNATE = 'Try Alternate';
  S_STUN_FORBIDDEN = 'Forbidden';
  S_STUN_ALLOCATION_MISMATCH = 'Allocation Mismatch';
  S_STUN_ADDRESS_FAMILY_NOT_SUPPORTED = 'Address Family not Supported';
  S_STUN_WRONG_CREDENTIALS = 'Wrong Credentials';
  S_STUN_UNSUPPORTED_TRANSPORT_PROTOCOL = 'Unsupported Transport Protocol';
  S_STUN_PEER_ADDRESS_FAMILY_MISMATCH = 'Peer Address Family Mismatch';
  S_STUN_ALLOCATION_QUOTA_REACHED = 'Allocation Quota Reached';
  S_STUN_INSUFFICIENT_CAPACITY = 'Insufficient Capacity';
  S_STUN_ICE_ROLE_CONFLICT = 'Role Conflict';

{$ENDIF}

implementation

{$IFDEF SGC_STUN}

uses
  sgcP2P_STUN_Helpers;

function GetAttributeTypeFromValue(aValue: Word): TsgcStunMessageAttribute;
begin
  case aValue of
    // STUN
    CS_STUN_ATTRIBUTE_REQUIRED_MAPPED_ADDRESS:
      result := stmaMapped_Address;
    CS_STUN_ATTRIBUTE_REQUIRED_XOR_MAPPED_ADDRESS:
      result := stmaXOR_Mapped_Address;
    CS_STUN_ATTRIBUTE_REQUIRED_USERNAME:
      result := stmaUsername;
    CS_STUN_ATTRIBUTE_REQUIRED_MESSAGE_INTEGRITY:
      result := stmaMesssage_Integrity;
    CS_STUN_ATTRIBUTE_OPTIONAL_FINGERPRINT:
      result := stmaFingerprint;
    CS_STUN_ATTRIBUTE_REQUIRED_ERROR_CODE:
      result := stmaError_Code;
    CS_STUN_ATTRIBUTE_REQUIRED_REALM:
      result := stmaRealm;
    CS_STUN_ATTRIBUTE_REQUIRED_NONCE:
      result := stmaNonce;
    CS_STUN_ATTRIBUTE_REQUIRED_UNKNOWN_ATTRIBUTES:
      result := stmaUnknown_Attributes;
    CS_STUN_ATTRIBUTE_OPTIONAL_SOFTWARE:
      result := stmaSoftware;
    CS_STUN_ATTRIBUTE_OPTIONAL_ALTERNATE_SERVER:
      result := stmaAlternate_Server;
    CS_STUN_ATTRIBUTE_CHANGE_REQUEST:
      result := stmaChange_Request;
    CS_STUN_ATTRIBUTE_RESPONSE_PORT:
      result := stmaResponse_Port;
    CS_STUN_ATTRIBUTE_PADDING:
      result := stmaPadding;
    CS_STUN_ATTRIBUTE_CACHE_TIMEOUT:
      result := stmaCache_Timeout;
    CS_STUN_ATTRIBUTE_RESPONSE_ORIGIN:
      result := stmaResponse_Origin;
    CS_STUN_ATTRIBUTE_OTHER_ADDRESS:
      result := stmaOther_Address;
    CS_STUN_ATTRIBUTE_SOURCE_ADDRESS:
      result := stmaSource_Address;
    CS_STUN_ATTRIBUTE_CHANGED_ADDRESS:
      result := stmaChanged_Address;
    CS_STUN_ATTRIBUTE_REQUIRED_MESSAGE_INTEGRITY_SHA256:
      result := stmaMesssage_Integrity_SHA256;
    CS_STUN_ATTRIBUTE_REQUIRED_PASSWORD_ALGORITHM:
      result := stmaPassword_Algorithm;
    CS_STUN_ATTRIBUTE_REQUIRED_USERHASH:
      result := stmaUserhash;
    CS_STUN_ATTRIBUTE_OPTIONAL_PASSWORD_ALGORITHMS:
      result := stmaPassword_Algorithms;
    CS_STUN_ATTRIBUTE_OPTIONAL_ALTERNATE_DOMAIN:
      result := stmaAlternate_Domain;
    // TURN
    CS_STUN_ATTRIBUTE_CHANNEL_NUMBER:
      result := stmaChannel_Number;
    CS_STUN_ATTRIBUTE_LIFETIME:
      result := stmaLifetime;
    CS_STUN_ATTRIBUTE_XOR_PEER_ADDRESS:
      result := stmaXOR_Peer_Address;
    CS_STUN_ATTRIBUTE_DATA:
      result := stmaData;
    CS_STUN_ATTRIBUTE_XOR_RELAYED_ADDRESS:
      result := stmaXOR_Relayed_Address;
    CS_STUN_ATTRIBUTE_REQUESTED_ADDRESS_FAMILY:
      result := stmaRequested_Address_Family;
    CS_STUN_ATTRIBUTE_EVENT_PORT:
      result := stmaEven_port;
    CS_STUN_ATTRIBUTE_REQUESTED_TRANSPORT:
      result := stmaRequested_Transport;
    CS_STUN_ATTRIBUTE_DONT_FRAGMENT:
      result := stmaDont_Fragment;
    CS_STUN_ATTRIBUTE_RESERVATION_TOKEN:
      result := stmaReservation_Token;
    CS_STUN_ATTRIBUTE_ADDITIONAL_ADDRESS_FAMILY:
      result := stmaAdditional_Address_Family;
    CS_STUN_ATTRIBUTE_ADDRESS_ERROR_CODE:
      result := stmaAddress_Error_Code;
    CS_STUN_ATTRIBUTE_ICMP:
      result := stmaICMP;
    // ICE
    CS_STUN_ATTRIBUTE_PRIORITY:
      result := stmaPriority;
    CS_STUN_ATTRIBUTE_USE_CANDIDATE:
      result := stmaUse_Candidate;
    CS_STUN_ATTRIBUTE_ICE_CONTROLLED:
      result := stmaICE_Controlled;
    CS_STUN_ATTRIBUTE_ICE_CONTROLLING:
      result := stmaICE_Controlling;
  else
    result := stmaMapped_None;
  end;
end;

function GetAttributeTypeFromType(aValue: TsgcStunMessageAttribute): Word;
begin
  result := 0;
  case aValue of
    // STUN
    stmaMapped_Address:
      result := CS_STUN_ATTRIBUTE_REQUIRED_MAPPED_ADDRESS;
    stmaXOR_Mapped_Address:
      result := CS_STUN_ATTRIBUTE_REQUIRED_XOR_MAPPED_ADDRESS;
    stmaUsername:
      result := CS_STUN_ATTRIBUTE_REQUIRED_USERNAME;
    stmaMesssage_Integrity:
      result := CS_STUN_ATTRIBUTE_REQUIRED_MESSAGE_INTEGRITY;
    stmaFingerprint:
      result := CS_STUN_ATTRIBUTE_OPTIONAL_FINGERPRINT;
    stmaError_Code:
      result := CS_STUN_ATTRIBUTE_REQUIRED_ERROR_CODE;
    stmaRealm:
      result := CS_STUN_ATTRIBUTE_REQUIRED_REALM;
    stmaNonce:
      result := CS_STUN_ATTRIBUTE_REQUIRED_NONCE;
    stmaUnknown_Attributes:
      result := CS_STUN_ATTRIBUTE_REQUIRED_UNKNOWN_ATTRIBUTES;
    stmaSoftware:
      result := CS_STUN_ATTRIBUTE_OPTIONAL_SOFTWARE;
    stmaAlternate_Server:
      result := CS_STUN_ATTRIBUTE_OPTIONAL_ALTERNATE_SERVER;
    stmaChange_Request:
      result := CS_STUN_ATTRIBUTE_CHANGE_REQUEST;
    stmaResponse_Port:
      result := CS_STUN_ATTRIBUTE_RESPONSE_PORT;
    stmaPadding:
      result := CS_STUN_ATTRIBUTE_PADDING;
    stmaCache_Timeout:
      result := CS_STUN_ATTRIBUTE_CACHE_TIMEOUT;
    stmaResponse_Origin:
      result := CS_STUN_ATTRIBUTE_RESPONSE_ORIGIN;
    stmaOther_Address:
      result := CS_STUN_ATTRIBUTE_OTHER_ADDRESS;
    stmaSource_Address:
      result := CS_STUN_ATTRIBUTE_SOURCE_ADDRESS;
    stmaChanged_Address:
      result := CS_STUN_ATTRIBUTE_CHANGED_ADDRESS;
    stmaMesssage_Integrity_SHA256:
      result := CS_STUN_ATTRIBUTE_REQUIRED_MESSAGE_INTEGRITY_SHA256;
    stmaPassword_Algorithm:
      result := CS_STUN_ATTRIBUTE_REQUIRED_PASSWORD_ALGORITHM;
    stmaUserhash:
      result := CS_STUN_ATTRIBUTE_REQUIRED_USERHASH;
    stmaPassword_Algorithms:
      result := CS_STUN_ATTRIBUTE_OPTIONAL_PASSWORD_ALGORITHMS;
    stmaAlternate_Domain:
      result := CS_STUN_ATTRIBUTE_OPTIONAL_ALTERNATE_DOMAIN;
    // TURN
    stmaChannel_Number:
      result := CS_STUN_ATTRIBUTE_CHANNEL_NUMBER;
    stmaLifetime:
      result := CS_STUN_ATTRIBUTE_LIFETIME;
    stmaXOR_Peer_Address:
      result := CS_STUN_ATTRIBUTE_XOR_PEER_ADDRESS;
    stmaData:
      result := CS_STUN_ATTRIBUTE_DATA;
    stmaXOR_Relayed_Address:
      result := CS_STUN_ATTRIBUTE_XOR_RELAYED_ADDRESS;
    stmaRequested_Address_Family:
      result := CS_STUN_ATTRIBUTE_REQUESTED_ADDRESS_FAMILY;
    stmaEven_port:
      result := CS_STUN_ATTRIBUTE_EVENT_PORT;
    stmaRequested_Transport:
      result := CS_STUN_ATTRIBUTE_REQUESTED_TRANSPORT;
    stmaDont_Fragment:
      result := CS_STUN_ATTRIBUTE_DONT_FRAGMENT;
    stmaReservation_Token:
      result := CS_STUN_ATTRIBUTE_RESERVATION_TOKEN;
    stmaAdditional_Address_Family:
      result := CS_STUN_ATTRIBUTE_ADDITIONAL_ADDRESS_FAMILY;
    stmaAddress_Error_Code:
      result := CS_STUN_ATTRIBUTE_ADDRESS_ERROR_CODE;
    stmaICMP:
      result := CS_STUN_ATTRIBUTE_ICMP;
    // ICE
    stmaPriority:
      result := CS_STUN_ATTRIBUTE_PRIORITY;
    stmaUse_Candidate:
      result := CS_STUN_ATTRIBUTE_USE_CANDIDATE;
    stmaICE_Controlled:
      result := CS_STUN_ATTRIBUTE_ICE_CONTROLLED;
    stmaICE_Controlling:
      result := CS_STUN_ATTRIBUTE_ICE_CONTROLLING;
  end;
end;

function GetAttributeClassFromType(aValue: TsgcStunMessageAttribute)
  : TsgcSTUN_Attribute_Class;
begin
  case aValue of
    // STUN
    stmaMapped_Address:
      result := TsgcSTUN_Attribute_MAPPED_ADDRESS;
    stmaXOR_Mapped_Address:
      result := TsgcSTUN_Attribute_XOR_MAPPED_ADDRESS;
    stmaUsername:
      result := TsgcSTUN_Attribute_USERNAME;
    stmaMesssage_Integrity:
      result := TsgcSTUN_Attribute_MESSAGE_INTEGRITY;
    stmaFingerprint:
      result := TsgcSTUN_Attribute_FINGERPRINT;
    stmaError_Code:
      result := TsgcSTUN_Attribute_ERROR_CODE;
    stmaRealm:
      result := TsgcSTUN_Attribute_REALM;
    stmaNonce:
      result := TsgcSTUN_Attribute_NONCE;
    stmaUnknown_Attributes:
      result := TsgcSTUN_Attribute_UNKNOWN_ATTRIBUTES;
    stmaSoftware:
      result := TsgcSTUN_Attribute_SOFTWARE;
    stmaAlternate_Server:
      result := TsgcSTUN_Attribute_ALTERNATE_SERVER;
    stmaChange_Request:
      result := TsgcSTUN_Attribute_CHANGE_REQUEST;
    stmaResponse_Port:
      result := TsgcSTUN_Attribute_RESPONSE_PORT;
    stmaPadding:
      result := TsgcSTUN_Attribute_PADDING;
    stmaCache_Timeout:
      result := TsgcSTUN_Attribute_CACHE_TIMEOUT;
    stmaResponse_Origin:
      result := TsgcSTUN_Attribute_RESPONSE_ORIGIN;
    stmaOther_Address:
      result := TsgcSTUN_Attribute_OTHER_ADDRESS;
    stmaSource_Address:
      result := TsgcSTUN_Attribute_SOURCE_ADDRESS;
    stmaChanged_Address:
      result := TsgcSTUN_Attribute_CHANGED_ADDRESS;
    stmaMesssage_Integrity_SHA256:
      result := TsgcSTUN_Attribute_MESSAGE_INTEGRITY_SHA256;
    stmaPassword_Algorithm:
      result := TsgcSTUN_Attribute_PASSWORD_ALGORITHM;
    stmaUserhash:
      result := TsgcSTUN_Attribute_USERHASH;
    stmaPassword_Algorithms:
      result := TsgcSTUN_Attribute_PASSWORD_ALGORITHMS;
    stmaAlternate_Domain:
      result := TsgcSTUN_Attribute_ALTERNATE_DOMAIN;
    // TURN
    stmaChannel_Number:
      result := TsgcSTUN_Attribute_CHANNEL_NUMBER;
    stmaLifetime:
      result := TsgcSTUN_Attribute_LIFETIME;
    stmaXOR_Peer_Address:
      result := TsgcSTUN_Attribute_XOR_PEER_ADDRESS;
    stmaData:
      result := TsgcSTUN_Attribute_DATA;
    stmaXOR_Relayed_Address:
      result := TsgcSTUN_Attribute_XOR_RELAYED_ADDRESS;
    stmaRequested_Address_Family:
      result := TsgcSTUN_Attribute_REQUESTED_ADDRESS_FAMILY;
    stmaEven_port:
      result := TsgcSTUN_Attribute_EVEN_PORT;
    stmaRequested_Transport:
      result := TsgcSTUN_Attribute_REQUESTED_TRANSPORT;
    stmaDont_Fragment:
      result := TsgcSTUN_Attribute_DONT_FRAGMENT;
    stmaReservation_Token:
      result := TsgcSTUN_Attribute_RESERVATION_TOKEN;
    stmaAdditional_Address_Family:
      result := TsgcSTUN_Attribute_ADDITIONAL_ADDRESS_FAMILY;
    stmaAddress_Error_Code:
      result := TsgcSTUN_Attribute_ADDRESS_ERROR_CODE;
    stmaICMP:
      result := TsgcSTUN_Attribute_ICMP;
    // ICE
    stmaPriority:
      result := TsgcSTUN_Attribute_PRIORITY;
    stmaUse_Candidate:
      result := TsgcSTUN_Attribute_USE_CANDIDATE;
    stmaICE_Controlled:
      result := TsgcSTUN_Attribute_ICE_CONTROLLED;
    stmaICE_Controlling:
      result := TsgcSTUN_Attribute_ICE_CONTROLLING;
  else
    result := TsgcSTUN_Attribute_None;
  end;
end;

constructor TsgcSTUN_Header.Create;
begin
  inherited;
  SetLength(FTransactionId, 12);
end;

function TsgcSTUN_Header.GetTransactionIdString: string;
var
  i: Integer;
  oBytes: TBytes;
begin
  SetLength(oBytes, Length(TransactionId));
  for i := 0 to Length(oBytes) - 1 do
  begin
    if TransactionId[i] = 0 then
    begin
      SetLength(oBytes, i);
      break;
    end
    else
      oBytes[i] := TransactionId[i];
  end;

  result := sgcBytesToStringRaw(oBytes);
end;

procedure TsgcSTUN_Header.Read(const aBytes: TBytes);
begin
  // MessageType
  MessageType := sgcGetWordFromBytes(aBytes, 0);
  MessageType_Method := sgcGetSTUNMessageMethod(MessageType);
  MessageType_Class := sgcGetSTUNMessageClass(MessageType);

  // MessageLength
  MessageLength := sgcGetWordFromBytes(aBytes, 2);

  // MagicCookie
  MagicCookie := sgcGetCardinalFromBytes(aBytes, 4);

  // TransactionId
  if Length(aBytes) >= 20 then
    Move(aBytes[8], TransactionId[0], 12)
  else
    raise Exception.Create(S_STUN_INVALID_HEADER_FORMAT);
end;

function TsgcSTUN_Header.Write: TBytes;
begin
  // MessageType
  sgcSTUNAddWord((Ord(MessageType_Method) and $F) or
    ((Ord(MessageType_Method) and $70) shl 1) or
    ((Ord(MessageType_Method) and $F80) shl 2) or
    ((Ord(MessageType_Class) and $1) shl 4) or
    ((Ord(MessageType_Class) and $2) shl 7), result);
  // MessageLength
  sgcSTUNAddWord(MessageLength, result);
  // MagicCookie
  sgcSTUNAddCardinal(MagicCookie, result);
  // Transaction Id
  sgcSTUNAddBytes(TransactionId, result);
end;

constructor TsgcSTUN_Attribute.Create;
begin

end;

function TsgcSTUN_Attribute.GetPayloadLength: Word;
begin
  result := Length(GetPayload);
  while (result mod 4) <> 0 do
    result := result + 1;
end;

procedure TsgcSTUN_Attribute.IncOffset(Value: Word = 1);
begin
  FOffset := FOffset + Value;
end;

procedure TsgcSTUN_Attribute.Read(Value: TBytes; var aOffset: Integer);
begin
  FOffset := aOffset;
  // attribute type
  AttributeType := GetAttributeTypeFromValue(sgcGetWordFromBytes(Value,
    FOffset));
  IncOffset(2);
  // attribute length
  AttributeLength := sgcGetWordFromBytes(Value, FOffset);
  IncOffset(2);
  // read payload
  DoReadPayload(Value);
  // adjust offset padding
  while (FOffset mod 4) <> 0 do
    IncOffset(1);
  aOffset := FOffset;
end;

function TsgcSTUN_Attribute.Write: TBytes;
begin
  // Initialize
  SetLength(result, 0);
  // common
  sgcSTUNAddWord(GetAttributeTypeFromType(AttributeType), result);
  sgcSTUNAddWord(Length(GetPayload), result);

  // payload
  sgcSTUNAddBytes(GetPayload, result);

  // padding
  while (Length(result) mod 4) <> 0 do
    SetLength(result, Length(result) + 1);
end;

constructor TsgcSTUN_Attribute_XOR_MAPPED_ADDRESS.Create;
begin
  inherited;
  AttributeType := stmaXOR_Mapped_Address;
end;

procedure TsgcSTUN_Attribute_XOR_MAPPED_ADDRESS.DoReadPayload(Value: TBytes);
var
  i: Integer;
  vAddress: Cardinal;
  oBytes, oXOR: TBytes;
begin
  inherited;
  FFamily := sgcGetWordFromBytes(Value, FOffset);
  IncOffset(2);
  FPort := sgcGetWordFromBytes(Value, FOffset);
  IncOffset(2);
  FPort := FPort xor (CS_STUN_MAGIC_COOKIE shr 16);
  if FFamily = 1 then
  begin
    vAddress := sgcGetCardinalFromBytes(Value, FOffset)
      xor CS_STUN_MAGIC_COOKIE;
    SetLength(oBytes, SizeOf(vAddress));
    sgcMove(vAddress, oBytes[0], SizeOf(vAddress));
    for i := Length(oBytes) - 1 Downto 0 do
    begin
      if FAddress <> '' then
        FAddress := FAddress + '.';
      FAddress := FAddress + IntToStr(oBytes[i]);
    end;
    IncOffset(4);
  end
  else if FFamily = 2 then
  begin
    // ... ipv6
    SetLength(oBytes, 16);
    sgcMove(Value[FOffset], oBytes[0], Length(oBytes));
    // ... xor
    sgcSTUNAddCardinal(CS_STUN_MAGIC_COOKIE, oXOR);
    sgcSTUNAddBytes(TransactionId, oXOR);

    // ... xor bytes
    for i := 0 to Length(oBytes) - 1 do
      oBytes[i] := oBytes[i] xor oXOR[i];
    // ... ip string
    for i := 0 to 7 do
    begin
      if FAddress <> '' then
        FAddress := FAddress + ':';
      FAddress := FAddress + IntToStr(sgcGetWordFromBytes(oBytes, i * 2));
    end;
    IncOffset(16);
  end;
end;

function TsgcSTUN_Attribute_XOR_MAPPED_ADDRESS.GetPayload: TBytes;
begin
  // family
  sgcSTUNAddWord(Family, result);
  // port
  sgcSTUNAddWord(Word(Port) xor (CS_STUN_MAGIC_COOKIE shr 16), result);
  // address
  case Family of
    1:
      sgcSTUNAddBytes(sgcSTUNXorIpV4(Address, CS_STUN_MAGIC_COOKIE), result);
    2:
      sgcSTUNAddBytes(sgcSTUNXorIpV6(Address, CS_STUN_MAGIC_COOKIE,
        TransactionId), result);
  end;
end;

procedure TsgcSTUN_Attribute_XOR_MAPPED_ADDRESS.SetTransactionId
  (const Value: TBytes);
begin
  if Assigned(Value) then
  begin
    SetLength(FTransactionId, Length(Value));
    sgcMove(Value[0], FTransactionId[0], Length(FTransactionId));
  end
  else
    SetLength(FTransactionId, 0);
end;

constructor TsgcSTUN_Attribute_MAPPED_ADDRESS.Create;
begin
  inherited;
  AttributeType := stmaMapped_Address;
end;

procedure TsgcSTUN_Attribute_MAPPED_ADDRESS.DoReadPayload(Value: TBytes);
var
  i: Integer;
begin
  inherited;
  FFamily := sgcGetWordFromBytes(Value, FOffset);
  IncOffset(2);
  FPort := sgcGetWordFromBytes(Value, FOffset);
  IncOffset(2);
  if FFamily = 1 then
  begin
    for i := 0 to 3 do
    begin
      if FAddress <> '' then
        FAddress := FAddress + '.';
      FAddress := FAddress + IntToStr(Value[i + FOffset]);
    end;
    IncOffset(4);
  end
  else if FFamily = 2 then
  begin
    for i := 0 to 7 do
    begin
      if FAddress <> '' then
        FAddress := FAddress + ':';
      FAddress := FAddress + IntToStr(sgcGetWordFromBytes(Value,
        FOffset + i * 2));
    end;
    IncOffset(16);
  end;
end;

function TsgcSTUN_Attribute_MAPPED_ADDRESS.GetPayload: TBytes;
begin
  // family
  sgcSTUNAddWord(Family, result);
  // port
  sgcSTUNAddWord(Port, result);
  // address
  case Family of
    1:
      sgcSTUNAddBytes(sgcSTUNXorIpV4(Address, 0), result);
    2:
      sgcSTUNAddBytes(sgcSTUNXorIpV6(Address, 0, nil), result);
  end;
end;

constructor TsgcSTUN_Attribute_USERNAME.Create;
begin
  inherited;
  AttributeType := stmaUsername;
end;

procedure TsgcSTUN_Attribute_USERNAME.DoReadPayload(Value: TBytes);
begin
  inherited;
  Username := '';
  if AttributeLength > 0 then
  begin
    Username := sgcGetStringFromBytes(Value, FOffset, AttributeLength);
    IncOffset(AttributeLength);
  end;
end;

function TsgcSTUN_Attribute_USERNAME.GetPayload: TBytes;
begin
  result := sgcGetBytesFromUTF8String(Username);
end;

constructor TsgcSTUN_Attribute_MESSAGE_INTEGRITY.Create;
begin
  inherited;
  AttributeType := stmaMesssage_Integrity;
  SetLength(FPayLoad, CS_STUN_ATTRIBUTE_MESSAGE_INTEGRITY_LENGTH);
end;

procedure TsgcSTUN_Attribute_MESSAGE_INTEGRITY.DoReadPayload(Value: TBytes);
begin
  inherited;
  SetLength(FPayLoad, AttributeLength);
  sgcMove(Value[FOffset], FPayLoad[0], Length(FPayLoad));
  IncOffset(AttributeLength);
end;

function TsgcSTUN_Attribute_MESSAGE_INTEGRITY.GetPayload: TBytes;
begin
  result := PayLoad;
end;

procedure TsgcSTUN_Attribute_MESSAGE_INTEGRITY.SetKey(const Value: TBytes);
begin
  SetLength(FKey, Length(Value));
  sgcMove(Value[0], FKey[0], Length(FKey));
end;

procedure TsgcSTUN_Attribute_MESSAGE_INTEGRITY.SetPayLoad(const Value: TBytes);
begin
  SetLength(FPayLoad, Length(Value));
  sgcMove(Value[0], FPayLoad[0], Length(FPayLoad));
end;

constructor TsgcSTUN_Attribute_FINGERPRINT.Create;
begin
  inherited;
  AttributeType := stmaFingerprint;
end;

procedure TsgcSTUN_Attribute_FINGERPRINT.DoReadPayload(Value: TBytes);
var
  oBytes: TBytes;
begin
  inherited;
  SetLength(oBytes, AttributeLength);
  sgcMove(Value[FOffset], oBytes[0], Length(oBytes));
  IncOffset(AttributeLength);

  Fingerprint := sgcGetCardinalFromBytes(oBytes);
end;

function TsgcSTUN_Attribute_FINGERPRINT.GetPayload: TBytes;
begin
  sgcSTUNAddCardinal(Fingerprint, result);
end;

constructor TsgcSTUN_Attribute_ERROR_CODE.Create;
begin
  inherited;
  AttributeType := stmaError_Code;
end;

procedure TsgcSTUN_Attribute_ERROR_CODE.DoReadPayload(Value: TBytes);
begin
  inherited;
  IncOffset(2); // avoid 2 first bytes
  FCode := sgcGetWordFromBytes(Value, FOffset);
  FCode := (FCode and $FF) + ((FCode and $700) shr 8) * 100;
  IncOffset(2);
  FReason := sgcGetStringFromBytes(Value, FOffset, AttributeLength - 4);
  IncOffset(AttributeLength - 4);
end;

function TsgcSTUN_Attribute_ERROR_CODE.GetPayload: TBytes;
begin
  sgcSTUNAddWord(0, result);
  sgcSTUNAddWord((Code mod 100) or (((Code div 100) and 7) shl 8), result);
  if Reason <> '' then
    sgcSTUNAddBytes(sgcGetBytesFromUTF8String(Reason), result);
end;

constructor TsgcSTUN_Attribute_REALM.Create;
begin
  inherited;
  AttributeType := stmaRealm;
end;

procedure TsgcSTUN_Attribute_REALM.DoReadPayload(Value: TBytes);
begin
  inherited;
  Realm := '';
  if AttributeLength > 0 then
  begin
    Realm := sgcGetStringFromBytes(Value, FOffset, AttributeLength);
    IncOffset(AttributeLength);
  end;
end;

function TsgcSTUN_Attribute_REALM.GetPayload: TBytes;
begin
  result := sgcGetBytesFromUTF8String(Realm);
end;

constructor TsgcSTUN_Attribute_NONCE.Create;
begin
  inherited;
  AttributeType := stmaNonce;
end;

procedure TsgcSTUN_Attribute_NONCE.DoReadPayload(Value: TBytes);
begin
  Nonce := '';
  if AttributeLength > 0 then
  begin
    Nonce := sgcGetStringFromBytes(Value, FOffset, AttributeLength);
    IncOffset(AttributeLength);
  end;
end;

function TsgcSTUN_Attribute_NONCE.GetPayload: TBytes;
begin
  result := sgcGetBytesFromUTF8String(Nonce);
end;

constructor TsgcSTUN_Attribute_UNKNOWN_ATTRIBUTES.Create;
begin
  inherited;
  AttributeType := stmaUnknown_Attributes;
  FUnknownAttributes := [];
end;

procedure TsgcSTUN_Attribute_UNKNOWN_ATTRIBUTES.DoReadPayload(Value: TBytes);
var
  i: Integer;
begin
  inherited;
  i := 0;
  while i < AttributeLength do
  begin
    UnknownAttributes := UnknownAttributes +
      [GetAttributeTypeFromValue(sgcGetWordFromBytes(Value, FOffset))];
    IncOffset(2);
    i := 2;
  end;
end;

function TsgcSTUN_Attribute_UNKNOWN_ATTRIBUTES.GetPayload: TBytes;
var
  i: TsgcStunMessageAttribute;
begin
  inherited;
  result := nil;
  for i := Low(TsgcStunMessageAttribute) to High(TsgcStunMessageAttribute) do
  begin
    if i in UnknownAttributes then
      sgcSTUNAddWord(GetAttributeTypeFromType(i), result);
  end;
end;

constructor TsgcSTUN_Attribute_SOFTWARE.Create;
begin
  inherited;
  AttributeType := stmaSoftware;
end;

procedure TsgcSTUN_Attribute_SOFTWARE.DoReadPayload(Value: TBytes);
begin
  inherited;
  Software := '';
  if AttributeLength > 0 then
  begin
    Software := sgcGetStringFromBytes(Value, FOffset, AttributeLength);
    IncOffset(AttributeLength);
  end;
end;

function TsgcSTUN_Attribute_SOFTWARE.GetPayload: TBytes;
begin
  result := sgcGetBytesFromUTF8String(Software);
end;

constructor TsgcSTUN_Attribute_ALTERNATE_SERVER.Create;
begin
  inherited;
  AttributeType := stmaAlternate_Server;
end;

constructor TsgcSTUN_Attribute_CHANGE_REQUEST.Create;
begin
  inherited;
  AttributeType := stmaChange_Request;
end;

procedure TsgcSTUN_Attribute_CHANGE_REQUEST.DoReadPayload(Value: TBytes);
begin
  inherited;
end;

function TsgcSTUN_Attribute_CHANGE_REQUEST.GetPayload: TBytes;
begin
  result := nil;
end;

constructor TsgcSTUN_Attribute_RESPONSE_PORT.Create;
begin
  inherited;
  AttributeType := stmaResponse_Port;
end;

procedure TsgcSTUN_Attribute_RESPONSE_PORT.DoReadPayload(Value: TBytes);
begin
  inherited;
end;

function TsgcSTUN_Attribute_RESPONSE_PORT.GetPayload: TBytes;
begin
  result := nil;
end;

constructor TsgcSTUN_Attribute_PADDING.Create;
begin
  inherited;
  AttributeType := stmaPadding;
end;

procedure TsgcSTUN_Attribute_PADDING.DoReadPayload(Value: TBytes);
begin
  inherited;
end;

function TsgcSTUN_Attribute_PADDING.GetPayload: TBytes;
begin
  result := nil;
end;

constructor TsgcSTUN_Attribute_CACHE_TIMEOUT.Create;
begin
  inherited;
  AttributeType := stmaCache_Timeout;
end;

procedure TsgcSTUN_Attribute_CACHE_TIMEOUT.DoReadPayload(Value: TBytes);
begin
  inherited;
end;

function TsgcSTUN_Attribute_CACHE_TIMEOUT.GetPayload: TBytes;
begin
  result := nil;
end;

constructor TsgcSTUN_Attribute_RESPONSE_ORIGIN.Create;
begin
  inherited;
  AttributeType := stmaResponse_Origin;
end;

constructor TsgcSTUN_Attribute_OTHER_ADDRESS.Create;
begin
  inherited;
  AttributeType := stmaOther_Address;
end;

constructor TsgcSTUN_Attribute_SOURCE_ADDRESS.Create;
begin
  inherited;
  AttributeType := stmaSource_Address;
end;

constructor TsgcSTUN_Attribute_CHANGED_ADDRESS.Create;
begin
  inherited;
  AttributeType := stmaChanged_Address;
end;

constructor TsgcSTUN_Message.Create;
begin
  inherited;
  FFingerprint := stfsNone;
  FCredentials := stauNone;
  FMessageIntegrity := stisNone;
  FMessageIntegritySHA256 := stisNone;
end;

destructor TsgcSTUN_Message.Destroy;
begin
  sgcFree(FHeader);
  sgcFree(FAttributes);
  inherited;
end;

procedure TsgcSTUN_Message.DoComputeFingerprint;
var
  oAttribute: TsgcSTUN_Attribute_FINGERPRINT;
begin
  oAttribute := TsgcSTUN_Attribute_FINGERPRINT.Create();
  oAttribute.Fingerprint := GetCRC(DoWrite) XOR CS_STUN_FINGERPRINT_XOR;
  Attributes.Add(oAttribute);
end;

function xHexToBin(const HexStr: String): TBytes;
const
  HexSymbols = '0123456789ABCDEF';
var
  i, J: Integer;
  B: Byte;
begin
  SetLength(result, (Length(HexStr) + 1) shr 1);
  B := 0;
  i := 0;
  while i < Length(HexStr) do
  begin
    J := 0;
    while J < Length(HexSymbols) do
    begin
      if HexStr[i + 1] = HexSymbols[J + 1] then
        break;
      Inc(J);
    end;
    if J = Length(HexSymbols) then; // error
    if Odd(i) then
      result[i shr 1] := B shl 4 + J
    else
      B := J;
    Inc(i);
  end;
  if Odd(i) then
    result[i shr 1] := B;
end;

function bintoHex(const bin: array of Byte): String;
const
  HexSymbols = '0123456789ABCDEF';
var
  i: Integer;
begin
  SetLength(result, 2 * Length(bin));
  for i := 0 to Length(bin) - 1 do
  begin
    result[1 + 2 * i + 0] := HexSymbols[1 + bin[i] shr 4];
    result[1 + 2 * i + 1] := HexSymbols[1 + bin[i] and $0F];
  end;
end;

procedure GetErrorResponseCode(aValue: TsgcStunErrorResponseCodes;
  var Code: Word; var Reason: String);
begin
  case aValue of
    sercTryAlternate:
      begin
        Code := 300;
        Reason := S_STUN_TRY_ALTERNATE;
      end;
    sercInvalidRequest:
      begin
        Code := 400;
        Reason := S_STUN_INVALID_REQUEST;
      end;
    sercUnauthorized:
      begin
        Code := 401;
        Reason := S_STUN_UNAUTHORIZED;
      end;
    sercForbidden:
      begin
        Code := 403;
        Reason := S_STUN_FORBIDDEN;
      end;
    sercUnknownAttribute:
      begin
        Code := 420;
        Reason := S_STUN_UNKNOWN_ATTRIBUTE;
      end;
    sercAllocationMismatch:
      begin
        Code := 437;
        Reason := S_STUN_ALLOCATION_MISMATCH;
      end;
    sercStaleNonce:
      begin
        Code := 438;
        Reason := S_STUN_STALE_NONCE;
      end;
    sercAddressFamilyNotSupported:
      begin
        Code := 440;
        Reason := S_STUN_ADDRESS_FAMILY_NOT_SUPPORTED;
      end;
    sercWrongCredentials:
      begin
        Code := 441;
        Reason := S_STUN_WRONG_CREDENTIALS;
      end;
    sercUnsupportedTransportProtocol:
      begin
        Code := 442;
        Reason := S_STUN_UNSUPPORTED_TRANSPORT_PROTOCOL;
      end;
    sercPeerAddressFamilyMismatch:
      begin
        Code := 443;
        Reason := S_STUN_PEER_ADDRESS_FAMILY_MISMATCH;
      end;
    sercAllocationQuotaReached:
      begin
        Code := 486;
        Reason := S_STUN_ALLOCATION_QUOTA_REACHED;
      end;
    sercICERoleConflict:
      begin
        Code := 487;
        Reason := S_STUN_ICE_ROLE_CONFLICT;
      end;
    sercInsufficientCapacity:
      begin
        Code := 508;
        Reason := S_STUN_INSUFFICIENT_CAPACITY;
      end;
  end;
end;

procedure TsgcSTUN_Message.DoComputeCredentials;
var
  oAttribute: TsgcSTUN_Attribute;
  vKey: string;
begin
  case Credentials of
    stauShortTermCredential:
      begin
        vKey := '';
        if Assigned(FOnICERequestMessageIntegrityPassword) then
          FOnICERequestMessageIntegrityPassword(self, self, vKey);
      end;
    stauLongTermCredential:
      begin
        vKey := Username + ':' + Realm + ':' + Password;
        if MessageType = stmtClientRequest then
        begin
          // ... username
          oAttribute := TsgcSTUN_Attribute_USERNAME.Create();
          TsgcSTUN_Attribute_USERNAME(oAttribute).Username := Username;
          Attributes.Add(oAttribute);
          // ... realm
          oAttribute := TsgcSTUN_Attribute_REALM.Create();
          TsgcSTUN_Attribute_REALM(oAttribute).Realm := Realm;
          Attributes.Add(oAttribute);
          // ... nonce
          oAttribute := TsgcSTUN_Attribute_NONCE.Create();
          TsgcSTUN_Attribute_NONCE(oAttribute).Nonce := Nonce;
          Attributes.Add(oAttribute);
        end;
      end;
  end;

  // ... message integrity
  DoComputeHeaderMessageLength;
  Header.MessageLength := Header.MessageLength + CS_STUN_ATTRIBUTE_HEADER_LENGTH
    + CS_STUN_ATTRIBUTE_MESSAGE_INTEGRITY_LENGTH;

  oAttribute := TsgcSTUN_Attribute_MESSAGE_INTEGRITY.Create();
  case Credentials of
    stauShortTermCredential:
      begin
        TsgcSTUN_Attribute_MESSAGE_INTEGRITY(oAttribute).Key :=
          sgcGetBytesFromUTF8String(vKey);
      end;
    stauLongTermCredential:
      begin
        TsgcSTUN_Attribute_MESSAGE_INTEGRITY(oAttribute).Key :=
          GetMD5(sgcGetBytesFromUTF8String(vKey));
      end;
  end;
  TsgcSTUN_Attribute_MESSAGE_INTEGRITY(oAttribute).PayLoad :=
    GetHMACSHA1(DoWrite, TsgcSTUN_Attribute_MESSAGE_INTEGRITY(oAttribute).Key);
  Attributes.Add(oAttribute);

  // ... message integrity SHA256
  if MessageIntegritySHA256 = stisMessageIntegrityValid then
  begin
    DoComputeHeaderMessageLength;
    Header.MessageLength := Header.MessageLength +
      CS_STUN_ATTRIBUTE_HEADER_LENGTH +
      CS_STUN_ATTRIBUTE_MESSAGE_INTEGRITY_SHA256_LENGTH;

    oAttribute := TsgcSTUN_Attribute_MESSAGE_INTEGRITY_SHA256.Create();
    TsgcSTUN_Attribute_MESSAGE_INTEGRITY_SHA256(oAttribute).Key :=
      sgcGetBytesFromUTF8String(vKey);
    TsgcSTUN_Attribute_MESSAGE_INTEGRITY(oAttribute).PayLoad :=
      GetHMACSHA256(DoWrite, TsgcSTUN_Attribute_MESSAGE_INTEGRITY
      (oAttribute).Key);
    Attributes.Add(oAttribute);
  end;
end;

procedure TsgcSTUN_Message.DoComputeHeaderMessageLength;
var
  i: Integer;
begin
  Header.MessageLength := 0;
  for i := 0 to Attributes.Count - 1 do
    Header.MessageLength := Header.MessageLength +
      TsgcSTUN_Attribute(Attributes.Items[i]).GetPayloadLength +
      CS_STUN_ATTRIBUTE_HEADER_LENGTH;
end;

procedure TsgcSTUN_Message.DoReadPayload(Bytes: TBytes);
var
  i: Integer;
  oClass: TsgcSTUN_Attribute_Class;
  oAttribute: TsgcSTUN_Attribute;
  oMessageIntegrity: TsgcSTUN_Attribute_MESSAGE_INTEGRITY;
  oMessageIntegritySHA256: TsgcSTUN_Attribute_MESSAGE_INTEGRITY_SHA256;
  vAttribute: Word;
  vMessageIntegrity: Boolean;
  vICE: Boolean;
begin
  i := 20;
  vMessageIntegrity := False;
  vICE := False;
  oMessageIntegrity := nil;
  oMessageIntegritySHA256 := nil;

  while i < Length(Bytes) - 1 do
  begin
    vAttribute := sgcGetWordFromBytes(Bytes, i);
    oClass := GetAttributeClassFromType(GetAttributeTypeFromValue(vAttribute));
    if Assigned(oClass) then
    begin
      oAttribute := oClass.Create();
      oAttribute.Read(Bytes, i);
      if oAttribute.AttributeType <> stmaMapped_None then
      begin
        if (oAttribute.AttributeType = stmaICE_Controlled) or
          (oAttribute.AttributeType = stmaICE_Controlling) then
          vICE := True;

        // ... ignore attributes that follow MESSAGE_INTEGRITY
        // ... except MESSAGE_INTEGRITY_SHA256 or FINGERPRINT
        if (oAttribute.AttributeType = stmaFingerprint) or
          (vMessageIntegrity = False) or
          (vMessageIntegrity and
          ((oAttribute.AttributeType = stmaMesssage_Integrity_SHA256))) then
        begin
          Attributes.Add(oAttribute);
          if oAttribute.AttributeType = stmaMesssage_Integrity then
          begin
            vMessageIntegrity := True;
            oMessageIntegrity := TsgcSTUN_Attribute_MESSAGE_INTEGRITY
              (oAttribute);
          end
          else if oAttribute.AttributeType = stmaMesssage_Integrity_SHA256 then
            oMessageIntegritySHA256 :=
              TsgcSTUN_Attribute_MESSAGE_INTEGRITY_SHA256(oAttribute);
        end;

        if oAttribute.AttributeType = stmaFingerprint then
        begin
          if IsFingerprintValid(oAttribute, Bytes) then
            Fingerprint := stfsFingerprintValid
          else
            Fingerprint := stfsFingerprintInvalid;
        end;
      end;
    end
  end;

  if Assigned(oMessageIntegrity) then
    DoValidateMessageIntegrity(oMessageIntegrity, Bytes, vICE);
  if Assigned(oMessageIntegritySHA256) then
    DoValidateMessageIntegrity(oMessageIntegritySHA256, Bytes, vICE);
end;

procedure TsgcSTUN_Message.DoReadHeader(Bytes: TBytes);
begin
  Header.Read(Bytes);
end;

function TsgcSTUN_Message.DoWrite: TBytes;
begin
  sgcSTUNAddBytes(Header.Write, result);
  if Attributes.Count > 0 then
    sgcSTUNAddBytes(Attributes.Write, result);
end;

function TsgcSTUN_Message.IsFingerprintValid(const aAttribute
  : TsgcSTUN_Attribute; const aBytes: TBytes): Boolean;
var
  oBytes: TBytes;
begin
  SetLength(oBytes, Length(aBytes) - CS_STUN_ATTRIBUTE_HEADER_LENGTH -
    CS_STUN_ATTRIBUTE_FINGERPRINT_LENGTH);
  sgcMove(aBytes[0], oBytes[0], Length(oBytes));

  result := Cardinal(GetCRC(oBytes) xor CS_STUN_FINGERPRINT_XOR)
    = TsgcSTUN_Attribute_FINGERPRINT(aAttribute).Fingerprint;
end;

function TsgcSTUN_Message.GetAttributes: TsgcSTUN_AttributeList;
begin
  if not Assigned(FAttributes) then
    FAttributes := TsgcSTUN_AttributeList.Create;
  result := FAttributes;
end;

function TsgcSTUN_Message.GetHeader: TsgcSTUN_Header;
begin
  if not Assigned(FHeader) then
    FHeader := TsgcSTUN_Header.Create;
  result := FHeader;
end;

procedure TsgcSTUN_Message.DoValidateMessageIntegrity(const aAttribute
  : TsgcSTUN_Attribute; const aBytes: TBytes; aICE: Boolean);
var
  i: Integer;
  oAttribute: TsgcSTUN_Attribute;
  vUsername, vRealm, vNonce, vPassword: string;
  oBytes, oBytesLength, oBytesHMAC, oMessageIntegrityKey: TBytes;
  vLength, vValue: Integer;
begin
  MessageIntegrity := stisMessageIntegrityInvalid;
  oBytesHMAC := nil;
  vUsername := '';
  vRealm := '';
  vNonce := '';

  SetLength(oMessageIntegrityKey, 0);
  if aICE then
  begin
    // ... username
    oAttribute := Attributes.GetAttribute(stmaUsername);
    if Assigned(oAttribute) then
      vUsername := TsgcSTUN_Attribute_USERNAME(oAttribute).Username
    else
    begin
      MessageIntegrity := stisMessageIntegrityInvalidRequest;
      exit;
    end;

    if Assigned(FOnICEValidateMessageIntegrity) then
      FOnICEValidateMessageIntegrity(self, vUsername, vPassword);

    // ... calculate Message Integrity Key
    oMessageIntegrityKey := sgcGetBytesFromUTF8String(vPassword)
  end
  else
  begin
    if Assigned(FOnRequestMessageIntegrityKey) then
    begin
      FOnRequestMessageIntegrityKey(self, Header.TransactionIdString,
        oMessageIntegrityKey);
    end
    else
    begin
      if aAttribute.AttributeType = stmaMesssage_Integrity then
      begin
        // ... username
        oAttribute := Attributes.GetAttribute(stmaUsername);
        if Assigned(oAttribute) then
          vUsername := TsgcSTUN_Attribute_USERNAME(oAttribute).Username
        else
        begin
          MessageIntegrity := stisMessageIntegrityInvalidRequest;
          exit;
        end;

        // ... realm
        oAttribute := Attributes.GetAttribute(stmaRealm);
        if Assigned(oAttribute) then
          vRealm := TsgcSTUN_Attribute_REALM(oAttribute).Realm
        else
        begin
          MessageIntegrity := stisMessageIntegrityInvalidRequest;
          exit;
        end;
        // ... nonce
        oAttribute := Attributes.GetAttribute(stmaNonce);
        if Assigned(oAttribute) then
          vNonce := TsgcSTUN_Attribute_NONCE(oAttribute).Nonce
        else
        begin
          MessageIntegrity := stisMessageIntegrityInvalidRequest;
          exit;
        end;
      end;
      // ... password
      if Assigned(FOnRequestMessageIntegrityPassword) then
        FOnRequestMessageIntegrityPassword(self, vUsername, vRealm, vPassword);

      // ... calculate Message Integrity Key
      case aAttribute.AttributeType of
        stmaMesssage_Integrity:
          oMessageIntegrityKey :=
            GetMD5(sgcGetBytesFromUTF8String(vUsername + ':' + vRealm + ':' +
            vPassword));
        stmaMesssage_Integrity_SHA256:
          oMessageIntegrityKey := sgcGetBytesFromUTF8String(vPassword);
      end;
    end;
  end;

  // ... copy message till length before hmac
  vLength := 0;
  case aAttribute.AttributeType of
    stmaMesssage_Integrity:
      vLength := CS_STUN_ATTRIBUTE_MESSAGE_INTEGRITY_LENGTH;
    stmaMesssage_Integrity_SHA256:
      vLength := CS_STUN_ATTRIBUTE_MESSAGE_INTEGRITY_SHA256_LENGTH;
  end;
  vValue := Header.MessageLength + CS_STUN_HEADER_LENGTH -
    CS_STUN_ATTRIBUTE_HEADER_LENGTH - vLength;
  if Fingerprint <> stfsNone then
    vValue := vValue - CS_STUN_ATTRIBUTE_HEADER_LENGTH -
      CS_STUN_ATTRIBUTE_FINGERPRINT_LENGTH;
  SetLength(oBytes, vValue);
  sgcMove(aBytes[0], oBytes[0], Length(oBytes));
  // ... update message length
  vValue := Header.MessageLength;
  if Fingerprint <> stfsNone then
    vValue := vValue - CS_STUN_ATTRIBUTE_HEADER_LENGTH -
      CS_STUN_ATTRIBUTE_FINGERPRINT_LENGTH;
  sgcSTUNAddWord(vValue, oBytesLength);
  oBytes[2] := oBytesLength[0];
  oBytes[3] := oBytesLength[1];

  // ... compute hmac
  case aAttribute.AttributeType of
    stmaMesssage_Integrity:
      oBytesHMAC := GetHMACSHA1(oBytes, oMessageIntegrityKey);
    stmaMesssage_Integrity_SHA256:
      oBytesHMAC := GetHMACSHA256(oBytes, oMessageIntegrityKey);
  end;

  // ... compare arrays
  for i := 0 to Length(oBytesHMAC) - 1 do
  begin
    if oBytesHMAC[i] <> TsgcSTUN_Attribute_MESSAGE_INTEGRITY(aAttribute)
      .PayLoad[i] then
      exit;
  end;

  MessageIntegrity := stisMessageIntegrityValid;
end;

procedure TsgcSTUN_Message.Read(Bytes: TBytes);
begin
  // ... header
  DoReadHeader(Bytes);
  // ... payload
  DoReadPayload(Bytes);
end;

procedure TsgcSTUN_Message.SetMessageType(const Value: TsgcStunMessageType);
begin
  FMessageType := Value;
end;

function TsgcSTUN_Message.Write: TBytes;
var
  oAttribute: TsgcSTUN_Attribute;
begin
  if Fingerprint = stfsFingerprintValid then
  begin
    if (Credentials = stauShortTermCredential) or
      (Credentials = stauLongTermCredential) then
      DoComputeCredentials;
    // ... add the 8 bytes of fingerprint
    DoComputeHeaderMessageLength;
    Header.MessageLength := Header.MessageLength +
      CS_STUN_ATTRIBUTE_HEADER_LENGTH + CS_STUN_ATTRIBUTE_FINGERPRINT_LENGTH;
    // fingerprint
    DoComputeFingerprint;
  end
  else if MessageIntegrity = stisMessageIntegrityValid then
  begin
    // ... message integrity
    DoComputeHeaderMessageLength;
    Header.MessageLength := Header.MessageLength +
      CS_STUN_ATTRIBUTE_HEADER_LENGTH +
      CS_STUN_ATTRIBUTE_MESSAGE_INTEGRITY_LENGTH;

    oAttribute := TsgcSTUN_Attribute_MESSAGE_INTEGRITY.Create();
    TsgcSTUN_Attribute_MESSAGE_INTEGRITY(oAttribute).Key :=
      GetMD5(sgcGetBytesFromUTF8String(Username + ':' + Realm + ':' +
      Password));
    TsgcSTUN_Attribute_MESSAGE_INTEGRITY(oAttribute).PayLoad :=
      GetHMACSHA1(DoWrite, TsgcSTUN_Attribute_MESSAGE_INTEGRITY
      (oAttribute).Key);
    Attributes.Add(oAttribute);
  end;

  result := DoWrite;
end;

destructor TsgcSTUN_AttributeList.Destroy;
var
  oObject: TsgcSTUN_Attribute;
begin
  while Count > 0 do
  begin
    oObject := TsgcSTUN_Attribute(Items[0]);
    sgcFree(oObject);
    Delete(0);
  end;
  inherited;
end;

function TsgcSTUN_AttributeList.GetAttribute(const aType
  : TsgcStunMessageAttribute): TsgcSTUN_Attribute;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
  begin
    if TsgcSTUN_Attribute(Items[i]).AttributeType = aType then
    begin
      result := TsgcSTUN_Attribute(Items[i]);
      break;
    end;
  end;
end;

function TsgcSTUN_AttributeList.Write: TBytes;
var
  i: Integer;
  oBytes: TBytes;
begin
  result := nil;
  oBytes := nil;

  for i := 0 to Count - 1 do
  begin
    oBytes := TsgcSTUN_Attribute(Items[i]).Write;
    sgcSTUNAddBytes(oBytes, result);
  end;
end;

procedure TsgcSTUN_Attribute_None.DoReadPayload(Value: TBytes);
begin
  inherited;
end;

constructor TsgcSTUN_Options.Create;
begin
  inherited;
  FFingerprint := False;
  FSoftware := True;
end;

procedure TsgcSTUN_Options.Assign(aSource: TPersistent);
begin
  if aSource is TsgcSTUN_Options then
  begin
    Fingerprint := TsgcSTUN_Options(aSource).Fingerprint;
    Software := TsgcSTUN_Options(aSource).Software;
  end
  else
    inherited Assign(aSource);
end;

constructor TsgcSTUN_Attribute_MESSAGE_INTEGRITY_SHA256.Create;
begin
  inherited;
  AttributeType := stmaMesssage_Integrity_SHA256;
  SetLength(FPayLoad, CS_STUN_ATTRIBUTE_MESSAGE_INTEGRITY_SHA256_LENGTH);
end;

procedure TsgcSTUN_Attribute_MESSAGE_INTEGRITY_SHA256.DoReadPayload
  (Value: TBytes);
begin
  inherited;
  SetLength(FPayLoad, AttributeLength);
  sgcMove(Value[FOffset], FPayLoad[0], Length(FPayLoad));
  IncOffset(AttributeLength);
end;

function TsgcSTUN_Attribute_MESSAGE_INTEGRITY_SHA256.GetPayload: TBytes;
begin
  result := PayLoad;
end;

procedure TsgcSTUN_Attribute_MESSAGE_INTEGRITY_SHA256.SetKey
  (const Value: TBytes);
begin
  SetLength(FKey, Length(Value));
  sgcMove(Value[0], FKey[0], Length(FKey));
end;

procedure TsgcSTUN_Attribute_MESSAGE_INTEGRITY_SHA256.SetPayLoad
  (const Value: TBytes);
begin
  SetLength(FPayLoad, Length(Value));
  sgcMove(Value[0], FPayLoad[0], Length(FPayLoad));
end;

constructor TsgcSTUN_Attribute_PASSWORD_ALGORITHM.Create;
begin
  inherited;
  AttributeType := stmaPassword_Algorithm;
end;

constructor TsgcSTUN_Attribute_USERHASH.Create;
begin
  inherited;
  AttributeType := stmaUserhash;
end;

constructor TsgcSTUN_Attribute_PASSWORD_ALGORITHMS.Create;
begin
  inherited;
  AttributeType := stmaPassword_Algorithms;
end;

constructor TsgcSTUN_Attribute_ALTERNATE_DOMAIN.Create;
begin
  inherited;
  AttributeType := stmaAlternate_Domain;
end;

procedure TsgcSTUN_Attribute_ALTERNATE_DOMAIN.DoReadPayload(Value: TBytes);
begin
  inherited;
  Domain := sgcGetStringFromBytes(Value, FOffset, AttributeLength);
  IncOffset(AttributeLength);
end;

function TsgcSTUN_Attribute_ALTERNATE_DOMAIN.GetPayload: TBytes;
begin
  result := sgcGetBytesFromUTF8String(Domain);
end;

constructor TsgcSTUN_Attribute_CHANNEL_NUMBER.Create;
begin
  inherited;
  AttributeType := stmaChannel_Number;
end;

procedure TsgcSTUN_Attribute_CHANNEL_NUMBER.DoReadPayload(Value: TBytes);
begin
  inherited;
  Channel := sgcGetWordFromBytes(Value, FOffset);
  // Value + rfu
  IncOffset(4);
end;

function TsgcSTUN_Attribute_CHANNEL_NUMBER.GetPayload: TBytes;
begin
  sgcSTUNAddWord(Channel, result);
  sgcSTUNAddWord(0, result);
end;

constructor TsgcSTUN_Attribute_LIFETIME.Create;
begin
  inherited;
  AttributeType := stmaLifetime;
end;

procedure TsgcSTUN_Attribute_LIFETIME.DoReadPayload(Value: TBytes);
begin
  inherited;
  Lifetime := sgcGetCardinalFromBytes(Value, FOffset);
  IncOffset(4);
end;

function TsgcSTUN_Attribute_LIFETIME.GetPayload: TBytes;
begin
  sgcSTUNAddCardinal(Lifetime, result);
end;

constructor TsgcSTUN_Attribute_XOR_PEER_ADDRESS.Create;
begin
  inherited;
  AttributeType := stmaXOR_Peer_Address;
end;

constructor TsgcSTUN_Attribute_DATA.Create;
begin
  inherited;
  AttributeType := stmaData;
end;

procedure TsgcSTUN_Attribute_DATA.DoReadPayload(Value: TBytes);
begin
  inherited;
  SetLength(FData, AttributeLength);
  sgcMove(Value[FOffset], FData[0], AttributeLength);
  IncOffset(AttributeLength);
end;

function TsgcSTUN_Attribute_DATA.GetPayload: TBytes;
begin
  result := FData;
end;

procedure TsgcSTUN_Attribute_DATA.SetData(const Value: TBytes);
begin
  if Length(Value) > 0 then
  begin
    SetLength(FData, Length(Value));
    sgcMove(Value[0], FData[0], Length(FData));
  end
  else
    SetLength(FData, 0);
end;

constructor TsgcSTUN_Attribute_XOR_RELAYED_ADDRESS.Create;
begin
  inherited;
  AttributeType := stmaXOR_Relayed_Address;
end;

constructor TsgcSTUN_Attribute_REQUESTED_ADDRESS_FAMILY.Create;
begin
  inherited;
  AttributeType := stmaRequested_Address_Family;
  Family := 1;
end;

procedure TsgcSTUN_Attribute_REQUESTED_ADDRESS_FAMILY.DoReadPayload
  (Value: TBytes);
begin
  inherited;
  Family := Value[FOffset];
  Inc(FOffset, 4);
end;

function TsgcSTUN_Attribute_REQUESTED_ADDRESS_FAMILY.GetPayload: TBytes;
begin
  sgcStunAddByte(Family, result);
  // reserved bytes
  SetLength(result, Length(result) + 3);
end;

constructor TsgcSTUN_Attribute_EVEN_PORT.Create;
begin
  inherited;
  AttributeType := stmaEven_port;
end;

procedure TsgcSTUN_Attribute_EVEN_PORT.DoReadPayload(Value: TBytes);
begin
  inherited;
  Even := Value[FOffset] and $80 <> $80;
end;

function TsgcSTUN_Attribute_EVEN_PORT.GetPayload: TBytes;
begin
  if Even then
    sgcStunAddByte($80, result)
  else
    sgcStunAddByte(0, result);
end;

constructor TsgcSTUN_Attribute_REQUESTED_TRANSPORT.Create;
begin
  inherited;
  AttributeType := stmaRequested_Transport;
  Protocol := 17;
end;

procedure TsgcSTUN_Attribute_REQUESTED_TRANSPORT.DoReadPayload(Value: TBytes);
begin
  inherited;
  Protocol := Value[FOffset];
  // protocol + rffu
  IncOffset(4);
end;

function TsgcSTUN_Attribute_REQUESTED_TRANSPORT.GetPayload: TBytes;
begin
  sgcStunAddByte(Protocol, result);
  SetLength(result, Length(result) + 3);
end;

constructor TsgcSTUN_Attribute_DONT_FRAGMENT.Create;
begin
  inherited;
  AttributeType := stmaDont_Fragment;
end;

procedure TsgcSTUN_Attribute_DONT_FRAGMENT.DoReadPayload(Value: TBytes);
begin
  inherited;
end;

constructor TsgcSTUN_Attribute_RESERVATION_TOKEN.Create;
begin
  inherited;
  AttributeType := stmaReservation_Token;
  SetLength(FToken, 8);
end;

procedure TsgcSTUN_Attribute_RESERVATION_TOKEN.DoReadPayload(Value: TBytes);
begin
  inherited;
  sgcMove(Value[FOffset], Token[0], Length(Token));
  IncOffset(Length(Token));
end;

function TsgcSTUN_Attribute_RESERVATION_TOKEN.GetPayload: TBytes;
begin
  result := Token;
end;

constructor TsgcSTUN_Attribute_ADDITIONAL_ADDRESS_FAMILY.Create;
begin
  inherited;
  AttributeType := stmaAdditional_Address_Family;
end;

constructor TsgcSTUN_Attribute_ADDRESS_ERROR_CODE.Create;
begin
  inherited;
  AttributeType := stmaAddress_Error_Code;
  Family := 1;
end;

procedure TsgcSTUN_Attribute_ADDRESS_ERROR_CODE.DoReadPayload(Value: TBytes);
begin
  inherited;
  Family := Value[FOffset];
  IncOffset(2);
  FCode := sgcGetWordFromBytes(Value, FOffset);
  FCode := (FCode and $FF) + ((FCode and $700) shr 8) * 100;
  IncOffset(2);
  FReason := sgcGetStringFromBytes(Value, FOffset, AttributeLength - 4);
  IncOffset(AttributeLength - 4);
end;

function TsgcSTUN_Attribute_ADDRESS_ERROR_CODE.GetPayload: TBytes;
begin
  sgcStunAddByte(Family, result);
  sgcStunAddByte(0, result);
  sgcSTUNAddWord((Code mod 100) or (((Code div 100) and 7) shl 8), result);
  if Reason <> '' then
    sgcSTUNAddBytes(sgcGetBytesFromUTF8String(Reason), result);
end;

constructor TsgcSTUN_Attribute_ICMP.Create;
begin
  inherited;
  AttributeType := stmaICMP;
end;

procedure TsgcSTUN_Attribute_ICMP.DoReadPayload(Value: TBytes);
begin
  inherited;
  if sgcGetWordFromBytes(Value, FOffset) <> 0 then
    raise Exception.Create(Format(S_STUN_INVALID_FORMAT_ATTRIBUTE, ['ICMP']));
  IncOffset(2);
  ICMPType := Value[FOffset];
  IncOffset(1);
  ICMPCode := Value[FOffset];
  IncOffset(1);
  ErrorData := sgcGetCardinalFromBytes(Value, FOffset);
  IncOffset(4);
end;

function TsgcSTUN_Attribute_ICMP.GetPayload: TBytes;
begin
  sgcSTUNAddWord(0, result);
  sgcStunAddByte(ICMPType, result);
  sgcStunAddByte(ICMPCode, result);
  sgcSTUNAddCardinal(ErrorData, result);
end;

constructor TsgcSTUN_Attribute_PRIORITY.Create;
begin
  inherited;
  AttributeType := stmaPriority;
  FPriority := 0;
end;

procedure TsgcSTUN_Attribute_PRIORITY.DoReadPayload(Value: TBytes);
begin
  inherited;
  Priority := sgcGetCardinalFromBytes(Value, FOffset);
  IncOffset(4);
end;

function TsgcSTUN_Attribute_PRIORITY.GetPayload: TBytes;
begin
  sgcSTUNAddCardinal(Priority, result);
end;

constructor TsgcSTUN_Attribute_USE_CANDIDATE.Create;
begin
  inherited;
  AttributeType := stmaUse_Candidate;
end;

procedure TsgcSTUN_Attribute_USE_CANDIDATE.DoReadPayload(Value: TBytes);
begin
  inherited;
end;

function TsgcSTUN_Attribute_USE_CANDIDATE.GetPayload: TBytes;
begin
  Result := nil;
end;

constructor TsgcSTUN_Attribute_ICE_CONTROL_BASE.Create;
begin
  inherited;
  SetLength(FTieBreaker, 8);
end;

procedure TsgcSTUN_Attribute_ICE_CONTROL_BASE.DoReadPayload(Value: TBytes);
begin
  inherited;
  sgcMove(Value[FOffset], TieBreaker[0], Length(TieBreaker));
  IncOffset(Length(TieBreaker));
end;

function TsgcSTUN_Attribute_ICE_CONTROL_BASE.GetPayload: TBytes;
begin
  result := TieBreaker;
end;

procedure TsgcSTUN_Attribute_ICE_CONTROL_BASE.SetTieBreaker
  (const Value: TBytes);
begin
  if Assigned(Value) then
  begin
    SetLength(FTieBreaker, Length(Value));
    sgcMove(Value[0], FTieBreaker[0], Length(FTieBreaker));
  end
  else
    SetLength(FTieBreaker, 0);
end;

constructor TsgcSTUN_Attribute_ICE_CONTROLLED.Create;
begin
  inherited;
  AttributeType := stmaICE_Controlled;
end;

constructor TsgcSTUN_Attribute_ICE_CONTROLLING.Create;
begin
  inherited;
  AttributeType := stmaICE_Controlling;
end;

{$ENDIF}

end.
