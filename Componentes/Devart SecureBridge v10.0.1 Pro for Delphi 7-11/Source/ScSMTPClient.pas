
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$I SB.inc}
unit ScSMTPClient;

interface

uses
{$IFDEF POSIX}
  Posix.Unistd,
{$ENDIF}
  SysUtils, Classes, SyncObjs,
{$IFNDEF NEXTGEN}
  Contnrs,
{$ENDIF}
  ScTypes, ScConsts, ScCLRClasses, ScUtils, ScBridge,
  ScVio, ScVioTcp, ScSecureConnection, ScSSLClient,
  ScCoders, ScMailMessage, ScSMTPUtils;

type
  TScSMTPAuthenticationType = (satNone, satDefault, satSASLMechanism);

  TScSMTPErrorEvent = procedure (Sender: TObject;
    ErrorCode: integer; const ErrorMessage: string; var Fail: boolean) of object;

  TScSMTPRecipientErrorEvent = procedure (Sender: TObject; const RecipientAddress: string;
    ReplyCode: integer; const ReplyText: string; var Skip: boolean) of object;

const
  Def_AuthType = satDefault;
  Def_Pipelined = False;
  Def_TCPKeepAlive = True;
  Def_TLSMode = tmDisableTLS;
  Def_UseEhlo = True;
  Def_UseVerp = False;
  SMTP_DEFAULT_TIMEOUT = 180; // 3 min

type
  TScSMTPClient = class;
  TScSASLCollection = class;

  TScSASLMechanism = class
  protected
    FSecurityLevel: cardinal;
  public
    constructor Create; virtual;
    class function ServiceName: string; virtual;

    function IsReadyToStart: boolean; virtual;
    function TryStartAuthenticate(const Host, Protocol: string; out InitialResponse: string): boolean; virtual;
    function StartAuthenticate(const Challenge, Host, Protocol: string): string; virtual; abstract;
    function ContinueAuthenticate(const Response, Host, Protocol: string): string; virtual;

    property SecurityLevel: cardinal read FSecurityLevel;
  end;

  TScSASLAnonymous = class(TScSASLMechanism)
  private
    FTraceInfo: string;
  public
    class function ServiceName: string; override;

    function IsReadyToStart: boolean; override;
    function TryStartAuthenticate(const Host, Protocol: string; out InitialResponse: string): boolean; override;
    function StartAuthenticate(const Challenge, Host, Protocol: string): string; override;

    property TraceInfo: string read FTraceInfo write FTraceInfo;
  end;

  TScSASLUserPassMechanism = class(TScSASLMechanism)
  protected
    FUsername: string;
    FPassword: string;
  public
    function IsReadyToStart: boolean; override;

    property Username: string read FUsername write FUsername;
    property Password: string read FPassword write FPassword;
  end;

  TScSASLPlain = class(TScSASLUserPassMechanism)
  protected
    FLogin: string;
  public
    class function ServiceName: string; override;

    function IsReadyToStart: boolean; override;
    function TryStartAuthenticate(const Host, Protocol: string; out InitialResponse: string): boolean; override;
    function StartAuthenticate(const Challenge, Host, Protocol: string): string; override;

    property Login: string read FLogin write FLogin;
  end;

  TScSASLLogin = class(TScSASLUserPassMechanism)
  public
    constructor Create; override;
    class function ServiceName: string; override;

    function TryStartAuthenticate(const Host, Protocol: string; out InitialResponse: string): boolean; override;
    function StartAuthenticate(const Challenge, Host, Protocol: string): string; override;
    function ContinueAuthenticate(const Response, Host, Protocol: string): string; override;
  end;

  TScSASLOTP = class(TScSASLUserPassMechanism)
  public
    constructor Create; override;
    class function ServiceName: string; override;

    function TryStartAuthenticate(const Host, Protocol: string; out InitialResponse: string): boolean; override;
    function StartAuthenticate(const Challenge, Host, Protocol: string): string; override;
    function ContinueAuthenticate(const Response, Host, Protocol: string): string; override;
  end;

  TScSASLSKey = class(TScSASLUserPassMechanism)
  public
    constructor Create; override;
    class function ServiceName: string; override;

    function IsReadyToStart: boolean; override;
    function TryStartAuthenticate(const Host, Protocol: string; out InitialResponse: string): boolean; override;
    function StartAuthenticate(const Challenge, Host, Protocol: string): string; override;
    function ContinueAuthenticate(const Response, Host, Protocol: string): string; override;
  end;

  TScSASLCRAMMD5 = class(TScSASLUserPassMechanism)
  public
    class function ServiceName: string; override;
    function StartAuthenticate(const Challenge, Host, Protocol: string): string; override;
  end;

  TScSASLCRAMSHA1 = class(TScSASLUserPassMechanism)
  public
    class function ServiceName: string; override;
    function StartAuthenticate(const Challenge, Host, Protocol: string): string; override;
  end;

  TScSASLItem = class(TCollectionItem)
  private
    FSASLMechanism: TScSASLMechanism;
    FIsOwner: boolean;
    procedure SetSASLMechanism(Value: TScSASLMechanism);

  protected
    function GetDisplayName: string; override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Init(const ServiceName: string);
  published
    property SASLMechanism: TScSASLMechanism read FSASLMechanism write SetSASLMechanism;
  end;

  TScSASLCollection = class(TOwnedCollection)
  private
    function GetItem(Index: integer): TScSASLItem;
    procedure SetItem(Index: integer; Value: TScSASLItem);

    procedure CheckIfEmpty;
    function SASLLogin(const Command, Host, Protocol: string;
      SASLMechanism: TScSASLMechanism; MIMECoder: TScCoder;
      const OkReplies, ContinueReplies: array of integer;
      Client: TScSMTPClient; IsInitialResponseSupport: boolean): boolean;
    procedure ParseCapabilities(Capabilities, SupportedList: TStrings;
      const AuthType: string = 'AUTH');

  public
    constructor Create(AOwner: TPersistent); reintroduce;

    procedure Login(const Command, Host, Protocol: string;
      const OkReplies, ContinueReplies: array of integer; Client: TScSMTPClient;
      Capabilities: TStrings; const AuthType: string = 'AUTH';
      IsInitialResponseSupport: boolean = True);

    property Items[Index: integer]: TScSASLItem read GetItem write SetItem; default;
  end;

  TScSMTPClientOptions = class(TPersistent)
  private
    FClient: TScSMTPClient;
    FBindAddress: string;
    FTCPKeepAlive: boolean;
    FIPVersion: TIPVersion;
    FSocketReceiveBufferSize: integer;
    FSocketSendBufferSize: integer;

    FHeloName: string;
    FMailAgent: string;
    FPipelined: boolean;
    FUseEhlo: boolean;
    FUseVerp: boolean;
    FVerpDelimiters: string;

    procedure SetBindAddress(const Value: string);
    procedure SetTCPKeepAlive(Value: boolean);
    procedure SetIPVersion(const Value: TIPVersion);
    procedure SetSocketReceiveBufferSize(Value: integer);
    procedure SetSocketSendBufferSize(Value: integer);

    procedure SetPipelined(Value: boolean);
    procedure SetUseEhlo(Value: boolean);

  protected
    procedure AssignTo(Dest: TPersistent); override;

  public
    constructor Create(Client: TScSMTPClient);
  published
    property BindAddress: string read FBindAddress write SetBindAddress; // useful on systems with more than one IP.
    property TCPKeepAlive: boolean read FTCPKeepAlive write SetTCPKeepAlive default Def_TCPKeepAlive;
    property IPVersion: TIPVersion read FIPVersion write SetIPVersion default DefValIPVersion;
    property SocketReceiveBufferSize: integer read FSocketReceiveBufferSize write SetSocketReceiveBufferSize default DEFAULT_SOCKET_BUFFER_SIZE;
    property SocketSendBufferSize: integer read FSocketSendBufferSize write SetSocketSendBufferSize default DEFAULT_SOCKET_BUFFER_SIZE;

    property HeloName: string read FHeloName write FHeloName;
    property MailAgent: string read FMailAgent write FMailAgent;
    property Pipelined: boolean read FPipelined write SetPipelined default Def_Pipelined;
    property UseEhlo: boolean read FUseEhlo write SetUseEhlo default Def_UseEhlo;
    property UseVerp: boolean read FUseVerp write FUseVerp default Def_UseVerp;
    property VerpDelimiters: string read FVerpDelimiters write FVerpDelimiters;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TScSMTPClient = class(TComponent)
  private
    FProxyOptions: TProxyOptions;
    FSSLOptions: TScSSLClientOptions;
    FOptions: TScSMTPClientOptions;
    FTLSMode: TScTLSMode;

    FHostName: string;
    FPort: integer;
    FTimeout: integer;
    FUsername: string;
    FPassword: string;

    // URI specifics
    FUri: string;
    FScheme: string;
    FQuery: string;
    FPortNo: string;
    FResource: string;
    FParameters: string;
    FPath: string;
    FFragment: string;

    FConnection: TScSecureConnection;
    FLastReply: TStringList;
    FLastReplyCode: integer;
    FFormattedReply: TStringList;
    FSPMidLines: boolean;
    FCapabilities: TStrings;
    FEncoding: Encoding;

    FAuthenticationType: TScSMTPAuthenticationType;
    FIsAuthenticated: boolean;
    FSASLMechanisms: TScSASLCollection;

    FBeforeConnect: TNotifyEvent;
    FAfterConnect: TNotifyEvent;
    FBeforeDisconnect: TNotifyEvent;
    FAfterDisconnect: TNotifyEvent;
    FOnSendCommand: TScSendLineEvent;
    FOnReadReply: TScReadLineEvent;
    FOnError: TScSMTPErrorEvent;
    FOnRecipientError: TScSMTPRecipientErrorEvent;

    procedure SetUri(const Value: string);
    procedure SetHostName(const Value: string);
  {$HPPEMIT '#ifdef SetPort'}
  {$HPPEMIT '#undef SetPort'}
  {$HPPEMIT '#endif'}
    procedure SetPort(const Value: integer);
    procedure SetUsername(const Value: string);
    procedure SetPassword(const Value: string);
    procedure SetTimeout(const Value: integer);
    procedure SetSocketReceiveBufferSize(Value: integer);
    procedure SetSocketSendBufferSize(Value: integer);
    procedure SetProxyOptions(Value: TProxyOptions);
    procedure SetSSLOptions(Value: TScSSLClientOptions);
    procedure SetOptions(Value: TScSMTPClientOptions);
    procedure SetTLSMode(const Value: TScTLSMode);
    procedure SetAuthenticationType(const Value: TScSMTPAuthenticationType);
    procedure SetSASLMechanisms(Value: TScSASLCollection);

    function GetUseUTF8: boolean;
    procedure SetUseUTF8(Value: boolean);

    procedure DoAfterDisconnect(Sender: TObject);
    procedure ReadReplyEvent(const Line: string);
    function GetIsSecure: boolean;
    procedure ReadSASLMechanisms(Reader: TReader);

    procedure UpdateUsernames;
    procedure UpdatePasswords;

  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Notification(Component: TComponent; Operation: TOperation); override;
    procedure DefineProperties(Filer: TFiler); override;

    procedure WriteStr(const Data: string; AEncoding: Encoding = nil);
    procedure WriteRFCStr(const Data: string; AEncoding: Encoding = nil);
    procedure WriteHeaderList(Strings: TScHeaderList);
    procedure WriteRFCStrings(Strings: TStrings; AEncoding: Encoding = nil);

    function GetActive: boolean;
    procedure CheckInactive;
    function CreateConnection(const AHost: string; APort: integer): TScSecureConnection;
    procedure GetResponse;
    procedure CheckResponse(const AllowedResponses: array of integer);

    function GetFormattedReply: TStringList;
    class function IsEndMarker(const Line: string): boolean;
    class function IsEndReply(const ReplyCode, Line: string): boolean;
    class function IsValidCode(const StrCode: string): boolean;

    procedure SendHello;
    procedure StartTLS;
    procedure InternalSend(Message: TScMailMessage; const From: string; Recipients: TScMailAddressList);
    procedure SendNoPipelined(Message: TScMailMessage; const From: string; Recipients: TScMailAddressList);
    procedure SendPipelined(Message: TScMailMessage; const From: string; Recipients: TScMailAddressList);
    function SkipErrorRecipient(const Address: string): boolean;
    procedure SendText(Strings: TStrings; EncoderClass: TScCoderClass; TextEncoding: Encoding);
    procedure SendAlternateView(Message: TScMailMessage; AlternateView: TScAlternateView);
    procedure SendAttachmentData(Attachment: TScDataAttachment; EncoderClass: TScCoderClass);
    procedure SendAttachment(Message: TScMailMessage; Attachment: TScDataAttachment);
    procedure SendMessage(Message: TScMailMessage; NeedEncode: boolean = True);
    procedure SendBody(Message: TScMailMessage);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Connect;
    procedure Disconnect;
    function Authenticate: boolean;
    procedure RaiseReplyError;

    function SendCmd(const Command: string; const AllowedResponses: array of integer): integer;

    procedure Send(const From, Recipients, Subject, Body: string); overload;
    procedure Send(Message: TScMailMessage); overload;
    procedure Send(Message: TScMailMessage; const From: string); overload;
    procedure Send(Message: TScMailMessage; const From: string; Recipients: TScMailAddressList); overload;
    procedure Send(Message: TScMailMessage; Recipients: TScMailAddressList); overload;

    procedure Expand(const UserName: string);
    function Verify(const UserName: string): string;

    property Active: boolean read GetActive;
    property IsSecure: boolean read GetIsSecure;
    property ReplyCode: integer read FLastReplyCode;
    property FormattedReply: TStringList read GetFormattedReply;

    property CEncoding: Encoding read FEncoding write FEncoding;
    property SASLMechanisms: TScSASLCollection read FSASLMechanisms write SetSASLMechanisms;

  published
    property HostName: string read FHostName write SetHostName;
    property Port: integer read FPort write SetPort default SMTP_DEFAULT_PORT;
    property Timeout: integer read FTimeout write SetTimeout default SMTP_DEFAULT_TIMEOUT;
    property Username: string read FUsername write SetUsername;
    property Password: string read FPassword write SetPassword;
    property ProxyOptions: TProxyOptions read FProxyOptions write SetProxyOptions;
    property SSLOptions: TScSSLClientOptions read FSSLOptions write SetSSLOptions;
    property Options: TScSMTPClientOptions read FOptions write SetOptions;
    property Uri: string read FUri write SetUri;
    property UseUTF8: boolean read GetUseUTF8 write SetUseUTF8 default False;

    property AuthenticationType: TScSMTPAuthenticationType read FAuthenticationType write SetAuthenticationType default Def_AuthType;
    property TLSMode: TScTLSMode read FTLSMode write SetTLSMode default Def_TLSMode;

    property BeforeConnect: TNotifyEvent read FBeforeConnect write FBeforeConnect;
    property AfterConnect: TNotifyEvent read FAfterConnect write FAfterConnect;
    property BeforeDisconnect: TNotifyEvent read FBeforeDisconnect write FBeforeDisconnect;
    property AfterDisconnect: TNotifyEvent read FAfterDisconnect write FAfterDisconnect;
    property OnSendCommand: TScSendLineEvent read FOnSendCommand write FOnSendCommand;
    property OnReadReply: TScReadLineEvent read FOnReadReply write FOnReadReply;
    property OnError: TScSMTPErrorEvent read FOnError write FOnError;
    property OnRecipientError: TScSMTPRecipientErrorEvent read FOnRecipientError write FOnRecipientError;
  end;

  EScSMTPError = class(EScError)
  protected
    FSMTPErrorCode: integer;
  public
    constructor Create(ASMTPErrorCode: integer; const ErrorMessage: string);
    property SMTPErrorCode: integer read FSMTPErrorCode;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  WinSock, {$IFNDEF FPC}Windows,{$ENDIF}
{$ENDIF}
{$IFDEF POSIX}
  Posix.NetinetIn, Posix.SysSocket,
{$ENDIF}
{$IFDEF UNIX}
  Sockets,
{$ENDIF}
{$IFDEF VER17P}
  Types,
{$ENDIF}
  Math, StrUtils,
  ScFunctions, ScDECUtil, ScHashAlgorithm, ScHash, ScHMAC;

const
  SmtpScheme = 'smtp:';

{ EScSMTPError }

constructor EScSMTPError.Create(ASMTPErrorCode: integer; const ErrorMessage: string);
begin
  inherited CreateFmt(SSMTPServerError, [ErrorMessage], seSMTPServerError);
  FSMTPErrorCode := ASMTPErrorCode;
end;

{ TScSASLMechanism }

constructor TScSASLMechanism.Create;
begin
  inherited Create;
end;

class function TScSASLMechanism.ServiceName: string;
begin
  Result := '';
end;

function TScSASLMechanism.IsReadyToStart;
begin
  Result := True;
end;

function TScSASLMechanism.TryStartAuthenticate(const Host, Protocol: string; out InitialResponse: string): boolean;
begin
  InitialResponse := '';
  Result := False;
end;

function TScSASLMechanism.ContinueAuthenticate(const Response, Host, Protocol: string): string;
begin
  Result := '';
end;

{ TScSASLAnonymous }

class function TScSASLAnonymous.ServiceName: string;
begin
  Result := 'ANONYMOUS';
end;

function TScSASLAnonymous.IsReadyToStart: boolean;
begin
  Result := TraceInfo <> '';
end;

function TScSASLAnonymous.TryStartAuthenticate(const Host, Protocol: string; out InitialResponse: string): boolean;
begin
  InitialResponse := TraceInfo;
  Result := True;
end;

function TScSASLAnonymous.StartAuthenticate(const Challenge, Host, Protocol: string): string;
begin
  Result := TraceInfo;
end;

{ TScSASLUserPassMechanism }

function TScSASLUserPassMechanism.IsReadyToStart;
begin
  Result := Username <> '';
end;

{ TScSASLPlain }

class function TScSASLPlain.ServiceName: string;
begin
  Result := 'PLAIN';
end;

function TScSASLPlain.IsReadyToStart: boolean;
begin
  Result := inherited IsReadyToStart;
  if not Result then
    Result := Login <> '';
end;

function TScSASLPlain.TryStartAuthenticate(const Host, Protocol: string; out InitialResponse: string): boolean;
var
  UserStr: string;
begin
  UserStr := Username;
  if UserStr = '' then
    UserStr := Login;

  InitialResponse := Login + #0 + UserStr + #0 + Password;
  Result := True;
end;

function TScSASLPlain.StartAuthenticate(const Challenge, Host, Protocol: string): string;
begin
  TryStartAuthenticate(Host, Protocol, Result);
end;

{ TScSASLLogin }

constructor TScSASLLogin.Create;
begin
  inherited;

  FSecurityLevel := 20;
end;

class function TScSASLLogin.ServiceName: string;
begin
  Result := 'LOGIN';
end;

function TScSASLLogin.TryStartAuthenticate(const Host, Protocol: string; out InitialResponse: string): boolean;
begin
  InitialResponse := Username;
  Result := True;
end;

function TScSASLLogin.StartAuthenticate(const Challenge, Host, Protocol: string): string;
begin
  Result := Username;
end;

function TScSASLLogin.ContinueAuthenticate(const Response, Host, Protocol: string): string;
begin
  Result := Password;
end;

{ TScSASLOTP }

constructor TScSASLOTP.Create;
begin
  inherited;

  FSecurityLevel := 100;
end;

class function TScSASLOTP.ServiceName: string;
begin
  Result := 'OTP';
end;

function TScSASLOTP.TryStartAuthenticate(const Host, Protocol: string; out InitialResponse: string): boolean;
begin
  InitialResponse := Username;
  Result := True;
end;

function TScSASLOTP.StartAuthenticate(const Challenge, Host, Protocol: string): string;
begin
  Result := Username;
end;

function KeyToSixWord(const Key: Int64): string;

  function GetBits(Src: Int64; Start, Count: integer): word;
  begin
    Result := (Src shl Start) shr (64 - Count);
  end;
var
  Parity, i: integer;
begin
  for i := 0 to 4 do
    Result := Result + OTP_DICTIONARY[GetBits(Key, i * 11, 11)] + ' ';

  Parity := 0;
  for i := 0 to 32 do
    Inc(Parity, GetBits(Key, i * 2, 2));

  Result := Result + OTP_DICTIONARY[GetBits(Key, 55, 11) + (Parity and 3)];
end;

function GenerateMD5Key(const Seed, Password: string; HashAlgClass: THashAlgorithmClass; Iters: integer): Int64;

  function Hash4ToInt64(const Hash: TBytes): Int64;
  var
    HashInt: array[0..3] of cardinal;
  begin
    HashInt[0] := GetIntLE(Hash, 0);
    HashInt[1] := GetIntLE(Hash, 4);
    HashInt[2] := GetIntLE(Hash, 8);
    HashInt[3] := GetIntLE(Hash, 12);
    Result := (Int64(HashInt[0] xor HashInt[2]) shl 32) or (HashInt[1] xor HashInt[3]);
  end;

var
  HashAlg: THashAlgorithm;
  TmpBuffer: TBytes;
  H64: Int64;
  i: integer;
begin
  HashAlg := HashAlgClass.Create;
  try
    SetLength(TmpBuffer, 8);
    H64 := Hash4ToInt64(HashAlg.ComputeHash(Encoding.Default.GetBytes(LowerCase(Seed) + Password)));

    for i := 1 to Iters do begin
      TmpBuffer[0] := byte(H64 shr 32);
      TmpBuffer[1] := byte(H64 shr 40);
      TmpBuffer[2] := byte(H64 shr 48);
      TmpBuffer[3] := byte(H64 shr 56);
      TmpBuffer[4] := byte(H64);
      TmpBuffer[5] := byte(H64 shr 8);
      TmpBuffer[6] := byte(H64 shr 16);
      TmpBuffer[7] := byte(H64 shr 24);

      H64 := Hash4ToInt64(HashAlg.ComputeHash(TmpBuffer));
    end;
  finally
    HashAlg.Free;
  end;

  Result := SwapInt64(H64);
end;

function GenerateSHA1Key(const Seed, Password: string; Iters: integer): Int64;

  function Hash5ToInt64(const Hash: TBytes): Int64;
  var
    HashInt: array[0..4] of cardinal;
  begin
    HashInt[0] := GetIntLE(Hash, 0);
    HashInt[1] := GetIntLE(Hash, 4);
    HashInt[2] := GetIntLE(Hash, 8);
    HashInt[3] := GetIntLE(Hash, 12);
    HashInt[4] := GetIntLE(Hash, 16);
    Result := (Int64(HashInt[0] xor HashInt[2] xor HashInt[4]) shl 32) or (HashInt[1] xor HashInt[3]);
  end;

var
  SHA1: THash_SHA1;
  TmpBuffer: TBytes;
  H64: Int64;
  i: integer;
begin
  SHA1 := THash_SHA1.Create;
  try
    SetLength(TmpBuffer, 8);
    H64 := Hash5ToInt64(SHA1.ComputeHash(Encoding.Default.GetBytes(LowerCase(Seed) + Password)));

    for i := 1 to Iters do begin
      TmpBuffer[0] := byte(H64 shr 56);
      TmpBuffer[1] := byte(H64 shr 48);
      TmpBuffer[2] := byte(H64 shr 40);
      TmpBuffer[3] := byte(H64 shr 32);
      TmpBuffer[4] := byte(H64 shr 24);
      TmpBuffer[5] := byte(H64 shr 16);
      TmpBuffer[6] := byte(H64 shr 8);
      TmpBuffer[7] := byte(H64);

      H64 := Hash5ToInt64(SHA1.ComputeHash(TmpBuffer));
    end;
  finally
    SHA1.Free;
  end;

  Result := H64;
end;

function GenerateSixWordKey(const Response, Password: string; out Key: string): boolean;
var
  OtpPos: integer;
  OtpStr, Method, Seed: string;
  Iters: integer;
begin
  OtpPos := Pos('otp-', Response);
  if OtpPos > 0 then begin
    Inc(OtpPos, 4); // remove 'otp-'
    OtpStr := Copy(Response, OtpPos, MaxInt);
    Method := ExtractFirstWord(OtpStr, ' ' );
    Iters := StrToInt(ExtractFirstWord(OtpStr, ' '));
    Seed := ExtractFirstWord(OtpStr, ' ');

    if Method = 'sha1' then
      Key := KeyToSixWord(GenerateSHA1Key(Seed, Password, Iters))
    else
    if Method = 'md5' then
      Key := KeyToSixWord(GenerateMD5Key(Seed, Password, THash_MD5, Iters))
    else
      raise EScError.Create(seUnknownOTPMethod);

    Result := True;
  end
  else begin
    Key := '';
    Result := False;
  end;
end;

function TScSASLOTP.ContinueAuthenticate(const Response, Host, Protocol: string): string;
var
  Key: string;
begin
  if GenerateSixWordKey(Response, Password, Key) then
    Result := 'word:' + Key
  else
    Result := '';
end;

{ TScSASLSKey }

constructor TScSASLSKey.Create;
begin
  inherited;

  FSecurityLevel := 90;
end;

class function TScSASLSKey.ServiceName: string;
begin
  Result := 'SKEY';
end;

function TScSASLSKey.IsReadyToStart: boolean;
begin
  Result := True;
end;

function TScSASLSKey.TryStartAuthenticate(const Host, Protocol: string; out InitialResponse: string): boolean;
begin
  InitialResponse := Username;
  Result := True;
end;

function TScSASLSKey.StartAuthenticate(const Challenge, Host, Protocol: string): string;
begin
  Result := Username;
end;

function TScSASLSKey.ContinueAuthenticate(const Response, Host, Protocol: string): string;
var
  RespStr, Seed: string;
  Iters: cardinal;
begin
  RespStr := Trim(Response);
  Iters := StrToIntDef(ExtractFirstWord(RespStr, ' ' ), 0);
  Seed := ExtractFirstWord(RespStr, ' ');
  Result := KeyToSixWord(GenerateMD5Key(Seed, Password, THash_MD4, Iters));
end;

{ TIdSASLCRAMMD5 }

class function TScSASLCRAMMD5.ServiceName: string;
begin
  Result := 'CRAM-MD5';
end;

function TScSASLCRAMMD5.StartAuthenticate(const Challenge, Host, Protocol: string): string;
var
  csp: THMAC;
  Buf: TBytes;
begin
  if Length(Challenge) = 0 then begin
    Result := '';
    Exit;
  end;

  csp := THMAC.Create(THash_MD5, Encoding.Default.GetBytes(Password));
  try
    SetLength(Buf, 0);
    Buf := Encoding.Default.GetBytes(Challenge);
    Buf := csp.ComputeHash(Buf);
    Result := LowerCase(BytesToHexStr(Buf));
  finally
    csp.Free;
  end;

  Result := Username + ' ' + Result;
end;

{ TScSASLCRAMSHA1 }

class function TScSASLCRAMSHA1.ServiceName: string;
begin
  Result := 'CRAM-SHA1';
end;

function TScSASLCRAMSHA1.StartAuthenticate(const Challenge, Host, Protocol: string): string;
var
  csp: THMAC;
  Buf: TBytes;
begin
  if Length(Challenge) = 0 then begin
    Result := '';
    Exit;
  end;

  csp := THMAC.Create(THash_SHA1, Encoding.Default.GetBytes(Password));
  try
    SetLength(Buf, 0);
    Buf := Encoding.Default.GetBytes(Challenge);
    Buf := csp.ComputeHash(Buf);
    Result := LowerCase(BytesToHexStr(Buf));
  finally
    csp.Free;
  end;

  Result := Username + ' ' + Result;
end;

{ TScSASLItem }

destructor TScSASLItem.Destroy;
begin
  if FIsOwner then
    FSASLMechanism.Free;

  inherited;
end;

procedure TScSASLItem.Init(const ServiceName: string);
begin
  if FIsOwner then
    FreeAndNil(FSASLMechanism);

  if ServiceName = 'ANONYMOUS' then
    FSASLMechanism := TScSASLAnonymous.Create
  else
  if ServiceName = 'PLAIN' then
    FSASLMechanism := TScSASLPlain.Create
  else
  if ServiceName = 'LOGIN' then
    FSASLMechanism := TScSASLLogin.Create
  else
  if ServiceName = 'OTP' then
    FSASLMechanism := TScSASLOTP.Create
  else
  if ServiceName = 'SKEY' then
    FSASLMechanism := TScSASLSKey.Create
  else
  if ServiceName = 'CRAM-MD5' then
    FSASLMechanism := TScSASLCRAMMD5.Create
  else
  if ServiceName = 'CRAM-SHA1' then
    FSASLMechanism := TScSASLCRAMSHA1.Create
  else
    FSASLMechanism := TScSASLLogin.Create;

  FIsOwner := True;
end;

procedure TScSASLItem.Assign(Source: TPersistent);
begin
  if Source is TScSASLItem then
    SASLMechanism := TScSASLItem(Source).SASLMechanism
  else
    inherited Assign(Source);
end;

function TScSASLItem.GetDisplayName: string;
begin
  if FSASLMechanism <> nil then
    Result := FSASLMechanism.ServiceName
  else
    Result := inherited GetDisplayName;
end;

procedure TScSASLItem.SetSASLMechanism(Value: TScSASLMechanism);
begin
  if FSASLMechanism <> Value then begin
    if FIsOwner then
      FreeAndNil(FSASLMechanism);

    FSASLMechanism := Value;
    FIsOwner := False;
  end;
end;

{ TScSASLCollection }

constructor TScSASLCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TScSASLItem);
end;

function TScSASLCollection.GetItem(Index: integer): TScSASLItem;
begin
  Result := TScSASLItem(inherited Items[Index]);
end;

procedure TScSASLCollection.SetItem(Index: integer; Value: TScSASLItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TScSASLCollection.CheckIfEmpty;
var
  i: integer;
begin
  for i := 0 to Count - 1 do begin
    if Items[i].SASLMechanism <> nil then
      Exit;
  end;

  raise EScError.Create(seSASLMechanismRequired);
end;

function TScSASLCollection.SASLLogin(const Command, Host, Protocol: string;
  SASLMechanism: TScSASLMechanism; MIMECoder: TScCoder;
  const OkReplies, ContinueReplies: array of integer;
  Client: TScSMTPClient; IsInitialResponseSupport: boolean): boolean;

  function CheckResponse(ReplyCode: integer; const OkResponses, ContinueResponses: array of integer): boolean;
  var
    i: integer;
  begin
    Result := True;
    for i := Low(OkResponses) to High(OkResponses) do begin
      if ReplyCode = OkResponses[i] then
        Exit;
    end;

    for i := Low(ContinueResponses) to High(ContinueResponses) do begin
      if ReplyCode = ContinueResponses[i] then
        Exit;
    end;

    Result := False;
  end;

var
  InitialResponseStr, Response: string;
  IsAuthStarted: boolean;
begin
  Result := False;
  IsAuthStarted := False;

  // IMAP - RFC 4959
  // POP3 - RFC 2449, RFC 5034
  if IsInitialResponseSupport then begin
    if SASLMechanism.TryStartAuthenticate(Host, Protocol, InitialResponseStr) then begin
      Client.SendCmd(Command + ' ' + SASLMechanism.ServiceName + ' ' + MIMECoder.Encode(InitialResponseStr), []);
      if not CheckResponse(Client.ReplyCode, OkReplies, ContinueReplies) then
        Exit;

      IsAuthStarted := True;
    end;
  end;

  if not IsAuthStarted then begin
    Client.SendCmd(Command + ' ' + SASLMechanism.ServiceName, []);
    if not CheckResponse(Client.ReplyCode, OkReplies, ContinueReplies) then
      Exit;
  end;

  if CheckResponse(Client.ReplyCode, OkReplies, []) then begin
    Result := True;
    Exit;
  end;

  // Continue
  if not IsAuthStarted then begin
    Response := MIMECoder.Decode(TrimRight(Client.FLastReply.Text));
    Response := SASLMechanism.StartAuthenticate(Response, Host, Protocol);
    Client.SendCmd(MIMECoder.Encode(Response), []);

    if not CheckResponse(Client.ReplyCode, OkReplies, ContinueReplies) then
      Exit;
  end;

  while CheckResponse(Client.ReplyCode, [], ContinueReplies) do begin
    Response := MIMECoder.Decode(TrimRight(Client.FLastReply.Text));
    Response := SASLMechanism.ContinueAuthenticate(Response, Host, Protocol);
    Client.SendCmd(MIMECoder.Encode(Response), []);

    if not CheckResponse(Client.ReplyCode, OkReplies, ContinueReplies) then
      Exit;
  end;

  Result := CheckResponse(Client.ReplyCode, OkReplies, []);
end;

procedure TScSASLCollection.ParseCapabilities(Capabilities, SupportedList: TStrings;
  const AuthType: string = 'AUTH');
var
  SASLType, Str: string;
  AuthLen: integer;
  i: integer;
begin
  if (Capabilities = nil) or (SupportedList = nil) then
    Exit;

  SupportedList.BeginUpdate;
  try
    AuthLen := Length(AuthType);

    for i := 0 to Capabilities.Count - 1 do begin
      Str := Capabilities[i];

      if not AnsiSameText(Copy(Str, 1, AuthLen), AuthType) then
        continue;

      if (Length(Str) > AuthLen) and ((Str[AuthLen + 1] = ' ') or (Str[AuthLen + 1] = '=')) then begin
        Str := UpperCase(Copy(Str, AuthLen + 1, MaxInt)); // remove AUTH
        Str := StringReplace(Str, '=', ' ', [rfReplaceAll]);
        Str := Trim(Str);

        while Length(Str) > 0 do begin
          SASLType := ExtractFirstWord(Str, ' ');
          if (SASLType <> '') and (SupportedList.IndexOf(SASLType) = -1) then
            SupportedList.Add(SASLType);
        end;
      end;
    end;
  finally
    SupportedList.EndUpdate;
  end;
end;

procedure TScSASLCollection.Login(const Command, Host, Protocol: string;
  const OkReplies, ContinueReplies: array of integer; Client: TScSMTPClient;
  Capabilities: TStrings; const AuthType: string = 'AUTH';
  IsInitialResponseSupport: boolean = True);
var
  MIMECoder: TScCoder;
  SASLList: TCRList;
  SupportedList: TStrings;
  SASLMechanism: TScSASLMechanism;
  ReplyError: TStringList;
  ErrorCode: integer;
  i: integer;
begin
  CheckIfEmpty;

  SASLList := TCRList.Create;
  try
    SupportedList := TStringList.Create;
    try
      ParseCapabilities(Capabilities, SupportedList, AuthType);

      for i := Count - 1 downto 0 do begin
        SASLMechanism := Items[i].SASLMechanism;
        if SASLMechanism <> nil then begin
          if SupportedList.IndexOf(SASLMechanism.ServiceName) = -1 then
            Continue;

          if SASLList.IndexOf(SASLMechanism) = -1 then
            SASLList.Add(SASLMechanism);
        end;
      end;
    finally
      SupportedList.Free;
    end;

    if SASLList.Count = 0 then
      raise EScError.Create(seSASLNotSupported);

    MIMECoder := nil;
    ErrorCode := 0;
    try
      ReplyError := nil;
      try
        for i := 0 to SASLList.Count - 1 do begin
          SASLMechanism := TScSASLMechanism(SASLList.Items[i]);
          if not SASLMechanism.IsReadyToStart then
            Continue;

          if MIMECoder = nil then
            MIMECoder := TScBase64Coder.Create;

          if SASLLogin(Command, Host, Protocol, SASLMechanism, MIMECoder, OkReplies, ContinueReplies, Client, IsInitialResponseSupport) then
            Exit;

          if not Assigned(ReplyError) then begin
            ReplyError := TStringList.Create;
            ReplyError.Assign(Client.FLastReply);
            ErrorCode := Client.ReplyCode;
          end;
        end;

        if Assigned(ReplyError) then
          raise EScSMTPError.Create(ErrorCode, ReplyError.Text)
        else
          raise EScError.Create(seSASLNotReady);
      finally
        ReplyError.Free;
      end;
    finally
      MIMECoder.Free;
    end;
  finally
    SASLList.Free;
  end;
end;

{ TScSMTPClientOptions }

constructor TScSMTPClientOptions.Create(Client: TScSMTPClient);
begin
  inherited Create;

  Assert(Client <> nil);
  FClient := Client;
  FTCPKeepAlive := Def_TCPKeepAlive;
  FIPVersion := DefValIPVersion;
  FSocketReceiveBufferSize := DEFAULT_SOCKET_BUFFER_SIZE;
  FSocketSendBufferSize := DEFAULT_SOCKET_BUFFER_SIZE;

  FPipelined := Def_Pipelined;
  FUseEhlo := Def_UseEhlo;
  FUseVerp := Def_UseVerp;
end;

procedure TScSMTPClientOptions.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScSMTPClientOptions) then begin
    TScSMTPClientOptions(Dest).FClient.CheckInactive;
    TScSMTPClientOptions(Dest).FBindAddress := FBindAddress;
    TScSMTPClientOptions(Dest).FTCPKeepAlive := FTCPKeepAlive;
    TScSMTPClientOptions(Dest).FIPVersion := FIPVersion;
    TScSMTPClientOptions(Dest).FSocketReceiveBufferSize := FSocketReceiveBufferSize;
    TScSMTPClientOptions(Dest).FSocketSendBufferSize := FSocketSendBufferSize;

    TScSMTPClientOptions(Dest).FHeloName:= FHeloName;
    TScSMTPClientOptions(Dest).FMailAgent := FMailAgent;
    TScSMTPClientOptions(Dest).FPipelined := FPipelined;
    TScSMTPClientOptions(Dest).FUseEhlo := FUseEhlo;
    TScSMTPClientOptions(Dest).FUseVerp := FUseVerp;
    TScSMTPClientOptions(Dest).FVerpDelimiters := FVerpDelimiters;
  end
  else
    inherited;
end;

procedure TScSMTPClientOptions.SetBindAddress(const Value: string);
begin
  if Value <> FBindAddress then begin
    FClient.CheckInactive;
    FBindAddress := Trim(Value);
  end;
end;

procedure TScSMTPClientOptions.SetTCPKeepAlive(Value: boolean);
begin
  if Value <> FTCPKeepAlive then begin
    FClient.CheckInactive;
    FTCPKeepAlive := Value;
  end;
end;

procedure TScSMTPClientOptions.SetIPVersion(const Value: TIPVersion);
begin
  if Value <> FIPVersion then begin
    FClient.CheckInactive;
    FIPVersion := Value;
  end;
end;

procedure TScSMTPClientOptions.SetSocketReceiveBufferSize(Value: integer);
begin
  if Value <> FSocketReceiveBufferSize then begin
    FSocketReceiveBufferSize := Value;
    FClient.SetSocketReceiveBufferSize(Value);
  end;
end;

procedure TScSMTPClientOptions.SetSocketSendBufferSize(Value: integer);
begin
  if Value <> FSocketSendBufferSize then begin
    FSocketSendBufferSize := Value;
    FClient.SetSocketSendBufferSize(Value);
  end;
end;

procedure TScSMTPClientOptions.SetPipelined(Value: boolean);
begin
  FPipelined := Value;
  if Value then
    FUseEhlo := True;
end;

procedure TScSMTPClientOptions.SetUseEhlo(Value: boolean);
begin
  FUseEhlo := Value;

  if not Value then begin
    FClient.FAuthenticationType := satDefault;
    if FClient.FTLSMode in [tmRequireExplicitTLS, tmAllowExplicitTLS] then begin
      FClient.FTLSMode := Def_TLSMode;
      FPipelined := False;
    end;
  end;
end;

{ TScSMTPClient }

constructor TScSMTPClient.Create(AOwner: TComponent);
begin
  inherited;

  FProxyOptions := TProxyOptions.Create;
  FSSLOptions := TScSSLClientOptions.Create(Self);
  FOptions := TScSMTPClientOptions.Create(Self);
  FCapabilities := TStringList.Create;
  FLastReply := TStringList.Create;
  FFormattedReply := TStringList.Create;
  FSASLMechanisms := TScSASLCollection.Create(Self);

  FPort := SMTP_DEFAULT_PORT;
  FTimeout := SMTP_DEFAULT_TIMEOUT;
  FTLSMode := Def_TLSMode;
  FAuthenticationType := Def_AuthType;
end;

destructor TScSMTPClient.Destroy;
begin
  Disconnect;

  FProxyOptions.Free;
  FSSLOptions.Free;
  FOptions.Free;
  FCapabilities.Free;
  FLastReply.Free;
  FFormattedReply.Free;
  FSASLMechanisms.Free;

  inherited;
end;

procedure TScSMTPClient.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScSMTPClient) then begin
    TScSMTPClient(Dest).CheckInactive;
    TScSMTPClient(Dest).FProxyOptions.Assign(FProxyOptions);
    TScSMTPClient(Dest).FSSLOptions.Assign(FSSLOptions);
    TScSMTPClient(Dest).FOptions.Assign(FOptions);
    TScSMTPClient(Dest).FSASLMechanisms.Assign(FSASLMechanisms);
    TScSMTPClient(Dest).FHostName := FHostName;
    TScSMTPClient(Dest).FPort := FPort;
    TScSMTPClient(Dest).FTimeout := FTimeout;
    TScSMTPClient(Dest).FUsername := FUsername;
    TScSMTPClient(Dest).FPassword := FPassword;
    TScSMTPClient(Dest).FTLSMode := FTLSMode;
    TScSMTPClient(Dest).FAuthenticationType := FAuthenticationType;
  end
  else
    inherited;
end;

procedure TScSMTPClient.Notification(Component: TComponent; Operation: TOperation);
begin
  if (FSSLOptions <> nil) and (Component = FSSLOptions.Storage) and (Operation = opRemove) then
    FSSLOptions.Storage := nil;

  inherited;
end;

procedure TScSMTPClient.DefineProperties(Filer: TFiler);
begin
  inherited;

  Filer.DefineProperty('SASLMechanisms', ReadSASLMechanisms, nil, False);
end;

procedure TScSMTPClient.ReadSASLMechanisms(Reader: TReader);
begin
  Reader.ReadValue;
  Reader.ReadCollection(FSASLMechanisms);
end;

function TScSMTPClient.CreateConnection(const AHost: string; APort: integer): TScSecureConnection;
var
  ConnectionParameters: TScSecureConnectionParameters;
  opt: integer;
begin
  if (AHost = '') or (APort = 0) then
    raise EScError.CreateFmt(SInvalidInputArg, ['Host, Port'], seInvalidInputArg);

  ConnectionParameters := TScSecureConnectionParameters.Create;
  try
    ConnectionParameters.BindAddress := Options.BindAddress;
    ConnectionParameters.Hostname := AHost;
    ConnectionParameters.Port := APort;
    ConnectionParameters.IPVersion := Options.IPVersion;
    ConnectionParameters.Timeout := FTimeout;
    ConnectionParameters.IsSecure := FTLSMode <> tmDisableTLS;
    ConnectionParameters.ProxyOptions.Assign(FProxyOptions);
    ConnectionParameters.SSLOptions.Assign(FSSLOptions);
    ConnectionParameters.SSLOptions.DisableInsertEmptyFragment := True;
    ConnectionParameters.SSLOptions.UseSecureSessionResumption := True;

    Result := TScSecureConnection.Create;
    try
      Result.CreateVio(ConnectionParameters);
      Result.Connect;

      if Options.TCPKeepAlive then
        opt := 1
      else
        opt := 0;
      Result.SetSocketOption(SOL_SOCKET, SO_KEEPALIVE, opt);

      if Options.SocketReceiveBufferSize > 0 then
        Result.SetSocketOption(SOL_SOCKET, SO_RCVBUF, Options.SocketReceiveBufferSize);
      if Options.SocketSendBufferSize > 0 then
        Result.SetSocketOption(SOL_SOCKET, SO_SNDBUF, Options.SocketSendBufferSize);

      Result.AfterDisconnect := DoAfterDisconnect;
    except
      Result.Free;
      raise;
    end;
  finally
    ConnectionParameters.Free;
  end;
end;

procedure TScSMTPClient.Connect;
var
  CurHostName: string;
begin
  if Active then
    raise EScError.Create(seSMTPClientActive);

  if Assigned(BeforeConnect) then
    BeforeConnect(Self);

  if FHostName = '' then
    CurHostName := LOCAL_HOST
  else
    CurHostName := FHostName;

  if FTLSMode = tmImplicitTLS then begin
    if FPort = SMTP_DEFAULT_PORT then
      FPort := SMTPS_DEFAULT_PORT;
  end
  else
  if FTLSMode = tmDisableTLS then begin
    if FPort = SMTPS_DEFAULT_PORT then
      FPort := SMTP_DEFAULT_PORT;
  end;

  if FPort = 0 then
    FPort := SMTP_DEFAULT_PORT;

  FIsAuthenticated := False;
  FConnection.Release;
  FConnection := nil;
  FConnection := CreateConnection(CurHostName, FPort);

  try
    if FTLSMode = tmImplicitTLS then
      FConnection.IsSecure := True;

    GetResponse;
    CheckResponse([220]);

    SendHello;
  except
    FConnection.Release;
    FConnection := nil;
    raise;
  end;

  if Assigned(AfterConnect) then
    AfterConnect(Self);
end;

procedure TScSMTPClient.Disconnect;
begin
  if Assigned(BeforeDisconnect) then
    BeforeDisconnect(Self);

  if Active then
    try
      SendCmd('QUIT', [221]);
    except
    end;

  FConnection.Release;
  FConnection := nil;

  FIsAuthenticated := False;
end;

function TScSMTPClient.GetActive: boolean;
begin
  Result := (FConnection <> nil) and FConnection.CheckIsConnected;
end;

function TScSMTPClient.GetIsSecure: boolean;
begin
  Result := (FConnection <> nil) and FConnection.IsSecure;
end;

procedure TScSMTPClient.CheckInactive;
begin
  if Active then
    raise EScError.Create(seClientOpened);
end;

procedure TScSMTPClient.GetResponse;
var
  ReplyLine, ReplyDesc, ReplyCode: string;
begin
  FLastReply.Clear;
  FLastReplyCode := 0;
  FSPMidLines := False;

  ReplyLine := FConnection.ReadLine(FEncoding);
  ReadReplyEvent(ReplyLine);

  ReplyCode := Trim(Copy(ReplyLine, 1, 4));
  if (Length(ReplyCode) >= 4) and (ReplyCode[4] = '-') then
    SetLength(ReplyCode, 3);
  FLastReply.Add(Copy(ReplyLine, Length(ReplyCode) + 2, MaxInt));

  if (Length(ReplyLine) >= 4) and (ReplyLine[4] = '-') then begin
    repeat
      ReplyLine := FConnection.ReadLine(FEncoding);
      ReadReplyEvent(ReplyLine);

      if AnsiSameText(LeftStr(ReplyLine, 3), ReplyCode) then
        ReplyDesc := Copy(ReplyLine, 5{xyz }, MaxInt)
      else begin
        if (Length(ReplyLine) >= 1) and (ReplyLine[1] = ' ') then
          FSPMidLines := True;
        ReplyDesc := TrimLeft(ReplyLine);
      end;
      FLastReply.Add(ReplyDesc);
    until IsEndReply(ReplyCode, ReplyLine);
  end;

  if not IsValidCode(ReplyCode) then
    raise EScError.CreateFmt(SInvalidReplyCode, [ReplyCode], seInvalidReplyCode);
  FLastReplyCode := StrToIntDef(ReplyCode, 0);
end;

procedure TScSMTPClient.CheckResponse(const AllowedResponses: array of integer);
var
  Fail: boolean;
  ErrorMessage: string;
  i: integer;
begin
  for i := Low(AllowedResponses) to High(AllowedResponses) do begin
    if FLastReplyCode = AllowedResponses[i] then
      Exit;
  end;

  // SMTP server can return 421 when it is going to shut down the connection
  if (Length(AllowedResponses) > 0) or (FLastReplyCode = 421) then begin
    if FLastReplyCode = 421 then
      try
        Disconnect;
      except
      end;

    Fail := True;
    ErrorMessage := FLastReply.Text;
    if Assigned(OnError) then
      OnError(Self, FLastReplyCode, ErrorMessage, Fail);
    if Fail then
      raise EScSMTPError.Create(FLastReplyCode, ErrorMessage);
  end;
end;

class function TScSMTPClient.IsEndMarker(const Line: string): boolean;
begin
  Result := (Length(Line) >= 3) and IsNumeric(Copy(Line, 1, 3));
  if Result then
    Result := (Length(Line) = 3) or ((Length(Line) >= 4) and (Line[4] = ' '));
end;

class function TScSMTPClient.IsEndReply(const ReplyCode, Line: string): boolean;
begin
  Result := IsEndMarker(Line) and AnsiSameText(LeftStr(Line, 3), ReplyCode);
end;

class function TScSMTPClient.IsValidCode(const StrCode: string): boolean;
var
  Code: integer;
begin
  if TryStrToInt(StrCode, Code) then
    Result := (Code >= 100) and (Code <= 599)
  else
    Result := False;
end;

function TScSMTPClient.GetFormattedReply: TStringList;
var
  StrCode: string;
  i: integer;
begin
  FFormattedReply.Clear;

  if FLastReplyCode > 0 then begin
    StrCode := IntToStr(FLastReplyCode);
    if FLastReply.Count > 0 then begin
      for i := 0 to FLastReply.Count - 2 do begin
        if (i = 0) or not FSPMidLines then
          FFormattedReply.Add(StrCode + '-' + FLastReply[i])
        else
          FFormattedReply.Add(' ' + FLastReply[i]);
      end;

      FFormattedReply.Add(StrCode + ' ' + FLastReply[FLastReply.Count - 1]);
    end
    else
      FFormattedReply.Add(StrCode + ' ');
  end
  else
  if FLastReply.Count > 0 then
    FFormattedReply.AddStrings(FLastReply);

  Result := FFormattedReply;
end;

procedure TScSMTPClient.RaiseReplyError;
begin
  raise EScSMTPError.Create(FLastReplyCode, FLastReply.Text);
end;

procedure TScSMTPClient.WriteStr(const Data: string; AEncoding: Encoding = nil);
begin
  if AEncoding <> nil then
    FConnection.Write(AEncoding.GetBytes(Data))
  else
  if FEncoding <> nil then
    FConnection.Write(FEncoding.GetBytes(Data))
  else
    FConnection.Write(Encoding.Default.GetBytes(Data));

  if Assigned(FOnSendCommand) then
    FOnSendCommand(Self, Data);
end;

procedure TScSMTPClient.WriteRFCStr(const Data: string; AEncoding: Encoding = nil);
begin
  if Data = '' then
    WriteStr(CRLF, AEncoding)
  else
  if Data[1] = '.' then
    WriteStr('.' + Data + CRLF, AEncoding)
  else
    WriteStr(Data + CRLF, AEncoding);
end;

procedure TScSMTPClient.WriteHeaderList(Strings: TScHeaderList);
var
  Str, Line: string;
  i: integer;
begin
  for i := 0 to Strings.Count - 1 do begin
    Str := WrapText(Strings[i], CRLF+' ', [#9, #32, ','], 78);

    while Str <> '' do begin
      Line := TrimRight(ExtractFirstWord(Str, CRLF));
      WriteStr(Line + CRLF);
    end;
  end;
end;

procedure TScSMTPClient.WriteRFCStrings(Strings: TStrings; AEncoding: Encoding = nil);
var
  i: integer;
begin
  for i := 0 to Strings.Count - 1 do
    WriteRFCStr(Strings[i], AEncoding);
end;

function TScSMTPClient.SendCmd(const Command: string; const AllowedResponses: array of integer): integer;
begin
  if not Active then
    raise EScError.Create(seSMTPClientNotConnected);

  WriteStr(Command + CRLF);
  GetResponse;
  CheckResponse(AllowedResponses);
  Result := FLastReplyCode;
end;

function TScSMTPClient.Authenticate: boolean;
var
  SASLMechanism: TScSASLMechanism;
  HasLogin, HasOTP, HasSKey, HasCRAMMD5, HasCRAMSHA1: boolean;
  SASLItem: TScSASLItem;
  i: integer;
begin
  if FIsAuthenticated then begin
    Result := True;
    Exit;
  end;

  StartTLS;

  case FAuthenticationType of
    satNone: begin
      FIsAuthenticated := True;
    end;

    satDefault: begin
      HasLogin := False;
      HasOTP := False;
      HasSKey := False;
      HasCRAMMD5 := False;
      HasCRAMSHA1 := False;

      for i := 0 to FSASLMechanisms.Count - 1 do begin
        SASLMechanism := FSASLMechanisms[i].SASLMechanism;
        if SASLMechanism <> nil then
          if SASLMechanism is TScSASLLogin then
            HasLogin := True
          else
          if SASLMechanism is TScSASLOTP then
            HasOTP := True
          else
          if SASLMechanism is TScSASLSKey then
            HasSKey := True
          else
          if SASLMechanism is TScSASLCRAMMD5 then
            HasCRAMMD5 := True
          else
          if SASLMechanism is TScSASLCRAMSHA1 then
            HasCRAMSHA1 := True;
      end;

      if not HasCRAMMD5 then begin
        SASLItem := TScSASLItem(FSASLMechanisms.Add);
        SASLItem.Init('CRAM-MD5');
        TScSASLUserPassMechanism(SASLItem.SASLMechanism).Username := Username;
        TScSASLUserPassMechanism(SASLItem.SASLMechanism).Password := Password;
      end;

      if not HasCRAMSHA1 then begin
        SASLItem := TScSASLItem(FSASLMechanisms.Add);
        SASLItem.Init('CRAM-SHA1');
        TScSASLUserPassMechanism(SASLItem.SASLMechanism).Username := Username;
        TScSASLUserPassMechanism(SASLItem.SASLMechanism).Password := Password;
      end;

      if not HasLogin then begin
        SASLItem := TScSASLItem(FSASLMechanisms.Add);
        SASLItem.Init('LOGIN');
        TScSASLUserPassMechanism(SASLItem.SASLMechanism).Username := Username;
        TScSASLUserPassMechanism(SASLItem.SASLMechanism).Password := Password;
      end;

      if not HasSKey then begin
        SASLItem := TScSASLItem(FSASLMechanisms.Add);
        SASLItem.Init('SKEY');
        TScSASLUserPassMechanism(SASLItem.SASLMechanism).Username := Username;
        TScSASLUserPassMechanism(SASLItem.SASLMechanism).Password := Password;
      end;

      if not HasOTP then begin
        SASLItem := TScSASLItem(FSASLMechanisms.Add);
        SASLItem.Init('OTP');
        TScSASLUserPassMechanism(SASLItem.SASLMechanism).Username := Username;
        TScSASLUserPassMechanism(SASLItem.SASLMechanism).Password := Password;
      end;

      FSASLMechanisms.Login('AUTH', FHostName, 'smtp', [235], [334], Self, FCapabilities);
      FIsAuthenticated := True;
    end;

    satSASLMechanism: begin
      FSASLMechanisms.Login('AUTH', FHostName, 'smtp', [235], [334], Self, FCapabilities);
      FIsAuthenticated := True;
    end;
  end;

  Result := FIsAuthenticated;
end;

procedure TScSMTPClient.StartTLS;
begin
  if FTLSMode = tmDisableTLS then
    Exit;

  if FCapabilities.IndexOf('STARTTLS') > -1 then begin
    try
      SendCmd('STARTTLS', []);
    except
      Disconnect;
      raise;
    end;

    if FLastReplyCode = 220 then begin
      try
        FConnection.IsSecure := True;
        SendHello;
      except
        Disconnect;
        raise;
      end;
    end
    else
    if FTLSMode = tmRequireExplicitTLS then begin
      Disconnect;
      raise EScError.Create(seSSLNegotiationCommandFailed);
    end;
  end
  else
    if FTLSMode = tmRequireExplicitTLS then begin
      Disconnect;
      raise EScError.Create(seSSLNegotiationCommandFailed);
    end;
end;

procedure TScSMTPClient.SendHello;
var
  AHeloName: string;
begin
  FCapabilities.Clear;
  if Options.HeloName <> '' then
    AHeloName := Options.HeloName
  else
    AHeloName := ScVioTcp.GetHostName;

  if Options.UseEhlo and (SendCmd('EHLO ' + AHeloName, []) = 250) then begin
    FCapabilities.AddStrings(FLastReply);
    if FCapabilities.Count > 0 then
      FCapabilities.Delete(0);
  end
  else
    SendCmd('HELO ' + AHeloName, [250]);
end;

procedure TScSMTPClient.SendNoPipelined(Message: TScMailMessage; const From: string; Recipients: TScMailAddressList);
var
  Command: string;
  ErrCount: integer;
  i: integer;
begin
  Command := 'MAIL FROM:<' + From + '>';
  if Options.UseVerp then begin
    if FCapabilities.IndexOf('VERP') > -1 then
      Command := Command + ' VERP'
    else
      Command := Command + ' XVERP';

    if Options.VerpDelimiters <> '' then
      Command := Command + '=' + Options.VerpDelimiters;
  end;

  SendCmd(Command, [250]);

  try
    if Recipients.Count > 0 then begin
      ErrCount := 0;
      for i := 0 to Recipients.Count - 1 do begin
        SendCmd('RCPT TO:<' + Recipients[i].Address + '>', []);
        if not (FLastReplyCode in [250, 251]) then begin
          Inc(ErrCount);
          if not SkipErrorRecipient(Recipients[i].Address) then begin
            RaiseReplyError;
            Break;
          end;
        end;
      end;

      if ErrCount = Recipients.Count then
        RaiseReplyError;
    end;

    SendCmd('DATA', [354]);
    SendMessage(Message);
    SendCmd('.', [250]);
  except
    on EScSMTPError do begin
      SendCmd('RSET', []);
      raise;
    end
    else
      raise;
  end;
end;

procedure TScSMTPClient.SendPipelined(Message: TScMailMessage; const From: string; Recipients: TScMailAddressList);
var
  ErrorReply: TStringList;
  ErrorReplyCode: integer;
  Fail: boolean;
  ErrorMessage: string;
  SB: StringBuilder;
  ErrCount: integer;
  i: integer;
begin
  SB := StringBuilder.Create(8192);
  try
    SB.Append('MAIL FROM:<');
    SB.Append(From);
    SB.Append('>');

    if Options.UseVerp then begin
      if FCapabilities.IndexOf('VERP') > -1 then
        SB.Append(' VERP')
      else
        SB.Append(' XVERP');

      if Options.VerpDelimiters <> '' then begin
        SB.Append('=');
        SB.Append(Options.VerpDelimiters);
      end;
    end;

    ErrorReply := nil;
    ErrorReplyCode := 0;
    try
      SB.Append(CRLF);

      for i := 0 to Recipients.Count - 1 do begin
        SB.Append('RCPT TO:<');
        SB.Append(Recipients[i].Address);
        SB.Append('>');
        SB.Append(CRLF);
      end;

      SB.Append('DATA');
      SB.Append(CRLF);
      WriteStr(SB.ToString);

      // MAIL FROM:
      GetResponse;
      if FLastReplyCode <> 250 then
        if not Assigned(ErrorReply) then begin
          ErrorReply := TStringList.Create;
          ErrorReply.Assign(FLastReply);
          ErrorReplyCode := FLastReplyCode;
        end;

      // RCPT TO:
      if Recipients.Count > 0 then begin
        ErrCount := 0;
        for i := 0 to Recipients.Count - 1 do begin
          GetResponse;
          if not (FLastReplyCode in [250, 251]) then begin
            Inc(ErrCount);
            if not SkipErrorRecipient(Recipients[i].Address) then
              if not Assigned(ErrorReply) then begin
                ErrorReply := TStringList.Create;
                ErrorReply.Assign(FLastReply);
                ErrorReplyCode := FLastReplyCode;
              end;
          end;
        end;

        if not Assigned(ErrorReply) and (ErrCount = Recipients.Count) then begin
          ErrorReply := TStringList.Create;
          ErrorReply.Assign(FLastReply);
          ErrorReplyCode := FLastReplyCode;
        end;
      end;

      // DATA
      GetResponse;
      if FLastReplyCode = 354 then begin
        SendMessage(Message);
        if SendCmd('.', []) <> 250 then
          if not Assigned(ErrorReply) then begin
            ErrorReply := TStringList.Create;
            ErrorReply.Assign(FLastReply);
            ErrorReplyCode := FLastReplyCode;
          end;
      end
      else
        if not Assigned(ErrorReply) then begin
          ErrorReply := TStringList.Create;
          ErrorReply.Assign(FLastReply);
          ErrorReplyCode := FLastReplyCode;
        end;

      if Assigned(ErrorReply) then begin
        SendCmd('RSET', []);

        Fail := True;
        ErrorMessage := ErrorReply.Text;
        if Assigned(OnError) then
          OnError(Self, ErrorReplyCode, ErrorMessage, Fail);
        if Fail then
          raise EScSMTPError.Create(ErrorReplyCode, ErrorMessage);
      end;
    finally
      ErrorReply.Free;
    end;
  finally
    SB.Free;
  end;
end;

function TScSMTPClient.SkipErrorRecipient(const Address: string): boolean;
begin
  Result := Assigned(FOnRecipientError);
  if Result then
    FOnRecipientError(Self, Address, FLastReplyCode, FLastReply.Text, Result);
end;

procedure TScSMTPClient.SendMessage(Message: TScMailMessage; NeedEncode: boolean = True);
begin
  FConnection.BeginWriteBuffering;
  try
    if NeedEncode then begin
      WriteHeaderList(Message.GeneratedHeaders);
      SendBody(Message);
    end
    else begin
      WriteHeaderList(Message.Headers);
      WriteStr(CRLF);
      WriteRFCStrings(Message.Body);
    end;
  finally
    FConnection.EndWriteBuffering;
  end;
end;

procedure TScSMTPClient.SendText(Strings: TStrings; EncoderClass: TScCoderClass; TextEncoding: Encoding);
var
  TextStream, EncodedStream: TStream;
  Encoder: TScCoder;
  Str: string;
  Buf: TBytes;
begin
  TextStream := TMemoryStream.Create;
  try
    Str := Strings.Text;
    if Length(Str) > 0 then begin
      SetLength(Buf, 0);
      Buf := TextEncoding.GetBytes(Str);
      TextStream.WriteBuffer(Buf[0], Length(Buf));
    end;

    TextStream.Position := 0;

    EncodedStream := TScSecureConnectionStream.Create(FConnection);
    try
      Encoder := EncoderClass.Create;
      try
        Encoder.Encode(TextStream, EncodedStream);
      finally
        Encoder.Free;
      end;
    finally
      EncodedStream.Free;
    end;
  finally
    TextStream.Free;
  end;
end;

procedure TScSMTPClient.SendAttachmentData(Attachment: TScDataAttachment; EncoderClass: TScCoderClass);
var
  AttachmentStream, EncodedStream: TStream;
  Encoder: TScCoder;
begin
  EncodedStream := TScSecureConnectionStream.Create(FConnection);
  try
    Encoder := EncoderClass.Create;
    try
      if Attachment is TScAttachment then
        Encoder.Filename := TScAttachment(Attachment).Filename
      else
        Encoder.Filename := 'inline';

      AttachmentStream := Attachment.OpenStream;
      try
        Encoder.Encode(AttachmentStream, EncodedStream);
      finally
        Attachment.CloseStream;
      end;
    finally
      Encoder.Free;
    end;
  finally
    EncodedStream.Free;
  end;
end;

procedure TScSMTPClient.SendBody(Message: TScMailMessage);
var
  MessageEncoding: Encoding;
  Attachment: TScAttachment;
  AlternateView: TScAlternateView;
  LinkedResource: TScLinkedResource;
  ContentTransferEncodingStr, BoundaryStr, Str: string;
  i, j: integer;
begin
  ContentTransferEncodingStr := LowerCase(Message.ContentTransferEncoding);
  if (Message.Body.Count > 0) and
    ((ContentTransferEncodingStr = 'base64') or (ContentTransferEncodingStr = 'quoted-printable'))
  then begin
    if (Message.AlternateViews.Count > 0) or (Message.Attachments.Count > 0) then
      raise EScError.Create(seAttachCannotBeEncodedWhenTransferEncodingSet);

    WriteStr(CRLF); // after headers
    WriteStr(CRLF);

    MessageEncoding := EncodingByCharset(Message.ContentCharset);
    if ContentTransferEncodingStr = 'base64' then
      SendText(Message.Body, TScCoderMIME, MessageEncoding)
    else
      SendText(Message.Body, TScCoderQuotedPrintable, MessageEncoding);
  end
  else
  if Message.Encoding = mePlainText then begin
    WriteStr(CRLF); // after headers

    MessageEncoding := EncodingByCharset(Message.ContentCharset);
    WriteRFCStrings(Message.Body, MessageEncoding);
    WriteStr(CRLF);

    for i := 0 to Message.AlternateViews.Count - 1 do begin
      WriteStr(CRLF + '------- Start of text attachment -------' + CRLF);
      SendAlternateView(Message, Message.AlternateViews.Items[i]);
      WriteStr('------- End of text attachment -------' + CRLF + CRLF);
    end;

    for i := 0 to Message.Attachments.Count - 1 do begin
      Attachment := Message.Attachments[i];

      if Attachment.ContentTransferEncoding = '' then
        Attachment.ContentTransferEncoding := 'UUE';

      if SameText(Attachment.ContentTransferEncoding, 'XXE') then
        SendAttachmentData(Attachment, TScCoderXXE)
      else
      // if SameText(Attachment.ContentTransferEncoding, 'UUE') then
        SendAttachmentData(Attachment, TScCoderUUE);

      WriteStr(CRLF);
    end;
  end
  else begin
    BoundaryStr := '';

    if Message.Body.Count > 0 then begin
      if (Message.AlternateViews.Count > 0) or (Message.Attachments.Count > 0) then begin
        // Put the message into attachment instead of body
        AlternateView := TScAlternateView.Create(Message.AlternateViews, Message.Body);
        AlternateView.Index := 0;
        if AlternateView.ContentType = '' then
          AlternateView.ContentType := 'text/plain';
        AlternateView.ContentCharset := Message.ContentCharset;
        AlternateView.ContentTransferEncoding := 'quoted-printable';
      end
      else begin
        WriteStr(CRLF); // after headers
        MessageEncoding := EncodingByCharset(Message.ContentCharset);
        WriteRFCStrings(Message.Body, MessageEncoding);
        WriteStr(CRLF);
      end;
    end;

    if (Message.AlternateViews.Count > 0) or (Message.Attachments.Count > 1) then begin
      WriteStr(CRLF); // after headers
      WriteStr(SThisIsMultiPartMessageInMIMEFormat + CRLF + CRLF); // preamble

      BoundaryStr := Message.MIMEBoundary.Boundary;
      if BoundaryStr = '' then begin
        BoundaryStr := TScMimeBoundary.GenerateBoundary;
        Message.MIMEBoundary.Push(BoundaryStr, -1); // -1 is top level
      end;
    end;

    if (Message.AlternateViews.Count > 0) and (Message.Attachments.Count > 0) then begin
      WriteStr('--' + BoundaryStr + CRLF);

      // begin multipart/alternative
      BoundaryStr := TScMimeBoundary.GenerateBoundary;
      Message.MIMEBoundary.Push(BoundaryStr, Message.MIMEBoundary.ParentIndex + 1);
      WriteStr('Content-Type: multipart/alternative;' + CRLF);
      WriteStr(TAB + 'boundary="' + BoundaryStr + '"' + CRLF + CRLF + CRLF);
    end;

    for i := 0 to Message.AlternateViews.Count - 1 do begin
      AlternateView := Message.AlternateViews[i];

      if AlternateView.LinkedResources.Count > 0 then begin
        WriteStr('--' + BoundaryStr + CRLF);

        // begin multipart/related
        BoundaryStr := TScMimeBoundary.GenerateBoundary;
        Message.MIMEBoundary.Push(BoundaryStr, Message.MIMEBoundary.ParentIndex + 1);
        WriteStr('Content-Type: multipart/related;' + CRLF);
        WriteStr(TAB + 'boundary="' + BoundaryStr + '"' + CRLF + CRLF + CRLF);
      end;

      WriteStr('--' + BoundaryStr + CRLF);
      SendAlternateView(Message, AlternateView);

      for j := 0 to AlternateView.LinkedResources.Count - 1 do begin
        LinkedResource := AlternateView.LinkedResources[j];

        WriteStr('--' + BoundaryStr + CRLF);
        SendAttachment(Message, LinkedResource);
      end;

      if AlternateView.LinkedResources.Count > 0 then begin
        // end multipart/related
        WriteStr('--' + BoundaryStr + '--' + CRLF + CRLF);
        Message.MIMEBoundary.Pop;
        BoundaryStr := Message.MIMEBoundary.Boundary;
      end;
    end;

    if (Message.AlternateViews.Count > 0) and (Message.Attachments.Count > 0) then begin
      // end multipart/alternative
      WriteStr('--' + BoundaryStr + '--' + CRLF + CRLF);
      Message.MIMEBoundary.Pop;
      BoundaryStr := Message.MIMEBoundary.Boundary;
    end;

    for i := 0 to Message.Attachments.Count - 1 do begin
      Attachment := Message.Attachments[i];

      if MediaTypeMatches(ParseHeaderItem(Attachment.ContentType), 'multipart') then begin
        WriteStr('--' + BoundaryStr + CRLF);

        BoundaryStr := TScMimeBoundary.GenerateBoundary;
        Message.MIMEBoundary.Push(BoundaryStr, Message.MIMEBoundary.ParentIndex + 1);
        WriteStr('Content-Type: ' + ReplaceHeaderParam(Attachment.ContentType, 'boundary', '', Str) + ';' + CRLF);
        WriteStr(TAB + 'boundary="' + BoundaryStr + '"' + CRLF + CRLF + CRLF);
      end
      else begin
        if BoundaryStr <> '' then begin
          while Attachment.ParentIndex < Message.MIMEBoundary.ParentIndex do begin
            WriteStr('--' + BoundaryStr + '--' + CRLF + CRLF);
            Message.MIMEBoundary.Pop;
            BoundaryStr := Message.MIMEBoundary.Boundary;
          end;

          WriteStr('--' + BoundaryStr + CRLF);
        end;

        SendAttachment(Message, Attachment);
      end;
    end;

    if BoundaryStr <> '' then begin
      for i := 0 to Message.MIMEBoundary.Count - 1 do begin
        WriteStr('--' + Message.MIMEBoundary.Boundary + '--' + CRLF + CRLF);
        Message.MIMEBoundary.Pop;
      end;
    end;
  end;
end;

procedure TScSMTPClient.SendAlternateView(Message: TScMailMessage; AlternateView: TScAlternateView);
var
  AttachmentEncoding: Encoding;
  IsEncodingRequired: boolean;
  ContentTransferEncodingStr, Str: string;
  i, j: integer;
begin
  if AlternateView.ContentType = '' then
    AlternateView.ContentType := 'text/plain; charset="us-ascii"';

  ContentTransferEncodingStr := LowerCase(AlternateView.ContentTransferEncoding);

  if ContentTransferEncodingStr = '' then begin
    ContentTransferEncodingStr := 'quoted-printable';
    AlternateView.ContentTransferEncoding := 'quoted-printable';
  end
  else
  if (ContentTransferEncodingStr <> 'base64') and
    (ContentTransferEncodingStr <> 'quoted-printable')
  then begin
    IsEncodingRequired := False;
    for i := 0 to AlternateView.Body.Count - 1 do begin
      Str := AlternateView.Body[i];
      for j := 1 to Length(Str) do begin
        if Str[j] > #127 then begin
          IsEncodingRequired := True;
          break;
        end;
      end;
      if IsEncodingRequired then
        break;
    end;

    if IsEncodingRequired then begin
      ContentTransferEncodingStr := '8bit';
      AlternateView.ContentTransferEncoding := '8bit';
    end;
  end;

  if AlternateView.ContentType <> '' then
    WriteStr('Content-Type: ' + AlternateView.ContentType + CRLF);

  if ContentTransferEncodingStr <> '' then
    WriteStr('Content-Transfer-Encoding: ' + ContentTransferEncodingStr + CRLF);

  if AlternateView.ContentID <> '' then
    WriteStr('Content-ID: ' + AlternateView.ContentID + CRLF);

  if AlternateView.ContentDescription <> '' then
    WriteStr('Content-Description: ' + AlternateView.ContentDescription + CRLF);

  WriteHeaderList(AlternateView.SpecialHeaders);
  WriteStr(CRLF);

  AttachmentEncoding := EncodingByCharset(AlternateView.ContentCharset);
  if ContentTransferEncodingStr = 'quoted-printable' then
    SendText(AlternateView.Body, TScCoderQuotedPrintable, AttachmentEncoding)
  else
  if ContentTransferEncodingStr = 'base64' then
    SendText(AlternateView.Body, TScCoderMIME, AttachmentEncoding)
  else
    WriteRFCStrings(AlternateView.Body, AttachmentEncoding);
end;

procedure TScSMTPClient.SendAttachment(Message: TScMailMessage; Attachment: TScDataAttachment);
var
  AttachmentStream: TStream;
  MessageEncoding: Encoding;
  AFileName: string;
  Str: string;
  IsBinhex40: boolean;
begin
  if Attachment.ContentTransferEncoding = '' then
    Attachment.ContentTransferEncoding := 'base64';

  if Attachment is TScAttachment then
    if TScAttachment(Attachment).ContentDisposition = '' then
      TScAttachment(Attachment).ContentDisposition := 'attachment';

  if Attachment.ContentType = '' then
    if AnsiSameText(Attachment.ContentTransferEncoding, 'base64') then
      Attachment.ContentType := 'application/octet-stream'
    else
      Attachment.ContentType := 'text/plain; charset="us-ascii"';

  IsBinhex40 := AnsiSameText(Attachment.ContentTransferEncoding, 'binhex40');
  if IsBinhex40 then
    Attachment.ContentType := 'application/mac-binhex40';

  if Attachment is TScAttachment then
    AFileName := EncodeHeader(ExtractFileName(TScAttachment(Attachment).FileName), '', Message.HeadersTransferEncoding, Message.HeadersCharset)
  else
    AFileName := '';

  WriteStr('Content-Type: ' + Attachment.ContentType);
  if AFileName <> '' then begin
    WriteStr(';' + CRLF);
    WriteStr(TAB + 'name="' + AFileName + '"');
  end;
  WriteStr(CRLF);

  if not IsBinhex40 then begin
    WriteStr('Content-Transfer-Encoding: ' + Attachment.ContentTransferEncoding + CRLF);
    if Attachment is TScAttachment then
      WriteStr('Content-Disposition: ' + TScAttachment(Attachment).ContentDisposition + CRLF);
  end;

  if Attachment.ContentID <> '' then
    WriteStr('Content-ID: '+ Attachment.ContentID + CRLF);

  if Attachment.ContentDescription <> '' then
    WriteStr('Content-Description: ' + Attachment.ContentDescription + CRLF);

  WriteHeaderList(Attachment.SpecialHeaders);
  WriteStr(CRLF);

  if SameText(Attachment.ContentTransferEncoding, 'base64') then
    SendAttachmentData(Attachment, TScCoderMIME)
  else
  if SameText(Attachment.ContentTransferEncoding, 'quoted-printable') then
    SendAttachmentData(Attachment, TScCoderQuotedPrintable)
  else
  if SameText(Attachment.ContentTransferEncoding, 'binhex40') then
    SendAttachmentData(Attachment, TScBinHex4Coder)
  else begin
    MessageEncoding := EncodingByCharset(Attachment.ContentCharset);
    AttachmentStream := Attachment.OpenStream;
    try
      while TStreamUtils.ReadLine(AttachmentStream, Str, -1, MessageEncoding) do
        WriteRFCStr(Str + CRLF, MessageEncoding);
    finally
      Attachment.CloseStream;
    end;
  end;

  WriteStr(CRLF);
end;

(*//d TODO
function TScCustomAttachment.ResolveContentType(const ContentType: string): string;
var
  MailMessage: TScMailMessage;
  ACollection: TScAttachmentCollection;
  ContentTypeStr: string;
begin
  if ContentType <> '' then
    Result := ContentType
  else begin
    Result := '';

    if Collection is TScAttachmentCollection then
      ACollection := TScAttachmentCollection(Collection)
    else
      ACollection := nil;

    if Assigned(ACollection) then begin
      if ACollection.GetOwner is TScMailMessage then
        MailMessage := TScMailMessage(ACollection.GetOwner)
      else
        MailMessage := nil;

      if Assigned(MailMessage) and (MailMessage.Encoding = meMIME) then begin
        if ParentIndex <> -1 then begin
          ContentTypeStr := ACollection.Items[ParentIndex].Headers.Values['Content-Type'];
          if MediaTypeMatches(ParseHeaderItem(ContentTypeStr), 'multipart/digest') then
            Result := 'message/rfc822'
          else
            Result := 'text/plain; charset=us-ascii';
        end
        else
          Result := 'text/plain; charset=us-ascii';
      end;
    end;
  end;
end;
*)

procedure TScSMTPClient.InternalSend(Message: TScMailMessage; const From: string; Recipients: TScMailAddressList);
begin
  // to start TLS
  Authenticate;

  if Options.MailAgent <> '' then
    Message.SpecialHeaders.Names['X-Mailer'] := Options.MailAgent;

  if Options.Pipelined and (FCapabilities.IndexOf('PIPELINING') > -1) then
    SendPipelined(Message, From, Recipients)
  else
    SendNoPipelined(Message, From, Recipients);
end;

procedure TScSMTPClient.Send(const From, Recipients, Subject, Body: string);
var
  Message: TScMailMessage;
begin
  Message := TScMailMessage.Create;
  try
    Message.From.AsString := From;
    Message.ToAddress.AsString := Recipients;
    Message.Subject := Subject;
    Message.Body.Text := Body;

    Send(Message);
  finally
    Message.Free;
  end;
end;

procedure TScSMTPClient.Send(Message: TScMailMessage);
var
  Recipients: TScMailAddressList;
  i: integer;
begin
  Recipients := TScMailAddressList.Create(Self);
  try
    for i := 0 to Message.ToAddress.Count - 1 do
      Recipients.Add.Assign(Message.ToAddress[i]);

    for i := 0 to Message.CC.Count - 1 do
      Recipients.Add.Assign(Message.CC[i]);

    for i := 0 to Message.Bcc.Count - 1 do
      Recipients.Add.Assign(Message.Bcc[i]);

    Send(Message, Recipients);
  finally
    Recipients.Free;
  end;
end;

procedure TScSMTPClient.Send(Message: TScMailMessage; Recipients: TScMailAddressList);
var
  From: string;
begin
  From := Trim(Message.Sender.Address);

  if From = '' then
    From := Trim(Message.From.Address);

  InternalSend(Message, From, Recipients);
end;

procedure TScSMTPClient.Send(Message: TScMailMessage; const From: string);
var
  Recipients: TScMailAddressList;
  i: integer;
begin
  Recipients := TScMailAddressList.Create(Self);
  try
    for i := 0 to Message.ToAddress.Count - 1 do
      Recipients.Add.Assign(Message.ToAddress[i]);

    for i := 0 to Message.CC.Count - 1 do
      Recipients.Add.Assign(Message.CC[i]);

    for i := 0 to Message.Bcc.Count - 1 do
      Recipients.Add.Assign(Message.Bcc[i]);

    InternalSend(Message, From, Recipients);
  finally
    Recipients.Free;
  end;
end;

procedure TScSMTPClient.Send(Message: TScMailMessage; const From: string; Recipients: TScMailAddressList);
begin
  InternalSend(Message, From, Recipients);
end;

procedure TScSMTPClient.Expand(const UserName: string);
begin
  SendCmd('EXPN ' + UserName, [250, 251]);
end;

function TScSMTPClient.Verify(const UserName: string): string;
begin
  SendCmd('VRFY ' + UserName, [250, 251]);
  Result := FLastReply[0];
end;

procedure TScSMTPClient.DoAfterDisconnect(Sender: TObject);
begin
  if Assigned(FAfterDisconnect) then
    FAfterDisconnect(Self);
end;

procedure TScSMTPClient.ReadReplyEvent(const Line: string);
begin
  if Assigned(FOnReadReply) then
    FOnReadReply(Self, Line);
end;

function TScSMTPClient.GetUseUTF8: boolean;
begin
  Result := FEncoding = Encoding.UTF8;
end;

procedure TScSMTPClient.SetUseUTF8(Value: boolean);
begin
  if Value then
    FEncoding := Encoding.UTF8
  else
    if (FEncoding = Encoding.UTF8) or (FEncoding = Encoding.Unicode) or (FEncoding = Encoding.BigEndianUnicode) then
      FEncoding := Encoding.Default;
end;

procedure TScSMTPClient.SetTLSMode(const Value: TScTLSMode);
begin
  if FTLSMode <> Value then begin
    CheckInactive;
    FTLSMode := Value;

    if FTLSMode in [tmRequireExplicitTLS, tmAllowExplicitTLS] then
      Options.FUseEhlo := True;
  end;
end;

procedure TScSMTPClient.SetAuthenticationType(const Value: TScSMTPAuthenticationType);
begin
  FAuthenticationType := Value;
  if Value = satSASLMechanism then
    Options.FUseEhlo := True;
end;

procedure TScSMTPClient.SetUri(const Value: string);
begin
  if FUri <> Value then begin
    CheckInactive;

    FUri := Trim(Value);

    TScHttpParser.ParseURL(FUri, FScheme, FUserName, FPassword,
      FHostName, FPortNo, FPath, FResource, FParameters, FQuery, FFragment);
    if FScheme = '' then
      FScheme := SmtpScheme;
    if FPortNo = '' then
      FPort := SMTP_DEFAULT_PORT
    else
      FPort := StrToInt(Copy(FPortNo, 2, Length(FPortNo)));
  end;
end;

procedure TScSMTPClient.SetHostName(const Value: string);
begin
  if Value <> FHostName then begin
    CheckInactive;
    FHostName := Trim(Value);
  end;
end;

procedure TScSMTPClient.SetPort(const Value: integer);
begin
  if Value <> FPort then begin
    CheckInactive;
    FPort := Value;
  end;
end;

procedure TScSMTPClient.UpdateUsernames;
var
  i: integer;
begin
  for i := 0 to FSASLMechanisms.Count - 1 do
    if FSASLMechanisms[i].SASLMechanism is TScSASLUserPassMechanism then
      TScSASLUserPassMechanism(FSASLMechanisms[i].SASLMechanism).Username := FUsername;
end;

procedure TScSMTPClient.UpdatePasswords;
var
  i: integer;
begin
  for i := 0 to FSASLMechanisms.Count - 1 do
    if FSASLMechanisms[i].SASLMechanism is TScSASLUserPassMechanism then
      TScSASLUserPassMechanism(FSASLMechanisms[i].SASLMechanism).Password := FPassword;
end;

procedure TScSMTPClient.SetUsername(const Value: string);
begin
  if Value <> FUsername then begin
    CheckInactive;
    FUsername := Trim(Value);
    UpdateUsernames;
  end;
end;

procedure TScSMTPClient.SetPassword(const Value: string);
begin
  if Value <> FPassword then begin
    CheckInactive;
    FPassword := Trim(Value);
    UpdatePasswords;
  end;
end;

procedure TScSMTPClient.SetTimeout(const Value: integer);
begin
  if Value <> FTimeout then begin
    CheckInactive;
    FTimeout := Value;
  end;
end;

procedure TScSMTPClient.SetSocketReceiveBufferSize(Value: integer);
begin
  if GetActive and (Value > 0) then
    FConnection.SetSocketOption(SOL_SOCKET, SO_RCVBUF, Value);
end;

procedure TScSMTPClient.SetSocketSendBufferSize(Value: integer);
begin
  if GetActive and (Value > 0) then
    FConnection.SetSocketOption(SOL_SOCKET, SO_SNDBUF, Value);
end;

procedure TScSMTPClient.SetProxyOptions(Value: TProxyOptions);
begin
  if Value <> FProxyOptions then begin
    CheckInactive;
    FProxyOptions.Assign(Value);
  end;
end;

procedure TScSMTPClient.SetSSLOptions(Value: TScSSLClientOptions);
begin
  if Value <> FSSLOptions then begin
    CheckInactive;
    FSSLOptions.Assign(Value);
  end;
end;

procedure TScSMTPClient.SetOptions(Value: TScSMTPClientOptions);
begin
  if Value <> FOptions then begin
    CheckInactive;
    FOptions.Assign(Value);
  end;
end;

procedure TScSMTPClient.SetSASLMechanisms(Value: TScSASLCollection);
begin
  FSASLMechanisms.Assign(Value);
end;

end.
