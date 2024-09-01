
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF DAC}
{$I SB.inc}
unit ScCMS;
{$ENDIF}

interface

uses
{$IFDEF VER16P}
  System.Types,
{$ENDIF}
  Classes, SysUtils, ScConsts, ScTypes, ScUtils,
  ScCLRClasses, ScDECUtil, ScSymmetricAlgorithm,
  ScASN1, ScBridge, ScCertificateExts, ScCertificateConsts;

const
  DefaultDigestAlgorithm = haSHA2_256;
  DefaultEncryptionAlgorithm = saAES192_cbc;

type
  TScCMSEncoding = (cePEM, ceSMIME, ceDER);

  TScCMSSubjectIdentifierType = (sitIssuerAndSerialNumber, sitSubjectKeyIdentifier, sitKeyIdentifier);

  TScCMSSubjectIdentifier = class
  private
    FSubjectIdentifierType: TScCMSSubjectIdentifierType;
    FIssuer: TScDistinguishedName;
    FSerialNumber: string;
    FSubjectKeyIdentifier: string;
    FKeyIdentifierDate: TDateTime;

  protected
    FReadOnly: boolean;
    procedure SetReadOnly(Value: boolean);
    procedure CheckReadOnly;

    function CheckCertificateMatching(Certificate: TScCertificate): boolean;

    procedure Parse(LexemInfo: TScLexemInfo);
    function Encode: TBytes;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Init(SubjectIdentifierType: TScCMSSubjectIdentifierType; Certificate: TScCertificate); overload;
    procedure Init(Certificate: TScCertificate); overload;
    procedure Assign(Source: TScCMSSubjectIdentifier);

    property SubjectIdentifierType: TScCMSSubjectIdentifierType read FSubjectIdentifierType;
    property Issuer: TScDistinguishedName read FIssuer;
    property SerialNumber: string read FSerialNumber;
    property SubjectKeyIdentifier: string read FSubjectKeyIdentifier;
    property KeyIdentifierDate: TDateTime read FKeyIdentifierDate;
  end;

  TScCMSOriginatorIdentifierOrKeyType = (oitIssuerAndSerialNumber, oitSubjectKeyIdentifier, oitPublicKeyInfo);

  TScCMSOriginatorIdentifierOrKey = class
  private
    FOriginatorIdentifierOrKeyType: TScCMSOriginatorIdentifierOrKeyType;
    FIssuer: TScDistinguishedName;
    FSerialNumber: string;
    FSubjectKeyIdentifier: string;
    FPublicKeyAlgorithmIdentifier: TScASN1AlgorithmIdentifier;
    FPublicKey: TBytes;

  protected
    procedure Parse(LexemInfo: TScLexemInfo);
    function Encode: TBytes;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Init(OriginatorIdentifierOrKeyType: TScCMSOriginatorIdentifierOrKeyType; Certificate: TScCertificate); overload;
    procedure Init(Certificate: TScCertificate); overload;
    procedure Assign(Source: TScCMSOriginatorIdentifierOrKey);

    property OriginatorIdentifierOrKeyType: TScCMSOriginatorIdentifierOrKeyType read FOriginatorIdentifierOrKeyType;
    property Issuer: TScDistinguishedName read FIssuer;
    property SerialNumber: string read FSerialNumber;
    property SubjectKeyIdentifier: string read FSubjectKeyIdentifier;
    property PublicKeyAlgorithmIdentifier: TScASN1AlgorithmIdentifier read FPublicKeyAlgorithmIdentifier;
    property PublicKey: TBytes read FPublicKey;
  end;

  TScCMSSignedAttributes = class(TScPKCS7Attributes)
  protected
    function GetASN1Description: TScASN1Description; override;
  end;

  TScCMSUnsignedAttributes = class(TScPKCS7Attributes)
  protected
    function GetASN1Description: TScASN1Description; override;
  end;

  TScCMSSMIMEAttributes = class(TScASN1Attributes)
  public
    constructor Create; overload;
    constructor Create(const RawValue: TBytes); overload;

    procedure Decode(const RawValue: TBytes);
    function Encode: TBytes;
  end;

  TScCMSContentInfo = class
  private
    FContentType: TScOId;
    FContentBuffer: TBytes;
    FContentStream: TScStreamInfo;

  protected
    FReadOnly: boolean;
    procedure SetReadOnly(Value: boolean);
    procedure CheckReadOnly;

    procedure Clear;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Init(ContentType: TScOId; const ContentBuffer: TBytes); overload;
    procedure Init(ContentType: TScOId; ContentStream: TStream); overload;
    procedure Init(const ContentBuffer: TBytes); overload;
    procedure Init(ContentStream: TStream); overload;
    procedure Assign(Source: TScCMSContentInfo);

    function GetContentData: TBytes;

    property ContentType: TScOId read FContentType;
    property ContentBuffer: TBytes read FContentBuffer;
    property ContentStream: TScStreamInfo read FContentStream;
  end;

  TScCMSIncludedAttribute = (ciaContentType, ciaMessageDigest, ciaSigningTime, ciaSMIMEAttribute);
  TScCMSIncludedAttributes = set of TScCMSIncludedAttribute;

  TScCMSSignerInfo = class(TScPersistent)
  private
    FDigestAlgorithmIdentifier: TScASN1AlgorithmIdentifier;
    FSignerIdentifier: TScCMSSubjectIdentifier;
    FSignedAttributes: TScCMSSignedAttributes;
    FUnsignedAttributes: TScCMSUnsignedAttributes;
    FSignatureAlgorithmIdentifier: TScASN1AlgorithmIdentifier;
    FCertificate: TScCertificate;
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF}
    FCertificateListRef: TScCertificateList;

    FIncludedAttributes: TScCMSIncludedAttributes;
    FSMIMEAttribute: TScCMSSMIMEAttributes;

    procedure SetCertificate(Value: TScCertificate);
    procedure SetDigestAlgorithmIdentifier(Value: TScASN1AlgorithmIdentifier);
    function GetDigestAlgorithm: TScHashAlgorithm;
    procedure SetDigestAlgorithm(Value: TScHashAlgorithm);
    procedure SetSignerIdentifier(Value: TScCMSSubjectIdentifier);
    procedure SetSignatureAlgorithmIdentifier(Value: TScASN1AlgorithmIdentifier);
    function GetSignatureAlgorithm: TScSignatureAlgorithm;
    procedure SetSignatureAlgorithm(Value: TScSignatureAlgorithm);

    function GetContentType: string;
    procedure SetContentType(const Value: string);
    function GetMessageDigest: TBytes;
    procedure SetMessageDigest(const Value: TBytes);
    function GetSigningTime: TDateTime;
    procedure SetSigningTime(const Value: TDateTime);

  protected
    FReadOnly: boolean;
    procedure SetReadOnly(Value: boolean); virtual;
    procedure CheckReadOnly;
    procedure SetIncludedAttributes(const Value: TScCMSIncludedAttributes);
    function FindAttributeByOid(const OId: string): TScPKCS7Attribute;

    function Clone: TScPersistent; override;
    function GetVersion: integer;

  public
    constructor Create; overload;
    constructor Create(SignerIdentifierType: TScCMSSubjectIdentifierType; Certificate: TScCertificate); overload;
    constructor Create(Certificate: TScCertificate); overload;
    destructor Destroy; override;

    procedure Assign(Source: TScPersistent); override;

    function CalcHash(const Content: TBytes): TBytes; overload;
    function CalcHash(Stream: TStream; Count: Int64 = 0): TBytes; overload;
    procedure CheckHash(const Content: TBytes); overload;
    procedure CheckHash(Stream: TStream; Count: Int64 = 0); overload;

    property Certificate: TScCertificate read FCertificate write SetCertificate;
    property DigestAlgorithmIdentifier: TScASN1AlgorithmIdentifier read FDigestAlgorithmIdentifier write SetDigestAlgorithmIdentifier;
    property DigestAlgorithm: TScHashAlgorithm read GetDigestAlgorithm write SetDigestAlgorithm;
    property SignerIdentifier: TScCMSSubjectIdentifier read FSignerIdentifier write SetSignerIdentifier;
    property SignedAttributes: TScCMSSignedAttributes read FSignedAttributes;
    property UnsignedAttributes: TScCMSUnsignedAttributes read FUnsignedAttributes;
    property SignatureAlgorithmIdentifier: TScASN1AlgorithmIdentifier read FSignatureAlgorithmIdentifier write SetSignatureAlgorithmIdentifier;
    property SignatureAlgorithm: TScSignatureAlgorithm read GetSignatureAlgorithm write SetSignatureAlgorithm;

    property IncludedAttributes: TScCMSIncludedAttributes read FIncludedAttributes write FIncludedAttributes;
    property ContentType: string read GetContentType write SetContentType;
    property MessageDigest: TBytes read GetMessageDigest write SetMessageDigest;
    property SigningTime: TDateTime read GetSigningTime write SetSigningTime;
    property SMIMEAttribute: TScCMSSMIMEAttributes read FSMIMEAttribute;

    property Version: integer read GetVersion;
  end;

  TScCMSSignature = class (TScCMSSignerInfo)
  private
    FContentBuffer: TBytes;
    FContentStream: TScStreamInfo;
    FSignature: TBytes;

    procedure GetSignatureAlgorithms(out SignHashAlg: TScHashAlgorithm; out Padding: TScPaddingMode);
    function CalcContentHash(OnlyContentData: boolean = False): TBytes;

  protected
    function Clone: TScPersistent; override;
    procedure Parse(LexemInfo: TScLexemInfo);
    function Encode: TBytes;
    procedure CheckIncludedAttributes;

  public
    constructor Create;
    destructor Destroy; override;

    procedure ComputeSignature;
    procedure CheckSignature;
    property Signature: TBytes read FSignature;
  end;

  TScCMSSignatures = class(TScPersistentObjectList)
  private
    function GetSignatures(Index: integer): TScCMSSignature;
    procedure SetSignatures(Index: integer; Item: TScCMSSignature);
  protected
    function GetItemClassType: TScPersistentClass; override;

  public
    property Signatures[Index: integer]: TScCMSSignature read GetSignatures write SetSignatures; default;
  end;

  TScCMSData = class
  private
    FTmpBase64Buf: TBytes;
    FCMSEncoding: TScCMSEncoding;

  protected
    FCertificates: TScCertificateList;
    procedure ClearCertificateList;

    function GetHeader: TBytes;
    function GetFooter: TBytes;
    procedure WriteHeader(OutStream: TStream);
    procedure WriteFooter(OutStream: TStream);
    function EncodeBlock(const Data: TBytes; Offset, Count: integer): TBytes; overload;
    procedure EncodeBlock(const Data: TBytes; Offset, Count: integer;
      OutStream: TStream; IsLastBlock: boolean; out UnReadCount: integer); overload;

    class function DecodeData(const Data: TBytes; out CMSEncoding: TScCMSEncoding): TBytes; overload;
    class procedure DecodeData(InStream, OutStream: TStream; out CMSEncoding: TScCMSEncoding); overload;

  public
    constructor Create;
    destructor Destroy; override;

    property CMSEncoding: TScCMSEncoding read FCMSEncoding write FCMSEncoding;
  end;

  TScCMSSignedData = class(TScCMSData)
  private
    FContentInfo: TScCMSContentInfo;
    FDigestAlgorithms: TStringList; // TScASN1AlgorithmIdentifiers
    FSignatures: TScCMSSignatures;

    procedure SetReadOnly(Value: boolean);

    function FindSignatureInfo(Certificate: TScCertificate): TScCMSSignature;
    procedure Decode(ASN1Compiler: TScASN1Compiler; IsStreamUsed: boolean); overload;

    procedure EncodeContentData(OutStream: TStream;
      const PrefixData: TBytes; PrefixOffset, PrefixCount: integer; out PostfixData: TBytes);

  public
    constructor Create;
    destructor Destroy; override;

    procedure Init(ContentInfo: TScCMSContentInfo); overload;
    procedure Init(const ContentBuffer: TBytes); overload;
    procedure Init(ContentStream: TStream); overload;

    procedure Decode(const RawData: TBytes); overload;
    procedure Decode(Stream: TStream); overload;
    function Encode(IncludeContent: boolean = False): TBytes; overload;
    procedure Encode(Stream: TStream; IncludeContent: boolean = False); overload;

    procedure ComputeSignature(SignerInfo: TScCMSSignerInfo);
    procedure CheckSignature(Certificate: TScCertificate);

    property ContentInfo: TScCMSContentInfo read FContentInfo;
    property Certificates: TScCertificateList read FCertificates;
    property Signatures: TScCMSSignatures read FSignatures;
  end;

  TScCMSRecipient = class
  private
    FCertificate: TScCertificate;
    FRecipientIdentifierType: TScCMSSubjectIdentifierType;
  public
    constructor Create; overload;
    constructor Create(RecipientIdentifierType: TScCMSSubjectIdentifierType; Certificate: TScCertificate); overload;
    constructor Create(Certificate: TScCertificate); overload;
    procedure Init(RecipientIdentifierType: TScCMSSubjectIdentifierType; Certificate: TScCertificate); overload;
    procedure Init(Certificate: TScCertificate); overload;

    property Certificate: TScCertificate read FCertificate;
    property RecipientIdentifierType: TScCMSSubjectIdentifierType read FRecipientIdentifierType;
  end;

  TScCMSRecipientInfoType = (ritKeyTransport, ritKeyAgreement, ritKEK, ritPassword, ritUnknown);

  TScCMSRecipientInfo = class(TScPersistent)
  private
    FRecipientInfoType: TScCMSRecipientInfoType;
    FKeyEncryptionAlgorithmIdentifier: TScASN1AlgorithmIdentifier;
    FEncryptedKey: TBytes;

  protected
    function GetVersion: integer; virtual;
    procedure Parse(LexemInfo: TScLexemInfo); virtual; abstract;
    function Encode: TBytes; virtual; abstract;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Assign(Source: TScPersistent); override;

    property RecipientInfoType: TScCMSRecipientInfoType read FRecipientInfoType;
    property KeyEncryptionAlgorithmIdentifier: TScASN1AlgorithmIdentifier read FKeyEncryptionAlgorithmIdentifier;
    property EncryptedKey: TBytes read FEncryptedKey;
  end;

  TScCMSKeyTransRecipientInfo = class(TScCMSRecipientInfo)
  private
    FRecipientIdentifier: TScCMSSubjectIdentifier;

  protected
    function Clone: TScPersistent; override;

    function GetVersion: integer; override;
    procedure Parse(LexemInfo: TScLexemInfo); override;
    function Encode: TBytes; override;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TScPersistent); override;
    procedure Init(Recipient: TScCMSRecipient); overload;
    procedure Init(Recipient: TScCMSRecipient; const EncryptedKey: TBytes); overload;

    property RecipientIdentifier: TScCMSSubjectIdentifier read FRecipientIdentifier;
  end;

  TScCMSKeyAgreeRecipientInfo = class(TScCMSRecipientInfo)
  private
    FRecipientIdentifier: TScCMSSubjectIdentifier;
    FOriginatorIdentifier: TScCMSOriginatorIdentifierOrKey;
    FUserKeyingMaterial: TBytes;

  protected
    function Clone: TScPersistent; override;

    function GetVersion: integer; override;
    procedure Parse(LexemInfo: TScLexemInfo); override;
    function Encode: TBytes; override;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TScPersistent); override;

    property RecipientIdentifier: TScCMSSubjectIdentifier read FRecipientIdentifier;
    property OriginatorIdentifier: TScCMSOriginatorIdentifierOrKey read FOriginatorIdentifier;
    property UserKeyingMaterial: TBytes read FUserKeyingMaterial;
  end;

  TScCMSKEKRecipientInfo = class(TScCMSRecipientInfo)
  private
    FKeyIdentifier: TBytes;
    FDate: TDateTime;

  protected
    function Clone: TScPersistent; override;

    function GetVersion: integer; override;
    procedure Parse(LexemInfo: TScLexemInfo); override;
    function Encode: TBytes; override;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TScPersistent); override;

    property KeyIdentifier: TBytes read FKeyIdentifier;
    property Date: TDateTime read FDate;
  end;

  TScCMSPasswordRecipientInfo = class(TScCMSRecipientInfo)
  private
    FKeyDerivationAlgorithmIdentifier: TScASN1AlgorithmIdentifier;

  protected
    function Clone: TScPersistent; override;

    function GetVersion: integer; override;
    procedure Parse(LexemInfo: TScLexemInfo); override;
    function Encode: TBytes; override;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Assign(Source: TScPersistent); override;

    property KeyDerivationAlgorithmIdentifier: TScASN1AlgorithmIdentifier read FKeyDerivationAlgorithmIdentifier;
  end;

  TScCMSRecipientInfos = class(TScPersistentObjectList)
  private
    function GetRecipientInfo(Index: integer): TScCMSRecipientInfo;
    procedure SetRecipientInfo(Index: integer; Item: TScCMSRecipientInfo);
  protected
    function GetItemClassType: TScPersistentClass; override;

  public
    property RecipientInfos[Index: integer]: TScCMSRecipientInfo read GetRecipientInfo write SetRecipientInfo; default;
  end;

  TScCMSEnvelopedData = class(TScCMSData)
  private
    FContentEncryptionAlgorithm: TScASN1AlgorithmIdentifier;
    FContentInfo: TScCMSContentInfo;
    FRecipientInfos: TScCMSRecipientInfos;
    FUnprotectedAttributes: TScCMSUnsignedAttributes;

    FSessionKey: TBytes;
    FSessionIV: TBytes;

    procedure SetReadOnly(Value: boolean);

    procedure Clear;
    procedure ClearSessionKey;
    function FindRecipientInfo(Certificate: TScCertificate): TScCMSRecipientInfo;
    procedure CreateIfNotExistsRecipientInfo(Recipient: TScCMSRecipient);
    function CreateDecCipher(Certificate: TScCertificate): TSymmetricAlgorithm;
    procedure Decode(ASN1Compiler: TScASN1Compiler; IsStreamUsed: boolean); overload;

    function GetEncryptionAlgorithm: TScSymmetricAlgorithm;
    function CalcEncryptedSize: Int64;
    procedure EncryptContentData(OutStream: TStream;
      const PrefixData: TBytes; PrefixOffset, PrefixCount: integer; out PostfixData: TBytes);

  public
    constructor Create;
    destructor Destroy; override;

    procedure Init(ContentInfo: TScCMSContentInfo; EncryptionAlgorithm: TScASN1AlgorithmIdentifier); overload;
    procedure Init(ContentInfo: TScCMSContentInfo; EncryptionAlgorithm: TScSymmetricAlgorithm = saTripleDES_cbc); overload;
    procedure Init(const ContentBuffer: TBytes; EncryptionAlgorithm: TScSymmetricAlgorithm = saTripleDES_cbc); overload;
    procedure Init(ContentStream: TStream; EncryptionAlgorithm: TScSymmetricAlgorithm = saTripleDES_cbc); overload;

    procedure Decode(const RawData: TBytes); overload;
    procedure Decode(Stream: TStream); overload;
    function Encode: TBytes; overload;
    procedure Encode(Stream: TStream); overload;

    procedure Encrypt(Recipient: TScCMSRecipient);
    function Decrypt(Certificate: TScCertificate): TBytes; overload;
    procedure Decrypt(Certificate: TScCertificate; OutStream: TStream); overload;

    property ContentEncryptionAlgorithm: TScASN1AlgorithmIdentifier read FContentEncryptionAlgorithm;
    property ContentInfo: TScCMSContentInfo read FContentInfo;
    property OriginatorCertificates: TScCertificateList read FCertificates;
    property RecipientInfos: TScCMSRecipientInfos read FRecipientInfos;
    property UnprotectedAttributes: TScCMSUnsignedAttributes read FUnprotectedAttributes;
  end;

{$IFDEF VER16P}
  [ComponentPlatformsAttribute({$IFDEF STD}pidDevartWinPlatforms{$ELSE}pidDevartAllPlatforms{$ENDIF})]
{$ENDIF}
  TScCMSProcessor = class(TComponent)
  private
    FStorage: TScStorage;
    FCertificateName: string;
    FCertificate: TScCertificate;
    FDigestAlgorithm: TScHashAlgorithm;
    FEncryptionAlgorithm: TScSymmetricAlgorithm;

    FContentType: TScOId;
    FEnvelopedData: TScCMSEnvelopedData;
    FRecipient: TScCMSRecipient;
    FSignedData: TScCMSSignedData;
    FSignerInfo: TScCMSSignerInfo;

    procedure SetStorage(Value: TScStorage);
    procedure SetCertificateName(const Value: string);
    procedure SetCertificate(Value: TScCertificate);
    procedure SetDigestAlgorithm(Value: TScHashAlgorithm);
    procedure SetEncryptionAlgorithm(Value: TScSymmetricAlgorithm);
    function GetCertificate: TScCertificate;

  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure Notification(Component: TComponent; Operation: TOperation); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Certificate: TScCertificate read FCertificate write SetCertificate;

    property EnvelopedData: TScCMSEnvelopedData read FEnvelopedData;
    property SignedData: TScCMSSignedData read FSignedData;

    class function EncodeData(const Data: TBytes; CMSEncoding: TScCMSEncoding): TBytes; overload;
    class procedure EncodeData(InStream, OutStream: TStream; CMSEncoding: TScCMSEncoding); overload;
    class function DecodeData(const Data: TBytes): TBytes; overload;
    class procedure DecodeData(InStream, OutStream: TStream); overload;

    function Encrypt(const Data: TBytes; Encoding: TScCMSEncoding = ceDER): TBytes; overload;
    procedure Encrypt(InStream, OutStream: TStream; Encoding: TScCMSEncoding = ceDER); overload;
    procedure Encrypt(const InFileName, OutFileName: string; Encoding: TScCMSEncoding = ceDER); overload;

    function Decrypt(const Data: TBytes): TBytes; overload;
    procedure Decrypt(InStream, OutStream: TStream); overload;
    procedure Decrypt(const InFileName, OutFileName: string); overload;

    function Sign(const Data: TBytes; IncludeContent: boolean = False; Encoding: TScCMSEncoding = ceDER): TBytes; overload;
    procedure Sign(InStream, OutStream: TStream; IncludeContent: boolean = False; Encoding: TScCMSEncoding = ceDER); overload;
    procedure Sign(const InFileName, OutFileName: string; IncludeContent: boolean = False; Encoding: TScCMSEncoding = ceDER); overload;

    procedure CheckSignature(const Data: TBytes); overload;
    procedure CheckSignature(InStream: TStream; TmpDecodedStream: TStream = nil); overload;
    procedure CheckSignature(const FileName: string; const TmpFileName: string = ''); overload;

    function SignAndEncrypt(const Data: TBytes; Encoding: TScCMSEncoding = ceDER): TBytes; overload;
    procedure SignAndEncrypt(InStream, OutStream: TStream; TmpSignedStream: TStream = nil; Encoding: TScCMSEncoding = ceDER); overload;
    procedure SignAndEncrypt(const InFileName, OutFileName: string; const TmpFileName: string = ''; Encoding: TScCMSEncoding = ceDER); overload;

    function DecryptAndCheckSignature(const Data: TBytes): TBytes; overload;
    procedure DecryptAndCheckSignature(InStream, OutStream: TStream; TmpDecryptedStream: TStream = nil); overload;
    procedure DecryptAndCheckSignature(const InFileName, OutFileName: string; const TmpFileName: string = ''); overload;

  published
    property Storage: TScStorage read FStorage write SetStorage;
    property CertificateName: string read FCertificateName write SetCertificateName;

    property DigestAlgorithm: TScHashAlgorithm read FDigestAlgorithm write SetDigestAlgorithm default DefaultDigestAlgorithm;
    property EncryptionAlgorithm: TScSymmetricAlgorithm read FEncryptionAlgorithm write SetEncryptionAlgorithm default DefaultEncryptionAlgorithm;
  end;

implementation

uses
  ScFunctions, ScBase64, ScAlgorithmSupport, ScHashAlgorithm;

const
  CERTIFICATE_PREFIX = '{84A2AFA4-B460-41EB-8DC4-9E270B9AAF62}';
  BUFFER_SIZE = 64 * 1024;
  DER_BUFFER_SIZE = DER_LINE_LENGTH * 32{MaxChipherBuffer} * 42{Koef to get Max buffer size less than 64K};

{ TScCMSSubjectIdentifier }

constructor TScCMSSubjectIdentifier.Create;
begin
  inherited;

  FIssuer := TScDistinguishedName.Create;
  FSubjectIdentifierType := sitIssuerAndSerialNumber;
end;

destructor TScCMSSubjectIdentifier.Destroy;
begin
  FIssuer.Free;
  inherited;
end;

procedure TScCMSSubjectIdentifier.SetReadOnly(Value: boolean);
begin
  FReadOnly := Value;
end;

procedure TScCMSSubjectIdentifier.CheckReadOnly;
begin
  if FReadOnly then
    raise EScError.Create(seChangingReadOnlyObject);
end;

procedure TScCMSSubjectIdentifier.Init(SubjectIdentifierType: TScCMSSubjectIdentifierType; Certificate: TScCertificate);
begin
  CheckReadOnly;

  if Certificate = nil then
    raise EScError.Create(seInvalidInputArgs);

  FSubjectIdentifierType := SubjectIdentifierType;
  FIssuer.Assign(Certificate.IssuerName);
  FSerialNumber := Certificate.SerialNumber;
  FSubjectKeyIdentifier := Certificate.SubjectKeyIdentifier;

  if (FSubjectIdentifierType in [sitSubjectKeyIdentifier, sitKeyIdentifier]) and
    (FSubjectKeyIdentifier = '') then
    raise EScError.Create(seNullSubjectKeyIdentifier);
end;

procedure TScCMSSubjectIdentifier.Init(Certificate: TScCertificate);
begin
  CheckReadOnly;

  if Certificate = nil then
    raise EScError.Create(seInvalidInputArgs);

  FSubjectIdentifierType := sitIssuerAndSerialNumber;
  FIssuer.Assign(Certificate.IssuerName);
  FSerialNumber := Certificate.SerialNumber;
  FSubjectKeyIdentifier := Certificate.SubjectKeyIdentifier;
end;

procedure TScCMSSubjectIdentifier.Assign(Source: TScCMSSubjectIdentifier);
begin
  CheckReadOnly;

  if Source = nil then
    raise EScError.Create(seInvalidInputArgs);

  FSubjectIdentifierType := Source.FSubjectIdentifierType;
  FIssuer.Assign(Source.FIssuer);
  FSerialNumber := Source.FSerialNumber;
  FSubjectKeyIdentifier := Source.FSubjectKeyIdentifier;
  FKeyIdentifierDate := Source.FKeyIdentifierDate;
end;

function TScCMSSubjectIdentifier.CheckCertificateMatching(Certificate: TScCertificate): boolean;
begin
  if Certificate = nil then
    raise EScError.Create(seInvalidInputArgs);

  Result := False;

  case FSubjectIdentifierType of
    sitIssuerAndSerialNumber:
      Result := FIssuer.Equals(Certificate.IssuerName) and (FSerialNumber = Certificate.SerialNumber);
    sitSubjectKeyIdentifier, sitKeyIdentifier:
      Result := SameText(FSubjectKeyIdentifier, Certificate.SubjectKeyIdentifier);
    else
      Assert(False);
  end;
end;

procedure TScCMSSubjectIdentifier.Parse(LexemInfo: TScLexemInfo);
begin
  FSerialNumber := '';
  FSubjectKeyIdentifier := '';
  FKeyIdentifierDate := 0;
  TScASNUtils.Parse(FIssuer, nil); // Clear

  LexemInfo := LexemInfo.GetChoiceLexem;

  if SameText(LexemInfo.Name, 'IssuerAndSerialNumber') then begin
    FSubjectIdentifierType := sitIssuerAndSerialNumber;
    TScASNUtils.Parse(FIssuer, LexemInfo['Issuer']);
    FSerialNumber := LexemInfo['SerialNumber'].AsString;
  end
  else if SameText(LexemInfo.Name, 'SubjectKeyIdentifier') then begin
    FSubjectIdentifierType := sitSubjectKeyIdentifier;
    FSubjectKeyIdentifier := BytesToHexStr(LexemInfo.AsBytes);
  end
  else if SameText(LexemInfo.Name, 'KeyId') then begin
    FSubjectIdentifierType := sitKeyIdentifier;
    FSubjectKeyIdentifier := BytesToHexStr(LexemInfo['SubjectKeyIdentifier'].AsBytes);
    FKeyIdentifierDate := LexemInfo['Date'].AsDateTime;
  end
  else
    raise EScError.Create(seWrongDataFormat);
end;

function TScCMSSubjectIdentifier.Encode: TBytes;
var
  ASN1Compiler: TScASN1Compiler;
begin
  ASN1Compiler := TScASN1Compiler.Create;
  try
    case FSubjectIdentifierType of
      sitIssuerAndSerialNumber: begin
        if not ASN1Compiler.Parse(asn1_ISSUER_AND_SERIAL_NUMBER_DESC) then
          raise EScError.Create(seWrongDataFormat);

        ASN1Compiler['Issuer'].EncodedData := FIssuer.Encode;
        ASN1Compiler['SerialNumber'].AsString := FSerialNumber;
      end;
      sitSubjectKeyIdentifier: begin
        if not ASN1Compiler.Parse(asn1_SUBJECT_KEY_IDENTIFIER_DESC) then
          raise EScError.Create(seWrongDataFormat);

        ASN1Compiler.Root.AsString := FSubjectKeyIdentifier;
      end;
      sitKeyIdentifier: begin
        if not ASN1Compiler.Parse(asn1_RECIPIENT_KEY_IDENTIFIER_DESC) then
          raise EScError.Create(seWrongDataFormat);

        ASN1Compiler['SubjectKeyIdentifier'].AsString := FSubjectKeyIdentifier;
        if FKeyIdentifierDate <> 0 then
          ASN1Compiler['Date'].AsDateTime := FKeyIdentifierDate;
      end;
    end;

    Result := ASN1Compiler.Build;
  finally
    ASN1Compiler.Free;
  end;
end;

{ TScCMSOriginatorIdentifierOrKey }

constructor TScCMSOriginatorIdentifierOrKey.Create;
begin
  inherited;

  FIssuer := TScDistinguishedName.Create;
  FPublicKeyAlgorithmIdentifier := TScASN1AlgorithmIdentifier.Create;
  FOriginatorIdentifierOrKeyType := oitIssuerAndSerialNumber;
end;

destructor TScCMSOriginatorIdentifierOrKey.Destroy;
begin
  FIssuer.Free;
  FPublicKeyAlgorithmIdentifier.Free;
  inherited;
end;

procedure TScCMSOriginatorIdentifierOrKey.Init(
  OriginatorIdentifierOrKeyType: TScCMSOriginatorIdentifierOrKeyType; Certificate: TScCertificate);
begin
  if (Certificate = nil) or (OriginatorIdentifierOrKeyType = oitPublicKeyInfo) then
    raise EScError.Create(seInvalidInputArgs);

  FOriginatorIdentifierOrKeyType := OriginatorIdentifierOrKeyType;
  FIssuer.Assign(Certificate.IssuerName);
  FSerialNumber := Certificate.SerialNumber;
  FSubjectKeyIdentifier := Certificate.SubjectKeyIdentifier;
end;

procedure TScCMSOriginatorIdentifierOrKey.Init(Certificate: TScCertificate);
begin
  if Certificate = nil then
    raise EScError.Create(seInvalidInputArgs);

  FOriginatorIdentifierOrKeyType := oitIssuerAndSerialNumber;
  FIssuer.Assign(Certificate.IssuerName);
  FSerialNumber := Certificate.SerialNumber;
  FSubjectKeyIdentifier := Certificate.SubjectKeyIdentifier;
end;

procedure TScCMSOriginatorIdentifierOrKey.Assign(Source: TScCMSOriginatorIdentifierOrKey);
begin
  if Source = nil then
    raise EScError.Create(seInvalidInputArgs);

  FOriginatorIdentifierOrKeyType := Source.FOriginatorIdentifierOrKeyType;
  FIssuer.Assign(Source.FIssuer);
  FSerialNumber := Source.FSerialNumber;
  FSubjectKeyIdentifier := Source.FSubjectKeyIdentifier;
  FPublicKeyAlgorithmIdentifier.Assign(Source.FPublicKeyAlgorithmIdentifier);
  FPublicKey := Source.FPublicKey;
end;

procedure TScCMSOriginatorIdentifierOrKey.Parse(LexemInfo: TScLexemInfo);
begin
  FSerialNumber := '';
  FSubjectKeyIdentifier := '';
  FPublicKeyAlgorithmIdentifier.Clear;
  SetLength(FPublicKey, 0);
  TScASNUtils.Parse(FIssuer, nil); // Clear

  LexemInfo := LexemInfo.GetChoiceLexem;

  if SameText(LexemInfo.Name, 'IssuerAndSerialNumber') then begin
    FOriginatorIdentifierOrKeyType := oitIssuerAndSerialNumber;
    TScASNUtils.Parse(FIssuer, LexemInfo['Issuer']);
    FSerialNumber := LexemInfo['SerialNumber'].AsString;
  end
  else if SameText(LexemInfo.Name, 'SubjectKeyIdentifier') then begin
    FOriginatorIdentifierOrKeyType := oitSubjectKeyIdentifier;
    FSubjectKeyIdentifier := BytesToHexStr(LexemInfo.AsBytes);
  end
  else if SameText(LexemInfo.Name, 'PublicKey') then begin
    FOriginatorIdentifierOrKeyType := oitPublicKeyInfo;
    TScASNUtils.Parse(FPublicKeyAlgorithmIdentifier, LexemInfo['PublicKey']['Algorithm']);
    FPublicKey := LexemInfo['PublicKey']['PublicKey'].AsBytes;
  end
  else
    raise EScError.Create(seWrongDataFormat);
end;

function TScCMSOriginatorIdentifierOrKey.Encode: TBytes;
var
  ASN1Compiler: TScASN1Compiler;
begin
  ASN1Compiler := TScASN1Compiler.Create;
  try
    case FOriginatorIdentifierOrKeyType of
      oitIssuerAndSerialNumber: begin
        if not ASN1Compiler.Parse(asn1_ISSUER_AND_SERIAL_NUMBER_DESC) then
          raise EScError.Create(seWrongDataFormat);

        ASN1Compiler['Issuer'].EncodedData := FIssuer.Encode;
        ASN1Compiler['SerialNumber'].AsString := FSerialNumber;
      end;
      oitSubjectKeyIdentifier: begin
        if not ASN1Compiler.Parse(asn1_SUBJECT_KEY_IDENTIFIER_DESC) then
          raise EScError.Create(seWrongDataFormat);

        ASN1Compiler.Root.AsString := FSubjectKeyIdentifier;
      end;
      oitPublicKeyInfo: begin
        if not ASN1Compiler.Parse(asn1_PUBLIC_KEY_DESC) then
          raise EScError.Create(seWrongDataFormat);

        ASN1Compiler['Algorithm']['Algorithm'].AsString := FPublicKeyAlgorithmIdentifier.Algorithm.Value;
        if Length(FPublicKeyAlgorithmIdentifier.Parameters) > 0 then
          ASN1Compiler['Algorithm']['Parameters'].EncodedData := FPublicKeyAlgorithmIdentifier.Parameters;
        ASN1Compiler['PublicKey'].AsBytes := FPublicKey;
      end;
    end;

    Result := ASN1Compiler.Build;
  finally
    ASN1Compiler.Free;
  end;
end;

{ TScCMSSignedAttributes }

function TScCMSSignedAttributes.GetASN1Description: TScASN1Description;
begin
  Result := asn1_SIGNED_ATTRS_DESC;
end;

{ TScCMSUnsignedAttributes }

function TScCMSUnsignedAttributes.GetASN1Description: TScASN1Description;
begin
  Result := asn1_UNSIGNED_ATTRS_DESC;
end;

{ TScCMSSMIMEAttributes }

constructor TScCMSSMIMEAttributes.Create;
begin
  inherited;
end;

constructor TScCMSSMIMEAttributes.Create(const RawValue: TBytes);
begin
  inherited Create;

  Decode(RawValue);
end;

procedure TScCMSSMIMEAttributes.Decode(const RawValue: TBytes);
var
  ASN1Compiler: TScASN1Compiler;
  AttrsLexem, AttrLexem, AttrValueLexem: TScLexemInfo;
  Attribute: TScASN1Attribute;
  i: integer;
begin
  if Length(RawValue) = 0 then
    raise EScError.Create(seInvalidInputArgs);

  Clear;

  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_SMIME_ATTRIBUTE_DESC, RawValue) then
      raise EScError.Create(seWrongDataFormat);

    AttrsLexem := ASN1Compiler['Attrs'];
    for i := 0 to AttrsLexem.ValueCount - 1 do begin
      AttrLexem := AttrsLexem.Values[i];

      AttrValueLexem := AttrLexem['AttrValue'];
      Attribute := TScASN1Attribute.Create(AttrLexem['AttrType'].AsOID, AttrValueLexem.AsBytes, AttrValueLexem.DataType);
      Add(Attribute);
    end;
  finally
    ASN1Compiler.Free;
  end;
end;

function TScCMSSMIMEAttributes.Encode: TBytes;
var
  ASN1Compiler: TScASN1Compiler;
  AttrsLexem, AttrLexem, AttrValueLexem: TScLexemInfo;
  Attribute: TScASN1Attribute;
  i: integer;
begin
  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_SMIME_ATTRIBUTE_DESC) then
      raise EScError.Create(seWrongDataFormat);

    AttrsLexem := ASN1Compiler['Attrs'];
    AttrsLexem.ValueCount := Count;

    for i := 0 to Count - 1 do begin
      Attribute := Attributes[i];

      AttrLexem := AttrsLexem.Values[i];
      AttrValueLexem := AttrLexem['AttrValue'];
      AttrLexem['AttrType'].AsOID := Attribute.OId.Value;
      if (Attribute.ASN1DataType <> dtAny) or (Length(Attribute.RawData) > 0) then begin
        AttrValueLexem.DataType := Attribute.ASN1DataType;
        AttrValueLexem.AsBytes := Attribute.RawData;
      end;
    end;

    Result := ASN1Compiler.Build;
  finally
    ASN1Compiler.Free;
  end;
end;

{ TScCMSContentInfo }

constructor TScCMSContentInfo.Create;
begin
  inherited;

  FContentType := TScOId.Create(OID_DATA_TYPE);
  FContentStream := TScStreamInfo.Create(nil, 0, 0);
end;

destructor TScCMSContentInfo.Destroy;
begin
  FContentType.Free;
  FContentStream.Free;

  inherited;
end;

procedure TScCMSContentInfo.SetReadOnly(Value: boolean);
begin
  FReadOnly := Value;
  TScASNUtils.SetReadOnly(FContentType, Value);
end;

procedure TScCMSContentInfo.CheckReadOnly;
begin
  if FReadOnly then
    raise EScError.Create(seChangingReadOnlyObject);
end;

procedure TScCMSContentInfo.Clear;
begin
  FContentType.Value := OID_DATA_TYPE;
  FContentBuffer := nil;
  FContentStream.Init(nil, 0, 0);
end;

procedure TScCMSContentInfo.Init(ContentType: TScOId; const ContentBuffer: TBytes);
begin
  CheckReadOnly;

  if ContentType = nil then
    raise EScError.Create(seInvalidInputArgs);

  FContentType.Assign(ContentType);
  FContentBuffer := ContentBuffer;
  FContentStream.Init(nil, 0, 0);
end;

procedure TScCMSContentInfo.Init(ContentType: TScOId; ContentStream: TStream);
begin
  CheckReadOnly;

  if (ContentType = nil) or (ContentStream = nil) then
    raise EScError.Create(seInvalidInputArgs);

  FContentType.Assign(ContentType);
  FContentBuffer := nil;
  FContentStream.Init(ContentStream, ContentStream.Position, ContentStream.Size - ContentStream.Position);
end;

procedure TScCMSContentInfo.Init(const ContentBuffer: TBytes);
begin
  CheckReadOnly;

  FContentType.Value := OID_DATA_TYPE;
  FContentBuffer := ContentBuffer;
  FContentStream.Init(nil, 0, 0);
end;

procedure TScCMSContentInfo.Init(ContentStream: TStream);
begin
  CheckReadOnly;

  if ContentStream = nil then
    raise EScError.Create(seInvalidInputArgs);

  FContentType.Value := OID_DATA_TYPE;
  FContentBuffer := nil;
  FContentStream.Init(ContentStream, ContentStream.Position, ContentStream.Size - ContentStream.Position);
end;

procedure TScCMSContentInfo.Assign(Source: TScCMSContentInfo);
begin
  CheckReadOnly;

  if Source = nil then
    raise EScError.Create(seInvalidInputArgs);

  FContentType.Assign(Source.FContentType);
  FContentBuffer := Source.FContentBuffer;
  FContentStream.Assign(Source.FContentStream);
end;

function TScCMSContentInfo.GetContentData: TBytes;
begin
  if FContentStream.Stream <> nil then begin
    SetLength(Result, FContentStream.Count);
    FContentStream.Stream.Position := FContentStream.Position;
    FContentStream.Stream.ReadBuffer(Result[0], Length(Result));
  end
  else
    Result := FContentBuffer;
end;

{ TScCMSSignerInfo }

constructor TScCMSSignerInfo.Create;
begin
  inherited;

  FDigestAlgorithmIdentifier := TScASN1AlgorithmIdentifier.Create;
  FDigestAlgorithmIdentifier.Algorithm.Value := OID_SHA256;
  FSignerIdentifier := TScCMSSubjectIdentifier.Create;
  FSignedAttributes := TScCMSSignedAttributes.Create;
  FUnsignedAttributes := TScCMSUnsignedAttributes.Create;
  FSMIMEAttribute := TScCMSSMIMEAttributes.Create;

  FSignatureAlgorithmIdentifier := TScASN1AlgorithmIdentifier.Create;
  FSignatureAlgorithmIdentifier.InitAsRSA;

  FIncludedAttributes := [ciaContentType, ciaMessageDigest, ciaSigningTime];
end;

constructor TScCMSSignerInfo.Create(SignerIdentifierType: TScCMSSubjectIdentifierType; Certificate: TScCertificate);
begin
  Create;

  if Certificate = nil then
    raise EScError.Create(seInvalidInputArgs);

  FCertificate := Certificate;
  FSignerIdentifier.Init(SignerIdentifierType, FCertificate);
end;

constructor TScCMSSignerInfo.Create(Certificate: TScCertificate);
begin
  Create;

  if Certificate = nil then
    raise EScError.Create(seInvalidInputArgs);

  FCertificate := Certificate;
  FSignerIdentifier.Init(FCertificate);
end;

destructor TScCMSSignerInfo.Destroy;
begin
  FDigestAlgorithmIdentifier.Free;
  FSignerIdentifier.Free;
  FSignedAttributes.Free;
  FUnsignedAttributes.Free;
  FSMIMEAttribute.Free;
  FSignatureAlgorithmIdentifier.Free;

  inherited;
end;

function TScCMSSignerInfo.Clone: TScPersistent;
begin
  Result := TScCMSSignerInfo.Create;
  Result.Assign(Self);
end;

procedure TScCMSSignerInfo.Assign(Source: TScPersistent);
begin
  if (Source = nil) or not (Source is TScCMSSignerInfo) then
    RaiseAssignError(Source);

  FDigestAlgorithmIdentifier.Assign(TScCMSSignerInfo(Source).FDigestAlgorithmIdentifier);
  FSignerIdentifier.Assign(TScCMSSignerInfo(Source).FSignerIdentifier);
  FSignedAttributes.Assign(TScCMSSignerInfo(Source).FSignedAttributes);
  FUnsignedAttributes.Assign(TScCMSSignerInfo(Source).FUnsignedAttributes);
  FSignatureAlgorithmIdentifier.Assign(TScCMSSignature(Source).FSignatureAlgorithmIdentifier);

  FCertificate := TScCMSSignerInfo(Source).FCertificate;
  FIncludedAttributes := TScCMSSignerInfo(Source).FIncludedAttributes;
  FSMIMEAttribute.Assign(TScCMSSignerInfo(Source).FSMIMEAttribute);
end;

procedure TScCMSSignerInfo.SetReadOnly(Value: boolean);
begin
  FReadOnly := Value;
  TScASNUtils.SetReadOnly(FDigestAlgorithmIdentifier, Value);
  FSignerIdentifier.SetReadOnly(Value);
  TScASNUtils.SetReadOnly(FSignatureAlgorithmIdentifier, Value);
end;

procedure TScCMSSignerInfo.CheckReadOnly;
begin
  if FReadOnly then
    raise EScError.Create(seChangingReadOnlyObject);
end;

function TScCMSSignerInfo.GetVersion: integer;
begin
  case FSignerIdentifier.SubjectIdentifierType of
    sitIssuerAndSerialNumber:
      Result := 1;
    sitSubjectKeyIdentifier:
      Result := 3;
  else
    raise EScError.Create(seInvalidIdentifierType);
  end;
end;

function TScCMSSignerInfo.CalcHash(const Content: TBytes): TBytes;
var
  csp: THashAlgorithm;
begin
  csp := CipherFactory.CreateHash(GetDigestAlgorithm);
  try
    Result := csp.ComputeHash(Content);
  finally
    csp.Free;
  end;
end;

function TScCMSSignerInfo.CalcHash(Stream: TStream; Count: Int64 = 0): TBytes;
var
  csp: THashAlgorithm;
  TmpBuf: TBytes;
  BufCount: integer;
begin
  if Stream = nil then
    raise EScError.Create(seInvalidInputArgs);

  csp := CipherFactory.CreateHash(GetDigestAlgorithm);
  try
    if Count <= 0 then begin
      Stream.Position := 0;
      Count := Stream.Size;
    end;

    SetLength(TmpBuf, BUFFER_SIZE);

    while Count > 0 do begin
      if Count > BUFFER_SIZE then
        BufCount := BUFFER_SIZE
      else
        BufCount := Count;

      Stream.ReadBuffer(TmpBuf[0], BufCount);
      csp.TransformBlock(TmpBuf, 0, BufCount);
      Dec(Count, BufCount);
    end;
    csp.TransformFinalBlock(nil, 0, 0);
    Result := csp.Hash;
  finally
    csp.Free;
  end;
end;

procedure TScCMSSignerInfo.CheckHash(const Content: TBytes);
var
  ComputedHash, StoredHash: TBytes;
begin
  StoredHash := GetMessageDigest;
  if Length(StoredHash) = 0 then
    raise EScError.Create(seMessageDigestNotExists);

  ComputedHash := CalcHash(Content);

  if (Length(StoredHash) <> Length(ComputedHash)) or
     (MemCompare(@StoredHash[0], @ComputedHash[0], Length(ComputedHash)) <> 0) then
    raise EScError.Create(seInvalidMessageDigest);
end;

procedure TScCMSSignerInfo.CheckHash(Stream: TStream; Count: Int64 = 0);
var
  ComputedHash, StoredHash: TBytes;
begin
  StoredHash := GetMessageDigest;
  if Length(StoredHash) = 0 then
    raise EScError.Create(seMessageDigestNotExists);

  ComputedHash := CalcHash(Stream, Count);

  if (Length(StoredHash) <> Length(ComputedHash)) or
     (MemCompare(@StoredHash[0], @ComputedHash[0], Length(ComputedHash)) <> 0) then
    raise EScError.Create(seInvalidMessageDigest);
end;

procedure TScCMSSignerInfo.SetCertificate(Value: TScCertificate);
begin
  CheckReadOnly;
  FCertificate := Value;

  if FCertificate <> nil then
    FSignerIdentifier.Init(FCertificate);
end;

procedure TScCMSSignerInfo.SetDigestAlgorithmIdentifier(Value: TScASN1AlgorithmIdentifier);
begin
  CheckReadOnly;
  FDigestAlgorithmIdentifier.Assign(Value);
end;

function TScCMSSignerInfo.GetDigestAlgorithm: TScHashAlgorithm;
begin
  Result := CipherFactory.OidToHashAlgorithm(FDigestAlgorithmIdentifier.Algorithm.Value);
end;

procedure TScCMSSignerInfo.SetDigestAlgorithm(Value: TScHashAlgorithm);
begin
  CheckReadOnly;
  FDigestAlgorithmIdentifier.Algorithm.Value := CipherFactory.HashAlgorithmToOid(Value);
end;

procedure TScCMSSignerInfo.SetSignerIdentifier(Value: TScCMSSubjectIdentifier);
begin
  CheckReadOnly;
  FSignerIdentifier.Assign(Value);
end;

procedure TScCMSSignerInfo.SetSignatureAlgorithmIdentifier(Value: TScASN1AlgorithmIdentifier);
begin
  CheckReadOnly;
  FSignatureAlgorithmIdentifier.Assign(Value);
end;

function TScCMSSignerInfo.GetSignatureAlgorithm: TScSignatureAlgorithm;
begin
  Result := CipherFactory.OidToSignatureAlgorithm(FSignatureAlgorithmIdentifier.Algorithm.Value);
end;

procedure TScCMSSignerInfo.SetSignatureAlgorithm(Value: TScSignatureAlgorithm);
begin
  CheckReadOnly;
  FSignatureAlgorithmIdentifier.Algorithm.Value := CipherFactory.SignatureAlgorithmToOid(Value);
end;

procedure TScCMSSignerInfo.SetIncludedAttributes(const Value: TScCMSIncludedAttributes);
var
  OldIncludedAttributes: TScCMSIncludedAttributes;
begin
  if FIncludedAttributes <> Value then begin
    OldIncludedAttributes := FIncludedAttributes;
    FIncludedAttributes := Value;

    if not (ciaContentType in (OldIncludedAttributes * FIncludedAttributes)) then
      if ciaContentType in FIncludedAttributes then
        SetContentType(OID_DATA_TYPE)
      else
        SetContentType('');

    if not (ciaMessageDigest in (OldIncludedAttributes * FIncludedAttributes)) then
      if not (ciaMessageDigest in FIncludedAttributes) then
        SetMessageDigest(nil);

    if not (ciaSigningTime in (OldIncludedAttributes * FIncludedAttributes)) then
      if ciaSigningTime in FIncludedAttributes then
        SetSigningTime(Now)
      else
        SetSigningTime(0);
  end;
end;

function TScCMSSignerInfo.FindAttributeByOid(const OId: string): TScPKCS7Attribute;
var
  i: integer;
begin
  for i := 0 to FSignedAttributes.Count - 1 do begin
    Result := FSignedAttributes[i];
    if Result.OId.Value = OId then
      Exit;
  end;

  Result := nil;
end;

function TScCMSSignerInfo.GetContentType: string;
var
  PKCS7Attribute: TScPKCS7Attribute;
begin
  PKCS7Attribute := FindAttributeByOid(OID_CONTENT_TYPE);
  if PKCS7Attribute = nil then
    Result := ''
  else
    Result := PKCS7Attribute.Values[0].AsString;
end;

procedure TScCMSSignerInfo.SetContentType(const Value: string);
var
  PKCS7Attribute: TScPKCS7Attribute;
begin
  PKCS7Attribute := FindAttributeByOid(OID_CONTENT_TYPE);

  if Value <> '' then begin
    Include(FIncludedAttributes, ciaContentType);
    if PKCS7Attribute = nil then begin
      PKCS7Attribute := TScPKCS7Attribute.Create;
      FSignedAttributes.Add(PKCS7Attribute);
      PKCS7Attribute.OId.Value := OID_CONTENT_TYPE;
    end
    else
      PKCS7Attribute.ClearValues;

    PKCS7Attribute.AddValue(Value, dtObjectIdentifier);
  end
  else begin
    Exclude(FIncludedAttributes, ciaContentType);
    if PKCS7Attribute <> nil then
      FSignedAttributes.Remove(PKCS7Attribute);
  end;
end;

function TScCMSSignerInfo.GetMessageDigest: TBytes;
var
  PKCS7Attribute: TScPKCS7Attribute;
begin
  PKCS7Attribute := FindAttributeByOid(OID_MESSAGE_DIGEST);
  if PKCS7Attribute = nil then
    SetLength(Result, 0)
  else
    Result := PKCS7Attribute.Values[0].RawData;
end;

procedure TScCMSSignerInfo.SetMessageDigest(const Value: TBytes);
var
  PKCS7Attribute: TScPKCS7Attribute;
begin
  PKCS7Attribute := FindAttributeByOid(OID_MESSAGE_DIGEST);

  if Length(Value) > 0 then begin
    Include(FIncludedAttributes, ciaMessageDigest);
    if PKCS7Attribute = nil then begin
      PKCS7Attribute := TScPKCS7Attribute.Create;
      FSignedAttributes.Add(PKCS7Attribute);
      PKCS7Attribute.OId.Value := OID_MESSAGE_DIGEST;
    end
    else
      PKCS7Attribute.ClearValues;

    PKCS7Attribute.AddValue(Value, dtOctetString);
  end
  else begin
    Exclude(FIncludedAttributes, ciaMessageDigest);
    if PKCS7Attribute <> nil then
      FSignedAttributes.Remove(PKCS7Attribute);
  end;
end;

function TScCMSSignerInfo.GetSigningTime: TDateTime;
var
  PKCS7Attribute: TScPKCS7Attribute;
begin
  PKCS7Attribute := FindAttributeByOid(OID_SIGNING_TIME);
  if PKCS7Attribute = nil then
    Result := 0
  else
    Result := StrToDateTime(PKCS7Attribute.Values[0].AsString);
end;

procedure TScCMSSignerInfo.SetSigningTime(const Value: TDateTime);
var
  PKCS7Attribute: TScPKCS7Attribute;
begin
  PKCS7Attribute := FindAttributeByOid(OID_SIGNING_TIME);

  if Value <> 0 then begin
    Include(FIncludedAttributes, ciaSigningTime);
    if PKCS7Attribute = nil then begin
      PKCS7Attribute := TScPKCS7Attribute.Create;
      FSignedAttributes.Add(PKCS7Attribute);
      PKCS7Attribute.OId.Value := OID_SIGNING_TIME;
    end
    else
      PKCS7Attribute.ClearValues;

    PKCS7Attribute.AddValue(DateTimeToStr(Value), dtUTCTime);
  end
  else begin
    Exclude(FIncludedAttributes, ciaSigningTime);
    if PKCS7Attribute <> nil then
      FSignedAttributes.Remove(PKCS7Attribute);
  end;
end;

{ TScCMSSignature }

constructor TScCMSSignature.Create;
begin
  inherited;

  FContentStream := TScStreamInfo.Create(nil, 0, 0);
  SetReadOnly(True);
end;

destructor TScCMSSignature.Destroy;
begin
  FContentStream.Free;

  inherited;
end;

function TScCMSSignature.Clone: TScPersistent;
begin
  Result := TScCMSSignature.Create;

  TScCMSSignature(Result).SetReadOnly(False);
  try
    Result.Assign(Self);
  finally
    TScCMSSignature(Result).SetReadOnly(True);
  end;
end;

function TScCMSSignature.CalcContentHash(OnlyContentData: boolean = False): TBytes;
var
  csp: THashAlgorithm;
  DigestBuf: TBytes;
  TmpBuf: TBytes;
  Count: Int64;
  BufCount: integer;
begin
  SetLength(DigestBuf, 0);

  csp := CipherFactory.CreateHash(GetDigestAlgorithm);
  try
    if not OnlyContentData and (FSignedAttributes.Count > 0) then begin
      DigestBuf := FSignedAttributes.Encode;
      if Length(DigestBuf) = 0 then
        raise EScError.Create(seContentDataNotFound);

      DigestBuf[0] := $31; /// ASN1DataTypes[ctSet] - tools.ietf.org/html/rfc5652#section-5.4
      Result := csp.ComputeHash(DigestBuf);
    end
    else
    if (FContentStream.Stream <> nil) and (FContentStream.Count > 0) then begin
      SetLength(TmpBuf, BUFFER_SIZE);
      FContentStream.Stream.Position := FContentStream.Position;
      Count := FContentStream.Count;
      while Count > 0 do begin
        if Count > BUFFER_SIZE then
          BufCount := BUFFER_SIZE
        else
          BufCount := Count;

        FContentStream.Stream.ReadBuffer(TmpBuf[0], BufCount);
        csp.TransformBlock(TmpBuf, 0, BufCount);
        Dec(Count, BufCount);
      end;
      csp.TransformFinalBlock(nil, 0, 0);
      Result := csp.Hash;
    end
    else begin
      if Length(FContentBuffer) = 0 then
        raise EScError.Create(seContentDataNotFound);

      Result := csp.ComputeHash(FContentBuffer);
    end;
  finally
    csp.Free;
  end;
end;

procedure TScCMSSignature.GetSignatureAlgorithms(out SignHashAlg: TScHashAlgorithm;
  out Padding: TScPaddingMode);
var
  SignAlg: TScSignatureAlgorithm;
begin
  Assert(FCertificate <> nil);

  SignAlg := GetSignatureAlgorithm;
  if SignAlg = saRSA_PSS_Encryption then begin
    if Length(FSignatureAlgorithmIdentifier.Parameters) > 0 then
      FCertificate.Key.PSSParams.Decode(FSignatureAlgorithmIdentifier.Parameters);
    SignHashAlg := FCertificate.Key.PSSParams.HashAlgorithm;
    Padding := pmPSS;
  end
  else begin
    SignHashAlg := CipherFactory.GetHashAlgFromSignAlg(SignAlg);
    Padding := pmPKCS1;
  end;
  if SignHashAlg = haNone then
    SignHashAlg := GetDigestAlgorithm;
end;

procedure TScCMSSignature.CheckIncludedAttributes;
var
  PKCS7Attribute: TScPKCS7Attribute;
begin
  PKCS7Attribute := FindAttributeByOid(OID_CONTENT_TYPE);
  if ciaContentType in FIncludedAttributes then begin
    if PKCS7Attribute = nil then
      SetContentType(OID_DATA_TYPE);
  end
  else
    if PKCS7Attribute <> nil then
      FSignedAttributes.Remove(PKCS7Attribute);

  PKCS7Attribute := FindAttributeByOid(OID_MESSAGE_DIGEST);
  if ciaMessageDigest in FIncludedAttributes then begin
    if PKCS7Attribute = nil then
      SetMessageDigest(CalcContentHash(True));
  end
  else
    if PKCS7Attribute <> nil then
      FSignedAttributes.Remove(PKCS7Attribute);

  PKCS7Attribute := FindAttributeByOid(OID_SIGNING_TIME);
  if ciaSigningTime in FIncludedAttributes then begin
    if PKCS7Attribute = nil then
      SetSigningTime(Now);
  end
  else
    if PKCS7Attribute <> nil then
      FSignedAttributes.Remove(PKCS7Attribute);
end;

procedure TScCMSSignature.ComputeSignature;
var
  SignHashAlg: TScHashAlgorithm;
  Padding: TScPaddingMode;
  Hash: TBytes;
begin
  if FCertificate = nil then
    raise EScError.Create(seCertificateNotExists);
  if (FCertificate.Key.Algorithm <> aaEC) and (FCertificate.Key.BitCount < 1024) then
    raise EScError.Create(seSmallCertificateKeyLength);

  CheckIncludedAttributes;
  GetSignatureAlgorithms(SignHashAlg, Padding);
  Hash := CalcContentHash;
  FSignature := FCertificate.SignHash(Hash, SignHashAlg, Padding);
end;

procedure TScCMSSignature.CheckSignature;
var
  SignHashAlg: TScHashAlgorithm;
  Padding: TScPaddingMode;
  Hash, ContentHash, StoredHash: TBytes;
begin
  if FCertificate = nil then
    raise EScError.Create(seCertificateNotExists);
  if Length(FSignature) = 0 then
    raise EScError.Create(seSignatureNotFound);

  SetLength(StoredHash, 0);
  SetLength(ContentHash, 0);
  
  if ((FContentStream.Stream <> nil) and (FContentStream.Count > 0)) or (Length(FContentBuffer) > 0) then begin
    StoredHash := GetMessageDigest;
    if Length(StoredHash) > 0 then begin
      ContentHash := CalcContentHash(True);
      if (Length(StoredHash) <> Length(ContentHash)) or
         (MemCompare(@StoredHash[0], @ContentHash[0], Length(ContentHash)) <> 0) then
        raise EScError.Create(seInvalidMessageDigest);
    end;
  end;

  GetSignatureAlgorithms(SignHashAlg, Padding);
  Hash := CalcContentHash;
  if not FCertificate.VerifyHashSign(Hash, FSignature, SignHashAlg, Padding) then
    raise EScError.Create(seInvalidSignature);
end;

procedure TScCMSSignature.Parse(LexemInfo: TScLexemInfo);
var
  Certificate: TScCertificate;
  PKCS7Attribute: TScPKCS7Attribute;
  i: integer;
begin
  SetReadOnly(False);
  try
    FCertificate := nil;
    FIncludedAttributes := [];
    FSMIMEAttribute.Clear;

    FSignerIdentifier.Parse(LexemInfo['Sid']);
    TScASNUtils.Parse(FDigestAlgorithmIdentifier, LexemInfo['DigestAlgorithm']);
    TScASNUtils.Parse(FSignatureAlgorithmIdentifier, LexemInfo['SignatureAlgorithm']);
    FSignedAttributes.Parse(LexemInfo['SignedAttrs']);
    FUnsignedAttributes.Parse(LexemInfo['UnsignedAttrs']);
    FSignature := LexemInfo['Signature'].AsBytes;

    for i := 0 to FSignedAttributes.Count - 1 do begin
      PKCS7Attribute := FSignedAttributes[i];

      if (PKCS7Attribute.OId.Value = OID_CONTENT_TYPE) and (PKCS7Attribute.ValueCount > 0) then
        FIncludedAttributes := FIncludedAttributes + [ciaContentType]
      else
      if (PKCS7Attribute.OId.Value = OID_MESSAGE_DIGEST) and (PKCS7Attribute.ValueCount > 0) then
        FIncludedAttributes := FIncludedAttributes + [ciaMessageDigest]
      else
      if (PKCS7Attribute.OId.Value = OID_SIGNING_TIME) and (PKCS7Attribute.ValueCount > 0) then
        FIncludedAttributes := FIncludedAttributes + [ciaSigningTime]
      else
      if (PKCS7Attribute.OId.Value = OID_SMIME_CAPABILITIES) and (PKCS7Attribute.ValueCount > 0) then begin
        FIncludedAttributes := FIncludedAttributes + [ciaSMIMEAttribute];
        FSMIMEAttribute.Decode(PKCS7Attribute.Values[0].RawData);
      end;
    end;

    if FCertificateListRef <> nil then begin
      if FSignerIdentifier.SubjectIdentifierType = sitSubjectKeyIdentifier then begin
        for i := 0 to FCertificateListRef.Count - 1 do begin
          Certificate := FCertificateListRef[i];
          if SameText(FSignerIdentifier.SubjectKeyIdentifier, Certificate.SubjectKeyIdentifier) then begin
            FCertificate := Certificate;
            Break;
          end;
        end;
      end
      else begin
        for i := 0 to FCertificateListRef.Count - 1 do begin
          Certificate := FCertificateListRef[i];
          if (FSignerIdentifier.SerialNumber = Certificate.SerialNumber) and
              FSignerIdentifier.Issuer.Equals(Certificate.IssuerName)
          then begin
            FCertificate := Certificate;
            Break;
          end;
        end;
      end;
    end;
  finally
    SetReadOnly(True);
  end;
end;

function TScCMSSignature.Encode: TBytes;
var
  ASN1Compiler: TScASN1Compiler;
  SignAlg: TScSignatureAlgorithm;
begin
  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_SIGNER_INFO_DESC) then
      raise EScError.Create(seWrongDataFormat);

    ASN1Compiler['Version'].AsInteger := GetVersion;
    ASN1Compiler['Sid'].EncodedData := FSignerIdentifier.Encode;

    ASN1Compiler['DigestAlgorithm']['Algorithm'].AsString := FDigestAlgorithmIdentifier.Algorithm.Value;
    if Length(FDigestAlgorithmIdentifier.Parameters) > 0 then
      ASN1Compiler['DigestAlgorithm']['Parameters'].EncodedData := FDigestAlgorithmIdentifier.Parameters;

    ASN1Compiler['SignedAttrs'].EncodedData := FSignedAttributes.Encode;

    ASN1Compiler['SignatureAlgorithm']['Algorithm'].AsString := FSignatureAlgorithmIdentifier.Algorithm.Value;
    SignAlg := GetSignatureAlgorithm;
    if SignAlg = saRSA_PSS_Encryption then begin
      if Length(FSignatureAlgorithmIdentifier.Parameters) > 0 then
        ASN1Compiler['SignatureAlgorithm']['Parameters'].EncodedData := FSignatureAlgorithmIdentifier.Parameters
      else
      if FCertificate <> nil then
        ASN1Compiler['SignatureAlgorithm']['Parameters'].EncodedData := FCertificate.Key.PSSParams.Encode
      else
        ASN1Compiler['SignatureAlgorithm']['Parameters'].DataType := dtNull;
    end
    else
    if SignAlg = saRSA_Encryption then
      ASN1Compiler['SignatureAlgorithm']['Parameters'].DataType := dtNull;

    ASN1Compiler['Signature'].AsBytes := FSignature;
    if FUnsignedAttributes.Count > 0 then
      ASN1Compiler['UnsignedAttrs'].EncodedData := FUnsignedAttributes.Encode;

    Result := ASN1Compiler.Build;
  finally
    ASN1Compiler.Free;
  end;
end;

{ TScCMSSignatures }

function TScCMSSignatures.GetItemClassType: TScPersistentClass;
begin
  Result := TScCMSSignature;
end;

function TScCMSSignatures.GetSignatures(Index: integer): TScCMSSignature;
begin
  Result := TObject(Items[Index]) as TScCMSSignature;
end;

procedure TScCMSSignatures.SetSignatures(Index: integer; Item: TScCMSSignature);
begin
  Items[Index] := Item;
end;

{ TScCMSData }

constructor TScCMSData.Create;
begin
  inherited;

  FCertificates := TScCertificateList.Create(nil);

  SetLength(FTmpBase64Buf, (((DER_BUFFER_SIZE + DER_LINE_LENGTH) div 3) * 4) + (DER_BUFFER_SIZE div DER_LINE_LENGTH){EOL});
  FCMSEncoding := ceDER;
end;

destructor TScCMSData.Destroy;
begin
  ClearCertificateList;
  FCertificates.Free;

  inherited;
end;

procedure TScCMSData.ClearCertificateList;
var
  Cert: TScCertificate;
  i: integer;
begin
  // we can not call FCertificates.Clear, because it frees all certificates,
  // but FCertificates can contain user's ceriticates

  for i := FCertificates.Count - 1 downto 0 do begin
    Cert := FCertificates[i];
    FCertificates.Remove(Cert);
    if Copy(Cert.CertName, 1, Length(CERTIFICATE_PREFIX)) = CERTIFICATE_PREFIX then
      Cert.Free;
  end;
end;

function TScCMSData.GetHeader: TBytes;
begin
  if FCMSEncoding = cePEM then
    Result := Encoding.Default.GetBytes(CMS_HEADER + #$0A)
  else
    SetLength(Result, 0);
end;

function TScCMSData.GetFooter: TBytes;
begin
  if FCMSEncoding = cePEM then
    Result := Encoding.Default.GetBytes(CMS_FOOTER + #$0A)
  else
    SetLength(Result, 0);
end;

procedure TScCMSData.WriteHeader(OutStream: TStream);
var
  buf: TBytes;
begin
  SetLength(buf, 0);

  if FCMSEncoding = cePEM then begin
    buf := Encoding.Default.GetBytes(CMS_HEADER + #$0A);
    OutStream.WriteBuffer(buf[0], Length(buf));
  end;
end;

procedure TScCMSData.WriteFooter(OutStream: TStream);
var
  buf: TBytes;
begin
  SetLength(buf, 0);

  if FCMSEncoding = cePEM then begin
    buf := Encoding.Default.GetBytes(CMS_FOOTER + #$0A);
    OutStream.WriteBuffer(buf[0], Length(buf));
  end;
end;

function TScCMSData.EncodeBlock(const Data: TBytes; Offset, Count: integer): TBytes;
var
  OutOffset, OutCount: integer;
begin
  if FCMSEncoding = ceDER then begin
    Result := Data;
    Exit;
  end
  else begin
    if (Count mod 3) <> 0 then
      OutCount := (Count div 3) * 4 + 4
    else
      OutCount := (Count div 3) * 4;
    OutCount := OutCount + (OutCount + 1 div PEM_LINE_LENGTH){EOL};
    SetLength(Result, OutCount);

    OutOffset := 0;
    while Count >= DER_LINE_LENGTH do begin
      TBase64.InternalEncode(Data, Offset, DER_LINE_LENGTH, Result, OutOffset, OutCount);
      Assert(OutCount = PEM_LINE_LENGTH);
      Result[OutOffset + PEM_LINE_LENGTH] := $0A;
      Dec(Count, DER_LINE_LENGTH);
      Inc(Offset, DER_LINE_LENGTH);
      Inc(OutOffset, PEM_LINE_LENGTH + 1{EOL});
    end;

    if Count > 0 then begin
      TBase64.InternalEncode(Data, Offset, Count, Result, OutOffset, OutCount);
      Result[OutOffset + OutCount] := $0A;
    end;
  end;
end;

procedure TScCMSData.EncodeBlock(const Data: TBytes; Offset, Count: integer;
  OutStream: TStream; IsLastBlock: boolean; out UnReadCount: integer);
var
  OutOffset, OutCount: integer;
  MaxBlockSize: integer;
begin
  if FCMSEncoding = ceDER then begin
    if Count > 0 then
      OutStream.WriteBuffer(Data[Offset], Count);
    UnReadCount := 0;
  end
  else begin
    MaxBlockSize := Length(FTmpBase64Buf) - (PEM_LINE_LENGTH + 1{EOL});
    OutOffset := 0;
    while Count >= DER_LINE_LENGTH do begin
      TBase64.InternalEncode(Data, Offset, DER_LINE_LENGTH, FTmpBase64Buf, OutOffset, OutCount);
      Assert(OutCount = PEM_LINE_LENGTH);
      FTmpBase64Buf[OutOffset + PEM_LINE_LENGTH] := $0A;
      Dec(Count, DER_LINE_LENGTH);
      Inc(Offset, DER_LINE_LENGTH);
      Inc(OutOffset, PEM_LINE_LENGTH + 1{EOL});

      if OutOffset >= MaxBlockSize then begin
        OutStream.WriteBuffer(FTmpBase64Buf[0], OutOffset);
        OutOffset := 0;
      end;
    end;

    if OutOffset > 0 then
      OutStream.WriteBuffer(FTmpBase64Buf[0], OutOffset);

    if IsLastBlock then begin
      if Count > 0 then begin
        TBase64.InternalEncode(Data, Offset, Count, FTmpBase64Buf, 0, OutCount);
        FTmpBase64Buf[OutCount] := $0A;
        OutStream.WriteBuffer(FTmpBase64Buf[0], OutCount + 1{EOL});
      end;
      UnReadCount := 0;
    end
    else
      UnReadCount := Count;
  end;
end;

class function TScCMSData.DecodeData(const Data: TBytes; out CMSEncoding: TScCMSEncoding): TBytes;
var
  s: string;
  Offset, PrevOffset, Count: integer;
  OutOffset: integer;
  DecCount, UnReadCount: integer;
  FooterLength, LineLength, EOLLength: integer;
  IsFinised: boolean;
begin
  if Length(Data) = 0 then
    raise EScError.Create(seInvalidInputArgs);

  Count := Length(Data);

  if Count > (Length(CMS_HEADER) * 2 + 2) then begin
    s := Encoding.Default.GetString(Data, 0, Length(CMS_HEADER));

    if SameText(s, CMS_HEADER) then
      CMSEncoding := cePEM
    else
    if SameText(Copy(s, 1, Length(CMS_MIME_HEADER)), CMS_MIME_HEADER) then
      CMSEncoding := ceSMIME
    else begin
      CMSEncoding := ceDER;
      Result := Data;
      Exit;
    end;

    // reading header
    SetLength(Result, (Count * 3) div 4);
    OutOffset := 0;

    if CMSEncoding = cePEM then begin
      Offset := Length(CMS_HEADER);
      if (Data[Offset] = $0D) and (Data[Offset + 1] = $0A) then
        EOLLength := 2
      else
      if (Data[Offset] = $0D) or (Data[Offset] = $0A) then
        EOLLength := 1
      else
        raise EScError.Create(seWrongDataFormat);

      Inc(Offset, EOLLength);
      FooterLength := Length(CMS_FOOTER) + EOLLength;
    end
    else begin
      Assert(CMSEncoding = ceSMIME);

      Offset := Length(CMS_MIME_HEADER);
      while Offset + 1 < Count do begin
        if (Data[Offset] = $0A) and (Data[Offset + 1] = $0A) then
          break;
        Inc(Offset);
      end;
      Inc(Offset, 2);

      if Offset >= Count then
        raise EScError.Create(seWrongDataFormat);

      EOLLength := 1;
      FooterLength := 1{EOL};
    end;

    // decoding data
    LineLength := PEM_LINE_LENGTH + EOLLength;
    IsFinised := False;
    while Count > (Offset + LineLength + FooterLength) do begin
      if not IsFinised then begin
        TBase64.InternalDecode(Data, Offset, PEM_LINE_LENGTH, Result, OutOffset, DecCount, UnReadCount, IsFinised);
        Inc(OutOffset, DecCount);
      end;
      Inc(Offset, LineLength);
    end;

    PrevOffset := Offset;
    while Offset < Count do begin
      if (Data[Offset] = $0D) or (Data[Offset] = $0A) then
        break;
      Inc(Offset);
    end;

    if (Offset - PrevOffset > 0) and not IsFinised then begin
      TBase64.InternalDecode(Data, PrevOffset, Offset - PrevOffset, Result, OutOffset, DecCount, UnReadCount, IsFinised);
      Inc(OutOffset, DecCount);
    end;
    SetLength(Result, OutOffset);

    // reading footer
    if CMSEncoding = cePEM then begin
      Inc(Offset, EOLLength);
      PrevOffset := Offset;
      while Offset < Count do begin
        if (Data[Offset] = $0D) or (Data[Offset] = $0A) then
          break;
        Inc(Offset);
      end;

      if (Offset - PrevOffset) > 0 then
        s := Encoding.Default.GetString(Data, PrevOffset, Offset - PrevOffset)
      else
        s := '';
      if not SameText(s, CMS_FOOTER) then
        raise EScError.Create(seWrongDataFormat);
    end;
  end
  else
    raise EScError.Create(seWrongDataFormat);
end;

class procedure TScCMSData.DecodeData(InStream, OutStream: TStream; out CMSEncoding: TScCMSEncoding);
var
  s: string;
  Buf, DecBuf: TBytes;
  OldPosition, Count: Int64;
  Offset, PrevOffset: integer;
  BufSize, BufCount, DecOffet, DecCount, UnReadCount: integer;
  FooterLength, LineLength, EOLLength: integer;
  IsFinised: boolean;
  i: integer;
begin
  if (InStream = nil) or (OutStream = nil) or (InStream = OutStream) then
    raise EScError.Create(seInvalidInputArgs);

  Count := InStream.Size - InStream.Position;

  if Count > (Length(CMS_HEADER) * 2 + 2) then begin
    OldPosition := InStream.Position;

    SetLength(Buf, (PEM_LINE_LENGTH + 2{EOL}) * 1024);
    InStream.ReadBuffer(Buf[0], Length(CMS_HEADER) + 2);
    s := Encoding.Default.GetString(Buf, 0, Length(CMS_HEADER));

    if SameText(s, CMS_HEADER) then
      CMSEncoding := cePEM
    else
    if SameText(Copy(s, 1, Length(CMS_MIME_HEADER)), CMS_MIME_HEADER) then
      CMSEncoding := ceSMIME
    else begin
      CMSEncoding := ceDER;
      InStream.Position := OldPosition;
      Exit;
    end;

    // reading header
    if CMSEncoding = cePEM then begin
      Offset := Length(CMS_HEADER);
      if (Buf[Offset] = $0D) and (Buf[Offset + 1] = $0A) then
        EOLLength := 2
      else
      if (Buf[Offset] = $0D) or (Buf[Offset] = $0A) then begin
        EOLLength := 1;
        InStream.Position := InStream.Position - 1;
      end
      else
        raise EScError.Create(seWrongDataFormat);

      Dec(Count, Offset + EOLLength);
      FooterLength := Length(CMS_FOOTER) + EOLLength;
    end
    else begin
      Assert(CMSEncoding = ceSMIME);
      InStream.Position := OldPosition;

      while Count > 1 do begin
        InStream.ReadBuffer(Buf[0], PEM_LINE_LENGTH);
        i := 0;
        while i < PEM_LINE_LENGTH do begin
          if (Buf[i] = $0A) and (Buf[i + 1] = $0A) then
            break;
          Inc(i);
        end;
        Dec(Count, i);
        if i < PEM_LINE_LENGTH then begin
          InStream.Position := InStream.Position - (PEM_LINE_LENGTH - i) + 2;
          break;
        end;
      end;
      Dec(Count, 2);
      if Count <= 0 then
        raise EScError.Create(seWrongDataFormat);

      EOLLength := 1;
      FooterLength := 0;
    end;

    // decoding data
    LineLength := PEM_LINE_LENGTH + EOLLength;
    BufSize := LineLength * 1024;
    Assert(Length(Buf) >= BufSize);
    SetLength(DecBuf, DER_LINE_LENGTH * 1024);

    IsFinised := False;
    while Count > (LineLength + FooterLength) do begin
      if Count > BufSize + FooterLength then
        BufCount := BufSize
      else
        BufCount := ((Count - FooterLength) div LineLength) * LineLength;

      InStream.ReadBuffer(Buf[0], BufCount);
      DecOffet := 0;
      Offset := 0;

      while (Offset < BufCount) and not IsFinised do begin
        TBase64.InternalDecode(Buf, Offset, PEM_LINE_LENGTH, DecBuf, DecOffet, DecCount, UnReadCount, IsFinised);
        Inc(DecOffet, DecCount);
        Inc(Offset, LineLength);
      end;

      OutStream.WriteBuffer(DecBuf[0], DecOffet);
      Dec(Count, BufCount);
    end;

    InStream.ReadBuffer(Buf[0], Count);
    Offset := 0;
    if Count > FooterLength then begin
      while Offset < Count do begin
        if (Buf[Offset] = $0D) or (Buf[Offset] = $0A) then
          break;
        Inc(Offset);
      end;

      if (Offset > 0) and not IsFinised then begin
        TBase64.InternalDecode(Buf, 0, Offset, DecBuf, 0, DecCount, UnReadCount, IsFinised);
        OutStream.WriteBuffer(DecBuf[0], DecCount);
      end;

      Inc(Offset, EOLLength);
    end;

    // reading footer
    if CMSEncoding = cePEM then begin
      while (Buf[Offset] = $0D) or (Buf[Offset] = $0A) do
        Inc(Offset);

      PrevOffset := Offset;
      while Offset < Count do begin
        if (Buf[Offset] = $0D) or (Buf[Offset] = $0A) then
          break;
        Inc(Offset);
      end;

      if (Offset - PrevOffset) > 0 then
        s := Encoding.Default.GetString(Buf, PrevOffset, Offset - PrevOffset)
      else
        s := '';
      if not SameText(s, CMS_FOOTER) then
        raise EScError.Create(seWrongDataFormat);
    end;
  end
  else
    raise EScError.Create(seWrongDataFormat);
end;

{ TScCMSSignedData }

constructor TScCMSSignedData.Create;
begin
  inherited;

  FContentInfo := TScCMSContentInfo.Create;
  FDigestAlgorithms := TStringList.Create;
  FDigestAlgorithms.Sorted := True;
  FSignatures := TScCMSSignatures.Create;

  SetReadOnly(True);
end;

destructor TScCMSSignedData.Destroy;
begin
  FContentInfo.Free;
  FDigestAlgorithms.Free;
  FSignatures.Free;

  inherited;
end;

procedure TScCMSSignedData.SetReadOnly(Value: boolean);
begin
  FContentInfo.SetReadOnly(Value);
end;

procedure TScCMSSignedData.Init(ContentInfo: TScCMSContentInfo);
begin
  if ContentInfo = nil then
    raise EScError.Create(seInvalidInputArgs);

  SetReadOnly(False);
  try
    FContentInfo.Assign(ContentInfo);
    FSignatures.Clear;
  finally
    SetReadOnly(True);
  end;
end;

procedure TScCMSSignedData.Init(const ContentBuffer: TBytes);
begin
  SetReadOnly(False);
  try
    FContentInfo.Init(ContentBuffer);
    FSignatures.Clear;
  finally
    SetReadOnly(True);
  end;
end;

procedure TScCMSSignedData.Init(ContentStream: TStream);
begin
  if ContentStream = nil then
    raise EScError.Create(seInvalidInputArgs);

  SetReadOnly(False);
  try
    FContentInfo.Init(ContentStream);
    FSignatures.Clear;
  finally
    SetReadOnly(True);
  end;
end;

function TScCMSSignedData.FindSignatureInfo(Certificate: TScCertificate): TScCMSSignature;
var
  i: integer;
begin
  if Certificate = nil then
    raise EScError.Create(seInvalidInputArgs);

  for i := 0 to FSignatures.Count - 1 do begin
    Result := FSignatures[i];
    if Result.SignerIdentifier.CheckCertificateMatching(Certificate) then
      Exit;
  end;

  raise EScError.Create(seRecipientInfoNotFound);
end;

procedure TScCMSSignedData.ComputeSignature(SignerInfo: TScCMSSignerInfo);
var
  Signature: TScCMSSignature;
  Cert: TScCertificate;
  i: integer;
begin
  if (SignerInfo = nil) or (SignerInfo.Certificate = nil) then
    raise EScError.Create(seInvalidInputArgs);

  Signature := TScCMSSignature.Create;
  try
    Signature.SetReadOnly(False);
    Signature.Assign(SignerInfo);
    Signature.FContentBuffer := FContentInfo.FContentBuffer;
    Signature.FContentStream.Assign(FContentInfo.FContentStream);

    Signature.ComputeSignature;
    Signature.SetReadOnly(True);
  except
    Signature.Free;
    raise;
  end;

  FSignatures.Add(Signature);

  Cert := nil;
  for i := 0 to FCertificates.Count - 1 do begin
    Cert := FCertificates[i];
    if (Cert.SerialNumber = SignerInfo.Certificate.SerialNumber) and Cert.IssuerName.Equals(SignerInfo.Certificate.IssuerName) then
      Break
    else
      Cert := nil;
  end;
  if Cert = nil then
    FCertificates.Add(SignerInfo.Certificate);
end;

procedure TScCMSSignedData.CheckSignature(Certificate: TScCertificate);
var
  Signature: TScCMSSignature;
begin
  if Certificate = nil then
    raise EScError.Create(seInvalidInputArgs);

  Signature := FindSignatureInfo(Certificate);
  Signature.FCertificate := Certificate;
  Signature.CheckSignature;
end;

procedure TScCMSSignedData.Decode(const RawData: TBytes);
var
  ASN1Compiler: TScASN1Compiler;
begin
  if Length(RawData) = 0 then
    raise EScError.Create(seInvalidInputArgs);

  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_SIGNED_DATA_DESC, RawData) then
      raise EScError.Create(seWrongDataFormat);

    Decode(ASN1Compiler, False);
  finally
    ASN1Compiler.Free;
  end;
end;

procedure TScCMSSignedData.Decode(Stream: TStream);
var
  ASN1Compiler: TScASN1Compiler;
begin
  if Stream = nil then
    raise EScError.Create(seInvalidInputArgs);

  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_SIGNED_DATA_DESC, Stream) then
      raise EScError.Create(seWrongDataFormat);

    Decode(ASN1Compiler, True);
  finally
    ASN1Compiler.Free;
  end;
end;

procedure TScCMSSignedData.Decode(ASN1Compiler: TScASN1Compiler; IsStreamUsed: boolean);
var
  DigestLexem, EncapContentInfoLexem: TScLexemInfo;
  CertificatesLexem, SignerInfosLexem: TScLexemInfo;
  Cert: TScCertificate;
  Signature: TScCMSSignature;
  ContentType: string;
  i: integer;
begin
  SetReadOnly(False);
  try
    ClearCertificateList;
    FDigestAlgorithms.Clear;
    FContentInfo.Clear;
    FSignatures.Clear;

    ContentType := ASN1Compiler['ContentType'].AsString;
    if ContentType <> OID_SIGNED_DATA_TYPE then
      raise EScError.Create(seNotSignedData);

    DigestLexem := ASN1Compiler['DigestAlgorithms'];
    for i := 0 to DigestLexem.ValueCount - 1 do begin
      /// AlgIdent := TScASN1AlgorithmIdentifier.Create;
      /// FDigestAlgorithms.Add(AlgIdent);
      /// TScASNUtils.Parse(AlgIdent, DigestLexem.Values[i]);
      FDigestAlgorithms.Add(DigestLexem.Values[i]['Algorithm'].AsString);
    end;

    EncapContentInfoLexem := ASN1Compiler['EncapContentInfo'];
    FContentInfo.ContentType.Value := EncapContentInfoLexem['eContentType'].AsString;
    if IsStreamUsed then
      FContentInfo.FContentStream.Assign(EncapContentInfoLexem['eContent'].AsStreamInfo)
    else
      FContentInfo.FContentBuffer := EncapContentInfoLexem['eContent'].AsBytes;

    CertificatesLexem := ASN1Compiler['Certificates'];
    for i := 0 to CertificatesLexem.ValueCount - 1 do begin
      if SameText(CertificatesLexem.Values[i]['CertificateDesc'].Name, 'Certificate') then begin
        Cert := TScCertificate.Create(FCertificates);
        Cert.CertName := CERTIFICATE_PREFIX + IntToStr(FCertificates.Count);
        Cert.SetRawData(nil, nil, CertificatesLexem.Values[i].AsBytes);
      end
      else
        raise EScError.Create(seUnknownCertFormat);
    end;

    SignerInfosLexem := ASN1Compiler['SignerInfos'];
    for i := 0 to SignerInfosLexem.ValueCount - 1 do begin
      Signature := TScCMSSignature.Create;
      FSignatures.Add(Signature);
      Signature.FCertificateListRef := FCertificates;
      if IsStreamUsed then
        Signature.FContentStream.Assign(FContentInfo.FContentStream)
      else
        Signature.FContentBuffer := FContentInfo.FContentBuffer;

      Signature.Parse(SignerInfosLexem.Values[i]);
    end;
  finally
    SetReadOnly(True);
  end;
end;

procedure TScCMSSignedData.EncodeContentData(OutStream: TStream;
  const PrefixData: TBytes; PrefixOffset, PrefixCount: integer; out PostfixData: TBytes);
var
  TmpBuf: TBytes;
  Count: Int64;
  UnReadCount: integer;
begin
  if (OutStream = nil) or (FContentInfo.FContentStream.Stream = nil) then
    raise EScError.Create(seInvalidInputArgs);

  if PrefixCount > DER_LINE_LENGTH then
    raise EScError.Create(seInvalidInputArgs);

  SetLength(TmpBuf, DER_BUFFER_SIZE + DER_LINE_LENGTH);
  if PrefixCount > 0 then
    Move(PrefixData[PrefixOffset], TmpBuf[0], PrefixCount);
  UnReadCount := PrefixCount;

  FContentInfo.FContentStream.Stream.Position := FContentInfo.FContentStream.Position;
  Count := FContentInfo.FContentStream.Count + UnReadCount;

  while Count >= DER_BUFFER_SIZE do begin
    FContentInfo.FContentStream.Stream.ReadBuffer(TmpBuf[UnReadCount], DER_BUFFER_SIZE - UnReadCount);
    EncodeBlock(TmpBuf, 0, DER_BUFFER_SIZE, OutStream, False, UnReadCount);
    Assert(UnReadCount = 0);
    Dec(Count, DER_BUFFER_SIZE);
  end;

  if Count > 0 then begin
    FContentInfo.FContentStream.Stream.ReadBuffer(TmpBuf[UnReadCount], Count - UnReadCount);
    EncodeBlock(TmpBuf, 0, Count, OutStream, False, UnReadCount);
  end;

  if UnReadCount > 0 then
    Move(TmpBuf[Count - UnReadCount], TmpBuf[0], UnReadCount);

  SetLength(TmpBuf, UnReadCount);
  PostfixData := TmpBuf;
end;

function TScCMSSignedData.Encode(IncludeContent: boolean = False): TBytes;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Encode(Stream, IncludeContent);
    Stream.Position := 0;
    SetLength(Result, Stream.Size);
    Stream.ReadBuffer(Result[0], Length(Result));
  finally
    Stream.Free;
  end;
end;

procedure TScCMSSignedData.Encode(Stream: TStream; IncludeContent: boolean = False);
var
  ASN1Compiler: TScASN1Compiler;
  EncapContentInfoLexem, DigestLexem, CertificatesLexem, SignerInfosLexem: TScLexemInfo;
  Signature: TScCMSSignature;
  DigestAlg: string;
  MaxVer: integer;
  ASN1Buf, PostfixData: TBytes;
  ASN1ContentOffset, UnReadCount, PostfixDataLen: integer;
  i: integer;
begin
  if Stream = nil then
    raise EScError.Create(seInvalidInputArgs);

  if FSignatures.Count = 0 then
    raise EScError.Create(seDataNotSigned);

  SetLength(ASN1Buf, 0);

  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_SIGNED_DATA_DESC) then
      raise EScError.Create(seWrongDataFormat);

    ASN1Compiler['ContentType'].AsString := OID_SIGNED_DATA_TYPE;

    if FContentInfo.ContentType.Value = OID_DATA_TYPE then
      MaxVer := 1
    else
      MaxVer := 3;

    EncapContentInfoLexem := ASN1Compiler['EncapContentInfo'];
    EncapContentInfoLexem['eContentType'].AsString := FContentInfo.ContentType.Value;
    if IncludeContent then begin
      if (FContentInfo.FContentStream.Stream <> nil) and (FContentInfo.FContentStream.Count > 0) then
        EncapContentInfoLexem['eContent'].PreDefinedSize := FContentInfo.FContentStream.Count
      else
      if Length(FContentInfo.FContentBuffer) > 0 then
        EncapContentInfoLexem['eContent'].AsBytes := FContentInfo.FContentBuffer;
    end;

    CertificatesLexem := ASN1Compiler['Certificates'];
    CertificatesLexem.ValueCount := FCertificates.Count;
    for i := 0 to FCertificates.Count - 1 do
      CertificatesLexem.Values[i].AsBytes := FCertificates[i].GetRawData;

    FDigestAlgorithms.Clear;
    SignerInfosLexem := ASN1Compiler['SignerInfos'];
    SignerInfosLexem.ValueCount := FSignatures.Count;
    for i := 0 to FSignatures.Count - 1 do begin
      Signature := FSignatures[i];

      DigestAlg := Signature.FDigestAlgorithmIdentifier.Algorithm.Value;
      if FDigestAlgorithms.IndexOf(DigestAlg) = -1 then
        FDigestAlgorithms.Add(DigestAlg);

      if Signature.GetVersion > MaxVer then
        MaxVer := Signature.GetVersion;
      SignerInfosLexem.Values[i].AsBytes := Signature.Encode;
    end;

    DigestLexem := ASN1Compiler['DigestAlgorithms'];
    DigestLexem.ValueCount := FDigestAlgorithms.Count;
    for i := 0 to FDigestAlgorithms.Count - 1 do
      DigestLexem.Values[i]['Algorithm'].AsString := FDigestAlgorithms[i]; /// FDigestAlgorithms[i].Algorithm.Value;

    ASN1Compiler['Version'].AsInteger := MaxVer;

    ASN1Buf := ASN1Compiler.Build;

    WriteHeader(Stream);

    if IncludeContent and (FContentInfo.FContentStream.Stream <> nil) and (FContentInfo.FContentStream.Count > 0) then begin
      ASN1ContentOffset := EncapContentInfoLexem['eContent'].GetOffset;
      EncodeBlock(ASN1Buf, 0, ASN1ContentOffset, Stream, False, UnReadCount);

      EncodeContentData(Stream, ASN1Buf, ASN1ContentOffset - UnReadCount, UnReadCount, PostfixData);
      PostfixDataLen := Length(PostfixData);
      if PostfixDataLen > 0 then begin
        if PostfixDataLen > ASN1ContentOffset then
          raise EScError.Create(seInternalError);

        Move(PostfixData[0], ASN1Buf[ASN1ContentOffset - PostfixDataLen], PostfixDataLen);
      end;

      EncodeBlock(ASN1Buf, ASN1ContentOffset - PostfixDataLen, Length(ASN1Buf) - ASN1ContentOffset + PostfixDataLen, Stream, True, UnReadCount);
    end
    else
      EncodeBlock(ASN1Buf, 0, Length(ASN1Buf), Stream, True, UnReadCount);

    WriteFooter(Stream);
  finally
    ASN1Compiler.Free;
  end;
end;

{ TScCMSRecipient }

constructor TScCMSRecipient.Create;
begin
  inherited Create;

  FCertificate := nil;
  FRecipientIdentifierType := sitIssuerAndSerialNumber;
end;

constructor TScCMSRecipient.Create(RecipientIdentifierType: TScCMSSubjectIdentifierType; Certificate: TScCertificate);
begin
  inherited Create;

  FCertificate := Certificate;
  FRecipientIdentifierType := RecipientIdentifierType;
end;

constructor TScCMSRecipient.Create(Certificate: TScCertificate);
begin
  inherited Create;

  FCertificate := Certificate;
  FRecipientIdentifierType := sitIssuerAndSerialNumber;
end;

procedure TScCMSRecipient.Init(RecipientIdentifierType: TScCMSSubjectIdentifierType; Certificate: TScCertificate);
begin
  if Certificate = nil then
    raise EScError.Create(seInvalidInputArgs);

  FCertificate := Certificate;
  FRecipientIdentifierType := RecipientIdentifierType;
end;

procedure TScCMSRecipient.Init(Certificate: TScCertificate);
begin
  if Certificate = nil then
    raise EScError.Create(seInvalidInputArgs);

  FCertificate := Certificate;
  FRecipientIdentifierType := sitIssuerAndSerialNumber;
end;

{ TScCMSRecipientInfo }

constructor TScCMSRecipientInfo.Create;
begin
  inherited;

  FRecipientInfoType := ritUnknown;
  FKeyEncryptionAlgorithmIdentifier := TScASN1AlgorithmIdentifier.Create;
end;

destructor TScCMSRecipientInfo.Destroy;
begin
  FKeyEncryptionAlgorithmIdentifier.Free;

  inherited;
end;

procedure TScCMSRecipientInfo.Assign(Source: TScPersistent);
begin
  if (Source = nil) or not (Source is TScCMSRecipientInfo) then
    RaiseAssignError(Source);

  FRecipientInfoType := TScCMSRecipientInfo(Source).FRecipientInfoType;
  FKeyEncryptionAlgorithmIdentifier.Assign(TScCMSRecipientInfo(Source).FKeyEncryptionAlgorithmIdentifier);
  FEncryptedKey := TScCMSRecipientInfo(Source).FEncryptedKey;
end;

function TScCMSRecipientInfo.GetVersion: integer;
begin
{$IFDEF FPC}
  Result := 0;
{$ENDIF}
  raise EScError.Create(seInvalidRecipientInfo);
end;

{ TScCMSKeyTransRecipientInfo }

constructor TScCMSKeyTransRecipientInfo.Create;
begin
  inherited;

  FRecipientInfoType := ritKeyTransport;
  FRecipientIdentifier := TScCMSSubjectIdentifier.Create;
end;

destructor TScCMSKeyTransRecipientInfo.Destroy;
begin
  FRecipientIdentifier.Free;

  inherited;
end;

function TScCMSKeyTransRecipientInfo.Clone: TScPersistent;
begin
  Result := TScCMSKeyTransRecipientInfo.Create;
  Result.Assign(Self);
end;

procedure TScCMSKeyTransRecipientInfo.Assign(Source: TScPersistent);
begin
  inherited Assign(Source);

  if Source is TScCMSKeyTransRecipientInfo then
    FRecipientIdentifier.Assign(TScCMSKeyTransRecipientInfo(Source).FRecipientIdentifier);
end;

procedure TScCMSKeyTransRecipientInfo.Init(Recipient: TScCMSRecipient);
begin
  Init(Recipient, nil);
end;

procedure TScCMSKeyTransRecipientInfo.Init(Recipient: TScCMSRecipient; const EncryptedKey: TBytes);
begin
  if Recipient = nil then
    raise EScError.Create(seInvalidInputArgs);

  FRecipientIdentifier.Init(Recipient.RecipientIdentifierType, Recipient.Certificate);
  FKeyEncryptionAlgorithmIdentifier.InitAsRSA;
  FEncryptedKey := EncryptedKey;
end;

function TScCMSKeyTransRecipientInfo.GetVersion: integer;
begin
  case FRecipientIdentifier.SubjectIdentifierType of
    sitIssuerAndSerialNumber:
      Result := 0;
    sitSubjectKeyIdentifier:
      Result := 2;
  else
    raise EScError.Create(seInvalidIdentifierType);
  end;
end;

procedure TScCMSKeyTransRecipientInfo.Parse(LexemInfo: TScLexemInfo);
var
  RecipientInfoLexem: TScLexemInfo;
begin
  RecipientInfoLexem := LexemInfo['RecipientInfo'];

  if SameText(RecipientInfoLexem.Name, 'ktri') then begin
    FRecipientIdentifier.Parse(RecipientInfoLexem['RecipientIdentifier']);
    TScASNUtils.Parse(FKeyEncryptionAlgorithmIdentifier, RecipientInfoLexem['KeyEncryptionAlgorithm']);
    FEncryptedKey := RecipientInfoLexem['EncryptedKey'].AsBytes;
  end
  else
    raise EScError.Create(seInvalidRecipientInfo);
end;

function TScCMSKeyTransRecipientInfo.Encode: TBytes;
var
  ASN1Compiler: TScASN1Compiler;
begin
  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_KEY_TRANS_RECIPIENT_INFO_DESC) then
      raise EScError.Create(seWrongDataFormat);

    ASN1Compiler['Version'].AsInteger := GetVersion;
    ASN1Compiler['RecipientIdentifier'].EncodedData := FRecipientIdentifier.Encode;

    ASN1Compiler['KeyEncryptionAlgorithm']['Algorithm'].AsString := FKeyEncryptionAlgorithmIdentifier.Algorithm.Value;
    if Length(FKeyEncryptionAlgorithmIdentifier.Parameters) > 0 then
      ASN1Compiler['KeyEncryptionAlgorithm']['Parameters'].EncodedData := FKeyEncryptionAlgorithmIdentifier.Parameters;

    ASN1Compiler['EncryptedKey'].AsBytes := FEncryptedKey;

    Result := ASN1Compiler.Build;
  finally
    ASN1Compiler.Free;
  end;
end;

{ TScCMSKeyAgreeRecipientInfo }

constructor TScCMSKeyAgreeRecipientInfo.Create;
begin
  inherited;

  FRecipientInfoType := ritKeyAgreement;
  FRecipientIdentifier := TScCMSSubjectIdentifier.Create;
  FOriginatorIdentifier := TScCMSOriginatorIdentifierOrKey.Create;
end;

destructor TScCMSKeyAgreeRecipientInfo.Destroy;
begin
  FRecipientIdentifier.Free;
  FOriginatorIdentifier.Free;

  inherited;
end;

function TScCMSKeyAgreeRecipientInfo.Clone: TScPersistent;
begin
  Result := TScCMSKeyAgreeRecipientInfo.Create;
  Result.Assign(Self);
end;

procedure TScCMSKeyAgreeRecipientInfo.Assign(Source: TScPersistent);
begin
  inherited Assign(Source);

  if Source is TScCMSKeyAgreeRecipientInfo then begin
    FRecipientIdentifier.Assign(TScCMSKeyAgreeRecipientInfo(Source).FRecipientIdentifier);
    FOriginatorIdentifier.Assign(TScCMSKeyAgreeRecipientInfo(Source).FOriginatorIdentifier);
    FUserKeyingMaterial := TScCMSKeyAgreeRecipientInfo(Source).FUserKeyingMaterial;
  end;
end;

function TScCMSKeyAgreeRecipientInfo.GetVersion: integer;
begin
  Result := 3;
end;

procedure TScCMSKeyAgreeRecipientInfo.Parse(LexemInfo: TScLexemInfo);
var
  RecipientInfoLexem, RecipientEncryptedKeysLexem: TScLexemInfo;
begin
  RecipientInfoLexem := LexemInfo['RecipientInfo'];

  if SameText(RecipientInfoLexem.Name, 'kari') then begin
    FOriginatorIdentifier.Parse(RecipientInfoLexem['Originator']);
    FUserKeyingMaterial := RecipientInfoLexem['Ukm'].AsBytes;
    TScASNUtils.Parse(FKeyEncryptionAlgorithmIdentifier, RecipientInfoLexem['KeyEncryptionAlgorithm']);

    RecipientEncryptedKeysLexem := RecipientInfoLexem['RecipientEncryptedKeys'];
    if RecipientEncryptedKeysLexem.ValueCount > 1 then
      raise EScError.Create(seWrongDataFormat)
    else
    if RecipientEncryptedKeysLexem.ValueCount = 1 then begin
      FRecipientIdentifier.Parse(RecipientEncryptedKeysLexem.Values[0]['Rid']);
      FEncryptedKey := RecipientEncryptedKeysLexem.Values[0]['EncryptedKey'].AsBytes;
    end;
  end
  else
    raise EScError.Create(seInvalidRecipientInfo);
end;

function TScCMSKeyAgreeRecipientInfo.Encode: TBytes;
var
  ASN1Compiler: TScASN1Compiler;
  RecipientEncryptedKeysLexem: TScLexemInfo;
begin
  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_KEY_AGREE_RECIPIENT_INFO_DESC) then
      raise EScError.Create(seWrongDataFormat);

    ASN1Compiler['Version'].AsInteger := GetVersion;
    ASN1Compiler['Originator'].EncodedData := FOriginatorIdentifier.Encode;
    ASN1Compiler['Ukm'].AsBytes := FUserKeyingMaterial;

    ASN1Compiler['KeyEncryptionAlgorithm']['Algorithm'].AsString := FKeyEncryptionAlgorithmIdentifier.Algorithm.Value;
    if Length(FKeyEncryptionAlgorithmIdentifier.Parameters) > 0 then
      ASN1Compiler['KeyEncryptionAlgorithm']['Parameters'].EncodedData := FKeyEncryptionAlgorithmIdentifier.Parameters;

    RecipientEncryptedKeysLexem := ASN1Compiler['RecipientEncryptedKeys'];
    RecipientEncryptedKeysLexem.ValueCount := 1;
    RecipientEncryptedKeysLexem.Values[0]['Rid'].EncodedData := FRecipientIdentifier.Encode;
    RecipientEncryptedKeysLexem.Values[0]['EncryptedKey'].AsBytes := FEncryptedKey;

    Result := ASN1Compiler.Build;
  finally
    ASN1Compiler.Free;
  end;
end;

{ TScCMSKEKRecipientInfo }

constructor TScCMSKEKRecipientInfo.Create;
begin
  inherited;

  FRecipientInfoType := ritKEK;
end;

destructor TScCMSKEKRecipientInfo.Destroy;
begin
  inherited;
end;

function TScCMSKEKRecipientInfo.Clone: TScPersistent;
begin
  Result := TScCMSKEKRecipientInfo.Create;
  Result.Assign(Self);
end;

procedure TScCMSKEKRecipientInfo.Assign(Source: TScPersistent);
begin
  inherited Assign(Source);

  if Source is TScCMSKEKRecipientInfo then begin
    FKeyIdentifier := TScCMSKEKRecipientInfo(Source).FKeyIdentifier;
    FDate := TScCMSKEKRecipientInfo(Source).FDate;
  end;
end;

function TScCMSKEKRecipientInfo.GetVersion: integer;
begin
  Result := 4;
end;

procedure TScCMSKEKRecipientInfo.Parse(LexemInfo: TScLexemInfo);
var
  RecipientInfoLexem: TScLexemInfo;
begin
  RecipientInfoLexem := LexemInfo['RecipientInfo'];

  if SameText(RecipientInfoLexem.Name, 'kekri') then begin
    FKeyIdentifier := RecipientInfoLexem['KEKIdentifier']['KeyIdentifier'].AsBytes;
    FDate := RecipientInfoLexem['KEKIdentifier']['Date'].AsDateTime;
    TScASNUtils.Parse(FKeyEncryptionAlgorithmIdentifier, RecipientInfoLexem['KeyEncryptionAlgorithm']);
    FEncryptedKey := RecipientInfoLexem['EncryptedKey'].AsBytes;
  end
  else
    raise EScError.Create(seInvalidRecipientInfo);
end;

function TScCMSKEKRecipientInfo.Encode: TBytes;
var
  ASN1Compiler: TScASN1Compiler;
begin
  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_KEK_RECIPIENT_INFO_DESC) then
      raise EScError.Create(seWrongDataFormat);

    ASN1Compiler['Version'].AsInteger := GetVersion;

    ASN1Compiler['KEKIdentifier']['KeyIdentifier'].AsBytes := FKeyIdentifier;
    if FDate <> 0 then
      ASN1Compiler['KEKIdentifier']['Date'].AsDateTime := FDate;

    ASN1Compiler['KeyEncryptionAlgorithm']['Algorithm'].AsString := FKeyEncryptionAlgorithmIdentifier.Algorithm.Value;
    if Length(FKeyEncryptionAlgorithmIdentifier.Parameters) > 0 then
      ASN1Compiler['KeyEncryptionAlgorithm']['Parameters'].EncodedData := FKeyEncryptionAlgorithmIdentifier.Parameters;

    ASN1Compiler['EncryptedKey'].AsBytes := FEncryptedKey;

    Result := ASN1Compiler.Build;
  finally
    ASN1Compiler.Free;
  end;
end;

{ TScCMSPasswordRecipientInfo }

constructor TScCMSPasswordRecipientInfo.Create;
begin
  inherited;

  FRecipientInfoType := ritPassword;
  FKeyDerivationAlgorithmIdentifier := TScASN1AlgorithmIdentifier.Create;
end;

destructor TScCMSPasswordRecipientInfo.Destroy;
begin
  FKeyDerivationAlgorithmIdentifier.Free;

  inherited;
end;

function TScCMSPasswordRecipientInfo.Clone: TScPersistent;
begin
  Result := TScCMSPasswordRecipientInfo.Create;
  Result.Assign(Self);
end;

procedure TScCMSPasswordRecipientInfo.Assign(Source: TScPersistent);
begin
  inherited Assign(Source);

  if Source is TScCMSPasswordRecipientInfo then
    FKeyDerivationAlgorithmIdentifier.Assign(TScCMSPasswordRecipientInfo(Source).FKeyDerivationAlgorithmIdentifier);
end;

function TScCMSPasswordRecipientInfo.GetVersion: integer;
begin
  Result := 0;
end;

procedure TScCMSPasswordRecipientInfo.Parse(LexemInfo: TScLexemInfo);
var
  RecipientInfoLexem: TScLexemInfo;
begin
  RecipientInfoLexem := LexemInfo['RecipientInfo'];

  if SameText(RecipientInfoLexem.Name, 'pwri') then begin
    TScASNUtils.Parse(FKeyDerivationAlgorithmIdentifier, RecipientInfoLexem['KeyDerivationAlgorithm']);
    TScASNUtils.Parse(FKeyEncryptionAlgorithmIdentifier, RecipientInfoLexem['KeyEncryptionAlgorithm']);
    FEncryptedKey := RecipientInfoLexem['EncryptedKey'].AsBytes;
  end
  else
    raise EScError.Create(seInvalidRecipientInfo);
end;

function TScCMSPasswordRecipientInfo.Encode: TBytes;
var
  ASN1Compiler: TScASN1Compiler;
begin
  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_PASSWORD_RECIPIENT_INFO_DESC) then
      raise EScError.Create(seWrongDataFormat);

    ASN1Compiler['Version'].AsInteger := GetVersion;

    ASN1Compiler['KeyDerivationAlgorithm']['Algorithm'].AsString := FKeyDerivationAlgorithmIdentifier.Algorithm.Value;
    if Length(FKeyDerivationAlgorithmIdentifier.Parameters) > 0 then
      ASN1Compiler['KeyDerivationAlgorithm']['Parameters'].EncodedData := FKeyDerivationAlgorithmIdentifier.Parameters;

    ASN1Compiler['KeyEncryptionAlgorithm']['Algorithm'].AsString := FKeyEncryptionAlgorithmIdentifier.Algorithm.Value;
    if Length(FKeyEncryptionAlgorithmIdentifier.Parameters) > 0 then
      ASN1Compiler['KeyEncryptionAlgorithm']['Parameters'].EncodedData := FKeyEncryptionAlgorithmIdentifier.Parameters;

    ASN1Compiler['EncryptedKey'].AsBytes := FEncryptedKey;

    Result := ASN1Compiler.Build;
  finally
    ASN1Compiler.Free;
  end;
end;

{ TScCMSRecipientInfos }

function TScCMSRecipientInfos.GetItemClassType: TScPersistentClass;
begin
  Result := TScCMSRecipientInfo;
end;

function TScCMSRecipientInfos.GetRecipientInfo(Index: integer): TScCMSRecipientInfo;
begin
  Result := TObject(Items[Index]) as TScCMSRecipientInfo;
end;

procedure TScCMSRecipientInfos.SetRecipientInfo(Index: integer; Item: TScCMSRecipientInfo);
begin
  Items[Index] := Item;
end;

{ TScCMSEnvelopedData }

constructor TScCMSEnvelopedData.Create;
begin
  inherited;

  FContentEncryptionAlgorithm := TScASN1AlgorithmIdentifier.Create;
  FContentEncryptionAlgorithm.Algorithm.Value := OID_DES_EDE3_CBC; /// https://msdn.microsoft.com/en-us/library/zy4dse44(v=vs.110).aspx

  FContentInfo := TScCMSContentInfo.Create;
  FRecipientInfos := TScCMSRecipientInfos.Create;
  FUnprotectedAttributes := TScCMSUnsignedAttributes.Create;

  SetReadOnly(True);
end;

destructor TScCMSEnvelopedData.Destroy;
begin
  ClearSessionKey; // to protect

  FContentEncryptionAlgorithm.Free;
  FContentInfo.Free;
  FRecipientInfos.Free;
  FUnprotectedAttributes.Free;

  inherited;
end;

procedure TScCMSEnvelopedData.SetReadOnly(Value: boolean);
begin
  TScASNUtils.SetReadOnly(FContentEncryptionAlgorithm, Value);
  FContentInfo.SetReadOnly(Value);
end;

procedure TScCMSEnvelopedData.ClearSessionKey;
begin
  if Length(FSessionKey) > 0 then
    FillChar(FSessionKey[0], Length(FSessionKey), 0); // to protect
  SetLength(FSessionKey, 0);
  SetLength(FSessionIV, 0);
end;

procedure TScCMSEnvelopedData.Clear;
begin
  ClearSessionKey;
  FContentEncryptionAlgorithm.Clear;
  FRecipientInfos.Clear;
end;

procedure TScCMSEnvelopedData.Init(ContentInfo: TScCMSContentInfo; EncryptionAlgorithm: TScASN1AlgorithmIdentifier);
begin
  if (ContentInfo = nil) or (EncryptionAlgorithm = nil) then
    raise EScError.Create(seInvalidInputArgs);

  SetReadOnly(False);
  try
    Clear;
    FContentInfo.Assign(ContentInfo);
    FContentEncryptionAlgorithm.Assign(EncryptionAlgorithm);
  finally
    SetReadOnly(True);
  end;
end;

procedure TScCMSEnvelopedData.Init(ContentInfo: TScCMSContentInfo;
  EncryptionAlgorithm: TScSymmetricAlgorithm = saTripleDES_cbc);
var
  OId: string;
begin
  if ContentInfo = nil then
    raise EScError.Create(seInvalidInputArgs);

  OId := CipherFactory.EncryptionAlgorithmToOid(EncryptionAlgorithm); // to check available algorithm

  SetReadOnly(False);
  try
    Clear;
    FContentInfo.Assign(ContentInfo);
    FContentEncryptionAlgorithm.Algorithm.Value := OId;
  finally
    SetReadOnly(True);
  end;
end;

procedure TScCMSEnvelopedData.Init(const ContentBuffer: TBytes;
  EncryptionAlgorithm: TScSymmetricAlgorithm = saTripleDES_cbc);
var
  OId: string;
begin
  OId := CipherFactory.EncryptionAlgorithmToOid(EncryptionAlgorithm); // to check available algorithm

  SetReadOnly(False);
  try
    Clear;
    FContentInfo.Init(ContentBuffer);
    FContentEncryptionAlgorithm.Algorithm.Value := OId;
  finally
    SetReadOnly(True);
  end;
end;

procedure TScCMSEnvelopedData.Init(ContentStream: TStream;
  EncryptionAlgorithm: TScSymmetricAlgorithm = saTripleDES_cbc);
var
  OId: string;
begin
  if ContentStream = nil then
    raise EScError.Create(seInvalidInputArgs);

  OId := CipherFactory.EncryptionAlgorithmToOid(EncryptionAlgorithm); // to check available algorithm

  SetReadOnly(False);
  try
    Clear;
    FContentInfo.Init(ContentStream);
    FContentEncryptionAlgorithm.Algorithm.Value := OId;
  finally
    SetReadOnly(True);
  end;
end;

function TScCMSEnvelopedData.FindRecipientInfo(Certificate: TScCertificate): TScCMSRecipientInfo;
var
  RecipientInfo: TScCMSRecipientInfo;
  i: integer;
begin
  if Certificate = nil then
    raise EScError.Create(seInvalidInputArgs);

  for i := 0 to FRecipientInfos.Count - 1 do begin
    RecipientInfo := FRecipientInfos[i];
    if RecipientInfo.RecipientInfoType = ritKeyTransport then
      if (RecipientInfo as TScCMSKeyTransRecipientInfo).RecipientIdentifier.CheckCertificateMatching(Certificate) then begin
        Result := RecipientInfo;
        Exit;
      end;
  end;

  raise EScError.Create(seRecipientInfoNotFound);
end;

procedure TScCMSEnvelopedData.CreateIfNotExistsRecipientInfo(Recipient: TScCMSRecipient);
var
  RecipientInfo: TScCMSRecipientInfo;
  KeyTransRecipientInfo: TScCMSKeyTransRecipientInfo;
  i: integer;
begin
  if (Recipient = nil) or (Recipient.Certificate = nil) then
    raise EScError.Create(seInvalidInputArgs);
  if (Recipient.Certificate.Key.Algorithm <> aaEC) and (Recipient.Certificate.Key.BitCount < 1024) then
    raise EScError.Create(seSmallCertificateKeyLength);

  KeyTransRecipientInfo := nil;

  for i := 0 to FRecipientInfos.Count - 1 do begin
    RecipientInfo := FRecipientInfos[i];
    if RecipientInfo.RecipientInfoType = ritKeyTransport then begin
      KeyTransRecipientInfo := RecipientInfo as TScCMSKeyTransRecipientInfo;
      if (KeyTransRecipientInfo.RecipientIdentifier.SubjectIdentifierType = Recipient.RecipientIdentifierType) and
        KeyTransRecipientInfo.RecipientIdentifier.CheckCertificateMatching(Recipient.Certificate)
      then begin
        if Length(KeyTransRecipientInfo.FEncryptedKey) > 0 then
          Exit   // RecipientInfo is found and doesn't need in changes
        else
          Break; // RecipientInfo is found, but EncryptedKey must be set
      end;

      KeyTransRecipientInfo := nil;
    end;
  end;

  if KeyTransRecipientInfo = nil then begin
    KeyTransRecipientInfo := TScCMSKeyTransRecipientInfo.Create;
    try
      KeyTransRecipientInfo.Init(Recipient);
    except
      KeyTransRecipientInfo.Free;
      raise;
    end;
    FRecipientInfos.Add(KeyTransRecipientInfo);
  end;

  Assert(Length(FSessionKey) > 0);
  KeyTransRecipientInfo.FEncryptedKey := Recipient.Certificate.Encrypt(FSessionKey);
end;

function TScCMSEnvelopedData.GetEncryptionAlgorithm: TScSymmetricAlgorithm;
begin
  Result := CipherFactory.OidToEncryptionAlgorithm(FContentEncryptionAlgorithm.Algorithm.Value);
end;

procedure TScCMSEnvelopedData.Encrypt(Recipient: TScCMSRecipient);
var
  Algo: TScSymmetricAlgorithm;
  Params: TBytes;
begin
  if Random = nil then
    raise Exception.Create(SInternalError);

  if Length(FSessionKey) = 0 then begin
    Algo := GetEncryptionAlgorithm;
    SetLength(FSessionKey, CipherFactory.GetKeySize(Algo));
    Random.Random(FSessionKey, 0, Length(FSessionKey));

    if (Length(FContentEncryptionAlgorithm.Parameters) > 2) and
       (FContentEncryptionAlgorithm.Parameters[0] = 4) and  // ASN1 ctOctetString
       (FContentEncryptionAlgorithm.Parameters[1] <= Length(FContentEncryptionAlgorithm.Parameters) - 2) // Length
    then begin
      SetLength(FSessionIV, FContentEncryptionAlgorithm.Parameters[1]);
      Move(FContentEncryptionAlgorithm.Parameters[2], FSessionIV[0], Length(FSessionIV));
    end
    else begin
      SetLength(FSessionIV, CipherFactory.GetBlockSize(Algo));
      Random.Random(FSessionIV, 0, Length(FSessionIV));

      SetLength(Params, Length(FSessionIV) + 2);
      Params[0] := 4; // ASN1 ctOctetString
      Params[1] := Length(FSessionIV); // Length
      Move(FSessionIV[0], Params[2], Length(FSessionIV));
      TScASNUtils.SetReadOnly(FContentEncryptionAlgorithm, False);
      FContentEncryptionAlgorithm.Parameters := Params;
      TScASNUtils.SetReadOnly(FContentEncryptionAlgorithm, True);
    end;
  end;

  CreateIfNotExistsRecipientInfo(Recipient);
end;

function TScCMSEnvelopedData.CalcEncryptedSize: Int64;
var
  BlockSize: integer;
begin
  if FContentInfo.ContentStream.Stream <> nil then
    Result := FContentInfo.ContentStream.Count
  else
    Result := Length(FContentInfo.FContentBuffer);

  BlockSize := CipherFactory.GetBlockSize(GetEncryptionAlgorithm);
  Result := ((Result div BlockSize) + 1) * BlockSize;
end;

procedure TScCMSEnvelopedData.EncryptContentData(OutStream: TStream;
  const PrefixData: TBytes; PrefixOffset, PrefixCount: integer; out PostfixData: TBytes);
var
  c: TSymmetricAlgorithm;
  TmpBuf: TBytes;
  Count: Int64;
  BufCount, UnReadCount: integer;
  Pad: byte;
begin
  if OutStream = nil then
    raise EScError.Create(seInvalidInputArgs);

  if PrefixCount > DER_LINE_LENGTH then
    raise EScError.Create(seInvalidInputArgs);

  Assert(Length(FSessionKey) > 0);
  c := CipherFactory.CreateCipher(GetEncryptionAlgorithm, FSessionKey, FSessionIV);
  try
    if FContentInfo.ContentStream.Stream <> nil then begin
      SetLength(TmpBuf, DER_BUFFER_SIZE + DER_LINE_LENGTH);
      if PrefixCount > 0 then
        Move(PrefixData[PrefixOffset], TmpBuf[0], PrefixCount);

      FContentInfo.ContentStream.Stream.Position := FContentInfo.ContentStream.Position;
      Count := FContentInfo.ContentStream.Count;

      while Count >= DER_BUFFER_SIZE do begin
        FContentInfo.FContentStream.Stream.ReadBuffer(TmpBuf[PrefixCount], DER_BUFFER_SIZE);
        c.EncodeBuffer(@TmpBuf[PrefixCount], @TmpBuf[PrefixCount], DER_BUFFER_SIZE);
        EncodeBlock(TmpBuf, 0, DER_BUFFER_SIZE, OutStream, False, UnReadCount);
        Assert(UnReadCount = 0);
        if PrefixCount > 0 then
          Move(TmpBuf[DER_BUFFER_SIZE], TmpBuf[0], PrefixCount);
        Dec(Count, DER_BUFFER_SIZE);
      end;

      if Count > 0 then
        FContentInfo.ContentStream.Stream.ReadBuffer(TmpBuf[PrefixCount], Count);
      Pad := c.BlockSize - (Count mod c.BlockSize);
      Assert((PrefixCount + Count + Pad) <= Length(TmpBuf));
      if Pad > 0 then
        FillChar(TmpBuf[PrefixCount + Count], Pad, Pad);
      c.EncodeBuffer(@TmpBuf[PrefixCount], @TmpBuf[PrefixCount], Count + Pad);
      BufCount := Count + Pad + PrefixCount;
      EncodeBlock(TmpBuf, 0, BufCount, OutStream, False, UnReadCount);
      if UnReadCount > 0 then
        Move(TmpBuf[BufCount - UnReadCount], TmpBuf[0], UnReadCount);

      SetLength(TmpBuf, UnReadCount);
      PostfixData := TmpBuf;
    end
    else begin
      BufCount := Length(FContentInfo.FContentBuffer);
      Pad := c.BlockSize - (BufCount mod c.BlockSize);
      SetLength(TmpBuf, BufCount + Pad + PrefixCount);
      if PrefixCount > 0 then
        Move(PrefixData[PrefixOffset], TmpBuf[0], PrefixCount);
      if BufCount > 0 then
        Move(FContentInfo.FContentBuffer[0], TmpBuf[PrefixCount], BufCount);
      if Pad > 0 then
        FillChar(TmpBuf[PrefixCount + BufCount], Pad, Pad);
      c.EncodeBuffer(@TmpBuf[PrefixCount], @TmpBuf[PrefixCount], BufCount + Pad);

      EncodeBlock(TmpBuf, 0, Length(TmpBuf), OutStream, False, UnReadCount);
      if UnReadCount > 0 then
        Move(TmpBuf[Length(TmpBuf) - UnReadCount], TmpBuf[0], UnReadCount);

      SetLength(TmpBuf, UnReadCount);
      PostfixData := TmpBuf;
    end;
  finally
    c.Free;
  end;
end;

function TScCMSEnvelopedData.CreateDecCipher(Certificate: TScCertificate): TSymmetricAlgorithm;
var
  RecipientInfo: TScCMSRecipientInfo;
  Algo: TScSymmetricAlgorithm;
  Key, IV: TBytes;
begin
  if Certificate = nil then
    raise EScError.Create(seInvalidInputArgs);

  RecipientInfo := FindRecipientInfo(Certificate);
  Key := Certificate.Decrypt(RecipientInfo.EncryptedKey);

  Algo := GetEncryptionAlgorithm;
  if (Length(FContentEncryptionAlgorithm.Parameters) > 2) and
     (FContentEncryptionAlgorithm.Parameters[0] = 4) and  // ASN1 ctOctetString
     (FContentEncryptionAlgorithm.Parameters[1] <= Length(FContentEncryptionAlgorithm.Parameters) - 2) // Length
  then begin
    SetLength(IV, FContentEncryptionAlgorithm.Parameters[1]);
    Move(FContentEncryptionAlgorithm.Parameters[2], IV[0], Length(IV));
  end
  else
    SetLength(IV, CipherFactory.GetBlockSize(Algo));

  Result := CipherFactory.CreateCipher(Algo, Key, IV);
  FillChar(Key[0], Length(Key), 0); // to protect
end;

function TScCMSEnvelopedData.Decrypt(Certificate: TScCertificate): TBytes;
var
  c: TSymmetricAlgorithm;
  BufCount: integer;
  Pad: byte;
  i: integer;
begin
  c := CreateDecCipher(Certificate);
  try
    if FContentInfo.ContentStream.Stream <> nil then begin
      BufCount := FContentInfo.ContentStream.Count;
      SetLength(Result, BufCount);

      if BufCount > 0 then begin
        FContentInfo.ContentStream.Stream.Position := FContentInfo.ContentStream.Position;
        FContentInfo.ContentStream.Stream.ReadBuffer(Result[0], BufCount);
        c.DecodeBuffer(@Result[0], @Result[0], BufCount);
      end;
    end
    else begin
      BufCount := Length(FContentInfo.FContentBuffer);
      SetLength(Result, BufCount);

      if BufCount > 0 then
        c.DecodeBuffer(@FContentInfo.FContentBuffer[0], @Result[0], BufCount);
    end;

    if BufCount > 0 then begin
      Pad := Result[BufCount - 1];
      for i := 1 to Pad do
        if Result[BufCount - i] <> Pad then
          raise EScError.Create(seIPCorruptData);

      SetLength(Result, BufCount - Pad);
    end;
  finally
    c.Free;
  end;
end;

procedure TScCMSEnvelopedData.Decrypt(Certificate: TScCertificate; OutStream: TStream);
var
  InStream: TStream;
  Data: TBytes;
  Count: Int64;
  OldInPosition, OldOutPosition: Int64;
  BufCount: integer;
  c: TSymmetricAlgorithm;
  Pad: byte;
  i: integer;
begin
  if OutStream = nil then
    raise EScError.Create(seInvalidInputArgs);

  c := CreateDecCipher(Certificate);
  try
    if FContentInfo.ContentStream.Stream <> nil then begin
      OldOutPosition := OutStream.Position;

      SetLength(Data, BUFFER_SIZE);
      InStream := FContentInfo.ContentStream.Stream;
      InStream.Position := FContentInfo.ContentStream.Position;
      Count := FContentInfo.ContentStream.Count;
      OldInPosition := InStream.Position;
      BufCount := BUFFER_SIZE;

      while Count > 0 do begin
        if Count < BUFFER_SIZE then
          BufCount := Count;

        InStream.ReadBuffer(Data[0], BufCount);
        c.DecodeBuffer(@Data[0], @Data[0], BufCount);

        if InStream = OutStream then begin
          OldInPosition := InStream.Position;
          OutStream.Position := OldOutPosition;
        end;

        OutStream.WriteBuffer(Data[0], BufCount);

        if InStream = OutStream then begin
          OldOutPosition := OutStream.Position;
          InStream.Position := OldInPosition;
        end;

        Dec(Count, BufCount);
      end;

      if InStream = OutStream then
        OutStream.Position := OldOutPosition;

      if BufCount > 0 then begin
        Pad := Data[BufCount - 1];
        if Pad > BufCount then
          Pad := BufCount;

        for i := 1 to Pad do
          if Data[BufCount - i] <> Pad then
            raise EScError.Create(seIPCorruptData);

        OutStream.Position := OutStream.Position - Pad;
      end;
    end
    else begin
      BufCount := Length(FContentInfo.FContentBuffer);

      if BufCount > 0 then begin
        SetLength(Data, BufCount);
        c.DecodeBuffer(@FContentInfo.FContentBuffer[0], @Data[0], BufCount);

        Pad := Data[BufCount - 1];
        for i := 1 to Pad do
          if Data[BufCount - i] <> Pad then
            raise EScError.Create(seIPCorruptData);

        OutStream.WriteBuffer(Data[0], BufCount - Pad);
      end;
    end;

    OutStream.Size := OutStream.Position;
  finally
    c.Free;
  end;
end;

procedure TScCMSEnvelopedData.Decode(const RawData: TBytes);
var
  ASN1Compiler: TScASN1Compiler;
begin
  if Length(RawData) = 0 then
    raise EScError.Create(seInvalidInputArgs);

  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_ENVELOPED_DATA_DESC, RawData) then
      raise EScError.Create(seWrongDataFormat);

    Decode(ASN1Compiler, False);
  finally
    ASN1Compiler.Free;
  end;
end;

procedure TScCMSEnvelopedData.Decode(Stream: TStream);
var
  ASN1Compiler: TScASN1Compiler;
begin
  if Stream = nil then
    raise EScError.Create(seInvalidInputArgs);

  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_ENVELOPED_DATA_DESC, Stream) then
      raise EScError.Create(seWrongDataFormat);

    Decode(ASN1Compiler, True);
  finally
    ASN1Compiler.Free;
  end;
end;

procedure TScCMSEnvelopedData.Decode(ASN1Compiler: TScASN1Compiler; IsStreamUsed: boolean);
var
  OriginatorLexem, CertificatesLexem: TScLexemInfo;
  RecipientInfosLexem, RecipientInfoLexem, EncryptedContentInfoLexem: TScLexemInfo;
  Cert: TScCertificate;
  RecipientInfo: TScCMSRecipientInfo;
  RecipientInfoName: string;
  ContentType: string;
  i: integer;
begin
  SetReadOnly(False);
  try
    ClearSessionKey; // to protect
    ClearCertificateList;
    FContentEncryptionAlgorithm.Clear;
    FContentInfo.Clear;
    FRecipientInfos.Clear;
    FUnprotectedAttributes.Clear;

    ContentType := ASN1Compiler['ContentType'].AsString;
    if ContentType <> OID_ENVELOPED_DATA_TYPE then
      raise EScError.Create(seNotEnvelopedData);

    OriginatorLexem := ASN1Compiler['OriginatorInfo'];
    if not OriginatorLexem.IsNull then begin
      CertificatesLexem := OriginatorLexem['Certificates'];
      for i := 0 to CertificatesLexem.ValueCount - 1 do begin
        if SameText(CertificatesLexem.Values[i]['CertificateDesc'].Name, 'Certificate') then begin
          Cert := TScCertificate.Create(FCertificates);
          Cert.CertName := CERTIFICATE_PREFIX + IntToStr(FCertificates.Count);
          Cert.SetRawData(nil, nil, CertificatesLexem.Values[i].AsBytes);
        end
        else
          raise EScError.Create(seUnknownCertFormat);
      end;
    end;

  {$IFNDEF VER24P}
    RecipientInfo := nil;
  {$ENDIF}  

    RecipientInfosLexem := ASN1Compiler['RecipientInfos'];
    for i := 0 to RecipientInfosLexem.ValueCount - 1 do begin
      RecipientInfoLexem := RecipientInfosLexem.Values[i];
      RecipientInfoName := RecipientInfoLexem['RecipientInfo'].Name;

      if SameText(RecipientInfoName, 'ktri') then
        RecipientInfo := TScCMSKeyTransRecipientInfo.Create
      else if SameText(RecipientInfoName, 'kari') then
        RecipientInfo := TScCMSKeyAgreeRecipientInfo.Create
      else if SameText(RecipientInfoName, 'kekri') then
        RecipientInfo := TScCMSKEKRecipientInfo.Create
      else if SameText(RecipientInfoName, 'pwri') then
        RecipientInfo := TScCMSPasswordRecipientInfo.Create
      else
        raise EScError.Create(seInvalidRecipientInfo);

      FRecipientInfos.Add(RecipientInfo);
      RecipientInfo.Parse(RecipientInfoLexem);
    end;

    EncryptedContentInfoLexem := ASN1Compiler['EncryptedContentInfo'];
    TScASNUtils.Parse(FContentEncryptionAlgorithm, EncryptedContentInfoLexem['ContentEncryptionAlgorithm']);
    FContentInfo.ContentType.Value := EncryptedContentInfoLexem['ContentType'].AsString;
    if IsStreamUsed then
      FContentInfo.FContentStream.Assign(EncryptedContentInfoLexem['EncryptedContent'].AsStreamInfo)
    else
      FContentInfo.FContentBuffer := EncryptedContentInfoLexem['EncryptedContent'].AsBytes;

    FUnprotectedAttributes.Parse(ASN1Compiler['UnprotectedAttributes']);
  finally
    SetReadOnly(True);
  end;
end;

function TScCMSEnvelopedData.Encode: TBytes;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Encode(Stream);
    Stream.Position := 0;
    SetLength(Result, Stream.Size);
    Stream.ReadBuffer(Result[0], Length(Result));
  finally
    Stream.Free;
  end;
end;

procedure TScCMSEnvelopedData.Encode(Stream: TStream);
var
  ASN1Compiler: TScASN1Compiler;
  OriginatorLexem, RecipientInfosLxem, EncryptedContentInfoLexem: TScLexemInfo;
  RecipientInfo: TScCMSRecipientInfo;
  MaxVer: integer;
  EncryptedSize: Int64;
  ASN1Buf, PostfixData: TBytes;
  ASN1ContentOffset, UnReadCount, PostfixDataLen: integer;
  i: integer;
begin
  if Stream = nil then
    raise EScError.Create(seInvalidInputArgs);

  if Length(FSessionKey) = 0 then
    raise EScError.Create(seDataNotEncrypted);

  SetLength(ASN1Buf, 0);

  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_ENVELOPED_DATA_DESC) then
      raise EScError.Create(seWrongDataFormat);

    ASN1Compiler['ContentType'].AsString := OID_ENVELOPED_DATA_TYPE;

    MaxVer := 0;

    if FCertificates.Count > 0 then begin
      OriginatorLexem := ASN1Compiler['OriginatorInfo'];
      OriginatorLexem['Certificates'].ValueCount := FCertificates.Count;
      for i := 0 to FCertificates.Count - 1 do
        OriginatorLexem['Certificates'].Values[i].AsBytes := FCertificates[i].GetRawData;
    end;

    RecipientInfosLxem := ASN1Compiler['RecipientInfos'];
    RecipientInfosLxem.ValueCount := FRecipientInfos.Count;
    for i := 0 to FRecipientInfos.Count - 1 do begin
      RecipientInfo := FRecipientInfos[i];
      if RecipientInfo.FRecipientInfoType in [ritPassword, ritUnknown] then
        MaxVer := 3
      else
      if (MaxVer = 0) and (RecipientInfo.GetVersion <> 0) then
        MaxVer := 2;

      RecipientInfosLxem.Values[i].EncodedData := RecipientInfo.Encode;
    end;

    EncryptedSize := CalcEncryptedSize;
    EncryptedContentInfoLexem := ASN1Compiler['EncryptedContentInfo'];
    EncryptedContentInfoLexem['ContentType'].AsString := FContentInfo.ContentType.Value;
    EncryptedContentInfoLexem['EncryptedContent'].PreDefinedSize := EncryptedSize;
    EncryptedContentInfoLexem['ContentEncryptionAlgorithm']['Algorithm'].AsString := FContentEncryptionAlgorithm.Algorithm.Value;
    if Length(FContentEncryptionAlgorithm.Parameters) > 0 then
      EncryptedContentInfoLexem['ContentEncryptionAlgorithm']['Parameters'].EncodedData := FContentEncryptionAlgorithm.Parameters;

    if FUnprotectedAttributes.Count > 0 then
      ASN1Compiler['UnprotectedAttributes'].EncodedData := FUnprotectedAttributes.Encode;

    if (MaxVer = 0) and ((FCertificates.Count > 0) or (FUnprotectedAttributes.Count > 0)) then
      MaxVer := 2;
    ASN1Compiler['Version'].AsInteger := MaxVer;

    ASN1Buf := ASN1Compiler.Build;

    WriteHeader(Stream);
    ASN1ContentOffset := EncryptedContentInfoLexem['EncryptedContent'].GetOffset;
    EncodeBlock(ASN1Buf, 0, ASN1ContentOffset, Stream, False, UnReadCount);
    EncryptContentData(Stream, ASN1Buf, ASN1ContentOffset - UnReadCount, UnReadCount, PostfixData);
    PostfixDataLen := Length(PostfixData);
    if PostfixDataLen > 0 then begin
      if PostfixDataLen > ASN1ContentOffset then
        raise EScError.Create(seInternalError);

      Move(PostfixData[0], ASN1Buf[ASN1ContentOffset - PostfixDataLen], PostfixDataLen);
    end;
    EncodeBlock(ASN1Buf, ASN1ContentOffset - PostfixDataLen, Length(ASN1Buf) - ASN1ContentOffset + PostfixDataLen, Stream, True, UnReadCount);
    WriteFooter(Stream);

  finally
    ASN1Compiler.Free;
  end;

  ClearSessionKey; // to protect
end;

{ TScCMSProcessor }

constructor TScCMSProcessor.Create(AOwner: TComponent);
begin
  inherited;

  FEncryptionAlgorithm := DefaultEncryptionAlgorithm;
  FDigestAlgorithm := DefaultDigestAlgorithm;

  FContentType := TScOId.Create;
  FEnvelopedData := TScCMSEnvelopedData.Create;
  FRecipient := TScCMSRecipient.Create;
  FSignedData := TScCMSSignedData.Create;
  FSignerInfo := TScCMSSignerInfo.Create;
end;

destructor TScCMSProcessor.Destroy;
begin
  FContentType.Free;
  FEnvelopedData.Free;
  FRecipient.Free;
  FSignedData.Free;
  FSignerInfo.Free;

  inherited;
end;

procedure TScCMSProcessor.Notification(Component: TComponent; Operation: TOperation);
begin
  if (Component = FStorage) and (Operation = opRemove) then
    Storage := nil;

  inherited;
end;

procedure TScCMSProcessor.AssignTo(Dest: TPersistent);
begin
  if IsClass(Dest, TScCMSProcessor) then begin
    TScCMSProcessor(Dest).Storage := Storage;
    TScCMSProcessor(Dest).FCertificateName := FCertificateName;
    TScCMSProcessor(Dest).FCertificate := FCertificate;
    TScCMSProcessor(Dest).FDigestAlgorithm := FDigestAlgorithm;
    TScCMSProcessor(Dest).FEncryptionAlgorithm := FEncryptionAlgorithm;
  end
  else
    inherited;
end;

function TScCMSProcessor.GetCertificate: TScCertificate;
begin
  if FCertificate <> nil then
    Result := FCertificate
  else begin
    if FStorage = nil then
      raise EScError.Create(seStorageNoSet);
    Result := FStorage.Certificates.FindCertificate(FCertificateName);
  end;

  if Result = nil then
    raise EScError.Create(seCertificateNotDefined);
end;

procedure TScCMSProcessor.SetStorage(Value: TScStorage);
begin
  if FStorage <> Value then begin
    if FStorage <> nil then
      FStorage.RemoveFreeNotification(Self);

    FStorage := Value;

    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;

procedure TScCMSProcessor.SetCertificateName(const Value: string);
begin
  if FCertificateName <> Value then begin
    FCertificateName := Value;
    FCertificate := nil;
  end;
end;

procedure TScCMSProcessor.SetCertificate(Value: TScCertificate);
begin
  if FCertificate <> Value then begin
    FCertificate := Value;
    FCertificateName := '';
  end;
end;

procedure TScCMSProcessor.SetDigestAlgorithm(Value: TScHashAlgorithm);
begin
  CipherFactory.HashAlgorithmToOid(Value);  // to check available algorithm
  FDigestAlgorithm := Value;
end;

procedure TScCMSProcessor.SetEncryptionAlgorithm(Value: TScSymmetricAlgorithm);
begin
  CipherFactory.EncryptionAlgorithmToOId(Value); // to check available algorithm
  FEncryptionAlgorithm := Value;
end;

class function TScCMSProcessor.EncodeData(const Data: TBytes; CMSEncoding: TScCMSEncoding): TBytes;
var
  CMSData: TScCMSData;
  Header, Footer, EncodedData: TBytes;
begin
  if CMSEncoding = ceDER then begin
    Result := Data;
    Exit;
  end;

  SetLength(Header, 0);
  SetLength(Footer, 0);
  SetLength(EncodedData, 0);

  CMSData := TScCMSData.Create;
  try
    CMSData.FCMSEncoding := CMSEncoding;
    Header := CMSData.GetHeader;
    Footer := CMSData.GetFooter;
    EncodedData := CMSData.EncodeBlock(Data, 0, Length(Data));
    SetLength(Result, Length(Header) + Length(Footer) + Length(EncodedData));
    Move(Header[0], Result[0], Length(Header));
    Move(EncodedData[0], Result[Length(Header)], Length(EncodedData));
    Move(Footer[0], Result[Length(Header) + Length(EncodedData)], Length(Footer));
  finally
    CMSData.Free;
  end;
end;

class procedure TScCMSProcessor.EncodeData(InStream, OutStream: TStream; CMSEncoding: TScCMSEncoding);
var
  CMSData: TScCMSData;
  Count: Int64;
  UnReadCount: integer;
  TmpBuf: TBytes;
begin
  CMSData := TScCMSData.Create;
  try
    CMSData.FCMSEncoding := CMSEncoding;
    CMSData.WriteHeader(OutStream);

    Count := InStream.Size - InStream.Position;
    SetLength(TmpBuf, DER_BUFFER_SIZE);

    while Count >= DER_BUFFER_SIZE do begin
      InStream.ReadBuffer(TmpBuf[0], DER_BUFFER_SIZE);
      CMSData.EncodeBlock(TmpBuf, 0, DER_BUFFER_SIZE, OutStream, False, UnReadCount);
      Assert(UnReadCount = 0);
      Dec(Count, DER_BUFFER_SIZE);
    end;

    if Count > 0 then begin
      InStream.ReadBuffer(TmpBuf[0], Count);
      CMSData.EncodeBlock(TmpBuf, 0, Count, OutStream, True, UnReadCount);
    end;

    CMSData.WriteFooter(OutStream);
  finally
    CMSData.Free;
  end;
end;

class function TScCMSProcessor.DecodeData(const Data: TBytes): TBytes;
var
  CMSEncoding: TScCMSEncoding;
begin
  Result := TScCMSData.DecodeData(Data, CMSEncoding);
end;

class procedure TScCMSProcessor.DecodeData(InStream, OutStream: TStream);
var
  CMSEncoding: TScCMSEncoding;
  TmpBuf: TBytes;
  Count: Int64;
  BufSize: integer;
begin
  TScCMSData.DecodeData(InStream, OutStream, CMSEncoding);

  if CMSEncoding = ceDER then begin
    // Copying data
    Count := InStream.Size - InStream.Position;
    SetLength(TmpBuf, BUFFER_SIZE);
    BufSize := BUFFER_SIZE;
    while Count > 0 do begin
      if Count < BUFFER_SIZE then
        BufSize := Count;
      InStream.ReadBuffer(TmpBuf[0], BufSize);
      OutStream.WriteBuffer(TmpBuf[0], BufSize);
      Dec(Count, BufSize);
    end;
  end;
end;

function TScCMSProcessor.Encrypt(const Data: TBytes; Encoding: TScCMSEncoding = ceDER): TBytes;
var
  t: TBytes;
begin
  FEnvelopedData.ClearCertificateList;
  FEnvelopedData.Init(Data, FEncryptionAlgorithm);
  FRecipient.Init(GetCertificate);
  FEnvelopedData.Encrypt(FRecipient);

  FEnvelopedData.FCMSEncoding := Encoding;
  Result := FEnvelopedData.Encode;

  t := nil;
  FEnvelopedData.Init(t); // to clear FEnvelopedData.FContentInfo buffer
end;

procedure TScCMSProcessor.Encrypt(InStream, OutStream: TStream; Encoding: TScCMSEncoding = ceDER);
begin
  FEnvelopedData.ClearCertificateList;
  FEnvelopedData.Init(InStream, FEncryptionAlgorithm);
  FRecipient.Init(GetCertificate);
  FEnvelopedData.Encrypt(FRecipient);

  FEnvelopedData.FCMSEncoding := Encoding;
  FEnvelopedData.Encode(OutStream);
end;

procedure TScCMSProcessor.Encrypt(const InFileName, OutFileName: string; Encoding: TScCMSEncoding = ceDER);
var
  InStream, OutStream: TFileStream;
begin
  OutStream := nil;
  InStream := TFileStream.Create(InFileName, fmOpenRead or fmShareDenyWrite);
  try
    OutStream := TFileStream.Create(OutFileName, fmCreate);
    Encrypt(InStream, OutStream, Encoding);
  finally
    InStream.Free;
    OutStream.Free;
  end;
end;

function TScCMSProcessor.Decrypt(const Data: TBytes): TBytes;
var
  CMSEncoding: TScCMSEncoding;
  DecodedData: TBytes;
begin
  DecodedData := FEnvelopedData.DecodeData(Data, CMSEncoding);
  FEnvelopedData.Decode(DecodedData);
  Result := FEnvelopedData.Decrypt(GetCertificate);
end;

procedure TScCMSProcessor.Decrypt(InStream, OutStream: TStream);
var
  CMSEncoding: TScCMSEncoding;
  OldOutPos: Int64;
begin
  OldOutPos := OutStream.Position;
  FEnvelopedData.DecodeData(InStream, OutStream, CMSEncoding);

  if CMSEncoding = ceDER then
    FEnvelopedData.Decode(InStream)
  else begin
    OutStream.Position := OldOutPos;
    FEnvelopedData.Decode(OutStream);
  end;

  OutStream.Position := OldOutPos;
  FEnvelopedData.Decrypt(GetCertificate, OutStream);
  OutStream.Position := OldOutPos;
end;

procedure TScCMSProcessor.Decrypt(const InFileName, OutFileName: string);
var
  InStream, OutStream: TFileStream;
begin
  OutStream := nil;
  InStream := TFileStream.Create(InFileName, fmOpenRead or fmShareDenyWrite);
  try
    OutStream := TFileStream.Create(OutFileName, fmCreate);
    Decrypt(InStream, OutStream);
  finally
    InStream.Free;
    OutStream.Free;
  end;
end;

function TScCMSProcessor.Sign(const Data: TBytes; IncludeContent: boolean = False;
  Encoding: TScCMSEncoding = ceDER): TBytes;
var
  t: TBytes;
begin
  FSignerInfo.Certificate := GetCertificate;
  FSignerInfo.DigestAlgorithm := FDigestAlgorithm;

  FSignedData.ClearCertificateList;
  FSignedData.Init(Data);
  FSignedData.ComputeSignature(FSignerInfo);

  FSignedData.FCMSEncoding := Encoding;
  Result := FSignedData.Encode(IncludeContent);

  t := nil;
  FSignedData.Init(t);
end;

procedure TScCMSProcessor.Sign(InStream, OutStream: TStream; IncludeContent: boolean = False;
  Encoding: TScCMSEncoding = ceDER);
begin
  FSignerInfo.Certificate := GetCertificate;
  FSignerInfo.DigestAlgorithm := FDigestAlgorithm;

  FSignedData.ClearCertificateList;
  FSignedData.Init(InStream);
  FSignedData.ComputeSignature(FSignerInfo);

  FSignedData.FCMSEncoding := Encoding;
  FSignedData.Encode(OutStream, IncludeContent);
end;

procedure TScCMSProcessor.Sign(const InFileName, OutFileName: string; IncludeContent: boolean = False;
  Encoding: TScCMSEncoding = ceDER);
var
  InStream, OutStream: TFileStream;
begin
  OutStream := nil;
  InStream := TFileStream.Create(InFileName, fmOpenRead or fmShareDenyWrite);
  try
    OutStream := TFileStream.Create(OutFileName, fmCreate);
    Sign(InStream, OutStream, IncludeContent, Encoding);
  finally
    InStream.Free;
    OutStream.Free;
  end;
end;

procedure TScCMSProcessor.CheckSignature(const Data: TBytes);
var
  CMSEncoding: TScCMSEncoding;
  DecodedData: TBytes;
begin
  DecodedData := FSignedData.DecodeData(Data, CMSEncoding);
  FSignedData.Decode(DecodedData);
  FSignedData.CheckSignature(GetCertificate);
end;

procedure TScCMSProcessor.CheckSignature(InStream: TStream; TmpDecodedStream: TStream = nil);
var
  CMSEncoding: TScCMSEncoding;
  WasCreated: boolean;
begin
  if TmpDecodedStream = nil then begin
    TmpDecodedStream := TMemoryStream.Create;
    WasCreated := True;
  end
  else
    WasCreated := False;

  try
    FSignedData.DecodeData(InStream, TmpDecodedStream, CMSEncoding);

    if CMSEncoding = ceDER then
      FSignedData.Decode(InStream)
    else begin
      TmpDecodedStream.Position := 0;
      FSignedData.Decode(TmpDecodedStream);
    end;

    FSignedData.CheckSignature(GetCertificate);
  finally
    if WasCreated then
      TmpDecodedStream.Free;
  end;
end;

procedure TScCMSProcessor.CheckSignature(const FileName: string; const TmpFileName: string = '');
var
  Stream: TFileStream;
  TmpDecodedStream: TStream;
begin
  TmpDecodedStream := nil;
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    if TmpFileName <> '' then
      TmpDecodedStream := TFileStream.Create(TmpFileName, fmCreate);

    CheckSignature(Stream, TmpDecodedStream);
  finally
    Stream.Free;
    TmpDecodedStream.Free;
  end;
end;

function TScCMSProcessor.SignAndEncrypt(const Data: TBytes; Encoding: TScCMSEncoding = ceDER): TBytes;
var
  ContentInfo: TScCMSContentInfo;
  SignedData: TBytes;
  t: TBytes;
begin
  SignedData := Sign(Data, True, ceDER);

  ContentInfo := TScCMSContentInfo.Create;
  try
    FContentType.Value := OID_SIGNED_DATA_TYPE;
    ContentInfo.Init(FContentType, SignedData);
    FEnvelopedData.ClearCertificateList;
    FEnvelopedData.Init(ContentInfo, FEncryptionAlgorithm);

    FRecipient.Init(GetCertificate);
    FEnvelopedData.Encrypt(FRecipient);
    FEnvelopedData.FCMSEncoding := Encoding;
    Result := FEnvelopedData.Encode;
  finally
    ContentInfo.Free;
  end;

  t := nil;
  FEnvelopedData.Init(t); // to clear FEnvelopedData.FContentInfo buffer
end;

procedure TScCMSProcessor.SignAndEncrypt(InStream, OutStream: TStream; TmpSignedStream: TStream = nil;
  Encoding: TScCMSEncoding = ceDER);
var
  ContentInfo: TScCMSContentInfo;
  WasCreated: boolean;
begin
  if TmpSignedStream = nil then begin
    TmpSignedStream := TMemoryStream.Create;
    WasCreated := True;
  end
  else
    WasCreated := False;

  try
    Sign(InStream, TmpSignedStream, True, ceDER);
    TmpSignedStream.Position := 0;

    ContentInfo := TScCMSContentInfo.Create;
    try
      FContentType.Value := OID_SIGNED_DATA_TYPE;
      ContentInfo.Init(FContentType, TmpSignedStream);
      FEnvelopedData.ClearCertificateList;
      FEnvelopedData.Init(ContentInfo, FEncryptionAlgorithm);

      FRecipient.Init(GetCertificate);
      FEnvelopedData.Encrypt(FRecipient);
      FEnvelopedData.FCMSEncoding := Encoding;
      FEnvelopedData.Encode(OutStream);
    finally
      ContentInfo.Free;
    end;
  finally
    if WasCreated then
      TmpSignedStream.Free;
  end;
end;

procedure TScCMSProcessor.SignAndEncrypt(const InFileName, OutFileName: string; const TmpFileName: string = '';
  Encoding: TScCMSEncoding = ceDER);
var
  InStream, OutStream: TFileStream;
  TmpSignedStream: TStream;
begin
  TmpSignedStream := nil;
  OutStream := nil;
  InStream := TFileStream.Create(InFileName, fmOpenRead or fmShareDenyWrite);
  try
    if TmpFileName <> '' then
      TmpSignedStream := TFileStream.Create(TmpFileName, fmCreate);

    OutStream := TFileStream.Create(OutFileName, fmCreate);
    SignAndEncrypt(InStream, OutStream, TmpSignedStream, Encoding);
  finally
    InStream.Free;
    OutStream.Free;
    TmpSignedStream.Free;
  end;
end;

function TScCMSProcessor.DecryptAndCheckSignature(const Data: TBytes): TBytes;
var
  DecryptedData: TBytes;
begin
  DecryptedData := Decrypt(Data);
  if FEnvelopedData.ContentInfo.ContentType.Value <> OID_SIGNED_DATA_TYPE then
    raise EScError.Create(seWrongDataFormat);

  FSignedData.Decode(DecryptedData);
  FSignedData.CheckSignature(GetCertificate);
  Result := FSignedData.ContentInfo.GetContentData;
end;

procedure TScCMSProcessor.DecryptAndCheckSignature(InStream, OutStream: TStream; TmpDecryptedStream: TStream = nil);
var
  TmpBuf: TBytes;
  Count: Int64;
  BufSize: integer;
  WasCreated: boolean;
begin
  if TmpDecryptedStream = nil then begin
    TmpDecryptedStream := TMemoryStream.Create;
    WasCreated := True;
  end
  else
    WasCreated := False;

  try
    Decrypt(InStream, TmpDecryptedStream);
    if FEnvelopedData.ContentInfo.ContentType.Value <> OID_SIGNED_DATA_TYPE then
      raise EScError.Create(seWrongDataFormat);

    TmpDecryptedStream.Position := 0;
    FSignedData.Decode(TmpDecryptedStream);
    FSignedData.CheckSignature(GetCertificate);

    // copy data from ContentInfo to OutStream
    Assert(FSignedData.ContentInfo.ContentStream.Stream <> nil);
    FSignedData.ContentInfo.ContentStream.Stream.Position := FSignedData.ContentInfo.ContentStream.Position;
    Count := FSignedData.ContentInfo.ContentStream.Count;
    SetLength(TmpBuf, BUFFER_SIZE);
    BufSize := BUFFER_SIZE;
    while Count > 0 do begin
      if Count < BUFFER_SIZE then
        BufSize := Count;
      FSignedData.ContentInfo.ContentStream.Stream.ReadBuffer(TmpBuf[0], BufSize);
      OutStream.WriteBuffer(TmpBuf[0], BufSize);
      Dec(Count, BufSize);
    end;
  finally
    if WasCreated then
      TmpDecryptedStream.Free;
  end;
end;

procedure TScCMSProcessor.DecryptAndCheckSignature(const InFileName, OutFileName: string; const TmpFileName: string = '');
var
  InStream, OutStream: TFileStream;
  TmpDecryptedStream: TStream;
begin
  TmpDecryptedStream := nil;
  OutStream := nil;
  InStream := TFileStream.Create(InFileName, fmOpenRead or fmShareDenyWrite);
  try
    if TmpFileName <> '' then
      TmpDecryptedStream := TFileStream.Create(TmpFileName, fmCreate);

    OutStream := TFileStream.Create(OutFileName, fmCreate);
    DecryptAndCheckSignature(InStream, OutStream, TmpDecryptedStream);
  finally
    InStream.Free;
    OutStream.Free;
    TmpDecryptedStream.Free;
  end;
end;

end.
