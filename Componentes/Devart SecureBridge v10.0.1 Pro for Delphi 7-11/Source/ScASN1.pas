
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF DAC}
{$I SB.inc}
unit ScASN1;
{$ENDIF}

interface

uses
  SysUtils, Classes,
{$IFNDEF SBRIDGE}
  CRTypes, CRFunctions, CLRClasses,
{$IFNDEF UNIDACPRO}
  TdsUtils;
{$ELSE}
  TdsUtilsUni;
{$ENDIF}
{$ELSE}
  ScTypes, ScFunctions, ScCLRClasses,
  ScUtils;
{$ENDIF}

type
  // !!! Synchronize with ASN1Descriptions
  TScASN1Description = (
    asn1_CERTIFICATE_DESC, asn1_EC_SIGN_DESC, asn1_X25519_PUBLIC_KEY_PARAMS_DESC,
    asn1_EC_DOMAIN_PARAMS_DESC, asn1_RSA_PUBLIC_KEY_PKCS1_DESC,
    asn1_RSA_PRIVATE_KEY_PKCS1_DESC, asn1_DSA_PUBLIC_KEY_PKCS1_DESC,
    asn1_DSA_PRIVATE_KEY_PKCS1_DESC, asn1_EC_PUBLIC_KEY_DESC, asn1_EC_POINT_DESC,
    asn1_EC_PRIVATE_KEY_DESC, asn1_RSA_PKCS8_KEY_DESC, asn1_DSA_PKCS8_KEY_DESC,
    asn1_Ed25519_PKCS8_KEY_DESC, asn1_EC_PKCS8_KEY_DESC,
    asn1_PRIVATE_KEY_PKCS8_PBES2ENC_DESC, asn1_RSAES_OAEP_PARAMS_DESC,
    asn1_RSASSA_PSS_PARAMS_DESC, asn1_ATTR_AND_VALUE_DESC, asn1_ATTRIBUTE_DESC,
    asn1_RDN_FORMAT, asn1_DISTINGUISHED_NAME_FORMAT,
    asn1_ISSUER_AND_SERIAL_NUMBER_DESC, asn1_SUBJECT_KEY_IDENTIFIER_DESC,
    asn1_RECIPIENT_KEY_IDENTIFIER_DESC, asn1_PUBLIC_KEY_DESC,
    asn1_SMIME_ATTRIBUTE_DESC, asn1_SIGNER_INFO_DESC, asn1_SIGNED_DATA_DESC,
    asn1_KEY_TRANS_RECIPIENT_INFO_DESC, asn1_KEY_AGREE_RECIPIENT_INFO_DESC,
    asn1_KEK_RECIPIENT_INFO_DESC, asn1_PASSWORD_RECIPIENT_INFO_DESC,
    asn1_ENVELOPED_DATA_DESC, asn1_DISTINGUISHED_NAMES, asn1_EC_PK_RESTRICTIONS_DESC,
    asn1_RSA_PUBLIC_KEY_OPENSSL_DESC, asn1_RSA_PUBLIC_KEY_OPENSSL_DESC2,
    asn1_DSA_PUBLIC_KEY_OPENSSL_DESC, asn1_RSA_PRIVATE_KEY_OPENSSL_DESC,
    asn1_DSA_PRIVATE_KEY_OPENSSL_DESC, asn1_DSA_CERT_PUBLIC_KEY_DESC,
    asn1_DSA_TRUNC_PUBLIC_KEY_PKCS1_DESC, asn1_PKCS8_KEY_DESC,
    asn1_DSA_PRIVATE_KEY_PKCS8_DESC, asn1_PRIVATE_KEY_PKCS8ENC_DESC,
    asn1_PBES2_PARAMS_DESC, asn1_PBES1_PARAMS_DESC, asn1_PKCS12_PBE_PARAMS_DESC,
    asn1_BASIC_CONSTRAINTS_EXTENSION_DESC, asn1_BASIC_CONSTRAINTS2_EXTENSION_DESC,
    asn1_KEY_USAGE_EXTENSION_DESC, asn1_EXTENDED_KEY_USAGE_EXTENSION_DESC,
    asn1_SUBJECT_KEY_ID_EXTENSION_DESC, asn1_CERTIFICATE_POLICIES_EXTENSION_DESC,
    asn1_POLICY_MAPPINGS_EXTENSION_DESC, asn1_ALTERNATIVE_NAME_EXTENSION_DESC,
    asn1_SUBJECT_DIRECTORY_ATTRIBUTES_EXTENSION_DESC,
    asn1_CRL_DISTRIBUTION_POINTS_EXTENSION_DESC, asn1_AUTHORITY_INFO_ACCESS_EXTENSION_DESC,
    asn1_SUBJECT_INFO_ACCESS_EXTENSION_DESC, asn1_CRL_NUMBER_EXTENSION_DESC,
    asn1_CRL_ISSUING_DISTRIBUTION_POINT_EXTENSION_DESC, asn1_CRL_REASON_EXTENSION_DESC,
    asn1_CRL_INVALIDITY_DATE_EXTENSION_DESC, asn1_CRL_CERTIFICATE_ISSUER_EXTENSION_DESC,
    asn1_SIGNED_CERTIFICATE_TIMESTAMP_LIST_DESC,
    asn1_CERTIFICATE_LIST_DESC, asn1_PARTIAL_CERTIFICATE_LIST_DESC, asn1_EXTENSIONS_DESC,
    asn1_AUTHORITY_KEY_ID_EXTENSION_DESC, asn1_AUTHORITY_KEY_ID_EXTENSION_DESC2,
    asn1_ATTRIBUTES_DESC, asn1_SIGNED_ATTRS_DESC, asn1_UNSIGNED_ATTRS_DESC,
    asn1_PFX_DESC, asn1_AUTH_SAFE_DESC_EX, asn1_ENCRYPTED_DATA_DESC,
    asn1_SAFE_CONTENTS_DESC_EX, asn1_SAFE_BAG_VALUE_DESC
  );

  TOId = array of integer;

  TScASN1DataType = (
    dtAny, dtImplicitArray, dtChoice, dtSimpleChoice,
    dtBoolean, dtInteger, dtBit, dtOctetString, dtNull,
    dtObjectIdentifier, dtEnumerated,
    dtUTF8String, dtNumericString, dtPrintableString, dtTeletexString, dtVideotexString,
    dtIA5String, dtGraphicString, dtVisibleString, dtGeneralString, dtUniversalString, dtBMPString,
    dtUTCTime, dtGeneralizedTime, dtSequence, dtSet
  );

  TScASN1DataTypes = set of TScASN1DataType;

  TScLexemStatus = (lsIsEncodedData, lsIsNull, lsIsImplicit, lsIsOptional, lsIsPreDefined);
  TScLexemStatuses = set of TScLexemStatus;

  PScLexemData = ^TScLexemData;
  TScLexemData = packed record
    Buffer: TBytes;
    StartPos: Int64;
    Size: Int64;
    AddedSize: integer;
    DataType: TScASN1DataType;
    IsIndefiniteSize: boolean;
  end;

  TScLexemInfo = class;
  TScLexemInfos = array of TScLexemInfo;
{$IFNDEF VER12P}
  {$EXTERNALSYM TScLexemInfos}
{$ENDIF}
  PScLexemInfo = {$IFDEF AUTOREFCOUNT}^{$ENDIF}TScLexemInfo; // Performance optimization for AUTOREFCOUNT

  TScLexemInfoClass = class of TScLexemInfo;
  TScLexemInfo = class
  private
    FLexemData: TScLexemData;
    FParentLexemInfo: PScLexemInfo;

    FName: string;
    FStatus: TScLexemStatuses;
    FChoiceDataTypes: TScASN1DataTypes;
    FIdentifier: byte;

    function GetIsNull: boolean;
    function GetDataType: TScASN1DataType;
    procedure SetDataType(Value: TScASN1DataType);
    function GetAsBytes: TBytes;
    procedure SetAsBytes(const Value: TBytes);
    function GetAsString: string;
    procedure SetAsString(const Value: string);
    function GetAsWideString: WideString;
    procedure SetAsWideString(const Value: WideString);
    function GetAsBoolean: boolean;
    procedure SetAsBoolean(Value: boolean);
    function GetAsInteger: Int64;
    procedure SetAsInteger(Value: Int64);
    function GetAsBigInteger: TBytes;
    procedure SetAsBigInteger(const Value: TBytes);
    function GetAsOID: string;
    procedure SetAsOID(const Value: string);
    function GetAsDateTime: TDateTime;
    procedure SetAsDateTime(const Value: TDateTime);
    procedure SetPreDefinedSize(Value: Int64);

    function GetEncodedData: TBytes;
    procedure SetEncodedData(const Value: TBytes);

    procedure SetParentNotNulls;
    function GetStream: TStream;

  protected
    function GetValue(Index: integer): TScLexemInfo; virtual;
    function GetValueCount: integer; virtual;
    procedure SetValueCount(Value: integer); virtual;

    procedure AssignTo(Dest: TScLexemInfo); virtual;
    function Clone(const LexemInfoClass: TScLexemInfoClass = nil): TScLexemInfo; virtual;
    function GetValueByName(const Name: string): TScLexemInfo;
    function LexemByName(const Name: string): TScLexemInfo; virtual;

  public
    constructor Create; virtual;
    destructor Destroy; override;

    function GetChoiceLexem: TScLexemInfo; virtual;
    procedure SetChoiceLexem(const Name: string); virtual;
    function GetOffset: Int64;
    function AsStreamInfo: TScStreamInfoRec;

    property EncodedData: TBytes read GetEncodedData write SetEncodedData;
    property IsNull: boolean read GetIsNull;
    property AsBytes: TBytes read GetAsBytes write SetAsBytes;
    property AsString: string read GetAsString write SetAsString;
    property AsWideString: WideString read GetAsWideString write SetAsWideString;
    property AsBoolean: boolean read GetAsBoolean write SetAsBoolean;
    property AsInteger: Int64 read GetAsInteger write SetAsInteger;
    property AsBigInteger: TBytes read GetAsBigInteger write SetAsBigInteger;
    property AsOID: string read GetAsOID write SetAsOID;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property DataType: TScASN1DataType read GetDataType write SetDataType;
    property PreDefinedSize: Int64 write SetPreDefinedSize;

    property Name: string read FName;
    property Values[Index: integer]: TScLexemInfo read GetValue;
    property ValueCount: integer read GetValueCount write SetValueCount;
    property ValueByName[const Name: string]: TScLexemInfo read GetValueByName; default;
  end;

  TScOidLexemInfo = class(TScLexemInfo)
  private
    FTemplateOId: string;
  protected
    procedure AssignTo(Dest: TScLexemInfo); override;
  public
    constructor Create; override;
  end;

  TScComplexLexemInfo = class(TScLexemInfo)
  private
    FElems: TScLexemInfos;
    FChoiceNo: integer;
  protected
    procedure AssignTo(Dest: TScLexemInfo); override;
    function Clone(const LexemInfoClass: TScLexemInfoClass = nil): TScLexemInfo; override;
    function AddElem(DataType: TScASN1DataType; IsArray: boolean): TScLexemInfo;

    function LexemByName(const Name: string): TScLexemInfo; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function GetChoiceLexem: TScLexemInfo; override;
    procedure SetChoiceLexem(const Name: string); override;
  end;

  TScArrayLexemInfo = class(TScComplexLexemInfo)
  private
    FArray: TCRObjectList;
  protected
    function GetValue(Index: integer): TScLexemInfo; override;
    function GetValueCount: integer; override;
    procedure SetValueCount(Value: integer); override;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  TScRootLexemInfo = class(TScArrayLexemInfo)
  private
    FStream: TStream;
  public
    constructor Create; override;
  end;

  TScASN1Reader = class
  public
    class function GetAsBytes(Stream: TStream; const LexemData: TScLexemData): TBytes;
    class function GetAsString(Stream: TStream; const LexemData: TScLexemData): string;
    class function GetAsWideString(Stream: TStream; const LexemData: TScLexemData): WideString;
    class function GetAsBoolean(Stream: TStream; const LexemData: TScLexemData): boolean;
    class function GetAsInteger(Stream: TStream; const LexemData: TScLexemData): Int64;
    class function GetAsBigInteger(Stream: TStream; const LexemData: TScLexemData): TBytes;
    class function GetAsOID(Stream: TStream; const LexemData: TScLexemData): string;
    class function GetAsDateTime(Stream: TStream; const LexemData: TScLexemData): TDateTime;
  end;

  TScASN1Writer = class
  public
    class procedure SetAsBytes(var LexemData: TScLexemData; const Value: TBytes);
    class procedure SetAsString(var LexemData: TScLexemData; const Value: string);
    class procedure SetAsWideString(var LexemData: TScLexemData; const Value: WideString);
    class procedure SetAsBoolean(var LexemData: TScLexemData; Value: boolean);
    class procedure SetAsInteger(var LexemData: TScLexemData; Value: Int64);
    class procedure SetAsBigInteger(var LexemData: TScLexemData; const Value: TBytes);
    class procedure SetAsOID(var LexemData: TScLexemData; const Value: string);
    class procedure SetAsDateTime(var LexemData: TScLexemData; Value: TDateTime);
  end;

  TScASN1Compiler = class
  protected
    FLexemInfo: TScRootLexemInfo;
    FStream: TStream;

    function ReadByte(LexemInfo: TScLexemInfo; var CurOffset: Int64): byte; {$IFDEF USE_INLINE}inline;{$ENDIF}
    // class function ReadWord(LexemInfo: TScLexemInfo): word;
    // class function ReadInt32(LexemInfo: TScLexemInfo): integer;
    // class function ReadInt64(LexemInfo: TScLexemInfo): Int64; overload;
    function ReadInt64(LexemInfo: TScLexemInfo; Count: integer; var CurOffset: Int64): Int64;
    function ReadLength(LexemInfo: TScLexemInfo; var CurOffset: Int64): Int64;

    class function DataTypeNameToType(const DataTypeName: string): TScASN1DataType;
    class function IdentNoToClass(Identifier: byte): TScASN1DataType;
    class procedure ParseDescription(const Description: string; StartPos, EndPos: integer; ParentLexemInfo: TScComplexLexemInfo);
    function ParseData(ParentLexemInfo: TScComplexLexemInfo; var ParentCurOffset: Int64): boolean;

    class function CalcInfoSize(Size: Int64): integer;
    class function CalcBlockSize(LexemInfo: TScLexemInfo): Int64;
    class procedure SetSizes(LexemInfo: TScLexemInfo; out PreDefinedSize: Int64);
    class procedure SetOffsets(LexemInfo: TScLexemInfo; out PreDefinedSize: Int64);
    class procedure InternalBuild(LexemInfo: TScComplexLexemInfo; var Data: TBytes);

    function GetValueByName(const Name: string): TScLexemInfo;
    function GetRoot: TScLexemInfo;

  public
    destructor Destroy; override;

    function Build: TBytes;

    function Parse(const Description: TScASN1Description): boolean; overload;
    function Parse(const Description: TScASN1Description; const Data: TBytes): boolean; overload;
    function Parse(const Description: TScASN1Description; const Data: TBytes; Offset, Size: integer): boolean; overload;
    function Parse(const Description: TScASN1Description; Stream: TStream): boolean; overload;

    property Root: TScLexemInfo read GetRoot;
    property ValueByName[const Name: string]: TScLexemInfo read GetValueByName; default;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  DateUtils, SyncObjs,
{$IFNDEF SBRIDGE}
{$IFNDEF UNIDACPRO}
  TdsSSLConsts, TdsCertificateConsts;
{$ELSE}
  TdsSSLConstsUni, TdsCertificateConstsUni;
{$ENDIF}
{$ELSE}
  ScConsts, ScCertificateConsts;
{$ENDIF}

const
  // !!! Synchronize with TScASN1Description
  ASN1Descriptions: array [TScASN1Description] of string = (
    CERTIFICATE_DESC, EC_SIGN_DESC, X25519_PUBLIC_KEY_PARAMS_DESC,
    EC_DOMAIN_PARAMS_DESC, RSA_PUBLIC_KEY_PKCS1_DESC,
    RSA_PRIVATE_KEY_PKCS1_DESC, DSA_PUBLIC_KEY_PKCS1_DESC,
    DSA_PRIVATE_KEY_PKCS1_DESC, EC_PUBLIC_KEY_DESC, EC_POINT_DESC,
    EC_PRIVATE_KEY_DESC, RSA_PKCS8_KEY_DESC, DSA_PKCS8_KEY_DESC,
    Ed25519_PKCS8_KEY_DESC, EC_PKCS8_KEY_DESC,
    PRIVATE_KEY_PKCS8_PBES2ENC_DESC, RSAES_OAEP_PARAMS_DESC,
    RSASSA_PSS_PARAMS_DESC, ATTR_AND_VALUE_DESC, ATTRIBUTE_DESC,
    RDN_FORMAT, DISTINGUISHED_NAME_FORMAT,
    ISSUER_AND_SERIAL_NUMBER_DESC, SUBJECT_KEY_IDENTIFIER_DESC,
    RECIPIENT_KEY_IDENTIFIER_DESC, PUBLIC_KEY_DESC,
    SMIME_ATTRIBUTE_DESC, SIGNER_INFO_DESC, SIGNED_DATA_DESC,
    KEY_TRANS_RECIPIENT_INFO_DESC, KEY_AGREE_RECIPIENT_INFO_DESC,
    KEK_RECIPIENT_INFO_DESC, PASSWORD_RECIPIENT_INFO_DESC,
    ENVELOPED_DATA_DESC, DISTINGUISHED_NAMES, EC_PK_RESTRICTIONS_DESC,
    RSA_PUBLIC_KEY_OPENSSL_DESC, RSA_PUBLIC_KEY_OPENSSL_DESC2,
    DSA_PUBLIC_KEY_OPENSSL_DESC, RSA_PRIVATE_KEY_OPENSSL_DESC,
    DSA_PRIVATE_KEY_OPENSSL_DESC, DSA_CERT_PUBLIC_KEY_DESC,
    DSA_TRUNC_PUBLIC_KEY_PKCS1_DESC, PKCS8_KEY_DESC,
    DSA_PRIVATE_KEY_PKCS8_DESC, PRIVATE_KEY_PKCS8ENC_DESC,
    PBES2_PARAMS_DESC, PBES1_PARAMS_DESC, PKCS12_PBE_PARAMS_DESC,
    BASIC_CONSTRAINTS_EXTENSION_DESC, BASIC_CONSTRAINTS2_EXTENSION_DESC,
    KEY_USAGE_EXTENSION_DESC, EXTENDED_KEY_USAGE_EXTENSION_DESC,
    SUBJECT_KEY_ID_EXTENSION_DESC, CERTIFICATE_POLICIES_EXTENSION_DESC,
    POLICY_MAPPINGS_EXTENSION_DESC, ALTERNATIVE_NAME_EXTENSION_DESC,
    SUBJECT_DIRECTORY_ATTRIBUTES_EXTENSION_DESC,
    CRL_DISTRIBUTION_POINTS_EXTENSION_DESC, AUTHORITY_INFO_ACCESS_EXTENSION_DESC,
    SUBJECT_INFO_ACCESS_EXTENSION_DESC, CRL_NUMBER_EXTENSION_DESC,
    CRL_ISSUING_DISTRIBUTION_POINT_EXTENSION_DESC, CRL_REASON_EXTENSION_DESC,
    CRL_INVALIDITY_DATE_EXTENSION_DESC, CRL_CERTIFICATE_ISSUER_EXTENSION_DESC,
    SIGNED_CERTIFICATE_TIMESTAMP_LIST_DESC,
    CERTIFICATE_LIST_DESC, PARTIAL_CERTIFICATE_LIST_DESC, EXTENSIONS_DESC,
    AUTHORITY_KEY_ID_EXTENSION_DESC, AUTHORITY_KEY_ID_EXTENSION_DESC2,
    ATTRIBUTES_DESC, SIGNED_ATTRS_DESC, UNSIGNED_ATTRS_DESC,
    PFX_DESC, AUTH_SAFE_DESC_EX, ENCRYPTED_DATA_DESC,
    SAFE_CONTENTS_DESC_EX, SAFE_BAG_VALUE_DESC
  );

var
  ASN1ParsedDescriptionsLock: TCriticalSection;
  ASN1ParsedDescriptions: array [TScASN1Description] of TScLexemInfo;

type
  TASN1ClassInfo = record
    No: byte;
    Desc: string;
  end;

const
  ASN1DataTypes: array [TScASN1DataType] of TASN1ClassInfo = (
    (No: $3F{dtAny};              Desc: 'ANY'),
    (No: $3F{dtImplicitArray};    Desc: 'ARRAY'),
    (No: $3F{dtChoice};           Desc: 'CHOICE'),
    (No: $3F{dtSimpleChoice};     Desc: 'CHOICE'),
    (No: $01{dtBoolean};          Desc: 'BOOL'),
    (No: $02{dtInteger};          Desc: 'INT'),
    (No: $03{dtBit};              Desc: 'BIT'),
    (No: $04{dtOctetString};      Desc: 'OCTSTR'),
    (No: $05{dtNull};             Desc: 'NULL'),
    (No: $06{dtObjectIdentifier}; Desc: 'OID'),
    (No: $0A{dtEnumerated};       Desc: 'ENUM'),
    (No: $0C{dtUTF8String};       Desc: 'UTF8STR'),
    (No: $12{dtNumericString};    Desc: 'NUMERICSTR'),
    (No: $13{dtPrintableString};  Desc: 'PRINTABLESTR'),
    (No: $14{dtTeletexString};    Desc: 'TELETEXSTR'),
    (No: $15{dtVideotexString};   Desc: 'VIDEOTEXSTR'),
    (No: $16{dtIA5String};        Desc: 'IA5STR'),
    (No: $19{dtGraphicString};    Desc: 'GRAPHICSTR'),
    (No: $1A{dtVisibleString};    Desc: 'VISIBLESTR'),
    (No: $1B{dtGeneralString};    Desc: 'GENERALSTR'),
    (No: $1C{dtUniversalString};  Desc: 'UNIVERSALSTR'),
    (No: $1E{dtBMPString};        Desc: 'BMPSTR'),
    (No: $17{dtUTCTime};          Desc: 'UTCTIME'),
    (No: $18{dtGeneralizedTime};  Desc: 'GENTIME'),
    (No: $30{dtSequence};         Desc: 'SEQ'),
    (No: $31{dtSet};              Desc: 'SET')
  );

const
  SimpleTypes = [
    dtBoolean, dtInteger, dtNull, dtEnumerated,
    dtUTF8String, dtNumericString, dtPrintableString, dtTeletexString, dtVideotexString,
    dtIA5String, dtGraphicString, dtVisibleString, dtGeneralString, dtUniversalString, dtBMPString,
    dtUTCTime, dtGeneralizedTime
  ];

const
  TwoHexLookup: packed array[0..255] of array[1..2] of Char =
  ('00','01','02','03','04','05','06','07','08','09','0A','0B','0C','0D','0E','0F',
   '10','11','12','13','14','15','16','17','18','19','1A','1B','1C','1D','1E','1F',
   '20','21','22','23','24','25','26','27','28','29','2A','2B','2C','2D','2E','2F',
   '30','31','32','33','34','35','36','37','38','39','3A','3B','3C','3D','3E','3F',
   '40','41','42','43','44','45','46','47','48','49','4A','4B','4C','4D','4E','4F',
   '50','51','52','53','54','55','56','57','58','59','5A','5B','5C','5D','5E','5F',
   '60','61','62','63','64','65','66','67','68','69','6A','6B','6C','6D','6E','6F',
   '70','71','72','73','74','75','76','77','78','79','7A','7B','7C','7D','7E','7F',
   '80','81','82','83','84','85','86','87','88','89','8A','8B','8C','8D','8E','8F',
   '90','91','92','93','94','95','96','97','98','99','9A','9B','9C','9D','9E','9F',
   'A0','A1','A2','A3','A4','A5','A6','A7','A8','A9','AA','AB','AC','AD','AE','AF',
   'B0','B1','B2','B3','B4','B5','B6','B7','B8','B9','BA','BB','BC','BD','BE','BF',
   'C0','C1','C2','C3','C4','C5','C6','C7','C8','C9','CA','CB','CC','CD','CE','CF',
   'D0','D1','D2','D3','D4','D5','D6','D7','D8','D9','DA','DB','DC','DD','DE','DF',
   'E0','E1','E2','E3','E4','E5','E6','E7','E8','E9','EA','EB','EC','ED','EE','EF',
   'F0','F1','F2','F3','F4','F5','F6','F7','F8','F9','FA','FB','FC','FD','FE','FF');

{ TScLexemInfo }

constructor TScLexemInfo.Create;
begin
  inherited;

  FStatus := [lsIsNull];
end;

destructor TScLexemInfo.Destroy;
begin
  inherited;
end;

procedure TScLexemInfo.AssignTo(Dest: TScLexemInfo);
begin
  Dest.FLexemData.StartPos := FLexemData.StartPos;
  Dest.FLexemData.AddedSize := FLexemData.AddedSize;
  Dest.FLexemData.Size := FLexemData.Size;
  Dest.FLexemData.DataType := FLexemData.DataType;

  Dest.FName := FName;
  Dest.FIdentifier := FIdentifier;
  Dest.FStatus := FStatus;
  Dest.FChoiceDataTypes := FChoiceDataTypes;
end;

function TScLexemInfo.Clone(const LexemInfoClass: TScLexemInfoClass = nil): TScLexemInfo;
begin
  if LexemInfoClass <> nil then
    Result := TScLexemInfoClass(LexemInfoClass).Create
  else
    Result := TScLexemInfoClass(Self.ClassType).Create;

  Self.AssignTo(Result);
  Result.FParentLexemInfo := PScLexemInfo(Self);
end;

function TScLexemInfo.GetIsNull: boolean;
begin
  Result := lsIsNull in FStatus;
end;

function TScLexemInfo.GetDataType: TScASN1DataType;
begin
  Result := FLexemData.DataType;
end;

procedure TScLexemInfo.SetDataType(Value: TScASN1DataType);
begin
  if FLexemData.DataType <> dtAny then
    raise EScError.Create(seDataTypeIsSet);

  FLexemData.DataType := Value;
end;

function TScLexemInfo.GetValue(Index: integer): TScLexemInfo;
begin
{$IFDEF FPC}
  Result := nil;
{$ENDIF}
  raise EScError.Create(seInvalidInputArgs);
end;

function TScLexemInfo.GetValueCount: integer;
begin
  Result := 0;
end;

procedure TScLexemInfo.SetValueCount(Value: integer);
begin
  raise EScError.Create(seCannotConvert);
end;

procedure TScLexemInfo.SetParentNotNulls;
var
  li: TScLexemInfo;
begin
  if FParentLexemInfo <> nil then begin
    li := TScLexemInfo(FParentLexemInfo);
    Exclude(li.FStatus, lsIsNull);
    li.SetParentNotNulls;
  end;
end;

function TScLexemInfo.GetEncodedData: TBytes;
var
  Stream: TStream;
begin
  SetLength(Result, FLexemData.Size + FLexemData.AddedSize);

  Stream := GetStream;
  if (Stream <> nil) and (Length(Result) > 0) then begin
    Stream.Position := FLexemData.StartPos;
    Stream.Read(Result[0], Length(Result));
  end;
end;

procedure TScLexemInfo.SetEncodedData(const Value: TBytes);
begin
  FLexemData.Buffer := Value;
  Include(FStatus, lsIsEncodedData);
  Exclude(FStatus, lsIsNull);
  SetParentNotNulls;
end;

function TScLexemInfo.GetAsBytes: TBytes;
begin
  Result := TScASN1Reader.GetAsBytes(GetStream, FLexemData);
end;

procedure TScLexemInfo.SetAsBytes(const Value: TBytes);
begin
  TScASN1Writer.SetAsBytes(FLexemData, Value);
  Exclude(FStatus, lsIsNull);
  SetParentNotNulls;
end;

function TScLexemInfo.GetAsString: string;
begin
  Result := TScASN1Reader.GetAsString(GetStream, FLexemData);
end;

procedure TScLexemInfo.SetAsString(const Value: string);
begin
  TScASN1Writer.SetAsString(FLexemData, Value);
  Exclude(FStatus, lsIsNull);
  SetParentNotNulls;
end;

function TScLexemInfo.GetAsWideString: WideString;
begin
  Result := TScASN1Reader.GetAsWideString(GetStream, FLexemData);
end;

procedure TScLexemInfo.SetAsWideString(const Value: WideString);
begin
  TScASN1Writer.SetAsWideString(FLexemData, Value);
  Exclude(FStatus, lsIsNull);
  SetParentNotNulls;
end;

function TScLexemInfo.GetAsBoolean: boolean;
begin
  Result := TScASN1Reader.GetAsBoolean(GetStream, FLexemData);
end;

procedure TScLexemInfo.SetAsBoolean(Value: boolean);
begin
  TScASN1Writer.SetAsBoolean(FLexemData, Value);
  Exclude(FStatus, lsIsNull);
  SetParentNotNulls;
end;

function TScLexemInfo.GetAsInteger: Int64;
begin
  Result := TScASN1Reader.GetAsInteger(GetStream, FLexemData);
end;

procedure TScLexemInfo.SetAsInteger(Value: Int64);
begin
  TScASN1Writer.SetAsInteger(FLexemData, Value);
  Exclude(FStatus, lsIsNull);
  SetParentNotNulls;
end;

function TScLexemInfo.GetAsBigInteger: TBytes;
begin
  Result := TScASN1Reader.GetAsBigInteger(GetStream, FLexemData);
end;

procedure TScLexemInfo.SetAsBigInteger(const Value: TBytes);
begin
  TScASN1Writer.SetAsBigInteger(FLexemData, Value);
  Exclude(FStatus, lsIsNull);
  SetParentNotNulls;
end;

function TScLexemInfo.GetAsOID: string;
begin
  Result := TScASN1Reader.GetAsOID(GetStream, FLexemData);
end;

procedure TScLexemInfo.SetAsOID(const Value: string);
begin
  TScASN1Writer.SetAsOID(FLexemData, Value);
  Exclude(FStatus, lsIsNull);
  SetParentNotNulls;
end;

function TScLexemInfo.GetAsDateTime: TDateTime;
begin
  Result := TScASN1Reader.GetAsDateTime(GetStream, FLexemData);
end;

procedure TScLexemInfo.SetAsDateTime(const Value: TDateTime);
begin
  TScASN1Writer.SetAsDateTime(FLexemData, Value);
  Exclude(FStatus, lsIsNull);
  SetParentNotNulls;
end;

function TScLexemInfo.AsStreamInfo: TScStreamInfoRec;
begin
  if not (lsIsNull in FStatus) and (GetStream = nil) then
    raise EScError.Create(seInternalError);

  Result.Stream := GetStream;
  Result.Position := FLexemData.StartPos + FLexemData.AddedSize;
  Result.Count := FLexemData.Size;
end;

procedure TScLexemInfo.SetPreDefinedSize(Value: Int64);
begin
  if Value < 0 then
    raise EScError.Create(seInvalidInputArgs);

  FLexemData.Size := Value;
  if Value > 0 then
    Include(FStatus, lsIsPreDefined)
  else
    Exclude(FStatus, lsIsPreDefined);

  Exclude(FStatus, lsIsNull);
  SetParentNotNulls;
end;

function TScLexemInfo.GetStream: TStream;
var
  RootLexemInfo: TScLexemInfo;
begin
  RootLexemInfo := Self;
  while RootLexemInfo.FParentLexemInfo <> nil do
    RootLexemInfo := TScLexemInfo(RootLexemInfo.FParentLexemInfo);

  if RootLexemInfo is TScRootLexemInfo then
    Result := TScRootLexemInfo(RootLexemInfo).FStream
  else
    raise EScError.Create(seInvalidInputArgs);
end;

function TScLexemInfo.GetOffset: Int64;
begin
  Result := FLexemData.StartPos;
end;

function TScLexemInfo.GetChoiceLexem: TScLexemInfo;
begin
  Result := Self;
end;

procedure TScLexemInfo.SetChoiceLexem(const Name: string);
begin
  raise EScError.Create(seInvalidInputArgs);
end;

function TScLexemInfo.GetValueByName(const Name: string): TScLexemInfo;
begin
  Result := LexemByName(Name);
  if (Result <> nil) and (Result.FLexemData.DataType = dtChoice) and ((Result as TScComplexLexemInfo).FChoiceNo > -1) then
    Result := TScComplexLexemInfo(Result).FElems[TScComplexLexemInfo(Result).FChoiceNo];
end;

function TScLexemInfo.LexemByName(const Name: string): TScLexemInfo;
begin
{$IFDEF FPC}
  Result := nil;
{$ENDIF}
  raise EScError.CreateFmt(SLexemNotFound, [Name], seLexemNotFound);
end;

{ TScOidLexemInfo }

constructor TScOidLexemInfo.Create;
begin
  inherited;
end;

procedure TScOidLexemInfo.AssignTo(Dest: TScLexemInfo);
begin
  inherited AssignTo(Dest);

  if Dest is TScOidLexemInfo then
    TScOidLexemInfo(Dest).FTemplateOId := FTemplateOId;
end;

{ TScComplexLexemInfo }

constructor TScComplexLexemInfo.Create;
begin
  inherited;

  FChoiceNo := -1;
end;

destructor TScComplexLexemInfo.Destroy;
var
  i: integer;
begin
  for i := 0 to Length(FElems) - 1 do
    FElems[i].Free;

  inherited;
end;

procedure TScComplexLexemInfo.AssignTo(Dest: TScLexemInfo);
begin
  inherited AssignTo(Dest);

  if Dest is TScComplexLexemInfo then
    TScComplexLexemInfo(Dest).FChoiceNo := FChoiceNo;
end;

function TScComplexLexemInfo.Clone(const LexemInfoClass: TScLexemInfoClass = nil): TScLexemInfo;
var
  Len: integer;
  i: integer;
begin
  Result := inherited Clone(LexemInfoClass);

  Len := Length(FElems);
  if Len = 0 then
    Exit;

  SetLength(TScComplexLexemInfo(Result).FElems, Len);
  for i := 0 to Len - 1 do begin
    TScComplexLexemInfo(Result).FElems[i] := FElems[i].Clone;
    TScComplexLexemInfo(Result).FElems[i].FParentLexemInfo := PScLexemInfo(Result);
  end;
end;

function TScComplexLexemInfo.AddElem(DataType: TScASN1DataType; IsArray: boolean): TScLexemInfo;
var
  l: integer;
begin
  l := Length(FElems);
  SetLength(FElems, l + 1);

  if IsArray then
    FElems[l] := TScArrayLexemInfo.Create
  else
  if DataType in [dtAny, dtChoice, dtSequence, dtSet, dtOctetString, dtBit, dtImplicitArray] then
    FElems[l] := TScComplexLexemInfo.Create
  else
  if DataType = dtObjectIdentifier then
    FElems[l] := TScOidLexemInfo.Create
  else
    FElems[l] := TScLexemInfo.Create;

  FElems[l].FLexemData.DataType := DataType;
  FElems[l].FParentLexemInfo := PScLexemInfo(Self);
  Result := FElems[l];
end;

function TScComplexLexemInfo.LexemByName(const Name: string): TScLexemInfo;

  function FindByName(LexemInfo: TScComplexLexemInfo): TScLexemInfo;
  var
    TmpLexemInfo: TScLexemInfo;
    CompTmpLexemInfo: TScComplexLexemInfo;
    i: integer;
  begin
    Result := nil;

    if (LexemInfo.ClassType = TScArrayLexemInfo) and (TScArrayLexemInfo(LexemInfo).FArray <> nil) then begin
      for i := 0 to TScArrayLexemInfo(LexemInfo).FArray.Count - 1 do begin
        TmpLexemInfo := TScLexemInfo(TScArrayLexemInfo(LexemInfo).FArray[i]);
        if TmpLexemInfo is TScComplexLexemInfo then
          Result := FindByName(TScComplexLexemInfo(TmpLexemInfo))
        else
          Result := nil;

        if Result <> nil then
          Exit;
      end;
    end
    else begin
      for i := 0 to Length(LexemInfo.FElems) - 1 do begin
        if SameText(LexemInfo.FElems[i].FName, Name) then begin
          Result := LexemInfo.FElems[i];
          Exit;
        end;
      end;

      for i := 0 to Length(LexemInfo.FElems) - 1 do begin
        if LexemInfo.FElems[i] is TScComplexLexemInfo then begin
          CompTmpLexemInfo := TScComplexLexemInfo(LexemInfo.FElems[i]);
          if (CompTmpLexemInfo.FLexemData.DataType = dtChoice) and (CompTmpLexemInfo.FChoiceNo > -1) then begin
            TmpLexemInfo := CompTmpLexemInfo.FElems[CompTmpLexemInfo.FChoiceNo];
            if SameText(TmpLexemInfo.FName, Name) then
              Result := TmpLexemInfo
            else
              if TmpLexemInfo is TScComplexLexemInfo then
                Result := FindByName(TScComplexLexemInfo(TmpLexemInfo));
          end
          else
          if Length(CompTmpLexemInfo.FElems) > 0 then
            Result := FindByName(CompTmpLexemInfo);

          if Result <> nil then
            Exit;
        end;
      end;
    end;
  end;

begin
  Result := FindByName(Self);

  if Result = nil then
    raise EScError.CreateFmt(SLexemNotFound, [Name], seLexemNotFound);
end;

function TScComplexLexemInfo.GetChoiceLexem: TScLexemInfo;
begin
  if (FLexemData.DataType = dtChoice) and (FChoiceNo > -1) then
    Result := FElems[FChoiceNo]
  else
    Result := Self;
end;

procedure TScComplexLexemInfo.SetChoiceLexem(const Name: string);
var
  i: integer;
begin
  if FLexemData.DataType <> dtChoice then
    raise EScError.Create(seInvalidInputArgs);

  for i := 0 to Length(FElems) - 1 do begin
    if SameText(FElems[i].FName, Name) then begin
      FChoiceNo := i;
      Exit;
    end;
  end;

  raise EScError.Create(seInvalidInputArgs);
end;

{ TScArrayLexemInfo }

constructor TScArrayLexemInfo.Create;
begin
  inherited;
end;

destructor TScArrayLexemInfo.Destroy;
begin
  FArray.Free;

  inherited;
end;

function TScArrayLexemInfo.GetValue(Index: integer): TScLexemInfo;
begin
  if FArray = nil then
    raise EScError.Create(seInvalidInputArgs);

  Result := TScLexemInfo(FArray[Index]);
end;

function TScArrayLexemInfo.GetValueCount: integer;
begin
  if FArray = nil then
    Result := 0
  else
    Result := FArray.Count;
end;

procedure TScArrayLexemInfo.SetValueCount(Value: integer);
begin
  if not (FLexemData.DataType in [dtImplicitArray, dtSequence, dtSet]) then
    raise EScError.Create(seCannotConvert);

  if Value <= 0 then begin
    FreeAndNil(FArray);
    Exit;
  end;

  if FArray = nil then
    FArray := TCRObjectList.Create;

  if FArray.Count > Value then begin
    while FArray.Count > Value do
      FArray.Delete(FArray.Count);
  end
  else
  if FArray.Count < Value then begin
    while FArray.Count < Value do
      FArray.Add(Self.Clone);
  end;
end;

{ TScRootLexemInfo }

constructor TScRootLexemInfo.Create;
begin
  inherited;
end;

{ TScASN1Reader }

class function TScASN1Reader.GetAsBytes(Stream: TStream; const LexemData: TScLexemData): TBytes;
begin
  if (LexemData.DataType = dtBit) and (LexemData.Size > 0) then begin
    SetLength(Result, LexemData.Size - 1);
    if LexemData.Size > 1 then begin
      Stream.Position := LexemData.StartPos + LexemData.AddedSize + 1;
      Stream.Read(Result[0], LexemData.Size - 1);
    end;
  end
  else begin
    SetLength(Result, LexemData.Size);
    if LexemData.Size > 0 then begin
      Stream.Position := LexemData.StartPos + LexemData.AddedSize;
      Stream.Read(Result[0], LexemData.Size);
    end;
  end;
end;

class function TScASN1Reader.GetAsString(Stream: TStream; const LexemData: TScLexemData): string;
var
  Buf: TBytes;
  ws: WideString;
  sa: AnsiString;
  i, p: integer;
begin
  case LexemData.DataType of
    dtBoolean:
      Result := BoolToStr(GetAsBoolean(Stream, LexemData), True);

    dtInteger, dtEnumerated: begin
      SetLength(Result, LexemData.Size * 2);
      SetLength(Buf, LexemData.Size);
      if LexemData.Size > 0 then begin
        Stream.Position := LexemData.StartPos + LexemData.AddedSize;
        Stream.Read(Buf[0], LexemData.Size);
      end;

      p := 1;
      for i := 0 to LexemData.Size - 1 do begin
        Result[p] := TwoHexLookup[Buf[i]][1];
        Result[p + 1] := TwoHexLookup[Buf[i]][2];
        Inc(p, 2);
      end;
    end;

    dtBit: begin
      if LexemData.Size <= 1 then begin
        Result := '';
      end
      else begin
        SetLength(Result, (LexemData.Size - 1) * 2);
        SetLength(Buf, LexemData.Size);
        if LexemData.Size > 0 then begin
          Stream.Position := LexemData.StartPos + LexemData.AddedSize;
          Stream.Read(Buf[0], LexemData.Size);
        end;

        p := 1;
        for i := 1 to LexemData.Size - 1 do begin
          Result[p] := TwoHexLookup[Buf[i]][1];
          Result[p + 1] := TwoHexLookup[Buf[i]][2];
          Inc(p, 2);
        end;
      end;
    end;

    dtNull:
      Result := '';

    dtObjectIdentifier:
      Result := GetAsOID(Stream, LexemData);

    dtUTCTime, dtGeneralizedTime:
      Result := DateTimeToStr(GetAsDateTime(Stream, LexemData));

    dtUTF8String: begin
      SetLengthA(sa, LexemData.Size);
      if LexemData.Size > 0 then begin
        Stream.Position := LexemData.StartPos + LexemData.AddedSize;
        Stream.Read(PAnsiChar(sa)^, LexemData.Size);
      end;

      Result := string({$IFNDEF SBRIDGE}CRFunctions{$ELSE}ScFunctions{$ENDIF}.UTF8Decode(sa));
    end;

    dtUniversalString: begin
      SetLength(ws, LexemData.Size shr 1);
      if LexemData.Size > 0 then begin
        Stream.Position := LexemData.StartPos + LexemData.AddedSize;
        Stream.Read(ws[1], LexemData.Size);
      end;
      Result := string(ws);
    end;

    dtBMPString: begin
      SetLength(Buf, LexemData.Size);
      if LexemData.Size > 0 then begin
        Stream.Position := LexemData.StartPos + LexemData.AddedSize;
        Stream.Read(Buf[0], LexemData.Size);
      end;
      Result := Encoding.BigEndianUnicode.GetString(Buf);
    end;

    dtNumericString, dtPrintableString, dtTeletexString, dtVideotexString,
    dtIA5String, dtGraphicString, dtVisibleString, dtGeneralString: begin
      SetLengthA(sa, LexemData.Size);
      if LexemData.Size > 0 then begin
        Stream.Position := LexemData.StartPos + LexemData.AddedSize;
        Stream.Read(PAnsiChar(sa)^, LexemData.Size);
      end;

      Result := string(sa);
    end;

    else
      Result := BytesToHexStr(GetAsBytes(Stream, LexemData), ' ');
  end;
end;

class function TScASN1Reader.GetAsWideString(Stream: TStream; const LexemData: TScLexemData): WideString;
var
  Buf: TBytes;
  sa: AnsiString;
begin
  case LexemData.DataType of
    dtUTF8String: begin
      SetLengthA(sa, LexemData.Size);
      if LexemData.Size > 0 then begin
        Stream.Position := LexemData.StartPos + LexemData.AddedSize;
        Stream.Read(PAnsiChar(sa)^, LexemData.Size);
      end;

      Result := {$IFNDEF SBRIDGE}CRFunctions{$ELSE}ScFunctions{$ENDIF}.UTF8Decode(sa);
    end;

    dtUniversalString: begin
      SetLength(Result, LexemData.Size shr 1);
      if LexemData.Size > 0 then begin
        Stream.Position := LexemData.StartPos + LexemData.AddedSize;
        Stream.Read(Result[1], LexemData.Size);
      end;
    end;

    dtBMPString: begin
      SetLength(Buf, LexemData.Size);
      if LexemData.Size > 0 then begin
        Stream.Position := LexemData.StartPos + LexemData.AddedSize;
        Stream.Read(Buf[0], LexemData.Size);
      end;
      Result := {$IFNDEF NEXTGEN}Encoding.BigEndianUnicode.GetWideString(Buf){$ELSE}WideString(Encoding.BigEndianUnicode.GetString(Buf)){$ENDIF}
    end;

    dtNumericString, dtPrintableString, dtTeletexString, dtVideotexString,
    dtIA5String, dtGraphicString, dtVisibleString, dtGeneralString: begin
      SetLengthA(sa, LexemData.Size);
      if LexemData.Size > 0 then begin
        Stream.Position := LexemData.StartPos + LexemData.AddedSize;
        Stream.Read(PAnsiChar(sa)^, LexemData.Size);
      end;

      Result := WideString(sa);
    end;

    else
      Result := WideString(GetAsString(Stream, LexemData));
  end;
end;

class function TScASN1Reader.GetAsBoolean(Stream: TStream; const LexemData: TScLexemData): boolean;
var
  b: byte;
begin
  if LexemData.DataType <> dtBoolean then
    raise EScError.Create(seCannotConvert);

  if LexemData.Size = 0 then
    Result := False
  else begin
    Stream.Position := LexemData.StartPos + LexemData.AddedSize;
    Stream.Read(b, 1);
    Result := b <> 0;
  end;
end;

class function TScASN1Reader.GetAsInteger(Stream: TStream; const LexemData: TScLexemData): Int64;
var
  Buf: array [0..7] of byte;
  i: integer;
begin
  if not (LexemData.DataType in [dtInteger, dtEnumerated]) or (LexemData.Size > 8) then
    raise EScError.Create(seCannotConvert);

  if LexemData.Size > 0 then begin
    Stream.Position := LexemData.StartPos + LexemData.AddedSize;
    Stream.Read(Buf[0], LexemData.Size);
  end;

  Result := 0;
  for i := 0 to LexemData.Size - 1 do
    Result := (Result shl 8) + Buf[i];
end;

class function TScASN1Reader.GetAsBigInteger(Stream: TStream; const LexemData: TScLexemData): TBytes;
var
  Buf: TBytes;
  Len, Off: integer;
begin
  if LexemData.DataType <> dtInteger then
    raise EScError.Create(seCannotConvert);

  if LexemData.Size > 0 then begin
    SetLength(Buf, LexemData.Size);
    Stream.Position := LexemData.StartPos + LexemData.AddedSize;
    Stream.Read(Buf[0], LexemData.Size);
  end;

  Off := 0;
  Len := LexemData.Size;

  while (Len > 1) and (Buf[Off] = 0) do begin
    Inc(Off);
    Dec(Len);
  end;

  SetLength(Result, Len);
  if Len > 0 then
    Move(Buf[Off], Result[0], Len);
end;

class function TScASN1Reader.GetAsOID(Stream: TStream; const LexemData: TScLexemData): string;
var
  Buf: TBytes;
  OId: TOId;
  Offset, Size: Int64;
  Idx: integer;
  i: integer;
begin
  if LexemData.DataType <> dtObjectIdentifier then
    raise EScError.Create(seCannotConvert);

  if LexemData.Size <= 0 then begin
    Result := '';
    Exit;
  end;

  SetLength(Buf, LexemData.Size);
  Stream.Position := LexemData.StartPos + LexemData.AddedSize;
  Stream.Read(Buf[0], LexemData.Size);

  Offset := 0;
  Size := LexemData.Size;
  SetLength(OId, Size + 1);

  if Buf[Offset] >= 80 then begin
    OId[0] := 2;
    OId[1] := Buf[Offset] - 80;
  end
  else
  if Buf[Offset] >= 40 then begin
    OId[0] := 1;
    OId[1] := Buf[Offset] - 40;
  end
  else begin
    OId[0] := 0;
    OId[1] := Buf[Offset];
  end;
  Idx := 2;
  Inc(Offset);
  Dec(Size);

  while Size > 0 do begin
    OId[Idx] := 0;
    while Buf[Offset] >= $80 do begin
      OId[Idx] := (OId[Idx] shl 7) + (Buf[Offset] and $7F);
      Inc(Offset);
      Dec(Size);
      if Size = 0 then
        raise EScError.Create(seWrongDataFormat);
    end;
    OId[Idx] := (OId[Idx] shl 7) + Buf[Offset];
    Inc(Offset);
    Dec(Size);

    Inc(Idx);
  end;

  Result := '';
  for i := 0 to Idx - 1 do begin
    if i > 0 then
      Result := Result + '.';
    Result := Result + IntToStr(OId[i]);
  end;
end;

{$IFNDEF MSWINDOWS}
type
  TSystemTime = record
    wYear: Word;
    wMonth: Word;
    wDayOfWeek: Word;
    wDay: Word;
    wHour: Word;
    wMinute: Word;
    wSecond: Word;
    wMilliseconds: Word;
  end;
{$ENDIF}

const
  UTC_TIME_LEN = 13; // 'YYMMDDHHMMSSZ'
  GENERALIZED_TIME_LEN = 15; // 'YYYYMMDDHHMMSSZ'

class function TScASN1Reader.GetAsDateTime(Stream: TStream; const LexemData: TScLexemData): TDateTime;
var
  Buf: TBytes;
  IsUtc: boolean;
  UtcTime: TSystemTime;
{$IFDEF MSWINDOWS}
  LocalTime: TSystemTime;
  TZ: TTimeZoneInformation;
{$ENDIF}
  Off: Int64;
begin
  if LexemData.Size <= 0 then begin
    Result := 0;
    Exit;
  end;

  if (LexemData.DataType = dtUTCTime) and (LexemData.Size = UTC_TIME_LEN) then
    IsUtc := True
  else
  if (LexemData.DataType = dtGeneralizedTime) and (LexemData.Size = GENERALIZED_TIME_LEN) then
    IsUtc := False
  else
    raise EScError.Create(seCannotConvert);

  SetLength(Buf, LexemData.Size);
  Stream.Position := LexemData.StartPos + LexemData.AddedSize;
  Stream.Read(Buf[0], LexemData.Size);

  if Buf[LexemData.Size - 1] <> Ord('Z') then
    raise EScError.Create(seWrongDataFormat);

  Off := 0;

  if IsUtc then begin
    UtcTime.wYear := (Buf[Off] - Ord('0')) * 10 + (Buf[Off + 1] - Ord('0'));
    Inc(Off, 2);

    // 0 <= year < 50 : assume century 21
    // 50 <= year < 70 : illegal per PKIX
    // 70 < year <= 99 : assume century 20
    if UtcTime.wYear < 50 then
      UtcTime.wYear := UtcTime.wYear + 2000
    else
      UtcTime.wYear := UtcTime.wYear + 1900;
  end
  else begin
    UtcTime.wYear := (Buf[Off] - Ord('0')) * 1000 +
                     (Buf[Off + 1] - Ord('0')) * 100 +
                     (Buf[Off + 2] - Ord('0')) * 10 +
                     (Buf[Off + 3] - Ord('0'));
    Inc(Off, 4);
  end;

  UtcTime.wMonth := (Buf[Off] - Ord('0')) * 10 + (Buf[Off + 1] - Ord('0'));
  UtcTime.wDay := (Buf[Off + 2] - Ord('0')) * 10 + (Buf[Off + 3] - Ord('0'));
  UtcTime.wHour := (Buf[Off + 4] - Ord('0')) * 10 + (Buf[Off + 5] - Ord('0'));
  UtcTime.wMinute := (Buf[Off + 6] - Ord('0')) * 10 + (Buf[Off + 7] - Ord('0'));
  UtcTime.wSecond := (Buf[Off + 8] - Ord('0')) * 10 + (Buf[Off + 9] - Ord('0'));
  UtcTime.wMilliseconds := 0;

{$IFDEF MSWINDOWS}
  GetTimeZoneInformation(TZ);
  if GetTimeZoneInformation(TZ) = TIME_ZONE_ID_STANDARD then
    TZ.DaylightBias := TZ.StandardBias;
  SystemTimeToTzSpecificLocalTime(@TZ, UtcTime, LocalTime);
  Result := SystemTimeToDateTime(LocalTime);
{$ELSE}
  Result := EncodeDateTime(UtcTime.wYear, UtcTime.wMonth, UtcTime.wDay, UtcTime.wHour, UtcTime.wMinute, UtcTime.wSecond, UtcTime.wMilliseconds);
  {$IFDEF POSIX}
  Result := TTimeZone.Local.ToLocalTime(Result);
  {$ENDIF}
{$ENDIF}
end;

{ TScASN1Writer }

class procedure TScASN1Writer.SetAsBytes(var LexemData: TScLexemData; const Value: TBytes);
begin
  if LexemData.DataType = dtBit then begin
    SetLength(LexemData.Buffer, Length(Value) + 1);
    LexemData.Buffer[0] := 0;
    if Length(Value) > 0 then
      Move(Value[0], LexemData.Buffer[1], Length(Value));
  end
  else
    LexemData.Buffer := Value;
end;

class procedure TScASN1Writer.SetAsString(var LexemData: TScLexemData; const Value: string);

  function HexCharToByte(const Ch: Char): byte;
  begin
    case Ch of
     '0'..'9':
       Result := Ord(Ch) - Ord('0');
     'A'..'F':
       Result := Ord(Ch) - Ord('A') + 10;
     'a'..'f':
       Result := Ord(Ch) - Ord('a') + 10;
    else
      Result := 0;
    end;
  end;

var
  Buf: TBytes;
  Len: integer;
  b1, b2: byte;
  p, i: integer;
begin
  case LexemData.DataType of
    dtBoolean:
      SetAsBoolean(LexemData, StrToBool(Value));

    dtInteger, dtEnumerated:
      LexemData.Buffer := HexToBytes(Value);

    dtBit: begin
    {$IFNDEF VER10P}
      SetLength(Buf, 0);
    {$ENDIF}
      Buf := HexToBytes(Value);
      SetLength(LexemData.Buffer, Length(Buf) + 1);
      LexemData.Buffer[0] := 0;
      Move(Buf[0], LexemData.Buffer[1], Length(Buf));
    end;

    dtNull:
      SetLength(LexemData.Buffer, 0);

    dtObjectIdentifier:
      SetAsOID(LexemData, Value);

    dtUTCTime, dtGeneralizedTime:
      SetAsDateTime(LexemData, StrToDateTime(Value));

    dtUTF8String:
      LexemData.Buffer := Encoding.UTF8.GetBytes(Value);

    dtUniversalString:
      LexemData.Buffer := Encoding.Unicode.GetBytes(Value);

    dtBMPString:
      LexemData.Buffer := Encoding.BigEndianUnicode.GetBytes(Value);

    dtNumericString, dtPrintableString, dtTeletexString, dtVideotexString,
    dtIA5String, dtGraphicString, dtVisibleString, dtGeneralString:
      LexemData.Buffer := Encoding.Default.GetBytes(Value);

    else begin
      Len := Length(Value);
      SetLength(LexemData.Buffer, Len);

      p := 1;
      i := 0;
      while p < Len do begin
        b1 := HexCharToByte(Value[p]);
        b2 := HexCharToByte(Value[p + 1]);
        Inc(p, 2);
        LexemData.Buffer[i] := (b1 shl 4) + b2;
        Inc(i);
        if (p <= Len) and CharInSet(Value[p], [' ', ':']) then
          Inc(p);
      end;

      SetLength(LexemData.Buffer, i);
    end;
  end;
end;

class procedure TScASN1Writer.SetAsWideString(var LexemData: TScLexemData; const Value: WideString);
begin
  case LexemData.DataType of
    dtUTF8String:
      LexemData.Buffer := Encoding.UTF8.GetBytes(Value);

    dtUniversalString:
      LexemData.Buffer := Encoding.Unicode.GetBytes(Value);

    dtBMPString:
      LexemData.Buffer := Encoding.BigEndianUnicode.GetBytes(Value);

    dtNumericString, dtPrintableString, dtTeletexString, dtVideotexString,
    dtIA5String, dtGraphicString, dtVisibleString, dtGeneralString:
      LexemData.Buffer := Encoding.Default.GetBytes(Value);

    else
      SetAsString(LexemData, string(Value));
  end;
end;

class procedure TScASN1Writer.SetAsBoolean(var LexemData: TScLexemData; Value: boolean);
begin
  if LexemData.DataType <> dtBoolean then
    raise EScError.Create(seCannotConvert);

  SetLength(LexemData.Buffer, 1);
  if Value then
    LexemData.Buffer[0] := 1
  else
    LexemData.Buffer[0] := 0;
end;

class procedure TScASN1Writer.SetAsInteger(var LexemData: TScLexemData; Value: Int64);
var
  i: integer;
begin
  if not (LexemData.DataType in [dtInteger, dtEnumerated]) then
    raise EScError.Create(seCannotConvert);

  if Value <= 0 then begin
    SetLength(LexemData.Buffer, 1);
    LexemData.Buffer[0] := 0;
    Exit;
  end;

  SetLength(LexemData.Buffer, 8);
  i := 7;

  while Value > 0 do begin
    LexemData.Buffer[i] := byte(Value); // and $FF
    Value := Value shr 8;
    Dec(i);
  end;

  if LexemData.Buffer[i + 1] >= $80 then begin // negative sign
    LexemData.Buffer[i] := 0;
    Dec(i);
  end;

  if i >= 0 then begin
    Move(LexemData.Buffer[i + 1], LexemData.Buffer[0], 7 - i);
    SetLength(LexemData.Buffer, 7 - i);
  end;
end;

class procedure TScASN1Writer.SetAsBigInteger(var LexemData: TScLexemData; const Value: TBytes);
begin
  if LexemData.DataType <> dtInteger then
    raise EScError.Create(seCannotConvert);

  if Value[0] >= $80 then begin // negative sign
    SetLength(LexemData.Buffer, Length(Value) + 1);
    LexemData.Buffer[0] := 0;
    Move(Value[0], LexemData.Buffer[1], Length(Value));
  end
  else
    LexemData.Buffer := Value;
end;

class procedure TScASN1Writer.SetAsOID(var LexemData: TScLexemData; const Value: string);
var
  OId: TOId;
  Off: integer;
  Count: integer;
  i: integer;
begin
  if LexemData.DataType <> dtObjectIdentifier then
    raise EScError.Create(seCannotConvert);

  SetLength(OId, 32);
  Count := 0;
  for i := 1 to Length(Value) do begin
    if Value[i] = '.' then
      Inc(Count)
    else begin
      if not CharInSet(Value[i], ['0'..'9']) then
        raise EScError.Create(seCannotConvert);

      OId[Count] := OId[Count] * 10 + (Ord(Value[i]) - Ord('0'));
    end;
  end;
  Inc(Count);

  SetLength(LexemData.Buffer, 128);
  LexemData.Buffer[0] := OId[0] * 40 + OId[1];
  Off := 1;

  for i := 2 to Count - 1 do begin
    if OId[i] > 268435456 then begin
      LexemData.Buffer[Off] := (OId[i] div 268435456) or $80;
      OId[i] := OId[i] mod 268435456;
      Inc(Off);
    end;
    if OId[i] > 2097152 then begin
      LexemData.Buffer[Off] := (OId[i] div 2097152) or $80;
      OId[i] := OId[i] mod 2097152;
      Inc(Off);
    end;
    if OId[i] > 16384 then begin
      LexemData.Buffer[Off] := (OId[i] div 16384) or $80;
      OId[i] := OId[i] mod 16384;
      Inc(Off);
    end;
    if OId[i] > 128 then begin
      LexemData.Buffer[Off] := (OId[i] div 128) or $80;
      OId[i] := OId[i] mod 128;
      Inc(Off);
    end;

    LexemData.Buffer[Off] := OId[i];
    Inc(Off);
  end;

  SetLength(LexemData.Buffer, Off);
end;

class procedure TScASN1Writer.SetAsDateTime(var LexemData: TScLexemData; Value: TDateTime);
var
  UtcTime: TSystemTime;
{$IFDEF MSWINDOWS}
  TZ: TTimeZoneInformation;
{$ENDIF}
  Off: integer;
begin
  if not (LexemData.DataType in [dtUTCTime, dtGeneralizedTime]) then
    raise EScError.Create(seCannotConvert);

{$IFDEF MSWINDOWS}
  GetTimeZoneInformation(TZ);
  if GetTimeZoneInformation(TZ) = TIME_ZONE_ID_STANDARD then
    TZ.DaylightBias := TZ.StandardBias;

  Value := IncMinute(Value, TZ.Bias + TZ.DaylightBias);
  DateTimeToSystemTime(Value, UtcTime);
{$ELSE}
  {$IFDEF POSIX}
  Value := TTimeZone.Local.ToUniversalTime(Value);
  {$ENDIF}
  DecodeDateTime(Value, UtcTime.wYear, UtcTime.wMonth, UtcTime.wDay, UtcTime.wHour, UtcTime.wMinute, UtcTime.wSecond, UtcTime.wMilliseconds);
{$ENDIF}

  if LexemData.DataType = dtUTCTime then
    SetLength(LexemData.Buffer, UTC_TIME_LEN)
  else
    SetLength(LexemData.Buffer, GENERALIZED_TIME_LEN);

  LexemData.Buffer[Length(LexemData.Buffer) - 1] := Ord('Z');

  if LexemData.DataType = dtUTCTime then begin
    // 0 <= year < 50 : assume century 21
    // 50 <= year < 70 : illegal per PKIX
    // 70 < year <= 99 : assume century 20
    if UtcTime.wYear >= 2000 then
      UtcTime.wYear := UtcTime.wYear - 2000
    else
      UtcTime.wYear := UtcTime.wYear - 1900;

    LexemData.Buffer[0] := (UtcTime.wYear div 10) + Ord('0');
    LexemData.Buffer[1] := (UtcTime.wYear mod 10) + Ord('0');
    Off := 2;
  end
  else begin
    LexemData.Buffer[0] := (UtcTime.wYear div 1000) + Ord('0');
    LexemData.Buffer[1] := ((UtcTime.wYear mod 1000) div 100) + Ord('0');
    LexemData.Buffer[2] := ((UtcTime.wYear mod 100) div 10) + Ord('0');
    LexemData.Buffer[3] := (UtcTime.wYear mod 10) + Ord('0');
    Off := 4;
  end;

  LexemData.Buffer[Off] := (UtcTime.wMonth div 10) + Ord('0');
  LexemData.Buffer[Off + 1] := (UtcTime.wMonth mod 10) + Ord('0');
  LexemData.Buffer[Off + 2] := (UtcTime.wDay div 10) + Ord('0');
  LexemData.Buffer[Off + 3] := (UtcTime.wDay mod 10) + Ord('0');
  LexemData.Buffer[Off + 4] := (UtcTime.wHour div 10) + Ord('0');
  LexemData.Buffer[Off + 5] := (UtcTime.wHour mod 10) + Ord('0');
  LexemData.Buffer[Off + 6] := (UtcTime.wMinute div 10) + Ord('0');
  LexemData.Buffer[Off + 7] := (UtcTime.wMinute mod 10) + Ord('0');
  LexemData.Buffer[Off + 8] := (UtcTime.wSecond div 10) + Ord('0');
  LexemData.Buffer[Off + 9] := (UtcTime.wSecond mod 10) + Ord('0');
end;

{ TScASN1Compiler }

destructor TScASN1Compiler.Destroy;
begin
  FLexemInfo.Free;
  FStream.Free;

  inherited;
end;

function TScASN1Compiler.GetRoot: TScLexemInfo;
begin
  if FLexemInfo <> nil then
    Result := FLexemInfo.FElems[0]
  else
    Result := nil;
end;

function TScASN1Compiler.GetValueByName(const Name: string): TScLexemInfo;
begin
  if FLexemInfo <> nil then
    Result := FLexemInfo.GetValueByName(Name)
  else
    Result := nil;
end;

function TScASN1Compiler.ReadByte(LexemInfo: TScLexemInfo; var CurOffset: Int64): byte;
begin
  if CurOffset >= LexemInfo.FLexemData.Size then
    raise EScError.Create(seWrongDataFormat);

  FStream.Position := LexemInfo.FLexemData.StartPos + LexemInfo.FLexemData.AddedSize + CurOffset;
  FStream.Read(Result, 1);
  Inc(CurOffset);
end;
{
class function TScASN1Compiler.ReadWord(LexemInfo: TScLexemInfo): word;
begin
  if (LexemInfo.FCurPos + 1) >= LexemInfo.FLexemData.Size then
    raise EScError.Create(seWrongDataFormat);

  Result := (word(LexemInfo.FLexemData.FData[LexemInfo.FLexemData.Offset + LexemInfo.FCurPos]) shl 8) +
             word(LexemInfo.FLexemData.FData[LexemInfo.FLexemData.Offset + LexemInfo.FCurPos + 1]);
  Inc(LexemInfo.FCurPos, 2);
end;

class function TScASN1Compiler.ReadInt32(LexemInfo: TScLexemInfo): integer;
begin
  if (LexemInfo.FCurPos + 3) >= LexemInfo.FLexemData.Size then
    raise EScError.Create(seWrongDataFormat);

  Result := (integer(LexemInfo.FLexemData.FData[LexemInfo.FLexemData.Offset + LexemInfo.FCurPos]) shl 24) +
            (integer(LexemInfo.FLexemData.FData[LexemInfo.FLexemData.Offset + LexemInfo.FCurPos + 1]) shl 16) +
            (integer(LexemInfo.FLexemData.FData[LexemInfo.FLexemData.Offset + LexemInfo.FCurPos + 2]) shl 8) +
             integer(LexemInfo.FLexemData.FData[LexemInfo.FLexemData.Offset + LexemInfo.FCurPos + 3]);
  Inc(LexemInfo.FCurPos, 4);
end;

class function TScASN1Compiler.ReadInt64(LexemInfo: TScLexemInfo): Int64;
begin
  if (LexemInfo.FCurPos + 7) >= LexemInfo.FLexemData.Size then
    raise EScError.Create(seWrongDataFormat);

  Result := (Int64(LexemInfo.FLexemData.FData[LexemInfo.FLexemData.Offset + LexemInfo.FCurPos]) shl 56) +
            (Int64(LexemInfo.FLexemData.FData[LexemInfo.FLexemData.Offset + LexemInfo.FCurPos + 1]) shl 48) +
            (Int64(LexemInfo.FLexemData.FData[LexemInfo.FLexemData.Offset + LexemInfo.FCurPos + 2]) shl 40) +
            (Int64(LexemInfo.FLexemData.FData[LexemInfo.FLexemData.Offset + LexemInfo.FCurPos + 3]) shl 32) +
            (Int64(LexemInfo.FLexemData.FData[LexemInfo.FLexemData.Offset + LexemInfo.FCurPos + 4]) shl 24) +
            (Int64(LexemInfo.FLexemData.FData[LexemInfo.FLexemData.Offset + LexemInfo.FCurPos + 5]) shl 16) +
            (Int64(LexemInfo.FLexemData.FData[LexemInfo.FLexemData.Offset + LexemInfo.FCurPos + 6]) shl 8) +
             Int64(LexemInfo.FLexemData.FData[LexemInfo.FLexemData.Offset + LexemInfo.FCurPos + 7]);
  Inc(LexemInfo.FCurPos, 8);
end;
}
function TScASN1Compiler.ReadInt64(LexemInfo: TScLexemInfo; Count: integer; var CurOffset: Int64): Int64;
var
  Buf: array [0..7] of byte;
  i: integer;
begin
  if (Count > 8) or ((CurOffset + Count - 1) >= LexemInfo.FLexemData.Size) then
    raise EScError.Create(seWrongDataFormat);

  FStream.Position := LexemInfo.FLexemData.StartPos + LexemInfo.FLexemData.AddedSize + CurOffset;
  FStream.Read(Buf[0], Count);

  Result := 0;
  for i := 0 to Count - 1 do
    Result := (Result shl 8) + Buf[i];

  Inc(CurOffset, Count);
end;

function TScASN1Compiler.ReadLength(LexemInfo: TScLexemInfo; var CurOffset: Int64): Int64;
var
  Len: byte;
begin
  Len := ReadByte(LexemInfo, CurOffset);

  if Len = $80 then
    Result := -1
  else
  if Len < $80 then
    Result := Len
  else
    Result := ReadInt64(LexemInfo, Len - $80, CurOffset);
end;

class function TScASN1Compiler.DataTypeNameToType(const DataTypeName: string): TScASN1DataType;
begin
  for Result := Low(TScASN1DataType) to High(TScASN1DataType) do
    if DataTypeName = ASN1DataTypes[Result].Desc then
      Exit;

  raise EScError.Create(seWrongDataFormat);
end;

class function TScASN1Compiler.IdentNoToClass(Identifier: byte): TScASN1DataType;
begin
  for Result := Low(TScASN1DataType) to High(TScASN1DataType) do
    if Identifier = ASN1DataTypes[Result].No then
      Break;
end;

function TScASN1Compiler.Parse(const Description: TScASN1Description): boolean;
begin
  Result := Parse(Description, TStream(nil));
end;

function TScASN1Compiler.Parse(const Description: TScASN1Description; const Data: TBytes): boolean;
begin
  FreeAndNil(FStream);
  FStream := TMemoryStream.Create;
  FStream.WriteBuffer(Data[0], Length(Data));
  FStream.Position := 0;
  Result := Parse(Description, FStream);
end;

function TScASN1Compiler.Parse(const Description: TScASN1Description;
  const Data: TBytes; Offset, Size: integer): boolean;
begin
  FreeAndNil(FStream);
  FStream := TMemoryStream.Create;
  FStream.WriteBuffer(Data[Offset], Size);
  FStream.Position := 0;
  Result := Parse(Description, FStream);
end;

function TScASN1Compiler.Parse(const Description: TScASN1Description; Stream: TStream): boolean;
var
  LexemInfo: TScRootLexemInfo;
  CurOffset: Int64;
  IsStreamOwner: boolean;
begin
  FreeAndNil(FLexemInfo);
  if FStream <> Stream then
    FreeAndNil(FStream);

  if ASN1ParsedDescriptions[Description] = nil then begin
    ASN1ParsedDescriptionsLock.Enter;
    try
      if ASN1ParsedDescriptions[Description] = nil then begin
        LexemInfo := TScRootLexemInfo.Create;
        try
          ParseDescription(ASN1Descriptions[Description], 1, Length(ASN1Descriptions[Description]), LexemInfo);
        except
          LexemInfo.Free;
          raise;
        end;
        ASN1ParsedDescriptions[Description] := LexemInfo;
      end;
    finally
      ASN1ParsedDescriptionsLock.Leave;
    end;
  end;

  FLexemInfo := TScRootLexemInfo(ASN1ParsedDescriptions[Description].Clone);
  FLexemInfo.FParentLexemInfo := nil;

  if Stream <> nil then begin
    FLexemInfo.FLexemData.StartPos := Stream.Position;
    FLexemInfo.FLexemData.Size := Stream.Size - Stream.Position;
    FLexemInfo.FStream := Stream;

    IsStreamOwner := FStream <> nil;
    FStream := Stream;
    try
      CurOffset := 0;
      Result := ParseData(FLexemInfo, CurOffset);
    finally
      if not IsStreamOwner then
        FStream := nil;
    end;
  end
  else
    Result := True;
end;

class procedure TScASN1Compiler.ParseDescription(const Description: string;
  StartPos, EndPos: integer; ParentLexemInfo: TScComplexLexemInfo);
const
  S_IMPLICIT = 'IMPLICIT';
  S_OPTIONAL = 'OPTIONAL';

  function FindSymbol(const Ch: Char): integer;
  begin
    Result := StartPos + 1;
    while (Result <= EndPos) and (Description[Result] <> Ch) do
      Inc(Result);

    if Result > EndPos then
      raise EScError.Create(seWrongDataFormat);
  end;

  procedure SkipSpaces;
  begin
    while (StartPos <= EndPos) and (Description[StartPos] = ' ') do
      Inc(StartPos);
  end;

var
  LexemInfo: TScLexemInfo;
  ComplLexemInfo: TScComplexLexemInfo;
  DataTypeName, FieldName: string;
  IsImplicit, IsOptional, IsArray: boolean;
  NeedSimplify: boolean;
  DataType: TScASN1DataType;
  Identifier: integer;
  BracketCount: integer;
  i, p: integer;
begin
  /// "FieldName" OPTIONAL [$Identifier] IMPLICIT DataType[] {}

  while StartPos < EndPos do begin
    while CharInSet(Description[StartPos], [',', ' ']) and (StartPos <= EndPos) do
      Inc(StartPos);

    if (ParentLexemInfo.FLexemData.DataType in [dtChoice, dtSequence, dtSet, dtOctetString, dtBit]) and
      (StartPos <= Length(Description)) and (Description[StartPos] = '}') then
      Break;

    // FieldName
    if (StartPos <= EndPos) and (Description[StartPos] = '"') then begin
      p := FindSymbol('"');
      FieldName := Copy(Description, StartPos + 1, p - StartPos - 1);

      StartPos := p + 1;
      SkipSpaces;
    end
    else
      FieldName := '';

    // IsOptional
    if Copy(Description, StartPos, Length(S_OPTIONAL)) = S_OPTIONAL then begin
      IsOptional := True;
      Inc(StartPos, Length(S_OPTIONAL));
      SkipSpaces;
    end
    else
      IsOptional := False;

    // Identifier
    if (StartPos <= EndPos) and (Description[StartPos] = '[') then begin
      p := FindSymbol(']');
      Identifier := StrToInt(Copy(Description, StartPos + 1, p - StartPos - 1));

      StartPos := p + 1;
      SkipSpaces;
    end
    else
      Identifier := -1;

    // Implicit
    if Copy(Description, StartPos, Length(S_IMPLICIT)) = S_IMPLICIT then begin
      IsImplicit := True;
      Inc(StartPos, Length(S_IMPLICIT));
      SkipSpaces;
    end
    else
      IsImplicit := False;

    // DataType
    p := StartPos;
    while (p <= EndPos) and CharInSet(Description[p], ['A'..'Z', '0'..'9']) do
      Inc(p);
    DataTypeName := Copy(Description, StartPos, p - StartPos);
    StartPos := p;
    SkipSpaces;

    // IsArray
    if (StartPos < EndPos) and (Description[StartPos] = '[') then begin
      if Description[StartPos + 1] <> ']' then
        raise EScError.Create(seWrongDataFormat);

      IsArray := True;
      Inc(StartPos, 2);
      SkipSpaces;
    end
    else
      IsArray := False;

    DataType := DataTypeNameToType(DataTypeName);
    if IsImplicit and ((DataType in [dtAny, dtImplicitArray, dtChoice]) or (Identifier = -1)) then
      raise EScError.Create(seWrongDataFormat);

    if Identifier = -1 then begin
      if DataType <> dtAny then
        Identifier := ASN1DataTypes[DataType].No;
    end
    else
      if not IsImplicit or (DataType in [dtImplicitArray, dtSequence, dtSet]) then
        Identifier := Identifier or $A0
      else
        Identifier := Identifier or $80;

    LexemInfo := ParentLexemInfo.AddElem(DataType, IsArray);
    LexemInfo.FIdentifier := byte(Identifier);
    LexemInfo.FName := FieldName;

    if IsImplicit then
      Include(LexemInfo.FStatus, lsIsImplicit);
    if IsOptional then
      Include(LexemInfo.FStatus, lsIsOptional);

    case DataType of
      dtChoice, dtSequence, dtSet, dtOctetString, dtBit, dtImplicitArray: begin
        if (StartPos <= EndPos) and (Description[StartPos] = '{') then begin
          Inc(StartPos);
          BracketCount := 0;
          p := StartPos + 1;
          while p <= EndPos do begin
            if Description[p] = '{' then
              Inc(BracketCount)
            else
            if Description[p] = '}' then
              if BracketCount = 0 then
                Break
              else
                Dec(BracketCount);

            Inc(p);
          end;

          if p > EndPos then
            raise EScError.Create(seWrongDataFormat);

          ComplLexemInfo := LexemInfo as TScComplexLexemInfo;
          ParseDescription(Description, StartPos, p - 1, ComplLexemInfo);
          StartPos := p + 1;

          if DataType = dtChoice then begin
            NeedSimplify := True;
            for i := 0 to Length(ComplLexemInfo.FElems) - 1 do
              if not (ComplLexemInfo.FElems[i].FLexemData.DataType in SimpleTypes) or
                (lsIsImplicit in ComplLexemInfo.FElems[i].FStatus)
              then begin
                NeedSimplify := False;
                Break;
              end;

            if NeedSimplify then begin
              for i := 0 to Length(ComplLexemInfo.FElems) - 1 do
                Include(ComplLexemInfo.FChoiceDataTypes, ComplLexemInfo.FElems[i].FLexemData.DataType);

              LexemInfo := TScLexemInfo.Create;
              ComplLexemInfo.AssignTo(LexemInfo);
              ComplLexemInfo.Free;
              LexemInfo.FLexemData.DataType := dtSimpleChoice;
              ParentLexemInfo.FElems[Length(ParentLexemInfo.FElems) - 1] := LexemInfo;
              LexemInfo.FParentLexemInfo := PScLexemInfo(ParentLexemInfo);
            end;
          end;
        end
        else
        if DataType in [dtChoice, dtSequence, dtSet] then
          raise EScError.Create(seWrongDataFormat);
      end;

      dtObjectIdentifier: begin
        if (StartPos <= EndPos) and (Description[StartPos] = '(') then begin
          p := FindSymbol(')');
          (LexemInfo as TScOidLexemInfo).FTemplateOId := Copy(Description, StartPos + 1, p - StartPos - 1);
          Exclude(LexemInfo.FStatus, lsIsNull);
          LexemInfo.SetParentNotNulls;
          StartPos := p + 1;
        end;
      end;

      dtNull:
        Exclude(LexemInfo.FStatus, lsIsNull);

      dtAny: ;
      dtBoolean: ;
      dtInteger, dtEnumerated: ;
      dtUTF8String, dtNumericString, dtPrintableString, dtTeletexString, dtVideotexString,
      dtIA5String, dtGraphicString, dtVisibleString, dtGeneralString, dtUniversalString, dtBMPString: ;
      dtUTCTime, dtGeneralizedTime: ;
    else
      Assert(False);
    end;
  end;
end;

function TScASN1Compiler.ParseData(ParentLexemInfo: TScComplexLexemInfo; var ParentCurOffset: Int64): boolean;
var
  LexemInfo: TScLexemInfo;
  Len: Int64;
  Identifier: byte;

  function SelectChoice: boolean;
  var
    ComplLexemInfo: TScComplexLexemInfo;
    DataType: TScASN1DataType;
    j: integer;
  begin
    Result := False;
    if (LexemInfo.FIdentifier and $C0) > 0 then begin
      Assert(LexemInfo.FIdentifier = Identifier);
      Identifier := ReadByte(ParentLexemInfo, ParentCurOffset);
      Len := ReadLength(ParentLexemInfo, ParentCurOffset);
      if (ParentLexemInfo.FLexemData.Size - ParentCurOffset) < Len then
        Exit;
    end;

    if LexemInfo.FChoiceDataTypes <> [] then begin
      DataType := IdentNoToClass(Identifier);
      if DataType in LexemInfo.FChoiceDataTypes then begin
        LexemInfo.FLexemData.DataType := DataType;
        Result := True;
      end;
    end
    else
    if LexemInfo is TScComplexLexemInfo then begin
      ComplLexemInfo := TScComplexLexemInfo(LexemInfo);
      for j := 0 to Length(ComplLexemInfo.FElems) - 1 do begin
        if ComplLexemInfo.FElems[j].FIdentifier = Identifier then begin
          ComplLexemInfo.FChoiceNo := j;
          LexemInfo := ComplLexemInfo.FElems[j];
          Result := True;
          Exit;
        end;
      end;
    end;
  end;

var
  DataType: TScASN1DataType;
  CurOffset, PrevOffset: Int64;
  ArrayLen: Int64;
  LexemInfoi: TScComplexLexemInfo;
  i: integer;
begin
  Result := False;

  i := 0;
  while i < Length(ParentLexemInfo.FElems) do begin
    LexemInfo := ParentLexemInfo.FElems[i];

    if (ParentLexemInfo.FLexemData.Size - ParentCurOffset) < 2 then begin
      if not (lsIsOptional in LexemInfo.FStatus) then
        Exit;

      Inc(i);
      Continue;
    end;

    if LexemInfo.FLexemData.DataType = dtImplicitArray then begin
      PrevOffset := 0;
      Len := ParentLexemInfo.FLexemData.Size;
      DataType := dtImplicitArray;
    end
    else begin
    {$IFDEF VER10}
      {$WARNINGS OFF}
    {$ENDIF}
    {$IFDEF VER11}
      {$WARNINGS OFF}
    {$ENDIF}
      PrevOffset := ParentCurOffset;
      Identifier := ReadByte(ParentLexemInfo, ParentCurOffset);
      Len := ReadLength(ParentLexemInfo, ParentCurOffset);

      if ParentLexemInfo.FLexemData.IsIndefiniteSize and (Identifier = 0) and (Len = 0) then begin
        while i < Length(ParentLexemInfo.FElems) do begin
          if not (lsIsOptional in ParentLexemInfo.FElems[i].FStatus) then
            Exit;
          Inc(i);
        end;

        Dec(ParentCurOffset, 2{Identifier+Length});
        Result := True;
        Exit;
      end;

      if (ParentLexemInfo.FLexemData.Size - ParentCurOffset) < Len then
        Exit;

      while ((LexemInfo.FIdentifier and $C0) > 0) and
            (LexemInfo.FLexemData.DataType <> dtAny) and
            (LexemInfo.FIdentifier <> Identifier) do
      begin
        if not (lsIsOptional in LexemInfo.FStatus) then
          Exit;

        Inc(i);
        if i < Length(ParentLexemInfo.FElems) then
          LexemInfo := ParentLexemInfo.FElems[i]
        else begin
          Result := True;
          Exit;
        end;
      end;

      if LexemInfo.FLexemData.DataType in [dtChoice, dtSimpleChoice] then begin
        if not SelectChoice then
          Exit;
      end
      else
        while not (LexemInfo.FLexemData.DataType in [dtAny, dtChoice, dtSimpleChoice]) and (LexemInfo.FIdentifier <> Identifier) do begin
          if not (lsIsOptional in LexemInfo.FStatus) then
            Exit;

          Inc(i);
          if i < Length(ParentLexemInfo.FElems) then
            LexemInfo := ParentLexemInfo.FElems[i]
          else begin
            Result := True;
            Exit;
          end;
        end;

      if LexemInfo.FLexemData.DataType in [dtChoice, dtSimpleChoice] then
        if not SelectChoice then
          Exit;

      if lsIsImplicit in LexemInfo.FStatus then
        DataType := LexemInfo.FLexemData.DataType
      else
      if LexemInfo.FLexemData.DataType = dtAny then begin
        if (Identifier and $C0) = 0 then begin
          DataType := IdentNoToClass(Identifier);
          if DataType > High(TScASN1DataType) then
            DataType := dtPrintableString;
        end
        else
          DataType := dtPrintableString;

        LexemInfo.FLexemData.DataType := DataType;
      end
      else begin
        if (Identifier and $C0) > 0 then begin // 8th or 7th bits are set
          // not Implicit
          Identifier := ReadByte(ParentLexemInfo, ParentCurOffset);
          Len := ReadLength(ParentLexemInfo, ParentCurOffset);
          if (ParentLexemInfo.FLexemData.Size - ParentCurOffset) < Len then
            Exit;
        end;

        DataType := IdentNoToClass(Identifier);
        if LexemInfo.FLexemData.DataType <> DataType then
          Exit;
      end;
    end;

    LexemInfo.FLexemData.StartPos := ParentLexemInfo.FLexemData.StartPos + ParentLexemInfo.FLexemData.AddedSize + PrevOffset;
    LexemInfo.FLexemData.AddedSize := ParentCurOffset - PrevOffset;

    if Len < 0 then begin
      LexemInfo.FLexemData.Size := ParentLexemInfo.FLexemData.Size - ParentCurOffset;
      LexemInfo.FLexemData.IsIndefiniteSize := True;
    end
    else begin
      LexemInfo.FLexemData.Size := Len;
      LexemInfo.FLexemData.IsIndefiniteSize := False;
      Inc(ParentCurOffset, Len);
    end;

    Exclude(LexemInfo.FStatus, lsIsNull);
    CurOffset := 0;

    case DataType of
      dtSequence, dtSet, dtOctetString, dtImplicitArray: begin
        if (LexemInfo is TScComplexLexemInfo) and (Length(TScComplexLexemInfo(LexemInfo).FElems) > 0) then begin
          if LexemInfo.ClassType <> TScArrayLexemInfo then begin
            if not ParseData(TScComplexLexemInfo(LexemInfo), CurOffset) then
              Exit;

            if LexemInfo.FLexemData.IsIndefiniteSize then begin
              Identifier := ReadByte(LexemInfo, CurOffset);
              Len := ReadLength(LexemInfo, CurOffset);
              if (Identifier <> 0) or (Len <> 0) then
                Exit;

              LexemInfo.FLexemData.Size := CurOffset;
              LexemInfo.FLexemData.IsIndefiniteSize := False;
              Inc(ParentCurOffset, LexemInfo.FLexemData.Size);
            end;
          end
          else begin
            ArrayLen := 0;
            if Len > 0 then
              TScArrayLexemInfo(LexemInfo).FArray := TCRObjectList.Create;

            while ArrayLen < Len do begin
              LexemInfoi := TScComplexLexemInfo(LexemInfo.Clone(TScComplexLexemInfo {for memory optimization}));
              TScArrayLexemInfo(LexemInfo).FArray.Add(LexemInfoi);

              LexemInfoi.FLexemData.StartPos := LexemInfo.FLexemData.StartPos + LexemInfo.FLexemData.AddedSize + ArrayLen;
              LexemInfoi.FLexemData.AddedSize := 0;
              CurOffset := 0;
              if not ParseData(LexemInfoi, CurOffset) then
                Exit;

              LexemInfoi.FLexemData.Size := CurOffset;
              Inc(ArrayLen, CurOffset);
            end;
          end;
        end;
      end;

      dtBit: begin
        if (LexemInfo is TScComplexLexemInfo) and (Length(TScComplexLexemInfo(LexemInfo).FElems) > 0) then begin
          ReadByte(LexemInfo, CurOffset); // unused bit count
          if not ParseData(TScComplexLexemInfo(LexemInfo), CurOffset) then
            Exit;
        end
        else
        if Len <= 0 then
          Exit;
      end;

      dtObjectIdentifier: begin
        if Len <= 0 then
          Exit;

        if (LexemInfo is TScOidLexemInfo) and (TScOidLexemInfo(LexemInfo).FTemplateOId <> '') and
           (LexemInfo.GetAsOID <> TScOidLexemInfo(LexemInfo).FTemplateOId) then
          Exit;
      end;

      dtInteger, dtEnumerated,
      dtUTCTime, dtGeneralizedTime: begin
        if Len <= 0 then
          Exit;
      end;

      dtBoolean: begin
        if Len <> 1 then
          Exit;
      end;

      dtNull: begin
        if Len <> 0 then
          Exit;
      end;

      dtUTF8String, dtNumericString, dtPrintableString, dtTeletexString, dtVideotexString,
      dtIA5String, dtGraphicString, dtVisibleString, dtGeneralString, dtUniversalString, dtBMPString: begin
        if Len < 0 then
          Exit;
      end;
    else
      Assert(False);
    end;

    Inc(i);
  end;

  Result := True;
end;

class function TScASN1Compiler.CalcInfoSize(Size: Int64): integer;
begin
  if Size < $80 then
    Result := 2   // Type + Len
  else
  if Size <= $FF then
    Result := 3   // Type + SizeOfLen + Len(1)
  else
  if Size <= $FFFF then
    Result := 4   // Type + SizeOfLen + Len(2)
  else
  if Size <= $FFFFFF then
    Result := 5   // Type + SizeOfLen + Len(3)
  else
  if Size <= $FFFFFFFF then
    Result := 6   // Type + SizeOfLen + Len(4)
  else
  if Size <= $FFFFFFFFFF then
    Result := 7   // Type + SizeOfLen + Len(5)
  else
  if Size <= $FFFFFFFFFFFF then
    Result := 8   // Type + SizeOfLen + Len(6)
  else
  if Size <= $FFFFFFFFFFFFFF then
    Result := 9   // Type + SizeOfLen + Len(7)
  else
    Result := 10; // Type + SizeOfLen + Len(8)
end;

class function TScASN1Compiler.CalcBlockSize(LexemInfo: TScLexemInfo): Int64;
var
  AddedSize: integer;
begin
  if LexemInfo.IsNull and (LexemInfo.FLexemData.DataType <> dtNull) then
    Result := 0
  else begin
    if (lsIsEncodedData in LexemInfo.FStatus) or (LexemInfo.FLexemData.DataType = dtImplicitArray) then
      AddedSize := 0
    else
      AddedSize := CalcInfoSize(LexemInfo.FLexemData.Size);

    Result := LexemInfo.FLexemData.Size + AddedSize;

    if (LexemInfo.FIdentifier <> $FF) and ((LexemInfo.FIdentifier and $80) = $80) and
      not (lsIsImplicit in LexemInfo.FStatus)
    then begin
      LexemInfo.FLexemData.AddedSize := AddedSize;
      Result := Result + CalcInfoSize(Result);
    end;
  end;
end;

class procedure TScASN1Compiler.SetSizes(LexemInfo: TScLexemInfo; out PreDefinedSize: Int64);
var
  ComplLexemInfo: TScComplexLexemInfo;
  LexemInfoi: TScLexemInfo;
  CurPreDefinedSize: Int64;
  i: integer;
begin
  PreDefinedSize := 0;

  case LexemInfo.FLexemData.DataType of
    dtChoice: begin
      if lsIsNull in LexemInfo.FStatus then
        LexemInfo.FLexemData.Size := 0
      else
      if lsIsEncodedData in LexemInfo.FStatus then begin
        if lsIsPreDefined in LexemInfo.FStatus then
          PreDefinedSize := LexemInfo.FLexemData.Size
        else
          LexemInfo.FLexemData.Size := Length(LexemInfo.FLexemData.Buffer);
      end
      else begin
        ComplLexemInfo := LexemInfo as TScComplexLexemInfo;

        if ComplLexemInfo.FChoiceNo = -1 then
          raise EScError.Create(seChoiceLexemNotDefined);

        SetSizes(ComplLexemInfo.FElems[ComplLexemInfo.FChoiceNo], PreDefinedSize);
        ComplLexemInfo.FLexemData.Size := ComplLexemInfo.FElems[ComplLexemInfo.FChoiceNo].FLexemData.Size;

        if lsIsEncodedData in ComplLexemInfo.FElems[ComplLexemInfo.FChoiceNo].FStatus then
          Include(ComplLexemInfo.FStatus, lsIsEncodedData);

        if lsIsNull in ComplLexemInfo.FElems[ComplLexemInfo.FChoiceNo].FStatus then
          Include(ComplLexemInfo.FStatus, lsIsNull);
      end;
    end;

    dtSequence, dtSet, dtImplicitArray,
    dtOctetString, dtBit,
    dtSimpleChoice,
    dtAny: begin
      if lsIsNull in LexemInfo.FStatus then
        LexemInfo.FLexemData.Size := 0
      else
      if lsIsPreDefined in LexemInfo.FStatus then
        PreDefinedSize := LexemInfo.FLexemData.Size
      else
      if Length(LexemInfo.FLexemData.Buffer) > 0 then
        LexemInfo.FLexemData.Size := Length(LexemInfo.FLexemData.Buffer)
      else
      if (LexemInfo.ClassType = TScArrayLexemInfo) and (TScArrayLexemInfo(LexemInfo).FArray <> nil) then begin
        LexemInfo.FLexemData.Size := 0;
        for i := 0 to TScArrayLexemInfo(LexemInfo).FArray.Count - 1 do begin
          LexemInfoi := TScLexemInfo(TScArrayLexemInfo(LexemInfo).FArray[i]);
          CurPreDefinedSize := 0;
          SetSizes(LexemInfoi, CurPreDefinedSize);
          LexemInfo.FLexemData.Size := LexemInfo.FLexemData.Size + LexemInfoi.FLexemData.Size;
          Inc(PreDefinedSize, CurPreDefinedSize);
        end;
      end
      else begin
        LexemInfo.FLexemData.Size := 0;
        if LexemInfo is TScComplexLexemInfo then begin
          ComplLexemInfo := TScComplexLexemInfo(LexemInfo);
          for i := 0 to Length(ComplLexemInfo.FElems) - 1 do begin
            CurPreDefinedSize := 0;
            SetSizes(ComplLexemInfo.FElems[i], CurPreDefinedSize);
            ComplLexemInfo.FLexemData.Size := ComplLexemInfo.FLexemData.Size + CalcBlockSize(ComplLexemInfo.FElems[i]);
            Inc(PreDefinedSize, CurPreDefinedSize);
          end;
        end;
      end;
    end;

    dtObjectIdentifier: begin
      if (Length(LexemInfo.FLexemData.Buffer) = 0) and (LexemInfo is TScOidLexemInfo) and
         (TScOidLexemInfo(LexemInfo).FTemplateOId <> '') then
        LexemInfo.SetAsOID(TScOidLexemInfo(LexemInfo).FTemplateOId);

      LexemInfo.FLexemData.Size := Length(LexemInfo.FLexemData.Buffer);
    end;

    dtUTF8String, dtNumericString, dtPrintableString, dtTeletexString, dtVideotexString,
    dtIA5String, dtGraphicString, dtVisibleString, dtGeneralString, dtUniversalString, dtBMPString: begin
      if lsIsPreDefined in LexemInfo.FStatus then
        PreDefinedSize := LexemInfo.FLexemData.Size
      else
        LexemInfo.FLexemData.Size := Length(LexemInfo.FLexemData.Buffer);
    end;

    dtUTCTime, dtGeneralizedTime:
      LexemInfo.FLexemData.Size := Length(LexemInfo.FLexemData.Buffer);

    dtInteger, dtEnumerated: begin
      if (Length(LexemInfo.FLexemData.Buffer) = 0) and not LexemInfo.IsNull then
        SetLength(LexemInfo.FLexemData.Buffer, 1);

      LexemInfo.FLexemData.Size := Length(LexemInfo.FLexemData.Buffer);
    end;

    dtNull:
      LexemInfo.FLexemData.Size := 0;
  else
    Assert(False);
  end;
end;

class procedure TScASN1Compiler.SetOffsets(LexemInfo: TScLexemInfo; out PreDefinedSize: Int64);
var
  ComplLexemInfo: TScComplexLexemInfo;
  LexemInfoi, LexemInfoPrev: TScLexemInfo;
  CurPreDefinedSize: Int64;
  i: integer;
begin
  if lsIsPreDefined in LexemInfo.FStatus then
    PreDefinedSize := LexemInfo.FLexemData.Size
  else
    PreDefinedSize := 0;

  if (LexemInfo.ClassType = TScArrayLexemInfo) and (TScArrayLexemInfo(LexemInfo).FArray <> nil) then begin
    if Length(LexemInfo.FLexemData.Buffer) > 0 then
      // none
    else begin
      LexemInfoPrev := nil;
      for i := 0 to TScArrayLexemInfo(LexemInfo).FArray.Count - 1 do begin
        LexemInfoi := TScLexemInfo(TScArrayLexemInfo(LexemInfo).FArray[i]);
        if i = 0 then begin
          if LexemInfo.FLexemData.DataType = dtImplicitArray then
            LexemInfoi.FLexemData.StartPos := LexemInfo.FLexemData.StartPos
          else
            LexemInfoi.FLexemData.StartPos := LexemInfo.FLexemData.StartPos +
              LexemInfo.FLexemData.AddedSize + CalcInfoSize(LexemInfo.FLexemData.Size);
        end
        else
          LexemInfoi.FLexemData.StartPos := LexemInfoPrev.FLexemData.StartPos + LexemInfoPrev.FLexemData.Size;

        CurPreDefinedSize := 0;
        SetOffsets(LexemInfoi, CurPreDefinedSize);
        Inc(PreDefinedSize, CurPreDefinedSize);
        LexemInfoPrev := LexemInfoi;
      end;
    end;
  end
  else
  if LexemInfo.FLexemData.DataType = dtChoice then begin
    ComplLexemInfo := LexemInfo as TScComplexLexemInfo;
    if not (lsIsEncodedData in LexemInfo.FStatus) then begin
      ComplLexemInfo.FElems[ComplLexemInfo.FChoiceNo].FLexemData.StartPos := LexemInfo.FLexemData.StartPos + LexemInfo.FLexemData.AddedSize;
      SetOffsets(ComplLexemInfo.FElems[ComplLexemInfo.FChoiceNo], PreDefinedSize);
    end;
  end
  else
  if LexemInfo is TScComplexLexemInfo then begin
    ComplLexemInfo := TScComplexLexemInfo(LexemInfo);
    CurPreDefinedSize := 0;
    for i := 0 to Length(ComplLexemInfo.FElems) - 1 do begin
      if i = 0 then begin
        if (ComplLexemInfo.ClassType = TScArrayLexemInfo) or (ComplLexemInfo.FLexemData.DataType in [dtImplicitArray, dtAny]) then
          ComplLexemInfo.FElems[i].FLexemData.StartPos := ComplLexemInfo.FLexemData.StartPos
        else
          ComplLexemInfo.FElems[i].FLexemData.StartPos := ComplLexemInfo.FLexemData.StartPos +
            ComplLexemInfo.FLexemData.AddedSize + CalcInfoSize(ComplLexemInfo.FLexemData.Size)
      end
      else
        ComplLexemInfo.FElems[i].FLexemData.StartPos := ComplLexemInfo.FElems[i - 1].FLexemData.StartPos +
          CalcBlockSize(ComplLexemInfo.FElems[i - 1]) - CurPreDefinedSize;

      if not ComplLexemInfo.FElems[i].IsNull and
        (ComplLexemInfo.FElems[i] is TScComplexLexemInfo) and
        (Length(TScComplexLexemInfo(ComplLexemInfo.FElems[i]).FElems) > 0)
      then begin
        CurPreDefinedSize := 0;
        SetOffsets(ComplLexemInfo.FElems[i], CurPreDefinedSize);
      end
      else
      if lsIsPreDefined in ComplLexemInfo.FElems[i].FStatus then
        CurPreDefinedSize := ComplLexemInfo.FElems[i].FLexemData.Size;

      Inc(PreDefinedSize, CurPreDefinedSize);
    end;
  end;
end;

class procedure TScASN1Compiler.InternalBuild(LexemInfo: TScComplexLexemInfo; var Data: TBytes);

  procedure WriteSize(DataSize: Int64; var Offset: Int64);
  begin
    if DataSize < $80 then begin
      Data[Offset] := Byte(DataSize);
      Inc(Offset, 1);
    end
    else
    if DataSize <= $FF then begin
      Data[Offset] := $81; // SizeOfLen
      Data[Offset + 1] := Byte(DataSize);
      Inc(Offset, 2);
    end
    else
    if DataSize <= $FFFF then begin
      Data[Offset] := $82; // SizeOfLen
      Data[Offset + 1] := Byte(DataSize shr 8);
      Data[Offset + 2] := Byte(DataSize);
      Inc(Offset, 3);
    end
    else
    if DataSize <= $FFFFFF then begin
      Data[Offset] := $83; // SizeOfLen
      Data[Offset + 1] := Byte(DataSize shr 16);
      Data[Offset + 2] := Byte(DataSize shr 8);
      Data[Offset + 3] := Byte(DataSize);
      Inc(Offset, 4);
    end
    else
    if DataSize <= $FFFFFFFF then begin
      Data[Offset] := $84; // SizeOfLen
      Data[Offset + 1] := Byte(DataSize shr 24);
      Data[Offset + 2] := Byte(DataSize shr 16);
      Data[Offset + 3] := Byte(DataSize shr 8);
      Data[Offset + 4] := Byte(DataSize);
      Inc(Offset, 5);
    end
    else
    if DataSize <= $FFFFFFFFFF then begin
      Data[Offset] := $85; // SizeOfLen
      Data[Offset + 1] := Byte(DataSize shr 32);
      Data[Offset + 2] := Byte(DataSize shr 24);
      Data[Offset + 3] := Byte(DataSize shr 16);
      Data[Offset + 4] := Byte(DataSize shr 8);
      Data[Offset + 5] := Byte(DataSize);
      Inc(Offset, 6);
    end
    else
    if DataSize <= $FFFFFFFFFFFF then begin
      Data[Offset] := $86; // SizeOfLen
      Data[Offset + 1] := Byte(DataSize shr 40);
      Data[Offset + 2] := Byte(DataSize shr 32);
      Data[Offset + 3] := Byte(DataSize shr 24);
      Data[Offset + 4] := Byte(DataSize shr 16);
      Data[Offset + 5] := Byte(DataSize shr 8);
      Data[Offset + 6] := Byte(DataSize);
      Inc(Offset, 7);
    end
    else
    if DataSize <= $FFFFFFFFFFFFFF then begin
      Data[Offset] := $87; // SizeOfLen
      Data[Offset + 1] := Byte(DataSize shr 48);
      Data[Offset + 2] := Byte(DataSize shr 40);
      Data[Offset + 3] := Byte(DataSize shr 32);
      Data[Offset + 4] := Byte(DataSize shr 24);
      Data[Offset + 5] := Byte(DataSize shr 16);
      Data[Offset + 6] := Byte(DataSize shr 8);
      Data[Offset + 7] := Byte(DataSize);
      Inc(Offset, 8);
    end
    else begin
      Data[Offset] := $88; // SizeOfLen
      Data[Offset + 1] := Byte(DataSize shr 56);
      Data[Offset + 2] := Byte(DataSize shr 48);
      Data[Offset + 3] := Byte(DataSize shr 40);
      Data[Offset + 4] := Byte(DataSize shr 32);
      Data[Offset + 5] := Byte(DataSize shr 24);
      Data[Offset + 6] := Byte(DataSize shr 16);
      Data[Offset + 7] := Byte(DataSize shr 8);
      Data[Offset + 8] := Byte(DataSize);
      Inc(Offset, 9);
    end;
  end;

var
  LexemInfoi: TScLexemInfo;
  LexemDatai: PScLexemData;
  i: integer;
begin
  if (LexemInfo.ClassType = TScArrayLexemInfo) and (TScArrayLexemInfo(LexemInfo).FArray <> nil) then begin
    if Length(LexemInfo.FLexemData.Buffer) > 0 then
      Move(LexemInfo.FLexemData.Buffer[0], Data[LexemInfo.FLexemData.StartPos], Length(LexemInfo.FLexemData.Buffer))
    else
      for i := 0 to TScArrayLexemInfo(LexemInfo).FArray.Count - 1 do begin
        LexemInfoi := TScLexemInfo(TScArrayLexemInfo(LexemInfo).FArray[i]);
        if Length(LexemInfoi.FLexemData.Buffer) > 0 then
          Move(LexemInfoi.FLexemData.Buffer[0], Data[LexemInfoi.FLexemData.StartPos], Length(LexemInfoi.FLexemData.Buffer))
        else
        if LexemInfoi is TScComplexLexemInfo then
          InternalBuild(TScComplexLexemInfo(LexemInfoi), Data);
      end;
  end
  else
  for i := 0 to Length(LexemInfo.FElems) - 1 do begin
    LexemInfoi := LexemInfo.FElems[i];
    LexemDatai := @LexemInfoi.FLexemData;
    if LexemInfoi.IsNull and (LexemDatai.DataType <> dtNull) then
      Continue;

    if LexemInfoi.FLexemData.DataType = dtChoice then begin
      if (LexemInfoi.FIdentifier and $80) = $80 then begin
        Data[LexemDatai.StartPos] := LexemInfoi.FIdentifier;

        if not (lsIsImplicit in LexemInfoi.FStatus) then begin
          Inc(LexemDatai.StartPos);
          WriteSize(LexemDatai.Size + LexemDatai.AddedSize, LexemDatai.StartPos);
        end;
      end;

      if not (lsIsEncodedData in LexemInfoi.FStatus) then begin
        LexemInfoi := (LexemInfoi as TScComplexLexemInfo).FElems[TScComplexLexemInfo(LexemInfoi).FChoiceNo];
        LexemDatai := @LexemInfoi.FLexemData;
      end;
    end;

    if not (lsIsEncodedData in LexemInfoi.FStatus) then begin
      /// Write type and size
      if (LexemInfoi.FIdentifier = $FF) or ((LexemInfoi.FIdentifier and $80) = 0) then
        Data[LexemDatai.StartPos] := ASN1DataTypes[LexemDatai.DataType].No // Type
      else begin
        Data[LexemDatai.StartPos] := LexemInfoi.FIdentifier;

        if not (lsIsImplicit in LexemInfoi.FStatus) then begin
          Inc(LexemDatai.StartPos);
          WriteSize(LexemDatai.Size + LexemDatai.AddedSize, LexemDatai.StartPos);

          Data[LexemDatai.StartPos] := ASN1DataTypes[LexemDatai.DataType].No;
        end;
      end;

      Inc(LexemDatai.StartPos);
      WriteSize(LexemDatai.Size, LexemDatai.StartPos);
    end;

    if not (lsIsPreDefined in LexemInfoi.FStatus) and (Length(LexemDatai.Buffer) = 0) and
      (LexemInfoi is TScComplexLexemInfo) and (Length(TScComplexLexemInfo(LexemInfoi).FElems) > 0)
    then
      InternalBuild(TScComplexLexemInfo(LexemInfoi), Data)
    else
      if lsIsPreDefined in LexemInfoi.FStatus then
        // None
      else
      if Length(LexemDatai.Buffer) > 0 then
        Move(LexemDatai.Buffer[0], Data[LexemDatai.StartPos], Length(LexemDatai.Buffer));
  end;
end;

function TScASN1Compiler.Build: TBytes;
var
  PreDefinedSize: Int64;
begin
  if (FLexemInfo = nil) or (Length(FLexemInfo.FElems) = 0) then
    raise EScError.Create(seInternalError);

  PreDefinedSize := 0;
  SetSizes(FLexemInfo, PreDefinedSize);

  SetLength(Result, FLexemInfo.FLexemData.Size - PreDefinedSize);

  FLexemInfo.FLexemData.StartPos := 0;
  PreDefinedSize := 0;
  SetOffsets(FLexemInfo, PreDefinedSize);

  InternalBuild(FLexemInfo, Result);
end;

procedure FreeLexemInfos;
var
  ad: TScASN1Description;
begin
  for ad := Low(TScASN1Description) to High(TScASN1Description) do
    ASN1ParsedDescriptions[ad].Free;
end;

initialization
  ASN1ParsedDescriptionsLock := TCriticalSection.Create;

finalization
  FreeLexemInfos;
  ASN1ParsedDescriptionsLock.Free;

end.
