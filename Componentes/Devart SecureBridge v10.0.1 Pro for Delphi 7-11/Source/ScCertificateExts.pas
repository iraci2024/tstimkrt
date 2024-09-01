
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//////////////////////////////////////////////////

{$IFNDEF DAC}
{$I SB.inc}
unit ScCertificateExts;
{$ENDIF}

interface

uses
  Classes, SysUtils,
{$IFNDEF SBRIDGE}
  CLRClasses, CRTypes, CRFunctions,
  CRHashAlgorithm,
{$IFNDEF UNIDACPRO}
  TdsSSLConsts, TdsUtils,
  TdsASN1, TdsOids, TdsCertificateConsts;
{$ELSE}
  TdsSSLConstsUni, TdsUtilsUni,
  TdsASN1Uni, TdsOidsUni, TdsCertificateConstsUni;
{$ENDIF}
{$ELSE}
  ScCLRClasses, ScTypes, ScFunctions,
  ScHashAlgorithm,
  ScConsts, ScUtils,
  ScASN1, ScOids, ScCertificateConsts;
{$ENDIF}

type
  TScPaddingMode = (pmPKCS1, pmPKCS2, pmOAEP, pmPSS, pmNone);

  TScPSSParams = class
  private
    FHashAlgorithm: TScHashAlgorithm;
    FMaskGenAlgorithm: string;
    FMaskGenHashAlgorithm: TScHashAlgorithm;
    FSaltLength: integer;
    FTrailerField: integer;

  public
    constructor Create;
    procedure Assign(Source: TScPSSParams);

    procedure Init;
    function Encode: TBytes;
    procedure Decode(const RawData: TBytes);

    property HashAlgorithm: TScHashAlgorithm read FHashAlgorithm write FHashAlgorithm;
    property MaskGenHashAlgorithm: TScHashAlgorithm read FMaskGenHashAlgorithm write FMaskGenHashAlgorithm;
    property SaltLength: integer read FSaltLength write FSaltLength;
  end;

  TScOAEPParams = class
  private
    FHashAlgorithm: TScHashAlgorithm;
    FMaskGenAlgorithm: string;
    FMaskGenHashAlgorithm: TScHashAlgorithm;
    FEmptyStrHash: TBytes;

    procedure SetHashAlgorithm(Value: TScHashAlgorithm);

  public
    constructor Create;
    procedure Assign(Source: TScOAEPParams);

    function Encode: TBytes;
    procedure Decode(const RawData: TBytes);
    function GetEmptyStrHash: TBytes;

    property HashAlgorithm: TScHashAlgorithm read FHashAlgorithm write SetHashAlgorithm;
    property MaskGenHashAlgorithm: TScHashAlgorithm read FMaskGenHashAlgorithm write FMaskGenHashAlgorithm;
  end;

  TScOId = class(TScPersistent)
  private
    FValue: string;
    FFriendlyName: string;
    FBeforeChange: TNotifyEvent;
    FAfterChange: TNotifyEvent;

    procedure SetValue(const AValue: string);
    function GetFriendlyName: string;
    procedure SetFriendlyName(const AValue: string);

  protected
    FReadOnly: boolean;
    procedure SetReadOnly(Value: boolean);
    procedure CheckReadOnly;
    function Clone: TScPersistent; override;

  public
    constructor Create; overload;
    constructor Create(const Value: string); overload;
    procedure Assign(Source: TScPersistent); override;

    property Value: string read FValue write SetValue; // Gets or sets the dotted number of the identifier
    property FriendlyName: string read GetFriendlyName write SetFriendlyName; // Gets or sets the friendly name of the identifier
  end;

  TScOIds = class(TScPersistentObjectList)
  private
    function GetOId(Index: integer): TScOId;
    procedure SetOId(Index: integer; Item: TScOId);
  protected
    function GetItemClassType: TScPersistentClass; override;

  public
    property OIds[Index: integer]: TScOId read GetOId write SetOId; default;
  end;

  TScASN1AlgorithmIdentifier = class(TScPersistent)
  private
    FOId: TScOId;
    FParameters: TBytes;

    procedure SetOId(Value: TScOId);
    procedure SetParameters(const Value: TBytes);

  protected
    FReadOnly: boolean;
    procedure SetReadOnly(Value: boolean);
    procedure CheckReadOnly;

    function Clone: TScPersistent; override;
    procedure Parse(LexemInfo: TScLexemInfo);
    procedure Update; virtual;

  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TScPersistent); override;

    procedure InitAsRSA;
    procedure Clear;

    property Algorithm: TScOId read FOId write SetOId;
    property Parameters: TBytes read FParameters write SetParameters;
  end;

  TScSignatureAlgorithmIdentifier = class(TScASN1AlgorithmIdentifier)
  private
    FHashAlgorithm: TScHashAlgorithm;
    FPaddingMode: TScPaddingMode;
    FPSSParams: TScPSSParams;

  protected
    procedure Update; override;

  public
    constructor Create; override;
    destructor Destroy; override;

    property HashAlgorithm: TScHashAlgorithm read FHashAlgorithm;
    property PaddingMode: TScPaddingMode read FPaddingMode;
    property PSSParams: TScPSSParams read FPSSParams;
  end;

  TScASN1AlgorithmIdentifiers = class(TScPersistentObjectList)
  private
    function GetAlgorithmIdentifier(Index: integer): TScASN1AlgorithmIdentifier;
    procedure SetAlgorithmIdentifier(Index: integer; Item: TScASN1AlgorithmIdentifier);
  protected
    function GetItemClassType: TScPersistentClass; override;

  public
    property AlgorithmIdentifiers[Index: integer]: TScASN1AlgorithmIdentifier read GetAlgorithmIdentifier write SetAlgorithmIdentifier; default;
  end;

  TScASN1Attribute = class(TScPersistent)
  private
    FOId: TScOId;
    FASN1DataType: TScASN1DataType;
    FRawData: TBytes;

    procedure SetOId(Value: TScOId);
    function GetAsString: string;

  protected
    function Clone: TScPersistent; override;
  public
    constructor Create(const AOId: string; const AValue: TBytes; ADataType: TScASN1DataType); overload;
    constructor Create(const AOId, AValue: string; ADataType: TScASN1DataType); overload;
    destructor Destroy; override;

    procedure Assign(Source: TScPersistent); override;
    function Equals(AttrValue: TScASN1Attribute): boolean; reintroduce;
    function Encode: TBytes;

    property OId: TScOId read FOId write SetOId;
    property ASN1DataType: TScASN1DataType read FASN1DataType;
    property RawData: TBytes read FRawData;
    property AsString: string read GetAsString;
  end;

  TScASN1Attributes = class(TScPersistentObjectList)
  private
    function GetAttribute(Index: integer): TScASN1Attribute;
    procedure SetAttribute(Index: integer; Item: TScASN1Attribute);
  protected
    function GetItemClassType: TScPersistentClass; override;

  public
    property Attributes[Index: integer]: TScASN1Attribute read GetAttribute write SetAttribute; default;
  end;

  TScPKCS7Attribute = class(TScPersistent)
  private
    FOId: TScOId;
    FValues: TScASN1Attributes;
    procedure SetOId(Value: TScOId);
    function GetValueCount: integer;
    function GetValue(Index: integer): TScASN1Attribute;
    procedure SetValue(Index: integer; Value: TScASN1Attribute);

  protected
    function Clone: TScPersistent; override;
    procedure Parse(LexemInfo: TScLexemInfo);

  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TScPersistent); override;

    function Encode: TBytes;

    procedure AddValue(Item: TScASN1Attribute); overload;
    procedure AddValue(const Value: TBytes; ASN1DataType: TScASN1DataType); overload;
    procedure AddValue(const Value: string; ASN1DataType: TScASN1DataType); overload;
    procedure ClearValues;
    procedure DeleteValue(Index: integer);

    property OId: TScOId read FOId write SetOId;
    property ValueCount: integer read GetValueCount;
    property Values[Index: integer]: TScASN1Attribute read GetValue write SetValue;
  end;

  TScPKCS7Attributes = class(TScPersistentObjectList)
  private
    function GetAttribute(Index: integer): TScPKCS7Attribute;
    procedure SetAttribute(Index: integer; Item: TScPKCS7Attribute);
  protected
    function GetItemClassType: TScPersistentClass; override;

    function GetASN1Description: TScASN1Description; virtual;
    procedure Parse(LexemInfo: TScLexemInfo);

  public
    function Encode: TBytes;

    property Attributes[Index: integer]: TScPKCS7Attribute read GetAttribute write SetAttribute; default;
  end;

  // Relative Distinguished Name (RDN) of a certificate
  TScRelativeDistinguishedName = class(TScPersistent)
  private
    FList: TScASN1Attributes;

    function GetItem(Index: integer): TScASN1Attribute;
    function GetName(Index: integer): string;
    function GetValue(const OId: string): string;
    function GetValueFromIndex(Index: integer): string;
    function GetCount: integer;

  protected
    function IndexOfOId(const OId: string): integer; // Searches for an Object with the specified Object identifier and returns the zero-based index of the first occurrence within the entire list.
    procedure Parse(LexemInfo: TScLexemInfo);
    function Clone: TScPersistent; override;

  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TScPersistent); override;

    function Encode: TBytes;

    procedure Add(ASN1Attribute: TScASN1Attribute); overload;
    procedure Add(const OId, Value: string; DataType: TScASN1DataType = dtPrintableString); overload;
    procedure Clear;
    procedure Remove(const OId: string); // Removes the first occurrence of a specific element from the list.
    procedure RemoveAt(const Index: integer); // Removes the element at the specified index of the list.

    function Equals(Value: TScRelativeDistinguishedName): boolean; reintroduce;
    // Gets the comma-delimited distinguished name from an X500 certificate. "CN=TTCA, OU=TT Certification, O=TT, L=Dur, S=Western Cape, C=ZA"
    function ToString: string; {$IFDEF VER12P}override;{$ENDIF} {$IFDEF FPC}reintroduce;{$ENDIF}

    property Count: integer read GetCount; // Gets the number of elements actually contained in the list.
    property Items[Index: integer]: TScASN1Attribute read GetItem; default;
    property Names[Index: integer]: string read GetName;
    property Values[const OId: string]: string read GetValue;
    property ValueFromIndex[Index: integer]: string read GetValueFromIndex;
  end;

  // Distinguished Name (DN) of a certificate
  TScDistinguishedName = class(TScPersistent)
  private
    FList: TCRObjectList;

    function GetItem(Index: integer): TScRelativeDistinguishedName;
    function GetCount: integer;
    function GetName(Index: integer): string;
    function GetValue(const OId: string): string;
    function GetValueFromIndex(Index: integer): string;
    function GetValueCount: integer;

  protected
    function Clone: TScPersistent; override;
    procedure Parse(LexemInfo: TScLexemInfo);

  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TScPersistent); override;

    procedure Decode(const Buffer: TBytes; Offset, Size: integer);
    function Encode: TBytes;

    function IndexOfOId(const OId: string): integer;
    procedure Add(RelativeDistinguishedName: TScRelativeDistinguishedName); overload;
    procedure Add(const OId, Value: string; DataType: TScASN1DataType = dtPrintableString); overload;
    procedure Clear;
    procedure RemoveAt(const Index: integer);

    function Equals(Value: TScDistinguishedName): boolean; reintroduce;
    // Gets the comma-delimited distinguished name from an X500 certificate. "CN=TTCA, OU=TT Certification, O=TT, L=Dur, S=Western Cape, C=ZA"
    function ToString: string; {$IFDEF VER12P}override;{$ENDIF} {$IFDEF FPC}reintroduce;{$ENDIF}

    property Count: integer read GetCount; // Gets the number of TScRelativeDistinguishedName.
    property Items[Index: integer]: TScRelativeDistinguishedName read GetItem; default;
    property Names[Index: integer]: string read GetName;
    property Values[const OId: string]: string read GetValue;
    property ValueFromIndex[Index: integer]: string read GetValueFromIndex;
    property ValueCount: integer read GetValueCount; // Gets the number of elements actually contained in the list.
  end;

  TScDistinguishedNameList = class(TScPersistentObjectList)
  private
    function GetName(Index: integer): TScDistinguishedName;
    procedure SetName(Index: integer; Item: TScDistinguishedName);
  protected
    function GetItemClassType: TScPersistentClass; override;

  public
    property Names[Index: integer]: TScDistinguishedName read GetName write SetName; default;
  end;

  TScGeneralName = class(TScPersistent)
  private
    FName: string;
    FValue: string;
    FDirectoryName: TScDistinguishedName;
    procedure SetDirectoryName(Value: TScDistinguishedName);

  protected
    function Clone: TScPersistent; override;
    procedure Parse(LexemInfo: TScLexemInfo);

  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(Source: TScPersistent); override;
    function Equals(GName: TScGeneralName): boolean; reintroduce; overload;
    function Equals(DName: TScDistinguishedName): boolean; reintroduce; overload;

    function ToString: string; {$IFDEF VER12P}override;{$ENDIF} {$IFDEF FPC}reintroduce;{$ENDIF}

    property Name: string read FName write FName;
    property Value: string read FValue write FValue;
    property DirectoryName: TScDistinguishedName read FDirectoryName write SetDirectoryName;
  end;

  TScGeneralNames = class(TScPersistentObjectList)
  private
    function GetGeneralName(Index: integer): TScGeneralName;
    procedure SetGeneralName(Index: integer; Item: TScGeneralName);
  protected
    function GetItemClassType: TScPersistentClass; override;
    procedure Parse(LexemInfo: TScLexemInfo);

  public
    function Equals(Value: TScGeneralNames): boolean; reintroduce;
    function HasEqual(Value: TScDistinguishedName): boolean;

    function FindByName(const AName: string): TScGeneralName;
    property GeneralNames[Index: integer]: TScGeneralName read GetGeneralName write SetGeneralName; default;

    function ToString: string; {$IFDEF VER12P}override;{$ENDIF} {$IFDEF FPC}reintroduce;{$ENDIF}
  end;

  TScCertificateExtensionClass = class of TScCertificateExtension;

  TScCertificateExtension = class(TScPersistent)
  protected
    FOId: TScOId;
    FCritical: boolean;
    FRawData: TBytes;

    function Clone: TScPersistent; override;
    procedure Parse(const DERValue: TBytes); virtual;

  public
    constructor Create(const OId: string; ACritical: boolean; const DERValue: TBytes); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TScPersistent); override;

    function ToString: string; {$IFDEF VER12P}override;{$ELSE}virtual;{$ENDIF} {$IFDEF FPC}reintroduce;{$ENDIF}

    property OId: TScOId read FOId; // The object identifier of the extension.
    property Critical: boolean read FCritical; // True, if it is a critical extension.
    property RawData: TBytes read FRawData; // A byte array that contains the encoded extension.
  end;

  TScCertBasicConstraintsExtension = class(TScCertificateExtension)
  private
    FCertificateAuthority: boolean;
    FHasPathLengthConstraint: boolean;
    FPathLengthConstraint: integer;

  protected
    function Clone: TScPersistent; override;
    procedure Parse(const DERValue: TBytes); override;
  public
    constructor Create(const OId: string; ACritical: boolean; const DERValue: TBytes); override;
    function ToString: string; override;

    property CertificateAuthority: boolean read FCertificateAuthority write FCertificateAuthority;
    property HasPathLengthConstraint: boolean read FHasPathLengthConstraint write FHasPathLengthConstraint;
    property PathLengthConstraint: integer read FPathLengthConstraint write FPathLengthConstraint;
  end;

  // !!! Synchronize with KEY_USAGE_FLAGS
  TScKeyUsageFlag = (
    kfDigitalSignature, // The key can be used as a digital signature.
    kfNonRepudiation,   // The key can be used for authentication.
    kfKeyEncipherment,  // The key can be used for key encryption.
    kfDataEncipherment, // The key can be used for data encryption.
    kfKeyAgreement,     // The key can be used to determine key agreement, such as a key created using the Diffie-Hellman key agreement algorithm.
    kfKeyCertSign,      // The key can be used to sign certificates.
    kfCRLSign,          // The key can be used to sign a certificate revocation list (CRL).
    kfEncipherOnly,     // The key can be used for encryption only.
    kfDecipherOnly      // The key can be used for decryption only.
  );

  TScKeyUsageFlags = set of TScKeyUsageFlag;

  TScCertKeyUsageExtension = class(TScCertificateExtension)
  private
    FKeyUsages: TScKeyUsageFlags;

  protected
    function Clone: TScPersistent; override;
    procedure Parse(const DERValue: TBytes); override;
  public
    constructor Create(const OId: string; ACritical: boolean; const DERValue: TBytes); override;
    function ToString: string; override;

    property KeyUsages: TScKeyUsageFlags read FKeyUsages write FKeyUsages;
  end;

  TScCertExtendedKeyUsageExtension = class(TScCertificateExtension)
  private
    FExtendedKeyUsages: TScOIds;

  protected
    function Clone: TScPersistent; override;
    procedure Parse(const DERValue: TBytes); override;
  public
    constructor Create(const OId: string; ACritical: boolean; const DERValue: TBytes); override;
    destructor Destroy; override;
    function ToString: string; override;

    property ExtendedKeyUsages: TScOIds read FExtendedKeyUsages;
  end;

  TScCertSubjectKeyIdExtension = class(TScCertificateExtension)
  private
    FSubjectKeyIdentifier: string;

  protected
    function Clone: TScPersistent; override;
    procedure Parse(const DERValue: TBytes); override;
  public
    constructor Create(const OId: string; ACritical: boolean; const DERValue: TBytes); override;
    function ToString: string; override;

    property SubjectKeyIdentifier: string read FSubjectKeyIdentifier write FSubjectKeyIdentifier;
  end;

  TScCertAuthorityKeyIdExtension = class(TScCertificateExtension)
  private
    FKeyIdentifier: string;
    FCertSerialNumber: string;
    FCertIssuers: TScGeneralNames;

  protected
    function Clone: TScPersistent; override;
    procedure Parse(const DERValue: TBytes); override;
  public
    constructor Create(const OId: string; ACritical: boolean; const DERValue: TBytes); override;
    destructor Destroy; override;
    function ToString: string; override;

    property KeyIdentifier: string read FKeyIdentifier write FKeyIdentifier;
    property CertSerialNumber: string read FCertSerialNumber write FCertSerialNumber;
    property CertIssuers: TScGeneralNames read FCertIssuers;
  end;

  TScQualifier = record
    QualifierId: string;
    CpsUri: string;
    NoticeReferenceOrganization: string;
    ExplicitText: string;
  end;

  TScPolicy = class(TScPersistent)
  protected
    function Clone: TScPersistent; override;

  public
    Identifier: string;
    Qualifiers: array of TScQualifier;

    procedure Assign(Source: TScPersistent); override;
  end;

  TScPolicyList = class(TScPersistentObjectList)
  private
    function GetPolicy(Index: integer): TScPolicy;
    procedure SetPolicy(Index: integer; Item: TScPolicy);
  protected
    function GetItemClassType: TScPersistentClass; override;

  public
    property Policies[Index: integer]: TScPolicy read GetPolicy write SetPolicy; default;
  end;

  TScCertPoliciesExtension = class(TScCertificateExtension)
  private
    FPolicyList: TScPolicyList;

  protected
    function Clone: TScPersistent; override;
    procedure Parse(const DERValue: TBytes); override;
  public
    constructor Create(const OId: string; ACritical: boolean; const DERValue: TBytes); override;
    destructor Destroy; override;
    function ToString: string; override;

    property Policies: TScPolicyList read FPolicyList;
  end;

  TScPolicyMapping = class(TScPersistent)
  private
    FIssuerDomainPolicy: TScOId;
    FSubjectDomainPolicy: TScOId;
  protected
    function Clone: TScPersistent; override;

  public
    constructor Create; overload;
    constructor Create(const AIssuerDomainPolicy, ASubjectDomainPolicy: string); overload;
    destructor Destroy; override;
    procedure Assign(Source: TScPersistent); override;

    property IssuerDomainPolicy: TScOId read FIssuerDomainPolicy;
    property SubjectDomainPolicy: TScOId read FSubjectDomainPolicy;
  end;

  TScPolicyMappingList = class(TScPersistentObjectList)
  private
    function GetPolicyMapping(Index: integer): TScPolicyMapping;
    procedure SetPolicyMapping(Index: integer; Item: TScPolicyMapping);
  protected
    function GetItemClassType: TScPersistentClass; override;

  public
    property PolicyMappings[Index: integer]: TScPolicyMapping read GetPolicyMapping write SetPolicyMapping; default;
  end;

  TScCertPolicyMappingsExtension = class(TScCertificateExtension)
  private
    FPolicyMappings: TScPolicyMappingList;

  protected
    function Clone: TScPersistent; override;
    procedure Parse(const DERValue: TBytes); override;
  public
    constructor Create(const OId: string; ACritical: boolean; const DERValue: TBytes); override;
    destructor Destroy; override;
    function ToString: string; override;

    property PolicyMappings: TScPolicyMappingList read FPolicyMappings;
  end;

  TScCertAlternativeNameExtension = class(TScCertificateExtension)
  private
    FGeneralNames: TScGeneralNames;

  protected
    function Clone: TScPersistent; override;
    procedure Parse(const DERValue: TBytes); override;
  public
    constructor Create(const OId: string; ACritical: boolean; const DERValue: TBytes); override;
    destructor Destroy; override;
    function ToString: string; override;

    property GeneralNames: TScGeneralNames read FGeneralNames;
  end;

  TScCertSubjectAlternativeNameExtension = class(TScCertAlternativeNameExtension)
  protected
    function Clone: TScPersistent; override;
  public
    constructor Create(const OId: string; ACritical: boolean; const DERValue: TBytes); override;
  end;

  TScCertIssuerAlternativeNameExtension = class(TScCertAlternativeNameExtension)
  protected
    function Clone: TScPersistent; override;
  public
    constructor Create(const OId: string; ACritical: boolean; const DERValue: TBytes); override;
  end;

  TScCertSubjectDirectoryAttributesExtension = class(TScCertificateExtension)
  private
    FDirectoryAttributes: TScPKCS7Attributes;

  protected
    function Clone: TScPersistent; override;
    procedure Parse(const DERValue: TBytes); override;
  public
    constructor Create(const OId: string; ACritical: boolean; const DERValue: TBytes); override;
    destructor Destroy; override;
    function ToString: string; override;

    property DirectoryAttributes: TScPKCS7Attributes read FDirectoryAttributes;
  end;

  // !!! Synchronize with CRL_REASONS
  TScCRLReason = (
    crKeyCompromise,
    crCACompromise,
    crAffiliationChanged,
    crSuperseded,
    crCessationOfOperation,
    crCertificateHold,
    crPrivilegeWithdrawn,
    crAACompromise,
    crRemoveFromCRL,
    crUnspecified
  );
  TScCRLReasons = set of TScCRLReason;

  TScCRLDistributionPoint = class(TScPersistent)
  private
    FDistributionPointName: TScGeneralNames;
    FReasons: TScCRLReasons;
    FCRLIssuer: TScGeneralNames;

  protected
    function Clone: TScPersistent; override;
    class procedure ParseReasons(ReasonsLexem: TScLexemInfo; out Reasons: TScCRLReasons);
    class function ReasonsToString(const Reasons: TScCRLReasons): string;

  public
    constructor Create; overload;
    destructor Destroy; override;
    procedure Assign(Source: TScPersistent); override;

    property DistributionPointName: TScGeneralNames read FDistributionPointName;
    property Reasons: TScCRLReasons read FReasons;
    property CRLIssuer: TScGeneralNames read FCRLIssuer;
  end;

  TScCRLDistributionPointList = class(TScPersistentObjectList)
  private
    function GetCRLDistributionPoint(Index: integer): TScCRLDistributionPoint;
    procedure SetCRLDistributionPoint(Index: integer; Item: TScCRLDistributionPoint);
  protected
    function GetItemClassType: TScPersistentClass; override;

  public
    property CRLDistributionPoints[Index: integer]: TScCRLDistributionPoint read GetCRLDistributionPoint write SetCRLDistributionPoint; default;
  end;

  TScCertCRLDistributionPointsExtension = class(TScCertificateExtension)
  private
    FCRLDistributionPoints: TScCRLDistributionPointList;
  protected
    function Clone: TScPersistent; override;
    procedure Parse(const DERValue: TBytes); override;
  public
    constructor Create(const OId: string; ACritical: boolean; const DERValue: TBytes); override;
    destructor Destroy; override;
    function ToString: string; override;

    property CRLDistributionPoints: TScCRLDistributionPointList read FCRLDistributionPoints;
  end;

  TScCertFreshestCRLExtension = class(TScCertCRLDistributionPointsExtension)
  protected
    function Clone: TScPersistent; override;
  public
    constructor Create(const OId: string; ACritical: boolean; const DERValue: TBytes); override;
  end;

  TScInfoAccess = class(TScPersistent)
  private
    FAccessMethod: TScOid;
    FAccessLocation: TScGeneralName;
  protected
    function Clone: TScPersistent; override;

  public
    constructor Create; overload;
    destructor Destroy; override;
    procedure Assign(Source: TScPersistent); override;

    property AccessMethod: TScOid read FAccessMethod;
    property AccessLocation: TScGeneralName read FAccessLocation;
  end;

  TScInfoAccessList = class(TScPersistentObjectList)
  private
    function GetInfoAccess(Index: integer): TScInfoAccess;
    procedure SetInfoAccess(Index: integer; Item: TScInfoAccess);
  protected
    function GetItemClassType: TScPersistentClass; override;

  public
    property InfoAccesses[Index: integer]: TScInfoAccess read GetInfoAccess write SetInfoAccess; default;
  end;

  TScCertAuthorityInfoAccessExtension = class(TScCertificateExtension)
  private
    FAuthorityInfoAccessList: TScInfoAccessList;
  protected
    function Clone: TScPersistent; override;
    procedure Parse(const DERValue: TBytes); override;
  public
    constructor Create(const OId: string; ACritical: boolean; const DERValue: TBytes); override;
    destructor Destroy; override;
    function ToString: string; override;

    property AuthorityInfoAccessList: TScInfoAccessList read FAuthorityInfoAccessList;
  end;

  TScCertSubjectInfoAccessExtension = class(TScCertificateExtension)
  private
    FSubjectInfoAccessList: TScInfoAccessList;
  protected
    function Clone: TScPersistent; override;
    procedure Parse(const DERValue: TBytes); override;
  public
    constructor Create(const OId: string; ACritical: boolean; const DERValue: TBytes); override;
    destructor Destroy; override;
    function ToString: string; override;

    property SubjectInfoAccessList: TScInfoAccessList read FSubjectInfoAccessList;
  end;

  TScCRLNumberExtension = class(TScCertificateExtension)
  private
    FCRLNumber: string;
  protected
    function Clone: TScPersistent; override;
    procedure Parse(const DERValue: TBytes); override;
  public
    constructor Create(const OId: string; ACritical: boolean; const DERValue: TBytes); override;
    function ToString: string; override;

    property CRLNumber: string read FCRLNumber;
  end;

  TScCRLDeltaIndicatorExtension = class(TScCertificateExtension)
  private
    FBaseCRLNumber: string;
  protected
    function Clone: TScPersistent; override;
    procedure Parse(const DERValue: TBytes); override;
  public
    constructor Create(const OId: string; ACritical: boolean; const DERValue: TBytes); override;
    function ToString: string; override;

    property BaseCRLNumber: string read FBaseCRLNumber;
  end;

  TScCRLIssuingDistributionPointExtension = class(TScCertificateExtension)
  private
    FDistributionPointName: TScGeneralNames;
    FOnlySomeReasons: TScCRLReasons;
    FIndirectCRL: boolean;
    FOnlyContainsUserCerts: boolean;
    FOnlyContainsCACerts: boolean;
    FOnlyContainsAttributeCerts: boolean;

  protected
    function Clone: TScPersistent; override;
    procedure Parse(const DERValue: TBytes); override;
  public
    constructor Create(const OId: string; ACritical: boolean; const DERValue: TBytes); override;
    destructor Destroy; override;
    function ToString: string; override;

    property DistributionPointName: TScGeneralNames read FDistributionPointName;
    property OnlySomeReasons: TScCRLReasons read FOnlySomeReasons;

    property IndirectCRL: boolean read FIndirectCRL;
    property OnlyContainsUserCerts: boolean read FOnlyContainsUserCerts;
    property OnlyContainsCACerts: boolean read FOnlyContainsCACerts;
    property OnlyContainsAttributeCerts: boolean read FOnlyContainsAttributeCerts;
  end;

  TScCRLReasonCodeExtension = class(TScCertificateExtension)
  private
    FCRLReason: TScCRLReason;
  protected
    function Clone: TScPersistent; override;
    procedure Parse(const DERValue: TBytes); override;
  public
    constructor Create(const OId: string; ACritical: boolean; const DERValue: TBytes); override;
    function ToString: string; override;

    property CRLReason: TScCRLReason read FCRLReason;
  end;

  TScCRLInvalidityDateExtension = class(TScCertificateExtension)
  private
    FInvalidityDate: TDateTime;
  protected
    function Clone: TScPersistent; override;
    procedure Parse(const DERValue: TBytes); override;
  public
    constructor Create(const OId: string; ACritical: boolean; const DERValue: TBytes); override;
    function ToString: string; override;

    property InvalidityDate: TDateTime read FInvalidityDate;
  end;

  TScCRLCertificateIssuerExtension = class(TScCertificateExtension)
  private
    FCertificateIssuer: TScGeneralNames;
  protected
    function Clone: TScPersistent; override;
    procedure Parse(const DERValue: TBytes); override;
  public
    constructor Create(const OId: string; ACritical: boolean; const DERValue: TBytes); override;
    destructor Destroy; override;
    function ToString: string; override;

    property CertificateIssuer: TScGeneralNames read FCertificateIssuer;
  end;

  TScSignedCertificateTimestamp = class(TScPersistent)
  private
    FVersion: integer;
    FLogID: TBytes;
    FTimeStamp: TDateTime;
    FSignatureHash: TScHashAlgorithm;
    FSignatureAlgorithm: TScAsymmetricAlgorithm;
    FSignature: TBytes;
  protected
    function Clone: TScPersistent; override;
    procedure Parse(const Buffer: TBytes; Offset: integer);

  public
    procedure Assign(Source: TScPersistent); override;

    function ToString: string; {$IFDEF VER12P}override;{$ENDIF} {$IFDEF FPC}reintroduce;{$ENDIF}

    property Version: integer read FVersion;
    property LogID: TBytes read FLogID;
    property TimeStamp: TDateTime read FTimeStamp;
    property SignatureHash: TScHashAlgorithm read FSignatureHash;
    property SignatureAlgorithm: TScAsymmetricAlgorithm read FSignatureAlgorithm;
    property Signature: TBytes read FSignature;
  end;

  TScSignedCertificateTimestampList = class(TScPersistentObjectList)
  private
    function GetSCT(Index: integer): TScSignedCertificateTimestamp;
    procedure SetSCT(Index: integer; Item: TScSignedCertificateTimestamp);
  protected
    function GetItemClassType: TScPersistentClass; override;

  public
    function ToString: string; {$IFDEF VER12P}override;{$ENDIF} {$IFDEF FPC}reintroduce;{$ENDIF}

    property SCTs[Index: integer]: TScSignedCertificateTimestamp read GetSCT write SetSCT; default;
  end;

  TScCertSignedCertificateTimestampListExtension = class(TScCertificateExtension)
  private
    FSCTList: TScSignedCertificateTimestampList;
  protected
    function Clone: TScPersistent; override;
    procedure Parse(const DERValue: TBytes); override;
  public
    constructor Create(const OId: string; ACritical: boolean; const DERValue: TBytes); override;
    destructor Destroy; override;
    function ToString: string; override;

    property SCTList: TScSignedCertificateTimestampList read FSCTList;
  end;

  TScExtensions = class(TScPersistentObjectList)
  private
    function GetExtension(Index: integer): TScCertificateExtension;
    procedure SetExtension(Index: integer; Item: TScCertificateExtension);
  protected
    function GetItemClassType: TScPersistentClass; override;
    procedure Parse(LexemInfo: TScLexemInfo);

  public
    function FindExtensionByClass(AClass: TScCertificateExtensionClass): TScCertificateExtension;
    property Extensions[Index: integer]: TScCertificateExtension read GetExtension write SetExtension; default;
  end;

  TScASNUtils = class
  public
    class procedure Parse(Obj: TScASN1AlgorithmIdentifier; LexemInfo: TScLexemInfo); overload;
    class procedure Parse(Obj: TScDistinguishedName; LexemInfo: TScLexemInfo); overload;
    class procedure Parse(Obj: TScPKCS7Attributes; LexemInfo: TScLexemInfo); overload;
    class procedure Parse(Obj: TScExtensions; LexemInfo: TScLexemInfo); overload;
    class procedure SetOIdBeforeChange(OId: TScOId; OnChange: TNotifyEvent);
    class procedure SetOIdAfterChange(OId: TScOId; OnChange: TNotifyEvent);
    class procedure SetReadOnly(OId: TScOId; Value: boolean); overload;
    class procedure SetReadOnly(ASN1AlgorithmIdentifier: TScASN1AlgorithmIdentifier; Value: boolean); overload;
  end;

implementation

uses
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
  ScReaderWriter, ScAlgorithmSupport, ScCipherSuites;
{$ENDIF}

const
  CRL_REASONS: array[TScCRLReason] of string = (
    'Key Compromise',
    'CA Compromise',
    'Affiliation Changed',
    'Superseded',
    'Cessation Of Operation',
    'Certificate Hold',
    'Privilege Withdrawn',
    'AA Compromise',
    'Remove From CRL',
    'Unspecified'
  );

{ TScPSSParams }

constructor TScPSSParams.Create;
begin
  inherited;

  Init;
end;

procedure TScPSSParams.Init;
begin
  FHashAlgorithm := haSHA1;
  FMaskGenAlgorithm := OID_RSA_MGF1;
  FMaskGenHashAlgorithm := haSHA1;
  FSaltLength := 20;
  FTrailerField := 0;
end;

procedure TScPSSParams.Assign(Source: TScPSSParams);
begin
  if Source = nil then
    raise EScError.Create(seInvalidInputArgs);

  FHashAlgorithm := Source.FHashAlgorithm;
  FMaskGenAlgorithm := Source.FMaskGenAlgorithm;
  FMaskGenHashAlgorithm := Source.FMaskGenHashAlgorithm;
  FSaltLength := Source.FSaltLength;
  FTrailerField := Source.FTrailerField;
end;

procedure TScPSSParams.Decode(const RawData: TBytes);
var
  ASN1Compiler: TScASN1Compiler;
  MaskGenAlgorithmLexem: TScLexemInfo;
  OId: string;
begin
  if Length(RawData) = 0 then
    raise EScError.Create(seInvalidInputArgs);

  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_RSASSA_PSS_PARAMS_DESC, RawData) then
      raise EScError.Create(seWrongDataFormat);

    OId := ASN1Compiler['HashAlgorithm']['Algorithm'].AsString;
    if OId <> '' then
      FHashAlgorithm := CipherFactory.OidToHashAlgorithm(OId)
    else
      FHashAlgorithm := haSHA1;

    MaskGenAlgorithmLexem := ASN1Compiler['MaskGenAlgorithm']['Algorithm'];
    if not MaskGenAlgorithmLexem.IsNull and (MaskGenAlgorithmLexem.AsString <> OID_RSA_MGF1) then
      raise EScError.Create(seUnknownMGFId);

    OId := ASN1Compiler['MaskGenAlgorithm']['Parameters']['Algorithm'].AsString;
    if OId <> '' then
      FMaskGenHashAlgorithm := CipherFactory.OidToHashAlgorithm(OId)
    else
      FMaskGenHashAlgorithm := haSHA1;

    if not ASN1Compiler['SaltLength'].IsNull then
      FSaltLength := ASN1Compiler['SaltLength'].AsInteger
    else
      FSaltLength := 20;

    FTrailerField := ASN1Compiler['TrailerField'].AsInteger;
  finally
    ASN1Compiler.Free;
  end;
end;

function TScPSSParams.Encode: TBytes;
var
  ASN1Compiler: TScASN1Compiler;
begin
  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_RSASSA_PSS_PARAMS_DESC) then
      raise EScError.Create(seWrongDataFormat);

    if FHashAlgorithm <> haSHA1 then
      ASN1Compiler['HashAlgorithm']['Algorithm'].AsString := CipherFactory.HashAlgorithmToOid(FHashAlgorithm);

    if FMaskGenHashAlgorithm <> haSHA1 then begin
      ASN1Compiler['MaskGenAlgorithm']['Algorithm'].AsString := FMaskGenAlgorithm;
      ASN1Compiler['MaskGenAlgorithm']['Parameters']['Algorithm'].AsString := CipherFactory.HashAlgorithmToOid(FMaskGenHashAlgorithm);
    end;

    if FSaltLength <> 20 then
      ASN1Compiler['SaltLength'].AsInteger := FSaltLength;

    if FTrailerField <> 0 then
      ASN1Compiler['TrailerField'].AsInteger := FTrailerField;

    Result := ASN1Compiler.Build;
  finally
    ASN1Compiler.Free;
  end;
end;

{ TScOAEPParams }

constructor TScOAEPParams.Create;
begin
  inherited;

  FHashAlgorithm := haSHA1;
  FMaskGenAlgorithm := OID_RSA_MGF1;
  FMaskGenHashAlgorithm := haSHA1;
end;

procedure TScOAEPParams.Assign(Source: TScOAEPParams);
begin
  if Source = nil then
    raise EScError.Create(seInvalidInputArgs);

  FHashAlgorithm := Source.FHashAlgorithm;
  FMaskGenAlgorithm := Source.FMaskGenAlgorithm;
  FMaskGenHashAlgorithm := Source.FMaskGenHashAlgorithm;
end;

procedure TScOAEPParams.Decode(const RawData: TBytes);
var
  ASN1Compiler: TScASN1Compiler;
  MaskGenAlgorithmLexem: TScLexemInfo;
  OId: string;
begin
  if Length(RawData) = 0 then
    raise EScError.Create(seInvalidInputArgs);

  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_RSAES_OAEP_PARAMS_DESC, RawData) then
      raise EScError.Create(seWrongDataFormat);

    OId := ASN1Compiler['HashAlgorithm']['Algorithm'].AsString;
    if OId <> '' then
      FHashAlgorithm := CipherFactory.OidToHashAlgorithm(OId)
    else
      FHashAlgorithm := haSHA1;

    MaskGenAlgorithmLexem := ASN1Compiler['MaskGenAlgorithm']['Algorithm'];
    if not MaskGenAlgorithmLexem.IsNull and (MaskGenAlgorithmLexem.AsString <> OID_RSA_MGF1) then
      raise EScError.Create(seUnknownMGFId);

    OId := ASN1Compiler['MaskGenAlgorithm']['Parameters']['Algorithm'].AsString;
    if OId <> '' then
      FMaskGenHashAlgorithm := CipherFactory.OidToHashAlgorithm(OId)
    else
      FMaskGenHashAlgorithm := haSHA1;
  finally
    ASN1Compiler.Free;
  end;
end;

function TScOAEPParams.Encode: TBytes;
var
  ASN1Compiler: TScASN1Compiler;
begin
  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_RSAES_OAEP_PARAMS_DESC) then
      raise EScError.Create(seWrongDataFormat);

    if FHashAlgorithm <> haSHA1 then
      ASN1Compiler['HashAlgorithm']['Algorithm'].AsString := CipherFactory.HashAlgorithmToOid(FHashAlgorithm);

    if FMaskGenHashAlgorithm <> haSHA1 then begin
      ASN1Compiler['MaskGenAlgorithm']['Algorithm'].AsString := FMaskGenAlgorithm;
      ASN1Compiler['MaskGenAlgorithm']['Parameters']['Algorithm'].AsString := CipherFactory.HashAlgorithmToOid(FMaskGenHashAlgorithm);
    end;

    Result := ASN1Compiler.Build;
  finally
    ASN1Compiler.Free;
  end;
end;

function TScOAEPParams.GetEmptyStrHash: TBytes;
var
  csp: THashAlgorithm;
begin
  if FEmptyStrHash = nil then begin
    csp := CipherFactory.CreateHash(FHashAlgorithm);
    try
      FEmptyStrHash := csp.ComputeHash(nil);
    finally
      csp.Free;
    end;
  end;

  Result := FEmptyStrHash;
end;

procedure TScOAEPParams.SetHashAlgorithm(Value: TScHashAlgorithm);
begin
  if FHashAlgorithm <> Value then begin
    FHashAlgorithm := Value;
    FEmptyStrHash := nil;
  end;
end;

{ TScOId }

constructor TScOId.Create;
begin
  inherited;
end;

constructor TScOId.Create(const Value: string);
begin
  inherited Create;

  SetValue(Value);
end;

procedure TScOId.SetReadOnly(Value: boolean);
begin
  FReadOnly := Value;
end;

procedure TScOId.CheckReadOnly;
begin
  if FReadOnly then
    raise EScError.Create(seChangingReadOnlyObject);
end;

function TScOId.Clone: TScPersistent;
begin
  Result := TScOId.Create;
  Result.Assign(Self);
end;

procedure TScOId.Assign(Source: TScPersistent);
begin
  if (Source = nil) or not IsClass(Source, TScOId) then
    RaiseAssignError(Source);

  CheckReadOnly;

  if Self <> Source then begin
    if Assigned(FBeforeChange) then
      FBeforeChange(Self);

    FValue := TScOId(Source).FValue;
    FFriendlyName := TScOId(Source).FFriendlyName;

    if Assigned(FAfterChange) then
      FAfterChange(Self);
  end;
end;

procedure TScOId.SetValue(const AValue: string);
begin
  CheckReadOnly;

  if FValue <> AValue then begin
    if Assigned(FBeforeChange) then
      FBeforeChange(Self);

    FValue := AValue;
    if AValue = '' then
      FFriendlyName := ''
    else
      FFriendlyName := OIdToFriendlyName(AValue);

    if Assigned(FAfterChange) then
      FAfterChange(Self);
  end;
end;

function TScOId.GetFriendlyName: string;
begin
  if FFriendlyName <> '' then
    Result := FFriendlyName
  else
    Result := FValue;
end;

procedure TScOId.SetFriendlyName(const AValue: string);
var
  Str: string;
begin
  CheckReadOnly;

  if FFriendlyName <> AValue then begin
    if Assigned(FBeforeChange) then
      FBeforeChange(Self);

    FFriendlyName := AValue;
    Str := FriendlyNameToOId(AValue);
    if Str <> '' then
      FValue := Str;

    if Assigned(FAfterChange) then
      FAfterChange(Self);
  end;
end;

{ TScOIds }

function TScOIds.GetItemClassType: TScPersistentClass;
begin
  Result := TScOId;
end;

function TScOIds.GetOId(Index: integer): TScOId;
begin
  Result := TObject(Items[Index]) as TScOId;
end;

procedure TScOIds.SetOId(Index: integer; Item: TScOId);
begin
  Items[Index] := Item;
end;

{ TScASN1AlgorithmIdentifier }

constructor TScASN1AlgorithmIdentifier.Create;
begin
  inherited Create;

  FOId := TScOId.Create;
end;

destructor TScASN1AlgorithmIdentifier.Destroy;
begin
  FOId.Free;

  inherited;
end;

procedure TScASN1AlgorithmIdentifier.Update;
begin

end;

procedure TScASN1AlgorithmIdentifier.SetReadOnly(Value: boolean);
begin
  FReadOnly := Value;
  FOId.SetReadOnly(Value);
end;

procedure TScASN1AlgorithmIdentifier.CheckReadOnly;
begin
  if FReadOnly then
    raise EScError.Create(seChangingReadOnlyObject);
end;

function TScASN1AlgorithmIdentifier.Clone: TScPersistent;
begin
  Result := TScASN1AlgorithmIdentifier.Create;
  Result.Assign(Self);
end;

procedure TScASN1AlgorithmIdentifier.Assign(Source: TScPersistent);
begin
  if (Source = nil) or not IsClass(Source, TScASN1AlgorithmIdentifier) then
    RaiseAssignError(Source);

  CheckReadOnly;

  FOId.Assign(TScASN1AlgorithmIdentifier(Source).FOId);
  FParameters := TScASN1AlgorithmIdentifier(Source).FParameters;
  Update;
end;

procedure TScASN1AlgorithmIdentifier.Clear;
begin
  CheckReadOnly;

  FOId.Value := '';
  SetLength(FParameters, 0);
  Update;
end;

procedure TScASN1AlgorithmIdentifier.InitAsRSA;
begin
  CheckReadOnly;

  FOId.Value := OID_RSA_ENCRYPTION;
  SetLength(FParameters, 2);
  FParameters[0] := 5; // ASN1 ctNull
  FParameters[1] := 0;
  Update;
end;

procedure TScASN1AlgorithmIdentifier.Parse(LexemInfo: TScLexemInfo);
var
  ParametersLexem: TScLexemInfo;
  OldReadOnly: boolean;
begin
  OldReadOnly := FReadOnly;
  SetReadOnly(False);

  try
    FOId.Value := LexemInfo['Algorithm'].AsOID;
    ParametersLexem := LexemInfo['Parameters'];
    if ParametersLexem.IsNull then
      SetLength(FParameters, 0)
    else
      FParameters := ParametersLexem.EncodedData;

    Update;
  finally
    SetReadOnly(OldReadOnly);
  end;
end;

procedure TScASN1AlgorithmIdentifier.SetOId(Value: TScOId);
begin
  CheckReadOnly;

  FOId.Assign(Value);
  Update;
end;

procedure TScASN1AlgorithmIdentifier.SetParameters(const Value: TBytes);
begin
  CheckReadOnly;

  SetLength(FParameters, Length(Value));
  if Length(Value) > 0 then
    Move(Value[0], FParameters[0], Length(Value));
  Update;
end;

{ TScSignatureAlgorithmIdentifier }

constructor TScSignatureAlgorithmIdentifier.Create;
begin
  inherited;
  FPSSParams := TScPSSParams.Create;
end;

destructor TScSignatureAlgorithmIdentifier.Destroy;
begin
  FPSSParams.Free;
  inherited;
end;

procedure TScSignatureAlgorithmIdentifier.Update;
begin
  if (FOId.Value = OID_Ed25519) or (FOId.Value = OID_X25519) then begin
    FHashAlgorithm := haNone;
    FPaddingMode := pmNone;
  end
  else
  if FOId.Value = OID_SHA1_WITH_RSA_ENC then begin
    FHashAlgorithm := haSHA1;
    FPaddingMode := pmPKCS1;
  end
  else
  if (FOId.Value = OID_DSA_WITH_SHA1) or (FOId.Value = OID_ECDSA_WITH_SHA1) then begin
    FHashAlgorithm := haSHA1;
    FPaddingMode := pmNone;
  end
  else
  if FOId.Value = OID_RSA_PSS_ENCRYPTION then begin
    FHashAlgorithm := haSHA1;
    FPaddingMode := pmPSS;
  end
  else
  if FOId.Value = OID_SHA256_WITH_RSA_ENC then begin
    FHashAlgorithm := haSHA2_256;
    FPaddingMode := pmPKCS1;
  end
  else
  if (FOId.Value = OID_DSA_WITH_SHA256) or (FOId.Value = OID_ECDSA_WITH_SHA256) then begin
    FHashAlgorithm := haSHA2_256;
    FPaddingMode := pmNone;
  end
  else
  if FOId.Value = OID_SHA224_WITH_RSA_ENC then begin
    FHashAlgorithm := haSHA2_224;
    FPaddingMode := pmPKCS1;
  end
  else
  if (FOId.Value = OID_DSA_WITH_SHA224) or (FOId.Value = OID_ECDSA_WITH_SHA224) then begin
    FHashAlgorithm := haSHA2_224;
    FPaddingMode := pmNone;
  end
  else
  if FOId.Value = OID_SHA512_WITH_RSA_ENC then begin
    FHashAlgorithm := haSHA2_512;
    FPaddingMode := pmPKCS1;
  end
  else
  if FOId.Value = OID_ECDSA_WITH_SHA512 then begin
    FHashAlgorithm := haSHA2_512;
    FPaddingMode := pmNone;
  end
  else
  if FOId.Value = OID_SHA384_WITH_RSA_ENC then begin
    FHashAlgorithm := haSHA2_384;
    FPaddingMode := pmPKCS1;
  end
  else
  if FOId.Value = OID_ECDSA_WITH_SHA384 then begin
    FHashAlgorithm := haSHA2_384;
    FPaddingMode := pmNone;
  end
  else
  if FOId.Value = OID_MD5_WITH_RSA_ENC then begin
    FHashAlgorithm := haMD5;
    FPaddingMode := pmPKCS1;
  end
  else
  if FOId.Value = OID_MD4_WITH_RSA_ENC then begin
    FHashAlgorithm := haMD4;
    FPaddingMode := pmPKCS1;
  end
  else
  if FOId.Value = OID_MD2_WITH_RSA_ENC then begin
    FHashAlgorithm := haMD2;
    FPaddingMode := pmPKCS1;
  end
  else
    raise EScError.Create(seUnknownHashAlgorithm);

  if FPaddingMode = pmPSS then
    FPSSParams.Decode(FParameters)
  else
    FPSSParams.Init;
end;

{ TScASN1AlgorithmIdentifiers }

function TScASN1AlgorithmIdentifiers.GetItemClassType: TScPersistentClass;
begin
  Result := TScASN1AlgorithmIdentifier;
end;

function TScASN1AlgorithmIdentifiers.GetAlgorithmIdentifier(Index: integer): TScASN1AlgorithmIdentifier;
begin
  Result := TObject(Items[Index]) as TScASN1AlgorithmIdentifier;
end;

procedure TScASN1AlgorithmIdentifiers.SetAlgorithmIdentifier(Index: integer; Item: TScASN1AlgorithmIdentifier);
begin
  Items[Index] := Item;
end;

{ TScASN1Attribute }

constructor TScASN1Attribute.Create(const AOId: string; const AValue: TBytes; ADataType: TScASN1DataType);
begin
  inherited Create;

  FOId := TScOId.Create(AOId);
  FASN1DataType := ADataType;
  FRawData := AValue;
end;

constructor TScASN1Attribute.Create(const AOId, AValue: string; ADataType: TScASN1DataType);
var
  LexemData: TScLexemData;
begin
  inherited Create;

  FOId := TScOId.Create(AOId);
  FASN1DataType := ADataType;

  LexemData.DataType := FASN1DataType;
  TScASN1Writer.SetAsString(LexemData, AValue);
  FRawData := LexemData.Buffer;
end;

destructor TScASN1Attribute.Destroy;
begin
  FOId.Free;
  inherited;
end;

function TScASN1Attribute.Clone: TScPersistent;
begin
  Result := TScASN1Attribute.Create(FOId.Value, FRawData, FASN1DataType);
end;

procedure TScASN1Attribute.Assign(Source: TScPersistent);
begin
  if (Source = nil) or not IsClass(Source, TScASN1Attribute) then
    RaiseAssignError(Source);

  FOId.Assign(TScASN1Attribute(Source).FOId);
  FASN1DataType := TScASN1Attribute(Source).FASN1DataType;
  FRawData := TScASN1Attribute(Source).FRawData;
end;

function TScASN1Attribute.Equals(AttrValue: TScASN1Attribute): boolean;
begin
  Result := (FASN1DataType = AttrValue.FASN1DataType) and
    (Length(FRawData) = Length(AttrValue.FRawData)) and
    (MemCompare(@FRawData[0], @AttrValue.FRawData[0], Length(FRawData)) = 0);
end;

function TScASN1Attribute.Encode: TBytes;
var
  ASN1Compiler: TScASN1Compiler;
  ValueLexem: TScLexemInfo;
begin
  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_ATTR_AND_VALUE_DESC) then
      raise EScError.Create(seWrongDataFormat);

    ASN1Compiler['AttrType'].AsOID := FOId.Value;
    ValueLexem := ASN1Compiler['AttrValue'];
    ValueLexem.DataType := FASN1DataType;
    ValueLexem.AsBytes := FRawData;

    Result := ASN1Compiler.Build;
  finally
    ASN1Compiler.Free;
  end;
end;

function TScASN1Attribute.GetAsString: string;
var
  LexemData: TScLexemData;
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.WriteBuffer(FRawData[0], Length(FRawData));
    LexemData.StartPos := 0;
    LexemData.AddedSize := 0;
    LexemData.Size := Length(FRawData);
    LexemData.DataType := FASN1DataType;

    Result := TScASN1Reader.GetAsString(Stream, LexemData);
  finally
    Stream.Free;
  end;
end;

procedure TScASN1Attribute.SetOId(Value: TScOId);
begin
  FOId.Assign(Value);
end;

{ TScASN1Attributes }

function TScASN1Attributes.GetItemClassType: TScPersistentClass;
begin
  Result := TScASN1Attribute;
end;

function TScASN1Attributes.GetAttribute(Index: integer): TScASN1Attribute;
begin
  Result := TObject(Items[Index]) as TScASN1Attribute;
end;

procedure TScASN1Attributes.SetAttribute(Index: integer; Item: TScASN1Attribute);
begin
  if Item = nil then
    raise EScError.Create(seInvalidInputArgs);

  Items[Index] := Item;
end;

{ TScPKCS7Attribute }

constructor TScPKCS7Attribute.Create;
begin
  inherited Create;

  FValues := TScASN1Attributes.Create;
  FOId := TScOId.Create;
end;

destructor TScPKCS7Attribute.Destroy;
begin
  ClearValues;
  FOId.Free;
  FValues.Free;

  inherited;
end;

procedure TScPKCS7Attribute.Parse(LexemInfo: TScLexemInfo);
var
  ValuesLexem, ValueLexem: TScLexemInfo;
  i: integer;
begin
  ClearValues;
  FOId.Value := LexemInfo['Type'].AsOID;
  ValuesLexem := LexemInfo['Values'];

  for i := 0 to ValuesLexem.ValueCount - 1 do begin
    ValueLexem := ValuesLexem.Values[i]['Value'];
    AddValue(TScASN1Attribute.Create(FOId.Value, ValueLexem.AsBytes, ValueLexem.DataType));
  end;
end;

function TScPKCS7Attribute.Encode: TBytes;
var
  ASN1Compiler: TScASN1Compiler;
  ValuesLexem, ValueLexem: TScLexemInfo;
  i: integer;
begin
  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_ATTRIBUTE_DESC) then
      raise EScError.Create(seWrongDataFormat);

    ASN1Compiler['Type'].AsOID := FOId.Value;

    ValuesLexem := ASN1Compiler['Values'];
    ValuesLexem.ValueCount := FValues.Count;

    for i := 0 to FValues.Count - 1 do begin
      ValueLexem := ValuesLexem.Values[i]['Value'];
      ValueLexem.DataType := FValues[i].ASN1DataType;
      ValueLexem.AsBytes := FValues[i].RawData;
    end;

    Result := ASN1Compiler.Build;
  finally
    ASN1Compiler.Free;
  end;
end;

function TScPKCS7Attribute.Clone: TScPersistent;
begin
  Result := TScPKCS7Attribute.Create;
  Result.Assign(Self);
end;

procedure TScPKCS7Attribute.Assign(Source: TScPersistent);
var
  Src: TScPKCS7Attribute;
  i: integer;
begin
  if (Source = nil) or not IsClass(Source, TScPKCS7Attribute) then
    RaiseAssignError(Source);

  Src := TScPKCS7Attribute(Source);
  FOId.Assign(Src.FOId);

  FValues.Clear;
  for i := 0 to Src.FValues.Count - 1 do
    FValues.Add(Src.FValues[i].Clone);
end;

procedure TScPKCS7Attribute.AddValue(Item: TScASN1Attribute);
begin
  if Item = nil then
    raise EScError.Create(seInvalidInputArgs);

  FValues.Add(Item);
end;

procedure TScPKCS7Attribute.AddValue(const Value: TBytes; ASN1DataType: TScASN1DataType);
begin
  AddValue(TScASN1Attribute.Create(FOId.Value, Value, ASN1DataType));
end;

procedure TScPKCS7Attribute.AddValue(const Value: string; ASN1DataType: TScASN1DataType);
begin
  AddValue(TScASN1Attribute.Create(FOId.Value, Value, ASN1DataType));
end;

procedure TScPKCS7Attribute.ClearValues;
begin
  FValues.Clear;
end;

procedure TScPKCS7Attribute.DeleteValue(Index: integer);
begin
  FValues.Delete(Index);
end;

function TScPKCS7Attribute.GetValueCount: integer;
begin
  Result := FValues.Count;
end;

function TScPKCS7Attribute.GetValue(Index: integer): TScASN1Attribute;
begin
  Result := FValues[Index];
end;

procedure TScPKCS7Attribute.SetValue(Index: integer; Value: TScASN1Attribute);
begin
  if Value = nil then
    raise EScError.Create(seInvalidInputArgs);

  FValues[Index] := Value;
end;

procedure TScPKCS7Attribute.SetOId(Value: TScOId);
begin
  FOId.Assign(Value);
end;

{ TScPKCS7Attributes }

function TScPKCS7Attributes.GetItemClassType: TScPersistentClass;
begin
  Result := TScPKCS7Attribute;
end;

function TScPKCS7Attributes.GetASN1Description: TScASN1Description;
begin
  Result := asn1_ATTRIBUTES_DESC;
end;

procedure TScPKCS7Attributes.Parse(LexemInfo: TScLexemInfo);
var
  Attribute: TScPKCS7Attribute;
  i: integer;
begin
  Clear;

  for i := 0 to LexemInfo.ValueCount - 1 do begin
    Attribute := TScPKCS7Attribute.Create;
    Add(Attribute);
    Attribute.Parse(LexemInfo.Values[i]);
  end;
end;

function TScPKCS7Attributes.Encode: TBytes;
var
  ASN1Compiler: TScASN1Compiler;
  AttrsLexem: TScLexemInfo;
  i: integer;
begin
  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(GetASN1Description) then
      raise EScError.Create(seWrongDataFormat);

    AttrsLexem := ASN1Compiler.Root;
    AttrsLexem.ValueCount := Count;

    for i := 0 to Count - 1 do
      AttrsLexem.Values[i].AsBytes := Attributes[i].Encode;

    Result := ASN1Compiler.Build;
  finally
    ASN1Compiler.Free;
  end;
end;

function TScPKCS7Attributes.GetAttribute(Index: integer): TScPKCS7Attribute;
begin
  Result := TObject(Items[Index]) as TScPKCS7Attribute;
end;

procedure TScPKCS7Attributes.SetAttribute(Index: integer; Item: TScPKCS7Attribute);
begin
  if Item = nil then
    raise EScError.Create(seInvalidInputArgs);

  Items[Index] := Item;
end;

{ TScRelativeDistinguishedName }

constructor TScRelativeDistinguishedName.Create;
begin
  inherited;
  FList := TScASN1Attributes.Create;
end;

destructor TScRelativeDistinguishedName.Destroy;
begin
  FList.Free;
  inherited;
end;

function TScRelativeDistinguishedName.Clone: TScPersistent;
begin
  Result := TScRelativeDistinguishedName.Create;
  Result.Assign(Self);
end;

procedure TScRelativeDistinguishedName.Assign(Source: TScPersistent);
var
  Src: TScRelativeDistinguishedName;
  DstAttr, SrcAttr: TScASN1Attribute;
  i: integer;
begin
  if (Source = nil) or not IsClass(Source, TScRelativeDistinguishedName) then
    RaiseAssignError(Source);

  if Self = Source then
    Exit;

  FList.Clear;

  Src := TScRelativeDistinguishedName(Source);
  for i := 0 to Src.FList.Count - 1 do begin
    SrcAttr := Src.FList[i];
    DstAttr := TScASN1Attribute.Create(SrcAttr.OId.Value, SrcAttr.RawData, SrcAttr.ASN1DataType);
    FList.Add(DstAttr);
  end;
end;

procedure TScRelativeDistinguishedName.Parse(LexemInfo: TScLexemInfo);
var
  AttrValueLexem: TScLexemInfo;
  Attr: TScASN1Attribute;
  OId: string;
  i: integer;
begin
  FList.Clear;

  if LexemInfo = nil then
    Exit;

  // RDN_FORMAT
  for i := 0 to LexemInfo.ValueCount - 1 do begin
    OId := LexemInfo.Values[i]['AttrType'].AsOID;
    AttrValueLexem := LexemInfo.Values[i]['AttrValue'];
    Attr := TScASN1Attribute.Create(OId, AttrValueLexem.AsBytes, AttrValueLexem.DataType);
    FList.Add(Attr);
  end;
end;

function TScRelativeDistinguishedName.Encode: TBytes;
var
  ASN1Compiler: TScASN1Compiler;
  i: integer;
begin
  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_RDN_FORMAT) then
      raise EScError.Create(seWrongDataFormat);

    ASN1Compiler.Root.ValueCount := FList.Count;

    for i := 0 to FList.Count - 1 do
      ASN1Compiler.Root.Values[i]['Attr'].AsBytes := FList[i].Encode;

    Result := ASN1Compiler.Build;
  finally
    ASN1Compiler.Free;
  end;
end;

procedure TScRelativeDistinguishedName.Add(ASN1Attribute: TScASN1Attribute);
begin
  Flist.Add(ASN1Attribute);
end;

procedure TScRelativeDistinguishedName.Add(const OId, Value: string;
  DataType: TScASN1DataType = dtPrintableString);
var
  ASN1Attribute: TScASN1Attribute;
begin
  ASN1Attribute := TScASN1Attribute.Create(OId, Value, DataType);
  Flist.Add(ASN1Attribute);
end;

procedure TScRelativeDistinguishedName.Clear;
begin
  Flist.Clear;
end;

function TScRelativeDistinguishedName.IndexOfOId(const OId: string): integer;
var
  NameAttribute: TScASN1Attribute;
  i: integer;
begin
  for i := 0 to FList.Count - 1 do begin
    NameAttribute := FList.Attributes[i];
    if SameText(NameAttribute.OId.Value, OId) or SameText(NameAttribute.OId.FriendlyName, OId) then begin
      Result := i;
      Exit;
    end;
  end;

  Result := -1;
end;

procedure TScRelativeDistinguishedName.Remove(const OId: string);
var
  Index: integer;
begin
  Index := IndexOfOId(OId);
  if Index >= 0 then
    FList.Delete(Index);
end;

procedure TScRelativeDistinguishedName.RemoveAt(const Index: integer);
begin
  FList.Delete(Index);
end;

function TScRelativeDistinguishedName.GetItem(Index: integer): TScASN1Attribute;
begin
  Result := FList.Attributes[Index];
end;

function TScRelativeDistinguishedName.GetName(Index: integer): string;
begin
  Result := FList.Attributes[Index].OId.FriendlyName;
end;

function TScRelativeDistinguishedName.GetValueFromIndex(Index: integer): string;
begin
  Result := FList.Attributes[Index].AsString;
end;

function TScRelativeDistinguishedName.GetValue(const OId: string): string;
var
  Index: integer;
begin
  Index := IndexOfOId(OId);
  if Index >= 0 then
    Result := FList.Attributes[Index].AsString
  else
    raise EScError.CreateFmt(SDistinguishedNameNotFound, [OId], seDistinguishedNameNotFound);
end;

function TScRelativeDistinguishedName.GetCount: integer;
begin
  Result := FList.Count;
end;

function TScRelativeDistinguishedName.Equals(Value: TScRelativeDistinguishedName): boolean;
var
  Attr1, Attr2: TScASN1Attribute;
  i, j: integer;
begin
  Result := (Value <> nil) and (FList.Count = Value.FList.Count);
  if not Result then
    Exit;

  for i := 0 to FList.Count - 1 do begin
    Attr1 := FList.Attributes[i];

    j := 0;
    while j < Value.FList.Count do begin
      Attr2 := Value.FList.Attributes[j];
      if Attr1.OId.Value = Attr2.OId.Value then begin
        if not Attr1.Equals(Attr2) then begin
          Result := False;
          Exit;
        end;
        break;
      end;
      Inc(j);
    end;

    if j = Value.FList.Count then begin // OId not found
      Result := False;
      Exit;
    end;
  end;
end;

function TScRelativeDistinguishedName.ToString: string;
var
  NameAttribute: TScASN1Attribute;
  i: integer;
begin
  Result := '';

  for i := 0 to FList.Count - 1 do begin
    if i > 0 then
      Result := Result + ', ';

    NameAttribute := FList.Attributes[i];
    Result := Result + NameAttribute.OId.FriendlyName + '=' + NameAttribute.AsString;
  end;
end;

{ TScDistinguishedName }

constructor TScDistinguishedName.Create;
begin
  inherited;
  FList := TCRObjectList.Create;
end;

destructor TScDistinguishedName.Destroy;
begin
  FList.Free;
  inherited;
end;

function TScDistinguishedName.Clone: TScPersistent;
begin
  Result := TScDistinguishedName.Create;
  Result.Assign(Self);
end;

procedure TScDistinguishedName.Assign(Source: TScPersistent);
var
  RDN: TScRelativeDistinguishedName;
  i: integer;
begin
  if (Source = nil) or not IsClass(Source, TScDistinguishedName) then
    RaiseAssignError(Source);

  if Self = Source then
    Exit;

  FList.Clear;

  for i := 0 to TScDistinguishedName(Source).FList.Count - 1 do begin
    RDN := TScRelativeDistinguishedName.Create;
    FList.Add(RDN);
    RDN.Assign(TScRelativeDistinguishedName(TScDistinguishedName(Source).FList[i]));
  end;
end;

procedure TScDistinguishedName.Parse(LexemInfo: TScLexemInfo);
var
  AttrsLexem: TScLexemInfo;
  RDN: TScRelativeDistinguishedName;
  i: integer;
begin
  FList.Clear;

  if LexemInfo = nil then
    Exit;

  // DISTINGUISHED_NAME_FORMAT
  for i := 0 to LexemInfo.ValueCount - 1 do begin
    AttrsLexem := LexemInfo.Values[i]['Attrs'];

    RDN := TScRelativeDistinguishedName.Create;
    FList.Add(RDN);
    RDN.Parse(AttrsLexem);
  end;
end;

procedure TScDistinguishedName.Decode(const Buffer: TBytes; Offset, Size: integer);
var
  ASN1Compiler: TScASN1Compiler;
begin
  ASN1Compiler := TScASN1Compiler.Create;
  try
    if ASN1Compiler.Parse(asn1_DISTINGUISHED_NAMES, Buffer, Offset, Size) then
      Parse(ASN1Compiler.ValueByName['Names']);
  finally
    ASN1Compiler.Free;
  end;
end;

function TScDistinguishedName.Encode: TBytes;
var
  ASN1Compiler: TScASN1Compiler;
  i: integer;
begin
  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_DISTINGUISHED_NAME_FORMAT) then
      raise EScError.Create(seWrongDataFormat);

    ASN1Compiler.Root.ValueCount := FList.Count;

    for i := 0 to FList.Count - 1 do
      ASN1Compiler.Root.Values[i].EncodedData := TScRelativeDistinguishedName(FList[i]).Encode;

    Result := ASN1Compiler.Build;
  finally
    ASN1Compiler.Free;
  end;
end;

procedure TScDistinguishedName.Add(RelativeDistinguishedName: TScRelativeDistinguishedName);
begin
  FList.Add(RelativeDistinguishedName);
end;

procedure TScDistinguishedName.Add(const OId, Value: string;
  DataType: TScASN1DataType = dtPrintableString);
var
  RelativeDistinguishedName: TScRelativeDistinguishedName;
begin
  RelativeDistinguishedName := TScRelativeDistinguishedName.Create;
  FList.Add(RelativeDistinguishedName);
  RelativeDistinguishedName.Add(OId, Value, DataType);
end;

procedure TScDistinguishedName.Clear;
begin
  FList.Clear;
end;

procedure TScDistinguishedName.RemoveAt(const Index: integer);
begin
  FList.Delete(Index);
end;

function TScDistinguishedName.GetItem(Index: integer): TScRelativeDistinguishedName;
begin
  Result := TScRelativeDistinguishedName(FList.Items[Index]);
end;

function TScDistinguishedName.GetCount: integer;
begin
  Result := FList.Count;
end;

function TScDistinguishedName.IndexOfOId(const OId: string): integer;
var
  RDN: TScRelativeDistinguishedName;
  Count, Idx: integer;
  i: integer;
begin
  Count := 0;
  for i := 0 to FList.Count - 1 do begin
    RDN := TScRelativeDistinguishedName(FList.Items[i]);
    Idx := RDN.IndexOfOId(OId);
    if Idx >= 0 then begin
      Result := Count + Idx;
      Exit;
    end;
    Count := Count + RDN.Count;
  end;

  Result := -1;
end;

function TScDistinguishedName.GetName(Index: integer): string;
var
  RDN: TScRelativeDistinguishedName;
  i: integer;
begin
  for i := 0 to FList.Count - 1 do begin
    RDN := TScRelativeDistinguishedName(FList.Items[i]);
    if RDN.Count > Index then begin
      Result := RDN.Names[Index];
      Exit;
    end;

    Index := Index - RDN.Count;
  end;

  raise Exception.CreateFmt(SListIndexError, [Index]);
end;

function TScDistinguishedName.GetValueFromIndex(Index: integer): string;
var
  RDN: TScRelativeDistinguishedName;
  i: integer;
begin
  for i := 0 to FList.Count - 1 do begin
    RDN := TScRelativeDistinguishedName(FList.Items[i]);
    if RDN.Count > Index then begin
      Result := RDN.ValueFromIndex[Index];
      Exit;
    end;

    Index := Index - RDN.Count;
  end;

  raise Exception.CreateFmt(SListIndexError, [Index]);
end;

function TScDistinguishedName.GetValue(const OId: string): string;
var
  RDN: TScRelativeDistinguishedName;
  Idx: integer;
  i: integer;
begin
  for i := 0 to FList.Count - 1 do begin
    RDN := TScRelativeDistinguishedName(FList.Items[i]);
    Idx := RDN.IndexOfOId(OId);
    if Idx >= 0 then begin
      Result := RDN.ValueFromIndex[Idx];
      Exit;
    end;
  end;

  raise EScError.CreateFmt(SDistinguishedNameNotFound, [OId], seDistinguishedNameNotFound);
end;

function TScDistinguishedName.GetValueCount: integer;
var
  RDN: TScRelativeDistinguishedName;
  i: integer;
begin
  Result := 0;
  for i := 0 to FList.Count - 1 do begin
    RDN := TScRelativeDistinguishedName(FList.Items[i]);
    Result := Result + RDN.Count;
  end;
end;

function TScDistinguishedName.Equals(Value: TScDistinguishedName): boolean;
var
  RDN1, RDN2: TScRelativeDistinguishedName;
  i: integer;
begin
  Result := (Value <> nil) and (FList.Count = Value.FList.Count);
  if not Result then
    Exit;

  for i := 0 to FList.Count - 1 do begin
    RDN1 := TScRelativeDistinguishedName(FList.Items[i]);
    RDN2 := TScRelativeDistinguishedName(Value.FList.Items[i]);
    if not RDN1.Equals(RDN2) then begin
      Result := False;
      Exit;
    end;
  end;
end;

function TScDistinguishedName.ToString: string;
var
  RDN: TScRelativeDistinguishedName;
  i: integer;
begin
  Result := '';

  for i := 0 to FList.Count - 1 do begin
    if i > 0 then
      Result := Result + '; ';

    RDN := TScRelativeDistinguishedName(FList.Items[i]);
    Result := Result + RDN.ToString;
  end;
end;

{ TScDistinguishedNameList }

function TScDistinguishedNameList.GetItemClassType: TScPersistentClass;
begin
  Result := TScDistinguishedName;
end;

function TScDistinguishedNameList.GetName(Index: integer): TScDistinguishedName;
begin
  Result := TObject(Items[Index]) as TScDistinguishedName;
end;

procedure TScDistinguishedNameList.SetName(Index: integer; Item: TScDistinguishedName);
begin
  if Item = nil then
    raise EScError.Create(seInvalidInputArgs);

  Items[Index] := Item;
end;

{ TScGeneralName }

constructor TScGeneralName.Create;
begin
  inherited;
  FDirectoryName := TScDistinguishedName.Create;
end;

destructor TScGeneralName.Destroy;
begin
  FDirectoryName.Free;
  inherited;
end;

function TScGeneralName.Clone: TScPersistent;
begin
  Result := TScGeneralName.Create;
  Result.Assign(Self);
end;

procedure TScGeneralName.Assign(Source: TScPersistent);
begin
  if (Source = nil) or not IsClass(Source, TScGeneralName) then
    RaiseAssignError(Source);

  FName := TScGeneralName(Source).FName;
  FValue := TScGeneralName(Source).FValue;
  FDirectoryName.Assign(TScGeneralName(Source).FDirectoryName);
end;

function TScGeneralName.Equals(GName: TScGeneralName): boolean;
begin
  if GName = nil then
    Result := False
  else
  if (FName = 'DirectoryName') and (GName.FName = 'DirectoryName') then
    Result := FDirectoryName.Equals(GName.FDirectoryName)
  else
    Result := SameText(FValue, GName.FValue);
end;

function TScGeneralName.Equals(DName: TScDistinguishedName): boolean;
begin
  Result := (DName <> nil) and FDirectoryName.Equals(DName);
end;

procedure TScGeneralName.Parse(LexemInfo: TScLexemInfo);
begin
  // GENERAL_NAME_DESC
  FName := LexemInfo.Name;

  if SameText(FName, 'OtherName') then
    FValue := LexemInfo['TypeId'].AsString + '=' + LexemInfo['Value'].AsString
  else
  if SameText(FName, 'DirectoryName') then begin
    FDirectoryName.Parse(LexemInfo);
    FValue := FDirectoryName.ToString;
  end
  else
  if SameText(FName, 'EdiPartyName') then
    FValue := LexemInfo['NameAssigner'].AsString + '=' + LexemInfo['PartyName'].AsString
  else
    FValue := LexemInfo.AsString;
end;

function TScGeneralName.ToString: string;
begin
  if Name = '' then
    Result := FValue
  else
  if Name = 'DirectoryName' then
    Result := 'Directory Address: ' + FValue
  else
  if (Name = 'OtherName') or (Name = 'EdiPartyName') or (Name = 'Relative Name') then
    Result := FName + ': ' + FValue
  else
    Result := FName + '=' + FValue;
end;

procedure TScGeneralName.SetDirectoryName(Value: TScDistinguishedName);
begin
  FDirectoryName.Assign(Value);
end;

{ TScGeneralNames }

function TScGeneralNames.GetItemClassType: TScPersistentClass;
begin
  Result := TScGeneralName;
end;

function TScGeneralNames.GetGeneralName(Index: integer): TScGeneralName;
begin
  Result := TObject(Items[Index]) as TScGeneralName;
end;

procedure TScGeneralNames.SetGeneralName(Index: integer; Item: TScGeneralName);
begin
  if Item = nil then
    raise EScError.Create(seInvalidInputArgs);

  Items[Index] := Item;
end;

function TScGeneralNames.FindByName(const AName: string): TScGeneralName;
var
  i: integer;
begin
  for i := 0 to Count - 1 do begin
    if SameText(TScGeneralName(Items[i]).Name, AName) then begin
      Result := TScGeneralName(Items[i]);
      Exit;
    end;
  end;

  Result := nil;
end;

function TScGeneralNames.Equals(Value: TScGeneralNames): boolean;
var
  GName1, GName2: TScGeneralName;
  i: integer;
begin
  Result := (Value <> nil) and (Count = Value.Count);
  if not Result then
    Exit;

  for i := 0 to Count - 1 do begin
    GName1 := TScGeneralName(GeneralNames[i]);
    GName2 := TScGeneralName(Value.GeneralNames[i]);
    if not GName1.Equals(GName2) then begin
      Result := False;
      Exit;
    end;
  end;
end;

function TScGeneralNames.HasEqual(Value: TScDistinguishedName): boolean;
var
  i: integer;
begin
  Result := False;
  if Value = nil then
    Exit;

  for i := 0 to Count - 1 do begin
    if GeneralNames[i].Equals(Value) then begin
      Result := True;
      Exit;
    end;
  end;
end;

procedure TScGeneralNames.Parse(LexemInfo: TScLexemInfo);
var
  GeneralName: TScGeneralName;
  i: integer;
begin
  if LexemInfo.IsNull then
    Exit;

  for i := 0 to LexemInfo.ValueCount - 1 do begin
    GeneralName := TScGeneralName.Create;
    Add(GeneralName);
    GeneralName.Parse(LexemInfo.Values[i]['Name']);
  end;
end;

function TScGeneralNames.ToString: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to Count - 1 do begin
    if i > 0 then
      Result := Result + '; ';

    Result := Result + GeneralNames[i].ToString;
  end;
end;

{ TScCertificateExtension }

constructor TScCertificateExtension.Create(const OId: string; ACritical: boolean; const DERValue: TBytes);
begin
  inherited Create;

  FOId := TScOId.Create(OId);
  FCritical := ACritical;
  FRawData := DERValue;

  Parse(DERValue);
end;

destructor TScCertificateExtension.Destroy;
begin
  FOId.Free;

  inherited;
end;

function TScCertificateExtension.Clone: TScPersistent;
begin
  Result := TScCertificateExtension.Create(FOId.Value, FCritical, FRawData);
end;

procedure TScCertificateExtension.Assign(Source: TScPersistent);
begin
  if (Source = nil) or not IsClass(Source, TScCertificateExtension) then
    RaiseAssignError(Source);

  FOId.Assign(TScCertificateExtension(Source).FOId);
  FCritical := TScCertificateExtension(Source).FCritical;
  FRawData := TScCertificateExtension(Source).FRawData;

  Parse(FRawData);
end;

procedure TScCertificateExtension.Parse(const DERValue: TBytes);
begin
  // none
end;

function TScCertificateExtension.ToString: string;
begin
  Result := BytesToHexStr(FRawData);
end;

{ TScCertBasicConstraintsExtension }

constructor TScCertBasicConstraintsExtension.Create(const OId: string; ACritical: boolean; const DERValue: TBytes);
begin
  inherited; // for C++ Builder
end;

function TScCertBasicConstraintsExtension.Clone: TScPersistent;
begin
  Result := TScCertBasicConstraintsExtension.Create(FOId.Value, FCritical, FRawData);
end;

procedure TScCertBasicConstraintsExtension.Parse(const DERValue: TBytes);
const
  CERT_CA_SUBJECT_FLAG = $80;

var
  ASN1Compiler: TScASN1Compiler;
  InfoLexem: TScLexemInfo;
  SubjectType: TBytes;
begin
  SetLength(SubjectType, 0);

  ASN1Compiler := TScASN1Compiler.Create;
  try
    if FOId.Value = OID_CE_BASIC_CONSTRAINTS then begin
      if not ASN1Compiler.Parse(asn1_BASIC_CONSTRAINTS_EXTENSION_DESC, DERValue) then
        raise EScError.Create(seWrongExtensionData);

      InfoLexem := ASN1Compiler['Info'];

      SubjectType := InfoLexem['SubjectType'].AsBytes;
      if Length(SubjectType) > 0 then
        FCertificateAuthority := (SubjectType[0] and CERT_CA_SUBJECT_FLAG) <> 0
      else
        FCertificateAuthority := False;
      FHasPathLengthConstraint := not InfoLexem['Len'].IsNull;
      FPathLengthConstraint := InfoLexem['Len'].AsInteger;
    end
    else begin // OID_BASIC_CONSTRAINTS2
      if not ASN1Compiler.Parse(asn1_BASIC_CONSTRAINTS2_EXTENSION_DESC, DERValue) then
        raise EScError.Create(seWrongExtensionData);

      InfoLexem := ASN1Compiler['Info'];
      FCertificateAuthority := InfoLexem['CA'].AsBoolean;
      FHasPathLengthConstraint := not InfoLexem['Len'].IsNull;
      FPathLengthConstraint := InfoLexem['Len'].AsInteger;
    end;
  finally
    ASN1Compiler.Free;
  end;
end;

function TScCertBasicConstraintsExtension.ToString: string;
begin
  Result := 'Subject Type=';
  if FCertificateAuthority then
    Result := Result + 'CA'
  else
    Result := Result + 'End Entity';

  Result := Result + '; Path Length Constraint=';
  if not FHasPathLengthConstraint then
    Result := Result + 'None'
  else
    Result := Result + IntToStr(FPathLengthConstraint);
end;

{ TScCertKeyUsageExtension }

constructor TScCertKeyUsageExtension.Create(const OId: string; ACritical: boolean; const DERValue: TBytes);
begin
  inherited; // for C++ Builder
end;

function TScCertKeyUsageExtension.Clone: TScPersistent;
begin
  Result := TScCertKeyUsageExtension.Create(FOId.Value, FCritical, FRawData);
end;

procedure TScCertKeyUsageExtension.Parse(const DERValue: TBytes);
const
  CERT_DIGITAL_SIGNATURE_KEY_USAGE = $80; // 0 bit
  CERT_NON_REPUDIATION_KEY_USAGE   = $40; // 1st bit
  CERT_KEY_ENCIPHERMENT_KEY_USAGE  = $20; // 2 bit
  CERT_DATA_ENCIPHERMENT_KEY_USAGE = $10; // 3 bit
  CERT_KEY_AGREEMENT_KEY_USAGE     = $08; // 4 bit
  CERT_KEY_CERT_SIGN_KEY_USAGE     = $04; // 5 bit
  CERT_CRL_SIGN_KEY_USAGE          = $02; // 6 bit
  CERT_ENCIPHER_ONLY_KEY_USAGE     = $01; // 7 bit
  CERT_DECIPHER_ONLY_KEY_USAGE     = $80; // 8 bit

var
  ASN1Compiler: TScASN1Compiler;
  Buf: TBytes;
begin
  FKeyUsages := [];
  SetLength(Buf, 0);

  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_KEY_USAGE_EXTENSION_DESC, DERValue) then
      raise EScError.Create(seWrongExtensionData);

    Buf := ASN1Compiler['Data'].AsBytes;
    if Length(Buf) > 0 then begin
      if (Buf[0] and CERT_DIGITAL_SIGNATURE_KEY_USAGE) > 0 then
        Include(FKeyUsages, kfDigitalSignature);
      if (Buf[0] and CERT_NON_REPUDIATION_KEY_USAGE) > 0 then
        Include(FKeyUsages, kfNonRepudiation);
      if (Buf[0] and CERT_KEY_ENCIPHERMENT_KEY_USAGE) > 0 then
        Include(FKeyUsages, kfKeyEncipherment);
      if (Buf[0] and CERT_DATA_ENCIPHERMENT_KEY_USAGE) > 0 then
        Include(FKeyUsages, kfDataEncipherment);
      if (Buf[0] and CERT_KEY_AGREEMENT_KEY_USAGE) > 0 then
        Include(FKeyUsages, kfKeyAgreement);
      if (Buf[0] and CERT_KEY_CERT_SIGN_KEY_USAGE) > 0 then
        Include(FKeyUsages, kfKeyCertSign);
      if (Buf[0] and CERT_CRL_SIGN_KEY_USAGE) > 0 then
        Include(FKeyUsages, kfCrlSign);
      if (Buf[0] and CERT_ENCIPHER_ONLY_KEY_USAGE) > 0 then
        Include(FKeyUsages, kfEncipherOnly);

      if (Length(Buf) > 1) and ((Buf[1] and CERT_DECIPHER_ONLY_KEY_USAGE) > 0) then
        FKeyUsages := FKeyUsages + [kfDecipherOnly];
    end;
  finally
    ASN1Compiler.Free;
  end;
end;

function TScCertKeyUsageExtension.ToString: string;
const
  KEY_USAGE_FLAGS: array[TScKeyUsageFlag] of string = (
    'Digital Signature',
    'Non-Repudiation',
    'Key Encipherment',
    'Data Encipherment',
    'Key Agreement',
    'Certificate Signing',
    'CRL Signing',
    'Encipher Only',
    'Decipher Only'
  );

var
  KeyUsageFlag: TScKeyUsageFlag;
begin
  Result := '';

  for KeyUsageFlag := Low(TScKeyUsageFlag) to High(TScKeyUsageFlag) do begin
    if KeyUsageFlag in FKeyUsages then begin
      if Result <> '' then
        Result := Result + ', ';

      Result := Result + KEY_USAGE_FLAGS[KeyUsageFlag];
    end;
  end;
end;

{ TScCertExtendedKeyUsageExtension }

constructor TScCertExtendedKeyUsageExtension.Create(const OId: string; ACritical: boolean; const DERValue: TBytes);
begin
  inherited; // for C++ Builder
end;

function TScCertExtendedKeyUsageExtension.Clone: TScPersistent;
begin
  Result := TScCertExtendedKeyUsageExtension.Create(FOId.Value, FCritical, FRawData);
end;

destructor TScCertExtendedKeyUsageExtension.Destroy;
begin
  FExtendedKeyUsages.Free;
  inherited;
end;

procedure TScCertExtendedKeyUsageExtension.Parse(const DERValue: TBytes);
var
  ASN1Compiler: TScASN1Compiler;
  ExtKeyUsageLexem: TScLexemInfo;
  OId: TScOId;
  i: integer;
begin
  if FExtendedKeyUsages = nil then
    FExtendedKeyUsages := TScOIds.Create
  else
    FExtendedKeyUsages.Clear;

  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_EXTENDED_KEY_USAGE_EXTENSION_DESC, DERValue) then
      raise EScError.Create(seWrongExtensionData);

    ExtKeyUsageLexem := ASN1Compiler['Usage'];
    for i := 0 to ExtKeyUsageLexem.ValueCount - 1 do begin
      OId := TScOId.Create(ExtKeyUsageLexem.Values[i]['ID'].AsOID);
      FExtendedKeyUsages.Add(OId);
    end;
  finally
    ASN1Compiler.Free;
  end;
end;

function TScCertExtendedKeyUsageExtension.ToString: string;
var
  i: integer;
begin
  Result := '';
  if FExtendedKeyUsages = nil then
    Exit;

  for i := 0 to FExtendedKeyUsages.Count - 1 do begin
    if i > 0 then
      Result := Result + ', ';

    Result := Result + FExtendedKeyUsages.OIds[i].FriendlyName + ' (' + FExtendedKeyUsages.OIds[i].Value + ')';
  end;
end;

{ TScCertSubjectKeyIdExtension }

constructor TScCertSubjectKeyIdExtension.Create(const OId: string; ACritical: boolean; const DERValue: TBytes);
begin
  inherited; // for C++ Builder
end;

function TScCertSubjectKeyIdExtension.Clone: TScPersistent;
begin
  Result := TScCertSubjectKeyIdExtension.Create(FOId.Value, FCritical, FRawData);
end;

procedure TScCertSubjectKeyIdExtension.Parse(const DERValue: TBytes);
var
  ASN1Compiler: TScASN1Compiler;
begin
  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_SUBJECT_KEY_ID_EXTENSION_DESC, DERValue) then
      raise EScError.Create(seWrongExtensionData);

    FSubjectKeyIdentifier := BytesToHexStr(ASN1Compiler['Data'].AsBytes);
  finally
    ASN1Compiler.Free;
  end;
end;

function TScCertSubjectKeyIdExtension.ToString: string;
begin
  Result := FSubjectKeyIdentifier;
end;

{ TScCertAuthorityKeyIdExtension }

constructor TScCertAuthorityKeyIdExtension.Create(const OId: string; ACritical: boolean; const DERValue: TBytes);
begin
  inherited; // for C++ Builder
end;

function TScCertAuthorityKeyIdExtension.Clone: TScPersistent;
begin
  Result := TScCertAuthorityKeyIdExtension.Create(FOId.Value, FCritical, FRawData);
end;

destructor TScCertAuthorityKeyIdExtension.Destroy;
begin
  FCertIssuers.Free;
  inherited;
end;

procedure TScCertAuthorityKeyIdExtension.Parse(const DERValue: TBytes);
var
  Desc: TScASN1Description;
  ASN1Compiler: TScASN1Compiler;
  IssuerLexem: TScLexemInfo;
  GeneralName: TScGeneralName;
  i: integer;
begin
  if FCertIssuers = nil then
    FCertIssuers := TScGeneralNames.Create
  else
    FCertIssuers.Clear;

  ASN1Compiler := TScASN1Compiler.Create;
  try
    if FOId.FValue = OID_CE_AUTHORITY_KEY_IDENTIFIER then
      Desc := asn1_AUTHORITY_KEY_ID_EXTENSION_DESC
    else
      Desc := asn1_AUTHORITY_KEY_ID_EXTENSION_DESC2;

    if not ASN1Compiler.Parse(Desc, DERValue) then
      raise EScError.Create(seWrongExtensionData);

    FKeyIdentifier := BytesToHexStr(ASN1Compiler['KeyID'].AsBytes);
    FCertSerialNumber := ASN1Compiler['SerialNumber'].AsString;

    IssuerLexem := ASN1Compiler['Issuer'];

    if FOId.FValue = OID_CE_AUTHORITY_KEY_IDENTIFIER then
      FCertIssuers.Parse(IssuerLexem)
    else begin
      for i := 0 to IssuerLexem.ValueCount - 1 do begin
        GeneralName := TScGeneralName.Create;
        FCertIssuers.Add(GeneralName);
        GeneralName.DirectoryName.Parse(IssuerLexem.Values[i]['DirectoryName']);
        GeneralName.Value := GeneralName.DirectoryName.ToString;
      end;
    end;
  finally
    ASN1Compiler.Free;
  end;
end;

function TScCertAuthorityKeyIdExtension.ToString: string;
begin
  if FCertIssuers = nil then begin
    Result := '';
    Exit;
  end;

  Result := 'KeyID=' + FKeyIdentifier;

  if FCertIssuers.Count > 0 then begin
    Result := Result + '; '#13#10'Certificate Issuer: '#13#10;
    Result := Result + FCertIssuers.ToString;
  end;

  if FCertSerialNumber <> '' then
    Result := Result + '; '#13#10'Certificate SerialNumber=' + FCertSerialNumber;
end;

{ TScPolicy }

function TScPolicy.Clone: TScPersistent;
begin
  Result := TScPolicy.Create;
  Result.Assign(Self);
end;

procedure TScPolicy.Assign(Source: TScPersistent);
var
  i: integer;
begin
  if (Source = nil) or not IsClass(Source, TScPolicy) then
    RaiseAssignError(Source);

  Identifier := TScPolicy(Source).Identifier;

  SetLength(Qualifiers, Length(TScPolicy(Source).Qualifiers));
  for i := 0 to Length(TScPolicy(Source).Qualifiers) - 1 do
    Qualifiers[i] := TScPolicy(Source).Qualifiers[i];
end;

{ TScPolicyList }

function TScPolicyList.GetItemClassType: TScPersistentClass;
begin
  Result := TScPolicy;
end;

function TScPolicyList.GetPolicy(Index: integer): TScPolicy;
begin
  Result := TObject(Items[Index]) as TScPolicy;
end;

procedure TScPolicyList.SetPolicy(Index: integer; Item: TScPolicy);
begin
  if Item = nil then
    raise EScError.Create(seInvalidInputArgs);

  Items[Index] := Item;
end;

{ TScCertPoliciesExtension }

constructor TScCertPoliciesExtension.Create(const OId: string; ACritical: boolean; const DERValue: TBytes);
begin
  inherited; // for C++ Builder
end;

function TScCertPoliciesExtension.Clone: TScPersistent;
begin
  Result := TScCertPoliciesExtension.Create(FOId.Value, FCritical, FRawData);
end;

destructor TScCertPoliciesExtension.Destroy;
begin
  FPolicyList.Free;
  inherited;
end;

procedure TScCertPoliciesExtension.Parse(const DERValue: TBytes);
var
  ASN1Compiler: TScASN1Compiler;
  PoliciesLexem, PolicyLexem: TScLexemInfo;
  QualifiersLexem, QualifierLexem, QualifierValueLexem: TScLexemInfo;
  Policy: TScPolicy;
  i, j: integer;
begin
  if FPolicyList = nil then
    FPolicyList := TScPolicyList.Create
  else
    FPolicyList.Clear;

  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_CERTIFICATE_POLICIES_EXTENSION_DESC, DERValue) then
      raise EScError.Create(seWrongExtensionData);

    PoliciesLexem := ASN1Compiler['Policies'];
    for i := 0 to PoliciesLexem.ValueCount - 1 do begin
      Policy := TScPolicy.Create;
      FPolicyList.Add(Policy);

      PolicyLexem := PoliciesLexem.Values[i];
      Policy.Identifier := PolicyLexem['Identifier'].AsOID;

      QualifiersLexem := PolicyLexem['Qualifiers'];
      SetLength(Policy.Qualifiers, QualifiersLexem.ValueCount);

      for j := 0 to QualifiersLexem.ValueCount - 1 do begin
        QualifierLexem := QualifiersLexem.Values[j];
        Policy.Qualifiers[j].QualifierId := QualifierLexem['QualifierId'].AsOID;
        QualifierValueLexem := QualifierLexem['Qualifier'];

        if Policy.Qualifiers[j].QualifierId = OID_POLICY_QUALIFIER_CPS then begin
          if QualifierValueLexem.Name <> 'CPSuri' then
            raise EScError.Create(seWrongExtensionData);

          Policy.Qualifiers[j].CpsUri := QualifierValueLexem.AsString;
        end
        else
        if Policy.Qualifiers[j].QualifierId = OID_POLICY_QUALIFIER_UNOTICE then begin
          Policy.Qualifiers[j].NoticeReferenceOrganization := QualifierValueLexem['Organization'].AsString;
          Policy.Qualifiers[j].ExplicitText := QualifierValueLexem['ExplicitText'].AsString;
        end
        else
          raise EScError.Create(seWrongExtensionData);
      end;
    end;
  finally
    ASN1Compiler.Free;
  end;
end;

function TScCertPoliciesExtension.ToString: string;
var
  Policy: TScPolicy;
  OId: string;
  i, j: integer;
begin
  Result := '';
  if FPolicyList = nil then
    Exit;

  for i := 0 to FPolicyList.Count - 1 do begin
    if i > 0 then
      Result := Result + '; '#13#10;

    Policy := FPolicyList[i];
    Result := Result + '[' + IntToStr(i + 1) + ']Certificate Policy: '#13#10;
    OId := OIdToFriendlyName(Policy.Identifier);
    if OId = '' then
      OId := Policy.Identifier;
    Result := Result + 'Policy Identifier=' + OId;

    for j := 0 to Length(Policy.Qualifiers) - 1 do begin
      Result := Result + '; '#13#10;
      Result := Result + '[' + IntToStr(i + 1) + ',' + IntToStr(j + 1) + ']Policy Qualifier Info: '#13#10;

      OId := OIdToFriendlyName(Policy.Qualifiers[j].QualifierId);
      if OId = '' then
        OId := Policy.Qualifiers[j].QualifierId;
      Result := Result + 'Policy Qualifier Id=' + OId + '; ';

      Result := Result + 'Qualifier: ';
      if Policy.Qualifiers[j].QualifierId = OID_POLICY_QUALIFIER_CPS then
        Result := Result + Policy.Qualifiers[j].CpsUri
      else
      if Policy.Qualifiers[j].QualifierId = OID_POLICY_QUALIFIER_UNOTICE then begin
        if Policy.Qualifiers[j].NoticeReferenceOrganization <> '' then
          Result := Result + 'Organization=' + Policy.Qualifiers[j].NoticeReferenceOrganization + '; ';
        Result := Result + 'Explicit Text=' + Policy.Qualifiers[j].ExplicitText;
      end;
    end;
  end;
end;

{ TScPolicyMapping }

constructor TScPolicyMapping.Create;
begin
  inherited Create;

  FIssuerDomainPolicy := TScOId.Create;
  FSubjectDomainPolicy := TScOId.Create;
end;

constructor TScPolicyMapping.Create(const AIssuerDomainPolicy, ASubjectDomainPolicy: string);
begin
  inherited Create;

  FIssuerDomainPolicy := TScOId.Create(AIssuerDomainPolicy);
  FSubjectDomainPolicy := TScOId.Create(ASubjectDomainPolicy);
end;

destructor TScPolicyMapping.Destroy;
begin
  FIssuerDomainPolicy.Free;
  FSubjectDomainPolicy.Free;
  inherited;
end;

function TScPolicyMapping.Clone: TScPersistent;
begin
  Result := TScPolicyMapping.Create;
  Result.Assign(Self);
end;

procedure TScPolicyMapping.Assign(Source: TScPersistent);
begin
  if (Source = nil) or not IsClass(Source, TScPolicyMapping) then
    RaiseAssignError(Source);

  FIssuerDomainPolicy.Assign(TScPolicyMapping(Source).FIssuerDomainPolicy);
  FSubjectDomainPolicy.Assign(TScPolicyMapping(Source).FSubjectDomainPolicy);
end;

{ TScPolicyMappingList }

function TScPolicyMappingList.GetItemClassType: TScPersistentClass;
begin
  Result := TScPolicyMapping;
end;

function TScPolicyMappingList.GetPolicyMapping(Index: integer): TScPolicyMapping;
begin
  Result := TObject(Items[Index]) as TScPolicyMapping;
end;

procedure TScPolicyMappingList.SetPolicyMapping(Index: integer; Item: TScPolicyMapping);
begin
  if Item = nil then
    raise EScError.Create(seInvalidInputArgs);

  Items[Index] := Item;
end;

{ TScCertPolicyMappingsExtension }

constructor TScCertPolicyMappingsExtension.Create(const OId: string; ACritical: boolean; const DERValue: TBytes);
begin
  inherited; // for C++ Builder
end;

function TScCertPolicyMappingsExtension.Clone: TScPersistent;
begin
  Result := TScCertPolicyMappingsExtension.Create(FOId.Value, FCritical, FRawData);
end;

destructor TScCertPolicyMappingsExtension.Destroy;
begin
  FPolicyMappings.Free;
  inherited;
end;

procedure TScCertPolicyMappingsExtension.Parse(const DERValue: TBytes);
var
  ASN1Compiler: TScASN1Compiler;
  PolicyMappingsLexem, PolicyLexem: TScLexemInfo;
  PolicyMapping: TScPolicyMapping;
  i: integer;
begin
  if FPolicyMappings = nil then
    FPolicyMappings := TScPolicyMappingList.Create
  else
    FPolicyMappings.Clear;

  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_POLICY_MAPPINGS_EXTENSION_DESC, DERValue) then
      raise EScError.Create(seWrongExtensionData);

    PolicyMappingsLexem := ASN1Compiler['PolicyMappings'];

    for i := 0 to PolicyMappingsLexem.ValueCount - 1 do begin
      PolicyLexem := PolicyMappingsLexem.Values[i];
      PolicyMapping := TScPolicyMapping.Create(PolicyLexem['IssuerDomainPolicy'].AsOID, PolicyLexem['SubjectDomainPolicy'].AsOID);
      FPolicyMappings.Add(PolicyMapping);
    end;
  finally
    ASN1Compiler.Free;
  end;
end;

function TScCertPolicyMappingsExtension.ToString: string;
var
  PolicyMapping: TScPolicyMapping;
  i: integer;
begin
  Result := '';
  if FPolicyMappings = nil then
    Exit;

  for i := 0 to FPolicyMappings.Count - 1 do begin
    if i > 0 then
      Result := Result + '; '#13#10;

    PolicyMapping := FPolicyMappings[i];
    Result := Result + 'Issuer Domain Policy=' + PolicyMapping.IssuerDomainPolicy.FriendlyName;
    Result := Result + ', Subject Domain Policy=' + PolicyMapping.SubjectDomainPolicy.FriendlyName;
  end;
end;

{ TScCertAlternativeNameExtension }

constructor TScCertAlternativeNameExtension.Create(const OId: string; ACritical: boolean; const DERValue: TBytes);
begin
  inherited; // for C++ Builder
end;

function TScCertAlternativeNameExtension.Clone: TScPersistent;
begin
  Result := TScCertAlternativeNameExtension.Create(FOId.Value, FCritical, FRawData);
end;

destructor TScCertAlternativeNameExtension.Destroy;
begin
  FGeneralNames.Free;
  inherited;
end;

procedure TScCertAlternativeNameExtension.Parse(const DERValue: TBytes);
var
  ASN1Compiler: TScASN1Compiler;
  GeneralNamesLexem: TScLexemInfo;
begin
  if FGeneralNames = nil then
    FGeneralNames := TScGeneralNames.Create
  else
    FGeneralNames.Clear;

  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_ALTERNATIVE_NAME_EXTENSION_DESC, DERValue) then
      raise EScError.Create(seWrongExtensionData);

    GeneralNamesLexem := ASN1Compiler['GeneralNames'];
    FGeneralNames.Parse(GeneralNamesLexem);
  finally
    ASN1Compiler.Free;
  end;
end;

function TScCertAlternativeNameExtension.ToString: string;
begin
  if FGeneralNames = nil then
    Result := ''
  else
    Result := FGeneralNames.ToString;
end;

{ TScCertSubjectAlternativeNameExtension }

constructor TScCertSubjectAlternativeNameExtension.Create(const OId: string; ACritical: boolean; const DERValue: TBytes);
begin
  inherited; // for C++ Builder
end;

function TScCertSubjectAlternativeNameExtension.Clone: TScPersistent;
begin
  Result := TScCertSubjectAlternativeNameExtension.Create(FOId.Value, FCritical, FRawData);
end;

{ TScCertIssuerAlternativeNameExtension }

constructor TScCertIssuerAlternativeNameExtension.Create(const OId: string; ACritical: boolean; const DERValue: TBytes);
begin
  inherited; // for C++ Builder
end;

function TScCertIssuerAlternativeNameExtension.Clone: TScPersistent;
begin
  Result := TScCertIssuerAlternativeNameExtension.Create(FOId.Value, FCritical, FRawData);
end;

{ TScCertSubjectDirectoryAttributesExtension }

constructor TScCertSubjectDirectoryAttributesExtension.Create(const OId: string; ACritical: boolean; const DERValue: TBytes);
begin
  inherited; // for C++ Builder
end;

function TScCertSubjectDirectoryAttributesExtension.Clone: TScPersistent;
begin
  Result := TScCertSubjectDirectoryAttributesExtension.Create(FOId.Value, FCritical, FRawData);
end;

destructor TScCertSubjectDirectoryAttributesExtension.Destroy;
begin
  FDirectoryAttributes.Free;
  inherited;
end;

procedure TScCertSubjectDirectoryAttributesExtension.Parse(const DERValue: TBytes);
var
  ASN1Compiler: TScASN1Compiler;
  AttributesLexem: TScLexemInfo;
  Attribute: TScPKCS7Attribute;
  i: integer;
begin
  if FDirectoryAttributes = nil then
    FDirectoryAttributes := TScPKCS7Attributes.Create
  else
    FDirectoryAttributes.Clear;

  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_SUBJECT_DIRECTORY_ATTRIBUTES_EXTENSION_DESC, DERValue) then
      raise EScError.Create(seWrongExtensionData);

    AttributesLexem := ASN1Compiler['Attributes'];

    for i := 0 to AttributesLexem.ValueCount - 1 do begin
      Attribute := TScPKCS7Attribute.Create;
      FDirectoryAttributes.Add(Attribute);
      Attribute.Parse(AttributesLexem.Values[i]);
    end;
  finally
    ASN1Compiler.Free;
  end;
end;

function TScCertSubjectDirectoryAttributesExtension.ToString: string;
var
  Attribute: TScPKCS7Attribute;
  i, j: integer;
begin
  Result := '';
  if FDirectoryAttributes = nil then
    Exit;

  for i := 0 to FDirectoryAttributes.Count - 1 do begin
    if i > 0 then
      Result := Result + '; '#13#10;

    Attribute := FDirectoryAttributes[i];
    Result := Result + Attribute.OId.FriendlyName + ': ';

    for j := 0 to Attribute.ValueCount - 1 do begin
      if j > 0 then
        Result := Result + ', ';
      Result := Result + Attribute.Values[j].AsString;
    end;
  end;
end;

{ TScCRLDistributionPoint }

constructor TScCRLDistributionPoint.Create;
begin
  inherited Create;

  FDistributionPointName := TScGeneralNames.Create;
  FCRLIssuer := TScGeneralNames.Create;
end;

destructor TScCRLDistributionPoint.Destroy;
begin
  FDistributionPointName.Free;
  FCRLIssuer.Free;
  inherited;
end;

function TScCRLDistributionPoint.Clone: TScPersistent;
begin
  Result := TScCRLDistributionPoint.Create;
  Result.Assign(Self);
end;

procedure TScCRLDistributionPoint.Assign(Source: TScPersistent);
begin
  if (Source = nil) or not IsClass(Source, TScCRLDistributionPoint) then
    RaiseAssignError(Source);

  FDistributionPointName.Assign(TScCRLDistributionPoint(Source).FDistributionPointName);
  FCRLIssuer.Assign(TScCRLDistributionPoint(Source).FCRLIssuer);
  FReasons := TScCRLDistributionPoint(Source).FReasons;
end;

class procedure TScCRLDistributionPoint.ParseReasons(ReasonsLexem: TScLexemInfo; out Reasons: TScCRLReasons);
const
  CRL_KEY_COMPROMISE_REASON_FLAG         = $40; // 1st bit
  CRL_CA_COMPROMISE_REASON_FLAG          = $20; // 2 bit
  CRL_AFFILIATION_CHANGED_REASON_FLAG    = $10; // 3 bit
  CRL_SUPERSEDED_REASON_FLAG             = $08; // 4 bit
  CRL_CESSATION_OF_OPERATION_REASON_FLAG = $04; // 5 bit
  CRL_CERTIFICATE_HOLD_REASON_FLAG       = $02; // 6 bit
  CRL_PRIVILEGE_WITHDRAWN_REASON_FLAG    = $01; // 7 bit
  CRL_AA_COMPROMISE_REASON_FLAG          = $80; // 8 bit
var
  Buf: TBytes;
begin
  Reasons := [];
  if not ReasonsLexem.IsNull then begin
    SetLength(Buf, 0);
    Buf := ReasonsLexem.AsBytes;
    if Length(Buf) > 0 then begin
      if (Buf[0] and CRL_KEY_COMPROMISE_REASON_FLAG) > 0 then
        Include(Reasons, crKeyCompromise);
      if (Buf[0] and CRL_CA_COMPROMISE_REASON_FLAG) > 0 then
        Include(Reasons, crCACompromise);
      if (Buf[0] and CRL_AFFILIATION_CHANGED_REASON_FLAG) > 0 then
        Include(Reasons, crAffiliationChanged);
      if (Buf[0] and CRL_SUPERSEDED_REASON_FLAG) > 0 then
        Include(Reasons, crSuperseded);
      if (Buf[0] and CRL_CESSATION_OF_OPERATION_REASON_FLAG) > 0 then
        Include(Reasons, crCessationOfOperation);
      if (Buf[0] and CRL_CERTIFICATE_HOLD_REASON_FLAG) > 0 then
        Include(Reasons, crCertificateHold);
      if (Buf[0] and CRL_PRIVILEGE_WITHDRAWN_REASON_FLAG) > 0 then
        Include(Reasons, crPrivilegeWithdrawn);

      if (Length(Buf) > 1) and ((Buf[1] and CRL_AA_COMPROMISE_REASON_FLAG) > 0) then
        Include(Reasons, crAACompromise);
    end;
  end;
end;

class function TScCRLDistributionPoint.ReasonsToString(const Reasons: TScCRLReasons): string;
var
  ReasonFlag: TScCRLReason;
begin
  Result := '';
  for ReasonFlag := Low(TScCRLReason) to High(TScCRLReason) do begin
    if ReasonFlag in Reasons then begin
      if Result <> '' then
        Result := Result + ', ';

      Result := Result + CRL_REASONS[ReasonFlag];
    end;
  end;
end;

{ TScCRLDistributionPointList }

function TScCRLDistributionPointList.GetItemClassType: TScPersistentClass;
begin
  Result := TScCRLDistributionPoint;
end;

function TScCRLDistributionPointList.GetCRLDistributionPoint(Index: integer): TScCRLDistributionPoint;
begin
  Result := TObject(Items[Index]) as TScCRLDistributionPoint;
end;

procedure TScCRLDistributionPointList.SetCRLDistributionPoint(Index: integer; Item: TScCRLDistributionPoint);
begin
  if Item = nil then
    raise EScError.Create(seInvalidInputArgs);

  Items[Index] := Item;
end;

{ TScCertCRLDistributionPointsExtension }

constructor TScCertCRLDistributionPointsExtension.Create(const OId: string; ACritical: boolean; const DERValue: TBytes);
begin
  inherited; // for C++ Builder
end;

function TScCertCRLDistributionPointsExtension.Clone: TScPersistent;
begin
  Result := TScCertCRLDistributionPointsExtension.Create(FOId.Value, FCritical, FRawData);
end;

destructor TScCertCRLDistributionPointsExtension.Destroy;
begin
  FCRLDistributionPoints.Free;
  inherited;
end;

procedure TScCertCRLDistributionPointsExtension.Parse(const DERValue: TBytes);
var
  ASN1Compiler: TScASN1Compiler;
  CRLDistributionPointsLexem: TScLexemInfo;
  DistributionPointLexem, ReasonsLexem, CRLIssuerLexem: TScLexemInfo;
  CRLDistributionPoint: TScCRLDistributionPoint;
  GeneralName: TScGeneralName;
  RDN: TScRelativeDistinguishedName;
  i: integer;
begin
  if FCRLDistributionPoints = nil then
    FCRLDistributionPoints := TScCRLDistributionPointList.Create
  else
    FCRLDistributionPoints.Clear;

  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_CRL_DISTRIBUTION_POINTS_EXTENSION_DESC, DERValue) then
      raise EScError.Create(seWrongExtensionData);

    CRLDistributionPointsLexem := ASN1Compiler['CRLDistributionPoints'];

    for i := 0 to CRLDistributionPointsLexem.ValueCount - 1 do begin
      CRLDistributionPoint := TScCRLDistributionPoint.Create;
      FCRLDistributionPoints.Add(CRLDistributionPoint);

      DistributionPointLexem := CRLDistributionPointsLexem.Values[i]['DistributionPoint'];
      if not DistributionPointLexem.IsNull then begin
        if DistributionPointLexem.Name = 'FullName' then
          CRLDistributionPoint.DistributionPointName.Parse(DistributionPointLexem)
        else
        if DistributionPointLexem.Name = 'NameRelativeToCRLIssuer' then begin
          GeneralName := TScGeneralName.Create;
          CRLDistributionPoint.DistributionPointName.Add(GeneralName);
          GeneralName.FName := 'Relative Name';

          RDN := TScRelativeDistinguishedName.Create;
          try
            RDN.Parse(DistributionPointLexem);
            GeneralName.FValue := RDN.ToString;
          finally
            RDN.Free;
          end;
        end;
      end;

      ReasonsLexem := CRLDistributionPointsLexem.Values[i]['Reasons'];
      TScCRLDistributionPoint.ParseReasons(ReasonsLexem, CRLDistributionPoint.FReasons);

      CRLIssuerLexem := CRLDistributionPointsLexem.Values[i]['CRLIssuer'];
      CRLDistributionPoint.CRLIssuer.Parse(CRLIssuerLexem);
    end;
  finally
    ASN1Compiler.Free;
  end;
end;

function TScCertCRLDistributionPointsExtension.ToString: string;
var
  CRLDistributionPoint: TScCRLDistributionPoint;
  Str: string;
  i: integer;
begin
  Result := '';
  if FCRLDistributionPoints = nil then
    Exit;

  for i := 0 to FCRLDistributionPoints.Count - 1 do begin
    if i > 0 then
      Result := Result + '; '#13#10;

    CRLDistributionPoint := FCRLDistributionPoints[i];
    Result := Result + '[' + IntToStr(i + 1) + ']CRL Distribution Point '#13#10;
    Str := CRLDistributionPoint.DistributionPointName.ToString;
    if Str <> '' then
      Result := Result + 'Distribution Point Name: '#13#10 + Str;

    Str := TScCRLDistributionPoint.ReasonsToString(CRLDistributionPoint.FReasons);
    if Str <> '' then
      Result := Result + '; '#13#10'CRL Reasons=' + Str;

    Str := CRLDistributionPoint.CRLIssuer.ToString;
    if Str <> '' then
      Result := Result + '; '#13#10'CRL Issuer: '#13#10 + Str;
  end;
end;

{ TScCertFreshestCRLExtension }

constructor TScCertFreshestCRLExtension.Create(const OId: string; ACritical: boolean; const DERValue: TBytes);
begin
  inherited; // for C++ Builder
end;

function TScCertFreshestCRLExtension.Clone: TScPersistent;
begin
  Result := TScCertFreshestCRLExtension.Create(FOId.Value, FCritical, FRawData);
end;

{ TScInfoAccess }

constructor TScInfoAccess.Create;
begin
  inherited Create;

  FAccessMethod := TScOid.Create;
  FAccessLocation := TScGeneralName.Create;
end;

destructor TScInfoAccess.Destroy;
begin
  FAccessMethod.Free;
  FAccessLocation.Free;
  inherited;
end;

function TScInfoAccess.Clone: TScPersistent;
begin
  Result := TScInfoAccess.Create;
  Result.Assign(Self);
end;

procedure TScInfoAccess.Assign(Source: TScPersistent);
begin
  if (Source = nil) or not IsClass(Source, TScInfoAccess) then
    RaiseAssignError(Source);

  FAccessMethod.Assign(TScInfoAccess(Source).FAccessMethod);
  FAccessLocation.Assign(TScInfoAccess(Source).FAccessLocation);
end;

{ TScInfoAccessList }

function TScInfoAccessList.GetItemClassType: TScPersistentClass;
begin
  Result := TScInfoAccess;
end;

function TScInfoAccessList.GetInfoAccess(Index: integer): TScInfoAccess;
begin
  Result := TObject(Items[Index]) as TScInfoAccess;
end;

procedure TScInfoAccessList.SetInfoAccess(Index: integer; Item: TScInfoAccess);
begin
  if Item = nil then
    raise EScError.Create(seInvalidInputArgs);

  Items[Index] := Item;
end;

{ TScCertAuthorityInfoAccessExtension }

constructor TScCertAuthorityInfoAccessExtension.Create(const OId: string; ACritical: boolean; const DERValue: TBytes);
begin
  inherited; // for C++ Builder
end;

function TScCertAuthorityInfoAccessExtension.Clone: TScPersistent;
begin
  Result := TScCertAuthorityInfoAccessExtension.Create(FOId.Value, FCritical, FRawData);
end;

destructor TScCertAuthorityInfoAccessExtension.Destroy;
begin
  FAuthorityInfoAccessList.Free;
  inherited;
end;

procedure TScCertAuthorityInfoAccessExtension.Parse(const DERValue: TBytes);
var
  ASN1Compiler: TScASN1Compiler;
  AuthorityInfoAccessLexem, AccessDescriptionLexem: TScLexemInfo;
  AuthorityInfoAccess: TScInfoAccess;
  i: integer;
begin
  if FAuthorityInfoAccessList = nil then
    FAuthorityInfoAccessList := TScInfoAccessList.Create
  else
    FAuthorityInfoAccessList.Clear;

  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_AUTHORITY_INFO_ACCESS_EXTENSION_DESC, DERValue) then
      raise EScError.Create(seWrongExtensionData);

    AuthorityInfoAccessLexem := ASN1Compiler['AuthorityInfoAccess'];

    for i := 0 to AuthorityInfoAccessLexem.ValueCount - 1 do begin
      AuthorityInfoAccess := TScInfoAccess.Create;
      FAuthorityInfoAccessList.Add(AuthorityInfoAccess);

      AccessDescriptionLexem := AuthorityInfoAccessLexem.Values[i]['AccessDescription'];
      AuthorityInfoAccess.AccessMethod.Value := AccessDescriptionLexem['AccessMethod'].AsOID;
      AuthorityInfoAccess.AccessLocation.Parse(AccessDescriptionLexem['AccessLocation']);
    end;
  finally
    ASN1Compiler.Free;
  end;
end;

function TScCertAuthorityInfoAccessExtension.ToString: string;
var
  AuthorityInfoAccess: TScInfoAccess;
  i: integer;
begin
  Result := '';
  if FAuthorityInfoAccessList = nil then
    Exit;

  for i := 0 to FAuthorityInfoAccessList.Count - 1 do begin
    if i > 0 then
      Result := Result + '; '#13#10;

    AuthorityInfoAccess := FAuthorityInfoAccessList[i];
    Result := Result + '[' + IntToStr(i + 1) + ']Authority Info Access '#13#10;
    Result := Result + 'Access Method=' + AuthorityInfoAccess.AccessMethod.FriendlyName;
    Result := Result + ' (' + AuthorityInfoAccess.AccessMethod.Value + '); '#13#10;
    Result := Result + 'Alternative Name: ' + AuthorityInfoAccess.AccessLocation.ToString;
  end;
end;

{ TScCertSubjectInfoAccessExtension }

constructor TScCertSubjectInfoAccessExtension.Create(const OId: string; ACritical: boolean; const DERValue: TBytes);
begin
  inherited; // for C++ Builder
end;

function TScCertSubjectInfoAccessExtension.Clone: TScPersistent;
begin
  Result := TScCertSubjectInfoAccessExtension.Create(FOId.Value, FCritical, FRawData);
end;

destructor TScCertSubjectInfoAccessExtension.Destroy;
begin
  FSubjectInfoAccessList.Free;
  inherited;
end;

procedure TScCertSubjectInfoAccessExtension.Parse(const DERValue: TBytes);
var
  ASN1Compiler: TScASN1Compiler;
  SubjectInfoAccessLexem, AccessDescriptionLexem: TScLexemInfo;
  SubjectInfoAccess: TScInfoAccess;
  i: integer;
begin
  if FSubjectInfoAccessList = nil then
    FSubjectInfoAccessList := TScInfoAccessList.Create
  else
    FSubjectInfoAccessList.Clear;

  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_SUBJECT_INFO_ACCESS_EXTENSION_DESC, DERValue) then
      raise EScError.Create(seWrongExtensionData);

    SubjectInfoAccessLexem := ASN1Compiler['SubjectInfoAccess'];

    for i := 0 to SubjectInfoAccessLexem.ValueCount - 1 do begin
      SubjectInfoAccess := TScInfoAccess.Create;
      FSubjectInfoAccessList.Add(SubjectInfoAccess);

      AccessDescriptionLexem := SubjectInfoAccessLexem.Values[i]['AccessDescription'];
      SubjectInfoAccess.AccessMethod.Value := AccessDescriptionLexem['AccessMethod'].AsOID;
      SubjectInfoAccess.AccessLocation.Parse(AccessDescriptionLexem['AccessLocation']);
    end;
  finally
    ASN1Compiler.Free;
  end;
end;

function TScCertSubjectInfoAccessExtension.ToString: string;
var
  SubjectInfoAccess: TScInfoAccess;
  i: integer;
begin
  Result := '';
  if FSubjectInfoAccessList = nil then
    Exit;

  for i := 0 to FSubjectInfoAccessList.Count - 1 do begin
    if i > 0 then
      Result := Result + '; '#13#10;

    SubjectInfoAccess := FSubjectInfoAccessList[i];
    Result := Result + '[' + IntToStr(i + 1) + ']Subject Info Access '#13#10;
    Result := Result + 'Access Method=' + SubjectInfoAccess.AccessMethod.FriendlyName;
    Result := Result + ' (' + SubjectInfoAccess.AccessMethod.Value + '); '#13#10;
    Result := Result + 'Alternative Name: ' + SubjectInfoAccess.AccessLocation.ToString;
  end;
end;

{ TScCRLNumberExtension }

constructor TScCRLNumberExtension.Create(const OId: string; ACritical: boolean; const DERValue: TBytes);
begin
  inherited; // for C++ Builder
end;

function TScCRLNumberExtension.Clone: TScPersistent;
begin
  Result := TScCRLNumberExtension.Create(FOId.Value, FCritical, FRawData);
end;

procedure TScCRLNumberExtension.Parse(const DERValue: TBytes);
var
  ASN1Compiler: TScASN1Compiler;
begin
  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_CRL_NUMBER_EXTENSION_DESC, DERValue) then
      raise EScError.Create(seWrongExtensionData);

    FCRLNumber := ASN1Compiler['CRLNumber'].AsString;
  finally
    ASN1Compiler.Free;
  end;
end;

function TScCRLNumberExtension.ToString: string;
begin
  Result := 'CRL Number: ' + FCRLNumber;
end;

{ TScCRLDeltaIndicatorExtension }

constructor TScCRLDeltaIndicatorExtension.Create(const OId: string; ACritical: boolean; const DERValue: TBytes);
begin
  inherited; // for C++ Builder
end;

function TScCRLDeltaIndicatorExtension.Clone: TScPersistent;
begin
  Result := TScCRLDeltaIndicatorExtension.Create(FOId.Value, FCritical, FRawData);
end;

procedure TScCRLDeltaIndicatorExtension.Parse(const DERValue: TBytes);
var
  ASN1Compiler: TScASN1Compiler;
begin
  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_CRL_NUMBER_EXTENSION_DESC, DERValue) then
      raise EScError.Create(seWrongExtensionData);

    FBaseCRLNumber := ASN1Compiler['CRLNumber'].AsString;
  finally
    ASN1Compiler.Free;
  end;
end;

function TScCRLDeltaIndicatorExtension.ToString: string;
begin
  Result := 'Base CRL Number: ' + FBaseCRLNumber;
end;

{ TScCRLIssuingDistributionPointExtension }

constructor TScCRLIssuingDistributionPointExtension.Create(const OId: string; ACritical: boolean; const DERValue: TBytes);
begin
  inherited; // for C++ Builder
end;

function TScCRLIssuingDistributionPointExtension.Clone: TScPersistent;
begin
  Result := TScCRLIssuingDistributionPointExtension.Create(FOId.Value, FCritical, FRawData);
end;

destructor TScCRLIssuingDistributionPointExtension.Destroy;
begin
  FDistributionPointName.Free;
  inherited;
end;

procedure TScCRLIssuingDistributionPointExtension.Parse(const DERValue: TBytes);
var
  ASN1Compiler: TScASN1Compiler;
  DistributionPointLexem, OnlySomeReasonsLexem: TScLexemInfo;
  GeneralName: TScGeneralName;
  RDN: TScRelativeDistinguishedName;
begin
  if FDistributionPointName = nil then
    FDistributionPointName := TScGeneralNames.Create
  else
    FDistributionPointName.Clear;

  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_CRL_ISSUING_DISTRIBUTION_POINT_EXTENSION_DESC, DERValue) then
      raise EScError.Create(seWrongExtensionData);

    DistributionPointLexem := ASN1Compiler['DistributionPoint'];
    if not DistributionPointLexem.IsNull then begin
      if DistributionPointLexem.Name = 'FullName' then
        FDistributionPointName.Parse(DistributionPointLexem)
      else
      if DistributionPointLexem.Name = 'NameRelativeToCRLIssuer' then begin
        GeneralName := TScGeneralName.Create;
        FDistributionPointName.Add(GeneralName);
        GeneralName.FName := 'Relative Name';

        RDN := TScRelativeDistinguishedName.Create;
        try
          RDN.Parse(DistributionPointLexem);
          GeneralName.FValue := RDN.ToString;
        finally
          RDN.Free;
        end;
      end;
    end;

    OnlySomeReasonsLexem := ASN1Compiler['OnlySomeReasons'];
    TScCRLDistributionPoint.ParseReasons(OnlySomeReasonsLexem, FOnlySomeReasons);

    FIndirectCRL := ASN1Compiler['IndirectCRL'].AsBoolean;
    FOnlyContainsUserCerts := ASN1Compiler['OnlyContainsUserCerts'].AsBoolean;
    FOnlyContainsCACerts := ASN1Compiler['OnlyContainsCACerts'].AsBoolean;
    FOnlyContainsAttributeCerts := ASN1Compiler['OnlyContainsAttributeCerts'].AsBoolean;
  finally
    ASN1Compiler.Free;
  end;
end;

function TScCRLIssuingDistributionPointExtension.ToString: string;
var
  Str: string;
begin
  Result := '';
  if FDistributionPointName = nil then
    Exit;

  Result := 'Issuing CRL Distribution Point '#13#10;
  Str := FDistributionPointName.ToString;
  if Str <> '' then
    Result := Result + 'Distribution Point Name: '#13#10 + Str + #13#10;

  Result := Result + 'Only Contains User Certs=' + BoolToStr(FOnlyContainsUserCerts, True) + '; '#13#10;
  Result := Result + 'Only Contains CA Certs=' + BoolToStr(FOnlyContainsCACerts, True) + '; '#13#10;
  Str := TScCRLDistributionPoint.ReasonsToString(FOnlySomeReasons);
  if Str <> '' then
    Result := Result + 'Only Some Reasons=' + Str + '; '#13#10;
  Result := Result + 'Indirect CRL=' + BoolToStr(FIndirectCRL, True) + '; '#13#10;
  Result := Result + 'Only Contains Attribute Certs=' + BoolToStr(FOnlyContainsAttributeCerts, True);
end;

{ TScCRLReasonCodeExtension }

constructor TScCRLReasonCodeExtension.Create(const OId: string; ACritical: boolean; const DERValue: TBytes);
begin
  inherited; // for C++ Builder
end;

function TScCRLReasonCodeExtension.Clone: TScPersistent;
begin
  Result := TScCRLReasonCodeExtension.Create(FOId.Value, FCritical, FRawData);
end;

procedure TScCRLReasonCodeExtension.Parse(const DERValue: TBytes);
var
  ASN1Compiler: TScASN1Compiler;
begin
  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_CRL_REASON_EXTENSION_DESC, DERValue) then
      raise EScError.Create(seWrongExtensionData);

    case ASN1Compiler['CRLReason'].AsInteger of
      1:
        FCRLReason := crKeyCompromise;
      2:
        FCRLReason := crCACompromise;
      3:
        FCRLReason := crAffiliationChanged;
      4:
        FCRLReason := crSuperseded;
      5:
        FCRLReason := crCessationOfOperation;
      6:
        FCRLReason := crCertificateHold;
      8:
        FCRLReason := crRemoveFromCRL;
      9:
        FCRLReason := crPrivilegeWithdrawn;
      10:
        FCRLReason := crAACompromise;
    else
      FCRLReason := crUnspecified;
    end;
  finally
    ASN1Compiler.Free;
  end;
end;

function TScCRLReasonCodeExtension.ToString: string;
begin
  Result := 'CRL Reason: ' + CRL_REASONS[FCRLReason];
end;

{ TScCRLInvalidityDateExtension }

constructor TScCRLInvalidityDateExtension.Create(const OId: string; ACritical: boolean; const DERValue: TBytes);
begin
  inherited; // for C++ Builder
end;

function TScCRLInvalidityDateExtension.Clone: TScPersistent;
begin
  Result := TScCRLInvalidityDateExtension.Create(FOId.Value, FCritical, FRawData);
end;

procedure TScCRLInvalidityDateExtension.Parse(const DERValue: TBytes);
var
  ASN1Compiler: TScASN1Compiler;
begin
  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_CRL_INVALIDITY_DATE_EXTENSION_DESC, DERValue) then
      raise EScError.Create(seWrongExtensionData);

    FInvalidityDate := ASN1Compiler['InvalidityDate'].AsDateTime;
  finally
    ASN1Compiler.Free;
  end;
end;

function TScCRLInvalidityDateExtension.ToString: string;
begin
  Result := 'Invalidity Date: ' + DateTimeToStr(FInvalidityDate);
end;

{ TScCRLCertificateIssuerExtension }

constructor TScCRLCertificateIssuerExtension.Create(const OId: string; ACritical: boolean; const DERValue: TBytes);
begin
  inherited; // for C++ Builder
end;

function TScCRLCertificateIssuerExtension.Clone: TScPersistent;
begin
  Result := TScCRLCertificateIssuerExtension.Create(FOId.Value, FCritical, FRawData);
end;

destructor TScCRLCertificateIssuerExtension.Destroy;
begin
  FCertificateIssuer.Free;
  inherited;
end;

procedure TScCRLCertificateIssuerExtension.Parse(const DERValue: TBytes);
var
  ASN1Compiler: TScASN1Compiler;
begin
  if FCertificateIssuer = nil then
    FCertificateIssuer := TScGeneralNames.Create
  else
    FCertificateIssuer.Clear;

  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_CRL_CERTIFICATE_ISSUER_EXTENSION_DESC, DERValue) then
      raise EScError.Create(seWrongExtensionData);

    FCertificateIssuer.Parse(ASN1Compiler['CertificateIssuer']);
  finally
    ASN1Compiler.Free;
  end;
end;

function TScCRLCertificateIssuerExtension.ToString: string;
begin
  Result := '';
  if FCertificateIssuer = nil then
    Exit;

  Result := 'Certificate Issuer: '#13#10 + FCertificateIssuer.ToString;
end;

{ TScSignedCertificateTimestamp }

function TScSignedCertificateTimestamp.Clone: TScPersistent;
begin
  Result := TScSignedCertificateTimestamp.Create;
  Result.Assign(Self);
end;

procedure TScSignedCertificateTimestamp.Assign(Source: TScPersistent);
begin
  if (Source = nil) or not IsClass(Source, TScSignedCertificateTimestamp) then
    RaiseAssignError(Source);

  FVersion := TScSignedCertificateTimestamp(Source).FVersion;
  SetLength(FLogID, Length(TScSignedCertificateTimestamp(Source).FLogID));
  if Length(FLogID) > 0 then
    Move(TScSignedCertificateTimestamp(Source).FLogID[0], FLogID[0], Length(FLogID));

  FTimeStamp := TScSignedCertificateTimestamp(Source).FTimeStamp;
  FSignatureHash := TScSignedCertificateTimestamp(Source).FSignatureHash;
  FSignatureAlgorithm := TScSignedCertificateTimestamp(Source).FSignatureAlgorithm;

  SetLength(FSignature, Length(TScSignedCertificateTimestamp(Source).FSignature));
  if Length(FSignature) > 0 then
    Move(TScSignedCertificateTimestamp(Source).FSignature[0], FSignature[0], Length(FSignature));
end;

procedure TScSignedCertificateTimestamp.Parse(const Buffer: TBytes; Offset: integer);
var
  Len: integer;
  TS64: Int64;
begin
  FVersion := Buffer[Offset];

  if FVersion = 0 then begin
    if (Offset + 47) > Length(Buffer) then
      raise EScError.Create(seWrongExtensionData);

    SetLength(FLogID, 32);
    Move(Buffer[Offset + 1], FLogID[0], 32);
    Inc(Offset, 33);

    TS64 := GetInt64BE(Buffer, Offset);
    FTimeStamp := (TS64 / MSecsPerDay) + UnixDateDelta + GetLocalTimeZoneOffset;
    Inc(Offset, 8);

    Len := Buffer[Offset] shl 8 + Buffer[Offset + 1]; // Extensions
    Inc(Offset, 2);
    if (Offset + Len) > Length(Buffer) then
      raise EScError.Create(seWrongExtensionData);
    Inc(Offset, Len); // skip Extensions

    case Buffer[Offset] of
      0:
        FSignatureHash := haNone;
      1:
        FSignatureHash := haMD5;
      2:
        FSignatureHash := haSHA1;
      3:
        FSignatureHash := haSHA2_224;
      4:
        FSignatureHash := haSHA2_256;
      5:
        FSignatureHash := haSHA2_384;
      6:
        FSignatureHash := haSHA2_512;
    else
      FSignatureHash := haNone;
    end;

    case Buffer[Offset + 1] of
      1:
        FSignatureAlgorithm := aaRSA;
      2:
        FSignatureAlgorithm := aaDSA;
      3:
        FSignatureAlgorithm := aaEC;
    else
      FSignatureAlgorithm := aaRSA;
    end;

    Inc(Offset, 2);

    Len := Buffer[Offset] shl 8 + Buffer[Offset + 1];
    Inc(Offset, 2);
    if (Len = 0) or ((Offset + Len) > Length(Buffer)) then
      raise EScError.Create(seWrongExtensionData);

    SetLength(FSignature, Len);
    Move(Buffer[Offset], FSignature[0], Len);
  end;
end;

function TScSignedCertificateTimestamp.ToString: string;
begin
  Result := 'v' + IntToStr(FVersion + 1) + #13#10;
  Result := Result + BytesToHexStr(FLogID) + #13#10;
  Result := Result + DateTimeToStr(FTimeStamp) + #13#10;
  Result := Result + CipherFactory.HashAlgorithmToName(FSignatureHash) + #13#10;
  Result := Result + CipherFactory.PublicKeyAlgorithmToName(FSignatureAlgorithm) + #13#10;
  Result := Result + BytesToHexStr(FSignature) + #13#10;
end;

{ TScSignedCertificateTimestampList }

function TScSignedCertificateTimestampList.GetItemClassType: TScPersistentClass;
begin
  Result := TScSignedCertificateTimestamp;
end;

function TScSignedCertificateTimestampList.GetSCT(Index: integer): TScSignedCertificateTimestamp;
begin
  Result := TObject(Items[Index]) as TScSignedCertificateTimestamp;
end;

procedure TScSignedCertificateTimestampList.SetSCT(Index: integer; Item: TScSignedCertificateTimestamp);
begin
  if Item = nil then
    raise EScError.Create(seInvalidInputArgs);

  Items[Index] := Item;
end;

function TScSignedCertificateTimestampList.ToString: string;
var
  i: integer;
begin
  Result := '';
  for i := 0 to Count - 1 do
    Result := Result + GetSCT(i).ToString + #13#10;
end;

{ TScCertSignedCertificateTimestampListExtension }

constructor TScCertSignedCertificateTimestampListExtension.Create(const OId: string; ACritical: boolean; const DERValue: TBytes);
begin
  inherited; // for C++ Builder
end;

function TScCertSignedCertificateTimestampListExtension.Clone: TScPersistent;
begin
  Result := TScCertSignedCertificateTimestampListExtension.Create(FOId.Value, FCritical, FRawData);
end;

destructor TScCertSignedCertificateTimestampListExtension.Destroy;
begin
  FSCTList.Free;
  inherited;
end;

procedure TScCertSignedCertificateTimestampListExtension.Parse(const DERValue: TBytes);
var
  ASN1Compiler: TScASN1Compiler;
  Buffer: TBytes;
  Offset, Len: integer;
  SCT: TScSignedCertificateTimestamp;
begin
  if FSCTList = nil then
    FSCTList := TScSignedCertificateTimestampList.Create
  else
    FSCTList.Clear;

  SetLength(Buffer, 0);
  ASN1Compiler := TScASN1Compiler.Create;
  try
    if not ASN1Compiler.Parse(asn1_SIGNED_CERTIFICATE_TIMESTAMP_LIST_DESC, DERValue) then
      raise EScError.Create(seWrongExtensionData);

    Buffer := ASN1Compiler.Root.AsBytes;
    if Length(Buffer) < 2 then
      raise EScError.Create(seWrongExtensionData);

    Len := Buffer[0] shl 8 + Buffer[1];
    if (Len < 2) or ((Len + 2) <> Length(Buffer)) then
      raise EScError.Create(seWrongExtensionData);

    Offset := 2;
    while (Offset + 1) < Length(Buffer) do begin
      Len := Buffer[Offset] shl 8 + Buffer[Offset + 1];
      Inc(Offset, 2);
      if (Len < 1) or ((Offset + Len) > Length(Buffer)) then
        raise EScError.Create(seWrongExtensionData);

      SCT := TScSignedCertificateTimestamp.Create;
      FSCTList.Add(SCT);
      SCT.Parse(Buffer, Offset);
      Inc(Offset, Len);
    end;
  finally
    ASN1Compiler.Free;
  end;
end;

function TScCertSignedCertificateTimestampListExtension.ToString: string;
begin
  if FSCTList = nil then
    Result := ''
  else
    Result := FSCTList.ToString;
end;

{ TScExtensions }

function TScExtensions.GetItemClassType: TScPersistentClass;
begin
  Result := TScCertificateExtension;
end;

procedure TScExtensions.Parse(LexemInfo: TScLexemInfo);
var
  ExtLexem: TScLexemInfo;
  Extension: TScCertificateExtension;
  OId: string;
  DERValue: TBytes;
  Critical: boolean;
  i: integer;
begin
  Clear;
  SetLength(DERValue, 0);

  for i := 0 to LexemInfo.ValueCount - 1 do begin
    ExtLexem := LexemInfo.Values[i];
    OId := ExtLexem['ExtnID'].AsOID;
    Critical := ExtLexem['Critical'].AsBoolean;
    DERValue := ExtLexem['ExtnValue'].AsBytes;

    Extension := nil;
    try
      if (OId = OID_CE_BASIC_CONSTRAINTS) or (OId = OID_CE_BASIC_CONSTRAINTS2) then
        Extension := TScCertBasicConstraintsExtension.Create(OId, Critical, DERValue)
      else
      if OId = OID_CE_KEY_USAGE then
        Extension := TScCertKeyUsageExtension.Create(OId, Critical, DERValue)
      else
      if OId = OID_CE_EXTENDED_KEY_USAGE then
        Extension := TScCertExtendedKeyUsageExtension.Create(OId, Critical, DERValue)
      else
      if OId = OID_CE_SUBJECT_KEY_IDENTIFIER then
        Extension := TScCertSubjectKeyIdExtension.Create(OId, Critical, DERValue)
      else
      if (OId = OID_CE_AUTHORITY_KEY_IDENTIFIER) or (OId = OID_CE_AUTHORITY_KEY_IDENTIFIER2) then
        Extension := TScCertAuthorityKeyIdExtension.Create(OId, Critical, DERValue)
      else
      if OId = OID_CE_SUBJECT_ALTERNATIVE_NAME then
        Extension := TScCertSubjectAlternativeNameExtension.Create(OId, Critical, DERValue)
      else
      if OId = OID_CE_ISSUER_ALTERNATIVE_NAME then
        Extension := TScCertIssuerAlternativeNameExtension.Create(OId, Critical, DERValue)
      else
      if OId = OID_CE_SUBJECT_DIRECTORY_ATTRIBUTES then
        Extension := TScCertSubjectDirectoryAttributesExtension.Create(OId, Critical, DERValue)
      else
      if OId = OID_CE_CERTIFICATE_POLICIES then
        Extension := TScCertPoliciesExtension.Create(OId, Critical, DERValue)
      else
      if OId = OID_CE_POLICY_MAPPINGS then
        Extension := TScCertPolicyMappingsExtension.Create(OId, Critical, DERValue)
      else
      if OId = OID_CE_CRL_DISTRIBUTION_POINTS then
        Extension := TScCertCRLDistributionPointsExtension.Create(OId, Critical, DERValue)
      else
      if OId = OID_CE_FRESHEST_CRL_POINTS then
        Extension := TScCertFreshestCRLExtension.Create(OId, Critical, DERValue)
      else
      if OId = OID_PE_AUTHORITY_INFO_ACCESS then
        Extension := TScCertAuthorityInfoAccessExtension.Create(OId, Critical, DERValue)
      else
      if OId = OID_PE_SUBJECT_INFO_ACCESS then
        Extension := TScCertSubjectInfoAccessExtension.Create(OId, Critical, DERValue)
      else
      if OId = OID_CE_CRL_NUMBER then
        Extension := TScCRLNumberExtension.Create(OId, Critical, DERValue)
      else
      if OId = OID_CE_DELTA_CRL_INDICATOR then
        Extension := TScCRLDeltaIndicatorExtension.Create(OId, Critical, DERValue)
      else
      if OId = OID_CE_ISSUING_DISTRIBUTION_POINT then
        Extension := TScCRLIssuingDistributionPointExtension.Create(OId, Critical, DERValue)
      else
      if OId = OID_CE_CRL_REASONS then
        Extension := TScCRLReasonCodeExtension.Create(OId, Critical, DERValue)
      else
      if OId = OID_CE_CRL_INVALIDITY_DATE then
        Extension := TScCRLInvalidityDateExtension.Create(OId, Critical, DERValue)
      else
      if OId = OID_CE_CRL_CERTIFICATE_ISSUER then
        Extension := TScCRLCertificateIssuerExtension.Create(OId, Critical, DERValue)
      else
      if OId = OID_SIGNED_CERTIFICATE_TIMESTAMP_LIST then
        Extension := TScCertSignedCertificateTimestampListExtension.Create(OId, Critical, DERValue)
      else
        Extension := TScCertificateExtension.Create(OId, Critical, DERValue);

      Add(Extension);
    except
      Extension.Free;
      raise;
    end;
  end;
end;

function TScExtensions.FindExtensionByClass(AClass: TScCertificateExtensionClass): TScCertificateExtension;
var
  i: integer;
begin
  for i := 0 to Count - 1 do begin
    Result := TObject(Items[i]) as TScCertificateExtension;
    if Result.ClassType = AClass then // not IS for exact match
      Exit;
  end;

  Result := nil;
end;

function TScExtensions.GetExtension(Index: integer): TScCertificateExtension;
begin
  Result := TObject(Items[Index]) as TScCertificateExtension;
end;

procedure TScExtensions.SetExtension(Index: integer; Item: TScCertificateExtension);
begin
  Items[Index] := Item;
end;

{ TScASNUtils }

class procedure TScASNUtils.Parse(Obj: TScASN1AlgorithmIdentifier; LexemInfo: TScLexemInfo);
begin
  Obj.Parse(LexemInfo);
end;

class procedure TScASNUtils.Parse(Obj: TScDistinguishedName; LexemInfo: TScLexemInfo);
begin
  Obj.Parse(LexemInfo);
end;

class procedure TScASNUtils.Parse(Obj: TScPKCS7Attributes; LexemInfo: TScLexemInfo);
begin
  Obj.Parse(LexemInfo);
end;

class procedure TScASNUtils.Parse(Obj: TScExtensions; LexemInfo: TScLexemInfo);
begin
  Obj.Parse(LexemInfo);
end;

class procedure TScASNUtils.SetOIdBeforeChange(OId: TScOId; OnChange: TNotifyEvent);
begin
  OId.FBeforeChange := OnChange;
end;

class procedure TScASNUtils.SetOIdAfterChange(OId: TScOId; OnChange: TNotifyEvent);
begin
  OId.FAfterChange := OnChange;
end;

class procedure TScASNUtils.SetReadOnly(OId: TScOId; Value: boolean);
begin
  OId.SetReadOnly(Value);
end;

class procedure TScASNUtils.SetReadOnly(ASN1AlgorithmIdentifier: TScASN1AlgorithmIdentifier; Value: boolean);
begin
  ASN1AlgorithmIdentifier.SetReadOnly(Value);
end;

end.

