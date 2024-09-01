{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressCore Library }
{ }
{ Copyright (c) 1998-2021 Developer Express Inc. }
{ ALL RIGHTS RESERVED }
{ }
{ The entire contents of this file is protected by U.S. and }
{ International Copyright Laws. Unauthorized reproduction, }
{ reverse-engineering, and distribution of all or any portion of }
{ the code contained in this file is strictly prohibited and may }
{ result in severe civil and criminal penalties and will be }
{ prosecuted to the maximum extent possible under the law. }
{ }
{ RESTRICTIONS }
{ }
{ THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES }
{ (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE }
{ SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS }
{ LICENSED TO DISTRIBUTE THE EXPRESSCORE LIBRARY AND ALL }
{ ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
{ }
{ THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED }
{ FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE }
{ COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE }
{ AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT }
{ AND PERMISSION FROM DEVELOPER EXPRESS INC. }
{ }
{ CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON }
{ ADDITIONAL RESTRICTIONS. }
{ }
{ ******************************************************************** }

unit dxX509Certificate;

{$I cxVer.inc}

interface

uses
  Generics.Defaults, Generics.Collections, Windows, SysUtils, Classes,
  dxCore, dxCoreClasses, dxCryptoAPI;

type
  TdxX509Certificate = class;
  EdxX509Exception = class(EdxException);

  TdxX509CertificateImportFlag = (cifMakeExportable, cifStrongProtection,
    cifMachineKeySet, cifUserKeySet);
  TdxX509CertificateImportFlags = set of TdxX509CertificateImportFlag;
  TdxX509CertificateKeyUsageFlag = (kufDigitalSignature, kufNonRepudiation,
    kufKeyEncipherment, kufDataEncipherment, kufKeyAgreement, kufKeyCertSign,
    kufOfflineCRLSign);
  TdxX509CertificateKeyUsageFlags = set of TdxX509CertificateKeyUsageFlag;
  TdxX509StoreLocation = (slCurrentUser, slLocalMachine);
  TdxX509StoreMode = (smReadOnly = 0, smReadWrite = 1, smMaxAllowed = 2,
    smOpenExistingOnly = 4, smIncludeArchived = 8);
  TdxX509StoreName = (snAddressBook = 1, snAuthRoot = 2,
    snCertificateAuthority = 3, snDisallowed = 4, snMy = 5, snRoot = 6,
    snTrustedPeople = 7, snTrustedPublisher = 8);

  { TdxX509Certificate }

  TdxX509Certificate = class
  private
    FHandle: PCERT_CONTEXT;
    FFileName: string;
    FFriendlyName: TdxNullableString;
    FIssuedBy: TdxNullableString;
    FIssuedTo: TdxNullableString;
    FIssuer: TdxNullableString;
    FKeyUsage: TdxX509CertificateKeyUsageFlags;
    FPrivateKeyInfo: TdxNullableString;
    FPublicKeyAlgorithm: string;
    FPublicKeyAlgorithmName: TdxNullableString;
    FPublicKeyLength: TdxNullableInteger;
    FSerialNumber: string;
    FSignatureAlgorithm: string;
    FSignatureAlgorithmName: TdxNullableString;
    FSubject: TdxNullableString;
    FSubjectKeyIdentifier: TdxNullableString;
    FThumbprint: TdxNullableString;
    FUsageFlags: TdxNullableString;
    FValidFrom: TDateTime;
    FValidTo: TDateTime;
    FVersion: DWORD;

    function GetFriendlyName: string;
    function GetHasPrivateKey: Boolean;
    function GetIssuedBy: string;
    function GetIssuedTo: string;
    function GetIssuer: string;
    function GetKeyUsage: TdxX509CertificateKeyUsageFlags;
    function GetOIDInfo(const AOID: string): string;
    function GetPrivateKeyInfo: string;
    function GetPublicKeyAlgorithmName: string;
    function GetPublicKeyLength: Integer;
    function GetSignatureAlgorithmName: string;
    function GetSubject: string;
    function GetSubjectKeyIdentifier: string;
    function GetThumbprint: string;
    function GetUsageFlags: string;
  protected
    constructor Create; overload;
    constructor Create(AHandle: PCERT_CONTEXT); overload;

    property PrivateKeyInfo: string read GetPrivateKeyInfo;
    property PublicKeyAlgorithm: string read FPublicKeyAlgorithm;
    property SignatureAlgorithm: string read FSignatureAlgorithm;
    property SubjectKeyIdentifier: string read GetSubjectKeyIdentifier;
    property UsageFlags: string read GetUsageFlags;
  public
    constructor Create(const AFileName, APassword: string); overload;
    constructor Create(ACertificate: TdxX509Certificate); overload;
    constructor Create(AStream: TStream; const APassword: string); overload;
    destructor Destroy; override;

    property FileName: string read FFileName;
    property FriendlyName: string read GetFriendlyName;
    property Handle: PCERT_CONTEXT read FHandle;
    property HasPrivateKey: Boolean read GetHasPrivateKey;
    property IssuedBy: string read GetIssuedBy;
    property IssuedTo: string read GetIssuedTo;
    property Issuer: string read GetIssuer;
    property KeyUsage: TdxX509CertificateKeyUsageFlags read GetKeyUsage;
    property PublicKeyAlgorithmName: string read GetPublicKeyAlgorithmName;
    property PublicKeyLength: Integer read GetPublicKeyLength;
    property SerialNumber: string read FSerialNumber;
    property SignatureAlgorithmName: string read GetSignatureAlgorithmName;
    property Subject: string read GetSubject;
    property Thumbprint: string read GetThumbprint;
    property ValidFrom: TDateTime read FValidFrom;
    property ValidTo: TDateTime read FValidTo;
    property Version: DWORD read FVersion;
  end;

  { TdxX509CertificateList }

  TdxX509CertificateList = class(TObjectList<TdxX509Certificate>)
  public
    procedure CopyTo(AList: TdxX509CertificateList);
    procedure Import(const AFileName, APassword: string); overload;
    procedure Import(AStream: TStream; const APassword: string); overload;
  end;

  { TdxX509Store }

  TdxX509Store = class
  strict private
    FCertificateList: TdxX509CertificateList;
    FLocation: TdxX509StoreLocation;
    FMode: TdxX509StoreMode;
    FName: TdxX509StoreName;
    FHandle: HCERTSTORE;

    function GetHandle: HCERTSTORE;
    function HandleAllocated: Boolean;
    procedure SetHandle(AValue: HCERTSTORE);
    procedure SetMode(AValue: TdxX509StoreMode);
  protected
    procedure PopulateCertificateList;

    property Handle: HCERTSTORE read GetHandle write SetHandle;
  public
    constructor Create; overload;
    constructor Create(ALocation: TdxX509StoreLocation); overload;
    constructor Create(AName: TdxX509StoreName); overload;
    constructor Create(AName: TdxX509StoreName;
      ALocation: TdxX509StoreLocation); overload;
    destructor Destroy; override;

    procedure Close;
    procedure Open;

    property Certificates: TdxX509CertificateList read FCertificateList;
    property Location: TdxX509StoreLocation read FLocation;
    property Mode: TdxX509StoreMode read FMode;
    property Name: TdxX509StoreName read FName;
  end;

function dxX509CertificateKeyUsageFlagsToStr
  (AFlags: TdxX509CertificateKeyUsageFlags): string; // for internal use
function dxX509CertificateVersionToStr(AVersion: DWORD): string;
// for internal use
function dxX509DisplayCertificate(ACertificate: TdxX509Certificate;
  AParentWnd: THandle): Boolean; // for internal use
function dxX509IsUsableForDigitalSignature(ACertificate
  : TdxX509Certificate): Boolean;
function dxX509SelectCertificate(AStore: TdxX509Store; AParentWnd: THandle;
  out ACertificate: TdxX509Certificate): Boolean; // for internal use
function dxX509SHA1SignData(const AData: TBytes;
  ACertificate: TdxX509Certificate): TBytes; // for internal use
function dxX509StoreLocationToStr(ALocation: TdxX509StoreLocation): string;
// for internal use
function dxX509StoreNameToStr(AName: TdxX509StoreName): string;
// for internal use

implementation

uses
  Math;

const
  dxX509DefaultEncoding = X509_ASN_ENCODING or PKCS_7_ASN_ENCODING;

type
  { TdxCustomSigner }

  TdxCustomSigner = class
  strict protected
    class function GetSignAlgorithm: AnsiString; virtual; abstract;
    class function DoSign(const AData: TBytes;
      ACertificate: TdxX509Certificate): TBytes;
  public
    class function Sign(const AData: TBytes;
      ACertificate: TdxX509Certificate): TBytes;
  end;

  { TdxX509SHA1Signer }

  TdxX509SHA1Signer = class(TdxCustomSigner)
  strict protected
    class function GetSignAlgorithm: AnsiString; override;
  end;

  { TdxX509Utils }

  TdxX509Utils = class
  private
    class function GetCertificateNameBlobAsString(AHandle: PCERT_CONTEXT;
      const AFlag: Integer): string;
    class function SelectFromStore(AStore: TdxX509Store;
      const ATitle, AMessage: string; AMultiSelect: Boolean;
      AParentWnd: THandle; ASelectedCertificateList: TdxX509CertificateList)
      : Boolean; static;
  public
    class function GetCertificateGetKeyUsage(AHandle: PCERT_CONTEXT)
      : TdxX509CertificateKeyUsageFlags; static;
    class function GetCertificateFriendlyName(AHandle: PCERT_CONTEXT)
      : string; static;
    class function GetCertificateImportFlags
      (AFlags: TdxX509CertificateImportFlags): Integer; static;
    class function GetCertificateIssuer(AHandle: PCERT_CONTEXT): string;
    class function GetCertificateNameString(AHandle: PCERT_CONTEXT;
      AType, AFlags: Cardinal): string; static;
    class function GetCertificatePublicKeyLength(AHandle: PCERT_CONTEXT)
      : Integer; static;
    class function GetCertificatePrivateKeyInfo(AHandle: PCERT_CONTEXT)
      : string; static;
    class function GetCertificateSerialNumber(AHandle: PCERT_CONTEXT)
      : string; static;
    class function GetCertificateSubject(AHandle: PCERT_CONTEXT)
      : string; static;
    class function GetCertificateSubjectKeyIdentifier(AHandle: PCERT_CONTEXT)
      : string; static;
    class function GetCertificateThumbprint(AHandle: PCERT_CONTEXT)
      : string; static;
    class function GetCertificateUsageFlags(AHandle: PCERT_CONTEXT)
      : string; static;

    class function CertificateKeyUsageFlagsToStr
      (AFlags: TdxX509CertificateKeyUsageFlags): string; static;
    class function CertificateVersionToStr(AVersion: DWORD): string;
    class function StoreLocationToStr(ALocation: TdxX509StoreLocation)
      : string; static;
    class function StoreNameToStr(AName: TdxX509StoreName): string; static;

    class function LoadStore(AStream: TStream; const APassword: string;
      out AHandle: HCERTSTORE): Boolean; static;
    class procedure AddCertificateFromStore(AHandle: HCERTSTORE;
      ADestination: TdxX509CertificateList);
    class procedure RaiseException;

    // UI
    class function DisplayCertificate(ACertificate: TdxX509Certificate;
      AParentWnd: THandle): Boolean; overload;
    class function SelectCertificate(AStore: TdxX509Store;
      const ATitle, AMessage: string; AParentWnd: THandle;
      out ACertificate: TdxX509Certificate): Boolean;
  end;

function dxX509CertificateKeyUsageFlagsToStr
  (AFlags: TdxX509CertificateKeyUsageFlags): string;
begin
  Result := TdxX509Utils.CertificateKeyUsageFlagsToStr(AFlags);
end;

function dxX509CertificateVersionToStr(AVersion: DWORD): string;
begin
  Result := TdxX509Utils.CertificateVersionToStr(AVersion);
end;

function dxX509DisplayCertificate(ACertificate: TdxX509Certificate;
  AParentWnd: THandle): Boolean;
begin
  Result := TdxX509Utils.DisplayCertificate(ACertificate, AParentWnd);
end;

function dxX509IsUsableForDigitalSignature(ACertificate
  : TdxX509Certificate): Boolean;
begin
  Result := (ACertificate <> nil) and
    (kufDigitalSignature in ACertificate.KeyUsage) and
    ACertificate.HasPrivateKey;
end;

function dxX509SelectCertificate(AStore: TdxX509Store; AParentWnd: THandle;
  out ACertificate: TdxX509Certificate): Boolean;
begin
  Result := TdxX509Utils.SelectCertificate(AStore, '', '', AParentWnd,
    ACertificate);
end;

function dxX509SHA1SignData(const AData: TBytes;
  ACertificate: TdxX509Certificate): TBytes;
begin
  Result := TdxX509SHA1Signer.Sign(AData, ACertificate);
end;

function dxX509StoreLocationToStr(ALocation: TdxX509StoreLocation): string;
begin
  Result := TdxX509Utils.StoreLocationToStr(ALocation);
end;

function dxX509StoreNameToStr(AName: TdxX509StoreName): string;
begin
  Result := TdxX509Utils.StoreNameToStr(AName);
end;

{ TdxCustomSigner }

class function TdxCustomSigner.Sign(const AData: TBytes;
  ACertificate: TdxX509Certificate): TBytes;
begin
  Result := DoSign(AData, ACertificate);
end;

class function TdxCustomSigner.DoSign(const AData: TBytes;
  ACertificate: TdxX509Certificate): TBytes;
var
  ADataLength, ASignedDataLength: Integer;
  ADetached: Boolean;
  ASignMessageParameters: CRYPT_SIGN_MESSAGE_PARA;
begin
  SetLength(Result, 0);
  if ACertificate <> nil then
  begin
    ZeroMemory(@ASignMessageParameters, SizeOf(CRYPT_SIGN_MESSAGE_PARA));
    ASignMessageParameters.cbSize := SizeOf(CRYPT_SIGN_MESSAGE_PARA);
    ASignMessageParameters.dwMsgEncodingType := dxX509DefaultEncoding;
    ASignMessageParameters.pSigningCert := ACertificate.Handle;
    ASignMessageParameters.HashAlgorithm.pszObjId :=
      PAnsiChar(GetSignAlgorithm);
    ASignMessageParameters.rgpMsgCert := @ACertificate.Handle;
    ASignMessageParameters.cMsgCert := 1;

    ASignedDataLength := 0;
    ADetached := False;
    ADataLength := Length(AData);
    if CryptSignMessage(@ASignMessageParameters, ADetached, 1, @AData,
      @ADataLength, nil, @ASignedDataLength) then
    begin
      SetLength(Result, ASignedDataLength);
      CryptSignMessage(@ASignMessageParameters, ADetached, 1, @AData,
        @ADataLength, @Result[0], @ASignedDataLength);
    end;
  end;
end;

{ TdxX509SHA1Signer }

class function TdxX509SHA1Signer.GetSignAlgorithm: AnsiString;
begin
  Result := szOID_RSA_SHA1RSA;
end;

{ TdxX509Utils }

class function TdxX509Utils.GetCertificateFriendlyName
  (AHandle: PCERT_CONTEXT): string;
var
  AData: TBytes;
  ASize: DWORD;
  ALength: Integer;
begin
  ASize := 0;
  Result := '';
  if (AHandle <> nil) and CertGetCertificateContextProperty(AHandle,
    CERT_FRIENDLY_NAME_PROP_ID, nil, @ASize) and (ASize > 0) then
  begin
    SetLength(AData, ASize);
    CertGetCertificateContextProperty(AHandle, CERT_FRIENDLY_NAME_PROP_ID,
      @AData[0], @ASize);
    Result := TEncoding.Unicode.GetString(AData);
    ALength := Length(Result);
    if Result[ALength] = #0 then
      SetLength(Result, ALength - 1);
  end;
end;

class function TdxX509Utils.GetCertificateGetKeyUsage(AHandle: PCERT_CONTEXT)
  : TdxX509CertificateKeyUsageFlags;
const
  ASize = 1;
var
  AKeyUsage: TBytes;
begin
  Result := [];
  SetLength(AKeyUsage, ASize);
  CertGetIntendedKeyUsage(dxX509DefaultEncoding, AHandle.pCertInfo,
    @AKeyUsage[0], ASize);
  if AKeyUsage[0] <> 0 then
  begin
    if AKeyUsage[0] and CERT_DIGITAL_SIGNATURE_KEY_USAGE <> 0 then
      Include(Result, kufDigitalSignature);
    if AKeyUsage[0] and CERT_NON_REPUDIATION_KEY_USAGE <> 0 then
      Include(Result, kufNonRepudiation);
    if AKeyUsage[0] and CERT_KEY_ENCIPHERMENT_KEY_USAGE <> 0 then
      Include(Result, kufKeyEncipherment);
    if AKeyUsage[0] and CERT_DATA_ENCIPHERMENT_KEY_USAGE <> 0 then
      Include(Result, kufDataEncipherment);
    if AKeyUsage[0] and CERT_KEY_AGREEMENT_KEY_USAGE <> 0 then
      Include(Result, kufKeyAgreement);
    if AKeyUsage[0] and CERT_KEY_CERT_SIGN_KEY_USAGE <> 0 then
      Include(Result, kufKeyCertSign);
    if AKeyUsage[0] and CERT_OFFLINE_CRL_SIGN_KEY_USAGE <> 0 then
      Include(Result, kufOfflineCRLSign);
  end;
end;

class function TdxX509Utils.GetCertificateImportFlags
  (AFlags: TdxX509CertificateImportFlags): Integer;
begin
  Result := 0;
  if cifMakeExportable in AFlags then
    Result := Result or 1;
  if cifStrongProtection in AFlags then
    Result := Result or 2;
  if cifMachineKeySet in AFlags then
    Result := Result or $20;
  if cifUserKeySet in AFlags then
    Result := Result or $1000;
end;

class function TdxX509Utils.GetCertificateIssuer
  (AHandle: PCERT_CONTEXT): string;
begin
  Result := GetCertificateNameBlobAsString(AHandle, 1);
end;

class function TdxX509Utils.GetCertificateNameBlobAsString
  (AHandle: PCERT_CONTEXT; const AFlag: Integer): string;
const
  AType = CERT_X500_NAME_STR or CERT_NAME_STR_NO_PLUS_FLAG;
var
  AName: CERT_NAME_BLOB;
  ALength: Integer;
  AResult: array [Word] of WideChar;
begin
  Result := '';
  if AHandle <> nil then
  begin
    case AFlag of
      0:
        AName := AHandle.pCertInfo.Subject;
    else
      AName := AHandle.pCertInfo.Issuer;
    end;
    ALength := CertNameToStr(AHandle.dwCertEncodingType, @AName, AType, nil, 0);
    if ALength > 0 then
    begin
      CertNameToStr(AHandle.dwCertEncodingType, @AName, AType,
        @AResult[0], ALength);
      Result := string(AResult);
    end;
  end;
end;

class function TdxX509Utils.GetCertificateNameString(AHandle: PCERT_CONTEXT;
  AType, AFlags: Cardinal): string;
var
  ALength: Integer;
  AName: array [Word] of WideChar;
begin
  Result := '';
  if AHandle <> nil then
  begin
    ALength := CertGetNameString(AHandle, AType, AFlags, nil, nil, 0);
    if ALength > 0 then
    begin
      CertGetNameString(AHandle, AType, AFlags, nil, @AName[0], ALength);
      Result := string(AName);
    end;
  end;
end;

class function TdxX509Utils.GetCertificatePublicKeyLength
  (AHandle: PCERT_CONTEXT): Integer;
begin
  Result := 0;
  if AHandle <> nil then
    Result := CertGetPublicKeyLength(AHandle^.dwCertEncodingType,
      @AHandle^.pCertInfo.SubjectPublicKeyInfo);
end;

class function TdxX509Utils.GetCertificatePrivateKeyInfo
  (AHandle: PCERT_CONTEXT): string;
var
  AInfo: TBytes;
  ASize: DWORD;
begin
  Result := '';
  ASize := 0;
  if CertGetCertificateContextProperty(AHandle, CERT_KEY_PROV_INFO_PROP_ID, nil,
    @ASize) and (ASize > 0) then
  begin
    SetLength(AInfo, ASize);
    if CertGetCertificateContextProperty(AHandle, CERT_KEY_PROV_INFO_PROP_ID,
      @AInfo[0], @ASize) then
      Result := string(PCRYPT_KEY_PROV_INFO(@AInfo[0]).pwszContainerName)
    else
      TdxX509Utils.RaiseException;
  end;
end;

class function TdxX509Utils.GetCertificateSerialNumber
  (AHandle: PCERT_CONTEXT): string;
var
  I: Integer;
  P: Pointer;
begin
  Result := '';
  for I := AHandle^.pCertInfo.SerialNumber.cbData - 1 downto 0 do
  begin
    P := Pointer(TdxNativeInt(AHandle^.pCertInfo.SerialNumber.pbData) + I);
    Result := Result + IntToHex(Byte(P^), 2);
  end;
end;

class function TdxX509Utils.GetCertificateSubject
  (AHandle: PCERT_CONTEXT): string;
begin
  Result := GetCertificateNameBlobAsString(AHandle, 0);
end;

class function TdxX509Utils.GetCertificateSubjectKeyIdentifier
  (AHandle: PCERT_CONTEXT): string;
var
  ACryptDataBlob: PCRYPT_DATA_BLOB;
  AData: Pointer;
  AExtension: PCERT_EXTENSION;
  ASize: DWORD;
begin
  Result := '';
  if (AHandle <> nil) and (AHandle.pCertInfo <> nil) then
  begin
    AExtension := CertFindExtension(szOID_SUBJECT_KEY_IDENTIFIER,
      AHandle.pCertInfo.cExtension, AHandle.pCertInfo.rgExtension);
    if AExtension <> nil then
    begin
      ASize := 0;
      if CryptDecodeObject(dxX509DefaultEncoding, szOID_SUBJECT_KEY_IDENTIFIER,
        AExtension.Value.pbData, AExtension.Value.cbData, 0, nil, @ASize) then
      begin
        GetMem(AData, ASize);
        try
          CryptDecodeObject(dxX509DefaultEncoding, szOID_SUBJECT_KEY_IDENTIFIER,
            AExtension.Value.pbData, AExtension.Value.cbData, 0, AData, @ASize);
          ACryptDataBlob := PCRYPT_DATA_BLOB(AData);
          Result := dxAnsiStringToString
            (dxBinToHex(PAnsiChar(ACryptDataBlob.pbData),
            ACryptDataBlob.cbData));
        finally
          FreeMem(AData);
        end;
      end;
    end;
  end;
end;

class function TdxX509Utils.GetCertificateThumbprint
  (AHandle: PCERT_CONTEXT): string;
var
  AHash: TBytes;
  ASize: DWORD;
begin
  Result := '';
  ASize := 0;
  if (AHandle <> nil) and CertGetCertificateContextProperty(AHandle,
    CERT_HASH_PROP_ID, nil, @ASize) and (ASize > 0) then
  begin
    SetLength(AHash, ASize);
    CertGetCertificateContextProperty(AHandle, CERT_HASH_PROP_ID,
      @AHash[0], @ASize);
    Result := dxAnsiStringToString(dxBinToHex(PAnsiChar(@AHash[0]), ASize));
  end;
end;

class function TdxX509Utils.GetCertificateUsageFlags
  (AHandle: PCERT_CONTEXT): string;
var
  I: Integer;
  ASize: DWORD;
  AData: TBytes;
  AUsageInfo: PCERT_ENHKEY_USAGE;
  ACurrentChar: PAnsiChar;
  P: Pointer;
begin
  Result := '';
  if AHandle <> nil then
  begin
    ASize := 0;
    if not CertGetEnhancedKeyUsage(AHandle, 0, nil, @ASize) or (ASize = 0) then
    begin
      if GetLastError <> CRYPT_E_NOT_FOUND then
        TdxX509Utils.RaiseException
      else
        Exit;
    end;
    SetLength(AData, ASize);
    CertGetEnhancedKeyUsage(AHandle, 0, PCERT_ENHKEY_USAGE(@AData[0]), @ASize);
    AUsageInfo := PCERT_ENHKEY_USAGE(AData);
    for I := 0 to AUsageInfo.cUsageIdentifier - 1 do
    begin
      P := Pointer(TdxNativeInt(AUsageInfo.rgpszUsageIdentifier) + I *
        SizeOf(Pointer));
      ACurrentChar := PAnsiChar(P^);
      if Length(ACurrentChar) > 0 then
        Result := Result + string(AnsiString(ACurrentChar)) + ',';
    end;
    if Result <> '' then
      SetLength(Result, Length(Result) - 1);
  end;
end;

class function TdxX509Utils.CertificateKeyUsageFlagsToStr
  (AFlags: TdxX509CertificateKeyUsageFlags): string;
begin
  Result := '';
  if kufDigitalSignature in AFlags then
    Result := Result + 'Digital Signature, ';
  if kufNonRepudiation in AFlags then
    Result := Result + 'Non-Repudiation, ';
  if kufKeyEncipherment in AFlags then
    Result := Result + 'Key Encipherment, ';
  if kufDataEncipherment in AFlags then
    Result := Result + 'Data Encipherment, ';
  if kufKeyAgreement in AFlags then
    Result := Result + 'Key Agreement, ';
  if kufKeyCertSign in AFlags then
    Result := Result + 'Certificate Signing, ';
  if kufOfflineCRLSign in AFlags then
    Result := Result + 'Off-line CRL Signing, ';
  if Result <> '' then
    SetLength(Result, Length(Result) - 2);
end;

class function TdxX509Utils.CertificateVersionToStr(AVersion: DWORD): string;
begin
  Result := 'V' + IntToStr(AVersion + 1);
end;

class function TdxX509Utils.StoreLocationToStr
  (ALocation: TdxX509StoreLocation): string;
begin
  if ALocation = slCurrentUser then
    Result := 'CurrentUser'
  else
    Result := 'LocalMachine';
end;

class function TdxX509Utils.StoreNameToStr(AName: TdxX509StoreName): string;
begin
  case AName of
    snAddressBook:
      Result := 'AddressBook';
    snAuthRoot:
      Result := 'AuthRoot';
    snDisallowed:
      Result := 'Disallowed';
    snRoot:
      Result := 'Root';
    snTrustedPeople:
      Result := 'TrustedPeople';
    snTrustedPublisher:
      Result := 'TrustedPublisher';
  else
    Result := 'My';
  end;
end;

class function TdxX509Utils.LoadStore(AStream: TStream; const APassword: string;
  out AHandle: HCERTSTORE): Boolean;
var
  AData: TBytes;
  APFX: CRYPT_DATA_BLOB;
begin
  SetLength(AData, AStream.Size);
  AStream.Read(AData[0], AStream.Size);
  APFX.cbData := AStream.Size;
  APFX.pbData := @AData[0];
  AHandle := PFXImportCertStore(@APFX, PWideChar(APassword),
    GetCertificateImportFlags([cifMakeExportable]));
  Result := AHandle <> nil;
end;

class procedure TdxX509Utils.AddCertificateFromStore(AHandle: HCERTSTORE;
  ADestination: TdxX509CertificateList);
var
  ACertificateContext: PCERT_CONTEXT;
begin
  ACertificateContext := nil;
  repeat
    ACertificateContext := CertEnumCertificatesInStore(AHandle,
      ACertificateContext);
    if ACertificateContext <> nil then
      ADestination.Add(TdxX509Certificate.Create(ACertificateContext));
  until ACertificateContext = nil;
end;

class procedure TdxX509Utils.RaiseException;
begin
  raise EdxX509Exception.CreateFmt('%d', [GetLastError]);
end;

class function TdxX509Utils.DisplayCertificate(ACertificate: TdxX509Certificate;
  AParentWnd: THandle): Boolean;
var
  AChanged: Boolean;
  AViewInfo: CRYPTUI_VIEWCERTIFICATE_STRUCT;
begin
  Result := ACertificate <> nil;
  if Result then
  begin
    FillChar(AViewInfo, SizeOf(CRYPTUI_VIEWCERTIFICATE_STRUCT), 0);
    AViewInfo.dwSize := SizeOf(CRYPTUI_VIEWCERTIFICATE_STRUCT);
    AViewInfo.pCertContext := CertDuplicateCertificateContext
      (ACertificate.Handle);
    AViewInfo.hwndParent := AParentWnd;
    Result := CryptUIDlgViewCertificate(@AViewInfo, @AChanged);
  end;
end;

class function TdxX509Utils.SelectCertificate(AStore: TdxX509Store;
  const ATitle, AMessage: string; AParentWnd: THandle;
  out ACertificate: TdxX509Certificate): Boolean;
var
  ASelectedCertificateList: TdxX509CertificateList;
begin
  ACertificate := nil;
  ASelectedCertificateList := TdxX509CertificateList.Create;
  try
    Result := SelectFromStore(AStore, ATitle, AMessage, False, AParentWnd,
      ASelectedCertificateList);
    if Result then
      ACertificate := TdxX509Certificate.Create(ASelectedCertificateList.First)
  finally
    ASelectedCertificateList.Free;
  end;
end;

class function TdxX509Utils.SelectFromStore(AStore: TdxX509Store;
  const ATitle, AMessage: string; AMultiSelect: Boolean; AParentWnd: THandle;
  ASelectedCertificateList: TdxX509CertificateList): Boolean;
var
  ASourceStoreContext, ASelectedCertificateStoreContext: HCERTSTORE;
  ASelectedCertificateContext: HCERTSTORE;
  AViewInfo: CRYPTUI_SELECTCERTIFICATE_STRUCT;
  ASavedCount: Integer;
begin
  ASourceStoreContext := AStore.Handle;
  ASelectedCertificateStoreContext := CertOpenStore(CERT_STORE_PROV_MEMORY,
    dxX509DefaultEncoding, 0, 0, nil);
  try
    FillChar(AViewInfo, SizeOf(CRYPTUI_SELECTCERTIFICATE_STRUCT), 0);
    AViewInfo.dwSize := SizeOf(CRYPTUI_SELECTCERTIFICATE_STRUCT);
    AViewInfo.hwndParent := AParentWnd;
    AViewInfo.dwFlags := Cardinal(AMultiSelect);
    AViewInfo.szTitle := PAnsiString(ATitle);
    AViewInfo.szTitle := PAnsiString(ATitle);
    AViewInfo.szDisplayString := PAnsiString(AMessage);
    AViewInfo.cDisplayStores := 1;
    AViewInfo.rghDisplayStores := @ASourceStoreContext;
    AViewInfo.hSelectedCertStore := @ASelectedCertificateStoreContext;
    ASelectedCertificateContext := CryptUIDlgSelectCertificate(@AViewInfo);
    if AMultiSelect then
    begin
      ASavedCount := ASelectedCertificateList.Count;
      TdxX509Utils.AddCertificateFromStore(ASelectedCertificateStoreContext,
        ASelectedCertificateList);
      Result := ASavedCount < ASelectedCertificateList.Count;
    end
    else
    begin
      Result := ASelectedCertificateContext <> nil;
      if Result then
        ASelectedCertificateList.Add
          (TdxX509Certificate.Create(ASelectedCertificateContext));
      CertFreeCertificateContext(ASelectedCertificateContext);
    end;
  finally
    CertCloseStore(ASelectedCertificateStoreContext, 0);
  end;
end;

{ TdxX509Certificate }

constructor TdxX509Certificate.Create;
begin
  Create(nil);
end;

constructor TdxX509Certificate.Create(AHandle: PCERT_CONTEXT);
begin
  inherited Create;
  FHandle := AHandle;
  if FHandle <> nil then
  begin
    FHandle := CertDuplicateCertificateContext(AHandle);
    CryptCheck(FHandle <> nil);

    FKeyUsage := [];
    FPublicKeyAlgorithm :=
      string(Handle^.pCertInfo.SubjectPublicKeyInfo.Algorithm.pszObjId);
    FSignatureAlgorithm :=
      string(Handle^.pCertInfo.SignatureAlgorithm.pszObjId);
    FSerialNumber := TdxX509Utils.GetCertificateSerialNumber(Handle);
    FValidFrom := cxFileTimeToDateTime(Handle^.pCertInfo.NotBefore);
    FValidTo := cxFileTimeToDateTime(Handle^.pCertInfo.NotAfter);
    FVersion := Handle^.pCertInfo.dwVersion;
  end;
end;

constructor TdxX509Certificate.Create(const AFileName, APassword: string);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Create(AStream, APassword);
    FFileName := AFileName;
  finally
    AStream.Free;
  end;
end;

constructor TdxX509Certificate.Create(ACertificate: TdxX509Certificate);
begin
  Create(ACertificate.Handle);
  FFileName := ACertificate.FileName;
end;

constructor TdxX509Certificate.Create(AStream: TStream;
  const APassword: string);
var
  AStore: TdxX509Store;
begin
  AStore := TdxX509Store.Create;
  try
    AStore.Certificates.Import(AStream, PWideChar(APassword));
    if AStore.Certificates.Count > 0 then
      Create(AStore.Certificates.Last)
    else
      Create;
  finally
    AStore.Free;
  end;
end;

destructor TdxX509Certificate.Destroy;
begin
  CryptCheck(CertFreeCertificateContext(FHandle));
  FHandle := nil;
  inherited Destroy;
end;

function TdxX509Certificate.GetFriendlyName: string;
begin
  if FFriendlyName.IsNullOrEmpty then
    FFriendlyName := TdxX509Utils.GetCertificateFriendlyName(Handle);
  Result := FFriendlyName.Value;
end;

function TdxX509Certificate.GetHasPrivateKey: Boolean;
begin
  Result := PrivateKeyInfo <> '';
end;

function TdxX509Certificate.GetIssuedBy: string;
begin
  if FIssuedBy.IsNull then
    FIssuedBy := TdxX509Utils.GetCertificateNameString(Handle,
      CERT_NAME_SIMPLE_DISPLAY_TYPE, CERT_NAME_ISSUER_FLAG);
  Result := FIssuedBy.Value;
end;

function TdxX509Certificate.GetIssuedTo: string;
begin
  if FIssuedTo.IsNull then
    FIssuedTo := TdxX509Utils.GetCertificateNameString(Handle,
      CERT_NAME_SIMPLE_DISPLAY_TYPE, 0);
  Result := FIssuedTo.Value;
end;

function TdxX509Certificate.GetIssuer: string;
begin
  if FIssuer.IsNull then
    FIssuer := TdxX509Utils.GetCertificateIssuer(Handle);
  Result := FIssuer.Value;
end;

function TdxX509Certificate.GetKeyUsage: TdxX509CertificateKeyUsageFlags;
begin
  if FKeyUsage = [] then
    FKeyUsage := TdxX509Utils.GetCertificateGetKeyUsage(Handle);
  Result := FKeyUsage;
end;

function TdxX509Certificate.GetOIDInfo(const AOID: string): string;
var
  PInfo: PCRYPT_OID_INFO;
begin
  Result := AOID;
  PInfo := CryptFindOIDInfo(CRYPT_OID_INFO_OID_KEY,
    PAnsiChar(AnsiString(AOID)), 0);
  if PInfo <> nil then
    Result := string(WideString(PInfo.pwszName));
end;

function TdxX509Certificate.GetPrivateKeyInfo: string;
begin
  if FPrivateKeyInfo.IsNullOrEmpty then
    FPrivateKeyInfo := TdxX509Utils.GetCertificatePrivateKeyInfo(Handle);
  Result := FPrivateKeyInfo.Value;
end;

function TdxX509Certificate.GetPublicKeyAlgorithmName: string;
begin
  if FPublicKeyAlgorithmName.IsNull then
    FPublicKeyAlgorithmName := GetOIDInfo(PublicKeyAlgorithm);
  Result := FPublicKeyAlgorithmName.Value;
end;

function TdxX509Certificate.GetPublicKeyLength: Integer;
begin
  if FPublicKeyLength.IsNull then
    FPublicKeyLength := TdxX509Utils.GetCertificatePublicKeyLength(Handle);
  Result := FPublicKeyLength.Value;
end;

function TdxX509Certificate.GetSignatureAlgorithmName: string;
begin
  if FSignatureAlgorithmName.IsNull then
    FSignatureAlgorithmName := GetOIDInfo(SignatureAlgorithm);
  Result := FSignatureAlgorithmName.Value;
end;

function TdxX509Certificate.GetSubject: string;
begin
  if FSubject.IsNull then
    FSubject := TdxX509Utils.GetCertificateSubject(Handle);
  Result := FSubject.Value;
end;

function TdxX509Certificate.GetSubjectKeyIdentifier: string;
begin
  if FSubjectKeyIdentifier.IsNull then
    FSubjectKeyIdentifier :=
      TdxX509Utils.GetCertificateSubjectKeyIdentifier(Handle);
  Result := FSubjectKeyIdentifier.Value;
end;

function TdxX509Certificate.GetThumbprint: string;
begin
  if FThumbprint.IsNull then
    FThumbprint := TdxX509Utils.GetCertificateThumbprint(Handle);
  Result := FThumbprint.Value;
end;

function TdxX509Certificate.GetUsageFlags: string;
begin
  if FUsageFlags.IsNull then
    FUsageFlags := TdxX509Utils.GetCertificateUsageFlags(Handle);
  Result := FUsageFlags.Value;
end;

{ TdxX509CertificateList }

procedure TdxX509CertificateList.CopyTo(AList: TdxX509CertificateList);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    AList.Add(TdxX509Certificate.Create(Items[I]));
end;

procedure TdxX509CertificateList.Import(const AFileName, APassword: string);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    Import(AStream, APassword);
  finally
    AStream.Free;
  end;
end;

procedure TdxX509CertificateList.Import(AStream: TStream;
  const APassword: string);
var
  AStore: TdxX509Store;
  AHandle: HCERTSTORE;
begin
  AStore := TdxX509Store.Create;
  try
    if TdxX509Utils.LoadStore(AStream, APassword, AHandle) then
    begin
      AStore.Handle := AHandle;
      AStore.PopulateCertificateList;
      AStore.Certificates.CopyTo(Self);
    end
    else
      TdxX509Utils.RaiseException;
  finally
    AStore.Free;
  end;
end;

{ TdxX509Store }

constructor TdxX509Store.Create;
begin
  Create(slCurrentUser);
end;

constructor TdxX509Store.Create(ALocation: TdxX509StoreLocation);
begin
  Create(snMy, ALocation);
end;

constructor TdxX509Store.Create(AName: TdxX509StoreName);
begin
  Create(AName, slCurrentUser);
end;

constructor TdxX509Store.Create(AName: TdxX509StoreName;
  ALocation: TdxX509StoreLocation);
begin
  inherited Create;
  FMode := smReadOnly;
  FLocation := ALocation;
  FName := AName;
  FHandle := nil;
  FCertificateList := TdxX509CertificateList.Create;
end;

destructor TdxX509Store.Destroy;
begin
  Handle := nil;
  FreeAndNil(FCertificateList);
  inherited Destroy;
end;

procedure TdxX509Store.Close;
begin
  if HandleAllocated then
  begin
    FCertificateList.Clear;
    CryptCheck(CertCloseStore(FHandle, 0));
    FHandle := nil;
  end;
end;

procedure TdxX509Store.Open;

  function GetOpenFlags: Cardinal;
  var
    AActualMode: Cardinal;
  begin
    Result := 0;
    AActualMode := Word(Mode) and (Word(smReadWrite) or Word(smMaxAllowed));
    if AActualMode = 0 then
      Result := Result or 32768
    else if AActualMode = 2 then
      Result := Result or 4096;

    if (Word(Mode) and Word(smOpenExistingOnly)) = Word(smOpenExistingOnly) then
      Result := Result or 16384;
    if (Word(Mode) and Word(smIncludeArchived)) = Word(smIncludeArchived) then
      Result := Result or 512;
    if Location = slLocalMachine then
      Result := Result or 131072
    else if Location = slCurrentUser then
      Result := Result or 65536;
  end;

begin
  SetMode(smReadOnly);
  FHandle := CertOpenStore(CERT_STORE_PROV_SYSTEM, dxX509DefaultEncoding, 0,
    GetOpenFlags, PAnsiString(TdxX509Utils.StoreNameToStr(Name)));
  CryptCheck(FHandle <> nil);
  CryptCheck(CertControlStore(Handle, 0, 4, nil));
  PopulateCertificateList;
end;

function TdxX509Store.GetHandle: HCERTSTORE;
begin
  if not HandleAllocated then
    Open;
  Result := FHandle;
end;

function TdxX509Store.HandleAllocated: Boolean;
begin
  Result := Assigned(FHandle);
end;

procedure TdxX509Store.PopulateCertificateList;
begin
  TdxX509Utils.AddCertificateFromStore(Handle, Certificates);
end;

procedure TdxX509Store.SetHandle(AValue: HCERTSTORE);
begin
  Close;
  FHandle := AValue;
end;

procedure TdxX509Store.SetMode(AValue: TdxX509StoreMode);
begin
  if FMode <> AValue then
  begin
    Close;
    FMode := AValue;
  end;
end;

end.
