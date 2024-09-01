{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressPDFViewer }
{ }
{ Copyright (c) 2015-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSPDFVIEWER AND ALL }
{ ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM }
{ ONLY. }
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

unit dxPDFEncryption;

{$I cxVer.inc}

interface

uses
  SysUtils, Windows, dxCore, dxCrypto, dxProtectionUtils, dxPDFBase, dxPDFTypes;

type
  TdxPDFCryptMethodVersion = (cmvV2, cmvAESV2, cmvAESV3);
  TdxPDFPasswordCheckingResult = (pcrContinue, pcrSuccess, pcrExit);

  TdxPDFDocumentPermission = (pdpAllowPrint, pdpAllowPrintHighResolution,
    pdpAllowCopyContent, pdpAllowEditContent, pdpAllowExtractContent,
    pdpAllowAddOrModifyTextAnnotations, pdpAllowFillFields,
    pdpAllowAssembleDocument);
  TdxPDFDocumentPermissions = set of TdxPDFDocumentPermission;

  TdxPDFEncryptionAlgorithmType = (eatRC40Bit, eatRC128Bit, eatAES);

  { TdxPDFCustomEncryptionAlgorithm }

  TdxPDFCustomEncryptionAlgorithmClass = class of
    TdxPDFCustomEncryptionAlgorithm;

  TdxPDFCustomEncryptionAlgorithm = class
  strict private
  type
    TPermissionsMaskMap = array [TdxPDFDocumentPermission] of Cardinal;
  strict private
  const
    PermissionsMapR2: TPermissionsMaskMap = (4, 4, 16, 8, 16, 32, 32, 8);
    PermissionsMapR3: TPermissionsMaskMap = (4, 2048, 16, 8, 512, 32,
      256, 1024);
  strict private
    FPermissions: TdxPDFDocumentPermissions;
    FCryptMethodVersion: TdxPDFCryptMethodVersion;
    FDocumentID: TdxPDFDocumentID;
    FEncryptMetadata: Boolean;
    FIsOwnerKey: Boolean;
    FPasswordPadding: TBytes;
    FStreamFilterName: string;

    function CalculateMD5Hash(const AKey: TBytes): TBytes;
    function CalculateOwnerEncryptionKey(const APassword: TBytes): TBytes;
    function CalculateOwnerPasswordHash(const AOwnerPassword,
      AUserPassword: TBytes): TBytes;
    function CalculateUserPasswordHash(const APassword: TBytes): TBytes;
    function GetActualKeyLength: Integer;
    function GetExtendedKeyLength: Integer;
    function IsExistFilter(ADictionary: TdxPDFDictionary;
      const AName: string): Boolean;
    function PadOrTruncatePassword(const APasswordString: TBytes): TBytes;
    function ValidateHash(const AHash: TBytes): TBytes;
    function XorKey(const AKey: TBytes; AValue: Integer): TBytes;
    procedure UpdateEncryptionKey(const AKey: TBytes);
  strict protected
  const
    HashSize = 32;
  strict protected
    FEncodedOwnerPassword: TBytes;
    FEncodedUserPassword: TBytes;
    FEncryptedPermissions: TBytes;
    FEncryptionFlags: Cardinal;
    FEncryptionKey: TBytes;
    FKeyLength: Integer;
    FOwnerPasswordHash: TBytes;
    FSecurityHandlerRevision: Integer;
    FUserPasswordHash: TBytes;

    function CheckOwnerPassword(const APassword: TBytes): Boolean; virtual;
    function CheckUserPassword(const AUserPassword: TBytes): Boolean; virtual;

    // Crypt Filter
    procedure ReadCryptFilter(ADictionary: TdxPDFDictionary);
    function ReadCryptFilterKeyLength(ADictionary: TdxPDFDictionary): Integer;
    procedure ReadCryptMethodVersion(ADictionary: TdxPDFDictionary);

    function ConvertToBytes(const APassword: string): TBytes; virtual;
    function DoDecodePermissions(AFlags: Cardinal)
      : TdxPDFDocumentPermissions; virtual;
    function DoEncodePermissions(AActions: TdxPDFDocumentPermissions)
      : Cardinal; virtual;
    function DoUpdateEncryptionKey: TBytes; virtual;
    function DoValidateHash(const AHash: TBytes): TBytes; virtual;
    procedure Initialize(const ADocumentID: TdxPDFDocumentID); virtual;
    procedure InitializePassword(var APassword: TBytes); virtual;
    function ValidatePassword(const APasswordString: string): Boolean; virtual;

    function CalculateEncryptionKey(ANumber: Integer): TBytes;
    function CheckPassword(const AExpectedHash, AActualHash: TBytes)
      : Boolean; overload;
    function RC4Crypt(const AKey, AData: TBytes; ADataSize: Integer): TBytes;
  protected
    // Read / Write
    procedure Read(ADictionary: TdxPDFDictionary); virtual;
    function ReadKeyLength(ADictionary: TdxPDFDictionary): Integer; virtual;
    procedure Write(ADictionary: TdxPDFDictionary); virtual;

    property ActualKeyLength: Integer read GetActualKeyLength;
    property CryptMethodVersion: TdxPDFCryptMethodVersion
      read FCryptMethodVersion;
    property EncryptionFlags: Cardinal read FEncryptionFlags;
    property EncryptionKey: TBytes read FEncryptionKey write FEncryptionKey;
    property EncryptMetadata: Boolean read FEncryptMetadata;
    property ExtendedKeyLength: Integer read GetExtendedKeyLength;
    property IsOwnerKey: Boolean read FIsOwnerKey;
    property SecurityHandlerRevision: Integer read FSecurityHandlerRevision;
    property StreamFilterName: string read FStreamFilterName;
  public
    constructor Create(const ADocumentID: TdxPDFDocumentID;
      ADictionary: TdxPDFDictionary); overload;
    constructor Create(const ADocumentID: TdxPDFDocumentID;
      const AUserPassword, AOwnerPassword: string;
      const APermissions: TdxPDFDocumentPermissions); overload; virtual;
    class function GetType: TdxPDFEncryptionAlgorithmType; virtual;
    class function GetVersion: Integer; virtual;

    function CheckPassword(AAttemptLimit: Integer;
      AOnGetPasswordEvent: TdxGetPasswordEvent): Boolean; overload;
    function Decrypt(const AData: TBytes; ANumber: Integer): TBytes; virtual;
    function Encrypt(const AData: TBytes; ANumber: Integer): TBytes; virtual;

    property Permissions: TdxPDFDocumentPermissions read FPermissions;
  end;

  { TdxPDFAESBasedEncryptionAlgorithm }

  TdxPDFAESBasedEncryptionAlgorithm = class(TdxPDFCustomEncryptionAlgorithm)
  private const
    InitVectorSize = 16;
    SaltSize = 8;
  private
    FOwnerKeySalt: TBytes;
    FOwnerValidationSalt: TBytes;
    FUserKeySalt: TBytes;
    FUserValidationSalt: TBytes;

    function CalculateHash(const APasswordString, AData,
      AUserKey: TBytes): TBytes;
    function DoAESDecrypt(const AKey, AInitVector, AData: TBytes;
      APosition, AChainingMode, APaddingMode: Integer): TBytes;
    function DoAESEncrypt(const AKey, AInitVector, AData: TBytes): TBytes;
  protected
    function DoUpdateEncryptionKey: TBytes; override;
    function DoValidateHash(const AHash: TBytes): TBytes; override;

    function AESDecrypt(const AKey, AData: TBytes): TBytes; overload;
    function AESDecrypt(const AKey, AData: TBytes; AChainingMode: Integer)
      : TBytes; overload;
    function CreateAESCryptoKey(const AKey, AInitVector: TBytes): IdxCryptoKey;
  public
    constructor Create(const ADocumentID: TdxPDFDocumentID;
      const AUserPassword, AOwnerPassword: string;
      const APermissions: TdxPDFDocumentPermissions); override;
    class function GetType: TdxPDFEncryptionAlgorithmType; override;
  end;

  { TdxPDFMixedEncryptionAlgorithm }

  TdxPDFMixedEncryptionAlgorithm = class(TdxPDFAESBasedEncryptionAlgorithm)
  protected
    procedure Read(ADictionary: TdxPDFDictionary); override;
    procedure Write(ADictionary: TdxPDFDictionary); override;
  public
    class function GetVersion: Integer; override;

    function Decrypt(const AData: TBytes; ANumber: Integer): TBytes; override;
    function Encrypt(const AData: TBytes; ANumber: Integer): TBytes; override;
  end;

  { TdxPDFAESEncryptionAlgorithm }

  TdxPDFAESEncryptionAlgorithm = class(TdxPDFAESBasedEncryptionAlgorithm)
  strict private
    function CheckPermissions: Boolean;
  protected
    function CheckOwnerPassword(const AUserPassword: TBytes): Boolean; override;
    function CheckUserPassword(const AUserPassword: TBytes): Boolean; override;
    function ConvertToBytes(const APassword: string): TBytes; override;
    procedure InitializePassword(var APassword: TBytes); override;
    function ValidatePassword(const APasswordString: string): Boolean; override;

    procedure Read(ADictionary: TdxPDFDictionary); override;
    procedure Write(ADictionary: TdxPDFDictionary); override;
  public
    class function GetVersion: Integer; override;

    function Decrypt(const AData: TBytes; ANumber: Integer): TBytes; override;
    function Encrypt(const AData: TBytes; ANumber: Integer): TBytes; override;
  end;

  { TdxPDF40BitEncryptionAlgorithm }

  TdxPDF40BitEncryptionAlgorithm = class(TdxPDFCustomEncryptionAlgorithm)
  protected
    procedure Initialize(const ADocumentID: TdxPDFDocumentID); override;
    function ReadKeyLength(ADictionary: TdxPDFDictionary): Integer; override;
  public
    class function GetType: TdxPDFEncryptionAlgorithmType; override;
    class function GetVersion: Integer; override;
  end;

  { TdxPDFGreater40BitEncryptionAlgorithm }

  TdxPDFGreater40BitEncryptionAlgorithm = class(TdxPDFCustomEncryptionAlgorithm)
  protected
    procedure Initialize(const ADocumentID: TdxPDFDocumentID); override;
    procedure Read(ADictionary: TdxPDFDictionary); override;
  public
    class function GetType: TdxPDFEncryptionAlgorithmType; override;
    class function GetVersion: Integer; override;
  end;

  { TdxPDFEncryptionAlgorithms }

  TdxPDFEncryptionAlgorithms = class
  strict private
    class function GetAlgorithmClass(AType: TdxPDFEncryptionAlgorithmType)
      : TdxPDFCustomEncryptionAlgorithmClass; overload;
    class function GetAlgorithmClass(AVersion: Integer)
      : TdxPDFCustomEncryptionAlgorithmClass; overload;
  public
    class function Create(ADictionary: TdxPDFDictionary;
      const ADocumentID: TdxPDFDocumentID)
      : TdxPDFCustomEncryptionAlgorithm; overload;
    class function Create(const ADocumentID: TdxPDFDocumentID;
      AType: TdxPDFEncryptionAlgorithmType;
      const AUserPassword, AOwnerPassword: string;
      const APermissions: TdxPDFDocumentPermissions)
      : TdxPDFCustomEncryptionAlgorithm; overload;
  end;

  { TdxPDFEncryptionInfo }

  TdxPDFEncryptionInfo = class(TdxPDFBase, IUnknown, IdxPDFEncryptionInfo)
  strict private
    FAlgorithm: TdxPDFCustomEncryptionAlgorithm;
    FLock: TRtlCriticalSection;

    function GetAllowContentExtraction: Boolean;
    function GetAllowPrinting: Boolean;
    function TestPermissions(APermissions: TdxPDFDocumentPermissions): Boolean;
    // IdxPDFEncryptionInfo
    function EncryptMetadata: Boolean;
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  protected
    procedure Write(ADictionary: TdxPDFDictionary); reintroduce;
  public
    constructor Create(const ADocumentID: TdxPDFDocumentID;
      ADictionary: TdxPDFDictionary); overload;
    constructor Create(const ADocumentID: TdxPDFDocumentID;
      AType: TdxPDFEncryptionAlgorithmType;
      const AUserPassword, AOwnerPassword: string;
      const APermissions: TdxPDFDocumentPermissions); overload;
    destructor Destroy; override;
    procedure AfterConstruction; override;

    function CheckPassword(AAttemptsLimit: Integer;
      AOnGetPasswordEvent: TdxGetPasswordEvent): Boolean;
    function Decrypt(const AData: TBytes; ANumber: Integer): TBytes;
    function Encrypt(const AData: TBytes; ANumber: Integer): TBytes;

    property Algorithm: TdxPDFCustomEncryptionAlgorithm read FAlgorithm;
    property AllowContentExtraction: Boolean read GetAllowContentExtraction;
    property AllowPrinting: Boolean read GetAllowPrinting;
  end;

implementation

uses
  Math, dxHash, dxCryptoAPI, dxPDFUtils, dxPDFDocumentStrs;

const
  PasswordPadding: array [0 .. 31] of Byte = ($28, $BF, $4E, $5E, $4E, $75, $8A,
    $41, $64, $00, $4E, $56, $FF, $FA, $01, $08, $2E, $2E, $00, $B6, $D0, $68,
    $3E, $80, $2F, $0C, $A9, $FE, $64, $53, $69, $7A);

  { TdxPDFCustomEncryptionAlgorithm }

constructor TdxPDFCustomEncryptionAlgorithm.Create(const ADocumentID
  : TdxPDFDocumentID; ADictionary: TdxPDFDictionary);
begin
  inherited Create;
  Initialize(ADocumentID);
  Read(ADictionary);
end;

constructor TdxPDFCustomEncryptionAlgorithm.Create(const ADocumentID
  : TdxPDFDocumentID; const AUserPassword, AOwnerPassword: string;
  const APermissions: TdxPDFDocumentPermissions);
var
  AOwnerPasswordBytes: TBytes;
  AUserPasswordBytes: TBytes;
begin
  // do not change the order
  Initialize(ADocumentID);
  FPermissions := APermissions;
  FEncryptionFlags := DoEncodePermissions(FPermissions);
  AOwnerPasswordBytes := ConvertToBytes(AOwnerPassword);
  AUserPasswordBytes := ConvertToBytes(AUserPassword);
  FOwnerPasswordHash := CalculateOwnerPasswordHash(AOwnerPasswordBytes,
    AUserPasswordBytes);
  FUserPasswordHash := CalculateUserPasswordHash(AUserPasswordBytes);
end;

class function TdxPDFCustomEncryptionAlgorithm.GetType
  : TdxPDFEncryptionAlgorithmType;
begin
  raise Exception.Create('');
end;

class function TdxPDFCustomEncryptionAlgorithm.GetVersion: Integer;
begin
  Result := dxPDFInvalidValue;
end;

procedure TdxPDFCustomEncryptionAlgorithm.Read(ADictionary: TdxPDFDictionary);
begin
  FSecurityHandlerRevision := ADictionary.GetInteger(TdxPDFKeywords.Revision);
  FUserPasswordHash :=
    ValidateHash(ADictionary.GetBytes(TdxPDFKeywords.UserPasswordHash));
  FEncryptionFlags :=
    Cardinal(ADictionary.GetInteger(TdxPDFKeywords.Permissions));
  FEncryptMetadata := ADictionary.GetBoolean
    (TdxPDFKeywords.EncryptMetadata, True);
  FKeyLength := ReadKeyLength(ADictionary);
  FOwnerPasswordHash :=
    ValidateHash(ADictionary.GetBytes(TdxPDFKeywords.OwnerPasswordHash));

  FPermissions := DoDecodePermissions(FEncryptionFlags);
end;

procedure TdxPDFCustomEncryptionAlgorithm.Write(ADictionary: TdxPDFDictionary);
begin
  ADictionary.Add(TdxPDFKeywords.Version, GetVersion);
  ADictionary.Add(TdxPDFKeywords.Revision, FSecurityHandlerRevision);
  ADictionary.AddName(TdxPDFKeywords.Filter, TdxPDFKeywords.Standard);
  ADictionary.Add(TdxPDFKeywords.OwnerPasswordHash,
    TdxPDFSpecialBytes.Create(FOwnerPasswordHash));
  ADictionary.Add(TdxPDFKeywords.UserPasswordHash,
    TdxPDFSpecialBytes.Create(FUserPasswordHash));
  ADictionary.Add(TdxPDFKeywords.EncryptMetadata, EncryptMetadata, True);
  ADictionary.Add(TdxPDFKeywords.Permissions, Integer(FEncryptionFlags));
  ADictionary.Add(TdxPDFKeywords.Length, FKeyLength * 8);
end;

function TdxPDFCustomEncryptionAlgorithm.CheckPassword(AAttemptLimit: Integer;
  AOnGetPasswordEvent: TdxGetPasswordEvent): Boolean;
var
  AAttemptCount: Integer;
  APassword: string;
  APasswordBytes: TBytes;
begin
  InitializePassword(APasswordBytes);
  FIsOwnerKey := CheckOwnerPassword(APasswordBytes);
  Result := CheckUserPassword(APasswordBytes);
  if not Result then
  begin
    AAttemptCount := 0;
    while (AAttemptLimit = 0) or (AAttemptLimit > 0) and
      (AAttemptCount < AAttemptLimit) do
    begin
      if dxCallGetPasswordEvent(AOnGetPasswordEvent, Self, APassword) then
      begin
        if ValidatePassword(APassword) then
          Exit(True);
      end
      else
        Exit(False);
      Inc(AAttemptCount);
    end;
  end;
end;

function TdxPDFCustomEncryptionAlgorithm.Decrypt(const AData: TBytes;
  ANumber: Integer): TBytes;
begin
  Result := RC4Crypt(CalculateEncryptionKey(ANumber), AData, Length(AData));
end;

function TdxPDFCustomEncryptionAlgorithm.Encrypt(const AData: TBytes;
  ANumber: Integer): TBytes;
begin
  Result := RC4Crypt(CalculateEncryptionKey(ANumber), AData, Length(AData));
end;

function TdxPDFCustomEncryptionAlgorithm.CheckOwnerPassword(const APassword
  : TBytes): Boolean;
var
  I: Integer;
  AUserPasswordString: TBytes;
  AKey: TBytes;
begin
  AKey := CalculateOwnerEncryptionKey(APassword);
  if FSecurityHandlerRevision >= 3 then
  begin
    TdxPDFUtils.AddData(FOwnerPasswordHash, AUserPasswordString);
    for I := 19 downto 0 do
      AUserPasswordString := RC4Crypt(XorKey(AKey, I), AUserPasswordString,
        Length(AUserPasswordString));
  end
  else
    AUserPasswordString := RC4Crypt(AKey, FOwnerPasswordHash,
      Length(FOwnerPasswordHash));
  Result := CheckUserPassword(AUserPasswordString);
end;

function TdxPDFCustomEncryptionAlgorithm.CheckUserPassword(const AUserPassword
  : TBytes): Boolean;
begin
  Result := CheckPassword(FUserPasswordHash,
    CalculateUserPasswordHash(AUserPassword))
end;

function TdxPDFCustomEncryptionAlgorithm.ReadKeyLength
  (ADictionary: TdxPDFDictionary): Integer;
begin
  Result := ADictionary.GetInteger(TdxPDFKeywords.Length, 40);
  if Result mod 8 <> 0 then
    TdxPDFUtils.RaiseTestException;
  Result := Result div 8;
end;

function TdxPDFCustomEncryptionAlgorithm.ConvertToBytes(const APassword
  : string): TBytes;
begin
  Result := PadOrTruncatePassword(TEncoding.ASCII.GetBytes(APassword))
end;

function TdxPDFCustomEncryptionAlgorithm.DoDecodePermissions(AFlags: Cardinal)
  : TdxPDFDocumentPermissions;

  function DoDecodePermissions(AFlags: Cardinal;
    const AMap: TPermissionsMaskMap): TdxPDFDocumentPermissions;
  var
    AAction: TdxPDFDocumentPermission;
  begin
    Result := [];
    for AAction := Low(AAction) to High(AAction) do
    begin
      if AFlags and AMap[AAction] <> 0 then
        Include(Result, AAction);
    end;
  end;

begin
  if FSecurityHandlerRevision >= 3 then
    Result := DoDecodePermissions(AFlags, PermissionsMapR3)
  else if FSecurityHandlerRevision = 2 then
    Result := DoDecodePermissions(AFlags, PermissionsMapR2)
  else
    Result := [];
end;

function TdxPDFCustomEncryptionAlgorithm.DoEncodePermissions
  (AActions: TdxPDFDocumentPermissions): Cardinal;

  function DoEncodePermissions(ABase: Cardinal; const AMap: TPermissionsMaskMap)
    : Cardinal;
  var
    AAction: TdxPDFDocumentPermission;
  begin
    Result := ABase;
    for AAction := Low(AAction) to High(AAction) do
    begin
      if AAction in AActions then
        Result := Result or AMap[AAction];
    end;
  end;

begin
  if FSecurityHandlerRevision >= 3 then
    Result := DoEncodePermissions($FFFFF0C0, PermissionsMapR3)
  else if FSecurityHandlerRevision = 2 then
    Result := DoEncodePermissions($FFFFFFC0, PermissionsMapR2)
  else
    Result := 0;
end;

function TdxPDFCustomEncryptionAlgorithm.DoUpdateEncryptionKey: TBytes;
begin
  Result := TdxByteArray.Resize(FEncryptionKey, ExtendedKeyLength);
end;

function TdxPDFCustomEncryptionAlgorithm.DoValidateHash
  (const AHash: TBytes): TBytes;
begin
  if Length(AHash) <> HashSize then
    TdxPDFUtils.Abort
  else
    Result := AHash;
end;

function TdxPDFCustomEncryptionAlgorithm.ValidatePassword(const APasswordString
  : string): Boolean;
var
  APassword: TBytes;
begin
  APassword := ConvertToBytes(APasswordString);
  if CheckOwnerPassword(APassword) then
  begin
    FIsOwnerKey := True;
    Result := True;
  end
  else
    Result := CheckUserPassword(APassword);
end;

procedure TdxPDFCustomEncryptionAlgorithm.Initialize(const ADocumentID
  : TdxPDFDocumentID);
begin
  FEncryptMetadata := True;
  FDocumentID[0] := ADocumentID[0];
  FDocumentID[1] := ADocumentID[1];

  SetLength(FPasswordPadding, Length(PasswordPadding));
  cxCopyData(@PasswordPadding[0], @FPasswordPadding[0],
    Length(PasswordPadding));
end;

procedure TdxPDFCustomEncryptionAlgorithm.InitializePassword
  (var APassword: TBytes);
begin
  APassword := FPasswordPadding;
end;

function TdxPDFCustomEncryptionAlgorithm.CalculateEncryptionKey
  (ANumber: Integer): TBytes;
begin
  FEncryptionKey[FKeyLength] := ANumber and $FF;
  FEncryptionKey[FKeyLength + 1] := (ANumber and $FF00) shr 8;
  FEncryptionKey[FKeyLength + 2] := (ANumber and $FF0000) shr 16;
  FEncryptionKey[FKeyLength + 3] := 0;
  FEncryptionKey[FKeyLength + 4] := 0 shr 8;
  Result := TdxByteArray.Resize(CalculateMD5Hash(FEncryptionKey),
    ActualKeyLength);
end;

function TdxPDFCustomEncryptionAlgorithm.CheckPassword(const AExpectedHash,
  AActualHash: TBytes): Boolean;
begin
  Result := CompareMem(@AActualHash[0], @AExpectedHash[0],
    IfThen(FSecurityHandlerRevision = 2, HashSize, 16))
end;

function TdxPDFCustomEncryptionAlgorithm.RC4Crypt(const AKey, AData: TBytes;
  ADataSize: Integer): TBytes;
var
  ARC4Key: TdxRC4Key;
begin
  SetLength(Result, ADataSize);
  dxRC4Initialize(ARC4Key, AKey);
  dxRC4Crypt(ARC4Key, @AData[0], @Result[0], ADataSize);
end;

procedure TdxPDFCustomEncryptionAlgorithm.ReadCryptFilter
  (ADictionary: TdxPDFDictionary);
var
  AStringFilterName, AEmbeddedFileFilterName: string;
  AFilters, ATemp: TdxPDFDictionary;
begin
  AFilters := ADictionary.GetDictionary(TdxPDFKeywords.CryptFilters);
  FStreamFilterName := ADictionary.GetString(TdxPDFKeywords.StreamCryptFilter);
  AStringFilterName := ADictionary.GetString(TdxPDFKeywords.StringCryptFilter);

  if FSecurityHandlerRevision >= 5 then
  begin
    FEncodedOwnerPassword := ADictionary.GetBytes
      (TdxPDFKeywords.EncodedOwnerPassword);
    FEncodedUserPassword := ADictionary.GetBytes
      (TdxPDFKeywords.EncodedUserPassword);
    FEncryptedPermissions := ADictionary.GetBytes
      (TdxPDFKeywords.EncryptedPermissions);
  end;

  AEmbeddedFileFilterName := ADictionary.GetString
    (TdxPDFKeywords.EmbeddedFilesCryptFilter);
  if not IsExistFilter(AFilters, FStreamFilterName) then
    TdxPDFUtils.RaiseTestException;
  if not IsExistFilter(AFilters, AStringFilterName) then
    TdxPDFUtils.RaiseTestException;
  if (AEmbeddedFileFilterName <> '') and not IsExistFilter(AFilters,
    AEmbeddedFileFilterName) then
    TdxPDFUtils.RaiseTestException;

  if (FStreamFilterName = TdxPDFKeywords.StdCF) or
    (AStringFilterName = TdxPDFKeywords.StdCF) then
  begin
    ATemp := AFilters.GetDictionary(TdxPDFKeywords.StdCF);
    ReadCryptMethodVersion(ATemp);
    FKeyLength := ReadCryptFilterKeyLength(ATemp);
  end;
end;

function TdxPDFCustomEncryptionAlgorithm.ReadCryptFilterKeyLength
  (ADictionary: TdxPDFDictionary): Integer;
var
  AMaxKeyBits: Integer;
  AMaxKeyLength: Integer;
begin
  case FCryptMethodVersion of
    cmvAESV2:
      Result := 16; // 128 bit
    cmvAESV3:
      Result := 32; // 256 bit
  else
    if ADictionary <> nil then
    begin
      AMaxKeyLength := IfThen(FSecurityHandlerRevision >= 5, 32, 16);
      AMaxKeyBits := IfThen(FSecurityHandlerRevision >= 5, 256, 128);
      Result := ADictionary.GetInteger(TdxPDFKeywords.Length);
      if not InRange(Result, 5, AMaxKeyLength) then
      begin
        if not InRange(Result, 40, AMaxKeyBits) or (Result mod 8 <> 0) then
          TdxPDFUtils.Abort;
        Result := Result div 8;
      end;
    end
    else
      Result := 0;
  end;
end;

procedure TdxPDFCustomEncryptionAlgorithm.ReadCryptMethodVersion
  (ADictionary: TdxPDFDictionary);
var
  AName: string;
begin
  if ADictionary <> nil then
  begin
    AName := ADictionary.GetString(TdxPDFKeywords.CryptFilterMode);
    if AName = 'V2' then
      FCryptMethodVersion := cmvV2
    else if AName = 'AESV2' then
      FCryptMethodVersion := cmvAESV2
    else
      FCryptMethodVersion := cmvAESV3;
  end;
end;

function TdxPDFCustomEncryptionAlgorithm.CalculateMD5Hash
  (const AKey: TBytes): TBytes;
var
  AProvider: TdxMD5HashAlgorithm;
begin
  AProvider := TdxMD5HashAlgorithm.Create;
  try
    Result := AProvider.Calculate(AKey);
  finally
    AProvider.Free;
  end;
end;

function TdxPDFCustomEncryptionAlgorithm.CalculateOwnerPasswordHash
  (const AOwnerPassword, AUserPassword: TBytes): TBytes;
var
  AKey: TBytes;
  I: Integer;
begin
  AKey := CalculateOwnerEncryptionKey(AOwnerPassword);
  if FSecurityHandlerRevision >= 3 then
  begin
    TdxPDFUtils.AddData(AUserPassword, Result);
    for I := 0 to 19 do
      Result := RC4Crypt(XorKey(AKey, I), Result, Length(Result));
  end
  else
    Result := RC4Crypt(AKey, AUserPassword, Length(AUserPassword));
end;

function TdxPDFCustomEncryptionAlgorithm.CalculateUserPasswordHash
  (const APassword: TBytes): TBytes;
var
  I: Integer;
  AKey, AData, AUserHash: TBytes;
begin
  AKey := TdxByteArray.Concatenate(APassword, FOwnerPasswordHash);
  TdxPDFUtils.AddByte(FEncryptionFlags and $FF, AKey);
  TdxPDFUtils.AddByte((FEncryptionFlags and $FF00) shr 8, AKey);
  TdxPDFUtils.AddByte((FEncryptionFlags and $FF0000) shr 16, AKey);
  TdxPDFUtils.AddByte((FEncryptionFlags and $FF000000) shr 24, AKey);
  TdxPDFUtils.AddData(FDocumentID[0], AKey);
  if (FSecurityHandlerRevision >= 4) and not FEncryptMetadata then
  begin
    TdxPDFUtils.AddByte($FF, AKey);
    TdxPDFUtils.AddByte($FF, AKey);
    TdxPDFUtils.AddByte($FF, AKey);
    TdxPDFUtils.AddByte($FF, AKey);
  end;

  UpdateEncryptionKey(AKey);

  if FSecurityHandlerRevision < 3 then
  begin
    SetLength(AData, FKeyLength);
    TdxPDFUtils.CopyData(FEncryptionKey, 0, AData, 0, FKeyLength);
    Result := RC4Crypt(AData, FPasswordPadding, Length(FPasswordPadding));
  end
  else
  begin
    TdxPDFUtils.AddData(FPasswordPadding, AData);
    TdxPDFUtils.AddData(FDocumentID[0], AData);
    AUserHash := CalculateMD5Hash(AData);
    for I := 0 to 19 do
      AUserHash := RC4Crypt(XorKey(FEncryptionKey, I), AUserHash,
        Length(AUserHash));
    Result := PadOrTruncatePassword(AUserHash);
  end;
end;

function TdxPDFCustomEncryptionAlgorithm.CalculateOwnerEncryptionKey
  (const APassword: TBytes): TBytes;
var
  I: Integer;
begin
  Result := TdxByteArray.Resize(CalculateMD5Hash(APassword), FKeyLength);
  if FSecurityHandlerRevision >= 3 then
    for I := 0 to 50 - 1 do
      Result := CalculateMD5Hash(Result);
end;

function TdxPDFCustomEncryptionAlgorithm.GetActualKeyLength: Integer;
begin
  Result := Min(ExtendedKeyLength, 16);
end;

function TdxPDFCustomEncryptionAlgorithm.GetExtendedKeyLength: Integer;
begin
  Result := FKeyLength + 5;
end;

function TdxPDFCustomEncryptionAlgorithm.IsExistFilter
  (ADictionary: TdxPDFDictionary; const AName: string): Boolean;
begin
  Result := (AName = TdxPDFKeywords.Identity) or (AName = TdxPDFKeywords.StdCF)
    and (ADictionary <> nil) and ADictionary.Contains(TdxPDFKeywords.StdCF);
end;

function TdxPDFCustomEncryptionAlgorithm.PadOrTruncatePassword
  (const APasswordString: TBytes): TBytes;
var
  AData, APassword: TBytes;
begin
  APassword := TdxByteArray.Concatenate(TdxByteArray.Clone(APasswordString),
    FPasswordPadding);
  SetLength(Result, 0);
  SetLength(AData, HashSize);
  TdxPDFUtils.CopyData(APassword, 0, AData, 0, HashSize);
  TdxPDFUtils.AddData(AData, Result);
end;

function TdxPDFCustomEncryptionAlgorithm.ValidateHash
  (const AHash: TBytes): TBytes;
begin
  Result := DoValidateHash(AHash);
end;

function TdxPDFCustomEncryptionAlgorithm.XorKey(const AKey: TBytes;
  AValue: Integer): TBytes;
var
  I: Integer;
begin
  Result := TdxByteArray.Resize(Result, FKeyLength);
  for I := Low(Result) to High(Result) do
    Result[I] := AKey[I] xor AValue;
end;

procedure TdxPDFCustomEncryptionAlgorithm.UpdateEncryptionKey
  (const AKey: TBytes);
var
  I: Integer;
begin
  FEncryptionKey := CalculateMD5Hash(AKey);
  if FSecurityHandlerRevision >= 3 then
  begin
    for I := 0 to 50 - 1 do
      FEncryptionKey := CalculateMD5Hash(TdxByteArray.Resize(FEncryptionKey,
        FKeyLength));
  end;
  FEncryptionKey := TdxByteArray.Resize(FEncryptionKey, FKeyLength);
  FEncryptionKey := DoUpdateEncryptionKey;
end;

{ TdxPDFAESBasedEncryptionAlgorithm }

constructor TdxPDFAESBasedEncryptionAlgorithm.Create(const ADocumentID
  : TdxPDFDocumentID; const AUserPassword, AOwnerPassword: string;
  const APermissions: TdxPDFDocumentPermissions);
begin
  raise EdxPDFEncryptionException.Create
    ('The AES algorithm is not supported for file encryption');
end;

class function TdxPDFAESBasedEncryptionAlgorithm.GetType
  : TdxPDFEncryptionAlgorithmType;
begin
  Result := eatAES;
end;

function TdxPDFAESBasedEncryptionAlgorithm.DoUpdateEncryptionKey: TBytes;
begin
  if CryptMethodVersion = cmvAESV2 then
  begin
    Result := TdxByteArray.Resize(EncryptionKey, ExtendedKeyLength + 4);
    Result[ExtendedKeyLength] := $73;
    Result[ExtendedKeyLength + 1] := $41;
    Result[ExtendedKeyLength + 2] := $6C;
    Result[ExtendedKeyLength + 3] := $54;
  end
  else
    Result := inherited DoUpdateEncryptionKey;
end;

function TdxPDFAESBasedEncryptionAlgorithm.DoValidateHash
  (const AHash: TBytes): TBytes;
const
  AESV3HashLength = HashSize + 2 * SaltSize;
  PasswordLengthLimit = 127;

  procedure CheckLength;
  begin
    if Length(AHash) <> AESV3HashLength then
      TdxPDFUtils.RaiseTestException;
  end;

begin
  Result := AHash;
  case SecurityHandlerRevision of
    6:
      begin
        if Length(AHash) <> PasswordLengthLimit then
          CheckLength;
        Result := TdxByteArray.Resize(AHash, AESV3HashLength);
      end;
    5:
      CheckLength;
  else
    Result := inherited DoValidateHash(AHash);
  end;
end;

function TdxPDFAESBasedEncryptionAlgorithm.AESDecrypt(const AKey,
  AData: TBytes): TBytes;
var
  AInitializationVector: TBytes;
begin
  if Length(AData) - InitVectorSize < 0 then
    SetLength(Result, 0)
  else
  begin
    SetLength(AInitializationVector, InitVectorSize);
    TdxPDFUtils.CopyData(AData, 0, AInitializationVector, 0, InitVectorSize);
    Result := DoAESDecrypt(AKey, AInitializationVector, AData, InitVectorSize,
      CRYPT_MODE_CBC, RANDOM_PADDING);
  end;
end;

function TdxPDFAESBasedEncryptionAlgorithm.AESDecrypt(const AKey, AData: TBytes;
  AChainingMode: Integer): TBytes;
var
  AInitializationVector: TBytes;
begin
  SetLength(AInitializationVector, InitVectorSize);
  Result := DoAESDecrypt(AKey, AInitializationVector, AData, 0, AChainingMode,
    PKCS5_PADDING);
end;

function TdxPDFAESBasedEncryptionAlgorithm.CreateAESCryptoKey(const AKey,
  AInitVector: TBytes): IdxCryptoKey;
var
  ACryptoProvider: IdxCryptoProvider;
  AAlgorithmID, AProviderID: Integer;
begin
  TdxCryptoAlgorithms.GetInfo('AES', Length(AKey) * 8, AAlgorithmID,
    AProviderID);
  ACryptoProvider := TdxCryptoProvider.Create(nil, AProviderID);
  Result := TdxCryptoKey.Create(ACryptoProvider, AAlgorithmID, AKey);
  Result.SetChainingMode(CRYPT_MODE_CBC);
  Result.SetIV(AInitVector);
end;

function TdxPDFAESBasedEncryptionAlgorithm.CalculateHash(const APasswordString,
  AData, AUserKey: TBytes): TBytes;

  function DoCalculateHash(AHashAlgorithm: Integer;
    const AData: TBytes): TBytes;
  var
    AAlgorithm: TdxHashAlgorithm;
  begin
    case AHashAlgorithm of
      1:
        AAlgorithm := TdxSHA384HashAlgorithm.Create;
      2:
        AAlgorithm := TdxSHA512HashAlgorithm.Create;
    else
      AAlgorithm := TdxSHA256HashAlgorithm.Create;
    end;
    try
      Result := AAlgorithm.Calculate(AData);
    finally
      AAlgorithm.Free;
    end;
  end;

var
  AProvider: TdxSHA256HashAlgorithm;
  AHash, AKey, AInitVector, AEncryptedKey: TBytes;
  ARound, I: Integer;
  AK1: TBytes;
  AValue: Int64;
begin
  AProvider := TdxSHA256HashAlgorithm.Create;
  try
    AHash := AProvider.Calculate(AData);
    if SecurityHandlerRevision = 5 then
      Exit(AHash);
    ARound := 0;
    while True do
    begin
      SetLength(AK1, 0);
      for I := 0 to 64 - 1 do
      begin
        TdxPDFUtils.AddData(APasswordString, AK1);
        TdxPDFUtils.AddData(AHash, AK1);
        TdxPDFUtils.AddData(AUserKey, AK1);
      end;
      SetLength(AKey, InitVectorSize);
      SetLength(AInitVector, InitVectorSize);
      TdxPDFUtils.CopyData(AHash, 0, AKey, 0, InitVectorSize);
      TdxPDFUtils.CopyData(AHash, InitVectorSize, AInitVector, 0,
        InitVectorSize);
      AEncryptedKey := DoAESEncrypt(AKey, AInitVector, AK1);
      AValue := 0;
      for I := 0 to 16 - 1 do
        Inc(AValue, AEncryptedKey[I]);
      AHash := DoCalculateHash(AValue mod 3, AEncryptedKey);
      if (ARound >= 63) and (AEncryptedKey[Length(AEncryptedKey) - 1] <= ARound
        - 32) then
        Exit(TdxByteArray.Resize(AHash, 32));
      Inc(ARound);
    end;
  finally
    AProvider.Free;
  end;
end;

function TdxPDFAESBasedEncryptionAlgorithm.DoAESDecrypt(const AKey, AInitVector,
  AData: TBytes; APosition, AChainingMode, APaddingMode: Integer): TBytes;
var
  ATemp: TBytes;
begin
  SetLength(ATemp, Length(AData) - APosition);
  TdxPDFUtils.CopyData(AData, APosition, ATemp, 0, Length(AData) - APosition);
  Result := TdxCipher.Decrypt(CreateAESCryptoKey(AKey, AInitVector), ATemp);
end;

function TdxPDFAESBasedEncryptionAlgorithm.DoAESEncrypt(const AKey, AInitVector,
  AData: TBytes): TBytes;
begin
  Result := TdxCipher.Encrypt(CreateAESCryptoKey(AKey, AInitVector),
    AData, False);
end;

{ TdxPDFMixedEncryptionAlgorithm }

class function TdxPDFMixedEncryptionAlgorithm.GetVersion: Integer;
begin
  Result := 4;
end;

function TdxPDFMixedEncryptionAlgorithm.Decrypt(const AData: TBytes;
  ANumber: Integer): TBytes;
var
  AKey: TBytes;
begin
  if StreamFilterName = TdxPDFKeywords.Identity then
    Result := AData
  else
  begin
    AKey := CalculateEncryptionKey(ANumber);
    if CryptMethodVersion = cmvAESV2 then
      Result := AESDecrypt(AKey, AData)
    else
      Result := RC4Crypt(AKey, AData, Length(AData));
  end;
end;

function TdxPDFMixedEncryptionAlgorithm.Encrypt(const AData: TBytes;
  ANumber: Integer): TBytes;
begin
  TdxPDFUtils.RaiseNotImplementedException;
end;

procedure TdxPDFMixedEncryptionAlgorithm.Read(ADictionary: TdxPDFDictionary);
begin
  inherited Read(ADictionary);
  if SecurityHandlerRevision <> 4 then
    TdxPDFUtils.RaiseTestException
  else
    ReadCryptFilter(ADictionary);
end;

procedure TdxPDFMixedEncryptionAlgorithm.Write(ADictionary: TdxPDFDictionary);
begin
  TdxPDFUtils.RaiseNotImplementedException;
end;

{ TdxPDFAESEncryptionAlgorithm }

class function TdxPDFAESEncryptionAlgorithm.GetVersion: Integer;
begin
  Result := 5;
end;

function TdxPDFAESEncryptionAlgorithm.Decrypt(const AData: TBytes;
  ANumber: Integer): TBytes;
begin
  Result := AESDecrypt(EncryptionKey, AData);
end;

function TdxPDFAESEncryptionAlgorithm.Encrypt(const AData: TBytes;
  ANumber: Integer): TBytes;
begin
  TdxPDFUtils.RaiseNotImplementedException;
end;

function TdxPDFAESEncryptionAlgorithm.CheckUserPassword(const AUserPassword
  : TBytes): Boolean;
var
  AData, APassword: TBytes;
begin
  TdxPDFUtils.AddData(AUserPassword, APassword);
  TdxPDFUtils.AddData(FUserValidationSalt, APassword);
  SetLength(AData, 0);
  if CheckPassword(FUserPasswordHash, CalculateHash(AUserPassword, APassword,
    AData)) then
  begin
    SetLength(APassword, 0);
    TdxPDFUtils.AddData(AUserPassword, APassword);
    TdxPDFUtils.AddData(FUserKeySalt, APassword);
    FEncryptionKey := AESDecrypt(CalculateHash(AUserPassword, APassword, AData),
      FEncodedUserPassword, CRYPT_MODE_CBC);
    Result := CheckPermissions;
  end
  else
    Result := False;
end;

function TdxPDFAESEncryptionAlgorithm.CheckOwnerPassword(const AUserPassword
  : TBytes): Boolean;
begin
  Result := False;
end;

function TdxPDFAESEncryptionAlgorithm.ConvertToBytes(const APassword
  : string): TBytes;
begin
  Result := TEncoding.UTF8.GetBytes(APassword);
end;

function TdxPDFAESEncryptionAlgorithm.ValidatePassword(const APasswordString
  : string): Boolean;
var
  APassword, AOwnerPassword: TBytes;
begin
  Result := inherited;
  if not Result then
  begin
    APassword := ConvertToBytes(APasswordString);
    TdxPDFUtils.AddData(APassword, AOwnerPassword);
    TdxPDFUtils.AddData(FOwnerValidationSalt, AOwnerPassword);
    TdxPDFUtils.AddData(FUserPasswordHash, AOwnerPassword);
    if CheckPassword(FOwnerPasswordHash, CalculateHash(APassword,
      AOwnerPassword, FUserPasswordHash)) then
    begin
      SetLength(AOwnerPassword, 0);
      TdxPDFUtils.AddData(APassword, AOwnerPassword);
      TdxPDFUtils.AddData(FOwnerKeySalt, AOwnerPassword);
      TdxPDFUtils.AddData(FUserPasswordHash, AOwnerPassword);
      FEncryptionKey := AESDecrypt(CalculateHash(APassword, AOwnerPassword,
        FUserPasswordHash), FEncodedOwnerPassword, CRYPT_MODE_CBC);
      Result := CheckPermissions;
    end;
  end;
end;

procedure TdxPDFAESEncryptionAlgorithm.Write(ADictionary: TdxPDFDictionary);
begin
  TdxPDFUtils.RaiseNotImplementedException;
end;

procedure TdxPDFAESEncryptionAlgorithm.InitializePassword
  (var APassword: TBytes);
begin
  SetLength(APassword, 0);
end;

procedure TdxPDFAESEncryptionAlgorithm.Read(ADictionary: TdxPDFDictionary);
const
  KeySaltPosition = HashSize + SaltSize;
begin
  inherited Read(ADictionary);
  if (SecurityHandlerRevision <> 5) and (SecurityHandlerRevision <> 6) then
    TdxPDFUtils.RaiseTestException
  else
  begin
    ReadCryptFilter(ADictionary);
    FOwnerValidationSalt := TdxByteArray.Resize(FOwnerValidationSalt, SaltSize);
    FOwnerKeySalt := TdxByteArray.Resize(FOwnerKeySalt, SaltSize);
    TdxPDFUtils.CopyData(FOwnerPasswordHash, HashSize, FOwnerValidationSalt, 0,
      SaltSize);
    TdxPDFUtils.CopyData(FOwnerPasswordHash, KeySaltPosition, FOwnerKeySalt, 0,
      SaltSize);
    FUserValidationSalt := TdxByteArray.Resize(FUserValidationSalt, SaltSize);
    FUserKeySalt := TdxByteArray.Resize(FUserKeySalt, SaltSize);
    TdxPDFUtils.CopyData(FUserPasswordHash, HashSize, FUserValidationSalt, 0,
      SaltSize);
    TdxPDFUtils.CopyData(FUserPasswordHash, KeySaltPosition, FUserKeySalt, 0,
      SaltSize);
  end;
end;

function TdxPDFAESEncryptionAlgorithm.CheckPermissions: Boolean;
var
  APermissions: TBytes;
begin
  APermissions := AESDecrypt(EncryptionKey, FEncryptedPermissions,
    CRYPT_MODE_ECB);
  Result := (APermissions[9] = Byte('a')) and (APermissions[10] = Byte('d')) and
    (APermissions[11] = Byte('b')) and
    (((APermissions[2] shl 16) + (APermissions[1] shl 8) + APermissions[0])
    = (EncryptionFlags and $FFFFFF));
end;

{ TdxPDF40BitEncryptionAlgorithm }

class function TdxPDF40BitEncryptionAlgorithm.GetVersion: Integer;
begin
  Result := 1;
end;

class function TdxPDF40BitEncryptionAlgorithm.GetType
  : TdxPDFEncryptionAlgorithmType;
begin
  Result := eatRC40Bit;
end;

procedure TdxPDF40BitEncryptionAlgorithm.Initialize(const ADocumentID
  : TdxPDFDocumentID);
begin
  inherited;
  FSecurityHandlerRevision := 2;
  FKeyLength := 5;
end;

function TdxPDF40BitEncryptionAlgorithm.ReadKeyLength
  (ADictionary: TdxPDFDictionary): Integer;
begin
  Result := 5; // 40 bit
end;

{ TdxPDFGreater40BitEncryptionAlgorithm }

class function TdxPDFGreater40BitEncryptionAlgorithm.GetVersion: Integer;
begin
  Result := 2;
end;

class function TdxPDFGreater40BitEncryptionAlgorithm.GetType
  : TdxPDFEncryptionAlgorithmType;
begin
  Result := eatRC128Bit;
end;

procedure TdxPDFGreater40BitEncryptionAlgorithm.
  Read(ADictionary: TdxPDFDictionary);
begin
  inherited;
  if not InRange(FKeyLength, 5, 16) { 40-128 bit } then
    TdxPDFUtils.RaiseTestException;
  if not InRange(SecurityHandlerRevision, 2, 3) then
    TdxPDFUtils.RaiseTestException;
end;

procedure TdxPDFGreater40BitEncryptionAlgorithm.Initialize(const ADocumentID
  : TdxPDFDocumentID);
begin
  inherited;
  FSecurityHandlerRevision := 3;
  FKeyLength := 16;
end;

{ TdxPDFEncryptionInfo }

constructor TdxPDFEncryptionInfo.Create(const ADocumentID: TdxPDFDocumentID;
  ADictionary: TdxPDFDictionary);
begin
  inherited Create;
  FAlgorithm := TdxPDFEncryptionAlgorithms.Create(ADictionary, ADocumentID)
end;

constructor TdxPDFEncryptionInfo.Create(const ADocumentID: TdxPDFDocumentID;
  AType: TdxPDFEncryptionAlgorithmType;
  const AUserPassword, AOwnerPassword: string;
  const APermissions: TdxPDFDocumentPermissions);
begin
  inherited Create;
  FAlgorithm := TdxPDFEncryptionAlgorithms.Create(ADocumentID, AType,
    AUserPassword, AOwnerPassword, APermissions);
end;

destructor TdxPDFEncryptionInfo.Destroy;
begin
  FreeAndNil(FAlgorithm);
  DeleteCriticalSection(FLock);
  inherited Destroy;
end;

procedure TdxPDFEncryptionInfo.AfterConstruction;
begin
  inherited AfterConstruction;
  InitializeCriticalSectionAndSpinCount(FLock, 1024);
end;

function TdxPDFEncryptionInfo.CheckPassword(AAttemptsLimit: Integer;
  AOnGetPasswordEvent: TdxGetPasswordEvent): Boolean;
begin
  Result := FAlgorithm.CheckPassword(AAttemptsLimit, AOnGetPasswordEvent);
end;

function TdxPDFEncryptionInfo.Decrypt(const AData: TBytes;
  ANumber: Integer): TBytes;
begin
  EnterCriticalSection(FLock);
  try
    Result := FAlgorithm.Decrypt(AData, ANumber);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TdxPDFEncryptionInfo.Encrypt(const AData: TBytes;
  ANumber: Integer): TBytes;
begin
  EnterCriticalSection(FLock);
  try
    Result := FAlgorithm.Encrypt(AData, ANumber);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TdxPDFEncryptionInfo.Write(ADictionary: TdxPDFDictionary);
begin
  Algorithm.Write(ADictionary);
end;

function TdxPDFEncryptionInfo.GetAllowContentExtraction: Boolean;
begin
  Result := TestPermissions([pdpAllowCopyContent, pdpAllowExtractContent]);
end;

function TdxPDFEncryptionInfo.GetAllowPrinting: Boolean;
begin
  Result := TestPermissions([pdpAllowPrint, pdpAllowPrintHighResolution]);
end;

function TdxPDFEncryptionInfo.TestPermissions(APermissions
  : TdxPDFDocumentPermissions): Boolean;
begin
  Result := Algorithm.IsOwnerKey or
    (APermissions * Algorithm.Permissions = APermissions);
end;

function TdxPDFEncryptionInfo.EncryptMetadata: Boolean;
begin
  Result := Algorithm.EncryptMetadata;
end;

function TdxPDFEncryptionInfo.QueryInterface(const IID: TGUID; out Obj)
  : HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TdxPDFEncryptionInfo._AddRef: Integer;
begin
  Result := -1;
end;

function TdxPDFEncryptionInfo._Release: Integer;
begin
  Result := -1;
end;

{ TdxPDFEncryptionAlgorithms }

class function TdxPDFEncryptionAlgorithms.Create(ADictionary: TdxPDFDictionary;
  const ADocumentID: TdxPDFDocumentID): TdxPDFCustomEncryptionAlgorithm;
var
  AClass: TdxPDFCustomEncryptionAlgorithmClass;
begin
  if ADictionary <> nil then
    AClass := GetAlgorithmClass(ADictionary.GetInteger(TdxPDFKeywords.Version))
  else
    AClass := nil;

  if AClass <> nil then
    Result := AClass.Create(ADocumentID, ADictionary)
  else
    Result := nil;
end;

class function TdxPDFEncryptionAlgorithms.Create(const ADocumentID
  : TdxPDFDocumentID; AType: TdxPDFEncryptionAlgorithmType;
  const AUserPassword, AOwnerPassword: string;
  const APermissions: TdxPDFDocumentPermissions)
  : TdxPDFCustomEncryptionAlgorithm;
begin
  Result := GetAlgorithmClass(AType).Create(ADocumentID, AUserPassword,
    AOwnerPassword, APermissions);
end;

class function TdxPDFEncryptionAlgorithms.GetAlgorithmClass(AVersion: Integer)
  : TdxPDFCustomEncryptionAlgorithmClass;
begin
  if TdxPDF40BitEncryptionAlgorithm.GetVersion = AVersion then
    Exit(TdxPDF40BitEncryptionAlgorithm);
  if TdxPDFGreater40BitEncryptionAlgorithm.GetVersion = AVersion then
    Exit(TdxPDFGreater40BitEncryptionAlgorithm);
  if TdxPDFMixedEncryptionAlgorithm.GetVersion = AVersion then
    Exit(TdxPDFMixedEncryptionAlgorithm);
  if TdxPDFAESEncryptionAlgorithm.GetVersion = AVersion then
    Exit(TdxPDFAESEncryptionAlgorithm);
  Result := nil;
end;

class function TdxPDFEncryptionAlgorithms.GetAlgorithmClass
  (AType: TdxPDFEncryptionAlgorithmType): TdxPDFCustomEncryptionAlgorithmClass;
begin
  case AType of
    eatAES:
      Result := TdxPDFAESEncryptionAlgorithm;
    eatRC128Bit:
      Result := TdxPDFGreater40BitEncryptionAlgorithm;
  else // eatRC40Bit:
    Result := TdxPDF40BitEncryptionAlgorithm;
  end;
end;

end.
