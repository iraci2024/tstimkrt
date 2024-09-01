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

unit dxPDFSignature;

{$I cxVer.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxX509Certificate,
  dxPDFBase, dxPDFTypes, dxPDFCore;

type
  TdxPDFSignatureField = class;

  TdxPDFAnnotationPermission = (apCreate, apDelete, apModify, apCopy, apImport,
    apExport, apOnline, apSummaryView); // for internal use
  TdxPDFAnnotationPermissions = set of TdxPDFAnnotationPermission;
  // for internal use
  TdxPDFDocumentAccessPermission = (dapNoChanges, dapFormFillingAndSignatures,
    dapAnnotationsFormFillingAndSignatures); // for internal use
  TdxPDFEmbeddedFilePermission = (fupCreate, fupDelete, fupModify, fupImport);
  // for internal use
  TdxPDFEmbeddedFilePermissions = set of TdxPDFEmbeddedFilePermission;
  // for internal use
  TdxPDFInteractiveFormFieldPermission = (ifpAdd, ifpDelete, ifpFillIn,
    ifpImport, ifpExport, ifpSubmitStandalone, ifpSpawnTemplate,
    ifpBarcodePlainText, ifpOnline); // for internal use
  TdxPDFInteractiveFormFieldPermissions = set of
    TdxPDFInteractiveFormFieldPermission; // for internal use
  TdxPDFSignatureFieldLockRange = (lrAll, lrInclude, lrExclude);
  // for internal use

  { TdxPDFSignatureFieldInfo }

  TdxPDFSignatureFieldInfo = class(TPersistent)
  strict private
    FCertificate: TdxX509Certificate;
    FContactInfo: string;
    FLocation: string;
    FReason: string;
    FSignedDate: TDateTime;
    procedure SetCertificate(const AValue: TdxX509Certificate);
  protected
    FField: TdxPDFSignatureField;
    procedure SetSignedTime(AValue: TDateTime);
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure LoadCertificate(const AFileName, APassword: string); overload;
    procedure LoadCertificate(AStream: TStream; APassword: string); overload;
    //
    property Certificate: TdxX509Certificate read FCertificate
      write SetCertificate;
    property ContactInfo: string read FContactInfo write FContactInfo;
    property Location: string read FLocation write FLocation;
    property Reason: string read FReason write FReason;
    property SignedDate: TDateTime read FSignedDate;
  end;

  TdxPDFSignatureFieldInfoList = class(TObjectList<TdxPDFSignatureFieldInfo>);

  { TdxPDFSignatureByteRange }

  TdxPDFSignatureByteRange = record // for internal use
  strict private
    FLength: Integer;
    FStart: Integer;
  public
    class function Create(AStart, ALength: Integer)
      : TdxPDFSignatureByteRange; static;
    property Length: Integer read FLength;
    property Start: Integer read FStart;
  end;

  TdxPDFSignatureByteRanges = array of TdxPDFSignatureByteRange;

  { TdxPDFSignatureFieldLock }

  TdxPDFSignatureFieldLock = class(TdxPDFObject) // for internal use
  strict private
  const
    LockRangeMap: array [TdxPDFSignatureFieldLockRange] of string = ('All',
      'Include', 'Exclude');
  strict private
    FFieldNames: TStringList;
    FRange: TdxPDFSignatureFieldLockRange;

    function ReadLockRange(AObject: TdxPDFBase): TdxPDFSignatureFieldLockRange;
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
  public
    class function Parse(ADictionary: TdxPDFReaderDictionary)
      : TdxPDFSignatureFieldLock; static;
    //
    property FieldNames: TStringList read FFieldNames;
    property Range: TdxPDFSignatureFieldLockRange read FRange;
  end;

  { TdxPDFCustomSignatureTransformMethod }

  TdxPDFCustomSignatureTransformMethod = class(TdxPDFObject) // for internal use
  protected
    function GetValidVersion: string; virtual;
  public
    class function Parse(ADictionary: TdxPDFReaderDictionary)
      : TdxPDFCustomSignatureTransformMethod; static;
    //
    property ValidVersion: string read GetValidVersion;
  end;

  { TdxPDFDocumentMDPSignatureTransformMethod }

  TdxPDFDocumentMDPSignatureTransformMethod = class
    (TdxPDFCustomSignatureTransformMethod) // for internal use
  strict private
    FPermission: TdxPDFDocumentAccessPermission;
  protected
    procedure Initialize; override;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;
  public
    class function GetTypeName: string; override;
    //
    property Permission: TdxPDFDocumentAccessPermission read FPermission;
  end;

  { TdxPDFFieldMDPSignatureTransformMethod }

  TdxPDFFieldMDPSignatureTransformMethod = class
    (TdxPDFCustomSignatureTransformMethod) // for internal use
  strict private
    FLock: TdxPDFSignatureFieldLock;
  protected
    procedure DestroySubClasses; override;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;
  public
    class function GetTypeName: string; override;
    //
    property Lock: TdxPDFSignatureFieldLock read FLock;
  end;

  { TdxPDFUsageRightsSignatureTransformMethod }

  TdxPDFUsageRightsSignatureTransformMethod = class
    (TdxPDFCustomSignatureTransformMethod) // for internal use
  strict private
    FAllowFullSave: Boolean;
    FAllowModifySignature: Boolean;
    FAnnotationUsagePermissions: TdxPDFAnnotationPermissions;
    FEmbeddedFileUsagePermissions: TdxPDFEmbeddedFilePermissions;
    FInteractiveFormFieldUsagePermissions
      : TdxPDFInteractiveFormFieldPermissions;
    FMessageText: string;
    FRestrictOtherPermissions: Boolean;
    //
    function ConvertToAnnotationPermission(const AValue: string)
      : TdxPDFAnnotationPermission;
    function ConvertToEmbeddedFilePermission(const AValue: string)
      : TdxPDFEmbeddedFilePermission;
    function ConvertToInteractiveFormFieldPermission(const AValue: string)
      : TdxPDFInteractiveFormFieldPermission;
    function GetAnnotationPermissions(ADictionary: TdxPDFReaderDictionary)
      : TdxPDFAnnotationPermissions;
    function GetEmbeddedFilePermissions(ADictionary: TdxPDFReaderDictionary)
      : TdxPDFEmbeddedFilePermissions;
    function GetInteractiveFormFieldPermissions(ADictionary
      : TdxPDFReaderDictionary): TdxPDFInteractiveFormFieldPermissions;
    function IsAllowPermission(ADictionary: TdxPDFReaderDictionary;
      const AKey, AExpectedValue: string): Boolean;
    procedure ConvertPermissions(ADictionary: TdxPDFReaderDictionary;
      const AKey: string; AConvertElementProc: TProc<string>);
  protected
    function GetValidVersion: string; override;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;
  public
    class function GetTypeName: string; override;
    //
    property AllowFullSave: Boolean read FAllowFullSave;
    property AllowModifySignature: Boolean read FAllowModifySignature;
    property AnnotationUsagePermissions: TdxPDFAnnotationPermissions
      read FAnnotationUsagePermissions;
    property EmbeddedFileUsagePermissions: TdxPDFEmbeddedFilePermissions
      read FEmbeddedFileUsagePermissions;
    property InteractiveFormFieldUsagePermissions
      : TdxPDFInteractiveFormFieldPermissions
      read FInteractiveFormFieldUsagePermissions;
    property MessageText: string read FMessageText;
    property RestrictOtherPermissions: Boolean read FRestrictOtherPermissions;
  end;

  { TdxPDFSignatureReference }

  TdxPDFSignatureReference = class(TdxPDFObject) // for internal use
  strict private
    FDigestMethod: string;
    FTransformMethod: TdxPDFCustomSignatureTransformMethod;
  protected
    procedure DestroySubClasses; override;
    procedure Initialize; override;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;
  public
    property TransformMethod: TdxPDFCustomSignatureTransformMethod
      read FTransformMethod;
    property DigestMethod: string read FDigestMethod;
  end;

  { TdxPDFSignature }

  TdxPDFSignature = class(TdxPDFObject) // for internal use
  strict private
    FAlteredInteractiveFormFieldCount: Integer;
    FAlteredPageCount: Integer;
    FByteRanges: TdxPDFSignatureByteRanges;
    FByteRangesPlaceHolder: TdxPDFPlaceHolder;
    FCertificate: TdxX509Certificate;
    FContactInfo: string;
    FContent: TBytes;
    FContentPlaceHolder: TdxPDFPlaceHolder;
    FFilledInInteractiveFormFieldCount: Integer;
    FFilter: string;
    FLocation: string;
    FName: string;
    FReason: string;
    FReferences: TObjectList<TdxPDFSignatureReference>;
    FSignTime: TDateTime;
    FSubFilter: string;
    //
    procedure SetByteRangesPlaceHolder(const AValue: TdxPDFPlaceHolder);
    procedure SetCertificate(const AValue: TdxX509Certificate);
    procedure SetContentPlaceHolder(const AValue: TdxPDFPlaceHolder);
    //
    function SignData(const AData: TBytes): TBytes;
    procedure ReadByteRange(ADictionary: TdxPDFReaderDictionary);
    procedure ReadChanges(ADictionary: TdxPDFReaderDictionary);
    procedure ReadReference(ADictionary: TdxPDFReaderDictionary);
    procedure WriteContent(AWriter: TdxPDFWriter; const AData: TBytes);
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
    //
    procedure Populate(AInfo: TdxPDFSignatureFieldInfo);
    //
    property ByteRangesPlaceHolder: TdxPDFPlaceHolder
      write SetByteRangesPlaceHolder;
    property ContentPlaceHolder: TdxPDFPlaceHolder write SetContentPlaceHolder;
    property Filter: string read FFilter write FFilter;
    property SubFilter: string read FSubFilter write FSubFilter;
  public
    class function Parse(ADictionary: TdxPDFReaderDictionary)
      : TdxPDFSignature; static;
    constructor Create(AInfo: TdxPDFSignatureFieldInfo); overload;
    procedure Patch(AWriter: TdxPDFWriter);
    procedure UpdateSignTime;
    //
    property Certificate: TdxX509Certificate read FCertificate
      write SetCertificate;
    property ContactInfo: string read FContactInfo write FContactInfo;
    property Location: string read FLocation write FLocation;
    property Name: string read FName;
    property Reason: string read FReason write FReason;
    property SignTime: TDateTime read FSignTime;
  end;

  { TdxPDFSignatureField }

  TdxPDFSignatureField = class(TdxPDFInteractiveFormField) // for internal use
  strict private
    FCanWriteSignature: Boolean;
    FLock: TdxPDFSignatureFieldLock;
    FSignature: TdxPDFSignature;
  protected
    function GetName: string; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary;
      ANumber: Integer); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;

    function CreateInfo: TdxPDFSignatureFieldInfo;
    property CanWriteSignature: Boolean read FCanWriteSignature;
  public
    constructor Create(AForm: TdxPDFInteractiveForm;
      AWidget: TdxPDFCustomAnnotation; ASignature: TdxPDFSignature);
      reintroduce;
    class function GetTypeName: string; override;
    //
    property Lock: TdxPDFSignatureFieldLock read FLock;
    property Signature: TdxPDFSignature read FSignature;
  end;

implementation

uses
  Math, dxCore, dxHash, cxDateUtils, dxPDFUtils;

{ TdxPDFSignatureFieldInfo }

destructor TdxPDFSignatureFieldInfo.Destroy;
begin
  FreeAndNil(FCertificate);
  inherited Destroy;
end;

procedure TdxPDFSignatureFieldInfo.Assign(Source: TPersistent);
begin
  if Source is TdxPDFSignatureFieldInfo then
  begin
    ContactInfo := TdxPDFSignatureFieldInfo(Source).ContactInfo;
    Location := TdxPDFSignatureFieldInfo(Source).Location;
    Reason := TdxPDFSignatureFieldInfo(Source).Reason;
    Certificate := TdxPDFSignatureFieldInfo(Source).Certificate;
    FSignedDate := TdxPDFSignatureFieldInfo(Source).SignedDate;
  end;
end;

procedure TdxPDFSignatureFieldInfo.LoadCertificate(const AFileName,
  APassword: string);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadCertificate(AStream, APassword);
  finally
    AStream.Free;
  end;
end;

procedure TdxPDFSignatureFieldInfo.LoadCertificate(AStream: TStream;
  APassword: string);
var
  ACertificate: TdxX509Certificate;
begin
  ACertificate := TdxX509Certificate.Create(AStream, APassword);
  try
    Certificate := ACertificate;
  finally
    ACertificate.Free;
  end;
end;

procedure TdxPDFSignatureFieldInfo.SetSignedTime(AValue: TDateTime);
begin
  FSignedDate := AValue;
end;

procedure TdxPDFSignatureFieldInfo.SetCertificate(const AValue
  : TdxX509Certificate);
begin
  FreeAndNil(FCertificate);
  if dxX509IsUsableForDigitalSignature(AValue) then
    FCertificate := TdxX509Certificate.Create(AValue)
  else if AValue <> nil then
    raise EdxX509Exception.Create
      ('The certificate cannot be used for the digital signature');
end;

{ TdxPDFSignatureByteRange }

class function TdxPDFSignatureByteRange.Create(AStart, ALength: Integer)
  : TdxPDFSignatureByteRange;
begin
  Result.FStart := AStart;
  Result.FLength := ALength;
end;

{ TdxPDFSignatureFieldLock }

class function TdxPDFSignatureFieldLock.Parse(ADictionary
  : TdxPDFReaderDictionary): TdxPDFSignatureFieldLock;
begin
  if ADictionary = nil then
    Exit(nil);
  Result := TdxPDFSignatureFieldLock.Create;
  Result.Read(ADictionary);
end;

procedure TdxPDFSignatureFieldLock.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FFieldNames := TStringList.Create;
end;

procedure TdxPDFSignatureFieldLock.DestroySubClasses;
begin
  FreeAndNil(FFieldNames);
  inherited DestroySubClasses;
end;

function TdxPDFSignatureFieldLock.ReadLockRange(AObject: TdxPDFBase)
  : TdxPDFSignatureFieldLockRange;
var
  AIndex: TdxPDFSignatureFieldLockRange;
  AValue: string;
begin
  if AObject is TdxPDFString then
  begin
    AValue := TdxPDFString(AObject).Value;
    for AIndex := Low(TdxPDFSignatureFieldLockRange)
      to High(TdxPDFSignatureFieldLockRange) do
    begin
      if LockRangeMap[AIndex] = AValue then
        Exit(AIndex);
    end;
    Result := lrAll;
  end
  else if AObject is TdxPDFInteger then
    Result := TdxPDFSignatureFieldLockRange(TdxPDFInteger(AObject).Value)
  else
    Result := lrAll;
end;

procedure TdxPDFSignatureFieldLock.ReadProperties
  (ADictionary: TdxPDFReaderDictionary);
begin
  inherited ReadProperties(ADictionary);
  FRange := ReadLockRange(ADictionary.GetObject(TdxPDFKeywords.Action));
  if FRange <> lrAll then
    ADictionary.PopulateList(TdxPDFKeywords.Fields, FFieldNames);
end;

procedure TdxPDFSignatureFieldLock.Write(AHelper: TdxPDFWriterHelper;
  ADictionary: TdxPDFWriterDictionary);
begin
  inherited Write(AHelper, ADictionary);
  ADictionary.AddName(TdxPDFKeywords.Action, LockRangeMap[Range]);
  ADictionary.Add(TdxPDFKeywords.Fields, FFieldNames);
end;

{ TdxPDFCustomSignatureTransformMethod }

class function TdxPDFCustomSignatureTransformMethod.Parse
  (ADictionary: TdxPDFReaderDictionary): TdxPDFCustomSignatureTransformMethod;
var
  AClass: TdxPDFObjectClass;
  ADataDictionary: TdxPDFReaderDictionary;
  AName: string;
begin
  AName := ADictionary.GetString('TransformMethod');
  if (AName = '') or ADictionary.TryGetDictionary('Data', ADataDictionary) and
    (ADataDictionary.Number <> ADataDictionary.Repository.Catalog.Number) then
    Exit(nil);
  if dxPDFTryGetDocumentObjectClass(AName, AClass) then
  begin
    Result := AClass.Create as TdxPDFCustomSignatureTransformMethod;
    TdxPDFCustomSignatureTransformMethod(Result).
      Read(ADictionary.GetDictionary('TransformParams'));
  end
  else
    Result := nil;
end;

function TdxPDFCustomSignatureTransformMethod.GetValidVersion: string;
begin
  Result := '1.2';
end;

{ TdxPDFDocumentMDPSignatureTransformMethod }

class function TdxPDFDocumentMDPSignatureTransformMethod.GetTypeName: string;
begin
  Result := 'DocMDP';
end;

procedure TdxPDFDocumentMDPSignatureTransformMethod.Initialize;
begin
  inherited Initialize;
  FPermission := dapFormFillingAndSignatures;
end;

procedure TdxPDFDocumentMDPSignatureTransformMethod.ReadProperties
  (ADictionary: TdxPDFReaderDictionary);
begin
  inherited ReadProperties(ADictionary);
  FPermission := TdxPDFDocumentAccessPermission(ADictionary.GetInteger('P',
    Ord(FPermission)));
end;

{ TdxPDFFieldMDPSignatureTransformMethod }

class function TdxPDFFieldMDPSignatureTransformMethod.GetTypeName: string;
begin
  Result := 'FieldMDP';
end;

procedure TdxPDFFieldMDPSignatureTransformMethod.DestroySubClasses;
begin
  FreeAndNil(FLock);
  inherited DestroySubClasses;
end;

procedure TdxPDFFieldMDPSignatureTransformMethod.ReadProperties
  (ADictionary: TdxPDFReaderDictionary);
begin
  inherited ReadProperties(ADictionary);
  FLock := TdxPDFSignatureFieldLock.Parse(ADictionary);
end;

{ TdxPDFUsageRightsSignatureTransformMethod }

class function TdxPDFUsageRightsSignatureTransformMethod.GetTypeName: string;
begin
  Result := 'UR';
end;

function TdxPDFUsageRightsSignatureTransformMethod.GetValidVersion: string;
begin
  Result := '2.2';
end;

procedure TdxPDFUsageRightsSignatureTransformMethod.ReadProperties
  (ADictionary: TdxPDFReaderDictionary);
begin
  inherited ReadProperties(ADictionary);
  FAllowFullSave := IsAllowPermission(ADictionary, 'Document', 'FullSave');
  FAllowModifySignature := IsAllowPermission(ADictionary, 'Signature',
    'Modify');
  FMessageText := ADictionary.GetString('Msg');
  FAnnotationUsagePermissions := GetAnnotationPermissions(ADictionary);
  FInteractiveFormFieldUsagePermissions := GetInteractiveFormFieldPermissions
    (ADictionary);
  FEmbeddedFileUsagePermissions := GetEmbeddedFilePermissions(ADictionary);
  FRestrictOtherPermissions := ADictionary.GetBoolean('P', False);
end;

function TdxPDFUsageRightsSignatureTransformMethod.ConvertToAnnotationPermission
  (const AValue: string): TdxPDFAnnotationPermission;
begin
  if AValue = 'Create' then
    Result := apCreate
  else if AValue = 'Delete' then
    Result := apDelete
  else if AValue = 'Modify' then
    Result := apModify
  else if AValue = 'Copy' then
    Result := apCopy
  else if AValue = 'Import' then
    Result := apImport
  else if AValue = 'Export' then
    Result := apExport
  else if AValue = 'Online' then
    Result := apOnline
  else
    Result := apSummaryView
end;

function TdxPDFUsageRightsSignatureTransformMethod.
  ConvertToEmbeddedFilePermission(const AValue: string)
  : TdxPDFEmbeddedFilePermission;
begin
  if AValue = 'Create' then
    Result := fupCreate
  else if AValue = 'Delete' then
    Result := fupDelete
  else if AValue = 'Modify' then
    Result := fupModify
  else
    Result := fupImport
end;

function TdxPDFUsageRightsSignatureTransformMethod.
  ConvertToInteractiveFormFieldPermission(const AValue: string)
  : TdxPDFInteractiveFormFieldPermission;
begin
  if AValue = 'pAdd' then
    Result := ifpAdd
  else if AValue = 'Delete' then
    Result := ifpDelete
  else if AValue = 'FillIn' then
    Result := ifpFillIn
  else if AValue = 'Import' then
    Result := ifpImport
  else if AValue = 'Export' then
    Result := ifpExport
  else if AValue = 'SubmitStandalone' then
    Result := ifpSubmitStandalone
  else if AValue = 'SpawnTemplate' then
    Result := ifpSpawnTemplate
  else if AValue = 'BarcodePlaintext' then
    Result := ifpBarcodePlainText
  else
    Result := ifpOnline
end;

function TdxPDFUsageRightsSignatureTransformMethod.GetAnnotationPermissions
  (ADictionary: TdxPDFReaderDictionary): TdxPDFAnnotationPermissions;
var
  APermissions: TdxPDFAnnotationPermissions;
begin
  APermissions := [];
  ConvertPermissions(ADictionary, TdxPDFKeywords.Annotations,
    procedure(AValue: string)
    begin
      Include(APermissions, ConvertToAnnotationPermission(AValue));
    end);
  Result := APermissions;
end;

function TdxPDFUsageRightsSignatureTransformMethod.GetEmbeddedFilePermissions
  (ADictionary: TdxPDFReaderDictionary): TdxPDFEmbeddedFilePermissions;
var
  APermissions: TdxPDFEmbeddedFilePermissions;
begin
  APermissions := [];
  ConvertPermissions(ADictionary, TdxPDFKeywords.EmbeddedFileReference,
    procedure(AValue: string)
    begin
      Include(APermissions, ConvertToEmbeddedFilePermission(AValue));
    end);
  Result := APermissions;
end;

function TdxPDFUsageRightsSignatureTransformMethod.
  GetInteractiveFormFieldPermissions(ADictionary: TdxPDFReaderDictionary)
  : TdxPDFInteractiveFormFieldPermissions;
var
  APermissions: TdxPDFInteractiveFormFieldPermissions;
begin
  APermissions := [];
  ConvertPermissions(ADictionary, TdxPDFKeywords.Form, procedure(AValue: string)
    begin
      Include(APermissions, ConvertToInteractiveFormFieldPermission(AValue));
    end);
  Result := APermissions;
end;

function TdxPDFUsageRightsSignatureTransformMethod.IsAllowPermission
  (ADictionary: TdxPDFReaderDictionary;
const AKey, AExpectedValue: string): Boolean;
var
  AArray: TdxPDFArray;
begin
  Result := ADictionary.TryGetArray(AKey, AArray) and (AArray.Count = 1) and
    (AArray[0].ObjectType = otString) and
    (TdxPDFString(AArray[0]).Text = AExpectedValue);
end;

procedure TdxPDFUsageRightsSignatureTransformMethod.ConvertPermissions
  (ADictionary: TdxPDFReaderDictionary; const AKey: string;
AConvertElementProc: TProc<string>);
var
  AArray: TdxPDFArray;
  AElement: TdxPDFBase;
begin
  if ADictionary.TryGetArray(AKey, AArray) then
    for AElement in AArray.ElementList do
      if AElement.ObjectType = otString then
        AConvertElementProc(TdxPDFString(AElement).Text)
end;

{ TdxPDFSignatureReference }

procedure TdxPDFSignatureReference.DestroySubClasses;
begin
  FreeAndNil(FTransformMethod);
  inherited DestroySubClasses;
end;

procedure TdxPDFSignatureReference.Initialize;
begin
  inherited Initialize;
  FDigestMethod := 'MD5';
end;

procedure TdxPDFSignatureReference.ReadProperties
  (ADictionary: TdxPDFReaderDictionary);
begin
  inherited ReadProperties(ADictionary);
  FDigestMethod := ADictionary.GetString('DigestMethod', FDigestMethod);
  FTransformMethod := TdxPDFCustomSignatureTransformMethod.Parse(ADictionary);
end;

{ TdxPDFSignatureField }

constructor TdxPDFSignatureField.Create(AForm: TdxPDFInteractiveForm;
AWidget: TdxPDFCustomAnnotation; ASignature: TdxPDFSignature);
begin
  inherited Create(AForm, AWidget);
  FSignature := ASignature;
  FCanWriteSignature := True;
  Form.SignatureFlags := [sfAppendOnly, sfSignaturesExist];
end;

class function TdxPDFSignatureField.GetTypeName: string;
begin
  Result := 'Sig';
end;

function TdxPDFSignatureField.GetName: string;
begin
  if FSignature <> nil then
    Result := FSignature.Name
  else
    Result := inherited GetName;
end;

procedure TdxPDFSignatureField.DestroySubClasses;
begin
  FreeAndNil(FSignature);
  FreeAndNil(FLock);
  inherited DestroySubClasses;
end;

procedure TdxPDFSignatureField.Read(ADictionary: TdxPDFReaderDictionary;
ANumber: Integer);
begin
  inherited Read(ADictionary, ANumber);
  FSignature := TdxPDFSignature.Parse
    (ADictionary.GetDictionary(TdxPDFKeywords.ShortValue,
    TdxPDFKeywords.Contents));
  FLock := TdxPDFSignatureFieldLock.Parse
    (ADictionary.GetDictionary(TdxPDFKeywords.Lock));
end;

procedure TdxPDFSignatureField.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  if FCanWriteSignature and (Widget <> nil) then
  begin
    FSignature.UpdateSignTime;
    Widget.Modified := FSignature.SignTime;
  end;
  inherited Write(AHelper, ADictionary);
  ADictionary.AddReference(TdxPDFKeywords.Lock, FLock);
  if FCanWriteSignature then
    ADictionary.AddReference(TdxPDFKeywords.ShortValue, FSignature);
end;

function TdxPDFSignatureField.CreateInfo: TdxPDFSignatureFieldInfo;
begin
  Result := TdxPDFSignatureFieldInfo.Create;
  Result.FField := Self;
  if FSignature <> nil then
    FSignature.Populate(Result);
end;

{ TdxPDFSignature }

class function TdxPDFSignature.Parse(ADictionary: TdxPDFReaderDictionary)
  : TdxPDFSignature;
begin
  if ADictionary = nil then
    Exit(nil);
  Result := TdxPDFSignature.Create;
  Result.Read(ADictionary);
end;

constructor TdxPDFSignature.Create(AInfo: TdxPDFSignatureFieldInfo);
begin
  inherited Create;
  FFilter := 'Adobe.PPKMS';
  FSubFilter := 'adbe.pkcs7.sha1';
  FName := TdxPDFUtils.GenerateGUID;
  Certificate := AInfo.Certificate;
  ContactInfo := AInfo.ContactInfo;
  Location := AInfo.Location;
  Reason := AInfo.Reason;
end;

procedure TdxPDFSignature.Patch(AWriter: TdxPDFWriter);
var
  ADataToSign, ABuffer: TBytes;
  ASavedPosition, ASecondByteRangeLength, ASecondByteRangeOffset: Int64;
begin
  if FContentPlaceHolder.IsValid and FByteRangesPlaceHolder.IsValid then
  begin
    ASavedPosition := AWriter.Stream.Position;
    try
      ASecondByteRangeOffset := FContentPlaceHolder.Offset +
        FContentPlaceHolder.Size;
      ASecondByteRangeLength := AWriter.Stream.Size - ASecondByteRangeOffset;

      AWriter.Stream.Position := FByteRangesPlaceHolder.Offset;
      AWriter.WriteString('[ 0 ' + IntToStr(FContentPlaceHolder.Offset) + ' ' +
        IntToStr(ASecondByteRangeOffset) + ' ' +
        IntToStr(ASecondByteRangeLength) + ']');

      AWriter.Stream.Position := 0;
      SetLength(ADataToSign, FContentPlaceHolder.Offset);
      ADataToSign := AWriter.Stream.ReadArray(FContentPlaceHolder.Offset);
      AWriter.Stream.Position := ASecondByteRangeOffset;
      SetLength(ABuffer, ASecondByteRangeLength);
      ABuffer := AWriter.Stream.ReadArray(ASecondByteRangeLength);
      ADataToSign := TdxByteArray.Concatenate(ADataToSign, ABuffer);

      AWriter.Stream.Position := FContentPlaceHolder.Offset;
      WriteContent(AWriter,
        SignData(TdxSHA1HashAlgorithm.Calculate(ADataToSign)));
    finally
      AWriter.Stream.Position := ASavedPosition;
    end;
  end;
end;

procedure TdxPDFSignature.Populate(AInfo: TdxPDFSignatureFieldInfo);
begin
  AInfo.Location := Location;
  AInfo.Reason := Reason;
  AInfo.ContactInfo := ContactInfo;
  AInfo.SetSignedTime(SignTime);
end;

procedure TdxPDFSignature.UpdateSignTime;
begin
  FSignTime := dxGetNowInUTC;
end;

procedure TdxPDFSignature.CreateSubClasses;
begin
  inherited CreateSubClasses;
  ByteRangesPlaceHolder := nil;
  ContentPlaceHolder := nil;
  FReferences := TObjectList<TdxPDFSignatureReference>.Create;
end;

procedure TdxPDFSignature.DestroySubClasses;
begin
  FreeAndNil(FReferences);
  ContentPlaceHolder := nil;
  ByteRangesPlaceHolder := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFSignature.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  FFilter := ADictionary.GetString(TdxPDFKeywords.Filter);
  FSubFilter := ADictionary.GetString(TdxPDFKeywords.SubFilter);
  FContent := ADictionary.GetBytes(TdxPDFKeywords.Contents);
  FName := ADictionary.GetString(TdxPDFKeywords.Name);
  FSignTime := ADictionary.GetDate(TdxPDFKeywords.Modified);
  FLocation := ADictionary.GetString(TdxPDFKeywords.Location);
  FReason := ADictionary.GetString(TdxPDFKeywords.Reason);
  FContactInfo := ADictionary.GetString(TdxPDFKeywords.ContactInfo);
  ReadByteRange(ADictionary);
  ReadReference(ADictionary);
  ReadChanges(ADictionary);
end;

procedure TdxPDFSignature.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);

  function GetContentLength: Integer;
  var
    AData: TBytes;
  begin
    SetLength(AData, 20);
    Result := Length(SignData(AData)) * 2 + 2;
  end;

begin
  inherited Write(AHelper, ADictionary);
  ADictionary.AddName(TdxPDFKeywords.Filter, Filter);
  ADictionary.AddName(TdxPDFKeywords.SubFilter, SubFilter);
  ADictionary.Add(TdxPDFKeywords.Location, Location);
  ADictionary.Add(TdxPDFKeywords.Reason, Reason);
  ADictionary.Add(TdxPDFKeywords.ContactInfo, ContactInfo);
  ADictionary.AddDate(TdxPDFKeywords.Modified, FSignTime);
  ADictionary.Add(TdxPDFKeywords.Name, Name);

  ContentPlaceHolder := TdxPDFPlaceHolder.Create(GetContentLength);
  ADictionary.Add(TdxPDFKeywords.Contents, FContentPlaceHolder);

  ByteRangesPlaceHolder := TdxPDFPlaceHolder.Create(36);
  ADictionary.Add(TdxPDFKeywords.ByteRange, FByteRangesPlaceHolder);
end;

procedure TdxPDFSignature.SetByteRangesPlaceHolder(const AValue
  : TdxPDFPlaceHolder);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FByteRangesPlaceHolder));
end;

procedure TdxPDFSignature.SetCertificate(const AValue: TdxX509Certificate);
begin
  FreeAndNil(FCertificate);
  FCertificate := AValue;
end;

procedure TdxPDFSignature.SetContentPlaceHolder(const AValue
  : TdxPDFPlaceHolder);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FContentPlaceHolder));
end;

function TdxPDFSignature.SignData(const AData: TBytes): TBytes;
begin
  Result := dxX509SHA1SignData(AData, Certificate);
end;

procedure TdxPDFSignature.ReadByteRange(ADictionary: TdxPDFReaderDictionary);
var
  AArray: TdxPDFArray;
  I, AIndex, ACount, AStart, ALength: Integer;
begin
  if not ADictionary.TryGetArray(TdxPDFKeywords.ByteRange, AArray) then
    Exit;
  if (AArray.Count >= 4) and (AArray.Count mod 2 = 0) then
  begin
    ACount := AArray.Count div 2;
    SetLength(FByteRanges, ACount);
    AIndex := 0;
    for I := 0 to ACount - 1 do
    begin
      AStart := TdxPDFUtils.ConvertToInt(AArray[AIndex]);
      ALength := TdxPDFUtils.ConvertToInt(AArray[AIndex + 1]);
      Inc(AIndex, 2);
      if (AStart < 0) or (ALength < 0) then
        TdxPDFUtils.RaiseTestException;
      FByteRanges[I] := TdxPDFSignatureByteRange.Create(AStart, ALength);
    end;
  end;
end;

procedure TdxPDFSignature.ReadChanges(ADictionary: TdxPDFReaderDictionary);
var
  AArray: TdxPDFArray;
begin
  if not ADictionary.TryGetArray('Changes', AArray) then
    Exit;
  if AArray.Count = 3 then
  begin
    FAlteredPageCount := TdxPDFUtils.ConvertToInt(AArray[0]);
    FAlteredInteractiveFormFieldCount := TdxPDFUtils.ConvertToInt(AArray[1]);
    FFilledInInteractiveFormFieldCount := TdxPDFUtils.ConvertToInt(AArray[2]);
  end;
end;

procedure TdxPDFSignature.ReadReference(ADictionary: TdxPDFReaderDictionary);
var
  AArray: TdxPDFArray;
  AObject: TdxPDFBase;
  AReference: TdxPDFSignatureReference;
  I: Integer;
begin
  FReferences.Clear;
  if not ADictionary.TryGetArray('Reference', AArray) then
    Exit;
  for I := 0 to AArray.Count - 1 do
  begin
    AObject := ADictionary.Repository.ResolveReference(AArray[I]);
    if AObject.ObjectType = otDictionary then
    begin
      AReference := TdxPDFSignatureReference.Create;
      AReference.Read(AObject as TdxPDFReaderDictionary);
      FReferences.Add(AReference);
    end;
  end;
end;

procedure TdxPDFSignature.WriteContent(AWriter: TdxPDFWriter;
const AData: TBytes);

  function Convert(AValue: Integer): Byte;
  begin
    Result := IfThen(AValue > 9, AValue + $37, AValue + $30);
  end;

var
  ACurrentByte: Byte;
  AHexData: TBytes;
  I, J: Integer;
begin
  SetLength(AHexData, FContentPlaceHolder.Size);
  AHexData[0] := 60;
  AHexData[FContentPlaceHolder.Size - 1] := 62;

  J := 0;
  for I := 0 to Length(AData) - 1 do
  begin
    ACurrentByte := AData[I];
    AHexData[J + 1] := Convert(ACurrentByte shr 4);
    AHexData[J + 2] := Convert(ACurrentByte and 15);
    Inc(J, 2);
  end;

  AWriter.Stream.Position := FContentPlaceHolder.Offset;
  AWriter.Stream.WriteArray(AHexData, FContentPlaceHolder.Size);
end;

initialization

dxPDFRegisterDocumentObjectClass(TdxPDFSignatureField);
dxPDFRegisterDocumentObjectClass(TdxPDFDocumentMDPSignatureTransformMethod);
dxPDFRegisterDocumentObjectClass(TdxPDFFieldMDPSignatureTransformMethod);
dxPDFRegisterDocumentObjectClass(TdxPDFUsageRightsSignatureTransformMethod);

finalization

dxPDFUnregisterDocumentObjectClass(TdxPDFUsageRightsSignatureTransformMethod);
dxPDFUnregisterDocumentObjectClass(TdxPDFFieldMDPSignatureTransformMethod);
dxPDFUnregisterDocumentObjectClass(TdxPDFDocumentMDPSignatureTransformMethod);
dxPDFUnregisterDocumentObjectClass(TdxPDFSignatureField);

end.
