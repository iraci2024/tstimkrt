{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressGanttControl }
{ }
{ Copyright (c) 2020 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSGANTTCONTROL AND ALL }
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

unit dxGanttControlOutlineCodes;

{$I cxVer.inc}
{$I dxGanttControl.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes, System.Contnrs,
{$ENDIF}
  SysUtils, Generics.Defaults, Generics.Collections, Classes,
  dxCore, dxCoreClasses,
  dxGanttControlCustomClasses,
  dxGanttControlCustomDataModel;

type
  TdxGanttControlOutlineCodes = class;

  { TdxGanttControlOutlineCodeReference }

  TdxGanttOutlineCodeReferenceAssignedValue = (FieldID, ValueID, ValueGUID);
  TdxGanttOutlineCodeReferenceAssignedValues = set of
    TdxGanttOutlineCodeReferenceAssignedValue;

  TdxGanttControlOutlineCodeReference = class
    (TdxGanttControlModelElementListItem)
  strict private
    FAssignedValues: TdxGanttOutlineCodeReferenceAssignedValues;
    FFieldID: Integer;
    FValueID: Integer;
    FValueGUID: string;
    procedure SetFieldID(const Value: Integer);
    procedure SetValueID(const AValue: Integer);
    procedure SetValueGUID(const Value: string);
  protected
    procedure DoReset; override;
  public
    procedure Assign(Source: TPersistent); override;
{$REGION 'for internal use'}
    function IsValueAssigned(const AValue
      : TdxGanttOutlineCodeReferenceAssignedValue): Boolean;
    procedure ResetValue(const AValue
      : TdxGanttOutlineCodeReferenceAssignedValue); overload;
{$ENDREGION 'for internal use'}
    property FieldID: Integer read FFieldID write SetFieldID;
    property ValueID: Integer read FValueID write SetValueID;
    property ValueGUID: string read FValueGUID write SetValueGUID;
  end;

  { TdxGanttControlOutlineCodeReferences }

  TdxGanttControlOutlineCodeReferences = class
    (TdxGanttControlElementCustomOwnedList)
  strict private
    function GetItem(Index: Integer)
      : TdxGanttControlOutlineCodeReference; inline;
  protected
    function CreateItem: TdxGanttControlModelElementListItem; override;
  public
    function Append: TdxGanttControlOutlineCodeReference;
    procedure Clear;
    procedure Remove(AItem: TdxGanttControlOutlineCodeReference);

    property Items[Index: Integer]: TdxGanttControlOutlineCodeReference
      read GetItem; default;
  end;

  { TdxGanttControlOutlineCodeMask }

  TdxGanttControlOutlineCodeMaskType = (Numbers, UppercaseLetters,
    LowercaseLetters, Characters);

  TdxGanttControlOutlineCodeValueType = (Date = 4, Duration = 6, Cost = 9,
    Number = 15, Flag = 17, Text = 21, FinishDate = 27);

  TdxGanttOutlineCodeMaskAssignedValue = (Level, &Type, Length, Separator);
  TdxGanttOutlineCodeMaskAssignedValues = set of
    TdxGanttOutlineCodeMaskAssignedValue;

  TdxGanttControlOutlineCodeMask = class(TdxGanttControlModelElementListItem)
  strict private
    FAssignedValues: TdxGanttOutlineCodeMaskAssignedValues;
    FLevel: Integer;
    FType: TdxGanttControlOutlineCodeMaskType;
    FLength: Integer;
    FSeparator: string;
    procedure SetLength(const Value: Integer);
    procedure SetLevel(const Value: Integer);
    procedure SetSeparator(const Value: string);
    procedure SetType(const Value: TdxGanttControlOutlineCodeMaskType);
  protected
    procedure DoReset; override;
  public
    procedure Assign(Source: TPersistent); override;
{$REGION 'for internal use'}
    function IsValueAssigned(const AValue
      : TdxGanttOutlineCodeMaskAssignedValue): Boolean;
    procedure ResetValue(const AValue
      : TdxGanttOutlineCodeMaskAssignedValue); overload;
{$ENDREGION 'for internal use'}
    property Level: Integer read FLevel write SetLevel;
    property &Type: TdxGanttControlOutlineCodeMaskType read FType write SetType;
    property Length: Integer read FLength write SetLength;
    property Separator: string read FSeparator write SetSeparator;
  end;

  { TdxGanttControlOutlineCodeMasks }

  TdxGanttControlOutlineCodeMasks = class(TdxGanttControlElementCustomOwnedList)
  strict private
    function GetItem(Index: Integer): TdxGanttControlOutlineCodeMask; inline;
  protected
    function CreateItem: TdxGanttControlModelElementListItem; override;
  public
    function Append: TdxGanttControlOutlineCodeMask;
    procedure Clear;
    procedure Remove(AItem: TdxGanttControlOutlineCodeMask);

    property Items[Index: Integer]: TdxGanttControlOutlineCodeMask
      read GetItem; default;
  end;

  { TdxGanttControlOutlineCodeValue }

  TdxGanttOutlineCodeValueAssignedValue = (Collapsed, ValueID, ParentValueID,
    Value, Description);
  TdxGanttOutlineCodeValueAssignedValues = set of
    TdxGanttOutlineCodeValueAssignedValue;

  TdxGanttControlOutlineCodeValue = class(TdxGanttControlModelElementListItem)
  strict private
    FAssignedValues: TdxGanttOutlineCodeValueAssignedValues;
    FCollapsed: Boolean;
    FValueID: Integer;
    FFieldGUID: string;
    FType: TdxGanttControlOutlineCodeValueType;
    FParentValueID: Integer;
    FValue: string;
    FDescription: string;

    procedure SetDescription(const Value: string);
    procedure SetFieldGUID(const Value: string);
    procedure SetCollapsed(const Value: Boolean);
    procedure SetParentValueID(const Value: Integer);
    procedure SetType(const Value: TdxGanttControlOutlineCodeValueType);
    procedure SetValue(const AValue: string);
    procedure SetValueID(const Value: Integer);
  protected
    procedure DoReset; override;
  public
    procedure Assign(Source: TPersistent); override;
{$REGION 'for internal use'}
    function IsValueAssigned(const AValue
      : TdxGanttOutlineCodeValueAssignedValue): Boolean;
    procedure ResetValue(const AValue
      : TdxGanttOutlineCodeValueAssignedValue); overload;
{$ENDREGION 'for internal use'}
    property Collapsed: Boolean read FCollapsed write SetCollapsed;
    property ValueID: Integer read FValueID write SetValueID;
    property FieldGUID: string read FFieldGUID write SetFieldGUID;
    property &Type: TdxGanttControlOutlineCodeValueType read FType
      write SetType;
    property ParentValueID: Integer read FParentValueID write SetParentValueID;
    property Value: string read FValue write SetValue;
    property Description: string read FDescription write SetDescription;
  end;

  { TdxGanttControlOutlineCodeValues }

  TdxGanttControlOutlineCodeValues = class
    (TdxGanttControlElementCustomOwnedList)
  strict private
    function GetItem(Index: Integer): TdxGanttControlOutlineCodeValue; inline;
  protected
    function CreateItem: TdxGanttControlModelElementListItem; override;
  public
    function Append: TdxGanttControlOutlineCodeValue;
    procedure Clear;
    procedure Remove(AItem: TdxGanttControlOutlineCodeValue);

    property Items[Index: Integer]: TdxGanttControlOutlineCodeValue
      read GetItem; default;
  end;

  { TdxGanttControlOutlineCode }

  TdxGanttOutlineCodeAssignedValue = (FieldID, FieldName, Alias, PhoneticAlias,
    Enterprise, EnterpriseOutlineCodeAlias, ShowIndent,
    ResourceSubstitutionEnabled, LeafOnly, AllLevelsRequired,
    OnlyTableValuesAllowed);

  TdxGanttOutlineCodeAssignedValues = set of TdxGanttOutlineCodeAssignedValue;

  TdxGanttControlOutlineCode = class(TdxGanttControlModelElementListItem)
  strict private
    FAssignedValues: TdxGanttOutlineCodeAssignedValues;
    FGUID: string;
    FFieldID: Integer;
    FFieldName: string;
    FAlias: string;
    FPhoneticAlias: string;
    FEnterprise: Boolean;
    FEnterpriseOutlineCodeAlias: Integer;
    FShowIndent: Boolean;
    FResourceSubstitutionEnabled: Boolean;
    FLeafOnly: Boolean;
    FAllLevelsRequired: Boolean;
    FOnlyTableValuesAllowed: Boolean;
    FMasks: TdxGanttControlOutlineCodeMasks;
    FValues: TdxGanttControlOutlineCodeValues;

    procedure SetAlias(const Value: string);
    procedure SetAllLevelsRequired(const Value: Boolean);
    procedure SetEnterprise(const Value: Boolean);
    procedure SetEnterpriseOutlineCodeAlias(const Value: Integer);
    procedure SetFieldID(const Value: Integer);
    procedure SetFieldName(const Value: string);
    procedure SetGUID(const Value: string);
    procedure SetLeafOnly(const Value: Boolean);
    procedure SetMasks(const Value: TdxGanttControlOutlineCodeMasks);
    procedure SetOnlyTableValuesAllowed(const Value: Boolean);
    procedure SetPhoneticAlias(const Value: string);
    procedure SetResourceSubstitutionEnabled(const Value: Boolean);
    procedure SetShowIndent(const Value: Boolean);
    procedure SetValues(const Value: TdxGanttControlOutlineCodeValues);
  protected
    procedure DoReset; override;
  public
    constructor Create(AOwner: TdxGanttControlModelElement); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
{$REGION 'for internal use'}
    function IsValueAssigned(const AValue
      : TdxGanttOutlineCodeAssignedValue): Boolean;
    procedure ResetValue(const AValue
      : TdxGanttOutlineCodeAssignedValue); overload;
{$ENDREGION 'for internal use'}
    property GUID: string read FGUID write SetGUID;
    property FieldID: Integer read FFieldID write SetFieldID;
    property FieldName: string read FFieldName write SetFieldName;
    property Alias: string read FAlias write SetAlias;
    property PhoneticAlias: string read FPhoneticAlias write SetPhoneticAlias;
    property Enterprise: Boolean read FEnterprise write SetEnterprise;
    property EnterpriseOutlineCodeAlias: Integer
      read FEnterpriseOutlineCodeAlias write SetEnterpriseOutlineCodeAlias;
    property ResourceSubstitutionEnabled: Boolean
      read FResourceSubstitutionEnabled write SetResourceSubstitutionEnabled;
    property ShowIndent: Boolean read FShowIndent write SetShowIndent;
    property LeafOnly: Boolean read FLeafOnly write SetLeafOnly;
    property AllLevelsRequired: Boolean read FAllLevelsRequired
      write SetAllLevelsRequired;
    property OnlyTableValuesAllowed: Boolean read FOnlyTableValuesAllowed
      write SetOnlyTableValuesAllowed;
    property Masks: TdxGanttControlOutlineCodeMasks read FMasks write SetMasks;
    property Values: TdxGanttControlOutlineCodeValues read FValues
      write SetValues;
  end;

  { TdxGanttControlOutlineCodes }

  TdxGanttControlOutlineCodes = class(TdxGanttControlModelElementList)
  strict private
    function GetItem(Index: Integer): TdxGanttControlOutlineCode; inline;
  protected
    function CreateItem: TdxGanttControlModelElementListItem; override;
  public
    function Append: TdxGanttControlOutlineCode;
    procedure Clear;
    procedure Remove(AItem: TdxGanttControlOutlineCode);

    property Items[Index: Integer]: TdxGanttControlOutlineCode
      read GetItem; default;
  end;

implementation

uses
  dxGanttControlUtils, RTLConsts;

{ TdxGanttControlOutlineCodeReference }

procedure TdxGanttControlOutlineCodeReference.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlOutlineCodeReference;
begin
  if Source is TdxGanttControlOutlineCodeReference then
  begin
    ASource := TdxGanttControlOutlineCodeReference(Source);
    BeginUpdate;
    try
      FieldID := ASource.FieldID;
      ValueID := ASource.ValueID;
      ValueGUID := ASource.ValueGUID;
      FAssignedValues := ASource.FAssignedValues;
    finally
      EndUpdate;
    end;
  end;
  inherited Assign(Source);
end;

procedure TdxGanttControlOutlineCodeReference.DoReset;
begin
  FAssignedValues := [];
end;

function TdxGanttControlOutlineCodeReference.IsValueAssigned
  (const AValue: TdxGanttOutlineCodeReferenceAssignedValue): Boolean;
begin
  Result := AValue in FAssignedValues;
end;

procedure TdxGanttControlOutlineCodeReference.ResetValue
  (const AValue: TdxGanttOutlineCodeReferenceAssignedValue);
begin
  if IsValueAssigned(AValue) then
  begin
    Exclude(FAssignedValues, AValue);
    Changed;
  end
end;

procedure TdxGanttControlOutlineCodeReference.SetFieldID(const Value: Integer);
begin
  if (FieldID <> Value) or not IsValueAssigned
    (TdxGanttOutlineCodeReferenceAssignedValue.FieldID) then
  begin
    Include(FAssignedValues, TdxGanttOutlineCodeReferenceAssignedValue.FieldID);
    FFieldID := Value;
    Changed;
  end;
end;

procedure TdxGanttControlOutlineCodeReference.SetValueID(const AValue: Integer);
begin
  if (FValueID <> AValue) or not IsValueAssigned
    (TdxGanttOutlineCodeReferenceAssignedValue.ValueID) then
  begin
    Include(FAssignedValues, TdxGanttOutlineCodeReferenceAssignedValue.ValueID);
    FValueID := AValue;
    Changed;
  end;
end;

procedure TdxGanttControlOutlineCodeReference.SetValueGUID(const Value: string);
begin
  if (ValueGUID <> Value) or not IsValueAssigned
    (TdxGanttOutlineCodeReferenceAssignedValue.ValueGUID) then
  begin
    Include(FAssignedValues, TdxGanttOutlineCodeReferenceAssignedValue.
      ValueGUID);
    FValueGUID := Value;
    Changed;
  end;
end;

{ TdxGanttControlOutlineCodeReferences }

function TdxGanttControlOutlineCodeReferences.Append
  : TdxGanttControlOutlineCodeReference;
begin
  Result := TdxGanttControlOutlineCodeReference(CreateItem);
  InternalAdd(Result);
end;

procedure TdxGanttControlOutlineCodeReferences.Clear;
begin
  InternalClear;
end;

function TdxGanttControlOutlineCodeReferences.CreateItem
  : TdxGanttControlModelElementListItem;
begin
  Result := TdxGanttControlOutlineCodeReference.Create(Self);
end;

function TdxGanttControlOutlineCodeReferences.GetItem(Index: Integer)
  : TdxGanttControlOutlineCodeReference;
begin
  Result := TdxGanttControlOutlineCodeReference(inherited Items[Index]);
end;

procedure TdxGanttControlOutlineCodeReferences.Remove
  (AItem: TdxGanttControlOutlineCodeReference);
begin
  InternalRemove(AItem);
end;

{ TdxGanttControlOutlineCodeMask }

procedure TdxGanttControlOutlineCodeMask.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlOutlineCodeMask;
begin
  if Source is TdxGanttControlOutlineCodeMask then
  begin
    ASource := TdxGanttControlOutlineCodeMask(Source);
    BeginUpdate;
    try
      Level := ASource.Level;
      &Type := ASource.&Type;
      Length := ASource.Length;
      Separator := ASource.Separator;
      FAssignedValues := ASource.FAssignedValues;
    finally
      EndUpdate;
    end;
  end;
  inherited Assign(Source);
end;

procedure TdxGanttControlOutlineCodeMask.DoReset;
begin
  FAssignedValues := [];
end;

function TdxGanttControlOutlineCodeMask.IsValueAssigned
  (const AValue: TdxGanttOutlineCodeMaskAssignedValue): Boolean;
begin
  Result := AValue in FAssignedValues;
end;

procedure TdxGanttControlOutlineCodeMask.ResetValue(const AValue
  : TdxGanttOutlineCodeMaskAssignedValue);
begin
  if IsValueAssigned(AValue) then
  begin
    Exclude(FAssignedValues, AValue);
    Changed;
  end
end;

procedure TdxGanttControlOutlineCodeMask.SetLength(const Value: Integer);
begin
  if (Length <> Value) or not IsValueAssigned
    (TdxGanttOutlineCodeMaskAssignedValue.Length) then
  begin
    Include(FAssignedValues, TdxGanttOutlineCodeMaskAssignedValue.Length);
    FLength := Value;
    Changed;
  end;
end;

procedure TdxGanttControlOutlineCodeMask.SetLevel(const Value: Integer);
begin
  if (Level <> Value) or not IsValueAssigned
    (TdxGanttOutlineCodeMaskAssignedValue.Level) then
  begin
    Include(FAssignedValues, TdxGanttOutlineCodeMaskAssignedValue.Level);
    FLevel := Value;
    Changed;
  end;
end;

procedure TdxGanttControlOutlineCodeMask.SetSeparator(const Value: string);
begin
  if (Separator <> Value) or not IsValueAssigned
    (TdxGanttOutlineCodeMaskAssignedValue.Separator) then
  begin
    Include(FAssignedValues, TdxGanttOutlineCodeMaskAssignedValue.Separator);
    FSeparator := Value;
    Changed;
  end;
end;

procedure TdxGanttControlOutlineCodeMask.SetType(const Value
  : TdxGanttControlOutlineCodeMaskType);
begin
  if (FType <> Value) or not IsValueAssigned
    (TdxGanttOutlineCodeMaskAssignedValue.&Type) then
  begin
    Include(FAssignedValues, TdxGanttOutlineCodeMaskAssignedValue.&Type);
    FType := Value;
    Changed;
  end;
end;

{ TdxGanttControlOutlineCodeMasks }

function TdxGanttControlOutlineCodeMasks.Append: TdxGanttControlOutlineCodeMask;
begin
  Result := TdxGanttControlOutlineCodeMask(CreateItem);
  InternalAdd(Result);
end;

procedure TdxGanttControlOutlineCodeMasks.Clear;
begin
  InternalClear;
end;

function TdxGanttControlOutlineCodeMasks.CreateItem
  : TdxGanttControlModelElementListItem;
begin
  Result := TdxGanttControlOutlineCodeMask.Create(Self);
end;

function TdxGanttControlOutlineCodeMasks.GetItem(Index: Integer)
  : TdxGanttControlOutlineCodeMask;
begin
  Result := TdxGanttControlOutlineCodeMask(inherited Items[Index]);
end;

procedure TdxGanttControlOutlineCodeMasks.Remove
  (AItem: TdxGanttControlOutlineCodeMask);
begin
  InternalRemove(AItem);
end;

{ TdxGanttControlOutlineCodeValue }

procedure TdxGanttControlOutlineCodeValue.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlOutlineCodeValue;
begin
  if Source is TdxGanttControlOutlineCodeValue then
  begin
    ASource := TdxGanttControlOutlineCodeValue(Source);
    BeginUpdate;
    try
      ValueID := ASource.ValueID;
      FieldGUID := ASource.FieldGUID;
      &Type := ASource.&Type;
      ParentValueID := ASource.ParentValueID;
      Value := ASource.Value;
      Description := ASource.Description;
      Collapsed := ASource.Collapsed;
      FAssignedValues := ASource.FAssignedValues;
    finally
      EndUpdate;
    end;
  end;
  inherited Assign(Source);
end;

procedure TdxGanttControlOutlineCodeValue.DoReset;
begin
  FFieldGUID := TdxGanttControlUtils.GenerateGUID;
  FType := TdxGanttControlOutlineCodeValueType.Text;
  FAssignedValues := [];
end;

function TdxGanttControlOutlineCodeValue.IsValueAssigned
  (const AValue: TdxGanttOutlineCodeValueAssignedValue): Boolean;
begin
  Result := AValue in FAssignedValues;
end;

procedure TdxGanttControlOutlineCodeValue.ResetValue(const AValue
  : TdxGanttOutlineCodeValueAssignedValue);
begin
  if IsValueAssigned(AValue) then
  begin
    Exclude(FAssignedValues, AValue);
    Changed;
  end
end;

procedure TdxGanttControlOutlineCodeValue.SetDescription(const Value: string);
begin
  if (Description <> Value) or not IsValueAssigned
    (TdxGanttOutlineCodeValueAssignedValue.Description) then
  begin
    Include(FAssignedValues, TdxGanttOutlineCodeValueAssignedValue.Description);
    FDescription := Value;
    Changed;
  end;
end;

procedure TdxGanttControlOutlineCodeValue.SetFieldGUID(const Value: string);
begin
  if FieldGUID <> Value then
  begin
    FFieldGUID := Value;
    Changed;
  end;
end;

procedure TdxGanttControlOutlineCodeValue.SetCollapsed(const Value: Boolean);
begin
  if (Collapsed <> Value) or not IsValueAssigned
    (TdxGanttOutlineCodeValueAssignedValue.Collapsed) then
  begin
    Include(FAssignedValues, TdxGanttOutlineCodeValueAssignedValue.Collapsed);
    FCollapsed := Value;
    Changed;
  end;
end;

procedure TdxGanttControlOutlineCodeValue.SetParentValueID
  (const Value: Integer);
begin
  if (ParentValueID <> Value) or not IsValueAssigned
    (TdxGanttOutlineCodeValueAssignedValue.ParentValueID) then
  begin
    Include(FAssignedValues,
      TdxGanttOutlineCodeValueAssignedValue.ParentValueID);
    FParentValueID := Value;
    Changed;
  end;
end;

procedure TdxGanttControlOutlineCodeValue.SetType
  (const Value: TdxGanttControlOutlineCodeValueType);
begin
  if &Type <> Value then
  begin
    FType := Value;
    Changed;
  end;
end;

procedure TdxGanttControlOutlineCodeValue.SetValue(const AValue: string);
begin
  if (FValue <> AValue) or not IsValueAssigned
    (TdxGanttOutlineCodeValueAssignedValue.Value) then
  begin
    Include(FAssignedValues, TdxGanttOutlineCodeValueAssignedValue.Value);
    FValue := AValue;
    Changed;
  end;
end;

procedure TdxGanttControlOutlineCodeValue.SetValueID(const Value: Integer);
begin
  if (ValueID <> Value) or not IsValueAssigned
    (TdxGanttOutlineCodeValueAssignedValue.ValueID) then
  begin
    Include(FAssignedValues, TdxGanttOutlineCodeValueAssignedValue.ValueID);
    FValueID := Value;
    Changed;
  end;
end;

{ TdxGanttControlOutlineCodeValues }

function TdxGanttControlOutlineCodeValues.Append
  : TdxGanttControlOutlineCodeValue;
begin
  Result := TdxGanttControlOutlineCodeValue(CreateItem);
  InternalAdd(Result);
end;

procedure TdxGanttControlOutlineCodeValues.Clear;
begin
  InternalClear;
end;

function TdxGanttControlOutlineCodeValues.CreateItem
  : TdxGanttControlModelElementListItem;
begin
  Result := TdxGanttControlOutlineCodeValue.Create(Self);
end;

function TdxGanttControlOutlineCodeValues.GetItem(Index: Integer)
  : TdxGanttControlOutlineCodeValue;
begin
  Result := TdxGanttControlOutlineCodeValue(inherited Items[Index]);
end;

procedure TdxGanttControlOutlineCodeValues.Remove
  (AItem: TdxGanttControlOutlineCodeValue);
begin
  InternalRemove(AItem);
end;

{ TdxGanttControlOutlineCode }

constructor TdxGanttControlOutlineCode.Create
  (AOwner: TdxGanttControlModelElement);
begin
  inherited Create(AOwner);
  FMasks := TdxGanttControlOutlineCodeMasks.Create(Self);
  FValues := TdxGanttControlOutlineCodeValues.Create(Self);
end;

destructor TdxGanttControlOutlineCode.Destroy;
begin
  FreeAndNil(FMasks);
  FreeAndNil(FValues);
  inherited Destroy;
end;

procedure TdxGanttControlOutlineCode.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlOutlineCode;
begin
  if Source is TdxGanttControlOutlineCode then
  begin
    ASource := TdxGanttControlOutlineCode(Source);
    BeginUpdate;
    try
      GUID := ASource.GUID;
      FieldID := ASource.FieldID;
      FieldName := ASource.FieldName;
      Alias := ASource.Alias;
      PhoneticAlias := ASource.PhoneticAlias;
      Enterprise := ASource.Enterprise;
      EnterpriseOutlineCodeAlias := ASource.EnterpriseOutlineCodeAlias;
      ResourceSubstitutionEnabled := ASource.ResourceSubstitutionEnabled;
      ShowIndent := ASource.ShowIndent;
      LeafOnly := ASource.LeafOnly;
      AllLevelsRequired := ASource.AllLevelsRequired;
      OnlyTableValuesAllowed := ASource.OnlyTableValuesAllowed;
      Masks := ASource.Masks;
      Values := ASource.Values;
      FAssignedValues := ASource.FAssignedValues;
    finally
      EndUpdate;
    end;
  end;
  inherited Assign(Source);
end;

procedure TdxGanttControlOutlineCode.DoReset;
begin
  FGUID := TdxGanttControlUtils.GenerateGUID;
  FMasks.Reset;
  FValues.Reset;
  FAssignedValues := [];
end;

function TdxGanttControlOutlineCode.IsValueAssigned(const AValue
  : TdxGanttOutlineCodeAssignedValue): Boolean;
begin
  Result := AValue in FAssignedValues;
end;

procedure TdxGanttControlOutlineCode.ResetValue(const AValue
  : TdxGanttOutlineCodeAssignedValue);
begin
  if IsValueAssigned(AValue) then
  begin
    Exclude(FAssignedValues, AValue);
    Changed;
  end
end;

procedure TdxGanttControlOutlineCode.SetAlias(const Value: string);
begin
  if (Alias <> Value) or not IsValueAssigned
    (TdxGanttOutlineCodeAssignedValue.Alias) then
  begin
    Include(FAssignedValues, TdxGanttOutlineCodeAssignedValue.Alias);
    FAlias := Value;
    Changed;
  end;
end;

procedure TdxGanttControlOutlineCode.SetAllLevelsRequired(const Value: Boolean);
begin
  if (AllLevelsRequired <> Value) or
    not IsValueAssigned(TdxGanttOutlineCodeAssignedValue.AllLevelsRequired) then
  begin
    Include(FAssignedValues,
      TdxGanttOutlineCodeAssignedValue.AllLevelsRequired);
    FAllLevelsRequired := Value;
    Changed;
  end;
end;

procedure TdxGanttControlOutlineCode.SetEnterprise(const Value: Boolean);
begin
  if (Enterprise <> Value) or not IsValueAssigned
    (TdxGanttOutlineCodeAssignedValue.Enterprise) then
  begin
    Include(FAssignedValues, TdxGanttOutlineCodeAssignedValue.Enterprise);
    FEnterprise := Value;
    Changed;
  end;
end;

procedure TdxGanttControlOutlineCode.SetEnterpriseOutlineCodeAlias
  (const Value: Integer);
begin
  if (EnterpriseOutlineCodeAlias <> Value) or
    not IsValueAssigned
    (TdxGanttOutlineCodeAssignedValue.EnterpriseOutlineCodeAlias) then
  begin
    Include(FAssignedValues,
      TdxGanttOutlineCodeAssignedValue.EnterpriseOutlineCodeAlias);
    FEnterpriseOutlineCodeAlias := Value;
    Changed;
  end;
end;

procedure TdxGanttControlOutlineCode.SetFieldID(const Value: Integer);
begin
  if (FieldID <> Value) or not IsValueAssigned
    (TdxGanttOutlineCodeAssignedValue.FieldID) then
  begin
    Include(FAssignedValues, TdxGanttOutlineCodeAssignedValue.FieldID);
    FFieldID := Value;
    Changed;
  end;
end;

procedure TdxGanttControlOutlineCode.SetFieldName(const Value: string);
begin
  if (FieldName <> Value) or not IsValueAssigned
    (TdxGanttOutlineCodeAssignedValue.FieldName) then
  begin
    Include(FAssignedValues, TdxGanttOutlineCodeAssignedValue.FieldName);
    FFieldName := Value;
    Changed;
  end;
end;

procedure TdxGanttControlOutlineCode.SetGUID(const Value: string);
begin
  if GUID <> Value then
  begin
    FGUID := Value;
    Changed;
  end;
end;

procedure TdxGanttControlOutlineCode.SetLeafOnly(const Value: Boolean);
begin
  if (LeafOnly <> Value) or not IsValueAssigned
    (TdxGanttOutlineCodeAssignedValue.LeafOnly) then
  begin
    Include(FAssignedValues, TdxGanttOutlineCodeAssignedValue.LeafOnly);
    FLeafOnly := Value;
    Changed;
  end;
end;

procedure TdxGanttControlOutlineCode.SetMasks(const Value
  : TdxGanttControlOutlineCodeMasks);
begin
  FMasks.Assign(Value);
end;

procedure TdxGanttControlOutlineCode.SetOnlyTableValuesAllowed
  (const Value: Boolean);
begin
  if (OnlyTableValuesAllowed <> Value) or
    not IsValueAssigned(TdxGanttOutlineCodeAssignedValue.OnlyTableValuesAllowed)
  then
  begin
    Include(FAssignedValues,
      TdxGanttOutlineCodeAssignedValue.OnlyTableValuesAllowed);
    FOnlyTableValuesAllowed := Value;
    Changed;
  end;
end;

procedure TdxGanttControlOutlineCode.SetPhoneticAlias(const Value: string);
begin
  if (PhoneticAlias <> Value) or not IsValueAssigned
    (TdxGanttOutlineCodeAssignedValue.PhoneticAlias) then
  begin
    Include(FAssignedValues, TdxGanttOutlineCodeAssignedValue.PhoneticAlias);
    FPhoneticAlias := Value;
    Changed;
  end;
end;

procedure TdxGanttControlOutlineCode.SetResourceSubstitutionEnabled
  (const Value: Boolean);
begin
  if (ResourceSubstitutionEnabled <> Value) or
    not IsValueAssigned
    (TdxGanttOutlineCodeAssignedValue.ResourceSubstitutionEnabled) then
  begin
    Include(FAssignedValues,
      TdxGanttOutlineCodeAssignedValue.ResourceSubstitutionEnabled);
    FResourceSubstitutionEnabled := Value;
    Changed;
  end;
end;

procedure TdxGanttControlOutlineCode.SetShowIndent(const Value: Boolean);
begin
  if (ShowIndent <> Value) or not IsValueAssigned
    (TdxGanttOutlineCodeAssignedValue.ShowIndent) then
  begin
    Include(FAssignedValues, TdxGanttOutlineCodeAssignedValue.ShowIndent);
    FShowIndent := Value;
    Changed;
  end;
end;

procedure TdxGanttControlOutlineCode.SetValues(const Value
  : TdxGanttControlOutlineCodeValues);
begin
  FValues.Assign(Value);
end;

{ TdxGanttControlOutlineCodes }

function TdxGanttControlOutlineCodes.Append: TdxGanttControlOutlineCode;
begin
  Result := TdxGanttControlOutlineCode(CreateItem);
  InternalAdd(Result);
end;

procedure TdxGanttControlOutlineCodes.Clear;
begin
  InternalClear;
end;

function TdxGanttControlOutlineCodes.CreateItem
  : TdxGanttControlModelElementListItem;
begin
  Result := TdxGanttControlOutlineCode.Create(Self);
end;

function TdxGanttControlOutlineCodes.GetItem(Index: Integer)
  : TdxGanttControlOutlineCode;
begin
  Result := TdxGanttControlOutlineCode(inherited Items[Index]);
end;

procedure TdxGanttControlOutlineCodes.Remove(AItem: TdxGanttControlOutlineCode);
begin
  InternalRemove(AItem);
end;

end.
