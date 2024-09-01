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

unit dxGanttControlExtendedAttributes;

{$I cxVer.inc}
{$I dxGanttControl.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes, System.Contnrs,
{$ENDIF}
  SysUtils, RTLConsts, Generics.Defaults, Generics.Collections, Classes,
  dxCore, dxCoreClasses,
  dxGanttControlCustomClasses,
  dxGanttControlCustomDataModel;

type
  { TdxGanttControlExtendedAttributeReference }

  TdxGanttExtendedAttributeReferenceAssignedValue = (FieldID, Value,
    DurationFormat, ValueGUID);
  TdxGanttExtendedAttributeReferenceAssignedValues = set of
    TdxGanttExtendedAttributeReferenceAssignedValue;

  TdxGanttControlExtendedAttributeReference = class
    (TdxGanttControlModelElementListItem)
  strict private
    FAssignedValues: TdxGanttExtendedAttributeReferenceAssignedValues;
    FFieldID: Integer;
    FValue: string;
    FValueGUID: string;
    FDurationFormat: TdxDurationFormat;
    procedure SetDurationFormat(const Value: TdxDurationFormat);
    procedure SetFieldID(const Value: Integer);
    procedure SetValue(const AValue: string);
    procedure SetValueGUID(const Value: string);
  protected
    procedure DoReset; override;
  public
    procedure Assign(Source: TPersistent); override;
{$REGION 'for internal use'}
    function IsValueAssigned(const AValue
      : TdxGanttExtendedAttributeReferenceAssignedValue): Boolean;
    procedure ResetValue(const AValue
      : TdxGanttExtendedAttributeReferenceAssignedValue); overload;
{$ENDREGION 'for internal use'}
    property FieldID: Integer read FFieldID write SetFieldID;
    property Value: string read FValue write SetValue;
    property DurationFormat: TdxDurationFormat read FDurationFormat
      write SetDurationFormat;
    property ValueGUID: string read FValueGUID write SetValueGUID;
  end;

  { TdxGanttControlExtendedAttributeReferences }

  TdxGanttControlExtendedAttributeReferences = class
    (TdxGanttControlElementCustomOwnedList)
  strict private
    function GetItem(Index: Integer)
      : TdxGanttControlExtendedAttributeReference; inline;
  protected
    function CreateItem: TdxGanttControlModelElementListItem; override;
  public
    function Append: TdxGanttControlExtendedAttributeReference;
    procedure Clear;
    procedure Remove(AItem: TdxGanttControlExtendedAttributeReference);

    property Items[Index: Integer]: TdxGanttControlExtendedAttributeReference
      read GetItem; default;
  end;

  { TdxGanttControlExtendedAttributeValue }

  TdxGanttExtendedAttributeValueAssignedValue = (Value, Description, Phonetic);
  TdxGanttExtendedAttributeValueAssignedValues = set of
    TdxGanttExtendedAttributeValueAssignedValue;

  TdxGanttControlExtendedAttributeValue = class
    (TdxGanttControlModelElementListItem)
  strict private
    FAssignedValues: TdxGanttExtendedAttributeValueAssignedValues;
    FID: Integer;
    FValue: string;
    FDescription: string;
    FPhonetic: string;
    procedure SetDescription(const Value: string);
    procedure SetID(const Value: Integer);
    procedure SetPhonetic(const Value: string);
    procedure SetValue(const AValue: string);
  protected
    procedure DoReset; override;
  public
    procedure Assign(Source: TPersistent); override;
{$REGION 'for internal use'}
    function IsValueAssigned(const AValue
      : TdxGanttExtendedAttributeValueAssignedValue): Boolean;
    procedure ResetValue(const AValue
      : TdxGanttExtendedAttributeValueAssignedValue); overload;
{$ENDREGION 'for internal use'}
    property ID: Integer read FID write SetID;
    property Value: string read FValue write SetValue;
    property Description: string read FDescription write SetDescription;
    property Phonetic: string read FPhonetic write SetPhonetic;
  end;

  { TdxGanttControlExtendedAttributeValues }

  TdxGanttControlExtendedAttributeValues = class
    (TdxGanttControlElementCustomOwnedList)
  strict private
    function GetItem(Index: Integer)
      : TdxGanttControlExtendedAttributeValue; inline;
  protected
    function CreateItem: TdxGanttControlModelElementListItem; override;
  public
    function Append: TdxGanttControlExtendedAttributeValue;
    procedure Clear;
    procedure Remove(AItem: TdxGanttControlExtendedAttributeValue);

    property Items[Index: Integer]: TdxGanttControlExtendedAttributeValue
      read GetItem; default;
  end;

  { TdxGanttControlExtendedAttribute }

  TdxGanttControlExtendedAttributeCalculationType = (None, Rollup, Calculation);

  TdxGanttControlExtendedAttributeCFType = (Cost, Date, Duration, Finish, Flag,
    Number, Start, Text);

  TdxGanttControlExtendedAttributeElemType = (Task = 20, Resource = 21,
    Assignment = 23);

  TdxGanttControlExtendedAttributeRollupType = (Maximum, Minimum, CountAll, Sum,
    Average, AverageFirstSubLevel, CountFirstSubLevel, CountNonSummaries);

  TdxGanttControlExtendedAttributeValuelistSortOrder = (Descending, Ascending);

  TdxGanttExtendedAttributeAssignedValue = (FieldID, FieldName, CFType, GUID,
    ElemType, MaxMultiValues, UserDef, Alias, SecondaryGUID, SecondaryPID,
    AutoRollDown, DefaultGUID, Ltuid, PhoneticAlias, RollupType,
    CalculationType, Formula, RestrictValues, ValuelistSortOrder,
    AppendNewValues, Default);

  TdxGanttExtendedAttributeAssignedValues = set of
    TdxGanttExtendedAttributeAssignedValue;

  TdxGanttControlExtendedAttribute = class(TdxGanttControlModelElementListItem)
  strict private
    FAssignedValues: TdxGanttExtendedAttributeAssignedValues;
    FFieldID: Integer;
    FFieldName: string;
    FCFType: TdxGanttControlExtendedAttributeCFType;
    FGUID: string;
    FElemType: TdxGanttControlExtendedAttributeElemType;
    FMaxMultiValues: Integer;
    FUserDef: Boolean;
    FAlias: string;
    FSecondaryGUID: string;
    FSecondaryPID: string;
    FAutoRollDown: Boolean;
    FDefaultGUID: string;
    FLtuid: string;
    FPhoneticAlias: string;
    FRollupType: TdxGanttControlExtendedAttributeRollupType;
    FCalculationType: TdxGanttControlExtendedAttributeCalculationType;
    FFormula: string;
    FRestrictValues: Boolean;
    FValuelistSortOrder: TdxGanttControlExtendedAttributeValuelistSortOrder;
    FAppendNewValues: Boolean;
    FDefault: string;
    FValueList: TdxGanttControlExtendedAttributeValues;
    procedure SetAlias(const Value: string);
    procedure SetAppendNewValues(const Value: Boolean);
    procedure SetAutoRollDown(const Value: Boolean);
    procedure SetCalculationType(const Value
      : TdxGanttControlExtendedAttributeCalculationType);
    procedure SetCFType(const Value: TdxGanttControlExtendedAttributeCFType);
    procedure SetDefault(const Value: string);
    procedure SetDefaultGUID(const Value: string);
    procedure SetElemType(const Value
      : TdxGanttControlExtendedAttributeElemType);
    procedure SetFieldID(const Value: Integer);
    procedure SetFieldName(const Value: string);
    procedure SetFormula(const Value: string);
    procedure SetGUID(const Value: string);
    procedure SetLtuid(const Value: string);
    procedure SetMaxMultiValues(const Value: Integer);
    procedure SetPhoneticAlias(const Value: string);
    procedure SetRestrictValues(const Value: Boolean);
    procedure SetRollupType(const Value
      : TdxGanttControlExtendedAttributeRollupType);
    procedure SetSecondaryGUID(const Value: string);
    procedure SetSecondaryPID(const Value: string);
    procedure SetUserDef(const Value: Boolean);
    procedure SetValueList(const Value: TdxGanttControlExtendedAttributeValues);
    procedure SetValuelistSortOrder(const Value
      : TdxGanttControlExtendedAttributeValuelistSortOrder);
  protected
    procedure DoReset; override;
  public
    constructor Create(AOwner: TdxGanttControlModelElement); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
{$REGION 'for internal use'}
    function IsValueAssigned(const AValue
      : TdxGanttExtendedAttributeAssignedValue): Boolean;
    procedure ResetValue(const AValue
      : TdxGanttExtendedAttributeAssignedValue); overload;
{$ENDREGION 'for internal use'}
    property FieldID: Integer read FFieldID write SetFieldID;
    property FieldName: string read FFieldName write SetFieldName;
    property CFType: TdxGanttControlExtendedAttributeCFType read FCFType
      write SetCFType;
    property GUID: string read FGUID write SetGUID;
    property ElemType: TdxGanttControlExtendedAttributeElemType read FElemType
      write SetElemType;
    property MaxMultiValues: Integer read FMaxMultiValues
      write SetMaxMultiValues;
    property UserDef: Boolean read FUserDef write SetUserDef;
    property Alias: string read FAlias write SetAlias;
    property SecondaryGUID: string read FSecondaryGUID write SetSecondaryGUID;
    property SecondaryPID: string read FSecondaryPID write SetSecondaryPID;
    property AutoRollDown: Boolean read FAutoRollDown write SetAutoRollDown;
    property DefaultGUID: string read FDefaultGUID write SetDefaultGUID;
    property Ltuid: string read FLtuid write SetLtuid;
    property PhoneticAlias: string read FPhoneticAlias write SetPhoneticAlias;
    property RollupType: TdxGanttControlExtendedAttributeRollupType
      read FRollupType write SetRollupType;
    property CalculationType: TdxGanttControlExtendedAttributeCalculationType
      read FCalculationType write SetCalculationType;
    property Formula: string read FFormula write SetFormula;
    property RestrictValues: Boolean read FRestrictValues
      write SetRestrictValues;
    property ValuelistSortOrder
      : TdxGanttControlExtendedAttributeValuelistSortOrder
      read FValuelistSortOrder write SetValuelistSortOrder;
    property AppendNewValues: Boolean read FAppendNewValues
      write SetAppendNewValues;
    property Default: string read FDefault write SetDefault;
    property ValueList: TdxGanttControlExtendedAttributeValues read FValueList
      write SetValueList;
  end;

  { TdxGanttControlExtendedAttributes }

  TdxGanttControlExtendedAttributes = class(TdxGanttControlModelElementList)
  strict private
    function GetItem(Index: Integer): TdxGanttControlExtendedAttribute; inline;
  protected
    function CreateItem: TdxGanttControlModelElementListItem; override;
  public
    function Append: TdxGanttControlExtendedAttribute;
    procedure Clear;
    procedure Remove(AItem: TdxGanttControlExtendedAttribute);

    property Items[Index: Integer]: TdxGanttControlExtendedAttribute
      read GetItem; default;
  end;

implementation

{ TdxGanttControlExtendedAttributeReference }

procedure TdxGanttControlExtendedAttributeReference.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlExtendedAttributeReference;
begin
  if Source is TdxGanttControlExtendedAttributeReference then
  begin
    ASource := TdxGanttControlExtendedAttributeReference(Source);
    BeginUpdate;
    try
      FieldID := ASource.FieldID;
      Value := ASource.Value;
      DurationFormat := ASource.DurationFormat;
      ValueGUID := ASource.ValueGUID;
      FAssignedValues := ASource.FAssignedValues;
    finally
      EndUpdate;
    end;
  end;
  inherited Assign(Source);
end;

procedure TdxGanttControlExtendedAttributeReference.DoReset;
begin
  FAssignedValues := [];
end;

function TdxGanttControlExtendedAttributeReference.IsValueAssigned
  (const AValue: TdxGanttExtendedAttributeReferenceAssignedValue): Boolean;
begin
  Result := AValue in FAssignedValues;
end;

procedure TdxGanttControlExtendedAttributeReference.ResetValue
  (const AValue: TdxGanttExtendedAttributeReferenceAssignedValue);
begin
  if IsValueAssigned(AValue) then
  begin
    Exclude(FAssignedValues, AValue);
    Changed;
  end
end;

procedure TdxGanttControlExtendedAttributeReference.SetDurationFormat
  (const Value: TdxDurationFormat);
begin
  if (DurationFormat <> Value) or not IsValueAssigned
    (TdxGanttExtendedAttributeReferenceAssignedValue.DurationFormat) then
  begin
    Include(FAssignedValues, TdxGanttExtendedAttributeReferenceAssignedValue.
      DurationFormat);
    FDurationFormat := Value;
    Changed;
  end;
end;

procedure TdxGanttControlExtendedAttributeReference.SetFieldID
  (const Value: Integer);
begin
  if (FieldID <> Value) or not IsValueAssigned
    (TdxGanttExtendedAttributeReferenceAssignedValue.FieldID) then
  begin
    Include(FAssignedValues,
      TdxGanttExtendedAttributeReferenceAssignedValue.FieldID);
    FFieldID := Value;
    Changed;
  end;
end;

procedure TdxGanttControlExtendedAttributeReference.SetValue
  (const AValue: string);
begin
  if (FValue <> AValue) or not IsValueAssigned
    (TdxGanttExtendedAttributeReferenceAssignedValue.Value) then
  begin
    Include(FAssignedValues,
      TdxGanttExtendedAttributeReferenceAssignedValue.Value);
    FValue := AValue;
    Changed;
  end;
end;

procedure TdxGanttControlExtendedAttributeReference.SetValueGUID
  (const Value: string);
begin
  if (ValueGUID <> Value) or not IsValueAssigned
    (TdxGanttExtendedAttributeReferenceAssignedValue.ValueGUID) then
  begin
    Include(FAssignedValues, TdxGanttExtendedAttributeReferenceAssignedValue.
      ValueGUID);
    FValueGUID := Value;
    Changed;
  end;
end;

{ TdxGanttControlExtendedAttributeReferences }

function TdxGanttControlExtendedAttributeReferences.Append
  : TdxGanttControlExtendedAttributeReference;
begin
  Result := TdxGanttControlExtendedAttributeReference(CreateItem);
  InternalAdd(Result);
end;

procedure TdxGanttControlExtendedAttributeReferences.Clear;
begin
  InternalClear;
end;

function TdxGanttControlExtendedAttributeReferences.CreateItem
  : TdxGanttControlModelElementListItem;
begin
  Result := TdxGanttControlExtendedAttributeReference.Create(Self);
end;

function TdxGanttControlExtendedAttributeReferences.GetItem(Index: Integer)
  : TdxGanttControlExtendedAttributeReference;
begin
  Result := TdxGanttControlExtendedAttributeReference(inherited Items[Index]);
end;

procedure TdxGanttControlExtendedAttributeReferences.Remove
  (AItem: TdxGanttControlExtendedAttributeReference);
begin
  InternalRemove(AItem);
end;

{ TdxGanttControlExtendedAttributeValue }

procedure TdxGanttControlExtendedAttributeValue.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlExtendedAttributeValue;
begin
  if Source is TdxGanttControlExtendedAttributeValue then
  begin
    ASource := TdxGanttControlExtendedAttributeValue(Source);
    BeginUpdate;
    try
      ID := ASource.ID;
      Value := ASource.Value;
      Description := ASource.Description;
      Phonetic := ASource.Phonetic;
      FAssignedValues := ASource.FAssignedValues;
    finally
      EndUpdate;
    end;
  end;
  inherited Assign(Source);
end;

procedure TdxGanttControlExtendedAttributeValue.DoReset;
begin
  FID := -1;
  FAssignedValues := [];
end;

function TdxGanttControlExtendedAttributeValue.IsValueAssigned
  (const AValue: TdxGanttExtendedAttributeValueAssignedValue): Boolean;
begin
  Result := AValue in FAssignedValues;
end;

procedure TdxGanttControlExtendedAttributeValue.ResetValue
  (const AValue: TdxGanttExtendedAttributeValueAssignedValue);
begin
  if IsValueAssigned(AValue) then
  begin
    Exclude(FAssignedValues, AValue);
    Changed;
  end
end;

procedure TdxGanttControlExtendedAttributeValue.SetDescription
  (const Value: string);
begin
  if (Description <> Value) or not IsValueAssigned
    (TdxGanttExtendedAttributeValueAssignedValue.Description) then
  begin
    Include(FAssignedValues, TdxGanttExtendedAttributeValueAssignedValue.
      Description);
    FDescription := Value;
    Changed;
  end;
end;

procedure TdxGanttControlExtendedAttributeValue.SetID(const Value: Integer);
begin
  if ID <> Value then
  begin
    FID := Value;
    Changed;
  end;
end;

procedure TdxGanttControlExtendedAttributeValue.SetPhonetic
  (const Value: string);
begin
  if (Phonetic <> Value) or not IsValueAssigned
    (TdxGanttExtendedAttributeValueAssignedValue.Phonetic) then
  begin
    Include(FAssignedValues,
      TdxGanttExtendedAttributeValueAssignedValue.Phonetic);
    FPhonetic := Value;
    Changed;
  end;
end;

procedure TdxGanttControlExtendedAttributeValue.SetValue(const AValue: string);
begin
  if (FValue <> AValue) or not IsValueAssigned
    (TdxGanttExtendedAttributeValueAssignedValue.Value) then
  begin
    Include(FAssignedValues, TdxGanttExtendedAttributeValueAssignedValue.Value);
    FValue := AValue;
    Changed;
  end;
end;

{ TdxGanttControlExtendedAttributeValues }

function TdxGanttControlExtendedAttributeValues.Append
  : TdxGanttControlExtendedAttributeValue;
begin
  Result := TdxGanttControlExtendedAttributeValue(CreateItem);
  InternalAdd(Result);
end;

procedure TdxGanttControlExtendedAttributeValues.Clear;
begin
  InternalClear;
end;

function TdxGanttControlExtendedAttributeValues.CreateItem
  : TdxGanttControlModelElementListItem;
begin
  Result := TdxGanttControlExtendedAttributeValue.Create(Self);
end;

function TdxGanttControlExtendedAttributeValues.GetItem(Index: Integer)
  : TdxGanttControlExtendedAttributeValue;
begin
  Result := TdxGanttControlExtendedAttributeValue(inherited Items[Index]);
end;

procedure TdxGanttControlExtendedAttributeValues.Remove
  (AItem: TdxGanttControlExtendedAttributeValue);
begin
  InternalRemove(AItem);
end;

{ TdxGanttControlExtendedAttribute }

procedure TdxGanttControlExtendedAttribute.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlExtendedAttribute;
begin
  if Source is TdxGanttControlExtendedAttribute then
  begin
    ASource := TdxGanttControlExtendedAttribute(Source);
    BeginUpdate;
    try
      FieldID := ASource.FieldID;
      FieldName := ASource.FieldName;
      CFType := ASource.CFType;
      GUID := ASource.GUID;
      ElemType := ASource.ElemType;
      MaxMultiValues := ASource.MaxMultiValues;
      UserDef := ASource.UserDef;
      Alias := ASource.Alias;
      SecondaryGUID := ASource.SecondaryGUID;
      SecondaryPID := ASource.SecondaryPID;
      AutoRollDown := ASource.AutoRollDown;
      DefaultGUID := ASource.DefaultGUID;
      Ltuid := ASource.Ltuid;
      PhoneticAlias := ASource.PhoneticAlias;
      RollupType := ASource.RollupType;
      CalculationType := ASource.CalculationType;
      Formula := ASource.Formula;
      RestrictValues := ASource.RestrictValues;
      ValuelistSortOrder := ASource.ValuelistSortOrder;
      AppendNewValues := ASource.AppendNewValues;
      Default := ASource.Default;
      ValueList := ASource.ValueList;
      FAssignedValues := ASource.FAssignedValues;
    finally
      EndUpdate;
    end;
  end;
  inherited Assign(Source);
end;

constructor TdxGanttControlExtendedAttribute.Create
  (AOwner: TdxGanttControlModelElement);
begin
  inherited Create(AOwner);
  FValueList := TdxGanttControlExtendedAttributeValues.Create(Self);
end;

destructor TdxGanttControlExtendedAttribute.Destroy;
begin
  FreeAndNil(FValueList);
  inherited Destroy;
end;

procedure TdxGanttControlExtendedAttribute.DoReset;
begin
  FValueList.Reset;
  FAssignedValues := [];
end;

function TdxGanttControlExtendedAttribute.IsValueAssigned
  (const AValue: TdxGanttExtendedAttributeAssignedValue): Boolean;
begin
  Result := AValue in FAssignedValues;
end;

procedure TdxGanttControlExtendedAttribute.ResetValue
  (const AValue: TdxGanttExtendedAttributeAssignedValue);
begin
  if IsValueAssigned(AValue) then
  begin
    Exclude(FAssignedValues, AValue);
    Changed;
  end
end;

procedure TdxGanttControlExtendedAttribute.SetAlias(const Value: string);
begin
  if (Alias <> Value) or not IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.Alias) then
  begin
    Include(FAssignedValues, TdxGanttExtendedAttributeAssignedValue.Alias);
    FAlias := Value;
    Changed;
  end;
end;

procedure TdxGanttControlExtendedAttribute.SetAppendNewValues
  (const Value: Boolean);
begin
  if (AppendNewValues <> Value) or
    not IsValueAssigned(TdxGanttExtendedAttributeAssignedValue.AppendNewValues)
  then
  begin
    Include(FAssignedValues,
      TdxGanttExtendedAttributeAssignedValue.AppendNewValues);
    FAppendNewValues := Value;
    Changed;
  end;
end;

procedure TdxGanttControlExtendedAttribute.SetAutoRollDown
  (const Value: Boolean);
begin
  if (AutoRollDown <> Value) or not IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.AutoRollDown) then
  begin
    Include(FAssignedValues,
      TdxGanttExtendedAttributeAssignedValue.AutoRollDown);
    FAutoRollDown := Value;
    Changed;
  end;
end;

procedure TdxGanttControlExtendedAttribute.SetCalculationType
  (const Value: TdxGanttControlExtendedAttributeCalculationType);
begin
  if (CalculationType <> Value) or
    not IsValueAssigned(TdxGanttExtendedAttributeAssignedValue.CalculationType)
  then
  begin
    Include(FAssignedValues,
      TdxGanttExtendedAttributeAssignedValue.CalculationType);
    FCalculationType := Value;
    Changed;
  end;
end;

procedure TdxGanttControlExtendedAttribute.SetCFType
  (const Value: TdxGanttControlExtendedAttributeCFType);
begin
  if (CFType <> Value) or not IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.CFType) then
  begin
    Include(FAssignedValues, TdxGanttExtendedAttributeAssignedValue.CFType);
    FCFType := Value;
    Changed;
  end;
end;

procedure TdxGanttControlExtendedAttribute.SetDefault(const Value: string);
begin
  if (Default <> Value) or not IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.Default) then
  begin
    Include(FAssignedValues, TdxGanttExtendedAttributeAssignedValue.Default);
    FDefault := Value;
    Changed;
  end;
end;

procedure TdxGanttControlExtendedAttribute.SetDefaultGUID(const Value: string);
begin
  if (DefaultGUID <> Value) or not IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.DefaultGUID) then
  begin
    Include(FAssignedValues,
      TdxGanttExtendedAttributeAssignedValue.DefaultGUID);
    FDefaultGUID := Value;
    Changed;
  end;
end;

procedure TdxGanttControlExtendedAttribute.SetElemType
  (const Value: TdxGanttControlExtendedAttributeElemType);
begin
  if (ElemType <> Value) or not IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.ElemType) then
  begin
    Include(FAssignedValues, TdxGanttExtendedAttributeAssignedValue.ElemType);
    FElemType := Value;
    Changed;
  end;
end;

procedure TdxGanttControlExtendedAttribute.SetFieldID(const Value: Integer);
begin
  if (FieldID <> Value) or not IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.FieldID) then
  begin
    Include(FAssignedValues, TdxGanttExtendedAttributeAssignedValue.FieldID);
    FFieldID := Value;
    Changed;
  end;
end;

procedure TdxGanttControlExtendedAttribute.SetFieldName(const Value: string);
begin
  if (FieldName <> Value) or not IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.FieldName) then
  begin
    Include(FAssignedValues, TdxGanttExtendedAttributeAssignedValue.FieldName);
    FFieldName := Value;
    Changed;
  end;
end;

procedure TdxGanttControlExtendedAttribute.SetFormula(const Value: string);
begin
  if (Formula <> Value) or not IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.Formula) then
  begin
    Include(FAssignedValues, TdxGanttExtendedAttributeAssignedValue.Formula);
    FFormula := Value;
    Changed;
  end;
end;

procedure TdxGanttControlExtendedAttribute.SetGUID(const Value: string);
begin
  if (GUID <> Value) or not IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.GUID) then
  begin
    Include(FAssignedValues, TdxGanttExtendedAttributeAssignedValue.GUID);
    FGUID := Value;
    Changed;
  end;
end;

procedure TdxGanttControlExtendedAttribute.SetLtuid(const Value: string);
begin
  if (Ltuid <> Value) or not IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.Ltuid) then
  begin
    Include(FAssignedValues, TdxGanttExtendedAttributeAssignedValue.Ltuid);
    FLtuid := Value;
    Changed;
  end;
end;

procedure TdxGanttControlExtendedAttribute.SetMaxMultiValues
  (const Value: Integer);
begin
  if (MaxMultiValues <> Value) or not IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.MaxMultiValues) then
  begin
    Include(FAssignedValues,
      TdxGanttExtendedAttributeAssignedValue.MaxMultiValues);
    FMaxMultiValues := Value;
    Changed;
  end;
end;

procedure TdxGanttControlExtendedAttribute.SetPhoneticAlias
  (const Value: string);
begin
  if (PhoneticAlias <> Value) or not IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.PhoneticAlias) then
  begin
    Include(FAssignedValues,
      TdxGanttExtendedAttributeAssignedValue.PhoneticAlias);
    FPhoneticAlias := Value;
    Changed;
  end;
end;

procedure TdxGanttControlExtendedAttribute.SetRestrictValues
  (const Value: Boolean);
begin
  if (RestrictValues <> Value) or not IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.RestrictValues) then
  begin
    Include(FAssignedValues,
      TdxGanttExtendedAttributeAssignedValue.RestrictValues);
    FRestrictValues := Value;
    Changed;
  end;
end;

procedure TdxGanttControlExtendedAttribute.SetRollupType
  (const Value: TdxGanttControlExtendedAttributeRollupType);
begin
  if (RollupType <> Value) or not IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.RollupType) then
  begin
    Include(FAssignedValues, TdxGanttExtendedAttributeAssignedValue.RollupType);
    FRollupType := Value;
    Changed;
  end;
end;

procedure TdxGanttControlExtendedAttribute.SetSecondaryGUID
  (const Value: string);
begin
  if (SecondaryGUID <> Value) or not IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.SecondaryGUID) then
  begin
    Include(FAssignedValues,
      TdxGanttExtendedAttributeAssignedValue.SecondaryGUID);
    FSecondaryGUID := Value;
    Changed;
  end;
end;

procedure TdxGanttControlExtendedAttribute.SetSecondaryPID(const Value: string);
begin
  if (SecondaryPID <> Value) or not IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.SecondaryPID) then
  begin
    Include(FAssignedValues,
      TdxGanttExtendedAttributeAssignedValue.SecondaryPID);
    FSecondaryPID := Value;
    Changed;
  end;
end;

procedure TdxGanttControlExtendedAttribute.SetUserDef(const Value: Boolean);
begin
  if (UserDef <> Value) or not IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.UserDef) then
  begin
    Include(FAssignedValues, TdxGanttExtendedAttributeAssignedValue.UserDef);
    FUserDef := Value;
    Changed;
  end;
end;

procedure TdxGanttControlExtendedAttribute.SetValueList
  (const Value: TdxGanttControlExtendedAttributeValues);
begin
  FValueList.Assign(Value);
end;

procedure TdxGanttControlExtendedAttribute.SetValuelistSortOrder
  (const Value: TdxGanttControlExtendedAttributeValuelistSortOrder);
begin
  if (ValuelistSortOrder <> Value) or
    not IsValueAssigned(TdxGanttExtendedAttributeAssignedValue.
    ValuelistSortOrder) then
  begin
    Include(FAssignedValues,
      TdxGanttExtendedAttributeAssignedValue.ValuelistSortOrder);
    FValuelistSortOrder := Value;
    Changed;
  end;
end;

{ TdxGanttControlExtendedAttributes }

function TdxGanttControlExtendedAttributes.Append
  : TdxGanttControlExtendedAttribute;
begin
  Result := TdxGanttControlExtendedAttribute(CreateItem);
  InternalAdd(Result);
end;

procedure TdxGanttControlExtendedAttributes.Clear;
begin
  InternalClear;
end;

function TdxGanttControlExtendedAttributes.CreateItem
  : TdxGanttControlModelElementListItem;
begin
  Result := TdxGanttControlExtendedAttribute.Create(Self);
end;

function TdxGanttControlExtendedAttributes.GetItem(Index: Integer)
  : TdxGanttControlExtendedAttribute;
begin
  Result := TdxGanttControlExtendedAttribute(inherited Items[Index]);
end;

procedure TdxGanttControlExtendedAttributes.Remove
  (AItem: TdxGanttControlExtendedAttribute);
begin
  InternalRemove(AItem);
end;

end.
