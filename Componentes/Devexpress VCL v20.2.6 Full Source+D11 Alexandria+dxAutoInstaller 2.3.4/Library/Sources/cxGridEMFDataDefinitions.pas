{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressQuantumGrid }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL }
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

unit cxGridEMFDataDefinitions;

interface

uses
  SysUtils, Classes,
  dxEMF.Metadata, cxEMFData, cxEMFDataDefinitions,
  dxCoreClasses, cxEdit, cxDBEdit, cxFilter, cxCustomData, cxGridCustomView,
  cxDataStorage,
  cxGridCustomTableView;

type
  TcxGridEMFDataController = class;

  { TcxGridEMFDefaultValuesProvider }

  TcxGridEMFDefaultValuesProvider = class(TcxEMFDefaultValuesProvider);

  { TcxGridItemEMFDataBinding }

  TcxGridItemEMFDataBinding = class(TcxGridItemDataBinding,
    IdxEMFDataController)
  strict private
    function GetEMFDataController: TdxEMFDataController;
    function GetDataController: TcxGridEMFDataController; inline;
    function GetField: TdxEMFDataField; inline;
    function GetFieldName: string; inline;
    function GetMemberInfo: TdxMappingMemberInfo; inline;
    procedure SetFieldName(const Value: string);

    // IInterface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function IdxEMFDataController.GetDataController = GetEMFDataController;
  private
    function GetDefaultValuesProvider: TcxGridEMFDefaultValuesProvider; inline;
  protected
    function GetDefaultValueTypeClass: TcxValueTypeClass; override;
    function GetFilterFieldName: string; override;
    procedure Init; override;
    function IsValueTypeStored: Boolean; override;
    property DefaultValuesProvider: TcxGridEMFDefaultValuesProvider
      read GetDefaultValuesProvider;
  public
    procedure Assign(Source: TPersistent); override;
    function DefaultCaption: string; override;
    function DefaultRepositoryItem: TcxEditRepositoryItem; override;
    function DefaultWidth(ATakeHeaderIntoAccount: Boolean = True)
      : Integer; override;
    property DataController: TcxGridEMFDataController read GetDataController;

    property Field: TdxEMFDataField read GetField;
    property MemberInfo: TdxMappingMemberInfo read GetMemberInfo;
  published
    property FieldName: string read GetFieldName write SetFieldName;
  end;

  { TcxCustomGridEMFDataController }

  TcxCustomGridEMFDataController = class(TdxEMFDataController,
    IcxCustomGridDataController, IcxGridDataController)
  private
    FPrevScrollBarPos: Integer;
    function GetGridViewValue: TcxCustomGridTableView; inline;
    function GetEntityInfo: TdxEntityInfo;
    function GetController: TcxCustomGridTableController; inline;
  protected
    // IcxCustomGridDataController
    procedure AssignData(ADataController: TcxCustomDataController);
    procedure DeleteAllItems;
    procedure GetFakeComponentLinks(AList: TList);
    function GetGridView: TcxCustomGridView;
    function HasAllItems: Boolean;
    function IsDataChangeable: Boolean;
    function IsDataLinked: Boolean;
    function SupportsCreateAllItems: Boolean;
    // IcxGridDataController
    procedure CheckGridModeBufferCount;
    function DoScroll(AForward: Boolean): Boolean;
    function DoScrollPage(AForward: Boolean): Boolean;
    function GetItemDataBindingClass: TcxGridItemDataBindingClass;
    function GetItemDefaultValuesProviderClass
      : TcxCustomEditDefaultValuesProviderClass;
    function GetNavigatorIsBof: Boolean;
    function GetNavigatorIsEof: Boolean;
    function GetScrollBarPos: Integer;
    function GetScrollBarRecordCount: Integer;
    function SetScrollBarPos(Value: Integer): Boolean;

    function CanSelectRow(ARowIndex: Integer): Boolean; override;
    function CompareByField(ARecordIndex1, ARecordIndex2: Integer;
      AField: TcxCustomDataField; AMode: TcxDataControllerComparisonMode)
      : Integer; override;

    procedure DoValueTypeClassChanged(AItemIndex: Integer); override;
    function GetDefaultActiveRelationIndex: Integer; override;

    function GetItemByMemberName(const AMemberName: string)
      : TcxCustomGridTableItem;
    function DoGetGroupRowDisplayText(const ARowInfo: TcxRowInfo;
      var AItemIndex: Integer): string; override;
    function GetItemID(AItem: TObject): Integer; override;
    function GetSummaryGroupItemLinkClass
      : TcxDataSummaryGroupItemLinkClass; override;
    function GetSummaryItemClass: TcxDataSummaryItemClass; override;
    function SupportsScrollBarParams: Boolean; virtual;
    procedure FilterChanged; override;

    function CanIgnoreTimeForFiltering(ADataField: TdxEMFDataField)
      : Boolean; override;
    procedure InitializeDateTimeGrouping(ADataField: TdxEMFDataField); override;
    procedure PopulateFilterValues(AList: TcxDataFilterValueList;
      AItemIndex: Integer; ACriteria: TcxFilterCriteria;
      var AUseFilteredRecords: Boolean; out ANullExists: Boolean;
      AUniqueOnly: Boolean; AFilteredRecordsShowFilteredItemsOnly
      : Boolean); override;
    function NeedPrepareGroupValue(AEMFDataField: TdxEMFDataField)
      : Boolean; override;

    property Controller: TcxCustomGridTableController read GetController;
    property EntityInfo: TdxEntityInfo read GetEntityInfo;
  public
    procedure CreateAllItems(AMissingItemsOnly: Boolean = False);

    procedure BeginFullUpdate; override;
    procedure EndFullUpdate; override;
    procedure FocusControl(AItemIndex: Integer; var Done: Boolean); override;
    function GetDetailDataControllerByLinkObject(ALinkObject: TObject)
      : TcxCustomDataController; override;
    function GetDisplayText(ARecordIndex: Integer; AItemIndex: Integer)
      : string; override;
    function GetItem(Index: Integer): TObject; override;
    function GetFilterItemFieldCaption(AItem: TObject): string; override;
    procedure UpdateData; override;
    property GridView: TcxCustomGridTableView read GetGridViewValue;
  end;

  { TcxGridEMFDataController }

  TcxGridEMFDataController = class(TcxCustomGridEMFDataController)
  public
    function CreateDetailLinkObject(ARelation: TcxCustomDataRelation;
      ARecordIndex: Integer): TObject; override;
  published
    property DataSource;
    property Summary;
    property DetailKeyFieldNames;
    property MasterKeyFieldNames;
  end;

implementation

uses
  TypInfo, Variants, DB, Graphics, RTLConsts,
  cxClasses, cxControls, cxDBData, cxVariants,
  cxEditDBRegisteredRepositoryItems, cxGridCommon, cxGridLevel,
  dxEMF.Serializer, dxEMF.DataDefinitions,
  dxEMF.Utils, dxDateRanges, cxEditDataRegisteredRepositoryItems;

type

  TCustomGridTableItemAccess = class(TcxCustomGridTableItem);
  TdxEMFDataFieldAccess = class(TdxEMFDataField);

  TdxGraphicBlobSerializer = class(TdxPersistentBlobSerializer)
  public
    class function GetStorageDBType: TFieldType; override;
  end;

  { TcxGridItemEMFDataBinding }

procedure TcxGridItemEMFDataBinding.Assign(Source: TPersistent);
begin
  if Source is TcxGridItemEMFDataBinding then
    FieldName := TcxGridItemEMFDataBinding(Source).FieldName;
  inherited Assign(Source);
end;

function TcxGridItemEMFDataBinding.GetDataController: TcxGridEMFDataController;
begin
  Result := TcxGridEMFDataController(inherited DataController);
end;

function TcxGridItemEMFDataBinding.GetField: TdxEMFDataField;
begin
  Result := DataController.Fields[Item.Index];
end;

function TcxGridItemEMFDataBinding.GetFieldName: string;
begin
  Result := DataController.GetItemFieldName(Item.Index);
end;

function TcxGridItemEMFDataBinding.GetMemberInfo: TdxMappingMemberInfo;
begin
  Result := DataController.GetMemberInfo(Field);
end;

function TcxGridItemEMFDataBinding.DefaultCaption: string;
var
  AInfo: TdxMappingMemberInfo;
begin
  AInfo := MemberInfo;
  if AInfo = nil then
    Result := FieldName
  else
    Result := AInfo.ActualName;
end;

function TcxGridItemEMFDataBinding.DefaultRepositoryItem: TcxEditRepositoryItem;
begin
  if MemberInfo = nil then
    Result := inherited DefaultRepositoryItem
  else if MemberInfo.FieldType = ftUnknown then
    Result := GetDefaultEditDataRepositoryItems.GetItem(ValueTypeClass)
  else
    Result := GetDefaultEditDBRepositoryItems.GetItem(MemberInfo.FieldType);
end;

function TcxGridItemEMFDataBinding.DefaultWidth(ATakeHeaderIntoAccount
  : Boolean): Integer;
begin
  Result := inherited DefaultWidth(ATakeHeaderIntoAccount);

  TcxCustomGridTableItemAccess.CheckWidthValue(Item, Result);
end;

function TcxGridItemEMFDataBinding.GetDefaultValuesProvider
  : TcxGridEMFDefaultValuesProvider;
begin
  Result := TcxGridEMFDefaultValuesProvider(inherited GetDefaultValuesProvider);
end;

function TcxGridItemEMFDataBinding.GetDefaultValueTypeClass: TcxValueTypeClass;
begin
  Result := nil;
end;

function TcxGridItemEMFDataBinding.GetEMFDataController: TdxEMFDataController;
begin
  Result := DataController;
end;

function TcxGridItemEMFDataBinding.GetFilterFieldName: string;
begin
  if MemberInfo = nil then
    Result := ''
  else
    Result := MemberInfo.MemberName;
end;

procedure TcxGridItemEMFDataBinding.Init;
begin
  inherited Init;
  DefaultValuesProvider.MemberInfo := MemberInfo;
  DefaultValuesProvider.FieldName := FieldName;
end;

function TcxGridItemEMFDataBinding.IsValueTypeStored: Boolean;
begin
  Result := FieldName = '';
end;

function TcxGridItemEMFDataBinding.QueryInterface(const IID: TGUID;
  out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TcxGridItemEMFDataBinding.SetFieldName(const Value: string);
begin
  if FieldName = Value then
    Exit;
  FilterMRUValueItems.ClearItems;
  DataController.ChangeFieldName(Item.Index, Value);
end;

function TcxGridItemEMFDataBinding._AddRef: Integer;
begin
  Result := -1;
end;

function TcxGridItemEMFDataBinding._Release: Integer;
begin
  Result := -1;
end;

{ TcxCustomGridEMFDataController }

function TcxCustomGridEMFDataController.GetController
  : TcxCustomGridTableController;
begin
  Result := GridView.Controller;
end;

function TcxCustomGridEMFDataController.GetGridViewValue
  : TcxCustomGridTableView;
begin
  Result := TcxCustomGridTableView(GetGridView);
end;

function TcxCustomGridEMFDataController.GetDefaultActiveRelationIndex: Integer;
begin
  Result := TcxCustomGridTableViewAccess.GetDefaultActiveDetailIndex(GridView);
end;

function TcxCustomGridEMFDataController.GetDetailDataControllerByLinkObject
  (ALinkObject: TObject): TcxCustomDataController;
begin
  if ALinkObject = nil then
    Result := nil
  else
    Result := TcxCustomGridView(ALinkObject).DataController;
end;

function TcxCustomGridEMFDataController.GetDisplayText(ARecordIndex,
  AItemIndex: Integer): string;
begin
  if not GridView.ViewData.GetDisplayText(ARecordIndex, AItemIndex, Result) then
    Result := inherited GetDisplayText(ARecordIndex, AItemIndex);
  TcxCustomGridTableItemAccess.DoGetDataText(GridView.Items[AItemIndex],
    ARecordIndex, Result);
end;

procedure TcxCustomGridEMFDataController.AssignData(ADataController
  : TcxCustomDataController);
begin
end;

procedure TcxCustomGridEMFDataController.BeginFullUpdate;
begin
  GridView.BeginUpdate;
  inherited BeginFullUpdate;
end;

function TcxCustomGridEMFDataController.CanIgnoreTimeForFiltering
  (ADataField: TdxEMFDataField): Boolean;
begin
  Result := TCustomGridTableItemAccess(ADataField.Item)
    .CanIgnoreTimeForFiltering;
end;

function TcxCustomGridEMFDataController.CanSelectRow
  (ARowIndex: Integer): Boolean;
begin
  Result := TcxCustomGridTableViewAccess.CanSelectRecord(GridView, ARowIndex);
end;

procedure TcxCustomGridEMFDataController.CheckGridModeBufferCount;
begin
end;

function TcxCustomGridEMFDataController.CompareByField(ARecordIndex1,
  ARecordIndex2: Integer; AField: TcxCustomDataField;
  AMode: TcxDataControllerComparisonMode): Integer;
begin
  if GridView.ViewData.NeedsCustomDataComparison(AField, AMode) then
    Result := GridView.ViewData.CustomCompareDataValues(AField,
      GetComparedValue(ARecordIndex1, AField), GetComparedValue(ARecordIndex2,
      AField), AMode)
  else
    Result := inherited CompareByField(ARecordIndex1, ARecordIndex2,
      AField, AMode);
end;

procedure TcxCustomGridEMFDataController.CreateAllItems(AMissingItemsOnly
  : Boolean = False);
var
  AMemberInfo: TdxMappingMemberInfo;
  AItem: TcxCustomGridTableItem;
begin
  if EntityInfo = nil then
    Exit;
  ShowHourglassCursor;
  try
    GridView.BeginUpdate;
    BeginUpdateFields;
    try
      for AMemberInfo in EntityInfo.MemberAttributes do
        if AMemberInfo.IsColumn and not AMemberInfo.IsAssociationList and
          (not AMissingItemsOnly or (GetItemByMemberName(AMemberInfo.MemberName)
          = nil)) then
        begin
          AItem := GridView.CreateItem;
          (AItem.DataBinding as TcxGridItemEMFDataBinding).FieldName :=
            AMemberInfo.MemberName;
          AItem.Name := CreateUniqueName(GridView.Owner, GridView, AItem,
            ScxGridPrefixName, AMemberInfo.MemberName);
        end;
    finally
      EndUpdateFields;
      GridView.EndUpdate;
    end;
  finally
    HideHourglassCursor;
  end;
end;

procedure TcxCustomGridEMFDataController.DeleteAllItems;
begin
  GridView.ClearItems;
end;

function TcxCustomGridEMFDataController.DoGetGroupRowDisplayText
  (const ARowInfo: TcxRowInfo; var AItemIndex: Integer): string;
var
  AValue: Variant;
  ARecordIndex: Integer;
begin
  if InMemoryMode then
    Result := inherited
  else if DataSource <> nil then
  begin
    AValue := GetGroupRowValueByItemIndex(ARowInfo, AItemIndex);
    ARecordIndex := ARowInfo.RecordIndex;
    Result := GridView.ViewData.GetDisplayTextFromValue(ARecordIndex,
      AItemIndex, AValue);
    TCustomGridTableItemAccess(GridView.Items[AItemIndex])
      .DoGetDataText(ARecordIndex, Result);
  end
  else
    Result := '';
end;

function TcxCustomGridEMFDataController.DoScroll(AForward: Boolean): Boolean;
begin
  Result := SupportsScrollBarParams;
  if Result then
    if AForward then
      Controller.GoToNext(False, False)
    else
      Controller.GoToPrev(False, False);
end;

function TcxCustomGridEMFDataController.DoScrollPage(AForward: Boolean)
  : Boolean;
begin
  Result := SupportsScrollBarParams;
  if Result then
    if AForward then
      TcxCustomGridTableControllerAccess.FocusNextPage(Controller, False)
    else
      TcxCustomGridTableControllerAccess.FocusPrevPage(Controller, False);
end;

procedure TcxCustomGridEMFDataController.DoValueTypeClassChanged
  (AItemIndex: Integer);
begin
  TcxCustomGridTableViewAccess.ItemValueTypeClassChanged(GridView, AItemIndex);
end;

procedure TcxCustomGridEMFDataController.EndFullUpdate;
begin
  inherited EndFullUpdate;
  GridView.EndUpdate;
end;

procedure TcxCustomGridEMFDataController.FilterChanged;
begin
  inherited FilterChanged;
  TcxCustomGridTableViewAccess.FilterChanged(GridView);
end;

procedure TcxCustomGridEMFDataController.FocusControl(AItemIndex: Integer;
  var Done: Boolean);
begin
  inherited;
  TcxCustomGridTableViewAccess.FocusDataControl(GridView, AItemIndex, Done);
end;

function TcxCustomGridEMFDataController.GetEntityInfo: TdxEntityInfo;
begin
  if DataSource = nil then
    Result := nil
  else
    Result := DataSource.EntityInfo;
end;

procedure TcxCustomGridEMFDataController.GetFakeComponentLinks(AList: TList);
begin
  if (DataSource <> nil) and (DataSource.Owner <> GridView.Owner) and
    (AList.IndexOf(DataSource.Owner) = -1) then
    AList.Add(DataSource.Owner);
end;

function TcxCustomGridEMFDataController.GetFilterItemFieldCaption
  (AItem: TObject): string;
begin
  Result := TcxCustomGridTableItemAccess.GetFilterCaption
    (TcxCustomGridTableItem(AItem));
end;

function TcxCustomGridEMFDataController.GetGridView: TcxCustomGridView;
begin
  Result := TcxCustomGridView(GetOwner);
end;

function TcxCustomGridEMFDataController.GetItem(Index: Integer): TObject;
begin
  Result := GridView.Items[Index];
end;

function TcxCustomGridEMFDataController.GetItemByMemberName(const AMemberName
  : string): TcxCustomGridTableItem;
var
  I: Integer;
  AItem: TcxCustomGridTableItem;
begin
  for I := 0 to GridView.ItemCount - 1 do
  begin
    AItem := GridView.Items[I];
    if SameText((AItem.DataBinding as TcxGridItemEMFDataBinding).FieldName,
      AMemberName) then
      Exit(AItem);
  end;
  Result := nil;
end;

function TcxCustomGridEMFDataController.GetItemDataBindingClass
  : TcxGridItemDataBindingClass;
begin
  Result := TcxGridItemEMFDataBinding;
end;

function TcxCustomGridEMFDataController.GetItemDefaultValuesProviderClass
  : TcxCustomEditDefaultValuesProviderClass;
begin
  Result := TcxGridEMFDefaultValuesProvider;
end;

function TcxCustomGridEMFDataController.GetItemID(AItem: TObject): Integer;
begin
  if AItem is TcxCustomGridTableItem then
    Result := TcxCustomGridTableItem(AItem).ID
  else
    Result := -1;
end;

function TcxCustomGridEMFDataController.GetNavigatorIsBof: Boolean;
begin
  Result := GridView.Controller.IsStart;
end;

function TcxCustomGridEMFDataController.GetNavigatorIsEof: Boolean;
begin
  Result := GridView.Controller.IsFinish;
end;

function TcxCustomGridEMFDataController.GetScrollBarPos: Integer;
begin
  if SupportsScrollBarParams then
    if dceInsert in EditState then
      Result := FPrevScrollBarPos
    else
      Result := FocusedRowIndex
  else
    Result := -1;
  FPrevScrollBarPos := Result;
end;

function TcxCustomGridEMFDataController.GetScrollBarRecordCount: Integer;
begin
  if SupportsScrollBarParams then
    Result := -1
  else
    Result := -1;
end;

function TcxCustomGridEMFDataController.GetSummaryGroupItemLinkClass
  : TcxDataSummaryGroupItemLinkClass;
begin
  Result := TcxCustomGridTableViewAccess.GetSummaryGroupItemLinkClass(GridView);
  if Result = nil then
    Result := inherited GetSummaryGroupItemLinkClass;
end;

function TcxCustomGridEMFDataController.GetSummaryItemClass
  : TcxDataSummaryItemClass;
begin
  Result := TcxCustomGridTableViewAccess.GetSummaryItemClass(GridView);
  if Result = nil then
    Result := inherited GetSummaryItemClass;
end;

procedure TcxCustomGridEMFDataController.InitializeDateTimeGrouping
  (ADataField: TdxEMFDataField);
var
  AItem: TcxCustomGridTableItem;
begin
  AItem := ADataField.Item as TcxCustomGridTableItem;
  TdxEMFDataFieldAccess(ADataField).DateTimeGrouping :=
    TdxDateTimeGrouping(TCustomGridTableItemAccess(AItem)
    .GetDateTimeHandlingGrouping);
end;

function TcxCustomGridEMFDataController.HasAllItems: Boolean;
var
  AMemberInfo: TdxMappingMemberInfo;
begin
  for AMemberInfo in EntityInfo.MemberAttributes do
    if AMemberInfo.IsColumn and not AMemberInfo.IsAssociationList then
      if GetItemByMemberName(AMemberInfo.MemberName) = nil then
        Exit(False);
  Result := True;
end;

function TcxCustomGridEMFDataController.IsDataChangeable: Boolean;
begin
  Result := True;
end;

function TcxCustomGridEMFDataController.IsDataLinked: Boolean;
begin
  Result := EntityInfo <> nil;
end;

function TcxCustomGridEMFDataController.NeedPrepareGroupValue
  (AEMFDataField: TdxEMFDataField): Boolean;
begin
  if (AEMFDataField.MemberInfo <> nil) and AEMFDataField.MemberInfo.IsReference
  then
  begin
    if AEMFDataField.MemberInfo.LookupResultField.HasValue then
      Result := True
    else
      Result := Pos('EMF', (AEMFDataField.Item as TcxCustomGridTableItem)
        .GetProperties.ClassName) > 0;
  end
  else
    Result := inherited NeedPrepareGroupValue(AEMFDataField);
end;

procedure TcxCustomGridEMFDataController.PopulateFilterValues
  (AList: TcxDataFilterValueList; AItemIndex: Integer;
  ACriteria: TcxFilterCriteria; var AUseFilteredRecords: Boolean;
  out ANullExists: Boolean; AUniqueOnly, AFilteredRecordsShowFilteredItemsOnly
  : Boolean);
var
  I, AValueListCount: Integer;
  ADisplayText: string;
  AItem: TcxFilterValueItem;
  AProperties: TcxCustomEditProperties;
  AValueList: TcxDataFilterValueList;
begin
  ANullExists := False;
  if ACriteria.MaxValueListCount < 0 then
    Exit;
  AValueList := TcxDataFilterValueList.Create(AList.Criteria);
  try
    inherited PopulateFilterValues(AValueList, AItemIndex, ACriteria,
      AUseFilteredRecords, ANullExists, AUniqueOnly,
      AFilteredRecordsShowFilteredItemsOnly);
    AProperties := GridView.Items[AItemIndex].GetProperties;
    AValueListCount := AValueList.Count;
    if (ACriteria.MaxValueListCount > 0) and
      (AValueListCount >= ACriteria.MaxValueListCount) then
      AValueListCount := ACriteria.MaxValueListCount;
    for I := 0 to AValueListCount - 1 do
    begin
      AItem := AValueList[I];
      ADisplayText := AProperties.GetDisplayText(AValueList[I].Value, True);
      AList.Add(AItem.Kind, AItem.Value, ADisplayText, True);
    end;
  finally
    AValueList.Free;
  end;
end;

function TcxCustomGridEMFDataController.SetScrollBarPos(Value: Integer)
  : Boolean;
begin
  Result := SupportsScrollBarParams;
end;

function TcxCustomGridEMFDataController.SupportsCreateAllItems: Boolean;
begin
  Result := True;
end;

function TcxCustomGridEMFDataController.SupportsScrollBarParams: Boolean;
begin
  Result := False;
end;

procedure TcxCustomGridEMFDataController.UpdateData;
begin
  inherited UpdateData;
  TcxCustomGridTableViewAccess.UpdateRecord(GridView);
end;

{ TdxGraphicBlobSerializer }

class function TdxGraphicBlobSerializer.GetStorageDBType: TFieldType;
begin
  Result := TFieldType.ftGraphic;
end;

{ TcxGridEMFDataController }

function TcxGridEMFDataController.CreateDetailLinkObject
  (ARelation: TcxCustomDataRelation; ARecordIndex: Integer): TObject;
begin
  Result := TcxGridLevelAccess.CreateLinkObject(TcxGridLevel(ARelation.Item),
    ARelation, ARecordIndex);
end;

initialization

TdxBlobSerializerFactory.Register(TGraphic, TdxGraphicBlobSerializer);

finalization

TdxBlobSerializerFactory.UnRegister(TGraphic);

end.
