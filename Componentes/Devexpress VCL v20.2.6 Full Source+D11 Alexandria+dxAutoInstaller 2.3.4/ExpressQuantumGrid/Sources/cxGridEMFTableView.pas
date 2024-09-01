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

unit cxGridEMFTableView;

interface

uses
  SysUtils, Classes, cxGridCustomTableView, cxGridCustomView, cxGridTableView,
  cxDataControllerConditionalFormatting,
  dxCoreClasses, cxEdit, cxDBEdit,
  cxStorage, cxCustomData, cxDataStorage,
  dxEMF.Metadata, cxEMFData, cxGridEMFDataDefinitions;

type

  TcxGridEMFTableView = class;

  { TcxGridEMFColumn }

  TcxGridEMFColumn = class(TcxGridColumn)
  strict private
    function GetDataBinding: TcxGridItemEMFDataBinding; inline;
    procedure SetDataBinding(Value: TcxGridItemEMFDataBinding); inline;
    function GetFieldName: string; inline;
    function GetIsVirtualColumn: Boolean; inline;
    function GetHasMemberInfo: Boolean; inline;
    function GetIsAssociationList: Boolean; inline;
    function GetDataController: TcxGridEMFDataController; inline;
  protected
    function CanEdit: Boolean; override;
    function CanFilter(AVisually: Boolean): Boolean; override;
    function CanGroup: Boolean; override;
    function CanSort: Boolean; override;
    procedure DateTimeGroupingChanged; override;

    property DataController: TcxGridEMFDataController read GetDataController;
    property HasMemberInfo: Boolean read GetHasMemberInfo;
    property IsVirtualColumn: Boolean read GetIsVirtualColumn;
    property IsAssociationList: Boolean read GetIsAssociationList;
  public
    property FieldName: string read GetFieldName;
  published
    property DataBinding: TcxGridItemEMFDataBinding read GetDataBinding
      write SetDataBinding;
  end;

  { TcxGridEMFSummaryItem }

  TcxGridEMFSummaryItem = class(TdxEMFSummaryItem, IUnknown, IcxStoredObject,
    IcxGridSummaryItem)
  strict private
    FDisplayText: string;
    FVisibleForCustomization: Boolean;
    function GetColumn: TcxGridEMFColumn; inline;
    function GetGridView: TcxGridEMFTableView;
    procedure SetColumn(Value: TcxGridEMFColumn);
    procedure SetDisplayText(const Value: string);
    procedure SetVisibleForCustomization(Value: Boolean);
  protected
    // IInterface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    // IcxStoredObject
    function GetObjectName: string;
    function GetProperties(AProperties: TStrings): Boolean;
    procedure GetPropertyValue(const AName: string; var AValue: Variant);
    procedure SetPropertyValue(const AName: string; const AValue: Variant);
    // IcxGridSummaryItem
    function GetDisplayText: string;
    function GetVisibleForCustomization: Boolean;

    function CanSetKind(Value: TcxSummaryKind): Boolean; override;

    property GridView: TcxGridEMFTableView read GetGridView;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    function FormatValue(const AValue: Variant; AIsFooter: Boolean)
      : string; override;
  published
    property Column: TcxGridEMFColumn read GetColumn write SetColumn;
    property DisplayText: string read FDisplayText write SetDisplayText;
    property Sorted;
    property VisibleForCustomization: Boolean read FVisibleForCustomization
      write SetVisibleForCustomization default True;
  end;

  { TcxGridEMFGroupRow }

  TcxGridEMFGroupRow = class(TcxGridGroupRow)
  protected
    function GetDisplayText(Index: Integer): string; override;
    function GetDisplayTextByGroupedColumn(AGroupedColumnIndex: Integer)
      : string; override;
  end;

  { TcxGridEMFMasterDataRow }

  TcxGridEMFMasterDataRow = class(TcxGridMasterDataRow)
  strict private
    function GetDataController: TdxEMFDataController; inline;
    procedure UpdateMasterRecordIndex;
  protected
    procedure RefreshRecordInfo; override;
    property DataController: TdxEMFDataController read GetDataController;
  public
    constructor Create(AViewData: TcxCustomGridTableViewData; AIndex: Integer;
      const ARecordInfo: TcxRowInfo); override;
  end;

  { TcxGridEMFNewItemRow }

  TcxGridEMFNewItemRow = class(TcxGridNewItemRow);

  { TcxGridEMFViewData }

  TcxGridEMFViewData = class(TcxGridViewData)
  protected
    function GetGroupRecordClass(const ARecordInfo: TcxRowInfo)
      : TcxCustomGridRecordClass; override;
    function GetMasterRecordClass(const ARecordInfo: TcxRowInfo)
      : TcxCustomGridRecordClass; override;
    function GetNewItemRecordClass: TcxCustomGridRecordClass; override;
  end;

  { TcxGridEMFScrollbarAnnotationOptionsHelper }

  TcxGridEMFScrollbarAnnotationOptionsHelper = class
    (TcxGridScrollbarAnnotationOptionsHelper)
  protected
    function GetDefaultValue(Index: Integer): Boolean; override;
    procedure SetValue(Index: Integer; Value: Boolean); override;
  end;

  { TcxGridEMFScrollbarAnnotationOptions }

  TcxGridEMFScrollbarAnnotationOptions = class
    (TcxGridScrollbarAnnotationOptions)
  protected
    function CreateHelper: TcxGridScrollbarAnnotationOptionsHelper; override;
  end;

  { TcxGridEMFTableFiltering }

  TcxGridEMFTableFiltering = class(TcxGridTableFiltering);

  { TcxGridEMFTableView }

  TcxGridEMFTableView = class(TcxGridTableView)
  strict private
    function GetColumn(Index: Integer): TcxGridEMFColumn; inline;
    function GetDataController: TcxGridEMFDataController;
    function GetFiltering: TcxGridEMFTableFiltering;
    procedure SetColumn(Index: Integer; Value: TcxGridEMFColumn); inline;
    procedure SetDataController(Value: TcxGridEMFDataController); inline;
    procedure SetFiltering(Value: TcxGridEMFTableFiltering);
  protected
    // IcxGridViewLayoutEditorSupport - for design-time layout editor
    function CanEditViewLayoutAndData: Boolean; override;
    // IcxDataControllerConditionalFormattingProviderOwner
    function GetConditionalFormattingProvider
      : TcxDataControllerConditionalFormattingProvider; override;

    procedure AfterRestoring; override;
    function CanBeUsedAsDetail: Boolean; override;
    function CanBeUsedAsMaster: Boolean; override;
    function CanGetHitTest: Boolean; override;
    function IsCheckBoxSelectionSupported: Boolean; override;
    function IsDataRowFixingSupported: Boolean; override;
    function IsDetailVisible(AGridView: TcxCustomGridView): Boolean; override;
    function IsMergedGroupsSupported: Boolean; override;
    function IsPersistentMultiSelectSupported: Boolean; override;

    function CreateScrollbarAnnotations
      : TcxGridScrollbarAnnotationOptions; override;
    function GetDataControllerClass: TcxCustomDataControllerClass; override;
    function GetFilteringClass: TcxCustomGridTableFilteringClass; override;
    function GetItemClass: TcxCustomGridTableItemClass; override;
    function GetSummaryItemClass: TcxDataSummaryItemClass; override;
    function GetViewDataClass: TcxCustomGridViewDataClass; override;
  public
    function CreateColumn: TcxGridEMFColumn;
    function GetColumnByFieldName(const AFieldName: string): TcxGridEMFColumn;
    property Columns[Index: Integer]: TcxGridEMFColumn read GetColumn
      write SetColumn;
  published
    property DataController: TcxGridEMFDataController read GetDataController
      write SetDataController;
    property Filtering: TcxGridEMFTableFiltering read GetFiltering
      write SetFiltering;
  end;

implementation

uses
  RTLConsts,
  Variants,
  dxEMF.Utils,
  cxGridDBDataDefinitions;

type
  TdxEMFDataControllerInfoAccess = class(TdxEMFDataControllerInfo);
  TdxEMFDataControllerAccess = class(TdxEMFDataController);
  TdxEMFDataFieldAccess = class(TdxEMFDataField);

  { TcxGridEMFColumn }

function TcxGridEMFColumn.GetDataController: TcxGridEMFDataController;
begin
  Result := TcxGridEMFDataController(inherited DataController);
end;

function TcxGridEMFColumn.GetHasMemberInfo: Boolean;
begin
  Result := DataBinding.MemberInfo <> nil;
end;

function TcxGridEMFColumn.GetIsAssociationList: Boolean;
begin
  Result := HasMemberInfo and DataBinding.MemberInfo.IsAssociationList;
end;

function TcxGridEMFColumn.GetIsVirtualColumn: Boolean;
begin
  Result := HasMemberInfo and DataBinding.MemberInfo.IsVirtualColumn;
end;

function TcxGridEMFColumn.CanEdit: Boolean;
begin
  Result := inherited CanEdit and not IsVirtualColumn and not IsAssociationList;
end;

function TcxGridEMFColumn.CanFilter(AVisually: Boolean): Boolean;
begin
  Result := inherited CanFilter(AVisually) and
    not(IsVirtualColumn or IsAssociationList);
end;

function TcxGridEMFColumn.CanGroup: Boolean;
begin
  Result := inherited CanGroup and not IsAssociationList and HasMemberInfo and
    (not IsVirtualColumn or DataController.InMemoryMode);
end;

function TcxGridEMFColumn.CanSort: Boolean;
begin
  Result := inherited CanSort and not IsAssociationList and HasMemberInfo and
    (not IsVirtualColumn or DataController.InMemoryMode);
end;

procedure TcxGridEMFColumn.DateTimeGroupingChanged;
begin
  TdxEMFDataControllerAccess(DataController).GroupingChangedFlag := True;
  inherited DateTimeGroupingChanged;
end;

function TcxGridEMFColumn.GetDataBinding: TcxGridItemEMFDataBinding;
begin
  Result := TcxGridItemEMFDataBinding(inherited DataBinding);
end;

function TcxGridEMFColumn.GetFieldName: string;
begin
  if DataBinding = nil then
    Result := ''
  else
    Result := DataBinding.FieldName;
end;

procedure TcxGridEMFColumn.SetDataBinding(Value: TcxGridItemEMFDataBinding);
begin
  inherited DataBinding := Value;
end;

{ TcxGridEMFSummaryItem }

procedure TcxGridEMFSummaryItem.Assign(Source: TPersistent);
var
  ASource: TcxGridEMFSummaryItem absolute Source;
begin
  inherited Assign(Source);
  if Source is TcxGridEMFSummaryItem then
  begin
    DisplayText := ASource.DisplayText;
    VisibleForCustomization := ASource.VisibleForCustomization;
  end;
end;

function TcxGridEMFSummaryItem.CanSetKind(Value: TcxSummaryKind): Boolean;
begin
  Result := GridView.IsRestoring or inherited CanSetKind(Value);
end;

constructor TcxGridEMFSummaryItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FVisibleForCustomization := True;
end;

function TcxGridEMFSummaryItem.FormatValue(const AValue: Variant;
  AIsFooter: Boolean): string;
begin
  try
    Result := inherited FormatValue(AValue, AIsFooter);
  except
    Result := VarToStr(AValue);
  end;
end;

function TcxGridEMFSummaryItem.GetColumn: TcxGridEMFColumn;
begin
  Result := TcxGridEMFColumn(ItemLink);
end;

function TcxGridEMFSummaryItem.GetDisplayText: string;
begin
  Result := DisplayText;
end;

function TcxGridEMFSummaryItem.GetGridView: TcxGridEMFTableView;
begin
  Result := TcxGridEMFTableView
    ((DataController as TcxCustomGridEMFDataController).GridView);
end;

function TcxGridEMFSummaryItem.GetObjectName: string;
begin
  Result := '';
end;

function TcxGridEMFSummaryItem.GetProperties(AProperties: TStrings): Boolean;
begin
  Result := False;
end;

procedure TcxGridEMFSummaryItem.GetPropertyValue(const AName: string;
  var AValue: Variant);
begin
  if AName = 'Column' then
    if Column <> nil then
      AValue := (Column as IcxStoredObject).GetObjectName
    else
      AValue := '';
end;

function TcxGridEMFSummaryItem.GetVisibleForCustomization: Boolean;
begin
  Result := VisibleForCustomization;
end;

function TcxGridEMFSummaryItem.QueryInterface(const IID: TGUID;
  out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TcxGridEMFSummaryItem.SetColumn(Value: TcxGridEMFColumn);
begin
  ItemLink := Value;
end;

procedure TcxGridEMFSummaryItem.SetDisplayText(const Value: string);
begin
  if FDisplayText = Value then
    Exit;
  FDisplayText := Value;
  GridView.Changed(vcProperty);
end;

procedure TcxGridEMFSummaryItem.SetPropertyValue(const AName: string;
  const AValue: Variant);
begin
  if AName = 'Column' then
    Column := TcxGridEMFColumn(TcxCustomGridTableViewAccess.FindItemByObjectName
      (GridView, AValue));
end;

procedure TcxGridEMFSummaryItem.SetVisibleForCustomization(Value: Boolean);
begin
  if FVisibleForCustomization = Value then
    Exit;
  FVisibleForCustomization := Value;
  GridView.Changed(vcProperty);
end;

function TcxGridEMFSummaryItem._AddRef: Integer;
begin
  Result := -1;
end;

function TcxGridEMFSummaryItem._Release: Integer;
begin
  Result := -1;
end;

{ TcxGridEMFViewData }

function TcxGridEMFViewData.GetGroupRecordClass(const ARecordInfo: TcxRowInfo)
  : TcxCustomGridRecordClass;
begin
  Result := TcxGridEMFGroupRow;
end;

function TcxGridEMFViewData.GetMasterRecordClass(const ARecordInfo: TcxRowInfo)
  : TcxCustomGridRecordClass;
begin
  Result := TcxGridEMFMasterDataRow;
end;

function TcxGridEMFViewData.GetNewItemRecordClass: TcxCustomGridRecordClass;
begin
  Result := TcxGridEMFNewItemRow;
end;

{ TcxGridEMFTableView }

procedure TcxGridEMFTableView.AfterRestoring;
begin
  DataController.UpdateItems(True);
  inherited AfterRestoring;
end;

function TcxGridEMFTableView.CanBeUsedAsDetail: Boolean;
begin
  Result := True;
end;

function TcxGridEMFTableView.CanBeUsedAsMaster: Boolean;
begin
  Result := True;
end;

function TcxGridEMFTableView.CanEditViewLayoutAndData: Boolean;
begin
  Result := False;
end;

function TcxGridEMFTableView.CanGetHitTest: Boolean;
begin
  Result := inherited CanGetHitTest and DataController.IsRowInfoValid;
end;

function TcxGridEMFTableView.CreateColumn: TcxGridEMFColumn;
begin
  Result := TcxGridEMFColumn(inherited CreateColumn);
end;

function TcxGridEMFTableView.CreateScrollbarAnnotations
  : TcxGridScrollbarAnnotationOptions;
begin
  Result := TcxGridEMFScrollbarAnnotationOptions.Create(Self);
end;

function TcxGridEMFTableView.GetColumn(Index: Integer): TcxGridEMFColumn;
begin
  Result := TcxGridEMFColumn(inherited Columns[Index]);
end;

function TcxGridEMFTableView.GetColumnByFieldName(const AFieldName: string)
  : TcxGridEMFColumn;
var
  I: Integer;
begin
  for I := 0 to ColumnCount - 1 do
  begin
    Result := Columns[I];
    if AnsiCompareText(Result.FieldName, AFieldName) = 0 then
      Exit;
  end;
  Result := nil;
end;

function TcxGridEMFTableView.GetConditionalFormattingProvider
  : TcxDataControllerConditionalFormattingProvider;
begin
  Result := nil;
end;

function TcxGridEMFTableView.GetDataController: TcxGridEMFDataController;
begin
  Result := TcxGridEMFDataController(FDataController);
end;

function TcxGridEMFTableView.GetFiltering: TcxGridEMFTableFiltering;
begin
  Result := TcxGridEMFTableFiltering(inherited Filtering);
end;

function TcxGridEMFTableView.GetDataControllerClass
  : TcxCustomDataControllerClass;
begin
  Result := TcxGridEMFDataController;
end;

function TcxGridEMFTableView.GetFilteringClass
  : TcxCustomGridTableFilteringClass;
begin
  Result := TcxGridEMFTableFiltering;
end;

function TcxGridEMFTableView.GetItemClass: TcxCustomGridTableItemClass;
begin
  Result := TcxGridEMFColumn;
end;

function TcxGridEMFTableView.GetSummaryItemClass: TcxDataSummaryItemClass;
begin
  Result := TcxGridEMFSummaryItem;
end;

function TcxGridEMFTableView.GetViewDataClass: TcxCustomGridViewDataClass;
begin
  Result := TcxGridEMFViewData;
end;

function TcxGridEMFTableView.IsCheckBoxSelectionSupported: Boolean;
begin
  Result := False;
end;

function TcxGridEMFTableView.IsDataRowFixingSupported: Boolean;
begin
  Result := False;
end;

function TcxGridEMFTableView.IsDetailVisible
  (AGridView: TcxCustomGridView): Boolean;
begin
  Result := (AGridView.MasterRecordIndex < DataController.RecordCount) and
    inherited IsDetailVisible(AGridView);
end;

function TcxGridEMFTableView.IsMergedGroupsSupported: Boolean;
begin
  Result := True;
end;

function TcxGridEMFTableView.IsPersistentMultiSelectSupported: Boolean;
begin
  Result := False;
end;

procedure TcxGridEMFTableView.SetColumn(Index: Integer;
  Value: TcxGridEMFColumn);
begin
  inherited Columns[Index] := Value;
end;

procedure TcxGridEMFTableView.SetDataController
  (Value: TcxGridEMFDataController);
begin
  FDataController.Assign(Value);
end;

procedure TcxGridEMFTableView.SetFiltering(Value: TcxGridEMFTableFiltering);
begin
  inherited Filtering := Value;
end;

{ TcxGridEMFScrollbarAnnotationOptionsHelper }

function TcxGridEMFScrollbarAnnotationOptionsHelper.GetDefaultValue
  (Index: Integer): Boolean;
begin
  Result := Index in [1, 2];
end;

procedure TcxGridEMFScrollbarAnnotationOptionsHelper.SetValue(Index: Integer;
  Value: Boolean);
begin
  if not(Index in [3, 4]) then
    inherited SetValue(Index, Value);
end;

{ TcxGridEMFScrollbarAnnotationOptions }

function TcxGridEMFScrollbarAnnotationOptions.CreateHelper
  : TcxGridScrollbarAnnotationOptionsHelper;
begin
  Result := TcxGridEMFScrollbarAnnotationOptionsHelper.Create
    (GridView as TcxCustomGridTableView);
end;

{ TcxGridEMFMasterDataRow }

constructor TcxGridEMFMasterDataRow.Create
  (AViewData: TcxCustomGridTableViewData; AIndex: Integer;
  const ARecordInfo: TcxRowInfo);
begin
  inherited;
  UpdateMasterRecordIndex;
end;

function TcxGridEMFMasterDataRow.GetDataController: TdxEMFDataController;
begin
  Result := TdxEMFDataController(inherited DataController);
end;

procedure TcxGridEMFMasterDataRow.RefreshRecordInfo;
begin
  inherited RefreshRecordInfo;
  UpdateMasterRecordIndex;
end;

procedure TcxGridEMFMasterDataRow.UpdateMasterRecordIndex;
var
  I: Integer;
begin
  if DataController.HasRelations then
    for I := 0 to DetailGridViewCount - 1 do
      if DetailGridViewExists[I] then
        TdxEMFDataControllerAccess(DetailGridViews[I].DataController)
          .SetMasterRecordIndex(RecordIndex);
end;

{ TcxGridEMFGroupRow }

function TcxGridEMFGroupRow.GetDisplayText(Index: Integer): string;
begin
  if ViewData.HasCustomDataHandling(GroupedColumn, doGrouping) then
    Result := ViewData.GetCustomDataDisplayText(GroupedColumn.Index, Value)
  else
    Result := inherited GetDisplayText(Index);
end;

function TcxGridEMFGroupRow.GetDisplayTextByGroupedColumn(AGroupedColumnIndex
  : Integer): string;
var
  AColumn: TcxGridEMFColumn;
  AColumnIndex: Integer;
  AValue: Variant;
begin
  AColumn := GroupedColumns[AGroupedColumnIndex] as TcxGridEMFColumn;
  AColumnIndex := AColumn.Index;
  if ViewData.HasCustomDataHandling(AColumn, doGrouping) then
  begin
    AValue := DataController.GetGroupRowValue(RecordInfo, AColumnIndex);
    if TdxEMFDataFieldAccess(AColumn.DataBinding.Field).DateTimeGrouping = dtgByHour
    then
      AValue := EncodeTime(AValue, 0, 0, 0)
    else
      AValue := TdxEMFDataFieldAccess(AColumn.DataBinding.Field)
        .TryCastVariant(AValue);
    Result := ViewData.GetCustomDataDisplayText(AColumnIndex, AValue);
  end
  else
    Result := DataController.GetGroupRowDisplayText(RecordInfo, AColumnIndex);
end;

initialization

cxGridRegisteredViews.Register(TcxGridEMFTableView, 'EMF Table');
Classes.RegisterClasses([TcxGridEMFColumn, TcxGridTableViewStyleSheet]);

finalization

cxGridRegisteredViews.Unregister(TcxGridEMFTableView);

end.
