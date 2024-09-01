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

unit cxGridEMFReg;

interface

uses
  DesignEditors, DesignIntf, DesignMenus, VCLEditors,
  Classes, Controls, ExtCtrls, Graphics, Menus, SysUtils, TypInfo, ImgList,
  cxGridCustomView, cxClasses, dxEMF.Reg;

procedure Register;

implementation

uses
  Types, RTLConsts,
  dxEMF.Core.Reg,
  dxEMF.MetaData,
  dxEMF.DataDefinitions,
  cxLibraryReg, cxEdit, cxGridReg, cxGridLevel,
  cxEMFData, cxEMFLookupGrid, cxEMFLookupEdit, cxEMFEntityEditor,
  cxEditRepositoryEditor,
  cxGridEMFTableView, cxGridTableView, cxGridCustomTableView,
  cxGridEMFDataDefinitions;

type

  TcxEMFLookupGridColumnAccess = class(TcxEMFLookupGridColumn);
  TdxEMFCustomDataContextAccess = class(TdxEMFCustomDataContext);
  TdxEntityInfoAccess = class(TdxEntityInfo);

  { TdxEMFDataSourceEntityNameProperty }

  TdxEMFDataSourceEntityNameProperty = class(TStringProperty)
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TdxEMFFieldNameProperty }

  TdxEMFMemberNameProperty = class(TStringProperty)
  protected
    function GetDataController: TdxEMFDataController; virtual;
    function GetEntityInfo: TdxEntityInfo;
    procedure GetValueList(AList: TStrings); virtual;
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  { TdxEMFMasterMemberNameProperty }

  TdxEMFMasterMemberNameProperty = class(TdxEMFMemberNameProperty)
  protected
    function GetDataController: TdxEMFDataController; override;
  end;

  { TdxEMFDataControllerCollectionName }

  TdxEMFDataControllerCollectionName = class(TdxEMFMasterMemberNameProperty)
  protected
    procedure GetValueList(AList: TStrings); override;
  end;

  { TdxEMFDataControllerMasterKeyFieldNames }

  TdxEMFDataControllerMasterKeyFieldNames = class
    (TdxEMFMasterMemberNameProperty, ICustomPropertyListDrawing)
  protected
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean);
  public
    procedure GetValueList(AList: TStrings); override;
  end;

  { TdxEMFDataControllerDetailKeyFieldNames }

  TdxEMFDataControllerDetailKeyFieldNames = class(TdxEMFMemberNameProperty,
    ICustomPropertyListDrawing)
  protected
    procedure ListMeasureHeight(const Value: string; ACanvas: TCanvas;
      var AHeight: Integer);
    procedure ListMeasureWidth(const Value: string; ACanvas: TCanvas;
      var AWidth: Integer);
    procedure ListDrawValue(const Value: string; ACanvas: TCanvas;
      const ARect: TRect; ASelected: Boolean);
    procedure GetValueList(AList: TStrings); override;
  end;

  { TcxEMFDataBindingFieldNamePropertyEditor }

  TcxEMFDataBindingFieldNamePropertyEditor = class(TdxEMFMemberNameProperty)
  protected
    function GetDataController: TdxEMFDataController; override;
  end;

  { TcxEMFLookupGridColumnFieldNamePropertyEditor }

  TcxEMFLookupGridColumnFieldNamePropertyEditor = class
    (TdxEMFMemberNameProperty)
  protected
    function GetDataController: TdxEMFDataController; override;
  end;

  { TdxEMFDataSummaryItemFieldNameProperty }

  TdxEMFDataSummaryItemFieldNameProperty = class(TdxEMFMemberNameProperty)
  protected
    function GetDataController: TdxEMFDataController; override;
  end;

  TcxGridEMFTableItemPropertiesProperty = class
    (TcxCustomGridTableItemPropertiesProperty)
  protected
    function IsAssociation(AItem: TcxGridEMFColumn): Boolean;
  public
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TcxGridEMFColumnRepositoryItemProperty }

  TcxGridEMFColumnRepositoryItemProperty = class(TComponentProperty)
  private
    FAssociationCount: Integer;
    FNonAssociationCount: Integer;
    FStrProc: TGetStrProc;
    function IsAssociation(AItem: TcxGridEMFColumn): Boolean;
    procedure StrProc(const S: string);
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  { TdxEMFDataSourceEntityNameProperty }

function TdxEMFDataSourceEntityNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

procedure TdxEMFDataSourceEntityNameProperty.GetValues(Proc: TGetStrProc);
var
  ADataSource: TdxCustomEMFDataSource;
  AEntityClasses: TArray<TClass>;
  AClass: TClass;
begin
  ADataSource := GetComponent(0) as TdxCustomEMFDataSource;
  if ADataSource.DataContext <> nil then
    AEntityClasses := TdxEMFCustomDataContextAccess(ADataSource.DataContext)
      .GetEntityClasses
  else if ADataSource.PackageName <> '' then
    AEntityClasses := TdxEMFCustomDataContextAccess.GetEntityClasses
      (ADataSource.PackageName)
  else
    Exit;
  for AClass in AEntityClasses do
    Proc(TdxEntityInfoAccess.GetQualifiedClassName(AClass));
end;

{ TdxEMFDataSummaryItemFieldNameProperty }

function TdxEMFDataSummaryItemFieldNameProperty.GetDataController
  : TdxEMFDataController;
begin
  Result := (GetComponent(0) as TdxEMFSummaryItem).DataController;
end;

{ TcxEMFDataBindingFieldNamePropertyEditor }

function TcxEMFDataBindingFieldNamePropertyEditor.GetDataController
  : TdxEMFDataController;
begin
  Result := (GetComponent(0) as TcxGridItemEMFDataBinding).DataController;
end;

{ TcxEMFLookupGridColumnFieldNamePropertyEditor }

function TcxEMFLookupGridColumnFieldNamePropertyEditor.GetDataController
  : TdxEMFDataController;
begin
  Result := TcxEMFLookupGridColumnAccess
    (GetComponent(0) as TcxEMFLookupGridColumn).DataController;
end;

{ TdxEMFDataControllerDetailKeyFieldNames }

procedure TdxEMFDataControllerDetailKeyFieldNames.GetValueList(AList: TStrings);
var
  AEntityInfo: TdxEntityInfo;
  AMemberInfo: TdxMappingMemberInfo;
begin
  AEntityInfo := GetEntityInfo;
  if AEntityInfo <> nil then
    for AMemberInfo in AEntityInfo.PersistentProperties do
      AList.Add(AMemberInfo.MemberName);
end;

procedure TdxEMFDataControllerDetailKeyFieldNames.ListDrawValue
  (const Value: string; ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
var
  AIsKeyField: Boolean;
  AEntityInfo: TdxEntityInfo;
  AMemberInfo: TdxMappingMemberInfo;
begin
  AEntityInfo := GetEntityInfo;
  if AEntityInfo = nil then
    Exit;
  AIsKeyField := False;
  for AMemberInfo in AEntityInfo.PersistentProperties do
    if SameText(AMemberInfo.MemberName, Value) and AMemberInfo.IsAssociation
    then
    begin
      AIsKeyField := True;
      Break
    end;
  if AIsKeyField then
    ACanvas.Font.Style := [fsBold]
  else
    ACanvas.Font.Style := [];
  if not ASelected then
  begin
    ACanvas.Brush.Color := clWindow;
    ACanvas.FillRect(ARect);
    if AIsKeyField then
      ACanvas.Font.Color := clWindowText
    else
      ACanvas.Font.Color := clGrayText;
  end
  else
  begin
    ACanvas.Brush.Color := clHighlight;
    ACanvas.FillRect(ARect);
    ACanvas.Font.Color := clHighlightText;
  end;
  ACanvas.Brush.Style := bsClear;
  ACanvas.TextOut(ARect.Left + 2, ARect.Top + 2, Value);
  ACanvas.Brush.Style := bsSolid;
end;

procedure TdxEMFDataControllerDetailKeyFieldNames.ListMeasureHeight
  (const Value: string; ACanvas: TCanvas; var AHeight: Integer);
begin
end;

procedure TdxEMFDataControllerDetailKeyFieldNames.ListMeasureWidth
  (const Value: string; ACanvas: TCanvas; var AWidth: Integer);
begin

end;

{ TdxEMFDataControllerMasterKeyFieldNames }

procedure TdxEMFDataControllerMasterKeyFieldNames.GetValueList(AList: TStrings);
var
  AEntityInfo: TdxEntityInfo;
  AMemberInfo: TdxMappingMemberInfo;
begin
  AEntityInfo := GetEntityInfo;
  if AEntityInfo <> nil then
    for AMemberInfo in AEntityInfo.PersistentProperties do
      AList.Add(AMemberInfo.MemberName);
end;

procedure TdxEMFDataControllerMasterKeyFieldNames.ListDrawValue
  (const Value: string; ACanvas: TCanvas; const ARect: TRect;
  ASelected: Boolean);
var
  AIsKeyField: Boolean;
  AEntityInfo: TdxEntityInfo;
begin
  AEntityInfo := GetEntityInfo;
  if AEntityInfo = nil then
    Exit;
  AIsKeyField := AEntityInfo.KeyAttributes.IndexOfName(Value) >= 0;
  if AIsKeyField then
    ACanvas.Font.Style := [fsBold]
  else
    ACanvas.Font.Style := [];
  if not ASelected then
  begin
    ACanvas.Brush.Color := clWindow;
    ACanvas.FillRect(ARect);
    if AIsKeyField then
      ACanvas.Font.Color := clWindowText
    else
      ACanvas.Font.Color := clGrayText;
  end
  else
  begin
    ACanvas.Brush.Color := clHighlight;
    ACanvas.FillRect(ARect);
    ACanvas.Font.Color := clHighlightText;
  end;
  ACanvas.Brush.Style := bsClear;
  ACanvas.TextOut(ARect.Left + 2, ARect.Top + 2, Value);
  ACanvas.Brush.Style := bsSolid;
end;

procedure TdxEMFDataControllerMasterKeyFieldNames.ListMeasureHeight
  (const Value: string; ACanvas: TCanvas; var AHeight: Integer);
begin

end;

procedure TdxEMFDataControllerMasterKeyFieldNames.ListMeasureWidth
  (const Value: string; ACanvas: TCanvas; var AWidth: Integer);
begin

end;

{ TdxEMFMasterMemberNameProperty }

function TdxEMFMasterMemberNameProperty.GetDataController: TdxEMFDataController;
var
  AIDataController: IcxCustomGridDataController;
  AParentLevel: TcxGridLevel;
begin
  Result := nil;
  if Supports(GetComponent(0), IcxCustomGridDataController, AIDataController)
  then
  begin
    AParentLevel := AIDataController.GridView.Level as TcxGridLevel;
    if AParentLevel <> nil then
      AParentLevel := AParentLevel.Parent;
    if (AParentLevel <> nil) and (AParentLevel.GridView <> nil) and
      (AParentLevel.GridView.DataController is TdxEMFDataController) then
      Result := TdxEMFDataController(AParentLevel.GridView.DataController);
  end;
end;

{ TdxEMFDataControllerCollectionName }

procedure TdxEMFDataControllerCollectionName.GetValueList(AList: TStrings);
var
  AEntityInfo: TdxEntityInfo;
  AMemberInfo: TdxMappingMemberInfo;
begin
  AEntityInfo := GetEntityInfo;
  if AEntityInfo <> nil then
    for AMemberInfo in AEntityInfo.AssociationListProperties do
      AList.Add(AMemberInfo.MemberName);
end;

{ TdxEMFFieldNameProperty }

function TdxEMFMemberNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

function TdxEMFMemberNameProperty.GetDataController: TdxEMFDataController;
begin
  Result := GetComponent(0) as TdxEMFDataController;
end;

function TdxEMFMemberNameProperty.GetEntityInfo: TdxEntityInfo;
var
  ADataController: TdxEMFDataController;
begin
  ADataController := GetDataController;
  if (ADataController = nil) or (ADataController.DataSource = nil) then
    Result := nil
  else
    Result := ADataController.DataSource.EntityInfo;
end;

procedure TdxEMFMemberNameProperty.GetValueList(AList: TStrings);
var
  AEntityInfo: TdxEntityInfo;
  AMemberInfo: TdxMappingMemberInfo;
begin
  AEntityInfo := GetEntityInfo;
  if AEntityInfo <> nil then
    for AMemberInfo in AEntityInfo.PersistentProperties do
      AList.Add(AMemberInfo.MemberName);
end;

procedure TdxEMFMemberNameProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  AValues: TStringList;
begin
  AValues := TStringList.Create;
  try
    GetValueList(AValues);
    for I := 0 to AValues.Count - 1 do
      Proc(AValues[I]);
  finally
    AValues.Free;
  end;
end;

{ TcxGridEMFTableItemPropertiesProperty }

function TcxGridEMFTableItemPropertiesProperty.IsAssociation
  (AItem: TcxGridEMFColumn): Boolean;
begin
  Result := (AItem.DataBinding.MemberInfo <> nil) and
    (AItem.DataBinding.MemberInfo.IsAssociation or
    AItem.DataBinding.MemberInfo.IsReference);
end;

procedure TcxGridEMFTableItemPropertiesProperty.GetValues(Proc: TGetStrProc);
var
  I, AAssociationCount, ANonAssociationCount: Integer;
  ADesc: string;
  AItem: TcxGridEMFColumn;
begin
  AAssociationCount := 0;
  ANonAssociationCount := 0;
  for I := 0 to PropCount - 1 do
  begin
    AItem := TcxGridEMFColumn(GetComponent(I));
    if IsAssociation(AItem) then
      Inc(AAssociationCount)
    else
      Inc(ANonAssociationCount);
  end;
  if AAssociationCount = PropCount then
    Proc(GetRegisteredEditProperties.GetDescriptionByClass
      (TcxEMFLookupComboBoxProperties))
  else
  begin
    for I := 0 to GetRegisteredEditProperties.Count - 1 do
    begin
      if (GetRegisteredEditProperties[I] = TcxEMFLookupComboBoxProperties) and
        (ANonAssociationCount = PropCount) then
        Continue;
      ADesc := GetRegisteredEditProperties.Descriptions[I];
      if ADesc <> '' then
        Proc(ADesc);
    end;
  end;
end;

procedure TcxGridEMFTableItemPropertiesProperty.SetValue(const Value: string);
const
  scxDoesNotMatchPropertyType = 'The selected property type doesn' +
    '''t match the property type of the component introduced in an ancestor form';
var
  APropertiesClass: TcxCustomEditPropertiesClass;
  I: Integer;
  AItem: TcxGridEMFColumn;
  AIsAssociation: Boolean;
begin
  APropertiesClass := TcxCustomEditPropertiesClass
    (GetRegisteredEditProperties.FindByClassName(Value));
  if APropertiesClass = nil then
    APropertiesClass := TcxCustomEditPropertiesClass
      (GetRegisteredEditProperties.FindByDescription(Value));
  for I := 0 to PropCount - 1 do
  begin
    AItem := TcxGridEMFColumn(GetComponent(I));
    if (AItem.PropertiesClass <> APropertiesClass) and
      (csAncestor in AItem.ComponentState) then
      raise Exception.Create(scxDoesNotMatchPropertyType);

    AIsAssociation := IsAssociation(AItem);
    if (AIsAssociation = (APropertiesClass = TcxEMFLookupComboBoxProperties)) or
      (APropertiesClass = nil) then
      AItem.PropertiesClass := APropertiesClass;
  end;
  Modified;
end;

{ TcxGridEMFColumnRepositoryItemProperty }

function TcxGridEMFColumnRepositoryItemProperty.GetAttributes
  : TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  if TcxGridEMFColumn(GetComponent(0)).RepositoryItem <> nil then
    Include(Result, paNotNestable);
end;

procedure TcxGridEMFColumnRepositoryItemProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  AItem: TcxGridEMFColumn;
begin
  FAssociationCount := 0;
  FNonAssociationCount := 0;
  for I := 0 to PropCount - 1 do
  begin
    AItem := TcxGridEMFColumn(GetComponent(I));
    if IsAssociation(AItem) then
      Inc(FAssociationCount)
    else
      Inc(FNonAssociationCount);
  end;
  FStrProc := Proc;
  Designer.GetComponentNames(GetTypeData(GetPropType), StrProc);
end;

function TcxGridEMFColumnRepositoryItemProperty.IsAssociation
  (AItem: TcxGridEMFColumn): Boolean;
begin
  Result := (AItem.DataBinding.MemberInfo <> nil) and
    AItem.DataBinding.MemberInfo.IsAssociation;
end;

procedure TcxGridEMFColumnRepositoryItemProperty.SetValue(const Value: string);
var
  I: Integer;
  AItem: TcxGridEMFColumn;
  AIsAssociation: Boolean;
  AComponent: TComponent;
begin
  if Value = '' then
    AComponent := nil
  else
  begin
    AComponent := Designer.GetComponent(Value);
    if not(AComponent is GetTypeData(GetPropType)^.ClassType) then
      raise EDesignPropertyError.CreateRes(@SInvalidPropertyValue);
  end;
  for I := 0 to PropCount - 1 do
  begin
    AItem := TcxGridEMFColumn(GetComponent(I));
    AIsAssociation := IsAssociation(AItem);
    if (AComponent = nil) or
      (AIsAssociation = (AComponent.ClassType =
      TcxEditRepositoryEMFLookupComboBoxItem)) then
      AItem.RepositoryItem := TcxEditRepositoryItem(AComponent);
  end;
  Modified;
end;

procedure TcxGridEMFColumnRepositoryItemProperty.StrProc(const S: string);
begin
  if FAssociationCount = PropCount then
  begin
    if TcxEditRepositoryItem(Designer.GetComponent(S)).ClassType = TcxEditRepositoryEMFLookupComboBoxItem
    then
      FStrProc(S);
  end
  else
  begin
    if FNonAssociationCount = PropCount then
    begin
      if TcxEditRepositoryItem(Designer.GetComponent(S)).ClassType <> TcxEditRepositoryEMFLookupComboBoxItem
      then
        FStrProc(S);
    end
    else
      FStrProc(S);
  end;
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);
  RegisterClasses([TcxEditRepositoryEMFLookupComboBoxItem]);
  RegisterEMFComponent(TdxEMFDataSource);
  RegisterEditRepositoryItem(TcxEditRepositoryEMFLookupComboBoxItem,
    cxSEditRepositoryEMFLookupComboBoxItem);

  RegisterPropertyEditor(TypeInfo(string), TdxCustomEMFDataSource,
    'PackageName', TdxEMFPackageNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxCustomEMFDataSource, 'EntityName',
    TdxEMFDataSourceEntityNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxEMFDataController,
    'CollectionName', TdxEMFDataControllerCollectionName);
  RegisterPropertyEditor(TypeInfo(string), TdxEMFDataController,
    'MasterKeyFieldNames', TdxEMFDataControllerMasterKeyFieldNames);
  RegisterPropertyEditor(TypeInfo(string), TdxEMFDataController,
    'DetailKeyFieldNames', TdxEMFDataControllerDetailKeyFieldNames);
  RegisterPropertyEditor(TypeInfo(string), TdxEMFSummaryItem, 'FieldName',
    TdxEMFDataSummaryItemFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TcxGridItemEMFDataBinding,
    'FieldName', TcxEMFDataBindingFieldNamePropertyEditor);
  RegisterPropertyEditor(TypeInfo(string), TcxEMFLookupGridColumn, 'FieldName',
    TcxEMFLookupGridColumnFieldNamePropertyEditor);

  RegisterPropertyEditor(TypeInfo(TcxCustomEditProperties), TcxGridEMFColumn,
    'Properties', TcxGridEMFTableItemPropertiesProperty);
  RegisterPropertyEditor(TypeInfo(TcxEditRepositoryItem), TcxGridEMFColumn,
    'RepositoryItem', TcxGridEMFColumnRepositoryItemProperty);

  RegisterPropertyEditor(TypeInfo(TcxCustomGridTableItem),
    TcxGridEMFSummaryItem, 'Column', TcxGridTableSummaryItemColumnProperty);

  UnlistPublishedProperty(TcxGridEMFTableFiltering, 'ExpressionEditing');
  UnlistPublishedProperty(TcxGridEMFScrollbarAnnotationOptions, 'ShowErrors');
  UnlistPublishedProperty(TcxGridEMFScrollbarAnnotationOptions,
    'ShowSearchResults');
  UnlistPublishedProperty(TcxGridEMFColumn, 'VisibleForExpressionEditor');
  UnlistPublishedProperty(TcxGridItemEMFDataBinding, 'Expression');
  UnlistPublishedProperty(TcxGridItemEMFDataBinding, 'ValueType');
  UnlistPublishedProperty(TcxGridEMFColumn, 'VisibleForExpressionEditor');
  UnlistPublishedProperty(TcxGridEMFTableView, 'FixedDataRows');
  RegisterClass(TdxEMFQueryProperties);

  RegisterNoIcon([TcxGridEMFTableView]);
  HideClassProperties(TcxEMFLookupGridColumn, ['RepositoryItem']);
end;

end.
