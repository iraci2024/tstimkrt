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

unit cxGridWizardEMFViewsSelectItemsForDisplayPage;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, DB, ComCtrls,
  dxCore, cxGraphics, cxClasses, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, dxLayoutControlAdapters,
  dxLayoutContainer, cxContainer, cxEdit,
  cxListBox, StdCtrls, cxButtons, dxLayoutControl, cxGridWizardCustomPage,
  cxGridWizardStrs, dxLayoutLookAndFeels,
  cxCheckListBox, cxListView, cxCheckBox, cxCustomListBox,
  cxGridWizardEMFTableViewHelper, dxEMF.Metadata;

type
  { TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame }

  TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame = class
    (TcxGridWizardCustomPageFrame)
    btnAddAllFields: TcxButton;
    btnAddField: TcxButton;
    btnDeleteAllFields: TcxButton;
    btnDeleteField: TcxButton;
    btnMoveFieldDown: TcxButton;
    btnMoveFieldUp: TcxButton;
    lciAddAllFields: TdxLayoutItem;
    lciAddField: TdxLayoutItem;
    lciDataSetFields: TdxLayoutItem;
    lciDeleteAllFields: TdxLayoutItem;
    lciDeleteField: TdxLayoutItem;
    lciGridViewFields: TdxLayoutItem;
    lciMoveFieldDown: TdxLayoutItem;
    lciMoveFieldUp: TdxLayoutItem;
    lcUsedFieldsGroup1: TdxLayoutGroup;
    lcUsedFieldsGroup2: TdxLayoutGroup;
    lvDataSetFields: TcxCheckListBox;
    lvGridViewFields: TcxCheckListBox;
    pupmDataSetFieldsCustomization: TPopupMenu;
    pupmGridViewFieldsCustomization: TPopupMenu;
    pupmitDataSetAddAll: TMenuItem;
    pupmitDataSetAddSelected: TMenuItem;
    pupmitDataSetDeselectAll: TMenuItem;
    pupmitDataSetSelectAll: TMenuItem;
    pupmitDataSetSeparator: TMenuItem;
    pupmitGridViewCheckSelected: TMenuItem;
    pupmitGridViewDeleteAll: TMenuItem;
    pupmitGridViewDeleteSelected: TMenuItem;
    pupmitGridViewDeselectAll: TMenuItem;
    pupmitGridViewMoveDown: TMenuItem;
    pupmitGridViewMoveUp: TMenuItem;
    pupmitGridViewSelectAll: TMenuItem;
    pupmitGRidViewSeparator1: TMenuItem;
    pupmitGRidViewSeparator2: TMenuItem;
    pupmitGRidViewSeparator3: TMenuItem;
    pupmitGridViewUncheckSelected: TMenuItem;
    procedure btnAddAllFieldsClick(Sender: TObject);
    procedure btnAddFieldClick(Sender: TObject);
    procedure btnDeleteAllFieldsClick(Sender: TObject);
    procedure btnDeleteFieldClick(Sender: TObject);
    procedure btnMoveFieldDownClick(Sender: TObject);
    procedure btnMoveFieldUpClick(Sender: TObject);
    procedure lvDataSetFieldsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lvDataSetFieldsEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure lvGridViewFieldsClick(Sender: TObject);
    procedure lvGridViewFieldsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lvGridViewFieldsEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure pupmitDataSetDeselectAllClick(Sender: TObject);
    procedure pupmitDataSetSelectAllClick(Sender: TObject);
    procedure pupmitGridViewCheckSelectedClick(Sender: TObject);
    procedure pupmitGridViewDeselectAllClick(Sender: TObject);
    procedure pupmitGridViewSelectAllClick(Sender: TObject);
    procedure pupmitGridViewUncheckSelectedClick(Sender: TObject);
  private
    FGridViewChanged: Boolean;

    function GetColumns: TdxMemberInfoList;
    function GetHelper: TcxGridWizardEMFTableViewHelper;

    procedure ChangeCheckStateForSelected(AList: TcxCheckListBox;
      ACheckState: Boolean);
    procedure CheckMissingFields;
    procedure MoveSelected(ASource, ATarget: TcxCheckListBox);
    procedure PopulateDataSetFields;
    procedure PopulateGridViewFields;
    procedure ReorderDataSetFields;
    procedure SwapItems(AList: TcxCheckListBox; AIndex1, AIndex2: Integer);
    procedure UpdateControlsState;
  protected
    function GetPageDescription: string; override;
    function GetPageTitle: string; override;

    property Columns: TdxMemberInfoList read GetColumns;
    property Helper: TcxGridWizardEMFTableViewHelper read GetHelper;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ApplyLocalization; override;
    procedure ApplySettings; override;
    procedure LoadSettings; override;
  end;

implementation

uses
  cxGridWizardCustomHelper, Math, cxGeometry;

{$R *.dfm}

function GetSelectedCount(AList: TcxCheckListBox): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to AList.Count - 1 do
  begin
    if AList.Selected[I] then
      Inc(Result);
  end;
end;

procedure SelectAll(AList: TcxCheckListBox);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
    AList.Selected[I] := True;
end;

procedure SelectNone(AList: TcxCheckListBox);
var
  I: Integer;
begin
  for I := 0 to AList.Count - 1 do
    AList.Selected[I] := False;
end;

{ TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame }

constructor TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.Create
  (AOwner: TComponent);
begin
  inherited Create(AOwner);
  lvDataSetFields.InnerCheckListBox.MultiSelect := True;
  lvGridViewFields.InnerCheckListBox.MultiSelect := True;
end;

procedure TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.ApplyLocalization;
begin
  lciDataSetFields.Caption := cxGetResourceString
    (@scxgwEMFSelectItemsForDisplayDataSourceFields);
  lciGridViewFields.Caption := cxGetResourceString
    (@scxgwSelectItemsForDisplayGridViewFields);

  pupmitDataSetAddAll.Caption := cxGetResourceString(@scxgwCommonAddAll);
  pupmitDataSetAddSelected.Caption :=
    cxGetResourceString(@scxgwCommonAddSelected);
  pupmitDataSetDeselectAll.Caption :=
    cxGetResourceString(@scxgwCommonDeselectAll);
  pupmitDataSetSelectAll.Caption := cxGetResourceString(@scxgwCommonSelectAll);
  pupmitGridViewDeleteAll.Caption := cxGetResourceString(@scxgwCommonDeleteAll);
  pupmitGridViewDeleteSelected.Caption :=
    cxGetResourceString(@scxgwCommonDeleteSelected);
  pupmitGridViewDeselectAll.Caption :=
    cxGetResourceString(@scxgwCommonDeselectAll);
  pupmitGridViewSelectAll.Caption := cxGetResourceString(@scxgwCommonSelectAll);
  pupmitGridViewMoveDown.Caption :=
    cxGetResourceString(@scxgwCommonMoveSelectedDown);
  pupmitGridViewMoveUp.Caption := cxGetResourceString
    (@scxgwCommonMoveSelectedUp);
  pupmitGridViewCheckSelected.Caption :=
    cxGetResourceString(@scxgwCommonCheckSelected);
  pupmitGridViewUncheckSelected.Caption :=
    cxGetResourceString(@scxgwCommonUncheckSelected);
end;

procedure TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.ApplySettings;
var
  I, AItemIndex: Integer;
  AFieldName: string;
  AUnboundIndexes: array of Integer;
  ACount: Integer;
begin
  if not FGridViewChanged then
    Exit;

  Helper.DataController.BeginUpdateFields;
  try
    ACount := 0;
    for I := 0 to Helper.ItemsCount - 1 do
      if Helper.ItemFieldName[I] = '' then
      begin
        Inc(ACount);
        SetLength(AUnboundIndexes, ACount);
        AUnboundIndexes[ACount - 1] := I;
      end;

    for I := 0 to lvDataSetFields.Items.Count - 1 do
    begin
      AFieldName := lvDataSetFields.Items[I].Text;
      if Helper.GetItemIndexByFieldName(AFieldName) >= 0 then
        Helper.DeleteItem(AFieldName);
    end;

    for I := 0 to lvGridViewFields.Items.Count - 1 do
    begin
      AFieldName := lvGridViewFields.Items[I].Text;
      AItemIndex := Helper.GetItemIndexByFieldName(AFieldName);
      if AItemIndex < 0 then
      begin
        Helper.AddItem(AFieldName);
        Helper.ChangeItemIndex(Helper.ItemsCount - 1, I);
      end
      else if AItemIndex <> I then
        Helper.ChangeItemIndex(AItemIndex, I);
      Helper.ItemVisible[I] := lvGridViewFields.Items[I].Checked;
    end;

    for I := 0 to ACount - 1 do
      Helper.ChangeItemIndex(lvGridViewFields.Items.Count + I,
        AUnboundIndexes[I]);
  finally
    Helper.DataController.EndUpdateFields;
  end;
end;

procedure TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.LoadSettings;
begin
  if Helper.DataController.DataSource.EntityInfo <> nil then
  begin
    CheckMissingFields;
    PopulateDataSetFields;
    PopulateGridViewFields;
  end;
  UpdateControlsState;
  FGridViewChanged := False;
end;

function TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.
  GetPageDescription: string;
begin
  Result := cxGetResourceString(@scxgwSelectItemsForDisplayPageDescription);
end;

function TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.
  GetPageTitle: string;
begin
  Result := cxGetResourceString(@scxgwSelectItemsForDisplayPageTitle);
end;

function TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.GetColumns
  : TdxMemberInfoList;
begin
  Result := Helper.DataController.DataSource.EntityInfo.MemberAttributes;
end;

function TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.GetHelper
  : TcxGridWizardEMFTableViewHelper;
begin
  Result := inherited Helper as TcxGridWizardEMFTableViewHelper;
end;

procedure TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.
  ChangeCheckStateForSelected(AList: TcxCheckListBox; ACheckState: Boolean);
var
  I: Integer;
begin
  AList.Items.BeginUpdate;
  try
    for I := 0 to AList.Items.Count - 1 do
      if AList.Selected[I] then
        lvGridViewFields.Items[I].Checked := ACheckState;
  finally
    AList.Items.EndUpdate;
    FGridViewChanged := True;
  end;
end;

procedure TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.
  CheckMissingFields;

  function GetColumn(const AName: string): TdxMappingMemberInfo;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to Columns.Count - 1 do
      if Columns[I].MemberName = AName then
      begin
        Result := Columns[I];
        Break;
      end;
  end;

var
  I: Integer;
  AFieldName: string;
begin
  for I := Helper.ItemsCount - 1 downto 0 do
  begin
    AFieldName := Helper.ItemFieldName[I];
    if (GetColumn(AFieldName) = nil) and (AFieldName <> '') then
      Helper.DeleteItem(I);
  end;
end;

procedure TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.MoveSelected
  (ASource, ATarget: TcxCheckListBox);
var
  AItem: TcxCheckListBoxItem;
  I: Integer;
begin
  if GetSelectedCount(ASource) > 0 then
  begin
    ATarget.Items.BeginUpdate;
    try
      for I := 0 to ASource.Items.Count - 1 do
        if ASource.Selected[I] then
        begin
          AItem := ATarget.Items.Add;
          AItem.Text := ASource.Items[I].Text;
          AItem.Checked := True;
        end;
    finally
      ATarget.Items.EndUpdate;
    end;

    ASource.Items.BeginUpdate;
    try
      for I := ASource.Items.Count - 1 downto 0 do
      begin
        if ASource.Selected[I] then
          ASource.Items.Delete(I);
      end;
    finally
      ASource.Items.EndUpdate;
      if ASource.Count > 0 then
        ASource.Selected[0] := True
      else
        ASource.ItemIndex := -1;
    end;

    FGridViewChanged := True;
  end;
end;

procedure TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.
  PopulateDataSetFields;
var
  I: Integer;
  AFieldName: string;
begin
  lvDataSetFields.Items.Clear;
  for I := 0 to Columns.Count - 1 do
  begin
    if Columns[I].IsColumn and not Columns[I].IsAssociationList then
    begin
      AFieldName := Columns[I].MemberName;
      if Helper.GetItemIndexByFieldName(AFieldName) = -1 then
        lvDataSetFields.AddItem(AFieldName);
    end;
  end;
end;

procedure TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.
  PopulateGridViewFields;
var
  AItem: TcxCheckListBoxItem;
  I: Integer;
begin
  lvGridViewFields.Items.Clear;
  for I := 0 to Helper.ItemsCount - 1 do
    if Helper.ItemFieldName[I] <> '' then
    begin
      AItem := lvGridViewFields.Items.Add;
      AItem.Text := Helper.ItemFieldName[I];
      AItem.Checked := Helper.ItemVisible[I];
    end;
end;

procedure TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.
  ReorderDataSetFields;
var
  AFieldName: string;
  AStringList: TStringList;
  I, J: Integer;
begin
  AStringList := TStringList.Create;
  try
    for I := 0 to lvDataSetFields.Items.Count - 1 do
      AStringList.Add(lvDataSetFields.Items[I].Text);
    lvDataSetFields.Clear;
    for I := 0 to Columns.Count - 1 do
    begin
      AFieldName := Columns[I].MemberName;
      for J := 0 to AStringList.Count - 1 do
        if SameText(AStringList.Strings[J], AFieldName) then
        begin
          lvDataSetFields.Items.Add.Text := AFieldName;
          Break;
        end;
    end;
  finally
    AStringList.Free;
  end;
end;

procedure TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.SwapItems
  (AList: TcxCheckListBox; AIndex1, AIndex2: Integer);
var
  ATempChecked: Boolean;
  ATempObject: TObject;
  ATempSelected: Boolean;
  ATempText: TCaption;
begin
  ATempChecked := AList.Items[AIndex1].Checked;
  ATempObject := AList.Items[AIndex1].ItemObject;
  ATempSelected := AList.Selected[AIndex1];
  ATempText := AList.Items[AIndex1].Text;

  AList.Items[AIndex1].Text := AList.Items[AIndex2].Text;
  AList.Items[AIndex1].ItemObject := AList.Items[AIndex2].ItemObject;
  AList.Items[AIndex1].Checked := AList.Items[AIndex2].Checked;
  AList.Selected[AIndex1] := AList.Selected[AIndex2];

  AList.Items[AIndex2].Text := ATempText;
  AList.Items[AIndex2].ItemObject := ATempObject;
  AList.Items[AIndex2].Checked := ATempChecked;
  AList.Selected[AIndex2] := ATempSelected;

  FGridViewChanged := True;
end;

procedure TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.
  UpdateControlsState;
var
  ADataSetFieldsSelCount: Integer;
  AGridViewFieldsSelCount: Integer;
begin
  ADataSetFieldsSelCount := GetSelectedCount(lvDataSetFields);
  AGridViewFieldsSelCount := GetSelectedCount(lvGridViewFields);

  lciAddField.Enabled := ADataSetFieldsSelCount > 0;
  lciAddAllFields.Enabled := lvDataSetFields.Count > 0;
  lciDeleteField.Enabled := AGridViewFieldsSelCount > 0;
  lciDeleteAllFields.Enabled := lvGridViewFields.Count > 0;
  lciMoveFieldUp.Enabled := (AGridViewFieldsSelCount > 0) and
    not lvGridViewFields.Selected[0];
  lciMoveFieldDown.Enabled := (AGridViewFieldsSelCount > 0) and
    not lvGridViewFields.Selected[lvGridViewFields.Count - 1];

  pupmitDataSetAddSelected.Enabled := lciAddField.Enabled;
  pupmitDataSetAddAll.Enabled := lciAddAllFields.Enabled;
  pupmitDataSetDeselectAll.Enabled := ADataSetFieldsSelCount > 0;
  pupmitDataSetSelectAll.Enabled := ADataSetFieldsSelCount <
    lvDataSetFields.Count;

  pupmitGridViewDeleteSelected.Enabled := lciDeleteField.Enabled;
  pupmitGridViewDeleteAll.Enabled := lciDeleteAllFields.Enabled;
  pupmitGridViewCheckSelected.Enabled := AGridViewFieldsSelCount > 0;
  pupmitGridViewUncheckSelected.Enabled := AGridViewFieldsSelCount > 0;
  pupmitGridViewMoveUp.Enabled := lciMoveFieldUp.Enabled;
  pupmitGridViewMoveDown.Enabled := lciMoveFieldDown.Enabled;
  pupmitGridViewSelectAll.Enabled := AGridViewFieldsSelCount <
    lvGridViewFields.Count;
  pupmitGridViewDeselectAll.Enabled := AGridViewFieldsSelCount > 0;
end;

{ Events }

procedure TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.
  btnAddAllFieldsClick(Sender: TObject);
begin
  SelectAll(lvDataSetFields);
  MoveSelected(lvDataSetFields, lvGridViewFields);
  UpdateControlsState;
end;

procedure TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.btnAddFieldClick
  (Sender: TObject);
begin
  MoveSelected(lvDataSetFields, lvGridViewFields);
  UpdateControlsState;
end;

procedure TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.
  btnDeleteAllFieldsClick(Sender: TObject);
begin
  SelectAll(lvGridViewFields);
  MoveSelected(lvGridViewFields, lvDataSetFields);
  ReorderDataSetFields;
  UpdateControlsState;
end;

procedure TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.
  btnDeleteFieldClick(Sender: TObject);
begin
  MoveSelected(lvGridViewFields, lvDataSetFields);
  ReorderDataSetFields;
  UpdateControlsState;
end;

procedure TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.
  btnMoveFieldDownClick(Sender: TObject);
var
  I: Integer;
begin
  if (lvGridViewFields.Count > 0) and not lvGridViewFields.Selected
    [lvGridViewFields.Count - 1] then
  begin
    for I := lvGridViewFields.Items.Count - 2 downto 0 do
      if lvGridViewFields.Selected[I] then
        SwapItems(lvGridViewFields, I, I + 1);
  end;
  UpdateControlsState;
end;

procedure TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.
  btnMoveFieldUpClick(Sender: TObject);
var
  I: Integer;
begin
  if (lvGridViewFields.Count > 0) and not lvGridViewFields.Selected[0] then
  begin
    for I := 1 to lvGridViewFields.Items.Count - 1 do
      if lvGridViewFields.Selected[I] then
        SwapItems(lvGridViewFields, I, I - 1);
  end;
  UpdateControlsState;
end;

procedure TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.
  lvDataSetFieldsDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := (Source as TcxDragControlObject).Control = lvGridViewFields;
end;

procedure TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.
  lvDataSetFieldsEndDrag(Sender, Target: TObject; X, Y: Integer);
begin
  if Target = lvGridViewFields.InnerCheckListBox then
  begin
    MoveSelected(lvDataSetFields, lvGridViewFields);
    UpdateControlsState;
  end;
end;

procedure TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.
  lvGridViewFieldsClick(Sender: TObject);
begin
  UpdateControlsState;
  FGridViewChanged := True;
end;

procedure TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.
  lvGridViewFieldsDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  ADragObject: TcxDragControlObject;
begin
  ADragObject := Source as TcxDragControlObject;
  Accept := (ADragObject.Control = lvDataSetFields) or
    (ADragObject.Control = lvGridViewFields);
end;

procedure TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.
  lvGridViewFieldsEndDrag(Sender, Target: TObject; X, Y: Integer);
var
  ACount: Integer;
  ASelCount: Integer;
  ATargetIndex: Integer;
  I: Integer;
begin
  if Target = lvDataSetFields.InnerCheckListBox then
    MoveSelected(lvGridViewFields, lvDataSetFields)
  else if Target = lvGridViewFields.InnerCheckListBox then
  begin
    ATargetIndex := lvGridViewFields.ItemAtPos(Point(X, Y), True);
    if ATargetIndex >= 0 then
    begin
      lvGridViewFields.Items.BeginUpdate;
      try
        ACount := lvGridViewFields.Items.Count;
        ASelCount := GetSelectedCount(lvGridViewFields);
        for I := 0 to ACount - 1 do
        begin
          if lvGridViewFields.Selected[I] then
            lvGridViewFields.Items.Add.Assign(lvGridViewFields.Items[I]);
        end;

        for I := ACount - 1 downto 0 do
        begin
          if lvGridViewFields.Selected[I] then
            lvGridViewFields.Items.Delete(I);
        end;

        if Y > cxRectCenter(lvGridViewFields.ItemRect(ATargetIndex)).Y then
          Inc(ATargetIndex);

        for I := ATargetIndex to (ACount - ASelCount) - 1 do
          lvGridViewFields.Items.Add.Assign(lvGridViewFields.Items[I]);

        for I := (ACount - ASelCount) - 1 downto ATargetIndex do
          lvGridViewFields.Items.Delete(I);
      finally
        lvGridViewFields.Items.EndUpdate;
      end;

      ATargetIndex := Min(ATargetIndex, lvGridViewFields.Count - ASelCount);
      lvGridViewFields.ItemIndex := ATargetIndex;
      for I := 0 to ASelCount - 1 do
        lvGridViewFields.Selected[ATargetIndex + I] := True;

      FGridViewChanged := True;
    end;
  end;

  UpdateControlsState;
end;

procedure TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.
  pupmitDataSetDeselectAllClick(Sender: TObject);
begin
  SelectNone(lvDataSetFields);
  UpdateControlsState;
end;

procedure TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.
  pupmitDataSetSelectAllClick(Sender: TObject);
begin
  SelectAll(lvDataSetFields);
  UpdateControlsState;
end;

procedure TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.
  pupmitGridViewCheckSelectedClick(Sender: TObject);
begin
  ChangeCheckStateForSelected(lvGridViewFields, True);
  UpdateControlsState;
end;

procedure TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.
  pupmitGridViewDeselectAllClick(Sender: TObject);
begin
  SelectNone(lvGridViewFields);
  UpdateControlsState;
end;

procedure TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.
  pupmitGridViewSelectAllClick(Sender: TObject);
begin
  SelectAll(lvGridViewFields);
  UpdateControlsState;
end;

procedure TcxGridWizardEMFViewsSelectItemsForDisplayPageFrame.
  pupmitGridViewUncheckSelectedClick(Sender: TObject);
begin
  ChangeCheckStateForSelected(lvGridViewFields, False);
  UpdateControlsState;
end;

end.
