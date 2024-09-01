{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressSpreadSheet }
{ }
{ Copyright (c) 2001-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL }
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

unit dxSpreadSheetDefinedNameManagerDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
{$IFDEF DELPHI101BERLIN}
  System.ImageList,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, ImgList,
  ActnList, ComCtrls, dxCore, cxGraphics, cxControls, cxImageList,
  cxLookAndFeels, cxLookAndFeelPainters,
  dxLayoutLookAndFeels, cxClasses, dxLayoutContainer, dxLayoutControl,
  dxLayoutControlAdapters, cxButtons, cxEdit,
  dxForms, cxLabel, dxLayoutcxEditAdapters, cxContainer, cxRadioGroup,
  dxSpreadSheetTypes, dxSpreadSheetCore, cxMemo,
  dxSpreadSheetPrinting, cxTextEdit, cxEditRepositoryItems, cxListView,
  cxMaskEdit, cxButtonEdit,
  dxSpreadSheetReferenceEditDialog, dxSpreadSheetDefinedNameEditDialog,
  dxSpreadSheetCoreFormulasTokens;

type

  { TdxSpreadSheetDefinedNameManagerDialogForm }

  TdxSpreadSheetDefinedNameManagerDialogFormClass = class of
    TdxSpreadSheetDefinedNameManagerDialogForm;

  TdxSpreadSheetDefinedNameManagerDialogForm = class
    (TdxSpreadSheetReferenceEditDialogForm, IdxSpreadSheetListener2)
    acCreate: TAction;
    acDelete: TAction;
    acEdit: TAction;
    Actions: TActionList;
    beReference: TcxButtonEdit;
    btnApplyReference: TcxButton;
    btnClose: TcxButton;
    btnCreate: TcxButton;
    btnDelete: TcxButton;
    btnEdit: TcxButton;
    btnRejectReference: TcxButton;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutAutoCreatedGroup2: TdxLayoutAutoCreatedGroup;
    ilImages: TcxImageList;
    lbReference: TdxLayoutLabeledItem;
    liBtnApplyReference: TdxLayoutItem;
    liBtnClose: TdxLayoutItem;
    liBtnCreate: TdxLayoutItem;
    liBtnEdit: TdxLayoutItem;
    liBtnRejectReference: TdxLayoutItem;
    liBtnRemove: TdxLayoutItem;
    liDefinedNames: TdxLayoutItem;
    liReference: TdxLayoutItem;
    lvDefinedNames: TcxListView;

    procedure acCreateExecute(Sender: TObject);
    procedure acDeleteExecute(Sender: TObject);
    procedure acEditExecute(Sender: TObject);
    procedure beReferenceEnter(Sender: TObject);
    procedure beReferenceExit(Sender: TObject);
    procedure beReferencePropertiesChange(Sender: TObject);
    procedure btnApplyReferenceClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnRejectReferenceClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure lvDefinedNamesChanging(Sender: TObject; Item: TListItem;
      Change: TItemChange; var AllowChange: Boolean);
    procedure lvDefinedNamesColumnClick(Sender: TObject; Column: TListColumn);
    procedure lvDefinedNamesCompare(Sender: TObject; Item1, Item2: TListItem;
      Data: Integer; var Compare: Integer);
    procedure lvDefinedNamesDblClick(Sender: TObject);
    procedure lvDefinedNamesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lvDefinedNamesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
  strict private
    FEditDialog: TdxSpreadSheetDefinedNameEditDialogForm;
    FIsProtected: Boolean;
    FReferenceModified: Boolean;
    FSortColumnIndex: Integer;
    FSortOrder: TdxSortOrder;

    function GetScopeName(ADefinedName: TdxSpreadSheetDefinedName): string;
    function GetSelectedDefinedName: TdxSpreadSheetDefinedName;
    function GetSelection: TList;
    function NeedReloadDefinedNames: Boolean;
    procedure SetReferenceModified(AValue: Boolean);
    //
    procedure HandlerEditDialogDestroyed(Sender: TObject);
    procedure HandlerEditDialogShown(Sender: TObject);
  protected
    procedure ApplyLocalizations; override;
    procedure CheckReferenceChangesAreSaved;
    procedure Edit(ADefinedName: TdxSpreadSheetDefinedName = nil); virtual;
    procedure Finalize; override;
    procedure InitializeCore; override;
    function ShowConfirmation(const AMessage: Pointer): Integer; overload;
    function ShowConfirmation(const AMessage: string): Integer; overload;
    function ShowDeleteConfirmation(ASelected: TList): Integer;
    function ValidateReference(const S: string): Boolean; override;
    procedure UpdateControlState; virtual;
    procedure UpdateReference; virtual;

    procedure AddDefinedName(ADefinedName: TdxSpreadSheetDefinedName); virtual;
    procedure PopulateDefinedNames(ADefinedNames
      : TdxSpreadSheetDefinedNames); virtual;
    procedure UpdateDefinedNameDisplayInfo; overload; virtual;
    procedure UpdateDefinedNameDisplayInfo(AItem: TListItem); overload; virtual;

    // IdxSpreadSheetListener
    procedure DataChanged(Sender: TdxCustomSpreadSheet); override;
    // IdxSpreadSheetListener2
    procedure ActiveSheetChanged(Sender: TdxCustomSpreadSheet);
    procedure DefinedNamesChanged(Sender: TdxCustomSpreadSheet);

    property ReferenceModified: Boolean read FReferenceModified
      write SetReferenceModified;
    property SelectedDefinedName: TdxSpreadSheetDefinedName
      read GetSelectedDefinedName;
  end;

var
  SpreadSheetDefinedNameManagerDialogFormClass
    : TdxSpreadSheetDefinedNameManagerDialogFormClass =
    TdxSpreadSheetDefinedNameManagerDialogForm;

procedure HideDefinedNameManagerDialog;
procedure ShowDefinedNameManagerDialog(ASpreadSheet: TdxCustomSpreadSheet);

implementation

uses
  dxSpreadSheetDialogStrs, dxSpreadSheetUtils;

{$R *.dfm}

var
  FDialog: TdxSpreadSheetDefinedNameManagerDialogForm;

function IsDefinedNameManagerDialogVisible: Boolean;
begin
  Result := FDialog <> nil;
end;

procedure HideDefinedNameManagerDialog;
begin
  FreeAndNil(FDialog);
end;

procedure ShowDefinedNameManagerDialog(ASpreadSheet: TdxCustomSpreadSheet);
begin
  if FDialog = nil then
  begin
    FDialog := SpreadSheetDefinedNameManagerDialogFormClass.Create
      (GetParentForm(ASpreadSheet));
    FDialog.Initialize(ASpreadSheet.ActiveSheetAsTable);
  end;
  FDialog.Show;
end;

{ TdxSpreadSheetDefinedNameManagerDialogForm }

procedure TdxSpreadSheetDefinedNameManagerDialogForm.ApplyLocalizations;
var
  I: Integer;
begin
  inherited;

  Caption := cxGetResourceString(@sdxDefinedNameManagerDialogCaption);

  acCreate.Caption := cxGetResourceString
    (@sdxDefinedNameManagerDialogButtonNew);
  acDelete.Caption := cxGetResourceString
    (@sdxDefinedNameManagerDialogButtonDelete);
  acEdit.Caption := cxGetResourceString(@sdxDefinedNameManagerDialogButtonEdit);
  btnClose.Caption := cxGetResourceString
    (@sdxDefinedNameManagerDialogButtonClose);

  lvDefinedNames.Column[0].Caption :=
    cxGetResourceString(@sdxDefinedNameManagerDialogColumnName);
  lvDefinedNames.Column[1].Caption :=
    cxGetResourceString(@sdxDefinedNameManagerDialogColumnValue);
  lvDefinedNames.Column[2].Caption :=
    cxGetResourceString(@sdxDefinedNameManagerDialogColumnReference);
  lvDefinedNames.Column[3].Caption :=
    cxGetResourceString(@sdxDefinedNameManagerDialogColumnScope);
  lvDefinedNames.Column[4].Caption :=
    cxGetResourceString(@sdxDefinedNameManagerDialogColumnComment);

  lbReference.Caption := cxGetResourceString
    (@sdxDefinedNameManagerDialogRefersTo);

  for I := 0 to lvDefinedNames.Items.Count - 1 do
    UpdateDefinedNameDisplayInfo(lvDefinedNames.Items[I]);
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.
  CheckReferenceChangesAreSaved;
begin
  if ReferenceModified then
    case ShowConfirmation(@sdxDefinedNameManagerDialogSaveConfirmation) of
      mrNo:
        btnRejectReference.Click;
      mrYes:
        btnApplyReference.Click;
    end;
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.Edit
  (ADefinedName: TdxSpreadSheetDefinedName = nil);
begin
  if FEditDialog = nil then
  begin
    FEditDialog := SpreadSheetDefinedNameEditDialogFormClass.Create(Self);
    FEditDialog.OnDestroy := HandlerEditDialogDestroyed;
    FEditDialog.OnShow := HandlerEditDialogShown;
    FEditDialog.Initialize(SpreadSheet, ADefinedName);
    FEditDialog.Show;
  end;
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.Finalize;
begin
  FreeAndNil(FEditDialog);
  inherited;
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.InitializeCore;
begin
  inherited;
  FSortColumnIndex := 0;
  FSortOrder := soAscending;
  FIsProtected := Sheet.OptionsProtection.&Protected;
  PopulateDefinedNames(SpreadSheet.DefinedNames);
  UpdateControlState;
end;

function TdxSpreadSheetDefinedNameManagerDialogForm.ShowConfirmation
  (const AMessage: Pointer): Integer;
begin
  Result := ShowConfirmation(cxGetResourceString(AMessage));
end;

function TdxSpreadSheetDefinedNameManagerDialogForm.ShowConfirmation
  (const AMessage: string): Integer;
begin
  Result := MessageDlg(AMessage, mtConfirmation, [mbYes, mbNo], 0);
end;

function TdxSpreadSheetDefinedNameManagerDialogForm.ShowDeleteConfirmation
  (ASelected: TList): Integer;
var
  AMessage: string;
begin
  if ASelected.Count = 1 then
  begin
    AMessage :=
      Format(cxGetResourceString
      (@sdxDefinedNameManagerDialogDeleteSelectedNameConfirmation),
      [TdxSpreadSheetDefinedName(ASelected[0]).Caption]);
  end
  else
    AMessage := cxGetResourceString
      (@sdxDefinedNameManagerDialogDeleteSelectedNamesConfirmation);

  Result := ShowConfirmation(AMessage);
end;

function TdxSpreadSheetDefinedNameManagerDialogForm.ValidateReference
  (const S: string): Boolean;
begin
  Result := dxSpreadSheetIsFormula(S) or inherited ValidateReference(S);
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.UpdateControlState;
var
  AName: TdxSpreadSheetDefinedName;
begin
  AName := SelectedDefinedName;
  acCreate.Enabled := not FIsProtected;
  acDelete.Enabled := not FIsProtected and (lvDefinedNames.SelCount > 0);
  acEdit.Enabled := not FIsProtected and (AName <> nil);

  btnApplyReference.Enabled := not FIsProtected and ReferenceModified and
    (AName <> nil);
  btnRejectReference.Enabled := not FIsProtected and ReferenceModified and
    (AName <> nil);
  beReference.Enabled := not FIsProtected and (AName <> nil);
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.UpdateReference;
begin
  if SelectedDefinedName <> nil then
    beReference.Text := SelectedDefinedName.Reference
  else
    beReference.Text := '';

  ReferenceModified := False;
  UpdateControlState;
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.AddDefinedName
  (ADefinedName: TdxSpreadSheetDefinedName);
var
  AItem: TListItem;
begin
  AItem := lvDefinedNames.Items.Add;
  AItem.Data := ADefinedName;
  UpdateDefinedNameDisplayInfo(AItem);
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.PopulateDefinedNames
  (ADefinedNames: TdxSpreadSheetDefinedNames);
var
  ASelected: TdxSpreadSheetDefinedName;
  I: Integer;
begin
  lvDefinedNames.Items.BeginUpdate;
  try
    ASelected := SelectedDefinedName;
    try
      lvDefinedNames.Items.Clear;
      for I := 0 to ADefinedNames.Count - 1 do
        AddDefinedName(ADefinedNames[I]);
    finally
      lvDefinedNames.Selected := lvDefinedNames.FindData(0, ASelected,
        True, False);
      lvDefinedNamesSelectItem(nil, lvDefinedNames.Selected,
        lvDefinedNames.Selected <> nil);
    end;
  finally
    lvDefinedNames.Items.EndUpdate;
  end;
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.
  UpdateDefinedNameDisplayInfo;
var
  I: Integer;
begin
  lvDefinedNames.Items.BeginUpdate;
  try
    for I := 0 to lvDefinedNames.Items.Count - 1 do
      UpdateDefinedNameDisplayInfo(lvDefinedNames.Items[I]);
  finally
    lvDefinedNames.Items.EndUpdate;
  end;
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.
  UpdateDefinedNameDisplayInfo(AItem: TListItem);
var
  ADefinedName: TdxSpreadSheetDefinedName;
begin
  lvDefinedNames.Items.BeginUpdate;
  try
    ADefinedName := TdxSpreadSheetDefinedName(AItem.Data);

    AItem.Caption := ADefinedName.Caption;
    AItem.SubItems.Clear;
    AItem.SubItems.Add(ADefinedName.ValueAsText);
    AItem.SubItems.Add(ADefinedName.Reference);
    AItem.SubItems.Add(GetScopeName(ADefinedName));
    AItem.SubItems.Add(ADefinedName.Comment);
  finally
    lvDefinedNames.Items.EndUpdate;
  end;
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.DataChanged
  (Sender: TdxCustomSpreadSheet);
begin
  inherited;
  if not NeedReloadDefinedNames then
    UpdateDefinedNameDisplayInfo;
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.ActiveSheetChanged
  (Sender: TdxCustomSpreadSheet);
begin
  // do nothing
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.DefinedNamesChanged
  (Sender: TdxCustomSpreadSheet);
begin
  if NeedReloadDefinedNames then
  begin
    btnRejectReference.Click;
    PopulateDefinedNames(SpreadSheet.DefinedNames);
  end
  else
  begin
    UpdateDefinedNameDisplayInfo;
    if not ReferenceModified then
      UpdateReference;
  end;
end;

function TdxSpreadSheetDefinedNameManagerDialogForm.GetScopeName
  (ADefinedName: TdxSpreadSheetDefinedName): string;
begin
  if ADefinedName.Scope <> nil then
    Result := ADefinedName.Scope.Caption
  else
    Result := cxGetResourceString(@sdxDefinedNameManagerDialogWorkbook);
end;

function TdxSpreadSheetDefinedNameManagerDialogForm.GetSelectedDefinedName
  : TdxSpreadSheetDefinedName;
var
  AItem: TListItem;
begin
  Result := nil;
  if lvDefinedNames.SelCount = 1 then
  begin
    AItem := lvDefinedNames.Selected;
    if AItem <> nil then
      Result := TdxSpreadSheetDefinedName(AItem.Data);
  end;
end;

function TdxSpreadSheetDefinedNameManagerDialogForm.
  NeedReloadDefinedNames: Boolean;
var
  I: Integer;
begin
  if lvDefinedNames.Items.Count <> SpreadSheet.DefinedNames.Count then
    Exit(True);
  for I := 0 to SpreadSheet.DefinedNames.Count - 1 do
  begin
    if lvDefinedNames.FindData(0, SpreadSheet.DefinedNames[I], True, False) = nil
    then
      Exit(True);
  end;
  Result := False;
end;

function TdxSpreadSheetDefinedNameManagerDialogForm.GetSelection: TList;
var
  AItem: TListItem;
  I: Integer;
begin
  Result := TList.Create;
  Result.Capacity := lvDefinedNames.SelCount;
  for I := 0 to lvDefinedNames.Items.Count - 1 do
  begin
    AItem := lvDefinedNames.Items[I];
    if AItem.Selected then
      Result.Add(AItem.Data);
  end;
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.SetReferenceModified
  (AValue: Boolean);
begin
  if FReferenceModified <> AValue then
  begin
    FReferenceModified := AValue;
    UpdateControlState;
  end;
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.HandlerEditDialogDestroyed
  (Sender: TObject);
begin
  if not(csDestroying in ComponentState) then
    Show;
  FEditDialog := nil;
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.HandlerEditDialogShown
  (Sender: TObject);
begin
  Hide;
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  CheckReferenceChangesAreSaved;
  Action := caFree;
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.FormDestroy
  (Sender: TObject);
begin
  Finalize;
  FDialog := nil;
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.acCreateExecute
  (Sender: TObject);
begin
  CheckReferenceChangesAreSaved;
  Edit;
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.acDeleteExecute
  (Sender: TObject);
var
  ASelection: TList;
  I: Integer;
begin
  ASelection := GetSelection;
  try
    if (ASelection.Count > 0) and (ShowDeleteConfirmation(ASelection) = mrYes)
    then
    begin
      SpreadSheet.BeginUpdate;
      try
        SpreadSheet.History.BeginAction
          (TdxSpreadSheetHistoryDeleteDefinedNameAction);
        try
          for I := 0 to ASelection.Count - 1 do
            TObject(ASelection.List[I]).Free;
        finally
          SpreadSheet.History.EndAction;
        end;
      finally
        SpreadSheet.EndUpdate;
      end;
    end;
  finally
    ASelection.Free;
  end;
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.acEditExecute
  (Sender: TObject);
begin
  CheckReferenceChangesAreSaved;
  if SelectedDefinedName <> nil then
    Edit(SelectedDefinedName);
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.beReferenceEnter
  (Sender: TObject);
begin
  StartEditing(beReference, smCells);
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.beReferenceExit
  (Sender: TObject);
begin
  FinishEditing(beReference);
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.beReferencePropertiesChange
  (Sender: TObject);
begin
  ReferenceModified := True;
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.btnApplyReferenceClick
  (Sender: TObject);
begin
  if SelectedDefinedName <> nil then
  begin
    ReferenceModified := False;
    SelectedDefinedName.Reference := beReference.Text;
  end;
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.btnCloseClick
  (Sender: TObject);
begin
  Close;
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.btnRejectReferenceClick
  (Sender: TObject);
begin
  UpdateReference;
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.lvDefinedNamesChanging
  (Sender: TObject; Item: TListItem; Change: TItemChange;
  var AllowChange: Boolean);
begin
  CheckReferenceChangesAreSaved;
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.lvDefinedNamesColumnClick
  (Sender: TObject; Column: TListColumn);
begin
  if FSortColumnIndex <> Column.Index then
  begin
    FSortColumnIndex := Column.Index;
    FSortOrder := soAscending;
  end
  else if FSortOrder = soAscending then
    FSortOrder := soDescending
  else
    FSortOrder := soAscending;

  lvDefinedNames.AlphaSort;
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.lvDefinedNamesCompare
  (Sender: TObject; Item1, Item2: TListItem; Data: Integer;
  var Compare: Integer);
begin
  if FSortColumnIndex = 0 then
    Compare := CompareStr(Item1.Caption, Item2.Caption)
  else
    Compare := CompareStr(Item1.SubItems[FSortColumnIndex - 1],
      Item2.SubItems[FSortColumnIndex - 1]);

  if FSortOrder = soDescending then
    Compare := -Compare;
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.lvDefinedNamesDblClick
  (Sender: TObject);
begin
  acEdit.Execute;
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.lvDefinedNamesKeyDown
  (Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then
    acDelete.Execute;
  if Key = VK_RETURN then
    acEdit.Execute;
end;

procedure TdxSpreadSheetDefinedNameManagerDialogForm.lvDefinedNamesSelectItem
  (Sender: TObject; Item: TListItem; Selected: Boolean);
begin
  UpdateControlState;
  UpdateReference;
end;

end.
