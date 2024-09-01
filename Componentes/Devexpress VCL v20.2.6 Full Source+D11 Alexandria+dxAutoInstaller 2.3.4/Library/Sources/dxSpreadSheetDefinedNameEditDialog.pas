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

unit dxSpreadSheetDefinedNameEditDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Forms, Dialogs, Menus, StdCtrls,
  dxCore, cxGraphics, cxControls,
  cxLookAndFeels, cxLookAndFeelPainters, dxLayoutLookAndFeels, cxClasses,
  dxLayoutContainer, dxLayoutControl,
  dxLayoutControlAdapters, cxButtons, dxLayoutcxEditAdapters, cxContainer,
  cxEdit, cxLabel, cxRadioGroup,
  dxSpreadSheetTypes, dxSpreadSheetCore, dxForms,
  dxSpreadSheetReferenceEditDialog, cxEditRepositoryItems,
  cxTextEdit, cxMaskEdit, cxButtonEdit, cxMemo, cxDropDownEdit;

type

  { TdxSpreadSheetDefinedNameEditDialogForm }

  TdxSpreadSheetDefinedNameEditDialogFormClass = class of
    TdxSpreadSheetDefinedNameEditDialogForm;

  TdxSpreadSheetDefinedNameEditDialogForm = class
    (TdxSpreadSheetReferenceEditDialogForm)
    beReference: TcxButtonEdit;
    btnCancel: TcxButton;
    btnOK: TcxButton;
    cbScope: TcxComboBox;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    liBtnCancel: TdxLayoutItem;
    liBtnOK: TdxLayoutItem;
    liComment: TdxLayoutItem;
    liName: TdxLayoutItem;
    liReference: TdxLayoutItem;
    liScope: TdxLayoutItem;
    meComment: TcxMemo;
    teName: TcxTextEdit;

    procedure beReferenceEnter(Sender: TObject);
    procedure beReferenceExit(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
  protected
    FDefinedName: TdxSpreadSheetDefinedName;

    procedure ApplyChanges; virtual;
    procedure ApplyLocalizations; override;
    procedure InitializeCore; override;
    procedure InitializeDefaultsFromSelection;
    procedure PopulateScopes; virtual;
    function ValidateReference(const S: string): Boolean; override;
  public
    procedure Initialize(ASpreadSheet: TdxCustomSpreadSheet;
      ADefinedName: TdxSpreadSheetDefinedName = nil); reintroduce; virtual;
  end;

var
  SpreadSheetDefinedNameEditDialogFormClass
    : TdxSpreadSheetDefinedNameEditDialogFormClass =
    TdxSpreadSheetDefinedNameEditDialogForm;

procedure HideDefinedNameEditDialog;
procedure ShowDefinedNameEditDialog(ASpreadSheet: TdxCustomSpreadSheet;
  ADefinedName: TdxSpreadSheetDefinedName = nil);

implementation

uses
  Math,
  dxSpreadSheetDialogStrs,
  dxSpreadSheetCoreHistory,
  dxSpreadSheetCoreReferences,
  dxSpreadSheetCoreHelpers,
  dxSpreadSheetUtils;

{$R *.dfm}

var
  FDialog: TdxSpreadSheetDefinedNameEditDialogForm = nil;

function IsDefinedNameEditDialogVisible: Boolean;
begin
  Result := FDialog <> nil;
end;

procedure HideDefinedNameEditDialog;
begin
  FreeAndNil(FDialog);
end;

procedure ShowDefinedNameEditDialog(ASpreadSheet: TdxCustomSpreadSheet;
  ADefinedName: TdxSpreadSheetDefinedName = nil);
begin
  if FDialog = nil then
  begin
    FDialog := SpreadSheetDefinedNameEditDialogFormClass.Create(ASpreadSheet);
    FDialog.Initialize(ASpreadSheet, ADefinedName);
  end;
  FDialog.Show;
end;

{ TdxSpreadSheetDefinedNameEditDialogForm }

procedure TdxSpreadSheetDefinedNameEditDialogForm.ApplyChanges;
var
  ADefinedName: TdxSpreadSheetDefinedName;
begin
  if Sheet.OptionsProtection.&Protected then
    Exit;

  beReference.ValidateEdit;
  if FDefinedName <> nil then
  begin
    SpreadSheet.History.BeginAction
      (TdxSpreadSheetHistoryChangeDefinedNameAction);
    try
      FDefinedName.Caption := teName.EditingValue;
      FDefinedName.Comment := meComment.EditingValue;
      FDefinedName.Reference := beReference.EditingValue;
    finally
      SpreadSheet.History.EndAction;
    end;
  end
  else
  begin
    SpreadSheet.History.BeginAction
      (TdxSpreadSheetHistoryCreateDefinedNameAction);
    try
      ADefinedName := SpreadSheet.DefinedNames.Add(teName.EditingValue,
        beReference.EditingValue,
        cbScope.ItemObject as TdxSpreadSheetCustomView);
      ADefinedName.Comment := meComment.EditingValue;
    finally
      SpreadSheet.History.EndAction;
    end;
  end;
end;

procedure TdxSpreadSheetDefinedNameEditDialogForm.ApplyLocalizations;
begin
  inherited;

  if FDefinedName <> nil then
    Caption := cxGetResourceString(@sdxDefinedNameEditorCaptionEditName)
  else
    Caption := cxGetResourceString(@sdxDefinedNameEditorCaptionNewName);

  liName.Caption := cxGetResourceString(@sdxDefinedNameEditorName);
  liScope.Caption := cxGetResourceString(@sdxDefinedNameEditorScope);
  liComment.Caption := cxGetResourceString(@sdxDefinedNameEditorComment);
  liReference.Caption := cxGetResourceString(@sdxDefinedNameEditorRefersTo);

  btnCancel.Caption := cxGetResourceString(@sdxDefinedNameEditorButtonCancel);
  btnOK.Caption := cxGetResourceString(@sdxDefinedNameEditorButtonOK);
end;

procedure TdxSpreadSheetDefinedNameEditDialogForm.Initialize
  (ASpreadSheet: TdxCustomSpreadSheet; ADefinedName: TdxSpreadSheetDefinedName);
begin
  FDefinedName := ADefinedName;
  inherited Initialize(ASpreadSheet.ActiveSheetAsTable);
end;

procedure TdxSpreadSheetDefinedNameEditDialogForm.InitializeCore;
begin
  PopulateScopes;

  cbScope.Enabled := FDefinedName = nil;
  btnOK.Enabled := not Sheet.OptionsProtection.Protected;

  if FDefinedName <> nil then
  begin
    cbScope.ItemIndex :=
      Max(0, cbScope.Properties.Items.IndexOfObject(FDefinedName.Scope));
    beReference.EditValue := FDefinedName.Reference;
    meComment.EditValue := FDefinedName.Comment;
    teName.EditValue := FDefinedName.Caption;
  end
  else
  begin
    meComment.EditValue := '';
    beReference.EditValue := '';
    teName.EditValue := '';
    cbScope.ItemIndex := 0;
    if Sheet.Selection.Count > 0 then
      InitializeDefaultsFromSelection;
  end;
end;

procedure TdxSpreadSheetDefinedNameEditDialogForm.
  InitializeDefaultsFromSelection;

  function SafeGetCellText(const ARow, AColumn: Integer): string;
  var
    ACell: TdxSpreadSheetCell;
  begin
    ACell := Sheet.Cells[ARow, AColumn];
    if ACell <> nil then
      Result := TdxSpreadSheetDefinedNameHelper.ReplaceIllegalChars
        (ACell.AsString, SpreadSheet)
    else
      Result := '';
  end;

var
  AHeaders: TcxBorders;
  ASelection: TRect;
begin
  ASelection := Sheet.Selection.Area;
  if dxSpreadSheetIsValidArea(ASelection) then
  begin
    beReference.EditValue := AreaToString(ASelection, Sheet);

    AHeaders := TdxSpreadSheetDefinedNameHelper.GetHeaders(Sheet, ASelection);
    if [bRight, bBottom] * AHeaders <> [] then
      teName.Text := SafeGetCellText(ASelection.Bottom, ASelection.Right);
    if [bLeft, bTop] * AHeaders <> [] then
      teName.Text := SafeGetCellText(ASelection.Top, ASelection.Left);
  end;
end;

procedure TdxSpreadSheetDefinedNameEditDialogForm.PopulateScopes;
var
  I: Integer;
begin
  cbScope.Properties.Items.BeginUpdate;
  try
    cbScope.Properties.Items.Clear;
    cbScope.Properties.Items.AddObject
      (cxGetResourceString(@sdxDefinedNameManagerDialogWorkbook), nil);
    for I := 0 to SpreadSheet.SheetCount - 1 do
      cbScope.Properties.Items.AddObject(SpreadSheet.Sheets[I].Caption,
        SpreadSheet.Sheets[I]);
  finally
    cbScope.Properties.Items.EndUpdate;
  end;
end;

function TdxSpreadSheetDefinedNameEditDialogForm.ValidateReference
  (const S: string): Boolean;
begin
  Result := dxSpreadSheetIsFormula(S) or inherited ValidateReference(S);
end;

procedure TdxSpreadSheetDefinedNameEditDialogForm.beReferenceEnter
  (Sender: TObject);
begin
  StartEditing(beReference, smCells);
end;

procedure TdxSpreadSheetDefinedNameEditDialogForm.beReferenceExit
  (Sender: TObject);
begin
  FinishEditing(beReference);
end;

procedure TdxSpreadSheetDefinedNameEditDialogForm.btnCancelClick
  (Sender: TObject);
begin
  Close;
end;

procedure TdxSpreadSheetDefinedNameEditDialogForm.btnOKClick(Sender: TObject);
begin
  ApplyChanges;
  Close;
end;

procedure TdxSpreadSheetDefinedNameEditDialogForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TdxSpreadSheetDefinedNameEditDialogForm.FormDestroy(Sender: TObject);
begin
  FDialog := nil;
end;

end.
