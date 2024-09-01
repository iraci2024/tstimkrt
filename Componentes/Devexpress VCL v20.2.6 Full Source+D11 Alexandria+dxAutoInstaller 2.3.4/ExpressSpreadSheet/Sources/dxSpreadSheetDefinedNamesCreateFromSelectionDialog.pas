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

unit dxSpreadSheetDefinedNamesCreateFromSelectionDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, StdCtrls, TypInfo,
  Generics.Defaults, Generics.Collections,
  dxCore, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  cxClasses, cxContainer, cxEdit, cxCheckListBox,
  cxCheckBox, cxButtons, cxTextEdit, dxLayoutControlAdapters,
  dxLayoutcxEditAdapters, dxLayoutContainer, dxLayoutControl,
  dxLayoutLookAndFeels, dxSpreadSheetCore, dxSpreadSheetProtection, cxLabel,
  dxForms, dxCoreClasses;

type

  { TdxSpreadSheetCreateDefinedNamesFromSelectionDialogForm }

  TdxSpreadSheetCreateDefinedNamesFromSelectionDialogFormClass = class of
    TdxSpreadSheetCreateDefinedNamesFromSelectionDialogForm;

  TdxSpreadSheetCreateDefinedNamesFromSelectionDialogForm = class(TdxForm)
    btnCancel: TcxButton;
    btnOK: TcxButton;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList;
    lbHeader: TcxLabel;
    lciBottom: TdxLayoutCheckBoxItem;
    lciLeft: TdxLayoutCheckBoxItem;
    lciRight: TdxLayoutCheckBoxItem;
    lciTop: TdxLayoutCheckBoxItem;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;

  strict private
    FSelection: TdxRectList;
    FSheet: TdxSpreadSheetTableView;

    function GetHeaders: TcxBorders;
    function GetSpreadSheet: TdxCustomSpreadSheet;
    procedure SetHeaders(const Value: TcxBorders);
  protected
    procedure Apply; virtual;
    procedure ApplyLocalizations; virtual;
    procedure Initialize(ASheet: TdxSpreadSheetTableView); virtual;
    //
    property Headers: TcxBorders read GetHeaders write SetHeaders;
    property Selection: TdxRectList read FSelection;
    property Sheet: TdxSpreadSheetTableView read FSheet;
    property SpreadSheet: TdxCustomSpreadSheet read GetSpreadSheet;
  public
    destructor Destroy; override;
  end;

var
  dxSpreadSheetCreateDefinedNamesFromSelectionDialogFormClass
    : TdxSpreadSheetCreateDefinedNamesFromSelectionDialogFormClass =
    TdxSpreadSheetCreateDefinedNamesFromSelectionDialogForm;

procedure ShowCreateDefinedNamesFromSelectionDialog
  (ASheet: TdxSpreadSheetTableView);

implementation

uses
  dxSpreadSheetDialogStrs, dxSpreadSheetPasswordDialog,
  dxSpreadSheetCoreHelpers;

{$R *.dfm}

procedure ShowCreateDefinedNamesFromSelectionDialog
  (ASheet: TdxSpreadSheetTableView);
var
  ADialog: TdxSpreadSheetCreateDefinedNamesFromSelectionDialogForm;
begin
  ADialog := dxSpreadSheetCreateDefinedNamesFromSelectionDialogFormClass.Create
    (GetParentForm(ASheet.SpreadSheet));
  try
    ADialog.Initialize(ASheet);
    if ADialog.ShowModal = mrOk then
      ADialog.Apply;
  finally
    ADialog.Free;
  end;
end;

{ TdxSpreadSheetProtectWorkbookDialogForm }

destructor TdxSpreadSheetCreateDefinedNamesFromSelectionDialogForm.Destroy;
begin
  FreeAndNil(FSelection);
  inherited;
end;

procedure TdxSpreadSheetCreateDefinedNamesFromSelectionDialogForm.Apply;
begin
  SpreadSheet.DefinedNames.AddFromSelection(Sheet, Selection, Headers);
end;

procedure TdxSpreadSheetCreateDefinedNamesFromSelectionDialogForm.
  ApplyLocalizations;
begin
  Caption := cxGetResourceString
    (@sdxCreateDefinedNameFromSelectionDialogCaption);
  lbHeader.Caption := cxGetResourceString
    (@sdxCreateDefinedNameFromSelectionDialogHeader);
  lciLeft.Caption := cxGetResourceString
    (@sdxCreateDefinedNameFromSelectionDialogLeftColumn);
  lciTop.Caption := cxGetResourceString
    (@sdxCreateDefinedNameFromSelectionDialogTopRow);
  lciRight.Caption := cxGetResourceString
    (@sdxCreateDefinedNameFromSelectionDialogRightColumn);
  lciBottom.Caption := cxGetResourceString
    (@sdxCreateDefinedNameFromSelectionDialogBottomRow);
  btnOK.Caption := cxGetResourceString
    (@sdxCreateDefinedNameFromSelectionDialogButtonOK);
  btnCancel.Caption := cxGetResourceString
    (@sdxCreateDefinedNameFromSelectionDialogButtonCancel);
end;

procedure TdxSpreadSheetCreateDefinedNamesFromSelectionDialogForm.Initialize
  (ASheet: TdxSpreadSheetTableView);
begin
  FSheet := ASheet;
  FSelection := Sheet.Selection.ToRectList;
  SetControlLookAndFeel(Self, SpreadSheet.DialogsLookAndFeel);
  Headers := TdxSpreadSheetDefinedNameHelper.GetHeaders(Sheet, Selection);
  ApplyLocalizations;
end;

function TdxSpreadSheetCreateDefinedNamesFromSelectionDialogForm.GetHeaders
  : TcxBorders;
begin
  Result := [];
  if lciLeft.Checked then
    Include(Result, bLeft);
  if lciTop.Checked then
    Include(Result, bTop);
  if lciRight.Checked then
    Include(Result, bRight);
  if lciBottom.Checked then
    Include(Result, bBottom);
end;

function TdxSpreadSheetCreateDefinedNamesFromSelectionDialogForm.GetSpreadSheet
  : TdxCustomSpreadSheet;
begin
  Result := Sheet.SpreadSheet;
end;

procedure TdxSpreadSheetCreateDefinedNamesFromSelectionDialogForm.SetHeaders
  (const Value: TcxBorders);
begin
  lciBottom.Checked := bBottom in Value;
  lciLeft.Checked := bLeft in Value;
  lciRight.Checked := bRight in Value;
  lciTop.Checked := bTop in Value;
end;

end.
