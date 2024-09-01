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

unit dxSpreadSheetInsertFunctionDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Menus, ActnList,
  dxCore, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  dxLayoutControlAdapters, dxLayoutLookAndFeels,
  dxLayoutContainer, cxEdit, cxClasses, cxButtons, dxLayoutControl, cxContainer,
  cxListBox, dxSpreadSheetCore, dxForms,
  dxLayoutcxEditAdapters, cxTextEdit, cxMaskEdit, cxDropDownEdit,
  dxSpreadSheetFunctions, cxCustomListBox, cxButtonEdit;

type

  { TdxSpreadSheetInsertFunctionDialogForm }

  TdxSpreadSheetInsertFunctionDialogFormClass = class of
    TdxSpreadSheetInsertFunctionDialogForm;

  TdxSpreadSheetInsertFunctionDialogForm = class(TdxForm)
    acFind: TAction;
    Actions: TActionList;
    btnCancel: TcxButton;
    btnOk: TcxButton;
    cbCategory: TcxComboBox;
    dxLayoutCxLookAndFeel: TdxLayoutCxLookAndFeel;
    dxLayoutCxLookAndFeelBold: TdxLayoutCxLookAndFeel;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutLookAndFeelList: TdxLayoutLookAndFeelList;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    lbFunctions: TcxListBox;
    lcMain: TdxLayoutControl;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMainItem2: TdxLayoutItem;
    liCategory: TdxLayoutItem;
    liFunctions: TdxLayoutItem;
    liSearchBox: TdxLayoutItem;
    lliFunctionDefinition: TdxLayoutLabeledItem;
    lliFunctionDescription: TdxLayoutLabeledItem;
    teSearchBox: TcxButtonEdit;

    procedure acFindExecute(Sender: TObject);
    procedure cbCategoryPropertiesChange(Sender: TObject);
    procedure lbFunctionsClick(Sender: TObject);
    procedure lbFunctionsDblClick(Sender: TObject);
    procedure teSearchBoxButtonClick(Sender: TObject; AButtonIndex: Integer);
    procedure teSearchBoxKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure teSearchBoxPropertiesChange(Sender: TObject);
  strict private
    procedure ApplyLocalization;
    function CheckFunction(AFunctionInfo: TdxSpreadSheetFunctionInfo;
      AFunctionType: TdxSpreadSheetFunctionType;
      const ASearchText: string): Boolean;
    function HasFunctions(AFunctionType: TdxSpreadSheetFunctionType): Boolean;
    //
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
  protected
    procedure Initialize(ASpreadSheet: TdxCustomSpreadSheet);
    procedure PopulateCategories;
    procedure PopulateFunctions(AFunctionType: TdxSpreadSheetFunctionType;
      const ASearchText: string = '');
  end;

var
  dxSpreadSheetInsertFunctionDialogClass
    : TdxSpreadSheetInsertFunctionDialogFormClass =
    TdxSpreadSheetInsertFunctionDialogForm;

function ShowInsertFunctionDialog(ASpreadSheet: TdxCustomSpreadSheet;
  out AInfo: TdxSpreadSheetFunctionInfo): Boolean;

implementation

uses
  dxSpreadSheetDialogStrs, dxStringHelper, StrUtils, Math, dxSpreadSheetUtils;

{$R *.dfm}

function ShowInsertFunctionDialog(ASpreadSheet: TdxCustomSpreadSheet;
  out AInfo: TdxSpreadSheetFunctionInfo): Boolean;
var
  ADialog: TdxSpreadSheetInsertFunctionDialogForm;
begin
  ADialog := dxSpreadSheetInsertFunctionDialogClass.Create
    (GetParentForm(ASpreadSheet));
  try
    ADialog.Initialize(ASpreadSheet);
    Result := (ADialog.ShowModal = mrOk) and
      (ADialog.lbFunctions.ItemObject <> nil);
    if Result then
      AInfo := TdxSpreadSheetFunctionInfo(ADialog.lbFunctions.ItemObject);
  finally
    ADialog.Free;
  end;
end;

{ TdxSpreadSheetInsertFunctionDialogForm }

procedure TdxSpreadSheetInsertFunctionDialogForm.Initialize
  (ASpreadSheet: TdxCustomSpreadSheet);
begin
  SetControlLookAndFeel(Self, ASpreadSheet.DialogsLookAndFeel);
  PopulateCategories;
  ApplyLocalization;
end;

procedure TdxSpreadSheetInsertFunctionDialogForm.PopulateCategories;
var
  AItems: TStrings;
  AType: TdxSpreadSheetFunctionType;
begin
  AItems := cbCategory.Properties.Items;
  AItems.BeginUpdate;
  try
    AItems.Clear;
    AItems.AddObject(cxGetResourceString(@sdxInsertFunctionDialogCategoryAll),
      TObject(ftCommon));
    for AType := ftCompatibility to High(AType) do
    begin
      if HasFunctions(AType) then
        AItems.AddObject(dxSpreadSheetFunctionTypeNameAsString(AType),
          TObject(AType));
    end;
  finally
    AItems.EndUpdate;
  end;
  cbCategory.ItemIndex := 0;
end;

procedure TdxSpreadSheetInsertFunctionDialogForm.PopulateFunctions
  (AFunctionType: TdxSpreadSheetFunctionType; const ASearchText: string = '');
var
  AFunctionInfo: TdxSpreadSheetFunctionInfo;
  AFunctions: TdxSpreadSheetFunctionsRepository;
  I: Integer;
begin
  lbFunctions.Items.BeginUpdate;
  try
    lbFunctions.Items.Clear;

    AFunctions := dxSpreadSheetFunctionsRepository;
    for I := 0 to AFunctions.Count - 1 do
    begin
      AFunctionInfo := AFunctions.Items[I];
      if CheckFunction(AFunctionInfo, AFunctionType, ASearchText) then
        lbFunctions.AddItem(AFunctionInfo.Name, AFunctionInfo);
    end;
  finally
    lbFunctions.Items.EndUpdate;
    lbFunctions.ItemIndex := 0;
    lbFunctionsClick(nil);
  end;
end;

procedure TdxSpreadSheetInsertFunctionDialogForm.ApplyLocalization;
begin
  Caption := cxGetResourceString(@sdxInsertFunctionDialogCaption);
  btnOk.Caption := cxGetResourceString
    (@sdxContainerCustomizationDialogButtonOK);
  btnCancel.Caption := cxGetResourceString
    (@sdxContainerCustomizationDialogButtonCancel);
  liCategory.Caption := cxGetResourceString(@sdxInsertFunctionDialogCategory);
  liSearchBox.Caption := cxGetResourceString(@sdxInsertFunctionDialogFunctions);
  teSearchBox.TextHint := cxGetResourceString
    (@sdxInsertFunctionDialogSearchBoxTextHint);
end;

function TdxSpreadSheetInsertFunctionDialogForm.CheckFunction
  (AFunctionInfo: TdxSpreadSheetFunctionInfo;
  AFunctionType: TdxSpreadSheetFunctionType; const ASearchText: string)
  : Boolean;
var
  ASearchTextLowerCase: string;
begin
  Result := Assigned(AFunctionInfo.Proc) and
    ((AFunctionType = ftCommon) or (AFunctionInfo.TypeID = AFunctionType));
  if Result and (ASearchText <> '') then
  begin
    ASearchTextLowerCase := LowerCase(ASearchText);
    Result := (Pos(ASearchTextLowerCase, LowerCase(AFunctionInfo.Name)) > 0) or
      (Pos(ASearchTextLowerCase,
      LowerCase(cxGetResourceString(AFunctionInfo.DescriptionPtr))) > 0);
  end;
end;

function TdxSpreadSheetInsertFunctionDialogForm.HasFunctions
  (AFunctionType: TdxSpreadSheetFunctionType): Boolean;
var
  AFunctions: TdxSpreadSheetFunctionsRepository;
  I: Integer;
begin
  AFunctions := dxSpreadSheetFunctionsRepository;
  for I := 0 to AFunctions.Count - 1 do
  begin
    if CheckFunction(AFunctions.Items[I], AFunctionType, '') then
      Exit(True);
  end;
  Result := False;
end;

procedure TdxSpreadSheetInsertFunctionDialogForm.CMDialogKey
  (var Message: TCMDialogKey);
begin
  if (Message.CharCode <> VK_RETURN) or not teSearchBox.Focused then
    inherited;
end;

procedure TdxSpreadSheetInsertFunctionDialogForm.acFindExecute(Sender: TObject);
begin
  if not teSearchBox.Focused then
    teSearchBox.SetFocus;
end;

procedure TdxSpreadSheetInsertFunctionDialogForm.cbCategoryPropertiesChange
  (Sender: TObject);
begin
  PopulateFunctions(TdxSpreadSheetFunctionType(cbCategory.ItemObject),
    teSearchBox.Text);
end;

procedure TdxSpreadSheetInsertFunctionDialogForm.lbFunctionsClick
  (Sender: TObject);
var
  AInfo: TdxSpreadSheetFunctionInfo;
begin
  AInfo := lbFunctions.ItemObject as TdxSpreadSheetFunctionInfo;
  if AInfo <> nil then
  begin
    lliFunctionDescription.Caption := cxGetResourceString(AInfo.DescriptionPtr);
    lliFunctionDefinition.Caption := AInfo.ToString;
  end
  else
  begin
    lliFunctionDescription.Caption := '';
    lliFunctionDefinition.Caption := '';
  end;
end;

procedure TdxSpreadSheetInsertFunctionDialogForm.lbFunctionsDblClick
  (Sender: TObject);
begin
  if lbFunctions.ItemObject <> nil then
    btnOk.Click;
end;

procedure TdxSpreadSheetInsertFunctionDialogForm.teSearchBoxButtonClick
  (Sender: TObject; AButtonIndex: Integer);
begin
  if AButtonIndex = 1 then
    teSearchBox.Text := '';
end;

procedure TdxSpreadSheetInsertFunctionDialogForm.teSearchBoxKeyDown
  (Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  case Key of
    VK_RETURN:
      lbFunctionsDblClick(nil);

    VK_DOWN, VK_UP:
      begin
        lbFunctions.ItemIndex :=
          EnsureRange(lbFunctions.ItemIndex + ValueIncr[Key = VK_DOWN], 0,
          lbFunctions.Items.Count - 1);
        lbFunctionsClick(nil);
      end;
  end;
end;

procedure TdxSpreadSheetInsertFunctionDialogForm.teSearchBoxPropertiesChange
  (Sender: TObject);
begin
  teSearchBox.Properties.Buttons[0].Visible := teSearchBox.Text = '';
  teSearchBox.Properties.Buttons[1].Visible := teSearchBox.Text <> '';
  cbCategoryPropertiesChange(Sender);
end;

end.
