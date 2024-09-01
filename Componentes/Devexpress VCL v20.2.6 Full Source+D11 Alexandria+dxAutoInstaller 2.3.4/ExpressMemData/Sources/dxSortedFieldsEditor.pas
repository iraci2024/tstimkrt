{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressMemData }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSMEMDATA AND ALL }
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

unit dxSortedFieldsEditor;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DB, ImgList,
  ActnList, Menus,
  dxCore, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  dxLayoutContainer, dxLayoutControlAdapters,
  cxClasses, dxLayoutControl, dxmdaset, cxImageList, cxButtons, dxForms;

type
  TdxSortedFieldsEditor = class(TdxForm)
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutControl1: TdxLayoutControl;
    lbSortedFields: TListBox;
    dxLayoutItem2: TdxLayoutItem;
    btnCancel: TButton;
    dxLayoutItem3: TdxLayoutItem;
    btnOk: TButton;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutGroup4: TdxLayoutGroup;
    ActionList1: TActionList;
    aAddField: TAction;
    aDeleteFields: TAction;
    dxLayoutItem1: TdxLayoutItem;
    btnAddFields: TcxButton;
    ilActions: TcxImageList;
    dxLayoutItem5: TdxLayoutItem;
    btnDeleteFields: TcxButton;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    pmAddFields: TPopupMenu;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    dxLayoutItem6: TdxLayoutItem;
    btnDescending: TcxButton;
    dxLayoutItem7: TdxLayoutItem;
    btnMoveUp: TcxButton;
    dxLayoutItem8: TdxLayoutItem;
    btnCaseInsensitive: TcxButton;
    dxLayoutItem9: TdxLayoutItem;
    btnMoveDown: TcxButton;
    dxLayoutItem10: TdxLayoutItem;
    btnAscending: TcxButton;
    dxLayoutSeparatorItem2: TdxLayoutSeparatorItem;
    aAscending: TAction;
    aDescending: TAction;
    aCaseInsensitive: TAction;
    aMoveUp: TAction;
    aMoveDown: TAction;
    procedure aAddFieldExecute(Sender: TObject);
    procedure pmAddFieldsPopup(Sender: TObject);
    procedure aDeleteFieldsExecute(Sender: TObject);
    procedure lbSortedFieldsClick(Sender: TObject);
    procedure aMoveUpExecute(Sender: TObject);
    procedure aMoveDownExecute(Sender: TObject);
    procedure aAscendingExecute(Sender: TObject);
  private
    FMemData: TdxMemData;

    procedure SetMemData(AMemData: TdxMemData);
    procedure pmAddFieldsClick(Sender: TObject);
    function IsFieldSorded(const AFieldName: string): Boolean;
    function GetFieldIndex(const AFieldName: string): Integer;
    function GetFieldName(const AListName: string): string;
    function GetValidSortedFields(const ASortedFields: string): string;

    procedure AddField(const AField: string);
  protected
    procedure RefreshListBoxes;
    procedure RefreshEnabled;
    function GetSortedFields: string;

    property MemData: TdxMemData read FMemData write SetMemData;
  end;

procedure EditSortedFields(AMemData: TdxMemData);

implementation

{$R *.dfm}

uses
  Math;

procedure EditSortedFields(AMemData: TdxMemData);
var
  AEditor: TdxSortedFieldsEditor;
begin
  AEditor := TdxSortedFieldsEditor.Create(nil);
  try
    AEditor.MemData := AMemData;
    AEditor.Caption := AMemData.Name + '.SortedFields Editor';
    if AEditor.ShowModal = mrOk then
      AMemData.SortedFields := AEditor.GetSortedFields;
  finally
    AEditor.Free;
  end;
end;

type
  TSortingOption = (soA, soD, soN);
  TSortingOptions = set of TSortingOption;

function GetOptions(const AFieldItem: string): TSortingOptions;
var
  AColonPos: Integer;
  I: Integer;
begin
  AColonPos := Pos(':', AFieldItem);
  if AColonPos > 0 then
  begin
    Result := [];
    for I := AColonPos to Length(AFieldItem) do
    begin
      case AFieldItem[I] of
        'n', 'N':
          Include(Result, soN);
        'd', 'D':
          begin
            Include(Result, soD);
            Exclude(Result, soA);
          end;
        'a', 'A':
          begin
            Include(Result, soA);
            Exclude(Result, soD);
          end;
      end;
    end;
  end
  else
    Result := [];
end;

procedure TdxSortedFieldsEditor.SetMemData(AMemData: TdxMemData);
begin
  if FMemData <> AMemData then
  begin
    FMemData := AMemData;
    RefreshListBoxes;
  end;
end;

procedure TdxSortedFieldsEditor.pmAddFieldsClick(Sender: TObject);

  procedure RemoveField(const AField: string);
  begin
    lbSortedFields.Items.Delete(GetFieldIndex(AField));
  end;

begin
  if (Sender as TMenuItem).Checked then
    RemoveField(RemoveAccelChars(TMenuItem(Sender).Caption, False))
  else
    AddField(RemoveAccelChars(TMenuItem(Sender).Caption, False));

  RefreshEnabled;
end;

function TdxSortedFieldsEditor.IsFieldSorded(const AFieldName: string): Boolean;
begin
  Result := GetFieldIndex(AFieldName) > -1;
end;

procedure TdxSortedFieldsEditor.lbSortedFieldsClick(Sender: TObject);
var
  I: Integer;
begin
  if (lbSortedFields.SelCount = 1) and
    (not lbSortedFields.Selected[lbSortedFields.ItemIndex]) then
    for I := 0 to lbSortedFields.Count - 1 do
      lbSortedFields.Selected[I] := lbSortedFields.ItemIndex = I;
  RefreshEnabled;
end;

function TdxSortedFieldsEditor.GetFieldIndex(const AFieldName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to lbSortedFields.Count - 1 do
    if GetFieldName(lbSortedFields.Items[I]) = AFieldName then
      Exit(I);
end;

function TdxSortedFieldsEditor.GetFieldName(const AListName: string): string;
var
  AColonPos: Integer;
begin
  AColonPos := Pos(':', AListName);
  if AColonPos > 0 then
    Result := Copy(AListName, 1, AColonPos - 1)
  else
    Result := AListName;
end;

function TdxSortedFieldsEditor.GetValidSortedFields(const ASortedFields
  : string): string;

  function IsValidSortedField(var ASortedField: string): Boolean;
  var
    ASortedRule: string;
    AColonPos: Integer;
    I: Integer;
    AFieldName: string;
  begin
    Result := False;
    AFieldName := GetFieldName(ASortedField);
    for I := 0 to MemData.FieldCount - 1 do
      if AFieldName = MemData.Fields[I].DisplayName then
      begin
        Result := True;
        AColonPos := Pos(':', ASortedField);
        if AColonPos > 0 then
        begin
          ASortedRule := AnsiUpperCase(Copy(ASortedField, AColonPos + 1,
            Length(ASortedField) - AColonPos));
          ASortedField := AFieldName;
          if (Length(ASortedRule) > 0) and CharInSet(ASortedRule[1],
            ['A', 'D', 'N']) then
          begin
            ASortedField := ASortedField + ':' + ASortedRule[1];
            if (Length(ASortedRule) > 1) and (ASortedRule[1] <> 'N') and
              (ASortedRule[2] = 'N') then
              ASortedField := ASortedField + 'N';
          end;
        end;
        Break;
      end;
  end;

  function IsRepeatingField(AIndex: Integer; AStringList: TStrings): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := AStringList.Count - 1 downto AIndex + 1 do
      if GetFieldName(AStringList[I]) = GetFieldName(AStringList[AIndex]) then
      begin
        Result := True;
        Break;
      end;
  end;

var
  AStringList: TStrings;
  ASortedField: string;
  I: Integer;
begin
  AStringList := TStringList.Create;
  try
    AStringList.Delimiter := ';';
    AStringList.DelimitedText := ASortedFields;
    for I := AStringList.Count - 1 downto 0 do
    begin
      ASortedField := AStringList[I];
      if IsRepeatingField(I, AStringList) or not IsValidSortedField(ASortedField)
      then
        AStringList.Delete(I)
      else
        AStringList[I] := ASortedField;
    end;
    Result := AStringList.DelimitedText;
  finally
    AStringList.Free;
  end;
end;

procedure TdxSortedFieldsEditor.aAscendingExecute(Sender: TObject);
var
  AListNameBuilder: TStringBuilder;
begin
  TAction(Sender).Checked := not TAction(Sender).Checked;
  AListNameBuilder := TStringBuilder.Create;
  try
    AListNameBuilder.Append
      (GetFieldName(lbSortedFields.Items[lbSortedFields.ItemIndex]));
    if aAscending.Checked or aDescending.Checked or aCaseInsensitive.Checked
    then
    begin
      AListNameBuilder.Append(':');
      if aAscending.Checked then
        AListNameBuilder.Append('A')
      else if aDescending.Checked then
        AListNameBuilder.Append('D');
      if aCaseInsensitive.Checked then
        AListNameBuilder.Append('N');
    end;
    lbSortedFields.Items[lbSortedFields.ItemIndex] := AListNameBuilder.ToString;
    lbSortedFields.Selected[lbSortedFields.ItemIndex] := True;
  finally
    AListNameBuilder.Free;
  end;
end;

procedure TdxSortedFieldsEditor.AddField(const AField: string);
begin
  lbSortedFields.Items.Add(AField);
  lbSortedFields.ClearSelection;
  lbSortedFields.Selected[lbSortedFields.Count - 1] := True;
  RefreshEnabled;
end;

procedure TdxSortedFieldsEditor.aMoveDownExecute(Sender: TObject);
var
  ANewItemIndex: Integer;
begin
  ANewItemIndex := lbSortedFields.ItemIndex + 1;
  lbSortedFields.Items.Move(lbSortedFields.ItemIndex, ANewItemIndex);
  lbSortedFields.Selected[ANewItemIndex] := True;
  RefreshEnabled;
end;

procedure TdxSortedFieldsEditor.aMoveUpExecute(Sender: TObject);
var
  ANewItemIndex: Integer;
begin
  ANewItemIndex := lbSortedFields.ItemIndex - 1;
  lbSortedFields.Items.Move(lbSortedFields.ItemIndex, ANewItemIndex);
  lbSortedFields.Selected[ANewItemIndex] := True;
  RefreshEnabled;
end;

procedure TdxSortedFieldsEditor.aDeleteFieldsExecute(Sender: TObject);
var
  AIndex: Integer;
begin
  AIndex := Min(lbSortedFields.ItemIndex, lbSortedFields.Count - 2);
  lbSortedFields.DeleteSelected;
  lbSortedFields.Selected[AIndex] := True;
  RefreshEnabled;
end;

procedure TdxSortedFieldsEditor.pmAddFieldsPopup(Sender: TObject);
var
  I: Integer;
begin
  for I := 0 to pmAddFields.Items.Count - 1 do
    pmAddFields.Items[I].Checked :=
      IsFieldSorded(RemoveAccelChars(pmAddFields.Items[I].Caption, False));
end;

procedure TdxSortedFieldsEditor.RefreshListBoxes;
var
  I: Integer;
  AMenuItem: TMenuItem;
begin
  lbSortedFields.Items.Delimiter := ';';
  lbSortedFields.Items.DelimitedText :=
    GetValidSortedFields(MemData.SortedFields);
  for I := 1 to MemData.FieldList.Count - 1 do
  begin
    AMenuItem := TMenuItem.Create(pmAddFields);
    pmAddFields.Items.Add(AMenuItem);
    AMenuItem.Caption := MemData.FieldList.Fields[I].FieldName;
    AMenuItem.OnClick := pmAddFieldsClick;
  end;
  RefreshEnabled;
end;

procedure TdxSortedFieldsEditor.RefreshEnabled;
var
  ASelectedField: string;
  AOptions: TSortingOptions;
begin
  aAddField.Enabled := pmAddFields.Items.Count > 0;
  aDeleteFields.Enabled := lbSortedFields.SelCount > 0;
  aMoveUp.Enabled := (lbSortedFields.SelCount = 1) and
    (lbSortedFields.ItemIndex > 0);
  aMoveDown.Enabled := (lbSortedFields.SelCount = 1) and
    (lbSortedFields.ItemIndex < lbSortedFields.Count - 1);

  if lbSortedFields.SelCount = 1 then
  begin
    ASelectedField := lbSortedFields.Items[lbSortedFields.ItemIndex];
    AOptions := GetOptions(ASelectedField);
    aAscending.Enabled := True;
    aAscending.Checked := soA in AOptions;
    aDescending.Enabled := True;
    aDescending.Checked := soD in AOptions;
    aCaseInsensitive.Enabled := True;
    aCaseInsensitive.Checked := soN in AOptions;
  end
  else
  begin
    aAscending.Enabled := False;
    aAscending.Checked := False;
    aDescending.Enabled := False;
    aDescending.Checked := False;
    aCaseInsensitive.Enabled := False;
    aCaseInsensitive.Checked := False;
  end;
end;

procedure TdxSortedFieldsEditor.aAddFieldExecute(Sender: TObject);
begin
  //
end;

function TdxSortedFieldsEditor.GetSortedFields: string;
begin
  Result := lbSortedFields.Items.DelimitedText;
end;

end.
