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

unit dxGanttControlSheetColumnsEditor;

{$I cxVer.inc}
{$I dxGanttControl.inc}

interface

uses
  SysUtils, Windows, Messages, DesignIntf, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, Generics.Defaults, Generics.Collections,
  dxForms, cxDesignWindows, StdCtrls, cxControls, dxGanttControlCustomSheet,
  Menus;

type
  { TdxGanttControlSheetColumnsEditor }

  TdxGanttControlSheetColumnsEditor = class(TcxDesignFormEditor)
    pmMain: TPopupMenu;
    miAdd: TMenuItem;
    miDelete: TMenuItem;
    miSelectAll: TMenuItem;
    lbMain: TcxCustomizeListBox;
    miSeparator1: TMenuItem;
    miSeparateor2: TMenuItem;
    miReset: TMenuItem;
    procedure lbMainClick(Sender: TObject);
    procedure lbMainDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure lbMainDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure lbMainEndDrag(Sender, Target: TObject; X, Y: Integer);
    procedure miResetClick(Sender: TObject);
    procedure miDeleteClick(Sender: TObject);
    procedure miSelectAllClick(Sender: TObject);
  strict private
    FPreviousDragIndex: Integer;

    procedure AddItemHandler(Sender: TObject);
    function GetColumns: TdxGanttControlSheetColumns;
    procedure Reindex(AList: TList; ANewIndex: Integer);
  protected
    procedure InitFormEditor; override;
    procedure UpdateContent; override;
    procedure UpdateState;
  public
    property Columns: TdxGanttControlSheetColumns read GetColumns;
  end;

procedure dxShowGanttControlSheetColumnsEditor(ADesigner: IDesigner;
  ASource: TdxGanttControlSheetColumns);

implementation

{$R *.dfm}

uses
  Math;

type
  TdxGanttControlSheetOptionsAccess = class(TdxGanttControlSheetOptions);
  TdxGanttControlSheetColumnsAccess = class(TdxGanttControlSheetColumns);

  TdxGanttControlSheetColumnAccess = class(TdxGanttControlSheetColumn);
  TdxGanttControlSheetColumnClassAccess = class of
    TdxGanttControlSheetColumnAccess;

procedure dxShowGanttControlSheetColumnsEditor(ADesigner: IDesigner;
  ASource: TdxGanttControlSheetColumns);
begin
  ShowFormEditorClass(ADesigner, TdxGanttControlSheetOptionsAccess
    (ASource.Owner).GetOwnerComponent, ASource, 'Columns',
    TdxGanttControlSheetColumnsEditor);
end;

{ TdxGanttControlSheetColumnsEditor }

procedure TdxGanttControlSheetColumnsEditor.AddItemHandler(Sender: TObject);
var
  AColumn: TdxGanttControlSheetColumn;
begin
  Columns.Add(TdxGanttControlSheetColumnsAccess(Columns).RegisteredColumnClasses
    [TMenuItem(Sender).Tag]);
  AColumn := Columns[Columns.Count - 1];
  AColumn.Visible := True;
  SelectComponent(AColumn);
  UpdateContent;
  Designer.Modified;
  ListBoxSynchronizeSelection(lbMain);
end;

function TdxGanttControlSheetColumnsEditor.GetColumns
  : TdxGanttControlSheetColumns;
begin
  Result := TdxGanttControlSheetColumns(ComponentProperty);
end;

procedure TdxGanttControlSheetColumnsEditor.InitFormEditor;
var
  I: Integer;
  AList: TList<TdxGanttControlSheetColumnClass>;
  AMenuItem: TMenuItem;
begin
  inherited InitFormEditor;
  UpdateContent;
  AList := TdxGanttControlSheetColumnsAccess(Columns).RegisteredColumnClasses;
  for I := 0 to AList.Count - 1 do
  begin
    AMenuItem := TMenuItem.Create(pmMain);
    miAdd.Add(AMenuItem);
    AMenuItem.Tag := I;
    AMenuItem.Caption := TdxGanttControlSheetColumnClassAccess(AList[I])
      .GetDesignCaption;
    AMenuItem.OnClick := AddItemHandler;
  end;

end;

procedure TdxGanttControlSheetColumnsEditor.lbMainClick(Sender: TObject);
var
  AList: TList;
  I: Integer;
begin
  AList := TList.Create;
  try
    for I := 0 to lbMain.Items.Count - 1 do
      if lbMain.Selected[I] then
        AList.Add(lbMain.Items.Objects[I]);
    SelectComponents(AList, Columns);
  finally
    AList.Free;
  end;
end;

procedure TdxGanttControlSheetColumnsEditor.lbMainDragDrop(Sender,
  Source: TObject; X, Y: Integer);
begin
  ListBoxDragDrop(lbMain, Sender, Source, X, Y, FPreviousDragIndex, Reindex);
end;

procedure TdxGanttControlSheetColumnsEditor.lbMainDragOver(Sender,
  Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  ListBoxDragOver(lbMain, Sender, Source, X, Y, State, Accept,
    FPreviousDragIndex);
end;

procedure TdxGanttControlSheetColumnsEditor.lbMainEndDrag(Sender,
  Target: TObject; X, Y: Integer);
begin
  ListBoxEndDrag(lbMain, Sender, Target, X, Y, FPreviousDragIndex);
end;

procedure TdxGanttControlSheetColumnsEditor.miDeleteClick(Sender: TObject);

  function FindItemToSelect: TObject;
  var
    I: Integer;
  begin
    Result := nil;
    with lbMain do
    begin
      if ItemIndex = -1 then
        Exit;
      if not Selected[ItemIndex] then
        Result := Items.Objects[ItemIndex]
      else
      begin
        for I := ItemIndex + 1 to Items.Count - 1 do
          if not Selected[I] then
          begin
            Result := Items.Objects[I];
            Exit
          end;
        for I := ItemIndex - 1 downto 0 do
          if not Selected[I] then
          begin
            Result := Items.Objects[I];
            Exit
          end;
      end;
    end;
  end;

var
  AList: TList;
  I: Integer;
  AColumn: TdxGanttControlSheetColumn;
  AItemToSelect: TObject;
  AIndex: Integer;
begin
  if lbMain.SelCount = 0 then
    Exit;
  AItemToSelect := FindItemToSelect;
  lbMain.Items.BeginUpdate;
  BeginUpdate;
  Columns.BeginUpdate;
  try
    AList := TList.Create;
    try
      ListBoxGetSelection(lbMain, AList);
      lbMain.DeleteSelected;
      AIndex := lbMain.Items.IndexOfObject(AItemToSelect);
      for I := AList.Count - 1 downto 0 do
      begin
        AColumn := TdxGanttControlSheetColumn(AList[I]);
        TdxGanttControlSheetColumnsAccess(Columns).Extract(AColumn);
      end;
    finally
      AList.Free;
    end;
    Designer.Modified;
    UpdateContent;
    if AIndex >= 0 then
    begin
      ListBoxSetSelected(lbMain, AIndex, True);
      SelectComponent(TPersistent(lbMain.Items.Objects[AIndex]));
    end
    else
      SelectComponent(Component);
  finally
    Columns.EndUpdate;
    CancelUpdate;
    lbMain.Items.EndUpdate;
  end;
end;

procedure TdxGanttControlSheetColumnsEditor.miResetClick(Sender: TObject);
begin
  BeginUpdate;
  try
    Columns.Reset;
    UpdateContent;
    ListBoxApplySelection(lbMain, ComponentProperty);
  finally
    EndUpdate;
  end;
  Designer.Modified;
end;

procedure TdxGanttControlSheetColumnsEditor.miSelectAllClick(Sender: TObject);
begin
  ListBoxSelectAll(lbMain);
end;

procedure TdxGanttControlSheetColumnsEditor.Reindex(AList: TList;
  ANewIndex: Integer);
var
  I: Integer;
begin
  if AList.Count = 0 then
    Exit;
  ANewIndex := Min(ANewIndex, Columns.Count - 1);
  if TdxGanttControlSheetColumn(AList[0]).Index < ANewIndex then
  begin
    for I := 0 to AList.Count - 1 do
      TdxGanttControlSheetColumn(AList[I]).Index := ANewIndex;
  end
  else
  begin
    for I := AList.Count - 1 downto 0 do
      TdxGanttControlSheetColumn(AList[I]).Index := ANewIndex;
  end;
  UpdateContent;
  Designer.Modified;
end;

procedure TdxGanttControlSheetColumnsEditor.UpdateContent;
var
  ASelections: TStringList;
  AItemIndex, ATopIndex: Integer;
  I: Integer;
begin
  inherited UpdateContent;
  lbMain.Items.BeginUpdate;
  try
    ListBoxSaveSelection(lbMain, ASelections, AItemIndex, ATopIndex);
    try
      lbMain.Items.Clear;
      for I := 0 to Columns.Count - 1 do
        lbMain.Items.AddObject(TdxGanttControlSheetColumnAccess(Columns[I])
          .GetDesignCaption, Columns[I]);
    finally
      ListBoxRestoreSelection(lbMain, ASelections, AItemIndex, ATopIndex);
    end;
  finally
    lbMain.Items.EndUpdate;
  end;
  UpdateState;
end;

procedure TdxGanttControlSheetColumnsEditor.UpdateState;
begin
  miSelectAll.Enabled := lbMain.Items.Count > 0;
  miDelete.Enabled := lbMain.Items.Count > 0;
end;

end.
