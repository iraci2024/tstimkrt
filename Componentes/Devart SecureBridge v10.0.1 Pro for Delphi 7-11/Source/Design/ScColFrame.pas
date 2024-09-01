
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright � 2007-2021 Devart. All right reserved.
//  Collection Frame
//////////////////////////////////////////////////

{$I SB.inc}

unit ScColFrame;

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  Classes, SysUtils,
  ScFrame, ScTabEditor;

type
  TScColFrame = class(TScFrame)
    lbItemName: TListBox;
    PanelItem: TPanel;
    procedure lbItemNameClick(Sender: TObject);

  protected
    FOldItemIndex: integer;

    FInStoreItem, FInSelectItem: boolean;

    function GetItems: TCollection; virtual;
    function GetItemName(Item: TCollectionItem): string; virtual;
    procedure InitItems; virtual;
    procedure StoreItem;
    function IsControlEnabled(Control: TControl): boolean;
    procedure ItemToControls(Item: TCollectionItem); virtual;
    procedure ControlsToItem(Item: TCollectionItem); virtual;
    procedure UpdateControlState(Control: TControl);
    procedure UpdateControlsState; virtual;

    procedure DoActivate; override;
    procedure DoFinish; override;

    property Items: TCollection read GetItems;
  public
    procedure SelectItem;  
  end;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

{uses
  DB, DASQLComponentEditor;
}

function TScColFrame.GetItems: TCollection;
begin
  Result := nil;
  Assert(False, 'Must be overriden');
end;

function TScColFrame.GetItemName(Item: TCollectionItem): string;
begin
  Result := '';
  Assert(False, 'Must be overriden');
end;

procedure TScColFrame.ItemToControls(Item: TCollectionItem);
begin
  Assert(False, 'Must be overriden');
end;

procedure TScColFrame.ControlsToItem(Item: TCollectionItem);
begin
  Assert(False, 'Must be overriden');
end;

procedure TScColFrame.InitItems;
var
  i: integer;
  OldIndex: integer;
begin
  OldIndex := lbItemName.ItemIndex;
  lbItemName.Items.Clear;
  Enabled := Items.Count > 0;
  if not Enabled then begin
    UpdateControlsState;
    if (Editor.ActiveControl = nil) and (TScTabEditorForm(Editor).PageControl.ActivePage = Page) then
      Editor.ActiveControl := Page;
    Exit;
  end;
  FOldItemIndex := -1;

  for i := 0 to Items.Count - 1 do
    lbItemName.Items.Add(GetItemName(Items.Items[i]));

  if OldIndex >= 0 then begin
    if OldIndex < Items.Count then
      lbItemName.ItemIndex := OldIndex
    else
      lbItemName.ItemIndex := Items.Count - 1;
  end
  else
    lbItemName.ItemIndex := 0;

  SelectItem;
end;

function TScColFrame.IsControlEnabled(Control: TControl): boolean;
begin
  Result := Enabled and Control.Enabled and Control.Parent.Enabled and Control.Visible;
end;

procedure TScColFrame.UpdateControlState(Control: TControl);
var
  e: boolean;
  i: integer;
begin
  e := IsControlEnabled(Control);

  if Control is TPanel then begin
    for i := 0 to TPanel(Control).ControlCount - 1 do begin
      TPanel(Control).Controls[i].Enabled := e;
      UpdateControlState(TPanel(Control).Controls[i]);
    end;
  end
  else
  if Control is TComboBox then begin
    if e then
      TComboBox(Control).Color := clWindow
    else
    begin
      TComboBox(Control).Color := clBtnFace;
      TComboBox(Control).Text := '';
      TComboBox(Control).ItemIndex := -1;
    end;
  end
  else
  if Control is TEdit then begin
    if e then begin
      if TEdit(Control).ReadOnly then
        TEdit(Control).Color := clBtnFace
      else
        TEdit(Control).Color := clWindow;
      TEdit(Control).ParentFont := True;
    end
    else
    begin
      TEdit(Control).Color := clBtnFace;
      TEdit(Control).Font.Color := clBtnFace;
      TEdit(Control).Text := '';
    end;
  end
  else
  if Control is TMemo then begin
    if e then begin
      TMemo(Control).Color := clWindow;
      TMemo(Control).ParentFont := True;
    end
    else
    begin
      TMemo(Control).Color := clBtnFace;
      TMemo(Control).Font.Color := clBtnFace;
      TMemo(Control).Text := '';
    end;
  end
  else
  if Control is TCustomMemo then begin // TUniMemo
    if not e then
      TCustomMemo(Control).Text := '';
  end
  else
  if not (Control is TLabel) and not (Control is TButton) and
    not (Control is TBevel) and not (Control is TCheckBox) then
    Assert(False, Control.Name + ' is ' + Control.ClassName);
end;

procedure TScColFrame.UpdateControlsState;
var
  i: integer;
begin
  for i := 0 to PanelItem.ControlCount - 1 do
    UpdateControlState(PanelItem.Controls[i]);
end;

procedure TScColFrame.StoreItem;
var
  Item: TCollectionItem;
begin
  if (FOldItemIndex <> - 1) and (Items.Count > FOldItemIndex) then begin
    Item := Items.Items[FOldItemIndex];
    FInStoreItem := True;
    try
      ControlsToItem(Item);
    finally
      FInStoreItem := False;
    end;
  end;
end;

procedure TScColFrame.SelectItem;
var
  Item: TCollectionItem;
  OldModified: boolean;
  i: integer;

begin
  OldModified := Modified;
  FInSelectItem := True;
  try
    PanelItem.Enabled := lbItemName.ItemIndex <> - 1;

    if PanelItem.Enabled then begin
      for i := 0 to PanelItem.ControlCount - 1 do
        PanelItem.Controls[i].Enabled := True;

      Item := Items.Items[lbItemName.ItemIndex];
      ItemToControls(Item);

      FOldItemIndex := lbItemName.ItemIndex;
    end;

  finally
    UpdateControlsState;
    FInSelectItem := False;
    Modified := OldModified;
  end;
end;

procedure TScColFrame.DoActivate;
begin
  inherited;

  // on processing error in ControlsToItem FInStoreItem maybe True 
  if not FInStoreItem then
    InitItems;
end;

procedure TScColFrame.DoFinish;
begin
  inherited;
{
  Temporary commented to avoid folowing situations:
  1. Set DataType to Integer
  2. Set value to 'qqqq'
  3. Press Cancel button
  4. Press 'Yes'

  Assert(Owner is TDASQLEditorForm);
  if (TDASQLEditorForm(Owner).ActiveControl <> TDASQLEditorForm(Owner).btCancel)
    and (TDASQLEditorForm(Owner).ActiveControl <> nil) then
}

  StoreItem;
end;

procedure TScColFrame.lbItemNameClick(Sender: TObject);
begin
  if lbItemName.ItemIndex <> FOldItemIndex then begin
    StoreItem;
    SelectItem;
  end;
end;

end.
