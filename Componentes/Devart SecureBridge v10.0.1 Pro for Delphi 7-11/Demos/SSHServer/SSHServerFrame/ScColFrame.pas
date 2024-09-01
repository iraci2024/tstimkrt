unit ScColFrame;

{$I ..\Base\SBDemo.inc}
interface

uses
{$IFDEF CLR}
  System.ComponentModel,
{$ENDIF}
  Windows, Messages, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Classes, SysUtils, Buttons;

type
  TScColFrame = class(TFrame)
    lbItemName: TListBox;
    Panel1: TPanel;
    btNew: TButton;
    btDelete: TButton;
    PanelItem: TPanel;
    procedure lbItemNameClick(Sender: TObject);

  protected
    FActivated: boolean; // To avoid duplicate call (for example, on TFrame.FrameExit and PageControl.OnChanging events)
    FModified: boolean;
    FOldItemIndex: integer;
    FInStoreItem, FInSelectItem: boolean;

    function GetItems: TCollection; virtual;
    function GetItemName(Item: TCollectionItem): string; virtual;
    procedure InitItems; virtual;
    procedure StoreItem;
    procedure ItemToControls(Item: TCollectionItem); virtual;
    procedure ControlsToItem(Item: TCollectionItem); virtual;
    procedure UpdateControlState(Control: TControl);
    procedure UpdateControlsState; virtual;

    procedure DoActivate; virtual;
    procedure DoFinish; virtual;
    function DoSave: Boolean; virtual;

    property Items: TCollection read GetItems;
  public
    procedure Activate;
    procedure Finish;
    function Save: Boolean;
    procedure SelectItem;
  end;

implementation

{$IFDEF CLR}
{$R *.nfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

{ TScColFrame }

procedure TScColFrame.Activate;
begin
  if not FActivated then
    DoActivate;
  FActivated := True;
end;

procedure TScColFrame.Finish;
begin
  if FActivated then
    DoFinish;
  FActivated := False;
end;

function TScColFrame.Save: Boolean;
begin
  Result := DoSave;
end;

function TScColFrame.GetItems: TCollection;
begin
  Result := nil;
  Assert(False);
end;

function TScColFrame.GetItemName(Item: TCollectionItem): string;
begin
  Result := '';
  Assert(False);
end;

procedure TScColFrame.ItemToControls(Item: TCollectionItem);
begin
  Assert(False);
end;

procedure TScColFrame.ControlsToItem(Item: TCollectionItem);
begin
  Assert(False);
end;

procedure TScColFrame.InitItems;
var
  i: integer;
  OldIndex: integer;
begin
  OldIndex := lbItemName.ItemIndex;
  lbItemName.Items.Clear;

  PanelItem.Enabled := Items.Count > 0;
  if not PanelItem.Enabled then begin
    for i := 0 to PanelItem.ControlCount - 1 do
      if PanelItem.Controls[i] is TCheckBox then
        TCheckBox(PanelItem.Controls[i]).Checked := False;

    UpdateControlsState;
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

procedure TScColFrame.UpdateControlState(Control: TControl);
var
  e: boolean;
  i: Integer;
begin
  e := Enabled and Control.Enabled and Control.Parent.Enabled and Control.Visible;

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
  end;
end;

procedure TScColFrame.UpdateControlsState;
var
  i: Integer;
begin
  for i := 0 to PanelItem.ControlCount - 1 do
    UpdateControlState(PanelItem.Controls[i]);
end;

procedure TScColFrame.StoreItem;
var
  Item: TCollectionItem;
begin
  if (FOldItemIndex <> - 1) and
    (Items.Count > FOldItemIndex) then begin
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
  OldModified: boolean;
  Item: TCollectionItem;
  i: integer;
begin
  OldModified := FModified;
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
    FModified := OldModified;
  end;

  FModified := False;
end;

procedure TScColFrame.DoActivate;
begin
  // on processing error in ControlsToItem FInStoreItem maybe True
  if not FInStoreItem then
    InitItems;
end;

procedure TScColFrame.DoFinish;
begin
  lbItemName.Items.Clear;
end;

function TScColFrame.DoSave: Boolean;
begin
  Result := True;
end;

procedure TScColFrame.lbItemNameClick(Sender: TObject);
begin
  if lbItemName.ItemIndex <> FOldItemIndex then begin
    try
      StoreItem;
    except
      lbItemName.ItemIndex := FOldItemIndex;
      raise;
    end;
    SelectItem;
  end;
end;

end.

