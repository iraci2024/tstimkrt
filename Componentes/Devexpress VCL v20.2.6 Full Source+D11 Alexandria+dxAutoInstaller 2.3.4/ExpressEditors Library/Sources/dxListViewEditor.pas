unit dxListViewEditor;

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, dxLayoutContainer, dxLayoutControlAdapters, cxClasses,
  dxLayoutControl, dxListView;

const
  UM_TREEEDIT = WM_USER;

type
  TdxItemInfo = class
  private
    FSubItems: TStringList;
    FSubItemImages: TList;
    FCaption: string;
    FImageIndex: Integer;
    FStateIndex: Integer;
    FGroupID: Integer;
  public
    constructor Create(AItem: TdxListItem);
    destructor Destroy; override;
  end;

  TfmListViewItems = class(TForm)
    btNewItem: TButton;
    btDelete: TButton;
    tvItems: TTreeView;
    btNewSubItem: TButton;
    edCaption: TEdit;
    edImageIndex: TEdit;
    edStateImageIndex: TEdit;
    btOk: TButton;
    btCancel: TButton;
    btApply: TButton;
    btHelp: TButton;
    cbGroupID: TComboBox;
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutControl1: TdxLayoutControl;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    grpItemProperties: TdxLayoutGroup;
    dxLayoutGroup6: TdxLayoutGroup;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem6: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutGroup7: TdxLayoutGroup;
    dxLayoutItem9: TdxLayoutItem;
    dxLayoutItem10: TdxLayoutItem;
    dxLayoutItem11: TdxLayoutItem;
    dxLayoutItem12: TdxLayoutItem;
    procedure btNewItemClick(Sender: TObject);
    procedure btNewSubItemClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure btApplyClick(Sender: TObject);
    procedure btHelpClick(Sender: TObject);
    procedure cbGroupIDChange(Sender: TObject);
    procedure edCaptionExit(Sender: TObject);
    procedure edImageIndexExit(Sender: TObject);
    procedure edStateImageIndexExit(Sender: TObject);
    procedure tvItemsChange(Sender: TObject; Node: TTreeNode);
    procedure tvItemsChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure tvItemsDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure tvItemsDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure tvItemsEdited(Sender: TObject; Node: TTreeNode; var S: string);
    procedure tvItemsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure tvItemsEditing(Sender: TObject; Node: TTreeNode;
      var AllowEdit: Boolean);
    procedure ValueChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FItems: TdxListItems;
    FDropping: Boolean;
    procedure FlushControls;
    procedure GetItem(AItemInfo: TdxItemInfo; AValue: TdxListItem);
    procedure SetItem(AValue: TdxItemInfo);
    procedure SetStates;
    procedure SetSubItem(const S: string; AImageIndex: Integer);
    procedure UpdateCaption;
  public
    property Items: TdxListItems read FItems;
  end;

function dxEditListViewItems(AItems: TdxListItems): Boolean;

implementation

uses
  CommCtrl, dxThreading;

const
  SItemEditNoGroupID = '-1 - (None)';
  SItemEditGroupIDStr = '%d - %s';

{$R *.dfm}

var
  SavedWidth, SavedHeight, SavedLeft, SavedTop: Integer;

function dxEditListViewItems(AItems: TdxListItems): Boolean;
var
  I, J: Integer;
  ANode: TTreeNode;
  AItemInfo: TdxItemInfo;
  AListView: TdxListView;
begin
  with TfmListViewItems.Create(Application) do
    try
      FItems := AItems;
      tvItems.Items.BeginUpdate;
      try
        for I := 0 to Items.Count - 1 do
        begin
          AItemInfo := TdxItemInfo.Create(Items[I]);
          ANode := tvItems.Items.AddObject(nil, AItemInfo.FCaption, AItemInfo);
          for J := 0 to AItemInfo.FSubItems.Count - 1 do
            AItemInfo.FSubItems.Objects[J] := tvItems.Items.AddChild(ANode,
              AItemInfo.FSubItems[J]);
        end;
      finally
        tvItems.Items.EndUpdate;
      end;

      AListView := TdxListView(FItems.ListView);
      cbGroupID.AddItem(SItemEditNoGroupID, nil);
      for I := 0 to AListView.Groups.Count - 1 do
        cbGroupID.AddItem(Format(SItemEditGroupIDStr,
          [AListView.Groups[I].GroupID, AListView.Groups[I].Header]),
          AListView.Groups[I]);

      with tvItems do
        if Items.Count > 0 then
        begin
          Selected := Items.GetFirstNode;
          tvItemsChange(nil, Selected);
        end;
      SetStates;

      Result := ShowModal = mrOK;
      if Result and btApply.Enabled then
        btApplyClick(nil);
    finally
      Free;
    end;
end;

procedure ConvertError(AValue: TEdit);
begin
  with AValue do
  begin
    SetFocus;
    SelectAll;
  end;
end;

{ TdxItemInfo }

constructor TdxItemInfo.Create(AItem: TdxListItem);
var
  I: Integer;
begin
  inherited Create;
  FSubItems := TStringList.Create;
  FSubItemImages := TList.Create;
  FStateIndex := -1;
  FGroupID := -1;
  if AItem <> nil then
    with AItem do
    begin
      FCaption := Caption;
      FImageIndex := ImageIndex;
      FStateIndex := StateIndex;
      FGroupID := GroupID;
      FSubItems.Assign(SubItems);
      for I := 0 to SubItems.Count - 1 do
        FSubItemImages.Add(Pointer(SubItemImages[I]));
    end;
end;

destructor TdxItemInfo.Destroy;
begin
  FSubItems.Free;
  FSubItemImages.Free;
  inherited Destroy;
end;

{ TdxListViewItems }

procedure TfmListViewItems.SetStates;
begin
  btDelete.Enabled := tvItems.Items.Count > 0;
  grpItemProperties.Enabled := btDelete.Enabled;
  btApply.Enabled := False;
  btNewSubItem.Enabled := tvItems.Selected <> nil;
end;

procedure TfmListViewItems.GetItem(AItemInfo: TdxItemInfo; AValue: TdxListItem);
var
  I: Integer;
begin
  with AValue do
  begin
    Caption := AItemInfo.FCaption;
    ImageIndex := AItemInfo.FImageIndex;
    StateIndex := AItemInfo.FStateIndex;
    GroupID := AItemInfo.FGroupID;
    SubItems.Assign(AItemInfo.FSubItems);
    for I := 0 to AItemInfo.FSubItems.Count - 1 do
      SubItemImages[I] := Integer(AItemInfo.FSubItemImages[I]);
  end;
end;

procedure TfmListViewItems.SetSubItem(const S: string; AImageIndex: Integer);
begin
  edStateImageIndex.Enabled := False;
  cbGroupID.Enabled := False;
  edImageIndex.Text := IntToStr(AImageIndex);
  edCaption.Text := S;
end;

procedure TfmListViewItems.SetItem(AValue: TdxItemInfo);
var
  I: Integer;
begin
  edImageIndex.Enabled := True;
  edStateImageIndex.Enabled := True;
  cbGroupID.Enabled := True;
  if AValue <> nil then
    with AValue do
    begin
      edCaption.Text := FCaption;
      edImageIndex.Text := IntToStr(FImageIndex);
      edStateImageIndex.Text := IntToStr(FStateIndex);

      if FGroupID < 0 then
        cbGroupID.ItemIndex := 0
      else
      begin
        for I := 1 to cbGroupID.Items.Count - 1 do
        begin
          if TdxListGroup(cbGroupID.Items.Objects[I]).GroupID = FGroupID then
          begin
            cbGroupID.ItemIndex := I;
            break;
          end;
        end;
      end;
    end
  else
  begin
    edCaption.Text := '';
    edImageIndex.Text := '';
    edStateImageIndex.Text := '';
  end;
end;

procedure TfmListViewItems.FlushControls;
begin
  edCaptionExit(nil);
  edImageIndexExit(nil);
  edStateImageIndexExit(nil);
end;

procedure TfmListViewItems.tvItemsChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  if not FDropping then
    FlushControls;
end;

procedure TfmListViewItems.tvItemsChange(Sender: TObject; Node: TTreeNode);
var
  ATempEnabled: Boolean;
  AItemInfoSubs: TStrings;
  AIndex: Integer;
begin
  ATempEnabled := btApply.Enabled;
  if Node <> nil then
  begin
    SetStates;
    if Node.Data <> nil then
      SetItem(TdxItemInfo(Node.Data))
    else
    begin
      AItemInfoSubs := TdxItemInfo(Node.Parent.Data).FSubItems;
      AIndex := AItemInfoSubs.IndexOfObject(Node);
      SetSubItem(AItemInfoSubs[AIndex],
        Integer(TdxItemInfo(Node.Parent.Data).FSubItemImages[AIndex]));
    end;
  end
  else
    SetItem(nil);
  btApply.Enabled := ATempEnabled;
end;

procedure TfmListViewItems.btNewItemClick(Sender: TObject);
var
  ANode: TTreeNode;
begin
  ANode := tvItems.Items.AddObject(nil, '', TdxItemInfo.Create(nil));
  ANode.MakeVisible;
  tvItems.Selected := ANode;
  edCaption.SetFocus;
  btApply.Enabled := True;
end;

procedure TfmListViewItems.btNewSubItemClick(Sender: TObject);
var
  ANode, ANewNode: TTreeNode;
begin
  with tvItems do
  begin
    ANode := Selected;
    if ANode <> nil then
    begin
      if ANode.Data <> nil then
      begin
        ANewNode := ANode.Owner.AddChild(ANode, '');
        TdxItemInfo(ANode.Data).FSubItems.AddObject('', ANewNode);
        TdxItemInfo(ANode.Data).FSubItemImages.Add(Pointer(-1));
      end
      else
      begin
        ANewNode := ANode.Owner.Add(ANode, '');
        TdxItemInfo(ANode.Parent.Data).FSubItems.AddObject('', ANewNode);
        TdxItemInfo(ANode.Parent.Data).FSubItemImages.Add(Pointer(-1));
      end;
      ANewNode.MakeVisible;
      Selected := ANewNode;
    end;
  end;
  edCaption.SetFocus;
  btApply.Enabled := True;
end;

procedure TfmListViewItems.btDeleteClick(Sender: TObject);
var
  ANode: TTreeNode;
  AIndex: Integer;
begin
  ANode := tvItems.Selected;
  if ANode <> nil then
  begin
    if ANode.Data = nil then
    begin
      AIndex := TdxItemInfo(ANode.Parent.Data).FSubItems.IndexOfObject(ANode);
      TdxItemInfo(ANode.Parent.Data).FSubItems.Delete(AIndex);
      TdxItemInfo(ANode.Parent.Data).FSubItemImages.Delete(AIndex);
    end
    else
      TdxItemInfo(ANode.Data).Free;
    ANode.Free;
  end;
  if tvItems.Items.Count = 0 then
    SetItem(nil);
  SetStates;
  btApply.Enabled := True;
end;

procedure TfmListViewItems.ValueChange(Sender: TObject);
begin
  btApply.Enabled := True;
  if Sender = edCaption then
    edCaptionExit(Sender);
end;

procedure TfmListViewItems.edCaptionExit(Sender: TObject);
var
  ANode: TTreeNode;
  AItemInfoSubs: TStrings;
begin
  ANode := tvItems.Selected;
  if ANode <> nil then
  begin
    if ANode.Data = nil then
    begin
      AItemInfoSubs := TdxItemInfo(ANode.Parent.Data).FSubItems;
      AItemInfoSubs[AItemInfoSubs.IndexOfObject(ANode)] := edCaption.Text;
    end
    else
      TdxItemInfo(ANode.Data).FCaption := edCaption.Text;
    ANode.Text := edCaption.Text;
  end;
end;

procedure TfmListViewItems.cbGroupIDChange(Sender: TObject);
var
  ANode: TTreeNode;
begin
  ValueChange(Sender);

  ANode := tvItems.Selected;
  if (ANode <> nil) and (ANode.Data <> nil) then
  begin
    if cbGroupID.ItemIndex = 0 then
    begin
      TdxItemInfo(ANode.Data).FGroupID := -1;
    end
    else
    begin
      TdxItemInfo(ANode.Data).FGroupID :=
        TdxListGroup(cbGroupID.Items.Objects[cbGroupID.ItemIndex]).GroupID;
    end;
  end;
end;

procedure TfmListViewItems.edImageIndexExit(Sender: TObject);
var
  ANode: TTreeNode;
  AIndex: Integer;
begin
  if ActiveControl <> btCancel then
    try
      ANode := tvItems.Selected;
      if (ANode <> nil) then
      begin
        if (ANode.Data <> nil) then
          TdxItemInfo(ANode.Data).FImageIndex := StrToInt(edImageIndex.Text)
        else
        begin
          AIndex := TdxItemInfo(ANode.Parent.Data)
            .FSubItems.IndexOfObject(ANode);
          TdxItemInfo(ANode.Parent.Data).FSubItemImages[AIndex] :=
            Pointer(StrToInt(edImageIndex.Text));
        end;
      end;
    except
      ConvertError(edImageIndex);
      raise;
    end;
end;

procedure TfmListViewItems.edStateImageIndexExit(Sender: TObject);
var
  ANode: TTreeNode;
begin
  if ActiveControl <> btCancel then
    try
      ANode := tvItems.Selected;
      if (ANode <> nil) and (ANode.Data <> nil) then
        TdxItemInfo(ANode.Data).FStateIndex := StrToInt(edStateImageIndex.Text);
    except
      ConvertError(edStateImageIndex);
      raise;
    end;
end;

procedure TfmListViewItems.btApplyClick(Sender: TObject);
var
  ANode: TTreeNode;
  ListItem: TdxListItem;
begin
  FlushControls;
  Items.BeginUpdate;
  try
    Items.Clear;
    if tvItems.Items.Count > 0 then
      ANode := tvItems.Items[0]
    else
      ANode := nil;
    while ANode <> nil do
    begin
      if ANode.Data <> nil then
      begin
        ListItem := Items.Add;
        GetItem(TdxItemInfo(ANode.Data), ListItem);
        ANode := ANode.GetNextSibling;
      end
    end;
  finally
    Items.EndUpdate;
  end;
  btApply.Enabled := False;
end;

procedure TfmListViewItems.tvItemsDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  ANode, SelNode: TTreeNode;
  Child: Boolean;
begin
  Child := GetKeyState(VK_SHIFT) < 0;
  SelNode := tvItems.Selected;
  ANode := tvItems.GetNodeAt(X, Y);
  if ANode = nil then
    ANode := tvItems.DropTarget;
  if ANode = nil then
    Accept := False
  else
    Accept := (SelNode <> ANode) and not(Child and SelNode.HasAsParent(ANode))
      and not ANode.HasAsParent(SelNode) and
      not(SelNode.HasChildren and (ANode.Data = nil)) and
      not(Child and (ANode.Data <> nil) and SelNode.HasChildren);
end;

procedure TfmListViewItems.tvItemsDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var
  ANode, SelNode: TTreeNode;
  Child: Boolean;

  procedure AddItem(Source, Dest: TTreeNode);
  var
    AItemInfoSubs: TStrings;
  begin
    // dropping from child to child
    if (Source.Data = nil) and (Dest.Data = nil) then
    begin
      // remove child from parent list
      AItemInfoSubs := TdxItemInfo(Source.Parent.Data).FSubItems;
      AItemInfoSubs.Delete(AItemInfoSubs.IndexOfObject(Source));
      // move child to new parent
      Source.MoveTo(Dest, naAdd);
      // add child to new parent list
      TdxItemInfo(Dest.Parent.Data).FSubItems.AddObject(Source.Text, Source);
    end
    // dropping from a parent to a parent
    else if (Source.Data <> nil) and (Dest.Data <> nil) then
    begin
      if not Child then
        Source.MoveTo(Dest, naInsert)
      else
      begin
        TdxItemInfo(Source.Data).Free;
        Source.Data := nil;
        // make Source a child of Dest
        Source.MoveTo(Dest, naAddChild);
        // add child to new parent list
        TdxItemInfo(Dest.Data).FSubItems.AddObject(Source.Text, Source);
      end;
    end
    // dropping from parent to child
    else if (Source.Data <> nil) and (Dest.Data = nil) then
    begin
      // remove Source's parent node data
      TdxItemInfo(Source.Data).Free;
      Source.Data := nil;
      // make Source a child of Dest
      Source.MoveTo(Dest, naAdd);
      // add child to new parent list
      TdxItemInfo(Dest.Parent.Data).FSubItems.AddObject(Source.Text, Source);
    end
    // dropping from child to parent
    else if (Source.Data = nil) and (Dest.Data <> nil) then
    begin
      // remove child from parent list
      AItemInfoSubs := TdxItemInfo(Source.Parent.Data).FSubItems;
      AItemInfoSubs.Delete(AItemInfoSubs.IndexOfObject(Source));
      if Child then
      begin
        // move child to new parent
        Source.MoveTo(Dest, naAddChild);
        // add child to new parent list
        TdxItemInfo(Dest.Data).FSubItems.AddObject(Source.Text, Source);
      end
      else
      begin
        // move child to be sibling of Dest
        Source.MoveTo(Dest, naInsert);
        // create Parent ANode item info for Source
        Source.Data := TdxItemInfo.Create(nil);
      end;
    end;
  end;

begin
  with tvItems do
  begin
    SelNode := Selected;
    if (SelNode <> nil) then
    begin
      Child := GetKeyState(VK_SHIFT) < 0;
      ANode := tvItems.DropTarget; // GetNodeAt(X, Y);
      if ANode = nil then
      begin
        ANode := Items[Items.Count - 1];
        while (ANode <> nil) and not ANode.IsVisible do
          ANode := ANode.GetPrev;
      end;
      if ANode <> nil then
        try
          if Child and (ANode.Parent <> nil) then
            ANode := ANode.Parent;
          FDropping := True;
          AddItem(SelNode, ANode);
          SelNode.Selected := True;
          btApply.Enabled := True;
        finally
          FDropping := False;
        end;
    end;
  end;
end;

procedure TfmListViewItems.tvItemsEdited(Sender: TObject; Node: TTreeNode;
  var S: string);
begin
  edCaption.Text := S;
  edCaptionExit(nil);
  btNewItem.Default := True;
end;

procedure TfmListViewItems.btHelpClick(Sender: TObject);
begin
  Application.HelpContext(HelpContext);
end;

procedure TfmListViewItems.tvItemsKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  TdxUIThreadSyncService.EnqueueInvokeInUIThread(Self, UpdateCaption);
end;

procedure TfmListViewItems.UpdateCaption;
var
  AItemInfoSubs: TStrings;
  ATextBuffer: array [0 .. 255] of char;
  AEditNode: TTreeNode;
  AEditHand: HWnd;
begin
  if tvItems.IsEditing then
  begin
    AEditNode := tvItems.Selected;
    AEditHand := TreeView_GetEditControl(tvItems.Handle);
    if (AEditHand = 0) or (AEditNode = nil) then
      Exit;
    GetWindowText(AEditHand, ATextBuffer, Length(ATextBuffer));
    if AEditNode.Data = nil then
    begin
      AItemInfoSubs := TdxItemInfo(AEditNode.Parent.Data).FSubItems;
      AItemInfoSubs[AItemInfoSubs.IndexOfObject(AEditNode)] := ATextBuffer;
    end
    else
      TdxItemInfo(AEditNode.Data).FCaption := ATextBuffer;
    edCaption.Text := ATextBuffer;
  end;
end;

procedure TfmListViewItems.tvItemsEditing(Sender: TObject; Node: TTreeNode;
  var AllowEdit: Boolean);
begin
  btNewItem.Default := False;
end;

procedure TfmListViewItems.FormCreate(Sender: TObject);
begin
  if SavedWidth <> 0 then
    Width := SavedWidth;
  if SavedHeight <> 0 then
    Height := SavedHeight;
  if SavedLeft <> 0 then
    Left := SavedLeft
  else
    Left := (Screen.Width - Width) div 2;
  if SavedTop <> 0 then
    Top := SavedTop
  else
    Top := (Screen.Height - Height) div 2;
end;

procedure TfmListViewItems.FormDestroy(Sender: TObject);
begin
  TdxUIThreadSyncService.Unsubscribe(Self);
  SavedWidth := Width;
  SavedHeight := Height;
  SavedLeft := Left;
  SavedTop := Top;
end;

end.
