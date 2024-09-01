{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2021                                      }
{            Email : info@tmssoftware.com                            }
{            Web : https://www.tmssoftware.com                       }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit WEBLib.TMSFNCBitmapContainerEditor;

{$I WEBLib.TMSFNCDefines.inc}

{$IFDEF FMXLIB}
{$WARNINGS OFF}
{$HINTS OFF}
{$IF COMPILERVERSION > 30}
{$DEFINE DELPHIBERLIN}
{$IFEND}
{$HINTS ON}
{$WARNINGS ON}
{$ENDIF}

{$IFDEF FMXLIB}
{$DEFINE FMXVCLLIB}
{$ENDIF}
{$IFDEF VCLLIB}
{$DEFINE FMXVCLLIB}
{$DEFINE VCLWEBLIB}
{$ENDIF}
{$IFDEF CMNLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}
{$IFDEF WEBLIB}
{$DEFINE CMNWEBLIB}
{$DEFINE VCLWEBLIB}
{$ENDIF}

interface

uses
  Classes, WEBLib.TMSFNCCustomComponent, WEBLib.TMSFNCCustomControl, WEBLib.Controls, WEBLib.Graphics,
  WEBLib.TMSFNCTypes, WEBLib.StdCtrls, WEBLib.ExtCtrls, WEBLib.TMSFNCBitmapContainer, WEBLib.TMSFNCURLBitmapContainer, WEBLib.Forms, WEBLib.TMSFNCGraphics,
  WEBLib.TMSFNCGraphicsTypes, WEBLib.TMSFNCEditorListView, WEBLib.TMSFNCEditorButton, WEBLib.TMSFNCEditorPanel, WEBLib.TMSFNCEditorsTools
  {$IFDEF FMXLIB}
  ,FMX.Types, FMX.Memo, FMX.ListBox, FMX.Layouts, FMX.ComboEdit
  {$ENDIF}
  {$IFNDEF LCLLIB}
  {$IFNDEF WEBLIB}
  ,UITypes
  {$ENDIF}
  ,Types
  {$ENDIF}
  ;

resourcestring
  sTMSFNCBitmapContainerEditorOK = 'OK';
  sTMSFNCBitmapContainerEditorCancel = 'Cancel';
  sTMSFNCBitmapContainerEditorFill = '&Fill';
  sTMSFNCBitmapContainerEditorAppend = '&Append';
  sTMSFNCBitmapContainerEditorClear = '&Clear';
  sTMSFNCBitmapContainerEditorFillHint = 'Fill list with multiple items';
  sTMSFNCBitmapContainerEditorAppendHint = 'Add items to the list';
  sTMSFNCBitmapContainerEditorClearHint = 'Clear all items from the list';
  sTMSFNCBitmapContainerEditorDeleteHint = 'Delete Bitmap Item(s)';
  sTMSFNCBitmapContainerEditorAddHint = 'Add new Bitmap Item';
  sTMSFNCBitmapContainerEditorDuplicateHint = 'Duplicate Bitmap Item(s)';


const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : First Release

  TTMSFNCDELETEBITMAPITEM = {$IFDEF WEBLIB}'data:image/png;base64,' + {$ENDIF} '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';

  TTMSFNCADDBITMAPITEM = {$IFDEF WEBLIB}'data:image/png;base64,' + {$ENDIF} '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';

  TTMSFNCDUPLICATEBITMAPITEM = {$IFDEF WEBLIB}'data:image/png;base64,' + {$ENDIF} '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';

type
  TTMSFNCBitmapContainerEditor = class;

  {$IFDEF FMXLIB}
  TTMSFNCBitmapContainerEditorParent = TFmxObject;
  TTMSFNCBitmapContainerEditorComboBox = class(TComboEdit);
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  TTMSFNCBitmapContainerEditorParent = TWinControl;
  TTMSFNCBitmapContainerEditorComboBox = class(TComboBox);
  {$ENDIF}

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsDesktop)]
  {$ENDIF}
  TTMSFNCBitmapContainerEditor = class(TTMSFNCCustomComponent)
  private
    FImgMargin : Single;
    FListItemHeight: Single;
    frm: TTMSFNCCustomDesignerForm;
    FPanel, FListPanel, FTopPanel: TTMSFNCEditorPanel;
    FCopyBitmapContainer: TTMSFNCBitmapContainer;
    FButtonOk, FButtonCancel: TTMSFNCEditorButton;
    FButtonFill, FButtonAppend, FButtonClear: TTMSFNCEditorButton;
    FButtonDelete, FButtonAdd, FButtonDuplicate: TTMSFNCEditorButton;
    FBitmapList: TTMSFNCBitmapEditorListView;
    FBitmapContainer: TTMSFNCBitmapContainer;
    {$IFNDEF FMXLIB}
    FVScrlBox: TScrollBox;
    {$ENDIF}
    {$IFDEF FMXLIB}
    FVScrlBox: TVertScrollBox;
    {$ENDIF}
    procedure SetBitmapContainer(const Value: TTMSFNCBitmapContainer);
    procedure AssignBitmapListItems;
    procedure AssignControlItems;
    procedure SetBitmapItemProperties(AFrom, ATo: TTMSFNCBitmapItem); overload;
    procedure SetBitmapItemProperties(AFrom: TTMSFNCEditorListItem; ATo: TTMSFNCBitmapItem); overload;
    procedure SetEditorListItemProperties(AFrom: TTMSFNCBitmapItem; ATo: TTMSFNCEditorListItem);
    procedure SetItemButtonAppearance(AButton: TTMSFNCEditorButton);
    procedure SetActionButtonAppearance(AButton: TTMSFNCEditorButton);
    procedure EnableItemButtons(AEnabled: Boolean);
    {$IFDEF LCLLIB}
    procedure DoDropFiles(Sender: TObject; const FileNames: array of String);
    {$ENDIF}
  protected
    function GetInstance: NativeUInt; override;
    procedure UpdateBitmapList;
    procedure UpdateBitmapContainer;
    procedure DoBitmapListItemsChanged(Sender: TObject);
    procedure DoBitmapListItemSelectChanged(Sender: TObject; AIndex: Integer; AItem: TTMSFNCEditorListItem; ASelected: Boolean);
    procedure DoCopyBitmapChange(Sender: TObject);
    procedure RegisterRuntimeClasses; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure BuildEditor(AParent: TTMSFNCBitmapContainerEditorParent); virtual;
    procedure DoURLBitmapDownloadComplete(Sender: TObject; ItemIndex: integer);
    procedure DoAddItem(Sender: TObject); virtual;
    procedure DoFillItems(Sender: TObject); virtual; {$IFDEF WEBLIB}async;{$ENDIF}
    procedure DoDeleteItems(Sender: TObject); virtual;
    procedure DoDuplicateItems(Sender: TObject); virtual;
    procedure DoAppendItems(Sender: TObject); virtual; {$IFDEF WEBLIB}async;{$ENDIF}
    procedure DoClearItems(Sender: TObject); virtual;
    {$IFNDEF FMXLIB}
    procedure DoFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    {$ENDIF}
    {$IFDEF FMXLIB}
    procedure DoFormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
    {$ENDIF}
    {$IFDEF WEBLIB}
    procedure DoButtonOKClick(Sender: TObject); virtual;
    procedure DoButtonCancelClick(Sender: TObject); virtual;
    {$ENDIF}
    procedure SetControlProperties(AParent: TTMSFNCBitmapContainerEditorParent); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF WEBLIB}
    procedure Execute(AProc: TModalResultProc = nil);
    {$ELSE}
    function Execute: TModalResult;
    {$ENDIF}
    procedure Assign(Source: TPersistent); override;
  published
    property BitmapContainer: TTMSFNCBitmapContainer read FBitmapContainer write SetBitmapContainer;
  end;

  TTMSFNCBitmapContainerEditorForm = class(TTMSFNCCustomDesignerForm)
  {$IFDEF WEBLIB}
  private
    FBitmapContainerEditor: TTMSFNCBitmapContainerEditor;
  protected
    procedure Loaded; override;
    procedure LoadDFMValues; override;
  public
    constructor CreateDialogNew(ABitmapContainerEditor: TTMSFNCBitmapContainerEditor; ACaption: string); reintroduce; virtual;
  {$ENDIF}
  end;

implementation

uses
  WEBLib.TMSFNCUtils, SysUtils, WEBLib.Dialogs, Math
  {$IFDEF WEBLIB}
  ,WEBLib.WebTools
  {$ENDIF}
  {$IFNDEF FMXMOBILE}
  {$IFDEF DELPHIBERLIN}
  ,FMX.DialogService.Sync
  {$ENDIF}
  {$ENDIF}
  ;

type
  TTMSFNCURLBitmapContainerOpen = class(TTMSFNCURLBitmapContainer);
  {$IFNDEF WEBLIB}
  TTMSFNCBitmapContainerOpenDialog = TOpenDialog;
  {$ENDIF}
  {$IFDEF WEBLIB}
  TTMSFNCBitmapContainerOpenDialog = TWebOpenDialog;
  {$ENDIF}

{ TTMSFNCBitmapContainerEditor }

function TranslateTextEx(AText: String): String;
begin
  {$IFDEF FMXLIB}
  Result := TranslateText(AText);
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  Result := AText;
  {$ENDIF}
end;

procedure TTMSFNCBitmapContainerEditor.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FBitmapContainer) then
    FBitmapContainer := nil;
end;

procedure TTMSFNCBitmapContainerEditor.RegisterRuntimeClasses;
begin
  inherited;
  RegisterClass(TTMSFNCBitmapContainerEditor);
end;

procedure TTMSFNCBitmapContainerEditor.Assign(Source: TPersistent);
begin
  if (Source is TTMSFNCBitmapContainerEditor) then
    FBitmapContainer := (Source as TTMSFNCBitmapContainerEditor).BitmapContainer;
end;

procedure TTMSFNCBitmapContainerEditor.AssignBitmapListItems;
var
  bli: TTMSFNCEditorListItem;
  I: Integer;
begin
  FBitmapList.BeginUpdate;
  FBitmapList.Items.Clear;
  for I := 0 to FBitmapContainer.Items.Count - 1 do
  begin
    bli := FBitmapList.Items.Add;
    SetEditorListItemProperties(FBitmapContainer.Items[i], bli);
  end;
  FBitmapList.EndUpdate;
end;

procedure TTMSFNCBitmapContainerEditor.AssignControlItems;
var
  bi: TTMSFNCBitmapItem;
  I: Integer;
begin
  FBitmapContainer.BeginUpdate;
  FBitmapContainer.Items.Clear;
  for I := 0 to FBitmapList.Items.Count - 1 do
  begin
    bi := FBitmapContainer.Items.Add;
    SetBitmapItemProperties(FBitmapList.Items[I], bi);
  end;
  FBitmapContainer.EndUpdate;
end;

procedure TTMSFNCBitmapContainerEditor.BuildEditor(AParent: TTMSFNCBitmapContainerEditorParent);
{$IFNDEF WEBLIB}
var
  bs: TBytesStream;
{$ENDIF}
begin
  if Assigned(AParent) then
  begin
    FTopPanel := TTMSFNCEditorPanel.Create(AParent);
    {$IFDEF FMXLIB}
    FTopPanel.Align := TAlignLayout.Top;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FTopPanel.Align := alTop;
    FTopPanel.Top := 0;
    {$ENDIF}
    FTopPanel.Parent := AParent;
    FTopPanel.Height := 37;
    {$IFDEF CMNLIB}
    FTopPanel.PanelPosition := eppCenter;
    {$ENDIF}
    {$IFNDEF CMNLIB}
    FTopPanel.PanelPosition := eppTop;
    {$ENDIF}
    FTopPanel.Appearance.StrokeSides := [gsLeft, gsTop, gsRight];
    SetEditorBackPanelAppearance(FTopPanel);

    FButtonDelete := TTMSFNCEditorButton.Create(FTopPanel);
    FButtonDelete.Parent := FTopPanel;
    {$IFDEF WEBLIB}
    FButtonDelete.Bitmap.LoadFromResource(TTMSFNCDELETEBITMAPITEM, HInstance);
    {$ENDIF}
    {$IFNDEF WEBLIB}
    bs := TBytesStream.Create(TTMSFNCUtils.Decode64ToBytes(TTMSFNCDELETEBITMAPITEM));
    try
      FButtonDelete.Bitmap.LoadFromStream(bs);
    finally
      bs.Free
    end;
    {$ENDIF}
    FButtonDelete.OnClick := DoDeleteItems;
    FButtonDelete.Hint := sTMSFNCBitmapContainerEditorDeleteHint;

    FButtonDuplicate := TTMSFNCEditorButton.Create(FTopPanel);
    FButtonDuplicate.Parent := FTopPanel;
    {$IFDEF WEBLIB}
    FButtonDuplicate.Bitmap.LoadFromResource(TTMSFNCDUPLICATEBITMAPITEM, HInstance);
    {$ENDIF}
    {$IFNDEF WEBLIB}
    bs := TBytesStream.Create(TTMSFNCUtils.Decode64ToBytes(TTMSFNCDUPLICATEBITMAPITEM));
    try
      FButtonDuplicate.Bitmap.LoadFromStream(bs);
    finally
      bs.Free
    end;
    {$ENDIF}
    FButtonDuplicate.OnClick := DoDuplicateItems;
    FButtonDuplicate.Hint := sTMSFNCBitmapContainerEditorDuplicateHint;

    FButtonAdd := TTMSFNCEditorButton.Create(FTopPanel);
    FButtonAdd.Parent := FTopPanel;
    {$IFDEF WEBLIB}
    FButtonAdd.Bitmap.LoadFromResource(TTMSFNCADDBITMAPITEM, HInstance);
    {$ENDIF}
    {$IFNDEF WEBLIB}
    bs := TBytesStream.Create(TTMSFNCUtils.Decode64ToBytes(TTMSFNCADDBITMAPITEM));
    try
      FButtonAdd.Bitmap.LoadFromStream(bs);
    finally
      bs.Free
    end;
    {$ENDIF}
    FButtonAdd.OnClick := DoAddItem;
    FButtonAdd.Hint := sTMSFNCBitmapContainerEditorAddHint;

    FButtonFill := TTMSFNCEditorButton.Create(FTopPanel);
    FButtonFill.Parent := FTopPanel;
    FButtonFill.Text := TranslateTextEx(sTMSFNCBitmapContainerEditorFill);
    FButtonFill.OnClick := DoFillItems;
    FButtonFill.Hint := sTMSFNCBitmapContainerEditorFillHint;

    FButtonClear := TTMSFNCEditorButton.Create(FTopPanel);
    FButtonClear.Parent := FTopPanel;
    FButtonClear.Text := TranslateTextEx(sTMSFNCBitmapContainerEditorClear);
    FButtonClear.OnClick := DoClearItems;
    FButtonClear.Hint := sTMSFNCBitmapContainerEditorClearHint;

    FButtonAppend := TTMSFNCEditorButton.Create(FTopPanel);
    FButtonAppend.Parent := FTopPanel;
    FButtonAppend.Text := TranslateTextEx(sTMSFNCBitmapContainerEditorAppend);
    FButtonAppend.OnClick := DoAppendItems;
    FButtonAppend.Hint := sTMSFNCBitmapContainerEditorAppendHint;

    FListPanel := TTMSFNCEditorPanel.Create(AParent);
    FListPanel.Parent := AParent;
    {$IFDEF FMXLIB}
    FListPanel.Align := TAlignLayout.Client;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FListPanel.Align := alClient;
    {$ENDIF}
    FListPanel.Appearance.StrokeSides := [gsLeft, gsRight];
    FListPanel.Appearance.Stroke.Kind := gskNone;
    SetEditorSubPanelAppearance(FListPanel);

    {$IFDEF FMXLIB}
    FVScrlBox := TVertScrollBox.Create(FListPanel);
    FVScrlBox.Align := TAlignLayout.Client;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FVScrlBox := TScrollBox.Create(FListPanel);
    FVScrlBox.Align := alClient;
    FVScrlBox.Top := 0;
    {$IFDEF LCLLIB}
    FVScrlBox.HorzScrollBar.Visible:= False;
    {$ENDIF}
    {$ENDIF}
    FVScrlBox.Parent := FListPanel;
    {$IFDEF CMNWEBLIB}
    FVScrlBox.BorderStyle := bsNone;
    if IsLightTheme then
      FVScrlBox.Color := EDITORSUBBACKCOLORLIGHT
    else
      FVScrlBox.Color := EDITORSUBBACKCOLORDARK;
    {$ENDIF}

    FBitmapList := TTMSFNCBitmapEditorListView.Create(FListPanel);
    FBitmapList.Parent := FVScrlBox;

    FPanel := TTMSFNCEditorPanel.Create(AParent);
    {$IFDEF FMXLIB}
    FPanel.Align := TAlignLayout.Bottom;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FPanel.Align := alBottom;
    {$ENDIF}
    FPanel.Height := 37;
    FPanel.Parent := AParent;
    {$IFDEF CMNLIB}
    FPanel.PanelPosition := eppCenter;
    {$ENDIF}
    {$IFNDEF CMNLIB}
    FPanel.PanelPosition := eppBottom;
    {$ENDIF}

    FPanel.Appearance.StrokeSides := [gsLeft, gsRight, gsBottom];
    SetEditorBackPanelAppearance(FPanel);

    FButtonCancel := TTMSFNCEditorButton.Create(FPanel);
    FButtonCancel.ModalResult := mrCancel;
    FButtonCancel.Parent := FPanel;
    FButtonCancel.Text := TranslateTextEx(sTMSFNCBitmapContainerEditorCancel);
    {$IFDEF WEBLIB}
    FButtonCancel.OnClick := DoButtonCancelClick;
    {$ENDIF}

    FButtonOK := TTMSFNCEditorButton.Create(FPanel);
    FButtonOK.ModalResult := mrOk;
    FButtonOK.Parent := FPanel;
    FButtonOK.Text := TranslateTextEx(sTMSFNCBitmapContainerEditorOK);
    {$IFDEF WEBLIB}
    FButtonOK.OnClick := DoButtonOKClick;
    {$ENDIF}

    {$IFNDEF WEBLIB}
    SetControlProperties(AParent);
    {$ENDIF}
  end;
end;

{$IFDEF WEBLIB}
procedure TTMSFNCBitmapContainerEditor.DoButtonOKClick(Sender: TObject);
begin
  if Assigned(frm) then
    frm.ModalResult := mrOK;
end;

procedure TTMSFNCBitmapContainerEditor.DoButtonCancelClick(Sender: TObject);
begin
  if Assigned(frm) then
    frm.ModalResult := mrCancel;
end;
{$ENDIF}

constructor TTMSFNCBitmapContainerEditor.Create(AOwner: TComponent);
begin
  inherited;
  FListItemHeight := 60;
  FImgMargin := 5;
  FCopyBitmapContainer := TTMSFNCBitmapContainer.Create;
end;

destructor TTMSFNCBitmapContainerEditor.Destroy;
begin
  FreeAndNil(FCopyBitmapContainer);
  inherited;
end;

procedure TTMSFNCBitmapContainerEditor.DoAddItem(Sender: TObject);
var
  i: Integer;
  sy: single;
begin
  if (FBitmapList.LastSelectedItemIndex >= 0) and (FBitmapList.LastSelectedItemIndex < FBitmapList.Items.Count - 1) and FBitmapList.Items[FBitmapList.LastSelectedItemIndex].Selected then
  begin
    i := FBitmapList.LastSelectedItemIndex + 1;
    FBitmapContainer.Items.Insert(i);
  end
  else
  begin
    FBitmapContainer.Items.Add;
    i := FBitmapContainer.Items.Count - 1;
  end;

  UpdateBitmapList;
  FBitmapList.SelectItem(i);

  {$IFDEF WEBLIB}
  if FVScrlBox.VertScrollBar.IsScrollBarVisible then
    FBitmapList.Width := FVScrlBox.Width - 18
  else
    FBitmapList.Width := FVScrlBox.Width;
  {$ENDIF}

  {$IFDEF FMXLIB}
  sy := FVScrlBox.ViewportPosition.Y + FVScrlBox.Height - 100;
  {$ENDIF}
  {$IFDEF VCLLIB}
  sy := FVScrlBox.VertScrollBar.Position + FVScrlBox.Height - 100;
  {$ENDIF}

  {$IFNDEF WEBLIB}
  if FBitmapList.Items[I].DrawRect.Bottom >=  sy then
    {$IFDEF FMXLIB}
    FVScrlBox.ViewportPosition := PointF(0, FBitmapList.Items[I].DrawRect.Bottom - FVScrlBox.Height);
    {$ENDIF}
    {$IFDEF CMNLIB}
    FVScrlBox.VertScrollBar.Position := Round(FBitmapList.Items[I].DrawRect.Bottom - FVScrlBox.Height);
    {$ENDIF}
  {$ENDIF}
end;

procedure TTMSFNCBitmapContainerEditor.DoBitmapListItemsChanged(Sender: TObject);
begin
  UpdateBitmapContainer;
end;

procedure TTMSFNCBitmapContainerEditor.DoBitmapListItemSelectChanged(Sender: TObject; AIndex: Integer; AItem: TTMSFNCEditorListItem; ASelected: Boolean);
begin
  if ASelected or (Length(FBitmapList.GetSelectedItems) > 0)then
    EnableItemButtons(True)
  else
    EnableItemButtons(False);
end;

procedure TTMSFNCBitmapContainerEditor.DoClearItems(Sender: TObject);
begin
  FBitmapContainer.Items.Clear;
  UpdateBitmapList;
end;

procedure TTMSFNCBitmapContainerEditor.DoCopyBitmapChange(Sender: TObject);
begin
  if Assigned(FBitmapList) then
    FBitmapList.Invalidate;
end;

procedure TTMSFNCBitmapContainerEditor.DoDeleteItems(Sender: TObject);
begin
  FBitmapList.DeleteSelectedItems;
  FBitmapList.Height := Max(FBitmapList.GetListHeight, FVScrlBox.Height);
  UpdateBitmapContainer;
end;

procedure TTMSFNCBitmapContainerEditor.DoDuplicateItems(Sender: TObject);
var
  i, li, bi: integer;
  bci: TTMSFNCBitmapItem;
begin
  li := -1;

  if (FBitmapList.LastSelectedItemIndex >= 0) and (FBitmapList.LastSelectedItemIndex <= FBitmapList.Items.Count - 1) and FBitmapList.Items[FBitmapList.LastSelectedItemIndex].Selected then
  begin
    li := FBitmapList.LastSelectedItemIndex;
    bi := li;
    if (FBitmapList.LastSelectedItemIndex < FBitmapList.Items.Count - 1) then
      bci := FBitmapContainer.Items.Insert(li + 1)
    else
      bci := FBitmapContainer.Items.Add;

    SetBitmapItemProperties(FBitmapContainer.Items[li], bci);
    bci.Name := bci.Name + '1';
    inc(bi);
  end
  else
  begin
    bi := FBitmapList.Items.Count - 1;
  end;

  for I := Length(FBitmapList.GetSelectedItems) - 1 downto 0 do
  begin
    if FBitmapList.GetSelectedItems[I].Index <> li then
    begin
      bci := FBitmapContainer.Items.Insert(bi + 1);
      SetBitmapItemProperties(FBitmapList.GetSelectedItems[I], bci);
      bci.Name := bci.Name + '1';
      Inc(bi);
    end;
  end;

  UpdateBitmapList;
  FBitmapList.SelectItem(bi);
end;

procedure TTMSFNCBitmapContainerEditor.DoFillItems(Sender: TObject);
var
  od: TTMSFNCBitmapContainerOpenDialog;
  I: Integer;
  str: string;
  bi: TTMSFNCBitmapItem;
  {$IFDEF WEBLIB}
  res: Boolean;
  {$ENDIF}
begin
  if not Assigned(FBitmapContainer) then
    Exit;

  od := TTMSFNCBitmapContainerOpenDialog.Create(Self);
  try
    od.Options := od.Options + [TOpenOption.ofAllowMultiSelect];
    {$IFNDEF WEBLIB}
    od.Title := 'Fill Bitmap Container';
    od.Filter := 'All (*.gif;*.png;*.jpg;*.jpeg;*.bmp;*.tif;*.tiff;*.ico;*.emf;*.wmf;*.svg)|*.gif;'+
    '*.png;*.jpg;*.jpeg;*.bmp;*.tif;*.tiff;*.ico;*.emf;*.wmf;*.svg|GIF Image (*.gif)|*.gif|Portable Network Graphics (*.png)|*.png|'+
    'JPEG Image File (*.jpg)|*.jpg|JPEG Image File (*.jpeg)|*.jpeg|Bitmaps (*.bmp)|*.bmp|TIFF Images (*.tif)|*.tif|TIFF Images'+
    '(*.tiff)|*.tiff|Icons (*.ico)|*.ico|Enhanced Metafiles (*.emf)|*.emf|Metafiles (*.wmf)|*.wmf|Scalable Vector Graphics (*.svg)|*.svg';
    od.FilterIndex := 0;
    {$ENDIF}
    {$IFNDEF WEBLIB}
    if od.Execute then
    {$ENDIF}
    {$IFDEF WEBLIB}
    res := await(Boolean, od.Perform);
    if res then
    {$ENDIF}
    begin
    FBitmapContainer.BeginUpdate;
    FBitmapContainer.Items.Clear;

    for I := 0 to od.Files.Count - 1 do
    begin
      bi := FBitmapContainer.Items.Add;
      str := od.Files[I]{$IFDEF WEBLIB}.Name{$ENDIF};
      bi.Name := ExtractFileName(str);

      {$IFNDEF WEBLIB}
      str := od.Files[I];
      bi.Bitmap.LoadFromFile(str);
      {$ENDIF}
      {$IFDEF WEBLIB}
      str := await(string, od.Files[I].FileAsBase64);
      str := 'data:' + od.Files[I].MimeType + ';base64,' + str;
      bi.Bitmap.LoadFromResource(str);
      {$ENDIF}
    end;
    FBitmapContainer.EndUpdate;
    UpdateBitmapList;
  end;
  finally
    od.Free;
  end;
end;

{$IFNDEF FMXLIB}
procedure TTMSFNCBitmapContainerEditor.DoFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
{$ENDIF}
{$IFDEF FMXLIB}
procedure TTMSFNCBitmapContainerEditor.DoFormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char; Shift: TShiftState);
{$ENDIF}
begin
  inherited;

  if Key = KEY_ESCAPE then
  begin
    frm.Close;
  end
  else if Key = KEY_RETURN then
  begin
    if FBitmapList.EditMode then
    begin
      FBitmapList.EndEditMode(True);
    end
    else
    begin
      frm.ModalResult := mrOk;
      {$IFNDEF FMXLIB}
      frm.Close;
      {$ENDIF}
      {$IFDEF FMXLIB}
      frm.CloseModal;
      {$ENDIF}
    end;
  end;
end;

procedure TTMSFNCBitmapContainerEditor.DoAppendItems(Sender: TObject);
var
  od: TTMSFNCBitmapContainerOpenDialog;
  I: Integer;
  str: string;
  bi: TTMSFNCBitmapItem;
  {$IFDEF WEBLIB}
  res: Boolean;
  {$ENDIF}
begin
  if not Assigned(FBitmapContainer) then
    Exit;

  od := TTMSFNCBitmapContainerOpenDialog.Create(Self);
  try
    od.Options := od.Options + [TOpenOption.ofAllowMultiSelect];
    {$IFNDEF WEBLIB}
    od.Title := 'Append Bitmap Container';
    od.Filter := 'All (*.gif;*.png;*.jpg;*.jpeg;*.bmp;*.tif;*.tiff;*.ico;*.emf;*.wmf;*.svg)|*.gif;'+
    '*.png;*.jpg;*.jpeg;*.bmp;*.tif;*.tiff;*.ico;*.emf;*.wmf;*.svg|GIF Image (*.gif)|*.gif|Portable Network Graphics (*.png)|*.png|'+
    'JPEG Image File (*.jpg)|*.jpg|JPEG Image File (*.jpeg)|*.jpeg|Bitmaps (*.bmp)|*.bmp|TIFF Images (*.tif)|*.tif|TIFF Images'+
    '(*.tiff)|*.tiff|Icons (*.ico)|*.ico|Enhanced Metafiles (*.emf)|*.emf|Metafiles (*.wmf)|*.wmf|Scalable Vector Graphics (*.svg)|*.svg';
    od.FilterIndex := 0;
    {$ENDIF}
    {$IFNDEF WEBLIB}
    if od.Execute then
    {$ENDIF}
    {$IFDEF WEBLIB}
    res := await(Boolean, od.Perform);
    if res then
    {$ENDIF}
    begin
      FBitmapContainer.BeginUpdate;

      for I := 0 to od.Files.Count - 1 do
      begin
        bi := FBitmapContainer.Items.Add;
        str := od.Files[I]{$IFDEF WEBLIB}.Name{$ENDIF};
        bi.Name := ExtractFileName(str);

        {$IFNDEF WEBLIB}
        str := od.Files[I];
        bi.Bitmap.LoadFromFile(str);
        {$ENDIF}
        {$IFDEF WEBLIB}
        str := await(string, od.Files[I].FileAsBase64);
        str := 'data:' + od.Files[I].MimeType + ';base64,' + str;
        bi.Bitmap.LoadFromResource(str);
        {$ENDIF}
      end;
      FBitmapContainer.EndUpdate;
      UpdateBitmapList;
    end;
  finally
    od.Free;
  end;
end;

procedure TTMSFNCBitmapContainerEditor.DoURLBitmapDownloadComplete(Sender: TObject; ItemIndex: integer);
begin
  UpdateBitmapList;
end;

procedure TTMSFNCBitmapContainerEditor.EnableItemButtons(AEnabled: Boolean);
begin
  if Assigned(FButtonDelete) then
  begin
    FButtonDelete.Enabled := AEnabled;
    FButtonDelete.Invalidate;
  end;

  if Assigned(FButtonDuplicate) then
  begin
    FButtonDuplicate.Enabled := AEnabled;
    FButtonDuplicate.Invalidate;
  end;
end;

{$IFDEF LCLLIB}
procedure TTMSFNCBitmapContainerEditor.DoDropFiles(Sender: TObject; const FileNames: array of String);
begin
  if Assigned(FBitmapList) then
     FBitmapList.AddDroppedFiles(Sender, FileNames);
end;
{$ENDIF}

{$IFDEF WEBLIB}
procedure TTMSFNCBitmapContainerEditor.Execute(AProc: TModalResultProc = nil);
{$ELSE}
function TTMSFNCBitmapContainerEditor.Execute: TModalResult;
{$ENDIF}
begin
  FCopyBitmapContainer.Assign(FBitmapContainer);
  {$IFDEF WEBLIB}
  frm := TTMSFNCBitmapContainerEditorForm.CreateDialogNew(Self, 'Bitmap Container Editor');
  {$ELSE}
  frm := TTMSFNCBitmapContainerEditorForm.CreateNew(Application);
  frm.Caption := 'Bitmap Container Editor';
  {$ENDIF}
  {$IFDEF CMNLIB}
  frm.KeyPreview := True;
  {$ENDIF}
  frm.OnKeyDown := DoFormKeyDown;

  frm.Width := 380;
  frm.Height := 442;
  {$IFDEF FMXLIB}
  frm.Position := TFormPosition.ScreenCenter;
  frm.Fill.Kind := TBrushKind.Solid;
  if IsLightTheme then
    frm.Fill.Color := EDITORSUBBACKCOLORLIGHT
  else
    frm.Fill.Color := EDITORSUBBACKCOLORDARK;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  {$IFDEF LCLLIB}
  frm.AllowDropFiles := True;
  frm.OnDropFiles := @DoDropFiles;
  {$ENDIF}
  frm.Position := poScreenCenter;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  BuildEditor(frm);
  frm.ActiveControl := FTopPanel;
  {$ENDIF}
  TTMSFNCUtils.ScaleForCurrentDPI(frm);
  {$IFNDEF WEBLIB}
  Result := frm.ShowModal;
  if (Result <> mrOk) and Assigned(BitmapContainer) then
  begin
    BitmapContainer.Assign(FCopyBitmapContainer);
  end;
  frm.Free;
  {$ELSE}
  frm.ShowModal(
  procedure(AResult: TModalResult)
  begin
    if (AResult <> mrOk) and Assigned(BitmapContainer) then
    begin
      BitmapContainer.Assign(FCopyBitmapContainer);
    end;
    frm := nil;
    if Assigned(AProc) then
      AProc(AResult);
  end);
  SetControlProperties(frm);
  {$ENDIF}
end;

function TTMSFNCBitmapContainerEditor.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;

procedure TTMSFNCBitmapContainerEditor.SetActionButtonAppearance(AButton: TTMSFNCEditorButton);
begin
  AButton.ShowHint := True;
  SetEditorTabButtonAppearance(AButton, EDITORPRIMCOLOR);
end;

procedure TTMSFNCBitmapContainerEditor.SetBitmapContainer(const Value: TTMSFNCBitmapContainer);
begin
  FBitmapContainer := Value;
end;

procedure TTMSFNCBitmapContainerEditor.SetBitmapItemProperties(AFrom: TTMSFNCEditorListItem; ATo: TTMSFNCBitmapItem);
begin
  if (ATo is TTMSFNCURLBitmapItem) then
    (ATo as TTMSFNCURLBitmapItem).URL := AFrom.DataString;

  ATo.Name := AFrom.Name;
  ATo.Tag := AFrom.Tag;
  ATo.Bitmap.Assign(AFrom.Bitmap);
end;

procedure TTMSFNCBitmapContainerEditor.SetControlProperties(AParent: TTMSFNCBitmapContainerEditorParent);
begin
  if Assigned(FButtonDelete) then
  begin
    SetItemButtonAppearance(FButtonDelete);
    {$IFDEF FMXLIB}
    FButtonDelete.Position.X := 40;
    FButtonDelete.Align := TAlignLayout.Right;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FButtonDelete.Left := 40;
    FButtonDelete.Align := alRight;
    {$IFNDEF LCLLIB}
    FButtonDelete.AlignWithMargins := True;
    {$ENDIF}
    {$ENDIF}
    {$IFDEF LCLLIB}
    FButtonDelete.BorderSpacing.Right := 5;
    FButtonDelete.BorderSpacing.Top := 5;
    FButtonDelete.BorderSpacing.Bottom := 5;
    FButtonDelete.BorderSpacing.Left := 5;
    {$ENDIF}
    {$IFNDEF LCLLIB}
    FButtonDelete.Margins.Right := 5;
    FButtonDelete.Margins.Top := 5;
    FButtonDelete.Margins.Bottom := 5;
    FButtonDelete.Margins.Left := 5;
    {$ENDIF}
  end;

  if Assigned(FButtonDuplicate) then
  begin
    SetItemButtonAppearance(FButtonDuplicate);
    {$IFDEF FMXLIB}
    FButtonDuplicate.Position.X := 30;
    FButtonDuplicate.Align := TAlignLayout.Right;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FButtonDuplicate.Left := 30;
    FButtonDuplicate.Align := alRight;
    {$IFNDEF LCLLIB}
    FButtonDuplicate.AlignWithMargins := True;
    {$ENDIF}
    {$ENDIF}
    {$IFDEF LCLLIB}
    FButtonDuplicate.BorderSpacing.Right := 5;
    FButtonDuplicate.BorderSpacing.Top := 5;
    FButtonDuplicate.BorderSpacing.Bottom := 5;
    FButtonDuplicate.BorderSpacing.Left := 5;
    {$ENDIF}
    {$IFNDEF LCLLIB}
    FButtonDuplicate.Margins.Right := 5;
    FButtonDuplicate.Margins.Top := 5;
    FButtonDuplicate.Margins.Bottom := 5;
    FButtonDuplicate.Margins.Left := 5;
    {$ENDIF}
  end;

  if Assigned(FButtonAdd) then
  begin
    SetItemButtonAppearance(FButtonAdd);
    {$IFDEF FMXLIB}
    FButtonAdd.Position.X := 10;
    FButtonAdd.Align := TAlignLayout.Right;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FButtonAdd.Left := 10;
    FButtonAdd.Align := alRight;
    {$IFNDEF LCLLIB}
    FButtonAdd.AlignWithMargins := True;
    {$ENDIF}
    {$ENDIF}
    {$IFDEF LCLLIB}
    FButtonAdd.BorderSpacing.Right := 5;
    FButtonAdd.BorderSpacing.Top := 5;
    FButtonAdd.BorderSpacing.Bottom := 5;
    FButtonAdd.BorderSpacing.Left := 5;
    {$ENDIF}
    {$IFNDEF LCLLIB}
    FButtonAdd.Margins.Right := 5;
    FButtonAdd.Margins.Top := 5;
    FButtonAdd.Margins.Bottom := 5;
    FButtonAdd.Margins.Left := 5;
    {$ENDIF}
  end;

  if Assigned(FButtonFill) then
  begin
    {$IFDEF FMXLIB}
    FButtonFill.Align := TAlignLayout.Left;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FButtonFill.Align := alLeft;
    FButtonFill.Left := 1;
    {$IFNDEF LCLLIB}
    FButtonFill.AlignWithMargins := True;
    {$ENDIF}
    {$ENDIF}
    {$IFNDEF LCLLIB}
    FButtonFill.Margins.Right := 5;
    FButtonFill.Margins.Top := 5;
    FButtonFill.Margins.Bottom := 5;
    FButtonFill.Margins.Left := 5;
    {$ENDIF}
    {$IFDEF LCLLIB}
    FButtonFill.BorderSpacing.Right := 5;
    FButtonFill.BorderSpacing.Top := 5;
    FButtonFill.BorderSpacing.Bottom := 5;
    FButtonFill.BorderSpacing.Left := 5;
    {$ENDIF}

    SetActionButtonAppearance(FButtonFill);
  end;

  if Assigned(FButtonClear) then
  begin
    {$IFDEF FMXLIB}
    FButtonClear.Position.X := 20;
    FButtonClear.Align := TAlignLayout.Left;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FButtonClear.Left := 20;
    FButtonClear.Align := alLeft;
    {$IFNDEF LCLLIB}
    FButtonClear.AlignWithMargins := True;
    {$ENDIF}
    {$ENDIF}
    {$IFDEF LCLLIB}
    FButtonClear.BorderSpacing.Right := 5;
    FButtonClear.BorderSpacing.Top := 5;
    FButtonClear.BorderSpacing.Bottom := 5;
    FButtonClear.BorderSpacing.Left := 5;
    {$ENDIF}
    {$IFNDEF LCLLIB}
    FButtonClear.Margins.Right := 5;
    FButtonClear.Margins.Top := 5;
    FButtonClear.Margins.Bottom := 5;
    FButtonClear.Margins.Left := 5;
    {$ENDIF}

    SetActionButtonAppearance(FButtonClear);
  end;

  if Assigned(FButtonAppend) then
  begin
    {$IFDEF FMXLIB}
    FButtonAppend.Position.X := 10;
    FButtonAppend.Align := TAlignLayout.Left;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FButtonAppend.Left := 10;
    FButtonAppend.Align := alLeft;
    {$IFNDEF LCLLIB}
    FButtonAppend.AlignWithMargins := True;
    {$ENDIF}
    {$ENDIF}
    {$IFDEF LCLLIB}
    FButtonAppend.BorderSpacing.Right := 0;
    FButtonAppend.BorderSpacing.Top := 5;
    FButtonAppend.BorderSpacing.Bottom := 5;
    FButtonAppend.BorderSpacing.Left := 0;
    {$ENDIF}
    {$IFNDEF LCLLIB}
    FButtonAppend.Margins.Right := 0;
    FButtonAppend.Margins.Top := 5;
    FButtonAppend.Margins.Bottom := 5;
    FButtonAppend.Margins.Left := 0;
    {$ENDIF}

    SetActionButtonAppearance(FButtonAppend);
  end;

  if Assigned(FBitmapList) then
  begin
    {$IFDEF FMXLIB}
    FBitmapList.Align := TAlignLayout.Top;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FBitmapList.Align := alTop;
    {$IFDEF VCLLIB}
    FBitmapList.SetAcceptDrag(True);
    {$ENDIF}
    {$ENDIF}
    FBitmapList.BeginUpdate;
    AssignBitmapListItems;
    FBitmapList.EndUpdate;
    FBitmapList.Height := FBitmapList.GetListHeight;
    {$IFNDEF LCLLIB}
    FBitmapList.OnAddNewItem := DoAddItem;
    FBitmapList.OnItemsChanged := DoBitmapListItemsChanged;
    FBitmapList.OnItemSelectedChanged := DoBitmapListItemSelectChanged;
    {$ENDIF}
    {$IFDEF LCLLIB}
    FBitmapList.OnAddNewItem := @DoAddItem;
    FBitmapList.OnItemsChanged := @DoBitmapListItemsChanged;
    FBitmapList.OnItemSelectedChanged := @DoBitmapListItemSelectChanged;
    {$ENDIF}

    if FBitmapContainer is TTMSFNCURLBitmapContainer then
    begin
      {$IFNDEF LCLLIB}
      TTMSFNCURLBitmapContainerOpen((FBitmapContainer as TTMSFNCURLBitmapContainer)).OnInternalDownloadComplete := DoURLBitmapDownloadComplete;
      {$ENDIF}
      {$IFDEF LCLLIB}
      TTMSFNCURLBitmapContainerOpen((FBitmapContainer as TTMSFNCURLBitmapContainer)).OnInternalDownloadComplete := @DoURLBitmapDownloadComplete;
      {$ENDIF}
      FBitmapList.DrawDataString := True;
    end
    else
      FBitmapList.DrawDataString := False;

    SetEditorBitmapListAppearance(FBitmapList, EDITORPRIMCOLOR, 5);

//    SetBitmapListAppearance;
  end;

  if Assigned(FButtonCancel) then
  begin
    {$IFDEF FMXLIB}
    FButtonCancel.Align := TAlignLayout.Right;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FButtonCancel.Align := alRight;
    FButtonCancel.Left := 100;
    {$IFNDEF LCLLIB}
    FButtonCancel.AlignWithMargins := True;
    {$ENDIF}
    {$ENDIF}
    {$IFDEF LCLLIB}
    FButtonCancel.BorderSpacing.Right := 5;
    FButtonCancel.BorderSpacing.Top := 5;
    FButtonCancel.BorderSpacing.Bottom := 5;
    FButtonCancel.BorderSpacing.Left := 5;
    {$ENDIF}
    {$IFNDEF LCLLIB}
    FButtonCancel.Margins.Right := 5;
    FButtonCancel.Margins.Top := 5;
    FButtonCancel.Margins.Bottom := 5;
    FButtonCancel.Margins.Left := 5;
    {$ENDIF}

    SetEditorCancelButtonAppearance(FButtonCancel);
  end;

  if Assigned(FButtonOk) then
  begin
    {$IFDEF FMXLIB}
    FButtonOk.Align := TAlignLayout.Right;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FButtonOk.Align := alRight;
    FButtonCancel.Left := 99;
    {$IFNDEF LCLLIB}
    FButtonOK.AlignWithMargins := True;
    {$ENDIF}
    {$ENDIF}
    {$IFNDEF LCLLIB}
    FButtonOK.Margins.Right := 0;
    FButtonOK.Margins.Top := 5;
    FButtonOK.Margins.Bottom := 5;
    FButtonOK.Margins.Left := 5;
    {$ENDIF}
    {$IFDEF LCLLIB}
    FButtonOK.BorderSpacing.Right := 0;
    FButtonOK.BorderSpacing.Top := 5;
    FButtonOK.BorderSpacing.Bottom := 5;
    FButtonOK.BorderSpacing.Left := 5;
    {$ENDIF}

    SetEditorOKButtonAppearance(FButtonOk);
  end;

  EnableItemButtons(False);

  Invalidate;
end;

procedure TTMSFNCBitmapContainerEditor.SetBitmapItemProperties(AFrom, ATo: TTMSFNCBitmapItem);
begin
  if (AFrom is TTMSFNCURLBitmapItem) and (ATo is TTMSFNCURLBitmapItem) then
    (ATo as TTMSFNCURLBitmapItem).URL := (AFrom as TTMSFNCURLBitmapItem).URL;

  ATo.Name := AFrom.Name;
  ATo.Tag := AFrom.Tag;
  ATo.Bitmap.Assign(AFrom.Bitmap);
end;

procedure TTMSFNCBitmapContainerEditor.SetEditorListItemProperties(AFrom: TTMSFNCBitmapItem; ATo: TTMSFNCEditorListItem);
begin
  if AFrom is TTMSFNCURLBitmapItem then
    ATo.DataString := (AFrom as TTMSFNCURLBitmapItem).URL
  else
    ATo.DataString := '';

  ATo.Name := AFrom.Name;
  ATo.Tag := AFrom.Tag;
  ATo.Bitmap.Assign(AFrom.Bitmap);
end;

procedure TTMSFNCBitmapContainerEditor.SetItemButtonAppearance(AButton: TTMSFNCEditorButton);
begin
  AButton.Text := '';
  AButton.Width := 27;
  AButton.ShowHint := True;

  SetEditorIconButtonAppearance(AButton, EDITORPRIMCOLOR);
end;

procedure TTMSFNCBitmapContainerEditor.UpdateBitmapContainer;
begin
  FBitmapContainer.BeginUpdate;
  AssignControlItems;
  FBitmapContainer.EndUpdate;
end;

procedure TTMSFNCBitmapContainerEditor.UpdateBitmapList;
begin
  FBitmapList.OnItemsChanged := nil;
  FBitmapList.BeginUpdate;
  AssignBitmapListItems;
  FBitmapList.Height := Max(FBitmapList.GetListHeight, FVScrlBox.Height);
  FBitmapList.EndUpdate;
  {$IFNDEF LCLLIB}
  FBitmapList.OnItemsChanged := DoBitmapListItemsChanged;
  {$ENDIF}
  {$IFDEF LCLLIB}
  FBitmapList.OnItemsChanged := @DoBitmapListItemsChanged;
  {$ENDIF}
end;

{$IFDEF WEBLIB}

{ TTMSFNCBitmapContainerEditorForm }

constructor TTMSFNCBitmapContainerEditorForm.CreateDialogNew(ABitmapContainerEditor: TTMSFNCBitmapContainerEditor; ACaption: string);
begin
  FBitmapContainerEditor := ABitmapContainerEditor;
  inherited CreateDialogNew(ACaption);
end;

procedure TTMSFNCBitmapContainerEditorForm.LoadDFMValues;
begin
  inherited LoadDFMValues;
  if Assigned(FBitmapContainerEditor) then
  begin
    FBitmapContainerEditor.BuildEditor(Self);
    if ASsigned(FBitmapContainerEditor.FBitmapList) then
      ActiveControl := FBitmapContainerEditor.FBitmapList;
  end;
end;

procedure TTMSFNCBitmapContainerEditorForm.Loaded;
var
  css: string;
begin
  inherited;

  css := Application.IDECSS;

  if (css <> '') then
  begin
    AddCSS('IDECSS', css);
    ElementClassName := 'IDEBkg';
    CaptionElement['class'] := 'IDECaption IDEFont';
  end;
end;

{$ENDIF}

end.
