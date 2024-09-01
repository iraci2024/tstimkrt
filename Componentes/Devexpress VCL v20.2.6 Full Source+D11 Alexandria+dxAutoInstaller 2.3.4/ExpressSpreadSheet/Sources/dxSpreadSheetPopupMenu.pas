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

unit dxSpreadSheetPopupMenu;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Classes, Graphics, ImgList, Menus,
  dxCore, cxControls, cxGraphics,
  dxSpreadSheetCore, dxSpreadSheetUtils, dxSpreadSheetTypes, dxBuiltInPopupMenu,
  dxSpreadSheetHyperlinks,
  dxSpreadSheetProtection;

type

  { TdxSpreadSheetCustomPopupMenu }

  TdxSpreadSheetCustomPopupMenu = class(TComponent)
  strict private
    FImageList: TcxImageList;
    FPopupMenu: TPopupMenu;

    procedure AdapterClickHandler(Sender: TObject);
    function CreateMenuItem(AParent: TComponent = nil): TMenuItem;
    function GetSpreadSheet: TdxCustomSpreadSheet; inline;
    procedure Initialize;
  protected
    function AddImage(const AResourceName: string): Integer;
    function AddMenuItem(ACaption: Pointer; ACommandID: Word;
      AClickHandler: TNotifyEvent; AEnabled: Boolean = True;
      const AImageResName: string = ''; AParent: TComponent = nil): TComponent;
    procedure AddSeparator(AParent: TComponent = nil);
    function AddSubMenu(ACaption: Pointer): TComponent;

    function DoCustomPopup(const P: TPoint): Boolean; virtual;
    procedure PopulateMenuItems; virtual; abstract;

    property ImageList: TcxImageList read FImageList;
    property PopupMenu: TPopupMenu read FPopupMenu;
    property SpreadSheet: TdxCustomSpreadSheet read GetSpreadSheet;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Popup(const P: TPoint): Boolean; // for internal use only
  end;

  { TdxSpreadSheetBuiltInPageControlTabPopupMenu }

  TdxSpreadSheetBuiltInPageControlTabPopupMenu = class
    (TdxSpreadSheetCustomPopupMenu)
  public const
    cidDelete = 1;
    cidHide = 2;
    cidInsert = 3;
    cidRename = 4;
    cidUnhide = 5;
    cidProtect = 6;
    cidUnprotect = 7;
  protected
    function CanDelete: Boolean; virtual;
    function CanHide: Boolean; virtual;
    function CanInsert: Boolean; virtual;
    function CanRename: Boolean; virtual;
    function CanUnhide: Boolean; virtual;

    procedure DoDelete(Sender: TObject);
    procedure DoHide(Sender: TObject);
    procedure DoInsert(Sender: TObject);
    procedure DoProtect(Sender: TObject);
    procedure DoRename(Sender: TObject);
    procedure DoUnhide(Sender: TObject);
    procedure DoUnprotect(Sender: TObject);

    function DoCustomPopup(const P: TPoint): Boolean; override;
    procedure PopulateMenuItems; override;
  end;

  { TdxSpreadSheetBuiltInTableViewPopupMenu }

  TdxSpreadSheetBuiltInTableViewPopupMenu = class(TdxSpreadSheetCustomPopupMenu)
  public const
    cidBringToFront = 1;
    cidClearContent = 2;
    cidCopy = 3;
    cidCut = 4;
    cidDelete = 5;
    cidDeleteComments = 6;
    cidDeleteHyperlink = 7;
    cidEditComment = 8;
    cidEditContainer = 9;
    cidEditHyperlink = 10;
    cidFormatCells = 11;
    cidHide = 12;
    cidInsert = 13;
    cidMerge = 14;
    cidOpenHyperlink = 15;
    cidPaste = 16;
    cidPasteSpecialFormulas = 17;
    cidPasteSpecialFormulasAndColumnWidths = 18;
    cidPasteSpecialFormulasAndFormatting = 19;
    cidPasteSpecialFormulasAndStyles = 20;
    cidPasteSpecialValues = 21;
    cidPasteSpecialValuesAndFormatting = 22;
    cidPasteSpecialValuesAndStyles = 23;
    cidSendToBack = 24;
    cidShowPasteSpecialDialog = 25;
    cidSplit = 26;
    cidToggleCommentVisibility = 27;
    cidUnhide = 28;
  strict private
    function GetFocusedCellComment: TdxSpreadSheetContainer;
    function GetFocusedContainer: TdxSpreadSheetContainer;
    function GetHyperlink: TdxSpreadSheetHyperlink;
    function GetView: TdxSpreadSheetTableView; inline;
  protected
    procedure ChangeVisibility(AValue: Boolean);
    function DoCustomPopup(const P: TPoint): Boolean; override;
    function GetFocusedCell(ACreateIfNotExists: Boolean): TdxSpreadSheetCell;
    procedure PopulateMenuItems; override;
    procedure PopulateMenuItemsForCells; virtual;
    procedure PopulateMenuItemsForCellsComments; virtual;
    procedure PopulateMenuItemsForContainer; virtual;
    procedure PopulateMenuItemsForHyperlink;
    procedure PopulatePasteSpecialMenuItems;

    procedure DoBringToFront(Sender: TObject);
    procedure DoClearContent(Sender: TObject);
    procedure DoCopy(Sender: TObject);
    procedure DoCut(Sender: TObject);
    procedure DoDelete(Sender: TObject);
    procedure DoDeleteComments(Sender: TObject);
    procedure DoDeleteHyperlink(Sender: TObject);
    procedure DoEditComment(Sender: TObject);
    procedure DoEditContainer(Sender: TObject);
    procedure DoEditHyperlink(Sender: TObject);
    procedure DoFormatCells(Sender: TObject);
    procedure DoHide(Sender: TObject);
    procedure DoInsert(Sender: TObject);
    procedure DoMerge(Sender: TObject);
    procedure DoOpenHyperlink(Sender: TObject);
    procedure DoPaste(Sender: TObject);
    procedure DoPasteSpecial(Sender: TObject);
    procedure DoSendToBack(Sender: TObject);
    procedure DoShowPasteSpecialDialog(Sender: TObject);
    procedure DoSplit(Sender: TObject);
    procedure DoToggleCommentVisibility(Sender: TObject);
    procedure DoUnhide(Sender: TObject);

    property FocusedCellComment: TdxSpreadSheetContainer
      read GetFocusedCellComment;
    property FocusedContainer: TdxSpreadSheetContainer read GetFocusedContainer;
    property Hyperlink: TdxSpreadSheetHyperlink read GetHyperlink;
    property View: TdxSpreadSheetTableView read GetView;
  end;

implementation

uses
  Math, SysUtils, Dialogs, dxGDIPlusClasses, cxClasses, dxSpreadSheetStrs,
  dxSpreadSheetFormatCellsDialog,
  dxHashUtils, dxSpreadSheetContainerCustomizationDialog,
  dxSpreadSheetUnhideSheetDialog, dxSpreadSheetContainers,
  dxSpreadSheetEditHyperlinkDialog, dxSpreadSheetPasteSpecialDialog,
  dxSpreadSheetProtectSheetDialog,
  dxSpreadSheetPasswordDialog, dxOLECryptoContainerStrs, dxInputDialogs,
  dxSpreadSheetCoreStrs;

{$R dxSpreadSheetPopupMenu.res}

type
  TdxSpreadSheetTableItemsAccess = class(TdxSpreadSheetTableItems);
  TdxSpreadSheetTableViewAccess = class(TdxSpreadSheetTableView);
  TdxCustomSpreadSheetAccess = class(TdxCustomSpreadSheet);

function HasVisibleItems(AParentItem: TMenuItem): Boolean;
var
  I: Integer;
begin
  for I := 0 to AParentItem.Count - 1 do
  begin
    if AParentItem[I].Visible then
      Exit(True);
  end;
  Result := False;
end;

{ TdxSpreadSheetCustomPopupMenu }

constructor TdxSpreadSheetCustomPopupMenu.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FImageList := TcxImageList.Create(Self);
  FPopupMenu := TPopupMenu.Create(Self);
  FPopupMenu.Images := ImageList;
end;

destructor TdxSpreadSheetCustomPopupMenu.Destroy;
begin
  FreeAndNil(FPopupMenu);
  FreeAndNil(FImageList);
  inherited Destroy;
end;

procedure TdxSpreadSheetCustomPopupMenu.Initialize;
begin
  ImageList.Clear;
  FPopupMenu.Items.Clear;
  PopulateMenuItems;
end;

function TdxSpreadSheetCustomPopupMenu.Popup(const P: TPoint): Boolean;
var
  AAdapter: TdxCustomBuiltInPopupMenuAdapter;
  AScreenPoint: TPoint;
begin
  Initialize;
  AScreenPoint := SpreadSheet.ClientToScreen(P);
  if DoCustomPopup(AScreenPoint) then
    Exit(True);

  if TdxBuiltInPopupMenuAdapterManager.IsActualAdapterStandard then
  begin
    Result := HasVisibleItems(FPopupMenu.Items);
    if Result then
      FPopupMenu.Popup(AScreenPoint.X, AScreenPoint.Y)
  end
  else
  begin
    AAdapter := TdxBuiltInPopupMenuAdapterManager.GetActualAdapterClass.
      Create(nil);
    try
      AAdapter.SetImages(ImageList);
      AAdapter.SetLookAndFeel(SpreadSheet.LookAndFeel);
      TdxBuiltInPopupMenuAdapterHelper.AddMenu(AAdapter, FPopupMenu,
        AdapterClickHandler);
      Result := AAdapter.Popup(AScreenPoint);
    finally
      AAdapter.Free;
    end;
  end;
end;

function TdxSpreadSheetCustomPopupMenu.AddImage(const AResourceName
  : string): Integer;
begin
  Result := ImageList.AddImageFromResource(HInstance, AResourceName, 'PNG');
end;

function TdxSpreadSheetCustomPopupMenu.AddMenuItem(ACaption: Pointer;
  ACommandID: Word; AClickHandler: TNotifyEvent; AEnabled: Boolean = True;
  const AImageResName: string = ''; AParent: TComponent = nil): TComponent;
var
  AMenuItem: TMenuItem;
begin
  AMenuItem := CreateMenuItem(AParent);
  AMenuItem.Caption := cxGetResourceString(ACaption);
  AMenuItem.OnClick := AClickHandler;
  AMenuItem.Tag := ACommandID;
  AMenuItem.ImageIndex := AddImage(AImageResName);
  AMenuItem.Enabled := AEnabled;
  Result := AMenuItem;
end;

procedure TdxSpreadSheetCustomPopupMenu.AddSeparator(AParent: TComponent = nil);
begin
  CreateMenuItem(AParent).Caption := '-';
end;

function TdxSpreadSheetCustomPopupMenu.AddSubMenu(ACaption: Pointer)
  : TComponent;
begin
  Result := AddMenuItem(ACaption, 0, nil);
end;

function TdxSpreadSheetCustomPopupMenu.DoCustomPopup(const P: TPoint): Boolean;
begin
  Result := False;
end;

procedure TdxSpreadSheetCustomPopupMenu.AdapterClickHandler(Sender: TObject);
begin
  TMenuItem((Sender as TComponent).Tag).Click;
end;

function TdxSpreadSheetCustomPopupMenu.CreateMenuItem(AParent: TComponent = nil)
  : TMenuItem;
begin
  if AParent = nil then
    AParent := FPopupMenu.Items;
  Result := TMenuItem.Create(AParent);
  (AParent as TMenuItem).Add(Result);
end;

function TdxSpreadSheetCustomPopupMenu.GetSpreadSheet: TdxCustomSpreadSheet;
begin
  Result := Owner as TdxCustomSpreadSheet;
end;

{ TdxSpreadSheetBuiltInPageControlTabPopupMenu }

function TdxSpreadSheetBuiltInPageControlTabPopupMenu.CanDelete: Boolean;
begin
  Result := (SpreadSheet.VisibleSheetCount > 1) and
    SpreadSheet.OptionsProtection.ActualAllowChangeStructure;
end;

function TdxSpreadSheetBuiltInPageControlTabPopupMenu.CanHide: Boolean;
begin
  Result := (SpreadSheet.VisibleSheetCount > 1) and
    SpreadSheet.OptionsProtection.ActualAllowChangeStructure;
end;

function TdxSpreadSheetBuiltInPageControlTabPopupMenu.CanInsert: Boolean;
begin
  Result := SpreadSheet.OptionsProtection.ActualAllowChangeStructure;
end;

function TdxSpreadSheetBuiltInPageControlTabPopupMenu.CanRename: Boolean;
begin
  Result := SpreadSheet.OptionsProtection.ActualAllowChangeStructure;
end;

function TdxSpreadSheetBuiltInPageControlTabPopupMenu.CanUnhide: Boolean;
begin
  Result := (SpreadSheet.VisibleSheetCount <> SpreadSheet.SheetCount) and
    SpreadSheet.OptionsProtection.ActualAllowChangeStructure;
end;

procedure TdxSpreadSheetBuiltInPageControlTabPopupMenu.DoDelete
  (Sender: TObject);
begin
  SpreadSheet.ActiveSheet.Free;
end;

procedure TdxSpreadSheetBuiltInPageControlTabPopupMenu.DoHide(Sender: TObject);
begin
  SpreadSheet.ActiveSheet.Visible := False;
end;

procedure TdxSpreadSheetBuiltInPageControlTabPopupMenu.DoInsert
  (Sender: TObject);
var
  ASheet: TdxSpreadSheetCustomView;
begin
  ASheet := SpreadSheet.AddSheet;
  ASheet.Index := SpreadSheet.ActiveSheetIndex;
  ASheet.Active := True;
end;

procedure TdxSpreadSheetBuiltInPageControlTabPopupMenu.DoProtect
  (Sender: TObject);
begin
  SpreadSheet.ActiveSheetAsTable.Protect;
end;

procedure TdxSpreadSheetBuiltInPageControlTabPopupMenu.DoRename
  (Sender: TObject);
var
  AValue: string;
begin
  AValue := SpreadSheet.ActiveSheet.Caption;
  while True do
    try
      if dxInputQuery(cxGetResourceString(@sdxRenameDialogCaption),
        cxGetResourceString(@sdxRenameDialogSheetName), AValue) then
      begin
        AValue := Trim(AValue);
        if AValue <> '' then
          SpreadSheet.ActiveSheet.Caption := AValue;
      end;
      Break;
    except
      MessageDlg(cxGetResourceString(@sdxErrorCannotRenameSheet), mtWarning,
        [mbOK], 0);
    end;
end;

procedure TdxSpreadSheetBuiltInPageControlTabPopupMenu.DoUnhide
  (Sender: TObject);
begin
  ShowUnhideSheetDialog(SpreadSheet);
end;

procedure TdxSpreadSheetBuiltInPageControlTabPopupMenu.DoUnprotect
  (Sender: TObject);
begin
  SpreadSheet.ActiveSheetAsTable.Unprotect;
end;

function TdxSpreadSheetBuiltInPageControlTabPopupMenu.DoCustomPopup
  (const P: TPoint): Boolean;
begin
  Result := TdxCustomSpreadSheetAccess(SpreadSheet).DoPageControlContextPopup(P,
    PopupMenu);
end;

procedure TdxSpreadSheetBuiltInPageControlTabPopupMenu.PopulateMenuItems;
begin
  AddMenuItem(@sdxBuiltInPopupMenuInsert, cidInsert, DoInsert, CanInsert,
    'DXSPREADSHEET_POPUPMENU_GLYPH_INSERTSHEET');
  AddMenuItem(@sdxBuiltInPopupMenuDelete, cidDelete, DoDelete, CanDelete,
    'DXSPREADSHEET_POPUPMENU_GLYPH_REMOVESHEET');
  AddMenuItem(@sdxBuiltInPopupMenuRename, cidRename, DoRename, CanRename);

  if SpreadSheet.ActiveSheetAsTable.OptionsProtection.Protected then
    AddMenuItem(@sdxBuiltInPopupMenuUnprotectSheet, cidUnprotect, DoUnprotect,
      True, 'DXSPREADSHEET_POPUPMENU_GLYPH_PROTECTION')
  else
    AddMenuItem(@sdxBuiltInPopupMenuProtectSheet, cidProtect, DoProtect, True,
      'DXSPREADSHEET_POPUPMENU_GLYPH_PROTECTION');

  AddSeparator;
  AddMenuItem(@sdxBuiltInPopupMenuHide, cidHide, DoHide, CanHide);
  AddMenuItem(@sdxBuiltInPopupMenuUnhideDialog, cidUnhide, DoUnhide, CanUnhide);
end;

{ TdxSpreadSheetBuiltInTableViewPopupMenu }

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.ChangeVisibility
  (AValue: Boolean);

  procedure DoChangeItemsVisibility(AItems: TdxSpreadSheetTableItems;
    AStartIndex, AFinishIndex: Integer);
  var
    I: Integer;
  begin
    if AValue then
    begin
      TdxSpreadSheetTableItemsAccess(AItems).ForEach(
        procedure(AItem: TdxDynamicListItem)
        begin
          TdxSpreadSheetTableItem(AItem).Visible := True;
        end, AStartIndex, AFinishIndex);
    end
    else
      for I := AStartIndex to AFinishIndex do
        AItems.CreateItem(I).Visible := AValue;
  end;

  procedure DoChangeVisibilityInArea(const AArea: TRect);
  begin
    if dxSpreadSheetIsEntireRow(AArea) then
      DoChangeItemsVisibility(View.Rows, AArea.Top, AArea.Bottom)
    else if dxSpreadSheetIsEntireColumn(AArea) then
      DoChangeItemsVisibility(View.Columns, AArea.Left, AArea.Right);
  end;

var
  AAreaIndex: Integer;
  ASelection: TdxSpreadSheetTableViewSelection;
begin
  View.BeginUpdate;
  try
    TdxSpreadSheetTableViewAccess(View).History.BeginAction
      (TdxSpreadSheetHistoryChangeRowColumnItemAction);
    try
      ASelection := View.Selection;
      for AAreaIndex := 0 to ASelection.Count - 1 do
        DoChangeVisibilityInArea
          (dxSpreadSheetGetRealArea(ASelection[AAreaIndex].Rect));
      if AValue then
        TdxSpreadSheetTableViewAccess(View).Pack;
    finally
      TdxSpreadSheetTableViewAccess(View).History.EndAction;
    end;
  finally
    View.EndUpdate;
  end;
end;

function TdxSpreadSheetBuiltInTableViewPopupMenu.DoCustomPopup
  (const P: TPoint): Boolean;
begin
  Result := TdxCustomSpreadSheetAccess(SpreadSheet).DoTableViewContextPopup(P,
    PopupMenu);
end;

function TdxSpreadSheetBuiltInTableViewPopupMenu.GetFocusedCell
  (ACreateIfNotExists: Boolean): TdxSpreadSheetCell;
begin
  Result := View.Selection.FocusedCell;
  if (Result = nil) and ACreateIfNotExists then
    Result := View.CreateCell(View.Selection.FocusedRow,
      View.Selection.FocusedColumn);
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.PopulateMenuItems;
begin
  AddMenuItem(@sdxBuiltInPopupMenuCut, cidCut, DoCut, View.CanCutToClipboard,
    'DXSPREADSHEET_POPUPMENU_GLYPH_CUT');
  AddMenuItem(@sdxBuiltInPopupMenuCopy, cidCopy, DoCopy,
    View.CanCopyToClipboard, 'DXSPREADSHEET_POPUPMENU_GLYPH_COPY');
  AddMenuItem(@sdxBuiltInPopupMenuPaste, cidPaste, DoPaste,
    View.CanPasteFromClipboard, 'DXSPREADSHEET_POPUPMENU_GLYPH_PASTE');
  PopulatePasteSpecialMenuItems;

  AddSeparator;

  if FocusedContainer = nil then
    PopulateMenuItemsForCells
  else
    PopulateMenuItemsForContainer
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.PopulateMenuItemsForCells;
const
  DeleteCommandCaptions: array [Boolean] of Pointer =
    (@sdxBuiltInPopupMenuDelete, @sdxBuiltInPopupMenuDeleteDialog);
  InsertCommandCaptions: array [Boolean] of Pointer =
    (@sdxBuiltInPopupMenuInsert, @sdxBuiltInPopupMenuInsertDialog);
var
  ACanEditHeaders: Boolean;
  AEntireColumnIsSelected: Boolean;
  AEntireRowIsSelected: Boolean;
  AEntireRowOrColumnIsSelected: Boolean;
begin
  if SpreadSheet.OptionsBehavior.Editing then
  begin
    AEntireColumnIsSelected := (FocusedContainer = nil) and
      dxSpreadSheetIsEntireColumn(View.Selection.Area);
    AEntireRowIsSelected := (FocusedContainer = nil) and
      dxSpreadSheetIsEntireRow(View.Selection.Area);
    AEntireRowOrColumnIsSelected := AEntireColumnIsSelected or
      AEntireRowIsSelected;

    // Merging
    AddMenuItem(@sdxBuiltInPopupMenuMergeCells, cidMerge, DoMerge,
      View.CanMergeSelected, 'DXSPREADSHEET_POPUPMENU_GLYPH_MERGE');
    AddMenuItem(@sdxBuiltInPopupMenuSplitCells, cidSplit, DoSplit,
      View.CanSplitSelected, 'DXSPREADSHEET_POPUPMENU_GLYPH_UNMERGE');

    // Selection
    AddSeparator;
    if SpreadSheet.OptionsBehavior.Inserting then
      AddMenuItem(InsertCommandCaptions[not AEntireRowOrColumnIsSelected],
        cidInsert, DoInsert, View.CanInsert);
    if SpreadSheet.OptionsBehavior.Deleting then
      AddMenuItem(DeleteCommandCaptions[not AEntireRowOrColumnIsSelected],
        cidDelete, DoDelete, View.CanDelete);
    AddMenuItem(@sdxBuiltInPopupMenuClearContents, cidClearContent,
      DoClearContent, View.CanClearCells);

    // Comments
    PopulateMenuItemsForCellsComments;

    // Formatting
    if SpreadSheet.OptionsBehavior.Formatting then
    begin
      AddSeparator;
      AddMenuItem(@sdxBuiltInPopupMenuFormatCells, cidFormatCells,
        DoFormatCells, View.OptionsProtection.ActualAllowFormatCells,
        'DXSPREADSHEET_POPUPMENU_GLYPH_FORMATCELLS');
    end;

    // Row / Columns
    if AEntireRowOrColumnIsSelected then
    begin
      ACanEditHeaders := AEntireRowIsSelected and
        View.OptionsProtection.ActualAllowResizeRows or
        AEntireColumnIsSelected and
        View.OptionsProtection.ActualAllowResizeColumns;

      AddMenuItem(@sdxBuiltInPopupMenuHide, cidHide, DoHide, ACanEditHeaders);
      AddMenuItem(@sdxBuiltInPopupMenuUnhide, cidUnhide, DoUnhide,
        ACanEditHeaders);
    end
    else
      PopulateMenuItemsForHyperlink;
  end;
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.
  PopulateMenuItemsForCellsComments;
const
  ChangeCommentVisibilityCommandCaption: array [Boolean] of Pointer =
    (@sdxBuiltInPopupMenuShowComment, @sdxBuiltInPopupMenuHideComment);
  EditCommentCommandCaption: array [Boolean] of Pointer =
    (@sdxBuiltInPopupMenuInsertComment, @sdxBuiltInPopupMenuEditComment);
  EditCommentImages: array [Boolean] of string =
    ('DXSPREADSHEET_POPUPMENU_GLYPH_INSERTCOMMENT',
    'DXSPREADSHEET_POPUPMENU_GLYPH_EDITCOMMENT');
var
  AContainer: TdxSpreadSheetContainer;
begin
  AddSeparator;
  AContainer := FocusedCellComment;
  if View.CanEditContainers then
    AddMenuItem(EditCommentCommandCaption[AContainer <> nil], cidEditComment,
      DoEditComment, View.CanEditComment, EditCommentImages[AContainer <> nil]);
  if View.CanDeleteComments then
    AddMenuItem(@sdxBuiltInPopupMenuDeleteComment, cidDeleteComments,
      DoDeleteComments, True, 'DXSPREADSHEET_POPUPMENU_GLYPH_DELETECOMMENT');
  if AContainer <> nil then
    AddMenuItem(ChangeCommentVisibilityCommandCaption[AContainer.Visible],
      cidToggleCommentVisibility, DoToggleCommentVisibility);
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.PopulateMenuItemsForContainer;
begin
  AddMenuItem(@sdxBuiltInPopupMenuBringToFront, cidBringToFront, DoBringToFront,
    View.Containers.Count > 1, 'DXSPREADSHEET_POPUPMENU_GLYPH_BRINGTOFRONT');
  AddMenuItem(@sdxBuiltInPopupMenuSendToBack, cidSendToBack, DoSendToBack,
    View.Containers.Count > 1, 'DXSPREADSHEET_POPUPMENU_GLYPH_SENDTOBACK');
  AddSeparator;

  AddMenuItem(@sdxBuiltInPopupMenuDelete, cidDelete, DoDelete, View.CanDelete);
  AddSeparator;

  AddMenuItem(@sdxBuiltInPopupMenuCustomizeObject, cidEditContainer,
    DoEditContainer, View.CanEditContainers);

  if FocusedCellComment = nil then
  begin
    AddSeparator;
    PopulateMenuItemsForHyperlink;
  end;
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.PopulateMenuItemsForHyperlink;
const
  Map: array [Boolean] of Pointer = (@sdxBuiltInPopupMenuCreateHyperlink,
    @sdxBuiltInPopupMenuCreateHyperlink);
begin
  if View.OptionsProtection.ActualAllowEditHyperlinks then
    AddMenuItem(Map[Hyperlink <> nil], cidEditHyperlink, DoEditHyperlink,
      View.CanEditHyperlinks, 'DXSPREADSHEET_POPUPMENU_GLYPH_HYPERLINK');

  if Hyperlink <> nil then
  begin
    AddMenuItem(@sdxBuiltInPopupMenuOpenHyperlink, cidOpenHyperlink,
      DoOpenHyperlink);
    if View.OptionsProtection.ActualAllowEditHyperlinks then
    begin
      AddMenuItem(@sdxBuiltInPopupMenuRemoveHyperlink, cidDeleteHyperlink,
        DoDeleteHyperlink, View.CanDeleteHyperlink,
        'DXSPREADSHEET_POPUPMENU_GLYPH_DELETEHYPERLINK');
    end;
  end;
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.PopulatePasteSpecialMenuItems;
var
  ARootMenu: TComponent;
begin
  ARootMenu := nil;
  if View.CanPasteFromClipboard([]) then
  begin
    ARootMenu := AddSubMenu(@sdxBuiltInPopupMenuPasteSpecial);

    AddMenuItem(@sdxBuiltInPopupMenuPasteSpecialAll, cidPaste, DoPaste, True,
      'DXSPREADSHEET_POPUPMENU_GLYPH_PASTE', ARootMenu);

    AddSeparator(ARootMenu);

    AddMenuItem(@sdxBuiltInPopupMenuPasteSpecialValues, cidPasteSpecialValues,
      DoPasteSpecial, True, '', ARootMenu);
    AddMenuItem(@sdxBuiltInPopupMenuPasteSpecialValuesAndFormatting,
      cidPasteSpecialValuesAndFormatting, DoPasteSpecial, True, '', ARootMenu);
    AddMenuItem(@sdxBuiltInPopupMenuPasteSpecialValuesAndStyles,
      cidPasteSpecialValuesAndStyles, DoPasteSpecial, True, '', ARootMenu);

    AddSeparator(ARootMenu);

    AddMenuItem(@sdxBuiltInPopupMenuPasteSpecialFormulas,
      cidPasteSpecialFormulas, DoPasteSpecial, True, '', ARootMenu);
    AddMenuItem(@sdxBuiltInPopupMenuPasteSpecialFormulasAndFormatting,
      cidPasteSpecialFormulasAndFormatting, DoPasteSpecial, True, '',
      ARootMenu);
    AddMenuItem(@sdxBuiltInPopupMenuPasteSpecialFormulasAndStyles,
      cidPasteSpecialFormulasAndStyles, DoPasteSpecial, True, '', ARootMenu);
    AddMenuItem(@sdxBuiltInPopupMenuPasteSpecialFormulasAndColumnWidths,
      cidPasteSpecialFormulasAndColumnWidths, DoPasteSpecial, True, '',
      ARootMenu);

    AddSeparator(ARootMenu);
  end;

  AddMenuItem(@sdxBuiltInPopupMenuPasteSpecialShowDialog,
    cidShowPasteSpecialDialog, DoShowPasteSpecialDialog, (ARootMenu <> nil) or
    View.CanPasteFromClipboard, '', ARootMenu);
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.DoBringToFront
  (Sender: TObject);
begin
  FocusedContainer.BringToFront;
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.DoClearContent
  (Sender: TObject);
begin
  View.ClearCellValues;
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.DoCopy(Sender: TObject);
begin
  View.CopyToClipboard;
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.DoCut(Sender: TObject);
begin
  View.CutToClipboard;
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.DoDelete(Sender: TObject);
begin
  if FocusedContainer <> nil then
    FocusedContainer.Free
  else
    View.DeleteCells;
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.DoDeleteComments
  (Sender: TObject);
begin
  View.DeleteComments;
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.DoDeleteHyperlink
  (Sender: TObject);
begin
  View.DeleteHyperlink;
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.DoEditComment
  (Sender: TObject);
begin
  View.EditComment;
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.DoEditContainer
  (Sender: TObject);
begin
  ShowContainerCustomizationDialog(FocusedContainer);
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.DoEditHyperlink
  (Sender: TObject);
begin
  View.EditHyperlink;
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.DoFormatCells
  (Sender: TObject);
begin
  ShowFormatCellsDialog(View);
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.DoHide(Sender: TObject);
begin
  ChangeVisibility(False);
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.DoInsert(Sender: TObject);
begin
  View.InsertCells;
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.DoMerge(Sender: TObject);
begin
  View.MergeSelected;
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.DoOpenHyperlink
  (Sender: TObject);
begin
  Hyperlink.Execute;
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.DoPaste(Sender: TObject);
begin
  View.PasteFromClipboard;
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.DoPasteSpecial
  (Sender: TObject);
var
  AOptions: TdxSpreadSheetClipboardPasteOptions;
begin
  case TComponent(Sender).Tag of
    cidPasteSpecialFormulas:
      AOptions := [cpoValues, cpoFormulas];
    cidPasteSpecialFormulasAndFormatting:
      AOptions := [cpoValues, cpoFormulas, cpoNumberFormatting];
    cidPasteSpecialFormulasAndColumnWidths:
      AOptions := [cpoValues, cpoFormulas, cpoStyles, cpoComments,
        cpoColumnWidths];
    cidPasteSpecialFormulasAndStyles:
      AOptions := [cpoValues, cpoFormulas, cpoStyles, cpoComments];
    cidPasteSpecialValuesAndFormatting:
      AOptions := [cpoValues, cpoNumberFormatting];
    cidPasteSpecialValuesAndStyles:
      AOptions := [cpoValues, cpoStyles, cpoComments];
  else
    AOptions := [cpoValues];
  end;
  View.PasteFromClipboard(AOptions);
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.DoSendToBack(Sender: TObject);
begin
  FocusedContainer.SendToBack;
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.DoShowPasteSpecialDialog
  (Sender: TObject);
begin
  ShowPasteSpecialDialog(SpreadSheet);
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.DoSplit(Sender: TObject);
begin
  View.SplitSelected;
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.DoToggleCommentVisibility
  (Sender: TObject);
begin
  FocusedCellComment.Visible := not FocusedCellComment.Visible;
end;

procedure TdxSpreadSheetBuiltInTableViewPopupMenu.DoUnhide(Sender: TObject);
begin
  ChangeVisibility(True);
end;

function TdxSpreadSheetBuiltInTableViewPopupMenu.GetFocusedCellComment
  : TdxSpreadSheetContainer;
begin
  if not View.Containers.FindCommentContainer(GetFocusedCell(False), Result)
  then
    Result := nil;
end;

function TdxSpreadSheetBuiltInTableViewPopupMenu.GetFocusedContainer
  : TdxSpreadSheetContainer;
begin
  Result := TdxSpreadSheetTableViewAccess(View).Controller.FocusedContainer;
end;

function TdxSpreadSheetBuiltInTableViewPopupMenu.GetHyperlink
  : TdxSpreadSheetHyperlink;
begin
  Result := TdxSpreadSheetTableViewAccess(View).Controller.FocusedHyperlink;
end;

function TdxSpreadSheetBuiltInTableViewPopupMenu.GetView
  : TdxSpreadSheetTableView;
begin
  Result := SpreadSheet.ActiveSheetAsTable;
end;

end.
