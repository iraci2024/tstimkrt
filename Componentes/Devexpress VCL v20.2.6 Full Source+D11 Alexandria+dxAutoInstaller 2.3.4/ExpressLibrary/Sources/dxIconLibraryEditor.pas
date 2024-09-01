{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ Express Cross Platform Library classes }
{ }
{ Copyright (c) 2000-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL }
{ ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM }
{ ONLY. }
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

unit dxIconLibraryEditor;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls,
  Forms, Dialogs, cxGraphics, cxControls,
  cxContainer, cxEdit, Menus, ExtCtrls, StdCtrls, cxButtons, cxImage, cxListBox,
  cxLabel, cxTextEdit, cxMaskEdit,
  cxButtonEdit, dxGalleryControl, cxCheckListBox, ExtDlgs, dxGallery, cxClasses,
  cxGeometry, dxGDIPlusClasses,
  dxIconLibraryEditorHelpers, ComCtrls, dxCore, dxIconLibrary, ImgList,
  cxLookAndFeels, cxLookAndFeelPainters, cxPC,
  ActnList, cxImageList, cxCustomListBox, cxCheckBox, dxSmartImage,
  dxPictureEditor, dxLayoutLookAndFeels, dxThreading,
  dxLayoutContainer, dxLayoutControl, dxLayoutcxEditAdapters;

type
  TdxfmImagePicker = class;

  TdxPopulateGalleryMode = (gpmGroups, gpmSearch);

  { TdxSearchString }

  TdxSearchString = record
    Data: string;
    DataAssigned: Boolean;

    class function Create(const S: string): TdxSearchString; static;
    function Check(const S: string): Boolean; inline;
  end;

  { TdxLoadImagesTask }

  TdxLoadImagesTask = class(TdxTask)
  strict private
    FImagesToLoad: TList;
    FLoadedImages: TThreadList;
    FTargetGallery: TdxGalleryControl;
    FUpdateTimer: TcxTimer;

    procedure SyncAssignImages;
    procedure SyncComplete;
    procedure SyncReleaseResources;
    procedure TimerHandler(Sender: TObject);
  protected
    procedure Complete; override;
    procedure Execute; override;
  public
    constructor Create(ATarget: TdxGalleryControl; AImagesToLoad: TList);
    destructor Destroy; override;
  end;

  { TdxCustomPopulateHelper }

  TdxCustomPopulateHelper = class
  strict private
    FImagePicker: TdxfmImagePicker;

    function GetCategoryList: TcxCheckListBox;
    function GetCollectionList: TcxCheckListBox;
    function GetGallery: TdxGalleryControl;
    function GetGalleryGroupHidden: TdxGalleryControlGroup;
    function GetGalleryGroupSearch: TdxGalleryControlGroup;
    function GetIconLibrary: TdxIconLibrary;
    function GetSelection: TdxGalleryControl;
    function GetSizeList: TcxCheckListBox;
    procedure SetGalleryGroupHidden(AValue: TdxGalleryControlGroup);
    procedure SetGalleryGroupSearch(AValue: TdxGalleryControlGroup);
  protected
    function IsListBoxCheckedByText(ACheckListBox: TcxCheckListBox;
      const AText: string): Boolean;
    procedure PopulateCore; virtual; abstract;

    property CategoryList: TcxCheckListBox read GetCategoryList;
    property CollectionList: TcxCheckListBox read GetCollectionList;
    property Gallery: TdxGalleryControl read GetGallery;
    property GalleryGroupHidden: TdxGalleryControlGroup
      read GetGalleryGroupHidden write SetGalleryGroupHidden;
    property GalleryGroupSearch: TdxGalleryControlGroup
      read GetGalleryGroupSearch write SetGalleryGroupSearch;
    property IconLibrary: TdxIconLibrary read GetIconLibrary;
    property ImagePicker: TdxfmImagePicker read FImagePicker;
    property Selection: TdxGalleryControl read GetSelection;
    property SizeList: TcxCheckListBox read GetSizeList;
  public
    constructor Create(AImagePicker: TdxfmImagePicker); virtual;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Populate;
  end;

  { TdxPopulateContentHelper }

  TdxPopulateContentHelper = class(TdxCustomPopulateHelper)
  strict private
  const
    DefaultImageSize: TSize = (cx: 16; cy: 16);
    TAG_UNUSED = $123;
    TAG_USED = 0;
  strict private
    function AddCheckListBoxItem(ACheckListBox: TcxCheckListBox;
      const AText: string; AChecked: Boolean): Boolean;
    function AddGalleryGroup(const ACaption: string; AIndex: Integer;
      AVisible: Boolean): TdxGalleryControlGroup;
    procedure AddGalleryGroups(ASet: TdxIconLibrarySet);
    procedure AddGalleryItem(AImageItem: TdxIconLibraryImage);
    procedure AddGalleryItems(ACategoryItem: TdxIconLibraryCategory);
    procedure DeleteUnusedItems(ACheckListBox: TcxCheckListBox);
    function FindCheckListBoxItem(ACheckListBox: TcxCheckListBox;
      const AText: string; out AItem: TcxCheckListBoxItem): Boolean;
    function MakeHint(AImageItem: TdxIconLibraryImage): string;
    procedure MarkItemsUnused(ACheckListBox: TcxCheckListBox);
    function GetIndexForCheckListItem(ACheckListBox: TcxCheckListBox;
      const AText: string): Integer;
    function GetIndexForGalleryGroup(const ANameItem: string): Integer;
  protected
    procedure PopulateCore; override;
  end;

  { TdxPopulateGalleryHelper }

  TdxPopulateGalleryHelper = class(TdxCustomPopulateHelper)
  strict private
  const
    NoPreviewImageIndex = 9;
  strict private
    FImagesToLoad: TList;
    FIsDestroying: Boolean;
    FLoadImagesTaskHandle: THandle;
    FMaxSize: TSize;

    procedure LoadImagesTaskComplete;
    procedure StartLoadImagesThread;
    procedure StopLoadImagesThread;

    function GetGalleryItem(AIconLibraryImage: TdxIconLibraryImage)
      : TdxGalleryControlItem;
    function GetIndexForItem(AGroup: TdxGalleryControlGroup;
      const ANameItem: string): Integer;
    function GetPopulateGalleryMode: TdxPopulateGalleryMode;
    procedure PopulateGalleryGroups(ACollectionItem: TdxIconLibrarySet;
      const ASearchString: TdxSearchString);
    procedure PopulateGalleryImages(AGroup: TdxGalleryControlGroup;
      ACategory: TdxIconLibraryCategory; const ASearchString: TdxSearchString);
  protected
    procedure DestroyGalleryItem(AImage: TdxIconLibraryImage); inline;
    procedure DestroyGalleryItems(ACategory: TdxIconLibraryCategory); overload;
    procedure DestroyGalleryItems(ASet: TdxIconLibrarySet); overload;
    procedure DestroyGalleryItems; overload;
    procedure PopulateCore; override;
  public
    constructor Create(AImagePicker: TdxfmImagePicker); override;
    destructor Destroy; Override;
    //
    property PopulateGalleryMode: TdxPopulateGalleryMode
      read GetPopulateGalleryMode;
  end;

  { TdxfmImagePicker }

  TdxfmImagePicker = class(TdxPictureEditorDialog, IdxIconLibraryListener)
    actF3: TAction;
    AlignmentConstraint1: TdxLayoutAlignmentConstraint;
    beFind: TcxButtonEdit;
    clbCategories: TcxCheckListBox;
    clbCollection: TcxCheckListBox;
    clbSize: TcxCheckListBox;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    gcIcons: TdxGalleryControl;
    gcSelection: TdxGalleryControl;
    gcSelectionGroup: TdxGalleryControlGroup;
    LayoutCxLookAndFeel: TdxLayoutCxLookAndFeel;
    LayoutLookAndFeelList: TdxLayoutLookAndFeelList;
    lbCategories: TdxLayoutLabeledItem;
    lcIconLibrary: TdxLayoutControl;
    lcIconLibraryGroup_Root: TdxLayoutGroup;
    liCategories: TdxLayoutItem;
    liCollections: TdxLayoutItem;
    liGallery: TdxLayoutItem;
    liPadding1: TdxLayoutEmptySpaceItem;
    liSearchBox: TdxLayoutItem;
    liSelection: TdxLayoutItem;
    liSize: TdxLayoutItem;
    miCheckSelected: TMenuItem;
    miIconsDeselectAll: TMenuItem;
    miIconsDeselectAllinThisGroup: TMenuItem;
    miIconsAddToSelection: TMenuItem;
    miIconsSelectAll: TMenuItem;
    miIconsSelectAllinThisGroup: TMenuItem;
    miIconsSelectionDeleteSelected: TMenuItem;
    miIconsSelectionDeselectAll: TMenuItem;
    miIconsSelectionSelectAll: TMenuItem;
    miIconsShowInExplorer: TMenuItem;
    miLine1: TMenuItem;
    miLine3: TMenuItem;
    miLine4: TMenuItem;
    miSelectAll: TMenuItem;
    miSelectNone: TMenuItem;
    miUncheckSelected: TMenuItem;
    pmIconGallery: TPopupMenu;
    pmIconsSelection: TPopupMenu;
    pmSelection: TPopupMenu;
    tsDXImageGallery: TcxTabSheet;

    procedure actF3Execute(Sender: TObject);
    procedure beFindPropertiesButtonClick(Sender: TObject;
      AButtonIndex: Integer);
    procedure beFindPropertiesChange(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure clbCategoriesClickCheck(Sender: TObject; AIndex: Integer;
      APrevState, ANewState: TcxCheckBoxState);
    procedure clbCollectionClickCheck(Sender: TObject; AIndex: Integer;
      APrevState, ANewState: TcxCheckBoxState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure gcIconsDblClick(Sender: TObject);
    procedure gcSelectionDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure gcSelectionDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure miIconsDeselectAllClick(Sender: TObject);
    procedure miIconsSelectAllinThisGroupClick(Sender: TObject);
    procedure miIconsAddToSelectionClick(Sender: TObject);
    procedure miIconsSelectionDeleteSelectedClick(Sender: TObject);
    procedure miIconsSelectionDeselectAllClick(Sender: TObject);
    procedure miIconsShowInExplorerClick(Sender: TObject);
    procedure miSelectClick(Sender: TObject);
    procedure miUncheckSelectedClick(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
    procedure pmIconGalleryPopup(Sender: TObject);
    procedure pmIconsSelectionPopup(Sender: TObject);
    procedure pmSelectionPopup(Sender: TObject);
  strict private
    FGalleryGroupHidden: TdxGalleryControlGroup;
    FGalleryGroupSearch: TdxGalleryControlGroup;
    FMultiSelect: Boolean;
    FPopulateGalleryHelper: TdxPopulateGalleryHelper;
    FPopulateGalleryLockCount: Integer;
    FPopulateGalleryMode: TdxPopulateGalleryMode;
    FSelectedGroup: TdxGalleryControlGroup;

    procedure PopulateContent;
    procedure PopulateGallery;
    procedure SetMultiSelect(AValue: Boolean);
    procedure UpdateGalleryItemsSelection(AGallery: TdxGalleryControl;
      ASelect: Boolean); overload;
    procedure UpdateGalleryItemsSelection(AGroup: TdxGalleryControlGroup;
      ASelect: Boolean); overload;
    procedure UpdateSelectionBoxSize;
  protected
    procedure Initialize; override;
    procedure SaveSettings(APicture: TPicture); override;
    procedure SelectSize(const ASize: TSize);

    procedure AddToSelection(AImageItem: TdxGalleryControlItem);
    function GetIconLibraryImage(AGalleryControlItem: TdxGalleryControlItem)
      : TdxIconLibraryImage;
    function HasSelectedButNotAddedItems: Boolean;
    function HasSelection: Boolean;

    // IdxIconLibraryListener
    procedure OnChanged(Sender: TdxIconLibraryCollection);
    procedure OnChanging(Sender: TdxIconLibraryCollection);
    procedure OnRemoving(Sender: TdxIconLibraryCustomObject); overload;

    property GalleryGroupHidden: TdxGalleryControlGroup read FGalleryGroupHidden
      write FGalleryGroupHidden;
    property GalleryGroupSearch: TdxGalleryControlGroup read FGalleryGroupSearch
      write FGalleryGroupSearch;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect;
    property PopulateGalleryMode: TdxPopulateGalleryMode
      read FPopulateGalleryMode;
  public
    function ExecuteImageCollectionEditor(AFiles: TStrings;
      const ASuggestedImageSize: TSize): Boolean; virtual;
  end;

  { TdxImagePickerFormHelper }

  TdxImagePickerFormHelper = class(TInterfacedObject, IdxAdvancedPictureEditor,
    IdxImageCollectionEditor)
  strict private
    FImagePicker: TdxfmImagePicker;

    function GetImagePicker: TdxfmImagePicker;
  protected
    // IdxAdvancedPictureEditor
    function IdxAdvancedPictureEditor.Execute = ExecuteAdvancedPictureEditor;
    function ExecuteAdvancedPictureEditor(APicture: TPicture;
      AGraphicClass: TGraphicClass; AImportList: TStrings;
      APreferSmartImage: Boolean): Boolean;

    // IdxImageCollection
    function IdxImageCollectionEditor.Execute = ExecuteImageCollectionEditor;
    function ExecuteImageCollectionEditor(AFiles: TStrings;
      const ASuggestedImageSize: TSize): Boolean;

    property ImagePicker: TdxfmImagePicker read GetImagePicker;
  public
    destructor Destroy; override;
  end;

var
  dxIconLibrary: TdxIconLibrary;

procedure dxExecuteImagePicker(APicture: TPicture; AImportList: TStrings = nil);

implementation

{$R *.dfm}

uses
  Math, Clipbrd, cxShellCommon, StrUtils;

const
  sdxAddToSelectionConfirmation =
    'The selected items have not been added to the Selection box. Would you like to add them?';
  sdxGalleryDeselectAllInGroup = 'Deselect All in %s';
  sdxGalleryGroupHidden = 'Hidden';
  sdxGalleryGroupSearchResult = 'Search Result';
  sdxGalleryItemHint = '%s'#13#10'Category: %s'#13#10'Collection: %s';
  sdxGallerySelectAllInGroup = 'Select All in %s';

type
  TdxGalleryControlAccess = class(TdxGalleryControl);

procedure dxExecuteImagePicker(APicture: TPicture; AImportList: TStrings = nil);
var
  ADialog: TdxfmImagePicker;
begin
  ADialog := TdxfmImagePicker.Create(nil);
  try
    ADialog.ImportList := AImportList;
    ADialog.Execute(APicture);
  finally
    ADialog.Free;
  end;
end;

procedure CreateIconLibrary;
begin
  dxIconLibrary := TdxIconLibrary.Create(dxGetIconLibraryPath);
  dxIconLibrary.Refresh;
end;

procedure DestroyIconLibrary;
begin
  FreeAndNil(dxIconLibrary);
end;

{ TdxSearchString }

function TdxSearchString.Check(const S: string): Boolean;
begin
  if DataAssigned then
    Result := Pos(Data, AnsiLowerCase(S)) > 0
  else
    Result := True;
end;

class function TdxSearchString.Create(const S: string): TdxSearchString;
begin
  Result.Data := AnsiLowerCase(S);
  Result.DataAssigned := S <> '';
end;

{ TdxImagePickerFormHelper }

destructor TdxImagePickerFormHelper.Destroy;
begin
  FreeAndNil(FImagePicker);
  inherited Destroy;
end;

function TdxImagePickerFormHelper.ExecuteImageCollectionEditor(AFiles: TStrings;
  const ASuggestedImageSize: TSize): Boolean;
begin
  Result := ImagePicker.ExecuteImageCollectionEditor(AFiles,
    ASuggestedImageSize);
end;

function TdxImagePickerFormHelper.ExecuteAdvancedPictureEditor
  (APicture: TPicture; AGraphicClass: TGraphicClass; AImportList: TStrings;
  APreferSmartImage: Boolean): Boolean;
begin
  ImagePicker.PreferSmartImage := APreferSmartImage;
  ImagePicker.GraphicClass := AGraphicClass;
  ImagePicker.ImportList := AImportList;
  Result := ImagePicker.Execute(APicture);
end;

function TdxImagePickerFormHelper.GetImagePicker: TdxfmImagePicker;
begin
  if FImagePicker = nil then
    FImagePicker := TdxfmImagePicker.Create(nil);
  Result := FImagePicker;
end;

{ TdxLoadImagesTask }

constructor TdxLoadImagesTask.Create(ATarget: TdxGalleryControl;
  AImagesToLoad: TList);
begin
  FTargetGallery := ATarget;
  FImagesToLoad := TList.Create;
  FImagesToLoad.Assign(AImagesToLoad);
  FLoadedImages := TThreadList.Create;
  FUpdateTimer := cxCreateTimer(TimerHandler);
end;

destructor TdxLoadImagesTask.Destroy;
begin
  FreeAndNil(FLoadedImages);
  FreeAndNil(FImagesToLoad);
  inherited;
end;

procedure TdxLoadImagesTask.Complete;
begin
  dxCallThreadMethod(SyncComplete, tmcmSync);
end;

procedure TdxLoadImagesTask.Execute;
var
  AImage: TdxIconLibraryImage;
  I: Integer;
begin
  for I := 0 to FImagesToLoad.Count - 1 do
  begin
    AImage := TdxIconLibraryImage(FImagesToLoad.List[I]);
    try
      AImage.LoadFromFile;
    except
      // begin
      // TdxGalleryItem(AImageItem.Tag).Collection.Remove(TdxGalleryItem(AImageItem.Tag));
      // ACategory.Remove(AImageItem);
      // end;
    end;
    FLoadedImages.Add(AImage);
    if Canceled then
      Break;
  end;
end;

procedure TdxLoadImagesTask.SyncAssignImages;
var
  AIconLibraryImage: TdxIconLibraryImage;
  AList: TList;
begin
  AList := FLoadedImages.LockList;
  try
    if AList.Count > 0 then
    begin
      FTargetGallery.BeginUpdate;
      try
        while not Canceled and (AList.Count > 0) do
        begin
          AIconLibraryImage := TdxIconLibraryImage(AList.List[0]);
          TdxGalleryControlItem(AIconLibraryImage.Tag)
            .Glyph.Assign(AIconLibraryImage.Image);
          AIconLibraryImage.Image.Clear;
          AList.Delete(0);
        end;
      finally
        FTargetGallery.EndUpdate;
      end;
    end;
  finally
    FLoadedImages.UnlockList;
  end;
end;

procedure TdxLoadImagesTask.SyncComplete;
begin
  FreeAndNil(FUpdateTimer);

  if Canceled then
    SyncReleaseResources
  else
    SyncAssignImages;

  inherited Complete;
end;

procedure TdxLoadImagesTask.SyncReleaseResources;
var
  I: Integer;
begin
  with FLoadedImages.LockList do
    try
      for I := 0 to Count - 1 do
        TdxIconLibraryImage(List[I]).Image.Clear;
    finally
      FLoadedImages.UnlockList;
    end;
end;

procedure TdxLoadImagesTask.TimerHandler(Sender: TObject);
begin
  SyncAssignImages;
end;

{ TdxCustomPopulateHelper }

constructor TdxCustomPopulateHelper.Create(AImagePicker: TdxfmImagePicker);
begin
  inherited Create;
  FImagePicker := AImagePicker;
end;

procedure TdxCustomPopulateHelper.BeginUpdate;
begin
  Gallery.BeginUpdate;
  CollectionList.Items.BeginUpdate;
  CategoryList.Items.BeginUpdate;
  SizeList.Items.BeginUpdate;
end;

procedure TdxCustomPopulateHelper.EndUpdate;
begin
  SizeList.Items.EndUpdate;
  CategoryList.Items.EndUpdate;
  CollectionList.Items.EndUpdate;
  Gallery.EndUpdate;
end;

procedure TdxCustomPopulateHelper.Populate;
begin
  BeginUpdate;
  try
    PopulateCore;
  finally
    EndUpdate;
  end;
end;

function TdxCustomPopulateHelper.GetCategoryList: TcxCheckListBox;
begin
  Result := FImagePicker.clbCategories;
end;

function TdxCustomPopulateHelper.GetCollectionList: TcxCheckListBox;
begin
  Result := FImagePicker.clbCollection;
end;

function TdxCustomPopulateHelper.GetGallery: TdxGalleryControl;
begin
  Result := FImagePicker.gcIcons;
end;

function TdxCustomPopulateHelper.GetGalleryGroupHidden: TdxGalleryControlGroup;
begin
  Result := FImagePicker.GalleryGroupHidden;
end;

function TdxCustomPopulateHelper.GetGalleryGroupSearch: TdxGalleryControlGroup;
begin
  Result := FImagePicker.GalleryGroupSearch;
end;

function TdxCustomPopulateHelper.GetIconLibrary: TdxIconLibrary;
begin
  Result := dxIconLibrary;
end;

function TdxCustomPopulateHelper.GetSelection: TdxGalleryControl;
begin
  Result := ImagePicker.gcSelection;
end;

function TdxCustomPopulateHelper.GetSizeList: TcxCheckListBox;
begin
  Result := FImagePicker.clbSize;
end;

function TdxCustomPopulateHelper.IsListBoxCheckedByText(ACheckListBox
  : TcxCheckListBox; const AText: string): Boolean;
var
  AIndex: Integer;
begin
  AIndex := ACheckListBox.Items.IndexOf(AText);
  Result := (AIndex > -1) and ACheckListBox.Items[AIndex].Checked;
end;

procedure TdxCustomPopulateHelper.SetGalleryGroupHidden
  (AValue: TdxGalleryControlGroup);
begin
  FImagePicker.GalleryGroupHidden := AValue;
end;

procedure TdxCustomPopulateHelper.SetGalleryGroupSearch
  (AValue: TdxGalleryControlGroup);
begin
  FImagePicker.GalleryGroupSearch := AValue;
end;

{ TdxPopulateContentHelper }

procedure TdxPopulateContentHelper.PopulateCore;
var
  I: Integer;
begin
  MarkItemsUnused(SizeList);
  MarkItemsUnused(CollectionList);
  MarkItemsUnused(CategoryList);
  try
    for I := 0 to IconLibrary.Count - 1 do
      AddGalleryGroups(IconLibrary[I]);
  finally
    DeleteUnusedItems(CollectionList);
    DeleteUnusedItems(CategoryList);
    DeleteUnusedItems(SizeList);
  end;

  if ImagePicker.GalleryGroupSearch = nil then
    GalleryGroupSearch := AddGalleryGroup(sdxGalleryGroupSearchResult,
      Gallery.Gallery.Groups.Count, False);
  if ImagePicker.GalleryGroupHidden = nil then
    GalleryGroupHidden := AddGalleryGroup(sdxGalleryGroupHidden,
      Gallery.Gallery.Groups.Count, False);
end;

function TdxPopulateContentHelper.AddCheckListBoxItem(ACheckListBox
  : TcxCheckListBox; const AText: string; AChecked: Boolean): Boolean;
var
  AItem: TcxCheckListBoxItem;
  AItemIndex: Integer;
begin
  Result := False;
  if not FindCheckListBoxItem(ACheckListBox, AText, AItem) then
  begin
    AItemIndex := GetIndexForCheckListItem(ACheckListBox, AText);

    AItem := ACheckListBox.Items.Add;
    AItem.Index := AItemIndex;
    AItem.Text := AText;
    AItem.Checked := AChecked;
    Result := True;
  end;
  AItem.Tag := TAG_USED;
end;

function TdxPopulateContentHelper.AddGalleryGroup(const ACaption: string;
  AIndex: Integer; AVisible: Boolean): TdxGalleryControlGroup;
begin
  Result := Gallery.Gallery.Groups.Add;
  Result.Index := AIndex;
  Result.Visible := AVisible;
  Result.Caption := ACaption;
end;

procedure TdxPopulateContentHelper.AddGalleryGroups(ASet: TdxIconLibrarySet);
var
  ACategoryItem: TdxIconLibraryCategory;
  I: Integer;
begin
  AddCheckListBoxItem(CollectionList, ASet.DisplayName,
    ASet.DisplayName = 'Images');
  for I := 0 to ASet.Count - 1 do
  begin
    ACategoryItem := ASet.Items[I];
    if AddCheckListBoxItem(CategoryList, ACategoryItem.DisplayName, True) then
      AddGalleryGroup(ACategoryItem.DisplayName,
        GetIndexForGalleryGroup(ACategoryItem.DisplayName), True);
    AddGalleryItems(ACategoryItem);
  end;
end;

procedure TdxPopulateContentHelper.AddGalleryItem
  (AImageItem: TdxIconLibraryImage);
var
  AGalleryItem: TdxGalleryControlItem;
begin
  AddCheckListBoxItem(SizeList, AImageItem.ImageSizeAsString,
    cxSizeIsEqual(DefaultImageSize, AImageItem.ImageSize));
  if AImageItem.Tag = 0 then
  begin
    AGalleryItem := TdxGalleryControlItem.Create(nil);
    AImageItem.Tag := TdxNativeInt(AGalleryItem);
    AGalleryItem.Caption := AImageItem.DisplayName;
    AGalleryItem.Hint := MakeHint(AImageItem);
    AGalleryItem.Tag := TdxNativeInt(AImageItem);
  end;
end;

procedure TdxPopulateContentHelper.AddGalleryItems(ACategoryItem
  : TdxIconLibraryCategory);
var
  I: Integer;
begin
  for I := 0 to ACategoryItem.Count - 1 do
    AddGalleryItem(ACategoryItem.Items[I]);
end;

procedure TdxPopulateContentHelper.DeleteUnusedItems(ACheckListBox
  : TcxCheckListBox);
var
  AItem: TcxCheckListBoxItem;
  I: Integer;
begin
  ACheckListBox.Items.BeginUpdate;
  try
    for I := ACheckListBox.Items.Count - 1 downto 0 do
    begin
      AItem := ACheckListBox.Items[I];
      if AItem.Tag = TAG_UNUSED then
        AItem.Free;
    end;
  finally
    ACheckListBox.Items.EndUpdate;
  end;
end;

function TdxPopulateContentHelper.FindCheckListBoxItem(ACheckListBox
  : TcxCheckListBox; const AText: string;
  out AItem: TcxCheckListBoxItem): Boolean;
var
  I: Integer;
begin
  for I := 0 to ACheckListBox.Items.Count - 1 do
  begin
    AItem := ACheckListBox.Items[I];
    if AItem.Text = AText then
      Exit(True);
  end;
  Result := False;
end;

function TdxPopulateContentHelper.MakeHint(AImageItem
  : TdxIconLibraryImage): string;
var
  ACollectionName: string;
  AGroupName: string;
begin
  AGroupName := '-';
  ACollectionName := '-';
  if AImageItem.Parent is TdxIconLibraryCategory then
  begin
    AGroupName := TdxIconLibraryCategory(AImageItem.Parent).DisplayName;
    if AImageItem.Parent.Parent is TdxIconLibraryCollection then
      ACollectionName := TdxIconLibraryCollection(AImageItem.Parent.Parent)
        .DisplayName;
  end;
  Result := Format(sdxGalleryItemHint, [AImageItem.DisplayName, AGroupName,
    ACollectionName]);
end;

procedure TdxPopulateContentHelper.MarkItemsUnused(ACheckListBox
  : TcxCheckListBox);
var
  I: Integer;
begin
  for I := 0 to ACheckListBox.Items.Count - 1 do
    ACheckListBox.Items[I].Tag := TAG_UNUSED;
end;

function TdxPopulateContentHelper.GetIndexForCheckListItem
  (ACheckListBox: TcxCheckListBox; const AText: string): Integer;
begin
  Result := 0;
  while (Result < ACheckListBox.Items.Count) and
    (AnsiCompareStr(AText, ACheckListBox.Items[Result].Text) > 0) do
    Inc(Result);
end;

function TdxPopulateContentHelper.GetIndexForGalleryGroup(const ANameItem
  : string): Integer;
begin
  Result := 0;
  while (Result < Gallery.Gallery.Groups.Count) and
    (AnsiCompareStr(ANameItem, Gallery.Gallery.Groups[Result].Caption) > 0) do
    Inc(Result);
end;

{ TdxPopulateGalleryHelper }

constructor TdxPopulateGalleryHelper.Create(AImagePicker: TdxfmImagePicker);
begin
  inherited;
  FImagesToLoad := TList.Create;
end;

destructor TdxPopulateGalleryHelper.Destroy;
begin
  FIsDestroying := True;
  StopLoadImagesThread;
  DestroyGalleryItems;
  FreeAndNil(FImagesToLoad);
  inherited Destroy;
end;

procedure TdxPopulateGalleryHelper.PopulateCore;
var
  ASearchString: TdxSearchString;
  I: Integer;
begin
  StopLoadImagesThread;

  FMaxSize := cxNullSize;
  ASearchString := TdxSearchString.Create(ImagePicker.beFind.Text);
  for I := 0 to Gallery.Gallery.Groups.Count - 1 do
    Gallery.Gallery.Groups[I].Visible := False;
  for I := 0 to IconLibrary.Count - 1 do
    PopulateGalleryGroups(IconLibrary[I], ASearchString);
  Gallery.OptionsView.Item.Image.Size.Size := FMaxSize;

  StartLoadImagesThread;
end;

procedure TdxPopulateGalleryHelper.LoadImagesTaskComplete;
begin
  FLoadImagesTaskHandle := 0;
end;

procedure TdxPopulateGalleryHelper.StartLoadImagesThread;
begin
  if FLoadImagesTaskHandle = 0 then
    FLoadImagesTaskHandle := dxTasksDispatcher.Run
      (TdxLoadImagesTask.Create(Gallery, FImagesToLoad), LoadImagesTaskComplete,
      tmcmSync);
  FImagesToLoad.Clear;
end;

procedure TdxPopulateGalleryHelper.StopLoadImagesThread;
begin
  if FLoadImagesTaskHandle <> 0 then
    dxTasksDispatcher.Cancel(FLoadImagesTaskHandle, True);
end;

procedure TdxPopulateGalleryHelper.DestroyGalleryItems(ASet: TdxIconLibrarySet);
var
  I: Integer;
begin
  for I := 0 to ASet.Count - 1 do
    DestroyGalleryItems(ASet.Items[I]);
end;

procedure TdxPopulateGalleryHelper.DestroyGalleryItem
  (AImage: TdxIconLibraryImage);
var
  AItem: TdxGalleryItem;
begin
  if not FIsDestroying then
  begin
    Selection.BeginUpdate;
    try
      while True do
      begin
        AItem := Selection.Gallery.FindItemByTag(TdxNativeInt(AImage));
        if AItem <> nil then
          AItem.Free
        else
          Break;
      end;
    finally
      Selection.EndUpdate;
    end;
  end;

  GetGalleryItem(AImage).Free;
  AImage.Tag := 0;
end;

procedure TdxPopulateGalleryHelper.DestroyGalleryItems
  (ACategory: TdxIconLibraryCategory);
var
  I: Integer;
begin
  for I := 0 to ACategory.Count - 1 do
    DestroyGalleryItem(ACategory.Items[I]);
end;

procedure TdxPopulateGalleryHelper.DestroyGalleryItems;
var
  I: Integer;
begin
  for I := 0 to IconLibrary.Count - 1 do
    DestroyGalleryItems(IconLibrary.Items[I]);
end;

function TdxPopulateGalleryHelper.GetGalleryItem(AIconLibraryImage
  : TdxIconLibraryImage): TdxGalleryControlItem;
begin
  Result := TdxGalleryControlItem(AIconLibraryImage.Tag);
end;

function TdxPopulateGalleryHelper.GetIndexForItem
  (AGroup: TdxGalleryControlGroup; const ANameItem: string): Integer;
var
  ACompareResult: Integer;
  AHigh: Integer;
  ALow: Integer;
  AMiddle: Integer;
begin
  if AGroup.ItemCount = 0 then
    Exit(0);

  ALow := 0;
  AHigh := AGroup.ItemCount - 1;
  while ALow <= AHigh do
  begin
    AMiddle := ALow + (AHigh - ALow) shr 1;
    ACompareResult := AnsiCompareText(AGroup.Items[AMiddle].Caption, ANameItem);
    if ACompareResult < 0 then
      ALow := AMiddle + 1
    else
      AHigh := AMiddle - 1;
  end;
  Result := ALow;
end;

function TdxPopulateGalleryHelper.GetPopulateGalleryMode
  : TdxPopulateGalleryMode;
begin
  Result := ImagePicker.PopulateGalleryMode;
end;

procedure TdxPopulateGalleryHelper.PopulateGalleryGroups(ACollectionItem
  : TdxIconLibrarySet; const ASearchString: TdxSearchString);
var
  ACategoryItem: TdxIconLibraryCategory;
  AGroup: TdxGalleryControlGroup;
  AIsCategoryVisible: Boolean;
  AIsCollectionVisible: Boolean;
  I, J: Integer;
begin
  AIsCollectionVisible := IsListBoxCheckedByText(CollectionList,
    ACollectionItem.DisplayName);
  for I := 0 to ACollectionItem.Count - 1 do
  begin
    ACategoryItem := ACollectionItem[I];
    AIsCategoryVisible := AIsCollectionVisible and
      IsListBoxCheckedByText(CategoryList, ACategoryItem.DisplayName);
    if AIsCategoryVisible then
    begin
      if PopulateGalleryMode = gpmSearch then
        AGroup := GalleryGroupSearch
      else if not Gallery.Gallery.Groups.FindByCaption
        (ACategoryItem.DisplayName, AGroup) then
        raise EdxException.CreateFmt('Internal Error: %s group was not found',
          [ACategoryItem.DisplayName]);

      PopulateGalleryImages(AGroup, ACategoryItem, ASearchString);
      AGroup.Visible := AGroup.ItemCount > 0;
    end
    else
      for J := 0 to ACategoryItem.Count - 1 do
        GetGalleryItem(ACategoryItem[J]).Group := GalleryGroupHidden;
  end;
end;

procedure TdxPopulateGalleryHelper.PopulateGalleryImages
  (AGroup: TdxGalleryControlGroup; ACategory: TdxIconLibraryCategory;
  const ASearchString: TdxSearchString);
var
  AGalleryItem: TdxGalleryControlItem;
  AImageItem: TdxIconLibraryImage;
  I: Integer;
begin
  for I := 0 to ACategory.Count - 1 do
  begin
    AImageItem := ACategory[I];
    AGalleryItem := GetGalleryItem(AImageItem);
    if IsListBoxCheckedByText(SizeList, AImageItem.ImageSizeAsString) and
      ASearchString.Check(AImageItem.DisplayName) then
    begin
      AGalleryItem.Group := AGroup;
      AGalleryItem.Index := GetIndexForItem(AGroup, AImageItem.DisplayName);
      AGalleryItem.ImageIndex := NoPreviewImageIndex;
      FMaxSize := cxSizeMax(FMaxSize, AImageItem.ImageSize);
      if AGalleryItem.Glyph.Empty then
        FImagesToLoad.Add(AImageItem);
    end
    else
      AGalleryItem.Group := GalleryGroupHidden;
  end;
end;

{ TdxfmImagePicker }

function TdxfmImagePicker.ExecuteImageCollectionEditor(AFiles: TStrings;
  const ASuggestedImageSize: TSize): Boolean;
var
  I: Integer;
begin
  MultiSelect := True;
  PopulateContent;
  SelectSize(ASuggestedImageSize);
  PopulateGallery;

  Result := (ShowModal = mrOk) and HasSelection;
  if Result then
  begin
    for I := 0 to gcSelectionGroup.ItemCount - 1 do
      AFiles.Add(GetIconLibraryImage(gcSelectionGroup.Items[I]).FileName);
  end;

  gcSelectionGroup.Items.Clear;
  UpdateGalleryItemsSelection(gcIcons, False);
end;

procedure TdxfmImagePicker.Initialize;
begin
  inherited Initialize;
  MultiSelect := False;
  pcMain.ActivePage := tsPictureEditor;
  PopulateContent;
  PopulateGallery;
end;

procedure TdxfmImagePicker.SaveSettings(APicture: TPicture);
var
  ACheckedItem: TdxGalleryControlItem;
begin
  if pcMain.ActivePage = tsDXImageGallery then
  begin
    ACheckedItem := gcIcons.Gallery.GetCheckedItem;
    if (ACheckedItem = nil) or not ACheckedItem.Group.Visible then
      Exit;
    SetImage(ACheckedItem.Glyph);
  end;
  inherited;
end;

procedure TdxfmImagePicker.SelectSize(const ASize: TSize);
var
  AItem: TcxCheckListBoxItem;
  ASizeIsEmpty: Boolean;
  ASizeString: string;
  I: Integer;
begin
  clbSize.Items.BeginUpdate;
  try
    ASizeIsEmpty := cxSizeIsEmpty(ASize);
    ASizeString := cxSizeToString(ASize);
    for I := 0 to clbSize.Count - 1 do
    begin
      AItem := clbSize.Items[I];
      AItem.Checked := ASizeIsEmpty or (AItem.Text = ASizeString) or
        (AItem.Text = TdxIconLibrary.SizeVector);
    end;
  finally
    clbSize.Items.EndUpdate;
  end;
end;

procedure TdxfmImagePicker.AddToSelection(AImageItem: TdxGalleryControlItem);
var
  AItem: TdxGalleryControlItem;
begin
  gcSelection.BeginUpdate;
  try
    AItem := gcSelectionGroup.Items.Add;
    AItem.Caption := AImageItem.Caption;
    AItem.Glyph.Assign(AImageItem.Glyph);
    AItem.Tag := AImageItem.Tag;
  finally
    gcSelection.EndUpdate;
  end;
end;

function TdxfmImagePicker.GetIconLibraryImage(AGalleryControlItem
  : TdxGalleryControlItem): TdxIconLibraryImage;
begin
  Result := TdxIconLibraryImage(AGalleryControlItem.Tag);
end;

function TdxfmImagePicker.HasSelectedButNotAddedItems: Boolean;
var
  AList: TList;
  I: Integer;
begin
  if MultiSelect then
  begin
    AList := TList.Create;
    try
      gcIcons.Gallery.GetCheckedItems(AList);
      for I := 0 to AList.Count - 1 do
      begin
        if gcSelection.Gallery.FindItemByTag(TdxGalleryControlItem(AList.List[I]
          ).Tag) = nil then
          Exit(True);
      end;
    finally
      AList.Free;
    end;
  end;
  Result := False;
end;

function TdxfmImagePicker.HasSelection: Boolean;
begin
  if MultiSelect then
    Result := gcSelectionGroup.ItemCount > 0
  else
    Result := gcIcons.Gallery.GetCheckedItem <> nil;
end;

procedure TdxfmImagePicker.OnChanged(Sender: TdxIconLibraryCollection);
begin
  PopulateContent;
  PopulateGallery;
  FPopulateGalleryHelper.EndUpdate;
  HideHourglassCursor;
end;

procedure TdxfmImagePicker.OnChanging(Sender: TdxIconLibraryCollection);
begin
  ShowHourglassCursor;
  FPopulateGalleryHelper.BeginUpdate;
end;

procedure TdxfmImagePicker.OnRemoving(Sender: TdxIconLibraryCustomObject);
begin
  if Sender is TdxIconLibrarySet then
    FPopulateGalleryHelper.DestroyGalleryItems(TdxIconLibrarySet(Sender))
  else if Sender is TdxIconLibraryCategory then
    FPopulateGalleryHelper.DestroyGalleryItems(TdxIconLibraryCategory(Sender))
  else if Sender is TdxIconLibraryImage then
    FPopulateGalleryHelper.DestroyGalleryItem(TdxIconLibraryImage(Sender));
end;

procedure TdxfmImagePicker.PopulateContent;
begin
  with TdxPopulateContentHelper.Create(Self) do
    try
      Populate;
    finally
      Free;
    end;
end;

procedure TdxfmImagePicker.PopulateGallery;
begin
  if FPopulateGalleryLockCount = 0 then
    FPopulateGalleryHelper.Populate;
end;

procedure TdxfmImagePicker.SetMultiSelect(AValue: Boolean);
begin
  FMultiSelect := AValue;
  tsPictureEditor.TabVisible := not MultiSelect;
  liSelection.Visible := MultiSelect;
  if MultiSelect then
    gcIcons.OptionsBehavior.ItemCheckMode := icmMultiple
  else
    gcIcons.OptionsBehavior.ItemCheckMode := icmSingleRadio;
end;

procedure TdxfmImagePicker.UpdateGalleryItemsSelection
  (AGallery: TdxGalleryControl; ASelect: Boolean);
var
  I: Integer;
begin
  AGallery.BeginUpdate;
  try
    for I := 0 to AGallery.Gallery.Groups.Count - 1 do
      UpdateGalleryItemsSelection(AGallery.Gallery.Groups[I], ASelect);
  finally
    AGallery.EndUpdate;
  end;
end;

procedure TdxfmImagePicker.UpdateGalleryItemsSelection
  (AGroup: TdxGalleryControlGroup; ASelect: Boolean);
var
  I: Integer;
begin
  if AGroup <> nil then
  begin
    AGroup.Items.BeginUpdate;
    try
      for I := 0 to AGroup.ItemCount - 1 do
        AGroup.Items[I].Checked := ASelect;
    finally
      AGroup.Items.EndUpdate(False);
    end;
  end;
end;

procedure TdxfmImagePicker.UpdateSelectionBoxSize;
var
  AGallery: TdxGalleryControlAccess;
  AImageSize: TSize;
  I: Integer;
begin
  AGallery := TdxGalleryControlAccess(gcSelection);

  AImageSize := cxNullSize;
  for I := 0 to gcSelectionGroup.ItemCount - 1 do
    AImageSize := cxSizeMax(AImageSize, gcSelectionGroup.Items[I].Glyph.Size);
  AGallery.OptionsView.Item.Image.Size.Size := AImageSize;

  if AGallery.ViewInfo.RowCount > 0 then
    AGallery.Height := cxMarginsHeight(AGallery.ViewInfo.ContentOffset) +
      AGallery.BorderSize * 2 + cxRectHeight(AGallery.ViewInfo.ContentBounds) -
      AGallery.ViewInfo.ItemSize.cy * Max(AGallery.ViewInfo.RowCount - 2, 0);
end;

procedure TdxfmImagePicker.FormCreate(Sender: TObject);
begin
  inherited;
  FPopulateGalleryMode := gpmGroups;
  FPopulateGalleryHelper := TdxPopulateGalleryHelper.Create(Self);

  clbSize.InnerCheckListBox.MultiSelect := True;
  clbCollection.InnerCheckListBox.MultiSelect := True;
  clbCategories.InnerCheckListBox.MultiSelect := True;

  dxIconLibrary.Listeners.Add(Self);

  UpdateSelectionBoxSize;
end;

procedure TdxfmImagePicker.FormDestroy(Sender: TObject);
begin
  dxIconLibrary.Listeners.Remove(Self);
  FreeAndNil(FPopulateGalleryHelper);
  inherited;
end;

procedure TdxfmImagePicker.beFindPropertiesButtonClick(Sender: TObject;
  AButtonIndex: Integer);
begin
  beFind.Text := '';
end;

procedure TdxfmImagePicker.beFindPropertiesChange(Sender: TObject);
const
  PopulateModeToImageIndex: array [TdxPopulateGalleryMode] of Integer = (0, 1);
begin
  if beFind.Text <> '' then
    FPopulateGalleryMode := gpmSearch
  else
    FPopulateGalleryMode := gpmGroups;

  beFind.Properties.Buttons[0].ImageIndex := PopulateModeToImageIndex
    [FPopulateGalleryMode];
  PopulateGallery;
end;

procedure TdxfmImagePicker.btnOkClick(Sender: TObject);
begin
  if MultiSelect and HasSelectedButNotAddedItems then
  begin
    case MessageDlg(sdxAddToSelectionConfirmation, mtConfirmation,
      [mbYes, mbNo, mbCancel], 0) of
      mrYes:
        miIconsAddToSelection.Click;
      mrCancel:
        Exit;
    end;
  end;
  ModalResult := mrOk;
end;

procedure TdxfmImagePicker.clbCategoriesClickCheck(Sender: TObject;
  AIndex: Integer; APrevState, ANewState: TcxCheckBoxState);
begin
  gcIcons.Gallery.Groups[AIndex].Visible := (ANewState = cbsChecked) and
    (FPopulateGalleryMode = gpmGroups);
  PopulateGallery;
end;

procedure TdxfmImagePicker.clbCollectionClickCheck(Sender: TObject;
  AIndex: Integer; APrevState, ANewState: TcxCheckBoxState);
begin
  PopulateGallery;
end;

procedure TdxfmImagePicker.gcIconsDblClick(Sender: TObject);
var
  AItem: TdxGalleryControlItem;
begin
  AItem := gcIcons.Gallery.Groups.GetItemAtPos(gcIcons.MouseDownPos);
  if AItem <> nil then
  begin
    UpdateGalleryItemsSelection(gcIcons, False);
    AItem.Checked := True;
    if MultiSelect then
      miIconsAddToSelection.Click
    else
      ModalResult := mrOk;
  end;
end;

procedure TdxfmImagePicker.gcSelectionDragDrop(Sender, Source: TObject;
  X, Y: Integer);
begin
  inherited;
  UpdateSelectionBoxSize;
end;

procedure TdxfmImagePicker.gcSelectionDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := Source is TdxGalleryControlDragObject;
  if Accept then
    gcSelection.Controller.DragCopy := TdxGalleryControlDragObject(Source)
      .Control <> Sender;
end;

procedure TdxfmImagePicker.miIconsSelectionDeleteSelectedClick(Sender: TObject);
var
  AIndex: Integer;
  ASelection: TList;
begin
  ASelection := TList.Create;
  try
    gcSelection.Gallery.GetCheckedItems(ASelection);
    if ASelection.Count > 0 then
    begin
      gcSelection.BeginUpdate;
      try
        gcSelection.Gallery.UncheckAll;
        AIndex := TdxGalleryControlItem(ASelection.Last).Index;
        if AIndex + 1 < gcSelectionGroup.ItemCount then
          gcSelectionGroup.Items[AIndex + 1].Checked := True;
        for AIndex := 0 to ASelection.Count - 1 do
          TObject(ASelection.List[AIndex]).Free;
        if (gcSelectionGroup.ItemCount > 0) and
          (gcSelection.Gallery.GetCheckedItem = nil) then
          gcSelectionGroup.Items[gcSelectionGroup.ItemCount - 1]
            .Checked := True;
      finally
        gcSelection.EndUpdate;
      end;
      UpdateSelectionBoxSize;
    end;
  finally
    ASelection.Free;
  end;
end;

procedure TdxfmImagePicker.miIconsDeselectAllClick(Sender: TObject);
begin
  UpdateGalleryItemsSelection(gcIcons, TComponent(Sender).Tag <> 0)
end;

procedure TdxfmImagePicker.miIconsSelectAllinThisGroupClick(Sender: TObject);
begin
  UpdateGalleryItemsSelection(FSelectedGroup, TComponent(Sender).Tag <> 0);
end;

procedure TdxfmImagePicker.miIconsAddToSelectionClick(Sender: TObject);
var
  AChecked: TList;
  I: Integer;
begin
  if MultiSelect then
  begin
    AChecked := TList.Create;
    try
      gcIcons.Gallery.GetCheckedItems(AChecked);
      if AChecked.Count > 0 then
      begin
        gcSelection.BeginUpdate;
        try
          for I := 0 to AChecked.Count - 1 do
            AddToSelection(TdxGalleryControlItem(AChecked.List[I]));
        finally
          gcSelection.EndUpdate;
        end;
        UpdateSelectionBoxSize;
      end;
    finally
      AChecked.Free;
    end;
  end;
end;

procedure TdxfmImagePicker.miIconsSelectionDeselectAllClick(Sender: TObject);
begin
  UpdateGalleryItemsSelection(gcSelection, TComponent(Sender).Tag <> 0);
end;

procedure TdxfmImagePicker.miSelectClick(Sender: TObject);
var
  AActiveComponent: TComponent;
  ACheckListBox: TcxCustomCheckListBox;
  I: Integer;
begin
  AActiveComponent := TComponent(ActiveControl);
  if AActiveComponent is TcxCustomInnerCheckListBox then
  begin
    ACheckListBox := TcxCustomInnerCheckListBox(AActiveComponent).Container;
    for I := 0 to ACheckListBox.Items.Count - 1 do
      ACheckListBox.Selected[I] := TComponent(Sender).Tag <> 0;
  end;
end;

procedure TdxfmImagePicker.miIconsShowInExplorerClick(Sender: TObject);
var
  ACheckedItem: TdxGalleryControlItem;
begin
  ACheckedItem := gcIcons.Gallery.GetCheckedItem;
  if ACheckedItem <> nil then
    dxShowInExplorer(GetIconLibraryImage(ACheckedItem).FileName);
end;

procedure TdxfmImagePicker.miUncheckSelectedClick(Sender: TObject);
const
  StateMap: array [Boolean] of TcxCheckBoxState = (cbsUnchecked, cbsChecked);
var
  ACheckListBox: TcxCheckListBox;
  ANewState: TcxCheckBoxState;
  APrevState: TcxCheckBoxState;
  I: Integer;
begin
  if pmSelection.PopupComponent is TcxCustomInnerCheckListBox then
  begin
    ACheckListBox := TcxCustomInnerCheckListBox(pmSelection.PopupComponent)
      .Container as TcxCheckListBox;
    gcIcons.BeginUpdate;
    try
      Inc(FPopulateGalleryLockCount);
      try
        for I := 0 to ACheckListBox.Items.Count - 1 do
        begin
          if ACheckListBox.Selected[I] then
          begin
            ANewState := StateMap[TComponent(Sender).Tag <> 0];
            APrevState := StateMap[ACheckListBox.Items[I].Checked];
            if APrevState <> ANewState then
            begin
              ACheckListBox.Items[I].Checked := ANewState = cbsChecked;
              if Assigned(ACheckListBox.OnClickCheck) then
                ACheckListBox.OnClickCheck(ACheckListBox, I, APrevState,
                  ANewState);
            end;
          end;
        end;
      finally
        Dec(FPopulateGalleryLockCount);
      end;
      PopulateGallery;
    finally
      gcIcons.EndUpdate;
    end;
  end;
end;

procedure TdxfmImagePicker.pmIconGalleryPopup(Sender: TObject);
var
  AHasSelection: Boolean;
  ASelectedGroupName: string;
begin
  AHasSelection := gcIcons.Gallery.GetCheckedItem <> nil;

  if MultiSelect then
    FSelectedGroup := gcIcons.Gallery.Groups.GetGroupAtPos
      (gcIcons.ScreenToClient(GetMouseCursorPos))
  else
    FSelectedGroup := nil;

  if FSelectedGroup <> nil then
    ASelectedGroupName := FSelectedGroup.Caption
  else
    ASelectedGroupName := '';

  miIconsAddToSelection.Visible := MultiSelect;
  miIconsAddToSelection.Enabled := AHasSelection;
  miIconsShowInExplorer.Visible := AHasSelection;

  miIconsDeselectAll.Enabled := AHasSelection;
  miIconsDeselectAll.Visible := MultiSelect;
  miIconsDeselectAllinThisGroup.Enabled := AHasSelection;
  miIconsDeselectAllinThisGroup.Caption := Format(sdxGalleryDeselectAllInGroup,
    [ASelectedGroupName]);
  miIconsDeselectAllinThisGroup.Visible := FSelectedGroup <> nil;

  miIconsSelectAll.Visible := MultiSelect;
  miIconsSelectAllinThisGroup.Caption := Format(sdxGallerySelectAllInGroup,
    [ASelectedGroupName]);
  miIconsSelectAllinThisGroup.Visible := FSelectedGroup <> nil;
end;

procedure TdxfmImagePicker.pmIconsSelectionPopup(Sender: TObject);
var
  AHasSelection: Boolean;
begin
  AHasSelection := gcSelection.Gallery.GetCheckedItem <> nil;
  miIconsSelectionDeleteSelected.Enabled := AHasSelection;
  miIconsSelectionDeselectAll.Enabled := AHasSelection;
end;

procedure TdxfmImagePicker.pmSelectionPopup(Sender: TObject);
begin
  miCheckSelected.Visible := pmSelection.PopupComponent is
    TcxCustomInnerCheckListBox;
  miUncheckSelected.Visible := pmSelection.PopupComponent is
    TcxCustomInnerCheckListBox;
end;

procedure TdxfmImagePicker.actF3Execute(Sender: TObject);
begin
  beFind.SetFocus;
end;

procedure TdxfmImagePicker.FormShortCut(var Msg: TWMKey; var Handled: Boolean);
begin
  if beFind.Focused then
    case Msg.CharCode of
      VK_ESCAPE:
        begin
          beFind.Text := '';
          Handled := True;
        end;
      VK_RETURN:
        begin
          gcIcons.SetFocus;
          Handled := True;
        end;
    end;
end;

procedure TdxfmImagePicker.pcMainChange(Sender: TObject);
begin
  if pcMain.ActivePage = tsDXImageGallery then
    ActiveControl := gcIcons;
end;

initialization

CreateIconLibrary;

finalization

DestroyIconLibrary;

end.
