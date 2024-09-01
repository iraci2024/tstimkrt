{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ Express Cross Platform Library classes }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL }
{ ACCOMPANYING VCL AND CLX CONTROLS AS PART OF AN EXECUTABLE }
{ PROGRAM ONLY. }
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

unit dxShellControls; // for internal use only

{$I cxVer.inc}

interface

uses
  Windows, SysUtils, Classes, Controls, AppEvnts, ShlObj, ShellAPI, Messages,
  Math, Types, CommCtrl,
  Forms, StdCtrls, ImgList, Graphics, ComCtrls,
  dxCore, dxCoreGraphics, cxControls, cxGraphics, cxGeometry,
  cxLookAndFeelPainters, cxShellCommon,
  cxShellControls, dxTreeView, dxCustomTree, dxListView, cxShellListView,
  cxCustomCanvas;

type
  TdxShellTreeView = class;
  TdxShellListView = class;

  { TdxShellTreeViewItemProducer }

  TdxShellTreeViewItemProducer = class(TdxCustomShellTreeItemProducer)
  strict private
    FNode: TdxTreeViewNode;
    //
    function GetTreeView: TdxShellTreeView;
  protected
    function CanAddFolder(AFolder: TcxShellFolder): Boolean; override;
    function CreateFakeProducer: TdxCustomShellTreeItemProducer; override;
    function GetEnumFlags: Cardinal; override;
    function GetItemsInfoGatherer: TcxShellItemsInfoGatherer; override;
    function GetShowToolTip: Boolean; override;
    //
    property Node: TdxTreeViewNode read FNode write FNode;
    property TreeView: TdxShellTreeView read GetTreeView;
  public
    constructor Create(AOwner: TWinControl); override;
    destructor Destroy; override;
    //
    procedure NotifyAddItem(Index: Integer); override;
    procedure NotifyRemoveItem(Index: Integer); override;
    procedure ProcessItems(AIFolder: IShellFolder; APIDL: PItemIDList;
      ANode: TdxTreeViewNode; APreloadItemCount: Integer); reintroduce;
      overload;
    procedure SetItemsCount(Count: Integer); override;
  end;

  { TdxShellTreeViewOptions }

  TdxShellTreeViewOptions = class(TcxShellOptions)
  protected
    procedure DoNotifyUpdateContents; override;
  published
    property FileMask;
  end;

  { TdxShellTreeViewNodeViewInfo }

  TdxShellTreeViewNodeViewInfo = class(TdxTreeViewNodeViewInfo)
  strict private
    FDrawState: TdxListViewItemStates;
    function GetState: TdxListViewItemStates;
    function GetTreeView: TdxShellTreeView;
  protected
    function GetTextColor(ASelected: Boolean): TColor; override;
    procedure DrawSelection(ACanvas: TcxCanvas); override;

    property DrawState: TdxListViewItemStates read FDrawState;
    property TreeView: TdxShellTreeView read GetTreeView;
  public
    procedure Draw(ACanvas: TcxCanvas); override;
  end;

  { TdxShellTreeViewViewInfo }

  TdxShellTreeViewViewInfo = class(TdxTreeViewViewInfo)
  protected
    function CreateNodeViewInfo: TdxTreeViewNodeViewInfo; override;
    function GetBackgroundColor: TColor; override;
  end;

  TdxShellTreeViewStateData = record
    CurrentPath: PItemIDList;
    ExpandedNodeList: TList;
    ViewPort: TPoint;
  end;

  { TdxShellTreeView }

  TdxShellTreeView = class(TdxCustomTreeView, IcxShellDependedControls,
    IcxShellRoot)
  strict private
    FAbsolutePIDL: PItemIDList;
    FAppEvents: TApplicationEvents;
    FContextPopupItemProducer: TdxShellTreeViewItemProducer;
    FDependedControls: TcxShellDependedControls;
    FFirstLevelNodesVisible: Boolean;
    FInternalSmallImages: TImageList;
    FIsChangeNotificationCreationLocked: Boolean;
    FIsUpdating: Boolean;
    FItemProducersList: TThreadList;
    FItemsInfoGatherer: TcxShellItemsInfoGatherer;
    FLockVisibleUpdate: Integer;
    FLockChange: Integer;
    FNavigation: Boolean;
    FShellChangeNotificationCreation: Boolean;
    FShellChangeNotifierData: TcxShellChangeNotifierData;
    FShellOptions: TdxShellTreeViewOptions;
    FShellRoot: TcxShellTreeRoot;
    FStateData: TdxShellTreeViewStateData;
    //
    FOnAddFolder: TcxShellAddFolderEvent;
    FOnRootChanged: TcxRootChangedEvent;
    FOnShellChange: TcxShellChangeEvent;
    procedure ContextPopupItemProducerDestroyHandler(Sender: TObject);
    function CreateShellNode(AParentNode: TdxTreeViewNode;
      AShellItem: TcxShellItemInfo): TdxTreeViewNode;
    procedure DoNavigate(APIDL: PItemIDList);
    function GetAbsolutePIDL: PItemIDList;
    function GetNodeAbsolutePIDL(ANode: TdxTreeViewNode): PItemIDList;
    function GetPath: string;
    function GetShellRootNode: TdxTreeViewNode;
    procedure LockUpdateVisibleInfo;
    procedure RestoreTreeState;
    procedure RootFolderChanged(Sender: TObject; Root: TcxCustomShellRoot);
    procedure RootSettingsChanged(Sender: TObject);
    procedure SaveAbsolutePIDL(AValue: PItemIDList);
    procedure SaveTreeState;
    procedure SetAbsolutePIDL(AValue: PItemIDList);
    procedure SetFirstLevelNodesVisible(const Value: Boolean);
    procedure SetPath(AValue: string);
    procedure SetRootField(const AValue: TcxShellTreeRoot);
    procedure ShellChangeNotify(AEventID: Longint; APidl1, APidl2: PItemIDList);
    procedure UnlockUpdateVisibleInfo;
    //
    procedure DSMNotifyAddItem(var Message: TMessage);
      message DSM_NOTIFYADDITEM;
    procedure DSMNotifyRemoveItem(var Message: TMessage);
      message DSM_NOTIFYREMOVEITEM;
    procedure DSMSetCount(var Message: TMessage); message DSM_SETCOUNT;
    procedure DSMSynchronizeRoot(var Message: TMessage);
      message DSM_SYNCHRONIZEROOT;
    procedure DSMSystemShellChangeNotify(var Message: TMessage);
      message DSM_SYSTEMSHELLCHANGENOTIFY;
  private
    function IsShellRootNode(ANode: TdxTreeViewNode): Boolean;
  protected
    function CanCollapse(Sender: TdxTreeCustomNode): Boolean; override;
    function CanExpand(Sender: TdxTreeCustomNode): Boolean; override;
    function CreateViewInfo: TdxTreeViewViewInfo; override;
    procedure DeleteNode(Sender: TdxTreeCustomNode); override;
    procedure DestroyWnd; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure InitControl; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    // IcxShellDependedControls
    function GetDependedControls: TcxShellDependedControls;
    // IcxShellRoot
    function GetRoot: TcxCustomShellRoot;
    //
    procedure AddItemProducer(AProducer: TdxShellTreeViewItemProducer);
    procedure ChangeTreeContent(AChangeProc: TProc);
    function CheckFileMask(AFolder: TcxShellFolder): Boolean;
    procedure CreateChangeNotification(ANode: TdxTreeViewNode = nil);
    function DoAddFolder(AFolder: TcxShellFolder): Boolean;
    procedure DoOnIdle(Sender: TObject; var Done: Boolean);
    procedure DoSelectionChanged; override;
    function GetItemProducer(ANode: TdxTreeViewNode)
      : TdxShellTreeViewItemProducer;
    function GetNodeByPIDL(APIDL: PItemIDList; ANearestInChain: Boolean = False;
      ACheckExpanded: Boolean = False; AStartParentNode: TdxTreeViewNode = nil)
      : TdxTreeViewNode;
    function GetShellItemInfo(ANode: TdxTreeViewNode): TcxShellItemInfo;
    function HasHottrack: Boolean; override;
    procedure RemoveChangeNotification;
    procedure RemoveItemProducer(AProducer: TdxShellTreeViewItemProducer);
    function ShowFirstLevelNodes: Boolean; override;
    procedure UpdateContent;
    procedure UpdateItem(AItem: TcxShellItemInfo);
    procedure UpdateNode(ANode: TdxTreeViewNode; AFast: Boolean);
    procedure UpdateRootNodes;
    procedure UpdateVisibleItems;
    procedure UpdateViewPort(const APoint: TPoint); override;
    //
    property FirstLevelNodesVisible: Boolean read FFirstLevelNodesVisible
      write SetFirstLevelNodesVisible;
    property ItemsInfoGatherer: TcxShellItemsInfoGatherer
      read FItemsInfoGatherer;
    property ShellRootNode: TdxTreeViewNode read GetShellRootNode;
    //
    property OnRootChanged: TcxRootChangedEvent read FOnRootChanged
      write FOnRootChanged;
    property OnShellChange: TcxShellChangeEvent read FOnShellChange
      write FOnShellChange;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //
    property AbsolutePIDL: PItemIDList read GetAbsolutePIDL
      write SetAbsolutePIDL;
    property Path: string read GetPath write SetPath;
    property ScrollBars default ssBoth;
    property ShellOptions: TdxShellTreeViewOptions read FShellOptions
      write FShellOptions;
    property ShellRoot: TcxShellTreeRoot read FShellRoot write SetRootField;
    //
    property OnAddFolder: TcxShellAddFolderEvent read FOnAddFolder
      write FOnAddFolder;
    property OnKeyDown;
    property OnMouseDown;
    property OnSelectionChanged;
  end;

  { TdxShellListViewItemProducer }

  TdxShellListViewItemProducer = class(TcxCustomItemProducer)
  private
    FThumbnails: THandle;
    //
    procedure CheckThumnails;
    function GetListView: TdxShellListView;
  protected
    function CanAddFolder(AFolder: TcxShellFolder): Boolean; override;
    function DoCompareItems(AItem1, AItem2: TcxShellFolder;
      out ACompare: Integer): Boolean; override;
    procedure DoSlowInitialization(AItem: TcxShellItemInfo); override;
    function GetEnumFlags: Cardinal; override;
    function GetItemsInfoGatherer: TcxShellItemsInfoGatherer; override;
    function GetThumbnailIndex(AItem: TcxShellItemInfo): Integer; override;
    function GetShowToolTip: Boolean; override;
    function SlowInitializationDone(AItem: TcxShellItemInfo): Boolean; override;
    //
    property ListView: TdxShellListView read GetListView;
  public
    //
    procedure ProcessDetails(ShellFolder: IShellFolder;
      CharWidth: Integer); override;
    procedure ProcessItems(AIFolder: IShellFolder; AFolderPIDL: PItemIDList;
      APreloadItemCount: Integer); override;
  end;

  { TdxShellListViewContextMenu }

  TdxShellListViewContextMenu = class(TcxShellCustomContextMenu)
  private
    FListView: TdxShellListView;
  protected
    function GetWindowHandle: THandle; override;
    procedure Populate; override;
  public
    constructor Create(AListView: TdxShellListView);
    property ListView: TdxShellListView read FListView;
  end;

  TdxShellListViewContextMenuClass = class of TdxShellListViewContextMenu;

  { TdxShellListViewCurrentFolderContextMenu }

  TdxShellListViewCurrentFolderContextMenu = class(TdxShellListViewContextMenu)
  private
    FLastSortColumnIndexCommandId: Cardinal;
    procedure AddCheckItem(const ACaption: string; AId: Cardinal;
      AIsChecked: Boolean; AMenu: HMenu = 0);
    procedure AddItem(const ACaption: string; AId: Cardinal; AMenu: HMenu = 0);
    procedure AddRadioGroup(AItems: array of string;
      AStartId, AStartPos: Cardinal; AItemIndex: Integer; AMenu: HMenu = 0);
    procedure AddSeparator(AMenu: HMenu = 0);
    function AddSubItem(const ACaption: string; AMenu: HMenu = 0): HMenu;
  protected
    procedure ExecuteMenuItemCommand(ACommandId: Cardinal); override;
    procedure Populate; override;
  end;

  { TdxShellListViewOptions }

  TdxShellListViewOptions = class(TcxShellListViewOptions)
  strict private
    FShowReadOnly: Boolean;
  public
    constructor Create(AOwner: TWinControl); override;
    procedure Assign(Source: TPersistent); override;
  published
    property ShowReadOnly: Boolean read FShowReadOnly write FShowReadOnly
      default False;
  end;

  { TdxShellListViewPainter }

  TdxShellListViewPainter = class(TdxListViewPainter)
  protected
    procedure PrepareGlyphBitmap(ABitmap: TcxAlphaBitmap;
      AImages: TCustomImageList; AImageIndex, AOverlayIndex: Integer;
      ADrawMode: TcxImageDrawMode; ASmoothImage: Boolean; ABrush: THandle;
      ATransparentColor: TColor; AUseLeftBottomPixelAsTransparent: Boolean;
      APalette: IdxColorPalette); override;
  end;

  { TdxShellListItemViewInfo }

  TdxShellListItemViewInfo = class(TdxListItemViewInfo)
  strict private
    function GetShellListView: TdxShellListView;
  protected
    procedure DrawGlyph(ACanvas: TcxCustomCanvas); override;
    property ShellListView: TdxShellListView read GetShellListView;
  end;

  { TdxShellListViewViewInfo }

  TdxShellListViewViewInfo = class(TdxListViewViewInfo)
  protected
    function CreateItemViewInfo(AOwner: TdxListViewCustomGroupViewInfo;
      AItemIndex: Integer): TdxListItemCustomViewInfo; override;
    function GetIconsGlyphSideGap: Integer; override;
  end;

  { TdxShellListViewController }

  TdxShellListViewController = class(TdxListViewController)
  protected
    procedure ProcessItemMouseDown(AItemViewInfo: TdxListItemCustomViewInfo;
      AButton: TMouseButton; AShift: TShiftState;
      const AMousePos: TPoint); override;
  public
    procedure KeyDown(AKey: Word; AShift: TShiftState); override;
    procedure SelectAll;
  end;

  { TdxShellListView }

  TdxNavigationEvent = procedure(Sender: TdxShellListView; APIDL: PItemIDList;
    ADisplayName: WideString) of object;

  TdxShellListView = class(TdxCustomListView, IcxShellDependedControls,
    IcxShellRoot)
  strict private
    FAbsolutePIDL: PItemIDList;
    FAppEvents: TApplicationEvents;
    FDependedControls: TcxShellDependedControls;
    FNavigateFolderLinks: Boolean;
    FFakeThumbnailImages: TCustomImageList;
    FFirstUpdateItem: Integer;
    FInternalSmallImages: TImageList;
    FIsThumbnailView: Boolean;
    FIsFinishEditing: Boolean;
    FItemProducer: TdxShellListViewItemProducer;
    FItemsInfoGatherer: TcxShellItemsInfoGatherer;
    FLargeIconSize: TcxShellIconSize;
    FLargeImages: TCustomImageList;
    FLastUpdateItem: Integer;
    FNotificationLock: Boolean;
    FSelectedFiles: TStringList;
    FShellChangeNotifierData: TcxShellChangeNotifierData;
    FShellOptions: TdxShellListViewOptions;
    FShellRoot: TcxShellListRoot;
    FSorting: Boolean;
    FThumbnailOptions: TcxShellThumbnailOptions;
    FThumbnailOverlayImageHelper: TObject;
    FViewID: Integer;
    //
    FAfterNavigation: TdxNavigationEvent;
    FBeforeNavigation: TdxNavigationEvent;
    FOnAddFolder: TcxShellAddFolderEvent;
    FOnCompare: TcxShellCompareEvent;
    FOnExecuteItem: TcxShellExecuteItemEvent;
    FOnRootChanged: TcxRootChangedEvent;
    FOnShellChange: TcxShellChangeEvent;
    FOnViewChanged: TNotifyEvent;
    //
    procedure DoUpdateContent(AChangeProc: TProc);
    function GetAbsolutePIDL: PItemIDList;
    function GetFolder(AIndex: Integer): TcxShellFolder;
    function GetFolderCount: Integer;
    function GetLargeImageListType: Integer;
    function GetPath: string;
    function GetSelectedFilePaths: TStrings;
    function GetShellRoot: TcxShellListRoot;
    procedure ResetSorting;
    procedure RootFolderChanged(Sender: TObject; Root: TcxCustomShellRoot);
    procedure RootSettingsChanged(Sender: TObject);
    procedure SaveAbsolutePIDL(AValue: PItemIDList);
    procedure SetAbsolutePIDL(AValue: PItemIDList);
    procedure SetLargeIconSize(const AValue: TcxShellIconSize);
    procedure SetPath(AValue: string);
    procedure SetShellRoot(const AValue: TcxShellListRoot);
    procedure SetSorting(const AValue: Boolean);
    procedure ShellChangeNotify(AEventID: Longint; APidl1, APidl2: PItemIDList);
    procedure UpdateColumnSortOrder(AIndex: Integer);
    procedure UpdateColumnsSortOrder;
    //
    procedure DSMSynchronizeRoot(var Message: TMessage);
      message DSM_SYNCHRONIZEROOT;
    procedure DSMSystemShellChangeNotify(var Message: TMessage);
      message DSM_SYSTEMSHELLCHANGENOTIFY;
  private
    FIsInternalItemCreation: Boolean;
  protected
    function CanEdit(AItem: TdxListItem): Boolean; override;
    function CreateController: TdxListViewController; override;
    function CreatePainter: TdxListViewPainter; override;
    function CreateViewInfo: TdxListViewViewInfo; override;
    procedure CreateWnd; override;
    procedure DblClick; override;
    procedure DestroyWnd; override;
    procedure DoChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    procedure DoColumnClick(AColumn: TdxListColumn); override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure DoEdited(AItem: TdxListItem; var ACaption: string); override;
    procedure FinishItemCaptionEditing(AAccept: Boolean = True); override;
    function InternalMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    function IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    function OwnerDataFetch(AItem: TdxListItem; ARequest: TItemRequest)
      : Boolean; override;
    function OwnerDataFind(AFind: TItemFind; const AFindString: string;
      const AFindPosition: TPoint; AFindData: Pointer; AStartIndex: Integer;
      ADirection: TSearchDirection; AWrap: Boolean): Integer; override;
    procedure ShowInplaceEdit(AItemIndex: Integer; ABounds: TRect;
      const AText: string); override;
    function SupportsItemEnabledState: Boolean; override;
    procedure ValidatePasteText(var AText: string); override;
    procedure ViewStyleChanged; override;
    //
    procedure ChangeView(AViewId: Integer);
    function CheckFileMask(AFolder: TcxShellFolder): Boolean;
    procedure CheckLargeImages;
    procedure CheckUpdateItems;
    procedure CreateChangeNotification;
    procedure CreateNewFolder;
    function CreateSelectedPidlsList: TList;
    procedure DestroySelectedPidlsList(ASelectedPidls: TList);
    procedure DisplayContextMenu(const APos: TPoint);
    function DoAddFolder(AFolder: TcxShellFolder): Boolean;
    procedure DoAfterNavigation;
    procedure DoBeforeNavigation(APIDL: PItemIDList);
    function DoCompare(AItem1, AItem2: TcxShellFolder; out ACompare: Integer)
      : Boolean; virtual;
    procedure DoCreateColumns;
    procedure DoItemDblClick(AItemIndex: Integer);
    procedure DoOnIdle(Sender: TObject; var Done: Boolean);
    procedure DoProcessDefaultCommand(AShellItem: TcxShellItemInfo); virtual;
    procedure DoProcessNavigation(AShellItem: TcxShellItemInfo);
    procedure DoViewChanged;
    function GetCurrentViewId: Integer;
    function GetItemInfo(AIndex: Integer): TcxShellItemInfo;
    function GetLargeIconSize: TSize;
    function GetPidlByItemIndex(AIndex: Integer): PItemIDList;
    function GetThumbnailSize: TSize;
    function GetViewCaption(const AViewId: Integer;
      AShell32DLLHandle: THandle = 0): string;
    function GetViewOptions(AForNavigation: Boolean = False)
      : TcxShellViewOptions;
    procedure InvalidateImageList;
    function IsThumbnailView: Boolean;
    procedure Navigate(APIDL: PItemIDList); virtual;
    procedure RemoveChangeNotification;
    procedure SortColumnChanged; virtual;
    procedure ThumbnailOptionsChanged(Sender: TObject);
    procedure UpdateThumbnails;
    procedure UpdateThumbnailSize(AViewId: Integer);
    //
    procedure DSMNotifyUpdateContents(var Message: TMessage);
      message DSM_NOTIFYUPDATECONTENTS;
    procedure DSMNotifyUpdateItem(var Message: TMessage);
      message DSM_NOTIFYUPDATE;
    procedure DSMSetCount(var Message: TMessage); message DSM_SETCOUNT;
    // IcxShellDependedControls
    function GetDependedControls: TcxShellDependedControls;
    // IcxShellRoot
    function GetRoot: TcxCustomShellRoot;
    procedure InplaceEditKeyPress(Sender: TObject; var Key: Char); override;
    //
    property NavigateFolderLinks: Boolean read FNavigateFolderLinks
      write FNavigateFolderLinks;
    property FirstUpdateItem: Integer read FFirstUpdateItem
      write FFirstUpdateItem;
    property ItemProducer: TdxShellListViewItemProducer read FItemProducer;
    property ItemsInfoGatherer: TcxShellItemsInfoGatherer
      read FItemsInfoGatherer;
    property LargeImages: TCustomImageList read FLargeImages;
    property LastUpdateItem: Integer read FLastUpdateItem write FLastUpdateItem;
    //
    property ThumbnailOverlayImageHelper: TObject
      read FThumbnailOverlayImageHelper;
    property OnCompare: TcxShellCompareEvent read FOnCompare write FOnCompare;
    property OnShellChange: TcxShellChangeEvent read FOnShellChange
      write FOnShellChange;
    property OnViewChanged: TNotifyEvent read FOnViewChanged
      write FOnViewChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //
    procedure BrowseParent;
    procedure Sort; overload;
    procedure Sort(AColumnIndex: Integer; AIsAscending: Boolean); overload;
    procedure UpdateContent;
    //
    property AbsolutePIDL: PItemIDList read GetAbsolutePIDL
      write SetAbsolutePIDL;
    property DependedControls: TcxShellDependedControls read FDependedControls
      write FDependedControls;
    property FolderCount: Integer read GetFolderCount;
    property Folders[AIndex: Integer]: TcxShellFolder read GetFolder;
    property LargeIconSize: TcxShellIconSize read FLargeIconSize
      write SetLargeIconSize;
    property Path: string read GetPath write SetPath;
    property SelectedFilePaths: TStrings read GetSelectedFilePaths;
    property Sorting: Boolean read FSorting write SetSorting default False;
    property ShellOptions: TdxShellListViewOptions read FShellOptions
      write FShellOptions;
    property ShellRoot: TcxShellListRoot read GetShellRoot write SetShellRoot;
    property ThumbnailOptions: TcxShellThumbnailOptions read FThumbnailOptions
      write FThumbnailOptions;
    property Options;
    //
    property AfterNavigation: TdxNavigationEvent read FAfterNavigation
      write FAfterNavigation;
    property BeforeNavigation: TdxNavigationEvent read FBeforeNavigation
      write FBeforeNavigation;
    property OnAddFolder: TcxShellAddFolderEvent read FOnAddFolder
      write FOnAddFolder;
    property OnExecuteItem: TcxShellExecuteItemEvent read FOnExecuteItem
      write FOnExecuteItem;
    property OnRootChanged: TcxRootChangedEvent read FOnRootChanged
      write FOnRootChanged;
    property OnSelectionChanged;
  published
    property ViewStyle;
    //
    property OnSelectItem;
  end;

implementation

uses
  IOUtils, RTLConsts, dxGDIPlusClasses, dxCoreClasses, dxTypeHelpers,
  cxEditConsts,
  ActiveX, ComObj;

const
  dxShellExtraLargeIconSize: TSize = (cx: 256; cy: 256);
  dxShellLargeIconSize: TSize = (cx: 96; cy: 96);
  dxShellMediumIconSize: TSize = (cx: 48; cy: 48);

type
  PPItemIDList = ^PItemIDList;
  TcxShellTreeRootAccess = class(TcxShellTreeRoot);

  TcxShellThumbnailOptionsAccess = class(TcxShellThumbnailOptions);

  TdxListItemCustomViewInfoHelper = class helper for TdxListItemCustomViewInfo
  private
    function GetItemIndex: Integer;
  public
    property ItemIndex: Integer read GetItemIndex;
  end;

  TcxBitmap32Helper = class helper for TcxBitmap32
  public
    procedure SmoothResize(ANewWidth, ANewHeight: Integer);
  end;

  { TThumbnailOverlayImageHelper }

  TThumbnailOverlayImageHelper = class
  public type
    TIconInfo = class
    public
      FileName: string;
      IconIndex: Integer;
      Image: TcxCanvasBasedImage;
      constructor Create(const AFileName: string; AIconIndex: Integer);
      destructor Destroy; override;
      procedure UpdateImage(ACanvas: TcxCustomCanvas;
        AIconSize, ADrawSize: Integer);
    end;
  strict private
    FLastCanvas: TcxCustomCanvas;
    FLastLoadedSize: Integer;
    FOverlayIconsData: array [1 .. 15] of TIconInfo;
    FResourceDLLName: string;
    procedure CheckReservedOverlayIconInfo;
    function GetOverlayHandlerIconInfo(const AHandlerCLSID: TCLSID;
      out AIconFileName: string; out AIconIndex: Integer): Boolean;
    function GetNextOverlayIconHandlerName(AKey: HKEY; AIndex: Integer;
      out AHandlerName: string): Boolean;
    function GetOverlayIconHandlerCLSID(AKey: HKEY; const AName: string;
      out ACLSID: TCLSID): Boolean;
    function GetResourceDLLName: string;
    procedure PopulateOverlayIconsData;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Initialize(ACanvas: TcxCustomCanvas; ASize: Integer);
    procedure Draw(ACanvas: TcxCustomCanvas; const ABounds: TRect;
      AOverlayIndex: Integer);
  end;

function IsValidShellNameChar(AChar: Char): Boolean;
begin
  Result := (AChar <> '\') and (AChar <> '/') and (AChar <> '"') and
    (AChar <> ':') and (AChar <> '*') and (AChar <> '?') and (AChar <> '<') and
    (AChar <> '>') and (AChar <> '|');
end;

function GetExplorerShowCheckBoxes: Boolean;
const
  KeyName = 'SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\Advanced';
var
  AValue, ASize: DWORD;
  ASubKey: HKEY;
begin
  Result := False;
  if RegOpenKeyEx(HKEY_CURRENT_USER, KeyName, 0, KEY_READ, ASubKey) = ERROR_SUCCESS
  then
    try
      ASize := SizeOf(AValue);
      if RegQueryValueExW(ASubKey, 'AutoCheckSelect', nil, nil, @AValue, @ASize)
        = ERROR_SUCCESS then
        Result := AValue = 1;
    finally
      RegCloseKey(ASubKey);
    end;
end;

{ TdxListItemCustomViewInfoHelper }

function TdxListItemCustomViewInfoHelper.GetItemIndex: Integer;
begin
  Result := inherited ItemIndex;
end;

{ TcxBitmap32Helper }

procedure TcxBitmap32Helper.SmoothResize(ANewWidth, ANewHeight: Integer);
begin
  Resize(ANewWidth, ANewHeight, True, True);
end;

{ TThumbnailOverlayImageHelper.TIconInfo }

constructor TThumbnailOverlayImageHelper.TIconInfo.Create(const AFileName
  : string; AIconIndex: Integer);
begin
  inherited Create;
  FileName := AFileName;
  IconIndex := AIconIndex;
end;

destructor TThumbnailOverlayImageHelper.TIconInfo.Destroy;
begin
  Image.Free;
  inherited Destroy;
end;

procedure TThumbnailOverlayImageHelper.TIconInfo.UpdateImage
  (ACanvas: TcxCustomCanvas; AIconSize, ADrawSize: Integer);
const
  LOAD_LIBRARY_AS_IMAGE_RESOURCE = $00000020;
var
  ALibrary: THandle;
  AIcon: HICON;
  AVclIcon: TIcon;
  ABitmap: TcxBitmap32;
begin
  FreeAndNil(Image);
  ALibrary := LoadLibraryEx(PChar(FileName), 0, LOAD_LIBRARY_AS_IMAGE_RESOURCE);
  if ALibrary > 0 then
  begin
    AIcon := LoadImage(ALibrary, MAKEINTRESOURCE(IconIndex), IMAGE_ICON,
      AIconSize, AIconSize, LR_CREATEDIBSECTION);
    if AIcon <> 0 then
    begin
      AVclIcon := TIcon.Create;
      try
        AVclIcon.Handle := AIcon;
        ABitmap := TcxBitmap32.Create;
        try
          ABitmap.Assign(AVclIcon);
          ABitmap.SmoothResize(ADrawSize, ADrawSize);
          Image := ACanvas.CreateImage(ABitmap, afDefined);
        finally
          ABitmap.Free;
        end;
      finally
        AVclIcon.Free;
      end;
      DestroyIcon(AIcon);
    end;
    FreeLibrary(ALibrary);
  end;
end;

{ TThumbnailOverlayImageHelper }

constructor TThumbnailOverlayImageHelper.Create;
begin
  inherited Create;
  FResourceDLLName := GetResourceDLLName;
  PopulateOverlayIconsData;
end;

destructor TThumbnailOverlayImageHelper.Destroy;
var
  I: Integer;
begin
  for I := Low(FOverlayIconsData) to High(FOverlayIconsData) do
    FOverlayIconsData[I].Free;
  inherited Destroy;
end;

procedure TThumbnailOverlayImageHelper.Draw(ACanvas: TcxCustomCanvas;
  const ABounds: TRect; AOverlayIndex: Integer);
var
  AIconInfo: TIconInfo;
  AImage: TcxCanvasBasedImage;
  ADrawBounds: TRect;
begin
  Inc(AOverlayIndex);
  if InRange(AOverlayIndex, 1, 15) then
  begin
    AIconInfo := FOverlayIconsData[AOverlayIndex];
    if AIconInfo <> nil then
    begin
      AImage := AIconInfo.Image;
      if AImage <> nil then
      begin
        ADrawBounds.Init(ABounds.Left, ABounds.Bottom - AImage.Height,
          ABounds.Left + AImage.Width, ABounds.Bottom);
        AIconInfo.Image.Draw(ADrawBounds);
      end;
    end;
  end;
end;

procedure TThumbnailOverlayImageHelper.Initialize(ACanvas: TcxCustomCanvas;
  ASize: Integer);
var
  I, AIconSize, ADrawSize: Integer;
begin
  if (ASize <> FLastLoadedSize) or (FLastCanvas <> ACanvas) then
  begin
    FLastLoadedSize := ASize;
    FLastCanvas := ACanvas;
    if ASize > 96 then
    begin
      AIconSize := 256;
      ADrawSize := MulDiv(ASize, 3, 8)
    end
    else if ASize > 48 then
    begin
      AIconSize := 96;
      ADrawSize := ASize div 2;
    end
    else
    begin
      AIconSize := 48;
      ADrawSize := MulDiv(ASize, 2, 3);
    end;
    for I := Low(FOverlayIconsData) to High(FOverlayIconsData) do
    begin
      if FOverlayIconsData[I] <> nil then
        FOverlayIconsData[I].UpdateImage(ACanvas, AIconSize, ADrawSize);
    end;
  end;
end;

function TThumbnailOverlayImageHelper.GetNextOverlayIconHandlerName(AKey: HKEY;
  AIndex: Integer; out AHandlerName: string): Boolean;
var
  AStatus, ASize: DWORD;
begin
  Result := False;
  ASize := 256;
  while True do
  begin
    SetLength(AHandlerName, ASize - 1);
    AStatus := RegEnumKeyEx(AKey, AIndex, PChar(AHandlerName), ASize, nil, nil,
      nil, nil);
    case AStatus of
      ERROR_SUCCESS:
        begin
          SetLength(AHandlerName, ASize);
          Exit(True);
        end;
      ERROR_NO_MORE_ITEMS:
        begin
          AHandlerName := '';
          Exit(True);
        end;
      ERROR_MORE_DATA:
        Inc(ASize, ASize);
    else
      Exit;
    end;
  end;
end;

function TThumbnailOverlayImageHelper.GetOverlayIconHandlerCLSID(AKey: HKEY;
  const AName: string; out ACLSID: TCLSID): Boolean;
var
  ABuffer: string;
  AStatus, ABufferLength, ASize: DWORD;
  ASubKey: HKEY;
begin
  Result := False;
  if RegOpenKeyEx(AKey, PChar(AName), 0, KEY_READ, ASubKey) = ERROR_SUCCESS then
    try
      ABufferLength := 256;
      while True do
      begin
        SetLength(ABuffer, ABufferLength);
        ASize := ABufferLength * SizeOf(Char);
        AStatus := RegQueryValueExW(ASubKey, nil, nil, nil,
          @ABuffer[1], @ASize);
        case AStatus of
          ERROR_SUCCESS:
            begin
              SetLength(ABuffer, (ASize div SizeOf(Char)) - 1);
              ACLSID := StringToGUID(ABuffer);
              Exit(True);
            end;
          ERROR_MORE_DATA:
            ABufferLength := (ASize div SizeOf(Char));
        else
          Exit;
        end;
      end;
    finally
      RegCloseKey(ASubKey);
    end;
end;

function TThumbnailOverlayImageHelper.GetOverlayHandlerIconInfo
  (const AHandlerCLSID: TCLSID; out AIconFileName: string;
  out AIconIndex: Integer): Boolean;
var
  AFlags: DWORD;
  AUnknownIntf: IUnknown;
  AShellIconOverlayIdentifier: IShellIconOverlayIdentifier;
begin
  Result := False;
  if Succeeded(CoCreateInstance(AHandlerCLSID, nil, CLSCTX_INPROC_SERVER or
    CLSCTX_LOCAL_SERVER, IUnknown, AUnknownIntf)) then
    try
      if Succeeded(AUnknownIntf.QueryInterface(IID_IShellIconOverlayIdentifier,
        AShellIconOverlayIdentifier)) then
        try
          SetLength(AIconFileName, MAX_PATH);
          AIconFileName[1] := #0;
          AFlags := 0;
          AIconIndex := 0;
          if Succeeded(AShellIconOverlayIdentifier.GetOverlayInfo
            (PChar(AIconFileName), Length(AIconFileName), AIconIndex, AFlags))
          then
          begin
            AIconFileName := PChar(AIconFileName);
            if AFlags and ISIOI_ICONFILE <> 0 then
            begin
              if AFlags and ISIOI_ICONINDEX = 0 then
                AIconIndex := 0;
            end
            else
            begin
              AIconFileName := '';
              AIconIndex := 0;
            end;
            Exit(True);
          end;
        finally
          AShellIconOverlayIdentifier := nil;
        end;
    finally
      AUnknownIntf := nil;
    end;
end;

procedure TThumbnailOverlayImageHelper.CheckReservedOverlayIconInfo;
const
  IconIndices: array [Boolean, 1 .. 4] of Integer = ((29, 30, 0, 31),
    (164, 163, 157, 169));
var
  AOverlayIconIndex, AIconIndex: Integer;
begin
  for AOverlayIconIndex := 1 to 4 do
    if FOverlayIconsData[AOverlayIconIndex] = nil then
    begin
      AIconIndex := IconIndices[IsWinVistaOrLater, AOverlayIconIndex];
      if AIconIndex <> 0 then
        FOverlayIconsData[AOverlayIconIndex] :=
          TIconInfo.Create(FResourceDLLName, AIconIndex);
    end;
end;

function TThumbnailOverlayImageHelper.GetResourceDLLName: string;
const
  ResourceDLLNames: array [Boolean] of string = ('shell32.dll', 'imageres.dll');
begin
  SetLength(Result, MAX_PATH);
  OleCheck(SHGetFolderPath(0, CSIDL_SYSTEM, 0, SHGFP_TYPE_CURRENT,
    PChar(Result)));
  Result := PChar(Result) + '\' + ResourceDLLNames[IsWinVistaOrLater];
end;

procedure TThumbnailOverlayImageHelper.PopulateOverlayIconsData;
const
  ShellIconOverlayIdentifiersKeyName =
    'SOFTWARE\Microsoft\Windows\CurrentVersion\Explorer\ShellIconOverlayIdentifiers';
var
  AIndex, AOverlayIconIndex, AIconIndex: Integer;
  AKey: HKEY;
  AHandlerName, AIconFileName: string;
  ACLSID: TCLSID;
begin
  AIconIndex := 0;
  if RegOpenKeyExW(HKEY_LOCAL_MACHINE,
    PChar(ShellIconOverlayIdentifiersKeyName), 0, KEY_READ, AKey) = ERROR_SUCCESS
  then
    try
      AIndex := 0;
      while True do
      begin
        if not GetNextOverlayIconHandlerName(AKey, AIndex, AHandlerName) then
          Break;
        if AHandlerName = '' then
          Break;
        if not GetOverlayIconHandlerCLSID(AKey, AHandlerName, ACLSID) then
          Break;
        if not GetOverlayHandlerIconInfo(ACLSID, AIconFileName, AIconIndex) then
          Break;
        AOverlayIconIndex := SHGetIconOverlayIndex(PChar(AIconFileName),
          AIconIndex);
        if InRange(AOverlayIconIndex, Low(FOverlayIconsData),
          High(FOverlayIconsData)) then
        begin
          if FOverlayIconsData[AOverlayIconIndex] = nil then
            FOverlayIconsData[AOverlayIconIndex] :=
              TIconInfo.Create(AIconFileName, AOverlayIconIndex);
        end;
        Inc(AIndex);
      end;
    finally
      RegCloseKey(AKey);
    end;
  CheckReservedOverlayIconInfo;
end;

{ TdxShellTreeViewItemProducer }

constructor TdxShellTreeViewItemProducer.Create(AOwner: TWinControl);
begin
  inherited Create(AOwner);
  TreeView.AddItemProducer(Self);
end;

destructor TdxShellTreeViewItemProducer.Destroy;
begin
  TreeView.RemoveItemProducer(Self);
  inherited Destroy;
end;

procedure TdxShellTreeViewItemProducer.NotifyAddItem(Index: Integer);
begin
  if Node <> nil then
    Owner.Perform(DSM_NOTIFYADDITEM, Index, LPARAM(Node));
end;

procedure TdxShellTreeViewItemProducer.NotifyRemoveItem(Index: Integer);
begin
  if Node <> nil then
    Owner.Perform(DSM_NOTIFYREMOVEITEM, Index, LPARAM(Node));
end;

procedure TdxShellTreeViewItemProducer.ProcessItems(AIFolder: IShellFolder;
  APIDL: PItemIDList; ANode: TdxTreeViewNode; APreloadItemCount: Integer);

  function SetNodeText: Boolean;
  var
    ATempPIDL: PItemIDList;
  begin
    Result := not TreeView.IsShellRootNode(Node);
    if not Result then
      Exit;
    ATempPIDL := GetLastPidlItem(APIDL);
    Node.Caption := GetShellItemDisplayName
      (TdxShellTreeViewItemProducer(ShellItemInfo.ItemProducer).ShellFolder,
      ATempPIDL, True);
  end;

var
  AFileInfo: TShFileInfo;
begin
  Node := ANode;
  cxShellGetThreadSafeFileInfo(PChar(APIDL), 0, AFileInfo, SizeOf(AFileInfo),
    SHGFI_PIDL or SHGFI_DISPLAYNAME or SHGFI_SYSICONINDEX);
  if not SetNodeText then
    Node.Caption := StrPas(AFileInfo.szDisplayName);
  Node.ImageIndex := AFileInfo.iIcon;
  cxShellGetThreadSafeFileInfo(PChar(APIDL), 0, AFileInfo, SizeOf(AFileInfo),
    SHGFI_PIDL or SHGFI_SYSICONINDEX or SHGFI_OPENICON);
  ProcessItems(AIFolder, APIDL, APreloadItemCount);
end;

procedure TdxShellTreeViewItemProducer.SetItemsCount(Count: Integer);
begin
  if Node <> nil then
    Owner.Perform(DSM_SETCOUNT, Count, LPARAM(Node))
end;

function TdxShellTreeViewItemProducer.CanAddFolder
  (AFolder: TcxShellFolder): Boolean;
begin
  Result := TreeView.DoAddFolder(AFolder);
end;

function TdxShellTreeViewItemProducer.CreateFakeProducer
  : TdxCustomShellTreeItemProducer;
begin
  Result := TdxShellTreeViewItemProducer.Create(TreeView);
end;

function TdxShellTreeViewItemProducer.GetEnumFlags: Cardinal;
var
  APIDL: PItemIDList;
begin
  Result := TreeView.ShellOptions.GetEnumFlags;
  if TreeView.ShellOptions.TrackShellChanges and (ShellItemInfo <> nil) and
    Succeeded(cxGetFolderLocation(0, CSIDL_NETWORK, 0, 0, APIDL)) then
  begin
    if IsSubPath(ShellItemInfo.pidl, APIDL) then
      Result := Result or SHCONTF_ENABLE_ASYNC;
    DisposePidl(APIDL);
  end;
end;

function TdxShellTreeViewItemProducer.GetItemsInfoGatherer
  : TcxShellItemsInfoGatherer;
begin
  Result := TreeView.ItemsInfoGatherer;
end;

function TdxShellTreeViewItemProducer.GetShowToolTip: Boolean;
begin
  Result := TreeView.ShellOptions.ShowToolTip;
end;

function TdxShellTreeViewItemProducer.GetTreeView: TdxShellTreeView;
begin
  Result := TdxShellTreeView(Owner);
end;

{ TdxShellTreeViewOptions }

procedure TdxShellTreeViewOptions.DoNotifyUpdateContents;
begin
  (Owner as TdxShellTreeView).UpdateContent;
end;

{ TdxShellTreeViewNodeViewInfo }

procedure TdxShellTreeViewNodeViewInfo.Draw(ACanvas: TcxCanvas);
begin
  FDrawState := GetState;
  inherited Draw(ACanvas);
end;

procedure TdxShellTreeViewNodeViewInfo.DrawSelection(ACanvas: TcxCanvas);
var
  R: TRect;
begin
  R := SelectionRect;
  if not TreeView.OptionsView.RowSelect then
    if TreeView.UseRightToLeftAlignment then
      Dec(R.Left, LevelOffset)
    else
      Dec(R.Right, LevelOffset - TreeView.ViewInfo.ViewPort.X);
  Painter.DrawListViewItemBackground(ACanvas, R, DrawState, True);
end;

function TdxShellTreeViewNodeViewInfo.GetState: TdxListViewItemStates;
begin
  Result := [];
  if HasHottrack then
    Include(Result, dxlisHot);
  if HasSelection then
    Include(Result, dxlisSelected);
  if Node = TreeView.FocusedNode then
    Include(Result, dxlisFocused);
  if not TreeView.Focused then
    Include(Result, dxlisInactive);
end;

function TdxShellTreeViewNodeViewInfo.GetTextColor(ASelected: Boolean): TColor;
begin
  Result := Painter.GetListViewItemTextColor(DrawState, True);
end;

function TdxShellTreeViewNodeViewInfo.GetTreeView: TdxShellTreeView;
begin
  Result := TdxShellTreeView(inherited TreeView);
end;

{ TdxShellTreeViewViewInfo }

function TdxShellTreeViewViewInfo.CreateNodeViewInfo: TdxTreeViewNodeViewInfo;
begin
  Result := TdxShellTreeViewNodeViewInfo.Create(TreeView);
end;

function TdxShellTreeViewViewInfo.GetBackgroundColor: TColor;
begin
  if Painter.LookAndFeelStyle = lfsNative then
    Result := clWhite
  else
    Result := Painter.GridLikeControlContentColor;
end;

{ TdxShellTreeView }

constructor TdxShellTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDependedControls := TcxShellDependedControls.Create;
  FFirstLevelNodesVisible := True;
  FItemsInfoGatherer := TcxShellItemsInfoGatherer.Create(Self);
  FShellRoot := TcxShellTreeRoot.Create(Self, 0);
  FShellRoot.OnFolderChanged := RootFolderChanged;
  FShellRoot.OnSettingsChanged := RootSettingsChanged;
  FShellOptions := TdxShellTreeViewOptions.Create(Self);
  FItemProducersList := TThreadList.Create;
  FInternalSmallImages := TImageList.Create(Self);
  FInternalSmallImages.ShareImages := True;
  FInternalSmallImages.Handle := cxShellGetImageList(SHGFI_SMALLICON);
  ImageList_SetBkColor(FInternalSmallImages.Handle, CLR_NONE);
  OptionsView.Images := FInternalSmallImages;
  DoubleBuffered := True;
  FAppEvents := TApplicationEvents.Create(nil);
  FAppEvents.OnIdle := DoOnIdle;
  ScrollBars := ssBoth;
end;

destructor TdxShellTreeView.Destroy;
var
  AList: TList;
  I: Integer;
begin
  FreeAndNil(FAppEvents);
  RemoveChangeNotification;

  AList := FItemProducersList.LockList;
  try
    for I := 0 to AList.Count - 1 do
      TdxShellTreeViewItemProducer(AList[I]).ClearFetchQueue;
  finally
    FItemProducersList.UnlockList;
  end;

  Root.Clear;
  FreeAndNil(FInternalSmallImages);
  FreeAndNil(FItemProducersList);
  FreeAndNil(FShellOptions);
  FreeAndNil(FShellRoot);
  FreeAndNil(FItemsInfoGatherer);
  FreeAndNil(FDependedControls);
  inherited Destroy;
end;

function TdxShellTreeView.CanCollapse(Sender: TdxTreeCustomNode): Boolean;
begin
  Result := inherited CanCollapse(Sender) and
    (FFirstLevelNodesVisible or not IsShellRootNode(TdxTreeViewNode(Sender)));
end;

function TdxShellTreeView.CanExpand(Sender: TdxTreeCustomNode): Boolean;

  function GetDataForProcessing(out AProcessingFolder: IShellFolder;
    out AProcessingPidl: PItemIDList): Boolean;
  var
    AItemProducer: TdxShellTreeViewItemProducer;
    AItemInfo: TcxShellItemInfo;
    ANode: TdxTreeViewNode;
  begin
    ANode := Sender as TdxTreeViewNode;
    if not IsShellRootNode(ANode) then
    begin
      AItemProducer := GetItemProducer(ANode.Parent);
      AItemInfo := GetShellItemInfo(ANode);
      ANode.HasChildren := AItemInfo.IsFolder;
      Result := AItemInfo.IsFolder and
        Succeeded(AItemProducer.ShellFolder.BindToObject(AItemInfo.pidl, nil,
        IID_IShellFolder, AProcessingFolder));
      if Result then
        AProcessingPidl := ConcatenatePidls(AItemProducer.FolderPidl,
          AItemInfo.pidl);
    end
    else
    begin
      Result := True;
      AProcessingFolder := ShellRoot.ShellFolder;
      AProcessingPidl := GetPidlCopy(ShellRoot.pidl);
    end;
  end;

  function InternalCanExpand: Boolean;
  var
    ANode: TdxTreeViewNode;
    AProcessingPidl: PItemIDList;
    AProcessingFolder: IShellFolder;
    APreloadItemCount: Integer;
  begin
    Inc(FLockChange);
    try
      Result := GetDataForProcessing(AProcessingFolder, AProcessingPidl);
      if Result then
        try
          ANode := Sender as TdxTreeViewNode;
          APreloadItemCount := ViewInfo.NumberOfNodesInContentRect;
          GetItemProducer(ANode).ProcessItems(AProcessingFolder,
            AProcessingPidl, ANode, APreloadItemCount);
        finally
          DisposePidl(AProcessingPidl);
        end;
    finally
      Dec(FLockChange);
    end;
  end;

begin
  Result := ((Sender.First <> nil) or InternalCanExpand) and
    inherited CanExpand(Sender);
end;

function TdxShellTreeView.CreateViewInfo: TdxTreeViewViewInfo;
begin
  Result := TdxShellTreeViewViewInfo.Create(Self);
end;

procedure TdxShellTreeView.DeleteNode(Sender: TdxTreeCustomNode);
var
  AItemProducer: TdxShellTreeViewItemProducer;
  ANode: TdxTreeViewNode;
begin
  inherited DeleteNode(Sender);
  ANode := Sender as TdxTreeViewNode;
  AItemProducer := GetItemProducer(ANode);
  if AItemProducer <> nil then
  begin
    AItemProducer.Free;
    ANode.Data := nil;
  end;
end;

procedure TdxShellTreeView.DestroyWnd;
var
  ATempPIDL: PItemIDList;
begin
  ATempPIDL := AbsolutePIDL;
  try
    SaveAbsolutePIDL(ATempPIDL);
  finally
    DisposePidl(ATempPIDL);
  end;
  RemoveChangeNotification;
  inherited DestroyWnd;
end;

procedure TdxShellTreeView.DoContextPopup(MousePos: TPoint;
  var Handled: Boolean);
var
  AItem: TcxShellItemInfo;
  AItemPIDLList: TList;
  ANode: TdxTreeViewNode;
  AHitAtNode: Boolean;
begin
  try
    AHitAtNode := GetNodeAtPos(MousePos, ANode);
    if not ShellOptions.ContextMenus or not AHitAtNode then
    begin
      inherited DoContextPopup(MousePos, Handled);
      Exit;
    end;
    FIsUpdating := True;
    ANode.Selected := True;
    Handled := True;
    if IsShellRootNode(ANode) then
      Exit;

    FContextPopupItemProducer := GetItemProducer(ANode.Parent);
    FContextPopupItemProducer.OnDestroy :=
      ContextPopupItemProducerDestroyHandler;
    FContextPopupItemProducer.LockRead;
    try
      AItem := FContextPopupItemProducer.ItemInfo[ANode.Index];
      if AItem.pidl <> nil then
      begin
        AItemPIDLList := TList.Create;
        try
          AItemPIDLList.Add(GetPidlCopy(AItem.pidl));
          cxShellCommon.DisplayContextMenu(Handle,
            FContextPopupItemProducer.ShellFolder, AItemPIDLList,
            ClientToScreen(MousePos));
        finally
          DisposePidl(AItemPIDLList[0]);
          AItemPIDLList.Free;
        end;
      end;
    finally
      if FContextPopupItemProducer <> nil then
        FContextPopupItemProducer.UnlockRead;
    end;
  finally
    if ANode <> nil then
      ANode.Selected := ANode = FocusedNode;
    if FContextPopupItemProducer <> nil then
    begin
      FContextPopupItemProducer.OnDestroy := nil;
      FContextPopupItemProducer := nil;
    end;
    FIsUpdating := False;
  end;
end;

procedure TdxShellTreeView.InitControl;
begin
  if not IsLoading then
  begin
    if ShellRoot.pidl = nil then
      TcxShellTreeRootAccess(ShellRoot).CheckRoot;
    if ShellRootNode <> nil then
      ShellRootNode.Expanded := True;
    CreateChangeNotification;
  end;
end;

procedure TdxShellTreeView.KeyDown(var Key: Word; Shift: TShiftState);

  procedure InvokeContextMenuCommand(const ACommandStr: AnsiString);
  var
    AShellFolder: IShellFolder;
    AItemPIDLList: TList;
    AItem: TcxShellItemInfo;
    AParentItemProducer: TdxShellTreeViewItemProducer;
  begin
    if (FocusedNode <> nil) and not IsShellRootNode(FocusedNode) then
    begin
      AItemPIDLList := TList.Create;
      try
        AParentItemProducer := GetItemProducer(FocusedNode.Parent);
        AShellFolder := AParentItemProducer.ShellFolder;
        AItem := AParentItemProducer.ItemInfo[FocusedNode.Index];
        AItemPIDLList.Add(GetPidlCopy(AItem.pidl));
        cxShellInvokeContextMenuCommand(AShellFolder, AItemPIDLList,
          ACommandStr);
      finally
        if AItemPIDLList.Count > 0 then
          DisposePidl(AItemPIDLList[0]);
        AItemPIDLList.Free;
      end;
    end;
  end;

begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_F5:
      UpdateContent
  else
    if ShellOptions.ContextMenus and
      (cxShellIsClipboardCommandContextMenuShortCut(Key, Shift) or
      (Key = VK_DELETE) and (FocusedNode <> nil)) then
      InvokeContextMenuCommand(cxShellGetContextMenuCommandStrByShortCut
        (Key, Shift));
  end;
end;

procedure TdxShellTreeView.Loaded;
begin
  if ShellRoot.pidl = nil then
    TcxShellTreeRootAccess(ShellRoot).CheckRoot;
  if ShellRootNode <> nil then
    ShellRootNode.Expanded := True;
  if HandleAllocated then
    CreateChangeNotification;
end;

function TdxShellTreeView.GetDependedControls: TcxShellDependedControls;
begin
  Result := FDependedControls;
end;

function TdxShellTreeView.GetRoot: TcxCustomShellRoot;
begin
  Result := FShellRoot;
end;

procedure TdxShellTreeView.AddItemProducer
  (AProducer: TdxShellTreeViewItemProducer);
var
  ATempList: TList;
begin
  ATempList := FItemProducersList.LockList;
  try
    ATempList.Add(AProducer);
  finally
    FItemProducersList.UnlockList;
  end;
end;

procedure TdxShellTreeView.ChangeTreeContent(AChangeProc: TProc);
var
  APrevSelectedPidl, ASelectedPidl: PItemIDList;
begin
  BeginUpdate;
  try
    if HandleAllocated then
      SendMessage(Handle, WM_SETREDRAW, 0, 0);
    LockUpdateVisibleInfo;
    APrevSelectedPidl := AbsolutePIDL;
    FIsUpdating := True;
    try
      SaveTreeState;
      try
        AChangeProc;
      finally
        RestoreTreeState;
      end;
    finally
      FIsUpdating := False;
      ASelectedPidl := AbsolutePIDL;
      try
        if not EqualPIDLs(APrevSelectedPidl, ASelectedPidl) then
          DoSelectionChanged;
      finally
        DisposePidl(ASelectedPidl);
      end;
      DisposePidl(APrevSelectedPidl);
      UnlockUpdateVisibleInfo;
      if HandleAllocated then
        SendMessage(Handle, WM_SETREDRAW, 1, 0);
      Update;
    end;
  finally
    EndUpdate;
  end;
end;

function TdxShellTreeView.CheckFileMask(AFolder: TcxShellFolder): Boolean;
begin
  Result := ShellOptions.IsFileNameValid(ExtractFileName(AFolder.PathName));
end;

procedure TdxShellTreeView.CreateChangeNotification
  (ANode: TdxTreeViewNode = nil);

  function GetShellChangeNotifierPIDL: PItemIDList;
  begin
    CheckShellRoot(ShellRoot);
    if ANode = nil then
      ANode := ShellRootNode;
    if IsShellRootNode(ANode) then
      Result := ShellRoot.pidl
    else
      Result := GetShellItemInfo(ANode).FullPIDL;
  end;

begin
  if FIsChangeNotificationCreationLocked then
    Exit;
  FShellChangeNotificationCreation := True;
  try
    if not ShellOptions.TrackShellChanges or (Root.Count = 0) then
      RemoveChangeNotification
    else
      cxShellRegisterChangeNotifier(GetShellChangeNotifierPIDL, Handle,
        DSM_SYSTEMSHELLCHANGENOTIFY, True, FShellChangeNotifierData);
  finally
    FShellChangeNotificationCreation := False;
  end;
end;

function TdxShellTreeView.DoAddFolder(AFolder: TcxShellFolder): Boolean;
begin
  Result := AFolder.IsFolder and (ShellOptions.ShowNonFolders or
    ShellOptions.ShowZipFilesWithFolders or not cxShellIsZipFile(AFolder)) or
    not AFolder.IsFolder and CheckFileMask(AFolder);
  if Assigned(FOnAddFolder) then
    FOnAddFolder(Self, AFolder, Result);
end;

procedure TdxShellTreeView.DoOnIdle(Sender: TObject; var Done: Boolean);
var
  AList: TList;
  AItem: TcxShellItemInfo;
begin
  AList := ItemsInfoGatherer.ProcessedItems.LockList;
  try
    if AList.Count > 0 then
      AItem := TcxShellItemInfo(AList.Extract(AList.First))
    else
      AItem := nil;
  finally
    ItemsInfoGatherer.ProcessedItems.UnlockList;
  end;
  if AItem <> nil then
    UpdateItem(AItem);
end;

function TdxShellTreeView.GetNodeByPIDL(APIDL: PItemIDList;
  ANearestInChain: Boolean = False; ACheckExpanded: Boolean = False;
  AStartParentNode: TdxTreeViewNode = nil): TdxTreeViewNode;
var
  AItemIndex, I: Integer;
  APID, AStartPidl: PItemIDList;
  AItemProducer: TdxShellTreeViewItemProducer;
begin
  Result := nil;
  if APIDL = nil then
    Exit;

  CheckShellRoot(ShellRoot);
  if EqualPIDLs(ShellRoot.pidl, APIDL) then
  begin
    Result := ShellRootNode;
    Exit;
  end;

  if AStartParentNode = nil then
  begin
    AStartParentNode := ShellRootNode;
    AStartPidl := ShellRoot.pidl;
  end
  else
  begin
    AItemProducer := GetItemProducer(AStartParentNode);
    AStartPidl := AItemProducer.FolderPidl;
  end;

  if not IsSubPath(AStartPidl, APIDL) then
    Exit;

  for I := 0 to GetPidlItemsCount(AStartPidl) - 1 do
    APIDL := GetNextItemID(APIDL);
  Result := AStartParentNode;
  for I := 0 to GetPidlItemsCount(APIDL) - 1 do
  begin
    APID := ExtractParticularPidl(APIDL);
    if APID = nil then
      Break;
    try
      AItemProducer := GetItemProducer(Result);
      AItemIndex := AItemProducer.GetItemIndexByPidl(APID);
      if not InRange(AItemIndex, 0, Result.Count - 1) then
      begin
        if not ANearestInChain then
          Result := nil;
        Break;
      end;
      Result := TdxTreeViewNode(AItemProducer.ItemInfo[AItemIndex].Data);
      if ACheckExpanded and not Result.Expanded then
        Break;
      APIDL := GetNextItemID(APIDL);
    finally
      DisposePidl(APID);
    end;
  end;
end;

function TdxShellTreeView.HasHottrack: Boolean;
begin
  Result := True;
end;

procedure TdxShellTreeView.RemoveChangeNotification;
begin
  cxShellUnregisterChangeNotifier(FShellChangeNotifierData);
end;

procedure TdxShellTreeView.RemoveItemProducer
  (AProducer: TdxShellTreeViewItemProducer);
var
  ATempList: TList;
begin
  ATempList := FItemProducersList.LockList;
  try
    ATempList.Remove(AProducer);
  finally
    FItemProducersList.UnlockList;
  end;
end;

procedure TdxShellTreeView.UpdateContent;
begin
  if ShellRoot.ShellFolder = nil then
    CheckShellRoot(ShellRoot)
  else
    ShellChangeNotify(0, ShellRoot.pidl, nil);
end;

procedure TdxShellTreeView.UpdateItem(AItem: TcxShellItemInfo);
var
  AItemProducer: TdxShellTreeViewItemProducer;
  AUpdatingNode: TdxTreeViewNode;
begin
  AUpdatingNode := TdxTreeViewNode(AItem.Data);
  AItemProducer := TdxShellTreeViewItemProducer(AItem.ItemProducer);
  AItemProducer.LockRead;
  try
    AUpdatingNode.ImageIndex := AItem.IconIndex;
    AUpdatingNode.Caption := AItem.Name;
    AUpdatingNode.HasChildren := AItem.HasSubfolder;
    AItem.Processed := True;
  finally
    AItemProducer.UnlockRead;
  end;
end;

procedure TdxShellTreeView.UpdateNode(ANode: TdxTreeViewNode; AFast: Boolean);

  procedure InternalUpdateNode(AUpdatedNode: TdxTreeViewNode);
  var
    AFullPIDL: PItemIDList;
    AParentItemProducer: TdxShellTreeViewItemProducer;
    AShellItemInfo: TcxShellItemInfo;
  begin
    AShellItemInfo := GetShellItemInfo(AUpdatedNode);
    AParentItemProducer := GetItemProducer(AUpdatedNode.Parent);
    if AParentItemProducer = nil then
      Exit;
    AFullPIDL := ConcatenatePidls(AParentItemProducer.FolderPidl,
      AShellItemInfo.pidl);
    try
      GetItemProducer(AUpdatedNode).FolderPidl := AFullPIDL;
      AUpdatedNode.HasChildren := HasSubItems(AParentItemProducer.ShellFolder,
        AFullPIDL, AParentItemProducer.GetEnumFlags);
    finally
      DisposePidl(AFullPIDL);
    end;
  end;

var
  ATempNode: TdxTreeViewNode;
begin
  if not IsLoading and ShellRoot.IsValid then
  begin
    if ANode = nil then
    begin
      if (Root.Count > 0) and (ShellRootNode.Data <> nil) then
        Root.Clear;
      if Root.Count = 0 then
        ATempNode := Root.AddChildFirst
      else
        ATempNode := ShellRootNode;
      ATempNode.Data := TdxShellTreeViewItemProducer.Create(Self);
    end
    else
      ATempNode := ANode;
    if not AFast or IsShellRootNode(ATempNode) then
    begin
      ATempNode.HasChildren := True;
      if not CanExpand(ATempNode) then
        InternalUpdateNode(ATempNode);
    end
    else
      InternalUpdateNode(ATempNode);
    if (ANode = nil) and HandleAllocated then
      CreateChangeNotification;
  end;
end;

procedure TdxShellTreeView.UpdateRootNodes;
begin
  UpdateNode(nil, False);
end;

procedure TdxShellTreeView.UpdateVisibleItems;
var
  I, AFirstVisibleIndex, ALastVisibleIndex: Integer;
  ANode: TdxTreeViewNode;
  AItems: TList;
  AVisibleCount: Integer;
  AItemInfo: TcxShellItemInfo;
begin
  AVisibleCount := Min(ViewInfo.NumberOfNodesInContentRect + 1,
    AbsoluteVisibleNodes.Count);
  if (FLockVisibleUpdate > 0) or (AVisibleCount = 0) or
    not GetNodeAtPos(ViewInfo.ContentRect.TopLeft, ANode) then
    Exit;
  AItems := TList.Create;
  try
    AFirstVisibleIndex := AbsoluteVisibleNodes.IndexOf(ANode);
    ALastVisibleIndex := Min(AFirstVisibleIndex + AVisibleCount - 1,
      AbsoluteVisibleNodes.Count - 1);
    for I := AFirstVisibleIndex to ALastVisibleIndex do
    begin
      ANode := AbsoluteVisibleNodes[I];
      if not IsShellRootNode(ANode) then
      begin
        AItemInfo := GetShellItemInfo(ANode);
        if not AItemInfo.Updated or not AItemInfo.Processed then
          AItems.Add(AItemInfo);
      end;
    end;
    ItemsInfoGatherer.ClearVisibleItems;
    ItemsInfoGatherer.RequestItems(AItems);
  finally
    AItems.Free;
  end;
end;

procedure TdxShellTreeView.UpdateViewPort(const APoint: TPoint);
begin
  inherited UpdateViewPort(APoint);
  UpdateVisibleItems;
end;

function TdxShellTreeView.IsShellRootNode(ANode: TdxTreeViewNode): Boolean;
begin
  Result := ANode = ShellRootNode;
end;

procedure TdxShellTreeView.ContextPopupItemProducerDestroyHandler
  (Sender: TObject);
begin
  FContextPopupItemProducer.UnlockRead;
  FContextPopupItemProducer.OnDestroy := nil;
  FContextPopupItemProducer := nil;
end;

function TdxShellTreeView.CreateShellNode(AParentNode: TdxTreeViewNode;
  AShellItem: TcxShellItemInfo): TdxTreeViewNode;
var
  AItemProducer: TdxShellTreeViewItemProducer;
begin
  AItemProducer := TdxShellTreeViewItemProducer.Create(Self);
  AItemProducer.ShellItemInfo := AShellItem;
  Result := AParentNode.AddChild(AShellItem.Name, AItemProducer);
  AShellItem.Data := Result;
  Result.ImageIndex := AShellItem.IconIndex;
  Result.HasChildren := AShellItem.HasSubfolder;
end;

procedure TdxShellTreeView.DoSelectionChanged;
begin
  if not FIsUpdating then
  begin
    inherited DoSelectionChanged;
    UpdateNode(FocusedNode, not FNavigation);
  end;
end;

function TdxShellTreeView.GetAbsolutePIDL: PItemIDList;
begin
  Result := nil;
  if FocusedNode <> nil then
    Result := GetNodeAbsolutePIDL(FocusedNode);
end;

function TdxShellTreeView.GetItemProducer(ANode: TdxTreeViewNode)
  : TdxShellTreeViewItemProducer;
begin
  Result := TdxShellTreeViewItemProducer(ANode.Data);
end;

function TdxShellTreeView.GetNodeAbsolutePIDL(ANode: TdxTreeViewNode)
  : PItemIDList;
var
  AItemProducer: TdxShellTreeViewItemProducer;
  AItem: TcxShellItemInfo;
begin
  if ANode.IsRoot then
  begin
    CheckShellRoot(ShellRoot);
    Result := GetPidlCopy(ShellRoot.pidl);
  end
  else
  begin
    AItemProducer := TdxShellTreeViewItemProducer(ANode.Data);
    if (AItemProducer <> nil) and (AItemProducer.FolderPidl <> nil) then
      Result := GetPidlCopy(AItemProducer.FolderPidl)
    else
    begin
      AItemProducer := TdxShellTreeViewItemProducer(ANode.Parent.Data);
      if AItemProducer <> nil then
      begin
        AItemProducer.LockRead;
        try
          AItem := AItemProducer.ItemInfo[ANode.Index];
          Result := GetPidlCopy(AItem.FullPIDL);
        finally
          AItemProducer.UnlockRead;
        end;
      end
      else
        Result := nil;
    end;
  end;
end;

function TdxShellTreeView.GetPath: string;
var
  ATempPIDL: PItemIDList;
begin
  ATempPIDL := AbsolutePIDL;
  try
    Result := GetPidlName(ATempPIDL);
  finally
    DisposePidl(ATempPIDL);
  end;
end;

function TdxShellTreeView.GetShellRootNode: TdxTreeViewNode;
begin
  Result := Root.First;
end;

function TdxShellTreeView.GetShellItemInfo(ANode: TdxTreeViewNode)
  : TcxShellItemInfo;
begin
  Result := GetItemProducer(ANode).ShellItemInfo;
end;

procedure TdxShellTreeView.LockUpdateVisibleInfo;
begin
  Inc(FLockVisibleUpdate);
end;

procedure TdxShellTreeView.RestoreTreeState;

  procedure RestoreExpandedNodes;

    procedure ExpandNode(APIDL: PItemIDList);
    var
      ANode: TdxTreeViewNode;
    begin
      CheckShellRoot(ShellRoot);
      if APIDL = nil then
        APIDL := ShellRoot.pidl;
      ANode := GetNodeByPIDL(APIDL);
      if ANode <> nil then
        ANode.Expanded := True;
    end;

    procedure DestroyExpandedNodeList;
    var
      I: Integer;
    begin
      if FStateData.ExpandedNodeList = nil then
        Exit;
      for I := 0 to FStateData.ExpandedNodeList.Count - 1 do
        DisposePidl(PItemIDList(FStateData.ExpandedNodeList[I]));
      FreeAndNil(FStateData.ExpandedNodeList);
    end;

  var
    I: Integer;
  begin
    try
      for I := 0 to FStateData.ExpandedNodeList.Count - 1 do
        ExpandNode(PItemIDList(FStateData.ExpandedNodeList[I]));
    finally
      DestroyExpandedNodeList;
    end;
  end;

  procedure RestoreCurrentPath;
  var
    ACurrentPath, ATempPIDL: PItemIDList;
  begin
    if FStateData.CurrentPath = nil then
      Exit;
    ACurrentPath := GetPidlCopy(FStateData.CurrentPath);
    try
      repeat
        if GetNodeByPIDL(ACurrentPath) <> nil then
        begin
          FNavigation := True;
          try
            DoNavigate(ACurrentPath);
          finally
            FNavigation := False;
          end;
          Break;
        end;
        ATempPIDL := ACurrentPath;
        ACurrentPath := GetPidlParent(ACurrentPath);
        DisposePidl(ATempPIDL);
      until False;
    finally
      DisposePidl(ACurrentPath);
    end;
  end;

begin
  try
    RestoreExpandedNodes;
    RestoreCurrentPath;
    ViewInfo.ViewPort := FStateData.ViewPort;
  finally
    DisposePidl(FStateData.CurrentPath);
    FStateData.CurrentPath := nil;
  end;
end;

procedure TdxShellTreeView.RootFolderChanged(Sender: TObject;
  Root: TcxCustomShellRoot);
begin
  Self.Root.Clear;
  UpdateRootNodes;
  if Assigned(FOnRootChanged) then
    FOnRootChanged(Self, FShellRoot);
end;

procedure TdxShellTreeView.RootSettingsChanged(Sender: TObject);
begin
  if not IsLoading then
    FDependedControls.SynchronizeRoot(ShellRoot);
end;

procedure TdxShellTreeView.SaveAbsolutePIDL(AValue: PItemIDList);
var
  ATempPIDL: PItemIDList;
begin
  ATempPIDL := GetPidlCopy(AValue);
  DisposePidl(FAbsolutePIDL);
  FAbsolutePIDL := ATempPIDL;
end;

procedure TdxShellTreeView.SaveTreeState;

  procedure DoSaveExpandedNode(ANode: TdxTreeViewNode);
  begin
    if IsShellRootNode(ANode) then
      FStateData.ExpandedNodeList.Add(nil)
    else
      FStateData.ExpandedNodeList.Add
        (GetPidlCopy(GetShellItemInfo(ANode).FullPIDL));
  end;

  procedure SaveExpandedNodes(AParentNode: TdxTreeViewNode);
  var
    ANode: TdxTreeViewNode;
  begin
    if FStateData.ExpandedNodeList = nil then
      FStateData.ExpandedNodeList := TList.Create;
    ANode := AParentNode.First;
    while ANode <> nil do
    begin
      if ANode.Expanded then
        DoSaveExpandedNode(ANode);
      if ANode.HasChildren then
        SaveExpandedNodes(ANode);
      ANode := ANode.Next;
    end;
  end;

  procedure SaveCurrentPath;
  begin
    if (FocusedNode <> nil) and not FocusedNode.IsRoot then
    begin
      if IsShellRootNode(FocusedNode) then
        FStateData.CurrentPath := ShellRoot.pidl
      else
        FStateData.CurrentPath := GetShellItemInfo(FocusedNode).FullPIDL;
      FStateData.CurrentPath := GetPidlCopy(FStateData.CurrentPath);
    end
    else
      FStateData.CurrentPath := nil;
  end;

begin
  FStateData.ViewPort := ViewInfo.ViewPort;
  SaveExpandedNodes(Root);
  SaveCurrentPath;
end;

procedure TdxShellTreeView.SetAbsolutePIDL(AValue: PItemIDList);
begin
  if CheckAbsolutePIDL(AValue, ShellRoot, True) then
  begin
    FNavigation := True;
    try
      DoNavigate(AValue);
    finally
      FNavigation := False;
    end;
  end;
end;

procedure TdxShellTreeView.SetFirstLevelNodesVisible(const Value: Boolean);
begin
  if FFirstLevelNodesVisible <> Value then
  begin
    FFirstLevelNodesVisible := Value;
    Changed([tvcStructure]);
  end;
end;

procedure TdxShellTreeView.SetPath(AValue: string);

  function GetViewOptions: TcxShellViewOptions;
  begin
    Result := [svoShowHidden];
    if ShellOptions.ShowNonFolders then
      Include(Result, svoShowFiles);
    if ShellOptions.ShowFolders then
      Include(Result, svoShowFolders);
  end;

var
  APIDL: PItemIDList;
begin
  APIDL := PathToAbsolutePIDL(AValue, ShellRoot, GetViewOptions);
  if APIDL <> nil then
    try
      AbsolutePIDL := APIDL;
    finally
      DisposePidl(APIDL);
    end;
end;

procedure TdxShellTreeView.SetRootField(const AValue: TcxShellTreeRoot);
begin
  FShellRoot.Assign(AValue);
end;

procedure TdxShellTreeView.ShellChangeNotify(AEventID: Longint;
  APidl1, APidl2: PItemIDList);

  function NeedProcessMessage(AEventID: Longint;
    APidl1, APidl2: PItemIDList): Boolean;
  var
    AParentPidl: PItemIDList;
  begin
    Result := GetNodeByPIDL(APidl1) <> nil;
    if not Result then
    begin
      AParentPidl := GetPidlParent(APidl1);
      try
        Result := GetNodeByPIDL(AParentPidl) <> nil;
      finally
        DisposePidl(AParentPidl);
      end;
    end;
  end;

begin
  if not FShellChangeNotificationCreation and not FIsUpdating and
    (FLockChange = 0) and NeedProcessMessage(AEventID, APidl1, APidl2) then
    try
      ChangeTreeContent(procedure
        begin
          Root.Clear;
          UpdateRootNodes;
        end);
    finally
      if Assigned(FOnShellChange) then
        FOnShellChange(Self, AEventID, APidl1, APidl2);
    end;
end;

procedure TdxShellTreeView.UnlockUpdateVisibleInfo;
begin
  if FLockVisibleUpdate > 0 then
    Dec(FLockVisibleUpdate);
end;

procedure TdxShellTreeView.DoNavigate(APIDL: PItemIDList);
var
  ASourcePidl: PItemIDList;
  AShellFolder: IShellFolder;
  APartDstPidl: PItemIDList;
  I: Integer;
  ATempProducer: TdxShellTreeViewItemProducer;
  ATempIndex: Integer;
begin
  if (ShellRootNode = nil) or Failed(SHGetDesktopFolder(AShellFolder)) then
    Exit;
  BeginUpdate;
  try
    ASourcePidl := ShellRoot.pidl;
    if GetPidlItemsCount(ASourcePidl) > GetPidlItemsCount(APIDL) then
    begin
      ShellRoot.pidl := APIDL;
      FocusedNode := ShellRootNode;
      Exit;
    end;
    for I := 0 to GetPidlItemsCount(ASourcePidl) - 1 do
      APIDL := GetNextItemID(APIDL);
    FocusedNode := ShellRootNode;
    for I := 0 to GetPidlItemsCount(APIDL) - 1 do
    begin
      ATempProducer := FocusedNode.Data;
      APartDstPidl := ExtractParticularPidl(APIDL);
      APIDL := GetNextItemID(APIDL);
      if APartDstPidl = nil then
        Break;
      try
        ATempIndex := ATempProducer.GetItemIndexByPidl(APartDstPidl);
        if ATempIndex = -1 then
          Break;
        FocusedNode := FocusedNode.Items[ATempIndex];
      finally
        DisposePidl(APartDstPidl);
      end;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxShellTreeView.DSMNotifyAddItem(var Message: TMessage);
var
  AParentProducer: TdxShellTreeViewItemProducer;
  ANode: TdxTreeViewNode;
begin
  ANode := TdxTreeViewNode(Message.LPARAM);
  AParentProducer := GetItemProducer(ANode);
  if AParentProducer = nil then
    Exit;
  AParentProducer.LockRead;
  try
    CreateShellNode(ANode, AParentProducer.Items[Message.WParam]);
  finally
    AParentProducer.UnlockRead;
  end;
end;

procedure TdxShellTreeView.DSMNotifyRemoveItem(var Message: TMessage);
var
  ANode: TdxTreeViewNode;
begin
  ANode := TdxTreeViewNode(Message.LPARAM);
  if Message.WParam < WParam(ANode.Count) then
    ANode.Items[Message.WParam].Free;
end;

procedure TdxShellTreeView.DSMSetCount(var Message: TMessage);
var
  ANode: TdxTreeViewNode;
  AParentProducer: TdxShellTreeViewItemProducer;
  I: Integer;
  AWasExpanded: Boolean;
begin
  ANode := TdxTreeViewNode(Message.LPARAM);
  if Message.WParam = 0 then
  begin
    ANode.Clear;
    ANode.HasChildren := False;
    Exit;
  end;
  AParentProducer := GetItemProducer(ANode);
  AParentProducer.LockRead;
  try
    BeginUpdate;
    try
      AWasExpanded := ANode.Expanded;
      for I := 0 to AParentProducer.Items.Count - 1 do
        CreateShellNode(ANode, AParentProducer.Items[I]);
      ANode.Expanded := AWasExpanded;
    finally
      EndUpdate;
    end;
    if ANode.First = nil then
      ANode.HasChildren := False;
  finally
    AParentProducer.UnlockRead;
  end;
end;

procedure TdxShellTreeView.DSMSynchronizeRoot(var Message: TMessage);
begin
  if not IsLoading then
    ShellRoot.Update(TcxCustomShellRoot(Message.WParam));
end;

procedure TdxShellTreeView.DSMSystemShellChangeNotify(var Message: TMessage);
var
  AEventID: Integer;
  APidl1, APidl2: PItemIDList;
begin
  cxShellGetNotifyParams(Message, AEventID, APidl1, APidl2);
  try
    ShellChangeNotify(AEventID, APidl1, APidl2);
  finally
    DisposePidl(APidl1);
    DisposePidl(APidl2);
  end;
end;

function TdxShellTreeView.ShowFirstLevelNodes: Boolean;
begin
  Result := FFirstLevelNodesVisible;
end;

procedure TdxShellListViewItemProducer.ProcessDetails(ShellFolder: IShellFolder;
CharWidth: Integer);
begin
  inherited ProcessDetails(ShellFolder, 10);
  ListView.DoCreateColumns;
end;

procedure TdxShellListViewItemProducer.ProcessItems(AIFolder: IShellFolder;
AFolderPIDL: PItemIDList; APreloadItemCount: Integer);
begin
  CheckThumnails;
  inherited ProcessItems(AIFolder, AFolderPIDL, APreloadItemCount);
end;

function TdxShellListViewItemProducer.CanAddFolder
  (AFolder: TcxShellFolder): Boolean;
begin
  Result := ListView.DoAddFolder(AFolder);
end;

function TdxShellListViewItemProducer.DoCompareItems(AItem1,
  AItem2: TcxShellFolder; out ACompare: Integer): Boolean;
begin
  Result := ListView.DoCompare(AItem1, AItem2, ACompare);
end;

procedure TdxShellListViewItemProducer.DoSlowInitialization
  (AItem: TcxShellItemInfo);
begin
  AItem.UpdateThumbnail;
end;

function TdxShellListViewItemProducer.GetEnumFlags: Cardinal;
begin
  Result := ListView.ShellOptions.GetEnumFlags;
  if ListView.ShellOptions.TrackShellChanges then
    Result := Result or SHCONTF_ENABLE_ASYNC;
end;

function TdxShellListViewItemProducer.GetItemsInfoGatherer
  : TcxShellItemsInfoGatherer;
begin
  Result := ListView.ItemsInfoGatherer;
end;

function TdxShellListViewItemProducer.GetThumbnailIndex
  (AItem: TcxShellItemInfo): Integer;

  procedure AddThumbnailToImageList(AHBitmap: HBITMAP; ARequiredSize: TSize);
  var
    ABitmap: TcxBitmap;
    AThumbnail: TBitmap;
    ARect: TRect;
  begin
    ABitmap := TcxBitmap32.CreateSize(cxRect(ARequiredSize));
    ABitmap.Canvas.Lock;
    try
      AThumbnail := TBitmap.Create;
      AThumbnail.Handle := AHBitmap;
      AThumbnail.Canvas.Lock;
      try
        ARect := cxRectCenter(ABitmap.ClientRect, AThumbnail.Width,
          AThumbnail.Height);
        cxBitBlt(ABitmap.Canvas.Handle, AThumbnail.Canvas.Handle, ARect,
          cxNullPoint, SRCCOPY);
        Result := ImageList_Add(FThumbnails, ABitmap.Handle, 0);
      finally
        AThumbnail.Canvas.Unlock;
        AThumbnail.Free;
      end;
    finally
      ABitmap.Canvas.Unlock;
      ABitmap.Free;
    end;
  end;

var
  ATempPIDL: PItemIDList;
  AFlags: Cardinal;
  AExtractor: IExtractImage;
  APathBuffer: array [0 .. 1024] of WideChar;
  ASize: TSize;
  APriority: Cardinal;
  AHBitmap: HBITMAP;
  ARequiredSize: TSize;
  AItemImageFactory: IShellItemImageFactory;
  AShellItem: IShellItem;
begin
  Result := inherited GetThumbnailIndex(AItem);
  if ListView.IsThumbnailView then
  begin
    ARequiredSize := ListView.GetThumbnailSize;
    if IsWinVistaOrLater and (SHCreateItemFromIDList(AItem.FullPIDL,
      CLSID_ShellItem, AShellItem) = S_OK) and
      (AShellItem.QueryInterface(IID_IShellItemImageFactory, AItemImageFactory)
      = S_OK) and (AItemImageFactory.GetImage(ARequiredSize, 0, AHBitmap) = S_OK)
    then
      AddThumbnailToImageList(AHBitmap, ARequiredSize)
    else
    begin
      ATempPIDL := GetPidlCopy(AItem.pidl);
      try
        if ShellFolder.GetUIObjectOf(0, 1, ATempPIDL, IID_IExtractImage, nil,
          AExtractor) = S_OK then
        begin
          ASize := ARequiredSize;
          APriority := IEIT_PRIORITY_NORMAL;
          AFlags := IEIFLAG_OFFLINE or IEIFLAG_QUALITY or IEIFLAG_ORIGSIZE;
          AExtractor.GetLocation(APathBuffer, 255, APriority, ASize,
            32, AFlags);
          if AExtractor.Extract(AHBitmap) = 0 then
            AddThumbnailToImageList(AHBitmap, ARequiredSize);
        end;
      finally
        DisposePidl(ATempPIDL);
      end;
    end;
  end;
end;

function TdxShellListViewItemProducer.GetShowToolTip: Boolean;
begin
  Result := ListView.ShellOptions.ShowToolTip;
end;

function TdxShellListViewItemProducer.SlowInitializationDone
  (AItem: TcxShellItemInfo): Boolean;
begin
  Result := AItem.ThumbnailUpdated;
end;

procedure TdxShellListViewItemProducer.CheckThumnails;
var
  ASize: TSize;
begin
  ASize := ListView.GetThumbnailSize;
  if ListView.IsThumbnailView then
  begin
    ListView.Images.LargeImages.Clear;
    FThumbnails := ListView.Images.LargeImages.Handle;
    ListView.InvalidateImageList;
  end;
end;

function TdxShellListViewItemProducer.GetListView: TdxShellListView;
begin
  Result := TdxShellListView(Owner);
end;

{ TdxShellListViewContextMenu }

constructor TdxShellListViewContextMenu.Create(AListView: TdxShellListView);
begin
  inherited Create;
  FListView := AListView;
end;

function TdxShellListViewContextMenu.GetWindowHandle: THandle;
begin
  Result := FListView.Handle;
end;

procedure TdxShellListViewContextMenu.Populate;
var
  AItemPIDLList: TList;
begin
  AItemPIDLList := FListView.CreateSelectedPidlsList;
  try
    AddDefaultShellItems(FListView.ItemProducer.ShellFolder, AItemPIDLList);
  finally
    FListView.DestroySelectedPidlsList(AItemPIDLList);
  end;
end;

{ TdxShellListViewCurrentFolderContextMenu }

const
  cmdExtraLargeIconId = 1;
  cmdLargeIconId = 2;
  cmdIconId = 3;
  cmdSmallIconId = 4;
  cmdListId = 5;
  cmdDetailId = 6;

  cmdRefreshId = 7;
  cmdAscendingId = 8;
  cmdDescendingId = 9;
  cmdLastId = cmdDescendingId;

procedure TdxShellListViewCurrentFolderContextMenu.ExecuteMenuItemCommand
  (ACommandId: Cardinal);
begin
  case ACommandId of
    cmdExtraLargeIconId .. cmdDetailId:
      begin
        ListView.ChangeView(ACommandId);
        ListView.DoViewChanged;
      end;
    cmdRefreshId:
      ListView.UpdateContent;
    cmdDescendingId:
      ListView.Sort(ListView.ItemProducer.SortColumn, False);
    cmdAscendingId:
      ListView.Sort(ListView.ItemProducer.SortColumn, True);
  else
    if (ACommandId > cmdLastId) and (ACommandId <= FLastSortColumnIndexCommandId)
    then
      ListView.Sort(ACommandId - cmdLastId - 1,
        ListView.ItemProducer.SortDescending)
    else
    begin
      ListView.FIsInternalItemCreation := True;
      try
        inherited ExecuteMenuItemCommand(ACommandId);
      finally
        ListView.FIsInternalItemCreation := False;
      end;
    end;
  end;
end;

procedure TdxShellListViewCurrentFolderContextMenu.Populate;

  function GetCheckedIconSizeMenuItemIndex: Integer;
  begin
    Result := ListView.GetCurrentViewId;
    Dec(Result);
  end;

const
  AIconSizeMenuItemsCount = 6;
  ASortDirectionMenuItemCaptions: array [0 .. 1] of string = ('Ascending',
    'Descending');
  SortDirectionMenuItemIndex: array [Boolean] of Integer = (0, 1);
var
  AViewSubmenu: HMenu;
  I: Integer;
  ASortColumnMenuItemCaptions: array of string;
  AViewMenuItemCaptions: array [0 .. AIconSizeMenuItemsCount - 1] of string;
  ALibraryHandle: THandle;
begin
  ALibraryHandle := LoadLibraryEx('shell32.dll', 0, LOAD_LIBRARY_AS_DATAFILE);
  try
    AViewSubmenu := AddSubItem(dxGetLocalizedSystemResourceString
      (sdxShellViewsCaption, ALibraryHandle, IfThen(IsWinSevenOrLater, 31145,
      33585)), 0);
    for I := 0 to Length(AViewMenuItemCaptions) - 1 do
      if IsWinSevenOrLater or
        not((I + 1) in [cmdExtraLargeIconId, cmdSmallIconId]) then
        AViewMenuItemCaptions[I] := ListView.GetViewCaption(I + 1,
          ALibraryHandle);
  finally
    FreeLibrary(ALibraryHandle);
  end;
  AddRadioGroup(AViewMenuItemCaptions, cmdExtraLargeIconId, 0,
    GetCheckedIconSizeMenuItemIndex, AViewSubmenu);
  if ListView.Sorting then
  begin
    SetLength(ASortColumnMenuItemCaptions, ListView.Columns.Count);
    for I := 0 to ListView.Columns.Count - 1 do
      ASortColumnMenuItemCaptions[I] := ListView.Columns[I].Caption;
    AViewSubmenu := AddSubItem('Sort by', 0);
    AddRadioGroup(ASortColumnMenuItemCaptions, cmdLastId + 1, 0,
      ListView.ItemProducer.SortColumn, AViewSubmenu);
    AddSeparator(AViewSubmenu);
    AddRadioGroup(ASortDirectionMenuItemCaptions, cmdAscendingId,
      Length(ASortColumnMenuItemCaptions) + 1,
      SortDirectionMenuItemIndex[ListView.ItemProducer.SortDescending],
      AViewSubmenu);
    FLastSortColumnIndexCommandId := cmdLastId +
      Length(ASortColumnMenuItemCaptions);
  end
  else
    FLastSortColumnIndexCommandId := 0;
  AddItem('Refresh', cmdRefreshId);
  AddDefaultShellItems(FListView.ItemProducer.ShellFolder);
end;

procedure TdxShellListViewCurrentFolderContextMenu.AddCheckItem
  (const ACaption: string; AId: Cardinal; AIsChecked: Boolean;
AMenu: HMenu = 0);
begin
  if AMenu = 0 then
    AMenu := Menu;
  AppendMenu(AMenu, MF_ENABLED or MF_STRING or IfThen(AIsChecked, MFS_CHECKED,
    MFS_UNCHECKED), AId, PChar(ACaption));
end;

procedure TdxShellListViewCurrentFolderContextMenu.AddItem(const ACaption
  : string; AId: Cardinal; AMenu: HMenu);
begin
  AddCheckItem(ACaption, AId, False, AMenu);
end;

procedure TdxShellListViewCurrentFolderContextMenu.AddRadioGroup
  (AItems: array of string; AStartId, AStartPos: Cardinal; AItemIndex: Integer;
AMenu: HMenu = 0);
var
  I: Integer;
  AMenuItemInfo: TMenuItemInfo;
begin
  if AMenu = 0 then
    AMenu := Menu;
  for I := 0 to High(AItems) do
  begin
    if AItems[I] = '' then
      Continue;
    cxZeroMemory(@AMenuItemInfo, SizeOf(AMenuItemInfo));
    AMenuItemInfo.cbSize := SizeOf(AMenuItemInfo);
    AMenuItemInfo.fMask := MIIM_FTYPE or MIIM_STATE or MIIM_STRING or MIIM_ID;
    AMenuItemInfo.fType := MFT_RADIOCHECK;
    AMenuItemInfo.fState := IfThen(I = AItemIndex, MFS_CHECKED, MFS_UNCHECKED);
    AMenuItemInfo.dwTypeData := PChar(AItems[I]);
    AMenuItemInfo.wID := AStartId + Cardinal(I);
    InsertMenuItem(AMenu, AStartPos + Cardinal(I), True, AMenuItemInfo);
  end;
end;

procedure TdxShellListViewCurrentFolderContextMenu.AddSeparator
  (AMenu: HMenu = 0);
begin
  if AMenu = 0 then
    AMenu := Menu;
  AppendMenu(AMenu, MF_SEPARATOR, 0, nil);
end;

function TdxShellListViewCurrentFolderContextMenu.AddSubItem(const ACaption
  : string; AMenu: HMenu): HMenu;
begin
  Result := CreatePopupMenu;
  if AMenu = 0 then
    AMenu := Menu;
  AppendMenu(AMenu, MF_ENABLED or MF_STRING or MF_POPUP, Result,
    PChar(ACaption));
end;

{ TdxShellListViewOptions }

constructor TdxShellListViewOptions.Create(AOwner: TWinControl);
begin
  inherited Create(AOwner);
  FShowReadOnly := False;
end;

procedure TdxShellListViewOptions.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TdxShellListViewOptions then
    ShowReadOnly := TdxShellListViewOptions(Source).ShowReadOnly;
end;

{ TdxShellListViewPainter }

procedure TdxShellListViewPainter.PrepareGlyphBitmap(ABitmap: TcxAlphaBitmap;
AImages: TCustomImageList; AImageIndex, AOverlayIndex: Integer;
ADrawMode: TcxImageDrawMode; ASmoothImage: Boolean; ABrush: THandle;
ATransparentColor: TColor; AUseLeftBottomPixelAsTransparent: Boolean;
APalette: IdxColorPalette);

  function IndexToOverlayMask(Index: Integer): Integer;
  begin
    Result := Index shl 8;
  end;

  function GetDrawingStyle(AStyle: TDrawingStyle): Integer;
  const
    DrawingStyles: array [TDrawingStyle] of Longint = (ILD_FOCUS, ILD_SELECTED,
      ILD_NORMAL, ILD_TRANSPARENT);
  begin
    Result := DrawingStyles[AStyle];
    if AOverlayIndex >= 0 then
      Result := Result or ILD_OVERLAYMASK and
        IndexToOverlayMask(AOverlayIndex + 1);
  end;

  procedure Lighten(APercent: Byte);
  var
    X: Integer;
    Y: Integer;
    Pixel: PRGBQuad;
  begin
    for Y := 0 to ABitmap.Height - 1 do
    begin
      Pixel := ABitmap.ScanLine[Y];
      for X := 0 to ABitmap.Width - 1 do
      begin
        with Pixel^ do
        begin
          rgbRed := Byte(Trunc(255 - APercent / 100 * (255 - rgbRed)));
          rgbGreen := Byte(Trunc(255 - APercent / 100 * (255 - rgbGreen)));
          rgbBlue := Byte(Trunc(255 - APercent / 100 * (255 - rgbBlue)));
        end;
        Inc(Pixel);
      end;
    end;
  end;

var
  ABounds: TRect;
begin
  if IsWinSevenOrLater then
  begin
    ABitmap.HandleType := bmDIB;
    ABitmap.IgnorePalette := True;
    ABitmap.AlphaFormat := afIgnored;
    ABitmap.Clear;
    ImageList_DrawEx(AImages.Handle, AImageIndex, ABitmap.Canvas.Handle, 0, 0,
      0, 0, CLR_NONE, CLR_NONE, GetDrawingStyle(TDrawingStyle.dsNormal));
  end
  else
  begin
    ABounds := ABitmap.ClientRect;
    cxDrawImage(ABitmap.cxCanvas.Handle, ABounds, ABounds, nil, AImages,
      AImageIndex, idmNormal, False, ABrush, ATransparentColor,
      AUseLeftBottomPixelAsTransparent, APalette);
    cxDrawImage(ABitmap.cxCanvas.Handle, ABounds, ABounds, nil, AImages,
      AOverlayIndex, idmNormal, False, ABrush, ATransparentColor,
      AUseLeftBottomPixelAsTransparent, APalette);
  end;
  if ADrawMode = idmDingy then
    Lighten(50);
end;

{ TdxShellListItemViewInfo }

procedure TdxShellListItemViewInfo.DrawGlyph(ACanvas: TcxCustomCanvas);

  procedure InternalDrawIcon(ACanvas: TcxCustomCanvas;
  AShellItem: TcxShellItemInfo);
  var
    R: TRect;
  begin
    R := cxRectCenter(GlyphBounds, ShellListView.LargeImages.Width,
      ShellListView.LargeImages.Height);
    if IsWinXP then
      ACanvas.FrameRect(GlyphBounds, clBtnFace);
    DrawGlyphCore(ACanvas, R, ShellListView.LargeImages, AShellItem.IconIndex,
      -1, GetGlyphState);
  end;

var
  AShellItem: TcxShellItemInfo;
begin
  if ShellListView.IsThumbnailView then
  begin
    AShellItem := ShellListView.GetItemInfo(ItemIndex);
    if AShellItem.HasThumbnail then
      inherited DrawGlyph(ACanvas)
    else
      InternalDrawIcon(ACanvas, AShellItem);
    TThumbnailOverlayImageHelper(ShellListView.ThumbnailOverlayImageHelper)
      .Draw(ACanvas, GlyphBounds, AShellItem.OverlayIndex);
  end
  else
    inherited DrawGlyph(ACanvas);
end;

function TdxShellListItemViewInfo.GetShellListView: TdxShellListView;
begin
  Result := ListView as TdxShellListView;
end;

{ TdxShellListViewViewInfo }

function TdxShellListViewViewInfo.CreateItemViewInfo
  (AOwner: TdxListViewCustomGroupViewInfo; AItemIndex: Integer)
  : TdxListItemCustomViewInfo;
begin
  if IsReportView then
    Result := inherited CreateItemViewInfo(AOwner, AItemIndex)
  else
    Result := TdxShellListItemViewInfo.Create(AOwner, AItemIndex,
      ItemViewParams);
end;

function TdxShellListViewViewInfo.GetIconsGlyphSideGap: Integer;
begin
  Result := 11;
end;

{ TdxShellListViewController }

procedure TdxShellListViewController.ProcessItemMouseDown
  (AItemViewInfo: TdxListItemCustomViewInfo; AButton: TMouseButton;
AShift: TShiftState; const AMousePos: TPoint);
begin
  inherited ProcessItemMouseDown(AItemViewInfo, AButton, AShift, AMousePos);
  if (AShift = [ssLeft, ssDouble]) and TdxShellListView(ListView)
    .GetItemInfo(AItemViewInfo.Item.Index).IsFolder then
  begin
    FocusedItemIndex := -1;
    ClearSelection;
  end;
end;

procedure TdxShellListViewController.SelectAll;
var
  I: Integer;
begin
  ResetSelectionChangedFlag;
  ListView.BeginUpdate;
  try
    for I := 0 to ListView.Items.Count - 1 do
      SelectItem(I, True);
  finally
    ListView.EndUpdate;
    CheckSelectionChangedFlag;
  end;
end;

procedure TdxShellListViewController.KeyDown(AKey: Word; AShift: TShiftState);
begin
  inherited KeyDown(AKey, AShift);
  case AKey of
    Ord('A'):
      if AShift = [ssCtrl] then
        SelectAll;
  end;
end;

{ TdxShellListView }

constructor TdxShellListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDependedControls := TcxShellDependedControls.Create;
  FNavigateFolderLinks := True;
  FFirstUpdateItem := -1;
  FLargeImages := TImageList.Create(nil);
  FLargeImages.ShareImages := True;
  FFakeThumbnailImages := TImageList.Create(nil);

  CheckLargeImages;
  FInternalSmallImages := TImageList.Create(Self);
  FInternalSmallImages.ShareImages := True;
  FInternalSmallImages.Handle := cxShellGetImageList(SHGFI_SMALLICON);

  FItemProducer := TdxShellListViewItemProducer.Create(Self);
  FItemsInfoGatherer := TcxShellItemsInfoGatherer.Create(Self);
  FLastUpdateItem := -1;
  FShellOptions := TdxShellListViewOptions.Create(Self);
  FThumbnailOptions := TcxShellThumbnailOptions.Create(Self);
  TcxShellThumbnailOptionsAccess(FThumbnailOptions).OnChange :=
    ThumbnailOptionsChanged;
  FShellRoot := TcxShellListRoot.Create(Self, 0);
  FShellRoot.OnFolderChanged := RootFolderChanged;
  FShellRoot.OnSettingsChanged := RootSettingsChanged;

  DoubleBuffered := True;
  Images.LargeImages := FLargeImages;
  Options.OwnerData := True;

  // visual settings
  Options.PaddingOptions.Item.Margin := TRect.Create(2, 6, 2, 6);
  Options.ExplorerStyle := True;
  Options.Report.RowSelect := True;
  Options.Icons.TextLineCount := 3;
  Options.Icons.SmallIconsColumnWidth := 306;
  Options.ShowCheckBoxes := GetExplorerShowCheckBoxes;

  FAppEvents := TApplicationEvents.Create(nil);
  FAppEvents.OnIdle := DoOnIdle;

  FSelectedFiles := TStringList.Create;
  ParentFont := True;
  FThumbnailOverlayImageHelper := TThumbnailOverlayImageHelper.Create;
end;

destructor TdxShellListView.Destroy;
begin
  FreeAndNil(FThumbnailOverlayImageHelper);
  FreeAndNil(FSelectedFiles);
  SaveAbsolutePIDL(nil);
  FreeAndNil(FAppEvents);
  RemoveChangeNotification;
  FreeAndNil(FItemProducer);
  FreeAndNil(FItemsInfoGatherer);
  FreeAndNil(FShellRoot);
  FreeAndNil(FShellOptions);
  FreeAndNil(FThumbnailOptions);
  FreeAndNil(FInternalSmallImages);
  FreeAndNil(FFakeThumbnailImages);
  FreeAndNil(FLargeImages);
  FreeAndNil(FDependedControls);
  inherited Destroy;
end;

procedure TdxShellListView.BrowseParent;
var
  APIDL: PItemIDList;
begin
  APIDL := GetPidlParent(ItemProducer.FolderPidl);
  try
    Navigate(APIDL);
  finally
    DisposePidl(APIDL);
  end;
end;

procedure TdxShellListView.Sort;
begin
  ItemProducer.Sort;
end;

procedure TdxShellListView.Sort(AColumnIndex: Integer; AIsAscending: Boolean);
begin
  if (AColumnIndex <> ItemProducer.SortColumn) or
    (ItemProducer.SortDescending xor not AIsAscending) then
  begin
    ItemProducer.SortColumn := AColumnIndex;
    ItemProducer.SortDescending := not AIsAscending;
    SortColumnChanged;
  end;
end;

procedure TdxShellListView.UpdateContent;
begin
  DoUpdateContent(CheckUpdateItems);
end;

function TdxShellListView.CanEdit(AItem: TdxListItem): Boolean;
begin
  Result := True;
  if AItem = nil then
    Exit;
  if AItem.Index > ItemProducer.Items.Count - 1 then
  begin
    Result := False;
    Exit;
  end;
  Result := GetItemInfo(AItem.Index).CanRename and inherited CanEdit(AItem);
end;

function TdxShellListView.CreateController: TdxListViewController;
begin
  Result := TdxShellListViewController.Create(Self);
end;

function TdxShellListView.CreatePainter: TdxListViewPainter;
begin
  Result := TdxShellListViewPainter.Create(Self);
end;

function TdxShellListView.CreateViewInfo: TdxListViewViewInfo;
begin
  Result := TdxShellListViewViewInfo.Create(Self);
end;

procedure TdxShellListView.CreateWnd;
begin
  inherited CreateWnd;
  if FInternalSmallImages.Handle <> 0 then
    Images.SmallImages := FInternalSmallImages;
  if ShellRoot.pidl = nil then
    TcxShellTreeRootAccess(ShellRoot).CheckRoot
  else
    CheckUpdateItems;
  if FAbsolutePIDL <> nil then
    AbsolutePIDL := FAbsolutePIDL;
end;

procedure TdxShellListView.DblClick;
var
  AItemViewInfo: TdxListItemCustomViewInfo;
  AMousePos: TPoint;
begin
  AMousePos := GetMouseCursorClientPos;
  if ViewInfo.GetItemAtPos(AMousePos, AItemViewInfo) then
  begin
    if AItemViewInfo.GetPart(AMousePos) <> TdxListItemPart.StateGlyph then
      DoItemDblClick(AItemViewInfo.ItemIndex);
  end;
  inherited DblClick;
end;

procedure TdxShellListView.DestroyWnd;
var
  APIDL: PItemIDList;
begin
  APIDL := AbsolutePIDL;
  try
    SaveAbsolutePIDL(APIDL);
  finally
    DisposePidl(APIDL);
  end;
  RemoveChangeNotification;
  Columns.Clear;
  inherited DestroyWnd;
end;

procedure TdxShellListView.DoChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  inherited DoChangeScaleEx(M, D, isDpiChange);
  UpdateThumbnailSize(FViewID);
end;

procedure TdxShellListView.DoColumnClick(AColumn: TdxListColumn);
begin
  if FSorting then
  begin
    if ItemProducer.SortColumn = AColumn.Index then
      ItemProducer.SortDescending := not ItemProducer.SortDescending
    else
      ItemProducer.SortColumn := AColumn.Index;
    SortColumnChanged;
  end;
  inherited;
end;

procedure TdxShellListView.DoContextPopup(MousePos: TPoint;
var Handled: Boolean);
begin
  if ShellOptions.ContextMenus and (SelectedItemCount > 0) or
    (SelectedItemCount = 0) and ShellOptions.CurrentFolderContextMenu then
  begin
    Handled := True;
    ItemProducer.LockRead;
    try
      DisplayContextMenu(ClientToScreen(MousePos));
    finally
      ItemProducer.UnlockRead;
    end;
  end
  else
    inherited DoContextPopup(MousePos, Handled);
end;

procedure TdxShellListView.DoEdited(AItem: TdxListItem; var ACaption: string);
var
  AShellItem: TcxShellItemInfo;
  APIDL: PItemIDList;
begin
  if AItem.Caption = ACaption then
    Exit;
  inherited DoEdited(AItem, ACaption);
  AShellItem := GetItemInfo(AItem.Index);
  if Succeeded(ItemProducer.ShellFolder.SetNameOf(Handle, AShellItem.pidl,
    PWideChar(ACaption), SHGDN_INFOLDER or SHGDN_FOREDITING, APIDL)) then
    try
      if not ShellOptions.TrackShellChanges then
        AShellItem.SetNewPidl(ItemProducer.ShellFolder,
          ItemProducer.FolderPidl, APIDL);
    finally
      DisposePidl(APIDL);
    end;
end;

procedure TdxShellListView.FinishItemCaptionEditing(AAccept: Boolean = True);
var
  AIsEditing: Boolean;
begin
  if FIsFinishEditing then
    Exit;
  FIsFinishEditing := True;
  AIsEditing := IsEditing;
  inherited FinishItemCaptionEditing(AAccept);
  if AAccept and AIsEditing and not ShellOptions.TrackShellChanges then
    UpdateContent;
  FIsFinishEditing := False;
end;

function TdxShellListView.InternalMouseWheel(Shift: TShiftState;
WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  if ssCtrl in Shift then
  begin
    FinishItemCaptionEditing(True);
    Result := True;
  end
  else
    Result := inherited InternalMouseWheel(Shift, WheelDelta, MousePos);
end;

function TdxShellListView.IsMouseWheelHandleNeeded(Shift: TShiftState;
WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := (ssCtrl in Shift) or inherited IsMouseWheelHandleNeeded(Shift,
    WheelDelta, MousePos);
end;

procedure TdxShellListView.KeyDown(var Key: Word; Shift: TShiftState);

  procedure InvokeContextMenuCommand(const ACommandStr: AnsiString);
  var
    AShellFolder: IShellFolder;
    AItemPIDLList: TList;
  begin
    AItemPIDLList := CreateSelectedPidlsList;
    try
      if FocusedItem <> nil then
        AShellFolder := ShellRoot.ShellFolder
      else
      begin
        AShellFolder := ShellRoot.Folder.ParentShellFolder;
        AItemPIDLList.Add(GetPidlCopy(ShellRoot.Folder.RelativePIDL));
      end;
      cxShellInvokeContextMenuCommand(AShellFolder, AItemPIDLList, ACommandStr);
    finally
      DestroySelectedPidlsList(AItemPIDLList);
    end;
  end;

begin
  inherited KeyDown(Key, Shift);
  if not IsEditing then
    case Key of
      VK_RETURN:
        if Controller.FocusedItemIndex >= 0 then
          DoItemDblClick(Controller.FocusedItemIndex);
      VK_BACK:
        if ShellOptions.AutoNavigate then
          BrowseParent;
      VK_F2:
        if FocusedItem <> nil then
          FocusedItem.EditCaption;
      VK_F5:
        UpdateContent;
    else
      if ShellOptions.ContextMenus and
        (cxShellIsClipboardCommandContextMenuShortCut(Key, Shift) or
        (Key = VK_DELETE) and (Controller.FocusedItemIndex >= 0)) then
        InvokeContextMenuCommand(cxShellGetContextMenuCommandStrByShortCut
          (Key, Shift));
    end;
end;

procedure TdxShellListView.Loaded;
begin
  inherited Loaded;
  if csDesigning in ComponentState then
    TcxShellTreeRootAccess(ShellRoot).RootUpdated
  else
    CheckUpdateItems;
end;

function TdxShellListView.OwnerDataFetch(AItem: TdxListItem;
ARequest: TItemRequest): Boolean;
var
  AShellItem: TcxShellItemInfo;
  I: Integer;
begin
  Result := True;
  ItemProducer.LockRead;
  try
    if AItem.Index >= ItemProducer.Items.Count then
      Exit;
    AShellItem := GetItemInfo(AItem.Index);
    AItem.Caption := AShellItem.Name;
    if IsThumbnailView then
      AItem.ImageIndex := AShellItem.ThumbnailIndex
    else
      AItem.ImageIndex := AShellItem.IconIndex;
    AShellItem.CheckUpdate(ItemProducer.ShellFolder,
      ItemProducer.FolderPidl, False);
    if not AShellItem.ThumbnailUpdated then
      FItemsInfoGatherer.RequestItemInfo(AShellItem);
    if ViewStyle = TdxListViewStyle.Report then
    begin
      if AShellItem.Details.Count = 0 then
        AShellItem.FetchDetails(Handle, ItemProducer.ShellFolder,
          ItemProducer.Details);
      for I := 0 to AShellItem.Details.Count - 1 do
        AItem.SubItems.Add(AShellItem.Details[I]);
    end;
    AItem.Cut := AShellItem.IsGhosted;
    AItem.OverlayIndex := AShellItem.OverlayIndex;
  finally
    ItemProducer.UnlockRead;
  end;
  Result := inherited OwnerDataFetch(AItem, ARequest);
end;

function TdxShellListView.OwnerDataFind(AFind: TItemFind;
const AFindString: string; const AFindPosition: TPoint; AFindData: Pointer;
AStartIndex: Integer; ADirection: TSearchDirection; AWrap: Boolean): Integer;

  function IsItemSuitable(AIndex: Integer; ALocalFind: TItemFind;
  const ALocalFindString: string): Boolean;
  var
    ACaption: string;
  begin
    ACaption := GetItemInfo(AIndex).Name;
    if ALocalFind = ifPartialString then
      ACaption := Copy(ACaption, 1, Length(ALocalFindString));
    Result := CompareText(ALocalFindString, ACaption) = 0;
  end;

  function FindItemByCaption(ALocalFind: TItemFind;
  const ALocalFindString: string; ALocalStartIndex: Integer;
  ALocalWrap: Boolean): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    ALocalStartIndex := EnsureRange(ALocalStartIndex, 0,
      ItemProducer.Items.Count - 1);
    for I := ALocalStartIndex to ItemProducer.Items.Count - 1 do
      if IsItemSuitable(I, ALocalFind, ALocalFindString) then
      begin
        Result := I;
        Break;
      end;
    if ALocalWrap and (Result = -1) then
      for I := 0 to ALocalStartIndex - 1 do
        if IsItemSuitable(I, ALocalFind, ALocalFindString) then
        begin
          Result := I;
          Break;
        end;
  end;

begin
  Result := inherited OwnerDataFind(AFind, AFindString, AFindPosition,
    AFindData, AStartIndex, ADirection, AWrap);
  if (Result = -1) and (AFind in [ifPartialString, ifExactString]) then
    Result := FindItemByCaption(AFind, AFindString, AStartIndex, AWrap);
end;

procedure TdxShellListView.ShowInplaceEdit(AItemIndex: Integer; ABounds: TRect;
const AText: string);
var
  AFolder: TcxShellFolder;
  ASelCount: Integer;
begin
  AFolder := Folders[AItemIndex];
  if AFolder.IsFolder then
    ASelCount := MaxInt
  else
    ASelCount := Length(TPath.GetFileNameWithoutExtension(AFolder.PathName));
  InplaceEdit.Show(Self, ABounds, AText, Fonts.Item, 0, ASelCount, MAX_PATH);
end;

function TdxShellListView.SupportsItemEnabledState: Boolean;
begin
  Result := False;
end;

procedure TdxShellListView.ValidatePasteText(var AText: string);
var
  I: Integer;
begin
  for I := Length(AText) downto 1 do
    if not IsValidShellNameChar(AText[I]) then
      Delete(AText, I, 1);
end;

procedure TdxShellListView.ViewStyleChanged;
var
  P: TPoint;
begin
  if ViewStyle = TdxListViewStyle.Icon then
    P := ScaleFactor.Apply(TPoint.Create(2, 6))
  else
    P := ScaleFactor.Apply(TPoint.Create(5, 1));
  Options.PaddingOptions.Item.Margin := TRect.Create(P, P);
  if (ViewStyle = TdxListViewStyle.Report) and LookAndFeel.NativeStyle then
    Fonts.SubItem.Color := cl3DDkShadow;
end;

procedure TdxShellListView.ChangeView(AViewId: Integer);
begin
  FViewID := AViewId;
  case AViewId of
    cmdSmallIconId:
      ViewStyle := TdxListViewStyle.SmallIcon;
    cmdListId:
      ViewStyle := TdxListViewStyle.List;
    cmdDetailId:
      ViewStyle := TdxListViewStyle.Report;
  else // cmdExtraLargeId, cmdLargeId, cmdIconId
    ViewStyle := TdxListViewStyle.Icon;
    if IsWinSevenOrLater or (AViewId = cmdLargeIconId) then
      LargeIconSize := isExtraLarge
    else
      LargeIconSize := isDefault;
    UpdateThumbnailSize(AViewId);
  end;
  ThumbnailOptions.ShowThumbnails :=
    (AViewId in [cmdExtraLargeIconId, cmdLargeIconId]) or IsWinSevenOrLater and
    (AViewId = cmdIconId);
  if IsThumbnailView then
    TThumbnailOverlayImageHelper(ThumbnailOverlayImageHelper)
      .Initialize(ActualCanvas, ThumbnailOptions.Width);
end;

function TdxShellListView.CheckFileMask(AFolder: TcxShellFolder): Boolean;

  function CheckIsFileLink(out AFileName: string): Boolean;
  var
    ALink: IShellLink;
    ATargetPidl: PItemIDList;
  begin
    Result := AFolder.IsLink and
      Succeeded(AFolder.ParentShellFolder.BindToObject(AFolder.RelativePIDL,
      nil, IID_IShellLink, ALink)) and Succeeded(ALink.GetIDList(ATargetPidl));
    if Result then
    begin
      AFileName := GetPidlName(ATargetPidl);
      Result := AFileName <> '';
    end
    else
      AFileName := '';
  end;

var
  AFileName: string;
begin
  if AFolder.IsFolderLink then
    Exit(True);
  if not CheckIsFileLink(AFileName) then
    AFileName := AFolder.PathName;
  Result := ShellOptions.IsFileNameValid(ExtractFileName(AFileName));
end;

procedure TdxShellListView.CheckLargeImages;
var
  AHImages: HIMAGELIST;
begin
  if IsWinXPOrLater then
  begin
    SHGetImageList(GetLargeImageListType, IID_IImageList, AHImages);
    FLargeImages.Handle := AHImages;
  end
  else
    FLargeImages.Handle := cxShellGetImageList(SHGFI_LARGEICON);
end;

procedure TdxShellListView.CheckUpdateItems;
var
  APIDL: PItemIDList;
  AItemIndex: Integer;
begin
  if IsLoading or not HandleAllocated then
    Exit;
  APIDL := nil;
  try
    if IsEditing then
    begin
      APIDL := GetPidlCopy(GetPidlByItemIndex(FocusedItem.Index));
      FinishItemCaptionEditing(False);
    end;
    BeginUpdate;
    try
      ItemProducer.ClearItems;
      if not ShellRoot.IsValid then
        Items.Clear
      else if ItemProducer.Items.Count = 0 then
        ItemProducer.ProcessItems(ShellRoot.ShellFolder, ShellRoot.pidl,
          PRELOAD_ITEMS_COUNT);
      CreateChangeNotification;
    finally
      EndUpdate;
    end;
    if APIDL <> nil then
    begin
      AItemIndex := ItemProducer.GetItemIndexByPidl(APIDL);
      if (AItemIndex >= 0) and (AItemIndex < Items.Count) then
        Items[AItemIndex].EditCaption;
    end;
  finally
    DisposePidl(APIDL);
  end;
end;

procedure TdxShellListView.CreateChangeNotification;
begin
  if ShellOptions.TrackShellChanges then
    cxShellRegisterChangeNotifier(ItemProducer.FolderPidl, Handle,
      DSM_SYSTEMSHELLCHANGENOTIFY, False, FShellChangeNotifierData)
  else
    RemoveChangeNotification;
end;

procedure TdxShellListView.CreateNewFolder;
var
  AIContextMenu: IContextMenu;
  AInvokeCommandInfo: TCMInvokeCommandInfo;
  AMenu: HMenu;
begin
  AMenu := CreatePopupMenu;
  try
    if Succeeded(ShellRoot.ShellFolder.CreateViewObject(Handle,
      IID_IContextMenu, AIContextMenu)) then
    begin
      AIContextMenu.QueryContextMenu(AMenu, 0, 0, $7FFF, 0);
      ZeroMemory(@AInvokeCommandInfo, SizeOf(AInvokeCommandInfo));
      AInvokeCommandInfo.cbSize := SizeOf(AInvokeCommandInfo);
      AInvokeCommandInfo.hwnd := Handle;
      AInvokeCommandInfo.lpVerb := PAnsiChar('NewFolder');
      AInvokeCommandInfo.nShow := SW_SHOWNORMAL;
      FIsInternalItemCreation := True;
      try
        AIContextMenu.InvokeCommand(AInvokeCommandInfo);
      finally
        FIsInternalItemCreation := False;
      end;
    end;
  finally
    DestroyMenu(AMenu);
  end;
end;

function TdxShellListView.CreateSelectedPidlsList: TList;
var
  APIDL: PItemIDList;
  I: Integer;
begin
  Result := TList.Create;
  for I := 0 to SelectedItemCount - 1 do
  begin
    APIDL := GetPidlByItemIndex(TdxShellListViewController(Controller)
      .SelectedIndices[I]);
    if APIDL <> nil then
      Result.Add(GetPidlCopy(APIDL));
  end;
end;

procedure TdxShellListView.DestroySelectedPidlsList(ASelectedPidls: TList);
var
  I: Integer;
begin
  try
    for I := 0 to ASelectedPidls.Count - 1 do
      DisposePidl(ASelectedPidls[I]);
  finally
    FreeAndNil(ASelectedPidls);
  end;
end;

procedure TdxShellListView.DisplayContextMenu(const APos: TPoint);
var
  AContextMenu: TdxShellListViewContextMenu;
begin
  if SelectedItemCount <> 0 then
    AContextMenu := TdxShellListViewContextMenu.Create(Self)
  else
    AContextMenu := TdxShellListViewCurrentFolderContextMenu.Create(Self);
  try
    AContextMenu.Popup(APos);
  finally
    AContextMenu.Free;
  end;
end;

function TdxShellListView.DoAddFolder(AFolder: TcxShellFolder): Boolean;
var
  AIsFolder: Boolean;
begin
  AIsFolder := AFolder.IsFolder and (ShellOptions.ShowZipFilesWithFolders or
    not cxShellIsZipFile(AFolder));

  Result := AIsFolder or (ShellOptions.ShowReadOnly or
    not(sfaReadOnly in AFolder.Attributes)) and CheckFileMask(AFolder);

  if Assigned(FOnAddFolder) then
    FOnAddFolder(Self, AFolder, Result);
end;

procedure TdxShellListView.DoAfterNavigation;
begin
  if Assigned(AfterNavigation) then
    AfterNavigation(Self, ShellRoot.pidl, ShellRoot.CurrentPath);
end;

procedure TdxShellListView.DoBeforeNavigation(APIDL: PItemIDList);
var
  ADesktop: IShellFolder;
  APath: WideString;
  AName: TStrRet;
begin
  if Failed(SHGetDesktopFolder(ADesktop)) then
    Exit;
  if Succeeded(ADesktop.GetDisplayNameOf(APIDL, SHGDN_NORMAL or
    SHGDN_FORPARSING, AName)) then
    APath := GetTextFromStrRet(AName, APIDL)
  else
    APath := '';
  if Assigned(BeforeNavigation) then
    BeforeNavigation(Self, APIDL, APath);
end;

function TdxShellListView.DoCompare(AItem1, AItem2: TcxShellFolder;
out ACompare: Integer): Boolean;
begin
  Result := Assigned(FOnCompare);
  if Result then
    FOnCompare(Self, AItem1, AItem2, ACompare);
end;

procedure TdxShellListView.DoCreateColumns;
var
  AColumn: TdxListColumn;
  I, AMaxWidth: Integer;
begin
  Columns.BeginUpdate;
  try
    Columns.Clear;
    AMaxWidth := Screen.WorkAreaRect.Width;
    for I := 0 to ItemProducer.Details.Count - 1 do
    begin
      AColumn := Columns.Add;
      AColumn.Caption := ItemProducer.Details[I].Text;
      AColumn.Alignment := ItemProducer.Details[I].Alignment;
      AColumn.MaxWidth := AMaxWidth;
      AColumn.MinWidth := ScaleFactor.Apply(90);
      AColumn.Width := ItemProducer.Details[I].Width;
      UpdateColumnSortOrder(I);
    end;
  finally
    Columns.EndUpdate;
  end;
end;

procedure TdxShellListView.DoItemDblClick(AItemIndex: Integer);
var
  AShellItemInfo: TcxShellItemInfo;
  ALink: IShellLink;
  ATargetPidl: PItemIDList;
begin
  if ShellOptions.AutoNavigate then
  begin
    ItemProducer.LockRead;
    try
      AShellItemInfo := GetItemInfo(AItemIndex);
      if AShellItemInfo.IsFolder and (ShellOptions.ShowZipFilesWithFolders or
        not cxShellIsZipFile(AShellItemInfo.Folder)) then
      begin
        if AShellItemInfo.Folder.ShellFolder <> nil then
          DoProcessNavigation(AShellItemInfo);
      end
      else if NavigateFolderLinks and AShellItemInfo.IsFolderLink and
        (ShellOptions.ShowZipFilesWithFolders or
        not AShellItemInfo.IsZipFolderLink) then
      begin
        if Succeeded(ItemProducer.ShellFolder.BindToObject(AShellItemInfo.pidl,
          nil, IID_IShellLink, ALink)) and
          Succeeded(ALink.GetIDList(ATargetPidl)) and
          Succeeded(ALink.Resolve(Handle, 0)) then
          Navigate(ATargetPidl)
      end
      else if ShellOptions.AutoExecute then
        DoProcessDefaultCommand(AShellItemInfo);
    finally
      ItemProducer.UnlockRead;
    end;
  end;
end;

procedure TdxShellListView.DoOnIdle(Sender: TObject; var Done: Boolean);
var
  AList: TList;
  AShellItem: TcxShellItemInfo;
begin
  AList := FItemsInfoGatherer.ProcessedItems.LockList;
  try
    if AList.Count > 0 then
      AShellItem := TcxShellItemInfo(AList.Extract(AList.First))
    else
      AShellItem := nil;
  finally
    FItemsInfoGatherer.ProcessedItems.UnlockList;
  end;
  if AShellItem <> nil then
    LayoutChanged;
end;

procedure TdxShellListView.DoProcessDefaultCommand
  (AShellItem: TcxShellItemInfo);
var
  APIDL: PItemIDList;
  AExecuteInfo: PShellExecuteInfo;
  AHandled: Boolean;
begin
  APIDL := ConcatenatePidls(ItemProducer.FolderPidl, AShellItem.pidl);
  try
    AHandled := False;
    if Assigned(OnExecuteItem) then
      OnExecuteItem(Self, APIDL, AHandled);
    if not AHandled then
    begin
      New(AExecuteInfo);
      try
        ZeroMemory(AExecuteInfo, SizeOf(TShellExecuteInfo));
        AExecuteInfo.cbSize := SizeOf(TShellExecuteInfo);
        AExecuteInfo.fMask := SEE_MASK_INVOKEIDLIST;
        AExecuteInfo.Wnd := Handle;
        AExecuteInfo.lpIDList := APIDL;
        AExecuteInfo.nShow := SW_SHOW;
        ShellExecuteEx(AExecuteInfo);
      finally
        Dispose(AExecuteInfo);
      end;
    end;
  finally
    DisposePidl(APIDL);
  end;
end;

procedure TdxShellListView.DoProcessNavigation(AShellItem: TcxShellItemInfo);
var
  APIDL: PItemIDList;
begin
  if not AShellItem.IsFolder then
    Exit;
  APIDL := ConcatenatePidls(ItemProducer.FolderPidl, AShellItem.pidl);
  try
    Navigate(APIDL);
  finally
    DisposePidl(APIDL);
  end;
end;

procedure TdxShellListView.DoViewChanged;
begin
  dxCallNotify(FOnViewChanged, Self);
end;

function TdxShellListView.GetCurrentViewId: Integer;
begin
  Result := cmdDetailId;
  case ViewStyle of
    TdxListViewStyle.Icon:
      begin
        if not IsWinSevenOrLater then
          if ThumbnailOptions.ShowThumbnails then
            Result := cmdLargeIconId
          else
            Result := cmdIconId
        else if ThumbnailOptions.Width >= dxShellExtraLargeIconSize.cx then
          Result := cmdExtraLargeIconId
        else if ThumbnailOptions.Width >= dxShellLargeIconSize.cx then
          Result := cmdLargeIconId
        else
          Result := cmdIconId;
      end;
    TdxListViewStyle.SmallIcon:
      Result := cmdSmallIconId;
    TdxListViewStyle.List:
      Result := cmdListId;
  end;
end;

function TdxShellListView.GetItemInfo(AIndex: Integer): TcxShellItemInfo;
begin
  Result := ItemProducer.ItemInfo[AIndex];
end;

function TdxShellListView.GetLargeIconSize: TSize;
begin
  Result := cxSize(FLargeImages.Width, FLargeImages.Height);
end;

function TdxShellListView.GetPidlByItemIndex(AIndex: Integer): PItemIDList;
begin
  Result := GetItemInfo(AIndex).pidl;
end;

function TdxShellListView.GetThumbnailSize: TSize;
begin
  Result := cxSize(ThumbnailOptions.Width, ThumbnailOptions.Height);
end;

function TdxShellListView.GetViewCaption(const AViewId: Integer;
AShell32DLLHandle: THandle = 0): string;
var
  ALibraryHandle: THandle;
begin
  if AShell32DLLHandle <> 0 then
    ALibraryHandle := AShell32DLLHandle
  else
    ALibraryHandle := LoadLibraryEx('shell32.dll', 0, LOAD_LIBRARY_AS_DATAFILE);
  try
    case AViewId of
      cmdExtraLargeIconId:
        Result := dxGetLocalizedSystemResourceString
          (sdxShellExtraLargeIconsCaption, ALibraryHandle,
          IfThen(IsWinSevenOrLater, 31136, -1));
      cmdLargeIconId:
        Result := dxGetLocalizedSystemResourceString(sdxShellLargeIconsCaption,
          ALibraryHandle, IfThen(IsWinSevenOrLater, 31137, 16384));
      cmdIconId:
        if IsWinSevenOrLater then
          Result := dxGetLocalizedSystemResourceString
            (sdxShellMediumIconsCaption, ALibraryHandle, 31138)
        else
          Result := cxGetResourceString(@sdxShellIconsCaption);
      cmdSmallIconId:
        Result := dxGetLocalizedSystemResourceString(sdxShellSmallIconsCaption,
          ALibraryHandle, 31139);
      cmdListId:
        Result := dxGetLocalizedSystemResourceString(sdxShellListCaption,
          ALibraryHandle, IfThen(IsWinSevenOrLater, 31140, 33579));
      cmdDetailId:
        Result := dxGetLocalizedSystemResourceString(sdxShellDetailsCaption,
          ALibraryHandle, IfThen(IsWinSevenOrLater, 31141, 33580));
    else
      Result := '';
    end;
    Result := StringReplace(Result, '|', '', [rfReplaceAll]);
  finally
    if AShell32DLLHandle = 0 then
      FreeLibrary(ALibraryHandle);
  end;
end;

function TdxShellListView.GetViewOptions(AForNavigation: Boolean = False)
  : TcxShellViewOptions;
begin
  if AForNavigation then
    Result := [svoShowFolders, svoShowHidden]
  else
  begin
    Result := [];
    if ShellOptions.ShowNonFolders then
      Include(Result, svoShowFiles);
    if ShellOptions.ShowFolders then
      Include(Result, svoShowFolders);
    if ShellOptions.ShowHidden then
      Include(Result, svoShowHidden);
  end;
end;

procedure TdxShellListView.InvalidateImageList;
begin
  Painter.InvalidateImageList(Images.LargeImages);
end;

function TdxShellListView.IsThumbnailView: Boolean;
begin
  Result := IsWinXPOrLater and (ViewStyle = TdxListViewStyle.Icon) and
    ThumbnailOptions.ShowThumbnails;
end;

procedure TdxShellListView.Navigate(APIDL: PItemIDList);
begin
  if EqualPIDLs(APIDL, ItemProducer.FolderPidl) then
    Exit;
  BeginUpdate;
  try
    DoBeforeNavigation(APIDL);
    ShellRoot.pidl := APIDL;
    DoAfterNavigation;
  finally
    EndUpdate;
  end;
end;

procedure TdxShellListView.RemoveChangeNotification;
begin
  cxShellUnregisterChangeNotifier(FShellChangeNotifierData);
end;

procedure TdxShellListView.SortColumnChanged;
begin
  BeginUpdate;
  try
    Sort;
    UpdateColumnsSortOrder;
  finally
    EndUpdate;
  end;
end;

procedure TdxShellListView.ThumbnailOptionsChanged(Sender: TObject);
begin
  UpdateThumbnails;
  CheckUpdateItems;
end;

procedure TdxShellListView.UpdateThumbnails;
begin
  if IsThumbnailView then
  begin
    FIsThumbnailView := True;
    FFakeThumbnailImages.Width := ThumbnailOptions.Width;
    FFakeThumbnailImages.Height := ThumbnailOptions.Height;
    Images.LargeImages := FFakeThumbnailImages;
  end
  else
  begin
    FIsThumbnailView := False;
    Images.LargeImages := FLargeImages;
  end;
end;

procedure TdxShellListView.UpdateThumbnailSize(AViewId: Integer);

  function GetThumbnailSize(AViewId: Integer): TSize;
  begin
    if AViewId = cmdExtraLargeIconId then
      Result := dxShellExtraLargeIconSize
    else if AViewId = cmdLargeIconId then
      Result := dxShellLargeIconSize
    else
      Result := dxShellMediumIconSize;
    Result.Init(Min(dxShellExtraLargeIconSize.cx, ScaleFactor.Apply(Result.cx)),
      Min(dxShellExtraLargeIconSize.cy, ScaleFactor.Apply(Result.cy)));
  end;

begin
  if ViewStyle <> TdxListViewStyle.Icon then
    Exit;
  ThumbnailOptions.BeginUpdate;
  try
    ThumbnailOptions.Width := GetThumbnailSize(AViewId).cx;
    ThumbnailOptions.Height := GetThumbnailSize(AViewId).cy;
  finally
    ThumbnailOptions.EndUpdate;
  end;
end;

procedure TdxShellListView.DSMNotifyUpdateContents(var Message: TMessage);
begin
  if not IsLoading then
    CheckUpdateItems;
end;

procedure TdxShellListView.DSMNotifyUpdateItem(var Message: TMessage);
begin
  LayoutChanged;
end;

procedure TdxShellListView.DSMSetCount(var Message: TMessage);
begin
  Items.Count := Message.WParam;
  FocusedItem := nil;
end;

function TdxShellListView.GetDependedControls: TcxShellDependedControls;
begin
  Result := DependedControls;
end;

function TdxShellListView.GetRoot: TcxCustomShellRoot;
begin
  Result := FShellRoot;
end;

procedure TdxShellListView.InplaceEditKeyPress(Sender: TObject; var Key: Char);
begin
  if not IsValidShellNameChar(Key) then
    Key := #0;
end;

procedure TdxShellListView.DoUpdateContent(AChangeProc: TProc);
var
  AItemPIDLList: TList;
  I, AItemIndex: Integer;
  APIDL: PItemIDList;
begin
  AItemPIDLList := CreateSelectedPidlsList;
  if FocusedItem <> nil then
    APIDL := GetPidlCopy(GetPidlByItemIndex(FocusedItem.Index))
  else
    APIDL := nil;
  try
    AChangeProc;
  finally
    for I := 0 to AItemPIDLList.Count - 1 do
    begin
      AItemIndex := ItemProducer.GetItemIndexByPidl(AItemPIDLList[I]);
      if AItemIndex >= 0 then
        TdxShellListViewController(Controller).SelectItem(AItemIndex, True);
    end;
    if APIDL <> nil then
    begin
      AItemIndex := ItemProducer.GetItemIndexByPidl(APIDL);
      if AItemIndex > 0 then
        TdxShellListViewController(Controller).FocusedItemIndex := AItemIndex;
      DisposePidl(APIDL);
    end;
    DestroySelectedPidlsList(AItemPIDLList);
  end;
end;

function TdxShellListView.GetAbsolutePIDL: PItemIDList;
begin
  CheckShellRoot(ShellRoot);
  Result := GetPidlCopy(ShellRoot.pidl);
end;

function TdxShellListView.GetFolder(AIndex: Integer): TcxShellFolder;
begin
  Result := GetItemInfo(AIndex).Folder;
end;

function TdxShellListView.GetFolderCount: Integer;
begin
  Result := Items.Count;
end;

function TdxShellListView.GetLargeImageListType: Integer;
begin
  case FLargeIconSize of
    isDefault:
      Result := SHIL_LARGE;
    isExtraLarge:
      Result := SHIL_EXTRALARGE;
  else // isJumbo
    if IsWinVistaOrLater then
      Result := SHIL_JUMBO
    else
      Result := SHIL_EXTRALARGE;
  end;
end;

function TdxShellListView.GetPath: string;
var
  APIDL: PItemIDList;
begin
  APIDL := AbsolutePIDL;
  try
    Result := GetPidlName(APIDL);
  finally
    DisposePidl(APIDL);
  end;
end;

function TdxShellListView.GetSelectedFilePaths: TStrings;
var
  I: Integer;
begin
  FSelectedFiles.Clear;
  for I := 0 to SelectedItemCount - 1 do
    FSelectedFiles.Add(Folders[TdxShellListViewController(Controller)
      .SelectedIndices[I]].PathName);
  Result := FSelectedFiles;
end;

function TdxShellListView.GetShellRoot: TcxShellListRoot;
begin
  Result := FShellRoot;
end;

procedure TdxShellListView.ResetSorting;
begin
  FItemProducer.SortDescending := False;
  FItemProducer.SortColumn := 0;
end;

procedure TdxShellListView.RootFolderChanged(Sender: TObject;
Root: TcxCustomShellRoot);
begin
  ResetSorting;
  CheckUpdateItems;
  if Assigned(FOnRootChanged) then
    FOnRootChanged(Self, Root);
end;

procedure TdxShellListView.RootSettingsChanged(Sender: TObject);
begin
  if not IsLoading then
    DependedControls.SynchronizeRoot(ShellRoot);
end;

procedure TdxShellListView.SaveAbsolutePIDL(AValue: PItemIDList);
var
  APIDL: PItemIDList;
begin
  APIDL := GetPidlCopy(AValue);
  DisposePidl(FAbsolutePIDL);
  FAbsolutePIDL := APIDL;
end;

procedure TdxShellListView.SetAbsolutePIDL(AValue: PItemIDList);
begin
  if CheckAbsolutePIDL(AValue, ShellRoot, True, False) then
    Navigate(AValue);
end;

procedure TdxShellListView.SetLargeIconSize(const AValue: TcxShellIconSize);
begin
  if AValue <> FLargeIconSize then
  begin
    FLargeIconSize := AValue;
    CheckLargeImages;
    if ViewStyle = TdxListViewStyle.Icon then
      LayoutChanged;
  end;
end;

procedure TdxShellListView.SetPath(AValue: string);
var
  APIDL: PItemIDList;
begin
  APIDL := PathToAbsolutePIDL(AValue, ShellRoot, GetViewOptions(True), False);
  if APIDL <> nil then
    try
      AbsolutePIDL := APIDL;
    finally
      DisposePidl(APIDL);
    end;
end;

procedure TdxShellListView.SetShellRoot(const AValue: TcxShellListRoot);
begin
  FShellRoot.Assign(AValue);
end;

procedure TdxShellListView.SetSorting(const AValue: Boolean);
begin
  if FSorting <> AValue then
  begin
    FSorting := AValue;
    SortColumnChanged;
  end;
end;

procedure TdxShellListView.ShellChangeNotify(AEventID: Longint;
APidl1, APidl2: PItemIDList);
begin
  if FNotificationLock then
    Exit;
  FNotificationLock := True;
  try
    DoUpdateContent(CheckUpdateItems);
  finally
    FNotificationLock := False;
  end;
  if Assigned(FOnShellChange) then
    FOnShellChange(Self, AEventID, APidl1, APidl2);
end;

procedure TdxShellListView.UpdateColumnSortOrder(AIndex: Integer);
begin
  if not FSorting or (AIndex <> ItemProducer.SortColumn) then
    Columns[AIndex].SortOrder := soNone
  else if ItemProducer.SortDescending then
    Columns[AIndex].SortOrder := soDescending
  else
    Columns[AIndex].SortOrder := soAscending;
end;

procedure TdxShellListView.UpdateColumnsSortOrder;
var
  I: Integer;
begin
  Columns.BeginUpdate;
  try
    for I := 0 to Columns.Count - 1 do
      UpdateColumnSortOrder(I);
  finally
    Columns.EndUpdate;
  end;
end;

procedure TdxShellListView.DSMSynchronizeRoot(var Message: TMessage);
begin
  if not IsLoading then
    ShellRoot.Update(TcxCustomShellRoot(Message.WParam));
end;

procedure TdxShellListView.DSMSystemShellChangeNotify(var Message: TMessage);

  procedure CheckRenameNotify(AEventID: Longint; APidl1, APidl2: PItemIDList);
  var
    AShellItemInfo: TcxShellItemInfo;
  begin
    if (AEventID and SHCNE_RENAMEFOLDER = SHCNE_RENAMEFOLDER) or
      (AEventID and SHCNE_RENAMEITEM = SHCNE_RENAMEITEM) then
    begin
      AShellItemInfo := ItemProducer.GetItemByPidl(GetLastPidlItem(APidl1));
      if AShellItemInfo <> nil then
        AShellItemInfo.SetNewPidl(ItemProducer.ShellFolder,
          ItemProducer.FolderPidl, GetLastPidlItem(APidl2));
    end;
  end;

var
  AEventID: Integer;
  APidl1, APidl2: PItemIDList;
  APidls: PPidlList;
  ALock: THandle;
  I: Integer;
begin
  ALock := SHChangeNotification_Lock(Message.WParam, Message.LPARAM, APidls,
    AEventID);
  if ALock <> 0 then
    try
      APidl1 := GetPidlCopy(APidls^[0]);
      APidl2 := GetPidlCopy(APidls^[1]);
      try
        CheckRenameNotify(AEventID, APidl1, APidl2);
        ShellChangeNotify(AEventID, APidl1, APidl2);
        if FIsInternalItemCreation and
          ((AEventID = SHCNE_MKDIR) or (AEventID = SHCNE_CREATE)) then
        begin
          I := ItemProducer.GetItemIndexByPidl(GetLastPidlItem(APidl1));
          if I <> -1 then
            StartItemCaptionEditing(I);
        end;
      finally
        DisposePidl(APidl1);
        DisposePidl(APidl2);
      end;
    finally
      SHChangeNotification_UnLock(ALock);
    end;
end;

end.
