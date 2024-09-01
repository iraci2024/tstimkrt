{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressNavBar }
{ }
{ Copyright (c) 2002-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSNAVBAR AND ALL ACCOMPANYING }
{ VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit dxSkinsdxNavBarHamburgerMenuPainter;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Classes, Graphics, SysUtils, Menus, ImgList, Messages,
  Generics.Defaults, Generics.Collections,
  dxCoreGraphics, cxGraphics, cxGeometry, cxAccessibility, dxSkinsCore,
  dxSkinInfo, cxClasses,
  dxFluentDesignFormInterfaces, dxNavBarAccessibility,
  dxNavBar, dxNavBarCollns, dxNavBarBase, dxNavBarCustomPainters,
  dxNavBarOverflowPanel, dxNavBarStyles,
  dxNavBarExplorerViews, dxNavBarOffice11Views, dxNavBarSkinBasedViews,
  dxSkinsdxNavBarAccordionViewPainter;

type
  TdxNavBarHamburgerMenuChildGroupViewInfo = class;
  TdxNavBarHamburgerMenuGroupViewInfo = class;
  TdxNavBarHamburgerMenuPainter = class;
  TdxNavBarHamburgerMenuSkinPainterHelper = class;
  TdxNavBarHamburgerMenuViewInfo = class;

  TdxNavBarHamburgerMenuPopupAnimationType = (patFade, patExpand);
  // for internal use only

  { TdxNavBarHamburgerMenuController }

  TdxNavBarHamburgerMenuController = class(TdxNavBarAccordionViewController)
  strict private
    FIsChangingToMinimalState: Boolean;
    FOverlayControl: TdxNavBarCustomPopupControl;

    function CanShowOverlay: Boolean;
    function GetViewInfo: TdxNavBarHamburgerMenuViewInfo;
    function IsChildGroupActivated: Boolean;
    function IsOverlayControlExists: Boolean;
    function IsOverlayControlVisible: Boolean;
    function IsOverlayMinimalMode: Boolean;
    procedure DoScroll(ADirection: TcxDirection);
    procedure PartChanged;
    procedure ScrollDown;
    procedure ScrollUp;
  strict protected
    function NeedResetPressedPartOnMouseUp: Boolean; override;
    procedure InternalCheckHeight; override;
    procedure InternalSetWidth(const AValue: Integer); override;
  protected
    function AllowResize: Boolean; override;
    function CanHasPopupControl: Boolean; override;
    function CreateGroupPopupControl: TdxNavBarCustomPopupControl; override;
    function GetActualNavBar: TdxCustomNavBar; override;
    function GetCollapsible: Boolean; override;
    function GetPartAtPos(const APoint: TPoint): TdxNavBarPart; override;
    function GetRealCollapsed: Boolean; override;
    function IsNavigationClient: Boolean; override;
    function IsResetGroupMouseTrackingNeeded: Boolean; override;
    function IsResetLinkMouseTrackingNeeded: Boolean; override;
    function NeedClosePopupControlsOnLinkClick: Boolean; override;
    function NeedShowPopupControl: Boolean; override;
    procedure BeginDragging; override;
    procedure ChangeNavBarCollapseState; override;
    procedure CheckMouseUp; override;
    procedure DestroyPopupControls; override;
    procedure DoClick(const APart: TdxNavBarPart); override;
    procedure DoSetHotPart(const APart: TdxNavBarPart); override;
    procedure DoSetPressedPart(const APart: TdxNavBarPart); override;
    procedure EndDragging; override;
    procedure RequestAlign; override;
    procedure SetCollapsed(AValue: Boolean); override;

    function DoScrollDown: Boolean; override;
    function DoScrollUp: Boolean; override;
    function GetScrollButtonsTimerInterval: Integer; override;
    procedure DoScrollDownByTimer; override;
    procedure DoScrollUpByTimer; override;

    procedure InternalCloseOverlay; override;
    procedure InternalShowOverlay; override;
    procedure InternalShowPopupControl(const ADroppedDownPart
      : TdxNavBarPart); override;
  public
    destructor Destroy; override;

    procedure ClosePopupControl;
    procedure ShowPopupControl;

    property Collapsed;
    property ViewInfo: TdxNavBarHamburgerMenuViewInfo read GetViewInfo;
  end;

  { TdxNavBarHamburgerMenuHeaderPanelViewInfo }

  TdxNavBarHamburgerMenuHeaderPanelViewInfo = class
    (TdxNavBarHeaderPanelViewInfo) // for internal use only
  strict private
    FSignClientRect: TRect;
    function GetSignHeight: Integer;
    function GetViewInfo: TdxNavBarHamburgerMenuViewInfo;
  protected
    function CanDrawSign: Boolean; override;
    function CanDrawText: Boolean; override;
    function GetHeight: Integer; override;
    function GetText: string; override;
    procedure CalculateRect(var X, Y: Integer); override;
    procedure CalculateSignRect; override;
    procedure CalculateTextRect; override;
    procedure ClearRects; override;
    procedure DoCalculateBounds(var X, Y: Integer); override;

    property SignClientRect: TRect read FSignClientRect;
    property ViewInfo: TdxNavBarHamburgerMenuViewInfo read GetViewInfo;
  end;

  { TdxNavBarHamburgerMenuItemViewInfo }

  TdxNavBarHamburgerMenuItemViewInfo = class(TdxNavBarAccordionLinkViewInfo)
  strict private
    function GetViewInfo: TdxNavBarHamburgerMenuViewInfo;
  public
    function FontColor: TColor; override;
    function GetDrawEdgeFlag: Integer; override;
    function SelectionRect: TRect; override;

    property ViewInfo: TdxNavBarHamburgerMenuViewInfo read GetViewInfo;
  end;

  { TdxNavBarHamburgerMenuLinkViewInfo }

  TdxNavBarHamburgerMenuLinkViewInfo = class(TdxNavBarHamburgerMenuItemViewInfo)
  strict private
    procedure CalculateAsTopGroupCaption(X, Y: Integer);
  public
    procedure CalculateBounds(X, Y: Integer); override;
  end;

  { TdxNavBarHamburgerMenuGroupViewInfo }

  TdxNavBarHamburgerMenuGroupViewInfo = class(TdxNavBarAccordionGroupViewInfo)
  strict private
    function GetViewInfo: TdxNavBarHamburgerMenuViewInfo;
  protected
    function GetControlRect: TRect; override;
    function GetState: TdxNavBarObjectStates; override;
    function IsCaptionCalculationNeeded: Boolean; override;
    procedure CalculateCaptionImageRect(X, Y: Integer); override;
    procedure DoCalculateCaptionBounds(X, Y: Integer); override;
    procedure OffsetContent; override;

    property ViewInfo: TdxNavBarHamburgerMenuViewInfo read GetViewInfo;
  public
    function AcrylicForegroundColor: TColor; override;
    function CaptionFontColor: TColor; override;
    procedure CalculateBounds(var X, Y: Integer); override;
  end;

  { TdxNavBarHamburgerMenuChildGroupViewInfo }

  TdxNavBarHamburgerMenuChildGroupViewInfo = class
    (TdxNavBarAccordionChildGroupViewInfo)
  strict private
    function GetViewInfo: TdxNavBarHamburgerMenuViewInfo;
  protected
    function GetExpandButtonAreaRight: Integer; override;
    function GetExpandButtonOffset: TSize; override;
    function GetExpandButtonRightIndent: Integer; override;
    function GetImageIndent: Integer; override;
    function GetLevel: Integer; override;
    function GetState: TdxNavBarObjectStates; override;

    property ViewInfo: TdxNavBarHamburgerMenuViewInfo read GetViewInfo;
  public
    constructor Create(AViewInfo: TdxNavBarViewInfo;
      AParentInfo: TdxNavBarGroupViewInfo; AGroup: TdxNavBarGroup;
      ACaptionVisible, AItemsVisible: Boolean); override;
    function AcrylicForegroundColor: TColor; override;
    function BgAlphaBlend: Byte; override;
    function CaptionStyle: TdxNavBarBaseStyle; override;
    procedure CalculateBounds(var X, Y: Integer); override;
    procedure CalculateCaptionBounds(X, Y: Integer); override;
  end;

  { TdxNavBarHamburgerMenuNavPaneCustomItem }

  TdxNavBarHamburgerMenuNavPaneCustomItem = class // for internal use only
  private
    FItem: TdxNavBarCustomItemViewInfo;
    FTextRect: TRect;

    function GetDesignRect: TRect;
    function GetFocusRect: TRect;
    function GetImageIndex: Integer;
    function GetImageList: TCustomImageList;
    function GetState: TdxNavBarObjectStates;
  strict protected
    function GetGroupViewInfo: TdxNavBarGroupViewInfo; virtual;
    function GetImageRect: TRect; virtual;
    function GetRect: TRect; virtual;
    function GetText: string; virtual;
    function GetTextRect: TRect; virtual;
  protected
    function GetNavBarElement: TdxNavBarCustomItem; virtual;
    procedure Offset(const AOffset: TPoint); virtual;
    procedure SetImageRect(const AValue: TRect); virtual;
    procedure SetRect(const AValue: TRect); virtual;
    procedure SetTextRect(const AValue: TRect); virtual;

    property GroupViewInfo: TdxNavBarGroupViewInfo read GetGroupViewInfo;
  public
    constructor Create(AItem: TdxNavBarCustomItemViewInfo);

    procedure DoRightToLeftConversion(const AParentRect: TRect);

    property DesignRect: TRect read GetDesignRect;
    property FocusRect: TRect read GetFocusRect;
    property ImageIndex: Integer read GetImageIndex;
    property ImageList: TCustomImageList read GetImageList;
    property ImageRect: TRect read GetImageRect;
    property Rect: TRect read GetRect;
    property State: TdxNavBarObjectStates read GetState;
    property Text: string read GetText;
    property TextRect: TRect read GetTextRect;
  end;

  { TdxNavBarHamburgerMenuNavPaneLink }

  TdxNavBarHamburgerMenuNavPaneLink = class
    (TdxNavBarHamburgerMenuNavPaneCustomItem)
  strict private
    function GetItem: TdxNavBarItem;
    function GetLink: TdxNavBarItemLink;
    function GetLinkViewInfo: TdxNavBarLinkViewInfo;
  strict protected
    function GetGroupViewInfo: TdxNavBarGroupViewInfo; override;
    function GetNavBarElement: TdxNavBarCustomItem; override;
    function GetText: string; override;
    function GetTextRect: TRect; override;

    property LinkViewInfo: TdxNavBarLinkViewInfo read GetLinkViewInfo;
  public
    property Link: TdxNavBarItemLink read GetLink;
    property Item: TdxNavBarItem read GetItem;
  end;

  { TdxNavBarHamburgerMenuNavPaneGroup }

  TdxNavBarHamburgerMenuNavPaneGroup = class
    (TdxNavBarHamburgerMenuNavPaneCustomItem)
  strict protected
    function GetGroup: TdxNavBarGroup;
  strict protected
    function GetGroupViewInfo: TdxNavBarGroupViewInfo; override;
    function GetNavBarElement: TdxNavBarCustomItem; override;
    function GetText: string; override;
    function GetTextRect: TRect; override;
    procedure SetTextRect(const AValue: TRect); override;
  public
    property Group: TdxNavBarGroup read GetGroup;
  end;

  { TdxNavBarHamburgerMenuNavPaneEmptyGroup }

  TdxNavBarHamburgerMenuNavPaneEmptyGroup = class
    (TdxNavBarHamburgerMenuNavPaneGroup)
  strict protected
    procedure SetImageRect(const AValue: TRect); override;
    procedure SetTextRect(const AValue: TRect); override;
  end;

  { TdxNavBarHamburgerMenuOverflowPanelPopupMenu }

  TdxNavBarHamburgerMenuOverflowPanelPopupMenuClass = class of
    TdxNavBarHamburgerMenuOverflowPanelPopupMenu;

  TdxNavBarHamburgerMenuOverflowPanelPopupMenu = class
    (TdxNavBarCustomOverflowPanelPopupMenu)
  protected
    procedure DoHiddenItemClick(Sender: TObject); override;
  end;

  { TdxNavBarHamburgerMenuOverflowPanelLink }

  TdxNavBarHamburgerMenuOverflowPanelLink = class
    (TdxNavBarOverflowPanelViewInfoItem)
  strict private
    FItemLink: TdxNavBarItemLink;
    procedure SetItemLink(const AValue: TdxNavBarItemLink);
  protected
    function GetCaption: string; override;
    function GetImageIndex: Integer; override;
    function GetPartElement: TObject; override;
    function GetPartIndex: Integer; override;
    procedure SetGroup(const AValue: TdxNavBarGroup); override;
  public
    property ItemLink: TdxNavBarItemLink read FItemLink write SetItemLink;
  end;

  { TdxNavBarHamburgerMenuOverflowPanelGroup }

  TdxNavBarHamburgerMenuOverflowPanelGroup = class
    (TdxNavBarOverflowPanelViewInfoItem)
  protected
    function GetImageIndex: Integer; override;
  end;

  { TdxNavBarHamburgerMenuOverflowPanelViewInfo }

  TdxNavBarHamburgerMenuOverflowPanelViewInfoClass = class of
    TdxNavBarHamburgerMenuOverflowPanelViewInfo;

  TdxNavBarHamburgerMenuOverflowPanelViewInfo = class
    (TdxNavBarCustomOverflowPanelViewInfo)
  private
    FItemWidth: Integer;
    function GetMinHeight: Integer;
    function GetSkinHelper: TdxNavBarSkinBasedPainterHelper;
    function GetViewInfo: TdxNavBarHamburgerMenuViewInfo;
  protected
    function AllowCustomizing: Boolean; override;
    function GetClientOffset: TRect; override;
    function GetHeight: Integer; override;
    function GetImageWidthAddon: Integer; override;
    function GetItemSelectionWidth: Integer; override;
    function GetSignWidth: Integer; override;
    function UseSmallImages: Boolean; override;
    procedure CalculateClientRect(out ARect: TRect); override;
    procedure CalculateRect(X, Y: Integer); override;
    procedure CalculateSignRect(const ARect: TRect); override;
    procedure ClearRects; override;

    procedure DoItemLinkClick; virtual;

    function AddGroup(AGroup: TdxNavBarGroup)
      : TdxNavBarHamburgerMenuOverflowPanelGroup;
    function AddItemLink(AItemLink: TdxNavBarItemLink)
      : TdxNavBarHamburgerMenuOverflowPanelLink;
    function IsSkinAvailable: Boolean;
    procedure Calculate(const ABounds: TRect); overload;

    property ViewInfo: TdxNavBarHamburgerMenuViewInfo read GetViewInfo;
  public
    function TryDoClick(const APart: TdxNavBarPart): Boolean; override;
  end;

  { TdxNavBarHamburgerMenuNavPaneViewInfo }

  TdxNavBarHamburgerMenuNavPaneItemList =
    TList<TdxNavBarHamburgerMenuNavPaneCustomItem>; // for internal use only

  TdxNavBarHamburgerMenuNavPaneViewInfo = class(TdxNavBarPartViewInfo)
  // for internal use only
  strict private
    FCompactItemPattern: TdxNavBarGroupViewInfo;
    FGroups: TObjectDictionary<TdxNavBarGroup, TdxNavBarGroupViewInfo>;
    FHiddenItems: TdxNavBarHamburgerMenuNavPaneItemList;
    FIAccessibilityHelper: IdxNavBarAccessibilityHelper;
    FItemIAccessibilityHelpers: TInterfaceList;
    FItemPattern: TdxNavBarGroupViewInfo;
    FItems: TObjectList<TdxNavBarHamburgerMenuNavPaneCustomItem>;
    FOverflowPanelViewInfo: TdxNavBarHamburgerMenuOverflowPanelViewInfo;
    FPopupMenu: TdxNavBarCustomOverflowPanelPopupMenu;
    FSignIAccessibilityHelper: IdxNavBarAccessibilityHelper;
    FVisibleItems: TdxNavBarHamburgerMenuNavPaneItemList;

    function GetGroupViewInfoAtItemsPos(const P: TPoint)
      : TdxNavBarGroupViewInfo;

    function GetIAccessibilityHelper: IdxNavBarAccessibilityHelper;
    function GetActualItemPattern: TdxNavBarGroupViewInfo;
    function GetAllowCustomizing: Boolean;
    function GetAllowHideItems: Boolean;
    function GetItemIAccessibilityHelper(AIndex: Integer)
      : IdxNavBarAccessibilityHelper;
    function GetItemCount: Integer;
    function GetItemDrawEdgeFlag: Cardinal;
    function GetItemFont: TFont;
    function GetItemTextColor: TColor;
    function GetPainter: TdxNavBarHamburgerMenuPainter;
    function GetPopupMenuImageIndent: Integer;
    function GetSignIAccessibilityHelper: IdxNavBarAccessibilityHelper;
    function GetSignRect: TRect;
    function GetViewInfo: TdxNavBarHamburgerMenuViewInfo;
    function GetVisibleItemCount: Integer;
    procedure SetItemIAccessibilityHelperCount(Value: Integer);

    function GetItemsRect: TRect;

    procedure CalculateItemsPattern;
    procedure ClearVisualElements;

    procedure OnOverflowPanelSignButtonClickHandler
      (const AClientMousePos: TPoint);
  protected
    FClientRect: TRect;
    FOverflowPanelRect: TRect;
    FRect: TRect;

    function GetGroupViewInfoAtPos(const P: TPoint): TdxNavBarGroupViewInfo;
    function GetGroupViewInfoByGroup(AGroup: TdxNavBarGroup)
      : TdxNavBarGroupViewInfo;
    function GetLinkViewInfoAtPos(const P: TPoint): TdxNavBarLinkViewInfo;
    function GetLinkViewInfoByLink(ALink: TdxNavBarItemLink)
      : TdxNavBarLinkViewInfo;
    function GetItemAccessibilityHelperClass
      : TdxNavBarItemCollectionItemAccessibilityHelperClass;
    function GetItemIAccessibilityHelperCount: Integer;
    function GetItemImageSize: TSize;
    function GetPartAtPos(const APoint: TPoint): TdxNavBarPart;

    procedure Calculate(const ABounds: TRect);
    procedure CalculateSignRect(const ABounds: TRect);
    procedure Clear;
    procedure CreateGroupInfos;
    procedure DoRightToLeftConversion;
    procedure HideGroup(AGroup: TdxNavBarGroup);
    procedure HideLink(AItemLink: TdxNavBarItemLink);
    procedure RecreateGroupInfos;
    procedure ShowPopupMenu(const APosition: TPoint);

    function Contains(AGroup: TdxNavBarGroup): Boolean;
    function IsEmpty: Boolean;
    function TryDoClick: Boolean;
    function UseCompactLayout: Boolean;

    property Items: TObjectList<TdxNavBarHamburgerMenuNavPaneCustomItem>
      read FItems;
    property HiddenItems: TdxNavBarHamburgerMenuNavPaneItemList
      read FHiddenItems;
    property VisibleItems: TdxNavBarHamburgerMenuNavPaneItemList
      read FVisibleItems;

    property IAccessibilityHelper: IdxNavBarAccessibilityHelper
      read GetIAccessibilityHelper;
    property ItemIAccessibilityHelpers[AIndex: Integer]
      : IdxNavBarAccessibilityHelper read GetItemIAccessibilityHelper;

    property ActualItemPattern: TdxNavBarGroupViewInfo
      read GetActualItemPattern;
    property CompactItemPattern: TdxNavBarGroupViewInfo
      read FCompactItemPattern;
    property ItemPattern: TdxNavBarGroupViewInfo read FItemPattern;
    property ItemDrawEdgeFlag: Cardinal read GetItemDrawEdgeFlag;
    property ItemFont: TFont read GetItemFont;
    property ItemTextColor: TColor read GetItemTextColor;
    property PopupMenu: TdxNavBarCustomOverflowPanelPopupMenu read FPopupMenu;
    property PopupMenuImageIndent: Integer read GetPopupMenuImageIndent;
    property OverflowPanelViewInfo: TdxNavBarHamburgerMenuOverflowPanelViewInfo
      read FOverflowPanelViewInfo;
    property AllowCustomizing: Boolean read GetAllowCustomizing;
    property AllowHideItems: Boolean read GetAllowHideItems;
    property ViewInfo: TdxNavBarHamburgerMenuViewInfo read GetViewInfo;
  public
    constructor Create(AViewInfo: TdxNavBarViewInfo); override;
    destructor Destroy; override;

    procedure CheckItemIAccessibilityHelperCount;

    property ItemCount: Integer read GetItemCount;
    property ItemIAccessibilityHelperCount: Integer
      read GetItemIAccessibilityHelperCount;
    property ItemsRect: TRect read GetItemsRect;
    property Painter: TdxNavBarHamburgerMenuPainter read GetPainter;
    property Rect: TRect read FRect;
    property SignIAccessibilityHelper: IdxNavBarAccessibilityHelper
      read GetSignIAccessibilityHelper;
    property SignRect: TRect read GetSignRect;
    property VisibleItemCount: Integer read GetVisibleItemCount;
  end;

  { TdxNavBarHamburgerMenuCollapsedStateCalculator }

  TdxNavBarHamburgerMenuCollapsedStateCalculator = class
    (TdxNavBarCollapsedViewCalculator)
  strict private
    function GetViewInfo: TdxNavBarHamburgerMenuViewInfo;
  strict protected
    function CreateGroupViewInfo(AGroup: TdxNavBarGroup)
      : TdxNavBarGroupViewInfo; override;
    function GetMaxElementSize: Integer; override;
    procedure CalculateGroupMaxImageSize(AGroupViewInfo
      : TdxNavBarGroupViewInfo); override;
    procedure CalculatePanelsMaxImageSize; override;
  end;

  { TdxNavBarHamburgerMenuViewInfo }

  TdxNavBarHamburgerMenuViewInfo = class(TdxNavBarAccordionViewInfo)
  strict private
    FActualSelectedPopupGroup: TdxNavBarGroup;
    FCollapsedStateCalculator: TdxNavBarHamburgerMenuCollapsedStateCalculator;
    FContentOffset: Integer;
    FIsPopupControlCalculation: Boolean;
    FNavPaneViewInfo: TdxNavBarHamburgerMenuNavPaneViewInfo;

    function GetActualScrollingAreaHeight: Integer;
    function GetController: TdxNavBarHamburgerMenuController;
    function GetNavPaneRect: TRect;
    function GetPainter: TdxNavBarHamburgerMenuPainter;
    function GetSelectedPopupGroup: TdxNavBarGroup;
    function GetSkinHelper: TdxNavBarHamburgerMenuSkinPainterHelper;

    function GetNavPaneIAccessibilityHelper: IdxNavBarAccessibilityHelper;
    function GetNavPaneSignIAccessibilityHelper: IdxNavBarAccessibilityHelper;

    function IsDefaultHeaderColor: Boolean;
    function IsDefaultOverflowPanelColor: Boolean;
    function IsDefaultBottomScrollButtonColor: Boolean;
    function IsDefaultTopScrollButtonColor: Boolean;
    function IsScrollButtonStateDisabled(ATopButton: Boolean): Boolean;
    function IsScrollButtonVisible(const AButtonRect: TRect): Boolean;
  protected
    function CanGroupCaptionBoundsByImage: Boolean; override;
    function CanHasVisibleItemsInGroup(AGroup: TdxNavBarGroup)
      : Boolean; override;
    function GetDefaultSmallImageWidth: Integer; override;
    function GetGroupItemsVisible(AGroup: TdxNavBarGroup): Boolean; override;
    function IsPtHeaderSign(const pt: TPoint): Boolean; override;
    function IsTopAlignment(AGroup: TdxNavBarGroup): Boolean; override;
    function NeedCalculateScrollBarVisible: Boolean; override;
    procedure DoGroupActiveToggle(AGroup: TdxNavBarGroup); override;

    function AllowControlExpandAnimation: Boolean; override;
    function CanHasActiveGroup: Boolean; override;
    function CanHasHeader: Boolean; override;

    function GetHeaderSignAccessibilityHelperClass
      : TdxNavBarCustomAccessibilityHelperClass; override;
    // for internal use only
    function GetHeaderSignIndents: TRect; override;
    function GetChildItemOffset: Integer; override;
    function GetItemCaptionOffsets: TRect; override;
    function GetLinksImageEdges: TRect; override;
    function GetPartIAccessibilityHelper(const APart: TdxNavBarPart)
      : IdxNavBarAccessibilityHelper; override;
    function GetOverflowPanelHeight: Integer; override;

    function GetLargeImageHeight(AItemViewInfo: TdxNavBarCustomItemViewInfo)
      : Integer; override;
    function GetLargeImageWidth(AItemViewInfo: TdxNavBarCustomItemViewInfo)
      : Integer; override;
    function GetSmallImageHeight(AItemViewInfo: TdxNavBarCustomItemViewInfo)
      : Integer; override;
    function GetSmallImageWidth(AItemViewInfo: TdxNavBarCustomItemViewInfo)
      : Integer; override;

    function GetNavBarCollapsedWidth: Integer; override;
    function GetNavBarMinExpandedWidth: Integer; override;
    function GetPopupTopPos: Integer; override;
    function IsCheckBoundsNeeded: Boolean; override;
    function IsHeaderVisible: Boolean; override;
    procedure CheckControlWindowRegion(AGroup: TdxNavBarGroupViewInfo);
      override;

    function GetGroupAccessibilityHelperClass
      : TdxNavBarCustomAccessibilityHelperClass; override;
    function GetGroupCaptionPanelAccessibilityHelperClass
      : TdxNavBarCustomAccessibilityHelperClass; override;
    function IsNavPaneActiveGroup(AGroupViewInfo: TdxNavBarGroupViewInfo)
      : Boolean; overload; // for internal use only
    function IsNavPaneActiveGroup(AGroup: TdxNavBarGroup): Boolean; overload;
    // for internal use only
    function IsRootGroup(AGroupViewInfo: TdxNavBarGroupViewInfo): Boolean;
      overload; // for internal use only
    function IsRootGroup(AGroup: TdxNavBarGroup): Boolean; overload;
    // for internal use only
    function IsSelectedPopupGroup(AGroupViewInfo: TdxNavBarGroupViewInfo)
      : Boolean; // for internal use only
    function IsVerticalItem(AItemViewInfo: TdxNavBarCustomItemViewInfo)
      : Boolean; // for internal use only

    procedure CalculateGroupVerticalCaption(AGroupViewInfo
      : TdxNavBarGroupViewInfo); // for internal use only
    procedure CalculateVerticalCaption(ACaption: string; AFont: TFont;
      var ACaptionRect, ACaptionTextRect: TRect);
    procedure CalculateVerticalCaptionDesignRect
      (AItem: TdxNavBarCustomItemViewInfo);
    procedure CheckSelectedPopupGroupState(AGroupViewInfo
      : TdxNavBarGroupViewInfo; var AStates: TdxNavBarObjectStates);

    procedure CreateColors; override;
    procedure RefreshColors; override;
    procedure ReleaseColors; override;

    function GetGroupEdges: TPoint; override;
    function GetGroupCaptionHeightAddon: Integer; override;
    function GetGroupCaptionImageIndent: Integer; override;
    function GetGroupCaptionImageOffsets: TRect; override;
    function GetGroupCaptionSeparatorWidth: Integer; override;
    function GetGroupCaptionTextIndent: Integer; override;
    function GetGroupSeparatorWidth: Integer; override;

    function CreateGroupViewInfo(AGroup: TdxNavBarGroup;
      ACaptionVisible, AItemsVisible: Boolean): TdxNavBarGroupViewInfo;
    // for internal use only
    function GetImageRectWidth(AItemViewInfo: TdxNavBarCustomItemViewInfo)
      : Integer; // for internal use only
    procedure BeginActiveGroupPopupControl;
    procedure EndActiveGroupPopupControl;

    function GetGroupsArea: TRect; override;
    function IsNavPaneVisible: Boolean; // for internal use only
    procedure DoCreateGroupsInfo; override;

    procedure AlignItems; virtual;
    procedure CalculateGroups(X, Y: Integer); override;
    procedure DoCalculateBounds(X, Y: Integer); override;
    procedure CorrectBounds; override;
    procedure CorrectPanelsBounds; // for internal use only
    procedure InternalCalculateMaxImageSize; override;

    function GetScrollingAreaBounds: TRect; // for internal use only
    procedure ResetScrollOffset; // for internal use only

    function CanHasScrollButtons: Boolean; override;
    function GetScrollButtonHorizontalEdge: Integer; override;
    function GetScrollButtonsBounds: TRect; override;
    function IsBottomScrollButtonDisabled: Boolean; override;
    function IsTopScrollButtonStateDisabled: Boolean; override;

    function CanShowGroupItems(AGroup: TdxNavBarGroup): Boolean;
    // for internal use only
    function ChildGroupContentOffset: TRect; // for internal use only
    function GetGroupControlClipRect: TRect; // for internal use only
    function GetMasterPopupGroup: TdxNavBarGroup; // for internal use only
    function GetMaxImageSize: TSize; // for internal use only
    function GetTextIndent: Integer; // for internal use only
    function IsCollapsed: Boolean; // for internal use only
    function IsGroupShowing: Boolean; // for internal use only
    function IsSkinAvailable: Boolean; // for internal use only
    function ItemContentOffset: TRect; // for internal use only
    procedure UpdateEffectColors; override;
    procedure UpdateScrollOffset(ADelta: Integer); // for internal use only

    property ActualSelectedPopupGroup: TdxNavBarGroup
      read FActualSelectedPopupGroup; // for internal use only
    property IsPopupControlCalculation: Boolean read FIsPopupControlCalculation;
    // for internal use only
    property NavPane: TdxNavBarHamburgerMenuNavPaneViewInfo
      read FNavPaneViewInfo; // for internal use only
    property NavPaneIAccessibilityHelper: IdxNavBarAccessibilityHelper
      read GetNavPaneIAccessibilityHelper; // for internal use only
    property NavPaneItemIAccessibilityHelper: IdxNavBarAccessibilityHelper
      read GetNavPaneIAccessibilityHelper; // for internal use only
    property NavPaneSignIAccessibilityHelper: IdxNavBarAccessibilityHelper
      read GetNavPaneSignIAccessibilityHelper; // for internal use only
    property SkinHelper: TdxNavBarHamburgerMenuSkinPainterHelper
      read GetSkinHelper; // for internal use only
  public
    constructor Create(APainter: TdxNavBarPainter); override;
    destructor Destroy; override;

    function GetLinkViewInfoAtPos(const P: TPoint)
      : TdxNavBarLinkViewInfo; override;
    function GetLinkViewInfoAtSelectedPos(const P: TPoint)
      : TdxNavBarLinkViewInfo; override;
    function GetLinkViewInfoByLink(ALink: TdxNavBarItemLink)
      : TdxNavBarLinkViewInfo; override;
    function GetGroupViewInfoAtPos(const P: TPoint)
      : TdxNavBarGroupViewInfo; override;
    function GetGroupViewInfoByGroup(AGroup: TdxNavBarGroup)
      : TdxNavBarGroupViewInfo; override;
    function GetGripSize: Integer; override;
    function IsBottomScrollButtonVisible: Boolean; override;
    function IsGroupActive(AGroup: TdxNavBarGroup): Boolean; override;
    function IsTopScrollButtonVisible: Boolean; override;
    procedure DoRightToLeftConversion; override;

    procedure AssignDefaultBackgroundStyle; override;
    procedure AssignDefaultGroupHeaderStyle; override;
    procedure AssignDefaultGroupHeaderActiveStyle; override;
    procedure AssignDefaultChildGroupCaptionHotTrackedStyle; override;
    procedure AssignDefaultChildGroupCaptionPressedStyle; override;
    procedure AssignDefaultChildGroupCaptionStyle; override;
    procedure AssignDefaultItemHotTrackedStyle; override;
    procedure AssignDefaultItemStyle; override;
    procedure AssignDefaultNavigationPaneHeaderStyle; override;

    function HeaderBackColor: TColor; override;
    function HeaderBackColor2: TColor; override;
    function HeaderFont: TFont; override;
    function HeaderFontColor: TColor; override;
    function OverflowPanelGradientMode: TdxBarStyleGradientMode; override;

    function BorderColor: TColor; override;
    function BorderWidth: Integer; override;

    function BottomScrollButtonBackColor: TColor; override;
    function BottomScrollButtonBackColor2: TColor; override;
    function BottomScrollButtonAlphaBlend: Byte; override;
    function BottomScrollButtonAlphaBlend2: Byte; override;
    function BottomScrollButtonGradientMode: TdxBarStyleGradientMode; override;
    function TopScrollButtonBackColor: TColor; override;
    function TopScrollButtonBackColor2: TColor; override;
    function TopScrollButtonAlphaBlend: Byte; override;
    function TopScrollButtonAlphaBlend2: Byte; override;
    function TopScrollButtonGradientMode: TdxBarStyleGradientMode; override;

    property HeaderRect;
    property HeaderSignRect;
    property NavPaneRect: TRect read GetNavPaneRect;
    property Painter: TdxNavBarHamburgerMenuPainter read GetPainter;
  end;

  { TdxNavBarHamburgerMenuPainter }

  TdxNavBarHamburgerMenuPainter = class(TdxNavBarAccordionViewPainter)
  strict private
    FNavPaneSignPainter: TdxNavBarOverflowPanelPainter;

    function DoDrawScrollButton(ADownButton: Boolean; const ARect: TRect;
      AState: TdxNavBarObjectStates): Boolean;
    function GetController: TdxNavBarHamburgerMenuController;
    function GetSkinHelper: TdxNavBarHamburgerMenuSkinPainterHelper;
    function GetViewInfo: TdxNavBarHamburgerMenuViewInfo;

    function IsBackgroundImagePainting: Boolean;
    function InternalDrawNavBarBackground(AElement: TdxSkinElement;
      const ARect: TRect; AUseSolidColor: Boolean): Boolean;
    function InternalDrawPanelBackground(AElement: TdxSkinElement;
      const ARect: TRect; AUseSolidColor: Boolean): Boolean;
    procedure InternalDrawBottomScrollButton;
    procedure InternalDrawGroupBackground(AGroupViewInfo
      : TdxNavBarGroupViewInfo; const ARect: TRect);
    procedure InternalDrawTopScrollButton;
    procedure DrawScrollButton(AProc: TProc);
  protected
    class function GetChildGroupViewInfoClass
      : TdxNavBarChildGroupViewInfoClass; override;
    class function GetDropInfoCalculatorClass
      : TdxNavBarDropInfoCalculatorClass; override;
    class function GetGroupViewInfoClass: TdxNavBarGroupViewInfoClass; override;
    class function GetHeaderPanelViewInfoClass
      : TdxNavBarHeaderPanelViewInfoClass; override;
    class function GetLinkViewInfoClass: TdxNavBarLinkViewInfoClass; override;
    class function GetSkinPainterHelperClass
      : TdxNavBarSkinBasedPainterHelperClass; override;
    class function GetViewInfoClass: TdxNavBarViewInfoClass; override;
    class function SelectionPainterClass
      : TdxNavBarCustomSelectionPainterClass; override;
    class function SignPainterClass: TdxNavBarCustomSignPainterClass; override;
    class function ScrollButtonsPainterClass
      : TdxNavBarCustomScrollButtonsPainterClass; override;
    function CreateLinkViewInfo(AViewInfo: TdxNavBarGroupViewInfo;
      ALink: TdxNavBarItemLink; ACaptionVisible, AImageVisible: Boolean)
      : TdxNavBarLinkViewInfo; override;
    function CreateGroupViewInfo(AViewInfo: TdxNavBarViewInfo;
      AGroup: TdxNavBarGroup; ACaptionVisible, AItemsVisible: Boolean)
      : TdxNavBarGroupViewInfo; override;
    function GetControllerClass: TdxNavBarControllerClass; override;
    function GetDefaultColorSchemeName: TdxSkinName; override;
    function UseHeaderCustomDrawing: Boolean; override;

    class function GetOverflowPanelPopupMenuClass
      : TdxNavBarHamburgerMenuOverflowPanelPopupMenuClass; virtual;
    class function GetOverflowPanelViewInfoClass
      : TdxNavBarHamburgerMenuOverflowPanelViewInfoClass; virtual;
    procedure DrawNavPane; virtual;
    procedure DrawNavPaneItem
      (AItem: TdxNavBarHamburgerMenuNavPaneCustomItem); virtual;
    procedure DrawNavPaneItemBackground
      (AItem: TdxNavBarHamburgerMenuNavPaneCustomItem); virtual;
    procedure DrawNavPaneItemFocusRect
      (AItem: TdxNavBarHamburgerMenuNavPaneCustomItem); virtual;
    procedure DrawNavPaneItemImage
      (AItem: TdxNavBarHamburgerMenuNavPaneCustomItem); virtual;
    procedure DrawNavPaneItemText
      (AItem: TdxNavBarHamburgerMenuNavPaneCustomItem); virtual;

    function GetGroupColorPalette(AGroupViewInfo: TdxNavBarGroupViewInfo)
      : IdxColorPalette;
    function GetHeaderSignColorPalette(AState: TdxSkinElementState)
      : IdxColorPalette;
    function GetItemColorPalette(ALinkViewInfo: TdxNavBarLinkViewInfo)
      : IdxColorPalette; override;
    function GetNavPaneItemColorPalette: IdxColorPalette;

    procedure DrawDefaultGlyph(ACanvas: TcxCanvas; const ARect: TRect);
    // for internal use only
    procedure DrawElementHighlight(const R: TRect); override;
    procedure DrawItemHighlightEffects(ALinkViewInfo
      : TdxNavBarLinkViewInfo); override;
    procedure DrawPopupControl(ACanvas: TcxCanvas;
      AViewInfo: TdxNavBarCustomPopupControlViewInfo); // for internal use only
    procedure OnDrawOverflowPanelPopupMenuItemHandler(ACanvas: TCanvas;
      ARect: TRect; AImageList: TCustomImageList; AImageIndex: Integer;
      AText: string; AState: TdxNavBarObjectStates); // for internal use only

    procedure DrawNavPaneBackground; // for internal use only
    procedure DrawNavPaneItems; // for internal use only
    procedure DrawNavPaneSign; // for internal use only

    function AllowUsualItemSelection(ALinkViewInfo: TdxNavBarLinkViewInfo)
      : Boolean; override;
    function GetAcrylicForegroundColor: TColor; override;
    function GetRootGroupAcrylicForegroundColor(AGroupViewInfo
      : TdxNavBarGroupViewInfo): TColor;
    property SkinHelper: TdxNavBarHamburgerMenuSkinPainterHelper
      read GetSkinHelper; // for internal use only
  public
    constructor Create(ANavBar: TdxCustomNavBar); override;
    destructor Destroy; override;

    procedure DrawBackground; override;
    procedure DrawHeaderBackground; override;
    procedure DrawHeaderSign; override;
    procedure DrawNavBarControl; override;

    procedure DrawChildGroupCaption(AChildGroupViewInfo
      : TdxNavBarChildGroupViewInfo); override;
    procedure DrawChildGroupSelection(AChildGroupViewInfo
      : TdxNavBarChildGroupViewInfo); override;
    procedure DrawGroupBackground(AGroupViewInfo
      : TdxNavBarGroupViewInfo); override;
    procedure DrawGroupCaptionButton(AGroupViewInfo
      : TdxNavBarGroupViewInfo); override;
    procedure DrawGroupCaptionImage(AGroupViewInfo
      : TdxNavBarGroupViewInfo); override;
    procedure DrawGroupCaptionText(AGroupViewInfo
      : TdxNavBarGroupViewInfo); override;
    procedure DrawItemCaption(ALinkViewInfo: TdxNavBarLinkViewInfo); override;
    procedure DrawItemImage(ALinkViewInfo: TdxNavBarLinkViewInfo); override;

    procedure DrawBottomScrollButton; override;
    procedure DrawTopScrollButton; override;

    property Controller: TdxNavBarHamburgerMenuController read GetController;
    property ViewInfo: TdxNavBarHamburgerMenuViewInfo read GetViewInfo;
  end;

  { TdxNavBarHamburgerMenuSkinPainterHelper }

  TdxNavBarHamburgerMenuSkinPainterHelper = class
    (TdxNavBarAccordionViewSkinPainterHelper)
  strict private
    function IsDefaultColorSchemeName: Boolean;
    function UseBuiltInSkin: Boolean;
  protected
    function GetFullSkinName: TdxSkinName; override;
    function GetSkinName: TdxSkinName; override;
    function GetSkinPainterData(var AData: TdxSkinInfo): Boolean; override;

    function HamburgerButton: TdxSkinElement;
    function HamburgerMenuNavPaneBackground: TdxSkinElement;
    function HamburgerScrollDownButton: TdxSkinElement;
    function HamburgerScrollUpButton: TdxSkinElement;
  public
    function NavBarBackground: TdxSkinElement; override;
    function NavBarDistanceBetweenRootGroups: Integer; override;
    function NavBarChildGroup: TdxSkinElement; override;
    function NavBarChildGroupExpandButton(AIsClose: Boolean)
      : TdxSkinElement; override;
    function NavBarChildItemOffset: Integer; override;
    function NavBarGroupButtonCaption: TdxSkinElement; override;
    function NavBarGroupSigns(AIsClose: Boolean): TdxSkinElement; override;
    function NavBarItem: TdxSkinElement; override;
    function NavPaneItem(ASelected: Boolean): TdxSkinElement; override;
    function NavPaneOverflowPanelSign: TdxSkinElement; override;
    function NavPaneOverflowPanelItem: TdxSkinElement; override;
  end;

  { TdxNavBarHamburgerMenuNavPaneIAccessibilityHelper }

  TdxNavBarHamburgerMenuNavPaneIAccessibilityHelper = class
    (TdxNavBarItemCollectionAccessibilityHelper) // for internal use
  private
    function GetNavPaneViewInfo: TdxNavBarHamburgerMenuNavPaneViewInfo;
  protected
    function ChildIsSimpleElement(AIndex: Integer): Boolean; override;
    function GetChild(AIndex: Integer): TcxAccessibilityHelper; override;
    function GetChildCount: Integer; override;
    function GetParent: TcxAccessibilityHelper; override;
    function GetRole(AChildID: TcxAccessibleSimpleChildElementID)
      : Integer; override;
    function GetState(AChildID: TcxAccessibleSimpleChildElementID)
      : Integer; override;
    function GetSupportedProperties(AChildID: TcxAccessibleSimpleChildElementID)
      : TcxAccessibleObjectProperties; override;

    procedure CheckItemIAccessibilityHelperCount; override;
    function GetBounds: TRect; override;
    function GetItemCount: Integer; override;
    function GetItemIAccessibilityHelperCount: Integer; override;
    function GetItemIAccessibilityHelper(AIndex: Integer)
      : IdxNavBarAccessibilityHelper; override;

    property NavPaneViewInfo: TdxNavBarHamburgerMenuNavPaneViewInfo
      read GetNavPaneViewInfo;
  end;

  { TdxNavBarHamburgerMenuNavPaneItemAccessibilityHelper }

  TdxNavBarHamburgerMenuNavPaneItemAccessibilityHelper = class
    (TdxNavBarItemCollectionItemAccessibilityHelper) // for internal use
  private
    function GetGroup: TdxNavBarGroup;
    function GetViewInfo: TdxNavBarHamburgerMenuViewInfo;
  protected
    function ChildIsSimpleElement(AIndex: Integer): Boolean; override;
    procedure DoDefaultAction(AChildID
      : TcxAccessibleSimpleChildElementID); override;
    function GetName(AChildID: TcxAccessibleSimpleChildElementID)
      : string; override;
    function GetParent: TcxAccessibilityHelper; override;
    function GetRole(AChildID: TcxAccessibleSimpleChildElementID)
      : Integer; override;
    function GetState(AChildID: TcxAccessibleSimpleChildElementID)
      : Integer; override;
    function GetSupportedProperties(AChildID: TcxAccessibleSimpleChildElementID)
      : TcxAccessibleObjectProperties; override;

    function GetBounds: TRect; override;
    function IsClickKey(AKey: Word): Boolean; override;
    function IsContainer: Boolean; override;
    procedure Click(AKey: Word); override;

    property Group: TdxNavBarGroup read GetGroup;
    property ViewInfo: TdxNavBarHamburgerMenuViewInfo read GetViewInfo;
  end;

  { TdxNavBarHamburgerMenuNavPaneSignAccessibilityHelper }

  TdxNavBarHamburgerMenuNavPaneSignAccessibilityHelper = class
    (TdxNavBarCustomAccessibilityHelper) // for internal use
  private
    function GetNavPaneViewInfo: TdxNavBarHamburgerMenuNavPaneViewInfo;
  protected
    function ChildIsSimpleElement(AIndex: Integer): Boolean; override;
    procedure DoDefaultAction(AChildID
      : TcxAccessibleSimpleChildElementID); override;
    function GetName(AChildID: TcxAccessibleSimpleChildElementID)
      : string; override;
    function GetParent: TcxAccessibilityHelper; override;
    function GetRole(AChildID: TcxAccessibleSimpleChildElementID)
      : Integer; override;
    function GetState(AChildID: TcxAccessibleSimpleChildElementID)
      : Integer; override;
    function GetSupportedProperties(AChildID: TcxAccessibleSimpleChildElementID)
      : TcxAccessibleObjectProperties; override;

    function GetBounds: TRect; override;
    function IsClickKey(AKey: Word): Boolean; override;
    function IsContainer: Boolean; override;
    procedure Click(AKey: Word); override;

    property NavPaneViewInfo: TdxNavBarHamburgerMenuNavPaneViewInfo
      read GetNavPaneViewInfo;
  end;

  { TdxNavBarHamburgerMenuGroupAccessibilityHelper }

  TdxNavBarHamburgerMenuGroupAccessibilityHelper = class
    (TdxNavBarGroupAccessibilityHelper) // for internal use
  protected
    function GetState(AChildID: TcxAccessibleSimpleChildElementID)
      : Integer; override;
  end;

  { TdxNavBarHamburgerMenuGroupCaptionPanelAccessibilityHelper }

  TdxNavBarHamburgerMenuGroupCaptionPanelAccessibilityHelper = class
    (TdxNavBarGroupCaptionPanelAccessibilityHelper) // for internal use
  protected
    function GetState(AChildID: TcxAccessibleSimpleChildElementID)
      : Integer; override;
  end;

  { TdxNavBarHamburgerMenuSignAccessibilityHelper }

  TdxNavBarHamburgerMenuSignAccessibilityHelper = class
    (TdxNavBarHeaderPanelSignAccessibilityHelper) // for internal use
  protected
    function GetName(AChildID: TcxAccessibleSimpleChildElementID)
      : string; override;
  end;

implementation

{$R dxNavBarHamburgerMenu.res}

uses
  Types, Math, Controls,
  dxCore, dxCoreClasses, dxTypeHelpers, dxAnimation, dxGDIPlusClasses,
  dxOffice11, dxDPIAwareUtils, cxContainer,
  cxLookAndFeelPainters, cxControls, dxAcrylicEffect, dxCustomFluentDesignForm,
  dxNavBarViewsFact, dxNavBarConsts, dxNavBarBaseViews, dxNavBarGroupItems,
  dxNavBarGraphics;

const
  dxNavBarHamburgerMenuAutoScrollInterval = 50;
  dxNavBarHamburgerMenuItemVerticalCaptionIndent = 5;
  dxNavBarHamburgerMenuOverflowPanelItemLink = 100;
  dxNavBarHamburgerMenuScrollDelta = 10;
  sdxDefaultColorSchemeName = 'HamburgerMenu';

type
  TdxHamburgerMenuCustomPopupControl = class;
  TdxHamburgerMenuOverlayControl = class;

  TdxNavBarAccess = class(TdxCustomNavBar);
  TdxNavBarControllerAccess = class(TdxNavBarController);
  TdxNavBarGroupViewInfoAccess = class(TdxNavBarGroupViewInfo);
  TdxNavBarHamburgerMenuBehaviorOptionsAccess = class
    (TdxNavBarHamburgerMenuBehaviorOptions);
  TdxNavBarItemViewInfoAccess = class(TdxNavBarCustomItemViewInfo);
  TdxNavBarOverflowPanelPopupMenuAccess = class
    (TdxNavBarCustomOverflowPanelPopupMenu);
  TdxNavBarPopupControlAccess = class(TdxNavBarCustomPopupControl);
  TdxNavBarViewInfoAccess = class(TdxNavBarViewInfo);

  { TdxNavBarGroupViewInfoHelper }

  TdxNavBarGroupViewInfoHelper = class helper for TdxNavBarGroupViewInfo
  public
    function GetHasScrollBar: Boolean;
    function TopLevel: Boolean;
  end;

  { TdxHamburgerMenuDropInfoCalculator }

  TdxHamburgerMenuDropInfoCalculator = class(TdxNavBarDropInfoCalculator)
  strict private
    function GetViewInfo: TdxNavBarHamburgerMenuViewInfo;
  protected
    function IsTargetGroupVerticalLayout(const ATargetPoint: TPoint)
      : Boolean; override;
    function IsTargetVerticalLayout(AGroup: TdxNavBarGroup): Boolean; override;
    procedure CheckDropInfo(var ADropTargetInfo
      : TdxNavBarDropTargetInfo); override;
  end;

  { TdxHamburgerMenuGroupLayout }

  TdxHamburgerMenuGroupLayout = record
  strict private
    HasExpandButton: Boolean;
    HasImage: Boolean;
    MaxExpandButtonWidth: Integer;
    MaxImageWidth: Integer;

    function GetViewInfo: TdxNavBarHamburgerMenuViewInfo;
    procedure AddChildsOffset;
    procedure CalculateWithExpandButton;
    procedure CalculateWithoutExpandButton;
  public
    CaptionLeft: Integer;
    ExpandButtonCenter: Integer;
    ImageCenter: Integer;
    Group: TdxNavBarGroupViewInfo;

    function IsTopLevel(AViewInfo: TdxNavBarHamburgerMenuViewInfo): Boolean;
    function UseLargeImages(AItem: TdxNavBarCustomItemViewInfo): Boolean;
    procedure Calculate(AGroup: TdxNavBarGroupViewInfo);
    procedure CalculateElementInfos(AItem: TdxNavBarCustomItemViewInfo);
    procedure CalculateTopLevelGroupCaptionLayout
      (AViewInfo: TdxNavBarHamburgerMenuViewInfo);

    procedure Reset;
  end;

  { TdxHamburgerMenuItemsLayoutCalculator }

  TdxHamburgerMenuItemsLayoutCalculator = class
  strict private
    FViewInfo: TdxNavBarHamburgerMenuViewInfo;
    procedure Align(AGroup: TdxNavBarChildGroupViewInfo;
      const ALayout: TdxHamburgerMenuGroupLayout); overload;
    procedure Align(ALink: TdxNavBarLinkViewInfo;
      const ALayout: TdxHamburgerMenuGroupLayout); overload;
    procedure AlignItems(AGroupViewInfo: TdxNavBarGroupViewInfo);
  protected
    procedure DoAlign(AGroupViewInfo: TdxNavBarGroupViewInfo;
      const ALayout: TdxHamburgerMenuGroupLayout);
  public
    constructor Create(AViewInfo: TdxNavBarHamburgerMenuViewInfo);
    class procedure AlignContent(AViewInfo: TdxNavBarHamburgerMenuViewInfo);
  end;

  { TdxHamburgerHelper }

  TdxHamburgerHelper = class
  public
    class function NeedDrawNavPaneItemBackground(AStates: TdxNavBarObjectStates)
      : Boolean; overload; static;
    class function NeedDrawNavPaneItemBackground(AState: TdxNavBarPartState)
      : Boolean; overload; static;
    class procedure CheckWidth(var AWidth: Integer); static;
    class procedure NavBarItemLinkClick(ANavBar: TdxCustomNavBar;
      AItemLink: TdxNavBarItemLink); static;
  end;

  { TdxHamburgerMenuRootGroupViewInfo }

  TdxHamburgerMenuRootGroupViewInfo = class(TdxNavBarHamburgerMenuGroupViewInfo)
  protected
    function IsCaptionCalculationNeeded: Boolean; override;
    procedure DoCalculateCaptionBounds(X, Y: Integer); override;
  end;

  { TdxHamburgerMenuSelectedPopupGroupViewInfo }

  TdxHamburgerMenuSelectedPopupGroupViewInfo = class
    (TdxNavBarHamburgerMenuGroupViewInfo)
  public
    function IsItemsVisible: Boolean; override;
  end;

  { TdxHamburgerMenuCollapsedGroupViewInfo }

  TdxHamburgerMenuCollapsedGroupViewInfo = class
    (TdxNavBarHamburgerMenuGroupViewInfo)
  protected
    function GetState: TdxNavBarObjectStates; override;
    procedure OffsetContent; override;
  public
    function IsCaptionImageVisible: Boolean; override;
    function IsCaptionSignVisible: Boolean; override;
  end;

  { TdxHamburgerMenuCollapsedStateCalculatorGroupViewInfo }

  TdxHamburgerMenuCollapsedStateCalculatorGroupViewInfo = class
    (TdxHamburgerMenuCollapsedGroupViewInfo)
  public
    function IsItemsVisible: Boolean; override;
  end;

  { TdxHamburgerMenuOverflowPanelPainter }

  TdxHamburgerMenuOverflowPanelPainter = class
    (TdxNavBarSkinBasedOverflowPanelPainter)
  protected
    function GetItemElement: TdxSkinElement; override;
    procedure DrawOverflowPanelItemBackground(ACanvas: TCanvas;
      APanel: TdxNavBarCustomOverflowPanelViewInfo; AItemIndex: Integer;
      AState: TdxNavBarPartState; const ARect: TRect); override;
  public
    procedure DrawBackground(ACanvas: TCanvas;
      APanel: TdxNavBarCustomOverflowPanelViewInfo); override;
    procedure DrawSign(ACanvas: TCanvas;
      APanel: TdxNavBarCustomOverflowPanelViewInfo); override;
  end;

  { TdxHamburgerSignPainter }

  TdxHamburgerSignPainter = class(TdxNavBarExplorerBarSignPainter)
  protected
    class procedure InternalDrawSign(ACanvas: TCanvas; ARect: TRect;
      AScaleFactor: TdxScaleFactor; AForeColor, ABackColor1,
      ABackColor2: TColor; AState: TdxNavBarObjectStates); override;
  end;

  { TdxHamburgerNavPaneDefaultItemPattern }

  TdxHamburgerNavPaneDefaultItemPattern = class
    (TdxNavBarHamburgerMenuGroupViewInfo)
  strict protected
    FImageSize: TSize;
    function IsCaptionCalculationNeeded: Boolean; override;
  protected
    FHasImage: Boolean;
    function GetImageWidth: Integer; override;
    function GetImageHeight: Integer; override;
    function HasImage: Boolean; override;
    function HasScrollBarInGroupCaption: Boolean; override;
    procedure CalculateBounds(var X, Y: Integer); override;

    function ActualTextRect: TRect; virtual;
    procedure CorrectRects; virtual;

    function AllowHideItems: Boolean;
  public
    constructor Create(AViewInfo: TdxNavBarViewInfo; AGroup: TdxNavBarGroup;
      ACaptionVisible, AItemsVisible: Boolean); override;
    function CaptionHAlignment: TdxBarStyleHAlignment; override;
    function CaptionVAlignment: TdxBarStyleVAlignment; override;

    procedure Calculate(const AImageSize: TSize);
  end;

  { TdxHamburgerNavPaneCompactItemPattern }

  TdxHamburgerNavPaneCompactItemPattern = class
    (TdxHamburgerNavPaneDefaultItemPattern)
  protected
    function ActualTextRect: TRect; override;
    procedure CorrectRects; override;
  end;

  { TdxHamburgerVerticalLayoutCalculator }

  TdxHamburgerLayoutCalculatorClass = class of
    TdxHamburgerVerticalLayoutCalculator;

  TdxHamburgerVerticalLayoutCalculator = class
  strict protected
  class var
    FBounds: TRect;
    FItems: TdxNavBarHamburgerMenuNavPaneItemList;
    FVisibleItems: TdxNavBarHamburgerMenuNavPaneItemList;

    FItemPatternRect: TRect;
    FItemPatternImageRect: TRect;
    FItemPatternTextRect: TRect;

    FTopAlignedItemCount: Integer;
    FBottomAlignedItemCount: Integer;
    FLastTopAlignedItem: TdxNavBarHamburgerMenuNavPaneCustomItem;
  strict private
    class procedure CalculateItemLayoutPattern
      (APattern: TdxHamburgerNavPaneDefaultItemPattern);
    class procedure CalculateItemLayout
      (AItem: TdxNavBarHamburgerMenuNavPaneCustomItem; const APosition: TPoint);
    class procedure CalculateItemsLayout(AAllowHideItems: Boolean);
    class procedure Initialize;
  protected
    class function IsItemVisible(AItem: TdxNavBarHamburgerMenuNavPaneCustomItem)
      : Boolean; virtual;
    class procedure CalculateItemPosition
      (AItem: TdxNavBarHamburgerMenuNavPaneCustomItem;
      var APosition: TPoint); virtual;
  public
    class procedure Calculate(APattern: TdxNavBarGroupViewInfo;
      const ABounds: TRect;
      AItems, AVisibleItems: TdxNavBarHamburgerMenuNavPaneItemList); virtual;
  end;

  { TdxHamburgerHorizontalLayoutCalculator }

  TdxHamburgerHorizontalLayoutCalculator = class
    (TdxHamburgerVerticalLayoutCalculator)
  protected
    class function IsItemVisible(AItem: TdxNavBarHamburgerMenuNavPaneCustomItem)
      : Boolean; override;
    class procedure CalculateItemPosition
      (AItem: TdxNavBarHamburgerMenuNavPaneCustomItem;
      var APosition: TPoint); override;

    class function IsTopAlignment
      (AItem: TdxNavBarHamburgerMenuNavPaneCustomItem): Boolean; virtual;
  end;

  { TdxHamburgerMenuOverflowPanelLayoutCalculator }

  TdxHamburgerMenuOverflowPanelLayoutCalculator = class
    (TdxHamburgerHorizontalLayoutCalculator)
  public
    class function IsTopAlignment
      (AItem: TdxNavBarHamburgerMenuNavPaneCustomItem): Boolean; override;
    class procedure Calculate(APattern: TdxNavBarGroupViewInfo;
      const ABounds: TRect;
      AItems, AVisibleItems: TdxNavBarHamburgerMenuNavPaneItemList); override;
  end;

  { TdxHamburgerMenuOverlayInnerControl }

  TdxHamburgerMenuOverlayInnerControl = class(TdxNavBarPopupInnerControl)
  protected
    function GetMapperClass: TdxNavBarCloneMapperClass; override;
  end;

  { TdxHamburgerMenuActiveGroupPopupControlViewInfo }

  TdxHamburgerMenuActiveGroupPopupControlViewInfo = class
    (TdxNavBarCustomPopupControlViewInfo)
  strict private
    function GetPainter: TdxNavBarHamburgerMenuPainter;
  protected
    function CalculatePosition: TPoint; override;
    function GetBorderOffsets: TRect; override;
    function GetClientRect: TRect; override;
    function GetMaxHeight: Integer; override;
    procedure CalculateBounds(AClientWidth: Integer); override;
  public
    property Painter: TdxNavBarHamburgerMenuPainter read GetPainter;
  end;

  { TdxHamburgerMenuAnimationController }

  TdxHamburgerMenuAnimationController = class
    (TdxNavBarCollapseStateAnimationController)
  strict private
    FIsExpandMode: Boolean;
    function GetMaster: TdxCustomNavBar;
    function GetPopupControl: TdxHamburgerMenuCustomPopupControl;
    procedure DoFinish;
  strict protected
    function GetAnimationBufferRect: TRect; override;
    procedure CalculateBounds(var ASourceRect, ATargetRect: TRect); override;
    procedure AfterAnimation; override;
    procedure BeforeAnimation; override;
    procedure HandleAnimationFinishStep; override;
    procedure HandleAnimationStep; override;
    procedure ResumeAnimationTransition; override;

    property PopupControl: TdxHamburgerMenuCustomPopupControl
      read GetPopupControl;
  protected
    function AllowAnimation(AIsExpandMode: Boolean): Boolean; override;
    function GetClientRectOffset: TPoint; override;
    function IsExpandMode: Boolean; override;

    procedure Animate(AExpand: Boolean);
  end;

  { TdxHamburgerMenuCustomPopupControl }

  TdxHamburgerMenuCustomPopupControl = class(TdxNavBarCustomPopupControl,
    IdxAcrylicHostControl)
  strict private
    FInnerControlTopLeft: TPoint;
    function UseAnimation
      (AType: TdxNavBarHamburgerMenuPopupAnimationType): Boolean;
    function UseFading: Boolean;
    procedure SetAnimationController(const AValue
      : TdxHamburgerMenuAnimationController);
    procedure Animate(AIsExpandMode: Boolean);
    procedure ChangeInnerControlVisibility(AVisible: Boolean);
    procedure InvalidateWindow;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  strict protected
    FAnimationController: TdxHamburgerMenuAnimationController;

    function GetAnimationType
      : TdxNavBarHamburgerMenuPopupAnimationType; virtual;
    procedure CreateAnimationController; virtual;
    procedure DoCollapsed; virtual;
    procedure DoExpanded; virtual;
    procedure CollapseStateChanged(AIsExpandMode: Boolean);

    property AnimationController: TdxHamburgerMenuAnimationController
      read FAnimationController write SetAnimationController;
  protected
    // IdxAcrylicHostControl
    function GetAcrylicColor: TdxAlphaColor;
    function IsAcrylic: Boolean;

    procedure InitPopup; override;
    procedure DoCloseUp; override;
    procedure DoShowed; override;
    procedure Paint; override;

    function UseExpanding: Boolean;
    procedure AfterAnimation;
    procedure BeforeAnimation(AIsExpandMode: Boolean); virtual;
    procedure HandleAnimationStep;
    procedure UpdateChildControls;

    property InnerControlTopLeft: TPoint read FInnerControlTopLeft;
  public
    destructor Destroy; override;
  end;

  { TdxHamburgerMenuOverlayViewInfo }

  TdxHamburgerMenuOverlayViewInfo = class(TdxNavBarCustomPopupControlViewInfo)
  protected
    procedure CalculateBounds(AClientWidth: Integer); override;
  end;

  { TdxHamburgerMenuOverlayMinimalAnimationController }

  TdxHamburgerMenuOverlayMinimalAnimationController = class
    (TdxHamburgerMenuAnimationController)
  protected
    function GetAnimationBufferRect: TRect; override;
    procedure HandleAnimationStep; override;
  end;

  { TdxHamburgerMenuOverlayControl }

  TdxHamburgerMenuOverlayControl = class(TdxHamburgerMenuCustomPopupControl)
  strict private
    function IsOverlayMinimal: Boolean;
    procedure SetOverlayVisible(AValue: Boolean);
  strict protected
    function GetAnimationType
      : TdxNavBarHamburgerMenuPopupAnimationType; override;
    procedure DoCollapsed; override;
    procedure DoExpanded; override;
  protected
    function CreateInnerControl: TdxCustomNavBar; override;
    function CreatePopupViewInfo: TdxNavBarCustomPopupControlViewInfo; override;
    procedure CreateAnimationController; override;
    procedure BeforeAnimation(AIsExpandMode: Boolean); override;
  public
    constructor Create(ANavBar: TdxCustomNavBar); override;
  end;

  { TdxHamburgerActiveGroupMapper }

  TdxHamburgerActiveGroupMapper = class(TdxNavBarActiveGroupMapper)
  protected
    procedure CloneMasterOptions; override;
  end;

  { TdxHamburgerActiveGroupInnerControl }

  TdxHamburgerActiveGroupInnerControl = class(TdxNavBarActiveGroupInnerControl)
  protected
    function GetMapperClass: TdxNavBarCloneMapperClass; override;
  end;

  { TGroupOverlayViewInfo }

  TGroupOverlayViewInfo = class(TdxNavBarCustomPopupControlViewInfo)
  protected
    function CalculatePosition: TPoint; override;
  end;

  { TdxHamburgerMenuActiveGroupAnimationController }

  TdxHamburgerMenuActiveGroupAnimationController = class
    (TdxHamburgerMenuAnimationController)
  strict protected
    function GetExpandAnimationTime: Cardinal; override;
  end;

  { TdxHamburgerMenuSelectedGroupPopupControlViewInfo }

  TdxHamburgerMenuSelectedGroupPopupControlViewInfo = class
    (TdxHamburgerMenuActiveGroupPopupControlViewInfo)
  strict private
    function GetSkinHelper: TdxNavBarSkinBasedPainterHelper;
  protected
    function GetBorderOffsets: TRect; override;
  end;

  { TdxHamburgerMenuActiveGroupPopupControl }

  TdxHamburgerMenuActiveGroupPopupControl = class
    (TdxHamburgerMenuCustomPopupControl)
  protected
    function CreateInnerControl: TdxCustomNavBar; override;
    function CreatePopupViewInfo: TdxNavBarCustomPopupControlViewInfo; override;
    function GetAnimationType
      : TdxNavBarHamburgerMenuPopupAnimationType; override;
    procedure CreateAnimationController; override;
    procedure Paint; override;
  end;

  { TdxHamburgerMenuNavPaneCustomLayoutCalculator }

  TdxHamburgerMenuNavPaneCustomLayoutCalculator = class abstract
  strict private
  class var
    FCompactItemSize: TSize;
    FItemsSize: TSize;
    FPanel: TdxNavBarHamburgerMenuNavPaneViewInfo;
  strict private
    class function GetItemHeight: Integer;
    class function GetItemWidth: Integer;

    class procedure CalculateItems(ACalculator
      : TdxHamburgerLayoutCalculatorClass; AItemPattern: TdxNavBarGroupViewInfo;
      const ARect: TRect; AItems, AVisibleItems, AHiddenItems
      : TdxNavBarHamburgerMenuNavPaneItemList);

    class procedure CalculateRect(const ABounds: TRect);
    class procedure CalculateOverflowPanelItems;
    class procedure CalculateOverflowPanelRect;

    class procedure PopulateOverflowPanel(AHiddenItems
      : TdxNavBarHamburgerMenuNavPaneItemList);
  strict protected
    class function GetRectHeight: Integer; virtual; abstract;
    class procedure AlignItems; virtual; abstract;

    class function GetItemsRect: TRect; virtual;
    class function IsEnoughSpace: Boolean; virtual;
    class procedure DoCalculateOverflowPanelRect; virtual;

    class function GetMaxTopPos: Integer;
    class function GetOverflowPanelSignSize: TSize;
    class function IsOverflowPanelVisible: Boolean;
    class procedure OffsetItems(AOffsetY: Integer);
    class procedure SetPanelRectTop(AValue: Integer);

    class property CompactItemSize: TSize read FCompactItemSize;
    class property ItemsSize: TSize read FItemsSize;
    class property Panel: TdxNavBarHamburgerMenuNavPaneViewInfo read FPanel;
  public
    class procedure Calculate(APanel: TdxNavBarHamburgerMenuNavPaneViewInfo;
      const ABounds: TRect);
  end;

  { TdxHamburgerMenuNavPaneDefaultLayoutCalculator }

  TdxHamburgerMenuNavPaneDefaultLayoutCalculator = class
    (TdxHamburgerMenuNavPaneCustomLayoutCalculator)
  strict protected
    class function GetItemsRect: TRect; override;
    class function GetRectHeight: Integer; override;
    class procedure AlignItems; override;
    class procedure DoCalculateOverflowPanelRect; override;
  end;

  { TdxHamburgerMenuNavPaneCompactLayoutCalculator }

  TdxHamburgerMenuNavPaneCompactLayoutCalculator = class
    (TdxHamburgerMenuNavPaneCustomLayoutCalculator)
  strict protected
    class function GetItemsRect: TRect; override;
    class function GetRectHeight: Integer; override;
    class function IsEnoughSpace: Boolean; override;
    class procedure AlignItems; override;
    class procedure DoCalculateOverflowPanelRect; override;
  end;

  { TdxNavBarHamburgerMenuSeparatorViewInfo }

  TdxNavBarHamburgerMenuSeparatorViewInfo = class
    (TdxNavBarHamburgerMenuItemViewInfo)
  public
    function SeparatorRect: TRect; override;
  end;

  { TdxNavBarHamburgerMenuSeparatorViewInfo }

function TdxNavBarHamburgerMenuSeparatorViewInfo.SeparatorRect: TRect;
var
  ASeparatorIndent: Integer;
begin
  ASeparatorIndent := ScaleFactor.Apply(8);
  if ViewInfo.IsCollapsed then
  begin
    Result := Rect;
    Inc(Result.Left, ASeparatorIndent);
    Dec(Result.Right, ASeparatorIndent);
  end
  else
  begin
    Result := CaptionRect;
    Dec(Result.Right, ASeparatorIndent);
    if UseLargeImages then
      Inc(Result.Left, ASeparatorIndent);
  end;
  Result := cxRectCenterVertically(Result, ScaleFactor.Apply(1));
end;

{ TdxNavBarGroupViewInfoHelper }

function TdxNavBarGroupViewInfoHelper.GetHasScrollBar: Boolean;
begin
  Result := HasScrollBar;
end;

function TdxNavBarGroupViewInfoHelper.TopLevel: Boolean;
begin
  Result := IsTopLevel;
end;

{ TdxHamburgerMenuDropInfoCalculator }

function TdxHamburgerMenuDropInfoCalculator.IsTargetGroupVerticalLayout
  (const ATargetPoint: TPoint): Boolean;
begin
  if GetViewInfo.NavPane.UseCompactLayout then
    Result := not PtInRect(GetViewInfo.NavPane.FClientRect, ATargetPoint)
  else
    Result := inherited IsTargetGroupVerticalLayout(ATargetPoint);
end;

function TdxHamburgerMenuDropInfoCalculator.IsTargetVerticalLayout
  (AGroup: TdxNavBarGroup): Boolean;
begin
  if GetViewInfo.NavPane.UseCompactLayout then
    Result := GetViewInfo.NavPane.Contains(AGroup)
  else
    Result := inherited IsTargetVerticalLayout(AGroup);
end;

procedure TdxHamburgerMenuDropInfoCalculator.CheckDropInfo(var ADropTargetInfo
  : TdxNavBarDropTargetInfo);
begin
  inherited CheckDropInfo(ADropTargetInfo);
end;

function TdxHamburgerMenuDropInfoCalculator.GetViewInfo
  : TdxNavBarHamburgerMenuViewInfo;
begin
  Result := NavBar.ViewInfo as TdxNavBarHamburgerMenuViewInfo;
end;

{ TdxHamburgerMenuGroupLayout }

procedure TdxHamburgerMenuGroupLayout.Reset;
begin
  Group := nil;
  ImageCenter := 0;
  CaptionLeft := 0;
  ExpandButtonCenter := 0;
  HasImage := False;
  HasExpandButton := False;
  MaxImageWidth := 0;
  MaxExpandButtonWidth := 0;
end;

function TdxHamburgerMenuGroupLayout.IsTopLevel
  (AViewInfo: TdxNavBarHamburgerMenuViewInfo): Boolean;
begin
  Result := Group.TopLevel and AViewInfo.IsNavPaneActiveGroup(Group) or
    AViewInfo.IsGroupShowing and AViewInfo.IsRootGroup(Group);
end;

function TdxHamburgerMenuGroupLayout.UseLargeImages
  (AItem: TdxNavBarCustomItemViewInfo): Boolean;
begin
  Result := False;
  if AItem <> nil then
  begin
    if AItem is TdxNavBarChildGroupViewInfo then
      Result := TdxNavBarChildGroupViewInfo(AItem).UseLargeImages
    else if AItem is TdxNavBarLinkViewInfo then
      Result := TdxNavBarLinkViewInfo(AItem).UseLargeImages;
  end;
end;

procedure TdxHamburgerMenuGroupLayout.AddChildsOffset;
var
  AChildLevelIndent: Integer;
  AViewInfo: TdxNavBarHamburgerMenuViewInfo;
begin
  AViewInfo := GetViewInfo;
  if not IsTopLevel(AViewInfo) and not AViewInfo.IsRootGroup(Group) then
  begin
    AChildLevelIndent := AViewInfo.NavBar.OptionsView.HamburgerMenu.
      ChildLevelIndent;
    if AChildLevelIndent = -1 then
      AChildLevelIndent := AViewInfo.GetChildItemOffset;
    if AChildLevelIndent <> 0 then
    begin
      Inc(CaptionLeft, AChildLevelIndent);
      if ImageCenter > 0 then
        Inc(ImageCenter, AChildLevelIndent);
      Inc(ExpandButtonCenter, AChildLevelIndent);
    end;
  end;
end;

procedure TdxHamburgerMenuGroupLayout.Calculate(AGroup: TdxNavBarGroupViewInfo);
var
  AItem: TdxNavBarCustomItemViewInfo;
  AViewInfo: TdxNavBarHamburgerMenuViewInfo;
begin
  Reset;
  Group := AGroup;
  for AItem in Group.Infos do
    if not UseLargeImages(AItem) then
      CalculateElementInfos(AItem);
  CaptionLeft := Group.CaptionTextRect.Left;
  ImageCenter := Group.ImageRect.CenterPoint.X;
  AViewInfo := GetViewInfo;
  AddChildsOffset;
  if IsTopLevel(AViewInfo) then
    CalculateTopLevelGroupCaptionLayout(AViewInfo);
  if HasExpandButton and not AViewInfo.IsNavPaneActiveGroup(Group) then
    CalculateWithExpandButton
  else
    CalculateWithoutExpandButton;
end;

procedure TdxHamburgerMenuGroupLayout.CalculateElementInfos
  (AItem: TdxNavBarCustomItemViewInfo);
var
  AGroupViewInfo: TdxNavBarChildGroupViewInfo;
begin
  MaxImageWidth := Max(MaxImageWidth, AItem.ImageRect.Width);
  if not HasImage then
    HasImage := AItem.HasImage;
  if AItem is TdxNavBarChildGroupViewInfo then
  begin
    AGroupViewInfo := TdxNavBarChildGroupViewInfo(AItem);
    HasExpandButton := not AGroupViewInfo.TopLevel;
    if HasExpandButton then
      MaxExpandButtonWidth := Max(MaxExpandButtonWidth,
        AGroupViewInfo.ExpandButtonRect.Width);
  end
end;

procedure TdxHamburgerMenuGroupLayout.CalculateTopLevelGroupCaptionLayout
  (AViewInfo: TdxNavBarHamburgerMenuViewInfo);
begin
  CaptionLeft := AViewInfo.GetGroupCaptionTextIndent;
  if HasImage then
  begin
    ImageCenter := AViewInfo.GetNavBarCollapsedWidth div 2;
    CaptionLeft := ImageCenter + MaxImageWidth div 2 + AViewInfo.GetTextIndent;
  end;
end;

procedure TdxHamburgerMenuGroupLayout.CalculateWithExpandButton;
var
  AHalfMaxImageWidth, AHalfExpandButtonWidth, ATextIndent: Integer;
begin
  ATextIndent := GetViewInfo.GetTextIndent;
  AHalfMaxImageWidth := MaxImageWidth div 2;
  AHalfExpandButtonWidth := MaxExpandButtonWidth div 2;

  ExpandButtonCenter := Group.ImageRect.CenterPoint.X;
  if Group.ImageRect.IsEmpty then
    ExpandButtonCenter := Group.CaptionTextRect.Left + AHalfExpandButtonWidth;
  ImageCenter := ExpandButtonCenter + AHalfExpandButtonWidth + ATextIndent +
    AHalfMaxImageWidth;
  CaptionLeft := ImageCenter + IfThen(HasImage, AHalfMaxImageWidth +
    ATextIndent);

  AddChildsOffset;
end;

procedure TdxHamburgerMenuGroupLayout.CalculateWithoutExpandButton;
var
  AHalfMaxImageWidth: Integer;
  AViewInfo: TdxNavBarHamburgerMenuViewInfo;
begin
  AHalfMaxImageWidth := MaxImageWidth div 2;
  if HasImage then
  begin
    AViewInfo := GetViewInfo;
    if ImageCenter = 0 then
      ImageCenter := CaptionLeft + AHalfMaxImageWidth;
    CaptionLeft := ImageCenter + AHalfMaxImageWidth + AViewInfo.GetTextIndent;
    AddChildsOffset;
  end;
end;

function TdxHamburgerMenuGroupLayout.GetViewInfo
  : TdxNavBarHamburgerMenuViewInfo;
begin
  Result := Group.ViewInfo as TdxNavBarHamburgerMenuViewInfo;
end;

{ TdxHamburgerMenuItemsLayoutCalculator }

constructor TdxHamburgerMenuItemsLayoutCalculator.Create
  (AViewInfo: TdxNavBarHamburgerMenuViewInfo);
begin
  inherited Create;
  FViewInfo := AViewInfo;
end;

class procedure TdxHamburgerMenuItemsLayoutCalculator.AlignContent
  (AViewInfo: TdxNavBarHamburgerMenuViewInfo);
var
  I: Integer;
  AGroupViewInfo: TdxNavBarGroupViewInfoAccess;
  AHelper: TdxHamburgerMenuItemsLayoutCalculator;
  ATemplate: TdxHamburgerMenuGroupLayout;
  AItem: TdxNavBarCustomItemViewInfo;
  ARootGroupList: TList<TdxNavBarGroupViewInfoAccess>;
begin
  AHelper := TdxHamburgerMenuItemsLayoutCalculator.Create(AViewInfo);
  try
    ATemplate.Reset;
    for I := 0 to AViewInfo.GroupCount - 1 do
      if not ATemplate.UseLargeImages(AViewInfo.Groups[I]) then
        ATemplate.CalculateElementInfos(AViewInfo.Groups[I]);
    ATemplate.CalculateTopLevelGroupCaptionLayout(AViewInfo);
    ARootGroupList := TList<TdxNavBarGroupViewInfoAccess>.Create;
    try
      for I := 0 to AViewInfo.GroupCount - 1 do
      begin
        AGroupViewInfo := TdxNavBarGroupViewInfoAccess(AViewInfo.Groups[I]);
        AGroupViewInfo.ImageRect.Offset(ATemplate.ImageCenter -
          AGroupViewInfo.ImageRect.CenterPoint.X, 0);
        Inc(AGroupViewInfo.FCaptionTextRect.Left, ATemplate.CaptionLeft -
          AGroupViewInfo.CaptionTextRect.Left);
        if not AViewInfo.IsRootGroup(AGroupViewInfo) then
          AHelper.AlignItems(AGroupViewInfo)
        else
          ARootGroupList.Add(AGroupViewInfo);
      end;

      for AGroupViewInfo in ARootGroupList do
      begin
        if AViewInfo.IsInternal then
          ATemplate.Reset;
        for AItem in AGroupViewInfo.Infos do
          if not ATemplate.UseLargeImages(AItem) then
            ATemplate.CalculateElementInfos(AItem);
        ATemplate.CalculateTopLevelGroupCaptionLayout(AViewInfo);
        AHelper.DoAlign(AGroupViewInfo, ATemplate);
      end;
    finally
      ARootGroupList.Free;
    end;
  finally
    AHelper.Free;
  end;
end;

procedure TdxHamburgerMenuItemsLayoutCalculator.Align
  (AGroup: TdxNavBarChildGroupViewInfo;
  const ALayout: TdxHamburgerMenuGroupLayout);
var
  AGroupAccess: TdxNavBarGroupViewInfoAccess;
begin
  AGroup.ExpandButtonRect.Offset(ALayout.ExpandButtonCenter -
    AGroup.ExpandButtonRect.CenterPoint.X, 0);
  if AGroup.HasImage then
    AGroup.ImageRect.Offset(ALayout.ImageCenter -
      AGroup.ImageRect.CenterPoint.X, 0);
  AGroup.CaptionTextRect.Offset(ALayout.CaptionLeft -
    AGroup.CaptionTextRect.Left, 0);
  AGroupAccess := TdxNavBarGroupViewInfoAccess(AGroup);
  AGroupAccess.FCaptionTextRect.Right := Min(AGroup.CaptionTextRect.Right,
    AGroup.CaptionRect.Right);
  if AGroup.CaptionTextRect.IntersectsWith(AGroup.CaptionSignRect) then
    AGroupAccess.FCaptionTextRect.Right := AGroup.CaptionSignRect.Left;
  AlignItems(AGroup);
end;

procedure TdxHamburgerMenuItemsLayoutCalculator.AlignItems(AGroupViewInfo
  : TdxNavBarGroupViewInfo);
var
  ALayout: TdxHamburgerMenuGroupLayout;
begin
  ALayout.Calculate(AGroupViewInfo);
  DoAlign(AGroupViewInfo, ALayout);
end;

procedure TdxHamburgerMenuItemsLayoutCalculator.Align
  (ALink: TdxNavBarLinkViewInfo; const ALayout: TdxHamburgerMenuGroupLayout);
begin
  if not FViewInfo.IsVerticalItem(ALink) and
    (ALink.GroupViewInfo.Group.LinksUseSmallImages xor FViewInfo.IsCollapsed)
  then
  begin
    if ALayout.ImageCenter > 0 then
      ALink.ImageRect.Offset(ALayout.ImageCenter -
        ALink.ImageRect.CenterPoint.X, 0);
    ALink.CaptionRect.Offset(ALayout.CaptionLeft - ALink.CaptionRect.Left, 0);

    TdxNavBarItemViewInfoAccess(ALink).FCaptionRect.Right :=
      Min(ALink.CaptionRect.Right, ALink.Rect.Right);

    TdxNavBarItemCalculator.CalculateFocusRect(ALink, ALink.Rect, ALink.Rect);
  end;
end;

procedure TdxHamburgerMenuItemsLayoutCalculator.DoAlign(AGroupViewInfo
  : TdxNavBarGroupViewInfo; const ALayout: TdxHamburgerMenuGroupLayout);
var
  AItem: TdxNavBarCustomItemViewInfo;
begin
  for AItem in AGroupViewInfo.Infos do
    if FViewInfo.IsCollapsed xor not ALayout.UseLargeImages(AItem) then
    begin
      if AItem is TdxNavBarChildGroupViewInfo then
        Align(TdxNavBarChildGroupViewInfo(AItem), ALayout)
      else
        Align(AItem as TdxNavBarLinkViewInfo, ALayout);
    end;
end;

{ TdxHamburgerHelper }

class function TdxHamburgerHelper.NeedDrawNavPaneItemBackground
  (AStates: TdxNavBarObjectStates): Boolean;
begin
  Result := AStates * [sActive, sPressed, sHotTracked] <> [];
end;

class function TdxHamburgerHelper.NeedDrawNavPaneItemBackground
  (AState: TdxNavBarPartState): Boolean;
const
  StateMap: array [TdxNavBarPartState] of TdxNavBarObjectState = (sDisabled,
    sHotTracked, sPressed, sDisabled, sDisabled, sDisabled);
var
  AStates: TdxNavBarObjectStates;
begin
  AStates := [];
  Include(AStates, StateMap[AState]);
  Result := NeedDrawNavPaneItemBackground(AStates);
end;

class procedure TdxHamburgerHelper.CheckWidth(var AWidth: Integer);
begin
  AWidth := IfThen(AWidth = 0, 150, AWidth);
end;

class procedure TdxHamburgerHelper.NavBarItemLinkClick(ANavBar: TdxCustomNavBar;
  AItemLink: TdxNavBarItemLink);
begin
  TdxNavBarControllerAccess(ANavBar.Controller).DoLinkClick(ANavBar, AItemLink);
end;

{ TdxHamburgerMenuRootGroupViewInfo }

function TdxHamburgerMenuRootGroupViewInfo.IsCaptionCalculationNeeded: Boolean;
begin
  Result := True;
end;

procedure TdxHamburgerMenuRootGroupViewInfo.DoCalculateCaptionBounds(X,
  Y: Integer);
begin
  inherited DoCalculateCaptionBounds(X, Y);
  FRect.Height := 0;
  if TdxNavBarAccess(NavBar).IsDragging and (ItemCount = 0) and
    not ViewInfo.IsNavPaneActiveGroup(Self) then
    FRect.Height := 2 * TdxNavBarViewInfoAccess(ViewInfo).GetDragArrowHeight;
  FCaptionRect.Height := FRect.Height;
  FCaptionTextRect.Height := 0;
  FCaptionSignRect.Height := 0;
end;

{ TdxHamburgerMenuSelectedPopupGroupViewInfo }

function TdxHamburgerMenuSelectedPopupGroupViewInfo.IsItemsVisible: Boolean;
begin
  Result := True;
end;

{ TdxHamburgerMenuCollapsedGroupViewInfo }

function TdxHamburgerMenuCollapsedGroupViewInfo.GetState: TdxNavBarObjectStates;
begin
  Result := inherited GetState;
  ViewInfo.CheckSelectedPopupGroupState(Self, Result);
end;

procedure TdxHamburgerMenuCollapsedGroupViewInfo.OffsetContent;
begin
  inherited OffsetContent;
  FCaptionTextRect.Empty;
end;

function TdxHamburgerMenuCollapsedGroupViewInfo.IsCaptionImageVisible: Boolean;
begin
  Result := True;
end;

function TdxHamburgerMenuCollapsedGroupViewInfo.IsCaptionSignVisible: Boolean;
begin
  Result := False;
end;

{ TdxHamburgerMenuCollapsedStateCalculatorGroupViewInfo }

function TdxHamburgerMenuCollapsedStateCalculatorGroupViewInfo.
  IsItemsVisible: Boolean;
begin
  Result := inherited IsItemsVisible or not Group.LinksUseSmallImages;
end;

{ TdxHamburgerMenuOverflowPanelPainter }

procedure TdxHamburgerMenuOverflowPanelPainter.DrawBackground(ACanvas: TCanvas;
  APanel: TdxNavBarCustomOverflowPanelViewInfo);
begin
  // do nothing
end;

function TdxHamburgerMenuOverflowPanelPainter.GetItemElement: TdxSkinElement;
begin
  Result := (Helper as TdxNavBarHamburgerMenuSkinPainterHelper)
    .NavPaneItem(False);
end;

procedure TdxHamburgerMenuOverflowPanelPainter.DrawOverflowPanelItemBackground
  (ACanvas: TCanvas; APanel: TdxNavBarCustomOverflowPanelViewInfo;
  AItemIndex: Integer; AState: TdxNavBarPartState; const ARect: TRect);
var
  AActualState: TdxNavBarPartState;
begin
  AActualState := AState;
  if TdxNavBarHamburgerMenuPainter(APanel.NavBar.Painter).AllowHighlightEffects
  then
    AActualState := oisNormal;
  if IsActiveGroup(APanel, AItemIndex) then
    AActualState := oisPressed;
  if TdxHamburgerHelper.NeedDrawNavPaneItemBackground(AActualState) then
    inherited DrawOverflowPanelItemBackground(ACanvas, APanel, AItemIndex,
      AActualState, ARect);
end;

procedure TdxHamburgerMenuOverflowPanelPainter.DrawSign(ACanvas: TCanvas;
  APanel: TdxNavBarCustomOverflowPanelViewInfo);
var
  R: TRect;
begin
  inherited DrawSign(ACanvas, APanel);
  if TdxNavBarHamburgerMenuPainter(APanel.NavBar.Painter).AllowHighlightEffects
  then
  begin
    R := APanel.SignRect;
    if GetPartElementState(APanel, dxNavBarPart(nbOverflowPanelSign)) = esHot
    then
      TdxNavBarHamburgerMenuPainter(APanel.NavBar.Painter)
        .DrawElementHighlight(R);
    TdxNavBarHamburgerMenuPainter(APanel.NavBar.Painter)
      .DrawElementHighlightBorders(R);
  end;
end;

{ TdxHamburgerSignPainter }

class procedure TdxHamburgerSignPainter.InternalDrawSign(ACanvas: TCanvas;
  ARect: TRect; AScaleFactor: TdxScaleFactor;
  AForeColor, ABackColor1, ABackColor2: TColor; AState: TdxNavBarObjectStates);
var
  pt1, pt2, pt3: TPoint;
  AArrowHeight, AArrowWidth: Integer;
  R: TRect;
begin
  AArrowHeight := 5;
  AArrowWidth := 2 * AArrowHeight - 1;

  R := ARect;
  DrawSignSelection(ACanvas, R, AForeColor, ABackColor1, ABackColor2, AState);

  pt1.X := ARect.Left + (ARect.Right - ARect.Left) div 2 - 1;
  pt1.Y := ARect.Top + (ARect.Bottom - ARect.Top) div 2 +
    (AArrowHeight div 2) - 1;
  pt2.X := pt1.X - AArrowWidth div 2;
  pt2.Y := pt1.Y - AArrowHeight + 1;
  pt3.X := pt2.X + AArrowWidth - 1;
  pt3.Y := pt2.Y;

  ACanvas.Brush.Color := AForeColor;
  ACanvas.Pen.Color := AForeColor;
  ACanvas.Brush.Style := bsSolid;
  ACanvas.Pen.Style := psSolid;
  ACanvas.Polygon([pt1, pt2, pt3]);
end;

{ TdxHamburgerNavPaneDefaultItemPattern }

constructor TdxHamburgerNavPaneDefaultItemPattern.Create
  (AViewInfo: TdxNavBarViewInfo; AGroup: TdxNavBarGroup;
  ACaptionVisible, AItemsVisible: Boolean);
begin
  inherited Create(AViewInfo, AGroup, ACaptionVisible, AItemsVisible);
  FHasImage := True;
end;

function TdxHamburgerNavPaneDefaultItemPattern.CaptionHAlignment
  : TdxBarStyleHAlignment;
begin
  Result := TdxBarStyleHAlignment.haLeft;
end;

function TdxHamburgerNavPaneDefaultItemPattern.CaptionVAlignment
  : TdxBarStyleVAlignment;
begin
  Result := TdxBarStyleVAlignment.vaCenter;
end;

procedure TdxHamburgerNavPaneDefaultItemPattern.Calculate
  (const AImageSize: TSize);
var
  X, Y: Integer;
begin
  X := 0;
  Y := 0;
  FImageSize := AImageSize;
  CalculateBounds(X, Y);
end;

function TdxHamburgerNavPaneDefaultItemPattern.GetImageWidth: Integer;
begin
  if FImageSize.IsZero then
    Result := inherited GetImageWidth
  else
    Result := FImageSize.cx;
end;

function TdxHamburgerNavPaneDefaultItemPattern.GetImageHeight: Integer;
begin
  if FImageSize.IsZero then
    Result := inherited GetImageHeight
  else
    Result := FImageSize.cy;
end;

function TdxHamburgerNavPaneDefaultItemPattern.HasImage: Boolean;
begin
  Result := FHasImage;
end;

function TdxHamburgerNavPaneDefaultItemPattern.
  HasScrollBarInGroupCaption: Boolean;
begin
  Result := False;
end;

procedure TdxHamburgerNavPaneDefaultItemPattern.CalculateBounds(var X,
  Y: Integer);
begin
  inherited CalculateBounds(X, Y);
  CorrectRects;
end;

function TdxHamburgerNavPaneDefaultItemPattern.
  IsCaptionCalculationNeeded: Boolean;
begin
  Result := True;
end;

function TdxHamburgerNavPaneDefaultItemPattern.ActualTextRect: TRect;
begin
  Result := FCaptionTextRect;
end;

procedure TdxHamburgerNavPaneDefaultItemPattern.CorrectRects;
begin
  FRect := FCaptionRect;
  FItemsRect := FRect;
  if ViewInfo.IsCollapsed then
  begin
    FCaptionSignRect.Empty;
    FCaptionTextRect.Empty;
  end;
  CalcDesignRect(FRect);
end;

function TdxHamburgerNavPaneDefaultItemPattern.AllowHideItems: Boolean;
begin
  Result := NavBar.OptionsView.HamburgerMenu.ShowOverflowPanel;
end;

{ TdxHamburgerNavPaneCompactItemPattern }

function TdxHamburgerNavPaneCompactItemPattern.ActualTextRect: TRect;
begin
  Result := cxNullRect;
end;

procedure TdxHamburgerNavPaneCompactItemPattern.CorrectRects;
begin
  FCaptionSignRect.Empty;
  FCaptionRect.Width := ViewInfo.GetNavBarCollapsedWidth;
  inherited CorrectRects;
end;

{ TdxHamburgerVerticalLayoutCalculator }

class procedure TdxHamburgerVerticalLayoutCalculator.Calculate
  (APattern: TdxNavBarGroupViewInfo; const ABounds: TRect;
  AItems, AVisibleItems: TdxNavBarHamburgerMenuNavPaneItemList);
var
  AItemPattern: TdxHamburgerNavPaneDefaultItemPattern;
begin
  AItemPattern := APattern as TdxHamburgerNavPaneDefaultItemPattern;
  FItems := AItems;
  FVisibleItems := AVisibleItems;
  FBounds := ABounds;
  Initialize;
  CalculateItemLayoutPattern(AItemPattern);
  CalculateItemsLayout(AItemPattern.AllowHideItems);
end;

class function TdxHamburgerVerticalLayoutCalculator.IsItemVisible
  (AItem: TdxNavBarHamburgerMenuNavPaneCustomItem): Boolean;
begin
  Result := cxRectIsEqual(cxRectUnion(AItem.Rect, FBounds), FBounds);
end;

class procedure TdxHamburgerVerticalLayoutCalculator.CalculateItemLayoutPattern
  (APattern: TdxHamburgerNavPaneDefaultItemPattern);
begin
  FItemPatternRect := APattern.Rect;
  FItemPatternImageRect := APattern.ImageRect;
  FItemPatternTextRect := APattern.ActualTextRect;
end;

class procedure TdxHamburgerVerticalLayoutCalculator.CalculateItemLayout
  (AItem: TdxNavBarHamburgerMenuNavPaneCustomItem; const APosition: TPoint);
begin
  AItem.SetRect(cxRectOffset(FItemPatternRect, APosition));
  AItem.SetImageRect(cxRectOffset(FItemPatternImageRect, APosition));
  AItem.SetTextRect(cxRectOffset(FItemPatternTextRect, APosition));
end;

class procedure TdxHamburgerVerticalLayoutCalculator.CalculateItemPosition
  (AItem: TdxNavBarHamburgerMenuNavPaneCustomItem; var APosition: TPoint);
begin
  APosition.X := FBounds.Left;
  APosition.Y := FBounds.Top + FItemPatternRect.Height * FTopAlignedItemCount;
  FLastTopAlignedItem := AItem;
  Inc(FTopAlignedItemCount);
end;

class procedure TdxHamburgerVerticalLayoutCalculator.CalculateItemsLayout
  (AAllowHideItems: Boolean);
var
  I: Integer;
  AItem: TdxNavBarHamburgerMenuNavPaneCustomItem;
  APosition: TPoint;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    AItem := FItems[I];
    CalculateItemPosition(AItem, APosition);
    CalculateItemLayout(AItem, APosition);
    if IsItemVisible(AItem) or not AAllowHideItems then
      FVisibleItems.Add(AItem)
    else
      Break;
  end;
end;

class procedure TdxHamburgerVerticalLayoutCalculator.Initialize;
begin
  FBottomAlignedItemCount := 0;
  FTopAlignedItemCount := 0;
  FLastTopAlignedItem := nil;
end;

{ TdxHamburgerHorizontalLayoutCalculator }

class function TdxHamburgerHorizontalLayoutCalculator.IsItemVisible
  (AItem: TdxNavBarHamburgerMenuNavPaneCustomItem): Boolean;
begin
  Result := (AItem.Rect.Left >= FBounds.Left) and
    (AItem.Rect.Right <= FBounds.Right);
  if Result and (FLastTopAlignedItem <> nil) and (FLastTopAlignedItem <> AItem)
  then
    Result := not cxRectIntersect(FLastTopAlignedItem.Rect, AItem.Rect);
end;

class procedure TdxHamburgerHorizontalLayoutCalculator.CalculateItemPosition
  (AItem: TdxNavBarHamburgerMenuNavPaneCustomItem; var APosition: TPoint);
begin
  APosition.Y := FBounds.Top;
  if IsTopAlignment(AItem) then
  begin
    APosition.X := FBounds.Left + FItemPatternRect.Width * FTopAlignedItemCount;
    Inc(FTopAlignedItemCount);
    FLastTopAlignedItem := AItem;
  end
  else
  begin
    Inc(FBottomAlignedItemCount);
    APosition.X := FBounds.Right - FItemPatternRect.Width *
      FBottomAlignedItemCount;
  end;
end;

class function TdxHamburgerHorizontalLayoutCalculator.IsTopAlignment
  (AItem: TdxNavBarHamburgerMenuNavPaneCustomItem): Boolean;
begin
  Result := AItem.GroupViewInfo.Group.Align = TcxAlignmentVert.vaTop;
end;

{ TdxHamburgerMenuOverflowPanelLayoutCalculator }

class function TdxHamburgerMenuOverflowPanelLayoutCalculator.IsTopAlignment
  (AItem: TdxNavBarHamburgerMenuNavPaneCustomItem): Boolean;
begin
  Result := True;
end;

class procedure TdxHamburgerMenuOverflowPanelLayoutCalculator.Calculate
  (APattern: TdxNavBarGroupViewInfo; const ABounds: TRect;
  AItems, AVisibleItems: TdxNavBarHamburgerMenuNavPaneItemList);
var
  AItem: TdxNavBarHamburgerMenuNavPaneCustomItem;
  AOffset: TPoint;
begin
  inherited Calculate(APattern, ABounds, AItems, AVisibleItems);
  if AVisibleItems.Count > 0 then
  begin
    AOffset.Y := 0;
    AOffset.X := ABounds.Right - AVisibleItems.Last.Rect.Right;
    if not AOffset.IsZero then
      for AItem in AVisibleItems do
        AItem.Offset(AOffset);
  end;
end;

{ TdxHamburgerMenuOverlayInnerControl }

function TdxHamburgerMenuOverlayInnerControl.GetMapperClass
  : TdxNavBarCloneMapperClass;
begin
  Result := TdxNavBarControlMapper;
end;

{ TdxHamburgerMenuActiveGroupPopupControlViewInfo }

procedure TdxHamburgerMenuActiveGroupPopupControlViewInfo.CalculateBounds
  (AClientWidth: Integer);
var
  AViewInfo: TdxNavBarHamburgerMenuViewInfo;
begin
  TdxHamburgerHelper.CheckWidth(AClientWidth);
  AViewInfo := ViewInfo as TdxNavBarHamburgerMenuViewInfo;
  AViewInfo.BeginActiveGroupPopupControl;
  try
    FRect := TdxNavBarGroupPopupControlCalculator.CalculateBounds(ViewInfo,
      TdxHamburgerMenuSelectedPopupGroupViewInfo, AClientWidth,
      GetBorderOffsets)
  finally
    AViewInfo.EndActiveGroupPopupControl;
  end;
end;

function TdxHamburgerMenuActiveGroupPopupControlViewInfo.GetPainter
  : TdxNavBarHamburgerMenuPainter;
begin
  Result := inherited Painter as TdxNavBarHamburgerMenuPainter;
end;

function TdxHamburgerMenuActiveGroupPopupControlViewInfo.
  CalculatePosition: TPoint;

  procedure CorrectPosition(const ASize: TSize; var APosition: TPoint);
  var
    ADesktopWorkArea: TRect;
  begin
    ADesktopWorkArea := GetDesktopWorkArea(APosition);
    if ASize.cy > ADesktopWorkArea.Bottom - APosition.Y then
      APosition.Offset(0, -(APosition.Y + ASize.cy - ADesktopWorkArea.Bottom));
    if ASize.cx > ADesktopWorkArea.Right - APosition.X then
      Result.Offset(-(NavBar.Width + ASize.cx), 0);
  end;

begin
  Result := TdxNavBarGroupPopupControlCalculator.CalculatePosition
    (ViewInfo, FRect);
  CorrectPosition(FRect.Size, Result);
end;

function TdxHamburgerMenuActiveGroupPopupControlViewInfo.GetClientRect: TRect;
begin
  Result := TdxNavBarGroupPopupControlCalculator.CalculateClientRect(FRect,
    GetBorderOffsets);
end;

function TdxHamburgerMenuActiveGroupPopupControlViewInfo.GetMaxHeight: Integer;
begin
  Result := TdxNavBarGroupPopupControlCalculator.CalculateMaxHeight(ViewInfo);
end;

function TdxHamburgerMenuActiveGroupPopupControlViewInfo.
  GetBorderOffsets: TRect;
begin
  Result := cxRect(2, 2, 2, 2);
end;

{ TdxHamburgerMenuAnimationController }

procedure TdxHamburgerMenuAnimationController.Animate(AExpand: Boolean);
begin
  FIsExpandMode := AExpand;
  InternalAnimate;
end;

function TdxHamburgerMenuAnimationController.AllowAnimation
  (AIsExpandMode: Boolean): Boolean;
begin
  Result := not PopupControl.IsAcrylic and AIsExpandMode and
    inherited AllowAnimation(AIsExpandMode);
end;

function TdxHamburgerMenuAnimationController.GetClientRectOffset: TPoint;
begin
  Result := cxPoint(PopupControl.InnerControlTopLeft.X,
    PopupControl.InnerControlTopLeft.Y);
end;

function TdxHamburgerMenuAnimationController.IsExpandMode: Boolean;
begin
  Result := FIsExpandMode;
end;

procedure TdxHamburgerMenuAnimationController.AfterAnimation;
begin
  DoFinish;
  inherited AfterAnimation;
end;

procedure TdxHamburgerMenuAnimationController.BeforeAnimation;
begin
  inherited BeforeAnimation;
  PopupControl.BeforeAnimation(IsExpandMode);
end;

procedure TdxHamburgerMenuAnimationController.HandleAnimationFinishStep;
begin
  inherited HandleAnimationFinishStep;
  DoFinish;
end;

procedure TdxHamburgerMenuAnimationController.HandleAnimationStep;
begin
  inherited HandleAnimationStep;
  PopupControl.HandleAnimationStep;
end;

procedure TdxHamburgerMenuAnimationController.ResumeAnimationTransition;
begin
  if IsExpandMode then
    AnimationTransition.Resume
  else
    AnimationTransition.ImmediateAnimation;
end;

function TdxHamburgerMenuAnimationController.GetAnimationBufferRect: TRect;
begin
  Result := cxRect(PopupControl.InnerControlTopLeft.X,
    PopupControl.InnerControlTopLeft.Y, PopupControl.InnerControl.Width,
    PopupControl.InnerControl.Height);
  Result := PopupControl.ClientRect;
end;

procedure TdxHamburgerMenuAnimationController.CalculateBounds(var ASourceRect,
  ATargetRect: TRect);
var
  ACollapsedWidth: Integer;
begin
  ATargetRect := PopupControl.ClientRect;
  ASourceRect := ATargetRect;
  ACollapsedWidth := 0;
  if GetMaster.OptionsBehavior.HamburgerMenu.DisplayMode <> dmOverlayMinimal
  then
    ACollapsedWidth := TdxNavBarViewInfoAccess(Control.ViewInfo)
      .GetNavBarCollapsedWidth;
  if IsExpandMode then
  begin
    if GetMaster.ViewInfo.GetExpandDirection = dirLeft then
      ASourceRect.Left := ASourceRect.Right
    else
      ASourceRect.Right := ASourceRect.Left;
  end
  else
  begin
    if GetMaster.ViewInfo.GetExpandDirection = dirLeft then
      ATargetRect.Left := ATargetRect.Right - ACollapsedWidth
    else
      ATargetRect.Right := ATargetRect.Left + ACollapsedWidth;
  end;
end;

function TdxHamburgerMenuAnimationController.GetMaster: TdxCustomNavBar;
begin
  Result := PopupControl.Master;
end;

function TdxHamburgerMenuAnimationController.GetPopupControl
  : TdxHamburgerMenuCustomPopupControl;
begin
  Result := inherited Control.Parent as TdxHamburgerMenuCustomPopupControl;
end;

procedure TdxHamburgerMenuAnimationController.DoFinish;
begin
  SetWindowRegion(PopupControl, TargetRect);
  PopupControl.AfterAnimation;
end;

{ TdxHamburgerMenuCustomPopupControl }

destructor TdxHamburgerMenuCustomPopupControl.Destroy;
begin
  FreeAndNil(FAnimationController);
  inherited Destroy;
end;

function TdxHamburgerMenuCustomPopupControl.GetAcrylicColor: TdxAlphaColor;
begin
  Result := TdxNavBarAccess(Master).GetAcrylicHostColor;
end;

function TdxHamburgerMenuCustomPopupControl.IsAcrylic: Boolean;
begin
  Result := TdxNavBarAccess(Master).IsAcrylicEnabled;
end;

procedure TdxHamburgerMenuCustomPopupControl.InitPopup;
begin
  inherited InitPopup;
  FInnerControlTopLeft.X := InnerControl.Left;
  FInnerControlTopLeft.Y := InnerControl.Top;
  ChangeInnerControlVisibility(False);
  if IsAcrylic then
    UpdateChildControls;
end;

procedure TdxHamburgerMenuCustomPopupControl.DoCloseUp;
begin
  Animate(False);
  inherited DoCloseUp;
end;

procedure TdxHamburgerMenuCustomPopupControl.CreateAnimationController;
begin
  AnimationController := TdxHamburgerMenuAnimationController.Create
    (InnerControl);
end;

function TdxHamburgerMenuCustomPopupControl.GetAnimationType
  : TdxNavBarHamburgerMenuPopupAnimationType;
begin
  Result := patFade;
end;

procedure TdxHamburgerMenuCustomPopupControl.DoCollapsed;
begin
  // do nothing
end;

procedure TdxHamburgerMenuCustomPopupControl.DoExpanded;
begin
  // do nothing
end;

procedure TdxHamburgerMenuCustomPopupControl.DoShowed;
begin
  CreateAnimationController;
  Animate(True);
  inherited DoShowed;
  if IsAcrylic then
    dxApplyAcrylicAccent(Handle, True, GetAcrylicColor);
end;

procedure TdxHamburgerMenuCustomPopupControl.Paint;
begin
  if not AnimationController.DrawAnimationBuffer(Canvas) then
    inherited Paint;
end;

function TdxHamburgerMenuCustomPopupControl.UseExpanding: Boolean;
begin
  Result := UseAnimation(patExpand);
end;

procedure TdxHamburgerMenuCustomPopupControl.AfterAnimation;
begin
  ChangeInnerControlVisibility(True);
  CollapseStateChanged(AnimationController.IsExpandMode);
end;

procedure TdxHamburgerMenuCustomPopupControl.BeforeAnimation
  (AIsExpandMode: Boolean);
begin
  AlphaBlend := False;
  AlphaBlend := True;
  if UseFading then
    AlphaBlendValue := IfThen(AIsExpandMode, 0, 255);
end;

procedure TdxHamburgerMenuCustomPopupControl.HandleAnimationStep;
begin
  if UseFading then
    AlphaBlendValue := AnimationController.CurrentAlpha;
  InvalidateWindow;
end;

function TdxHamburgerMenuCustomPopupControl.UseAnimation
  (AType: TdxNavBarHamburgerMenuPopupAnimationType): Boolean;
begin
  Result := GetAnimationType = AType;
end;

function TdxHamburgerMenuCustomPopupControl.UseFading: Boolean;
begin
  Result := UseAnimation(patFade);
end;

procedure TdxHamburgerMenuCustomPopupControl.WMEraseBkgnd
  (var Message: TWMEraseBkgnd);
begin
  if (AnimationController = nil) or (AnimationController <> nil) and
    not AnimationController.Active then
    inherited;
end;

procedure TdxHamburgerMenuCustomPopupControl.SetAnimationController
  (const AValue: TdxHamburgerMenuAnimationController);
begin
  FreeAndNil(FAnimationController);
  FAnimationController := AValue;
end;

procedure TdxHamburgerMenuCustomPopupControl.Animate(AIsExpandMode: Boolean);
begin
  AnimationController.Animate(AIsExpandMode);
end;

procedure TdxHamburgerMenuCustomPopupControl.ChangeInnerControlVisibility
  (AVisible: Boolean);
var
  ATopLeft: TPoint;
begin
  ATopLeft.X := IfThen(AVisible, InnerControlTopLeft.X, cxInvisibleCoordinate);
  ATopLeft.Y := IfThen(AVisible, InnerControlTopLeft.Y, cxInvisibleCoordinate);
  InnerControl.SetBounds(ATopLeft.X, ATopLeft.Y, InnerControl.Width,
    InnerControl.Height);
end;

procedure TdxHamburgerMenuCustomPopupControl.CollapseStateChanged
  (AIsExpandMode: Boolean);
begin
  if AIsExpandMode then
    DoExpanded
  else
    DoCollapsed;
end;

procedure TdxHamburgerMenuCustomPopupControl.InvalidateWindow;
begin
  if UseExpanding then
  begin
    SetWindowRegion(Self, AnimationController.CurrentBounds, roSet, False);
    cxRedrawWindow(Handle, RDW_INVALIDATE or RDW_ERASE or RDW_FRAME or
      RDW_ALLCHILDREN or RDW_UPDATENOW);
  end
  else
    Invalidate;
end;

procedure TdxHamburgerMenuCustomPopupControl.UpdateChildControls;
begin
  if not(csDesigning in ComponentState) then
    TdxAcrylicHostControlHelper.UpdateChildControls(Self, IsAcrylic, Self);
end;

{ TdxHamburgerMenuOverlayControl }

constructor TdxHamburgerMenuOverlayControl.Create(ANavBar: TdxCustomNavBar);
begin
  inherited Create(ANavBar);
  ParentBiDiMode := False;
  BiDiMode := ANavBar.BiDiMode;
  OwnerBounds := ANavBar.BoundsRect;
  OwnerParent := ANavBar.Parent;
end;

function TdxHamburgerMenuOverlayControl.CreateInnerControl: TdxCustomNavBar;
begin
  Result := TdxHamburgerMenuOverlayInnerControl.Create(Self);
end;

function TdxHamburgerMenuOverlayControl.CreatePopupViewInfo
  : TdxNavBarCustomPopupControlViewInfo;
begin
  Result := TdxHamburgerMenuOverlayViewInfo.Create(Master.ViewInfo);
end;

procedure TdxHamburgerMenuOverlayControl.CreateAnimationController;
begin
  if IsOverlayMinimal then
    AnimationController := TdxHamburgerMenuOverlayMinimalAnimationController.
      Create(InnerControl)
  else
    inherited CreateAnimationController;
end;

procedure TdxHamburgerMenuOverlayControl.BeforeAnimation(AIsExpandMode
  : Boolean);
begin
  SetWindowRegion(Handle, CreateRectRgnIndirect(cxEmptyRect), False);
  inherited BeforeAnimation(AIsExpandMode);
end;

function TdxHamburgerMenuOverlayControl.GetAnimationType
  : TdxNavBarHamburgerMenuPopupAnimationType;
begin
  Result := patExpand;
end;

procedure TdxHamburgerMenuOverlayControl.DoCollapsed;
begin
  SetOverlayVisible(False);
  TdxNavBarAccess(TdxNavBarAccess(Master)).DoNavigationPaneCollapsed;
end;

procedure TdxHamburgerMenuOverlayControl.DoExpanded;
begin
  SetOverlayVisible(True);
  TdxNavBarAccess(Master).DoNavigationPaneExpanded;
end;

function TdxHamburgerMenuOverlayControl.IsOverlayMinimal: Boolean;
begin
  Result := Master.OptionsBehavior.HamburgerMenu.DisplayMode = dmOverlayMinimal;
end;

procedure TdxHamburgerMenuOverlayControl.SetOverlayVisible(AValue: Boolean);
var
  AMaster: TdxNavBarAccess;
begin
  AMaster := TdxNavBarAccess(Master);
  TdxNavBarHamburgerMenuBehaviorOptionsAccess
    (AMaster.OptionsBehavior.HamburgerMenu).SetOverlayVisible(AValue);
end;

{ TdxHamburgerMenuOverlayViewInfo }

procedure TdxHamburgerMenuOverlayViewInfo.CalculateBounds
  (AClientWidth: Integer);
var
  ANavBar: TdxNavBarAccess;
  AWidth, AHeight: Integer;
begin
  TdxHamburgerHelper.CheckWidth(AClientWidth);
  AWidth := AClientWidth;
  AHeight := GetMaxHeight;
  ANavBar := TdxNavBarAccess(NavBar);
  if ANavBar.OptionsBehavior.HamburgerMenu.DisplayMode = dmOverlayMinimal then
  begin
    AHeight := ANavBar.OriginalHeight;
    if Assigned(ANavBar.OnGetOverlaySize) then
      ANavBar.OnGetOverlaySize(NavBar, AWidth, AHeight);
  end;
  FRect := cxRectSetWidth(FRect, AWidth);
  FRect := cxRectSetHeight(FRect, AHeight);
end;

{ TdxHamburgerActiveGroupMapper }

procedure TdxHamburgerActiveGroupMapper.CloneMasterOptions;
begin
  inherited CloneMasterOptions;
  NavBar.OptionsView.HamburgerMenu := Master.OptionsView.HamburgerMenu;
end;

{ TdxHamburgerActiveGroupInnerControl }

function TdxHamburgerActiveGroupInnerControl.GetMapperClass
  : TdxNavBarCloneMapperClass;
begin
  Result := TdxHamburgerActiveGroupMapper;
end;

{ TdxHamburgerMenuActiveGroupPopupControl }

function TdxHamburgerMenuActiveGroupPopupControl.CreateInnerControl
  : TdxCustomNavBar;
begin
  Result := TdxHamburgerActiveGroupInnerControl.Create(Self);
end;

function TdxHamburgerMenuActiveGroupPopupControl.CreatePopupViewInfo
  : TdxNavBarCustomPopupControlViewInfo;
begin
  if Master.OptionsBehavior.HamburgerMenu.SelectedGroupPopupMode = pmDocked then
    Result := TGroupOverlayViewInfo.Create(Master.ViewInfo)
  else
  begin
    if (Master.Painter as TdxNavBarHamburgerMenuPainter).IsSkinAvailable then
      Result := TdxHamburgerMenuSelectedGroupPopupControlViewInfo.Create
        (Master.ViewInfo)
    else
      Result := TdxHamburgerMenuActiveGroupPopupControlViewInfo.Create
        (Master.ViewInfo);
  end;
end;

function TdxHamburgerMenuActiveGroupPopupControl.GetAnimationType
  : TdxNavBarHamburgerMenuPopupAnimationType;
begin
  Result := patFade;
end;

procedure TdxHamburgerMenuActiveGroupPopupControl.CreateAnimationController;
begin
  AnimationController := TdxHamburgerMenuActiveGroupAnimationController.Create
    (InnerControl);
end;

procedure TdxHamburgerMenuActiveGroupPopupControl.Paint;
begin
  inherited Paint;
  if Master.OptionsBehavior.HamburgerMenu.SelectedGroupPopupMode = pmUndocked
  then
    TdxNavBarHamburgerMenuPainter(Master.Painter).DrawPopupControl(Canvas,
      PopupViewInfo);
end;

{ TdxHamburgerMenuNavPaneCustomLayoutCalculator }

class procedure TdxHamburgerMenuNavPaneCustomLayoutCalculator.Calculate
  (APanel: TdxNavBarHamburgerMenuNavPaneViewInfo; const ABounds: TRect);

  function GetLayoutCalculator: TdxHamburgerLayoutCalculatorClass;
  begin
    if FPanel.UseCompactLayout then
      Result := TdxHamburgerHorizontalLayoutCalculator
    else
      Result := TdxHamburgerVerticalLayoutCalculator
  end;

  function GetLayoutPattern: TdxNavBarGroupViewInfo;
  begin
    if FPanel.UseCompactLayout then
      Result := FPanel.CompactItemPattern
    else
      Result := FPanel.ItemPattern;
  end;

begin
  FPanel := APanel;
  if FPanel.Items.Count > 0 then
  begin
    FCompactItemSize.cx := GetItemWidth;
    FCompactItemSize.cy := GetItemHeight;
    FItemsSize.cx := Panel.Items.Count * FCompactItemSize.cx;
    FItemsSize.cy := Panel.Items.Count * FCompactItemSize.cy;

    CalculateRect(ABounds);
    CalculateOverflowPanelRect;
    CalculateItems(GetLayoutCalculator, GetLayoutPattern, GetItemsRect,
      Panel.Items, Panel.VisibleItems, Panel.HiddenItems);

    if FPanel.VisibleItems.Count > 0 then
      AlignItems
    else if IsOverflowPanelVisible then
      SetPanelRectTop(Panel.FOverflowPanelRect.Top)
    else
      SetPanelRectTop(Panel.FClientRect.Bottom);
    CalculateOverflowPanelItems;
  end
end;

class function TdxHamburgerMenuNavPaneCustomLayoutCalculator.
  GetItemsRect: TRect;
begin
  Result := FPanel.FClientRect;
end;

class function TdxHamburgerMenuNavPaneCustomLayoutCalculator.
  IsEnoughSpace: Boolean;
begin
  Result := ItemsSize.cy <= Panel.FClientRect.Height;
end;

class procedure TdxHamburgerMenuNavPaneCustomLayoutCalculator.
  DoCalculateOverflowPanelRect;
begin
  Panel.FOverflowPanelRect := Panel.FClientRect;
  Panel.FOverflowPanelRect.Top :=
    Max(Panel.FOverflowPanelRect.Bottom - GetOverflowPanelSignSize.cy,
    Panel.FClientRect.Top);
  Panel.FOverflowPanelRect.Left := Panel.FOverflowPanelRect.Right;
end;

class function TdxHamburgerMenuNavPaneCustomLayoutCalculator.
  GetOverflowPanelSignSize: TSize;
begin
  Result := CompactItemSize;
  Result.cy := Result.cy + Panel.ViewInfo.BorderWidth;
end;

class function TdxHamburgerMenuNavPaneCustomLayoutCalculator.
  IsOverflowPanelVisible: Boolean;
begin
  Result := not Panel.FOverflowPanelRect.IsEmpty;
end;

class procedure TdxHamburgerMenuNavPaneCustomLayoutCalculator.OffsetItems
  (AOffsetY: Integer);
var
  AItem: TdxNavBarHamburgerMenuNavPaneCustomItem;
  AOffset: TPoint;
begin
  AOffset.X := 0;
  AOffset.Y := IfThen(FPanel.AllowHideItems or (AOffsetY >= 0), AOffsetY, 0);
  if not AOffset.IsZero then
    for AItem in FPanel.VisibleItems do
      AItem.Offset(AOffset);
end;

class procedure TdxHamburgerMenuNavPaneCustomLayoutCalculator.SetPanelRectTop
  (AValue: Integer);
var
  ABottom: Integer;
begin
  Panel.FClientRect.Top := Max(AValue, GetMaxTopPos);
  Panel.FClientRect.Height := GetRectHeight;

  ABottom := Panel.FRect.Bottom;
  Panel.FRect := Panel.FClientRect;
  Panel.FRect.Top := Max(Panel.ViewInfo.HeaderRect.Bottom,
    Panel.FRect.Top - GetSkinElementOffsets
    (Panel.ViewInfo.SkinHelper.NavPaneOverflowPanel, Panel.ScaleFactor).Top);
  Panel.FRect.Bottom := ABottom;
end;

class function TdxHamburgerMenuNavPaneCustomLayoutCalculator.
  GetItemHeight: Integer;
begin
  Result := FPanel.ActualItemPattern.Rect.Height;
end;

class function TdxHamburgerMenuNavPaneCustomLayoutCalculator.
  GetItemWidth: Integer;
begin
  Result := FPanel.CompactItemPattern.Rect.Width;
end;

class function TdxHamburgerMenuNavPaneCustomLayoutCalculator.
  GetMaxTopPos: Integer;
begin
  Result := Panel.ViewInfo.GroupsArea.Top;
end;

class procedure TdxHamburgerMenuNavPaneCustomLayoutCalculator.CalculateItems
  (ACalculator: TdxHamburgerLayoutCalculatorClass;
  AItemPattern: TdxNavBarGroupViewInfo; const ARect: TRect;
  AItems, AVisibleItems, AHiddenItems: TdxNavBarHamburgerMenuNavPaneItemList);
var
  I: Integer;
begin
  ACalculator.Calculate(AItemPattern as TdxHamburgerNavPaneDefaultItemPattern,
    ARect, AItems, AVisibleItems);
  AHiddenItems.AddRange(AItems);
  for I := 0 to AVisibleItems.Count - 1 do
    AHiddenItems.Remove(AVisibleItems[I]);
end;

class procedure TdxHamburgerMenuNavPaneCustomLayoutCalculator.CalculateRect
  (const ABounds: TRect);
var
  AClientOffset: TRect;
begin
  Panel.FRect := ABounds;
  if Panel.AllowHideItems then
    Inc(Panel.FRect.Top, Panel.FRect.Height div 2);

  Panel.FClientRect := Panel.Rect;
  AClientOffset := GetSkinElementOffsets
    (Panel.ViewInfo.SkinHelper.NavPaneOverflowPanel, Panel.ScaleFactor);
  Inc(Panel.FClientRect.Top, AClientOffset.Top);
  Dec(Panel.FClientRect.Bottom, AClientOffset.Bottom);
end;

class procedure TdxHamburgerMenuNavPaneCustomLayoutCalculator.
  CalculateOverflowPanelItems;

  function GetOverflowPanelSignRect(AItemCount: Integer): TRect;
  begin
    Result.Empty;
    if (AItemCount * CompactItemSize.cx > Panel.FOverflowPanelRect.Width -
      GetOverflowPanelSignSize.cx) or Panel.AllowCustomizing then
      Result := cxRectCenter(Panel.FOverflowPanelRect,
        GetOverflowPanelSignSize);
  end;

var
  AItemsRect, ASignRect: TRect;
  AHiddenItems, AVisibleItems: TdxNavBarHamburgerMenuNavPaneItemList;
begin
  if not Panel.FOverflowPanelRect.IsEmpty then
  begin
    AHiddenItems := TdxNavBarHamburgerMenuNavPaneItemList.Create;
    AVisibleItems := TdxNavBarHamburgerMenuNavPaneItemList.Create;
    try
      AItemsRect := FPanel.FOverflowPanelRect;
      ASignRect := GetOverflowPanelSignRect(FPanel.HiddenItems.Count);
      if not ASignRect.IsEmpty then
      begin
        Dec(ASignRect.Top,
          GetSkinElementOffsets(Panel.ViewInfo.SkinHelper.NavPaneOverflowPanel,
          Panel.ScaleFactor).Left);
        FPanel.CalculateSignRect(ASignRect);
        AItemsRect.Right := ASignRect.Left;
      end;

      AHiddenItems.AddRange(FPanel.HiddenItems);
      FPanel.HiddenItems.Clear;
      CalculateItems(TdxHamburgerMenuOverflowPanelLayoutCalculator,
        FPanel.CompactItemPattern, AItemsRect, AHiddenItems, AVisibleItems,
        FPanel.HiddenItems);
      FPanel.VisibleItems.AddRange(AVisibleItems);

      PopulateOverflowPanel(FPanel.HiddenItems);
    finally
      AVisibleItems.Free;
      AHiddenItems.Free;
    end;
  end;
end;

class procedure TdxHamburgerMenuNavPaneCustomLayoutCalculator.
  CalculateOverflowPanelRect;
begin
  if FPanel.AllowHideItems and not IsEnoughSpace then
  begin
    DoCalculateOverflowPanelRect;
    if Panel.FOverflowPanelRect.Height < GetOverflowPanelSignSize.cy then
    begin
      Panel.FOverflowPanelRect.Top :=
        Max(Panel.FOverflowPanelRect.Bottom - GetOverflowPanelSignSize.cy,
        GetMaxTopPos);
      Panel.FOverflowPanelRect.Height := GetOverflowPanelSignSize.cy;
    end;
  end
  else
    Panel.FOverflowPanelRect.Empty;
end;

class procedure TdxHamburgerMenuNavPaneCustomLayoutCalculator.
  PopulateOverflowPanel(AHiddenItems: TdxNavBarHamburgerMenuNavPaneItemList);
var
  AHiddenItem: TdxNavBarHamburgerMenuNavPaneCustomItem;
  I: Integer;
begin
  for I := AHiddenItems.Count - 1 downto 0 do
  begin
    AHiddenItem := AHiddenItems[I];
    if AHiddenItem is TdxNavBarHamburgerMenuNavPaneLink then
      FPanel.HideLink(TdxNavBarHamburgerMenuNavPaneLink(AHiddenItem).Link)
    else
      FPanel.HideGroup(AHiddenItem.GroupViewInfo.Group);
  end;
end;

{ TdxHamburgerMenuNavPaneDefaultLayoutCalculator }

class function TdxHamburgerMenuNavPaneDefaultLayoutCalculator.
  GetItemsRect: TRect;
begin
  Result := inherited GetItemsRect;
  if IsOverflowPanelVisible then
    Dec(Result.Bottom, GetOverflowPanelSignSize.cy);
end;

class function TdxHamburgerMenuNavPaneDefaultLayoutCalculator.
  GetRectHeight: Integer;
begin
  Result := ItemsSize.cy;
  if IsOverflowPanelVisible then
    Inc(Result, GetOverflowPanelSignSize.cy);
end;

class procedure TdxHamburgerMenuNavPaneDefaultLayoutCalculator.AlignItems;
var
  ATopPos: Integer;
begin
  if IsOverflowPanelVisible then
    ATopPos := Max(Panel.FClientRect.Top, Panel.FClientRect.Bottom -
      GetOverflowPanelSignSize.cy)
  else
    ATopPos := Max(Panel.FClientRect.Top, Panel.FClientRect.Bottom);
  OffsetItems(ATopPos - Panel.VisibleItems.Last.Rect.Bottom);
  SetPanelRectTop(Panel.VisibleItems.First.Rect.Top);
end;

class procedure TdxHamburgerMenuNavPaneDefaultLayoutCalculator.
  DoCalculateOverflowPanelRect;
begin
  inherited DoCalculateOverflowPanelRect;
  Panel.FOverflowPanelRect.Left := Panel.FClientRect.Left;
end;

{ TdxHamburgerMenuNavPaneCompactLayoutCalculator }

class function TdxHamburgerMenuNavPaneCompactLayoutCalculator.
  GetItemsRect: TRect;
begin
  Result := inherited GetItemsRect;
  Result.Right := Result.Right - Panel.FOverflowPanelRect.Width;
  Result.Height := GetRectHeight;
end;

class function TdxHamburgerMenuNavPaneCompactLayoutCalculator.
  GetRectHeight: Integer;
begin
  Result := CompactItemSize.cy;
end;

class function TdxHamburgerMenuNavPaneCompactLayoutCalculator.
  IsEnoughSpace: Boolean;
begin
  Result := ItemsSize.cx <= Panel.FClientRect.Width;
end;

class procedure TdxHamburgerMenuNavPaneCompactLayoutCalculator.AlignItems;
var
  ATopPos, AHeightDifference: Integer;
begin
  ATopPos := Panel.FClientRect.Bottom - CompactItemSize.cy;
  AHeightDifference := Panel.FClientRect.Height - GetRectHeight +
    IfThen(GetMaxTopPos > ATopPos, GetMaxTopPos - ATopPos);
  OffsetItems(AHeightDifference);
  SetPanelRectTop(ATopPos);
end;

class procedure TdxHamburgerMenuNavPaneCompactLayoutCalculator.
  DoCalculateOverflowPanelRect;
begin
  inherited DoCalculateOverflowPanelRect;
  Panel.FOverflowPanelRect.Left := Panel.FOverflowPanelRect.Right -
    GetOverflowPanelSignSize.cx;
end;

{ TdxHamburgerMenuOverlayMinimalAnimationController }

function TdxHamburgerMenuOverlayMinimalAnimationController.
  GetAnimationBufferRect: TRect;
begin
  Result := TargetRect;
  if Control.ViewInfo.GetExpandDirection = dirRight then
  begin
    Result.Right := AnimationTransition.CurrentRect.Right;
    Result.Left := Result.Right - TargetRect.Width;
  end
  else
  begin
    Result.Left := AnimationTransition.CurrentRect.Left;
    Result.Width := AnimationTransition.CurrentRect.Width;
  end;
end;

procedure TdxHamburgerMenuOverlayMinimalAnimationController.HandleAnimationStep;
begin
  SetWindowRegion(PopupControl, GetAnimationBufferRect, roSet, False);
  cxRedrawWindow(PopupControl.Handle, RDW_INVALIDATE or RDW_ERASE or
    RDW_FRAME or RDW_ALLCHILDREN or RDW_UPDATENOW);
end;

{ TGroupOverlayViewInfo }

function TGroupOverlayViewInfo.CalculatePosition: TPoint;
var
  AViewInfo: TdxNavBarHamburgerMenuViewInfo;
begin
  AViewInfo := NavBar.ViewInfo as TdxNavBarHamburgerMenuViewInfo;
  if Painter.ViewInfo.GetExpandDirection = dirLeft then
    Result := NavBar.ClientToScreen
      (cxPoint(AViewInfo.ClientRect.Right - AViewInfo.GetNavBarCollapsedWidth -
      TdxNavBarAccess(NavBar).OriginalWidth + AViewInfo.GetGroupEdges.X, 0))
  else
    Result := NavBar.ClientToScreen
      (cxPoint(AViewInfo.ClientRect.Right - AViewInfo.GetGroupEdges.X, 0));
end;

{ TdxHamburgerMenuActiveGroupAnimationController }

function TdxHamburgerMenuActiveGroupAnimationController.GetExpandAnimationTime
  : Cardinal;
begin
  Result := 150;
end;

{ TdxHamburgerMenuSelectedGroupPopupControlViewInfo }

function TdxHamburgerMenuSelectedGroupPopupControlViewInfo.
  GetBorderOffsets: TRect;
var
  AElement: TdxSkinElement;
begin
  AElement := GetSkinHelper.NavPanePopupControl;
  if AElement <> nil then
    if not AElement.Image.Empty then
      Result := AElement.Image.Margins.Margin
    else
      with AElement.Borders do
        Result := cxRect(Left.Thin, Top.Thin, Right.Thin, Bottom.Thin)
  else
    Result := inherited GetBorderOffsets;
end;

function TdxHamburgerMenuSelectedGroupPopupControlViewInfo.GetSkinHelper
  : TdxNavBarSkinBasedPainterHelper;
begin
  Result := Painter.SkinHelper;
end;

{ TdxNavBarHamburgerMenuController }

destructor TdxNavBarHamburgerMenuController.Destroy;
begin
  FreeAndNil(FOverlayControl);
  inherited Destroy;
end;

procedure TdxNavBarHamburgerMenuController.ClosePopupControl;
begin
  InternalClosePopupControl;
end;

procedure TdxNavBarHamburgerMenuController.ShowPopupControl;
begin
  InternalShowPopupControl(NavBar.ActiveGroup, dxNavBarPart(nbNone));
end;

function TdxNavBarHamburgerMenuController.AllowResize: Boolean;
begin
  Result := not IsOverlayMinimalMode or not(csAligning in NavBar.ControlState)
    and IsOverlayMinimalMode and (TdxNavBarAccess(NavBar).IsResizing or
    NavBar.IsLoading) or IsOverlayMinimalMode and TdxNavBarAccess(NavBar)
    .ExpandAnimationController.Active;
end;

function TdxNavBarHamburgerMenuController.CanHasPopupControl: Boolean;
begin
  Result := True;
end;

function TdxNavBarHamburgerMenuController.CreateGroupPopupControl
  : TdxNavBarCustomPopupControl;
begin
  Result := TdxHamburgerMenuActiveGroupPopupControl.Create(NavBar);
end;

function TdxNavBarHamburgerMenuController.GetActualNavBar: TdxCustomNavBar;
begin
  if IsOverlayControlExists and IsOverlayControlVisible then
    Result := TdxNavBarPopupControlAccess(FOverlayControl).InnerControl
  else
    Result := inherited GetActualNavBar;
end;

function TdxNavBarHamburgerMenuController.GetCollapsible: Boolean;
begin
  Result := not ViewInfo.IsInternal;
end;

function TdxNavBarHamburgerMenuController.GetPartAtPos(const APoint: TPoint)
  : TdxNavBarPart;
begin
  if ViewInfo.IsNavPaneVisible then
  begin
    Result := ViewInfo.NavPane.GetPartAtPos(APoint);
    if Result.MajorPartIndex <> nbNone then
      Exit;
  end;
  Result := inherited GetPartAtPos(APoint);
end;

function TdxNavBarHamburgerMenuController.GetRealCollapsed: Boolean;
begin
  Result := TdxNavBarHamburgerMenuBehaviorOptionsAccess
    (NavBar.OptionsBehavior.HamburgerMenu)
    .IsOverlayVisible xor NavBar.OptionsBehavior.HamburgerMenu.Collapsed;
end;

function TdxNavBarHamburgerMenuController.IsNavigationClient: Boolean;
begin
  Result := False;
end;

function TdxNavBarHamburgerMenuController.
  IsResetGroupMouseTrackingNeeded: Boolean;
begin
  Result := IsChildGroupActivated;
  if not Result then
    Result := FNavBar.ActiveGroup = FNavBar.HotTrackedGroup;
end;

function TdxNavBarHamburgerMenuController.
  IsResetLinkMouseTrackingNeeded: Boolean;
begin
  Result := IsChildGroupActivated;
  if not Result then
    Result := FNavBar.ActiveGroup = nil;
end;

function TdxNavBarHamburgerMenuController.
  NeedClosePopupControlsOnLinkClick: Boolean;
begin
  Result := False;
end;

function TdxNavBarHamburgerMenuController.NeedShowPopupControl: Boolean;
begin
  Result := inherited NeedShowPopupControl and
    ((ViewInfo.ActualSelectedPopupGroup.ChildCount > 0) or
    ViewInfo.ActualSelectedPopupGroup.UseControl and
    ViewInfo.ActualSelectedPopupGroup.ShowControl);
end;

procedure TdxNavBarHamburgerMenuController.BeginDragging;
begin
  inherited BeginDragging;
  ViewInfo.NavPane.RecreateGroupInfos;
end;

procedure TdxNavBarHamburgerMenuController.ChangeNavBarCollapseState;

  function CollapseOverlayControl: Boolean;
  var
    ANavBar: TdxNavBarAccess;
  begin
    ANavBar := TdxNavBarAccess(NavBar);
    Result := ANavBar.IsInternal and not Collapsed;
    if Result then
      (ANavBar.GetMasterNavBar.Controller as TdxNavBarHamburgerMenuController)
        .FOverlayControl.CloseUp;
  end;

begin
  if not CollapseOverlayControl then
    inherited ChangeNavBarCollapseState;
  ViewInfo.ResetScrollOffset;
end;

procedure TdxNavBarHamburgerMenuController.CheckMouseUp;
begin
  if CanDoGroupMouseUp then
    DoGroupMouseUp(NavBar.SourceGroup)
  else if CanDoLinkMouseUp then
    DoLinkMouseUp(NavBar.SourceLink);
end;

procedure TdxNavBarHamburgerMenuController.DestroyPopupControls;
begin
  if IsOverlayControlExists then
    FreeAndNil(FOverlayControl);
  inherited DestroyPopupControls;
end;

procedure TdxNavBarHamburgerMenuController.DoClick(const APart: TdxNavBarPart);
begin
  if not ViewInfo.NavPane.TryDoClick then
    inherited DoClick(APart);
end;

procedure TdxNavBarHamburgerMenuController.DoSetHotPart
  (const APart: TdxNavBarPart);
begin
  PartChanged;
end;

procedure TdxNavBarHamburgerMenuController.DoSetPressedPart
  (const APart: TdxNavBarPart);
begin
  PartChanged;
end;

procedure TdxNavBarHamburgerMenuController.EndDragging;
begin
  inherited EndDragging;
  ViewInfo.NavPane.RecreateGroupInfos;
end;

procedure TdxNavBarHamburgerMenuController.RequestAlign;
begin
  if IsOverlayMinimalMode then
    FIsChangingToMinimalState := False;
end;

procedure TdxNavBarHamburgerMenuController.SetCollapsed(AValue: Boolean);
begin
  NavBar.OptionsBehavior.HamburgerMenu.Collapsed := AValue;
end;

function TdxNavBarHamburgerMenuController.DoScrollDown: Boolean;
begin
  ScrollDown;
  Result := True;
end;

function TdxNavBarHamburgerMenuController.DoScrollUp: Boolean;
begin
  ScrollUp;
  Result := True;
end;

function TdxNavBarHamburgerMenuController.GetScrollButtonsTimerInterval
  : Integer;
begin
  Result := dxNavBarHamburgerMenuAutoScrollInterval;
end;

procedure TdxNavBarHamburgerMenuController.DoScrollDownByTimer;
begin
  ScrollDown;
end;

procedure TdxNavBarHamburgerMenuController.DoScrollUpByTimer;
begin
  ScrollUp;
end;

procedure TdxNavBarHamburgerMenuController.InternalCloseOverlay;
begin
  if not ViewInfo.IsInternal then
  begin
    if IsOverlayControlVisible then
      FOverlayControl.CloseUp;
  end
  else
    (NavBar.Parent as TdxNavBarCustomPopupControl).CloseUp;
end;

procedure TdxNavBarHamburgerMenuController.InternalShowOverlay;
var
  ALinkSelf: TcxObjectLink;
begin
  DestroyPopupControls;
  if not ViewInfo.IsInternal and CanShowOverlay then
  begin
    ALinkSelf := cxAddObjectLink(Self);
    try
      FOverlayControl := TdxHamburgerMenuOverlayControl.Create(FNavBar);
      FOverlayControl.Popup(nil);
    finally
      cxRemoveObjectLink(ALinkSelf);
    end;
  end
end;

procedure TdxNavBarHamburgerMenuController.InternalShowPopupControl
  (const ADroppedDownPart: TdxNavBarPart);
begin
  DestroyPopupControls;
  if not NavBar.IsDesigning then
    inherited InternalShowPopupControl(ADroppedDownPart);
end;

function TdxNavBarHamburgerMenuController.NeedResetPressedPartOnMouseUp
  : Boolean;
begin
  Result := not IsdxNavBarPartsEqual(PressedPart, dxNavBarPart(nbHeaderSign));
end;

procedure TdxNavBarHamburgerMenuController.InternalCheckHeight;
begin
  if IsOverlayMinimalMode and (NavBar.Height <> ViewInfo.HeaderRect.Height) then
  begin
    TdxNavBarAccess(NavBar).BeginResize;
    NavBar.Height := ViewInfo.HeaderRect.Height;
    TdxNavBarAccess(NavBar).EndResize;
  end;
end;

procedure TdxNavBarHamburgerMenuController.InternalSetWidth
  (const AValue: Integer);
begin
  TdxNavBarAccess(NavBar).BeginResize;
  inherited InternalSetWidth(AValue);
  TdxNavBarAccess(NavBar).EndResize;
end;

function TdxNavBarHamburgerMenuController.CanShowOverlay: Boolean;
begin
  Result := not NavBar.IsDesigning and TdxNavBarAccess(NavBar).DoCanShowOverlay;
end;

function TdxNavBarHamburgerMenuController.GetViewInfo
  : TdxNavBarHamburgerMenuViewInfo;
begin
  Result := inherited ViewInfo as TdxNavBarHamburgerMenuViewInfo;
end;

function TdxNavBarHamburgerMenuController.IsChildGroupActivated: Boolean;
begin
  Result := ViewInfo.ActualSelectedPopupGroup = NavBar.HotTrackedGroup;
end;

function TdxNavBarHamburgerMenuController.IsOverlayControlExists: Boolean;
begin
  Result := FOverlayControl <> nil;
end;

function TdxNavBarHamburgerMenuController.IsOverlayControlVisible: Boolean;
begin
  Result := IsOverlayControlExists and FOverlayControl.Visible;
end;

function TdxNavBarHamburgerMenuController.IsOverlayMinimalMode: Boolean;
begin
  Result := NavBar.OptionsBehavior.HamburgerMenu.DisplayMode = dmOverlayMinimal;
end;

procedure TdxNavBarHamburgerMenuController.DoScroll(ADirection: TcxDirection);
const
  Delta: array [TcxDirection] of Integer = (0, dxNavBarHamburgerMenuScrollDelta,
    -dxNavBarHamburgerMenuScrollDelta, dxNavBarHamburgerMenuScrollDelta,
    -dxNavBarHamburgerMenuScrollDelta);
begin
  ViewInfo.UpdateScrollOffset(Delta[ADirection]);
end;

procedure TdxNavBarHamburgerMenuController.PartChanged;
begin
  if ViewInfo.IsHeaderVisible then
    InvalidateAll(doRedraw);
end;

procedure TdxNavBarHamburgerMenuController.ScrollDown;
begin
  DoScroll(dirDown);
end;

procedure TdxNavBarHamburgerMenuController.ScrollUp;
begin
  DoScroll(dirUp);
end;

{ TdxNavBarHamburgerMenuHeaderPanelViewInfo }

function TdxNavBarHamburgerMenuHeaderPanelViewInfo.CanDrawSign: Boolean;
var
  AIsInternal: Boolean;
begin
  AIsInternal := ViewInfo.IsInternal;
  Result := not AIsInternal or AIsInternal and
    (ViewInfo.GetMasterPopupGroup = nil);
end;

function TdxNavBarHamburgerMenuHeaderPanelViewInfo.CanDrawText: Boolean;
begin
  Result := GetText <> '';
end;

function TdxNavBarHamburgerMenuHeaderPanelViewInfo.GetHeight: Integer;
var
  AOffset: TRect;
  AHeightAddon, ACaptionImageIndent: Integer;
begin
  ACaptionImageIndent := ScaleFactor.Apply(ViewInfo.GetGroupCaptionImageIndent);
  AOffset := GetSkinElementOffsets(ViewInfo.SkinHelper.HamburgerButton,
    ScaleFactor);
  AHeightAddon := Max(AOffset.Top, ACaptionImageIndent) +
    Max(AOffset.Bottom, ACaptionImageIndent);
  Result := ScaleFactor.Apply(Max(GetSignHeight, 32)) + AHeightAddon div 2;
  dxAdjustToTouchableSize(Result, ScaleFactor);
end;

function TdxNavBarHamburgerMenuHeaderPanelViewInfo.GetText: string;
var
  AGroup: TdxNavBarGroup;
begin
  AGroup := ViewInfo.GetMasterPopupGroup;
  if AGroup <> nil then
    Result := AGroup.Caption
  else
    Result := inherited GetText;
end;

procedure TdxNavBarHamburgerMenuHeaderPanelViewInfo.CalculateRect(var X,
  Y: Integer);
begin
  inherited CalculateRect(X, Y);
  if ViewInfo.IsGroupShowing then
    Dec(FRect.Left, ViewInfo.GetGroupEdges.X);
end;

procedure TdxNavBarHamburgerMenuHeaderPanelViewInfo.CalculateSignRect;
var
  ASignHalfSize: Integer;
  AOffset: TRect;
begin
  FSignRect.Empty;

  ASignHalfSize := GetSignHeight div 2;
  FSignClientRect := Rect;
  SignClientRect.Width := ViewInfo.GetNavBarCollapsedWidth - Rect.Left;

  AOffset := GetSkinElementOffsets(ViewInfo.SkinHelper.HamburgerButton,
    ScaleFactor);
  FSignRect := cxRectContent(SignClientRect, cxRect(0, AOffset.Top, 0,
    AOffset.Bottom));
  FSignRect := cxRect(FSignRect.CenterPoint, FSignRect.CenterPoint);
  FSignRect.Inflate(ASignHalfSize, ASignHalfSize);

  if ViewInfo.GetExpandDirection = dirLeft then
  begin
    RTLConvert(FSignRect);
    RTLConvert(FSignClientRect);
  end;

  if not CanDrawSign then
  begin
    FSignClientRect.Empty;
    FSignRect.Left := 0;
    FSignRect.Width := 0;
  end;
end;

procedure TdxNavBarHamburgerMenuHeaderPanelViewInfo.CalculateTextRect;
begin
  FTextRect := ClientRect;
  FTextRect.Top := FSignRect.Top;
  FTextRect.Bottom := FSignRect.Bottom;
  FTextRect.Left := FSignRect.Right + ViewInfo.GetGroupCaptionImageIndent;
end;

procedure TdxNavBarHamburgerMenuHeaderPanelViewInfo.ClearRects;
begin
  inherited ClearRects;
  FSignClientRect.Empty;
end;

procedure TdxNavBarHamburgerMenuHeaderPanelViewInfo.DoCalculateBounds(var X,
  Y: Integer);

  procedure FlipRects;
  begin
    RTLConvert(FRect);
    RTLConvert(FTextRect);
  end;

begin
  inherited DoCalculateBounds(X, Y);
  if ViewInfo.GetExpandDirection = dirLeft then
    FlipRects;
  if NavBar.UseRightToLeftAlignment and ViewInfo.IsInternal then
    FlipRects;
end;

function TdxNavBarHamburgerMenuHeaderPanelViewInfo.GetSignHeight: Integer;
begin
  Result := 24;
end;

function TdxNavBarHamburgerMenuHeaderPanelViewInfo.GetViewInfo
  : TdxNavBarHamburgerMenuViewInfo;
begin
  Result := inherited ViewInfo as TdxNavBarHamburgerMenuViewInfo;
end;

{ TdxNavBarHamburgerMenuItemViewInfo }

function TdxNavBarHamburgerMenuItemViewInfo.FontColor: TColor;
begin
  Result := Style.Font.Color;
end;

function TdxNavBarHamburgerMenuItemViewInfo.GetDrawEdgeFlag: Integer;
begin
  Result := inherited GetDrawEdgeFlag;
  if NavBar.UseRightToLeftAlignment then
    Result := Result or DT_RIGHT;
end;

function TdxNavBarHamburgerMenuItemViewInfo.GetViewInfo
  : TdxNavBarHamburgerMenuViewInfo;
begin
  Result := inherited ViewInfo as TdxNavBarHamburgerMenuViewInfo;
end;

function TdxNavBarHamburgerMenuItemViewInfo.SelectionRect: TRect;
begin
  Result := Rect;
end;

{ TdxNavBarHamburgerMenuLinkViewInfo }

procedure TdxNavBarHamburgerMenuLinkViewInfo.CalculateAsTopGroupCaption(X,
  Y: Integer);
var
  AGroupViewInfo: TdxHamburgerNavPaneDefaultItemPattern;
begin
  AGroupViewInfo := TdxHamburgerNavPaneDefaultItemPattern.Create(ViewInfo,
    GroupViewInfo.Group, True, False);
  try
    AGroupViewInfo.FHasImage := HasImage;
    AGroupViewInfo.CalculateBounds(X, Y);
    if FImageRect.Width < AGroupViewInfo.ImageRect.Width then
      FImageRect.Offset(AGroupViewInfo.ImageRect.CenterPoint.X -
        FImageRect.CenterPoint.X, AGroupViewInfo.ImageRect.CenterPoint.Y -
        FImageRect.CenterPoint.Y)
    else
      FImageRect := AGroupViewInfo.ImageRect;
    FRect := AGroupViewInfo.Rect;
    FCaptionRect := AGroupViewInfo.CaptionTextRect;
    if not ViewInfo.IsCollapsed then
    begin
      Inc(FRect.Right, AGroupViewInfo.GetTextEdges.X);
      FCaptionRect.Right := FRect.Right;
    end;
  finally
    AGroupViewInfo.Free;
  end;
end;

procedure TdxNavBarHamburgerMenuLinkViewInfo.CalculateBounds(X, Y: Integer);

  function NeedCalculateAsTopLevelGroup: Boolean;
  begin
    Result := (ViewInfo.IsInternal or ViewInfo.IsPopupControlCalculation) and
      TdxNavBarGroupViewInfoAccess(GroupViewInfo).HasGroups;
  end;

begin
  if ViewInfo.IsVerticalItem(Self) then
  begin
    FRect := cxRect(X, Y, ViewInfo.ClientWidth - X, 0);
    ViewInfo.CalculateVerticalCaption(Caption, Font, FRect, FCaptionRect);
    ViewInfo.CalculateVerticalCaptionDesignRect(Self);
  end
  else
  begin
    inherited CalculateBounds(X, Y);
    if ((ViewInfo.IsNavPaneActiveGroup(GroupViewInfo) or
      ViewInfo.IsRootGroup(GroupViewInfo) or ViewInfo.IsPopupControlCalculation)
      and GroupViewInfo.Group.LinksUseSmallImages) then
    begin
      if NeedCalculateAsTopLevelGroup then
        CalculateAsTopGroupCaption(X, Y)
      else if not ViewInfo.IsGroupShowing and
        not ViewInfo.IsPopupControlCalculation or ViewInfo.IsGroupShowing and
        TdxNavBarGroupViewInfoAccess(GroupViewInfo).HasGroups and ViewInfo.IsPopupControlCalculation
      then
        CalculateAsTopGroupCaption(X, Y);
    end;
    if ViewInfo.IsCollapsed then
    begin
      if not GroupViewInfo.Group.LinksUseSmallImages then
      begin
        FRect.Height := FRect.Height - FCaptionRect.Height;
        FImageRect.Offset(FImageRect.CenterPoint.X -
          ViewInfo.GetNavBarCollapsedWidth div 2, 0);
      end;
      FCaptionRect.Empty;
    end;
    TdxNavBarItemCalculator.CalculateFocusRect(Self, Rect, Rect);
    TdxNavBarItemCalculator.CalculateDesignRect(Self, Rect);
  end;
end;

{ TdxNavBarHamburgerMenuGroupViewInfo }

function TdxNavBarHamburgerMenuGroupViewInfo.AcrylicForegroundColor: TColor;
begin
  Result := (Painter as TdxNavBarHamburgerMenuPainter)
    .GetRootGroupAcrylicForegroundColor(Self);
end;

function TdxNavBarHamburgerMenuGroupViewInfo.CaptionFontColor: TColor;
begin
  Result := CaptionFont.Color;
end;

procedure TdxNavBarHamburgerMenuGroupViewInfo.CalculateBounds(var X,
  Y: Integer);
var
  AOffset: Integer;
begin
  inherited CalculateBounds(X, Y);
  if ViewInfo.IsVerticalItem(Self) then
    ViewInfo.CalculateVerticalCaptionDesignRect(Self);
  if HasImage then
  begin
    AOffset := ViewInfo.GetNavBarCollapsedWidth div 2 -
      FImageRect.CenterPoint.X;
    FImageRect.Offset(AOffset, 0);
    Inc(FCaptionTextRect.Left, AOffset);
  end;
  if not Group.OptionsExpansion.ShowExpandButton then
    FCaptionTextRect.Right := Rect.Right;
end;

function TdxNavBarHamburgerMenuGroupViewInfo.GetControlRect: TRect;
var
  AClipRect: TRect;
  ANavBar: TdxNavBarAccess;
begin
  ANavBar := TdxNavBarAccess(NavBar);
  if ViewInfo.IsGroupShowing or ViewInfo.IsNavPaneActiveGroup(Self) then
    Result := TdxNavBarViewInfoAccess(ANavBar.ViewInfo).GroupsArea
  else
  begin
    Result := inherited GetControlRect;
    if NavBar.ScrollBar.Visible and NavBar.UseRightToLeftAlignment then
    begin
      AClipRect := ViewInfo.GetGroupControlClipRect;
      Result.Left := AClipRect.Left;
      Result.Right := AClipRect.Right;
    end;
  end;
end;

function TdxNavBarHamburgerMenuGroupViewInfo.GetState: TdxNavBarObjectStates;
begin
  Result := inherited GetState;
  if ViewInfo.IsNavPaneActiveGroup(Self) then
    Include(Result, sPressed);
end;

function TdxNavBarHamburgerMenuGroupViewInfo.IsCaptionCalculationNeeded
  : Boolean;
begin
  Result := inherited IsCaptionCalculationNeeded or ViewInfo.IsInternal;
end;

procedure TdxNavBarHamburgerMenuGroupViewInfo.CalculateCaptionImageRect(X,
  Y: Integer);
begin
  inherited CalculateCaptionImageRect(X, Y);
  if not HasImage then
  begin
    FImageRect.Empty;
    FCaptionTextRect.Left := GetImageIndent;
  end;
end;

procedure TdxNavBarHamburgerMenuGroupViewInfo.DoCalculateCaptionBounds(X,
  Y: Integer);
begin
  if ViewInfo.IsVerticalItem(Self) then
    ViewInfo.CalculateGroupVerticalCaption(Self)
  else
    inherited DoCalculateCaptionBounds(X, Y);
end;

procedure TdxNavBarHamburgerMenuGroupViewInfo.OffsetContent;
begin
  DoOffsetContent;
end;

function TdxNavBarHamburgerMenuGroupViewInfo.GetViewInfo
  : TdxNavBarHamburgerMenuViewInfo;
begin
  Result := inherited ViewInfo as TdxNavBarHamburgerMenuViewInfo;
end;

{ TdxNavBarHamburgerMenuChildGroupViewInfo }

constructor TdxNavBarHamburgerMenuChildGroupViewInfo.Create
  (AViewInfo: TdxNavBarViewInfo; AParentInfo: TdxNavBarGroupViewInfo;
  AGroup: TdxNavBarGroup; ACaptionVisible, AItemsVisible: Boolean);
var
  AHamburgerViewInfo: TdxNavBarHamburgerMenuViewInfo;
begin
  AHamburgerViewInfo := AViewInfo as TdxNavBarHamburgerMenuViewInfo;
  if not AHamburgerViewInfo.IsPopupControlCalculation then
    AItemsVisible := not AHamburgerViewInfo.IsCollapsed;
  inherited Create(AViewInfo, AParentInfo, AGroup, ACaptionVisible,
    AItemsVisible);
end;

function TdxNavBarHamburgerMenuChildGroupViewInfo.
  AcrylicForegroundColor: TColor;
begin
  if TopLevel then
    Result := (Painter as TdxNavBarHamburgerMenuPainter)
      .GetRootGroupAcrylicForegroundColor(Self)
  else
    Result := inherited AcrylicForegroundColor;
end;

function TdxNavBarHamburgerMenuChildGroupViewInfo.BgAlphaBlend: Byte;

  function FindParentGroupViewInfo: TdxNavBarGroupViewInfo;
  begin
    Result := CaptionInfo.GroupViewInfo;
    if Result <> nil then
      while Result is TdxNavBarHamburgerMenuChildGroupViewInfo do
        Result := TdxNavBarHamburgerMenuChildGroupViewInfo(Result)
          .CaptionInfo.GroupViewInfo;
  end;

var
  AParentGroup: TdxNavBarGroupViewInfo;
begin
  AParentGroup := FindParentGroupViewInfo;
  if AParentGroup <> nil then
    Result := AParentGroup.BgAlphaBlend
  else
    Result := inherited BgAlphaBlend;
end;

function TdxNavBarHamburgerMenuChildGroupViewInfo.CaptionStyle
  : TdxNavBarBaseStyle;
begin
  if sPressed in State then
    Result := NavBar.DefaultStyles.ChildGroupCaptionPressed
  else if sHotTracked in State then
    Result := NavBar.DefaultStyles.ChildGroupCaptionHotTracked
  else
    Result := NavBar.DefaultStyles.ChildGroupCaption;
end;

procedure TdxNavBarHamburgerMenuChildGroupViewInfo.CalculateBounds(var X,
  Y: Integer);
var
  AOffset: Integer;
begin
  inherited CalculateBounds(X, Y);
  if HasImage and IsTopLevel then
  begin
    AOffset := ViewInfo.GetNavBarCollapsedWidth div 2 -
      FImageRect.CenterPoint.X;
    FImageRect.Offset(AOffset, 0);
    if not ViewInfo.IsCollapsed then
    begin
      Inc(FCaptionTextRect.Left, AOffset);
      FCaptionTextRect.Right := Min(FCaptionTextRect.Right + AOffset,
        CaptionRect.Right);
    end;
  end;
  FExpandButtonRect.Offset(0, FCaptionRect.CenterPoint.Y -
    FExpandButtonRect.CenterPoint.Y);
  if not HasImage then
  begin
    FImageRect.Empty;
    Dec(FCaptionTextRect.Left, ViewInfo.GetGroupCaptionTextIndent +
      GetTextEdges.X);
  end;
end;

procedure TdxNavBarHamburgerMenuChildGroupViewInfo.CalculateCaptionBounds(X,
  Y: Integer);
var
  R: TRect;
begin
  if IsTopLevel or ViewInfo.IsPopupControlCalculation then
  begin
    TdxNavBarGroupCalculator.CalculateCaptionBounds(Self, X, Y);
    if not ViewInfo.IsCollapsed then
    begin
      R := FCaptionTextRect;
      cxScreenCanvas.Canvas.Font := CaptionFont;
      cxDrawText(cxScreenCanvas.Handle, Group.Caption, R, GetDrawEdgeFlag or
        DT_CALCRECT);
      cxScreenCanvas.Dormant;
      FCaptionTextRect.Width := R.Width;
    end;
  end
  else
    inherited CalculateCaptionBounds(X, Y);
  if ViewInfo.IsCollapsed then
  begin
    if ViewInfo.IsVerticalItem(Self) then
    begin
      ViewInfo.CalculateGroupVerticalCaption(Self);
      FCaptionRect := FRect;
      FCaptionTextRect := FCaptionRect;
      FImageRect.Empty;
    end
    else
      FCaptionTextRect.Empty;
    FCaptionSignRect.Empty;
    FExpandButtonRect.Empty;
  end;
end;

function TdxNavBarHamburgerMenuChildGroupViewInfo.
  GetExpandButtonAreaRight: Integer;
begin
  Result := GetExpandButtonOffset.cx + GetExpandButtonSize.cx +
    GetExpandButtonRightIndent;
  if HasImage then
  begin
    Inc(Result, FExpandButtonRect.Left);
    Dec(Result, ViewInfo.GetTextIndent);
    if CaptionRect.Left > 0 then
      Dec(Result, ViewInfo.GetItemCaptionOffsets.Left);
  end;
end;

function TdxNavBarHamburgerMenuChildGroupViewInfo.GetExpandButtonOffset: TSize;
var
  AElement: TdxSkinElement;
begin
  Result := cxNullSize;
  AElement := GetHelper.NavBarChildGroupExpandButton(True);
  if AElement <> nil then
    Result.cx := AElement.ContentOffset.Left;
end;

function TdxNavBarHamburgerMenuChildGroupViewInfo.
  GetExpandButtonRightIndent: Integer;
var
  AElement: TdxSkinElement;
begin
  AElement := GetHelper.NavBarChildGroupExpandButton(True);
  if AElement <> nil then
    Result := AElement.ContentOffset.Right
  else
    Result := 0;
end;

function TdxNavBarHamburgerMenuChildGroupViewInfo.GetImageIndent: Integer;
begin
  Result := ViewInfo.GetGroupCaptionImageIndent;
end;

function TdxNavBarHamburgerMenuChildGroupViewInfo.GetState
  : TdxNavBarObjectStates;
begin
  Result := inherited GetState;
  ViewInfo.CheckSelectedPopupGroupState(Self, Result);
end;

function TdxNavBarHamburgerMenuChildGroupViewInfo.GetLevel: Integer;
begin
  Result := inherited GetLevel;
  if ViewInfo.IsInternal and
    (NavBar.OptionsBehavior.HamburgerMenu.SelectedGroupPopupMode = pmDocked) or
    GetViewInfo.IsNavPaneVisible or ViewInfo.IsRootGroup(Group.Parent) then
    Result := Max(Result - 1, 0);
end;

function TdxNavBarHamburgerMenuChildGroupViewInfo.GetViewInfo
  : TdxNavBarHamburgerMenuViewInfo;
begin
  Result := inherited ViewInfo as TdxNavBarHamburgerMenuViewInfo;
end;

{ TdxNavBarHamburgerMenuNavPaneViewInfo }

constructor TdxNavBarHamburgerMenuNavPaneViewInfo.Create
  (AViewInfo: TdxNavBarViewInfo);
begin
  inherited Create(AViewInfo);
  FGroups := TObjectDictionary<TdxNavBarGroup, TdxNavBarGroupViewInfo>.Create
    ([doOwnsValues]);
  FItems := TObjectList<TdxNavBarHamburgerMenuNavPaneCustomItem>.Create;
  FVisibleItems := TdxNavBarHamburgerMenuNavPaneItemList.Create;

  FOverflowPanelViewInfo := Painter.GetOverflowPanelViewInfoClass.Create
    (ViewInfo);
  FOverflowPanelViewInfo.OnSignButtonClick :=
    OnOverflowPanelSignButtonClickHandler;

  FPopupMenu := Painter.GetOverflowPanelPopupMenuClass.Create
    (FOverflowPanelViewInfo);
  TdxNavBarOverflowPanelPopupMenuAccess(FPopupMenu).OnDrawItem :=
    Painter.OnDrawOverflowPanelPopupMenuItemHandler;
  FHiddenItems := TdxNavBarHamburgerMenuNavPaneItemList.Create;

  FItemIAccessibilityHelpers := TInterfaceList.Create;
end;

destructor TdxNavBarHamburgerMenuNavPaneViewInfo.Destroy;
begin
  SetItemIAccessibilityHelperCount(0);
  FreeAndNil(FItemIAccessibilityHelpers);
  NavBarAccessibleObjectOwnerObjectDestroyed(FSignIAccessibilityHelper);
  NavBarAccessibleObjectOwnerObjectDestroyed(FIAccessibilityHelper);
  FreeAndNil(FHiddenItems);
  FreeAndNil(FPopupMenu);
  FreeAndNil(FOverflowPanelViewInfo);
  FreeAndNil(FCompactItemPattern);
  FreeAndNil(FItemPattern);
  FreeAndNil(FVisibleItems);
  FreeAndNil(FItems);
  FreeAndNil(FGroups);
  inherited Destroy;
end;

procedure TdxNavBarHamburgerMenuNavPaneViewInfo.
  CheckItemIAccessibilityHelperCount;
begin
  SetItemIAccessibilityHelperCount(ItemCount);
end;

function TdxNavBarHamburgerMenuNavPaneViewInfo.GetGroupViewInfoAtItemsPos
  (const P: TPoint): TdxNavBarGroupViewInfo;
begin
  Result := GetGroupViewInfoAtPos(P);
  if (Result <> nil) and PtInRect(Result.CaptionRect, P) then
    Result := nil;
end;

function TdxNavBarHamburgerMenuNavPaneViewInfo.GetIAccessibilityHelper
  : IdxNavBarAccessibilityHelper;
begin
  if FIAccessibilityHelper = nil then
    FIAccessibilityHelper := NavBarGetAccessibilityHelper
      (TdxNavBarHamburgerMenuNavPaneIAccessibilityHelper.Create(Self, NavBar));
  Result := FIAccessibilityHelper;
end;

function TdxNavBarHamburgerMenuNavPaneViewInfo.GetActualItemPattern
  : TdxNavBarGroupViewInfo;
begin
  if UseCompactLayout then
    Result := FCompactItemPattern
  else
    Result := FItemPattern;
end;

function TdxNavBarHamburgerMenuNavPaneViewInfo.GetAllowCustomizing: Boolean;
begin
  Result := TdxNavBarHamburgerMenuBehaviorOptionsAccess
    (NavBar.OptionsBehavior.HamburgerMenu).AllowCustomizing;
end;

function TdxNavBarHamburgerMenuNavPaneViewInfo.GetAllowHideItems: Boolean;
begin
  Result := NavBar.OptionsView.HamburgerMenu.ShowOverflowPanel;
end;

function TdxNavBarHamburgerMenuNavPaneViewInfo.GetItemIAccessibilityHelper
  (AIndex: Integer): IdxNavBarAccessibilityHelper;
begin
  CheckItemIAccessibilityHelperCount;
  Result := FItemIAccessibilityHelpers[AIndex] as IdxNavBarAccessibilityHelper;
end;

function TdxNavBarHamburgerMenuNavPaneViewInfo.GetItemCount: Integer;
begin
  Result := FVisibleItems.Count;
end;

function TdxNavBarHamburgerMenuNavPaneViewInfo.GetItemDrawEdgeFlag: Cardinal;
begin
  Result := FItemPattern.GetDrawEdgeFlag;
end;

function TdxNavBarHamburgerMenuNavPaneViewInfo.GetItemFont: TFont;
begin
  Result := FItemPattern.CaptionFont;
end;

function TdxNavBarHamburgerMenuNavPaneViewInfo.GetItemTextColor: TColor;
begin
  Result := FItemPattern.CaptionFontColor;
end;

function TdxNavBarHamburgerMenuNavPaneViewInfo.GetPainter
  : TdxNavBarHamburgerMenuPainter;
begin
  Result := inherited Painter as TdxNavBarHamburgerMenuPainter;
end;

function TdxNavBarHamburgerMenuNavPaneViewInfo.GetPopupMenuImageIndent: Integer;
begin
  Result := ScaleFactor.Apply(3);
end;

function TdxNavBarHamburgerMenuNavPaneViewInfo.GetSignIAccessibilityHelper
  : IdxNavBarAccessibilityHelper;
begin
  if FSignIAccessibilityHelper = nil then
    FSignIAccessibilityHelper := NavBarGetAccessibilityHelper
      (TdxNavBarHamburgerMenuNavPaneSignAccessibilityHelper.Create
      (Self, NavBar));
  Result := FSignIAccessibilityHelper;
end;

function TdxNavBarHamburgerMenuNavPaneViewInfo.GetSignRect: TRect;
begin
  Result := OverflowPanelViewInfo.SignRect;
end;

function TdxNavBarHamburgerMenuNavPaneViewInfo.GetViewInfo
  : TdxNavBarHamburgerMenuViewInfo;
begin
  Result := inherited ViewInfo as TdxNavBarHamburgerMenuViewInfo;
end;

function TdxNavBarHamburgerMenuNavPaneViewInfo.GetVisibleItemCount: Integer;
begin
  Result := FVisibleItems.Count;
end;

procedure TdxNavBarHamburgerMenuNavPaneViewInfo.SetItemIAccessibilityHelperCount
  (Value: Integer);
var
  AIAccessibilityHelper: IdxNavBarAccessibilityHelper;
  I: Integer;
begin
  if FItemIAccessibilityHelpers.Count <> Value then
    if FItemIAccessibilityHelpers.Count < Value then
      for I := 1 to Value - FItemIAccessibilityHelpers.Count do
      begin
        AIAccessibilityHelper := NavBarGetAccessibilityHelper
          (GetItemAccessibilityHelperClass.Create(Self, NavBar));
        (AIAccessibilityHelper.GetHelper as
          TdxNavBarItemCollectionItemAccessibilityHelper).ItemIndex :=
          FItemIAccessibilityHelpers.Count;
        FItemIAccessibilityHelpers.Add(AIAccessibilityHelper);
      end
    else
      for I := FItemIAccessibilityHelpers.Count - 1 downto Value do
      begin
        AIAccessibilityHelper := FItemIAccessibilityHelpers[I]
          as IdxNavBarAccessibilityHelper;
        FItemIAccessibilityHelpers.Delete(I);
        NavBarAccessibleObjectOwnerObjectDestroyed(AIAccessibilityHelper);
      end;
end;

function TdxNavBarHamburgerMenuNavPaneViewInfo.GetItemsRect: TRect;
begin
  Result := FClientRect;
  if UseCompactLayout then
  begin
    Result.Right := FOverflowPanelRect.Left;
    Result.Bottom := Result.Top + ActualItemPattern.Rect.Height;
  end
  else
    Result.Bottom := FOverflowPanelRect.Top;
end;

function TdxNavBarHamburgerMenuNavPaneViewInfo.GetGroupViewInfoAtPos
  (const P: TPoint): TdxNavBarGroupViewInfo;
var
  I: Integer;
  AGroupViewInfo: TdxNavBarGroupViewInfo;
begin
  Result := nil;
  for I := 0 to FVisibleItems.Count - 1 do
    if PtInRect(FVisibleItems[I].Rect, P) then
    begin
      AGroupViewInfo := FVisibleItems[I].GroupViewInfo;
      if AGroupViewInfo <> nil then
      begin
        Result := TdxNavBarGroupViewInfoAccess(AGroupViewInfo)
          .GetGroupViewInfoAtPos(P);
        Break;
      end;
    end;
end;

function TdxNavBarHamburgerMenuNavPaneViewInfo.GetGroupViewInfoByGroup
  (AGroup: TdxNavBarGroup): TdxNavBarGroupViewInfo;
begin
  if not FGroups.TryGetValue(AGroup, Result) then
    Result := nil;
end;

function TdxNavBarHamburgerMenuNavPaneViewInfo.GetLinkViewInfoAtPos
  (const P: TPoint): TdxNavBarLinkViewInfo;
var
  I: Integer;
  AGroupViewInfo: TdxNavBarGroupViewInfo;
begin
  Result := nil;
  AGroupViewInfo := GetGroupViewInfoAtItemsPos(P);
  if AGroupViewInfo <> nil then
    for I := 0 to AGroupViewInfo.ItemCount - 1 do
      if PtInRect(AGroupViewInfo.Items[I].Rect, P) then
      begin
        Result := AGroupViewInfo.Items[I];
        Break;
      end;
end;

function TdxNavBarHamburgerMenuNavPaneViewInfo.GetLinkViewInfoByLink
  (ALink: TdxNavBarItemLink): TdxNavBarLinkViewInfo;
var
  I: Integer;
  AGroupViewInfo: TdxNavBarGroupViewInfo;
begin
  Result := nil;
  AGroupViewInfo := GetGroupViewInfoByGroup(ALink.Group);
  if AGroupViewInfo <> nil then
    for I := 0 to AGroupViewInfo.ItemCount - 1 do
      if AGroupViewInfo.Items[I].Link = ALink then
      begin
        Result := AGroupViewInfo.Items[I];
        Break;
      end;
end;

function TdxNavBarHamburgerMenuNavPaneViewInfo.GetItemAccessibilityHelperClass
  : TdxNavBarItemCollectionItemAccessibilityHelperClass;
begin
  Result := TdxNavBarHamburgerMenuNavPaneItemAccessibilityHelper;
end;

function TdxNavBarHamburgerMenuNavPaneViewInfo.
  GetItemIAccessibilityHelperCount: Integer;
begin
  CheckItemIAccessibilityHelperCount;
  Result := FItemIAccessibilityHelpers.Count;
end;

function TdxNavBarHamburgerMenuNavPaneViewInfo.GetItemImageSize: TSize;
begin
  Result.cx := FOverflowPanelViewInfo.GetImageWidth;
  Result.cy := FOverflowPanelViewInfo.GetImageHeight;
end;

function TdxNavBarHamburgerMenuNavPaneViewInfo.GetPartAtPos
  (const APoint: TPoint): TdxNavBarPart;
begin
  Result := FOverflowPanelViewInfo.GetPartAtPos(APoint);
end;

procedure TdxNavBarHamburgerMenuNavPaneViewInfo.CalculateItemsPattern;
begin
  TdxHamburgerNavPaneDefaultItemPattern(FCompactItemPattern)
    .Calculate(ViewInfo.GetMaxImageSize);
  TdxHamburgerNavPaneDefaultItemPattern(FItemPattern)
    .Calculate(ViewInfo.GetMaxImageSize);
  FOverflowPanelViewInfo.FItemWidth := FCompactItemPattern.Rect.Width;
end;

procedure TdxNavBarHamburgerMenuNavPaneViewInfo.ClearVisualElements;
begin
  FClientRect.Empty;
  FRect.Empty;
  FVisibleItems.Clear;
  FHiddenItems.Clear;

  FOverflowPanelRect.Empty;
  FOverflowPanelViewInfo.ClearItems;
  FOverflowPanelViewInfo.ClearRects;
end;

procedure TdxNavBarHamburgerMenuNavPaneViewInfo.HideGroup
  (AGroup: TdxNavBarGroup);
begin
  FOverflowPanelViewInfo.AddGroup(AGroup);
end;

procedure TdxNavBarHamburgerMenuNavPaneViewInfo.HideLink
  (AItemLink: TdxNavBarItemLink);
begin
  FOverflowPanelViewInfo.AddItemLink(AItemLink);
end;

procedure TdxNavBarHamburgerMenuNavPaneViewInfo.RecreateGroupInfos;
begin
  TdxNavBarAccess(NavBar).RecreateViewInfo;
end;

procedure TdxNavBarHamburgerMenuNavPaneViewInfo.ShowPopupMenu
  (const APosition: TPoint);
begin
  TdxNavBarOverflowPanelPopupMenuAccess(FPopupMenu).Show(APosition);
end;

procedure TdxNavBarHamburgerMenuNavPaneViewInfo.Calculate(const ABounds: TRect);
begin
  ClearVisualElements;
  FRect := cxRectSetTop(ABounds, ABounds.Bottom);
  if ViewInfo.IsNavPaneVisible and Assigned(FItemPattern) and
    Assigned(FCompactItemPattern) then
  begin
    CalculateItemsPattern;
    if UseCompactLayout then
      TdxHamburgerMenuNavPaneCompactLayoutCalculator.Calculate(Self, ABounds)
    else
      TdxHamburgerMenuNavPaneDefaultLayoutCalculator.Calculate(Self, ABounds);
  end;
end;

procedure TdxNavBarHamburgerMenuNavPaneViewInfo.CalculateSignRect
  (const ABounds: TRect);
begin
  OverflowPanelViewInfo.Calculate(ABounds);
end;

procedure TdxNavBarHamburgerMenuNavPaneViewInfo.Clear;
begin
  ClearVisualElements;
  FreeAndNil(FItemPattern);
  FreeAndNil(FCompactItemPattern);
  FGroups.Clear;
  FItems.Clear;
  if not ViewInfo.IsNavPaneVisible then
    CheckItemIAccessibilityHelperCount;
end;

procedure TdxNavBarHamburgerMenuNavPaneViewInfo.
  OnOverflowPanelSignButtonClickHandler(const AClientMousePos: TPoint);
begin
  TdxNavBarOverflowPanelPopupMenuAccess(FPopupMenu).Show(AClientMousePos);
end;

procedure TdxNavBarHamburgerMenuNavPaneViewInfo.CreateGroupInfos;

  procedure AddGroup(AGroup: TdxNavBarGroup;
    ATopAlignedItems, ABottomAlignedItems
    : TdxNavBarHamburgerMenuNavPaneItemList);

    procedure AddItem(AItem: TdxNavBarHamburgerMenuNavPaneCustomItem);
    begin
      if AGroup.Align = TcxAlignmentVert.vaTop then
        ATopAlignedItems.Add(AItem)
      else if not UseCompactLayout then
        ABottomAlignedItems.Add(AItem)
      else
        ABottomAlignedItems.Insert(0, AItem);
    end;

  var
    I: Integer;
    AGroupViewInfo: TdxNavBarGroupViewInfo;
    AIsSpecialRootGroup: Boolean;
    AChildItem: TdxNavBarCustomItemViewInfo;
  begin
    AIsSpecialRootGroup := ViewInfo.IsRootGroup(AGroup);
    AGroupViewInfo := TdxHamburgerNavPaneDefaultItemPattern.Create(ViewInfo,
      AGroup, True, AIsSpecialRootGroup);

    FreeAndNil(FItemPattern);
    FItemPattern := TdxHamburgerNavPaneDefaultItemPattern.Create(ViewInfo,
      AGroup, True, False);
    FreeAndNil(FCompactItemPattern);
    FCompactItemPattern := TdxHamburgerNavPaneCompactItemPattern.Create
      (ViewInfo, AGroup, True, False);

    if AIsSpecialRootGroup then
    begin
      if TdxNavBarAccess(NavBar).IsDragging and (AGroupViewInfo.Infos.Count = 0)
      then
        AddItem(TdxNavBarHamburgerMenuNavPaneEmptyGroup.Create(AGroupViewInfo))
      else
        for I := 0 to AGroupViewInfo.Infos.Count - 1 do
        begin
          AChildItem := AGroupViewInfo.Infos[I];
          if AChildItem is TdxNavBarLinkViewInfo then
            AddItem(TdxNavBarHamburgerMenuNavPaneLink.Create
              (TdxNavBarLinkViewInfo(AChildItem)));
        end
    end
    else
      AddItem(TdxNavBarHamburgerMenuNavPaneGroup.Create(AGroupViewInfo));
    FGroups.Add(AGroup, AGroupViewInfo);
  end;

var
  I: Integer;
  ABottomAlignedItems, ATopAlignedItems: TdxNavBarHamburgerMenuNavPaneItemList;
begin
  ABottomAlignedItems := TdxNavBarHamburgerMenuNavPaneItemList.Create;
  ATopAlignedItems := TdxNavBarHamburgerMenuNavPaneItemList.Create;
  try
    for I := 0 to NavBar.RootGroupCount - 1 do
      if NavBar.RootGroups[I].Visible then
        AddGroup(NavBar.RootGroups[I], ATopAlignedItems, ABottomAlignedItems);
    FItems.AddRange(ATopAlignedItems);
    FItems.AddRange(ABottomAlignedItems);
  finally
    ATopAlignedItems.Free;
    ABottomAlignedItems.Free;
  end;
end;

procedure TdxNavBarHamburgerMenuNavPaneViewInfo.DoRightToLeftConversion;
var
  I: Integer;
begin
  if not IsEmpty then
  begin
    for I := 0 to VisibleItems.Count - 1 do
      VisibleItems[I].DoRightToLeftConversion(Rect);
    TdxRightToLeftLayoutConverter.ConvertRect(FOverflowPanelRect, Rect);
    FOverflowPanelViewInfo.DoRightToLeftConversion;
  end;
end;

function TdxNavBarHamburgerMenuNavPaneViewInfo.Contains
  (AGroup: TdxNavBarGroup): Boolean;
begin
  Result := FGroups.ContainsKey(AGroup);
end;

function TdxNavBarHamburgerMenuNavPaneViewInfo.IsEmpty: Boolean;
begin
  Result := FItems.Count = 0;
end;

function TdxNavBarHamburgerMenuNavPaneViewInfo.TryDoClick: Boolean;
begin
  Result := FOverflowPanelViewInfo.TryDoClick(NavBar.Controller.HotPart);
end;

function TdxNavBarHamburgerMenuNavPaneViewInfo.UseCompactLayout: Boolean;
begin
  Result := (NavBar.OptionsView.HamburgerMenu.NavigationPaneMode = npmCompact)
    and not TdxNavBarControllerAccess(NavBar.Controller).Collapsed;
end;

{ TdxNavBarHamburgerMenuViewInfo }

constructor TdxNavBarHamburgerMenuViewInfo.Create(APainter: TdxNavBarPainter);
begin
  inherited Create(APainter);
  FCollapsedStateCalculator := TdxNavBarHamburgerMenuCollapsedStateCalculator.
    Create(Self);
  FNavPaneViewInfo := TdxNavBarHamburgerMenuNavPaneViewInfo.Create(Self);
  APainter.NavBar.IAccessibilityHelper.AttachChild(NavPaneIAccessibilityHelper);
end;

destructor TdxNavBarHamburgerMenuViewInfo.Destroy;
begin
  if not(csDestroying in NavBar.ComponentState) then
    NavBar.IAccessibilityHelper.DetachChild(NavPaneIAccessibilityHelper);
  FreeAndNil(FNavPaneViewInfo);
  FreeAndNil(FCollapsedStateCalculator);
  inherited Destroy;
end;

function TdxNavBarHamburgerMenuViewInfo.IsGroupActive
  (AGroup: TdxNavBarGroup): Boolean;
begin
  if not IsNavPaneVisible then
  begin
    if not TdxNavBarControllerAccess(NavBar.Controller).Collapsed or IsInternal
    then
      Result := inherited IsGroupActive(AGroup)
    else
      Result := AGroup = NavBar.ActiveGroup;
  end
  else
    Result := inherited;
end;

function TdxNavBarHamburgerMenuViewInfo.IsBottomScrollButtonVisible: Boolean;
begin
  Result := IsScrollButtonVisible(BottomScrollButtonRect);
end;

function TdxNavBarHamburgerMenuViewInfo.IsTopScrollButtonVisible: Boolean;
begin
  Result := IsScrollButtonVisible(TopScrollButtonRect);
end;

function TdxNavBarHamburgerMenuViewInfo.GetLinkViewInfoAtPos(const P: TPoint)
  : TdxNavBarLinkViewInfo;
begin
  if PtInRect(NavPane.Rect, P) then
    Result := NavPane.GetLinkViewInfoAtPos(P)
  else
    Result := inherited GetLinkViewInfoAtPos(P);
end;

function TdxNavBarHamburgerMenuViewInfo.GetLinkViewInfoAtSelectedPos
  (const P: TPoint): TdxNavBarLinkViewInfo;
begin
  if PtInRect(NavPane.Rect, P) then
    Result := InternalGetLinkViewInfoAtSelectedPos(Self, P)
  else
    Result := inherited GetLinkViewInfoAtSelectedPos(P);
end;

function TdxNavBarHamburgerMenuViewInfo.GetLinkViewInfoByLink
  (ALink: TdxNavBarItemLink): TdxNavBarLinkViewInfo;
begin
  Result := inherited GetLinkViewInfoByLink(ALink);
  if Result = nil then
    Result := NavPane.GetLinkViewInfoByLink(ALink);
end;

function TdxNavBarHamburgerMenuViewInfo.GetGroupViewInfoAtPos(const P: TPoint)
  : TdxNavBarGroupViewInfo;
begin
  if PtInRect(NavPane.Rect, P) then
    Result := NavPane.GetGroupViewInfoAtPos(P)
  else
    Result := inherited GetGroupViewInfoAtPos(P);
end;

function TdxNavBarHamburgerMenuViewInfo.GetGroupViewInfoByGroup
  (AGroup: TdxNavBarGroup): TdxNavBarGroupViewInfo;
begin
  Result := inherited GetGroupViewInfoByGroup(AGroup);
  if Result = nil then
    Result := NavPane.GetGroupViewInfoByGroup(AGroup);
end;

function TdxNavBarHamburgerMenuViewInfo.GetGripSize: Integer;
begin
  if IsInternal and not IsGroupPopupControlSizable then
    Result := 0
  else
    Result := inherited GetGripSize;
end;

procedure TdxNavBarHamburgerMenuViewInfo.DoRightToLeftConversion;
begin
  inherited DoRightToLeftConversion;
  NavPane.DoRightToLeftConversion;
end;

procedure TdxNavBarHamburgerMenuViewInfo.AssignDefaultBackgroundStyle;
begin
  inherited AssignDefaultBackgroundStyle;
  NavBar.DefaultStyles.Background.BackColor :=
    SkinHelper.NavBarBackground.Color;
  NavBar.DefaultStyles.Background.BackColor2 :=
    SkinHelper.NavBarBackground.Color;
end;

procedure TdxNavBarHamburgerMenuViewInfo.AssignDefaultGroupHeaderStyle;
begin
  inherited AssignDefaultGroupHeaderStyle;
  AssignFontStyle(GetHelper.NavBarGroupButtonCaption,
    NavBar.DefaultStyles.GroupHeader);
  NavBar.DefaultStyles.GroupHeader.HAlignment := haLeft;
end;

procedure TdxNavBarHamburgerMenuViewInfo.AssignDefaultGroupHeaderActiveStyle;
begin
  inherited AssignDefaultGroupHeaderActiveStyle;
  NavBar.DefaultStyles.GroupHeaderActive.Font.Assign
    (NavBar.DefaultStyles.GroupHeader.Font);
end;

procedure TdxNavBarHamburgerMenuViewInfo.
  AssignDefaultChildGroupCaptionHotTrackedStyle;
begin
  inherited AssignDefaultChildGroupCaptionHotTrackedStyle;
  AssignFontStyle(GetHelper.NavBarChildGroup,
    NavBar.DefaultStyles.ChildGroupCaptionHotTracked);
  NavBar.DefaultStyles.ChildGroupCaptionHotTracked.HAlignment := haLeft;
end;

procedure TdxNavBarHamburgerMenuViewInfo.
  AssignDefaultChildGroupCaptionPressedStyle;
begin
  inherited AssignDefaultChildGroupCaptionPressedStyle;
  AssignFontStyle(GetHelper.NavBarChildGroup,
    NavBar.DefaultStyles.ChildGroupCaptionPressed);
  NavBar.DefaultStyles.ChildGroupCaptionPressed.HAlignment := haLeft;
end;

procedure TdxNavBarHamburgerMenuViewInfo.AssignDefaultChildGroupCaptionStyle;
begin
  inherited AssignDefaultChildGroupCaptionStyle;
  AssignFontStyle(GetHelper.NavBarChildGroup,
    NavBar.DefaultStyles.ChildGroupCaption);
  NavBar.DefaultStyles.ChildGroupCaption.HAlignment := haLeft;
end;

procedure TdxNavBarHamburgerMenuViewInfo.AssignDefaultItemHotTrackedStyle;
begin
  inherited;
end;

procedure TdxNavBarHamburgerMenuViewInfo.AssignDefaultItemStyle;
begin
  inherited AssignDefaultItemStyle;
  AssignFontStyle(GetHelper.NavBarItem, NavBar.DefaultStyles.Item);
end;

procedure TdxNavBarHamburgerMenuViewInfo.AssignDefaultNavigationPaneHeaderStyle;
begin
  NavBar.DefaultStyles.NavigationPaneHeader.ResetValues;
  NavBar.DefaultStyles.NavigationPaneHeader.BackColor := clNone;
  NavBar.DefaultStyles.NavigationPaneHeader.BackColor2 := clNone;
  NavBar.DefaultStyles.NavigationPaneHeader.Font.Color := clNone;
  NavBar.DefaultStyles.NavigationPaneHeader.Font.Name := 'Arial';
  NavBar.DefaultStyles.NavigationPaneHeader.Font.Height := -15;
  NavBar.DefaultStyles.NavigationPaneHeader.Font.Style := [fsBold];
end;

function TdxNavBarHamburgerMenuViewInfo.HeaderBackColor: TColor;
begin
  if not IsDefaultHeaderColor then
    Result := inherited HeaderBackColor
  else
    Result := SkinHelper.NavBarBackground.Color;
end;

function TdxNavBarHamburgerMenuViewInfo.HeaderBackColor2: TColor;
begin
  if not IsDefaultHeaderColor then
    Result := inherited HeaderBackColor2
  else
    Result := SkinHelper.NavBarBackground.Color;
end;

function TdxNavBarHamburgerMenuViewInfo.HeaderFont: TFont;
begin
  Result := NavBar.DefaultStyles.GroupHeader.Font;
end;

function TdxNavBarHamburgerMenuViewInfo.HeaderFontColor: TColor;
var
  AElement: TdxSkinElement;
begin
  AElement := GetSkinHelper.NavBarGroupButtonCaption;
  if AElement <> nil then
    Result := AElement.TextColor
  else
    Result := inherited HeaderFontColor;
end;

function TdxNavBarHamburgerMenuViewInfo.OverflowPanelGradientMode
  : TdxBarStyleGradientMode;
begin
  if not IsDefaultOverflowPanelColor then
    Result := inherited OverflowPanelGradientMode
  else
    Result := TdxBarStyleGradientMode.gmVertical;
end;

function TdxNavBarHamburgerMenuViewInfo.BorderColor: TColor;
begin
  if IsSkinAvailable then
    Result := HeaderBackColor
  else
    Result := inherited BorderColor;
end;

function TdxNavBarHamburgerMenuViewInfo.BorderWidth: Integer;
begin
  Result := 0;
end;

function TdxNavBarHamburgerMenuViewInfo.BottomScrollButtonBackColor: TColor;
begin
  if not IsDefaultBottomScrollButtonColor then
    Result := inherited BottomScrollButtonBackColor
  else if sPressed in BottomScrollButtonState then
    Result := dxOffice11NavPaneGroupCaptionPressedColor1
  else if sActive in BottomScrollButtonState then
    Result := dxOffice11NavPaneGroupCaptionPressedColor2
  else if sHotTracked in BottomScrollButtonState then
    Result := dxOffice11NavPaneGroupCaptionHotColor1
  else
    Result := dxOffice11NavPaneGroupCaptionColor1;
end;

function TdxNavBarHamburgerMenuViewInfo.BottomScrollButtonBackColor2: TColor;
begin
  if not IsDefaultBottomScrollButtonColor then
    Result := inherited BottomScrollButtonBackColor2
  else if sPressed in BottomScrollButtonState then
    Result := dxOffice11NavPaneGroupCaptionPressedColor2
  else if sActive in BottomScrollButtonState then
    Result := dxOffice11NavPaneGroupCaptionPressedColor1
  else if sHotTracked in BottomScrollButtonState then
    Result := dxOffice11NavPaneGroupCaptionHotColor2
  else
    Result := dxOffice11NavPaneGroupCaptionColor2;
end;

function TdxNavBarHamburgerMenuViewInfo.BottomScrollButtonAlphaBlend: Byte;
begin
  if not IsDefaultBottomScrollButtonColor then
    Result := inherited BottomScrollButtonAlphaBlend
  else
    Result := 255;
end;

function TdxNavBarHamburgerMenuViewInfo.BottomScrollButtonAlphaBlend2: Byte;
begin
  if not IsDefaultBottomScrollButtonColor then
    Result := inherited BottomScrollButtonAlphaBlend2
  else
    Result := 255;
end;

function TdxNavBarHamburgerMenuViewInfo.BottomScrollButtonGradientMode
  : TdxBarStyleGradientMode;
begin
  if not IsDefaultBottomScrollButtonColor then
    Result := inherited BottomScrollButtonGradientMode
  else
    Result := TdxBarStyleGradientMode.gmVertical;
end;

function TdxNavBarHamburgerMenuViewInfo.TopScrollButtonBackColor: TColor;
begin
  if not IsDefaultTopScrollButtonColor then
    Result := inherited TopScrollButtonBackColor
  else if sPressed in TopScrollButtonState then
    Result := dxOffice11NavPaneGroupCaptionPressedColor1
  else if sActive in TopScrollButtonState then
    Result := dxOffice11NavPaneGroupCaptionPressedColor2
  else if sHotTracked in TopScrollButtonState then
    Result := dxOffice11NavPaneGroupCaptionHotColor1
  else
    Result := dxOffice11NavPaneGroupCaptionColor1;
end;

function TdxNavBarHamburgerMenuViewInfo.TopScrollButtonBackColor2: TColor;
begin
  if not IsDefaultTopScrollButtonColor then
    Result := inherited TopScrollButtonBackColor2
  else if sPressed in TopScrollButtonState then
    Result := dxOffice11NavPaneGroupCaptionPressedColor2
  else if sActive in TopScrollButtonState then
    Result := dxOffice11NavPaneGroupCaptionPressedColor1
  else if sHotTracked in TopScrollButtonState then
    Result := dxOffice11NavPaneGroupCaptionHotColor2
  else
    Result := dxOffice11NavPaneGroupCaptionColor2;
end;

function TdxNavBarHamburgerMenuViewInfo.TopScrollButtonAlphaBlend: Byte;
begin
  if not IsDefaultTopScrollButtonColor then
    Result := inherited TopScrollButtonAlphaBlend
  else
    Result := 255;
end;

function TdxNavBarHamburgerMenuViewInfo.TopScrollButtonAlphaBlend2: Byte;
begin
  if not IsDefaultTopScrollButtonColor then
    Result := inherited TopScrollButtonAlphaBlend2
  else
    Result := 255;
end;

function TdxNavBarHamburgerMenuViewInfo.TopScrollButtonGradientMode
  : TdxBarStyleGradientMode;
begin
  if not IsDefaultTopScrollButtonColor then
    Result := inherited TopScrollButtonGradientMode
  else
    Result := TdxBarStyleGradientMode.gmVertical;
end;

function TdxNavBarHamburgerMenuViewInfo.CanGroupCaptionBoundsByImage: Boolean;
begin
  Result := True;
end;

function TdxNavBarHamburgerMenuViewInfo.CanHasVisibleItemsInGroup
  (AGroup: TdxNavBarGroup): Boolean;
begin
  Result := inherited CanHasVisibleItemsInGroup(AGroup) or
    CanShowGroupItems(AGroup) or IsNavPaneActiveGroup(AGroup);
  if IsCollapsed then
    Result := not AGroup.ShowControl and
      (Result or (AGroup = GetController.SelectedPopupGroup) or
      CanShowGroupItems(AGroup));
end;

function TdxNavBarHamburgerMenuViewInfo.GetDefaultSmallImageWidth: Integer;
begin
  Result := ScaleFactor.Apply(dxNavBarDefaultSmallImageWidth);
end;

function TdxNavBarHamburgerMenuViewInfo.GetGroupItemsVisible
  (AGroup: TdxNavBarGroup): Boolean;
begin
  Result := not TdxNavBarControllerAccess(NavBar.Controller).Collapsed or
    IsRootGroup(AGroup);
end;

function TdxNavBarHamburgerMenuViewInfo.IsPtHeaderSign
  (const pt: TPoint): Boolean;
begin
  Result := PtInRect
    ((HeaderPanelViewInfo as TdxNavBarHamburgerMenuHeaderPanelViewInfo)
    .SignClientRect, pt);
end;

function TdxNavBarHamburgerMenuViewInfo.IsTopAlignment
  (AGroup: TdxNavBarGroup): Boolean;
begin
  Result := inherited IsTopAlignment(AGroup) or IsRootGroup(AGroup);
end;

function TdxNavBarHamburgerMenuViewInfo.NeedCalculateScrollBarVisible: Boolean;
begin
  Result := inherited NeedCalculateScrollBarVisible and
    not TdxNavBarControllerAccess(NavBar.Controller).Collapsed;
end;

procedure TdxNavBarHamburgerMenuViewInfo.DoGroupActiveToggle
  (AGroup: TdxNavBarGroup);
begin
  if IsNavPaneVisible and (AGroup.Parent = nil) then
    NavBar.ActiveGroup := AGroup
  else if TdxNavBarControllerAccess(NavBar.Controller).Collapsed then
  begin
    if AGroup.Parent = nil then
      NavBar.ActiveGroup := AGroup
    else
      NavBar.ActiveGroup := AGroup.Parent;
    FActualSelectedPopupGroup := AGroup;
    TdxNavBarControllerAccess(NavBar.Controller).InternalShowPopupControl
      (AGroup, dxNavBarPart(-1));
    NavBar.DesignerModified;
  end
  else
    inherited DoGroupActiveToggle(AGroup);
end;

function TdxNavBarHamburgerMenuViewInfo.AllowControlExpandAnimation: Boolean;
begin
  Result := True;
  if NavBar.OptionsBehavior.HamburgerMenu.DisplayMode <> dmOverlayMinimal then
    Result := not(NavBar.Align in [alTop, alBottom, alClient]);
end;

function TdxNavBarHamburgerMenuViewInfo.CanHasActiveGroup: Boolean;
begin
  Result := IsNavPaneVisible;
end;

function TdxNavBarHamburgerMenuViewInfo.CanHasHeader: Boolean;
begin
  Result := True;
end;

function TdxNavBarHamburgerMenuViewInfo.GetGroupEdges: TPoint;
begin
  Result := cxNullPoint
end;

function TdxNavBarHamburgerMenuViewInfo.GetGroupCaptionHeightAddon: Integer;
var
  AElement: TdxSkinElement;
begin
  AElement := GetSkinHelper.NavBarGroupButtonCaption;
  if AElement = nil then
    Result := ScaleFactor.Apply(5)
  else
    Result := ScaleFactor.Apply(cxMarginsHeight(AElement.ContentOffset.Rect));
  Result := Result div 2;
end;

function TdxNavBarHamburgerMenuViewInfo.GetGroupCaptionImageIndent: Integer;
var
  AElement: TdxSkinElement;
begin
  AElement := GetSkinHelper.NavBarGroupButtonCaption;
  if AElement = nil then
    Result := ChildGroupContentOffset.Left
  else
    Result := GetSkinElementOffsets(AElement, ScaleFactor).Left;
  Result := Result div 2;
end;

function TdxNavBarHamburgerMenuViewInfo.GetGroupCaptionImageOffsets: TRect;
var
  AElement: TdxSkinElement;
begin
  AElement := SkinHelper.NavBarGroupButtonCaption;
  if AElement = nil then
    Result := inherited GetGroupCaptionImageOffsets
  else
    Result := GetSkinElementOffsets(AElement, ScaleFactor);
end;

function TdxNavBarHamburgerMenuViewInfo.GetGroupCaptionSeparatorWidth: Integer;
begin
  Result := 0;
end;

function TdxNavBarHamburgerMenuViewInfo.GetGroupCaptionTextIndent: Integer;
begin
  Result := GetTextIndent;
end;

function TdxNavBarHamburgerMenuViewInfo.GetGroupSeparatorWidth: Integer;
begin
  if NavBar.OptionsView.HamburgerMenu.SpaceBetweenGroups = -1 then
    Result := SkinHelper.NavBarDistanceBetweenRootGroups
  else
    Result := NavBar.OptionsView.HamburgerMenu.SpaceBetweenGroups;
  Result := ScaleFactor.Apply(Result);
end;

function TdxNavBarHamburgerMenuViewInfo.CreateGroupViewInfo
  (AGroup: TdxNavBarGroup; ACaptionVisible, AItemsVisible: Boolean)
  : TdxNavBarGroupViewInfo;
var
  AClass: TdxNavBarGroupViewInfoClass;
begin
  if IsRootGroup(AGroup) or IsGroupShowing then
  begin
    AClass := TdxHamburgerMenuRootGroupViewInfo;
    ACaptionVisible := False;
    AItemsVisible := True;
  end
  else if IsCollapsed then
    AClass := TdxHamburgerMenuCollapsedGroupViewInfo
  else
    AClass := TdxNavBarHamburgerMenuGroupViewInfo;
  Result := AClass.Create(Self, AGroup, ACaptionVisible, AItemsVisible);
end;

function TdxNavBarHamburgerMenuViewInfo.GetImageRectWidth
  (AItemViewInfo: TdxNavBarCustomItemViewInfo): Integer;
begin
  if AItemViewInfo.HasImage then
    Result := AItemViewInfo.GetImageWidth
  else
    Result := 0;
end;

procedure TdxNavBarHamburgerMenuViewInfo.BeginActiveGroupPopupControl;
begin
  FIsPopupControlCalculation := True;
end;

procedure TdxNavBarHamburgerMenuViewInfo.EndActiveGroupPopupControl;
begin
  FIsPopupControlCalculation := False;
end;

function TdxNavBarHamburgerMenuViewInfo.GetGroupsArea: TRect;
begin
  Result := inherited GetGroupsArea;
  if IsNavPaneVisible then
    Result.Bottom := NavPane.Rect.Top;
end;

function TdxNavBarHamburgerMenuViewInfo.IsNavPaneVisible: Boolean;
begin
  Result := (NavBar.OptionsView.HamburgerMenu.NavigationPaneMode <> npmNone) and
    not IsGroupShowing;
end;

procedure TdxNavBarHamburgerMenuViewInfo.DoCreateGroupsInfo;
begin
  FNavPaneViewInfo.Clear;
  if NavBar.OptionsBehavior.HamburgerMenu.DisplayMode <> dmOverlayMinimal then
  begin
    if not IsNavPaneVisible then
      inherited DoCreateGroupsInfo
    else
    begin
      if IsNavPaneActiveGroup(NavBar.ActiveGroup) then
        FGroups.Add(TdxHamburgerMenuRootGroupViewInfo.Create(Self,
          NavBar.ActiveGroup, False, True));
      FNavPaneViewInfo.CreateGroupInfos;
    end;
  end;
end;

function TdxNavBarHamburgerMenuViewInfo.GetNavBarCollapsedWidth: Integer;
begin
  Result := FCollapsedStateCalculator.GetCollapsedWidth;
end;

function TdxNavBarHamburgerMenuViewInfo.GetNavBarMinExpandedWidth: Integer;
begin
  Result := FCollapsedStateCalculator.GetMinExpandedWidth;
end;

function TdxNavBarHamburgerMenuViewInfo.GetHeaderSignAccessibilityHelperClass
  : TdxNavBarCustomAccessibilityHelperClass;
begin
  Result := TdxNavBarHamburgerMenuSignAccessibilityHelper;
end;

function TdxNavBarHamburgerMenuViewInfo.GetHeaderSignIndents: TRect;
begin
  Result := cxNullRect;
end;

function TdxNavBarHamburgerMenuViewInfo.GetChildItemOffset: Integer;
begin
  Result := Max(GetHelper.NavBarChildItemOffset, 0);
end;

function TdxNavBarHamburgerMenuViewInfo.GetItemCaptionOffsets: TRect;
begin
  if GetHelper.NavBarItem <> nil then
    Result := GetHelper.NavBarItem.ContentOffset.Rect
  else
    Result := cxNullRect;
end;

function TdxNavBarHamburgerMenuViewInfo.GetLinksImageEdges: TRect;
var
  AEdge: Integer;
begin
  AEdge := GetGroupCaptionImageOffsets.Left;
  Result := cxRect(AEdge, 0, AEdge, 0);
end;

function TdxNavBarHamburgerMenuViewInfo.GetPartIAccessibilityHelper
  (const APart: TdxNavBarPart): IdxNavBarAccessibilityHelper;
begin
  if APart.MajorPartIndex = nbOverflowPanelSign then
    Result := NavPaneSignIAccessibilityHelper
  else
    Result := inherited GetPartIAccessibilityHelper(APart);
end;

function TdxNavBarHamburgerMenuViewInfo.GetOverflowPanelHeight: Integer;
begin
  Result := FNavPaneViewInfo.GetItemImageSize.cx;
  if FCollapsedStateCalculator.ImageHeightPeer then
    Result := Max(FCollapsedStateCalculator.MaxImageSize.cy, Result);
end;

function TdxNavBarHamburgerMenuViewInfo.GetLargeImageHeight
  (AItemViewInfo: TdxNavBarCustomItemViewInfo): Integer;
begin
  if AItemViewInfo.HasImage then
    Result := inherited GetLargeImageHeight(AItemViewInfo)
  else
    Result := 0;
end;

function TdxNavBarHamburgerMenuViewInfo.GetLargeImageWidth
  (AItemViewInfo: TdxNavBarCustomItemViewInfo): Integer;
begin
  if AItemViewInfo.HasImage then
    Result := inherited GetLargeImageWidth(AItemViewInfo)
  else
    Result := 0;
end;

function TdxNavBarHamburgerMenuViewInfo.GetSmallImageHeight
  (AItemViewInfo: TdxNavBarCustomItemViewInfo): Integer;
begin
  if AItemViewInfo.HasImage then
    Result := inherited GetSmallImageHeight(AItemViewInfo)
  else
    Result := 0;
end;

function TdxNavBarHamburgerMenuViewInfo.GetSmallImageWidth
  (AItemViewInfo: TdxNavBarCustomItemViewInfo): Integer;
begin
  if AItemViewInfo.HasImage then
    Result := inherited GetSmallImageWidth(AItemViewInfo)
  else
    Result := 0;
end;

function TdxNavBarHamburgerMenuViewInfo.GetPopupTopPos: Integer;

  procedure FindChildGroupTopPosition(ASelectedPopupGroup: TdxNavBarGroup;
    var ATopPosition: Integer);
  var
    I: Integer;
    AItem: TdxNavBarCustomItemViewInfo;
  begin
    dxTestCheck(GroupCount = 1, 'GroupCount <> 1');
    for I := 0 to Groups[0].Infos.Count - 1 do
    begin
      AItem := Groups[0].Infos[I];
      if (AItem is TdxNavBarChildGroupViewInfo) and
        (TdxNavBarChildGroupViewInfo(AItem).Group = ASelectedPopupGroup) then
      begin
        ATopPosition := TdxNavBarChildGroupViewInfo(AItem).Rect.Top;
        Break;
      end;
    end;
  end;

var
  I: Integer;
  ASelectedPopupGroup: TdxNavBarGroup;
begin
  if NavBar.OptionsBehavior.HamburgerMenu.SelectedGroupPopupMode = pmUndocked
  then
  begin
    Result := GroupsArea.Top;
    ASelectedPopupGroup := GetSelectedPopupGroup;
    dxTestCheck(ASelectedPopupGroup <> nil, 'SelectedPopupGroup = nil');
    if IsNavPaneVisible then
      FindChildGroupTopPosition(ASelectedPopupGroup, Result)
    else
      for I := 0 to GroupCount - 1 do
        if Groups[I].Group = ASelectedPopupGroup then
        begin
          Result := Groups[I].Rect.Top;
          Break;
        end;
  end
  else // pmDocked
    Result := NavBar.Top;
end;

function TdxNavBarHamburgerMenuViewInfo.IsCheckBoundsNeeded: Boolean;
begin
  Result := True;
end;

function TdxNavBarHamburgerMenuViewInfo.IsHeaderVisible: Boolean;
begin
  Result := NavBar.OptionsView.HamburgerMenu.ShowHeader;
  if IsInternal then
    Result := Result and not IsGroupShowing or IsGroupShowing and
      (NavBar.OptionsBehavior.HamburgerMenu.SelectedGroupPopupMode <>
      pmUndocked);
end;

procedure TdxNavBarHamburgerMenuViewInfo.CheckControlWindowRegion
  (AGroup: TdxNavBarGroupViewInfo);
begin
  SetGroupControlWindowRegion(GetGroupControlClipRect, AGroup);
end;

function TdxNavBarHamburgerMenuViewInfo.GetGroupAccessibilityHelperClass
  : TdxNavBarCustomAccessibilityHelperClass;
begin
  Result := TdxNavBarHamburgerMenuGroupAccessibilityHelper;
end;

function TdxNavBarHamburgerMenuViewInfo.
  GetGroupCaptionPanelAccessibilityHelperClass
  : TdxNavBarCustomAccessibilityHelperClass;
begin
  Result := TdxNavBarHamburgerMenuGroupCaptionPanelAccessibilityHelper;
end;

function TdxNavBarHamburgerMenuViewInfo.IsCollapsed: Boolean;
begin
  Result := GetController.Collapsed and not IsGroupShowing and
    not FIsPopupControlCalculation;
end;

function TdxNavBarHamburgerMenuViewInfo.IsNavPaneActiveGroup(AGroupViewInfo
  : TdxNavBarGroupViewInfo): Boolean;
begin
  Result := IsNavPaneActiveGroup(AGroupViewInfo.Group);
end;

function TdxNavBarHamburgerMenuViewInfo.IsNavPaneActiveGroup
  (AGroup: TdxNavBarGroup): Boolean;
begin
  Result := IsNavPaneVisible and (NavBar.ActiveGroup <> nil) and
    (NavBar.ActiveGroup = AGroup) and not IsRootGroup(AGroup);
end;

function TdxNavBarHamburgerMenuViewInfo.IsRootGroup(AGroupViewInfo
  : TdxNavBarGroupViewInfo): Boolean;
begin
  Result := IsRootGroup(AGroupViewInfo.Group);
end;

function TdxNavBarHamburgerMenuViewInfo.IsRootGroup
  (AGroup: TdxNavBarGroup): Boolean;
begin
  Result := (AGroup <> nil) and (AGroup.Level = 0) and
    (not AGroup.ShowCaption or not NavBar.ShowGroupCaptions);
end;

function TdxNavBarHamburgerMenuViewInfo.IsSelectedPopupGroup(AGroupViewInfo
  : TdxNavBarGroupViewInfo): Boolean;
begin
  Result := IsGroupShowing and AGroupViewInfo.TopLevel;
end;

function TdxNavBarHamburgerMenuViewInfo.IsVerticalItem(AItemViewInfo
  : TdxNavBarCustomItemViewInfo): Boolean;

  function IsSeparator: Boolean;
  begin
    Result := TdxNavBarItemViewInfoAccess(AItemViewInfo)
      .GetNavBarItem is TdxNavBarSeparator;
  end;

begin
  Result := not FIsPopupControlCalculation and not IsInternal and
    IsCollapsed and not AItemViewInfo.HasImage and not IsSeparator;
end;

procedure TdxNavBarHamburgerMenuViewInfo.CalculateGroupVerticalCaption
  (AGroupViewInfo: TdxNavBarGroupViewInfo);
var
  AGroupViewInfoAccess: TdxNavBarGroupViewInfoAccess;
begin
  AGroupViewInfoAccess := TdxNavBarGroupViewInfoAccess(AGroupViewInfo);
  AGroupViewInfoAccess.FImageRect.Empty;
  AGroupViewInfoAccess.CaptionSignRect.Empty;
  AGroupViewInfoAccess.FRect := AGroupViewInfoAccess.FCaptionRect;
  CalculateVerticalCaption(AGroupViewInfo.Group.Caption,
    AGroupViewInfo.CaptionFont, AGroupViewInfoAccess.FRect,
    AGroupViewInfoAccess.FCaptionTextRect);
  AGroupViewInfoAccess.FCaptionRect := AGroupViewInfoAccess.FRect;
end;

procedure TdxNavBarHamburgerMenuViewInfo.CalculateVerticalCaption
  (ACaption: string; AFont: TFont; var ACaptionRect, ACaptionTextRect: TRect);

  function GetCaptionHeight: Integer;
  begin
    Result := cxMarginsHeight(GetItemCaptionOffsets);
    Inc(Result, cxTextHeight(AFont));
  end;

var
  AContentOffset, ATextHeight, ATextWidth: Integer;
begin
  AContentOffset := dxNavBarHamburgerMenuItemVerticalCaptionIndent;
  ATextHeight := GetCaptionHeight;
  ATextWidth := cxTextWidth(AFont, ACaption);
  ACaptionRect.Height := AContentOffset + ATextWidth + AContentOffset;
  ACaptionTextRect := ACaptionRect;
  ACaptionTextRect.Left := ACaptionRect.CenterPoint.X - ATextHeight div 2;
  ACaptionTextRect.Right := ACaptionTextRect.Left + ATextHeight;
  ACaptionTextRect.Top := ACaptionRect.Top + AContentOffset;
  ACaptionTextRect.Height := ATextWidth;
end;

procedure TdxNavBarHamburgerMenuViewInfo.CalculateVerticalCaptionDesignRect
  (AItem: TdxNavBarCustomItemViewInfo);
var
  AItemAccess: TdxNavBarItemViewInfoAccess;
begin
  AItemAccess := TdxNavBarItemViewInfoAccess(AItem);
  AItemAccess.CalcDesignRect(AItemAccess.Rect);
  AItemAccess.FDesignRect := cxRectSetBottom(AItemAccess.DesignRect,
    AItemAccess.Rect.Bottom - AItemAccess.GetDesignSelectorSize.cy div 2);
end;

procedure TdxNavBarHamburgerMenuViewInfo.CheckSelectedPopupGroupState
  (AGroupViewInfo: TdxNavBarGroupViewInfo; var AStates: TdxNavBarObjectStates);
var
  AController: TdxNavBarControllerAccess;
begin
  AController := TdxNavBarControllerAccess(Painter.Controller);
  if (AController.SelectedPopupGroup <> nil) and
    (AController.SelectedPopupGroup = AGroupViewInfo.Group) then
    Include(AStates, sPressed);
end;

procedure TdxNavBarHamburgerMenuViewInfo.CreateColors;
begin
  CreateOffice11NavPaneColors;
end;

procedure TdxNavBarHamburgerMenuViewInfo.RefreshColors;
begin
  RefreshOffice11NavPaneColors;
end;

procedure TdxNavBarHamburgerMenuViewInfo.ReleaseColors;
begin
  ReleaseOffice11NavPaneColors;
end;

procedure TdxNavBarHamburgerMenuViewInfo.AlignItems;
begin
  TdxHamburgerMenuItemsLayoutCalculator.AlignContent(Self);
end;

procedure TdxNavBarHamburgerMenuViewInfo.CalculateGroups(X, Y: Integer);
begin
  NavPane.Calculate(inherited GetGroupsArea);
  inherited CalculateGroups(X, Y);
end;

procedure TdxNavBarHamburgerMenuViewInfo.DoCalculateBounds(X, Y: Integer);
begin
  FCollapsedStateCalculator.Reset;
  inherited DoCalculateBounds(X, Y);
  AlignItems;
end;

procedure TdxNavBarHamburgerMenuViewInfo.CorrectBounds;
begin
  inherited CorrectBounds;
  CorrectPanelsBounds;
  CalculateScrollButtonsBounds;
end;

procedure TdxNavBarHamburgerMenuViewInfo.CorrectPanelsBounds;

  procedure CheckContentOffset;
  var
    AScrollButtonsArea, AScrollingAreaBounds: TRect;
  begin
    if FContentOffset <> 0 then
    begin
      AScrollingAreaBounds := GetScrollingAreaBounds;
      AScrollButtonsArea := GetScrollButtonsBounds;
      if not(AScrollingAreaBounds.IsEmpty and
        (AScrollingAreaBounds.Height <= AScrollButtonsArea.Height)) then
      begin
        if (FContentOffset < 0) then
          FContentOffset := Max(FContentOffset, AScrollButtonsArea.Bottom -
            AScrollingAreaBounds.Bottom);
        if (FContentOffset > 0) then
          FContentOffset := Min(FContentOffset, AScrollButtonsArea.Top -
            AScrollingAreaBounds.Top);
      end
      else
        FContentOffset := 0;
    end;
  end;

var
  I: Integer;
begin
  CheckContentOffset;
  if FContentOffset <> 0 then
    for I := 0 to GroupCount - 1 do
      Groups[I].CorrectBounds(0, FContentOffset);
end;

procedure TdxNavBarHamburgerMenuViewInfo.InternalCalculateMaxImageSize;
begin
  FCollapsedStateCalculator.CalculateMaxImageSize;
end;

function TdxNavBarHamburgerMenuViewInfo.GetScrollingAreaBounds: TRect;
begin
  Result := cxInvalidRect;
  if IsNavPaneVisible then
  begin
    if ActiveGroupViewInfo <> nil then
      Result := ActiveGroupViewInfo.Rect;
  end
  else if GroupCount > 0 then
  begin
    Result := Groups[0].Rect;
    Result.Right := Groups[GroupCount - 1].Rect.Right;
    Result.Bottom := Groups[GroupCount - 1].Rect.Bottom;
  end;
end;

procedure TdxNavBarHamburgerMenuViewInfo.ResetScrollOffset;
begin
  FContentOffset := 0;
end;

function TdxNavBarHamburgerMenuViewInfo.CanHasScrollButtons: Boolean;
begin
  Result := GetController.Collapsed;
end;

function TdxNavBarHamburgerMenuViewInfo.GetScrollButtonHorizontalEdge: Integer;
begin
  Result := GetScrollingAreaBounds.Width div 2 -
    GetScrollButtonHorizontalSize div 2;
end;

function TdxNavBarHamburgerMenuViewInfo.GetScrollButtonsBounds: TRect;
begin
  Result := GroupsArea;
end;

function TdxNavBarHamburgerMenuViewInfo.IsBottomScrollButtonDisabled: Boolean;
begin
  Result := IsScrollButtonStateDisabled(False);
end;

function TdxNavBarHamburgerMenuViewInfo.IsTopScrollButtonStateDisabled: Boolean;
begin
  Result := IsScrollButtonStateDisabled(True);
end;

function TdxNavBarHamburgerMenuViewInfo.CanShowGroupItems
  (AGroup: TdxNavBarGroup): Boolean;
begin
  Result := AGroup.Expanded and IsRootGroup(AGroup);
end;

function TdxNavBarHamburgerMenuViewInfo.ChildGroupContentOffset: TRect;
begin
  if GetHelper.NavBarChildGroup <> nil then
    Result := GetHelper.NavBarChildGroup.ContentOffset.Rect
  else
    Result := cxNullRect;
end;

function TdxNavBarHamburgerMenuViewInfo.GetGroupControlClipRect: TRect;
begin
  Result := GroupsArea;
  if NavBar.ScrollBar.Visible and NavBar.IsRightToLeft then
  begin
    Result.Right := ClientRect.Right;
    Result.Left := NavBar.ScrollBar.Bounds.Right;
  end;
end;

function TdxNavBarHamburgerMenuViewInfo.GetMasterPopupGroup: TdxNavBarGroup;
begin
  if IsInternal then
    Result := TdxNavBarControllerAccess(TdxNavBarAccess(NavBar)
      .GetMasterNavBar.Controller).SelectedPopupGroup
  else
    Result := nil;
end;

function TdxNavBarHamburgerMenuViewInfo.GetMaxImageSize: TSize;
begin
  Result := FCollapsedStateCalculator.MaxImageSize;
end;

function TdxNavBarHamburgerMenuViewInfo.GetTextIndent: Integer;
begin
  Result := GetLinksImageEdges.Right;
end;

function TdxNavBarHamburgerMenuViewInfo.IsGroupShowing: Boolean;
begin
  Result := IsInternal and (GetSelectedPopupGroup <> nil);
end;

function TdxNavBarHamburgerMenuViewInfo.IsSkinAvailable: Boolean;
begin
  Result := Painter.IsSkinAvailable;
end;

function TdxNavBarHamburgerMenuViewInfo.ItemContentOffset: TRect;
begin
  Result := GetItemCaptionOffsets;
end;

procedure TdxNavBarHamburgerMenuViewInfo.UpdateEffectColors;
var
  AEffectColor: TColor;
begin
  AEffectColor := TdxNavBarAccess(NavBar).GetHybridScrollbarBaseColor;
  ContentHighlightEffectInfo.SetColor(dxColorToAlphaColor(AEffectColor, $20));
  BorderHighlightEffectInfo.SetColor(dxColorToAlphaColor(AEffectColor, $40));
  PressedEffectInfo.SetColor(dxColorToAlphaColor(AEffectColor, $20));
end;

procedure TdxNavBarHamburgerMenuViewInfo.UpdateScrollOffset(ADelta: Integer);
begin
  Inc(FContentOffset, ADelta);
  GetController.InvalidateAll(doRecalc);
end;

function TdxNavBarHamburgerMenuViewInfo.GetActualScrollingAreaHeight: Integer;
begin
  Result := ClientHeight;
end;

function TdxNavBarHamburgerMenuViewInfo.GetController
  : TdxNavBarHamburgerMenuController;
begin
  Result := Painter.Controller as TdxNavBarHamburgerMenuController;
end;

function TdxNavBarHamburgerMenuViewInfo.GetNavPaneRect: TRect;
begin
  Result := NavPane.Rect;
end;

function TdxNavBarHamburgerMenuViewInfo.GetPainter
  : TdxNavBarHamburgerMenuPainter;
begin
  Result := inherited Painter as TdxNavBarHamburgerMenuPainter;
end;

function TdxNavBarHamburgerMenuViewInfo.GetSelectedPopupGroup: TdxNavBarGroup;
begin
  if IsInternal then
    Result := GetMasterPopupGroup
  else
    Result := TdxNavBarControllerAccess(NavBar.Controller).SelectedPopupGroup;
end;

function TdxNavBarHamburgerMenuViewInfo.GetSkinHelper
  : TdxNavBarHamburgerMenuSkinPainterHelper;
begin
  Result := Painter.FSkinBasedPainterHelper as
    TdxNavBarHamburgerMenuSkinPainterHelper;
end;

function TdxNavBarHamburgerMenuViewInfo.GetNavPaneIAccessibilityHelper
  : IdxNavBarAccessibilityHelper;
begin
  Result := NavPane.IAccessibilityHelper;
end;

function TdxNavBarHamburgerMenuViewInfo.GetNavPaneSignIAccessibilityHelper
  : IdxNavBarAccessibilityHelper;
begin
  Result := NavPane.SignIAccessibilityHelper;
end;

function TdxNavBarHamburgerMenuViewInfo.IsDefaultHeaderColor: Boolean;
begin
  Result := (inherited HeaderBackColor = SkinHelper.NavBarBackground.Color) or
    (inherited HeaderBackColor2 = SkinHelper.NavBarBackground.Color);
end;

function TdxNavBarHamburgerMenuViewInfo.IsDefaultOverflowPanelColor: Boolean;
begin
  Result := (inherited OverflowPanelBackColor = clNone) or
    (inherited OverflowPanelBackColor2 = clNone);
end;

function TdxNavBarHamburgerMenuViewInfo.
  IsDefaultBottomScrollButtonColor: Boolean;
begin
  Result := (inherited BottomScrollButtonBackColor = clNone) or
    (inherited BottomScrollButtonBackColor2 = clNone);
end;

function TdxNavBarHamburgerMenuViewInfo.IsDefaultTopScrollButtonColor: Boolean;
begin
  Result := (inherited TopScrollButtonBackColor = clNone) or
    (inherited TopScrollButtonBackColor2 = clNone);
end;

function TdxNavBarHamburgerMenuViewInfo.IsScrollButtonStateDisabled
  (ATopButton: Boolean): Boolean;

  function IsEnoughSpace(const ABounds: TRect): Boolean;
  begin
    if ATopButton then
      Result := ABounds.Top >= GetScrollButtonsBounds.Top
    else
      Result := ABounds.Bottom <= GetScrollButtonsBounds.Bottom;
  end;

var
  AScrollingAreaBounds: TRect;
begin
  AScrollingAreaBounds := GetScrollingAreaBounds;
  Result := AScrollingAreaBounds.IsZero or cxRectIsInvalid(AScrollingAreaBounds)
    or IsEnoughSpace(AScrollingAreaBounds);
  if IsNavPaneVisible then
    Result := (NavBar.ActiveGroup = nil) or (NavBar.ActiveGroup <> nil)
      and Result;
end;

function TdxNavBarHamburgerMenuViewInfo.IsScrollButtonVisible(const AButtonRect
  : TRect): Boolean;
begin
  Result := PtInRect(GetScrollButtonsBounds,
    NavBar.ScreenToClient(GetMouseCursorPos)) and not AButtonRect.IsEmpty and
    HasEnoughSpaceForScrollButtons(GetActualScrollingAreaHeight);
end;

{ TdxNavBarHamburgerMenuCollapsedStateCalculator }

function TdxNavBarHamburgerMenuCollapsedStateCalculator.CreateGroupViewInfo
  (AGroup: TdxNavBarGroup): TdxNavBarGroupViewInfo;
begin
  Result := TdxHamburgerMenuCollapsedStateCalculatorGroupViewInfo.Create
    (FViewInfo, AGroup, True, AGroup.Level = 0);
end;

function TdxNavBarHamburgerMenuCollapsedStateCalculator.
  GetMaxElementSize: Integer;
var
  AGroupCaptionImageOffsets: TRect;
  AViewInfo: TdxNavBarHamburgerMenuViewInfo;
begin
  AViewInfo := GetViewInfo;
  AGroupCaptionImageOffsets := AViewInfo.GetGroupCaptionImageOffsets;
  Result := AViewInfo.GetGroupCaptionImageIndent * 2 + FMaxImageSize.cx +
    AGroupCaptionImageOffsets.Left + AGroupCaptionImageOffsets.Right;

  Result := Max(Result, AViewInfo.GetGroupCaptionSignSize.cx +
    AViewInfo.GetHeaderClientOffset.Left +
    AViewInfo.GetHeaderClientOffset.Right);
end;

procedure TdxNavBarHamburgerMenuCollapsedStateCalculator.
  CalculateGroupMaxImageSize(AGroupViewInfo: TdxNavBarGroupViewInfo);
var
  I: Integer;
begin
  inherited CalculateGroupMaxImageSize(AGroupViewInfo);
  for I := 0 to AGroupViewInfo.ItemCount - 1 do
    CalculateItemMaxImageSize(AGroupViewInfo.Items[I]);
end;

procedure TdxNavBarHamburgerMenuCollapsedStateCalculator.
  CalculatePanelsMaxImageSize;
begin
  inherited CalculatePanelsMaxImageSize;
  FMaxImageSize := GetViewInfo.NavPane.GetItemImageSize;
end;

function TdxNavBarHamburgerMenuCollapsedStateCalculator.GetViewInfo
  : TdxNavBarHamburgerMenuViewInfo;
begin
  Result := FViewInfo as TdxNavBarHamburgerMenuViewInfo;
end;

{ TdxNavBarHamburgerMenuPainter }

constructor TdxNavBarHamburgerMenuPainter.Create(ANavBar: TdxCustomNavBar);
begin
  inherited Create(ANavBar);
  FNavPaneSignPainter := TdxHamburgerMenuOverflowPanelPainter.Create
    (SkinHelper);
end;

destructor TdxNavBarHamburgerMenuPainter.Destroy;
begin
  FreeAndNil(FNavPaneSignPainter);
  inherited Destroy;
end;

procedure TdxNavBarHamburgerMenuPainter.DrawBackground;
begin
  if not InternalDrawNavBarBackground(SkinHelper.NavBarBackground,
    ViewInfo.GetBackgroundRect, True) then
    inherited DrawBackground;
end;

procedure TdxNavBarHamburgerMenuPainter.DrawHeaderBackground;
begin
  if not InternalDrawPanelBackground(SkinHelper.NavBarBackground,
    ViewInfo.HeaderPanelViewInfo.Rect, True) then
    inherited DrawHeaderBackground;
end;

procedure TdxNavBarHamburgerMenuPainter.DrawHeaderSign;
var
  AElement: TdxSkinElement;
  AHeaderViewInfo: TdxNavBarHamburgerMenuHeaderPanelViewInfo;
  AState, AElementState: TdxSkinElementState;
  R: TRect;
begin
  AHeaderViewInfo := TdxNavBarViewInfoAccess(ViewInfo)
    .HeaderPanelViewInfo as TdxNavBarHamburgerMenuHeaderPanelViewInfo;
  AState := NavBarObjectStateToSkinState(AHeaderViewInfo.SignState);
  if TdxNavBarAccess(NavBar).IsExpandAnimationPreparing or AllowHighlightEffects
  then
    AElementState := esNormal
  else
    AElementState := NavBarObjectStateToSkinState(AHeaderViewInfo.SignState);
  AElement := SkinHelper.HamburgerButton;
  if not DrawSkinElement(AElement, cxCanvas.Canvas,
    AHeaderViewInfo.SignClientRect, ScaleFactor, 0, AElementState) then
    inherited DrawHeaderSign;
  if AllowHighlightEffects then
  begin
    R := cxRectInflate(AHeaderViewInfo.SignClientRect, ScaleFactor.Apply(-1),
      ScaleFactor.Apply(-1));
    if AState = esHot then
      DrawElementHighlight(R);
    DrawElementHighlightBorders(R);
  end;
end;

procedure TdxNavBarHamburgerMenuPainter.DrawNavBarControl;
begin
  inherited DrawNavBarControl;
  if not TdxNavBarAccess(NavBar).IsGroupExpanding then
    DrawNavPane;
end;

procedure TdxNavBarHamburgerMenuPainter.DrawChildGroupCaption
  (AChildGroupViewInfo: TdxNavBarChildGroupViewInfo);
begin
  if AChildGroupViewInfo.TopLevel then
    DrawGroupCaption(AChildGroupViewInfo)
  else
    inherited DrawChildGroupCaption(AChildGroupViewInfo);
end;

procedure TdxNavBarHamburgerMenuPainter.DrawChildGroupSelection
  (AChildGroupViewInfo: TdxNavBarChildGroupViewInfo);
var
  AState: TdxSkinElementState;
begin
  AState := NavBarObjectStateToSkinState(AChildGroupViewInfo.State);
  if (AState = esNormal) and not IsAcrylicEnabled then
    InternalDrawGroupBackground(AChildGroupViewInfo,
      AChildGroupViewInfo.SelectionRect)
  else
  begin
    if not AllowHighlightEffects or (AState <> esHot) then
      inherited DrawChildGroupSelection(AChildGroupViewInfo);
    if AllowHighlightEffects then
    begin
      if AState = esHot then
      begin
        DrawElementHighlight(AChildGroupViewInfo.SelectionRect);
        DrawElementHighlightBorders(AChildGroupViewInfo.SelectionRect);
      end;
    end;
  end;
end;

procedure TdxNavBarHamburgerMenuPainter.DrawGroupBackground(AGroupViewInfo
  : TdxNavBarGroupViewInfo);
begin
  if SkinHelper.NavBarBackground <> nil then
    InternalDrawGroupBackground(AGroupViewInfo, AGroupViewInfo.ItemsRect)
  else
    inherited DrawGroupBackground(AGroupViewInfo);
end;

procedure TdxNavBarHamburgerMenuPainter.DrawGroupCaptionButton
  (AGroupViewInfo: TdxNavBarGroupViewInfo);
var
  AElement: TdxSkinElement;
  AState: TdxSkinElementState;
begin
  AState := NavBarObjectStateToSkinState(AGroupViewInfo.State);
  if not AllowHighlightEffects or (sPressed in AGroupViewInfo.State) and
    ViewInfo.IsCollapsed then
    if ViewInfo.IsCollapsed then
    begin
      AElement := SkinHelper.NavBarItem;
      if not DrawSkinElement(AElement, Canvas, AGroupViewInfo.CaptionRect,
        ScaleFactor, 0, AState) then
        SelectionPainterClass.DrawSelection(cxCanvas.Canvas,
          AGroupViewInfo.CaptionRect, AGroupViewInfo.BgBackColor,
          AGroupViewInfo.State, ScaleFactor);
    end
    else
      inherited DrawGroupCaptionButton(AGroupViewInfo);
  if AllowHighlightEffects then
  begin
    if (AState = esHot) and ViewInfo.IsCollapsed then
      DrawElementHighlight(AGroupViewInfo.CaptionRect);
    DrawElementHighlightBorders(AGroupViewInfo.CaptionRect, [bBottom, bTop]);
  end;
end;

procedure TdxNavBarHamburgerMenuPainter.DrawGroupCaptionImage
  (AGroupViewInfo: TdxNavBarGroupViewInfo);
begin
  ImagePainterClass.DrawImage(cxCanvas.Canvas, AGroupViewInfo.ImageList,
    AGroupViewInfo.ImageIndex, AGroupViewInfo.CaptionImageRect, True,
    GetGroupColorPalette(AGroupViewInfo));
end;

procedure TdxNavBarHamburgerMenuPainter.DrawGroupCaptionText(AGroupViewInfo
  : TdxNavBarGroupViewInfo);
var
  AColor: TColor;
begin
  if IsAcrylicEnabled then
    AColor := AGroupViewInfo.AcrylicForegroundColor
  else
    AColor := AGroupViewInfo.CaptionFontColor;
  if ViewInfo.IsVerticalItem(AGroupViewInfo) then
    DrawVerticalText(AGroupViewInfo.CaptionFont, AGroupViewInfo.Group.Caption,
      AGroupViewInfo.CaptionRect, AColor, raMinus90)
  else
    inherited DrawGroupCaptionText(AGroupViewInfo);
end;

procedure TdxNavBarHamburgerMenuPainter.DrawItemCaption(ALinkViewInfo
  : TdxNavBarLinkViewInfo);
begin
  if ViewInfo.IsVerticalItem(ALinkViewInfo) then
    DrawVerticalText(ALinkViewInfo.Font, ALinkViewInfo.Caption,
      ALinkViewInfo.CaptionRect, ALinkViewInfo.FontColor, raMinus90)
  else
    inherited DrawItemCaption(ALinkViewInfo);
end;

procedure TdxNavBarHamburgerMenuPainter.DrawItemImage(ALinkViewInfo
  : TdxNavBarLinkViewInfo);
var
  ALinkInfo: TdxNavBarHamburgerMenuLinkViewInfo;
begin
  ALinkInfo := ALinkViewInfo as TdxNavBarHamburgerMenuLinkViewInfo;
  ImagePainterClass.DrawImage(Canvas, ALinkInfo.ImageList, ALinkInfo.ImageIndex,
    ALinkInfo.ImageRect, ALinkInfo.IsEnabled or ALinkInfo.UseDisabledImages,
    GetItemColorPalette(ALinkInfo));
end;

procedure TdxNavBarHamburgerMenuPainter.DrawBottomScrollButton;
begin
  DrawScrollButton(InternalDrawBottomScrollButton);
end;

procedure TdxNavBarHamburgerMenuPainter.DrawTopScrollButton;
begin
  DrawScrollButton(InternalDrawTopScrollButton);
end;

procedure TdxNavBarHamburgerMenuPainter.DrawNavPaneBackground;
begin
  if not IsAcrylicEnabled then
    InternalDrawPanelBackground(SkinHelper.HamburgerMenuNavPaneBackground,
      ViewInfo.NavPane.Rect, False);
end;

procedure TdxNavBarHamburgerMenuPainter.DrawNavPaneItems;
var
  I: Integer;
begin
  for I := 0 to ViewInfo.NavPane.VisibleItems.Count - 1 do
    DrawNavPaneItem(ViewInfo.NavPane.VisibleItems[I]);
end;

procedure TdxNavBarHamburgerMenuPainter.DrawNavPaneSign;
begin
  if not ViewInfo.NavPane.FOverflowPanelRect.IsEmpty then
    FNavPaneSignPainter.Draw(Canvas, ViewInfo.NavPane.OverflowPanelViewInfo);
end;

function TdxNavBarHamburgerMenuPainter.AllowUsualItemSelection
  (ALinkViewInfo: TdxNavBarLinkViewInfo): Boolean;
begin
  Result := not AllowHighlightEffects or not ViewInfo.IsCollapsed and
    (ALinkViewInfo.State * [sHotTracked, sPressed, sSelected] = []) or
    ViewInfo.IsCollapsed and (ALinkViewInfo.State <> [sHotTracked]);
end;

function TdxNavBarHamburgerMenuPainter.GetAcrylicForegroundColor: TColor;
begin
  Result := ViewInfo.HeaderFontColor;
end;

function TdxNavBarHamburgerMenuPainter.GetRootGroupAcrylicForegroundColor
  (AGroupViewInfo: TdxNavBarGroupViewInfo): TColor;
begin
  Result := dxAlphaColorToColor
    (TdxAlphaColors.Blend(dxColorToAlphaColor(AGroupViewInfo.CaptionFontColor,
    165), dxColorToAlphaColor(GetBackgroundColorForComposition, 90)));
end;

class function TdxNavBarHamburgerMenuPainter.GetChildGroupViewInfoClass
  : TdxNavBarChildGroupViewInfoClass;
begin
  Result := TdxNavBarHamburgerMenuChildGroupViewInfo;
end;

class function TdxNavBarHamburgerMenuPainter.GetDropInfoCalculatorClass
  : TdxNavBarDropInfoCalculatorClass;
begin
  Result := TdxHamburgerMenuDropInfoCalculator;
end;

class function TdxNavBarHamburgerMenuPainter.GetGroupViewInfoClass
  : TdxNavBarGroupViewInfoClass;
begin
  Result := TdxNavBarHamburgerMenuGroupViewInfo;
end;

class function TdxNavBarHamburgerMenuPainter.GetHeaderPanelViewInfoClass
  : TdxNavBarHeaderPanelViewInfoClass;
begin
  Result := TdxNavBarHamburgerMenuHeaderPanelViewInfo
end;

class function TdxNavBarHamburgerMenuPainter.GetLinkViewInfoClass
  : TdxNavBarLinkViewInfoClass;
begin
  Result := TdxNavBarHamburgerMenuLinkViewInfo;
end;

class function TdxNavBarHamburgerMenuPainter.GetSkinPainterHelperClass
  : TdxNavBarSkinBasedPainterHelperClass;
begin
  Result := TdxNavBarHamburgerMenuSkinPainterHelper;
end;

class function TdxNavBarHamburgerMenuPainter.GetViewInfoClass
  : TdxNavBarViewInfoClass;
begin
  Result := TdxNavBarHamburgerMenuViewInfo;
end;

class function TdxNavBarHamburgerMenuPainter.SelectionPainterClass
  : TdxNavBarCustomSelectionPainterClass;
begin
  Result := TdxNavBarUltraFlatSelectionPainter;
end;

class function TdxNavBarHamburgerMenuPainter.SignPainterClass
  : TdxNavBarCustomSignPainterClass;
begin
  Result := TdxHamburgerSignPainter;
end;

class function TdxNavBarHamburgerMenuPainter.ScrollButtonsPainterClass
  : TdxNavBarCustomScrollButtonsPainterClass;
begin
  Result := TdxNavBarBaseScrollButtonsPainter;
end;

function TdxNavBarHamburgerMenuPainter.CreateLinkViewInfo
  (AViewInfo: TdxNavBarGroupViewInfo; ALink: TdxNavBarItemLink;
  ACaptionVisible, AImageVisible: Boolean): TdxNavBarLinkViewInfo;
begin
  if ALink.Item is TdxNavBarSeparator then
    Result := TdxNavBarHamburgerMenuSeparatorViewInfo.Create(AViewInfo, ALink,
      ACaptionVisible, AImageVisible)
  else
    Result := inherited CreateLinkViewInfo(AViewInfo, ALink, ACaptionVisible,
      AImageVisible);
end;

function TdxNavBarHamburgerMenuPainter.CreateGroupViewInfo
  (AViewInfo: TdxNavBarViewInfo; AGroup: TdxNavBarGroup;
  ACaptionVisible, AItemsVisible: Boolean): TdxNavBarGroupViewInfo;
begin
  Result := (AViewInfo as TdxNavBarHamburgerMenuViewInfo)
    .CreateGroupViewInfo(AGroup, ACaptionVisible, AItemsVisible);
end;

function TdxNavBarHamburgerMenuPainter.GetControllerClass
  : TdxNavBarControllerClass;
begin
  Result := TdxNavBarHamburgerMenuController;
end;

function TdxNavBarHamburgerMenuPainter.GetDefaultColorSchemeName: TdxSkinName;
begin
  Result := sdxDefaultColorSchemeName;
end;

function TdxNavBarHamburgerMenuPainter.UseHeaderCustomDrawing: Boolean;
begin
  Result := inherited UseHeaderCustomDrawing;
  if Assigned(NavBar.OnCustomDraw.HamburgerMenuHeader) then
    NavBar.OnCustomDraw.HamburgerMenuHeader(Self, Canvas, ViewInfo, Result);
end;

function TdxNavBarHamburgerMenuPainter.DoDrawScrollButton(ADownButton: Boolean;
  const ARect: TRect; AState: TdxNavBarObjectStates): Boolean;
var
  AElement: TdxSkinElement;
begin
  AElement := SkinHelper.HamburgerScrollDownButton;
  if not ADownButton then
    AElement := SkinHelper.HamburgerScrollUpButton;
  Result := DrawSkinElement(AElement, Canvas, ARect, ScaleFactor, 0,
    NavBarObjectStateToSkinState(AState));
end;

function TdxNavBarHamburgerMenuPainter.GetController
  : TdxNavBarHamburgerMenuController;
begin
  Result := inherited Controller as TdxNavBarHamburgerMenuController;
end;

function TdxNavBarHamburgerMenuPainter.GetSkinHelper
  : TdxNavBarHamburgerMenuSkinPainterHelper;
begin
  Result := FSkinBasedPainterHelper as TdxNavBarHamburgerMenuSkinPainterHelper;
end;

function TdxNavBarHamburgerMenuPainter.GetViewInfo
  : TdxNavBarHamburgerMenuViewInfo;
begin
  Result := inherited ViewInfo as TdxNavBarHamburgerMenuViewInfo;
end;

function TdxNavBarHamburgerMenuPainter.IsBackgroundImagePainting: Boolean;
begin
  Result := (ViewInfo.BgImage <> nil) and (ViewInfo.BgImage.Graphic <> nil) and
    not ViewInfo.BgImage.Graphic.Empty;
end;

function TdxNavBarHamburgerMenuPainter.InternalDrawNavBarBackground
  (AElement: TdxSkinElement; const ARect: TRect;
  AUseSolidColor: Boolean): Boolean;
begin
  Result := AElement <> nil;
  if Result and IsBackgroundImagePainting then
    BackgroundPainterClass.DrawBackground(Canvas, ARect, ViewInfo.BgImage, True,
      AElement.Color, AElement.Color, AElement.Color, ViewInfo.BgAlphaBlend,
      ViewInfo.BgAlphaBlend, ViewInfo.BgGradientMode, ScaleFactor)
  else if IsAcrylicEnabled then
  begin
    dxGPPaintCanvas.BeginPaint(cxCanvas.Handle, ARect);
    dxGPPaintCanvas.FillRectangle(ARect, GetAcrylicBackgroundColor);
    dxGPPaintCanvas.EndPaint;
  end
  else if AUseSolidColor then
    cxCanvas.FillRect(ARect, AElement.Color)
  else
    Result := DrawSkinElement(AElement, cxCanvas.Canvas, ARect, ScaleFactor);
end;

function TdxNavBarHamburgerMenuPainter.InternalDrawPanelBackground
  (AElement: TdxSkinElement; const ARect: TRect;
  AUseSolidColor: Boolean): Boolean;
begin
  Result := not IsBackgroundImagePainting and InternalDrawNavBarBackground
    (AElement, ARect, AUseSolidColor);
end;

procedure TdxNavBarHamburgerMenuPainter.InternalDrawBottomScrollButton;
begin
  if not DoDrawScrollButton(True, ViewInfo.BottomScrollButtonRect,
    ViewInfo.BottomScrollButtonState) then
    inherited DrawBottomScrollButton;
end;

procedure TdxNavBarHamburgerMenuPainter.InternalDrawGroupBackground
  (AGroupViewInfo: TdxNavBarGroupViewInfo; const ARect: TRect);
var
  AImage: TdxSmartImage;
begin
  dxGPPaintCanvas.BeginPaint(Canvas.Handle, ARect);
  try
    dxGPPaintCanvas.SetClipRect(ARect, gmIntersect);
    if (AGroupViewInfo.BgImage <> nil) and
      (AGroupViewInfo.BgImage.Graphic <> nil) and
      not AGroupViewInfo.BgImage.Graphic.Empty then
    begin
      AImage := TdxSmartImage.Create;
      try
        AImage.Assign(AGroupViewInfo.BgImage.Bitmap);
        dxGPPaintCanvas.Draw(AImage, AImage.ClientRect, ARect,
          AGroupViewInfo.BgAlphaBlend);
      finally
        AImage.Free;
      end;
    end
    else
      dxGPPaintCanvas.FillRectangle(ARect,
        dxColorToAlphaColor(SkinHelper.NavBarBackground.Color,
        AGroupViewInfo.BgAlphaBlend));
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

procedure TdxNavBarHamburgerMenuPainter.InternalDrawTopScrollButton;
begin
  if not DoDrawScrollButton(False, ViewInfo.TopScrollButtonRect,
    ViewInfo.TopScrollButtonState) then
    inherited DrawTopScrollButton;
end;

procedure TdxNavBarHamburgerMenuPainter.DrawScrollButton(AProc: TProc);
begin
  cxCanvas.SaveClipRegion;
  try
    cxCanvas.IntersectClipRect(ViewInfo.GetGroupsArea);
    AProc;
  finally
    cxCanvas.RestoreClipRegion;
  end;
end;

function TdxNavBarHamburgerMenuPainter.GetGroupColorPalette(AGroupViewInfo
  : TdxNavBarGroupViewInfo): IdxColorPalette;
begin
  if SkinHelper.NavBarGroupButtonCaption <> nil then
    Result := SkinHelper.NavBarGroupButtonCaption.GetGlyphColorPalette
      (NavBarObjectStateToSkinState(AGroupViewInfo.State))
  else
    Result := nil;
end;

function TdxNavBarHamburgerMenuPainter.GetHeaderSignColorPalette
  (AState: TdxSkinElementState): IdxColorPalette;
begin
  if SkinHelper.HamburgerButton <> nil then
    Result := SkinHelper.HamburgerButton.GetGlyphColorPalette(AState)
  else
    Result := nil;
end;

function TdxNavBarHamburgerMenuPainter.GetItemColorPalette
  (ALinkViewInfo: TdxNavBarLinkViewInfo): IdxColorPalette;
var
  AElement: TdxSkinElement;
begin
  AElement := SkinHelper.NavBarItem;
  if AElement <> nil then
    Result := AElement.GetGlyphColorPalette
      (NavBarObjectStateToSkinState(ALinkViewInfo.State))
  else
    Result := nil;
end;

function TdxNavBarHamburgerMenuPainter.GetNavPaneItemColorPalette
  : IdxColorPalette;
var
  AElement: TdxSkinElement;
begin
  AElement := SkinHelper.NavPaneItem(True);
  if AElement <> nil then
    Result := AElement.GetGlyphColorPalette(esNormal)
  else
    Result := nil;
end;

procedure TdxNavBarHamburgerMenuPainter.DrawDefaultGlyph(ACanvas: TcxCanvas;
  const ARect: TRect);
begin
  cxDrawImage(ACanvas, ARect, ViewInfo.GetDefaultOverflowPanelBitmap,
    ifmStretch, nil, ScaleFactor);
end;

procedure TdxNavBarHamburgerMenuPainter.DrawElementHighlight(const R: TRect);
begin
  if not cxRectIsEmpty(R) then
    ViewInfo.ContentHighlightEffectInfo.Apply(cxCanvas, R);
end;

procedure TdxNavBarHamburgerMenuPainter.DrawItemHighlightEffects
  (ALinkViewInfo: TdxNavBarLinkViewInfo);
var
  AState: TdxNavBarObjectStates;
  R: TRect;
begin
  AState := ALinkViewInfo.State;
  if (AState = [sHotTracked]) or not(sSelected in AState) and
    (sPressed in AState) then
    DrawElementHighlight(ALinkViewInfo.SelectionRect);

  if sHotTracked in AState then
    DrawElementHighlightBorders(ALinkViewInfo.SelectionRect);

  if (sPressed in AState) and not(sSelected in AState) then
    DrawElementPressed(ALinkViewInfo.SelectionRect);

  if (sSelected in AState) and not ViewInfo.IsCollapsed then
  begin
    R := cxRectInflate(ALinkViewInfo.SelectionRect, ScaleFactor.Apply(-1),
      ScaleFactor.Apply(-1));
    R.Right := R.Left + ScaleFactor.Apply(4);
    dxGpFillRect(Canvas.Handle, R, ALinkViewInfo.FontColor);
  end;
end;

procedure TdxNavBarHamburgerMenuPainter.DrawPopupControl(ACanvas: TcxCanvas;
  AViewInfo: TdxNavBarCustomPopupControlViewInfo);
var
  AElement: TdxSkinElement;
begin
  AElement := SkinHelper.NavPanePopupControl;
  if AElement <> nil then
  begin
    ACanvas.SaveClipRegion;
    try
      ACanvas.ExcludeClipRect(AViewInfo.ClientRect);
      AElement.Draw(ACanvas.Handle, AViewInfo.Rect, ScaleFactor);
    finally
      ACanvas.RestoreClipRegion;
    end;
  end;
end;

procedure TdxNavBarHamburgerMenuPainter.OnDrawOverflowPanelPopupMenuItemHandler
  (ACanvas: TCanvas; ARect: TRect; AImageList: TCustomImageList;
  AImageIndex: Integer; AText: string; AState: TdxNavBarObjectStates);
begin
  FNavPaneSignPainter.DrawPopupMenuItem(ViewInfo.NavPane.OverflowPanelViewInfo,
    ACanvas, ARect, AImageList, AImageIndex, AText, AState);
end;

class function TdxNavBarHamburgerMenuPainter.GetOverflowPanelPopupMenuClass
  : TdxNavBarHamburgerMenuOverflowPanelPopupMenuClass;
begin
  Result := TdxNavBarHamburgerMenuOverflowPanelPopupMenu
end;

class function TdxNavBarHamburgerMenuPainter.GetOverflowPanelViewInfoClass
  : TdxNavBarHamburgerMenuOverflowPanelViewInfoClass;
begin
  Result := TdxNavBarHamburgerMenuOverflowPanelViewInfo;
end;

procedure TdxNavBarHamburgerMenuPainter.DrawNavPane;
var
  AHandled: Boolean;
begin
  AHandled := False;
  if Assigned(NavBar.OnCustomDraw.HamburgerMenuNavigationPane) then
    NavBar.OnCustomDraw.HamburgerMenuNavigationPane(NavBar, Canvas, ViewInfo,
      AHandled);
  if not AHandled then
  begin
    DrawNavPaneBackground;
    DrawNavPaneItems;
    DrawNavPaneSign;
  end;
end;

procedure TdxNavBarHamburgerMenuPainter.DrawNavPaneItem
  (AItem: TdxNavBarHamburgerMenuNavPaneCustomItem);
begin
  DrawNavPaneItemBackground(AItem);
  DrawNavPaneItemImage(AItem);
  DrawNavPaneItemText(AItem);
  DrawNavPaneItemFocusRect(AItem);
  cxDrawDesignRect(cxCanvas, AItem.DesignRect,
    NavBar.DesignerIsSelected(AItem.GetNavBarElement));
end;

procedure TdxNavBarHamburgerMenuPainter.DrawNavPaneItemBackground
  (AItem: TdxNavBarHamburgerMenuNavPaneCustomItem);
var
  AElement: TdxSkinElement;
  AState: TdxSkinElementState;
  ANeedDraw: Boolean;
begin
  ANeedDraw := TdxHamburgerHelper.NeedDrawNavPaneItemBackground(AItem.State) or
    (AItem is TdxNavBarHamburgerMenuNavPaneEmptyGroup);
  if AItem is TdxNavBarHamburgerMenuNavPaneEmptyGroup then
    AState := esHot
  else
    AState := NavBarObjectStateToSkinState(AItem.State);
  if ANeedDraw then
  begin
    if not AllowHighlightEffects or (AState <> esHot) then
    begin
      AElement := SkinHelper.NavPaneItem(sActive in AItem.State);
      DrawSkinElement(AElement, Canvas, AItem.Rect, ScaleFactor, 0, AState);
    end;
  end;
  if AllowHighlightEffects then
  begin
    if AState = esHot then
      DrawElementHighlight(AItem.Rect);
    DrawElementHighlightBorders(AItem.Rect);
  end;
end;

procedure TdxNavBarHamburgerMenuPainter.DrawNavPaneItemFocusRect
  (AItem: TdxNavBarHamburgerMenuNavPaneCustomItem);
var
  AIndex: Integer;
begin
  AIndex := ViewInfo.NavPane.VisibleItems.IndexOf(AItem);
  if ViewInfo.NavPane.ItemIAccessibilityHelpers[AIndex].IsFocused then
    DrawSolidFocusRect(AItem.FocusRect, ViewInfo.NavPane.ItemTextColor);
end;

procedure TdxNavBarHamburgerMenuPainter.DrawNavPaneItemImage
  (AItem: TdxNavBarHamburgerMenuNavPaneCustomItem);
begin
  if not cxRectIsNull(AItem.ImageRect) then
  begin
    if IsImageAssigned(AItem.ImageList, AItem.ImageIndex) then
      ImagePainterClass.DrawImage(Canvas, AItem.ImageList, AItem.ImageIndex,
        AItem.ImageRect, True, GetNavPaneItemColorPalette)
    else
      DrawDefaultGlyph(cxCanvas, AItem.ImageRect);
  end;
end;

procedure TdxNavBarHamburgerMenuPainter.DrawNavPaneItemText
  (AItem: TdxNavBarHamburgerMenuNavPaneCustomItem);
begin
  if not cxRectIsNull(AItem.TextRect) then
  begin
    Canvas.Font := ViewInfo.NavPane.ItemFont;
    Canvas.Font.Color := ViewInfo.NavPane.ItemTextColor;
    if IsAcrylicEnabled then
      dxDrawTextOnGlass(cxCanvas, AItem.Text, AItem.TextRect,
        ViewInfo.NavPane.ItemDrawEdgeFlag, True)
    else
      cxDrawText(Canvas, AItem.Text, AItem.TextRect,
        ViewInfo.NavPane.ItemDrawEdgeFlag);
  end;
end;

{ TdxNavBarHamburgerMenuNavPaneCustomItem }

constructor TdxNavBarHamburgerMenuNavPaneCustomItem.Create
  (AItem: TdxNavBarCustomItemViewInfo);
begin
  inherited Create;
  FItem := AItem;
end;

procedure TdxNavBarHamburgerMenuNavPaneCustomItem.DoRightToLeftConversion
  (const AParentRect: TRect);
begin
  SetRect(TdxRightToLeftLayoutConverter.ConvertRect(Rect, AParentRect));
  SetImageRect(TdxRightToLeftLayoutConverter.ConvertRect(ImageRect,
    AParentRect));
  SetTextRect(TdxRightToLeftLayoutConverter.ConvertRect(TextRect, AParentRect));
end;

procedure TdxNavBarHamburgerMenuNavPaneCustomItem.SetImageRect
  (const AValue: TRect);
var
  ARealImageSize: TSize;
begin
  ARealImageSize.cx := FItem.GetImageWidth;
  ARealImageSize.cy := FItem.GetImageHeight;
  TdxNavBarItemViewInfoAccess(FItem).FImageRect :=
    cxRectCenter(AValue, ARealImageSize);
end;

function TdxNavBarHamburgerMenuNavPaneCustomItem.GetGroupViewInfo
  : TdxNavBarGroupViewInfo;
begin
  Result := nil;
end;

function TdxNavBarHamburgerMenuNavPaneCustomItem.GetImageRect: TRect;
begin
  Result := TdxNavBarItemViewInfoAccess(FItem).FImageRect;
end;

function TdxNavBarHamburgerMenuNavPaneCustomItem.GetRect: TRect;
begin
  Result := FItem.Rect;
end;

function TdxNavBarHamburgerMenuNavPaneCustomItem.GetText: string;
begin
  Result := '';
end;

function TdxNavBarHamburgerMenuNavPaneCustomItem.GetTextRect: TRect;
begin
  Result := FTextRect;
end;

function TdxNavBarHamburgerMenuNavPaneCustomItem.GetNavBarElement
  : TdxNavBarCustomItem;
begin
  Result := nil;
end;

procedure TdxNavBarHamburgerMenuNavPaneCustomItem.Offset(const AOffset: TPoint);
begin
  FItem.CorrectBounds(AOffset.X, AOffset.Y);
  OffsetRect(FTextRect, AOffset.X, AOffset.Y);
end;

procedure TdxNavBarHamburgerMenuNavPaneCustomItem.SetRect(const AValue: TRect);
var
  AItemAccess: TdxNavBarItemViewInfoAccess;
begin
  AItemAccess := TdxNavBarItemViewInfoAccess(FItem);
  AItemAccess.FRect := AValue;
  AItemAccess.FCaptionRect := AValue;

  TdxNavBarItemCalculator.CalculateFocusRect(AItemAccess);
  AItemAccess.CalcDesignRect(AValue);
end;

procedure TdxNavBarHamburgerMenuNavPaneCustomItem.SetTextRect
  (const AValue: TRect);
begin
  FTextRect := AValue;
end;

function TdxNavBarHamburgerMenuNavPaneCustomItem.GetDesignRect: TRect;
begin
  Result := FItem.DesignRect;
end;

function TdxNavBarHamburgerMenuNavPaneCustomItem.GetFocusRect: TRect;
begin
  Result := TdxNavBarItemCalculator.CalculateFocusRect(Rect, Rect);
end;

function TdxNavBarHamburgerMenuNavPaneCustomItem.GetImageIndex: Integer;
begin
  Result := FItem.ImageIndex;
end;

function TdxNavBarHamburgerMenuNavPaneCustomItem.GetImageList: TCustomImageList;
begin
  Result := FItem.ImageList;
end;

function TdxNavBarHamburgerMenuNavPaneCustomItem.GetState
  : TdxNavBarObjectStates;
begin
  Result := FItem.State;
end;

{ TdxNavBarHamburgerMenuNavPaneLink }

function TdxNavBarHamburgerMenuNavPaneLink.GetGroupViewInfo
  : TdxNavBarGroupViewInfo;
begin
  Result := LinkViewInfo.GroupViewInfo;
end;

function TdxNavBarHamburgerMenuNavPaneLink.GetNavBarElement
  : TdxNavBarCustomItem;
begin
  Result := Item;
end;

function TdxNavBarHamburgerMenuNavPaneLink.GetText: string;
begin
  Result := LinkViewInfo.Caption;
end;

function TdxNavBarHamburgerMenuNavPaneLink.GetTextRect: TRect;
begin
  Result := FTextRect;
end;

function TdxNavBarHamburgerMenuNavPaneLink.GetItem: TdxNavBarItem;
begin
  Result := LinkViewInfo.Item;
end;

function TdxNavBarHamburgerMenuNavPaneLink.GetLink: TdxNavBarItemLink;
begin
  Result := LinkViewInfo.Link;
end;

function TdxNavBarHamburgerMenuNavPaneLink.GetLinkViewInfo
  : TdxNavBarLinkViewInfo;
begin
  Result := FItem as TdxNavBarLinkViewInfo;
end;

{ TdxNavBarHamburgerMenuNavPaneEmptyGroup }

procedure TdxNavBarHamburgerMenuNavPaneEmptyGroup.SetImageRect
  (const AValue: TRect);
begin
  FItem.ImageRect.Empty;
end;

procedure TdxNavBarHamburgerMenuNavPaneEmptyGroup.SetTextRect
  (const AValue: TRect);
begin
  FItem.CaptionRect.Empty;
end;

{ TdxNavBarHamburgerMenuNavPaneGroup }

function TdxNavBarHamburgerMenuNavPaneGroup.GetGroupViewInfo
  : TdxNavBarGroupViewInfo;
begin
  Result := FItem as TdxNavBarGroupViewInfo;
end;

function TdxNavBarHamburgerMenuNavPaneGroup.GetNavBarElement
  : TdxNavBarCustomItem;
begin
  Result := Group;
end;

function TdxNavBarHamburgerMenuNavPaneGroup.GetText: string;
begin
  Result := GetGroupViewInfo.Group.Caption;
end;

function TdxNavBarHamburgerMenuNavPaneGroup.GetTextRect: TRect;
begin
  Result := GroupViewInfo.CaptionTextRect;
end;

procedure TdxNavBarHamburgerMenuNavPaneGroup.SetTextRect(const AValue: TRect);
begin
  TdxNavBarGroupViewInfoAccess(GroupViewInfo).FCaptionTextRect := AValue;
end;

function TdxNavBarHamburgerMenuNavPaneGroup.GetGroup: TdxNavBarGroup;
begin
  Result := GroupViewInfo.Group;
end;

{ TdxNavBarHamburgerMenuOverflowPanelPopupMenu }

procedure TdxNavBarHamburgerMenuOverflowPanelPopupMenu.DoHiddenItemClick
  (Sender: TObject);
var
  AElement: TObject;
begin
  AElement := TObject(TMenuItem(Sender).Tag);
  if AElement is TdxNavBarItemLink then
    TdxHamburgerHelper.NavBarItemLinkClick(NavBar, TdxNavBarItemLink(AElement))
  else if AElement is TdxNavBarGroup then
  begin
    inherited DoHiddenItemClick(Sender);
    TdxNavBarControllerAccess(NavBar.Controller)
      .DoGroupMouseUp(TdxNavBarGroup(AElement));
  end;
end;

{ TdxNavBarHamburgerMenuOverflowPanelLink }

function TdxNavBarHamburgerMenuOverflowPanelLink.GetCaption: string;
begin
  Result := ItemLink.Item.Caption;
end;

function TdxNavBarHamburgerMenuOverflowPanelLink.GetImageIndex: Integer;
begin
  if FItemLink.Group.LinksUseSmallImages then
    Result := FItemLink.Item.SmallImageIndex
  else
    Result := FItemLink.Item.LargeImageIndex;
end;

function TdxNavBarHamburgerMenuOverflowPanelLink.GetPartElement: TObject;
begin
  Result := ItemLink;
end;

function TdxNavBarHamburgerMenuOverflowPanelLink.GetPartIndex: Integer;
begin
  Result := dxNavBarHamburgerMenuOverflowPanelItemLink;
end;

procedure TdxNavBarHamburgerMenuOverflowPanelLink.SetGroup
  (const AValue: TdxNavBarGroup);
begin
  // do nothing
end;

procedure TdxNavBarHamburgerMenuOverflowPanelLink.SetItemLink
  (const AValue: TdxNavBarItemLink);
begin
  FItemLink := AValue;
  FGroup := FItemLink.Group;
end;

{ TdxNavBarHamburgerMenuOverflowPanelGroup }

function TdxNavBarHamburgerMenuOverflowPanelGroup.GetImageIndex: Integer;
begin
  if Group.UseSmallImages then
    Result := Group.SmallImageIndex
  else
    Result := Group.LargeImageIndex;
end;

{ TdxNavBarHamburgerMenuOverflowPanelViewInfo }

function TdxNavBarHamburgerMenuOverflowPanelViewInfo.TryDoClick
  (const APart: TdxNavBarPart): Boolean;
begin
  Result := APart.MajorPartIndex = dxNavBarHamburgerMenuOverflowPanelItemLink;
  if Result then
    DoItemLinkClick
  else
    Result := inherited TryDoClick(APart);
end;

function TdxNavBarHamburgerMenuOverflowPanelViewInfo.AddGroup
  (AGroup: TdxNavBarGroup): TdxNavBarHamburgerMenuOverflowPanelGroup;
begin
  Result := TdxNavBarHamburgerMenuOverflowPanelGroup
    (InternalAdd(TdxNavBarHamburgerMenuOverflowPanelGroup));
  Result.Group := AGroup;
end;

function TdxNavBarHamburgerMenuOverflowPanelViewInfo.AddItemLink
  (AItemLink: TdxNavBarItemLink): TdxNavBarHamburgerMenuOverflowPanelLink;
begin
  Result := TdxNavBarHamburgerMenuOverflowPanelLink
    (InternalAdd(TdxNavBarHamburgerMenuOverflowPanelLink));
  Result.ItemLink := AItemLink;
end;

function TdxNavBarHamburgerMenuOverflowPanelViewInfo.IsSkinAvailable: Boolean;
begin
  Result := TdxNavBarHamburgerMenuPainter(Painter).IsSkinAvailable;
end;

procedure TdxNavBarHamburgerMenuOverflowPanelViewInfo.Calculate
  (const ABounds: TRect);
begin
  FRect := ABounds;
  CalculateBounds(FRect.Left, FRect.Top);
end;

procedure TdxNavBarHamburgerMenuOverflowPanelViewInfo.DoItemLinkClick;
var
  AItemLink: TdxNavBarItemLink;
begin
  AItemLink := TdxNavBarHamburgerMenuOverflowPanelLink
    (Items[TdxNavBarControllerAccess(ViewInfo.NavBar.Controller)
    .HotPart.MinorPartIndex]).ItemLink;
  TdxHamburgerHelper.NavBarItemLinkClick(ViewInfo.NavBar, AItemLink);
end;

function TdxNavBarHamburgerMenuOverflowPanelViewInfo.AllowCustomizing: Boolean;
begin
  Result := TdxNavBarHamburgerMenuBehaviorOptionsAccess
    (NavBar.OptionsBehavior.HamburgerMenu).AllowCustomizing;
end;

function TdxNavBarHamburgerMenuOverflowPanelViewInfo.GetClientOffset: TRect;
var
  AElement: TdxSkinElement;
begin
  AElement := GetSkinHelper.NavPaneOverflowPanel;
  if AElement = nil then
    Result := cxSimpleRect
  else
    Result := ScaleFactor.Apply(AElement.ContentOffset.Rect);
  Result := ScaleFactor.Apply(Result);
end;

function TdxNavBarHamburgerMenuOverflowPanelViewInfo.GetHeight: Integer;
begin
  if not IsVisible then
    Result := 0
  else
  begin
    Result := TdxNavBarViewInfoAccess(ViewInfo).GetOverflowPanelHeight +
      GetHeightAddon;
    dxAdjustToTouchableSize(Result, ScaleFactor);
  end;
  if IsSkinAvailable and (Result > 0) then
    Result := Max(Result, GetMinHeight);
end;

function TdxNavBarHamburgerMenuOverflowPanelViewInfo.GetImageWidthAddon
  : Integer;
var
  AElement: TdxSkinElement;
begin
  AElement := GetSkinHelper.NavPaneOverflowPanelItem;
  if AElement = nil then
    Result := inherited GetImageWidthAddon
  else
    Result := ScaleFactor.Apply
      (cxMarginsWidth(AElement.ContentOffset.Rect)) div 2;
end;

function TdxNavBarHamburgerMenuOverflowPanelViewInfo.
  GetItemSelectionWidth: Integer;
begin
  Result := FItemWidth;
end;

function TdxNavBarHamburgerMenuOverflowPanelViewInfo.GetSignWidth: Integer;
begin
  Result := GetItemSelectionWidth
end;

function TdxNavBarHamburgerMenuOverflowPanelViewInfo.UseSmallImages: Boolean;
begin
  Result := True;
end;

procedure TdxNavBarHamburgerMenuOverflowPanelViewInfo.CalculateClientRect
  (out ARect: TRect);
var
  AOffset: Integer;
begin
  AOffset := GetClientOffset.Left;
  ARect := cxRectContent(FRect, cxRect(AOffset, AOffset, AOffset, AOffset));
end;

procedure TdxNavBarHamburgerMenuOverflowPanelViewInfo.CalculateSignRect
  (const ARect: TRect);
begin
  inherited CalculateSignRect(ARect);
  FSignRect := cxRectCenter(ARect, FSignRect.Width, FSignRect.Height);
end;

procedure TdxNavBarHamburgerMenuOverflowPanelViewInfo.CalculateRect(X,
  Y: Integer);
begin
  // do nothing
end;

procedure TdxNavBarHamburgerMenuOverflowPanelViewInfo.ClearRects;
begin
  inherited ClearRects;
  FItemWidth := 0;
end;

function TdxNavBarHamburgerMenuOverflowPanelViewInfo.GetMinHeight: Integer;
var
  AElement: TdxSkinElement;
begin
  Result := GetImageHeight;
  AElement := GetSkinHelper.NavPaneOverflowPanelItem;
  if AElement <> nil then
    Inc(Result, ScaleFactor.Apply
      (cxMarginsHeight(AElement.ContentOffset.Rect)));
end;

function TdxNavBarHamburgerMenuOverflowPanelViewInfo.GetSkinHelper
  : TdxNavBarSkinBasedPainterHelper;
begin
  Result := TdxNavBarHamburgerMenuPainter(Painter).SkinHelper;
end;

function TdxNavBarHamburgerMenuOverflowPanelViewInfo.GetViewInfo
  : TdxNavBarHamburgerMenuViewInfo;
begin
  Result := inherited ViewInfo as TdxNavBarHamburgerMenuViewInfo;
end;

{ TdxNavBarHamburgerMenuSkinPainterHelper }

function TdxNavBarHamburgerMenuSkinPainterHelper.GetFullSkinName: TdxSkinName;
begin
  if not IsDefaultColorSchemeName then
    Result := inherited GetFullSkinName
  else
    Result := sdxDefaultColorSchemeName;
end;

function TdxNavBarHamburgerMenuSkinPainterHelper.GetSkinName: TdxSkinName;
begin
  if UseBuiltInSkin then
  begin
    if IsDefaultColorSchemeName then
      Result := ''
    else
      Result := FSkinName
  end
  else
    Result := inherited GetSkinName;
end;

function TdxNavBarHamburgerMenuSkinPainterHelper.GetSkinPainterData
  (var AData: TdxSkinInfo): Boolean;
begin
  if UseBuiltInSkin then
    Result := TryCreateSkinInfo(AData)
  else
    Result := inherited GetSkinPainterData(AData);
end;

function TdxNavBarHamburgerMenuSkinPainterHelper.HamburgerButton
  : TdxSkinElement;
var
  ASkinInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinInfo) then
    Result := ASkinInfo.HamburgerMenuHamburgerButton;
end;

function TdxNavBarHamburgerMenuSkinPainterHelper.HamburgerMenuNavPaneBackground
  : TdxSkinElement;
var
  ASkinInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinInfo) then
    Result := ASkinInfo.HamburgerMenuNavPaneBackground;
end;

function TdxNavBarHamburgerMenuSkinPainterHelper.HamburgerScrollDownButton
  : TdxSkinElement;
var
  ASkinInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinInfo) then
    Result := ASkinInfo.HamburgerMenuScrollDownButton;
end;

function TdxNavBarHamburgerMenuSkinPainterHelper.HamburgerScrollUpButton
  : TdxSkinElement;
var
  ASkinInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinInfo) then
    Result := ASkinInfo.HamburgerMenuScrollUpButton;
end;

function TdxNavBarHamburgerMenuSkinPainterHelper.NavBarBackground
  : TdxSkinElement;
var
  ASkinInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinInfo) then
  begin
    Result := ASkinInfo.HamburgerMenuBackground;
    if Result = nil then
      Result := inherited NavBarBackground;
  end;
end;

function TdxNavBarHamburgerMenuSkinPainterHelper.
  NavBarDistanceBetweenRootGroups: Integer;
var
  ASkinInfo: TdxSkinInfo;
begin
  if GetSkinPainterData(ASkinInfo) and
    (ASkinInfo.HamburgerMenuDistanceBetweenGroups <> nil) then
    Result := ASkinInfo.HamburgerMenuDistanceBetweenGroups.Value
  else
    Result := 0;
end;

function TdxNavBarHamburgerMenuSkinPainterHelper.NavBarChildGroup
  : TdxSkinElement;
var
  ASkinInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinInfo) then
    Result := ASkinInfo.HamburgerMenuChildGroup;
end;

function TdxNavBarHamburgerMenuSkinPainterHelper.NavBarChildGroupExpandButton
  (AIsClose: Boolean): TdxSkinElement;
var
  ASkinInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinInfo) then
    if AIsClose then
      Result := ASkinInfo.HamburgerMenuChildGroupOpenButton
    else
      Result := ASkinInfo.HamburgerMenuChildGroupCloseButton;
end;

function TdxNavBarHamburgerMenuSkinPainterHelper.NavBarChildItemOffset: Integer;
var
  ASkinInfo: TdxSkinInfo;
begin
  if GetSkinPainterData(ASkinInfo) and
    (ASkinInfo.HamburgerMenuChildItemOffset <> nil) then
    Result := ASkinInfo.HamburgerMenuChildItemOffset.Value
  else
    Result := -1;
end;

function TdxNavBarHamburgerMenuSkinPainterHelper.NavBarGroupButtonCaption
  : TdxSkinElement;
var
  ASkinInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinInfo) then
  begin
    Result := ASkinInfo.HamburgerMenuGroup;
    if Result = nil then
      Result := inherited NavBarGroupButtonCaption;
  end;
end;

function TdxNavBarHamburgerMenuSkinPainterHelper.NavBarGroupSigns
  (AIsClose: Boolean): TdxSkinElement;
var
  ASkinInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinInfo) then
    if AIsClose then
      Result := ASkinInfo.HamburgerMenuGroupCloseButton
    else
      Result := ASkinInfo.HamburgerMenuGroupOpenButton;
end;

function TdxNavBarHamburgerMenuSkinPainterHelper.NavBarItem: TdxSkinElement;
var
  ASkinInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinInfo) then
    Result := ASkinInfo.HamburgerMenuItem;
end;

function TdxNavBarHamburgerMenuSkinPainterHelper.NavPaneItem(ASelected: Boolean)
  : TdxSkinElement;
var
  ASkinInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinInfo) then
    Result := ASkinInfo.HamburgerMenuNavPaneItem;
end;

function TdxNavBarHamburgerMenuSkinPainterHelper.NavPaneOverflowPanelSign
  : TdxSkinElement;
var
  ASkinInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinInfo) then
    Result := ASkinInfo.HamburgerMenuNavPaneExpandButton;
end;

function TdxNavBarHamburgerMenuSkinPainterHelper.NavPaneOverflowPanelItem
  : TdxSkinElement;
var
  ASkinInfo: TdxSkinInfo;
begin
  Result := nil;
  if GetSkinPainterData(ASkinInfo) then
    Result := ASkinInfo.HamburgerMenuNavPaneItem;
end;

function TdxNavBarHamburgerMenuSkinPainterHelper.
  IsDefaultColorSchemeName: Boolean;
begin
  Result := FSkinName = sdxDefaultColorSchemeName;
end;

function TdxNavBarHamburgerMenuSkinPainterHelper.UseBuiltInSkin: Boolean;
begin
  Result := LookAndFeel.NativeStyle or not LookAndFeel.NativeStyle and
    (LookAndFeel.SkinName = '') or (LookAndFeel.SkinPainter = nil);
end;

{ TdxNavBarHamburgerMenuNavPaneIAccessibilityHelper }

function TdxNavBarHamburgerMenuNavPaneIAccessibilityHelper.ChildIsSimpleElement
  (AIndex: Integer): Boolean;
begin
  Result := False;
end;

function TdxNavBarHamburgerMenuNavPaneIAccessibilityHelper.GetChild
  (AIndex: Integer): TcxAccessibilityHelper;
begin
  if AIndex < inherited GetChildCount then
    Result := inherited GetChild(AIndex)
  else
    Result := NavPaneViewInfo.SignIAccessibilityHelper.GetHelper;
end;

function TdxNavBarHamburgerMenuNavPaneIAccessibilityHelper.
  GetChildCount: Integer;
begin
  Result := inherited GetChildCount + 1;
end;

function TdxNavBarHamburgerMenuNavPaneIAccessibilityHelper.GetParent
  : TcxAccessibilityHelper;
begin
  Result := NavPaneViewInfo.NavBar.IAccessibilityHelper.GetHelper;
end;

function TdxNavBarHamburgerMenuNavPaneIAccessibilityHelper.GetRole
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := cxROLE_SYSTEM_PANE;
end;

function TdxNavBarHamburgerMenuNavPaneIAccessibilityHelper.GetState
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := Parent.States[cxAccessibleObjectSelfID];
  if (Result and cxSTATE_SYSTEM_INVISIBLE = 0) and
    ((NavPaneViewInfo = nil) or not NavPaneViewInfo.ViewInfo.IsNavPaneVisible)
  then
    Result := Result or cxSTATE_SYSTEM_INVISIBLE;
end;

function TdxNavBarHamburgerMenuNavPaneIAccessibilityHelper.
  GetSupportedProperties(AChildID: TcxAccessibleSimpleChildElementID)
  : TcxAccessibleObjectProperties;
begin
  Result := [aopLocation, aopFocus];
end;

procedure TdxNavBarHamburgerMenuNavPaneIAccessibilityHelper.
  CheckItemIAccessibilityHelperCount;
begin
  NavPaneViewInfo.CheckItemIAccessibilityHelperCount;
end;

function TdxNavBarHamburgerMenuNavPaneIAccessibilityHelper.GetBounds: TRect;
begin
  Result := NavPaneViewInfo.Rect;
end;

function TdxNavBarHamburgerMenuNavPaneIAccessibilityHelper.GetItemCount
  : Integer;
begin
  Result := NavPaneViewInfo.ItemCount;
end;

function TdxNavBarHamburgerMenuNavPaneIAccessibilityHelper.
  GetItemIAccessibilityHelperCount: Integer;
begin
  Result := NavPaneViewInfo.ItemIAccessibilityHelperCount;
end;

function TdxNavBarHamburgerMenuNavPaneIAccessibilityHelper.
  GetItemIAccessibilityHelper(AIndex: Integer): IdxNavBarAccessibilityHelper;
begin
  Result := NavPaneViewInfo.ItemIAccessibilityHelpers[AIndex];
end;

function TdxNavBarHamburgerMenuNavPaneIAccessibilityHelper.GetNavPaneViewInfo
  : TdxNavBarHamburgerMenuNavPaneViewInfo;
begin
  Result := TdxNavBarHamburgerMenuViewInfo(TdxNavBarAccess(OwnerObjectControl)
    .ViewInfo).NavPane;
end;

{ TdxNavBarHamburgerMenuNavPaneItemAccessibilityHelper }

function TdxNavBarHamburgerMenuNavPaneItemAccessibilityHelper.
  ChildIsSimpleElement(AIndex: Integer): Boolean;
begin
  Result := True;
end;

procedure TdxNavBarHamburgerMenuNavPaneItemAccessibilityHelper.DoDefaultAction
  (AChildID: TcxAccessibleSimpleChildElementID);
var
  ANavBar: TdxCustomNavBar;
  I: Integer;
begin
  inherited;
  ANavBar := TdxCustomNavBar(OwnerObjectControl);
  if ANavBar.ViewInfo.GetGroupViewInfoByGroup(Group).IsCaptionVisible then
    TdxNavBarAccess(OwnerObjectControl).DoGroupMouseUp(Group)
  else
    for I := 0 to Group.LinkCount - 1 do
      if Group.Links[I].Item.Visible then
      begin
        TdxNavBarAccess(ANavBar).DoLinkMouseUp(Group.Links[I]);
        Break;
      end;
end;

function TdxNavBarHamburgerMenuNavPaneItemAccessibilityHelper.GetName
  (AChildID: TcxAccessibleSimpleChildElementID): string;
begin
  Result := Group.Caption;
end;

function TdxNavBarHamburgerMenuNavPaneItemAccessibilityHelper.GetParent
  : TcxAccessibilityHelper;
begin
  Result := (OwnerObject as TdxNavBarHamburgerMenuNavPaneViewInfo)
    .IAccessibilityHelper.GetNavBarHelper;
end;

function TdxNavBarHamburgerMenuNavPaneItemAccessibilityHelper.GetRole
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := cxROLE_SYSTEM_PUSHBUTTON;
end;

function TdxNavBarHamburgerMenuNavPaneItemAccessibilityHelper.GetState
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := Parent.States[cxAccessibleObjectSelfID];
  if (Result and cxSTATE_SYSTEM_INVISIBLE = 0) and
    (ItemIndex >= ViewInfo.NavPane.VisibleItemCount) then
    Result := Result or cxSTATE_SYSTEM_INVISIBLE
  else
  begin
    Result := Result or cxSTATE_SYSTEM_FOCUSABLE or cxSTATE_SYSTEM_SELECTABLE;
    if IsFocused then
      Result := Result or cxSTATE_SYSTEM_FOCUSED;
  end;
end;

function TdxNavBarHamburgerMenuNavPaneItemAccessibilityHelper.
  GetSupportedProperties(AChildID: TcxAccessibleSimpleChildElementID)
  : TcxAccessibleObjectProperties;
begin
  Result := [aopLocation, aopFocus, aopDefaultAction];
end;

procedure TdxNavBarHamburgerMenuNavPaneItemAccessibilityHelper.Click
  (AKey: Word);
begin
  inherited Click(AKey);
  TdxNavBarAccess(OwnerObjectControl).DoGroupMouseUp(Group);
end;

function TdxNavBarHamburgerMenuNavPaneItemAccessibilityHelper.GetBounds: TRect;
begin
  Result := ViewInfo.NavPane.Items[ItemIndex].Rect;
end;

function TdxNavBarHamburgerMenuNavPaneItemAccessibilityHelper.IsClickKey
  (AKey: Word): Boolean;
begin
  Result := inherited IsClickKey(AKey) or (AKey in [VK_RETURN, VK_SPACE]);
end;

function TdxNavBarHamburgerMenuNavPaneItemAccessibilityHelper.
  IsContainer: Boolean;
begin
  Result := False;
end;

function TdxNavBarHamburgerMenuNavPaneItemAccessibilityHelper.GetGroup
  : TdxNavBarGroup;
begin
  Result := ViewInfo.NavPane.Items[ItemIndex].GroupViewInfo.Group;
end;

function TdxNavBarHamburgerMenuNavPaneItemAccessibilityHelper.GetViewInfo
  : TdxNavBarHamburgerMenuViewInfo;
begin
  Result := (OwnerObject as TdxNavBarHamburgerMenuNavPaneViewInfo).ViewInfo;
end;

{ TdxNavBarHamburgerMenuNavPaneSignAccessibilityHelper }

function TdxNavBarHamburgerMenuNavPaneSignAccessibilityHelper.
  ChildIsSimpleElement(AIndex: Integer): Boolean;
begin
  Result := True;
end;

procedure TdxNavBarHamburgerMenuNavPaneSignAccessibilityHelper.DoDefaultAction
  (AChildID: TcxAccessibleSimpleChildElementID);
begin
  inherited;
  NavPaneViewInfo.ShowPopupMenu
    (cxRectCenter(GetScreenBounds(cxAccessibleObjectSelfID)));
end;

function TdxNavBarHamburgerMenuNavPaneSignAccessibilityHelper.GetName
  (AChildID: TcxAccessibleSimpleChildElementID): string;
begin
  Result := cxGetResourceString(@sdxNavigationPaneOverflowPanelCustomizeHint);
end;

function TdxNavBarHamburgerMenuNavPaneSignAccessibilityHelper.GetParent
  : TcxAccessibilityHelper;
begin
  Result := (OwnerObject as TdxNavBarHamburgerMenuNavPaneViewInfo)
    .IAccessibilityHelper.GetNavBarHelper;
end;

function TdxNavBarHamburgerMenuNavPaneSignAccessibilityHelper.GetRole
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := cxROLE_SYSTEM_PUSHBUTTON;
end;

function TdxNavBarHamburgerMenuNavPaneSignAccessibilityHelper.GetState
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := Parent.States[cxAccessibleObjectSelfID];
  if (Result and cxSTATE_SYSTEM_INVISIBLE = 0) and
    (not NavPaneViewInfo.ViewInfo.IsNavPaneVisible or cxRectIsEmpty(GetBounds))
  then
    Result := Result or cxSTATE_SYSTEM_INVISIBLE
  else
  begin
    Result := Result or cxSTATE_SYSTEM_FOCUSABLE;
    if IsFocused then
      Result := Result or cxSTATE_SYSTEM_FOCUSED;
  end;
end;

function TdxNavBarHamburgerMenuNavPaneSignAccessibilityHelper.
  GetSupportedProperties(AChildID: TcxAccessibleSimpleChildElementID)
  : TcxAccessibleObjectProperties;
begin
  Result := [aopLocation, aopFocus, aopDefaultAction];
end;

procedure TdxNavBarHamburgerMenuNavPaneSignAccessibilityHelper.Click
  (AKey: Word);
begin
  inherited Click(AKey);
  NavPaneViewInfo.ShowPopupMenu
    (cxRectCenter(GetScreenBounds(cxAccessibleObjectSelfID)));
end;

function TdxNavBarHamburgerMenuNavPaneSignAccessibilityHelper.GetBounds: TRect;
begin
  Result := NavPaneViewInfo.SignRect;
end;

function TdxNavBarHamburgerMenuNavPaneSignAccessibilityHelper.IsClickKey
  (AKey: Word): Boolean;
begin
  Result := inherited IsClickKey(AKey) or (AKey in [VK_RETURN, VK_SPACE]);
end;

function TdxNavBarHamburgerMenuNavPaneSignAccessibilityHelper.
  IsContainer: Boolean;
begin
  Result := False;
end;

function TdxNavBarHamburgerMenuNavPaneSignAccessibilityHelper.GetNavPaneViewInfo
  : TdxNavBarHamburgerMenuNavPaneViewInfo;
begin
  Result := TdxNavBarHamburgerMenuNavPaneViewInfo(FOwnerObject);
end;

{ TdxNavBarHamburgerMenuGroupAccessibilityHelper }

function TdxNavBarHamburgerMenuGroupAccessibilityHelper.GetState
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := Parent.States[cxAccessibleObjectSelfID];
  if (Result and cxSTATE_SYSTEM_INVISIBLE = 0) and
    TdxNavBarHamburgerMenuViewInfo(NavBar.ViewInfo).IsNavPaneVisible and
    (NavBar.ActiveGroup <> Group) and (Group.Parent = nil) then
    Result := cxSTATE_SYSTEM_INVISIBLE
  else
    Result := inherited GetState(AChildID);
end;

{ TdxNavBarHamburgerMenuGroupCaptionPanelAccessibilityHelper }

function TdxNavBarHamburgerMenuGroupCaptionPanelAccessibilityHelper.GetState
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := Parent.States[cxAccessibleObjectSelfID];
  if (Result and cxSTATE_SYSTEM_INVISIBLE = 0) and
    TdxNavBarHamburgerMenuViewInfo(NavBar.ViewInfo).IsNavPaneVisible and
    (Group.Parent = nil) then
    Result := cxSTATE_SYSTEM_INVISIBLE
  else
    Result := inherited GetState(AChildID);
end;

{ TdxNavBarHamburgerMenuSignAccessibilityHelper }

function TdxNavBarHamburgerMenuSignAccessibilityHelper.GetName
  (AChildID: TcxAccessibleSimpleChildElementID): string;
begin
  Result := 'Menu Button';
end;

initialization

dxNavBarViewsFactory.RegisterView(dxNavBarHamburgerMenu, 'HamburgerMenu',
  TdxNavBarHamburgerMenuPainter);

finalization

UnRegisterView(dxNavBarHamburgerMenu);

end.
