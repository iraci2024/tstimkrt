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

unit dxNavBarOffice11Views;

{$I cxVer.inc}

interface

uses
  Windows, Classes, Graphics, Menus, ImgList, Contnrs, Forms, Controls,
  RTLConsts,
  dxCore, cxClasses, cxContainer, cxControls, cxGraphics, dxCoreClasses,
  dxNavBar, dxNavBarBase, dxNavBarStyles,
  dxNavBarCustomPainters, dxNavBarBaseViews, cxGeometry, dxNavBarOfficeViews,
  dxNavBarExplorerViews, dxNavBarCollns,
  cxAccessibility, dxNavBarOverflowPanel, dxNavBarAccessibility;

const
  nbOverflowPanelSign = dxNavBarOverflowPanel.nbOverflowPanelSign;
  nbOverflowPanelItem = dxNavBarOverflowPanel.nbOverflowPanelGroup;
  nbSplitter = 3;
  nbItemPanelCollapseItem = 4;
  nbItemPanelCollapseBar = 5;

type
  TdxNavBarNavigationPaneHeaderPanelViewInfo = class;
  TdxNavBarNavigationPaneViewInfo = class;
  TdxNavBarNavigationPanePainter = class;
  TdxNavBarPopupControlViewInfo = class;
  TdxNavBarPopupControlViewInfoClass = class of TdxNavBarPopupControlViewInfo;

  { TdxNavBarOffice11ExplorerBar }

  TdxNavBarOffice11LinkViewInfo = class(TdxNavBarExplorerBarLinkViewInfo);
  // #Ch to avoid BC

  TdxNavBarOffice11GroupViewInfo = class(TdxNavBarExplorerBarGroupViewInfo)
  private
    function IsDefaultBgColor: Boolean;
    function IsDefaultCaptionColor: Boolean;
  public
    function BorderColor: TColor; override;
    function BgBackColor: TColor; override;
    function BgBackColor2: TColor; override;
    function BgAlphaBlend: Byte; override;
    function BgAlphaBlend2: Byte; override;
    function BgGradientMode: TdxBarStyleGradientMode; override;
    function CaptionBackColor: TColor; override;
    function CaptionBackColor2: TColor; override;
    function CaptionAlphaBlend: Byte; override;
    function CaptionAlphaBlend2: Byte; override;
    function CaptionGradientMode: TdxBarStyleGradientMode; override;
    function CaptionFontColor: TColor; override;
  end;

  TdxNavBarOffice11ViewInfo = class(TdxNavBarExplorerBarViewInfo)
  private
    function IsDefaultBgColor: Boolean;
  protected
    procedure CreateColors; override;
    procedure RefreshColors; override;
    procedure ReleaseColors; override;

    function GetGroupEdges: TPoint; override;
    function GetGroupSeparatorWidth: Integer; override;
    function GetLinksImageEdges: TRect; override;
  public
    function BgBackColor: TColor; override;
    function BgBackColor2: TColor; override;
    function BgAlphaBlend: Byte; override;
    function BgAlphaBlend2: Byte; override;
    function BgGradientMode: TdxBarStyleGradientMode; override;

    procedure AssignDefaultBackgroundStyle; override;
    procedure AssignDefaultGroupBackgroundStyle; override;
    procedure AssignDefaultGroupHeaderStyle; override;
    procedure AssignDefaultGroupHeaderActiveStyle; override;
    procedure AssignDefaultItemStyle; override;
  end;

  TdxNavBarOffice11Painter = class(TdxNavBarExplorerBarPainter)
  protected
    class function GetViewInfoClass: TdxNavBarViewInfoClass; override;
    class function GetGroupViewInfoClass: TdxNavBarGroupViewInfoClass; override;
    class function GetLinkViewInfoClass: TdxNavBarLinkViewInfoClass; override;

    class function ButtonPainterClass
      : TdxNavBarCustomButtonPainterClass; override;
    class function SignPainterClass: TdxNavBarCustomSignPainterClass; override;
  end;

  { TdxNavBarOffice11SignPainter }

  TdxNavBarOffice11SignPainter = class(TdxNavBarExplorerBarSignPainter)
  protected
    class procedure InternalDrawSign(ACanvas: TCanvas; ARect: TRect;
      AScaleFactor: TdxScaleFactor; AForeColor, ABackColor1,
      ABackColor2: TColor; AState: TdxNavBarObjectStates); override;
    class function PrepareBitmap(ACanvas: TCanvas; const ASize: TSize;
      ABitmap: TBitmap; ABackColor1, ABackColor2: TColor;
      AState: TdxNavBarObjectStates): TBitmap;
  end;

  { TdxNavBarOffice11ExplorerBarGroupViewInfo }

  TdxNavBarOffice11ExplorerBarGroupViewInfo = class
    (TdxNavBarExplorerBarGroupViewInfo)
  private
    function IsDefaultCaptionColor: Boolean;
  public
    function CaptionBorderColor: TColor; override;
    function CaptionBackColor: TColor; override;
    function CaptionBackColor2: TColor; override;
    function CaptionAlphaBlend: Byte; override;
    function CaptionAlphaBlend2: Byte; override;
    function CaptionGradientMode: TdxBarStyleGradientMode; override;
  end;

  TdxNavBarOffice11ExplorerBarViewInfo = class(TdxNavBarExplorerBarViewInfo)
  protected
    procedure CreateColors; override;
    procedure RefreshColors; override;
    procedure ReleaseColors; override;

    function CanSelectLinkByRect: Boolean; override;
    function GetGroupBorderOffsets: TRect; override;
    function GetGroupCaptionHeightAddon: Integer; override;
    function GetGroupCaptionImageIndent: Integer; override;
    function GetGroupEdges: TPoint; override;
    function GetGroupSeparatorWidth: Integer; override;
  public
    procedure AssignDefaultBackgroundStyle; override;
    procedure AssignDefaultButtonStyle; override;
    procedure AssignDefaultGroupBackgroundStyle; override;
    procedure AssignDefaultGroupHeaderStyle; override;
    procedure AssignDefaultItemStyle; override;
    procedure AssignDefaultItemDisabledStyle; override;
    procedure AssignDefaultNavigationPaneHeaderStyle; override;
  end;

  TdxNavBarOffice11ExplorerBarControllerState = (ecsOverSizeGrip);
  TdxNavBarOffice11ExplorerBarControllerStates = set of
    TdxNavBarOffice11ExplorerBarControllerState;

  TdxNavBarOffice11ExplorerBarController = class(TdxNavBarExplorerBarController)
  private
    FInternalState: TdxNavBarOffice11ExplorerBarControllerStates;
    function GetMouseOverSizeGrip: Boolean;
    procedure SetMouseOverSizeGrip(AValue: Boolean);
    property MouseOverSizeGrip: Boolean read GetMouseOverSizeGrip
      write SetMouseOverSizeGrip;
  protected
    procedure DoMouseDown(AButton: TMouseButton; AShift: TShiftState;
      const APoint: TPoint); override;
    procedure DoMouseMove(AShift: TShiftState; const APoint: TPoint); override;
    function GetCursor: HIcon; override;
  end;

  TdxNavBarOffice11ExplorerBarPainter = class(TdxNavBarExplorerBarPainter)
  protected
    class function GetViewInfoClass: TdxNavBarViewInfoClass; override;
    class function GetGroupViewInfoClass: TdxNavBarGroupViewInfoClass; override;
    function GetControllerClass: TdxNavBarControllerClass; override;

    class function SignPainterClass: TdxNavBarCustomSignPainterClass; override;
  public
    procedure DrawNavBarControl; override;
    procedure DrawGroupControlSplitter(AGroupViewInfo
      : TdxNavBarExplorerBarGroupViewInfo); override;
  end;

  TdxNavBarOffice11ExplorerBarSignPainter = class
    (TdxNavBarExplorerBarSignPainter)
  protected
    class procedure DrawSignSelection(ACanvas: TCanvas; ARect: TRect;
      AForeColor, ABackColor1, ABackColor2: TColor;
      AState: TdxNavBarObjectStates); override;
  end;

  { TdxNavBarNavigationPane }

  TdxNavBarNavigationPaneGroupViewInfo = class(TdxNavBarGroupViewInfo)
  private
    function IsDefaultCaptionColor: Boolean;
  protected
    function GetActiveScrollBarBounds: TRect; override;
    function GetImageIndent: Integer; override;
    function IsTopLevel: Boolean; override;
  public
    function CaptionBorderColor: TColor; override;
    function CaptionBackColor: TColor; override;
    function CaptionBackColor2: TColor; override;
    function CaptionAlphaBlend: Byte; override;
    function CaptionAlphaBlend2: Byte; override;
    function CaptionGradientMode: TdxBarStyleGradientMode; override;
  end;

  TdxNavBarOffice11NavPaneGroupViewInfo = class
    (TdxNavBarNavigationPaneGroupViewInfo);

  TdxNavBarOffice11NavPaneLinkViewInfo = class(TdxNavBarOffice3LinkViewInfo)
  public
    function SeparatorColor: TColor; override;
  end;

  TdxNavBarNavigationPaneCustomViewInfo = class(TdxNavBarPartViewInfo)
  private
    function GetViewInfo: TdxNavBarNavigationPaneViewInfo;
    function GetPainter: TdxNavBarNavigationPanePainter;
  protected
    property ViewInfo: TdxNavBarNavigationPaneViewInfo read GetViewInfo;
    property Painter: TdxNavBarNavigationPanePainter read GetPainter;
  end;

  TdxNavBarItemCollectionItemAccessibilityHelperClass = class of
    TdxNavBarItemCollectionItemAccessibilityHelper;

  { TdxNavBarOverflowPanelViewInfo }

  TdxNavBarOverflowPanelViewInfo = class(TdxNavBarCustomOverflowPanelViewInfo)
  private
    FIAccessibilityHelper: IdxNavBarAccessibilityHelper;
    FItemIAccessibilityHelpers: TInterfaceList;
    FSignIAccessibilityHelper: IdxNavBarAccessibilityHelper;

    function GetIAccessibilityHelper: IdxNavBarAccessibilityHelper;
    function GetItemIAccessibilityHelper(AIndex: Integer)
      : IdxNavBarAccessibilityHelper;
    function GetItemIAccessibilityHelperCount: Integer;
    procedure SetItemIAccessibilityHelperCount(Value: Integer);
    function GetSignIAccessibilityHelper: IdxNavBarAccessibilityHelper;
  protected
    function AllowCustomizing: Boolean; override;
    function GetAccessibilityHelperClass
      : TdxNavBarCustomAccessibilityHelperClass; virtual;
    function GetItemAccessibilityHelperClass
      : TdxNavBarItemCollectionItemAccessibilityHelperClass; virtual;
    function GetSignAccessibilityHelperClass
      : TdxNavBarCustomAccessibilityHelperClass; virtual;
  public
    constructor Create(AViewInfo: TdxNavBarViewInfo); override;
    destructor Destroy; override;

    procedure CheckItemIAccessibilityHelperCount;
    property IAccessibilityHelper: IdxNavBarAccessibilityHelper
      read GetIAccessibilityHelper;
    property ItemIAccessibilityHelperCount: Integer
      read GetItemIAccessibilityHelperCount;
    property ItemIAccessibilityHelpers[AIndex: Integer]
      : IdxNavBarAccessibilityHelper read GetItemIAccessibilityHelper;
    property SignIAccessibilityHelper: IdxNavBarAccessibilityHelper
      read GetSignIAccessibilityHelper;
  end;

  { TdxNavBarItemPanelViewInfoItem }

  TdxNavBarItemPanelViewInfoItem = class
  private
    function GetCaption: string;
  protected
    procedure OffsetRects(dX, dY: Integer);
  public
    Index: Integer;
    ItemLink: TdxNavBarItemLink;
    Font: TFont;
    Rect: TRect;
    TextRect: TRect;
    ImageRect: TRect;
    ImageList: TCustomImageList;
    ImageIndex: Integer;

    property Caption: string read GetCaption;
  end;

  TdxNavBarItemPanelViewInfo = class(TdxNavBarNavigationPaneCustomViewInfo)
  private
    FActiveGroupViewInfo: TdxNavBarGroupViewInfo;
    FCollapseBarIAccessibilityHelper: IdxNavBarAccessibilityHelper;
    FCollapseBarGroup: TdxNavBarGroup;
    FCollapseBarRect: TRect;
    FItemIAccessibilityHelpers: TInterfaceList;
    FIAccessibilityHelper: IdxNavBarAccessibilityHelper;
    FItems: TObjectList;
    FRect: TRect;

    FIsCollapseMode: Boolean;

    function GetCollapseBarIAccessibilityHelper: IdxNavBarAccessibilityHelper;
    function GetIAccessibilityHelper: IdxNavBarAccessibilityHelper;
    function GetItemCount: Integer;
    function GetItemIAccessibilityHelperCount: Integer;
    function GetItemIAccessibilityHelper(AIndex: Integer)
      : IdxNavBarAccessibilityHelper;
    function GetItems(AIndex: Integer): TdxNavBarItemPanelViewInfoItem;
    procedure SetActiveGroupViewInfo(AValue: TdxNavBarGroupViewInfo);
    procedure SetItemIAccessibilityHelperCount(Value: Integer);

    function AddItem: TdxNavBarItemPanelViewInfoItem;
    procedure ClearItems;
    procedure CreateItems;
  protected
    procedure CalculateBounds(var X, Y: Integer); virtual;

    function GetMinHeight: Integer;

    procedure CorrectBounds(AHeightDifference: Integer);
    function GetAccessibilityHelperClass
      : TdxNavBarCustomAccessibilityHelperClass;
    function GetCollapseBarAccessibilityHelperClass
      : TdxNavBarCustomAccessibilityHelperClass;
    function GetCollapseBarFont: TFont;
    function GetCollapseBarText: string;
    function GetItemAccessibilityHelperClass
      : TdxNavBarItemCollectionItemAccessibilityHelperClass;
    function GetItemFont(AIndex: Integer): TFont;
    function GetItemIndexAtPos(const pt: TPoint): Integer;
    function GetPartAtPos(const pt: TPoint): TdxNavBarPart;

    function GetCollapseBarCaptionIndent: Integer;

    property IAccessibilityHelper: IdxNavBarAccessibilityHelper
      read GetIAccessibilityHelper;
    property ActiveGroupViewInfo: TdxNavBarGroupViewInfo
      read FActiveGroupViewInfo write SetActiveGroupViewInfo;
    property CollapseBarFont: TFont read GetCollapseBarFont;
    property CollapseBarText: string read GetCollapseBarText;
    property CollapseBarRect: TRect read FCollapseBarRect;
    property ItemCount: Integer read GetItemCount;
    property Items[AIndex: Integer]: TdxNavBarItemPanelViewInfoItem
      read GetItems;
    property Rect: TRect read FRect;
  public
    constructor Create(AViewInfo: TdxNavBarViewInfo); override;
    destructor Destroy; override;

    procedure CheckItemIAccessibilityHelperCount;
    procedure DoRightToLeftConversion;

    property CollapseBarIAccessibilityHelper: IdxNavBarAccessibilityHelper
      read GetCollapseBarIAccessibilityHelper;
    property ItemIAccessibilityHelperCount: Integer
      read GetItemIAccessibilityHelperCount;
    property ItemIAccessibilityHelpers[AIndex: Integer]
      : IdxNavBarAccessibilityHelper read GetItemIAccessibilityHelper;
  end;

  { TdxNavBarCollapsedViewCalculator }

  TdxNavBarCollapsedViewCalculator = class
  strict private
    FIsCalculationNeeded: Boolean;
    procedure CalculateGroupsMaxImageSize;
  strict protected
    FImageHeightPeer: Boolean;
    FMaxImageSize: TSize;
    FViewInfo: TdxNavBarViewInfo;
    function CreateGroupViewInfo(AGroup: TdxNavBarGroup)
      : TdxNavBarGroupViewInfo; virtual;
    function GetMaxElementSize: Integer; virtual;
    procedure CalculateGroupMaxImageSize(AGroupViewInfo
      : TdxNavBarGroupViewInfo); virtual;
    procedure CalculatePanelsMaxImageSize; virtual;

    procedure CalculateItemMaxImageSize(AItemViewInfo
      : TdxNavBarCustomItemViewInfo);
  protected
    property ImageHeightPeer: Boolean read FImageHeightPeer;
    property MaxImageSize: TSize read FMaxImageSize;
  public
    constructor Create(AViewInfo: TdxNavBarViewInfo);

    function GetCollapsedWidth: Integer;
    function GetMinExpandedWidth: Integer;
    procedure CalculateMaxImageSize;
    procedure Reset;
  end;

  { TdxNavBarNavigationPaneOverflowPanelPopupMenu }

  TdxNavBarNavigationPaneOverflowPanelPopupMenu = class
    (TdxNavBarCustomOverflowPanelPopupMenu)
  strict private
    procedure DoMoreButtonsClick(Sender: TObject);
    procedure DoFewerButtonsClick(Sender: TObject);
  strict protected
    procedure PopulateItems(ADefaultImageIndex: Integer); override;
  end;

  { TdxNavBarNavigationPaneViewInfo }

  TdxNavBarNavigationPaneViewInfo = class(TdxNavBarOffice3ViewInfo)
  private
    FActiveGroupCaptionPanelIAccessibilityHelper: IdxNavBarAccessibilityHelper;

    FSplitterRect: TRect;

    FOverflowPanelViewInfo: TdxNavBarCustomOverflowPanelViewInfo;
    FItemPanelViewInfo: TdxNavBarItemPanelViewInfo;

    FPopupMenu: TdxNavBarCustomOverflowPanelPopupMenu;

    FCollapsedViewCalculator: TdxNavBarCollapsedViewCalculator;

    function GetPainter: TdxNavBarNavigationPanePainter;

    function GetActiveGroupCaptionPanelIAccessibilityHelper
      : IdxNavBarAccessibilityHelper;
    function GetHeaderSignIAccessibilityHelper: IdxNavBarAccessibilityHelper;

    function GetOverflowPanelIAccessibilityHelper: IdxNavBarAccessibilityHelper;
    function GetOverflowPanelSignIAccessibilityHelper
      : IdxNavBarAccessibilityHelper;
    function GetOverflowPanelItemIAccessibilityHelper(AIndex: Integer)
      : IdxNavBarAccessibilityHelper;

    // OverflowPanel
    function GetOverflowPanelImageList: TImageList;
    function GetOverflowPanelItemCount: Integer;
    function GetOverflowPanelItems(AIndex: Integer)
      : TdxNavBarOverflowPanelViewInfoItem;
    function GetOverflowPanelPopupMenu: TPopupMenu;
    function GetOverflowPanelRect: TRect;
    function GetOverflowPanelSignRect: TRect;
    function GetOverflowPanelViewInfo: TdxNavBarOverflowPanelViewInfo;
    function GetOverflowPanelVisibleItemCount: Integer;

    function IsActiveGroupVisible: Boolean;
    function IsGroupReflectionNeeded: Boolean;
    function GetMinHeight: Integer;
    function GetRealGroupStartIndex: Integer;
    function GetRealGroupCount: Integer;
    procedure RecreateImageList;

    function IsDefaultHeaderColor: Boolean;
    function IsDefaultOverflowPanelColor: Boolean;
    function IsDefaultBottomScrollButtonColor: Boolean;
    function IsDefaultTopScrollButtonColor: Boolean;

    procedure OnOverflowPanelSignButtonClickHandler
      (const AClientMousePos: TPoint);
  protected
    procedure CreateColors; override;
    procedure RefreshColors; override;
    procedure ReleaseColors; override;

    // Sizes
    function GetGroupEdges: TPoint; override;
    function GetGroupSeparatorWidth: Integer; override;
    function GetGroupBorderOffsets: TRect; override;
    function GetGroupCaptionImageIndent: Integer; override;
    function GetGroupCaptionHeightAddon: Integer; override;
    function GetGroupCaptionPanelAccessibilityHelperClass
      : TdxNavBarCustomAccessibilityHelperClass; override;
    function GetNavBarCollapsedWidth: Integer; override;
    function GetNavBarMinExpandedWidth: Integer; override;
    function GetOverflowPanelHeight: Integer; override;
    function GetSpaceBetweenGroups: Integer; override;
    // Conditions
    function CanHasGroupViewAsIconView: Boolean; override;
    function CanHasHeader: Boolean; override;
    function CanHasImageInGroupCaption: Boolean; override;
    function CanGroupCaptionBoundsByImage: Boolean; override;
    function IsBottomBorderNeeded: Boolean;
    function IsCollapsed: Boolean;
    function IsGroupPopupControlSizable: Boolean; override;
    function IsHeaderVisible: Boolean; override;
    function IsSplitterVisible: Boolean;
    function IsTopBorderNeeded: Boolean;
    function HasItemPanel: Boolean; virtual;
    function HasOverflowPanel: Boolean; virtual;

    // Correction
    procedure CorrectBounds; override;
    procedure CorrectPanelsBounds; virtual;

    // Calculations
    procedure DoCreateGroupsInfo; override;
    procedure DoCalculateBounds(X, Y: Integer); override;
    procedure InternalCalculateMaxImageSize; override;

    function CreateCollapsedViewCalculator
      : TdxNavBarCollapsedViewCalculator; virtual;
    function GetItemPanelRect: TRect;
    function GetPopupTopPos: Integer; override;

    function GetBoundsUpdateType: TdxNavBarChangeType; override;

    function GetActiveGroupCaptionPanelAccessibilityHelperClass
      : TdxNavBarCustomAccessibilityHelperClass; virtual;
    function GetOverflowPanelViewInfoClass
      : TdxNavBarOverflowPanelViewInfoClass; virtual;

    function GetPartIAccessibilityHelper(const APart: TdxNavBarPart)
      : IdxNavBarAccessibilityHelper; override;
    function GetNavPanePartIAccessibilityHelper(const APart: TdxNavBarPart)
      : IdxNavBarAccessibilityHelper; virtual;
    property OverflowPanelViewInfo: TdxNavBarOverflowPanelViewInfo
      read GetOverflowPanelViewInfo;
  public
    constructor Create(APainter: TdxNavBarPainter); override;
    destructor Destroy; override;

    procedure CreateInfo; override;
    procedure CreateItemPanelViewInfo;
    procedure CreateOverflowPanelInfo(AItemCount: Integer;
      AClearOld: Boolean); virtual;
    procedure CalculateMaxImageSize; virtual;
    procedure CalculateSplitterBounds(var X, Y: Integer); virtual;
    procedure ClearRects; override;

    function GetHeaderSignDirection: TcxDirection; virtual;
    function GetSplitterHeight: Integer; virtual;

    function GetViewInfoAtDragPosition(const pt: TPoint;
      var ItemGroup: TdxNavBarGroupViewInfo;
      var Item1, Item2: TdxNavBarLinkViewInfo): Integer; override;

    function IsPtIncNavigationPaneOverflowPanelItemCount
      (const pt: TPoint): Boolean;
    function IsPtDecNavigationPaneOverflowPanelItemCount
      (const pt: TPoint): Boolean;
    function IsPtNavigationPaneHeader(const pt: TPoint): Boolean;
    function IsPtNavigationPaneHeaderSign(const pt: TPoint): Boolean;
    function IsPtNavigationPaneOverflowPanel(const pt: TPoint): Boolean;
    function IsPtNavigationPaneOverflowPanelSign(const pt: TPoint): Boolean;
    function IsPtNavigationPaneSplitter(const pt: TPoint): Boolean;
    function IsPtNavigationPaneItemPanel(const pt: TPoint): Boolean;

    procedure DoShowPopupMenu(const APoint: TPoint);
    procedure DoUpdatePopupMenu; virtual;

    function FindGroupWithAccel(AKey: Word): TdxNavBarGroup; override;
    function FindLinkWithAccel(AKey: Word): TdxNavBarItemLink; override;
    function GetLinkAtPos(const pt: TPoint): TdxNavBarItemLink; override;

    function BorderColor: TColor; override;
    function CollapseBarFontColor: TColor; virtual;

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
    function HeaderBackColor: TColor; override;
    function HeaderBackColor2: TColor; override;
    function HeaderAlphaBlend: Byte; override;
    function HeaderAlphaBlend2: Byte; override;
    function HeaderGradientMode: TdxBarStyleGradientMode; override;
    function HeaderFontColor: TColor; override;
    function OverflowPanelBackColor: TColor; override;
    function OverflowPanelBackColor2: TColor; override;
    function OverflowPanelAlphaBlend: Byte; override;
    function OverflowPanelAlphaBlend2: Byte; override;
    function OverflowPanelGradientMode: TdxBarStyleGradientMode; override;
    function SplitterBackColor: TColor; override;
    function SplitterBackColor2: TColor; override;
    function SplitterGradientMode: TdxBarStyleGradientMode; override;

    procedure AssignDefaultBackgroundStyle; override;
    procedure AssignDefaultButtonStyle; override;
    procedure AssignDefaultGroupBackgroundStyle; override;
    procedure AssignDefaultGroupHeaderStyle; override;
    procedure AssignDefaultGroupHeaderActiveStyle; override;
    procedure AssignDefaultGroupHeaderHotTrackedStyle; override;
    procedure AssignDefaultGroupHeaderPressedStyle; override;
    procedure AssignDefaultItemStyle; override;
    procedure AssignDefaultItemDisabledStyle; override;
    procedure AssignDefaultNavigationPaneHeaderStyle; override;

    procedure DoRightToLeftConversion; override;

    property HeaderRect;
    property HeaderSignIAccessibilityHelper: IdxNavBarAccessibilityHelper
      read GetHeaderSignIAccessibilityHelper;
    property HeaderSignRect;

    property ItemPanelRect: TRect read GetItemPanelRect;

    property OverflowPanelItemCount: Integer read GetOverflowPanelItemCount;
    property OverflowPanelItems[Index: Integer]
      : TdxNavBarOverflowPanelViewInfoItem read GetOverflowPanelItems;
    property OverflowPanelRect: TRect read GetOverflowPanelRect;
    property OverflowPanelSignRect: TRect read GetOverflowPanelSignRect;
    property OverflowPanelVisibleItemCount: Integer
      read GetOverflowPanelVisibleItemCount;

    property SizeGripRect;
    property SplitterRect: TRect read FSplitterRect;

    property ActiveGroupCaptionPanelIAccessibilityHelper
      : IdxNavBarAccessibilityHelper
      read GetActiveGroupCaptionPanelIAccessibilityHelper;
    property ImageList: TImageList read GetOverflowPanelImageList;
    property ItemPanelViewInfo: TdxNavBarItemPanelViewInfo
      read FItemPanelViewInfo;
    property PopupMenu: TPopupMenu read GetOverflowPanelPopupMenu;
    property Painter: TdxNavBarNavigationPanePainter read GetPainter;

    property NavPanePartIAccessibilityHelpers[const APart: TdxNavBarPart]
      : IdxNavBarAccessibilityHelper read GetNavPanePartIAccessibilityHelper;
    property OverflowPanelSignIAccessibilityHelper: IdxNavBarAccessibilityHelper
      read GetOverflowPanelSignIAccessibilityHelper;
    property OverflowPanelIAccessibilityHelper: IdxNavBarAccessibilityHelper
      read GetOverflowPanelIAccessibilityHelper;
  end;

  TdxNavBarOffice11NavPaneViewInfo = class(TdxNavBarNavigationPaneViewInfo);

  { TdxNavBarNavigationPaneController }

  TdxNavBarNavPanePartState = TdxNavBarPartState;

  TdxNavBarNavigationPaneController = class(TdxNavBarNavigationBarController)
  private
    function IsAnyItemHotTracked: Boolean;
    function IsOverflowPanelGroupHotTracked: Boolean;
    function GetNavPanePartState(const APart: TdxNavBarPart)
      : TdxNavBarNavPanePartState;
    function GetViewInfo: TdxNavBarNavigationPaneViewInfo;
    function GetOverflowPanelGroup(AIndex: Integer): TdxNavBarGroup;
    procedure CalcOverflowPanelHintRect(AItem: TObject; var ARect: TRect);

    property OverflowPanelGroup[AIndex: Integer]: TdxNavBarGroup
      read GetOverflowPanelGroup;
  protected
    function CanFocusOnClick(const APoint: TPoint): Boolean; override;
    function CanHasPopupControl: Boolean; override;
    function CreateGroupPopupControl: TdxNavBarCustomPopupControl; override;
    function GetCursor: HIcon; override;
    function GetPartAtPos(const APoint: TPoint): TdxNavBarPart; override;
    function GetPartState(const APart: TdxNavBarPart)
      : TdxNavBarPartState; override;

    procedure DoClick(const APart: TdxNavBarPart); override;
    procedure DoMouseMove(AShift: TShiftState; const APoint: TPoint); override;
    procedure DoMouseLeave; override;
    procedure DoSetHotPart(const APart: TdxNavBarPart); override;
    procedure DoSetPressedPart(const APart: TdxNavBarPart); override;

    function GetRealCollapsed: Boolean; override;
    function GetCollapsible: Boolean; override;
    procedure SetCollapsed(AValue: Boolean); override;

    function CalcHintRect(AHintInfo: THintInfo): TRect; override;
    procedure DoShowHint(var AHintInfo: THintInfo); override;
    procedure DoShowOverflowPanelHint(var AHintInfo: THintInfo); virtual;
    function GetNavPaneItemHintRect(const ACursorPos: TPoint): TRect; virtual;
    function GetNavPaneItemHintText: string; virtual;
    function GetNavPaneItemHintCursorRect: TRect; virtual;

    procedure DoOverflowPanelItemClick;
    procedure DoOverflowPanelSignClick;
    procedure DoCollapseBarClick;
    procedure DoCollapseItemClick;
    procedure DoSplitterDrag(const APoint: TPoint);
  public
    procedure ClosePopupControl;
    procedure ShowPopupControl;

    property Collapsed;
    property Collapsible;
    property DroppedDownPart;
    property ViewInfo: TdxNavBarNavigationPaneViewInfo read GetViewInfo;
  end;

  TdxNavBarNavigationPanePainter = class(TdxNavBarOffice3Painter)
  private
    FOverflowPanelPainter: TdxNavBarOverflowPanelPainter;

    function GetController: TdxNavBarNavigationPaneController;
    function GetOverflowPanelViewInfo: TdxNavBarOverflowPanelViewInfo;
    function GetViewInfo: TdxNavBarNavigationPaneViewInfo;

    procedure DoDrawCollapseItem(AItemViewInfo: TdxNavBarItemPanelViewInfoItem;
      AState: TdxNavBarNavPanePartState);
    procedure DrawItemPanelPartFocusRect(const APartRect: TRect);
    procedure InternalDrawVerticalText(AFont: TFont; const AText: string;
      const ARect: TRect);
    procedure DrawOverflowPanelItemBackground(ACanvas: TCanvas;
      AState: TdxNavBarNavPanePartState; const ARect: TRect); overload;
    procedure DrawOverflowPanelItemBackground(ACanvas: TCanvas;
      const APart: TdxNavBarPart; const ARect: TRect); overload;

    procedure OnDrawOverflowPanelPopupMenuItemHandler(ACanvas: TCanvas;
      ARect: TRect; AImageList: TCustomImageList; AImageIndex: Integer;
      AText: string; AState: TdxNavBarObjectStates);
  protected
    class function ButtonPainterClass
      : TdxNavBarCustomButtonPainterClass; override;
    class function GetViewInfoClass: TdxNavBarViewInfoClass; override;
    class function GetGroupViewInfoClass: TdxNavBarGroupViewInfoClass; override;
    class function GetHeaderPanelViewInfoClass
      : TdxNavBarHeaderPanelViewInfoClass; override;
    class function GetPopupControlViewInfoClass
      : TdxNavBarPopupControlViewInfoClass; virtual;
    class function OverflowPanelBackgroundPainterClass
      : TdxNavBarCustomButtonPainterClass; override;
    function GetControllerClass: TdxNavBarControllerClass; override;
    function UseHeaderCustomDrawing: Boolean; override;

    function GetDefaultOverflowPanelBitmap: TBitmap;
    function GetNavPanePartState(const APart: TdxNavBarPart)
      : TdxNavBarNavPanePartState;
    procedure CreateOverflowPanelPainter;

    function InternalCreateOverflowPainter: TdxNavBarOverflowPanelPainter;
      virtual; // for internal use
    procedure DoDrawOverflowPanel; virtual;
    procedure DoDrawSplitter; virtual;
  public
    constructor Create(ANavBar: TdxCustomNavBar); override;
    destructor Destroy; override;

    procedure DrawNavBarControl; override;
    procedure DrawGroupBackground(AGroupViewInfo
      : TdxNavBarGroupViewInfo); override;
    procedure DrawGroupCaption(AGroupViewInfo: TdxNavBarGroupViewInfo);
      override;
    procedure DrawHintWindow(AHintWindow: TdxNavBarHintWindow); override;
    procedure DrawItemPanel; virtual;
    procedure DrawItemsRect(AGroupViewInfo: TdxNavBarGroupViewInfo); override;
    procedure DrawGroupCaptionButton(AGroupViewInfo
      : TdxNavBarGroupViewInfo); override;
    procedure DrawCollapseBar(AItemPanelViewInfo
      : TdxNavBarItemPanelViewInfo); virtual;
    procedure DrawCollapseElementBackground(const ARect: TRect;
      AState: TdxNavBarNavPanePartState); virtual;
    procedure DrawCollapseItem(AItemViewInfo: TdxNavBarItemPanelViewInfoItem;
      AState: TdxNavBarNavPanePartState); virtual;
    procedure DrawBorder; virtual;
    procedure DrawHeaderBackground; override;
    procedure DrawHeaderSign; override;
    procedure DrawPopupControl(ACanvas: TcxCanvas;
      AViewInfo: TdxNavBarPopupControlViewInfo); virtual;
    procedure DrawOverflowPanel; virtual;
    procedure DrawOverflowPanelBackground; virtual;
    procedure DrawOverflowPanelSign; virtual;
    procedure DrawOverflowPanelItem
      (AItem: TdxNavBarOverflowPanelViewInfoItem); virtual;
    procedure DrawOverflowPanelItems; virtual;
    procedure DrawOverflowPanelHintWindow(ACanvas: TCanvas;
      const ARect: TRect); virtual;
    procedure DrawSplitter; virtual;
    procedure DrawSplitterSign; virtual;
    procedure DrawPopupMenuItem(ACanvas: TCanvas; ARect: TRect;
      AImageList: TCustomImageList; AImageIndex: Integer; AText: string;
      State: TdxNavBarObjectStates); virtual;

    property Controller: TdxNavBarNavigationPaneController read GetController;
    property OverflowPanelViewInfo: TdxNavBarOverflowPanelViewInfo
      read GetOverflowPanelViewInfo;
    property ViewInfo: TdxNavBarNavigationPaneViewInfo read GetViewInfo;
  end;

  TdxNavBarNavigationPaneButtonPainter = class(TdxNavBarCustomButtonPainter)
  protected
    class procedure InternalDrawButton(ACanvas: TCanvas; ARect: TRect;
      APicture: TPicture; AColor1, AColor2: TColor;
      AAlphaBlend1, AAlphaBlend2: Byte; AGradientMode: TdxBarStyleGradientMode;
      ABorderColor: TColor; AState: TdxNavBarObjectStates;
      AScaleFactor: TdxScaleFactor); override;
  end;

  TdxNavBarOffice11NavPaneButtonPainter = class
    (TdxNavBarNavigationPaneButtonPainter);

  TdxNavBarOffice11NavPanePainter = class(TdxNavBarNavigationPanePainter)
  protected
    class function GetGroupViewInfoClass: TdxNavBarGroupViewInfoClass; override;
    class function GetLinkViewInfoClass: TdxNavBarLinkViewInfoClass; override;
    class function GetViewInfoClass: TdxNavBarViewInfoClass; override;
    class function ButtonPainterClass
      : TdxNavBarCustomButtonPainterClass; override;
  end;

  TdxNavBarOffice11NavPaneGroupButtonPainter = class
    (TdxNavBarCustomButtonPainter)
  protected
    class procedure InternalDrawButton(ACanvas: TCanvas; ARect: TRect;
      APicture: TPicture; AColor1, AColor2: TColor;
      AAlphaBlend1, AAlphaBlend2: Byte; AGradientMode: TdxBarStyleGradientMode;
      ABorderColor: TColor; AState: TdxNavBarObjectStates;
      AScaleFactor: TdxScaleFactor); override;
  end;

  { TdxNavBarItemCollectionItemAccessibilityHelper }

  TdxNavBarItemCollectionItemAccessibilityHelper = class
    (TdxNavBarCustomAccessibilityHelper) // for internal use
  private
    FItemIndex: Integer;
  public
    property ItemIndex: Integer read FItemIndex write FItemIndex;
  end;

  { TdxNavBarItemCollectionAccessibilityHelper }

  TdxNavBarItemCollectionAccessibilityHelper = class
    (TdxNavBarCustomAccessibilityHelper) // for internal use
  protected
    function GetChild(AIndex: Integer): TcxAccessibilityHelper; override;
    function GetChildCount: Integer; override;

    procedure CheckItemIAccessibilityHelperCount; virtual; abstract;

    function GetItemCount: Integer; virtual; abstract;
    function GetItemIAccessibilityHelperCount: Integer; virtual; abstract;
    function GetItemIAccessibilityHelper(AIndex: Integer)
      : IdxNavBarAccessibilityHelper; virtual; abstract;
    function IsContainer: Boolean; override;

    property ItemCount: Integer read GetItemCount;
    property ItemIAccessibilityHelperCount: Integer
      read GetItemIAccessibilityHelperCount;
    property ItemIAccessibilityHelpers[AIndex: Integer]
      : IdxNavBarAccessibilityHelper read GetItemIAccessibilityHelper;
  end;

  TdxNavBarNavigationPaneOverflowPanelAccessibilityHelper = class
    (TdxNavBarItemCollectionAccessibilityHelper) // for internal use
  private
    function GetViewInfo: TdxNavBarOverflowPanelViewInfo;
  protected
    function ChildIsSimpleElement(AIndex: Integer): Boolean; override;
    function GetChild(AIndex: Integer): TcxAccessibilityHelper; override;
    function GetChildCount: Integer; override;
    function GetName(AChildID: TcxAccessibleSimpleChildElementID)
      : string; override;
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

    property ViewInfo: TdxNavBarOverflowPanelViewInfo read GetViewInfo;
  end;

  TdxNavBarNavigationPaneOverflowPanelItemAccessibilityHelper = class
    (TdxNavBarItemCollectionItemAccessibilityHelper) // for internal use
  private
    function GetGroup: TdxNavBarGroup;
    function GetViewInfo: TdxNavBarNavigationPaneViewInfo;
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

    procedure Click(AKey: Word); override;
    function GetBounds: TRect; override;
    function IsClickKey(AKey: Word): Boolean; override;
    function IsContainer: Boolean; override;

    property Group: TdxNavBarGroup read GetGroup;
    property ViewInfo: TdxNavBarNavigationPaneViewInfo read GetViewInfo;
  end;

  TdxNavBarNavigationPaneOverflowPanelSignAccessibilityHelper = class
    (TdxNavBarCustomAccessibilityHelper) // for internal use
  private
    function GetViewInfo: TdxNavBarNavigationPaneViewInfo;
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

    procedure Click(AKey: Word); override;
    function GetBounds: TRect; override;
    function IsClickKey(AKey: Word): Boolean; override;
    function IsContainer: Boolean; override;

    property ViewInfo: TdxNavBarNavigationPaneViewInfo read GetViewInfo;
  end;

  { TdxNavBarNavigationPaneGroupCaptionPanelAccessibilityHelper }

  TdxNavBarNavigationPaneGroupCaptionPanelAccessibilityHelper = class
    (TdxNavBarGroupCaptionPanelAccessibilityHelper) // for internal use
  protected
    function GetRole(AChildID: TcxAccessibleSimpleChildElementID)
      : Integer; override;
    function GetState(AChildID: TcxAccessibleSimpleChildElementID)
      : Integer; override;
  end;

  { TdxNavBarNavigationPaneActiveGroupCaptionPanelAccessibilityHelper }

  TdxNavBarNavigationPaneActiveGroupCaptionPanelAccessibilityHelper = class
    (TdxNavBarCustomAccessibilityHelper) // for internal use
  private
    function GetGroupViewInfo: TdxNavBarGroupViewInfo;
    function GetNavBar: TdxCustomNavBar;
    function GetNavBarViewInfo: TdxNavBarNavigationPaneViewInfo;
  protected
    function ChildIsSimpleElement(AIndex: Integer): Boolean; override;
    function GetName(AChildID: TcxAccessibleSimpleChildElementID)
      : string; override;
    function GetParent: TcxAccessibilityHelper; override;
    function GetRole(AChildID: TcxAccessibleSimpleChildElementID)
      : Integer; override;
    function GetState(AChildID: TcxAccessibleSimpleChildElementID)
      : Integer; override;
    function GetSupportedProperties(AChildID: TcxAccessibleSimpleChildElementID)
      : TcxAccessibleObjectProperties; override;

    function CanBeFocusedByDefault: Boolean; override;
    function GetAssociatedObject: TdxNavBarCustomAccessibilityHelper; override;
    function GetBounds: TRect; override;
    function IsContainer: Boolean; override;

    property NavBar: TdxCustomNavBar read GetNavBar;
    property NavBarViewInfo: TdxNavBarNavigationPaneViewInfo
      read GetNavBarViewInfo;
    property GroupViewInfo: TdxNavBarGroupViewInfo read GetGroupViewInfo;
  end;

  { TdxNavBarNavigationPaneHeaderPanelViewInfo }

  TdxNavBarNavigationPaneHeaderPanelViewInfo = class
    (TdxNavBarHeaderPanelViewInfo)
  protected
    function GetSignHintText: string; override;
    function GetText: string; override;
  end;

  { TdxNavBarNavigationPaneHeaderSignAccessibilityHelper }

  TdxNavBarNavigationPaneHeaderSignAccessibilityHelper = class
    (TdxNavBarHeaderPanelSignAccessibilityHelper); // for internal use

  { TdxNavBarItemPanelAccessibilityHelper }

  TdxNavBarItemPanelAccessibilityHelper = class
    (TdxNavBarItemCollectionAccessibilityHelper) // for internal use
  private
    function GetViewInfo: TdxNavBarItemPanelViewInfo;
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

    property ViewInfo: TdxNavBarItemPanelViewInfo read GetViewInfo;
  end;

  { TdxNavBarItemPanelCollapseBarAccessibilityHelper }

  TdxNavBarItemPanelCollapseBarAccessibilityHelper = class
    (TdxNavBarCustomAccessibilityHelper) // for internal use
  private
    function GetViewInfo: TdxNavBarItemPanelViewInfo;
  protected
    function ChildIsSimpleElement(AIndex: Integer): Boolean; override;
    procedure DoDefaultAction(AChildID
      : TcxAccessibleSimpleChildElementID); override;
    function GetDefaultActionDescription
      (AChildID: TcxAccessibleSimpleChildElementID): string; override;
    function GetName(AChildID: TcxAccessibleSimpleChildElementID)
      : string; override;
    function GetParent: TcxAccessibilityHelper; override;
    function GetRole(AChildID: TcxAccessibleSimpleChildElementID)
      : Integer; override;
    function GetState(AChildID: TcxAccessibleSimpleChildElementID)
      : Integer; override;
    function GetSupportedProperties(AChildID: TcxAccessibleSimpleChildElementID)
      : TcxAccessibleObjectProperties; override;

    function CanBeFocusedByDefault: Boolean; override;
    procedure Click(AKey: Word); override;
    function GetBounds: TRect; override;
    function IsClickKey(AKey: Word): Boolean; override;
    function IsContainer: Boolean; override;

    property ViewInfo: TdxNavBarItemPanelViewInfo read GetViewInfo;
  end;

  { TdxNavBarItemPanelItemAccessibilityHelper }

  TdxNavBarItemPanelItemAccessibilityHelper = class
    (TdxNavBarItemCollectionItemAccessibilityHelper) // for internal use
  private
    function GetItemPanelViewInfo: TdxNavBarItemPanelViewInfo;
  protected
    function ChildIsSimpleElement(AIndex: Integer): Boolean; override;
    procedure DoDefaultAction(AChildID
      : TcxAccessibleSimpleChildElementID); override;
    function GetDefaultActionDescription
      (AChildID: TcxAccessibleSimpleChildElementID): string; override;
    function GetName(AChildID: TcxAccessibleSimpleChildElementID)
      : string; override;
    function GetParent: TcxAccessibilityHelper; override;
    function GetRole(AChildID: TcxAccessibleSimpleChildElementID)
      : Integer; override;
    function GetState(AChildID: TcxAccessibleSimpleChildElementID)
      : Integer; override;
    function GetSupportedProperties(AChildID: TcxAccessibleSimpleChildElementID)
      : TcxAccessibleObjectProperties; override;

    function CanBeFocusedByDefault: Boolean; override;
    procedure Click(AKey: Word); override;
    function GetBounds: TRect; override;
    function IsClickKey(AKey: Word): Boolean; override;
    function IsContainer: Boolean; override;

    property ItemPanelViewInfo: TdxNavBarItemPanelViewInfo
      read GetItemPanelViewInfo;
  end;

  { TdxNavBarPopupControlViewInfo }

  TdxNavBarPopupControlViewInfo = class(TdxNavBarCustomPopupControlViewInfo)
  protected
    function CalculatePosition: TPoint; override;
    function GetBorderOffsets: TRect; override;
    function GetClientRect: TRect; override;
    function GetMaxHeight: Integer; override;
    procedure CalculateBounds(AClientWidth: Integer); override;

    function GetMinWidth: Integer; virtual;
    function IsPtSizeGrip(const pt: TPoint): Boolean; virtual;
  end;

  { TdxNavBarPopupControl }

  TdxNavBarPopupControlState = (pcsSizing, pcsOverSizeGrip);
  TdxNavBarPopupControlStates = set of TdxNavBarPopupControlState;

  TdxNavBarPopupControl = class(TdxNavBarCustomPopupControl)
  strict private
    FCapturePointOffset: Integer;
    FInternalState: TdxNavBarPopupControlStates;
    FOriginalWidth: Integer;
    FSizeFrame: TcxSizeFrame;
    function GetNavBarOriginalWidth: Integer;
    function GetMouseOverSizeGrip: Boolean;
    function GetPainter: TdxNavBarNavigationPanePainter;
    function GetViewInfo: TdxNavBarPopupControlViewInfo;
    procedure SetMouseOverSizeGrip(const AValue: Boolean);
  protected
    function CreatePopupViewInfo: TdxNavBarCustomPopupControlViewInfo; override;
    function GetOriginalWidth: Integer; override;
    procedure BeginResize(AControl: TControl; AButton: TMouseButton;
      AShift: TShiftState; const APoint: TPoint); override;
    procedure DoCanceled; override;
    procedure DoCloseUp; override;
    procedure DoShowed; override;
    procedure InitPopup; override;
    procedure Paint; override;
    procedure ScaleFactorChanged(M, D: Integer); override;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;

    function NeedAdjustWidth: Boolean;
    procedure DrawSizeFrame(const R: TRect);
    procedure EndResize(ACancel: Boolean = False);
    procedure RefreshPopupWindow;

    property MouseOverSizeGrip: Boolean read GetMouseOverSizeGrip
      write SetMouseOverSizeGrip;
  public
    constructor Create(ANavBar: TdxCustomNavBar); override;
    property ViewInfo: TdxNavBarPopupControlViewInfo read GetViewInfo;
    property Painter: TdxNavBarNavigationPanePainter read GetPainter;
  end;

implementation

uses
  Types, SysUtils, CommCtrl, Math, Generics.Defaults, Generics.Collections,
  cxLookAndFeels, cxLookAndFeelPainters, dxOffice11, dxCoreGraphics,
  dxTypeHelpers, dxThemeManager, dxUxTheme,
  dxNavBarConsts, dxNavBarGraphics, dxNavBarViewsFact;

type
  TdxCustomNavBarAccess = class(TdxCustomNavBar);
  TdxNavBarControllerAccess = class(TdxNavBarController);
  TdxNavBarCustomOverflowPanelPopupMenuAccess = class
    (TdxNavBarCustomOverflowPanelPopupMenu);
  TdxNavBarCustomOverflowPanelViewInfoAccess = class
    (TdxNavBarCustomOverflowPanelViewInfo);
  TdxNavBarHeaderPanelViewInfoAccess = class(TdxNavBarHeaderPanelViewInfo);
  TdxNavBarItemAccess = class(TdxNavBarItem);
  TdxNavBarNavigationPaneBehaviorOptionsAccess = class
    (TdxNavBarNavigationPaneBehaviorOptions);
  TdxNavBarPainterAccess = class(TdxNavBarPainter);
  TdxNavBarPopupControlAccess = class(TdxNavBarPopupControl);
  TdxNavBarViewInfoAccess = class(TdxNavBarViewInfo);

  { TdxNavBarNavigationPaneCalculator }

  TdxNavBarNavigationPaneCalculator = class(TdxNavBarCollapsedViewCalculator)
  strict private
    function GetViewInfo: TdxNavBarNavigationPaneViewInfo;
  strict protected
    function GetMaxElementSize: Integer; override;
    procedure CalculatePanelsMaxImageSize; override;
  end;

  { TdxNavBarNavigationPaneCalculator }

function TdxNavBarNavigationPaneCalculator.GetMaxElementSize: Integer;
var
  AGroupCaptionImageOffsets: TRect;
  AViewInfo: TdxNavBarNavigationPaneViewInfo;
begin
  AViewInfo := GetViewInfo;
  AGroupCaptionImageOffsets := AViewInfo.GetGroupCaptionImageOffsets;
  Result := Max(AGroupCaptionImageOffsets.Left + AGroupCaptionImageOffsets.Right
    + FMaxImageSize.cx, AViewInfo.OverflowPanelViewInfo.GetSignWidth +
    AViewInfo.OverflowPanelViewInfo.GetClientOffset.Left +
    AViewInfo.OverflowPanelViewInfo.GetClientOffset.Right);
  Result := Max(Result, AViewInfo.GetGroupCaptionSignSize.cx +
    AViewInfo.GetHeaderClientOffset.Left +
    AViewInfo.GetHeaderClientOffset.Right);
end;

procedure TdxNavBarNavigationPaneCalculator.CalculatePanelsMaxImageSize;
var
  AViewInfo: TdxNavBarNavigationPaneViewInfo;
begin
  inherited CalculatePanelsMaxImageSize;
  AViewInfo := GetViewInfo;
  FMaxImageSize := cxSize(AViewInfo.OverflowPanelViewInfo.GetImageWidth,
    AViewInfo.OverflowPanelViewInfo.GetImageHeight);
end;

function TdxNavBarNavigationPaneCalculator.GetViewInfo
  : TdxNavBarNavigationPaneViewInfo;
begin
  Result := FViewInfo as TdxNavBarNavigationPaneViewInfo;
end;

{ TdxNavBarOffice11GroupViewInfo }

function TdxNavBarOffice11GroupViewInfo.BorderColor: TColor;
begin
  if IsDefaultCaptionColor then
    Result := dxOffice11GroupBorderColor
  else
    Result := inherited BorderColor;
end;

function TdxNavBarOffice11GroupViewInfo.BgAlphaBlend: Byte;
begin
  if IsDefaultBgColor then
    Result := 255
  else
    Result := inherited BgAlphaBlend;
end;

function TdxNavBarOffice11GroupViewInfo.BgAlphaBlend2: Byte;
begin
  if IsDefaultBgColor then
    Result := 255
  else
    Result := inherited BgAlphaBlend2;
end;

function TdxNavBarOffice11GroupViewInfo.BgBackColor: TColor;
begin
  if IsDefaultBgColor then
    Result := dxOffice11GroupBackgroundColor1
  else
    Result := inherited BgBackColor;
end;

function TdxNavBarOffice11GroupViewInfo.BgBackColor2: TColor;
begin
  if IsDefaultBgColor then
    Result := dxOffice11GroupBackgroundColor2
  else
    Result := inherited BgBackColor2;
end;

function TdxNavBarOffice11GroupViewInfo.BgGradientMode: TdxBarStyleGradientMode;
begin
  if IsDefaultBgColor then
    Result := gmVertical
  else
    Result := inherited BgGradientMode;
end;

function TdxNavBarOffice11GroupViewInfo.CaptionAlphaBlend: Byte;
begin
  if IsDefaultCaptionColor then
    Result := 255
  else
    Result := inherited CaptionAlphaBlend;
end;

function TdxNavBarOffice11GroupViewInfo.CaptionAlphaBlend2: Byte;
begin
  if IsDefaultCaptionColor then
    Result := 255
  else
    Result := inherited CaptionAlphaBlend2;
end;

function TdxNavBarOffice11GroupViewInfo.CaptionBackColor: TColor;
begin
  if IsDefaultCaptionColor then
    Result := dxOffice11GroupCaptionColor1
  else
    Result := inherited CaptionBackColor;
end;

function TdxNavBarOffice11GroupViewInfo.CaptionBackColor2: TColor;
begin
  if IsDefaultCaptionColor then
    Result := dxOffice11GroupCaptionColor2
  else
    Result := inherited CaptionBackColor2;
end;

function TdxNavBarOffice11GroupViewInfo.CaptionGradientMode
  : TdxBarStyleGradientMode;
begin
  if IsDefaultCaptionColor then
    Result := gmHorizontal
  else
    Result := inherited CaptionGradientMode;
end;

function TdxNavBarOffice11GroupViewInfo.CaptionFontColor: TColor;
begin
  Result := inherited CaptionFontColor;
  if Result = clNone then
    Result := dxOffice11GroupFontColor;
end;

function TdxNavBarOffice11GroupViewInfo.IsDefaultBgColor: Boolean;
begin
  Result := (inherited BgBackColor = clNone) or
    (inherited BgBackColor2 = clNone);
end;

function TdxNavBarOffice11GroupViewInfo.IsDefaultCaptionColor: Boolean;
begin
  Result := (inherited CaptionBackColor = clNone) or
    (inherited CaptionBackColor2 = clNone);
end;

{ TdxNavBarOffice11ViewInfo }

function TdxNavBarOffice11ViewInfo.BgAlphaBlend: Byte;
begin
  if IsDefaultBgColor then
    Result := 255
  else
    Result := inherited BgAlphaBlend;
end;

function TdxNavBarOffice11ViewInfo.BgAlphaBlend2: Byte;
begin
  if IsDefaultBgColor then
    Result := 255
  else
    Result := inherited BgAlphaBlend2;
end;

function TdxNavBarOffice11ViewInfo.BgBackColor: TColor;
begin
  if IsDefaultBgColor then
    Result := dxOffice11BackgroundColor1
  else
    Result := inherited BgBackColor;
end;

function TdxNavBarOffice11ViewInfo.BgBackColor2: TColor;
begin
  if IsDefaultBgColor then
    Result := dxOffice11BackgroundColor2
  else
    Result := inherited BgBackColor2;
end;

function TdxNavBarOffice11ViewInfo.BgGradientMode: TdxBarStyleGradientMode;
begin
  if IsDefaultBgColor then
    Result := gmVertical
  else
    Result := inherited BgGradientMode;
end;

procedure TdxNavBarOffice11ViewInfo.AssignDefaultBackgroundStyle;
begin
  NavBar.DefaultStyles.Background.ResetValues;
  NavBar.DefaultStyles.Background.BackColor := clNone;
  NavBar.DefaultStyles.Background.BackColor2 := clNone;
end;

procedure TdxNavBarOffice11ViewInfo.AssignDefaultGroupBackgroundStyle;
begin
  NavBar.DefaultStyles.GroupBackground.ResetValues;
  NavBar.DefaultStyles.GroupBackground.BackColor := clNone;
  NavBar.DefaultStyles.GroupBackground.BackColor2 := clNone;
end;

procedure TdxNavBarOffice11ViewInfo.AssignDefaultGroupHeaderStyle;
begin
  NavBar.DefaultStyles.GroupHeader.ResetValues;
  NavBar.DefaultStyles.GroupHeader.BackColor := clNone;
  NavBar.DefaultStyles.GroupHeader.BackColor2 := clNone;
  NavBar.DefaultStyles.GroupHeader.Font.Color := clNone;
  NavBar.DefaultStyles.GroupHeader.Font.Height := ScaleFactor.Apply(-13);
  NavBar.DefaultStyles.GroupHeader.Font.Style := [fsBold];
end;

procedure TdxNavBarOffice11ViewInfo.AssignDefaultGroupHeaderActiveStyle;
begin
  NavBar.DefaultStyles.GroupHeaderActive.Assign
    (NavBar.DefaultStyles.GroupHeader);
end;

procedure TdxNavBarOffice11ViewInfo.AssignDefaultItemStyle;
begin
  NavBar.DefaultStyles.Item.ResetValues;
  NavBar.DefaultStyles.Item.Font.Color := dxOffice11LinkFontColor;
  NavBar.DefaultStyles.Item.HAlignment := haCenter;
end;

procedure TdxNavBarOffice11ViewInfo.CreateColors;
begin
  dxNavBarGraphics.CreateOffice11Colors;
end;

procedure TdxNavBarOffice11ViewInfo.RefreshColors;
begin
  dxNavBarGraphics.RefreshOffice11Colors;
end;

procedure TdxNavBarOffice11ViewInfo.ReleaseColors;
begin
  dxNavBarGraphics.ReleaseOffice11Colors;
end;

function TdxNavBarOffice11ViewInfo.GetGroupEdges: TPoint;
begin
  Result := ScaleFactor.Apply(cxPoint(10, 4));
end;

function TdxNavBarOffice11ViewInfo.GetGroupSeparatorWidth: Integer;
begin
  Result := ScaleFactor.Apply(4);
end;

function TdxNavBarOffice11ViewInfo.GetLinksImageEdges: TRect;
begin
  Result := cxRect(4, 4, 4, 4);
end;

function TdxNavBarOffice11ViewInfo.IsDefaultBgColor: Boolean;
begin
  Result := (inherited BgBackColor = clNone) or
    (inherited BgBackColor2 = clNone)
end;

{ TdxNavBarOffice11Painter }

class function TdxNavBarOffice11Painter.SignPainterClass
  : TdxNavBarCustomSignPainterClass;
begin
  Result := TdxNavBarOffice11SignPainter;
end;

class function TdxNavBarOffice11Painter.GetViewInfoClass
  : TdxNavBarViewInfoClass;
begin
  Result := TdxNavBarOffice11ViewInfo;
end;

class function TdxNavBarOffice11Painter.GetGroupViewInfoClass
  : TdxNavBarGroupViewInfoClass;
begin
  Result := TdxNavBarOffice11GroupViewInfo;
end;

class function TdxNavBarOffice11Painter.GetLinkViewInfoClass
  : TdxNavBarLinkViewInfoClass;
begin
  Result := TdxNavBarOffice11LinkViewInfo;
end;

class function TdxNavBarOffice11Painter.ButtonPainterClass
  : TdxNavBarCustomButtonPainterClass;
begin
  Result := TdxNavBarAdvExplorerButtonPainter;
end;

{ TdxNavBarOffice11ExplorerBarGroupViewInfo }

function TdxNavBarOffice11ExplorerBarGroupViewInfo.CaptionBorderColor: TColor;
begin
  Result := dxOffice11NavPaneBorder;
end;

function TdxNavBarOffice11ExplorerBarGroupViewInfo.CaptionBackColor: TColor;
begin
  if not IsDefaultCaptionColor then
    Result := inherited CaptionBackColor
  else
    Result := dxOffice11NavPaneGroupCaptionColor1;
end;

function TdxNavBarOffice11ExplorerBarGroupViewInfo.CaptionBackColor2: TColor;
begin
  if not IsDefaultCaptionColor then
    Result := inherited CaptionBackColor2
  else
    Result := dxOffice11NavPaneGroupCaptionColor2;
end;

function TdxNavBarOffice11ExplorerBarGroupViewInfo.CaptionAlphaBlend: Byte;
begin
  if not IsDefaultCaptionColor then
    Result := inherited CaptionAlphaBlend
  else
    Result := 255;
end;

function TdxNavBarOffice11ExplorerBarGroupViewInfo.CaptionAlphaBlend2: Byte;
begin
  if not IsDefaultCaptionColor then
    Result := inherited CaptionAlphaBlend2
  else
    Result := 255;
end;

function TdxNavBarOffice11ExplorerBarGroupViewInfo.CaptionGradientMode
  : TdxBarStyleGradientMode;
begin
  if not IsDefaultCaptionColor then
    Result := inherited CaptionGradientMode
  else
    Result := gmVertical;
end;

function TdxNavBarOffice11ExplorerBarGroupViewInfo.
  IsDefaultCaptionColor: Boolean;
begin
  Result := (inherited CaptionBackColor = clNone) or
    (inherited CaptionBackColor2 = clNone);
end;

{ TdxNavBarOffice11ExplorerBarViewInfo }

procedure TdxNavBarOffice11ExplorerBarViewInfo.AssignDefaultBackgroundStyle;
begin
  NavBar.DefaultStyles.Background.ResetValues;
  NavBar.DefaultStyles.Background.BackColor := clWindow;
  NavBar.DefaultStyles.Background.BackColor2 := clWindow;
end;

procedure TdxNavBarOffice11ExplorerBarViewInfo.AssignDefaultButtonStyle;
begin
  NavBar.DefaultStyles.Button.ResetValues;
  NavBar.DefaultStyles.Button.BackColor := clNone;
  NavBar.DefaultStyles.Button.BackColor2 := clNone;
end;

procedure TdxNavBarOffice11ExplorerBarViewInfo.
  AssignDefaultGroupBackgroundStyle;
begin
  NavBar.DefaultStyles.GroupBackground.ResetValues;
  NavBar.DefaultStyles.GroupBackground.BackColor := clWindow;
  NavBar.DefaultStyles.GroupBackground.BackColor2 := clWindow;
end;

procedure TdxNavBarOffice11ExplorerBarViewInfo.AssignDefaultGroupHeaderStyle;
begin
  NavBar.DefaultStyles.GroupHeader.ResetValues;
  NavBar.DefaultStyles.GroupHeader.BackColor := clNone;
  NavBar.DefaultStyles.GroupHeader.BackColor2 := clNone;
  NavBar.DefaultStyles.GroupHeader.Font.Style := [fsBold];
end;

procedure TdxNavBarOffice11ExplorerBarViewInfo.AssignDefaultItemStyle;
begin
  NavBar.DefaultStyles.Item.ResetValues;
  NavBar.DefaultStyles.Item.Font.Color := clWindowText;
  NavBar.DefaultStyles.Item.HAlignment := haCenter;
end;

procedure TdxNavBarOffice11ExplorerBarViewInfo.AssignDefaultItemDisabledStyle;
begin
  NavBar.DefaultStyles.ItemDisabled.Assign(NavBar.DefaultStyles.Item);
  NavBar.DefaultStyles.ItemDisabled.Font.Color := clGrayText;
end;

procedure TdxNavBarOffice11ExplorerBarViewInfo.
  AssignDefaultNavigationPaneHeaderStyle;
begin
  NavBar.DefaultStyles.NavigationPaneHeader.ResetValues;
  NavBar.DefaultStyles.NavigationPaneHeader.BackColor := clNone;
  NavBar.DefaultStyles.NavigationPaneHeader.BackColor2 := clNone;
  NavBar.DefaultStyles.NavigationPaneHeader.Font.Color := clNone;
  NavBar.DefaultStyles.NavigationPaneHeader.Font.Name := 'Arial';
  NavBar.DefaultStyles.NavigationPaneHeader.Font.Height :=
    ScaleFactor.Apply(-15);
  NavBar.DefaultStyles.NavigationPaneHeader.Font.Style := [fsBold];
end;

procedure TdxNavBarOffice11ExplorerBarViewInfo.CreateColors;
begin
  CreateOffice11NavPaneColors;
end;

procedure TdxNavBarOffice11ExplorerBarViewInfo.RefreshColors;
begin
  RefreshOffice11NavPaneColors;
end;

procedure TdxNavBarOffice11ExplorerBarViewInfo.ReleaseColors;
begin
  ReleaseOffice11NavPaneColors;
end;

function TdxNavBarOffice11ExplorerBarViewInfo.CanSelectLinkByRect: Boolean;
begin
  Result := True;
end;

function TdxNavBarOffice11ExplorerBarViewInfo.GetGroupBorderOffsets: TRect;
begin
  Result := cxNullRect;
end;

function TdxNavBarOffice11ExplorerBarViewInfo.
  GetGroupCaptionHeightAddon: Integer;
begin
  Result := ScaleFactor.Apply(6);
end;

function TdxNavBarOffice11ExplorerBarViewInfo.
  GetGroupCaptionImageIndent: Integer;
begin
  Result := ScaleFactor.Apply(2);
end;

function TdxNavBarOffice11ExplorerBarViewInfo.GetGroupEdges: TPoint;
begin
  Result := cxNullPoint;
end;

function TdxNavBarOffice11ExplorerBarViewInfo.GetGroupSeparatorWidth: Integer;
begin
  Result := 0;
end;

{ TdxNavBarOffice11ExplorerBarController }

procedure TdxNavBarOffice11ExplorerBarController.DoMouseDown
  (AButton: TMouseButton; AShift: TShiftState; const APoint: TPoint);
begin
  if MouseOverSizeGrip then
    TdxNavBarGroupControl(FNavBar.Parent).BeginResize(FNavBar, AButton,
      AShift, APoint)
  else
    inherited;
end;

procedure TdxNavBarOffice11ExplorerBarController.DoMouseMove
  (AShift: TShiftState; const APoint: TPoint);
begin
  MouseOverSizeGrip := (FNavBar.Parent is TdxNavBarGroupControl) and
    TdxNavBarGroupControl(FNavBar.Parent).IsOnPopupControl and
    not(FNavBar.IsPtBottomScrollButton(APoint) or
    FNavBar.IsPtTopScrollButton(APoint)) and
    cxRectPtIn(TdxNavBarGroupControl(FNavBar.Parent)
    .GetSizeGripRect(FNavBar), APoint);
  if not MouseOverSizeGrip then
    inherited;
end;

function TdxNavBarOffice11ExplorerBarController.GetCursor: HIcon;
begin
  if MouseOverSizeGrip then
    Result := Screen.Cursors[crSizeWE]
  else
    Result := inherited GetCursor;
end;

function TdxNavBarOffice11ExplorerBarController.GetMouseOverSizeGrip: Boolean;
begin
  Result := ecsOverSizeGrip in FInternalState;
end;

procedure TdxNavBarOffice11ExplorerBarController.SetMouseOverSizeGrip
  (AValue: Boolean);
begin
  if MouseOverSizeGrip <> AValue then
    if AValue then
      Include(FInternalState, ecsOverSizeGrip)
    else
      Exclude(FInternalState, ecsOverSizeGrip);
end;

{ TdxNavBarOffice11ExplorerBarPainter }

procedure TdxNavBarOffice11ExplorerBarPainter.DrawNavBarControl;
begin
  inherited DrawNavBarControl;
  if (NavBar.Parent is TdxNavBarGroupControl) and
    TdxNavBarGroupControl(NavBar.Parent).IsOnPopupControl then
    TdxNavBarGroupControl(NavBar.Parent).DrawSizeGrip(Canvas,
      TdxNavBarGroupControl(NavBar.Parent).GetSizeGripRect(NavBar));
end;

procedure TdxNavBarOffice11ExplorerBarPainter.DrawGroupControlSplitter
  (AGroupViewInfo: TdxNavBarExplorerBarGroupViewInfo);
var
  APoint: TPoint;
begin
  with AGroupViewInfo do
    ButtonPainterClass.DrawButton(Canvas, SplitterRect, CaptionImage,
      CaptionBackColor, CaptionBackColor2, CaptionAlphaBlend,
      CaptionAlphaBlend2, CaptionGradientMode, CaptionBorderColor, State,
      ScaleFactor);

  if dxOffice11NavPaneSplitterBitmap <> nil then
  begin
    APoint := cxRectCenter(AGroupViewInfo.SplitterRect,
      dxOffice11NavPaneSplitterBitmap.Width,
      dxOffice11NavPaneSplitterBitmap.Height).TopLeft;
    if APoint.X > AGroupViewInfo.SplitterRect.Left then
      Canvas.Draw(APoint.X, APoint.Y, dxOffice11NavPaneSplitterBitmap);
  end;
end;

class function TdxNavBarOffice11ExplorerBarPainter.GetViewInfoClass
  : TdxNavBarViewInfoClass;
begin
  Result := TdxNavBarOffice11ExplorerBarViewInfo;
end;

class function TdxNavBarOffice11ExplorerBarPainter.GetGroupViewInfoClass
  : TdxNavBarGroupViewInfoClass;
begin
  Result := TdxNavBarOffice11ExplorerBarGroupViewInfo;
end;

function TdxNavBarOffice11ExplorerBarPainter.GetControllerClass
  : TdxNavBarControllerClass;
begin
  Result := TdxNavBarOffice11ExplorerBarController;
end;

class function TdxNavBarOffice11ExplorerBarPainter.SignPainterClass
  : TdxNavBarCustomSignPainterClass;
begin
  Result := TdxNavBarOffice11ExplorerBarSignPainter;
end;

{ TdxNavBarOffice11ExplorerBarSignPainter }

class procedure TdxNavBarOffice11ExplorerBarSignPainter.DrawSignSelection
  (ACanvas: TCanvas; ARect: TRect; AForeColor, ABackColor1, ABackColor2: TColor;
  AState: TdxNavBarObjectStates);
begin
end;

{ TdxNavBarNavigationPane }

function TdxNavBarNavigationPaneGroupViewInfo.CaptionBorderColor: TColor;
begin
  Result := dxOffice11NavPaneBorder;
end;

function TdxNavBarNavigationPaneGroupViewInfo.CaptionBackColor: TColor;
begin
  if not IsDefaultCaptionColor then
    Result := inherited CaptionBackColor
  else if sActive in State then
  begin
    if sHotTracked in State then
      Result := dxOffice11NavPaneGroupCaptionPressedHotColor1
    else
      Result := dxOffice11NavPaneGroupCaptionPressedColor1;
  end
  else if sHotTracked in State then
    Result := dxOffice11NavPaneGroupCaptionHotColor1
  else
    Result := dxOffice11NavPaneGroupCaptionColor1;
end;

function TdxNavBarNavigationPaneGroupViewInfo.CaptionBackColor2: TColor;
begin
  if not IsDefaultCaptionColor then
    Result := inherited CaptionBackColor2
  else if sActive in State then
  begin
    if sHotTracked in State then
      Result := dxOffice11NavPaneGroupCaptionPressedHotColor2
    else
      Result := dxOffice11NavPaneGroupCaptionPressedColor2;
  end
  else if sHotTracked in State then
    Result := dxOffice11NavPaneGroupCaptionHotColor2
  else
    Result := dxOffice11NavPaneGroupCaptionColor2;
end;

function TdxNavBarNavigationPaneGroupViewInfo.CaptionAlphaBlend: Byte;
begin
  if not IsDefaultCaptionColor then
    Result := inherited CaptionAlphaBlend
  else
    Result := 255;
end;

function TdxNavBarNavigationPaneGroupViewInfo.CaptionAlphaBlend2: Byte;
begin
  if not IsDefaultCaptionColor then
    Result := inherited CaptionAlphaBlend2
  else
    Result := 255;
end;

function TdxNavBarNavigationPaneGroupViewInfo.CaptionGradientMode
  : TdxBarStyleGradientMode;
begin
  if not IsDefaultCaptionColor then
    Result := inherited CaptionGradientMode
  else
    Result := gmVertical;
end;

function TdxNavBarNavigationPaneGroupViewInfo.GetActiveScrollBarBounds: TRect;
var
  ANavPaneViewInfo: TdxNavBarNavigationPaneViewInfo;
begin
  ANavPaneViewInfo := ViewInfo as TdxNavBarNavigationPaneViewInfo;
  Result := cxRectContent(FRect, GetBorderOffsets);
  if TdxNavBarViewInfoAccess(ANavPaneViewInfo).IsHeaderVisible then
    Result.Top := FItemsRect.Top;
  if NavBar.UseRightToLeftScrollBar then
    Result.Right := Result.Left + ViewInfo.GetActiveGroupScrollBarWidth
  else
    Result.Left := Result.Right - ViewInfo.GetActiveGroupScrollBarWidth;
  TdxNavBarViewInfoAccess(ViewInfo).CalculateScrollBarBoundsBySizeGrip(Result);
end;

function TdxNavBarNavigationPaneGroupViewInfo.GetImageIndent: Integer;
var
  AWidth: Integer;
  ANavPaneViewInfo: TdxNavBarNavigationPaneViewInfo;
begin
  ANavPaneViewInfo := ViewInfo as TdxNavBarNavigationPaneViewInfo;
  AWidth := ANavPaneViewInfo.GetNavBarCollapsedWidth -
    ANavPaneViewInfo.GetGroupEdges.X;
  Result := (AWidth - GetImageWidth) div 2;
end;

function TdxNavBarNavigationPaneGroupViewInfo.IsTopLevel: Boolean;
begin
  Result := inherited IsTopLevel or (Level <> 0) and
    (Group = (ViewInfo as TdxNavBarNavigationPaneViewInfo)
    .Painter.Controller.SelectedPopupGroup);
end;

function TdxNavBarNavigationPaneGroupViewInfo.IsDefaultCaptionColor: Boolean;
begin
  Result := (inherited CaptionBackColor = clNone) or
    (inherited CaptionBackColor2 = clNone);
end;

{ TdxNavBarOffice11NavPaneLinkViewInfo }

function TdxNavBarOffice11NavPaneLinkViewInfo.SeparatorColor: TColor;
begin
  Result := ViewInfo.BorderColor;
end;

{ TdxNavBarNavigationPaneCustomViewInfo }

function TdxNavBarNavigationPaneCustomViewInfo.GetViewInfo
  : TdxNavBarNavigationPaneViewInfo;
begin
  Result := inherited ViewInfo as TdxNavBarNavigationPaneViewInfo;
end;

function TdxNavBarNavigationPaneCustomViewInfo.GetPainter
  : TdxNavBarNavigationPanePainter;
begin
  Result := inherited Painter as TdxNavBarNavigationPanePainter;
end;

{ TdxNavBarOverflowPanelViewInfo }

constructor TdxNavBarOverflowPanelViewInfo.Create(AViewInfo: TdxNavBarViewInfo);
begin
  inherited;
  FItemIAccessibilityHelpers := TInterfaceList.Create;
end;

destructor TdxNavBarOverflowPanelViewInfo.Destroy;
begin
  SetItemIAccessibilityHelperCount(0);
  FreeAndNil(FItemIAccessibilityHelpers);
  NavBarAccessibleObjectOwnerObjectDestroyed(FSignIAccessibilityHelper);
  NavBarAccessibleObjectOwnerObjectDestroyed(FIAccessibilityHelper);
  inherited;
end;

procedure TdxNavBarOverflowPanelViewInfo.CheckItemIAccessibilityHelperCount;
begin
  SetItemIAccessibilityHelperCount(ItemCount);
end;

function TdxNavBarOverflowPanelViewInfo.AllowCustomizing: Boolean;
begin
  Result := NavBar.OptionsBehavior.NavigationPane.AllowCustomizing;
end;

function TdxNavBarOverflowPanelViewInfo.GetAccessibilityHelperClass
  : TdxNavBarCustomAccessibilityHelperClass;
begin
  Result := TdxNavBarNavigationPaneOverflowPanelAccessibilityHelper;
end;

function TdxNavBarOverflowPanelViewInfo.GetItemAccessibilityHelperClass
  : TdxNavBarItemCollectionItemAccessibilityHelperClass;
begin
  Result := TdxNavBarNavigationPaneOverflowPanelItemAccessibilityHelper;
end;

function TdxNavBarOverflowPanelViewInfo.GetSignAccessibilityHelperClass
  : TdxNavBarCustomAccessibilityHelperClass;
begin
  Result := TdxNavBarNavigationPaneOverflowPanelSignAccessibilityHelper;
end;

function TdxNavBarOverflowPanelViewInfo.GetIAccessibilityHelper
  : IdxNavBarAccessibilityHelper;
begin
  if FIAccessibilityHelper = nil then
    FIAccessibilityHelper := NavBarGetAccessibilityHelper
      (GetAccessibilityHelperClass.Create(Self, NavBar));
  Result := FIAccessibilityHelper;
end;

function TdxNavBarOverflowPanelViewInfo.GetItemIAccessibilityHelper
  (AIndex: Integer): IdxNavBarAccessibilityHelper;
begin
  CheckItemIAccessibilityHelperCount;
  Result := FItemIAccessibilityHelpers[AIndex] as IdxNavBarAccessibilityHelper;
end;

function TdxNavBarOverflowPanelViewInfo.
  GetItemIAccessibilityHelperCount: Integer;
begin
  CheckItemIAccessibilityHelperCount;
  Result := FItemIAccessibilityHelpers.Count;
end;

procedure TdxNavBarOverflowPanelViewInfo.SetItemIAccessibilityHelperCount
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

function TdxNavBarOverflowPanelViewInfo.GetSignIAccessibilityHelper
  : IdxNavBarAccessibilityHelper;
begin
  if FSignIAccessibilityHelper = nil then
    FSignIAccessibilityHelper := NavBarGetAccessibilityHelper
      (GetSignAccessibilityHelperClass.Create(Self, NavBar));
  Result := FSignIAccessibilityHelper;
end;

{ TdxNavBarItemPanelViewInfoItem }

procedure TdxNavBarItemPanelViewInfoItem.OffsetRects(dX, dY: Integer);
begin
  Rect := cxRectOffset(Rect, dX, dY);
  TextRect := cxRectOffset(TextRect, dX, dY);
  ImageRect := cxRectOffset(ImageRect, dX, dY);
end;

function TdxNavBarItemPanelViewInfoItem.GetCaption: string;
begin
  Result := ItemLink.Item.Caption;
end;

{ TdxNavBarItemPanelViewInfo }

constructor TdxNavBarItemPanelViewInfo.Create(AViewInfo: TdxNavBarViewInfo);
begin
  inherited;
  FItems := TObjectList.Create;
  NavBar.GroupContainerIAccessibilityHelper.AttachChild(IAccessibilityHelper);
  FItemIAccessibilityHelpers := TInterfaceList.Create;
end;

destructor TdxNavBarItemPanelViewInfo.Destroy;
begin
  SetItemIAccessibilityHelperCount(0);
  FreeAndNil(FItemIAccessibilityHelpers);
  if not(csDestroying in NavBar.ComponentState) then
    NavBar.GroupContainerIAccessibilityHelper.DetachChild(IAccessibilityHelper);
  NavBarAccessibleObjectOwnerObjectDestroyed(FIAccessibilityHelper);
  NavBarAccessibleObjectOwnerObjectDestroyed(FCollapseBarIAccessibilityHelper);
  FreeAndNil(FItems);
  inherited;
end;

procedure TdxNavBarItemPanelViewInfo.CheckItemIAccessibilityHelperCount;
begin
  SetItemIAccessibilityHelperCount(ItemCount);
end;

procedure TdxNavBarItemPanelViewInfo.CalculateBounds(var X, Y: Integer);

  procedure CalculateItemRects(ARect: TRect);
  var
    I: Integer;
    AItem: TdxNavBarItemPanelViewInfoItem;
    ATextWidth, AIndent: Integer;
    AImageSize: TSize;
  begin
    for I := 0 to ItemCount - 1 do
    begin
      AItem := Items[I];
      AIndent := ViewInfo.GetGroupCaptionImageIndent;
      ATextWidth := cxTextWidth(AItem.Font, AItem.Caption);
      AItem.TextRect := cxRect(ARect.Left, ARect.Bottom, ARect.Right,
        ARect.Bottom + ATextWidth + AIndent * 2);

      if IsImageAssigned(AItem.ImageList, AItem.ImageIndex) then
        AImageSize := cxSize(ViewInfo.GetSmallImageWidth,
          ViewInfo.GetSmallImageHeight)
      else
        AImageSize := cxNullSize;
      AItem.ImageRect := cxRectCenter(cxRect(ARect.Left, AItem.TextRect.Bottom,
        ARect.Right, AItem.TextRect.Bottom + AImageSize.cy), AImageSize);

      AItem.Rect := cxRect(ARect.Left, AItem.TextRect.Top, ARect.Right,
        AItem.ImageRect.Bottom);
      if IsImageAssigned(AItem.ImageList, AItem.ImageIndex) then
        AItem.Rect.Bottom := AItem.Rect.Bottom + AIndent;

      ARect.Bottom := AItem.Rect.Bottom;
    end;
  end;

begin
  FRect := cxNullRect;
  FCollapseBarRect := cxNullRect;
  if ViewInfo.HasItemPanel then
  begin
    if (FActiveGroupViewInfo <> nil) and not FIsCollapseMode then
    begin
      FActiveGroupViewInfo.CalculateBounds(X, Y);
      FRect := FActiveGroupViewInfo.Rect;
    end
    else
    begin
      FRect := Bounds(X, Y, ViewInfo.ClientWidth - 2 * X, GetMinHeight);
      Y := FRect.Bottom;
    end;
    if FIsCollapseMode then
    begin
      FCollapseBarRect := FRect;
      CalculateItemRects(FCollapseBarRect);
    end
    else
      FCollapseBarRect := cxNullRect;
  end;
end;

function TdxNavBarItemPanelViewInfo.GetMinHeight: Integer;
begin
  if FIsCollapseMode then
    Result := cxTextWidth(CollapseBarFont, CollapseBarText) +
      GetCollapseBarCaptionIndent * 2
  else
    Result := ViewInfo.GetActiveGroupMinHeight;
end;

procedure TdxNavBarItemPanelViewInfo.CorrectBounds(AHeightDifference: Integer);
var
  I, ALastVisibleIndex: Integer;
begin
  FRect.Bottom := FRect.Bottom + AHeightDifference;
  if (FActiveGroupViewInfo <> nil) and not FIsCollapseMode then
    FActiveGroupViewInfo.CorrectActiveGroupBounds(0, AHeightDifference)
  else
    TdxCustomNavBarAccess(NavBar).ActiveGroupScrollBar.Visible := False;
  if FIsCollapseMode then
  begin
    ALastVisibleIndex := -1;
    for I := 0 to ItemCount - 1 do
    begin
      if cxRectHeight(Items[I].Rect) <= AHeightDifference then
      begin
        AHeightDifference := AHeightDifference - cxRectHeight(Items[I].Rect);
        ALastVisibleIndex := I;
      end
      else
        Break;
    end;
    for I := ItemCount - 1 downto ALastVisibleIndex + 1 do
      FItems.Delete(I);
    FCollapseBarRect.Bottom := FCollapseBarRect.Bottom + AHeightDifference;
    for I := 0 to ItemCount - 1 do
      Items[I].OffsetRects(0, AHeightDifference);
    CheckItemIAccessibilityHelperCount;
  end;
end;

function TdxNavBarItemPanelViewInfo.GetAccessibilityHelperClass
  : TdxNavBarCustomAccessibilityHelperClass;
begin
  Result := TdxNavBarItemPanelAccessibilityHelper;
end;

function TdxNavBarItemPanelViewInfo.GetCollapseBarAccessibilityHelperClass
  : TdxNavBarCustomAccessibilityHelperClass;
begin
  Result := TdxNavBarItemPanelCollapseBarAccessibilityHelper;
end;

function TdxNavBarItemPanelViewInfo.GetCollapseBarFont: TFont;
begin
  Result := ViewInfo.HeaderFont;
end;

function TdxNavBarItemPanelViewInfo.GetCollapseBarText: string;
begin
  if NavBar.OptionsView.NavigationPane.ShowActiveGroupCaptionWhenCollapsed and
    (FCollapseBarGroup <> nil) then
    Result := FCollapseBarGroup.Caption
  else
    Result := cxGetResourceString(@sdxNavigationPaneCollapseBar);
end;

function TdxNavBarItemPanelViewInfo.GetItemAccessibilityHelperClass
  : TdxNavBarItemCollectionItemAccessibilityHelperClass;
begin
  Result := TdxNavBarItemPanelItemAccessibilityHelper;
end;

function TdxNavBarItemPanelViewInfo.GetItemFont(AIndex: Integer): TFont;
begin
  Result := NavBar.DefaultStyles.Item.Font;
end;

function TdxNavBarItemPanelViewInfo.GetItemIndexAtPos(const pt: TPoint)
  : Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to ItemCount - 1 do
    if PtInRect(Items[I].Rect, pt) then
    begin
      Result := I;
      Break;
    end;
end;

function TdxNavBarItemPanelViewInfo.GetPartAtPos(const pt: TPoint)
  : TdxNavBarPart;
begin
  Result.MinorPartIndex := GetItemIndexAtPos(pt);
  if Result.MinorPartIndex <> nbNone then
    Result.MajorPartIndex := nbItemPanelCollapseItem
  else
    Result.MajorPartIndex := nbItemPanelCollapseBar;
end;

function TdxNavBarItemPanelViewInfo.GetCollapseBarCaptionIndent: Integer;
begin
  Result := ScaleFactor.Apply(4);
end;

function TdxNavBarItemPanelViewInfo.GetCollapseBarIAccessibilityHelper
  : IdxNavBarAccessibilityHelper;
begin
  if FCollapseBarIAccessibilityHelper = nil then
    FCollapseBarIAccessibilityHelper := NavBarGetAccessibilityHelper
      (GetCollapseBarAccessibilityHelperClass.Create(Self, NavBar));
  Result := FCollapseBarIAccessibilityHelper;
end;

function TdxNavBarItemPanelViewInfo.GetIAccessibilityHelper
  : IdxNavBarAccessibilityHelper;
begin
  if FIAccessibilityHelper = nil then
    FIAccessibilityHelper := NavBarGetAccessibilityHelper
      (GetAccessibilityHelperClass.Create(Self, NavBar));
  Result := FIAccessibilityHelper;
end;

function TdxNavBarItemPanelViewInfo.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxNavBarItemPanelViewInfo.GetItemIAccessibilityHelperCount: Integer;
begin
  CheckItemIAccessibilityHelperCount;
  Result := FItemIAccessibilityHelpers.Count;
end;

function TdxNavBarItemPanelViewInfo.GetItemIAccessibilityHelper(AIndex: Integer)
  : IdxNavBarAccessibilityHelper;
begin
  CheckItemIAccessibilityHelperCount;
  Result := FItemIAccessibilityHelpers[AIndex] as IdxNavBarAccessibilityHelper;
end;

function TdxNavBarItemPanelViewInfo.GetItems(AIndex: Integer)
  : TdxNavBarItemPanelViewInfoItem;
begin
  Result := TdxNavBarItemPanelViewInfoItem(FItems[AIndex]);
end;

procedure TdxNavBarItemPanelViewInfo.SetActiveGroupViewInfo
  (AValue: TdxNavBarGroupViewInfo);
begin
  FActiveGroupViewInfo := AValue;
end;

procedure TdxNavBarItemPanelViewInfo.SetItemIAccessibilityHelperCount
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
        NavBarAccessibleObjectOwnerObjectDestroyed(AIAccessibilityHelper);
        FItemIAccessibilityHelpers.Delete(I);
      end;
end;

function TdxNavBarItemPanelViewInfo.AddItem: TdxNavBarItemPanelViewInfoItem;
begin
  Result := TdxNavBarItemPanelViewInfoItem.Create;
  Result.Index := FItems.Add(Result);
  Result.Font := GetItemFont(-1);
  Result.ImageList := NavBar.SmallImages;
end;

procedure TdxNavBarItemPanelViewInfo.ClearItems;
begin
  FItems.Clear;
end;

procedure TdxNavBarItemPanelViewInfo.CreateItems;

  procedure AddLinkItem(ALink: TdxNavBarItemLink);
  var
    APanelItem: TdxNavBarItemPanelViewInfoItem;
  begin
    if ALink.CanSelect and ALink.Item.Visible then
    begin
      APanelItem := AddItem;
      APanelItem.ItemLink := ALink;
      APanelItem.ImageIndex := APanelItem.ItemLink.Item.SmallImageIndex;
    end;
  end;

var
  I: Integer;
  AChild: TObject;
begin
  ClearItems;
  if FIsCollapseMode and (FCollapseBarGroup <> nil) and ViewInfo.HasItemPanel
  then
    for I := 0 to FCollapseBarGroup.ChildCount - 1 do
    begin
      AChild := FCollapseBarGroup.Children[I];
      if AChild is TdxNavBarItemLink then
        AddLinkItem(TdxNavBarItemLink(AChild))
    end;
end;

procedure TdxNavBarItemPanelViewInfo.DoRightToLeftConversion;
var
  I: Integer;
begin
  RTLConvert(FCollapseBarRect);
  RTLConvert(FRect);
  for I := 0 to ItemCount - 1 do
  begin
    RTLConvert(Items[I].Rect);
    RTLConvert(Items[I].TextRect);
    RTLConvert(Items[I].ImageRect);
  end;
end;

{ TdxNavBarCollapsedViewCalculator }

constructor TdxNavBarCollapsedViewCalculator.Create
  (AViewInfo: TdxNavBarViewInfo);
begin
  inherited Create;
  FViewInfo := AViewInfo;
  FIsCalculationNeeded := True;
end;

function TdxNavBarCollapsedViewCalculator.GetCollapsedWidth: Integer;
var
  AViewInfoAccess: TdxNavBarViewInfoAccess;
begin
  AViewInfoAccess := TdxNavBarViewInfoAccess(FViewInfo);
  AViewInfoAccess.InternalCalculateMaxImageSize;
  Result := GetMaxElementSize;
  Result := Result + AViewInfoAccess.GetGroupEdges.X * 2;
end;

function TdxNavBarCollapsedViewCalculator.GetMinExpandedWidth: Integer;
begin
  Result := GetCollapsedWidth * 2;
end;

procedure TdxNavBarCollapsedViewCalculator.CalculateMaxImageSize;
begin
  if not FIsCalculationNeeded then
    Exit;
  CalculatePanelsMaxImageSize;
  CalculateGroupsMaxImageSize;
  FIsCalculationNeeded := False;
end;

procedure TdxNavBarCollapsedViewCalculator.Reset;
begin
  FIsCalculationNeeded := True;
end;

function TdxNavBarCollapsedViewCalculator.CreateGroupViewInfo
  (AGroup: TdxNavBarGroup): TdxNavBarGroupViewInfo;
begin
  Result := TdxNavBarPainterAccess(FViewInfo.Painter)
    .CreateGroupViewInfo(FViewInfo, AGroup, True, False)
end;

function TdxNavBarCollapsedViewCalculator.GetMaxElementSize: Integer;
begin
  Result := Max(FMaxImageSize.cx, FMaxImageSize.cy);
end;

procedure TdxNavBarCollapsedViewCalculator.CalculateGroupMaxImageSize
  (AGroupViewInfo: TdxNavBarGroupViewInfo);
begin
  CalculateItemMaxImageSize(AGroupViewInfo);
end;

procedure TdxNavBarCollapsedViewCalculator.CalculatePanelsMaxImageSize;
begin
  FMaxImageSize := cxNullSize;
end;

procedure TdxNavBarCollapsedViewCalculator.CalculateItemMaxImageSize
  (AItemViewInfo: TdxNavBarCustomItemViewInfo);
begin
  FMaxImageSize.cx := Max(FMaxImageSize.cx, AItemViewInfo.GetImageWidth);
  FMaxImageSize.cy := Max(FMaxImageSize.cy, AItemViewInfo.GetImageHeight);
end;

procedure TdxNavBarCollapsedViewCalculator.CalculateGroupsMaxImageSize;

  function GetGroupViewInfo(AList: TList; AIndex: Integer)
    : TdxNavBarGroupViewInfo;
  begin
    Result := TdxNavBarGroupViewInfo(AList[AIndex]);
  end;

  procedure CalculateGroupsMaxImageSize(AList: TObjectList);
  var
    I: Integer;
    AGroupViewInfo: TdxNavBarGroupViewInfo;
  begin
    FImageHeightPeer := AList.Count > 0;
    for I := 0 to AList.Count - 1 do
    begin
      AGroupViewInfo := GetGroupViewInfo(AList, I);
      FImageHeightPeer := FImageHeightPeer and
        (GetGroupViewInfo(AList, 0).GetImageHeight = AGroupViewInfo.
        GetImageHeight);
      CalculateGroupMaxImageSize(AGroupViewInfo);
    end;
  end;

var
  I: Integer;
  AList: TObjectList;
begin
  AList := TObjectList.Create;
  try
    for I := 0 to FViewInfo.NavBar.RootGroupCount - 1 do
      AList.Add(CreateGroupViewInfo(FViewInfo.NavBar.RootGroups[I]));
    CalculateGroupsMaxImageSize(AList);
  finally
    AList.Free;
  end;
end;

{ TdxNavBarNavigationPaneOverflowPanelPopupMenu }

procedure TdxNavBarNavigationPaneOverflowPanelPopupMenu.PopulateItems
  (ADefaultImageIndex: Integer);
begin
  AddItem(Items, cxGetResourceString(@sdxNavBarOffice11ShowMoreButtons),
    NavBar.CanDecNavigationPaneOverflowPanelItemCount, DoMoreButtonsClick,
    ADefaultImageIndex);
  AddItem(Items, cxGetResourceString(@sdxNavBarOffice11ShowFewerButtons),
    NavBar.CanIncNavigationPaneOverflowPanelItemCount, DoFewerButtonsClick,
    ADefaultImageIndex + 1);
  inherited PopulateItems(ADefaultImageIndex);
end;

procedure TdxNavBarNavigationPaneOverflowPanelPopupMenu.DoMoreButtonsClick
  (Sender: TObject);
begin
  NavBar.DoDecNavigationPaneOverflowPanelItemCount;
end;

procedure TdxNavBarNavigationPaneOverflowPanelPopupMenu.DoFewerButtonsClick
  (Sender: TObject);
begin
  NavBar.DoIncNavigationPaneOverflowPanelItemCount;
end;

{ TdxNavBarOffice11NavPanelViewInfo }

constructor TdxNavBarNavigationPaneViewInfo.Create(APainter: TdxNavBarPainter);
begin
  inherited Create(APainter);
  FOverflowPanelViewInfo := GetOverflowPanelViewInfoClass.Create(Self);
  TdxNavBarCustomOverflowPanelViewInfoAccess(FOverflowPanelViewInfo)
    .OnSignButtonClick := OnOverflowPanelSignButtonClickHandler;

  FPopupMenu := TdxNavBarNavigationPaneOverflowPanelPopupMenu.Create
    (OverflowPanelViewInfo);
  TdxNavBarCustomOverflowPanelPopupMenuAccess(FPopupMenu).OnDrawItem :=
    Painter.OnDrawOverflowPanelPopupMenuItemHandler;

  FItemPanelViewInfo := TdxNavBarItemPanelViewInfo.Create(Self);
  APainter.NavBar.GroupContainerIAccessibilityHelper.AttachChild
    (ActiveGroupCaptionPanelIAccessibilityHelper);
  APainter.NavBar.GroupContainerIAccessibilityHelper.AttachChild
    (OverflowPanelIAccessibilityHelper);
  FCollapsedViewCalculator := CreateCollapsedViewCalculator;
end;

destructor TdxNavBarNavigationPaneViewInfo.Destroy;
begin
  FreeAndNil(FPopupMenu);
  FreeAndNil(FCollapsedViewCalculator);
  if not(csDestroying in NavBar.ComponentState) then
  begin
    NavBar.GroupContainerIAccessibilityHelper.DetachChild
      (ActiveGroupCaptionPanelIAccessibilityHelper);
    NavBar.GroupContainerIAccessibilityHelper.DetachChild
      (OverflowPanelIAccessibilityHelper);
  end;
  NavBarAccessibleObjectOwnerObjectDestroyed
    (FActiveGroupCaptionPanelIAccessibilityHelper);
  FreeAndNil(FItemPanelViewInfo);
  FreeAndNil(FOverflowPanelViewInfo);
  inherited;
end;

function TdxNavBarNavigationPaneViewInfo.FindGroupWithAccel(AKey: Word)
  : TdxNavBarGroup;
var
  AIndex: Integer;
  AGroup: TdxNavBarGroup;
begin
  Result := inherited FindGroupWithAccel(AKey);
  if (Result = nil) and OverflowPanelViewInfo.IsVisible and not IsCollapsed then
    for AIndex := 0 to OverflowPanelViewInfo.ItemCount - 1 do
    begin
      AGroup := OverflowPanelViewInfo.Items[AIndex].Group;
      if IsAccel(AKey, AGroup.Caption) then
      begin
        Result := AGroup;
        Break;
      end
    end;
end;

function TdxNavBarNavigationPaneViewInfo.FindLinkWithAccel(AKey: Word)
  : TdxNavBarItemLink;
var
  AIndex: Integer;
  AItemPanelViewInfoItem: TdxNavBarItemPanelViewInfoItem;
begin
  Result := inherited FindLinkWithAccel(AKey);
  if Result = nil then
    for AIndex := 0 to FItemPanelViewInfo.ItemCount - 1 do
    begin
      AItemPanelViewInfoItem := FItemPanelViewInfo.Items[AIndex];
      if IsAccel(AKey, AItemPanelViewInfoItem.Caption) then
      begin
        Result := AItemPanelViewInfoItem.ItemLink;
        Break;
      end
    end;
end;

function TdxNavBarNavigationPaneViewInfo.GetLinkAtPos(const pt: TPoint)
  : TdxNavBarItemLink;
var
  APart: TdxNavBarPart;
begin
  if IsCollapsed then
  begin
    APart := Painter.Controller.GetPartAtPos(pt);
    if APart.MajorPartIndex = nbItemPanelCollapseItem then
      Result := ItemPanelViewInfo.Items[APart.MinorPartIndex].ItemLink
    else if Painter.Controller.DroppedDownPart.MajorPartIndex = nbItemPanelCollapseBar
    then
      Result := TdxNavBarPopupControlAccess(Painter.Controller.PopupControl)
        .GetLinkAtPos(pt)
    else
      Result := nil;
  end
  else
    Result := inherited GetLinkAtPos(pt);
end;

function TdxNavBarNavigationPaneViewInfo.BorderColor: TColor;
begin
  Result := dxOffice11NavPaneBorder;
end;

function TdxNavBarNavigationPaneViewInfo.CollapseBarFontColor: TColor;
begin
  Result := BorderColor;
end;

function TdxNavBarNavigationPaneViewInfo.BottomScrollButtonBackColor: TColor;
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

function TdxNavBarNavigationPaneViewInfo.BottomScrollButtonBackColor2: TColor;
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

function TdxNavBarNavigationPaneViewInfo.BottomScrollButtonAlphaBlend: Byte;
begin
  if not IsDefaultBottomScrollButtonColor then
    Result := inherited BottomScrollButtonAlphaBlend
  else
    Result := 255;
end;

function TdxNavBarNavigationPaneViewInfo.BottomScrollButtonAlphaBlend2: Byte;
begin
  if not IsDefaultBottomScrollButtonColor then
    Result := inherited BottomScrollButtonAlphaBlend2
  else
    Result := 255;
end;

function TdxNavBarNavigationPaneViewInfo.BottomScrollButtonGradientMode
  : TdxBarStyleGradientMode;
begin
  if not IsDefaultBottomScrollButtonColor then
    Result := inherited BottomScrollButtonGradientMode
  else
    Result := gmVertical;
end;

function TdxNavBarNavigationPaneViewInfo.TopScrollButtonBackColor: TColor;
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

function TdxNavBarNavigationPaneViewInfo.TopScrollButtonBackColor2: TColor;
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

function TdxNavBarNavigationPaneViewInfo.TopScrollButtonAlphaBlend: Byte;
begin
  if not IsDefaultTopScrollButtonColor then
    Result := inherited TopScrollButtonAlphaBlend
  else
    Result := 255;
end;

function TdxNavBarNavigationPaneViewInfo.TopScrollButtonAlphaBlend2: Byte;
begin
  if not IsDefaultTopScrollButtonColor then
    Result := inherited TopScrollButtonAlphaBlend2
  else
    Result := 255;
end;

function TdxNavBarNavigationPaneViewInfo.TopScrollButtonGradientMode
  : TdxBarStyleGradientMode;
begin
  if not IsDefaultTopScrollButtonColor then
    Result := inherited TopScrollButtonGradientMode
  else
    Result := gmVertical;
end;

function TdxNavBarNavigationPaneViewInfo.HeaderBackColor: TColor;
begin
  if not IsDefaultHeaderColor then
    Result := inherited HeaderBackColor
  else
    Result := dxOffice11NavPaneHeaderColor1;
end;

function TdxNavBarNavigationPaneViewInfo.HeaderBackColor2: TColor;
begin
  if not IsDefaultHeaderColor then
    Result := inherited HeaderBackColor2
  else
    Result := dxOffice11NavPaneHeaderColor2;
end;

function TdxNavBarNavigationPaneViewInfo.HeaderAlphaBlend: Byte;
begin
  if not IsDefaultHeaderColor then
    Result := inherited HeaderAlphaBlend
  else
    Result := 255;
end;

function TdxNavBarNavigationPaneViewInfo.HeaderAlphaBlend2: Byte;
begin
  if not IsDefaultHeaderColor then
    Result := inherited HeaderAlphaBlend2
  else
    Result := 255;
end;

function TdxNavBarNavigationPaneViewInfo.HeaderGradientMode
  : TdxBarStyleGradientMode;
begin
  if not IsDefaultHeaderColor then
    Result := inherited HeaderGradientMode
  else
    Result := gmVertical;
end;

function TdxNavBarNavigationPaneViewInfo.HeaderFontColor: TColor;
begin
  Result := inherited HeaderFontColor;
  if Result = clNone then
    Result := dxOffice11NavPaneHeaderFontColor;
end;

function TdxNavBarNavigationPaneViewInfo.OverflowPanelBackColor: TColor;
begin
  if not IsDefaultOverflowPanelColor then
    Result := inherited OverflowPanelBackColor
  else
    Result := dxOffice11NavPaneGroupCaptionColor1;
end;

function TdxNavBarNavigationPaneViewInfo.OverflowPanelBackColor2: TColor;
begin
  if not IsDefaultOverflowPanelColor then
    Result := inherited OverflowPanelBackColor2
  else
    Result := dxOffice11NavPaneGroupCaptionColor2;
end;

function TdxNavBarNavigationPaneViewInfo.OverflowPanelAlphaBlend: Byte;
begin
  if not IsDefaultOverflowPanelColor then
    Result := inherited OverflowPanelAlphaBlend
  else
    Result := 255;
end;

function TdxNavBarNavigationPaneViewInfo.OverflowPanelAlphaBlend2: Byte;
begin
  if not IsDefaultOverflowPanelColor then
    Result := inherited OverflowPanelAlphaBlend2
  else
    Result := 255;
end;

function TdxNavBarNavigationPaneViewInfo.OverflowPanelGradientMode
  : TdxBarStyleGradientMode;
begin
  if not IsDefaultOverflowPanelColor then
    Result := inherited OverflowPanelGradientMode
  else
    Result := gmVertical;
end;

function TdxNavBarNavigationPaneViewInfo.SplitterBackColor: TColor;
begin
  if XPScheme = schUnknown then
    Result := dxOffice11NavPaneSplitterColor1
  else
    Result := inherited SplitterBackColor;
end;

function TdxNavBarNavigationPaneViewInfo.SplitterBackColor2: TColor;
begin
  if XPScheme = schUnknown then
    Result := dxOffice11NavPaneSplitterColor2
  else
    Result := inherited SplitterBackColor2;
end;

function TdxNavBarNavigationPaneViewInfo.SplitterGradientMode
  : TdxBarStyleGradientMode;
begin
  if XPScheme = schUnknown then
    Result := gmVertical
  else
    Result := inherited SplitterGradientMode;
end;

procedure TdxNavBarNavigationPaneViewInfo.AssignDefaultBackgroundStyle;
begin
  NavBar.DefaultStyles.Background.ResetValues;
  NavBar.DefaultStyles.Background.BackColor := clWindow;
  NavBar.DefaultStyles.Background.BackColor2 := clWindow;
end;

procedure TdxNavBarNavigationPaneViewInfo.AssignDefaultButtonStyle;
begin
  NavBar.DefaultStyles.Button.ResetValues;
  NavBar.DefaultStyles.Button.BackColor := clNone;
  NavBar.DefaultStyles.Button.BackColor2 := clNone;
end;

procedure TdxNavBarNavigationPaneViewInfo.AssignDefaultGroupBackgroundStyle;
begin
  NavBar.DefaultStyles.GroupBackground.ResetValues;
  NavBar.DefaultStyles.GroupBackground.BackColor := clWindow;
  NavBar.DefaultStyles.GroupBackground.BackColor2 := clWindow;
end;

procedure TdxNavBarNavigationPaneViewInfo.AssignDefaultGroupHeaderStyle;
begin
  NavBar.DefaultStyles.GroupHeader.ResetValues;
  NavBar.DefaultStyles.GroupHeader.BackColor := clNone;
  NavBar.DefaultStyles.GroupHeader.BackColor2 := clNone;
  NavBar.DefaultStyles.GroupHeader.Font.Style := [fsBold];
end;

procedure TdxNavBarNavigationPaneViewInfo.AssignDefaultGroupHeaderActiveStyle;
begin
  inherited;
  if IsHighContrastWhite then
    NavBar.DefaultStyles.GroupHeaderActive.Font.Color := clHighlightText;
end;

procedure TdxNavBarNavigationPaneViewInfo.
  AssignDefaultGroupHeaderHotTrackedStyle;
begin
  inherited;
  if IsHighContrastWhite then
    NavBar.DefaultStyles.GroupHeaderHotTracked.Font.Color := clHighlightText;
end;

procedure TdxNavBarNavigationPaneViewInfo.AssignDefaultGroupHeaderPressedStyle;
begin
  inherited;
  if IsHighContrastWhite then
    NavBar.DefaultStyles.GroupHeaderPressed.Font.Color := clHighlightText;
end;

procedure TdxNavBarNavigationPaneViewInfo.AssignDefaultItemStyle;
begin
  NavBar.DefaultStyles.Item.ResetValues;
  NavBar.DefaultStyles.Item.Font.Color := clWindowText;
  NavBar.DefaultStyles.Item.HAlignment := haCenter;
end;

procedure TdxNavBarNavigationPaneViewInfo.AssignDefaultItemDisabledStyle;
begin
  NavBar.DefaultStyles.ItemDisabled.Assign(NavBar.DefaultStyles.Item);
  NavBar.DefaultStyles.ItemDisabled.Font.Color := clGrayText;
end;

procedure TdxNavBarNavigationPaneViewInfo.
  AssignDefaultNavigationPaneHeaderStyle;
begin
  NavBar.DefaultStyles.NavigationPaneHeader.ResetValues;
  NavBar.DefaultStyles.NavigationPaneHeader.BackColor := clNone;
  NavBar.DefaultStyles.NavigationPaneHeader.BackColor2 := clNone;
  NavBar.DefaultStyles.NavigationPaneHeader.Font.Color := clNone;
  NavBar.DefaultStyles.NavigationPaneHeader.Font.Name := 'Arial';
  NavBar.DefaultStyles.NavigationPaneHeader.Font.Height :=
    ScaleFactor.Apply(-15);
  NavBar.DefaultStyles.NavigationPaneHeader.Font.Style := [fsBold];
end;

procedure TdxNavBarNavigationPaneViewInfo.CreateColors;
begin
  CreateOffice11NavPaneColors;
end;

procedure TdxNavBarNavigationPaneViewInfo.RefreshColors;
begin
  RefreshOffice11NavPaneColors;
end;

procedure TdxNavBarNavigationPaneViewInfo.ReleaseColors;
begin
  ReleaseOffice11NavPaneColors;
end;

function TdxNavBarNavigationPaneViewInfo.GetGroupEdges: TPoint;
begin
  Result := ScaleFactor.Apply(cxPoint(1, 0));
end;

function TdxNavBarNavigationPaneViewInfo.GetGroupSeparatorWidth: Integer;
begin
  Result := 0;
end;

function TdxNavBarNavigationPaneViewInfo.GetGroupBorderOffsets: TRect;
begin
  Result := inherited GetGroupBorderOffsets;
  if IsTopBorderNeeded then
    Result.Top := Result.Top + BorderWidth;
  if IsBottomBorderNeeded then
    Result.Bottom := Result.Bottom + BorderWidth;
end;

function TdxNavBarNavigationPaneViewInfo.GetGroupCaptionImageIndent: Integer;
begin
  Result := ScaleFactor.Apply(4);
end;

function TdxNavBarNavigationPaneViewInfo.GetGroupCaptionHeightAddon: Integer;
begin
  Result := ScaleFactor.Apply(4);
end;

function TdxNavBarNavigationPaneViewInfo.
  GetGroupCaptionPanelAccessibilityHelperClass
  : TdxNavBarCustomAccessibilityHelperClass;
begin
  Result := TdxNavBarNavigationPaneGroupCaptionPanelAccessibilityHelper;
end;

function TdxNavBarNavigationPaneViewInfo.GetNavBarCollapsedWidth: Integer;
begin
  Result := FCollapsedViewCalculator.GetCollapsedWidth;
end;

function TdxNavBarNavigationPaneViewInfo.GetNavBarMinExpandedWidth: Integer;
begin
  Result := FCollapsedViewCalculator.GetMinExpandedWidth;
end;

function TdxNavBarNavigationPaneViewInfo.GetOverflowPanelHeight: Integer;
begin
  Result := OverflowPanelViewInfo.GetImageHeight;
  if FCollapsedViewCalculator.ImageHeightPeer then
    Result := Max(FCollapsedViewCalculator.MaxImageSize.cy, Result);
end;

function TdxNavBarNavigationPaneViewInfo.GetSpaceBetweenGroups: Integer;
begin
  Result := GetGroupSeparatorWidth;
end;

function TdxNavBarNavigationPaneViewInfo.GetHeaderSignDirection: TcxDirection;
const
  ASignDirectionMap: array [Boolean] of TcxDirection = (dirLeft, dirRight);
begin
  Result := ASignDirectionMap[IsCollapsed xor (GetExpandDirection = dirLeft)];
end;

function TdxNavBarNavigationPaneViewInfo.GetSplitterHeight: Integer;
begin
  if not IsSplitterVisible then
    Result := 0
  else
    Result := ScaleFactor.Apply(7);
end;

function TdxNavBarNavigationPaneViewInfo.CanHasGroupViewAsIconView: Boolean;
begin
  Result := False;
end;

function TdxNavBarNavigationPaneViewInfo.CanHasHeader: Boolean;
begin
  Result := True;
end;

function TdxNavBarNavigationPaneViewInfo.CanHasImageInGroupCaption: Boolean;
begin
  Result := True;
end;

function TdxNavBarNavigationPaneViewInfo.CanGroupCaptionBoundsByImage: Boolean;
begin
  Result := True;
end;

function TdxNavBarNavigationPaneViewInfo.IsBottomBorderNeeded: Boolean;
begin
  Result := not IsGroupReflectionNeeded or IsInternal or
    NavBar.IsNavigationClient;
end;

function TdxNavBarNavigationPaneViewInfo.IsCollapsed: Boolean;
begin
  Result := Painter.Controller.Collapsed;
end;

function TdxNavBarNavigationPaneViewInfo.IsGroupPopupControlSizable: Boolean;
begin
  Result := TdxNavBarNavigationPaneBehaviorOptionsAccess
    (NavBar.OptionsBehavior.NavigationPane).AllowPopupResizing;
end;

function TdxNavBarNavigationPaneViewInfo.IsHeaderVisible: Boolean;
begin
  Result := NavBar.OptionsView.NavigationPane.ShowHeader;
end;

function TdxNavBarNavigationPaneViewInfo.IsSplitterVisible: Boolean;
begin
  Result := HasItemPanel and OverflowPanelViewInfo.IsVisible;
end;

function TdxNavBarNavigationPaneViewInfo.IsTopBorderNeeded: Boolean;
begin
  Result := not IsHeaderVisible;
end;

function TdxNavBarNavigationPaneViewInfo.HasItemPanel: Boolean;
begin
  Result := True;
end;

function TdxNavBarNavigationPaneViewInfo.HasOverflowPanel: Boolean;
begin
  Result := OverflowPanelViewInfo.IsVisible;
end;

procedure TdxNavBarNavigationPaneViewInfo.DoCreateGroupsInfo;

  procedure CheckFocusedAccessibleObject;
  var
    AFocusedAccessibleObject: IdxNavBarAccessibilityHelper;
  begin
    AFocusedAccessibleObject := TdxCustomNavBarAccess(NavBar)
      .FocusedAccessibleObject;
    if (AFocusedAccessibleObject <> nil) and (NavBar.ActiveGroup <> nil) and
      (AFocusedAccessibleObject.GetHelper = NavBar.ActiveGroup.
      CaptionPanelIAccessibilityHelper.GetHelper) then
      TdxCustomNavBarAccess(NavBar).FocusedAccessibleObject :=
        ActiveGroupCaptionPanelIAccessibilityHelper;
  end;

var
  I: Integer;
begin
  if IsActiveGroupVisible then
    AddGroup(Self, NavBar.ActiveGroup, False, True);

  if IsGroupReflectionNeeded then
    for I := 0 to NavBar.RootGroupCount - 1 do
      if NavBar.RootGroups[I].Visible then
        AddGroup(Self, NavBar.RootGroups[I], True, False);
  CheckFocusedAccessibleObject;
end;

procedure TdxNavBarNavigationPaneViewInfo.DoCalculateBounds(X, Y: Integer);
var
  I: Integer;
begin
  FCollapsedViewCalculator.Reset;
  CalculateMaxImageSize;
  CalculateHeaderBounds(X, Y);
  FItemPanelViewInfo.CalculateBounds(X, Y);
  CalculateSplitterBounds(X, Y);
  for I := GetRealGroupStartIndex to GroupCount - 1 do
    Groups[I].CalculateBounds(X, Y);
end;

procedure TdxNavBarNavigationPaneViewInfo.InternalCalculateMaxImageSize;
begin
  FCollapsedViewCalculator.CalculateMaxImageSize;
end;

function TdxNavBarNavigationPaneViewInfo.CreateCollapsedViewCalculator
  : TdxNavBarCollapsedViewCalculator;
begin
  Result := TdxNavBarNavigationPaneCalculator.Create(Self);
end;

function TdxNavBarNavigationPaneViewInfo.GetItemPanelRect: TRect;
begin
  Result := FItemPanelViewInfo.FRect;
end;

function TdxNavBarNavigationPaneViewInfo.GetPopupTopPos: Integer;
begin
  Result := cxRectHeight(HeaderRect) - 1;
end;

function TdxNavBarNavigationPaneViewInfo.GetBoundsUpdateType
  : TdxNavBarChangeType;
begin
  Result := doRecreate;
end;

function TdxNavBarNavigationPaneViewInfo.
  GetActiveGroupCaptionPanelAccessibilityHelperClass
  : TdxNavBarCustomAccessibilityHelperClass;
begin
  Result := TdxNavBarNavigationPaneActiveGroupCaptionPanelAccessibilityHelper;
end;

function TdxNavBarNavigationPaneViewInfo.GetOverflowPanelViewInfoClass
  : TdxNavBarOverflowPanelViewInfoClass;
begin
  Result := TdxNavBarOverflowPanelViewInfo;
end;

function TdxNavBarNavigationPaneViewInfo.GetPartIAccessibilityHelper
  (const APart: TdxNavBarPart): IdxNavBarAccessibilityHelper;
begin
  Result := GetNavPanePartIAccessibilityHelper(APart);
end;

function TdxNavBarNavigationPaneViewInfo.GetNavPanePartIAccessibilityHelper
  (const APart: TdxNavBarPart): IdxNavBarAccessibilityHelper;
begin
  case APart.MajorPartIndex of
    nbOverflowPanelItem:
      Result := GetOverflowPanelItemIAccessibilityHelper(APart.MinorPartIndex);
    nbOverflowPanelSign:
      Result := OverflowPanelSignIAccessibilityHelper;
    nbItemPanelCollapseItem:
      Result := FItemPanelViewInfo.ItemIAccessibilityHelpers
        [APart.MinorPartIndex];
    nbItemPanelCollapseBar:
      Result := FItemPanelViewInfo.CollapseBarIAccessibilityHelper;
    nbHeaderSign:
      Result := TdxNavBarHeaderPanelViewInfoAccess(HeaderPanelViewInfo)
        .IAccessibilityHelper;
  else
    Result := nil;
  end;
end;

procedure TdxNavBarNavigationPaneViewInfo.CreateInfo;
var
  AOverflowPanelItemCount: Integer;
begin
  inherited;
  if NavBar.ShowGroupCaptions then
    AOverflowPanelItemCount := NavBar.NavigationPaneOverflowPanelItemCount
  else
    AOverflowPanelItemCount := 0;
  CreateOverflowPanelInfo(AOverflowPanelItemCount, True);
  CreateItemPanelViewInfo;
end;

procedure TdxNavBarNavigationPaneViewInfo.CreateItemPanelViewInfo;
begin
  FItemPanelViewInfo.ActiveGroupViewInfo := ActiveGroupViewInfo;
  FItemPanelViewInfo.FIsCollapseMode := IsCollapsed;
  FItemPanelViewInfo.FCollapseBarGroup := NavBar.ActiveGroup;
  FItemPanelViewInfo.CreateItems;
end;

procedure TdxNavBarNavigationPaneViewInfo.CreateOverflowPanelInfo
  (AItemCount: Integer; AClearOld: Boolean);
var
  I: Integer;
  AItem: TdxNavBarOverflowPanelViewInfoItem;
begin
  if AClearOld then
    OverflowPanelViewInfo.ClearItems;
  if not HasOverflowPanel then
    Exit;
  for I := GroupCount - 1 downto GetRealGroupStartIndex do
  begin
    if OverflowPanelItemCount >= AItemCount then
      Break;
    AItem := OverflowPanelViewInfo.AddItem;
    AItem.Group := Groups[I].Group;
    RemoveGroup(Groups[I]);
  end;
end;

procedure TdxNavBarNavigationPaneViewInfo.CalculateMaxImageSize;
begin
  InternalCalculateMaxImageSize;
end;

procedure TdxNavBarNavigationPaneViewInfo.CalculateSplitterBounds(var X,
  Y: Integer);
begin
  if not IsSplitterVisible then
    Exit;

  FSplitterRect := Bounds(X, Y, ClientWidth - 2 * X, GetSplitterHeight);
  Y := FSplitterRect.Bottom + GetSpaceBetweenGroups;
end;

procedure TdxNavBarNavigationPaneViewInfo.ClearRects;
begin
  inherited;
  OverflowPanelViewInfo.ClearRects;
  SetRectEmpty(FSizeGripRect);
  SetRectEmpty(FSplitterRect);
end;

procedure TdxNavBarNavigationPaneViewInfo.CorrectBounds;
begin
  CorrectPanelsBounds;
  CalculateScrollButtonsBounds;
  CalculateSizeGripBounds;
end;

procedure TdxNavBarNavigationPaneViewInfo.CorrectPanelsBounds;

  procedure CalcHeightDifference(out AShortage, AHeightDifference: Integer);
  var
    ARequiredHeight, AMinRequiredHeight, AClientHeight: Integer;
    AItemPanelHeight, AMinItemPanelHeight: Integer;
  begin
    AMinItemPanelHeight := IfThen(HasItemPanel,
      FItemPanelViewInfo.GetMinHeight, 0);
    AMinRequiredHeight := GetMinHeight;

    if GroupCount > 0 then
      ARequiredHeight := Groups[GroupCount - 1].Rect.Bottom
    else
      ARequiredHeight := GroupsArea.Top + AMinItemPanelHeight;
    Inc(ARequiredHeight, OverflowPanelViewInfo.GetHeight);
    if GetRealGroupCount = 0 then
      Inc(ARequiredHeight, GetSplitterHeight);

    AClientHeight := IfThen(ClientHeight >= AMinRequiredHeight, ClientHeight,
      AMinRequiredHeight);
    AHeightDifference := AClientHeight - ARequiredHeight;
    AItemPanelHeight := cxRectHeight(FItemPanelViewInfo.FRect);

    AShortage := 0;
    if (AItemPanelHeight + AHeightDifference < AMinItemPanelHeight) or not HasItemPanel
    then // if AHeightDifference < 0
    begin
      AShortage := AMinItemPanelHeight - AItemPanelHeight - AHeightDifference;
      AHeightDifference := AMinItemPanelHeight - AItemPanelHeight;
    end
  end;

  procedure CorrectOverflowPanel(AShortage: Integer;
    var AHeightDifference: Integer);

    procedure CorrectOverflowPanelBounds;
    var
      X, Y: Integer;
    begin
      X := SplitterRect.Left;
      if (GetRealGroupCount = 0) or not NavBar.ShowGroupCaptions then
        Y := IfThen(HasItemPanel, SplitterRect.Bottom, GroupsArea.Top)
      else
        Y := Groups[GroupCount - 1].Rect.Bottom;
      OverflowPanelViewInfo.CalculateBounds(X, Y);
    end;

    procedure RemoveExcessGroups(var AOverflowPanelGroupCount: Integer);
    var
      I: Integer;
    begin
      for I := GroupCount - 1 downto GetRealGroupStartIndex do
      begin
        if AShortage <= 0 then
          Break;
        Dec(AShortage, cxRectHeight(Groups[I].Rect));
        Inc(AOverflowPanelGroupCount);
      end;
      if HasItemPanel then
        Inc(AHeightDifference, -AShortage);
    end;

  var
    AGroupCount: Integer;
  begin
    if not HasOverflowPanel then
      Exit;
    AGroupCount := NavBar.NavigationPaneOverflowPanelItemCount;
    if not NavBar.ShowGroupCaptions then
      AGroupCount := GroupCount - 1
    else if AShortage > 0 then
      RemoveExcessGroups(AGroupCount);
    CreateOverflowPanelInfo(AGroupCount, False);
    CorrectOverflowPanelBounds;
  end;

  procedure OffsetElements(AHeightDifference: Integer);
  var
    I: Integer;
  begin
    for I := GetRealGroupStartIndex to GroupCount - 1 do
      Groups[I].CorrectBounds(0, AHeightDifference);
    if IsSplitterVisible then
      OffsetRect(FSplitterRect, 0, AHeightDifference);
    OverflowPanelViewInfo.OffsetElements(AHeightDifference);
  end;

var
  AHeightDifference, AShortage: Integer;
begin
  CalcHeightDifference(AShortage, AHeightDifference);
  CorrectOverflowPanel(AShortage, AHeightDifference);
  CorrectTopVisibleIndex(AHeightDifference);
  if HasItemPanel then
    FItemPanelViewInfo.CorrectBounds(AHeightDifference);
  OffsetElements(AHeightDifference);
end;

function TdxNavBarNavigationPaneViewInfo.GetViewInfoAtDragPosition
  (const pt: TPoint; var ItemGroup: TdxNavBarGroupViewInfo;
  var Item1, Item2: TdxNavBarLinkViewInfo): Integer;
begin
  Result := inherited GetViewInfoAtDragPosition(pt, ItemGroup, Item1, Item2);
  if (ItemGroup <> nil) and (GroupCount > 0) and (ItemGroup <> Groups[0]) and
    (NavBar.ActiveGroup = ItemGroup.Group) then
  begin
    ItemGroup := Groups[0];
    Item1 := nil;
    if ItemGroup.ItemCount > 0 then
      Item2 := ItemGroup.Items[0]
    else
      Item2 := nil;
    Result := 0;
  end;
end;

function TdxNavBarNavigationPaneViewInfo.
  IsPtIncNavigationPaneOverflowPanelItemCount(const pt: TPoint): Boolean;
begin
  if GetRealGroupCount > 0 then
    Result := pt.Y > SplitterRect.Top + cxRectHeight(SplitterRect) div 2 +
      cxRectHeight(Groups[GetRealGroupStartIndex].Rect)
  else
    Result := pt.Y > SplitterRect.Top + cxRectHeight(SplitterRect) div 2 + 20;
end;

function TdxNavBarNavigationPaneViewInfo.
  IsPtDecNavigationPaneOverflowPanelItemCount(const pt: TPoint): Boolean;
begin
  if GetRealGroupCount > 0 then
    Result := pt.Y < SplitterRect.Top + cxRectHeight(SplitterRect) div 2 -
      cxRectHeight(Groups[GetRealGroupStartIndex].Rect)
  else
    Result := pt.Y < SplitterRect.Top + cxRectHeight(SplitterRect) div 2 - 20;
end;

function TdxNavBarNavigationPaneViewInfo.IsPtNavigationPaneHeader
  (const pt: TPoint): Boolean;
begin
  Result := IsPtHeader(pt);
end;

function TdxNavBarNavigationPaneViewInfo.IsPtNavigationPaneHeaderSign
  (const pt: TPoint): Boolean;
begin
  Result := IsPtHeaderSign(pt);
end;

function TdxNavBarNavigationPaneViewInfo.IsPtNavigationPaneOverflowPanel
  (const pt: TPoint): Boolean;
begin
  Result := PtInRect(OverflowPanelRect, pt);
end;

function TdxNavBarNavigationPaneViewInfo.IsPtNavigationPaneOverflowPanelSign
  (const pt: TPoint): Boolean;
begin
  Result := PtInRect(OverflowPanelSignRect, pt);
end;

function TdxNavBarNavigationPaneViewInfo.IsPtNavigationPaneSplitter
  (const pt: TPoint): Boolean;
begin
  Result := PtInRect(FSplitterRect, pt);
end;

function TdxNavBarNavigationPaneViewInfo.IsPtNavigationPaneItemPanel
  (const pt: TPoint): Boolean;
begin
  Result := PtInRect(FItemPanelViewInfo.FRect, pt);
end;

procedure TdxNavBarNavigationPaneViewInfo.DoShowPopupMenu(const APoint: TPoint);
begin
  TdxNavBarCustomOverflowPanelPopupMenuAccess(FPopupMenu).Show(APoint);
end;

procedure TdxNavBarNavigationPaneViewInfo.DoUpdatePopupMenu;
begin
  RecreateImageList;
  TdxNavBarCustomOverflowPanelPopupMenuAccess(FPopupMenu).Populate;
end;

function TdxNavBarNavigationPaneViewInfo.IsDefaultHeaderColor: Boolean;
begin
  Result := (inherited HeaderBackColor = clNone) or
    (inherited HeaderBackColor2 = clNone);
end;

function TdxNavBarNavigationPaneViewInfo.IsDefaultOverflowPanelColor: Boolean;
begin
  Result := (inherited OverflowPanelBackColor = clNone) or
    (inherited OverflowPanelBackColor2 = clNone);
end;

function TdxNavBarNavigationPaneViewInfo.
  IsDefaultBottomScrollButtonColor: Boolean;
begin
  Result := (inherited BottomScrollButtonBackColor = clNone) or
    (inherited BottomScrollButtonBackColor2 = clNone);
end;

function TdxNavBarNavigationPaneViewInfo.IsDefaultTopScrollButtonColor: Boolean;
begin
  Result := (inherited TopScrollButtonBackColor = clNone) or
    (inherited TopScrollButtonBackColor2 = clNone);
end;

procedure TdxNavBarNavigationPaneViewInfo.OnOverflowPanelSignButtonClickHandler
  (const AClientMousePos: TPoint);
begin
  DoShowPopupMenu(AClientMousePos);
end;

function TdxNavBarNavigationPaneViewInfo.GetPainter
  : TdxNavBarNavigationPanePainter;
begin
  Result := TdxNavBarNavigationPanePainter(inherited Painter);
end;

function TdxNavBarNavigationPaneViewInfo.
  GetActiveGroupCaptionPanelIAccessibilityHelper: IdxNavBarAccessibilityHelper;
begin
  if FActiveGroupCaptionPanelIAccessibilityHelper = nil then
    FActiveGroupCaptionPanelIAccessibilityHelper :=
      NavBarGetAccessibilityHelper
      (GetActiveGroupCaptionPanelAccessibilityHelperClass.Create(Self, NavBar));
  Result := FActiveGroupCaptionPanelIAccessibilityHelper;
end;

function TdxNavBarNavigationPaneViewInfo.GetHeaderSignIAccessibilityHelper
  : IdxNavBarAccessibilityHelper;
begin
  Result := TdxNavBarHeaderPanelViewInfoAccess(HeaderPanelViewInfo)
    .IAccessibilityHelper;
end;

function TdxNavBarNavigationPaneViewInfo.GetOverflowPanelIAccessibilityHelper
  : IdxNavBarAccessibilityHelper;
begin
  Result := OverflowPanelViewInfo.GetIAccessibilityHelper;
end;

procedure TdxNavBarNavigationPaneViewInfo.DoRightToLeftConversion;
begin
  inherited DoRightToLeftConversion;
  RTLConvert(FSplitterRect);
  FOverflowPanelViewInfo.DoRightToLeftConversion;
  FItemPanelViewInfo.DoRightToLeftConversion;
end;

function TdxNavBarNavigationPaneViewInfo.GetOverflowPanelImageList: TImageList;
begin
  Result := TdxNavBarCustomOverflowPanelPopupMenuAccess(FPopupMenu).ImageList;
end;

function TdxNavBarNavigationPaneViewInfo.GetOverflowPanelItemCount: Integer;
begin
  Result := OverflowPanelViewInfo.ItemCount;
end;

function TdxNavBarNavigationPaneViewInfo.GetOverflowPanelItems(AIndex: Integer)
  : TdxNavBarOverflowPanelViewInfoItem;
begin
  Result := OverflowPanelViewInfo.Items[AIndex];
end;

function TdxNavBarNavigationPaneViewInfo.GetOverflowPanelPopupMenu: TPopupMenu;
begin
  Result := FPopupMenu;
end;

function TdxNavBarNavigationPaneViewInfo.
  GetOverflowPanelVisibleItemCount: Integer;
begin
  Result := OverflowPanelViewInfo.VisibleItemCount;
end;

function TdxNavBarNavigationPaneViewInfo.GetOverflowPanelRect: TRect;
begin
  Result := FOverflowPanelViewInfo.Rect;
end;

function TdxNavBarNavigationPaneViewInfo.GetOverflowPanelSignRect: TRect;
begin
  Result := OverflowPanelViewInfo.SignRect;
end;

function TdxNavBarNavigationPaneViewInfo.GetOverflowPanelViewInfo
  : TdxNavBarOverflowPanelViewInfo;
begin
  Result := FOverflowPanelViewInfo as TdxNavBarOverflowPanelViewInfo;
end;

function TdxNavBarNavigationPaneViewInfo.
  GetOverflowPanelSignIAccessibilityHelper: IdxNavBarAccessibilityHelper;
begin
  Result := OverflowPanelViewInfo.SignIAccessibilityHelper;
end;

function TdxNavBarNavigationPaneViewInfo.
  GetOverflowPanelItemIAccessibilityHelper(AIndex: Integer)
  : IdxNavBarAccessibilityHelper;
begin
  Result := OverflowPanelViewInfo.ItemIAccessibilityHelpers[AIndex]
end;

function TdxNavBarNavigationPaneViewInfo.IsActiveGroupVisible: Boolean;
begin
  Result := not IsCollapsed and (NavBar.ActiveGroup <> nil) and
    NavBar.ActiveGroup.Visible;
end;

function TdxNavBarNavigationPaneViewInfo.IsGroupReflectionNeeded: Boolean;
begin
  Result := OverflowPanelViewInfo.IsVisible or NavBar.ShowGroupCaptions;
end;

function TdxNavBarNavigationPaneViewInfo.GetMinHeight: Integer;
begin
  Result := GroupsArea.Top + OverflowPanelViewInfo.GetHeight +
    GetSplitterHeight;
  if IsActiveGroupVisible and (ActiveGroupViewInfo.Control <> nil) and
    (ActiveGroupViewInfo.Control.GetMinHeight <> 0) and HasItemPanel then
    Inc(Result, FItemPanelViewInfo.GetMinHeight);
end;

function TdxNavBarNavigationPaneViewInfo.GetRealGroupStartIndex: Integer;
begin
  if IsActiveGroupVisible then
    Result := 1
  else
    Result := 0;
end;

function TdxNavBarNavigationPaneViewInfo.GetRealGroupCount: Integer;
begin
  Result := GroupCount - GetRealGroupStartIndex;
end;

procedure TdxNavBarNavigationPaneViewInfo.RecreateImageList;
begin
  TdxNavBarCustomOverflowPanelPopupMenuAccess(FPopupMenu).RecreateImageList;
end;

{ TdxNavBarNavigationPaneController }

procedure TdxNavBarNavigationPaneController.ClosePopupControl;
begin
  InternalClosePopupControl;
end;

procedure TdxNavBarNavigationPaneController.ShowPopupControl;
begin
  InternalShowPopupControl(NavBar.ActiveGroup,
    dxNavBarPart(nbItemPanelCollapseBar));
end;

procedure TdxNavBarNavigationPaneController.DoClick(const APart: TdxNavBarPart);
begin
  case HotPart.MajorPartIndex of
    nbItemPanelCollapseBar:
      DoCollapseBarClick;
    nbItemPanelCollapseItem:
      DoCollapseItemClick;
  else
    if not ViewInfo.OverflowPanelViewInfo.TryDoClick(HotPart) then
      inherited DoClick(APart);
  end;
end;

procedure TdxNavBarNavigationPaneController.DoMouseMove(AShift: TShiftState;
  const APoint: TPoint);
begin
  HotPart := GetPartAtPos(APoint);
  if PressedPart.MajorPartIndex = nbSplitter then
    DoSplitterDrag(APoint)
  else
    inherited DoMouseMove(AShift, APoint);
end;

procedure TdxNavBarNavigationPaneController.DoMouseLeave;
begin
  HotPart := dxNavBarPart(nbNone);
  inherited;
end;

procedure TdxNavBarNavigationPaneController.DoSetHotPart
  (const APart: TdxNavBarPart);
begin
  InvalidateAll(doRedraw);
end;

procedure TdxNavBarNavigationPaneController.DoSetPressedPart
  (const APart: TdxNavBarPart);
begin
  InvalidateAll(doRedraw);
end;

function TdxNavBarNavigationPaneController.GetRealCollapsed: Boolean;
begin
  Result := NavBar.OptionsBehavior.NavigationPane.Collapsed;
end;

function TdxNavBarNavigationPaneController.GetCollapsible: Boolean;
begin
  Result := FNavBar.OptionsBehavior.NavigationPane.Collapsible;
end;

procedure TdxNavBarNavigationPaneController.SetCollapsed(AValue: Boolean);
begin
  NavBar.OptionsBehavior.NavigationPane.Collapsed := AValue;
end;

function TdxNavBarNavigationPaneController.CalcHintRect
  (AHintInfo: THintInfo): TRect;
begin
  if IsAnyItemHotTracked then
  begin
    if NavBar.ShowNavigationPaneOverflowPanelHints then
      Result := GetNavPaneItemHintRect(AHintInfo.CursorPos);
  end
  else
    Result := inherited CalcHintRect(AHintInfo);
end;

procedure TdxNavBarNavigationPaneController.DoShowHint(var AHintInfo
  : THintInfo);
begin
  if IsAnyItemHotTracked then
  begin
    if NavBar.ShowNavigationPaneOverflowPanelHints then
      DoShowOverflowPanelHint(AHintInfo);
  end
  else
    inherited;
end;

procedure TdxNavBarNavigationPaneController.DoShowOverflowPanelHint
  (var AHintInfo: THintInfo);
begin
  ViewInfo.HintText := GetNavPaneItemHintText;
  AHintInfo.CursorRect := GetNavPaneItemHintCursorRect;
end;

function TdxNavBarNavigationPaneController.GetNavPaneItemHintRect
  (const ACursorPos: TPoint): TRect;
var
  ANavBarObject: TObject;
begin
  case HotPart.MajorPartIndex of
    nbOverflowPanelItem:
      ANavBarObject := ViewInfo.OverflowPanelItems
        [HotPart.MinorPartIndex].Group;
    nbItemPanelCollapseItem:
      ANavBarObject := TdxNavBarItemPanelViewInfoItem
        (ViewInfo.ItemPanelViewInfo.Items[HotPart.MinorPartIndex])
        .ItemLink.Item;
  else
    ANavBarObject := nil;
  end;
  Result := GetItemHintRect(ANavBarObject, CalcOverflowPanelHintRect);
end;

function TdxNavBarNavigationPaneController.GetNavPaneItemHintText: string;
begin
  Result := '';
  case HotPart.MajorPartIndex of
    nbOverflowPanelItem:
      Result := RemoveAccelChars(ViewInfo.OverflowPanelItems
        [HotPart.MinorPartIndex].Group.Caption, False);
    nbOverflowPanelSign:
      Result := cxGetResourceString
        (@sdxNavigationPaneOverflowPanelCustomizeHint);
    nbItemPanelCollapseItem:
      Result := RemoveAccelChars
        (GetLinkHintText(ViewInfo.ItemPanelViewInfo.Items
        [HotPart.MinorPartIndex].ItemLink), False);
    nbItemPanelCollapseBar:
      Result := cxGetResourceString(@sdxNavigationPaneCollapseBarHint);
  else
    Result := GetHintText;
  end;
end;

function TdxNavBarNavigationPaneController.GetNavPaneItemHintCursorRect: TRect;
begin
  case HotPart.MajorPartIndex of
    nbOverflowPanelItem:
      Result := ViewInfo.OverflowPanelItems[HotPart.MinorPartIndex]
        .SelectionRect;
    nbOverflowPanelSign:
      Result := ViewInfo.OverflowPanelSignRect;
    nbItemPanelCollapseItem:
      Result := ViewInfo.ItemPanelViewInfo.Items[HotPart.MinorPartIndex].Rect;
    nbItemPanelCollapseBar:
      Result := ViewInfo.ItemPanelViewInfo.CollapseBarRect;
  else
    Result := GetHintCursorRect;
  end;
end;

procedure TdxNavBarNavigationPaneController.DoOverflowPanelItemClick;
begin
  ViewInfo.OverflowPanelViewInfo.DoItemClick;
end;

procedure TdxNavBarNavigationPaneController.DoOverflowPanelSignClick;
begin
  ViewInfo.OverflowPanelViewInfo.DoSignClick;
end;

procedure TdxNavBarNavigationPaneController.DoCollapseBarClick;
begin
  InternalShowPopupControl(NavBar.ActiveGroup,
    dxNavBarPart(nbItemPanelCollapseBar));
end;

procedure TdxNavBarNavigationPaneController.DoCollapseItemClick;
begin
  DoLinkClick(NavBar, ViewInfo.FItemPanelViewInfo.Items[HotPart.MinorPartIndex]
    .ItemLink);
end;

procedure TdxNavBarNavigationPaneController.DoSplitterDrag
  (const APoint: TPoint);
begin
  if FNavBar.CanDecNavigationPaneOverflowPanelItemCount and
    ViewInfo.IsPtDecNavigationPaneOverflowPanelItemCount(APoint) then
    FNavBar.DoDecNavigationPaneOverflowPanelItemCount
  else if FNavBar.CanIncNavigationPaneOverflowPanelItemCount and
    ViewInfo.IsPtIncNavigationPaneOverflowPanelItemCount(APoint) then
    FNavBar.DoIncNavigationPaneOverflowPanelItemCount;
end;

function TdxNavBarNavigationPaneController.CanFocusOnClick
  (const APoint: TPoint): Boolean;
begin
  Result := not ViewInfo.IsPtNavigationPaneSplitter(APoint) and
    inherited CanFocusOnClick(APoint);
end;

function TdxNavBarNavigationPaneController.CanHasPopupControl: Boolean;
begin
  Result := True;
end;

function TdxNavBarNavigationPaneController.CreateGroupPopupControl
  : TdxNavBarCustomPopupControl;
begin
  Result := TdxNavBarPopupControl.Create(NavBar);
end;

function TdxNavBarNavigationPaneController.GetCursor: HIcon;

  function InternalGetCursor(const APart: TdxNavBarPart;
    out ACursor: HIcon): Boolean;
  begin
    Result := not IsdxNavBarPartsEqual(APart, dxNavBarPart(nbNone));
    if IsdxNavBarPartsEqual(APart, dxNavBarPart(nbSplitter)) then
      if FNavBar.ShowGroupCaptions then
        ACursor := Screen.Cursors[crSizeNS]
      else
        ACursor := 0
    else
      ACursor := Screen.Cursors[FNavBar.HotTrackedGroupCursor];
  end;

begin
  if not(InternalGetCursor(PressedPart, Result) or
    InternalGetCursor(GetPartAtPos(NavBar.ScreenToClient(GetMouseCursorPos)),
    Result)) then
    Result := inherited GetCursor;
end;

function TdxNavBarNavigationPaneController.IsAnyItemHotTracked: Boolean;
begin
  Result := not IsdxNavBarPartsEqual
    (GetPartAtPos(NavBar.ScreenToClient(GetMouseCursorPos)),
    dxNavBarPart(nbNone));
end;

function TdxNavBarNavigationPaneController.
  IsOverflowPanelGroupHotTracked: Boolean;
begin
  Result := HotPart.MajorPartIndex = nbOverflowPanelItem;
end;

function TdxNavBarNavigationPaneController.GetNavPanePartState
  (const APart: TdxNavBarPart): TdxNavBarNavPanePartState;
var
  AGroup: TdxNavBarGroup;
begin
  if APart.MajorPartIndex = nbOverflowPanelItem then
    AGroup := OverflowPanelGroup[APart.MinorPartIndex]
  else
    AGroup := nil;

  if (AGroup <> nil) and (FNavBar.ActiveGroup = AGroup) then
  begin
    if IsdxNavBarPartsEqual(HotPart, APart) and
      (IsdxNavBarPartsEqual(PressedPart, dxNavBarPart(nbNone)) or
      IsdxNavBarPartsEqual(PressedPart, APart)) then
      Result := oisHotCheck
    else
      Result := oisChecked
  end
  else if IsdxNavBarPartsEqual(DroppedDownPart, APart) then
    Result := oisDroppedDown
  else if IsdxNavBarPartsEqual(PressedPart, APart) then
    if IsdxNavBarPartsEqual(HotPart, APart) then
      Result := oisPressed
    else
      Result := oisNormal
  else if IsdxNavBarPartsEqual(HotPart, APart) and
    IsdxNavBarPartsEqual(PressedPart, dxNavBarPart(nbNone)) then
    Result := oisHot
  else
    Result := oisNormal;
end;

procedure TdxNavBarNavigationPaneController.CalcOverflowPanelHintRect
  (AItem: TObject; var ARect: TRect);
begin
  if Assigned(NavBar.OnCalcNavigationPaneOverflowPanelHintRect) then
    NavBar.OnCalcNavigationPaneOverflowPanelHintRect(NavBar,
      TdxNavBarGroup(AItem), ViewInfo, ARect);
end;

function TdxNavBarNavigationPaneController.GetPartAtPos(const APoint: TPoint)
  : TdxNavBarPart;
begin
  Result := ViewInfo.OverflowPanelViewInfo.GetPartAtPos(APoint);
  if (Result.MajorPartIndex = nbNone) then
  begin
    if ViewInfo.IsPtNavigationPaneSplitter(APoint) then
      Result.MajorPartIndex := nbSplitter
    else if Collapsed and ViewInfo.IsPtNavigationPaneItemPanel(APoint) then
      Result := ViewInfo.FItemPanelViewInfo.GetPartAtPos(APoint)
    else
      Result := inherited GetPartAtPos(APoint);
  end;
end;

function TdxNavBarNavigationPaneController.GetPartState
  (const APart: TdxNavBarPart): TdxNavBarPartState;
begin
  Result := GetNavPanePartState(APart);
  if (Result = oisNormal) and (APart.MajorPartIndex = nbOverflowPanelSign) then
    Result := inherited GetPartState(APart);
end;

function TdxNavBarNavigationPaneController.GetViewInfo
  : TdxNavBarNavigationPaneViewInfo;
begin
  Result := TdxNavBarNavigationPaneViewInfo(inherited ViewInfo);
end;

function TdxNavBarNavigationPaneController.GetOverflowPanelGroup
  (AIndex: Integer): TdxNavBarGroup;
begin
  Result := ViewInfo.OverflowPanelViewInfo.Group[AIndex];
end;

{ TdxNavBarNavigationPanePainter }

constructor TdxNavBarNavigationPanePainter.Create(ANavBar: TdxCustomNavBar);
begin
  inherited Create(ANavBar);
  CreateOverflowPanelPainter;
end;

destructor TdxNavBarNavigationPanePainter.Destroy;
begin
  FreeAndNil(FOverflowPanelPainter);
  inherited Destroy;
end;

procedure TdxNavBarNavigationPanePainter.DrawNavBarControl;
begin
  inherited DrawNavBarControl;
  DrawItemPanel;
  DrawOverflowPanel;
  DrawSplitter;
  DrawBorder;
  InternalDrawSizeGrip(cxCanvas);
end;

procedure TdxNavBarNavigationPanePainter.DrawGroupBackground(AGroupViewInfo
  : TdxNavBarGroupViewInfo);
begin
  inherited;

end;

procedure TdxNavBarNavigationPanePainter.DrawGroupCaption(AGroupViewInfo
  : TdxNavBarGroupViewInfo);
begin
  DrawGroupCaptionButton(AGroupViewInfo);
  if not Controller.Collapsed then
    DrawGroupCaptionText(AGroupViewInfo);
  if AGroupViewInfo.IsCaptionImageVisible then
    DrawGroupCaptionImage(AGroupViewInfo);
end;

procedure TdxNavBarNavigationPanePainter.DrawHintWindow
  (AHintWindow: TdxNavBarHintWindow);
begin
  if Controller.IsAnyItemHotTracked then
    DrawOverflowPanelHintWindow(AHintWindow.Canvas, AHintWindow.ClientRect)
  else
    inherited;
end;

procedure TdxNavBarNavigationPanePainter.DrawItemPanel;
var
  I: Integer;
begin
  if Controller.Collapsed then
  begin
    DrawCollapseBar(ViewInfo.FItemPanelViewInfo);
    for I := 0 to ViewInfo.FItemPanelViewInfo.ItemCount - 1 do
      DrawCollapseItem(ViewInfo.FItemPanelViewInfo.Items[I],
        GetNavPanePartState(dxNavBarPart(nbItemPanelCollapseItem, I)));
  end;
end;

procedure TdxNavBarNavigationPanePainter.DrawItemsRect(AGroupViewInfo
  : TdxNavBarGroupViewInfo);
begin
  if ViewInfo.CanHasVisibleItemsInGroup(AGroupViewInfo.Group) then
    inherited;
end;

procedure TdxNavBarNavigationPanePainter.DrawGroupCaptionButton
  (AGroupViewInfo: TdxNavBarGroupViewInfo);
begin
  with AGroupViewInfo do
    TdxNavBarOffice11NavPaneGroupButtonPainter.DrawButton(Canvas, CaptionRect,
      CaptionImage, CaptionBackColor, CaptionBackColor2, CaptionAlphaBlend,
      CaptionAlphaBlend2, CaptionGradientMode, CaptionBorderColor, State,
      ScaleFactor);
end;

procedure TdxNavBarNavigationPanePainter.DrawCollapseBar(AItemPanelViewInfo
  : TdxNavBarItemPanelViewInfo);
var
  AState: TdxNavBarNavPanePartState;
begin
  AState := GetNavPanePartState(dxNavBarPart(nbItemPanelCollapseBar));
  DrawCollapseElementBackground(AItemPanelViewInfo.CollapseBarRect, AState);
  InternalDrawVerticalText(AItemPanelViewInfo.CollapseBarFont,
    AItemPanelViewInfo.CollapseBarText, AItemPanelViewInfo.CollapseBarRect);
  if AItemPanelViewInfo.CollapseBarIAccessibilityHelper.IsFocused then
    DrawItemPanelPartFocusRect(AItemPanelViewInfo.CollapseBarRect);
end;

procedure TdxNavBarNavigationPanePainter.DrawCollapseElementBackground
  (const ARect: TRect; AState: TdxNavBarNavPanePartState);
begin
  case AState of
    oisNormal:
      begin
        Canvas.Brush.Color := dxGetMiddleRGB(ViewInfo.OverflowPanelBackColor,
          ViewInfo.OverflowPanelBackColor2, 50);
        Canvas.FillRect(ARect);
      end;
  else
    DrawOverflowPanelItemBackground(Canvas, AState, ARect);
  end;
  cxCanvas.FrameRect(ARect, clBlack, 1, [bBottom]);
end;

procedure TdxNavBarNavigationPanePainter.DrawCollapseItem
  (AItemViewInfo: TdxNavBarItemPanelViewInfoItem;
  AState: TdxNavBarNavPanePartState);
begin
  DoDrawCollapseItem(AItemViewInfo, AState);
end;

procedure TdxNavBarNavigationPanePainter.DrawBorder;
var
  ABorderRect: TRect;
begin
  with cxCanvas do
  begin
    SaveClipRegion;
    try
      ABorderRect := cxRectBounds(0, 0, ViewInfo.ClientWidth,
        ViewInfo.ClientHeight);
      SetClipRegion(TcxRegion.Create(cxRectInflate(ABorderRect,
        -ViewInfo.BorderWidth, -ViewInfo.BorderWidth)), roSubtract);
      FillRect(ABorderRect, ViewInfo.BorderColor);
    finally
      RestoreClipRegion;
    end;
  end;
end;

procedure TdxNavBarNavigationPanePainter.DrawHeaderBackground;
var
  R: TRect;
begin
  R := ViewInfo.HeaderRect;
  Inc(R.Top);
  with ViewInfo do
    BackgroundPainterClass.DrawBackground(Canvas, R, HeaderImage, False, clNone,
      HeaderBackColor, HeaderBackColor2, HeaderAlphaBlend, HeaderAlphaBlend2,
      OverflowPanelGradientMode, ScaleFactor);
end;

procedure TdxNavBarNavigationPanePainter.DrawHeaderSign;
const
  AStateMap: array [Boolean] of TdxNavBarObjectStates = ([sExpanded], []);
var
  ACanvas: TcxCanvas;
  ABitmap: TcxBitmap;
  ASignColor: TColor;
begin
  ABitmap := TcxBitmap.CreateSize(ViewInfo.HeaderSignRect);
  try
    ACanvas := ABitmap.cxCanvas;

    case GetNavPanePartState(dxNavBarPart(nbHeaderSign)) of
      oisNormal:
        begin
          ASignColor := ViewInfo.HeaderFontColor;
          ACanvas.CopyRect(ABitmap.ClientRect, Canvas, ViewInfo.HeaderSignRect);
        end;
    else
      if IsHighContrastWhite then
        ASignColor := clHighlightText
      else
        ASignColor := clBlack;
      DrawOverflowPanelItemBackground(ACanvas.Canvas,
        dxNavBarPart(nbHeaderSign), ABitmap.ClientRect);
    end;

    ABitmap.Rotate(raMinus90);
    TdxNavBarOffice11ExplorerBarSignPainter.DrawSign(ACanvas.Canvas,
      ABitmap.ClientRect, ScaleFactor, ASignColor, clNone, clNone,
      AStateMap[ViewInfo.GetHeaderSignDirection = dirRight]);
    ABitmap.Rotate(raPlus90);

    with ViewInfo.HeaderSignRect.TopLeft do
      Canvas.Draw(X, Y, ABitmap);
  finally
    ABitmap.Free;
  end;
end;

procedure TdxNavBarNavigationPanePainter.DrawPopupControl(ACanvas: TcxCanvas;
  AViewInfo: TdxNavBarPopupControlViewInfo);
begin
  ACanvas.FrameRect(AViewInfo.Rect, ViewInfo.BorderColor);
  ACanvas.SaveClipRegion;
  try
    ACanvas.ExcludeClipRect(AViewInfo.ClientRect);
    ACanvas.FillRect(cxRectInflate(AViewInfo.Rect, -1, -1), $EEC6A9);
  finally
    ACanvas.RestoreClipRegion;
  end;
end;

procedure TdxNavBarNavigationPanePainter.DrawOverflowPanel;
var
  AHandled: Boolean;
begin
  if not OverflowPanelViewInfo.IsVisible then
    Exit;
  AHandled := False;
  if Assigned(NavBar.OnCustomDrawNavigationPaneOverflowPanel) then
    NavBar.OnCustomDrawNavigationPaneOverflowPanel(NavBar, Canvas, ViewInfo,
      AHandled);
  if not AHandled then
    DoDrawOverflowPanel;
end;

procedure TdxNavBarNavigationPanePainter.DrawOverflowPanelBackground;
begin
  FOverflowPanelPainter.DrawBackground(Canvas, ViewInfo.OverflowPanelViewInfo);
end;

procedure TdxNavBarNavigationPanePainter.DrawOverflowPanelSign;
begin
  FOverflowPanelPainter.DrawSign(Canvas, ViewInfo.OverflowPanelViewInfo);
end;

procedure TdxNavBarNavigationPanePainter.DrawOverflowPanelItem
  (AItem: TdxNavBarOverflowPanelViewInfoItem);
begin
  FOverflowPanelPainter.DrawItem(Canvas, ViewInfo.OverflowPanelViewInfo, AItem);
end;

procedure TdxNavBarNavigationPanePainter.DrawOverflowPanelItems;
begin
  FOverflowPanelPainter.DrawItems(Canvas, ViewInfo.OverflowPanelViewInfo);
end;

procedure TdxNavBarNavigationPanePainter.DrawOverflowPanelHintWindow
  (ACanvas: TCanvas; const ARect: TRect);
var
  AHandled: Boolean;
  AGroup: TdxNavBarGroup;
begin
  AHandled := False;
  if Assigned(NavBar.OnCustomDrawNavigationPaneOverflowPanelHint) then
  begin
    if Controller.IsOverflowPanelGroupHotTracked then
      AGroup := ViewInfo.OverflowPanelItems
        [Controller.HotPart.MinorPartIndex].Group
    else
      AGroup := nil;
    NavBar.OnCustomDrawNavigationPaneOverflowPanelHint(NavBar, ACanvas, AGroup,
      NavBar.ViewInfo.HintText, ARect, AHandled);
  end;
  if not AHandled then
    with ViewInfo do
      TdxNavBarCustomHintPainter.DrawHint(ACanvas, ARect, ViewInfo.HintText,
        HintImage, HintBackColor, HintBackColor2, HintAlphaBlend,
        HintAlphaBlend2, HintGradientMode, HintFont, ScaleFactor);
end;

procedure TdxNavBarNavigationPanePainter.DrawSplitter;
var
  AHandled: Boolean;
begin
  if not ViewInfo.IsSplitterVisible then
    Exit;
  AHandled := False;
  if Assigned(NavBar.OnCustomDrawNavigationPaneSplitter) then
    NavBar.OnCustomDrawNavigationPaneSplitter(NavBar, Canvas, ViewInfo,
      AHandled);
  if not AHandled then
    DoDrawSplitter;
end;

procedure TdxNavBarNavigationPanePainter.DrawSplitterSign;
var
  APoint: TPoint;
begin
  if dxOffice11NavPaneSplitterBitmap <> nil then
  begin
    APoint := cxRectCenter(ViewInfo.SplitterRect,
      dxOffice11NavPaneSplitterBitmap.Width,
      dxOffice11NavPaneSplitterBitmap.Height).TopLeft;
    if APoint.X > ViewInfo.OverflowPanelRect.Left then
      Canvas.Draw(APoint.X, APoint.Y, dxOffice11NavPaneSplitterBitmap);
  end;
end;

procedure TdxNavBarNavigationPanePainter.DrawPopupMenuItem(ACanvas: TCanvas;
  ARect: TRect; AImageList: TCustomImageList; AImageIndex: Integer;
  AText: string; State: TdxNavBarObjectStates);
var
  R: TRect;
begin
  BackgroundPainterClass.DrawBackground(ACanvas, ARect, nil, False, clNone,
    clMenu, clMenu, 255, 255, gmVertical, ScaleFactor);
  R := ARect;
  R.Right := R.Left + 2 * ViewInfo.OverflowPanelViewInfo.GetPopupMenuImageIndent
    + ViewInfo.GetSmallImageWidth;
  if NavBar.UseRightToLeftAlignment then
    R := TdxRightToLeftLayoutConverter.ConvertRect(R, ARect);
  BackgroundPainterClass.DrawBackground(ACanvas, R, nil, False, clNone,
    dxOffice11NavPaneGroupCaptionColor1, dxOffice11NavPaneGroupCaptionColor2,
    255, 255, gmHorizontal, ScaleFactor);
  if AText <> '-' then
  begin
    InflateRect(R, -1, -1);
    if sSelected in State then
    begin
      ButtonPainterClass.DrawButton(ACanvas, ARect, nil,
        dxOffice11NavPaneGroupCaptionHotColor1,
        dxOffice11NavPaneGroupCaptionHotColor2, 255, 255, gmVertical,
        dxOffice11NavPaneBorder, [], ScaleFactor);
      if sActive in State then
        ButtonPainterClass.DrawButton(ACanvas, R, nil,
          dxOffice11NavPaneGroupCaptionPressedColor1,
          dxOffice11NavPaneGroupCaptionPressedColor2, 255, 255, gmVertical,
          dxOffice11NavPaneBorder, [], ScaleFactor);
    end
    else if sActive in State then
      ButtonPainterClass.DrawButton(ACanvas, R, nil,
        dxOffice11NavPaneGroupCaptionHotColor1,
        dxOffice11NavPaneGroupCaptionHotColor1, 255, 255, gmVertical,
        dxOffice11NavPaneBorder, [], ScaleFactor);

    InflateRect(R, 1 - ViewInfo.OverflowPanelViewInfo.GetPopupMenuImageIndent,
      1 - ViewInfo.OverflowPanelViewInfo.GetPopupMenuImageIndent);
    cxDrawImage(ACanvas.Handle, R, R, nil, AImageList, AImageIndex,
      EnabledImageDrawModeMap[not(sDisabled in State)]);

    R := ARect;
    R.Left := R.Left + 2 * ViewInfo.OverflowPanelViewInfo.
      GetPopupMenuImageIndent + ViewInfo.GetSmallImageWidth +
      ViewInfo.OverflowPanelViewInfo.GetPopupMenuTextIndent;
    if NavBar.UseRightToLeftAlignment then
      R := TdxRightToLeftLayoutConverter.ConvertRect(R, ARect);
    if sDisabled in State then
      ACanvas.Font.Color := clGrayText
    else if (sSelected in State) and IsHighContrastWhite then
      ACanvas.Font.Color := clHighlightText
    else
      ACanvas.Font.Color := clMenuText;

    cxDrawText(ACanvas, AText, R, NavBar.DrawTextBiDiModeFlags(DT_LEFT or
      DT_VCENTER or DT_SINGLELINE));
  end
  else
  begin
    ACanvas.Pen.Color := dxGetDarkerColor
      (dxOffice11NavPaneGroupCaptionColor2, 80);
    if NavBar.UseRightToLeftAlignment then
    begin
      ACanvas.MoveTo(ARect.Left, R.Top + cxRectHeight(ARect) div 2);
      ACanvas.LineTo(R.Left - cxRectWidth(R) div 2,
        R.Top + cxRectHeight(ARect) div 2);
    end
    else
    begin
      ACanvas.MoveTo(R.Right + cxRectWidth(R) div 2,
        R.Top + cxRectHeight(ARect) div 2);
      ACanvas.LineTo(ARect.Right, R.Top + cxRectHeight(ARect) div 2);
    end;
  end;
end;

class function TdxNavBarNavigationPanePainter.ButtonPainterClass
  : TdxNavBarCustomButtonPainterClass;
begin
  Result := TdxNavBarNavigationPaneButtonPainter;
end;

class function TdxNavBarNavigationPanePainter.GetViewInfoClass
  : TdxNavBarViewInfoClass;
begin
  Result := TdxNavBarNavigationPaneViewInfo;
end;

class function TdxNavBarNavigationPanePainter.GetGroupViewInfoClass
  : TdxNavBarGroupViewInfoClass;
begin
  Result := TdxNavBarNavigationPaneGroupViewInfo;
end;

class function TdxNavBarNavigationPanePainter.GetHeaderPanelViewInfoClass
  : TdxNavBarHeaderPanelViewInfoClass;
begin
  Result := TdxNavBarNavigationPaneHeaderPanelViewInfo;
end;

class function TdxNavBarNavigationPanePainter.GetPopupControlViewInfoClass
  : TdxNavBarPopupControlViewInfoClass;
begin
  Result := TdxNavBarPopupControlViewInfo;
end;

class function TdxNavBarNavigationPanePainter.
  OverflowPanelBackgroundPainterClass: TdxNavBarCustomButtonPainterClass;
begin
  Result := TdxNavBarOffice11NavPaneGroupButtonPainter;
end;

function TdxNavBarNavigationPanePainter.GetDefaultOverflowPanelBitmap: TBitmap;
begin
  Result := ViewInfo.GetDefaultOverflowPanelBitmap;
end;

function TdxNavBarNavigationPanePainter.GetControllerClass
  : TdxNavBarControllerClass;
begin
  Result := TdxNavBarNavigationPaneController;
end;

function TdxNavBarNavigationPanePainter.UseHeaderCustomDrawing: Boolean;
begin
  Result := inherited UseHeaderCustomDrawing;
  if Assigned(NavBar.OnCustomDrawNavigationPaneHeader) then
    NavBar.OnCustomDrawNavigationPaneHeader(Self, Canvas, ViewInfo, Result);
end;

function TdxNavBarNavigationPanePainter.GetNavPanePartState
  (const APart: TdxNavBarPart): TdxNavBarNavPanePartState;
begin
  Result := Controller.GetNavPanePartState(APart);
  if (Result = oisNormal) and ViewInfo.NavPanePartIAccessibilityHelpers[APart].IsPressed
  then
    Result := oisPressed;
  if (Result = oisNormal) and (APart.MajorPartIndex in [nbOverflowPanelSign,
    nbOverflowPanelItem, nbHeaderSign]) and
    ViewInfo.NavPanePartIAccessibilityHelpers[APart].IsFocused then
    Result := oisHot;
end;

procedure TdxNavBarNavigationPanePainter.CreateOverflowPanelPainter;
begin
  FreeAndNil(FOverflowPanelPainter);
  FOverflowPanelPainter := InternalCreateOverflowPainter;
end;

function TdxNavBarNavigationPanePainter.InternalCreateOverflowPainter
  : TdxNavBarOverflowPanelPainter;
begin
  Result := TdxNavBarOverflowPanelPainter.Create;
end;

procedure TdxNavBarNavigationPanePainter.DoDrawOverflowPanel;
begin
  DrawOverflowPanelBackground;
  DrawOverflowPanelSign;
  DrawOverflowPanelItems;
end;

procedure TdxNavBarNavigationPanePainter.DoDrawSplitter;
var
  R: TRect;
begin
  with ViewInfo do
  begin
    Canvas.Brush.Color := clWhite;
    R := SplitterRect;
    Canvas.FillRect(Rect(R.Left, R.Top, R.Right, R.Top + 1));
    Inc(R.Top);
    BackgroundPainterClass.DrawBackground(Canvas, R, nil, False, clNone,
      SplitterBackColor, SplitterBackColor2, SplitterAlphaBlend,
      SplitterAlphaBlend2, SplitterGradientMode, ScaleFactor);
  end;
  DrawSplitterSign;
end;

function TdxNavBarNavigationPanePainter.GetViewInfo
  : TdxNavBarNavigationPaneViewInfo;
begin
  if inherited ViewInfo is TdxNavBarNavigationPaneViewInfo then
    Result := TdxNavBarNavigationPaneViewInfo(inherited ViewInfo)
  else
    Result := nil;
end;

function TdxNavBarNavigationPanePainter.GetController
  : TdxNavBarNavigationPaneController;
begin
  Result := TdxNavBarNavigationPaneController(inherited Controller);
end;

function TdxNavBarNavigationPanePainter.GetOverflowPanelViewInfo
  : TdxNavBarOverflowPanelViewInfo;
begin
  Result := ViewInfo.OverflowPanelViewInfo;
end;

procedure TdxNavBarNavigationPanePainter.DoDrawCollapseItem
  (AItemViewInfo: TdxNavBarItemPanelViewInfoItem;
  AState: TdxNavBarNavPanePartState);
begin
  DrawCollapseElementBackground(AItemViewInfo.Rect, AState);
  InternalDrawVerticalText(AItemViewInfo.Font, AItemViewInfo.Caption,
    AItemViewInfo.TextRect);
  if ImagePainterClass.IsValidImage(AItemViewInfo.ImageList,
    AItemViewInfo.ImageIndex) then
    ImagePainterClass.DrawImage(Canvas, AItemViewInfo.ImageList,
      AItemViewInfo.ImageIndex, AItemViewInfo.ImageRect);
  if ViewInfo.ItemPanelViewInfo.ItemIAccessibilityHelpers[AItemViewInfo.Index].IsFocused
  then
    DrawItemPanelPartFocusRect(AItemViewInfo.Rect);
end;

procedure TdxNavBarNavigationPanePainter.DrawItemPanelPartFocusRect
  (const APartRect: TRect);
begin
  DrawSolidFocusRect(cxRectInflate(APartRect, -1, -0, -1, -1),
    ViewInfo.HeaderFontColor);
end;

procedure TdxNavBarNavigationPanePainter.InternalDrawVerticalText(AFont: TFont;
  const AText: string; const ARect: TRect);
begin
  DrawVerticalText(AFont, AText, ARect, ViewInfo.CollapseBarFontColor,
    raPlus90);
end;

procedure TdxNavBarNavigationPanePainter.DrawOverflowPanelItemBackground
  (ACanvas: TCanvas; AState: TdxNavBarNavPanePartState; const ARect: TRect);
var
  AColor1, AColor2: TColor;
begin
  case AState of
    oisPressed, oisHotCheck, oisDroppedDown:
      begin
        AColor1 := dxOffice11NavPaneGroupCaptionPressedHotColor1;
        AColor2 := dxOffice11NavPaneGroupCaptionPressedHotColor2;
      end;
    oisChecked:
      begin
        AColor1 := dxOffice11NavPaneGroupCaptionPressedColor1;
        AColor2 := dxOffice11NavPaneGroupCaptionPressedColor2;
      end;
    oisHot:
      begin
        AColor1 := dxOffice11NavPaneGroupCaptionHotColor1;
        AColor2 := dxOffice11NavPaneGroupCaptionHotColor2;
      end;
  else { oisNormal }
    AColor1 := clNone;
    AColor2 := clNone;
  end;

  if (AColor1 <> clNone) and (AColor2 <> clNone) then
    BackgroundPainterClass.DrawBackground(ACanvas, ARect, nil, False, clNone,
      AColor1, AColor2, 255, 255, gmVertical, ScaleFactor);
end;

procedure TdxNavBarNavigationPanePainter.DrawOverflowPanelItemBackground
  (ACanvas: TCanvas; const APart: TdxNavBarPart; const ARect: TRect);
begin
  DrawOverflowPanelItemBackground(ACanvas, GetNavPanePartState(APart), ARect);
end;

procedure TdxNavBarNavigationPanePainter.OnDrawOverflowPanelPopupMenuItemHandler
  (ACanvas: TCanvas; ARect: TRect; AImageList: TCustomImageList;
  AImageIndex: Integer; AText: string; AState: TdxNavBarObjectStates);
begin
  FOverflowPanelPainter.DrawPopupMenuItem(OverflowPanelViewInfo, ACanvas, ARect,
    AImageList, AImageIndex, AText, AState);
end;

{ TdxNavBarNavigationPaneButtonPainter }

class procedure TdxNavBarNavigationPaneButtonPainter.InternalDrawButton
  (ACanvas: TCanvas; ARect: TRect; APicture: TPicture; AColor1, AColor2: TColor;
  AAlphaBlend1, AAlphaBlend2: Byte; AGradientMode: TdxBarStyleGradientMode;
  ABorderColor: TColor; AState: TdxNavBarObjectStates;
  AScaleFactor: TdxScaleFactor);
begin
  inherited;
  with TcxCanvas.Create(ACanvas) do
  begin
    FrameRect(ARect, ABorderColor);
    Free;
  end;
end;

{ TdxNavBarOffice11NavPanePainter }

class function TdxNavBarOffice11NavPanePainter.GetGroupViewInfoClass
  : TdxNavBarGroupViewInfoClass;
begin
  Result := TdxNavBarOffice11NavPaneGroupViewInfo;
end;

class function TdxNavBarOffice11NavPanePainter.GetLinkViewInfoClass
  : TdxNavBarLinkViewInfoClass;
begin
  Result := TdxNavBarOffice11NavPaneLinkViewInfo;
end;

class function TdxNavBarOffice11NavPanePainter.GetViewInfoClass
  : TdxNavBarViewInfoClass;
begin
  Result := TdxNavBarOffice11NavPaneViewInfo;
end;

class function TdxNavBarOffice11NavPanePainter.ButtonPainterClass
  : TdxNavBarCustomButtonPainterClass;
begin
  Result := TdxNavBarOffice11NavPaneButtonPainter;
end;

{ TdxNavBarOffice11NavPaneGroupButtonPainter }

class procedure TdxNavBarOffice11NavPaneGroupButtonPainter.InternalDrawButton
  (ACanvas: TCanvas; ARect: TRect; APicture: TPicture; AColor1, AColor2: TColor;
  AAlphaBlend1, AAlphaBlend2: Byte; AGradientMode: TdxBarStyleGradientMode;
  ABorderColor: TColor; AState: TdxNavBarObjectStates;
  AScaleFactor: TdxScaleFactor);
var
  ABackgroundRect: TRect;
begin
  ABackgroundRect := ARect;
  Inc(ABackgroundRect.Top);
  inherited InternalDrawButton(ACanvas, ABackgroundRect, APicture, AColor1,
    AColor2, AAlphaBlend1, AAlphaBlend2, AGradientMode, ABorderColor, AState,
    AScaleFactor);
  ACanvas.Pen.Color := ColorToRGB(ABorderColor);
  ACanvas.Pen.Style := psSolid;
  ACanvas.Pen.Width := 1;
  ACanvas.MoveTo(ARect.Left, ARect.Top);
  ACanvas.LineTo(ARect.Right, ARect.Top);
end;

{ TdxNavBarOffice11SignPainter }

class function TdxNavBarOffice11SignPainter.PrepareBitmap(ACanvas: TCanvas;
  const ASize: TSize; ABitmap: TBitmap; ABackColor1, ABackColor2: TColor;
  AState: TdxNavBarObjectStates): TBitmap;
var
  ABitmapColors: TRGBColors;
  ATransparentColor, APixelColor: TColor;
  AMinValue, AMaxValue, AValue: Byte;
  I, J: Integer;
begin
  Result := CloneBitmap(ABitmap, ASize);
  GetBitmapBits(Result, ABitmapColors, True);

  ATransparentColor := dxRGBQuadToColor(ABitmapColors[0]);

  AMaxValue := 1;
  AMinValue := 255;
  for I := 0 to Result.Width - 1 do
    for J := 0 to Result.Height - 1 do
    begin
      APixelColor := dxRGBQuadToColor(ABitmapColors[J * Result.Width + I]);
      if (APixelColor <> ATransparentColor) and (APixelColor <> 0 { mark } )
      then
      begin
        AValue := GetRValue(APixelColor);
        AMaxValue := Max(AMaxValue, AValue);
        AMinValue := Min(AMinValue, AValue);
      end;
    end;

  for I := 0 to Result.Width - 1 do
    for J := 0 to Result.Height - 1 do
    begin
      APixelColor := dxRGBQuadToColor(ABitmapColors[J * Result.Width + I]);
      if (APixelColor <> ATransparentColor) and (APixelColor <> 0 { mark } )
      then
      begin
        AValue := GetRValue(APixelColor);
        APixelColor := dxGetMiddleRGB(ColorToRGB(ABackColor1),
          ColorToRGB(ABackColor2), MulDiv(AValue - AMinValue, 100,
          AMaxValue - AMinValue));
        ABitmapColors[J * Result.Width + I] := dxColorToRGBQuad(APixelColor);
      end;
    end;

  SetBitmapBits(Result, ABitmapColors, True);
  Result.Transparent := True;
end;

class procedure TdxNavBarOffice11SignPainter.InternalDrawSign(ACanvas: TCanvas;
  ARect: TRect; AScaleFactor: TdxScaleFactor;
  AForeColor, ABackColor1, ABackColor2: TColor; AState: TdxNavBarObjectStates);

  function GetSignBitmap: TBitmap;
  begin
    if sExpanded in AState then
      Result := dxOffice11CaptionCollapseSignBitmap
    else
      Result := dxOffice11CaptionExpandSignBitmap;
  end;

  function GetBitmapSize(ABitmap: TBitmap): TSize;
  begin
    Result := AScaleFactor.Apply(cxSize(ABitmap.Width, ABitmap.Height));
  end;

var
  ABitmap: TBitmap;
begin
  ABitmap := GetSignBitmap;
  ABitmap := PrepareBitmap(ACanvas, GetBitmapSize(ABitmap), ABitmap,
    ABackColor1, ABackColor2, AState);
  try
    ACanvas.Draw(ARect.Left, ARect.Top, ABitmap);
  finally
    ABitmap.Free;
  end;
end;

{ TdxNavBarItemCollectionAccessibilityHelper }

function TdxNavBarItemCollectionAccessibilityHelper.GetChild(AIndex: Integer)
  : TcxAccessibilityHelper;
begin
  if AIndex < inherited GetChildCount then
    Result := inherited GetChild(AIndex)
  else
  begin
    Dec(AIndex, inherited GetChildCount);
    CheckItemIAccessibilityHelperCount;
    Result := ItemIAccessibilityHelpers[AIndex].GetHelper;
  end;
end;

function TdxNavBarItemCollectionAccessibilityHelper.GetChildCount: Integer;
begin
  Result := inherited GetChildCount;
  CheckItemIAccessibilityHelperCount;
  Inc(Result, ItemIAccessibilityHelperCount);
end;

function TdxNavBarItemCollectionAccessibilityHelper.IsContainer: Boolean;
begin
  Result := True;
end;

{ TdxNavBarNavigationPaneOverflowPanelAccessibilityHelper }

function TdxNavBarNavigationPaneOverflowPanelAccessibilityHelper.
  ChildIsSimpleElement(AIndex: Integer): Boolean;
begin
  Result := False;
end;

function TdxNavBarNavigationPaneOverflowPanelAccessibilityHelper.GetChild
  (AIndex: Integer): TcxAccessibilityHelper;
begin
  if AIndex < inherited GetChildCount then
    Result := inherited GetChild(AIndex)
  else
    Result := ViewInfo.SignIAccessibilityHelper.GetHelper;
end;

function TdxNavBarNavigationPaneOverflowPanelAccessibilityHelper.
  GetChildCount: Integer;
begin
  Result := inherited GetChildCount + 1;
end;

function TdxNavBarNavigationPaneOverflowPanelAccessibilityHelper.GetName
  (AChildID: TcxAccessibleSimpleChildElementID): string;
begin
  Result := 'Overflow Panel';
end;

function TdxNavBarNavigationPaneOverflowPanelAccessibilityHelper.GetParent
  : TcxAccessibilityHelper;
begin
  Result := ViewInfo.NavBar.GroupContainerIAccessibilityHelper.GetHelper;
end;

function TdxNavBarNavigationPaneOverflowPanelAccessibilityHelper.GetRole
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := cxROLE_SYSTEM_PANE;
end;

function TdxNavBarNavigationPaneOverflowPanelAccessibilityHelper.GetState
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := Parent.States[cxAccessibleObjectSelfID];
  if not ViewInfo.IsVisible then
    Result := Result or cxSTATE_SYSTEM_INVISIBLE;
end;

function TdxNavBarNavigationPaneOverflowPanelAccessibilityHelper.
  GetSupportedProperties(AChildID: TcxAccessibleSimpleChildElementID)
  : TcxAccessibleObjectProperties;
begin
  Result := [aopLocation, aopFocus];
end;

procedure TdxNavBarNavigationPaneOverflowPanelAccessibilityHelper.
  CheckItemIAccessibilityHelperCount;
begin
  ViewInfo.CheckItemIAccessibilityHelperCount;
end;

function TdxNavBarNavigationPaneOverflowPanelAccessibilityHelper.
  GetBounds: TRect;
begin
  Result := ViewInfo.Rect;
end;

function TdxNavBarNavigationPaneOverflowPanelAccessibilityHelper.
  GetItemCount: Integer;
begin
  Result := ViewInfo.ItemCount;
end;

function TdxNavBarNavigationPaneOverflowPanelAccessibilityHelper.
  GetItemIAccessibilityHelperCount: Integer;
begin
  Result := ViewInfo.ItemIAccessibilityHelperCount;
end;

function TdxNavBarNavigationPaneOverflowPanelAccessibilityHelper.
  GetItemIAccessibilityHelper(AIndex: Integer): IdxNavBarAccessibilityHelper;
begin
  Result := ViewInfo.ItemIAccessibilityHelpers[AIndex];
end;

function TdxNavBarNavigationPaneOverflowPanelAccessibilityHelper.GetViewInfo
  : TdxNavBarOverflowPanelViewInfo;
begin
  Result := TdxNavBarOverflowPanelViewInfo(FOwnerObject);
end;

{ TdxNavBarNavigationPaneOverflowPanelItemAccessibilityHelper }

function TdxNavBarNavigationPaneOverflowPanelItemAccessibilityHelper.
  ChildIsSimpleElement(AIndex: Integer): Boolean;
begin
  Result := True;
end;

procedure TdxNavBarNavigationPaneOverflowPanelItemAccessibilityHelper.
  DoDefaultAction(AChildID: TcxAccessibleSimpleChildElementID);
var
  ANavBar: TdxCustomNavBar;
  I: Integer;
begin
  inherited;
  ANavBar := TdxCustomNavBar(OwnerObjectControl);
  if ANavBar.ViewInfo.GetGroupViewInfoByGroup(Group).IsCaptionVisible then
    TdxCustomNavBarAccess(OwnerObjectControl).DoGroupMouseUp(Group)
  else
    for I := 0 to Group.LinkCount - 1 do
      if Group.Links[I].Item.Visible then
      begin
        TdxCustomNavBarAccess(ANavBar).DoLinkMouseUp(Group.Links[I]);
        Break;
      end;
end;

function TdxNavBarNavigationPaneOverflowPanelItemAccessibilityHelper.GetName
  (AChildID: TcxAccessibleSimpleChildElementID): string;
begin
  Result := Group.Caption;
end;

function TdxNavBarNavigationPaneOverflowPanelItemAccessibilityHelper.GetParent
  : TcxAccessibilityHelper;
begin
  Result := (OwnerObject as TdxNavBarOverflowPanelViewInfo)
    .FIAccessibilityHelper.GetHelper;
end;

function TdxNavBarNavigationPaneOverflowPanelItemAccessibilityHelper.GetRole
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := cxROLE_SYSTEM_PUSHBUTTON;
end;

function TdxNavBarNavigationPaneOverflowPanelItemAccessibilityHelper.GetState
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := Parent.States[cxAccessibleObjectSelfID];
  if ItemIndex >= ViewInfo.OverflowPanelVisibleItemCount then
    Result := Result or cxSTATE_SYSTEM_INVISIBLE
  else
  begin
    Result := Result or cxSTATE_SYSTEM_FOCUSABLE;
    if IsFocused then
      Result := Result or cxSTATE_SYSTEM_FOCUSED;
  end;
end;

function TdxNavBarNavigationPaneOverflowPanelItemAccessibilityHelper.
  GetSupportedProperties(AChildID: TcxAccessibleSimpleChildElementID)
  : TcxAccessibleObjectProperties;
begin
  Result := [aopLocation, aopFocus, aopDefaultAction];
end;

procedure TdxNavBarNavigationPaneOverflowPanelItemAccessibilityHelper.Click
  (AKey: Word);
begin
  inherited Click(AKey);
  TdxCustomNavBarAccess(OwnerObjectControl).DoGroupMouseUp(Group);
end;

function TdxNavBarNavigationPaneOverflowPanelItemAccessibilityHelper.
  GetBounds: TRect;
begin
  Result := ViewInfo.OverflowPanelItems[ItemIndex].SelectionRect;
end;

function TdxNavBarNavigationPaneOverflowPanelItemAccessibilityHelper.IsClickKey
  (AKey: Word): Boolean;
begin
  Result := inherited IsClickKey(AKey) or (AKey in [VK_RETURN, VK_SPACE]);
end;

function TdxNavBarNavigationPaneOverflowPanelItemAccessibilityHelper.
  IsContainer: Boolean;
begin
  Result := False;
end;

function TdxNavBarNavigationPaneOverflowPanelItemAccessibilityHelper.GetGroup
  : TdxNavBarGroup;
begin
  Result := ViewInfo.OverflowPanelItems[ItemIndex].Group;
end;

function TdxNavBarNavigationPaneOverflowPanelItemAccessibilityHelper.GetViewInfo
  : TdxNavBarNavigationPaneViewInfo;
begin
  Result := TdxNavBarOverflowPanelViewInfo(FOwnerObject)
    .ViewInfo as TdxNavBarNavigationPaneViewInfo;
end;

{ TdxNavBarNavigationPaneOverflowPanelSignAccessibilityHelper }

function TdxNavBarNavigationPaneOverflowPanelSignAccessibilityHelper.
  ChildIsSimpleElement(AIndex: Integer): Boolean;
begin
  Result := True;
end;

procedure TdxNavBarNavigationPaneOverflowPanelSignAccessibilityHelper.
  DoDefaultAction(AChildID: TcxAccessibleSimpleChildElementID);
begin
  inherited;
  ViewInfo.DoShowPopupMenu
    (cxRectCenter(GetScreenBounds(cxAccessibleObjectSelfID)));
end;

function TdxNavBarNavigationPaneOverflowPanelSignAccessibilityHelper.GetName
  (AChildID: TcxAccessibleSimpleChildElementID): string;
begin
  Result := cxGetResourceString(@sdxNavigationPaneOverflowPanelCustomizeHint);
end;

function TdxNavBarNavigationPaneOverflowPanelSignAccessibilityHelper.GetParent
  : TcxAccessibilityHelper;
begin
  Result := (OwnerObject as TdxNavBarOverflowPanelViewInfo)
    .FIAccessibilityHelper.GetHelper;
end;

function TdxNavBarNavigationPaneOverflowPanelSignAccessibilityHelper.GetRole
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := cxROLE_SYSTEM_PUSHBUTTON;
end;

function TdxNavBarNavigationPaneOverflowPanelSignAccessibilityHelper.GetState
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := Parent.States[cxAccessibleObjectSelfID] or cxSTATE_SYSTEM_FOCUSABLE;
  if IsFocused then
    Result := Result or cxSTATE_SYSTEM_FOCUSED;
end;

function TdxNavBarNavigationPaneOverflowPanelSignAccessibilityHelper.
  GetSupportedProperties(AChildID: TcxAccessibleSimpleChildElementID)
  : TcxAccessibleObjectProperties;
begin
  Result := [aopLocation, aopFocus, aopDefaultAction];
end;

// IdxNavBarAccessibilityHelper
procedure TdxNavBarNavigationPaneOverflowPanelSignAccessibilityHelper.Click
  (AKey: Word);
begin
  inherited Click(AKey);
  ViewInfo.DoShowPopupMenu
    (cxRectCenter(GetScreenBounds(cxAccessibleObjectSelfID)));
end;

function TdxNavBarNavigationPaneOverflowPanelSignAccessibilityHelper.
  GetBounds: TRect;
begin
  Result := ViewInfo.OverflowPanelSignRect;
end;

function TdxNavBarNavigationPaneOverflowPanelSignAccessibilityHelper.IsClickKey
  (AKey: Word): Boolean;
begin
  Result := inherited IsClickKey(AKey) or (AKey in [VK_RETURN, VK_SPACE]);
end;

function TdxNavBarNavigationPaneOverflowPanelSignAccessibilityHelper.
  IsContainer: Boolean;
begin
  Result := False;
end;

function TdxNavBarNavigationPaneOverflowPanelSignAccessibilityHelper.GetViewInfo
  : TdxNavBarNavigationPaneViewInfo;
begin
  Result := TdxNavBarOverflowPanelViewInfo(FOwnerObject)
    .ViewInfo as TdxNavBarNavigationPaneViewInfo;
end;

{ TdxNavBarNavigationPaneGroupCaptionPanelAccessibilityHelper }

function TdxNavBarNavigationPaneGroupCaptionPanelAccessibilityHelper.GetRole
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  if Group.Parent = nil then
    Result := cxROLE_SYSTEM_PUSHBUTTON
  else
    Result := inherited;
end;

function TdxNavBarNavigationPaneGroupCaptionPanelAccessibilityHelper.GetState
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  if Group.Parent = nil then
  begin
    Result := Parent.States[cxAccessibleObjectSelfID];
    if (Result and cxSTATE_SYSTEM_INVISIBLE = 0) and not GroupViewInfo.IsCaptionVisible
    then
      Result := Result or cxSTATE_SYSTEM_INVISIBLE
    else
    begin
      Result := Result or cxSTATE_SYSTEM_FOCUSABLE;
      if IsFocused then
        Result := Result or cxSTATE_SYSTEM_FOCUSED;
    end;
  end
  else
    Result := inherited;
end;

{ TdxNavBarNavigationPaneActiveGroupCaptionPanelAccessibilityHelper }

function TdxNavBarNavigationPaneActiveGroupCaptionPanelAccessibilityHelper.
  ChildIsSimpleElement(AIndex: Integer): Boolean;
begin
  Result := False;
end;

function TdxNavBarNavigationPaneActiveGroupCaptionPanelAccessibilityHelper.
  GetName(AChildID: TcxAccessibleSimpleChildElementID): string;
begin
  Result := NavBar.ActiveGroup.Caption;
end;

function TdxNavBarNavigationPaneActiveGroupCaptionPanelAccessibilityHelper.
  GetParent: TcxAccessibilityHelper;
begin
  Result := NavBar.GroupContainerIAccessibilityHelper.GetHelper;
end;

function TdxNavBarNavigationPaneActiveGroupCaptionPanelAccessibilityHelper.
  GetRole(AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := cxROLE_SYSTEM_STATICTEXT;
end;

function TdxNavBarNavigationPaneActiveGroupCaptionPanelAccessibilityHelper.
  GetState(AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := Parent.States[cxAccessibleObjectSelfID];
  if (NavBar.ActiveGroup = nil) or not NavBar.ActiveGroup.Visible or
    not NavBar.ShowGroupCaptions or (GroupViewInfo = nil) then
    Result := Result or cxSTATE_SYSTEM_INVISIBLE or cxSTATE_SYSTEM_UNAVAILABLE
  else
  begin
    Result := Result or cxSTATE_SYSTEM_FOCUSABLE;
    if IsFocused then
      Result := Result or cxSTATE_SYSTEM_FOCUSED;
  end;
end;

function TdxNavBarNavigationPaneActiveGroupCaptionPanelAccessibilityHelper.
  GetSupportedProperties(AChildID: TcxAccessibleSimpleChildElementID)
  : TcxAccessibleObjectProperties;
begin
  Result := [aopLocation, aopFocus];
end;

function TdxNavBarNavigationPaneActiveGroupCaptionPanelAccessibilityHelper.
  CanBeFocusedByDefault: Boolean;
begin
  Result := True;
end;

function TdxNavBarNavigationPaneActiveGroupCaptionPanelAccessibilityHelper.
  GetAssociatedObject: TdxNavBarCustomAccessibilityHelper;
begin
  if Visible then
    Result := NavBar.ActiveGroup.CaptionPanelIAccessibilityHelper.
      GetNavBarHelper
  else
    Result := nil;
end;

function TdxNavBarNavigationPaneActiveGroupCaptionPanelAccessibilityHelper.
  GetBounds: TRect;
begin
  Result := GroupViewInfo.Rect;
end;

function TdxNavBarNavigationPaneActiveGroupCaptionPanelAccessibilityHelper.
  IsContainer: Boolean;
begin
  Result := False;
end;

function TdxNavBarNavigationPaneActiveGroupCaptionPanelAccessibilityHelper.
  GetGroupViewInfo: TdxNavBarGroupViewInfo;
var
  I: Integer;
begin
  // Requires
  Assert(NavBar.ActiveGroup <> nil);
  //
  Result := nil;
  for I := NavBarViewInfo.GetRealGroupStartIndex to NavBarViewInfo.
    GroupCount - 1 do
    if NavBar.ViewInfo.Groups[I].Group = NavBar.ActiveGroup then
    begin
      Result := NavBar.ViewInfo.Groups[I];
      Break;
    end;
end;

function TdxNavBarNavigationPaneActiveGroupCaptionPanelAccessibilityHelper.
  GetNavBar: TdxCustomNavBar;
begin
  Result := TdxNavBarOffice11NavPaneViewInfo(FOwnerObject).NavBar;
end;

function TdxNavBarNavigationPaneActiveGroupCaptionPanelAccessibilityHelper.
  GetNavBarViewInfo: TdxNavBarNavigationPaneViewInfo;
begin
  Result := TdxNavBarNavigationPaneViewInfo(NavBar.ViewInfo);
end;

{ TdxNavBarNavigationPaneHeaderPanelViewInfo }

function TdxNavBarNavigationPaneHeaderPanelViewInfo.GetSignHintText: string;
begin
  if (Painter.Controller as TdxNavBarNavigationPaneController).Collapsed then
    Result := cxGetResourceString(@sdxNavigationPaneExpandNavPaneSignHint)
  else
    Result := cxGetResourceString(@sdxNavigationPaneMinimizeNavPaneSignHint);
end;

function TdxNavBarNavigationPaneHeaderPanelViewInfo.GetText: string;
begin
  Result := NavBar.ActiveGroup.Caption;
end;

{ TdxNavBarItemPanelAccessibilityHelper }

function TdxNavBarItemPanelAccessibilityHelper.ChildIsSimpleElement
  (AIndex: Integer): Boolean;
begin
  Result := False;
end;

function TdxNavBarItemPanelAccessibilityHelper.GetChild(AIndex: Integer)
  : TcxAccessibilityHelper;
begin
  if AIndex < inherited GetChildCount then
    Result := inherited GetChild(AIndex)
  else
    Result := ViewInfo.CollapseBarIAccessibilityHelper.GetHelper;
end;

function TdxNavBarItemPanelAccessibilityHelper.GetChildCount: Integer;
begin
  Result := inherited GetChildCount + 1;
end;

function TdxNavBarItemPanelAccessibilityHelper.GetParent
  : TcxAccessibilityHelper;
begin
  Result := ViewInfo.NavBar.GroupContainerIAccessibilityHelper.GetHelper;
end;

function TdxNavBarItemPanelAccessibilityHelper.GetRole
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := cxROLE_SYSTEM_PANE;
end;

function TdxNavBarItemPanelAccessibilityHelper.GetState
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := Parent.States[cxAccessibleObjectSelfID];
  if not ViewInfo.Painter.Controller.Collapsed then
    Result := Result or cxSTATE_SYSTEM_INVISIBLE;
end;

function TdxNavBarItemPanelAccessibilityHelper.GetSupportedProperties
  (AChildID: TcxAccessibleSimpleChildElementID): TcxAccessibleObjectProperties;
begin
  Result := [aopLocation, aopFocus];
end;

procedure TdxNavBarItemPanelAccessibilityHelper.
  CheckItemIAccessibilityHelperCount;
begin
  ViewInfo.CheckItemIAccessibilityHelperCount;
end;

function TdxNavBarItemPanelAccessibilityHelper.GetBounds: TRect;
begin
  Result := ViewInfo.Rect;
end;

function TdxNavBarItemPanelAccessibilityHelper.GetItemCount: Integer;
begin
  Result := ViewInfo.ItemCount;
end;

function TdxNavBarItemPanelAccessibilityHelper.
  GetItemIAccessibilityHelperCount: Integer;
begin
  Result := ViewInfo.ItemIAccessibilityHelperCount;
end;

function TdxNavBarItemPanelAccessibilityHelper.GetItemIAccessibilityHelper
  (AIndex: Integer): IdxNavBarAccessibilityHelper;
begin
  Result := ViewInfo.ItemIAccessibilityHelpers[AIndex];
end;

function TdxNavBarItemPanelAccessibilityHelper.GetViewInfo
  : TdxNavBarItemPanelViewInfo;
begin
  Result := TdxNavBarItemPanelViewInfo(FOwnerObject);
end;

{ TdxNavBarItemPanelCollapseBarAccessibilityHelper }

function TdxNavBarItemPanelCollapseBarAccessibilityHelper.ChildIsSimpleElement
  (AIndex: Integer): Boolean;
begin
  Result := True;
end;

procedure TdxNavBarItemPanelCollapseBarAccessibilityHelper.DoDefaultAction
  (AChildID: TcxAccessibleSimpleChildElementID);
begin
  inherited;
  ViewInfo.Painter.Controller.ShowPopupControl;
end;

function TdxNavBarItemPanelCollapseBarAccessibilityHelper.
  GetDefaultActionDescription(AChildID
  : TcxAccessibleSimpleChildElementID): string;
begin
  Result := 'Expand';
end;

function TdxNavBarItemPanelCollapseBarAccessibilityHelper.GetName
  (AChildID: TcxAccessibleSimpleChildElementID): string;
begin
  Result := TdxCustomNavBar(OwnerObjectControl).ActiveGroup.Caption;
end;

function TdxNavBarItemPanelCollapseBarAccessibilityHelper.GetParent
  : TcxAccessibilityHelper;
begin
  Result := (OwnerObject as TdxNavBarItemPanelViewInfo)
    .IAccessibilityHelper.GetHelper;
end;

function TdxNavBarItemPanelCollapseBarAccessibilityHelper.GetRole
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := cxROLE_SYSTEM_GROUPING;
end;

function TdxNavBarItemPanelCollapseBarAccessibilityHelper.GetState
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := Parent.States[cxAccessibleObjectSelfID] or cxSTATE_SYSTEM_FOCUSABLE;
  if IsFocused then
    Result := Result or cxSTATE_SYSTEM_FOCUSED;
end;

function TdxNavBarItemPanelCollapseBarAccessibilityHelper.GetSupportedProperties
  (AChildID: TcxAccessibleSimpleChildElementID): TcxAccessibleObjectProperties;
begin
  Result := [aopLocation, aopFocus, aopDefaultAction];
end;

function TdxNavBarItemPanelCollapseBarAccessibilityHelper.
  CanBeFocusedByDefault: Boolean;
begin
  Result := True;
end;

procedure TdxNavBarItemPanelCollapseBarAccessibilityHelper.Click(AKey: Word);
begin
  inherited Click(AKey);
  ViewInfo.Painter.Controller.ShowPopupControl;
end;

function TdxNavBarItemPanelCollapseBarAccessibilityHelper.GetBounds: TRect;
begin
  Result := ViewInfo.CollapseBarRect;
end;

function TdxNavBarItemPanelCollapseBarAccessibilityHelper.IsClickKey
  (AKey: Word): Boolean;
begin
  Result := inherited IsClickKey(AKey) or (AKey in [VK_RETURN, VK_SPACE]);
end;

function TdxNavBarItemPanelCollapseBarAccessibilityHelper.IsContainer: Boolean;
begin
  Result := False;
end;

function TdxNavBarItemPanelCollapseBarAccessibilityHelper.GetViewInfo
  : TdxNavBarItemPanelViewInfo;
begin
  Result := TdxNavBarItemPanelViewInfo(OwnerObject);
end;

{ TdxNavBarItemPanelItemAccessibilityHelper }

function TdxNavBarItemPanelItemAccessibilityHelper.ChildIsSimpleElement
  (AIndex: Integer): Boolean;
begin
  Result := True;
end;

procedure TdxNavBarItemPanelItemAccessibilityHelper.DoDefaultAction
  (AChildID: TcxAccessibleSimpleChildElementID);
begin
  inherited;
  TdxNavBarControllerAccess(ItemPanelViewInfo.Painter.Controller)
    .DoLinkClick(ItemPanelViewInfo.NavBar, ItemPanelViewInfo.Items[ItemIndex]
    .ItemLink);
end;

function TdxNavBarItemPanelItemAccessibilityHelper.GetDefaultActionDescription
  (AChildID: TcxAccessibleSimpleChildElementID): string;
begin
  Result := 'Press';
end;

function TdxNavBarItemPanelItemAccessibilityHelper.GetName
  (AChildID: TcxAccessibleSimpleChildElementID): string;
begin
  Result := ItemPanelViewInfo.Items[ItemIndex].Caption;
end;

function TdxNavBarItemPanelItemAccessibilityHelper.GetParent
  : TcxAccessibilityHelper;
begin
  Result := TdxNavBarItemPanelViewInfo(FOwnerObject)
    .IAccessibilityHelper.GetHelper;
end;

function TdxNavBarItemPanelItemAccessibilityHelper.GetRole
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := cxROLE_SYSTEM_LISTITEM;
end;

function TdxNavBarItemPanelItemAccessibilityHelper.GetState
  (AChildID: TcxAccessibleSimpleChildElementID): Integer;
begin
  Result := Parent.States[cxAccessibleObjectSelfID] or cxSTATE_SYSTEM_FOCUSABLE;
  if IsFocused then
    Result := Result or cxSTATE_SYSTEM_FOCUSED;
end;

function TdxNavBarItemPanelItemAccessibilityHelper.GetSupportedProperties
  (AChildID: TcxAccessibleSimpleChildElementID): TcxAccessibleObjectProperties;
begin
  Result := [aopLocation, aopFocus, aopDefaultAction];
end;

function TdxNavBarItemPanelItemAccessibilityHelper.
  CanBeFocusedByDefault: Boolean;
begin
  Result := True;
end;

procedure TdxNavBarItemPanelItemAccessibilityHelper.Click(AKey: Word);
begin
  inherited Click(AKey);
  TdxNavBarControllerAccess(ItemPanelViewInfo.Painter.Controller)
    .DoLinkClick(ItemPanelViewInfo.NavBar, ItemPanelViewInfo.Items[ItemIndex]
    .ItemLink);
end;

function TdxNavBarItemPanelItemAccessibilityHelper.GetBounds: TRect;
begin
  Result := ItemPanelViewInfo.Items[ItemIndex].Rect;
end;

function TdxNavBarItemPanelItemAccessibilityHelper.IsClickKey
  (AKey: Word): Boolean;
begin
  Result := inherited IsClickKey(AKey) or (AKey in [VK_RETURN, VK_SPACE]);
end;

function TdxNavBarItemPanelItemAccessibilityHelper.IsContainer: Boolean;
begin
  Result := False;
end;

function TdxNavBarItemPanelItemAccessibilityHelper.GetItemPanelViewInfo
  : TdxNavBarItemPanelViewInfo;
begin
  Result := TdxNavBarItemPanelViewInfo(FOwnerObject);
end;

{ TdxNavBarPopupControlViewInfo }

procedure TdxNavBarPopupControlViewInfo.CalculateBounds(AClientWidth: Integer);
begin
  FRect := TdxNavBarGroupPopupControlCalculator.CalculateBounds(ViewInfo,
    AClientWidth, GetBorderOffsets);
end;

function TdxNavBarPopupControlViewInfo.CalculatePosition: TPoint;
begin
  Result := TdxNavBarGroupPopupControlCalculator.CalculatePosition
    (ViewInfo, FRect);
end;

function TdxNavBarPopupControlViewInfo.GetClientRect: TRect;
begin
  Result := TdxNavBarGroupPopupControlCalculator.CalculateClientRect(FRect,
    GetBorderOffsets);
end;

function TdxNavBarPopupControlViewInfo.GetMaxHeight: Integer;
begin
  Result := TdxNavBarGroupPopupControlCalculator.CalculateMaxHeight(ViewInfo);
end;

function TdxNavBarPopupControlViewInfo.GetBorderOffsets: TRect;
begin
  Result := cxRect(2, 2, 2, 2);
end;

function TdxNavBarPopupControlViewInfo.GetMinWidth: Integer;
begin
  Result := TdxNavBarViewInfoAccess(ViewInfo).GetNavBarMinExpandedWidth +
    GetBorderOffsets.Left + GetBorderOffsets.Right;
end;

{ TdxNavBarPopupControlViewInfo }

function TdxNavBarPopupControlViewInfo.IsPtSizeGrip(const pt: TPoint): Boolean;
begin
  if Painter.ViewInfo.GetExpandDirection = dirLeft then
    Result := pt.X <= GetBorderOffsets.Left
  else
    Result := (pt.X >= cxRectWidth(FRect) - GetBorderOffsets.Right);
end;

{ TdxNavBarPopupControl }

constructor TdxNavBarPopupControl.Create(ANavBar: TdxCustomNavBar);
begin
  inherited Create(ANavBar);
  FOriginalWidth := ScaleFactor.Apply(GetNavBarOriginalWidth,
    TdxNavBarPainterAccess(Painter).ScaleFactor);
end;

function TdxNavBarPopupControl.CreatePopupViewInfo
  : TdxNavBarCustomPopupControlViewInfo;
begin
  Result := (Painter as TdxNavBarNavigationPanePainter)
    .GetPopupControlViewInfoClass.Create(Master.ViewInfo);
end;

function TdxNavBarPopupControl.GetOriginalWidth: Integer;
begin
  Result := FOriginalWidth;
end;

procedure TdxNavBarPopupControl.BeginResize(AControl: TControl;
  AButton: TMouseButton; AShift: TShiftState; const APoint: TPoint);
var
  ARealPoint: TPoint;
begin
  SetCaptureControl(Self);
  if AControl <> Self then
    ARealPoint := cxClientToParent(AControl, APoint, Self)
  else
    ARealPoint := APoint;
  if (Painter.ViewInfo as TdxNavBarNavigationPaneViewInfo).GetExpandDirection = dirLeft
  then
    FCapturePointOffset := -ARealPoint.X
  else
    FCapturePointOffset := Width - ARealPoint.X;
  Include(FInternalState, pcsSizing);
  DrawSizeFrame(BoundsRect);
end;

procedure TdxNavBarPopupControl.DoCanceled;
begin
  if pcsSizing in FInternalState then
    EndResize(True)
  else
    inherited DoCanceled;
end;

procedure TdxNavBarPopupControl.DoCloseUp;
begin
  inherited DoCloseUp;
  if pcsSizing in FInternalState then
    EndResize(True);
end;

procedure TdxNavBarPopupControl.DoShowed;
begin
  inherited DoShowed;
  if (InnerControl.Groups.Count > 0) and InnerControl.Groups[0].UseControl and
    InnerControl.Groups[0].ShowControl then
  begin
    TdxNavBarPainterAccess(InnerControl.Painter).CheckDrawParamChanges;
    ActiveControl := FindNextControl(nil, True, True, False);
  end;
end;

procedure TdxNavBarPopupControl.InitPopup;
begin
  inherited InitPopup;
  if NeedAdjustWidth then
    FOriginalWidth := GetNavBarOriginalWidth;
end;

procedure TdxNavBarPopupControl.Paint;
begin
  (ViewInfo.Painter as TdxNavBarNavigationPanePainter).DrawPopupControl(Canvas,
    ViewInfo);
end;

procedure TdxNavBarPopupControl.ScaleFactorChanged(M, D: Integer);
begin
  inherited ScaleFactorChanged(M, D);
  FOriginalWidth := MulDiv(FOriginalWidth, M, D);
end;

procedure TdxNavBarPopupControl.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if (GetCaptureControl <> nil) and ViewInfo.IsPtSizeGrip(cxPoint(X, Y)) then
    BeginResize(Self, Button, Shift, cxPoint(X, Y));
end;

procedure TdxNavBarPopupControl.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  ANewRect: TRect;
begin
  if not(pcsSizing in FInternalState) then
    MouseOverSizeGrip := TdxNavBarViewInfoAccess(Master.ViewInfo)
      .IsGroupPopupControlSizable and ViewInfo.IsPtSizeGrip(Point(X, Y))
  else if (Painter.ViewInfo as TdxNavBarNavigationPaneViewInfo)
    .GetExpandDirection = dirLeft then
  begin
    if cxRectWidth(BoundsRect) - X - FCapturePointOffset > ViewInfo.GetMinWidth
    then
    begin
      ANewRect := BoundsRect;
      ANewRect.Left := ANewRect.Left + X + FCapturePointOffset;
      DrawSizeFrame(ANewRect);
    end;
  end
  else if X + FCapturePointOffset > ViewInfo.GetMinWidth then
  begin
    ANewRect := BoundsRect;
    ANewRect.Right := ANewRect.Left + X + FCapturePointOffset;
    DrawSizeFrame(ANewRect);
  end;
end;

procedure TdxNavBarPopupControl.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if pcsSizing in FInternalState then
    EndResize;
end;

function TdxNavBarPopupControl.NeedAdjustWidth: Boolean;
begin
  Result := Master.OptionsBehavior.NavigationPane.AdjustWidthByPopup;
end;

procedure TdxNavBarPopupControl.RefreshPopupWindow;
var
  ASize: TSize;
begin
  ASize := CalculateSize;
  SetBounds(Left, Top, ASize.cx, ASize.cy);
  Refresh;
end;

procedure TdxNavBarPopupControl.DrawSizeFrame(const R: TRect);
var
  ABorderWidth: Integer;
begin
  if not IsRectEmpty(R) then
  begin
    ABorderWidth := 1;
    if FSizeFrame = nil then
      FSizeFrame := TcxSizeFrame.Create(ABorderWidth);
    FSizeFrame.Show;
    FSizeFrame.DrawSizeFrame(R);
  end
  else
    FreeAndNil(FSizeFrame);
end;

procedure TdxNavBarPopupControl.EndResize(ACancel: Boolean = False);

  procedure UpdateBounds(ALeft, ATop, AWidth, AHeight: Integer);
  begin
    FOriginalWidth := FOriginalWidth + AWidth - Width;
    if NeedAdjustWidth then
      TdxNavBarControllerAccess(Painter.Controller).OriginalWidth :=
        FOriginalWidth;
    SetBounds(ALeft, ATop, AWidth, AHeight);
    RefreshPopupWindow;
  end;

var
  ANewBounds: TRect;
begin
  SetCaptureControl(nil);
  Exclude(FInternalState, pcsSizing);
  if not ACancel then
  begin
    ANewBounds := FSizeFrame.BoundsRect;
    UpdateBounds(ANewBounds.Left, ANewBounds.Top, cxRectWidth(ANewBounds),
      cxRectHeight(ANewBounds));
  end;
  DrawSizeFrame(cxEmptyRect);
end;

function TdxNavBarPopupControl.GetNavBarOriginalWidth: Integer;
begin
  Result := TdxNavBarControllerAccess(Painter.Controller).OriginalWidth;
end;

function TdxNavBarPopupControl.GetMouseOverSizeGrip: Boolean;
begin
  Result := pcsOverSizeGrip in FInternalState;
end;

function TdxNavBarPopupControl.GetPainter: TdxNavBarNavigationPanePainter;
begin
  Result := inherited Painter as TdxNavBarNavigationPanePainter;
end;

function TdxNavBarPopupControl.GetViewInfo: TdxNavBarPopupControlViewInfo;
begin
  Result := PopupViewInfo as TdxNavBarPopupControlViewInfo;
end;

procedure TdxNavBarPopupControl.SetMouseOverSizeGrip(const AValue: Boolean);
begin
  if MouseOverSizeGrip <> AValue then
    if AValue then
    begin
      Include(FInternalState, pcsOverSizeGrip);
      Cursor := crSizeWE;
    end
    else
    begin
      Exclude(FInternalState, pcsOverSizeGrip);
      Cursor := crDefault;
    end;
end;

initialization

RegisterView(dxNavBarOffice11TaskPaneView, 'Office11TaskPaneView',
  TdxNavBarOffice11Painter);
RegisterView(dxNavBarOffice11ExplorerBarView, 'Office11ExplorerBarView',
  TdxNavBarOffice11ExplorerBarPainter);
RegisterView(dxNavBarOffice11NavigatorPaneView, 'Office11NavigationPaneView',
  TdxNavBarOffice11NavPanePainter);

finalization

UnRegisterView(dxNavBarOffice11TaskPaneView);
UnRegisterView(dxNavBarOffice11ExplorerBarView);
UnRegisterView(dxNavBarOffice11NavigatorPaneView);

end.
