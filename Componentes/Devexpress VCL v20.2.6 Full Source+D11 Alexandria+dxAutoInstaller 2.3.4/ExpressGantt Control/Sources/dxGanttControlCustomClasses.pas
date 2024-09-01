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

unit dxGanttControlCustomClasses;

{$I cxVer.inc}
{$I dxGanttControl.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes, System.Contnrs,
{$ENDIF}
  Types, SysUtils, Windows, Messages, MultiMon, Controls, Graphics, Forms,
  Classes, Generics.Defaults, Generics.Collections, StdCtrls,
  dxCore, dxCoreClasses, cxGraphics, cxCustomCanvas, cxGeometry,
  cxLookAndFeels, cxLookAndFeelPainters, cxClasses, cxControls,
  dxDirectX.D2D.Classes, dxSVGImage, dxTouch, dxMessages;

type
  TdxGanttControlCustomItemViewInfo = class;
  TdxGanttControlHistory = class;
  TdxGanttControlBase = class;
  TdxGanttControlDragHelper = class;
  TdxGanttControlCustomController = class;
  TdxGanttControlHitTest = class;
  TdxGanttControlCustomScrollBars = class;

  { TdxGanttControlCanvasCustomCache }

  TdxGanttControlCanvasCustomCache = class
  strict private
  type
    TImageCacheKey = record
      Image: TGraphic;
      Size: TSize;
    end;

    TImageCache = TObjectDictionary<TImageCacheKey, TcxCanvasBasedImage>;
  strict private
    FFonts: TObjectDictionary<HFont, TcxCanvasBasedFont>;
    FImages: TImageCache;
    FBaseBoldFont: TFont;
    FBaseItalicFont: TFont;
  protected
    function GetCanvas: TcxCustomCanvas; virtual; abstract;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    function GetImage(AImage: TGraphic): TcxCanvasBasedImage;
    function GetBaseFont: TFont; virtual; abstract;
    function GetBaseBoldFont: TFont; virtual;
    function GetBaseItalicFont: TFont; virtual;

    function GetControlFont: TcxCanvasBasedFont;
    function GetFont(AFont: TFont): TcxCanvasBasedFont;

    property Canvas: TcxCustomCanvas read GetCanvas;
  end;

  { TdxGanttControlCustomItemViewInfo }

  TdxGanttControlCustomItemViewInfo = class abstract
  strict private
    FBounds: TRect;
    FState: TcxButtonState;
    function GetCanvas: TcxCustomCanvas; inline;
    procedure SetState(const Value: TcxButtonState);
  protected
    function CalculateSize: TSize; virtual;
    procedure DoDraw; virtual; abstract;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); virtual;
    function GetCanvasCache: TdxGanttControlCanvasCustomCache; virtual;
      abstract;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
      virtual; abstract;
    function GetScaleFactor: TdxScaleFactor; virtual; abstract;
    function GetUseRightToLeftAlignment: Boolean; virtual; abstract;
    procedure PrepareCanvas(ACanvas: TcxCustomCanvas); virtual;
    function GetResizeHitZoneWidth: Integer;

    procedure DoScroll(const DX, DY: Integer); virtual;
    procedure Scroll(const DX, DY: Integer);

    procedure Invalidate; overload;
    procedure Invalidate(const R: TRect); overload; virtual;

    function HasHotTrackState: Boolean; virtual;
    function HasPressedState: Boolean; virtual;
    function GetCurrentCursor(const P: TPoint; const ADefaultCursor: TCursor)
      : TCursor; virtual;
    function IsPressed: Boolean; virtual;
    procedure MouseEnter;
    procedure MouseLeave;
    procedure UpdateState;

    procedure Reset; virtual;

    function GetHintText: string; virtual;
    function HasHint: Boolean; virtual;

    function CalculateHitTest(const AHitTest: TdxGanttControlHitTest)
      : Boolean; virtual;
    function GetClipBounds: TRect; virtual;
    procedure SetBounds(ABounds: TRect);
  public
    procedure AfterConstruction; override;
    procedure Calculate(const R: TRect); virtual;
    procedure Draw;
    procedure CalculateLayout; virtual;
    procedure Recalculate;
    procedure ViewChanged; virtual;

    property Bounds: TRect read FBounds;
    property Canvas: TcxCustomCanvas read GetCanvas;
    property CanvasCache: TdxGanttControlCanvasCustomCache read GetCanvasCache;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter
      read GetLookAndFeelPainter;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property State: TcxButtonState read FState;
    property UseRightToLeftAlignment: Boolean read GetUseRightToLeftAlignment;
  end;

  { TdxGanttControlCustomOwnedItemViewInfo }

  TdxGanttControlCustomOwnedItemViewInfo = class abstract
    (TdxGanttControlCustomItemViewInfo)
  strict private
    FCanvasCache: TdxGanttControlCanvasCustomCache;
    FLookAndFeelPainter: TcxCustomLookAndFeelPainter;
    FScaleFactor: TdxScaleFactor;
    FUseRightToLeftAlignment: Boolean;
    FOwner: TdxGanttControlCustomItemViewInfo;
  protected
    function GetCanvasCache: TdxGanttControlCanvasCustomCache; override;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter; override;
    function GetScaleFactor: TdxScaleFactor; override;
    function GetUseRightToLeftAlignment: Boolean; override;
    procedure Invalidate(const R: TRect); override;
    procedure Reset; override;
    procedure UpdateCachedValues;
  public
    constructor Create(AOwner: TdxGanttControlCustomItemViewInfo); virtual;

    property Owner: TdxGanttControlCustomItemViewInfo read FOwner;
  end;

  { TdxGanttControlCustomParentViewInfo }

  TdxGanttControlCustomParentViewInfo = class abstract
    (TdxGanttControlCustomOwnedItemViewInfo)
  strict private
    FViewInfos: TdxFastObjectList;
    function GetViewInfo(Index: Integer)
      : TdxGanttControlCustomItemViewInfo; inline;
    function GetViewInfoCount: Integer; inline;
  protected
    procedure DoDraw; override;
    procedure DoScroll(const DX, DY: Integer); override;

    function CalculateItemBounds(AItem: TdxGanttControlCustomItemViewInfo)
      : TRect; virtual;
    procedure Clear; virtual;
    procedure AddChild(AChild: TdxGanttControlCustomItemViewInfo); virtual;

    function CalculateHitTest(const AHitTest: TdxGanttControlHitTest)
      : Boolean; override;
    procedure Reset; override;

    property ViewInfos[Index: Integer]: TdxGanttControlCustomItemViewInfo
      read GetViewInfo;
    property ViewInfoCount: Integer read GetViewInfoCount;
  public
    constructor Create(AOwner: TdxGanttControlCustomItemViewInfo); override;
    destructor Destroy; override;

    procedure Calculate(const R: TRect); override;
    procedure CalculateLayout; override;
    procedure ViewChanged; override;
  end;

  { TdxGanttControlPersistent }

  TdxGanttControlPersistent = class abstract(TInterfacedPersistent)
  strict private
    FLockCount: Integer;
    FAfterResetHandlers: TdxNotifyEventHandler;
    FBeforeResetHandlers: TdxNotifyEventHandler;
  protected
    procedure Changed; overload;
    procedure DoChanged; overload; virtual; abstract;
    procedure DoReset; virtual; abstract;
    function IsUpdateLocked: Boolean; virtual;
  public
    procedure AfterConstruction; override;
    procedure Assign(Source: TPersistent); override;

    procedure Reset;

    procedure BeginUpdate;
    procedure CancelUpdate;
    procedure EndUpdate;

{$REGION 'for internal use'}
    property AfterResetHandlers: TdxNotifyEventHandler read FAfterResetHandlers;
    property BeforeResetHandlers: TdxNotifyEventHandler
      read FBeforeResetHandlers;
{$ENDREGION 'for internal use'}
  end;

  { TdxGanttControlCustomOptions }

  TdxGanttControlOptionsChangedType = (Layout, Cache, Size, View);
  TdxGanttControlOptionsChangedTypes = set of TdxGanttControlOptionsChangedType;

  TdxGanttControlCustomOptions = class abstract(TdxGanttControlPersistent)
  protected type
{$REGION 'protected type'}
    TChangedEvent = procedure(Sender: TObject;
      AChanges: TdxGanttControlOptionsChangedTypes) of object;
    TChangedHandlers = TdxMulticastMethod<TChangedEvent>;
{$ENDREGION}
  strict private
    FChanges: TdxGanttControlOptionsChangedTypes;
    FOwner: TPersistent;

    FChangedHandlers: TChangedHandlers;
  protected
    function GetOwner: TPersistent; override;

    procedure Changed(AChanges: TdxGanttControlOptionsChangedTypes); overload;
    procedure DoChanged; overload; override; final;
    procedure DoChanged(AChanges: TdxGanttControlOptionsChangedTypes);
      overload; virtual;
    function GetScaleFactor: TdxScaleFactor; virtual; abstract;
  public
    constructor Create(AOwner: TPersistent); virtual;

    property Owner: TPersistent read FOwner;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property ChangedHandlers: TChangedHandlers read FChangedHandlers;
  end;

  { TdxGanttControlHitTest }

  TdxGanttControlHitTest = class
  strict private
    FController: TdxGanttControlCustomController;
    FHitPoint: TPoint;
    FHitObject: TdxGanttControlCustomItemViewInfo;
  protected
    procedure Clear;
    procedure SetHitObject(const AHitObject: TdxGanttControlCustomItemViewInfo);
    procedure SetHitPoint(const Value: TPoint);

    property Controller: TdxGanttControlCustomController read FController;
  public
    constructor Create(AController: TdxGanttControlCustomController);

    procedure Calculate(const X, Y: Integer);
    procedure Recalculate;

    property HitObject: TdxGanttControlCustomItemViewInfo read FHitObject;
    property HitPoint: TPoint read FHitPoint;
  end;

  { TdxGanttControlDragAndDropObject }

  TdxGanttControlDragAndDropObject = class abstract(TcxDragAndDropObject)
  strict private
    FController: TdxGanttControlCustomController;
    FDragImage: TcxDragImage;
    function GetControl: TdxGanttControlBase; inline;
    function GetHitTest: TdxGanttControlHitTest; inline;
  protected
    procedure BeginDragAndDrop; override;
    procedure EndDragAndDrop(Accepted: Boolean); override;

    procedure AfterScrolling(AScrollDirection: TcxDirection); virtual;
    procedure ApplyChanges(const P: TPoint); virtual; abstract;
    procedure BeforeScrolling; virtual;
    function CanDrop(const P: TPoint): Boolean; virtual;
    function CreateDragImage: TcxDragImage; virtual;
    procedure HideDragImage;
    procedure ShowDragImage(const P: TPoint);

    property DragImage: TcxDragImage read FDragImage;
  public
    constructor Create(AController: TdxGanttControlCustomController);
      reintroduce; virtual;

    property Control: TdxGanttControlBase read GetControl;
    property Controller: TdxGanttControlCustomController read FController;
    property HitTest: TdxGanttControlHitTest read GetHitTest;
  end;

  TdxGanttControlDragAndDropObjectClass = class of
    TdxGanttControlDragAndDropObject;

  { TdxGanttControlResizingObject }

  TdxGanttControlResizingObject = class abstract
    (TdxGanttControlDragAndDropObject)
  protected
    function CreateDragImage: TcxDragImage; override; final;

    function GetDragImageHeight: Integer; virtual; abstract;
    function GetDragImageWidth: Integer; virtual; abstract;
  end;

  { TdxGanttControlMovingObject }

  TdxGanttControlMovingObject = class abstract(TdxGanttControlDragAndDropObject)
  protected
    procedure CheckScrolling(const P: TPoint);
  end;

  { TdxGanttControlDragObject }

  TdxGanttControlDragObject = class(TcxDragControlObject);
  TdxGanttControlDragObjectClass = class of TdxGanttControlDragObject;

  { TdxGanttControlDragHelper }

  TdxGanttControlDragHelper = class abstract // for internal use
  private const
    ScrollHotZoneWidth = 15;
    ScrollTimeInterval = 35;
  strict private
    FController: TdxGanttControlCustomController;
    FScrollDirection: TcxDirection;
    FScrollTimer: TcxTimer;
    procedure ScrollTimerHandler(Sender: TObject);

    function GetDragAndDropObject: TcxDragAndDropObject; inline;
    function GetHitTest: TdxGanttControlHitTest; inline;
    procedure SetScrollDirection(const Value: TcxDirection);
  protected
    // drag'n'drop
    procedure BeginDragAndDrop; virtual;
    function CanDrag(X, Y: Integer): Boolean; virtual;
    function CreateDragAndDropObject: TdxGanttControlDragAndDropObject;
      virtual; abstract;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); virtual;
    procedure DragDrop(Source: TObject; X, Y: Integer); virtual;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); virtual;
    procedure EndDrag(Target: TObject; X, Y: Integer); virtual;
    procedure EndDragAndDrop(Accepted: Boolean); virtual;
    function GetDragObjectClass: TdxGanttControlDragObjectClass; virtual;
    procedure StartDrag(var DragObject: TDragObject); virtual;
    function StartDragAndDrop(const P: TPoint): Boolean; virtual; abstract;

    procedure AfterScrolling(AScrollDirection: TcxDirection); virtual;
    procedure BeforeScrolling; virtual;
    function CanScroll: Boolean; virtual;
    procedure CheckScrolling(const P: TPoint);
    procedure DoScroll; virtual;
    function GetScrollableArea: TRect; virtual;

    property Controller: TdxGanttControlCustomController read FController;
  public
    constructor Create(AController: TdxGanttControlCustomController);
    destructor Destroy; override;

    function IsDragging: Boolean;

    property DragAndDropObject: TcxDragAndDropObject read GetDragAndDropObject;
    property HitTest: TdxGanttControlHitTest read GetHitTest;
    property ScrollDirection: TcxDirection read FScrollDirection
      write SetScrollDirection;
  end;

  { TdxGanttControlMainScrollbarsHelper }

  TdxGanttControlMainScrollbarsHelper = class(TdxMainScrollbarsOwnerHelper)
  // for internal use
  strict private
    function GetOwner: TdxGanttControlCustomScrollBars;
  protected
    function CanScrollContentByGestureWithoutScrollBars: Boolean; override;
    function GetControl: TcxControl; override;
    function GetHScrollBarBounds: TRect; override;
    function GetScrollbarBasedGestureClientSize: TSize; override;
    function GetScrollBarLookAndFeel: TcxLookAndFeel; override;
    function GetScrollContentForegroundColor: TColor; override;
    function GetVScrollBarBounds: TRect; override;
    function HasScrollBarArea: Boolean; override;
    procedure InitScrollBarsParameters; override;
    function IsDataScrollbar(AKind: TScrollBarKind): Boolean; override;
    procedure OwnerBoundsChanged; override;
    procedure OwnerCheckOverpan(AScrollKind: TScrollBarKind;
      ANewDataPos, AMinDataPos, AMaxDataPos: Integer;
      ADeltaX, ADeltaY: Integer); override;
    procedure OwnerGestureScroll(AScrollKind: TScrollBarKind;
      ANewScrollPos: Integer); override;
    procedure OwnerUpdate; override;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode;
      var AScrollPos: Integer); override;
    procedure ScrollBarVisibilityChanged(AScrollBars: TScrollBarKinds);
      override;

    property Owner: TdxGanttControlCustomScrollBars read GetOwner;
  end;

  { TdxGanttControlCustomScrollBars }

  TdxGanttControlCustomScrollBars = class abstract(TcxIUnknownObject,
    IdxGestureClient, IdxTouchScrollUIOwner, IdxHybridScrollbarOwner)
  // for internal use
  strict private
    FController: TdxGanttControlCustomController;
    FHelper: TdxGanttControlMainScrollbarsHelper;
  private
    function GetHScrollBar: IcxControlScrollBar;
    function GetVScrollBar: IcxControlScrollBar;
  protected
{$REGION 'IdxGestureClient'}
    function AllowGesture(AGestureId: Integer): Boolean;
    function AllowPan(AScrollKind: TScrollBarKind): Boolean;
    procedure BeginGestureScroll(APos: TPoint);
    procedure EndGestureScroll;
    procedure GestureScroll(ADeltaX, ADeltaY: Integer);
    function GetPanOptions: Integer;
    function IsPanArea(const APoint: TPoint): Boolean;
    function NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean; virtual;
{$ENDREGION}
{$REGION 'IdxTouchScrollUIOwner'}
    procedure CheckUIPosition; virtual;
    function GetOwnerControl: TcxControl; virtual;
    function HasVisibleUI: Boolean; virtual;
    procedure HideUI; virtual;
{$ENDREGION}
{$REGION 'IdxHybridScrollbarOwner'}
    function GetBaseColor: TColor;
    function GetManager: TdxHybridScrollbarsManager; virtual;
    procedure IdxHybridScrollbarOwner.Invalidate = InvalidateScrollBars;
    procedure InvalidateScrollBars; virtual;
{$ENDREGION}
    function GetMouseWheelActiveScrollBar(Shift: TShiftState)
      : IcxControlScrollBar; virtual;
    function DoMouseWheel(Shift: TShiftState; AIsIncrement: Boolean): Boolean;

    procedure DoHScroll(ScrollCode: TScrollCode; var ScrollPos: Integer);
      virtual; abstract;
    procedure DoVScroll(ScrollCode: TScrollCode; var ScrollPos: Integer);
      virtual; abstract;
    procedure SetScrollInfo(AScrollBarKind: TScrollBarKind;
      AMin, AMax, AStep, APage, APos: Integer; AAllowShow, AAllowHide: Boolean);

    function GetHScrollBarAreaHeight: Integer;
    function GetScrollbarMode: TdxScrollbarMode;
    function GetVScrollBarAreaWidth: Integer;

    procedure CalculateScrollBars;
    procedure DoCreateScrollBars;
    procedure DoDestroyScrollBars;
    procedure DrawSizeGrip(ACanvas: TcxCustomCanvas);
    function GetClientRect(const R: TRect): TRect;
    procedure HideTouchScrollUI(AImmediately: Boolean = False);
    procedure InitScrollBars; virtual;
    procedure InitScrollBarsParameters;
    function IsUnlimitedScrolling(AScrollKind: TScrollBarKind;
      ADeltaX, ADeltaY: Integer): Boolean; virtual;
    procedure DoInitHScrollBarParameters; virtual;
    procedure DoInitVScrollBarParameters; virtual;
    procedure ShowTouchScrollUI;
    procedure UnInitScrollbars; virtual;

    function GetHScrollBarBounds: TRect;
    function GetVScrollBarBounds: TRect;

    function CanScrolling(AKind: TScrollBarKind): Boolean; virtual;
    procedure DoScrollContent(ADirection: TcxDirection);
    procedure ProcessControlScrollingOnMiddleButton;

    function NCSizeChanged: Boolean;

    property Controller: TdxGanttControlCustomController read FController;
    property HScrollBar: IcxControlScrollBar read GetHScrollBar;
    property Helper: TdxGanttControlMainScrollbarsHelper read FHelper;
    property VScrollBar: IcxControlScrollBar read GetVScrollBar;
  public
    constructor Create(AController: TdxGanttControlCustomController); virtual;
    destructor Destroy; override;
  end;

  { TdxGanttControlCustomController }

  TdxGanttControlCustomController = class(TcxIUnknownObject)
  strict private
    FControl: TdxGanttControlBase;
    FDragHelper: TdxGanttControlDragHelper;
    FHitTest: TdxGanttControlHitTest;
    function GetHitTest: TdxGanttControlHitTest; inline;
    function GetScaleFactor: TdxScaleFactor;
  protected
    function CreateHitTest: TdxGanttControlHitTest; virtual;
    function CreateDragHelper: TdxGanttControlDragHelper; virtual;
    function GetDragHelper: TdxGanttControlDragHelper; virtual;
    function GetGestureClient(const APoint: TPoint): IdxGestureClient; virtual;
    function GetViewInfo: TdxGanttControlCustomItemViewInfo; virtual;
    function IsPanArea(const APoint: TPoint): Boolean; virtual;
    function ProcessNCSizeChanged: Boolean; virtual;

    // scroll
    procedure DoCreateScrollBars; virtual;
    procedure DoDestroyScrollBars; virtual;
    function GetTouchScrollUIOwner(const APoint: TPoint)
      : IdxTouchScrollUIOwner; virtual;
    procedure InitScrollBars; virtual;
    procedure UnInitScrollbars; virtual;
    procedure UpdateHitTest(X, Y: Integer); virtual;

    function IsMouseWheelHandleNeeded(const MousePos: TPoint): Boolean; virtual;
    procedure DoClick; virtual;
    procedure DoDblClick; virtual;
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); virtual;
    procedure DoMouseMove(Shift: TShiftState; X: Integer; Y: Integer); virtual;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); virtual;
    function DoMouseWheel(Shift: TShiftState; AIsIncrement: Boolean;
      const AMousePos: TPoint): Boolean; virtual;

    procedure DoKeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure DoKeyPress(var Key: Char); virtual;
    procedure DoKeyUp(var Key: Word; Shift: TShiftState); virtual;

    procedure CheckHint(Shift: TShiftState);

    procedure Activated; virtual;
    procedure Deactivated; virtual;
    procedure Changed;
    procedure HideEditing; virtual;

    // drag'n'drop
    procedure BeginDragAndDrop;
    function CanAutoScroll(ADirection: TcxDirection): Boolean; virtual;
    function CanDrag(X, Y: Integer): Boolean;
    function CreateDragAndDropObject: TdxGanttControlDragAndDropObject;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean);
    procedure DragDrop(Source: TObject; X, Y: Integer);
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean);
    procedure EndDrag(Target: TObject; X, Y: Integer);
    procedure EndDragAndDrop(Accepted: Boolean);
    function GetDesignHitTest(X, Y: Integer; Shift: TShiftState)
      : Boolean; virtual;
    function GetDragObjectClass: TdxGanttControlDragObjectClass;
    procedure StartDrag(var DragObject: TDragObject);
    function StartDragAndDrop(const P: TPoint): Boolean;

    property DragHelper: TdxGanttControlDragHelper read GetDragHelper;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property ViewInfo: TdxGanttControlCustomItemViewInfo read GetViewInfo;
  public
    constructor Create(AControl: TdxGanttControlBase); virtual;
    destructor Destroy; override;

{$REGION 'for internal use'}
    procedure Click;
    procedure DblClick;
    function GetCurrentCursor(X: Integer; Y: Integer): TCursor;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer);
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer);
    procedure MouseLeave;
    function MouseWheel(Shift: TShiftState; AIsIncrement: Boolean;
      const AMousePos: TPoint): Boolean;

    procedure KeyDown(var Key: Word; Shift: TShiftState);
    procedure KeyPress(var Key: Char);
    procedure KeyUp(var Key: Word; Shift: TShiftState);
{$ENDREGION}
    property Control: TdxGanttControlBase read FControl;
    property HitTest: TdxGanttControlHitTest read GetHitTest;
  end;

  { TdxGanttControlCustomViewInfo }

  TdxGanttControlCustomViewInfo = class abstract
    (TdxGanttControlCustomParentViewInfo)
  end;

  { TdxGanttControlHistoryItem }

  TdxGanttControlHistoryItem = class
  strict private
    FOwner: TdxGanttControlHistory;
  protected
    procedure DoRedo; virtual;
    procedure DoUndo; virtual;

    property Owner: TdxGanttControlHistory read FOwner;
  public
    constructor Create(AOwner: TdxGanttControlHistory); virtual;

    procedure Execute;
    procedure Redo;
    procedure Undo;
  end;

  { TdxGanttControlCompositeHistoryItem }

  TdxGanttControlCompositeHistoryItem = class(TdxGanttControlHistoryItem)
  strict private
    FItems: TObjectList<TdxGanttControlHistoryItem>;
  protected
    procedure DoRedo; override;
    procedure DoUndo; override;
  public
    constructor Create(AHistory: TdxGanttControlHistory); override;
    destructor Destroy; override;

    procedure AddItem(AItem: TdxGanttControlHistoryItem);
    function IsEmpty: Boolean;
  end;

  { TdxGanttControlHistory }

  TdxGanttControlHistory = class
  public const
    Capacity: Integer = 100;
  strict private
    FControl: TdxGanttControlBase;
    FCurrentIndex: Integer;
    FItems: TObjectList<TdxGanttControlHistoryItem>;
    FTransaction: TdxGanttControlCompositeHistoryItem;
    FTransactionCount: Integer;
    function GetCount: Integer;
  protected
    procedure CheckCapacity;
    property Control: TdxGanttControlBase read FControl;
    property Transaction: TdxGanttControlCompositeHistoryItem read FTransaction;
  public
    constructor Create(AControl: TdxGanttControlBase);
    destructor Destroy; override;

    procedure AddItem(AItem: TdxGanttControlHistoryItem);
    function CanRedo: Boolean;
    function CanUndo: Boolean;
    procedure Clear;
    procedure Undo;
    procedure Redo;

    procedure BeginTransaction;
    procedure CancelTransaction;
    procedure EndTransaction;
    property Count: Integer read GetCount;
  end;

  { TdxGanttControlBehaviorOptions }

  TdxGanttControlBehaviorOptions = class(TdxGanttControlPersistent)
  private
    FAlwaysShowEditor: Boolean;
    FConfirmDelete: Boolean;
    FGanttControl: TdxGanttControlBase;
    FShowHints: Boolean;
  protected
    procedure DoChanged; override;
    procedure DoReset; override;
  public
    constructor Create(AOwner: TdxGanttControlBase); virtual;
    procedure Assign(Source: TPersistent); override;

    property GanttControl: TdxGanttControlBase read FGanttControl;
  published
    property AlwaysShowEditor: Boolean read FAlwaysShowEditor
      write FAlwaysShowEditor default False;
    property ConfirmDelete: Boolean read FConfirmDelete write FConfirmDelete
      default False;
    property ShowHints: Boolean read FShowHints write FShowHints default True;
  end;

  { TdxGanttControlHintWindow }

  TdxGanttControlHintWindow = class(THintWindow) // for internal use
  strict private
    function InternalGetOwner: TdxGanttControlBase; inline;
    function GetDrawTextFlags: Integer;
    function GetPainter: TcxCustomLookAndFeelPainter;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure NCPaint(DC: HDC); override;
    procedure Paint; override;
  public
    function CalcHintRect(AMaxWidth: Integer; const AHint: string;
      AData: Pointer): TRect; override;
    property Owner: TdxGanttControlBase read InternalGetOwner;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
  end;

  { TdxGanttControlHintController }

  TdxGanttControlHintController = class abstract(TcxIUnknownObject, IUnknown,
    IcxMouseTrackingCaller) // for internal use
  public const
    MaxWidth: Integer = 250;
  protected
    FAutoHide: Boolean;
    FHintWindow: THintWindow;
    FOwner: TdxGanttControlBase;
    FHideBeforeShow: Boolean;
    FHintFlags: Integer;
    FHintRect: TRect;
    FHintText: string;
    FLockHint: Boolean;
    FShowing: Boolean;
    FTimer: TcxTimer;
    FViewInfo: TdxGanttControlCustomItemViewInfo;
    // IcxMouseTrackingCaller
    procedure MouseLeave;
    // methods
    function CanShowHint: Boolean; virtual;
    procedure DoActivate(const AHintRect: TRect; const AHintText: string;
      AImmediateHint: Boolean = False; AAutoHide: Boolean = True); virtual;
    procedure HideHint; virtual;
    procedure ShowHint; virtual;
    procedure StartHideHintTimer;
    procedure StartShowHintTimer;
    procedure StopTimer;
    procedure TimerHandler(Sender: TObject);
  public
    constructor Create(AOwner: TdxGanttControlBase); virtual;
    destructor Destroy; override;
    procedure Activate(AHitViewInfo: TdxGanttControlCustomItemViewInfo);
    function CalcHintRect(AMaxWidth: Integer; const AHintText: string;
      AFlags: Integer): TRect;
    procedure Hide;
    procedure Reset;

    property GanttControl: TdxGanttControlBase read FOwner;
    property LockHint: Boolean read FLockHint write FLockHint;
    property Showing: Boolean read FShowing;
  end;

  { TdxGanttControlBase }

  TdxGanttControlChangedType = (Data, Layout, Cache, Size, View);
  TdxGanttControlChangedTypes = set of TdxGanttControlChangedType;

  TdxGanttControlBase = class abstract(TcxControl, IdxSkinSupport,
    IdxDirect2DSupport)
  strict private
    FController: TdxGanttControlCustomController;
    FHistory: TdxGanttControlHistory;
    FHintController: TdxGanttControlHintController;
    FOptionsBehavior: TdxGanttControlBehaviorOptions;
    FViewInfo: TdxGanttControlCustomViewInfo;

    FChanges: TdxGanttControlChangedTypes;
    FLockCount: Integer;

    procedure SetOptionsBehavior(const Value: TdxGanttControlBehaviorOptions);
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMNCSizeChanged(var Message: TMessage); message DXM_NCSIZECHANGED;
  protected
    procedure CreateActualCanvas; override;
    procedure FreeActualCanvas; override;

    procedure CreateSubClasses; virtual;
    procedure DestroySubClasses; virtual;

    procedure BiDiModeChanged; override;
    procedure BoundsChanged; override;
    procedure DoPaint; override;
    procedure FontChanged; override;
    procedure Loaded; override;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel;
      AChangedValues: TcxLookAndFeelValues); override;

    // IdxGestureOwner
    function GetGestureClient(const APoint: TPoint): IdxGestureClient; override;

    // scroll
    function AllowTouchScrollUIMode: Boolean; override;
    procedure DoCreateScrollBars; override;
    procedure DoDestroyScrollBars; override;
    function GetMainScrollBarsClass: TcxControlCustomScrollBarsClass; override;
    function GetTouchScrollUIOwner(const APoint: TPoint)
      : IdxTouchScrollUIOwner; override;
    function HasScrollBars: Boolean; override;
    function InternalMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure InitScrollBars; override;
    function IsMouseWheelHandleNeeded(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    function NeedsScrollBars: Boolean; override;
    procedure UnInitScrollbars; override;

    // drag'n'drop support
    function CanDrag(X, Y: Integer): Boolean; override;
    function CreateDragAndDropObject: TcxDragAndDropObject; override;
    procedure DoEndDrag(Target: TObject; X, Y: Integer); override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure DragOver(Source: TObject; X, Y: Integer; State: TDragState;
      var Accept: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function GetDesignHitTest(X: Integer; Y: Integer; Shift: TShiftState)
      : Boolean; override;
    function GetDragObjectClass: TDragControlObjectClass; override;
    function StartDragAndDrop(const P: TPoint): Boolean; override;

    procedure DoInitialize; virtual;
    procedure Initialize;

    function CreateController: TdxGanttControlCustomController;
      virtual; abstract;
    function CreateHistory: TdxGanttControlHistory; virtual;
    function CreateViewInfo: TdxGanttControlCustomViewInfo; virtual; abstract;

    procedure Changed(AType: TdxGanttControlChangedTypes);
    procedure CheckChanges;
    procedure DoCheckChanges(AChanges: TdxGanttControlChangedTypes); virtual;
    function IsUpdateLocked: Boolean;

    procedure Click; override;
    procedure DblClick; override;
    function GetCurrentCursor(X: Integer; Y: Integer): TCursor; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure MouseLeave(AControl: TControl); override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;

    property HintController: TdxGanttControlHintController read FHintController;
    property ViewInfo: TdxGanttControlCustomViewInfo read FViewInfo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure BeginUpdate;
    procedure CancelUpdate;
    procedure EndUpdate;

{$REGION 'for internal use'}
    function IsEditable: Boolean;
{$ENDREGION}
    property Controller: TdxGanttControlCustomController read FController;
    property Font;
    property History: TdxGanttControlHistory read FHistory;
    property LookAndFeel;
    property OptionsBehavior: TdxGanttControlBehaviorOptions
      read FOptionsBehavior write SetOptionsBehavior;
  end;

  { TdxGanttControlCustomDataProvider }

  TdxGanttControlCustomDataProvider = class abstract
  strict private
    FCollapsedItems: TList;
    FControl: TdxGanttControlBase;
    FInnerList: TdxFastObjectList;

    function GetItem(AIndex: Integer): TObject;
    function GetCount: Integer;
  protected
    function CanExpand(AItem: TObject): Boolean; virtual;
    function CanCollapse(AItem: TObject): Boolean; virtual;

    function CanAddItem(AItem: TObject): Boolean; virtual;
    procedure DoPopulate; virtual;
    procedure Populate(AForce: Boolean);
    function InternalIndexOf(AItem: TObject): Integer; virtual;

    function GetDataItemCount: Integer; virtual; abstract;
    function GetDataItem(Index: Integer): TObject; virtual; abstract;

    procedure ClearItems; virtual;
    procedure ItemRemoved(AItem: TObject); virtual;

    function FirstDataItem: TObject;
    function LastDataItem: TObject;

    property DataItemCount: Integer read GetDataItemCount;
    property DataItems[Index: Integer]: TObject read GetDataItem;
    property InnerList: TdxFastObjectList read FInnerList;
  public
    constructor Create(AControl: TdxGanttControlBase); virtual;
    destructor Destroy; override;

    function IndexOf(AItem: TObject): Integer;
    procedure Refresh(AForce: Boolean);

    procedure Collapse(AItem: TObject);
    procedure Expand(AItem: TObject);
    function IsExpanded(AItem: TObject): Boolean;

    property Control: TdxGanttControlBase read FControl;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TObject read GetItem; default;
  end;

implementation

uses
  RTLConsts, Math,
  cxDrawTextUtils, dxCustomHint,
  dxTypeHelpers, cxScrollBar,
  dxFormattedText;

type
  TcxControlAccess = class(TcxControl);

  { TdxGanttControlCanvasCustomCache }

constructor TdxGanttControlCanvasCustomCache.Create;
begin
  inherited Create;
  FFonts := TObjectDictionary<HFont, TcxCanvasBasedFont>.Create([doOwnsValues]);
  FImages := TObjectDictionary<TImageCacheKey, TcxCanvasBasedImage>.Create
    ([doOwnsValues]);
end;

destructor TdxGanttControlCanvasCustomCache.Destroy;
begin
  Clear;
  FreeAndNil(FFonts);
  FreeAndNil(FImages);
  inherited Destroy;
end;

function TdxGanttControlCanvasCustomCache.GetControlFont: TcxCanvasBasedFont;
begin
  Result := GetFont(GetBaseFont);
end;

function TdxGanttControlCanvasCustomCache.GetBaseBoldFont: TFont;
begin
  if FBaseBoldFont = nil then
  begin
    FBaseBoldFont := TFont.Create;
    FBaseBoldFont.Assign(GetBaseFont);
    FBaseBoldFont.Style := FBaseBoldFont.Style + [fsBold];
  end;
  Result := FBaseBoldFont;
end;

function TdxGanttControlCanvasCustomCache.GetBaseItalicFont: TFont;
begin
  if FBaseItalicFont = nil then
  begin
    FBaseItalicFont := TFont.Create;
    FBaseItalicFont.Assign(GetBaseFont);
    FBaseItalicFont.Style := FBaseItalicFont.Style + [fsItalic];
  end;
  Result := FBaseItalicFont;
end;

function TdxGanttControlCanvasCustomCache.GetFont(AFont: TFont)
  : TcxCanvasBasedFont;
begin
  if FFonts.TryGetValue(AFont.Handle, Result) then
    Exit;
  Result := Canvas.CreateFonT(AFont);
  FFonts.Add(AFont.Handle, Result);
end;

function TdxGanttControlCanvasCustomCache.GetImage(AImage: TGraphic)
  : TcxCanvasBasedImage;
var
  AKey: TImageCacheKey;
begin
  AKey.Image := AImage;
  AKey.Size := TSize.Create(AImage.Width, AImage.Height);
  if not FImages.TryGetValue(AKey, Result) then
  begin
    Canvas.ImageStretchQuality := isqHigh;
    Result := Canvas.CreateImage(AImage);
    FImages.Add(AKey, Result);
  end;
end;

procedure TdxGanttControlCanvasCustomCache.Clear;
begin
  FFonts.Clear;
  FImages.Clear;
  FreeAndNil(FBaseBoldFont);
  FreeAndNil(FBaseItalicFont);
end;

{ TdxGanttControlCustomItemViewInfo }

procedure TdxGanttControlCustomItemViewInfo.CalculateLayout;
begin
  // do nothing
end;

procedure TdxGanttControlCustomItemViewInfo.Recalculate;
begin
  Calculate(Bounds);
end;

procedure TdxGanttControlCustomItemViewInfo.ViewChanged;
begin
  // do nothing
end;

function TdxGanttControlCustomItemViewInfo.CalculateSize: TSize;
begin
  Result := cxNullSize;
end;

procedure TdxGanttControlCustomItemViewInfo.Draw;
begin
  Canvas.SaveClipRegion;
  try
    PrepareCanvas(Canvas);
    Canvas.IntersectClipRect(GetClipBounds);
    DoDraw;
  finally
    Canvas.RestoreClipRegion;
  end;
end;

function TdxGanttControlCustomItemViewInfo.GetCanvas: TcxCustomCanvas;
begin
  Result := CanvasCache.Canvas;
end;

procedure TdxGanttControlCustomItemViewInfo.SetState
  (const Value: TcxButtonState);
begin
  if FState <> Value then
  begin
    FState := Value;
    if HasHotTrackState then
      Invalidate;
  end;
end;

procedure TdxGanttControlCustomItemViewInfo.AfterConstruction;
begin
  inherited AfterConstruction;
  Reset;
end;

procedure TdxGanttControlCustomItemViewInfo.Calculate(const R: TRect);
begin
  FBounds := R;
end;

procedure TdxGanttControlCustomItemViewInfo.DoRightToLeftConversion
  (const AClientBounds: TRect);
begin
  FBounds := TdxRightToLeftLayoutConverter.ConvertRect(FBounds, AClientBounds);
end;

procedure TdxGanttControlCustomItemViewInfo.DoScroll(const DX, DY: Integer);
begin
  FBounds := cxRectOffset(FBounds, DX, DY);
end;

procedure TdxGanttControlCustomItemViewInfo.Scroll(const DX, DY: Integer);
begin
  DoScroll(DX, DY);
end;

procedure TdxGanttControlCustomItemViewInfo.PrepareCanvas
  (ACanvas: TcxCustomCanvas);
begin
  // do nothing
end;

function TdxGanttControlCustomItemViewInfo.GetResizeHitZoneWidth: Integer;
begin
  Result := dxGetTouchableSize(ScaleFactor.Apply(Mouse.DragThreshold * 2),
    ScaleFactor);
end;

procedure TdxGanttControlCustomItemViewInfo.Invalidate;
begin
  Invalidate(Bounds);
end;

procedure TdxGanttControlCustomItemViewInfo.Invalidate(const R: TRect);
begin
  // do nothing
end;

procedure TdxGanttControlCustomItemViewInfo.MouseEnter;
begin
  SetState(cxbsHot);
end;

procedure TdxGanttControlCustomItemViewInfo.MouseLeave;
begin
  SetState(cxbsNormal);
end;

procedure TdxGanttControlCustomItemViewInfo.UpdateState;
begin
  if HasPressedState then
  begin
    if IsPressed then
      SetState(cxbsPressed)
    else if State = cxbsPressed then
      SetState(cxbsNormal);
  end;
  if not HasHotTrackState then
    SetState(cxbsNormal);
end;

function TdxGanttControlCustomItemViewInfo.HasHint: Boolean;
begin
  Result := False;
end;

function TdxGanttControlCustomItemViewInfo.HasHotTrackState: Boolean;
begin
  Result := False;
end;

function TdxGanttControlCustomItemViewInfo.HasPressedState: Boolean;
begin
  Result := False;
end;

function TdxGanttControlCustomItemViewInfo.GetCurrentCursor(const P: TPoint;
  const ADefaultCursor: TCursor): TCursor;
begin
  Result := ADefaultCursor;
end;

function TdxGanttControlCustomItemViewInfo.GetHintText: string;
begin
  Result := '';
end;

function TdxGanttControlCustomItemViewInfo.IsPressed: Boolean;
begin
  Result := False;
end;

function TdxGanttControlCustomItemViewInfo.CalculateHitTest(const AHitTest
  : TdxGanttControlHitTest): Boolean;
begin
  Result := PtInRect(Bounds, AHitTest.HitPoint);
  if Result then
    AHitTest.SetHitObject(Self);
end;

function TdxGanttControlCustomItemViewInfo.GetClipBounds: TRect;
begin
  Result := FBounds;
end;

procedure TdxGanttControlCustomItemViewInfo.Reset;
begin
end;

procedure TdxGanttControlCustomItemViewInfo.SetBounds(ABounds: TRect);
begin
  FBounds := ABounds;
end;

{ TdxGanttControlCustomOwnedItemViewInfo }

constructor TdxGanttControlCustomOwnedItemViewInfo.Create
  (AOwner: TdxGanttControlCustomItemViewInfo);
begin
  inherited Create;
  FOwner := AOwner;
  UpdateCachedValues;
end;

function TdxGanttControlCustomOwnedItemViewInfo.GetCanvasCache
  : TdxGanttControlCanvasCustomCache;
begin
  Result := FCanvasCache;
end;

function TdxGanttControlCustomOwnedItemViewInfo.GetLookAndFeelPainter
  : TcxCustomLookAndFeelPainter;
begin
  Result := FLookAndFeelPainter;
end;

function TdxGanttControlCustomOwnedItemViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := FScaleFactor;
end;

function TdxGanttControlCustomOwnedItemViewInfo.
  GetUseRightToLeftAlignment: Boolean;
begin
  Result := FUseRightToLeftAlignment;
end;

procedure TdxGanttControlCustomOwnedItemViewInfo.Reset;
begin
  inherited Reset;
  UpdateCachedValues;
end;

procedure TdxGanttControlCustomOwnedItemViewInfo.UpdateCachedValues;
begin
  if Owner <> nil then
  begin
    FCanvasCache := Owner.CanvasCache;
    FLookAndFeelPainter := Owner.LookAndFeelPainter;
    FScaleFactor := Owner.ScaleFactor;
    FUseRightToLeftAlignment := Owner.UseRightToLeftAlignment;
  end;
end;

procedure TdxGanttControlCustomOwnedItemViewInfo.Invalidate(const R: TRect);
begin
  Owner.Invalidate(R);
end;

{ TdxGanttControlCustomParentViewInfo }

constructor TdxGanttControlCustomParentViewInfo.Create
  (AOwner: TdxGanttControlCustomItemViewInfo);
begin
  inherited Create(AOwner);
  FViewInfos := TdxFastObjectList.Create;
end;

destructor TdxGanttControlCustomParentViewInfo.Destroy;
begin
  FreeAndNil(FViewInfos);
  inherited Destroy;
end;

procedure TdxGanttControlCustomParentViewInfo.Calculate(const R: TRect);
var
  I: Integer;
  AItem: TdxGanttControlCustomItemViewInfo;
  AItemBounds: TRect;
begin
  inherited Calculate(R);
  for I := 0 to ViewInfoCount - 1 do
  begin
    AItem := ViewInfos[I];
    AItemBounds := CalculateItemBounds(AItem);
    AItem.Calculate(AItemBounds);
  end;
end;

function TdxGanttControlCustomParentViewInfo.CalculateHitTest
  (const AHitTest: TdxGanttControlHitTest): Boolean;
var
  I: Integer;
begin
  Result := inherited CalculateHitTest(AHitTest);
  if Result then
    for I := 0 to ViewInfoCount - 1 do
      if ViewInfos[I].CalculateHitTest(AHitTest) then
        Exit(True);
end;

procedure TdxGanttControlCustomParentViewInfo.CalculateLayout;
var
  I: Integer;
begin
  for I := 0 to ViewInfoCount - 1 do
    ViewInfos[I].CalculateLayout;
end;

procedure TdxGanttControlCustomParentViewInfo.ViewChanged;
var
  I: Integer;
begin
  for I := 0 to ViewInfoCount - 1 do
    ViewInfos[I].ViewChanged;
end;

procedure TdxGanttControlCustomParentViewInfo.Reset;
var
  I: Integer;
begin
  inherited Reset;
  for I := 0 to ViewInfoCount - 1 do
    ViewInfos[I].Reset;
end;

procedure TdxGanttControlCustomParentViewInfo.Clear;
begin
  FViewInfos.Clear;
end;

procedure TdxGanttControlCustomParentViewInfo.DoDraw;
var
  I: Integer;
begin
  for I := 0 to ViewInfoCount - 1 do
    ViewInfos[I].Draw;
end;

procedure TdxGanttControlCustomParentViewInfo.DoScroll(const DX, DY: Integer);
var
  I: Integer;
begin
  inherited DoScroll(DX, DY);
  for I := 0 to ViewInfoCount - 1 do
    ViewInfos[I].DoScroll(DX, DY);
end;

procedure TdxGanttControlCustomParentViewInfo.AddChild
  (AChild: TdxGanttControlCustomItemViewInfo);
begin
  if AChild <> nil then
    FViewInfos.Add(AChild);
end;

function TdxGanttControlCustomParentViewInfo.CalculateItemBounds
  (AItem: TdxGanttControlCustomItemViewInfo): TRect;
var
  ASize: TSize;
begin
  ASize := AItem.CalculateSize;
  if cxSizeIsEqual(ASize, cxNullSize) then
    Exit(cxNullRect);
  Result := Bounds;
  if ASize.cx >= 0 then
    if UseRightToLeftAlignment then
      Result.Left := Result.Right - ASize.cx
    else
      Result.Right := Result.Left + ASize.cx;
  if ASize.cy >= 0 then
    Result.Bottom := Result.Top + ASize.cy;
end;

function TdxGanttControlCustomParentViewInfo.GetViewInfo(Index: Integer)
  : TdxGanttControlCustomItemViewInfo;
begin
  Result := TdxGanttControlCustomItemViewInfo(FViewInfos[Index]);
end;

function TdxGanttControlCustomParentViewInfo.GetViewInfoCount: Integer;
begin
  Result := FViewInfos.Count;
end;

{ TdxGanttControlPersistent }

procedure TdxGanttControlPersistent.AfterConstruction;
begin
  inherited AfterConstruction;
  BeginUpdate;
  try
    Reset;
  finally
    CancelUpdate;
  end;
end;

procedure TdxGanttControlPersistent.Assign(Source: TPersistent);
begin
  if Source = nil then
    Reset
  else if not Source.InheritsFrom(TdxGanttControlPersistent) then
    inherited Assign(Source);
end;

procedure TdxGanttControlPersistent.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TdxGanttControlPersistent.Changed;
begin
  if IsUpdateLocked then
    Exit;
  DoChanged;
end;

procedure TdxGanttControlPersistent.EndUpdate;
begin
  Dec(FLockCount);
  Changed;
end;

function TdxGanttControlPersistent.IsUpdateLocked: Boolean;
begin
  Result := FLockCount > 0;
end;

procedure TdxGanttControlPersistent.Reset;
begin
  BeginUpdate;
  try
    if not BeforeResetHandlers.Empty then
      BeforeResetHandlers.Invoke(Self);
    DoReset;
    if not AfterResetHandlers.Empty then
      AfterResetHandlers.Invoke(Self);
  finally
    EndUpdate;
  end;
end;

procedure TdxGanttControlPersistent.CancelUpdate;
begin
  Dec(FLockCount);
end;

{ TdxGanttControlCustomOptions }

constructor TdxGanttControlCustomOptions.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TdxGanttControlCustomOptions.DoChanged;
begin
  DoChanged(FChanges);
end;

procedure TdxGanttControlCustomOptions.Changed
  (AChanges: TdxGanttControlOptionsChangedTypes);
begin
  FChanges := FChanges + AChanges;
  if IsUpdateLocked or (FChanges = []) then
    Exit;
  DoChanged(FChanges);
  FChanges := [];
end;

procedure TdxGanttControlCustomOptions.DoChanged
  (AChanges: TdxGanttControlOptionsChangedTypes);
begin
  if not ChangedHandlers.Empty then
    ChangedHandlers.Invoke(Self, AChanges);
end;

function TdxGanttControlCustomOptions.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

{ TdxGanttControlHitTest }

constructor TdxGanttControlHitTest.Create(AController
  : TdxGanttControlCustomController);
begin
  inherited Create;
  FController := AController;
end;

procedure TdxGanttControlHitTest.Calculate(const X, Y: Integer);
begin
  SetHitPoint(cxPoint(X, Y));
end;

procedure TdxGanttControlHitTest.Clear;
begin
  FHitObject := nil;
  FHitPoint := cxInvisiblePoint;
end;

procedure TdxGanttControlHitTest.Recalculate;
var
  P: TPoint;
begin
  P := HitPoint;
  Clear;
  Calculate(P.X, P.Y);
end;

procedure TdxGanttControlHitTest.SetHitObject(const AHitObject
  : TdxGanttControlCustomItemViewInfo);
begin
  if FHitObject <> AHitObject then
  begin
    if FHitObject <> nil then
      FHitObject.MouseLeave;
    FHitObject := AHitObject;
    if FHitObject <> nil then
      FHitObject.MouseEnter;
  end;
end;

procedure TdxGanttControlHitTest.SetHitPoint(const Value: TPoint);
begin
  if not FHitPoint.IsEqual(Value) then
  begin
    FHitPoint := Value;
    if Controller.ViewInfo <> nil then
      Controller.ViewInfo.CalculateHitTest(Self);
  end;
end;

{ TdxGanttControlHintWindow }

function TdxGanttControlHintWindow.CalcHintRect(AMaxWidth: Integer;
  const AHint: string; AData: Pointer): TRect;
var
  AIndent: Integer;
  AFormattedText: TdxFormattedText;
begin
  Result := cxRect(0, 0, AMaxWidth, MaxInt);
  cxScreenCanvas.Font := Font;
  AFormattedText := TdxFormattedText.Create;
  try
    TdxFormattedTextConverters.Import(AFormattedText, AHint, Font);
    AFormattedText.CalculateLayout(cxScreenCanvas.Canvas, Font, Result,
      GetDrawTextFlags, Owner.ScaleFactor);
    Result := TRect.CreateSize(AFormattedText.TextSize);
  finally
    AFormattedText.Free;
  end;
  cxScreenCanvas.Dormant;
  AIndent := 2 * Owner.ScaleFactor.Apply(cxTextOffset);
  Inc(Result.Right, AIndent + 2);
  Inc(Result.Bottom, AIndent);
end;

function TdxGanttControlHintWindow.GetDrawTextFlags: Integer;
begin
  Result := CXTO_WORDBREAK or CXTO_NOPREFIX;
  if IsRightToLeft then
    Result := Result or CXTO_RTLREADING;
end;

function TdxGanttControlHintWindow.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := Owner.LookAndFeel.Painter;
end;

procedure TdxGanttControlHintWindow.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  cxPaintCanvas.BeginPaint(Message.DC);
  try
    Painter.DrawHintBackground(cxPaintCanvas, ClientRect);
  finally
    cxPaintCanvas.EndPaint;
  end;
  Message.Result := 1;
end;

function TdxGanttControlHintWindow.InternalGetOwner: TdxGanttControlBase;
begin
  Result := TdxGanttControlBase(inherited Owner);
end;

procedure TdxGanttControlHintWindow.NCPaint(DC: HDC);
begin
  cxPaintCanvas.BeginPaint(DC);
  try
    cxPaintCanvas.FillRect(cxRectSetNullOrigin(BoundsRect),
      cxGetActualColor(Painter.GetHintBorderColor, clWindowFrame));
    Painter.DrawHintBackground(cxPaintCanvas, ClientRect);
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TdxGanttControlHintWindow.Paint;
var
  ATextRect: TRect;
  AFormattedText: TdxFormattedText;
begin
  cxPaintCanvas.BeginPaint(Canvas);
  try
    Painter.DrawHintBackground(cxPaintCanvas, ClientRect);

    cxPaintCanvas.Font := Font;
    cxPaintCanvas.Font.Color := Painter.ScreenTipGetTitleTextColor;
    cxPaintCanvas.Brush.Style := bsClear;

    AFormattedText := TdxFormattedText.Create;
    try
      TdxFormattedTextConverters.Import(AFormattedText, Text, Font);
      ATextRect := cxRectInflate(ClientRect,
        -Owner.ScaleFactor.Apply(cxTextOffset));
      AFormattedText.CalculateLayout(cxPaintCanvas.Canvas, cxPaintCanvas.Font,
        ATextRect, GetDrawTextFlags, Owner.ScaleFactor);
      AFormattedText.Draw(cxPaintCanvas.Canvas, ATextRect.TopLeft);
    finally
      AFormattedText.Free;
    end;
  finally
    cxPaintCanvas.EndPaint;
  end;
end;

{ TdxGanttControlDragAndDropObject }

procedure TdxGanttControlDragAndDropObject.AfterScrolling(AScrollDirection
  : TcxDirection);
begin
  // do nothing
end;

procedure TdxGanttControlDragAndDropObject.BeforeScrolling;
begin
  // do nothing
end;

function TdxGanttControlDragAndDropObject.CanDrop(const P: TPoint): Boolean;
begin
  Result := False;
end;

procedure TdxGanttControlDragAndDropObject.BeginDragAndDrop;
begin
  inherited BeginDragAndDrop;
  FDragImage := CreateDragImage;
end;

constructor TdxGanttControlDragAndDropObject.Create
  (AController: TdxGanttControlCustomController);
begin
  inherited Create(AController.Control);
  FController := AController;
end;

function TdxGanttControlDragAndDropObject.CreateDragImage: TcxDragImage;
begin
  Result := nil;
end;

procedure TdxGanttControlDragAndDropObject.EndDragAndDrop(Accepted: Boolean);
begin
  try
    try
      Controller.DragHelper.ScrollDirection := dirNone;
      if Accepted and CanDrop(CurMousePos) then
        ApplyChanges(CurMousePos);
    finally
      FreeAndNil(FDragImage);
      inherited EndDragAndDrop(Accepted);
    end;
  except
    raise;
  end;
end;

function TdxGanttControlDragAndDropObject.GetControl: TdxGanttControlBase;
begin
  Result := TdxGanttControlBase(inherited Control);
end;

function TdxGanttControlDragAndDropObject.GetHitTest: TdxGanttControlHitTest;
begin
  Result := Controller.HitTest;
end;

procedure TdxGanttControlDragAndDropObject.HideDragImage;
begin
  FDragImage.Hide;
end;

procedure TdxGanttControlDragAndDropObject.ShowDragImage(const P: TPoint);
begin
  FDragImage.MoveTo(Control.ClientToScreen(P));
  FDragImage.Show;
end;

{ TdxGanttControlResizingObject }

function TdxGanttControlResizingObject.CreateDragImage: TcxDragImage;
begin
  Result := TcxDragImage.Create;
  Result.SetBounds(0, 0, GetDragImageWidth, GetDragImageHeight);
  Result.Canvas.Lock;
  try
    Result.Canvas.Brush.Bitmap := AllocPatternBitmap(clBlack, clWhite);
    Result.Canvas.Canvas.FillRect(Result.ClientRect);
  finally
    Result.Canvas.Unlock;
  end;
end;

{ TdxGanttControlMovingObject }

procedure TdxGanttControlMovingObject.CheckScrolling(const P: TPoint);
begin
  Controller.DragHelper.CheckScrolling(P);
end;

{ TdxGanttControlDragHelper }

constructor TdxGanttControlDragHelper.Create(AController
  : TdxGanttControlCustomController);
begin
  inherited Create;
  FController := AController;
end;

destructor TdxGanttControlDragHelper.Destroy;
begin
  FreeAndNil(FScrollTimer);
  inherited Destroy;
end;

procedure TdxGanttControlDragHelper.BeginDragAndDrop;
begin
  // do nothing
end;

function TdxGanttControlDragHelper.CanDrag(X, Y: Integer): Boolean;
begin
  Result := StartDragAndDrop(TPoint.Create(X, Y));
end;

procedure TdxGanttControlDragHelper.DragAndDrop(const P: TPoint;
  var Accepted: Boolean);
begin
  if DragAndDropObject <> nil then
    DragAndDropObject.DoDragAndDrop(P, Accepted);
end;

procedure TdxGanttControlDragHelper.DragDrop(Source: TObject; X, Y: Integer);
begin
  // do nothing
end;

procedure TdxGanttControlDragHelper.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  Accept := False;
end;

procedure TdxGanttControlDragHelper.EndDrag(Target: TObject; X, Y: Integer);
begin
  ScrollDirection := dirNone;
end;

procedure TdxGanttControlDragHelper.EndDragAndDrop(Accepted: Boolean);
begin
  ScrollDirection := dirNone;
end;

procedure TdxGanttControlDragHelper.AfterScrolling(AScrollDirection
  : TcxDirection);
begin
  if (DragAndDropObject <> nil) and
    (DragAndDropObject is TdxGanttControlDragAndDropObject) then
    TdxGanttControlDragAndDropObject(DragAndDropObject)
      .AfterScrolling(AScrollDirection);
end;

procedure TdxGanttControlDragHelper.BeforeScrolling;
begin
  if (DragAndDropObject <> nil) and
    (DragAndDropObject is TdxGanttControlDragAndDropObject) then
    TdxGanttControlDragAndDropObject(DragAndDropObject).BeforeScrolling;
end;

function TdxGanttControlDragHelper.CanScroll: Boolean;
begin
  Result := Controller.CanAutoScroll(ScrollDirection);
end;

procedure TdxGanttControlDragHelper.CheckScrolling(const P: TPoint);
var
  R: TRect;
  ADirection: TcxDirection;
  AScrollHotZoneWidth: Integer;
begin
  R := GetScrollableArea;
  if PtInRect(R, P) then
  begin
    AScrollHotZoneWidth := Controller.ViewInfo.ScaleFactor.Apply
      (ScrollHotZoneWidth);
    if P.X < R.Left + AScrollHotZoneWidth then
      ADirection := dirLeft
    else if R.Right - AScrollHotZoneWidth <= P.X then
      ADirection := dirRight
    else if P.Y < R.Top + AScrollHotZoneWidth then
      ADirection := dirUp
    else if P.Y > R.Bottom - AScrollHotZoneWidth then
      ADirection := dirDown
    else
      ADirection := dirNone;
  end
  else
    ADirection := dirNone;
  if Controller.ViewInfo.UseRightToLeftAlignment then
    case ADirection of
      dirLeft:
        ADirection := dirRight;
      dirRight:
        ADirection := dirLeft;
    end;
  if not Controller.CanAutoScroll(ADirection) then
    ADirection := dirNone;
  ScrollDirection := ADirection;
end;

procedure TdxGanttControlDragHelper.DoScroll;
begin
  // do nothing
end;

function TdxGanttControlDragHelper.GetScrollableArea: TRect;
begin
  Result := Controller.ViewInfo.Bounds;
end;

procedure TdxGanttControlDragHelper.ScrollTimerHandler(Sender: TObject);
var
  AAccepted: Boolean;
  P: TPoint;
begin
  if CanScroll then
  begin
    P := Controller.HitTest.HitPoint;
    BeforeScrolling;
    try
      DoScroll;
    finally
      AfterScrolling(ScrollDirection);
    end;
    if IsDragging then
      Controller.DragAndDrop(P, AAccepted);
  end
  else
    ScrollDirection := dirNone;
end;

function TdxGanttControlDragHelper.GetDragAndDropObject: TcxDragAndDropObject;
begin
  if IsDragging then
    Result := Controller.Control.DragAndDropObject
  else
    Result := nil;
end;

function TdxGanttControlDragHelper.GetDragObjectClass
  : TdxGanttControlDragObjectClass;
begin
  Result := nil;
end;

function TdxGanttControlDragHelper.GetHitTest: TdxGanttControlHitTest;
begin
  Result := Controller.HitTest;
end;

function TdxGanttControlDragHelper.IsDragging: Boolean;
begin
  Result := Controller.Control.DragAndDropState in [ddsStarting, ddsInProcess];
end;

procedure TdxGanttControlDragHelper.SetScrollDirection
  (const Value: TcxDirection);
begin
  if ScrollDirection <> Value then
  begin
    FreeAndNil(FScrollTimer);
    FScrollDirection := Value;
    if FScrollDirection <> dirNone then
    begin
      FScrollTimer := TcxTimer.Create(nil);
      FScrollTimer.Interval := ScrollTimeInterval;
      FScrollTimer.OnTimer := ScrollTimerHandler;
    end;
  end;
end;

procedure TdxGanttControlDragHelper.StartDrag(var DragObject: TDragObject);
begin
  // do nothing
end;

{ TdxGanttControlMainScrollBarsHelper }

function TdxGanttControlMainScrollbarsHelper.
  CanScrollContentByGestureWithoutScrollBars: Boolean;
begin
  Result := True;
end;

function TdxGanttControlMainScrollbarsHelper.GetControl: TcxControl;
begin
  Result := Owner.GetOwnerControl;
end;

function TdxGanttControlMainScrollbarsHelper.GetHScrollBarBounds: TRect;
begin
  Result := Owner.GetHScrollBarBounds;
end;

function TdxGanttControlMainScrollbarsHelper.GetOwner
  : TdxGanttControlCustomScrollBars;
begin
  Result := inherited Owner as TdxGanttControlCustomScrollBars;
end;

function TdxGanttControlMainScrollbarsHelper.
  GetScrollbarBasedGestureClientSize: TSize;
var
  R: TRect;
begin
  R := Owner.Controller.ViewInfo.Bounds;
  Result := Size(R.Width, R.Height);
end;

function TdxGanttControlMainScrollbarsHelper.GetScrollBarLookAndFeel
  : TcxLookAndFeel;
var
  AScrollBarOwner: IcxScrollBarOwner;
begin
  Supports(GetControl, IcxScrollBarOwner, AScrollBarOwner);
  Result := AScrollBarOwner.GetLookAndFeel;
end;

function TdxGanttControlMainScrollbarsHelper.
  GetScrollContentForegroundColor: TColor;
begin
  Result := Owner.Controller.ViewInfo.LookAndFeelPainter.
    GridLikeControlContentTextColor;
end;

function TdxGanttControlMainScrollbarsHelper.GetVScrollBarBounds: TRect;
begin
  Result := Owner.GetVScrollBarBounds;
end;

function TdxGanttControlMainScrollbarsHelper.HasScrollBarArea: Boolean;
begin
  Result := GetScrollbarMode in [sbmClassic, sbmHybrid];
end;

procedure TdxGanttControlMainScrollbarsHelper.InitScrollBarsParameters;
begin
  Owner.InitScrollBarsParameters;
end;

function TdxGanttControlMainScrollbarsHelper.IsDataScrollbar
  (AKind: TScrollBarKind): Boolean;
begin
  Result := False;
end;

procedure TdxGanttControlMainScrollbarsHelper.OwnerBoundsChanged;
begin
  Owner.Controller.Changed;
end;

procedure TdxGanttControlMainScrollbarsHelper.OwnerCheckOverpan
  (AScrollKind: TScrollBarKind; ANewDataPos, AMinDataPos, AMaxDataPos: Integer;
  ADeltaX, ADeltaY: Integer);
begin
  if Owner.IsUnlimitedScrolling(AScrollKind, ADeltaX, ADeltaY) then
    AMaxDataPos := MaxInt;
  inherited OwnerCheckOverpan(AScrollKind, ANewDataPos, AMinDataPos,
    AMaxDataPos, ADeltaX, ADeltaY);
end;

procedure TdxGanttControlMainScrollbarsHelper.OwnerGestureScroll
  (AScrollKind: TScrollBarKind; ANewScrollPos: Integer);
begin
  Scroll(AScrollKind, scTrack, ANewScrollPos);
end;

procedure TdxGanttControlMainScrollbarsHelper.OwnerUpdate;
begin
  GetControl.Update;
end;

procedure TdxGanttControlMainScrollbarsHelper.Scroll(AScrollBarKind
  : TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  if AScrollBarKind = sbHorizontal then
    Owner.DoHScroll(AScrollCode, AScrollPos)
  else
    Owner.DoVScroll(AScrollCode, AScrollPos);
end;

procedure TdxGanttControlMainScrollbarsHelper.ScrollBarVisibilityChanged
  (AScrollBars: TScrollBarKinds);
begin
  Owner.Controller.Changed;
end;

{ TdxGanttControlCustomScrollBars }

constructor TdxGanttControlCustomScrollBars.Create
  (AController: TdxGanttControlCustomController);
begin
  inherited Create;
  FController := AController;
  FHelper := TdxGanttControlMainScrollbarsHelper.Create(Self);
end;

destructor TdxGanttControlCustomScrollBars.Destroy;
begin
  TdxTouchScrollUIModeManager.Deactivate(Self);
  FreeAndNil(FHelper);
  inherited Destroy;
end;

function TdxGanttControlCustomScrollBars.AllowGesture
  (AGestureId: Integer): Boolean;
begin
  Result := (GetInteractiveGestureByGestureID(AGestureId)
    in GetOwnerControl.Touch.InteractiveGestures);
end;

function TdxGanttControlCustomScrollBars.AllowPan
  (AScrollKind: TScrollBarKind): Boolean;
begin
  Result := CanScrolling(AScrollKind);
end;

procedure TdxGanttControlCustomScrollBars.BeginGestureScroll(APos: TPoint);
begin
  FHelper.BeginGestureScroll(APos);
end;

procedure TdxGanttControlCustomScrollBars.EndGestureScroll;
begin
  FHelper.EndGestureScroll;
end;

procedure TdxGanttControlCustomScrollBars.GestureScroll(ADeltaX,
  ADeltaY: Integer);
var
  AScrollKind: TScrollBarKind;
begin
  for AScrollKind := Low(TScrollBarKind) to High(TScrollBarKind) do
    FHelper.ScrollBarBasedGestureScroll(AScrollKind, ADeltaX, ADeltaY);
end;

function TdxGanttControlCustomScrollBars.GetPanOptions: Integer;
begin
  Result := TcxControlAccess(GetOwnerControl).GetPanOptions;
end;

function TdxGanttControlCustomScrollBars.IsPanArea(const APoint
  : TPoint): Boolean;
begin
  Result := Controller.IsPanArea(APoint);
end;

function TdxGanttControlCustomScrollBars.NCSizeChanged: Boolean;
var
  ASize: Integer;
begin
  Result := False;
  if HScrollBar <> nil then
  begin
    ASize := Helper.GetScrollBarSize.cy;
    if HScrollBar.Height <> ASize then
    begin
      HScrollBar.Height := ASize;
      Result := True;
    end;
  end;
  if VScrollBar <> nil then
  begin
    ASize := Helper.GetScrollBarSize.cx;
    if VScrollBar.Width <> ASize then
    begin
      VScrollBar.Width := ASize;
      Result := True;
    end;
  end;
  CalculateScrollBars;
end;

function TdxGanttControlCustomScrollBars.NeedPanningFeedback
  (AScrollKind: TScrollBarKind): Boolean;
begin
  Result := True;
end;

procedure TdxGanttControlCustomScrollBars.CheckUIPosition;
begin
  CalculateScrollBars;
end;

function TdxGanttControlCustomScrollBars.GetOwnerControl: TcxControl;
begin
  Result := Controller.Control;
end;

function TdxGanttControlCustomScrollBars.GetScrollbarMode: TdxScrollbarMode;
begin
  Result := FHelper.GetScrollbarMode;
end;

function TdxGanttControlCustomScrollBars.HasVisibleUI: Boolean;
begin
  Result := FHelper.IsScrollBarVisible(sbVertical) or
    FHelper.IsScrollBarVisible(sbHorizontal);
end;

procedure TdxGanttControlCustomScrollBars.HideUI;
begin
  FHelper.HideTouchScrollUIDirectly;
end;

function TdxGanttControlCustomScrollBars.GetBaseColor: TColor;
begin
  Result := FHelper.GetHybridScrollbarBaseColor;
end;

function TdxGanttControlCustomScrollBars.GetManager: TdxHybridScrollbarsManager;
begin
  Result := FHelper.GetHybridScrollBarsManager;
end;

procedure TdxGanttControlCustomScrollBars.InvalidateScrollBars;
begin
  FHelper.MainScrollBars.Invalidate;
end;

function TdxGanttControlCustomScrollBars.CanScrolling
  (AKind: TScrollBarKind): Boolean;
begin
  Result := FHelper.IsScrollBarActive(AKind);
end;

procedure TdxGanttControlCustomScrollBars.DoScrollContent
  (ADirection: TcxDirection);
begin
  FHelper.ScrollContent(ADirection);
end;

procedure TdxGanttControlCustomScrollBars.ProcessControlScrollingOnMiddleButton;
var
  AIsScrollingContent: Boolean;
begin
  cxProcessControlScrollingOnMiddleButton(Controller.Control,
    CanScrolling(sbHorizontal), CanScrolling(sbVertical), DoScrollContent,
    AIsScrollingContent);
end;

function TdxGanttControlCustomScrollBars.GetMouseWheelActiveScrollBar
  (Shift: TShiftState): IcxControlScrollBar;
begin
  Result := nil;
  if not FHelper.IsScrollBarActive(sbHorizontal) then
    Exit(VScrollBar);
  if not FHelper.IsScrollBarActive(sbVertical) then
    Exit(HScrollBar);
  if Shift = [ssShift] then
    Result := HScrollBar
  else
    Result := VScrollBar;
end;

function TdxGanttControlCustomScrollBars.DoMouseWheel(Shift: TShiftState;
  AIsIncrement: Boolean): Boolean;

  procedure InternalScroll(AScrollBar: IcxControlScrollBar; ACode: TScrollCode);
  const
    AScrollDirectionMap: array [Boolean, Boolean] of TcxDirection =
      ((dirLeft, dirRight), (dirUp, dirDown));
  var
    APos: Integer;
    AScrollDirection: TcxDirection;
  begin
    if Controller.DragHelper <> nil then
      Controller.DragHelper.BeforeScrolling;
    try
      APos := AScrollBar.Position;
      Helper.Scroll(AScrollBar.Kind, ACode, APos);
      Helper.ShowTouchScrollUI(Self);
      Helper.HideTouchScrollUI(Self);
      AScrollBar.Position := APos;
    finally
      if Controller.DragHelper <> nil then
      begin
        if ACode = scLineUp then
          AScrollDirection := AScrollDirectionMap
            [AScrollBar.Kind = sbVertical, False]
        else if ACode = scLineDown then
          AScrollDirection := AScrollDirectionMap
            [AScrollBar.Kind = sbVertical, True]
        else
          AScrollDirection := dirNone;
        Controller.DragHelper.AfterScrolling(AScrollDirection);
      end;
    end;
  end;

const
  AScrollCodeMap: array [Boolean] of TScrollCode = (scLineUp, scLineDown);
  AScrollCodePageMap: array [Boolean] of TScrollCode = (scPageUp, scPageDown);
var
  I: Integer;
  AScrollBar: IcxControlScrollBar;
begin
  AScrollBar := GetMouseWheelActiveScrollBar(Shift);
  Result := AScrollBar <> nil;
  if Result then
  begin
    if Mouse.WheelScrollLines = -1 then
      InternalScroll(AScrollBar, AScrollCodePageMap[AIsIncrement])
    else
      for I := 0 to Mouse.WheelScrollLines - 1 do
        InternalScroll(AScrollBar, AScrollCodeMap[AIsIncrement]);
  end;
end;

procedure TdxGanttControlCustomScrollBars.SetScrollInfo(AScrollBarKind
  : TScrollBarKind; AMin, AMax, AStep, APage, APos: Integer;
  AAllowShow, AAllowHide: Boolean);
begin
  FHelper.SetScrollBarInfo(AScrollBarKind, AMin, AMax, AStep, APage, APos,
    AAllowShow, AAllowHide);
end;

function TdxGanttControlCustomScrollBars.GetHScrollBarAreaHeight: Integer;
begin
  Result := FHelper.GetHScrollBarAreaHeight;
end;

function TdxGanttControlCustomScrollBars.GetVScrollBarAreaWidth: Integer;
begin
  Result := FHelper.GetVScrollBarAreaWidth;
end;

procedure TdxGanttControlCustomScrollBars.CalculateScrollBars;
begin
  FHelper.UpdateScrollBars;
end;

procedure TdxGanttControlCustomScrollBars.DoCreateScrollBars;
begin
  FHelper.CreateScrollBars;
end;

procedure TdxGanttControlCustomScrollBars.DoDestroyScrollBars;
begin
  FHelper.HideScrollBars;
  if GetScrollbarMode = sbmTouch then
    TdxTouchScrollUIModeManager.Deactivate(Self);
  FHelper.DestroyScrollBars;
end;

procedure TdxGanttControlCustomScrollBars.DrawSizeGrip
  (ACanvas: TcxCustomCanvas);
begin
  if ACanvas is TcxCanvas then
    FHelper.DrawSizeGrip(TcxCanvas(ACanvas));
end;

function TdxGanttControlCustomScrollBars.GetClientRect(const R: TRect): TRect;
begin
  Result := R;
  if GetScrollbarMode = sbmClassic then
  begin
    if (HScrollBar <> nil) and HScrollBar.Visible then
      Result.Bottom := Result.Bottom - GetHScrollBarAreaHeight;
    if (VScrollBar <> nil) and VScrollBar.Visible then
    begin
      if Controller.ViewInfo.UseRightToLeftAlignment then
        Result.Left := Result.Left + GetVScrollBarAreaWidth
      else
        Result.Right := Result.Right - GetVScrollBarAreaWidth;
    end;
  end;
end;

procedure TdxGanttControlCustomScrollBars.HideTouchScrollUI
  (AImmediately: Boolean);
begin
  FHelper.HideTouchScrollUI(Self, AImmediately);
end;

procedure TdxGanttControlCustomScrollBars.InitScrollBars;
begin
  FHelper.InitScrollBars;
end;

procedure TdxGanttControlCustomScrollBars.InitScrollBarsParameters;
begin
  DoInitHScrollBarParameters;
  DoInitVScrollBarParameters;
end;

function TdxGanttControlCustomScrollBars.IsUnlimitedScrolling
  (AScrollKind: TScrollBarKind; ADeltaX, ADeltaY: Integer): Boolean;
begin
  Result := False;
end;

procedure TdxGanttControlCustomScrollBars.DoInitHScrollBarParameters;
begin
end;

procedure TdxGanttControlCustomScrollBars.DoInitVScrollBarParameters;
begin
end;

procedure TdxGanttControlCustomScrollBars.ShowTouchScrollUI;
begin
  if (HScrollBar <> nil) or (VScrollBar <> nil) then
    FHelper.ShowTouchScrollUI(Self, True);
end;

procedure TdxGanttControlCustomScrollBars.UnInitScrollbars;
begin
  FHelper.UnInitScrollbars;
end;

function TdxGanttControlCustomScrollBars.GetHScrollBar: IcxControlScrollBar;
begin
  Result := FHelper.GetScrollBar(sbHorizontal);
end;

function TdxGanttControlCustomScrollBars.GetHScrollBarBounds: TRect;
begin
  Result := Controller.ViewInfo.Bounds;
  case GetScrollbarMode of
    sbmTouch:
      begin
        Result.Top := Result.Bottom - HScrollBar.Height;
        if FHelper.IsScrollBarVisible(sbVertical) then
          Result.Right := Result.Right - VScrollBar.Width;
      end;
    sbmClassic:
      begin
        Result.Top := Result.Bottom - HScrollBar.Height;
        if FHelper.IsScrollBarVisible(sbVertical) then
          Result.Right := Result.Right - VScrollBar.Width;
      end
  else
    Result.Top := Result.Bottom - HScrollBar.Height;
    if FHelper.IsScrollBarActive(sbVertical) then
      Result.Right := Result.Right - VScrollBar.Width;
  end;
  if Controller.ViewInfo.UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result,
      Controller.ViewInfo.Bounds);
end;

function TdxGanttControlCustomScrollBars.GetVScrollBar: IcxControlScrollBar;
begin
  Result := FHelper.GetScrollBar(sbVertical);
end;

function TdxGanttControlCustomScrollBars.GetVScrollBarBounds: TRect;
begin
  Result := Controller.ViewInfo.Bounds;
  case GetScrollbarMode of
    sbmTouch:
      begin
        Result.Left := Result.Right - VScrollBar.Width;
        if FHelper.IsScrollBarVisible(sbHorizontal) then
          Result.Bottom := Result.Bottom - HScrollBar.Height;
      end;
    sbmClassic:
      begin
        Result.Left := Result.Right - VScrollBar.Width;
        if FHelper.IsScrollBarVisible(sbHorizontal) then
          Result.Bottom := Result.Bottom - HScrollBar.Height;
      end;
  else
    Result.Left := Result.Right - VScrollBar.Width;
    if FHelper.IsScrollBarActive(sbHorizontal) then
      Result.Bottom := Result.Bottom - HScrollBar.Height;
  end;
  if Controller.ViewInfo.UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result,
      Controller.ViewInfo.Bounds);
end;

{ TdxGanttControlCustomController }

constructor TdxGanttControlCustomController.Create
  (AControl: TdxGanttControlBase);
begin
  inherited Create;
  FControl := AControl;
  FHitTest := CreateHitTest;
  FDragHelper := CreateDragHelper;
end;

destructor TdxGanttControlCustomController.Destroy;
begin
  FreeAndNil(FDragHelper);
  FreeAndNil(FHitTest);
  inherited Destroy;
end;

procedure TdxGanttControlCustomController.Activated;
begin
  DoCreateScrollBars;
end;

procedure TdxGanttControlCustomController.Deactivated;
begin
  if not Control.IsLoading then
    HideEditing;
  DoDestroyScrollBars;
end;

procedure TdxGanttControlCustomController.Changed;
begin
  Control.Changed([TdxGanttControlChangedType.Size]);
end;

procedure TdxGanttControlCustomController.CheckHint(Shift: TShiftState);
var
  AHitViewInfo: TdxGanttControlCustomItemViewInfo;
begin
  if Control.IsDesigning then
    Exit;
  if (HitTest.HitObject <> nil) and not(ssLeft in Shift) and HitTest.HitObject.HasHint
  then
    AHitViewInfo := HitTest.HitObject
  else
    AHitViewInfo := nil;

  if AHitViewInfo <> nil then
    Control.HintController.Activate(AHitViewInfo)
  else
    Control.HintController.HideHint;
end;

procedure TdxGanttControlCustomController.HideEditing;
begin
  // do nothing
end;

procedure TdxGanttControlCustomController.DoClick;
begin
  // do nothing
end;

procedure TdxGanttControlCustomController.DoDblClick;
begin
  // do nothing
end;

procedure TdxGanttControlCustomController.DoMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  Control.HintController.Hide;
end;

procedure TdxGanttControlCustomController.DoMouseMove(Shift: TShiftState;
  X, Y: Integer);
begin
  CheckHint(Shift);
end;

procedure TdxGanttControlCustomController.DoMouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  // do nothing
end;

function TdxGanttControlCustomController.DoMouseWheel(Shift: TShiftState;
  AIsIncrement: Boolean; const AMousePos: TPoint): Boolean;
begin
  Result := False;
end;

procedure TdxGanttControlCustomController.DoKeyDown(var Key: Word;
  Shift: TShiftState);
begin
  // do nothing
end;

procedure TdxGanttControlCustomController.DoKeyPress(var Key: Char);
begin
  // do nothing
end;

procedure TdxGanttControlCustomController.DoKeyUp(var Key: Word;
  Shift: TShiftState);
begin
  // do nothing
end;

function TdxGanttControlCustomController.CreateHitTest: TdxGanttControlHitTest;
begin
  Result := TdxGanttControlHitTest.Create(Self);
end;

function TdxGanttControlCustomController.CreateDragHelper
  : TdxGanttControlDragHelper;
begin
  Result := nil;
end;

function TdxGanttControlCustomController.GetDragHelper
  : TdxGanttControlDragHelper;
begin
  Result := FDragHelper;
end;

function TdxGanttControlCustomController.GetGestureClient(const APoint: TPoint)
  : IdxGestureClient;
begin
  Result := nil;
end;

procedure TdxGanttControlCustomController.DoCreateScrollBars;
begin
  // do nothing
end;

procedure TdxGanttControlCustomController.DoDestroyScrollBars;
begin
  // do nothing
end;

function TdxGanttControlCustomController.GetTouchScrollUIOwner
  (const APoint: TPoint): IdxTouchScrollUIOwner;
begin
  Result := nil;
end;

function TdxGanttControlCustomController.GetHitTest: TdxGanttControlHitTest;
begin
  Result := Control.Controller.FHitTest;
end;

function TdxGanttControlCustomController.GetScaleFactor: TdxScaleFactor;
begin
  Result := Control.ScaleFactor;
end;

procedure TdxGanttControlCustomController.Click;
var
  P: TPoint;
begin
  P := Control.GetMouseCursorClientPos;
  UpdateHitTest(P.X, P.Y);
  DoClick;
end;

procedure TdxGanttControlCustomController.DblClick;
var
  P: TPoint;
begin
  P := Control.GetMouseCursorClientPos;
  UpdateHitTest(P.X, P.Y);
  DoDblClick;
end;

function TdxGanttControlCustomController.GetCurrentCursor(X,
  Y: Integer): TCursor;
begin
  UpdateHitTest(X, Y);
  Result := Control.Cursor;
  if HitTest.HitObject <> nil then
    Result := HitTest.HitObject.GetCurrentCursor(TPoint.Create(X, Y), Result);
end;

procedure TdxGanttControlCustomController.InitScrollBars;
begin
  // do nothing
end;

function TdxGanttControlCustomController.IsMouseWheelHandleNeeded
  (const MousePos: TPoint): Boolean;
begin
  Result := False;
end;

function TdxGanttControlCustomController.GetViewInfo
  : TdxGanttControlCustomItemViewInfo;
begin
  Result := Control.ViewInfo;
end;

function TdxGanttControlCustomController.IsPanArea(const APoint
  : TPoint): Boolean;
begin
  Result := False;
end;

procedure TdxGanttControlCustomController.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  UpdateHitTest(X, Y);
  DoMouseDown(Button, Shift, X, Y);
end;

procedure TdxGanttControlCustomController.MouseMove(Shift: TShiftState;
  X, Y: Integer);
begin
  UpdateHitTest(X, Y);
  DoMouseMove(Shift, X, Y);
end;

procedure TdxGanttControlCustomController.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  UpdateHitTest(X, Y);
  DoMouseUp(Button, Shift, X, Y);
end;

procedure TdxGanttControlCustomController.MouseLeave;
begin
  HitTest.SetHitObject(nil);
  HitTest.Clear;
end;

function TdxGanttControlCustomController.MouseWheel(Shift: TShiftState;
  AIsIncrement: Boolean; const AMousePos: TPoint): Boolean;
begin
  HitTest.Calculate(AMousePos.X, AMousePos.Y);
  Result := DoMouseWheel(Shift, AIsIncrement, AMousePos);
end;

function TdxGanttControlCustomController.ProcessNCSizeChanged: Boolean;
begin
  Result := False;
end;

procedure TdxGanttControlCustomController.KeyDown(var Key: Word;
  Shift: TShiftState);
begin
  DoKeyDown(Key, Shift);
end;

procedure TdxGanttControlCustomController.KeyPress(var Key: Char);
begin
  DoKeyPress(Key);
end;

procedure TdxGanttControlCustomController.KeyUp(var Key: Word;
  Shift: TShiftState);
begin
  DoKeyUp(Key, Shift);
end;

procedure TdxGanttControlCustomController.UnInitScrollbars;
begin
  // do nothing
end;

procedure TdxGanttControlCustomController.UpdateHitTest(X, Y: Integer);
begin
  HitTest.Calculate(X, Y);
end;

procedure TdxGanttControlCustomController.BeginDragAndDrop;
begin
  if DragHelper <> nil then
    DragHelper.BeginDragAndDrop;
end;

function TdxGanttControlCustomController.CanAutoScroll
  (ADirection: TcxDirection): Boolean;
begin
  Result := False;
end;

function TdxGanttControlCustomController.CanDrag(X, Y: Integer): Boolean;
begin
  UpdateHitTest(X, Y);
  Result := (DragHelper <> nil) and DragHelper.CanDrag(X, Y);
end;

function TdxGanttControlCustomController.CreateDragAndDropObject
  : TdxGanttControlDragAndDropObject;
begin
  if DragHelper <> nil then
    Result := DragHelper.CreateDragAndDropObject
  else
    Result := nil;
end;

procedure TdxGanttControlCustomController.DragAndDrop(const P: TPoint;
  var Accepted: Boolean);
begin
  UpdateHitTest(P.X, P.Y);
  if DragHelper <> nil then
    DragHelper.DragAndDrop(P, Accepted)
  else
    Accepted := False;
end;

procedure TdxGanttControlCustomController.DragDrop(Source: TObject;
  X, Y: Integer);
begin
  UpdateHitTest(X, Y);
  if DragHelper <> nil then
    DragHelper.DragDrop(Source, X, Y);
end;

procedure TdxGanttControlCustomController.DragOver(Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
begin
  UpdateHitTest(X, Y);
  if DragHelper <> nil then
    DragHelper.DragOver(Source, X, Y, State, Accept)
  else
    Accept := False;
end;

procedure TdxGanttControlCustomController.EndDrag(Target: TObject;
  X, Y: Integer);
begin
  UpdateHitTest(X, Y);
  if DragHelper <> nil then
    DragHelper.EndDrag(Target, X, Y);
end;

procedure TdxGanttControlCustomController.EndDragAndDrop(Accepted: Boolean);
begin
  if DragHelper <> nil then
    DragHelper.EndDragAndDrop(Accepted);
end;

function TdxGanttControlCustomController.GetDesignHitTest(X, Y: Integer;
  Shift: TShiftState): Boolean;
begin
  Result := False;
end;

function TdxGanttControlCustomController.GetDragObjectClass
  : TdxGanttControlDragObjectClass;
begin
  if DragHelper <> nil then
    Result := DragHelper.GetDragObjectClass
  else
    Result := nil;
end;

procedure TdxGanttControlCustomController.StartDrag(var DragObject
  : TDragObject);
begin
  if DragHelper <> nil then
    DragHelper.StartDrag(DragObject);
end;

function TdxGanttControlCustomController.StartDragAndDrop
  (const P: TPoint): Boolean;
begin
  UpdateHitTest(P.X, P.Y);
  Result := (DragHelper <> nil) and DragHelper.StartDragAndDrop(P);
end;

{ TdxGanttControlHistoryItem }

constructor TdxGanttControlHistoryItem.Create(AOwner: TdxGanttControlHistory);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TdxGanttControlHistoryItem.Redo;
begin
  Owner.Control.BeginUpdate;
  try
    DoRedo;
  finally
    Owner.Control.EndUpdate;
  end;
end;

procedure TdxGanttControlHistoryItem.Undo;
begin
  Owner.Control.BeginUpdate;
  try
    DoUndo;
  finally
    Owner.Control.EndUpdate;
  end;
end;

procedure TdxGanttControlHistoryItem.DoRedo;
begin
  // do nothing
end;

procedure TdxGanttControlHistoryItem.DoUndo;
begin
  // do nothing
end;

procedure TdxGanttControlHistoryItem.Execute;
begin
  Redo;
end;

{ TdxGanttControlCompositeHistoryItem }

constructor TdxGanttControlCompositeHistoryItem.Create
  (AHistory: TdxGanttControlHistory);
begin
  inherited Create(AHistory);
  FItems := TObjectList<TdxGanttControlHistoryItem>.Create;
end;

destructor TdxGanttControlCompositeHistoryItem.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TdxGanttControlCompositeHistoryItem.IsEmpty: Boolean;
begin
  Result := FItems.Count = 0;
end;

procedure TdxGanttControlCompositeHistoryItem.AddItem
  (AItem: TdxGanttControlHistoryItem);
begin
  FItems.Add(AItem);
end;

procedure TdxGanttControlCompositeHistoryItem.DoRedo;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    FItems[I].Redo;
end;

procedure TdxGanttControlCompositeHistoryItem.DoUndo;
var
  I: Integer;
begin
  for I := FItems.Count - 1 downto 0 do
    FItems[I].Undo;
end;

{ TdxGanttControlHistory }

constructor TdxGanttControlHistory.Create(AControl: TdxGanttControlBase);
begin
  inherited Create;
  FControl := AControl;
  FItems := TObjectList<TdxGanttControlHistoryItem>.Create;
  FCurrentIndex := -1;
end;

destructor TdxGanttControlHistory.Destroy;
begin
  FreeAndNil(FTransaction);
  FreeAndNil(FItems);
  inherited Destroy;
end;

procedure TdxGanttControlHistory.AddItem(AItem: TdxGanttControlHistoryItem);
begin
  if FTransactionCount > 0 then
    Transaction.AddItem(AItem)
  else
  begin
    while FCurrentIndex < FItems.Count - 1 do
      FItems.Delete(FItems.Count - 1);
    FItems.Add(AItem);
    CheckCapacity;
    FCurrentIndex := FItems.Count - 1;
  end;
end;

function TdxGanttControlHistory.CanRedo: Boolean;
begin
  Result := FControl.IsEditable and (FCurrentIndex < FItems.Count - 1);
end;

function TdxGanttControlHistory.CanUndo: Boolean;
begin
  Result := FControl.IsEditable and (FCurrentIndex > -1);
end;

procedure TdxGanttControlHistory.CheckCapacity;
begin
  if Capacity <> -1 then
    while Count > Capacity do
      FItems.Delete(0);
end;

procedure TdxGanttControlHistory.Clear;
begin
  FItems.Clear;
  FCurrentIndex := -1;
end;

procedure TdxGanttControlHistory.Undo;
begin
  if CanUndo then
  begin
    FControl.BeginUpdate;
    try
      FControl.Controller.HideEditing;
      FItems[FCurrentIndex].Undo;
      Dec(FCurrentIndex);
    finally
      FControl.EndUpdate;
    end;
  end;
end;

procedure TdxGanttControlHistory.Redo;
begin
  if CanRedo then
  begin
    FControl.BeginUpdate;
    try
      FControl.Controller.HideEditing;
      FItems[FCurrentIndex + 1].Redo;
      Inc(FCurrentIndex);
    finally
      FControl.EndUpdate;
    end;
  end;
end;

procedure TdxGanttControlHistory.BeginTransaction;
begin
  if FTransactionCount = 0 then
    FTransaction := TdxGanttControlCompositeHistoryItem.Create(Self);
  Inc(FTransactionCount);
end;

procedure TdxGanttControlHistory.CancelTransaction;
begin
  if Transaction = nil then
    Exit;
  FTransactionCount := 0;
  Transaction.Undo;
  FreeAndNil(FTransaction);
end;

procedure TdxGanttControlHistory.EndTransaction;
begin
  if Transaction = nil then
    Exit;
  Dec(FTransactionCount);
  if FTransactionCount = 0 then
  begin
    if Transaction.IsEmpty then
      FTransaction.Free
    else
      AddItem(FTransaction);
    FTransaction := nil;
  end;
end;

function TdxGanttControlHistory.GetCount: Integer;
begin
  Result := FItems.Count;
end;

{ TdxGanttControlBehaviorOptions }

constructor TdxGanttControlBehaviorOptions.Create(AOwner: TdxGanttControlBase);
begin
  inherited Create;
  FGanttControl := AOwner;
end;

procedure TdxGanttControlBehaviorOptions.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlBehaviorOptions;
begin
  if Source is TdxGanttControlBehaviorOptions then
  begin
    ASource := TdxGanttControlBehaviorOptions(Source);
    ConfirmDelete := ASource.ConfirmDelete;
    AlwaysShowEditor := ASource.AlwaysShowEditor;
  end;
  inherited Assign(Source);
end;

procedure TdxGanttControlBehaviorOptions.DoChanged;
begin
  // do nothing
end;

procedure TdxGanttControlBehaviorOptions.DoReset;
begin
  FConfirmDelete := False;
  FAlwaysShowEditor := False;
  FShowHints := True;
end;

{ TdxGanttControlHintController }

constructor TdxGanttControlHintController.Create(AOwner: TdxGanttControlBase);
begin
  inherited Create;
  FOwner := AOwner;
  FTimer := cxCreateTimer(TimerHandler, 1000, False);
  FHintWindow := TdxGanttControlHintWindow.Create(FOwner);
end;

destructor TdxGanttControlHintController.Destroy;
begin
  Hide;
  FreeAndNil(FTimer);
  FreeAndNil(FHintWindow);
  inherited Destroy;
end;

procedure TdxGanttControlHintController.Activate(AHitViewInfo
  : TdxGanttControlCustomItemViewInfo);
var
  R: TRect;
  AText: string;
begin
  if FViewInfo = AHitViewInfo then
    Exit;
  AText := AHitViewInfo.GetHintText;
  R := CalcHintRect(Max(GanttControl.ScaleFactor.Apply(MaxWidth),
    cxRectWidth(AHitViewInfo.Bounds)), AText, cxAlignBottom);
  DoActivate(R, AText);
  FViewInfo := AHitViewInfo;
end;

procedure TdxGanttControlHintController.DoActivate(const AHintRect: TRect;
  const AHintText: string; AImmediateHint: Boolean = False;
  AAutoHide: Boolean = True);
begin
  Hide;
  FHintText := AHintText;
  FHintRect := AHintRect;
  FAutoHide := AAutoHide;
  if AImmediateHint then
    ShowHint
  else
    StartShowHintTimer;
end;

function TdxGanttControlHintController.CalcHintRect(AMaxWidth: Integer;
  const AHintText: string; AFlags: Integer): TRect;
begin
  FHintFlags := AFlags;
  FHintWindow.Font := GanttControl.Font;
  Result := FHintWindow.CalcHintRect(AMaxWidth, AHintText, nil);
end;

procedure TdxGanttControlHintController.Hide;
begin
  HideHint;
end;

procedure TdxGanttControlHintController.Reset;
begin
  Hide;
end;

procedure TdxGanttControlHintController.MouseLeave;
begin
  GanttControl.MouseLeave(GanttControl);
  Reset;
end;

function TdxGanttControlHintController.CanShowHint: Boolean;
begin
  Result := not FLockHint and Application.Active and
    GanttControl.OptionsBehavior.ShowHints and
    (GanttControl.DragAndDropState = ddsNone);
end;

procedure TdxGanttControlHintController.HideHint;
begin
  StopTimer;
  if not Showing then
  begin
    if not FHideBeforeShow then
      FViewInfo := nil;
    Exit;
  end;
  EndMouseTracking(Self);
  FHintWindow.Hide;
  ShowWindow(FHintWindow.Handle, SW_HIDE);
  FShowing := False;
end;

procedure TdxGanttControlHintController.ShowHint;
begin
  FHideBeforeShow := True;
  HideHint;
  FHideBeforeShow := False;
  if not CanShowHint then
    Exit;
  FHintWindow.BiDiMode := GanttControl.BiDiMode;
  with GetMouseCursorPos do
  begin
    if FAutoHide then
    begin
      FHintRect := cxRectOffset(FHintRect, X, Y);
      if FHintFlags and cxAlignRight = cxAlignRight then
        FHintRect := cxRectSetRight(FHintRect, X);
      if FHintFlags and cxAlignBottom = cxAlignBottom then
        FHintRect := cxRectSetTop(FHintRect, Y + cxGetCursorSize.cy);
    end;
  end;
  if FHintWindow.UseRightToLeftAlignment then
    OffsetRect(FHintRect, -cxRectWidth(FHintRect), 0);
  FHintWindow.ActivateHint(FHintRect, FHintText);
  BeginMouseTracking(GanttControl, GanttControl.ClientBounds, Self);
  FShowing := True;
  if FShowing and FAutoHide then
    StartHideHintTimer;
end;

procedure TdxGanttControlHintController.StartHideHintTimer;
begin
  FTimer.Tag := 0;
  FTimer.Interval := Application.HintHidePause;
  FTimer.Enabled := True;
end;

procedure TdxGanttControlHintController.StartShowHintTimer;
begin
  FTimer.Tag := 1;
  FTimer.Interval := Max(Application.HintPause, 300);
  FTimer.Enabled := True;
end;

procedure TdxGanttControlHintController.StopTimer;
begin
  FTimer.Enabled := False;
end;

procedure TdxGanttControlHintController.TimerHandler(Sender: TObject);
begin
  if FTimer.Tag = 0 then
    HideHint
  else
    ShowHint;
end;

{ TdxGanttControlBase }

constructor TdxGanttControlBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Keys := [kAll .. kTab];
  FHistory := CreateHistory;
  DragMode := dmAutomatic;
end;

destructor TdxGanttControlBase.Destroy;
begin
  DestroySubClasses;
  FreeAndNil(FController);
  FreeAndNil(FHistory);
  inherited Destroy;
end;

function TdxGanttControlBase.AllowTouchScrollUIMode: Boolean;
begin
  Result := not IsDesigning;
end;

procedure TdxGanttControlBase.DoCreateScrollBars;
begin
  Controller.DoCreateScrollBars;
  inherited DoCreateScrollBars;
end;

procedure TdxGanttControlBase.DoDestroyScrollBars;
begin
  inherited DoDestroyScrollBars;
  Controller.DoDestroyScrollBars;
end;

function TdxGanttControlBase.GetMainScrollBarsClass
  : TcxControlCustomScrollBarsClass;
begin
  if IsPopupScrollBars or (ActualRenderMode = rmDirectX) then
    Result := TcxControlWindowedScrollBars
  else
    Result := TcxControlScrollBars;
end;

function TdxGanttControlBase.GetTouchScrollUIOwner(const APoint: TPoint)
  : IdxTouchScrollUIOwner;
begin
  Result := Controller.GetTouchScrollUIOwner(APoint);
end;

function TdxGanttControlBase.HasScrollBars: Boolean;
begin
  Result := True;
end;

function TdxGanttControlBase.InternalMouseWheel(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := Controller.MouseWheel(Shift, WheelDelta < 0,
    ScreenToClient(MousePos));
end;

procedure TdxGanttControlBase.BeginUpdate;
begin
  Inc(FLockCount);
  if FLockCount = 1 then
    History.BeginTransaction;
end;

procedure TdxGanttControlBase.BiDiModeChanged;
begin
  inherited BiDiModeChanged;
  ViewInfo.Reset;
  Changed([TdxGanttControlChangedType.Size]);
end;

procedure TdxGanttControlBase.BoundsChanged;
begin
  Changed([TdxGanttControlChangedType.Size]);
  inherited BoundsChanged;
end;

procedure TdxGanttControlBase.CancelUpdate;
begin
  if FLockCount = 1 then
    History.EndTransaction;
  Dec(FLockCount);
end;

procedure TdxGanttControlBase.Changed(AType: TdxGanttControlChangedTypes);
begin
  FChanges := FChanges + AType;
  CheckChanges;
end;

procedure TdxGanttControlBase.CheckChanges;
var
  AChanges: TdxGanttControlChangedTypes;
begin
  if IsUpdateLocked then
    Exit;
  AChanges := FChanges;
  FChanges := [];
  DoCheckChanges(AChanges);
end;

procedure TdxGanttControlBase.CreateActualCanvas;
begin
  inherited CreateActualCanvas;
  FViewInfo := CreateViewInfo;
  Changed([TdxGanttControlChangedType.Layout]);
end;

procedure TdxGanttControlBase.DoCheckChanges
  (AChanges: TdxGanttControlChangedTypes);
begin
  Controller.HitTest.Clear;
  if TdxGanttControlChangedType.Layout in AChanges then
    ViewInfo.CalculateLayout
  else if TdxGanttControlChangedType.Cache in AChanges then
    ViewInfo.Reset;
  if AChanges * [TdxGanttControlChangedType.Layout,
    TdxGanttControlChangedType.Size, TdxGanttControlChangedType.Data] <> [] then
    ViewInfo.Calculate(ClientBounds)
  else if TdxGanttControlChangedType.View in AChanges then
    ViewInfo.ViewChanged;
  if AChanges <> [] then
    Invalidate;
end;

procedure TdxGanttControlBase.DoInitialize;
begin
  FController := CreateController;
  FChanges := [TdxGanttControlChangedType.Layout];
  BorderStyle := cxcbsDefault;
  SetBounds(0, 0, ScaleFactor.Apply(350), ScaleFactor.Apply(250));
  CreateSubClasses;
end;

procedure TdxGanttControlBase.DoPaint;
begin
  inherited DoPaint;
  ViewInfo.Draw;
end;

procedure TdxGanttControlBase.EndUpdate;
begin
  if FLockCount = 1 then
    History.EndTransaction;
  Dec(FLockCount);
  CheckChanges;
end;

procedure TdxGanttControlBase.FontChanged;
begin
  inherited FontChanged;
  ViewInfo.Reset;
  Changed([TdxGanttControlChangedType.Size]);
end;

procedure TdxGanttControlBase.FreeActualCanvas;
begin
  FreeAndNil(FViewInfo);
  inherited FreeActualCanvas;
end;

procedure TdxGanttControlBase.CreateSubClasses;
begin
  FOptionsBehavior := TdxGanttControlBehaviorOptions.Create(Self);
  FHintController := TdxGanttControlHintController.Create(Self);
end;

procedure TdxGanttControlBase.DestroySubClasses;
begin
  FreeAndNil(FHintController);
  FreeAndNil(FOptionsBehavior);
end;

procedure TdxGanttControlBase.Initialize;
begin
  BeginUpdate;
  try
    DoInitialize;
  finally
    EndUpdate;
  end;
end;

function TdxGanttControlBase.CreateHistory: TdxGanttControlHistory;
begin
  Result := TdxGanttControlHistory.Create(Self);
end;

procedure TdxGanttControlBase.InitScrollBars;
begin
  inherited InitScrollBars;
  Controller.InitScrollBars;
end;

function TdxGanttControlBase.IsMouseWheelHandleNeeded(Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := Controller.IsMouseWheelHandleNeeded(MousePos);
end;

function TdxGanttControlBase.NeedsScrollBars: Boolean;
begin
  Result := False;
end;

procedure TdxGanttControlBase.SetOptionsBehavior(const Value
  : TdxGanttControlBehaviorOptions);
begin
  FOptionsBehavior.Assign(Value);
end;

function TdxGanttControlBase.CanDrag(X, Y: Integer): Boolean;
begin
  Result := inherited CanDrag(X, Y) and Controller.CanDrag(X, Y);
end;

function TdxGanttControlBase.CreateDragAndDropObject: TcxDragAndDropObject;
begin
  Result := Controller.CreateDragAndDropObject;
  if Result = nil then
    Result := inherited CreateDragAndDropObject;
end;

procedure TdxGanttControlBase.DoEndDrag(Target: TObject; X, Y: Integer);
begin
  Controller.EndDrag(Target, X, Y);
  inherited DoEndDrag(Target, X, Y);
end;

procedure TdxGanttControlBase.DoStartDrag(var DragObject: TDragObject);
begin
  inherited DoStartDrag(DragObject);
  Controller.StartDrag(DragObject);
end;

procedure TdxGanttControlBase.DragAndDrop(const P: TPoint;
  var Accepted: Boolean);
begin
  inherited DragAndDrop(P, Accepted);
  Controller.DragAndDrop(P, Accepted);
end;

procedure TdxGanttControlBase.DragOver(Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
begin
  inherited DragOver(Source, X, Y, State, Accept);
  Controller.DragOver(Source, X, Y, State, Accept);
end;

procedure TdxGanttControlBase.EndDragAndDrop(Accepted: Boolean);
begin
  inherited EndDragAndDrop(Accepted);
  Controller.EndDragAndDrop(Accepted);
end;

function TdxGanttControlBase.GetDesignHitTest(X, Y: Integer;
  Shift: TShiftState): Boolean;
begin
  Result := inherited GetDesignHitTest(X, Y, Shift) or
    Controller.GetDesignHitTest(X, Y, Shift);
end;

function TdxGanttControlBase.GetDragObjectClass: TDragControlObjectClass;
begin
  Result := Controller.GetDragObjectClass;
end;

function TdxGanttControlBase.StartDragAndDrop(const P: TPoint): Boolean;
begin
  Result := Controller.StartDragAndDrop(P);
end;

procedure TdxGanttControlBase.UnInitScrollbars;
begin
  inherited UnInitScrollbars;
  Controller.UnInitScrollbars;
end;

function TdxGanttControlBase.IsEditable: Boolean;
begin
  Result := Enabled;
end;

function TdxGanttControlBase.IsUpdateLocked: Boolean;
begin
  Result := (FLockCount > 0) or IsDestroying or IsLoading;
end;

procedure TdxGanttControlBase.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  if Key <> 0 then
    Controller.KeyDown(Key, Shift);
end;

procedure TdxGanttControlBase.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if Key <> #0 then
    Controller.KeyPress(Key);
end;

procedure TdxGanttControlBase.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited KeyUp(Key, Shift);
  if Key <> 0 then
    Controller.KeyUp(Key, Shift);
end;

procedure TdxGanttControlBase.Loaded;
begin
  inherited Loaded;
  CheckChanges;
end;

procedure TdxGanttControlBase.LookAndFeelChanged(Sender: TcxLookAndFeel;
  AChangedValues: TcxLookAndFeelValues);
begin
  inherited LookAndFeelChanged(Sender, AChangedValues);
  ViewInfo.Reset;
  Changed([TdxGanttControlChangedType.Size]);
end;

function TdxGanttControlBase.GetGestureClient(const APoint: TPoint)
  : IdxGestureClient;
begin
  Result := Controller.GetGestureClient(APoint);
  if Result = nil then
    Result := inherited GetGestureClient(APoint);
end;

procedure TdxGanttControlBase.Click;
begin
  Controller.Click;
  inherited Click;
end;

procedure TdxGanttControlBase.DblClick;
begin
  Controller.DblClick;
  inherited DblClick;
end;

procedure TdxGanttControlBase.CMDialogChar(var Message: TCMDialogChar);
begin
  if Focused then
  begin
    Message.Result := 1;
    Exit;
  end;
  inherited;
end;

procedure TdxGanttControlBase.CMNCSizeChanged(var Message: TMessage);
begin
  inherited;
  if Controller.ProcessNCSizeChanged then
    BoundsChanged;
end;

function TdxGanttControlBase.GetCurrentCursor(X: Integer; Y: Integer): TCursor;
begin
  Result := Controller.GetCurrentCursor(X, Y);
end;

procedure TdxGanttControlBase.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  Controller.MouseDown(Button, Shift, X, Y);
end;

procedure TdxGanttControlBase.MouseLeave(AControl: TControl);
begin
  Controller.MouseLeave;
  inherited MouseLeave(AControl);
end;

procedure TdxGanttControlBase.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  Controller.MouseMove(Shift, X, Y);
  inherited MouseMove(Shift, X, Y);
end;

procedure TdxGanttControlBase.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  Controller.MouseUp(Button, Shift, X, Y);
  inherited MouseUp(Button, Shift, X, Y);
end;

{ TdxGanttControlCustomDataProvider }

constructor TdxGanttControlCustomDataProvider.Create
  (AControl: TdxGanttControlBase);
begin
  inherited Create;
  FControl := AControl;
  FInnerList := TdxFastObjectList.Create(False);
  FCollapsedItems := TList.Create;
end;

destructor TdxGanttControlCustomDataProvider.Destroy;
begin
  FreeAndNil(FCollapsedItems);
  FreeAndNil(FInnerList);
  inherited Destroy;
end;

function TdxGanttControlCustomDataProvider.GetItem(AIndex: Integer): TObject;
begin
  Result := TObject(FInnerList[AIndex]);
end;

function TdxGanttControlCustomDataProvider.GetCount: Integer;
begin
  Result := FInnerList.Count;
end;

function TdxGanttControlCustomDataProvider.IndexOf(AItem: TObject): Integer;
begin
  Result := InternalIndexOf(AItem);
end;

function TdxGanttControlCustomDataProvider.InternalIndexOf
  (AItem: TObject): Integer;
begin
  for Result := 0 to Count - 1 do
    if Items[Result] = AItem then
      Exit;
  Result := -1;
end;

procedure TdxGanttControlCustomDataProvider.ClearItems;
begin
  FInnerList.Clear;
  FCollapsedItems.Clear;
end;

procedure TdxGanttControlCustomDataProvider.ItemRemoved(AItem: TObject);
begin
  FInnerList.Remove(AItem);
  FCollapsedItems.Remove(AItem);
  if not Control.IsUpdateLocked then
    Refresh(False);
end;

function TdxGanttControlCustomDataProvider.LastDataItem: TObject;
begin
  if DataItemCount = 0 then
    Result := nil
  else
    Result := DataItems[DataItemCount - 1];
end;

function TdxGanttControlCustomDataProvider.FirstDataItem: TObject;
begin
  if DataItemCount = 0 then
    Result := nil
  else
    Result := DataItems[0];
end;

procedure TdxGanttControlCustomDataProvider.Refresh(AForce: Boolean);
begin
  FInnerList.Clear;
  Populate(AForce);
end;

procedure TdxGanttControlCustomDataProvider.Expand(AItem: TObject);
begin
  if CanExpand(AItem) then
  begin
    FCollapsedItems.Remove(AItem);
    Control.Changed([TdxGanttControlChangedType.Data]);
  end;
end;

procedure TdxGanttControlCustomDataProvider.Collapse(AItem: TObject);
begin
  if CanCollapse(AItem) then
  begin
    FCollapsedItems.Add(AItem);
    Control.Changed([TdxGanttControlChangedType.Data]);
  end;
end;

function TdxGanttControlCustomDataProvider.IsExpanded(AItem: TObject): Boolean;
begin
  Result := FCollapsedItems.IndexOf(AItem) = -1;
end;

function TdxGanttControlCustomDataProvider.CanExpand(AItem: TObject): Boolean;
begin
  Result := not IsExpanded(AItem);
end;

function TdxGanttControlCustomDataProvider.CanCollapse(AItem: TObject): Boolean;
begin
  Result := IsExpanded(AItem);
end;

function TdxGanttControlCustomDataProvider.CanAddItem(AItem: TObject): Boolean;
begin
  Result := True;
end;

procedure TdxGanttControlCustomDataProvider.DoPopulate;
var
  I: Integer;
begin
  for I := 0 to DataItemCount - 1 do
    if CanAddItem(DataItems[I]) then
      FInnerList.Add(DataItems[I]);
end;

procedure TdxGanttControlCustomDataProvider.Populate(AForce: Boolean);
begin
  if (Control.IsUpdateLocked and not AForce) or Control.IsDestroying then
    Exit;
  DoPopulate;
end;

end.
