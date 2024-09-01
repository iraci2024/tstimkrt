{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressCommonLibrary }
{ }
{ Copyright (c) 1998-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSCOMMONLIBRARY AND ALL }
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

unit cxScrollBar;

{$I cxVer.inc}

interface

uses
  Windows, Forms, Messages, Types, SysUtils, Classes, Controls, StdCtrls,
  Graphics,
  dxTouch, dxThemeManager, cxGraphics, cxClasses, cxLookAndFeels,
  cxLookAndFeelPainters,
  dxFading, dxCoreClasses, cxGeometry, dxCoreGraphics, dxScrollbarAnnotations,
  Generics.Defaults, Generics.Collections, dxTypeHelpers, dxGdiPlusClasses,
  dxCustomHint;

const
  cxScrollInitialInterval = 400;
  cxScrollInterval = 60;
  cxScrollMinDistance: Integer = 34;
  cxScrollMaxDistance: Integer = 136;
  cxTouchScrollbarSize: TSize = (cx: 16; cy: 16);
  cxHybridScrollbarMaxSize: TSize = (cx: 16; cy: 16);
  cxHybridScrollbarMinSize: TSize = (cx: 6; cy: 6);

type
  TcxScrollBar = class;
  TcxScrollBarHelper = class;
  TcxScrollBarController = class;
  TcxScrollBarViewInfo = class;
  TcxScrollBarPartViewInfo = class;
  TcxScrollBarPainter = class;

  { IcxScrollBarOwner }

  IcxScrollBarOwner = interface
    ['{56771164-C253-40FF-B6D4-2A29D0C90236}']
    function GetControl: TWinControl;
    function GetLookAndFeel: TcxLookAndFeel;
    function GetScaleFactor: TdxScaleFactor;
  end;

  IcxScrollBarOwner2 = interface
    ['{39D5E864-7166-4168-A6C8-AC85303CF088}']
    function GetOrigin: TPoint;
    function GetVisible: Boolean;
    function GetVisibleRect: TRect;
    procedure SetFocusByMouse;
  end;

  IcxScrollBarOwner2Provider = interface
    ['{5CAEC427-0B33-41D2-8839-5BE9E0920BD4}']
    function GetOwner2: IcxScrollBarOwner2;
  end;

  { TcxScrollBarPartFadingHelper }

  TcxScrollBarPartFadingHelper = class(TdxFadingObjectHelper)
  private
    FOwner: TcxScrollBarPartViewInfo;
    FState: TcxButtonState;
  protected
    // IdxFadingObject
    function CanFade: Boolean; override;
    procedure DrawFadeImage; override;
    procedure GetFadingImages(out AFadeOutImage, AFadeInImage
      : TcxBitmap); override;
  public
    constructor Create(AOwner: TcxScrollBarPartViewInfo);
    procedure UpdateState;
    //
    property Owner: TcxScrollBarPartViewInfo read FOwner;
  end;

  { TcxScrollBarPartViewInfo }

  TcxScrollBarPartViewInfo = class
  private
    FOwner: TcxScrollBarViewInfo;
    FBounds: TRect;
    FFadingHelper: TcxScrollBarPartFadingHelper;
    FPart: TcxScrollBarPart;
    FState: TcxButtonState;
    function GetPainter: TcxScrollBarPainter;
    function GetState: TcxButtonState;
    procedure SetState(AValue: TcxButtonState);
  protected
    function CanFade: Boolean;
    procedure Invalidate;
    procedure UpdateFadingHelperState;

    property FadingHelper: TcxScrollBarPartFadingHelper read FFadingHelper;
    property Owner: TcxScrollBarViewInfo read FOwner;
    property Painter: TcxScrollBarPainter read GetPainter;
  public
    constructor Create(AOwner: TcxScrollBarViewInfo; APart: TcxScrollBarPart);
    destructor Destroy; override;
    property Bounds: TRect read FBounds write FBounds;
    property Part: TcxScrollBarPart read FPart;
    property State: TcxButtonState read GetState write SetState;
  end;

  TcxHorizontalScrollBarPart = (spbmLineLeft, spbmLineRight, spbmPageLeft,
    spbmPageRight);

  TcxScrollbarAnnotationInfo = class
  strict private
    FBounds: TRect;
    FColor: TdxAlphaColor;
    FRecordIndexes: TdxScrollbarAnnotationRowIndexList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Bounds: TRect read FBounds write FBounds;
    property Color: TdxAlphaColor read FColor write FColor;
    property RecordIndexes: TdxScrollbarAnnotationRowIndexList
      read FRecordIndexes;
  end;

  TcxScrollBarAnnotationHintableObject = class(TcxIUnknownObject,
    IcxHintableObject)
  strict private
    FBounds: TRect;
  public
    function HasHintPoint(const P: TPoint): Boolean;
    function IsHintAtMousePos: Boolean;
    function UseHintHidePause: Boolean;
    property Bounds: TRect read FBounds write FBounds;
  end;

  TcxScrollbarAnnotationInfos = class
  strict private
    FBounds: TRect;
    FChangedAnnotationKinds: TdxScrollbarAnnotationKinds;
    FInfos: TObjectDictionary<TdxScrollbarAnnotationKind,
      TObjectList<TcxScrollbarAnnotationInfo>>;
    FScrollbar: TcxScrollBarHelper;
    function GetAnnotationList(AKind: TdxScrollbarAnnotationKind)
      : TObjectList<TcxScrollbarAnnotationInfo>;
  protected
    procedure CreateItems(AKind: TdxScrollbarAnnotationKind);
    property ChangedAnnotationKinds: TdxScrollbarAnnotationKinds
      read FChangedAnnotationKinds write FChangedAnnotationKinds;
  public
    constructor Create(AOwner: TcxScrollBarHelper);
    destructor Destroy; override;
    procedure Calculate(const ABounds: TRect); virtual;
    procedure Invalidate;
    property Infos: TObjectDictionary < TdxScrollbarAnnotationKind,
      TObjectList < TcxScrollbarAnnotationInfo >> read FInfos;
    property Bounds: TRect read FBounds;
  end;

  { TcxScrollBarViewInfo }

  TcxScrollBarViewInfo = class
  private
    FAnnotationInfos: TcxScrollbarAnnotationInfos;
    FAnnotationInfosBounds: TRect;
    FArrowButtonLength: Integer;
    FDragThumbnailPosX: Integer;
    FDragThumbnailPosY: Integer;
    FKind: TScrollBarKind;
    FLength: Integer;
    FHotPart: TcxScrollBarPart;
    FMinThumbnailSize: Integer;
    FPartInfos: array [sbpLineUp .. sbpPageDown] of TcxScrollBarPartViewInfo;
    FPressedPart: TcxScrollBarPart;
    FScrollbar: TcxScrollBarHelper;
    procedure CheckKind;
    function GetLength: Integer;
    function GetHeight: Integer;
    function GetPartInfo(APartType: TcxScrollBarPart): TcxScrollBarPartViewInfo;
    procedure SetHotPart(AValue: TcxScrollBarPart);
    procedure SetPressedPart(AValue: TcxScrollBarPart);
    //
    function GetKind: TScrollBarKind;
    function GetMax: Integer;
    function GetMin: Integer;
    function GetMinThumbnailSize: Integer;
    function GetPageSize: Integer;
    function GetPosition: Integer;
    function GetScaleFactor: TdxScaleFactor;
    function IsEnabled: Boolean;
  protected
    procedure CalculateAnnotationInfos;
    function CreateAnnotationInfos: TcxScrollbarAnnotationInfos; virtual;
    procedure CreatePartInfos;
    procedure DestroyPartInfos;
    function GetArrowButtonLength: Integer; virtual;
    function GetBounds: TRect; virtual;
    function GetHorizontalScrollbarPart(APartType: TcxHorizontalScrollBarPart)
      : TcxScrollBarPartViewInfo;
    function GetPart(const P: TPoint): TcxScrollBarPart;
    procedure UpdateAnnotationInfos;
    procedure UpdateFadingHelperStates;

    property ScrollBar: TcxScrollBarHelper read FScrollbar;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  public
    constructor Create(AScrollBar: TcxScrollBarHelper); virtual;
    destructor Destroy; override;
    procedure AdjustPageRects;
    procedure Calculate; virtual;
    procedure CalculateThumbnailRect;
    procedure SetThumbnailPos(APos: Integer);
    //
    property AnnotationsInfo: TcxScrollbarAnnotationInfos read FAnnotationInfos;
    property Bounds: TRect read GetBounds;
    property HotPart: TcxScrollBarPart read FHotPart write SetHotPart;
    property PartInfo[APartType: TcxScrollBarPart]: TcxScrollBarPartViewInfo
      read GetPartInfo;
    property PressedPart: TcxScrollBarPart read FPressedPart
      write SetPressedPart;
  end;

  TcxScrollBarViewInfoClass = class of TcxScrollBarViewInfo;

  { TcxScrollBarHintHelper }

  TcxScrollBarHintHelper = class(TcxCustomHintHelper)
  private
    FController: TcxScrollBarController;
  protected
    procedure CorrectHintWindowRect(var ARect: TRect); override;
    function GetOwnerWinControl: TWinControl; override;
    property Controller: TcxScrollBarController read FController;
  public
    constructor Create(AController: TcxScrollBarController);
  end;

  { TcxScrollBarController }

  TcxScrollBarController = class
  private
    FAnnotationsHintableObject: TcxScrollBarAnnotationHintableObject;
    FDownMousePos: TPoint;
    FHintHelper: TcxScrollBarHintHelper;
    FHotAnnotationRowIndex: Integer;
    FHotAnnotationRowIndexLists: TdxScrollbarAnnotationRowIndexLists;
    FLastMousePos: TPoint;
    FSavePosition: Integer;
    FSaveThumbnailPos: TPoint;
    FScrollbar: TcxScrollBarHelper;
    FTimer: TcxTimer;
    procedure CheckHint;
    procedure CheckHotAnnotations(APos: TPoint);
    procedure FinishScrolling(AHotPart: TcxScrollBarPart = sbpNone);
    function GetPositionFromThumbnail: Integer;
    function GetViewInfo: TcxScrollBarViewInfo;
    procedure InternalScroll(AScrollCode: TScrollCode);
    function IsButtonHotTrack: Boolean;
    procedure OnTimer(Sender: TObject);
  protected
    function CreateHintHelper: TcxScrollBarHintHelper; virtual;
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); virtual;
    procedure DoMouseEnter(AControl: TControl); virtual;
    procedure DoMouseLeave(AControl: TControl); virtual;
    procedure DoMouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); virtual;
    procedure DoScroll(APart: TcxScrollBarPart);
    procedure SetPositionValue(AScrollCode: TScrollCode; AValue: Integer);
    procedure SetThumbnailValue(AValue: Integer; AUseSetter: Boolean); virtual;

    property HotAnnotationRowIndex: Integer read FHotAnnotationRowIndex;
    property ScrollBar: TcxScrollBarHelper read FScrollbar;
    property ViewInfo: TcxScrollBarViewInfo read GetViewInfo;
  public
    constructor Create(AScrollBar: TcxScrollBarHelper); virtual;
    destructor Destroy; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer);
    procedure MouseEnter(AControl: TControl);
    procedure MouseLeave(AControl: TControl);
    procedure MouseMove(Shift: TShiftState; X, Y: Integer);
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  end;

  TcxScrollBarControllerClass = class of TcxScrollBarController;

  { TcxScrollBarPainter }

  TcxScrollBarPainter = class
  private
    FScrollbar: TcxScrollBarHelper;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
    function GetScaleFactor: TdxScaleFactor;
    function GetViewInfo: TcxScrollBarViewInfo;
  protected
    procedure DoDrawScrollBarPart(ACanvas: TcxCanvas; const R: TRect;
      APart: TcxScrollBarPart; AState: TcxButtonState); virtual;
    procedure DrawScrollbarAnnotation(AGPCanvas: TdxGPGraphics;
      AInfo: TcxScrollbarAnnotationInfo); virtual;
    procedure DoDrawScrollbarAnnotations(AGPCanvas: TdxGPGraphics); virtual;
    procedure DrawScrollbarAnnotations(ACanvas: TcxCanvas); virtual;
    procedure DrawScrollBarBackground(ACanvas: TcxCanvas;
      const R: TRect); virtual;
    procedure DrawScrollBarPart(ACanvas: TcxCanvas;
      APart: TcxScrollBarPart); virtual;
    function FadingAvailable: Boolean; virtual;
    function GetMinThumbnailSize: Integer; virtual;

    property LookAndFeelPainter: TcxCustomLookAndFeelPainter
      read GetLookAndFeelPainter;
    property ScrollBar: TcxScrollBarHelper read FScrollbar;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property ViewInfo: TcxScrollBarViewInfo read GetViewInfo;
  public
    constructor Create(AScrollBar: TcxScrollBarHelper); virtual;
    destructor Destroy; override;
    function IsButtonHotTrack: Boolean; virtual;
    procedure Paint(ACanvas: TcxCanvas); virtual;
  end;

  TcxScrollBarPainterClass = class of TcxScrollBarPainter;

  { TcxScrollBarHelper }

  TcxScrollBarHelper = class(TcxIUnknownObject, IdxScrollbarAnnotationRenderer)
  private
    FAnnotations: TdxScrollbarAnnotations;
    FBoundsRect: TRect;
    FController: TcxScrollBarController;
    FEnabled: Boolean;
    FIsNonClient: Boolean;
    FKind: TScrollBarKind;
    FLargeChange: TScrollBarInc;
    FMax: Integer;
    FMin: Integer;
    FOwner: IcxScrollBarOwner;
    FOwner2: IcxScrollBarOwner2;
    FPageSize: Integer;
    FPainter: TcxScrollBarPainter;
    FPosition: Integer;
    FSmallChange: TScrollBarInc;
    FUnlimitedTracking: Boolean;
    FViewInfo: TcxScrollBarViewInfo;
    FVisible: Boolean;
    FVisibleRgn: HRGN;

    FOnChange: TNotifyEvent;
    FOnScroll: TScrollEvent;

    procedure ClearRegion;
    function GetAllowOwnerScrollOnDrag: Boolean;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
    function GetLookAndFeel: TcxLookAndFeel;
    function GetTempVisibleRegion: TcxRegion;
    procedure RefreshNCPart(const ARect: TRect);
    procedure SetVisibleRgn(const Value: HRGN);
  protected
    function GetBoundsRect: TRect;
    function GetEnabled: Boolean;
    function GetKind: TScrollBarKind;
    function GetLargeChange: TScrollBarInc;
    function GetMax: Integer;
    function GetMin: Integer;
    function GetPageSize: Integer;
    function GetPosition: Integer;
    function GetSmallChange: TScrollBarInc;
    function GetUnlimitedTracking: Boolean;
    function GetVisible: Boolean;
    procedure SetBoundsRect(const AValue: TRect);
    procedure SetEnabled(AValue: Boolean);
    procedure SetKind(Value: TScrollBarKind);
    procedure SetLargeChange(Value: TScrollBarInc);
    procedure SetMax(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetPageSize(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure SetVisible(AValue: Boolean);
    procedure SetSmallChange(Value: TScrollBarInc);
    procedure SetUnlimitedTracking(Value: Boolean);

    procedure Change; virtual;
    procedure DoBufferedPaint(ADC: HDC);
    procedure DoPaint(ACanvas: TcxCanvas); virtual;
    function GetControllerClass: TcxScrollBarControllerClass; virtual;
    function GetDefaultScrollbarAnnotationColor(AAnnotationKind
      : TdxScrollbarAnnotationKind): TdxAlphaColor; virtual;
    function GetPainterClass: TcxScrollBarPainterClass; virtual;
    function GetScaleFactor: TdxScaleFactor;
    function GetScrollBarSize: TSize; virtual;
    function GetViewInfoClass: TcxScrollBarViewInfoClass; virtual;
    function Handle: THandle;
    function HandleAllocated: Boolean;
    procedure Invalidate; virtual;
    procedure InvalidateRect(const ARect: TRect;
      AEraseBackground: Boolean = False); virtual;
    function IsOwnerControlCapture: Boolean; virtual;
    function IsRightToLeft: Boolean; virtual;
    procedure MoveToCurrentAnnotation;
    function NeedBuffering: Boolean; virtual;
    function PtInVisibleRgn(const APoint: TPoint): Boolean;
    function ScreenToClient(const APoint: TPoint): TPoint; virtual;
    procedure Scroll(ScrollCode: TScrollCode; var ScrollPos: Integer); virtual;
    procedure TranslateCoords(var X, Y: Integer);

    property AllowOwnerScrollOnDrag: Boolean read GetAllowOwnerScrollOnDrag;
    property Annotations: TdxScrollbarAnnotations read FAnnotations;
    property Controller: TcxScrollBarController read FController;
    property LookAndFeelPainter: TcxCustomLookAndFeelPainter
      read GetLookAndFeelPainter;
    property Owner: IcxScrollBarOwner read FOwner;
    property Owner2: IcxScrollBarOwner2 read FOwner2;
    property Painter: TcxScrollBarPainter read FPainter;
    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
    property ViewInfo: TcxScrollBarViewInfo read FViewInfo;
    property VisibleRgn: HRGN read FVisibleRgn write SetVisibleRgn;
  public
    constructor Create(AOwner: IcxScrollBarOwner); virtual;
    destructor Destroy; override;
    procedure Calculate; virtual;
    procedure CancelMode; virtual;
    function GetOwnerOrigin: TPoint; virtual;
    function GetOwnerVisible: Boolean; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); virtual;
    procedure MouseEnter(AControl: TControl); virtual;
    procedure MouseLeave(AControl: TControl); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); virtual;
    procedure Paint(ACanvas: TcxCanvas); overload; virtual;
    procedure Paint(ADC: HDC); overload; virtual;
    procedure Repaint; virtual;
    procedure SetScrollParams(AMin, AMax, APosition, APageSize: Integer;
      ARedraw: Boolean = True);
    // ScrollbarAnnotations
    procedure IdxScrollbarAnnotationRenderer.Invalidate =
      ScrollbarAnnotationsInvalidate;
    procedure IdxScrollbarAnnotationRenderer.Update =
      ScrollbarAnnotationsUpdate;
    procedure ScrollbarAnnotationsInvalidate(AAnnotationKinds
      : TdxScrollbarAnnotationKinds = dxAllScrollbarAnnotationKinds);
    procedure ScrollbarAnnotationsUpdate;
    procedure SetAnnotations(AAnnotations: TdxScrollbarAnnotations);

    property BoundsRect: TRect read GetBoundsRect write SetBoundsRect;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property IsNonClient: Boolean read FIsNonClient write FIsNonClient;
    property Kind: TScrollBarKind read GetKind write SetKind;
    property LargeChange: TScrollBarInc read GetLargeChange
      write SetLargeChange;
    property LookAndFeel: TcxLookAndFeel read GetLookAndFeel;
    property Max: Integer read GetMax write SetMax;
    property Min: Integer read GetMin write SetMin;
    property PageSize: Integer read GetPageSize write SetPageSize;
    property Position: Integer read GetPosition write SetPosition;
    property SmallChange: TScrollBarInc read GetSmallChange
      write SetSmallChange;
    property UnlimitedTracking: Boolean read GetUnlimitedTracking
      write SetUnlimitedTracking;
    property Visible: Boolean read GetVisible write SetVisible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnScroll: TScrollEvent read FOnScroll write FOnScroll;
  end;

  TcxScrollBarHelperClass = class of TcxScrollBarHelper;

  { TcxScrollBar }

  TcxScrollBar = class(TCustomControl, IdxScaleFactor, IcxScrollBarOwner,
    IdxSkinSupport, IdxScrollbarAnnotationRenderer)
  private
    FHelper: TcxScrollBarHelper;
    FLookAndFeel: TcxLookAndFeel;
    FRealCtl3D: Boolean;
    FScaleFactor: TdxOwnedScaleFactor;
    FThemeChangedNotificator: TdxThemeChangedNotificator;

    FOnChange: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnScroll: TScrollEvent;

    function GetCtl3D: Boolean;
    function GetInternalCtl3D: Boolean;
    function IsCtl3DStored: Boolean;
    procedure SetCtl3D(Value: Boolean);
    procedure SetInternalCtl3D(Value: Boolean);
    procedure ThemeChanged;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMEnabledChanged(var Message: TMessage);
      message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMSysColorChange(var Message: TMessage);
      message CM_SYSCOLORCHANGE;
    procedure CMVisibleChanged(var Message: TMessage);
      message CM_VISIBLECHANGED;
    procedure CNHScroll(var Message: TWMHScroll); message CN_HSCROLL;
    procedure CNVScroll(var Message: TWMVScroll); message CN_VSCROLL;
    procedure CNCtlColorScrollBar(var Message: TMessage);
      message CN_CTLCOLORSCROLLBAR;
    procedure WMCancelMode(var Message: TWMCancelMode); message WM_CANCELMODE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMGestureNotify(var Message: TWMGestureNotify);
      message WM_GESTURENOTIFY;
  protected
{$IFDEF DELPHIBERLIN}
    procedure ChangeScale(M, D: Integer; isDpiChange: Boolean); override;
{$ELSE}
    procedure ChangeScale(M, D: Integer); override;
{$ENDIF}
    procedure DoChange(Sender: TObject);
    procedure DoScrollEvent(Sender: TObject; ScrollCode: TScrollCode;
      var ScrollPos: Integer);
    procedure Change; virtual;
    function GetInitialSize: TSize; virtual;
    function GetHelperClass: TcxScrollBarHelperClass; virtual;
    function GetPainter: TcxCustomLookAndFeelPainter;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel;
      AChangedValues: TcxLookAndFeelValues); virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseEnter(AControl: TControl); dynamic;
    procedure MouseLeave(AControl: TControl); dynamic;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    // IcxScrollBarOwner
    function GetControl: TWinControl;
    function GetLookAndFeel: TcxLookAndFeel;
    function GetScaleFactor: TdxScaleFactor;

    function GetKind: TScrollBarKind;
    function GetLargeChange: TScrollBarInc;
    function GetMax: Integer;
    function GetMin: Integer;
    function GetPageSize: Integer;
    function GetPosition: Integer;
    function GetSmallChange: TScrollBarInc;
    function GetUnlimitedTracking: Boolean;
    procedure SetKind(Value: TScrollBarKind);
    procedure SetLargeChange(AValue: TScrollBarInc);
    procedure SetLookAndFeel(Value: TcxLookAndFeel);
    procedure SetMax(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetParent(AValue: TWinControl); override;
    procedure SetPageSize(Value: Integer);
    procedure SetPosition(Value: Integer);
    procedure SetSmallChange(AValue: TScrollBarInc);
    procedure SetUnlimitedTracking(AValue: Boolean);

    // ScrollbarAnnotations
    procedure IdxScrollbarAnnotationRenderer.Invalidate =
      ScrollbarAnnotationsInvalidate;
    procedure ScrollbarAnnotationsInvalidate(AAnnotationKinds
      : TdxScrollbarAnnotationKinds = dxAllScrollbarAnnotationKinds);
    procedure SetAnnotations(AAnnotations: TdxScrollbarAnnotations);
    procedure IdxScrollbarAnnotationRenderer.Update =
      ScrollbarAnnotationsUpdate;
    procedure ScrollbarAnnotationsUpdate;

    procedure DoPaint(ACanvas: TcxCanvas); virtual;
    procedure Paint; override;
    procedure Resize; override;
    procedure Scroll(ScrollCode: TScrollCode; var ScrollPos: Integer); virtual;

    property Helper: TcxScrollBarHelper read FHelper;
    property InternalCtl3D: Boolean read GetInternalCtl3D
      write SetInternalCtl3D;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
    property ScaleFactor: TdxOwnedScaleFactor read FScaleFactor;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetScrollParams(AMin, AMax, APosition, APageSize: Integer;
      ARedraw: Boolean = True);
    procedure SetParams(APosition, AMin, AMax: Integer);
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property Ctl3D read GetCtl3D write SetCtl3D stored IsCtl3DStored;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Kind: TScrollBarKind read GetKind write SetKind
      default sbHorizontal;
    property LargeChange: TScrollBarInc read GetLargeChange write SetLargeChange
      default 1;
    property LookAndFeel: TcxLookAndFeel read FLookAndFeel write SetLookAndFeel;
    property Max: Integer read GetMax write SetMax default 100;
    property Min: Integer read GetMin write SetMin default 0;
    property PageSize: Integer read GetPageSize write SetPageSize;
    property ParentBiDiMode;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property Position: Integer read GetPosition write SetPosition default 0;
    property ShowHint;
    property SmallChange: TScrollBarInc read GetSmallChange write SetSmallChange
      default 1;
    property UnlimitedTracking: Boolean read GetUnlimitedTracking
      write SetUnlimitedTracking default False;
    property Visible;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnScroll: TScrollEvent read FOnScroll write FOnScroll;
    property OnStartDock;
    property OnStartDrag;
  end;

  TcxScrollBarClass = class of TcxScrollBar;

function GetScaledScrollBarSize(AScaleFactor: TdxScaleFactor): TSize;
function GetScrollBarSize: TSize;
function GetTouchScrollBarSize(AScaleFactor: TdxScaleFactor): TSize;
function GetHybridScrollBarSize(AScaleFactor: TdxScaleFactor;
  AExpanded: Boolean): TSize;

implementation

uses
  Consts, Math, dxCore, dxUxTheme, dxThemeConsts, cxControls, dxDPIAwareUtils;

const
  cxTimerParts = [sbpLineUp, sbpLineDown, sbpPageUp, sbpPageDown];
  cxFadingParts = [sbpLineUp, sbpLineDown, sbpThumbnail];

type
  TcxControlAccess = class(TcxControl);

function GetScaledScrollBarSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  Result.cx := dxGetSystemMetrics(SM_CXVSCROLL, AScaleFactor);
  Result.cy := dxGetSystemMetrics(SM_CYHSCROLL, AScaleFactor);
end;

function GetTouchScrollBarSize(AScaleFactor: TdxScaleFactor): TSize;
begin
  if AScaleFactor <> nil then
    Result := AScaleFactor.Apply(cxTouchScrollbarSize)
  else
    Result := dxSystemScaleFactor.Apply(cxTouchScrollbarSize);
end;

function GetHybridScrollBarSize(AScaleFactor: TdxScaleFactor;
  AExpanded: Boolean): TSize;

  function HybridScrollbarSize(AExpanded: Boolean): TSize;
  begin
    if AExpanded then
      Result := cxHybridScrollbarMaxSize
    else
      Result := cxHybridScrollbarMinSize;
  end;

begin
  if AScaleFactor <> nil then
    Result := AScaleFactor.Apply(HybridScrollbarSize(AExpanded))
  else
    Result := dxSystemScaleFactor.Apply(HybridScrollbarSize(AExpanded));
end;

function GetScrollBarSize: TSize;
begin
  Result := GetScaledScrollBarSize(dxSystemScaleFactor);
end;

{ TcxScrollBarPartFadingHelper }

constructor TcxScrollBarPartFadingHelper.Create
  (AOwner: TcxScrollBarPartViewInfo);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TcxScrollBarPartFadingHelper.CanFade: Boolean;
begin
  Result := Owner.CanFade;
end;

procedure TcxScrollBarPartFadingHelper.DrawFadeImage;
begin
  Owner.Invalidate;
end;

procedure TcxScrollBarPartFadingHelper.GetFadingImages(out AFadeOutImage,
  AFadeInImage: TcxBitmap);

  function PrepareFadingImage(AState: TcxButtonState): TcxBitmap32;
  begin
    Result := TcxBitmap32.CreateSize(Owner.Bounds, True);
    Result.Canvas.Lock;
    Owner.Painter.DoDrawScrollBarPart(Result.cxCanvas, Result.ClientRect,
      FOwner.Part, AState);
    Result.Canvas.Unlock;
  end;

begin
  AFadeOutImage := PrepareFadingImage(cxbsNormal);
  AFadeInImage := PrepareFadingImage(cxbsHot);
end;

procedure TcxScrollBarPartFadingHelper.UpdateState;
var
  AState: TcxButtonState;
begin
  AState := Owner.State;
  if AState <> FState then
  begin
    CheckStartFading(FState, AState);
    FState := AState;
  end;
end;

{ TcxScrollBarPartViewInfo }

constructor TcxScrollBarPartViewInfo.Create(AOwner: TcxScrollBarViewInfo;
  APart: TcxScrollBarPart);
begin
  inherited Create;
  FOwner := AOwner;
  FState := cxbsNormal;
  FPart := APart;
  FBounds := cxEmptyRect;
  if FPart in cxFadingParts then
    FFadingHelper := TcxScrollBarPartFadingHelper.Create(Self);
end;

destructor TcxScrollBarPartViewInfo.Destroy;
begin
  FreeAndNil(FFadingHelper);
  inherited;
end;

function TcxScrollBarPartViewInfo.CanFade: Boolean;
begin
  Result := Painter.FadingAvailable;
end;

procedure TcxScrollBarPartViewInfo.Invalidate;
begin
  Owner.ScrollBar.InvalidateRect(Bounds);
end;

procedure TcxScrollBarPartViewInfo.UpdateFadingHelperState;
begin
  if FadingHelper <> nil then
    FadingHelper.UpdateState;
end;

function TcxScrollBarPartViewInfo.GetPainter: TcxScrollBarPainter;
begin
  Result := FOwner.ScrollBar.Painter;
end;

function TcxScrollBarPartViewInfo.GetState: TcxButtonState;
const
  PartPressedStateMap: array [Boolean] of TcxButtonState = (cxbsNormal,
    cxbsPressed);
begin
  if not FOwner.IsEnabled then
    Result := cxbsDisabled
  else if ((FPart <> sbpThumbnail) or ((FPart = sbpThumbnail) and
    Painter.IsButtonHotTrack)) and (FPart = FOwner.HotPart) then
    if FOwner.PressedPart = sbpNone then
      Result := cxbsHot
    else
      Result := PartPressedStateMap[FPart = FOwner.PressedPart]
  else
    Result := cxbsNormal;

  // if FOwner.IsEnabled then
  // Result := FState
  // else
  // Result := cxbsDisabled;
end;

procedure TcxScrollBarPartViewInfo.SetState(AValue: TcxButtonState);
begin
  FState := AValue;
end;

{ TcxScrollbarAnnotationInfo }

constructor TcxScrollbarAnnotationInfo.Create;
begin
  inherited Create;
  FRecordIndexes := TdxScrollbarAnnotationRowIndexList.Create;
end;

destructor TcxScrollbarAnnotationInfo.Destroy;
begin
  FreeAndNil(FRecordIndexes);
  inherited;
end;

{ TcxScrollBarAnnotationHintableObject }

function TcxScrollBarAnnotationHintableObject.HasHintPoint
  (const P: TPoint): Boolean;
begin
  Result := PtInRect(FBounds, P);
end;

function TcxScrollBarAnnotationHintableObject.IsHintAtMousePos: Boolean;
begin
  Result := True;
end;

function TcxScrollBarAnnotationHintableObject.UseHintHidePause: Boolean;
begin
  Result := False;
end;

{ TcxScrollbarAnnotationInfos }

constructor TcxScrollbarAnnotationInfos.Create(AOwner: TcxScrollBarHelper);
begin
  inherited Create;
  FScrollbar := AOwner;
  FInfos := TObjectDictionary < TdxScrollbarAnnotationKind,
    TObjectList < TcxScrollbarAnnotationInfo >>.Create([doOwnsValues]);
end;

destructor TcxScrollbarAnnotationInfos.Destroy;
begin
  FreeAndNil(FInfos);
  inherited Destroy;
end;

procedure TcxScrollbarAnnotationInfos.Calculate(const ABounds: TRect);
var
  AKind: TdxScrollbarAnnotationKind;
  AAnnotations: TdxScrollbarAnnotations;
  AKeys: TArray<TdxScrollbarAnnotationKind>;
{$IFNDEF DELPHIXE}
  I: Integer;
{$ENDIF}
begin
  FBounds := ABounds;
  AAnnotations := FScrollbar.Annotations;
  if AAnnotations <> nil then
  begin
{$IFDEF DELPHIXE}
    AKeys := FInfos.Keys.ToArray;
{$ELSE}
    SetLength(AKeys, FInfos.Count);
    I := 0;
    for AKind in FInfos.Keys do
    begin
      AKeys[I] := AKind;
      Inc(I);
    end;
{$ENDIF}
    for AKind in AKeys do
      if AKind in FChangedAnnotationKinds then
        FInfos.Remove(AKind);

    if not cxRectIsEmpty(FBounds) and (AAnnotations.GetScrollableRecordCount > 0)
    then
    begin
      for AKind in AAnnotations.ItemLists.Keys do
        if (AKind in FChangedAnnotationKinds) and
          (AAnnotations.ItemLists[AKind].Count > 0) then
          CreateItems(AKind);
      FChangedAnnotationKinds := [];
    end;
  end
  else
    FInfos.Clear;
end;

procedure TcxScrollbarAnnotationInfos.Invalidate;
begin
  FScrollbar.InvalidateRect(FBounds);
end;

procedure TcxScrollbarAnnotationInfos.CreateItems
  (AKind: TdxScrollbarAnnotationKind);

  function GetItemHeight(AStyle: TdxScrollbarAnnotationStyle;
    ACalculatedHeight: Double): Integer;
  var
    AMinHeight, AMaxHeight: Integer;
  begin
    AMinHeight := FScrollbar.ScaleFactor.Apply(Max(0, AStyle.MinHeight));
    AMaxHeight := FScrollbar.ScaleFactor.Apply(Max(0, AStyle.MaxHeight));
    if AMaxHeight <> 0 then
      AMinHeight := Min(AMaxHeight, AMinHeight);
    if ACalculatedHeight < AMinHeight then
      Result := AMinHeight
    else if (AMaxHeight <> 0) and (ACalculatedHeight > AMaxHeight) then
      Result := AMaxHeight
    else
      Result := Round(ACalculatedHeight);
  end;

var
  I: Integer;
  ALastIndexInScrollBand: Integer;
  AAnnotationInfo: TcxScrollbarAnnotationInfo;
  AAnnotationAreaScrollSize: Integer;
  AStartPos, APos, APrevPos: Integer;
  ADataScrollSize: Integer;
  AStyle: TdxScrollbarAnnotationStyle;
  AList: TdxScrollbarAnnotationRowIndexList;
  ARecordIndex: Integer;
  ARecordScrollSize, ARecordScrollPos: Integer;
  AItemBounds, R: TRect;
  AHeight, AWidth: Integer;
  ACalculatedHeight: Double;
  AAnnotations: TdxScrollbarAnnotations;
  AInfos: TObjectList<TcxScrollbarAnnotationInfo>;
begin
  AAnnotations := FScrollbar.Annotations;
  AStyle := AAnnotations.GetStyle(AKind);
  if AStyle.Color = dxacDefault then
    AStyle.Color := FScrollbar.GetDefaultScrollbarAnnotationColor(AKind);

  AItemBounds := Bounds;
  AWidth := FScrollbar.ScaleFactor.Apply(Max(0, AStyle.Width));
  if FScrollbar.GetKind = sbVertical then
  begin
    case AStyle.Alignment of
      saaFar:
        AItemBounds.Left := AItemBounds.Right - AWidth;
      saaCenter:
        AItemBounds := cxRectCenterHorizontally(AItemBounds, AWidth);
      saaNear:
        AItemBounds.Right := AItemBounds.Left + AWidth;
    end;
    AItemBounds := cxRectOffsetHorz(AItemBounds, AStyle.Offset);
    AStartPos := Bounds.Top;
    AAnnotationAreaScrollSize := Bounds.Height;
  end
  else
  begin
    case AStyle.Alignment of
      saaFar:
        AItemBounds.Top := AItemBounds.Bottom - AWidth;
      saaCenter:
        AItemBounds := cxRectCenterVertically(AItemBounds, AWidth);
      saaNear:
        AItemBounds.Bottom := AItemBounds.Top + AWidth;
    end;
    AItemBounds := cxRectOffsetVert(AItemBounds, AStyle.Offset);
    AStartPos := Bounds.Left;
    AAnnotationAreaScrollSize := Bounds.Width;
  end;

  ADataScrollSize := AAnnotations.GetDataScrollSize;
  if AAnnotations.IsRecordIndexBasedRendering then
  begin
    ACalculatedHeight := AAnnotationAreaScrollSize / ADataScrollSize;
    AHeight := GetItemHeight(AStyle, ACalculatedHeight);
  end
  else
  begin
    ACalculatedHeight := 0;
    AHeight := 0;
  end;

  AInfos := GetAnnotationList(AKind);

  APrevPos := -1;
  AList := AAnnotations.ItemLists[AKind];
  for I := 0 to AList.Count - 1 do
  begin
    ARecordIndex := AList[I];
    if AAnnotations.IsRecordIndexBasedRendering then
      APos := Trunc(AAnnotations.GetScrollableRecordIndexByRecordIndex
        (ARecordIndex) * ACalculatedHeight + AStartPos)
    else
    begin
      ARecordScrollPos := AAnnotations.GetRecordPixelScrollPosition
        (ARecordIndex);
      ALastIndexInScrollBand := AAnnotations.GetLastIndexInScrollBand
        (ARecordIndex);

      if ALastIndexInScrollBand < AAnnotations.GetScrollableRecordCount - 1 then
        ARecordScrollSize := AAnnotations.GetRecordPixelScrollPosition
          (ALastIndexInScrollBand + 1) - ARecordScrollPos
      else
        ARecordScrollSize := ADataScrollSize - ARecordScrollPos;
      ACalculatedHeight := AAnnotationAreaScrollSize * ARecordScrollSize /
        ADataScrollSize;
      APos := Trunc(AAnnotationAreaScrollSize * ARecordScrollPos /
        ADataScrollSize + AStartPos);
      AHeight := GetItemHeight(AStyle, ACalculatedHeight);
    end;
    if APos = APrevPos then
      AInfos.Last.RecordIndexes.Add(ARecordIndex)
    else
    begin
      APrevPos := APos;
      AAnnotationInfo := TcxScrollbarAnnotationInfo.Create;
      AAnnotationInfo.RecordIndexes.Add(ARecordIndex);
      AAnnotationInfo.Color := AStyle.Color;
      R := AItemBounds;
      if FScrollbar.GetKind = sbVertical then
      begin
        R.Top := APos;
        R.Bottom := APos + AHeight;
        if R.Bottom > Bounds.Bottom then
          R := cxRectOffsetVert(R, Bounds.Bottom - R.Bottom);
      end
      else
      begin
        R.Left := APos;
        R.Right := APos + AHeight;
        if R.Right > Bounds.Right then
          R := cxRectOffsetVert(R, Bounds.Right - R.Right);
      end;
      AAnnotationInfo.Bounds := R;
      AInfos.Add(AAnnotationInfo);
    end;
  end;
  if FScrollbar.IsRightToLeft then
    for I := 0 to AInfos.Count - 1 do
      AInfos[I].Bounds := TdxRightToLeftLayoutConverter.ConvertRect
        (AInfos[I].Bounds, Bounds);
end;

function TcxScrollbarAnnotationInfos.GetAnnotationList
  (AKind: TdxScrollbarAnnotationKind): TObjectList<TcxScrollbarAnnotationInfo>;
begin
  if not FInfos.TryGetValue(AKind, Result) then
  begin
    Result := TObjectList<TcxScrollbarAnnotationInfo>.Create(True);
    FInfos.Add(AKind, Result);
  end;
end;

{ TcxScrollBarViewInfo }

constructor TcxScrollBarViewInfo.Create(AScrollBar: TcxScrollBarHelper);
begin
  inherited Create;
  FScrollbar := AScrollBar;
  CreatePartInfos;
  FAnnotationInfos := CreateAnnotationInfos;
end;

destructor TcxScrollBarViewInfo.Destroy;
begin
  FreeAndNil(FAnnotationInfos);
  DestroyPartInfos;
  FScrollbar := nil;
  inherited;
end;

procedure TcxScrollBarViewInfo.AdjustPageRects;
begin
  CheckKind;
  if IsRectEmpty(FPartInfos[sbpThumbnail].Bounds) then
  begin
    FPartInfos[sbpPageUp].Bounds := cxEmptyRect;
    FPartInfos[sbpPageDown].Bounds := cxEmptyRect;
  end
  else
  begin
    if FKind = sbHorizontal then
    begin
      GetHorizontalScrollbarPart(spbmPageLeft).Bounds :=
        Rect(GetHorizontalScrollbarPart(spbmLineLeft).Bounds.Right, Bounds.Top,
        FPartInfos[sbpThumbnail].Bounds.Left, Bounds.Bottom);
      GetHorizontalScrollbarPart(spbmPageRight).Bounds :=
        Rect(FPartInfos[sbpThumbnail].Bounds.Right, Bounds.Top,
        GetHorizontalScrollbarPart(spbmLineRight).Bounds.Left, Bounds.Bottom);
    end
    else
    begin
      FPartInfos[sbpPageUp].Bounds :=
        Rect(Bounds.Left, FPartInfos[sbpLineUp].Bounds.Bottom, Bounds.Right,
        FPartInfos[sbpThumbnail].Bounds.Top);
      FPartInfos[sbpPageDown].Bounds :=
        Rect(Bounds.Left, FPartInfos[sbpThumbnail].Bounds.Bottom, Bounds.Right,
        FPartInfos[sbpLineDown].Bounds.Top);
    end;
  end;
end;

procedure TcxScrollBarViewInfo.Calculate;
var
  R: TRect;
begin
  CheckKind;
  FMinThumbnailSize := GetMinThumbnailSize;
  FLength := GetLength;
  FArrowButtonLength := Min(FLength div 2, GetArrowButtonLength);
  if FKind = sbHorizontal then
  begin
    GetHorizontalScrollbarPart(spbmLineLeft).Bounds :=
      cxRectBounds(Bounds.Left, Bounds.Top, FArrowButtonLength, GetHeight);
    GetHorizontalScrollbarPart(spbmLineRight).Bounds :=
      cxRectOffsetHorz(GetHorizontalScrollbarPart(spbmLineLeft).Bounds,
      FLength - FArrowButtonLength);
  end
  else
  begin
    FPartInfos[sbpLineUp].Bounds := cxRectBounds(Bounds.Left, Bounds.Top,
      GetHeight, FArrowButtonLength);
    FPartInfos[sbpLineDown].Bounds :=
      cxRectOffsetVert(FPartInfos[sbpLineUp].Bounds,
      FLength - FArrowButtonLength);
  end;
  CalculateThumbnailRect;
  if GetKind = sbVertical then
  begin
    R := cxRectInflate(Bounds, -2, 0);
    R.Top := PartInfo[sbpLineUp].Bounds.Bottom + 1;
    R.Bottom := PartInfo[sbpLineDown].Bounds.Top - 1;
  end
  else
  begin
    R := cxRectInflate(Bounds, 0, -2);
    if ScrollBar.IsRightToLeft then
    begin
      R.Left := PartInfo[sbpLineDown].Bounds.Right + 1;
      R.Right := PartInfo[sbpLineUp].Bounds.Left - 1;
    end
    else
    begin
      R.Left := PartInfo[sbpLineUp].Bounds.Right + 1;
      R.Right := PartInfo[sbpLineDown].Bounds.Left - 1;
    end;
  end;
  FAnnotationInfosBounds := R;
  CalculateAnnotationInfos;
end;

procedure TcxScrollBarViewInfo.CalculateThumbnailRect;
var
  AThumbRange, AThumbPos, AThumbMaxPos, AThumbLength: Integer;
  ADataRange, ADataPos, ADataMaxPos, ADataPageSize: Integer;
  R: TRect;
begin
  FPartInfos[sbpThumbnail].Bounds := cxEmptyRect;
  AdjustPageRects;
  if IsEnabled then
  begin
    AThumbRange := FLength - FArrowButtonLength * 2;
    ADataRange := GetMax - GetMin;
    ADataPos := GetPosition - GetMin;
    ADataPageSize := GetPageSize;
    ADataMaxPos := ADataRange - ADataPageSize + 1;
    if ADataPageSize = 0 then
    begin
      AThumbLength := dxGetSystemMetrics(IfThen(FKind = sbHorizontal,
        SM_CXHTHUMB, SM_CYVTHUMB), ScaleFactor);
      if AThumbLength > AThumbRange then
        Exit;
      AThumbMaxPos := AThumbRange - AThumbLength;
      if (AThumbMaxPos = 0) or (ADataRange = 0) then
        AThumbPos := 0
      else
        AThumbPos := MulDiv(AThumbMaxPos, ADataPos, ADataRange);
    end
    else
    begin
      AThumbLength := Min(AThumbRange, MulDiv(ADataPageSize, AThumbRange,
        ADataRange + 1));
      if (AThumbRange < FMinThumbnailSize) or (ADataRange = 0) then
        Exit;
      AThumbLength := Max(AThumbLength, FMinThumbnailSize);
      AThumbMaxPos := AThumbRange - AThumbLength;
      AThumbPos := MulDiv(AThumbMaxPos, Min(ADataPos, ADataMaxPos),
        ADataMaxPos);
    end;
    if FKind = sbHorizontal then
    begin
      R := cxRectBounds(GetHorizontalScrollbarPart(spbmLineLeft).Bounds.Right +
        AThumbPos, Bounds.Top, AThumbLength, GetHeight);
      if FDragThumbnailPosX <> 0 then
        R := cxRectSetLeft(R, FDragThumbnailPosX);
      if ScrollBar.IsRightToLeft and (FDragThumbnailPosX = 0) then
        FPartInfos[sbpThumbnail].Bounds :=
          TdxRightToLeftLayoutConverter.ConvertRect(R, Bounds)
      else
        FPartInfos[sbpThumbnail].Bounds := R;
    end
    else
    begin
      R := cxRectBounds(Bounds.Left, FPartInfos[sbpLineUp].Bounds.Bottom +
        AThumbPos, GetHeight, AThumbLength);
      if FDragThumbnailPosY <> 0 then
        R := cxRectSetTop(R, FDragThumbnailPosY);
      FPartInfos[sbpThumbnail].Bounds := R;
    end;
    AdjustPageRects;
  end;
end;

procedure TcxScrollBarViewInfo.SetThumbnailPos(APos: Integer);
begin
  if FKind = sbHorizontal then
    FPartInfos[sbpThumbnail].Bounds :=
      cxRectSetLeft(FPartInfos[sbpThumbnail].Bounds, APos)
  else
    FPartInfos[sbpThumbnail].Bounds :=
      cxRectSetTop(FPartInfos[sbpThumbnail].Bounds, APos);
end;

procedure TcxScrollBarViewInfo.CalculateAnnotationInfos;
begin
  if FAnnotationInfos.ChangedAnnotationKinds <> [] then
    FAnnotationInfos.Calculate(FAnnotationInfosBounds);
end;

function TcxScrollBarViewInfo.CreateAnnotationInfos
  : TcxScrollbarAnnotationInfos;
begin
  Result := TcxScrollbarAnnotationInfos.Create(ScrollBar);
end;

procedure TcxScrollBarViewInfo.CreatePartInfos;
var
  APart: TcxScrollBarPart;
begin
  for APart := sbpLineUp to sbpPageDown do
    FPartInfos[APart] := TcxScrollBarPartViewInfo.Create(Self, APart);
end;

procedure TcxScrollBarViewInfo.DestroyPartInfos;
var
  APart: TcxScrollBarPart;
begin
  for APart := sbpLineUp to sbpPageDown do
    FreeAndNil(FPartInfos[APart]);
end;

function TcxScrollBarViewInfo.GetArrowButtonLength: Integer;
var
  ASize: TSize;
begin
  ASize := ScrollBar.GetScrollBarSize;
  Result := IfThen(FKind = sbHorizontal, ASize.cy, ASize.cx);
end;

function TcxScrollBarViewInfo.GetHorizontalScrollbarPart
  (APartType: TcxHorizontalScrollBarPart): TcxScrollBarPartViewInfo;
const
  HorizontalScrollbarPartToScrollbarPart
    : array [Boolean, TcxHorizontalScrollBarPart] of TcxScrollBarPart =
    ((sbpLineUp, sbpLineDown, sbpPageUp, sbpPageDown),
    (sbpLineDown, sbpLineUp, sbpPageDown, sbpPageUp));
begin
  Result := FPartInfos[HorizontalScrollbarPartToScrollbarPart
    [ScrollBar.IsRightToLeft, APartType]];
end;

function TcxScrollBarViewInfo.GetPart(const P: TPoint): TcxScrollBarPart;
var
  APart: TcxScrollBarPart;
begin
  Result := sbpNone;
  for APart := sbpLineUp to sbpPageDown do
    if PtInRect(PartInfo[APart].Bounds, P) then
    begin
      Result := APart;
      Break;
    end;
end;

procedure TcxScrollBarViewInfo.UpdateAnnotationInfos;
begin
  CalculateAnnotationInfos;
  FAnnotationInfos.Invalidate;
end;

procedure TcxScrollBarViewInfo.UpdateFadingHelperStates;
var
  APart: TcxScrollBarPart;
begin
  for APart := sbpLineUp to sbpPageDown do
    FPartInfos[APart].UpdateFadingHelperState;
end;

procedure TcxScrollBarViewInfo.CheckKind;
begin
  FKind := GetKind;
end;

function TcxScrollBarViewInfo.GetLength: Integer;
begin
  if FKind = sbHorizontal then
    Result := cxRectWidth(Bounds)
  else
    Result := cxRectHeight(Bounds);
end;

function TcxScrollBarViewInfo.GetHeight: Integer;
begin
  if FKind = sbHorizontal then
    Result := cxRectHeight(Bounds)
  else
    Result := cxRectWidth(Bounds);
end;

function TcxScrollBarViewInfo.GetPartInfo(APartType: TcxScrollBarPart)
  : TcxScrollBarPartViewInfo;
begin
  Result := FPartInfos[APartType];
end;

procedure TcxScrollBarViewInfo.SetHotPart(AValue: TcxScrollBarPart);
begin
  if AValue <> FHotPart then
    FHotPart := AValue;
end;

procedure TcxScrollBarViewInfo.SetPressedPart(AValue: TcxScrollBarPart);
begin
  if AValue <> FPressedPart then
    FPressedPart := AValue;
end;

function TcxScrollBarViewInfo.GetBounds: TRect;
begin
  Result := FScrollbar.BoundsRect;
end;

function TcxScrollBarViewInfo.GetKind: TScrollBarKind;
begin
  Result := FScrollbar.Kind;
end;

function TcxScrollBarViewInfo.GetMax: Integer;
begin
  Result := FScrollbar.Max;
end;

function TcxScrollBarViewInfo.GetMin: Integer;
begin
  Result := FScrollbar.Min;
end;

function TcxScrollBarViewInfo.GetMinThumbnailSize: Integer;
begin
  Result := FScrollbar.Painter.GetMinThumbnailSize;
end;

function TcxScrollBarViewInfo.GetPageSize: Integer;
begin
  Result := FScrollbar.PageSize;
end;

function TcxScrollBarViewInfo.GetPosition: Integer;
begin
  Result := FScrollbar.Position;
end;

function TcxScrollBarViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := ScrollBar.ScaleFactor;
end;

function TcxScrollBarViewInfo.IsEnabled: Boolean;
begin
  Result := FScrollbar.Enabled;
end;

{ TcxScrollBar }

constructor TcxScrollBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLookAndFeel := TcxLookAndFeel.Create(Self);
  FLookAndFeel.OnChanged := LookAndFeelChanged;
  FScaleFactor := TdxOwnedScaleFactor.Create;
  FScaleFactor.UseOwnerValue := False;
  FHelper := GetHelperClass.Create(Self);
  FHelper.OnScroll := DoScrollEvent;
  FHelper.OnChange := DoChange;
  Width := 121;
  ControlStyle := [csFramed, csOpaque, csCaptureMouse];
  Height := GetInitialSize.cy;
  FThemeChangedNotificator := TdxThemeChangedNotificator.Create;
  FThemeChangedNotificator.OnThemeChanged := ThemeChanged;
  FHelper.Calculate;
end;

destructor TcxScrollBar.Destroy;
begin
  FreeAndNil(FThemeChangedNotificator);
  FreeAndNil(FHelper);
  FreeAndNil(FLookAndFeel);
  FreeAndNil(FScaleFactor);
  inherited Destroy;
end;

procedure TcxScrollBar.SetCtl3D(Value: Boolean);
begin
  FRealCtl3D := Value;
  InternalCtl3D := Value;
end;

procedure TcxScrollBar.SetInternalCtl3D(Value: Boolean);
begin
  if InternalCtl3D <> Value then
    inherited Ctl3D := Value;
end;

function TcxScrollBar.IsCtl3DStored: Boolean;
begin
  Result := not ParentCtl3D;
end;

procedure TcxScrollBar.SetKind(Value: TScrollBarKind);
begin
  if Kind <> Value then
  begin
    FHelper.Kind := Value;
    if not(csLoading in ComponentState) then
      SetBounds(Left, Top, Height, Width)
    else
      FHelper.Calculate;
    Invalidate;
  end;
end;

procedure TcxScrollBar.SetLargeChange(AValue: TScrollBarInc);
begin
  FHelper.LargeChange := AValue;
end;

procedure TcxScrollBar.SetLookAndFeel(Value: TcxLookAndFeel);
begin
  FLookAndFeel.Assign(Value);
end;

procedure TcxScrollBar.SetScrollParams(AMin, AMax, APosition,
  APageSize: Integer; ARedraw: Boolean = True);
begin
  FHelper.SetScrollParams(AMin, AMax, APosition, APageSize, ARedraw);
end;

procedure TcxScrollBar.SetParams(APosition, AMin, AMax: Integer);
begin
  SetScrollParams(AMin, AMax, APosition, PageSize);
end;

procedure TcxScrollBar.SetMax(Value: Integer);
begin
  SetScrollParams(Min, Value, Position, PageSize);
end;

procedure TcxScrollBar.SetMin(Value: Integer);
begin
  SetScrollParams(Value, Max, Position, PageSize);
end;

procedure TcxScrollBar.SetParent(AValue: TWinControl);
begin
  if TcxControlHelper.CanSetParent(Self, AValue) then
  begin
    inherited SetParent(AValue);
    TcxControlHelper.UpdateScaleFactorOnParentChange(Self);
  end;
end;

procedure TcxScrollBar.SetPageSize(Value: Integer);
begin
  SetScrollParams(Min, Max, Position, Value);
end;

procedure TcxScrollBar.SetPosition(Value: Integer);
begin
  SetScrollParams(Min, Max, Value, PageSize);
end;

procedure TcxScrollBar.SetSmallChange(AValue: TScrollBarInc);
begin
  FHelper.SmallChange := AValue;
end;

procedure TcxScrollBar.SetUnlimitedTracking(AValue: Boolean);
begin
  FHelper.UnlimitedTracking := AValue;
end;

{$IFDEF DELPHIBERLIN}

procedure TcxScrollBar.ChangeScale(M, D: Integer; isDpiChange: Boolean);
{$ELSE}

procedure TcxScrollBar.ChangeScale(M, D: Integer);
{$ENDIF}
begin
  ScaleFactor.Change(M, D);
  inherited;
  LookAndFeel.Refresh;
end;

procedure TcxScrollBar.DoChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TcxScrollBar.DoScrollEvent(Sender: TObject; ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  if Assigned(FOnScroll) then
    FOnScroll(Self, ScrollCode, ScrollPos);
end;

procedure TcxScrollBar.Change;
begin
  inherited Changed;
  DoChange(Self);
end;

function TcxScrollBar.GetInitialSize: TSize;
begin
  Result := GetScaledScrollBarSize(ScaleFactor);
end;

function TcxScrollBar.GetHelperClass: TcxScrollBarHelperClass;
begin
  Result := TcxScrollBarHelper;
end;

function TcxScrollBar.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := LookAndFeel.GetAvailablePainter(totScrollBar);
end;

procedure TcxScrollBar.LookAndFeelChanged(Sender: TcxLookAndFeel;
  AChangedValues: TcxLookAndFeelValues);
var
  ASaveValue: Boolean;
begin
  if (LookAndFeel.SkinPainter <> nil) and Ctl3D then
  begin
    ASaveValue := Ctl3D;
    InternalCtl3D := False;
    FRealCtl3D := ASaveValue;
  end
  else
    InternalCtl3D := FRealCtl3D;
  FHelper.Calculate;
  Invalidate;
end;

procedure TcxScrollBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  FHelper.MouseDown(Button, Shift, X, Y);
end;

procedure TcxScrollBar.MouseEnter(AControl: TControl);
begin
  FHelper.MouseEnter(Self);
  if AControl = Self then
    dxCallNotify(OnMouseEnter, Self);
end;

procedure TcxScrollBar.MouseLeave(AControl: TControl);
begin
  FHelper.MouseLeave(Self);
  if AControl = Self then
    dxCallNotify(OnMouseLeave, Self);
end;

procedure TcxScrollBar.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  FHelper.MouseMove(Shift, X, Y);
end;

procedure TcxScrollBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  FHelper.MouseUp(Button, Shift, X, Y);
end;

procedure TcxScrollBar.DoPaint(ACanvas: TcxCanvas);
begin
  FHelper.DoPaint(ACanvas);
end;

procedure TcxScrollBar.Paint;
var
  ABitmap: TcxBitmap;
begin
  ABitmap := TcxBitmap.CreateSize(Width, Height);
  try
    ABitmap.cxCanvas.Lock;
    try
      DoPaint(ABitmap.cxCanvas);
      cxBitBlt(Canvas.Handle, ABitmap.cxCanvas.Handle, ClientRect,
        cxNullPoint, SRCCOPY);
    finally
      ABitmap.cxCanvas.Unlock;
    end;
  finally
    ABitmap.Free;
  end;
end;

procedure TcxScrollBar.Resize;
begin
  inherited Resize;
  FHelper.BoundsRect := ClientRect;
  FHelper.Calculate;
end;

procedure TcxScrollBar.Scroll(ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  DoScrollEvent(Self, ScrollCode, ScrollPos);
end;

procedure TcxScrollBar.ScrollbarAnnotationsInvalidate(AAnnotationKinds
  : TdxScrollbarAnnotationKinds = dxAllScrollbarAnnotationKinds);
begin
  FHelper.ScrollbarAnnotationsInvalidate(AAnnotationKinds);
end;

procedure TcxScrollBar.SetAnnotations(AAnnotations: TdxScrollbarAnnotations);
begin
  FHelper.SetAnnotations(AAnnotations);
end;

procedure TcxScrollBar.ScrollbarAnnotationsUpdate;
begin
  FHelper.ScrollbarAnnotationsUpdate;
end;

function TcxScrollBar.GetCtl3D: Boolean;
begin
  Result := InternalCtl3D or FRealCtl3D;
end;

function TcxScrollBar.GetInternalCtl3D: Boolean;
begin
  Result := inherited Ctl3D;
end;

function TcxScrollBar.GetControl: TWinControl;
begin
  Result := Self;
end;

function TcxScrollBar.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := FLookAndFeel;
end;

function TcxScrollBar.GetScaleFactor: TdxScaleFactor;
begin
  Result := ScaleFactor;
end;

function TcxScrollBar.GetKind: TScrollBarKind;
begin
  Result := FHelper.Kind;
end;

function TcxScrollBar.GetLargeChange: TScrollBarInc;
begin
  Result := FHelper.LargeChange;
end;

function TcxScrollBar.GetMax: Integer;
begin
  Result := FHelper.Max;
end;

function TcxScrollBar.GetMin: Integer;
begin
  Result := FHelper.Min;
end;

function TcxScrollBar.GetPageSize: Integer;
begin
  Result := FHelper.PageSize;
end;

function TcxScrollBar.GetPosition: Integer;
begin
  Result := FHelper.Position;
end;

function TcxScrollBar.GetSmallChange: TScrollBarInc;
begin
  Result := FHelper.SmallChange;
end;

function TcxScrollBar.GetUnlimitedTracking: Boolean;
begin
  Result := FHelper.UnlimitedTracking;
end;

procedure TcxScrollBar.ThemeChanged;
begin
  FHelper.Calculate;
  UpdateScrollBarBitmaps;
  Invalidate;
  if Parent <> nil then
    Parent.Realign;
end;

procedure TcxScrollBar.CMCtl3DChanged(var Message: TMessage);
begin
  inherited;
  FRealCtl3D := InternalCtl3D;
  LookAndFeelChanged(LookAndFeel, []);
end;

procedure TcxScrollBar.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  FHelper.Enabled := Enabled;
  FHelper.Calculate;
  Invalidate;
end;

procedure TcxScrollBar.CNHScroll(var Message: TWMHScroll);
begin
  FHelper.Controller.InternalScroll(TScrollCode(Message.ScrollCode));
end;

procedure TcxScrollBar.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Message.lParam = 0 then
    MouseEnter(Self)
  else
    MouseEnter(TControl(Message.lParam));
end;

procedure TcxScrollBar.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Message.lParam = 0 then
    MouseLeave(Self)
  else
    MouseLeave(TControl(Message.lParam));
end;

procedure TcxScrollBar.CMSysColorChange(var Message: TMessage);
begin
  UpdateScrollBarBitmaps;
  inherited;
end;

procedure TcxScrollBar.CMVisibleChanged(var Message: TMessage);
begin
  FHelper.Visible := Visible;
  inherited;
end;

procedure TcxScrollBar.CNVScroll(var Message: TWMVScroll);
begin
  FHelper.Controller.InternalScroll(TScrollCode(Message.ScrollCode));
end;

procedure TcxScrollBar.CNCtlColorScrollBar(var Message: TMessage);
begin
  UpdateScrollBarBitmaps;
  with Message do
    CallWindowProc(DefWndProc, Handle, Msg, WParam, lParam);
end;

procedure TcxScrollBar.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TcxScrollBar.WMGestureNotify(var Message: TWMGestureNotify);
begin
  dxLockGestures(Handle, Message);
end;

procedure TcxScrollBar.WMCancelMode(var Message: TWMCancelMode);
begin
  FHelper.CancelMode;
  inherited;
end;

{ TcxScrollBarHintHelper }

procedure TcxScrollBarHintHelper.CorrectHintWindowRect(var ARect: TRect);
var
  P: TPoint;
begin
  inherited;
  P := GetMouseCursorPos;
  if GetOwnerWinControl.UseRightToLeftAlignment then
    ARect := cxRectSetOrigin(ARect, Point(P.X - ARect.Width, P.Y))
  else
    ARect := cxRectSetOrigin(ARect, P);
  OffsetRect(ARect, 0, cxGetCursorSize.cy);
end;

constructor TcxScrollBarHintHelper.Create(AController: TcxScrollBarController);
begin
  FController := AController;
end;

function TcxScrollBarHintHelper.GetOwnerWinControl: TWinControl;
begin
  Result := FController.ScrollBar.Owner.GetControl;
end;

{ TcxScrollBarController }

constructor TcxScrollBarController.Create(AScrollBar: TcxScrollBarHelper);
begin
  inherited Create;
  FScrollbar := AScrollBar;
  FTimer := cxCreateTimer(OnTimer, cxScrollInitialInterval, False);
  FHintHelper := CreateHintHelper;
  FAnnotationsHintableObject := TcxScrollBarAnnotationHintableObject.Create;
  FHotAnnotationRowIndexLists := TdxScrollbarAnnotationRowIndexLists.Create
    ([doOwnsValues]);
end;

destructor TcxScrollBarController.Destroy;
begin
  FreeAndNil(FHotAnnotationRowIndexLists);
  FreeAndNil(FAnnotationsHintableObject);
  FreeAndNil(FHintHelper);
  FreeAndNil(FTimer);
  FScrollbar := nil;
  inherited;
end;

procedure TcxScrollBarController.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  FHintHelper.MouseDown;
  DoMouseDown(Button, Shift, X, Y);
end;

procedure TcxScrollBarController.MouseEnter(AControl: TControl);
begin
  DoMouseEnter(AControl);
end;

procedure TcxScrollBarController.MouseLeave(AControl: TControl);
begin
  DoMouseLeave(AControl);
  FHintHelper.MouseLeave;
end;

procedure TcxScrollBarController.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  DoMouseMove(Shift, X, Y);
end;

procedure TcxScrollBarController.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DoMouseUp(Button, Shift, X, Y);
end;

procedure TcxScrollBarController.SetPositionValue(AScrollCode: TScrollCode;
  AValue: Integer);
begin
  case AScrollCode of
    scLineUp:
      Dec(AValue, FScrollbar.SmallChange);
    scLineDown:
      Inc(AValue, FScrollbar.SmallChange);
    scPageUp:
      Dec(AValue, FScrollbar.LargeChange);
    scPageDown:
      Inc(AValue, FScrollbar.LargeChange);
    scTop:
      AValue := FScrollbar.Min;
    scBottom:
      AValue := FScrollbar.Max;
  end;
  AValue := EnsureRange(AValue, FScrollbar.Min, FScrollbar.Max);
  FScrollbar.Scroll(AScrollCode, AValue);
  AValue := EnsureRange(AValue, FScrollbar.Min, FScrollbar.Max);
  if AValue <> FScrollbar.Position then
  begin
    if AScrollCode <> scTrack then
      FScrollbar.Position := AValue
    else
    begin
      FScrollbar.FPosition := AValue;
      FScrollbar.Repaint;
    end;
  end;
end;

procedure TcxScrollBarController.SetThumbnailValue(AValue: Integer;
  AUseSetter: Boolean);
begin
  if AUseSetter then
    FScrollbar.Position := AValue
  else
    FScrollbar.FPosition := AValue;
  DoScroll(sbpThumbnail);
end;

function TcxScrollBarController.CreateHintHelper: TcxScrollBarHintHelper;
begin
  Result := TcxScrollBarHintHelper.Create(Self);
end;

procedure TcxScrollBarController.DoMouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  APart: TcxScrollBarPart;
begin
  if (Button <> mbLeft) then
    Exit;
  APart := ViewInfo.GetPart(Point(X, Y));
  if APart <> sbpNone then
  begin
    try
      if APart = sbpThumbnail then
      begin
        FDownMousePos := Point(X, Y);
        FLastMousePos := FDownMousePos;
        FSavePosition := FScrollbar.Position;
        FSaveThumbnailPos := ViewInfo.PartInfo[sbpThumbnail].Bounds.TopLeft;
        InternalScroll(scTrack);
      end;
      ViewInfo.PressedPart := APart;
      ViewInfo.HotPart := APart;
      if APart in cxTimerParts then
      begin
        if (ScrollBar.Annotations <> nil) and (APart in [sbpPageUp, sbpPageDown]
          ) and (FHotAnnotationRowIndex <> -1) then
          ViewInfo.PressedPart := sbpNone
        else
        begin
          DoScroll(APart);
          FTimer.Interval := cxScrollInitialInterval;
          FTimer.Enabled := True;
        end;
      end;
    finally
      ViewInfo.UpdateFadingHelperStates;
      FScrollbar.Repaint;
    end;
  end;
end;

procedure TcxScrollBarController.DoMouseEnter(AControl: TControl);
begin
  ViewInfo.UpdateFadingHelperStates;
  if IsButtonHotTrack or (ViewInfo.PressedPart in cxTimerParts) then
    FScrollbar.Repaint;
end;

procedure TcxScrollBarController.DoMouseLeave(AControl: TControl);
begin
  if ViewInfo.PressedPart <> sbpThumbnail then
    ViewInfo.HotPart := sbpNone;
  if IsButtonHotTrack or (ViewInfo.PressedPart in cxTimerParts) then
    FScrollbar.Invalidate;
  ViewInfo.UpdateFadingHelperStates;
end;

procedure TcxScrollBarController.DoMouseMove(Shift: TShiftState; X, Y: Integer);
var
  APart: TcxScrollBarPart;
  R: TRect;
  ADelta, ANewPos, ASize: Integer;

  procedure UpdateThumbnail(ADeltaX, ADeltaY: Integer);
  begin
    if FScrollbar.Kind = sbHorizontal then
      ViewInfo.SetThumbnailPos(FSaveThumbnailPos.X + ADeltaX)
    else
      ViewInfo.SetThumbnailPos(FSaveThumbnailPos.Y + ADeltaY);
    ViewInfo.AdjustPageRects;
    FScrollbar.Repaint;
  end;

  function GetDeltaMinHorz: Integer;
  begin
    if ScrollBar.IsRightToLeft then
      Result := ViewInfo.PartInfo[sbpLineDown].Bounds.Right -
        FSaveThumbnailPos.X
    else
      Result := ViewInfo.PartInfo[sbpLineUp].Bounds.Right - FSaveThumbnailPos.X;
  end;

  function GetDeltaMaxHorz: Integer;
  begin
    if ScrollBar.IsRightToLeft then
      Result := ViewInfo.PartInfo[sbpLineUp].Bounds.Left -
        (FSaveThumbnailPos.X + ASize)
    else
      Result := ViewInfo.PartInfo[sbpLineDown].Bounds.Left -
        (FSaveThumbnailPos.X + ASize);
  end;

  function GetDeltaMinVert: Integer;
  begin
    Result := ViewInfo.PartInfo[sbpLineUp].Bounds.Bottom - FSaveThumbnailPos.Y;
  end;

  function GetDeltaMaxVert: Integer;
  begin
    Result := ViewInfo.PartInfo[sbpLineDown].Bounds.Top -
      (FSaveThumbnailPos.Y + ASize);
  end;

begin
  APart := ViewInfo.GetPart(Point(X, Y));
  if ViewInfo.PressedPart = sbpThumbnail then
  begin
    if FScrollbar.Kind = sbHorizontal then
    begin
      ASize := cxRectWidth(ViewInfo.PartInfo[sbpThumbnail].Bounds);
      R := Rect(-cxScrollMinDistance, -cxScrollMaxDistance,
        cxRectWidth(ViewInfo.Bounds) + cxScrollMinDistance,
        cxRectHeight(ViewInfo.Bounds) + cxScrollMaxDistance);
    end
    else
    begin
      ASize := cxRectHeight(ViewInfo.PartInfo[sbpThumbnail].Bounds);
      R := Rect(-cxScrollMaxDistance, -cxScrollMinDistance,
        cxRectWidth(ViewInfo.Bounds) + cxScrollMaxDistance,
        cxRectHeight(ViewInfo.Bounds) + cxScrollMinDistance);
    end;
    R := cxRectOffset(R, ViewInfo.Bounds.TopLeft);
    if not(FScrollbar.UnlimitedTracking or PtInRect(R, Point(X, Y))) then
    begin
      if FScrollbar.Position <> FSavePosition then
        SetThumbnailValue(FSavePosition, True);
    end
    else
    begin
      if FScrollbar.Kind = sbHorizontal then
      begin
        ADelta := X - FDownMousePos.X;
        if (ADelta = 0) or (FLastMousePos.X = X) then
          Exit;
        FLastMousePos.X := X;
        ADelta := EnsureRange(ADelta, GetDeltaMinHorz, GetDeltaMaxHorz);
        UpdateThumbnail(ADelta, 0);
        ViewInfo.FDragThumbnailPosX := FSaveThumbnailPos.X + ADelta;
      end
      else
      begin
        ADelta := Y - FDownMousePos.Y;
        if (ADelta = 0) or (FLastMousePos.Y = Y) then
          Exit;
        FLastMousePos.Y := Y;
        ADelta := EnsureRange(ADelta, GetDeltaMinVert, GetDeltaMaxVert);
        UpdateThumbnail(0, ADelta);
        ViewInfo.FDragThumbnailPosY := FSaveThumbnailPos.Y + ADelta;
      end;
      ANewPos := GetPositionFromThumbnail;
      if ANewPos <> FScrollbar.Position then
      begin
        SetThumbnailValue(ANewPos, False);
        FScrollbar.Change;
      end;
    end;
  end
  else
  begin
    if ViewInfo.PressedPart <> sbpNone then
      FTimer.Enabled := ViewInfo.PressedPart = APart;
    if ViewInfo.HotPart <> APart then
    begin
      ViewInfo.HotPart := APart;
      ViewInfo.UpdateFadingHelperStates;
      if IsButtonHotTrack then
        FScrollbar.Repaint;
    end;
    if (ViewInfo.PressedPart = sbpNone) and (ssLeft in Shift) and FScrollbar.AllowOwnerScrollOnDrag
    then
    begin
      if APart in [sbpPageUp, sbpLineUp] then
        DoScroll(sbpLineUp);
      if APart in [sbpPageDown, sbpLineDown] then
        DoScroll(sbpLineDown);
    end;
  end;
  if (ScrollBar.Annotations <> nil) and (ViewInfo.PressedPart <> sbpThumbnail)
  then
  begin
    CheckHotAnnotations(Point(X, Y));
    CheckHint;
  end
  else
    FHotAnnotationRowIndex := -1;
end;

procedure TcxScrollBarController.DoMouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  try
    FinishScrolling(ViewInfo.GetPart(Point(X, Y)));
  finally
    ViewInfo.UpdateFadingHelperStates;
  end;
  if (ScrollBar.Annotations <> nil) and (Button = mbLeft) then
    ScrollBar.MoveToCurrentAnnotation;
end;

procedure TcxScrollBarController.DoScroll(APart: TcxScrollBarPart);
begin
  case APart of
    sbpLineUp:
      InternalScroll(scLineUp);
    sbpLineDown:
      InternalScroll(scLineDown);
    sbpPageUp:
      InternalScroll(scPageUp);
    sbpPageDown:
      InternalScroll(scPageDown);
    sbpThumbnail:
      InternalScroll(scTrack);
  end;
end;

procedure TcxScrollBarController.CheckHint;
var
  AHintAreaBounds: TRect;
  AHint: string;
begin
  if FHotAnnotationRowIndex <> -1 then
  begin
    AHint := '';
    ScrollBar.Annotations.GetHint(FHotAnnotationRowIndexLists, AHint);
    if AHint <> '' then
    begin
      AHintAreaBounds := FAnnotationsHintableObject.Bounds;
      FHintHelper.ShowHint(AHintAreaBounds, AHintAreaBounds, AHint, False,
        FAnnotationsHintableObject);
    end;
  end;
end;

procedure TcxScrollBarController.CheckHotAnnotations(APos: TPoint);
var
  I: Integer;
  AKind: TdxScrollbarAnnotationKind;
  AAnnotationInfos: TObjectList<TcxScrollbarAnnotationInfo>;
  AItems: TdxScrollbarAnnotationRowIndexList;
begin
  FHotAnnotationRowIndex := -1;
  FHotAnnotationRowIndexLists.Clear;
  for AKind in ViewInfo.AnnotationsInfo.Infos.Keys do
  begin
    AAnnotationInfos := ViewInfo.AnnotationsInfo.Infos[AKind];
    if AAnnotationInfos <> nil then
      for I := 0 to AAnnotationInfos.Count - 1 do
        if PtInRect(AAnnotationInfos[I].Bounds, APos) then
        begin
          if not FHotAnnotationRowIndexLists.TryGetValue(AKind, AItems) then
          begin
            AItems := TdxScrollbarAnnotationRowIndexList.Create;
            FHotAnnotationRowIndexLists.Add(AKind, AItems);
          end;
          AItems.AddRange(AAnnotationInfos[I].RecordIndexes);
          if FHotAnnotationRowIndex = -1 then
          begin
            FHotAnnotationRowIndex := AAnnotationInfos[I].RecordIndexes[0];
            FAnnotationsHintableObject.Bounds := AAnnotationInfos[I].Bounds;
          end;
        end;
  end;
end;

procedure TcxScrollBarController.FinishScrolling
  (AHotPart: TcxScrollBarPart = sbpNone);
var
  AWasThumbnailPressed: Boolean;
begin
  ViewInfo.FDragThumbnailPosX := 0;
  ViewInfo.FDragThumbnailPosY := 0;
  ViewInfo.HotPart := AHotPart;
  if ViewInfo.PressedPart <> sbpNone then
  begin
    FTimer.Enabled := False;
    AWasThumbnailPressed := ViewInfo.PressedPart = sbpThumbnail;
    ViewInfo.PressedPart := sbpNone;
    try
      if AWasThumbnailPressed then
      begin
        FScrollbar.FPosition := GetPositionFromThumbnail;
        InternalScroll(scPosition);
      end;
    finally
      InternalScroll(scEndScroll);
      ViewInfo.CalculateThumbnailRect;
      ViewInfo.UpdateFadingHelperStates;
      FScrollbar.Invalidate;
    end;
  end;
end;

function TcxScrollBarController.GetPositionFromThumbnail: Integer;
var
  ATotal, AThumbnailSize, ADistance, X: Integer;
begin
  ATotal := FScrollbar.Max - FScrollbar.Min;
  if FScrollbar.PageSize > 0 then
    Dec(ATotal, FScrollbar.PageSize - 1);
  if FScrollbar.Kind = sbHorizontal then
  begin
    AThumbnailSize := ViewInfo.PartInfo[sbpThumbnail].Bounds.Right -
      ViewInfo.PartInfo[sbpThumbnail].Bounds.Left;
    ADistance := ViewInfo.GetHorizontalScrollbarPart(spbmLineRight).Bounds.Left
      - ViewInfo.GetHorizontalScrollbarPart(spbmLineLeft).Bounds.Right -
      AThumbnailSize;
    if ScrollBar.IsRightToLeft then
      X := ViewInfo.PartInfo[sbpLineUp].Bounds.Left - ViewInfo.PartInfo
        [sbpThumbnail].Bounds.Right
    else
      X := ViewInfo.PartInfo[sbpThumbnail].Bounds.Left - ViewInfo.PartInfo
        [sbpLineUp].Bounds.Right;
    Result := FScrollbar.Min + MulDiv(ATotal, X, ADistance);
  end
  else
  begin
    AThumbnailSize := ViewInfo.PartInfo[sbpThumbnail].Bounds.Bottom -
      ViewInfo.PartInfo[sbpThumbnail].Bounds.Top;
    ADistance := ViewInfo.PartInfo[sbpLineDown].Bounds.Top - ViewInfo.PartInfo
      [sbpLineUp].Bounds.Bottom - AThumbnailSize;
    Result := FScrollbar.Min + MulDiv(ATotal, ViewInfo.PartInfo[sbpThumbnail]
      .Bounds.Top - ViewInfo.PartInfo[sbpLineUp].Bounds.Bottom, ADistance);
  end;
end;

function TcxScrollBarController.GetViewInfo: TcxScrollBarViewInfo;
begin
  Result := FScrollbar.ViewInfo;
end;

procedure TcxScrollBarController.InternalScroll(AScrollCode: TScrollCode);
begin
  SetPositionValue(AScrollCode, FScrollbar.Position);
end;

function TcxScrollBarController.IsButtonHotTrack: Boolean;
begin
  Result := FScrollbar.Painter.IsButtonHotTrack;
end;

procedure TcxScrollBarController.OnTimer(Sender: TObject);

  function CheckHotPart: Boolean;
  var
    P: TPoint;
  begin
    P := FScrollbar.ScreenToClient(GetMouseCursorPos);
    Result := ViewInfo.GetPart(P) = ViewInfo.PressedPart;
  end;

begin
  FTimer.Enabled := False;
  if FScrollbar.IsOwnerControlCapture and (ViewInfo.PressedPart in cxTimerParts)
  then
  begin
    if FTimer.Interval = cxScrollInitialInterval then
      FTimer.Interval := cxScrollInterval;
    DoScroll(ViewInfo.PressedPart);
    FTimer.Enabled := CheckHotPart;
  end
  else
    FinishScrolling;
end;

{ TcxScrollBarHelper }

constructor TcxScrollBarHelper.Create(AOwner: IcxScrollBarOwner);
var
  AOwner2Provider: IcxScrollBarOwner2Provider;
begin
  inherited Create;
  FOwner := AOwner;
  FViewInfo := GetViewInfoClass.Create(Self);
  FController := GetControllerClass.Create(Self);
  FPainter := GetPainterClass.Create(Self);
  BoundsRect := cxRectBounds(0, 0, 120, GetScrollBarSize.cy);
  FKind := sbHorizontal;
  FEnabled := True;
  FPosition := 0;
  FMin := 0;
  FMax := 100;
  FSmallChange := 1;
  FLargeChange := 1;
  if Supports(AOwner, IcxScrollBarOwner2Provider, AOwner2Provider) then
    FOwner2 := AOwner2Provider.GetOwner2
  else
    FOwner2 := nil;
end;

destructor TcxScrollBarHelper.Destroy;
begin
  SetAnnotations(nil);
  if FVisibleRgn <> 0 then
    DeleteObject(FVisibleRgn);
  FreeAndNil(FPainter);
  FreeAndNil(FController);
  FreeAndNil(FViewInfo);
  inherited Destroy;
end;

procedure TcxScrollBarHelper.Calculate;
begin
  FViewInfo.Calculate;
end;

procedure TcxScrollBarHelper.CancelMode;
begin
  FController.FinishScrolling;
end;

function TcxScrollBarHelper.GetOwnerOrigin: TPoint;
begin
  if FOwner2 = nil then
    Result := cxNullPoint
  else
    Result := FOwner2.GetOrigin;
end;

function TcxScrollBarHelper.GetOwnerVisible: Boolean;
begin
  Result := (FOwner2 = nil) or FOwner2.GetVisible;
end;

procedure TcxScrollBarHelper.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  TranslateCoords(X, Y);
  FController.MouseDown(Button, Shift, X, Y);
end;

procedure TcxScrollBarHelper.MouseEnter(AControl: TControl);
begin
  FController.MouseEnter(AControl);
end;

procedure TcxScrollBarHelper.MouseLeave(AControl: TControl);
begin
  FController.MouseLeave(AControl);
end;

procedure TcxScrollBarHelper.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  TranslateCoords(X, Y);
  FController.MouseMove(Shift, X, Y);
end;

procedure TcxScrollBarHelper.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  TranslateCoords(X, Y);
  FController.MouseUp(Button, Shift, X, Y);
end;

procedure TcxScrollBarHelper.Repaint;
begin
  if HandleAllocated then
    if IsNonClient then
      RefreshNCPart(BoundsRect)
    else
    begin
      Invalidate;
      FOwner.GetControl.Update;
    end;
end;

procedure TcxScrollBarHelper.Paint(ACanvas: TcxCanvas);
var
  ARegion: TcxRegion;
begin
  ARegion := GetTempVisibleRegion;
  try
    ACanvas.SaveClipRegion;
    ACanvas.SetClipRegion(ARegion, roIntersect, False);
    if NeedBuffering then
      DoBufferedPaint(ACanvas.Handle)
    else
      DoPaint(ACanvas);
    ACanvas.RestoreClipRegion;
    ACanvas.SetClipRegion(ARegion, roSubtract, False);
  finally
    ARegion.Free;
  end;
end;

procedure TcxScrollBarHelper.DoBufferedPaint(ADC: HDC);
var
  ABitmap: TcxBitmap;
  R: TRect;
begin
  R := BoundsRect;
  if (VisibleRgn = 0) or (GetRgnBox(VisibleRgn, R) <> NULLREGION) then
  begin
    ABitmap := TcxBitmap.CreateSize(R);
    try
      ABitmap.cxCanvas.WindowOrg := R.TopLeft;
      DoPaint(ABitmap.cxCanvas);
      cxBitBlt(ADC, ABitmap.cxCanvas.Handle, R, R.TopLeft, SRCCOPY);
    finally
      ABitmap.Free;
    end;
  end;
end;

procedure TcxScrollBarHelper.Paint(ADC: HDC);
var
  APrevWindowOrg, AWindowOrg: TPoint;
begin
  cxPaintCanvas.BeginPaint(ADC);
  try
    APrevWindowOrg := cxPaintCanvas.WindowOrg;
    AWindowOrg := GetOwnerOrigin;
    AWindowOrg.Invert;
    AWindowOrg.Offset(APrevWindowOrg);
    cxPaintCanvas.WindowOrg := AWindowOrg;
    cxPaintCanvas.SetClipRegion(GetTempVisibleRegion, roIntersect);
    DoBufferedPaint(cxPaintCanvas.Handle);
  finally
    cxPaintCanvas.WindowOrg := APrevWindowOrg;
    cxPaintCanvas.EndPaint;
  end;
end;

procedure TcxScrollBarHelper.SetScrollParams(AMin, AMax, APosition,
  APageSize: Integer; ARedraw: Boolean = True);

  procedure InternalSetScrollParams;
  begin
    ViewInfo.CalculateThumbnailRect;
    ViewInfo.UpdateFadingHelperStates;
    if ARedraw and HandleAllocated then
      Repaint;
  end;

begin
  if (AMax < AMin) or (AMax - AMin < APageSize) then
    raise EInvalidOperation.Create(SScrollBarRange);
  APosition := EnsureRange(APosition, AMin, AMax);

  if (Min <> AMin) or (Max <> AMax) or (PageSize <> APageSize) or
    (Position <> APosition) then
  begin
    FMin := AMin;
    FMax := AMax;
    FPageSize := APageSize;
  end
  else
    ARedraw := False;

  if Position <> APosition then
  begin
    // Enabled := True; //#Ch B234451
    FPosition := APosition;
    InternalSetScrollParams;
    Change;
  end
  else
    InternalSetScrollParams;
end;

procedure TcxScrollBarHelper.ScrollbarAnnotationsInvalidate(AAnnotationKinds
  : TdxScrollbarAnnotationKinds = dxAllScrollbarAnnotationKinds);
begin
  FViewInfo.AnnotationsInfo.ChangedAnnotationKinds := AAnnotationKinds;
end;

procedure TcxScrollBarHelper.ScrollbarAnnotationsUpdate;
var
  AChangedKinds: TdxScrollbarAnnotationKinds;
begin
  if Annotations = nil then
    AChangedKinds := dxAllScrollbarAnnotationKinds
  else
    AChangedKinds := Annotations.ChangedKinds;
  FViewInfo.AnnotationsInfo.ChangedAnnotationKinds := AChangedKinds;
  if Visible then
    FViewInfo.UpdateAnnotationInfos;
end;

procedure TcxScrollBarHelper.SetAnnotations(AAnnotations
  : TdxScrollbarAnnotations);
begin
  if FAnnotations <> AAnnotations then
  begin
    if FAnnotations <> nil then
      FAnnotations.RemoveRenderer(Self);
    FAnnotations := AAnnotations;
    if FAnnotations <> nil then
      FAnnotations.AddRenderer(Self);
    ScrollbarAnnotationsUpdate;
  end;
end;

function TcxScrollBarHelper.GetBoundsRect: TRect;
begin
  Result := FBoundsRect;
end;

function TcxScrollBarHelper.GetEnabled: Boolean;
begin
  Result := FEnabled;
end;

function TcxScrollBarHelper.GetKind: TScrollBarKind;
begin
  Result := FKind;
end;

function TcxScrollBarHelper.GetLargeChange: TScrollBarInc;
begin
  Result := FLargeChange;
end;

function TcxScrollBarHelper.GetMax: Integer;
begin
  Result := FMax;
end;

function TcxScrollBarHelper.GetMin: Integer;
begin
  Result := FMin;
end;

function TcxScrollBarHelper.GetPageSize: Integer;
begin
  Result := FPageSize;
end;

function TcxScrollBarHelper.GetPosition: Integer;
begin
  Result := FPosition;
end;

function TcxScrollBarHelper.GetSmallChange: TScrollBarInc;
begin
  Result := FSmallChange;
end;

function TcxScrollBarHelper.GetUnlimitedTracking: Boolean;
begin
  Result := FUnlimitedTracking;
end;

function TcxScrollBarHelper.GetVisible: Boolean;
begin
  Result := FVisible;
end;

procedure TcxScrollBarHelper.SetBoundsRect(const AValue: TRect);
begin
  if not cxRectIsEqual(FBoundsRect, AValue) then
  begin
    FBoundsRect := AValue;
    ViewInfo.AnnotationsInfo.ChangedAnnotationKinds :=
      dxAllScrollbarAnnotationKinds;
    ClearRegion;
  end;
end;

procedure TcxScrollBarHelper.SetEnabled(AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    if not FEnabled then
      CancelMode;
  end;
end;

procedure TcxScrollBarHelper.SetKind(Value: TScrollBarKind);
begin
  if FKind <> Value then
  begin
    FKind := Value;
    // if not (csLoading in ComponentState) then
    // SetBounds(Left, Top, Height, Width)
    // else
    // ViewInfo.Calculate;
    // Invalidate;
  end;
end;

procedure TcxScrollBarHelper.SetLargeChange(Value: TScrollBarInc);
begin
  FLargeChange := Value;
end;

procedure TcxScrollBarHelper.SetMax(Value: Integer);
begin
  SetScrollParams(FMin, Value, FPosition, FPageSize, FVisible);
end;

procedure TcxScrollBarHelper.SetMin(Value: Integer);
begin
  SetScrollParams(Value, FMax, FPosition, FPageSize, FVisible);
end;

procedure TcxScrollBarHelper.SetPageSize(Value: Integer);
begin
  SetScrollParams(FMin, FMax, FPosition, Value, FVisible);
end;

procedure TcxScrollBarHelper.SetPosition(Value: Integer);
begin
  SetScrollParams(FMin, FMax, Value, FPageSize, FVisible);
end;

procedure TcxScrollBarHelper.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    if Visible then
      ViewInfo.CalculateAnnotationInfos
    else
      CancelMode;
  end;
end;

procedure TcxScrollBarHelper.SetSmallChange(Value: TScrollBarInc);
begin
  FSmallChange := Value;
end;

procedure TcxScrollBarHelper.SetUnlimitedTracking(Value: Boolean);
begin
  FUnlimitedTracking := Value;
end;

procedure TcxScrollBarHelper.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TcxScrollBarHelper.ClearRegion;
begin
  VisibleRgn := 0;
end;

procedure TcxScrollBarHelper.DoPaint(ACanvas: TcxCanvas);
begin
  FPainter.Paint(ACanvas);
end;

function TcxScrollBarHelper.GetControllerClass: TcxScrollBarControllerClass;
begin
  Result := TcxScrollBarController;
end;

function TcxScrollBarHelper.GetDefaultScrollbarAnnotationColor(AAnnotationKind
  : TdxScrollbarAnnotationKind): TdxAlphaColor;
begin
  case AAnnotationKind of
    dxSelectedRowScrollbarAnnotationID:
      Result := LookAndFeelPainter.DefaultSelectedScrollbarAnnotationColor;
    dxFocusedRowScrollbarAnnotationID:
      Result := LookAndFeelPainter.DefaultFocusedScrollbarAnnotationColor;
    dxErrorScrollbarAnnotationID:
      Result := LookAndFeelPainter.DefaultErrorScrollbarAnnotationColor;
    dxSearchResultScrollbarAnnotationID:
      Result := LookAndFeelPainter.DefaultSearchResultAnnotationColor;
  else
    Result := LookAndFeelPainter.DefaultCustomScrollbarAnnotationColor;
  end;
end;

function TcxScrollBarHelper.GetPainterClass: TcxScrollBarPainterClass;
begin
  Result := TcxScrollBarPainter;
end;

function TcxScrollBarHelper.GetScaleFactor: TdxScaleFactor;
begin
  Result := Owner.GetScaleFactor;
end;

function TcxScrollBarHelper.GetScrollBarSize: TSize;
begin
  Result := GetScaledScrollBarSize(ScaleFactor);
end;

function TcxScrollBarHelper.GetViewInfoClass: TcxScrollBarViewInfoClass;
begin
  Result := TcxScrollBarViewInfo;
end;

function TcxScrollBarHelper.Handle: THandle;
begin
  Result := FOwner.GetControl.Handle;
end;

function TcxScrollBarHelper.HandleAllocated: Boolean;
begin
  Result := FOwner.GetControl.HandleAllocated;
end;

procedure TcxScrollBarHelper.Invalidate;
begin
  InvalidateRect(BoundsRect);
end;

procedure TcxScrollBarHelper.InvalidateRect(const ARect: TRect;
  AEraseBackground: Boolean = False);
var
  ARegion: TcxRegion;
begin
  if HandleAllocated then
    if IsNonClient then
      RefreshNCPart(ARect)
    else
    begin
      ARegion := GetTempVisibleRegion;
      try
        ARegion.Combine(ARect, roIntersect);
        ARegion.Offset(GetOwnerOrigin);
        InvalidateRgn(Handle, ARegion.Handle, AEraseBackground);
      finally
        ARegion.Free;
      end;
    end;
end;

function TcxScrollBarHelper.IsOwnerControlCapture: Boolean;
begin
  Result := GetCaptureControl = FOwner.GetControl;
end;

function TcxScrollBarHelper.IsRightToLeft: Boolean;
var
  AControl: TWinControl;
begin
  AControl := Owner.GetControl;
  if Kind = sbHorizontal then
    Result := AControl.UseRightToLeftAlignment
  else
    Result := AControl.UseRightToLeftScrollBar;
end;

procedure TcxScrollBarHelper.MoveToCurrentAnnotation;
var
  AHandled: Boolean;
begin
  if Controller.HotAnnotationRowIndex <> -1 then
  begin
    AHandled := False;
    FAnnotations.Click(Controller.FHotAnnotationRowIndexLists, AHandled);
    if not AHandled then
      FAnnotations.CurrentRecordIndex := Controller.HotAnnotationRowIndex;
  end;
end;

function TcxScrollBarHelper.ScreenToClient(const APoint: TPoint): TPoint;
begin
  Result := FOwner.GetControl.ScreenToClient(APoint);
  TranslateCoords(Result.X, Result.Y);
end;

procedure TcxScrollBarHelper.Scroll(ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  if Assigned(FOnScroll) then
    FOnScroll(Self, ScrollCode, ScrollPos);
end;

procedure TcxScrollBarHelper.TranslateCoords(var X, Y: Integer);
var
  AOrigin: TPoint;
begin
  AOrigin := GetOwnerOrigin;
  Dec(X, AOrigin.X);
  Dec(Y, AOrigin.Y);
end;

function TcxScrollBarHelper.GetAllowOwnerScrollOnDrag: Boolean;
begin
  Result := (Owner.GetControl is TcxControl) and
    TcxControlAccess(Owner.GetControl).AllowScrollContentOnDrag;
end;

function TcxScrollBarHelper.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := LookAndFeel.GetAvailablePainter(totScrollBar);
end;

function TcxScrollBarHelper.GetLookAndFeel: TcxLookAndFeel;
begin
  Result := FOwner.GetLookAndFeel;
end;

function TcxScrollBarHelper.GetTempVisibleRegion: TcxRegion;
begin
  if VisibleRgn <> 0 then
  begin
    Result := TcxRegion.Create;
    Result.Combine(VisibleRgn, roSet);
  end
  else
    Result := TcxRegion.Create(FBoundsRect);
end;

function TcxScrollBarHelper.NeedBuffering: Boolean;
begin
  Result := not IsWinVistaOrLater;
end;

function TcxScrollBarHelper.PtInVisibleRgn(const APoint: TPoint): Boolean;
var
  P: TPoint;
begin
  P := cxPointOffset(APoint, GetOwnerOrigin, False);
  if VisibleRgn <> 0 then
    Result := PtInRegion(VisibleRgn, P.X, P.Y)
  else
    Result := PtInRect(BoundsRect, P);
end;

procedure TcxScrollBarHelper.RefreshNCPart(const ARect: TRect);
begin
  cxRedrawWindow(Handle, ARect, RDW_INVALIDATE or RDW_FRAME);
end;

procedure TcxScrollBarHelper.SetVisibleRgn(const Value: HRGN);
begin
  if FVisibleRgn <> 0 then
    DeleteObject(FVisibleRgn);
  FVisibleRgn := Value;
end;

{ TcxScrollBarPainter }

constructor TcxScrollBarPainter.Create(AScrollBar: TcxScrollBarHelper);
begin
  inherited Create;
  FScrollbar := AScrollBar;
end;

destructor TcxScrollBarPainter.Destroy;
begin
  FScrollbar := nil;
  inherited Destroy;
end;

function TcxScrollBarPainter.IsButtonHotTrack: Boolean;
begin
  Result := LookAndFeelPainter.IsButtonHotTrack;
end;

procedure TcxScrollBarPainter.DoDrawScrollBarPart(ACanvas: TcxCanvas;
  const R: TRect; APart: TcxScrollBarPart; AState: TcxButtonState);
begin
  ACanvas.SaveState;
  try
    ACanvas.UseRightToLeftAlignment := FScrollbar.IsRightToLeft;
    LookAndFeelPainter.DrawScaledScrollBarPart(ACanvas,
      FScrollbar.Kind = sbHorizontal, R, APart, AState, ScaleFactor);
  finally
    ACanvas.RestoreState;
  end;
end;

procedure TcxScrollBarPainter.DrawScrollbarAnnotation(AGPCanvas: TdxGPGraphics;
  AInfo: TcxScrollbarAnnotationInfo);
begin
  AGPCanvas.FillRectangle(AInfo.Bounds, AInfo.Color);
end;

procedure TcxScrollBarPainter.DoDrawScrollbarAnnotations
  (AGPCanvas: TdxGPGraphics);
var
  I: Integer;
  AKind: TdxScrollbarAnnotationKind;
  AList: TObjectList<TcxScrollbarAnnotationInfo>;
begin
  for AKind := Low(TdxScrollbarAnnotationKind)
    to High(TdxScrollbarAnnotationKind) do
    if ViewInfo.AnnotationsInfo.Infos.TryGetValue(AKind, AList) then
      for I := 0 to AList.Count - 1 do
        DrawScrollbarAnnotation(AGPCanvas, AList[I]);
end;

procedure TcxScrollBarPainter.DrawScrollbarAnnotations(ACanvas: TcxCanvas);
begin
  dxGPPaintCanvas.BeginPaint(ACanvas.Handle, ViewInfo.AnnotationsInfo.Bounds);
  try
    DoDrawScrollbarAnnotations(dxGPPaintCanvas);
  finally
    dxGPPaintCanvas.EndPaint;
  end;
end;

procedure TcxScrollBarPainter.DrawScrollBarBackground(ACanvas: TcxCanvas;
  const R: TRect);
begin
  ACanvas.SaveState;
  try
    ACanvas.UseRightToLeftAlignment := FScrollbar.IsRightToLeft;
    ACanvas.Brush := ScrollBar.Owner.GetControl.Brush;
    ACanvas.FillRect(R);
    LookAndFeelPainter.DrawScaledScrollBarBackground(ACanvas, R,
      FScrollbar.Kind = sbHorizontal, ScaleFactor);
  finally
    ACanvas.RestoreState;
  end;
end;

procedure TcxScrollBarPainter.DrawScrollBarPart(ACanvas: TcxCanvas;
  APart: TcxScrollBarPart);
var
  R: TRect;
begin
  R := ViewInfo.PartInfo[APart].Bounds;
  if not IsRectEmpty(R) and not dxFader.DrawFadeImage
    (ViewInfo.PartInfo[APart].FadingHelper, ACanvas.Handle, R) then
    DoDrawScrollBarPart(ACanvas, R, APart, ViewInfo.PartInfo[APart].State);
end;

function TcxScrollBarPainter.FadingAvailable: Boolean;
begin
  Result := IsButtonHotTrack and (LookAndFeelPainter.LookAndFeelStyle
    in [lfsSkin, lfsNative]);
end;

function TcxScrollBarPainter.GetMinThumbnailSize: Integer;
begin
  Result := LookAndFeelPainter.ScaledScrollBarMinimalThumbSize
    (FScrollbar.Kind = sbVertical, ScaleFactor);
end;

function TcxScrollBarPainter.GetLookAndFeelPainter: TcxCustomLookAndFeelPainter;
begin
  Result := FScrollbar.LookAndFeelPainter;
end;

function TcxScrollBarPainter.GetScaleFactor: TdxScaleFactor;
begin
  Result := ScrollBar.ScaleFactor;
end;

function TcxScrollBarPainter.GetViewInfo: TcxScrollBarViewInfo;
begin
  Result := FScrollbar.ViewInfo;
end;

procedure TcxScrollBarPainter.Paint(ACanvas: TcxCanvas);
var
  APart: TcxScrollBarPart;
  ABrushOrigin, ASaveBrushOrigin: TPoint;
  ANeedBrushOriginChange: Boolean;
begin
  ABrushOrigin := ScrollBar.GetOwnerOrigin;
  ANeedBrushOriginChange := not ABrushOrigin.IsZero;
  if ANeedBrushOriginChange then
  begin
    ABrushOrigin.Invert;
    SetBrushOrgEx(ACanvas.Handle, ABrushOrigin.X, ABrushOrigin.Y,
      @ASaveBrushOrigin);
  end;
  DrawScrollBarBackground(ACanvas, ViewInfo.Bounds);
  for APart := sbpLineUp to sbpPageDown do
    DrawScrollBarPart(ACanvas, APart);
  if (ScrollBar.Annotations <> nil) and
    not cxRectIsEmpty(ViewInfo.AnnotationsInfo.Bounds) then
    DrawScrollbarAnnotations(ACanvas);
  if ANeedBrushOriginChange then
    SetBrushOrgEx(ACanvas.Handle, ASaveBrushOrigin.X, ASaveBrushOrigin.Y, nil);
end;

end.
