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

unit dxGanttControlViewTimeline;

{$I cxVer.inc}
{$I dxGanttControl.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes, System.Contnrs,
{$ENDIF}
  SysUtils, Windows, Messages, MultiMon, Types, Forms, Controls, Graphics,
  Dialogs,
  Classes, Themes, Generics.Defaults, Generics.Collections, StdCtrls, Math,
  dxCore, dxCoreClasses, cxClasses, cxControls, cxCustomCanvas, cxGeometry,
  cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, dxTouch,
  dxGanttControlCommands,
  dxGanttControlCustomSheet,
  dxGanttControlCustomClasses,
  dxGanttControlCustomView,
  dxGanttControlDataModel,
  dxGanttControlTasks,
  dxGanttControlCalendars,
  dxGanttControlCustomDataModel;

type
  TdxGanttControlTimelineView = class;
  TdxGanttControlViewTimelineViewInfo = class;
  TdxGanttControlTimelineViewDataProvider = class;
  TdxGanttControlTimelineViewController = class;

  TdxGanttControlTimelineViewTimescaleUnit = (Automatic, Hours, Days, Weeks,
    Months, Quarters, Years);

  { TdxGanttControlTimelineTaskViewInfoCachedValues }

  TdxGanttControlTimelineTaskViewInfoCachedValues = class
    (TdxGanttControlTaskViewInfoCachedValues) // for internal use
  strict private
  const
    FDefaultMaxCaptionWidth = 150;
    FDefaultCaptionLineLength = 15;
  protected const
    FCaptionLeftOffsetCount = 4;
    FCaptionBottomOffsetCount = 2;
    FCaptionTopOffsetCount = 1;
  strict private
    FCaptionBottomOffset: Integer;
    FCaptionLeftOffset: Integer;
    FCaptionTopOffset: Integer;
    FMaxCaptionWidth: Integer;
    FLineLength: Integer;
    FScaledMinTaskWidth: Integer;
  public
    procedure Update(AViewInfo: TdxGanttControlViewTimelineViewInfo);
      reintroduce;
    property CaptionBottomOffset: Integer read FCaptionBottomOffset;
    property CaptionLeftOffset: Integer read FCaptionLeftOffset;
    property CaptionTopOffset: Integer read FCaptionTopOffset;
    property MaxCaptionWidth: Integer read FMaxCaptionWidth;
    property LineLength: Integer read FLineLength;
    property ScaledMinTaskWidth: Integer read FScaledMinTaskWidth;
  end;

  { TdxGanttControlTimelineTaskViewInfo }

  TdxGanttControlTimelineTaskViewInfo = class(TdxGanttControlTaskCustomViewInfo)
  protected
    function GetBoldFont: TcxCanvasBasedFont;
    function GetFont: TcxCanvasBasedFont;
    function InternalGetCachedValues
      : TdxGanttControlTimelineTaskViewInfoCachedValues; inline;
  strict private
    FCaptionLayout: TcxCanvasBasedTextLayout;
    FInformationLayout: TcxCanvasBasedTextLayout;
    FInformation: string;
    FInformationBounds: TRect;

    function GetOwner: TdxGanttControlViewTimelineViewInfo; inline;
  protected
    function CalculateBarBounds(const R: TRect): TRect; override;
    function CalculateBarProgressBounds(const R: TRect): TRect; override;
    function CalculateCaptionBounds(const R: TRect): TRect; override;
    function CalculateInformationBounds: TRect; virtual;
    procedure SetTextsLayoutAttributes; virtual;

    procedure DoDraw; override;
    procedure DoDrawBar(ACanvas: TcxCustomCanvas;
      const ABounds, AProgressBounds: TRect); virtual;
    procedure DoDrawCaption; virtual;
    procedure DoDrawInformation; virtual;
    procedure DoDrawSelection; virtual;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
    function GetActualTextBounds(const ABounds: TRect): TRect; virtual;
    function GetCaptionFlags: Integer; virtual;
    function GetCaptionFont: TcxCanvasBasedFont; virtual;
    function GetClipBounds: TRect; override;
    function GetInformationFlags: Integer; virtual;
    function IsSelected: Boolean;

    procedure DoScroll(const DX, DY: Integer); override;

    function GetCachedValues: TdxGanttControlTaskViewInfoCachedValues; override;
    function GetCaption: string; override;
    function GetInformation: string; virtual;
    procedure Initialize; override;

    property CaptionLayout: TcxCanvasBasedTextLayout read FCaptionLayout;
    property Information: string read FInformation;
    property InformationLayout: TcxCanvasBasedTextLayout
      read FInformationLayout;
    property CachedValues: TdxGanttControlTimelineTaskViewInfoCachedValues
      read InternalGetCachedValues;
    property Owner: TdxGanttControlViewTimelineViewInfo read GetOwner;
  public
    constructor Create(AOwner: TdxGanttControlCustomItemViewInfo;
      ATask: TdxGanttControlTask); override;
    destructor Destroy; override;
    procedure Calculate(const R: TRect); override;

    property InformationBounds: TRect read FInformationBounds;
  end;

  { TdxGanttControlTimelineMilestoneTaskViewInfo }

  TdxGanttControlTimelineMilestoneTaskViewInfo = class
    (TdxGanttControlTimelineTaskViewInfo)
  private
    FIsBarVisible: Boolean;
    FIsLabelVisible: Boolean;
    FNeedEndEllipsis: Boolean;
    function GetLabelRect: TRect; inline;
    function GetVisible: Boolean;
  protected
    function CalculateBarBounds(const R: TRect): TRect; override;
    function CalculateBarProgressBounds(const R: TRect): TRect; override;
    function CalculateCaptionBounds(const R: TRect): TRect; override;
    function CalculateHitTest(const AHitTest: TdxGanttControlHitTest)
      : Boolean; override;
    function CalculateInformationBounds: TRect; override;
    procedure SetTextsLayoutAttributes; override;
    procedure DoDrawBar(ACanvas: TcxCustomCanvas;
      const ABounds, AProgressBounds: TRect); override;
    procedure DoDrawCaption; override;
    procedure DoDrawInformation; override;
    procedure DoDrawSelection; override;
    function GetActualTextBounds(const ABounds: TRect): TRect; override;
    function GetDefaultColor: TColor; override;
    function GetCaptionFlags: Integer; override;
    function GetCaptionFont: TcxCanvasBasedFont; override;
    function GetInformationFlags: Integer; override;
    function GetInformation: string; override;
    function GetTaskHeight: Integer; override;

    property IsBarVisible: Boolean read FIsBarVisible write FIsBarVisible;
    property IsLabelVisible: Boolean read FIsLabelVisible write FIsLabelVisible;
    property Visible: Boolean read GetVisible;
  public
    constructor Create(AOwner: TdxGanttControlCustomItemViewInfo;
      ATask: TdxGanttControlTask); override;
    procedure Calculate(const R: TRect); override;
  end;

  { TdxGanttControlTimelineScaleCell }

  TdxGanttControlTimelineScaleCell = class
    (TdxGanttControlCustomOwnedItemViewInfo)
  protected const
    FCaptionTopOffsetCount = 0;
    FCaptionLeftOffsetCount = 2;
    FCaptionRightOffsetCount = 3;
    FCaptionBottomOffsetCount = 2;
  strict private
    FCaption: string;
    FCaptionLayout: TcxCanvasBasedTextLayout;
    FDateTime: TDateTime;
    function GetOwner: TdxGanttControlViewTimelineViewInfo; inline;
  protected
    procedure DoDraw; override;
    function GetHintText: string; override;
    function HasHint: Boolean; override;
    procedure Initialize(ADateTime: TDateTime);
    procedure ResetCaption;

    property CaptionLayout: TcxCanvasBasedTextLayout read FCaptionLayout;
  public
    constructor Create(AOwner: TdxGanttControlCustomItemViewInfo); override;
    destructor Destroy; override;

    property Owner: TdxGanttControlViewTimelineViewInfo read GetOwner;
    property Caption: string read FCaption;
    property DateTime: TDateTime read FDateTime;
  end;

  { TdxGanttControlTimelineProjectCustomLabel }

  TdxGanttControlTimelineProjectCustomLabel = class
    (TdxGanttControlCustomOwnedItemViewInfo)
  strict private
    FCaptionLayout: TcxCanvasBasedTextLayout;
    FDateTimeLayout: TcxCanvasBasedTextLayout;
    FDateTime: string;
    function GetOwner: TdxGanttControlViewTimelineViewInfo; inline;
  protected
    FIsVertical: Boolean;
    procedure DoDraw; override;
    function GetCaption: string; virtual; abstract;
    function GetFlags: Integer; virtual;
    function GetFont: TcxCanvasBasedFont;
    function GetFontHeight: Integer;
    function GetHorizontalAlignment: Integer; virtual; abstract;
    function GetIntervalBetweenLines: Integer;
    function HasHint: Boolean; override;
    procedure Initialize(ADateTime: TDateTime);
    procedure UpdateTextLayoutFlags;

    property CaptionLayout: TcxCanvasBasedTextLayout read FCaptionLayout;
    property DateTimeLayout: TcxCanvasBasedTextLayout read FDateTimeLayout;
  public
    constructor Create(AOwner: TdxGanttControlCustomItemViewInfo); override;
    destructor Destroy; override;

    property Caption: string read GetCaption;
    property DateTime: string read FDateTime;
    property Owner: TdxGanttControlViewTimelineViewInfo read GetOwner;
  end;

  { TdxGanttControlTimelineStartLabel }

  TdxGanttControlTimelineStartLabel = class
    (TdxGanttControlTimelineProjectCustomLabel)
  protected
    function GetCaption: string; override;
    function GetHorizontalAlignment: Integer; override;
  end;

  { TdxGanttControlTimelineFinishLabel }

  TdxGanttControlTimelineFinishLabel = class
    (TdxGanttControlTimelineProjectCustomLabel)
  protected
    function GetCaption: string; override;
    function GetHorizontalAlignment: Integer; override;
  end;

  { TdxGanttTimelineScrollBars }

  TdxGanttTimelineScrollBars = class(TdxGanttControlCustomScrollBars)
  strict private
    function GetController: TdxGanttControlTimelineViewController; inline;
  protected
    procedure DoHScroll(ScrollCode: TScrollCode;
      var ScrollPos: Integer); override;
    procedure DoVScroll(ScrollCode: TScrollCode;
      var ScrollPos: Integer); override;
    procedure DoInitHScrollBarParameters; override;
    procedure DoInitVScrollBarParameters; override;
  public
    property Controller: TdxGanttControlTimelineViewController
      read GetController;
  end;

  { TdxGanttControlTimelineViewController }

  TdxGanttControlTimelineViewController = class
    (TdxGanttControlViewCustomController)
  strict private
    FFocusedTask: TdxGanttControlTask;
    FSelection: TdxFastObjectList;
  private
    FScrollBars: TdxGanttTimelineScrollBars;
    FScrollBarsCalculationCount: Integer;

    function GetHitTask: TdxGanttControlTask;
    function GetFirstTask: TdxGanttControlTask;
    function GetFirstTaskInRow: TdxGanttControlTask;
    function GetPreviousTask: TdxGanttControlTask;
    function GetPreviousTaskInColumn: TdxGanttControlTask;
    function GetNextTask: TdxGanttControlTask;
    function GetNextTaskInColumn: TdxGanttControlTask;
    function GetLastTask: TdxGanttControlTask;
    function GetLastTaskInRow: TdxGanttControlTask;
    function GetRegularMilestone(ABeginIndex: Integer; AForward: Boolean)
      : TdxGanttControlTask;
    function GetMilestones
      : TObjectList<TdxGanttControlTimelineMilestoneTaskViewInfo>;
    function GetTaskPlaces: TdxFastObjectList;
    function InternalGetViewInfo: TdxGanttControlViewTimelineViewInfo;
    procedure SetFocusedTask(ATask: TdxGanttControlTask);
  protected
    procedure DoDblClick; override;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure DoMouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; AIsIncrement: Boolean;
      const AMousePos: TPoint): Boolean; override;
    function HitAtTask: Boolean; inline;

    procedure Activated; override;
    procedure CalculateScrollBars;
    function CreateScrollBars: TdxGanttTimelineScrollBars; virtual;
    procedure Deactivated; override;
    procedure DoCreateScrollBars; override;
    procedure DoDestroyScrollBars; override;
    function GetTouchScrollUIOwner(const APoint: TPoint)
      : IdxTouchScrollUIOwner; override;
    function GetGestureClient(const APoint: TPoint): IdxGestureClient; override;
    procedure InitScrollbars; override;
    function IsMouseWheelHandleNeeded(const MousePos: TPoint): Boolean;
      override;
    function IsPanArea(const APoint: TPoint): Boolean; override;
    function ProcessNCSizeChanged: Boolean; override;
    procedure UnInitScrollbars; override;

    procedure ClearSelection;
    procedure CheckFocusedTask;
    procedure CheckSelection;
    procedure DoSelect(ATask: TdxGanttControlTask; AShift: TShiftState);
    function IsSelected(ATask: TdxGanttControlTask): Boolean; inline;
    procedure MakeVisible(ATask: TdxGanttControlTask);
    procedure ResetSelectionAndFocusedTask;

    function InitializeBuiltInPopupMenu(APopupMenu: TdxGanttControlPopupMenu;
      var P: TPoint): Boolean; override;
    procedure ShowTaskInformationDialog(ATask: TdxGanttControlTask);

    property HitTask: TdxGanttControlTask read GetHitTask;
    property Milestones
      : TObjectList<TdxGanttControlTimelineMilestoneTaskViewInfo>
      read GetMilestones;
    property TaskPlaces: TdxFastObjectList read GetTaskPlaces;
    property ScrollBars: TdxGanttTimelineScrollBars read FScrollBars;
    property ScrollBarsCalculationCount: Integer
      read FScrollBarsCalculationCount;
    property ViewInfo: TdxGanttControlViewTimelineViewInfo
      read InternalGetViewInfo;
  public
    constructor Create(AView: TdxGanttControlCustomView); override;
    destructor Destroy; override;

    property FocusedTask: TdxGanttControlTask read FFocusedTask
      write SetFocusedTask;
    property Selection: TdxFastObjectList read FSelection;
  end;

  { TdxGanttControlViewTimelineViewInfo }

  TdxGanttControlViewTimelineViewInfo = class(TdxGanttControlViewCustomViewInfo)
  protected const
    FMinTaskWidth = 1;
    FStartLabelLeftOffsetFactor = 5;
    FStartInfoRightOffsetCount = 3;
  strict private
    FActiveTasks: TObjectList<TdxGanttControlTimelineTaskViewInfo>;
    FActualTimescaleUnit: TdxGanttControlTimelineViewTimescaleUnit;
    FCalendar: TdxGanttControlCalendar;
    FClientRect: TRect;
    FContentHeight: Integer;
    FIsEmpty: Boolean;
    FIsRightToLeftConverted: Boolean;
    FMilestones: TObjectList<TdxGanttControlTimelineMilestoneTaskViewInfo>;
    FMilestonesAlreadyConverted: Boolean;
    FTaskArea: TRect;
    FTimescaleCells: TObjectList<TdxGanttControlTimelineScaleCell>;
    FPriorHorzScrollPageWidth: Integer;
    FPriorVertScrollPageHeight: Integer;
    FProjectFinishLabel: TdxGanttControlTimelineProjectCustomLabel;
    FProjectFinishLabelSize: TSize;
    FProjectStartLabel: TdxGanttControlTimelineProjectCustomLabel;
    FProjectStartLabelSize: TSize;
    FTimescaleUnitWidth: Integer;
    FTimeSlotCount: Real;
    FTopScrollPos: Integer;
    FLeftScrollPos: Integer;

    FTaskLayoutBuilder: TObject;

    function GetController: TdxGanttControlTimelineViewController; inline;
    function GetCurrentCalendar: TdxGanttControlCalendar;
    function GetDataProvider: TdxGanttControlTimelineViewDataProvider;
    function GetView: TdxGanttControlTimelineView;

    function GetContentWidth: Integer;
    function GetHorzScrollPageWidth: Integer;
    function GetVertScrollPageHeight: Integer;
    procedure SetLeftScrollPos(AValue: Integer);
    procedure SetTopScrollPos(AValue: Integer);
    //
    procedure InitializeCache;
    procedure CalculateClientRect(const R: TRect);
    function CalculateTaskViewInfos: Boolean;
    procedure TaskListAfterResetHandler(Sender: TObject);
    procedure UpdateSpecialCachedValues
      (AUnit: TdxGanttControlTimelineViewTimescaleUnit);
  protected
    FCachedBoldFont: TcxCanvasBasedFont;
    FCachedBoldFontHeight: Integer;
    FCachedFont: TcxCanvasBasedFont;
    FCachedFontHeight: Integer;
    FCachedScaledTextOffset: Integer;

    FCachedTimescaleCaptionFormat: string;
    FCachedTimescaleCaptionMaxWidth: Integer;
    FCachedTimescaleCaptionOffsetTop: Integer;
    FCachedTimescaleCaptionOffsetLeft: Integer;
    FCachedTimescaleCaptionOffsetRight: Integer;
    FCachedTimescaleCaptionOffsetBottom: Integer;
    FCachedTimescaleCellHeight: Integer;

    FDisplayScaleFactor: Double;
    FProjectStartAsMilleseconds: Int64;
    FProjectFinishAsMilleseconds: Int64;

    FTaskCachedValues: TdxGanttControlTimelineTaskViewInfoCachedValues;

    procedure CalculateContent(const R: TRect); virtual;
    procedure CalculateContentAreas(ALineCount: Integer;
      AHasMilestones: Boolean); virtual;
    procedure CalculateTaskBoundsLeftAndRight(const APlaceTimeStart,
      APlaceTimeFinish: Int64; var ALeft, ARight: Integer);
    procedure CalculateTaskBoundsTopAndBottom(const APlaceLineStart,
      APlaceLineFinish: Int64; var ATop, ABottom: Integer);
    procedure CalculateTimescaleCells; virtual;
    procedure CalculateTimescaleUnitWidth; virtual;
    procedure CalculateStartAndFinishInfoSize; virtual;
    procedure CheckMilestoneVisibility; virtual;
    procedure CheckRightToLeftAlignment; virtual;
    procedure ConvertActiveTasks(const AClientRect: TRect);
    procedure ConvertContentAreas(const AClientRect: TRect);
    procedure ConvertMilestones(const AClientRect: TRect);
    procedure ConvertTimescale(const AClientRect: TRect);
    procedure DoDraw; override;
    function GetActualScrollbarMode: TdxScrollbarMode;
    function GetDateTimeByPos(X: Integer): TDateTime;
    function GetMilestoneInfo(ATask: TdxGanttControlTask)
      : TdxGanttControlTimelineMilestoneTaskViewInfo;
    function GetProjectFinish: TDateTime; inline;
    function GetProjectStart: TDateTime; inline;
    function GetStartLabelOffsetLeft: Integer; virtual;
    function GetStartLabelOffsetRight: Integer; virtual;
    procedure PrepareContent; virtual;
    procedure Reset; override;

    procedure DoScroll(const DX, DY: Integer); override;
    procedure ResetLeftScrollPos;
    procedure ResetTopScrollPos;

    function CalculateHitTest(const AHitTest: TdxGanttControlHitTest)
      : Boolean; override;

    property Calendar: TdxGanttControlCalendar read FCalendar;
    property DataProvider: TdxGanttControlTimelineViewDataProvider
      read GetDataProvider;

    property Milestones
      : TObjectList<TdxGanttControlTimelineMilestoneTaskViewInfo>
      read FMilestones;
    property ActiveTasks: TObjectList<TdxGanttControlTimelineTaskViewInfo>
      read FActiveTasks;
    property ActualTimescaleUnit: TdxGanttControlTimelineViewTimescaleUnit
      read FActualTimescaleUnit;
    property IsRightToLeftConverted: Boolean read FIsRightToLeftConverted;
    property IsEmpty: Boolean read FIsEmpty;
    property ProjectFinishLabel: TdxGanttControlTimelineProjectCustomLabel
      read FProjectFinishLabel;
    property ProjectStartLabel: TdxGanttControlTimelineProjectCustomLabel
      read FProjectStartLabel;
    property TaskArea: TRect read FTaskArea;
    property TaskLayoutBuilder: TObject read FTaskLayoutBuilder;
    property TimescaleCells: TObjectList<TdxGanttControlTimelineScaleCell>
      read FTimescaleCells;
  public
    constructor Create(AView: TdxGanttControlCustomView); override;
    destructor Destroy; override;
    procedure Calculate(const R: TRect); override;
    procedure ViewChanged; override;

    property ClientRect: TRect read FClientRect;
    property ContentHeight: Integer read FContentHeight;
    property ContentWidth: Integer read GetContentWidth;
    property Controller: TdxGanttControlTimelineViewController
      read GetController;
    property HorzScrollPageWidth: Integer read GetHorzScrollPageWidth;
    property LeftScrollPos: Integer read FLeftScrollPos write SetLeftScrollPos;
    property TopScrollPos: Integer read FTopScrollPos write SetTopScrollPos;
    property VertScrollPageHeight: Integer read GetVertScrollPageHeight;
    property View: TdxGanttControlTimelineView read GetView;
  end;

  { TdxGanttControlTimelineViewDataProvider }

  TdxGanttControlTimelineViewDataProvider = class
    (TdxGanttControlCustomDataProvider)
  strict private
    FDataModel: TdxGanttControlDataModel;

    procedure TaskBeforeResetHandler(Sender: TObject);
    procedure TaskChangedHandler(Sender: TObject;
      const AItem: TdxGanttControlModelElementListItem;
      AAction: TCollectionNotification);

    function GetTask(Index: Integer): TdxGanttControlTask; inline;
  protected
    function CanAddItem(AItem: TObject): Boolean; override;
    function GetDataItemCount: Integer; override;
    function GetDataItem(Index: Integer): TObject; override;
    property Tasks[Index: Integer]: TdxGanttControlTask read GetTask;
  public
    constructor Create(AControl: TdxGanttControlBase); override;
    destructor Destroy; override;

    property DataModel: TdxGanttControlDataModel read FDataModel;
  end;

  { TdxGanttControlTimelineView }

  TdxGanttControlTimelineView = class(TdxGanttControlCustomView)
  private
    FShowOnlyExplicitlyAddedTasks: Boolean;
    FTimescaleUnit: TdxGanttControlTimelineViewTimescaleUnit;
    FTimescaleUnitMinWidth: Integer;
    function GetViewInfo: TdxGanttControlViewTimelineViewInfo;
    procedure SetShowOnlyExplicitlyAddedTasks(const AValue: Boolean);
    procedure SetTimescaleUnit
      (AValue: TdxGanttControlTimelineViewTimescaleUnit);
    procedure SetTimescaleUnitMinWidth(AValue: Integer);
  protected
    function CreateController: TdxGanttControlViewCustomController; override;
    function CreateDataProvider: TdxGanttControlCustomDataProvider; override;
    function CreateViewInfo: TdxGanttControlViewCustomViewInfo; override;
    function GetType: TdxGanttControlViewType; override;
    procedure DoReset; override;

    property ViewInfo: TdxGanttControlViewTimelineViewInfo read GetViewInfo;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property ShowOnlyExplicitlyAddedTasks: Boolean
      read FShowOnlyExplicitlyAddedTasks write SetShowOnlyExplicitlyAddedTasks
      default False;
    property TimescaleUnit: TdxGanttControlTimelineViewTimescaleUnit
      read FTimescaleUnit write SetTimescaleUnit
      default TdxGanttControlTimelineViewTimescaleUnit.Automatic;
    property TimescaleUnitMinWidth: Integer read FTimescaleUnitMinWidth
      write SetTimescaleUnitMinWidth default 50;
  end;

implementation

uses
  DateUtils, RTLConsts,
  dxTypeHelpers, dxCultureInfo, cxDrawTextUtils,
  dxGDIPlusClasses,
  dxGanttControl,
  dxGanttControlStrs,
  dxGanttControlUtils,
  dxGanttControlViewCommands,
  dxGanttControlTaskInformationDialog;

function dxGetDateTimeAsString(ADateTime: TDateTime;
  AHoursMustBe: Boolean = False): string;
var
  AFormat: string;
begin
  AFormat := TdxCultureInfo.CurrentCulture.FormatSettings.ShortDateFormat;
  if AHoursMustBe or (TimeOf(ADateTime) > 0) then
    AFormat := AFormat + ' ' + TdxCultureInfo.CurrentCulture.FormatSettings.
      ShortTimeFormat;
  Result := FormatDateTime(AFormat, ADateTime);
end;

type
  TdxGanttControlHitTestAccess = class(TdxGanttControlHitTest);
  TdxCustomGanttControlAccess = class(TdxCustomGanttControl);

  { TdxRectInt64 }

  TdxRectInt64 = record
  private
    function GetHeight: Int64;
    function GetWidth: Int64;
  public
    Left, Top, Right, Bottom: Int64;
    constructor Create(const ALeft, ATop, ARight, ABottom: Int64);
    procedure MoveToTop(Y: Int64);

    property Height: Int64 read GetHeight;
    property Width: Int64 read GetWidth;
  end;

  { TdxGanttTimelineOccupiedArea }

  TdxGanttTimelineOccupiedArea = class
  private
    FBounds: TdxRectInt64;

    function LeftOf(const ABounds: TdxRectInt64): Boolean; inline;
    function RightOf(const ABounds: TdxRectInt64): Boolean; inline;
    function TopOf(const ABounds: TdxRectInt64): Boolean; inline;
    function BottomOf(const ABounds: TdxRectInt64): Boolean; inline;
    function CanMergeHorizontal(const ABounds: TdxRectInt64): Boolean; inline;
    function CanMergeVertical(const ABounds: TdxRectInt64): Boolean; inline;
  public
    constructor Create(const ABounds: TdxRectInt64);
    function Merge(const ABounds: TdxRectInt64): Boolean; overload;
    function Merge(AOccupiedArea: TdxGanttTimelineOccupiedArea)
      : Boolean; overload;

    property Bounds: TdxRectInt64 read FBounds;
  end;

  { TdxGanttTimelineOccupiedAreaManager }

  TdxGanttTimelineOccupiedAreaManager = class
  private
    FAreas: TdxFastObjectList;

    function GetItem(AIndex: Integer): TdxGanttTimelineOccupiedArea; inline;
  protected
    function MergeRecursively(var AArea: TdxGanttTimelineOccupiedArea): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function AddArea(const ABounds: TdxRectInt64): TdxGanttTimelineOccupiedArea;
    function GetFreeBounds(const ATaskPlaceBounds: TdxRectInt64): TdxRectInt64;

    property Items[Index: Integer]: TdxGanttTimelineOccupiedArea read GetItem;
  end;

  { TdxGanttControlTimelineTaskPlace }

  TdxGanttControlTimelineTaskPlace = class
  public
    LineFinish: Integer;
    LineStart: Integer;
    Task: TdxGanttControlTask;
    TimeFinish: Int64;
    TimeStart: Int64;
    constructor Create(ATask: TdxGanttControlTask);
    function IntersectHorz(APlace: TdxGanttControlTimelineTaskPlace)
      : Boolean; inline;
    function IntersectVert(APlace: TdxGanttControlTimelineTaskPlace)
      : Boolean; inline;
  end;

  { TdxGanttControlTimelineTaskLayoutBuilder }

  TdxGanttControlTimelineTaskLayoutBuilder = class
  private
    FLineCount: Integer;
    FMilestonePlaces: TdxFastObjectList;
    FTaskPlaces: TdxFastObjectList;
    function GetMilestonePlace(AIndex: Integer)
      : TdxGanttControlTimelineTaskPlace; inline;
    function GetTaskPlace(AIndex: Integer)
      : TdxGanttControlTimelineTaskPlace; inline;
  protected
    property MilestonePlace[Index: Integer]: TdxGanttControlTimelineTaskPlace
      read GetMilestonePlace;
    property MilestonePlaces: TdxFastObjectList read FMilestonePlaces;
  public
    constructor Create;
    destructor Destroy; override;
    procedure PopulatePlaces(ATasks: TdxGanttControlTasks;
      AShowOnlyExplicitlyAddedTasks: Boolean);
    procedure Calculate;
    function GetPlace(ATask: TdxGanttControlTask)
      : TdxGanttControlTimelineTaskPlace;

    property LineCount: Integer read FLineCount;
    property TaskPlace[Index: Integer]: TdxGanttControlTimelineTaskPlace
      read GetTaskPlace;
    property TaskPlaces: TdxFastObjectList read FTaskPlaces;
  end;

  { TdxRectInt64 }

constructor TdxRectInt64.Create(const ALeft, ATop, ARight, ABottom: Int64);
begin
  Left := ALeft;
  Right := ARight;
  Top := ATop;
  Bottom := ABottom;
end;

function TdxRectInt64.GetHeight: Int64;
begin
  Result := Bottom - Top;
end;

function TdxRectInt64.GetWidth: Int64;
begin
  Result := Right - Left;
end;

procedure TdxRectInt64.MoveToTop(Y: Int64);
begin
  Bottom := Bottom + Y - Top;
  Top := Y;
end;

{ TcxSchedulerOccupiedArea }

constructor TdxGanttTimelineOccupiedArea.Create(const ABounds: TdxRectInt64);
begin
  inherited Create;
  FBounds := ABounds;
end;

function TdxGanttTimelineOccupiedArea.LeftOf(const ABounds
  : TdxRectInt64): Boolean;
begin
  Result := FBounds.Right + 1 = ABounds.Left;
end;

function TdxGanttTimelineOccupiedArea.RightOf(const ABounds
  : TdxRectInt64): Boolean;
begin
  Result := FBounds.Left - 1 = ABounds.Right;
end;

function TdxGanttTimelineOccupiedArea.TopOf(const ABounds
  : TdxRectInt64): Boolean;
begin
  Result := FBounds.Bottom + 1 = ABounds.Top;
end;

function TdxGanttTimelineOccupiedArea.BottomOf(const ABounds
  : TdxRectInt64): Boolean;
begin
  Result := FBounds.Top - 1 = ABounds.Bottom;
end;

function TdxGanttTimelineOccupiedArea.CanMergeHorizontal(const ABounds
  : TdxRectInt64): Boolean;
begin
  Result := (FBounds.Top = ABounds.Top) and (FBounds.Height = ABounds.Height)
    and (RightOf(ABounds) or LeftOf(ABounds));
end;

function TdxGanttTimelineOccupiedArea.CanMergeVertical(const ABounds
  : TdxRectInt64): Boolean;
begin
  Result := (FBounds.Left = ABounds.Left) and (FBounds.Width = ABounds.Width)
    and (BottomOf(ABounds) or TopOf(ABounds));
end;

function TdxGanttTimelineOccupiedArea.Merge(const ABounds
  : TdxRectInt64): Boolean;
begin
  Result := CanMergeVertical(ABounds);
  if Result then
    if BottomOf(ABounds) then
      FBounds.Top := ABounds.Top
    else
      FBounds.Bottom := ABounds.Bottom
  else
  begin
    Result := CanMergeHorizontal(ABounds);
    if Result then
      if RightOf(ABounds) then
        FBounds.Left := ABounds.Left
      else
        FBounds.Right := ABounds.Right;
  end;
end;

function TdxGanttTimelineOccupiedArea.Merge(AOccupiedArea
  : TdxGanttTimelineOccupiedArea): Boolean;
begin
  Result := (Self <> AOccupiedArea) and Merge(AOccupiedArea.Bounds);
end;

{ TdxGanttTimelineOccupiedAreaManager }

constructor TdxGanttTimelineOccupiedAreaManager.Create;
begin
  inherited Create;
  FAreas := TdxFastObjectList.Create;
end;

destructor TdxGanttTimelineOccupiedAreaManager.Destroy;
begin
  FreeAndNil(FAreas);
  inherited Destroy;
end;

function TdxGanttTimelineOccupiedAreaManager.AddArea(const ABounds
  : TdxRectInt64): TdxGanttTimelineOccupiedArea;
var
  I: Integer;
  AOldArea: TdxGanttTimelineOccupiedArea;
begin
  Result := nil;
  for I := 0 to FAreas.Count - 1 do
  begin
    AOldArea := Items[I];
    if AOldArea.Merge(ABounds) then
    begin
      Result := AOldArea;
      Break;
    end;
  end;
  if Result = nil then
  begin
    Result := TdxGanttTimelineOccupiedArea.Create(ABounds);
    FAreas.Add(Result);
  end
  else
    MergeRecursively(Result);
end;

function TdxGanttTimelineOccupiedAreaManager.MergeRecursively
  (var AArea: TdxGanttTimelineOccupiedArea): Boolean;
var
  I: Integer;
  AOldArea: TdxGanttTimelineOccupiedArea;
begin
  Result := False;
  for I := 0 to FAreas.Count - 1 do
  begin
    AOldArea := Items[I];
    if AOldArea.Merge(AArea) then
    begin
      Result := True;
      FAreas.Remove(AArea);
      AArea := AOldArea;
      Break;
    end;
  end;
  if Result then
    while MergeRecursively(AArea) do;
end;

function TdxGanttTimelineOccupiedAreaManager.GetItem(AIndex: Integer)
  : TdxGanttTimelineOccupiedArea;
begin
  Result := TdxGanttTimelineOccupiedArea(FAreas[AIndex]);
end;

function TdxGanttTimelineOccupiedAreaManager.GetFreeBounds
  (const ATaskPlaceBounds: TdxRectInt64): TdxRectInt64;
var
  I: Integer;
  AOccupiedArea: TdxGanttTimelineOccupiedArea;
begin
  Result := ATaskPlaceBounds;
  for I := 0 to FAreas.Count - 1 do
  begin
    AOccupiedArea := Items[I];
    if (AOccupiedArea.Bounds.Right >= Result.Left) and
      (AOccupiedArea.Bounds.Left <= Result.Right) and
      (AOccupiedArea.Bounds.Bottom >= Result.Top) and
      (AOccupiedArea.Bounds.Top <= Result.Bottom) then
    begin
      Result.MoveToTop(AOccupiedArea.Bounds.Bottom + 1);
      Result := GetFreeBounds(Result);
      Break;
    end;
  end;
end;

{ TdxGanttControlTimelineTaskPlace }

constructor TdxGanttControlTimelineTaskPlace.Create(ATask: TdxGanttControlTask);
begin
  inherited Create;
  Task := ATask;
  TimeStart := TdxGanttControlUtils.DateTimeToMilliseconds(Task.Start);
  TimeFinish := TdxGanttControlUtils.DateTimeToMilliseconds(Task.Finish) - 1;
  LineStart := 0;
  LineFinish := 0;
end;

function TdxGanttControlTimelineTaskPlace.IntersectHorz
  (APlace: TdxGanttControlTimelineTaskPlace): Boolean;
begin
  Result := (APlace.TimeFinish >= TimeStart) and
    (APlace.TimeStart <= TimeFinish);
end;

function TdxGanttControlTimelineTaskPlace.IntersectVert
  (APlace: TdxGanttControlTimelineTaskPlace): Boolean;
begin
  Result := (APlace.LineFinish >= LineStart) and
    (APlace.LineStart <= LineFinish);
end;

{ TdxGanttControlTimelineTaskLayoutBuilder }

constructor TdxGanttControlTimelineTaskLayoutBuilder.Create;
begin
  inherited Create;
  FTaskPlaces := TdxFastObjectList.Create;
  FMilestonePlaces := TdxFastObjectList.Create;
  FLineCount := 1;
end;

destructor TdxGanttControlTimelineTaskLayoutBuilder.Destroy;
begin
  FreeAndNil(FMilestonePlaces);
  FreeAndNil(FTaskPlaces);
  inherited Destroy;
end;

function TdxGanttControlTimelineTaskLayoutBuilder.GetMilestonePlace
  (AIndex: Integer): TdxGanttControlTimelineTaskPlace;
begin
  Result := TdxGanttControlTimelineTaskPlace(FMilestonePlaces[AIndex]);
end;

function TdxGanttControlTimelineTaskLayoutBuilder.GetTaskPlace(AIndex: Integer)
  : TdxGanttControlTimelineTaskPlace;
begin
  Result := TdxGanttControlTimelineTaskPlace(FTaskPlaces[AIndex]);
end;

function TdxGanttControlTimelineTaskLayoutBuilder.GetPlace
  (ATask: TdxGanttControlTask): TdxGanttControlTimelineTaskPlace;
var
  I: Integer;
begin
  Result := nil;
  if ATask.Milestone then
    for I := 0 to FMilestonePlaces.Count - 1 do
      if TdxGanttControlTimelineTaskPlace(FMilestonePlaces[I]).Task = ATask then
        Exit(TdxGanttControlTimelineTaskPlace(FMilestonePlaces[I]))
      else
    else
      for I := 0 to FTaskPlaces.Count - 1 do
        if TdxGanttControlTimelineTaskPlace(FTaskPlaces[I]).Task = ATask then
          Exit(TdxGanttControlTimelineTaskPlace(FTaskPlaces[I]));
end;

function Compare(Left, Right: Pointer): Integer;
begin
  if TdxGanttControlTimelineTaskPlace(Left)
    .Task = TdxGanttControlTimelineTaskPlace(Right).Task then
    Exit(0);
  if TdxGanttControlTimelineTaskPlace(Left).TimeStart <
    TdxGanttControlTimelineTaskPlace(Right).TimeStart then
    Exit(-1);
  if TdxGanttControlTimelineTaskPlace(Left)
    .TimeStart = TdxGanttControlTimelineTaskPlace(Right).TimeStart then
    if TdxGanttControlTimelineTaskPlace(Left).Task.ID <
      TdxGanttControlTimelineTaskPlace(Right).Task.ID then
      Exit(-1)
    else
      Exit(1)
  else
    Result := 1;
end;

procedure TdxGanttControlTimelineTaskLayoutBuilder.PopulatePlaces
  (ATasks: TdxGanttControlTasks; AShowOnlyExplicitlyAddedTasks: Boolean);

  function CanDisplayOnTimeline(ATask: TdxGanttControlTask): Boolean;
  begin
    Result := not AShowOnlyExplicitlyAddedTasks;
    if Result then
      Result := not ATask.IsValueAssigned
        (TdxGanttTaskAssignedValue.DisplayOnTimeline) or ATask.DisplayOnTimeline
    else
      Result := ATask.IsValueAssigned
        (TdxGanttTaskAssignedValue.DisplayOnTimeline) and
        ATask.DisplayOnTimeline;
  end;

var
  I: Integer;
begin
  for I := 1 to ATasks.Count - 1 do
    if not(ATasks[I].IsValueAssigned(TdxGanttTaskAssignedValue.Blank) and
      ATasks[I].Blank) and CanDisplayOnTimeline(ATasks[I]) then
      if ATasks[I].Milestone then
        FMilestonePlaces.Add(TdxGanttControlTimelineTaskPlace.Create(ATasks[I]))
      else
        FTaskPlaces.Add(TdxGanttControlTimelineTaskPlace.Create(ATasks[I]));
  FTaskPlaces.Sort(Compare);
  FMilestonePlaces.Sort(Compare);
end;

procedure TdxGanttControlTimelineTaskLayoutBuilder.Calculate;
var
  I: Integer;
  ATaskPlace: TdxGanttControlTimelineTaskPlace;
  AManager: TdxGanttTimelineOccupiedAreaManager;
  ABounds: TdxRectInt64;
begin
  AManager := TdxGanttTimelineOccupiedAreaManager.Create;
  try
    for I := 0 to TaskPlaces.Count - 1 do
    begin
      ATaskPlace := TaskPlace[I];
      ABounds := AManager.GetFreeBounds
        (TdxRectInt64.Create(ATaskPlace.TimeStart, ATaskPlace.LineStart,
        ATaskPlace.TimeFinish, ATaskPlace.LineFinish));
      ATaskPlace.LineStart := ABounds.Top;
      ATaskPlace.LineFinish := ABounds.Bottom;
      AManager.AddArea(ABounds);
      FLineCount := Max(FLineCount, ATaskPlace.LineFinish + 1);
    end;
  finally
    AManager.Free;
  end;
end;

{ TdxGanttControlTimelineTaskViewInfoCachedValues }

procedure TdxGanttControlTimelineTaskViewInfoCachedValues.Update
  (AViewInfo: TdxGanttControlViewTimelineViewInfo);
var
  APainter: TcxCustomLookAndFeelPainter;
  AScaleFactor: TdxScaleFactor;
begin
  APainter := AViewInfo.LookAndFeelPainter;
  AScaleFactor := AViewInfo.ScaleFactor;
  inherited Update(APainter, AScaleFactor);
  FCaptionBottomOffset := FCaptionBottomOffsetCount *
    AViewInfo.FCachedScaledTextOffset;
  FCaptionTopOffset := FCaptionTopOffsetCount *
    AViewInfo.FCachedScaledTextOffset;
  FCaptionLeftOffset := FCaptionLeftOffsetCount *
    AViewInfo.FCachedScaledTextOffset;
  FMaxCaptionWidth := AScaleFactor.Apply(FDefaultMaxCaptionWidth);
  FLineLength := AScaleFactor.Apply(FDefaultCaptionLineLength);
  FTaskHeight := FCaptionTopOffset + AViewInfo.FCachedBoldFontHeight +
    AViewInfo.FCachedFontHeight + FCaptionBottomOffset;
  FScaledMinTaskWidth := AScaleFactor.Apply
    (TdxGanttControlViewTimelineViewInfo.FMinTaskWidth);
end;

{ TdxGanttControlTimelineTaskViewInfo }

constructor TdxGanttControlTimelineTaskViewInfo.Create
  (AOwner: TdxGanttControlCustomItemViewInfo; ATask: TdxGanttControlTask);
begin
  inherited Create(AOwner, ATask);
  FCaptionLayout := Canvas.CreateTextLayout;
  FInformationLayout := Canvas.CreateTextLayout;
end;

destructor TdxGanttControlTimelineTaskViewInfo.Destroy;
begin
  FreeAndNil(FCaptionLayout);
  FreeAndNil(FInformationLayout);
  inherited Destroy;
end;

procedure TdxGanttControlTimelineTaskViewInfo.Initialize;
begin
  inherited Initialize;
  FInformation := GetInformation;
end;

procedure TdxGanttControlTimelineTaskViewInfo.Calculate(const R: TRect);
begin
  inherited Calculate(R);
  FInformationBounds := CalculateInformationBounds;
  SetTextsLayoutAttributes;
end;

function TdxGanttControlTimelineTaskViewInfo.CalculateCaptionBounds
  (const R: TRect): TRect;
begin
  Result := R;
  Inc(Result.Left, CachedValues.CaptionLeftOffset);
  Inc(Result.Top, CachedValues.CaptionTopOffset);
  Dec(Result.Right, Owner.FCachedScaledTextOffset);
end;

function TdxGanttControlTimelineTaskViewInfo.CalculateInformationBounds: TRect;
begin
  Result := CaptionBounds;
  Inc(Result.Top, Owner.FCachedBoldFontHeight);
end;

function TdxGanttControlTimelineTaskViewInfo.CalculateBarBounds
  (const R: TRect): TRect;
begin
  if not(Task.IsValueAssigned(TdxGanttTaskAssignedValue.Start) and
    Task.IsValueAssigned(TdxGanttTaskAssignedValue.Finish)) then
    Exit(TRect.Null);
  Result := R;
end;

function TdxGanttControlTimelineTaskViewInfo.CalculateBarProgressBounds
  (const R: TRect): TRect;
var
  AValue: Integer;
begin
  if not Task.IsValueAssigned(TdxGanttTaskAssignedValue.PercentComplete) then
    AValue := 0
  else
    AValue := Task.PercentComplete;
  Result := R;
  Result.Right := R.Left + Round(AValue / 100 * cxRectWidth(R));
end;

procedure TdxGanttControlTimelineTaskViewInfo.SetTextsLayoutAttributes;
begin
  CaptionLayout.SetFlags(GetCaptionFlags);
  CaptionLayout.SetFont(GetCaptionFont);
  CaptionLayout.SetColor(LookAndFeelPainter.DefaultContentTextColor);
  CaptionLayout.SetText(Caption);

  InformationLayout.SetFlags(GetInformationFlags);
  InformationLayout.SetFont(GetFont);
  InformationLayout.SetColor(LookAndFeelPainter.DefaultContentTextColor);
  InformationLayout.SetText(Information);
end;

procedure TdxGanttControlTimelineTaskViewInfo.DoDraw;
begin
  DoDrawBar(Canvas, BarBounds, BarProgressBounds);
  if not CaptionBounds.IsEmpty then
    DoDrawCaption;
  if not FInformationBounds.IsEmpty then
    DoDrawInformation;
  if IsSelected then
    DoDrawSelection;
end;

procedure TdxGanttControlTimelineTaskViewInfo.DoDrawBar
  (ACanvas: TcxCustomCanvas; const ABounds, AProgressBounds: TRect);
begin
  LookAndFeelPainter.DrawGanttTask(ACanvas, ABounds, Color);
  LookAndFeelPainter.DrawGanttTaskProgress(ACanvas, ABounds,
    AProgressBounds, Color);
end;

function TdxGanttControlTimelineTaskViewInfo.GetActualTextBounds
  (const ABounds: TRect): TRect;
begin
  Result := ABounds;
  if not UseRightToLeftAlignment then
    Result.Left := Max(Result.Left, Owner.ClientRect.Left +
      CachedValues.CaptionLeftOffset)
  else
    Result.Right := Min(Result.Right, Owner.ClientRect.Right -
      CachedValues.CaptionLeftOffset);
end;

function TdxGanttControlTimelineTaskViewInfo.GetBoldFont: TcxCanvasBasedFont;
begin
  Result := Owner.FCachedBoldFont;
end;

function TdxGanttControlTimelineTaskViewInfo.GetFont: TcxCanvasBasedFont;
begin
  Result := Owner.FCachedFont;
end;

function TdxGanttControlTimelineTaskViewInfo.InternalGetCachedValues
  : TdxGanttControlTimelineTaskViewInfoCachedValues;
begin
  Result := TdxGanttControlTimelineTaskViewInfoCachedValues
    (inherited CachedValues);
end;

function TdxGanttControlTimelineTaskViewInfo.GetCaptionFlags: Integer;
const
  AAlignments: array [Boolean] of Integer = (CXTO_LEFT, CXTO_RIGHT);
begin
  Result := AAlignments[UseRightToLeftAlignment];
  if UseRightToLeftAlignment then
    Result := Result or CXTO_RTLREADING;
end;

function TdxGanttControlTimelineTaskViewInfo.GetCaptionFont: TcxCanvasBasedFont;
begin
  Result := GetBoldFont;
end;

function TdxGanttControlTimelineTaskViewInfo.GetClipBounds: TRect;
begin
  Result := inherited GetClipBounds;
  if IsSelected then
    Result := cxRectInflate(Result, 1, 1);
end;

function TdxGanttControlTimelineTaskViewInfo.GetInformationFlags: Integer;
begin
  Result := GetCaptionFlags;
end;

procedure TdxGanttControlTimelineTaskViewInfo.DoDrawCaption;
begin
  FCaptionLayout.Draw(GetActualTextBounds(CaptionBounds));
end;

procedure TdxGanttControlTimelineTaskViewInfo.DoDrawInformation;
begin
  FInformationLayout.Draw(GetActualTextBounds(InformationBounds));
end;

procedure TdxGanttControlTimelineTaskViewInfo.DoDrawSelection;
begin
  Canvas.FrameRect(cxRectInflate(Bounds, 1, 1),
    LookAndFeelPainter.DefaultContentTextColor, 2, cxBordersAll);
end;

procedure TdxGanttControlTimelineTaskViewInfo.DoRightToLeftConversion
  (const AClientBounds: TRect);
begin
  inherited DoRightToLeftConversion(AClientBounds);
  FInformationBounds := TdxRightToLeftLayoutConverter.ConvertRect
    (FInformationBounds, AClientBounds);
end;

procedure TdxGanttControlTimelineTaskViewInfo.DoScroll(const DX, DY: Integer);
begin
  inherited DoScroll(DX, DY);
  FInformationBounds := cxRectOffset(FInformationBounds, DX, DY);
end;

function TdxGanttControlTimelineTaskViewInfo.IsSelected: Boolean;
begin
  Result := Owner.Controller.IsSelected(Self.Task);
end;

function TdxGanttControlTimelineTaskViewInfo.GetOwner
  : TdxGanttControlViewTimelineViewInfo;
begin
  Result := TdxGanttControlViewTimelineViewInfo(inherited Owner);
end;

function TdxGanttControlTimelineTaskViewInfo.GetCachedValues
  : TdxGanttControlTaskViewInfoCachedValues;
begin
  Result := Owner.FTaskCachedValues;
end;

function TdxGanttControlTimelineTaskViewInfo.GetCaption: string;
begin
  Result := Task.Name;
end;

function TdxGanttControlTimelineTaskViewInfo.GetInformation: string;
begin
  Result := Format('%s - %s', [dxGetDateTimeAsString(Task.Start),
    dxGetDateTimeAsString(Task.Finish)]);
end;

{ TdxGanttControlTimelineMilestoneTaskViewInfo }

constructor TdxGanttControlTimelineMilestoneTaskViewInfo.Create
  (AOwner: TdxGanttControlCustomItemViewInfo; ATask: TdxGanttControlTask);
begin
  inherited Create(AOwner, ATask);
  FIsBarVisible := True;
  FIsLabelVisible := True;
end;

procedure TdxGanttControlTimelineMilestoneTaskViewInfo.Calculate
  (const R: TRect);
var
  ABounds: TRect;
begin
  inherited Calculate(R);
  ABounds := InformationBounds;
  ABounds.Top := BarBounds.Top;
  Inc(ABounds.Bottom, Owner.FCachedScaledTextOffset);
  SetBounds(cxRectInflate(ABounds, Owner.FCachedScaledTextOffset));
end;

function TdxGanttControlTimelineMilestoneTaskViewInfo.CalculateBarBounds
  (const R: TRect): TRect;
var
  AWidth, AHeight: Integer;
begin
  Result := inherited CalculateBarBounds(R);
  Result.Size := GetMilestoneSize;
  AWidth := Result.Width;
  AHeight := Result.Height;
  Result.Offset(-AWidth div 2 - Integer(not Odd(AWidth)),
    -AHeight div 2 - Integer(not Odd(AHeight)));
end;

function TdxGanttControlTimelineMilestoneTaskViewInfo.CalculateBarProgressBounds
  (const R: TRect): TRect;
begin
  Result := cxNullRect;
end;

function TdxGanttControlTimelineMilestoneTaskViewInfo.CalculateCaptionBounds
  (const R: TRect): TRect;
var
  AWidth, AMin, AMax: Integer;
begin
  Result := R;
  Result.Top := BarBounds.Bottom + 2 * Owner.FCachedScaledTextOffset +
    CachedValues.LineLength;
  Result.Bottom := Result.Top + Owner.FCachedFontHeight;
  if Length(Task.Name) > Length(Information) then
    AWidth := TdxGanttControlUtils.MeasureTextWidth(CaptionLayout,
      Task.Name, GetFont)
  else
    AWidth := TdxGanttControlUtils.MeasureTextWidth(CaptionLayout,
      Information, GetFont);
  if AWidth > CachedValues.MaxCaptionWidth then
  begin
    AWidth := CachedValues.MaxCaptionWidth;
    FNeedEndEllipsis := True;
  end;
  Dec(Result.Left, AWidth div 2);
  Result.Right := Result.Left + AWidth;
  AMax := Owner.ProjectFinishLabel.Bounds.Right;
  if Result.Right > AMax then
    Result := cxRectOffsetHorz(Result, AMax - Result.Right);
  AMin := Owner.ProjectStartLabel.Bounds.Left + Owner.GetStartLabelOffsetLeft;
  if Result.Left < AMin then
  begin
    Result := cxRectOffsetHorz(Result, AMin - Result.Left);
    FNeedEndEllipsis := FNeedEndEllipsis or (Result.Right > AMax);
    Result.Right := Min(Result.Right, AMax);
  end;
end;

function TdxGanttControlTimelineMilestoneTaskViewInfo.CalculateHitTest
  (const AHitTest: TdxGanttControlHitTest): Boolean;
begin
  Result := IsBarVisible and PtInRect(BarBounds, AHitTest.HitPoint);
  if not Result and IsLabelVisible then
    Result := PtInRect(GetLabelRect, AHitTest.HitPoint);
  if Result then
    TdxGanttControlHitTestAccess(AHitTest).SetHitObject(Self);
end;

function TdxGanttControlTimelineMilestoneTaskViewInfo.
  CalculateInformationBounds: TRect;
begin
  Result := CaptionBounds;
  Result.Top := Result.Bottom + Owner.FCachedScaledTextOffset;
  Inc(Result.Bottom, Owner.FCachedFontHeight);
end;

procedure TdxGanttControlTimelineMilestoneTaskViewInfo.SetTextsLayoutAttributes;
begin
  inherited SetTextsLayoutAttributes;
  CaptionLayout.SetLayoutConstraints(CaptionBounds);
end;

procedure TdxGanttControlTimelineMilestoneTaskViewInfo.DoDrawBar
  (ACanvas: TcxCustomCanvas; const ABounds, AProgressBounds: TRect);
begin
  if IsBarVisible then
    LookAndFeelPainter.DrawGanttMilestone(ACanvas, ABounds, ScaleFactor, Color);
end;

procedure TdxGanttControlTimelineMilestoneTaskViewInfo.DoDrawCaption;
var
  R: TRect;
begin
  if not IsLabelVisible then
    Exit;

  R.Left := cxRectCenter(BarBounds).X;
  R.Right := R.Left + 1;
  R.Top := BarBounds.Bottom + 2 * Owner.FCachedScaledTextOffset;
  R.Bottom := CaptionBounds.Top;
  Canvas.FillRect(R, LookAndFeelPainter.DefaultContentTextColor);
  CaptionLayout.Draw(CaptionBounds);
end;

procedure TdxGanttControlTimelineMilestoneTaskViewInfo.DoDrawInformation;
begin
  if IsLabelVisible then
    inherited DoDrawInformation;
end;

procedure TdxGanttControlTimelineMilestoneTaskViewInfo.DoDrawSelection;
var
  ABounds: TRect;
begin
  if not IsLabelVisible then
    Exit;

  ABounds := Bounds;
  ABounds.Top := CaptionBounds.Top - Owner.FCachedScaledTextOffset;
  Canvas.FrameRect(ABounds, LookAndFeelPainter.DefaultContentTextColor, 2,
    cxBordersAll);
end;

function TdxGanttControlTimelineMilestoneTaskViewInfo.GetActualTextBounds
  (const ABounds: TRect): TRect;
begin
  Result := ABounds;
end;

function TdxGanttControlTimelineMilestoneTaskViewInfo.GetDefaultColor: TColor;
begin
  Result := CachedValues.MilestoneColor;
end;

function TdxGanttControlTimelineMilestoneTaskViewInfo.GetCaptionFlags: Integer;
const
  AAlignment: array [Boolean] of Integer = (CXTO_LEFT, CXTO_RIGHT);
begin
  if not FNeedEndEllipsis then
    Result := CXTO_CENTER_HORIZONTALLY
  else
    Result := AAlignment[UseRightToLeftAlignment] or CXTO_END_ELLIPSIS;
  if UseRightToLeftAlignment then
    Result := Result or CXTO_RTLREADING;
end;

function TdxGanttControlTimelineMilestoneTaskViewInfo.GetCaptionFont
  : TcxCanvasBasedFont;
begin
  Result := GetFont;
end;

function TdxGanttControlTimelineMilestoneTaskViewInfo.
  GetInformationFlags: Integer;
begin
  Result := CXTO_CENTER_HORIZONTALLY;
end;

function TdxGanttControlTimelineMilestoneTaskViewInfo.GetInformation: string;
begin
  Result := Format('%s', [dxGetDateTimeAsString(Task.Start)]);
end;

function TdxGanttControlTimelineMilestoneTaskViewInfo.GetLabelRect: TRect;
begin
  Result := CaptionBounds;
  Result.Left := Min(Result.Left, InformationBounds.Left);
  Result.Right := Max(Result.Right, InformationBounds.Right);
  Result.Bottom := InformationBounds.Bottom;
end;

function TdxGanttControlTimelineMilestoneTaskViewInfo.GetTaskHeight: Integer;
begin
  Result := CachedValues.MilestoneSize.cy + 2 * Owner.FCachedScaledTextOffset +
    CachedValues.LineLength + CachedValues.TaskHeight;
end;

function TdxGanttControlTimelineMilestoneTaskViewInfo.GetVisible: Boolean;
begin
  Result := FIsBarVisible or FIsLabelVisible;
end;

{ TdxGanttControlTimelineScaleCell }

constructor TdxGanttControlTimelineScaleCell.Create
  (AOwner: TdxGanttControlCustomItemViewInfo);
begin
  inherited Create(AOwner);
  FCaptionLayout := Canvas.CreateTextLayout;
end;

destructor TdxGanttControlTimelineScaleCell.Destroy;
begin
  FreeAndNil(FCaptionLayout);
  inherited Destroy;
end;

procedure TdxGanttControlTimelineScaleCell.DoDraw;
var
  R: TRect;
begin
  R := Bounds;
  if not UseRightToLeftAlignment then
    R.Right := R.Left + 1
  else
    R.Left := R.Right - 1;
  Inc(R.Top, Owner.FCachedScaledTextOffset);
  Canvas.FillRect(R, LookAndFeelPainter.DefaultContentTextColor);

  R := Bounds;
  Inc(R.Top, Owner.FCachedTimescaleCaptionOffsetTop);
  if not UseRightToLeftAlignment then
    Inc(R.Left, Owner.FCachedTimescaleCaptionOffsetLeft)
  else
    Dec(R.Right, Owner.FCachedTimescaleCaptionOffsetLeft);
  FCaptionLayout.Draw(R);
end;

function TdxGanttControlTimelineScaleCell.GetOwner
  : TdxGanttControlViewTimelineViewInfo;
begin
  Result := TdxGanttControlViewTimelineViewInfo(inherited Owner);
end;

function TdxGanttControlTimelineScaleCell.GetHintText: string;
begin
  Result := FormatDateTime(TdxCultureInfo.CurrentCulture.FormatSettings.
    ShortDateFormat, FDateTime);
end;

function TdxGanttControlTimelineScaleCell.HasHint: Boolean;
begin
  Result := Owner.ActualTimescaleUnit =
    TdxGanttControlTimelineViewTimescaleUnit.Hours;
end;

procedure TdxGanttControlTimelineScaleCell.Initialize(ADateTime: TDateTime);
const
  AAlignment: array [Boolean] of Integer = (CXTO_LEFT, CXTO_RIGHT or
    CXTO_RTLREADING);
begin
  FDateTime := ADateTime;
  FCaption := FormatDateTime(Owner.FCachedTimescaleCaptionFormat, FDateTime);
  CaptionLayout.SetFlags(AAlignment[UseRightToLeftAlignment]);
  CaptionLayout.SetFont(Owner.FCachedFont);
  CaptionLayout.SetColor(LookAndFeelPainter.DefaultContentTextColor);
  CaptionLayout.SetText(FCaption);
end;

procedure TdxGanttControlTimelineScaleCell.ResetCaption;
begin
  FCaption := '';
  CaptionLayout.SetText(FCaption);
end;

{ TdxGanttControlTimelineProjectCustomLabel }

constructor TdxGanttControlTimelineProjectCustomLabel.Create
  (AOwner: TdxGanttControlCustomItemViewInfo);
begin
  inherited Create(AOwner);
  FCaptionLayout := Canvas.CreateTextLayout;
  FDateTimeLayout := Canvas.CreateTextLayout;
end;

destructor TdxGanttControlTimelineProjectCustomLabel.Destroy;
begin
  FreeAndNil(FCaptionLayout);
  FreeAndNil(FDateTimeLayout);
  inherited Destroy;
end;

procedure TdxGanttControlTimelineProjectCustomLabel.DoDraw;
var
  R: TRect;
begin
  R := Bounds;
  if not FIsVertical then
  begin
    FCaptionLayout.Draw(R);
    Inc(R.Top, GetFontHeight + GetIntervalBetweenLines);
    FDateTimeLayout.Draw(R);
  end
  else
  begin
    if GetHorizontalAlignment = CXTO_LEFT then
    begin
      FCaptionLayout.Draw(R);
      Inc(R.Left, GetFontHeight + GetIntervalBetweenLines);
      FDateTimeLayout.Draw(R);
    end
    else
    begin
      FDateTimeLayout.Draw(R);
      Dec(R.Right, GetFontHeight + GetIntervalBetweenLines);
      FCaptionLayout.Draw(R);
    end
  end;
end;

function TdxGanttControlTimelineProjectCustomLabel.GetOwner
  : TdxGanttControlViewTimelineViewInfo;
begin
  Result := TdxGanttControlViewTimelineViewInfo(inherited Owner);
end;

function TdxGanttControlTimelineProjectCustomLabel.GetFlags: Integer;
begin
  if FIsVertical then
    Result := CXTO_TOP or GetHorizontalAlignment
  else
    Result := GetHorizontalAlignment;
  if UseRightToLeftAlignment then
    Result := Result or CXTO_RTLREADING;
end;

function TdxGanttControlTimelineProjectCustomLabel.GetFont: TcxCanvasBasedFont;
begin
  Result := Owner.FCachedFont;
end;

function TdxGanttControlTimelineProjectCustomLabel.GetFontHeight: Integer;
begin
  Result := Owner.FCachedFontHeight;
end;

function TdxGanttControlTimelineProjectCustomLabel.
  GetIntervalBetweenLines: Integer;
begin
  Result := Owner.FCachedScaledTextOffset;
end;

function TdxGanttControlTimelineProjectCustomLabel.HasHint: Boolean;
begin
  Result := False;
end;

procedure TdxGanttControlTimelineProjectCustomLabel.Initialize
  (ADateTime: TDateTime);
begin
  FDateTime := dxGetDateTimeAsString(ADateTime);

  CaptionLayout.SetFont(GetFont);
  CaptionLayout.SetColor(LookAndFeelPainter.DefaultContentTextColor);
  CaptionLayout.SetText(Caption);

  DateTimeLayout.SetFont(GetFont);
  DateTimeLayout.SetColor(LookAndFeelPainter.DefaultContentTextColor);
  DateTimeLayout.SetText(DateTime);

  UpdateTextLayoutFlags;
end;

procedure TdxGanttControlTimelineProjectCustomLabel.UpdateTextLayoutFlags;
const
  RotationAngleMap: array [Boolean] of TcxRotationAngle = (ra0, raMinus90);
begin
  CaptionLayout.SetFlags(GetFlags);
  CaptionLayout.SetRotation(RotationAngleMap[FIsVertical]);

  DateTimeLayout.SetFlags(GetFlags);
  DateTimeLayout.SetRotation(RotationAngleMap[FIsVertical]);
end;

{ TdxGanttControlTimelineStartLabel }

function TdxGanttControlTimelineStartLabel.GetCaption: string;
begin
  Result := cxGetResourceString(@sdxGanttControlViewTimelineStartText);
end;

function TdxGanttControlTimelineStartLabel.GetHorizontalAlignment: Integer;
const
  AlignmentMap: array [Boolean] of Integer = (CXTO_RIGHT, CXTO_LEFT);
begin
  Result := AlignmentMap[UseRightToLeftAlignment];
end;

{ TdxGanttControlTimelineFinishLabel }

function TdxGanttControlTimelineFinishLabel.GetCaption: string;
begin
  Result := cxGetResourceString(@sdxGanttControlViewTimelineFinishText);
end;

function TdxGanttControlTimelineFinishLabel.GetHorizontalAlignment: Integer;
const
  AlignmentMap: array [Boolean] of Integer = (CXTO_LEFT, CXTO_RIGHT);
begin
  Result := AlignmentMap[UseRightToLeftAlignment]
end;

{ TdxGanttTimelineScrollBars }

procedure TdxGanttTimelineScrollBars.DoHScroll(ScrollCode: TScrollCode;
  var ScrollPos: Integer);
var
  APosition: Integer;
begin
  if ScrollCode = TScrollCode.scEndScroll then
    Exit;
  APosition := Controller.ViewInfo.LeftScrollPos;
  case ScrollCode of
    TScrollCode.scTrack:
      APosition := ScrollPos;
    TScrollCode.scLineUp:
      Dec(APosition, 20);
    TScrollCode.scLineDown:
      Inc(APosition, 20);
    TScrollCode.scPageUp:
      Dec(APosition, HScrollBar.PageSize);
    TScrollCode.scPageDown:
      Inc(APosition, HScrollBar.PageSize);
  end;
  APosition := Max(0, APosition);
  APosition := Min(HScrollBar.Max - HScrollBar.PageSize + 1, APosition);
  Controller.ViewInfo.LeftScrollPos := APosition;
  ScrollPos := Controller.ViewInfo.LeftScrollPos;
end;

function TdxGanttTimelineScrollBars.GetController
  : TdxGanttControlTimelineViewController;
begin
  Result := TdxGanttControlTimelineViewController(inherited Controller);
end;

procedure TdxGanttTimelineScrollBars.DoInitHScrollBarParameters;
var
  AMin, AMax, APageSize, APosition: Integer;
  AVisible: Boolean;
begin
  AVisible := (Controller.ViewInfo <> nil) and
    (Controller.ViewInfo.ContentWidth >
    Controller.ViewInfo.HorzScrollPageWidth);

  if AVisible then
  begin
    AMin := 0;
    AMax := Controller.ViewInfo.ContentWidth - 1;
    APageSize := Max(1, Controller.ViewInfo.HorzScrollPageWidth);
    APosition := Min(AMax - APageSize + 1, Controller.ViewInfo.LeftScrollPos);
    SetScrollInfo(sbHorizontal, AMin, AMax, 1, APageSize, APosition,
      True, True);
  end
  else
    Controller.ViewInfo.ResetLeftScrollPos;

  HScrollBar.Data.Visible := AVisible;
end;

procedure TdxGanttTimelineScrollBars.DoInitVScrollBarParameters;
var
  AMin, AMax, APageSize, APosition: Integer;
  AVisible: Boolean;
begin
  AVisible := (Controller.ViewInfo <> nil) and
    (Controller.ViewInfo.ContentHeight >
    Controller.ViewInfo.VertScrollPageHeight);

  if AVisible then
  begin
    AMin := 0;
    AMax := Controller.ViewInfo.ContentHeight - 1;
    APageSize := Max(1, Controller.ViewInfo.VertScrollPageHeight);
    APosition := Min(AMax - APageSize + 1, Controller.ViewInfo.TopScrollPos);
    SetScrollInfo(sbVertical, AMin, AMax, 1, APageSize, APosition, True, True);
  end;
  VScrollBar.Data.Visible := AVisible;
end;

procedure TdxGanttTimelineScrollBars.DoVScroll(ScrollCode: TScrollCode;
  var ScrollPos: Integer);
var
  APosition: Integer;
begin
  APosition := Controller.ViewInfo.TopScrollPos;
  case ScrollCode of
    scTrack:
      APosition := ScrollPos;
    scPageUp:
      Dec(APosition, Controller.ViewInfo.VertScrollPageHeight);
    scPageDown:
      Inc(APosition, Controller.ViewInfo.VertScrollPageHeight);
    scLineUp, scLineDown:
      begin
        if ScrollCode = scLineUp then
          Dec(APosition, 20)
        else
          Inc(APosition, 20);
      end;
  end;
  APosition := Max(0, APosition);
  APosition := Min(VScrollBar.Max - VScrollBar.PageSize + 1, APosition);
  Controller.ViewInfo.TopScrollPos := APosition;
  ScrollPos := Controller.ViewInfo.TopScrollPos;
end;

{ TdxGanttControlTimelineViewController }

constructor TdxGanttControlTimelineViewController.Create
  (AView: TdxGanttControlCustomView);
begin
  inherited Create(AView);
  FSelection := TdxFastObjectList.Create(False);
  FScrollBars := CreateScrollBars;
end;

destructor TdxGanttControlTimelineViewController.Destroy;
begin
  FreeAndNil(FSelection);
  FreeAndNil(FScrollBars);
  inherited Destroy;
end;

procedure TdxGanttControlTimelineViewController.DoDblClick;
begin
  inherited DoDblClick;
  if HitTest.HitObject is TdxGanttControlTimelineTaskViewInfo then
    ShowTaskInformationDialog(TdxGanttControlTimelineTaskViewInfo
      (HitTest.HitObject).Task);
end;

procedure TdxGanttControlTimelineViewController.DoKeyDown(var Key: Word;
  Shift: TShiftState);
var
  ATask: TdxGanttControlTask;
begin
  inherited DoKeyDown(Key, Shift);
  if (ViewInfo.TaskLayoutBuilder = nil) or
    ((TaskPlaces.Count = 0) and (Milestones.Count = 0)) then
    Exit;
  if ViewInfo.UseRightToLeftAlignment then
    Key := TdxRightToLeftLayoutConverter.ConvertVirtualKeyCode(Key);
  ATask := nil;
  if (FocusedTask = nil) and (Key in [VK_HOME, VK_LEFT, VK_UP, VK_RIGHT,
    VK_DOWN, VK_END]) then
    if TaskPlaces.Count > 0 then
      ATask := TdxGanttControlTimelineTaskPlace(TaskPlaces[0]).Task
    else
      ATask := Milestones[0].Task
  else
  begin
    case Key of
      VK_RETURN:
        if Selection.Count = 1 then
          ShowTaskInformationDialog(TdxGanttControlTask(Selection[0]));
      VK_HOME:
        if [ssCtrl] * Shift <> [] then
          ATask := GetFirstTask
        else
          ATask := GetFirstTaskInRow;
      VK_LEFT:
        ATask := GetPreviousTask;
      VK_UP:
        ATask := GetPreviousTaskInColumn;
      VK_RIGHT:
        ATask := GetNextTask;
      VK_DOWN:
        ATask := GetNextTaskInColumn;
      VK_END:
        if [ssCtrl] * Shift <> [] then
          ATask := GetLastTask
        else
          ATask := GetLastTaskInRow;
      VK_DELETE:
        if (Shift = [ssCtrl]) and (Selection.Count > 0) then
        begin
        end;
    end;
  end;
  if ATask <> nil then
  begin
    if (ssShift in Shift) and IsSelected(FocusedTask) and IsSelected(ATask) then
    begin
      Selection.Remove(FocusedTask);
      FocusedTask := ATask;
      ViewInfo.Invalidate;
    end
    else
    begin
      if ([ssShift] * Shift <> []) or ([ssCtrl] * Shift <> []) then
      begin
        if [ssShift, ssCtrl] * Shift <> [ssShift, ssCtrl] then
          if [ssShift] * Shift <> [] then
            Shift := Shift + [ssCtrl] - [ssShift]
          else
            Shift := Shift + [ssShift] - [ssCtrl];
      end;
      DoSelect(ATask, Shift);
    end;
  end;
end;

procedure TdxGanttControlTimelineViewController.DoMouseDown
  (Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer);
begin
  inherited DoMouseDown(Button, Shift, X, Y);
  if Button = mbMiddle then
    ScrollBars.ProcessControlScrollingOnMiddleButton;
  if HitAtTask then
    DoSelect(HitTask, Shift)
  else
    ClearSelection;
end;

procedure TdxGanttControlTimelineViewController.DoMouseMove(Shift: TShiftState;
  X: Integer; Y: Integer);
begin
  CheckHint(Shift);
end;

function TdxGanttControlTimelineViewController.DoMouseWheel(Shift: TShiftState;
  AIsIncrement: Boolean; const AMousePos: TPoint): Boolean;
var
  ADateTime, ANewDateTime: TDateTime;
  ATimescaleUnit: TdxGanttControlTimelineViewTimescaleUnit;
  ATopScrollPos: Integer;
begin
  if (Shift = [ssCtrl]) and not ViewInfo.IsEmpty then
  begin
    ADateTime := ViewInfo.GetDateTimeByPos(AMousePos.X);
    ATimescaleUnit := ViewInfo.ActualTimescaleUnit;
    if not AIsIncrement then
    begin
      if ATimescaleUnit > TdxGanttControlTimelineViewTimescaleUnit.Hours then
        ATimescaleUnit := TdxGanttControlTimelineViewTimescaleUnit
          (Integer(ATimescaleUnit) - 1);
    end
    else if ATimescaleUnit < High(TdxGanttControlTimelineViewTimescaleUnit) then
      ATimescaleUnit := TdxGanttControlTimelineViewTimescaleUnit
        (Integer(ATimescaleUnit) + 1);
    Result := ATimescaleUnit <> ViewInfo.ActualTimescaleUnit;
    if Result then
    begin
      ATopScrollPos := ViewInfo.TopScrollPos;
      ViewInfo.View.TimescaleUnit := ATimescaleUnit;
      if ADateTime > ViewInfo.GetProjectStart then
      begin
        ANewDateTime := ViewInfo.GetDateTimeByPos(AMousePos.X);
        if ViewInfo.ContentWidth > ViewInfo.HorzScrollPageWidth then
        begin
          ViewInfo.LeftScrollPos := Min(ViewInfo.ContentWidth,
            Trunc((ADateTime - ANewDateTime) * cxRectWidth(ViewInfo.TaskArea) /
            (ViewInfo.GetProjectFinish - ViewInfo.GetProjectStart)));
          if ViewInfo.LeftScrollPos <> 0 then
            ScrollBars.HScrollBar.SetPosition(ViewInfo.LeftScrollPos);
        end;
        ViewInfo.TopScrollPos := ATopScrollPos;
      end;
      Control.Invalidate;
    end;
  end
  else
    Result := ScrollBars.DoMouseWheel(Shift, AIsIncrement);
end;

function TdxGanttControlTimelineViewController.GetHitTask: TdxGanttControlTask;
begin
  if HitAtTask then
    Result := TdxGanttControlTimelineTaskViewInfo(HitTest.HitObject).Task
  else
    Result := nil;
end;

function TdxGanttControlTimelineViewController.GetFirstTask
  : TdxGanttControlTask;
begin
  Result := FocusedTask;
end;

function TdxGanttControlTimelineViewController.GetFirstTaskInRow
  : TdxGanttControlTask;
begin
  Result := FocusedTask;
end;

function TdxGanttControlTimelineViewController.GetPreviousTask
  : TdxGanttControlTask;
var
  AIndex: Integer;
  ATaskLayoutBuilder: TdxGanttControlTimelineTaskLayoutBuilder;
begin
  ATaskLayoutBuilder := TdxGanttControlTimelineTaskLayoutBuilder
    (ViewInfo.TaskLayoutBuilder);
  if FocusedTask = nil then
    if TaskPlaces.Count > 0 then
      Result := TdxGanttControlTimelineTaskPlace
        (TaskPlaces[TaskPlaces.Count - 1]).Task
    else if Milestones.Count > 0 then
      Result := Milestones[Milestones.Count - 1].Task
    else
      Result := nil
  else if not FocusedTask.Milestone then
  begin
    AIndex := TaskPlaces.IndexOf(ATaskLayoutBuilder.GetPlace(FocusedTask));
    if AIndex = 0 then
      if Milestones.Count > 0 then
      begin
        Result := GetRegularMilestone(0, True);
        if Result = nil then
          Result := TdxGanttControlTimelineTaskPlace
            (TaskPlaces[TaskPlaces.Count - 1]).Task;
      end
      else
        Result := TdxGanttControlTimelineTaskPlace
          (TaskPlaces[TaskPlaces.Count - 1]).Task
    else
      Result := TdxGanttControlTimelineTaskPlace(TaskPlaces[AIndex - 1]).Task;
  end
  else
  begin
    AIndex := Milestones.IndexOf(ViewInfo.GetMilestoneInfo(FocusedTask));
    if AIndex = 0 then
      if TaskPlaces.Count > 0 then
        Result := TdxGanttControlTimelineTaskPlace(TaskPlaces[0]).Task
      else
      begin
        Result := GetRegularMilestone(Milestones.Count - 1, False);
        if Result = nil then
          Result := TdxGanttControlTimelineTaskPlace(TaskPlaces[0]).Task;
      end
    else
    begin
      Result := GetRegularMilestone(AIndex - 1, False);
      if Result = nil then
        Result := TdxGanttControlTimelineTaskPlace(TaskPlaces[0]).Task;
    end
  end
end;

function TdxGanttControlTimelineViewController.GetPreviousTaskInColumn
  : TdxGanttControlTask;
begin
  Result := FocusedTask;
end;

function TdxGanttControlTimelineViewController.GetNextTask: TdxGanttControlTask;
var
  AIndex: Integer;
  ATaskLayoutBuilder: TdxGanttControlTimelineTaskLayoutBuilder;
begin
  ATaskLayoutBuilder := TdxGanttControlTimelineTaskLayoutBuilder
    (ViewInfo.TaskLayoutBuilder);
  if FocusedTask = nil then
    if TaskPlaces.Count > 0 then
      Result := TdxGanttControlTimelineTaskPlace(TaskPlaces[0]).Task
    else if Milestones.Count > 0 then
      Result := Milestones[0].Task
    else
      Result := nil
  else if not FocusedTask.Milestone then
  begin
    AIndex := TaskPlaces.IndexOf(ATaskLayoutBuilder.GetPlace(FocusedTask));
    if AIndex = TaskPlaces.Count - 1 then
      if Milestones.Count > 0 then
      begin
        Result := GetRegularMilestone(Milestones.Count - 1, False);
        if Result = nil then
          Result := TdxGanttControlTimelineTaskPlace(TaskPlaces[0]).Task;
      end
      else
        Result := TdxGanttControlTimelineTaskPlace(TaskPlaces[0]).Task
    else
      Result := TdxGanttControlTimelineTaskPlace(TaskPlaces[AIndex + 1]).Task;
  end
  else
  begin
    AIndex := Milestones.IndexOf(ViewInfo.GetMilestoneInfo(FocusedTask));
    if AIndex = Milestones.Count - 1 then
      if TaskPlaces.Count > 0 then
        Result := TdxGanttControlTimelineTaskPlace
          (TaskPlaces[TaskPlaces.Count - 1]).Task
      else
      begin
        Result := GetRegularMilestone(0, True);
        if Result = nil then
          Result := TdxGanttControlTimelineTaskPlace
            (TaskPlaces[TaskPlaces.Count - 1]).Task;
      end
    else
    begin
      Result := GetRegularMilestone(AIndex + 1, True);
      if Result = nil then
        Result := TdxGanttControlTimelineTaskPlace
          (TaskPlaces[TaskPlaces.Count - 1]).Task;
    end;
  end
end;

function TdxGanttControlTimelineViewController.GetNextTaskInColumn
  : TdxGanttControlTask;
begin
  Result := FocusedTask;
end;

function TdxGanttControlTimelineViewController.GetLastTask: TdxGanttControlTask;
begin
  Result := FocusedTask;
end;

function TdxGanttControlTimelineViewController.GetLastTaskInRow
  : TdxGanttControlTask;
begin
  Result := FocusedTask;
end;

function TdxGanttControlTimelineViewController.GetRegularMilestone
  (ABeginIndex: Integer; AForward: Boolean): TdxGanttControlTask;
const
  AIncrement: array [Boolean] of Integer = (-1, 1);
var
  AIndex, AEndIndex: Integer;
  AMilestone: TdxGanttControlTimelineMilestoneTaskViewInfo;
begin
  Result := nil;
  if AForward then
    AEndIndex := Milestones.Count - 1
  else
    AEndIndex := 0;
  AIndex := ABeginIndex;
  while (AForward and (AIndex <= AEndIndex)) or
    (not AForward and (AIndex >= AEndIndex)) do
  begin
    AMilestone := Milestones[AIndex];
    if AMilestone.IsLabelVisible then
      Exit(AMilestone.Task);
    AIndex := AIndex + AIncrement[AForward];
  end;
end;

function TdxGanttControlTimelineViewController.GetMilestones
  : TObjectList<TdxGanttControlTimelineMilestoneTaskViewInfo>;
begin
  Result := ViewInfo.Milestones;
end;

function TdxGanttControlTimelineViewController.GetTaskPlaces: TdxFastObjectList;
begin
  Result := TdxGanttControlTimelineTaskLayoutBuilder(ViewInfo.TaskLayoutBuilder)
    .TaskPlaces;
end;

function TdxGanttControlTimelineViewController.HitAtTask: Boolean;
begin
  Result := HitTest.HitObject is TdxGanttControlTimelineTaskViewInfo;
end;

function TdxGanttControlTimelineViewController.InternalGetViewInfo
  : TdxGanttControlViewTimelineViewInfo;
begin
  Result := TdxGanttControlViewTimelineViewInfo(inherited ViewInfo);
end;

procedure TdxGanttControlTimelineViewController.Activated;
begin
  DoCreateScrollBars;
end;

procedure TdxGanttControlTimelineViewController.CalculateScrollBars;
begin
  Inc(FScrollBarsCalculationCount);
  ScrollBars.CalculateScrollBars;
  Dec(FScrollBarsCalculationCount);
end;

function TdxGanttControlTimelineViewController.CreateScrollBars
  : TdxGanttTimelineScrollBars;
begin
  Result := TdxGanttTimelineScrollBars.Create(Self);
end;

procedure TdxGanttControlTimelineViewController.Deactivated;
begin
  DoDestroyScrollBars;
end;

procedure TdxGanttControlTimelineViewController.DoCreateScrollBars;
begin
  inherited DoCreateScrollBars;
  ScrollBars.DoCreateScrollBars;
end;

procedure TdxGanttControlTimelineViewController.DoDestroyScrollBars;
begin
  inherited DoDestroyScrollBars;
  ScrollBars.DoDestroyScrollBars;
end;

function TdxGanttControlTimelineViewController.GetTouchScrollUIOwner
  (const APoint: TPoint): IdxTouchScrollUIOwner;
begin
  Result := ScrollBars;
end;

function TdxGanttControlTimelineViewController.GetGestureClient
  (const APoint: TPoint): IdxGestureClient;
begin
  Result := FScrollBars;
end;

procedure TdxGanttControlTimelineViewController.InitScrollbars;
begin
  inherited InitScrollbars;
  ScrollBars.InitScrollbars;
end;

function TdxGanttControlTimelineViewController.IsMouseWheelHandleNeeded
  (const MousePos: TPoint): Boolean;
begin
  Result := True;
end;

function TdxGanttControlTimelineViewController.IsPanArea
  (const APoint: TPoint): Boolean;
begin
  Result := PtInRect(ViewInfo.ClientRect, APoint);
end;

function TdxGanttControlTimelineViewController.ProcessNCSizeChanged: Boolean;
begin
  Result := FScrollBars.NCSizeChanged;
end;

procedure TdxGanttControlTimelineViewController.UnInitScrollbars;
begin
  inherited UnInitScrollbars;
  ScrollBars.UnInitScrollbars;
end;

procedure TdxGanttControlTimelineViewController.DoSelect
  (ATask: TdxGanttControlTask; AShift: TShiftState);
var
  I: Integer;
begin
  if ssCtrl in AShift then
  begin
    if IsSelected(ATask) then
      FSelection.Remove(ATask)
    else
      FSelection.Add(ATask)
  end
  else
  begin
    if not IsSelected(ATask) then
    begin
      Selection.Clear;
      FSelection.Add(ATask);
    end
    else
    begin
      for I := Selection.Count - 1 downto 0 do
        if TdxGanttControlTask(Selection.Items[I]) <> ATask then
          Selection.Delete(I);
    end;
  end;
  FocusedTask := ATask;
  ViewInfo.Invalidate;
end;

procedure TdxGanttControlTimelineViewController.ClearSelection;
begin
  Selection.Clear;
  ViewInfo.Invalidate;
end;

procedure TdxGanttControlTimelineViewController.CheckFocusedTask;
var
  ABuilder: TdxGanttControlTimelineTaskLayoutBuilder;
begin
  if FocusedTask = nil then
    Exit;
  ABuilder := TdxGanttControlTimelineTaskLayoutBuilder
    (ViewInfo.TaskLayoutBuilder);
  if ABuilder.GetPlace(FocusedTask) = nil then
    FocusedTask := nil;
end;

procedure TdxGanttControlTimelineViewController.CheckSelection;
var
  ABuilder: TdxGanttControlTimelineTaskLayoutBuilder;
  I: Integer;
begin
  if Selection.Count = 0 then
    Exit;
  ABuilder := TdxGanttControlTimelineTaskLayoutBuilder
    (ViewInfo.TaskLayoutBuilder);
  for I := Selection.Count - 1 downto 0 do
    if ABuilder.GetPlace((Selection.Items[I] as TdxGanttControlTask)) = nil then
      Selection.Delete(I);
end;

function TdxGanttControlTimelineViewController.IsSelected
  (ATask: TdxGanttControlTask): Boolean;
begin
  Result := Selection.IndexOf(ATask) >= 0;
end;

procedure TdxGanttControlTimelineViewController.MakeVisible
  (ATask: TdxGanttControlTask);
var
  ATaskPlace: TdxGanttControlTimelineTaskPlace;
  ABounds, AClientRect: TRect;
  DX, DY: Integer;
begin
  if ATask = nil then
    Exit;
  AClientRect := ViewInfo.ClientRect;
  if ATask.Milestone then
    ABounds := ViewInfo.GetMilestoneInfo(ATask).Bounds
  else
  begin
    if ViewInfo.UseRightToLeftAlignment then
      ViewInfo.ConvertContentAreas(AClientRect);
    ATaskPlace := TdxGanttControlTimelineTaskLayoutBuilder
      (ViewInfo.TaskLayoutBuilder).GetPlace(ATask);
    ViewInfo.CalculateTaskBoundsLeftAndRight(ATaskPlace.TimeStart,
      ATaskPlace.TimeFinish, ABounds.Left, ABounds.Right);
    ViewInfo.CalculateTaskBoundsTopAndBottom(ATaskPlace.LineStart,
      ATaskPlace.LineFinish, ABounds.Top, ABounds.Bottom);
    if ViewInfo.UseRightToLeftAlignment then
    begin
      ViewInfo.ConvertContentAreas(AClientRect);
      ABounds := TdxRightToLeftLayoutConverter.ConvertRect(ABounds,
        AClientRect);
    end;
  end;

  DX := 0;
  DY := 0;
  if (ABounds.Right < AClientRect.Left) or (ABounds.Left > AClientRect.Right)
  then
    if not ViewInfo.UseRightToLeftAlignment then
      DX := Min(ABounds.Left - AClientRect.Left, ViewInfo.ContentWidth -
        ViewInfo.HorzScrollPageWidth - ViewInfo.LeftScrollPos)
    else
      DX := Min(AClientRect.Right - ABounds.Right, ViewInfo.ContentWidth -
        ViewInfo.HorzScrollPageWidth - ViewInfo.LeftScrollPos);
  if (ABounds.Bottom < AClientRect.Top) or (ABounds.Top > AClientRect.Bottom)
  then
    DY := Min(ABounds.Top - AClientRect.Top, ViewInfo.ContentHeight -
      ViewInfo.VertScrollPageHeight - ViewInfo.TopScrollPos);
  ViewInfo.DoScroll(DX, DY);
  if DX <> 0 then
    ScrollBars.HScrollBar.SetPosition(ViewInfo.LeftScrollPos);
  if DY <> 0 then
    ScrollBars.VScrollBar.SetPosition(ViewInfo.TopScrollPos);
end;

procedure TdxGanttControlTimelineViewController.ResetSelectionAndFocusedTask;
begin
  FFocusedTask := nil;
  FSelection.Clear;
end;

function TdxGanttControlTimelineViewController.InitializeBuiltInPopupMenu
  (APopupMenu: TdxGanttControlPopupMenu; var P: TPoint): Boolean;
begin
  Result := HitAtTask and IsSelected(HitTask);
  if Result then
  begin
    APopupMenu.AddCommand(TdxGanttControlGoToTaskCommand.Create(Control));
    APopupMenu.AddCommand(TdxGanttControlRemoveTaskFromTimelineCommand.Create
      (Control));
    APopupMenu.AddSeparator;
    APopupMenu.AddCommand
      (TdxGanttControlTimelineOpenTaskInformationDialogCommand.Create(Control));
  end;
end;

procedure TdxGanttControlTimelineViewController.SetFocusedTask
  (ATask: TdxGanttControlTask);
begin
  FFocusedTask := ATask;
  MakeVisible(FFocusedTask);
end;

procedure TdxGanttControlTimelineViewController.ShowTaskInformationDialog
  (ATask: TdxGanttControlTask);
begin
  dxGanttControlTaskInformationDialog.ShowTaskInformationDialog
    (View.Owner, ATask);
end;

{ TdxGanttControlViewTimelineViewInfo }

constructor TdxGanttControlViewTimelineViewInfo.Create
  (AView: TdxGanttControlCustomView);
begin
  inherited Create(AView);
  FTimescaleCells := TObjectList<TdxGanttControlTimelineScaleCell>.Create;
  FActiveTasks := TObjectList<TdxGanttControlTimelineTaskViewInfo>.Create;
  FMilestones :=
    TObjectList<TdxGanttControlTimelineMilestoneTaskViewInfo>.Create;
  FTaskCachedValues := TdxGanttControlTimelineTaskViewInfoCachedValues.Create;
  FTaskLayoutBuilder := nil;
  TdxCustomGanttControl(View.Owner).DataModel.Tasks.AfterResetHandlers.Add
    (TaskListAfterResetHandler);
end;

destructor TdxGanttControlViewTimelineViewInfo.Destroy;
begin
  TdxCustomGanttControl(View.Owner).DataModel.Tasks.AfterResetHandlers.Remove
    (TaskListAfterResetHandler);
  FreeAndNil(FTaskCachedValues);
  FreeAndNil(FTaskLayoutBuilder);
  FreeAndNil(FProjectStartLabel);
  FreeAndNil(FProjectFinishLabel);
  FreeAndNil(FActiveTasks);
  FreeAndNil(FMilestones);
  FreeAndNil(FTimescaleCells);
  inherited Destroy;
end;

procedure TdxGanttControlViewTimelineViewInfo.Calculate(const R: TRect);
var
  ABuilder: TdxGanttControlTimelineTaskLayoutBuilder;
begin
  inherited Calculate(R);
  FCalendar := GetCurrentCalendar;
  FIsEmpty := (DataProvider.DataModel.Tasks.Count = 0) or
    (CompareDateTime(GetProjectStart, GetProjectFinish) = 0);
  if IsEmpty then
    Exit;

  if Controller.ScrollBarsCalculationCount = 0 then
  begin
    Milestones.Clear;
    FTaskLayoutBuilder.Free;
    FTaskLayoutBuilder := TdxGanttControlTimelineTaskLayoutBuilder.Create;
    ABuilder := TdxGanttControlTimelineTaskLayoutBuilder(FTaskLayoutBuilder);
    ABuilder.PopulatePlaces(DataProvider.DataModel.Tasks,
      View.ShowOnlyExplicitlyAddedTasks);
    ABuilder.Calculate;
  end;
  CalculateContent(R);
end;

procedure TdxGanttControlViewTimelineViewInfo.CalculateContent(const R: TRect);
begin
  if View.Owner.IsDesigning then
    Exit;
  PrepareContent;
  CalculateClientRect(R);
  CalculateStartAndFinishInfoSize;
  CalculateTimescaleUnitWidth;
  if CalculateTaskViewInfos then
  begin
    Controller.CheckFocusedTask;
    Controller.CheckSelection;
    CalculateTimescaleCells;
    CheckRightToLeftAlignment;
  end;
  Controller.CalculateScrollBars;
end;

procedure TdxGanttControlViewTimelineViewInfo.CalculateContentAreas
  (ALineCount: Integer; AHasMilestones: Boolean);

  procedure CheckScrollPositions;
  begin
    if (FLeftScrollPos > 0) and
      (GetHorzScrollPageWidth > FPriorHorzScrollPageWidth) then
      FLeftScrollPos :=
        Max(0, FLeftScrollPos - (GetHorzScrollPageWidth -
        FPriorHorzScrollPageWidth));
    if (FTopScrollPos > 0) and
      (GetVertScrollPageHeight > FPriorVertScrollPageHeight) then
      FTopScrollPos :=
        Max(0, FTopScrollPos - (GetVertScrollPageHeight -
        FPriorVertScrollPageHeight));
  end;

  function GetMilestoneAreaHeight: Integer;
  begin
    if AHasMilestones then
      Result := FTaskCachedValues.MilestoneSize.cy div +2 *
        FCachedScaledTextOffset + FTaskCachedValues.LineLength + 2 *
        FCachedFontHeight
    else
      Result := 0;
  end;

var
  ATaskAreaWidth: Integer;
  ATaskAreaHeight: Integer;
  AProjectStartArea, AProjectFinishArea: TRect;
  ATimescaleHeight, AOffset: Integer;
begin
  CheckScrollPositions;

  ATaskAreaWidth := Trunc(FTimeSlotCount * FTimescaleUnitWidth);

  AProjectStartArea.Left := 1 - LeftScrollPos;
  FTaskArea.Left := AProjectStartArea.Left + FProjectStartLabelSize.cx;
  AProjectStartArea.Right := FTaskArea.Left - GetStartLabelOffsetRight;
  FTaskArea.Right := FTaskArea.Left + ATaskAreaWidth;
  AProjectFinishArea.Left := FTaskArea.Right + GetStartLabelOffsetRight;
  AProjectFinishArea.Right := FTaskArea.Right + FProjectFinishLabelSize.cx;

  AOffset := GetStartLabelOffsetLeft;
  ATimescaleHeight := FCachedTimescaleCellHeight;
  ATaskAreaHeight := FTaskCachedValues.TaskHeight * ALineCount;
  FContentHeight := Max(ATaskAreaHeight + GetMilestoneAreaHeight,
    Max(FProjectStartLabelSize.cy, FProjectFinishLabelSize.cy)) +
    ATimescaleHeight + 2 * AOffset;
  if FContentHeight <= ClientRect.Height - 2 * AOffset then
    FTaskArea.Top := cxRectCenter(ClientRect).Y - FContentHeight div 2 +
      ATimescaleHeight + AOffset - TopScrollPos
  else
    FTaskArea.Top := 1 - TopScrollPos + ATimescaleHeight + AOffset;
  FTaskArea.Bottom := FTaskArea.Top + ATaskAreaHeight;

  AProjectStartArea.Top := FTaskArea.Top;
  AProjectStartArea.Bottom := AProjectStartArea.Top + FProjectStartLabelSize.cy;
  ProjectStartLabel.Calculate(AProjectStartArea);
  AProjectFinishArea.Top := AProjectStartArea.Top;
  AProjectFinishArea.Bottom := AProjectFinishArea.Top +
    FProjectFinishLabelSize.cy;
  ProjectFinishLabel.Calculate(AProjectFinishArea);

  FPriorHorzScrollPageWidth := GetHorzScrollPageWidth;
  FPriorVertScrollPageHeight := GetVertScrollPageHeight;
end;

procedure TdxGanttControlViewTimelineViewInfo.CalculateTaskBoundsLeftAndRight
  (const APlaceTimeStart, APlaceTimeFinish: Int64; var ALeft, ARight: Integer);
begin
  ALeft := Max(TaskArea.Left + 1, TaskArea.Left +
    Trunc((APlaceTimeStart - FProjectStartAsMilleseconds) *
    FDisplayScaleFactor));
  ARight := Max(ALeft + FTaskCachedValues.ScaledMinTaskWidth,
    Min(TaskArea.Right - 1, Max(ALeft + 1,
    TaskArea.Left + Trunc((APlaceTimeFinish + 1 - FProjectStartAsMilleseconds) *
    FDisplayScaleFactor)) - 1));
end;

procedure TdxGanttControlViewTimelineViewInfo.CalculateTaskBoundsTopAndBottom
  (const APlaceLineStart, APlaceLineFinish: Int64; var ATop, ABottom: Integer);
begin
  ATop := TaskArea.Top + 1 + APlaceLineStart * FTaskCachedValues.TaskHeight;
  ABottom := Min(TaskArea.Bottom - 1,
    ATop + (APlaceLineFinish - APlaceLineStart + 1) *
    FTaskCachedValues.TaskHeight - 1);
end;

procedure TdxGanttControlViewTimelineViewInfo.CalculateTimescaleCells;
var
  AStart, AFinish, ACurrentTime, ANextTime: TDateTime;
  R: TRect;
  AOccupiedWidth: Integer;
  ACell: TdxGanttControlTimelineScaleCell;
  AIsLastCell: Boolean;
begin
  UpdateSpecialCachedValues(ActualTimescaleUnit);

  AStart := GetProjectStart;
  ACurrentTime := AStart;
  AFinish := GetProjectFinish;
  R := cxRect(TaskArea.Left, TaskArea.Top - FCachedTimescaleCellHeight,
    TaskArea.Left, TaskArea.Top);
  if (ActualTimescaleUnit <> TdxGanttControlTimelineViewTimescaleUnit.Hours) and
    (TimeOf(ACurrentTime) > 0) then
  begin
    ACurrentTime := Trunc(ACurrentTime) + 1;
    Inc(R.Left, Max(1, Trunc(FDisplayScaleFactor *
      (TdxGanttControlUtils.DateTimeToMilliseconds(ACurrentTime) -
      FProjectStartAsMilleseconds))));
    R.Right := R.Left;
  end;
  AOccupiedWidth := R.Right;
  ACell := nil;
  while ACurrentTime <= AFinish do
  begin
    case FActualTimescaleUnit of
      TdxGanttControlTimelineViewTimescaleUnit.Hours:
        ANextTime := IncHour(ACurrentTime);
      TdxGanttControlTimelineViewTimescaleUnit.Days:
        ANextTime := IncDay(ACurrentTime);
      TdxGanttControlTimelineViewTimescaleUnit.Weeks:
        ANextTime := IncWeek(ACurrentTime);
      TdxGanttControlTimelineViewTimescaleUnit.Months:
        ANextTime := IncMonth(ACurrentTime);
      TdxGanttControlTimelineViewTimescaleUnit.Quarters:
        ANextTime := IncMonth(ACurrentTime, 3);
    else
      ANextTime := IncYear(ACurrentTime);
    end;

    if R.Right >= AOccupiedWidth then
    begin
      R.Left := TaskArea.Left +
        Trunc(FDisplayScaleFactor * (TdxGanttControlUtils.DateTimeToMilliseconds
        (ACurrentTime) - FProjectStartAsMilleseconds));
      if R.Left > ClientRect.Right then
        Break;
      if R.Left = TaskArea.Right then
        Dec(R.Left);
      R.Right := TaskArea.Left +
        Trunc(FDisplayScaleFactor * (TdxGanttControlUtils.DateTimeToMilliseconds
        (ANextTime) - FProjectStartAsMilleseconds)) - 1;
      AOccupiedWidth := R.Left + FCachedTimescaleCaptionMaxWidth;
      if (ACell <> nil) and (ACell.Bounds.Right <= ClientRect.Left) then
        FTimescaleCells.Delete(FTimescaleCells.Count - 1);
      ACell := TdxGanttControlTimelineScaleCell.Create(Self);
      FTimescaleCells.Add(ACell);
      ACell.Initialize(ACurrentTime);
    end
    else
      R.Right := TaskArea.Left +
        Trunc(FDisplayScaleFactor * (TdxGanttControlUtils.DateTimeToMilliseconds
        (ANextTime) - FProjectStartAsMilleseconds)) - 1;
    AIsLastCell := R.Right >= TaskArea.Right;
    if AIsLastCell and (AOccupiedWidth > R.Right) then
      R.Right := Min(R.Right + FCachedTimescaleCaptionMaxWidth,
        ProjectFinishLabel.Bounds.Right);
    ACell.Calculate(R);
    if AIsLastCell and (AOccupiedWidth > ProjectFinishLabel.Bounds.Right) then
      ACell.ResetCaption;
    ACurrentTime := ANextTime;
  end;
end;

procedure TdxGanttControlViewTimelineViewInfo.CalculateTimescaleUnitWidth;

  procedure CalculatePeriodParams(AStart, AFinish: TDateTime;
    var AHoursBetween, ADaysBetween, AWeeksBetween, AMonthsBetween,
    AQuarterBetween, AYearsBetween: Real);
  const
    AMillisecondsPerDay = Int64(MSecsPerSec * SecsPerMin * MinsPerHour *
      HoursPerDay);
  var
    ATotalMilliseconds: Int64;
  begin
    ATotalMilliseconds := TdxGanttControlUtils.DateTimeToMilliseconds(AFinish) -
      TdxGanttControlUtils.DateTimeToMilliseconds(AStart);
    AHoursBetween := ATotalMilliseconds /
      (MSecsPerSec * SecsPerMin * MinsPerHour);
    ADaysBetween := ATotalMilliseconds / AMillisecondsPerDay;
    AWeeksBetween := ATotalMilliseconds / (AMillisecondsPerDay * DaysPerWeek);
    AMonthsBetween := ATotalMilliseconds /
      (AMillisecondsPerDay * ApproxDaysPerMonth);
    AQuarterBetween := AMonthsBetween / 3;
    AYearsBetween := ATotalMilliseconds /
      (AMillisecondsPerDay * ApproxDaysPerYear);
  end;

var
  AHoursBetween, ADaysBetween, AWeeksBetween, AMonthsBetween, AQuarterBetween,
    AYearsBetween: Real;
  AAvailableWidth, AActualUnitMinWidth: Integer;
begin
  AAvailableWidth := cxRectWidth(ClientRect) - FProjectFinishLabelSize.cx -
    FProjectStartLabelSize.cx;
  AActualUnitMinWidth := ScaleFactor.Apply(View.TimescaleUnitMinWidth);
  CalculatePeriodParams(GetProjectStart, GetProjectFinish, AHoursBetween,
    ADaysBetween, AWeeksBetween, AMonthsBetween, AQuarterBetween,
    AYearsBetween);
  FActualTimescaleUnit := View.TimescaleUnit;
  case ActualTimescaleUnit of
    TdxGanttControlTimelineViewTimescaleUnit.Hours:
      FTimeSlotCount := AHoursBetween;
    TdxGanttControlTimelineViewTimescaleUnit.Days:
      FTimeSlotCount := ADaysBetween;
    TdxGanttControlTimelineViewTimescaleUnit.Weeks:
      FTimeSlotCount := AWeeksBetween;
    TdxGanttControlTimelineViewTimescaleUnit.Months:
      FTimeSlotCount := AMonthsBetween;
    TdxGanttControlTimelineViewTimescaleUnit.Quarters:
      FTimeSlotCount := AQuarterBetween;
    TdxGanttControlTimelineViewTimescaleUnit.Years:
      FTimeSlotCount := AYearsBetween;
  else
    begin
      FTimeSlotCount := AHoursBetween;
      FActualTimescaleUnit := TdxGanttControlTimelineViewTimescaleUnit.Hours;
      if FTimeSlotCount * AActualUnitMinWidth > AAvailableWidth then
      begin
        FTimeSlotCount := ADaysBetween;
        FActualTimescaleUnit := TdxGanttControlTimelineViewTimescaleUnit.Days;
      end;
      if FTimeSlotCount * AActualUnitMinWidth > AAvailableWidth then
      begin
        FTimeSlotCount := AWeeksBetween;
        FActualTimescaleUnit := TdxGanttControlTimelineViewTimescaleUnit.Weeks;
      end;
      if FTimeSlotCount * AActualUnitMinWidth > AAvailableWidth then
      begin
        FTimeSlotCount := AMonthsBetween;
        FActualTimescaleUnit := TdxGanttControlTimelineViewTimescaleUnit.Months;
      end;
      if FTimeSlotCount * AActualUnitMinWidth > AAvailableWidth then
      begin
        FTimeSlotCount := AQuarterBetween;
        FActualTimescaleUnit :=
          TdxGanttControlTimelineViewTimescaleUnit.Quarters;
      end;
      if FTimeSlotCount * AActualUnitMinWidth > AAvailableWidth then
      begin
        FTimeSlotCount := AYearsBetween;
        FActualTimescaleUnit := TdxGanttControlTimelineViewTimescaleUnit.Years;
      end;
    end;
  end;
  FTimescaleUnitWidth := Max(Trunc(AAvailableWidth / FTimeSlotCount),
    AActualUnitMinWidth);
end;

procedure TdxGanttControlViewTimelineViewInfo.CalculateStartAndFinishInfoSize;

  function CalculateLabelWidth
    (ALabel: TdxGanttControlTimelineProjectCustomLabel;
    AOffset: Integer): Integer;
  begin
    Result := Max(TdxGanttControlUtils.MeasureTextWidth(ALabel.CaptionLayout,
      ALabel.Caption, ALabel.GetFont), TdxGanttControlUtils.MeasureTextWidth
      (ALabel.DateTimeLayout, ALabel.DateTime, ALabel.GetFont)) + AOffset;
  end;

var
  AOffsetWidth: Integer;
  AStartLabelWidth, AFinishLabelWidth: Integer;
begin
  ProjectStartLabel.Initialize(GetProjectStart);
  ProjectFinishLabel.Initialize(GetProjectFinish);

  AOffsetWidth := GetStartLabelOffsetLeft + GetStartLabelOffsetRight;
  AStartLabelWidth := CalculateLabelWidth(ProjectStartLabel, AOffsetWidth);
  AFinishLabelWidth := CalculateLabelWidth(ProjectFinishLabel, AOffsetWidth);

  ProjectStartLabel.FIsVertical := 2 * (AStartLabelWidth + AFinishLabelWidth) >=
    ClientRect.Width;
  ProjectFinishLabel.FIsVertical := ProjectStartLabel.FIsVertical;
  if ProjectStartLabel.FIsVertical then
  begin
    FProjectStartLabelSize.cx := AOffsetWidth + 2 * FCachedFontHeight;
    FProjectStartLabelSize.cy := AStartLabelWidth;

    FProjectFinishLabelSize.cx := FProjectStartLabelSize.cx;
    FProjectFinishLabelSize.cy := AFinishLabelWidth;
  end
  else
  begin
    FProjectStartLabelSize.cx := AStartLabelWidth;
    FProjectStartLabelSize.cy := 2 * FCachedFontHeight +
      FCachedScaledTextOffset;

    FProjectFinishLabelSize.cx := AFinishLabelWidth;
    FProjectFinishLabelSize.cy := FProjectStartLabelSize.cy;
  end;

  ProjectStartLabel.UpdateTextLayoutFlags;
  ProjectFinishLabel.UpdateTextLayoutFlags;
end;

procedure TdxGanttControlViewTimelineViewInfo.CheckMilestoneVisibility;

  procedure InternalCheckVisibility(ACandidateIndex: Integer);
  var
    ACandidate, ATask: TdxGanttControlTimelineMilestoneTaskViewInfo;
    I: Integer;
    ACanBreak: Boolean;
  begin
    ACandidate := FMilestones[ACandidateIndex];
    ACandidate.IsBarVisible := True;
    ACanBreak := False;
    for I := ACandidateIndex + 1 to FMilestones.Count - 1 do
    begin
      ATask := FMilestones[I];
      if ATask.Visible then
      begin
        ACandidate.IsBarVisible := ACandidate.IsBarVisible and
          (ATask.BarBounds.Left - ACandidate.BarBounds.Left >
          FCachedScaledTextOffset);
        ACandidate.IsLabelVisible := ACandidate.IsBarVisible;
        if not ACandidate.IsLabelVisible then
          ACanBreak := True
        else if ATask.IsLabelVisible then
        begin
          ACandidate.IsLabelVisible :=
            (ATask.GetLabelRect.Left - ACandidate.GetLabelRect.Right) >= 2 *
            FCachedScaledTextOffset;
          ACanBreak := True;
        end;
        if ACanBreak then
          Break;
      end;
    end;
  end;

var
  I: Integer;
begin
  for I := FMilestones.Count - 2 downto 0 do
    InternalCheckVisibility(I);
end;

procedure TdxGanttControlViewTimelineViewInfo.CheckRightToLeftAlignment;
begin
  if not UseRightToLeftAlignment then
    Exit;
  ConvertContentAreas(ClientRect);
  ConvertTimescale(ClientRect);
  ConvertActiveTasks(ClientRect);
  if not FMilestonesAlreadyConverted then
    ConvertMilestones(ClientRect);
  FIsRightToLeftConverted := True;
end;

procedure TdxGanttControlViewTimelineViewInfo.ConvertActiveTasks
  (const AClientRect: TRect);
var
  I: Integer;
begin
  for I := 0 to FActiveTasks.Count - 1 do
    FActiveTasks[I].DoRightToLeftConversion(AClientRect);
end;

procedure TdxGanttControlViewTimelineViewInfo.ConvertContentAreas
  (const AClientRect: TRect);
begin
  FProjectStartLabel.DoRightToLeftConversion(AClientRect);
  FProjectFinishLabel.DoRightToLeftConversion(AClientRect);
  FTaskArea := TdxRightToLeftLayoutConverter.ConvertRect(FTaskArea,
    AClientRect);
end;

procedure TdxGanttControlViewTimelineViewInfo.ConvertMilestones
  (const AClientRect: TRect);
var
  I: Integer;
begin
  for I := 0 to FMilestones.Count - 1 do
    FMilestones[I].DoRightToLeftConversion(AClientRect);
end;

procedure TdxGanttControlViewTimelineViewInfo.ConvertTimescale
  (const AClientRect: TRect);
var
  I: Integer;
begin
  for I := 0 to FTimescaleCells.Count - 1 do
    FTimescaleCells[I].DoRightToLeftConversion(AClientRect);
end;

function TdxGanttControlViewTimelineViewInfo.CalculateHitTest
  (const AHitTest: TdxGanttControlHitTest): Boolean;
var
  I: Integer;
begin
  Result := inherited CalculateHitTest(AHitTest);
  if Result then
  begin
    for I := 0 to FTimescaleCells.Count - 1 do
      if FTimescaleCells[I].CalculateHitTest(AHitTest) then
        Exit;
    for I := 0 to FMilestones.Count - 1 do
      if FMilestones[I].CalculateHitTest(AHitTest) then
        Exit;
    for I := 0 to FActiveTasks.Count - 1 do
      if FActiveTasks[I].CalculateHitTest(AHitTest) then
        Exit;
  end;
end;

procedure TdxGanttControlViewTimelineViewInfo.DoDraw;
var
  I: Integer;
begin
  Canvas.FillRect(Bounds, LookAndFeelPainter.DefaultContentColor);
  Canvas.FrameRect(TaskArea, LookAndFeelPainter.DefaultContentTextColor);
  if FIsEmpty then
    Exit;
  Controller.ScrollBars.DrawSizeGrip(Canvas);

  ProjectStartLabel.Draw;
  ProjectFinishLabel.Draw;
  for I := 0 to FTimescaleCells.Count - 1 do
    FTimescaleCells[I].Draw;
  for I := 0 to FActiveTasks.Count - 1 do
    FActiveTasks[I].Draw;
  for I := 0 to FMilestones.Count - 1 do
    if FMilestones[I].Visible then
      FMilestones[I].Draw;
end;

function TdxGanttControlViewTimelineViewInfo.GetActualScrollbarMode
  : TdxScrollbarMode;
begin
  Result := TdxCustomGanttControlAccess(View.Owner).GetScrollbarMode;
end;

function TdxGanttControlViewTimelineViewInfo.GetController
  : TdxGanttControlTimelineViewController;
begin
  Result := TdxGanttControlTimelineViewController(inherited Controller);
end;

function TdxGanttControlViewTimelineViewInfo.GetCurrentCalendar
  : TdxGanttControlCalendar;
begin
  Result := TdxCustomGanttControl(View.Owner).DataModel.ActiveCalendar;
end;

function TdxGanttControlViewTimelineViewInfo.GetDataProvider
  : TdxGanttControlTimelineViewDataProvider;
begin
  Result := TdxGanttControlTimelineViewDataProvider(inherited DataProvider);
end;

function TdxGanttControlViewTimelineViewInfo.GetDateTimeByPos(X: Integer)
  : TDateTime;

  function InternalGetResult(X1, X2: Integer; tm1, tm2: TDateTime): TDateTime;
  begin
    if not UseRightToLeftAlignment then
      Result := tm1 + (X - X1) / (X2 - X1) * (tm2 - tm1)
    else
      Result := tm1 + (X2 - X) / (X2 - X1) * (tm2 - tm1);
  end;

var
  I: Integer;
  ACount: Integer;
  tm1, tm2: TDateTime;
  X1, X2: Integer;
  ATimescaleCell: TdxGanttControlTimelineScaleCell;
begin
  ACount := TimescaleCells.Count;
  Result := GetProjectStart;
  if not UseRightToLeftAlignment then
  begin
    if X <= TaskArea.Left then
      Exit;
    ATimescaleCell := TimescaleCells[0];
    if X <= ATimescaleCell.Bounds.Left then
      Exit(InternalGetResult(ATimescaleCell.Bounds.Left, TaskArea.Left,
        ATimescaleCell.DateTime, Result));
    if X >= TaskArea.Right then
      Exit(GetProjectFinish);
  end
  else
  begin
    if X >= TaskArea.Right then
      Exit;
    ATimescaleCell := TimescaleCells[0];
    if X >= ATimescaleCell.Bounds.Right then
      Exit(InternalGetResult(ATimescaleCell.Bounds.Right, TaskArea.Right,
        Result, ATimescaleCell.DateTime));
    if X <= TaskArea.Left then
      Exit(GetProjectFinish);
  end;

  for I := 0 to ACount - 1 do
  begin
    ATimescaleCell := TimescaleCells[I];
    if not UseRightToLeftAlignment then
    begin
      X1 := ATimescaleCell.Bounds.Left;
      if I = ACount - 1 then
        X2 := TaskArea.Right
      else
        X2 := ATimescaleCell.Bounds.Right + 1;
    end
    else
    begin
      if I = ACount - 1 then
        X1 := TaskArea.Left
      else
        X1 := ATimescaleCell.Bounds.Left - 1;
      X2 := ATimescaleCell.Bounds.Right;
    end;
    if InRange(X, X1, X2) then
    begin
      tm1 := ATimescaleCell.DateTime;
      if I < ACount - 1 then
        tm2 := TimescaleCells[I + 1].DateTime
      else
        tm2 := GetProjectFinish;
      Result := InternalGetResult(X1, X2, tm1, tm2);
      Break;
    end;
  end;
end;

function TdxGanttControlViewTimelineViewInfo.GetMilestoneInfo
  (ATask: TdxGanttControlTask): TdxGanttControlTimelineMilestoneTaskViewInfo;
var
  I: Integer;
begin
  Result := nil;
  if ATask.Milestone then
    for I := 0 to FMilestones.Count - 1 do
      if FMilestones[I].Task = ATask then
      begin
        Result := FMilestones[I];
        Break;
      end;
end;

function TdxGanttControlViewTimelineViewInfo.GetProjectFinish: TDateTime;
begin
  Result := DataProvider.DataModel.Tasks[0].Finish;
end;

function TdxGanttControlViewTimelineViewInfo.GetProjectStart: TDateTime;
begin
  Result := DataProvider.DataModel.Tasks[0].Start;
end;

function TdxGanttControlViewTimelineViewInfo.GetView
  : TdxGanttControlTimelineView;
begin
  Result := TdxGanttControlTimelineView(inherited View);
end;

function TdxGanttControlViewTimelineViewInfo.GetStartLabelOffsetLeft: Integer;
begin
  Result := ScaleFactor.Apply(FStartLabelLeftOffsetFactor * cxTextOffset);
end;

function TdxGanttControlViewTimelineViewInfo.GetStartLabelOffsetRight: Integer;
begin
  Result := ScaleFactor.Apply(FStartInfoRightOffsetCount * cxTextOffset);
end;

procedure TdxGanttControlViewTimelineViewInfo.PrepareContent;
begin
  FActiveTasks.Clear;
  FTimescaleCells.Clear;
  FIsRightToLeftConverted := False;
  FProjectStartLabel.Free;
  FProjectStartLabel := TdxGanttControlTimelineStartLabel.Create(Self);
  FProjectFinishLabel.Free;
  FProjectFinishLabel := TdxGanttControlTimelineFinishLabel.Create(Self);
  InitializeCache;
end;

function TdxGanttControlViewTimelineViewInfo.GetContentWidth: Integer;
begin
  if IsEmpty then
    Exit(0)
  else if UseRightToLeftAlignment and IsRightToLeftConverted then
    Result := ProjectStartLabel.Bounds.Right - ProjectFinishLabel.Bounds.Left
  else
    Result := ProjectFinishLabel.Bounds.Right - ProjectStartLabel.Bounds.Left;
end;

function TdxGanttControlViewTimelineViewInfo.GetHorzScrollPageWidth: Integer;
begin
  Result := cxRectWidth(ClientRect);
end;

function TdxGanttControlViewTimelineViewInfo.GetVertScrollPageHeight: Integer;
begin
  Result := cxRectHeight(ClientRect);
end;

procedure TdxGanttControlViewTimelineViewInfo.DoScroll(const DX, DY: Integer);
const
  ASign: array [Boolean] of Integer = (1, -1);
var
  I: Integer;
begin
  if (DX = 0) and (DY = 0) then
    Exit;
  Inc(FLeftScrollPos, DX);
  Inc(FTopScrollPos, DY);
  for I := 0 to FMilestones.Count - 1 do
    FMilestones[I].Scroll(ASign[UseRightToLeftAlignment] * -DX, -DY);
  View.Changed([TdxGanttControlOptionsChangedType.View]);
  TdxCustomGanttControlAccess(View.Owner).HintController.Reset;
  Controller.HitTest.Recalculate;
  Invalidate;
  Controller.CalculateScrollBars;
end;

procedure TdxGanttControlViewTimelineViewInfo.Reset;
begin
  inherited Reset;
  ResetLeftScrollPos;
  ResetTopScrollPos;
  FMilestones.Clear;
end;

procedure TdxGanttControlViewTimelineViewInfo.ResetLeftScrollPos;
begin
  FLeftScrollPos := 0;
end;

procedure TdxGanttControlViewTimelineViewInfo.ResetTopScrollPos;
begin
  FTopScrollPos := 0;
end;

procedure TdxGanttControlViewTimelineViewInfo.SetLeftScrollPos(AValue: Integer);
begin
  AValue := Max(0, Min(AValue, ContentWidth - HorzScrollPageWidth));
  if AValue <> FLeftScrollPos then
    DoScroll(AValue - FLeftScrollPos, 0);
end;

procedure TdxGanttControlViewTimelineViewInfo.SetTopScrollPos(AValue: Integer);
begin
  AValue := Max(0, Min(AValue, ContentHeight - VertScrollPageHeight));
  if AValue <> FTopScrollPos then
    DoScroll(0, AValue - FTopScrollPos);
end;

procedure TdxGanttControlViewTimelineViewInfo.InitializeCache;

  procedure UpdateCachedValues;
  begin
    FCachedScaledTextOffset := ScaleFactor.Apply(cxTextOffset);
    FCachedBoldFont := CanvasCache.GetFont(CanvasCache.GetBaseBoldFont);
    FCachedFont := CanvasCache.GetControlFont;
    FCachedBoldFontHeight := TdxGanttControlUtils.MeasureTextHeight
      (GetTextLayout, MaxInt, 'Qq', FCachedBoldFont, True);
    FCachedFontHeight := TdxGanttControlUtils.MeasureTextHeight(GetTextLayout,
      MaxInt, 'Qq', FCachedFont, True);
  end;

  procedure UpdateBaseCachedValues;
  begin
    FCachedTimescaleCaptionOffsetTop :=
      TdxGanttControlTimelineTaskViewInfoCachedValues.FCaptionTopOffsetCount *
      FCachedScaledTextOffset;
    FCachedTimescaleCaptionOffsetLeft :=
      TdxGanttControlTimelineTaskViewInfoCachedValues.FCaptionLeftOffsetCount *
      FCachedScaledTextOffset;
    FCachedTimescaleCaptionOffsetRight :=
      TdxGanttControlTimelineScaleCell.FCaptionRightOffsetCount *
      FCachedScaledTextOffset;
    FCachedTimescaleCaptionOffsetBottom :=
      TdxGanttControlTimelineTaskViewInfoCachedValues.FCaptionBottomOffsetCount
      * FCachedScaledTextOffset;

    FCachedTimescaleCellHeight := TdxGanttControlUtils.MeasureTextHeight
      (GetTextLayout, MaxInt, '0', FCachedFont, True) +
      FCachedTimescaleCaptionOffsetTop + FCachedTimescaleCaptionOffsetBottom;
  end;

begin
  UpdateCachedValues;
  FTaskCachedValues.Update(Self);
  UpdateBaseCachedValues;
end;

procedure TdxGanttControlViewTimelineViewInfo.CalculateClientRect
  (const R: TRect);
begin
  FClientRect := R;
  if GetActualScrollbarMode = sbmClassic then
  begin
    if Controller.ScrollBars.VScrollBar.Visible then
      Dec(FClientRect.Right, Controller.ScrollBars.VScrollBar.Width);
    if Controller.ScrollBars.HScrollBar.Visible then
      Dec(FClientRect.Bottom, Controller.ScrollBars.HScrollBar.Height);
  end;
end;

function TdxGanttControlViewTimelineViewInfo.CalculateTaskViewInfos: Boolean;
var
  ABuilder: TdxGanttControlTimelineTaskLayoutBuilder;

  procedure CreateAndCalculateTaskViewInfos;
  var
    APlace: TdxGanttControlTimelineTaskPlace;
    ABounds: TRect;
    ATask: TdxGanttControlTimelineTaskViewInfo;
    AMilestone: TdxGanttControlTimelineMilestoneTaskViewInfo;
    I: Integer;
  begin
    for I := 0 to ABuilder.TaskPlaces.Count - 1 do
    begin
      ATask := nil;
      APlace := ABuilder.TaskPlace[I];
      CalculateTaskBoundsLeftAndRight(APlace.TimeStart, APlace.TimeFinish,
        ABounds.Left, ABounds.Right);
      if (ABounds.Right >= ClientRect.Left) and
        (ABounds.Left <= ClientRect.Right) then
      begin
        CalculateTaskBoundsTopAndBottom(APlace.LineStart, APlace.LineFinish,
          ABounds.Top, ABounds.Bottom);
        if (ABounds.Bottom >= ClientRect.Top) and
          (ABounds.Top <= ClientRect.Bottom) then
        begin
          ATask := TdxGanttControlTimelineTaskViewInfo.Create(Self,
            APlace.Task);
          FActiveTasks.Add(ATask);
        end;
      end;
      if ATask <> nil then
        ATask.Calculate(ABounds);
      if ABounds.Left > ClientRect.Right then
        Break;
    end;
    FMilestonesAlreadyConverted := UseRightToLeftAlignment and
      (FMilestones.Count > 0);
    if FMilestones.Count = 0 then
      for I := 0 to ABuilder.MilestonePlaces.Count - 1 do
      begin
        APlace := ABuilder.MilestonePlace[I];
        CalculateTaskBoundsLeftAndRight(APlace.TimeStart, APlace.TimeFinish,
          ABounds.Left, ABounds.Right);
        ABounds.Top := TaskArea.Bottom;
        ABounds.Bottom := TaskArea.Bottom;
        AMilestone := TdxGanttControlTimelineMilestoneTaskViewInfo.Create(Self,
          APlace.Task);
        AMilestone.Calculate(ABounds);
        FMilestones.Add(AMilestone);
      end;
  end;

var
  AScrollBars: TdxGanttTimelineScrollBars;
  ANeedCheckMilestoneVisibility: Boolean;
begin
  ABuilder := TdxGanttControlTimelineTaskLayoutBuilder(FTaskLayoutBuilder);
  CalculateContentAreas(ABuilder.LineCount, ABuilder.MilestonePlaces.Count > 0);

  FProjectStartAsMilleseconds := TdxGanttControlUtils.DateTimeToMilliseconds
    (GetProjectStart);
  FProjectFinishAsMilleseconds := TdxGanttControlUtils.DateTimeToMilliseconds
    (GetProjectFinish);
  FDisplayScaleFactor := cxRectWidth(TaskArea) /
    (FProjectFinishAsMilleseconds - FProjectStartAsMilleseconds);

  AScrollBars := Controller.ScrollBars;
  Result := (GetActualScrollbarMode = sbmTouch) or
    not((not AScrollBars.HScrollBar.Data.Visible and
    (ContentWidth > HorzScrollPageWidth)) or
    (AScrollBars.HScrollBar.Data.Visible and
    (ContentWidth <= HorzScrollPageWidth)) or
    (not AScrollBars.VScrollBar.Data.Visible and
    (ContentHeight > VertScrollPageHeight)) or
    (AScrollBars.VScrollBar.Data.Visible and
    (ContentHeight <= VertScrollPageHeight)));
  if Result then
  begin
    ANeedCheckMilestoneVisibility := FMilestones.Count = 0;
    CreateAndCalculateTaskViewInfos;
    if ANeedCheckMilestoneVisibility then
      CheckMilestoneVisibility;
  end;
end;

procedure TdxGanttControlViewTimelineViewInfo.TaskListAfterResetHandler
  (Sender: TObject);
begin
  Controller.ResetSelectionAndFocusedTask;
end;

procedure TdxGanttControlViewTimelineViewInfo.UpdateSpecialCachedValues
  (AUnit: TdxGanttControlTimelineViewTimescaleUnit);
begin
  if AUnit = TdxGanttControlTimelineViewTimescaleUnit.Hours then
  begin
    FCachedTimescaleCaptionFormat :=
      TdxCultureInfo.CurrentCulture.FormatSettings.ShortTimeFormat;
    FCachedTimescaleCaptionMaxWidth := TdxGanttControlUtils.MeasureTextWidth
      (GetTextLayout, FormatDateTime(FCachedTimescaleCaptionFormat, 12 / 24),
      FCachedFont);
  end
  else
  begin
    FCachedTimescaleCaptionFormat :=
      TdxCultureInfo.CurrentCulture.FormatSettings.ShortDateFormat;
    FCachedTimescaleCaptionMaxWidth := TdxGanttControlUtils.MeasureTextWidth
      (GetTextLayout, FormatDateTime(FCachedTimescaleCaptionFormat,
      EncodeDate(2020, 10, 30)), FCachedFont);
  end;
  Inc(FCachedTimescaleCaptionMaxWidth, FCachedTimescaleCaptionOffsetLeft +
    FCachedTimescaleCaptionOffsetRight);
end;

procedure TdxGanttControlViewTimelineViewInfo.ViewChanged;
begin
  CalculateContent(Bounds);
end;

{ TdxGanttControlTimelineViewDataProvider }

constructor TdxGanttControlTimelineViewDataProvider.Create
  (AControl: TdxGanttControlBase);
begin
  inherited Create(AControl);
  FDataModel := TdxCustomGanttControl(AControl).DataModel;
  FDataModel.Tasks.ListChangedHandlers.Add(TaskChangedHandler);
  FDataModel.Tasks.BeforeResetHandlers.Add(TaskBeforeResetHandler);
end;

destructor TdxGanttControlTimelineViewDataProvider.Destroy;
begin
  FDataModel.Tasks.ListChangedHandlers.Remove(TaskChangedHandler);
  FDataModel.Tasks.BeforeResetHandlers.Remove(TaskBeforeResetHandler);
  inherited Destroy;
end;

function TdxGanttControlTimelineViewDataProvider.CanAddItem
  (AItem: TObject): Boolean;
begin
  Result := TdxGanttControlTask(AItem).OutlineLevel > 0;
end;

function TdxGanttControlTimelineViewDataProvider.GetDataItemCount: Integer;
begin
  Result := DataModel.Tasks.Count;
end;

function TdxGanttControlTimelineViewDataProvider.GetDataItem
  (Index: Integer): TObject;
begin
  Result := DataModel.Tasks[Index];
end;

function TdxGanttControlTimelineViewDataProvider.GetTask(Index: Integer)
  : TdxGanttControlTask;
begin
  Result := TdxGanttControlTask(inherited DataItems[Index]);
end;

procedure TdxGanttControlTimelineViewDataProvider.TaskBeforeResetHandler
  (Sender: TObject);
begin
  ClearItems;
end;

procedure TdxGanttControlTimelineViewDataProvider.TaskChangedHandler
  (Sender: TObject; const AItem: TdxGanttControlModelElementListItem;
  AAction: TCollectionNotification);
begin
  if AAction in [cnRemoved, cnExtracted] then
    ItemRemoved(AItem);
end;

{ TdxGanttControlTimelineView }

procedure TdxGanttControlTimelineView.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlTimelineView;
begin
  if Source is TdxGanttControlTimelineView then
  begin
    ASource := TdxGanttControlTimelineView(Source);
    FTimescaleUnit := ASource.TimescaleUnit;
    FTimescaleUnitMinWidth := ASource.TimescaleUnitMinWidth;
    FShowOnlyExplicitlyAddedTasks := ASource.ShowOnlyExplicitlyAddedTasks;
    Changed([TdxGanttControlOptionsChangedType.Layout]);
  end;
  inherited Assign(Source);
end;

function TdxGanttControlTimelineView.CreateController
  : TdxGanttControlViewCustomController;
begin
  Result := TdxGanttControlTimelineViewController.Create(Self);
end;

function TdxGanttControlTimelineView.CreateDataProvider
  : TdxGanttControlCustomDataProvider;
begin
  Result := TdxGanttControlTimelineViewDataProvider.Create(Owner);
end;

function TdxGanttControlTimelineView.CreateViewInfo
  : TdxGanttControlViewCustomViewInfo;
begin
  Result := TdxGanttControlViewTimelineViewInfo.Create(Self);
end;

procedure TdxGanttControlTimelineView.DoReset;
begin
  inherited DoReset;
  FShowOnlyExplicitlyAddedTasks := False;
  FTimescaleUnit := TdxGanttControlTimelineViewTimescaleUnit.Automatic;
  FTimescaleUnitMinWidth := 50;
end;

function TdxGanttControlTimelineView.GetType: TdxGanttControlViewType;
begin
  Result := TdxGanttControlViewType.Timeline;
end;

function TdxGanttControlTimelineView.GetViewInfo
  : TdxGanttControlViewTimelineViewInfo;
begin
  Result := TdxGanttControlViewTimelineViewInfo(inherited ViewInfo);
end;

procedure TdxGanttControlTimelineView.SetShowOnlyExplicitlyAddedTasks
  (const AValue: Boolean);
begin
  if FShowOnlyExplicitlyAddedTasks <> AValue then
  begin
    FShowOnlyExplicitlyAddedTasks := AValue;
    Changed([TdxGanttControlOptionsChangedType.Layout]);
  end;
end;

procedure TdxGanttControlTimelineView.SetTimescaleUnit
  (AValue: TdxGanttControlTimelineViewTimescaleUnit);
begin
  if FTimescaleUnit <> AValue then
  begin
    FTimescaleUnit := AValue;
    Changed([TdxGanttControlOptionsChangedType.Cache,
      TdxGanttControlOptionsChangedType.View]);
  end;
end;

procedure TdxGanttControlTimelineView.SetTimescaleUnitMinWidth(AValue: Integer);
begin
  AValue := Max(AValue, 10);
  if FTimescaleUnitMinWidth <> AValue then
  begin
    FTimescaleUnitMinWidth := AValue;
    Changed([TdxGanttControlOptionsChangedType.Cache,
      TdxGanttControlOptionsChangedType.View]);
  end;
end;

end.
