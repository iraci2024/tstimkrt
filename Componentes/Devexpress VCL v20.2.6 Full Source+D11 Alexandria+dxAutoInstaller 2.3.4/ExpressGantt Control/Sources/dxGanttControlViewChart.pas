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

unit dxGanttControlViewChart;

{$I cxVer.inc}
{$I dxGanttControl.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes, System.Contnrs,
{$ENDIF}
  SysUtils, Windows, Messages, MultiMon, Types, Forms, Controls, Graphics,
  Dialogs,
  Classes, Themes, Generics.Defaults, Generics.Collections, StdCtrls,
  dxCore, dxCoreClasses, cxClasses, cxControls, cxCustomCanvas, cxGeometry,
  cxGraphics, cxLookAndFeels, cxLookAndFeelPainters, dxTouch, dxCoreGraphics,
  dxGanttControlCustomView,
  dxGanttControlCommands,
  dxGanttControlCustomSheet,
  dxGanttControlCustomClasses,
  dxGanttControlSplitter,
  dxGanttControlDataModel,
  dxGanttControlTasks,
  dxGanttControlCalendars,
  dxGanttControlCustomDataModel;

type
  TdxGanttControlGridlineOptions = class;
  TdxGanttControlChartView = class;
  TdxGanttControlViewChartAreaViewInfo = class;
  TdxGanttControlViewChartViewInfo = class;
  TdxGanttControlViewChartAreaHeaderViewInfo = class;
  TdxGanttControlChartViewSheetController = class;
  TdxGanttControlChartViewAreaController = class;
  TdxGanttControlChartViewController = class;
  TdxGanttControlChartViewDataProvider = class;

  TdxGanttControlChartViewTimescaleUnit = (Hours, QuarterDays, Days, Weeks,
    ThirdsOfMonths, Months, Quarters, HalfYears, Years);
  TdxGanttControlChartViewMinorHeaderWidth = (Normal = 1, Large, ExtraLarge);
  // for internal use

  { TdxViewChartCreateTaskCustomCommand }

  TdxViewChartCreateTaskCustomCommand = class(TdxGanttControlCommand)
  strict private
    FController: TdxGanttControlChartViewController;
    FIndex: Integer;
    FTask: TdxGanttControlTask;
  protected
    procedure BeforeExecute; override;
    property Task: TdxGanttControlTask read FTask;
  public
    constructor Create(AController: TdxGanttControlChartViewController;
      AIndex: Integer); reintroduce;
  end;

  { TdxViewChartAreaCreateTaskCommand }

  TdxViewChartAreaCreateTaskCommand = class(TdxViewChartCreateTaskCustomCommand)
  strict private
    FFinish: TDateTime;
    FStart: TDateTime;
  protected
    procedure DoExecute; override;
  public
    constructor Create(AController: TdxGanttControlChartViewAreaController;
      AIndex: Integer; AStart, AFinish: TDateTime); reintroduce;
  end;

  { TdxViewChartCreateTaskCommand }

  TdxViewChartCreateTaskCommand = class(TdxViewChartCreateTaskCustomCommand)
  strict private
    FValue: TdxGanttControlTask;
  protected
    procedure DoExecute; override;
  public
    constructor Create(AController: TdxGanttControlChartViewController;
      AIndex: Integer; AValue: TdxGanttControlTask); reintroduce;
  end;

  { TdxViewChartDeleteFocusedTaskCommand }

  TdxViewChartDeleteFocusedTaskCommand = class
    (TdxGanttControlSheetDeleteFocusedItemCommand)
  strict private
    function GetController: TdxGanttControlChartViewSheetController; inline;
  protected
    procedure DoExecute; override;
    property Controller: TdxGanttControlChartViewSheetController
      read GetController;
  end;

  { TdxGanttControlViewChartSheetViewInfo }

  TdxGanttControlViewChartSheetViewInfo = class
    (TdxGanttControlSheetCustomViewInfo)
  strict private
    function GetOwner: TdxGanttControlViewChartViewInfo; inline;
  protected
    function GetColumnHeaderHeight: Integer; override;
    procedure PrepareCanvas(ACanvas: TcxCustomCanvas); override;
  public
    property Owner: TdxGanttControlViewChartViewInfo read GetOwner;
  end;

  { TdxGanttControlViewChartAreaScaleHeaderViewInfo }

  TdxGanttControlViewChartAreaScaleHeaderViewInfo = class abstract
    (TdxGanttControlCustomOwnedItemViewInfo)
  strict private
    FDateTime: TDateTime;
    FCaption: string;
    FCaptionBounds: TRect;
    FSeparatorBounds: TRect;
    function GetOwner: TdxGanttControlViewChartAreaHeaderViewInfo; inline;
  protected
    function CalculateCaption: string; virtual; abstract;
    function CalculateDrawTextFlags: Integer; virtual;
    function CalculateSeparatorBounds: TRect; virtual;
    procedure DoDraw; override;
    function GetTextLayout: TcxCanvasBasedTextLayout; virtual; abstract;
  public
    constructor Create(AOwner: TdxGanttControlViewChartAreaHeaderViewInfo;
      ADateTime: TDateTime); reintroduce;
    procedure Calculate(const R: TRect); override;

    property Caption: string read FCaption;
    property DateTime: TDateTime read FDateTime;
    property Owner: TdxGanttControlViewChartAreaHeaderViewInfo read GetOwner;
  end;

  { TdxGanttControlViewChartAreaMajorHeaderViewInfo }

  TdxGanttControlViewChartAreaMajorHeaderViewInfo = class
    (TdxGanttControlViewChartAreaScaleHeaderViewInfo)
  protected
    function CalculateCaption: string; override;
    function CalculateDrawTextFlags: Integer; override;
    function CalculateSeparatorBounds: TRect; override;
    function GetTextLayout: TcxCanvasBasedTextLayout; override;
  end;

  { TdxGanttControlViewChartAreaMinorHeaderViewInfo }

  TdxGanttControlViewChartAreaMinorHeaderViewInfo = class
    (TdxGanttControlViewChartAreaScaleHeaderViewInfo)
  protected
    function CalculateCaption: string; override;
    function CalculateDrawTextFlags: Integer; override;
    function CalculateSeparatorBounds: TRect; override;
    function GetHintText: string; override;
    function GetTextLayout: TcxCanvasBasedTextLayout; override;
    function HasHint: Boolean; override;
  end;

  { TdxGanttControlViewChartAreaHeaderViewInfo }

  TdxGanttControlViewChartAreaHeaderViewInfo = class
    (TdxGanttControlCustomParentViewInfo)
  strict private
    FFirstVisibleDateTime: TDateTime;
    FMajorHeaders: TObjectList<TdxGanttControlViewChartAreaMajorHeaderViewInfo>;
    FMinorHeaders: TObjectList<TdxGanttControlViewChartAreaMinorHeaderViewInfo>;

    function GetFirstVisibleMajorDateTime: TDateTime;
    function GetNextMajorDateTime(ADateTime: TDateTime): TDateTime;

    function CalculateMajorHeaderHeight: Integer;
    function CalculateMajorHeaderWidth(ADateTime: TDateTime;
      ATotalWidth: Integer): Integer;
    function CalculateMajorHeaderFirstOffset: Integer;
    procedure DoCalculate;

    procedure CalculateMajorHeaders;
    procedure CalculateMinorHeaders;
    function GetOwner: TdxGanttControlViewChartAreaViewInfo; inline;
    function GetTimescaleUnit: TdxGanttControlChartViewTimescaleUnit; inline;
  protected
    FCachedMinorHeaderWidth: Integer;
    procedure DoDraw; override;

    function CalculateHitTest(const AHitTest: TdxGanttControlHitTest)
      : Boolean; override;
    function CalculateMinorHeaderWidth: Integer;
    function GetMinorDateTime(ADateTime: TDateTime): TDateTime;
    function GetFirstVisibleMinorDateTime: TDateTime;
    function GetNextMinorDateTime(ADateTime: TDateTime): TDateTime; overload;
    function GetNextMinorDateTime(ADateTime: TDateTime; ACount: Integer)
      : TDateTime; overload;
    function GetPreviousMinorDateTime(ADateTime: TDateTime): TDateTime;
    function IsEndOfMajor(AViewInfo
      : TdxGanttControlViewChartAreaMinorHeaderViewInfo): Boolean;
    function GetMinorHeaderViewInfo(ADateTime: TDateTime; AIsStart: Boolean)
      : TdxGanttControlViewChartAreaMinorHeaderViewInfo;

    function GetDateTimeByPos(X: Integer): TDateTime;
    function GetPosByDateTime(ADateTime: TDateTime): Integer;
    procedure Reset; override;

    property MajorHeaders
      : TObjectList<TdxGanttControlViewChartAreaMajorHeaderViewInfo>
      read FMajorHeaders;
    property MinorHeaders
      : TObjectList<TdxGanttControlViewChartAreaMinorHeaderViewInfo>
      read FMinorHeaders;
    property TimescaleUnit: TdxGanttControlChartViewTimescaleUnit
      read GetTimescaleUnit;
  public
    constructor Create(AOwner: TdxGanttControlCustomItemViewInfo); override;
    destructor Destroy; override;
    procedure Calculate(const R: TRect); override;
    procedure ViewChanged; override;

    property Owner: TdxGanttControlViewChartAreaViewInfo read GetOwner;
  end;

  { TdxGanttControlViewChartTaskViewInfo }

  TdxGanttControlViewChartTaskViewInfo = class
    (TdxGanttControlTaskCustomViewInfo)
  public const
{$REGION 'public const'}
    MaxCaptionWidth = 1000;
    TaskMinWidth = 2;
{$ENDREGION}
  strict private
    function GetOwner: TdxGanttControlViewChartAreaViewInfo; inline;
  protected
    function CalculateBarBounds(const R: TRect): TRect; override;
    function CalculateBarProgressBounds(const R: TRect): TRect; override;
    function CalculateCaptionBounds(const R: TRect): TRect; override;
    function DoCalculateBarBounds(ATaskStart, ATaskFinish: TDateTime)
      : TRect; virtual;
    function DoCalculateBarProgressBounds(AValue: Integer): TRect; virtual;

    procedure DoDraw; override;
    procedure DoDrawBar(ACanvas: TcxCustomCanvas;
      const ABounds, AProgressBounds: TRect); virtual;
    procedure DoDrawCaption; virtual;
    function IsFocused: Boolean;

    function GetCachedValues: TdxGanttControlTaskViewInfoCachedValues; override;
    function GetCaption: string; override;
    function GetCurrentCursor(const P: TPoint; const ADefaultCursor: TCursor)
      : TCursor; override;

    function GetHitBounds: TRect; override;
    function IsCompleteZone(const P: TPoint): Boolean; virtual;
    function IsMovingZone(const P: TPoint): Boolean; virtual;
    function IsSizingZone(const P: TPoint): Boolean; virtual;
  public
    property Owner: TdxGanttControlViewChartAreaViewInfo read GetOwner;
  end;

  { TdxGanttControlViewChartSummaryTaskViewInfo }

  TdxGanttControlViewChartSummaryTaskViewInfo = class
    (TdxGanttControlViewChartTaskViewInfo)
  protected
    procedure DoDrawBar(ACanvas: TcxCustomCanvas;
      const ABounds, AProgressBounds: TRect); override;
    function GetDefaultColor: TColor; override;
    function GetTaskHeight: Integer; override;
  end;

  { TdxGanttControlViewChartMilestoneTaskViewInfo }

  TdxGanttControlViewChartMilestoneTaskViewInfo = class
    (TdxGanttControlViewChartTaskViewInfo)
  protected
    function DoCalculateBarBounds(ATaskStart, ATaskFinish: TDateTime)
      : TRect; override;
    procedure DoDrawBar(ACanvas: TcxCustomCanvas;
      const ABounds, AProgressBounds: TRect); override;
    function GetDefaultColor: TColor; override;
    function GetTaskHeight: Integer; override;
    function IsCompleteZone(const P: TPoint): Boolean; override;
    function IsSizingZone(const P: TPoint): Boolean; override;
  end;

  { TdxGanttControlViewChartEmptyTaskViewInfo }

  TdxGanttControlViewChartEmptyTaskViewInfo = class
    (TdxGanttControlViewChartTaskViewInfo)
  protected
    function CalculateBarBounds(const R: TRect): TRect; override;
    function CalculateBarProgressBounds(const R: TRect): TRect; override;
    function GetCaption: string; override;
    function GetCurrentCalendar: TdxGanttControlCalendar; override;
    function GetDefaultColor: TColor; override;
    function IsCompleteZone(const P: TPoint): Boolean; override;
    function IsMovingZone(const P: TPoint): Boolean; override;
    function IsSizingZone(const P: TPoint): Boolean; override;
  end;

  { TdxGanttControlViewChartPredecessorLinkViewInfo }

  TdxGanttControlViewChartPredecessorLinkViewInfo = class abstract
    (TdxGanttControlCustomOwnedItemViewInfo)
  public const
{$REGION 'public const'}
    LinkArc = 3;
    LinkArrowWidth = 5;
    LinkLineStartOffset = 5;
    LinkLineFinishOffset = 10;
    LinkLineWidth = 1;
{$ENDREGION}
  strict private
    FArrowPoints: TcxArrowPoints;
    FLink: TdxGanttControlTaskPredecessorLink;
    FPoints: TPoints;
    FPredecessorViewInfo: TdxGanttControlViewChartTaskViewInfo;
    FTaskViewInfo: TdxGanttControlViewChartTaskViewInfo;

    procedure CalculateArrow;
    function IsHotState: Boolean;
    function GetArrowDirection: TcxArrowDirection; inline;
    function GetColor: TColor;
    function GetWidth: Integer;
    function GetOwner: TdxGanttControlViewChartAreaViewInfo; inline;
  protected
    procedure DoDraw; override;

    procedure AddPoint(const P: TPoint; var AIndex: Integer);
    function GetLinkType: TdxGanttControlTaskPredecessorLinkType;
      virtual; abstract;

    function CalculateHitTest(const AHitTest: TdxGanttControlHitTest)
      : Boolean; override;
    procedure DoCalculate(const AStartPoint, AFinishPoint: TPoint;
      var AIndex: Integer); virtual;
    function GetFinishPoint: TPoint;
    function GetHintText: string; override;
    function GetStartPoint: TPoint;
    function GetStartLineFinishPoint(const AStartPoint: TPoint): TPoint;
    function HasHotTrackState: Boolean; override;
    function HasHint: Boolean; override;

    property Link: TdxGanttControlTaskPredecessorLink read FLink;
    property PredecessorViewInfo: TdxGanttControlViewChartTaskViewInfo
      read FPredecessorViewInfo;
    property TaskViewInfo: TdxGanttControlViewChartTaskViewInfo
      read FTaskViewInfo;
  public
    constructor Create(AOwner: TdxGanttControlViewChartAreaViewInfo;
      ALink: TdxGanttControlTaskPredecessorLink;
      ATaskViewInfo, APredecessorViewInfo
      : TdxGanttControlViewChartTaskViewInfo); reintroduce;

    procedure Calculate(const R: TRect); override;

    property Owner: TdxGanttControlViewChartAreaViewInfo read GetOwner;
  end;

  { TdxGanttControlViewChartPredecessorLinkFFViewInfo }

  TdxGanttControlViewChartPredecessorLinkFFViewInfo = class
    (TdxGanttControlViewChartPredecessorLinkViewInfo)
  protected
    procedure DoCalculate(const AStartPoint, AFinishPoint: TPoint;
      var AIndex: Integer); override;
    function GetLinkType: TdxGanttControlTaskPredecessorLinkType; override;
  end;

  { TdxGanttControlViewChartPredecessorLinkFSViewInfo }

  TdxGanttControlViewChartPredecessorLinkFSViewInfo = class
    (TdxGanttControlViewChartPredecessorLinkViewInfo)
  protected
    procedure DoCalculate(const AStartPoint, AFinishPoint: TPoint;
      var AIndex: Integer); override;
    function GetLinkType: TdxGanttControlTaskPredecessorLinkType; override;
  end;

  { TdxGanttControlViewChartPredecessorLinkSSViewInfo }

  TdxGanttControlViewChartPredecessorLinkSSViewInfo = class
    (TdxGanttControlViewChartPredecessorLinkViewInfo)
  protected
    procedure DoCalculate(const AStartPoint, AFinishPoint: TPoint;
      var AIndex: Integer); override;
    function GetLinkType: TdxGanttControlTaskPredecessorLinkType; override;
  end;

  { TdxGanttControlViewChartPredecessorLinkSFViewInfo }

  TdxGanttControlViewChartPredecessorLinkSFViewInfo = class
    (TdxGanttControlViewChartPredecessorLinkViewInfo)
  protected
    procedure DoCalculate(const AStartPoint, AFinishPoint: TPoint;
      var AIndex: Integer); override;
    function GetLinkType: TdxGanttControlTaskPredecessorLinkType; override;
  end;

  { TdxChartAreaTextLayoutDictionary }

  TdxChartAreaTextLayoutDictionary = class // for internal use
  strict private
  type
    TCache = class(TObjectDictionary<Integer, TcxCanvasBasedTextLayout>);
  strict private
    FCachedTextLayout: TcxCanvasBasedTextLayout;
    FMajorHeaderCache: TCache;
    FMinorHeaderCache: TCache;
    FTaskCaption: TCache;
    FOwner: TdxGanttControlViewChartAreaViewInfo;
    function GetTextLayout(ACache: TCache; const AText: string)
      : TcxCanvasBasedTextLayout;
  public
    constructor Create(AOwner: TdxGanttControlViewChartAreaViewInfo);
    destructor Destroy; override;
    procedure Clear;
    function GetMajorHeaderTextLayout(const AText: string)
      : TcxCanvasBasedTextLayout; inline;
    function GetMinorHeaderTextLayout(const AText: string)
      : TcxCanvasBasedTextLayout; inline;
    function GetTaskCaptionTextLayout(const AText: string)
      : TcxCanvasBasedTextLayout; inline;
  end;

  { TdxGanttControlViewChartAreaViewInfo }

  TdxGanttControlViewChartAreaViewInfo = class
    (TdxGanttControlCustomParentViewInfo)
  strict private
    FTaskCachedValues: TdxGanttControlTaskViewInfoCachedValues;
    FCalendar: TdxGanttControlCalendar;
    FClientRect: TRect;
    FHiddenTasks: TObjectList<TdxGanttControlViewChartTaskViewInfo>;
    FNonWorkTimeAreas: TList<TRect>;
    FPredecessorLinks
      : TObjectList<TdxGanttControlViewChartPredecessorLinkViewInfo>;
    FTasks: TObjectList<TdxGanttControlViewChartTaskViewInfo>;
    FTextLayoutDictionary: TdxChartAreaTextLayoutDictionary;

    FGridlineCurrentDateBounds: TRect;
    FGridlineProjectFinishBounds: TRect;
    FGridlineProjectStartBounds: TRect;

    function CalculateHeaderBounds: TRect;
    procedure CalculateNonWorkTimeAreas;
    procedure CalculatePredecessorLinks;
    procedure CalculateGridlines;
    procedure CalculateTasks;
    function CreateTaskViewInfo(ATask: TdxGanttControlTask)
      : TdxGanttControlViewChartTaskViewInfo;
    procedure DoCalculate;
    function IsNonWorkTime(ATime: TDateTime): Boolean;

    function GetController: TdxGanttControlChartViewAreaController; inline;
    function GetCurrentCalendar: TdxGanttControlCalendar;
    function GetFullyVisibleTaskCount: Integer; inline;
    function GetHeaderViewInfo
      : TdxGanttControlViewChartAreaHeaderViewInfo; inline;
    function GetHScrollBarPageSize: Integer;
    function GetOwner: TdxGanttControlViewChartViewInfo; inline;
  protected
    FFirstVisibleRowIndex: Integer;
    FFirstVisibleDateTime: TDateTime;
    FTimescaleUnit: TdxGanttControlChartViewTimescaleUnit;

    procedure DoDraw; override;
    function CalculateItemBounds(AItem: TdxGanttControlCustomItemViewInfo)
      : TRect; override;
    function CalculateHitTest(const AHitTest: TdxGanttControlHitTest)
      : Boolean; override;
    function GetTaskRowViewInfo(const P: TPoint)
      : TdxGanttControlViewChartTaskViewInfo;

    function GetMajorHeaderTextLayout(const AText: string)
      : TcxCanvasBasedTextLayout; inline;
    function GetMinorHeaderTextLayout(const AText: string)
      : TcxCanvasBasedTextLayout; inline;
    function GetTaskCaptionTextLayout(const AText: string)
      : TcxCanvasBasedTextLayout; inline;
    procedure Reset; override;

    property Calendar: TdxGanttControlCalendar read FCalendar;
    property Controller: TdxGanttControlChartViewAreaController
      read GetController;
    property FullyVisibleTaskCount: Integer read GetFullyVisibleTaskCount;
    property HeaderViewInfo: TdxGanttControlViewChartAreaHeaderViewInfo
      read GetHeaderViewInfo;
    property HScrollBarPageSize: Integer read GetHScrollBarPageSize;
    property TaskCachedValues: TdxGanttControlTaskViewInfoCachedValues
      read FTaskCachedValues;
  public
    constructor Create(AOwner: TdxGanttControlCustomItemViewInfo); override;
    destructor Destroy; override;

    procedure Calculate(const R: TRect); override;
    procedure CalculateLayout; override;
    procedure ViewChanged; override;

    property ClientRect: TRect read FClientRect;
    property Tasks: TObjectList<TdxGanttControlViewChartTaskViewInfo>
      read FTasks;
    property Owner: TdxGanttControlViewChartViewInfo read GetOwner;
  end;

  { TdxGanttControlViewChartViewInfo }

  TdxGanttControlViewChartViewInfo = class(TdxGanttControlViewCustomViewInfo)
  strict private
    FChartAreaViewInfo: TdxGanttControlViewChartAreaViewInfo;
    FSheetViewInfo: TdxGanttControlViewChartSheetViewInfo;
    FSplitterViewInfo: TdxGanttControlSplitterViewInfo;
    function GetController: TdxGanttControlChartViewController; inline;
    function GetTotalRowCount: Integer;
    function GetView: TdxGanttControlChartView; inline;
    function GetVisibleRowCount: Integer;
  protected
    function CreateSheetViewInfo: TdxGanttControlViewChartSheetViewInfo;
    function CalculateChartAreaBounds: TRect;
    function CalculateSheetBounds: TRect;
    function CalculateSplitterBounds: TRect;
    procedure Clear; override;
    function GetMaxSheetWidth: Integer;

    function GetHeaderHeight: Integer;

    function CalculateItemBounds(AItem: TdxGanttControlCustomItemViewInfo)
      : TRect; override;

    property Controller: TdxGanttControlChartViewController read GetController;
    property TotalRowCount: Integer read GetTotalRowCount;
    property VisibleRowCount: Integer read GetVisibleRowCount;
  public
    procedure CalculateLayout; override;

    property ChartAreaViewInfo: TdxGanttControlViewChartAreaViewInfo
      read FChartAreaViewInfo;
    property SheetViewInfo: TdxGanttControlViewChartSheetViewInfo
      read FSheetViewInfo;
    property SplitterViewInfo: TdxGanttControlSplitterViewInfo
      read FSplitterViewInfo;

    property View: TdxGanttControlChartView read GetView;
  end;

  { TdxGanttControlChartViewSplitterOptions }

  TdxGanttControlChartViewSplitterOptions = class
    (TdxGanttControlSplitterOptions) // for internal use
  protected
    function GetScaleFactor: TdxScaleFactor; override;
  end;

  { TdxGanttControlChartViewSheetDragHelper }

  TdxGanttControlChartViewSheetDragHelper = class
    (TdxGanttControlSheetDragHelper) // for internal use
  strict private
    function CreateTaskOutlineLevelChangingObject(ACellViewInfo
      : TdxGanttControlSheetCellViewInfo): TdxGanttControlResizingObject;
    function GetController: TdxGanttControlChartViewSheetController; inline;
  protected
    function CreateDragAndDropObjectByPoint(const P: TPoint)
      : TdxGanttControlDragAndDropObject; override;
  public
    property Controller: TdxGanttControlChartViewSheetController
      read GetController;
  end;

  { TdxChartViewSheetScrollBars }

  TdxChartViewSheetScrollBars = class(TdxGanttSheetScrollBars)
  // for internal use
  protected
    function GetMouseWheelActiveScrollBar(Shift: TShiftState)
      : IcxControlScrollBar; override;
    procedure DoInitVScrollBarParameters; override;
    function CanScrolling(AKind: TScrollBarKind): Boolean; override;
  end;

  { TdxGanttControlChartViewSheetController }

  TdxGanttControlChartViewSheetController = class
    (TdxGanttControlSheetController)
  strict private
    FParentController: TdxGanttControlChartViewController;
    procedure TaskListBeforeResetHandler(Sender: TObject);
    procedure TaskChangedHandler(Sender: TdxGanttControlDataModel;
      ATask: TdxGanttControlTask);
    procedure TaskListChangedHandler(Sender: TObject;
      const AItem: TdxGanttControlModelElementListItem;
      AAction: TCollectionNotification);
    function GetFocusedDataItem: TdxGanttControlTask; inline;
    function GetScrollBars: TdxChartViewSheetScrollBars; inline;
  protected
    function CreateChangeCellValueCommand(const AValue: Variant)
      : TdxGanttControlSheetChangeCellValueCommand; override;
    function CreateDragHelper: TdxGanttControlDragHelper; override;
    function CreateScrollBars: TdxGanttSheetScrollBars; override;
    function GetViewInfo: TdxGanttControlCustomItemViewInfo; override;
    procedure DeleteFocusedItem; override;
    procedure DoDblClick; override;
    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure DoMouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    function GetVisibleRowCount: Integer; override;
    function HasItemExpandState(AItem: TObject): Boolean; override;
    procedure ShowTaskInformationDialog(ATask: TdxGanttControlTask);

    property FocusedDataItem: TdxGanttControlTask read GetFocusedDataItem;
    property ScrollBars: TdxChartViewSheetScrollBars read GetScrollBars;
  public
    constructor Create(AParentController: TdxGanttControlChartViewController);
      reintroduce;
    destructor Destroy; override;
  end;

  { TdxChartTaskDragCustomImage }

  TdxChartTaskDragCustomImage = class(TcxDragImage) // for internal use
  strict private
    FImage: TcxAlphaBitmap;
    FImageOffset: TPoint;
  protected
    procedure Paint; override;

    procedure SetImageOffset(const P: TPoint);
    procedure SetImageSize(const ASize: TSize);

    procedure DoUpdateImage(AImage: TcxAlphaBitmap); virtual; abstract;
    procedure UpdateImage;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;

  { TdxChartTaskDragObject }

  TdxChartTaskDragObject = class abstract(TdxGanttControlMovingObject)
  // for internal use
  strict private
    FTask: TdxGanttControlTask;
    FTaskViewInfoBounds: TRect;
    FTaskViewInfo: TdxGanttControlViewChartTaskViewInfo;
    function GetController: TdxGanttControlChartViewAreaController; inline;
    function GetDragImage: TdxChartTaskDragCustomImage; inline;
    function InternalGetTaskViewInfo: TdxGanttControlViewChartTaskViewInfo;
  protected
    FFinish: TDateTime;
    FStart: TDateTime;

    procedure AfterScrolling(AScrollDirection: TcxDirection); override;
    procedure BeginDragAndDrop; override;
    function CanDrop(const P: TPoint): Boolean; override;
    function CanScroll(ADirection: TcxDirection): Boolean; virtual;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    function GetScrollableArea: TRect; virtual;
    function GetTaskViewInfo: TdxGanttControlViewChartTaskViewInfo; virtual;

    procedure UpdateDragImage; virtual;
    procedure UpdateTimeBounds(P: TPoint); virtual; abstract;

    function GetCalendar: TdxGanttControlCalendar; virtual;

    property Task: TdxGanttControlTask read FTask;
    property TaskViewInfo: TdxGanttControlViewChartTaskViewInfo
      read InternalGetTaskViewInfo;
    property TaskViewInfoBounds: TRect read FTaskViewInfoBounds;
  public
    constructor Create(AController: TdxGanttControlChartViewAreaController;
      ATaskViewInfo: TdxGanttControlViewChartTaskViewInfo);
      reintroduce; virtual;
    property Controller: TdxGanttControlChartViewAreaController
      read GetController;

    property DragImage: TdxChartTaskDragCustomImage read GetDragImage;
  end;

  { TdxChartTaskDragImage }

  TdxChartTaskDragImage = class(TdxChartTaskDragCustomImage) // for internal use
  public const
    LineWidth = 2;
  strict private
    FColor: TColor;
    FWidth: Integer;
  protected
    procedure DoUpdateImage(AImage: TcxAlphaBitmap); override;
  public
    constructor Create(AWidth: Integer; AColor: TColor); reintroduce;
  end;

  { TdxChartTaskChangeTimeBoundObject }

  TdxChartTaskChangeTimeBoundObject = class abstract(TdxChartTaskDragObject)
  // for internal use
  protected
    function CanScroll(ADirection: TcxDirection): Boolean; override;
    function CreateDragImage: TcxDragImage; override;
    function GetScrollableArea: TRect; override;
  end;

  { TdxChartTaskResizingObject }

  TdxChartTaskResizingObject = class(TdxChartTaskChangeTimeBoundObject)
  // for internal use
  protected
    procedure ApplyChanges(const P: TPoint); override;
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;
    procedure UpdateTimeBounds(P: TPoint); override;
  end;

  { TdxChartTaskMovingObject }

  TdxChartTaskMovingObject = class(TdxChartTaskChangeTimeBoundObject)
  // for internal use
  strict private
    FStartOffset: Integer;
    FStartPoint: TPoint;
  protected
    procedure ApplyChanges(const P: TPoint); override;
    procedure BeforeBeginDragAndDrop; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;
    function IsLinking(const P: TPoint): Boolean;
    procedure UpdateTimeBounds(P: TPoint); override;

    function Clone: TdxChartTaskMovingObject;

    property StartOffset: Integer read FStartOffset;
  public
    constructor Create(AController: TdxGanttControlChartViewAreaController;
      ATaskViewInfo: TdxGanttControlViewChartTaskViewInfo); override;
  end;

  { TdxChartTaskLinkingImage }

  TdxChartTaskLinkingImage = class(TdxChartTaskDragCustomImage)
  // for internal use
  public const
    DefaultLineWidth = 2;
  strict private
    FColor: TColor;
    FFinish: TPoint;
    FStart: TPoint;
    FLineWidth: Integer;
  protected
    procedure DoUpdateImage(AImage: TcxAlphaBitmap); override;
    function OffsetStart(DX, DY: Integer): Boolean;
    procedure SetFinish(const Value: TPoint);
  public
    constructor Create(const ABounds: TRect; const AStart: TPoint;
      AColor: TColor; ALineWidth: Integer); reintroduce;
  end;

  { TdxChartTaskLinkingObject }

  TdxChartTaskLinkingObject = class(TdxChartTaskDragObject) // for internal use
  strict private
    FClientRect: TRect;
    FBeforeScrollingFirstVisibleDateTime: TDateTime;
    FBeforeScrollingFirstVisibleRowIndex: Integer;
    FBeforeScrollingFirstVisibleRowHeight: Integer;
    FStart: TPoint;
    function GetDragImage: TdxChartTaskLinkingImage; inline;
  protected
    procedure AfterScrolling(AScrollDirection: TcxDirection); override;
    procedure ApplyChanges(const P: TPoint); override;
    procedure BeforeScrolling; override;
    function CreateDragImage: TcxDragImage; override;
    function CanDrop(const P: TPoint): Boolean; override;
    function CanScroll(ADirection: TcxDirection): Boolean; override;
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;
    function GetScrollableArea: TRect; override;
    procedure UpdateDragImage; override;
    procedure UpdateTimeBounds(P: TPoint); override;
  public
    constructor Create(AMovingObject: TdxChartTaskMovingObject); reintroduce;
    property DragImage: TdxChartTaskLinkingImage read GetDragImage;
  end;

  { TdxChartTaskProgressImage }

  TdxChartTaskProgressImage = class(TdxChartTaskDragCustomImage)
  // for internal use
  strict private
    FDragObject: TdxChartTaskDragObject;
    FValue: Integer;
    procedure SetValue(const Value: Integer);
  protected
    procedure DoUpdateImage(AImage: TcxAlphaBitmap); override;
  public
    constructor Create(ADragObject: TdxChartTaskDragObject); reintroduce;
    property Value: Integer read FValue write SetValue;
  end;

  { TdxChartTaskChangeProgressObject }

  TdxChartTaskChangeProgressObject = class(TdxChartTaskDragObject)
  // for internal use
  strict private
    FValue: Integer;
    function GetDragImage: TdxChartTaskProgressImage; inline;
  protected
    procedure ApplyChanges(const P: TPoint); override;
    procedure BeginDragAndDrop; override;
    function CanScroll(ADirection: TcxDirection): Boolean; override;
    function CreateDragImage: TcxDragImage; override;
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;
    function GetScrollableArea: TRect; override;
    procedure UpdateTimeBounds(P: TPoint); override;

    property DragImage: TdxChartTaskProgressImage read GetDragImage;
  end;

  { TdxChartNewTaskCreatingObject }

  TdxChartNewTaskCreatingObject = class(TdxChartTaskChangeTimeBoundObject)
  // for internal use
  strict private
    FTaskViewInfoIndex: Integer;
  protected
    procedure ApplyChanges(const P: TPoint); override;
    procedure BeginDragAndDrop; override;
    function CanDrop(const P: TPoint): Boolean; override;
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;
    function GetTaskViewInfo: TdxGanttControlViewChartTaskViewInfo; override;
    procedure UpdateTimeBounds(P: TPoint); override;
  end;

  { TdxChartDragHelper }

  TdxChartDragHelper = class(TdxGanttControlDragHelper) // for internal use
  strict private
    FConvertedObject: TdxChartTaskMovingObject;
    function GetController: TdxGanttControlChartViewAreaController; inline;
    function CreateChangePercentCompleteObject
      (AViewInfo: TdxGanttControlViewChartTaskViewInfo)
      : TdxChartTaskChangeProgressObject;
    function CreateLinkingObject(AMovingObject: TdxChartTaskMovingObject)
      : TdxChartTaskLinkingObject;
    function CreateMovingObject(AViewInfo: TdxGanttControlViewChartTaskViewInfo)
      : TdxChartTaskMovingObject;
    function CreateNewTaskCreatingObject(AViewInfo
      : TdxGanttControlViewChartTaskViewInfo): TdxChartNewTaskCreatingObject;
    function CreateResizingObject(AViewInfo
      : TdxGanttControlViewChartTaskViewInfo): TdxChartTaskResizingObject;
  protected
    function CreateDragAndDropObject: TdxGanttControlDragAndDropObject;
      override;
    function StartDragAndDrop(const P: TPoint): Boolean; override;

    function CanScroll: Boolean; override;
    function CreateDragAndDropObjectByPoint(const P: TPoint)
      : TdxGanttControlDragAndDropObject;
    procedure ConvertToLinkingObject(AMovingObject: TdxChartTaskMovingObject);
    procedure DoScroll; override;
    function GetScrollableArea: TRect; override;
  public
    property Controller: TdxGanttControlChartViewAreaController
      read GetController;
  end;

  { TdxChartViewAreaScrollBars }

  TdxChartViewAreaScrollBars = class(TdxGanttControlCustomScrollBars)
  // for internal use
  strict private
    function GetController: TdxGanttControlChartViewAreaController; inline;
  protected
    FHPosition: Integer;

    function NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean;
      override;
    procedure DoInitHScrollBarParameters; override;
    procedure DoInitVScrollBarParameters; override;
    procedure DoHScroll(ScrollCode: TScrollCode;
      var ScrollPos: Integer); override;
    procedure DoVScroll(ScrollCode: TScrollCode;
      var ScrollPos: Integer); override;
    function IsUnlimitedScrolling(AScrollKind: TScrollBarKind;
      ADeltaX, ADeltaY: Integer): Boolean; override;
  public
    constructor Create(AController: TdxGanttControlCustomController); override;
    property Controller: TdxGanttControlChartViewAreaController
      read GetController;
  end;

  { TdxGanttControlChartViewAreaController }

  TdxGanttControlChartViewAreaController = class
    (TdxGanttControlCustomController)
  strict private
    FFirstVisibleDateTime: TDateTime;
    FScrollBars: TdxChartViewAreaScrollBars;
    FParentController: TdxGanttControlChartViewController;
    FMinorHeaderWidth: TdxGanttControlChartViewMinorHeaderWidth;

    function GetDataProvider: TdxGanttControlChartViewDataProvider; inline;
    function InternalGetViewInfo: TdxGanttControlViewChartAreaViewInfo; inline;
    procedure SetFirstVisibleDateTime(const Value: TDateTime);
    procedure SetMinorHeaderWidth(const Value
      : TdxGanttControlChartViewMinorHeaderWidth);
  protected
    function CanAutoScroll(ADirection: TcxDirection): Boolean; override;
    function CreateDragHelper: TdxGanttControlDragHelper; override;
    function GetGestureClient(const APoint: TPoint): IdxGestureClient; override;
    function GetLastVisibleDateTime: TDateTime;
    function GetViewInfo: TdxGanttControlCustomItemViewInfo; override;
    function IsPanArea(const APoint: TPoint): Boolean; override;

    procedure DoCreateScrollBars; override;
    procedure DoDestroyScrollBars; override;
    function GetTouchScrollUIOwner(const APoint: TPoint)
      : IdxTouchScrollUIOwner; override;
    procedure InitScrollbars; override;
    procedure UnInitScrollbars; override;

    procedure DoClick; override;
    procedure DoDblClick; override;
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure DoMouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; AIsIncrement: Boolean;
      const AMousePos: TPoint): Boolean; override;
    procedure SetFocusedRow(ATask: TdxGanttControlViewChartTaskViewInfo);

    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;

    procedure MakeFocusedRowVisible;
    function ProcessNCSizeChanged: Boolean; override;

    property DataProvider: TdxGanttControlChartViewDataProvider
      read GetDataProvider;
    property MinorHeaderWidth: TdxGanttControlChartViewMinorHeaderWidth
      read FMinorHeaderWidth write SetMinorHeaderWidth;
    property ScrollBars: TdxChartViewAreaScrollBars read FScrollBars;
    property ViewInfo: TdxGanttControlViewChartAreaViewInfo
      read InternalGetViewInfo;
  public
    constructor Create(AParentController: TdxGanttControlChartViewController);
      reintroduce;
    destructor Destroy; override;

    property FirstVisibleDateTime: TDateTime read FFirstVisibleDateTime
      write SetFirstVisibleDateTime;
    property ParentController: TdxGanttControlChartViewController
      read FParentController;
  end;

  { TdxChartViewSplitterController }

  TdxChartViewSplitterController = class(TdxGanttControlSplitterController)
  // for internal use
  strict private
    FParentController: TdxGanttControlChartViewController;
  protected
    procedure ApplySize(const P: TPoint); override;
    function CalculateDragPoint(const P: TPoint): TPoint; override;
    function GetViewInfo: TdxGanttControlCustomItemViewInfo; override;
  public
    constructor Create(AParentController: TdxGanttControlChartViewController);
      reintroduce;

    property ParentController: TdxGanttControlChartViewController
      read FParentController;
  end;

  { TdxGanttControlChartViewController }

  TdxGanttControlChartViewController = class
    (TdxGanttControlViewCustomController)
  strict private
    FChartAreaController: TdxGanttControlChartViewAreaController;
    FSheetController: TdxGanttControlChartViewSheetController;
    FSplitterController: TdxChartViewSplitterController;
    function GetDataProvider: TdxGanttControlChartViewDataProvider; inline;
    function GetFirstVisibleRowIndex: Integer;
    function GetFocusedRowIndex: Integer;
    function GetView: TdxGanttControlChartView; inline;
    function InternalGetViewInfo: TdxGanttControlViewChartViewInfo; inline;
    procedure SetFirstVisibleRowIndex(const Value: Integer);
    procedure SetFocusedRowIndex(const Value: Integer);
  protected
    function CreateChartAreaController
      : TdxGanttControlChartViewAreaController; virtual;
    function CreateSheetController
      : TdxGanttControlChartViewSheetController; virtual;
    function CreateSplitterController: TdxChartViewSplitterController; virtual;
    procedure HideEditing; override;

    function GetActiveController(const P: TPoint)
      : TdxGanttControlCustomController;
    function GetControllerByCursorPos(const P: TPoint)
      : TdxGanttControlCustomController; override;

    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoKeyPress(var Key: Char); override;
    procedure DoKeyUp(var Key: Word; Shift: TShiftState); override;

    procedure Activated; override;
    procedure Deactivated; override;
    procedure DoCreateScrollBars; override;
    procedure DoDestroyScrollBars; override;
    procedure InitScrollbars; override;
    function IsMouseWheelHandleNeeded(const MousePos: TPoint): Boolean;
      override;
    function ProcessNCSizeChanged: Boolean; override;
    procedure UnInitScrollbars; override;

    function InitializeBuiltInPopupMenu(APopupMenu: TdxGanttControlPopupMenu;
      var P: TPoint): Boolean; override;

    property DataProvider: TdxGanttControlChartViewDataProvider
      read GetDataProvider;
    property SplitterController: TdxChartViewSplitterController
      read FSplitterController;

    property ViewInfo: TdxGanttControlViewChartViewInfo
      read InternalGetViewInfo;
  public
    constructor Create(AView: TdxGanttControlCustomView); override;
    destructor Destroy; override;

    property ChartAreaController: TdxGanttControlChartViewAreaController
      read FChartAreaController;
    property SheetController: TdxGanttControlChartViewSheetController
      read FSheetController;

    property FirstVisibleRowIndex: Integer read GetFirstVisibleRowIndex
      write SetFirstVisibleRowIndex;
    property FocusedRowIndex: Integer read GetFocusedRowIndex
      write SetFocusedRowIndex;
    property View: TdxGanttControlChartView read GetView;
  end;

  { TdxGanttControlChartViewDataProvider }

  TdxGanttControlChartViewDataProvider = class
    (TdxGanttControlSheetCustomDataProvider)
  strict private
    FDataModel: TdxGanttControlDataModel;
    FTaskDictionary: TDictionary<Integer, TdxGanttControlTask>;

    procedure InitializeNewTask(ATask: TdxGanttControlTask);
    procedure TasksBeforeResetHandler(Sender: TObject);
    procedure TasksChangedHandler(Sender: TObject;
      const AItem: TdxGanttControlModelElementListItem;
      AAction: TCollectionNotification);

    function InternalGetDataItem(Index: Integer): TdxGanttControlTask; inline;
    function GetTask(AIndex: Integer): TdxGanttControlTask; inline;
  protected
    function CanExpand(AItem: TObject): Boolean; override;
    function CanCollapse(AItem: TObject): Boolean; override;

    procedure InternalAppendItem; override;
    procedure InternalInsertNewItem(AIndex: Integer); override;
    procedure InternalInsertItem(AIndex: Integer; ADataItem: TObject); override;
    procedure InternalExtractItem(AIndex: Integer); override;
    procedure InternalExtractLastItem; override;

    procedure ClearItems; override;
    procedure ItemRemoved(AItem: TObject); override;

    function CanAddItem(AItem: TObject): Boolean; override;
    function GetDataItemCount: Integer; override;
    function GetDataItem(Index: Integer): TObject; override;
    function GetDataItemIndex(ADataItem: TObject): Integer; override;
    function GetRowHeaderCaption(AData: TObject): string; override;
    procedure DoPopulate; override;

    property DataItems[Index: Integer]: TdxGanttControlTask
      read InternalGetDataItem;
  public
    constructor Create(AControl: TdxGanttControlBase); override;
    destructor Destroy; override;

    function GetTaskByUID(const AUID: Integer): TdxGanttControlTask;

    property DataModel: TdxGanttControlDataModel read FDataModel;
    property Tasks[AIndex: Integer]: TdxGanttControlTask read GetTask; default;
  end;

  { TdxGanttControlChartViewSheetOptions }

  TdxGanttControlChartViewSheetOptions = class(TdxGanttControlSheetOptions)
  public const
    DefaultWidth = 200;
  strict private
    FHidden: Boolean;
    FVisible: Boolean;
    FWidth: Integer;
    FOnSizeChanged: TNotifyEvent;
    function InternalGetController
      : TdxGanttControlChartViewSheetController; inline;
    function InternalGetOwner: TdxGanttControlChartView; inline;
    procedure SetHidden(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    procedure SetWidth(const Value: Integer);
  protected
    function CreateColumns: TdxGanttControlSheetColumns; override;
    procedure DoReset; override;
    function GetController: TdxGanttControlSheetController; override;
    function GetDataProvider: TdxGanttControlSheetCustomDataProvider; override;
    function GetOwnerComponent: TComponent; override;
    function GetScaleFactor: TdxScaleFactor; override;

    procedure DoSizeChanged; virtual;
    property Hidden: Boolean read FHidden write SetHidden default False;
  public
    procedure Assign(Source: TPersistent); override;

    property Controller: TdxGanttControlChartViewSheetController
      read InternalGetController;
    property Owner: TdxGanttControlChartView read InternalGetOwner;
  published
    property CellAutoHeight;
    property Columns;
    property RowHeight;
    property Width: Integer read FWidth write SetWidth default DefaultWidth;
    property Visible: Boolean read FVisible write SetVisible default True;
    property OnSizeChanged: TNotifyEvent read FOnSizeChanged
      write FOnSizeChanged;
  end;

  { TdxGanttControlGridline }

  TdxGanttControlGridline = class(TdxGanttControlPersistent)
  strict private
    FColor: TColor;
    FOwner: TdxGanttControlGridlineOptions;
    FStyle: TPenStyle;
    procedure SetColor(const Value: TColor);
    procedure SetStyle(const Value: TPenStyle);
  protected
    procedure DoChanged; override;
    procedure DoReset; override;
  public
    constructor Create(AOwner: TdxGanttControlGridlineOptions);
    procedure Assign(Source: TPersistent); override;

    function IsVisible: Boolean;
  published
    property Color: TColor read FColor write SetColor default clDefault;
    property Style: TPenStyle read FStyle write SetStyle
      default TPenStyle.psSolid;
  end;

  { TdxGanttControlGridlineOptions }

  TdxGanttControlGridlineOptions = class(TdxGanttControlPersistent)
  strict private
    FCurrentDate: TdxGanttControlGridline;
    FOwner: TdxGanttControlChartView;
    FProjectFinish: TdxGanttControlGridline;
    FProjectStart: TdxGanttControlGridline;
  private
    procedure SetCurrentDate(const Value: TdxGanttControlGridline);
    procedure SetProjectFinish(const Value: TdxGanttControlGridline);
    procedure SetProjectStart(const Value: TdxGanttControlGridline);
  protected
    procedure DoReset; override;
    procedure DoChanged; override;
  public
    constructor Create(AOwner: TdxGanttControlChartView);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property CurrentDate: TdxGanttControlGridline read FCurrentDate
      write SetCurrentDate;
    property ProjectFinish: TdxGanttControlGridline read FProjectFinish
      write SetProjectFinish;
    property ProjectStart: TdxGanttControlGridline read FProjectStart
      write SetProjectStart;
  end;

  { TdxGanttControlChartView }

  TdxGanttControlChartView = class(TdxGanttControlCustomView)
  strict private
    FOptionsGridline: TdxGanttControlGridlineOptions;
    FOptionsSheet: TdxGanttControlChartViewSheetOptions;
    FOptionsSplitter: TdxGanttControlSplitterOptions;
    FShowTaskProgress: Boolean;
    FTimescaleUnit: TdxGanttControlChartViewTimescaleUnit;

    FOnFirstVisibleDateTimeChanged: TNotifyEvent;
    FOnTimescaleChanged: TNotifyEvent;

    procedure OptionsChangedHandler(Sender: TObject;
      AChanges: TdxGanttControlOptionsChangedTypes);

    function GetController: TdxGanttControlChartViewController; inline;
    function GetDataProvider: TdxGanttControlCustomDataProvider; inline;
    function GetFirstVisibleDateTime: TDateTime;
    procedure SetOptionsGridline(const Value: TdxGanttControlGridlineOptions);
    procedure SetFirstVisibleDateTime(const Value: TDateTime);
    procedure SetOptionsSheet(const Value
      : TdxGanttControlChartViewSheetOptions);
    procedure SetOptionsSplitter(const Value: TdxGanttControlSplitterOptions);
    procedure SetShowTaskProgress(const Value: Boolean);
    procedure SetTimescaleUnit(const Value
      : TdxGanttControlChartViewTimescaleUnit);
  protected
    function CreateController: TdxGanttControlViewCustomController; override;
    function CreateDataProvider: TdxGanttControlCustomDataProvider; override;
    function CreateViewInfo: TdxGanttControlViewCustomViewInfo; override;
    function GetLastVisibleDateTime: TDateTime;
    function GetType: TdxGanttControlViewType; override;
    procedure DoReset; override;

    procedure DoFirstVisibleDateTimeChanged; virtual;
    procedure DoTimescaleChanged; virtual;

    property OptionsSplitter: TdxGanttControlSplitterOptions
      read FOptionsSplitter write SetOptionsSplitter;
    property OnTimescaleChanged: TNotifyEvent read FOnTimescaleChanged
      write FOnTimescaleChanged;
  public
    constructor Create(AOwner: TPersistent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property Controller: TdxGanttControlChartViewController read GetController;
    property DataProvider: TdxGanttControlCustomDataProvider
      read GetDataProvider;
    property FirstVisibleDateTime: TDateTime read GetFirstVisibleDateTime
      write SetFirstVisibleDateTime;
  published
    property OptionsGridline: TdxGanttControlGridlineOptions
      read FOptionsGridline write SetOptionsGridline;
    property OptionsSheet: TdxGanttControlChartViewSheetOptions
      read FOptionsSheet write SetOptionsSheet;
    property ShowTaskProgress: Boolean read FShowTaskProgress
      write SetShowTaskProgress default True;
    property TimescaleUnit: TdxGanttControlChartViewTimescaleUnit
      read FTimescaleUnit write SetTimescaleUnit
      default TdxGanttControlChartViewTimescaleUnit.Days;
    property OnFirstVisibleDateTimeChanged: TNotifyEvent
      read FOnFirstVisibleDateTimeChanged write FOnFirstVisibleDateTimeChanged;
  end;

implementation

uses
  Math, DateUtils, Variants, RTLConsts,
  dxTypeHelpers, cxDrawTextUtils, cxDateUtils, dxCultureInfo, dxHash,
  dxGanttControl,
  dxGanttControlViewCommands,
  dxGanttControlViewChartSheetColumns,
  dxGanttControlTaskCommands,
  dxGanttControlStrs,
  dxGanttControlUtils,
  dxGanttControlCursors,
  dxGanttControlTaskDependencyDialog,
  dxGanttControlTaskInformationDialog;

type
  TcxCustomDragImageAccess = class(TcxCustomDragImage);
  TdxCustomGanttControlAccess = class(TdxCustomGanttControl);
  TdxGanttControlChartViewAccess = class(TdxGanttControlChartView);
  TdxGanttControlHitTestAccess = class(TdxGanttControlHitTest);
  TdxGanttControlElementCustomListAccess = class
    (TdxGanttControlModelElementList);
  TdxGanttControlCustomItemViewInfoAccess = class
    (TdxGanttControlCustomItemViewInfo);
  TdxGanttControlTaskPredecessorLinkAccess = class
    (TdxGanttControlTaskPredecessorLink);

  { TdxGanttControlTaskOutlineLevelChangingObject }

  TdxGanttControlTaskOutlineLevelChangingObject = class
    (TdxGanttControlResizingObject)
  strict private
    FPoints: TList<Integer>;
    FStartX: Integer;
    FTask: TdxGanttControlTask;
    FMaxValue: Integer;

    procedure CalculateMaxValue;
    function PointToValue(X: Integer): Integer;
    procedure PopulatePoints;
    function GetController: TdxGanttControlChartViewSheetController; inline;
  protected
    procedure ApplyChanges(const P: TPoint); override;
    function CanDrop(const P: TPoint): Boolean; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    function GetDragImageHeight: Integer; override;
    function GetDragImageWidth: Integer; override;
  public
    constructor Create(AController: TdxGanttControlChartViewSheetController;
      AViewInfo: TdxGanttControlViewChartSheetColumnTaskNameViewInfo);
      reintroduce;
    destructor Destroy; override;
    property Controller: TdxGanttControlChartViewSheetController
      read GetController;
  end;

  { TdxGanttControlTaskOutlineLevelChangingObject }

constructor TdxGanttControlTaskOutlineLevelChangingObject.Create
  (AController: TdxGanttControlChartViewSheetController;
  AViewInfo: TdxGanttControlViewChartSheetColumnTaskNameViewInfo);
begin
  inherited Create(AController);
  FTask := TdxGanttControlTask(AViewInfo.Owner.Data);
  FPoints := TList<Integer>.Create;
  if AViewInfo.UseRightToLeftAlignment then
    FStartX := AViewInfo.Bounds.Right
  else
    FStartX := AViewInfo.Bounds.Left;
  CalculateMaxValue;
  PopulatePoints;
end;

destructor TdxGanttControlTaskOutlineLevelChangingObject.Destroy;
begin
  FreeAndNil(FPoints);
  inherited Destroy;
end;

procedure TdxGanttControlTaskOutlineLevelChangingObject.ApplyChanges
  (const P: TPoint);
var
  AValue: Integer;
  ACommand: TdxGanttControlCommand;
begin
  AValue := PointToValue(P.X);
  if AValue <> FTask.OutlineLevel then
  begin
    Controller.Control.BeginUpdate;
    try
      while FTask.OutlineLevel <> AValue do
      begin
        if FTask.OutlineLevel > AValue then
          ACommand := TdxGanttControlDecreaseTaskOutlineLevelCommand.Create
            (Controller.Control, FTask)
        else
          ACommand := TdxGanttControlIncreaseTaskOutlineLevelCommand.Create
            (Controller.Control, FTask);
        try
          ACommand.Execute;
        finally
          ACommand.Free;
        end;
      end;
    finally
      Controller.Control.EndUpdate;
    end;
  end;
end;

function TdxGanttControlTaskOutlineLevelChangingObject.CanDrop
  (const P: TPoint): Boolean;
begin
  Result := True;
end;

procedure TdxGanttControlTaskOutlineLevelChangingObject.DragAndDrop
  (const P: TPoint; var Accepted: Boolean);
var
  AValue: Integer;
  APos: TPoint;
begin
  Accepted := True;
  APos.Y := Controller.ViewInfo.ClientRect.Top +
    Controller.ViewInfo.ColumnHeaderHeight;
  AValue := PointToValue(P.X);
  APos.X := FPoints[AValue - 1];
  ShowDragImage(APos);
end;

function TdxGanttControlTaskOutlineLevelChangingObject.
  GetDragImageHeight: Integer;
begin
  Result := Controller.ViewInfo.ClientRect.Height -
    Controller.ViewInfo.ColumnHeaderHeight;
end;

function TdxGanttControlTaskOutlineLevelChangingObject.
  GetDragImageWidth: Integer;
begin
  Result := 1;
end;

procedure TdxGanttControlTaskOutlineLevelChangingObject.CalculateMaxValue;
var
  I: Integer;
begin
  FMaxValue := FTask.OutlineLevel;
  if FTask.ID > 0 then
    for I := FTask.ID - 1 downto 0 do
      if not FTask.Owner[I].Blank then
      begin
        FMaxValue := FTask.Owner[I].OutlineLevel + 1;
        Break;
      end;
end;

function TdxGanttControlTaskOutlineLevelChangingObject.PointToValue
  (X: Integer): Integer;
var
  I: Integer;
begin
  if (X <= FPoints[0]) xor Controller.ViewInfo.UseRightToLeftAlignment then
    Exit(1);
  if (X >= FPoints.Last) xor Controller.ViewInfo.UseRightToLeftAlignment then
    Exit(FPoints.Count);
  for I := 1 to FPoints.Count - 1 do
  begin
    if (X < FPoints[I]) xor Controller.ViewInfo.UseRightToLeftAlignment then
    begin
      if Abs(X - FPoints[I - 1]) > Abs(X - FPoints[I]) then
        Exit(I + 1)
      else
        Exit(I)
    end;
  end;
  Result := 1;
end;

procedure TdxGanttControlTaskOutlineLevelChangingObject.PopulatePoints;
var
  I: Integer;
  X: Integer;
  ADelta: Integer;
begin
  FPoints.Clear;
  ADelta := TdxGanttControlViewChartSheetColumnTaskNameViewInfo.
    CalculateOutlineLevelOffset(Controller.ScaleFactor, 1);
  X := FStartX;
  for I := 1 to FMaxValue do
  begin
    if Controller.ViewInfo.UseRightToLeftAlignment then
      Dec(X, ADelta)
    else
      Inc(X, ADelta);
    FPoints.Add(X);
  end;
end;

function TdxGanttControlTaskOutlineLevelChangingObject.GetController
  : TdxGanttControlChartViewSheetController;
begin
  Result := TdxGanttControlChartViewSheetController(inherited Controller);
end;

{ TdxChartTaskDragImage }

constructor TdxChartTaskDragImage.Create(AWidth: Integer; AColor: TColor);
begin
  inherited Create;
  FColor := AColor;
  FWidth := AWidth;
  TransparentColor := True;
  TransparentColorValue := clWhite;
end;

procedure TdxChartTaskDragImage.DoUpdateImage(AImage: TcxAlphaBitmap);
begin
  AImage.cxCanvas.FillRect(AImage.ClientRect, TransparentColorValue);
  AImage.cxCanvas.FrameRect(AImage.ClientRect, FColor, FWidth);
end;

{ TdxChartTaskChangeProgressObject }

procedure TdxChartTaskChangeProgressObject.ApplyChanges(const P: TPoint);
begin
  UpdateTimeBounds(P);
  with TdxGanttControlChangeTaskPercentCompleteCommand.Create(Control, Task,
    FValue) do
    try
      Execute;
    finally
      Free;
    end;
end;

procedure TdxChartTaskChangeProgressObject.BeginDragAndDrop;
begin
  inherited BeginDragAndDrop;
  FValue := Task.PercentComplete;
end;

function TdxChartTaskChangeProgressObject.CanScroll
  (ADirection: TcxDirection): Boolean;
begin
  if ADirection = dirLeft then
    Result := Task.Start < Controller.ViewInfo.HeaderViewInfo.MinorHeaders.
      First.DateTime
  else if ADirection = dirRight then
    Result := Task.Finish > Controller.ViewInfo.HeaderViewInfo.MinorHeaders.
      Last.DateTime
  else
    Result := False;
end;

function TdxChartTaskChangeProgressObject.CreateDragImage: TcxDragImage;
begin
  Result := TdxChartTaskProgressImage.Create(Self);
end;

function TdxChartTaskChangeProgressObject.GetDragAndDropCursor
  (Accepted: Boolean): TCursor;
begin
  if Accepted then
    Result := TdxGanttControlCursors.TaskComplete
  else
    Result := inherited GetDragAndDropCursor(Accepted);
end;

function TdxChartTaskChangeProgressObject.GetDragImage
  : TdxChartTaskProgressImage;
begin
  Result := TdxChartTaskProgressImage(inherited DragImage);
end;

function TdxChartTaskChangeProgressObject.GetScrollableArea: TRect;
begin
  Result := TaskViewInfoBounds;
end;

procedure TdxChartTaskChangeProgressObject.UpdateTimeBounds(P: TPoint);
const
  ACompleteMap: array [Boolean] of Integer = (0, 100);
var
  ADateTime: TDateTime;
  S: Integer;
  ADuration: TdxGanttControlDuration;
begin
  if P.X <= TaskViewInfo.BarBounds.Left then
    FValue := ACompleteMap[TaskViewInfo.UseRightToLeftAlignment]
  else if P.X >= TaskViewInfo.BarBounds.Right then
    FValue := ACompleteMap[not TaskViewInfo.UseRightToLeftAlignment]
  else
  begin
    ADateTime := Controller.ViewInfo.HeaderViewInfo.GetDateTimeByPos(P.X);
    if not GetCalendar.IsWorkTime(ADateTime - OneMinute) then
      ADateTime := GetCalendar.GetPreviousWorkTime(ADateTime);
    ADuration := TdxGanttControlDuration.Create(Task.RealDuration);
    S := ADuration.ToSeconds;
    ADuration := TdxGanttControlDuration.Create(Task.Start, ADateTime,
      GetCalendar, Task.RealDurationFormat);
    FValue := MulDiv(ADuration.ToSeconds, 100, S);
  end;
  DragImage.Value := FValue;
end;

{ TdxChartViewSheetScrollBars }

function TdxChartViewSheetScrollBars.CanScrolling
  (AKind: TScrollBarKind): Boolean;
begin
  Result := (AKind = sbVertical) or inherited CanScrolling(AKind);
end;

procedure TdxChartViewSheetScrollBars.DoInitVScrollBarParameters;
begin
  inherited DoInitVScrollBarParameters;
  if GetScrollbarMode = sbmClassic then
    VScrollBar.Data.Visible := False
end;

function TdxChartViewSheetScrollBars.GetMouseWheelActiveScrollBar
  (Shift: TShiftState): IcxControlScrollBar;
begin
  if Shift * [ssShift] = [] then
    Result := VScrollBar
  else
    Result := inherited GetMouseWheelActiveScrollBar(Shift);
end;

{ TdxGanttControlViewChartViewInfo }

procedure TdxGanttControlViewChartViewInfo.CalculateLayout;
begin
  Clear;
  if View.OptionsSheet.Visible then
  begin
    FSheetViewInfo := CreateSheetViewInfo;
    AddChild(FSheetViewInfo);
    if TdxGanttControlChartViewAccess(View).OptionsSplitter.Visible then
    begin
      FSplitterViewInfo := TdxGanttControlSplitterViewInfo.Create(Self,
        TdxGanttControlChartViewAccess(View).OptionsSplitter);
      AddChild(FSplitterViewInfo);
    end;
  end;
  FChartAreaViewInfo := TdxGanttControlViewChartAreaViewInfo.Create(Self);
  AddChild(FChartAreaViewInfo);
  inherited CalculateLayout;
end;

function TdxGanttControlViewChartViewInfo.CreateSheetViewInfo
  : TdxGanttControlViewChartSheetViewInfo;
begin
  Result := TdxGanttControlViewChartSheetViewInfo.Create(Self,
    View.OptionsSheet);
end;

function TdxGanttControlViewChartViewInfo.CalculateChartAreaBounds: TRect;
var
  AWidth: Integer;
begin
  Result := Bounds;
  AWidth := CalculateSheetBounds.Width + CalculateSplitterBounds.Width;
  if UseRightToLeftAlignment then
    Result.Right := Result.Right - AWidth
  else
    Result.Left := Result.Left + AWidth;
end;

function TdxGanttControlViewChartViewInfo.CalculateSheetBounds: TRect;
var
  AWidth: Integer;
begin
  if View.OptionsSheet.Visible and not View.OptionsSheet.Hidden then
  begin
    AWidth := ScaleFactor.Apply(View.OptionsSheet.Width);
    AWidth := Min(AWidth, GetMaxSheetWidth);
    AWidth := Max(AWidth, ScaleFactor.Apply(TdxGanttControlChartViewAccess(View)
      .OptionsSplitter.MinSize));
  end
  else
    AWidth := 0;
  Result := Bounds;
  if UseRightToLeftAlignment then
    Result.Left := Result.Right - AWidth
  else
    Result.Right := Result.Left + AWidth;
end;

function TdxGanttControlViewChartViewInfo.CalculateSplitterBounds: TRect;
begin
  Result := CalculateSheetBounds;
  if Result.Width = 0 then
    Exit(cxNullRect);
  if UseRightToLeftAlignment then
  begin
    Result.Right := Result.Left;
    Result.Left := Result.Left - TdxGanttControlChartViewAccess(View)
      .OptionsSplitter.Width;
  end
  else
  begin
    Result.Left := Result.Right;
    Result.Right := Result.Right + TdxGanttControlChartViewAccess(View)
      .OptionsSplitter.Width;
  end;
end;

procedure TdxGanttControlViewChartViewInfo.Clear;
begin
  FSheetViewInfo := nil;
  FChartAreaViewInfo := nil;
  FSplitterViewInfo := nil;
  inherited Clear;
end;

function TdxGanttControlViewChartViewInfo.GetMaxSheetWidth: Integer;
var
  AOptionsSplitter: TdxGanttControlSplitterOptions;
begin
  Result := Bounds.Width - Controller.ChartAreaController.ScrollBars.
    GetVScrollBarAreaWidth;
  AOptionsSplitter := TdxGanttControlChartViewAccess(View).OptionsSplitter;
  if AOptionsSplitter.Visible then
    Dec(Result, AOptionsSplitter.Width);
end;

function TdxGanttControlViewChartViewInfo.CalculateItemBounds
  (AItem: TdxGanttControlCustomItemViewInfo): TRect;
begin
  if AItem is TdxGanttControlViewChartSheetViewInfo then
    Result := CalculateSheetBounds
  else if AItem is TdxGanttControlSplitterViewInfo then
    Result := CalculateSplitterBounds
  else if AItem is TdxGanttControlViewChartAreaViewInfo then
    Result := CalculateChartAreaBounds
  else
    Result := cxNullRect;
end;

function TdxGanttControlViewChartViewInfo.GetHeaderHeight: Integer;
begin
  Result := dxGetTouchableSize
    (MulDiv(TdxGanttControlChartViewSheetOptions.ColumnHeaderMinHeight,
    Abs(CanvasCache.GetBaseFont.Height), 11), ScaleFactor);
end;

function TdxGanttControlViewChartViewInfo.GetController
  : TdxGanttControlChartViewController;
begin
  Result := TdxGanttControlChartViewController(inherited Controller);
end;

function TdxGanttControlViewChartViewInfo.GetTotalRowCount: Integer;
begin
  if FSheetViewInfo <> nil then
    Result := FSheetViewInfo.DataRows.Count + Controller.FirstVisibleRowIndex
  else
    Result := GetVisibleRowCount + Controller.FirstVisibleRowIndex;
  Result := Max(Result, DataProvider.Count);
end;

function TdxGanttControlViewChartViewInfo.GetView: TdxGanttControlChartView;
begin
  Result := TdxGanttControlChartView(inherited View);
end;

function TdxGanttControlViewChartViewInfo.GetVisibleRowCount: Integer;
begin
  if FSheetViewInfo <> nil then
    Result := FSheetViewInfo.VisibleRowCount
  else
    Result := Trunc((Bounds.Height - GetHeaderHeight) /
      MulDiv(View.OptionsSheet.RowHeight,
      Abs(CanvasCache.GetBaseFont.Height), 11));
end;

{ TdxChartTaskChangeTimeBoundObject }

function TdxChartTaskChangeTimeBoundObject.CanScroll
  (ADirection: TcxDirection): Boolean;
begin
  Result := ADirection = dirRight;
  if not Result then
  begin
    if FStart < Controller.ViewInfo.HeaderViewInfo.MinorHeaders[0].DateTime then
      Result := ADirection = dirLeft;
  end;
end;

function TdxChartTaskChangeTimeBoundObject.CreateDragImage: TcxDragImage;
var
  AColor: TColor;
begin
  AColor := TaskViewInfo.GetColor;
  if AColor = clDefault then
    AColor := TaskViewInfo.LookAndFeelPainter.DefaultSelectionColor;
  Result := TdxChartTaskDragImage.Create
    (TaskViewInfo.ScaleFactor.Apply(TdxChartTaskDragImage.LineWidth), AColor);
end;

function TdxChartTaskChangeTimeBoundObject.GetScrollableArea: TRect;
begin
  Result := TaskViewInfoBounds;
end;

{ TdxChartTaskDragObject }

constructor TdxChartTaskDragObject.Create(AController
  : TdxGanttControlChartViewAreaController;
  ATaskViewInfo: TdxGanttControlViewChartTaskViewInfo);
begin
  inherited Create(AController);
  FTask := ATaskViewInfo.Task;
  FTaskViewInfo := ATaskViewInfo;
  FTaskViewInfoBounds := FTaskViewInfo.Bounds;
end;

procedure TdxChartTaskDragObject.DragAndDrop(const P: TPoint;
  var Accepted: Boolean);
begin
  Accepted := CanDrop(P);
  if Accepted then
  begin
    UpdateTimeBounds(P);
    UpdateDragImage;
  end
  else
    HideDragImage;
  inherited DragAndDrop(P, Accepted);
  CheckScrolling(P);
end;

procedure TdxChartTaskDragObject.AfterScrolling(AScrollDirection: TcxDirection);
begin
  inherited AfterScrolling(AScrollDirection);
  FTaskViewInfo := nil;
end;

procedure TdxChartTaskDragObject.BeginDragAndDrop;
begin
  inherited BeginDragAndDrop;
  if Task <> nil then
  begin
    FStart := Task.Start;
    FFinish := Task.Finish;
  end;
end;

function TdxChartTaskDragObject.CanDrop(const P: TPoint): Boolean;
begin
  Result := FTaskViewInfoBounds.Contains(P);
end;

function TdxChartTaskDragObject.CanScroll(ADirection: TcxDirection): Boolean;
begin
  Result := True;
end;

function TdxChartTaskDragObject.GetCalendar: TdxGanttControlCalendar;
begin
  if Task <> nil then
    Result := Task.RealCalendar
  else
    Result := Controller.DataProvider.DataModel.ActiveCalendar;
end;

function TdxChartTaskDragObject.GetController
  : TdxGanttControlChartViewAreaController;
begin
  Result := TdxGanttControlChartViewAreaController(inherited Controller);
end;

function TdxChartTaskDragObject.GetDragImage: TdxChartTaskDragCustomImage;
begin
  Result := TdxChartTaskDragCustomImage(inherited DragImage);
end;

function TdxChartTaskDragObject.GetScrollableArea: TRect;
begin
  Result := Controller.ViewInfo.ClientRect;
end;

function TdxChartTaskDragObject.GetTaskViewInfo
  : TdxGanttControlViewChartTaskViewInfo;
var
  I: Integer;
begin
  for I := 0 to Controller.ViewInfo.Tasks.Count - 1 do
    if Controller.ViewInfo.Tasks[I].Task = Task then
      Exit(Controller.ViewInfo.Tasks[I]);
  Result := nil;
end;

function TdxChartTaskDragObject.InternalGetTaskViewInfo
  : TdxGanttControlViewChartTaskViewInfo;
begin
  if FTaskViewInfo = nil then
    FTaskViewInfo := GetTaskViewInfo;
  Result := FTaskViewInfo;
end;

procedure TdxChartTaskDragObject.UpdateDragImage;
var
  ATaskViewInfo: TdxGanttControlViewChartTaskViewInfo;
  R: TRect;
  P: TPoint;
begin
  ATaskViewInfo := TaskViewInfo;
  if ATaskViewInfo <> nil then
    R := ATaskViewInfo.DoCalculateBarBounds(FStart, FFinish)
  else
    Exit;
  DragImage.SetImageSize(R.Size);
  P := TPoint.Create(R.Left, 0);
  P.Offset(-ATaskViewInfo.Owner.Bounds.Left, 0);
  if P.X < 0 then
    DragImage.SetImageOffset(P)
  else
    DragImage.SetImageOffset(TPoint.Null);
  R.Intersect(ATaskViewInfo.Owner.ClientRect);
  DragImage.Width := R.Width;
  DragImage.Height := R.Height;
  ShowDragImage(R.TopLeft);
end;

{ TdxGanttControlChartViewSheetController }

constructor TdxGanttControlChartViewSheetController.Create(AParentController
  : TdxGanttControlChartViewController);
begin
  inherited Create(AParentController.Control,
    AParentController.View.OptionsSheet);
  FParentController := AParentController;
  TdxCustomGanttControl(Control).DataModel.TaskChangedHandlers.Add
    (TaskChangedHandler);
  TdxCustomGanttControl(Control).DataModel.Tasks.ListChangedHandlers.Add
    (TaskListChangedHandler);
  TdxCustomGanttControl(Control).DataModel.Tasks.BeforeResetHandlers.Add
    (TaskListBeforeResetHandler);
end;

destructor TdxGanttControlChartViewSheetController.Destroy;
begin
  TdxCustomGanttControl(Control).DataModel.TaskChangedHandlers.Remove
    (TaskChangedHandler);
  TdxCustomGanttControl(Control).DataModel.Tasks.ListChangedHandlers.Remove
    (TaskListChangedHandler);
  TdxCustomGanttControl(Control).DataModel.Tasks.BeforeResetHandlers.Remove
    (TaskListBeforeResetHandler);
  inherited Destroy;
end;

function TdxGanttControlChartViewSheetController.CreateChangeCellValueCommand
  (const AValue: Variant): TdxGanttControlSheetChangeCellValueCommand;
begin
  Result := TdxGanttControlViewChartSheetChangeCellValueCommand.Create
    (Self, AValue);
end;

function TdxGanttControlChartViewSheetController.CreateDragHelper
  : TdxGanttControlDragHelper;
begin
  Result := TdxGanttControlChartViewSheetDragHelper.Create(Self);
end;

function TdxGanttControlChartViewSheetController.CreateScrollBars
  : TdxGanttSheetScrollBars;
begin
  Result := TdxChartViewSheetScrollBars.Create(Self);
end;

procedure TdxGanttControlChartViewSheetController.DeleteFocusedItem;
begin
  with TdxViewChartDeleteFocusedTaskCommand.Create(Self) do
    try
      Execute;
    finally
      Free;
    end;
end;

procedure TdxGanttControlChartViewSheetController.DoDblClick;
var
  ATask: TdxGanttControlTask;
begin
  inherited DoDblClick;
  if (HitTest.HitObject is TdxGanttControlSheetCellCustomViewInfo) and
    (TdxGanttControlSheetCellCustomViewInfo(HitTest.HitObject).Owner.Data = nil)
  then
    ShowTaskInformationDialog(nil)
  else if (HitTest.HitObject is TdxGanttControlSheetCellCustomViewInfo) and
    (TdxGanttControlSheetCellCustomViewInfo(HitTest.HitObject)
    .Owner.Data is TdxGanttControlTask) then
  begin
    ATask := TdxGanttControlTask(TdxGanttControlSheetCellCustomViewInfo
      (HitTest.HitObject).Owner.Data);
    ShowTaskInformationDialog(ATask);
  end;
end;

procedure TdxGanttControlChartViewSheetController.DoKeyDown(var Key: Word;
  Shift: TShiftState);
begin
  inherited DoKeyDown(Key, Shift);
  if (Shift = [ssShift, ssAlt]) and (FocusedDataItem <> nil) and
    not FocusedDataItem.Blank and (Key in [VK_LEFT, VK_RIGHT]) then
  begin
    if Key = VK_RIGHT then
      with TdxGanttControlIncreaseTaskOutlineLevelCommand.Create(Control,
        FocusedDataItem) do
        try
          Execute;
        finally
          Free;
        end
    else if Key = VK_LEFT then
      with TdxGanttControlDecreaseTaskOutlineLevelCommand.Create(Control,
        FocusedDataItem) do
        try
          Execute;
        finally
          Free;
        end;
  end;
end;

procedure TdxGanttControlChartViewSheetController.DoMouseDown
  (Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AData: TObject;
begin
  inherited DoMouseDown(Button, Shift, X, Y);
  if HitTest.HitObject is TdxGanttControlViewChartSheetExpandButtonViewInfo then
  begin
    AData := TdxGanttControlViewChartSheetExpandButtonViewInfo
      (HitTest.HitObject).Owner.Owner.Data;
    SetFocusedCell(TdxGanttControlViewChartSheetExpandButtonViewInfo
      (HitTest.HitObject).Owner);
    ToggleExpandState(AData);
  end;
end;

procedure TdxGanttControlChartViewSheetController.DoMouseMove
  (Shift: TShiftState; X, Y: Integer);
begin
  inherited DoMouseMove(Shift, X, Y);
end;

procedure TdxGanttControlChartViewSheetController.DoMouseUp
  (Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited DoMouseUp(Button, Shift, X, Y);
end;

function TdxGanttControlChartViewSheetController.GetFocusedDataItem
  : TdxGanttControlTask;
begin
  Result := TdxGanttControlTask(inherited FocusedDataItem);
end;

function TdxGanttControlChartViewSheetController.GetScrollBars
  : TdxChartViewSheetScrollBars;
begin
  Result := TdxChartViewSheetScrollBars(inherited ScrollBars);
end;

function TdxGanttControlChartViewSheetController.GetViewInfo
  : TdxGanttControlCustomItemViewInfo;
begin
  if FParentController.ViewInfo = nil then
    Result := nil
  else
    Result := FParentController.ViewInfo.SheetViewInfo;
end;

function TdxGanttControlChartViewSheetController.GetVisibleRowCount: Integer;
begin
  Result := FParentController.ViewInfo.VisibleRowCount;
end;

function TdxGanttControlChartViewSheetController.HasItemExpandState
  (AItem: TObject): Boolean;
begin
  Result := (AItem <> nil) and TdxGanttControlTask(AItem).Summary;
end;

procedure TdxGanttControlChartViewSheetController.ShowTaskInformationDialog
  (ATask: TdxGanttControlTask);
begin
  dxGanttControlTaskInformationDialog.ShowTaskInformationDialog
    (FParentController, ATask)
end;

procedure TdxGanttControlChartViewSheetController.TaskListBeforeResetHandler
  (Sender: TObject);
begin
  if (FParentController.ViewInfo = nil) or
    (FParentController.ViewInfo.SheetViewInfo = nil) then
    Exit;
  FParentController.ViewInfo.SheetViewInfo.CachedDataRowHeight.Clear;
end;

procedure TdxGanttControlChartViewSheetController.TaskChangedHandler
  (Sender: TdxGanttControlDataModel; ATask: TdxGanttControlTask);
begin
  if (FParentController.ViewInfo = nil) or
    (FParentController.ViewInfo.SheetViewInfo = nil) then
    Exit;
  if ATask <> nil then
    FParentController.ViewInfo.SheetViewInfo.CachedDataRowHeight.Remove(ATask);
end;

procedure TdxGanttControlChartViewSheetController.TaskListChangedHandler
  (Sender: TObject; const AItem: TdxGanttControlModelElementListItem;
  AAction: TCollectionNotification);
begin
  if (FParentController.ViewInfo = nil) or
    (FParentController.ViewInfo.SheetViewInfo = nil) then
    Exit;
  if AAction in [cnExtracted, cnRemoved] then
    FParentController.ViewInfo.SheetViewInfo.CachedDataRowHeight.Remove(AItem);
end;

{ TdxGanttControlChartViewSplitterOptions }

function TdxGanttControlChartViewSplitterOptions.GetScaleFactor: TdxScaleFactor;
begin
  Result := TdxCustomGanttControlAccess(TdxGanttControlChartView(Owner).Owner)
    .ScaleFactor;
end;

function TdxGanttControlChartViewSheetDragHelper.CreateDragAndDropObjectByPoint
  (const P: TPoint): TdxGanttControlDragAndDropObject;
var
  ATaskNameViewInfo: TdxGanttControlViewChartSheetColumnTaskNameViewInfo;
begin
  Result := nil;
  HitTest.Calculate(P.X, P.Y);
  if HitTest.HitObject is TdxGanttControlViewChartSheetColumnTaskNameViewInfo
  then
  begin
    ATaskNameViewInfo := TdxGanttControlViewChartSheetColumnTaskNameViewInfo
      (HitTest.HitObject);
    if ATaskNameViewInfo.IsOutlineLevelChangeZone(P) then
      Result := CreateTaskOutlineLevelChangingObject(ATaskNameViewInfo);
  end;
  if Result = nil then
    Result := inherited CreateDragAndDropObjectByPoint(P);
end;

function TdxGanttControlChartViewSheetDragHelper.
  CreateTaskOutlineLevelChangingObject(ACellViewInfo
  : TdxGanttControlSheetCellViewInfo): TdxGanttControlResizingObject;
begin
  Result := TdxGanttControlTaskOutlineLevelChangingObject.Create(Controller,
    TdxGanttControlViewChartSheetColumnTaskNameViewInfo(ACellViewInfo));
end;

function TdxGanttControlChartViewSheetDragHelper.GetController
  : TdxGanttControlChartViewSheetController;
begin
  Result := TdxGanttControlChartViewSheetController(inherited Controller);
end;

{ TdxChartTaskResizingObject }

procedure TdxChartTaskResizingObject.ApplyChanges(const P: TPoint);
begin
  UpdateTimeBounds(P);
  with TdxGanttControlChangeTaskFinishCommand.Create(Control, Task, FFinish) do
    try
      Execute;
    finally
      Free;
    end;
end;

function TdxChartTaskResizingObject.GetDragAndDropCursor
  (Accepted: Boolean): TCursor;
begin
  if Accepted then
    Result := TdxGanttControlCursors.TaskResize
  else
    Result := inherited GetDragAndDropCursor(Accepted);
end;

procedure TdxChartTaskResizingObject.UpdateTimeBounds(P: TPoint);

  function GetDateTime(X: Integer): TDateTime;
  begin
    Result := Controller.ViewInfo.HeaderViewInfo.GetDateTimeByPos(X);
    if not GetCalendar.IsWorkTime(Result - OneMinute) then
      Result := GetCalendar.GetPreviousWorkTime(Result);
  end;

var
  ADate: TDateTime;
  X: Integer;
begin
  X := P.X;
  ADate := GetDateTime(X);
  FFinish := Max(ADate, FStart);
end;

{ TdxViewChartCreateTaskCustomCommand }

constructor TdxViewChartCreateTaskCustomCommand.Create
  (AController: TdxGanttControlChartViewController; AIndex: Integer);
begin
  inherited Create(AController.Control);
  FController := AController;
  FIndex := AIndex;
end;

procedure TdxViewChartCreateTaskCustomCommand.BeforeExecute;
var
  AViewInfo: TdxGanttControlViewChartTaskViewInfo;
  AHistoryItem: TdxGanttControlCreateDataItemHistoryItem;
  Var1: Integer;
begin
  inherited BeforeExecute;
  AViewInfo := FController.ChartAreaController.ViewInfo.Tasks
    [FIndex - FController.FirstVisibleRowIndex];
  FTask := AViewInfo.Task;
  if FTask = nil then
  begin
    for Var1 :=
      FController.DataProvider.Count to FIndex do
    begin
      AHistoryItem := TdxGanttControlAppendDataItemHistoryItem.Create
        (FController.SheetController);
      Control.History.AddItem(AHistoryItem);
      AHistoryItem.Execute;
    end;
    FTask := TdxGanttControlTask(FController.DataProvider.LastDataItem);
  end;
end;

{ TdxChartTaskDragCustomImage }

constructor TdxChartTaskDragCustomImage.Create;
begin
  inherited Create;
  FImage := TcxAlphaBitmap.Create;
  FImageOffset := TPoint.Null;
end;

destructor TdxChartTaskDragCustomImage.Destroy;
begin
  FreeAndNil(FImage);
  inherited Destroy;
end;

procedure TdxChartTaskDragCustomImage.Paint;
begin
  WindowCanvas.Draw(FImageOffset.X, FImageOffset.Y, FImage);
end;

procedure TdxChartTaskDragCustomImage.SetImageOffset(const P: TPoint);
begin
  FImageOffset := P;
end;

procedure TdxChartTaskDragCustomImage.SetImageSize(const ASize: TSize);
begin
  if FImage.ClientRect.Size.IsEqual(ASize) then
    Exit;
  FImage.SetSize(ASize.cx, ASize.cy);
  UpdateImage;
end;

procedure TdxChartTaskDragCustomImage.UpdateImage;
begin
  FImage.Canvas.Lock;
  try
    FImage.Clear;
    DoUpdateImage(FImage);
  finally
    FImage.Canvas.Unlock;
  end;
end;

{ TdxChartTaskLinkingObject }

constructor TdxChartTaskLinkingObject.Create(AMovingObject
  : TdxChartTaskMovingObject);
begin
  inherited Create(AMovingObject.Controller, AMovingObject.TaskViewInfo);
  FStart.X := TaskViewInfo.BarBounds.Left + AMovingObject.StartOffset;
  if TaskViewInfo.UseRightToLeftAlignment then
    FStart.X := FStart.X + TaskViewInfo.BarBounds.Width;
  FStart.Y := (TaskViewInfo.BarBounds.Top +
    TaskViewInfo.BarBounds.Bottom) div 2;
  FClientRect := Controller.ViewInfo.ClientRect;
  FClientRect.Top := Controller.ViewInfo.HeaderViewInfo.Bounds.Bottom;
end;

procedure TdxChartTaskLinkingObject.AfterScrolling(AScrollDirection
  : TcxDirection);
var
  DX, DY: Integer;
begin
  inherited AfterScrolling(AScrollDirection);
  DX := 0;
  DY := 0;
  case AScrollDirection of
    dirLeft, dirRight:
      if FBeforeScrollingFirstVisibleDateTime <> Controller.FirstVisibleDateTime
      then
      begin
        DX := Controller.ViewInfo.HeaderViewInfo.CalculateMinorHeaderWidth *
          IfThen(Controller.DragHelper.ScrollDirection = dirLeft, 1, -1) *
          IfThen(Controller.ViewInfo.UseRightToLeftAlignment, -1, 1);
      end;
    dirUp:
      if FBeforeScrollingFirstVisibleRowIndex <> Controller.ParentController.FirstVisibleRowIndex
      then
        DY := Controller.ViewInfo.Tasks.First.Bounds.Height;
    dirDown:
      if FBeforeScrollingFirstVisibleRowIndex <> Controller.ParentController.FirstVisibleRowIndex
      then
        DY := -FBeforeScrollingFirstVisibleRowHeight;
  end;
  if DragImage.OffsetStart(DX, DY) then
    cxRedrawWindow(Control.Handle, RDW_INVALIDATE or RDW_UPDATENOW);
end;

procedure TdxChartTaskLinkingObject.ApplyChanges(const P: TPoint);
begin
  HitTest.Calculate(P.X, P.Y);
  if (HitTest.HitObject <> TaskViewInfo) and
    (HitTest.HitObject is TdxGanttControlViewChartTaskViewInfo) then
  begin
    with TdxGanttControlTaskAddPredecessorCommand.Create(Control,
      TdxGanttControlViewChartTaskViewInfo(HitTest.HitObject).Task, Task.UID) do
      try
        RaiseValidateException := True;
        Execute;
      finally
        Free;
      end;
  end;
end;

procedure TdxChartTaskLinkingObject.BeforeScrolling;
begin
  inherited BeforeScrolling;
  FBeforeScrollingFirstVisibleDateTime := Controller.FirstVisibleDateTime;
  FBeforeScrollingFirstVisibleRowIndex :=
    Controller.ParentController.FirstVisibleRowIndex;
  FBeforeScrollingFirstVisibleRowHeight :=
    Controller.ViewInfo.Tasks.First.Bounds.Height;
end;

function TdxChartTaskLinkingObject.CanDrop(const P: TPoint): Boolean;
begin
  Result := True;
end;

function TdxChartTaskLinkingObject.CanScroll(ADirection: TcxDirection): Boolean;
begin
  Result := True;
end;

function TdxChartTaskLinkingObject.CreateDragImage: TcxDragImage;
begin
  Result := TdxChartTaskLinkingImage.Create(FClientRect,
    cxPointOffset(FStart, FClientRect.TopLeft, False), TaskViewInfo.Color,
    TaskViewInfo.ScaleFactor.Apply(TdxChartTaskLinkingImage.DefaultLineWidth));
end;

function TdxChartTaskLinkingObject.GetDragAndDropCursor
  (Accepted: Boolean): TCursor;
begin
  Result := TdxGanttControlCursors.TaskLinking;
end;

function TdxChartTaskLinkingObject.GetDragImage: TdxChartTaskLinkingImage;
begin
  Result := TdxChartTaskLinkingImage(inherited DragImage);
end;

function TdxChartTaskLinkingObject.GetScrollableArea: TRect;
begin
  Result := FClientRect;
end;

procedure TdxChartTaskLinkingObject.UpdateDragImage;
begin
  ShowDragImage(FClientRect.TopLeft);
end;

procedure TdxChartTaskLinkingObject.UpdateTimeBounds(P: TPoint);
begin
  DragImage.SetFinish(cxPointOffset(HitTest.HitPoint,
    FClientRect.TopLeft, False));
end;

{ TdxChartTaskMovingObject }

constructor TdxChartTaskMovingObject.Create(AController
  : TdxGanttControlChartViewAreaController;
  ATaskViewInfo: TdxGanttControlViewChartTaskViewInfo);
begin
  inherited Create(AController, ATaskViewInfo);
  if TaskViewInfo.UseRightToLeftAlignment then
    FStartOffset := HitTest.HitPoint.X - TaskViewInfo.BarBounds.Right
  else
    FStartOffset := HitTest.HitPoint.X - TaskViewInfo.BarBounds.Left;
end;

procedure TdxChartTaskMovingObject.BeforeBeginDragAndDrop;
begin
  inherited BeforeBeginDragAndDrop;
  FStartPoint := HitTest.HitPoint;
end;

function TdxChartTaskMovingObject.Clone: TdxChartTaskMovingObject;
begin
  Result := TdxChartTaskMovingObject.Create(Controller, TaskViewInfo);
  Result.FStartOffset := FStartOffset;
end;

procedure TdxChartTaskMovingObject.DragAndDrop(const P: TPoint;
  var Accepted: Boolean);
begin
  if not IsLinking(P) then
    inherited DragAndDrop(P, Accepted);
end;

procedure TdxChartTaskMovingObject.ApplyChanges(const P: TPoint);
begin
  UpdateTimeBounds(P);
  with TdxGanttControlChangeTaskStartCommand.Create(Control, Task, FStart) do
    try
      Execute;
    finally
      Free;
    end;
end;

function TdxChartTaskMovingObject.GetDragAndDropCursor
  (Accepted: Boolean): TCursor;
begin
  if Accepted then
    Result := TdxGanttControlCursors.TaskMoving
  else
    Result := inherited GetDragAndDropCursor(Accepted);
end;

function TdxChartTaskMovingObject.IsLinking(const P: TPoint): Boolean;
var
  DX, DY: Integer;
begin
  HitTest.Calculate(P.X, P.Y);
  Result := (HitTest.HitObject <> TaskViewInfo) and
    (HitTest.HitObject is TdxGanttControlViewChartTaskViewInfo);
  if not Result and not FStartPoint.IsEqual(cxInvisiblePoint) then
  begin
    DX := Abs(FStartPoint.X - HitTest.HitPoint.X);
    DY := Abs(FStartPoint.Y - HitTest.HitPoint.Y);
    if Max(DX, DY) > Controller.ViewInfo.ScaleFactor.Apply(Mouse.DragThreshold)
    then
    begin
      FStartPoint := cxInvisiblePoint;
      Result := DY > DX;
    end;
  end;
  if Result then
    TdxChartDragHelper(Controller.DragHelper).ConvertToLinkingObject(Self);
end;

procedure TdxChartTaskMovingObject.UpdateTimeBounds(P: TPoint);

  function GetDateTime(X: Integer): TDateTime;
  begin
    Result := Controller.ViewInfo.HeaderViewInfo.GetDateTimeByPos(X);
    if Result <> InvalidDate then
      Result := GetCalendar.GetNextWorkTime(Result);
  end;

var
  ADate: TDateTime;
  X: Integer;
  ADuration: TdxGanttControlDuration;
begin
  X := P.X - FStartOffset;
  ADate := GetDateTime(X);
  if ADate <> InvalidDate then
  begin
    FStart := ADate;
    ADuration := TdxGanttControlDuration.Create(Task.RealDuration);
    FFinish := ADuration.GetWorkFinish(FStart, GetCalendar,
      Task.RealDurationFormat);
  end;
end;

{ TdxGanttControlViewChartSheetViewInfo }

function TdxGanttControlViewChartSheetViewInfo.GetColumnHeaderHeight: Integer;
begin
  Result := Owner.GetHeaderHeight;
end;

function TdxGanttControlViewChartSheetViewInfo.GetOwner
  : TdxGanttControlViewChartViewInfo;
begin
  Result := TdxGanttControlViewChartViewInfo(inherited Owner);
end;

procedure TdxGanttControlViewChartSheetViewInfo.PrepareCanvas
  (ACanvas: TcxCustomCanvas);
begin
  // do nothing
end;

{ TdxChartNewTaskCreatingObject }

procedure TdxChartNewTaskCreatingObject.ApplyChanges(const P: TPoint);
begin
  with TdxViewChartAreaCreateTaskCommand.Create(Controller,
    FTaskViewInfoIndex + Controller.ParentController.FirstVisibleRowIndex,
    FStart, FFinish) do
    try
      Execute;
    finally
      Free;
    end;
end;

procedure TdxChartNewTaskCreatingObject.BeginDragAndDrop;
begin
  inherited BeginDragAndDrop;
  FStart := Controller.ViewInfo.HeaderViewInfo.GetDateTimeByPos(CurMousePos.X);
  FStart := Controller.DataProvider.DataModel.ActiveCalendar.
    GetNextWorkTime(FStart);
  FFinish := FStart;
  FTaskViewInfoIndex := Controller.ViewInfo.Tasks.IndexOf(TaskViewInfo);
end;

function TdxChartNewTaskCreatingObject.CanDrop(const P: TPoint): Boolean;
begin
  UpdateTimeBounds(P);
  Result := FFinish > FStart;
end;

function TdxChartNewTaskCreatingObject.GetDragAndDropCursor
  (Accepted: Boolean): TCursor;
begin
  if Accepted then
    Result := TdxGanttControlCursors.TaskCreating
  else
    Result := inherited GetDragAndDropCursor(Accepted);
end;

function TdxChartNewTaskCreatingObject.GetTaskViewInfo
  : TdxGanttControlViewChartTaskViewInfo;
begin
  if Task <> nil then
    Result := inherited GetTaskViewInfo
  else
    Result := Controller.ViewInfo.Tasks[FTaskViewInfoIndex];
end;

procedure TdxChartNewTaskCreatingObject.UpdateTimeBounds(P: TPoint);
begin
  FFinish := Controller.ViewInfo.HeaderViewInfo.GetDateTimeByPos(P.X);
  if not GetCalendar.IsWorkTime(FFinish - OneMinute) then
    FFinish := GetCalendar.GetPreviousWorkTime(FFinish);
end;

{ TdxChartTaskProgressImage }

constructor TdxChartTaskProgressImage.Create(ADragObject
  : TdxChartTaskDragObject);
begin
  inherited Create;
  FDragObject := ADragObject;
  FValue := 0;
  SetImageSize(FDragObject.TaskViewInfo.BarBounds.Size);
end;

procedure TdxChartTaskProgressImage.SetValue(const Value: Integer);
begin
  if FValue <> Value then
  begin
    FValue := Value;
    UpdateImage;
    Invalidate;
  end;
end;

procedure TdxChartTaskProgressImage.DoUpdateImage(AImage: TcxAlphaBitmap);
var
  R: TRect;
begin
  AImage.cxCanvas.FillRect(AImage.ClientRect,
    FDragObject.Controller.ViewInfo.LookAndFeelPainter.DefaultContentColor);
  R := FDragObject.TaskViewInfo.DoCalculateBarProgressBounds(Value);
  R.Location := TPoint.Null;
  FDragObject.TaskViewInfo.DoDrawBar(AImage.cxCanvas, AImage.ClientRect, R);
end;

{ TdxGanttControlViewChartAreaMinorHeaderViewInfo }

function TdxGanttControlViewChartAreaMinorHeaderViewInfo.
  CalculateCaption: string;
var
  ADate: TcxDateTime;
  ASystemDate: TSystemTime;
begin
  case Owner.TimescaleUnit of
    TdxGanttControlChartViewTimescaleUnit.Hours,
      TdxGanttControlChartViewTimescaleUnit.QuarterDays:
      Result := FormatDateTime('HH', DateTime);
    TdxGanttControlChartViewTimescaleUnit.Days:
      Result := TdxCultureInfo.CurrentCulture.FormatSettings.ShortDayNames
        [dxDayOfWeekToVCL(dxDayOfWeek(DateTime))];
    TdxGanttControlChartViewTimescaleUnit.Weeks:
      Result := FormatDateTime('dd/mm', DateTime);
    TdxGanttControlChartViewTimescaleUnit.ThirdsOfMonths:
      begin
        ADate := cxGetLocalCalendar.FromDateTime(DateTime);
        if ADate.Day < 10 then
          Result := cxGetResourceString
            (@sdxGanttControlViewChartThirdsOfMonthsCaptionBegin)
        else if ADate.Day < 20 then
          Result := cxGetResourceString
            (@sdxGanttControlViewChartThirdsOfMonthsCaptionMiddle)
        else
          Result := cxGetResourceString
            (@sdxGanttControlViewChartThirdsOfMonthsCaptionEnd)
      end;
    TdxGanttControlChartViewTimescaleUnit.Months:
      begin
        DateTimeToSystemTime(DateTime, ASystemDate);
        Result := TdxCultureInfo.CurrentCulture.FormatSettings.ShortMonthNames
          [ASystemDate.wMonth];
      end;
    TdxGanttControlChartViewTimescaleUnit.Quarters:
      begin
        ADate := cxGetLocalCalendar.FromDateTime(DateTime);
        Result := Format(cxGetResourceString(@sdxGanttControlViewChartQuarters),
          [ADate.Month div 3 + 1]);
      end;
    TdxGanttControlChartViewTimescaleUnit.HalfYears:
      begin
        ADate := cxGetLocalCalendar.FromDateTime(DateTime);
        Result := Format
          (cxGetResourceString(@sdxGanttControlViewChartHalfYears),
          [ADate.Month div 6 + 1]);
      end;
    TdxGanttControlChartViewTimescaleUnit.Years:
      Result := Format('''%s', [FormatDateTime('yy', DateTime)]);
  else
    Result := '';
  end;
end;

function TdxGanttControlViewChartAreaMinorHeaderViewInfo.
  CalculateDrawTextFlags: Integer;
begin
  Result := inherited CalculateDrawTextFlags or CXTO_CENTER_HORIZONTALLY;
end;

function TdxGanttControlViewChartAreaMinorHeaderViewInfo.
  CalculateSeparatorBounds: TRect;
begin
  Result := inherited CalculateSeparatorBounds;
  if not Owner.IsEndOfMajor(Self) then
    Result.Top := Result.Top + ScaleFactor.Apply(6);
end;

function TdxGanttControlViewChartAreaMinorHeaderViewInfo.GetHintText: string;
begin
  Result := DateTimeToStr(DateTime);
end;

function TdxGanttControlViewChartAreaMinorHeaderViewInfo.GetTextLayout
  : TcxCanvasBasedTextLayout;
begin
  Result := Owner.Owner.GetMinorHeaderTextLayout(Caption);
end;

function TdxGanttControlViewChartAreaMinorHeaderViewInfo.HasHint: Boolean;
begin
  Result := True;
end;

{ TdxGanttControlViewChartPredecessorLinkSFViewInfo }

procedure TdxGanttControlViewChartPredecessorLinkSFViewInfo.DoCalculate
  (const AStartPoint, AFinishPoint: TPoint; var AIndex: Integer);
var
  P: TPoint;
begin
  P := GetStartLineFinishPoint(AStartPoint);
  AddPoint(P, AIndex);
  if P.Y > AFinishPoint.Y then
    P.Y := PredecessorViewInfo.Bounds.Top
  else
    P.Y := PredecessorViewInfo.Bounds.Bottom;
  AddPoint(P, AIndex);
  P.X := AFinishPoint.X;
  P := cxPointOffset(P, ScaleFactor.Apply(LinkLineFinishOffset), 0,
    not UseRightToLeftAlignment);
  AddPoint(P, AIndex);
  P.Y := AFinishPoint.Y;
  AddPoint(P, AIndex);
  AddPoint(AFinishPoint, AIndex);
end;

function TdxGanttControlViewChartPredecessorLinkSFViewInfo.GetLinkType
  : TdxGanttControlTaskPredecessorLinkType;
begin
  Result := TdxGanttControlTaskPredecessorLinkType.SF;
end;

{ TdxChartAreaTextLayoutDictionary }

constructor TdxChartAreaTextLayoutDictionary.Create
  (AOwner: TdxGanttControlViewChartAreaViewInfo);
begin
  inherited Create;
  FOwner := AOwner;
  FMajorHeaderCache := TCache.Create([doOwnsValues]);
  FMinorHeaderCache := TCache.Create([doOwnsValues]);
  FTaskCaption := TCache.Create([doOwnsValues]);
end;

destructor TdxChartAreaTextLayoutDictionary.Destroy;
begin
  FreeAndNil(FCachedTextLayout);
  FreeAndNil(FTaskCaption);
  FreeAndNil(FMajorHeaderCache);
  FreeAndNil(FMinorHeaderCache);
  inherited Destroy;
end;

procedure TdxChartAreaTextLayoutDictionary.Clear;
begin
  FMajorHeaderCache.Clear;
  FMinorHeaderCache.Clear;
  FTaskCaption.Clear;
end;

function TdxChartAreaTextLayoutDictionary.GetMajorHeaderTextLayout
  (const AText: string): TcxCanvasBasedTextLayout;
begin
  Result := GetTextLayout(FMajorHeaderCache, AText);
end;

function TdxChartAreaTextLayoutDictionary.GetMinorHeaderTextLayout
  (const AText: string): TcxCanvasBasedTextLayout;
begin
  Result := GetTextLayout(FMinorHeaderCache, AText);
end;

function TdxChartAreaTextLayoutDictionary.GetTaskCaptionTextLayout
  (const AText: string): TcxCanvasBasedTextLayout;
begin
  Result := GetTextLayout(FTaskCaption, AText);
end;

function TdxChartAreaTextLayoutDictionary.GetTextLayout(ACache: TCache;
  const AText: string): TcxCanvasBasedTextLayout;
var
  AHash: Integer;
begin
  if FOwner.Canvas is TcxCanvas then
  begin
    if FCachedTextLayout = nil then
      FCachedTextLayout := FOwner.Canvas.CreateTextLayout;
    Exit(FCachedTextLayout);
  end;
  AHash := dxElfHash(AText);
  if not ACache.TryGetValue(AHash, Result) then
  begin
    Result := FOwner.Canvas.CreateTextLayout;
    ACache.Add(AHash, Result);
  end;
end;

{ TdxGanttControlViewChartAreaViewInfo }

constructor TdxGanttControlViewChartAreaViewInfo.Create
  (AOwner: TdxGanttControlCustomItemViewInfo);
begin
  inherited Create(AOwner);
  FNonWorkTimeAreas := TList<TRect>.Create;
  FTasks := TObjectList<TdxGanttControlViewChartTaskViewInfo>.Create;
  FPredecessorLinks :=
    TObjectList<TdxGanttControlViewChartPredecessorLinkViewInfo>.Create;
  FHiddenTasks := TObjectList<TdxGanttControlViewChartTaskViewInfo>.Create;
  FTaskCachedValues := TdxGanttControlTaskViewInfoCachedValues.Create;
  FTextLayoutDictionary := TdxChartAreaTextLayoutDictionary.Create(Self);
end;

destructor TdxGanttControlViewChartAreaViewInfo.Destroy;
begin
  FreeAndNil(FTextLayoutDictionary);
  FreeAndNil(FTaskCachedValues);
  FreeAndNil(FHiddenTasks);
  FreeAndNil(FPredecessorLinks);
  FreeAndNil(FTasks);
  FreeAndNil(FNonWorkTimeAreas);
  inherited Destroy;
end;

function TdxGanttControlViewChartAreaViewInfo.CalculateHeaderBounds: TRect;
begin
  Result := Bounds;
  Result.Bottom := Result.Top + Owner.GetHeaderHeight;
end;

function TdxGanttControlViewChartAreaViewInfo.CalculateItemBounds
  (AItem: TdxGanttControlCustomItemViewInfo): TRect;
begin
  if AItem is TdxGanttControlViewChartAreaHeaderViewInfo then
    Result := CalculateHeaderBounds
  else
    Result := inherited CalculateItemBounds(AItem);
end;

function TdxGanttControlViewChartAreaViewInfo.CalculateHitTest
  (const AHitTest: TdxGanttControlHitTest): Boolean;
var
  I: Integer;
begin
  Result := ClientRect.Contains(AHitTest.HitPoint) and
    inherited CalculateHitTest(AHitTest);
  if Result then
  begin
    if HeaderViewInfo.CalculateHitTest(AHitTest) then
      Exit;
    for I := 0 to FPredecessorLinks.Count - 1 do
    begin
      if FPredecessorLinks[I].CalculateHitTest(AHitTest) then
        Exit;
    end;
    for I := 0 to FTasks.Count - 1 do
    begin
      if FTasks[I].CalculateHitTest(AHitTest) then
        Exit;
    end;
  end;
end;

procedure TdxGanttControlViewChartAreaViewInfo.Calculate(const R: TRect);
begin
  FTaskCachedValues.Update(LookAndFeelPainter, ScaleFactor);
  FFirstVisibleRowIndex := Controller.ParentController.FirstVisibleRowIndex;
  FFirstVisibleDateTime := Controller.FirstVisibleDateTime;
  FTimescaleUnit := Owner.View.TimescaleUnit;
  inherited Calculate(R);
  FCalendar := GetCurrentCalendar;
  FClientRect := Controller.ScrollBars.GetClientRect(Bounds);
  DoCalculate;
end;

procedure TdxGanttControlViewChartAreaViewInfo.CalculateLayout;
begin
  FNonWorkTimeAreas.Clear;
  AddChild(TdxGanttControlViewChartAreaHeaderViewInfo.Create(Self));
end;

procedure TdxGanttControlViewChartAreaViewInfo.ViewChanged;
begin
  if (FFirstVisibleRowIndex <> Controller.ParentController.FirstVisibleRowIndex)
    or (FFirstVisibleDateTime <> Controller.FirstVisibleDateTime) then
  begin
    if FFirstVisibleDateTime <> Controller.FirstVisibleDateTime then
    begin
      FFirstVisibleDateTime := Controller.FirstVisibleDateTime;
      inherited ViewChanged;
    end;
    FFirstVisibleRowIndex := Controller.ParentController.FirstVisibleRowIndex;
    DoCalculate;
  end
  else
    CalculateGridlines;
end;

procedure TdxGanttControlViewChartAreaViewInfo.DoDraw;
var
  I: Integer;
  AColor, ANonWorkTimeAreaColor: TColor;
begin
  Canvas.FillRect(Bounds, LookAndFeelPainter.DefaultContentColor);
  Controller.ScrollBars.DrawSizeGrip(Canvas);
  inherited DoDraw;
  AColor := ColorToRgb(LookAndFeelPainter.DefaultContentColor);
  ANonWorkTimeAreaColor := Rgb(MulDiv(GetRValue(AColor), 97, 100),
    MulDiv(GetGValue(AColor), 97, 100), MulDiv(GetBValue(AColor), 97, 100));
  for I := 0 to FNonWorkTimeAreas.Count - 1 do
    Canvas.FillRect(FNonWorkTimeAreas[I], ANonWorkTimeAreaColor);

  if not FGridlineProjectStartBounds.IsZero then
    Canvas.Line(FGridlineProjectStartBounds.TopLeft,
      FGridlineProjectStartBounds.BottomRight,
      cxGetActualColor(Owner.View.OptionsGridline.ProjectStart.Color,
      LookAndFeelPainter.DefaultGanttProjectStartGridlineColor), 1,
      Owner.View.OptionsGridline.ProjectStart.Style);
  if not FGridlineProjectFinishBounds.IsZero then
    Canvas.Line(FGridlineProjectFinishBounds.TopLeft,
      FGridlineProjectFinishBounds.BottomRight,
      cxGetActualColor(Owner.View.OptionsGridline.ProjectFinish.Color,
      LookAndFeelPainter.DefaultGanttProjectFinishGridlineColor), 1,
      Owner.View.OptionsGridline.ProjectFinish.Style);
  if not FGridlineCurrentDateBounds.IsZero then
    Canvas.Line(FGridlineCurrentDateBounds.TopLeft,
      FGridlineCurrentDateBounds.BottomRight,
      cxGetActualColor(Owner.View.OptionsGridline.CurrentDate.Color,
      LookAndFeelPainter.DefaultGanttCurrentDateGridlineColor), 1,
      Owner.View.OptionsGridline.CurrentDate.Style);

  for I := 0 to FTasks.Count - 1 do
    FTasks[I].Draw;
  for I := 0 to FPredecessorLinks.Count - 1 do
    FPredecessorLinks[I].Draw;
end;

procedure TdxGanttControlViewChartAreaViewInfo.CalculateNonWorkTimeAreas;
var
  I: Integer;
  R: TRect;
begin
  FNonWorkTimeAreas.Clear;
  R := ClientRect;
  R.Top := HeaderViewInfo.Bounds.Bottom;
  for I := 0 to HeaderViewInfo.MinorHeaders.Count - 1 do
    if IsNonWorkTime(HeaderViewInfo.MinorHeaders[I].DateTime) then
    begin
      R.Left := HeaderViewInfo.MinorHeaders[I].Bounds.Left;
      R.Right := HeaderViewInfo.MinorHeaders[I].Bounds.Right;
      FNonWorkTimeAreas.Add(R);
    end;
end;

procedure TdxGanttControlViewChartAreaViewInfo.CalculatePredecessorLinks;

  function GetTaskViewInfo(ATask: TdxGanttControlTask)
    : TdxGanttControlViewChartTaskViewInfo;
  var
    I: Integer;
    R: TRect;
  begin
    Result := nil;
    for I := 0 to FTasks.Count - 1 do
      if FTasks[I].Task = ATask then
      begin
        Result := FTasks[I];
        Break;
      end;
    for I := 0 to FHiddenTasks.Count - 1 do
      if FHiddenTasks[I].Task = ATask then
      begin
        Result := FHiddenTasks[I];
        Break;
      end;

    if Result = nil then
    begin
      Result := CreateTaskViewInfo(ATask);
      R := Bounds;
      R.Height := MulDiv(Owner.View.OptionsSheet.RowHeight,
        Abs(CanvasCache.GetBaseFont.Height), 11);
      if Controller.DataProvider.IndexOf(ATask) < FFirstVisibleRowIndex then
        R.MoveToBottom(Bounds.Top - 10)
      else
        R.MoveToTop(Bounds.Bottom + 10);
      Result.Calculate(R);
      FHiddenTasks.Add(Result);
    end;
  end;

  function CreateLinkLinkViewInfo(ATask, APredecessor: TdxGanttControlTask;
    APredecessorLink: TdxGanttControlTaskPredecessorLink)
    : TdxGanttControlViewChartPredecessorLinkViewInfo;
  begin
    case APredecessorLink.&Type of
      TdxGanttControlTaskPredecessorLinkType.FF:
        Result := TdxGanttControlViewChartPredecessorLinkFFViewInfo.Create(Self,
          APredecessorLink, GetTaskViewInfo(ATask),
          GetTaskViewInfo(APredecessor));
      TdxGanttControlTaskPredecessorLinkType.SS:
        Result := TdxGanttControlViewChartPredecessorLinkSSViewInfo.Create(Self,
          APredecessorLink, GetTaskViewInfo(ATask),
          GetTaskViewInfo(APredecessor));
      TdxGanttControlTaskPredecessorLinkType.SF:
        Result := TdxGanttControlViewChartPredecessorLinkSFViewInfo.Create(Self,
          APredecessorLink, GetTaskViewInfo(ATask),
          GetTaskViewInfo(APredecessor));
    else
      Result := TdxGanttControlViewChartPredecessorLinkFSViewInfo.Create(Self,
        APredecessorLink, GetTaskViewInfo(ATask),
        GetTaskViewInfo(APredecessor));
    end;
  end;

var
  R: TRect;
  I, J: Integer;
  ATask: TdxGanttControlTask;
  APredecessor: TdxGanttControlTask;
  ALinkViewInfo: TdxGanttControlViewChartPredecessorLinkViewInfo;
  AStartRowIndex, AFinishRowIndex: Integer;
  AFirstVisibleID, ALastVisibleID: Integer;
begin
  FPredecessorLinks.Clear;
  FHiddenTasks.Clear;
  R := Bounds;
  R.Top := HeaderViewInfo.Bounds.Bottom;
  if FFirstVisibleRowIndex >= Controller.DataProvider.Count then
    Exit;
  AStartRowIndex :=
    Max(0, Controller.ParentController.FirstVisibleRowIndex - 300);
  AFinishRowIndex := Min(Controller.DataProvider.Count - 1,
    Controller.ParentController.FirstVisibleRowIndex + Tasks.Count + 300);
  AFirstVisibleID := Controller.DataProvider.Tasks
    [Controller.ParentController.FirstVisibleRowIndex].ID;
  ALastVisibleID := Controller.DataProvider.Tasks
    [Min(Controller.DataProvider.Count - 1,
    Controller.ParentController.FirstVisibleRowIndex + Tasks.Count)].ID;
  for I := AStartRowIndex to AFinishRowIndex do
  begin
    ATask := Controller.DataProvider[I];
    if ATask.Blank or not ATask.IsValueAssigned
      (TdxGanttTaskAssignedValue.PredecessorLinks) then
      Continue;
    for J := 0 to ATask.PredecessorLinks.Count - 1 do
    begin
      if not ATask.PredecessorLinks[J].IsValueAssigned
        (TdxGanttTaskPredecessorLinkAssignedValue.PredecessorUID) then
        Continue;
      APredecessor := Controller.DataProvider.GetTaskByUID
        (ATask.PredecessorLinks[J].PredecessorUID);
      if (APredecessor = nil) or APredecessor.Blank or
        not APredecessor.IsValueAssigned(TdxGanttTaskAssignedValue.Start) or
        not APredecessor.IsValueAssigned(TdxGanttTaskAssignedValue.Finish) then
        Continue;
      if (ATask.ID < AFirstVisibleID) and (APredecessor.ID < AFirstVisibleID)
      then
        Continue;
      if (ATask.ID > ALastVisibleID) and (APredecessor.ID > ALastVisibleID) then
        Continue;
      ALinkViewInfo := CreateLinkLinkViewInfo(ATask, APredecessor,
        ATask.PredecessorLinks[J]);
      ALinkViewInfo.Calculate(R);
      FPredecessorLinks.Add(ALinkViewInfo);
    end;
  end;
end;

procedure TdxGanttControlViewChartAreaViewInfo.CalculateGridlines;

  function CalculateBound(ADateTime: TDateTime;
    AGridline: TdxGanttControlGridline): TRect;
  var
    I: Integer;
    AMinorHeader: TdxGanttControlViewChartAreaMinorHeaderViewInfo;
    AIsStart: Boolean;
  begin
    Result := TRect.Null;
    if not AGridline.IsVisible then
      Exit;
    AIsStart := AGridline <> Owner.View.OptionsGridline.ProjectFinish;
    AMinorHeader := HeaderViewInfo.GetMinorHeaderViewInfo(ADateTime, AIsStart);
    if AMinorHeader <> nil then
    begin
      if AIsStart then
      begin
        I := HeaderViewInfo.MinorHeaders.IndexOf(AMinorHeader);
        if I = 0 then
          Exit;
        AMinorHeader := HeaderViewInfo.MinorHeaders[I - 1];
      end;
      Result.Height := ClientRect.Height;
      Result.Width := 0;
      Result.Top := AMinorHeader.Bounds.Bottom + 1;
      if UseRightToLeftAlignment then
        Result.MoveToLeft(AMinorHeader.Bounds.Left)
      else
        Result.MoveToRight(AMinorHeader.Bounds.Right - 1);
    end;
  end;

begin
  FGridlineProjectStartBounds :=
    CalculateBound(Controller.DataProvider.DataModel.RealProjectStart,
    Owner.View.OptionsGridline.ProjectStart);
  FGridlineProjectFinishBounds :=
    CalculateBound(Controller.DataProvider.DataModel.RealProjectFinish,
    Owner.View.OptionsGridline.ProjectFinish);
  FGridlineCurrentDateBounds := CalculateBound(Now,
    Owner.View.OptionsGridline.CurrentDate);
end;

procedure TdxGanttControlViewChartAreaViewInfo.CalculateTasks;

  procedure AddTaskViewInfo(ATask: TdxGanttControlTask; const R: TRect);
  begin
    FTasks.Add(CreateTaskViewInfo(ATask));
    FTasks.Last.Calculate(R);
  end;

var
  I: Integer;
  R: TRect;
  ATask: TdxGanttControlTask;
begin
  FTasks.Clear;
  R := ClientRect;
  if Owner.SheetViewInfo <> nil then
  begin
    for I := 0 to Owner.SheetViewInfo.DataRows.Count - 1 do
    begin
      ATask := TdxGanttControlTask(Owner.SheetViewInfo.DataRows[I].Data);
      R.Top := Owner.SheetViewInfo.DataRows[I].Bounds.Top;
      R.Bottom := Owner.SheetViewInfo.DataRows[I].Bounds.Bottom;
      AddTaskViewInfo(ATask, R);
    end;
  end
  else
  begin
    R.Top := HeaderViewInfo.Bounds.Bottom;
    I := FFirstVisibleRowIndex;
    while R.Top < ClientRect.Bottom do
    begin
      R.Bottom := R.Top + MulDiv(Owner.View.OptionsSheet.RowHeight,
        Abs(CanvasCache.GetBaseFont.Height), 11);
      if I < Controller.DataProvider.Count then
        ATask := Controller.DataProvider.Tasks[I]
      else
        ATask := nil;
      AddTaskViewInfo(ATask, R);
      Inc(I);
      R.Top := R.Bottom;
    end;
  end;
end;

function TdxGanttControlViewChartAreaViewInfo.CreateTaskViewInfo
  (ATask: TdxGanttControlTask): TdxGanttControlViewChartTaskViewInfo;
begin
  if (ATask = nil) or (ATask.Blank) then
    Result := TdxGanttControlViewChartEmptyTaskViewInfo.Create(Self, ATask)
  else if ATask.Summary then
    Result := TdxGanttControlViewChartSummaryTaskViewInfo.Create(Self, ATask)
  else if ATask.Milestone then
    Result := TdxGanttControlViewChartMilestoneTaskViewInfo.Create(Self, ATask)
  else
    Result := TdxGanttControlViewChartTaskViewInfo.Create(Self, ATask);
end;

procedure TdxGanttControlViewChartAreaViewInfo.DoCalculate;
begin
  CalculateNonWorkTimeAreas;
  CalculateTasks;
  CalculatePredecessorLinks;
  CalculateGridlines;
  Controller.ScrollBars.CalculateScrollBars;
end;

function TdxGanttControlViewChartAreaViewInfo.IsNonWorkTime
  (ATime: TDateTime): Boolean;
begin
  if FCalendar = nil then
    Exit(False);
  if FTimescaleUnit = TdxGanttControlChartViewTimescaleUnit.Hours then
    Result := not FCalendar.IsWorkTime(ATime)
  else if FTimescaleUnit = TdxGanttControlChartViewTimescaleUnit.QuarterDays
  then
    Result := not(FCalendar.IsWorkTime(ATime) or
      FCalendar.IsWorkTime(ATime + 1 / 24 - OneMinute) or
      FCalendar.IsWorkTime(ATime + 2 / 24 - OneMinute) or
      FCalendar.IsWorkTime(ATime + 3 / 24 - OneMinute) or
      FCalendar.IsWorkTime(ATime + 4 / 24 - OneMinute) or
      FCalendar.IsWorkTime(ATime + 5 / 24 - OneMinute) or
      FCalendar.IsWorkTime(ATime + 6 / 24 - OneMinute))
  else if FTimescaleUnit = TdxGanttControlChartViewTimescaleUnit.Days then
    Result := not FCalendar.IsWorkday(ATime)
  else
    Result := False;
end;

procedure TdxGanttControlViewChartAreaViewInfo.Reset;
begin
  inherited Reset;
  FTextLayoutDictionary.Clear;
end;

function TdxGanttControlViewChartAreaViewInfo.GetController
  : TdxGanttControlChartViewAreaController;
begin
  Result := Owner.Controller.ChartAreaController;
end;

function TdxGanttControlViewChartAreaViewInfo.GetCurrentCalendar
  : TdxGanttControlCalendar;
begin
  Result := TdxCustomGanttControl(Owner.View.Owner).DataModel.ActiveCalendar;
end;

function TdxGanttControlViewChartAreaViewInfo.GetFullyVisibleTaskCount: Integer;
begin
  Result := Tasks.Count;
  if Tasks.Last.Bounds.Bottom > ClientRect.Bottom then
    Dec(Result);
end;

function TdxGanttControlViewChartAreaViewInfo.GetHeaderViewInfo
  : TdxGanttControlViewChartAreaHeaderViewInfo;
begin
  Result := TdxGanttControlViewChartAreaHeaderViewInfo(ViewInfos[0]);
end;

function TdxGanttControlViewChartAreaViewInfo.GetOwner
  : TdxGanttControlViewChartViewInfo;
begin
  Result := TdxGanttControlViewChartViewInfo(inherited Owner);
end;

function TdxGanttControlViewChartAreaViewInfo.GetTaskRowViewInfo
  (const P: TPoint): TdxGanttControlViewChartTaskViewInfo;
var
  I: Integer;
begin
  for I := 0 to Tasks.Count - 1 do
    if Tasks[I].Bounds.Contains(P) then
      Exit(Tasks[I]);
  Result := nil;
end;

function TdxGanttControlViewChartAreaViewInfo.GetHScrollBarPageSize: Integer;
begin
  Result := HeaderViewInfo.MinorHeaders.Count;
end;

function TdxGanttControlViewChartAreaViewInfo.GetMajorHeaderTextLayout
  (const AText: string): TcxCanvasBasedTextLayout;
begin
  Result := FTextLayoutDictionary.GetMajorHeaderTextLayout(AText);
end;

function TdxGanttControlViewChartAreaViewInfo.GetMinorHeaderTextLayout
  (const AText: string): TcxCanvasBasedTextLayout;
begin
  Result := FTextLayoutDictionary.GetMinorHeaderTextLayout(AText);
end;

function TdxGanttControlViewChartAreaViewInfo.GetTaskCaptionTextLayout
  (const AText: string): TcxCanvasBasedTextLayout;
begin
  Result := FTextLayoutDictionary.GetTaskCaptionTextLayout(AText);
end;

{ TdxGanttControlViewChartPredecessorLinkSSViewInfo }

procedure TdxGanttControlViewChartPredecessorLinkSSViewInfo.DoCalculate
  (const AStartPoint, AFinishPoint: TPoint; var AIndex: Integer);
var
  P: TPoint;
begin
  if UseRightToLeftAlignment xor (AStartPoint.X -
    IfThen(UseRightToLeftAlignment, -1, 1) * ScaleFactor.Apply
    (LinkLineStartOffset - LinkLineFinishOffset) <= AFinishPoint.X) then
    P := GetStartLineFinishPoint(AStartPoint)
  else
  begin
    P := AStartPoint;
    P.X := AFinishPoint.X;
    P := cxPointOffset(P, ScaleFactor.Apply(LinkLineFinishOffset), 0,
      UseRightToLeftAlignment);
  end;
  AddPoint(P, AIndex);
  P.Y := AFinishPoint.Y;
  AddPoint(P, AIndex);
  AddPoint(AFinishPoint, AIndex);
end;

function TdxGanttControlViewChartPredecessorLinkSSViewInfo.GetLinkType
  : TdxGanttControlTaskPredecessorLinkType;
begin
  Result := TdxGanttControlTaskPredecessorLinkType.SS;
end;

{ TdxViewChartCreateTaskCommand }

constructor TdxViewChartCreateTaskCommand.Create(AController
  : TdxGanttControlChartViewController; AIndex: Integer;
  AValue: TdxGanttControlTask);
begin
  inherited Create(AController, AIndex);
  FValue := AValue;
end;

procedure TdxViewChartCreateTaskCommand.DoExecute;
begin
  inherited DoExecute;
  with TdxGanttControlChangeTaskCommand.Create(Control, Task, FValue) do
    try
      Execute;
    finally
      Free;
    end;
end;

{ TdxGanttControlViewChartMilestoneTaskViewInfo }

function TdxGanttControlViewChartMilestoneTaskViewInfo.DoCalculateBarBounds
  (ATaskStart, ATaskFinish: TDateTime): TRect;
begin
  Result := inherited DoCalculateBarBounds(ATaskStart, ATaskFinish);
  Result.Size := GetMilestoneSize;
  Result.Offset(-Result.Width div 2, 0);
end;

procedure TdxGanttControlViewChartMilestoneTaskViewInfo.DoDrawBar
  (ACanvas: TcxCustomCanvas; const ABounds, AProgressBounds: TRect);
begin
  LookAndFeelPainter.DrawGanttMilestone(ACanvas, ABounds, ScaleFactor, Color);
end;

function TdxGanttControlViewChartMilestoneTaskViewInfo.GetDefaultColor: TColor;
begin
  Result := CachedValues.MilestoneColor;
end;

function TdxGanttControlViewChartMilestoneTaskViewInfo.GetTaskHeight: Integer;
begin
  Result := CachedValues.MilestoneSize.cy;
end;

function TdxGanttControlViewChartMilestoneTaskViewInfo.IsCompleteZone
  (const P: TPoint): Boolean;
begin
  Result := False;
end;

function TdxGanttControlViewChartMilestoneTaskViewInfo.IsSizingZone
  (const P: TPoint): Boolean;
begin
  Result := False;
end;

{ TdxGanttControlViewChartPredecessorLinkViewInfo }

constructor TdxGanttControlViewChartPredecessorLinkViewInfo.Create
  (AOwner: TdxGanttControlViewChartAreaViewInfo;
  ALink: TdxGanttControlTaskPredecessorLink;
  ATaskViewInfo, APredecessorViewInfo: TdxGanttControlViewChartTaskViewInfo);
begin
  inherited Create(AOwner);
  FLink := ALink;
  FPredecessorViewInfo := APredecessorViewInfo;
  FTaskViewInfo := ATaskViewInfo;
end;

procedure TdxGanttControlViewChartPredecessorLinkViewInfo.Calculate
  (const R: TRect);
var
  AStartPoint, AFinishPoint: TPoint;
  AIndex: Integer;
begin
  inherited Calculate(R);
  AIndex := 0;
  SetLength(FPoints, 10);
  AStartPoint := GetStartPoint;
  AFinishPoint := GetFinishPoint;
  AddPoint(AStartPoint, AIndex);
  DoCalculate(AStartPoint, AFinishPoint, AIndex);
  SetLength(FPoints, AIndex);
  CalculateArrow;
end;

procedure TdxGanttControlViewChartPredecessorLinkViewInfo.DoDraw;

  procedure AdjustPositionArcRect(var R: TRect; AIndex: Integer);
  begin
    if FPoints[AIndex - 1].X = FPoints[AIndex].X then
    begin
      if FPoints[AIndex - 1].Y > FPoints[AIndex].Y then
        R.MoveToTop(FPoints[AIndex].Y)
      else
        R.MoveToBottom(FPoints[AIndex].Y + 1);
      if FPoints[AIndex].X > FPoints[AIndex + 1].X then
        R.MoveToRight(FPoints[AIndex].X + 1)
      else
        R.MoveToLeft(FPoints[AIndex].X);
    end
    else
    begin
      if FPoints[AIndex - 1].X > FPoints[AIndex].X then
        R.MoveToLeft(FPoints[AIndex].X)
      else
        R.MoveToRight(FPoints[AIndex].X + 1);
      if FPoints[AIndex].Y > FPoints[AIndex + 1].Y then
        R.MoveToBottom(FPoints[AIndex].Y + 1)
      else
        R.MoveToTop(FPoints[AIndex].Y);
    end;
  end;

  procedure CalculateArcPoints(var R: TRect; AIndex: Integer;
    var P, P1: TPoint);
  var
    ALinkArc: Integer;
  begin
    ALinkArc := ScaleFactor.Apply(LinkArc);
    P := FPoints[AIndex];
    P1 := FPoints[AIndex];
    if R.Top = FPoints[AIndex].Y then
    begin
      if R.Left = FPoints[AIndex].X then
      begin
        P := cxPointOffset(P, ALinkArc, 0);
        P1 := cxPointOffset(P1, 0, ALinkArc);
      end
      else
      begin
        P := cxPointOffset(P, 0, ALinkArc);
        P1 := cxPointOffset(P1, -ALinkArc + 1, 0);
      end;
    end
    else
    begin
      if R.Left = FPoints[AIndex].X then
      begin
        P := cxPointOffset(P, 0, -ALinkArc + 1);
        P1 := cxPointOffset(P1, ALinkArc, 0);
      end
      else
      begin
        P := cxPointOffset(P, -ALinkArc + 1, 0);
        P1 := cxPointOffset(P1, 0, -ALinkArc + 1);
      end;
    end;
  end;

  procedure DrawArrow(AColor: TColor);
  var
    AArrowPoints: TcxArrowPoints;
    ADirection: TcxArrowDirection;
  begin
    AArrowPoints := FArrowPoints;
    if IsHotState then
    begin
      ADirection := GetArrowDirection;
      case ADirection of
        adUp, adDown:
          begin
            AArrowPoints[0] := cxPointOffset(AArrowPoints[0],
              IfThen(ADirection = adDown, 1, -1) * ScaleFactor.Apply(2),
              IfThen(ADirection = adUp, 1, -1) * ScaleFactor.Apply(2));
            AArrowPoints[2] := cxPointOffset(AArrowPoints[2],
              IfThen(ADirection = adUp, 1, -1) * ScaleFactor.Apply(2),
              IfThen(ADirection = adUp, 1, -1) * ScaleFactor.Apply(2));
          end;
        adLeft, adRight:
          begin
            AArrowPoints[0] := cxPointOffset(AArrowPoints[0],
              IfThen(ADirection = adLeft, 1, -1) * ScaleFactor.Apply(2),
              IfThen(ADirection = adLeft, 1, -1) * ScaleFactor.Apply(2));
            AArrowPoints[2] := cxPointOffset(AArrowPoints[2],
              IfThen(ADirection = adLeft, 1, -1) * ScaleFactor.Apply(2),
              IfThen(ADirection = adRight, 1, -1) * ScaleFactor.Apply(2));
          end;
      end;
    end;
    Canvas.Polyline(AArrowPoints, AColor, 1);
    Canvas.FillPolygon(AArrowPoints, AColor);
  end;

var
  I: Integer;
  P, P1: TPoint;
  AColor: TColor;
  AWidth: Integer;
  ALinkArc: Integer;
  AOffsetX, AOffsetY: Integer;
  R: TRect;
  ALine: TRect;
begin
  AColor := GetColor;
  AWidth := GetWidth;
  ALinkArc := ScaleFactor.Apply(LinkArc);
  R := TRect.Null;
  R.Width := ALinkArc * 2 + 1;
  R.Height := ALinkArc * 2 + 1;
  for I := 1 to Length(FPoints) - 1 do
  begin
    P := FPoints[I - 1];
    P1 := FPoints[I];
    AOffsetY := IfThen(P.X = P1.X, ALinkArc, 0);
    AOffsetX := IfThen(P.Y = P1.Y, ALinkArc, 0);
    if I > 1 then
      P := cxPointOffset(P, AOffsetX, AOffsetY, (P.Y < P1.Y) or (P.X < P1.X));
    if I < Length(FPoints) - 1 then
      P1 := cxPointOffset(P1, AOffsetX, AOffsetY, (P.Y > P1.Y) or (P.X > P1.X));
    ALine := TRect.Create(P, P1);
    if ALine.Width = 0 then
    begin
      ALine.Width := AWidth;
      ALine.Offset(-AWidth div 2, 0);
      ALine.NormalizeRect;
      ALine.Bottom := ALine.Bottom + 1;
    end;
    if ALine.Height = 0 then
    begin
      ALine.Height := AWidth;
      ALine.Offset(0, -AWidth div 2);
      ALine.NormalizeRect;
      ALine.Right := ALine.Right + 1;
    end;
    Canvas.FillRect(ALine, AColor);

    if I < Length(FPoints) - 1 then
    begin
      AdjustPositionArcRect(R, I);
      CalculateArcPoints(R, I, P, P1);
      Canvas.Arc(R.Left, R.Top, R.Right, R.Bottom, P.X, P.Y, P1.X, P1.Y,
        AColor, AWidth);
    end;
  end;
  DrawArrow(AColor);
end;

procedure TdxGanttControlViewChartPredecessorLinkViewInfo.AddPoint
  (const P: TPoint; var AIndex: Integer);
begin
  if AIndex >= Length(FPoints) then
    SetLength(FPoints, AIndex + 1);
  FPoints[AIndex] := P;
  Inc(AIndex);
end;

procedure TdxGanttControlViewChartPredecessorLinkViewInfo.DoCalculate
  (const AStartPoint, AFinishPoint: TPoint; var AIndex: Integer);
begin
  // do nothing
end;

function TdxGanttControlViewChartPredecessorLinkViewInfo.CalculateHitTest
  (const AHitTest: TdxGanttControlHitTest): Boolean;
var
  I: Integer;
  R: TRect;
begin
  Result := False;
  for I := 1 to Length(FPoints) - 1 do
  begin
    R := TRect.Create(FPoints[I - 1], FPoints[I]);
    R := cxRectInflate(R, IfThen(R.Width = 0, GetResizeHitZoneWidth div 2, 0),
      IfThen(R.Height = 0, GetResizeHitZoneWidth div 2, 0));
    R.NormalizeRect;
    Result := PtInRect(R, AHitTest.HitPoint);
    if Result then
    begin
      TdxGanttControlHitTestAccess(AHitTest).SetHitObject(Self);
      Break;
    end;
  end;
end;

function TdxGanttControlViewChartPredecessorLinkViewInfo.GetFinishPoint: TPoint;
begin
  Result.Y := (FTaskViewInfo.BarBounds.Top +
    FTaskViewInfo.BarBounds.Bottom) div 2;
  if UseRightToLeftAlignment xor (GetLinkType
    in [TdxGanttControlTaskPredecessorLinkType.SS,
    TdxGanttControlTaskPredecessorLinkType.FS]) then
    Result.X := FTaskViewInfo.BarBounds.Left - 1
  else
    Result.X := FTaskViewInfo.BarBounds.Right;
end;

function TdxGanttControlViewChartPredecessorLinkViewInfo.GetHintText: string;
var
  ALag, AFrom, ATo: string;
begin
  case FLink.&Type of
    TdxGanttControlTaskPredecessorLinkType.FF:
      Result := cxGetResourceString
        (@sdxGanttControlTaskDependencyDialogLinkTypeFF);
    TdxGanttControlTaskPredecessorLinkType.SF:
      Result := cxGetResourceString
        (@sdxGanttControlTaskDependencyDialogLinkTypeSF);
    TdxGanttControlTaskPredecessorLinkType.SS:
      Result := cxGetResourceString
        (@sdxGanttControlTaskDependencyDialogLinkTypeSS);
  else
    Result := cxGetResourceString
      (@sdxGanttControlTaskDependencyDialogLinkTypeFS);
  end;
  ALag := TdxGanttControlTaskPredecessorLinkAccess(FLink).GetLagAsDisplayValue;
  if ALag <> '' then
    Result := Format('%s'#13#10'%s %s',
      [Result, RemoveAccelChars(cxGetResourceString
      (@sdxGanttControlTaskDependencyDialogLag)), ALag]);
  AFrom := Format('%s %s %s',
    [cxGetResourceString(@sdxGanttControlTaskDependencyDialogFrom),
    Format(cxGetResourceString(@sdxGanttControlTaskID),
    [FPredecessorViewInfo.Task.ID]), FPredecessorViewInfo.Task.Name]);
  ATo := Format('%s %s %s',
    [cxGetResourceString(@sdxGanttControlTaskDependencyDialogTo),
    Format(cxGetResourceString(@sdxGanttControlTaskID), [FTaskViewInfo.Task.ID]
    ), FTaskViewInfo.Task.Name]);
  Result := Format('%s'#13#10'%s'#13#10'%s', [Result, AFrom, ATo]);
end;

function TdxGanttControlViewChartPredecessorLinkViewInfo.GetStartPoint: TPoint;
begin
  Result.Y := (FPredecessorViewInfo.BarBounds.Top +
    FPredecessorViewInfo.BarBounds.Bottom) div 2;
  if UseRightToLeftAlignment xor (GetLinkType
    in [TdxGanttControlTaskPredecessorLinkType.FF,
    TdxGanttControlTaskPredecessorLinkType.FS]) then
    Result.X := FPredecessorViewInfo.BarBounds.Right -
      IfThen(FPredecessorViewInfo.Task.Milestone, 1, 0)
  else
    Result.X := FPredecessorViewInfo.BarBounds.Left - 1 +
      IfThen(FPredecessorViewInfo.Task.Milestone, 1, 0);
end;

function TdxGanttControlViewChartPredecessorLinkViewInfo.GetStartLineFinishPoint
  (const AStartPoint: TPoint): TPoint;
begin
  Result := cxPointOffset(AStartPoint, ScaleFactor.Apply(LinkLineStartOffset) +
    IfThen(FPredecessorViewInfo.Task.Milestone, 1, 0), 0,
    UseRightToLeftAlignment xor (GetLinkType
    in [TdxGanttControlTaskPredecessorLinkType.FF,
    TdxGanttControlTaskPredecessorLinkType.FS]))
end;

procedure TdxGanttControlViewChartPredecessorLinkViewInfo.CalculateArrow;

  function GetArrowRect(ADirection: TcxArrowDirection): TRect;
  var
    P: TPoint;
  begin
    P := FPoints[Length(FPoints) - 1];
    Result := TRect.Null;
    if ADirection in [adUp, adDown] then
    begin
      Result.Width := ScaleFactor.Apply(LinkArrowWidth) * 2;
      Result.Height := ScaleFactor.Apply(LinkArrowWidth);
      Result.MoveToLeft(P.X - Result.Width div 2 + 1);
    end
    else
    begin
      Result.Width := ScaleFactor.Apply(LinkArrowWidth);
      Result.Height := ScaleFactor.Apply(LinkArrowWidth) * 2;
      Result.MoveToBottom(P.Y + Result.Height div 2);
    end;

    case ADirection of
      adUp:
        Result.MoveToTop(P.Y);
      adDown:
        Result.MoveToBottom(P.Y + 1);
      adLeft:
        Result.MoveToLeft(P.X);
      adRight:
        Result.MoveToRight(P.X + 1);
    end;
  end;

var
  R: TRect;
  ADirection: TcxArrowDirection;
begin
  ADirection := GetArrowDirection;
  R := GetArrowRect(ADirection);
  LookAndFeelPainter.CalculateArrowPoints(R, FArrowPoints, ADirection, True,
    ScaleFactor.Apply(LinkLineStartOffset - LinkLineWidth + GetWidth));
end;

function TdxGanttControlViewChartPredecessorLinkViewInfo.GetArrowDirection
  : TcxArrowDirection;
var
  P, P1: TPoint;
begin
  P := FPoints[Length(FPoints) - 1];
  P1 := FPoints[Length(FPoints) - 2];
  if P1.X = P.X then
  begin
    if P1.Y > P.Y then
      Result := adUp
    else
      Result := adDown;
  end
  else
  begin
    if P1.X > P.X then
      Result := adLeft
    else
      Result := adRight;
  end;
end;

function TdxGanttControlViewChartPredecessorLinkViewInfo.GetColor: TColor;
begin
  Result := cxGetActualColor(FPredecessorViewInfo.Color,
    LookAndFeelPainter.DefaultContentTextColor);
end;

function TdxGanttControlViewChartPredecessorLinkViewInfo.GetWidth: Integer;
begin
  Result := ScaleFactor.Apply(LinkLineWidth);
  if IsHotState then
    Result := Result + ScaleFactor.Apply(2);
end;

function TdxGanttControlViewChartPredecessorLinkViewInfo.HasHint: Boolean;
begin
  Result := True;
end;

function TdxGanttControlViewChartPredecessorLinkViewInfo.
  HasHotTrackState: Boolean;
begin
  Result := True;
end;

function TdxGanttControlViewChartPredecessorLinkViewInfo.IsHotState: Boolean;
begin
  Result := (State = cxbsHot) and not Owner.Controller.DragHelper.IsDragging;
end;

function TdxGanttControlViewChartPredecessorLinkViewInfo.GetOwner
  : TdxGanttControlViewChartAreaViewInfo;
begin
  Result := TdxGanttControlViewChartAreaViewInfo(inherited Owner);
end;

{ TdxChartViewSplitterController }

procedure TdxChartViewSplitterController.ApplySize(const P: TPoint);
var
  AWidth: Integer;
begin
  if ViewInfo.UseRightToLeftAlignment then
    AWidth := ViewInfo.Owner.Bounds.Right - P.X
  else
    AWidth := P.X - ViewInfo.Owner.Bounds.Left;
  ParentController.View.OptionsSheet.Width := ScaleFactor.Revert(AWidth);
end;

function TdxChartViewSplitterController.CalculateDragPoint
  (const P: TPoint): TPoint;
var
  AParentViewInfo: TdxGanttControlViewChartViewInfo;
begin
  Result := inherited CalculateDragPoint(P);
  AParentViewInfo := TdxGanttControlViewChartViewInfo(ViewInfo.Owner);
  if AParentViewInfo.SheetViewInfo <> nil then
  begin
    if ViewInfo.UseRightToLeftAlignment then
      Result.X := Max(AParentViewInfo.SheetViewInfo.Bounds.Right -
        AParentViewInfo.GetMaxSheetWidth, Result.X)
    else
      Result.X := Min(AParentViewInfo.SheetViewInfo.Bounds.Left +
        AParentViewInfo.GetMaxSheetWidth, Result.X)
  end;
end;

constructor TdxChartViewSplitterController.Create(AParentController
  : TdxGanttControlChartViewController);
begin
  inherited Create(AParentController.Control,
    TdxGanttControlChartViewAccess(AParentController.View).OptionsSplitter);
  FParentController := AParentController;
end;

function TdxChartViewSplitterController.GetViewInfo
  : TdxGanttControlCustomItemViewInfo;
begin
  Result := ParentController.ViewInfo.SplitterViewInfo;
end;

{ TdxGanttControlViewChartPredecessorLinkFSViewInfo }

procedure TdxGanttControlViewChartPredecessorLinkFSViewInfo.DoCalculate
  (const AStartPoint, AFinishPoint: TPoint; var AIndex: Integer);
var
  P: TPoint;
begin
  P := AFinishPoint;
  if TaskViewInfo.Task.Milestone then
    P := cxPointOffset(P, TaskViewInfo.GetMilestoneSize.cx div 2, 0,
      not UseRightToLeftAlignment);
  if not UseRightToLeftAlignment xor (AStartPoint.X +
    IfThen(UseRightToLeftAlignment, 1, -1) * ScaleFactor.Apply
    (LinkLineStartOffset) >= P.X) then
  begin
    if Abs(AStartPoint.X - P.X) < ScaleFactor.Apply(LinkLineStartOffset) then
      P := GetStartLineFinishPoint(AStartPoint)
    else
    begin
      P := AStartPoint;
      P.X := AFinishPoint.X;
    end;
    AddPoint(P, AIndex);
    if P.Y > AFinishPoint.Y then
      P.Y := TaskViewInfo.BarBounds.Bottom
    else
      P.Y := TaskViewInfo.BarBounds.Top - 1;
    AddPoint(P, AIndex);
  end
  else
  begin
    P := GetStartLineFinishPoint(AStartPoint);
    AddPoint(P, AIndex);
    if P.Y > AFinishPoint.Y then
      P.Y := PredecessorViewInfo.Bounds.Top
    else
      P.Y := PredecessorViewInfo.Bounds.Bottom;
    AddPoint(P, AIndex);
    P.X := AFinishPoint.X;
    P := cxPointOffset(P, ScaleFactor.Apply(LinkLineFinishOffset), 0,
      UseRightToLeftAlignment);
    AddPoint(P, AIndex);
    P.Y := AFinishPoint.Y;
    AddPoint(P, AIndex);
    AddPoint(AFinishPoint, AIndex);
  end;
end;

function TdxGanttControlViewChartPredecessorLinkFSViewInfo.GetLinkType
  : TdxGanttControlTaskPredecessorLinkType;
begin
  Result := TdxGanttControlTaskPredecessorLinkType.FS;
end;

{ TdxGanttControlChartViewAreaController }

constructor TdxGanttControlChartViewAreaController.Create(AParentController
  : TdxGanttControlChartViewController);
begin
  inherited Create(AParentController.Control);
  FParentController := AParentController;
  FScrollBars := TdxChartViewAreaScrollBars.Create(Self);
  FMinorHeaderWidth := TdxGanttControlChartViewMinorHeaderWidth.Normal;
end;

destructor TdxGanttControlChartViewAreaController.Destroy;
begin
  FreeAndNil(FScrollBars);
  inherited Destroy;
end;

function TdxGanttControlChartViewAreaController.CreateDragHelper
  : TdxGanttControlDragHelper;
begin
  Result := TdxChartDragHelper.Create(Self);
end;

function TdxGanttControlChartViewAreaController.GetGestureClient
  (const APoint: TPoint): IdxGestureClient;
begin
  HitTest.Calculate(APoint.X, APoint.Y);
  if HitTest.HitObject is TdxGanttControlViewChartTaskViewInfo then
    Result := nil
  else
    Result := FScrollBars;
end;

function TdxGanttControlChartViewAreaController.GetLastVisibleDateTime
  : TDateTime;
begin
  if ViewInfo.HeaderViewInfo.MinorHeaders.Count > 0 then
    Result := ViewInfo.HeaderViewInfo.MinorHeaders.Last.DateTime
  else
    Result := FirstVisibleDateTime;
end;

function TdxGanttControlChartViewAreaController.GetViewInfo
  : TdxGanttControlCustomItemViewInfo;
begin
  Result := ParentController.ViewInfo.ChartAreaViewInfo;
end;

function TdxGanttControlChartViewAreaController.IsPanArea
  (const APoint: TPoint): Boolean;
begin
  Result := PtInRect(ViewInfo.ClientRect, APoint);
end;

function TdxGanttControlChartViewAreaController.CanAutoScroll
  (ADirection: TcxDirection): Boolean;
begin
  Result := True;
end;

function TdxGanttControlChartViewAreaController.GetDataProvider
  : TdxGanttControlChartViewDataProvider;
begin
  Result := ParentController.DataProvider;
end;

procedure TdxGanttControlChartViewAreaController.DoCreateScrollBars;
begin
  inherited DoCreateScrollBars;
  ScrollBars.DoCreateScrollBars;
end;

procedure TdxGanttControlChartViewAreaController.DoDblClick;
var
  ALinkInfo: TdxGanttControlViewChartPredecessorLinkViewInfo;
  ATask: TdxGanttControlTask;
begin
  inherited DoDblClick;
  if HitTest.HitObject is TdxGanttControlViewChartPredecessorLinkViewInfo then
  begin
    ALinkInfo := TdxGanttControlViewChartPredecessorLinkViewInfo
      (HitTest.HitObject);
    ShowTaskDependencyDialog(Control, ALinkInfo.Link);
  end;
  if HitTest.HitObject is TdxGanttControlViewChartTaskViewInfo then
  begin
    ATask := TdxGanttControlViewChartTaskViewInfo(HitTest.HitObject).Task;
    ShowTaskInformationDialog(FParentController, ATask);
  end;
end;

procedure TdxGanttControlChartViewAreaController.DoDestroyScrollBars;
begin
  ScrollBars.DoDestroyScrollBars;
  inherited DoDestroyScrollBars;
end;

procedure TdxGanttControlChartViewAreaController.DoKeyDown(var Key: Word;
  Shift: TShiftState);
begin
  inherited DoKeyDown(Key, Shift);
  if Key = 0 then
    Exit;
  case Key of
    VK_LEFT:
      ScrollBars.DoHScroll(scLineUp, ScrollBars.FHPosition);
    VK_RIGHT:
      ScrollBars.DoHScroll(scLineDown, ScrollBars.FHPosition);
    VK_HOME:
      begin
        ParentController.FocusedRowIndex := 0;
        MakeFocusedRowVisible;
      end;
    VK_END:
      begin
        ParentController.FocusedRowIndex := Max(0, DataProvider.Count - 1);
        MakeFocusedRowVisible;
      end;
    VK_UP, VK_DOWN:
      begin
        ParentController.FocusedRowIndex :=
          Max(0, ParentController.FocusedRowIndex +
          IfThen(Key = VK_DOWN, 1, -1));
        MakeFocusedRowVisible;
      end;
    VK_PRIOR, VK_NEXT:
      begin
        ParentController.FocusedRowIndex :=
          Max(0, ParentController.FocusedRowIndex + IfThen(Key = VK_NEXT, 1, -1)
          * (ViewInfo.FullyVisibleTaskCount - 1));
        MakeFocusedRowVisible;
      end;
    VK_DELETE:
      begin
        MakeFocusedRowVisible;
        ParentController.SheetController.DeleteFocusedItem;
      end;
    VK_INSERT:
      begin
        MakeFocusedRowVisible;
        ParentController.SheetController.InsertNewDataItem;
      end;
    VK_RETURN:
      if Shift = [ssCtrl] then
      begin
        MakeFocusedRowVisible;
        ParentController.SheetController.ShowTaskInformationDialog
          (ViewInfo.Tasks[ParentController.FocusedRowIndex -
          ParentController.FirstVisibleRowIndex].Task);
      end;
  end;
end;

function TdxGanttControlChartViewAreaController.GetTouchScrollUIOwner
  (const APoint: TPoint): IdxTouchScrollUIOwner;
begin
  Result := ScrollBars;
end;

procedure TdxGanttControlChartViewAreaController.InitScrollbars;
begin
  inherited InitScrollbars;
  ScrollBars.InitScrollbars;
end;

procedure TdxGanttControlChartViewAreaController.UnInitScrollbars;
begin
  ScrollBars.UnInitScrollbars;
  inherited UnInitScrollbars;
end;

function TdxGanttControlChartViewAreaController.InternalGetViewInfo
  : TdxGanttControlViewChartAreaViewInfo;
begin
  Result := TdxGanttControlViewChartAreaViewInfo(inherited ViewInfo);
end;

procedure TdxGanttControlChartViewAreaController.SetFocusedRow
  (ATask: TdxGanttControlViewChartTaskViewInfo);
var
  P: TPoint;
begin
  P := ParentController.SheetController.FocusedCell;
  P.Y := ATask.Owner.Tasks.IndexOf(ATask) +
    ParentController.FirstVisibleRowIndex;
  P.X := -1;
  ParentController.SheetController.FocusedCell := P;
end;

procedure TdxGanttControlChartViewAreaController.SetMinorHeaderWidth
  (const Value: TdxGanttControlChartViewMinorHeaderWidth);
begin
  if FMinorHeaderWidth <> Value then
  begin
    ScrollBars.ShowTouchScrollUI;
    FMinorHeaderWidth := Value;
    if ViewInfo <> nil then
      ViewInfo.Reset;
    TdxGanttControlChartViewAccess(ParentController.View)
      .Changed([TdxGanttControlOptionsChangedType.Size]);
  end;
end;

procedure TdxGanttControlChartViewAreaController.DoClick;
begin
  // do nothing
end;

procedure TdxGanttControlChartViewAreaController.DoMouseDown
  (Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited DoMouseDown(Button, Shift, X, Y);
  if Button = mbMiddle then
    ScrollBars.ProcessControlScrollingOnMiddleButton;
  if ParentController.SheetController.EditingController.IsEditing then
  begin
    ParentController.SheetController.EditingController.HideEdit(True);
    HitTest.Recalculate;
  end;
  if HitTest.HitObject is TdxGanttControlViewChartTaskViewInfo then
    SetFocusedRow(TdxGanttControlViewChartTaskViewInfo(HitTest.HitObject));
  if (Button = mbRight) and
    (HitTest.HitObject is TdxGanttControlViewChartAreaViewInfo) then
    SetFocusedRow(TdxGanttControlViewChartAreaViewInfo(HitTest.HitObject)
      .GetTaskRowViewInfo(TPoint.Create(X, Y)));
end;

procedure TdxGanttControlChartViewAreaController.DoMouseMove(Shift: TShiftState;
  X, Y: Integer);
begin
  inherited DoMouseMove(Shift, X, Y);
end;

procedure TdxGanttControlChartViewAreaController.MakeFocusedRowVisible;
var
  AFirstVisibleRowIndex: Integer;
  ACount: Integer;
begin
  AFirstVisibleRowIndex := ParentController.FirstVisibleRowIndex;
  ACount := ViewInfo.Tasks.Count;
  if ViewInfo.Tasks.Last.Bounds.Bottom > ViewInfo.ClientRect.Bottom then
    Dec(ACount);
  if ParentController.FocusedRowIndex < AFirstVisibleRowIndex then
    AFirstVisibleRowIndex := Max(0, ParentController.FocusedRowIndex)
  else if ParentController.FocusedRowIndex >= AFirstVisibleRowIndex + ACount
  then
    AFirstVisibleRowIndex := ParentController.FocusedRowIndex - ACount + 1;

  if (AFirstVisibleRowIndex <> ParentController.FirstVisibleRowIndex) then
    ParentController.FirstVisibleRowIndex := AFirstVisibleRowIndex;
end;

function TdxGanttControlChartViewAreaController.ProcessNCSizeChanged: Boolean;
begin
  Result := ScrollBars.NCSizeChanged;
end;

function TdxGanttControlChartViewAreaController.DoMouseWheel(Shift: TShiftState;
  AIsIncrement: Boolean; const AMousePos: TPoint): Boolean;
var
  ATimescaleFactor: Integer;
  ADateTime: TDateTime;
  AWidth: Integer;
  P: TPoint;
  AFirstVisibleDateTime: TDateTime;
begin
  if Shift = [ssCtrl] then
  begin
    P := HitTest.HitPoint;
    ADateTime := ViewInfo.HeaderViewInfo.GetDateTimeByPos(P.X);
    ATimescaleFactor := Ord(FParentController.View.TimescaleUnit) * 3 + 3 -
      Ord(MinorHeaderWidth);
    Result := False;
    if AIsIncrement and
      (ATimescaleFactor < Ord(High(TdxGanttControlChartViewTimescaleUnit)) *
      3 + 2) then
    begin
      Inc(ATimescaleFactor);
      Result := True;
    end
    else if not AIsIncrement and (ATimescaleFactor > 0) then
    begin
      Dec(ATimescaleFactor);
      Result := True;
    end;
    if Result then
    begin
      Control.BeginUpdate;
      try
        ParentController.View.TimescaleUnit :=
          TdxGanttControlChartViewTimescaleUnit(ATimescaleFactor div 3);
        MinorHeaderWidth := TdxGanttControlChartViewMinorHeaderWidth
          (3 - ATimescaleFactor mod 3);
        ViewInfo.HeaderViewInfo.FCachedMinorHeaderWidth := -1;
        AFirstVisibleDateTime := ViewInfo.HeaderViewInfo.GetMinorDateTime
          (ADateTime);
        if ViewInfo.UseRightToLeftAlignment then
          AWidth := ViewInfo.ClientRect.Right - P.X
        else
          AWidth := P.X - ViewInfo.ClientRect.Left;
        AWidth := AWidth - ViewInfo.HeaderViewInfo.CalculateMinorHeaderWidth;
        while AWidth > 0 do
        begin
          AWidth := AWidth - ViewInfo.HeaderViewInfo.CalculateMinorHeaderWidth;
          AFirstVisibleDateTime :=
            ViewInfo.HeaderViewInfo.GetPreviousMinorDateTime
            (AFirstVisibleDateTime);
        end;
        FirstVisibleDateTime := AFirstVisibleDateTime;
      finally
        Control.EndUpdate;
      end;
      ParentController.View.DoTimescaleChanged;
    end;
  end
  else
    Result := ScrollBars.DoMouseWheel(Shift, AIsIncrement);
end;

procedure TdxGanttControlChartViewAreaController.SetFirstVisibleDateTime
  (const Value: TDateTime);
begin
  if FirstVisibleDateTime <> Value then
  begin
    ScrollBars.ShowTouchScrollUI;
    FFirstVisibleDateTime := Value;
    TdxGanttControlChartViewAccess(ParentController.View)
      .Changed([TdxGanttControlOptionsChangedType.View]);
    ParentController.View.DoFirstVisibleDateTimeChanged;
  end;
end;

{ TdxGanttControlChartViewSheetOptions }

procedure TdxGanttControlChartViewSheetOptions.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlChartViewSheetOptions;
begin
  if Source is TdxGanttControlChartViewSheetOptions then
  begin
    ASource := TdxGanttControlChartViewSheetOptions(Source);
    Width := ASource.Width;
    Visible := ASource.Visible;
    Hidden := ASource.Hidden;
  end;
  inherited Assign(Source);
end;

function TdxGanttControlChartViewSheetOptions.CreateColumns
  : TdxGanttControlSheetColumns;
begin
  Result := TdxGanttControlViewChartSheetColumns.Create(Self);
end;

procedure TdxGanttControlChartViewSheetOptions.DoReset;
begin
  inherited DoReset;
  FWidth := DefaultWidth;
  FVisible := True;
  FHidden := False;
end;

procedure TdxGanttControlChartViewSheetOptions.DoSizeChanged;
begin
  if Assigned(OnSizeChanged) then
    OnSizeChanged(Self);
end;

function TdxGanttControlChartViewSheetOptions.GetController
  : TdxGanttControlSheetController;
begin
  if Owner.Controller = nil then
    Result := nil
  else
    Result := Owner.Controller.SheetController;
end;

function TdxGanttControlChartViewSheetOptions.GetDataProvider
  : TdxGanttControlSheetCustomDataProvider;
begin
  Result := TdxGanttControlChartViewDataProvider(Owner.DataProvider);
end;

function TdxGanttControlChartViewSheetOptions.GetOwnerComponent: TComponent;
begin
  Result := Owner.Owner;
end;

function TdxGanttControlChartViewSheetOptions.GetScaleFactor: TdxScaleFactor;
begin
  Result := Owner.ScaleFactor;
end;

function TdxGanttControlChartViewSheetOptions.InternalGetController
  : TdxGanttControlChartViewSheetController;
begin
  Result := TdxGanttControlChartViewSheetController(inherited Controller);
end;

function TdxGanttControlChartViewSheetOptions.InternalGetOwner
  : TdxGanttControlChartView;
begin
  Result := TdxGanttControlChartView(inherited Owner);
end;

procedure TdxGanttControlChartViewSheetOptions.SetWidth(const Value: Integer);
begin
  if Width <> Value then
  begin
    FWidth := Value;
    DoSizeChanged;
    Changed([TdxGanttControlOptionsChangedType.Size]);
  end;
end;

procedure TdxGanttControlChartViewSheetOptions.SetHidden(const Value: Boolean);
begin
  if FHidden <> Value then
  begin
    FHidden := Value;
    Changed([TdxGanttControlOptionsChangedType.Size]);
  end;
end;

procedure TdxGanttControlChartViewSheetOptions.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    if FVisible then
      Controller.Activated
    else
      Controller.Deactivated;
    Changed([TdxGanttControlOptionsChangedType.Layout]);
  end;
end;

{ TdxChartViewAreaScrollBars }

constructor TdxChartViewAreaScrollBars.Create(AController
  : TdxGanttControlCustomController);
begin
  inherited;
  FHPosition := 5;
end;

function TdxChartViewAreaScrollBars.NeedPanningFeedback
  (AScrollKind: TScrollBarKind): Boolean;
begin
  Result := AScrollKind = sbVertical;
end;

procedure TdxChartViewAreaScrollBars.DoHScroll(ScrollCode: TScrollCode;
  var ScrollPos: Integer);
var
  ADateTime: TDateTime;
  APosition: Integer;
begin
  ADateTime := Controller.FirstVisibleDateTime;
  APosition := ScrollPos;
  case ScrollCode of
    scTrack:
      if (APosition > 0) and (APosition < (HScrollBar.Max - HScrollBar.PageSize))
      then
        ADateTime := Controller.ViewInfo.HeaderViewInfo.GetNextMinorDateTime
          (ADateTime, APosition - FHPosition);
    scPageUp:
      begin
        ADateTime := Controller.ViewInfo.HeaderViewInfo.GetNextMinorDateTime
          (ADateTime, -Controller.ViewInfo.HScrollBarPageSize);
        Inc(APosition, -Controller.ViewInfo.HScrollBarPageSize);
      end;
    scPageDown:
      begin
        ADateTime := Controller.ViewInfo.HeaderViewInfo.GetNextMinorDateTime
          (ADateTime, Controller.ViewInfo.HScrollBarPageSize);
        Inc(APosition, Controller.ViewInfo.HScrollBarPageSize);
      end;
    scLineUp:
      begin
        ADateTime := Controller.ViewInfo.HeaderViewInfo.GetPreviousMinorDateTime
          (ADateTime);
        Dec(APosition);
      end;
    scLineDown:
      begin
        ADateTime := Controller.ViewInfo.HeaderViewInfo.GetNextMinorDateTime
          (ADateTime);
        Inc(APosition);
      end;
  end;
  Helper.LockScrollBars;
  try
    ADateTime := Max(ADateTime, TdxGanttControlDataModel.MinDate);
    FHPosition := APosition;
    if (ScrollCode <> scTrack) or TdxCustomGanttControlAccess(GetOwnerControl).IsGestureScrolling
    then
    begin
      FHPosition := Min(HScrollBar.Max - HScrollBar.PageSize - 5, FHPosition);
      if ADateTime > Controller.ViewInfo.HeaderViewInfo.GetNextMinorDateTime
        (TdxGanttControlDataModel.MinDate, 5) then
        FHPosition := Max(5, FHPosition);
    end;
    Controller.FirstVisibleDateTime := ADateTime;
    ScrollPos := FHPosition;
  finally
    Helper.UnlockScrollBars;
  end;
end;

procedure TdxChartViewAreaScrollBars.DoInitHScrollBarParameters;
var
  AMin, AMax, APageSize, APosition: Integer;
begin
  AMin := 0;
  APageSize := Controller.ViewInfo.HScrollBarPageSize;
  AMax := APageSize * 5;
  APosition := FHPosition;
  SetScrollInfo(sbHorizontal, AMin, AMax, 1, APageSize, APosition, True, True);
  HScrollBar.UnlimitedTracking := True;
end;

procedure TdxChartViewAreaScrollBars.DoInitVScrollBarParameters;
var
  AMin, AMax, APageSize, APosition: Integer;
begin
  AMin := 0;
  AMax := Controller.ParentController.ViewInfo.TotalRowCount;
  APageSize := Controller.ParentController.ViewInfo.VisibleRowCount;
  APosition := Controller.ParentController.FirstVisibleRowIndex;
  SetScrollInfo(sbVertical, AMin, AMax, 1, APageSize, APosition, True, True);
  VScrollBar.UnlimitedTracking := True;
end;

procedure TdxChartViewAreaScrollBars.DoVScroll(ScrollCode: TScrollCode;
  var ScrollPos: Integer);
begin
  Controller.ParentController.SheetController.ScrollBars.DoVScroll(ScrollCode,
    ScrollPos);
end;

function TdxChartViewAreaScrollBars.IsUnlimitedScrolling
  (AScrollKind: TScrollBarKind; ADeltaX, ADeltaY: Integer): Boolean;
begin
  Result := (AScrollKind = sbHorizontal) or (AScrollKind = sbVertical) and
    (ADeltaY < 0);
end;

function TdxChartViewAreaScrollBars.GetController
  : TdxGanttControlChartViewAreaController;
begin
  Result := TdxGanttControlChartViewAreaController(inherited Controller);
end;

{ TdxGanttControlChartViewDataProvider }

constructor TdxGanttControlChartViewDataProvider.Create
  (AControl: TdxGanttControlBase);
begin
  inherited Create(AControl);
  FTaskDictionary := TDictionary<Integer, TdxGanttControlTask>.Create;
  FDataModel := TdxCustomGanttControl(AControl).DataModel;
  FDataModel.Tasks.ListChangedHandlers.Add(TasksChangedHandler);
  FDataModel.Tasks.BeforeResetHandlers.Add(TasksBeforeResetHandler);
end;

destructor TdxGanttControlChartViewDataProvider.Destroy;
begin
  FDataModel.Tasks.BeforeResetHandlers.Remove(TasksBeforeResetHandler);
  FDataModel.Tasks.ListChangedHandlers.Remove(TasksChangedHandler);
  FreeAndNil(FTaskDictionary);
  inherited Destroy;
end;

function TdxGanttControlChartViewDataProvider.GetTaskByUID(const AUID: Integer)
  : TdxGanttControlTask;
begin
  if not FTaskDictionary.TryGetValue(AUID, Result) then
    Result := nil;
end;

function TdxGanttControlChartViewDataProvider.CanExpand(AItem: TObject)
  : Boolean;
begin
  Result := inherited CanExpand(AItem) and TdxGanttControlTask(AItem).Summary;
end;

procedure TdxGanttControlChartViewDataProvider.ClearItems;
begin
  inherited ClearItems;
  FTaskDictionary.Clear;
end;

function TdxGanttControlChartViewDataProvider.CanAddItem
  (AItem: TObject): Boolean;
var
  ATask: TdxGanttControlTask;
begin
  ATask := TdxGanttControlTask(AItem);
  Result := not(ATask.Summary and (ATask.UID = 0) and (ATask.OutlineLevel = 0)
    and (ATask.ID = 0));
end;

function TdxGanttControlChartViewDataProvider.CanCollapse
  (AItem: TObject): Boolean;
begin
  Result := inherited CanCollapse(AItem) and TdxGanttControlTask(AItem).Summary;
end;

procedure TdxGanttControlChartViewDataProvider.InitializeNewTask
  (ATask: TdxGanttControlTask);
var
  AIndex: Integer;
begin
  AIndex := ATask.ID;
  ATask.Blank := True;
  if DataModel.Tasks[AIndex - 1].Summary and
    IsExpanded(DataModel.Tasks[AIndex - 1]) then
    ATask.OutlineLevel := DataModel.Tasks[AIndex - 1].OutlineLevel + 1
  else
    ATask.OutlineLevel := DataModel.Tasks[AIndex - 1].OutlineLevel;
end;

procedure TdxGanttControlChartViewDataProvider.InternalAppendItem;
var
  ATask: TdxGanttControlTask;
begin
  ATask := DataModel.Tasks.Append;
  InitializeNewTask(ATask);
end;

procedure TdxGanttControlChartViewDataProvider.InternalInsertNewItem
  (AIndex: Integer);
var
  ATask: TdxGanttControlTask;
begin
  ATask := TdxGanttControlTask(TdxGanttControlElementCustomListAccess
    (DataModel.Tasks).CreateItem);
  InternalInsertItem(AIndex, ATask);
  InitializeNewTask(ATask);
end;

procedure TdxGanttControlChartViewDataProvider.ItemRemoved(AItem: TObject);
begin
  inherited ItemRemoved(AItem);
  FTaskDictionary.Remove(TdxGanttControlTask(AItem).UID);
end;

procedure TdxGanttControlChartViewDataProvider.InternalInsertItem
  (AIndex: Integer; ADataItem: TObject);
begin
  TdxGanttControlElementCustomListAccess(DataModel.Tasks).InternalInsert(AIndex,
    TdxGanttControlModelElementListItem(ADataItem));
end;

procedure TdxGanttControlChartViewDataProvider.InternalExtractItem
  (AIndex: Integer);
begin
  TdxGanttControlElementCustomListAccess(DataModel.Tasks)
    .InternalExtract(DataItems[AIndex]);
end;

procedure TdxGanttControlChartViewDataProvider.InternalExtractLastItem;
var
  ATask: TdxGanttControlTask;
begin
  ATask := DataModel.Tasks[DataModel.Tasks.Count - 1];
  TdxGanttControlElementCustomListAccess(DataModel.Tasks)
    .InternalExtract(ATask);
end;

function TdxGanttControlChartViewDataProvider.GetDataItemCount: Integer;
begin
  Result := DataModel.Tasks.Count;
end;

function TdxGanttControlChartViewDataProvider.GetDataItem
  (Index: Integer): TObject;
begin
  Result := DataModel.Tasks[Index];
end;

function TdxGanttControlChartViewDataProvider.GetDataItemIndex
  (ADataItem: TObject): Integer;
begin
  Result := TdxGanttControlTask(ADataItem).ID;
end;

function TdxGanttControlChartViewDataProvider.GetTask(AIndex: Integer)
  : TdxGanttControlTask;
begin
  Result := TdxGanttControlTask(inherited Items[AIndex]);
end;

function TdxGanttControlChartViewDataProvider.GetRowHeaderCaption
  (AData: TObject): string;
begin
  if AData = nil then
    Result := inherited GetRowHeaderCaption(AData)
  else
    Result := IntToStr(TdxGanttControlTask(AData).ID);
end;

function TdxGanttControlChartViewDataProvider.InternalGetDataItem
  (Index: Integer): TdxGanttControlTask;
begin
  Result := TdxGanttControlTask(inherited DataItems[Index]);
end;

procedure TdxGanttControlChartViewDataProvider.DoPopulate;
var
  I: Integer;
  ACollapsedLevel: Integer;
  ATask: TdxGanttControlTask;
begin
  FTaskDictionary.Clear;
  ACollapsedLevel := -1;
  for I := 0 to DataItemCount - 1 do
  begin
    ATask := DataItems[I];
    if ACollapsedLevel = -1 then
    begin
      if not IsExpanded(ATask) then
        ACollapsedLevel := ATask.OutlineLevel;
    end
    else if ATask.OutlineLevel <= ACollapsedLevel then
      if not IsExpanded(ATask) then
        ACollapsedLevel := ATask.OutlineLevel
      else
        ACollapsedLevel := -1;
    if ((ACollapsedLevel = -1) or (ACollapsedLevel = ATask.OutlineLevel)) and
      CanAddItem(ATask) then
    begin
      InnerList.Add(ATask);
      FTaskDictionary.Add(ATask.UID, ATask);
    end;
  end;
end;

procedure TdxGanttControlChartViewDataProvider.TasksBeforeResetHandler
  (Sender: TObject);
begin
  ClearItems;
end;

procedure TdxGanttControlChartViewDataProvider.TasksChangedHandler
  (Sender: TObject; const AItem: TdxGanttControlModelElementListItem;
  AAction: TCollectionNotification);
begin
  if AAction in [cnRemoved, cnExtracted] then
    ItemRemoved(AItem);
end;

{ TdxChartDragHelper }

function TdxChartDragHelper.CreateDragAndDropObject
  : TdxGanttControlDragAndDropObject;
begin
  if FConvertedObject <> nil then
    Result := CreateLinkingObject(FConvertedObject)
  else
    Result := CreateDragAndDropObjectByPoint(HitTest.HitPoint);
end;

function TdxChartDragHelper.StartDragAndDrop(const P: TPoint): Boolean;
var
  ADragObject: TdxGanttControlDragAndDropObject;
begin
  ADragObject := CreateDragAndDropObjectByPoint(P);
  try
    Result := ADragObject <> nil;
  finally
    ADragObject.Free;
  end;
end;

function TdxChartDragHelper.CreateDragAndDropObjectByPoint(const P: TPoint)
  : TdxGanttControlDragAndDropObject;
var
  ATaskViewInfo: TdxGanttControlViewChartTaskViewInfo;
  I: Integer;
begin
  for I := 0 to Controller.ViewInfo.Tasks.Count - 1 do
  begin
    ATaskViewInfo := Controller.ViewInfo.Tasks[I];
    if (ATaskViewInfo is TdxGanttControlViewChartEmptyTaskViewInfo) and
      ATaskViewInfo.Bounds.Contains(P) then
      Exit(CreateNewTaskCreatingObject(ATaskViewInfo));
    if ATaskViewInfo.IsSizingZone(P) then
      Exit(CreateResizingObject(ATaskViewInfo));
    if ATaskViewInfo.IsCompleteZone(P) then
      Exit(CreateChangePercentCompleteObject(ATaskViewInfo));
    if ATaskViewInfo.IsMovingZone(P) then
      Exit(CreateMovingObject(ATaskViewInfo));
  end;
  Result := nil;
end;

procedure TdxChartDragHelper.ConvertToLinkingObject(AMovingObject
  : TdxChartTaskMovingObject);
begin
  FConvertedObject := AMovingObject.Clone;
  try
    Controller.Control.FinishDragAndDrop(False);
    Controller.Control.BeginDragAndDrop;
  finally
    FreeAndNil(FConvertedObject);
  end;
end;

function TdxChartDragHelper.CanScroll: Boolean;
begin
  Result := inherited CanScroll;
  if Result and (DragAndDropObject <> nil) and
    (DragAndDropObject is TdxChartTaskDragObject) then
    Result := TdxChartTaskDragObject(DragAndDropObject)
      .CanScroll(ScrollDirection);
end;

function TdxChartDragHelper.CreateChangePercentCompleteObject
  (AViewInfo: TdxGanttControlViewChartTaskViewInfo)
  : TdxChartTaskChangeProgressObject;
begin
  Result := TdxChartTaskChangeProgressObject.Create(Controller, AViewInfo);
end;

function TdxChartDragHelper.CreateLinkingObject(AMovingObject
  : TdxChartTaskMovingObject): TdxChartTaskLinkingObject;
begin
  Result := TdxChartTaskLinkingObject.Create(AMovingObject);
end;

function TdxChartDragHelper.CreateMovingObject
  (AViewInfo: TdxGanttControlViewChartTaskViewInfo): TdxChartTaskMovingObject;
begin
  Result := TdxChartTaskMovingObject.Create(Controller, AViewInfo);
end;

function TdxChartDragHelper.CreateNewTaskCreatingObject
  (AViewInfo: TdxGanttControlViewChartTaskViewInfo)
  : TdxChartNewTaskCreatingObject;
begin
  Result := TdxChartNewTaskCreatingObject.Create(Controller, AViewInfo);
end;

function TdxChartDragHelper.CreateResizingObject
  (AViewInfo: TdxGanttControlViewChartTaskViewInfo): TdxChartTaskResizingObject;
begin
  Result := TdxChartTaskResizingObject.Create(Controller, AViewInfo);
end;

procedure TdxChartDragHelper.DoScroll;
var
  ADateTime: TDateTime;
  AIndex: Integer;
begin
  ADateTime := Controller.FirstVisibleDateTime;
  AIndex := Controller.ParentController.FirstVisibleRowIndex;
  case ScrollDirection of
    dirLeft:
      ADateTime := Controller.ViewInfo.HeaderViewInfo.GetPreviousMinorDateTime
        (ADateTime);
    dirRight:
      ADateTime := Controller.ViewInfo.HeaderViewInfo.GetNextMinorDateTime
        (ADateTime);
    dirUp:
      Dec(AIndex);
    dirDown:
      Inc(AIndex);
  end;
  Controller.ParentController.FirstVisibleRowIndex := AIndex;
  Controller.FirstVisibleDateTime := ADateTime;
end;

function TdxChartDragHelper.GetController
  : TdxGanttControlChartViewAreaController;
begin
  Result := TdxGanttControlChartViewAreaController(inherited Controller);
end;

function TdxChartDragHelper.GetScrollableArea: TRect;
begin
  if (DragAndDropObject <> nil) and (DragAndDropObject is TdxChartTaskDragObject)
  then
    Result := TdxChartTaskDragObject(DragAndDropObject).GetScrollableArea
  else
    Result := inherited GetScrollableArea;
end;

{ TdxGanttControlChartViewController }

constructor TdxGanttControlChartViewController.Create
  (AView: TdxGanttControlCustomView);
begin
  inherited Create(AView);
  FChartAreaController := CreateChartAreaController;
  FSheetController := CreateSheetController;
  FSplitterController := CreateSplitterController;
end;

destructor TdxGanttControlChartViewController.Destroy;
begin
  FreeAndNil(FSplitterController);
  FreeAndNil(FChartAreaController);
  FreeAndNil(FSheetController);
  inherited Destroy;
end;

procedure TdxGanttControlChartViewController.Activated;
begin
  inherited Activated;
  SheetController.Activated;
  ChartAreaController.Activated;
end;

procedure TdxGanttControlChartViewController.Deactivated;
begin
  inherited Deactivated;
  if View.OptionsSheet.Visible then
    SheetController.Deactivated;
  ChartAreaController.Deactivated;
end;

procedure TdxGanttControlChartViewController.DoCreateScrollBars;
begin
  inherited DoCreateScrollBars;
  if View.OptionsSheet.Visible then
    SheetController.DoCreateScrollBars;
  ChartAreaController.DoCreateScrollBars;
end;

procedure TdxGanttControlChartViewController.DoDestroyScrollBars;
begin
  inherited DoDestroyScrollBars;
  ChartAreaController.DoDestroyScrollBars;
  SheetController.DoDestroyScrollBars;
end;

function TdxGanttControlChartViewController.CreateChartAreaController
  : TdxGanttControlChartViewAreaController;
begin
  Result := TdxGanttControlChartViewAreaController.Create(Self);
end;

function TdxGanttControlChartViewController.CreateSheetController
  : TdxGanttControlChartViewSheetController;
begin
  Result := TdxGanttControlChartViewSheetController.Create(Self);
end;

function TdxGanttControlChartViewController.CreateSplitterController
  : TdxChartViewSplitterController;
begin
  Result := TdxChartViewSplitterController.Create(Self);
end;

function TdxGanttControlChartViewController.GetActiveController(const P: TPoint)
  : TdxGanttControlCustomController;
begin
  if CaptureController <> nil then
    Result := CaptureController
  else
    Result := GetControllerByCursorPos(P);
end;

function TdxGanttControlChartViewController.GetControllerByCursorPos
  (const P: TPoint): TdxGanttControlCustomController;
begin
  if (SheetController.ViewInfo <> nil) and
    PtInRect(SheetController.ViewInfo.Bounds, P) then
    Result := SheetController
  else if (SplitterController.ViewInfo <> nil) and
    PtInRect(SplitterController.ViewInfo.Bounds, P) then
    Result := SplitterController
  else
    Result := ChartAreaController;
end;

procedure TdxGanttControlChartViewController.DoKeyDown(var Key: Word;
  Shift: TShiftState);
begin
  inherited DoKeyDown(Key, Shift);
  if Key <> 0 then
    if SheetController.IsActive then
      SheetController.KeyDown(Key, Shift)
    else
      ChartAreaController.KeyDown(Key, Shift);
end;

procedure TdxGanttControlChartViewController.DoKeyPress(var Key: Char);
begin
  inherited DoKeyPress(Key);
  if (Key <> #0) and SheetController.IsActive then
    SheetController.KeyPress(Key);
end;

procedure TdxGanttControlChartViewController.DoKeyUp(var Key: Word;
  Shift: TShiftState);
begin
  inherited DoKeyUp(Key, Shift);
  if (Key <> 0) and SheetController.IsActive then
    SheetController.KeyUp(Key, Shift);
end;

function TdxGanttControlChartViewController.InitializeBuiltInPopupMenu
  (APopupMenu: TdxGanttControlPopupMenu; var P: TPoint): Boolean;
var
  ABounds: TRect;
  AClientBounds: TRect;
begin
  Result := P.IsEqual(cxInvalidPoint) or
    (HitTest.HitObject is TdxGanttControlViewChartTaskViewInfo) or
    (HitTest.HitObject is TdxGanttControlSheetCellViewInfo) or
    (HitTest.HitObject is TdxGanttControlSheetRowHeaderViewInfo) or
    (HitTest.HitObject is TdxGanttControlViewChartAreaViewInfo);
  if P.IsEqual(cxInvalidPoint) then
  begin
    if SheetController.IsActive then
    begin
      SheetController.MakeFocusedCellVisible;
      if SheetController.FocusedCellViewInfo <> nil then
        ABounds := SheetController.FocusedCellViewInfo.Bounds
      else
        ABounds := ViewInfo.SheetViewInfo.DataRows
          [FocusedRowIndex - FirstVisibleRowIndex].HeaderViewInfo.Bounds;
      AClientBounds := SheetController.ViewInfo.ClientRect;
    end
    else
    begin
      ChartAreaController.MakeFocusedRowVisible;
      ABounds := ChartAreaController.ViewInfo.Tasks
        [FocusedRowIndex - FirstVisibleRowIndex].Bounds;
      ABounds.Width := 0;
      if ViewInfo.UseRightToLeftAlignment then
        ABounds.MoveToRight(ChartAreaController.ViewInfo.Tasks
          [FocusedRowIndex - FirstVisibleRowIndex].Bounds.Right);
      AClientBounds := ChartAreaController.ViewInfo.ClientRect;
    end;
    P.Y := (ABounds.Top + ABounds.Bottom) div 2;
    if ViewInfo.UseRightToLeftAlignment then
      P.X := Max(ABounds.Left, AClientBounds.Left)
    else
      P.X := Min(ABounds.Right, AClientBounds.Right);
  end;
  if Result then
  begin
    if not(HitTest.HitObject is TdxGanttControlViewChartTaskViewInfo) then
    begin
      APopupMenu.AddCommand(TdxGanttControlScrollToTaskCommand.Create(Control));
      APopupMenu.AddSeparator;
    end;
    APopupMenu.AddCommand(TdxGanttControlInsertTaskViewCommand.Create(Control),
      VK_INSERT, []);
    APopupMenu.AddCommand(TdxGanttControlDeleteTaskViewCommand.Create(Control),
      VK_DELETE, [ssCtrl]);
    APopupMenu.AddSeparator;
    APopupMenu.AddCommand(TdxGanttControlOpenTaskInformationDialogCommand.Create
      (Control));
    APopupMenu.AddCommand(TdxGanttControlAddTaskToTimelineCommand.Create
      (Control));
  end;
end;

procedure TdxGanttControlChartViewController.InitScrollbars;
begin
  inherited InitScrollbars;
  ChartAreaController.InitScrollbars;
  if View.OptionsSheet.Visible then
    SheetController.InitScrollbars;
end;

function TdxGanttControlChartViewController.IsMouseWheelHandleNeeded
  (const MousePos: TPoint): Boolean;
begin
  Result := not FSheetController.EditingController.IsEditing;
end;

function TdxGanttControlChartViewController.ProcessNCSizeChanged: Boolean;
begin
  Result := False;
  if View.OptionsSheet.Visible then
    Result := SheetController.ProcessNCSizeChanged;
  Result := ChartAreaController.ProcessNCSizeChanged or Result;
end;

function TdxGanttControlChartViewController.InternalGetViewInfo
  : TdxGanttControlViewChartViewInfo;
begin
  Result := TdxGanttControlViewChartViewInfo(inherited ViewInfo);
end;

procedure TdxGanttControlChartViewController.SetFirstVisibleRowIndex
  (const Value: Integer);
begin
  SheetController.FirstVisibleRowIndex := Value;
end;

procedure TdxGanttControlChartViewController.SetFocusedRowIndex
  (const Value: Integer);
var
  P: TPoint;
begin
  P := SheetController.FocusedCell;
  P.Y := Value;
  SheetController.FocusedCell := P;
end;

procedure TdxGanttControlChartViewController.UnInitScrollbars;
begin
  inherited UnInitScrollbars;
  if View.OptionsSheet.Visible then
    SheetController.UnInitScrollbars;
  ChartAreaController.UnInitScrollbars;
end;

function TdxGanttControlChartViewController.GetDataProvider
  : TdxGanttControlChartViewDataProvider;
begin
  Result := TdxGanttControlChartViewDataProvider(inherited DataProvider);
end;

function TdxGanttControlChartViewController.GetFirstVisibleRowIndex: Integer;
begin
  Result := SheetController.FirstVisibleRowIndex;
end;

function TdxGanttControlChartViewController.GetFocusedRowIndex: Integer;
begin
  Result := SheetController.FocusedCell.Y;
end;

function TdxGanttControlChartViewController.GetView: TdxGanttControlChartView;
begin
  Result := TdxGanttControlChartView(inherited View);
end;

procedure TdxGanttControlChartViewController.HideEditing;
begin
  inherited HideEditing;
  FSheetController.HideEditing;
  FChartAreaController.HideEditing;
end;

{ TdxGanttControlGridlineOptions }

constructor TdxGanttControlGridlineOptions.Create
  (AOwner: TdxGanttControlChartView);
begin
  inherited Create;
  FOwner := AOwner;
  FCurrentDate := TdxGanttControlGridline.Create(Self);
  FProjectFinish := TdxGanttControlGridline.Create(Self);
  FProjectStart := TdxGanttControlGridline.Create(Self);
end;

destructor TdxGanttControlGridlineOptions.Destroy;
begin
  FreeAndNil(FCurrentDate);
  FreeAndNil(FProjectFinish);
  FreeAndNil(FProjectStart);
  inherited Destroy;
end;

procedure TdxGanttControlGridlineOptions.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlGridlineOptions;
begin
  if Source is TdxGanttControlGridlineOptions then
  begin
    ASource := TdxGanttControlGridlineOptions(Source);
    CurrentDate := ASource.CurrentDate;
    ProjectFinish := ASource.ProjectFinish;
    ProjectStart := ASource.ProjectStart;
  end;
  inherited Assign(Source);
end;

procedure TdxGanttControlGridlineOptions.DoChanged;
begin
  FOwner.Changed([TdxGanttControlOptionsChangedType.View]);
end;

procedure TdxGanttControlGridlineOptions.DoReset;
begin
  CurrentDate.Reset;
  ProjectFinish.Reset;
  ProjectStart.Reset;
end;

procedure TdxGanttControlGridlineOptions.SetCurrentDate
  (const Value: TdxGanttControlGridline);
begin
  FCurrentDate.Assign(Value);
end;

procedure TdxGanttControlGridlineOptions.SetProjectFinish
  (const Value: TdxGanttControlGridline);
begin
  FProjectFinish.Assign(Value);
end;

procedure TdxGanttControlGridlineOptions.SetProjectStart
  (const Value: TdxGanttControlGridline);
begin
  FProjectStart.Assign(Value);
end;

{ TdxGanttControlViewChartPredecessorLinkFFViewInfo }

procedure TdxGanttControlViewChartPredecessorLinkFFViewInfo.DoCalculate
  (const AStartPoint, AFinishPoint: TPoint; var AIndex: Integer);
var
  P: TPoint;
begin
  if UseRightToLeftAlignment xor (AStartPoint.X +
    IfThen(UseRightToLeftAlignment, -1, 1) * ScaleFactor.Apply
    (LinkLineStartOffset - LinkLineFinishOffset) >= AFinishPoint.X) then
    P := GetStartLineFinishPoint(AStartPoint)
  else
  begin
    P := AStartPoint;
    P.X := AFinishPoint.X;
    P := cxPointOffset(P, ScaleFactor.Apply(LinkLineFinishOffset), 0,
      not UseRightToLeftAlignment);
  end;
  AddPoint(P, AIndex);
  P.Y := AFinishPoint.Y;
  AddPoint(P, AIndex);
  AddPoint(AFinishPoint, AIndex);
end;

function TdxGanttControlViewChartPredecessorLinkFFViewInfo.GetLinkType
  : TdxGanttControlTaskPredecessorLinkType;
begin
  Result := TdxGanttControlTaskPredecessorLinkType.FF;
end;

{ TdxGanttControlGridline }

constructor TdxGanttControlGridline.Create
  (AOwner: TdxGanttControlGridlineOptions);
begin
  inherited Create;
  FOwner := AOwner;
end;

procedure TdxGanttControlGridline.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlGridline;
begin
  if Source is TdxGanttControlGridline then
  begin
    ASource := TdxGanttControlGridline(Source);
    Color := ASource.Color;
    Style := ASource.Style;
  end;
  inherited Assign(Source);
end;

procedure TdxGanttControlGridline.DoChanged;
begin
  FOwner.Changed;
end;

procedure TdxGanttControlGridline.DoReset;
begin
  FColor := clDefault;
  FStyle := TPenStyle.psSolid;
end;

function TdxGanttControlGridline.IsVisible: Boolean;
begin
  Result := Style <> TPenStyle.psClear;
end;

procedure TdxGanttControlGridline.SetColor(const Value: TColor);
begin
  if Color <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TdxGanttControlGridline.SetStyle(const Value: TPenStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    Changed;
  end;
end;

{ TdxGanttControlViewChartSummaryTaskViewInfo }

procedure TdxGanttControlViewChartSummaryTaskViewInfo.DoDrawBar
  (ACanvas: TcxCustomCanvas; const ABounds, AProgressBounds: TRect);
begin
  LookAndFeelPainter.DrawGanttSummaryTask(ACanvas, ABounds, ScaleFactor, Color);
  LookAndFeelPainter.DrawGanttSummaryTaskProgress(ACanvas, ABounds,
    AProgressBounds, ScaleFactor, Color);
end;

function TdxGanttControlViewChartSummaryTaskViewInfo.GetDefaultColor: TColor;
begin
  Result := CachedValues.SummaryColor;
end;

function TdxGanttControlViewChartSummaryTaskViewInfo.GetTaskHeight: Integer;
begin
  Result := CachedValues.SummaryHeight;
end;

{ TdxChartTaskLinkingImage }

constructor TdxChartTaskLinkingImage.Create(const ABounds: TRect;
  const AStart: TPoint; AColor: TColor; ALineWidth: Integer);
begin
  inherited Create;
  TransparentColor := True;
  TransparentColorValue := clWindow;
  FStart := AStart;
  FColor := AColor;
  FLineWidth := ALineWidth;
  Width := ABounds.Width;
  Height := ABounds.Height;
  SetImageSize(ABounds.Size);
end;

procedure TdxChartTaskLinkingImage.DoUpdateImage(AImage: TcxAlphaBitmap);
begin
  AImage.cxCanvas.FillRect(AImage.ClientRect, TransparentColorValue);
  AImage.cxCanvas.Line(FStart, FFinish, FColor, FLineWidth);
end;

function TdxChartTaskLinkingImage.OffsetStart(DX, DY: Integer): Boolean;
begin
  Result := (DX <> 0) or (DY <> 0);
  if Result then
  begin
    FStart.Offset(DX, DY);
    UpdateImage;
    cxRedrawWindow(Handle, RDW_INVALIDATE or RDW_ERASE or RDW_ERASENOW);
  end;
end;

procedure TdxChartTaskLinkingImage.SetFinish(const Value: TPoint);
begin
  if not FFinish.IsEqual(Value) then
  begin
    FFinish := Value;
    UpdateImage;
    Invalidate;
  end;
end;

{ TdxGanttControlViewChartAreaMajorHeaderViewInfo }

function TdxGanttControlViewChartAreaMajorHeaderViewInfo.
  CalculateCaption: string;
var
  ADate: TcxDateTime;
begin
  case Owner.TimescaleUnit of
    TdxGanttControlChartViewTimescaleUnit.Hours,
      TdxGanttControlChartViewTimescaleUnit.QuarterDays:
      Result := FormatDateTime('ddd dd MMM', DateTime);
    TdxGanttControlChartViewTimescaleUnit.Days:
      Result := DateToStr(DateTime,
        TdxCultureInfo.CurrentCulture.FormatSettings);
    TdxGanttControlChartViewTimescaleUnit.Weeks:
      Result := FormatDateTime('MMM yyyy', DateTime);
    TdxGanttControlChartViewTimescaleUnit.ThirdsOfMonths:
      Result := FormatDateTime('MMMM', DateTime);
    TdxGanttControlChartViewTimescaleUnit.Months:
      begin
        ADate := cxGetLocalCalendar.FromDateTime(DateTime);
        if ADate.Month <= 3 then
          Result := cxGetResourceString(@sdxGanttControlViewChartFirstQuarter)
        else if ADate.Month <= 6 then
          Result := cxGetResourceString(@sdxGanttControlViewChartSecondQuarter)
        else if ADate.Month <= 9 then
          Result := cxGetResourceString(@sdxGanttControlViewChartThirdQuarter)
        else
          Result := cxGetResourceString(@sdxGanttControlViewChartFourthQuarter);
      end;
    TdxGanttControlChartViewTimescaleUnit.Quarters,
      TdxGanttControlChartViewTimescaleUnit.HalfYears,
      TdxGanttControlChartViewTimescaleUnit.Years:
      Result := FormatDateTime('yyyy', DateTime);
  else
    Result := '';
  end;
end;

function TdxGanttControlViewChartAreaMajorHeaderViewInfo.
  CalculateDrawTextFlags: Integer;
begin
  Result := inherited CalculateDrawTextFlags;
  if UseRightToLeftAlignment then
    Result := Result or CXTO_RIGHT;
end;

function TdxGanttControlViewChartAreaMajorHeaderViewInfo.
  CalculateSeparatorBounds: TRect;
begin
  Result := inherited CalculateSeparatorBounds;
  Result.Top := Result.Top + ScaleFactor.Apply(6);
end;

function TdxGanttControlViewChartAreaMajorHeaderViewInfo.GetTextLayout
  : TcxCanvasBasedTextLayout;
begin
  Result := Owner.Owner.GetMajorHeaderTextLayout(Caption);
end;

{ TdxGanttControlViewChartAreaScaleHeaderViewInfo }

constructor TdxGanttControlViewChartAreaScaleHeaderViewInfo.Create
  (AOwner: TdxGanttControlViewChartAreaHeaderViewInfo; ADateTime: TDateTime);
begin
  inherited Create(AOwner);
  FDateTime := ADateTime;
  FCaption := CalculateCaption;
end;

function TdxGanttControlViewChartAreaScaleHeaderViewInfo.
  CalculateDrawTextFlags: Integer;
begin
  Result := CXTO_SINGLELINE or CXTO_CENTER_VERTICALLY or CXTO_END_ELLIPSIS;
end;

function TdxGanttControlViewChartAreaScaleHeaderViewInfo.
  CalculateSeparatorBounds: TRect;
begin
  Result := Bounds;
  Result.Width := LookAndFeelPainter.BorderSize;
  if not UseRightToLeftAlignment then
    Result.MoveToRight(Bounds.Right);
end;

procedure TdxGanttControlViewChartAreaScaleHeaderViewInfo.Calculate
  (const R: TRect);
begin
  inherited Calculate(R);
  FCaptionBounds := Bounds;
  FCaptionBounds.Inflate(-ScaleFactor.Apply(cxTextOffset), 0);
  FSeparatorBounds := CalculateSeparatorBounds;
end;

procedure TdxGanttControlViewChartAreaScaleHeaderViewInfo.DoDraw;
var
  ATextLayout: TcxCanvasBasedTextLayout;
begin
  ATextLayout := GetTextLayout;
  ATextLayout.SetFlags(CalculateDrawTextFlags);
  ATextLayout.SetFont(CanvasCache.GetControlFont);
  ATextLayout.SetColor(LookAndFeelPainter.DefaultHeaderTextColor);
  ATextLayout.SetText(FCaption);
  ATextLayout.SetLayoutConstraints(FCaptionBounds.Width,
    FCaptionBounds.Height, 0);
  ATextLayout.Draw(FCaptionBounds);
  Canvas.FillRect(FSeparatorBounds, LookAndFeelPainter.DefaultGridlineColor);
end;

function TdxGanttControlViewChartAreaScaleHeaderViewInfo.GetOwner
  : TdxGanttControlViewChartAreaHeaderViewInfo;
begin
  Result := TdxGanttControlViewChartAreaHeaderViewInfo(inherited Owner);
end;

{ TdxGanttControlViewChartTaskViewInfo }

procedure TdxGanttControlViewChartTaskViewInfo.DoDraw;
var
  R: TRect;
  I: Integer;
begin
  if IsFocused then
    LookAndFeelPainter.DrawGanttFocusedRow(Canvas, cxRectInflate(Bounds, 0, 2));
  I := Owner.Tasks.IndexOf(Self) + 1;
  if IsFocused or ((I < Owner.Tasks.Count) and Owner.Tasks[I].IsFocused) then
  begin
    R := Bounds;
    R.Height := 1;
    R.MoveToBottom(Bounds.Bottom);
    Canvas.FillRect(R, LookAndFeelPainter.DefaultSelectionColor);
  end;
  if not BarBounds.IsEmpty then
  begin
    DoDrawBar(Canvas, BarBounds, BarProgressBounds);
    if not CaptionBounds.IsEmpty then
      DoDrawCaption;
  end;
end;

procedure TdxGanttControlViewChartTaskViewInfo.DoDrawCaption;
var
  ATextLayout: TcxCanvasBasedTextLayout;
  ATextFlags: Integer;
begin
  ATextLayout := Owner.GetTaskCaptionTextLayout(Caption);
  ATextFlags := CXTO_CENTER_VERTICALLY;
  if UseRightToLeftAlignment then
    ATextFlags := ATextFlags or CXTO_RIGHT;
  ATextLayout.SetFlags(ATextFlags);
  ATextLayout.SetFont(CanvasCache.GetControlFont);
  ATextLayout.SetColor(LookAndFeelPainter.DefaultContentTextColor);
  ATextLayout.SetText(Caption);
  ATextLayout.SetLayoutConstraints(CaptionBounds.Width, CaptionBounds.Height);
  ATextLayout.Draw(CaptionBounds);
end;

function TdxGanttControlViewChartTaskViewInfo.IsFocused: Boolean;
begin
  Result := Owner.Tasks.IndexOf(Self) = Owner.Controller.ParentController.
    FocusedRowIndex - Owner.FFirstVisibleRowIndex;
end;

function TdxGanttControlViewChartTaskViewInfo.IsCompleteZone
  (const P: TPoint): Boolean;
var
  R: TRect;
begin
  R := GetHitBounds;
  R.Width := GetResizeHitZoneWidth;
  if UseRightToLeftAlignment then
    R.MoveToLeft(BarProgressBounds.Left - GetResizeHitZoneWidth div 2)
  else
    R.MoveToLeft(BarProgressBounds.Right - GetResizeHitZoneWidth div 2);
  Result := R.Contains(P);
end;

function TdxGanttControlViewChartTaskViewInfo.IsMovingZone
  (const P: TPoint): Boolean;
begin
  Result := GetHitBounds.Contains(P) and not IsCompleteZone(P) and
    not IsSizingZone(P);
end;

function TdxGanttControlViewChartTaskViewInfo.IsSizingZone
  (const P: TPoint): Boolean;
var
  R: TRect;
begin
  R := GetHitBounds;
  R.Width := GetResizeHitZoneWidth;
  if UseRightToLeftAlignment then
    R.MoveToLeft(BarBounds.Left - GetResizeHitZoneWidth div 2)
  else
    R.MoveToLeft(BarBounds.Right - GetResizeHitZoneWidth div 2);
  Result := R.Contains(P);
end;

procedure TdxGanttControlViewChartTaskViewInfo.DoDrawBar
  (ACanvas: TcxCustomCanvas; const ABounds, AProgressBounds: TRect);
begin
  LookAndFeelPainter.DrawGanttTask(ACanvas, ABounds, Color);
  LookAndFeelPainter.DrawGanttTaskProgress(ACanvas, ABounds,
    AProgressBounds, Color);
end;

function TdxGanttControlViewChartTaskViewInfo.CalculateBarBounds
  (const R: TRect): TRect;
begin
  if not(Task.IsValueAssigned(TdxGanttTaskAssignedValue.Start) and
    Task.IsValueAssigned(TdxGanttTaskAssignedValue.Finish)) then
    Exit(TRect.Null);
  Result := DoCalculateBarBounds(Task.Start, Task.Finish);
end;

function TdxGanttControlViewChartTaskViewInfo.DoCalculateBarBounds(ATaskStart,
  ATaskFinish: TDateTime): TRect;

  procedure CorrectBoundsByTime(var R: TRect; ADateTime: TDateTime;
    AIsStart: Boolean);
  var
    AStart, AFinish: TDateTime;
    AOffset: Integer;
    AOffsetPerDay: Double;
  begin
    if Owner.HeaderViewInfo.TimescaleUnit <=
      TdxGanttControlChartViewTimescaleUnit.QuarterDays then
    begin
      if not Calendar.IsWorkday(ADateTime) or
        (AIsStart and (Calendar.GetStartWorkTime(ADateTime) >= ADateTime)) or
        (not AIsStart and (Calendar.GetFinishWorkTime(ADateTime - OneMinute) <=
        ADateTime)) then
        Exit;
    end;

    AStart := Owner.HeaderViewInfo.GetMinorDateTime(ADateTime);
    AFinish := Owner.HeaderViewInfo.GetNextMinorDateTime(AStart);
    AOffset := Owner.HeaderViewInfo.CalculateMinorHeaderWidth;

    if (Owner.HeaderViewInfo.TimescaleUnit =
      TdxGanttControlChartViewTimescaleUnit.Days) and
      Calendar.IsWorkday(ADateTime) then
    begin
      AStart := Calendar.GetStartWorkTime(AStart);
      AFinish := Calendar.GetFinishWorkTime(AFinish - OneMinute);
      ADateTime := Max(ADateTime, AStart);
      ADateTime := Min(ADateTime, AFinish);
      AOffset := Round(AOffset * (ADateTime - AStart) / (AFinish - AStart));
    end
    else if Owner.HeaderViewInfo.TimescaleUnit >
      TdxGanttControlChartViewTimescaleUnit.Days then
    begin
      AOffsetPerDay := AOffset / (AFinish - AStart);
      AOffset := Round(AOffset * (DateOf(ADateTime) - AStart) /
        (AFinish - AStart));

      if (AOffsetPerDay > 0) and Calendar.IsWorkday(ADateTime) and
        ((AIsStart and (Calendar.GetStartWorkTime(ADateTime) > ADateTime)) or
        (not AIsStart and (Calendar.GetFinishWorkTime(ADateTime - OneMinute) <
        ADateTime))) then
      begin
        AStart := Calendar.GetStartWorkTime(ADateTime);
        AFinish := Calendar.GetFinishWorkTime(AStart);
        ADateTime := Max(ADateTime, AStart);
        ADateTime := Min(ADateTime, AFinish);
        AOffset := AOffset + Round(AOffsetPerDay * (ADateTime - AStart) /
          (AFinish - AStart));
      end
      else if not AIsStart then
        AOffset := AOffset + Round(AOffsetPerDay);
    end
    else
      AOffset := Round(AOffset * (ADateTime - AStart) / (AFinish - AStart));

    if (AOffset = 0) and (Owner.HeaderViewInfo.TimescaleUnit <=
      TdxGanttControlChartViewTimescaleUnit.Days) then
      Exit;
    if AIsStart then
    begin
      if UseRightToLeftAlignment then
        R.Right := R.Right - AOffset
      else
        R.Left := R.Left + AOffset;
    end
    else
    begin
      if UseRightToLeftAlignment then
        R.Left := R.Left - AOffset +
          Owner.HeaderViewInfo.CalculateMinorHeaderWidth
      else
        R.Right := R.Right + AOffset -
          Owner.HeaderViewInfo.CalculateMinorHeaderWidth;
    end
  end;

  procedure CorrectStart(var R: TRect);
  var
    AStart: TDateTime;
    AFirstVisibleDateTime: TDateTime;
    AMinorHeader: TdxGanttControlViewChartAreaMinorHeaderViewInfo;
  begin
    AStart := ATaskStart;
    AFirstVisibleDateTime := Owner.HeaderViewInfo.MinorHeaders.First.DateTime;
    if AStart > AFirstVisibleDateTime then
    begin
      AMinorHeader := Owner.HeaderViewInfo.GetMinorHeaderViewInfo(AStart, True);
      if AMinorHeader <> nil then
        if UseRightToLeftAlignment then
          R.Right := AMinorHeader.Bounds.Right
        else
          R.Left := AMinorHeader.Bounds.Left;
      CorrectBoundsByTime(R, AStart, True);
    end
    else if AStart < AFirstVisibleDateTime then
    begin
      if UseRightToLeftAlignment then
        Inc(R.Right, MaxCaptionWidth)
      else
        Dec(R.Left, MaxCaptionWidth);
    end;
  end;

  procedure CorrectFinish(var R: TRect);
  var
    AFinish: TDateTime;
    AFirstVisibleDateTime: TDateTime;
    ALastVisibleDateTime: TDateTime;
    AMinorHeader: TdxGanttControlViewChartAreaMinorHeaderViewInfo;
    ADate: TDateTime;
    AWidth, AStep: Integer;
    AMinorHeaderWidth: Integer;
  begin
    AFinish := ATaskFinish;
    ALastVisibleDateTime := Owner.HeaderViewInfo.GetNextMinorDateTime
      (Owner.HeaderViewInfo.MinorHeaders.Last.DateTime);
    AFirstVisibleDateTime := Owner.HeaderViewInfo.MinorHeaders.First.DateTime;
    AMinorHeaderWidth := Owner.HeaderViewInfo.CalculateMinorHeaderWidth;
    if AFinish < ALastVisibleDateTime then
    begin
      if AFinish <= AFirstVisibleDateTime then
      begin
        ADate := Owner.HeaderViewInfo.GetPreviousMinorDateTime
          (Owner.HeaderViewInfo.MinorHeaders.First.DateTime);
        AWidth := 0;
        AStep := AMinorHeaderWidth;
        while (ADate >= AFinish) and (AWidth < MaxCaptionWidth) do
        begin
          ADate := Owner.HeaderViewInfo.GetPreviousMinorDateTime(ADate);
          AWidth := AWidth + AStep;
        end;
        if UseRightToLeftAlignment then
          R.Left := Bounds.Right + AWidth
        else
          R.Right := Bounds.Left - AWidth;
      end
      else
      begin
        AMinorHeader := Owner.HeaderViewInfo.GetMinorHeaderViewInfo
          (AFinish, False);
        if AFinish > AMinorHeader.DateTime then
        begin
          if UseRightToLeftAlignment then
            R.Left := AMinorHeader.Bounds.Left
          else
            R.Right := AMinorHeader.Bounds.Right;
        end
        else
        begin
          if UseRightToLeftAlignment then
            R.Left := AMinorHeader.Bounds.Right
          else
            R.Right := AMinorHeader.Bounds.Left;
        end;
      end;
      CorrectBoundsByTime(R, AFinish, False);
    end
    else
    begin
      if UseRightToLeftAlignment then
        Dec(R.Left, AMinorHeaderWidth)
      else
        Inc(R.Right, AMinorHeaderWidth);
    end;
    R.Width := Max(R.Width, ScaleFactor.Apply(TaskMinWidth));
  end;

begin
  Result := Bounds;
  Result.Height := GetTaskHeight;
  Result.Offset(0, (Bounds.Height - Result.Height) div 2);
  if (ATaskStart >= Owner.HeaderViewInfo.GetNextMinorDateTime
    (Owner.HeaderViewInfo.MinorHeaders.Last.DateTime)) then
  begin
    if UseRightToLeftAlignment then
      Result.Left := Bounds.Left -
        Owner.HeaderViewInfo.CalculateMinorHeaderWidth
    else
      Result.Left := Bounds.Right +
        Owner.HeaderViewInfo.CalculateMinorHeaderWidth;
    Result.Right := Result.Left;
    Exit;
  end;
  CorrectStart(Result);
  CorrectFinish(Result);
  if ATaskFinish <= Owner.HeaderViewInfo.MinorHeaders.First.DateTime then
  begin
    if UseRightToLeftAlignment then
      Result.Right := Result.Left +
        Owner.HeaderViewInfo.CalculateMinorHeaderWidth
    else
      Result.Left := Result.Right -
        Owner.HeaderViewInfo.CalculateMinorHeaderWidth;
  end
  else if ATaskStart <= Owner.HeaderViewInfo.MinorHeaders.First.DateTime then
    if UseRightToLeftAlignment then
      Result.Right := Bounds.Right +
        Owner.HeaderViewInfo.CalculateMinorHeaderWidth
    else
      Result.Left := Bounds.Left -
        Owner.HeaderViewInfo.CalculateMinorHeaderWidth;
end;

function TdxGanttControlViewChartTaskViewInfo.CalculateBarProgressBounds
  (const R: TRect): TRect;
var
  AValue: Integer;
begin
  if not Task.IsValueAssigned(TdxGanttTaskAssignedValue.PercentComplete) then
    AValue := 0
  else
    AValue := Task.PercentComplete;
  Result := DoCalculateBarProgressBounds(AValue);
end;

function TdxGanttControlViewChartTaskViewInfo.DoCalculateBarProgressBounds
  (AValue: Integer): TRect;
var
  ADuration: TdxGanttControlDuration;
  S, M, H: Integer;
  AFinish: TDateTime;
begin
  if not Owner.Controller.ParentController.View.ShowTaskProgress then
    Exit(TRect.Null);
  Result := BarBounds;
  if Result.IsEmpty then
    Exit(TRect.Null);
  if AValue = 100 then
    Exit;
  if AValue = 0 then
  begin
    Result.Width := 0;
    if UseRightToLeftAlignment then
      Result.MoveToRight(BarBounds.Right);
    Exit;
  end;
  if Task.RealDuration = '' then
    Exit(TRect.Null);
  ADuration := TdxGanttControlDuration.Create(Task.RealDuration);
  S := ((ADuration.Hour * 60) + ADuration.Minute) * 60 + ADuration.Second;
  S := MulDiv(S, AValue, 100);
  H := S div 3600;
  M := (S - H * 3600) div 60;
  S := S mod 60;
  ADuration := TdxGanttControlDuration.Create(Format('PT%dH%dM%dS', [H, M, S]));
  AFinish := ADuration.GetWorkFinish(Task.Start, Task.RealCalendar,
    Task.RealDurationFormat);
  if UseRightToLeftAlignment then
    Result.Left := Owner.HeaderViewInfo.GetPosByDateTime(AFinish)
  else
    Result.Right := Owner.HeaderViewInfo.GetPosByDateTime(AFinish);
end;

function TdxGanttControlViewChartTaskViewInfo.CalculateCaptionBounds
  (const R: TRect): TRect;
begin
  if BarBounds.IsEmpty then
    Exit(TRect.Null);
  if Trim(Caption) = '' then
    Exit(TRect.Null);

  Result := Bounds;
  if UseRightToLeftAlignment then
    Result.Right := BarBounds.Left - ScaleFactor.Apply
      (LookAndFeelPainter.GetGanttTaskTextLabelOffset)
  else
    Result.Left := BarBounds.Right + ScaleFactor.Apply
      (LookAndFeelPainter.GetGanttTaskTextLabelOffset);
  Result.Width := MaxCaptionWidth;
end;

function TdxGanttControlViewChartTaskViewInfo.GetCachedValues
  : TdxGanttControlTaskViewInfoCachedValues;
begin
  Result := Owner.TaskCachedValues;
end;

function TdxGanttControlViewChartTaskViewInfo.GetCaption: string;
begin
  Result := Task.ResourceName;
end;

function TdxGanttControlViewChartTaskViewInfo.GetCurrentCursor(const P: TPoint;
  const ADefaultCursor: TCursor): TCursor;
begin
  if IsSizingZone(P) then
    Result := TdxGanttControlCursors.TaskResize
  else if IsCompleteZone(P) then
    Result := TdxGanttControlCursors.TaskComplete
  else if IsMovingZone(P) then
    Result := TdxGanttControlCursors.TaskMove
  else
    Result := inherited GetCurrentCursor(P, ADefaultCursor);
end;

function TdxGanttControlViewChartTaskViewInfo.GetHitBounds: TRect;
begin
  Result := inherited GetHitBounds;
  Result.Inflate(GetResizeHitZoneWidth div 2, 0);
  Result.Height := dxGetTouchableSize(GetTaskHeight, ScaleFactor);
  Result.Width := dxGetTouchableSize(Result.Width, ScaleFactor);
  Result := cxRectCenter(inherited GetHitBounds, Result.Size);
end;

function TdxGanttControlViewChartTaskViewInfo.GetOwner
  : TdxGanttControlViewChartAreaViewInfo;
begin
  Result := TdxGanttControlViewChartAreaViewInfo(inherited Owner);
end;

{ TdxViewChartAreaCreateTaskCommand }

constructor TdxViewChartAreaCreateTaskCommand.Create
  (AController: TdxGanttControlChartViewAreaController; AIndex: Integer;
  AStart, AFinish: TDateTime);
begin
  inherited Create(AController.ParentController, AIndex);
  FFinish := AFinish;
  FStart := AStart;
end;

procedure TdxViewChartAreaCreateTaskCommand.DoExecute;
begin
  with TdxGanttControlChangeTaskStartCommand.Create(Control, Task, FStart) do
    try
      Execute;
    finally
      Free;
    end;
  with TdxGanttControlChangeTaskFinishCommand.Create(Control, Task, FFinish) do
    try
      Execute;
    finally
      Free;
    end;
  inherited DoExecute;
end;

{ TdxViewChartDeleteFocusedTaskCommand }

procedure TdxViewChartDeleteFocusedTaskCommand.DoExecute;
begin
  inherited DoExecute;
  with TdxGanttControlDeleteTaskCommand.Create(Control,
    Controller.FocusedDataItem) do
    try
      Execute;
    finally
      Free;
    end;
end;

function TdxViewChartDeleteFocusedTaskCommand.GetController
  : TdxGanttControlChartViewSheetController;
begin
  Result := TdxGanttControlChartViewSheetController(inherited Controller);
end;

{ TdxGanttControlViewChartEmptyTaskViewInfo }

function TdxGanttControlViewChartEmptyTaskViewInfo.CalculateBarBounds
  (const R: TRect): TRect;
begin
  Result := TRect.Null;
end;

function TdxGanttControlViewChartEmptyTaskViewInfo.CalculateBarProgressBounds
  (const R: TRect): TRect;
begin
  Result := TRect.Null;
end;

function TdxGanttControlViewChartEmptyTaskViewInfo.GetCaption: string;
begin
  Result := '';
end;

function TdxGanttControlViewChartEmptyTaskViewInfo.GetCurrentCalendar
  : TdxGanttControlCalendar;
begin
  Result := Owner.Calendar;
end;

function TdxGanttControlViewChartEmptyTaskViewInfo.GetDefaultColor: TColor;
var
  AIsManual: Boolean;
begin
  AIsManual := Owner.Controller.DataProvider.DataModel.Properties.
    MarkNewTasksAsManuallyScheduled;
  Result := GetDefaultColor(AIsManual);
end;

function TdxGanttControlViewChartEmptyTaskViewInfo.IsCompleteZone
  (const P: TPoint): Boolean;
begin
  Result := False;
end;

function TdxGanttControlViewChartEmptyTaskViewInfo.IsMovingZone
  (const P: TPoint): Boolean;
begin
  Result := False;
end;

function TdxGanttControlViewChartEmptyTaskViewInfo.IsSizingZone
  (const P: TPoint): Boolean;
begin
  Result := False;
end;

{ TdxGanttControlViewChartAreaHeaderViewInfo }

constructor TdxGanttControlViewChartAreaHeaderViewInfo.Create
  (AOwner: TdxGanttControlCustomItemViewInfo);
begin
  inherited Create(AOwner);
  FMajorHeaders :=
    TObjectList<TdxGanttControlViewChartAreaMajorHeaderViewInfo>.Create;
  FMinorHeaders :=
    TObjectList<TdxGanttControlViewChartAreaMinorHeaderViewInfo>.Create;
end;

destructor TdxGanttControlViewChartAreaHeaderViewInfo.Destroy;
begin
  FreeAndNil(FMajorHeaders);
  FreeAndNil(FMinorHeaders);
  inherited Destroy;
end;

procedure TdxGanttControlViewChartAreaHeaderViewInfo.Calculate(const R: TRect);
begin
  inherited Calculate(R);
  DoCalculate;
end;

procedure TdxGanttControlViewChartAreaHeaderViewInfo.DoDraw;
var
  I: Integer;
begin
  LookAndFeelPainter.DrawScaledHeader(Canvas, Bounds, cxbsNormal, [], [bBottom],
    ScaleFactor, False, False);
  for I := 0 to FMajorHeaders.Count - 1 do
    FMajorHeaders[I].Draw;
  for I := 0 to FMinorHeaders.Count - 1 do
    FMinorHeaders[I].Draw;
  inherited DoDraw;
end;

function TdxGanttControlViewChartAreaHeaderViewInfo.CalculateHitTest
  (const AHitTest: TdxGanttControlHitTest): Boolean;
var
  I: Integer;
begin
  Result := inherited CalculateHitTest(AHitTest);
  if Result then
  begin
    for I := 0 to FMajorHeaders.Count - 1 do
      if FMajorHeaders[I].CalculateHitTest(AHitTest) then
        Exit;
    for I := 0 to FMinorHeaders.Count - 1 do
      if FMinorHeaders[I].CalculateHitTest(AHitTest) then
        Exit;
  end;
end;

function TdxGanttControlViewChartAreaHeaderViewInfo.IsEndOfMajor
  (AViewInfo: TdxGanttControlViewChartAreaMinorHeaderViewInfo): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to FMajorHeaders.Count - 1 do
  begin
    if UseRightToLeftAlignment then
    begin
      if AViewInfo.Bounds.Left = FMajorHeaders[I].Bounds.Left then
        Exit;
    end
    else
    begin
      if AViewInfo.Bounds.Right = FMajorHeaders[I].Bounds.Right then
        Exit;
    end;
  end;
  Result := False;
end;

procedure TdxGanttControlViewChartAreaHeaderViewInfo.Reset;
begin
  inherited Reset;
  FCachedMinorHeaderWidth := -1;
end;

procedure TdxGanttControlViewChartAreaHeaderViewInfo.ViewChanged;
begin
  inherited ViewChanged;
  DoCalculate;
end;

function TdxGanttControlViewChartAreaHeaderViewInfo.GetMinorHeaderViewInfo
  (ADateTime: TDateTime; AIsStart: Boolean)
  : TdxGanttControlViewChartAreaMinorHeaderViewInfo;
var
  I: Integer;
begin
  for I := 0 to MinorHeaders.Count - 1 do
  begin
    if AIsStart then
    begin
      if (CompareDateTime(MinorHeaders[I].DateTime, ADateTime) <= 0) and
        (CompareDateTime(GetNextMinorDateTime(MinorHeaders[I].DateTime),
        ADateTime) > 0) then
        Exit(MinorHeaders[I]);
    end
    else
    begin
      if (CompareDateTime(MinorHeaders[I].DateTime, ADateTime) < 0) and
        (CompareDateTime(GetNextMinorDateTime(MinorHeaders[I].DateTime),
        ADateTime) >= 0) then
        Exit(MinorHeaders[I]);
    end;
  end;
  Result := nil;
end;

function TdxGanttControlViewChartAreaHeaderViewInfo.GetDateTimeByPos(X: Integer)
  : TDateTime;
var
  AStart, AFinish: TDateTime;
  AStartX: Integer;
  ADuration: TdxGanttControlDuration;
  S: Integer;
begin
  AStart := MinorHeaders.First.DateTime;
  if UseRightToLeftAlignment then
    AStartX := MinorHeaders.First.Bounds.Right
  else
    AStartX := MinorHeaders.First.Bounds.Left;
  if UseRightToLeftAlignment xor (X < AStartX) then
  begin
    while UseRightToLeftAlignment xor (X < AStartX) do
    begin
      AStart := GetPreviousMinorDateTime(AStart);
      if UseRightToLeftAlignment then
        Inc(AStartX, CalculateMinorHeaderWidth)
      else
        Dec(AStartX, CalculateMinorHeaderWidth);
    end;
  end
  else
  begin
    while UseRightToLeftAlignment xor
      (X > (AStartX + IfThen(UseRightToLeftAlignment, -1, 1) *
      CalculateMinorHeaderWidth)) do
    begin
      AStart := GetNextMinorDateTime(AStart);
      if UseRightToLeftAlignment then
        Dec(AStartX, CalculateMinorHeaderWidth)
      else
        Inc(AStartX, CalculateMinorHeaderWidth);
    end;
  end;
  AFinish := GetNextMinorDateTime(AStart);
  if TimescaleUnit = TdxGanttControlChartViewTimescaleUnit.Days then
  begin
    if Owner.Calendar.IsWorkday(AStart) then
    begin
      AStart := Owner.Calendar.GetStartWorkTime(AStart);
      AFinish := Owner.Calendar.GetFinishWorkTime(AFinish - OneMinute);
    end;
  end;
  ADuration := TdxGanttControlDuration.Create(AStart, AFinish, Owner.Calendar,
    TdxDurationFormat.Days);
  S := ADuration.ToSeconds;
  S := Ceil(Abs(X - AStartX) / CalculateMinorHeaderWidth * S);
  ADuration := TdxGanttControlDuration.Create
    (Format('PT%dH%dM%dS', [S div 3600, (S mod 3600) div 60, S mod 60]));
  Result := ADuration.GetWorkFinish(AStart, Owner.Calendar,
    TdxDurationFormat.Days);
  Result := Max(Result, AStart);
  Result := Min(Result, AFinish);
end;

function TdxGanttControlViewChartAreaHeaderViewInfo.GetPosByDateTime
  (ADateTime: TDateTime): Integer;
var
  AStart, AFinish: TDateTime;
begin
  if ADateTime < MinorHeaders.First.DateTime then
  begin
    if UseRightToLeftAlignment then
      Result := MinorHeaders.First.Bounds.Right
    else
      Result := MinorHeaders.First.Bounds.Left;
    AStart := MinorHeaders.First.DateTime;
    while ADateTime < AStart do
    begin
      AStart := GetPreviousMinorDateTime(AStart);
      if UseRightToLeftAlignment then
        Inc(Result, CalculateMinorHeaderWidth)
      else
        Dec(Result, CalculateMinorHeaderWidth);
    end;
  end
  else
  begin
    if UseRightToLeftAlignment then
      Result := MinorHeaders.First.Bounds.Right
    else
      Result := MinorHeaders.First.Bounds.Left;
    AStart := MinorHeaders.First.DateTime;
    while ADateTime > GetNextMinorDateTime(AStart) do
    begin
      AStart := GetNextMinorDateTime(AStart);
      if UseRightToLeftAlignment then
        Dec(Result, CalculateMinorHeaderWidth)
      else
        Inc(Result, CalculateMinorHeaderWidth);
    end;
  end;
  AFinish := GetNextMinorDateTime(AStart);
  if TimescaleUnit = TdxGanttControlChartViewTimescaleUnit.Days then
  begin
    AStart := Owner.Calendar.GetStartWorkTime(AStart);
    AFinish := Owner.Calendar.GetFinishWorkTime(AFinish - OneMinute);
    if AStart = AFinish then
      AFinish := AStart + 1;
    ADateTime := Max(ADateTime, AStart);
    ADateTime := Min(ADateTime, AFinish);
  end;
  Result := Result + Ceil((ADateTime - AStart) / (AFinish - AStart) *
    CalculateMinorHeaderWidth);
end;

function TdxGanttControlViewChartAreaHeaderViewInfo.GetFirstVisibleMajorDateTime
  : TDateTime;
var
  ADate: TcxDateTime;
  Y: Integer;
begin
  case TimescaleUnit of
    TdxGanttControlChartViewTimescaleUnit.Hours,
      TdxGanttControlChartViewTimescaleUnit.QuarterDays:
      Result := DateOf(FFirstVisibleDateTime);
    TdxGanttControlChartViewTimescaleUnit.Days:
      Result := dxGetStartDateOfWeek(FFirstVisibleDateTime);
    TdxGanttControlChartViewTimescaleUnit.Weeks:
      Result := dxGetStartDateOfMonth(FFirstVisibleDateTime);
    TdxGanttControlChartViewTimescaleUnit.ThirdsOfMonths:
      Result := dxGetStartDateOfMonth(FFirstVisibleDateTime);
    TdxGanttControlChartViewTimescaleUnit.Months:
      begin
        ADate := cxGetLocalCalendar.FromDateTime(FFirstVisibleDateTime);
        ADate.Month := ((ADate.Month - 1) div 3) * 3 + 1;
        Result := cxGetLocalCalendar.ToDateTime(ADate);
        Result := cxGetLocalCalendar.GetFirstDayOfMonth(Result);
      end;
    TdxGanttControlChartViewTimescaleUnit.Quarters,
      TdxGanttControlChartViewTimescaleUnit.HalfYears:
      Result := cxGetLocalCalendar.GetFirstDayOfYear(FFirstVisibleDateTime);
    TdxGanttControlChartViewTimescaleUnit.Years:
      begin
        Result := cxGetLocalCalendar.GetFirstDayOfYear(FFirstVisibleDateTime);
        Y := 1984 - cxGetLocalCalendar.GetYear(Result);
        Result := cxGetLocalCalendar.AddYears(Result, Y mod 5);
      end
  else
    Result := InvalidDate;
  end;
end;

function TdxGanttControlViewChartAreaHeaderViewInfo.GetNextMajorDateTime
  (ADateTime: TDateTime): TDateTime;
begin
  case TimescaleUnit of
    TdxGanttControlChartViewTimescaleUnit.Hours,
      TdxGanttControlChartViewTimescaleUnit.QuarterDays:
      Result := ADateTime + 1;
    TdxGanttControlChartViewTimescaleUnit.Days:
      Result := ADateTime + 7;
    TdxGanttControlChartViewTimescaleUnit.Weeks:
      Result := cxGetLocalCalendar.AddMonths(ADateTime, 1);
    TdxGanttControlChartViewTimescaleUnit.ThirdsOfMonths:
      Result := cxGetLocalCalendar.AddMonths(ADateTime, 1);
    TdxGanttControlChartViewTimescaleUnit.Months:
      Result := cxGetLocalCalendar.AddMonths(ADateTime, 3);
    TdxGanttControlChartViewTimescaleUnit.Quarters,
      TdxGanttControlChartViewTimescaleUnit.HalfYears:
      Result := cxGetLocalCalendar.AddYears(ADateTime, 1);
    TdxGanttControlChartViewTimescaleUnit.Years:
      Result := cxGetLocalCalendar.AddYears(ADateTime, 5);
  else
    Result := InvalidDate;
  end;
end;

function TdxGanttControlViewChartAreaHeaderViewInfo.GetMinorDateTime
  (ADateTime: TDateTime): TDateTime;
var
  ADate: TcxDateTime;
begin
  case TimescaleUnit of
    TdxGanttControlChartViewTimescaleUnit.Hours:
      Result := DateOf(ADateTime) + HourOf(ADateTime) / 24;
    TdxGanttControlChartViewTimescaleUnit.QuarterDays:
      Result := DateOf(ADateTime) + (HourOf(ADateTime) div 6) / 4;
    TdxGanttControlChartViewTimescaleUnit.Days:
      Result := DateOf(ADateTime);
    TdxGanttControlChartViewTimescaleUnit.Weeks:
      Result := StartOfTheWeek(ADateTime);
    TdxGanttControlChartViewTimescaleUnit.ThirdsOfMonths:
      begin
        ADate := cxGetLocalCalendar.FromDateTime(ADateTime);
        if ADate.Day < 11 then
          ADate.Day := 1
        else if ADate.Day < 21 then
          ADate.Day := 11
        else
          ADate.Day := 21;
        Result := cxGetLocalCalendar.ToDateTime(ADate)
      end;
    TdxGanttControlChartViewTimescaleUnit.Months:
      Result := DateOf(cxGetLocalCalendar.GetFirstDayOfMonth(ADateTime));
    TdxGanttControlChartViewTimescaleUnit.Quarters:
      begin
        ADate := cxGetLocalCalendar.FromDateTime(ADateTime);
        ADate.Month := ((ADate.Month - 1) div 3) * 3 + 1;
        ADate.Day := 1;
        Result := cxGetLocalCalendar.ToDateTime(ADate);
      end;
    TdxGanttControlChartViewTimescaleUnit.HalfYears:
      begin
        ADate := cxGetLocalCalendar.FromDateTime(ADateTime);
        ADate.Month := ((ADate.Month - 1) div 6) * 6 + 1;
        Result := cxGetLocalCalendar.ToDateTime(ADate);
        Result := DateOf(cxGetLocalCalendar.GetFirstDayOfMonth(Result));
      end;
    TdxGanttControlChartViewTimescaleUnit.Years:
      Result := DateOf(cxGetLocalCalendar.GetFirstDayOfYear(ADateTime));
  else
    Result := InvalidDate;
  end;
end;

function TdxGanttControlViewChartAreaHeaderViewInfo.GetFirstVisibleMinorDateTime
  : TDateTime;
begin
  Result := GetMinorDateTime(FFirstVisibleDateTime);
end;

function TdxGanttControlViewChartAreaHeaderViewInfo.GetNextMinorDateTime
  (ADateTime: TDateTime): TDateTime;
var
  ADate: TcxDateTime;
begin
  case TimescaleUnit of
    TdxGanttControlChartViewTimescaleUnit.Hours:
      Result := ADateTime + 1 / 24;
    TdxGanttControlChartViewTimescaleUnit.QuarterDays:
      Result := ADateTime + 1 / 4;
    TdxGanttControlChartViewTimescaleUnit.Days:
      Result := ADateTime + 1;
    TdxGanttControlChartViewTimescaleUnit.Weeks:
      Result := ADateTime + 7;
    TdxGanttControlChartViewTimescaleUnit.ThirdsOfMonths:
      begin
        ADate := cxGetLocalCalendar.FromDateTime(ADateTime);
        if ADate.Day < 11 then
          ADate.Day := 11
        else if ADate.Day < 21 then
          ADate.Day := 21
        else
        begin
          ADate := cxGetLocalCalendar.FromDateTime
            (cxGetLocalCalendar.AddMonths(ADate, 1));
          ADate.Day := 1;
        end;
        Result := cxGetLocalCalendar.ToDateTime(ADate);
      end;
    TdxGanttControlChartViewTimescaleUnit.Months:
      Result := cxGetLocalCalendar.AddMonths(ADateTime, 1);
    TdxGanttControlChartViewTimescaleUnit.Quarters:
      Result := cxGetLocalCalendar.AddMonths(ADateTime, 3);
    TdxGanttControlChartViewTimescaleUnit.HalfYears:
      Result := cxGetLocalCalendar.AddMonths(ADateTime, 6);
    TdxGanttControlChartViewTimescaleUnit.Years:
      Result := cxGetLocalCalendar.AddYears(ADateTime, 1);
  else
    Result := InvalidDate;
  end;
end;

function TdxGanttControlViewChartAreaHeaderViewInfo.GetNextMinorDateTime
  (ADateTime: TDateTime; ACount: Integer): TDateTime;
var
  I: Integer;
begin
  Result := ADateTime;
  for I := 0 to Abs(ACount) - 1 do
    if ACount < 0 then
      Result := GetPreviousMinorDateTime(Result)
    else
      Result := GetNextMinorDateTime(Result);
end;

function TdxGanttControlViewChartAreaHeaderViewInfo.GetPreviousMinorDateTime
  (ADateTime: TDateTime): TDateTime;
var
  ADate: TcxDateTime;
begin
  case TimescaleUnit of
    TdxGanttControlChartViewTimescaleUnit.Hours:
      Result := ADateTime - 1 / 24;
    TdxGanttControlChartViewTimescaleUnit.QuarterDays:
      Result := ADateTime - 1 / 4;
    TdxGanttControlChartViewTimescaleUnit.Days:
      Result := ADateTime - 1;
    TdxGanttControlChartViewTimescaleUnit.Weeks:
      Result := ADateTime - 7;
    TdxGanttControlChartViewTimescaleUnit.ThirdsOfMonths:
      begin
        ADate := cxGetLocalCalendar.FromDateTime(ADateTime);
        if ADate.Day > 21 then
          ADate.Day := 21
        else if ADate.Day > 11 then
          ADate.Day := 11
        else if ADate.Day > 1 then
          ADate.Day := 1
        else
        begin
          ADate := cxGetLocalCalendar.FromDateTime
            (cxGetLocalCalendar.AddMonths(ADate, -1));
          ADate.Day := 21;
        end;
        Result := cxGetLocalCalendar.ToDateTime(ADate);
      end;
    TdxGanttControlChartViewTimescaleUnit.Months:
      Result := cxGetLocalCalendar.AddMonths(ADateTime, -1);
    TdxGanttControlChartViewTimescaleUnit.Quarters:
      Result := cxGetLocalCalendar.AddMonths(ADateTime, -3);
    TdxGanttControlChartViewTimescaleUnit.HalfYears:
      Result := cxGetLocalCalendar.AddMonths(ADateTime, -6);
    TdxGanttControlChartViewTimescaleUnit.Years:
      Result := cxGetLocalCalendar.AddYears(ADateTime, -1);
  else
    Result := InvalidDate;
  end;
end;

function TdxGanttControlViewChartAreaHeaderViewInfo.GetTimescaleUnit
  : TdxGanttControlChartViewTimescaleUnit;
begin
  Result := Owner.FTimescaleUnit;
end;

function TdxGanttControlViewChartAreaHeaderViewInfo.
  CalculateMajorHeaderHeight: Integer;
begin
  Result := (Bounds.Bottom - Bounds.Top - LookAndFeelPainter.BorderSize) div 2;
end;

function TdxGanttControlViewChartAreaHeaderViewInfo.CalculateMajorHeaderWidth
  (ADateTime: TDateTime; ATotalWidth: Integer): Integer;
begin
  case TimescaleUnit of
    TdxGanttControlChartViewTimescaleUnit.Hours:
      Result := CalculateMinorHeaderWidth * 24;
    TdxGanttControlChartViewTimescaleUnit.QuarterDays:
      Result := CalculateMinorHeaderWidth * 4;
    TdxGanttControlChartViewTimescaleUnit.Days:
      Result := CalculateMinorHeaderWidth * 7;
    TdxGanttControlChartViewTimescaleUnit.Weeks:
      begin
        Result := Trunc(CalculateMinorHeaderWidth *
          (GetNextMajorDateTime(ADateTime) - ADateTime) / 7);
        if ATotalWidth = 0 then
          Exit;
        Result := Result + Ceil(CalculateMinorHeaderWidth *
          (ADateTime - GetFirstVisibleMinorDateTime) / 7) - ATotalWidth;
      end;
    TdxGanttControlChartViewTimescaleUnit.ThirdsOfMonths,
      TdxGanttControlChartViewTimescaleUnit.Months:
      Result := CalculateMinorHeaderWidth * 3;
    TdxGanttControlChartViewTimescaleUnit.Quarters:
      Result := CalculateMinorHeaderWidth * 4;
    TdxGanttControlChartViewTimescaleUnit.HalfYears:
      Result := CalculateMinorHeaderWidth * 2;
    TdxGanttControlChartViewTimescaleUnit.Years:
      Result := CalculateMinorHeaderWidth * 5;
  else
    Result := 0;
  end;
end;

function TdxGanttControlViewChartAreaHeaderViewInfo.
  CalculateMajorHeaderFirstOffset: Integer;
var
  ADate: TDateTime;
  ADateTime: TcxDateTime;
  Y, M, D: Integer;
begin
  ADate := GetFirstVisibleMajorDateTime;
  if ADate = GetFirstVisibleMinorDateTime then
    Result := 0
  else
  begin
    if TimescaleUnit = TdxGanttControlChartViewTimescaleUnit.ThirdsOfMonths then
    begin
      ADateTime := cxGetLocalCalendar.FromDateTime
        (GetFirstVisibleMinorDateTime);
      D := ADateTime.Day;
      Result := -Trunc(CalculateMajorHeaderWidth(ADate, 0) * (D div 10) / 3);
    end
    else if TimescaleUnit = TdxGanttControlChartViewTimescaleUnit.Months then
    begin
      ADateTime := cxGetLocalCalendar.FromDateTime
        (GetFirstVisibleMinorDateTime);
      M := ADateTime.Month;
      Result := -Trunc(CalculateMajorHeaderWidth(ADate, 0) *
        ((M - 1) mod 3) / 3);
    end
    else if TimescaleUnit = TdxGanttControlChartViewTimescaleUnit.Quarters then
    begin
      ADateTime := cxGetLocalCalendar.FromDateTime
        (GetFirstVisibleMinorDateTime);
      M := ADateTime.Month;
      Result := -Trunc(CalculateMajorHeaderWidth(ADate, 0) *
        (((M - 1) div 3) mod 4) / 4);
    end
    else if TimescaleUnit = TdxGanttControlChartViewTimescaleUnit.HalfYears then
    begin
      ADateTime := cxGetLocalCalendar.FromDateTime
        (GetFirstVisibleMinorDateTime);
      M := ADateTime.Month;
      Result := -Trunc(CalculateMajorHeaderWidth(ADate, 0) *
        (((M - 1) div 6) mod 2) / 2);
    end
    else if TimescaleUnit = TdxGanttControlChartViewTimescaleUnit.Years then
    begin
      ADateTime := cxGetLocalCalendar.FromDateTime
        (GetFirstVisibleMinorDateTime);
      Y := ADateTime.Year - 1984;
      Result := -Trunc(CalculateMajorHeaderWidth(ADate, 0) * (Y mod 5) / 5);
    end
    else
      Result := Trunc(CalculateMajorHeaderWidth(ADate, 0) *
        (ADate - GetFirstVisibleMinorDateTime) / (GetNextMajorDateTime(ADate)
        - ADate));
  end;
end;

procedure TdxGanttControlViewChartAreaHeaderViewInfo.DoCalculate;
begin
  FFirstVisibleDateTime := Owner.FFirstVisibleDateTime;
  CalculateMajorHeaders;
  CalculateMinorHeaders;
end;

function TdxGanttControlViewChartAreaHeaderViewInfo.
  CalculateMinorHeaderWidth: Integer;

  function GetWidth(ABase: Integer): Integer;
  begin
    Result := MulDiv(ABase, Abs(CanvasCache.GetBaseFont.Height), 11);
  end;

var
  I: Integer;
  ATextLayout: TcxCanvasBasedTextLayout;
begin
  if FCachedMinorHeaderWidth > 0 then
    Exit(FCachedMinorHeaderWidth);

  case TimescaleUnit of
    TdxGanttControlChartViewTimescaleUnit.Weeks:
      FCachedMinorHeaderWidth := GetWidth(46);
    TdxGanttControlChartViewTimescaleUnit.ThirdsOfMonths:
      FCachedMinorHeaderWidth := GetWidth(29);
    TdxGanttControlChartViewTimescaleUnit.Months:
      begin
        FCachedMinorHeaderWidth := GetWidth(39);
        ATextLayout := Canvas.CreateTextLayout;
        try
          for I := 1 to 12 do
            FCachedMinorHeaderWidth := Max(FCachedMinorHeaderWidth,
              ScaleFactor.Apply(cxTextOffset * 2) +
              LookAndFeelPainter.BorderSize +
              TdxGanttControlUtils.MeasureTextWidth(ATextLayout,
              TdxCultureInfo.CurrentCulture.FormatSettings.ShortMonthNames[I],
              CanvasCache.GetControlFont))
        finally
          ATextLayout.Free;
        end;
      end;
    TdxGanttControlChartViewTimescaleUnit.Quarters:
      FCachedMinorHeaderWidth := GetWidth(41);
    TdxGanttControlChartViewTimescaleUnit.HalfYears:
      FCachedMinorHeaderWidth := GetWidth(28);
    TdxGanttControlChartViewTimescaleUnit.Years:
      FCachedMinorHeaderWidth := GetWidth(30);
  else
    FCachedMinorHeaderWidth := GetWidth(24);
    ATextLayout := Canvas.CreateTextLayout;
    try
      for I := 1 to 7 do
        FCachedMinorHeaderWidth := Max(FCachedMinorHeaderWidth,
          ScaleFactor.Apply(cxTextOffset * 2) + LookAndFeelPainter.BorderSize +
          TdxGanttControlUtils.MeasureTextWidth(ATextLayout,
          TdxCultureInfo.CurrentCulture.FormatSettings.ShortDayNames[I],
          CanvasCache.GetControlFont))
    finally
      ATextLayout.Free;
    end;
  end;
  FCachedMinorHeaderWidth := FCachedMinorHeaderWidth *
    Ord(Owner.Owner.Controller.ChartAreaController.MinorHeaderWidth);
  Result := FCachedMinorHeaderWidth;
end;

procedure TdxGanttControlViewChartAreaHeaderViewInfo.CalculateMajorHeaders;
var
  R: TRect;
  AWidth: Integer;
  ATotalWidth: Integer;
  ADate: TDateTime;
begin
  FMajorHeaders.Clear;
  R := Bounds;
  R.Height := CalculateMajorHeaderHeight;
  ADate := GetFirstVisibleMajorDateTime;
  if UseRightToLeftAlignment then
    R.MoveToRight(Bounds.Right);
  ATotalWidth := CalculateMajorHeaderFirstOffset;
  if UseRightToLeftAlignment then
    R.Offset(-ATotalWidth, 0)
  else
    R.Offset(ATotalWidth, 0);
  while (ATotalWidth < Bounds.Width) or (FMajorHeaders.Count = 0) do
  begin
    AWidth := CalculateMajorHeaderWidth(ADate, ATotalWidth);
    R.Width := AWidth;
    if UseRightToLeftAlignment then
      R.MoveToRight(Bounds.Right - ATotalWidth);
    FMajorHeaders.Add(TdxGanttControlViewChartAreaMajorHeaderViewInfo.Create
      (Self, ADate));
    FMajorHeaders.Last.Calculate(R);
    ADate := GetNextMajorDateTime(ADate);
    if UseRightToLeftAlignment then
      R.Offset(-AWidth, 0)
    else
      R.Offset(AWidth, 0);
    Inc(ATotalWidth, AWidth);
  end;
end;

procedure TdxGanttControlViewChartAreaHeaderViewInfo.CalculateMinorHeaders;
var
  R: TRect;
  AWidth: Integer;
  ATotalWidth: Integer;
  ADate: TDateTime;
begin
  FMinorHeaders.Clear;
  R := Bounds;
  R.Bottom := R.Bottom - LookAndFeelPainter.BorderSize;
  R.Top := R.Top + CalculateMajorHeaderHeight;
  AWidth := CalculateMinorHeaderWidth;
  R.Width := AWidth;
  if UseRightToLeftAlignment then
    R.MoveToRight(Bounds.Right);
  ADate := GetFirstVisibleMinorDateTime;
  ATotalWidth := 0;
  while (ATotalWidth < Bounds.Width) or (FMinorHeaders.Count = 0) do
  begin
    FMinorHeaders.Add(TdxGanttControlViewChartAreaMinorHeaderViewInfo.Create
      (Self, ADate));
    FMinorHeaders.Last.Calculate(R);
    ADate := GetNextMinorDateTime(ADate);
    if UseRightToLeftAlignment then
      R.Offset(-AWidth, 0)
    else
      R.Offset(AWidth, 0);
    Inc(ATotalWidth, AWidth);
  end;
end;

function TdxGanttControlViewChartAreaHeaderViewInfo.GetOwner
  : TdxGanttControlViewChartAreaViewInfo;
begin
  Result := TdxGanttControlViewChartAreaViewInfo(inherited Owner);
end;

{ TdxGanttControlChartView }

constructor TdxGanttControlChartView.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FOptionsGridline := TdxGanttControlGridlineOptions.Create(Self);
  FOptionsSheet := TdxGanttControlChartViewSheetOptions.Create(Self);
  FOptionsSheet.ChangedHandlers.Add(OptionsChangedHandler);
  FOptionsSplitter := TdxGanttControlChartViewSplitterOptions.Create(Self);
  FOptionsSplitter.ChangedHandlers.Add(OptionsChangedHandler);
end;

destructor TdxGanttControlChartView.Destroy;
begin
  FreeAndNil(FOptionsSplitter);
  FreeAndNil(FOptionsSheet);
  FreeAndNil(FOptionsGridline);
  inherited Destroy;
end;

function TdxGanttControlChartView.CreateController
  : TdxGanttControlViewCustomController;
begin
  Result := TdxGanttControlChartViewController.Create(Self);
end;

function TdxGanttControlChartView.CreateDataProvider
  : TdxGanttControlCustomDataProvider;
begin
  Result := TdxGanttControlChartViewDataProvider.Create(Owner);
end;

procedure TdxGanttControlChartView.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlChartView;
begin
  if Source is TdxGanttControlChartView then
  begin
    ASource := TdxGanttControlChartView(Source);
    OptionsSheet := ASource.OptionsSheet;
    OptionsSplitter := ASource.OptionsSplitter;
    ShowTaskProgress := ASource.ShowTaskProgress;
    TimescaleUnit := ASource.TimescaleUnit;
    FirstVisibleDateTime := ASource.FirstVisibleDateTime;
    OptionsGridline := ASource.OptionsGridline;
  end;
  inherited Assign(Source);
end;

function TdxGanttControlChartView.CreateViewInfo
  : TdxGanttControlViewCustomViewInfo;
begin
  Result := TdxGanttControlViewChartViewInfo.Create(Self);
end;

procedure TdxGanttControlChartView.DoReset;
begin
  inherited DoReset;
  OptionsGridline.Reset;
  FOptionsSheet.Reset;
  FOptionsSplitter.Reset;
  FTimescaleUnit := TdxGanttControlChartViewTimescaleUnit.Days;
  FirstVisibleDateTime := Now;
  FShowTaskProgress := True;
end;

procedure TdxGanttControlChartView.DoTimescaleChanged;
begin
  if Assigned(OnTimescaleChanged) then
    OnTimescaleChanged(Self);
end;

procedure TdxGanttControlChartView.DoFirstVisibleDateTimeChanged;
begin
  if Assigned(OnFirstVisibleDateTimeChanged) then
    OnFirstVisibleDateTimeChanged(Self);
end;

function TdxGanttControlChartView.GetController
  : TdxGanttControlChartViewController;
begin
  Result := TdxGanttControlChartViewController(inherited Controller);
end;

function TdxGanttControlChartView.GetDataProvider
  : TdxGanttControlCustomDataProvider;
begin
  Result := TdxGanttControlCustomDataProvider(inherited DataProvider);
end;

function TdxGanttControlChartView.GetFirstVisibleDateTime: TDateTime;
begin
  Result := Controller.ChartAreaController.FirstVisibleDateTime;
end;

function TdxGanttControlChartView.GetLastVisibleDateTime: TDateTime;
begin
  Result := Controller.ChartAreaController.GetLastVisibleDateTime;
end;

function TdxGanttControlChartView.GetType: TdxGanttControlViewType;
begin
  Result := TdxGanttControlViewType.Chart;
end;

procedure TdxGanttControlChartView.OptionsChangedHandler(Sender: TObject;
  AChanges: TdxGanttControlOptionsChangedTypes);
begin
  Changed(AChanges);
end;

procedure TdxGanttControlChartView.SetFirstVisibleDateTime
  (const Value: TDateTime);
begin
  TdxGanttControlChartViewController(Controller)
    .ChartAreaController.FirstVisibleDateTime := Value;
end;

procedure TdxGanttControlChartView.SetOptionsGridline
  (const Value: TdxGanttControlGridlineOptions);
begin
  FOptionsGridline.Assign(Value);
end;

procedure TdxGanttControlChartView.SetOptionsSheet
  (const Value: TdxGanttControlChartViewSheetOptions);
begin
  FOptionsSheet.Assign(Value);
end;

procedure TdxGanttControlChartView.SetOptionsSplitter
  (const Value: TdxGanttControlSplitterOptions);
begin
  FOptionsSplitter.Assign(Value);
end;

procedure TdxGanttControlChartView.SetShowTaskProgress(const Value: Boolean);
begin
  if ShowTaskProgress <> Value then
  begin
    FShowTaskProgress := Value;
    Changed([TdxGanttControlOptionsChangedType.View]);
  end;
end;

procedure TdxGanttControlChartView.SetTimescaleUnit
  (const Value: TdxGanttControlChartViewTimescaleUnit);
begin
  if TimescaleUnit <> Value then
  begin
    FTimescaleUnit := Value;
    Changed([TdxGanttControlOptionsChangedType.Cache,
      TdxGanttControlOptionsChangedType.Size]);
  end;
end;

end.
