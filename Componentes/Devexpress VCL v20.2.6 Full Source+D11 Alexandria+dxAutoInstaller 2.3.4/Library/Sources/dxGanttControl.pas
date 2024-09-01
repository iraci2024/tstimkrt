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

unit dxGanttControl;

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
  cxClasses, dxCore, dxCoreClasses, cxControls, cxGeometry, cxLookAndFeels,
  cxLookAndFeelPainters,
  cxGraphics, cxCustomCanvas, dxTouch, dxGDIPlusClasses, dxBuiltInPopupMenu,
  dxGanttControlCustomClasses,
  dxGanttControlCommands,
  dxGanttControlCustomDataModel,
  dxGanttControlDataModel,
  dxGanttControlCalendars,
  dxGanttControlTasks,
  dxGanttControlAssignments,
  dxGanttControlResources,
  dxGanttControlCustomSheet,
  dxGanttControlSplitter,
  dxGanttControlCustomView,
  dxGanttControlViewChart,
  dxGanttControlViewResourceSheet,
  dxGanttControlViewTimeline;

type
  TdxCustomGanttControl = class;
  TdxGanttControlViewInfo = class;

  { TdxGanttControlController }

  TdxGanttControlController = class(TdxGanttControlCustomController)
  strict private
    FActiveViewController: TdxGanttControlViewCustomController;
    FKeyboardController: TdxGanttControlViewCustomController;
    function GetControl: TdxCustomGanttControl; inline;
  protected
    function GetDragHelper: TdxGanttControlDragHelper; override;
    procedure HideEditing; override;

    procedure DoCreateScrollBars; override;
    procedure DoDestroyScrollBars; override;
    function GetGestureClient(const APoint: TPoint): IdxGestureClient; override;
    function GetTouchScrollUIOwner(const APoint: TPoint)
      : IdxTouchScrollUIOwner; override;
    procedure InitScrollbars; override;
    function IsMouseWheelHandleNeeded(const MousePos: TPoint): Boolean;
      override;
    function ProcessNCSizeChanged: Boolean; override;
    procedure UnInitScrollbars; override;

    procedure DoClick; override;
    procedure DoDblClick; override;
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure DoMouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; AIsIncrement: Boolean;
      const AMousePos: TPoint): Boolean; override;

    procedure DoKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoKeyPress(var Key: Char); override;
    procedure DoKeyUp(var Key: Word; Shift: TShiftState); override;

    function GetDesignHitTest(X, Y: Integer; Shift: TShiftState)
      : Boolean; override;

    function InitializeBuiltInPopupMenu(APopupMenu: TdxGanttControlPopupMenu;
      var P: TPoint): Boolean;

    procedure CheckActiveViewController;
    property ActiveViewController: TdxGanttControlViewCustomController
      read FActiveViewController;
  public
    property Control: TdxCustomGanttControl read GetControl;
  end;

  { TdxGanttControlCanvasCache }

  TdxGanttControlCanvasCache = class(TdxGanttControlCanvasCustomCache)
  strict private
    FControlViewInfo: TdxGanttControlViewInfo;
  protected
    function GetCanvas: TcxCustomCanvas; override;
  public
    constructor Create(AControlViewInfo: TdxGanttControlViewInfo); reintroduce;

    function GetBaseFont: TFont; override;

    property ControlViewInfo: TdxGanttControlViewInfo read FControlViewInfo;
  end;

  { TdxGanttControlViewInfo }

  TdxGanttControlViewInfo = class(TdxGanttControlCustomViewInfo)
  strict private
    FCanvasCache: TdxGanttControlCanvasCache;
    FControl: TdxCustomGanttControl;
  protected
    function CalculateItemBounds(AItem: TdxGanttControlCustomItemViewInfo)
      : TRect; override;
    function GetCanvasCache: TdxGanttControlCanvasCustomCache; override;
    function GetLookAndFeelPainter: TcxCustomLookAndFeelPainter; override;
    function GetScaleFactor: TdxScaleFactor; override;
    function GetUseRightToLeftAlignment: Boolean; override;
    procedure DoDraw; override;
    procedure Invalidate(const R: TRect); override;
  public
    constructor Create(AControl: TdxCustomGanttControl); reintroduce;
    destructor Destroy; override;

    procedure Reset; override;

    procedure CalculateLayout; override;

    property Control: TdxCustomGanttControl read FControl;
  end;

  { TdxCustomGanttControl }

  TdxShowTaskDependencyDialogEvent = procedure(Sender: TObject;
    ALink: TdxGanttControlTaskPredecessorLink; var AHandled: Boolean) of object;
  TdxShowTaskInformationDialogEvent = procedure(Sender: TObject;
    ATask: TdxGanttControlTask; var AHandled: Boolean) of object;
  TdxGanttControlViewChangedEvent = procedure(Sender: TObject;
    AView: TdxGanttControlCustomView) of object;
  TdxGanttControlTaskEvent = procedure(Sender: TObject;
    ATask: TdxGanttControlTask) of object;
  TdxGanttControlResourceEvent = procedure(Sender: TObject;
    AResource: TdxGanttControlResource) of object;
  TdxGanttControlAssignmentEvent = procedure(Sender: TObject;
    AAssignment: TdxGanttControlAssignment) of object;

  TdxCustomGanttControl = class(TdxGanttControlBase)
  strict private
    FActiveViewType: TdxGanttControlViewType;
    FBuiltInPopupMenu: TdxGanttControlPopupMenu;
    FDataModel: TdxGanttControlDataModel;

    FViewChart: TdxGanttControlChartView;
    FViewResourceSheet: TdxGanttControlResourceSheetView;
    FViewTimeline: TdxGanttControlTimelineView;

    FOnActiveViewChanged: TNotifyEvent;
    FOnDataModelLoaded: TNotifyEvent;
    FOnShowTaskDependencyDialog: TdxShowTaskDependencyDialogEvent;
    FOnShowTaskInformationDialog: TdxShowTaskInformationDialogEvent;
    FOnViewChanged: TdxGanttControlViewChangedEvent;

    FOnAssignmentChanged: TdxGanttControlAssignmentEvent;
    FOnAssignmentDeleted: TdxGanttControlAssignmentEvent;
    FOnAssignmentInserted: TdxGanttControlAssignmentEvent;
    FOnResourceChanged: TdxGanttControlResourceEvent;
    FOnResourceDeleted: TdxGanttControlResourceEvent;
    FOnResourceInserted: TdxGanttControlResourceEvent;
    FOnTaskChanged: TdxGanttControlTaskEvent;
    FOnTaskDeleted: TdxGanttControlTaskEvent;
    FOnTaskInserted: TdxGanttControlTaskEvent;

    FViewTimelineEvents: TNotifyEvent;
    FViewResourceSheetEvents: TNotifyEvent;
    FViewChartEvents: TNotifyEvent;

    procedure AssignmentChangedHandler(Sender: TdxGanttControlDataModel;
      AAssignment: TdxGanttControlAssignment);
    procedure AssignmentsChangedHandler(Sender: TObject;
      const AItem: TdxGanttControlModelElementListItem;
      AAction: TCollectionNotification);
    procedure DataModelAfterResetHandler(Sender: TObject);
    procedure DataModelLoadedHandler(Sender: TObject);
    procedure ResourceChangedHandler(Sender: TdxGanttControlDataModel;
      AResource: TdxGanttControlResource);
    procedure ResourcesChangedHandler(Sender: TObject;
      const AItem: TdxGanttControlModelElementListItem;
      AAction: TCollectionNotification);
    procedure TaskChangedHandler(Sender: TdxGanttControlDataModel;
      ATask: TdxGanttControlTask);
    procedure TasksChangedHandler(Sender: TObject;
      const AItem: TdxGanttControlModelElementListItem;
      AAction: TCollectionNotification);

    procedure SetActiveViewType(const Value: TdxGanttControlViewType);
    procedure SetViewChart(const Value: TdxGanttControlChartView);
    procedure SetViewResourceSheet(const Value
      : TdxGanttControlResourceSheetView);
    procedure SetViewTimeline(const Value: TdxGanttControlTimelineView);
    function GetActiveView: TdxGanttControlCustomView;
    function GetController: TdxGanttControlController; inline;
    function GetViewInfo: TdxGanttControlViewInfo; inline;
  protected
    function CreateController: TdxGanttControlCustomController; override;

    function CreateViewInfo: TdxGanttControlCustomViewInfo; override;
    procedure CreateSubClasses; override;
    procedure DoInitialize; override;
    procedure DestroySubClasses; override;

    function CreateDataModel: TdxGanttControlDataModel; virtual;
    procedure SubscribeDataModelEvents;
    procedure UnsubscribeDataModelEvents;

    procedure DoCheckChanges(AChanges: TdxGanttControlChangedTypes); override;
    function DoShowTaskDependencyDialog
      (ALink: TdxGanttControlTaskPredecessorLink): Boolean; virtual;
    function DoShowTaskInformationDialog(ATask: TdxGanttControlTask)
      : Boolean; virtual;
    procedure DoAssignmentChanged(AAssignment: TdxGanttControlAssignment);
    procedure DoAssignmentDeleted(AAssignment: TdxGanttControlAssignment);
    procedure DoAssignmentInserted(AAssignment: TdxGanttControlAssignment);
    procedure DoResourceChanged(AResource: TdxGanttControlResource);
    procedure DoResourceDeleted(AResource: TdxGanttControlResource);
    procedure DoResourceInserted(AResource: TdxGanttControlResource);
    procedure DoSetActiveViewType(const Value: TdxGanttControlViewType);
    procedure DoTaskChanged(ATask: TdxGanttControlTask);
    procedure DoTaskDeleted(ATask: TdxGanttControlTask);
    procedure DoTaskInserted(ATask: TdxGanttControlTask);

    procedure ViewChanged(AView: TdxGanttControlCustomView;
      AChanges: TdxGanttControlOptionsChangedTypes);
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;

    property ActiveViewType: TdxGanttControlViewType read FActiveViewType
      write SetActiveViewType;
    property ViewInfo: TdxGanttControlViewInfo read GetViewInfo;
    property OnViewChanged: TdxGanttControlViewChangedEvent read FOnViewChanged
      write FOnViewChanged;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;

    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(const AStream: TStream);
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(const AStream: TStream);

    property ActiveView: TdxGanttControlCustomView read GetActiveView;
    property DataModel: TdxGanttControlDataModel read FDataModel;
    property Controller: TdxGanttControlController read GetController;
    // for internal use

    property ViewChart: TdxGanttControlChartView read FViewChart
      write SetViewChart;
    property ViewResourceSheet: TdxGanttControlResourceSheetView
      read FViewResourceSheet write SetViewResourceSheet;
    property ViewTimeline: TdxGanttControlTimelineView read FViewTimeline
      write SetViewTimeline;

    property OnActiveViewChanged: TNotifyEvent read FOnActiveViewChanged
      write FOnActiveViewChanged;
    property OnDataModelLoaded: TNotifyEvent read FOnDataModelLoaded
      write FOnDataModelLoaded;
    property OnShowTaskDependencyDialog: TdxShowTaskDependencyDialogEvent
      read FOnShowTaskDependencyDialog write FOnShowTaskDependencyDialog;
    property OnShowTaskInformationDialog: TdxShowTaskInformationDialogEvent
      read FOnShowTaskInformationDialog write FOnShowTaskInformationDialog;
    property OnAssignmentChanged: TdxGanttControlAssignmentEvent
      read FOnAssignmentChanged write FOnAssignmentChanged;
    property OnAssignmentDeleted: TdxGanttControlAssignmentEvent
      read FOnAssignmentDeleted write FOnAssignmentDeleted;
    property OnAssignmentInserted: TdxGanttControlAssignmentEvent
      read FOnAssignmentInserted write FOnAssignmentInserted;
    property OnResourceChanged: TdxGanttControlResourceEvent
      read FOnResourceChanged write FOnResourceChanged;
    property OnResourceDeleted: TdxGanttControlResourceEvent
      read FOnResourceDeleted write FOnResourceDeleted;
    property OnResourceInserted: TdxGanttControlResourceEvent
      read FOnResourceInserted write FOnResourceInserted;
    property OnTaskChanged: TdxGanttControlTaskEvent read FOnTaskChanged
      write FOnTaskChanged;
    property OnTaskDeleted: TdxGanttControlTaskEvent read FOnTaskDeleted
      write FOnTaskDeleted;
    property OnTaskInserted: TdxGanttControlTaskEvent read FOnTaskInserted
      write FOnTaskInserted;
    // nested events
    property ViewChartEvents: TNotifyEvent read FViewChartEvents
      write FViewChartEvents;
    property ViewResourceSheetEvents: TNotifyEvent read FViewResourceSheetEvents
      write FViewResourceSheetEvents;
    property ViewTimelineEvents: TNotifyEvent read FViewTimelineEvents
      write FViewTimelineEvents;
  end;

  { TdxGanttControl }

  TdxGanttControl = class(TdxCustomGanttControl)
  published
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle default cxcbsDefault;
    property BorderWidth;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode default dmAutomatic;
    property Enabled;
    property Font;
    property LookAndFeel;
    property OptionsBehavior;
    property ParentBiDiMode;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property ViewChart;
    property ViewResourceSheet;
    property ViewTimeline;
    property Visible;

    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;

    property OnActiveViewChanged;
    property OnDataModelLoaded;
    property OnShowTaskDependencyDialog;
    property OnShowTaskInformationDialog;
    property OnAssignmentChanged;
    property OnAssignmentDeleted;
    property OnAssignmentInserted;
    property OnResourceChanged;
    property OnResourceDeleted;
    property OnResourceInserted;
    property OnTaskChanged;
    property OnTaskDeleted;
    property OnTaskInserted;

    // nested events
    property ViewChartEvents;
    property ViewResourceSheetEvents;
    property ViewTimelineEvents;
  end;

implementation

uses
  Math, Menus,
  dxTypeHelpers, dxCustomHint,
  dxGanttControlViewCommands;

type
  TdxGanttControlCustomControllerAccess = class
    (TdxGanttControlCustomController);
  TdxGanttControlViewCustomControllerAccess = class
    (TdxGanttControlViewCustomController);
  TdxGanttControlCustomViewAccess = class(TdxGanttControlCustomView);

  { TdxGanttControlController }

procedure TdxGanttControlController.CheckActiveViewController;
begin
  if Control.ActiveView = nil then
    FActiveViewController := nil
  else
    FActiveViewController := Control.ActiveView.Controller;
  if FKeyboardController <> FActiveViewController then
    FKeyboardController := FActiveViewController
end;

function TdxGanttControlController.GetControl: TdxCustomGanttControl;
begin
  Result := TdxCustomGanttControl(inherited Control);
end;

function TdxGanttControlController.GetDragHelper: TdxGanttControlDragHelper;
begin
  if ActiveViewController <> nil then
    Result := TdxGanttControlCustomControllerAccess(ActiveViewController)
      .DragHelper
  else
    Result := inherited GetDragHelper;
end;

procedure TdxGanttControlController.HideEditing;
begin
  inherited HideEditing;
  if ActiveViewController <> nil then
    TdxGanttControlCustomControllerAccess(ActiveViewController).HideEditing;
end;

function TdxGanttControlController.InitializeBuiltInPopupMenu
  (APopupMenu: TdxGanttControlPopupMenu; var P: TPoint): Boolean;
begin
  Result := (ActiveViewController <> nil) and
    TdxGanttControlViewCustomControllerAccess(ActiveViewController)
    .InitializeBuiltInPopupMenu(APopupMenu, P);
end;

procedure TdxGanttControlController.InitScrollbars;
begin
  inherited InitScrollbars;
  TdxGanttControlCustomControllerAccess(ActiveViewController).InitScrollbars;
end;

function TdxGanttControlController.IsMouseWheelHandleNeeded(const MousePos
  : TPoint): Boolean;
begin
  Result := TdxGanttControlCustomControllerAccess(ActiveViewController)
    .IsMouseWheelHandleNeeded(MousePos);
end;

function TdxGanttControlController.ProcessNCSizeChanged: Boolean;
begin
  Result := (ActiveViewController <> nil) and
    TdxGanttControlCustomControllerAccess(ActiveViewController)
    .ProcessNCSizeChanged;
end;

procedure TdxGanttControlController.UnInitScrollbars;
begin
  inherited UnInitScrollbars;
  TdxGanttControlCustomControllerAccess(ActiveViewController).UnInitScrollbars;
end;

procedure TdxGanttControlController.DoCreateScrollBars;
begin
  inherited DoCreateScrollBars;
  if ActiveViewController <> nil then
    TdxGanttControlCustomControllerAccess(ActiveViewController)
      .DoCreateScrollBars;
end;

procedure TdxGanttControlController.DoDestroyScrollBars;
begin
  inherited DoDestroyScrollBars;
  if ActiveViewController <> nil then
    TdxGanttControlCustomControllerAccess(ActiveViewController)
      .DoDestroyScrollBars;
end;

function TdxGanttControlController.GetGestureClient(const APoint: TPoint)
  : IdxGestureClient;
begin
  if ActiveViewController <> nil then
    Result := TdxGanttControlCustomControllerAccess(ActiveViewController)
      .GetGestureClient(APoint)
  else
    Result := inherited GetGestureClient(APoint);
end;

function TdxGanttControlController.GetTouchScrollUIOwner(const APoint: TPoint)
  : IdxTouchScrollUIOwner;
begin
  if ActiveViewController <> nil then
    Result := TdxGanttControlCustomControllerAccess(ActiveViewController)
      .GetTouchScrollUIOwner(APoint)
  else
    Result := inherited GetTouchScrollUIOwner(APoint);
end;

procedure TdxGanttControlController.DoClick;
begin
  inherited DoClick;
  if ActiveViewController <> nil then
    ActiveViewController.Click;
end;

procedure TdxGanttControlController.DoDblClick;
begin
  inherited DoDblClick;
  if ActiveViewController <> nil then
    ActiveViewController.DblClick;
end;

procedure TdxGanttControlController.DoMouseDown(Button: TMouseButton;
  Shift: TShiftState; X: Integer; Y: Integer);
begin
  inherited DoMouseDown(Button, Shift, X, Y);
  if ActiveViewController <> nil then
    ActiveViewController.MouseDown(Button, Shift, X, Y);
  FKeyboardController := ActiveViewController;
end;

procedure TdxGanttControlController.DoMouseMove(Shift: TShiftState; X: Integer;
  Y: Integer);
begin
  inherited DoMouseMove(Shift, X, Y);
  if ActiveViewController <> nil then
    ActiveViewController.MouseMove(Shift, X, Y);
end;

procedure TdxGanttControlController.DoMouseUp(Button: TMouseButton;
  Shift: TShiftState; X: Integer; Y: Integer);
begin
  inherited DoMouseUp(Button, Shift, X, Y);
  if ActiveViewController <> nil then
    ActiveViewController.MouseUp(Button, Shift, X, Y);
end;

function TdxGanttControlController.DoMouseWheel(Shift: TShiftState;
  AIsIncrement: Boolean; const AMousePos: TPoint): Boolean;
begin
  Result := ActiveViewController.MouseWheel(Shift, AIsIncrement, AMousePos);
end;

procedure TdxGanttControlController.DoKeyDown(var Key: Word;
  Shift: TShiftState);
begin
  inherited DoKeyDown(Key, Shift);
  if (Key <> 0) and (FKeyboardController <> nil) then
    FKeyboardController.KeyDown(Key, Shift);
  if (Key <> 0) and (Shift = [ssCtrl]) then
    if Key = Ord('Z') then
      Control.History.Undo
    else if Key = Ord('Y') then
      Control.History.Redo;
end;

procedure TdxGanttControlController.DoKeyPress(var Key: Char);
begin
  inherited DoKeyPress(Key);
  if (Key <> #0) and (FKeyboardController <> nil) then
    FKeyboardController.KeyPress(Key);
end;

procedure TdxGanttControlController.DoKeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited DoKeyUp(Key, Shift);
  if (Key <> 0) and (FKeyboardController <> nil) then
    FKeyboardController.KeyUp(Key, Shift);
end;

function TdxGanttControlController.GetDesignHitTest(X, Y: Integer;
  Shift: TShiftState): Boolean;
begin
  Result := (ActiveViewController <> nil) and
    TdxGanttControlCustomControllerAccess(ActiveViewController)
    .GetDesignHitTest(X, Y, Shift);
end;

{ TdxGanttControlCanvasCache }

constructor TdxGanttControlCanvasCache.Create(AControlViewInfo
  : TdxGanttControlViewInfo);
begin
  inherited Create;
  FControlViewInfo := AControlViewInfo;
end;

function TdxGanttControlCanvasCache.GetBaseFont: TFont;
begin
  Result := ControlViewInfo.Control.Font;
end;

function TdxGanttControlCanvasCache.GetCanvas: TcxCustomCanvas;
begin
  Result := ControlViewInfo.Control.ActualCanvas;
end;

{ TdxGanttControlViewInfo }

constructor TdxGanttControlViewInfo.Create(AControl: TdxCustomGanttControl);
begin
  inherited Create(nil);
  FControl := AControl;
  FCanvasCache := TdxGanttControlCanvasCache.Create(Self);
end;

destructor TdxGanttControlViewInfo.Destroy;
begin
  FreeAndNil(FCanvasCache);
  inherited Destroy;
end;

procedure TdxGanttControlViewInfo.DoDraw;
begin
  if Control.ActiveView = nil then
    Canvas.FillRect(Bounds, LookAndFeelPainter.DefaultContentColor);
  inherited DoDraw;
end;

procedure TdxGanttControlViewInfo.Reset;
begin
  inherited Reset;
  FCanvasCache.Clear;
end;

procedure TdxGanttControlViewInfo.CalculateLayout;
begin
  Clear;
  if Control.ActiveView <> nil then
    AddChild(TdxGanttControlCustomViewAccess(Control.ActiveView)
      .CreateViewInfo);
  inherited CalculateLayout;
end;

function TdxGanttControlViewInfo.CalculateItemBounds
  (AItem: TdxGanttControlCustomItemViewInfo): TRect;
begin
  Result := Bounds;
end;

function TdxGanttControlViewInfo.GetCanvasCache
  : TdxGanttControlCanvasCustomCache;
begin
  Result := FCanvasCache;
end;

function TdxGanttControlViewInfo.GetLookAndFeelPainter
  : TcxCustomLookAndFeelPainter;
begin
  Result := Control.LookAndFeelPainter;
end;

function TdxGanttControlViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := Control.ScaleFactor;
end;

procedure TdxGanttControlViewInfo.Invalidate(const R: TRect);
begin
  Control.InvalidateRect(R, False);
end;

function TdxGanttControlViewInfo.GetUseRightToLeftAlignment: Boolean;
begin
  Result := Control.UseRightToLeftAlignment;
end;

{ TdxCustomGanttControl }

constructor TdxCustomGanttControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TdxCustomGanttControl.Destroy;
begin
  FreeAndNil(FBuiltInPopupMenu);
  inherited Destroy;
end;

procedure TdxCustomGanttControl.AfterConstruction;
begin
  BeginUpdate;
  try
    inherited AfterConstruction;
    Initialize;
  finally
    EndUpdate;
  end;
end;

procedure TdxCustomGanttControl.LoadFromFile(const AFileName: string);
begin
  DataModel.LoadFromFile(AFileName);
end;

procedure TdxCustomGanttControl.LoadFromStream(const AStream: TStream);
begin
  DataModel.LoadFromStream(AStream);
end;

procedure TdxCustomGanttControl.SaveToFile(const AFileName: string);
begin
  DataModel.SaveToFile(AFileName);
end;

procedure TdxCustomGanttControl.SaveToStream(const AStream: TStream);
begin
  DataModel.SaveToStream(AStream);
end;

function TdxCustomGanttControl.CreateDataModel: TdxGanttControlDataModel;
begin
  Result := TdxGanttControlDataModel.Create;
end;

procedure TdxCustomGanttControl.SubscribeDataModelEvents;
begin
  DataModel.TaskChangedHandlers.Add(TaskChangedHandler);
  DataModel.ResourceChangedHandlers.Add(ResourceChangedHandler);
  DataModel.AssignmentChangedHandlers.Add(AssignmentChangedHandler);
  DataModel.AfterResetHandlers.Add(DataModelAfterResetHandler);
  DataModel.LoadedHandlers.Add(DataModelLoadedHandler);

  DataModel.Tasks.ListChangedHandlers.Add(TasksChangedHandler);
  DataModel.Resources.ListChangedHandlers.Add(ResourcesChangedHandler);
  DataModel.Assignments.ListChangedHandlers.Add(AssignmentsChangedHandler);
end;

procedure TdxCustomGanttControl.UnsubscribeDataModelEvents;
begin
  DataModel.TaskChangedHandlers.Remove(TaskChangedHandler);
  DataModel.ResourceChangedHandlers.Remove(ResourceChangedHandler);
  DataModel.AssignmentChangedHandlers.Remove(AssignmentChangedHandler);
  DataModel.AfterResetHandlers.Remove(DataModelAfterResetHandler);
  DataModel.LoadedHandlers.Remove(DataModelLoadedHandler);

  DataModel.Tasks.ListChangedHandlers.Remove(TasksChangedHandler);
  DataModel.Resources.ListChangedHandlers.Remove(ResourcesChangedHandler);
  DataModel.Assignments.ListChangedHandlers.Remove(AssignmentsChangedHandler);
end;

function TdxCustomGanttControl.CreateController
  : TdxGanttControlCustomController;
begin
  Result := TdxGanttControlController.Create(Self);
end;

function TdxCustomGanttControl.GetActiveView: TdxGanttControlCustomView;
begin
  case ActiveViewType of
    TdxGanttControlViewType.Chart:
      Result := ViewChart;
    TdxGanttControlViewType.Timeline:
      Result := ViewTimeline;
    TdxGanttControlViewType.ResourceSheet:
      Result := ViewResourceSheet;
  else
    Result := nil;
  end;
end;

function TdxCustomGanttControl.GetController: TdxGanttControlController;
begin
  Result := TdxGanttControlController(inherited Controller);
end;

function TdxCustomGanttControl.GetViewInfo: TdxGanttControlViewInfo;
begin
  Result := TdxGanttControlViewInfo(inherited ViewInfo);
end;

procedure TdxCustomGanttControl.DoCheckChanges
  (AChanges: TdxGanttControlChangedTypes);
begin
  if TdxGanttControlChangedType.Layout in AChanges then
    Controller.CheckActiveViewController;
  if (TdxGanttControlChangedType.Data in AChanges) and (ActiveView <> nil) then
    ActiveView.DataProvider.Refresh(False);
  inherited DoCheckChanges(AChanges);
end;

procedure TdxCustomGanttControl.DoContextPopup(MousePos: TPoint;
  var Handled: Boolean);
var
  ACapture: THandle;
  P: TPoint;
begin
  ACapture := GetCapture;
  if (ACapture <> 0) and (ACapture <> Handle) then
    Exit;
  inherited DoContextPopup(MousePos, Handled);
  if not Handled then
    Handled := DoShowPopupMenu(PopupMenu, P.X, P.Y);
  if not Handled { and Options.Behavior.ShowPopupMenuAllowed } then
  begin
    if FBuiltInPopupMenu = nil then
      FBuiltInPopupMenu := TdxGanttControlPopupMenu.Create(Self);
    FBuiltInPopupMenu.Popup(MousePos);
    Handled := True;
  end;
end;

procedure TdxCustomGanttControl.DoInitialize;
begin
  inherited DoInitialize;
  SetActiveViewType(TdxGanttControlViewType.Chart);
end;

procedure TdxCustomGanttControl.ViewChanged(AView: TdxGanttControlCustomView;
  AChanges: TdxGanttControlOptionsChangedTypes);

  function ViewChangesToChanges(AChanges: TdxGanttControlOptionsChangedTypes)
    : TdxGanttControlChangedTypes;
  begin
    Result := [];
    if TdxGanttControlOptionsChangedType.Layout in AChanges then
      Include(Result, TdxGanttControlChangedType.Layout);
    if TdxGanttControlOptionsChangedType.Size in AChanges then
      Include(Result, TdxGanttControlChangedType.Size);
    if TdxGanttControlOptionsChangedType.Cache in AChanges then
      Include(Result, TdxGanttControlChangedType.Cache);
    if TdxGanttControlOptionsChangedType.View in AChanges then
      Include(Result, TdxGanttControlChangedType.View);
  end;

begin
  if AView.Active then
    Changed(ViewChangesToChanges(AChanges));
  if not IsLoading and Assigned(FOnViewChanged) then
    FOnViewChanged(Self, AView);
end;

procedure TdxCustomGanttControl.SetActiveViewType
  (const Value: TdxGanttControlViewType);
begin
  TdxGanttControlActivateViewCommand.SetActiveView(Self, Value);
end;

procedure TdxCustomGanttControl.AssignmentChangedHandler
  (Sender: TdxGanttControlDataModel; AAssignment: TdxGanttControlAssignment);
begin
  Changed([TdxGanttControlChangedType.Data]);
  DoAssignmentChanged(AAssignment);
end;

procedure TdxCustomGanttControl.AssignmentsChangedHandler(Sender: TObject;
  const AItem: TdxGanttControlModelElementListItem;
  AAction: TCollectionNotification);
begin
  case AAction of
    cnAdded:
      DoAssignmentInserted(TdxGanttControlAssignment(AItem));
    cnExtracted, cnRemoved:
      DoAssignmentDeleted(TdxGanttControlAssignment(AItem));
  end;
end;

procedure TdxCustomGanttControl.DataModelAfterResetHandler(Sender: TObject);
begin
  History.Clear;
end;

procedure TdxCustomGanttControl.DataModelLoadedHandler(Sender: TObject);
begin
  ViewChart.FirstVisibleDateTime := DataModel.RealProjectStart;
  if Assigned(OnDataModelLoaded) then
    OnDataModelLoaded(Self);
end;

procedure TdxCustomGanttControl.ResourceChangedHandler
  (Sender: TdxGanttControlDataModel; AResource: TdxGanttControlResource);
begin
  Changed([TdxGanttControlChangedType.Data]);
  DoResourceChanged(AResource);
end;

procedure TdxCustomGanttControl.ResourcesChangedHandler(Sender: TObject;
  const AItem: TdxGanttControlModelElementListItem;
  AAction: TCollectionNotification);
begin
  case AAction of
    cnAdded:
      DoResourceInserted(TdxGanttControlResource(AItem));
    cnExtracted, cnRemoved:
      DoResourceDeleted(TdxGanttControlResource(AItem));
  end;
end;

procedure TdxCustomGanttControl.TaskChangedHandler
  (Sender: TdxGanttControlDataModel; ATask: TdxGanttControlTask);
begin
  Changed([TdxGanttControlChangedType.Data]);
  DoTaskChanged(ATask);
end;

procedure TdxCustomGanttControl.TasksChangedHandler(Sender: TObject;
  const AItem: TdxGanttControlModelElementListItem;
  AAction: TCollectionNotification);
begin
  case AAction of
    cnAdded:
      DoTaskInserted(TdxGanttControlTask(AItem));
    cnExtracted, cnRemoved:
      DoTaskDeleted(TdxGanttControlTask(AItem));
  end;
end;

procedure TdxCustomGanttControl.SetViewChart(const Value
  : TdxGanttControlChartView);
begin
  ViewChart.Assign(Value);
end;

procedure TdxCustomGanttControl.SetViewResourceSheet
  (const Value: TdxGanttControlResourceSheetView);
begin
  ViewResourceSheet.Assign(Value);
end;

procedure TdxCustomGanttControl.SetViewTimeline(const Value
  : TdxGanttControlTimelineView);
begin
  ViewTimeline.Assign(Value);
end;

function TdxCustomGanttControl.CreateViewInfo: TdxGanttControlCustomViewInfo;
begin
  Result := TdxGanttControlViewInfo.Create(Self);
end;

procedure TdxCustomGanttControl.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FDataModel := CreateDataModel;
  SubscribeDataModelEvents;
  FViewChart := TdxGanttControlChartView.Create(Self);
  FViewResourceSheet := TdxGanttControlResourceSheetView.Create(Self);
  FViewTimeline := TdxGanttControlTimelineView.Create(Self);
end;

procedure TdxCustomGanttControl.DestroySubClasses;
begin
  FreeAndNil(FViewChart);
  FreeAndNil(FViewResourceSheet);
  FreeAndNil(FViewTimeline);
  UnsubscribeDataModelEvents;
  FreeAndNil(FDataModel);
  inherited DestroySubClasses;
end;

function TdxCustomGanttControl.DoShowTaskDependencyDialog
  (ALink: TdxGanttControlTaskPredecessorLink): Boolean;
begin
  Result := False;
  if Assigned(OnShowTaskDependencyDialog) then
    OnShowTaskDependencyDialog(Self, ALink, Result);
end;

function TdxCustomGanttControl.DoShowTaskInformationDialog
  (ATask: TdxGanttControlTask): Boolean;
begin
  Result := False;
  if Assigned(OnShowTaskInformationDialog) then
    OnShowTaskInformationDialog(Self, ATask, Result);
end;

procedure TdxCustomGanttControl.DoAssignmentChanged
  (AAssignment: TdxGanttControlAssignment);
begin
  if Assigned(OnAssignmentChanged) then
    OnAssignmentChanged(Self, AAssignment);
end;

procedure TdxCustomGanttControl.DoAssignmentDeleted
  (AAssignment: TdxGanttControlAssignment);
begin
  if Assigned(OnAssignmentDeleted) then
    OnAssignmentDeleted(Self, AAssignment);
end;

procedure TdxCustomGanttControl.DoAssignmentInserted
  (AAssignment: TdxGanttControlAssignment);
begin
  if Assigned(OnAssignmentInserted) then
    OnAssignmentInserted(Self, AAssignment);
end;

procedure TdxCustomGanttControl.DoResourceChanged
  (AResource: TdxGanttControlResource);
begin
  if Assigned(OnResourceChanged) then
    OnResourceChanged(Self, AResource);
end;

procedure TdxCustomGanttControl.DoResourceDeleted
  (AResource: TdxGanttControlResource);
begin
  if Assigned(OnResourceDeleted) then
    OnResourceDeleted(Self, AResource);
end;

procedure TdxCustomGanttControl.DoResourceInserted
  (AResource: TdxGanttControlResource);
begin
  if Assigned(OnResourceInserted) then
    OnResourceInserted(Self, AResource);
end;

procedure TdxCustomGanttControl.DoSetActiveViewType
  (const Value: TdxGanttControlViewType);
begin
  if ActiveView <> nil then
    TdxGanttControlCustomControllerAccess(ActiveView.Controller).Deactivated;
  FActiveViewType := Value;
  if ActiveView <> nil then
  begin
    ActiveView.DataProvider.Refresh(True);
    TdxGanttControlCustomControllerAccess(ActiveView.Controller).Activated;
  end;
  Changed([TdxGanttControlChangedType.Layout]);
  if not IsLoading then
    dxCallNotify(FOnActiveViewChanged, Self);
end;

procedure TdxCustomGanttControl.DoTaskChanged(ATask: TdxGanttControlTask);
begin
  if Assigned(OnTaskChanged) then
    OnTaskChanged(Self, ATask);
end;

procedure TdxCustomGanttControl.DoTaskDeleted(ATask: TdxGanttControlTask);
begin
  if Assigned(OnTaskDeleted) then
    OnTaskDeleted(Self, ATask);
end;

procedure TdxCustomGanttControl.DoTaskInserted(ATask: TdxGanttControlTask);
begin
  if Assigned(OnTaskInserted) then
    OnTaskInserted(Self, ATask);
end;

end.
