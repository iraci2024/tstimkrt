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

unit dxGanttControlCustomView;

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
  dxGanttControlCustomSheet,
  dxGanttControlSplitter,
  dxGanttControlCalendars,
  dxGanttControlDataModel,
  dxGanttControlTasks,
  dxGanttControlAssignments,
  dxGanttControlResources;

type
  TdxGanttControlCustomView = class;
  TdxGanttControlViewCustomController = class;

  { TdxGanttControlViewCommand }

  TdxGanttControlViewCommand = class abstract(TdxGanttControlCommand)
  strict private
    function GetController: TdxGanttControlViewCustomController;
  protected
    function GetView: TdxGanttControlCustomView; virtual; abstract;

    property Controller: TdxGanttControlViewCustomController read GetController;
  public
    function GetMenuCaption: string; virtual;
    function GetMenuDescription: string; virtual;
    function GetMenuImage: TdxSmartImage; virtual;
    function IsChecked: Boolean; virtual;

    property View: TdxGanttControlCustomView read GetView;
  end;

  TdxGanttControlViewCommandClass = class of TdxGanttControlViewCommand;

  { TdxGanttControlTaskViewInfoCachedValues }

  TdxGanttControlTaskViewInfoCachedValues = class // for internal use
  protected
    FManualTaskColor: TColor;
    FMilestoneColor: TColor;
    FMilestoneSize: TSize;
    FTaskColor: TColor;
    FTaskHeight: Integer;
    FSummaryColor: TColor;
    FSummaryHeight: Integer;
  public
    procedure Update(APainter: TcxCustomLookAndFeelPainter;
      AScaleFactor: TdxScaleFactor); virtual;
    property ManualTaskColor: TColor read FManualTaskColor;
    property MilestoneColor: TColor read FMilestoneColor;
    property MilestoneSize: TSize read FMilestoneSize;
    property SummaryColor: TColor read FSummaryColor;
    property SummaryHeight: Integer read FSummaryHeight;
    property TaskColor: TColor read FTaskColor;
    property TaskHeight: Integer read FTaskHeight;
  end;

  { TdxGanttControlTaskCustomViewInfo }

  TdxGanttControlTaskCustomViewInfo = class
    (TdxGanttControlCustomOwnedItemViewInfo)
  strict private
    FBarBounds: TRect;
    FBarProgressBounds: TRect;
    FCalendar: TdxGanttControlCalendar;
    FCaption: string;
    FCaptionBounds: TRect;
    FTask: TdxGanttControlTask;
  protected
    function CalculateBarBounds(const R: TRect): TRect; virtual; abstract;
    function CalculateBarProgressBounds(const R: TRect): TRect;
      virtual; abstract;
    function CalculateCaptionBounds(const R: TRect): TRect; virtual; abstract;

    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
    procedure DoScroll(const DX, DY: Integer); override;

    function CalculateHitTest(const AHitTest: TdxGanttControlHitTest)
      : Boolean; override;
    function DoGetColor(const AColor: TColor): TColor;
    function GetCachedValues: TdxGanttControlTaskViewInfoCachedValues;
      virtual; abstract;
    function GetCaption: string; virtual; abstract;
    function GetColor: TColor;
    function GetCurrentCalendar: TdxGanttControlCalendar; virtual;
    function GetDefaultColor: TColor; overload; virtual;
    function GetDefaultColor(AIsManual: Boolean): TColor; overload; virtual;
    function GetHitBounds: TRect; virtual;
    function GetHintText: string; override;
    function GetMilestoneSize: TSize; virtual;
    function GetTaskHeight: Integer; virtual;
    function HasHint: Boolean; override;
    procedure Initialize; virtual;

    property Calendar: TdxGanttControlCalendar read FCalendar;
    property CachedValues: TdxGanttControlTaskViewInfoCachedValues
      read GetCachedValues;
    property Caption: string read FCaption;
    property Color: TColor read GetColor;
  public
    constructor Create(AOwner: TdxGanttControlCustomItemViewInfo;
      ATask: TdxGanttControlTask); reintroduce; virtual;
    procedure Calculate(const R: TRect); override;

    property BarBounds: TRect read FBarBounds;
    property BarProgressBounds: TRect read FBarProgressBounds;
    property CaptionBounds: TRect read FCaptionBounds;
    property Task: TdxGanttControlTask read FTask;
  end;

  { TdxGanttControlViewCustomViewInfo }

  TdxGanttControlViewCustomViewInfo = class(TdxGanttControlCustomParentViewInfo)
  strict private
    FTextLayout: TcxCanvasBasedTextLayout;
    FView: TdxGanttControlCustomView;
    function GetController: TdxGanttControlViewCustomController; inline;
    function GetDataProvider: TdxGanttControlCustomDataProvider; inline;
  protected
    function GetScaleFactor: TdxScaleFactor; override;
    function GetTextLayout: TcxCanvasBasedTextLayout;

    procedure Reset; override;

    property Controller: TdxGanttControlViewCustomController read GetController;
    property DataProvider: TdxGanttControlCustomDataProvider
      read GetDataProvider;
  public
    constructor Create(AView: TdxGanttControlCustomView); reintroduce; virtual;
    destructor Destroy; override;

    property View: TdxGanttControlCustomView read FView;
  end;

  { TdxGanttControlPopupMenu }

  TdxGanttControlPopupMenu = class(TComponent)
  strict private
    FAdapter: TdxCustomBuiltInPopupMenuAdapter;
    FCommands: TdxFastObjectList;
    FControl: TdxGanttControlBase;
    FImages: TcxImageList;
    procedure MenuItemClick(Sender: TObject);
  protected
    function Initialize(var P: TPoint): Boolean;
  public
    constructor Create(AControl: TdxGanttControlBase); reintroduce;
    destructor Destroy; override;

    procedure AddCommand(ACommand: TdxGanttControlViewCommand; AKey: Word;
      AShift: TShiftState); overload;
    procedure AddCommand(ACommand: TdxGanttControlViewCommand;
      AShortCut: TShortCut = 0); overload;
    procedure AddSeparator;

    function Popup(const P: TPoint): Boolean;
  end;

  { TdxGanttControlViewCustomController }

  TdxGanttControlViewCustomController = class abstract
    (TdxGanttControlCustomController)
  strict private
    FCaptureController: TdxGanttControlCustomController;
    FView: TdxGanttControlCustomView;
    function GetDataProvider: TdxGanttControlCustomDataProvider; inline;
    function InternalGetViewInfo: TdxGanttControlViewCustomViewInfo; inline;
  protected
    function GetControllerByCursorPos(const P: TPoint)
      : TdxGanttControlCustomController; virtual;
    function GetCurrentController: TdxGanttControlCustomController; overload;
    function GetCurrentController(const P: TPoint)
      : TdxGanttControlCustomController; overload; virtual;
    function GetDesignHitTest(X: Integer; Y: Integer; Shift: TShiftState)
      : Boolean; override;
    function GetDragHelper: TdxGanttControlDragHelper; override;
    function GetGestureClient(const APoint: TPoint): IdxGestureClient; override;
    function GetViewInfo: TdxGanttControlCustomItemViewInfo; override;

    procedure DoClick; override;
    procedure DoDblClick; override;
    procedure DoMouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure DoMouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure DoMouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; AIsIncrement: Boolean;
      const AMousePos: TPoint): Boolean; override;
    function GetTouchScrollUIOwner(const APoint: TPoint)
      : IdxTouchScrollUIOwner; override;

    function InitializeBuiltInPopupMenu(APopupMenu: TdxGanttControlPopupMenu;
      var P: TPoint): Boolean; virtual;

    property CaptureController: TdxGanttControlCustomController
      read FCaptureController;
    property DataProvider: TdxGanttControlCustomDataProvider
      read GetDataProvider;
    property ViewInfo: TdxGanttControlViewCustomViewInfo
      read InternalGetViewInfo;
  public
    constructor Create(AView: TdxGanttControlCustomView); reintroduce; virtual;

    property View: TdxGanttControlCustomView read FView;
  end;

  { TdxGanttControlCustomView }

  TdxGanttControlViewType = (None, Chart, ResourceSheet, Timeline);

  TdxGanttControlCustomView = class abstract(TdxGanttControlCustomOptions)
  strict private
    FController: TdxGanttControlViewCustomController;
    FDataProvider: TdxGanttControlCustomDataProvider;
    function GetActive: Boolean;
    function GetViewInfo: TdxGanttControlViewCustomViewInfo;
    function InternalGetOwner: TdxGanttControlBase; inline;
    procedure SetActive(const Value: Boolean);
  protected
    procedure DoChanged(AChanges: TdxGanttControlOptionsChangedTypes); override;
    procedure DoReset; override;

    function CreateController: TdxGanttControlViewCustomController;
      virtual; abstract;
    function CreateDataProvider: TdxGanttControlCustomDataProvider;
      virtual; abstract;
    function CreateViewInfo: TdxGanttControlViewCustomViewInfo;
      virtual; abstract;
    function GetScaleFactor: TdxScaleFactor; override;
    function GetType: TdxGanttControlViewType; virtual; abstract;

    property ViewInfo: TdxGanttControlViewCustomViewInfo read GetViewInfo;
  public
    destructor Destroy; override;
    procedure AfterConstruction; override;

    property Controller: TdxGanttControlViewCustomController read FController;
    // for internal use
    property DataProvider: TdxGanttControlCustomDataProvider read FDataProvider;
    // for internal use
    property Owner: TdxGanttControlBase read InternalGetOwner;
    // for internal use
  published
    property Active: Boolean read GetActive write SetActive default False;
  end;

implementation

uses
  Menus,
  RTLConsts,
  dxTypeHelpers,
  dxGanttControl;

type
  TdxGanttControlHitTestAccess = class(TdxGanttControlHitTest);
  TdxGanttControlBaseAccess = class(TdxGanttControlBase);
  TdxGanttControlCustomControllerAccess = class
    (TdxGanttControlCustomController);
  TdxGanttControlControllerAccess = class(TdxGanttControlController);
  TdxCustomGanttControlAccess = class(TdxCustomGanttControl);
  TdxGanttControlCustomViewInfoAccess = class(TdxGanttControlCustomViewInfo);

  { TdxGanttControlViewCommand }

function TdxGanttControlViewCommand.GetController
  : TdxGanttControlViewCustomController;
begin
  Result := View.Controller;
end;

function TdxGanttControlViewCommand.GetMenuCaption: string;
begin
  Result := '';
end;

function TdxGanttControlViewCommand.GetMenuDescription: string;
begin
  Result := '';
end;

function TdxGanttControlViewCommand.GetMenuImage: TdxSmartImage;
begin
  Result := nil;
end;

function TdxGanttControlViewCommand.IsChecked: Boolean;
begin
  Result := False;
end;

{ TdxGanttControlTaskViewInfoCachedValues }

procedure TdxGanttControlTaskViewInfoCachedValues.Update
  (APainter: TcxCustomLookAndFeelPainter; AScaleFactor: TdxScaleFactor);
begin
  FMilestoneSize := AScaleFactor.Apply(APainter.GetGanttMilestoneSize);
  FSummaryHeight := AScaleFactor.Apply(APainter.GetGanttSummaryTaskHeight);
  FTaskHeight := AScaleFactor.Apply(APainter.GetGanttTaskHeight);

  FTaskColor := APainter.GetGanttTaskColor(False);
  FManualTaskColor := APainter.GetGanttTaskColor(True);
  FSummaryColor := APainter.GetGanttSummaryTaskColor;
  FMilestoneColor := APainter.GetGanttMilestoneColor;
end;

{ TdxGanttControlTaskCustomViewInfo }

constructor TdxGanttControlTaskCustomViewInfo.Create
  (AOwner: TdxGanttControlCustomItemViewInfo; ATask: TdxGanttControlTask);
begin
  inherited Create(AOwner);
  FTask := ATask;
  Initialize;
end;

procedure TdxGanttControlTaskCustomViewInfo.Calculate(const R: TRect);
begin
  inherited Calculate(R);
  FCalendar := GetCurrentCalendar;
  FBarBounds := CalculateBarBounds(R);
  FBarProgressBounds := CalculateBarProgressBounds(R);
  FCaptionBounds := CalculateCaptionBounds(R);
end;

function TdxGanttControlTaskCustomViewInfo.DoGetColor
  (const AColor: TColor): TColor;
begin
  Result := AColor;
end;

procedure TdxGanttControlTaskCustomViewInfo.DoRightToLeftConversion
  (const AClientBounds: TRect);
begin
  inherited DoRightToLeftConversion(AClientBounds);
  FBarBounds := TdxRightToLeftLayoutConverter.ConvertRect(BarBounds,
    AClientBounds);
  FBarProgressBounds := TdxRightToLeftLayoutConverter.ConvertRect
    (BarProgressBounds, AClientBounds);
  FCaptionBounds := TdxRightToLeftLayoutConverter.ConvertRect(FCaptionBounds,
    AClientBounds);
end;

procedure TdxGanttControlTaskCustomViewInfo.DoScroll(const DX, DY: Integer);
begin
  inherited DoScroll(DX, DY);
  FBarBounds := cxRectOffset(FBarBounds, DX, DY);
  FBarProgressBounds := cxRectOffset(FBarProgressBounds, DX, DY);
  FCaptionBounds := cxRectOffset(FCaptionBounds, DX, DY);
end;

function TdxGanttControlTaskCustomViewInfo.GetColor: TColor;
begin
  Result := DoGetColor(GetDefaultColor);
end;

function TdxGanttControlTaskCustomViewInfo.GetCurrentCalendar
  : TdxGanttControlCalendar;
begin
  Result := Task.RealCalendar;
end;

function TdxGanttControlTaskCustomViewInfo.GetDefaultColor
  (AIsManual: Boolean): TColor;
begin
  if AIsManual then
    Result := CachedValues.ManualTaskColor
  else
    Result := CachedValues.TaskColor;
end;

function TdxGanttControlTaskCustomViewInfo.GetHintText: string;
var
  AStart, AFinish, ADuration, AComplete: string;
begin
  Result := Format('[B]%s[/B]', [Task.Name]);
  AStart := Task.GetStartInfo;
  if AStart <> '' then
    Result := Result + #13#10 + AStart;
  if not Task.Milestone then
  begin
    AFinish := Task.GetFinishInfo;
    ADuration := Task.GetDurationInfo;
    AComplete := Task.GetPercentCompleteInfo;
    if AFinish <> '' then
      Result := Result + #13#10 + AFinish;
    if ADuration <> '' then
      Result := Result + #13#10 + ADuration;
    if AComplete <> '' then
      Result := Result + #13#10 + AComplete;
  end;
end;

function TdxGanttControlTaskCustomViewInfo.GetHitBounds: TRect;
begin
  Result := FBarBounds;
end;

function TdxGanttControlTaskCustomViewInfo.GetDefaultColor: TColor;
begin
  Result := GetDefaultColor(Task.Manual);
end;

function TdxGanttControlTaskCustomViewInfo.GetMilestoneSize: TSize;
begin
  Result := CachedValues.MilestoneSize;
end;

function TdxGanttControlTaskCustomViewInfo.GetTaskHeight: Integer;
begin
  Result := CachedValues.TaskHeight;
end;

function TdxGanttControlTaskCustomViewInfo.HasHint: Boolean;
begin
  Result := True;
end;

function TdxGanttControlTaskCustomViewInfo.CalculateHitTest(const AHitTest
  : TdxGanttControlHitTest): Boolean;
begin
  Result := PtInRect(GetHitBounds, AHitTest.HitPoint);
  if Result then
    TdxGanttControlHitTestAccess(AHitTest).SetHitObject(Self);
end;

procedure TdxGanttControlTaskCustomViewInfo.Initialize;
begin
  FCaption := GetCaption;
end;

{ TdxGanttControlViewCustomViewInfo }

constructor TdxGanttControlViewCustomViewInfo.Create
  (AView: TdxGanttControlCustomView);
begin
  inherited Create(TdxGanttControlBaseAccess(AView.Owner).ViewInfo);
  FView := AView;
end;

destructor TdxGanttControlViewCustomViewInfo.Destroy;
begin
  FreeAndNil(FTextLayout);
  inherited Destroy;
end;

function TdxGanttControlViewCustomViewInfo.GetController
  : TdxGanttControlViewCustomController;
begin
  Result := View.Controller;
end;

function TdxGanttControlViewCustomViewInfo.GetDataProvider
  : TdxGanttControlCustomDataProvider;
begin
  Result := View.DataProvider;
end;

function TdxGanttControlViewCustomViewInfo.GetScaleFactor: TdxScaleFactor;
begin
  Result := Controller.ScaleFactor;
end;

function TdxGanttControlViewCustomViewInfo.GetTextLayout
  : TcxCanvasBasedTextLayout;
begin
  if FTextLayout = nil then
    FTextLayout := Canvas.CreateTextLayout;
  Result := FTextLayout;
end;

procedure TdxGanttControlViewCustomViewInfo.Reset;
begin
  inherited Reset;
  FreeAndNil(FTextLayout);
end;

{ TdxGanttControlPopupMenu }

constructor TdxGanttControlPopupMenu.Create(AControl: TdxGanttControlBase);
begin
  inherited Create(nil);
  FControl := AControl;
  FCommands := TdxFastObjectList.Create;
  FImages := TcxImageList.Create(Self);
  FAdapter := TdxBuiltInPopupMenuAdapterManager.GetActualAdapterClass.
    Create(Self);
  FAdapter.SetImages(FImages);
end;

destructor TdxGanttControlPopupMenu.Destroy;
begin
  FreeAndNil(FAdapter);
  FreeAndNil(FCommands);
  FreeAndNil(FImages);
  inherited Destroy;
end;

procedure TdxGanttControlPopupMenu.AddCommand
  (ACommand: TdxGanttControlViewCommand; AKey: Word; AShift: TShiftState);
begin
  AddCommand(ACommand, ShortCut(AKey, AShift));
end;

procedure TdxGanttControlPopupMenu.AddCommand
  (ACommand: TdxGanttControlViewCommand; AShortCut: TShortCut = 0);
var
  AImage: TdxSmartImage;
  AImageIndex: Integer;
  AItem: TComponent;
begin
  FCommands.Add(ACommand);
  AImage := ACommand.GetMenuImage;
  if AImage <> nil then
  begin
    AImageIndex := FImages.Count;
    FImages.Add(AImage);
  end
  else
    AImageIndex := -1;
  if ACommand.Visible then
  begin
    AItem := FAdapter.Add(ACommand.GetMenuCaption, MenuItemClick,
      FCommands.Count - 1, AImageIndex, ACommand.Enabled, AShortCut);
    if ACommand.IsChecked then
      FAdapter.SetChecked(AItem, True);
  end;
end;

procedure TdxGanttControlPopupMenu.AddSeparator;
begin
  FAdapter.AddSeparator;
end;

function TdxGanttControlPopupMenu.Initialize(var P: TPoint): Boolean;
begin
  FAdapter.Clear;
  FCommands.Clear;
  FImages.Clear;
  Result := TdxGanttControlControllerAccess(FControl.Controller)
    .InitializeBuiltInPopupMenu(Self, P);
end;

procedure TdxGanttControlPopupMenu.MenuItemClick(Sender: TObject);
begin
  TdxGanttControlCommand(FCommands[TComponent(Sender).Tag]).Execute;
end;

function TdxGanttControlPopupMenu.Popup(const P: TPoint): Boolean;
var
  APopupPoint: TPoint;
begin
  TdxGanttControlBaseAccess(FControl).HintController.Hide;
  TdxGanttControlBaseAccess(FControl).HintController.LockHint := True;
  try
    APopupPoint := P;
    FAdapter.BiDiMode := FControl.BiDiMode;
    Result := Initialize(APopupPoint) and
      FAdapter.Popup(FControl.ClientToScreen(APopupPoint));
  finally
    TdxGanttControlBaseAccess(FControl).HintController.LockHint := False;
  end;
end;

{ TdxGanttControlViewCustomController }

constructor TdxGanttControlViewCustomController.Create
  (AView: TdxGanttControlCustomView);
begin
  inherited Create(AView.Owner);
  FView := AView;
end;

function TdxGanttControlViewCustomController.GetControllerByCursorPos
  (const P: TPoint): TdxGanttControlCustomController;
begin
  Result := nil;
end;

function TdxGanttControlViewCustomController.GetCurrentController
  : TdxGanttControlCustomController;
begin
  Result := GetCurrentController(HitTest.HitPoint);
end;

function TdxGanttControlViewCustomController.GetCurrentController
  (const P: TPoint): TdxGanttControlCustomController;
begin
  if FCaptureController <> nil then
    Result := FCaptureController
  else
    Result := GetControllerByCursorPos(P);
end;

function TdxGanttControlViewCustomController.GetDataProvider
  : TdxGanttControlCustomDataProvider;
begin
  Result := View.DataProvider;
end;

function TdxGanttControlViewCustomController.GetDesignHitTest(X, Y: Integer;
  Shift: TShiftState): Boolean;
var
  AController: TdxGanttControlCustomController;
begin
  AController := GetControllerByCursorPos(TPoint.Create(X, Y));
  if AController <> nil then
    Result := TdxGanttControlCustomControllerAccess(AController)
      .GetDesignHitTest(X, Y, Shift)
  else
    Result := inherited GetDesignHitTest(X, Y, Shift);
end;

function TdxGanttControlViewCustomController.InitializeBuiltInPopupMenu
  (APopupMenu: TdxGanttControlPopupMenu; var P: TPoint): Boolean;
begin
  Result := False;
end;

function TdxGanttControlViewCustomController.InternalGetViewInfo
  : TdxGanttControlViewCustomViewInfo;
begin
  Result := TdxGanttControlViewCustomViewInfo(inherited ViewInfo);
end;

function TdxGanttControlViewCustomController.GetDragHelper
  : TdxGanttControlDragHelper;
begin
  if GetCurrentController <> nil then
    Result := TdxGanttControlCustomControllerAccess(GetCurrentController)
      .DragHelper
  else
    Result := inherited GetDragHelper;
end;

function TdxGanttControlViewCustomController.GetGestureClient
  (const APoint: TPoint): IdxGestureClient;
var
  AController: TdxGanttControlCustomController;
begin
  if FCaptureController <> nil then
    Result := TdxGanttControlCustomControllerAccess(FCaptureController)
      .GetGestureClient(APoint)
  else
  begin
    AController := GetControllerByCursorPos(APoint);
    if AController <> nil then
      Result := TdxGanttControlCustomControllerAccess(AController)
        .GetGestureClient(APoint)
    else
      Result := inherited GetGestureClient(APoint);
  end;
end;

function TdxGanttControlViewCustomController.GetViewInfo
  : TdxGanttControlCustomItemViewInfo;
begin
  Result := View.ViewInfo;
end;

procedure TdxGanttControlViewCustomController.DoClick;
begin
  if FCaptureController <> nil then
    FCaptureController.Click;
  inherited DoClick;
end;

procedure TdxGanttControlViewCustomController.DoDblClick;
begin
  if GetCurrentController <> nil then
    GetCurrentController.DblClick;
  inherited DoDblClick;
end;

procedure TdxGanttControlViewCustomController.DoMouseDown(Button: TMouseButton;
  Shift: TShiftState; X: Integer; Y: Integer);
begin
  FCaptureController := GetControllerByCursorPos(TPoint.Create(X, Y));
  if GetCurrentController(TPoint.Create(X, Y)) <> nil then
    GetCurrentController(TPoint.Create(X, Y)).MouseDown(Button, Shift, X, Y);
  inherited DoMouseDown(Button, Shift, X, Y);
end;

procedure TdxGanttControlViewCustomController.DoMouseMove(Shift: TShiftState;
  X: Integer; Y: Integer);
begin
  if GetCurrentController(TPoint.Create(X, Y)) <> nil then
    GetCurrentController(TPoint.Create(X, Y)).MouseMove(Shift, X, Y);
  inherited DoMouseMove(Shift, X, Y);
end;

procedure TdxGanttControlViewCustomController.DoMouseUp(Button: TMouseButton;
  Shift: TShiftState; X: Integer; Y: Integer);
begin
  inherited DoMouseUp(Button, Shift, X, Y);
  if GetCurrentController(TPoint.Create(X, Y)) <> nil then
    GetCurrentController(TPoint.Create(X, Y)).MouseUp(Button, Shift, X, Y);
  FCaptureController := nil;
end;

function TdxGanttControlViewCustomController.DoMouseWheel(Shift: TShiftState;
  AIsIncrement: Boolean; const AMousePos: TPoint): Boolean;
var
  AController: TdxGanttControlCustomController;
begin
  AController := GetCurrentController(AMousePos);
  Result := (AController <> nil) and AController.MouseWheel(Shift, AIsIncrement,
    AMousePos);
end;

function TdxGanttControlViewCustomController.GetTouchScrollUIOwner
  (const APoint: TPoint): IdxTouchScrollUIOwner;
var
  AController: TdxGanttControlCustomController;
begin
  AController := GetCurrentController(APoint);
  if AController <> nil then
    Result := TdxGanttControlCustomControllerAccess(AController)
      .GetTouchScrollUIOwner(APoint)
  else
    Result := inherited GetTouchScrollUIOwner(APoint);
end;

{ TdxGanttControlCustomView }

destructor TdxGanttControlCustomView.Destroy;
begin
  FreeAndNil(FController);
  FreeAndNil(FDataProvider);
  inherited Destroy;
end;

procedure TdxGanttControlCustomView.AfterConstruction;
begin
  FDataProvider := CreateDataProvider;
  FController := CreateController;
  inherited AfterConstruction;
end;

procedure TdxGanttControlCustomView.DoChanged
  (AChanges: TdxGanttControlOptionsChangedTypes);
begin
  inherited DoChanged(AChanges);
  TdxCustomGanttControlAccess(Owner).ViewChanged(Self, AChanges)
end;

procedure TdxGanttControlCustomView.DoReset;
begin
  // do nothing
end;

function TdxGanttControlCustomView.GetActive: Boolean;
begin
  Result := TdxCustomGanttControlAccess(Owner).ActiveViewType = GetType;
end;

function TdxGanttControlCustomView.GetViewInfo
  : TdxGanttControlViewCustomViewInfo;
var
  I: Integer;
  AControlViewInfo: TdxGanttControlCustomViewInfoAccess;
begin
  if not Active then
    Exit(nil);
  AControlViewInfo := TdxGanttControlCustomViewInfoAccess
    (TdxGanttControlBaseAccess(Owner).ViewInfo);
  for I := 0 to AControlViewInfo.ViewInfoCount - 1 do
    if (AControlViewInfo.ViewInfos[I] is TdxGanttControlViewCustomViewInfo) and
      (TdxGanttControlViewCustomViewInfo(AControlViewInfo.ViewInfos[I])
      .View = Self) then
      Exit(TdxGanttControlViewCustomViewInfo(AControlViewInfo.ViewInfos[I]));
  Result := nil;
end;

function TdxGanttControlCustomView.InternalGetOwner: TdxGanttControlBase;
begin
  Result := TdxGanttControlBase(inherited Owner);
end;

function TdxGanttControlCustomView.GetScaleFactor: TdxScaleFactor;
begin
  Result := TdxCustomGanttControlAccess(Owner).ScaleFactor;
end;

procedure TdxGanttControlCustomView.SetActive(const Value: Boolean);
var
  AType: TdxGanttControlViewType;
begin
  if Active <> Value then
  begin
    if Value then
      AType := GetType
    else
    begin
      AType := TdxCustomGanttControlAccess(Owner).ActiveViewType;
      Inc(AType);
      if AType > High(TdxGanttControlViewType) then
        AType := TdxGanttControlViewType.Chart;
    end;
    TdxCustomGanttControlAccess(Owner).ActiveViewType := AType;
  end;
end;

end.
