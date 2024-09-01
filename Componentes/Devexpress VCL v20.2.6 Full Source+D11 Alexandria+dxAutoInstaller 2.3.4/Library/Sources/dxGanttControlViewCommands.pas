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

unit dxGanttControlViewCommands; // for internal use

{$I cxVer.inc}
{$I dxGanttControl.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes, System.Contnrs,
{$ENDIF}
  Types, SysUtils, Classes, Graphics, ImgList, Windows, Controls,
  Generics.Defaults, Generics.Collections, Forms, StdCtrls, Variants,
  dxCore, dxCoreClasses, dxGDIPlusClasses,
  dxGanttControl,
  dxGanttControlCustomView,
  dxGanttControlCustomClasses,
  dxGanttControlCommands,
  dxGanttControlTasks,
  dxGanttControlViewChart,
  dxGanttControlViewTimeline;

type
  { TdxGanttControlActivateViewCommand }

  TdxGanttControlActivateViewCommand = class abstract
    (TdxGanttControlViewCommand)
  protected
    procedure DoExecute; override;
  public
    class procedure SetActiveView(AControl: TdxGanttControlBase;
      AViewType: TdxGanttControlViewType); static;
    function Enabled: Boolean; override;
    function IsChecked: Boolean; override;
  end;

  { TdxGanttControlActivateChartViewCommand }

  TdxGanttControlActivateChartViewCommand = class
    (TdxGanttControlActivateViewCommand)
  protected
    function GetView: TdxGanttControlCustomView; override;
  end;

  { TdxGanttControlActivateResourceSheetViewCommand }

  TdxGanttControlActivateResourceSheetViewCommand = class
    (TdxGanttControlActivateViewCommand)
  protected
    function GetView: TdxGanttControlCustomView; override;
  end;

  { TdxGanttControlActivateTimelineViewCommand }

  TdxGanttControlActivateTimelineViewCommand = class
    (TdxGanttControlActivateViewCommand)
  protected
    function GetView: TdxGanttControlCustomView; override;
  end;

  { TdxGanttControlViewChartCommand }

  TdxGanttControlViewChartCommand = class(TdxGanttControlViewCommand)
  strict private
    function GetController: TdxGanttControlChartViewController; inline;
    function GetTask: TdxGanttControlTask;
    function InternalGetView: TdxGanttControlChartView; inline;
  protected
    function GetView: TdxGanttControlCustomView; override;

    property Task: TdxGanttControlTask read GetTask;
    property Controller: TdxGanttControlChartViewController read GetController;
  public
    function Enabled: Boolean; override;
    property View: TdxGanttControlChartView read InternalGetView;
  end;

  { TdxGanttControlScrollToTaskCommand }

  TdxGanttControlScrollToTaskCommand = class(TdxGanttControlViewChartCommand)
  protected
    procedure DoExecute; override;
  public
    function GetMenuCaption: string; override;
    function GetMenuImage: TdxSmartImage; override;
    function Enabled: Boolean; override;
  end;

  { TdxGanttControlInsertTaskViewCommand }

  TdxGanttControlInsertTaskViewCommand = class(TdxGanttControlViewChartCommand)
  protected
    procedure DoExecute; override;
  public
    function GetMenuCaption: string; override;
    function Enabled: Boolean; override;
  end;

  { TdxGanttControlDeleteTaskViewCommand }

  TdxGanttControlDeleteTaskViewCommand = class(TdxGanttControlViewChartCommand)
  protected
    procedure DoExecute; override;
  public
    function GetMenuCaption: string; override;
    function Enabled: Boolean; override;
  end;

  { TdxGanttControlOpenTaskInformationDialogCommand }

  TdxGanttControlOpenTaskInformationDialogCommand = class
    (TdxGanttControlViewChartCommand)
  protected
    procedure DoExecute; override;
  public
    function GetMenuCaption: string; override;
    function GetMenuImage: TdxSmartImage; override;
  end;

  { TdxGanttControlAddTaskToTimelineCommand }

  TdxGanttControlAddTaskToTimelineCommand = class
    (TdxGanttControlViewChartCommand)
  protected
    procedure DoExecute; override;
  public
    function Enabled: Boolean; override;
    function GetMenuCaption: string; override;
    function GetMenuImage: TdxSmartImage; override;
    function IsChecked: Boolean; override;
  end;

  { TdxGanttControlViewTimelineCommand }

  TdxGanttControlViewTimelineCommand = class(TdxGanttControlViewCommand)
  strict private
    function GetController: TdxGanttControlTimelineViewController; inline;
    function GetTasks: TdxFastObjectList;
    function InternalGetView: TdxGanttControlTimelineView; inline;
  protected
    function GetView: TdxGanttControlCustomView; override;

    property Tasks: TdxFastObjectList read GetTasks;
    property Controller: TdxGanttControlTimelineViewController
      read GetController;
  public
    function Enabled: Boolean; override;
    property View: TdxGanttControlTimelineView read InternalGetView;
  end;

  { TdxGanttControlGoToTaskCommand }

  TdxGanttControlGoToTaskCommand = class(TdxGanttControlViewTimelineCommand)
  protected
    procedure DoExecute; override;
  public
    function Enabled: Boolean; override;
    function GetMenuCaption: string; override;
  end;

  { TdxGanttControlRemoveTaskFromTimelineCommand }

  TdxGanttControlRemoveTaskFromTimelineCommand = class
    (TdxGanttControlViewTimelineCommand)
  protected
    procedure DoExecute; override;
  public
    function Enabled: Boolean; override;
    function GetMenuCaption: string; override;
  end;

  { TdxGanttControlTimelineOpenTaskInformationDialogCommand }

  TdxGanttControlTimelineOpenTaskInformationDialogCommand = class
    (TdxGanttControlViewTimelineCommand)
  protected
    procedure DoExecute; override;
  public
    function Enabled: Boolean; override;
    function GetMenuCaption: string; override;
    function GetMenuImage: TdxSmartImage; override;
  end;

implementation

uses
  RTLConsts,
  dxGanttControlTaskCommands,
  dxGanttControlCustomSheet,
  dxGanttControlStrs,
  dxGanttControlImages;

type
  TdxGanttControlCustomControllerAccess = class
    (TdxGanttControlCustomController);
  TdxGanttControlSheetControllerAccess = class(TdxGanttControlSheetController);
  TdxCustomGanttControlAccess = class(TdxCustomGanttControl);
  TdxGanttControlCustomViewAccess = class(TdxGanttControlCustomView);
  TdxGanttControlChartViewAccess = class(TdxGanttControlChartView);
  TdxGanttControlChartViewSheetControllerAccess = class
    (TdxGanttControlChartViewSheetController);
  TdxGanttControlTimelineViewControllerAccess = class
    (TdxGanttControlTimelineViewController);
  TdxGanttControlTimelineViewAccess = class(TdxGanttControlTimelineView);

  { TdxGanttControlActivateViewHistoryItem }

  TdxGanttControlActivateViewHistoryItem = class(TdxGanttControlHistoryItem)
  protected
    FOldActiveView: TdxGanttControlCustomView;
    FView: TdxGanttControlCustomView;
    procedure DoRedo; override;
    procedure DoUndo; override;
  end;

  { TdxGanttControlActivateViewHistoryItem }

procedure TdxGanttControlActivateViewHistoryItem.DoRedo;
begin
  inherited DoRedo;
  TdxCustomGanttControlAccess(FView.Owner).DoSetActiveViewType
    (TdxGanttControlCustomViewAccess(FView).GetType);
end;

procedure TdxGanttControlActivateViewHistoryItem.DoUndo;
begin
  inherited DoUndo;
  if FOldActiveView <> nil then
    TdxCustomGanttControlAccess(FOldActiveView.Owner)
      .DoSetActiveViewType(TdxGanttControlCustomViewAccess
      (FOldActiveView).GetType);
end;

{ TdxGanttControlActivateViewCommand }

procedure TdxGanttControlActivateViewCommand.DoExecute;
var
  AHistoryItem: TdxGanttControlActivateViewHistoryItem;
  AOldActiveView: TdxGanttControlCustomView;
begin
  if View.Active then
    Exit;
  inherited DoExecute;
  AOldActiveView := TdxCustomGanttControl(Control).ActiveView;
  AHistoryItem := TdxGanttControlActivateViewHistoryItem.Create
    (Control.History);
  try
    AHistoryItem.FOldActiveView := AOldActiveView;
    AHistoryItem.FView := View;
    if AOldActiveView <> nil then
      Control.History.AddItem(AHistoryItem);
    AHistoryItem.Execute;
  finally
    if AOldActiveView = nil then
      AHistoryItem.Free;
  end;
end;

function TdxGanttControlActivateViewCommand.Enabled: Boolean;
begin
  Result := True;
end;

function TdxGanttControlActivateViewCommand.IsChecked: Boolean;
begin
  Result := View.Active;
end;

class procedure TdxGanttControlActivateViewCommand.SetActiveView
  (AControl: TdxGanttControlBase; AViewType: TdxGanttControlViewType);
var
  ACommand: TdxGanttControlActivateViewCommand;
begin
  if AViewType = TdxGanttControlViewType.None then
    Exit;
  case AViewType of
    TdxGanttControlViewType.ResourceSheet:
      ACommand := TdxGanttControlActivateResourceSheetViewCommand.Create
        (AControl);
    TdxGanttControlViewType.Timeline:
      ACommand := TdxGanttControlActivateTimelineViewCommand.Create(AControl);
  else
    ACommand := TdxGanttControlActivateChartViewCommand.Create(AControl);
  end;
  try
    ACommand.Execute;
  finally
    ACommand.Free;
  end;
end;

{ TdxGanttControlActivateChartViewCommand }

function TdxGanttControlActivateChartViewCommand.GetView
  : TdxGanttControlCustomView;
begin
  Result := TdxCustomGanttControl(Control).ViewChart;
end;

{ TdxGanttControlActivateResourceSheetViewCommand }

function TdxGanttControlActivateResourceSheetViewCommand.GetView
  : TdxGanttControlCustomView;
begin
  Result := TdxCustomGanttControl(Control).ViewResourceSheet;
end;

{ TdxGanttControlActivateTimelineViewCommand }

function TdxGanttControlActivateTimelineViewCommand.GetView
  : TdxGanttControlCustomView;
begin
  Result := TdxCustomGanttControl(Control).ViewTimeline;
end;

{ TdxGanttControlViewChartCommand }

function TdxGanttControlViewChartCommand.Enabled: Boolean;
begin
  Result := TdxCustomGanttControl(Control).ViewChart.Active;
end;

function TdxGanttControlViewChartCommand.GetController
  : TdxGanttControlChartViewController;
begin
  Result := TdxGanttControlChartViewController(inherited Controller);
end;

function TdxGanttControlViewChartCommand.GetTask: TdxGanttControlTask;
begin
  Result := TdxGanttControlChartViewSheetControllerAccess
    (Controller.SheetController).FocusedDataItem;
end;

function TdxGanttControlViewChartCommand.GetView: TdxGanttControlCustomView;
begin
  Result := TdxCustomGanttControl(Control).ViewChart;
end;

function TdxGanttControlViewChartCommand.InternalGetView
  : TdxGanttControlChartView;
begin
  Result := TdxCustomGanttControl(Control).ViewChart;
end;

{ TdxGanttControlScrollToTaskCommand }

procedure TdxGanttControlScrollToTaskCommand.DoExecute;
begin
  inherited DoExecute;
  if not((Task.Start >= View.FirstVisibleDateTime) and
    ((Task.Start < TdxGanttControlChartViewAccess(View).GetLastVisibleDateTime)))
  then
    View.FirstVisibleDateTime := Task.Start;
end;

function TdxGanttControlScrollToTaskCommand.Enabled: Boolean;
begin
  Result := inherited Enabled and (Task <> nil) and (not Task.Blank) and
    Task.IsValueAssigned(TdxGanttTaskAssignedValue.Start) and
    Task.IsValueAssigned(TdxGanttTaskAssignedValue.Finish);
end;

function TdxGanttControlScrollToTaskCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxGanttControlCommandScrollToTaskCaption);
end;

function TdxGanttControlScrollToTaskCommand.GetMenuImage: TdxSmartImage;
begin
  Result := TdxGanttControlImages.MenuScrollToTask;
end;

{ TdxGanttControlInsertTaskViewCommand }

procedure TdxGanttControlInsertTaskViewCommand.DoExecute;
begin
  inherited DoExecute;
  with TdxGanttControlSheetInsertNewItemCommand.Create
    (Controller.SheetController) do
    try
      Execute;
    finally
      Free;
    end;
end;

function TdxGanttControlInsertTaskViewCommand.Enabled: Boolean;
begin
  Result := inherited Enabled and Control.IsEditable;
end;

function TdxGanttControlInsertTaskViewCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxGanttControlCommandInsertTaskCaption);
end;

{ TdxGanttControlDeleteTaskViewCommand }

procedure TdxGanttControlDeleteTaskViewCommand.DoExecute;
begin
  inherited DoExecute;
  TdxGanttControlChartViewSheetControllerAccess(Controller.SheetController)
    .DeleteFocusedItem;
end;

function TdxGanttControlDeleteTaskViewCommand.Enabled: Boolean;
begin
  Result := inherited Enabled and Control.IsEditable and (Task <> nil);
end;

function TdxGanttControlDeleteTaskViewCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxGanttControlCommandDeleteTaskCaption);
end;

{ TdxGanttControlOpenTaskInformationDialogCommand }

procedure TdxGanttControlOpenTaskInformationDialogCommand.DoExecute;
begin
  inherited DoExecute;
  TdxGanttControlChartViewSheetControllerAccess(Controller.SheetController)
    .ShowTaskInformationDialog(Task);
end;

function TdxGanttControlOpenTaskInformationDialogCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxGanttControlCommandInformationCaption);
end;

function TdxGanttControlOpenTaskInformationDialogCommand.GetMenuImage
  : TdxSmartImage;
begin
  Result := TdxGanttControlImages.MenuTaskInformation;
end;

{ TdxGanttControlAddTaskToTimelineCommand }

procedure TdxGanttControlAddTaskToTimelineCommand.DoExecute;
begin
  inherited DoExecute;
  with TdxGanttControlChangeTaskDisplayOnTimelineCommand.Create(Control, Task,
    not IsChecked) do
    try
      Execute;
    finally
      Free;
    end;
end;

function TdxGanttControlAddTaskToTimelineCommand.Enabled: Boolean;
begin
  Result := inherited Enabled and Control.IsEditable and (Task <> nil) and
    (not Task.Blank);
end;

function TdxGanttControlAddTaskToTimelineCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString
    (@sdxGanttControlCommandAddTaskToTimelineCaption);
end;

function TdxGanttControlAddTaskToTimelineCommand.GetMenuImage: TdxSmartImage;
begin
  Result := nil;
end;

function TdxGanttControlAddTaskToTimelineCommand.IsChecked: Boolean;
begin
  Result := (Task <> nil) and not Task.Blank and
    Task.IsValueAssigned(TdxGanttTaskAssignedValue.DisplayOnTimeline) and
    Task.DisplayOnTimeline;
end;

{ TdxGanttControlViewTimelineCommand }

function TdxGanttControlViewTimelineCommand.Enabled: Boolean;
begin
  Result := TdxCustomGanttControl(Control).ViewTimeline.Active;
end;

function TdxGanttControlViewTimelineCommand.GetController
  : TdxGanttControlTimelineViewController;
begin
  Result := TdxGanttControlTimelineViewController(inherited Controller);
end;

function TdxGanttControlViewTimelineCommand.GetTasks: TdxFastObjectList;
begin
  Result := TdxGanttControlTimelineViewAccess(View)
    .ViewInfo.Controller.Selection;
end;

function TdxGanttControlViewTimelineCommand.GetView: TdxGanttControlCustomView;
begin
  Result := TdxCustomGanttControl(Control).ViewTimeline;
end;

function TdxGanttControlViewTimelineCommand.InternalGetView
  : TdxGanttControlTimelineView;
begin
  Result := TdxCustomGanttControl(Control).ViewTimeline;
end;

{ TdxGanttControlGoToTaskCommand }

procedure TdxGanttControlGoToTaskCommand.DoExecute;

  function GetParent(ATask: TdxGanttControlTask): TdxGanttControlTask;
  var
    I: Integer;
  begin
    Result := nil;
    if ATask.OutlineLevel = 0 then
      Exit;
    for I := ATask.ID - 1 downto 0 do
      if (ATask.Owner[I].Summary) and
        (ATask.Owner[I].OutlineLevel = ATask.OutlineLevel - 1) then
        Exit(ATask.Owner[I]);
  end;

  procedure ExpandTaskParent(AParent: TdxGanttControlTask);
  var
    AView: TdxGanttControlChartView;
  begin
    if (AParent = nil) or (AParent.ID = 0) then
      Exit;
    AView := TdxCustomGanttControlAccess(Control).ViewChart;
    ExpandTaskParent(GetParent(AParent));
    if not AView.DataProvider.IsExpanded(AParent) then
    begin
      TdxCustomGanttControl(Control).ViewChart.Controller.FocusedRowIndex :=
        TdxCustomGanttControl(Control).ViewChart.DataProvider.IndexOf(AParent);
      TdxGanttControlSheetControllerAccess(AView.Controller.SheetController)
        .ExpandItem;
      AView.DataProvider.Refresh(True);
      TdxCustomGanttControlAccess(Control).ViewInfo.Recalculate;
    end;
  end;

var
  ATask: TdxGanttControlTask;
begin
  ATask := TdxGanttControlTask(Tasks[Tasks.Count - 1]);
  with TdxGanttControlActivateChartViewCommand.Create(Control) do
    try
      Execute;
    finally
      Free;
    end;

  TdxCustomGanttControlAccess(Control).ViewInfo.CalculateLayout;
  TdxCustomGanttControlAccess(Control).ViewInfo.Recalculate;
  TdxGanttControlCustomControllerAccess(TdxCustomGanttControl(Control)
    .ViewChart.Controller).InitScrollbars;
  TdxCustomGanttControlAccess(Control).ViewInfo.Recalculate;
  ExpandTaskParent(GetParent(ATask));
  TdxCustomGanttControl(Control).ViewChart.Controller.FocusedRowIndex :=
    TdxCustomGanttControl(Control).ViewChart.DataProvider.IndexOf(ATask);
  if not((ATask.Start >= TdxCustomGanttControl(Control)
    .ViewChart.FirstVisibleDateTime) and
    ((ATask.Start < TdxGanttControlChartViewAccess(TdxCustomGanttControl
    (Control).ViewChart).GetLastVisibleDateTime))) then
    TdxCustomGanttControl(Control).ViewChart.FirstVisibleDateTime :=
      ATask.Start;
end;

function TdxGanttControlGoToTaskCommand.Enabled: Boolean;
begin
  Result := inherited Enabled and (Tasks.Count = 1);
end;

function TdxGanttControlGoToTaskCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxGanttControlCommandGoToTaskCaption)
end;

{ TdxGanttControlRemoveTaskFromTimelineCommand }

procedure TdxGanttControlRemoveTaskFromTimelineCommand.DoExecute;
var
  I: Integer;
begin
  View.BeginUpdate;
  try
    for I := 0 to Tasks.Count - 1 do
      with TdxGanttControlChangeTaskDisplayOnTimelineCommand.Create(Control,
        TdxGanttControlTask(Tasks[I]), False) do
        try
          Execute;
        finally
          Free;
        end;
  finally
    TdxGanttControlTimelineViewAccess(View)
      .Changed([TdxGanttControlOptionsChangedType.Layout]);
    View.EndUpdate;
  end;
end;

function TdxGanttControlRemoveTaskFromTimelineCommand.Enabled: Boolean;
begin
  Result := inherited Enabled and (Tasks.Count > 0);
end;

function TdxGanttControlRemoveTaskFromTimelineCommand.GetMenuCaption: string;
begin
  Result := cxGetResourceString
    (@sdxGanttControlCommandRemoveFromTimelineCaption);
end;

{ TdxGanttControlTimelineOpenTaskInformationDialogCommand }

procedure TdxGanttControlTimelineOpenTaskInformationDialogCommand.DoExecute;
begin
  inherited DoExecute;
  TdxGanttControlTimelineViewControllerAccess(Controller)
    .ShowTaskInformationDialog(TdxGanttControlTask(Tasks[Tasks.Count - 1]));
end;

function TdxGanttControlTimelineOpenTaskInformationDialogCommand.
  Enabled: Boolean;
begin
  Result := inherited Enabled and (Tasks.Count = 1);
end;

function TdxGanttControlTimelineOpenTaskInformationDialogCommand.
  GetMenuCaption: string;
begin
  Result := cxGetResourceString(@sdxGanttControlCommandInformationCaption);
end;

function TdxGanttControlTimelineOpenTaskInformationDialogCommand.GetMenuImage
  : TdxSmartImage;
begin
  Result := TdxGanttControlImages.MenuTaskInformation;
end;

end.
