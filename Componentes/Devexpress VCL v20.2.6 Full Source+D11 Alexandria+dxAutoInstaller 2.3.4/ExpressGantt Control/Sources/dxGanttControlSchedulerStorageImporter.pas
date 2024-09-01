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

unit dxGanttControlSchedulerStorageImporter;

{$I cxVer.inc}
{$I dxGanttControl.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes, System.Contnrs,
{$ENDIF}
  SysUtils, Generics.Defaults, Generics.Collections, Classes, Controls, Forms,
  cxClasses, dxCore, dxCoreClasses, cxSchedulerStorage, dxGanttControlDataModel;

procedure dxGanttControlImportFromSchedulerStorage
  (AStorage: TcxCustomSchedulerStorage; ADataModel: TdxGanttControlDataModel);

implementation

uses
  RTLConsts, Math, Variants,
  dxGanttControlCustomDataModel, dxGanttControlCalendars,
  dxGanttControlResources, dxGanttControlTasks;

type
  TcxSchedulerEventAccess = class(TcxSchedulerEvent);
  TcxSchedulerStorageResourcesAccess = class(TcxSchedulerStorageResources);
  TcxCustomSchedulerStorageAccess = class(TcxCustomSchedulerStorage);

  TdxSchedulerEventLink = class
  private
    FEventFrom: TcxSchedulerEvent;
    FEventTo: TcxSchedulerEvent;
    FRelation: TcxSchedulerEventRelation;

    function GetPredecessorLinkType: TdxGanttControlTaskPredecessorLinkType;
  public
    constructor Create(AEventFrom, AEventTo: TcxSchedulerEvent;
      ARelation: TcxSchedulerEventRelation);

    property EventFrom: TcxSchedulerEvent read FEventFrom;
    property EventTo: TcxSchedulerEvent read FEventTo;
    property GanttControlTaskPredecessorLinkType
      : TdxGanttControlTaskPredecessorLinkType read GetPredecessorLinkType;
  end;

var
  dxResourceDictionary: TObjectDictionary<TcxSchedulerStorageResourceItem,
    TdxGanttControlResource>;

  { TdxSchedulerEventLink }

constructor TdxSchedulerEventLink.Create(AEventFrom,
  AEventTo: TcxSchedulerEvent; ARelation: TcxSchedulerEventRelation);
begin
  FEventFrom := AEventFrom;
  FEventTo := AEventTo;
  FRelation := ARelation;
end;

function TdxSchedulerEventLink.GetPredecessorLinkType
  : TdxGanttControlTaskPredecessorLinkType;
begin
  case FRelation of
    trFinishToStart:
      Result := TdxGanttControlTaskPredecessorLinkType.FS;
    trStartToStart:
      Result := TdxGanttControlTaskPredecessorLinkType.SS;
    trFinishToFinish:
      Result := TdxGanttControlTaskPredecessorLinkType.FF;
  else
    Result := TdxGanttControlTaskPredecessorLinkType.SF;
  end;
end;

procedure ImportResources(AStorage: TcxCustomSchedulerStorage;
  ADataModel: TdxGanttControlDataModel);
var
  I: Integer;
  AResource: TcxSchedulerStorageResourceItem;
  AGanttResource: TdxGanttControlResource;
begin
  for I := 0 to AStorage.ResourceCount - 1 do
  begin
    AResource := AStorage.Resources.ResourceItems[I];
    AGanttResource := ADataModel.Resources.Append;
    dxResourceDictionary.AddOrSetValue(AResource, AGanttResource);
    AGanttResource.Name := AResource.Name;
    AGanttResource.Inactive := not AResource.Visible;
    AGanttResource.Start := AResource.WorkStart;
    AGanttResource.Finish := AResource.WorkFinish;
  end;
end;

procedure AssignTaskResource(ATaskUID, AResourceUID: Integer;
  ADataModel: TdxGanttControlDataModel);
begin
  with ADataModel.Assignments.Append do
  begin
    TaskUID := ATaskUID;
    ResourceUID := AResourceUID;
  end;
end;

procedure ImportEvents(AStorage: TcxCustomSchedulerStorage;
  ADataModel: TdxGanttControlDataModel);
var
  AEventToTaskDictionary: TObjectDictionary<TcxSchedulerEvent,
    TdxGanttControlTask>;
  ATaskToEventDictionary: TObjectDictionary<TdxGanttControlTask,
    TcxSchedulerEvent>;
  ALinks: TcxObjectList;

  procedure AddPredecessorLinks;
  var
    I: Integer;
    AEventLink: TdxSchedulerEventLink;
    ATask, APredecessor: TdxGanttControlTask;
  begin
    for I := 0 to ALinks.Count - 1 do
    begin
      AEventLink := TdxSchedulerEventLink(ALinks[I]);
      if AEventToTaskDictionary.TryGetValue(AEventLink.EventTo, ATask) and
        AEventToTaskDictionary.TryGetValue(AEventLink.EventFrom, APredecessor)
      then
        with ATask.PredecessorLinks.Append do
        begin
          PredecessorUID := APredecessor.UID;
          &Type := AEventLink.GanttControlTaskPredecessorLinkType;
          LinkLag := 0;
          LagFormat := TdxGanttControlTaskPredecessorLagFormat.Days;
        end;
    end;

  end;

  function IsAllOutlineLevelsAssigned: Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := 0 to ADataModel.Tasks.Count - 1 do
      if not ADataModel.Tasks[I].IsValueAssigned
        (TdxGanttTaskAssignedValue.OutlineLevel) then
        Exit(False);
  end;

  procedure AssignOutlineLevel;
  var
    I: Integer;
    ATask, AParentTask: TdxGanttControlTask;
    AEvent: TcxSchedulerEvent;
  begin
    while not IsAllOutlineLevelsAssigned do
      for I := 0 to ADataModel.Tasks.Count - 1 do
      begin
        ATask := ADataModel.Tasks[I];
        if not ATask.IsValueAssigned(TdxGanttTaskAssignedValue.OutlineLevel) and
          ATaskToEventDictionary.TryGetValue(ATask, AEvent) then
        begin
          if AEvent.ParentGroup = nil then
            ATask.OutlineLevel := 1
          else if AEventToTaskDictionary.TryGetValue(AEvent.ParentGroup,
            AParentTask) and AParentTask.IsValueAssigned
            (TdxGanttTaskAssignedValue.OutlineLevel) then
            ATask.OutlineLevel := AParentTask.OutlineLevel + 1;
        end;
      end;
  end;

  procedure AssignConstraints;
  var
    I: Integer;
    ATask: TdxGanttControlTask;
    AEvent: TcxSchedulerEvent;
    AValue: Variant;
    AMinStart: TDateTime;
  begin
    AMinStart := ADataModel.Tasks[0].Start;
    for I := 0 to ADataModel.Tasks.Count - 1 do
    begin
      ATask := ADataModel.Tasks[I];
      if ATask.Summary then
        Continue;
      if not ATaskToEventDictionary.TryGetValue(ATask, AEvent) then
        Continue;
      AValue := TcxSchedulerEventAccess(AEvent)
        .GetValueDefault(TcxCustomSchedulerStorageAccess(AStorage)
        .FStartField, Null);
      if VarIsNull(AValue) then
        Continue;
      if (AEvent.Start <> VarToDateTime(AValue)) or (AEvent.Start > AMinStart)
      then
      begin
        ATask.ConstraintType := TdxGanttControlTaskConstraintType.
          StartNoEarlierThan;
        ATask.ConstraintDate := AValue;
      end;
    end;
  end;

var
  I, J: Integer;
  AEvent: TcxSchedulerEvent;
  ATask: TdxGanttControlTask;
  AMinStart, AMaxFinish: TDateTime;
  AGanttResource: TdxGanttControlResource;
  ACalendar: TdxGanttControlCalendar;
begin
  ACalendar := ADataModel.Calendars.GetCalendarByUID
    (ADataModel.Properties.CalendarUID);
  AEventToTaskDictionary :=
    TObjectDictionary<TcxSchedulerEvent, TdxGanttControlTask>.Create([]);
  ATaskToEventDictionary := TObjectDictionary<TdxGanttControlTask,
    TcxSchedulerEvent>.Create([]);
  try
    ALinks := TcxObjectList.Create(True);
    try
      AMinStart := MaxDateTime;
      AMaxFinish := MinDateTime;
      for I := 0 to AStorage.EventCount - 1 do
      begin
        AEvent := AStorage.Events[I];
        if AEvent.EventType in [etNone, etPattern] then
        begin
          ATask := ADataModel.Tasks.Append;
          AEventToTaskDictionary.AddOrSetValue(AEvent, ATask);
          ATaskToEventDictionary.AddOrSetValue(ATask, AEvent);
          ATask.Name := AEvent.Caption;
          ATask.Start := AEvent.Start;
          AMinStart := Min(AMinStart, AEvent.Start);
          ATask.Finish := AEvent.Finish;
          AMaxFinish := Max(AMaxFinish, AEvent.Finish);
          ATask.CalendarUID := ACalendar.UID;
          ATask.Duration := TdxGanttControlDuration.Create(ATask.Start,
            ATask.Finish, ACalendar).ToString;
          ATask.DurationFormat := TdxDurationFormat.ElapsedDays;
          ATask.PercentComplete := AEvent.TaskComplete;
          ATask.Milestone := AEvent.Duration = 0;
          ATask.Recurring := AEvent.IsRecurring;
          ATask.Summary := AEvent.IsGroup;
          ATask.Created := Now;
          for J := 0 to AEvent.ResourceIDCount - 1 do
            if dxResourceDictionary.TryGetValue
              (TcxSchedulerStorageResourcesAccess(AStorage.Resources)
              .GetResourceItemByID(AEvent.ResourceIDs[J]), AGanttResource) then
              AssignTaskResource(ATask.UID, AGanttResource.UID, ADataModel);
          for J := 0 to AEvent.TaskLinks.Count - 1 do
            ALinks.Add(TdxSchedulerEventLink.Create(AEvent,
              AEvent.TaskLinks[J].Link, AEvent.TaskLinks[J].Relation));

        end;
      end;
      AddPredecessorLinks;
      AssignOutlineLevel;
      ADataModel.Tasks[0].Start := AMinStart;
      ADataModel.Tasks[0].Finish := AMaxFinish;
      AssignConstraints;
    finally
      ALinks.Free;
    end;
  finally
    AEventToTaskDictionary.Free;
    ATaskToEventDictionary.Free;
  end;
end;

procedure dxGanttControlImportFromSchedulerStorage
  (AStorage: TcxCustomSchedulerStorage; ADataModel: TdxGanttControlDataModel);
var
  APreviousCursor: TCursor;
begin
  APreviousCursor := Screen.Cursor;
  ADataModel.Reset;
  dxResourceDictionary := TObjectDictionary<TcxSchedulerStorageResourceItem,
    TdxGanttControlResource>.Create;
  ADataModel.BeginUpdate;
  try
    Screen.Cursor := crHourGlass;
    ImportResources(AStorage, ADataModel);
    ADataModel.Properties.CalendarUID :=
      ADataModel.Calendars.Create24HoursCalendar.UID;
    ImportEvents(AStorage, ADataModel);
  finally
    dxResourceDictionary.Free;
    ADataModel.EndUpdate;
  end;
  Screen.Cursor := APreviousCursor;
end;

end.
