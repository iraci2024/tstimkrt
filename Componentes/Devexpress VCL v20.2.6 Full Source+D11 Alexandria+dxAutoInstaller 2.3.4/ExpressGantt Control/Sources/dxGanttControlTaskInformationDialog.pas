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

unit dxGanttControlTaskInformationDialog;

{$I cxVer.inc}
{$I dxGanttControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Generics.Defaults, Generics.Collections,
  Variants, Classes, Graphics, Controls, Forms,
  ComCtrls, Dialogs,
  dxForms, cxGraphics, cxControls, cxLookAndFeels, cxLookAndFeelPainters,
  dxLayoutcxEditAdapters,
  dxLayoutControlAdapters, cxContainer, cxEdit, Menus, StdCtrls, cxButtons,
  dxLayoutContainer, cxSpinEdit,
  dxMeasurementUnitEdit, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxClasses,
  dxLayoutControl, dxLayoutLookAndFeels,
  dxCore, dxGanttControl, dxGanttControlCustomDataModel,
  dxGanttControlDataModel, dxGanttControlStrs,
  dxGanttControlViewChart, dxGanttControlTasks, dxGanttControlCustomClasses,
  dxGanttControlCommands, dxGanttControlCalendars,
  cxDateUtils, cxCalendar, cxPC, cxGroupBox;

type
  TdxGanttControlTaskInformationDialogForm = class(TdxForm)
    lcMainGroup_Root: TdxLayoutGroup;
    lcMain: TdxLayoutControl;
    meDuration: TdxMeasurementUnitEdit;
    liDuration: TdxLayoutItem;
    btnDelete: TcxButton;
    liBtnDelete: TdxLayoutItem;
    btnOk: TcxButton;
    liBtnOk: TdxLayoutItem;
    btnCancel: TcxButton;
    liBtnCancel: TdxLayoutItem;
    lgButtons: TdxLayoutGroup;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    edName: TcxTextEdit;
    liName: TdxLayoutItem;
    liEstimated: TdxLayoutCheckBoxItem;
    liPercentComplete: TdxLayoutItem;
    mePercentComplete: TdxMeasurementUnitEdit;
    cmbScheduleMode: TcxComboBox;
    liScheduleMode: TdxLayoutItem;
    lgGeneralGroup: TdxLayoutGroup;
    lgDates: TdxLayoutGroup;
    deDateStart: TcxDateEdit;
    liDateStart: TdxLayoutItem;
    deDateFinish: TcxDateEdit;
    liDateFinish: TdxLayoutItem;
    cmbConstraintType: TcxComboBox;
    liConstraintType: TdxLayoutItem;
    deConstraintDate: TcxDateEdit;
    liConstraintDate: TdxLayoutItem;
    lgAdvancedGroup: TdxLayoutGroup;
    lgConstrainTask: TdxLayoutGroup;
    lgTogGroup: TdxLayoutGroup;
    lgTabs: TdxLayoutGroup;
    dxLayoutSeparatorItem1: TdxLayoutSeparatorItem;
    cmbCalendar: TcxComboBox;
    liCalendar: TdxLayoutItem;
    liDisplayOnTimeline: TdxLayoutCheckBoxItem;
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure meDurationPropertiesChange(Sender: TObject);
    procedure meDurationPropertiesEditValueChanged(Sender: TObject);
    procedure liEstimatedClick(Sender: TObject);
    procedure cmbScheduleModePropertiesChange(Sender: TObject);
    procedure deDateStartPropertiesEditValueChanged(Sender: TObject);
    procedure deDateFinishPropertiesEditValueChanged(Sender: TObject);
    procedure cmbConstraintTypePropertiesEditValueChanged(Sender: TObject);
    procedure deConstraintDatePropertiesEditValueChanged(Sender: TObject);
    procedure deDateStartPropertiesCloseUp(Sender: TObject);
    procedure deDateFinishPropertiesCloseUp(Sender: TObject);
    procedure deConstraintDatePropertiesCloseUp(Sender: TObject);
  private
    FActualTask: TdxGanttControlTask;
    FDurationEditHelper: TdxMeasurementUnitEditHelper;
    FInitializing: Boolean;
    FPercentEditHelper: TdxMeasurementUnitEditHelper;
    FTask: TdxGanttControlTask;

    function GetGanttControl: TdxCustomGanttControl;
  protected
    procedure ApplyLocalization; virtual;
    procedure AddPossibleDurationDescriptions; virtual;
    procedure CheckConstraintDateEnabling; virtual;
    procedure CheckScheduleModeDependency; virtual;
    function GetActualCalendarUID: Integer;
    function GetActualDurationFormat(var AError: Boolean;
      var AErrorText: TCaption): TdxDurationFormat;
    function GetActualDurationValue: Variant;
    function GetActualPercentValue: Variant;
    function GetConstraintTypeComboBoxItemIndex
      (AType: TdxGanttControlTaskConstraintType): Integer;
    function GetConstraintType(const ATypeStr: string)
      : TdxGanttControlTaskConstraintType;
    function GetInitialDurationDescription(AFormat: TdxDurationFormat)
      : string; virtual;
    class function GetDurationFormat(ADescription: string; var AError: Boolean;
      var AErrorText: TCaption): TdxDurationFormat;
    class function GetDuration(AValue: Double;
      AFormat: TdxDurationFormat): Integer;
    procedure Initialize; virtual;
    procedure InitializeCalendar; virtual;
    procedure InitializeDurationEditor;
    procedure InitializePercentEditor;
    class procedure InitializeMeasurementUnits;
    function IsElapsed(AFormat: TdxDurationFormat): Boolean;
    function IsEstimated(AFormat: TdxDurationFormat): Boolean;
    function IsManualScheduleMode: Boolean;
    procedure StoreInitialValues; virtual;

    procedure DateValidateHandler(Sender: TcxDateEdit; AAsStartDate: Boolean);
    procedure DurationEditIncrementValueHandler(Sender: TObject;
      AButton: TcxSpinEditButton; var AValue: Variant; var AHandled: Boolean);
    procedure DurationEditValidateHandler(Sender: TObject;
      var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);
    procedure PercentEditIncrementValueHandler(Sender: TObject;
      AButton: TcxSpinEditButton; var AValue: Variant; var AHandled: Boolean);
    procedure PercentEditValidateHandler(Sender: TObject;
      var DisplayValue: Variant; var ErrorText: TCaption; var Error: Boolean);

    property ActualTask: TdxGanttControlTask read FActualTask;
    property GanttControl: TdxCustomGanttControl read GetGanttControl;
    property DurationEditHelper: TdxMeasurementUnitEditHelper
      read FDurationEditHelper;
    property PercentEditHelper: TdxMeasurementUnitEditHelper
      read FPercentEditHelper;
    property Task: TdxGanttControlTask read FTask;
  public
    constructor Create(AGanttControl: TdxGanttControlBase;
      ATask: TdxGanttControlTask); reintroduce; overload; virtual;
    constructor Create(AGanttControl: TdxGanttControlBase;
      ANewTaskIndex: Integer); reintroduce; overload; virtual;
    destructor Destroy; override;
  end;

  TdxGanttControlTaskInformationDialogFormClass = class of
    TdxGanttControlTaskInformationDialogForm;

var
  dxGanttControlTaskInformationDialogFormClass
    : TdxGanttControlTaskInformationDialogFormClass =
    TdxGanttControlTaskInformationDialogForm;

procedure ShowTaskInformationDialog(AController
  : TdxGanttControlChartViewController; ATask: TdxGanttControlTask); overload;
procedure ShowTaskInformationDialog(AGanttControl: TdxGanttControlBase;
  ATask: TdxGanttControlTask); overload;

implementation

{$R *.dfm}

uses
  DateUtils,
  Math,
  dxCoreClasses,
  cxGeometry,
  dxGanttControlUtils,
  dxGanttControlTaskCommands;

type
  TdxCustomGanttControlAccess = class(TdxCustomGanttControl);
  TdxGanttControlTaskPredecessorLinkAccess = class
    (TdxGanttControlTaskPredecessorLink);
  TcxTabsViewInfoAccess = class(TcxTabsViewInfo);

var
  muElapsedTimePrefix, muEstimatedTimePostfix, muMinuteExtraShort, muMinute,
    muMinutes, muMinuteShort, muMinutesShort, muHourExtraShort, muHour, muHours,
    muHourShort, muHoursShort, muDayExtraShort, muDay, muDays, muWeekExtraShort,
    muWeek, muWeeks, muWeekShort, muWeeksShort, muMonthExtraShort, muMonth,
    muMonths, muMonthShort, muMonthsShort, muPercent: string;

  ctAsSoonAsPossible, ctAsLateAsPossible, ctMustStartOn, ctMustFinishOn,
    ctStartNoEarlierThan, ctStartNoLaterThan, ctFinishNoEarlierThan,
    ctFinishNoLaterThan: string;

  dxGanttCalendarUIDs: TArray<Integer>;

type

  { TdxConstraintChangesHelper }

  TdxConstraintChangesHelper = record
  public
    ConstraintTypeIndex: Integer;
    ConstraintDate: Variant;
    Duration: Int64;
    FinishDate: Variant;
    StartDate: Variant;

    function IsConstraintTypeChanged(ACandidateIndex: Integer): Boolean;
    function IsConstraintDateChanged(ACandidate: Variant): Boolean;
    function IsFinishChanged(ACandidate: Variant): Boolean;
    function IsStartChanged(ACandidate: Variant): Boolean;
  end;

var
  ConstraintChangesHelper: TdxConstraintChangesHelper;

type
  { TdxDurationEditHelper }

  TdxDurationEditHelper = class(TdxMeasurementUnitEditHelper)
  protected
    function GetCheckableCandidate(const ADescription: string): string;
      override;
  end;

  { TdxConstraintChangesHelper }

function TdxConstraintChangesHelper.IsConstraintTypeChanged(ACandidateIndex
  : Integer): Boolean;
begin
  Result := ConstraintTypeIndex <> ACandidateIndex;
end;

function TdxConstraintChangesHelper.IsConstraintDateChanged
  (ACandidate: Variant): Boolean;
begin
  Result := ConstraintDate <> ACandidate;
end;

function TdxConstraintChangesHelper.IsFinishChanged
  (ACandidate: Variant): Boolean;
begin
  Result := FinishDate <> ACandidate;
end;

function TdxConstraintChangesHelper.IsStartChanged(ACandidate: Variant)
  : Boolean;
begin
  Result := StartDate <> ACandidate;
end;

{ TdxDurationEditHelper }

function TdxDurationEditHelper.GetCheckableCandidate(const ADescription
  : string): string;
begin
  Result := inherited GetCheckableCandidate(ADescription);
  while (Length(Result) > 1) and
    (Result[Length(Result)] = muEstimatedTimePostfix[1]) and
    (Result[Length(Result) - 1] = muEstimatedTimePostfix[1]) do
    Delete(Result, Length(Result), 1);
end;

{ TdxGanttControlTaskInformationDialogForm }

constructor TdxGanttControlTaskInformationDialogForm.Create
  (AGanttControl: TdxGanttControlBase; ATask: TdxGanttControlTask);
begin
  inherited Create(AGanttControl);
  FTask := ATask;
  FActualTask := TdxGanttControlTask.Create(ATask.Owner);
  FActualTask.BeginUpdate;
  FActualTask.Assign(Task);
  Initialize;
  FActualTask.ResetValue(TdxGanttTaskAssignedValue.&Type);
  FActualTask.ResetValue(TdxGanttTaskAssignedValue.Duration);
  FActualTask.ResetValue(TdxGanttTaskAssignedValue.DurationFormat);
  FActualTask.ResetValue(TdxGanttTaskAssignedValue.PercentComplete);
  FActualTask.ResetValue(TdxGanttTaskAssignedValue.Manual);
  FActualTask.ResetValue(TdxGanttTaskAssignedValue.Start);
  FActualTask.ResetValue(TdxGanttTaskAssignedValue.Finish);
  FActualTask.ResetValue(TdxGanttTaskAssignedValue.ConstraintType);
  FActualTask.ResetValue(TdxGanttTaskAssignedValue.ConstraintDate);
end;

constructor TdxGanttControlTaskInformationDialogForm.Create
  (AGanttControl: TdxGanttControlBase; ANewTaskIndex: Integer);
begin
  inherited Create(AGanttControl);
  FTask := nil;
  FActualTask := TdxGanttControlTask.Create(GanttControl.DataModel.Tasks);
  FActualTask.Manual := GanttControl.DataModel.Properties.
    MarkNewTasksAsManuallyScheduled;
  Initialize;
end;

procedure TdxGanttControlTaskInformationDialogForm.
  deConstraintDatePropertiesEditValueChanged(Sender: TObject);
begin
  if FInitializing then
    Exit;
  if (Trim(deConstraintDate.Text) = '') or
    not ConstraintChangesHelper.IsStartChanged(deConstraintDate.EditValue) then
    FActualTask.ResetValue(TdxGanttTaskAssignedValue.ConstraintDate)
  else
  begin
    FActualTask.ConstraintDate := deConstraintDate.Date;
    FActualTask.ResetValue(TdxGanttTaskAssignedValue.Start);
    FActualTask.ResetValue(TdxGanttTaskAssignedValue.Finish)
  end;
end;

procedure TdxGanttControlTaskInformationDialogForm.
  cmbConstraintTypePropertiesEditValueChanged(Sender: TObject);
begin
  if FInitializing then
    Exit;
  CheckConstraintDateEnabling;
  FActualTask.ConstraintType := GetConstraintType(cmbConstraintType.Text);
  if ConstraintChangesHelper.IsConstraintTypeChanged(cmbConstraintType.ItemIndex)
  then
  begin
    FActualTask.ResetValue(TdxGanttTaskAssignedValue.Start);
    FActualTask.ResetValue(TdxGanttTaskAssignedValue.Finish);
  end
  else
    FActualTask.ResetValue(TdxGanttTaskAssignedValue.ConstraintType);
end;

procedure TdxGanttControlTaskInformationDialogForm.
  deDateFinishPropertiesEditValueChanged(Sender: TObject);
begin
  if FInitializing then
    Exit;
  if (Trim(deDateFinish.Text) = '') or
    not ConstraintChangesHelper.IsStartChanged(deDateFinish.EditValue) then
    FActualTask.ResetValue(TdxGanttTaskAssignedValue.Finish)
  else
  begin
    FActualTask.Finish := deDateFinish.Date;
    FActualTask.ResetValue(TdxGanttTaskAssignedValue.ConstraintType);
    FActualTask.ResetValue(TdxGanttTaskAssignedValue.ConstraintDate);
  end;
end;

procedure TdxGanttControlTaskInformationDialogForm.deDateStartPropertiesCloseUp
  (Sender: TObject);
begin
  DateValidateHandler(deDateStart, True);
end;

procedure TdxGanttControlTaskInformationDialogForm.deDateFinishPropertiesCloseUp
  (Sender: TObject);
begin
  DateValidateHandler(deDateFinish, False);
end;

procedure TdxGanttControlTaskInformationDialogForm.
  deConstraintDatePropertiesCloseUp(Sender: TObject);
var
  AConstraintType: TdxGanttControlTaskConstraintType;
begin
  AConstraintType := GetConstraintType(cmbConstraintType.Text);
  if AConstraintType in [TdxGanttControlTaskConstraintType.MustStartOn,
    TdxGanttControlTaskConstraintType.StartNoEarlierThan,
    TdxGanttControlTaskConstraintType.StartNoLaterThan] then
    DateValidateHandler(deConstraintDate, True)
  else if AConstraintType in [TdxGanttControlTaskConstraintType.MustFinishOn,
    TdxGanttControlTaskConstraintType.FinishNoEarlierThan,
    TdxGanttControlTaskConstraintType.FinishNoLaterThan] then
    DateValidateHandler(deConstraintDate, False);
end;

procedure TdxGanttControlTaskInformationDialogForm.
  deDateStartPropertiesEditValueChanged(Sender: TObject);
begin
  if FInitializing then
    Exit;
  if (Trim(deDateStart.Text) = '') or not ConstraintChangesHelper.IsStartChanged
    (deDateStart.EditValue) then
    FActualTask.ResetValue(TdxGanttTaskAssignedValue.Start)
  else
  begin
    FActualTask.Start := deDateStart.Date;
    FActualTask.ResetValue(TdxGanttTaskAssignedValue.ConstraintType);
    FActualTask.ResetValue(TdxGanttTaskAssignedValue.ConstraintDate);
  end;
end;

destructor TdxGanttControlTaskInformationDialogForm.Destroy;
begin
  FreeAndNil(FDurationEditHelper);
  FreeAndNil(FPercentEditHelper);
  FreeAndNil(FActualTask);
  inherited Destroy;
end;

procedure TdxGanttControlTaskInformationDialogForm.FormCloseQuery
  (Sender: TObject; var CanClose: Boolean);
var
  ADisplayValue: Variant;
  AErrorText: TCaption;
  AError: Boolean;
  ADurationFormat: TdxDurationFormat;
begin
  if ModalResult <> mrOk then
    Exit;

  if FActualTask <> nil then
  begin
    DurationEditValidateHandler(nil, ADisplayValue, AErrorText, AError);
    CanClose := not AError;
    if not CanClose then
    begin
      MessageDlg(AErrorText, mtWarning, [mbOK], 0);
      meDuration.SetFocus;
      Exit;
    end;
    ADurationFormat := GetActualDurationFormat(AError, AErrorText);
    if Task <> nil then
      if ((ADurationFormat = TdxDurationFormat.Days) and
        (Task.DurationFormat = TdxDurationFormat.Null)) or
        ((ADurationFormat = TdxDurationFormat.EstimatedDays) and
        (Task.DurationFormat = TdxDurationFormat.EstimatedNull)) then
        ADurationFormat := Task.DurationFormat;
    FActualTask.DurationFormat := ADurationFormat;

    PercentEditValidateHandler(nil, ADisplayValue, AErrorText, AError);
    CanClose := not AError;
    if not CanClose then
    begin
      MessageDlg(AErrorText, mtWarning, [mbOK], 0);
      mePercentComplete.SetFocus;
      Exit;
    end;
    FActualTask.PercentComplete := GetActualPercentValue;

    FActualTask.Name := Trim(edName.Text);
    FActualTask.Manual := IsManualScheduleMode;
    FActualTask.DisplayOnTimeline := liDisplayOnTimeline.Checked;
    FActualTask.CalendarUID := GetActualCalendarUID;
  end;

  if CanClose and (FActualTask = nil) then
    CanClose := not GanttControl.OptionsBehavior.ConfirmDelete or
      (MessageDlg(GetDeletingTaskConfirmation(Task), mtConfirmation,
      [mbOK, mbCancel], 0) = mrOk);
end;

procedure TdxGanttControlTaskInformationDialogForm.FormShow(Sender: TObject);
var
  dH, dW: Integer;
begin
  lcMain.HandleNeeded;
  dH := lcMain.OccupiedClientHeight + lcMain.Top - ClientHeight;
  dW := lcMain.OccupiedClientWidth - ClientWidth;
  Width := Width + dW;
  Height := Height + dH;
  edName.SetFocus;
end;

procedure TdxGanttControlTaskInformationDialogForm.ApplyLocalization;
begin
  InitializeMeasurementUnits;
  if ActualTask.Summary then
    Caption := cxGetResourceString
      (@sdxGanttControlTaskInformationDialogSummaryCaption)
  else
    Caption := cxGetResourceString
      (@sdxGanttControlTaskInformationDialogCaption);

  lgGeneralGroup.CaptionOptions.Text :=
    cxGetResourceString(@sdxGanttControlTaskInformationDialogGeneralTabCaption);
  lgAdvancedGroup.CaptionOptions.Text :=
    cxGetResourceString
    (@sdxGanttControlTaskInformationDialogAdvancedTabCaption);

  liName.CaptionOptions.Text := cxGetResourceString
    (@sdxGanttControlTaskInformationDialogName);
  liDuration.CaptionOptions.Text :=
    cxGetResourceString(@sdxGanttControlTaskInformationDialogDuration);
  liEstimated.CaptionOptions.Text :=
    cxGetResourceString(@sdxGanttControlTaskInformationDialogEstimated);

  liPercentComplete.CaptionOptions.Text :=
    cxGetResourceString(@sdxGanttControlTaskInformationDialogPercentComplete);
  liScheduleMode.CaptionOptions.Text :=
    cxGetResourceString(@sdxGanttControlTaskInformationDialogScheduleMode);
  cmbScheduleMode.Properties.Items.Add
    (cxGetResourceString
    (@sdxGanttControlTaskInformationDialogManuallySchedule));
  cmbScheduleMode.Properties.Items.Add
    (cxGetResourceString(@sdxGanttControlTaskInformationDialogAutoSchedule));
  lgDates.CaptionOptions.Text := cxGetResourceString
    (@sdxGanttControlTaskInformationDialogDates);
  liDateStart.CaptionOptions.Text :=
    cxGetResourceString(@sdxGanttControlTaskInformationDialogDateStart);
  liDateFinish.CaptionOptions.Text :=
    cxGetResourceString(@sdxGanttControlTaskInformationDialogDateFinish);
  liDisplayOnTimeline.CaptionOptions.Text :=
    cxGetResourceString(@sdxGanttControlTaskInformationDialogDisplayOnTimeline);

  lgConstrainTask.CaptionOptions.Text :=
    cxGetResourceString(@sdxGanttControlTaskInformationDialogConstrainTask);
  liConstraintType.CaptionOptions.Text :=
    cxGetResourceString(@sdxGanttControlTaskInformationDialogConstraintType);

  ctAsSoonAsPossible := cxGetResourceString
    (@sdxGanttControlTaskConstraintTypeASAP);
  ctAsLateAsPossible := cxGetResourceString
    (@sdxGanttControlTaskConstraintTypeALAP);
  ctMustStartOn := cxGetResourceString(@sdxGanttControlTaskConstraintTypeMSO);
  ctMustFinishOn := cxGetResourceString(@sdxGanttControlTaskConstraintTypeMFO);
  ctStartNoEarlierThan := cxGetResourceString
    (@sdxGanttControlTaskConstraintTypeSNET);
  ctStartNoLaterThan := cxGetResourceString
    (@sdxGanttControlTaskConstraintTypeSNLT);
  ctFinishNoEarlierThan := cxGetResourceString
    (@sdxGanttControlTaskConstraintTypeFNET);
  ctFinishNoLaterThan := cxGetResourceString
    (@sdxGanttControlTaskConstraintTypeFNLT);

  liConstraintDate.CaptionOptions.Text :=
    cxGetResourceString(@sdxGanttControlTaskInformationDialogConstraintDate);

  liCalendar.CaptionOptions.Text :=
    cxGetResourceString(@sdxGanttControlTaskInformationDialogCalendar);

  btnDelete.Caption := cxGetResourceString(@sdxGanttControlDialogDelete);
  btnOk.Caption := cxGetResourceString(@sdxGanttControlDialogOk);
  btnCancel.Caption := cxGetResourceString(@sdxGanttControlDialogCancel);
end;

procedure TdxGanttControlTaskInformationDialogForm.
  AddPossibleDurationDescriptions;

  procedure InternalAdd(const ADescription: string);
  begin
    DurationEditHelper.AddPossibleDescription(ADescription);
    DurationEditHelper.AddPossibleDescription(muElapsedTimePrefix +
      ADescription);
    DurationEditHelper.AddPossibleDescription
      (ADescription + muEstimatedTimePostfix);
    DurationEditHelper.AddPossibleDescription(muElapsedTimePrefix + ADescription
      + muEstimatedTimePostfix);
  end;

begin
  InternalAdd(muMinuteExtraShort);
  InternalAdd(muMinute);
  InternalAdd(muMinutes);
  InternalAdd(muMinuteShort);
  InternalAdd(muMinutesShort);
  InternalAdd(muHourExtraShort);
  InternalAdd(muHour);
  InternalAdd(muHours);
  InternalAdd(muHourShort);
  InternalAdd(muHoursShort);
  InternalAdd(muDayExtraShort);
  InternalAdd(muDay);
  InternalAdd(muDays);
  InternalAdd(muWeekExtraShort);
  InternalAdd(muWeek);
  InternalAdd(muWeeks);
  InternalAdd(muWeekShort);
  InternalAdd(muWeeksShort);
  InternalAdd(muMonthExtraShort);
  InternalAdd(muMonth);
  InternalAdd(muMonths);
  InternalAdd(muMonthShort);
  InternalAdd(muMonthsShort);
end;

procedure TdxGanttControlTaskInformationDialogForm.CheckConstraintDateEnabling;
begin
  liConstraintDate.Enabled := not(GetConstraintType(cmbConstraintType.Text)
    in [TdxGanttControlTaskConstraintType.AsSoonAsPossible,
    TdxGanttControlTaskConstraintType.AsLateAsPossible]);
end;

procedure TdxGanttControlTaskInformationDialogForm.CheckScheduleModeDependency;
begin
  liDuration.Enabled := IsManualScheduleMode or not ActualTask.Summary;
  liEstimated.Enabled := liDuration.Enabled and not ActualTask.Summary;
  liDateStart.Enabled := liDuration.Enabled;
  liDateFinish.Enabled := liDateStart.Enabled;
  liConstraintType.Enabled := not IsManualScheduleMode;
end;

function TdxGanttControlTaskInformationDialogForm.GetActualCalendarUID: Integer;
begin
  Result := dxGanttCalendarUIDs[cmbCalendar.ItemIndex];
end;

function TdxGanttControlTaskInformationDialogForm.GetActualDurationFormat
  (var AError: Boolean; var AErrorText: TCaption): TdxDurationFormat;
var
  AActualDescription: string;
begin
  AActualDescription := TdxDurationEditHelper(DurationEditHelper)
    .GetCheckableCandidate(DurationEditHelper.GetDescriptionFromText
    (meDuration.Text));
  if AActualDescription = '' then
  begin
    AActualDescription := DurationEditHelper.DefaultDescription;
    if liEstimated.Checked then
      AActualDescription := AActualDescription + muEstimatedTimePostfix;
  end;
  Result := GetDurationFormat(AActualDescription, AError, AErrorText);
end;

function TdxGanttControlTaskInformationDialogForm.
  GetActualDurationValue: Variant;
begin
  if Trim(meDuration.Text) = '' then
    Result := 1
  else
    Result := DurationEditHelper.GetValueFromText(Trim(meDuration.Text), False);
end;

function TdxGanttControlTaskInformationDialogForm.GetActualPercentValue
  : Variant;
begin
  if Trim(mePercentComplete.Text) = '' then
    Result := 0
  else
    Result := PercentEditHelper.GetValueFromText(Trim(mePercentComplete.Text));
end;

function TdxGanttControlTaskInformationDialogForm.
  GetConstraintTypeComboBoxItemIndex
  (AType: TdxGanttControlTaskConstraintType): Integer;
var
  ATypeStr: string;
  I: Integer;
begin
  Result := 0;
  case AType of
    TdxGanttControlTaskConstraintType.AsSoonAsPossible:
      ATypeStr := ctAsSoonAsPossible;
    TdxGanttControlTaskConstraintType.AsLateAsPossible:
      ATypeStr := ctAsLateAsPossible;
    TdxGanttControlTaskConstraintType.MustStartOn:
      ATypeStr := ctMustStartOn;
    TdxGanttControlTaskConstraintType.MustFinishOn:
      ATypeStr := ctMustFinishOn;
    TdxGanttControlTaskConstraintType.StartNoEarlierThan:
      ATypeStr := ctStartNoEarlierThan;
    TdxGanttControlTaskConstraintType.StartNoLaterThan:
      ATypeStr := ctStartNoLaterThan;
    TdxGanttControlTaskConstraintType.FinishNoEarlierThan:
      ATypeStr := ctFinishNoEarlierThan;
  else
    ATypeStr := ctFinishNoLaterThan;
  end;
  for I := 0 to cmbConstraintType.Properties.Items.Count - 1 do
    if cmbConstraintType.Properties.Items[I] = ATypeStr then
    begin
      Result := I;
      Break;
    end;
end;

function TdxGanttControlTaskInformationDialogForm.GetConstraintType
  (const ATypeStr: string): TdxGanttControlTaskConstraintType;
begin
  if ATypeStr = ctAsSoonAsPossible then
    Result := TdxGanttControlTaskConstraintType.AsSoonAsPossible
  else if ATypeStr = ctAsLateAsPossible then
    Result := TdxGanttControlTaskConstraintType.AsLateAsPossible
  else if ATypeStr = ctMustStartOn then
    Result := TdxGanttControlTaskConstraintType.MustStartOn
  else if ATypeStr = ctMustFinishOn then
    Result := TdxGanttControlTaskConstraintType.MustFinishOn
  else if ATypeStr = ctStartNoEarlierThan then
    Result := TdxGanttControlTaskConstraintType.StartNoEarlierThan
  else if ATypeStr = ctStartNoLaterThan then
    Result := TdxGanttControlTaskConstraintType.StartNoLaterThan
  else if ATypeStr = ctFinishNoLaterThan then
    Result := TdxGanttControlTaskConstraintType.FinishNoLaterThan
  else
    Result := TdxGanttControlTaskConstraintType.FinishNoEarlierThan;
end;

function TdxGanttControlTaskInformationDialogForm.GetInitialDurationDescription
  (AFormat: TdxDurationFormat): string;
begin
  case AFormat of
    TdxDurationFormat.Minutes, TdxDurationFormat.EstimatedMinutes:
      Result := muMinuteExtraShort;
    TdxDurationFormat.ElapsedMinutes, TdxDurationFormat.EstimatedElapsedMinutes:
      Result := muElapsedTimePrefix + muMinuteExtraShort;
    TdxDurationFormat.Hours, TdxDurationFormat.EstimatedHours:
      Result := muHourExtraShort;
    TdxDurationFormat.ElapsedHours, TdxDurationFormat.EstimatedElapsedHours:
      Result := muElapsedTimePrefix + muHourExtraShort;
    TdxDurationFormat.Days, TdxDurationFormat.EstimatedDays,
      TdxDurationFormat.Null, TdxDurationFormat.EstimatedNull:
      Result := muDayExtraShort;
    TdxDurationFormat.ElapsedDays, TdxDurationFormat.EstimatedElapsedDays:
      Result := muElapsedTimePrefix + muDayExtraShort;
    TdxDurationFormat.Weeks, TdxDurationFormat.EstimatedWeeks:
      Result := muWeekExtraShort;
    TdxDurationFormat.ElapsedWeeks, TdxDurationFormat.EstimatedElapsedWeeks:
      Result := muElapsedTimePrefix + muWeekExtraShort;
    TdxDurationFormat.Months, TdxDurationFormat.EstimatedMonths:
      Result := muMonthExtraShort;
    TdxDurationFormat.ElapsedMonths, TdxDurationFormat.EstimatedElapsedMonths:
      Result := muElapsedTimePrefix + muMonthExtraShort;
  end;
  Result := ' ' + Result;
end;

class function TdxGanttControlTaskInformationDialogForm.GetDurationFormat
  (ADescription: string; var AError: Boolean; var AErrorText: TCaption)
  : TdxDurationFormat;

  function CheckMeasureUnit(const AMeasureUnit: string;
    ACheckUnits: array of string): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := Low(ACheckUnits) to High(ACheckUnits) do
      if AMeasureUnit = AnsiUpperCase(ACheckUnits[I]) then
        Exit;
    Result := False;
  end;

begin
  AError := False;
  InitializeMeasurementUnits;
  AErrorText :=
    Format(cxGetResourceString(@sdxGanttControlMessageInvalidMeasurementUnit),
    [ADescription]);
  ADescription := AnsiUpperCase(Trim(ADescription));
  Result := TdxDurationFormat.Days;

  if CheckMeasureUnit(ADescription, [muDayExtraShort, muDay, muDays]) then
    Result := TdxDurationFormat.Days
  else if CheckMeasureUnit(ADescription, [muHourExtraShort, muHourShort,
    muHoursShort, muHour, muHours]) then
    Result := TdxDurationFormat.Hours
  else if CheckMeasureUnit(ADescription, [muWeekExtraShort, muWeekShort,
    muWeeksShort, muWeek, muWeeks]) then
    Result := TdxDurationFormat.Weeks
  else if CheckMeasureUnit(ADescription, [muMonthExtraShort, muMonthShort,
    muMonthsShort, muMonth, muMonths]) then
    Result := TdxDurationFormat.Months
  else if CheckMeasureUnit(ADescription, [muMinuteExtraShort, muMinuteShort,
    muMinutesShort, muMinute, muMinutes]) then
    Result := TdxDurationFormat.Minutes
  else

    if CheckMeasureUnit(ADescription, [muElapsedTimePrefix + muDayExtraShort,
    muElapsedTimePrefix + muDay, muElapsedTimePrefix + muDays]) then
    Result := TdxDurationFormat.ElapsedDays
  else if CheckMeasureUnit(ADescription,
    [muElapsedTimePrefix + muHourExtraShort, muElapsedTimePrefix + muHourShort,
    muElapsedTimePrefix + muHoursShort, muElapsedTimePrefix + muHour,
    muElapsedTimePrefix + muHours]) then
    Result := TdxDurationFormat.ElapsedHours
  else if CheckMeasureUnit(ADescription,
    [muElapsedTimePrefix + muWeekExtraShort, muElapsedTimePrefix + muWeekShort,
    muElapsedTimePrefix + muWeeksShort, muElapsedTimePrefix + muWeek,
    muElapsedTimePrefix + muWeeks]) then
    Result := TdxDurationFormat.ElapsedWeeks
  else if CheckMeasureUnit(ADescription,
    [muElapsedTimePrefix + muMonthExtraShort, muElapsedTimePrefix +
    muMonthShort, muElapsedTimePrefix + muMonthsShort,
    muElapsedTimePrefix + muMonth, muElapsedTimePrefix + muMonths]) then
    Result := TdxDurationFormat.ElapsedMonths
  else if CheckMeasureUnit(ADescription,
    [muElapsedTimePrefix + muMinuteExtraShort, muElapsedTimePrefix +
    muMinuteShort, muElapsedTimePrefix + muMinutesShort,
    muElapsedTimePrefix + muMinute, muElapsedTimePrefix + muMinutes]) then
    Result := TdxDurationFormat.ElapsedMinutes
  else

    if CheckMeasureUnit(ADescription, [muDayExtraShort + muEstimatedTimePostfix,
    muDay + muEstimatedTimePostfix, muDays + muEstimatedTimePostfix]) then
    Result := TdxDurationFormat.EstimatedDays
  else if CheckMeasureUnit(ADescription,
    [muHourExtraShort + muEstimatedTimePostfix,
    muHourShort + muEstimatedTimePostfix, muHoursShort + muEstimatedTimePostfix,
    muHour + muEstimatedTimePostfix, muHours + muEstimatedTimePostfix]) then
    Result := TdxDurationFormat.EstimatedHours
  else if CheckMeasureUnit(ADescription,
    [muWeekExtraShort + muEstimatedTimePostfix,
    muWeekShort + muEstimatedTimePostfix, muWeeksShort + muEstimatedTimePostfix,
    muWeek + muEstimatedTimePostfix, muWeeks + muEstimatedTimePostfix]) then
    Result := TdxDurationFormat.EstimatedWeeks
  else if CheckMeasureUnit(ADescription,
    [muMonthExtraShort + muEstimatedTimePostfix,
    muMonthShort + muEstimatedTimePostfix,
    muMonthsShort + muEstimatedTimePostfix, muMonth + muEstimatedTimePostfix,
    muMonths + muEstimatedTimePostfix]) then
    Result := TdxDurationFormat.EstimatedMonths
  else if CheckMeasureUnit(ADescription,
    [muMinuteExtraShort + muEstimatedTimePostfix,
    muMinuteShort + muEstimatedTimePostfix,
    muMinutesShort + muEstimatedTimePostfix, muMinute + muEstimatedTimePostfix,
    muMinutes + muEstimatedTimePostfix]) then
    Result := TdxDurationFormat.EstimatedMinutes
  else

    if CheckMeasureUnit(ADescription,
    [muElapsedTimePrefix + muDayExtraShort + muEstimatedTimePostfix,
    muElapsedTimePrefix + muDay + muEstimatedTimePostfix,
    muElapsedTimePrefix + muDays + muEstimatedTimePostfix]) then
    Result := TdxDurationFormat.EstimatedElapsedDays
  else if CheckMeasureUnit(ADescription,
    [muElapsedTimePrefix + muHourExtraShort + muEstimatedTimePostfix,
    muElapsedTimePrefix + muHourShort + muEstimatedTimePostfix,
    muElapsedTimePrefix + muHoursShort + muEstimatedTimePostfix,
    muElapsedTimePrefix + muHour + muEstimatedTimePostfix,
    muElapsedTimePrefix + muHours + muEstimatedTimePostfix]) then
    Result := TdxDurationFormat.EstimatedElapsedHours
  else if CheckMeasureUnit(ADescription,
    [muElapsedTimePrefix + muWeekExtraShort + muEstimatedTimePostfix,
    muElapsedTimePrefix + muWeekShort + muEstimatedTimePostfix,
    muElapsedTimePrefix + muWeeksShort + muEstimatedTimePostfix,
    muElapsedTimePrefix + muWeek + muEstimatedTimePostfix,
    muElapsedTimePrefix + muWeeks + muEstimatedTimePostfix]) then
    Result := TdxDurationFormat.EstimatedElapsedWeeks
  else if CheckMeasureUnit(ADescription,
    [muElapsedTimePrefix + muMonthExtraShort + muEstimatedTimePostfix,
    muElapsedTimePrefix + muMonthShort + muEstimatedTimePostfix,
    muElapsedTimePrefix + muMonthsShort + muEstimatedTimePostfix,
    muElapsedTimePrefix + muMonth + muEstimatedTimePostfix,
    muElapsedTimePrefix + muMonths + muEstimatedTimePostfix]) then
    Result := TdxDurationFormat.EstimatedElapsedMonths
  else if CheckMeasureUnit(ADescription,
    [muElapsedTimePrefix + muMinuteExtraShort + muEstimatedTimePostfix,
    muElapsedTimePrefix + muMinuteShort + muEstimatedTimePostfix,
    muElapsedTimePrefix + muMinutesShort + muEstimatedTimePostfix,
    muElapsedTimePrefix + muMinute + muEstimatedTimePostfix,
    muElapsedTimePrefix + muMinutes + muEstimatedTimePostfix]) then
    Result := TdxDurationFormat.EstimatedElapsedMinutes
  else
    AError := True;
end;

class function TdxGanttControlTaskInformationDialogForm.GetDuration
  (AValue: Double; AFormat: TdxDurationFormat): Integer;
begin
  Result := 0;
  case AFormat of
    TdxDurationFormat.Minutes, TdxDurationFormat.ElapsedMinutes:
      Result := Round(AValue * 10);
    TdxDurationFormat.Hours, TdxDurationFormat.ElapsedHours:
      Result := Round(AValue * 600);
    TdxDurationFormat.Days:
      Result := Round(AValue * 4800);
    TdxDurationFormat.ElapsedDays:
      Result := Round(AValue * 14400);
    TdxDurationFormat.Weeks:
      Result := Round(AValue * 24000);
    TdxDurationFormat.ElapsedWeeks:
      Result := Round(AValue * 100800);
    TdxDurationFormat.Months:
      Result := Round(AValue * 96000);
    TdxDurationFormat.ElapsedMonths:
      Result := Round(AValue * 432000);
  end;
end;

procedure TdxGanttControlTaskInformationDialogForm.Initialize;
begin
  FInitializing := True;
  ApplyLocalization;
  BiDiMode := GanttControl.BiDiMode;
  dxLayoutCxLookAndFeel1.LookAndFeel := TdxCustomGanttControlAccess
    (GanttControl).LookAndFeel;
  edName.Text := ActualTask.Name;

  InitializeCalendar;

  InitializeDurationEditor;
  InitializePercentEditor;

  deDateStart.Properties.DisplayFormat :=
    TdxGanttControlUtils.GetShortDateTimeFormat;
  deDateStart.Properties.EditFormat := deDateStart.Properties.DisplayFormat;
  deDateStart.Properties.MinDate := TdxGanttControlDataModel.MinDate;
  deDateFinish.Properties.DisplayFormat := deDateStart.Properties.DisplayFormat;
  deDateFinish.Properties.EditFormat := deDateStart.Properties.EditFormat;
  deDateFinish.Properties.MinDate := TdxGanttControlDataModel.MinDate;
  deConstraintDate.Properties.DisplayFormat :=
    deDateStart.Properties.DisplayFormat;
  deConstraintDate.Properties.EditFormat :=
    deDateStart.Properties.DisplayFormat;
  deConstraintDate.Properties.MinDate := TdxGanttControlDataModel.MinDate;
  if Task <> nil then
  begin
    if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Start) then
      deDateStart.Date := Task.Start;
    if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Finish) then
      deDateFinish.Date := Task.Finish;
  end;

  cmbScheduleMode.ItemIndex := Integer(ActualTask.Manual <> True);
  CheckScheduleModeDependency;

  liDisplayOnTimeline.Checked := ActualTask.DisplayOnTimeline or
    (not GanttControl.ViewTimeline.ShowOnlyExplicitlyAddedTasks and
    not ActualTask.IsValueAssigned
    (TdxGanttTaskAssignedValue.DisplayOnTimeline));

  if ActualTask.Summary then
  begin
    cmbConstraintType.Properties.Items.Add(ctAsSoonAsPossible);
    cmbConstraintType.Properties.Items.Add(ctFinishNoEarlierThan);
    cmbConstraintType.Properties.Items.Add(ctStartNoEarlierThan);
  end
  else
  begin
    cmbConstraintType.Properties.Items.Add(ctAsLateAsPossible);
    cmbConstraintType.Properties.Items.Add(ctAsSoonAsPossible);
    cmbConstraintType.Properties.Items.Add(ctFinishNoEarlierThan);
    cmbConstraintType.Properties.Items.Add(ctFinishNoLaterThan);
    cmbConstraintType.Properties.Items.Add(ctMustFinishOn);
    cmbConstraintType.Properties.Items.Add(ctMustStartOn);
    cmbConstraintType.Properties.Items.Add(ctStartNoEarlierThan);
    cmbConstraintType.Properties.Items.Add(ctStartNoLaterThan);
  end;
  cmbConstraintType.ItemIndex := GetConstraintTypeComboBoxItemIndex
    (ActualTask.ConstraintType);
  if ActualTask.IsValueAssigned(TdxGanttTaskAssignedValue.ConstraintDate) then
    deConstraintDate.Date := ActualTask.ConstraintDate;
  CheckConstraintDateEnabling;

  liBtnDelete.Visible := Task <> nil;
  StoreInitialValues;
  FInitializing := False;
end;

procedure TdxGanttControlTaskInformationDialogForm.InitializeCalendar;
var
  I: Integer;
  AList: TList<Integer>;
begin
  AList := TList<Integer>.Create;
  try
    cmbCalendar.Properties.Items.Clear;
    cmbCalendar.Properties.Items.Add
      (cxGetResourceString(@sdxGanttControlCaptionNone));
    cmbCalendar.ItemIndex := 0;
    AList.Add(-1);
    for I := 0 to GanttControl.DataModel.Calendars.Count - 1 do
      if GanttControl.DataModel.Calendars[I].IsBaseCalendar then
      begin
        cmbCalendar.Properties.Items.Add
          (GanttControl.DataModel.Calendars[I].Name);
        AList.Add(GanttControl.DataModel.Calendars[I].UID);
        if Task = nil then
          cmbCalendar.ItemIndex := 0
        else if Task.CalendarUID = GanttControl.DataModel.Calendars[I].UID then
          cmbCalendar.ItemIndex := cmbCalendar.Properties.Items.Count - 1;
      end;
{$IFNDEF DELPHIXE}
    SetLength(dxGanttCalendarUIDs, AList.Count);
    for I := 0 to AList.Count - 1 do
      dxGanttCalendarUIDs[I] := AList[I];
{$ELSE}
    dxGanttCalendarUIDs := AList.ToArray;
{$ENDIF}
  finally
    AList.Free;
  end;
end;

procedure TdxGanttControlTaskInformationDialogForm.InitializeDurationEditor;
var
  AFormat: TdxDurationFormat;
  ADuration: TdxGanttControlDuration;
begin
  if (Task = nil) or Task.Blank then
  begin
    ADuration := TdxGanttControlDuration.Create
      (GanttControl.DataModel.DefaultTaskDuration);
    AFormat := TdxDurationFormat.Days;
  end
  else
  begin
    ADuration := TdxGanttControlDuration.Create(Task.RealDuration);
    AFormat := Task.RealDurationFormat;
  end;
  ActualTask.Duration := ADuration.ToString;
  ActualTask.DurationFormat := AFormat;
  FDurationEditHelper := TdxDurationEditHelper.Create
    (GetInitialDurationDescription(AFormat), 1, 2, 0, MaxCurrency,
    cxGetResourceString(@sdxGanttControlDurationFormatDayExtraShort));
  AddPossibleDurationDescriptions;
  meDuration.ActiveProperties.OnIncrementValue :=
    DurationEditIncrementValueHandler;
  meDuration.ActiveProperties.OnValidate := DurationEditValidateHandler;
  meDuration.ActiveProperties.MinValue := DurationEditHelper.MinValue;
  meDuration.ActiveProperties.MaxValue := DurationEditHelper.MaxValue;
  meDuration.ActiveProperties.ExceptionOnInvalidInput := True;
  case AFormat of
    TdxDurationFormat.Minutes, TdxDurationFormat.EstimatedMinutes:
      meDuration.Text := FDurationEditHelper.GetTextFromValue
        (ADuration.ToMinutes);

    TdxDurationFormat.ElapsedMinutes, TdxDurationFormat.EstimatedElapsedMinutes:
      meDuration.Text := FDurationEditHelper.GetTextFromValue
        (ADuration.ToElapsedMinutes);

    TdxDurationFormat.Hours, TdxDurationFormat.EstimatedHours:
      meDuration.Text := FDurationEditHelper.GetTextFromValue
        (ADuration.ToHours);

    TdxDurationFormat.ElapsedHours, TdxDurationFormat.EstimatedElapsedHours:
      meDuration.Text := FDurationEditHelper.GetTextFromValue
        (ADuration.ToElapsedHours);

    TdxDurationFormat.Days, TdxDurationFormat.Null,
      TdxDurationFormat.EstimatedDays, TdxDurationFormat.EstimatedNull:
      meDuration.Text := FDurationEditHelper.GetTextFromValue(ADuration.ToDays);

    TdxDurationFormat.ElapsedDays, TdxDurationFormat.EstimatedElapsedDays:
      meDuration.Text := FDurationEditHelper.GetTextFromValue
        (ADuration.ToElapsedDays);

    TdxDurationFormat.Weeks, TdxDurationFormat.EstimatedWeeks:
      meDuration.Text := FDurationEditHelper.GetTextFromValue
        (ADuration.ToWeeks);

    TdxDurationFormat.ElapsedWeeks, TdxDurationFormat.EstimatedElapsedWeeks:
      meDuration.Text := FDurationEditHelper.GetTextFromValue
        (ADuration.ToElapsedWeeks);

    TdxDurationFormat.Months, TdxDurationFormat.EstimatedMonths:
      meDuration.Text := FDurationEditHelper.GetTextFromValue
        (ADuration.ToMonths);

    TdxDurationFormat.ElapsedMonths, TdxDurationFormat.EstimatedElapsedMonths:
      meDuration.Text := FDurationEditHelper.GetTextFromValue
        (ADuration.ToElapsedMonths);
  end;
  liEstimated.Checked := IsEstimated(AFormat);
end;

procedure TdxGanttControlTaskInformationDialogForm.InitializePercentEditor;
begin
  FPercentEditHelper := TdxMeasurementUnitEditHelper.Create(muPercent, 5, 0, 0,
    100, muPercent);
  mePercentComplete.ActiveProperties.OnIncrementValue :=
    PercentEditIncrementValueHandler;
  mePercentComplete.ActiveProperties.OnValidate := PercentEditValidateHandler;
  mePercentComplete.ActiveProperties.MinValue := PercentEditHelper.MinValue;
  mePercentComplete.ActiveProperties.MaxValue := PercentEditHelper.MaxValue;
  mePercentComplete.ActiveProperties.ExceptionOnInvalidInput := True;
  mePercentComplete.Text := PercentEditHelper.GetTextFromValue
    (ActualTask.PercentComplete);
end;

procedure TdxGanttControlTaskInformationDialogForm.meDurationPropertiesChange
  (Sender: TObject);
var
  AText, ADescription: string;
  AFormat: TdxDurationFormat;
  AError: Boolean;
  AErrorText: TCaption;
begin
  AText := Trim(meDuration.Text);
  ADescription := TdxDurationEditHelper(DurationEditHelper)
    .GetCheckableCandidate(DurationEditHelper.GetDescriptionFromText(AText));
  if ADescription = '' then
    liEstimated.Checked := False
  else
  begin
    AFormat := GetDurationFormat(ADescription, AError, AErrorText);
    if not AError then
    begin
      liEstimated.Checked := IsEstimated(AFormat);
      liCalendar.Enabled := not IsElapsed(AFormat);
      if not liCalendar.Enabled then
        cmbCalendar.ItemIndex := 0;
    end
    else
    begin
      if AText[Length(AText)] <> muEstimatedTimePostfix[1] then
        liEstimated.Checked := False;
    end;
  end;
end;

procedure TdxGanttControlTaskInformationDialogForm.
  meDurationPropertiesEditValueChanged(Sender: TObject);
var
  AValue: Variant;
  AError: Boolean;
  AErrorText: TCaption;
  AActualDuration: TdxGanttControlDuration;
begin
  if FInitializing then
    Exit;
  DurationEditValidateHandler(nil, AValue, AErrorText, AError);
  if not AError then
  begin
    AActualDuration := TdxGanttControlDuration.Create(GetActualDurationValue,
      GetActualDurationFormat(AError, AErrorText));
    if AActualDuration.ToSeconds <> ConstraintChangesHelper.Duration then
      ActualTask.Duration := AActualDuration.ToString;
  end;
end;

class procedure TdxGanttControlTaskInformationDialogForm.
  InitializeMeasurementUnits;
begin
  muMinuteExtraShort := cxGetResourceString
    (@sdxGanttControlDurationFormatMinuteExtraShort);
  muMinute := cxGetResourceString(@sdxGanttControlDurationFormatMinute);
  muMinutes := cxGetResourceString(@sdxGanttControlDurationFormatMinutes);
  muMinuteShort := cxGetResourceString
    (@sdxGanttControlDurationFormatMinuteShort);
  muMinutesShort := cxGetResourceString
    (@sdxGanttControlDurationFormatMinutesShort);
  muHourExtraShort := cxGetResourceString
    (@sdxGanttControlDurationFormatHourExtraShort);
  muHour := cxGetResourceString(@sdxGanttControlDurationFormatHour);
  muHours := cxGetResourceString(@sdxGanttControlDurationFormatHours);
  muHourShort := cxGetResourceString(@sdxGanttControlDurationFormatHourShort);
  muHoursShort := cxGetResourceString(@sdxGanttControlDurationFormatHoursShort);
  muDayExtraShort := cxGetResourceString
    (@sdxGanttControlDurationFormatDayExtraShort);
  muDay := cxGetResourceString(@sdxGanttControlDurationFormatDay);
  muDays := cxGetResourceString(@sdxGanttControlDurationFormatDays);
  muWeekExtraShort := cxGetResourceString
    (@sdxGanttControlDurationFormatWeekExtraShort);
  muWeek := cxGetResourceString(@sdxGanttControlDurationFormatWeek);
  muWeeks := cxGetResourceString(@sdxGanttControlDurationFormatWeeks);
  muWeekShort := cxGetResourceString(@sdxGanttControlDurationFormatWeekShort);
  muWeeksShort := cxGetResourceString(@sdxGanttControlDurationFormatWeeksShort);
  muMonthExtraShort := cxGetResourceString
    (@sdxGanttControlDurationFormatMonthExtraShort);
  muMonth := cxGetResourceString(@sdxGanttControlDurationFormatMonth);
  muMonths := cxGetResourceString(@sdxGanttControlDurationFormatMonths);
  muMonthShort := cxGetResourceString(@sdxGanttControlDurationFormatMonthShort);
  muMonthsShort := cxGetResourceString
    (@sdxGanttControlDurationFormatMonthsShort);
  muPercent := '%';
  muElapsedTimePrefix := cxGetResourceString
    (@sdxGanttControlDurationFormatElapsedTimePrefix);
  muEstimatedTimePostfix := cxGetResourceString
    (@sdxGanttControlDurationFormatEstimatedTimePostfix);
end;

function TdxGanttControlTaskInformationDialogForm.IsElapsed
  (AFormat: TdxDurationFormat): Boolean;
begin
  Result := AFormat in [TdxDurationFormat.ElapsedMinutes,
    TdxDurationFormat.EstimatedElapsedMinutes, TdxDurationFormat.ElapsedHours,
    TdxDurationFormat.EstimatedElapsedHours, TdxDurationFormat.ElapsedDays,
    TdxDurationFormat.EstimatedElapsedDays, TdxDurationFormat.EstimatedNull,
    TdxDurationFormat.ElapsedWeeks, TdxDurationFormat.EstimatedElapsedWeeks,
    TdxDurationFormat.ElapsedMonths, TdxDurationFormat.EstimatedElapsedMonths];
end;

function TdxGanttControlTaskInformationDialogForm.IsEstimated
  (AFormat: TdxDurationFormat): Boolean;
begin
  Result := AFormat in [TdxDurationFormat.EstimatedMinutes,
    TdxDurationFormat.EstimatedElapsedMinutes, TdxDurationFormat.EstimatedHours,
    TdxDurationFormat.EstimatedElapsedHours, TdxDurationFormat.EstimatedDays,
    TdxDurationFormat.EstimatedElapsedDays, TdxDurationFormat.EstimatedWeeks,
    TdxDurationFormat.EstimatedElapsedWeeks, TdxDurationFormat.EstimatedMonths,
    TdxDurationFormat.EstimatedElapsedMonths];
end;

procedure TdxGanttControlTaskInformationDialogForm.liEstimatedClick
  (Sender: TObject);
var
  AText: string;
  AFormat: TdxDurationFormat;
  AError: Boolean;
  AErrorText: TCaption;
  L, P: Integer;
begin
  AText := Trim(meDuration.Text);
  L := Length(AText);
  if AText <> '' then
  begin
    if liEstimated.Checked then
    begin
      AFormat := GetDurationFormat(DurationEditHelper.GetDescriptionFromText
        (AText), AError, AErrorText);
      if not(AError or IsEstimated(AFormat)) then
        meDuration.Text := meDuration.Text + muEstimatedTimePostfix;
    end
    else
    begin
      P := L + 1;
      while AText[P - 1] = muEstimatedTimePostfix[1] do
      begin
        P := P - 1;
        if P = 1 then
          Break;
      end;
      if P <= L then
      begin
        Delete(AText, P, L - P + 1);
        meDuration.Text := AText;
      end;
    end;
  end;
end;

function TdxGanttControlTaskInformationDialogForm.IsManualScheduleMode: Boolean;
begin
  Result := cmbScheduleMode.ItemIndex = 0;
end;

function TdxGanttControlTaskInformationDialogForm.GetGanttControl
  : TdxCustomGanttControl;
begin
  Result := Owner as TdxCustomGanttControl;
end;

procedure TdxGanttControlTaskInformationDialogForm.DateValidateHandler
  (Sender: TcxDateEdit; AAsStartDate: Boolean);
begin
  if not VarIsNull(Sender.EditValue) and (TimeOf(Sender.Date) = 0) then
    if AAsStartDate then
      Sender.Date := Int(Sender.Date) + 8 / 24
    else
      Sender.Date := Int(Sender.Date) + 17 / 24;
end;

procedure TdxGanttControlTaskInformationDialogForm.
  DurationEditIncrementValueHandler(Sender: TObject; AButton: TcxSpinEditButton;
  var AValue: Variant; var AHandled: Boolean);
begin
  AHandled := DurationEditHelper.IncrementValue(AButton, AValue);
end;

procedure TdxGanttControlTaskInformationDialogForm.DurationEditValidateHandler
  (Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
  var Error: Boolean);
var
  AValue: Variant;
  AFloatValue: Double;
begin
  AValue := GetActualDurationValue;
  if VarIsNull(AValue) then
    Error := True
  else
  begin
    AFloatValue := AValue;
    Error := (CompareValue(AFloatValue, DurationEditHelper.MinValue) < 0) or
      (CompareValue(AFloatValue, DurationEditHelper.MaxValue) > 0);
  end;
  if Error then
    ErrorText := cxGetResourceString
      (@sdxGanttControlMessageInvalidDurationFormat);
end;

procedure TdxGanttControlTaskInformationDialogForm.
  PercentEditIncrementValueHandler(Sender: TObject; AButton: TcxSpinEditButton;
  var AValue: Variant; var AHandled: Boolean);
begin
  AHandled := PercentEditHelper.IncrementValue(AButton, AValue);
end;

procedure TdxGanttControlTaskInformationDialogForm.PercentEditValidateHandler
  (Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
  var Error: Boolean);
var
  AValue: Variant;
  AFloatValue: Double;
begin
  AValue := GetActualPercentValue;
  if VarIsNull(AValue) then
    Error := True
  else
  begin
    AFloatValue := AValue;
    Error := (CompareValue(AFloatValue, PercentEditHelper.MinValue) < 0) or
      (CompareValue(AFloatValue, PercentEditHelper.MaxValue) > 0);
  end;
  if Error then
    ErrorText := cxGetResourceString
      (@sdxGanttControlMessageInvalidPercentageCompletedValue);
end;

procedure TdxGanttControlTaskInformationDialogForm.StoreInitialValues;
begin
  ConstraintChangesHelper.ConstraintTypeIndex := cmbConstraintType.ItemIndex;
  ConstraintChangesHelper.ConstraintDate := deConstraintDate.EditValue;
  ConstraintChangesHelper.Duration := TdxGanttControlDuration.Create
    (ActualTask.Duration).ToSeconds;
  ConstraintChangesHelper.StartDate := deDateStart.EditValue;
  ConstraintChangesHelper.FinishDate := deDateFinish.EditValue;
end;

procedure TdxGanttControlTaskInformationDialogForm.btnDeleteClick
  (Sender: TObject);
begin
  FreeAndNil(FActualTask);
  ModalResult := mrOk;
end;

procedure TdxGanttControlTaskInformationDialogForm.btnOkClick(Sender: TObject);
begin
  ModalResult := (Sender as TcxButton).ModalResult;
end;

procedure TdxGanttControlTaskInformationDialogForm.
  cmbScheduleModePropertiesChange(Sender: TObject);
begin
  CheckScheduleModeDependency;
end;

procedure ShowTaskInformationDialog(AController
  : TdxGanttControlChartViewController; ATask: TdxGanttControlTask);
begin
  ShowTaskInformationDialog(AController.Control, ATask);
end;

procedure ShowTaskInformationDialog(AGanttControl: TdxGanttControlBase;
  ATask: TdxGanttControlTask);
var
  FTaskInformationForm: TdxGanttControlTaskInformationDialogForm;
  AControl: TdxCustomGanttControlAccess;
  ACommand: TdxGanttControlCommand;
begin
  AControl := TdxCustomGanttControlAccess(AGanttControl);
  if not AControl.DoShowTaskInformationDialog(ATask) then
  begin
    if ATask <> nil then
      FTaskInformationForm := dxGanttControlTaskInformationDialogFormClass.
        Create(AControl, ATask)
    else
      FTaskInformationForm := dxGanttControlTaskInformationDialogFormClass.
        Create(AControl, TdxCustomGanttControl(AGanttControl)
        .ViewChart.Controller.FocusedRowIndex);
    try
      cxDialogsMetricsStore.InitDialog(FTaskInformationForm);
      if FTaskInformationForm.ShowModal = mrOk then
      begin
        if ATask <> nil then
          ACommand := TdxGanttControlChangeTaskCommand.Create(AGanttControl,
            ATask, FTaskInformationForm.ActualTask)
        else
          ACommand := TdxViewChartCreateTaskCommand.Create
            (TdxCustomGanttControl(AGanttControl).ViewChart.Controller,
            TdxCustomGanttControl(AGanttControl)
            .ViewChart.Controller.FocusedRowIndex,
            FTaskInformationForm.ActualTask);
        try
          ACommand.Execute;
        finally
          ACommand.Free;
        end;
      end;
      cxDialogsMetricsStore.StoreMetrics(FTaskInformationForm);
    finally
      FTaskInformationForm.Free;
    end;
  end;
end;

end.
