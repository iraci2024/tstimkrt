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

unit dxGanttControlTaskDependencyDialog;

{$I cxVer.inc}
{$I dxGanttControl.inc}

interface

uses
{$IFDEF DELPHIXE2}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, dxForms, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, dxLayoutcxEditAdapters,
  dxLayoutControlAdapters, cxContainer, cxEdit, Menus, StdCtrls, cxButtons,
  dxLayoutContainer, cxSpinEdit,
  dxMeasurementUnitEdit, cxTextEdit, cxMaskEdit, cxDropDownEdit, cxClasses,
  dxLayoutControl,
  dxCore, dxGanttControl, dxGanttControlStrs, dxGanttControlViewChart,
  dxGanttControlTasks,
  dxLayoutLookAndFeels, dxGanttControlCustomClasses, dxGanttControlCommands;

type
  TdxGanttControlTaskDependencyDialogForm = class(TdxForm)
    lcMainGroup_Root: TdxLayoutGroup;
    lcMain: TdxLayoutControl;
    liFromCaption: TdxLayoutLabeledItem;
    liFrom: TdxLayoutLabeledItem;
    liToCaption: TdxLayoutLabeledItem;
    liTo: TdxLayoutLabeledItem;
    liLinkType: TdxLayoutItem;
    cmbLinkType: TcxComboBox;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutGroup2: TdxLayoutGroup;
    AlignmentConstraint1: TdxLayoutAlignmentConstraint;
    meLagEdit: TdxMeasurementUnitEdit;
    liLag: TdxLayoutItem;
    dxLayoutAutoCreatedGroup1: TdxLayoutAutoCreatedGroup;
    dxLayoutEmptySpaceItem1: TdxLayoutEmptySpaceItem;
    btnDelete: TcxButton;
    liBtnDelete: TdxLayoutItem;
    btnOk: TcxButton;
    liBtnOk: TdxLayoutItem;
    btnCancel: TcxButton;
    liBtnCancel: TdxLayoutItem;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    dxLayoutEmptySpaceItem2: TdxLayoutEmptySpaceItem;
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure cmbLinkTypePropertiesChange(Sender: TObject);
  private
    FActualLink: TdxGanttControlTaskPredecessorLink;
    FLagEditHelper: TdxMeasurementUnitEditHelper;
    FLink: TdxGanttControlTaskPredecessorLink;
    FPredecessor: TdxGanttControlTask;
    FTask: TdxGanttControlTask;

    function GetGanttControl: TdxCustomGanttControl;
  protected
    procedure ApplyLocalization; virtual;
    procedure AddPossibleDescriptions; virtual;
    function CheckLinkType(var AErrorText: TCaption): Boolean;
    function GetInitialUnitDescription
      (AFormat: TdxGanttControlTaskPredecessorLagFormat): string; virtual;
    class function GetLagFormat(ADescription: string; var AError: Boolean;
      var AErrorText: TCaption): TdxGanttControlTaskPredecessorLagFormat;
    class function GetLinkLag(AValue: Double;
      ALagFormat: TdxGanttControlTaskPredecessorLagFormat): Integer;
    function GetLinkType(const ATypeStr: string): Integer;
    function GetLinkTypeComboBoxItemIndex
      (AType: TdxGanttControlTaskPredecessorLinkType): Integer;
    procedure Initialize; virtual;
    procedure InitializeLagEditor;
    procedure InitializeLinkTypes;

    class procedure InitializeMeasurementUnits;
    function LinkCanBeDeleted: Boolean;

    procedure LagEditIncrementValueHandler(Sender: TObject;
      AButton: TcxSpinEditButton; var AValue: Variant; var AHandled: Boolean);
    procedure LagEditValidateHandler(Sender: TObject; var DisplayValue: Variant;
      var ErrorText: TCaption; var Error: Boolean);

    property ActualLink: TdxGanttControlTaskPredecessorLink read FActualLink;
    property GanttControl: TdxCustomGanttControl read GetGanttControl;
    property LagEditHelper: TdxMeasurementUnitEditHelper read FLagEditHelper;
    property Link: TdxGanttControlTaskPredecessorLink read FLink;
    property Predecessor: TdxGanttControlTask read FPredecessor;
    property Task: TdxGanttControlTask read FTask;
  public
    constructor Create(AGanttControl: TdxGanttControlBase;
      ALink: TdxGanttControlTaskPredecessorLink); reintroduce; virtual;
    destructor Destroy; override;
  end;

  TdxGanttControlTaskDependencyDialogFormClass = class of
    TdxGanttControlTaskDependencyDialogForm;

var
  dxGanttControlTaskDependencyDialogFormClass
    : TdxGanttControlTaskDependencyDialogFormClass =
    TdxGanttControlTaskDependencyDialogForm;

procedure ShowTaskDependencyDialog(AGanttControl: TdxGanttControlBase;
  ALink: TdxGanttControlTaskPredecessorLink);

implementation

{$R *.dfm}

uses
  Math,
  dxGanttControlCustomDataModel,
  dxGanttControlTaskCommands;

type
  TdxCustomGanttControlAccess = class(TdxCustomGanttControl);
  TdxGanttControlTaskPredecessorLinkAccess = class
    (TdxGanttControlTaskPredecessorLink);

var
  muElapsedTimePrefix, muMinuteExtraShort, muMinute, muMinutes, muMinuteShort,
    muMinutesShort, muHourExtraShort, muHour, muHours, muHourShort,
    muHoursShort, muDayExtraShort, muDay, muDays, muWeekExtraShort, muWeek,
    muWeeks, muWeekShort, muWeeksShort, muMonthExtraShort, muMonth, muMonths,
    muMonthShort, muMonthsShort, muPercent: string;

  ltFF, ltFS, ltSF, ltSS, ltNone: string;

  { TdxGanttControlTaskDependencyDialogForm }

constructor TdxGanttControlTaskDependencyDialogForm.Create
  (AGanttControl: TdxGanttControlBase;
  ALink: TdxGanttControlTaskPredecessorLink);
begin
  inherited Create(AGanttControl);
  FLink := ALink;
  FActualLink := TdxGanttControlTaskPredecessorLink.Create(FLink.Owner);
  FActualLink.Assign(Link);
  FActualLink.ResetValue(TdxGanttTaskPredecessorLinkAssignedValue.&Type);
  FActualLink.ResetValue(TdxGanttTaskPredecessorLinkAssignedValue.LinkLag);
  FActualLink.ResetValue(TdxGanttTaskPredecessorLinkAssignedValue.LagFormat);
  FPredecessor := Link.Task.Owner.GetItemByUID(Link.PredecessorUID);
  FTask := Link.Task;
  Initialize;
end;

destructor TdxGanttControlTaskDependencyDialogForm.Destroy;
begin
  FreeAndNil(FLagEditHelper);
  FreeAndNil(FActualLink);
  inherited Destroy;
end;

procedure TdxGanttControlTaskDependencyDialogForm.FormCloseQuery
  (Sender: TObject; var CanClose: Boolean);
var
  ADisplayValue: Variant;
  AErrorText: TCaption;
  AError: Boolean;
  AActualDescription: string;
begin
  if ModalResult <> mrOk then
    Exit;

  if FActualLink <> nil then
  begin
    if not CheckLinkType(AErrorText) then
    begin
      CanClose := False;
      MessageDlg(AErrorText, mtWarning, [mbOK], 0);
      cmbLinkType.SetFocus;
      Exit;
    end;

    if LinkCanBeDeleted then
      FreeAndNil(FActualLink)
    else
    begin
      LagEditValidateHandler(nil, ADisplayValue, AErrorText, AError);
      CanClose := not AError;
      if not CanClose then
      begin
        MessageDlg(AErrorText, mtWarning, [mbOK], 0);
        meLagEdit.SetFocus;
        Exit;
      end;

      FActualLink.&Type := TdxGanttControlTaskPredecessorLinkType
        (GetLinkType(cmbLinkType.Text));

      AActualDescription :=
        Trim(LagEditHelper.GetDescriptionFromText(meLagEdit.Text));
      if AActualDescription = '' then
        AActualDescription := LagEditHelper.DefaultDescription;
      FActualLink.LagFormat := GetLagFormat(AActualDescription, AError,
        AErrorText);
      if AError then
      begin
        CanClose := False;
        MessageDlg(AErrorText, mtWarning, [mbOK], 0);
        meLagEdit.SetFocus;
        Exit;
      end
      else
        FActualLink.LinkLag :=
          GetLinkLag(LagEditHelper.GetValueFromText(meLagEdit.Text),
          FActualLink.LagFormat);
    end;
  end;

  if CanClose and (FActualLink = nil) then
    CanClose := not GanttControl.OptionsBehavior.ConfirmDelete or
      (MessageDlg(cxGetResourceString(@sdxGanttControlConfirmationDeleteLink),
      mtConfirmation, [mbYes, mbNo], 0) = mrYes);
end;

procedure TdxGanttControlTaskDependencyDialogForm.FormShow(Sender: TObject);
var
  dH, dW: Integer;
begin
  lcMain.HandleNeeded;
  dH := lcMain.OccupiedClientHeight - ClientHeight;
  dW := lcMain.OccupiedClientWidth - ClientWidth;
  Width := Width + dW;
  Height := Height + dH;
  cmbLinkType.SetFocus;
end;

procedure TdxGanttControlTaskDependencyDialogForm.ApplyLocalization;
begin
  InitializeMeasurementUnits;
  Caption := cxGetResourceString(@sdxGanttControlTaskDependencyDialogCaption);
  liFromCaption.CaptionOptions.Text :=
    cxGetResourceString(@sdxGanttControlTaskDependencyDialogFrom);
  liToCaption.CaptionOptions.Text :=
    cxGetResourceString(@sdxGanttControlTaskDependencyDialogTo);
  liLinkType.CaptionOptions.Text :=
    cxGetResourceString(@sdxGanttControlTaskDependencyDialogLinkType);

  InitializeLinkTypes;
  cmbLinkType.Properties.Items.Add(ltFS);
  cmbLinkType.Properties.Items.Add(ltSS);
  cmbLinkType.Properties.Items.Add(ltFF);
  cmbLinkType.Properties.Items.Add(ltSF);
  cmbLinkType.Properties.Items.Add(ltNone);

  liLag.CaptionOptions.Text := cxGetResourceString
    (@sdxGanttControlTaskDependencyDialogLag);
  btnDelete.Caption := cxGetResourceString(@sdxGanttControlDialogDelete);
  btnOk.Caption := cxGetResourceString(@sdxGanttControlDialogOk);
  btnCancel.Caption := cxGetResourceString(@sdxGanttControlDialogCancel);
end;

procedure TdxGanttControlTaskDependencyDialogForm.AddPossibleDescriptions;

  procedure InternalAdd(const ADescription: string);
  begin
    LagEditHelper.AddPossibleDescription(ADescription);
    LagEditHelper.AddPossibleDescription(muElapsedTimePrefix + ADescription);
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
  InternalAdd(muPercent);
end;

function TdxGanttControlTaskDependencyDialogForm.CheckLinkType
  (var AErrorText: TCaption): Boolean;
var
  ALinkType: Integer;
begin
  Result := True;
  ALinkType := GetLinkType(cmbLinkType.Text);
  if Task.Summary and ((ALinkType > -1) and
    (TdxGanttControlTaskPredecessorLinkType(ALinkType)
    in [TdxGanttControlTaskPredecessorLinkType.FF,
    TdxGanttControlTaskPredecessorLinkType.SF])) then
  begin
    Result := False;
    AErrorText := cxGetResourceString
      (@sdxGanttControlMessageInvalidSummaryTaskLink);
  end;
end;

procedure TdxGanttControlTaskDependencyDialogForm.cmbLinkTypePropertiesChange
  (Sender: TObject);
begin
  liLag.Enabled := not LinkCanBeDeleted;
end;

function TdxGanttControlTaskDependencyDialogForm.GetInitialUnitDescription
  (AFormat: TdxGanttControlTaskPredecessorLagFormat): string;
begin
  case AFormat of
    TdxGanttControlTaskPredecessorLagFormat.Minutes:
      Result := muMinuteExtraShort;
    TdxGanttControlTaskPredecessorLagFormat.ElapsedMinutes:
      Result := muElapsedTimePrefix + muMinuteExtraShort;
    TdxGanttControlTaskPredecessorLagFormat.Hours:
      Result := muHourExtraShort;
    TdxGanttControlTaskPredecessorLagFormat.ElapsedHours:
      Result := muElapsedTimePrefix + muHourExtraShort;
    TdxGanttControlTaskPredecessorLagFormat.Days:
      Result := muDayExtraShort;
    TdxGanttControlTaskPredecessorLagFormat.ElapsedDays:
      Result := muElapsedTimePrefix + muDayExtraShort;
    TdxGanttControlTaskPredecessorLagFormat.Weeks:
      Result := muWeekExtraShort;
    TdxGanttControlTaskPredecessorLagFormat.ElapsedWeeks:
      Result := muElapsedTimePrefix + muWeekExtraShort;
    TdxGanttControlTaskPredecessorLagFormat.Months:
      Result := muMonthExtraShort;
    TdxGanttControlTaskPredecessorLagFormat.ElapsedMonths:
      Result := muElapsedTimePrefix + muMonthExtraShort;
    TdxGanttControlTaskPredecessorLagFormat.Percent:
      Result := muPercent;
    TdxGanttControlTaskPredecessorLagFormat.ElapsedPercent:
      Result := muElapsedTimePrefix + muPercent;
  end;
end;

class function TdxGanttControlTaskDependencyDialogForm.GetLagFormat
  (ADescription: string; var AError: Boolean; var AErrorText: TCaption)
  : TdxGanttControlTaskPredecessorLagFormat;

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
  AErrorText :=
    Format(cxGetResourceString(@sdxGanttControlMessageInvalidMeasurementUnit),
    [ADescription]);
  ADescription := AnsiUpperCase(ADescription);
  Result := TdxGanttControlTaskPredecessorLagFormat.Days;
  if CheckMeasureUnit(ADescription, [muDayExtraShort, muDay, muDays]) then
    Result := TdxGanttControlTaskPredecessorLagFormat.Days
  else if CheckMeasureUnit(ADescription, [muHourExtraShort, muHourShort,
    muHoursShort, muHour, muHours]) then
    Result := TdxGanttControlTaskPredecessorLagFormat.Hours
  else if CheckMeasureUnit(ADescription, [muWeekExtraShort, muWeekShort,
    muWeeksShort, muWeek, muWeeks]) then
    Result := TdxGanttControlTaskPredecessorLagFormat.Weeks
  else if CheckMeasureUnit(ADescription, [muMonthExtraShort, muMonthShort,
    muMonthsShort, muMonth, muMonths]) then
    Result := TdxGanttControlTaskPredecessorLagFormat.Months
  else if ADescription = AnsiUpperCase(muPercent) then
    Result := TdxGanttControlTaskPredecessorLagFormat.Percent
  else if CheckMeasureUnit(ADescription, [muMinuteExtraShort, muMinuteShort,
    muMinutesShort, muMinute, muMinutes]) then
    Result := TdxGanttControlTaskPredecessorLagFormat.Minutes
  else

    if CheckMeasureUnit(ADescription, [muElapsedTimePrefix + muDayExtraShort,
    muElapsedTimePrefix + muDay, muElapsedTimePrefix + muDays]) then
    Result := TdxGanttControlTaskPredecessorLagFormat.ElapsedDays
  else if CheckMeasureUnit(ADescription,
    [muElapsedTimePrefix + muHourExtraShort, muElapsedTimePrefix + muHourShort,
    muElapsedTimePrefix + muHoursShort, muElapsedTimePrefix + muHour,
    muElapsedTimePrefix + muHours]) then
    Result := TdxGanttControlTaskPredecessorLagFormat.ElapsedHours
  else if CheckMeasureUnit(ADescription,
    [muElapsedTimePrefix + muWeekExtraShort, muElapsedTimePrefix + muWeekShort,
    muElapsedTimePrefix + muWeeksShort, muElapsedTimePrefix + muWeek,
    muElapsedTimePrefix + muWeeks]) then
    Result := TdxGanttControlTaskPredecessorLagFormat.ElapsedWeeks
  else if CheckMeasureUnit(ADescription,
    [muElapsedTimePrefix + muMonthExtraShort, muElapsedTimePrefix +
    muMonthShort, muElapsedTimePrefix + muMonthsShort,
    muElapsedTimePrefix + muMonth, muElapsedTimePrefix + muMonths]) then
    Result := TdxGanttControlTaskPredecessorLagFormat.ElapsedMonths
  else if ADescription = AnsiUpperCase(muElapsedTimePrefix + muPercent) then
    Result := TdxGanttControlTaskPredecessorLagFormat.ElapsedPercent
  else if CheckMeasureUnit(ADescription,
    [muElapsedTimePrefix + muMinuteExtraShort, muElapsedTimePrefix +
    muMinuteShort, muElapsedTimePrefix + muMinutesShort,
    muElapsedTimePrefix + muMinute, muElapsedTimePrefix + muMinutes]) then
    Result := TdxGanttControlTaskPredecessorLagFormat.ElapsedMinutes
  else
    AError := True;
end;

class function TdxGanttControlTaskDependencyDialogForm.GetLinkLag
  (AValue: Double; ALagFormat: TdxGanttControlTaskPredecessorLagFormat)
  : Integer;
begin
  Result := 0;
  case ALagFormat of
    TdxGanttControlTaskPredecessorLagFormat.Minutes,
      TdxGanttControlTaskPredecessorLagFormat.ElapsedMinutes:
      Result := Round(AValue * 10);
    TdxGanttControlTaskPredecessorLagFormat.Hours,
      TdxGanttControlTaskPredecessorLagFormat.ElapsedHours:
      Result := Round(AValue * 600);
    TdxGanttControlTaskPredecessorLagFormat.Days:
      Result := Round(AValue * 4800);
    TdxGanttControlTaskPredecessorLagFormat.ElapsedDays:
      Result := Round(AValue * 14400);
    TdxGanttControlTaskPredecessorLagFormat.Weeks:
      Result := Round(AValue * 24000);
    TdxGanttControlTaskPredecessorLagFormat.ElapsedWeeks:
      Result := Round(AValue * 100800);
    TdxGanttControlTaskPredecessorLagFormat.Months:
      Result := Round(AValue * 96000);
    TdxGanttControlTaskPredecessorLagFormat.ElapsedMonths:
      Result := Round(AValue * 432000);
    TdxGanttControlTaskPredecessorLagFormat.Percent,
      TdxGanttControlTaskPredecessorLagFormat.ElapsedPercent:
      Result := Round(AValue);
  end;
end;

function TdxGanttControlTaskDependencyDialogForm.GetLinkType(const ATypeStr
  : string): Integer;
begin
  if ATypeStr = ltFF then
    Result := Integer(TdxGanttControlTaskPredecessorLinkType.FF)
  else if ATypeStr = ltFS then
    Result := Integer(TdxGanttControlTaskPredecessorLinkType.FS)
  else if ATypeStr = ltSF then
    Result := Integer(TdxGanttControlTaskPredecessorLinkType.SF)
  else if ATypeStr = ltSS then
    Result := Integer(TdxGanttControlTaskPredecessorLinkType.SS)
  else
    Result := -1;
end;

function TdxGanttControlTaskDependencyDialogForm.GetLinkTypeComboBoxItemIndex
  (AType: TdxGanttControlTaskPredecessorLinkType): Integer;
var
  ATypeStr: string;
  I: Integer;
begin
  Result := 0;
  case AType of
    TdxGanttControlTaskPredecessorLinkType.FF:
      ATypeStr := ltFF;
    TdxGanttControlTaskPredecessorLinkType.FS:
      ATypeStr := ltFS;
    TdxGanttControlTaskPredecessorLinkType.SF:
      ATypeStr := ltSF;
    TdxGanttControlTaskPredecessorLinkType.SS:
      ATypeStr := ltSS;
  else
    ATypeStr := ltNone;
  end;
  for I := 0 to cmbLinkType.Properties.Items.Count - 1 do
    if cmbLinkType.Properties.Items[I] = ATypeStr then
    begin
      Result := I;
      Break;
    end;
end;

procedure TdxGanttControlTaskDependencyDialogForm.Initialize;
begin
  ApplyLocalization;
  BiDiMode := GanttControl.BiDiMode;
  dxLayoutCxLookAndFeel1.LookAndFeel := TdxCustomGanttControlAccess
    (GanttControl).LookAndFeel;
  liFrom.CaptionOptions.Text := Predecessor.Name;
  liTo.CaptionOptions.Text := Task.Name;
  cmbLinkType.ItemIndex := GetLinkTypeComboBoxItemIndex(Link.&Type);
  InitializeLagEditor;
end;

procedure TdxGanttControlTaskDependencyDialogForm.InitializeLagEditor;
var
  ALagFormat: TdxGanttControlTaskPredecessorLagFormat;
begin
  if Link.IsValueAssigned(TdxGanttTaskPredecessorLinkAssignedValue.LagFormat)
  then
    ALagFormat := Link.LagFormat
  else
    ALagFormat := TdxGanttControlTaskPredecessorLagFormat.Days;
  FLagEditHelper := TdxMeasurementUnitEditHelper.Create
    (GetInitialUnitDescription(ALagFormat), 1, 3, MinCurrency, MaxCurrency,
    cxGetResourceString(@sdxGanttControlDurationFormatDayExtraShort));
  AddPossibleDescriptions;
  meLagEdit.ActiveProperties.OnIncrementValue := LagEditIncrementValueHandler;
  meLagEdit.ActiveProperties.OnValidate := LagEditValidateHandler;
  meLagEdit.ActiveProperties.MinValue := LagEditHelper.MinValue;
  meLagEdit.ActiveProperties.MaxValue := LagEditHelper.MaxValue;
  meLagEdit.ActiveProperties.ExceptionOnInvalidInput := True;
  case ALagFormat of
    TdxGanttControlTaskPredecessorLagFormat.Minutes,
      TdxGanttControlTaskPredecessorLagFormat.ElapsedMinutes:
      meLagEdit.Text := LagEditHelper.GetTextFromValue(Link.LinkLag / 10);
    TdxGanttControlTaskPredecessorLagFormat.Hours,
      TdxGanttControlTaskPredecessorLagFormat.ElapsedHours:
      meLagEdit.Text := LagEditHelper.GetTextFromValue(Link.LinkLag / 600);
    TdxGanttControlTaskPredecessorLagFormat.Days:
      meLagEdit.Text := LagEditHelper.GetTextFromValue(Link.LinkLag / 4800);
    TdxGanttControlTaskPredecessorLagFormat.ElapsedDays:
      meLagEdit.Text := LagEditHelper.GetTextFromValue(Link.LinkLag / 14400);
    TdxGanttControlTaskPredecessorLagFormat.Weeks:
      meLagEdit.Text := LagEditHelper.GetTextFromValue(Link.LinkLag / 24000);
    TdxGanttControlTaskPredecessorLagFormat.ElapsedWeeks:
      meLagEdit.Text := LagEditHelper.GetTextFromValue(Link.LinkLag / 100800);
    TdxGanttControlTaskPredecessorLagFormat.Months:
      meLagEdit.Text := LagEditHelper.GetTextFromValue(Link.LinkLag / 96000);
    TdxGanttControlTaskPredecessorLagFormat.ElapsedMonths:
      meLagEdit.Text := LagEditHelper.GetTextFromValue(Link.LinkLag / 432000);
    TdxGanttControlTaskPredecessorLagFormat.Percent,
      TdxGanttControlTaskPredecessorLagFormat.ElapsedPercent:
      meLagEdit.Text := LagEditHelper.GetTextFromValue(Link.LinkLag);
  end;
end;

procedure TdxGanttControlTaskDependencyDialogForm.InitializeLinkTypes;
begin
  ltFF := cxGetResourceString(@sdxGanttControlTaskDependencyDialogLinkTypeFF);
  ltFS := cxGetResourceString(@sdxGanttControlTaskDependencyDialogLinkTypeFS);
  ltSF := cxGetResourceString(@sdxGanttControlTaskDependencyDialogLinkTypeSF);
  ltSS := cxGetResourceString(@sdxGanttControlTaskDependencyDialogLinkTypeSS);
  ltNone := cxGetResourceString
    (@sdxGanttControlTaskDependencyDialogLinkTypeNone);
end;

class procedure TdxGanttControlTaskDependencyDialogForm.
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
end;

function TdxGanttControlTaskDependencyDialogForm.GetGanttControl
  : TdxCustomGanttControl;
begin
  Result := Owner as TdxCustomGanttControl;
end;

procedure TdxGanttControlTaskDependencyDialogForm.LagEditIncrementValueHandler
  (Sender: TObject; AButton: TcxSpinEditButton; var AValue: Variant;
  var AHandled: Boolean);
begin
  AHandled := LagEditHelper.IncrementValue(AButton, AValue);
end;

procedure TdxGanttControlTaskDependencyDialogForm.LagEditValidateHandler
  (Sender: TObject; var DisplayValue: Variant; var ErrorText: TCaption;
  var Error: Boolean);
var
  AValue: Variant;
  AFloatValue: Double;
begin
  AValue := LagEditHelper.GetValueFromText(meLagEdit.Text, False);
  if VarIsNull(AValue) then
    Error := True
  else
  begin
    AFloatValue := AValue;
    Error := (CompareValue(AFloatValue, LagEditHelper.MinValue) < 0) or
      (CompareValue(AFloatValue, LagEditHelper.MaxValue) > 0);
  end;
  if Error then
    ErrorText := cxGetResourceString(@sdxGanttControlMessageInvalidLagValue);
end;

function TdxGanttControlTaskDependencyDialogForm.LinkCanBeDeleted: Boolean;
begin
  Result := GetLinkType(cmbLinkType.Text) = -1;
end;

procedure TdxGanttControlTaskDependencyDialogForm.btnDeleteClick
  (Sender: TObject);
begin
  FreeAndNil(FActualLink);
  ModalResult := mrOk;
end;

procedure TdxGanttControlTaskDependencyDialogForm.btnOkClick(Sender: TObject);
begin
  ModalResult := (Sender as TcxButton).ModalResult;
end;

procedure ShowTaskDependencyDialog(AGanttControl: TdxGanttControlBase;
  ALink: TdxGanttControlTaskPredecessorLink);
var
  FTaskDependencyForm: TdxGanttControlTaskDependencyDialogForm;
  AGantt: TdxCustomGanttControlAccess;
begin
  AGantt := TdxCustomGanttControlAccess(AGanttControl);
  if not AGantt.DoShowTaskDependencyDialog(ALink) then
  begin
    FTaskDependencyForm := dxGanttControlTaskDependencyDialogFormClass.Create
      (AGantt, ALink);
    try
      cxDialogsMetricsStore.InitDialog(FTaskDependencyForm);
      if FTaskDependencyForm.ShowModal = mrOk then
        with TdxGanttControlChangeTaskPredecessorCommand.Create(AGantt, ALink,
          FTaskDependencyForm.ActualLink) do
          try
            Execute;
          finally
            Free;
          end;
      cxDialogsMetricsStore.StoreMetrics(FTaskDependencyForm);
    finally
      FTaskDependencyForm.Free;
    end;
  end;
end;

end.
