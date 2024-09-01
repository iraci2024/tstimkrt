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

unit dxGanttControlAssignments;

{$I cxVer.inc}
{$I dxGanttControl.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes, System.Contnrs,
{$ENDIF}
  SysUtils, Generics.Defaults, Generics.Collections, Classes,
  dxCore, dxCoreClasses,
  dxGanttControlCustomClasses,
  dxGanttControlCustomDataModel,
  dxGanttControlExtendedAttributes;

type
  { TdxGanttControlAssignmentBaseline }

  TdxGanttAssignmentBaselineAssignedValue = (Start, Finish);
  TdxGanttAssignmentBaselineAssignedValues = set of
    TdxGanttAssignmentBaselineAssignedValue;

  TdxGanttControlAssignmentBaseline = class(TdxGanttControlCustomBaseline)
  strict private
    FAssignedValues: TdxGanttAssignmentBaselineAssignedValues;

    FFinish: TDateTime;
    FStart: TDateTime;
    FTimephasedDataItems: TdxGanttControlTimephasedDataItems;

    procedure SetFinish(const Value: TDateTime);
    procedure SetStart(const Value: TDateTime);
    procedure SetTimephasedDataItems(const Value
      : TdxGanttControlTimephasedDataItems);
  protected
    procedure DoReset; override;
  public
    constructor Create(AOwner: TdxGanttControlModelElement); override;
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;

{$REGION 'for internal use'}
    function IsValueAssigned(const AValue
      : TdxGanttAssignmentBaselineAssignedValue): Boolean; overload;
    procedure ResetValue(const AValue
      : TdxGanttAssignmentBaselineAssignedValue); overload;
{$ENDREGION 'for internal use'}
    property Finish: TDateTime read FFinish write SetFinish;
    property Start: TDateTime read FStart write SetStart;
    property TimephasedDataItems: TdxGanttControlTimephasedDataItems
      read FTimephasedDataItems write SetTimephasedDataItems;
  end;

  { TdxGanttControlAssignmentBaselines }

  TdxGanttControlAssignmentBaselines = class(TdxGanttControlCustomBaselines)
  strict private
    function GetItem(Index: Integer): TdxGanttControlAssignmentBaseline; inline;
  protected
    function CreateItem: TdxGanttControlModelElementListItem; override;
  public
    function Append: TdxGanttControlAssignmentBaseline; reintroduce;
    property Items[Index: Integer]: TdxGanttControlAssignmentBaseline
      read GetItem; default;
  end;

  { TdxGanttControlAssignment }

  TdxGanttAssignmentAssignedValue = (GUID, TaskUID, ResourceUID,
    PercentWorkComplete, ActualCost, ActualFinish, ActualOvertimeCost,
    ActualOvertimeWork, ActualStart, ActualWork, ACWP, Confirmed, Cost,
    CostRateTable, CostVariance, CV, Delay, Finish, FinishVariance, Hyperlink,
    HyperlinkAddress, HyperlinkSubAddress, WorkVariance, HasFixedRateUnits,
    FixedMaterial, LevelingDelay, LevelingDelayFormat, LinkedFields, Milestone,
    Notes, OverAllocated, OvertimeCost, OvertimeWork, RegularWork,
    RemainingCost, RemainingOvertimeCost, RemainingOvertimeWork, RemainingWork,
    ResponsePending, Start, Stop, Resume, StartVariance, Summary, SV, Units,
    UpdateNeeded, VAC, Work, WorkContour, BCWS, BCWP, BookingType,
    ActualWorkProtected, ActualOvertimeWorkProtected, Created, AssnOwner,
    AssnOwnerGuid, BudgetCost, BudgetWork, NoteContainsObjects, RateScale);

  TdxGanttAssignmentAssignedValues = set of TdxGanttAssignmentAssignedValue;

  TdxGanttControlAssignment = class(TdxGanttControlModelUIDElement)
  strict private
    FAssignedValues: TdxGanttAssignmentAssignedValues;
    FGUID: string;
    FTaskUID: Integer;
    FResourceUID: Integer;
    FPercentWorkComplete: Integer;
    FActualCost: Double;
    FActualFinish: TDateTime;
    FActualOvertimeCost: Double;
    FActualOvertimeWork: string;
    FActualStart: TDateTime;
    FActualWork: string;
    FACWP: Double;
    FConfirmed: Boolean;
    FCost: Double;
    FCostRateTable: TdxGanttControlCostRateTable;
    FCostVariance: Double;
    FCV: Double;
    FDelay: Integer;
    FFinish: TDateTime;
    FFinishVariance: Integer;
    FHyperlink: string;
    FHyperlinkAddress: string;
    FHyperlinkSubAddress: string;
    FWorkVariance: Double;
    FHasFixedRateUnits: Boolean;
    FFixedMaterial: Boolean;
    FLevelingDelay: Integer;
    FLevelingDelayFormat: TdxDurationFormat;
    FLinkedFields: Boolean;
    FMilestone: Boolean;
    FNotes: string;
    FOverAllocated: Boolean;
    FOvertimeCost: Double;
    FOvertimeWork: string;
    FRegularWork: string;
    FRemainingCost: Double;
    FRemainingOvertimeCost: Double;
    FRemainingOvertimeWork: string;
    FRemainingWork: string;
    FResponsePending: Boolean;
    FStart: TDateTime;
    FStop: TDateTime;
    FResume: TDateTime;
    FStartVariance: Integer;
    FSummary: Boolean;
    FSV: Double;
    FUnits: Double;
    FUpdateNeeded: Boolean;
    FVAC: Double;
    FWork: string;
    FWorkContour: Integer;
    FBCWS: Double;
    FBCWP: Double;
    FBookingType: TdxGanttControlBookingType;
    FActualWorkProtected: string;
    FActualOvertimeWorkProtected: string;
    FCreated: TDateTime;
    FAssnOwner: string;
    FAssnOwnerGuid: string;
    FBudgetCost: Double;
    FBudgetWork: string;
    FNoteContainsObjects: Boolean;
    FRateScale: Integer;

    FBaselines: TdxGanttControlAssignmentBaselines;
    FEnterpriseExtendedAttributes: TdxGanttControlEnterpriseExtendedAttributes;
    FExtendedAttributes: TdxGanttControlExtendedAttributeReferences;
    FTimephasedDataItems: TdxGanttControlTimephasedDataItems;

    procedure SetActualCost(const Value: Double);
    procedure SetActualFinish(const Value: TDateTime);
    procedure SetActualOvertimeCost(const Value: Double);
    procedure SetActualOvertimeWork(const Value: string);
    procedure SetActualOvertimeWorkProtected(const Value: string);
    procedure SetActualStart(const Value: TDateTime);
    procedure SetActualWork(const Value: string);
    procedure SetActualWorkProtected(const Value: string);
    procedure SetACWP(const Value: Double);
    procedure SetAssnOwner(const Value: string);
    procedure SetAssnOwnerGuid(const Value: string);
    procedure SetBaselines(const Value: TdxGanttControlAssignmentBaselines);
    procedure SetBCWP(const Value: Double);
    procedure SetBCWS(const Value: Double);
    procedure SetBookingType(const Value: TdxGanttControlBookingType);
    procedure SetBudgetCost(const Value: Double);
    procedure SetBudgetWork(const Value: string);
    procedure SetConfirmed(const Value: Boolean);
    procedure SetCost(const Value: Double);
    procedure SetCostRateTable(const Value: TdxGanttControlCostRateTable);
    procedure SetCostVariance(const Value: Double);
    procedure SetCreated(const Value: TDateTime);
    procedure SetCV(const Value: Double);
    procedure SetDelay(const Value: Integer);
    procedure SetEnterpriseExtendedAttributes(const Value
      : TdxGanttControlEnterpriseExtendedAttributes);
    procedure SetExtendedAttributes(const Value
      : TdxGanttControlExtendedAttributeReferences);
    procedure SetFinish(const Value: TDateTime);
    procedure SetFinishVariance(const Value: Integer);
    procedure SetFixedMaterial(const Value: Boolean);
    procedure SetGUID(const Value: string);
    procedure SetHasFixedRateUnits(const Value: Boolean);
    procedure SetHyperlink(const Value: string);
    procedure SetHyperlinkAddress(const Value: string);
    procedure SetHyperlinkSubAddress(const Value: string);
    procedure SetLevelingDelay(const Value: Integer);
    procedure SetLevelingDelayFormat(const Value: TdxDurationFormat);
    procedure SetLinkedFields(const Value: Boolean);
    procedure SetMilestone(const Value: Boolean);
    procedure SetNoteContainsObjects(const Value: Boolean);
    procedure SetNotes(const Value: string);
    procedure SetOverAllocated(const Value: Boolean);
    procedure SetOvertimeCost(const Value: Double);
    procedure SetOvertimeWork(const Value: string);
    procedure SetPercentWorkComplete(const Value: Integer);
    procedure SetRateScale(const Value: Integer);
    procedure SetRegularWork(const Value: string);
    procedure SetRemainingCost(const Value: Double);
    procedure SetRemainingOvertimeCost(const Value: Double);
    procedure SetRemainingOvertimeWork(const Value: string);
    procedure SetRemainingWork(const Value: string);
    procedure SetResourceUID(const Value: Integer);
    procedure SetResponsePending(const Value: Boolean);
    procedure SetResume(const Value: TDateTime);
    procedure SetStart(const Value: TDateTime);
    procedure SetStartVariance(const Value: Integer);
    procedure SetStop(const Value: TDateTime);
    procedure SetSummary(const Value: Boolean);
    procedure SetSV(const Value: Double);
    procedure SetTaskUID(const Value: Integer);
    procedure SetTimephasedDataItems(const Value
      : TdxGanttControlTimephasedDataItems);
    procedure SetUnits(const Value: Double);
    procedure SetUpdateNeeded(const Value: Boolean);
    procedure SetVAC(const Value: Double);
    procedure SetWork(const Value: string);
    procedure SetWorkContour(const Value: Integer);
    procedure SetWorkVariance(const Value: Double);
  protected
    procedure DoReset; override;
  public
    constructor Create(AOwner: TdxGanttControlModelElement); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

{$REGION 'for internal use'}
    function IsValueAssigned(const AValue
      : TdxGanttAssignmentAssignedValue): Boolean;
    procedure ResetValue(const AValue
      : TdxGanttAssignmentAssignedValue); overload;
{$ENDREGION 'for internal use'}
    property TaskUID: Integer read FTaskUID write SetTaskUID;
    property ResourceUID: Integer read FResourceUID write SetResourceUID;

{$REGION 'for internal use'}
    property GUID: string read FGUID write SetGUID;
    property PercentWorkComplete: Integer read FPercentWorkComplete
      write SetPercentWorkComplete;
    property ActualCost: Double read FActualCost write SetActualCost;
    property ActualFinish: TDateTime read FActualFinish write SetActualFinish;
    property ActualOvertimeCost: Double read FActualOvertimeCost
      write SetActualOvertimeCost;
    property ActualOvertimeWork: string read FActualOvertimeWork
      write SetActualOvertimeWork;
    property ActualStart: TDateTime read FActualStart write SetActualStart;
    property ActualWork: string read FActualWork write SetActualWork;
    property ACWP: Double read FACWP write SetACWP;
    property Confirmed: Boolean read FConfirmed write SetConfirmed;
    property Cost: Double read FCost write SetCost;
    property CostRateTable: TdxGanttControlCostRateTable read FCostRateTable
      write SetCostRateTable;
    property CostVariance: Double read FCostVariance write SetCostVariance;
    property CV: Double read FCV write SetCV;
    property Delay: Integer read FDelay write SetDelay;
    property Finish: TDateTime read FFinish write SetFinish;
    property FinishVariance: Integer read FFinishVariance
      write SetFinishVariance;
    property Hyperlink: string read FHyperlink write SetHyperlink;
    property HyperlinkAddress: string read FHyperlinkAddress
      write SetHyperlinkAddress;
    property HyperlinkSubAddress: string read FHyperlinkSubAddress
      write SetHyperlinkSubAddress;
    property WorkVariance: Double read FWorkVariance write SetWorkVariance;
    property HasFixedRateUnits: Boolean read FHasFixedRateUnits
      write SetHasFixedRateUnits;
    property FixedMaterial: Boolean read FFixedMaterial write SetFixedMaterial;
    property LevelingDelay: Integer read FLevelingDelay write SetLevelingDelay;
    property LevelingDelayFormat: TdxDurationFormat read FLevelingDelayFormat
      write SetLevelingDelayFormat;
    property LinkedFields: Boolean read FLinkedFields write SetLinkedFields;
    property Milestone: Boolean read FMilestone write SetMilestone;
    property Notes: string read FNotes write SetNotes;
    property OverAllocated: Boolean read FOverAllocated write SetOverAllocated;
    property OvertimeCost: Double read FOvertimeCost write SetOvertimeCost;
    property OvertimeWork: string read FOvertimeWork write SetOvertimeWork;
    property RegularWork: string read FRegularWork write SetRegularWork;
    property RemainingCost: Double read FRemainingCost write SetRemainingCost;
    property RemainingOvertimeCost: Double read FRemainingOvertimeCost
      write SetRemainingOvertimeCost;
    property RemainingOvertimeWork: string read FRemainingOvertimeWork
      write SetRemainingOvertimeWork;
    property RemainingWork: string read FRemainingWork write SetRemainingWork;
    property ResponsePending: Boolean read FResponsePending
      write SetResponsePending;
    property Start: TDateTime read FStart write SetStart;
    property Stop: TDateTime read FStop write SetStop;
    property Resume: TDateTime read FResume write SetResume;
    property StartVariance: Integer read FStartVariance write SetStartVariance;
    property Summary: Boolean read FSummary write SetSummary;
    property SV: Double read FSV write SetSV;
    property Units: Double read FUnits write SetUnits;
    property UpdateNeeded: Boolean read FUpdateNeeded write SetUpdateNeeded;
    property VAC: Double read FVAC write SetVAC;
    property Work: string read FWork write SetWork;
    property WorkContour: Integer read FWorkContour write SetWorkContour;
    property BCWS: Double read FBCWS write SetBCWS;
    property BCWP: Double read FBCWP write SetBCWP;
    property BookingType: TdxGanttControlBookingType read FBookingType
      write SetBookingType;
    property ActualWorkProtected: string read FActualWorkProtected
      write SetActualWorkProtected;
    property ActualOvertimeWorkProtected: string
      read FActualOvertimeWorkProtected write SetActualOvertimeWorkProtected;
    property Created: TDateTime read FCreated write SetCreated;
    property AssnOwner: string read FAssnOwner write SetAssnOwner;
    property AssnOwnerGuid: string read FAssnOwnerGuid write SetAssnOwnerGuid;
    property BudgetCost: Double read FBudgetCost write SetBudgetCost;
    property BudgetWork: string read FBudgetWork write SetBudgetWork;
    property NoteContainsObjects: Boolean read FNoteContainsObjects
      write SetNoteContainsObjects;
    property RateScale: Integer read FRateScale write SetRateScale;

    property Baselines: TdxGanttControlAssignmentBaselines read FBaselines
      write SetBaselines;
    property EnterpriseExtendedAttributes
      : TdxGanttControlEnterpriseExtendedAttributes
      read FEnterpriseExtendedAttributes write SetEnterpriseExtendedAttributes;
    property ExtendedAttributes: TdxGanttControlExtendedAttributeReferences
      read FExtendedAttributes write SetExtendedAttributes;
    property TimephasedDataItems: TdxGanttControlTimephasedDataItems
      read FTimephasedDataItems write SetTimephasedDataItems;
{$ENDREGION 'for internal use'}
  end;

  { TdxGanttControlAssignments }

  TdxGanttControlAssignments = class(TdxGanttControlModelUIDElementList)
  strict private
    function GetItem(Index: Integer): TdxGanttControlAssignment; inline;
  protected
    function CreateItem: TdxGanttControlModelElementListItem; override;
    procedure DoItemChanged
      (AItem: TdxGanttControlModelElementListItem); override;
  public
    function Append: TdxGanttControlAssignment;
    procedure Clear;
    procedure Remove(AItem: TdxGanttControlAssignment);

    property Items[Index: Integer]: TdxGanttControlAssignment
      read GetItem; default;
  end;

implementation

uses
  Math, RTLConsts,
  dxGanttControlDataModel;

type
  TdxGanttControlDataModelAccess = class(TdxGanttControlDataModel);

  { TdxGanttControlAssignmentBaseline }

constructor TdxGanttControlAssignmentBaseline.Create
  (AOwner: TdxGanttControlModelElement);
begin
  inherited Create(AOwner);
  FTimephasedDataItems := TdxGanttControlTimephasedDataItems.Create(Self);
end;

destructor TdxGanttControlAssignmentBaseline.Destroy;
begin
  FreeAndNil(FTimephasedDataItems);
  inherited Destroy;
end;

function TdxGanttControlAssignmentBaseline.IsValueAssigned
  (const AValue: TdxGanttAssignmentBaselineAssignedValue): Boolean;
begin
  Result := AValue in FAssignedValues;
end;

procedure TdxGanttControlAssignmentBaseline.ResetValue
  (const AValue: TdxGanttAssignmentBaselineAssignedValue);
begin
  if IsValueAssigned(AValue) then
  begin
    Exclude(FAssignedValues, AValue);
    Changed;
  end
end;

procedure TdxGanttControlAssignmentBaseline.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlAssignmentBaseline;
begin
  BeginUpdate;
  try
    if Source is TdxGanttControlAssignmentBaseline then
    begin
      ASource := TdxGanttControlAssignmentBaseline(Source);
      Finish := ASource.Finish;
      Start := ASource.Start;
      TimephasedDataItems := ASource.TimephasedDataItems;
    end;
    inherited Assign(Source);
  finally
    EndUpdate;
  end;
end;

procedure TdxGanttControlAssignmentBaseline.DoReset;
begin
  inherited DoReset;
  FTimephasedDataItems.Reset;
  FAssignedValues := [];
end;

procedure TdxGanttControlAssignmentBaseline.SetFinish(const Value: TDateTime);
begin
  if (Finish <> Value) or not IsValueAssigned
    (TdxGanttAssignmentBaselineAssignedValue.Finish) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentBaselineAssignedValue.Finish);
    FFinish := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignmentBaseline.SetStart(const Value: TDateTime);
begin
  if (Start <> Value) or not IsValueAssigned
    (TdxGanttAssignmentBaselineAssignedValue.Start) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentBaselineAssignedValue.Start);
    FStart := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignmentBaseline.SetTimephasedDataItems
  (const Value: TdxGanttControlTimephasedDataItems);
begin
  FTimephasedDataItems.Assign(Value);
end;

{ TdxGanttControlAssignmentBaselines }

function TdxGanttControlAssignmentBaselines.Append
  : TdxGanttControlAssignmentBaseline;
begin
  Result := TdxGanttControlAssignmentBaseline(inherited Append);
end;

function TdxGanttControlAssignmentBaselines.CreateItem
  : TdxGanttControlModelElementListItem;
begin
  Result := TdxGanttControlAssignmentBaseline.Create(Self);
end;

function TdxGanttControlAssignmentBaselines.GetItem(Index: Integer)
  : TdxGanttControlAssignmentBaseline;
begin
  Result := TdxGanttControlAssignmentBaseline(inherited Items[Index]);
end;

{ TdxGanttControlAssignment }

constructor TdxGanttControlAssignment.Create
  (AOwner: TdxGanttControlModelElement);
begin
  inherited Create(AOwner);
  FBaselines := TdxGanttControlAssignmentBaselines.Create(Self);
  FEnterpriseExtendedAttributes := TdxGanttControlEnterpriseExtendedAttributes.
    Create(Self);
  FExtendedAttributes := TdxGanttControlExtendedAttributeReferences.
    Create(Self);
  FTimephasedDataItems := TdxGanttControlTimephasedDataItems.Create(Self);
end;

destructor TdxGanttControlAssignment.Destroy;
begin
  FreeAndNil(FBaselines);
  FreeAndNil(FEnterpriseExtendedAttributes);
  FreeAndNil(FExtendedAttributes);
  FreeAndNil(FTimephasedDataItems);
  inherited Destroy;
end;

procedure TdxGanttControlAssignment.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlAssignment;
begin
  BeginUpdate;
  try
    if Source is TdxGanttControlAssignment then
    begin
      ASource := TdxGanttControlAssignment(Source);
      GUID := ASource.GUID;
      TaskUID := ASource.TaskUID;
      ResourceUID := ASource.ResourceUID;
      PercentWorkComplete := ASource.PercentWorkComplete;
      ActualCost := ASource.ActualCost;
      ActualFinish := ASource.ActualFinish;
      ActualOvertimeCost := ASource.ActualOvertimeCost;
      ActualOvertimeWork := ASource.ActualOvertimeWork;
      ActualStart := ASource.ActualStart;
      ActualWork := ASource.ActualWork;
      ACWP := ASource.ACWP;
      Confirmed := ASource.Confirmed;
      Cost := ASource.Cost;
      CostRateTable := ASource.CostRateTable;
      CostVariance := ASource.CostVariance;
      CV := ASource.CV;
      Delay := ASource.Delay;
      Finish := ASource.Finish;
      FinishVariance := ASource.FinishVariance;
      Hyperlink := ASource.Hyperlink;
      HyperlinkAddress := ASource.HyperlinkAddress;
      HyperlinkSubAddress := ASource.HyperlinkSubAddress;
      WorkVariance := ASource.WorkVariance;
      HasFixedRateUnits := ASource.HasFixedRateUnits;
      FixedMaterial := ASource.FixedMaterial;
      LevelingDelay := ASource.LevelingDelay;
      LevelingDelayFormat := ASource.LevelingDelayFormat;
      LinkedFields := ASource.LinkedFields;
      Milestone := ASource.Milestone;
      Notes := ASource.Notes;
      OverAllocated := ASource.OverAllocated;
      OvertimeCost := ASource.OvertimeCost;
      OvertimeWork := ASource.OvertimeWork;
      RegularWork := ASource.RegularWork;
      RemainingCost := ASource.RemainingCost;
      RemainingOvertimeCost := ASource.RemainingOvertimeCost;
      RemainingOvertimeWork := ASource.RemainingOvertimeWork;
      RemainingWork := ASource.RemainingWork;
      ResponsePending := ASource.ResponsePending;
      Start := ASource.Start;
      Stop := ASource.Stop;
      Resume := ASource.Resume;
      StartVariance := ASource.StartVariance;
      Summary := ASource.Summary;
      SV := ASource.SV;
      Units := ASource.Units;
      UpdateNeeded := ASource.UpdateNeeded;
      VAC := ASource.VAC;
      Work := ASource.Work;
      WorkContour := ASource.WorkContour;
      BCWS := ASource.BCWS;
      BCWP := ASource.BCWP;
      BookingType := ASource.BookingType;
      ActualWorkProtected := ASource.ActualWorkProtected;
      ActualOvertimeWorkProtected := ASource.ActualOvertimeWorkProtected;
      Created := ASource.Created;
      AssnOwner := ASource.AssnOwner;
      AssnOwnerGuid := ASource.AssnOwnerGuid;
      BudgetCost := ASource.BudgetCost;
      BudgetWork := ASource.BudgetWork;
      NoteContainsObjects := ASource.NoteContainsObjects;
      Baselines := ASource.Baselines;
      EnterpriseExtendedAttributes := ASource.EnterpriseExtendedAttributes;
      ExtendedAttributes := ASource.ExtendedAttributes;
      TimephasedDataItems := ASource.TimephasedDataItems;
      FAssignedValues := ASource.FAssignedValues;
    end;
    inherited Assign(Source);
  finally
    EndUpdate;
  end;
end;

procedure TdxGanttControlAssignment.DoReset;
begin
  inherited DoReset;
  FAssignedValues := [];
  FBaselines.Reset;
  FEnterpriseExtendedAttributes.Reset;
  FExtendedAttributes.Reset;
  FTimephasedDataItems.Reset;
end;

function TdxGanttControlAssignment.IsValueAssigned(const AValue
  : TdxGanttAssignmentAssignedValue): Boolean;
begin
  Result := AValue in FAssignedValues;
end;

procedure TdxGanttControlAssignment.ResetValue(const AValue
  : TdxGanttAssignmentAssignedValue);
begin
  if IsValueAssigned(AValue) then
  begin
    Exclude(FAssignedValues, AValue);
    Changed;
  end
end;

procedure TdxGanttControlAssignment.SetActualCost(const Value: Double);
begin
  if not(SameValue(ActualCost, Value) and
    IsValueAssigned(TdxGanttAssignmentAssignedValue.ActualCost)) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.ActualCost);
    FActualCost := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetActualFinish(const Value: TDateTime);
begin
  if (ActualFinish <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.ActualFinish) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.ActualFinish);
    FActualFinish := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetActualOvertimeCost(const Value: Double);
begin
  if not(SameValue(ActualOvertimeCost, Value) and
    IsValueAssigned(TdxGanttAssignmentAssignedValue.ActualOvertimeCost)) then
  begin
    Include(FAssignedValues,
      TdxGanttAssignmentAssignedValue.ActualOvertimeCost);
    FActualOvertimeCost := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetActualOvertimeWork(const Value: string);
begin
  if (ActualOvertimeWork <> Value) or
    not IsValueAssigned(TdxGanttAssignmentAssignedValue.ActualOvertimeWork) then
  begin
    Include(FAssignedValues,
      TdxGanttAssignmentAssignedValue.ActualOvertimeWork);
    FActualOvertimeWork := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetActualOvertimeWorkProtected
  (const Value: string);
begin
  if (ActualOvertimeWorkProtected <> Value) or
    not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.ActualOvertimeWorkProtected) then
  begin
    Include(FAssignedValues,
      TdxGanttAssignmentAssignedValue.ActualOvertimeWorkProtected);
    FActualOvertimeWorkProtected := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetActualStart(const Value: TDateTime);
begin
  if (ActualStart <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.ActualStart) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.ActualStart);
    FActualStart := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetActualWork(const Value: string);
begin
  if (ActualWork <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.ActualWork) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.ActualWork);
    FActualWork := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetActualWorkProtected(const Value: string);
begin
  if (ActualWorkProtected <> Value) or
    not IsValueAssigned(TdxGanttAssignmentAssignedValue.ActualWorkProtected)
  then
  begin
    Include(FAssignedValues,
      TdxGanttAssignmentAssignedValue.ActualWorkProtected);
    FActualWorkProtected := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetACWP(const Value: Double);
begin
  if not(SameValue(ACWP, Value) and
    IsValueAssigned(TdxGanttAssignmentAssignedValue.ACWP)) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.ACWP);
    FACWP := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetAssnOwner(const Value: string);
begin
  if (AssnOwner <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.AssnOwner) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.AssnOwner);
    FAssnOwner := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetAssnOwnerGuid(const Value: string);
begin
  if (AssnOwnerGuid <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.AssnOwnerGuid) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.AssnOwnerGuid);
    FAssnOwnerGuid := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetBaselines(const Value
  : TdxGanttControlAssignmentBaselines);
begin
  FBaselines.Assign(Value);
end;

procedure TdxGanttControlAssignment.SetBCWP(const Value: Double);
begin
  if not(SameValue(BCWP, Value) and
    IsValueAssigned(TdxGanttAssignmentAssignedValue.BCWP)) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.BCWP);
    FBCWP := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetBCWS(const Value: Double);
begin
  if not(SameValue(BCWS, Value) and
    IsValueAssigned(TdxGanttAssignmentAssignedValue.BCWS)) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.BCWS);
    FBCWS := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetBookingType
  (const Value: TdxGanttControlBookingType);
begin
  if (BookingType <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.BookingType) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.BookingType);
    FBookingType := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetBudgetCost(const Value: Double);
begin
  if not(SameValue(BudgetCost, Value) and
    IsValueAssigned(TdxGanttAssignmentAssignedValue.BudgetCost)) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.BudgetCost);
    FBudgetCost := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetBudgetWork(const Value: string);
begin
  if (BudgetWork <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.BudgetWork) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.BudgetWork);
    FBudgetWork := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetConfirmed(const Value: Boolean);
begin
  if (Confirmed <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.Confirmed) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.Confirmed);
    FConfirmed := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetCost(const Value: Double);
begin
  if not(SameValue(Cost, Value) and
    IsValueAssigned(TdxGanttAssignmentAssignedValue.Cost)) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.Cost);
    FCost := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetCostRateTable
  (const Value: TdxGanttControlCostRateTable);
begin
  if (CostRateTable <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.CostRateTable) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.CostRateTable);
    FCostRateTable := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetCostVariance(const Value: Double);
begin
  if not(SameValue(CostVariance, Value) and
    IsValueAssigned(TdxGanttAssignmentAssignedValue.CostVariance)) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.CostVariance);
    FCostVariance := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetCreated(const Value: TDateTime);
begin
  if (Created <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.Created) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.Created);
    FCreated := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetCV(const Value: Double);
begin
  if not(SameValue(CV, Value) and
    IsValueAssigned(TdxGanttAssignmentAssignedValue.CV)) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.CV);
    FCV := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetDelay(const Value: Integer);
begin
  if (Delay <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.Delay) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.Delay);
    FDelay := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetEnterpriseExtendedAttributes
  (const Value: TdxGanttControlEnterpriseExtendedAttributes);
begin
  FEnterpriseExtendedAttributes.Assign(Value);
end;

procedure TdxGanttControlAssignment.SetExtendedAttributes
  (const Value: TdxGanttControlExtendedAttributeReferences);
begin
  FExtendedAttributes.Assign(Value);
end;

procedure TdxGanttControlAssignment.SetFinish(const Value: TDateTime);
begin
  if (Finish <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.Finish) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.Finish);
    FFinish := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetFinishVariance(const Value: Integer);
begin
  if (FinishVariance <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.FinishVariance) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.FinishVariance);
    FFinishVariance := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetFixedMaterial(const Value: Boolean);
begin
  if (FixedMaterial <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.FixedMaterial) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.FixedMaterial);
    FFixedMaterial := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetGUID(const Value: string);
begin
  if (GUID <> Value) or not IsValueAssigned(TdxGanttAssignmentAssignedValue.GUID)
  then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.GUID);
    FGUID := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetHasFixedRateUnits(const Value: Boolean);
begin
  if (HasFixedRateUnits <> Value) or
    not IsValueAssigned(TdxGanttAssignmentAssignedValue.HasFixedRateUnits) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.HasFixedRateUnits);
    FHasFixedRateUnits := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetHyperlink(const Value: string);
begin
  if (Hyperlink <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.Hyperlink) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.Hyperlink);
    FHyperlink := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetHyperlinkAddress(const Value: string);
begin
  if (HyperlinkAddress <> Value) or
    not IsValueAssigned(TdxGanttAssignmentAssignedValue.HyperlinkAddress) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.HyperlinkAddress);
    FHyperlinkAddress := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetHyperlinkSubAddress(const Value: string);
begin
  if (HyperlinkSubAddress <> Value) or
    not IsValueAssigned(TdxGanttAssignmentAssignedValue.HyperlinkSubAddress)
  then
  begin
    Include(FAssignedValues,
      TdxGanttAssignmentAssignedValue.HyperlinkSubAddress);
    FHyperlinkSubAddress := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetLevelingDelay(const Value: Integer);
begin
  if (LevelingDelay <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.LevelingDelay) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.LevelingDelay);
    FLevelingDelay := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetLevelingDelayFormat
  (const Value: TdxDurationFormat);
begin
  if (LevelingDelayFormat <> Value) or
    not IsValueAssigned(TdxGanttAssignmentAssignedValue.LevelingDelayFormat)
  then
  begin
    Include(FAssignedValues,
      TdxGanttAssignmentAssignedValue.LevelingDelayFormat);
    FLevelingDelayFormat := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetLinkedFields(const Value: Boolean);
begin
  if (LinkedFields <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.LinkedFields) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.LinkedFields);
    FLinkedFields := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetMilestone(const Value: Boolean);
begin
  if (Milestone <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.Milestone) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.Milestone);
    FMilestone := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetNoteContainsObjects
  (const Value: Boolean);
begin
  if (NoteContainsObjects <> Value) or
    not IsValueAssigned(TdxGanttAssignmentAssignedValue.NoteContainsObjects)
  then
  begin
    Include(FAssignedValues,
      TdxGanttAssignmentAssignedValue.NoteContainsObjects);
    FNoteContainsObjects := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetNotes(const Value: string);
begin
  if (Notes <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.Notes) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.Notes);
    FNotes := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetOverAllocated(const Value: Boolean);
begin
  if (OverAllocated <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.OverAllocated) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.OverAllocated);
    FOverAllocated := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetOvertimeCost(const Value: Double);
begin
  if not(SameValue(OvertimeCost, Value) and
    IsValueAssigned(TdxGanttAssignmentAssignedValue.OvertimeCost)) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.OvertimeCost);
    FOvertimeCost := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetOvertimeWork(const Value: string);
begin
  if (OvertimeWork <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.OvertimeWork) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.OvertimeWork);
    FOvertimeWork := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetPercentWorkComplete
  (const Value: Integer);
begin
  if (PercentWorkComplete <> Value) or
    not IsValueAssigned(TdxGanttAssignmentAssignedValue.PercentWorkComplete)
  then
  begin
    Include(FAssignedValues,
      TdxGanttAssignmentAssignedValue.PercentWorkComplete);
    FPercentWorkComplete := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetRateScale(const Value: Integer);
begin
  if (RateScale <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.RateScale) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.RateScale);
    FRateScale := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetRegularWork(const Value: string);
begin
  if (RegularWork <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.RegularWork) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.RegularWork);
    FRegularWork := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetRemainingCost(const Value: Double);
begin
  if not(SameValue(RemainingCost, Value) and
    IsValueAssigned(TdxGanttAssignmentAssignedValue.RemainingCost)) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.RemainingCost);
    FRemainingCost := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetRemainingOvertimeCost
  (const Value: Double);
begin
  if not(SameValue(RemainingOvertimeCost, Value) and
    IsValueAssigned(TdxGanttAssignmentAssignedValue.RemainingOvertimeCost)) then
  begin
    Include(FAssignedValues,
      TdxGanttAssignmentAssignedValue.RemainingOvertimeCost);
    FRemainingOvertimeCost := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetRemainingOvertimeWork
  (const Value: string);
begin
  if (RemainingOvertimeWork <> Value) or
    not IsValueAssigned(TdxGanttAssignmentAssignedValue.RemainingOvertimeWork)
  then
  begin
    Include(FAssignedValues,
      TdxGanttAssignmentAssignedValue.RemainingOvertimeWork);
    FRemainingOvertimeWork := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetRemainingWork(const Value: string);
begin
  if (RemainingWork <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.RemainingWork) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.RemainingWork);
    FRemainingWork := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetResourceUID(const Value: Integer);
begin
  if (ResourceUID <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.ResourceUID) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.ResourceUID);
    FResourceUID := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetResponsePending(const Value: Boolean);
begin
  if (ResponsePending <> Value) or
    not IsValueAssigned(TdxGanttAssignmentAssignedValue.ResponsePending) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.ResponsePending);
    FResponsePending := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetResume(const Value: TDateTime);
begin
  if (Resume <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.Resume) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.Resume);
    FResume := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetStart(const Value: TDateTime);
begin
  if (Start <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.Start) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.Start);
    FStart := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetStartVariance(const Value: Integer);
begin
  if (StartVariance <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.StartVariance) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.StartVariance);
    FStartVariance := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetStop(const Value: TDateTime);
begin
  if (Stop <> Value) or not IsValueAssigned(TdxGanttAssignmentAssignedValue.Stop)
  then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.Stop);
    FStop := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetSummary(const Value: Boolean);
begin
  if (Summary <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.Summary) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.Summary);
    FSummary := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetSV(const Value: Double);
begin
  if not(SameValue(SV, Value) and
    IsValueAssigned(TdxGanttAssignmentAssignedValue.SV)) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.SV);
    FSV := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetTaskUID(const Value: Integer);
begin
  if (TaskUID <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.TaskUID) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.TaskUID);
    FTaskUID := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetTimephasedDataItems
  (const Value: TdxGanttControlTimephasedDataItems);
begin
  FTimephasedDataItems.Assign(Value);
end;

procedure TdxGanttControlAssignment.SetUnits(const Value: Double);
begin
  if not(SameValue(Units, Value) and
    IsValueAssigned(TdxGanttAssignmentAssignedValue.Units)) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.Units);
    FUnits := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetUpdateNeeded(const Value: Boolean);
begin
  if (UpdateNeeded <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.UpdateNeeded) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.UpdateNeeded);
    FUpdateNeeded := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetVAC(const Value: Double);
begin
  if not(SameValue(VAC, Value) and
    IsValueAssigned(TdxGanttAssignmentAssignedValue.VAC)) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.VAC);
    FVAC := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetWork(const Value: string);
begin
  if (Work <> Value) or not IsValueAssigned(TdxGanttAssignmentAssignedValue.Work)
  then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.Work);
    FWork := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetWorkContour(const Value: Integer);
begin
  if (WorkContour <> Value) or not IsValueAssigned
    (TdxGanttAssignmentAssignedValue.WorkContour) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.WorkContour);
    FWorkContour := Value;
    Changed;
  end;
end;

procedure TdxGanttControlAssignment.SetWorkVariance(const Value: Double);
begin
  if not(SameValue(WorkVariance, Value) and
    IsValueAssigned(TdxGanttAssignmentAssignedValue.WorkVariance)) then
  begin
    Include(FAssignedValues, TdxGanttAssignmentAssignedValue.WorkVariance);
    FWorkVariance := Value;
    Changed;
  end;
end;

{ TdxGanttControlAssignments }

function TdxGanttControlAssignments.Append: TdxGanttControlAssignment;
begin
  Result := TdxGanttControlAssignment(CreateItem);
  InternalAdd(Result);
end;

procedure TdxGanttControlAssignments.Clear;
begin
  InternalClear;
end;

function TdxGanttControlAssignments.CreateItem
  : TdxGanttControlModelElementListItem;
begin
  Result := TdxGanttControlAssignment.Create(Self);
end;

procedure TdxGanttControlAssignments.DoItemChanged
  (AItem: TdxGanttControlModelElementListItem);
begin
  TdxGanttControlDataModelAccess(DataModel).AssignmentsChangedHandler
    (Self, AItem);
end;

function TdxGanttControlAssignments.GetItem(Index: Integer)
  : TdxGanttControlAssignment;
begin
  Result := TdxGanttControlAssignment(inherited Items[Index]);
end;

procedure TdxGanttControlAssignments.Remove(AItem: TdxGanttControlAssignment);
begin
  InternalRemove(AItem);
end;

end.
