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

unit dxGanttControlTasks;

{$I cxVer.inc}
{$I dxGanttControl.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes, System.Contnrs,
{$ENDIF}
  SysUtils, Controls, Classes, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses,
  dxGanttControlCustomClasses,
  dxGanttControlCustomDataModel,
  dxGanttControlExtendedAttributes,
  dxGanttControlOutlineCodes,
  dxGanttControlCalendars;

type
  TdxGanttControlTask = class;
  TdxGanttControlTasks = class;
  TdxGanttControlWBSMasks = class;

  TdxGanttControlTaskPredecessorLinkType = (FF, FS, SF, SS);

  TdxGanttControlTaskPredecessorLagFormat = (Minutes = 3, ElapsedMinutes, Hours,
    ElapsedHours, Days, ElapsedDays, Weeks, ElapsedWeeks, Months, ElapsedMonths,
    Percent = 19, ElapsedPercent);

  TdxGanttControlTaskConstraintType = (AsSoonAsPossible, AsLateAsPossible,
    MustStartOn, MustFinishOn, StartNoEarlierThan, StartNoLaterThan,
    FinishNoEarlierThan, FinishNoLaterThan);

  TdxGanttControlTaskCommitmentType =
    (TheTaskHasNoDeliverableOrDependencyOnDeliverable,
    TheTaskHasOnAssociatedDeliverable,
    TheTaskHasDependencyOnAnAssociatedDeliverable);

  TdxGanttControlTaskEarnedValueMethod = (UseComplete, UsePhysicalComplete);

  TdxGanttControlTaskType = (FixedUnits, FixedDuration, FixedWork);

  TdxGanttControlWBSMaskType = (Numbers, UppercaseLetters, LowercaseLetters,
    Characters);

  { TdxGanttControlWBSMask }

  TdxGanttControlWBSMask = class(TdxGanttControlModelElementListItem)
  strict private
    FType: TdxGanttControlWBSMaskType;
    FLength: Integer;
    FSeparator: string;
    function InternalGetOwner: TdxGanttControlWBSMasks; inline;
    function GetLevel: Integer;
    procedure SetLength(const Value: Integer);
    procedure SetLevel(const Value: Integer);
    procedure SetSeparator(const Value: string);
    procedure SetType(const Value: TdxGanttControlWBSMaskType);
  protected
    procedure DoReset; override;
  public
    procedure Assign(Source: TPersistent); override;

    property Level: Integer read GetLevel write SetLevel;
    property &Type: TdxGanttControlWBSMaskType read FType write SetType;
    property Length: Integer read FLength write SetLength;
    property Owner: TdxGanttControlWBSMasks read InternalGetOwner;
    property Separator: string read FSeparator write SetSeparator;
  end;

  { TdxGanttControlWBSMasks }

  TdxGanttWBSMasksAssignedValue = (VerifyUniqueCodes, GenerateCodes, Prefix);
  TdxGanttWBSMasksAssignedValues = set of TdxGanttWBSMasksAssignedValue;

  TdxGanttControlWBSMasks = class(TdxGanttControlModelElementList)
  strict private
    FAssignedValues: TdxGanttWBSMasksAssignedValues;
    FVerifyUniqueCodes: Boolean;
    FGenerateCodes: Boolean;
    FPrefix: string;
    function GetItem(Index: Integer): TdxGanttControlWBSMask; inline;
    procedure SetGenerateCodes(const Value: Boolean);
    procedure SetPrefix(const Value: string);
    procedure SetVerifyUniqueCodes(const Value: Boolean);
  protected
    function CreateItem: TdxGanttControlModelElementListItem; override;
    procedure DoReset; override;

    function GetNextLevel: Integer;
  public
    procedure Assign(Source: TPersistent); override;
{$REGION 'for internal use'}
    function IsValueAssigned(const AValue
      : TdxGanttWBSMasksAssignedValue): Boolean;
    procedure ResetValue(const AValue: TdxGanttWBSMasksAssignedValue); overload;
{$ENDREGION 'for internal use'}
    function Append: TdxGanttControlWBSMask;
    procedure Remove(AItem: TdxGanttControlWBSMask);

    property GenerateCodes: Boolean read FGenerateCodes write SetGenerateCodes;
    property Items[Index: Integer]: TdxGanttControlWBSMask
      read GetItem; default;
    property Prefix: string read FPrefix write SetPrefix;
    property VerifyUniqueCodes: Boolean read FVerifyUniqueCodes
      write SetVerifyUniqueCodes;
  end;

  { TdxGanttControlTaskPredecessorLink }

  TdxGanttTaskPredecessorLinkAssignedValue = (PredecessorUID, &Type,
    CrossProject, CrossProjectName, LinkLag, LagFormat);
  TdxGanttTaskPredecessorLinkAssignedValues = set of
    TdxGanttTaskPredecessorLinkAssignedValue;

  TdxGanttControlTaskPredecessorLink = class
    (TdxGanttControlModelElementListItem)
  strict private
    FAssignedValues: TdxGanttTaskPredecessorLinkAssignedValues;
    FPredecessorUID: Integer;
    FType: TdxGanttControlTaskPredecessorLinkType;
    FCrossProject: Boolean;
    FCrossProjectName: string;
    FLinkLag: Integer;
    FLagFormat: TdxGanttControlTaskPredecessorLagFormat;
    function GetTask: TdxGanttControlTask; inline;
    procedure SetCrossProject(const Value: Boolean);
    procedure SetCrossProjectName(const Value: string);
    procedure SetLagFormat(const Value
      : TdxGanttControlTaskPredecessorLagFormat);
    procedure SetLinkLag(const Value: Integer);
    procedure SetPredecessorUID(const Value: Integer);
    procedure SetType(const Value: TdxGanttControlTaskPredecessorLinkType);
  protected
    procedure DoReset; override;

    function GetLagAsDisplayValue: string;
  public
    procedure Assign(Source: TPersistent); override;
{$REGION 'for internal use'}
    function IsValueAssigned(const AValue
      : TdxGanttTaskPredecessorLinkAssignedValue): Boolean;
    procedure ResetValue(const AValue
      : TdxGanttTaskPredecessorLinkAssignedValue); overload;
{$ENDREGION 'for internal use'}
    function ToString: string; override;

    function IsElapsedLagFormat: Boolean;
    function IsPercentLagFormat: Boolean;

    property Task: TdxGanttControlTask read GetTask;
    property PredecessorUID: Integer read FPredecessorUID
      write SetPredecessorUID;
    property &Type: TdxGanttControlTaskPredecessorLinkType read FType
      write SetType;
{$REGION 'for internal use'}
    property CrossProject: Boolean read FCrossProject write SetCrossProject;
    property CrossProjectName: string read FCrossProjectName
      write SetCrossProjectName;
{$ENDREGION 'for internal use'}
    property LinkLag: Integer read FLinkLag write SetLinkLag;
    property LagFormat: TdxGanttControlTaskPredecessorLagFormat read FLagFormat
      write SetLagFormat;
  end;

  { TdxGanttControlTaskPredecessorLinks }

  TdxGanttControlTaskPredecessorLinks = class
    (TdxGanttControlElementCustomOwnedList)
  strict private
    function GetItem(Index: Integer)
      : TdxGanttControlTaskPredecessorLink; inline;
  protected
    function CreateItem: TdxGanttControlModelElementListItem; override;
    procedure DoListChanged(const AItem: TdxGanttControlModelElementListItem;
      AAction: TCollectionNotification); override;
  public
    function Append: TdxGanttControlTaskPredecessorLink;
    procedure Clear;
    function GetItemByPredecessorUID(AUID: Integer)
      : TdxGanttControlTaskPredecessorLink;
    procedure Remove(AItem: TdxGanttControlTaskPredecessorLink);
    function ToString: string; override;
    function ToArray: TArray<Integer>;

    property Items[Index: Integer]: TdxGanttControlTaskPredecessorLink
      read GetItem; default;
  end;

  { TdxGanttControlTaskBaseline }

  TdxGanttTaskBaselineAssignedValue = (Start, Finish, Duration, DurationFormat,
    EstimatedDuration, FixedCost, Interim);
  TdxGanttTaskBaselineAssignedValues = set of TdxGanttTaskBaselineAssignedValue;

  TdxGanttControlTaskBaseline = class(TdxGanttControlCustomBaseline)
  strict private
    FAssignedValues: TdxGanttTaskBaselineAssignedValues;
    FDuration: TDuration;
    FDurationFormat: TdxDurationFormat;
    FEstimatedDuration: Boolean;
    FFixedCost: Double;
    FInterim: Boolean;
    FStart: TDateTime;
    FFinish: TDateTime;
    FTimephasedDataItems: TdxGanttControlTimephasedDataItems;

    procedure SetDuration(const Value: TDuration);
    procedure SetDurationFormat(const Value: TdxDurationFormat);
    procedure SetEstimatedDuration(const Value: Boolean);
    procedure SetFinish(const Value: TDateTime);
    procedure SetFixedCost(const Value: Double);
    procedure SetInterim(const Value: Boolean);
    procedure SetStart(const Value: TDateTime);
    procedure SetTimephasedDataItems(const Value
      : TdxGanttControlTimephasedDataItems);
  protected
    procedure DoReset; override;
  public
    constructor Create(AOwner: TdxGanttControlModelElement); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
{$REGION 'for internal use'}
    function IsValueAssigned(const AValue: TdxGanttTaskBaselineAssignedValue)
      : Boolean; overload;
    procedure ResetValue(const AValue
      : TdxGanttTaskBaselineAssignedValue); overload;
{$ENDREGION 'for internal use'}
    property TimephasedDataItems: TdxGanttControlTimephasedDataItems
      read FTimephasedDataItems write SetTimephasedDataItems;
    property Interim: Boolean read FInterim write SetInterim;
    property Start: TDateTime read FStart write SetStart;
    property Finish: TDateTime read FFinish write SetFinish;
    property Duration: TDuration read FDuration write SetDuration;
    property DurationFormat: TdxDurationFormat read FDurationFormat
      write SetDurationFormat;
    property EstimatedDuration: Boolean read FEstimatedDuration
      write SetEstimatedDuration;
    property FixedCost: Double read FFixedCost write SetFixedCost;
  end;

  { TdxGanttControlTaskBaselines }

  TdxGanttControlTaskBaselines = class(TdxGanttControlCustomBaselines)
  strict private
    function GetItem(Index: Integer): TdxGanttControlTaskBaseline; inline;
  protected
    function CreateItem: TdxGanttControlModelElementListItem; override;
  public
    function Append: TdxGanttControlTaskBaseline; reintroduce;

    property Items[Index: Integer]: TdxGanttControlTaskBaseline
      read GetItem; default;
  end;

  { TdxGanttControlTask }

  TdxGanttTaskAssignedValue = (Active, ActualCost, ActualDuration, ActualFinish,
    ActualOvertimeCost, ActualOvertimeWork, ActualOvertimeWorkProtected,
    ActualStart, ActualWork, ActualWorkProtected, ACWP, BCWP, BCWS, CalendarUID,
    CommitmentFinish, CommitmentStart, CommitmentType, ConstraintDate,
    ConstraintType, Contact, Cost, Critical, CV, Deadline, DisplayAsSummary,
    DisplayOnTimeline, Duration, DurationFormat, EarlyFinish, EarlyStart,
    EarnedValueMethod, EffortDriven, Estimated, ExternalTask,
    ExternalTaskProject, Finish, FinishSlack, FinishVariance, FixedCost,
    FixedCostAccrual, FreeformDurationFormat, FreeSlack, GUID, HideBar,
    Hyperlink, HyperlinkAddress, HyperlinkSubAddress, IgnoreResourceCalendar,
    Blank, &Published, Subproject, SubprojectReadOnly, LateFinish, LateStart,
    LevelAssignments, LevelingCanSplit, LevelingDelay, LevelingDelayFormat,
    Manual, ManualDuration, ManualFinish, ManualStart, Milestone, Name,
    NoteContainsObjects, Notes, OutlineLevel, OutlineNumber, OverAllocated,
    OvertimeCost, OvertimeWork, PercentComplete, PercentWorkComplete,
    PhysicalPercentComplete, PreLeveledFinish, PreLeveledStart, Priority,
    Recurring, RegularWork, RemainingCost, RemainingDuration,
    RemainingOvertimeCost, RemainingOvertimeWork, RemainingWork, Resume,
    ResumeValid, Rollup, Start, StartSlack, StartVariance, StatusManager, Stop,
    SubprojectName, Summary, TotalSlack, &Type, WBS, WBSLevel, Work,
    WorkVariance, Baselines, ExtendedAttributes, EnterpriseExtendedAttributes,
    OutlineCodes, PredecessorLinks, TimephasedDataItems);

  TdxGanttTaskAssignedValues = set of TdxGanttTaskAssignedValue;

  TdxGanttControlTask = class(TdxGanttControlModelUIDElement)
  strict private
    FAssignedValues: TdxGanttTaskAssignedValues;
    FActive: Boolean;
    FActualCost: Double;
    FActualDuration: TDuration;
    FActualFinish: TDateTime;
    FActualOvertimeCost: Double;
    FActualOvertimeWork: string;
    FActualOvertimeWorkProtected: string;
    FActualStart: TDateTime;
    FActualWork: string;
    FActualWorkProtected: string;
    FACWP: Double;
    FBaselines: TdxGanttControlTaskBaselines;
    FBCWP: Double;
    FBCWS: Double;
    FCalendarUID: Integer;
    FCommitmentFinish: TDateTime;
    FCommitmentStart: TDateTime;
    FCommitmentType: TdxGanttControlTaskCommitmentType;
    FConstraintDate: TDateTime;
    FConstraintType: TdxGanttControlTaskConstraintType;
    FContact: string;
    FCost: Double;
    FCreated: TDateTime;
    FCritical: Boolean;
    FCV: Double;
    FDeadline: TDateTime;
    FDisplayAsSummary: Boolean;
    FDisplayOnTimeline: Boolean;
    FDuration: TDuration;
    FDurationAsDisplayValue: Variant;
    FDurationFormat: TdxDurationFormat;
    FEarlyFinish: TDateTime;
    FEarlyStart: TDateTime;
    FEarnedValueMethod: TdxGanttControlTaskEarnedValueMethod;
    FEffortDriven: Boolean;
    FEnterpriseExtendedAttributes: TdxGanttControlEnterpriseExtendedAttributes;
    FEstimated: Boolean;
    FExtendedAttributes: TdxGanttControlExtendedAttributeReferences;
    FExternalTask: Boolean;
    FExternalTaskProject: string;
    FFinish: TDateTime;
    FFinishSlack: Integer;
    FFinishVariance: Integer;
    FFixedCost: Double;
    FFixedCostAccrual: Integer;
    FFreeformDurationFormat: TdxDurationFormat;
    FFreeSlack: Integer;
    FGUID: string;
    FHideBar: Boolean;
    FHyperlink: string;
    FHyperlinkAddress: string;
    FHyperlinkSubAddress: string;
    FIgnoreResourceCalendar: Boolean;
    FBlank: Boolean;
    FPublished: Boolean;
    FSubproject: Boolean;
    FSubprojectReadOnly: Boolean;
    FLateFinish: TDateTime;
    FLateStart: TDateTime;
    FLevelAssignments: Boolean;
    FLevelingCanSplit: Boolean;
    FLevelingDelay: Integer;
    FLevelingDelayFormat: TdxDurationFormat;
    FManual: Boolean;
    FManualDuration: TDuration;
    FManualFinish: TDateTime;
    FManualStart: TDateTime;
    FMilestone: Boolean;
    FName: string;
    FNoteContainsObjects: Boolean;
    FNotes: string;
    FOutlineCodes: TdxGanttControlOutlineCodeReferences;
    FOutlineLevel: Integer;
    FOutlineNumber: string;
    FOverAllocated: Boolean;
    FOvertimeCost: Double;
    FOvertimeWork: string;
    FPercentComplete: Integer;
    FPercentWorkComplete: Integer;
    FPhysicalPercentComplete: Integer;
    FPredecessorLinks: TdxGanttControlTaskPredecessorLinks;
    FPreLeveledFinish: TDateTime;
    FPreLeveledStart: TDateTime;
    FPriority: Integer;
    FRecurring: Boolean;
    FRegularWork: string;
    FRemainingCost: Double;
    FRemainingDuration: TDuration;
    FRemainingOvertimeCost: Double;
    FRemainingOvertimeWork: string;
    FRemainingWork: string;
    FResume: TDateTime;
    FResumeValid: Boolean;
    FRollup: Boolean;
    FStart: TDateTime;
    FStartSlack: Integer;
    FStartVariance: Integer;
    FStatusManager: string;
    FStop: TDateTime;
    FSubprojectName: string;
    FSummary: Boolean;
    FTimephasedDataItems: TdxGanttControlTimephasedDataItems;
    FTotalSlack: Integer;
    FType: TdxGanttControlTaskType;
    FWBS: string;
    FWBSLevel: string;
    FWork: string;
    FWorkVariance: Double;

    FLinks: TdxFastList;

    function GetDurationAsDisplayValue: Variant;
    function GetID: Integer;
    function GetRealCalendar: TdxGanttControlCalendar;
    function GetRealDuration: string;
    function GetRealDurationFormat: TdxDurationFormat;
    function GetRealManual: Boolean;
    function GetResourceName: string;
    function GetResourceNames: TArray<string>;
    function GetResources: TArray<Integer>;
    function InternalGetOwner: TdxGanttControlTasks; inline;

    function GetBaselines: TdxGanttControlTaskBaselines;
    function GetExtendedAttributes: TdxGanttControlExtendedAttributeReferences;
    function GetEnterpriseExtendedAttributes
      : TdxGanttControlEnterpriseExtendedAttributes;
    function GetOutlineCodes: TdxGanttControlOutlineCodeReferences;
    function GetPredecessorLinks: TdxGanttControlTaskPredecessorLinks;
    function GetTimephasedDataItems: TdxGanttControlTimephasedDataItems;

    procedure SetActive(const Value: Boolean);
    procedure SetActualCost(const Value: Double);
    procedure SetActualDuration(const Value: TDuration);
    procedure SetActualFinish(const Value: TDateTime);
    procedure SetActualOvertimeCost(const Value: Double);
    procedure SetActualOvertimeWork(const Value: string);
    procedure SetActualOvertimeWorkProtected(const Value: string);
    procedure SetActualStart(const Value: TDateTime);
    procedure SetActualWork(const Value: string);
    procedure SetActualWorkProtected(const Value: string);
    procedure SetACWP(const Value: Double);
    procedure SetBaselines(const Value: TdxGanttControlTaskBaselines);
    procedure SetBCWP(const Value: Double);
    procedure SetBCWS(const Value: Double);
    procedure SetCalendarUID(const Value: Integer);
    procedure SetCommitmentFinish(const Value: TDateTime);
    procedure SetCommitmentStart(const Value: TDateTime);
    procedure SetCommitmentType(const Value: TdxGanttControlTaskCommitmentType);
    procedure SetConstraintDate(const Value: TDateTime);
    procedure SetConstraintType(const Value: TdxGanttControlTaskConstraintType);
    procedure SetContact(const Value: string);
    procedure SetCost(const Value: Double);
    procedure SetCreated(const Value: TDateTime);
    procedure SetCritical(const Value: Boolean);
    procedure SetCV(const Value: Double);
    procedure SetDeadline(const Value: TDateTime);
    procedure SetDisplayAsSummary(const Value: Boolean);
    procedure SetDisplayOnTimeline(const Value: Boolean);
    procedure SetDuration(const Value: TDuration);
    procedure SetDurationFormat(const Value: TdxDurationFormat);
    procedure SetEarlyFinish(const Value: TDateTime);
    procedure SetEarlyStart(const Value: TDateTime);
    procedure SetEarnedValueMethod(const Value
      : TdxGanttControlTaskEarnedValueMethod);
    procedure SetEffortDriven(const Value: Boolean);
    procedure SetEnterpriseExtendedAttributes(const Value
      : TdxGanttControlEnterpriseExtendedAttributes);
    procedure SetEstimated(const Value: Boolean);
    procedure SetExtendedAttributes(const Value
      : TdxGanttControlExtendedAttributeReferences);
    procedure SetExternalTask(const Value: Boolean);
    procedure SetExternalTaskProject(const Value: string);
    procedure SetFinish(const Value: TDateTime);
    procedure SetFinishSlack(const Value: Integer);
    procedure SetFinishVariance(const Value: Integer);
    procedure SetFixedCost(const Value: Double);
    procedure SetFixedCostAccrual(const Value: Integer);
    procedure SetFreeformDurationFormat(const Value: TdxDurationFormat);
    procedure SetFreeSlack(const Value: Integer);
    procedure SetGUID(const Value: string);
    procedure SetHideBar(const Value: Boolean);
    procedure SetHyperlink(const Value: string);
    procedure SetHyperlinkAddress(const Value: string);
    procedure SetHyperlinkSubAddress(const Value: string);
    procedure SetID(const Value: Integer);
    procedure SetIgnoreResourceCalendar(const Value: Boolean);
    procedure SetBlank(const Value: Boolean);
    procedure SetPublished(const Value: Boolean);
    procedure SetSubproject(const Value: Boolean);
    procedure SetSubprojectReadOnly(const Value: Boolean);
    procedure SetLateFinish(const Value: TDateTime);
    procedure SetLateStart(const Value: TDateTime);
    procedure SetLevelAssignments(const Value: Boolean);
    procedure SetLevelingCanSplit(const Value: Boolean);
    procedure SetLevelingDelay(const Value: Integer);
    procedure SetLevelingDelayFormat(const Value: TdxDurationFormat);
    procedure SetManual(const Value: Boolean);
    procedure SetManualDuration(const Value: TDuration);
    procedure SetManualFinish(const Value: TDateTime);
    procedure SetManualStart(const Value: TDateTime);
    procedure SetMilestone(const Value: Boolean);
    procedure SetName(const Value: string);
    procedure SetNoteContainsObjects(const Value: Boolean);
    procedure SetNotes(const Value: string);
    procedure SetOutlineCodes(const Value
      : TdxGanttControlOutlineCodeReferences);
    procedure SetOutlineLevel(const Value: Integer);
    procedure SetOutlineNumber(const Value: string);
    procedure SetOverAllocated(const Value: Boolean);
    procedure SetOvertimeCost(const Value: Double);
    procedure SetOvertimeWork(const Value: string);
    procedure SetPercentComplete(const Value: Integer);
    procedure SetPercentWorkComplete(const Value: Integer);
    procedure SetPhysicalPercentComplete(const Value: Integer);
    procedure SetPredecessorLinks(const Value
      : TdxGanttControlTaskPredecessorLinks);
    procedure SetPreLeveledFinish(const Value: TDateTime);
    procedure SetPreLeveledStart(const Value: TDateTime);
    procedure SetPriority(const Value: Integer);
    procedure SetRecurring(const Value: Boolean);
    procedure SetRegularWork(const Value: string);
    procedure SetRemainingCost(const Value: Double);
    procedure SetRemainingDuration(const Value: TDuration);
    procedure SetRemainingOvertimeCost(const Value: Double);
    procedure SetRemainingOvertimeWork(const Value: string);
    procedure SetRemainingWork(const Value: string);
    procedure SetResume(const Value: TDateTime);
    procedure SetResumeValid(const Value: Boolean);
    procedure SetRollup(const Value: Boolean);
    procedure SetStart(const Value: TDateTime);
    procedure SetStartSlack(const Value: Integer);
    procedure SetStartVariance(const Value: Integer);
    procedure SetStatusManager(const Value: string);
    procedure SetStop(const Value: TDateTime);
    procedure SetSubprojectName(const Value: string);
    procedure SetSummary(const Value: Boolean);
    procedure SetTimephasedDataItems(const Value
      : TdxGanttControlTimephasedDataItems);
    procedure SetTotalSlack(const Value: Integer);
    procedure SetType(const Value: TdxGanttControlTaskType);
    procedure SetWBS(const Value: string);
    procedure SetWBSLevel(const Value: string);
    procedure SetWork(const Value: string);
    procedure SetWorkVariance(const Value: Double);
  protected
    procedure DoReset; override;
    procedure SetUID(const Value: Integer); override;

    property Links: TdxFastList read FLinks;
  public
    constructor Create(AOwner: TdxGanttControlModelElement); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
{$REGION 'for internal use'}
    function IsValueAssigned(const AValue: TdxGanttTaskAssignedValue): Boolean;
    procedure ResetValue(const AValue: TdxGanttTaskAssignedValue); overload;
{$ENDREGION 'for internal use'}
{$REGION 'for internal use'}
    function GetDurationInfo: string;
    function GetFinishInfo: string;
    function GetPercentCompleteInfo: string;
    function GetStartInfo: string;
{$ENDREGION}
    property Owner: TdxGanttControlTasks read InternalGetOwner;
{$REGION 'for internal use'}
    property RealCalendar: TdxGanttControlCalendar read GetRealCalendar;
    property RealDuration: string read GetRealDuration;
    property RealDurationFormat: TdxDurationFormat read GetRealDurationFormat;
    property DurationAsDisplayValue: Variant read GetDurationAsDisplayValue;
    property RealManual: Boolean read GetRealManual;
    property ResourceName: string read GetResourceName;
    property ResourceNames: TArray<string> read GetResourceNames;
    property Resources: TArray<Integer> read GetResources;
{$ENDREGION}
{$REGION 'for internal use'}
    property Baselines: TdxGanttControlTaskBaselines read GetBaselines
      write SetBaselines;
    property ExtendedAttributes: TdxGanttControlExtendedAttributeReferences
      read GetExtendedAttributes write SetExtendedAttributes;
    property EnterpriseExtendedAttributes
      : TdxGanttControlEnterpriseExtendedAttributes
      read GetEnterpriseExtendedAttributes
      write SetEnterpriseExtendedAttributes;
    property OutlineCodes: TdxGanttControlOutlineCodeReferences
      read GetOutlineCodes write SetOutlineCodes;
    property TimephasedDataItems: TdxGanttControlTimephasedDataItems
      read GetTimephasedDataItems write SetTimephasedDataItems;
{$ENDREGION}
    property Blank: Boolean read FBlank write SetBlank;
    property CalendarUID: Integer read FCalendarUID write SetCalendarUID;
    property ConstraintDate: TDateTime read FConstraintDate
      write SetConstraintDate;
    property ConstraintType: TdxGanttControlTaskConstraintType
      read FConstraintType write SetConstraintType;
    property Created: TDateTime read FCreated write SetCreated;
    property DisplayOnTimeline: Boolean read FDisplayOnTimeline
      write SetDisplayOnTimeline;
    property Duration: TDuration read FDuration write SetDuration;
    property DurationFormat: TdxDurationFormat read FDurationFormat
      write SetDurationFormat;
    property Estimated: Boolean read FEstimated write SetEstimated;
    property Finish: TDateTime read FFinish write SetFinish;
    property GUID: string read FGUID write SetGUID;
    property ID: Integer read GetID write SetID;
    property Manual: Boolean read FManual write SetManual;
    property Milestone: Boolean read FMilestone write SetMilestone;
    property Name: string read FName write SetName;
    property OutlineLevel: Integer read FOutlineLevel write SetOutlineLevel;
    property PercentComplete: Integer read FPercentComplete
      write SetPercentComplete;
    property PredecessorLinks: TdxGanttControlTaskPredecessorLinks
      read GetPredecessorLinks write SetPredecessorLinks;
    property Start: TDateTime read FStart write SetStart;
    property Summary: Boolean read FSummary write SetSummary;
{$REGION 'for internal use'}
    property Active: Boolean read FActive write SetActive;
    property ActualCost: Double read FActualCost write SetActualCost;
    property ActualDuration: TDuration read FActualDuration
      write SetActualDuration;
    property ActualFinish: TDateTime read FActualFinish write SetActualFinish;
    property ActualOvertimeCost: Double read FActualOvertimeCost
      write SetActualOvertimeCost;
    property ActualOvertimeWork: string read FActualOvertimeWork
      write SetActualOvertimeWork;
    property ActualOvertimeWorkProtected: string
      read FActualOvertimeWorkProtected write SetActualOvertimeWorkProtected;
    property ActualStart: TDateTime read FActualStart write SetActualStart;
    property ActualWork: string read FActualWork write SetActualWork;
    property ActualWorkProtected: string read FActualWorkProtected
      write SetActualWorkProtected;
    property ACWP: Double read FACWP write SetACWP;
    property BCWP: Double read FBCWP write SetBCWP;
    property BCWS: Double read FBCWS write SetBCWS;
    property CommitmentFinish: TDateTime read FCommitmentFinish
      write SetCommitmentFinish;
    property CommitmentStart: TDateTime read FCommitmentStart
      write SetCommitmentStart;
    property CommitmentType: TdxGanttControlTaskCommitmentType
      read FCommitmentType write SetCommitmentType;
    property Contact: string read FContact write SetContact;
    property Cost: Double read FCost write SetCost;
    property Critical: Boolean read FCritical write SetCritical;
    property CV: Double read FCV write SetCV;
    property Deadline: TDateTime read FDeadline write SetDeadline;
    property DisplayAsSummary: Boolean read FDisplayAsSummary
      write SetDisplayAsSummary;
    property EarlyFinish: TDateTime read FEarlyFinish write SetEarlyFinish;
    property EarlyStart: TDateTime read FEarlyStart write SetEarlyStart;
    property EarnedValueMethod: TdxGanttControlTaskEarnedValueMethod
      read FEarnedValueMethod write SetEarnedValueMethod;
    property EffortDriven: Boolean read FEffortDriven write SetEffortDriven;
    property ExternalTask: Boolean read FExternalTask write SetExternalTask;
    property ExternalTaskProject: string read FExternalTaskProject
      write SetExternalTaskProject;
    property FinishSlack: Integer read FFinishSlack write SetFinishSlack;
    property FinishVariance: Integer read FFinishVariance
      write SetFinishVariance;
    property FixedCost: Double read FFixedCost write SetFixedCost;
    property FixedCostAccrual: Integer read FFixedCostAccrual
      write SetFixedCostAccrual;
    property FreeformDurationFormat: TdxDurationFormat
      read FFreeformDurationFormat write SetFreeformDurationFormat;
    property FreeSlack: Integer read FFreeSlack write SetFreeSlack;
    property HideBar: Boolean read FHideBar write SetHideBar;
    property Hyperlink: string read FHyperlink write SetHyperlink;
    property HyperlinkAddress: string read FHyperlinkAddress
      write SetHyperlinkAddress;
    property HyperlinkSubAddress: string read FHyperlinkSubAddress
      write SetHyperlinkSubAddress;
    property IgnoreResourceCalendar: Boolean read FIgnoreResourceCalendar
      write SetIgnoreResourceCalendar;
    property &Published: Boolean read FPublished write SetPublished;
    property Subproject: Boolean read FSubproject write SetSubproject;
    property SubprojectReadOnly: Boolean read FSubprojectReadOnly
      write SetSubprojectReadOnly;
    property LateFinish: TDateTime read FLateFinish write SetLateFinish;
    property LateStart: TDateTime read FLateStart write SetLateStart;
    property LevelAssignments: Boolean read FLevelAssignments
      write SetLevelAssignments;
    property LevelingCanSplit: Boolean read FLevelingCanSplit
      write SetLevelingCanSplit;
    property LevelingDelay: Integer read FLevelingDelay write SetLevelingDelay;
    property LevelingDelayFormat: TdxDurationFormat read FLevelingDelayFormat
      write SetLevelingDelayFormat;
    property ManualDuration: TDuration read FManualDuration
      write SetManualDuration;
    property ManualFinish: TDateTime read FManualFinish write SetManualFinish;
    property ManualStart: TDateTime read FManualStart write SetManualStart;
    property NoteContainsObjects: Boolean read FNoteContainsObjects
      write SetNoteContainsObjects;
    property Notes: string read FNotes write SetNotes;
    property OutlineNumber: string read FOutlineNumber write SetOutlineNumber;
    property OverAllocated: Boolean read FOverAllocated write SetOverAllocated;
    property OvertimeCost: Double read FOvertimeCost write SetOvertimeCost;
    property OvertimeWork: string read FOvertimeWork write SetOvertimeWork;
    property PercentWorkComplete: Integer read FPercentWorkComplete
      write SetPercentWorkComplete;
    property PhysicalPercentComplete: Integer read FPhysicalPercentComplete
      write SetPhysicalPercentComplete;
    property PreLeveledFinish: TDateTime read FPreLeveledFinish
      write SetPreLeveledFinish;
    property PreLeveledStart: TDateTime read FPreLeveledStart
      write SetPreLeveledStart;
    property Priority: Integer read FPriority write SetPriority;
    property Recurring: Boolean read FRecurring write SetRecurring;
    property RegularWork: string read FRegularWork write SetRegularWork;
    property RemainingCost: Double read FRemainingCost write SetRemainingCost;
    property RemainingDuration: TDuration read FRemainingDuration
      write SetRemainingDuration;
    property RemainingOvertimeCost: Double read FRemainingOvertimeCost
      write SetRemainingOvertimeCost;
    property RemainingOvertimeWork: string read FRemainingOvertimeWork
      write SetRemainingOvertimeWork;
    property RemainingWork: string read FRemainingWork write SetRemainingWork;
    property Resume: TDateTime read FResume write SetResume;
    property ResumeValid: Boolean read FResumeValid write SetResumeValid;
    property Rollup: Boolean read FRollup write SetRollup;
    property StartSlack: Integer read FStartSlack write SetStartSlack;
    property StartVariance: Integer read FStartVariance write SetStartVariance;
    property StatusManager: string read FStatusManager write SetStatusManager;
    property Stop: TDateTime read FStop write SetStop;
    property SubprojectName: string read FSubprojectName
      write SetSubprojectName;
    property TotalSlack: Integer read FTotalSlack write SetTotalSlack;
    property &Type: TdxGanttControlTaskType read FType write SetType;
    property WBS: string read FWBS write SetWBS;
    property WBSLevel: string read FWBSLevel write SetWBSLevel;
    property Work: string read FWork write SetWork;
    property WorkVariance: Double read FWorkVariance write SetWorkVariance;
{$ENDREGION}
  end;

  { TdxGanttControlTasks }

  TdxGanttControlTasks = class(TdxGanttControlModelUIDElementList)
  strict private
    FDirtyTasks: TList<Integer>;
    FLateAsPossibleTasks: TdxFastList;
    function GetItem(Index: Integer): TdxGanttControlTask; inline;
  protected
    function AppendEmpty: TdxGanttControlTask;
    function CreateItem: TdxGanttControlModelElementListItem; override;
    procedure CreateProjectSummary; virtual;
    procedure DoItemChanged
      (AItem: TdxGanttControlModelElementListItem); override;
    procedure DoReset; override;
    procedure InsertEmpty(Index, ACount: Integer);
    procedure InternalAdd(AItem: TdxGanttControlModelElementListItem); override;
    procedure InternalClear; override;
    procedure InternalExtract
      (AItem: TdxGanttControlModelElementListItem); override;
    procedure InternalInsert(AIndex: Integer;
      AItem: TdxGanttControlModelElementListItem); override;
    procedure InternalRemove
      (AItem: TdxGanttControlModelElementListItem); override;

    procedure AddDirtyTask(AUID: Integer);
    procedure CalculateDirtyTask(ATask: TdxGanttControlTask);
    procedure CalculateDirtyTasks;
    function IsDirtyTask(ATask: TdxGanttControlTask): Boolean;
    property LateAsPossibleTasks: TdxFastList read FLateAsPossibleTasks;
  public
    constructor Create(ADataModel: TdxGanttControlCustomDataModel); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    function Append: TdxGanttControlTask;
    function Insert(Index: Integer): TdxGanttControlTask;
    procedure Delete(Index: Integer);
    function GetItemByUID(const AUID: Integer): TdxGanttControlTask;
      reintroduce;

    property Items[Index: Integer]: TdxGanttControlTask read GetItem; default;
  end;

function GetDeletingTaskConfirmation(ATask: TdxGanttControlTask): string;

implementation

uses
  Variants, Math, StrUtils, RTLConsts,
  dxCultureInfo,
  dxGanttControlStrs,
  dxGanttControlDataModel,
  dxGanttControlResources,
  dxGanttControlUtils,
  dxGanttControlTaskDependencyDialog;

type
  TdxGanttControlDataModelAccess = class(TdxGanttControlDataModel);
  TdxGanttControlDataModelPropertiesAccess = class
    (TdxGanttControlDataModelProperties);

function GetDeletingTaskConfirmation(ATask: TdxGanttControlTask): string;
begin
  if ATask.Summary then
    Result := cxGetResourceString(@sdxGanttControlConfirmationDeleteSummary)
  else
    Result := cxGetResourceString(@sdxGanttControlConfirmationDeleteTask);
  if Trim(ATask.Name) = '' then
    Result := Format(Result,
      [Format(cxGetResourceString(@sdxGanttControlTaskID), [ATask.ID])])
  else
    Result := Format(Result, [ATask.Name]);
end;

{ TdxGanttControlTaskPredecessorLink }

procedure TdxGanttControlTaskPredecessorLink.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlTaskPredecessorLink;
begin
  if Source is TdxGanttControlTaskPredecessorLink then
  begin
    ASource := TdxGanttControlTaskPredecessorLink(Source);
    BeginUpdate;
    try
      &Type := ASource.&Type;
      CrossProject := ASource.CrossProject;
      CrossProjectName := ASource.CrossProjectName;
      LinkLag := ASource.LinkLag;
      LagFormat := ASource.LagFormat;
      PredecessorUID := ASource.PredecessorUID;
      FAssignedValues := ASource.FAssignedValues;
    finally
      EndUpdate;
    end;
  end;
  inherited Assign(Source);
end;

procedure TdxGanttControlTaskPredecessorLink.DoReset;
begin
  FType := TdxGanttControlTaskPredecessorLinkType.FS;
  FAssignedValues := [];
end;

function TdxGanttControlTaskPredecessorLink.IsValueAssigned
  (const AValue: TdxGanttTaskPredecessorLinkAssignedValue): Boolean;
begin
  Result := AValue in FAssignedValues;
end;

procedure TdxGanttControlTaskPredecessorLink.ResetValue
  (const AValue: TdxGanttTaskPredecessorLinkAssignedValue);
begin
  if IsValueAssigned(AValue) then
  begin
    if AValue = TdxGanttTaskPredecessorLinkAssignedValue.PredecessorUID then
      Task.Owner.AddDirtyTask(PredecessorUID);
    Exclude(FAssignedValues, AValue);
    Changed;
  end
end;

function TdxGanttControlTaskPredecessorLink.GetLagAsDisplayValue: string;

  function GetUnit(const AValue: Double): string;
  const
    OneUnitMap: array [3 .. 12] of TcxResourceStringID =
      (@sdxGanttControlDurationFormatMinuteShort,
      @sdxGanttControlDurationFormatMinuteShort,
      @sdxGanttControlDurationFormatHourShort,
      @sdxGanttControlDurationFormatHourShort,
      @sdxGanttControlDurationFormatDay, @sdxGanttControlDurationFormatDay,
      @sdxGanttControlDurationFormatWeekShort,
      @sdxGanttControlDurationFormatWeekShort,
      @sdxGanttControlDurationFormatMonthShort,
      @sdxGanttControlDurationFormatMonthShort);
    MoreUnitsMap: array [3 .. 12] of TcxResourceStringID =
      (@sdxGanttControlDurationFormatMinutesShort,
      @sdxGanttControlDurationFormatMinutesShort,
      @sdxGanttControlDurationFormatHoursShort,
      @sdxGanttControlDurationFormatHoursShort,
      @sdxGanttControlDurationFormatDays, @sdxGanttControlDurationFormatDays,
      @sdxGanttControlDurationFormatWeeksShort,
      @sdxGanttControlDurationFormatWeeksShort,
      @sdxGanttControlDurationFormatMonthsShort,
      @sdxGanttControlDurationFormatMonthsShort);
  begin
    if IsPercentLagFormat then
      Result := '%'
    else
    begin
      if AValue = 1.0 then
        Result := cxGetResourceString(OneUnitMap[Ord(LagFormat)])
      else
        Result := cxGetResourceString(MoreUnitsMap[Ord(LagFormat)]);
    end;
    if IsElapsedLagFormat then
      Result := cxGetResourceString
        (@sdxGanttControlDurationFormatElapsedTimePrefix) + Result;
    if not IsPercentLagFormat then
      Result := ' ' + Result;
  end;

  function GetDenominator: Double;
  begin
    case LagFormat of
      TdxGanttControlTaskPredecessorLagFormat.Hours,
        TdxGanttControlTaskPredecessorLagFormat.ElapsedHours:
        Result := 10 * 60;
      TdxGanttControlTaskPredecessorLagFormat.Days:
        Result := 10 * 60 * 8;
      TdxGanttControlTaskPredecessorLagFormat.ElapsedDays:
        Result := 10 * 60 * 24;
      TdxGanttControlTaskPredecessorLagFormat.Weeks:
        Result := 10 * 60 * 8 * 5;
      TdxGanttControlTaskPredecessorLagFormat.ElapsedWeeks:
        Result := 10 * 60 * 24 * 7;
      TdxGanttControlTaskPredecessorLagFormat.Months:
        Result := 10 * 60 * 8 * 5 * 4;
      TdxGanttControlTaskPredecessorLagFormat.ElapsedMonths:
        Result := 10 * 60 * 24 * 30;
      TdxGanttControlTaskPredecessorLagFormat.Percent,
        TdxGanttControlTaskPredecessorLagFormat.ElapsedPercent:
        Result := 1;
    else
      Result := 10;
    end;
  end;

var
  AHasLinkLag: Boolean;
  ALag: Double;
  AUnit: string;
begin
  Result := '';
  AHasLinkLag := IsValueAssigned
    (TdxGanttTaskPredecessorLinkAssignedValue.LinkLag) and (LinkLag <> 0) and
    IsValueAssigned(TdxGanttTaskPredecessorLinkAssignedValue.LagFormat);
  if AHasLinkLag then
  begin
    ALag := LinkLag / GetDenominator;
    if ALag > 0 then
      Result := Result + '+'
    else
      Result := Result + '-';
    ALag := Abs(ALag);
    AUnit := GetUnit(ALag);
    Result := Format('%s%g%s', [Result, ALag, AUnit],
      TdxCultureInfo.CurrentCulture.FormatSettings);
  end;
end;

function TdxGanttControlTaskPredecessorLink.GetTask: TdxGanttControlTask;
begin
  Result := TdxGanttControlTask(TdxGanttControlTaskPredecessorLinks
    (Owner).Owner);
end;

procedure TdxGanttControlTaskPredecessorLink.SetCrossProject
  (const Value: Boolean);
begin
  if (CrossProject <> Value) or not IsValueAssigned
    (TdxGanttTaskPredecessorLinkAssignedValue.CrossProject) then
  begin
    Include(FAssignedValues, TdxGanttTaskPredecessorLinkAssignedValue.
      CrossProject);
    FCrossProject := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTaskPredecessorLink.SetCrossProjectName
  (const Value: string);
begin
  if (CrossProjectName <> Value) or
    not IsValueAssigned(TdxGanttTaskPredecessorLinkAssignedValue.
    CrossProjectName) then
  begin
    Include(FAssignedValues, TdxGanttTaskPredecessorLinkAssignedValue.
      CrossProjectName);
    FCrossProjectName := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTaskPredecessorLink.SetLagFormat
  (const Value: TdxGanttControlTaskPredecessorLagFormat);
begin
  if (LagFormat <> Value) or not IsValueAssigned
    (TdxGanttTaskPredecessorLinkAssignedValue.LagFormat) then
  begin
    Include(FAssignedValues, TdxGanttTaskPredecessorLinkAssignedValue.
      LagFormat);
    FLagFormat := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTaskPredecessorLink.SetLinkLag(const Value: Integer);
begin
  if (LinkLag <> Value) or not IsValueAssigned
    (TdxGanttTaskPredecessorLinkAssignedValue.LinkLag) then
  begin
    Include(FAssignedValues, TdxGanttTaskPredecessorLinkAssignedValue.LinkLag);
    FLinkLag := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTaskPredecessorLink.SetPredecessorUID
  (const Value: Integer);
begin
  if (FPredecessorUID <> Value) or
    not IsValueAssigned(TdxGanttTaskPredecessorLinkAssignedValue.PredecessorUID)
  then
  begin
    if IsValueAssigned(TdxGanttTaskPredecessorLinkAssignedValue.PredecessorUID)
    then
      Task.Owner.AddDirtyTask(PredecessorUID);
    Include(FAssignedValues, TdxGanttTaskPredecessorLinkAssignedValue.
      PredecessorUID);
    FPredecessorUID := Value;
    if IsValueAssigned(TdxGanttTaskPredecessorLinkAssignedValue.PredecessorUID)
    then
      Task.Owner.AddDirtyTask(PredecessorUID);
    Changed;
  end;
end;

procedure TdxGanttControlTaskPredecessorLink.SetType
  (const Value: TdxGanttControlTaskPredecessorLinkType);
begin
  if (FType <> Value) or not IsValueAssigned
    (TdxGanttTaskPredecessorLinkAssignedValue.&Type) then
  begin
    Include(FAssignedValues, TdxGanttTaskPredecessorLinkAssignedValue.&Type);
    FType := Value;
    Changed;
  end;
end;

function TdxGanttControlTaskPredecessorLink.ToString: string;
var
  AType: string;
  AHasLinkLag: Boolean;
  ATask: TdxGanttControlTask;
begin
  if not IsValueAssigned(TdxGanttTaskPredecessorLinkAssignedValue.PredecessorUID)
  then
    Exit('');
  ATask := TdxGanttControlDataModel(DataModel).Tasks.GetItemByUID
    (PredecessorUID);
  if ATask = nil then
    Exit('');
  Result := IntToStr(ATask.ID);
  case &Type of
    TdxGanttControlTaskPredecessorLinkType.FF:
      AType := cxGetResourceString(@sdxGanttControlTaskPredecessorLinkTypeFF);
    TdxGanttControlTaskPredecessorLinkType.SS:
      AType := cxGetResourceString(@sdxGanttControlTaskPredecessorLinkTypeSS);
    TdxGanttControlTaskPredecessorLinkType.SF:
      AType := cxGetResourceString(@sdxGanttControlTaskPredecessorLinkTypeSF);
  else
    AType := cxGetResourceString(@sdxGanttControlTaskPredecessorLinkTypeFS);
  end;
  AHasLinkLag := IsValueAssigned
    (TdxGanttTaskPredecessorLinkAssignedValue.LinkLag) and (LinkLag <> 0) and
    IsValueAssigned(TdxGanttTaskPredecessorLinkAssignedValue.LagFormat);
  if (IsValueAssigned(TdxGanttTaskPredecessorLinkAssignedValue.&Type) and
    (&Type <> TdxGanttControlTaskPredecessorLinkType.FS)) or AHasLinkLag then
    Result := Result + AType;
  if AHasLinkLag then
    Result := Format('%s%s', [Result, GetLagAsDisplayValue]);
end;

function TdxGanttControlTaskPredecessorLink.IsElapsedLagFormat: Boolean;
begin
  Result := LagFormat
    in [TdxGanttControlTaskPredecessorLagFormat.ElapsedMinutes,
    TdxGanttControlTaskPredecessorLagFormat.ElapsedHours,
    TdxGanttControlTaskPredecessorLagFormat.ElapsedDays,
    TdxGanttControlTaskPredecessorLagFormat.ElapsedWeeks,
    TdxGanttControlTaskPredecessorLagFormat.ElapsedMonths,
    TdxGanttControlTaskPredecessorLagFormat.ElapsedPercent];
end;

function TdxGanttControlTaskPredecessorLink.IsPercentLagFormat: Boolean;
begin
  Result := LagFormat in [TdxGanttControlTaskPredecessorLagFormat.Percent,
    TdxGanttControlTaskPredecessorLagFormat.ElapsedPercent];
end;

{ TdxGanttControlTaskPredecessorLinks }

function TdxGanttControlTaskPredecessorLinks.Append
  : TdxGanttControlTaskPredecessorLink;
begin
  Result := TdxGanttControlTaskPredecessorLink(CreateItem);
  InternalAdd(Result);
end;

procedure TdxGanttControlTaskPredecessorLinks.Clear;
begin
  InternalClear;
end;

function TdxGanttControlTaskPredecessorLinks.GetItemByPredecessorUID
  (AUID: Integer): TdxGanttControlTaskPredecessorLink;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].PredecessorUID = AUID then
      Exit(Items[I]);
end;

function TdxGanttControlTaskPredecessorLinks.CreateItem
  : TdxGanttControlModelElementListItem;
begin
  Result := TdxGanttControlTaskPredecessorLink.Create(Self);
end;

procedure TdxGanttControlTaskPredecessorLinks.DoListChanged
  (const AItem: TdxGanttControlModelElementListItem;
  AAction: TCollectionNotification);
var
  ALink: TdxGanttControlTaskPredecessorLink;
begin
  ALink := TdxGanttControlTaskPredecessorLink(AItem);
  if ALink.IsValueAssigned(TdxGanttTaskPredecessorLinkAssignedValue.
    PredecessorUID) then
    ALink.Task.Owner.AddDirtyTask(ALink.PredecessorUID);
  inherited DoListChanged(AItem, AAction);
end;

function TdxGanttControlTaskPredecessorLinks.GetItem(Index: Integer)
  : TdxGanttControlTaskPredecessorLink;
begin
  Result := TdxGanttControlTaskPredecessorLink(inherited Items[Index])
end;

procedure TdxGanttControlTaskPredecessorLinks.Remove
  (AItem: TdxGanttControlTaskPredecessorLink);
begin
  InternalRemove(AItem);
end;

function TdxGanttControlTaskPredecessorLinks.ToString: string;
var
  I: Integer;
  AValue: string;
begin
  for I := 0 to Count - 1 do
    if Result = '' then
      Result := Items[I].ToString
    else
    begin
      AValue := Items[I].ToString;
      if AValue <> '' then
        Result := Format('%s%s%s',
          [Result, TdxCultureInfo.CurrentCulture.FormatSettings.
          ListSeparator, AValue]);
    end;
end;

function TdxGanttControlTaskPredecessorLinks.ToArray: TArray<Integer>;
var
  AList: TList<Integer>;
  I: Integer;
  ATask: TdxGanttControlTask;
begin
  AList := TList<Integer>.Create;
  try
    for I := 0 to Count - 1 do
      if Items[I].IsValueAssigned
        (TdxGanttTaskPredecessorLinkAssignedValue.PredecessorUID) then
      begin
        ATask := TdxGanttControlDataModel(DataModel)
          .Tasks.GetItemByUID(Items[I].PredecessorUID);
        if ATask <> nil then
          AList.Add(ATask.ID);
      end;
{$IFNDEF DELPHIXE}
    SetLength(Result, AList.Count);
    for I := 0 to AList.Count - 1 do
      Result[I] := AList[I];
{$ELSE}
    Result := AList.ToArray;
{$ENDIF}
  finally
    AList.Free;
  end;
end;

{ TdxGanttControlTaskBaseline }

constructor TdxGanttControlTaskBaseline.Create
  (AOwner: TdxGanttControlModelElement);
begin
  inherited Create(AOwner);
  FTimephasedDataItems := TdxGanttControlTimephasedDataItems.Create(Self);
end;

destructor TdxGanttControlTaskBaseline.Destroy;
begin
  FreeAndNil(FTimephasedDataItems);
  inherited Destroy;
end;

procedure TdxGanttControlTaskBaseline.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlTaskBaseline;
begin
  if Source is TdxGanttControlTaskBaseline then
  begin
    ASource := TdxGanttControlTaskBaseline(Source);
    BeginUpdate;
    try
      TimephasedDataItems := ASource.TimephasedDataItems;
      Interim := ASource.Interim;
      Start := ASource.Start;
      Finish := ASource.Finish;
      Duration := ASource.Duration;
      DurationFormat := ASource.DurationFormat;
      EstimatedDuration := ASource.EstimatedDuration;
      FixedCost := ASource.FixedCost;
      FAssignedValues := ASource.FAssignedValues;
    finally
      EndUpdate;
    end;
  end;
  inherited Assign(Source);
end;

function TdxGanttControlTaskBaseline.IsValueAssigned(const AValue
  : TdxGanttTaskBaselineAssignedValue): Boolean;
begin
  Result := AValue in FAssignedValues;
end;

procedure TdxGanttControlTaskBaseline.ResetValue(const AValue
  : TdxGanttTaskBaselineAssignedValue);
begin
  if IsValueAssigned(AValue) then
  begin
    Exclude(FAssignedValues, AValue);
    Changed;
  end
end;

procedure TdxGanttControlTaskBaseline.DoReset;
begin
  inherited DoReset;
  FTimephasedDataItems.Reset;
  FAssignedValues := [];
end;

procedure TdxGanttControlTaskBaseline.SetDuration(const Value: TDuration);
begin
  if (FDuration <> Value) or not IsValueAssigned
    (TdxGanttTaskBaselineAssignedValue.Duration) then
  begin
    Include(FAssignedValues, TdxGanttTaskBaselineAssignedValue.Duration);
    FDuration := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTaskBaseline.SetDurationFormat
  (const Value: TdxDurationFormat);
begin
  if (DurationFormat <> Value) or not IsValueAssigned
    (TdxGanttTaskBaselineAssignedValue.DurationFormat) then
  begin
    FDurationFormat := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTaskBaseline.SetEstimatedDuration
  (const Value: Boolean);
begin
  if (FEstimatedDuration <> Value) or
    not IsValueAssigned(TdxGanttTaskBaselineAssignedValue.EstimatedDuration)
  then
  begin
    Include(FAssignedValues,
      TdxGanttTaskBaselineAssignedValue.EstimatedDuration);
    FEstimatedDuration := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTaskBaseline.SetFinish(const Value: TDateTime);
begin
  if (Finish <> Value) or not IsValueAssigned
    (TdxGanttTaskBaselineAssignedValue.Finish) then
  begin
    Include(FAssignedValues, TdxGanttTaskBaselineAssignedValue.Finish);
    FFinish := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTaskBaseline.SetFixedCost(const Value: Double);
begin
  if not(SameValue(FixedCost, Value) and
    IsValueAssigned(TdxGanttTaskBaselineAssignedValue.FixedCost)) then
  begin
    Include(FAssignedValues, TdxGanttTaskBaselineAssignedValue.FixedCost);
    FFixedCost := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTaskBaseline.SetInterim(const Value: Boolean);
begin
  if (Interim <> Value) or not IsValueAssigned
    (TdxGanttTaskBaselineAssignedValue.Interim) then
  begin
    Include(FAssignedValues, TdxGanttTaskBaselineAssignedValue.Interim);
    FInterim := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTaskBaseline.SetStart(const Value: TDateTime);
begin
  if (Start <> Value) or not IsValueAssigned
    (TdxGanttTaskBaselineAssignedValue.Start) then
  begin
    Include(FAssignedValues, TdxGanttTaskBaselineAssignedValue.Start);
    FStart := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTaskBaseline.SetTimephasedDataItems
  (const Value: TdxGanttControlTimephasedDataItems);
begin
  TimephasedDataItems.Assign(Value);
end;

{ TdxGanttControlTaskBaselines }

function TdxGanttControlTaskBaselines.Append: TdxGanttControlTaskBaseline;
begin
  Result := TdxGanttControlTaskBaseline(inherited Append);
end;

function TdxGanttControlTaskBaselines.CreateItem
  : TdxGanttControlModelElementListItem;
begin
  Result := TdxGanttControlTaskBaseline.Create(Self);
end;

function TdxGanttControlTaskBaselines.GetItem(Index: Integer)
  : TdxGanttControlTaskBaseline;
begin
  Result := TdxGanttControlTaskBaseline(inherited Items[Index]);
end;

{ TdxGanttControlTask }

constructor TdxGanttControlTask.Create(AOwner: TdxGanttControlModelElement);
begin
  inherited Create(AOwner);
  FDurationAsDisplayValue := Null;
  FLinks := TdxFastList.Create;
end;

destructor TdxGanttControlTask.Destroy;
begin
  FreeAndNil(FLinks);
  FreeAndNil(FPredecessorLinks);
  FreeAndNil(FBaselines);
  FreeAndNil(FExtendedAttributes);
  FreeAndNil(FOutlineCodes);
  FreeAndNil(FTimephasedDataItems);
  FreeAndNil(FEnterpriseExtendedAttributes);
  inherited Destroy;
end;

procedure TdxGanttControlTask.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlTask;
begin
  if Source is TdxGanttControlTask then
  begin
    ASource := TdxGanttControlTask(Source);
    BeginUpdate;
    try
      GUID := ASource.GUID;
      Name := ASource.Name;
      &Type := ASource.&Type;
      Blank := ASource.Blank;
      Created := ASource.Created;
      Contact := ASource.Contact;
      WBS := ASource.WBS;
      WBSLevel := ASource.WBSLevel;
      OutlineNumber := ASource.OutlineNumber;
      OutlineLevel := ASource.OutlineLevel;
      Priority := ASource.Priority;
      Start := ASource.Start;
      Finish := ASource.Finish;
      Duration := ASource.Duration;
      DurationFormat := ASource.DurationFormat;
      Work := ASource.Work;
      Stop := ASource.Stop;
      Resume := ASource.Resume;
      ResumeValid := ASource.ResumeValid;
      EffortDriven := ASource.EffortDriven;
      Recurring := ASource.Recurring;
      OverAllocated := ASource.OverAllocated;
      Estimated := ASource.Estimated;
      Milestone := ASource.Milestone;
      Summary := ASource.Summary;
      Critical := ASource.Critical;
      Subproject := ASource.Subproject;
      SubprojectReadOnly := ASource.SubprojectReadOnly;
      SubprojectName := ASource.SubprojectName;
      ExternalTask := ASource.ExternalTask;
      ExternalTaskProject := ASource.ExternalTaskProject;
      EarlyStart := ASource.EarlyStart;
      EarlyFinish := ASource.EarlyFinish;
      LateStart := ASource.LateStart;
      LateFinish := ASource.LateFinish;
      StartVariance := ASource.StartVariance;
      FinishVariance := ASource.FinishVariance;
      WorkVariance := ASource.WorkVariance;
      FreeSlack := ASource.FreeSlack;
      TotalSlack := ASource.TotalSlack;
      FixedCost := ASource.FixedCost;
      FixedCostAccrual := ASource.FixedCostAccrual;
      PercentComplete := ASource.PercentComplete;
      PercentWorkComplete := ASource.PercentWorkComplete;
      Cost := ASource.Cost;
      OvertimeCost := ASource.OvertimeCost;
      OvertimeWork := ASource.OvertimeWork;
      ActualStart := ASource.ActualStart;
      ActualFinish := ASource.ActualFinish;
      ActualDuration := ASource.ActualDuration;
      ActualCost := ASource.ActualCost;
      ActualOvertimeCost := ASource.ActualOvertimeCost;
      ActualWork := ASource.ActualWork;
      ActualOvertimeWork := ASource.ActualOvertimeWork;
      RegularWork := ASource.RegularWork;
      RemainingDuration := ASource.RemainingDuration;
      RemainingCost := ASource.RemainingCost;
      RemainingWork := ASource.RemainingWork;
      RemainingOvertimeCost := ASource.RemainingOvertimeCost;
      RemainingOvertimeWork := ASource.RemainingOvertimeWork;
      ACWP := ASource.ACWP;
      CV := ASource.CV;
      ConstraintType := ASource.ConstraintType;
      CalendarUID := ASource.CalendarUID;
      ConstraintDate := ASource.ConstraintDate;
      Deadline := ASource.Deadline;
      LevelAssignments := ASource.LevelAssignments;
      LevelingCanSplit := ASource.LevelingCanSplit;
      LevelingDelay := ASource.LevelingDelay;
      LevelingDelayFormat := ASource.LevelingDelayFormat;
      PreLeveledStart := ASource.PreLeveledStart;
      PreLeveledFinish := ASource.PreLeveledFinish;
      Hyperlink := ASource.Hyperlink;
      HyperlinkAddress := ASource.HyperlinkAddress;
      HyperlinkSubAddress := ASource.HyperlinkSubAddress;
      IgnoreResourceCalendar := ASource.IgnoreResourceCalendar;
      Notes := ASource.Notes;
      HideBar := ASource.HideBar;
      Rollup := ASource.Rollup;
      BCWS := ASource.BCWS;
      BCWP := ASource.BCWP;
      PhysicalPercentComplete := ASource.PhysicalPercentComplete;
      EarnedValueMethod := ASource.EarnedValueMethod;
      PredecessorLinks := ASource.FPredecessorLinks;
      ActualWorkProtected := ASource.ActualWorkProtected;
      ActualOvertimeWorkProtected := ASource.ActualOvertimeWorkProtected;
      Baselines := ASource.FBaselines;
      &Published := ASource.&Published;
      StatusManager := ASource.StatusManager;
      CommitmentStart := ASource.CommitmentStart;
      CommitmentFinish := ASource.CommitmentFinish;
      CommitmentType := ASource.CommitmentType;
      ExtendedAttributes := ASource.FExtendedAttributes;
      OutlineCodes := ASource.FOutlineCodes;
      TimephasedDataItems := ASource.FTimephasedDataItems;
      NoteContainsObjects := ASource.NoteContainsObjects;
      EnterpriseExtendedAttributes := ASource.FEnterpriseExtendedAttributes;
      Active := ASource.Active;
      Manual := ASource.Manual;
      StartSlack := ASource.StartSlack;
      ManualFinish := ASource.ManualFinish;
      FreeformDurationFormat := ASource.FreeformDurationFormat;
      ManualDuration := ASource.ManualDuration;
      ManualStart := ASource.ManualStart;
      DisplayAsSummary := ASource.DisplayAsSummary;
      DisplayOnTimeline := ASource.DisplayOnTimeline;
      FinishSlack := ASource.FinishSlack;
      FAssignedValues := ASource.FAssignedValues;
    finally
      EndUpdate;
    end;
  end;
  inherited Assign(Source);
end;

procedure TdxGanttControlTask.DoReset;
begin
  inherited DoReset;
  FLinks.Clear;
  FreeAndNil(FPredecessorLinks);
  FreeAndNil(FBaselines);
  FreeAndNil(FExtendedAttributes);
  FreeAndNil(FOutlineCodes);
  FreeAndNil(FTimephasedDataItems);
  FreeAndNil(FEnterpriseExtendedAttributes);
  FAssignedValues := [];
  Created := Now;
end;

procedure TdxGanttControlTask.SetUID(const Value: Integer);
begin
  inherited SetUID(Value);
  Owner.AddDirtyTask(Value);
end;

function TdxGanttControlTask.GetID: Integer;
begin
  Result := Index;
end;

function TdxGanttControlTask.GetRealCalendar: TdxGanttControlCalendar;
var
  ADataModel: TdxGanttControlDataModel;
begin
  Result := nil;
  ADataModel := TdxGanttControlDataModel(DataModel);
  if IsValueAssigned(TdxGanttTaskAssignedValue.CalendarUID) then
    Result := ADataModel.Calendars.GetCalendarByUID(CalendarUID);
  if Result <> nil then
    Exit;
  Result := ADataModel.ActiveCalendar;
end;

function TdxGanttControlTask.GetRealDuration: string;
var
  ADuration: TdxGanttControlDuration;
begin
  if IsValueAssigned(TdxGanttTaskAssignedValue.Duration) then
    Result := Duration
  else if IsValueAssigned(TdxGanttTaskAssignedValue.Start) and
    IsValueAssigned(TdxGanttTaskAssignedValue.Finish) then
  begin
    ADuration := TdxGanttControlDuration.Create(Start, Finish, RealCalendar,
      RealDurationFormat);
    Result := ADuration.ToString;
  end
  else
    Result := '';
end;

function TdxGanttControlTask.GetRealDurationFormat: TdxDurationFormat;
begin
  if IsValueAssigned(TdxGanttTaskAssignedValue.DurationFormat) then
    Result := DurationFormat
  else
    Result := TdxGanttControlDataModelPropertiesAccess
      (Safe<TdxGanttControlDataModel>.Cast(Owner.DataModel).Properties)
      .GetActualDurationFormat;
end;

function TdxGanttControlTask.GetRealManual: Boolean;
begin
  if IsValueAssigned(TdxGanttTaskAssignedValue.Manual) then
    Result := Manual
  else
    Result := TdxGanttControlDataModel(DataModel)
      .Properties.MarkNewTasksAsManuallyScheduled;
end;

function TdxGanttControlTask.GetResourceName: string;
var
  I: Integer;
  ANames: TArray<string>;
begin
  Result := '';
  ANames := ResourceNames;
  for I := 0 to Length(ANames) - 1 do
    if Result = '' then
      Result := ANames[I]
    else
      Result := Format('%s%s %s',
        [Result, TdxCultureInfo.CurrentCulture.FormatSettings.ListSeparator,
        ANames[I]]);
end;

function TdxGanttControlTask.GetResourceNames: TArray<string>;
var
  I: Integer;
  ADataModel: TdxGanttControlDataModel;
  AResource: TdxGanttControlResource;
  ALength: Integer;
  AResources: TArray<Integer>;
begin
  Result := nil;
  AResources := GetResources;
  if Length(AResources) = 0 then
    Exit;
  ADataModel := TdxGanttControlDataModel(DataModel);
  for I := 0 to Length(AResources) - 1 do
  begin
    AResource := TdxGanttControlResource
      (ADataModel.Resources.GetItemByUID(AResources[I]));
    if AResource <> nil then
    begin
      ALength := Length(Result);
      SetLength(Result, ALength + 1);
      Result[ALength] := AResource.Name;
    end;
  end;
end;

function TdxGanttControlTask.GetResources: TArray<Integer>;
var
  AResult: TList<Integer>;
  ADataModel: TdxGanttControlDataModel;
  I: Integer;
  AResourceUID: Integer;
begin
  AResult := TList<Integer>.Create;
  try
    ADataModel := TdxGanttControlDataModel(DataModel);
    for I := 0 to ADataModel.Assignments.Count - 1 do
    begin
      if ADataModel.Assignments[I].TaskUID = UID then
      begin
        AResourceUID := ADataModel.Assignments[I].ResourceUID;
        if (AResourceUID <= 0) or (AResult.IndexOf(AResourceUID) >= 0) or
          (ADataModel.Resources.GetItemByUID(AResourceUID) = nil) then
          Continue;
        AResult.Add(AResourceUID);
      end;
    end;
{$IFNDEF DELPHIXE}
    SetLength(Result, AResult.Count);
    for I := 0 to AResult.Count - 1 do
      Result[I] := AResult[I];
{$ELSE}
    Result := AResult.ToArray;
{$ENDIF}
  finally
    AResult.Free;
  end;
end;

function TdxGanttControlTask.InternalGetOwner: TdxGanttControlTasks;
begin
  Result := TdxGanttControlTasks(inherited Owner);
end;

function TdxGanttControlTask.GetBaselines: TdxGanttControlTaskBaselines;
begin
  if FBaselines = nil then
    FBaselines := TdxGanttControlTaskBaselines.Create(Self);
  Result := FBaselines;
  Include(FAssignedValues, TdxGanttTaskAssignedValue.Baselines);
end;

function TdxGanttControlTask.GetDurationAsDisplayValue: Variant;
var
  ADurationFormat: TdxDurationFormat;
  AValue: Double;
begin
  if VarIsNull(FDurationAsDisplayValue) then
  begin
    ADurationFormat := RealDurationFormat;
    AValue := TdxGanttControlDuration.GetDurationValue(RealDuration,
      ADurationFormat);
    FDurationAsDisplayValue :=
      Format('%g %s', [RoundTo(AValue, -4),
      TdxGanttControlDuration.GetDurationMeasurementUnit(ADurationFormat,
      AValue <> 1, True)], TdxCultureInfo.CurrentCulture.FormatSettings);
  end;
  Result := FDurationAsDisplayValue;
end;

function TdxGanttControlTask.GetExtendedAttributes
  : TdxGanttControlExtendedAttributeReferences;
begin
  if FExtendedAttributes = nil then
    FExtendedAttributes := TdxGanttControlExtendedAttributeReferences.
      Create(Self);
  Result := FExtendedAttributes;
  Include(FAssignedValues, TdxGanttTaskAssignedValue.ExtendedAttributes);
end;

function TdxGanttControlTask.GetEnterpriseExtendedAttributes
  : TdxGanttControlEnterpriseExtendedAttributes;
begin
  if FEnterpriseExtendedAttributes = nil then
    FEnterpriseExtendedAttributes :=
      TdxGanttControlEnterpriseExtendedAttributes.Create(Self);
  Result := FEnterpriseExtendedAttributes;
  Include(FAssignedValues,
    TdxGanttTaskAssignedValue.EnterpriseExtendedAttributes);
end;

function TdxGanttControlTask.GetOutlineCodes
  : TdxGanttControlOutlineCodeReferences;
begin
  if FOutlineCodes = nil then
    FOutlineCodes := TdxGanttControlOutlineCodeReferences.Create(Self);
  Result := FOutlineCodes;
  Include(FAssignedValues, TdxGanttTaskAssignedValue.OutlineCodes);
end;

function TdxGanttControlTask.GetPredecessorLinks
  : TdxGanttControlTaskPredecessorLinks;
begin
  if FPredecessorLinks = nil then
    FPredecessorLinks := TdxGanttControlTaskPredecessorLinks.Create(Self);
  Result := FPredecessorLinks;
  Include(FAssignedValues, TdxGanttTaskAssignedValue.PredecessorLinks);
end;

function TdxGanttControlTask.GetTimephasedDataItems
  : TdxGanttControlTimephasedDataItems;
begin
  if FTimephasedDataItems = nil then
    FTimephasedDataItems := TdxGanttControlTimephasedDataItems.Create(Self);
  Result := FTimephasedDataItems;
  Include(FAssignedValues, TdxGanttTaskAssignedValue.TimephasedDataItems);
end;

{$REGION 'for internal use'}

function TdxGanttControlTask.GetDurationInfo: string;
begin
  Result := cxGetResourceString(@sdxGanttControlTimelineTaskHintDurationCaption)
    + ' ' + DurationAsDisplayValue;
end;

function TdxGanttControlTask.GetFinishInfo: string;
begin
  if IsValueAssigned(TdxGanttTaskAssignedValue.Finish) then
    Result := cxGetResourceString(@sdxGanttControlTimelineTaskHintFinishCaption)
      + ' ' + FormatDateTime
      (TdxGanttControlUtils.GetShortDateTimeFormat, Finish)
  else
    Result := '';
end;

function TdxGanttControlTask.GetPercentCompleteInfo: string;
begin
  if IsValueAssigned(TdxGanttTaskAssignedValue.PercentComplete) then
    Result := Format('%s %d%%',
      [cxGetResourceString(@sdxGanttControlTimelineTaskHintCompleteCaption),
      PercentComplete])
  else
    Result := '';
end;

function TdxGanttControlTask.GetStartInfo: string;
begin
  if IsValueAssigned(TdxGanttTaskAssignedValue.Start) then
    Result := cxGetResourceString(@sdxGanttControlTimelineTaskHintStartCaption)
      + ' ' + FormatDateTime(TdxGanttControlUtils.GetShortDateTimeFormat, Start)
  else
    Result := '';
end;
{$ENDREGION}

function TdxGanttControlTask.IsValueAssigned(const AValue
  : TdxGanttTaskAssignedValue): Boolean;
begin
  Result := AValue in FAssignedValues;
end;

procedure TdxGanttControlTask.ResetValue(const AValue
  : TdxGanttTaskAssignedValue);
begin
  if IsValueAssigned(AValue) then
  begin
    if AValue in [TdxGanttTaskAssignedValue.Duration,
      TdxGanttTaskAssignedValue.DurationFormat] then
      FDurationAsDisplayValue := Null;
    Exclude(FAssignedValues, AValue);
    case AValue of
      TdxGanttTaskAssignedValue.Baselines:
        FreeAndNil(FBaselines);
      TdxGanttTaskAssignedValue.ExtendedAttributes:
        FreeAndNil(FExtendedAttributes);
      TdxGanttTaskAssignedValue.EnterpriseExtendedAttributes:
        FreeAndNil(FEnterpriseExtendedAttributes);
      TdxGanttTaskAssignedValue.OutlineCodes:
        FreeAndNil(FOutlineCodes);
      TdxGanttTaskAssignedValue.PredecessorLinks:
        FreeAndNil(FPredecessorLinks);
      TdxGanttTaskAssignedValue.TimephasedDataItems:
        FreeAndNil(FTimephasedDataItems);
    end;
    Changed;
  end
end;

procedure TdxGanttControlTask.SetActive(const Value: Boolean);
begin
  if (Active <> Value) or not IsValueAssigned(TdxGanttTaskAssignedValue.Active)
  then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.Active);
    FActive := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetManual(const Value: Boolean);
begin
  if (Manual <> Value) or not IsValueAssigned(TdxGanttTaskAssignedValue.Manual)
  then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.Manual);
    FManual := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetManualDuration(const Value: TDuration);
begin
  if (ManualDuration <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.ManualDuration) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.ManualDuration);
    FManualDuration := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetManualFinish(const Value: TDateTime);
begin
  if (ManualFinish <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.ManualFinish) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.ManualFinish);
    FManualFinish := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetManualStart(const Value: TDateTime);
begin
  if (ManualStart <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.ManualStart) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.ManualStart);
    FManualStart := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetActualCost(const Value: Double);
begin
  if not(SameValue(ActualCost, Value) and
    IsValueAssigned(TdxGanttTaskAssignedValue.ActualCost)) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.ActualCost);
    FActualCost := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetActualDuration(const Value: TDuration);
begin
  if (ActualDuration <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.ActualDuration) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.ActualDuration);
    FActualDuration := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetActualFinish(const Value: TDateTime);
begin
  if (ActualFinish <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.ActualFinish) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.ActualFinish);
    FActualFinish := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetActualOvertimeCost(const Value: Double);
begin
  if not(SameValue(ActualOvertimeCost, Value) and
    IsValueAssigned(TdxGanttTaskAssignedValue.ActualOvertimeCost)) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.ActualOvertimeCost);
    FActualOvertimeCost := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetActualOvertimeWork(const Value: string);
begin
  if (ActualOvertimeWork <> Value) or
    not IsValueAssigned(TdxGanttTaskAssignedValue.ActualOvertimeWork) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.ActualOvertimeWork);
    FActualOvertimeWork := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetActualOvertimeWorkProtected
  (const Value: string);
begin
  if (ActualOvertimeWorkProtected <> Value) or
    not IsValueAssigned(TdxGanttTaskAssignedValue.ActualOvertimeWorkProtected)
  then
  begin
    Include(FAssignedValues,
      TdxGanttTaskAssignedValue.ActualOvertimeWorkProtected);
    FActualOvertimeWorkProtected := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetActualStart(const Value: TDateTime);
begin
  if (ActualStart <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.ActualStart) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.ActualStart);
    FActualStart := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetActualWork(const Value: string);
begin
  if (ActualWork <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.ActualWork) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.ActualWork);
    FActualWork := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetActualWorkProtected(const Value: string);
begin
  if (ActualWorkProtected <> Value) or
    not IsValueAssigned(TdxGanttTaskAssignedValue.ActualWorkProtected) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.ActualWorkProtected);
    FActualWorkProtected := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetACWP(const Value: Double);
begin
  if not(SameValue(ACWP, Value) and
    IsValueAssigned(TdxGanttTaskAssignedValue.ACWP)) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.ACWP);
    FACWP := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetBaselines(const Value
  : TdxGanttControlTaskBaselines);
begin
  if Value = nil then
    ResetValue(TdxGanttTaskAssignedValue.Baselines)
  else
    Baselines.Assign(Value);
end;

procedure TdxGanttControlTask.SetBCWP(const Value: Double);
begin
  if not(SameValue(BCWP, Value) and
    IsValueAssigned(TdxGanttTaskAssignedValue.BCWP)) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.BCWP);
    FBCWP := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetBCWS(const Value: Double);
begin
  if not(SameValue(BCWS, Value) and
    IsValueAssigned(TdxGanttTaskAssignedValue.BCWS)) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.BCWS);
    FBCWS := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetCalendarUID(const Value: Integer);
begin
  if (CalendarUID <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.CalendarUID) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.CalendarUID);
    FCalendarUID := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetCommitmentFinish(const Value: TDateTime);
begin
  if (CommitmentFinish <> Value) or
    not IsValueAssigned(TdxGanttTaskAssignedValue.CommitmentFinish) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.CommitmentFinish);
    FCommitmentFinish := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetCommitmentStart(const Value: TDateTime);
begin
  if (CommitmentStart <> Value) or
    not IsValueAssigned(TdxGanttTaskAssignedValue.CommitmentStart) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.CommitmentStart);
    FCommitmentStart := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetCommitmentType(const Value
  : TdxGanttControlTaskCommitmentType);
begin
  if (CommitmentType <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.CommitmentType) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.CommitmentType);
    FCommitmentType := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetConstraintDate(const Value: TDateTime);
begin
  if (ConstraintDate <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.ConstraintDate) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.ConstraintDate);
    FConstraintDate := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetConstraintType(const Value
  : TdxGanttControlTaskConstraintType);
begin
  if (ConstraintType <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.ConstraintType) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.ConstraintType);
    if FConstraintType = TdxGanttControlTaskConstraintType.AsLateAsPossible then
      Owner.LateAsPossibleTasks.Remove(Self);
    FConstraintType := Value;
    if (ID >= 0) and
      (FConstraintType = TdxGanttControlTaskConstraintType.AsLateAsPossible)
    then
      Owner.LateAsPossibleTasks.Add(Self);
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetContact(const Value: string);
begin
  if (Contact <> Value) or not IsValueAssigned(TdxGanttTaskAssignedValue.Contact)
  then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.Contact);
    FContact := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetCost(const Value: Double);
begin
  if not(SameValue(Cost, Value) and
    IsValueAssigned(TdxGanttTaskAssignedValue.Cost)) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.Cost);
    FCost := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetCreated(const Value: TDateTime);
begin
  if Created <> Value then
  begin
    FCreated := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetCritical(const Value: Boolean);
begin
  if (Critical <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.Critical) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.Critical);
    FCritical := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetCV(const Value: Double);
begin
  if not(SameValue(CV, Value) and IsValueAssigned(TdxGanttTaskAssignedValue.CV))
  then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.CV);
    FCV := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetDeadline(const Value: TDateTime);
begin
  if (Deadline <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.Deadline) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.Deadline);
    FDeadline := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetDisplayAsSummary(const Value: Boolean);
begin
  if (DisplayAsSummary <> Value) or
    not IsValueAssigned(TdxGanttTaskAssignedValue.DisplayAsSummary) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.DisplayAsSummary);
    FDisplayAsSummary := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetDisplayOnTimeline(const Value: Boolean);
begin
  if (DisplayOnTimeline <> Value) or
    not IsValueAssigned(TdxGanttTaskAssignedValue.DisplayOnTimeline) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.DisplayOnTimeline);
    FDisplayOnTimeline := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetDuration(const Value: TDuration);
begin
  if (Duration <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.Duration) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.Duration);
    FDuration := Value;
    FDurationAsDisplayValue := Null;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetDurationFormat(const Value: TdxDurationFormat);
begin
  if (DurationFormat <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.DurationFormat) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.DurationFormat);
    FDurationFormat := Value;
    FDurationAsDisplayValue := Null;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetEarlyFinish(const Value: TDateTime);
begin
  if (EarlyFinish <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.EarlyFinish) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.EarlyFinish);
    FEarlyFinish := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetEarlyStart(const Value: TDateTime);
begin
  if (EarlyStart <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.EarlyStart) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.EarlyStart);
    FEarlyStart := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetEarnedValueMethod
  (const Value: TdxGanttControlTaskEarnedValueMethod);
begin
  if (EarnedValueMethod <> Value) or
    not IsValueAssigned(TdxGanttTaskAssignedValue.EarnedValueMethod) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.EarnedValueMethod);
    FEarnedValueMethod := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetEffortDriven(const Value: Boolean);
begin
  if (EffortDriven <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.EffortDriven) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.EffortDriven);
    FEffortDriven := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetEnterpriseExtendedAttributes
  (const Value: TdxGanttControlEnterpriseExtendedAttributes);
begin
  if Value = nil then
    ResetValue(TdxGanttTaskAssignedValue.EnterpriseExtendedAttributes)
  else
    EnterpriseExtendedAttributes.Assign(Value);
end;

procedure TdxGanttControlTask.SetEstimated(const Value: Boolean);
begin
  if (Estimated <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.Estimated) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.Estimated);
    FEstimated := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetExtendedAttributes
  (const Value: TdxGanttControlExtendedAttributeReferences);
begin
  if Value = nil then
    ResetValue(TdxGanttTaskAssignedValue.ExtendedAttributes)
  else
    ExtendedAttributes.Assign(Value);
end;

procedure TdxGanttControlTask.SetExternalTask(const Value: Boolean);
begin
  if (ExternalTask <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.ExternalTask) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.ExternalTask);
    FExternalTask := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetExternalTaskProject(const Value: string);
begin
  if (ExternalTaskProject <> Value) or
    not IsValueAssigned(TdxGanttTaskAssignedValue.ExternalTaskProject) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.ExternalTaskProject);
    FExternalTaskProject := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetFinish(const Value: TDateTime);
begin
  if (Finish <> Value) or not IsValueAssigned(TdxGanttTaskAssignedValue.Finish)
  then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.Finish);
    FFinish := Value;
    FDurationAsDisplayValue := Null;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetFinishSlack(const Value: Integer);
begin
  if (FinishSlack <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.FinishSlack) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.FinishSlack);
    FFinishSlack := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetFinishVariance(const Value: Integer);
begin
  if (FinishVariance <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.FinishVariance) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.FinishVariance);
    FFinishVariance := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetFixedCost(const Value: Double);
begin
  if not(SameValue(FixedCost, Value) and
    IsValueAssigned(TdxGanttTaskAssignedValue.FixedCost)) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.FixedCost);
    FFixedCost := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetFixedCostAccrual(const Value: Integer);
begin
  if (FixedCostAccrual <> Value) or
    not IsValueAssigned(TdxGanttTaskAssignedValue.FixedCostAccrual) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.FixedCostAccrual);
    FFixedCostAccrual := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetFreeformDurationFormat
  (const Value: TdxDurationFormat);
begin
  if (FreeformDurationFormat <> Value) or
    not IsValueAssigned(TdxGanttTaskAssignedValue.FreeformDurationFormat) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.FreeformDurationFormat);
    FFreeformDurationFormat := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetFreeSlack(const Value: Integer);
begin
  if (FreeSlack <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.FreeSlack) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.FreeSlack);
    FFreeSlack := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetGUID(const Value: string);
begin
  if (GUID <> Value) or not IsValueAssigned(TdxGanttTaskAssignedValue.GUID) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.GUID);
    FGUID := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetHideBar(const Value: Boolean);
begin
  if (HideBar <> Value) or not IsValueAssigned(TdxGanttTaskAssignedValue.HideBar)
  then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.HideBar);
    FHideBar := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetHyperlink(const Value: string);
begin
  if (Hyperlink <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.Hyperlink) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.Hyperlink);
    FHyperlink := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetHyperlinkAddress(const Value: string);
begin
  if (HyperlinkAddress <> Value) or
    not IsValueAssigned(TdxGanttTaskAssignedValue.HyperlinkAddress) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.HyperlinkAddress);
    FHyperlinkAddress := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetHyperlinkSubAddress(const Value: string);
begin
  if (HyperlinkSubAddress <> Value) or
    not IsValueAssigned(TdxGanttTaskAssignedValue.HyperlinkSubAddress) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.HyperlinkSubAddress);
    FHyperlinkSubAddress := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetID(const Value: Integer);
begin
  Index := Value;
end;

procedure TdxGanttControlTask.SetIgnoreResourceCalendar(const Value: Boolean);
begin
  if (IgnoreResourceCalendar <> Value) or
    not IsValueAssigned(TdxGanttTaskAssignedValue.IgnoreResourceCalendar) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.IgnoreResourceCalendar);
    FIgnoreResourceCalendar := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetBlank(const Value: Boolean);
begin
  if (Blank <> Value) or not IsValueAssigned(TdxGanttTaskAssignedValue.Blank)
  then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.Blank);
    FBlank := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetPublished(const Value: Boolean);
begin
  if (&Published <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.&Published) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.&Published);
    FPublished := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetSubproject(const Value: Boolean);
begin
  if (Subproject <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.Subproject) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.Subproject);
    FSubproject := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetSubprojectReadOnly(const Value: Boolean);
begin
  if (SubprojectReadOnly <> Value) or
    not IsValueAssigned(TdxGanttTaskAssignedValue.SubprojectReadOnly) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.SubprojectReadOnly);
    FSubprojectReadOnly := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetLateFinish(const Value: TDateTime);
begin
  if (LateFinish <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.LateFinish) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.LateFinish);
    FLateFinish := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetLateStart(const Value: TDateTime);
begin
  if (LateStart <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.LateStart) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.LateStart);
    FLateStart := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetLevelAssignments(const Value: Boolean);
begin
  if (LevelAssignments <> Value) or
    not IsValueAssigned(TdxGanttTaskAssignedValue.LevelAssignments) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.LevelAssignments);
    FLevelAssignments := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetLevelingCanSplit(const Value: Boolean);
begin
  if (LevelingCanSplit <> Value) or
    not IsValueAssigned(TdxGanttTaskAssignedValue.LevelingCanSplit) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.LevelingCanSplit);
    FLevelingCanSplit := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetLevelingDelay(const Value: Integer);
begin
  if (LevelingDelay <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.LevelingDelay) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.LevelingDelay);
    FLevelingDelay := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetLevelingDelayFormat
  (const Value: TdxDurationFormat);
begin
  if (LevelingDelayFormat <> Value) or
    not IsValueAssigned(TdxGanttTaskAssignedValue.LevelingDelayFormat) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.LevelingDelayFormat);
    FLevelingDelayFormat := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetMilestone(const Value: Boolean);
begin
  if (Milestone <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.Milestone) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.Milestone);
    FMilestone := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetName(const Value: string);
begin
  if (Name <> Value) or not IsValueAssigned(TdxGanttTaskAssignedValue.Name) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.Name);
    FName := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetNoteContainsObjects(const Value: Boolean);
begin
  if (NoteContainsObjects <> Value) or
    not IsValueAssigned(TdxGanttTaskAssignedValue.NoteContainsObjects) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.NoteContainsObjects);
    FNoteContainsObjects := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetNotes(const Value: string);
begin
  if (Notes <> Value) or not IsValueAssigned(TdxGanttTaskAssignedValue.Notes)
  then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.Notes);
    FNotes := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetOutlineCodes(const Value
  : TdxGanttControlOutlineCodeReferences);
begin
  if Value = nil then
    ResetValue(TdxGanttTaskAssignedValue.OutlineCodes)
  else
    OutlineCodes.Assign(Value);
end;

procedure TdxGanttControlTask.SetOutlineLevel(const Value: Integer);
begin
  if (OutlineLevel <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.OutlineLevel) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.OutlineLevel);
    FOutlineLevel := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetOutlineNumber(const Value: string);
begin
  if (OutlineNumber <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.OutlineNumber) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.OutlineNumber);
    FOutlineNumber := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetOverAllocated(const Value: Boolean);
begin
  if (OverAllocated <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.OverAllocated) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.OverAllocated);
    FOverAllocated := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetOvertimeCost(const Value: Double);
begin
  if not(SameValue(OvertimeCost, Value) and
    IsValueAssigned(TdxGanttTaskAssignedValue.OvertimeCost)) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.OvertimeCost);
    FOvertimeCost := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetOvertimeWork(const Value: string);
begin
  if (OvertimeWork <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.OvertimeWork) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.OvertimeWork);
    FOvertimeWork := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetPercentComplete(const Value: Integer);
begin
  if (PercentComplete <> Value) or
    not IsValueAssigned(TdxGanttTaskAssignedValue.PercentComplete) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.PercentComplete);
    FPercentComplete := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetPercentWorkComplete(const Value: Integer);
begin
  if (PercentWorkComplete <> Value) or
    not IsValueAssigned(TdxGanttTaskAssignedValue.PercentWorkComplete) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.PercentWorkComplete);
    FPercentWorkComplete := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetPhysicalPercentComplete(const Value: Integer);
begin
  if (PhysicalPercentComplete <> Value) or
    not IsValueAssigned(TdxGanttTaskAssignedValue.PhysicalPercentComplete) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.PhysicalPercentComplete);
    FPhysicalPercentComplete := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetPredecessorLinks
  (const Value: TdxGanttControlTaskPredecessorLinks);
begin
  if Value = nil then
    ResetValue(TdxGanttTaskAssignedValue.PredecessorLinks)
  else
    PredecessorLinks.Assign(Value);
end;

procedure TdxGanttControlTask.SetPreLeveledFinish(const Value: TDateTime);
begin
  if (PreLeveledFinish <> Value) or
    not IsValueAssigned(TdxGanttTaskAssignedValue.PreLeveledFinish) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.PreLeveledFinish);
    FPreLeveledFinish := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetPreLeveledStart(const Value: TDateTime);
begin
  if (PreLeveledStart <> Value) or
    not IsValueAssigned(TdxGanttTaskAssignedValue.PreLeveledStart) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.PreLeveledStart);
    FPreLeveledStart := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetPriority(const Value: Integer);
begin
  if (Priority <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.Priority) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.Priority);
    FPriority := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetRecurring(const Value: Boolean);
begin
  if (Recurring <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.Recurring) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.Recurring);
    FRecurring := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetRegularWork(const Value: string);
begin
  if (RegularWork <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.RegularWork) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.RegularWork);
    FRegularWork := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetRemainingCost(const Value: Double);
begin
  if not(SameValue(RemainingCost, Value) and
    IsValueAssigned(TdxGanttTaskAssignedValue.RemainingCost)) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.RemainingCost);
    FRemainingCost := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetRemainingDuration(const Value: TDuration);
begin
  if (RemainingDuration <> Value) or
    not IsValueAssigned(TdxGanttTaskAssignedValue.RemainingDuration) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.RemainingDuration);
    FRemainingDuration := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetRemainingOvertimeCost(const Value: Double);
begin
  if not(SameValue(RemainingOvertimeCost, Value) and
    IsValueAssigned(TdxGanttTaskAssignedValue.RemainingOvertimeCost)) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.RemainingOvertimeCost);
    FRemainingOvertimeCost := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetRemainingOvertimeWork(const Value: string);
begin
  if (RemainingOvertimeWork <> Value) or
    not IsValueAssigned(TdxGanttTaskAssignedValue.RemainingOvertimeWork) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.RemainingOvertimeWork);
    FRemainingOvertimeWork := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetRemainingWork(const Value: string);
begin
  if (RemainingWork <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.RemainingWork) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.RemainingWork);
    FRemainingWork := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetResume(const Value: TDateTime);
begin
  if (Resume <> Value) or not IsValueAssigned(TdxGanttTaskAssignedValue.Resume)
  then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.Resume);
    FResume := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetResumeValid(const Value: Boolean);
begin
  if (ResumeValid <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.ResumeValid) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.ResumeValid);
    FResumeValid := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetRollup(const Value: Boolean);
begin
  if (Rollup <> Value) or not IsValueAssigned(TdxGanttTaskAssignedValue.Rollup)
  then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.Rollup);
    FRollup := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetStart(const Value: TDateTime);
begin
  if (Start <> Value) or not IsValueAssigned(TdxGanttTaskAssignedValue.Start)
  then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.Start);
    FStart := Value;
    FDurationAsDisplayValue := Null;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetStartSlack(const Value: Integer);
begin
  if (StartSlack <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.StartSlack) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.StartSlack);
    FStartSlack := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetStartVariance(const Value: Integer);
begin
  if (StartVariance <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.StartVariance) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.StartVariance);
    FStartVariance := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetStatusManager(const Value: string);
begin
  if (StatusManager <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.StatusManager) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.StatusManager);
    FStatusManager := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetStop(const Value: TDateTime);
begin
  if (Stop <> Value) or not IsValueAssigned(TdxGanttTaskAssignedValue.Stop) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.Stop);
    FStop := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetSubprojectName(const Value: string);
begin
  if (SubprojectName <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.SubprojectName) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.SubprojectName);
    FSubprojectName := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetSummary(const Value: Boolean);
begin
  if (Summary <> Value) or not IsValueAssigned(TdxGanttTaskAssignedValue.Summary)
  then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.Summary);
    FSummary := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetTimephasedDataItems
  (const Value: TdxGanttControlTimephasedDataItems);
begin
  if Value = nil then
    ResetValue(TdxGanttTaskAssignedValue.TimephasedDataItems)
  else
    TimephasedDataItems.Assign(Value);
end;

procedure TdxGanttControlTask.SetTotalSlack(const Value: Integer);
begin
  if (TotalSlack <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.TotalSlack) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.TotalSlack);
    FTotalSlack := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetType(const Value: TdxGanttControlTaskType);
begin
  if (&Type <> Value) or not IsValueAssigned(TdxGanttTaskAssignedValue.&Type)
  then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.&Type);
    FType := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetWBS(const Value: string);
begin
  if (WBS <> Value) or not IsValueAssigned(TdxGanttTaskAssignedValue.WBS) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.WBS);
    FWBS := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetWBSLevel(const Value: string);
begin
  if (WBSLevel <> Value) or not IsValueAssigned
    (TdxGanttTaskAssignedValue.WBSLevel) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.WBSLevel);
    FWBSLevel := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetWork(const Value: string);
begin
  if (Work <> Value) or not IsValueAssigned(TdxGanttTaskAssignedValue.Work) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.Work);
    FWork := Value;
    Changed;
  end;
end;

procedure TdxGanttControlTask.SetWorkVariance(const Value: Double);
begin
  if not(SameValue(WorkVariance, Value) and
    IsValueAssigned(TdxGanttTaskAssignedValue.WorkVariance)) then
  begin
    Include(FAssignedValues, TdxGanttTaskAssignedValue.WorkVariance);
    FWorkVariance := Value;
    Changed;
  end;
end;

{ TdxGanttControlTasks }

constructor TdxGanttControlTasks.Create(ADataModel
  : TdxGanttControlCustomDataModel);
begin
  inherited Create(ADataModel);
  FDirtyTasks := TList<Integer>.Create;
  FLateAsPossibleTasks := TdxFastList.Create;
end;

destructor TdxGanttControlTasks.Destroy;
begin
  FreeAndNil(FLateAsPossibleTasks);
  FreeAndNil(FDirtyTasks);
  inherited Destroy;
end;

procedure TdxGanttControlTasks.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Count = 0 then
    CreateProjectSummary;
end;

function TdxGanttControlTasks.Append: TdxGanttControlTask;
begin
  Result := TdxGanttControlTask.Create(Self);
  InternalAdd(Result);
end;

function TdxGanttControlTasks.AppendEmpty: TdxGanttControlTask;
begin
  Result := Append;
  Result.Blank := True;
end;

function TdxGanttControlTasks.Insert(Index: Integer): TdxGanttControlTask;
begin
  BeginUpdate;
  try
    while Count < Index do
      AppendEmpty;
    Result := TdxGanttControlTask.Create(Self);
    InternalAdd(Result);
    Result.Index := Index;
  finally
    EndUpdate;
  end;
end;

procedure TdxGanttControlTasks.InsertEmpty(Index, ACount: Integer);
var
  I: Integer;
  AItem: TdxGanttControlTask;
begin
  for I := Index to Index + ACount - 1 do
  begin
    AItem := Insert(I);
    AItem.Blank := True;
  end;
end;

procedure TdxGanttControlTasks.Delete(Index: Integer);
begin
  InternalRemove(Items[Index]);
end;

function TdxGanttControlTasks.GetItemByUID(const AUID: Integer)
  : TdxGanttControlTask;
begin
  Result := TdxGanttControlTask(inherited GetItemByUID(AUID));
end;

function TdxGanttControlTasks.CreateItem: TdxGanttControlModelElementListItem;
begin
  Result := TdxGanttControlTask.Create(Self);
end;

procedure TdxGanttControlTasks.CreateProjectSummary;
var
  ATask: TdxGanttControlTask;
begin
  ATask := Append;
  ATask.OutlineLevel := 0;
  ATask.Summary := True;
  ATask.SetUID(0);
  ResetAutogeneratedUID;
end;

procedure TdxGanttControlTasks.DoItemChanged
  (AItem: TdxGanttControlModelElementListItem);
begin
  TdxGanttControlDataModelAccess(DataModel).TasksChangedHandler(Self, AItem);
end;

procedure TdxGanttControlTasks.DoReset;
begin
  inherited DoReset;
  InternalClear;
  CreateProjectSummary;
end;

procedure TdxGanttControlTasks.InternalAdd
  (AItem: TdxGanttControlModelElementListItem);
begin
  inherited InternalAdd(AItem);
  if TdxGanttControlTask(AItem).IsValueAssigned
    (TdxGanttTaskAssignedValue.ConstraintType) and
    (TdxGanttControlTask(AItem)
    .ConstraintType = TdxGanttControlTaskConstraintType.AsLateAsPossible) then
    FLateAsPossibleTasks.Add(AItem);
end;

procedure TdxGanttControlTasks.InternalClear;
begin
  inherited InternalClear;
  FDirtyTasks.Clear;
  FLateAsPossibleTasks.Clear;
end;

procedure TdxGanttControlTasks.InternalExtract
  (AItem: TdxGanttControlModelElementListItem);
begin
  inherited InternalExtract(AItem);
  FLateAsPossibleTasks.Remove(AItem);
end;

procedure TdxGanttControlTasks.InternalInsert(AIndex: Integer;
  AItem: TdxGanttControlModelElementListItem);
begin
  inherited InternalInsert(AIndex, AItem);
  if TdxGanttControlTask(AItem).IsValueAssigned
    (TdxGanttTaskAssignedValue.ConstraintType) and
    (TdxGanttControlTask(AItem)
    .ConstraintType = TdxGanttControlTaskConstraintType.AsLateAsPossible) then
    FLateAsPossibleTasks.Add(AItem);
end;

procedure TdxGanttControlTasks.InternalRemove
  (AItem: TdxGanttControlModelElementListItem);
begin
  inherited InternalRemove(AItem);
  FLateAsPossibleTasks.Remove(AItem);
end;

procedure TdxGanttControlTasks.AddDirtyTask(AUID: Integer);
var
  AIndex: Integer;
begin
  if not FDirtyTasks.BinarySearch(AUID, AIndex) then
    FDirtyTasks.Insert(AIndex, AUID);
end;

procedure TdxGanttControlTasks.CalculateDirtyTask(ATask: TdxGanttControlTask);
var
  I: Integer;
  AUID: Integer;
begin
  if not IsDirtyTask(ATask) then
    Exit;
  AUID := ATask.UID;
  if ATask <> nil then
  begin
    ATask.Links.Clear;
    for I := 0 to Count - 1 do
      if Items[I].IsValueAssigned(TdxGanttTaskAssignedValue.PredecessorLinks)
      then
        if Items[I].PredecessorLinks.GetItemByPredecessorUID(AUID) <> nil then
          ATask.Links.Add(Items[I]);
  end;
  FDirtyTasks.BinarySearch(AUID, I);
  FDirtyTasks.Delete(I);
end;

procedure TdxGanttControlTasks.CalculateDirtyTasks;
var
  I, J: Integer;
  ATask, APredecessor: TdxGanttControlTask;
begin
  if FDirtyTasks.Count = 0 then
    Exit;
  for I := 0 to Count - 1 do
    Items[I].Links.Clear;
  for I := 0 to Count - 1 do
  begin
    ATask := Items[I];
    if ATask.IsValueAssigned(TdxGanttTaskAssignedValue.PredecessorLinks) then
    begin
      for J := 0 to ATask.PredecessorLinks.Count - 1 do
      begin
        if ATask.PredecessorLinks[J].IsValueAssigned
          (TdxGanttTaskPredecessorLinkAssignedValue.PredecessorUID) then
        begin
          APredecessor :=
            GetItemByUID(ATask.PredecessorLinks[J].PredecessorUID);
          if APredecessor <> nil then
            APredecessor.Links.Add(ATask);
        end;
      end;
    end;
  end;
  FDirtyTasks.Clear;
end;

function TdxGanttControlTasks.IsDirtyTask(ATask: TdxGanttControlTask): Boolean;
var
  AIndex: Integer;
begin
  Result := FDirtyTasks.BinarySearch(ATask.UID, AIndex);
end;

function TdxGanttControlTasks.GetItem(Index: Integer): TdxGanttControlTask;
begin
  Result := TdxGanttControlTask(inherited Items[Index]);
end;

{ TdxGanttControlWBSMask }

procedure TdxGanttControlWBSMask.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlWBSMask;
begin
  if Source is TdxGanttControlWBSMask then
  begin
    ASource := TdxGanttControlWBSMask(Source);
    BeginUpdate;
    try
      Level := ASource.Level;
      &Type := ASource.&Type;
      Length := ASource.Length;
      Separator := ASource.Separator;
    finally
      EndUpdate;
    end;
  end;
  inherited Assign(Source);
end;

procedure TdxGanttControlWBSMask.DoReset;
begin
  FType := TdxGanttControlWBSMaskType.Numbers;
  FLength := 0;
  FSeparator := '.';
end;

function TdxGanttControlWBSMask.InternalGetOwner: TdxGanttControlWBSMasks;
begin
  Result := TdxGanttControlWBSMasks(inherited Owner);
end;

function TdxGanttControlWBSMask.GetLevel: Integer;
begin
  Result := Index + 1;
end;

procedure TdxGanttControlWBSMask.SetLength(const Value: Integer);
begin
  if Length <> Value then
  begin
    FLength := Value;
    Changed;
  end;
end;

procedure TdxGanttControlWBSMask.SetLevel(const Value: Integer);
begin
  Index := Value - 1;
end;

procedure TdxGanttControlWBSMask.SetSeparator(const Value: string);
begin
  if Separator <> Value then
  begin
    FSeparator := Value;
    Changed;
  end;
end;

procedure TdxGanttControlWBSMask.SetType(const Value
  : TdxGanttControlWBSMaskType);
begin
  if &Type <> Value then
  begin
    FType := Value;
    Changed;
  end;
end;

{ TdxGanttControlWBSMasks }

procedure TdxGanttControlWBSMasks.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlWBSMasks;
begin
  BeginUpdate;
  try
    if Source is TdxGanttControlWBSMasks then
    begin
      ASource := TdxGanttControlWBSMasks(Source);
      VerifyUniqueCodes := ASource.VerifyUniqueCodes;
      GenerateCodes := ASource.GenerateCodes;
      Prefix := ASource.Prefix;
      FAssignedValues := ASource.FAssignedValues;
    end;
    inherited Assign(Source);
  finally
    EndUpdate;
  end;
end;

function TdxGanttControlWBSMasks.Append: TdxGanttControlWBSMask;
begin
  Result := TdxGanttControlWBSMask(CreateItem);
  InternalAdd(Result);
end;

function TdxGanttControlWBSMasks.CreateItem
  : TdxGanttControlModelElementListItem;
begin
  Result := TdxGanttControlWBSMask.Create(Self);
end;

procedure TdxGanttControlWBSMasks.DoReset;
begin
  inherited DoReset;
  FAssignedValues := [];
end;

function TdxGanttControlWBSMasks.GetItem(Index: Integer)
  : TdxGanttControlWBSMask;
begin
  Result := TdxGanttControlWBSMask(inherited Items[Index]);
end;

function TdxGanttControlWBSMasks.GetNextLevel: Integer;
begin
  if Count = 0 then
    Result := 1
  else
    Result := Items[Count - 1].Level + 1;
end;

procedure TdxGanttControlWBSMasks.Remove(AItem: TdxGanttControlWBSMask);
begin
  InternalRemove(AItem);
end;

function TdxGanttControlWBSMasks.IsValueAssigned(const AValue
  : TdxGanttWBSMasksAssignedValue): Boolean;
begin
  Result := AValue in FAssignedValues;
end;

procedure TdxGanttControlWBSMasks.ResetValue(const AValue
  : TdxGanttWBSMasksAssignedValue);
begin
  if IsValueAssigned(AValue) then
  begin
    Exclude(FAssignedValues, AValue);
    Changed;
  end
end;

procedure TdxGanttControlWBSMasks.SetGenerateCodes(const Value: Boolean);
begin
  if (GenerateCodes <> Value) or not IsValueAssigned
    (TdxGanttWBSMasksAssignedValue.GenerateCodes) then
  begin
    Include(FAssignedValues, TdxGanttWBSMasksAssignedValue.GenerateCodes);
    FGenerateCodes := Value;
    Changed;
  end;
end;

procedure TdxGanttControlWBSMasks.SetPrefix(const Value: string);
begin
  if (Prefix <> Value) or not IsValueAssigned
    (TdxGanttWBSMasksAssignedValue.Prefix) then
  begin
    Include(FAssignedValues, TdxGanttWBSMasksAssignedValue.Prefix);
    FPrefix := Value;
    Changed;
  end;
end;

procedure TdxGanttControlWBSMasks.SetVerifyUniqueCodes(const Value: Boolean);
begin
  if (VerifyUniqueCodes <> Value) or
    not IsValueAssigned(TdxGanttWBSMasksAssignedValue.VerifyUniqueCodes) then
  begin
    Include(FAssignedValues, TdxGanttWBSMasksAssignedValue.VerifyUniqueCodes);
    FVerifyUniqueCodes := Value;
    Changed;
  end;
end;

end.
