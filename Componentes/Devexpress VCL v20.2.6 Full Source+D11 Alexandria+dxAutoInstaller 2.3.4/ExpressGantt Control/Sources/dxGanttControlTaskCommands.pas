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

unit dxGanttControlTaskCommands;

{$I cxVer.inc}
{$I dxGanttControl.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes, System.Contnrs,
{$ENDIF}
  Types, SysUtils, Classes, Graphics, ImgList, Windows, Controls, Dialogs,
  Generics.Defaults, Generics.Collections, Forms, StdCtrls, Variants,
  dxCoreClasses, cxGraphics, cxCustomCanvas, cxGeometry, cxClasses, cxControls,
  cxVariants, cxEdit, cxDrawTextUtils,
  dxGanttControlCustomClasses,
  dxGanttControlCustomDataModel,
  dxGanttControlDataModel,
  dxGanttControlTasks,
  dxGanttControlAssignments,
  dxGanttControlResources,
  dxGanttControlCalendars,
  dxGanttControlCommands;

type
  { TdxGanttControlTaskHistoryItem }

  TdxGanttControlTaskHistoryItem = class abstract(TdxGanttControlHistoryItem)
  strict private
    FTask: TdxGanttControlTask;
  protected
    property Task: TdxGanttControlTask read FTask;
  public
    constructor Create(AHistory: TdxGanttControlHistory;
      ATask: TdxGanttControlTask); reintroduce; virtual;
  end;

  { TdxGanttControlTaskMakeNotNullHistoryItem }

  TdxGanttControlTaskMakeNotNullHistoryItem = class
    (TdxGanttControlTaskHistoryItem)
  strict private
    FIsNull: Boolean;
  protected
    procedure DoRedo; override;
    procedure DoUndo; override;
  public
    constructor Create(AHistory: TdxGanttControlHistory;
      ATask: TdxGanttControlTask); override;
  end;

  { TdxGanttControlTaskSetAssignedValueHistoryItem }

  TdxGanttControlTaskSetAssignedValueHistoryItem = class
    (TdxGanttControlTaskHistoryItem)
  protected
    FAssignedValue: TdxGanttTaskAssignedValue;
    procedure DoUndo; override;
  end;

  { TdxGanttControlTaskPredecessorLinkSetAssignedValueHistoryItem }

  TdxGanttControlTaskPredecessorLinkSetAssignedValueHistoryItem = class
    (TdxGanttControlTaskPredecessorLinkHistoryItem)
  protected
    FAssignedValue: TdxGanttTaskPredecessorLinkAssignedValue;
    procedure DoUndo; override;
  end;

  { TdxGanttControlTaskResetAssignedValueHistoryItem }

  TdxGanttControlTaskResetAssignedValueHistoryItem = class
    (TdxGanttControlTaskHistoryItem)
  protected
    FAssignedValue: TdxGanttTaskAssignedValue;
    procedure DoRedo; override;
  end;

  { TdxGanttControlTaskPredecessorLinkResetAssignedValueHistoryItem }

  TdxGanttControlTaskPredecessorLinkResetAssignedValueHistoryItem = class
    (TdxGanttControlTaskPredecessorLinkHistoryItem)
  protected
    FAssignedValue: TdxGanttTaskPredecessorLinkAssignedValue;
    procedure DoRedo; override;
  end;

  { TdxGanttControlChangeTaskPropertyHistoryItem }

  TdxGanttControlChangeTaskPropertyHistoryItem = class abstract
    (TdxGanttControlTaskHistoryItem)
  protected
    FNewValue: Variant;
    FOldValue: Variant;

    function GetValue: Variant; virtual; abstract;
    procedure DoSetValue(const Value: Variant); virtual; abstract;
    procedure SetValue(const Value: Variant);

    procedure DoRedo; override;
    procedure DoUndo; override;
  end;

  TdxGanttControlChangeTaskPropertyHistoryItemClass = class of
    TdxGanttControlChangeTaskPropertyHistoryItem;

  { TdxGanttControlChangeTaskOutlineLevelHistoryItem }

  TdxGanttControlChangeTaskOutlineLevelHistoryItem = class
    (TdxGanttControlChangeTaskPropertyHistoryItem)
  protected
    function GetValue: Variant; override;
    procedure DoSetValue(const Value: Variant); override;
  end;

  { TdxGanttControlChangeTaskSummaryHistoryItem }

  TdxGanttControlChangeTaskSummaryHistoryItem = class
    (TdxGanttControlChangeTaskPropertyHistoryItem)
  protected
    function GetValue: Variant; override;
    procedure DoSetValue(const Value: Variant); override;
  end;

  { TdxGanttControlChangeTaskNameHistoryItem }

  TdxGanttControlChangeTaskNameHistoryItem = class
    (TdxGanttControlChangeTaskPropertyHistoryItem)
  protected
    function GetValue: Variant; override;
    procedure DoSetValue(const Value: Variant); override;
  end;

  { TdxGanttControlChangeTaskDisplayOnTimelineHistoryItem }

  TdxGanttControlChangeTaskDisplayOnTimelineHistoryItem = class
    (TdxGanttControlChangeTaskPropertyHistoryItem)
  protected
    function GetValue: Variant; override;
    procedure DoSetValue(const Value: Variant); override;
  end;

  { TdxGanttControlChangeTaskCalendarUIDHistoryItem }

  TdxGanttControlChangeTaskCalendarUIDHistoryItem = class
    (TdxGanttControlChangeTaskPropertyHistoryItem)
  protected
    function GetValue: Variant; override;
    procedure DoSetValue(const Value: Variant); override;
  end;

  { TdxGanttControlChangeTaskPercentCompleteHistoryItem }

  TdxGanttControlChangeTaskPercentCompleteHistoryItem = class
    (TdxGanttControlChangeTaskPropertyHistoryItem)
  protected
    function GetValue: Variant; override;
    procedure DoSetValue(const Value: Variant); override;
  end;

  { TdxGanttControlChangeTaskWorkVarianceHistoryItem }

  TdxGanttControlChangeTaskWorkVarianceHistoryItem = class
    (TdxGanttControlChangeTaskPropertyHistoryItem)
  protected
    function GetValue: Variant; override;
    procedure DoSetValue(const Value: Variant); override;
  end;

  { TdxGanttControlChangeTaskDurationFormatHistoryItem }

  TdxGanttControlChangeTaskDurationFormatHistoryItem = class
    (TdxGanttControlChangeTaskPropertyHistoryItem)
  protected
    function GetValue: Variant; override;
    procedure DoSetValue(const Value: Variant); override;
  end;

  { TdxGanttControlChangeTaskEstimatedHistoryItem }

  TdxGanttControlChangeTaskEstimatedHistoryItem = class
    (TdxGanttControlChangeTaskPropertyHistoryItem)
  protected
    function GetValue: Variant; override;
    procedure DoSetValue(const Value: Variant); override;
  end;

  { TdxGanttControlChangeTaskConstraintTypeHistoryItem }

  TdxGanttControlChangeTaskConstraintTypeHistoryItem = class
    (TdxGanttControlChangeTaskPropertyHistoryItem)
  protected
    function GetValue: Variant; override;
    procedure DoSetValue(const Value: Variant); override;
  end;

  { TdxGanttControlChangeTaskConstraintDateHistoryItem }

  TdxGanttControlChangeTaskConstraintDateHistoryItem = class
    (TdxGanttControlChangeTaskPropertyHistoryItem)
  protected
    function GetValue: Variant; override;
    procedure DoSetValue(const Value: Variant); override;
  end;

  { TdxGanttControlChangeManualHistoryItem }

  TdxGanttControlChangeManualHistoryItem = class
    (TdxGanttControlChangeTaskPropertyHistoryItem)
  protected
    function GetValue: Variant; override;
    procedure DoSetValue(const Value: Variant); override;
  end;

  { TdxGanttControlChangeTaskMilestoneHistoryItem }

  TdxGanttControlChangeTaskMilestoneHistoryItem = class
    (TdxGanttControlChangeTaskPropertyHistoryItem)
  protected
    function GetValue: Variant; override;
    procedure DoSetValue(const Value: Variant); override;
  end;

  { TdxGanttControlChangeTaskStartHistoryItem }

  TdxGanttControlChangeTaskStartHistoryItem = class
    (TdxGanttControlChangeTaskPropertyHistoryItem)
  protected
    function GetValue: Variant; override;
    procedure DoSetValue(const Value: Variant); override;
  end;

  { TdxGanttControlChangeTaskFinishHistoryItem }

  TdxGanttControlChangeTaskFinishHistoryItem = class
    (TdxGanttControlChangeTaskPropertyHistoryItem)
  protected
    function GetValue: Variant; override;
    procedure DoSetValue(const Value: Variant); override;
  end;

  { TdxGanttControlChangeTaskDurationHistoryItem }

  TdxGanttControlChangeTaskDurationHistoryItem = class
    (TdxGanttControlChangeTaskPropertyHistoryItem)
  protected
    function GetValue: Variant; override;
    procedure DoSetValue(const Value: Variant); override;
  end;

  { TdxGanttControlChangeTaskPredecessorLinkPropertyHistoryItem }

  TdxGanttControlChangeTaskPredecessorLinkPropertyHistoryItem = class abstract
    (TdxGanttControlChangeTaskPropertyHistoryItem)
  strict private
    FLink: TdxGanttControlTaskPredecessorLink;
  protected
    property Link: TdxGanttControlTaskPredecessorLink read FLink;
  public
    constructor Create(AHistory: TdxGanttControlHistory;
      ALink: TdxGanttControlTaskPredecessorLink); reintroduce; virtual;
  end;

  TdxGanttControlChangeTaskPredecessorLinkPropertyHistoryItemClass = class of
    TdxGanttControlChangeTaskPredecessorLinkPropertyHistoryItem;

  { TdxGanttControlChangeTaskPredecessorLinkTypeHistoryItem }

  TdxGanttControlChangeTaskPredecessorLinkTypeHistoryItem = class
    (TdxGanttControlChangeTaskPredecessorLinkPropertyHistoryItem)
  protected
    function GetValue: Variant; override;
    procedure DoSetValue(const Value: Variant); override;
  end;

  { TdxGanttControlChangeTaskPredecessorLinkLagHistoryItem }

  TdxGanttControlChangeTaskPredecessorLinkLagHistoryItem = class
    (TdxGanttControlChangeTaskPredecessorLinkPropertyHistoryItem)
  protected
    function GetValue: Variant; override;
    procedure DoSetValue(const Value: Variant); override;
  end;

  { TdxGanttControlChangeTaskPredecessorLinkLagFormatHistoryItem }

  TdxGanttControlChangeTaskPredecessorLinkLagFormatHistoryItem = class
    (TdxGanttControlChangeTaskPredecessorLinkPropertyHistoryItem)
  protected
    function GetValue: Variant; override;
    procedure DoSetValue(const Value: Variant); override;
  end;

  { TdxGanttControlTaskCustomCommand }

  TdxGanttControlTaskCustomCommand = class abstract(TdxGanttControlCommand)
  strict private
    FTask: TdxGanttControlTask;
  protected
    procedure DoSetAssignedValue(AValue: TdxGanttTaskAssignedValue);

    procedure BeginUpdate; override;
    procedure EndUpdate; override;

    property Task: TdxGanttControlTask read FTask;
  public
    constructor Create(AControl: TdxGanttControlBase;
      ATask: TdxGanttControlTask); reintroduce; virtual;
  end;

  { TdxGanttControlDeleteTaskCommand }

  TdxGanttControlDeleteTaskCommand = class(TdxGanttControlTaskCustomCommand)
  strict private
    FExecuteNeeded: Boolean;
    FIsLast: Boolean;
    FParentSummary: TdxGanttControlTask;
    FRaiseConfirmation: Boolean;
    procedure DeleteAssignments(ATask: TdxGanttControlTask);
    procedure DeleteTaskCore(AIndex: Integer);
    procedure CheckLinks(ATask: TdxGanttControlTask;
      ADeletedTasks: TList<TdxGanttControlTask>);
  protected
    procedure AfterExecute; override;
    procedure BeforeExecute; override;
    procedure BeginUpdate; override;
    procedure DoExecute; override;
    procedure EndUpdate; override;
    property RaiseConfirmation: Boolean read FRaiseConfirmation
      write FRaiseConfirmation;
  public
    constructor Create(AControl: TdxGanttControlBase;
      ATask: TdxGanttControlTask); override;
    function Enabled: Boolean; override;
  end;

  { TdxGanttControlSetTaskNotNullCommand }

  TdxGanttControlSetTaskNotNullCommand = class(TdxGanttControlTaskCustomCommand)
  protected
    procedure AfterExecute; override;
    procedure BeforeExecute; override;
    procedure DoExecute; override;
  public
    function Enabled: Boolean; override;
  end;

  { TdxGanttControlChangeTaskCommand }

  TdxGanttControlChangeTaskCommand = class(TdxGanttControlTaskCustomCommand)
  strict private
    FCommands: TdxFastObjectList;
    FNewTask: TdxGanttControlTask;
  protected
    procedure DoExecute; override;
  public
    constructor Create(AControl: TdxGanttControlBase;
      ATask, ANewTask: TdxGanttControlTask); reintroduce;
    destructor Destroy; override;
  end;

  { TdxGanttControlTaskCommand }

  TdxGanttControlTaskCommand = class abstract(TdxGanttControlTaskCustomCommand)
  protected
    procedure BeforeExecute; override;

    procedure MakeTaskNotNull; overload;
    procedure MakeTaskNotNull(ATask: TdxGanttControlTask); overload;
    procedure SetTaskMode;
  end;

  { TdxGanttControlSetTaskModeCommand }

  TdxGanttControlSetTaskModeCommand = class abstract(TdxGanttControlTaskCommand)
  protected
    function IsManual: Boolean; virtual;
    procedure DoExecute; override;

    class function CreateCommand(AControl: TdxGanttControlBase;
      ATask: TdxGanttControlTask): TdxGanttControlSetTaskModeCommand; static;
  public
    function Enabled: Boolean; override;
  end;

  { TdxGanttControlSetTaskManuallyScheduleModeCommand }

  TdxGanttControlSetTaskManuallyScheduleModeCommand = class
    (TdxGanttControlSetTaskModeCommand)
  protected
    function IsManual: Boolean; override;
  end;

  { TdxGanttControlSetTaskAutoScheduleModeCommand }

  TdxGanttControlSetTaskAutoScheduleModeCommand = class
    (TdxGanttControlSetTaskModeCommand)
  protected
    procedure DoExecute; override;
    class procedure SetBasicStartAndFinish(AControl: TdxGanttControlBase;
      ATask: TdxGanttControlTask); static;
  end;

  { TdxGanttControlChangeTaskPropertyCommand }

  TdxGanttControlChangeTaskPropertyCommand = class abstract
    (TdxGanttControlTaskCommand)
  strict private
    FNewValue: Variant;
    FOldIsAssigned: Boolean;
  protected
    procedure BeforeExecute; override;
    procedure DoExecute; override;

    function GetAssignedValue: TdxGanttTaskAssignedValue; virtual; abstract;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeTaskPropertyHistoryItemClass; virtual; abstract;
    function GetValue: Variant; virtual;
    function GetValidValue: Variant;
    function HasAssignedValue: Boolean; virtual;
    procedure SetAssignedValue;
    procedure SetValue; virtual;
    procedure ResetAssignedValue; overload;
    procedure ResetAssignedValue(AAssignedValue
      : TdxGanttTaskAssignedValue); overload;
    function ValidateValue(const AValue: Variant): Variant; virtual;

    function IsNewValueValid: Boolean; virtual;

    property NewValue: Variant read FNewValue;
    property OldIsAssigned: Boolean read FOldIsAssigned;
  public
    constructor Create(AControl: TdxGanttControlBase;
      ATask: TdxGanttControlTask; const ANewValue: Variant);
      reintroduce; virtual;
    function Enabled: Boolean; override;
  end;

  TdxGanttControlChangeTaskPropertyCommandClass = class of
    TdxGanttControlChangeTaskPropertyCommand;

  { TdxGanttControlChangeTaskOutlineLevelCommand }

  TdxGanttControlChangeTaskOutlineLevelCommand = class abstract
    (TdxGanttControlChangeTaskPropertyCommand)
  strict private
    FPreviousSummary: TdxGanttControlTask;
  protected
    procedure AfterExecute; override;
    procedure BeforeExecute; override;
    function GetAssignedValue: TdxGanttTaskAssignedValue; override;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeTaskPropertyHistoryItemClass; override;

    procedure CheckLinks(ATask: TdxGanttControlTask);
    function GetPreviousTask: TdxGanttControlTask;
    function GetSubTasks(ARecursive: Boolean = False)
      : TList<TdxGanttControlTask>;
    procedure SetTaskSummary(ATask: TdxGanttControlTask; ANewValue: Boolean);
    procedure SetTaskAutoScheduleMode(ATask: TdxGanttControlTask);
  end;

  { TdxGanttControlIncreaseTaskOutlineLevelCommand }

  TdxGanttControlIncreaseTaskOutlineLevelCommand = class
    (TdxGanttControlChangeTaskOutlineLevelCommand)
  strict private
    FSubTasks: TList<TdxGanttControlTask>;
  protected
    procedure AfterExecute; override;
    procedure BeforeExecute; override;
  public
    constructor Create(AControl: TdxGanttControlBase;
      ATask: TdxGanttControlTask); reintroduce;
    destructor Destroy; override;
    function Enabled: Boolean; override;
  end;

  { TdxGanttControlDecreaseTaskOutlineLevelCommand }

  TdxGanttControlDecreaseTaskOutlineLevelCommand = class
    (TdxGanttControlChangeTaskOutlineLevelCommand)
  protected
    procedure AfterExecute; override;
    procedure BeforeExecute; override;
  public
    constructor Create(AControl: TdxGanttControlBase;
      ATask: TdxGanttControlTask); reintroduce;
    function Enabled: Boolean; override;
  end;

  { TdxGanttControlChangeTaskSummaryCommand }

  TdxGanttControlChangeTaskSummaryCommand = class
    (TdxGanttControlChangeTaskPropertyCommand)
  protected
    procedure AfterExecute; override;
    function GetAssignedValue: TdxGanttTaskAssignedValue; override;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeTaskPropertyHistoryItemClass; override;
  end;

  { TdxGanttControlChangeTaskNameCommand }

  TdxGanttControlChangeTaskNameCommand = class
    (TdxGanttControlChangeTaskPropertyCommand)
  protected
    function GetAssignedValue: TdxGanttTaskAssignedValue; override;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeTaskPropertyHistoryItemClass; override;
  end;

  { TdxGanttControlChangeTaskDisplayOnTimelineCommand }

  TdxGanttControlChangeTaskDisplayOnTimelineCommand = class
    (TdxGanttControlChangeTaskPropertyCommand)
  protected
    function GetAssignedValue: TdxGanttTaskAssignedValue; override;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeTaskPropertyHistoryItemClass; override;
  end;

  { TdxGanttControlChangeTaskCalendarUIDCommand }

  TdxGanttControlChangeTaskCalendarUIDCommand = class
    (TdxGanttControlChangeTaskPropertyCommand)
  protected
    function GetAssignedValue: TdxGanttTaskAssignedValue; override;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeTaskPropertyHistoryItemClass; override;
  end;

  { TdxGanttControlSetTaskMilestoneCommand }

  TdxGanttControlSetTaskMilestoneCommand = class
    (TdxGanttControlChangeTaskPropertyCommand)
  protected
    procedure AfterExecute; override;
    function GetAssignedValue: TdxGanttTaskAssignedValue; override;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeTaskPropertyHistoryItemClass; override;
  end;

  { TdxGanttControlChangeTaskDurationFormatCommand }

  TdxGanttControlChangeTaskDurationFormatCommand = class
    (TdxGanttControlChangeTaskPropertyCommand)
  strict private
    FIsElapsed: Boolean;
  protected
    procedure AfterExecute; override;
    procedure BeforeExecute; override;
    function GetAssignedValue: TdxGanttTaskAssignedValue; override;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeTaskPropertyHistoryItemClass; override;
  end;

  { TdxGanttControlChangeTaskEstimatedCommand }

  TdxGanttControlChangeTaskEstimatedCommand = class
    (TdxGanttControlChangeTaskPropertyCommand)
  protected
    function GetAssignedValue: TdxGanttTaskAssignedValue; override;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeTaskPropertyHistoryItemClass; override;
  end;

  { TdxGanttControlChangeTaskManualCommand }

  TdxGanttControlChangeTaskManualCommand = class
    (TdxGanttControlChangeTaskPropertyCommand)
  protected
    function GetAssignedValue: TdxGanttTaskAssignedValue; override;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeTaskPropertyHistoryItemClass; override;
    procedure SetValue; override;
  end;

  { TdxGanttControlChangeSummaryManualCommand }

  TdxGanttControlChangeSummaryManualCommand = class
    (TdxGanttControlChangeTaskPropertyCommand)
  protected
    function GetAssignedValue: TdxGanttTaskAssignedValue; override;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeTaskPropertyHistoryItemClass; override;
  public
    function Enabled: Boolean; override;
  end;

  { TdxGanttControlChangeTaskDependentPropertyCustomCommand }

  TdxGanttControlChangeTaskDependentPropertyCustomCommand = class abstract
    (TdxGanttControlChangeTaskPropertyCommand)
  protected
    FIsDependent: Boolean;
  end;

  { TdxGanttControlChangeTaskPercentCompleteCommand }

  TdxGanttControlChangeTaskPercentCompleteCommand = class
    (TdxGanttControlChangeTaskDependentPropertyCustomCommand)
  protected
    procedure AfterExecute; override;
    function GetAssignedValue: TdxGanttTaskAssignedValue; override;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeTaskPropertyHistoryItemClass; override;
  end;

  { TdxGanttControlChangeTaskWorkVarianceCommand }

  TdxGanttControlChangeTaskWorkVarianceCommand = class
    (TdxGanttControlChangeTaskDependentPropertyCustomCommand)
  protected
    function GetAssignedValue: TdxGanttTaskAssignedValue; override;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeTaskPropertyHistoryItemClass; override;
  end;

  { TdxGanttControlChangeTaskDependentPropertyCommand }

  TdxGanttControlChangeTaskDependentPropertyCommand = class
    (TdxGanttControlChangeTaskDependentPropertyCustomCommand)
  strict private
    FCachedMinStart: TDateTime;
  protected
    procedure CalculateDependentLinks;
    procedure CalculateDependentSummary;
    function DoIsNewValueValid: Boolean; virtual;
    function GetMinStart: TDateTime;
    function IsNewValueValid: Boolean; override;
    procedure SetConstraint(AType: TdxGanttControlTaskConstraintType;
      AValue: TDateTime);
    procedure SetManual;
    procedure SetMilestone(Value: Boolean = True);
  protected
    procedure AfterExecute; override;
    procedure BeforeExecute; override;
  public
    constructor Create(AControl: TdxGanttControlBase;
      ATask: TdxGanttControlTask; const ANewValue: Variant); override;
  end;

  { TdxGanttControlChangeTaskConstraintCommand }

  TdxGanttControlChangeTaskConstraintCommand = class abstract
    (TdxGanttControlChangeTaskDependentPropertyCommand)
  protected
    procedure AfterExecute; override;
  end;

  { TdxGanttControlChangeTaskConstraintTypeCommand }

  TdxGanttControlChangeTaskConstraintTypeCommand = class
    (TdxGanttControlChangeTaskConstraintCommand)
  protected
    function GetAssignedValue: TdxGanttTaskAssignedValue; override;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeTaskPropertyHistoryItemClass; override;
  public
    function Enabled: Boolean; override;
  end;

  { TdxGanttControlChangeTaskConstraintDateCommand }

  TdxGanttControlChangeTaskConstraintDateCommand = class
    (TdxGanttControlChangeTaskConstraintCommand)
  protected
    function GetAssignedValue: TdxGanttTaskAssignedValue; override;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeTaskPropertyHistoryItemClass; override;
  public
    function Enabled: Boolean; override;
  end;

  { TdxGanttControlChangeTaskTimeBoundsCommand }

  TdxGanttControlChangeTaskTimeBoundsCommand = class abstract
    (TdxGanttControlChangeTaskDependentPropertyCommand)
  public
    function Enabled: Boolean; override;
  end;

  { TdxGanttControlChangeTaskStartCommand }

  TdxGanttControlChangeTaskStartCommand = class
    (TdxGanttControlChangeTaskTimeBoundsCommand)
  protected
    procedure AfterExecute; override;
    procedure BeforeExecute; override;
    function DoIsNewValueValid: Boolean; override;
    function GetAssignedValue: TdxGanttTaskAssignedValue; override;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeTaskPropertyHistoryItemClass; override;
    procedure SetValue; override;
    function ValidateValue(const AValue: Variant): Variant; override;
  end;

  { TdxGanttControlChangeTaskFinishCommand }

  TdxGanttControlChangeTaskFinishCommand = class
    (TdxGanttControlChangeTaskTimeBoundsCommand)
  strict private
    procedure SetDuration;
    procedure SetStart;
  protected
    procedure AfterExecute; override;
    procedure BeforeExecute; override;
    function GetAssignedValue: TdxGanttTaskAssignedValue; override;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeTaskPropertyHistoryItemClass; override;
  end;

  { TdxGanttControlChangeTaskDurationCommand }

  TdxGanttControlChangeTaskDurationCommand = class
    (TdxGanttControlChangeTaskTimeBoundsCommand)
  strict private
    FOldSeconds: Int64;
    class procedure SetFinish(AControl: TdxGanttControlBase;
      ATask: TdxGanttControlTask; Value: TDateTime); static;
    class procedure SetStart(AControl: TdxGanttControlBase;
      ATask: TdxGanttControlTask; Value: TDateTime); static;
  protected
    class procedure AdjustStartAndFinish(AControl: TdxGanttControlBase;
      ATask: TdxGanttControlTask); static;
    procedure AfterExecute; override;
    procedure BeforeExecute; override;
    function GetAssignedValue: TdxGanttTaskAssignedValue; override;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeTaskPropertyHistoryItemClass; override;
  end;

  { TdxGanttControlChangeTaskPredecessorsCommand }

  TdxGanttControlChangeTaskPredecessorsCommand = class
    (TdxGanttControlChangeTaskPropertyCommand)
  strict private
    FRaiseValidateException: Boolean;
    function CanCreatePredecessorLink(APredecessorUID: Integer): Boolean;
    procedure DeletePredecessorLink(AIndex: Integer); overload;
  protected
    procedure AfterExecute; override;
    procedure CreatePredecessorLink(APredecessorUID: Integer);
    class procedure DeletePredecessorLink(AControl: TdxGanttControlBase;
      ATask: TdxGanttControlTask; AIndex: Integer); overload; static;
    function GetAssignedValue: TdxGanttTaskAssignedValue; override;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeTaskPropertyHistoryItemClass; override;
    procedure SetValueAsArray(const Value: Variant);
    procedure SetValueAsString(const Value: string);
    procedure SetValue; override;
  public
    property RaiseValidateException: Boolean read FRaiseValidateException
      write FRaiseValidateException;
  end;

  { TdxGanttControlTaskAddPredecessorCommand }

  TdxGanttControlTaskAddPredecessorCommand = class
    (TdxGanttControlChangeTaskPredecessorsCommand)
  protected
    procedure SetValue; override;
  end;

  { TdxGanttControlChangeTaskPredecessorLinkPropertyCommand }

  TdxGanttControlChangeTaskPredecessorLinkPropertyCommand = class
    (TdxGanttControlTaskCommand)
  strict private
    FLink: TdxGanttControlTaskPredecessorLink;
    FNewValue: Variant;
  protected
    procedure DoExecute; override;

    function GetAssignedValue: TdxGanttTaskPredecessorLinkAssignedValue;
      virtual; abstract;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeTaskPredecessorLinkPropertyHistoryItemClass;
      virtual; abstract;
    function GetValue: Variant; virtual;
    procedure SetAssignedValue;
    procedure SetValue;
    procedure ResetAssignedValue;

    property Link: TdxGanttControlTaskPredecessorLink read FLink;
    property NewValue: Variant read FNewValue;
  public
    constructor Create(AControl: TdxGanttControlBase;
      ALink: TdxGanttControlTaskPredecessorLink; const ANewValue: Variant);
      reintroduce;
    function Enabled: Boolean; override;
  end;

  { TdxGanttControlChangeTaskPredecessorLinkTypeCommand }

  TdxGanttControlChangeTaskPredecessorLinkTypeCommand = class
    (TdxGanttControlChangeTaskPredecessorLinkPropertyCommand)
  protected
    function GetAssignedValue
      : TdxGanttTaskPredecessorLinkAssignedValue; override;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeTaskPredecessorLinkPropertyHistoryItemClass;
      override;
  end;

  { TdxGanttControlChangeTaskPredecessorLinkLagCommand }

  TdxGanttControlChangeTaskPredecessorLinkLagCommand = class
    (TdxGanttControlChangeTaskPredecessorLinkPropertyCommand)
  protected
    function GetAssignedValue
      : TdxGanttTaskPredecessorLinkAssignedValue; override;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeTaskPredecessorLinkPropertyHistoryItemClass;
      override;
  end;

  { TdxGanttControlChangeTaskPredecessorLinkLagFormatCommand }

  TdxGanttControlChangeTaskPredecessorLinkLagFormatCommand = class
    (TdxGanttControlChangeTaskPredecessorLinkPropertyCommand)
  protected
    function GetAssignedValue
      : TdxGanttTaskPredecessorLinkAssignedValue; override;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeTaskPredecessorLinkPropertyHistoryItemClass;
      override;
  end;

  { TdxGanttControlChangeTaskPredecessorCommand }

  TdxGanttControlChangeTaskPredecessorCommand = class
    (TdxGanttControlTaskCommand)
  strict private
    FCommands: TdxFastObjectList;
    FLink: TdxGanttControlTaskPredecessorLink;
    FNewLink: TdxGanttControlTaskPredecessorLink;
    procedure Delete;
  protected
    procedure DoExecute; override;
    procedure AfterExecute; override;
  public
    constructor Create(AControl: TdxGanttControlBase;
      ALink, ANewLink: TdxGanttControlTaskPredecessorLink); reintroduce;
    destructor Destroy; override;
    function Enabled: Boolean; override;
  end;

  { TdxGanttControlChangeTaskResourcesCommand }

  TdxGanttControlChangeTaskResourcesCommand = class
    (TdxGanttControlChangeTaskPropertyCommand)
  strict private
    procedure CreateAssignment(AResourceUID: Integer);
    procedure DeleteAssignment(AIndex: Integer); overload;
  protected
    function GetAssignedValue: TdxGanttTaskAssignedValue; override;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeTaskPropertyHistoryItemClass; override;
    function HasAssignedValue: Boolean; override;
    procedure SetValue; override;
    procedure SetValueAsArray(const Value: Variant);
    procedure SetValueAsString(const Value: string);
  end;

implementation

uses
  Math, DateUtils, RTLConsts,
  dxCore, cxDateUtils, dxCultureInfo, dxStringHelper,
  dxGanttControl,
  dxGanttControlUtils,
  dxGanttControlStrs,
  dxGanttControlResourceCommands,
  dxGanttControlTaskDependencyDialog;

type
  TdxGanttControlTasksAccess = class(TdxGanttControlTasks);
  TdxGanttControlTaskAccess = class(TdxGanttControlTask);
  TdxGanttControlTaskPredecessorLinksAccess = class
    (TdxGanttControlTaskPredecessorLinks);
  TdxGanttControlTaskDependencyDialogFormAccess = class
    (TdxGanttControlTaskDependencyDialogForm);

  { TdxTaskCalculator }

  TdxTaskCalculator = class abstract
  strict private
    FControl: TdxGanttControlBase;
    FTask: TdxGanttControlTask;
    function GetDataModel: TdxGanttControlDataModel;
  public
    constructor Create(AControl: TdxGanttControlBase;
      ATask: TdxGanttControlTask);

    class function AllowLink(ATask, APredecessor: TdxGanttControlTask): Boolean;
      overload; static;
    class function AllowLink(ATask: TdxGanttControlTask;
      APredecessorUID: Integer): Boolean; overload; static;
    class function CalculateBasicStart(ATask: TdxGanttControlTask)
      : TDateTime; static;
    class function GetSubTasks(ASummary: TdxGanttControlTask;
      AIncludeNullTask, ARecursive: Boolean)
      : TList<TdxGanttControlTask>; static;
    class function GetSummary(ATask: TdxGanttControlTask)
      : TdxGanttControlTask; static;
    class function IsSummary(ATask: TdxGanttControlTask): Boolean; static;
    class procedure ResetConstraint(AControl: TdxGanttControlBase;
      ATask: TdxGanttControlTask); static;
    class procedure SetDuration(AControl: TdxGanttControlBase;
      ATask: TdxGanttControlTask; const AValue: string); static;
    class procedure SetFinish(AControl: TdxGanttControlBase;
      ATask: TdxGanttControlTask; const AValue: TDateTime); static;
    class procedure SetPercentComplete(AControl: TdxGanttControlBase;
      ATask: TdxGanttControlTask; const AValue: Integer); static;
    class procedure SetStart(AControl: TdxGanttControlBase;
      ATask: TdxGanttControlTask; const AValue: TDateTime); static;

    procedure Calculate; virtual; abstract;

    property Control: TdxGanttControlBase read FControl;
    property DataModel: TdxGanttControlDataModel read GetDataModel;
    property Task: TdxGanttControlTask read FTask;
  end;

  { TdxDependentSummariesCalculator }

  TdxDependentSummaryCalculator = class(TdxTaskCalculator)
  public
    procedure Calculate; override;
    class procedure CalculateParentSummaryPercentComplete
      (AControl: TdxGanttControlBase; ATask: TdxGanttControlTask); static;
    class procedure CalculateSummaryStartAndFinish
      (AControl: TdxGanttControlBase; ASummary: TdxGanttControlTask); static;
    class procedure CalculateSummaryWorkVariance(AControl: TdxGanttControlBase;
      ASummary: TdxGanttControlTask); static;
    class procedure CalculateSummaryPercentComplete
      (AControl: TdxGanttControlBase; ASummary: TdxGanttControlTask); static;
    class procedure UpdateSummaryPercentComplete(AControl: TdxGanttControlBase;
      ASummary: TdxGanttControlTask); static;

    class procedure UpdateSummary(AControl: TdxGanttControlBase;
      ASummary: TdxGanttControlTask); static;
  end;

  { TdxDependentLinkCalculator }

  TdxDependentLinksCalculator = class(TdxTaskCalculator)
  protected
    class procedure CalculateStartAndFinish(ATask: TdxGanttControlTask;
      var AStart, AFinish: TDateTime); static;
    class procedure DoCalculateStartAndFinish(ATask: TdxGanttControlTask;
      var AStart, AFinish: TDateTime); static;
    class procedure UpdateLateAsPossibleTasks
      (AControl: TdxGanttControlBase); static;
  public
    procedure Calculate; override;
    class procedure CalculateLink(AControl: TdxGanttControlBase;
      ATask: TdxGanttControlTask; AForce: Boolean); static;
    class function CalculateMinStart(ATask: TdxGanttControlTask)
      : TDateTime; static;
    class procedure UpdateTaskStart(AControl: TdxGanttControlBase;
      ATask: TdxGanttControlTask); static;
  end;

  { TdxGanttControlTaskCandidatePredecessorLinks }

  TdxGanttControlTaskCandidatePredecessorLinks = class
    (TdxGanttControlTaskPredecessorLinks)
  protected
    procedure DoChanged; override;
  public
    class function CreateFromString(ATask: TdxGanttControlTask;
      const Value: string): TdxGanttControlTaskCandidatePredecessorLinks;
    function CheckDuplicate: Boolean;
    function IsEqual(AItem1, AItem2
      : TdxGanttControlTaskPredecessorLink): Boolean;
    procedure RemoveDuplicated;
  end;

  { TdxGanttControlTaskCandidateResource }

  TdxGanttControlTaskCandidateResource = class
  private
    FResource: TdxGanttControlResource;
    FName: string;
  public
    constructor Create(AResource: TdxGanttControlResource); overload;
    constructor Create(const AName: string); overload;

    property Resource: TdxGanttControlResource read FResource;
    property Name: string read FName;
  end;

  { TdxGanttControlTaskCandidateResources }

  TdxGanttControlTaskCandidateResources = class(TcxObjectList)
  public
    class function CreateFromString(ATask: TdxGanttControlTask;
      const Value: string): TdxGanttControlTaskCandidateResources;
    function CheckDuplicate: Boolean;
    function IsEqual(AItem1, AItem2
      : TdxGanttControlTaskCandidateResource): Boolean;
    procedure RemoveDuplicated;
  end;

  { TdxTaskStartComparer }

  TdxTaskStartComparer = class(TInterfacedObject,
    IComparer<TdxGanttControlTask>)
  protected
    function Compare(const Left, Right: TdxGanttControlTask): Integer;
  end;

  { TdxTaskIDComparer }

  TdxTaskIDComparer = class(TInterfacedObject, IComparer<TdxGanttControlTask>)
  protected
    function Compare(const Left, Right: TdxGanttControlTask): Integer;
  end;

  { TdxTaskCalculator }

constructor TdxTaskCalculator.Create(AControl: TdxGanttControlBase;
  ATask: TdxGanttControlTask);
begin
  inherited Create;
  FControl := AControl;
  FTask := ATask;
end;

class procedure TdxTaskCalculator.SetDuration(AControl: TdxGanttControlBase;
  ATask: TdxGanttControlTask; const AValue: string);
begin
  with TdxGanttControlChangeTaskDurationCommand.Create(AControl, ATask,
    AValue) do
    try
      FIsDependent := True;
      Execute;
    finally
      Free;
    end;
end;

class procedure TdxTaskCalculator.SetFinish(AControl: TdxGanttControlBase;
  ATask: TdxGanttControlTask; const AValue: TDateTime);
begin
  with TdxGanttControlChangeTaskFinishCommand.Create(AControl, ATask, AValue) do
    try
      FIsDependent := True;
      Execute;
    finally
      Free;
    end;
end;

class procedure TdxTaskCalculator.SetPercentComplete
  (AControl: TdxGanttControlBase; ATask: TdxGanttControlTask;
  const AValue: Integer);
begin
  with TdxGanttControlChangeTaskPercentCompleteCommand.Create(AControl, ATask,
    AValue) do
    try
      FIsDependent := True;
      Execute;
    finally
      Free;
    end;
end;

class procedure TdxTaskCalculator.SetStart(AControl: TdxGanttControlBase;
  ATask: TdxGanttControlTask; const AValue: TDateTime);
begin
  with TdxGanttControlChangeTaskStartCommand.Create(AControl, ATask, AValue) do
    try
      FIsDependent := True;
      Execute;
    finally
      Free;
    end;
end;

function TdxTaskCalculator.GetDataModel: TdxGanttControlDataModel;
begin
  Result := TdxGanttControlDataModel(Task.DataModel);
end;

class function TdxTaskCalculator.AllowLink(ATask, APredecessor
  : TdxGanttControlTask): Boolean;

  function CheckChain(ATask, APredecessor: TdxGanttControlTask): Boolean;
  var
    I: Integer;
  begin
    Result := (ATask <> APredecessor) and (ATask <> nil) and
      (APredecessor <> nil);
    if not Result then
      Exit;
    if APredecessor.IsValueAssigned(TdxGanttTaskAssignedValue.PredecessorLinks)
    then
      for I := 0 to APredecessor.PredecessorLinks.Count - 1 do
      begin
        Result := CheckChain(ATask,
          APredecessor.Owner.GetItemByUID(APredecessor.PredecessorLinks[I]
          .PredecessorUID));
        if not Result then
          Exit;
      end;
  end;

  function CheckSummaryChain(ATask, APredecessor: TdxGanttControlTask): Boolean;
  var
    I: Integer;
  begin
    Result := CheckChain(ATask, APredecessor);
    if not Result then
      Exit;
    if APredecessor.Summary then
    begin
      for I := APredecessor.ID + 1 to APredecessor.Owner.Count - 1 do
      begin
        if APredecessor.Owner[I].OutlineLevel <= APredecessor.OutlineLevel then
          Break;
        Result := CheckSummaryChain(ATask, APredecessor.Owner[I]);
        if not Result then
          Exit;
      end;
    end;
  end;

var
  I: Integer;
  ASummary: TdxGanttControlTask;
  ASubTasks: TList<TdxGanttControlTask>;
begin
  Result := CheckChain(ATask, APredecessor);
  if not Result then
    Exit;
  if APredecessor.Summary then
  begin
    ASummary := GetSummary(ATask);
    while ASummary <> nil do
    begin
      if ASummary = APredecessor then
        Exit(False);
      ASummary := GetSummary(ASummary);
    end;
    ASubTasks := GetSubTasks(APredecessor, False, False);
    try
      for I := 0 to ASubTasks.Count - 1 do
      begin
        Result := AllowLink(ATask, ASubTasks[I]);
        if not Result then
          Exit(False);
      end;
    finally
      ASubTasks.Free;
    end;
  end;
  if ATask.Summary then
  begin
    for I := ATask.ID + 1 to ATask.Owner.Count - 1 do
    begin
      if ATask.Owner[I].OutlineLevel <= ATask.OutlineLevel then
        Break;
      Result := CheckSummaryChain(ATask.Owner[I], APredecessor);
      if not Result then
        Exit;
    end;
  end;
  ASummary := GetSummary(APredecessor);
  while ASummary <> nil do
  begin
    Result := CheckChain(ATask, ASummary);
    if not Result then
      Exit;
    ASummary := GetSummary(ASummary);
  end;
end;

class function TdxTaskCalculator.AllowLink(ATask: TdxGanttControlTask;
  APredecessorUID: Integer): Boolean;
var
  APredecessor: TdxGanttControlTask;
begin
  APredecessor := ATask.Owner.GetItemByUID(APredecessorUID);
  Result := AllowLink(ATask, APredecessor);
end;

class function TdxTaskCalculator.GetSubTasks(ASummary: TdxGanttControlTask;
  AIncludeNullTask, ARecursive: Boolean): TList<TdxGanttControlTask>;
var
  I: Integer;
  AItems: TdxGanttControlTasks;
  ATask: TdxGanttControlTask;
  AComparer: IComparer<TdxGanttControlTask>;
begin
  AComparer := TdxTaskIDComparer.Create;
  Result := TList<TdxGanttControlTask>.Create(AComparer);
  if ASummary.Blank then
    Exit;
  AItems := ASummary.Owner;
  for I := ASummary.ID + 1 to AItems.Count - 1 do
  begin
    ATask := AItems[I];
    if ATask.Blank and not AIncludeNullTask then
      Continue;
    if not ATask.Blank and (ATask.OutlineLevel <= ASummary.OutlineLevel) then
      Break;
    if ARecursive or (ATask.OutlineLevel = ASummary.OutlineLevel + 1) then
      Result.Add(ATask);
  end;
end;

class function TdxTaskCalculator.GetSummary(ATask: TdxGanttControlTask)
  : TdxGanttControlTask;
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

class function TdxTaskCalculator.IsSummary(ATask: TdxGanttControlTask): Boolean;
begin
  Result := (ATask <> nil) and ATask.IsValueAssigned
    (TdxGanttTaskAssignedValue.Summary) and ATask.Summary;
end;

class procedure TdxTaskCalculator.ResetConstraint(AControl: TdxGanttControlBase;
  ATask: TdxGanttControlTask);
begin
  with TdxGanttControlChangeTaskConstraintTypeCommand.Create(AControl,
    ATask, Null) do
    try
      Execute;
    finally
      Free;
    end;
  with TdxGanttControlChangeTaskConstraintDateCommand.Create(AControl,
    ATask, Null) do
    try
      Execute;
    finally
      Free;
    end;
end;

class function TdxTaskCalculator.CalculateBasicStart(ATask: TdxGanttControlTask)
  : TDateTime;
var
  ACalendar: TdxGanttControlCalendar;
  AConstraintDate: TDateTime;
  ADuration: TdxGanttControlDuration;
  ADataModel: TdxGanttControlDataModel;
begin
  ADataModel := TdxGanttControlDataModel(ATask.DataModel);
  ACalendar := ATask.RealCalendar;
  Result := InvalidDate;
  if not ATask.Manual and ATask.IsValueAssigned
    (TdxGanttTaskAssignedValue.ConstraintType) and
    ATask.IsValueAssigned(TdxGanttTaskAssignedValue.ConstraintDate) then
  begin
    AConstraintDate := ATask.ConstraintDate;
    ADuration := TdxGanttControlDuration.Create(ATask.RealDuration);
    case ATask.ConstraintType of
      TdxGanttControlTaskConstraintType.AsSoonAsPossible:
        Result := ADataModel.RealProjectStart;
      TdxGanttControlTaskConstraintType.AsLateAsPossible:
        Result := ADuration.GetWorkStart(ADataModel.RealProjectFinish,
          ATask.RealCalendar, ATask.RealDurationFormat);

      TdxGanttControlTaskConstraintType.MustStartOn,
        TdxGanttControlTaskConstraintType.StartNoEarlierThan,
        TdxGanttControlTaskConstraintType.StartNoLaterThan:
        Result := AConstraintDate;

      TdxGanttControlTaskConstraintType.MustFinishOn,
        TdxGanttControlTaskConstraintType.FinishNoEarlierThan,
        TdxGanttControlTaskConstraintType.FinishNoLaterThan:
        Result := ADuration.GetWorkStart(AConstraintDate, ATask.RealCalendar,
          ATask.RealDurationFormat);
    end;
  end;
  if Result = InvalidDate then
    Result := ACalendar.GetNextWorkTime(ADataModel.RealProjectStart);
end;

{ TdxDependentSummariesCalculator }

procedure TdxDependentSummaryCalculator.Calculate;
var
  ASummary: TdxGanttControlTask;
begin
  ASummary := GetSummary(Task);
  while (ASummary <> nil) and ASummary.Manual do
    ASummary := GetSummary(ASummary);
  CalculateSummaryStartAndFinish(Control, ASummary);
end;

class procedure TdxDependentSummaryCalculator.CalculateSummaryWorkVariance
  (AControl: TdxGanttControlBase; ASummary: TdxGanttControlTask);

  procedure CalculateWorkVariance(ACalculatedSummary: TdxGanttControlTask);
  var
    ATotal: Double;
    ATaskDuration: Int64;
    I: Integer;
  begin
    ATotal := 0;
    for I := ACalculatedSummary.ID + 1 to ACalculatedSummary.Owner.Count - 1 do
    begin
      if ACalculatedSummary.Owner[I].Blank then
        Continue;
      if ACalculatedSummary.Owner[I].OutlineLevel = ACalculatedSummary.OutlineLevel
      then
        Break;
      if ACalculatedSummary.Owner[I].OutlineLevel >
        ACalculatedSummary.OutlineLevel + 1 then
        Continue;
      if ACalculatedSummary.Owner[I].Summary then
      begin
        if not ACalculatedSummary.IsValueAssigned
          (TdxGanttTaskAssignedValue.WorkVariance) then
          CalculateWorkVariance(ACalculatedSummary.Owner[I]);
        ATotal := ATotal + ACalculatedSummary.Owner[I].WorkVariance;
      end
      else if ACalculatedSummary.Owner[I].IsValueAssigned
        (TdxGanttTaskAssignedValue.Duration) then
      begin
        ATaskDuration := TdxGanttControlDuration.Create
          (ACalculatedSummary.Owner[I].Duration).ToSeconds;
        ATotal := ATotal + ATaskDuration / 60 * 1000;
      end;
    end;
    if ATotal > 0 then
    begin
      with TdxGanttControlChangeTaskWorkVarianceCommand.Create(AControl,
        ACalculatedSummary, ATotal) do
        try
          FIsDependent := True;
          Execute;
        finally
          Free;
        end;
    end;
  end;

begin
  if ASummary = nil then
    Exit;
  CalculateWorkVariance(ASummary);
  CalculateSummaryWorkVariance(AControl, GetSummary(ASummary));
end;

class procedure TdxDependentSummaryCalculator.CalculateSummaryPercentComplete
  (AControl: TdxGanttControlBase; ASummary: TdxGanttControlTask);

  function GetChildren: TList<TdxGanttControlTask>;
  var
    I: Integer;
    AComparer: IComparer<TdxGanttControlTask>;
  begin
    Result := TList<TdxGanttControlTask>.Create;
    for I := ASummary.ID + 1 to ASummary.Owner.Count - 1 do
    begin
      if ASummary.Blank then
        Continue;
      if ASummary.Owner[I].OutlineLevel = ASummary.OutlineLevel then
        Break;
      if ASummary.Owner[I].Summary then
        Continue;
      if not ASummary.Owner[I].IsValueAssigned
        (TdxGanttTaskAssignedValue.Duration) or not ASummary.Owner[I]
        .IsValueAssigned(TdxGanttTaskAssignedValue.Start) then
        Continue;
      Result.Add(ASummary.Owner[I]);
    end;
    AComparer := TdxTaskStartComparer.Create;
    Result.Sort(AComparer);
  end;

  function GetTaskStartCount(AList: TList<TdxGanttControlTask>): Integer;
  var
    I: Integer;
  begin
    Result := 1;
    for I := 1 to AList.Count - 1 do
      if CompareDateTime(AList[0].Start, AList[I].Start) = 0 then
        Inc(Result)
      else
        Break;
  end;

  procedure SetPercentComplete(ATask: TdxGanttControlTask; Value: Double);
  var
    AIntValue: Integer;
  begin
    AIntValue := Trunc(Value);
    if Frac(Value) >= 0.5 then
      Inc(AIntValue);
    TdxTaskCalculator.SetPercentComplete(AControl, ATask, AIntValue);
  end;

  procedure UpdateInnerSummaries;
  var
    I: Integer;
    AList: TList<TdxGanttControlTask>;
  begin
    AList := TList<TdxGanttControlTask>.Create;
    try
      for I := ASummary.ID + 1 to ASummary.Owner.Count - 1 do
      begin
        if ASummary.Blank then
          Continue;
        if ASummary.Owner[I].OutlineLevel = ASummary.OutlineLevel then
          Break;
        if ASummary.Owner[I].Summary then
          AList.Add(ASummary.Owner[I]);
      end;
      for I := AList.Count - 1 downto 0 do
        UpdateSummaryPercentComplete(AControl, AList[I]);
    finally
      AList.Free
    end;
  end;

var
  AList: TList<TdxGanttControlTask>;
  ATask: TdxGanttControlTask;
  I: Integer;
  ACount: Integer;
  AWorkVariance: Double;
  APercentWorkVariance: Double;
  ATaskWorkVariance: Double;
begin
  if not ASummary.IsValueAssigned(TdxGanttTaskAssignedValue.WorkVariance) then
    TdxDependentSummaryCalculator.CalculateSummaryWorkVariance(AControl,
      ASummary);
  AWorkVariance := ASummary.WorkVariance;
  APercentWorkVariance := AWorkVariance * ASummary.PercentComplete / 100;
  AList := GetChildren;
  try
    while AList.Count > 0 do
    begin
      if APercentWorkVariance = 0 then
        Break;
      ACount := GetTaskStartCount(AList);
      ATask := AList[0];
      ATaskWorkVariance := TdxGanttControlDuration.Create(ATask.RealDuration)
        .ToSeconds / 60 * 1000;
      if APercentWorkVariance > ATaskWorkVariance * ACount then
      begin
        APercentWorkVariance := APercentWorkVariance - ATaskWorkVariance;
        SetPercentComplete(ATask, 100);
        AList.Delete(0);
      end
      else
      begin
        APercentWorkVariance := APercentWorkVariance / ACount;
        while ACount > 0 do
        begin
          ATask := AList[0];
          ATaskWorkVariance := TdxGanttControlDuration.Create
            (ATask.RealDuration).ToSeconds / 60 * 1000;
          SetPercentComplete(ATask, APercentWorkVariance /
            ATaskWorkVariance * 100);
          AList.Delete(0);
          Dec(ACount);
        end;
        Break;
      end;
    end;
    for I := 0 to AList.Count - 1 do
      SetPercentComplete(AList[I], 0);
  finally
    AList.Free;
  end;
  UpdateInnerSummaries;
end;

class procedure TdxDependentSummaryCalculator.UpdateSummaryPercentComplete
  (AControl: TdxGanttControlBase; ASummary: TdxGanttControlTask);
var
  ASeconds: Int64;
  ATaskDuration: Int64;
  ATasks: TdxGanttControlTasks;
  I: Integer;
begin
  ASeconds := 0;
  if not ASummary.IsValueAssigned(TdxGanttTaskAssignedValue.WorkVariance) then
    CalculateSummaryWorkVariance(AControl, ASummary);
  ATasks := ASummary.Owner;
  for I := ASummary.ID + 1 to ATasks.Count - 1 do
  begin
    if ATasks[I].Blank then
      Continue;
    if ATasks[I].OutlineLevel = ASummary.OutlineLevel then
      Break;
    if ATasks[I].OutlineLevel > ASummary.OutlineLevel + 1 then
      Break;
    if ATasks[I].Summary then
    begin
      if not ATasks[I].IsValueAssigned(TdxGanttTaskAssignedValue.WorkVariance)
      then
        CalculateSummaryWorkVariance(AControl, ATasks[I]);
      Inc(ASeconds, Round(ATasks[I].WorkVariance * ATasks[I].PercentComplete /
        100 / 1000 * 60));
    end
    else if ASummary.Owner[I].IsValueAssigned(TdxGanttTaskAssignedValue.Duration)
    then
    begin
      ATaskDuration := TdxGanttControlDuration.Create
        (ASummary.Owner[I].Duration).ToSeconds;
      if ATasks[I].IsValueAssigned(TdxGanttTaskAssignedValue.PercentComplete)
      then
        Inc(ASeconds, MulDiv(ATaskDuration, ATasks[I].PercentComplete, 100));
    end;
  end;
  if ASummary.WorkVariance > 0 then
    SetPercentComplete(AControl, ASummary,
      Round(ASeconds / 60 * 1000 / ASummary.WorkVariance * 100));
end;

class procedure TdxDependentSummaryCalculator.UpdateSummary
  (AControl: TdxGanttControlBase; ASummary: TdxGanttControlTask);
begin
  if not IsSummary(ASummary) then
    Exit;
  CalculateSummaryStartAndFinish(AControl, ASummary);
  CalculateSummaryWorkVariance(AControl, ASummary);
  UpdateSummaryPercentComplete(AControl, ASummary);
  UpdateSummary(AControl, GetSummary(ASummary));
end;

class procedure TdxDependentSummaryCalculator.
  CalculateParentSummaryPercentComplete(AControl: TdxGanttControlBase;
  ATask: TdxGanttControlTask);
var
  ASummary: TdxGanttControlTask;
begin
  ASummary := GetSummary(ATask);
  if ASummary = nil then
    Exit;
  TdxDependentSummaryCalculator.UpdateSummaryPercentComplete(AControl,
    ASummary);
  CalculateParentSummaryPercentComplete(AControl, ASummary);
end;

class procedure TdxDependentSummaryCalculator.CalculateSummaryStartAndFinish
  (AControl: TdxGanttControlBase; ASummary: TdxGanttControlTask);

  procedure CalculateTimeBounds(ASummary: TdxGanttControlTask;
    var AStart, AFinish: TDateTime);
  var
    I: Integer;
    ATask: TdxGanttControlTask;
  begin
    for I := ASummary.Owner.IndexOf(ASummary) + 1 to ASummary.Owner.Count - 1 do
    begin
      ATask := ASummary.Owner[I];
      if ATask.Blank or
        (ATask.ConstraintType = TdxGanttControlTaskConstraintType.
        AsLateAsPossible) then
        Continue;
      if ATask.OutlineLevel <= ASummary.OutlineLevel then
        Break;
      if ATask.OutlineLevel > ASummary.OutlineLevel + 1 then
        Continue;
      if ATask.IsValueAssigned(TdxGanttTaskAssignedValue.Start) then
        AStart := Min(AStart, ATask.Start);
      if ATask.IsValueAssigned(TdxGanttTaskAssignedValue.Finish) then
        AFinish := Max(AFinish, ATask.Finish);
      if ATask.Summary and ATask.Manual then
        CalculateTimeBounds(ATask, AStart, AFinish);
    end;
  end;

var
  AStart, AFinish: TDateTime;
begin
  if (ASummary = nil) or ASummary.Manual then
    Exit;
  AStart := MaxDateTime;
  AFinish := MinDateTime;
  CalculateTimeBounds(ASummary, AStart, AFinish);
  if (AStart <> ASummary.Start) and (AStart <> MaxDateTime) then
    SetStart(AControl, ASummary, AStart);
  if (AFinish <> ASummary.Finish) and (AFinish <> MinDateTime) then
    SetFinish(AControl, ASummary, AFinish);
  if (AStart = MaxDateTime) and (AFinish = MinDateTime) then
  begin
    SetStart(AControl, ASummary, TdxTaskCalculator.CalculateBasicStart
      (ASummary));
    SetFinish(AControl, ASummary, ASummary.Start);
  end;
end;

{ TdxDependentLinkCalculator }

procedure TdxDependentLinksCalculator.Calculate;
var
  I: Integer;
  ATask: TdxGanttControlTask;
begin
  TdxGanttControlTasksAccess(Task.Owner).CalculateDirtyTask(Task);
  if TdxGanttControlTasksAccess(Task.Owner).IsDirtyTask(Task) then
    Exit;
  for I := 0 to TdxGanttControlTaskAccess(Task).Links.Count - 1 do
  begin
    ATask := TdxGanttControlTask(TdxGanttControlTaskAccess(Task).Links[I]);
    CalculateLink(Control, ATask, False);
  end;
end;

class procedure TdxDependentLinksCalculator.CalculateLink
  (AControl: TdxGanttControlBase; ATask: TdxGanttControlTask; AForce: Boolean);

  function GetStart: TDateTime;
  var
    ADuration: TdxGanttControlDuration;
    ADataModel: TdxGanttControlDataModel;
  begin
    Result := MinDateTime;
    if ATask.Manual then
    begin
      if ATask.IsValueAssigned(TdxGanttTaskAssignedValue.Start) then
        Result := ATask.Start;
    end
    else
    begin
      if ATask.IsValueAssigned(TdxGanttTaskAssignedValue.ConstraintType) then
      begin
        ADuration := TdxGanttControlDuration.Create(ATask.RealDuration);
        case ATask.ConstraintType of
          TdxGanttControlTaskConstraintType.StartNoEarlierThan:
            Result := ATask.ConstraintDate;
          TdxGanttControlTaskConstraintType.FinishNoEarlierThan:
            Result := ADuration.GetWorkStart(ATask.ConstraintDate,
              ATask.RealCalendar, ATask.RealDurationFormat);
        end;
      end;
      if (Result = MinDateTime) and
        (not ATask.IsValueAssigned(TdxGanttTaskAssignedValue.PredecessorLinks)
        or (ATask.PredecessorLinks.Count = 0)) then
      begin
        ADataModel := TdxGanttControlDataModel(ATask.DataModel);
        Result := ADataModel.RealProjectStart;
      end;
    end;
  end;

  function GetFinish: TDateTime;
  var
    ADuration: TdxGanttControlDuration;
  begin
    Result := MinDateTime;
    if ATask.Manual then
    begin
      if ATask.IsValueAssigned(TdxGanttTaskAssignedValue.Finish) then
        Result := ATask.Finish;
    end
    else if ATask.IsValueAssigned(TdxGanttTaskAssignedValue.ConstraintType) then
    begin
      ADuration := TdxGanttControlDuration.Create(ATask.RealDuration);
      case ATask.ConstraintType of
        TdxGanttControlTaskConstraintType.StartNoEarlierThan:
          Result := ADuration.GetWorkFinish(ATask.ConstraintDate,
            ATask.RealCalendar, ATask.RealDurationFormat);
        TdxGanttControlTaskConstraintType.FinishNoEarlierThan:
          Result := ATask.ConstraintDate;
      end;
    end;
  end;

  procedure CalculateSummary;
  var
    I: Integer;
  begin
    for I := ATask.ID + 1 to ATask.Owner.Count - 1 do
    begin
      if ATask.Owner[I].Blank then
        Continue;
      if ATask.Owner[I].OutlineLevel = ATask.OutlineLevel + 1 then
        TdxDependentLinksCalculator.CalculateLink(AControl,
          ATask.Owner[I], False)
      else
        Break;
    end;
  end;

var
  ADuration: TdxGanttControlDuration;
  AStart, AFinish: TDateTime;
begin
  if ATask.Summary then
  begin
    CalculateSummary;
    Exit;
  end;
  if not AForce and ATask.Manual then
    Exit;
  AStart := GetStart;
  AFinish := GetFinish;
  CalculateStartAndFinish(ATask, AStart, AFinish);
  if AStart = MinDateTime then
    AStart := ATask.RealCalendar.GetNextWorkTime(DateOf(ATask.Created));
  if not ATask.IsValueAssigned(TdxGanttTaskAssignedValue.Start) or
    (ATask.Start <> AStart) then
  begin
    ADuration := TdxGanttControlDuration.Create(ATask.RealDuration);
    if not ADuration.IsZero then
      AStart := ATask.RealCalendar.GetNextWorkTime(AStart);
    SetStart(AControl, ATask, AStart);
    SetDuration(AControl, ATask, ADuration.ToString);
  end;
end;

class function TdxDependentLinksCalculator.CalculateMinStart
  (ATask: TdxGanttControlTask): TDateTime;
var
  AFinish: TDateTime;
begin
  Result := MinDateTime;
  AFinish := MinDateTime;
  CalculateStartAndFinish(ATask, Result, AFinish);
  if Result = MinDateTime then
    Result := TdxGanttControlDataModel(ATask.DataModel).RealProjectStart;
end;

class procedure TdxDependentLinksCalculator.UpdateTaskStart
  (AControl: TdxGanttControlBase; ATask: TdxGanttControlTask);
var
  AStart: TDateTime;
begin
  if not ATask.IsValueAssigned(TdxGanttTaskAssignedValue.Start) or
    not ATask.IsValueAssigned(TdxGanttTaskAssignedValue.Manual) or ATask.Manual
  then
    Exit;

  AStart := CalculateMinStart(ATask);
  TdxTaskCalculator.SetStart(AControl, ATask, AStart);
end;

class procedure TdxDependentLinksCalculator.CalculateStartAndFinish
  (ATask: TdxGanttControlTask; var AStart, AFinish: TDateTime);

  procedure CalculateTaskAsLateAsPossible(ACalculatedTask: TdxGanttControlTask;
    var ACurrentFinish: TDateTime);
  var
    I: Integer;
    ADuration: TdxGanttControlDuration;
    ALinks: TdxFastList;
  begin
    ADuration := TdxGanttControlDuration.Create(ATask.RealDuration);
    if ACalculatedTask.IsValueAssigned(TdxGanttTaskAssignedValue.ConstraintType)
      and ACalculatedTask.IsValueAssigned
      (TdxGanttTaskAssignedValue.ConstraintDate) and
      (ACalculatedTask.ConstraintType
      in [TdxGanttControlTaskConstraintType.FinishNoLaterThan,
      TdxGanttControlTaskConstraintType.StartNoLaterThan]) then
    begin
      if ACalculatedTask.ConstraintType = TdxGanttControlTaskConstraintType.FinishNoLaterThan
      then
        ACurrentFinish := Min(ACurrentFinish,
          ADuration.GetWorkStart(ACalculatedTask.ConstraintDate,
          ACalculatedTask.RealCalendar, ACalculatedTask.RealDurationFormat))
      else
        ACurrentFinish := Min(ACurrentFinish, ACalculatedTask.ConstraintDate);
      Exit;
    end;
    TdxGanttControlTasksAccess(ATask.Owner).CalculateDirtyTask(ACalculatedTask);
    ALinks := TdxGanttControlTaskAccess(ACalculatedTask).Links;
    for I := 0 to ALinks.Count - 1 do
    begin
      CalculateTaskAsLateAsPossible(TdxGanttControlTask(ALinks[I]),
        ACurrentFinish);
      ACurrentFinish := Min(ACurrentFinish,
        ADuration.GetWorkStart(ACurrentFinish, ACalculatedTask.RealCalendar,
        ACalculatedTask.RealDurationFormat));
    end;
  end;

  procedure CalculateConstraintAsLateAsPossible;
  begin
    AFinish := TdxGanttControlDataModel(ATask.DataModel).RealProjectFinish;
    CalculateTaskAsLateAsPossible(ATask, AFinish);
  end;

  procedure CalculateConstraint;
  begin
    case ATask.ConstraintType of
      TdxGanttControlTaskConstraintType.AsLateAsPossible:
        CalculateConstraintAsLateAsPossible;
      TdxGanttControlTaskConstraintType.StartNoEarlierThan:
        AStart := Max(AStart, ATask.ConstraintDate);
      TdxGanttControlTaskConstraintType.StartNoLaterThan:
        AStart := Min(AStart, ATask.ConstraintDate);
      TdxGanttControlTaskConstraintType.FinishNoEarlierThan:
        AFinish := Max(AFinish, ATask.ConstraintDate);
      TdxGanttControlTaskConstraintType.FinishNoLaterThan:
        AFinish := Min(AFinish, ATask.ConstraintDate);
    end;
  end;

var
  ASummary: TdxGanttControlTask;
  ADuration: TdxGanttControlDuration;
  AOldFinish: TDateTime;
  AHasConstraint: Boolean;
begin
  if ATask.Summary then
    Exit;
  if ATask.RealDuration = '' then
    Exit;
  if ATask.IsValueAssigned(TdxGanttTaskAssignedValue.ConstraintType) and
    ATask.IsValueAssigned(TdxGanttTaskAssignedValue.ConstraintDate) and
    (ATask.ConstraintType in [TdxGanttControlTaskConstraintType.MustStartOn,
    TdxGanttControlTaskConstraintType.MustFinishOn]) then
  begin
    if ATask.ConstraintType = TdxGanttControlTaskConstraintType.MustStartOn then
      AStart := ATask.ConstraintDate
    else
    begin
      ADuration := TdxGanttControlDuration.Create(ATask.RealDuration);
      AStart := ADuration.GetWorkStart(ATask.ConstraintDate, ATask.RealCalendar,
        ATask.RealDurationFormat);
    end;
  end
  else
  begin
    AOldFinish := AFinish;
    DoCalculateStartAndFinish(ATask, AStart, AFinish);
    ASummary := GetSummary(ATask);
    while ASummary <> nil do
    begin
      if not ASummary.Manual then
        DoCalculateStartAndFinish(ASummary, AStart, AFinish);
      ASummary := GetSummary(ASummary);
    end;
    ADuration := TdxGanttControlDuration.Create(ATask.RealDuration);
    AHasConstraint := ATask.IsValueAssigned
      (TdxGanttTaskAssignedValue.ConstraintType) and
      (ATask.IsValueAssigned(TdxGanttTaskAssignedValue.ConstraintDate) or
      (ATask.ConstraintType = TdxGanttControlTaskConstraintType.
      AsLateAsPossible));
    if AHasConstraint then
      CalculateConstraint;
    if AFinish <> AOldFinish then
    begin
      if AHasConstraint and
        (ATask.ConstraintType
        in [TdxGanttControlTaskConstraintType.FinishNoLaterThan,
        TdxGanttControlTaskConstraintType.AsLateAsPossible]) then
      begin
        if ADuration.IsZero then
          AStart := AFinish
        else
          AStart := ADuration.GetWorkStart(AFinish, ATask.RealCalendar,
            ATask.RealDurationFormat);
      end
      else
      begin
        if ADuration.IsZero then
          AStart := Max(AStart, AFinish)
        else
          AStart := Max(AStart, ADuration.GetWorkStart(AFinish,
            ATask.RealCalendar, ATask.RealDurationFormat));
      end;
    end;
  end;
  if not ATask.IsValueAssigned(TdxGanttTaskAssignedValue.Start) or
    (ATask.Start <> AStart) then
    if not ADuration.IsZero then
      AStart := ATask.RealCalendar.GetNextWorkTime(AStart);
end;

class procedure TdxDependentLinksCalculator.DoCalculateStartAndFinish
  (ATask: TdxGanttControlTask; var AStart, AFinish: TDateTime);

  function GetPredecessorDuration(APredecessor: TdxGanttControlTask): TDateTime;
  var
    ADuration: TdxGanttControlDuration;
  begin
    ADuration := TdxGanttControlDuration.Create(APredecessor.Duration);
    Result := OneSecond * ADuration.Second + OneMinute * ADuration.Minute +
      OneHour * ADuration.Hour;
  end;

  function GetLinkLagDuration(APredecessor: TdxGanttControlTask;
    ALink: TdxGanttControlTaskPredecessorLink): TdxGanttControlDuration;
  var
    ALag: Integer;
    H, M, S: Integer;
  begin
    ALag := Abs(ALink.LinkLag);
    if ALink.IsPercentLagFormat then
      ALag := Ceil(GetPredecessorDuration(APredecessor) * ALag * 24 * 60 *
        10 / 100);
    H := Trunc(ALag / 10 / 60);
    ALag := ALag - H * 10 * 60;
    M := Trunc(ALag / 10);
    ALag := ALag - M * 10;
    S := Trunc(ALag * 60 / 10);
    Result := TdxGanttControlDuration.Create(Format('PT%dH%dM%dS', [H, M, S]));
  end;

  function GetNextDateTime(ADateTime: TDateTime;
    APredecessor: TdxGanttControlTask;
    ALink: TdxGanttControlTaskPredecessorLink): TDateTime;
  var
    ALinkLagDuration: TdxGanttControlDuration;
  begin
    Result := ADateTime;
    if not ALink.IsElapsedLagFormat then
      if ALink.LinkLag > 0 then
        ADateTime := ATask.RealCalendar.GetNextWorkTime(ADateTime)
      else
        ADateTime := ATask.RealCalendar.GetPreviousWorkTime(ADateTime);
    ALinkLagDuration := GetLinkLagDuration(APredecessor, ALink);
    if ALink.LinkLag > 0 then
      Result := ALinkLagDuration.GetWorkFinish(ADateTime, ATask.RealCalendar,
        ALink.IsElapsedLagFormat)
    else if ALink.LinkLag < 0 then
      Result := ALinkLagDuration.GetWorkStart
        (ATask.RealCalendar.GetPreviousWorkTime(ADateTime), ATask.RealCalendar,
        ALink.IsElapsedLagFormat);
  end;

var
  I: Integer;
  ALink: TdxGanttControlTaskPredecessorLink;
  APredecessor: TdxGanttControlTask;
  APredecessorLinks: TdxGanttControlTaskPredecessorLinks;
  ALinkType: TdxGanttControlTaskPredecessorLinkType;
begin
  APredecessorLinks := ATask.PredecessorLinks;
  for I := 0 to APredecessorLinks.Count - 1 do
  begin
    ALink := APredecessorLinks[I];
    if not ALink.IsValueAssigned
      (TdxGanttTaskPredecessorLinkAssignedValue.PredecessorUID) then
      Continue;
    APredecessor := ATask.Owner.GetItemByUID(ALink.PredecessorUID);
    if APredecessor = nil then
      Continue;
    if not ALink.IsValueAssigned(TdxGanttTaskPredecessorLinkAssignedValue.&Type)
    then
      ALinkType := TdxGanttControlTaskPredecessorLinkType.FS
    else
      ALinkType := ALink.&Type;
    if ATask.Summary and
      (ALinkType in [TdxGanttControlTaskPredecessorLinkType.FF,
      TdxGanttControlTaskPredecessorLinkType.SF]) then
      Continue;
    case ALinkType of
      TdxGanttControlTaskPredecessorLinkType.FF:
        if APredecessor.IsValueAssigned(TdxGanttTaskAssignedValue.Finish) then
          AFinish := Max(GetNextDateTime(APredecessor.Finish, APredecessor,
            ALink), AFinish);
      TdxGanttControlTaskPredecessorLinkType.FS:
        if APredecessor.IsValueAssigned(TdxGanttTaskAssignedValue.Finish) then
          AStart := Max(GetNextDateTime(APredecessor.Finish, APredecessor,
            ALink), AStart);
      TdxGanttControlTaskPredecessorLinkType.SF:
        if APredecessor.IsValueAssigned(TdxGanttTaskAssignedValue.Start) then
          AFinish := Max(GetNextDateTime(APredecessor.Start, APredecessor,
            ALink), AFinish);
      TdxGanttControlTaskPredecessorLinkType.SS:
        if APredecessor.IsValueAssigned(TdxGanttTaskAssignedValue.Start) then
          AStart := Max(GetNextDateTime(APredecessor.Start, APredecessor,
            ALink), AStart);
    end;
  end;
end;

class procedure TdxDependentLinksCalculator.UpdateLateAsPossibleTasks
  (AControl: TdxGanttControlBase);
var
  I: Integer;
  ALateAsPossibleTasks: TdxFastList;
  AStart: TDateTime;
  ATask: TdxGanttControlTask;
begin
  ALateAsPossibleTasks := TdxGanttControlTasksAccess
    (TdxCustomGanttControl(AControl).DataModel.Tasks).LateAsPossibleTasks;
  for I := 0 to ALateAsPossibleTasks.Count - 1 do
  begin
    ATask := TdxGanttControlTask(ALateAsPossibleTasks[I]);
    AStart := CalculateMinStart(ATask);
    if (AStart > MinDateTime) and (CompareDateTime(ATask.Start, AStart) <> 0)
    then
      SetStart(AControl, ATask, AStart);
  end;
end;

{ TdxGanttControlTaskCandidatePredecessorLinks }

function TdxGanttControlTaskCandidatePredecessorLinks.CheckDuplicate: Boolean;
var
  I, J: Integer;
begin
  Result := True;
  for I := 1 to Count - 1 do
    for J := 0 to I - 1 do
      if IsEqual(Items[I], Items[J]) then
        Exit(False);
end;

class function TdxGanttControlTaskCandidatePredecessorLinks.CreateFromString
  (ATask: TdxGanttControlTask; const Value: string)
  : TdxGanttControlTaskCandidatePredecessorLinks;

  function ExtractID(const S: string; var AStartPos: Integer;
    out AUID: Integer): Boolean;
  var
    st: string;
    I: Integer;
  begin
    st := '';
    for I := AStartPos to Length(S) do
      if CharInSet(S[I], ['0' .. '9']) then
        st := st + S[I]
      else
      begin
        AStartPos := I;
        Break;
      end;
    Result := TryStrToInt(st, AUID);
  end;

  function ExtractLinkType(const S: string; var AStartPos: Integer;
    out AType: TdxGanttControlTaskPredecessorLinkType): Boolean;
  var
    FF, FS, SF, SS: string;
    L: Integer;
  begin
    FF := AnsiUpperCase
      (cxGetResourceString(@sdxGanttControlTaskPredecessorLinkTypeFF));
    FS := AnsiUpperCase
      (cxGetResourceString(@sdxGanttControlTaskPredecessorLinkTypeFS));
    SF := AnsiUpperCase
      (cxGetResourceString(@sdxGanttControlTaskPredecessorLinkTypeSF));
    SS := AnsiUpperCase
      (cxGetResourceString(@sdxGanttControlTaskPredecessorLinkTypeSS));
    L := Length(S);
    Result := True;
    if (AStartPos + Length(FS) - 1 <= L) and
      (CompareMem(@S[AStartPos], @FS[1], Length(FS) * SizeOf(Char))) then
    begin
      AType := TdxGanttControlTaskPredecessorLinkType.FS;
      AStartPos := AStartPos + Length(FS);
    end
    else if (AStartPos + Length(FF) - 1 <= L) and
      (CompareMem(@S[AStartPos], @FF[1], Length(FF) * SizeOf(Char))) then
    begin
      AType := TdxGanttControlTaskPredecessorLinkType.FF;
      AStartPos := AStartPos + Length(FF);
    end
    else if (AStartPos + Length(SF) - 1 <= L) and
      (CompareMem(@S[AStartPos], @SF[1], Length(SF) * SizeOf(Char))) then
    begin
      AType := TdxGanttControlTaskPredecessorLinkType.SF;
      AStartPos := AStartPos + Length(SF);
    end
    else if (AStartPos + Length(SS) - 1 <= L) and
      (CompareMem(@S[AStartPos], @SS[1], Length(SS) * SizeOf(Char))) then
    begin
      AType := TdxGanttControlTaskPredecessorLinkType.SS;
      AStartPos := AStartPos + Length(SS);
    end
    else
      Result := False;
  end;

  function ExtractLinkLag(const S: string; var AStartPos: Integer;
    out ALag: Double): Boolean;
  var
    I, P: Integer;
  begin
    if AStartPos > Length(S) then
    begin
      Result := True;
      ALag := 0;
      Exit;
    end;
    P := Length(S);
    for I := AStartPos to Length(S) do
      if not CharInSet(S[I],
        ['-', '+', TdxCultureInfo.CurrentCulture.FormatSettings.
        DecimalSeparator, '0' .. '9']) then
      begin
        P := I - 1;
        Break;
      end;
    Result := TryStrToFloat(Copy(S, AStartPos, P - AStartPos + 1), ALag,
      TdxCultureInfo.CurrentCulture.FormatSettings);
    AStartPos := P + 1;
  end;

  function ExtractLagFormat(const S: string; var AStartPos: Integer;
    out ALagFormat: TdxGanttControlTaskPredecessorLagFormat): Boolean;
  var
    L: Integer;
    AError: Boolean;
    AErrText: TCaption;
  begin
    L := Length(S);
    if AStartPos > L then
    begin
      Result := True;
      ALagFormat := TdxGanttControlTaskPredecessorLagFormat.Days;
    end
    else
    begin
      TdxGanttControlTaskDependencyDialogFormAccess.InitializeMeasurementUnits;
      ALagFormat := TdxGanttControlTaskDependencyDialogFormAccess.GetLagFormat
        (Copy(S, AStartPos, L - AStartPos + 1), AError, AErrText);
      Result := not AError;
    end;
  end;

  function DoParse(AToken: string; out APredecessorID: Integer;
    out ALinkType: TdxGanttControlTaskPredecessorLinkType; out ALinkLag: Double;
    out ALagFormat: TdxGanttControlTaskPredecessorLagFormat): Boolean;
  var
    AStartPos: Integer;
  begin
    AToken := AnsiUpperCase(TdxStringHelper.Replace(AToken, ' ', ''));
    AStartPos := 1;
    Result := ExtractID(AToken, AStartPos, APredecessorID) and
      ExtractLinkType(AToken, AStartPos, ALinkType) and
      ExtractLinkLag(AToken, AStartPos, ALinkLag) and
      ExtractLagFormat(AToken, AStartPos, ALagFormat);
  end;

  function PopulateLinkFromToken(ALink: TdxGanttControlTaskPredecessorLink;
    AToken: string): Boolean;
  var
    APredecessorID: Integer;
    ALinkType: TdxGanttControlTaskPredecessorLinkType;
    ALinkLag: Double;
    ALagFormat: TdxGanttControlTaskPredecessorLagFormat;
  begin
    Result := True;
    if TryStrToInt(AToken, APredecessorID) then
    begin
      if APredecessorID < 0 then
        Result := False
      else
      begin
        ALinkType := TdxGanttControlTaskPredecessorLinkType.FS;
        ALinkLag := 0;
        ALagFormat := TdxGanttControlTaskPredecessorLagFormat.Days;
      end
    end
    else
      Result := DoParse(AToken, APredecessorID, ALinkType, ALinkLag,
        ALagFormat);
    if Result then
    begin
      if APredecessorID <= ATask.Owner.Count - 1 then
        ALink.PredecessorUID := ATask.Owner.Items[APredecessorID].UID
      else
        ALink.PredecessorUID := -APredecessorID;
      ALink.&Type := ALinkType;
      ALink.LagFormat := ALagFormat;
      ALink.LinkLag := TdxGanttControlTaskDependencyDialogFormAccess.GetLinkLag
        (ALinkLag, ALagFormat);
    end;
  end;

var
  AError: Boolean;
  ATokens: TStringList;
  AToken: string;
  ALink: TdxGanttControlTaskPredecessorLink;
  I: Integer;
begin
  AError := False;
  Result := TdxGanttControlTaskCandidatePredecessorLinks.Create(ATask);
  ATokens := TStringList.Create;
  try
    ATokens.Delimiter := TdxCultureInfo.CurrentCulture.FormatSettings.
      ListSeparator;
    ATokens.DelimitedText := TdxStringHelper.Replace(Value, ' ', '');
    for I := 0 to ATokens.Count - 1 do
    begin
      AToken := ATokens[I];
      if AToken <> '' then
      begin
        ALink := Result.Append;
        AError := not PopulateLinkFromToken(ALink, AToken);
        if AError then
          Break;
      end;
    end;
  finally
    ATokens.Free;
  end;
  if AError then
    FreeAndNil(Result);
end;

procedure TdxGanttControlTaskCandidatePredecessorLinks.DoChanged;
begin
  // do nothing
end;

function TdxGanttControlTaskCandidatePredecessorLinks.IsEqual(AItem1,
  AItem2: TdxGanttControlTaskPredecessorLink): Boolean;
begin
  Result := AItem1.PredecessorUID = AItem2.PredecessorUID;
end;

procedure TdxGanttControlTaskCandidatePredecessorLinks.RemoveDuplicated;
var
  AIndex, I: Integer;
begin
  AIndex := 0;
  while AIndex < Count - 1 do
  begin
    for I := Count - 1 downto AIndex + 1 do
      if IsEqual(Items[AIndex], Items[I]) then
        Remove(Items[I]);
    Inc(AIndex);
  end;
end;

{ TdxTaskStartComparer }

function TdxTaskStartComparer.Compare(const Left,
  Right: TdxGanttControlTask): Integer;
begin
  if not Left.IsValueAssigned(TdxGanttTaskAssignedValue.Start) and
    not Right.IsValueAssigned(TdxGanttTaskAssignedValue.Start) then
    Result := 0
  else
  begin
    if not Left.IsValueAssigned(TdxGanttTaskAssignedValue.Start) then
      Exit(1)
    else if not Right.IsValueAssigned(TdxGanttTaskAssignedValue.Start) then
      Exit(-1);
    Result := Sign(Left.Start - Right.Start);
    if Result = 0 then
      Result := Sign(TdxGanttControlDuration.Create(Left.RealDuration).ToSeconds
        - TdxGanttControlDuration.Create(Right.RealDuration).ToSeconds);
  end;
end;

{ TdxTaskIDComparer }

function TdxTaskIDComparer.Compare(const Left,
  Right: TdxGanttControlTask): Integer;
begin
  Result := Sign(Left.ID - Right.ID);
end;

{ TdxGanttControlTaskCandidateResource }

constructor TdxGanttControlTaskCandidateResource.Create
  (AResource: TdxGanttControlResource);
begin
  inherited Create;
  FResource := AResource;
end;

constructor TdxGanttControlTaskCandidateResource.Create(const AName: string);
begin
  inherited Create;
  FResource := nil;
  FName := AName;
end;

{ TdxGanttControlTaskCandidateResources }

class function TdxGanttControlTaskCandidateResources.CreateFromString
  (ATask: TdxGanttControlTask; const Value: string)
  : TdxGanttControlTaskCandidateResources;

  procedure AddCandidate(const AName: string);
  var
    AResource: TdxGanttControlResource;
  begin
    if AName <> '' then
    begin
      AResource := (ATask.DataModel as TdxGanttControlDataModel)
        .Resources.GetItemByName(AName);
      if AResource <> nil then
        Result.Add(TdxGanttControlTaskCandidateResource.Create(AResource))
      else
        Result.Add(TdxGanttControlTaskCandidateResource.Create(AName));
    end;
  end;

var
  S: string;
  P: Integer;
begin
  Result := TdxGanttControlTaskCandidateResources.Create(True);
  S := Trim(Value);
  if S = '' then
    Exit;
  P := Pos(TdxCultureInfo.CurrentCulture.FormatSettings.ListSeparator, S);
  while P > 0 do
  begin
    AddCandidate(Trim(Copy(S, 1, P - 1)));
    System.Delete(S, 1, P);
    P := Pos(TdxCultureInfo.CurrentCulture.FormatSettings.ListSeparator, S);
  end;
  if S <> '' then
    AddCandidate(Trim(S));
end;

function TdxGanttControlTaskCandidateResources.CheckDuplicate: Boolean;
var
  I, J: Integer;
  AResource: TdxGanttControlTaskCandidateResource;
begin
  Result := True;
  for I := 1 to Count - 1 do
  begin
    AResource := TdxGanttControlTaskCandidateResource(Items[I]);
    for J := 0 to I - 1 do
      if IsEqual(AResource, TdxGanttControlTaskCandidateResource(Items[J])) then
        Exit(False);
  end;
end;

function TdxGanttControlTaskCandidateResources.IsEqual(AItem1,
  AItem2: TdxGanttControlTaskCandidateResource): Boolean;
begin
  Result := ((AItem1.Resource <> nil) and (AItem2.Resource <> nil) and
    (AItem1.Resource.UID = AItem2.Resource.UID)) or
    ((AItem1.Resource = nil) and (AItem2.Resource = nil) and
    (AItem1.Name = AItem2.Name));
end;

procedure TdxGanttControlTaskCandidateResources.RemoveDuplicated;
var
  AIndex, I: Integer;
  AResource: TdxGanttControlTaskCandidateResource;
begin
  AIndex := 0;
  while AIndex < Count - 1 do
  begin
    AResource := TdxGanttControlTaskCandidateResource(Items[AIndex]);
    for I := Count - 1 downto AIndex + 1 do
      if IsEqual(AResource, TdxGanttControlTaskCandidateResource(Items[I])) then
        Remove(Items[I]);
    Inc(AIndex);
  end;
end;

{ TdxGanttControlTaskHistoryItem }

constructor TdxGanttControlTaskHistoryItem.Create
  (AHistory: TdxGanttControlHistory; ATask: TdxGanttControlTask);
begin
  inherited Create(AHistory);
  FTask := ATask;
end;

{ TdxGanttControlTaskMakeNotNullHistoryItem }

constructor TdxGanttControlTaskMakeNotNullHistoryItem.Create
  (AHistory: TdxGanttControlHistory; ATask: TdxGanttControlTask);
begin
  inherited Create(AHistory, ATask);
  FIsNull := ATask.Blank;
end;

procedure TdxGanttControlTaskMakeNotNullHistoryItem.DoRedo;
begin
  inherited DoRedo;
  Task.Blank := False;
end;

procedure TdxGanttControlTaskMakeNotNullHistoryItem.DoUndo;
begin
  Task.Blank := FIsNull;
  inherited DoUndo;
end;

{ TdxGanttControlTaskSetAssignedValueHistoryItem }

procedure TdxGanttControlTaskSetAssignedValueHistoryItem.DoUndo;
begin
  Task.ResetValue(FAssignedValue);
  inherited DoUndo;
end;

{ TdxGanttControlTaskPredecessorLinkSetAssignedValueHistoryItem }

procedure TdxGanttControlTaskPredecessorLinkSetAssignedValueHistoryItem.DoUndo;
begin
  Link.ResetValue(FAssignedValue);
  inherited DoUndo;
end;

{ TdxGanttControlTaskResetAssignedValueHistoryItem }

procedure TdxGanttControlTaskResetAssignedValueHistoryItem.DoRedo;
begin
  Task.ResetValue(FAssignedValue);
  inherited DoRedo;
end;

{ TdxGanttControlTaskPredecessorLinkResetAssignedValueHistoryItem }

procedure TdxGanttControlTaskPredecessorLinkResetAssignedValueHistoryItem.
  DoRedo;
begin
  Link.ResetValue(FAssignedValue);
  inherited DoRedo;
end;

{ TdxGanttControlChangeTaskPropertyHistoryItem }

procedure TdxGanttControlChangeTaskPropertyHistoryItem.SetValue
  (const Value: Variant);
begin
  if not VarIsNull(Value) then
    DoSetValue(Value);
end;

procedure TdxGanttControlChangeTaskPropertyHistoryItem.DoRedo;
begin
  inherited DoRedo;
  FOldValue := GetValue;
  SetValue(FNewValue);
end;

procedure TdxGanttControlChangeTaskPropertyHistoryItem.DoUndo;
begin
  SetValue(FOldValue);
  inherited DoUndo;
end;

{ TdxGanttControlChangeTaskOutlineLevelHistoryItem }

function TdxGanttControlChangeTaskOutlineLevelHistoryItem.GetValue: Variant;
begin
  Result := Task.OutlineLevel;
end;

procedure TdxGanttControlChangeTaskOutlineLevelHistoryItem.DoSetValue
  (const Value: Variant);
begin
  Task.OutlineLevel := Value;
end;

{ TdxGanttControlChangeTaskSummaryHistoryItem }

procedure TdxGanttControlChangeTaskSummaryHistoryItem.DoSetValue
  (const Value: Variant);
begin
  Task.Summary := Value;
end;

function TdxGanttControlChangeTaskSummaryHistoryItem.GetValue: Variant;
begin
  Result := Task.Summary;
end;

{ TdxGanttControlChangeTaskNameHistoryItem }

function TdxGanttControlChangeTaskNameHistoryItem.GetValue: Variant;
begin
  Result := Task.Name;
end;

procedure TdxGanttControlChangeTaskNameHistoryItem.DoSetValue
  (const Value: Variant);
begin
  Task.Name := VarToStr(Value);
end;

{ TdxGanttControlChangeTaskDisplayOnTimelineHistoryItem }

procedure TdxGanttControlChangeTaskDisplayOnTimelineHistoryItem.DoSetValue
  (const Value: Variant);
begin
  Task.DisplayOnTimeline := Value;
end;

function TdxGanttControlChangeTaskDisplayOnTimelineHistoryItem.GetValue
  : Variant;
begin
  Result := Task.DisplayOnTimeline;
end;

{ TdxGanttControlChangeTaskCalendarUIDHistoryItem }

procedure TdxGanttControlChangeTaskCalendarUIDHistoryItem.DoSetValue
  (const Value: Variant);
begin
  Task.CalendarUID := Value;
end;

function TdxGanttControlChangeTaskCalendarUIDHistoryItem.GetValue: Variant;
begin
  Result := Task.CalendarUID;
end;

{ TdxGanttControlChangeTaskPercentCompleteHistoryItem }

function TdxGanttControlChangeTaskPercentCompleteHistoryItem.GetValue: Variant;
begin
  Result := Task.PercentComplete;
end;

procedure TdxGanttControlChangeTaskPercentCompleteHistoryItem.DoSetValue
  (const Value: Variant);
begin
  Task.PercentComplete := Value;
end;

{ TdxGanttControlChangeTaskWorkVarianceHistoryItem }

function TdxGanttControlChangeTaskWorkVarianceHistoryItem.GetValue: Variant;
begin
  Result := Task.WorkVariance;
end;

procedure TdxGanttControlChangeTaskWorkVarianceHistoryItem.DoSetValue
  (const Value: Variant);
begin
  Task.WorkVariance := Value;
end;

{ TdxGanttControlChangeTaskDurationFormatHistoryItem }

function TdxGanttControlChangeTaskDurationFormatHistoryItem.GetValue: Variant;
begin
  Result := Task.DurationFormat;
end;

procedure TdxGanttControlChangeTaskDurationFormatHistoryItem.DoSetValue
  (const Value: Variant);
begin
  Task.DurationFormat := Value;
end;

{ TdxGanttControlChangeTaskEstimatedHistoryItem }

function TdxGanttControlChangeTaskEstimatedHistoryItem.GetValue: Variant;
begin
  Result := Task.Estimated;
end;

procedure TdxGanttControlChangeTaskEstimatedHistoryItem.DoSetValue
  (const Value: Variant);
begin
  Task.Estimated := Value;
end;

{ TdxGanttControlChangeTaskConstraintTypeHistoryItem }

function TdxGanttControlChangeTaskConstraintTypeHistoryItem.GetValue: Variant;
begin
  Result := Task.ConstraintType;
end;

procedure TdxGanttControlChangeTaskConstraintTypeHistoryItem.DoSetValue
  (const Value: Variant);
begin
  Task.ConstraintType := Value;
end;

{ TdxGanttControlChangeTaskConstraintDateHistoryItem }

function TdxGanttControlChangeTaskConstraintDateHistoryItem.GetValue: Variant;
begin
  Result := Task.ConstraintDate;
end;

procedure TdxGanttControlChangeTaskConstraintDateHistoryItem.DoSetValue
  (const Value: Variant);
begin
  Task.ConstraintDate := Value;
end;

{ TdxGanttControlChangeManualHistoryItem }

function TdxGanttControlChangeManualHistoryItem.GetValue: Variant;
begin
  Result := Task.Manual;
end;

procedure TdxGanttControlChangeManualHistoryItem.DoSetValue
  (const Value: Variant);
begin
  Task.Manual := Value;
end;

{ TdxGanttControlChangeTaskMilestoneHistoryItem }

function TdxGanttControlChangeTaskMilestoneHistoryItem.GetValue: Variant;
begin
  Result := Task.Milestone;
end;

procedure TdxGanttControlChangeTaskMilestoneHistoryItem.DoSetValue
  (const Value: Variant);
begin
  Task.Milestone := Value;
end;

{ TdxGanttControlChangeTaskStartHistoryItem }

function TdxGanttControlChangeTaskStartHistoryItem.GetValue: Variant;
begin
  Result := Task.Start;
end;

procedure TdxGanttControlChangeTaskStartHistoryItem.DoSetValue
  (const Value: Variant);
begin
  Task.Start := VarToDateTime(Value);
end;

{ TdxGanttControlChangeTaskFinishHistoryItem }

function TdxGanttControlChangeTaskFinishHistoryItem.GetValue: Variant;
begin
  Result := Task.Finish;
end;

procedure TdxGanttControlChangeTaskFinishHistoryItem.DoSetValue
  (const Value: Variant);
begin
  Task.Finish := VarToDateTime(Value);
end;

{ TdxGanttControlChangeTaskDurationHistoryItem }

function TdxGanttControlChangeTaskDurationHistoryItem.GetValue: Variant;
begin
  Result := Task.Duration;
end;

procedure TdxGanttControlChangeTaskDurationHistoryItem.DoSetValue
  (const Value: Variant);
begin
  Task.Duration := VarToStr(Value);
end;

{ TdxGanttControlChangeTaskPredecessorLinkPropertyHistoryItem }

constructor TdxGanttControlChangeTaskPredecessorLinkPropertyHistoryItem.Create
  (AHistory: TdxGanttControlHistory; ALink: TdxGanttControlTaskPredecessorLink);
begin
  inherited Create(AHistory, ALink.Task);
  FLink := ALink;
end;

{ TdxGanttControlChangeTaskPredecessorLinkTypeHistoryItem }

function TdxGanttControlChangeTaskPredecessorLinkTypeHistoryItem.
  GetValue: Variant;
begin
  Result := Link.&Type;
end;

procedure TdxGanttControlChangeTaskPredecessorLinkTypeHistoryItem.DoSetValue
  (const Value: Variant);
begin
  Link.&Type := Value;
end;

{ TdxGanttControlChangeTaskPredecessorLinkLagHistoryItem }

function TdxGanttControlChangeTaskPredecessorLinkLagHistoryItem.
  GetValue: Variant;
begin
  Result := Link.LinkLag;
end;

procedure TdxGanttControlChangeTaskPredecessorLinkLagHistoryItem.DoSetValue
  (const Value: Variant);
begin
  Link.LinkLag := Value;
end;

{ TdxGanttControlChangeTaskPredecessorLinkLagFormatHistoryItem }

function TdxGanttControlChangeTaskPredecessorLinkLagFormatHistoryItem.
  GetValue: Variant;
begin
  Result := Link.LagFormat;
end;

procedure TdxGanttControlChangeTaskPredecessorLinkLagFormatHistoryItem.
  DoSetValue(const Value: Variant);
begin
  Link.LagFormat := Value;
end;

{ TdxGanttControlTaskCustomCommand }

constructor TdxGanttControlTaskCustomCommand.Create
  (AControl: TdxGanttControlBase; ATask: TdxGanttControlTask);
begin
  inherited Create(AControl);
  FTask := ATask
end;

procedure TdxGanttControlTaskCustomCommand.DoSetAssignedValue
  (AValue: TdxGanttTaskAssignedValue);
var
  AHistoryItem: TdxGanttControlTaskSetAssignedValueHistoryItem;
  AHistory: TdxGanttControlHistory;
begin
  AHistory := Control.History;
  AHistoryItem := TdxGanttControlTaskSetAssignedValueHistoryItem.Create
    (AHistory, Task);
  AHistoryItem.FAssignedValue := AValue;
  AHistory.AddItem(AHistoryItem);
  AHistoryItem.Execute;
end;

procedure TdxGanttControlTaskCustomCommand.BeginUpdate;
begin
  inherited BeginUpdate;
  if Task <> nil then
    Task.BeginUpdate;
end;

procedure TdxGanttControlTaskCustomCommand.EndUpdate;
begin
  if Task <> nil then
    Task.EndUpdate;
  inherited EndUpdate;
end;

{ TdxGanttControlDeleteTaskCommand }

procedure TdxGanttControlDeleteTaskCommand.AfterExecute;
var
  I: Integer;
  ASubTasks: TList<TdxGanttControlTask>;
begin
  if not FExecuteNeeded then
    Exit;
  inherited AfterExecute;
  if FIsLast then
    for I := Task.Owner.Count - 1 downto 0 do
    begin
      if Task.Owner[I].Blank then
        DeleteTaskCore(I)
      else
        Break;
    end;
  if FParentSummary <> nil then
    TdxDependentSummaryCalculator.UpdateSummary(Control, FParentSummary);
  if (FParentSummary = nil) or (FParentSummary.ID = 0) then
    Exit;
  ASubTasks := TdxTaskCalculator.GetSubTasks(FParentSummary, False, False);
  try
    if ASubTasks.Count = 0 then
      with TdxGanttControlChangeTaskSummaryCommand.Create(Control,
        FParentSummary, False) do
        try
          Execute;
        finally
          Free;
        end;
  finally
    ASubTasks.Free;
  end;
end;

procedure TdxGanttControlDeleteTaskCommand.BeforeExecute;
begin
  inherited BeforeExecute;
  FExecuteNeeded := Task.Blank or not FRaiseConfirmation or
    not Control.OptionsBehavior.ConfirmDelete or
    (MessageDlg(GetDeletingTaskConfirmation(Task), mtConfirmation, mbYesNo,
    0) = mrYes);
  if FExecuteNeeded then
  begin
    FIsLast := Task.Owner[Task.Owner.Count - 1] = Task;
    FParentSummary := TdxTaskCalculator.GetSummary(Task);
  end;
end;

procedure TdxGanttControlDeleteTaskCommand.BeginUpdate;
begin
  inherited BeginUpdate;
  DataModel.BeginUpdate;
end;

procedure TdxGanttControlDeleteTaskCommand.DoExecute;
var
  I: Integer;
  ASubTasks: TList<TdxGanttControlTask>;
  ASubTask: TdxGanttControlTask;
begin
  if not FExecuteNeeded then
    Exit;
  inherited DoExecute;
  TdxGanttControlTasksAccess(Task.Owner).CalculateDirtyTasks;
  TdxGanttControlTasksAccess(Task.Owner).AddDirtyTask(Task.UID);
  ASubTasks := TdxTaskCalculator.GetSubTasks(Task, True, True);
  try
    for I := ASubTasks.Count - 1 downto 0 do
    begin
      ASubTask := ASubTasks[I];
      CheckLinks(ASubTask, ASubTasks);
      DeleteAssignments(ASubTask);
      DeleteTaskCore(ASubTask.ID);
    end;
    CheckLinks(Task, ASubTasks);
    DeleteAssignments(Task);
    DeleteTaskCore(Task.ID);
  finally
    ASubTasks.Free;
  end;
end;

function TdxGanttControlDeleteTaskCommand.Enabled: Boolean;
begin
  Result := inherited Enabled and (Task <> nil);
end;

procedure TdxGanttControlDeleteTaskCommand.EndUpdate;
begin
  inherited EndUpdate;
  DataModel.EndUpdate;
end;

procedure TdxGanttControlDeleteTaskCommand.DeleteAssignments
  (ATask: TdxGanttControlTask);
var
  I: Integer;
begin
  if ATask.Blank then
    Exit;
  for I := DataModel.Assignments.Count - 1 downto 0 do
  begin
    if DataModel.Assignments[I].TaskUID = ATask.UID then
      TdxGanttControlChangeResourceCustomCommand.DeleteAssignment(Control,
        DataModel.Assignments, I);
  end;
end;

procedure TdxGanttControlDeleteTaskCommand.DeleteTaskCore(AIndex: Integer);
var
  AHistoryItem: TdxGanttControlRemoveItemHistoryItem;
  AHistory: TdxGanttControlHistory;
begin
  AHistory := Control.History;
  AHistoryItem := TdxGanttControlRemoveItemHistoryItem.Create(AHistory,
    Task.Owner);
  AHistoryItem.Index := AIndex;
  AHistory.AddItem(AHistoryItem);
  AHistoryItem.Execute;
end;

procedure TdxGanttControlDeleteTaskCommand.CheckLinks
  (ATask: TdxGanttControlTask; ADeletedTasks: TList<TdxGanttControlTask>);

  procedure DeleteLink(ALink: TdxGanttControlTaskPredecessorLink);
  begin
    with TdxGanttControlChangeTaskPredecessorCommand.Create(Control,
      ALink, nil) do
      try
        Execute;
      finally
        Free;
      end;
  end;

var
  I, AIndex: Integer;
  ALink: TdxGanttControlTask;
  APredecessor: TdxGanttControlTaskPredecessorLink;
begin
  if ATask.Blank then
    Exit;
  for I := ATask.PredecessorLinks.Count - 1 downto 0 do
  begin
    if not ATask.PredecessorLinks[I].IsValueAssigned
      (TdxGanttTaskPredecessorLinkAssignedValue.PredecessorUID) then
      Continue;
    ALink := ATask.Owner.GetItemByUID(ATask.PredecessorLinks[I].PredecessorUID);
    if (ALink <> nil) and not ADeletedTasks.BinarySearch(ALink, AIndex) then
    begin
      TdxGanttControlTasksAccess(ATask.Owner).AddDirtyTask(ATask.UID);
      DeleteLink(ATask.PredecessorLinks[I]);
    end;
  end;
  TdxGanttControlTasksAccess(ATask.Owner).CalculateDirtyTask(ATask);
  if TdxGanttControlTasksAccess(ATask.Owner).IsDirtyTask(ATask) then
    Exit;
  for I := 0 to TdxGanttControlTaskAccess(ATask).Links.Count - 1 do
  begin
    ALink := TdxGanttControlTask(TdxGanttControlTaskAccess(ATask).Links[I]);
    if ADeletedTasks.BinarySearch(ALink, AIndex) then
      Continue;
    TdxGanttControlTasksAccess(ATask.Owner).AddDirtyTask(ALink.UID);
    APredecessor := ALink.PredecessorLinks.GetItemByPredecessorUID(ATask.UID);
    while APredecessor <> nil do
    begin
      DeleteLink(APredecessor);
      APredecessor := ALink.PredecessorLinks.GetItemByPredecessorUID(ATask.UID);
    end;
  end;
end;

constructor TdxGanttControlDeleteTaskCommand.Create
  (AControl: TdxGanttControlBase; ATask: TdxGanttControlTask);
begin
  inherited Create(AControl, ATask);
  FRaiseConfirmation := True;
end;

{ TdxGanttControlSetTaskNotNullCommand }

procedure TdxGanttControlSetTaskNotNullCommand.AfterExecute;
var
  AOutlineLevel: Integer;
  I: Integer;
begin
  AOutlineLevel := Task.Owner[0].OutlineLevel + 1;
  for I := Task.ID - 1 downto 0 do
  begin
    if Task.Owner[I].Blank then
      Continue;
    if Task.Owner[I].Summary then
      AOutlineLevel := Task.Owner[I].OutlineLevel + 1
    else
      AOutlineLevel := Task.Owner[I].OutlineLevel;
    Break;
  end;
  with TdxGanttControlChangeTaskOutlineLevelCommand.Create(Control, Task,
    AOutlineLevel) do
    try
      Execute;
    finally
      Free;
    end;
  inherited AfterExecute;
end;

procedure TdxGanttControlSetTaskNotNullCommand.BeforeExecute;
begin
  inherited BeforeExecute;
  if not Task.IsValueAssigned(TdxGanttTaskAssignedValue.Blank) then
    DoSetAssignedValue(TdxGanttTaskAssignedValue.Blank);
end;

procedure TdxGanttControlSetTaskNotNullCommand.DoExecute;
var
  AHistoryItem: TdxGanttControlTaskMakeNotNullHistoryItem;
  AHistory: TdxGanttControlHistory;
begin
  inherited DoExecute;
  AHistory := Control.History;
  AHistoryItem := TdxGanttControlTaskMakeNotNullHistoryItem.Create
    (AHistory, Task);
  AHistory.AddItem(AHistoryItem);
  AHistoryItem.Execute;
end;

function TdxGanttControlSetTaskNotNullCommand.Enabled: Boolean;
begin
  Result := inherited Enabled and
    (not Task.IsValueAssigned(TdxGanttTaskAssignedValue.Blank) or Task.Blank);
end;

{ TdxGanttControlChangeTaskCommand }

constructor TdxGanttControlChangeTaskCommand.Create
  (AControl: TdxGanttControlBase; ATask, ANewTask: TdxGanttControlTask);
begin
  inherited Create(AControl, ATask);
  FNewTask := ANewTask;
  FCommands := TdxFastObjectList.Create;
  if FNewTask <> nil then
  begin
    FCommands.Add(TdxGanttControlChangeTaskNameCommand.Create(Control, Task,
      FNewTask.Name));
    FCommands.Add(TdxGanttControlChangeTaskPercentCompleteCommand.Create
      (Control, Task, FNewTask.PercentComplete));
    FCommands.Add(TdxGanttControlChangeTaskCalendarUIDCommand.Create(Control,
      Task, FNewTask.CalendarUID));
    FCommands.Add(TdxGanttControlChangeTaskManualCommand.Create(Control, Task,
      FNewTask.Manual));
    FCommands.Add(TdxGanttControlChangeTaskConstraintTypeCommand.Create(Control,
      Task, FNewTask.ConstraintType));
    FCommands.Add(TdxGanttControlChangeTaskConstraintDateCommand.Create(Control,
      Task, FNewTask.ConstraintDate));
    FCommands.Add(TdxGanttControlChangeTaskStartCommand.Create(Control, Task,
      FNewTask.Start));
    FCommands.Add(TdxGanttControlChangeTaskFinishCommand.Create(Control, Task,
      FNewTask.Finish));
    FCommands.Add(TdxGanttControlChangeTaskDurationCommand.Create(Control, Task,
      FNewTask.Duration));
    FCommands.Add(TdxGanttControlChangeTaskDurationFormatCommand.Create(Control,
      Task, FNewTask.DurationFormat));
    FCommands.Add(TdxGanttControlChangeTaskEstimatedCommand.Create(Control,
      Task, FNewTask.Estimated));
    FCommands.Add(TdxGanttControlChangeTaskDisplayOnTimelineCommand.Create
      (Control, Task, FNewTask.DisplayOnTimeline));
  end;
end;

destructor TdxGanttControlChangeTaskCommand.Destroy;
begin
  FreeAndNil(FCommands);
  inherited Destroy;
end;

procedure TdxGanttControlChangeTaskCommand.DoExecute;
var
  I: Integer;
  ACommand: TdxGanttControlChangeTaskPropertyCommand;
begin
  if FNewTask = nil then
  begin
    with TdxGanttControlDeleteTaskCommand.Create(Control, Task) do
      try
        RaiseConfirmation := False;
        Execute;
      finally
        Free;
      end;
  end
  else
  begin
    for I := 0 to FCommands.Count - 1 do
    begin
      ACommand := TdxGanttControlChangeTaskPropertyCommand(FCommands[I]);
      if FNewTask.IsValueAssigned(ACommand.GetAssignedValue) then
        ACommand.Execute;
    end;
  end;
end;

{ TdxGanttControlTaskCommand }

procedure TdxGanttControlTaskCommand.BeforeExecute;
begin
  inherited BeforeExecute;
  if Task.Blank then
  begin
    MakeTaskNotNull;
    SetTaskMode;
  end;
end;

procedure TdxGanttControlTaskCommand.MakeTaskNotNull;
begin
  MakeTaskNotNull(Task);
end;

procedure TdxGanttControlTaskCommand.MakeTaskNotNull
  (ATask: TdxGanttControlTask);
begin
  with TdxGanttControlSetTaskNotNullCommand.Create(Control, ATask) do
    try
      Execute;
    finally
      Free;
    end;
end;

procedure TdxGanttControlTaskCommand.SetTaskMode;
begin
  with TdxGanttControlSetTaskModeCommand.CreateCommand(Control, Task) do
    try
      Execute;
    finally
      Free;
    end;
end;

{ TdxGanttControlSetTaskModeCommand }

class function TdxGanttControlSetTaskModeCommand.CreateCommand
  (AControl: TdxGanttControlBase; ATask: TdxGanttControlTask)
  : TdxGanttControlSetTaskModeCommand;
var
  ADataModel: TdxGanttControlDataModel;
  AManual: Boolean;
begin
  ADataModel := TdxCustomGanttControl(AControl).DataModel;
  AManual := ADataModel.Properties.MarkNewTasksAsManuallyScheduled;
  if AManual then
    Result := TdxGanttControlSetTaskManuallyScheduleModeCommand.Create
      (AControl, ATask)
  else
    Result := TdxGanttControlSetTaskAutoScheduleModeCommand.Create
      (AControl, ATask);
end;

procedure TdxGanttControlSetTaskModeCommand.DoExecute;
var
  AHistoryItem: TdxGanttControlChangeManualHistoryItem;
  AHistory: TdxGanttControlHistory;
begin
  inherited DoExecute;
  AHistory := Control.History;
  AHistoryItem := TdxGanttControlChangeManualHistoryItem.Create(AHistory, Task);
  AHistoryItem.FNewValue := IsManual;
  AHistory.AddItem(AHistoryItem);
  AHistoryItem.Execute;
end;

function TdxGanttControlSetTaskModeCommand.Enabled: Boolean;
begin
  Result := inherited Enabled and (not Task.Blank) and
    (not Task.IsValueAssigned(TdxGanttTaskAssignedValue.Manual) or
    (Task.Manual <> IsManual));
end;

function TdxGanttControlSetTaskModeCommand.IsManual: Boolean;
begin
  Result := False;
end;

{ TdxGanttControlSetTaskManuallyScheduleModeCommand }

function TdxGanttControlSetTaskManuallyScheduleModeCommand.IsManual: Boolean;
begin
  Result := True;
end;

{ TdxGanttControlSetTaskAutoScheduleModeCommand }

procedure TdxGanttControlSetTaskAutoScheduleModeCommand.DoExecute;
begin
  inherited DoExecute;
  TdxGanttControlSetTaskAutoScheduleModeCommand.SetBasicStartAndFinish
    (Control, Task);
end;

class procedure TdxGanttControlSetTaskAutoScheduleModeCommand.
  SetBasicStartAndFinish(AControl: TdxGanttControlBase;
  ATask: TdxGanttControlTask);
var
  ADuration: string;
  AStart: TDateTime;
  ACalendar: TdxGanttControlCalendar;
begin
  if ATask.Summary then
  begin
    TdxDependentSummaryCalculator.CalculateSummaryStartAndFinish
      (AControl, ATask);
    Exit;
  end;
  if ATask.IsValueAssigned(TdxGanttTaskAssignedValue.ConstraintType) and
    ATask.IsValueAssigned(TdxGanttTaskAssignedValue.ConstraintDate) and
    (ATask.ConstraintType <> TdxGanttControlTaskConstraintType.AsSoonAsPossible)
  then
    Exit;
  ADuration := ATask.RealDuration;
  ACalendar := ATask.RealCalendar;
  AStart := TdxDependentLinksCalculator.CalculateMinStart(ATask);
  with TdxGanttControlChangeTaskStartCommand.Create(AControl, ATask, AStart) do
    try
      FIsDependent := True;
      Execute;
    finally
      Free;
    end;
  if ADuration = '' then
    with TdxGanttControlChangeTaskFinishCommand.Create(AControl, ATask,
      ACalendar.GetFinishWorkTime(AStart)) do
      try
        FIsDependent := True;
        Execute;
      finally
        Free;
      end;
end;

{ TdxGanttControlChangeTaskPropertyCommand }

constructor TdxGanttControlChangeTaskPropertyCommand.Create
  (AControl: TdxGanttControlBase; ATask: TdxGanttControlTask;
  const ANewValue: Variant);
begin
  inherited Create(AControl, ATask);
  FNewValue := ANewValue;
end;

procedure TdxGanttControlChangeTaskPropertyCommand.BeforeExecute;
begin
  if HasAssignedValue then
    FOldIsAssigned := Task.IsValueAssigned(GetAssignedValue);
  inherited BeforeExecute;
end;

procedure TdxGanttControlChangeTaskPropertyCommand.DoExecute;
begin
  if HasAssignedValue and not VarIsNull(FNewValue) and
    not Task.IsValueAssigned(GetAssignedValue) then
    SetAssignedValue;
  SetValue;
  if HasAssignedValue and VarIsNull(FNewValue) then
    ResetAssignedValue;
  inherited DoExecute;
end;

function TdxGanttControlChangeTaskPropertyCommand.Enabled: Boolean;
begin
  Result := inherited Enabled and ((GetChangeValueHistoryItemClass = nil) or
    not Task.IsValueAssigned(GetAssignedValue) or (GetValue <> NewValue));
end;

function TdxGanttControlChangeTaskPropertyCommand.GetValue: Variant;
begin
  if GetChangeValueHistoryItemClass = nil then
    Exit(Null);

  with GetChangeValueHistoryItemClass.Create(Control.History, Task) do
    try
      Result := GetValue;
    finally
      Free;
    end;
end;

function TdxGanttControlChangeTaskPropertyCommand.GetValidValue: Variant;
begin
  Result := NewValue;
  if not IsNewValueValid then
    Result := ValidateValue(Result);
end;

function TdxGanttControlChangeTaskPropertyCommand.HasAssignedValue: Boolean;
begin
  Result := True;
end;

procedure TdxGanttControlChangeTaskPropertyCommand.ResetAssignedValue;
begin
  ResetAssignedValue(GetAssignedValue);
end;

procedure TdxGanttControlChangeTaskPropertyCommand.ResetAssignedValue
  (AAssignedValue: TdxGanttTaskAssignedValue);
var
  AHistoryItem: TdxGanttControlTaskResetAssignedValueHistoryItem;
  AHistory: TdxGanttControlHistory;
begin
  if not Task.IsValueAssigned(AAssignedValue) then
    Exit;
  AHistory := Control.History;
  AHistoryItem := TdxGanttControlTaskResetAssignedValueHistoryItem.Create
    (AHistory, Task);
  AHistoryItem.FAssignedValue := AAssignedValue;
  AHistory.AddItem(AHistoryItem);
  AHistoryItem.Execute;
end;

function TdxGanttControlChangeTaskPropertyCommand.IsNewValueValid: Boolean;
begin
  Result := True;
end;

procedure TdxGanttControlChangeTaskPropertyCommand.SetAssignedValue;
begin
  DoSetAssignedValue(GetAssignedValue);
end;

procedure TdxGanttControlChangeTaskPropertyCommand.SetValue;
var
  AHistoryItem: TdxGanttControlChangeTaskPropertyHistoryItem;
  ANewValue: Variant;
begin
  ANewValue := GetValidValue;
  if (ANewValue = GetValue) and FOldIsAssigned then
    Exit;
  AHistoryItem := GetChangeValueHistoryItemClass.Create(Control.History, Task);
  AHistoryItem.FNewValue := ANewValue;
  Control.History.AddItem(AHistoryItem);
  AHistoryItem.Execute;
end;

function TdxGanttControlChangeTaskPropertyCommand.ValidateValue
  (const AValue: Variant): Variant;
begin
  Result := AValue;
end;

{ TdxGanttControlChangeTaskOutlineLevelCommand }

procedure TdxGanttControlChangeTaskOutlineLevelCommand.AfterExecute;
var
  ASummary: TdxGanttControlTask;
begin
  ASummary := TdxTaskCalculator.GetSummary(Task);
  CheckLinks(ASummary);
  if not Task.IsValueAssigned(TdxGanttTaskAssignedValue.PercentComplete) or
    (Task.PercentComplete = 0) then
    TdxDependentLinksCalculator.CalculateLink(Control, Task, False);
  inherited AfterExecute;
  if (FPreviousSummary <> ASummary) and (FPreviousSummary <> nil) and
    FPreviousSummary.Summary then
    TdxDependentSummaryCalculator.UpdateSummary(Control, FPreviousSummary);
  if ASummary <> nil then
    TdxDependentSummaryCalculator.UpdateSummary(Control, ASummary);
end;

procedure TdxGanttControlChangeTaskOutlineLevelCommand.BeforeExecute;
begin
  inherited BeforeExecute;
  FPreviousSummary := TdxTaskCalculator.GetSummary(Task);
end;

function TdxGanttControlChangeTaskOutlineLevelCommand.GetAssignedValue
  : TdxGanttTaskAssignedValue;
begin
  Result := TdxGanttTaskAssignedValue.OutlineLevel;
end;

function TdxGanttControlChangeTaskOutlineLevelCommand.
  GetChangeValueHistoryItemClass
  : TdxGanttControlChangeTaskPropertyHistoryItemClass;
begin
  Result := TdxGanttControlChangeTaskOutlineLevelHistoryItem;
end;

procedure TdxGanttControlChangeTaskOutlineLevelCommand.CheckLinks
  (ATask: TdxGanttControlTask);
var
  I: Integer;
begin
  if (ATask = nil) or not ATask.IsValueAssigned
    (TdxGanttTaskAssignedValue.PredecessorLinks) then
    Exit;
  for I := 0 to ATask.PredecessorLinks.Count - 1 do
  begin
    if not TdxTaskCalculator.AllowLink(ATask,
      ATask.PredecessorLinks[I].PredecessorUID) then
      TdxGanttControlExceptions.
        ThrowOutlineChangeWouldCreateCircularRelationshipException;
  end;
end;

function TdxGanttControlChangeTaskOutlineLevelCommand.GetPreviousTask
  : TdxGanttControlTask;
var
  I: Integer;
begin
  for I := Task.ID - 1 downto 0 do
  begin
    Result := Task.Owner[I];
    if not Result.Blank and (Result.OutlineLevel <= Task.OutlineLevel) then
      Exit;
  end;
  Result := nil;
end;

function TdxGanttControlChangeTaskOutlineLevelCommand.GetSubTasks
  (ARecursive: Boolean = False): TList<TdxGanttControlTask>;
begin
  Result := TdxTaskCalculator.GetSubTasks(Task, False, ARecursive);
end;

procedure TdxGanttControlChangeTaskOutlineLevelCommand.SetTaskSummary
  (ATask: TdxGanttControlTask; ANewValue: Boolean);

  procedure DeletePredecessorLinksByUID(APredecessorLinks
    : TdxGanttControlTaskPredecessorLinks; AUID: Integer);
  var
    I: Integer;
  begin
    for I := APredecessorLinks.Count - 1 downto 0 do
      if APredecessorLinks[I].PredecessorUID = AUID then
        TdxGanttControlChangeTaskPredecessorsCommand.DeletePredecessorLink
          (Control, TdxGanttControlTask(APredecessorLinks.Owner), I);
  end;

var
  I: Integer;
  ASubTasks: TList<TdxGanttControlTask>;
  ASetAutoScheduleModeNeeded: Boolean;
begin
  if ANewValue then
  begin
    if Task <> ATask then
    begin
      DeletePredecessorLinksByUID(Task.PredecessorLinks, ATask.UID);
      ASubTasks := GetSubTasks(True);
      try
        for I := 0 to ASubTasks.Count - 1 do
          DeletePredecessorLinksByUID(ASubTasks[I].PredecessorLinks, ATask.UID);
      finally
        ASubTasks.Free;
      end;
    end
    else
    begin
      ASubTasks := GetSubTasks(True);
      try
        for I := 0 to ASubTasks.Count - 1 do
        begin
          DeletePredecessorLinksByUID(ASubTasks[I].PredecessorLinks, Task.UID);
          DeletePredecessorLinksByUID(Task.PredecessorLinks, ASubTasks[I].UID);
        end;
      finally
        ASubTasks.Free;
      end;
    end;
  end;
  with TdxGanttControlChangeTaskSummaryCommand.Create(Control, ATask,
    ANewValue) do
    try
      ASetAutoScheduleModeNeeded := ANewValue and Enabled;
      Execute;
    finally
      Free;
    end;
  if ATask.IsValueAssigned(TdxGanttTaskAssignedValue.Summary) and ATask.Summary
  then
    CheckLinks(ATask);
  if ASetAutoScheduleModeNeeded then
    SetTaskAutoScheduleMode(ATask);
  if ATask.Summary then
    TdxDependentSummaryCalculator.UpdateSummary(Control, ATask);
end;

procedure TdxGanttControlChangeTaskOutlineLevelCommand.SetTaskAutoScheduleMode
  (ATask: TdxGanttControlTask);
begin
  with TdxGanttControlSetTaskAutoScheduleModeCommand.Create(Control, ATask) do
    try
      Execute;
    finally
      Free;
    end;
end;

{ TdxGanttControlIncreaseTaskOutlineLevelCommand }

constructor TdxGanttControlIncreaseTaskOutlineLevelCommand.Create
  (AControl: TdxGanttControlBase; ATask: TdxGanttControlTask);
var
  AValue: Integer;
begin
  if ATask.Blank or not ATask.IsValueAssigned
    (TdxGanttTaskAssignedValue.OutlineLevel) then
    AValue := 1
  else
    AValue := ATask.OutlineLevel + 1;
  inherited Create(AControl, ATask, AValue);
end;

destructor TdxGanttControlIncreaseTaskOutlineLevelCommand.Destroy;
begin
  FreeAndNil(FSubTasks);
  inherited;
end;

procedure TdxGanttControlIncreaseTaskOutlineLevelCommand.AfterExecute;
var
  I: Integer;
begin
  inherited AfterExecute;
  for I := 0 to FSubTasks.Count - 1 do
    with TdxGanttControlIncreaseTaskOutlineLevelCommand.Create(Control,
      FSubTasks[I]) do
      try
        Execute;
      finally
        Free;
      end;
end;

procedure TdxGanttControlIncreaseTaskOutlineLevelCommand.BeforeExecute;
var
  ATask: TdxGanttControlTask;
begin
  inherited BeforeExecute;
  ATask := GetPreviousTask;
  SetTaskSummary(ATask, True);
  FSubTasks := GetSubTasks;
end;

function TdxGanttControlIncreaseTaskOutlineLevelCommand.Enabled: Boolean;
var
  ATask: TdxGanttControlTask;
begin
  Result := not Task.Blank and inherited Enabled;
  if Result then
  begin
    ATask := GetPreviousTask;
    Result := ATask.OutlineLevel >= Task.OutlineLevel;
  end;
end;

{ TdxGanttControlDecreaseTaskOutlineLevelCommand }

constructor TdxGanttControlDecreaseTaskOutlineLevelCommand.Create
  (AControl: TdxGanttControlBase; ATask: TdxGanttControlTask);
var
  AValue: Integer;
begin
  if ATask.Blank or not ATask.IsValueAssigned
    (TdxGanttTaskAssignedValue.OutlineLevel) then
    AValue := 1
  else
    AValue := ATask.OutlineLevel - 1;
  inherited Create(AControl, ATask, AValue);
end;

procedure TdxGanttControlDecreaseTaskOutlineLevelCommand.AfterExecute;
var
  AList: TList<TdxGanttControlTask>;
begin
  AList := GetSubTasks;
  try
    SetTaskSummary(Task, AList.Count > 0);
  finally
    AList.Free;
  end;
  inherited AfterExecute;
end;

procedure TdxGanttControlDecreaseTaskOutlineLevelCommand.BeforeExecute;
var
  ASummary: TdxGanttControlTask;
  I: Integer;
  AHasTasksInSummary: Boolean;
  AList: TList<TdxGanttControlTask>;
begin
  inherited BeforeExecute;
  ASummary := TdxTaskCalculator.GetSummary(Task);
  if ASummary = nil then
    Exit;
  AHasTasksInSummary := False;
  for I := ASummary.ID + 1 to Task.ID - 1 do
    if not ASummary.Owner[I].Blank then
    begin
      AHasTasksInSummary := True;
      Break;
    end;
  if not AHasTasksInSummary then
    SetTaskSummary(ASummary, False);
  AList := GetSubTasks;
  try
    for I := 0 to AList.Count - 1 do
      with TdxGanttControlDecreaseTaskOutlineLevelCommand.Create(Control,
        AList[I]) do
        try
          Execute;
        finally
          Free;
        end;
  finally
    AList.Free;
  end;
end;

function TdxGanttControlDecreaseTaskOutlineLevelCommand.Enabled: Boolean;
var
  ATask: TdxGanttControlTask;
begin
  Result := not Task.Blank and (NewValue >= 1) and inherited Enabled;
  if Result then
  begin
    ATask := GetPreviousTask;
    Result := ATask.OutlineLevel <= Task.OutlineLevel;
  end;
end;

{ TdxGanttControlChangeTaskSummaryCommand }

procedure TdxGanttControlChangeTaskSummaryCommand.AfterExecute;
var
  AStart: TDateTime;
  APercentComplete: Integer;
  ADuration: TdxGanttControlDuration;
begin
  inherited AfterExecute;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Summary) and
    Task.Summary and not Task.Manual then
    TdxTaskCalculator.ResetConstraint(Control, Task);
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Summary) and
    not Task.Summary and not Task.Manual then
  begin
    if not Task.IsValueAssigned(TdxGanttTaskAssignedValue.PercentComplete) or
      (Task.PercentComplete = 0) then
    begin
      APercentComplete := -1;
      if Task.PredecessorLinks.Count = 0 then
      begin
        AStart := TdxTaskCalculator.CalculateBasicStart(Task);
        TdxTaskCalculator.SetStart(Control, Task, AStart);
      end
      else
        TdxDependentLinksCalculator.CalculateLink(Control, Task, False);
    end
    else
      APercentComplete := Task.PercentComplete;
    ADuration := TdxGanttControlDuration.Create
      (TdxGanttControlDataModel.DefaultTaskDuration);
    TdxTaskCalculator.SetDuration(Control, Task,
      TdxGanttControlDataModel.DefaultTaskDuration);
    TdxTaskCalculator.SetFinish(Control, Task,
      ADuration.GetWorkFinish(Task.Start, Task.RealCalendar,
      Task.DurationFormat));
    if APercentComplete > 0 then
      TdxTaskCalculator.SetPercentComplete(Control, Task, APercentComplete);
  end;
end;

function TdxGanttControlChangeTaskSummaryCommand.GetAssignedValue
  : TdxGanttTaskAssignedValue;
begin
  Result := TdxGanttTaskAssignedValue.Summary;
end;

function TdxGanttControlChangeTaskSummaryCommand.GetChangeValueHistoryItemClass
  : TdxGanttControlChangeTaskPropertyHistoryItemClass;
begin
  Result := TdxGanttControlChangeTaskSummaryHistoryItem;
end;

{ TdxGanttControlChangeTaskNameCommand }

function TdxGanttControlChangeTaskNameCommand.GetAssignedValue
  : TdxGanttTaskAssignedValue;
begin
  Result := TdxGanttTaskAssignedValue.Name;
end;

function TdxGanttControlChangeTaskNameCommand.GetChangeValueHistoryItemClass
  : TdxGanttControlChangeTaskPropertyHistoryItemClass;
begin
  Result := TdxGanttControlChangeTaskNameHistoryItem;
end;

{ TdxGanttControlChangeTaskDisplayOnTimelineCommand }

function TdxGanttControlChangeTaskDisplayOnTimelineCommand.GetAssignedValue
  : TdxGanttTaskAssignedValue;
begin
  Result := TdxGanttTaskAssignedValue.DisplayOnTimeline;
end;

function TdxGanttControlChangeTaskDisplayOnTimelineCommand.
  GetChangeValueHistoryItemClass
  : TdxGanttControlChangeTaskPropertyHistoryItemClass;
begin
  Result := TdxGanttControlChangeTaskDisplayOnTimelineHistoryItem;
end;

{ TdxGanttControlChangeTaskCalendarUIDCommand }

function TdxGanttControlChangeTaskCalendarUIDCommand.GetAssignedValue
  : TdxGanttTaskAssignedValue;
begin
  Result := TdxGanttTaskAssignedValue.CalendarUID;
end;

function TdxGanttControlChangeTaskCalendarUIDCommand.
  GetChangeValueHistoryItemClass
  : TdxGanttControlChangeTaskPropertyHistoryItemClass;
begin
  Result := TdxGanttControlChangeTaskCalendarUIDHistoryItem;
end;

{ TdxGanttControlChangeTaskPercentCompleteCommand }

procedure TdxGanttControlChangeTaskPercentCompleteCommand.AfterExecute;
var
  I: Integer;
begin
  inherited AfterExecute;
  if not FIsDependent then
  begin
    if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Duration) then
      TdxDependentSummaryCalculator.CalculateParentSummaryPercentComplete
        (Control, Task);
    if Task.Summary then
      TdxDependentSummaryCalculator.CalculateSummaryPercentComplete
        (Control, Task);
  end;
  ResetAssignedValue(TdxGanttTaskAssignedValue.PercentWorkComplete);
  ResetAssignedValue(TdxGanttTaskAssignedValue.PhysicalPercentComplete);
  ResetAssignedValue(TdxGanttTaskAssignedValue.ActualDuration);
  ResetAssignedValue(TdxGanttTaskAssignedValue.RemainingDuration);
  ResetAssignedValue(TdxGanttTaskAssignedValue.ActualStart);
  ResetAssignedValue(TdxGanttTaskAssignedValue.ActualFinish);
  ResetAssignedValue(TdxGanttTaskAssignedValue.RemainingWork);
  for I := 0 to DataModel.Assignments.Count - 1 do
    if DataModel.Assignments[I].TaskUID = Task.UID then
    begin
      with TdxGanttControlChangeAssignmentPercentWorkCompleteCommand.Create
        (Control, DataModel.Assignments[I], NewValue) do
        try
          Execute;
        finally
          Free;
        end;
    end;
end;

function TdxGanttControlChangeTaskPercentCompleteCommand.GetAssignedValue
  : TdxGanttTaskAssignedValue;
begin
  Result := TdxGanttTaskAssignedValue.PercentComplete;
end;

function TdxGanttControlChangeTaskPercentCompleteCommand.
  GetChangeValueHistoryItemClass
  : TdxGanttControlChangeTaskPropertyHistoryItemClass;
begin
  Result := TdxGanttControlChangeTaskPercentCompleteHistoryItem;
end;

{ TdxGanttControlChangeTaskWorkVarianceCommand }

function TdxGanttControlChangeTaskWorkVarianceCommand.GetAssignedValue
  : TdxGanttTaskAssignedValue;
begin
  Result := TdxGanttTaskAssignedValue.WorkVariance;
end;

function TdxGanttControlChangeTaskWorkVarianceCommand.
  GetChangeValueHistoryItemClass
  : TdxGanttControlChangeTaskPropertyHistoryItemClass;
begin
  Result := TdxGanttControlChangeTaskWorkVarianceHistoryItem;
end;

{ TdxGanttControlSetTaskMilestoneCommand }

procedure TdxGanttControlSetTaskMilestoneCommand.AfterExecute;
begin
  if NewValue then
  begin
    with TdxGanttControlChangeTaskFinishCommand.Create(Control, Task,
      Task.Start) do
      try
        Execute;
      finally
        Free;
      end;
  end;
  inherited AfterExecute;
end;

function TdxGanttControlSetTaskMilestoneCommand.GetAssignedValue
  : TdxGanttTaskAssignedValue;
begin
  Result := TdxGanttTaskAssignedValue.Milestone;
end;

function TdxGanttControlSetTaskMilestoneCommand.GetChangeValueHistoryItemClass
  : TdxGanttControlChangeTaskPropertyHistoryItemClass;
begin
  Result := TdxGanttControlChangeTaskMilestoneHistoryItem;
end;

{ TdxGanttControlChangeTaskDurationFormatCommand }

procedure TdxGanttControlChangeTaskDurationFormatCommand.AfterExecute;
begin
  inherited AfterExecute;
  if (OldIsAssigned <> Task.IsValueAssigned
    (TdxGanttTaskAssignedValue.DurationFormat)) or
    (FIsElapsed <> TdxGanttControlDuration.IsElapsedFormat(Task.DurationFormat))
  then
    TdxGanttControlChangeTaskDurationCommand.AdjustStartAndFinish
      (Control, Task);
end;

procedure TdxGanttControlChangeTaskDurationFormatCommand.BeforeExecute;
begin
  inherited BeforeExecute;
  if OldIsAssigned then
    FIsElapsed := TdxGanttControlDuration.IsElapsedFormat(Task.DurationFormat);
end;

function TdxGanttControlChangeTaskDurationFormatCommand.GetAssignedValue
  : TdxGanttTaskAssignedValue;
begin
  Result := TdxGanttTaskAssignedValue.DurationFormat;
end;

function TdxGanttControlChangeTaskDurationFormatCommand.
  GetChangeValueHistoryItemClass
  : TdxGanttControlChangeTaskPropertyHistoryItemClass;
begin
  Result := TdxGanttControlChangeTaskDurationFormatHistoryItem;
end;

{ TdxGanttControlChangeTaskEstimatedCommand }

function TdxGanttControlChangeTaskEstimatedCommand.GetAssignedValue
  : TdxGanttTaskAssignedValue;
begin
  Result := TdxGanttTaskAssignedValue.Estimated;
end;

function TdxGanttControlChangeTaskEstimatedCommand.
  GetChangeValueHistoryItemClass
  : TdxGanttControlChangeTaskPropertyHistoryItemClass;
begin
  Result := TdxGanttControlChangeTaskEstimatedHistoryItem;
end;

{ TdxGanttControlChangeTaskManualCommand }

function TdxGanttControlChangeTaskManualCommand.GetAssignedValue
  : TdxGanttTaskAssignedValue;
begin
  Result := TdxGanttTaskAssignedValue.Manual;
end;

function TdxGanttControlChangeTaskManualCommand.GetChangeValueHistoryItemClass
  : TdxGanttControlChangeTaskPropertyHistoryItemClass;
begin
  Result := nil;
end;

procedure TdxGanttControlChangeTaskManualCommand.SetValue;
var
  AValue: Boolean;
begin
  if VarIsStr(NewValue) then
    AValue := NewValue = cxGetResourceString
      (@sdxGanttControlTaskModeManuallyScheduled)
  else
    AValue := (VarType(NewValue) = varBoolean) and NewValue;

  if AValue then
  begin
    with TdxGanttControlSetTaskManuallyScheduleModeCommand.Create
      (Control, Task) do
      try
        Execute;
      finally
        Free;
      end;
  end
  else
  begin
    with TdxGanttControlSetTaskAutoScheduleModeCommand.Create(Control, Task) do
      try
        Execute;
      finally
        Free;
      end;
  end;
end;

{ TdxGanttControlChangeSummaryManualCommand }

function TdxGanttControlChangeSummaryManualCommand.Enabled: Boolean;
begin
  Result := inherited Enabled and Task.Summary and
    Task.IsValueAssigned(TdxGanttTaskAssignedValue.Summary);
end;

function TdxGanttControlChangeSummaryManualCommand.GetAssignedValue
  : TdxGanttTaskAssignedValue;
begin
  Result := TdxGanttTaskAssignedValue.Manual;
end;

function TdxGanttControlChangeSummaryManualCommand.
  GetChangeValueHistoryItemClass
  : TdxGanttControlChangeTaskPropertyHistoryItemClass;
begin
  Result := TdxGanttControlChangeManualHistoryItem;
end;

{ TdxGanttControlChangeTaskDependentPropertyCommand }

constructor TdxGanttControlChangeTaskDependentPropertyCommand.Create
  (AControl: TdxGanttControlBase; ATask: TdxGanttControlTask;
  const ANewValue: Variant);
begin
  inherited Create(AControl, ATask, ANewValue);
  FCachedMinStart := InvalidDate;
end;

procedure TdxGanttControlChangeTaskDependentPropertyCommand.AfterExecute;
begin
  if not Task.Summary or not Task.Manual then
    CalculateDependentSummary;
  CalculateDependentLinks;
  inherited AfterExecute;
end;

procedure TdxGanttControlChangeTaskDependentPropertyCommand.BeforeExecute;
begin
  inherited BeforeExecute;
  if Task.Summary and not FIsDependent then
    SetManual;
end;

procedure TdxGanttControlChangeTaskDependentPropertyCommand.
  CalculateDependentLinks;
begin
  with TdxDependentLinksCalculator.Create(Control, Task) do
    try
      Calculate;
    finally
      Free;
    end;
end;

procedure TdxGanttControlChangeTaskDependentPropertyCommand.
  CalculateDependentSummary;
begin
  with TdxDependentSummaryCalculator.Create(Control, Task) do
    try
      Calculate;
    finally
      Free;
    end;
end;

function TdxGanttControlChangeTaskDependentPropertyCommand.
  DoIsNewValueValid: Boolean;
begin
  Result := True;
end;

function TdxGanttControlChangeTaskDependentPropertyCommand.GetMinStart
  : TDateTime;
begin
  if FCachedMinStart = InvalidDate then
    FCachedMinStart := TdxDependentLinksCalculator.CalculateMinStart(Task);
  Result := FCachedMinStart;
end;

function TdxGanttControlChangeTaskDependentPropertyCommand.
  IsNewValueValid: Boolean;
begin
  Result := FIsDependent or Task.Manual or VarIsNull(NewValue);
  if not Result then
    Result := DoIsNewValueValid;
end;

procedure TdxGanttControlChangeTaskDependentPropertyCommand.SetConstraint
  (AType: TdxGanttControlTaskConstraintType; AValue: TDateTime);
begin
  with TdxGanttControlChangeTaskConstraintTypeCommand.Create(Control,
    Task, AType) do
    try
      FIsDependent := True;
      Execute;
    finally
      Free;
    end;
  with TdxGanttControlChangeTaskConstraintDateCommand.Create(Control, Task,
    AValue) do
    try
      FIsDependent := True;
      Execute;
    finally
      Free;
    end;
end;

procedure TdxGanttControlChangeTaskDependentPropertyCommand.SetManual;
begin
  with TdxGanttControlChangeSummaryManualCommand.Create(Control, Task, True) do
    try
      Execute;
    finally
      Free;
    end;
end;

procedure TdxGanttControlChangeTaskDependentPropertyCommand.SetMilestone
  (Value: Boolean = True);
begin
  with TdxGanttControlSetTaskMilestoneCommand.Create(Control, Task, Value) do
    try
      Execute;
    finally
      Free;
    end;
end;

{ TdxGanttControlChangeTaskConstraintCommand }

procedure TdxGanttControlChangeTaskConstraintCommand.AfterExecute;
var
  AStart, AFinish: TDateTime;
begin
  inherited AfterExecute;
  if FIsDependent or Task.Manual or not Task.IsValueAssigned
    (TdxGanttTaskAssignedValue.Start) or not Task.IsValueAssigned
    (TdxGanttTaskAssignedValue.Finish) then
    Exit;
  AStart := Task.Start;
  AFinish := Task.Finish;
  TdxDependentLinksCalculator.CalculateStartAndFinish(Task, AStart, AFinish);
  if CompareDateTime(Task.Start, AStart) <> 0 then
    with TdxGanttControlChangeTaskStartCommand.Create(Control, Task, AStart) do
      try
        FIsDependent := True;
        Execute;
      finally
        Free;
      end;
end;

{ TdxGanttControlChangeTaskConstraintTypeCommand }

function TdxGanttControlChangeTaskConstraintTypeCommand.Enabled: Boolean;
begin
  Result := inherited Enabled and not Task.Manual;
end;

function TdxGanttControlChangeTaskConstraintTypeCommand.GetAssignedValue
  : TdxGanttTaskAssignedValue;
begin
  Result := TdxGanttTaskAssignedValue.ConstraintType;
end;

function TdxGanttControlChangeTaskConstraintTypeCommand.
  GetChangeValueHistoryItemClass
  : TdxGanttControlChangeTaskPropertyHistoryItemClass;
begin
  Result := TdxGanttControlChangeTaskConstraintTypeHistoryItem;
end;

{ TdxGanttControlChangeTaskConstraintDateCommand }

function TdxGanttControlChangeTaskConstraintDateCommand.Enabled: Boolean;
begin
  Result := inherited Enabled and
    (VarIsNull(NewValue) or
    (Task.IsValueAssigned(TdxGanttTaskAssignedValue.ConstraintType) and
    (Task.ConstraintType <> TdxGanttControlTaskConstraintType.
    AsSoonAsPossible)));
end;

function TdxGanttControlChangeTaskConstraintDateCommand.GetAssignedValue
  : TdxGanttTaskAssignedValue;
begin
  Result := TdxGanttTaskAssignedValue.ConstraintDate;
end;

function TdxGanttControlChangeTaskConstraintDateCommand.
  GetChangeValueHistoryItemClass
  : TdxGanttControlChangeTaskPropertyHistoryItemClass;
begin
  Result := TdxGanttControlChangeTaskConstraintDateHistoryItem;
end;

{ TdxGanttControlChangeTaskTimeBoundsCommand }

function TdxGanttControlChangeTaskTimeBoundsCommand.Enabled: Boolean;
begin
  Result := inherited Enabled and (not VarIsNull(NewValue) or Task.Summary or
    Task.Manual)
end;

{ TdxGanttControlChangeTaskStartCommand }

procedure TdxGanttControlChangeTaskStartCommand.AfterExecute;
begin
  if not Task.IsValueAssigned(TdxGanttTaskAssignedValue.Start) and not FIsDependent
  then
  begin
    with TdxGanttControlChangeTaskDurationCommand.Create(Control, Task, Null) do
      try
        FIsDependent := True;
        Execute;
      finally
        Free;
      end;
  end;
  ResetAssignedValue(TdxGanttTaskAssignedValue.ActualStart);
  ResetAssignedValue(TdxGanttTaskAssignedValue.ManualStart);
  inherited AfterExecute;
end;

procedure TdxGanttControlChangeTaskStartCommand.BeforeExecute;
begin
  inherited BeforeExecute;
  if not FIsDependent and not VarIsNull(NewValue) then
    SetConstraint(TdxGanttControlTaskConstraintType.StartNoEarlierThan,
      NewValue);
end;

function TdxGanttControlChangeTaskStartCommand.DoIsNewValueValid: Boolean;
begin
  Result := CompareDateTime(NewValue, GetMinStart) >= 0;
end;

function TdxGanttControlChangeTaskStartCommand.GetAssignedValue
  : TdxGanttTaskAssignedValue;
begin
  Result := TdxGanttTaskAssignedValue.Start;
end;

function TdxGanttControlChangeTaskStartCommand.GetChangeValueHistoryItemClass
  : TdxGanttControlChangeTaskPropertyHistoryItemClass;
begin
  Result := TdxGanttControlChangeTaskStartHistoryItem;
end;

procedure TdxGanttControlChangeTaskStartCommand.SetValue;
var
  ATaskDuration: string;
  ADuration: TdxGanttControlDuration;
begin
  ATaskDuration := Task.RealDuration;
  inherited SetValue;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Start) and
    (not Task.Summary or not FIsDependent) then
  begin
    if (ATaskDuration = '') and Task.IsValueAssigned
      (TdxGanttTaskAssignedValue.Finish) and (Task.Start >= Task.Finish) then
      ATaskDuration := TdxGanttControlDataModel.DefaultTaskDuration;
    if ATaskDuration <> '' then
    begin
      ADuration := TdxGanttControlDuration.Create(ATaskDuration);
      with TdxGanttControlChangeTaskFinishCommand.Create(Control, Task,
        ADuration.GetWorkFinish(Task.Start, Task.RealCalendar,
        Task.RealDurationFormat)) do
        try
          FIsDependent := True;
          Execute;
        finally
          Free;
        end;
    end;
  end;
end;

function TdxGanttControlChangeTaskStartCommand.ValidateValue
  (const AValue: Variant): Variant;
begin
  Result := Max(AValue, GetMinStart);
end;

{ TdxGanttControlChangeTaskFinishCommand }

procedure TdxGanttControlChangeTaskFinishCommand.AfterExecute;
begin
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Finish) then
  begin
    if not FIsDependent and Task.IsValueAssigned
      (TdxGanttTaskAssignedValue.Start) and (Task.Start > Task.Finish) then
    begin
      if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Manual) and not Task.Manual
      then
        SetStart
      else
      begin
        with TdxGanttControlChangeTaskStartCommand.Create(Control, Task,
          Task.Finish) do
          try
            FIsDependent := True;
            Execute;
          finally
            Free;
          end;
        if not Task.IsValueAssigned(TdxGanttTaskAssignedValue.Milestone) or
          not Task.Milestone then
          SetMilestone;
      end;
    end;
    if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Start) then
      SetDuration
    else if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Duration) then
      SetStart;
  end
  else if not FIsDependent then
  begin
    with TdxGanttControlChangeTaskDurationCommand.Create(Control, Task, Null) do
      try
        FIsDependent := True;
        Execute;
      finally
        Free;
      end;
  end;
  if Task.ID = 0 then
    TdxDependentLinksCalculator.UpdateLateAsPossibleTasks(Control);
  ResetAssignedValue(TdxGanttTaskAssignedValue.ActualFinish);
  ResetAssignedValue(TdxGanttTaskAssignedValue.ManualFinish);
  inherited AfterExecute;
end;

procedure TdxGanttControlChangeTaskFinishCommand.BeforeExecute;
begin
  inherited BeforeExecute;
  if not FIsDependent and not VarIsNull(NewValue) then
    SetConstraint(TdxGanttControlTaskConstraintType.FinishNoEarlierThan,
      NewValue);
end;

function TdxGanttControlChangeTaskFinishCommand.GetAssignedValue
  : TdxGanttTaskAssignedValue;
begin
  Result := TdxGanttTaskAssignedValue.Finish;
end;

function TdxGanttControlChangeTaskFinishCommand.GetChangeValueHistoryItemClass
  : TdxGanttControlChangeTaskPropertyHistoryItemClass;
begin
  Result := TdxGanttControlChangeTaskFinishHistoryItem;
end;

procedure TdxGanttControlChangeTaskFinishCommand.SetDuration;
var
  ADuration: TdxGanttControlDuration;
begin
  ADuration := TdxGanttControlDuration.Create(Task.Start, Task.Finish,
    Task.RealCalendar, Task.RealDurationFormat);
  with TdxGanttControlChangeTaskDurationCommand.Create(Control, Task,
    ADuration.ToString) do
    try
      FIsDependent := True;
      Execute;
    finally
      Free;
    end;
end;

procedure TdxGanttControlChangeTaskFinishCommand.SetStart;
var
  ADuration: TdxGanttControlDuration;
begin
  ADuration := TdxGanttControlDuration.Create(Task.Duration);
  with TdxGanttControlChangeTaskStartCommand.Create(Control, Task,
    ADuration.GetWorkStart(Task.Finish, Task.RealCalendar,
    Task.RealDurationFormat)) do
    try
      FIsDependent := True;
      Execute;
    finally
      Free;
    end;
end;

{ TdxGanttControlChangeTaskDurationCommand }

class procedure TdxGanttControlChangeTaskDurationCommand.AdjustStartAndFinish
  (AControl: TdxGanttControlBase; ATask: TdxGanttControlTask);
var
  ADuration: TdxGanttControlDuration;
  AStart: TDateTime;
begin
  if ATask.IsValueAssigned(TdxGanttTaskAssignedValue.Duration) then
  begin
    ADuration := TdxGanttControlDuration.Create(ATask.Duration);
    if ATask.IsValueAssigned(TdxGanttTaskAssignedValue.Start) then
    begin
      AStart := ATask.RealCalendar.GetNextWorkTime(ATask.Start);
      if AStart <> ATask.Start then
        SetStart(AControl, ATask, AStart)
      else
      begin
        if ADuration.IsZero then
          SetFinish(AControl, ATask, AStart)
        else
          SetFinish(AControl, ATask, ADuration.GetWorkFinish(AStart,
            ATask.RealCalendar, ATask.RealDurationFormat));
      end;
    end
    else if ATask.IsValueAssigned(TdxGanttTaskAssignedValue.Finish) then
      SetStart(AControl, ATask, ADuration.GetWorkStart(ATask.Finish,
        ATask.RealCalendar, ATask.RealDurationFormat));
    TdxDependentLinksCalculator.UpdateTaskStart(AControl, ATask);
  end
  else
  begin
    with TdxGanttControlChangeTaskFinishCommand.Create(AControl, ATask, Null) do
      try
        FIsDependent := True;
        Execute;
      finally
        Free;
      end;
  end;
end;

procedure TdxGanttControlChangeTaskDurationCommand.AfterExecute;
var
  APercentComplete: Integer;
  ADurationAsSeconds: Int64;
begin
  if not FIsDependent then
    AdjustStartAndFinish(Control, Task);
  if not VarIsNull(NewValue) and Task.IsValueAssigned
    (TdxGanttTaskAssignedValue.Start) and
    Task.IsValueAssigned(TdxGanttTaskAssignedValue.Finish) then
    SetMilestone(Task.Start = Task.Finish);
  TdxDependentSummaryCalculator.CalculateSummaryWorkVariance(Control,
    TdxDependentSummaryCalculator.GetSummary(Task));
  if not Task.Summary and Task.IsValueAssigned
    (TdxGanttTaskAssignedValue.PercentComplete) then
  begin
    ADurationAsSeconds := TdxGanttControlDuration.Create(Task.Duration)
      .ToSeconds;
    if ADurationAsSeconds = 0 then
    begin
      if Task.PercentComplete > 0 then
        APercentComplete := 100
      else
        APercentComplete := 0;
    end
    else
      APercentComplete :=
        Max(0, Min(100, Ceil(FOldSeconds * Task.PercentComplete /
        ADurationAsSeconds)));
    with TdxGanttControlChangeTaskPercentCompleteCommand.Create(Control, Task,
      APercentComplete) do
      try
        Execute;
      finally
        Free;
      end;
  end;
  ResetAssignedValue(TdxGanttTaskAssignedValue.ActualDuration);
  ResetAssignedValue(TdxGanttTaskAssignedValue.RemainingDuration);
  inherited AfterExecute;
end;

procedure TdxGanttControlChangeTaskDurationCommand.BeforeExecute;
var
  ADuration: TdxGanttControlDuration;
begin
  inherited BeforeExecute;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Duration) then
  begin
    ADuration := TdxGanttControlDuration.Create(Task.Duration);
    FOldSeconds := ADuration.ToSeconds;
  end
  else
    FOldSeconds := 0;
end;

class procedure TdxGanttControlChangeTaskDurationCommand.SetFinish
  (AControl: TdxGanttControlBase; ATask: TdxGanttControlTask; Value: TDateTime);
begin
  with TdxGanttControlChangeTaskFinishCommand.Create(AControl, ATask, Value) do
    try
      FIsDependent := True;
      Execute;
    finally
      Free;
    end;
end;

class procedure TdxGanttControlChangeTaskDurationCommand.SetStart
  (AControl: TdxGanttControlBase; ATask: TdxGanttControlTask; Value: TDateTime);
begin
  with TdxGanttControlChangeTaskStartCommand.Create(AControl, ATask, Value) do
    try
      FIsDependent := True;
      Execute;
    finally
      Free;
    end;
end;

function TdxGanttControlChangeTaskDurationCommand.GetAssignedValue
  : TdxGanttTaskAssignedValue;
begin
  Result := TdxGanttTaskAssignedValue.Duration;
end;

function TdxGanttControlChangeTaskDurationCommand.GetChangeValueHistoryItemClass
  : TdxGanttControlChangeTaskPropertyHistoryItemClass;
begin
  Result := TdxGanttControlChangeTaskDurationHistoryItem;
end;

{ TdxGanttControlChangeTaskPredecessorsCommand }

procedure TdxGanttControlChangeTaskPredecessorsCommand.AfterExecute;
begin
  inherited AfterExecute;
  TdxDependentLinksCalculator.CalculateLink(Control, Task, True)
end;

class procedure TdxGanttControlChangeTaskPredecessorsCommand.
  DeletePredecessorLink(AControl: TdxGanttControlBase;
  ATask: TdxGanttControlTask; AIndex: Integer);
var
  AHistoryItem: TdxGanttControlRemoveItemHistoryItem;
begin
  AHistoryItem := TdxGanttControlRemovePredecessorHistoryItem.Create
    (AControl.History, ATask);
  AHistoryItem.Index := AIndex;
  AControl.History.AddItem(AHistoryItem);
  AHistoryItem.Execute;
end;

function TdxGanttControlChangeTaskPredecessorsCommand.CanCreatePredecessorLink
  (APredecessorUID: Integer): Boolean;
begin
  Result := (Task.PredecessorLinks.GetItemByPredecessorUID(APredecessorUID)
    = nil) and TdxTaskCalculator.AllowLink(Task, APredecessorUID);
end;

procedure TdxGanttControlChangeTaskPredecessorsCommand.CreatePredecessorLink
  (APredecessorUID: Integer);

  function CreateNewTask: Integer;
  var
    AHistoryItem: TdxGanttControlCreateItemHistoryItem;
    AHistory: TdxGanttControlHistory;
  begin
    AHistory := Control.History;
    AHistoryItem := TdxGanttControlCreateItemHistoryItem.Create(AHistory,
      Task.Owner);
    AHistoryItem.Index := Task.Owner.Count;
    AHistory.AddItem(AHistoryItem);
    AHistoryItem.Execute;
    Result := Task.Owner[AHistoryItem.Index].UID;
  end;

  function IsTaskSummary(ATask, ACandidate: TdxGanttControlTask): Boolean;
  begin
    Result := ATask = ACandidate;
    if not Result and (ATask <> nil) then
      Result := IsTaskSummary(TdxTaskCalculator.GetSummary(ATask), ACandidate);
  end;

var
  AHistoryItem: TdxGanttControlCreateItemHistoryItem;
  APredecessor: TdxGanttControlTask;
  ATask: TdxGanttControlTask;
begin
  if APredecessorUID < 0 then
    APredecessorUID := CreateNewTask;
  ATask := Task.Owner.GetItemByUID(APredecessorUID);
  if not ATask.IsValueAssigned(TdxGanttTaskAssignedValue.Blank) or ATask.Blank
  then
  begin
    MakeTaskNotNull(ATask);
    with TdxGanttControlSetTaskModeCommand.CreateCommand(Control, ATask) do
      try
        Execute;
      finally
        Free;
      end;
    if ATask.Manual then
      TdxGanttControlSetTaskAutoScheduleModeCommand.SetBasicStartAndFinish
        (Control, ATask);
  end;
  if not CanCreatePredecessorLink(APredecessorUID) then
  begin
    if FRaiseValidateException then
    begin
      if Task.PredecessorLinks.GetItemByPredecessorUID(APredecessorUID) <> nil
      then
        TdxGanttControlExceptions.ThrowTasksCannotBeLinkedTwiceException
      else
      begin
        APredecessor := Task.Owner.GetItemByUID(APredecessorUID);
        if APredecessor <> nil then
        begin
          if APredecessor.PredecessorLinks.GetItemByPredecessorUID(Task.UID) <> nil
          then
            TdxGanttControlExceptions.ThrowTasksAreAlreadyLinkedException
          else if IsTaskSummary(Task, APredecessor) then
            TdxGanttControlExceptions.
              ThrowCannotLinkSummaryTaskToItsSubtaskException
          else
            TdxGanttControlExceptions.
              ThrowTasksAreAlreadyLinkedThroughAnotherTaskChainException;
        end;
      end;
    end;
    Exit;
  end;
  AHistoryItem := TdxGanttControlCreatePredecessorHistoryItem.Create
    (Control.History, Task);
  AHistoryItem.Index := Task.PredecessorLinks.Count;
  Control.History.AddItem(AHistoryItem);
  AHistoryItem.Execute;
  Task.PredecessorLinks[AHistoryItem.Index].PredecessorUID := APredecessorUID;
end;

procedure TdxGanttControlChangeTaskPredecessorsCommand.DeletePredecessorLink
  (AIndex: Integer);
begin
  TdxGanttControlChangeTaskPredecessorsCommand.DeletePredecessorLink(Control,
    Task, AIndex);
end;

function TdxGanttControlChangeTaskPredecessorsCommand.GetAssignedValue
  : TdxGanttTaskAssignedValue;
begin
  Result := TdxGanttTaskAssignedValue.PredecessorLinks;
end;

function TdxGanttControlChangeTaskPredecessorsCommand.
  GetChangeValueHistoryItemClass
  : TdxGanttControlChangeTaskPropertyHistoryItemClass;
begin
  Result := nil;
end;

procedure TdxGanttControlChangeTaskPredecessorsCommand.SetValue;
var
  AValue: string;
begin
  if VarIsNull(NewValue) or VarIsArray(NewValue) or
    VarIsType(NewValue, varInteger) then
    SetValueAsArray(NewValue)
  else if VarIsStr(NewValue) then
  begin
    AValue := Trim(VarToStr(NewValue));
    if AValue = '' then
      SetValueAsArray(Null)
    else
      SetValueAsString(AValue);
  end;
end;

procedure TdxGanttControlChangeTaskPredecessorsCommand.SetValueAsArray
  (const Value: Variant);
var
  AList: TList<Integer>;
  I: Integer;
  ATask: TdxGanttControlTask;
begin
  AList := TList<Integer>.Create;
  try
    if not VarIsNull(Value) then
      if VarIsArray(Value) then
        for I := VarArrayLowBound(Value, 1) to VarArrayHighBound(Value, 1) do
          AList.Add(Value[I])
      else
        AList.Add(Value);
    for I := Task.PredecessorLinks.Count - 1 downto 0 do
    begin
      if not Task.PredecessorLinks[I].IsValueAssigned
        (TdxGanttTaskPredecessorLinkAssignedValue.PredecessorUID) then
      begin
        DeletePredecessorLink(I);
        Continue;
      end;
      ATask := Task.Owner.GetItemByUID(Task.PredecessorLinks[I].PredecessorUID);
      if ATask = nil then
      begin
        DeletePredecessorLink(I);
        Continue;
      end;
      if AList.IndexOf(ATask.ID) <> -1 then
        AList.Extract(ATask.ID)
      else
        DeletePredecessorLink(I);
    end;
    for I := 0 to AList.Count - 1 do
      CreatePredecessorLink(Task.Owner[AList[I]].UID);
  finally
    AList.Free;
  end;
end;

procedure TdxGanttControlChangeTaskPredecessorsCommand.SetValueAsString
  (const Value: string);

  procedure ChangeIndex(ALink: TdxGanttControlTaskPredecessorLink;
    ANewIndex: Integer);
  var
    AHistoryItem: TdxGanttControlListChangeIndexItem;
    AHistory: TdxGanttControlHistory;
    AIndex: Integer;
  begin
    AIndex := Task.PredecessorLinks.IndexOf(ALink);
    if AIndex = ANewIndex then
      Exit;
    AHistory := Control.History;
    AHistoryItem := TdxGanttControlListChangeIndexItem.Create(AHistory,
      Task.PredecessorLinks);
    AHistoryItem.Index := AIndex;
    AHistoryItem.NewIndex := ANewIndex;
    AHistory.AddItem(AHistoryItem);
    AHistoryItem.Execute;
  end;

var
  ALinks: TdxGanttControlTaskCandidatePredecessorLinks;
  I: Integer;
  ALink: TdxGanttControlTaskPredecessorLink;
begin
  ALinks := TdxGanttControlTaskCandidatePredecessorLinks.CreateFromString
    (Task, Value);
  try
    if ALinks = nil then
    begin
      MessageDlg(cxGetResourceString
        (@sdxGanttControlMessageInvalidPredecessorInformation), mtWarning,
        [mbOK], 0);
      Exit;
    end;
    if not ALinks.CheckDuplicate then
    begin
      ALinks.RemoveDuplicated;
      MessageDlg(cxGetResourceString
        (@sdxGanttControlExceptionTasksCannotBeLinkedTwice), mtInformation,
        [mbOK], 0);
    end;
    for I := Task.PredecessorLinks.Count - 1 downto 0 do
    begin
      ALink := ALinks.GetItemByPredecessorUID
        (Task.PredecessorLinks[I].PredecessorUID);
      if ALink = nil then
        DeletePredecessorLink(I);
    end;
    for I := 0 to ALinks.Count - 1 do
    begin
      ALink := Task.PredecessorLinks.GetItemByPredecessorUID
        (ALinks[I].PredecessorUID);
      if ALink = nil then
      begin
        CreatePredecessorLink(ALinks[I].PredecessorUID);
        ALink := Task.PredecessorLinks[Task.PredecessorLinks.Count - 1];
      end;
      with TdxGanttControlChangeTaskPredecessorCommand.Create(Control, ALink,
        ALinks[I]) do
        try
          Execute;
        finally
          Free;
        end;
      ChangeIndex(ALink, I);
    end;
  finally
    ALinks.Free;
  end;
end;

{ TdxGanttControlTaskAddPredecessorCommand }

procedure TdxGanttControlTaskAddPredecessorCommand.SetValue;
begin
  CreatePredecessorLink(NewValue);
end;

{ TdxGanttControlChangeTaskPredecessorLinkPropertyCommand }

constructor TdxGanttControlChangeTaskPredecessorLinkPropertyCommand.Create
  (AControl: TdxGanttControlBase; ALink: TdxGanttControlTaskPredecessorLink;
  const ANewValue: Variant);
begin
  inherited Create(AControl, ALink.Task);
  FLink := ALink;
  FNewValue := ANewValue;
end;

procedure TdxGanttControlChangeTaskPredecessorLinkPropertyCommand.DoExecute;
begin
  if not VarIsNull(FNewValue) and not FLink.IsValueAssigned(GetAssignedValue)
  then
    SetAssignedValue;
  SetValue;
  if VarIsNull(FNewValue) then
    ResetAssignedValue;
end;

function TdxGanttControlChangeTaskPredecessorLinkPropertyCommand.
  Enabled: Boolean;
begin
  Result := inherited Enabled and (not Link.IsValueAssigned(GetAssignedValue) or
    (GetValue <> NewValue));
end;

function TdxGanttControlChangeTaskPredecessorLinkPropertyCommand.
  GetValue: Variant;
begin
  with GetChangeValueHistoryItemClass.Create(Control.History, Link) do
    try
      Result := GetValue;
    finally
      Free;
    end;
end;

procedure TdxGanttControlChangeTaskPredecessorLinkPropertyCommand.
  ResetAssignedValue;
var
  AHistoryItem: TdxGanttControlTaskPredecessorLinkResetAssignedValueHistoryItem;
  AHistory: TdxGanttControlHistory;
begin
  AHistory := Control.History;
  AHistoryItem :=
    TdxGanttControlTaskPredecessorLinkResetAssignedValueHistoryItem.Create
    (AHistory, Link);
  AHistoryItem.FAssignedValue := GetAssignedValue;
  AHistory.AddItem(AHistoryItem);
  AHistoryItem.Execute;
end;

procedure TdxGanttControlChangeTaskPredecessorLinkPropertyCommand.
  SetAssignedValue;
var
  AHistoryItem: TdxGanttControlTaskPredecessorLinkSetAssignedValueHistoryItem;
  AHistory: TdxGanttControlHistory;
begin
  AHistory := Control.History;
  AHistoryItem := TdxGanttControlTaskPredecessorLinkSetAssignedValueHistoryItem.
    Create(AHistory, Link);
  AHistoryItem.FAssignedValue := GetAssignedValue;
  AHistory.AddItem(AHistoryItem);
  AHistoryItem.Execute;
end;

procedure TdxGanttControlChangeTaskPredecessorLinkPropertyCommand.SetValue;
var
  AHistoryItem: TdxGanttControlChangeTaskPredecessorLinkPropertyHistoryItem;
begin
  AHistoryItem := GetChangeValueHistoryItemClass.Create(Control.History, Link);
  AHistoryItem.FNewValue := NewValue;
  Control.History.AddItem(AHistoryItem);
  AHistoryItem.Execute;
end;

{ TdxGanttControlChangeTaskPredecessorLinkTypeCommand }

function TdxGanttControlChangeTaskPredecessorLinkTypeCommand.GetAssignedValue
  : TdxGanttTaskPredecessorLinkAssignedValue;
begin
  Result := TdxGanttTaskPredecessorLinkAssignedValue.&Type;
end;

function TdxGanttControlChangeTaskPredecessorLinkTypeCommand.
  GetChangeValueHistoryItemClass
  : TdxGanttControlChangeTaskPredecessorLinkPropertyHistoryItemClass;
begin
  Result := TdxGanttControlChangeTaskPredecessorLinkTypeHistoryItem;
end;

{ TdxGanttControlChangeTaskPredecessorLinkLagCommand }

function TdxGanttControlChangeTaskPredecessorLinkLagCommand.GetAssignedValue
  : TdxGanttTaskPredecessorLinkAssignedValue;
begin
  Result := TdxGanttTaskPredecessorLinkAssignedValue.LinkLag;
end;

function TdxGanttControlChangeTaskPredecessorLinkLagCommand.
  GetChangeValueHistoryItemClass
  : TdxGanttControlChangeTaskPredecessorLinkPropertyHistoryItemClass;
begin
  Result := TdxGanttControlChangeTaskPredecessorLinkLagHistoryItem;
end;

{ TdxGanttControlChangeTaskPredecessorLinkLagFormatCommand }

function TdxGanttControlChangeTaskPredecessorLinkLagFormatCommand.
  GetAssignedValue: TdxGanttTaskPredecessorLinkAssignedValue;
begin
  Result := TdxGanttTaskPredecessorLinkAssignedValue.LagFormat;
end;

function TdxGanttControlChangeTaskPredecessorLinkLagFormatCommand.
  GetChangeValueHistoryItemClass
  : TdxGanttControlChangeTaskPredecessorLinkPropertyHistoryItemClass;
begin
  Result := TdxGanttControlChangeTaskPredecessorLinkLagFormatHistoryItem;
end;

{ TdxGanttControlChangeTaskPredecessorCommand }

constructor TdxGanttControlChangeTaskPredecessorCommand.Create
  (AControl: TdxGanttControlBase;
  ALink, ANewLink: TdxGanttControlTaskPredecessorLink);
begin
  inherited Create(AControl, ALink.Task);
  FLink := ALink;
  FNewLink := ANewLink;
  FCommands := TdxFastObjectList.Create;
  if FNewLink <> nil then
  begin
    FCommands.Add(TdxGanttControlChangeTaskPredecessorLinkTypeCommand.Create
      (Control, FLink, FNewLink.&Type));
    FCommands.Add(TdxGanttControlChangeTaskPredecessorLinkLagCommand.Create
      (Control, FLink, FNewLink.LinkLag));
    FCommands.Add(TdxGanttControlChangeTaskPredecessorLinkLagFormatCommand.
      Create(Control, FLink, FNewLink.LagFormat));
  end;
end;

destructor TdxGanttControlChangeTaskPredecessorCommand.Destroy;
begin
  FreeAndNil(FCommands);
  inherited Destroy;
end;

procedure TdxGanttControlChangeTaskPredecessorCommand.AfterExecute;
begin
  inherited AfterExecute;
  TdxDependentLinksCalculator.CalculateLink(Control, Task, True)
end;

procedure TdxGanttControlChangeTaskPredecessorCommand.Delete;
begin
  TdxGanttControlChangeTaskPredecessorsCommand.DeletePredecessorLink(Control,
    Task, Task.PredecessorLinks.IndexOf(FLink));
end;

procedure TdxGanttControlChangeTaskPredecessorCommand.DoExecute;
var
  I: Integer;
  ACommand: TdxGanttControlChangeTaskPredecessorLinkPropertyCommand;
begin
  inherited DoExecute;
  if FNewLink = nil then
    Delete
  else
    for I := 0 to FCommands.Count - 1 do
    begin
      ACommand := TdxGanttControlChangeTaskPredecessorLinkPropertyCommand
        (FCommands[I]);
      if FNewLink.IsValueAssigned(ACommand.GetAssignedValue) then
        ACommand.Execute;
    end;
end;

function TdxGanttControlChangeTaskPredecessorCommand.Enabled: Boolean;
var
  I: Integer;
begin
  Result := inherited Enabled;
  if Result and (FNewLink <> nil) then
  begin
    for I := 0 to FCommands.Count - 1 do
      if TdxGanttControlCommand(FCommands[I]).Enabled then
        Exit;
    Result := False;
  end;
end;

{ TdxGanttControlChangeTaskResourcesCommand }

function TdxGanttControlChangeTaskResourcesCommand.GetAssignedValue
  : TdxGanttTaskAssignedValue;
begin
  Result := TdxGanttTaskAssignedValue(-1);
end;

function TdxGanttControlChangeTaskResourcesCommand.
  GetChangeValueHistoryItemClass
  : TdxGanttControlChangeTaskPropertyHistoryItemClass;
begin
  Result := nil;
end;

function TdxGanttControlChangeTaskResourcesCommand.HasAssignedValue: Boolean;
begin
  Result := False;
end;

procedure TdxGanttControlChangeTaskResourcesCommand.SetValue;
var
  AValue: string;
begin
  if VarIsNull(NewValue) or VarIsArray(NewValue) or
    VarIsType(NewValue, varInteger) then
    SetValueAsArray(NewValue)
  else if VarIsStr(NewValue) then
  begin
    AValue := Trim(VarToStr(NewValue));
    if AValue = '' then
      SetValueAsArray(Null)
    else
      SetValueAsString(AValue);
  end;
end;

procedure TdxGanttControlChangeTaskResourcesCommand.SetValueAsArray
  (const Value: Variant);
var
  AList: TList<Integer>;
  I: Integer;
  AIndex: Integer;
begin
  AList := TList<Integer>.Create;
  try
    if not VarIsNull(Value) then
      if VarIsArray(Value) then
        for I := VarArrayLowBound(Value, 1) to VarArrayHighBound(Value, 1) do
          AList.Add(Value[I])
      else
        AList.Add(Value);
    for I := DataModel.Assignments.Count - 1 downto 0 do
    begin
      if not DataModel.Assignments[I].IsValueAssigned
        (TdxGanttAssignmentAssignedValue.ResourceUID) or
        not DataModel.Assignments[I].IsValueAssigned
        (TdxGanttAssignmentAssignedValue.TaskUID) or
        (DataModel.Assignments[I].TaskUID <> Task.UID) or
        (DataModel.Assignments[I].ResourceUID < 0) then
        Continue;

      AIndex := AList.IndexOf(DataModel.Assignments[I].ResourceUID);
      if AIndex <> -1 then
        AList.Delete(AIndex)
      else
        DeleteAssignment(I);
    end;
    for I := 0 to AList.Count - 1 do
      CreateAssignment(AList[I]);
  finally
    AList.Free;
  end;
end;

procedure TdxGanttControlChangeTaskResourcesCommand.SetValueAsString
  (const Value: string);

  function CreateNewResource: TdxGanttControlResource;
  var
    AResources: TdxGanttControlResources;
    AHistoryItem: TdxGanttControlCreateItemHistoryItem;
  begin
    AResources := TdxGanttControlDataModel(Task.DataModel).Resources;
    AHistoryItem := TdxGanttControlCreateItemHistoryItem.Create(Control.History,
      AResources);
    AHistoryItem.Index := AResources.Count;
    Control.History.AddItem(AHistoryItem);
    AHistoryItem.Execute;
    Result := AResources[AHistoryItem.Index];
  end;

  function AddNewResource(const AName: string): Integer;
  var
    AResource: TdxGanttControlResource;
  begin
    AResource := CreateNewResource;
    with TdxGanttControlChangeResourceNameCommand.Create(Control,
      AResource, AName) do
      try
        Execute;
      finally
        Free;
      end;
    Result := AResource.UID;
  end;

var
  AResources: TdxGanttControlTaskCandidateResources;
  AResource: TdxGanttControlTaskCandidateResource;
  I, AUID: Integer;
  AArray: TArray<Integer>;
begin
  AResources := TdxGanttControlTaskCandidateResources.CreateFromString
    (Task, Value);
  try
    if not AResources.CheckDuplicate then
    begin
      AResources.RemoveDuplicated;
      MessageDlg(cxGetResourceString
        (@sdxGanttControlMessageTwiceResourcesInformation), mtInformation,
        [mbOK], 0);
    end;

    SetLength(AArray, AResources.Count);
    for I := 0 to AResources.Count - 1 do
    begin
      AResource := TdxGanttControlTaskCandidateResource(AResources[I]);
      if AResource.Resource <> nil then
        AUID := AResource.Resource.UID
      else
        AUID := AddNewResource(AResource.Name);
      AArray[I] := AUID;
    end;
    SetValueAsArray(AArray);
  finally
    AResources.Free;
  end;
end;

procedure TdxGanttControlChangeTaskResourcesCommand.CreateAssignment
  (AResourceUID: Integer);
var
  AHistoryItem: TdxGanttControlCreateItemHistoryItem;
begin
  AHistoryItem := TdxGanttControlCreateItemHistoryItem.Create(Control.History,
    DataModel.Assignments);
  AHistoryItem.Index := DataModel.Assignments.Count;
  Control.History.AddItem(AHistoryItem);
  AHistoryItem.Execute;
  DataModel.Assignments[AHistoryItem.Index].TaskUID := Task.UID;
  DataModel.Assignments[AHistoryItem.Index].ResourceUID := AResourceUID;
end;

procedure TdxGanttControlChangeTaskResourcesCommand.DeleteAssignment
  (AIndex: Integer);
begin
  TdxGanttControlChangeResourceCustomCommand.DeleteAssignment(Control,
    DataModel.Assignments, AIndex);
end;

end.
