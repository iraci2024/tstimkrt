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

unit dxGanttControlXMLImporter;

{$I cxVer.inc}
{$I dxGanttControl.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes, System.Contnrs,
{$ENDIF}
  SysUtils, Generics.Defaults, Generics.Collections, Classes,
  dxCore, dxCoreClasses, dxXMLDoc,
  dxGanttControlCustomDataModel,
  dxGanttControlDataModel;

type
  { TdxGanttControlXMLImporter }

  TdxGanttControlXMLImporter = class(TdxGanttControlCustomImporter)
  protected
    class function GetDisplayName: string; override;
    class function GetExtensions: TArray<string>; override;
    function DoLoadFromStream(AStream: TStream): Boolean; override;
  end;

implementation

uses
  dxCultureInfo,
  dxGanttControlTasks,
  dxGanttControlOutlineCodes,
  dxGanttControlExtendedAttributes,
  dxGanttControlCalendars,
  dxGanttControlResources,
  dxGanttControlAssignments;

type
  TdxGanttControlElementCustomListAccess = class
    (TdxGanttControlModelElementList);
  TdxGanttControlCalendarAccess = class(TdxGanttControlCalendar);
  TdxGanttControlResourceAccess = class(TdxGanttControlResource);
  TdxGanttControlCustomBaselineAccess = class(TdxGanttControlCustomBaseline);
  TdxGanttControlTimephasedDataItemAccess = class
    (TdxGanttControlTimephasedDataItem);
  TdxGanttControlAssignmentAccess = class(TdxGanttControlAssignment);
  TdxGanttControlTaskAccess = class(TdxGanttControlTask);
  TdxGanttControlTaskPredecessorLinkAccess = class
    (TdxGanttControlTaskPredecessorLink);

  { TdxXMLNodeHelper }

  TdxXMLNodeHelper = class helper for TdxXMLNode
  private
    function GetTextAsDuration: TDuration;
    function GetTextAsInteger: Integer;
    function GetTextAsNumber: Double;
    function GetTextAsTime: TTime;
  public
    property TextAsDuration: TDuration read GetTextAsDuration;
    property TextAsInteger: Integer read GetTextAsInteger;
    property TextAsNumber: Double read GetTextAsNumber;
    property TextAsTime: TTime read GetTextAsTime;
  end;

  { TdxGanttControlXMLImporterCustomState }

  TdxGanttControlXMLImporterCustomState = class
  public type
    TGetStateHandler = function(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState of object;
    TStateHandlers = class(TDictionary<string, TGetStateHandler>);
  strict private
    FRootNode: TdxXMLNode;
    FStateHandlers: TStateHandlers;
  protected
    procedure DoImport(ANode: TdxXMLNode); virtual;
    function CreateStateHandlers: TStateHandlers; virtual;

    procedure AfterImport; virtual;
    procedure BeforeImport; virtual;

    procedure ThrowInvalidFormatException;

    property StateHandlers: TStateHandlers read FStateHandlers;
  public
    constructor Create(ARootNode: TdxXMLNode);
    destructor Destroy; override;

    procedure Import;

    property RootNode: TdxXMLNode read FRootNode;
  end;

  { TdxGanttControlXMLImporterCustomElementState }

  TdxGanttControlXMLImporterCustomElementState = class
    (TdxGanttControlXMLImporterCustomState)
  strict private
    FElement: TdxGanttControlModelElement;
  public
    constructor Create(ARootNode: TdxXMLNode;
      AElement: TdxGanttControlModelElement); reintroduce;

    property Element: TdxGanttControlModelElement read FElement;
  end;

  { TdxGanttControlXMLImporterProjectState }

  TdxGanttControlXMLImporterProjectState = class
    (TdxGanttControlXMLImporterCustomState)
  strict private
    FDataModel: TdxGanttControlDataModel;

    procedure CheckVersion(const Value: Integer);

    function AssignmentsHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
    function CalendarsHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
    function ExtendedAttributesHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
    function OutlineCodesHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
    function ResourcesHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
    function TasksHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
    function WBSMasksHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    procedure BeforeImport; override;
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
    procedure DoImport(ANode: TdxXMLNode); override;
  public
    constructor Create(ARootNode: TdxXMLNode;
      ADataModel: TdxGanttControlDataModel); reintroduce;

    property DataModel: TdxGanttControlDataModel read FDataModel;
  end;

  { TdxGanttControlXMLImporterTimePeriodState }

  TdxGanttControlXMLImporterTimePeriodState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetTimePeriod: TdxGanttControlTimePeriod; inline;
  protected
    procedure DoImport(ANode: TdxXMLNode); override;
  public
    property TimePeriod: TdxGanttControlTimePeriod read GetTimePeriod;
  end;

  { TdxGanttControlXMLImporterWorkTimeState }

  TdxGanttControlXMLImporterWorkTimeState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetWorkTime: TdxGanttControlCalendarWeekDayWorkTime; inline;
  protected
    procedure DoImport(ANode: TdxXMLNode); override;
  public
    property WorkTime: TdxGanttControlCalendarWeekDayWorkTime read GetWorkTime;
  end;

  { TdxGanttControlXMLImporterWorkTimesState }

  TdxGanttControlXMLImporterWorkTimesState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetWorkTimes: TdxGanttControlCalendarWeekDayWorkTimes; inline;
    function WorkTimeHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
  public
    property WorkTimes: TdxGanttControlCalendarWeekDayWorkTimes
      read GetWorkTimes;
  end;

  { TdxGanttControlXMLImporterCalendarExceptionState }

  TdxGanttControlXMLImporterCalendarExceptionState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetException: TdxGanttControlCalendarException; inline;
    function TimePeriodHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
    function WorkTimesHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    procedure BeforeImport; override;
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
    procedure DoImport(ANode: TdxXMLNode); override;
  public
    property Exception: TdxGanttControlCalendarException read GetException;
  end;

  { TdxGanttControlXMLImporterCalendarExceptionsState }

  TdxGanttControlXMLImporterCalendarExceptionsState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetExceptions: TdxGanttControlCalendarExceptions; inline;

    function ExceptionHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
  public
    property Exceptions: TdxGanttControlCalendarExceptions read GetExceptions;
  end;

  { TdxGanttControlXMLImporterCalendarWeekDayState }

  TdxGanttControlXMLImporterCalendarWeekDayState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetWeekDay: TdxGanttControlCalendarWeekDay; inline;
    function TimePeriodHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
    function WorkTimesHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    procedure AfterImport; override;
    procedure BeforeImport; override;
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
    procedure DoImport(ANode: TdxXMLNode); override;
  public
    property WeekDay: TdxGanttControlCalendarWeekDay read GetWeekDay;
  end;

  { TdxGanttControlXMLImporterCalendarWeekDaysState }

  TdxGanttControlXMLImporterCalendarWeekDaysState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetWeekDays: TdxGanttControlCalendarWeekDays; inline;
    function WeekDaysHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
  public
    property WeekDays: TdxGanttControlCalendarWeekDays read GetWeekDays;
  end;

  { TdxGanttControlXMLImporterCalendarWorkWeekState }

  TdxGanttControlXMLImporterCalendarWorkWeekState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetWorkWeek: TdxGanttControlCalendarWorkWeek; inline;
    function TimePeriodHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
    function WeekDaysHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    procedure BeforeImport; override;
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
    procedure DoImport(ANode: TdxXMLNode); override;
  public
    property WorkWeek: TdxGanttControlCalendarWorkWeek read GetWorkWeek;
  end;

  { TdxGanttControlXMLImporterCalendarWorkWeeksState }

  TdxGanttControlXMLImporterCalendarWorkWeeksState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetWorkWeeks: TdxGanttControlCalendarWorkWeeks; inline;
    function WorkWeekHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
  public
    property WorkWeeks: TdxGanttControlCalendarWorkWeeks read GetWorkWeeks;
  end;

  { TdxGanttControlXMLImporterCalendarState }

  TdxGanttControlXMLImporterCalendarState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    FIsBaseCalendar: Boolean;
    FIsBaseCalendarHasValue: Boolean;
    function GetCalendar: TdxGanttControlCalendar; inline;
    function ExceptionsHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
    function WeekDaysHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
    function WorkWeeksHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    procedure BeforeImport; override;
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
    procedure DoImport(ANode: TdxXMLNode); override;
    procedure AfterImport; override;
  public
    property Calendar: TdxGanttControlCalendar read GetCalendar;
  end;

  { TdxGanttControlXMLImporterCalendarsState }

  TdxGanttControlXMLImporterCalendarsState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetCalendars: TdxGanttControlCalendars; inline;

    function CalendarHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
  public
    property Calendars: TdxGanttControlCalendars read GetCalendars;
  end;

  { TdxGanttControlXMLImporterResourceAvailabilityPeriodState }

  TdxGanttControlXMLImporterResourceAvailabilityPeriodState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetAvailabilityPeriod
      : TdxGanttControlResourceAvailabilityPeriod; inline;
  protected
    procedure DoImport(ANode: TdxXMLNode); override;
  public
    property AvailabilityPeriod: TdxGanttControlResourceAvailabilityPeriod
      read GetAvailabilityPeriod;
  end;

  { TdxGanttControlXMLImporterResourceAvailabilityPeriodsState }

  TdxGanttControlXMLImporterResourceAvailabilityPeriodsState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetAvailabilityPeriods
      : TdxGanttControlResourceAvailabilityPeriods; inline;

    function AvailabilityPeriodHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
  public
    property AvailabilityPeriods: TdxGanttControlResourceAvailabilityPeriods
      read GetAvailabilityPeriods;
  end;

  { TdxGanttControlXMLImporterCustomBaselineState }

  TdxGanttControlXMLImporterCustomBaselineState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetBaseline: TdxGanttControlCustomBaseline; inline;
  protected
    procedure DoImport(ANode: TdxXMLNode); override;
  public
    property Baseline: TdxGanttControlCustomBaseline read GetBaseline;
  end;

  { TdxGanttControlXMLImporterResourceBaselineState }

  TdxGanttControlXMLImporterResourceBaselineState = class
    (TdxGanttControlXMLImporterCustomBaselineState);

  { TdxGanttControlXMLImporterExtendedAttributeReferenceState }

  TdxGanttControlXMLImporterExtendedAttributeReferenceState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetReference: TdxGanttControlExtendedAttributeReference; inline;
  protected
    procedure DoImport(ANode: TdxXMLNode); override;
  public
    property Reference: TdxGanttControlExtendedAttributeReference
      read GetReference;
  end;

  { TdxGanttControlXMLImporterOutlineCodeReferenceState }

  TdxGanttControlXMLImporterOutlineCodeReferenceState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetReference: TdxGanttControlOutlineCodeReference; inline;
  protected
    procedure DoImport(ANode: TdxXMLNode); override;
  public
    property Reference: TdxGanttControlOutlineCodeReference read GetReference;
  end;

  { TdxGanttControlXMLImporterResourceRateState }

  TdxGanttControlXMLImporterResourceRateState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetRate: TdxGanttControlResourceRate; inline;
  protected
    procedure DoImport(ANode: TdxXMLNode); override;
  public
    property Rate: TdxGanttControlResourceRate read GetRate;
  end;

  { TdxGanttControlXMLImporterResourceRatesState }

  TdxGanttControlXMLImporterResourceRatesState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetRates: TdxGanttControlResourceRates; inline;

    function RateHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
  public
    property Rates: TdxGanttControlResourceRates read GetRates;
  end;

  { TdxGanttControlXMLImporterTimephasedDataState }

  TdxGanttControlXMLImporterTimephasedDataState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetTimephasedData: TdxGanttControlTimephasedDataItem; inline;
  protected
    procedure DoImport(ANode: TdxXMLNode); override;
  public
    property TimephasedData: TdxGanttControlTimephasedDataItem
      read GetTimephasedData;
  end;

  { TdxGanttControlXMLImporterResourceState }

  TdxGanttControlXMLImporterResourceState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetResource: TdxGanttControlResource; inline;

    function AvailabilityPeriodsHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
    function BaselineHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
    function ExtendedAttributeHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
    function OutlineCodeHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
    function RatesHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
    function TimephasedDataHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
    procedure BeforeImport; override;
    procedure DoImport(ANode: TdxXMLNode); override;
  public
    property Resource: TdxGanttControlResource read GetResource;
  end;

  { TdxGanttControlXMLImporterResourcesState }

  TdxGanttControlXMLImporterResourcesState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetResources: TdxGanttControlResources; inline;

    function ResourceHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
  public
    property Resources: TdxGanttControlResources read GetResources;
  end;

  { TdxGanttControlXMLImporterOutlineCodeMaskState }

  TdxGanttControlXMLImporterOutlineCodeMaskState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetItem: TdxGanttControlOutlineCodeMask; inline;
  protected
    procedure DoImport(ANode: TdxXMLNode); override;
  public
    property Item: TdxGanttControlOutlineCodeMask read GetItem;
  end;

  { TdxGanttControlXMLImporterOutlineCodeMasksState }

  TdxGanttControlXMLImporterOutlineCodeMasksState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetList: TdxGanttControlOutlineCodeMasks; inline;

    function ListItemHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
  public
    property List: TdxGanttControlOutlineCodeMasks read GetList;
  end;

  { TdxGanttControlXMLImporterOutlineCodeValueState }

  TdxGanttControlXMLImporterOutlineCodeValueState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetItem: TdxGanttControlOutlineCodeValue; inline;
  protected
    procedure DoImport(ANode: TdxXMLNode); override;
  public
    property Item: TdxGanttControlOutlineCodeValue read GetItem;
  end;

  { TdxGanttControlXMLImporterOutlineCodeValuesState }

  TdxGanttControlXMLImporterOutlineCodeValuesState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetList: TdxGanttControlOutlineCodeValues; inline;

    function ListItemHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
  public
    property List: TdxGanttControlOutlineCodeValues read GetList;
  end;

  { TdxGanttControlXMLImporterOutlineCodeState }

  TdxGanttControlXMLImporterOutlineCodeState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetItem: TdxGanttControlOutlineCode; inline;

    function MasksHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
    function ValuesHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
    procedure BeforeImport; override;
    procedure DoImport(ANode: TdxXMLNode); override;
  public
    property Item: TdxGanttControlOutlineCode read GetItem;
  end;

  { TdxGanttControlXMLImporterOutlineCodesState }

  TdxGanttControlXMLImporterOutlineCodesState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetList: TdxGanttControlOutlineCodes; inline;

    function ListItemHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
  public
    property List: TdxGanttControlOutlineCodes read GetList;
  end;

  { TdxGanttControlXMLImporterExtendedAttributeValueState }

  TdxGanttControlXMLImporterExtendedAttributeValueState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetItem: TdxGanttControlExtendedAttributeValue; inline;
  protected
    procedure DoImport(ANode: TdxXMLNode); override;
  public
    property Item: TdxGanttControlExtendedAttributeValue read GetItem;
  end;

  { TdxGanttControlXMLImporterExtendedAttributeValueListState }

  TdxGanttControlXMLImporterExtendedAttributeValueListState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetList: TdxGanttControlExtendedAttributeValues; inline;

    function ListItemHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
  public
    property List: TdxGanttControlExtendedAttributeValues read GetList;
  end;

  { TdxGanttControlXMLImporterExtendedAttributeState }

  TdxGanttControlXMLImporterExtendedAttributeState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetItem: TdxGanttControlExtendedAttribute; inline;

    function ValueListHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
    procedure BeforeImport; override;
    procedure DoImport(ANode: TdxXMLNode); override;
  public
    property Item: TdxGanttControlExtendedAttribute read GetItem;
  end;

  { TdxGanttControlXMLImporterExtendedAttributesState }

  TdxGanttControlXMLImporterExtendedAttributesState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetList: TdxGanttControlExtendedAttributes; inline;

    function ListItemHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
  public
    property List: TdxGanttControlExtendedAttributes read GetList;
  end;

  { TdxGanttControlXMLImporterAssignmentBaselineState }

  TdxGanttControlXMLImporterAssignmentBaselineState = class
    (TdxGanttControlXMLImporterCustomBaselineState)
  strict private
    function GetItem: TdxGanttControlAssignmentBaseline; inline;
    function TimephasedDataHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
    procedure DoImport(ANode: TdxXMLNode); override;
  public
    property Item: TdxGanttControlAssignmentBaseline read GetItem;
  end;

  { TdxGanttControlXMLImporterEnterpriseExtendedAttributeValueState }

  TdxGanttControlXMLImporterEnterpriseExtendedAttributeValueState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetItem: TdxGanttControlEnterpriseExtendedAttributeValue; inline;
  protected
    procedure DoImport(ANode: TdxXMLNode); override;
  public
    property Item: TdxGanttControlEnterpriseExtendedAttributeValue read GetItem;
  end;

  { TdxGanttControlXMLImporterEnterpriseExtendedAttributeState }

  TdxGanttControlXMLImporterEnterpriseExtendedAttributeState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetItem: TdxGanttControlEnterpriseExtendedAttribute; inline;
    function ValueHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
    procedure DoImport(ANode: TdxXMLNode); override;
  public
    property Item: TdxGanttControlEnterpriseExtendedAttribute read GetItem;
  end;

  { TdxGanttControlXMLImporterAssignmentState }

  TdxGanttControlXMLImporterAssignmentState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetItem: TdxGanttControlAssignment; inline;

    function BaselineHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
    function EnterpriseExtendedAttributeHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
    function ExtendedAttributeHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
    function TimephasedDataHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
    procedure BeforeImport; override;
    procedure DoImport(ANode: TdxXMLNode); override;
  public
    property Item: TdxGanttControlAssignment read GetItem;
  end;

  { TdxGanttControlXMLImporterAssignmentsState }

  TdxGanttControlXMLImporterAssignmentsState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetList: TdxGanttControlAssignments; inline;

    function ListItemHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
  public
    property List: TdxGanttControlAssignments read GetList;
  end;

  { TdxGanttControlXMLImporterTaskPredecessorLinkState }

  TdxGanttControlXMLImporterTaskPredecessorLinkState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetItem: TdxGanttControlTaskPredecessorLink; inline;
  protected
    procedure DoImport(ANode: TdxXMLNode); override;
  public
    property Item: TdxGanttControlTaskPredecessorLink read GetItem;
  end;

  { TdxGanttControlXMLImporterTaskBaselineState }

  TdxGanttControlXMLImporterTaskBaselineState = class
    (TdxGanttControlXMLImporterCustomBaselineState)
  strict private
    function GetBaseline: TdxGanttControlTaskBaseline; inline;
    function TimephasedDataHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
    procedure DoImport(ANode: TdxXMLNode); override;
  public
    property Baseline: TdxGanttControlTaskBaseline read GetBaseline;
  end;

  { TdxGanttControlXMLImporterTaskState }

  TdxGanttControlXMLImporterTaskState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetItem: TdxGanttControlTask; inline;

    function BaselineHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
    function ExtendedAttributeHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
    function OutlineCodeHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
    function TimephasedDataHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
    function PredecessorLinkHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
    function EnterpriseExtendedAttributeHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
    procedure BeforeImport; override;
    procedure DoImport(ANode: TdxXMLNode); override;
  public
    property Item: TdxGanttControlTask read GetItem;
  end;

  { TdxGanttControlXMLImporterTasksState }

  TdxGanttControlXMLImporterTasksState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetList: TdxGanttControlTasks; inline;

    function ListItemHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
  public
    property List: TdxGanttControlTasks read GetList;
  end;

  { TdxGanttControlXMLImporterWBSMaskState }

  TdxGanttControlXMLImporterWBSMaskState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetItem: TdxGanttControlWBSMask; inline;
  protected
    procedure DoImport(ANode: TdxXMLNode); override;
  public
    property Item: TdxGanttControlWBSMask read GetItem;
  end;

  { TdxGanttControlXMLImporterWBSMasksMasksState }

  TdxGanttControlXMLImporterWBSMasksMasksState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetList: TdxGanttControlWBSMasks; inline;

    function ListItemHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
  public
    property List: TdxGanttControlWBSMasks read GetList;
  end;

  { TdxGanttControlXMLImporterWBSMasksState }

  TdxGanttControlXMLImporterWBSMasksState = class
    (TdxGanttControlXMLImporterCustomElementState)
  strict private
    function GetList: TdxGanttControlWBSMasks; inline;

    function ListItemHandler(ANode: TdxXMLNode)
      : TdxGanttControlXMLImporterCustomState;
  protected
    function CreateStateHandlers: TdxGanttControlXMLImporterCustomState.
      TStateHandlers; override;
    procedure DoImport(ANode: TdxXMLNode); override;
  public
    property List: TdxGanttControlWBSMasks read GetList;
  end;

  { TdxXMLNodeHelper }

function TdxXMLNodeHelper.GetTextAsDuration: TDuration;
begin
  Result := TextAsString;
end;

function TdxXMLNodeHelper.GetTextAsInteger: Integer;
begin
  if not TryStrToInt(TextAsString, Result) then
    Result := 0;
end;

function TdxXMLNodeHelper.GetTextAsNumber: Double;
begin
  try
    Result := StrToFloat(TextAsString,
      TdxCultureInfo.InvariantCulture.FormatSettings);
  except
    Result := 0.0;
  end;
end;

function TdxXMLNodeHelper.GetTextAsTime: TTime;
begin
  Result := StrToTime(TextAsString);
end;

{ TdxGanttControlXMLImporter }

function TdxGanttControlXMLImporter.DoLoadFromStream(AStream: TStream): Boolean;
var
  ADoc: TdxXMLDocument;
  AProject: TdxXMLNode;
  AProjectState: TdxGanttControlXMLImporterProjectState;
begin
  ADoc := TdxXMLDocument.Create;
  try
    ADoc.LoadFromStream(AStream);
    AProject := ADoc.Root.FindChild('Project');
    if AProject = nil then
      ThrowInvalidFormatException;
    AProjectState := TdxGanttControlXMLImporterProjectState.Create(AProject,
      DataModel);
    try
      AProjectState.Import;
    finally
      AProjectState.Free;
    end;
    Result := True;
  finally
    ADoc.Free;
  end;
end;

class function TdxGanttControlXMLImporter.GetDisplayName: string;
begin

end;

class function TdxGanttControlXMLImporter.GetExtensions: TArray<string>;
begin
  Result := TArray<string>.Create('.xml');
end;

{ TdxGanttControlXMLImporterCustomState }

procedure TdxGanttControlXMLImporterCustomState.AfterImport;
begin
  // do nothing
end;

procedure TdxGanttControlXMLImporterCustomState.BeforeImport;
begin
  // do nothing
end;

constructor TdxGanttControlXMLImporterCustomState.Create(ARootNode: TdxXMLNode);
begin
  inherited Create;
  FRootNode := ARootNode;
  FStateHandlers := CreateStateHandlers;
end;

destructor TdxGanttControlXMLImporterCustomState.Destroy;
begin
  FreeAndNil(FStateHandlers);
  inherited Destroy;
end;

procedure TdxGanttControlXMLImporterCustomState.DoImport(ANode: TdxXMLNode);
begin
end;

function TdxGanttControlXMLImporterCustomState.CreateStateHandlers
  : TStateHandlers;
begin
  Result := TStateHandlers.Create;
end;

procedure TdxGanttControlXMLImporterCustomState.Import;
begin
  BeforeImport;
  FRootNode.ForEach(procedure(ANode: TdxXMLNode; AUserData: Pointer)
    var
      AHandler: TGetStateHandler;
      AState: TdxGanttControlXMLImporterCustomState;
    begin
      if StateHandlers.TryGetValue(LowerCase(ANode.NameAsString), AHandler) then
      begin
        AState := AHandler(ANode);
        try
          AState.Import;
        finally
          AState.Free;
        end;
      end
      else
        DoImport(ANode);
    end);
  AfterImport;
end;

procedure TdxGanttControlXMLImporterCustomState.ThrowInvalidFormatException;
begin
  TdxGanttControlXMLImporter.ThrowInvalidFormatException;
end;

{ TdxGanttControlXMLImporterCustomElementState }

constructor TdxGanttControlXMLImporterCustomElementState.Create
  (ARootNode: TdxXMLNode; AElement: TdxGanttControlModelElement);
begin
  inherited Create(ARootNode);
  FElement := AElement;
end;

{ TdxGanttControlXMLImporterProjectState }

constructor TdxGanttControlXMLImporterProjectState.Create(ARootNode: TdxXMLNode;
ADataModel: TdxGanttControlDataModel);
begin
  inherited Create(ARootNode);
  FDataModel := ADataModel;
end;

function TdxGanttControlXMLImporterProjectState.AssignmentsHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterAssignmentsState.Create(ANode,
    DataModel.Assignments);
end;

procedure TdxGanttControlXMLImporterProjectState.BeforeImport;
begin
  inherited BeforeImport;
  FDataModel.Reset;
  FDataModel.OutlineCodes.Clear;
  FDataModel.ExtendedAttributes.Clear;
  FDataModel.Assignments.Clear;
  TdxGanttControlElementCustomListAccess(FDataModel.Calendars).InternalClear;
  TdxGanttControlElementCustomListAccess(FDataModel.Resources).InternalClear;
  TdxGanttControlElementCustomListAccess(FDataModel.Tasks).InternalClear;
end;

function TdxGanttControlXMLImporterProjectState.CalendarsHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterCalendarsState.Create(ANode,
    DataModel.Calendars);
end;

procedure TdxGanttControlXMLImporterProjectState.CheckVersion
  (const Value: Integer);
begin
  // do nothing
end;

function TdxGanttControlXMLImporterProjectState.ResourcesHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterResourcesState.Create(ANode,
    DataModel.Resources);
end;

function TdxGanttControlXMLImporterProjectState.TasksHandler(ANode: TdxXMLNode)
  : TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterTasksState.Create(ANode, DataModel.Tasks);
end;

function TdxGanttControlXMLImporterProjectState.WBSMasksHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterWBSMasksState.Create(ANode,
    DataModel.WBSMasks);
end;

function TdxGanttControlXMLImporterProjectState.CreateStateHandlers
  : TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('calendars', CalendarsHandler);
  Result.Add('resources', ResourcesHandler);
  Result.Add('outlinecodes', OutlineCodesHandler);
  Result.Add('extendedattributes', ExtendedAttributesHandler);
  Result.Add('assignments', AssignmentsHandler);
  Result.Add('tasks', TasksHandler);
  Result.Add('wbsmasks', WBSMasksHandler);
end;

procedure TdxGanttControlXMLImporterProjectState.DoImport(ANode: TdxXMLNode);
var
  AName: string;
begin
  AName := LowerCase(ANode.NameAsString);
  if AName = 'actualsinsync' then
    DataModel.Properties.ActualsInSync := ANode.TextAsBoolean
  else if AName = 'adminproject' then
    DataModel.Properties.AdminProject := ANode.TextAsBoolean
  else if AName = 'author' then
    DataModel.Properties.Author := ANode.TextAsString
  else if AName = 'autoaddnewresourcesandtasks' then
    DataModel.Properties.AutoAddNewResourcesAndTasks := ANode.TextAsBoolean
  else if AName = 'autolink' then
    DataModel.Properties.Autolink := ANode.TextAsBoolean
  else if AName = 'baselineforearnedvalue' then
    DataModel.Properties.BaselineForEarnedValue := ANode.TextAsInteger
  else if AName = 'calendaruid' then
    DataModel.Properties.CalendarUID := ANode.TextAsInteger
  else if AName = 'category' then
    DataModel.Properties.Category := ANode.TextAsString
  else if AName = 'company' then
    DataModel.Properties.Company := ANode.TextAsString
  else if AName = 'creationdate' then
    DataModel.Properties.ProjectCreated := ANode.TextAsDateTime
  else if AName = 'criticalslacklimit' then
    DataModel.Properties.CriticalSlackLimit := ANode.TextAsInteger
  else if AName = 'currencycode' then
    DataModel.Properties.CurrencyCode := ANode.TextAsString
  else if AName = 'currencydigits' then
    DataModel.Properties.CurrencyDigits := TdxGanttControlCurrencyDigits
      (ANode.TextAsInteger)
  else if AName = 'currencysymbol' then
    DataModel.Properties.CurrencySymbol := ANode.TextAsString
  else if AName = 'currencysymbolposition' then
    DataModel.Properties.CurrencySymbolPosition :=
      TdxGanttControlCurrencySymbolPosition(ANode.TextAsInteger)
  else if AName = 'dayspermonth' then
    DataModel.Properties.DaysPerMonth := ANode.TextAsInteger
  else if AName = 'defaultfinishtime' then
    DataModel.Properties.DefaultFinishTime := ANode.TextAsTime
  else if AName = 'defaultfixedcostaccrual' then
    DataModel.Properties.DefaultFixedCostAccrual :=
      TdxGanttControlFixedCostAccrual(ANode.TextAsInteger)
  else if AName = 'defaultovertimerate' then
    DataModel.Properties.DefaultOvertimeRate := ANode.TextAsNumber
  else if AName = 'defaultstandardrate' then
    DataModel.Properties.DefaultStandardRate := ANode.TextAsNumber
  else if AName = 'defaultstarttime' then
    DataModel.Properties.DefaultStartTime := ANode.TextAsTime
  else if AName = 'defaulttaskevmethod' then
    DataModel.Properties.DefaultTaskEVMethod :=
      TdxGanttControlTaskEarnedValueMethod(ANode.TextAsInteger)
  else if AName = 'defaulttasktype' then
    DataModel.Properties.DefaultTaskType :=
      TdxGanttControlTaskType(ANode.TextAsInteger)
  else if AName = 'durationformat' then
    DataModel.Properties.DurationFormat :=
      TdxDurationFormat(ANode.TextAsInteger)
  else if AName = 'earnedvaluemethod' then
    DataModel.Properties.EarnedValueMethod :=
      TdxGanttControlTaskEarnedValueMethod(ANode.TextAsInteger)
  else if AName = 'editableactualcosts' then
    DataModel.Properties.EditableActualCosts := ANode.TextAsBoolean
  else if AName = 'extendedcreationdate' then
    DataModel.Properties.ExtendedCreationDate := ANode.TextAsDateTime
  else if AName = 'finishdate' then
    DataModel.Properties.ProjectFinish := ANode.TextAsDateTime
  else if AName = 'fiscalyearstart' then
    DataModel.Properties.FiscalYearStart := ANode.TextAsBoolean
  else if AName = 'fystartdate' then
    DataModel.Properties.FYStartDate := TdxGanttControlCalendarMonth
      (ANode.TextAsInteger - 1)
  else if AName = 'guid' then
    DataModel.Properties.GUID := ANode.TextAsString
  else if AName = 'honorconstraints' then
    DataModel.Properties.HonorConstraints := ANode.TextAsBoolean
  else if AName = 'insertedprojectslikesummary' then
    DataModel.Properties.InsertedProjectsLikeSummary := ANode.TextAsBoolean
  else if AName = 'lastsaved' then
    DataModel.Properties.LastSaved := ANode.TextAsDateTime
  else if AName = 'manager' then
    DataModel.Properties.Manager := ANode.TextAsString
  else if AName = 'microsoftprojectserverurl' then
    DataModel.Properties.MicrosoftProjectServerURL := ANode.TextAsBoolean
  else if AName = 'minutesperday' then
    DataModel.Properties.MinutesPerDay := ANode.TextAsInteger
  else if AName = 'minutesperweek' then
    DataModel.Properties.MinutesPerWeek := ANode.TextAsInteger
  else if AName = 'movecompletedendsback' then
    DataModel.Properties.MoveCompletedEndsBack := ANode.TextAsBoolean
  else if AName = 'movecompletedendsforward' then
    DataModel.Properties.MoveCompletedEndsForward := ANode.TextAsBoolean
  else if AName = 'moveremainingstartsback' then
    DataModel.Properties.MoveRemainingStartsBack := ANode.TextAsBoolean
  else if AName = 'moveremainingstartsforward' then
    DataModel.Properties.MoveRemainingStartsForward := ANode.TextAsBoolean
  else if AName = 'multiplecriticalpaths' then
    DataModel.Properties.MultipleCriticalPaths := ANode.TextAsBoolean
  else if AName = 'name' then
    DataModel.Properties.Name := ANode.TextAsString
  else if AName = 'newtaskseffortdriven' then
    DataModel.Properties.NewTasksEffortDriven := ANode.TextAsBoolean
  else if AName = 'newtasksestimated' then
    DataModel.Properties.NewTasksEstimated := ANode.TextAsBoolean
  else if AName = 'newtaskstartdate' then
    DataModel.Properties.NewTaskStartDate := TdxGanttControlNewTaskStartDate
      (ANode.TextAsInteger)
  else if AName = 'removefileproperties' then
    DataModel.Properties.RemoveFileProperties := ANode.TextAsBoolean
  else if AName = 'revision' then
    DataModel.Properties.Revision := ANode.TextAsInteger
  else if AName = 'schedulefromstart' then
    DataModel.Properties.ScheduleFromStart := ANode.TextAsBoolean
  else if AName = 'splitsinprogresstasks' then
    DataModel.Properties.SplitsInProgressTasks := ANode.TextAsBoolean
  else if AName = 'spreadactualcost' then
    DataModel.Properties.SpreadActualCost := ANode.TextAsBoolean
  else if AName = 'spreadpercentcomplete' then
    DataModel.Properties.SpreadPercentComplete := ANode.TextAsBoolean
  else if AName = 'startdate' then
    DataModel.Properties.ProjectStart := ANode.TextAsDateTime
  else if AName = 'statusdate' then
    DataModel.Properties.StatusDate := ANode.TextAsDateTime
  else if AName = 'subject' then
    DataModel.Properties.Subject := ANode.TextAsString
  else if AName = 'taskupdatesresource' then
    DataModel.Properties.TaskUpdatesResource := ANode.TextAsBoolean
  else if AName = 'weekstartday' then
    DataModel.Properties.WeekStartDay :=
      TdxGanttControlWeekDay(ANode.TextAsInteger)
  else if AName = 'workformat' then
    DataModel.Properties.WorkFormat := TdxGanttControlWorkFormat
      (ANode.TextAsInteger)
  else if AName = 'title' then
    DataModel.Properties.Title := ANode.TextAsString
  else if AName = 'newtasksaremanual' then
    DataModel.Properties.MarkNewTasksAsManuallyScheduled := ANode.TextAsBoolean
  else if AName = 'saveversion' then
    CheckVersion(ANode.TextAsInteger)
  else if AName = 'updatemanuallyscheduledtaskswheneditinglinks' then
    DataModel.Properties.UpdateManuallyScheduledTasksWhenEditingLinks :=
      ANode.TextAsBoolean
  else if AName = 'keeptaskonnearestworkingtimewhenmadeautoscheduled' then
    DataModel.Properties.KeepTaskOnNearestWorkingTimeWhenMadeAutoScheduled :=
      ANode.TextAsBoolean
  else if AName = 'buildnumber' then
  else if AName = 'currentdate' then
  else if AName = 'projectexternallyedited' then
  else if AName = 'views' then
  else if AName = 'filters' then
  else if AName = 'groups' then
  else if AName = 'tables' then
  else if AName = 'maps' then
  else if AName = 'reports' then
  else if AName = 'drawings' then
  else if AName = 'datalinks' then
  else if AName = 'vbaprojects' then
  else if AName = 'baselinecalendar' then
  else
    inherited DoImport(ANode);
end;

function TdxGanttControlXMLImporterProjectState.ExtendedAttributesHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterExtendedAttributesState.Create(ANode,
    DataModel.ExtendedAttributes);
end;

function TdxGanttControlXMLImporterProjectState.OutlineCodesHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterOutlineCodesState.Create(ANode,
    DataModel.OutlineCodes);
end;

{ TdxGanttControlXMLImporterWorkTimeState }

procedure TdxGanttControlXMLImporterWorkTimeState.DoImport(ANode: TdxXMLNode);
var
  AName: string;
begin
  AName := LowerCase(ANode.NameAsString);
  if AName = 'fromtime' then
    WorkTime.FromTime := ANode.TextAsTime
  else if AName = 'totime' then
  begin
    WorkTime.ToTime := ANode.TextAsTime;
    if WorkTime.ToTime = 0 then
      WorkTime.ToTime := dxEndOfDay;
  end
  else
    inherited DoImport(ANode);
end;

function TdxGanttControlXMLImporterWorkTimeState.GetWorkTime
  : TdxGanttControlCalendarWeekDayWorkTime;
begin
  Result := TdxGanttControlCalendarWeekDayWorkTime(Element);
end;

{ TdxGanttControlXMLImporterTimePeriodState }

procedure TdxGanttControlXMLImporterTimePeriodState.DoImport(ANode: TdxXMLNode);
var
  AName: string;
begin
  AName := LowerCase(ANode.NameAsString);
  if AName = 'fromdate' then
    TimePeriod.FromDate := ANode.TextAsDateTime
  else if AName = 'todate' then
    TimePeriod.ToDate := ANode.TextAsDateTime
  else
    inherited DoImport(ANode);
end;

function TdxGanttControlXMLImporterTimePeriodState.GetTimePeriod
  : TdxGanttControlTimePeriod;
begin
  Result := TdxGanttControlTimePeriod(Element);
end;

{ TdxGanttControlXMLImporterWorkTimesState }

function TdxGanttControlXMLImporterWorkTimesState.CreateStateHandlers
  : TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('workingtime', WorkTimeHandler);
end;

function TdxGanttControlXMLImporterWorkTimesState.GetWorkTimes
  : TdxGanttControlCalendarWeekDayWorkTimes;
begin
  Result := TdxGanttControlCalendarWeekDayWorkTimes(Element);
end;

function TdxGanttControlXMLImporterWorkTimesState.WorkTimeHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterWorkTimeState.Create(ANode,
    WorkTimes.Append);
end;

{ TdxGanttControlXMLImporterCalendarExceptionState }

procedure TdxGanttControlXMLImporterCalendarExceptionState.BeforeImport;
begin
  Exception.DestroyTimePeriod;
  Exception.WorkTimes.Clear;
end;

function TdxGanttControlXMLImporterCalendarExceptionState.CreateStateHandlers
  : TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('timeperiod', TimePeriodHandler);
  Result.Add('workingtimes', WorkTimesHandler);
end;

procedure TdxGanttControlXMLImporterCalendarExceptionState.DoImport
  (ANode: TdxXMLNode);
var
  AName: string;
begin
  AName := LowerCase(ANode.NameAsString);
  if AName = 'enteredbyoccurrences' then
    Exception.EnteredByOccurrences := ANode.TextAsBoolean
  else if AName = 'occurrences' then
    Exception.Occurrences := ANode.TextAsInteger
  else if AName = 'name' then
    Exception.Name := ANode.TextAsString
  else if AName = 'type' then
    Exception.&Type := TdxGanttControlExceptionType(ANode.TextAsInteger)
  else if AName = 'month' then
    Exception.Month := TdxGanttControlCalendarMonth(ANode.TextAsInteger)
  else if AName = 'monthday' then
    Exception.MonthDay := ANode.TextAsInteger
  else if AName = 'dayworking' then
    Exception.Workday := ANode.TextAsBoolean
  else if AName = 'period' then
    Exception.Period := ANode.TextAsInteger
  else if AName = 'daysofweek' then
    Exception.DaysOfWeek := dxIntegerToGanttControlExceptionDaysOfWeek
      (ANode.TextAsInteger)
  else if AName = 'monthitem' then
    Exception.MonthItem := TdxGanttControlExceptionMonthItem
      (ANode.TextAsInteger)
  else if AName = 'monthposition' then
    Exception.MonthPosition := TdxGanttControlExceptionMonthPosition
      (ANode.TextAsInteger)
  else
    DoImport(ANode);
end;

function TdxGanttControlXMLImporterCalendarExceptionState.GetException
  : TdxGanttControlCalendarException;
begin
  Result := TdxGanttControlCalendarException(Element);
end;

function TdxGanttControlXMLImporterCalendarExceptionState.TimePeriodHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Exception.CreateTimePeriod;
  Result := TdxGanttControlXMLImporterTimePeriodState.Create(ANode,
    Exception.TimePeriod);
end;

function TdxGanttControlXMLImporterCalendarExceptionState.WorkTimesHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterWorkTimesState.Create(ANode,
    Exception.WorkTimes);
end;

{ TdxGanttControlXMLImporterCalendarExceptionsState }

function TdxGanttControlXMLImporterCalendarExceptionsState.CreateStateHandlers
  : TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('exception', ExceptionHandler);
end;

function TdxGanttControlXMLImporterCalendarExceptionsState.ExceptionHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterCalendarExceptionState.Create(ANode,
    Exceptions.Append);
end;

function TdxGanttControlXMLImporterCalendarExceptionsState.GetExceptions
  : TdxGanttControlCalendarExceptions;
begin
  Result := TdxGanttControlCalendarExceptions(inherited Element);
end;

{ TdxGanttControlXMLImporterCalendarWeekDayState }

procedure TdxGanttControlXMLImporterCalendarWeekDayState.AfterImport;
begin
  inherited AfterImport;
  if (WeekDay.WorkTimes.Count > 0) and not WeekDay.Workday then
    ThrowInvalidFormatException;
  if (WeekDay.WorkTimes.Count = 0) and WeekDay.Workday then
    ThrowInvalidFormatException;
end;

procedure TdxGanttControlXMLImporterCalendarWeekDayState.BeforeImport;
begin
  inherited BeforeImport;
  WeekDay.DestroyTimePeriod;
  WeekDay.WorkTimes.Clear;
end;

function TdxGanttControlXMLImporterCalendarWeekDayState.CreateStateHandlers
  : TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('timeperiod', TimePeriodHandler);
  Result.Add('workingtimes', WorkTimesHandler);
end;

procedure TdxGanttControlXMLImporterCalendarWeekDayState.DoImport
  (ANode: TdxXMLNode);
var
  AName: string;
begin
  AName := LowerCase(ANode.NameAsString);
  if AName = 'daytype' then
    WeekDay.DayType := TdxGanttControlWeekDayType(ANode.TextAsInteger)
  else if AName = 'dayworking' then
    WeekDay.Workday := ANode.TextAsBoolean
  else
    inherited DoImport(ANode);
end;

function TdxGanttControlXMLImporterCalendarWeekDayState.GetWeekDay
  : TdxGanttControlCalendarWeekDay;
begin
  Result := TdxGanttControlCalendarWeekDay(Element);
end;

function TdxGanttControlXMLImporterCalendarWeekDayState.TimePeriodHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  WeekDay.CreateTimePeriod;
  Result := TdxGanttControlXMLImporterTimePeriodState.Create(ANode,
    WeekDay.TimePeriod);
end;

function TdxGanttControlXMLImporterCalendarWeekDayState.WorkTimesHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterWorkTimesState.Create(ANode,
    WeekDay.WorkTimes);
end;

{ TdxGanttControlXMLImporterCalendarWeekDaysState }

function TdxGanttControlXMLImporterCalendarWeekDaysState.CreateStateHandlers
  : TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('weekday', WeekDaysHandler);
end;

function TdxGanttControlXMLImporterCalendarWeekDaysState.GetWeekDays
  : TdxGanttControlCalendarWeekDays;
begin
  Result := TdxGanttControlCalendarWeekDays(Element);
end;

function TdxGanttControlXMLImporterCalendarWeekDaysState.WeekDaysHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterCalendarWeekDayState.Create(ANode,
    WeekDays.Append);
end;

{ TdxGanttControlXMLImporterCalendarWorkWeekState }

procedure TdxGanttControlXMLImporterCalendarWorkWeekState.BeforeImport;
begin
  inherited BeforeImport;
  WorkWeek.WeekDays.Clear;
  WorkWeek.DestroyTimePeriod;
end;

function TdxGanttControlXMLImporterCalendarWorkWeekState.CreateStateHandlers
  : TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('timeperiod', TimePeriodHandler);
  Result.Add('weekdays', WeekDaysHandler);
end;

procedure TdxGanttControlXMLImporterCalendarWorkWeekState.DoImport
  (ANode: TdxXMLNode);
var
  AName: string;
begin
  AName := LowerCase(ANode.NameAsString);
  if AName = 'name' then
    WorkWeek.Name := ANode.TextAsString
  else
    inherited DoImport(ANode);
end;

function TdxGanttControlXMLImporterCalendarWorkWeekState.GetWorkWeek
  : TdxGanttControlCalendarWorkWeek;
begin
  Result := TdxGanttControlCalendarWorkWeek(Element);
end;

function TdxGanttControlXMLImporterCalendarWorkWeekState.TimePeriodHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  WorkWeek.CreateTimePeriod;
  Result := TdxGanttControlXMLImporterTimePeriodState.Create(ANode,
    WorkWeek.TimePeriod);
end;

function TdxGanttControlXMLImporterCalendarWorkWeekState.WeekDaysHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterCalendarWeekDaysState.Create(ANode,
    WorkWeek.WeekDays);
end;

{ TdxGanttControlXMLImporterCalendarWorkWeeksState }

function TdxGanttControlXMLImporterCalendarWorkWeeksState.CreateStateHandlers
  : TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('workweek', WorkWeekHandler);
end;

function TdxGanttControlXMLImporterCalendarWorkWeeksState.GetWorkWeeks
  : TdxGanttControlCalendarWorkWeeks;
begin
  Result := TdxGanttControlCalendarWorkWeeks(Element);
end;

function TdxGanttControlXMLImporterCalendarWorkWeeksState.WorkWeekHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterCalendarWorkWeekState.Create(ANode,
    WorkWeeks.Append);
end;

{ TdxGanttControlXMLImporterCalendarState }

procedure TdxGanttControlXMLImporterCalendarState.AfterImport;
begin
  inherited AfterImport;
  if FIsBaseCalendarHasValue then
  begin
    if FIsBaseCalendar <> Calendar.IsBaseCalendar then
      ThrowInvalidFormatException;
  end
  else if Calendar.IsBaseCalendar then
    ThrowInvalidFormatException;
end;

procedure TdxGanttControlXMLImporterCalendarState.BeforeImport;
begin
  inherited BeforeImport;
  if RootNode.FindChild('UID') = nil then
    ThrowInvalidFormatException;
  TdxGanttControlElementCustomListAccess(Calendar.Exceptions).InternalClear;
  TdxGanttControlElementCustomListAccess(Calendar.WeekDays).InternalClear;
  TdxGanttControlElementCustomListAccess(Calendar.WorkWeeks).InternalClear;
  FIsBaseCalendarHasValue := False;
end;

function TdxGanttControlXMLImporterCalendarState.CreateStateHandlers
  : TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('weekdays', WeekDaysHandler);
  Result.Add('exceptions', ExceptionsHandler);
  Result.Add('workweeks', WorkWeeksHandler);
end;

procedure TdxGanttControlXMLImporterCalendarState.DoImport(ANode: TdxXMLNode);
var
  AName: string;
begin
  AName := LowerCase(ANode.NameAsString);
  if AName = 'uid' then
    TdxGanttControlCalendarAccess(Calendar).SetUID(ANode.TextAsInteger)
  else if AName = 'name' then
    Calendar.Name := ANode.TextAsString
  else if AName = 'guid' then
    TdxGanttControlCalendarAccess(Calendar).GUID := ANode.TextAsString
  else if AName = 'isbaselinecalendar' then
    Calendar.BaselineCalendar := ANode.TextAsBoolean
  else if AName = 'basecalendaruid' then
    Calendar.BaseCalendarUID := ANode.TextAsInteger
  else if AName = 'isbasecalendar' then
  begin
    FIsBaseCalendar := ANode.TextAsBoolean;
    FIsBaseCalendarHasValue := True;
  end
  else
    inherited DoImport(ANode);
end;

function TdxGanttControlXMLImporterCalendarState.GetCalendar
  : TdxGanttControlCalendar;
begin
  Result := TdxGanttControlCalendar(inherited Element);
end;

function TdxGanttControlXMLImporterCalendarState.ExceptionsHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterCalendarExceptionsState.Create(ANode,
    Calendar.Exceptions);
end;

function TdxGanttControlXMLImporterCalendarState.WeekDaysHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterCalendarWeekDaysState.Create(ANode,
    Calendar.WeekDays);
end;

function TdxGanttControlXMLImporterCalendarState.WorkWeeksHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterCalendarWorkWeeksState.Create(ANode,
    Calendar.WorkWeeks);
end;

{ TdxGanttControlXMLImporterCalendarsState }

function TdxGanttControlXMLImporterCalendarsState.CalendarHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterCalendarState.Create(ANode,
    Calendars.Append);
end;

function TdxGanttControlXMLImporterCalendarsState.CreateStateHandlers
  : TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('calendar', CalendarHandler);
end;

function TdxGanttControlXMLImporterCalendarsState.GetCalendars
  : TdxGanttControlCalendars;
begin
  Result := TdxGanttControlCalendars(inherited Element);
end;

{ TdxGanttControlXMLImporterResourceAvailabilityPeriodsState }

function TdxGanttControlXMLImporterResourceAvailabilityPeriodsState.
  AvailabilityPeriodHandler(ANode: TdxXMLNode)
  : TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterResourceAvailabilityPeriodState.Create
    (ANode, AvailabilityPeriods.Append);
end;

function TdxGanttControlXMLImporterResourceAvailabilityPeriodsState.
  CreateStateHandlers: TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('availabilityperiod', AvailabilityPeriodHandler);
end;

function TdxGanttControlXMLImporterResourceAvailabilityPeriodsState.
  GetAvailabilityPeriods: TdxGanttControlResourceAvailabilityPeriods;
begin
  Result := TdxGanttControlResourceAvailabilityPeriods(Element);
end;

{ TdxGanttControlXMLImporterCustomBaselineState }

procedure TdxGanttControlXMLImporterCustomBaselineState.DoImport
  (ANode: TdxXMLNode);
var
  AName: string;
begin
  AName := LowerCase(ANode.NameAsString);
  if AName = 'number' then
    TdxGanttControlCustomBaselineAccess(Baseline).SetNumber(ANode.TextAsInteger)
  else if AName = 'work' then
    Baseline.Work := ANode.TextAsDuration
  else if AName = 'cost' then
    Baseline.Cost := ANode.TextAsNumber
  else if AName = 'bcws' then
    Baseline.BCWS := ANode.TextAsNumber
  else if AName = 'bcwp' then
    Baseline.BCWP := ANode.TextAsNumber
  else
    inherited DoImport(ANode);
end;

function TdxGanttControlXMLImporterCustomBaselineState.GetBaseline
  : TdxGanttControlCustomBaseline;
begin
  Result := TdxGanttControlCustomBaseline(Element);
end;

{ TdxGanttControlXMLImporterExtendedAttributeReferenceState }

procedure TdxGanttControlXMLImporterExtendedAttributeReferenceState.DoImport
  (ANode: TdxXMLNode);
var
  AName: string;
begin
  AName := LowerCase(ANode.NameAsString);
  if AName = 'fieldid' then
    Reference.FieldID := ANode.TextAsInteger
  else if AName = 'value' then
    Reference.Value := ANode.TextAsString
  else if AName = 'durationformat' then
    Reference.DurationFormat := TdxDurationFormat(ANode.TextAsInteger)
  else if AName = 'valueguid' then
    Reference.ValueGUID := ANode.TextAsString
  else
    inherited DoImport(ANode);
end;

function TdxGanttControlXMLImporterExtendedAttributeReferenceState.GetReference
  : TdxGanttControlExtendedAttributeReference;
begin
  Result := TdxGanttControlExtendedAttributeReference(Element);
end;

{ TdxGanttControlXMLImporterOutlineCodeReferenceState }

procedure TdxGanttControlXMLImporterOutlineCodeReferenceState.DoImport
  (ANode: TdxXMLNode);
var
  AName: string;
begin
  AName := LowerCase(ANode.NameAsString);
  if AName = 'fieldid' then
    Reference.FieldID := ANode.TextAsInteger
  else if AName = 'valueid' then
    Reference.ValueID := ANode.TextAsInteger
  else if AName = 'valueguid' then
    Reference.ValueGUID := ANode.TextAsString
  else
    inherited DoImport(ANode);
end;

function TdxGanttControlXMLImporterOutlineCodeReferenceState.GetReference
  : TdxGanttControlOutlineCodeReference;
begin
  Result := TdxGanttControlOutlineCodeReference(Element);
end;

{ TdxGanttControlXMLImporterResourceRateState }

procedure TdxGanttControlXMLImporterResourceRateState.DoImport
  (ANode: TdxXMLNode);
var
  AName: string;
begin
  AName := LowerCase(ANode.NameAsString);
  if AName = 'ratesfrom' then
    Rate.RatesFrom := ANode.TextAsDateTime
  else if AName = 'ratesto' then
    Rate.RatesTo := ANode.TextAsDateTime
  else if AName = 'ratetable' then
    Rate.RateTable := TdxGanttControlCostRateTable(ANode.TextAsInteger)
  else if AName = 'standardrate' then
    Rate.StandardRate := ANode.TextAsNumber
  else if AName = 'standardrateformat' then
    Rate.StandardRateFormat := TdxGanttControlRateFormat(ANode.TextAsInteger)
  else if AName = 'overtimerate' then
    Rate.OvertimeRate := ANode.TextAsNumber
  else if AName = 'overtimerateformat' then
    Rate.OvertimeRateFormat := TdxGanttControlRateFormat(ANode.TextAsInteger)
  else if AName = 'costperuse' then
    Rate.CostPerUse := ANode.TextAsNumber
  else
    inherited DoImport(ANode);
end;

function TdxGanttControlXMLImporterResourceRateState.GetRate
  : TdxGanttControlResourceRate;
begin
  Result := TdxGanttControlResourceRate(Element);
end;

{ TdxGanttControlXMLImporterResourceRatesState }

function TdxGanttControlXMLImporterResourceRatesState.RateHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterResourceRateState.Create(ANode,
    Rates.Append);
end;

function TdxGanttControlXMLImporterResourceRatesState.CreateStateHandlers
  : TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('rate', RateHandler);
end;

function TdxGanttControlXMLImporterResourceRatesState.GetRates
  : TdxGanttControlResourceRates;
begin
  Result := TdxGanttControlResourceRates(Element);
end;

{ TdxGanttControlXMLImporterTimephasedDataState }

procedure TdxGanttControlXMLImporterTimephasedDataState.DoImport
  (ANode: TdxXMLNode);
var
  AName: string;
begin
  AName := LowerCase(ANode.NameAsString);
  if AName = 'uid' then
  begin
    if TimephasedData.OwnerUID <> ANode.TextAsInteger then
      ThrowInvalidFormatException;
  end
  else if AName = 'type' then
    TimephasedData.&Type := TdxTimephasedDataType(ANode.TextAsInteger)
  else if AName = 'start' then
    TimephasedData.Start := ANode.TextAsDateTime
  else if AName = 'finish' then
    TimephasedData.Finish := ANode.TextAsDateTime
  else if AName = 'unit' then
    TimephasedData.&Unit := TdxTimephasedDataUnit(ANode.TextAsInteger)
  else if AName = 'value' then
    TimephasedData.Value := ANode.TextAsString
  else
    inherited DoImport(ANode);
end;

function TdxGanttControlXMLImporterTimephasedDataState.GetTimephasedData
  : TdxGanttControlTimephasedDataItem;
begin
  Result := TdxGanttControlTimephasedDataItem(Element);
end;

{ TdxGanttControlXMLImporterResourceState }

procedure TdxGanttControlXMLImporterResourceState.BeforeImport;
begin
  inherited BeforeImport;
  Resource.AvailabilityPeriods.Clear;
  Resource.ExtendedAttributes.Clear;
  Resource.Baselines.Clear;
  Resource.OutlineCodes.Clear;
  Resource.Rates.Clear;
  Resource.TimephasedDataItems.Clear;
end;

function TdxGanttControlXMLImporterResourceState.CreateStateHandlers
  : TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('availabilityperiods', AvailabilityPeriodsHandler);
  Result.Add('extendedattribute', ExtendedAttributeHandler);
  Result.Add('baseline', BaselineHandler);
  Result.Add('outlinecode', OutlineCodeHandler);
  Result.Add('rates', RatesHandler);
  Result.Add('timephaseddata', TimephasedDataHandler);
end;

procedure TdxGanttControlXMLImporterResourceState.DoImport(ANode: TdxXMLNode);
var
  AName: string;
begin
  AName := LowerCase(ANode.NameAsString);
  if AName = 'uid' then
    TdxGanttControlResourceAccess(Resource).SetUID(ANode.TextAsInteger)
  else if AName = 'id' then
    Resource.ID := ANode.TextAsInteger
  else if AName = 'guid' then
    TdxGanttControlResourceAccess(Resource).GUID := ANode.TextAsString
  else if AName = 'name' then
    Resource.Name := ANode.TextAsString
  else if AName = 'type' then
    Resource.&Type := TdxGanttControlResourceType(ANode.TextAsInteger)
  else if AName = 'isnull' then
    Resource.Blank := ANode.TextAsBoolean
  else if AName = 'initials' then
    Resource.Initials := ANode.TextAsString
  else if AName = 'phonetics' then
    Resource.Phonetics := ANode.TextAsString
  else if AName = 'ntaccount' then
    Resource.NTAccount := ANode.TextAsString
  else if AName = 'materiallabel' then
    Resource.MaterialLabel := ANode.TextAsString
  else if AName = 'code' then
    Resource.Code := ANode.TextAsString
  else if AName = 'group' then
    Resource.Group := ANode.TextAsString
  else if AName = 'workgroup' then
    Resource.WorkGroup := TdxGanttControlResourceWorkGroup(ANode.TextAsInteger)
  else if AName = 'emailaddress' then
    Resource.EmailAddress := ANode.TextAsString
  else if AName = 'hyperlink' then
    Resource.Hyperlink := ANode.TextAsString
  else if AName = 'hyperlinkaddress' then
    Resource.HyperlinkAddress := ANode.TextAsString
  else if AName = 'hyperlinksubaddress' then
    Resource.HyperlinkSubAddress := ANode.TextAsString
  else if AName = 'maxunits' then
    Resource.MaxUnits := ANode.TextAsNumber
  else if AName = 'peakunits' then
    Resource.PeakUnits := ANode.TextAsNumber
  else if AName = 'overallocated' then
    Resource.OverAllocated := ANode.TextAsBoolean
  else if AName = 'availablefrom' then
    Resource.AvailableFrom := ANode.TextAsDateTime
  else if AName = 'availableto' then
    Resource.AvailableTo := ANode.TextAsDateTime
  else if AName = 'start' then
    Resource.Start := ANode.TextAsDateTime
  else if AName = 'finish' then
    Resource.Finish := ANode.TextAsDateTime
  else if AName = 'canlevel' then
    Resource.CanLevel := ANode.TextAsBoolean
  else if AName = 'accrueat' then
    Resource.AccrueAt := ANode.TextAsInteger
  else if AName = 'work' then
    Resource.Work := ANode.TextAsDuration
  else if AName = 'regularwork' then
    Resource.RegularWork := ANode.TextAsDuration
  else if AName = 'overtimework' then
    Resource.OvertimeWork := ANode.TextAsDuration
  else if AName = 'actualwork' then
    Resource.ActualWork := ANode.TextAsDuration
  else if AName = 'remainingwork' then
    Resource.RemainingWork := ANode.TextAsDuration
  else if AName = 'actualovertimework' then
    Resource.ActualOvertimeWork := ANode.TextAsDuration
  else if AName = 'remainingovertimework' then
    Resource.RemainingOvertimeWork := ANode.TextAsDuration
  else if AName = 'percentworkcomplete' then
    Resource.PercentWorkComplete := ANode.TextAsInteger
  else if AName = 'standardrate' then
    Resource.StandardRate := ANode.TextAsNumber
  else if AName = 'standardrateformat' then
    Resource.StandardRateFormat := TdxGanttControlRateFormat
      (ANode.TextAsInteger)
  else if AName = 'cost' then
    Resource.Cost := ANode.TextAsNumber
  else if AName = 'overtimerate' then
    Resource.OvertimeRate := ANode.TextAsNumber
  else if AName = 'overtimerateformat' then
    Resource.OvertimeRateFormat := TdxGanttControlRateFormat
      (ANode.TextAsInteger)
  else if AName = 'overtimecost' then
    Resource.OvertimeCost := ANode.TextAsNumber
  else if AName = 'costperuse' then
    Resource.CostPerUse := ANode.TextAsNumber
  else if AName = 'actualcost' then
    Resource.ActualCost := ANode.TextAsNumber
  else if AName = 'actualovertimecost' then
    Resource.ActualOvertimeCost := ANode.TextAsNumber
  else if AName = 'remainingcost' then
    Resource.RemainingCost := ANode.TextAsNumber
  else if AName = 'remainingovertimecost' then
    Resource.RemainingOvertimeCost := ANode.TextAsNumber
  else if AName = 'workvariance' then
    Resource.WorkVariance := ANode.TextAsNumber
  else if AName = 'costvariance' then
    Resource.CostVariance := ANode.TextAsNumber
  else if AName = 'sv' then
    Resource.SV := ANode.TextAsNumber
  else if AName = 'cv' then
    Resource.CV := ANode.TextAsNumber
  else if AName = 'acwp' then
    Resource.ACWP := ANode.TextAsNumber
  else if AName = 'calendaruid' then
    Resource.CalendarUID := ANode.TextAsInteger
  else if AName = 'notes' then
    Resource.Notes := ANode.TextAsString
  else if AName = 'bcws' then
    Resource.BCWS := ANode.TextAsNumber
  else if AName = 'bcwp' then
    Resource.BCWP := ANode.TextAsNumber
  else if AName = 'isgeneric' then
    Resource.Generic := ANode.TextAsBoolean
  else if AName = 'isinactive' then
    Resource.Inactive := ANode.TextAsBoolean
  else if AName = 'isenterprise' then
    Resource.Enterprise := ANode.TextAsBoolean
  else if AName = 'bookingtype' then
    Resource.BookingType := TdxGanttControlBookingType(ANode.TextAsInteger)
  else if AName = 'actualworkprotected' then
    Resource.ActualWorkProtected := ANode.TextAsDuration
  else if AName = 'actualovertimeworkprotected' then
    Resource.ActualOvertimeWorkProtected := ANode.TextAsDuration
  else if AName = 'activedirectoryguid' then
    Resource.ActiveDirectoryGUID := ANode.TextAsString
  else if AName = 'creationdate' then
    Resource.Created := ANode.TextAsDateTime
  else if AName = 'iscostresource' then
    Resource.CostResource := ANode.TextAsBoolean
  else if AName = 'assnowner' then
    Resource.AssnOwner := ANode.TextAsString
  else if AName = 'assnownerguid' then
    Resource.AssnOwnerGuid := ANode.TextAsString
  else if AName = 'isbudget' then
    Resource.Budget := ANode.TextAsBoolean
  else
    inherited DoImport(ANode);
end;

function TdxGanttControlXMLImporterResourceState.AvailabilityPeriodsHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterResourceAvailabilityPeriodsState.Create
    (ANode, Resource.AvailabilityPeriods);
end;

function TdxGanttControlXMLImporterResourceState.BaselineHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterResourceBaselineState.Create(ANode,
    Resource.Baselines.Append);
end;

function TdxGanttControlXMLImporterResourceState.ExtendedAttributeHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterExtendedAttributeReferenceState.Create
    (ANode, Resource.ExtendedAttributes.Append);
end;

function TdxGanttControlXMLImporterResourceState.GetResource
  : TdxGanttControlResource;
begin
  Result := TdxGanttControlResource(Element);
end;

function TdxGanttControlXMLImporterResourceState.OutlineCodeHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterOutlineCodeReferenceState.Create(ANode,
    Resource.OutlineCodes.Append);
end;

function TdxGanttControlXMLImporterResourceState.RatesHandler(ANode: TdxXMLNode)
  : TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterResourceRatesState.Create(ANode,
    Resource.Rates);
end;

function TdxGanttControlXMLImporterResourceState.TimephasedDataHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterTimephasedDataState.Create(ANode,
    Resource.TimephasedDataItems.Append);
end;

{ TdxGanttControlXMLImporterResourcesState }

function TdxGanttControlXMLImporterResourcesState.CreateStateHandlers
  : TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('resource', ResourceHandler);
end;

function TdxGanttControlXMLImporterResourcesState.GetResources
  : TdxGanttControlResources;
begin
  Result := TdxGanttControlResources(Element);
end;

function TdxGanttControlXMLImporterResourcesState.ResourceHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterResourceState.Create(ANode,
    Resources.Append);
end;

{ TdxGanttControlXMLImporterResourceAvailabilityPeriodState }

procedure TdxGanttControlXMLImporterResourceAvailabilityPeriodState.DoImport
  (ANode: TdxXMLNode);
var
  AName: string;
begin
  AName := LowerCase(ANode.NameAsString);
  if AName = 'availablefrom' then
    AvailabilityPeriod.AvailableFrom := ANode.TextAsDateTime
  else if AName = 'availableto' then
    AvailabilityPeriod.AvailableTo := ANode.TextAsDateTime
  else if AName = 'availableunits' then
    AvailabilityPeriod.AvailableUnits := ANode.TextAsNumber
  else
    inherited DoImport(ANode);
end;

function TdxGanttControlXMLImporterResourceAvailabilityPeriodState.
  GetAvailabilityPeriod: TdxGanttControlResourceAvailabilityPeriod;
begin
  Result := TdxGanttControlResourceAvailabilityPeriod(Element);
end;

{ TdxGanttControlXMLImporterOutlineCodeMaskState }

procedure TdxGanttControlXMLImporterOutlineCodeMaskState.DoImport
  (ANode: TdxXMLNode);
var
  AName: string;
begin
  AName := LowerCase(ANode.NameAsString);
  if AName = 'level' then
    Item.Level := ANode.TextAsInteger
  else if AName = 'type' then
    Item.&Type := TdxGanttControlOutlineCodeMaskType(ANode.TextAsInteger)
  else if AName = 'length' then
    Item.Length := ANode.TextAsInteger
  else if AName = 'separator' then
    Item.Separator := ANode.TextAsString
  else
    inherited DoImport(ANode);
end;

function TdxGanttControlXMLImporterOutlineCodeMaskState.GetItem
  : TdxGanttControlOutlineCodeMask;
begin
  Result := TdxGanttControlOutlineCodeMask(Element);
end;

{ TdxGanttControlXMLImporterOutlineCodeMasksState }

function TdxGanttControlXMLImporterOutlineCodeMasksState.CreateStateHandlers
  : TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('mask', ListItemHandler);
end;

function TdxGanttControlXMLImporterOutlineCodeMasksState.GetList
  : TdxGanttControlOutlineCodeMasks;
begin
  Result := TdxGanttControlOutlineCodeMasks(Element);
end;

function TdxGanttControlXMLImporterOutlineCodeMasksState.ListItemHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterOutlineCodeMaskState.Create(ANode,
    List.Append);
end;

{ TdxGanttControlXMLImporterOutlineCodeValueState }

procedure TdxGanttControlXMLImporterOutlineCodeValueState.DoImport
  (ANode: TdxXMLNode);
var
  AName: string;
begin
  AName := LowerCase(ANode.NameAsString);
  if AName = 'valueid' then
    Item.ValueID := ANode.TextAsInteger
  else if AName = 'fieldguid' then
    Item.FieldGUID := ANode.TextAsString
  else if AName = 'type' then
    Item.&Type := TdxGanttControlOutlineCodeValueType(ANode.TextAsInteger)
  else if AName = 'parentvalueid' then
    Item.ParentValueID := ANode.TextAsInteger
  else if AName = 'value' then
    Item.Value := ANode.TextAsString
  else if AName = 'description' then
    Item.Description := ANode.TextAsString
  else if AName = 'iscollapsed' then
    Item.Collapsed := ANode.TextAsBoolean
  else
    inherited DoImport(ANode);
end;

function TdxGanttControlXMLImporterOutlineCodeValueState.GetItem
  : TdxGanttControlOutlineCodeValue;
begin
  Result := TdxGanttControlOutlineCodeValue(Element);
end;

{ TdxGanttControlXMLImporterOutlineCodeValuesState }

function TdxGanttControlXMLImporterOutlineCodeValuesState.CreateStateHandlers
  : TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('value', ListItemHandler);
end;

function TdxGanttControlXMLImporterOutlineCodeValuesState.GetList
  : TdxGanttControlOutlineCodeValues;
begin
  Result := TdxGanttControlOutlineCodeValues(Element);
end;

function TdxGanttControlXMLImporterOutlineCodeValuesState.ListItemHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterOutlineCodeValueState.Create(ANode,
    List.Append);
end;

{ TdxGanttControlXMLImporterOutlineCodeState }

procedure TdxGanttControlXMLImporterOutlineCodeState.BeforeImport;
begin
  inherited BeforeImport;
  Item.Masks.Clear;
  Item.Values.Clear;
end;

function TdxGanttControlXMLImporterOutlineCodeState.CreateStateHandlers
  : TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('masks', MasksHandler);
  Result.Add('values', ValuesHandler);
end;

procedure TdxGanttControlXMLImporterOutlineCodeState.DoImport
  (ANode: TdxXMLNode);
var
  AName: string;
begin
  AName := LowerCase(ANode.NameAsString);
  if AName = 'guid' then
    Item.GUID := ANode.TextAsString
  else if AName = 'fieldid' then
    Item.FieldID := ANode.TextAsInteger
  else if AName = 'fieldname' then
    Item.FieldName := ANode.TextAsString
  else if AName = 'alias' then
    Item.Alias := ANode.TextAsString
  else if AName = 'phoneticalias' then
    Item.PhoneticAlias := ANode.TextAsString
  else if AName = 'enterprise' then
    Item.Enterprise := ANode.TextAsBoolean
  else if AName = 'enterpriseoutlinecodealias' then
    Item.EnterpriseOutlineCodeAlias := ANode.TextAsInteger
  else if AName = 'resourcesubstitutionenabled' then
    Item.ResourceSubstitutionEnabled := ANode.TextAsBoolean
  else if AName = 'leafonly' then
    Item.LeafOnly := ANode.TextAsBoolean
  else if AName = 'alllevelsrequired' then
    Item.AllLevelsRequired := ANode.TextAsBoolean
  else if AName = 'onlytablevaluesallowed' then
    Item.OnlyTableValuesAllowed := ANode.TextAsBoolean
  else if AName = 'showindent' then
    Item.ShowIndent := ANode.TextAsBoolean
  else
    inherited DoImport(ANode);
end;

function TdxGanttControlXMLImporterOutlineCodeState.GetItem
  : TdxGanttControlOutlineCode;
begin
  Result := TdxGanttControlOutlineCode(Element);
end;

function TdxGanttControlXMLImporterOutlineCodeState.MasksHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterOutlineCodeMasksState.Create(ANode,
    Item.Masks);
end;

function TdxGanttControlXMLImporterOutlineCodeState.ValuesHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterOutlineCodeValuesState.Create(ANode,
    Item.Values);
end;

{ TdxGanttControlXMLImporterOutlineCodesState }

function TdxGanttControlXMLImporterOutlineCodesState.CreateStateHandlers
  : TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('outlinecode', ListItemHandler);
end;

function TdxGanttControlXMLImporterOutlineCodesState.GetList
  : TdxGanttControlOutlineCodes;
begin
  Result := TdxGanttControlOutlineCodes(Element);
end;

function TdxGanttControlXMLImporterOutlineCodesState.ListItemHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterOutlineCodeState.Create(ANode,
    List.Append);
end;

{ TdxGanttControlXMLImporterExtendedAttributeValueState }

procedure TdxGanttControlXMLImporterExtendedAttributeValueState.DoImport
  (ANode: TdxXMLNode);
var
  AName: string;
begin
  AName := LowerCase(ANode.NameAsString);
  if AName = 'id' then
    Item.ID := ANode.TextAsInteger
  else if AName = 'value' then
    Item.Value := ANode.TextAsString
  else if AName = 'description' then
    Item.Description := ANode.TextAsString
  else if AName = 'phonetic' then
    Item.Phonetic := ANode.TextAsString
  else
    inherited DoImport(ANode);
end;

function TdxGanttControlXMLImporterExtendedAttributeValueState.GetItem
  : TdxGanttControlExtendedAttributeValue;
begin
  Result := TdxGanttControlExtendedAttributeValue(Element);
end;

{ TdxGanttControlXMLImporterExtendedAttributeValueListState }

function TdxGanttControlXMLImporterExtendedAttributeValueListState.
  CreateStateHandlers: TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('value', ListItemHandler);
end;

function TdxGanttControlXMLImporterExtendedAttributeValueListState.GetList
  : TdxGanttControlExtendedAttributeValues;
begin
  Result := TdxGanttControlExtendedAttributeValues(Element);
end;

function TdxGanttControlXMLImporterExtendedAttributeValueListState.
  ListItemHandler(ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterExtendedAttributeValueState.Create(ANode,
    List.Append);
end;

{ TdxGanttControlXMLImporterExtendedAttributeState }

procedure TdxGanttControlXMLImporterExtendedAttributeState.BeforeImport;
begin
  inherited BeforeImport;
  Item.ValueList.Clear;
end;

function TdxGanttControlXMLImporterExtendedAttributeState.CreateStateHandlers
  : TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('valuelist', ValueListHandler);
end;

procedure TdxGanttControlXMLImporterExtendedAttributeState.DoImport
  (ANode: TdxXMLNode);
var
  AName: string;
begin
  AName := LowerCase(ANode.NameAsString);
  if AName = 'fieldid' then
    Item.FieldID := ANode.TextAsInteger
  else if AName = 'fieldname' then
    Item.FieldName := ANode.TextAsString
  else if AName = 'cftype' then
    Item.CFType := TdxGanttControlExtendedAttributeCFType(ANode.TextAsInteger)
  else if AName = 'guid' then
    Item.GUID := ANode.TextAsString
  else if AName = 'elemtype' then
    Item.ElemType := TdxGanttControlExtendedAttributeElemType
      (ANode.TextAsInteger)
  else if AName = 'maxmultivalues' then
    Item.MaxMultiValues := ANode.TextAsInteger
  else if AName = 'userdef' then
    Item.UserDef := ANode.TextAsBoolean
  else if AName = 'alias' then
    Item.Alias := ANode.TextAsString
  else if AName = 'secondaryguid' then
    Item.SecondaryGUID := ANode.TextAsString
  else if AName = 'secondarypid' then
    Item.SecondaryPID := ANode.TextAsString
  else if AName = 'autorolldown' then
    Item.AutoRollDown := ANode.TextAsBoolean
  else if AName = 'defaultguid' then
    Item.DefaultGUID := ANode.TextAsString
  else if AName = 'ltuid' then
    Item.Ltuid := ANode.TextAsString
  else if AName = 'phoneticalias' then
    Item.PhoneticAlias := ANode.TextAsString
  else if AName = 'rolluptype' then
    Item.RollupType := TdxGanttControlExtendedAttributeRollupType
      (ANode.TextAsInteger)
  else if AName = 'calculationtype' then
    Item.CalculationType := TdxGanttControlExtendedAttributeCalculationType
      (ANode.TextAsInteger)
  else if AName = 'formula' then
    Item.Formula := ANode.TextAsString
  else if AName = 'restrictvalues' then
    Item.RestrictValues := ANode.TextAsBoolean
  else if AName = 'valuelistsortorder' then
    Item.ValuelistSortOrder :=
      TdxGanttControlExtendedAttributeValuelistSortOrder(ANode.TextAsInteger)
  else if AName = 'appendnewvalues' then
    Item.AppendNewValues := ANode.TextAsBoolean
  else if AName = 'default' then
    Item.Default := ANode.TextAsString
  else
    inherited DoImport(ANode);
end;

function TdxGanttControlXMLImporterExtendedAttributeState.GetItem
  : TdxGanttControlExtendedAttribute;
begin
  Result := TdxGanttControlExtendedAttribute(Element);
end;

function TdxGanttControlXMLImporterExtendedAttributeState.ValueListHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterExtendedAttributeValueListState.Create
    (ANode, Item.ValueList);
end;

{ TdxGanttControlXMLImporterExtendedAttributesState }

function TdxGanttControlXMLImporterExtendedAttributesState.CreateStateHandlers
  : TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('extendedattribute', ListItemHandler);
end;

function TdxGanttControlXMLImporterExtendedAttributesState.GetList
  : TdxGanttControlExtendedAttributes;
begin
  Result := TdxGanttControlExtendedAttributes(Element);
end;

function TdxGanttControlXMLImporterExtendedAttributesState.ListItemHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterExtendedAttributeState.Create(ANode,
    List.Append);
end;

{ TdxGanttControlXMLImporterAssignmentBaselineState }

function TdxGanttControlXMLImporterAssignmentBaselineState.CreateStateHandlers
  : TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('timephaseddata', TimephasedDataHandler);
end;

procedure TdxGanttControlXMLImporterAssignmentBaselineState.DoImport
  (ANode: TdxXMLNode);
var
  AName: string;
begin
  AName := LowerCase(ANode.NameAsString);
  if AName = 'finish' then
    Item.Finish := ANode.TextAsDateTime
  else if AName = 'start' then
    Item.Start := ANode.TextAsDateTime
  else
    inherited DoImport(ANode);
end;

function TdxGanttControlXMLImporterAssignmentBaselineState.GetItem
  : TdxGanttControlAssignmentBaseline;
begin
  Result := TdxGanttControlAssignmentBaseline(Baseline);
end;

function TdxGanttControlXMLImporterAssignmentBaselineState.TimephasedDataHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterTimephasedDataState.Create(ANode,
    Item.TimephasedDataItems.Append);
end;

{ TdxGanttControlXMLImporterEnterpriseExtendedAttributeValueState }

procedure TdxGanttControlXMLImporterEnterpriseExtendedAttributeValueState.
  DoImport(ANode: TdxXMLNode);
var
  AName: string;
begin
  AName := LowerCase(ANode.NameAsString);
  if AName = 'id' then
    Item.ID := ANode.TextAsInteger
  else if AName = 'value' then
    Item.Value := ANode.TextAsString
  else if AName = 'description' then
    Item.Description := ANode.TextAsString
  else if AName = 'phonetic' then
    Item.Phonetic := ANode.TextAsString
  else
    inherited DoImport(ANode);
end;

function TdxGanttControlXMLImporterEnterpriseExtendedAttributeValueState.GetItem
  : TdxGanttControlEnterpriseExtendedAttributeValue;
begin
  Result := TdxGanttControlEnterpriseExtendedAttributeValue(Element);
end;

{ TdxGanttControlXMLImporterEnterpriseExtendedAttributeState }

function TdxGanttControlXMLImporterEnterpriseExtendedAttributeState.
  CreateStateHandlers: TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('value', ValueHandler);
end;

procedure TdxGanttControlXMLImporterEnterpriseExtendedAttributeState.DoImport
  (ANode: TdxXMLNode);
var
  AName: string;
begin
  AName := LowerCase(ANode.NameAsString);
  if AName = 'fieldidinhex' then
    Item.FieldIDInHex := ANode.TextAsString
  else if AName = 'fieldid' then
    Item.FieldID := ANode.TextAsInteger
  else if AName = 'fieldvalueguid' then
    Item.FieldValueGUID := ANode.TextAsString
  else
    inherited DoImport(ANode);
end;

function TdxGanttControlXMLImporterEnterpriseExtendedAttributeState.GetItem
  : TdxGanttControlEnterpriseExtendedAttribute;
begin
  Result := TdxGanttControlEnterpriseExtendedAttribute(Element);
end;

function TdxGanttControlXMLImporterEnterpriseExtendedAttributeState.ValueHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterEnterpriseExtendedAttributeValueState.
    Create(ANode, Item.Value);
end;

{ TdxGanttControlXMLImporterAssignmentState }

procedure TdxGanttControlXMLImporterAssignmentState.BeforeImport;
begin
  inherited BeforeImport;
  Item.Baselines.Clear;
  Item.EnterpriseExtendedAttributes.Clear;
  Item.ExtendedAttributes.Clear;
  Item.TimephasedDataItems.Clear;
end;

function TdxGanttControlXMLImporterAssignmentState.CreateStateHandlers
  : TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('baseline', BaselineHandler);
  Result.Add('enterpriseextendedattribute', EnterpriseExtendedAttributeHandler);
  Result.Add('extendedattribute', ExtendedAttributeHandler);
  Result.Add('timephaseddata', TimephasedDataHandler);
end;

procedure TdxGanttControlXMLImporterAssignmentState.DoImport(ANode: TdxXMLNode);
var
  AName: string;
begin
  AName := LowerCase(ANode.NameAsString);
  if AName = 'uid' then
    TdxGanttControlAssignmentAccess(Item).SetUID(ANode.TextAsInteger)
  else if AName = 'guid' then
    Item.GUID := ANode.TextAsString
  else if AName = 'taskuid' then
    Item.TaskUID := ANode.TextAsInteger
  else if AName = 'resourceuid' then
    Item.ResourceUID := ANode.TextAsInteger
  else if AName = 'percentworkcomplete' then
    Item.PercentWorkComplete := ANode.TextAsInteger
  else if AName = 'actualcost' then
    Item.ActualCost := ANode.TextAsNumber
  else if AName = 'actualfinish' then
    Item.ActualFinish := ANode.TextAsDateTime
  else if AName = 'actualovertimecost' then
    Item.ActualOvertimeCost := ANode.TextAsNumber
  else if AName = 'actualovertimework' then
    Item.ActualOvertimeWork := ANode.TextAsDuration
  else if AName = 'actualstart' then
    Item.ActualStart := ANode.TextAsDateTime
  else if AName = 'actualwork' then
    Item.ActualWork := ANode.TextAsDuration
  else if AName = 'acwp' then
    Item.ACWP := ANode.TextAsNumber
  else if AName = 'confirmed' then
    Item.Confirmed := ANode.TextAsBoolean
  else if AName = 'cost' then
    Item.Cost := ANode.TextAsNumber
  else if AName = 'costratetable' then
    Item.CostRateTable := TdxGanttControlCostRateTable(ANode.TextAsInteger)
  else if AName = 'costvariance' then
    Item.CostVariance := ANode.TextAsNumber
  else if AName = 'cv' then
    Item.CV := ANode.TextAsNumber
  else if AName = 'delay' then
    Item.Delay := ANode.TextAsInteger
  else if AName = 'finish' then
    Item.Finish := ANode.TextAsDateTime
  else if AName = 'finishvariance' then
    Item.FinishVariance := ANode.TextAsInteger
  else if AName = 'hyperlink' then
    Item.Hyperlink := ANode.TextAsString
  else if AName = 'hyperlinkaddress' then
    Item.HyperlinkAddress := ANode.TextAsString
  else if AName = 'hyperlinksubaddress' then
    Item.HyperlinkSubAddress := ANode.TextAsString
  else if AName = 'workvariance' then
    Item.WorkVariance := ANode.TextAsNumber
  else if AName = 'hasfixedrateunits' then
    Item.HasFixedRateUnits := ANode.TextAsBoolean
  else if AName = 'fixedmaterial' then
    Item.FixedMaterial := ANode.TextAsBoolean
  else if AName = 'levelingdelay' then
    Item.LevelingDelay := ANode.TextAsInteger
  else if AName = 'levelingdelayformat' then
    Item.LevelingDelayFormat := TdxDurationFormat(ANode.TextAsInteger)
  else if AName = 'linkedfields' then
    Item.LinkedFields := ANode.TextAsBoolean
  else if AName = 'milestone' then
    Item.Milestone := ANode.TextAsBoolean
  else if AName = 'notes' then
    Item.Notes := ANode.TextAsString
  else if AName = 'overallocated' then
    Item.OverAllocated := ANode.TextAsBoolean
  else if AName = 'overtimecost' then
    Item.OvertimeCost := ANode.TextAsNumber
  else if AName = 'overtimework' then
    Item.OvertimeWork := ANode.TextAsDuration
  else if AName = 'regularwork' then
    Item.RegularWork := ANode.TextAsDuration
  else if AName = 'remainingcost' then
    Item.RemainingCost := ANode.TextAsNumber
  else if AName = 'remainingovertimecost' then
    Item.RemainingOvertimeCost := ANode.TextAsNumber
  else if AName = 'remainingovertimework' then
    Item.RemainingOvertimeWork := ANode.TextAsDuration
  else if AName = 'remainingwork' then
    Item.RemainingWork := ANode.TextAsDuration
  else if AName = 'responsepending' then
    Item.ResponsePending := ANode.TextAsBoolean
  else if AName = 'start' then
    Item.Start := ANode.TextAsDateTime
  else if AName = 'stop' then
    Item.Stop := ANode.TextAsDateTime
  else if AName = 'resume' then
    Item.Resume := ANode.TextAsDateTime
  else if AName = 'startvariance' then
    Item.StartVariance := ANode.TextAsInteger
  else if AName = 'summary' then
    Item.Summary := ANode.TextAsBoolean
  else if AName = 'sv' then
    Item.SV := ANode.TextAsNumber
  else if AName = 'units' then
    Item.Units := ANode.TextAsNumber
  else if AName = 'updateneeded' then
    Item.UpdateNeeded := ANode.TextAsBoolean
  else if AName = 'vac' then
    Item.VAC := ANode.TextAsNumber
  else if AName = 'work' then
    Item.Work := ANode.TextAsDuration
  else if AName = 'workcontour' then
    Item.WorkContour := ANode.TextAsInteger
  else if AName = 'bcws' then
    Item.BCWS := ANode.TextAsNumber
  else if AName = 'bcwp' then
    Item.BCWP := ANode.TextAsNumber
  else if AName = 'bookingtype' then
    Item.BookingType := TdxGanttControlBookingType(ANode.TextAsInteger)
  else if AName = 'actualworkprotected' then
    Item.ActualWorkProtected := ANode.TextAsDuration
  else if AName = 'actualovertimeworkprotected' then
    Item.ActualOvertimeWorkProtected := ANode.TextAsDuration
  else if AName = 'creationdate' then
    Item.Created := ANode.TextAsDateTime
  else if AName = 'assnowner' then
    Item.AssnOwner := ANode.TextAsString
  else if AName = 'assnownerguid' then
    Item.AssnOwnerGuid := ANode.TextAsString
  else if AName = 'budgetcost' then
    Item.BudgetCost := ANode.TextAsNumber
  else if AName = 'budgetwork' then
    Item.BudgetWork := ANode.TextAsDuration
  else if AName = 'notecontainsobjects' then
    Item.NoteContainsObjects := ANode.TextAsBoolean
  else if AName = 'ratescale' then
    Item.RateScale := ANode.TextAsInteger
  else
    inherited DoImport(ANode);
end;

function TdxGanttControlXMLImporterAssignmentState.BaselineHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterAssignmentBaselineState.Create(ANode,
    Item.Baselines.Append);
end;

function TdxGanttControlXMLImporterAssignmentState.
  EnterpriseExtendedAttributeHandler(ANode: TdxXMLNode)
  : TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterEnterpriseExtendedAttributeState.Create
    (ANode, Item.EnterpriseExtendedAttributes.Append);
end;

function TdxGanttControlXMLImporterAssignmentState.ExtendedAttributeHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterExtendedAttributeReferenceState.Create
    (ANode, Item.ExtendedAttributes.Append);
end;

function TdxGanttControlXMLImporterAssignmentState.GetItem
  : TdxGanttControlAssignment;
begin
  Result := TdxGanttControlAssignment(Element);
end;

function TdxGanttControlXMLImporterAssignmentState.TimephasedDataHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterTimephasedDataState.Create(ANode,
    Item.TimephasedDataItems.Append);
end;

{ TdxGanttControlXMLImporterAssignmentsState }

function TdxGanttControlXMLImporterAssignmentsState.CreateStateHandlers
  : TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('assignment', ListItemHandler);
end;

function TdxGanttControlXMLImporterAssignmentsState.GetList
  : TdxGanttControlAssignments;
begin
  Result := TdxGanttControlAssignments(Element);
end;

function TdxGanttControlXMLImporterAssignmentsState.ListItemHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterAssignmentState.Create(ANode,
    List.Append);
end;

{ TdxGanttControlXMLImporterTaskPredecessorLinkState }

procedure TdxGanttControlXMLImporterTaskPredecessorLinkState.DoImport
  (ANode: TdxXMLNode);
var
  AName: string;
begin
  AName := LowerCase(ANode.NameAsString);
  if AName = 'predecessoruid' then
    Item.PredecessorUID := ANode.TextAsInteger
  else if AName = 'type' then
    Item.&Type := TdxGanttControlTaskPredecessorLinkType(ANode.TextAsInteger)
  else if AName = 'crossproject' then
    Item.CrossProject := ANode.TextAsBoolean
  else if AName = 'crossprojectname' then
    Item.CrossProjectName := ANode.TextAsString
  else if AName = 'linklag' then
    Item.LinkLag := ANode.TextAsInteger
  else if AName = 'lagformat' then
    Item.LagFormat := TdxGanttControlTaskPredecessorLagFormat
      (ANode.TextAsInteger)
  else
    inherited DoImport(ANode);
end;

function TdxGanttControlXMLImporterTaskPredecessorLinkState.GetItem
  : TdxGanttControlTaskPredecessorLink;
begin
  Result := TdxGanttControlTaskPredecessorLink(Element);
end;

{ TdxGanttControlXMLImporterTaskBaselineState }

function TdxGanttControlXMLImporterTaskBaselineState.CreateStateHandlers
  : TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('timephaseddata', TimephasedDataHandler);
end;

procedure TdxGanttControlXMLImporterTaskBaselineState.DoImport
  (ANode: TdxXMLNode);
var
  AName: string;
begin
  AName := LowerCase(ANode.NameAsString);
  if AName = 'interim' then
    Baseline.Interim := ANode.TextAsBoolean
  else if AName = 'start' then
    Baseline.Start := ANode.TextAsDateTime
  else if AName = 'finish' then
    Baseline.Finish := ANode.TextAsDateTime
  else if AName = 'duration' then
    Baseline.Duration := ANode.TextAsDuration
  else if AName = 'durationformat' then
    Baseline.DurationFormat := TdxDurationFormat(ANode.TextAsInteger)
  else if AName = 'estimatedduration' then
    Baseline.EstimatedDuration := ANode.TextAsBoolean
  else if AName = 'fixedcost' then
    Baseline.FixedCost := ANode.TextAsNumber
  else
    inherited DoImport(ANode);
end;

function TdxGanttControlXMLImporterTaskBaselineState.GetBaseline
  : TdxGanttControlTaskBaseline;
begin
  Result := TdxGanttControlTaskBaseline(inherited Baseline);
end;

function TdxGanttControlXMLImporterTaskBaselineState.TimephasedDataHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterTimephasedDataState.Create(ANode,
    Baseline.TimephasedDataItems.Append);
end;

{ TdxGanttControlXMLImporterTaskState }

procedure TdxGanttControlXMLImporterTaskState.BeforeImport;
begin
  inherited BeforeImport;
  Item.Baselines.Clear;
  Item.ExtendedAttributes.Clear;
  Item.OutlineCodes.Clear;
  Item.TimephasedDataItems.Clear;
  Item.PredecessorLinks.Clear;
  Item.EnterpriseExtendedAttributes.Clear;
end;

function TdxGanttControlXMLImporterTaskState.CreateStateHandlers
  : TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('baseline', BaselineHandler);
  Result.Add('extendedattribute', ExtendedAttributeHandler);
  Result.Add('outlinecode', OutlineCodeHandler);
  Result.Add('timephaseddata', TimephasedDataHandler);
  Result.Add('predecessorlink', PredecessorLinkHandler);
  Result.Add('enterpriseextendedattribute', EnterpriseExtendedAttributeHandler);
end;

procedure TdxGanttControlXMLImporterTaskState.DoImport(ANode: TdxXMLNode);
var
  AName: string;
begin
  AName := LowerCase(ANode.NameAsString);
  if AName = 'uid' then
    TdxGanttControlTaskAccess(Item).SetUID(ANode.TextAsInteger)
  else if AName = 'id' then
    Item.ID := ANode.TextAsInteger
  else if AName = 'guid' then
    Item.GUID := ANode.TextAsString
  else if AName = 'name' then
    Item.Name := ANode.TextAsString
  else if AName = 'type' then
    Item.&Type := TdxGanttControlTaskType(ANode.TextAsInteger)
  else if AName = 'isnull' then
    Item.Blank := ANode.TextAsBoolean
  else if AName = 'createdate' then
    Item.Created := ANode.TextAsDateTime
  else if AName = 'contact' then
    Item.Contact := ANode.TextAsString
  else if AName = 'wbs' then
    Item.WBS := ANode.TextAsString
  else if AName = 'wbslevel' then
    Item.WBSLevel := ANode.TextAsString
  else if AName = 'outlinenumber' then
    Item.OutlineNumber := ANode.TextAsString
  else if AName = 'outlinelevel' then
    Item.OutlineLevel := ANode.TextAsInteger
  else if AName = 'priority' then
    Item.Priority := ANode.TextAsInteger
  else if AName = 'start' then
    Item.Start := ANode.TextAsDateTime
  else if AName = 'finish' then
    Item.Finish := ANode.TextAsDateTime
  else if AName = 'duration' then
    Item.Duration := ANode.TextAsDuration
  else if AName = 'durationformat' then
    Item.DurationFormat := TdxDurationFormat(ANode.TextAsInteger)
  else if AName = 'work' then
    Item.Work := ANode.TextAsDuration
  else if AName = 'stop' then
    Item.Stop := ANode.TextAsDateTime
  else if AName = 'resume' then
    Item.Resume := ANode.TextAsDateTime
  else if AName = 'resumevalid' then
    Item.ResumeValid := ANode.TextAsBoolean
  else if AName = 'effortdriven' then
    Item.EffortDriven := ANode.TextAsBoolean
  else if AName = 'recurring' then
    Item.Recurring := ANode.TextAsBoolean
  else if AName = 'overallocated' then
    Item.OverAllocated := ANode.TextAsBoolean
  else if AName = 'estimated' then
    Item.Estimated := ANode.TextAsBoolean
  else if AName = 'milestone' then
    Item.Milestone := ANode.TextAsBoolean
  else if AName = 'summary' then
    Item.Summary := ANode.TextAsBoolean
  else if AName = 'critical' then
    Item.Critical := ANode.TextAsBoolean
  else if AName = 'issubproject' then
    Item.Subproject := ANode.TextAsBoolean
  else if AName = 'issubprojectreadonly' then
    Item.SubprojectReadOnly := ANode.TextAsBoolean
  else if AName = 'subprojectname' then
    Item.SubprojectName := ANode.TextAsString
  else if AName = 'externaltask' then
    Item.ExternalTask := ANode.TextAsBoolean
  else if AName = 'externaltaskproject' then
    Item.ExternalTaskProject := ANode.TextAsString
  else if AName = 'earlystart' then
    Item.EarlyStart := ANode.TextAsDateTime
  else if AName = 'earlyfinish' then
    Item.EarlyFinish := ANode.TextAsDateTime
  else if AName = 'latestart' then
    Item.LateStart := ANode.TextAsDateTime
  else if AName = 'latefinish' then
    Item.LateFinish := ANode.TextAsDateTime
  else if AName = 'startvariance' then
    Item.StartVariance := ANode.TextAsInteger
  else if AName = 'finishvariance' then
    Item.FinishVariance := ANode.TextAsInteger
  else if AName = 'workvariance' then
    Item.WorkVariance := ANode.TextAsNumber
  else if AName = 'freeslack' then
    Item.FreeSlack := ANode.TextAsInteger
  else if AName = 'totalslack' then
    Item.TotalSlack := ANode.TextAsInteger
  else if AName = 'fixedcost' then
    Item.FixedCost := ANode.TextAsNumber
  else if AName = 'fixedcostaccrual' then
    Item.FixedCostAccrual := ANode.TextAsInteger
  else if AName = 'percentcomplete' then
    Item.PercentComplete := ANode.TextAsInteger
  else if AName = 'percentworkcomplete' then
    Item.PercentWorkComplete := ANode.TextAsInteger
  else if AName = 'cost' then
    Item.Cost := ANode.TextAsNumber
  else if AName = 'overtimecost' then
    Item.OvertimeCost := ANode.TextAsNumber
  else if AName = 'overtimework' then
    Item.OvertimeWork := ANode.TextAsDuration
  else if AName = 'actualstart' then
    Item.ActualStart := ANode.TextAsDateTime
  else if AName = 'actualfinish' then
    Item.ActualFinish := ANode.TextAsDateTime
  else if AName = 'actualduration' then
    Item.ActualDuration := ANode.TextAsDuration
  else if AName = 'actualcost' then
    Item.ActualCost := ANode.TextAsNumber
  else if AName = 'actualovertimecost' then
    Item.ActualOvertimeCost := ANode.TextAsNumber
  else if AName = 'actualwork' then
    Item.ActualWork := ANode.TextAsDuration
  else if AName = 'actualovertimework' then
    Item.ActualOvertimeWork := ANode.TextAsDuration
  else if AName = 'regularwork' then
    Item.RegularWork := ANode.TextAsDuration
  else if AName = 'remainingduration' then
    Item.RemainingDuration := ANode.TextAsDuration
  else if AName = 'remainingcost' then
    Item.RemainingCost := ANode.TextAsNumber
  else if AName = 'remainingwork' then
    Item.RemainingWork := ANode.TextAsDuration
  else if AName = 'remainingovertimecost' then
    Item.RemainingOvertimeCost := ANode.TextAsNumber
  else if AName = 'remainingovertimework' then
    Item.RemainingOvertimeWork := ANode.TextAsDuration
  else if AName = 'acwp' then
    Item.ACWP := ANode.TextAsNumber
  else if AName = 'cv' then
    Item.CV := ANode.TextAsNumber
  else if AName = 'constrainttype' then
    Item.ConstraintType := TdxGanttControlTaskConstraintType
      (ANode.TextAsInteger)
  else if AName = 'calendaruid' then
    Item.CalendarUID := ANode.TextAsInteger
  else if AName = 'constraintdate' then
    Item.ConstraintDate := ANode.TextAsDateTime
  else if AName = 'deadline' then
    Item.Deadline := ANode.TextAsDateTime
  else if AName = 'levelassignments' then
    Item.LevelAssignments := ANode.TextAsBoolean
  else if AName = 'levelingcansplit' then
    Item.LevelingCanSplit := ANode.TextAsBoolean
  else if AName = 'levelingdelay' then
    Item.LevelingDelay := ANode.TextAsInteger
  else if AName = 'levelingdelayformat' then
    Item.LevelingDelayFormat := TdxDurationFormat(ANode.TextAsInteger)
  else if AName = 'preleveledstart' then
    Item.PreLeveledStart := ANode.TextAsDateTime
  else if AName = 'preleveledfinish' then
    Item.PreLeveledFinish := ANode.TextAsDateTime
  else if AName = 'hyperlink' then
    Item.Hyperlink := ANode.TextAsString
  else if AName = 'hyperlinkaddress' then
    Item.HyperlinkAddress := ANode.TextAsString
  else if AName = 'hyperlinksubaddress' then
    Item.HyperlinkSubAddress := ANode.TextAsString
  else if AName = 'ignoreresourcecalendar' then
    Item.IgnoreResourceCalendar := ANode.TextAsBoolean
  else if AName = 'notes' then
    Item.Notes := ANode.TextAsString
  else if AName = 'hidebar' then
    Item.HideBar := ANode.TextAsBoolean
  else if AName = 'rollup' then
    Item.Rollup := ANode.TextAsBoolean
  else if AName = 'bcws' then
    Item.BCWS := ANode.TextAsNumber
  else if AName = 'bcwp' then
    Item.BCWP := ANode.TextAsNumber
  else if AName = 'physicalpercentcomplete' then
    Item.PhysicalPercentComplete := ANode.TextAsInteger
  else if AName = 'earnedvaluemethod' then
    Item.EarnedValueMethod := TdxGanttControlTaskEarnedValueMethod
      (ANode.TextAsInteger)
  else if AName = 'actualworkprotected' then
    Item.ActualWorkProtected := ANode.TextAsDuration
  else if AName = 'actualovertimeworkprotected' then
    Item.ActualOvertimeWorkProtected := ANode.TextAsDuration
  else if AName = 'ispublished' then
    Item.&Published := ANode.TextAsBoolean
  else if AName = 'statusmanager' then
    Item.StatusManager := ANode.TextAsString
  else if AName = 'commitmentstart' then
    Item.CommitmentStart := ANode.TextAsDateTime
  else if AName = 'commitmentfinish' then
    Item.CommitmentFinish := ANode.TextAsDateTime
  else if AName = 'commitmenttype' then
    Item.CommitmentType := TdxGanttControlTaskCommitmentType
      (ANode.TextAsInteger)
  else if AName = 'notecontainsobjects' then
    Item.NoteContainsObjects := ANode.TextAsBoolean
  else if AName = 'active' then
    Item.Active := ANode.TextAsBoolean
  else if AName = 'manual' then
    Item.Manual := ANode.TextAsBoolean
  else if AName = 'manualstart' then
    Item.ManualStart := ANode.TextAsDateTime
  else if AName = 'manualfinish' then
    Item.ManualFinish := ANode.TextAsDateTime
  else if AName = 'manualduration' then
    Item.ManualDuration := ANode.TextAsDuration
  else if AName = 'freeformdurationformat' then
    Item.FreeformDurationFormat := TdxDurationFormat(ANode.TextAsInteger)
  else if AName = 'displayontimeline' then
    Item.DisplayOnTimeline := ANode.TextAsBoolean
  else if AName = 'displayassummary' then
    Item.DisplayAsSummary := ANode.TextAsBoolean
  else if AName = 'startslack' then
    Item.StartSlack := ANode.TextAsInteger
  else if AName = 'finishslack' then
    Item.FinishSlack := ANode.TextAsInteger
  else
    inherited DoImport(ANode);
end;

function TdxGanttControlXMLImporterTaskState.GetItem: TdxGanttControlTask;
begin
  Result := TdxGanttControlTask(Element);
end;

function TdxGanttControlXMLImporterTaskState.BaselineHandler(ANode: TdxXMLNode)
  : TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterTaskBaselineState.Create(ANode,
    Item.Baselines.Append);
end;

function TdxGanttControlXMLImporterTaskState.ExtendedAttributeHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterExtendedAttributeReferenceState.Create
    (ANode, Item.ExtendedAttributes.Append);
end;

function TdxGanttControlXMLImporterTaskState.OutlineCodeHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterOutlineCodeReferenceState.Create(ANode,
    Item.OutlineCodes.Append);
end;

function TdxGanttControlXMLImporterTaskState.TimephasedDataHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterTimephasedDataState.Create(ANode,
    Item.TimephasedDataItems.Append);
end;

function TdxGanttControlXMLImporterTaskState.PredecessorLinkHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterTaskPredecessorLinkState.Create(ANode,
    Item.PredecessorLinks.Append);
end;

function TdxGanttControlXMLImporterTaskState.EnterpriseExtendedAttributeHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterEnterpriseExtendedAttributeState.Create
    (ANode, Item.EnterpriseExtendedAttributes.Append);
end;

{ TdxGanttControlXMLImporterTasksState }

function TdxGanttControlXMLImporterTasksState.CreateStateHandlers
  : TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('task', ListItemHandler);
end;

function TdxGanttControlXMLImporterTasksState.GetList: TdxGanttControlTasks;
begin
  Result := TdxGanttControlTasks(Element);
end;

function TdxGanttControlXMLImporterTasksState.ListItemHandler(ANode: TdxXMLNode)
  : TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterTaskState.Create(ANode, List.Append);
end;

{ TdxGanttControlXMLImporterWBSMaskState }

procedure TdxGanttControlXMLImporterWBSMaskState.DoImport(ANode: TdxXMLNode);
var
  AName: string;
begin
  AName := LowerCase(ANode.NameAsString);
  if AName = 'level' then
    Item.Level := ANode.TextAsInteger
  else if AName = 'type' then
    Item.&Type := TdxGanttControlWBSMaskType(ANode.TextAsInteger)
  else if AName = 'length' then
    Item.Length := ANode.TextAsInteger
  else if AName = 'separator' then
    Item.Separator := ANode.TextAsString
  else
    inherited DoImport(ANode);
end;

function TdxGanttControlXMLImporterWBSMaskState.GetItem: TdxGanttControlWBSMask;
begin
  Result := TdxGanttControlWBSMask(Element);
end;

{ TdxGanttControlXMLImporterWBSMasksMasksState }

function TdxGanttControlXMLImporterWBSMasksMasksState.CreateStateHandlers
  : TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('wbsmask', ListItemHandler);
end;

function TdxGanttControlXMLImporterWBSMasksMasksState.GetList
  : TdxGanttControlWBSMasks;
begin
  Result := TdxGanttControlWBSMasks(Element);
end;

function TdxGanttControlXMLImporterWBSMasksMasksState.ListItemHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterWBSMaskState.Create(ANode, List.Append);
end;

{ TdxGanttControlXMLImporterWBSMasksState }

function TdxGanttControlXMLImporterWBSMasksState.CreateStateHandlers
  : TdxGanttControlXMLImporterCustomState.TStateHandlers;
begin
  Result := inherited CreateStateHandlers;
  Result.Add('masks', ListItemHandler);
end;

procedure TdxGanttControlXMLImporterWBSMasksState.DoImport(ANode: TdxXMLNode);
var
  AName: string;
begin
  AName := LowerCase(ANode.NameAsString);
  if AName = 'verifyuniquecodes' then
    List.VerifyUniqueCodes := ANode.TextAsBoolean
  else if AName = 'generatecodes' then
    List.GenerateCodes := ANode.TextAsBoolean
  else if AName = 'prefix' then
    List.Prefix := ANode.TextAsString
  else
    inherited DoImport(ANode);
end;

function TdxGanttControlXMLImporterWBSMasksState.GetList
  : TdxGanttControlWBSMasks;
begin
  Result := TdxGanttControlWBSMasks(Element);
end;

function TdxGanttControlXMLImporterWBSMasksState.ListItemHandler
  (ANode: TdxXMLNode): TdxGanttControlXMLImporterCustomState;
begin
  Result := TdxGanttControlXMLImporterWBSMasksMasksState.Create(ANode, List);
end;

initialization

TdxGanttControlXMLImporter.Register;

finalization

TdxGanttControlXMLImporter.Unregister;

end.
