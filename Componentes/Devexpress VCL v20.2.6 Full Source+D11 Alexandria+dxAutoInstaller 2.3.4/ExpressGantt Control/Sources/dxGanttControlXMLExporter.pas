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

unit dxGanttControlXMLExporter;

{$I cxVer.inc}
{$I dxGanttControl.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes, System.Contnrs,
{$ENDIF}
  SysUtils, Windows, Generics.Defaults, Generics.Collections, Classes,
  dxCore, dxCoreClasses, dxXMLDoc,
  dxGanttControlCustomDataModel,
  dxGanttControlDataModel;

type
  { TdxGanttControlXMLExporter }

  TdxGanttControlXMLExporter = class(TdxGanttControlCustomExporter)
  protected
    class function GetExtensions: TArray<string>; override;
    procedure DoSaveToStream(AStream: TStream); override;
  end;

implementation

uses
  StrUtils, RTLConsts, DateUtils,
  dxCultureInfo,
  dxGanttControlTasks,
  dxGanttControlOutlineCodes,
  dxGanttControlExtendedAttributes,
  dxGanttControlCalendars,
  dxGanttControlResources,
  dxGanttControlAssignments;

type
  TdxGanttControlCalendarAccess = class(TdxGanttControlCalendar);
  TdxGanttControlResourceAccess = class(TdxGanttControlResource);
  TdxGanttControlWeekDayWorkTimesAccess = class
    (TdxGanttControlCalendarWeekDayWorkTimes);

  { TdxXMLNodeHelper }

  TdxXMLNodeHelper = class helper for TdxXMLNode
  private
    procedure SetTextAsBoolean(const Value: Boolean);
    procedure SetTextAsCost(const Value: Double);
    procedure SetTextAsDateTime(const Value: TDateTime);
    procedure SetTextAsDuration(const Value: TDuration);
    procedure SetTextAsInteger(const Value: Integer);
    procedure SetTextAsNumber(const Value: Double);
    procedure SetTextAsTime(const Value: TTime);
  public
    property TextAsBoolean: Boolean write SetTextAsBoolean;
    property TextAsCost: Double write SetTextAsCost;
    property TextAsDateTime: TDateTime write SetTextAsDateTime;
    property TextAsDuration: TDuration write SetTextAsDuration;
    property TextAsInteger: Integer write SetTextAsInteger;
    property TextAsNumber: Double write SetTextAsNumber;
    property TextAsTime: TTime write SetTextAsTime;
  end;

  TdxGanttControlXMLExporterCustomElementState = class;
  TdxGanttControlXMLExporterCustomElementStateClass = class of
    TdxGanttControlXMLExporterCustomElementState;

  { TdxGanttControlXMLExporterCustomState }

  TdxGanttControlXMLExporterCustomState = class
  strict private
    FRootNode: TdxXMLNode;
  protected
    procedure BeforeExport; virtual;
    procedure ExportElement(AClass
      : TdxGanttControlXMLExporterCustomElementStateClass; ANode: TdxXMLNode;
      AElement: TdxGanttControlModelElement);
    procedure DoExport; virtual;
  public
    constructor Create(ARootNode: TdxXMLNode);
    procedure &Export;

    property RootNode: TdxXMLNode read FRootNode;
  end;

  { TdxGanttControlXMLExporterProjectState }

  TdxGanttControlXMLExporterProjectState = class
    (TdxGanttControlXMLExporterCustomState)
  strict private
    FDataModel: TdxGanttControlDataModel;
  protected
    procedure BeforeExport; override;
    procedure DoExport; override;
  public
    constructor Create(ARootNode: TdxXMLNode;
      ADataModel: TdxGanttControlDataModel); reintroduce;

    property DataModel: TdxGanttControlDataModel read FDataModel;
  end;

  { TdxGanttControlXMLExporterCustomElementState }

  TdxGanttControlXMLExporterCustomElementState = class
    (TdxGanttControlXMLExporterCustomState)
  strict private
    FElement: TdxGanttControlModelElement;
  public
    constructor Create(ARootNode: TdxXMLNode;
      AElement: TdxGanttControlModelElement); reintroduce;

    property Element: TdxGanttControlModelElement read FElement;
  end;

  { TdxGanttControlXMLExporterOutlineCodesState }

  TdxGanttControlXMLExporterOutlineCodesState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetOutlineCodes: TdxGanttControlOutlineCodes; inline;
  protected
    procedure DoExport; override;
  public
    property OutlineCodes: TdxGanttControlOutlineCodes read GetOutlineCodes;
  end;

  { TdxGanttControlXMLExporterOutlineCodeState }

  TdxGanttControlXMLExporterOutlineCodeState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetOutlineCode: TdxGanttControlOutlineCode; inline;
  protected
    procedure DoExport; override;
  public
    property OutlineCode: TdxGanttControlOutlineCode read GetOutlineCode;
  end;

  { TdxGanttControlXMLExporterOutlineCodeMasksState }

  TdxGanttControlXMLExporterOutlineCodeMasksState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetOutlineCodeMasks: TdxGanttControlOutlineCodeMasks; inline;
  protected
    procedure DoExport; override;
  public
    property OutlineCodeMasks: TdxGanttControlOutlineCodeMasks
      read GetOutlineCodeMasks;
  end;

  { TdxGanttControlXMLExporterOutlineCodeMaskState }

  TdxGanttControlXMLExporterOutlineCodeMaskState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetOutlineCodeMask: TdxGanttControlOutlineCodeMask; inline;
  protected
    procedure DoExport; override;
  public
    property OutlineCodeMask: TdxGanttControlOutlineCodeMask
      read GetOutlineCodeMask;
  end;

  { TdxGanttControlXMLExporterOutlineCodeValuesState }

  TdxGanttControlXMLExporterOutlineCodeValuesState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetOutlineCodeValues: TdxGanttControlOutlineCodeValues; inline;
  protected
    procedure DoExport; override;
  public
    property OutlineCodeValues: TdxGanttControlOutlineCodeValues
      read GetOutlineCodeValues;
  end;

  { TdxGanttControlXMLExporterOutlineCodeValueState }

  TdxGanttControlXMLExporterOutlineCodeValueState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetOutlineCodeValue: TdxGanttControlOutlineCodeValue; inline;
  protected
    procedure DoExport; override;
  public
    property OutlineCodeValue: TdxGanttControlOutlineCodeValue
      read GetOutlineCodeValue;
  end;

  { TdxGanttControlXMLExporterWBSMasksState }

  TdxGanttControlXMLExporterWBSMasksState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetWBSMasks: TdxGanttControlWBSMasks; inline;
  protected
    procedure DoExport; override;
  public
    property WBSMasks: TdxGanttControlWBSMasks read GetWBSMasks;
  end;

  { TdxGanttControlXMLExporterWBSMaskState }

  TdxGanttControlXMLExporterWBSMaskState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetWBSMask: TdxGanttControlWBSMask; inline;
  protected
    procedure DoExport; override;
  public
    property WBSMask: TdxGanttControlWBSMask read GetWBSMask;
  end;

  { TdxGanttControlXMLExporterExtendedAttributesState }

  TdxGanttControlXMLExporterExtendedAttributesState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetExtendedAttributes: TdxGanttControlExtendedAttributes; inline;
  protected
    procedure DoExport; override;
  public
    property ExtendedAttributes: TdxGanttControlExtendedAttributes
      read GetExtendedAttributes;
  end;

  { TdxGanttControlXMLExporterExtendedAttributeState }

  TdxGanttControlXMLExporterExtendedAttributeState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetExtendedAttribute: TdxGanttControlExtendedAttribute; inline;
  protected
    procedure DoExport; override;
  public
    property ExtendedAttribute: TdxGanttControlExtendedAttribute
      read GetExtendedAttribute;
  end;

  { TdxGanttControlXMLExporterExtendedAttributeValueListState }

  TdxGanttControlXMLExporterExtendedAttributeValueListState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetValues: TdxGanttControlExtendedAttributeValues; inline;
  protected
    procedure DoExport; override;
  public
    property Values: TdxGanttControlExtendedAttributeValues read GetValues;
  end;

  { TdxGanttControlXMLExporterExtendedAttributeValueState }

  TdxGanttControlXMLExporterExtendedAttributeValueState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetValue: TdxGanttControlExtendedAttributeValue; inline;
  protected
    procedure DoExport; override;
  public
    property Value: TdxGanttControlExtendedAttributeValue read GetValue;
  end;

  { TdxGanttControlXMLExporterCalendarsState }

  TdxGanttControlXMLExporterCalendarsState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetCalendars: TdxGanttControlCalendars; inline;
  protected
    procedure DoExport; override;
  public
    property Calendars: TdxGanttControlCalendars read GetCalendars;
  end;

  { TdxGanttControlXMLExporterCalendarState }

  TdxGanttControlXMLExporterCalendarState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetCalendar: TdxGanttControlCalendar; inline;
  protected
    procedure DoExport; override;
  public
    property Calendar: TdxGanttControlCalendar read GetCalendar;
  end;

  { TdxGanttControlXMLExporterWeekDaysState }

  TdxGanttControlXMLExporterWeekDaysState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetWeekDays: TdxGanttControlCalendarWeekDays; inline;
  protected
    procedure DoExport; override;
  public
    property WeekDays: TdxGanttControlCalendarWeekDays read GetWeekDays;
  end;

  { TdxGanttControlXMLExporterWeekDayState }

  TdxGanttControlXMLExporterWeekDayState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetWeekDay: TdxGanttControlCalendarWeekDay; inline;
  protected
    procedure DoExport; override;
  public
    property WeekDay: TdxGanttControlCalendarWeekDay read GetWeekDay;
  end;

  { TdxGanttControlXMLExporterTimePeriodState }

  TdxGanttControlXMLExporterTimePeriodState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetTimePeriod: TdxGanttControlTimePeriod; inline;
  protected
    procedure DoExport; override;
  public
    property TimePeriod: TdxGanttControlTimePeriod read GetTimePeriod;
  end;

  { TdxGanttControlXMLExporterWorkTimesState }

  TdxGanttControlXMLExporterWorkTimesState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetWorkTimes: TdxGanttControlCalendarWeekDayWorkTimes; inline;
  protected
    procedure DoExport; override;
  public
    property WorkTimes: TdxGanttControlCalendarWeekDayWorkTimes
      read GetWorkTimes;
  end;

  { TdxGanttControlXMLExporterWorkTimeState }

  TdxGanttControlXMLExporterWorkTimeState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetWorkTime: TdxGanttControlCalendarWeekDayWorkTime; inline;
  protected
    procedure DoExport; override;
  public
    property WorkTime: TdxGanttControlCalendarWeekDayWorkTime read GetWorkTime;
  end;

  { TdxGanttControlXMLExporterExceptionsState }

  TdxGanttControlXMLExporterExceptionsState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetExceptions: TdxGanttControlCalendarExceptions; inline;
  protected
    procedure DoExport; override;
  public
    property Exceptions: TdxGanttControlCalendarExceptions read GetExceptions;
  end;

  { TdxGanttControlXMLExporterExceptionState }

  TdxGanttControlXMLExporterExceptionState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetException: TdxGanttControlCalendarException; inline;
  protected
    procedure DoExport; override;
  public
    property Exception: TdxGanttControlCalendarException read GetException;
  end;

  { TdxGanttControlXMLExporterWorkWeeksState }

  TdxGanttControlXMLExporterWorkWeeksState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetWorkWeeks: TdxGanttControlCalendarWorkWeeks; inline;
  protected
    procedure DoExport; override;
  public
    property WorkWeeks: TdxGanttControlCalendarWorkWeeks read GetWorkWeeks;
  end;

  { TdxGanttControlXMLExporterWorkWeekState }

  TdxGanttControlXMLExporterWorkWeekState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetWorkWeek: TdxGanttControlCalendarWorkWeek; inline;
  protected
    procedure DoExport; override;
  public
    property WorkWeek: TdxGanttControlCalendarWorkWeek read GetWorkWeek;
  end;

  { TdxGanttControlXMLExporterTasksState }

  TdxGanttControlXMLExporterTasksState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetTasks: TdxGanttControlTasks; inline;
  protected
    procedure DoExport; override;
  public
    property Tasks: TdxGanttControlTasks read GetTasks;
  end;

  { TdxGanttControlXMLExporterTaskState }

  TdxGanttControlXMLExporterTaskState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetTask: TdxGanttControlTask; inline;
  protected
    procedure DoExport; override;
  public
    property Task: TdxGanttControlTask read GetTask;
  end;

  { TdxGanttControlXMLExporterTaskPredecessorLinksState }

  TdxGanttControlXMLExporterTaskPredecessorLinksState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetPredecessorLinks: TdxGanttControlTaskPredecessorLinks; inline;
  protected
    procedure DoExport; override;
  public
    property PredecessorLinks: TdxGanttControlTaskPredecessorLinks
      read GetPredecessorLinks;
  end;

  { TdxGanttControlXMLExporterTaskPredecessorLinkState }

  TdxGanttControlXMLExporterTaskPredecessorLinkState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetPredecessorLink: TdxGanttControlTaskPredecessorLink; inline;
  protected
    procedure DoExport; override;
  public
    property PredecessorLink: TdxGanttControlTaskPredecessorLink
      read GetPredecessorLink;
  end;

  { TdxGanttControlXMLExporterTaskBaselinesState }

  TdxGanttControlXMLExporterTaskBaselinesState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetBaselines: TdxGanttControlTaskBaselines; inline;
  protected
    procedure DoExport; override;
  public
    property Baselines: TdxGanttControlTaskBaselines read GetBaselines;
  end;

  { TdxGanttControlXMLExporterTaskBaselineState }

  TdxGanttControlXMLExporterTaskBaselineState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetBaseline: TdxGanttControlTaskBaseline; inline;
  protected
    procedure DoExport; override;
  public
    property Baseline: TdxGanttControlTaskBaseline read GetBaseline;
  end;

  { TdxGanttControlXMLExporterOutlineCodeReferencesState }

  TdxGanttControlXMLExporterOutlineCodeReferencesState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetOutlineCodes: TdxGanttControlOutlineCodeReferences; inline;
  protected
    procedure DoExport; override;
  public
    property OutlineCodes: TdxGanttControlOutlineCodeReferences
      read GetOutlineCodes;
  end;

  { TdxGanttControlXMLExporterOutlineCodeReferenceState }

  TdxGanttControlXMLExporterOutlineCodeReferenceState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetOutlineCode: TdxGanttControlOutlineCodeReference; inline;
  protected
    procedure DoExport; override;
  public
    property OutlineCode: TdxGanttControlOutlineCodeReference
      read GetOutlineCode;
  end;

  { TdxGanttControlXMLExporterResourcesState }

  TdxGanttControlXMLExporterResourcesState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetResources: TdxGanttControlResources; inline;
  protected
    procedure DoExport; override;
  public
    property Resources: TdxGanttControlResources read GetResources;
  end;

  { TdxGanttControlXMLExporterResourceState }

  TdxGanttControlXMLExporterResourceState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetResource: TdxGanttControlResource; inline;
  protected
    procedure DoExport; override;
  public
    property Resource: TdxGanttControlResource read GetResource;
  end;

  { TdxGanttControlXMLExporterResourceBaselinesState }

  TdxGanttControlXMLExporterResourceBaselinesState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetBaselines: TdxGanttControlResourceBaselines; inline;
  protected
    procedure DoExport; override;
  public
    property Baselines: TdxGanttControlResourceBaselines read GetBaselines;
  end;

  { TdxGanttControlXMLExporterResourceBaselineState }

  TdxGanttControlXMLExporterResourceBaselineState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetBaseline: TdxGanttControlResourceBaseline; inline;
  protected
    procedure DoExport; override;
  public
    property Baseline: TdxGanttControlResourceBaseline read GetBaseline;
  end;

  { TdxGanttControlXMLExporterAvailabilityPeriodsState }

  TdxGanttControlXMLExporterAvailabilityPeriodsState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetAvailabilityPeriods
      : TdxGanttControlResourceAvailabilityPeriods; inline;
  protected
    procedure DoExport; override;
  public
    property AvailabilityPeriods: TdxGanttControlResourceAvailabilityPeriods
      read GetAvailabilityPeriods;
  end;

  { TdxGanttControlXMLExporterAvailabilityPeriodState }

  TdxGanttControlXMLExporterAvailabilityPeriodState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetAvailabilityPeriod
      : TdxGanttControlResourceAvailabilityPeriod; inline;
  protected
    procedure DoExport; override;
  public
    property AvailabilityPeriod: TdxGanttControlResourceAvailabilityPeriod
      read GetAvailabilityPeriod;
  end;

  { TdxGanttControlXMLExporterResourceRatesState }

  TdxGanttControlXMLExporterResourceRatesState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetRates: TdxGanttControlResourceRates; inline;
  protected
    procedure DoExport; override;
  public
    property Rates: TdxGanttControlResourceRates read GetRates;
  end;

  { TdxGanttControlXMLExporterResourceRateState }

  TdxGanttControlXMLExporterResourceRateState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetRate: TdxGanttControlResourceRate; inline;
  protected
    procedure DoExport; override;
  public
    property Rate: TdxGanttControlResourceRate read GetRate;
  end;

  { TdxGanttControlXMLExporterAssignmentsState }

  TdxGanttControlXMLExporterAssignmentsState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetAssignments: TdxGanttControlAssignments; inline;
  protected
    procedure DoExport; override;
  public
    property Assignments: TdxGanttControlAssignments read GetAssignments;
  end;

  { TdxGanttControlXMLExporterAssignmentState }

  TdxGanttControlXMLExporterAssignmentState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetAssignment: TdxGanttControlAssignment; inline;
  protected
    procedure DoExport; override;
  public
    property Assignment: TdxGanttControlAssignment read GetAssignment;
  end;

  { TdxGanttControlXMLExporterAssignmentBaselinesState }

  TdxGanttControlXMLExporterAssignmentBaselinesState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetBaselines: TdxGanttControlAssignmentBaselines; inline;
  protected
    procedure DoExport; override;
  public
    property Baselines: TdxGanttControlAssignmentBaselines read GetBaselines;
  end;

  { TdxGanttControlXMLExporterAssignmentBaselineState }

  TdxGanttControlXMLExporterAssignmentBaselineState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetBaseline: TdxGanttControlAssignmentBaseline; inline;
  protected
    procedure DoExport; override;
  public
    property Baseline: TdxGanttControlAssignmentBaseline read GetBaseline;
  end;

  { TdxGanttControlXMLExporterEnterpriseExtendedAttributesState }

  TdxGanttControlXMLExporterEnterpriseExtendedAttributesState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetEnterpriseExtendedAttributes
      : TdxGanttControlEnterpriseExtendedAttributes; inline;
  protected
    procedure DoExport; override;
  public
    property EnterpriseExtendedAttributes
      : TdxGanttControlEnterpriseExtendedAttributes
      read GetEnterpriseExtendedAttributes;
  end;

  { TdxGanttControlXMLExporterEnterpriseExtendedAttributeState }

  TdxGanttControlXMLExporterEnterpriseExtendedAttributeState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetEnterpriseExtendedAttribute
      : TdxGanttControlEnterpriseExtendedAttribute; inline;
  protected
    procedure DoExport; override;
  public
    property EnterpriseExtendedAttribute
      : TdxGanttControlEnterpriseExtendedAttribute
      read GetEnterpriseExtendedAttribute;
  end;

  { TdxGanttControlXMLExporterEnterpriseExtendedAttributeValueState }

  TdxGanttControlXMLExporterEnterpriseExtendedAttributeValueState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetValue: TdxGanttControlEnterpriseExtendedAttributeValue; inline;
  protected
    procedure DoExport; override;
  public
    property Value: TdxGanttControlEnterpriseExtendedAttributeValue
      read GetValue;
  end;

  { TdxGanttControlXMLExporterExtendedAttributeReferencesState }

  TdxGanttControlXMLExporterExtendedAttributeReferencesState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetAttributes: TdxGanttControlExtendedAttributeReferences; inline;
  protected
    procedure DoExport; override;
  public
    property Attributes: TdxGanttControlExtendedAttributeReferences
      read GetAttributes;
  end;

  { TdxGanttControlXMLExporterExtendedAttributeState }

  TdxGanttControlXMLExporterExtendedAttributeReferenceState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetAttribute: TdxGanttControlExtendedAttributeReference; inline;
  protected
    procedure DoExport; override;
  public
    property Attribute: TdxGanttControlExtendedAttributeReference
      read GetAttribute;
  end;

  { TdxGanttControlXMLExporterTimephasedDataItemsState }

  TdxGanttControlXMLExporterTimephasedDataItemsState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetTimephasedDataItems: TdxGanttControlTimephasedDataItems; inline;
  protected
    procedure DoExport; override;
  public
    property TimephasedDataItems: TdxGanttControlTimephasedDataItems
      read GetTimephasedDataItems;
  end;

  { TdxGanttControlXMLExporterTimephasedDataState }

  TdxGanttControlXMLExporterTimephasedDataState = class
    (TdxGanttControlXMLExporterCustomElementState)
  strict private
    function GetTimephasedData: TdxGanttControlTimephasedDataItem; inline;
  protected
    procedure DoExport; override;
  public
    property TimephasedData: TdxGanttControlTimephasedDataItem
      read GetTimephasedData;
  end;

  { TdxXMLNodeHelper }

procedure TdxXMLNodeHelper.SetTextAsBoolean(const Value: Boolean);
begin
  TextAsString := IntToStr(Integer(Value));
end;

procedure TdxXMLNodeHelper.SetTextAsCost(const Value: Double);
const
  AFormat: array [Boolean] of string = ('%.0f', '%.2f');
begin
  TextAsString := ReplaceStr(Format(AFormat[Abs(Frac(Value)) > 0.0099999999],
    [Value], TdxCultureInfo.CurrentCulture.FormatSettings), ',', '.')
end;

procedure TdxXMLNodeHelper.SetTextAsDateTime(const Value: TDateTime);
begin
  TextAsString := FormatDateTime('yyyy-mm-dd''T''hh:nn:ss', Value);
end;

procedure TdxXMLNodeHelper.SetTextAsDuration(const Value: TDuration);
begin
  TextAsString := Value;
end;

procedure TdxXMLNodeHelper.SetTextAsInteger(const Value: Integer);
begin
  TextAsString := IntToStr(Value);
end;

procedure TdxXMLNodeHelper.SetTextAsNumber(const Value: Double);
begin
  TextAsString := ReplaceStr(Format('%.2f', [Value],
    TdxCultureInfo.CurrentCulture.FormatSettings), ',', '.');
end;

procedure TdxXMLNodeHelper.SetTextAsTime(const Value: TTime);
begin
  TextAsString := FormatDateTime('hh:mm:ss', Value);
end;

{ TdxGanttControlXMLExporterCustomState }

constructor TdxGanttControlXMLExporterCustomState.Create(ARootNode: TdxXMLNode);
begin
  inherited Create;
  FRootNode := ARootNode;
end;

procedure TdxGanttControlXMLExporterCustomState.BeforeExport;
begin
  // do nothing
end;

procedure TdxGanttControlXMLExporterCustomState.&Export;
begin
  BeforeExport;
  DoExport;
end;

procedure TdxGanttControlXMLExporterCustomState.ExportElement
  (AClass: TdxGanttControlXMLExporterCustomElementStateClass; ANode: TdxXMLNode;
  AElement: TdxGanttControlModelElement);
var
  AHandler: TdxGanttControlXMLExporterCustomElementState;
begin
  AHandler := AClass.Create(ANode, AElement);
  try
    AHandler.Export;
  finally
    AHandler.Free;
  end;
end;

procedure TdxGanttControlXMLExporterCustomState.DoExport;
begin
  // do nothing
end;

{ TdxGanttControlXMLExporterProjectState }

constructor TdxGanttControlXMLExporterProjectState.Create(ARootNode: TdxXMLNode;
  ADataModel: TdxGanttControlDataModel);
begin
  inherited Create(ARootNode);
  FDataModel := ADataModel;
end;

procedure TdxGanttControlXMLExporterProjectState.BeforeExport;
begin
  RootNode.Attributes.Add('xmlns', 'http://schemas.microsoft.com/project');
end;

procedure TdxGanttControlXMLExporterProjectState.DoExport;
begin
  RootNode.AddChild('SaveVersion').TextAsInteger := 14;
  RootNode.AddChild('BuildNumber').TextAsString := '16.0.12430.20184';
  if DataModel.Properties.IsValueAssigned(TdxGanttPropertiesAssignedValue.Name)
  then
    RootNode.AddChild('Name').TextAsString := DataModel.Properties.Name;
  if DataModel.Properties.IsValueAssigned(TdxGanttPropertiesAssignedValue.GUID)
  then
    RootNode.AddChild('GUID').TextAsString := DataModel.Properties.GUID;
  if DataModel.Properties.IsValueAssigned(TdxGanttPropertiesAssignedValue.Title)
  then
    RootNode.AddChild('Title').TextAsString := DataModel.Properties.Title;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.Company) then
    RootNode.AddChild('Company').TextAsString := DataModel.Properties.Company;
  RootNode.AddChild('CreationDate').TextAsDateTime :=
    DataModel.Properties.ProjectCreated;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.LastSaved) then
    RootNode.AddChild('LastSaved').TextAsDateTime :=
      DataModel.Properties.LastSaved;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.ScheduleFromStart) then
    RootNode.AddChild('ScheduleFromStart').TextAsBoolean :=
      DataModel.Properties.ScheduleFromStart;
  RootNode.AddChild('StartDate').TextAsDateTime := DataModel.RealProjectStart;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.StatusDate) then
    RootNode.AddChild('StatusDate').TextAsDateTime :=
      DataModel.Properties.StatusDate;
  RootNode.AddChild('FinishDate').TextAsDateTime := DataModel.RealProjectFinish;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.FYStartDate) then
    RootNode.AddChild('FYStartDate').TextAsInteger :=
      Integer(DataModel.Properties.FYStartDate) + 1;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.CriticalSlackLimit) then
    RootNode.AddChild('CriticalSlackLimit').TextAsInteger :=
      DataModel.Properties.CriticalSlackLimit;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.CurrencyDigits) then
    RootNode.AddChild('CurrencyDigits').TextAsInteger :=
      Integer(DataModel.Properties.CurrencyDigits);
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.CurrencySymbol) then
    RootNode.AddChild('CurrencySymbol').TextAsString :=
      DataModel.Properties.CurrencySymbol;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.CurrencyCode) then
    RootNode.AddChild('CurrencyCode').TextAsString :=
      DataModel.Properties.CurrencyCode;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.CurrencySymbolPosition) then
    RootNode.AddChild('CurrencySymbolPosition').TextAsInteger :=
      Integer(DataModel.Properties.CurrencySymbolPosition);
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.CalendarUID) then
    RootNode.AddChild('CalendarUID').TextAsInteger :=
      DataModel.Properties.CalendarUID;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.DefaultStartTime) then
    RootNode.AddChild('DefaultStartTime').TextAsTime :=
      DataModel.Properties.DefaultStartTime;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.DefaultFinishTime) then
    RootNode.AddChild('DefaultFinishTime').TextAsTime :=
      DataModel.Properties.DefaultFinishTime;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.MinutesPerDay) then
    RootNode.AddChild('MinutesPerDay').TextAsInteger :=
      DataModel.Properties.MinutesPerDay;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.MinutesPerWeek) then
    RootNode.AddChild('MinutesPerWeek').TextAsInteger :=
      DataModel.Properties.MinutesPerWeek;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.DaysPerMonth) then
    RootNode.AddChild('DaysPerMonth').TextAsInteger :=
      DataModel.Properties.DaysPerMonth;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.DefaultTaskType) then
    RootNode.AddChild('DefaultTaskType').TextAsInteger :=
      Integer(DataModel.Properties.DefaultTaskType);
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.DefaultFixedCostAccrual) then
    RootNode.AddChild('DefaultFixedCostAccrual').TextAsInteger :=
      Integer(DataModel.Properties.DefaultFixedCostAccrual);
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.DefaultStandardRate) then
    RootNode.AddChild('DefaultStandardRate').TextAsCost :=
      DataModel.Properties.DefaultStandardRate;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.DefaultOvertimeRate) then
    RootNode.AddChild('DefaultOvertimeRate').TextAsCost :=
      DataModel.Properties.DefaultOvertimeRate;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.DurationFormat) then
    RootNode.AddChild('DurationFormat').TextAsInteger :=
      Integer(DataModel.Properties.DurationFormat);
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.WorkFormat) then
    RootNode.AddChild('WorkFormat').TextAsInteger :=
      Integer(DataModel.Properties.WorkFormat);
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.EditableActualCosts) then
    RootNode.AddChild('EditableActualCosts').TextAsBoolean :=
      DataModel.Properties.EditableActualCosts;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.HonorConstraints) then
    RootNode.AddChild('HonorConstraints').TextAsBoolean :=
      DataModel.Properties.HonorConstraints;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.InsertedProjectsLikeSummary) then
    RootNode.AddChild('InsertedProjectsLikeSummary').TextAsBoolean :=
      DataModel.Properties.InsertedProjectsLikeSummary;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.MultipleCriticalPaths) then
    RootNode.AddChild('MultipleCriticalPaths').TextAsBoolean :=
      DataModel.Properties.MultipleCriticalPaths;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.NewTasksEffortDriven) then
    RootNode.AddChild('NewTasksEffortDriven').TextAsBoolean :=
      DataModel.Properties.NewTasksEffortDriven;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.NewTasksEstimated) then
    RootNode.AddChild('NewTasksEstimated').TextAsBoolean :=
      DataModel.Properties.NewTasksEstimated;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.SplitsInProgressTasks) then
    RootNode.AddChild('SplitsInProgressTasks').TextAsBoolean :=
      DataModel.Properties.SplitsInProgressTasks;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.SpreadActualCost) then
    RootNode.AddChild('SpreadActualCost').TextAsBoolean :=
      DataModel.Properties.SpreadActualCost;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.SpreadPercentComplete) then
    RootNode.AddChild('SpreadPercentComplete').TextAsBoolean :=
      DataModel.Properties.SpreadPercentComplete;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.TaskUpdatesResource) then
    RootNode.AddChild('TaskUpdatesResource').TextAsBoolean :=
      DataModel.Properties.TaskUpdatesResource;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.FiscalYearStart) then
    RootNode.AddChild('FiscalYearStart').TextAsBoolean :=
      DataModel.Properties.FiscalYearStart;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.WeekStartDay) then
    RootNode.AddChild('WeekStartDay').TextAsInteger :=
      Integer(DataModel.Properties.WeekStartDay);
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.MoveCompletedEndsBack) then
    RootNode.AddChild('MoveCompletedEndsBack').TextAsBoolean :=
      DataModel.Properties.MoveCompletedEndsBack;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.MoveRemainingStartsBack) then
    RootNode.AddChild('MoveRemainingStartsBack').TextAsBoolean :=
      DataModel.Properties.MoveRemainingStartsBack;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.MoveRemainingStartsForward) then
    RootNode.AddChild('MoveRemainingStartsForward').TextAsBoolean :=
      DataModel.Properties.MoveRemainingStartsForward;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.MoveCompletedEndsForward) then
    RootNode.AddChild('MoveCompletedEndsForward').TextAsBoolean :=
      DataModel.Properties.MoveCompletedEndsForward;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.BaselineForEarnedValue) then
    RootNode.AddChild('BaselineForEarnedValue').TextAsInteger :=
      DataModel.Properties.BaselineForEarnedValue;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.AutoAddNewResourcesAndTasks) then
    RootNode.AddChild('AutoAddNewResourcesAndTasks').TextAsBoolean :=
      DataModel.Properties.AutoAddNewResourcesAndTasks;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.MicrosoftProjectServerURL) then
    RootNode.AddChild('MicrosoftProjectServerURL').TextAsBoolean :=
      DataModel.Properties.MicrosoftProjectServerURL;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.Autolink) then
    RootNode.AddChild('Autolink').TextAsBoolean :=
      DataModel.Properties.Autolink;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.NewTaskStartDate) then
    RootNode.AddChild('NewTaskStartDate').TextAsInteger :=
      Integer(DataModel.Properties.NewTaskStartDate);
  RootNode.AddChild('NewTasksAreManual').TextAsBoolean :=
    DataModel.Properties.MarkNewTasksAsManuallyScheduled;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.DefaultTaskEVMethod) then
    RootNode.AddChild('DefaultTaskEVMethod').TextAsInteger :=
      Integer(DataModel.Properties.DefaultTaskEVMethod);
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.EarnedValueMethod) then
    RootNode.AddChild('EarnedValueMethod').TextAsInteger :=
      Integer(DataModel.Properties.EarnedValueMethod);
  RootNode.AddChild('ProjectExternallyEdited').TextAsBoolean := True;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.ExtendedCreationDate) then
    RootNode.AddChild('ExtendedCreationDate').TextAsDateTime :=
      DataModel.Properties.ExtendedCreationDate;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.ActualsInSync) then
    RootNode.AddChild('ActualsInSync').TextAsBoolean :=
      DataModel.Properties.ActualsInSync;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.RemoveFileProperties) then
    RootNode.AddChild('RemoveFileProperties').TextAsBoolean :=
      DataModel.Properties.RemoveFileProperties;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.AdminProject) then
    RootNode.AddChild('AdminProject').TextAsBoolean :=
      DataModel.Properties.AdminProject;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.UpdateManuallyScheduledTasksWhenEditingLinks)
  then
    RootNode.AddChild('UpdateManuallyScheduledTasksWhenEditingLinks')
      .TextAsBoolean :=
      DataModel.Properties.UpdateManuallyScheduledTasksWhenEditingLinks;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.KeepTaskOnNearestWorkingTimeWhenMadeAutoScheduled)
  then
    RootNode.AddChild('KeepTaskOnNearestWorkingTimeWhenMadeAutoScheduled')
      .TextAsBoolean :=
      DataModel.Properties.KeepTaskOnNearestWorkingTimeWhenMadeAutoScheduled;
  if DataModel.Properties.IsValueAssigned(TdxGanttPropertiesAssignedValue.Author)
  then
    RootNode.AddChild('Author').TextAsString := DataModel.Properties.Author;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.Category) then
    RootNode.AddChild('Category').TextAsString := DataModel.Properties.Category;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.Manager) then
    RootNode.AddChild('Manager').TextAsString := DataModel.Properties.Manager;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.Revision) then
    RootNode.AddChild('Revision').TextAsInteger :=
      DataModel.Properties.Revision;
  if DataModel.Properties.IsValueAssigned
    (TdxGanttPropertiesAssignedValue.Subject) then
    RootNode.AddChild('Subject').TextAsString := DataModel.Properties.Subject;

  ExportElement(TdxGanttControlXMLExporterOutlineCodesState,
    RootNode.AddChild('OutlineCodes'), DataModel.OutlineCodes);
  ExportElement(TdxGanttControlXMLExporterWBSMasksState,
    RootNode.AddChild('WBSMasks'), DataModel.WBSMasks);
  ExportElement(TdxGanttControlXMLExporterExtendedAttributesState,
    RootNode.AddChild('ExtendedAttributes'), DataModel.ExtendedAttributes);
  ExportElement(TdxGanttControlXMLExporterCalendarsState,
    RootNode.AddChild('Calendars'), DataModel.Calendars);
  ExportElement(TdxGanttControlXMLExporterTasksState,
    RootNode.AddChild('Tasks'), DataModel.Tasks);
  ExportElement(TdxGanttControlXMLExporterResourcesState,
    RootNode.AddChild('Resources'), DataModel.Resources);
  ExportElement(TdxGanttControlXMLExporterAssignmentsState,
    RootNode.AddChild('Assignments'), DataModel.Assignments);
end;

{ TdxGanttControlXMLExporterCustomElementState }

constructor TdxGanttControlXMLExporterCustomElementState.Create
  (ARootNode: TdxXMLNode; AElement: TdxGanttControlModelElement);
begin
  inherited Create(ARootNode);
  FElement := AElement;
end;

{ TdxGanttControlXMLExporterOutlineCodesState }

function TdxGanttControlXMLExporterOutlineCodesState.GetOutlineCodes
  : TdxGanttControlOutlineCodes;
begin
  Result := TdxGanttControlOutlineCodes(inherited Element);
end;

procedure TdxGanttControlXMLExporterOutlineCodesState.DoExport;
var
  I: Integer;
begin
  for I := 0 to OutlineCodes.Count - 1 do
    ExportElement(TdxGanttControlXMLExporterOutlineCodeState,
      RootNode.AddChild('OutlineCode'), OutlineCodes[I]);
end;

{ TdxGanttControlXMLExporterOutlineCodeState }

function TdxGanttControlXMLExporterOutlineCodeState.GetOutlineCode
  : TdxGanttControlOutlineCode;
begin
  Result := TdxGanttControlOutlineCode(inherited Element);
end;

procedure TdxGanttControlXMLExporterOutlineCodeState.DoExport;
begin
  RootNode.AddChild('Guid').TextAsString := OutlineCode.GUID;
  if OutlineCode.IsValueAssigned(TdxGanttOutlineCodeAssignedValue.FieldID) then
    RootNode.AddChild('FieldID').TextAsInteger := OutlineCode.FieldID;
  if OutlineCode.IsValueAssigned(TdxGanttOutlineCodeAssignedValue.FieldName)
  then
    RootNode.AddChild('FieldName').TextAsString := OutlineCode.FieldName;
  if OutlineCode.IsValueAssigned(TdxGanttOutlineCodeAssignedValue.Alias) then
    RootNode.AddChild('Alias').TextAsString := OutlineCode.Alias;
  if OutlineCode.IsValueAssigned(TdxGanttOutlineCodeAssignedValue.PhoneticAlias)
  then
    RootNode.AddChild('PhoneticAlias').TextAsString :=
      OutlineCode.PhoneticAlias;
  if OutlineCode.IsValueAssigned(TdxGanttOutlineCodeAssignedValue.Enterprise)
  then
    RootNode.AddChild('Enterprise').TextAsBoolean := OutlineCode.Enterprise;
  if OutlineCode.IsValueAssigned
    (TdxGanttOutlineCodeAssignedValue.EnterpriseOutlineCodeAlias) then
    RootNode.AddChild('EnterpriseOutlineCodeAlias').TextAsInteger :=
      OutlineCode.EnterpriseOutlineCodeAlias;
  if OutlineCode.IsValueAssigned(TdxGanttOutlineCodeAssignedValue.ShowIndent)
  then
    RootNode.AddChild('ShowIndent').TextAsBoolean := OutlineCode.ShowIndent;
  if OutlineCode.IsValueAssigned
    (TdxGanttOutlineCodeAssignedValue.ResourceSubstitutionEnabled) then
    RootNode.AddChild('ResourceSubstitutionEnabled').TextAsBoolean :=
      OutlineCode.ResourceSubstitutionEnabled;
  if OutlineCode.IsValueAssigned(TdxGanttOutlineCodeAssignedValue.LeafOnly) then
    RootNode.AddChild('LeafOnly').TextAsBoolean := OutlineCode.LeafOnly;
  if OutlineCode.IsValueAssigned
    (TdxGanttOutlineCodeAssignedValue.AllLevelsRequired) then
    RootNode.AddChild('AllLevelsRequired').TextAsBoolean :=
      OutlineCode.AllLevelsRequired;
  if OutlineCode.IsValueAssigned
    (TdxGanttOutlineCodeAssignedValue.OnlyTableValuesAllowed) then
    RootNode.AddChild('OnlyTableValuesAllowed').TextAsBoolean :=
      OutlineCode.OnlyTableValuesAllowed;

  if OutlineCode.Masks.Count > 0 then
    ExportElement(TdxGanttControlXMLExporterOutlineCodeMasksState,
      RootNode.AddChild('Masks'), OutlineCode.Masks);
  if OutlineCode.Values.Count > 0 then
    ExportElement(TdxGanttControlXMLExporterOutlineCodeValuesState,
      RootNode.AddChild('Values'), OutlineCode.Values);
end;

{ TdxGanttControlXMLExporterOutlineCodeMasksState }

function TdxGanttControlXMLExporterOutlineCodeMasksState.GetOutlineCodeMasks
  : TdxGanttControlOutlineCodeMasks;
begin
  Result := TdxGanttControlOutlineCodeMasks(inherited Element);
end;

procedure TdxGanttControlXMLExporterOutlineCodeMasksState.DoExport;
var
  I: Integer;
begin
  for I := 0 to OutlineCodeMasks.Count - 1 do
    ExportElement(TdxGanttControlXMLExporterOutlineCodeMaskState,
      RootNode.AddChild('Mask'), OutlineCodeMasks[I]);
end;

{ TdxGanttControlXMLExporterOutlineCodeMaskState }

function TdxGanttControlXMLExporterOutlineCodeMaskState.GetOutlineCodeMask
  : TdxGanttControlOutlineCodeMask;
begin
  Result := TdxGanttControlOutlineCodeMask(inherited Element);
end;

procedure TdxGanttControlXMLExporterOutlineCodeMaskState.DoExport;
begin
  if OutlineCodeMask.IsValueAssigned(TdxGanttOutlineCodeMaskAssignedValue.Level)
  then
    RootNode.AddChild('Level').TextAsInteger := OutlineCodeMask.Level;
  if OutlineCodeMask.IsValueAssigned(TdxGanttOutlineCodeMaskAssignedValue.&Type)
  then
    RootNode.AddChild('Type').TextAsInteger := Integer(OutlineCodeMask.&Type);
  if OutlineCodeMask.IsValueAssigned(TdxGanttOutlineCodeMaskAssignedValue.Length)
  then
    RootNode.AddChild('Length').TextAsInteger := OutlineCodeMask.Length;
  if OutlineCodeMask.IsValueAssigned
    (TdxGanttOutlineCodeMaskAssignedValue.Separator) then
    RootNode.AddChild('Separator').TextAsString := OutlineCodeMask.Separator;
end;

{ TdxGanttControlXMLExporterOutlineCodeValuesState }

function TdxGanttControlXMLExporterOutlineCodeValuesState.GetOutlineCodeValues
  : TdxGanttControlOutlineCodeValues;
begin
  Result := TdxGanttControlOutlineCodeValues(inherited Element);
end;

procedure TdxGanttControlXMLExporterOutlineCodeValuesState.DoExport;
var
  I: Integer;
begin
  for I := 0 to OutlineCodeValues.Count - 1 do
    ExportElement(TdxGanttControlXMLExporterOutlineCodeValueState,
      RootNode.AddChild('Value'), OutlineCodeValues[I]);
end;

{ TdxGanttControlXMLExporterOutlineCodeValueState }

function TdxGanttControlXMLExporterOutlineCodeValueState.GetOutlineCodeValue
  : TdxGanttControlOutlineCodeValue;
begin
  Result := TdxGanttControlOutlineCodeValue(inherited Element);
end;

procedure TdxGanttControlXMLExporterOutlineCodeValueState.DoExport;
begin
  if OutlineCodeValue.IsValueAssigned
    (TdxGanttOutlineCodeValueAssignedValue.ValueID) then
    RootNode.AddChild('ValueID').TextAsInteger := OutlineCodeValue.ValueID;
  RootNode.AddChild('FieldGUID').TextAsString := OutlineCodeValue.FieldGUID;
  if OutlineCodeValue.IsValueAssigned
    (TdxGanttOutlineCodeValueAssignedValue.ParentValueID) then
    RootNode.AddChild('ParentValueID').TextAsInteger :=
      OutlineCodeValue.ParentValueID;
  RootNode.AddChild('Type').TextAsInteger := Integer(OutlineCodeValue.&Type);
  if OutlineCodeValue.IsValueAssigned
    (TdxGanttOutlineCodeValueAssignedValue.Collapsed) then
    RootNode.AddChild('IsCollapsed').TextAsBoolean :=
      OutlineCodeValue.Collapsed;
  if OutlineCodeValue.IsValueAssigned
    (TdxGanttOutlineCodeValueAssignedValue.Value) then
    RootNode.AddChild('Value').TextAsString := OutlineCodeValue.Value;
  if OutlineCodeValue.IsValueAssigned
    (TdxGanttOutlineCodeValueAssignedValue.Description) then
    RootNode.AddChild('Description').TextAsString :=
      OutlineCodeValue.Description;
end;

{ TdxGanttControlXMLExporterWBSMasksState }

function TdxGanttControlXMLExporterWBSMasksState.GetWBSMasks
  : TdxGanttControlWBSMasks;
begin
  Result := TdxGanttControlWBSMasks(inherited Element);
end;

procedure TdxGanttControlXMLExporterWBSMasksState.DoExport;
var
  I: Integer;
  ANode: TdxXMLNode;
begin
  if WBSMasks.IsValueAssigned(TdxGanttWBSMasksAssignedValue.VerifyUniqueCodes)
  then
    RootNode.AddChild('VerifyUniqueCodes').TextAsBoolean :=
      WBSMasks.VerifyUniqueCodes;
  if WBSMasks.IsValueAssigned(TdxGanttWBSMasksAssignedValue.GenerateCodes) then
    RootNode.AddChild('GenerateCodes').TextAsBoolean := WBSMasks.GenerateCodes;
  if WBSMasks.IsValueAssigned(TdxGanttWBSMasksAssignedValue.Prefix) then
    RootNode.AddChild('Prefix').TextAsString := WBSMasks.Prefix;

  if WBSMasks.Count > 0 then
  begin
    ANode := RootNode.AddChild('Masks');
    for I := 0 to WBSMasks.Count - 1 do
      ExportElement(TdxGanttControlXMLExporterWBSMaskState,
        ANode.AddChild('WBSMask'), WBSMasks[I]);
  end;
end;

{ TdxGanttControlXMLExporterWBSMaskState }

function TdxGanttControlXMLExporterWBSMaskState.GetWBSMask
  : TdxGanttControlWBSMask;
begin
  Result := TdxGanttControlWBSMask(inherited Element);
end;

procedure TdxGanttControlXMLExporterWBSMaskState.DoExport;
begin
  RootNode.AddChild('Level').TextAsInteger := WBSMask.Level;
  RootNode.AddChild('Type').TextAsInteger := Integer(WBSMask.&Type);
  RootNode.AddChild('Length').TextAsInteger := WBSMask.Length;
  RootNode.AddChild('Separator').TextAsString := WBSMask.Separator;
end;

{ TdxGanttControlXMLExporterExtendedAttributesState }

function TdxGanttControlXMLExporterExtendedAttributesState.GetExtendedAttributes
  : TdxGanttControlExtendedAttributes;
begin
  Result := TdxGanttControlExtendedAttributes(inherited Element);
end;

procedure TdxGanttControlXMLExporterExtendedAttributesState.DoExport;
var
  I: Integer;
begin
  for I := 0 to ExtendedAttributes.Count - 1 do
    ExportElement(TdxGanttControlXMLExporterExtendedAttributeState,
      RootNode.AddChild('ExtendedAttribute'), ExtendedAttributes[I]);
end;

{ TdxGanttControlXMLExporterExtendedAttributeState }

function TdxGanttControlXMLExporterExtendedAttributeState.GetExtendedAttribute
  : TdxGanttControlExtendedAttribute;
begin
  Result := TdxGanttControlExtendedAttribute(inherited Element);
end;

procedure TdxGanttControlXMLExporterExtendedAttributeState.DoExport;
begin
  if ExtendedAttribute.IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.FieldID) then
    RootNode.AddChild('FieldID').TextAsInteger := ExtendedAttribute.FieldID;
  if ExtendedAttribute.IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.FieldName) then
    RootNode.AddChild('FieldName').TextAsString := ExtendedAttribute.FieldName;
  if ExtendedAttribute.IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.CFType) then
    RootNode.AddChild('CFType').TextAsInteger :=
      Integer(ExtendedAttribute.CFType);
  if ExtendedAttribute.IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.Ltuid) then
    RootNode.AddChild('Ltuid').TextAsString := ExtendedAttribute.Ltuid;
  if ExtendedAttribute.IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.GUID) then
    RootNode.AddChild('Guid').TextAsString := ExtendedAttribute.GUID;
  if ExtendedAttribute.IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.ElemType) then
    RootNode.AddChild('ElemType').TextAsInteger :=
      Integer(ExtendedAttribute.ElemType);
  if ExtendedAttribute.IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.MaxMultiValues) then
    RootNode.AddChild('MaxMultiValues').TextAsInteger :=
      ExtendedAttribute.MaxMultiValues;
  if ExtendedAttribute.IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.UserDef) then
    RootNode.AddChild('UserDef').TextAsBoolean := ExtendedAttribute.UserDef;
  if ExtendedAttribute.IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.Alias) then
    RootNode.AddChild('Alias').TextAsString := ExtendedAttribute.Alias;
  if ExtendedAttribute.IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.SecondaryPID) then
    RootNode.AddChild('SecondaryPID').TextAsString :=
      ExtendedAttribute.SecondaryPID;
  if ExtendedAttribute.IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.SecondaryGUID) then
    RootNode.AddChild('SecondaryGuid').TextAsString :=
      ExtendedAttribute.SecondaryGUID;
  if ExtendedAttribute.IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.AutoRollDown) then
    RootNode.AddChild('AutoRollDown').TextAsBoolean :=
      ExtendedAttribute.AutoRollDown;
  if ExtendedAttribute.IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.DefaultGUID) then
    RootNode.AddChild('DefaultGUID').TextAsString :=
      ExtendedAttribute.DefaultGUID;
  if ExtendedAttribute.IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.PhoneticAlias) then
    RootNode.AddChild('PhoneticAlias').TextAsString :=
      ExtendedAttribute.PhoneticAlias;
  if ExtendedAttribute.IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.RollupType) then
    RootNode.AddChild('RollupType').TextAsInteger :=
      Integer(ExtendedAttribute.RollupType);
  if ExtendedAttribute.IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.CalculationType) then
    RootNode.AddChild('CalculationType').TextAsInteger :=
      Integer(ExtendedAttribute.CalculationType);
  if ExtendedAttribute.IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.Formula) then
    RootNode.AddChild('Formula').TextAsString := ExtendedAttribute.Formula;
  if ExtendedAttribute.IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.RestrictValues) then
    RootNode.AddChild('RestrictValues').TextAsBoolean :=
      ExtendedAttribute.RestrictValues;
  if ExtendedAttribute.IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.ValuelistSortOrder) then
    RootNode.AddChild('ValuelistSortOrder').TextAsInteger :=
      Integer(ExtendedAttribute.ValuelistSortOrder);
  if ExtendedAttribute.IsValueAssigned
    (TdxGanttExtendedAttributeAssignedValue.AppendNewValues) then
    RootNode.AddChild('AppendNewValues').TextAsBoolean :=
      ExtendedAttribute.AppendNewValues;
  if ExtendedAttribute.IsValueAssigned(TdxGanttExtendedAttributeAssignedValue.
    Default) then
    RootNode.AddChild('Default').TextAsString := ExtendedAttribute.Default;

  if ExtendedAttribute.ValueList.Count > 0 then
    ExportElement(TdxGanttControlXMLExporterExtendedAttributeValueListState,
      RootNode.AddChild('ValueList'), ExtendedAttribute.ValueList);
end;

{ TdxGanttControlXMLExporterExtendedAttributeValueListState }

function TdxGanttControlXMLExporterExtendedAttributeValueListState.GetValues
  : TdxGanttControlExtendedAttributeValues;
begin
  Result := TdxGanttControlExtendedAttributeValues(inherited Element);
end;

procedure TdxGanttControlXMLExporterExtendedAttributeValueListState.DoExport;
var
  I: Integer;
begin
  for I := 0 to Values.Count - 1 do
    ExportElement(TdxGanttControlXMLExporterExtendedAttributeValueState,
      RootNode.AddChild('Calendar'), Values[I]);
end;

{ TdxGanttControlXMLExporterExtendedAttributeValueState }

function TdxGanttControlXMLExporterExtendedAttributeValueState.GetValue
  : TdxGanttControlExtendedAttributeValue;
begin
  Result := TdxGanttControlExtendedAttributeValue(inherited Element);
end;

procedure TdxGanttControlXMLExporterExtendedAttributeValueState.DoExport;
begin
  RootNode.AddChild('ID').TextAsInteger := Value.ID;
  if Value.IsValueAssigned(TdxGanttExtendedAttributeValueAssignedValue.Value)
  then
    RootNode.AddChild('Value').TextAsString := Value.Value;
  if Value.IsValueAssigned(TdxGanttExtendedAttributeValueAssignedValue.
    Description) then
    RootNode.AddChild('Description').TextAsString := Value.Description;
  if Value.IsValueAssigned(TdxGanttExtendedAttributeValueAssignedValue.Phonetic)
  then
    RootNode.AddChild('Phonetic').TextAsString := Value.Phonetic;
end;

{ TdxGanttControlXMLExporterCalendarsState }

function TdxGanttControlXMLExporterCalendarsState.GetCalendars
  : TdxGanttControlCalendars;
begin
  Result := TdxGanttControlCalendars(inherited Element);
end;

procedure TdxGanttControlXMLExporterCalendarsState.DoExport;
var
  I: Integer;
begin
  for I := 0 to Calendars.Count - 1 do
    ExportElement(TdxGanttControlXMLExporterCalendarState,
      RootNode.AddChild('Calendar'), Calendars[I]);
end;

{ TdxGanttControlXMLExporterCalendarState }

function TdxGanttControlXMLExporterCalendarState.GetCalendar
  : TdxGanttControlCalendar;
begin
  Result := TdxGanttControlCalendar(inherited Element);
end;

procedure TdxGanttControlXMLExporterCalendarState.DoExport;
begin
  RootNode.AddChild('UID').TextAsInteger := Calendar.UID;
  RootNode.AddChild('GUID').TextAsString := TdxGanttControlCalendarAccess
    (Calendar).GUID;
  if Calendar.IsValueAssigned(TdxGanttCalendarAssignedValue.Name) then
    RootNode.AddChild('Name').TextAsString := Calendar.Name;
  RootNode.AddChild('IsBaseCalendar').TextAsBoolean := Calendar.IsBaseCalendar;
  if Calendar.IsValueAssigned(TdxGanttCalendarAssignedValue.BaselineCalendar)
  then
    RootNode.AddChild('IsBaselineCalendar').TextAsBoolean :=
      Calendar.BaselineCalendar;
  RootNode.AddChild('BaseCalendarUID').TextAsInteger :=
    Calendar.BaseCalendarUID;

  if Calendar.WeekDays.Count > 0 then
    ExportElement(TdxGanttControlXMLExporterWeekDaysState,
      RootNode.AddChild('WeekDays'), Calendar.WeekDays);
  if Calendar.Exceptions.Count > 0 then
    ExportElement(TdxGanttControlXMLExporterExceptionsState,
      RootNode.AddChild('Exceptions'), Calendar.Exceptions);
  if Calendar.WorkWeeks.Count > 0 then
    ExportElement(TdxGanttControlXMLExporterWorkWeeksState,
      RootNode.AddChild('WorkWeeks'), Calendar.WorkWeeks);
end;

{ TdxGanttControlXMLExporterWeekDaysState }

function TdxGanttControlXMLExporterWeekDaysState.GetWeekDays
  : TdxGanttControlCalendarWeekDays;
begin
  Result := TdxGanttControlCalendarWeekDays(inherited Element);
end;

procedure TdxGanttControlXMLExporterWeekDaysState.DoExport;
var
  I: Integer;
begin
  for I := 0 to WeekDays.Count - 1 do
    ExportElement(TdxGanttControlXMLExporterWeekDayState,
      RootNode.AddChild('WeekDay'), WeekDays[I]);
end;

{ TdxGanttControlXMLExporterWeekDayState }

function TdxGanttControlXMLExporterWeekDayState.GetWeekDay
  : TdxGanttControlCalendarWeekDay;
begin
  Result := TdxGanttControlCalendarWeekDay(inherited Element);
end;

procedure TdxGanttControlXMLExporterWeekDayState.DoExport;
begin
  if WeekDay.IsValueAssigned(TdxGanttCalendarWeekDayAssignedValue.DayType) then
    RootNode.AddChild('DayType').TextAsInteger := Integer(WeekDay.DayType);
  if WeekDay.IsValueAssigned(TdxGanttCalendarWeekDayAssignedValue.Workday) then
    RootNode.AddChild('DayWorking').TextAsBoolean := WeekDay.Workday;

  if WeekDay.TimePeriod <> nil then
    ExportElement(TdxGanttControlXMLExporterTimePeriodState,
      RootNode.AddChild('TimePeriod'), WeekDay.TimePeriod);
  if WeekDay.WorkTimes.Count > 0 then
    ExportElement(TdxGanttControlXMLExporterWorkTimesState,
      RootNode.AddChild('WorkingTimes'), WeekDay.WorkTimes);
end;

{ TdxGanttControlXMLExporterTimePeriodState }

function TdxGanttControlXMLExporterTimePeriodState.GetTimePeriod
  : TdxGanttControlTimePeriod;
begin
  Result := TdxGanttControlTimePeriod(inherited Element);
end;

procedure TdxGanttControlXMLExporterTimePeriodState.DoExport;
begin
  RootNode.AddChild('FromDate').TextAsDateTime := TimePeriod.FromDate;
  RootNode.AddChild('ToDate').TextAsDateTime := TimePeriod.ToDate;
end;

{ TdxGanttControlXMLExporterWorkTimesState }

function TdxGanttControlXMLExporterWorkTimesState.GetWorkTimes
  : TdxGanttControlCalendarWeekDayWorkTimes;
begin
  Result := TdxGanttControlCalendarWeekDayWorkTimes(inherited Element);
end;

procedure TdxGanttControlXMLExporterWorkTimesState.DoExport;
var
  I: Integer;
begin
  TdxGanttControlWeekDayWorkTimesAccess(WorkTimes).DoSort;
  for I := 0 to WorkTimes.Count - 1 do
    ExportElement(TdxGanttControlXMLExporterWorkTimeState,
      RootNode.AddChild('WorkingTime'), WorkTimes[I]);
end;

{ TdxGanttControlXMLExporterWorkTimeState }

function TdxGanttControlXMLExporterWorkTimeState.GetWorkTime
  : TdxGanttControlCalendarWeekDayWorkTime;
begin
  Result := TdxGanttControlCalendarWeekDayWorkTime(inherited Element);
end;

procedure TdxGanttControlXMLExporterWorkTimeState.DoExport;
begin
  RootNode.AddChild('FromTime').TextAsTime := WorkTime.FromTime;
  if Abs(WorkTime.ToTime - dxEndOfDay) <= OneMillisecond then
    RootNode.AddChild('ToTime').TextAsTime := 1
  else
    RootNode.AddChild('ToTime').TextAsTime := WorkTime.ToTime;
end;

{ TdxGanttControlXMLExporterExceptionsState }

function TdxGanttControlXMLExporterExceptionsState.GetExceptions
  : TdxGanttControlCalendarExceptions;
begin
  Result := TdxGanttControlCalendarExceptions(inherited Element);
end;

procedure TdxGanttControlXMLExporterExceptionsState.DoExport;
var
  I: Integer;
begin
  for I := 0 to Exceptions.Count - 1 do
    ExportElement(TdxGanttControlXMLExporterExceptionState,
      RootNode.AddChild('Exception'), Exceptions[I]);
end;

{ TdxGanttControlXMLExporterExceptionState }

function TdxGanttControlXMLExporterExceptionState.GetException
  : TdxGanttControlCalendarException;
begin
  Result := TdxGanttControlCalendarException(inherited Element);
end;

procedure TdxGanttControlXMLExporterExceptionState.DoExport;
begin
  if Exception.IsValueAssigned
    (TdxGanttCalendarExceptionAssignedValue.EnteredByOccurrences) then
    RootNode.AddChild('EnteredByOccurrences').TextAsBoolean :=
      Exception.EnteredByOccurrences;
  if Exception.TimePeriod <> nil then
    ExportElement(TdxGanttControlXMLExporterTimePeriodState,
      RootNode.AddChild('TimePeriod'), Exception.TimePeriod);
  if Exception.IsValueAssigned
    (TdxGanttCalendarExceptionAssignedValue.Occurrences) then
    RootNode.AddChild('Occurrences').TextAsInteger := Exception.Occurrences;
  if Exception.IsValueAssigned(TdxGanttCalendarExceptionAssignedValue.Name) then
    RootNode.AddChild('Name').TextAsString := Exception.Name;
  if Exception.IsValueAssigned(TdxGanttCalendarExceptionAssignedValue.&Type)
  then
    RootNode.AddChild('Type').TextAsInteger := Integer(Exception.&Type);
  if Exception.IsValueAssigned(TdxGanttCalendarExceptionAssignedValue.Period)
  then
    RootNode.AddChild('Period').TextAsInteger := Exception.Period;
  if Exception.IsValueAssigned(TdxGanttCalendarExceptionAssignedValue.Month)
  then
    RootNode.AddChild('Month').TextAsInteger := Integer(Exception.Month);
  if Exception.IsValueAssigned(TdxGanttCalendarExceptionAssignedValue.MonthDay)
  then
    RootNode.AddChild('MonthDay').TextAsInteger := Exception.MonthDay;
  if Exception.IsValueAssigned(TdxGanttCalendarExceptionAssignedValue.MonthItem)
  then
    RootNode.AddChild('MonthItem').TextAsInteger :=
      Integer(Exception.MonthItem);
  if Exception.IsValueAssigned
    (TdxGanttCalendarExceptionAssignedValue.MonthPosition) then
    RootNode.AddChild('MonthPosition').TextAsInteger :=
      Integer(Exception.MonthPosition);
  if Exception.IsValueAssigned(TdxGanttCalendarExceptionAssignedValue.DaysOfWeek)
  then
    RootNode.AddChild('DaysOfWeek').TextAsInteger :=
      dxGanttControlExceptionDaysOfWeekToInteger(Exception.DaysOfWeek);
  if Exception.IsValueAssigned(TdxGanttCalendarExceptionAssignedValue.Workday)
  then
    RootNode.AddChild('DayWorking').TextAsBoolean := Exception.Workday;
  if Exception.WorkTimes.Count > 0 then
    ExportElement(TdxGanttControlXMLExporterWorkTimesState,
      RootNode.AddChild('WorkingTimes'), Exception.WorkTimes);
end;

{ TdxGanttControlXMLExporterWorkWeeksState }

function TdxGanttControlXMLExporterWorkWeeksState.GetWorkWeeks
  : TdxGanttControlCalendarWorkWeeks;
begin
  Result := TdxGanttControlCalendarWorkWeeks(inherited Element);
end;

procedure TdxGanttControlXMLExporterWorkWeeksState.DoExport;
var
  I: Integer;
begin
  for I := 0 to WorkWeeks.Count - 1 do
    ExportElement(TdxGanttControlXMLExporterWorkWeekState,
      RootNode.AddChild('WorkWeek'), WorkWeeks[I]);
end;

{ TdxGanttControlXMLExporterWorkWeekState }

function TdxGanttControlXMLExporterWorkWeekState.GetWorkWeek
  : TdxGanttControlCalendarWorkWeek;
begin
  Result := TdxGanttControlCalendarWorkWeek(inherited Element);
end;

procedure TdxGanttControlXMLExporterWorkWeekState.DoExport;
begin
  if WorkWeek.TimePeriod <> nil then
    ExportElement(TdxGanttControlXMLExporterTimePeriodState,
      RootNode.AddChild('TimePeriod'), WorkWeek.TimePeriod);
  if WorkWeek.IsValueAssigned(TdxGanttCalendarWorkWeekAssignedValue.Name) then
    RootNode.AddChild('Name').TextAsString := WorkWeek.Name;
  if WorkWeek.WeekDays <> nil then
    ExportElement(TdxGanttControlXMLExporterWeekDaysState,
      RootNode.AddChild('WeekDays'), WorkWeek.WeekDays);
end;

{ TdxGanttControlXMLExporterTasksState }

function TdxGanttControlXMLExporterTasksState.GetTasks: TdxGanttControlTasks;
begin
  Result := TdxGanttControlTasks(inherited Element);
end;

procedure TdxGanttControlXMLExporterTasksState.DoExport;
var
  I: Integer;
begin
  for I := 0 to Tasks.Count - 1 do
    ExportElement(TdxGanttControlXMLExporterTaskState,
      RootNode.AddChild('Task'), Tasks[I]);
end;

{ TdxGanttControlXMLExporterTaskState }

function TdxGanttControlXMLExporterTaskState.GetTask: TdxGanttControlTask;
begin
  Result := TdxGanttControlTask(inherited Element);
end;

procedure TdxGanttControlXMLExporterTaskState.DoExport;
var
  ADuration: TdxGanttControlDuration;
  ASeconds: Int64;
begin
  RootNode.AddChild('UID').TextAsInteger := Task.UID;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.GUID) then
    RootNode.AddChild('GUID').TextAsString := Task.GUID;
  RootNode.AddChild('ID').TextAsInteger := Task.ID;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Name) then
    RootNode.AddChild('Name').TextAsString := Task.Name;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Active) then
    RootNode.AddChild('Active').TextAsBoolean := Task.Active;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Manual) then
    RootNode.AddChild('Manual').TextAsBoolean := Task.Manual;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.&Type) then
    RootNode.AddChild('Type').TextAsInteger := Integer(Task.&Type);
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Blank) then
    RootNode.AddChild('IsNull').TextAsBoolean := Task.Blank;
  RootNode.AddChild('CreateDate').TextAsDateTime := Task.Created;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.WBS) then
    RootNode.AddChild('WBS').TextAsString := Task.WBS;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.WBSLevel) then
    RootNode.AddChild('WBSLevel').TextAsString := Task.WBSLevel;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.OutlineNumber) then
    RootNode.AddChild('OutlineNumber').TextAsString := Task.OutlineNumber;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.OutlineLevel) then
    RootNode.AddChild('OutlineLevel').TextAsInteger := Task.OutlineLevel;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Priority) then
    RootNode.AddChild('Priority').TextAsInteger := Task.Priority;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Start) then
    RootNode.AddChild('Start').TextAsDateTime := Task.Start;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Finish) then
    RootNode.AddChild('Finish').TextAsDateTime := Task.Finish;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Duration) then
  begin
    RootNode.AddChild('Duration').TextAsDuration := Task.Duration;
    ADuration := TdxGanttControlDuration.Create(Task.Duration);
    if Task.IsValueAssigned(TdxGanttTaskAssignedValue.PercentComplete) then
      ASeconds := MulDiv(ADuration.ToSeconds, Task.PercentComplete, 100)
    else
      ASeconds := 0;
    if not Task.IsValueAssigned(TdxGanttTaskAssignedValue.ActualDuration) then
    begin
      if ASeconds > 0 then
        RootNode.AddChild('ActualDuration').TextAsDuration :=
          Format('PT%dH%dM%dS', [ASeconds div 3600, (ASeconds mod 3600) div 60,
          ASeconds mod 60]);
    end;
    if not Task.IsValueAssigned(TdxGanttTaskAssignedValue.RemainingDuration)
    then
    begin
      ASeconds := ADuration.ToSeconds - ASeconds;
      RootNode.AddChild('RemainingDuration').TextAsDuration :=
        Format('PT%dH%dM%dS', [ASeconds div 3600, (ASeconds mod 3600) div 60,
        ASeconds mod 60]);
    end;
  end;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.ManualStart) then
    RootNode.AddChild('ManualStart').TextAsDateTime := Task.ManualStart;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.ManualFinish) then
    RootNode.AddChild('ManualFinish').TextAsDateTime := Task.ManualFinish;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.ManualDuration) then
    RootNode.AddChild('ManualDuration').TextAsDuration := Task.ManualDuration;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.DurationFormat) then
    RootNode.AddChild('DurationFormat').TextAsInteger :=
      Integer(Task.DurationFormat);
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.FreeformDurationFormat) then
    RootNode.AddChild('FreeformDurationFormat').TextAsInteger :=
      Integer(Task.FreeformDurationFormat);
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Work) then
    RootNode.AddChild('Work').TextAsString := Task.Work;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Stop) then
    RootNode.AddChild('Stop').TextAsDateTime := Task.Stop;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Resume) then
    RootNode.AddChild('Resume').TextAsDateTime := Task.Resume;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.ResumeValid) then
    RootNode.AddChild('ResumeValid').TextAsBoolean := Task.ResumeValid;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.EffortDriven) then
    RootNode.AddChild('EffortDriven').TextAsBoolean := Task.EffortDriven;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Recurring) then
    RootNode.AddChild('Recurring').TextAsBoolean := Task.Recurring;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.OverAllocated) then
    RootNode.AddChild('OverAllocated').TextAsBoolean := Task.OverAllocated;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Estimated) then
    RootNode.AddChild('Estimated').TextAsBoolean := Task.Estimated;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Milestone) then
    RootNode.AddChild('Milestone').TextAsBoolean := Task.Milestone;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Summary) then
    RootNode.AddChild('Summary').TextAsBoolean := Task.Summary;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.DisplayAsSummary) then
    RootNode.AddChild('DisplayAsSummary').TextAsBoolean :=
      Task.DisplayAsSummary;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.DisplayOnTimeline) then
    RootNode.AddChild('DisplayOnTimeline').TextAsBoolean :=
      Task.DisplayOnTimeline;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Critical) then
    RootNode.AddChild('Critical').TextAsBoolean := Task.Critical;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Subproject) then
    RootNode.AddChild('IsSubproject').TextAsBoolean := Task.Subproject;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.SubprojectReadOnly) then
    RootNode.AddChild('IsSubprojectReadOnly').TextAsBoolean :=
      Task.SubprojectReadOnly;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.ExternalTask) then
    RootNode.AddChild('ExternalTask').TextAsBoolean := Task.ExternalTask;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.EarlyStart) then
    RootNode.AddChild('EarlyStart').TextAsDateTime := Task.EarlyStart;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.EarlyFinish) then
    RootNode.AddChild('EarlyFinish').TextAsDateTime := Task.EarlyFinish;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.LateStart) then
    RootNode.AddChild('LateStart').TextAsDateTime := Task.LateStart;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.LateFinish) then
    RootNode.AddChild('LateFinish').TextAsDateTime := Task.LateFinish;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.StartVariance) then
    RootNode.AddChild('StartVariance').TextAsInteger := Task.StartVariance;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.FinishVariance) then
    RootNode.AddChild('FinishVariance').TextAsInteger := Task.FinishVariance;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.WorkVariance) then
    RootNode.AddChild('WorkVariance').TextAsNumber := Task.WorkVariance;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.FreeSlack) then
    RootNode.AddChild('FreeSlack').TextAsInteger := Task.FreeSlack;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.TotalSlack) then
    RootNode.AddChild('TotalSlack').TextAsInteger := Task.TotalSlack;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.StartSlack) then
    RootNode.AddChild('StartSlack').TextAsInteger := Task.StartSlack;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.FinishSlack) then
    RootNode.AddChild('FinishSlack').TextAsInteger := Task.FinishSlack;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.FixedCost) then
    RootNode.AddChild('FixedCost').TextAsCost := Task.FixedCost;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.FixedCostAccrual) then
    RootNode.AddChild('FixedCostAccrual').TextAsInteger :=
      Task.FixedCostAccrual;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.PercentComplete) then
    RootNode.AddChild('PercentComplete').TextAsInteger := Task.PercentComplete;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.PercentWorkComplete) then
    RootNode.AddChild('PercentWorkComplete').TextAsInteger :=
      Task.PercentWorkComplete;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Cost) then
    RootNode.AddChild('Cost').TextAsCost := Task.Cost;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.OvertimeCost) then
    RootNode.AddChild('OvertimeCost').TextAsCost := Task.OvertimeCost;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.OvertimeWork) then
    RootNode.AddChild('OvertimeWork').TextAsString := Task.OvertimeWork;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.ActualStart) then
    RootNode.AddChild('ActualStart').TextAsDateTime := Task.ActualStart;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.ActualFinish) then
    RootNode.AddChild('ActualFinish').TextAsDateTime := Task.ActualFinish;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.ActualDuration) then
    RootNode.AddChild('ActualDuration').TextAsDuration := Task.ActualDuration;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.ActualCost) then
    RootNode.AddChild('ActualCost').TextAsCost := Task.ActualCost;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.ActualOvertimeCost) then
    RootNode.AddChild('ActualOvertimeCost').TextAsCost :=
      Task.ActualOvertimeCost;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.ActualWork) then
    RootNode.AddChild('ActualWork').TextAsString := Task.ActualWork;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.ActualOvertimeWork) then
    RootNode.AddChild('ActualOvertimeWork').TextAsString :=
      Task.ActualOvertimeWork;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.ActualWorkProtected) then
    RootNode.AddChild('ActualWorkProtected').TextAsString :=
      Task.ActualWorkProtected;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.ActualOvertimeWorkProtected)
  then
    RootNode.AddChild('ActualOvertimeWorkProtected').TextAsString :=
      Task.ActualOvertimeWorkProtected;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.RegularWork) then
    RootNode.AddChild('RegularWork').TextAsString := Task.RegularWork;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.RemainingDuration) then
    RootNode.AddChild('RemainingDuration').TextAsDuration :=
      Task.RemainingDuration;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.RemainingCost) then
    RootNode.AddChild('RemainingCost').TextAsCost := Task.RemainingCost;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.RemainingWork) then
    RootNode.AddChild('RemainingWork').TextAsString := Task.RemainingWork;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.RemainingOvertimeCost) then
    RootNode.AddChild('RemainingOvertimeCost').TextAsCost :=
      Task.RemainingOvertimeCost;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.RemainingOvertimeWork) then
    RootNode.AddChild('RemainingOvertimeWork').TextAsString :=
      Task.RemainingOvertimeWork;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.ACWP) then
    RootNode.AddChild('ACWP').TextAsNumber := Task.ACWP;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.CV) then
    RootNode.AddChild('CV').TextAsNumber := Task.CV;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.ConstraintType) then
    RootNode.AddChild('ConstraintType').TextAsInteger :=
      Integer(Task.ConstraintType);
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.CalendarUID) then
    RootNode.AddChild('CalendarUID').TextAsInteger := Task.CalendarUID;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.ConstraintDate) then
    RootNode.AddChild('ConstraintDate').TextAsDateTime := Task.ConstraintDate;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.LevelAssignments) then
    RootNode.AddChild('LevelAssignments').TextAsBoolean :=
      Task.LevelAssignments;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.LevelingCanSplit) then
    RootNode.AddChild('LevelingCanSplit').TextAsBoolean :=
      Task.LevelingCanSplit;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.LevelingDelay) then
    RootNode.AddChild('LevelingDelay').TextAsInteger := Task.LevelingDelay;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.LevelingDelayFormat) then
    RootNode.AddChild('LevelingDelayFormat').TextAsInteger :=
      Integer(Task.LevelingDelayFormat);
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.IgnoreResourceCalendar) then
    RootNode.AddChild('IgnoreResourceCalendar').TextAsBoolean :=
      Task.IgnoreResourceCalendar;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Notes) then
    RootNode.AddChild('Notes').TextAsString := Task.Notes;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.NoteContainsObjects) then
    RootNode.AddChild('NoteContainsObjects').TextAsBoolean :=
      Task.NoteContainsObjects;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.HideBar) then
    RootNode.AddChild('HideBar').TextAsBoolean := Task.HideBar;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Rollup) then
    RootNode.AddChild('Rollup').TextAsBoolean := Task.Rollup;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.BCWS) then
    RootNode.AddChild('BCWS').TextAsNumber := Task.BCWS;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.BCWP) then
    RootNode.AddChild('BCWP').TextAsNumber := Task.BCWP;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.PhysicalPercentComplete)
  then
    RootNode.AddChild('PhysicalPercentComplete').TextAsInteger :=
      Task.PhysicalPercentComplete;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.EarnedValueMethod) then
    RootNode.AddChild('EarnedValueMethod').TextAsInteger :=
      Integer(Task.EarnedValueMethod);

  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.PredecessorLinks) then
    ExportElement(TdxGanttControlXMLExporterTaskPredecessorLinksState, RootNode,
      Task.PredecessorLinks);

  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.&Published) then
    RootNode.AddChild('IsPublished').TextAsBoolean := Task.&Published;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.CommitmentFinish) then
    RootNode.AddChild('CommitmentFinish').TextAsDateTime :=
      Task.CommitmentFinish;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.CommitmentStart) then
    RootNode.AddChild('CommitmentStart').TextAsDateTime := Task.CommitmentStart;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.CommitmentType) then
    RootNode.AddChild('CommitmentType').TextAsInteger :=
      Integer(Task.CommitmentType);

  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Contact) then
    RootNode.AddChild('Contact').TextAsString := Task.Contact;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Deadline) then
    RootNode.AddChild('Deadline').TextAsDateTime := Task.Deadline;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.ExternalTaskProject) then
    RootNode.AddChild('ExternalTaskProject').TextAsString :=
      Task.ExternalTaskProject;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Hyperlink) then
    RootNode.AddChild('Hyperlink').TextAsString := Task.Hyperlink;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.HyperlinkAddress) then
    RootNode.AddChild('HyperlinkAddress').TextAsString := Task.HyperlinkAddress;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.HyperlinkSubAddress) then
    RootNode.AddChild('HyperlinkSubAddress').TextAsString :=
      Task.HyperlinkSubAddress;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.PreLeveledFinish) then
    RootNode.AddChild('PreLeveledFinish').TextAsDateTime :=
      Task.PreLeveledFinish;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.PreLeveledStart) then
    RootNode.AddChild('PreLeveledStart').TextAsDateTime := Task.PreLeveledStart;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.StatusManager) then
    RootNode.AddChild('StatusManager').TextAsString := Task.StatusManager;
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.SubprojectName) then
    RootNode.AddChild('SubprojectName').TextAsString := Task.SubprojectName;

  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.ExtendedAttributes) then
    ExportElement(TdxGanttControlXMLExporterExtendedAttributeReferencesState,
      RootNode, Task.ExtendedAttributes);
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.Baselines) then
    ExportElement(TdxGanttControlXMLExporterTaskBaselinesState, RootNode,
      Task.Baselines);
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.EnterpriseExtendedAttributes)
  then
    ExportElement(TdxGanttControlXMLExporterEnterpriseExtendedAttributesState,
      RootNode, Task.EnterpriseExtendedAttributes);
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.OutlineCodes) then
    ExportElement(TdxGanttControlXMLExporterOutlineCodeReferencesState,
      RootNode, Task.OutlineCodes);
  if Task.IsValueAssigned(TdxGanttTaskAssignedValue.TimephasedDataItems) then
    ExportElement(TdxGanttControlXMLExporterTimephasedDataItemsState, RootNode,
      Task.TimephasedDataItems);
end;

{ TdxGanttControlXMLExporterTaskPredecessorLinksState }

function TdxGanttControlXMLExporterTaskPredecessorLinksState.GetPredecessorLinks
  : TdxGanttControlTaskPredecessorLinks;
begin
  Result := TdxGanttControlTaskPredecessorLinks(inherited Element);
end;

procedure TdxGanttControlXMLExporterTaskPredecessorLinksState.DoExport;
var
  I: Integer;
begin
  for I := 0 to PredecessorLinks.Count - 1 do
    ExportElement(TdxGanttControlXMLExporterTaskPredecessorLinkState,
      RootNode.AddChild('PredecessorLink'), PredecessorLinks[I]);
end;

{ TdxGanttControlXMLExporterTaskPredecessorLinkState }

function TdxGanttControlXMLExporterTaskPredecessorLinkState.GetPredecessorLink
  : TdxGanttControlTaskPredecessorLink;
begin
  Result := TdxGanttControlTaskPredecessorLink(inherited Element);
end;

procedure TdxGanttControlXMLExporterTaskPredecessorLinkState.DoExport;
begin
  if PredecessorLink.IsValueAssigned
    (TdxGanttTaskPredecessorLinkAssignedValue.PredecessorUID) then
    RootNode.AddChild('PredecessorUID').TextAsInteger :=
      PredecessorLink.PredecessorUID;
  if PredecessorLink.IsValueAssigned
    (TdxGanttTaskPredecessorLinkAssignedValue.&Type) then
    RootNode.AddChild('Type').TextAsInteger := Integer(PredecessorLink.&Type);
  if PredecessorLink.IsValueAssigned
    (TdxGanttTaskPredecessorLinkAssignedValue.CrossProject) then
    RootNode.AddChild('CrossProject').TextAsBoolean :=
      PredecessorLink.CrossProject;
  if PredecessorLink.IsValueAssigned
    (TdxGanttTaskPredecessorLinkAssignedValue.CrossProjectName) then
    RootNode.AddChild('CrossProjectName').TextAsString :=
      PredecessorLink.CrossProjectName;
  if PredecessorLink.IsValueAssigned
    (TdxGanttTaskPredecessorLinkAssignedValue.LinkLag) then
    RootNode.AddChild('LinkLag').TextAsInteger := PredecessorLink.LinkLag;
  if PredecessorLink.IsValueAssigned
    (TdxGanttTaskPredecessorLinkAssignedValue.LagFormat) then
    RootNode.AddChild('LagFormat').TextAsInteger :=
      Integer(PredecessorLink.LagFormat);
end;

{ TdxGanttControlXMLExporterTaskBaselinesState }

function TdxGanttControlXMLExporterTaskBaselinesState.GetBaselines
  : TdxGanttControlTaskBaselines;
begin
  Result := TdxGanttControlTaskBaselines(inherited Element);
end;

procedure TdxGanttControlXMLExporterTaskBaselinesState.DoExport;
var
  I: Integer;
begin
  for I := 0 to Baselines.Count - 1 do
    ExportElement(TdxGanttControlXMLExporterTaskBaselineState,
      RootNode.AddChild('Baseline'), Baselines[I]);
end;

{ TdxGanttControlXMLExporterTaskBaselineState }

function TdxGanttControlXMLExporterTaskBaselineState.GetBaseline
  : TdxGanttControlTaskBaseline;
begin
  Result := TdxGanttControlTaskBaseline(inherited Element);
end;

procedure TdxGanttControlXMLExporterTaskBaselineState.DoExport;
begin
  RootNode.AddChild('Number').TextAsInteger := Baseline.Number;
  if Baseline.IsValueAssigned(TdxGanttTaskBaselineAssignedValue.Duration) then
    RootNode.AddChild('Duration').TextAsDuration := Baseline.Duration;
  if Baseline.IsValueAssigned(TdxGanttTaskBaselineAssignedValue.DurationFormat)
  then
    RootNode.AddChild('DurationFormat').TextAsInteger :=
      Integer(Baseline.DurationFormat);
  if Baseline.IsValueAssigned
    (TdxGanttTaskBaselineAssignedValue.EstimatedDuration) then
    RootNode.AddChild('EstimatedDuration').TextAsBoolean :=
      Baseline.EstimatedDuration;
  if Baseline.IsValueAssigned(TdxGanttTaskBaselineAssignedValue.FixedCost) then
    RootNode.AddChild('FixedCost').TextAsNumber := Baseline.FixedCost;
  if Baseline.IsValueAssigned(TdxGanttTaskBaselineAssignedValue.Interim) then
    RootNode.AddChild('Interim').TextAsBoolean := Baseline.Interim;
  if Baseline.IsValueAssigned(TdxGanttTaskBaselineAssignedValue.Start) then
    RootNode.AddChild('Start').TextAsDateTime := Baseline.Start;
  if Baseline.IsValueAssigned(TdxGanttTaskBaselineAssignedValue.Finish) then
    RootNode.AddChild('Finish').TextAsDateTime := Baseline.Finish;
  ExportElement(TdxGanttControlXMLExporterTimephasedDataItemsState, RootNode,
    Baseline.TimephasedDataItems);
end;

{ TdxGanttControlXMLExporterOutlineCodeReferencesState }

function TdxGanttControlXMLExporterOutlineCodeReferencesState.GetOutlineCodes
  : TdxGanttControlOutlineCodeReferences;
begin
  Result := TdxGanttControlOutlineCodeReferences(inherited Element);
end;

procedure TdxGanttControlXMLExporterOutlineCodeReferencesState.DoExport;
var
  I: Integer;
begin
  for I := 0 to OutlineCodes.Count - 1 do
    ExportElement(TdxGanttControlXMLExporterOutlineCodeReferenceState,
      RootNode.AddChild('OutlineCode'), OutlineCodes[I]);
end;

{ TdxGanttControlXMLExporterOutlineCodeReferenceState }

function TdxGanttControlXMLExporterOutlineCodeReferenceState.GetOutlineCode
  : TdxGanttControlOutlineCodeReference;
begin
  Result := TdxGanttControlOutlineCodeReference(inherited Element);
end;

procedure TdxGanttControlXMLExporterOutlineCodeReferenceState.DoExport;
begin
  if OutlineCode.IsValueAssigned
    (TdxGanttOutlineCodeReferenceAssignedValue.FieldID) then
    RootNode.AddChild('FieldID').TextAsInteger := OutlineCode.FieldID;
  if OutlineCode.IsValueAssigned
    (TdxGanttOutlineCodeReferenceAssignedValue.ValueID) then
    RootNode.AddChild('ValueID').TextAsInteger := OutlineCode.ValueID;
  if OutlineCode.IsValueAssigned
    (TdxGanttOutlineCodeReferenceAssignedValue.ValueGUID) then
    RootNode.AddChild('ValueGUID').TextAsString := OutlineCode.ValueGUID;
end;

{ TdxGanttControlXMLExporterAvailabilityPeriodsState }

function TdxGanttControlXMLExporterAvailabilityPeriodsState.
  GetAvailabilityPeriods: TdxGanttControlResourceAvailabilityPeriods;
begin
  Result := TdxGanttControlResourceAvailabilityPeriods(inherited Element);
end;

procedure TdxGanttControlXMLExporterAvailabilityPeriodsState.DoExport;
var
  I: Integer;
begin
  for I := 0 to AvailabilityPeriods.Count - 1 do
    ExportElement(TdxGanttControlXMLExporterAvailabilityPeriodState,
      RootNode.AddChild('AvailabilityPeriod'), AvailabilityPeriods[I]);
end;

{ TdxGanttControlXMLExporterAvailabilityPeriodState }

function TdxGanttControlXMLExporterAvailabilityPeriodState.GetAvailabilityPeriod
  : TdxGanttControlResourceAvailabilityPeriod;
begin
  Result := TdxGanttControlResourceAvailabilityPeriod(inherited Element);
end;

procedure TdxGanttControlXMLExporterAvailabilityPeriodState.DoExport;
begin
  if AvailabilityPeriod.IsValueAssigned
    (TdxGanttResourceAvailabilityPeriodAssignedValue.AvailableFrom) then
    RootNode.AddChild('AvailableFrom').TextAsDateTime :=
      AvailabilityPeriod.AvailableFrom;
  if AvailabilityPeriod.IsValueAssigned
    (TdxGanttResourceAvailabilityPeriodAssignedValue.AvailableTo) then
    RootNode.AddChild('AvailableTo').TextAsDateTime :=
      AvailabilityPeriod.AvailableTo;
  if AvailabilityPeriod.IsValueAssigned
    (TdxGanttResourceAvailabilityPeriodAssignedValue.AvailableUnits) then
    RootNode.AddChild('AvailableUnits').TextAsNumber :=
      AvailabilityPeriod.AvailableUnits;
end;

{ TdxGanttControlXMLExporterResourceRatesState }

function TdxGanttControlXMLExporterResourceRatesState.GetRates
  : TdxGanttControlResourceRates;
begin
  Result := TdxGanttControlResourceRates(inherited Element);
end;

procedure TdxGanttControlXMLExporterResourceRatesState.DoExport;
var
  I: Integer;
begin
  for I := 0 to Rates.Count - 1 do
    ExportElement(TdxGanttControlXMLExporterResourceRateState,
      RootNode.AddChild('Rate'), Rates[I]);
end;

{ TdxGanttControlXMLExporterResourceRateState }

function TdxGanttControlXMLExporterResourceRateState.GetRate
  : TdxGanttControlResourceRate;
begin
  Result := TdxGanttControlResourceRate(inherited Element);
end;

procedure TdxGanttControlXMLExporterResourceRateState.DoExport;
begin
  RootNode.AddChild('RatesFrom').TextAsDateTime := Rate.RatesFrom;
  if Rate.IsValueAssigned(TdxGanttResourceRateAssignedValue.RatesTo) then
    RootNode.AddChild('RatesTo').TextAsDateTime := Rate.RatesTo;
  if Rate.IsValueAssigned(TdxGanttResourceRateAssignedValue.RateTable) then
    RootNode.AddChild('RateTable').TextAsInteger := Integer(Rate.RateTable);
  if Rate.IsValueAssigned(TdxGanttResourceRateAssignedValue.StandardRate) then
    RootNode.AddChild('StandardRate').TextAsCost := Rate.StandardRate;
  if Rate.IsValueAssigned(TdxGanttResourceRateAssignedValue.StandardRateFormat)
  then
    RootNode.AddChild('StandardRateFormat').TextAsInteger :=
      Integer(Rate.StandardRateFormat);
  if Rate.IsValueAssigned(TdxGanttResourceRateAssignedValue.OvertimeRate) then
    RootNode.AddChild('OvertimeRate').TextAsNumber := Rate.OvertimeRate;
  if Rate.IsValueAssigned(TdxGanttResourceRateAssignedValue.OvertimeRateFormat)
  then
    RootNode.AddChild('OvertimeRateFormat').TextAsInteger :=
      Integer(Rate.OvertimeRateFormat);
  if Rate.IsValueAssigned(TdxGanttResourceRateAssignedValue.CostPerUse) then
    RootNode.AddChild('CostPerUse').TextAsCost := Rate.CostPerUse;
end;

{ TdxGanttControlXMLExporterAssignmentsState }

function TdxGanttControlXMLExporterAssignmentsState.GetAssignments
  : TdxGanttControlAssignments;
begin
  Result := TdxGanttControlAssignments(inherited Element);
end;

procedure TdxGanttControlXMLExporterAssignmentsState.DoExport;
var
  I: Integer;
begin
  for I := 0 to Assignments.Count - 1 do
    ExportElement(TdxGanttControlXMLExporterAssignmentState,
      RootNode.AddChild('Assignment'), Assignments[I]);
end;

{ TdxGanttControlXMLExporterResourcesState }

function TdxGanttControlXMLExporterResourcesState.GetResources
  : TdxGanttControlResources;
begin
  Result := TdxGanttControlResources(inherited Element);
end;

procedure TdxGanttControlXMLExporterResourcesState.DoExport;
var
  I: Integer;
begin
  for I := 0 to Resources.Count - 1 do
    ExportElement(TdxGanttControlXMLExporterResourceState,
      RootNode.AddChild('Resource'), Resources[I]);
end;

{ TdxGanttControlXMLExporterResourceState }

function TdxGanttControlXMLExporterResourceState.GetResource
  : TdxGanttControlResource;
begin
  Result := TdxGanttControlResource(inherited Element);
end;

procedure TdxGanttControlXMLExporterResourceState.DoExport;
begin
  RootNode.AddChild('UID').TextAsInteger := Resource.UID;
  RootNode.AddChild('GUID').TextAsString := TdxGanttControlResourceAccess
    (Resource).GUID;
  RootNode.AddChild('ID').TextAsInteger := Resource.ID;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.Name) then
    RootNode.AddChild('Name').TextAsString := Resource.Name;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.&Type) then
    RootNode.AddChild('Type').TextAsInteger := Integer(Resource.&Type);
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.Blank) then
    RootNode.AddChild('IsNull').TextAsBoolean := Resource.Blank;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.Initials) then
    RootNode.AddChild('Initials').TextAsString := Resource.Initials;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.Phonetics) then
    RootNode.AddChild('Phonetics').TextAsString := Resource.Phonetics;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.NTAccount) then
    RootNode.AddChild('NTAccount').TextAsString := Resource.NTAccount;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.MaterialLabel) then
    RootNode.AddChild('MaterialLabel').TextAsString := Resource.MaterialLabel;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.Code) then
    RootNode.AddChild('Code').TextAsString := Resource.Code;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.Group) then
    RootNode.AddChild('Group').TextAsString := Resource.Group;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.WorkGroup) then
    RootNode.AddChild('WorkGroup').TextAsInteger := Integer(Resource.WorkGroup);
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.EmailAddress) then
    RootNode.AddChild('EmailAddress').TextAsString := Resource.EmailAddress;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.Hyperlink) then
    RootNode.AddChild('Hyperlink').TextAsString := Resource.Hyperlink;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.HyperlinkAddress)
  then
    RootNode.AddChild('HyperlinkAddress').TextAsString :=
      Resource.HyperlinkAddress;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.HyperlinkSubAddress)
  then
    RootNode.AddChild('HyperlinkSubAddress').TextAsString :=
      Resource.HyperlinkSubAddress;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.MaxUnits) then
    RootNode.AddChild('MaxUnits').TextAsNumber := Resource.MaxUnits;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.PeakUnits) then
    RootNode.AddChild('PeakUnits').TextAsNumber := Resource.PeakUnits;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.OverAllocated) then
    RootNode.AddChild('OverAllocated').TextAsBoolean := Resource.OverAllocated;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.AvailableFrom) then
    RootNode.AddChild('AvailableFrom').TextAsDateTime := Resource.AvailableFrom;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.AvailableTo) then
    RootNode.AddChild('AvailableTo').TextAsDateTime := Resource.AvailableTo;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.Start) then
    RootNode.AddChild('Start').TextAsDateTime := Resource.Start;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.Finish) then
    RootNode.AddChild('Finish').TextAsDateTime := Resource.Finish;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.CanLevel) then
    RootNode.AddChild('CanLevel').TextAsBoolean := Resource.CanLevel;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.AccrueAt) then
    RootNode.AddChild('AccrueAt').TextAsInteger := Resource.AccrueAt;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.Work) then
    RootNode.AddChild('Work').TextAsString := Resource.Work;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.RegularWork) then
    RootNode.AddChild('RegularWork').TextAsString := Resource.RegularWork;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.OvertimeWork) then
    RootNode.AddChild('OvertimeWork').TextAsString := Resource.OvertimeWork;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.ActualWork) then
    RootNode.AddChild('ActualWork').TextAsString := Resource.ActualWork;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.RemainingWork) then
    RootNode.AddChild('RemainingWork').TextAsString := Resource.RemainingWork;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.ActualOvertimeWork)
  then
    RootNode.AddChild('ActualOvertimeWork').TextAsString :=
      Resource.ActualOvertimeWork;
  if Resource.IsValueAssigned
    (TdxGanttResourceAssignedValue.RemainingOvertimeWork) then
    RootNode.AddChild('RemainingOvertimeWork').TextAsString :=
      Resource.RemainingOvertimeWork;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.PercentWorkComplete)
  then
    RootNode.AddChild('PercentWorkComplete').TextAsInteger :=
      Resource.PercentWorkComplete;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.StandardRate) then
    RootNode.AddChild('StandardRate').TextAsCost := Resource.StandardRate;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.StandardRateFormat)
  then
    RootNode.AddChild('StandardRateFormat').TextAsInteger :=
      Integer(Resource.StandardRateFormat);
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.Cost) then
    RootNode.AddChild('Cost').TextAsCost := Resource.Cost;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.OvertimeRate) then
    RootNode.AddChild('OvertimeRate').TextAsCost := Resource.OvertimeRate;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.OvertimeRateFormat)
  then
    RootNode.AddChild('OvertimeRateFormat').TextAsInteger :=
      Integer(Resource.OvertimeRateFormat);
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.OvertimeCost) then
    RootNode.AddChild('OvertimeCost').TextAsCost := Resource.OvertimeCost;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.CostPerUse) then
    RootNode.AddChild('CostPerUse').TextAsCost := Resource.CostPerUse;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.ActualCost) then
    RootNode.AddChild('ActualCost').TextAsCost := Resource.ActualCost;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.ActualOvertimeCost)
  then
    RootNode.AddChild('ActualOvertimeCost').TextAsCost :=
      Resource.ActualOvertimeCost;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.RemainingCost) then
    RootNode.AddChild('RemainingCost').TextAsCost := Resource.RemainingCost;
  if Resource.IsValueAssigned
    (TdxGanttResourceAssignedValue.RemainingOvertimeCost) then
    RootNode.AddChild('RemainingOvertimeCost').TextAsCost :=
      Resource.RemainingOvertimeCost;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.WorkVariance) then
    RootNode.AddChild('WorkVariance').TextAsNumber := Resource.WorkVariance;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.CostVariance) then
    RootNode.AddChild('CostVariance').TextAsCost := Resource.CostVariance;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.SV) then
    RootNode.AddChild('SV').TextAsNumber := Resource.SV;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.CV) then
    RootNode.AddChild('CV').TextAsNumber := Resource.CV;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.ACWP) then
    RootNode.AddChild('ACWP').TextAsNumber := Resource.ACWP;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.CalendarUID) then
    RootNode.AddChild('CalendarUID').TextAsInteger := Resource.CalendarUID;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.Notes) then
    RootNode.AddChild('Notes').TextAsString := Resource.Notes;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.BCWS) then
    RootNode.AddChild('BCWS').TextAsNumber := Resource.BCWS;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.BCWP) then
    RootNode.AddChild('BCWP').TextAsNumber := Resource.BCWP;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.Generic) then
    RootNode.AddChild('IsGeneric').TextAsBoolean := Resource.Generic;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.Inactive) then
    RootNode.AddChild('IsInactive').TextAsBoolean := Resource.Inactive;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.Enterprise) then
    RootNode.AddChild('IsEnterprise').TextAsBoolean := Resource.Enterprise;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.BookingType) then
    RootNode.AddChild('BookingType').TextAsInteger :=
      Integer(Resource.BookingType);
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.ActualWorkProtected)
  then
    RootNode.AddChild('ActualWorkProtected').TextAsString :=
      Resource.ActualWorkProtected;
  if Resource.IsValueAssigned
    (TdxGanttResourceAssignedValue.ActualOvertimeWorkProtected) then
    RootNode.AddChild('ActualOvertimeWorkProtected').TextAsString :=
      Resource.ActualOvertimeWorkProtected;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.ActiveDirectoryGUID)
  then
    RootNode.AddChild('ActiveDirectoryGUID').TextAsString :=
      Resource.ActiveDirectoryGUID;
  RootNode.AddChild('CreationDate').TextAsDateTime := Resource.Created;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.CostResource) then
    RootNode.AddChild('IsCostResource').TextAsBoolean := Resource.CostResource;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.AssnOwner) then
    RootNode.AddChild('AssnOwner').TextAsString := Resource.AssnOwner;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.AssnOwnerGuid) then
    RootNode.AddChild('AssnOwnerGuid').TextAsString := Resource.AssnOwnerGuid;
  if Resource.IsValueAssigned(TdxGanttResourceAssignedValue.Budget) then
    RootNode.AddChild('IsBudget').TextAsBoolean := Resource.Budget;

  ExportElement(TdxGanttControlXMLExporterExtendedAttributeReferencesState,
    RootNode, Resource.ExtendedAttributes);
  ExportElement(TdxGanttControlXMLExporterResourceBaselinesState, RootNode,
    Resource.Baselines);
  ExportElement(TdxGanttControlXMLExporterOutlineCodeReferencesState, RootNode,
    Resource.OutlineCodes);
  if Resource.AvailabilityPeriods.Count > 0 then
    ExportElement(TdxGanttControlXMLExporterAvailabilityPeriodsState,
      RootNode.AddChild('AvailabilityPeriods'), Resource.AvailabilityPeriods);
  if Resource.Rates.Count > 0 then
    ExportElement(TdxGanttControlXMLExporterResourceRatesState,
      RootNode.AddChild('Rates'), Resource.Rates);
  ExportElement(TdxGanttControlXMLExporterTimephasedDataItemsState, RootNode,
    Resource.TimephasedDataItems);
end;

{ TdxGanttControlXMLExporterResourceBaselinesState }

function TdxGanttControlXMLExporterResourceBaselinesState.GetBaselines
  : TdxGanttControlResourceBaselines;
begin
  Result := TdxGanttControlResourceBaselines(inherited Element);
end;

procedure TdxGanttControlXMLExporterResourceBaselinesState.DoExport;
var
  I: Integer;
begin
  for I := 0 to Baselines.Count - 1 do
    ExportElement(TdxGanttControlXMLExporterResourceBaselineState,
      RootNode.AddChild('Baseline'), Baselines[I]);
end;

{ TdxGanttControlXMLExporterResourceBaselineState }

function TdxGanttControlXMLExporterResourceBaselineState.GetBaseline
  : TdxGanttControlResourceBaseline;
begin
  Result := TdxGanttControlResourceBaseline(inherited Element);
end;

procedure TdxGanttControlXMLExporterResourceBaselineState.DoExport;
begin
  RootNode.AddChild('Number').TextAsInteger := Baseline.Number;
  if Baseline.IsValueAssigned(TdxGanttBaselineAssignedValue.Work) then
    RootNode.AddChild('Work').TextAsString := Baseline.Work;
  if Baseline.IsValueAssigned(TdxGanttBaselineAssignedValue.Cost) then
    RootNode.AddChild('Cost').TextAsNumber := Baseline.Cost;
  if Baseline.IsValueAssigned(TdxGanttBaselineAssignedValue.BCWS) then
    RootNode.AddChild('BCWS').TextAsNumber := Baseline.BCWS;
  if Baseline.IsValueAssigned(TdxGanttBaselineAssignedValue.BCWP) then
    RootNode.AddChild('BCWP').TextAsNumber := Baseline.BCWP;
end;

{ TdxGanttControlXMLExporterAssignmentState }

function TdxGanttControlXMLExporterAssignmentState.GetAssignment
  : TdxGanttControlAssignment;
begin
  Result := TdxGanttControlAssignment(inherited Element);
end;

procedure TdxGanttControlXMLExporterAssignmentState.DoExport;
begin
  RootNode.AddChild('UID').TextAsInteger := Assignment.UID;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.GUID) then
    RootNode.AddChild('GUID').TextAsString := Assignment.GUID;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.TaskUID) then
    RootNode.AddChild('TaskUID').TextAsInteger := Assignment.TaskUID;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.ResourceUID)
  then
    RootNode.AddChild('ResourceUID').TextAsInteger := Assignment.ResourceUID;
  if Assignment.IsValueAssigned
    (TdxGanttAssignmentAssignedValue.PercentWorkComplete) then
    RootNode.AddChild('PercentWorkComplete').TextAsInteger :=
      Assignment.PercentWorkComplete;
  RootNode.AddChild('ActualCost').TextAsCost := Assignment.ActualCost;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.ActualFinish)
  then
    RootNode.AddChild('ActualFinish').TextAsDateTime := Assignment.ActualFinish;
  if Assignment.IsValueAssigned
    (TdxGanttAssignmentAssignedValue.ActualOvertimeCost) then
    RootNode.AddChild('ActualOvertimeCost').TextAsCost :=
      Assignment.ActualOvertimeCost;
  if Assignment.IsValueAssigned
    (TdxGanttAssignmentAssignedValue.ActualOvertimeWork) then
    RootNode.AddChild('ActualOvertimeWork').TextAsString :=
      Assignment.ActualOvertimeWork;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.ActualStart)
  then
    RootNode.AddChild('ActualStart').TextAsDateTime := Assignment.ActualStart;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.ActualWork) then
    RootNode.AddChild('ActualWork').TextAsString := Assignment.ActualWork;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.ACWP) then
    RootNode.AddChild('ACWP').TextAsNumber := Assignment.ACWP;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.Confirmed) then
    RootNode.AddChild('Confirmed').TextAsBoolean := Assignment.Confirmed;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.Cost) then
    RootNode.AddChild('Cost').TextAsCost := Assignment.Cost;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.CostRateTable)
  then
    RootNode.AddChild('CostRateTable').TextAsInteger :=
      Integer(Assignment.CostRateTable);
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.RateScale) then
    RootNode.AddChild('RateScale').TextAsInteger := Assignment.RateScale;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.CostVariance)
  then
    RootNode.AddChild('CostVariance').TextAsCost := Assignment.CostVariance;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.CV) then
    RootNode.AddChild('CV').TextAsNumber := Assignment.CV;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.Delay) then
    RootNode.AddChild('Delay').TextAsInteger := Assignment.Delay;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.Finish) then
    RootNode.AddChild('Finish').TextAsDateTime := Assignment.Finish;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.FinishVariance)
  then
    RootNode.AddChild('FinishVariance').TextAsInteger :=
      Assignment.FinishVariance;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.Hyperlink) then
    RootNode.AddChild('Hyperlink').TextAsString := Assignment.Hyperlink;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.HyperlinkAddress)
  then
    RootNode.AddChild('HyperlinkAddress').TextAsString :=
      Assignment.HyperlinkAddress;
  if Assignment.IsValueAssigned
    (TdxGanttAssignmentAssignedValue.HyperlinkSubAddress) then
    RootNode.AddChild('HyperlinkSubAddress').TextAsString :=
      Assignment.HyperlinkSubAddress;
  RootNode.AddChild('WorkVariance').TextAsNumber := Assignment.WorkVariance;
  if Assignment.IsValueAssigned
    (TdxGanttAssignmentAssignedValue.HasFixedRateUnits) then
    RootNode.AddChild('HasFixedRateUnits').TextAsBoolean :=
      Assignment.HasFixedRateUnits;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.FixedMaterial)
  then
    RootNode.AddChild('FixedMaterial').TextAsBoolean :=
      Assignment.FixedMaterial;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.LevelingDelay)
  then
    RootNode.AddChild('LevelingDelay').TextAsInteger :=
      Assignment.LevelingDelay;
  if Assignment.IsValueAssigned
    (TdxGanttAssignmentAssignedValue.LevelingDelayFormat) then
    RootNode.AddChild('LevelingDelayFormat').TextAsInteger :=
      Integer(Assignment.LevelingDelayFormat);
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.LinkedFields)
  then
    RootNode.AddChild('LinkedFields').TextAsBoolean := Assignment.LinkedFields;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.Milestone) then
    RootNode.AddChild('Milestone').TextAsBoolean := Assignment.Milestone;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.Notes) then
    RootNode.AddChild('Notes').TextAsString := Assignment.Notes;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.OverAllocated)
  then
    RootNode.AddChild('OverAllocated').TextAsBoolean :=
      Assignment.OverAllocated;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.OvertimeCost)
  then
    RootNode.AddChild('OvertimeCost').TextAsCost := Assignment.OvertimeCost;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.OvertimeWork)
  then
    RootNode.AddChild('OvertimeWork').TextAsString := Assignment.OvertimeWork;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.RegularWork)
  then
    RootNode.AddChild('RegularWork').TextAsString := Assignment.RegularWork;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.RemainingCost)
  then
    RootNode.AddChild('RemainingCost').TextAsCost := Assignment.RemainingCost;
  RootNode.AddChild('RemainingOvertimeCost').TextAsCost :=
    Assignment.RemainingOvertimeCost;
  if Assignment.IsValueAssigned
    (TdxGanttAssignmentAssignedValue.RemainingOvertimeWork) then
    RootNode.AddChild('RemainingOvertimeWork').TextAsString :=
      Assignment.RemainingOvertimeWork;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.RemainingWork)
  then
    RootNode.AddChild('RemainingWork').TextAsString := Assignment.RemainingWork;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.ResponsePending)
  then
    RootNode.AddChild('ResponsePending').TextAsBoolean :=
      Assignment.ResponsePending;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.Start) then
    RootNode.AddChild('Start').TextAsDateTime := Assignment.Start;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.Stop) then
    RootNode.AddChild('Stop').TextAsDateTime := Assignment.Stop;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.Resume) then
    RootNode.AddChild('Resume').TextAsDateTime := Assignment.Resume;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.StartVariance)
  then
    RootNode.AddChild('StartVariance').TextAsInteger :=
      Assignment.StartVariance;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.Summary) then
    RootNode.AddChild('Summary').TextAsBoolean := Assignment.Summary;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.SV) then
    RootNode.AddChild('SV').TextAsNumber := Assignment.SV;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.Units) then
    RootNode.AddChild('Units').TextAsCost := Assignment.Units;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.UpdateNeeded)
  then
    RootNode.AddChild('UpdateNeeded').TextAsBoolean := Assignment.UpdateNeeded;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.VAC) then
    RootNode.AddChild('VAC').TextAsNumber := Assignment.VAC;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.Work) then
    RootNode.AddChild('Work').TextAsString := Assignment.Work;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.WorkContour)
  then
    RootNode.AddChild('WorkContour').TextAsInteger := Assignment.WorkContour;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.BCWS) then
    RootNode.AddChild('BCWS').TextAsNumber := Assignment.BCWS;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.BCWP) then
    RootNode.AddChild('BCWP').TextAsNumber := Assignment.BCWP;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.BookingType)
  then
    RootNode.AddChild('BookingType').TextAsInteger :=
      Integer(Assignment.BookingType);
  if Assignment.IsValueAssigned
    (TdxGanttAssignmentAssignedValue.ActualWorkProtected) then
    RootNode.AddChild('ActualWorkProtected').TextAsString :=
      Assignment.ActualWorkProtected;
  if Assignment.IsValueAssigned
    (TdxGanttAssignmentAssignedValue.ActualOvertimeWorkProtected) then
    RootNode.AddChild('ActualOvertimeWorkProtected').TextAsString :=
      Assignment.ActualOvertimeWorkProtected;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.Created) then
    RootNode.AddChild('CreationDate').TextAsDateTime := Assignment.Created;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.AssnOwner) then
    RootNode.AddChild('AssnOwner').TextAsString := Assignment.AssnOwner;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.AssnOwnerGuid)
  then
    RootNode.AddChild('AssnOwnerGuid').TextAsString := Assignment.AssnOwnerGuid;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.BudgetCost) then
    RootNode.AddChild('BudgetCost').TextAsCost := Assignment.BudgetCost;
  if Assignment.IsValueAssigned(TdxGanttAssignmentAssignedValue.BudgetWork) then
    RootNode.AddChild('BudgetWork').TextAsString := Assignment.BudgetWork;
  if Assignment.IsValueAssigned
    (TdxGanttAssignmentAssignedValue.NoteContainsObjects) then
    RootNode.AddChild('NoteContainsObjects').TextAsBoolean :=
      Assignment.NoteContainsObjects;

  ExportElement(TdxGanttControlXMLExporterAssignmentBaselinesState, RootNode,
    Assignment.Baselines);
  ExportElement(TdxGanttControlXMLExporterEnterpriseExtendedAttributesState,
    RootNode, Assignment.EnterpriseExtendedAttributes);
  ExportElement(TdxGanttControlXMLExporterExtendedAttributeReferencesState,
    RootNode, Assignment.ExtendedAttributes);
  ExportElement(TdxGanttControlXMLExporterTimephasedDataItemsState, RootNode,
    Assignment.TimephasedDataItems);
end;

{ TdxGanttControlXMLExporterAssignmentBaselinesState }

function TdxGanttControlXMLExporterAssignmentBaselinesState.GetBaselines
  : TdxGanttControlAssignmentBaselines;
begin
  Result := TdxGanttControlAssignmentBaselines(inherited Element);
end;

procedure TdxGanttControlXMLExporterAssignmentBaselinesState.DoExport;
var
  I: Integer;
begin
  for I := 0 to Baselines.Count - 1 do
    ExportElement(TdxGanttControlXMLExporterAssignmentBaselineState,
      RootNode.AddChild('Baseline'), Baselines[I]);
end;

{ TdxGanttControlXMLExporterAssignmentBaselineState }

function TdxGanttControlXMLExporterAssignmentBaselineState.GetBaseline
  : TdxGanttControlAssignmentBaseline;
begin
  Result := TdxGanttControlAssignmentBaseline(inherited Element);
end;

procedure TdxGanttControlXMLExporterAssignmentBaselineState.DoExport;
begin
  if Baseline.IsValueAssigned(TdxGanttAssignmentBaselineAssignedValue.Start)
  then
    RootNode.AddChild('Start').TextAsDateTime := Baseline.Start;
  if Baseline.IsValueAssigned(TdxGanttAssignmentBaselineAssignedValue.Finish)
  then
    RootNode.AddChild('Finish').TextAsDateTime := Baseline.Finish;
  if Baseline.TimephasedDataItems <> nil then
    ExportElement(TdxGanttControlXMLExporterTimephasedDataItemsState, RootNode,
      Baseline.TimephasedDataItems);
end;

{ TdxGanttControlXMLExporterEnterpriseExtendedAttributesState }

function TdxGanttControlXMLExporterEnterpriseExtendedAttributesState.
  GetEnterpriseExtendedAttributes: TdxGanttControlEnterpriseExtendedAttributes;
begin
  Result := TdxGanttControlEnterpriseExtendedAttributes(inherited Element);
end;

procedure TdxGanttControlXMLExporterEnterpriseExtendedAttributesState.DoExport;
var
  I: Integer;
begin
  for I := 0 to EnterpriseExtendedAttributes.Count - 1 do
    ExportElement(TdxGanttControlXMLExporterEnterpriseExtendedAttributeState,
      RootNode.AddChild('EnterpriseExtendedAttribute'),
      EnterpriseExtendedAttributes[I]);
end;

{ TdxGanttControlXMLExporterEnterpriseExtendedAttributeState }

function TdxGanttControlXMLExporterEnterpriseExtendedAttributeState.
  GetEnterpriseExtendedAttribute: TdxGanttControlEnterpriseExtendedAttribute;
begin
  Result := TdxGanttControlEnterpriseExtendedAttribute(inherited Element);
end;

procedure TdxGanttControlXMLExporterEnterpriseExtendedAttributeState.DoExport;
begin
  RootNode.AddChild('FieldIDInHex').TextAsString :=
    EnterpriseExtendedAttribute.FieldIDInHex;
  RootNode.AddChild('FieldID').TextAsInteger :=
    EnterpriseExtendedAttribute.FieldID;
  ExportElement(TdxGanttControlXMLExporterEnterpriseExtendedAttributeValueState,
    RootNode.AddChild('Value'), EnterpriseExtendedAttribute.Value);
  RootNode.AddChild('FieldValueGUID').TextAsString :=
    EnterpriseExtendedAttribute.FieldValueGUID;
end;

{ TdxGanttControlXMLExporterEnterpriseExtendedAttributeValueState }

function TdxGanttControlXMLExporterEnterpriseExtendedAttributeValueState.
  GetValue: TdxGanttControlEnterpriseExtendedAttributeValue;
begin
  Result := TdxGanttControlEnterpriseExtendedAttributeValue(inherited Element);
end;

procedure TdxGanttControlXMLExporterEnterpriseExtendedAttributeValueState.
  DoExport;
begin
  RootNode.AddChild('ID').TextAsInteger := Value.ID;
  if Value.IsValueAssigned(TdxGanttEnterpriseExtendedAttributeAssignedValue.
    Value) then
    RootNode.AddChild('Value').TextAsString := Value.Value;
  if Value.IsValueAssigned(TdxGanttEnterpriseExtendedAttributeAssignedValue.
    Description) then
    RootNode.AddChild('Description').TextAsString := Value.Description;
  if Value.IsValueAssigned(TdxGanttEnterpriseExtendedAttributeAssignedValue.
    Phonetic) then
    RootNode.AddChild('Phonetic').TextAsString := Value.Phonetic;
end;

{ TdxGanttControlXMLExporterExtendedAttributeReferencesState }

function TdxGanttControlXMLExporterExtendedAttributeReferencesState.
  GetAttributes: TdxGanttControlExtendedAttributeReferences;
begin
  Result := TdxGanttControlExtendedAttributeReferences(inherited Element);
end;

procedure TdxGanttControlXMLExporterExtendedAttributeReferencesState.DoExport;
var
  I: Integer;
begin
  for I := 0 to Attributes.Count - 1 do
    ExportElement(TdxGanttControlXMLExporterExtendedAttributeReferenceState,
      RootNode.AddChild('ExtendedAttribute'), Attributes[I]);
end;

{ TdxGanttControlXMLExporterExtendedAttributeReferenceState }

function TdxGanttControlXMLExporterExtendedAttributeReferenceState.GetAttribute
  : TdxGanttControlExtendedAttributeReference;
begin
  Result := TdxGanttControlExtendedAttributeReference(inherited Element);
end;

procedure TdxGanttControlXMLExporterExtendedAttributeReferenceState.DoExport;
begin
  if Attribute.IsValueAssigned(TdxGanttExtendedAttributeReferenceAssignedValue.
    FieldID) then
    RootNode.AddChild('FieldID').TextAsInteger := Attribute.FieldID;
  if Attribute.IsValueAssigned(TdxGanttExtendedAttributeReferenceAssignedValue.
    Value) then
    RootNode.AddChild('Value').TextAsString := Attribute.Value;
  if Attribute.IsValueAssigned(TdxGanttExtendedAttributeReferenceAssignedValue.
    ValueGUID) then
    RootNode.AddChild('ValueGUID').TextAsString := Attribute.ValueGUID;
  if Attribute.IsValueAssigned(TdxGanttExtendedAttributeReferenceAssignedValue.
    DurationFormat) then
    RootNode.AddChild('DurationFormat').TextAsInteger :=
      Integer(Attribute.DurationFormat);
end;

{ TdxGanttControlXMLExporterTimephasedDataItemsState }

function TdxGanttControlXMLExporterTimephasedDataItemsState.
  GetTimephasedDataItems: TdxGanttControlTimephasedDataItems;
begin
  Result := TdxGanttControlTimephasedDataItems(inherited Element);
end;

procedure TdxGanttControlXMLExporterTimephasedDataItemsState.DoExport;
var
  I: Integer;
begin
  for I := 0 to TimephasedDataItems.Count - 1 do
    ExportElement(TdxGanttControlXMLExporterTimephasedDataState,
      RootNode.AddChild('TimephasedData'), TimephasedDataItems[I]);
end;

{ TdxGanttControlXMLExporterTimephasedDataState }

function TdxGanttControlXMLExporterTimephasedDataState.GetTimephasedData
  : TdxGanttControlTimephasedDataItem;
begin
  Result := TdxGanttControlTimephasedDataItem(inherited Element);
end;

procedure TdxGanttControlXMLExporterTimephasedDataState.DoExport;
begin
  if TimephasedData.IsValueAssigned(TdxGanttTimephasedDataAssignedValue.&Type)
  then
    RootNode.AddChild('Type').TextAsInteger := Integer(TimephasedData.&Type);
  RootNode.AddChild('UID').TextAsInteger := TimephasedData.OwnerUID;
  if TimephasedData.IsValueAssigned(TdxGanttTimephasedDataAssignedValue.Start)
  then
    RootNode.AddChild('Start').TextAsDateTime := TimephasedData.Start;
  if TimephasedData.IsValueAssigned(TdxGanttTimephasedDataAssignedValue.Finish)
  then
    RootNode.AddChild('Finish').TextAsDateTime := TimephasedData.Finish;
  if TimephasedData.IsValueAssigned(TdxGanttTimephasedDataAssignedValue.&Unit)
  then
    RootNode.AddChild('Unit').TextAsInteger := Integer(TimephasedData.&Unit);
  if TimephasedData.IsValueAssigned(TdxGanttTimephasedDataAssignedValue.Value)
  then
    RootNode.AddChild('Value').TextAsString := TimephasedData.Value;
end;

{ TdxGanttControlXMLExporter }

procedure TdxGanttControlXMLExporter.DoSaveToStream(AStream: TStream);
var
  ADoc: TdxXMLDocument;
  AProject: TdxGanttControlXMLExporterProjectState;
begin
  ADoc := TdxXMLDocument.Create(DataModel);
  try
    ADoc.Standalone := 'yes';
    ADoc.AutoIndent := True;

    AProject := TdxGanttControlXMLExporterProjectState.Create
      (ADoc.Root.AddChild('Project'), DataModel);
    try
      AProject.Export;
    finally
      AProject.Free;
    end;
    ADoc.SaveToStream(AStream);
  finally
    ADoc.Free;
  end;
end;

class function TdxGanttControlXMLExporter.GetExtensions: TArray<string>;
begin
  Result := TArray<string>.Create('.xml');
end;

initialization

TdxGanttControlXMLExporter.Register;

finalization

TdxGanttControlXMLExporter.Unregister;

end.
