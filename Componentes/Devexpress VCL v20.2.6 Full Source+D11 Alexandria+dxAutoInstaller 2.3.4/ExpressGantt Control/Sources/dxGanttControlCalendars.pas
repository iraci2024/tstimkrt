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

unit dxGanttControlCalendars;

{$I cxVer.inc}
{$I dxGanttControl.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes, System.Contnrs,
{$ENDIF}
  SysUtils, Generics.Defaults, Generics.Collections, Classes, DateUtils,
  dxCore, dxCoreClasses,
  dxGanttControlCustomClasses,
  dxGanttControlCustomDataModel;

const
  dxEndOfDay = 1 - OneMillisecond;

type
  TdxGanttControlCalendars = class;
  TdxGanttControlCalendar = class;
  TdxGanttControlCalendarWeekDay = class;
  TdxGanttControlCalendarWeekDays = class;

  TdxGanttControlWeekDayType = (Exception, Sunday, Monday, Tuesday, Wednesday,
    Thursday, Friday, Saturday);
  TdxGanttControlExceptionDayOfWeek = (Sunday = 1, Monday, Tuesday, Wednesday,
    Thursday, Friday, Saturday);
  TdxGanttControlExceptionDaysOfWeek = set of TdxGanttControlExceptionDayOfWeek;
  TdxGanttControlExceptionMonthItem = (Day, Weekday, WeekendDay, Sunday, Monday,
    Tuesday, Wednesday, Thursday, Friday, Saturday);
  TdxGanttControlExceptionType = (Daily = 1, YearlyByDayOfMonth,
    MonthlyByPosition, Weekly, ByDayCount, ByDaysOfWeek);
  TdxGanttControlExceptionMonthPosition = (First, Second, Third, Fourth, Last);

{$REGION 'for internal use'}
  { TdxGanttControlDuration }

  TdxGanttControlDuration = record
  private
    FDuration: string;
    FHour: Word;
    FMinute: Word;
    FSecond: Word;

    class procedure ExtractWorkTime(ADay: TdxGanttControlCalendarWeekDay;
      APeriodNumber: Integer; var AFromTime, AToTime: TDateTime); static;
    class procedure DecreaseTimeValue(var AValue: Double;
      const ADelta: Double); static;
    class function GetDurationString(AHour, AMinute, ASecond: Word;
      AIsNegative: Boolean): string; static;

    function GetAsSeconds: Int64;
    function GetAsMinutes: Double;
    function GetAsHours: Double;
    function GetAsDays: Double;
    function GetAsWeeks: Double;
    function GetAsMonths: Double;
    function GetAsElapsedDays: Double;
    function GetAsElapsedWeeks: Double;
    function GetAsElapsedMonths: Double;
  public
    class function Create(const AStart, AFinish: TDateTime;
      ACalendar: TdxGanttControlCalendar; AFormat: TdxDurationFormat)
      : TdxGanttControlDuration; overload; static;
    class function Create(const AStart, AFinish: TDateTime;
      ACalendar: TdxGanttControlCalendar; AIsElapsed: Boolean = False)
      : TdxGanttControlDuration; overload; static;
    class function Create(const ADuration: string): TdxGanttControlDuration;
      overload; static;
    class function Create(AValue: Double; AFormat: TdxDurationFormat)
      : TdxGanttControlDuration; overload; static;
    class function Decode(const ADuration: string;
      var AHour, AMinute, ASecond: Word; var AIsNegative: Boolean)
      : Boolean; static;

    class function CalculateDayWorkTime(ADay: TDate; AStart, AFinish: TTime;
      ACalendar: TdxGanttControlCalendar; var AHour, AMinute, ASecond: Word)
      : Boolean; static;
    class procedure CalculateWorkTime(AStart, AFinish: TDateTime;
      ACalendar: TdxGanttControlCalendar;
      var AHour, AMinute, ASecond: Word); static;

    class function GetDurationMeasurementUnit(AFormat: TdxDurationFormat;
      AIsPlural, ANeedPostfix: Boolean): string; static;
    class function GetDurationValue(const ADuration: string;
      AFormat: TdxDurationFormat): Double; static;

    function GetWorkFinish(const AWorkStart: TDateTime;
      ACalendar: TdxGanttControlCalendar; AIsElapsed: Boolean = False)
      : TDateTime; overload;
    function GetWorkStart(const AWorkFinish: TDateTime;
      ACalendar: TdxGanttControlCalendar; AIsElapsed: Boolean = False)
      : TDateTime; overload;
    function GetWorkFinish(const AWorkStart: TDateTime;
      ACalendar: TdxGanttControlCalendar; AFormat: TdxDurationFormat)
      : TDateTime; overload;
    function GetWorkStart(const AWorkFinish: TDateTime;
      ACalendar: TdxGanttControlCalendar; AFormat: TdxDurationFormat)
      : TDateTime; overload;

    class function IsElapsedFormat(AFormat: TdxDurationFormat): Boolean; static;

    function IsNegative: Boolean;
    function IsNull: Boolean;
    function IsZero: Boolean;

    property Hour: Word read FHour;
    property Minute: Word read FMinute;
    property Second: Word read FSecond;

    property ToString: string read FDuration;

    property ToSeconds: Int64 read GetAsSeconds;
    property ToMinutes: Double read GetAsMinutes;
    property ToHours: Double read GetAsHours;
    property ToDays: Double read GetAsDays;
    property ToWeeks: Double read GetAsWeeks;
    property ToMonths: Double read GetAsMonths;

    property ToElapsedMinutes: Double read GetAsMinutes;
    property ToElapsedHours: Double read GetAsHours;
    property ToElapsedDays: Double read GetAsElapsedDays;
    property ToElapsedWeeks: Double read GetAsElapsedWeeks;
    property ToElapsedMonths: Double read GetAsElapsedMonths;
  end;
{$ENDREGION}
  { TdxGanttControlCalendarWeekDayWorkTime }

  TdxGanttControlCalendarWeekDayWorkTime = class
    (TdxGanttControlModelElementListItem)
  strict private
    FFromTime: TTime;
    FToTime: TTime;
    procedure SetFromTime(const Value: TTime);
    procedure SetToTime(const Value: TTime);
  protected
    procedure DoReset; override;
  public
    procedure Assign(Source: TPersistent); override;

    property FromTime: TTime read FFromTime write SetFromTime;
    property ToTime: TTime read FToTime write SetToTime;
  end;

  { TdxGanttControlCalendarWeekDayWorkTimes }

  TdxGanttControlCalendarWeekDayWorkTimes = class
    (TdxGanttControlElementCustomOwnedList)
  strict private
    FIsSorted: Boolean;
    function GetItem(Index: Integer)
      : TdxGanttControlCalendarWeekDayWorkTime; inline;
  protected
    function CreateItem: TdxGanttControlModelElementListItem; override;
    procedure DoItemChanged
      (AItem: TdxGanttControlModelElementListItem); override;
    procedure DoSort; virtual;

    function IsWorkTime(ADateTime: TDateTime): Boolean;
  public
    function Append: TdxGanttControlCalendarWeekDayWorkTime;
    procedure Clear;
    procedure Remove(AItem: TdxGanttControlCalendarWeekDayWorkTime);

    property Items[Index: Integer]: TdxGanttControlCalendarWeekDayWorkTime
      read GetItem; default;
  end;

  { TdxGanttControlCalendarWeekDay }

  TdxGanttCalendarWeekDayAssignedValue = (DayType, Workday);
  TdxGanttCalendarWeekDayAssignedValues = set of
    TdxGanttCalendarWeekDayAssignedValue;

  TdxGanttControlCalendarWeekDay = class(TdxGanttControlModelElementListItem)
  strict private
    FAssignedValues: TdxGanttCalendarWeekDayAssignedValues;
    FDayType: TdxGanttControlWeekDayType;
    FWorkday: Boolean;
    FTimePeriod: TdxGanttControlTimePeriod;
    FWorkTimes: TdxGanttControlCalendarWeekDayWorkTimes;

    function InternalGetOwner: TdxGanttControlCalendarWeekDays; inline;
    procedure SetDayType(const Value: TdxGanttControlWeekDayType);
    procedure SetWorkday(const Value: Boolean);
    procedure SetTimePeriod(const Value: TdxGanttControlTimePeriod);
    procedure SetWorkTimes(const Value
      : TdxGanttControlCalendarWeekDayWorkTimes);
  protected
    procedure DoReset; override;
  public
    constructor Create(AOwner: TdxGanttControlModelElement); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
{$REGION 'for internal use'}
    function IsValueAssigned(const AValue
      : TdxGanttCalendarWeekDayAssignedValue): Boolean;
    procedure ResetValue(const AValue
      : TdxGanttCalendarWeekDayAssignedValue); overload;
{$ENDREGION 'for internal use'}
    procedure CreateTimePeriod;
    procedure DestroyTimePeriod;

    property Owner: TdxGanttControlCalendarWeekDays read InternalGetOwner;
    // for internal use

    property DayType: TdxGanttControlWeekDayType read FDayType write SetDayType;
    property Workday: Boolean read FWorkday write SetWorkday;
    property TimePeriod: TdxGanttControlTimePeriod read FTimePeriod
      write SetTimePeriod;
    property WorkTimes: TdxGanttControlCalendarWeekDayWorkTimes read FWorkTimes
      write SetWorkTimes;
  end;

  { TdxGanttControlCalendarWeekDays }

  TdxGanttControlCalendarWeekDays = class(TdxGanttControlElementCustomOwnedList)
  strict private
    FDefaultWorkdayPattern: TdxGanttControlCalendarWeekDay;

    function GetDefaultWorkdayPattern: TdxGanttControlCalendarWeekDay;
    function GetItem(Index: Integer): TdxGanttControlCalendarWeekDay; inline;
  protected
    function CreateItem: TdxGanttControlModelElementListItem; override;

    function GetFinishWorkTime(ADate: TDateTime;
      var APattern: TdxGanttControlCalendarWeekDay): TDateTime;
    function GetStartWorkTime(ADate: TDateTime;
      var APattern: TdxGanttControlCalendarWeekDay): TDateTime;
    function IsNormalOrExceptionWorkday(ADate: TDate; AStart, AFinish: TTime;
      var APattern: TdxGanttControlCalendarWeekDay): Boolean;
    function IsWorkday(ADate: TDate;
      var APattern: TdxGanttControlCalendarWeekDay): Boolean;
    function IsWorkTime(ADateTime: TDateTime;
      var APattern: TdxGanttControlCalendarWeekDay): Boolean;

    property DefaultWorkdayPattern: TdxGanttControlCalendarWeekDay
      read GetDefaultWorkdayPattern;
  public
    function Append: TdxGanttControlCalendarWeekDay;
    procedure Clear;
    procedure Remove(AItem: TdxGanttControlCalendarWeekDay);

    property Items[Index: Integer]: TdxGanttControlCalendarWeekDay
      read GetItem; default;
  end;

{$REGION 'for internal use'}
  { TdxGanttControlCalendarException }

  TdxGanttControlCalendarMonth = (January, February, March, April, May, June,
    July, August, September, October, November, December);

  TdxGanttCalendarExceptionAssignedValue = (DaysOfWeek, Workday,
    EnteredByOccurrences, Month, MonthDay, MonthItem, MonthPosition, Name,
    Occurrences, Period, &Type);
  TdxGanttCalendarExceptionAssignedValues = set of
    TdxGanttCalendarExceptionAssignedValue;

  TdxGanttControlCalendarException = class(TdxGanttControlModelElementListItem)
  strict private
    FAssignedValues: TdxGanttCalendarExceptionAssignedValues;
    FDaysOfWeek: TdxGanttControlExceptionDaysOfWeek;
    FWorkday: Boolean;
    FEnteredByOccurrences: Boolean;
    FMonth: TdxGanttControlCalendarMonth;
    FMonthDay: Integer;
    FMonthItem: TdxGanttControlExceptionMonthItem;
    FMonthPosition: TdxGanttControlExceptionMonthPosition;
    FName: string;
    FOccurrences: Integer;
    FPeriod: Integer;
    FTimePeriod: TdxGanttControlTimePeriod;
    FType: TdxGanttControlExceptionType;
    FWorkTimes: TdxGanttControlCalendarWeekDayWorkTimes;

    procedure SetDaysOfWeek(const Value: TdxGanttControlExceptionDaysOfWeek);
    procedure SetWorkday(const Value: Boolean);
    procedure SetEnteredByOccurrences(const Value: Boolean);
    procedure SetMonth(const Value: TdxGanttControlCalendarMonth);
    procedure SetMonthDay(const Value: Integer);
    procedure SetMonthItem(const Value: TdxGanttControlExceptionMonthItem);
    procedure SetMonthPosition(const Value
      : TdxGanttControlExceptionMonthPosition);
    procedure SetName(const Value: string);
    procedure SetOccurrences(const Value: Integer);
    procedure SetPeriod(const Value: Integer);
    procedure SetTimePeriod(const Value: TdxGanttControlTimePeriod);
    procedure SetType(const Value: TdxGanttControlExceptionType);
    procedure SetWorkTimes(const Value
      : TdxGanttControlCalendarWeekDayWorkTimes);
  protected
    procedure DoReset; override;
  public
    constructor Create(AOwner: TdxGanttControlModelElement); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
{$REGION 'for internal use'}
    function IsValueAssigned(const AValue
      : TdxGanttCalendarExceptionAssignedValue): Boolean;
    procedure ResetValue(const AValue
      : TdxGanttCalendarExceptionAssignedValue); overload;
{$ENDREGION 'for internal use'}
    procedure CreateTimePeriod;
    procedure DestroyTimePeriod;

    property DaysOfWeek: TdxGanttControlExceptionDaysOfWeek read FDaysOfWeek
      write SetDaysOfWeek;
    property Workday: Boolean read FWorkday write SetWorkday;
    property EnteredByOccurrences: Boolean read FEnteredByOccurrences
      write SetEnteredByOccurrences;
    property Month: TdxGanttControlCalendarMonth read FMonth write SetMonth;
    property MonthDay: Integer read FMonthDay write SetMonthDay;
    property MonthItem: TdxGanttControlExceptionMonthItem read FMonthItem
      write SetMonthItem;
    property MonthPosition: TdxGanttControlExceptionMonthPosition
      read FMonthPosition write SetMonthPosition;
    property Name: string read FName write SetName;
    property Occurrences: Integer read FOccurrences write SetOccurrences;
    property Period: Integer read FPeriod write SetPeriod;
    property TimePeriod: TdxGanttControlTimePeriod read FTimePeriod
      write SetTimePeriod;
    property &Type: TdxGanttControlExceptionType read FType write SetType;
    property WorkTimes: TdxGanttControlCalendarWeekDayWorkTimes read FWorkTimes
      write SetWorkTimes;
  end;

  { TdxGanttControlCalendarExceptions }

  TdxGanttControlCalendarExceptions = class
    (TdxGanttControlElementCustomOwnedList)
  strict private
    function GetItem(Index: Integer): TdxGanttControlCalendarException; inline;
  protected
    function CreateItem: TdxGanttControlModelElementListItem; override;
  public
    function Append: TdxGanttControlCalendarException;
    procedure Clear;
    procedure Remove(AItem: TdxGanttControlCalendarException);

    property Items[Index: Integer]: TdxGanttControlCalendarException
      read GetItem; default;
  end;
{$ENDREGION}
  { TdxGanttControlCalendarWorkWeek }

  TdxGanttCalendarWorkWeekAssignedValue = (Name, TimePeriod);
  TdxGanttCalendarWorkWeekAssignedValues = set of
    TdxGanttCalendarWorkWeekAssignedValue;

  TdxGanttControlCalendarWorkWeek = class(TdxGanttControlModelElementListItem)
  strict private
    FAssignedValues: TdxGanttCalendarWorkWeekAssignedValues;
    FTimePeriod: TdxGanttControlTimePeriod;
    FName: string;
    FWeekDays: TdxGanttControlCalendarWeekDays;
    procedure SetName(const Value: string);
    procedure SetTimePeriod(const Value: TdxGanttControlTimePeriod);
    procedure SetWeekDays(const Value: TdxGanttControlCalendarWeekDays);
  protected
    procedure DoReset; override;
  public
    constructor Create(AOwner: TdxGanttControlModelElement); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
{$REGION 'for internal use'}
    function IsValueAssigned(const AValue
      : TdxGanttCalendarWorkWeekAssignedValue): Boolean;
    procedure ResetValue(const AValue
      : TdxGanttCalendarWorkWeekAssignedValue); overload;
{$ENDREGION 'for internal use'}
    procedure CreateTimePeriod;
    procedure DestroyTimePeriod;

    property TimePeriod: TdxGanttControlTimePeriod read FTimePeriod
      write SetTimePeriod;
    property Name: string read FName write SetName;
    property WeekDays: TdxGanttControlCalendarWeekDays read FWeekDays
      write SetWeekDays;
  end;

  { TdxGanttControlCalendarWorkWeeks }

  TdxGanttControlCalendarWorkWeeks = class
    (TdxGanttControlElementCustomOwnedList)
  strict private
    function GetItem(Index: Integer): TdxGanttControlCalendarWorkWeek; inline;
  protected
    function CreateItem: TdxGanttControlModelElementListItem; override;
  public
    function Append: TdxGanttControlCalendarWorkWeek;
    procedure Clear;
    procedure Remove(AItem: TdxGanttControlCalendarWorkWeek);

    property Items[Index: Integer]: TdxGanttControlCalendarWorkWeek
      read GetItem; default;
  end;

  { TdxGanttControlCalendar }

  TdxGanttCalendarAssignedValue = (BaselineCalendar, Name);
  TdxGanttCalendarAssignedValues = set of TdxGanttCalendarAssignedValue;

  TdxGanttControlCalendar = class(TdxGanttControlModelUIDElement)
  strict private
    FAssignedValues: TdxGanttCalendarAssignedValues;
    FBaseCalendarUID: Integer;
    FExceptions: TdxGanttControlCalendarExceptions;
    FGUID: string;
    FBaselineCalendar: Boolean;
    FName: string;
    FWeekDays: TdxGanttControlCalendarWeekDays;
    FWorkWeeks: TdxGanttControlCalendarWorkWeeks;

    function InternalGetOwner: TdxGanttControlCalendars; inline;
    function GetBaseCalendar: TdxGanttControlCalendar;
    function GetIsBaseCalendar: Boolean;
    procedure SetBaseCalendar(const Value: TdxGanttControlCalendar);
    procedure SetBaseCalendarUID(const Value: Integer);
    procedure SetExceptions(const Value: TdxGanttControlCalendarExceptions);
    procedure SetBaselineCalendar(const Value: Boolean);
    procedure SetName(const Value: string);
    procedure SetWeekDays(const Value: TdxGanttControlCalendarWeekDays);
    procedure SetWorkWeeks(const Value: TdxGanttControlCalendarWorkWeeks);
  protected
    procedure DoReset; override;

    function DoGetFinishWorkTime(ADate: TDateTime;
      var APattern: TdxGanttControlCalendarWeekDay): TDateTime;
    function DoGetStartWorkTime(ADate: TDateTime;
      var APattern: TdxGanttControlCalendarWeekDay): TDateTime;

    function IsNormalOrExceptionWorkday(ADate: TDate; AStart, AFinish: TTime;
      var APattern: TdxGanttControlCalendarWeekDay): Boolean;
    function IsValidNormalOrExceptionWorkday(ADate: TDate;
      AStart, AFinish: TTime;
      var APattern: TdxGanttControlCalendarWeekDay): Boolean;

    property GUID: string read FGUID write FGUID;
  public
    constructor Create(AOwner: TdxGanttControlModelElement); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
{$REGION 'for internal use'}
    function IsValueAssigned(const AValue
      : TdxGanttCalendarAssignedValue): Boolean;
    procedure ResetValue(const AValue: TdxGanttCalendarAssignedValue); overload;
{$ENDREGION 'for internal use'}
{$REGION 'for internal use'}
    function GetFinishWorkTime(ADate: TDateTime): TDateTime;
    function GetNextWorkTime(ADateTime: TDateTime): TDateTime;
    function GetPreviousWorkTime(ADateTime: TDateTime): TDateTime;
    function GetStartWorkTime(ADate: TDateTime): TDateTime;
    property Exceptions: TdxGanttControlCalendarExceptions read FExceptions
      write SetExceptions;
    property BaselineCalendar: Boolean read FBaselineCalendar
      write SetBaselineCalendar;
{$ENDREGION}
    function IsWorkday(ADate: TDate): Boolean; overload;
    function IsWorkTime(ADateTime: TDateTime): Boolean;

    property Owner: TdxGanttControlCalendars read InternalGetOwner;
    // for internal use

    property BaseCalendar: TdxGanttControlCalendar read GetBaseCalendar
      write SetBaseCalendar;
    property BaseCalendarUID: Integer read FBaseCalendarUID
      write SetBaseCalendarUID;
    property IsBaseCalendar: Boolean read GetIsBaseCalendar;
    property Name: string read FName write SetName;
    property WeekDays: TdxGanttControlCalendarWeekDays read FWeekDays
      write SetWeekDays;
    property WorkWeeks: TdxGanttControlCalendarWorkWeeks read FWorkWeeks
      write SetWorkWeeks;
  end;

  { TdxGanttControlCalendars }

  TdxGanttControlCalendars = class(TdxGanttControlModelUIDElementList)
  strict private
  type
    TCreateStandardCalendarEvent = procedure(Sender: TObject;
      ACalendar: TdxGanttControlCalendar) of object;
    TCreateStandardCalendarHandlers =
      TdxMulticastMethod<TCreateStandardCalendarEvent>;
  public const
    StandardCalendarWorkTimeFrom1 = 8 / 24;
    StandardCalendarWorkTimeTo1 = 12 / 24;
    StandardCalendarWorkTimeFrom2 = 13 / 24;
    StandardCalendarWorkTimeTo2 = 17 / 24;

    H24CalendarWorkTimeFrom = 0;
    H24CalendarWorkTimeTo = dxEndOfDay;

    NightCalendarWorkTimeFrom1 = 0;
    NightCalendarWorkTimeTo1 = 3 / 24;
    NightCalendarWorkTimeFrom2 = 4 / 24;
    NightCalendarWorkTimeTo2 = 8 / 24;
    NightCalendarWorkTimeFrom3 = 23 / 24;
    NightCalendarWorkTimeTo3 = dxEndOfDay;
  strict private
    FCreateStandardCalendarHandlers: TCreateStandardCalendarHandlers;
    FStandardCalendar: TdxGanttControlCalendar;

    function GetItem(Index: Integer): TdxGanttControlCalendar; inline;
  protected
    function CreateItem: TdxGanttControlModelElementListItem; override;

    function CreateStandardCalendar: TdxGanttControlCalendar; virtual;
    procedure DoReset; override;
    procedure DoItemChanged
      (AItem: TdxGanttControlModelElementListItem); override;
  public
    procedure Assign(Source: TPersistent); override;

    function Append: TdxGanttControlCalendar;
    function Create24HoursCalendar: TdxGanttControlCalendar; virtual;
    function CreateNightCalendar: TdxGanttControlCalendar; virtual;
    function GetCalendarByName(const Value: string): TdxGanttControlCalendar;
    function GetCalendarByUID(const AUID: Integer): TdxGanttControlCalendar;
    procedure Remove(AItem: TdxGanttControlCalendar);

{$REGION 'for internal use'}
    property CreateStandardCalendarHandlers: TCreateStandardCalendarHandlers
      read FCreateStandardCalendarHandlers;
{$ENDREGION}
    property Items[Index: Integer]: TdxGanttControlCalendar
      read GetItem; default;
    property StandardCalendar: TdxGanttControlCalendar read FStandardCalendar;
  end;

function dxGanttControlExceptionDaysOfWeekToInteger
  (ADaysOfWeek: TdxGanttControlExceptionDaysOfWeek): Integer;
function dxIntegerToGanttControlExceptionDaysOfWeek(const Value: Integer)
  : TdxGanttControlExceptionDaysOfWeek;

implementation

uses
  Math,
  RTLConsts,
  cxDateUtils,
  dxGanttControlUtils, dxGanttControlDataModel, dxGanttControlStrs;

const
  dxEpsilon = OneMillisecond / 10;

type
  TdxGanttControlCustomElementAccess = class(TdxGanttControlModelElement);
  TdxGanttControlDataModelAccess = class(TdxGanttControlDataModel);

function dxIsEndOfDay(ATime: TTime): Boolean; inline;
begin
  Result := Abs(ATime - dxEndOfDay) <= dxEpsilon
end;

function dxGanttControlExceptionDaysOfWeekToInteger
  (ADaysOfWeek: TdxGanttControlExceptionDaysOfWeek): Integer;
var
  AItem: TdxGanttControlExceptionDayOfWeek;
begin
  Result := 0;
  for AItem := Low(TdxGanttControlExceptionDayOfWeek)
    to High(TdxGanttControlExceptionDayOfWeek) do
  begin
    Result := Result * 2;
    if AItem in ADaysOfWeek then
      Inc(Result);
  end;
end;

function dxIntegerToGanttControlExceptionDaysOfWeek(const Value: Integer)
  : TdxGanttControlExceptionDaysOfWeek;
var
  AItem: TdxGanttControlExceptionDayOfWeek;
  AValue: Integer;
begin
  Result := [];
  AValue := Value;
  for AItem := High(TdxGanttControlExceptionDayOfWeek)
    downto Low(TdxGanttControlExceptionDayOfWeek) do
  begin
    if AValue mod 2 = 1 then
      Include(Result, AItem);
    AValue := AValue div 2;
  end;
end;

procedure ExchangeDateTimes(var AValue1, AValue2: TDateTime);
var
  dt: TDateTime;
begin
  dt := AValue1;
  AValue1 := AValue2;
  AValue2 := dt;
end;

{ TdxGanttControlDuration }

class function TdxGanttControlDuration.Create(const AStart, AFinish: TDateTime;
  ACalendar: TdxGanttControlCalendar; AFormat: TdxDurationFormat)
  : TdxGanttControlDuration;
begin
  Result := TdxGanttControlDuration.Create(AStart, AFinish, ACalendar,
    TdxGanttControlDuration.IsElapsedFormat(AFormat));
end;

class function TdxGanttControlDuration.Create(const AStart, AFinish: TDateTime;
  ACalendar: TdxGanttControlCalendar; AIsElapsed: Boolean = False)
  : TdxGanttControlDuration;
var
  dt: TDateTime;
begin
  if AIsElapsed then
  begin
    dt := Abs(AFinish - AStart);
    Result.FHour := Trunc(dt / OneHour);
    dt := dt - Result.FHour * OneHour;
    Result.FMinute := Trunc(dt / OneMinute);
    dt := dt - Result.FMinute * OneMinute;
    Result.FSecond := Round(dt / OneSecond);
    if Result.FSecond = 60 then
    begin
      Inc(Result.FMinute);
      Result.FSecond := 0;
    end;
    if Result.FMinute = 60 then
    begin
      Inc(Result.FHour);
      Result.FMinute := 0;
    end;
  end
  else
    CalculateWorkTime(AStart, AFinish, ACalendar, Result.FHour, Result.FMinute,
      Result.FSecond);
  Result.FDuration := GetDurationString(Result.FHour, Result.FMinute,
    Result.FSecond, AStart > AFinish);
end;

class function TdxGanttControlDuration.Create(const ADuration: string)
  : TdxGanttControlDuration;
var
  AIsNegative: Boolean;
  st: string;
begin
  st := Trim(ADuration);
  if st = '' then
  begin
    Result.FHour := 0;
    Result.FMinute := 0;
    Result.FSecond := 0;
    Result.FDuration := '';
  end
  else if Decode(st, Result.FHour, Result.FMinute, Result.FSecond, AIsNegative)
  then
    Result.FDuration := st
  else
    raise EdxGanttControlException.Create
      (Format(cxGetResourceString(@sdxGanttControlExceptionInvalidDuration),
      [ADuration]));
end;

class function TdxGanttControlDuration.Create(AValue: Double;
  AFormat: TdxDurationFormat): TdxGanttControlDuration;
var
  ATotalSecond: Int64;
begin
  case AFormat of
    TdxDurationFormat.Minutes, TdxDurationFormat.ElapsedMinutes,
      TdxDurationFormat.EstimatedMinutes,
      TdxDurationFormat.EstimatedElapsedMinutes:
      ATotalSecond := Round(AValue * 60);
    TdxDurationFormat.Hours, TdxDurationFormat.ElapsedHours,
      TdxDurationFormat.EstimatedHours, TdxDurationFormat.EstimatedElapsedHours:
      ATotalSecond := Round(AValue * 3600);
    TdxDurationFormat.Days, TdxDurationFormat.Null,
      TdxDurationFormat.EstimatedDays, TdxDurationFormat.EstimatedNull:
      ATotalSecond := Round(AValue * 28800);
    TdxDurationFormat.ElapsedDays, TdxDurationFormat.EstimatedElapsedDays:
      ATotalSecond := Round(AValue * 86400);
    TdxDurationFormat.Weeks, TdxDurationFormat.EstimatedWeeks:
      ATotalSecond := Round(AValue * 144000);
    TdxDurationFormat.ElapsedWeeks, TdxDurationFormat.EstimatedElapsedWeeks:
      ATotalSecond := Round(AValue * 604800);
    TdxDurationFormat.Months, TdxDurationFormat.EstimatedMonths:
      ATotalSecond := Round(AValue * 576000);
    TdxDurationFormat.ElapsedMonths, TdxDurationFormat.EstimatedElapsedMonths:
      ATotalSecond := Round(AValue * 2592000);
  else
    raise EdxGanttControlException.Create
      (cxGetResourceString(@sdxGanttControlExceptionInvalidDurationFormat));
  end;
  Result.FHour := ATotalSecond div 3600;
  ATotalSecond := ATotalSecond mod 3600;
  Result.FMinute := ATotalSecond div 60;
  Result.FSecond := ATotalSecond mod 60;
  Result.FDuration := GetDurationString(Result.FHour, Result.FMinute,
    Result.FSecond, AValue < 0);
end;

class function TdxGanttControlDuration.Decode(const ADuration: string;
  var AHour, AMinute, ASecond: Word; var AIsNegative: Boolean): Boolean;
var
  L, PH, PM: Integer;
  st: string;
  AIntHour, AIntMin, AIntSec: Integer;
begin
  st := Trim(ADuration);
  AIsNegative := (Length(st) > 1) and (st[1] = '-');
  if AIsNegative then
    Delete(st, 1, 1);
  L := Length(st);
  Result := (L >= 8) and (st[L] = 'S') and SameText('PT', Copy(st, 1, 2));
  if Result then
  begin
    PH := Pos('H', st);
    if (PH >= 4) and CharInSet(st[3], ['0' .. '9']) and
      TryStrToInt(Copy(st, 3, PH - 3), AIntHour) then
    begin
      PM := Pos('M', st);
      if (PM >= PH + 2) and CharInSet(st[PH + 1], ['0' .. '9']) and
        TryStrToInt(Copy(st, PH + 1, PM - (PH + 1)), AIntMin) then
        Result := TryStrToInt(Copy(st, PM + 1, L - (PM + 1)), AIntSec);
    end;
    if Result then
    begin
      AHour := AIntHour;
      AMinute := AIntMin;
      ASecond := AIntSec;
    end;
  end;
end;

class function TdxGanttControlDuration.CalculateDayWorkTime(ADay: TDate;
  AStart, AFinish: TTime; ACalendar: TdxGanttControlCalendar;
  var AHour, AMinute, ASecond: Word): Boolean;
var
  APattern: TdxGanttControlCalendarWeekDay;
  AWorkTime: TDateTime;
  AMSecond: Word;

  function PriorPeriodIsEarlyStart(ACurrentPeriodNumber: Integer): Boolean;
  begin
    Result := (ACurrentPeriodNumber = 0) or
      (AStart - APattern.WorkTimes[ACurrentPeriodNumber - 1].ToTime >
      dxEpsilon);
  end;

  function NextPeriodIsLaterFinish(ACurrentPeriodNumber: Integer): Boolean;
  begin
    Result := (ACurrentPeriodNumber = APattern.WorkTimes.Count - 1) or
      (APattern.WorkTimes[ACurrentPeriodNumber + 1].FromTime - AFinish >
      dxEpsilon);
  end;

  function InternalCalculateWorkTime: TDateTime;
  var
    I: Integer;
    AFromTime, AToTime: TDateTime;
  begin
    Result := 0;
    for I := 0 to APattern.WorkTimes.Count - 1 do
    begin
      ExtractWorkTime(APattern, I, AFromTime, AToTime);
      if ((AStart <= AFromTime) and (AFinish <= AFromTime) and
        PriorPeriodIsEarlyStart(I)) or (InRange(AStart, AFromTime, AToTime) and
        InRange(AFinish, AFromTime, AToTime)) or
        ((AStart >= AToTime) and (AFinish >= AToTime) and
        NextPeriodIsLaterFinish(I)) then
        if dxIsEndOfDay(AFinish) then
          Exit(1 - AStart)
        else
          Exit(AFinish - AStart);

      if AStart >= AToTime then
        Continue;

      if (AStart <= AFromTime) and (AFinish >= AToTime) then
        Result := Result + AToTime - AFromTime
      else if AStart >= AFromTime then
        Result := Result + AToTime - AStart
      else if AFinish <= AToTime then
        if dxIsEndOfDay(AFinish) then
          Result := Result + 1 - AFromTime
        else
          Result := Result + AFinish - AFromTime;

      if (AStart > 0) and (AStart < AFromTime) and PriorPeriodIsEarlyStart(I)
      then
        Result := Result + AFromTime - AStart;

      if not dxIsEndOfDay(AFinish) and (AFinish > AToTime) and
        NextPeriodIsLaterFinish(I) then
      begin
        Result := Result + AFinish - AToTime;
        Break;
      end;

      if AFinish <= AToTime then
        Break;
    end;
  end;

begin
  AHour := 0;
  AMinute := 0;
  ASecond := 0;
  Result := ACalendar.IsValidNormalOrExceptionWorkday(ADay, AStart, AFinish,
    APattern);
  if Result then
  begin
    AWorkTime := InternalCalculateWorkTime;
    if AWorkTime = 0 then
      Result := False
    else if AWorkTime = 1 then
      AHour := 24
    else
      DecodeTime(AWorkTime, AHour, AMinute, ASecond, AMSecond);
  end;
end;

class procedure TdxGanttControlDuration.CalculateWorkTime(AStart,
  AFinish: TDateTime; ACalendar: TdxGanttControlCalendar;
  var AHour, AMinute, ASecond: Word);

  procedure AddWorkTime(ADayHour, ADayMinute, ADaySecond: Word);
  begin
    Inc(ASecond, ADaySecond);
    Inc(AMinute, ASecond div 60);
    ASecond := ASecond mod 60;
    Inc(AMinute, ADayMinute);
    Inc(AHour, AMinute div 60);
    AMinute := AMinute mod 60;
    Inc(AHour, ADayHour);
  end;

var
  I: Integer;
  ADayHour, ADayMinute, ADaySecond: Word;
begin
  AHour := 0;
  AMinute := 0;
  ASecond := 0;
  if AStart <> AFinish then
  begin
    if AStart > AFinish then
      ExchangeDateTimes(AStart, AFinish);
    if Trunc(AStart) <> Trunc(AFinish) then
    begin
      if CalculateDayWorkTime(Trunc(AStart), Frac(AStart), dxEndOfDay,
        ACalendar, ADayHour, ADayMinute, ADaySecond) then
        AddWorkTime(ADayHour, ADayMinute, ADaySecond);
      for I := Trunc(AStart) + 1 to Trunc(AFinish) - 1 do
        if CalculateDayWorkTime(I, 0, dxEndOfDay, ACalendar, ADayHour,
          ADayMinute, ADaySecond) then
          AddWorkTime(ADayHour, ADayMinute, ADaySecond);
      if CalculateDayWorkTime(AFinish, 0, Frac(AFinish), ACalendar, ADayHour,
        ADayMinute, ADaySecond) then
        AddWorkTime(ADayHour, ADayMinute, ADaySecond);
    end
    else if CalculateDayWorkTime(Trunc(AStart), Frac(AStart), Frac(AFinish),
      ACalendar, ADayHour, ADayMinute, ADaySecond) then
      AddWorkTime(ADayHour, ADayMinute, ADaySecond);
  end;
end;

class procedure TdxGanttControlDuration.DecreaseTimeValue(var AValue: Double;
  const ADelta: Double);
begin
  AValue := AValue - ADelta;
  AValue := IfThen(AValue < dxEpsilon, 0, AValue);
end;

class procedure TdxGanttControlDuration.ExtractWorkTime
  (ADay: TdxGanttControlCalendarWeekDay; APeriodNumber: Integer;
  var AFromTime, AToTime: TDateTime);
begin
  AFromTime := ADay.WorkTimes[APeriodNumber].FromTime;
  AToTime := ADay.WorkTimes[APeriodNumber].ToTime;
  if (AToTime = 0) or dxIsEndOfDay(AToTime) then
    AToTime := 1;
end;

class function TdxGanttControlDuration.GetDurationString(AHour, AMinute,
  ASecond: Word; AIsNegative: Boolean): string;
begin
  Result := Format('PT%dH%dM%dS', [AHour, AMinute, ASecond]);
  if AIsNegative and not((AHour = 0) and (AMinute = 0) and (ASecond = 0)) then
    Result := '-' + Result;
end;

function TdxGanttControlDuration.GetAsSeconds: Int64;
begin
  Result := Hour * 60 * 60 + Minute * 60 + Second;
  if (Result > 0) and IsNegative then
    Result := -Result;
end;

function TdxGanttControlDuration.GetAsMinutes: Double;
begin
  Result := GetAsSeconds / 60;
end;

function TdxGanttControlDuration.GetAsHours: Double;
begin
  Result := GetAsSeconds / 3600;
end;

function TdxGanttControlDuration.GetAsDays: Double;
begin
  Result := GetAsSeconds / 28800;
end;

function TdxGanttControlDuration.GetAsWeeks: Double;
begin
  Result := GetAsSeconds / 144000;
end;

function TdxGanttControlDuration.GetAsMonths: Double;
begin
  Result := GetAsSeconds / 576000;
end;

function TdxGanttControlDuration.GetAsElapsedDays: Double;
begin
  Result := GetAsSeconds / 86400;
end;

function TdxGanttControlDuration.GetAsElapsedWeeks: Double;
begin
  Result := GetAsSeconds / 604800;
end;

function TdxGanttControlDuration.GetAsElapsedMonths: Double;
begin
  Result := GetAsSeconds / 2592000;
end;

class function TdxGanttControlDuration.GetDurationMeasurementUnit
  (AFormat: TdxDurationFormat; AIsPlural, ANeedPostfix: Boolean): string;
begin
  Result := '';
  case AFormat of
    TdxDurationFormat.Minutes, TdxDurationFormat.ElapsedMinutes,
      TdxDurationFormat.EstimatedMinutes,
      TdxDurationFormat.EstimatedElapsedMinutes:
      if not AIsPlural then
        Result := cxGetResourceString(@sdxGanttControlDurationFormatMinuteShort)
      else
        Result := cxGetResourceString
          (@sdxGanttControlDurationFormatMinutesShort);

    TdxDurationFormat.Hours, TdxDurationFormat.ElapsedHours,
      TdxDurationFormat.EstimatedHours, TdxDurationFormat.EstimatedElapsedHours:
      if not AIsPlural then
        Result := cxGetResourceString(@sdxGanttControlDurationFormatHourShort)
      else
        Result := cxGetResourceString(@sdxGanttControlDurationFormatHoursShort);

    TdxDurationFormat.Days, TdxDurationFormat.ElapsedDays,
      TdxDurationFormat.Null, TdxDurationFormat.EstimatedDays,
      TdxDurationFormat.EstimatedElapsedDays, TdxDurationFormat.EstimatedNull:
      if not AIsPlural then
        Result := cxGetResourceString(@sdxGanttControlDurationFormatDay)
      else
        Result := cxGetResourceString(@sdxGanttControlDurationFormatDays);

    TdxDurationFormat.Weeks, TdxDurationFormat.ElapsedWeeks,
      TdxDurationFormat.EstimatedWeeks, TdxDurationFormat.EstimatedElapsedWeeks:
      if not AIsPlural then
        Result := cxGetResourceString(@sdxGanttControlDurationFormatWeekShort)
      else
        Result := cxGetResourceString(@sdxGanttControlDurationFormatWeeksShort);

    TdxDurationFormat.Months, TdxDurationFormat.ElapsedMonths,
      TdxDurationFormat.EstimatedMonths,
      TdxDurationFormat.EstimatedElapsedMonths:
      if not AIsPlural then
        Result := cxGetResourceString(@sdxGanttControlDurationFormatMonthShort)
      else
        Result := cxGetResourceString
          (@sdxGanttControlDurationFormatMonthsShort);
  else
    raise EdxGanttControlException.Create
      (cxGetResourceString(@sdxGanttControlExceptionInvalidDurationFormat));
  end;
  if AFormat in [TdxDurationFormat.ElapsedMinutes,
    TdxDurationFormat.EstimatedElapsedMinutes, TdxDurationFormat.ElapsedHours,
    TdxDurationFormat.EstimatedElapsedHours, TdxDurationFormat.ElapsedDays,
    TdxDurationFormat.EstimatedElapsedDays, TdxDurationFormat.ElapsedWeeks,
    TdxDurationFormat.EstimatedElapsedWeeks, TdxDurationFormat.ElapsedMonths,
    TdxDurationFormat.EstimatedElapsedMonths] then
    Result := cxGetResourceString
      (@sdxGanttControlDurationFormatElapsedTimePrefix) + Result;

  if ANeedPostfix and
    (AFormat in [TdxDurationFormat.EstimatedMinutes .. TdxDurationFormat.
    EstimatedNull]) then
    Result := Result + cxGetResourceString
      (@sdxGanttControlDurationFormatEstimatedTimePostfix);
end;

class function TdxGanttControlDuration.GetDurationValue(const ADuration: string;
  AFormat: TdxDurationFormat): Double;
begin
  case AFormat of
    TdxDurationFormat.Minutes, TdxDurationFormat.EstimatedMinutes:
      Result := TdxGanttControlDuration.Create(ADuration).ToMinutes;
    TdxDurationFormat.Hours, TdxDurationFormat.EstimatedHours:
      Result := TdxGanttControlDuration.Create(ADuration).ToHours;
    TdxDurationFormat.Days, TdxDurationFormat.EstimatedDays,
      TdxDurationFormat.Null, TdxDurationFormat.EstimatedNull:
      Result := TdxGanttControlDuration.Create(ADuration).ToDays;
    TdxDurationFormat.Weeks, TdxDurationFormat.EstimatedWeeks:
      Result := TdxGanttControlDuration.Create(ADuration).ToWeeks;
    TdxDurationFormat.Months, TdxDurationFormat.EstimatedMonths:
      Result := TdxGanttControlDuration.Create(ADuration).ToMonths;
    TdxDurationFormat.ElapsedMinutes, TdxDurationFormat.EstimatedElapsedMinutes:
      Result := TdxGanttControlDuration.Create(ADuration).ToElapsedMinutes;
    TdxDurationFormat.ElapsedHours, TdxDurationFormat.EstimatedElapsedHours:
      Result := TdxGanttControlDuration.Create(ADuration).ToElapsedHours;
    TdxDurationFormat.ElapsedDays, TdxDurationFormat.EstimatedElapsedDays:
      Result := TdxGanttControlDuration.Create(ADuration).ToElapsedDays;
    TdxDurationFormat.ElapsedWeeks, TdxDurationFormat.EstimatedElapsedWeeks:
      Result := TdxGanttControlDuration.Create(ADuration).ToElapsedWeeks;
    TdxDurationFormat.ElapsedMonths, TdxDurationFormat.EstimatedElapsedMonths:
      Result := TdxGanttControlDuration.Create(ADuration).ToElapsedMonths;
  else
    raise EdxGanttControlException.Create
      (cxGetResourceString(@sdxGanttControlExceptionInvalidDurationFormat));
  end;
end;

function TdxGanttControlDuration.GetWorkFinish(const AWorkStart: TDateTime;
  ACalendar: TdxGanttControlCalendar; AIsElapsed: Boolean = False): TDateTime;
var
  AWorkDuration: Double;
  ADate: TDate;
  APattern: TdxGanttControlCalendarWeekDay;
  AFromTime, AToTime, AStart, dt: TDateTime;
  I: Integer;
begin
  if AIsElapsed then
    Exit(AWorkStart + Hour div 24 + EncodeTime(Hour mod 24, Minute, Second, 0));

  Result := AWorkStart;
  if IsNull then
    Exit;
  AWorkDuration := Hour div 24 + EncodeTime(Hour mod 24, Minute, Second, 0);
  ADate := Trunc(AWorkStart);
  AStart := Frac(AWorkStart);
  while AWorkDuration > 0 do
  begin
    if ACalendar.IsValidNormalOrExceptionWorkday(ADate, AStart, dxEndOfDay,
      APattern) then
      for I := 0 to APattern.WorkTimes.Count - 1 do
      begin
        ExtractWorkTime(APattern, I, AFromTime, AToTime);
        if (AStart > 0) and
          ((AStart < AFromTime) or ((AStart >= AToTime) and
          (I = APattern.WorkTimes.Count - 1))) then
        begin
          if AStart < AFromTime then
            dt := Min(AWorkDuration, AFromTime - AStart)
          else
            dt := Min(AWorkDuration, 1 - AStart);
          Result := Result + dt;
          DecreaseTimeValue(AWorkDuration, dt);
          AStart := 0;
        end;
        if AWorkDuration = 0 then
          Break;

        if Result < ADate + AToTime then
        begin
          if Result <= ADate + AFromTime then
          begin
            dt := Min(AWorkDuration, AToTime - AFromTime);
            Result := ADate + Min(AFromTime + dt, dxEndOfDay);
          end
          else
          begin
            dt := Min(AWorkDuration, AToTime - TimeOf(Result));
            Result := ADate + Min(TimeOf(Result) + dt, dxEndOfDay);
          end;
          DecreaseTimeValue(AWorkDuration, dt);
          AStart := IfThen(AStart < AToTime, 0, AStart);
        end;
        if AWorkDuration = 0 then
          Break;
      end;
    ADate := ADate + 1;
    AStart := 0;
  end;
end;

function TdxGanttControlDuration.GetWorkFinish(const AWorkStart: TDateTime;
  ACalendar: TdxGanttControlCalendar; AFormat: TdxDurationFormat): TDateTime;
begin
  Result := GetWorkFinish(AWorkStart, ACalendar, IsElapsedFormat(AFormat));
end;

function TdxGanttControlDuration.GetWorkStart(const AWorkFinish: TDateTime;
  ACalendar: TdxGanttControlCalendar; AIsElapsed: Boolean = False): TDateTime;
var
  AWorkDuration: Double;
  ADate: TDate;
  APattern: TdxGanttControlCalendarWeekDay;
  AFromTime, AToTime, AFinish, dt: TDateTime;
  I: Integer;
begin
  if AIsElapsed then
    Exit(AWorkFinish - Hour div 24 - EncodeTime(Hour mod 24, Minute,
      Second, 0));

  Result := AWorkFinish;
  if IsNull then
    Exit;
  AWorkDuration := Hour div 24 + EncodeTime(Hour mod 24, Minute, Second, 0);
  ADate := Trunc(AWorkFinish);
  AFinish := Frac(AWorkFinish);
  while AWorkDuration > 0 do
  begin
    if ADate < TdxGanttControlDataModel.MinDate then
      Exit(TdxGanttControlDataModel.MinDate);
    if ACalendar.IsValidNormalOrExceptionWorkday(ADate, 0, AFinish, APattern)
    then
      for I := APattern.WorkTimes.Count - 1 downto 0 do
      begin
        ExtractWorkTime(APattern, I, AFromTime, AToTime);
        if (AFinish < 1) and
          ((AFinish > AToTime) or ((AFinish <= AFromTime) and (I = 0))) then
        begin
          if AFinish > AToTime then
            dt := Min(AWorkDuration, AFinish - AToTime)
          else
            dt := Min(AWorkDuration, AFinish);
          Result := Result - dt;
          DecreaseTimeValue(AWorkDuration, dt);
          AFinish := 1;
        end;
        if AWorkDuration = 0 then
          Break;

        if Result > ADate + AFromTime then
        begin
          if Result >= ADate + AToTime then
          begin
            dt := Min(AWorkDuration, AToTime - AFromTime);
            Result := ADate + (AToTime - dt);
          end
          else
          begin
            dt := Min(AWorkDuration, TimeOf(Result) - AFromTime);
            Result := ADate + (TimeOf(Result) - dt);
          end;
          DecreaseTimeValue(AWorkDuration, dt);
          AFinish := IfThen(AFinish > AFromTime, 1, AFinish);
        end;
        if AWorkDuration = 0 then
          Break;
      end;
    ADate := ADate - 1;
    AFinish := 1;
  end;
end;

function TdxGanttControlDuration.GetWorkStart(const AWorkFinish: TDateTime;
  ACalendar: TdxGanttControlCalendar; AFormat: TdxDurationFormat): TDateTime;
begin
  Result := GetWorkStart(AWorkFinish, ACalendar, IsElapsedFormat(AFormat));
end;

class function TdxGanttControlDuration.IsElapsedFormat
  (AFormat: TdxDurationFormat): Boolean;
begin
  Result := AFormat in [TdxDurationFormat.ElapsedMinutes,
    TdxDurationFormat.ElapsedHours, TdxDurationFormat.ElapsedDays,
    TdxDurationFormat.ElapsedWeeks, TdxDurationFormat.ElapsedMonths,
    TdxDurationFormat.ElapsedPercent, TdxDurationFormat.EstimatedElapsedMinutes,
    TdxDurationFormat.EstimatedElapsedHours,
    TdxDurationFormat.EstimatedElapsedDays,
    TdxDurationFormat.EstimatedElapsedWeeks,
    TdxDurationFormat.EstimatedElapsedMonths,
    TdxDurationFormat.EstimatedElapsedPercent];
end;

function TdxGanttControlDuration.IsNegative: Boolean;
begin
  Result := not IsNull and (FDuration[1] = '-');
end;

function TdxGanttControlDuration.IsNull: Boolean;
begin
  Result := FDuration = '';
end;

function TdxGanttControlDuration.IsZero: Boolean;
begin
  Result := not IsNull and (FSecond = 0) and (FMinute = 0) and (FHour = 0);
end;

{ TdxGanttControlCalendarWeekDayWorkTime }

procedure TdxGanttControlCalendarWeekDayWorkTime.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlCalendarWeekDayWorkTime;
begin
  if Source is TdxGanttControlCalendarWeekDayWorkTime then
  begin
    ASource := TdxGanttControlCalendarWeekDayWorkTime(Source);
    FromTime := ASource.FromTime;
    ToTime := ASource.ToTime;
  end;
  inherited Assign(Source);
end;

procedure TdxGanttControlCalendarWeekDayWorkTime.DoReset;
begin
  FFromTime := 0;
  FToTime := 1;
end;

procedure TdxGanttControlCalendarWeekDayWorkTime.SetFromTime
  (const Value: TTime);
begin
  if FromTime <> Value then
  begin
    FFromTime := Value;
    Changed;
  end;
end;

procedure TdxGanttControlCalendarWeekDayWorkTime.SetToTime(const Value: TTime);
begin
  if ToTime <> Value then
  begin
    FToTime := Value;
    Changed;
  end;
end;

function dxGanttControlWeekDayWorkTimeCompare(Item1, Item2: Pointer): Integer;
var
  dt: Double;
  Left: TdxGanttControlModelElementListItem absolute Item1;
  Right: TdxGanttControlModelElementListItem absolute Item2;
begin
  dt := TdxGanttControlCalendarWeekDayWorkTime(Left).FromTime -
    TdxGanttControlCalendarWeekDayWorkTime(Right).FromTime;
  Result := IfThen(dt > dxEpsilon, 1, IfThen(dt < -dxEpsilon, -1, 0));
end;

{ TdxGanttControlCalendarWeekDayWorkTimes }

function TdxGanttControlCalendarWeekDayWorkTimes.Append
  : TdxGanttControlCalendarWeekDayWorkTime;
begin
  Result := TdxGanttControlCalendarWeekDayWorkTime(CreateItem);
  InternalAdd(Result);
end;

procedure TdxGanttControlCalendarWeekDayWorkTimes.Clear;
begin
  InternalClear;
end;

function TdxGanttControlCalendarWeekDayWorkTimes.CreateItem
  : TdxGanttControlModelElementListItem;
begin
  Result := TdxGanttControlCalendarWeekDayWorkTime.Create(Self);
end;

procedure TdxGanttControlCalendarWeekDayWorkTimes.DoItemChanged
  (AItem: TdxGanttControlModelElementListItem);
begin
  inherited DoItemChanged(AItem);
  FIsSorted := False;
end;

function TdxGanttControlCalendarWeekDayWorkTimes.GetItem(Index: Integer)
  : TdxGanttControlCalendarWeekDayWorkTime;
begin
  Result := TdxGanttControlCalendarWeekDayWorkTime(inherited Items[Index]);
end;

procedure TdxGanttControlCalendarWeekDayWorkTimes.DoSort;
begin
  if FIsSorted then
    Exit;
  InternalSort(dxGanttControlWeekDayWorkTimeCompare);
  Changed;
  FIsSorted := True;
end;

function TdxGanttControlCalendarWeekDayWorkTimes.IsWorkTime
  (ADateTime: TDateTime): Boolean;
var
  ATime: TTime;
  I: Integer;
begin
  ATime := TimeOf(ADateTime);
  for I := 0 to Count - 1 do
    if (CompareDateTime(Items[I].FromTime, ATime) <= 0) and
      (CompareDateTime(Items[I].ToTime, ATime) > 0) then
      Exit(True);
  Result := False;
end;

procedure TdxGanttControlCalendarWeekDayWorkTimes.Remove
  (AItem: TdxGanttControlCalendarWeekDayWorkTime);
begin
  InternalRemove(AItem);
end;

{ TdxGanttControlCalendarWeekDay }

constructor TdxGanttControlCalendarWeekDay.Create
  (AOwner: TdxGanttControlModelElement);
begin
  inherited Create(AOwner);
  FWorkTimes := TdxGanttControlCalendarWeekDayWorkTimes.Create(Self);
end;

destructor TdxGanttControlCalendarWeekDay.Destroy;
begin
  FreeAndNil(FTimePeriod);
  FreeAndNil(FWorkTimes);
  inherited Destroy;
end;

procedure TdxGanttControlCalendarWeekDay.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlCalendarWeekDay;
begin
  if Source is TdxGanttControlCalendarWeekDay then
  begin
    ASource := TdxGanttControlCalendarWeekDay(Source);
    BeginUpdate;
    try
      DayType := ASource.DayType;
      Workday := ASource.Workday;
      TimePeriod := ASource.TimePeriod;
      WorkTimes := ASource.WorkTimes;
      FAssignedValues := ASource.FAssignedValues;
    finally
      EndUpdate;
    end;
  end;
  inherited Assign(Source);
end;

procedure TdxGanttControlCalendarWeekDay.CreateTimePeriod;
begin
  if FTimePeriod = nil then
    FTimePeriod := TdxGanttControlTimePeriod.Create(Self);
end;

procedure TdxGanttControlCalendarWeekDay.DestroyTimePeriod;
begin
  FreeAndNil(FTimePeriod);
end;

procedure TdxGanttControlCalendarWeekDay.DoReset;
begin
  FAssignedValues := [];
  DestroyTimePeriod;
  FWorkTimes.Reset;
end;

function TdxGanttControlCalendarWeekDay.InternalGetOwner
  : TdxGanttControlCalendarWeekDays;
begin
  Result := TdxGanttControlCalendarWeekDays(inherited Owner);
end;

function TdxGanttControlCalendarWeekDay.IsValueAssigned
  (const AValue: TdxGanttCalendarWeekDayAssignedValue): Boolean;
begin
  Result := AValue in FAssignedValues;
end;

procedure TdxGanttControlCalendarWeekDay.ResetValue(const AValue
  : TdxGanttCalendarWeekDayAssignedValue);
begin
  if IsValueAssigned(AValue) then
  begin
    Exclude(FAssignedValues, AValue);
    Changed;
  end
end;

procedure TdxGanttControlCalendarWeekDay.SetDayType
  (const Value: TdxGanttControlWeekDayType);
begin
  if (DayType <> Value) or not IsValueAssigned
    (TdxGanttCalendarWeekDayAssignedValue.DayType) then
  begin
    Include(FAssignedValues, TdxGanttCalendarWeekDayAssignedValue.DayType);
    FDayType := Value;
    Changed;
  end;
end;

procedure TdxGanttControlCalendarWeekDay.SetWorkday(const Value: Boolean);
begin
  if (Workday <> Value) or not IsValueAssigned
    (TdxGanttCalendarWeekDayAssignedValue.Workday) then
  begin
    Include(FAssignedValues, TdxGanttCalendarWeekDayAssignedValue.Workday);
    FWorkday := Value;
    Changed;
  end;
end;

procedure TdxGanttControlCalendarWeekDay.SetTimePeriod
  (const Value: TdxGanttControlTimePeriod);
begin
  if Value = nil then
    DestroyTimePeriod
  else
  begin
    CreateTimePeriod;
    TimePeriod.Assign(Value);
  end;
end;

procedure TdxGanttControlCalendarWeekDay.SetWorkTimes
  (const Value: TdxGanttControlCalendarWeekDayWorkTimes);
begin
  WorkTimes.Assign(Value);
end;

{ TdxGanttControlCalendarWeekDays }

function TdxGanttControlCalendarWeekDays.Append: TdxGanttControlCalendarWeekDay;
begin
  Result := TdxGanttControlCalendarWeekDay(CreateItem);
  InternalAdd(Result);
end;

procedure TdxGanttControlCalendarWeekDays.Clear;
begin
  InternalClear;
end;

procedure TdxGanttControlCalendarWeekDays.Remove
  (AItem: TdxGanttControlCalendarWeekDay);
begin
  InternalRemove(AItem);
end;

function TdxGanttControlCalendarWeekDays.GetFinishWorkTime(ADate: TDateTime;
  var APattern: TdxGanttControlCalendarWeekDay): TDateTime;
var
  ADay: TdxGanttControlWeekDayType;
  I, J: Integer;
  ATime: TTime;
begin
  if not IsWorkday(ADate, APattern) then
    Exit(InvalidDate);
  if APattern <> nil then
  begin
    APattern.WorkTimes.DoSort;
    ATime := APattern.WorkTimes[APattern.WorkTimes.Count - 1].ToTime;
    if ATime = 0 then
      ATime := 1;
    Exit(DateOf(ADate) + ATime);
  end;

  Result := 0;
  ADay := TdxGanttControlWeekDayType(Ord(dxDayOfWeek(ADate)) + 1);
  for I := 0 to Count - 1 do
  begin
    if ((ADay = Items[I].DayType) or
      ((Items[I].DayType = TdxGanttControlWeekDayType.Exception) and
      Items[I].Workday)) and ((Items[I].TimePeriod = nil) or
      ((Items[I].TimePeriod.FromDate <= ADate) and (Items[I].TimePeriod.ToDate >
      ADate))) then
    begin
      for J := 0 to Items[I].WorkTimes.Count - 1 do
        if Result < Items[I].WorkTimes[J].ToTime then
        begin
          APattern := Items[I];
          Result := Items[I].WorkTimes[J].ToTime;
        end;
    end;
  end;
  Result := DateOf(ADate) + Result;
end;

function TdxGanttControlCalendarWeekDays.GetStartWorkTime(ADate: TDateTime;
  var APattern: TdxGanttControlCalendarWeekDay): TDateTime;
var
  ADay: TdxGanttControlWeekDayType;
  I, J: Integer;
begin
  if not IsWorkday(ADate, APattern) then
    Exit(InvalidDate);
  if APattern <> nil then
  begin
    APattern.WorkTimes.DoSort;
    Exit(DateOf(ADate) + APattern.WorkTimes[0].FromTime);
  end;

  Result := 1;
  ADay := TdxGanttControlWeekDayType(Ord(dxDayOfWeek(ADate)) + 1);
  for I := 0 to Count - 1 do
  begin
    if ((ADay = Items[I].DayType) or
      ((Items[I].DayType = TdxGanttControlWeekDayType.Exception) and
      Items[I].Workday)) and ((Items[I].TimePeriod = nil) or
      ((Items[I].TimePeriod.FromDate <= ADate) and (Items[I].TimePeriod.ToDate >
      ADate))) then
    begin
      for J := 0 to Items[I].WorkTimes.Count - 1 do
        if Result > Items[I].WorkTimes[J].FromTime then
        begin
          APattern := Items[I];
          Result := Items[I].WorkTimes[J].FromTime;
        end;
    end;
  end;
  Result := DateOf(ADate) + Result;
end;

function TdxGanttControlCalendarWeekDays.IsNormalOrExceptionWorkday
  (ADate: TDate; AStart, AFinish: TTime;
  var APattern: TdxGanttControlCalendarWeekDay): Boolean;
var
  ADay: TdxGanttControlWeekDayType;
  I: Integer;
begin
  APattern := nil;
  Result := True;
  if ADate < TdxGanttControlDataModel.MinDate then
    Exit;
  ADay := TdxGanttControlWeekDayType(Ord(dxDayOfWeek(ADate)) + 1);
  for I := Count - 1 downto 0 do
  begin
    Items[I].WorkTimes.DoSort;
    if Items[I].DayType = TdxGanttControlWeekDayType.Exception then
    begin
      if Items[I].TimePeriod = nil then
        Continue;
    end
    else if ADay <> Items[I].DayType then
      Continue;
    if (Items[I].TimePeriod = nil) or
      ((Items[I].TimePeriod.FromDate <= ADate) and
      (Items[I].TimePeriod.ToDate > ADate)) then
    begin
      if not Items[I].Workday and ((AStart > 0) or (AFinish < dxEndOfDay)) then
      begin
        APattern := DefaultWorkdayPattern;
        Exit(True);
      end
      else
      begin
        APattern := Items[I];
        Exit(Items[I].Workday);
      end;
    end;
  end;
end;

function TdxGanttControlCalendarWeekDays.IsWorkday(ADate: TDate;
  var APattern: TdxGanttControlCalendarWeekDay): Boolean;
begin
  Result := IsNormalOrExceptionWorkday(ADate, 0, dxEndOfDay, APattern);
end;

function TdxGanttControlCalendarWeekDays.IsWorkTime(ADateTime: TDateTime;
  var APattern: TdxGanttControlCalendarWeekDay): Boolean;
var
  ADay: TdxGanttControlWeekDayType;
  I: Integer;
begin
  Result := IsWorkday(ADateTime, APattern);
  if Result and not(ADateTime < TdxGanttControlDataModel.MinDate) then
  begin
    ADay := TdxGanttControlWeekDayType(Ord(dxDayOfWeek(ADateTime)) + 1);
    for I := Count - 1 downto 0 do
    begin
      if Items[I].DayType = TdxGanttControlWeekDayType.Exception then
      begin
        if Items[I].TimePeriod = nil then
          Continue;
      end
      else if ADay <> Items[I].DayType then
        Continue;
      if (Items[I].TimePeriod = nil) or
        ((Items[I].TimePeriod.FromDate <= ADateTime) and
        (Items[I].TimePeriod.ToDate > ADateTime)) then
        Exit(Items[I].Workday and Items[I].WorkTimes.IsWorkTime(ADateTime));
    end;
  end;
end;

function TdxGanttControlCalendarWeekDays.CreateItem
  : TdxGanttControlModelElementListItem;
begin
  Result := TdxGanttControlCalendarWeekDay.Create(Self);
end;

function TdxGanttControlCalendarWeekDays.GetDefaultWorkdayPattern
  : TdxGanttControlCalendarWeekDay;
var
  I: Integer;
begin
  if FDefaultWorkdayPattern = nil then
    for I := 0 to Count - 1 do
      if (Items[I].DayType <> TdxGanttControlWeekDayType.Exception) and
        Items[I].Workday then
      begin
        FDefaultWorkdayPattern := Items[I];
        Break;
      end;
  Result := FDefaultWorkdayPattern;
  if Result <> nil then
    Result.WorkTimes.DoSort;
end;

function TdxGanttControlCalendarWeekDays.GetItem(Index: Integer)
  : TdxGanttControlCalendarWeekDay;
begin
  Result := TdxGanttControlCalendarWeekDay(inherited Items[Index]);
end;

{ TdxGanttControlCalendarException }

constructor TdxGanttControlCalendarException.Create
  (AOwner: TdxGanttControlModelElement);
begin
  inherited Create(AOwner);
  FWorkTimes := TdxGanttControlCalendarWeekDayWorkTimes.Create(Self);
end;

destructor TdxGanttControlCalendarException.Destroy;
begin
  FreeAndNil(FTimePeriod);
  FreeAndNil(FWorkTimes);
  inherited Destroy;
end;

procedure TdxGanttControlCalendarException.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlCalendarException;
begin
  if Source is TdxGanttControlCalendarException then
  begin
    ASource := TdxGanttControlCalendarException(Source);
    BeginUpdate;
    try
      EnteredByOccurrences := ASource.EnteredByOccurrences;
      DaysOfWeek := ASource.DaysOfWeek;
      Workday := ASource.Workday;
      Month := ASource.Month;
      MonthDay := ASource.MonthDay;
      MonthItem := ASource.MonthItem;
      MonthPosition := ASource.MonthPosition;
      Name := ASource.Name;
      Occurrences := ASource.Occurrences;
      Period := ASource.Period;
      TimePeriod := ASource.TimePeriod;
      &Type := ASource.&Type;
      WorkTimes := ASource.WorkTimes;
      FAssignedValues := ASource.FAssignedValues;
    finally
      EndUpdate;
    end;
  end;
  inherited Assign(Source);
end;

procedure TdxGanttControlCalendarException.CreateTimePeriod;
begin
  if FTimePeriod = nil then
    FTimePeriod := TdxGanttControlTimePeriod.Create(Self);
end;

procedure TdxGanttControlCalendarException.DestroyTimePeriod;
begin
  FreeAndNil(FTimePeriod);
end;

procedure TdxGanttControlCalendarException.DoReset;
begin
  DestroyTimePeriod;
  FWorkTimes.Reset;
  FAssignedValues := [];
end;

function TdxGanttControlCalendarException.IsValueAssigned
  (const AValue: TdxGanttCalendarExceptionAssignedValue): Boolean;
begin
  Result := AValue in FAssignedValues;
end;

procedure TdxGanttControlCalendarException.ResetValue
  (const AValue: TdxGanttCalendarExceptionAssignedValue);
begin
  if IsValueAssigned(AValue) then
  begin
    Exclude(FAssignedValues, AValue);
    Changed;
  end
end;

procedure TdxGanttControlCalendarException.SetDaysOfWeek
  (const Value: TdxGanttControlExceptionDaysOfWeek);
begin
  if (DaysOfWeek <> Value) or not IsValueAssigned
    (TdxGanttCalendarExceptionAssignedValue.DaysOfWeek) then
  begin
    Include(FAssignedValues, TdxGanttCalendarExceptionAssignedValue.DaysOfWeek);
    FDaysOfWeek := Value;
    Changed;
  end;
end;

procedure TdxGanttControlCalendarException.SetWorkday(const Value: Boolean);
begin
  if (Workday <> Value) or not IsValueAssigned
    (TdxGanttCalendarExceptionAssignedValue.Workday) then
  begin
    Include(FAssignedValues, TdxGanttCalendarExceptionAssignedValue.Workday);
    FWorkday := Value;
    Changed;
  end;
end;

procedure TdxGanttControlCalendarException.SetEnteredByOccurrences
  (const Value: Boolean);
begin
  if (EnteredByOccurrences <> Value) or
    not IsValueAssigned(TdxGanttCalendarExceptionAssignedValue.
    EnteredByOccurrences) then
  begin
    Include(FAssignedValues,
      TdxGanttCalendarExceptionAssignedValue.EnteredByOccurrences);
    FEnteredByOccurrences := Value;
    Changed;
  end;
end;

procedure TdxGanttControlCalendarException.SetMonth
  (const Value: TdxGanttControlCalendarMonth);
begin
  if (Month <> Value) or not IsValueAssigned
    (TdxGanttCalendarExceptionAssignedValue.Month) then
  begin
    Include(FAssignedValues, TdxGanttCalendarExceptionAssignedValue.Month);
    FMonth := Value;
    Changed;
  end;
end;

procedure TdxGanttControlCalendarException.SetMonthDay(const Value: Integer);
begin
  if (MonthDay <> Value) or not IsValueAssigned
    (TdxGanttCalendarExceptionAssignedValue.MonthDay) then
  begin
    Include(FAssignedValues, TdxGanttCalendarExceptionAssignedValue.MonthDay);
    FMonthDay := Value;
    Changed;
  end;
end;

procedure TdxGanttControlCalendarException.SetMonthItem
  (const Value: TdxGanttControlExceptionMonthItem);
begin
  if (MonthItem <> Value) or not IsValueAssigned
    (TdxGanttCalendarExceptionAssignedValue.MonthItem) then
  begin
    Include(FAssignedValues, TdxGanttCalendarExceptionAssignedValue.MonthItem);
    FMonthItem := Value;
    Changed;
  end;
end;

procedure TdxGanttControlCalendarException.SetMonthPosition
  (const Value: TdxGanttControlExceptionMonthPosition);
begin
  if (MonthPosition <> Value) or not IsValueAssigned
    (TdxGanttCalendarExceptionAssignedValue.MonthPosition) then
  begin
    Include(FAssignedValues,
      TdxGanttCalendarExceptionAssignedValue.MonthPosition);
    FMonthPosition := Value;
    Changed;
  end;
end;

procedure TdxGanttControlCalendarException.SetName(const Value: string);
begin
  if (Name <> Value) or not IsValueAssigned
    (TdxGanttCalendarExceptionAssignedValue.Name) then
  begin
    Include(FAssignedValues, TdxGanttCalendarExceptionAssignedValue.Name);
    FName := Value;
    Changed;
  end;
end;

procedure TdxGanttControlCalendarException.SetOccurrences(const Value: Integer);
begin
  if (Occurrences <> Value) or not IsValueAssigned
    (TdxGanttCalendarExceptionAssignedValue.Occurrences) then
  begin
    Include(FAssignedValues,
      TdxGanttCalendarExceptionAssignedValue.Occurrences);
    FOccurrences := Value;
    Changed;
  end;
end;

procedure TdxGanttControlCalendarException.SetPeriod(const Value: Integer);
begin
  if (Period <> Value) or not IsValueAssigned
    (TdxGanttCalendarExceptionAssignedValue.Period) then
  begin
    Include(FAssignedValues, TdxGanttCalendarExceptionAssignedValue.Period);
    FPeriod := Value;
    Changed;
  end;
end;

procedure TdxGanttControlCalendarException.SetTimePeriod
  (const Value: TdxGanttControlTimePeriod);
begin
  if Value = nil then
    DestroyTimePeriod
  else
  begin
    CreateTimePeriod;
    TimePeriod.Assign(Value);
  end;
end;

procedure TdxGanttControlCalendarException.SetType
  (const Value: TdxGanttControlExceptionType);
begin
  if (&Type <> Value) or not IsValueAssigned
    (TdxGanttCalendarExceptionAssignedValue.&Type) then
  begin
    Include(FAssignedValues, TdxGanttCalendarExceptionAssignedValue.&Type);
    FType := Value;
    Changed;
  end;
end;

procedure TdxGanttControlCalendarException.SetWorkTimes
  (const Value: TdxGanttControlCalendarWeekDayWorkTimes);
begin
  FWorkTimes.Assign(Value);
end;

{ TdxGanttControlCalendarExceptions }

function TdxGanttControlCalendarExceptions.Append
  : TdxGanttControlCalendarException;
begin
  Result := TdxGanttControlCalendarException(CreateItem);
  InternalAdd(Result);
end;

procedure TdxGanttControlCalendarExceptions.Clear;
begin
  InternalClear;
end;

function TdxGanttControlCalendarExceptions.CreateItem
  : TdxGanttControlModelElementListItem;
begin
  Result := TdxGanttControlCalendarException.Create(Self);
end;

function TdxGanttControlCalendarExceptions.GetItem(Index: Integer)
  : TdxGanttControlCalendarException;
begin
  Result := TdxGanttControlCalendarException(inherited Items[Index]);
end;

procedure TdxGanttControlCalendarExceptions.Remove
  (AItem: TdxGanttControlCalendarException);
begin
  InternalRemove(AItem);
end;

{ TdxGanttControlCalendarWorkWeek }

constructor TdxGanttControlCalendarWorkWeek.Create
  (AOwner: TdxGanttControlModelElement);
begin
  inherited Create(AOwner);
  FWeekDays := TdxGanttControlCalendarWeekDays.Create(Self);
end;

destructor TdxGanttControlCalendarWorkWeek.Destroy;
begin
  FreeAndNil(FWeekDays);
  FreeAndNil(FTimePeriod);
  inherited Destroy;
end;

procedure TdxGanttControlCalendarWorkWeek.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlCalendarWorkWeek;
begin
  if Source is TdxGanttControlCalendarWorkWeek then
  begin
    ASource := TdxGanttControlCalendarWorkWeek(Source);
    BeginUpdate;
    try
      TimePeriod := ASource.TimePeriod;
      Name := ASource.Name;
      WeekDays := ASource.WeekDays;
      FAssignedValues := ASource.FAssignedValues;
    finally
      EndUpdate;
    end;
  end;
  inherited Assign(Source);
end;

procedure TdxGanttControlCalendarWorkWeek.CreateTimePeriod;
begin
  if FTimePeriod = nil then
    FTimePeriod := TdxGanttControlTimePeriod.Create(Self);
end;

procedure TdxGanttControlCalendarWorkWeek.DestroyTimePeriod;
begin
  FreeAndNil(FTimePeriod);
end;

procedure TdxGanttControlCalendarWorkWeek.DoReset;
begin
  DestroyTimePeriod;
  FWeekDays.Reset;
  FAssignedValues := [];
end;

function TdxGanttControlCalendarWorkWeek.IsValueAssigned
  (const AValue: TdxGanttCalendarWorkWeekAssignedValue): Boolean;
begin
  Result := AValue in FAssignedValues;
end;

procedure TdxGanttControlCalendarWorkWeek.ResetValue(const AValue
  : TdxGanttCalendarWorkWeekAssignedValue);
begin
  if IsValueAssigned(AValue) then
  begin
    Exclude(FAssignedValues, AValue);
    Changed;
  end
end;

procedure TdxGanttControlCalendarWorkWeek.SetName(const Value: string);
begin
  if (FName <> Value) or not IsValueAssigned
    (TdxGanttCalendarWorkWeekAssignedValue.Name) then
  begin
    Include(FAssignedValues, TdxGanttCalendarWorkWeekAssignedValue.Name);
    FName := Value;
    Changed;
  end;
end;

procedure TdxGanttControlCalendarWorkWeek.SetTimePeriod
  (const Value: TdxGanttControlTimePeriod);
begin
  if Value = nil then
  begin
    Exclude(FAssignedValues, TdxGanttCalendarWorkWeekAssignedValue.TimePeriod);
    DestroyTimePeriod;
  end
  else
  begin
    Include(FAssignedValues, TdxGanttCalendarWorkWeekAssignedValue.TimePeriod);
    CreateTimePeriod;
    TimePeriod.Assign(Value);
  end;
end;

procedure TdxGanttControlCalendarWorkWeek.SetWeekDays
  (const Value: TdxGanttControlCalendarWeekDays);
begin
  FWeekDays.Assign(Value);
end;

{ TdxGanttControlCalendarWorkWeeks }

function TdxGanttControlCalendarWorkWeeks.Append
  : TdxGanttControlCalendarWorkWeek;
begin
  Result := TdxGanttControlCalendarWorkWeek(CreateItem);
  InternalAdd(Result);
end;

procedure TdxGanttControlCalendarWorkWeeks.Clear;
begin
  InternalClear;
end;

function TdxGanttControlCalendarWorkWeeks.CreateItem
  : TdxGanttControlModelElementListItem;
begin
  Result := TdxGanttControlCalendarWorkWeek.Create(Self);
end;

function TdxGanttControlCalendarWorkWeeks.GetItem(Index: Integer)
  : TdxGanttControlCalendarWorkWeek;
begin
  Result := TdxGanttControlCalendarWorkWeek(inherited Items[Index]);
end;

procedure TdxGanttControlCalendarWorkWeeks.Remove
  (AItem: TdxGanttControlCalendarWorkWeek);
begin
  InternalRemove(AItem);
end;

{ TdxGanttControlCalendar }

constructor TdxGanttControlCalendar.Create(AOwner: TdxGanttControlModelElement);
begin
  inherited Create(AOwner);
  FWeekDays := TdxGanttControlCalendarWeekDays.Create(Self);
  FExceptions := TdxGanttControlCalendarExceptions.Create(Self);
  FWorkWeeks := TdxGanttControlCalendarWorkWeeks.Create(Self);
end;

destructor TdxGanttControlCalendar.Destroy;
begin
  FreeAndNil(FWorkWeeks);
  FreeAndNil(FExceptions);
  FreeAndNil(FWeekDays);
  inherited Destroy;
end;

procedure TdxGanttControlCalendar.DoReset;
begin
  FBaseCalendarUID := 0;
  FGUID := TdxGanttControlUtils.GenerateGUID;
  WeekDays.Reset;
  Exceptions.Reset;
  WorkWeeks.Reset;
  FAssignedValues := [];
end;

procedure TdxGanttControlCalendar.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlCalendar;
begin
  if Source is TdxGanttControlCalendar then
  begin
    ASource := TdxGanttControlCalendar(Source);
    BeginUpdate;
    try
      Name := ASource.Name;
      GUID := ASource.GUID;
      BaseCalendarUID := ASource.BaseCalendarUID;
      Exceptions := ASource.Exceptions;
      WeekDays := ASource.WeekDays;
      WorkWeeks := ASource.WorkWeeks;
      FAssignedValues := ASource.FAssignedValues;
    finally
      EndUpdate;
    end;
  end;
  inherited Assign(Source);
end;

function TdxGanttControlCalendar.GetFinishWorkTime(ADate: TDateTime): TDateTime;
var
  APattern: TdxGanttControlCalendarWeekDay;
begin
  Result := DoGetFinishWorkTime(ADate, APattern);
  if APattern = nil then
    if BaseCalendar <> nil then
      Result := BaseCalendar.GetFinishWorkTime(ADate)
    else if TdxGanttControlDataModel(DataModel).Properties.IsValueAssigned
      (TdxGanttPropertiesAssignedValue.DefaultFinishTime) then
      Result := ADate + TdxGanttControlDataModel(DataModel)
        .Properties.DefaultFinishTime;
end;

function TdxGanttControlCalendar.GetNextWorkTime(ADateTime: TDateTime)
  : TDateTime;
var
  I: Integer;
  APattern: TdxGanttControlCalendarWeekDay;
  ATodayNextTime: TDateTime;
begin
  if IsWorkTime(ADateTime) then
    Exit(ADateTime);
  if not IsWorkday(ADateTime) or
    (CompareDateTime(ADateTime, GetFinishWorkTime(ADateTime)) >= 0) then
    Result := GetNextWorkTime(DateOf(ADateTime) + 1)
  else
  begin
    Result := GetStartWorkTime(ADateTime);
    if Result >= ADateTime then
      Exit;
    Result := DoGetStartWorkTime(ADateTime, APattern);
    if APattern <> nil then
    begin
      APattern.WorkTimes.DoSort;
      for I := 0 to APattern.WorkTimes.Count - 1 do
      begin
        ATodayNextTime := DateOf(ADateTime) + APattern.WorkTimes[I].FromTime;
        if ADateTime <= ATodayNextTime then
          Exit(ATodayNextTime);
      end;
    end;
    if BaseCalendar <> nil then
      Result := BaseCalendar.GetNextWorkTime(ADateTime);
  end;
end;

function TdxGanttControlCalendar.GetPreviousWorkTime(ADateTime: TDateTime)
  : TDateTime;
var
  I: Integer;
  APattern: TdxGanttControlCalendarWeekDay;
  AToday, ATodayPriorTime: TDateTime;
begin
  if not IsWorkday(ADateTime) or (ADateTime <= GetStartWorkTime(ADateTime)) then
    Result := GetPreviousWorkTime(DateOf(ADateTime) - OneMinute)
  else
  begin
    Result := DoGetFinishWorkTime(ADateTime, APattern);
    if Result <= ADateTime then
      Exit(Result);
    if APattern <> nil then
    begin
      APattern.WorkTimes.DoSort;
      AToday := DateOf(ADateTime);
      for I := APattern.WorkTimes.Count - 1 downto 0 do
      begin
        ATodayPriorTime := AToday + APattern.WorkTimes[I].ToTime;
        if ADateTime >= ATodayPriorTime then
          Exit(ATodayPriorTime);
        if (ADateTime > AToday + APattern.WorkTimes[I].FromTime) and
          (ADateTime < ATodayPriorTime) then
          Exit(ADateTime);
      end;
    end;
    if BaseCalendar <> nil then
      Result := BaseCalendar.GetPreviousWorkTime(ADateTime);
  end;
end;

function TdxGanttControlCalendar.GetStartWorkTime(ADate: TDateTime): TDateTime;
var
  APattern: TdxGanttControlCalendarWeekDay;
begin
  Result := DoGetStartWorkTime(ADate, APattern);
  if APattern = nil then
    if BaseCalendar <> nil then
      Result := BaseCalendar.GetStartWorkTime(ADate)
    else if TdxGanttControlDataModel(DataModel).Properties.IsValueAssigned
      (TdxGanttPropertiesAssignedValue.DefaultStartTime) then
      Result := ADate + TdxGanttControlDataModel(DataModel)
        .Properties.DefaultStartTime;
end;

function TdxGanttControlCalendar.IsNormalOrExceptionWorkday(ADate: TDate;
  AStart, AFinish: TTime; var APattern: TdxGanttControlCalendarWeekDay)
  : Boolean;
begin
  if BaseCalendar <> nil then
    Result := BaseCalendar.IsNormalOrExceptionWorkday(ADate, AStart,
      AFinish, APattern)
  else
    Result := WeekDays.IsNormalOrExceptionWorkday(ADate, AStart, AFinish,
      APattern);
end;

function TdxGanttControlCalendar.IsValidNormalOrExceptionWorkday(ADate: TDate;
  AStart, AFinish: TTime; var APattern: TdxGanttControlCalendarWeekDay)
  : Boolean;
begin
  Result := IsNormalOrExceptionWorkday(ADate, AStart, AFinish, APattern) and
    (APattern <> nil) and (APattern.WorkTimes.Count > 0);
end;

function TdxGanttControlCalendar.DoGetFinishWorkTime(ADate: TDateTime;
  var APattern: TdxGanttControlCalendarWeekDay): TDateTime;
begin
  Result := WeekDays.GetFinishWorkTime(ADate, APattern);
end;

function TdxGanttControlCalendar.DoGetStartWorkTime(ADate: TDateTime;
  var APattern: TdxGanttControlCalendarWeekDay): TDateTime;
begin
  Result := WeekDays.GetStartWorkTime(ADate, APattern);
end;

function TdxGanttControlCalendar.IsWorkday(ADate: TDate): Boolean;
var
  APattern: TdxGanttControlCalendarWeekDay;
begin
  Result := WeekDays.IsWorkday(ADate, APattern);
  if (APattern = nil) and (BaseCalendar <> nil) then
    Result := BaseCalendar.IsWorkday(ADate);
end;

function TdxGanttControlCalendar.IsWorkTime(ADateTime: TDateTime): Boolean;
var
  APattern: TdxGanttControlCalendarWeekDay;
begin
  Result := WeekDays.IsWorkTime(ADateTime, APattern);
  if (APattern = nil) and (BaseCalendar <> nil) then
    Result := BaseCalendar.IsWorkTime(ADateTime);
end;

function TdxGanttControlCalendar.GetBaseCalendar: TdxGanttControlCalendar;
begin
  Result := Owner.GetCalendarByUID(BaseCalendarUID);
end;

function TdxGanttControlCalendar.GetIsBaseCalendar: Boolean;
begin
  Result := FBaseCalendarUID <= 0;
end;

function TdxGanttControlCalendar.InternalGetOwner: TdxGanttControlCalendars;
begin
  Result := TdxGanttControlCalendars(inherited Owner);
end;

function TdxGanttControlCalendar.IsValueAssigned(const AValue
  : TdxGanttCalendarAssignedValue): Boolean;
begin
  Result := AValue in FAssignedValues;
end;

procedure TdxGanttControlCalendar.ResetValue(const AValue
  : TdxGanttCalendarAssignedValue);
begin
  if IsValueAssigned(AValue) then
  begin
    Exclude(FAssignedValues, AValue);
    Changed;
  end
end;

procedure TdxGanttControlCalendar.SetBaseCalendar
  (const Value: TdxGanttControlCalendar);
begin
  if Value = nil then
    BaseCalendarUID := 0
  else if Owner = Value.Owner then
    BaseCalendarUID := Value.UID;
end;

procedure TdxGanttControlCalendar.SetWeekDays(const Value
  : TdxGanttControlCalendarWeekDays);
begin
  WeekDays.Assign(Value);
end;

procedure TdxGanttControlCalendar.SetWorkWeeks(const Value
  : TdxGanttControlCalendarWorkWeeks);
begin
  WorkWeeks.Assign(Value);
end;

procedure TdxGanttControlCalendar.SetBaseCalendarUID(const Value: Integer);
begin
  if BaseCalendarUID <> Value then
  begin
    FBaseCalendarUID := Value;
    Changed;
  end;
end;

procedure TdxGanttControlCalendar.SetExceptions(const Value
  : TdxGanttControlCalendarExceptions);
begin
  Exceptions.Assign(Value);
end;

procedure TdxGanttControlCalendar.SetBaselineCalendar(const Value: Boolean);
begin
  if (BaselineCalendar <> Value) or
    not IsValueAssigned(TdxGanttCalendarAssignedValue.BaselineCalendar) then
  begin
    Include(FAssignedValues, TdxGanttCalendarAssignedValue.BaselineCalendar);
    FBaselineCalendar := Value;
    Changed;
  end;
end;

procedure TdxGanttControlCalendar.SetName(const Value: string);
begin
  if (Name <> Value) or not IsValueAssigned(TdxGanttCalendarAssignedValue.Name)
  then
  begin
    Include(FAssignedValues, TdxGanttCalendarAssignedValue.Name);
    FName := Value;
    Changed;
  end;
end;

{ TdxGanttControlCalendars }

function TdxGanttControlCalendars.Append: TdxGanttControlCalendar;
begin
  Result := TdxGanttControlCalendar(CreateItem);
  InternalAdd(Result);
end;

procedure TdxGanttControlCalendars.Remove(AItem: TdxGanttControlCalendar);
var
  I: Integer;
begin
  if (AItem = nil) or (AItem.Owner <> Self) then
    Exit;
  for I := 0 to Count - 1 do
    if Items[I].BaseCalendarUID = AItem.UID then
      Items[I].BaseCalendarUID := -1;
  InternalRemove(AItem);
end;

function TdxGanttControlCalendars.GetCalendarByName(const Value: string)
  : TdxGanttControlCalendar;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if CompareText(Items[I].Name, Value) = 0 then
      Exit(Items[I]);
  Result := nil;
end;

function TdxGanttControlCalendars.GetCalendarByUID(const AUID: Integer)
  : TdxGanttControlCalendar;
begin
  Result := TdxGanttControlCalendar(GetItemByUID(AUID));
end;

function TdxGanttControlCalendars.CreateStandardCalendar
  : TdxGanttControlCalendar;

  procedure AppendWeekDay(ADayType: TdxGanttControlWeekDayType;
    AWorkday: Boolean);
  var
    AWeekDay: TdxGanttControlCalendarWeekDay;
  begin
    AWeekDay := Result.WeekDays.Append;
    AWeekDay.DayType := ADayType;
    AWeekDay.Workday := AWorkday;
    if AWorkday then
    begin
      with AWeekDay.WorkTimes.Append do
      begin
        FromTime := StandardCalendarWorkTimeFrom1;
        ToTime := StandardCalendarWorkTimeTo1;
      end;
      with AWeekDay.WorkTimes.Append do
      begin
        FromTime := StandardCalendarWorkTimeFrom2;
        ToTime := StandardCalendarWorkTimeTo2;
      end;
    end;
  end;

begin
  BeginUpdate;
  try
    Result := Append;
    Result.Name := cxGetResourceString(@sdxGanttControlStandardCalendarName);
    Result.BaseCalendarUID := 0;
    AppendWeekDay(TdxGanttControlWeekDayType.Sunday, False);
    AppendWeekDay(TdxGanttControlWeekDayType.Monday, True);
    AppendWeekDay(TdxGanttControlWeekDayType.Tuesday, True);
    AppendWeekDay(TdxGanttControlWeekDayType.Wednesday, True);
    AppendWeekDay(TdxGanttControlWeekDayType.Thursday, True);
    AppendWeekDay(TdxGanttControlWeekDayType.Friday, True);
    AppendWeekDay(TdxGanttControlWeekDayType.Saturday, False);
    if not CreateStandardCalendarHandlers.Empty then
      CreateStandardCalendarHandlers.Invoke(Self, Result);
  finally
    EndUpdate;
  end;
end;

procedure TdxGanttControlCalendars.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Count = 0 then
    CreateStandardCalendar;
end;

function TdxGanttControlCalendars.Create24HoursCalendar
  : TdxGanttControlCalendar;

  procedure AppendWeekDay(ADayType: TdxGanttControlWeekDayType);
  var
    AWeekDay: TdxGanttControlCalendarWeekDay;
  begin
    AWeekDay := Result.WeekDays.Append;
    AWeekDay.DayType := ADayType;
    AWeekDay.Workday := True;
    with AWeekDay.WorkTimes.Append do
    begin
      FromTime := H24CalendarWorkTimeFrom;
      ToTime := H24CalendarWorkTimeTo;
    end;
  end;

begin
  BeginUpdate;
  try
    Result := Append;
    Result.Name := cxGetResourceString(@sdxGanttControl24HoursCalendarName);
    Result.BaseCalendarUID := 0;
    AppendWeekDay(TdxGanttControlWeekDayType.Sunday);
    AppendWeekDay(TdxGanttControlWeekDayType.Monday);
    AppendWeekDay(TdxGanttControlWeekDayType.Tuesday);
    AppendWeekDay(TdxGanttControlWeekDayType.Wednesday);
    AppendWeekDay(TdxGanttControlWeekDayType.Thursday);
    AppendWeekDay(TdxGanttControlWeekDayType.Friday);
    AppendWeekDay(TdxGanttControlWeekDayType.Saturday);
  finally
    EndUpdate;
  end;
end;

function TdxGanttControlCalendars.CreateNightCalendar: TdxGanttControlCalendar;

  procedure AppendWeekDay(ADayType: TdxGanttControlWeekDayType;
    AWorkday: Boolean; AWorkTimes: Integer);
  var
    AWeekDay: TdxGanttControlCalendarWeekDay;
  begin
    AWeekDay := Result.WeekDays.Append;
    AWeekDay.DayType := ADayType;
    AWeekDay.Workday := AWorkday;
    if AWorkday then
      case AWorkTimes of
        3:
          with AWeekDay.WorkTimes.Append do
          begin
            FromTime := NightCalendarWorkTimeFrom3;
            ToTime := NightCalendarWorkTimeTo3;
          end;

        12:
          begin
            with AWeekDay.WorkTimes.Append do
            begin
              FromTime := NightCalendarWorkTimeFrom1;
              ToTime := NightCalendarWorkTimeTo1;
            end;
            with AWeekDay.WorkTimes.Append do
            begin
              FromTime := NightCalendarWorkTimeFrom2;
              ToTime := NightCalendarWorkTimeTo2;
            end;
          end;

        123:
          begin
            with AWeekDay.WorkTimes.Append do
            begin
              FromTime := NightCalendarWorkTimeFrom1;
              ToTime := NightCalendarWorkTimeTo1;
            end;
            with AWeekDay.WorkTimes.Append do
            begin
              FromTime := NightCalendarWorkTimeFrom2;
              ToTime := NightCalendarWorkTimeTo2;
            end;
            with AWeekDay.WorkTimes.Append do
            begin
              FromTime := NightCalendarWorkTimeFrom3;
              ToTime := NightCalendarWorkTimeTo3;
            end;
          end;
      end;
  end;

begin
  BeginUpdate;
  try
    Result := Append;
    Result.Name := cxGetResourceString(@sdxGanttControlNightCalendarName);
    Result.BaseCalendarUID := 0;
    AppendWeekDay(TdxGanttControlWeekDayType.Sunday, False, 0);
    AppendWeekDay(TdxGanttControlWeekDayType.Monday, True, 3);
    AppendWeekDay(TdxGanttControlWeekDayType.Tuesday, True, 123);
    AppendWeekDay(TdxGanttControlWeekDayType.Wednesday, True, 123);
    AppendWeekDay(TdxGanttControlWeekDayType.Thursday, True, 123);
    AppendWeekDay(TdxGanttControlWeekDayType.Friday, True, 123);
    AppendWeekDay(TdxGanttControlWeekDayType.Saturday, True, 12);
  finally
    EndUpdate;
  end;
end;

function TdxGanttControlCalendars.CreateItem
  : TdxGanttControlModelElementListItem;
begin
  Result := TdxGanttControlCalendar.Create(Self);
end;

procedure TdxGanttControlCalendars.DoItemChanged
  (AItem: TdxGanttControlModelElementListItem);
begin
  TdxGanttControlDataModelAccess(DataModel).CalendarsChangedHandler
    (Self, AItem);
end;

procedure TdxGanttControlCalendars.DoReset;
begin
  inherited DoReset;
  FStandardCalendar := CreateStandardCalendar;
end;

function TdxGanttControlCalendars.GetItem(Index: Integer)
  : TdxGanttControlCalendar;
begin
  Result := TdxGanttControlCalendar(inherited Items[Index]);
end;

end.
