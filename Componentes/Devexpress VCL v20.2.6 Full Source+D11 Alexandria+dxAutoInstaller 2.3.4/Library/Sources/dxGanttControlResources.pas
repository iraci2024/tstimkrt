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

unit dxGanttControlResources;

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
  dxGanttControlOutlineCodes,
  dxGanttControlExtendedAttributes;

type
  TdxGanttControlResourceType = (Material, Work, Cost);
  TdxGanttControlRateFormat = (Minutes = 1, Hours, Days, Weeks, Months, Years,
    MaterialResourceRate);
  TdxGanttControlResourceWorkGroup = (Default, None, EmailOnly, Web);

  { TdxGanttControlResourceAvailabilityPeriod }

  TdxGanttResourceAvailabilityPeriodAssignedValue = (AvailableFrom, AvailableTo,
    AvailableUnits);
  TdxGanttResourceAvailabilityPeriodAssignedValues = set of
    TdxGanttResourceAvailabilityPeriodAssignedValue;

  TdxGanttControlResourceAvailabilityPeriod = class
    (TdxGanttControlModelElementListItem)
  strict private
    FAssignedValues: TdxGanttResourceAvailabilityPeriodAssignedValues;
    FAvailableFrom: TDateTime;
    FAvailableTo: TDateTime;
    FAvailableUnits: Double;
  private
    procedure SetAvailableFrom(const Value: TDateTime);
    procedure SetAvailableTo(const Value: TDateTime);
    procedure SetAvailableUnits(const Value: Double);
  protected
    procedure DoReset; override;
  public
    procedure Assign(Source: TPersistent); override;
{$REGION 'for internal use'}
    function IsValueAssigned(const AValue
      : TdxGanttResourceAvailabilityPeriodAssignedValue): Boolean;
    procedure ResetValue(const AValue
      : TdxGanttResourceAvailabilityPeriodAssignedValue); overload;
{$ENDREGION 'for internal use'}
    property AvailableFrom: TDateTime read FAvailableFrom
      write SetAvailableFrom;
    property AvailableTo: TDateTime read FAvailableTo write SetAvailableTo;
    property AvailableUnits: Double read FAvailableUnits
      write SetAvailableUnits;
  end;

  { TdxGanttControlResourceAvailabilityPeriods }

  TdxGanttControlResourceAvailabilityPeriods = class
    (TdxGanttControlElementCustomOwnedList)
  strict private
    function GetItem(Index: Integer)
      : TdxGanttControlResourceAvailabilityPeriod; inline;
  protected
    function CreateItem: TdxGanttControlModelElementListItem; override;
  public
    function Append: TdxGanttControlResourceAvailabilityPeriod;
    procedure Clear;
    procedure Remove(AItem: TdxGanttControlResourceAvailabilityPeriod);

    property Items[Index: Integer]: TdxGanttControlResourceAvailabilityPeriod
      read GetItem; default;
  end;

  { TdxGanttControlResourceRate }

  TdxGanttResourceRateAssignedValue = (RatesTo, RateTable, StandardRate,
    StandardRateFormat, OvertimeRate, OvertimeRateFormat, CostPerUse);
  TdxGanttResourceRateAssignedValues = set of TdxGanttResourceRateAssignedValue;

  TdxGanttControlResourceRate = class(TdxGanttControlModelElementListItem)
  strict private
    FAssignedValues: TdxGanttResourceRateAssignedValues;
    FRatesFrom: TDateTime;
    FRatesTo: TDateTime;
    FRateTable: TdxGanttControlCostRateTable;
    FStandardRate: Double;
    FStandardRateFormat: TdxGanttControlRateFormat;
    FOvertimeRate: Double;
    FOvertimeRateFormat: TdxGanttControlRateFormat;
    FCostPerUse: Double;
  private
    procedure SetCostPerUse(const Value: Double);
    procedure SetOvertimeRate(const Value: Double);
    procedure SetOvertimeRateFormat(const Value: TdxGanttControlRateFormat);
    procedure SetRatesFrom(const Value: TDateTime);
    procedure SetRatesTo(const Value: TDateTime);
    procedure SetRateTable(const Value: TdxGanttControlCostRateTable);
    procedure SetStandardRate(const Value: Double);
    procedure SetStandardRateFormat(const Value: TdxGanttControlRateFormat);
  protected
    procedure DoReset; override;
  public
    procedure Assign(Source: TPersistent); override;
{$REGION 'for internal use'}
    function IsValueAssigned(const AValue
      : TdxGanttResourceRateAssignedValue): Boolean;
    procedure ResetValue(const AValue
      : TdxGanttResourceRateAssignedValue); overload;
{$ENDREGION 'for internal use'}
    property RatesFrom: TDateTime read FRatesFrom write SetRatesFrom;
    property RatesTo: TDateTime read FRatesTo write SetRatesTo;
    property RateTable: TdxGanttControlCostRateTable read FRateTable
      write SetRateTable;
    property StandardRate: Double read FStandardRate write SetStandardRate;
    property StandardRateFormat: TdxGanttControlRateFormat
      read FStandardRateFormat write SetStandardRateFormat;
    property OvertimeRate: Double read FOvertimeRate write SetOvertimeRate;
    property OvertimeRateFormat: TdxGanttControlRateFormat
      read FOvertimeRateFormat write SetOvertimeRateFormat;
    property CostPerUse: Double read FCostPerUse write SetCostPerUse;
  end;

  { TdxGanttControlResourceRates }

  TdxGanttControlResourceRates = class(TdxGanttControlElementCustomOwnedList)
  strict private
    function GetItem(Index: Integer): TdxGanttControlResourceRate; inline;
  protected
    function CreateItem: TdxGanttControlModelElementListItem; override;
  public
    function Append: TdxGanttControlResourceRate;
    procedure Clear;
    procedure Remove(AItem: TdxGanttControlResourceRate);

    property Items[Index: Integer]: TdxGanttControlResourceRate
      read GetItem; default;
  end;

  { TdxGanttControlResourceBaseline }

  TdxGanttControlResourceBaseline = class(TdxGanttControlCustomBaseline);

  { TdxGanttControlResourceBaselines }

  TdxGanttControlResourceBaselines = class(TdxGanttControlCustomBaselines)
  protected
    function CreateItem: TdxGanttControlModelElementListItem; override;
  end;

  { TdxGanttControlResource }

  TdxGanttResourceAssignedValue = (Name, &Type, Blank, Initials, Phonetics,
    NTAccount, MaterialLabel, Code, Group, WorkGroup, EmailAddress, Hyperlink,
    HyperlinkAddress, HyperlinkSubAddress, MaxUnits, PeakUnits, OverAllocated,
    AvailableFrom, AvailableTo, Start, Finish, CanLevel, AccrueAt, Work,
    RegularWork, OvertimeWork, ActualWork, RemainingWork, ActualOvertimeWork,
    RemainingOvertimeWork, PercentWorkComplete, StandardRate,
    StandardRateFormat, Cost, OvertimeRate, OvertimeRateFormat, OvertimeCost,
    CostPerUse, ActualCost, ActualOvertimeCost, RemainingCost,
    RemainingOvertimeCost, WorkVariance, CostVariance, SV, CV, ACWP,
    CalendarUID, Notes, BCWS, BCWP, Generic, Inactive, Enterprise, BookingType,
    ActualWorkProtected, ActualOvertimeWorkProtected, ActiveDirectoryGUID,
    CostResource, AssnOwner, AssnOwnerGuid, Budget);

  TdxGanttResourceAssignedValues = set of TdxGanttResourceAssignedValue;

  TdxGanttControlResource = class(TdxGanttControlModelUIDElement)
  strict private
    FAssignedValues: TdxGanttResourceAssignedValues;
    FGUID: string;
    FName: string;
    FType: TdxGanttControlResourceType;
    FBlank: Boolean;
    FInitials: string;
    FPhonetics: string;
    FNTAccount: string;
    FMaterialLabel: string;
    FCode: string;
    FGroup: string;
    FWorkGroup: TdxGanttControlResourceWorkGroup;
    FEmailAddress: string;
    FHyperlink: string;
    FHyperlinkAddress: string;
    FHyperlinkSubAddress: string;
    FMaxUnits: Double;
    FPeakUnits: Double;
    FOverAllocated: Boolean;
    FAvailableFrom: TDateTime;
    FAvailableTo: TDateTime;
    FStart: TDateTime;
    FFinish: TDateTime;
    FCanLevel: Boolean;
    FAccrueAt: Integer;
    FWork: string;
    FRegularWork: string;
    FOvertimeWork: string;
    FActualWork: string;
    FRemainingWork: string;
    FActualOvertimeWork: string;
    FRemainingOvertimeWork: string;
    FPercentWorkComplete: Integer;
    FStandardRate: Double;
    FStandardRateFormat: TdxGanttControlRateFormat;
    FCost: Double;
    FOvertimeRate: Double;
    FOvertimeRateFormat: TdxGanttControlRateFormat;
    FOvertimeCost: Double;
    FCostPerUse: Double;
    FActualCost: Double;
    FActualOvertimeCost: Double;
    FRemainingCost: Double;
    FRemainingOvertimeCost: Double;
    FWorkVariance: Double;
    FCostVariance: Double;
    FSV: Double;
    FCV: Double;
    FACWP: Double;
    FCalendarUID: Integer;
    FNotes: string;
    FBCWS: Double;
    FBCWP: Double;
    FGeneric: Boolean;
    FInactive: Boolean;
    FEnterprise: Boolean;
    FBookingType: TdxGanttControlBookingType;
    FActualWorkProtected: string;
    FActualOvertimeWorkProtected: string;
    FActiveDirectoryGUID: string;
    FCreated: TDateTime;
    FExtendedAttributes: TdxGanttControlExtendedAttributeReferences;
    FBaselines: TdxGanttControlResourceBaselines;
    FOutlineCodes: TdxGanttControlOutlineCodeReferences;
    FCostResource: Boolean;
    FAssnOwner: string;
    FAssnOwnerGuid: string;
    FBudget: Boolean;
    FAvailabilityPeriods: TdxGanttControlResourceAvailabilityPeriods;
    FRates: TdxGanttControlResourceRates;
    FTimephasedDataItems: TdxGanttControlTimephasedDataItems;

    function GetID: Integer;
    procedure SetAccrueAt(const Value: Integer);
    procedure SetActiveDirectoryGUID(const Value: string);
    procedure SetActualCost(const Value: Double);
    procedure SetActualOvertimeCost(const Value: Double);
    procedure SetActualOvertimeWork(const Value: string);
    procedure SetActualOvertimeWorkProtected(const Value: string);
    procedure SetActualWork(const Value: string);
    procedure SetActualWorkProtected(const Value: string);
    procedure SetACWP(const Value: Double);
    procedure SetAssnOwner(const Value: string);
    procedure SetAssnOwnerGuid(const Value: string);
    procedure SetAvailabilityPeriods(const Value
      : TdxGanttControlResourceAvailabilityPeriods);
    procedure SetAvailableFrom(const Value: TDateTime);
    procedure SetAvailableTo(const Value: TDateTime);
    procedure SetBaselines(const Value: TdxGanttControlResourceBaselines);
    procedure SetBCWP(const Value: Double);
    procedure SetBCWS(const Value: Double);
    procedure SetBookingType(const Value: TdxGanttControlBookingType);
    procedure SetCalendarUID(const Value: Integer);
    procedure SetCanLevel(const Value: Boolean);
    procedure SetCode(const Value: string);
    procedure SetCost(const Value: Double);
    procedure SetCostPerUse(const Value: Double);
    procedure SetCostVariance(const Value: Double);
    procedure SetCreated(const Value: TDateTime);
    procedure SetCV(const Value: Double);
    procedure SetEmailAddress(const Value: string);
    procedure SetExtendedAttributes(const Value
      : TdxGanttControlExtendedAttributeReferences);
    procedure SetFinish(const Value: TDateTime);
    procedure SetGroup(const Value: string);
    procedure SetWorkGroup(const Value: TdxGanttControlResourceWorkGroup);
    procedure SetGUID(const Value: string);
    procedure SetHyperlink(const Value: string);
    procedure SetHyperlinkAddress(const Value: string);
    procedure SetHyperlinkSubAddress(const Value: string);
    procedure SetID(const Value: Integer);
    procedure SetInitials(const Value: string);
    procedure SetBudget(const Value: Boolean);
    procedure SetCostResource(const Value: Boolean);
    procedure SetEnterprise(const Value: Boolean);
    procedure SetGeneric(const Value: Boolean);
    procedure SetInactive(const Value: Boolean);
    procedure SetBlank(const Value: Boolean);
    procedure SetMaterialLabel(const Value: string);
    procedure SetMaxUnits(const Value: Double);
    procedure SetName(const Value: string);
    procedure SetNotes(const Value: string);
    procedure SetNTAccount(const Value: string);
    procedure SetOutlineCodes(const Value
      : TdxGanttControlOutlineCodeReferences);
    procedure SetOverAllocated(const Value: Boolean);
    procedure SetOvertimeCost(const Value: Double);
    procedure SetOvertimeRate(const Value: Double);
    procedure SetOvertimeRateFormat(const Value: TdxGanttControlRateFormat);
    procedure SetOvertimeWork(const Value: string);
    procedure SetPeakUnits(const Value: Double);
    procedure SetPercentWorkComplete(const Value: Integer);
    procedure SetPhonetics(const Value: string);
    procedure SetRates(const Value: TdxGanttControlResourceRates);
    procedure SetRegularWork(const Value: string);
    procedure SetRemainingCost(const Value: Double);
    procedure SetRemainingOvertimeCost(const Value: Double);
    procedure SetRemainingOvertimeWork(const Value: string);
    procedure SetRemainingWork(const Value: string);
    procedure SetStandardRate(const Value: Double);
    procedure SetStandardRateFormat(const Value: TdxGanttControlRateFormat);
    procedure SetStart(const Value: TDateTime);
    procedure SetSV(const Value: Double);
    procedure SetTimephasedDataItems(const Value
      : TdxGanttControlTimephasedDataItems);
    procedure SetType(const Value: TdxGanttControlResourceType);
    procedure SetWork(const Value: string);
    procedure SetWorkVariance(const Value: Double);
  protected
    procedure DoReset; override;

    property GUID: string read FGUID write SetGUID;
  public
    constructor Create(AOwner: TdxGanttControlModelElement); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
{$REGION 'for internal use'}
    function IsValueAssigned(const AValue
      : TdxGanttResourceAssignedValue): Boolean;
    procedure ResetValue(const AValue: TdxGanttResourceAssignedValue); overload;
{$ENDREGION 'for internal use'}
    property CalendarUID: Integer read FCalendarUID write SetCalendarUID;
    property Created: TDateTime read FCreated write SetCreated;
    property Group: string read FGroup write SetGroup;
    property ID: Integer read GetID write SetID;
    property Blank: Boolean read FBlank write SetBlank;
    property Name: string read FName write SetName;
    property &Type: TdxGanttControlResourceType read FType write SetType;

{$REGION 'for internal use'}
    property Initials: string read FInitials write SetInitials;
    property Phonetics: string read FPhonetics write SetPhonetics;
    property NTAccount: string read FNTAccount write SetNTAccount;
    property MaterialLabel: string read FMaterialLabel write SetMaterialLabel;
    property Code: string read FCode write SetCode;
    property WorkGroup: TdxGanttControlResourceWorkGroup read FWorkGroup
      write SetWorkGroup;
    property EmailAddress: string read FEmailAddress write SetEmailAddress;
    property Hyperlink: string read FHyperlink write SetHyperlink;
    property HyperlinkAddress: string read FHyperlinkAddress
      write SetHyperlinkAddress;
    property HyperlinkSubAddress: string read FHyperlinkSubAddress
      write SetHyperlinkSubAddress;
    property MaxUnits: Double read FMaxUnits write SetMaxUnits;
    property PeakUnits: Double read FPeakUnits write SetPeakUnits;
    property OverAllocated: Boolean read FOverAllocated write SetOverAllocated;
    property AvailableFrom: TDateTime read FAvailableFrom
      write SetAvailableFrom;
    property AvailableTo: TDateTime read FAvailableTo write SetAvailableTo;
    property Start: TDateTime read FStart write SetStart;
    property Finish: TDateTime read FFinish write SetFinish;
    property CanLevel: Boolean read FCanLevel write SetCanLevel;
    property AccrueAt: Integer read FAccrueAt write SetAccrueAt;
    property Work: string read FWork write SetWork;
    property RegularWork: string read FRegularWork write SetRegularWork;
    property OvertimeWork: string read FOvertimeWork write SetOvertimeWork;
    property ActualWork: string read FActualWork write SetActualWork;
    property RemainingWork: string read FRemainingWork write SetRemainingWork;
    property ActualOvertimeWork: string read FActualOvertimeWork
      write SetActualOvertimeWork;
    property RemainingOvertimeWork: string read FRemainingOvertimeWork
      write SetRemainingOvertimeWork;
    property PercentWorkComplete: Integer read FPercentWorkComplete
      write SetPercentWorkComplete;
    property StandardRate: Double read FStandardRate write SetStandardRate;
    property StandardRateFormat: TdxGanttControlRateFormat
      read FStandardRateFormat write SetStandardRateFormat;
    property Cost: Double read FCost write SetCost;
    property OvertimeRate: Double read FOvertimeRate write SetOvertimeRate;
    property OvertimeRateFormat: TdxGanttControlRateFormat
      read FOvertimeRateFormat write SetOvertimeRateFormat;
    property OvertimeCost: Double read FOvertimeCost write SetOvertimeCost;
    property CostPerUse: Double read FCostPerUse write SetCostPerUse;
    property ActualCost: Double read FActualCost write SetActualCost;
    property ActualOvertimeCost: Double read FActualOvertimeCost
      write SetActualOvertimeCost;
    property RemainingCost: Double read FRemainingCost write SetRemainingCost;
    property RemainingOvertimeCost: Double read FRemainingOvertimeCost
      write SetRemainingOvertimeCost;
    property WorkVariance: Double read FWorkVariance write SetWorkVariance;
    property CostVariance: Double read FCostVariance write SetCostVariance;
    property SV: Double read FSV write SetSV;
    property CV: Double read FCV write SetCV;
    property ACWP: Double read FACWP write SetACWP;
    property Notes: string read FNotes write SetNotes;
    property BCWS: Double read FBCWS write SetBCWS;
    property BCWP: Double read FBCWP write SetBCWP;
    property Generic: Boolean read FGeneric write SetGeneric;
    property Inactive: Boolean read FInactive write SetInactive;
    property Enterprise: Boolean read FEnterprise write SetEnterprise;
    property BookingType: TdxGanttControlBookingType read FBookingType
      write SetBookingType;
    property ActualWorkProtected: string read FActualWorkProtected
      write SetActualWorkProtected;
    property ActualOvertimeWorkProtected: string
      read FActualOvertimeWorkProtected write SetActualOvertimeWorkProtected;
    property ActiveDirectoryGUID: string read FActiveDirectoryGUID
      write SetActiveDirectoryGUID;
    property CostResource: Boolean read FCostResource write SetCostResource;
    property AssnOwner: string read FAssnOwner write SetAssnOwner;
    property AssnOwnerGuid: string read FAssnOwnerGuid write SetAssnOwnerGuid;
    property Budget: Boolean read FBudget write SetBudget;
    property AvailabilityPeriods: TdxGanttControlResourceAvailabilityPeriods
      read FAvailabilityPeriods write SetAvailabilityPeriods;
    property ExtendedAttributes: TdxGanttControlExtendedAttributeReferences
      read FExtendedAttributes write SetExtendedAttributes;
    property Baselines: TdxGanttControlResourceBaselines read FBaselines
      write SetBaselines;
    property OutlineCodes: TdxGanttControlOutlineCodeReferences
      read FOutlineCodes write SetOutlineCodes;
    property Rates: TdxGanttControlResourceRates read FRates write SetRates;
    property TimephasedDataItems: TdxGanttControlTimephasedDataItems
      read FTimephasedDataItems write SetTimephasedDataItems;
{$ENDREGION}
  end;

  { TdxGanttControlResources }

  TdxGanttControlResources = class(TdxGanttControlModelUIDElementList)
  strict private
  type
    TCreateDefaultResourceEvent = procedure(Sender: TObject;
      AResource: TdxGanttControlResource) of object;
    TCreateDefaultResourceHandlers =
      TdxMulticastMethod<TCreateDefaultResourceEvent>;
  private
    FCreateDefaultResourceHandlers: TCreateDefaultResourceHandlers;
    function GetItem(Index: Integer): TdxGanttControlResource; inline;
  protected
    function AppendEmpty: TdxGanttControlResource;
    function CreateItem: TdxGanttControlModelElementListItem; override;
    procedure DoReset; override;
    procedure DoItemChanged
      (AItem: TdxGanttControlModelElementListItem); override;
    procedure InsertEmpty(Index, ACount: Integer);

    function CreateDefaultResource: TdxGanttControlResource; virtual;
  public
    procedure Assign(Source: TPersistent); override;
    function Append: TdxGanttControlResource;
    function GetItemByName(const AName: string): TdxGanttControlResource;
    function Insert(Index: Integer): TdxGanttControlResource;
    procedure Delete(Index: Integer);
    procedure Remove(AItem: TdxGanttControlResource);

    property Items[Index: Integer]: TdxGanttControlResource
      read GetItem; default;
{$REGION 'for internal use'}
    property CreateDefaultResourceHandlers: TCreateDefaultResourceHandlers
      read FCreateDefaultResourceHandlers;
{$ENDREGION}
  end;

implementation

uses
  Math, RTLConsts,
  dxGanttControlUtils,
  dxGanttControlDataModel;

type
  TdxGanttControlDataModelAccess = class(TdxGanttControlDataModel);

  { TdxGanttControlResourceAvailabilityPeriod }

procedure TdxGanttControlResourceAvailabilityPeriod.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlResourceAvailabilityPeriod;
begin
  if Source is TdxGanttControlResourceAvailabilityPeriod then
  begin
    ASource := TdxGanttControlResourceAvailabilityPeriod(Source);
    BeginUpdate;
    try
      AvailableFrom := ASource.AvailableFrom;
      AvailableTo := ASource.AvailableTo;
      AvailableUnits := ASource.AvailableUnits;
      FAssignedValues := ASource.FAssignedValues;
    finally
      EndUpdate;
    end;
  end;
  inherited Assign(Source);
end;

procedure TdxGanttControlResourceAvailabilityPeriod.DoReset;
begin
  FAssignedValues := [];
end;

function TdxGanttControlResourceAvailabilityPeriod.IsValueAssigned
  (const AValue: TdxGanttResourceAvailabilityPeriodAssignedValue): Boolean;
begin
  Result := AValue in FAssignedValues;
end;

procedure TdxGanttControlResourceAvailabilityPeriod.ResetValue
  (const AValue: TdxGanttResourceAvailabilityPeriodAssignedValue);
begin
  if IsValueAssigned(AValue) then
  begin
    Exclude(FAssignedValues, AValue);
    Changed;
  end
end;

procedure TdxGanttControlResourceAvailabilityPeriod.SetAvailableFrom
  (const Value: TDateTime);
begin
  if (AvailableFrom <> Value) or not IsValueAssigned
    (TdxGanttResourceAvailabilityPeriodAssignedValue.AvailableFrom) then
  begin
    Include(FAssignedValues, TdxGanttResourceAvailabilityPeriodAssignedValue.
      AvailableFrom);
    FAvailableFrom := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResourceAvailabilityPeriod.SetAvailableTo
  (const Value: TDateTime);
begin
  if (AvailableTo <> Value) or not IsValueAssigned
    (TdxGanttResourceAvailabilityPeriodAssignedValue.AvailableTo) then
  begin
    Include(FAssignedValues, TdxGanttResourceAvailabilityPeriodAssignedValue.
      AvailableTo);
    FAvailableTo := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResourceAvailabilityPeriod.SetAvailableUnits
  (const Value: Double);
begin
  if not(SameValue(AvailableUnits, Value) and
    IsValueAssigned(TdxGanttResourceAvailabilityPeriodAssignedValue.
    AvailableUnits)) then
  begin
    Include(FAssignedValues, TdxGanttResourceAvailabilityPeriodAssignedValue.
      AvailableUnits);
    FAvailableUnits := Value;
    Changed;
  end;
end;

{ TdxGanttControlResourceAvailabilityPeriods }

function TdxGanttControlResourceAvailabilityPeriods.Append
  : TdxGanttControlResourceAvailabilityPeriod;
begin
  Result := TdxGanttControlResourceAvailabilityPeriod(CreateItem);
  InternalAdd(Result);
end;

procedure TdxGanttControlResourceAvailabilityPeriods.Clear;
begin
  InternalClear;
end;

function TdxGanttControlResourceAvailabilityPeriods.CreateItem
  : TdxGanttControlModelElementListItem;
begin
  Result := TdxGanttControlResourceAvailabilityPeriod.Create(Self);
end;

function TdxGanttControlResourceAvailabilityPeriods.GetItem(Index: Integer)
  : TdxGanttControlResourceAvailabilityPeriod;
begin
  Result := TdxGanttControlResourceAvailabilityPeriod(inherited Items[Index]);
end;

procedure TdxGanttControlResourceAvailabilityPeriods.Remove
  (AItem: TdxGanttControlResourceAvailabilityPeriod);
begin
  InternalRemove(AItem);
end;

{ TdxGanttControlResourceRate }

procedure TdxGanttControlResourceRate.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlResourceRate;
begin
  if Source is TdxGanttControlResourceRate then
  begin
    ASource := TdxGanttControlResourceRate(Source);
    BeginUpdate;
    try
      RatesFrom := ASource.RatesFrom;
      RatesTo := ASource.RatesTo;
      RateTable := ASource.RateTable;
      StandardRate := ASource.StandardRate;
      StandardRateFormat := ASource.StandardRateFormat;
      OvertimeRate := ASource.OvertimeRate;
      OvertimeRateFormat := ASource.OvertimeRateFormat;
      CostPerUse := ASource.CostPerUse;
      FAssignedValues := ASource.FAssignedValues;
    finally
      EndUpdate;
    end;
  end;
  inherited Assign(Source);
end;

procedure TdxGanttControlResourceRate.DoReset;
begin
  RatesFrom := TdxGanttControlDataModel.MinDate;
  FAssignedValues := [];
end;

function TdxGanttControlResourceRate.IsValueAssigned(const AValue
  : TdxGanttResourceRateAssignedValue): Boolean;
begin
  Result := AValue in FAssignedValues;
end;

procedure TdxGanttControlResourceRate.ResetValue(const AValue
  : TdxGanttResourceRateAssignedValue);
begin
  if IsValueAssigned(AValue) then
  begin
    Exclude(FAssignedValues, AValue);
    Changed;
  end
end;

procedure TdxGanttControlResourceRate.SetCostPerUse(const Value: Double);
begin
  if not(SameValue(CostPerUse, Value) and
    IsValueAssigned(TdxGanttResourceRateAssignedValue.CostPerUse)) then
  begin
    Include(FAssignedValues, TdxGanttResourceRateAssignedValue.CostPerUse);
    FCostPerUse := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResourceRate.SetOvertimeRate(const Value: Double);
begin
  if not(SameValue(OvertimeRate, Value) and
    IsValueAssigned(TdxGanttResourceRateAssignedValue.OvertimeRate)) then
  begin
    Include(FAssignedValues, TdxGanttResourceRateAssignedValue.OvertimeRate);
    FOvertimeRate := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResourceRate.SetOvertimeRateFormat
  (const Value: TdxGanttControlRateFormat);
begin
  if (OvertimeRateFormat <> Value) or
    not IsValueAssigned(TdxGanttResourceRateAssignedValue.OvertimeRateFormat)
  then
  begin
    Include(FAssignedValues,
      TdxGanttResourceRateAssignedValue.OvertimeRateFormat);
    FOvertimeRateFormat := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResourceRate.SetRatesFrom(const Value: TDateTime);
begin
  if RatesFrom <> Value then
  begin
    FRatesFrom := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResourceRate.SetRatesTo(const Value: TDateTime);
begin
  if (RatesTo <> Value) or not IsValueAssigned
    (TdxGanttResourceRateAssignedValue.RatesTo) then
  begin
    Include(FAssignedValues, TdxGanttResourceRateAssignedValue.RatesTo);
    FRatesTo := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResourceRate.SetRateTable
  (const Value: TdxGanttControlCostRateTable);
begin
  if (RateTable <> Value) or not IsValueAssigned
    (TdxGanttResourceRateAssignedValue.RateTable) then
  begin
    Include(FAssignedValues, TdxGanttResourceRateAssignedValue.RateTable);
    FRateTable := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResourceRate.SetStandardRate(const Value: Double);
begin
  if not(SameValue(StandardRate, Value) and
    IsValueAssigned(TdxGanttResourceRateAssignedValue.StandardRate)) then
  begin
    Include(FAssignedValues, TdxGanttResourceRateAssignedValue.StandardRate);
    FStandardRate := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResourceRate.SetStandardRateFormat
  (const Value: TdxGanttControlRateFormat);
begin
  if (StandardRateFormat <> Value) or
    not IsValueAssigned(TdxGanttResourceRateAssignedValue.StandardRateFormat)
  then
  begin
    Include(FAssignedValues,
      TdxGanttResourceRateAssignedValue.StandardRateFormat);
    FStandardRateFormat := Value;
    Changed;
  end;
end;

{ TdxGanttControlResourceRates }

function TdxGanttControlResourceRates.Append: TdxGanttControlResourceRate;
begin
  Result := TdxGanttControlResourceRate(CreateItem);
  InternalAdd(Result);
end;

procedure TdxGanttControlResourceRates.Clear;
begin
  InternalClear;
end;

function TdxGanttControlResourceRates.CreateItem
  : TdxGanttControlModelElementListItem;
begin
  Result := TdxGanttControlResourceRate.Create(Self);
end;

function TdxGanttControlResourceRates.GetItem(Index: Integer)
  : TdxGanttControlResourceRate;
begin
  Result := TdxGanttControlResourceRate(inherited Items[Index]);
end;

procedure TdxGanttControlResourceRates.Remove
  (AItem: TdxGanttControlResourceRate);
begin
  InternalRemove(AItem);
end;

{ TdxGanttControlResourceBaselines }

function TdxGanttControlResourceBaselines.CreateItem
  : TdxGanttControlModelElementListItem;
begin
  Result := TdxGanttControlResourceBaseline.Create(Self);
end;

{ TdxGanttControlResource }

constructor TdxGanttControlResource.Create(AOwner: TdxGanttControlModelElement);
begin
  inherited Create(AOwner);
  FExtendedAttributes := TdxGanttControlExtendedAttributeReferences.
    Create(Self);
  FBaselines := TdxGanttControlResourceBaselines.Create(Self);
  FOutlineCodes := TdxGanttControlOutlineCodeReferences.Create(Self);
  FAvailabilityPeriods := TdxGanttControlResourceAvailabilityPeriods.
    Create(Self);
  FRates := TdxGanttControlResourceRates.Create(Self);
  FTimephasedDataItems := TdxGanttControlTimephasedDataItems.Create(Self);
end;

destructor TdxGanttControlResource.Destroy;
begin
  FreeAndNil(FTimephasedDataItems);
  FreeAndNil(FRates);
  FreeAndNil(FAvailabilityPeriods);
  FreeAndNil(FOutlineCodes);
  FreeAndNil(FBaselines);
  FreeAndNil(FExtendedAttributes);
  inherited Destroy;
end;

procedure TdxGanttControlResource.Assign(Source: TPersistent);
var
  ASource: TdxGanttControlResource;
begin
  if Source is TdxGanttControlResource then
  begin
    ASource := TdxGanttControlResource(Source);
    BeginUpdate;
    try
      Index := ASource.Index;
      GUID := ASource.GUID;
      Name := ASource.Name;
      &Type := ASource.&Type;
      Blank := ASource.Blank;
      Initials := ASource.Initials;
      Phonetics := ASource.Phonetics;
      NTAccount := ASource.NTAccount;
      MaterialLabel := ASource.MaterialLabel;
      Code := ASource.Code;
      Group := ASource.Group;
      EmailAddress := ASource.EmailAddress;
      Hyperlink := ASource.Hyperlink;
      HyperlinkAddress := ASource.HyperlinkAddress;
      HyperlinkSubAddress := ASource.HyperlinkSubAddress;
      MaxUnits := ASource.MaxUnits;
      PeakUnits := ASource.PeakUnits;
      OverAllocated := ASource.OverAllocated;
      AvailableFrom := ASource.AvailableFrom;
      AvailableTo := ASource.AvailableTo;
      Start := ASource.Start;
      Finish := ASource.Finish;
      CanLevel := ASource.CanLevel;
      AccrueAt := ASource.AccrueAt;
      Work := ASource.Work;
      RegularWork := ASource.RegularWork;
      OvertimeWork := ASource.OvertimeWork;
      ActualWork := ASource.ActualWork;
      RemainingWork := ASource.RemainingWork;
      ActualOvertimeWork := ASource.ActualOvertimeWork;
      RemainingOvertimeWork := ASource.RemainingOvertimeWork;
      PercentWorkComplete := ASource.PercentWorkComplete;
      StandardRate := ASource.StandardRate;
      StandardRateFormat := ASource.StandardRateFormat;
      Cost := ASource.Cost;
      OvertimeRate := ASource.OvertimeRate;
      OvertimeRateFormat := ASource.OvertimeRateFormat;
      OvertimeCost := ASource.OvertimeCost;
      CostPerUse := ASource.CostPerUse;
      ActualCost := ASource.ActualCost;
      ActualOvertimeCost := ASource.ActualOvertimeCost;
      RemainingCost := ASource.RemainingCost;
      RemainingOvertimeCost := ASource.RemainingOvertimeCost;
      WorkVariance := ASource.WorkVariance;
      CostVariance := ASource.CostVariance;
      SV := ASource.SV;
      CV := ASource.CV;
      ACWP := ASource.ACWP;
      CalendarUID := ASource.CalendarUID;
      Notes := ASource.Notes;
      BCWS := ASource.BCWS;
      BCWP := ASource.BCWP;
      Generic := ASource.Generic;
      Inactive := ASource.Inactive;
      Enterprise := ASource.Enterprise;
      BookingType := ASource.BookingType;
      ActualWorkProtected := ASource.ActualWorkProtected;
      ActualOvertimeWorkProtected := ASource.ActualOvertimeWorkProtected;
      ActiveDirectoryGUID := ASource.ActiveDirectoryGUID;
      Created := ASource.Created;
      ExtendedAttributes := ASource.ExtendedAttributes;
      Baselines := ASource.Baselines;
      OutlineCodes := ASource.OutlineCodes;
      CostResource := ASource.CostResource;
      AssnOwner := ASource.AssnOwner;
      AssnOwnerGuid := ASource.AssnOwnerGuid;
      Budget := ASource.Budget;
      AvailabilityPeriods := ASource.AvailabilityPeriods;
      Rates := ASource.Rates;
      TimephasedDataItems := ASource.TimephasedDataItems;
      FAssignedValues := ASource.FAssignedValues;
    finally
      EndUpdate;
    end;
  end;
  inherited Assign(Source);
end;

procedure TdxGanttControlResource.DoReset;
begin
  inherited DoReset;
  FGUID := TdxGanttControlUtils.GenerateGUID;
  FCreated := Now;
  FAvailabilityPeriods.Reset;
  FExtendedAttributes.Reset;
  FBaselines.Reset;
  FOutlineCodes.Reset;
  FRates.Reset;
  FTimephasedDataItems.Reset;
  FAssignedValues := [];
end;

function TdxGanttControlResource.GetID: Integer;
begin
  Result := Index;
end;

function TdxGanttControlResource.IsValueAssigned(const AValue
  : TdxGanttResourceAssignedValue): Boolean;
begin
  Result := AValue in FAssignedValues;
end;

procedure TdxGanttControlResource.ResetValue(const AValue
  : TdxGanttResourceAssignedValue);
begin
  if IsValueAssigned(AValue) then
  begin
    Exclude(FAssignedValues, AValue);
    Changed;
  end
end;

procedure TdxGanttControlResource.SetAccrueAt(const Value: Integer);
begin
  if (AccrueAt <> Value) or not IsValueAssigned
    (TdxGanttResourceAssignedValue.AccrueAt) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.AccrueAt);
    FAccrueAt := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetActiveDirectoryGUID(const Value: string);
begin
  if (ActiveDirectoryGUID <> Value) or
    not IsValueAssigned(TdxGanttResourceAssignedValue.ActiveDirectoryGUID) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.ActiveDirectoryGUID);
    FActiveDirectoryGUID := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetActualCost(const Value: Double);
begin
  if not(SameValue(ActualCost, Value) and
    IsValueAssigned(TdxGanttResourceAssignedValue.ActualCost)) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.ActualCost);
    FActualCost := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetActualOvertimeCost(const Value: Double);
begin
  if not(SameValue(ActualOvertimeCost, Value) and
    IsValueAssigned(TdxGanttResourceAssignedValue.ActualOvertimeCost)) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.ActualOvertimeCost);
    FActualOvertimeCost := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetActualOvertimeWork(const Value: string);
begin
  if (ActualOvertimeWork <> Value) or
    not IsValueAssigned(TdxGanttResourceAssignedValue.ActualOvertimeWork) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.ActualOvertimeWork);
    FActualOvertimeWork := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetActualOvertimeWorkProtected
  (const Value: string);
begin
  if (ActualOvertimeWorkProtected <> Value) or
    not IsValueAssigned
    (TdxGanttResourceAssignedValue.ActualOvertimeWorkProtected) then
  begin
    Include(FAssignedValues,
      TdxGanttResourceAssignedValue.ActualOvertimeWorkProtected);
    FActualOvertimeWorkProtected := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetActualWork(const Value: string);
begin
  if (ActualWork <> Value) or not IsValueAssigned
    (TdxGanttResourceAssignedValue.ActualWork) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.ActualWork);
    FActualWork := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetActualWorkProtected(const Value: string);
begin
  if (ActualWorkProtected <> Value) or
    not IsValueAssigned(TdxGanttResourceAssignedValue.ActualWorkProtected) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.ActualWorkProtected);
    FActualWorkProtected := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetACWP(const Value: Double);
begin
  if not(SameValue(ACWP, Value) and
    IsValueAssigned(TdxGanttResourceAssignedValue.ACWP)) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.ACWP);
    FACWP := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetAssnOwner(const Value: string);
begin
  if (AssnOwner <> Value) or not IsValueAssigned
    (TdxGanttResourceAssignedValue.AssnOwner) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.AssnOwner);
    FAssnOwner := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetAssnOwnerGuid(const Value: string);
begin
  if (AssnOwnerGuid <> Value) or not IsValueAssigned
    (TdxGanttResourceAssignedValue.AssnOwnerGuid) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.AssnOwnerGuid);
    FAssnOwnerGuid := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetAvailabilityPeriods
  (const Value: TdxGanttControlResourceAvailabilityPeriods);
begin
  FAvailabilityPeriods.Assign(Value);
end;

procedure TdxGanttControlResource.SetAvailableFrom(const Value: TDateTime);
begin
  if (AvailableFrom <> Value) or not IsValueAssigned
    (TdxGanttResourceAssignedValue.AvailableFrom) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.AvailableFrom);
    FAvailableFrom := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetAvailableTo(const Value: TDateTime);
begin
  if (AvailableTo <> Value) or not IsValueAssigned
    (TdxGanttResourceAssignedValue.AvailableTo) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.AvailableTo);
    FAvailableTo := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetBaselines(const Value
  : TdxGanttControlResourceBaselines);
begin
  FBaselines.Assign(Value);
end;

procedure TdxGanttControlResource.SetBCWP(const Value: Double);
begin
  if not(SameValue(BCWP, Value) and
    IsValueAssigned(TdxGanttResourceAssignedValue.BCWP)) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.BCWP);
    FBCWP := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetBCWS(const Value: Double);
begin
  if not(SameValue(BCWS, Value) and
    IsValueAssigned(TdxGanttResourceAssignedValue.BCWS)) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.BCWS);
    FBCWS := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetBookingType(const Value
  : TdxGanttControlBookingType);
begin
  if (BookingType <> Value) or not IsValueAssigned
    (TdxGanttResourceAssignedValue.BookingType) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.BookingType);
    FBookingType := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetCalendarUID(const Value: Integer);
begin
  if (CalendarUID <> Value) or not IsValueAssigned
    (TdxGanttResourceAssignedValue.CalendarUID) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.CalendarUID);
    FCalendarUID := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetCanLevel(const Value: Boolean);
begin
  if (CanLevel <> Value) or not IsValueAssigned
    (TdxGanttResourceAssignedValue.CanLevel) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.CanLevel);
    FCanLevel := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetCode(const Value: string);
begin
  if (Code <> Value) or not IsValueAssigned(TdxGanttResourceAssignedValue.Code)
  then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.Code);
    FCode := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetCost(const Value: Double);
begin
  if not(SameValue(Cost, Value) and
    IsValueAssigned(TdxGanttResourceAssignedValue.Cost)) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.Cost);
    FCost := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetCostPerUse(const Value: Double);
begin
  if not(SameValue(CostPerUse, Value) and
    IsValueAssigned(TdxGanttResourceAssignedValue.CostPerUse)) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.CostPerUse);
    FCostPerUse := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetCostVariance(const Value: Double);
begin
  if not(SameValue(CostVariance, Value) and
    IsValueAssigned(TdxGanttResourceAssignedValue.CostVariance)) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.CostVariance);
    FCostVariance := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetCreated(const Value: TDateTime);
begin
  if Created <> Value then
  begin
    FCreated := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetCV(const Value: Double);
begin
  if not(SameValue(CV, Value) and
    IsValueAssigned(TdxGanttResourceAssignedValue.CV)) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.CV);
    FCV := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetEmailAddress(const Value: string);
begin
  if (EmailAddress <> Value) or not IsValueAssigned
    (TdxGanttResourceAssignedValue.EmailAddress) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.EmailAddress);
    FEmailAddress := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetExtendedAttributes
  (const Value: TdxGanttControlExtendedAttributeReferences);
begin
  FExtendedAttributes.Assign(Value);
end;

procedure TdxGanttControlResource.SetFinish(const Value: TDateTime);
begin
  if (Finish <> Value) or not IsValueAssigned
    (TdxGanttResourceAssignedValue.Finish) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.Finish);
    FFinish := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetGroup(const Value: string);
begin
  if (Group <> Value) or not IsValueAssigned(TdxGanttResourceAssignedValue.Group)
  then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.Group);
    FGroup := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetWorkGroup(const Value
  : TdxGanttControlResourceWorkGroup);
begin
  if (WorkGroup <> Value) or not IsValueAssigned
    (TdxGanttResourceAssignedValue.WorkGroup) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.WorkGroup);
    FWorkGroup := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetGUID(const Value: string);
begin
  if GUID <> Value then
  begin
    FGUID := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetHyperlink(const Value: string);
begin
  if (Hyperlink <> Value) or not IsValueAssigned
    (TdxGanttResourceAssignedValue.Hyperlink) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.Hyperlink);
    FHyperlink := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetHyperlinkAddress(const Value: string);
begin
  if (HyperlinkAddress <> Value) or
    not IsValueAssigned(TdxGanttResourceAssignedValue.HyperlinkAddress) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.HyperlinkAddress);
    FHyperlinkAddress := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetHyperlinkSubAddress(const Value: string);
begin
  if (HyperlinkSubAddress <> Value) or
    not IsValueAssigned(TdxGanttResourceAssignedValue.HyperlinkSubAddress) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.HyperlinkSubAddress);
    FHyperlinkSubAddress := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetID(const Value: Integer);
begin
  Index := Value;
end;

procedure TdxGanttControlResource.SetInitials(const Value: string);
begin
  if (Initials <> Value) or not IsValueAssigned
    (TdxGanttResourceAssignedValue.Initials) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.Initials);
    FInitials := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetBudget(const Value: Boolean);
begin
  if (Budget <> Value) or not IsValueAssigned
    (TdxGanttResourceAssignedValue.Budget) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.Budget);
    FBudget := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetCostResource(const Value: Boolean);
begin
  if (CostResource <> Value) or not IsValueAssigned
    (TdxGanttResourceAssignedValue.CostResource) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.CostResource);
    FCostResource := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetEnterprise(const Value: Boolean);
begin
  if (Enterprise <> Value) or not IsValueAssigned
    (TdxGanttResourceAssignedValue.Enterprise) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.Enterprise);
    FEnterprise := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetGeneric(const Value: Boolean);
begin
  if (Generic <> Value) or not IsValueAssigned
    (TdxGanttResourceAssignedValue.Generic) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.Generic);
    FGeneric := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetInactive(const Value: Boolean);
begin
  if (Inactive <> Value) or not IsValueAssigned
    (TdxGanttResourceAssignedValue.Inactive) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.Inactive);
    FInactive := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetBlank(const Value: Boolean);
begin
  if (Blank <> Value) or not IsValueAssigned(TdxGanttResourceAssignedValue.Blank)
  then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.Blank);
    FBlank := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetMaterialLabel(const Value: string);
begin
  if (MaterialLabel <> Value) or not IsValueAssigned
    (TdxGanttResourceAssignedValue.MaterialLabel) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.MaterialLabel);
    FMaterialLabel := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetMaxUnits(const Value: Double);
begin
  if not(SameValue(MaxUnits, Value) and
    IsValueAssigned(TdxGanttResourceAssignedValue.MaxUnits)) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.MaxUnits);
    FMaxUnits := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetName(const Value: string);
begin
  if (Name <> Value) or not IsValueAssigned(TdxGanttResourceAssignedValue.Name)
  then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.Name);
    FName := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetNotes(const Value: string);
begin
  if (Notes <> Value) or not IsValueAssigned(TdxGanttResourceAssignedValue.Notes)
  then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.Notes);
    FNotes := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetNTAccount(const Value: string);
begin
  if (NTAccount <> Value) or not IsValueAssigned
    (TdxGanttResourceAssignedValue.NTAccount) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.NTAccount);
    FNTAccount := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetOutlineCodes
  (const Value: TdxGanttControlOutlineCodeReferences);
begin
  FOutlineCodes.Assign(Value);
end;

procedure TdxGanttControlResource.SetOverAllocated(const Value: Boolean);
begin
  if (OverAllocated <> Value) or not IsValueAssigned
    (TdxGanttResourceAssignedValue.OverAllocated) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.OverAllocated);
    FOverAllocated := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetOvertimeCost(const Value: Double);
begin
  if not(SameValue(OvertimeCost, Value) and
    IsValueAssigned(TdxGanttResourceAssignedValue.OvertimeCost)) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.OvertimeCost);
    FOvertimeCost := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetOvertimeRate(const Value: Double);
begin
  if not(SameValue(OvertimeRate, Value) and
    IsValueAssigned(TdxGanttResourceAssignedValue.OvertimeRate)) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.OvertimeRate);
    FOvertimeRate := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetOvertimeRateFormat
  (const Value: TdxGanttControlRateFormat);
begin
  if (OvertimeRateFormat <> Value) or
    not IsValueAssigned(TdxGanttResourceAssignedValue.OvertimeRateFormat) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.OvertimeRateFormat);
    FOvertimeRateFormat := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetOvertimeWork(const Value: string);
begin
  if (OvertimeWork <> Value) or not IsValueAssigned
    (TdxGanttResourceAssignedValue.OvertimeWork) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.OvertimeWork);
    FOvertimeWork := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetPeakUnits(const Value: Double);
begin
  if not(SameValue(PeakUnits, Value) and
    IsValueAssigned(TdxGanttResourceAssignedValue.PeakUnits)) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.PeakUnits);
    FPeakUnits := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetPercentWorkComplete(const Value: Integer);
begin
  if (PercentWorkComplete <> Value) or
    not IsValueAssigned(TdxGanttResourceAssignedValue.PercentWorkComplete) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.PercentWorkComplete);
    FPercentWorkComplete := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetPhonetics(const Value: string);
begin
  if (Phonetics <> Value) or not IsValueAssigned
    (TdxGanttResourceAssignedValue.Phonetics) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.Phonetics);
    FPhonetics := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetRates(const Value
  : TdxGanttControlResourceRates);
begin
  FRates.Assign(Value);
end;

procedure TdxGanttControlResource.SetRegularWork(const Value: string);
begin
  if (RegularWork <> Value) or not IsValueAssigned
    (TdxGanttResourceAssignedValue.RegularWork) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.RegularWork);
    FRegularWork := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetRemainingCost(const Value: Double);
begin
  if not(SameValue(RemainingCost, Value) and
    IsValueAssigned(TdxGanttResourceAssignedValue.RemainingCost)) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.RemainingCost);
    FRemainingCost := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetRemainingOvertimeCost(const Value: Double);
begin
  if not(SameValue(RemainingOvertimeCost, Value) and
    IsValueAssigned(TdxGanttResourceAssignedValue.RemainingOvertimeCost)) then
  begin
    Include(FAssignedValues,
      TdxGanttResourceAssignedValue.RemainingOvertimeCost);
    FRemainingOvertimeCost := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetRemainingOvertimeWork(const Value: string);
begin
  if (RemainingOvertimeWork <> Value) or
    not IsValueAssigned(TdxGanttResourceAssignedValue.RemainingOvertimeWork)
  then
  begin
    Include(FAssignedValues,
      TdxGanttResourceAssignedValue.RemainingOvertimeWork);
    FRemainingOvertimeWork := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetRemainingWork(const Value: string);
begin
  if (RemainingWork <> Value) or not IsValueAssigned
    (TdxGanttResourceAssignedValue.RemainingWork) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.RemainingWork);
    FRemainingWork := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetStandardRate(const Value: Double);
begin
  if not(SameValue(StandardRate, Value) and
    IsValueAssigned(TdxGanttResourceAssignedValue.StandardRate)) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.StandardRate);
    FStandardRate := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetStandardRateFormat
  (const Value: TdxGanttControlRateFormat);
begin
  if (StandardRateFormat <> Value) or
    not IsValueAssigned(TdxGanttResourceAssignedValue.StandardRateFormat) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.StandardRateFormat);
    FStandardRateFormat := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetStart(const Value: TDateTime);
begin
  if (Start <> Value) or not IsValueAssigned(TdxGanttResourceAssignedValue.Start)
  then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.Start);
    FStart := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetSV(const Value: Double);
begin
  if not(SameValue(SV, Value) and
    IsValueAssigned(TdxGanttResourceAssignedValue.SV)) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.SV);
    FSV := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetTimephasedDataItems
  (const Value: TdxGanttControlTimephasedDataItems);
begin
  FTimephasedDataItems.Assign(Value);
end;

procedure TdxGanttControlResource.SetType(const Value
  : TdxGanttControlResourceType);
begin
  if (&Type <> Value) or not IsValueAssigned(TdxGanttResourceAssignedValue.&Type)
  then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.&Type);
    FType := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetWork(const Value: string);
begin
  if (Work <> Value) or not IsValueAssigned(TdxGanttResourceAssignedValue.Work)
  then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.Work);
    FWork := Value;
    Changed;
  end;
end;

procedure TdxGanttControlResource.SetWorkVariance(const Value: Double);
begin
  if not(SameValue(WorkVariance, Value) and
    IsValueAssigned(TdxGanttResourceAssignedValue.WorkVariance)) then
  begin
    Include(FAssignedValues, TdxGanttResourceAssignedValue.WorkVariance);
    FWorkVariance := Value;
    Changed;
  end;
end;

{ TdxGanttControlResources }

function TdxGanttControlResources.Append: TdxGanttControlResource;
begin
  Result := TdxGanttControlResource(CreateItem);
  InternalAdd(Result);
end;

function TdxGanttControlResources.AppendEmpty: TdxGanttControlResource;
begin
  Result := Append;
  Result.Blank := True;
end;

procedure TdxGanttControlResources.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Count = 0 then
    CreateDefaultResource;
end;

function TdxGanttControlResources.GetItemByName(const AName: string)
  : TdxGanttControlResource;
var
  I: Integer;
  S: string;
begin
  Result := nil;
  S := AnsiUpperCase(AName);
  for I := 0 to Count - 1 do
    if AnsiUpperCase(Items[I].Name) = S then
      Exit(Items[I]);
end;

function TdxGanttControlResources.CreateDefaultResource
  : TdxGanttControlResource;
begin
  BeginUpdate;
  try
    Result := Append;
    Result.SetUID(0);
    Result.ID := 0;
    Result.&Type := TdxGanttControlResourceType.Work;
    Result.Blank := False;
    Result.MaxUnits := 1.0;
    Result.PeakUnits := 0.0;
    Result.OverAllocated := False;
    Result.CanLevel := True;
    Result.AccrueAt := 3;
    Result.Work := 'PT0H0M0S';
    Result.RegularWork := 'PT0H0M0S';
    Result.OvertimeWork := 'PT0H0M0S';
    Result.ActualWork := 'PT0H0M0S';
    Result.RemainingWork := 'PT0H0M0S';
    Result.ActualOvertimeWork := 'PT0H0M0S';
    Result.RemainingOvertimeWork := 'PT0H0M0S';
    Result.PercentWorkComplete := 0;
    Result.StandardRate := 0;
    Result.StandardRateFormat := TdxGanttControlRateFormat.Hours;
    Result.Cost := 0;
    Result.OvertimeRate := 0;
    Result.OvertimeRateFormat := TdxGanttControlRateFormat.Hours;
    Result.OvertimeCost := 0;
    Result.CostPerUse := 0;
    Result.ActualCost := 0;
    Result.ActualOvertimeCost := 0;
    Result.RemainingCost := 0;
    Result.RemainingOvertimeCost := 0;
    Result.WorkVariance := 0.0;
    Result.CostVariance := 0;
    Result.SV := 0.0;
    Result.CV := 0.0;
    Result.ACWP := 0.0;
    Result.BCWS := 0.0;
    Result.BCWP := 0.0;
    Result.Generic := False;
    Result.Inactive := False;
    Result.Enterprise := False;
    Result.BookingType := TdxGanttControlBookingType.Committed;
    Result.Created := Now;
    Result.CostResource := False;
    Result.Budget := False;
    if not CreateDefaultResourceHandlers.Empty then
      CreateDefaultResourceHandlers.Invoke(Self, Result);
  finally
    EndUpdate;
  end;
end;

function TdxGanttControlResources.CreateItem
  : TdxGanttControlModelElementListItem;
begin
  Result := TdxGanttControlResource.Create(Self);
end;

procedure TdxGanttControlResources.Delete(Index: Integer);
begin
  InternalRemove(Items[Index]);
end;

procedure TdxGanttControlResources.DoItemChanged
  (AItem: TdxGanttControlModelElementListItem);
begin
  TdxGanttControlDataModelAccess(DataModel).ResourcesChangedHandler
    (Self, AItem);
end;

procedure TdxGanttControlResources.DoReset;
begin
  inherited DoReset;
  CreateDefaultResource;
  ResetAutogeneratedUID;
end;

function TdxGanttControlResources.GetItem(Index: Integer)
  : TdxGanttControlResource;
begin
  Result := TdxGanttControlResource(inherited Items[Index]);
end;

function TdxGanttControlResources.Insert(Index: Integer)
  : TdxGanttControlResource;
begin
  while Count <= Index do
    AppendEmpty;
  Result := Append;
  if Items[Index].Blank then
    InternalRemove(Items[Index]);
  Result.Index := Index;
end;

procedure TdxGanttControlResources.InsertEmpty(Index, ACount: Integer);
var
  I: Integer;
  AItem: TdxGanttControlResource;
begin
  for I := Index to Index + ACount - 1 do
  begin
    AItem := Insert(I);
    AItem.Blank := True;
  end;
end;

procedure TdxGanttControlResources.Remove(AItem: TdxGanttControlResource);
begin
  InternalRemove(AItem);
end;

end.
