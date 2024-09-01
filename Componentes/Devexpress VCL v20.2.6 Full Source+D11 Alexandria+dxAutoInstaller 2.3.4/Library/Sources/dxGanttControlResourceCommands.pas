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

unit dxGanttControlResourceCommands;

{$I cxVer.inc}
{$I dxGanttControl.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes, System.Contnrs,
{$ENDIF}
  Types, SysUtils, Classes, Graphics, ImgList, Windows, Controls, Dialogs,
  Generics.Defaults, Generics.Collections, Forms, StdCtrls, Variants,
  dxCore, dxCoreClasses, cxGraphics, cxCustomCanvas, cxGeometry, cxClasses,
  cxControls,
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
  { TdxGanttControlAssignmentHistoryItem }

  TdxGanttControlAssignmentHistoryItem = class abstract
    (TdxGanttControlHistoryItem)
  strict private
    FAssignment: TdxGanttControlAssignment;
  protected
    property Assignment: TdxGanttControlAssignment read FAssignment;
  public
    constructor Create(AHistory: TdxGanttControlHistory;
      AAssignment: TdxGanttControlAssignment); reintroduce; virtual;
  end;

  { TdxGanttControlAssignmentSetAssignedValueHistoryItem }

  TdxGanttControlAssignmentSetAssignedValueHistoryItem = class
    (TdxGanttControlAssignmentHistoryItem)
  protected
    FAssignedValue: TdxGanttAssignmentAssignedValue;
    procedure DoUndo; override;
  end;

  { TdxGanttControlAssignmentResetAssignedValueHistoryItem }

  TdxGanttControlAssignmentResetAssignedValueHistoryItem = class
    (TdxGanttControlAssignmentHistoryItem)
  protected
    FAssignedValue: TdxGanttAssignmentAssignedValue;
    procedure DoRedo; override;
  end;

  { TdxGanttControlChangeAssignmentPropertyHistoryItem }

  TdxGanttControlChangeAssignmentPropertyHistoryItem = class abstract
    (TdxGanttControlAssignmentHistoryItem)
  protected
    FNewValue: Variant;
    FOldValue: Variant;

    function GetValue: Variant; virtual; abstract;
    procedure DoSetValue(const Value: Variant); virtual; abstract;
    procedure SetValue(const Value: Variant);

    procedure DoRedo; override;
    procedure DoUndo; override;
  end;

  TdxGanttControlChangeAssignmentPropertyHistoryItemClass = class of
    TdxGanttControlChangeAssignmentPropertyHistoryItem;

  { TdxGanttControlResourceHistoryItem }

  TdxGanttControlResourceHistoryItem = class abstract
    (TdxGanttControlHistoryItem)
  strict private
    FResource: TdxGanttControlResource;
  protected
    property Resource: TdxGanttControlResource read FResource;
  public
    constructor Create(AHistory: TdxGanttControlHistory;
      AResource: TdxGanttControlResource); reintroduce; virtual;
  end;

  { TdxGanttControlResourceSetAssignedValueHistoryItem }

  TdxGanttControlResourceSetAssignedValueHistoryItem = class
    (TdxGanttControlResourceHistoryItem)
  protected
    FAssignedValue: TdxGanttResourceAssignedValue;
    procedure DoUndo; override;
  end;

  { TdxGanttControlResourceResetAssignedValueHistoryItem }

  TdxGanttControlResourceResetAssignedValueHistoryItem = class
    (TdxGanttControlResourceHistoryItem)
  protected
    FAssignedValue: TdxGanttResourceAssignedValue;
    procedure DoRedo; override;
  end;

  { TdxGanttControlResourceMakeNotNullHistoryItem }

  TdxGanttControlResourceMakeNotNullHistoryItem = class
    (TdxGanttControlResourceHistoryItem)
  strict private
    FIsNull: Boolean;
  protected
    procedure DoRedo; override;
    procedure DoUndo; override;
  public
    constructor Create(AHistory: TdxGanttControlHistory;
      AResource: TdxGanttControlResource); override;
  end;

  { TdxGanttControlChangeResourcePropertyHistoryItem }

  TdxGanttControlChangeResourcePropertyHistoryItem = class abstract
    (TdxGanttControlResourceHistoryItem)
  protected
    FNewValue: Variant;
    FOldValue: Variant;

    function GetValue: Variant; virtual; abstract;
    procedure DoSetValue(const Value: Variant); virtual; abstract;
    procedure SetValue(const Value: Variant);

    procedure DoRedo; override;
    procedure DoUndo; override;
  end;

  TdxGanttControlChangeResourcePropertyHistoryItemClass = class of
    TdxGanttControlChangeResourcePropertyHistoryItem;

  { TdxGanttControlChangeAssignmentPercentWorkCompleteHistoryItem }

  TdxGanttControlChangeAssignmentPercentWorkCompleteHistoryItem = class
    (TdxGanttControlChangeAssignmentPropertyHistoryItem)
  protected
    function GetValue: Variant; override;
    procedure DoSetValue(const Value: Variant); override;
  end;

  { TdxGanttControlChangeResourceNameHistoryItem }

  TdxGanttControlChangeResourceNameHistoryItem = class
    (TdxGanttControlChangeResourcePropertyHistoryItem)
  protected
    function GetValue: Variant; override;
    procedure DoSetValue(const Value: Variant); override;
  end;

  { TdxGanttControlChangeResourceTypeHistoryItem }

  TdxGanttControlChangeResourceTypeHistoryItem = class
    (TdxGanttControlChangeResourcePropertyHistoryItem)
  protected
    function GetValue: Variant; override;
    procedure DoSetValue(const Value: Variant); override;
  end;

  { TdxGanttControlChangeResourceGroupHistoryItem }

  TdxGanttControlChangeResourceGroupHistoryItem = class
    (TdxGanttControlChangeResourcePropertyHistoryItem)
  protected
    function GetValue: Variant; override;
    procedure DoSetValue(const Value: Variant); override;
  end;

  { TdxGanttControlChangeResourceCalendarUIDHistoryItem }

  TdxGanttControlChangeResourceCalendarUIDHistoryItem = class
    (TdxGanttControlChangeResourcePropertyHistoryItem)
  protected
    function GetValue: Variant; override;
    procedure DoSetValue(const Value: Variant); override;
  end;

  { TdxGanttControlChangeAssignmentCustomCommand }

  TdxGanttControlChangeAssignmentCustomCommand = class abstract
    (TdxGanttControlCommand)
  strict private
    FAssignment: TdxGanttControlAssignment;
  protected
    procedure DoSetAssignedValue(AValue: TdxGanttAssignmentAssignedValue);
    property Assignment: TdxGanttControlAssignment read FAssignment;
  public
    constructor Create(AControl: TdxGanttControlBase;
      AAssignment: TdxGanttControlAssignment); reintroduce;

    class procedure DeleteAssignment(AControl: TdxGanttControlBase;
      Assignments: TdxGanttControlAssignments; AIndex: Integer);
      overload; static;
  end;

  { TdxGanttControlChangeAssignmentPropertyCommand }

  TdxGanttControlChangeAssignmentPropertyCommand = class abstract
    (TdxGanttControlChangeAssignmentCustomCommand)
  strict private
    FNewValue: Variant;
    FOldIsAssigned: Boolean;
  protected
    procedure BeforeExecute; override;
    procedure DoExecute; override;

    function GetAssignedValue: TdxGanttAssignmentAssignedValue;
      virtual; abstract;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeAssignmentPropertyHistoryItemClass;
      virtual; abstract;
    function GetValue: Variant; virtual;
    function GetValidValue: Variant; virtual;
    function HasAssignedValue: Boolean; virtual;
    procedure SetAssignedValue;
    procedure SetValue; virtual;
    procedure ResetAssignedValue;
    function ValidateValue(const AValue: Variant): Variant; virtual;

    function IsNewValueValid: Boolean; virtual;

    property NewValue: Variant read FNewValue;
    property OldIsAssigned: Boolean read FOldIsAssigned;
  public
    constructor Create(AControl: TdxGanttControlBase;
      AAssignment: TdxGanttControlAssignment; const ANewValue: Variant);
      reintroduce; virtual;
    function Enabled: Boolean; override;
  end;

  { TdxGanttControlChangeAssignmentPercentWorkCompleteCommand }

  TdxGanttControlChangeAssignmentPercentWorkCompleteCommand = class
    (TdxGanttControlChangeAssignmentPropertyCommand)
  protected
    function GetAssignedValue: TdxGanttAssignmentAssignedValue; override;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeAssignmentPropertyHistoryItemClass; override;
  end;

  { TdxGanttControlChangeResourceCustomCommand }

  TdxGanttControlChangeResourceCustomCommand = class abstract
    (TdxGanttControlCommand)
  strict private
    FResource: TdxGanttControlResource;
  protected
    procedure DoSetAssignedValue(AValue: TdxGanttResourceAssignedValue);
    property Resource: TdxGanttControlResource read FResource;
  public
    constructor Create(AControl: TdxGanttControlBase;
      AResource: TdxGanttControlResource); reintroduce;

    class procedure DeleteAssignment(AControl: TdxGanttControlBase;
      Assignments: TdxGanttControlAssignments; AIndex: Integer);
      overload; static;
  end;

  { TdxGanttControlSetResourceNotNullCommand }

  TdxGanttControlSetResourceNotNullCommand = class
    (TdxGanttControlChangeResourceCustomCommand)
  protected
    procedure BeforeExecute; override;
    procedure DoExecute; override;
  public
    function Enabled: Boolean; override;
  end;

  { TdxGanttControlChangeResourceCommand }

  TdxGanttControlChangeResourceCommand = class abstract
    (TdxGanttControlChangeResourceCustomCommand)
  protected
    procedure BeforeExecute; override;

    procedure MakeResourceNotNull; overload;
    procedure MakeResourceNotNull(AResource: TdxGanttControlResource); overload;
  end;

  { TdxGanttControlChangeResourcePropertyCommand }

  TdxGanttControlChangeResourcePropertyCommand = class abstract
    (TdxGanttControlChangeResourceCommand)
  strict private
    FNewValue: Variant;
    FOldIsAssigned: Boolean;
  protected
    procedure BeforeExecute; override;
    procedure DoExecute; override;

    function GetAssignedValue: TdxGanttResourceAssignedValue; virtual; abstract;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeResourcePropertyHistoryItemClass; virtual;
      abstract;
    function GetValue: Variant; virtual;
    function GetValidValue: Variant; virtual;
    function HasAssignedValue: Boolean; virtual;
    procedure SetAssignedValue;
    procedure SetValue; virtual;
    procedure ResetAssignedValue;
    function ValidateValue(const AValue: Variant): Variant; virtual;

    function IsNewValueValid: Boolean; virtual;

    property NewValue: Variant read FNewValue;
    property OldIsAssigned: Boolean read FOldIsAssigned;
  public
    constructor Create(AControl: TdxGanttControlBase;
      AResource: TdxGanttControlResource; const ANewValue: Variant);
      reintroduce; virtual;
    function Enabled: Boolean; override;
  end;

  TdxGanttControlChangeResourcePropertyCommandClass = class of
    TdxGanttControlChangeResourcePropertyCommand;

  { TdxGanttControlChangeResourceNameCommand }

  TdxGanttControlChangeResourceNameCommand = class
    (TdxGanttControlChangeResourcePropertyCommand)
  protected
    function GetAssignedValue: TdxGanttResourceAssignedValue; override;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeResourcePropertyHistoryItemClass; override;
  end;

  { TdxGanttControlChangeResourceTypeCommand }

  TdxGanttControlChangeResourceTypeCommand = class
    (TdxGanttControlChangeResourcePropertyCommand)
  protected
    function GetAssignedValue: TdxGanttResourceAssignedValue; override;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeResourcePropertyHistoryItemClass; override;
    function GetValidValue: Variant; override;
  end;

  { TdxGanttControlChangeResourceGroupCommand }

  TdxGanttControlChangeResourceGroupCommand = class
    (TdxGanttControlChangeResourcePropertyCommand)
  protected
    function GetAssignedValue: TdxGanttResourceAssignedValue; override;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeResourcePropertyHistoryItemClass; override;
  end;

  { TdxGanttControlChangeResourceCalendarUIDCommand }

  TdxGanttControlChangeResourceCalendarUIDCommand = class
    (TdxGanttControlChangeResourcePropertyCommand)
  protected
    function GetAssignedValue: TdxGanttResourceAssignedValue; override;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeResourcePropertyHistoryItemClass; override;
    function GetValidValue: Variant; override;
  end;

  { TdxGanttControlDeleteResourceCommand }

  TdxGanttControlDeleteResourceCommand = class
    (TdxGanttControlChangeResourceCustomCommand)
  strict private
    FExecuteNeeded: Boolean;
    FIsLast: Boolean;
    function GetDeletingResourceConfirmation: string;
  protected
    procedure AfterExecute; override;
    procedure BeforeExecute; override;
    procedure DoExecute; override;
  public
    function Enabled: Boolean; override;
  end;

implementation

uses
  RTLConsts,
  dxGanttControlStrs;

{ TdxGanttControlAssignmentHistoryItem }

constructor TdxGanttControlAssignmentHistoryItem.Create
  (AHistory: TdxGanttControlHistory; AAssignment: TdxGanttControlAssignment);
begin
  inherited Create(AHistory);
  FAssignment := AAssignment;
end;

{ TdxGanttControlAssignmentSetAssignedValueHistoryItem }

procedure TdxGanttControlAssignmentSetAssignedValueHistoryItem.DoUndo;
begin
  Assignment.ResetValue(FAssignedValue);
  inherited DoUndo;
end;

{ TdxGanttControlAssignmentResetAssignedValueHistoryItem }

procedure TdxGanttControlAssignmentResetAssignedValueHistoryItem.DoRedo;
begin
  Assignment.ResetValue(FAssignedValue);
  inherited DoRedo;
end;

{ TdxGanttControlChangeAssignmentPropertyHistoryItem }

procedure TdxGanttControlChangeAssignmentPropertyHistoryItem.DoRedo;
begin
  inherited DoRedo;
  FOldValue := GetValue;
  SetValue(FNewValue);
end;

procedure TdxGanttControlChangeAssignmentPropertyHistoryItem.DoUndo;
begin
  SetValue(FOldValue);
  inherited DoUndo;
end;

procedure TdxGanttControlChangeAssignmentPropertyHistoryItem.SetValue
  (const Value: Variant);
begin
  if not VarIsNull(Value) then
    DoSetValue(Value);
end;

{ TdxGanttControlResourceHistoryItem }

constructor TdxGanttControlResourceHistoryItem.Create
  (AHistory: TdxGanttControlHistory; AResource: TdxGanttControlResource);
begin
  inherited Create(AHistory);
  FResource := AResource;
end;

{ TdxGanttControlResourceSetAssignedValueHistoryItem }

procedure TdxGanttControlResourceSetAssignedValueHistoryItem.DoUndo;
begin
  Resource.ResetValue(FAssignedValue);
  inherited DoUndo;
end;

{ TdxGanttControlResourceResetAssignedValueHistoryItem }

procedure TdxGanttControlResourceResetAssignedValueHistoryItem.DoRedo;
begin
  Resource.ResetValue(FAssignedValue);
  inherited DoRedo;
end;

{ TdxGanttControlResourceMakeNotNullHistoryItem }

constructor TdxGanttControlResourceMakeNotNullHistoryItem.Create
  (AHistory: TdxGanttControlHistory; AResource: TdxGanttControlResource);
begin
  inherited Create(AHistory, AResource);
  FIsNull := Resource.Blank;
end;

procedure TdxGanttControlResourceMakeNotNullHistoryItem.DoRedo;
begin
  inherited DoRedo;
  Resource.Blank := False;
end;

procedure TdxGanttControlResourceMakeNotNullHistoryItem.DoUndo;
begin
  Resource.Blank := FIsNull;
  inherited DoUndo;
end;

{ TdxGanttControlChangeResourcePropertyHistoryItem }

procedure TdxGanttControlChangeResourcePropertyHistoryItem.DoRedo;
begin
  inherited DoRedo;
  FOldValue := GetValue;
  SetValue(FNewValue);
end;

procedure TdxGanttControlChangeResourcePropertyHistoryItem.DoUndo;
begin
  SetValue(FOldValue);
  inherited DoUndo;
end;

procedure TdxGanttControlChangeResourcePropertyHistoryItem.SetValue
  (const Value: Variant);
begin
  if not VarIsNull(Value) then
    DoSetValue(Value);
end;

{ TdxGanttControlChangeAssignmentPercentWorkCompleteHistoryItem }

function TdxGanttControlChangeAssignmentPercentWorkCompleteHistoryItem.
  GetValue: Variant;
begin
  Result := Assignment.PercentWorkComplete;
end;

procedure TdxGanttControlChangeAssignmentPercentWorkCompleteHistoryItem.
  DoSetValue(const Value: Variant);
begin
  Assignment.PercentWorkComplete := Value;
end;

{ TdxGanttControlChangeResourceNameHistoryItem }

procedure TdxGanttControlChangeResourceNameHistoryItem.DoSetValue
  (const Value: Variant);
begin
  Resource.Name := Value;
end;

function TdxGanttControlChangeResourceNameHistoryItem.GetValue: Variant;
begin
  Result := Resource.Name;
end;

{ TdxGanttControlChangeResourceTypeHistoryItem }

procedure TdxGanttControlChangeResourceTypeHistoryItem.DoSetValue
  (const Value: Variant);
begin
  Resource.&Type := Value;
end;

function TdxGanttControlChangeResourceTypeHistoryItem.GetValue: Variant;
begin
  Result := Resource.&Type;
end;

{ TdxGanttControlChangeResourceGroupHistoryItem }

procedure TdxGanttControlChangeResourceGroupHistoryItem.DoSetValue
  (const Value: Variant);
begin
  Resource.Group := Value;
end;

function TdxGanttControlChangeResourceGroupHistoryItem.GetValue: Variant;
begin
  Result := Resource.Group;
end;

{ TdxGanttControlChangeResourceCalendarUIDHistoryItem }

procedure TdxGanttControlChangeResourceCalendarUIDHistoryItem.DoSetValue
  (const Value: Variant);
begin
  Resource.CalendarUID := Value;
end;

function TdxGanttControlChangeResourceCalendarUIDHistoryItem.GetValue: Variant;
begin
  Result := Resource.CalendarUID;
end;

{ TdxGanttControlChangeAssignmentCustomCommand }

constructor TdxGanttControlChangeAssignmentCustomCommand.Create
  (AControl: TdxGanttControlBase; AAssignment: TdxGanttControlAssignment);
begin
  inherited Create(AControl);
  FAssignment := AAssignment;
end;

class procedure TdxGanttControlChangeAssignmentCustomCommand.DeleteAssignment
  (AControl: TdxGanttControlBase; Assignments: TdxGanttControlAssignments;
  AIndex: Integer);
var
  AHistoryItem: TdxGanttControlRemoveItemHistoryItem;
begin
  AHistoryItem := TdxGanttControlRemoveItemHistoryItem.Create(AControl.History,
    Assignments);
  AHistoryItem.Index := AIndex;
  AControl.History.AddItem(AHistoryItem);
  AHistoryItem.Execute;
end;

procedure TdxGanttControlChangeAssignmentCustomCommand.DoSetAssignedValue
  (AValue: TdxGanttAssignmentAssignedValue);
var
  AHistoryItem: TdxGanttControlAssignmentSetAssignedValueHistoryItem;
  AHistory: TdxGanttControlHistory;
begin
  AHistory := Control.History;
  AHistoryItem := TdxGanttControlAssignmentSetAssignedValueHistoryItem.Create
    (AHistory, Assignment);
  AHistoryItem.FAssignedValue := AValue;
  AHistory.AddItem(AHistoryItem);
  AHistoryItem.Execute;
end;

{ TdxGanttControlChangeAssignmentPropertyCommand }

constructor TdxGanttControlChangeAssignmentPropertyCommand.Create
  (AControl: TdxGanttControlBase; AAssignment: TdxGanttControlAssignment;
  const ANewValue: Variant);
begin
  inherited Create(AControl, AAssignment);
  FNewValue := ANewValue;
end;

procedure TdxGanttControlChangeAssignmentPropertyCommand.BeforeExecute;
begin
  if HasAssignedValue then
    FOldIsAssigned := Assignment.IsValueAssigned(GetAssignedValue);
  inherited BeforeExecute;
end;

procedure TdxGanttControlChangeAssignmentPropertyCommand.DoExecute;
begin
  if HasAssignedValue and not VarIsNull(FNewValue) and
    not Assignment.IsValueAssigned(GetAssignedValue) then
    SetAssignedValue;
  SetValue;
  if HasAssignedValue and VarIsNull(FNewValue) then
    ResetAssignedValue;
  inherited DoExecute;
end;

function TdxGanttControlChangeAssignmentPropertyCommand.Enabled: Boolean;
begin
  Result := inherited Enabled and ((GetChangeValueHistoryItemClass = nil) or
    not Assignment.IsValueAssigned(GetAssignedValue) or
    (GetValue <> GetValidValue));
end;

function TdxGanttControlChangeAssignmentPropertyCommand.GetValidValue: Variant;
begin
  Result := NewValue;
  if not IsNewValueValid then
    Result := ValidateValue(Result);
end;

function TdxGanttControlChangeAssignmentPropertyCommand.GetValue: Variant;
begin
  if GetChangeValueHistoryItemClass = nil then
    Exit(Null);

  with GetChangeValueHistoryItemClass.Create(Control.History, Assignment) do
    try
      Result := GetValue;
    finally
      Free;
    end;
end;

function TdxGanttControlChangeAssignmentPropertyCommand.
  HasAssignedValue: Boolean;
begin
  Result := True;
end;

function TdxGanttControlChangeAssignmentPropertyCommand.IsNewValueValid
  : Boolean;
begin
  Result := True;
end;

procedure TdxGanttControlChangeAssignmentPropertyCommand.ResetAssignedValue;
var
  AHistoryItem: TdxGanttControlAssignmentResetAssignedValueHistoryItem;
  AHistory: TdxGanttControlHistory;
begin
  AHistory := Control.History;
  AHistoryItem := TdxGanttControlAssignmentResetAssignedValueHistoryItem.Create
    (AHistory, Assignment);
  AHistoryItem.FAssignedValue := GetAssignedValue;
  AHistory.AddItem(AHistoryItem);
  AHistoryItem.Execute;
end;

procedure TdxGanttControlChangeAssignmentPropertyCommand.SetAssignedValue;
begin
  DoSetAssignedValue(GetAssignedValue);
end;

procedure TdxGanttControlChangeAssignmentPropertyCommand.SetValue;
var
  AHistoryItem: TdxGanttControlChangeAssignmentPropertyHistoryItem;
  ANewValue: Variant;
begin
  ANewValue := GetValidValue;
  AHistoryItem := GetChangeValueHistoryItemClass.Create(Control.History,
    Assignment);
  AHistoryItem.FNewValue := ANewValue;
  Control.History.AddItem(AHistoryItem);
  AHistoryItem.Execute;
end;

function TdxGanttControlChangeAssignmentPropertyCommand.ValidateValue
  (const AValue: Variant): Variant;
begin
  Result := AValue;
end;

{ TdxGanttControlChangeAssignmentPercentWorkCompleteCommand }

function TdxGanttControlChangeAssignmentPercentWorkCompleteCommand.
  GetAssignedValue: TdxGanttAssignmentAssignedValue;
begin
  Result := TdxGanttAssignmentAssignedValue.PercentWorkComplete;
end;

function TdxGanttControlChangeAssignmentPercentWorkCompleteCommand.
  GetChangeValueHistoryItemClass
  : TdxGanttControlChangeAssignmentPropertyHistoryItemClass;
begin
  Result := TdxGanttControlChangeAssignmentPercentWorkCompleteHistoryItem;
end;

{ TdxGanttControlChangeResourceCustomCommand }

constructor TdxGanttControlChangeResourceCustomCommand.Create
  (AControl: TdxGanttControlBase; AResource: TdxGanttControlResource);
begin
  inherited Create(AControl);
  FResource := AResource;
end;

class procedure TdxGanttControlChangeResourceCustomCommand.DeleteAssignment
  (AControl: TdxGanttControlBase; Assignments: TdxGanttControlAssignments;
  AIndex: Integer);
var
  AHistoryItem: TdxGanttControlRemoveItemHistoryItem;
begin
  AHistoryItem := TdxGanttControlRemoveItemHistoryItem.Create(AControl.History,
    Assignments);
  AHistoryItem.Index := AIndex;
  AControl.History.AddItem(AHistoryItem);
  AHistoryItem.Execute;
end;

procedure TdxGanttControlChangeResourceCustomCommand.DoSetAssignedValue
  (AValue: TdxGanttResourceAssignedValue);
var
  AHistoryItem: TdxGanttControlResourceSetAssignedValueHistoryItem;
  AHistory: TdxGanttControlHistory;
begin
  AHistory := Control.History;
  AHistoryItem := TdxGanttControlResourceSetAssignedValueHistoryItem.Create
    (AHistory, Resource);
  AHistoryItem.FAssignedValue := AValue;
  AHistory.AddItem(AHistoryItem);
  AHistoryItem.Execute;
end;

{ TdxGanttControlSetResourceNotNullCommand }

procedure TdxGanttControlSetResourceNotNullCommand.BeforeExecute;
begin
  inherited BeforeExecute;
  if not Resource.IsValueAssigned(TdxGanttResourceAssignedValue.Blank) then
    DoSetAssignedValue(TdxGanttResourceAssignedValue.Blank);
end;

procedure TdxGanttControlSetResourceNotNullCommand.DoExecute;
var
  AHistoryItem: TdxGanttControlResourceMakeNotNullHistoryItem;
  AHistory: TdxGanttControlHistory;
begin
  inherited DoExecute;
  AHistory := Control.History;
  AHistoryItem := TdxGanttControlResourceMakeNotNullHistoryItem.Create(AHistory,
    Resource);
  AHistory.AddItem(AHistoryItem);
  AHistoryItem.Execute;
end;

function TdxGanttControlSetResourceNotNullCommand.Enabled: Boolean;
begin
  Result := inherited Enabled and
    (not Resource.IsValueAssigned(TdxGanttResourceAssignedValue.Blank) or
    Resource.Blank);
end;

{ TdxGanttControlChangeResourceCommand }

procedure TdxGanttControlChangeResourceCommand.BeforeExecute;
begin
  inherited BeforeExecute;
  if Resource.Blank then
    MakeResourceNotNull;
end;

procedure TdxGanttControlChangeResourceCommand.MakeResourceNotNull;
begin
  MakeResourceNotNull(Resource);
end;

procedure TdxGanttControlChangeResourceCommand.MakeResourceNotNull
  (AResource: TdxGanttControlResource);
begin
  with TdxGanttControlSetResourceNotNullCommand.Create(Control, Resource) do
    try
      Execute;
    finally
      Free;
    end;
end;

{ TdxGanttControlChangeResourcePropertyCommand }

constructor TdxGanttControlChangeResourcePropertyCommand.Create
  (AControl: TdxGanttControlBase; AResource: TdxGanttControlResource;
  const ANewValue: Variant);
begin
  inherited Create(AControl, AResource);
  FNewValue := ANewValue;
end;

procedure TdxGanttControlChangeResourcePropertyCommand.BeforeExecute;
begin
  if HasAssignedValue then
    FOldIsAssigned := Resource.IsValueAssigned(GetAssignedValue);
  inherited BeforeExecute;
end;

procedure TdxGanttControlChangeResourcePropertyCommand.DoExecute;
begin
  if HasAssignedValue and not VarIsNull(FNewValue) and
    not Resource.IsValueAssigned(GetAssignedValue) then
    SetAssignedValue;
  SetValue;
  if HasAssignedValue and VarIsNull(FNewValue) then
    ResetAssignedValue;
  inherited DoExecute;
end;

function TdxGanttControlChangeResourcePropertyCommand.Enabled: Boolean;
begin
  Result := inherited Enabled and ((GetChangeValueHistoryItemClass = nil) or
    not Resource.IsValueAssigned(GetAssignedValue) or
    (GetValue <> GetValidValue));
end;

function TdxGanttControlChangeResourcePropertyCommand.GetValidValue: Variant;
begin
  Result := NewValue;
  if not IsNewValueValid then
    Result := ValidateValue(Result);
end;

function TdxGanttControlChangeResourcePropertyCommand.GetValue: Variant;
begin
  if GetChangeValueHistoryItemClass = nil then
    Exit(Null);

  with GetChangeValueHistoryItemClass.Create(Control.History, Resource) do
    try
      Result := GetValue;
    finally
      Free;
    end;
end;

function TdxGanttControlChangeResourcePropertyCommand.HasAssignedValue: Boolean;
begin
  Result := True;
end;

function TdxGanttControlChangeResourcePropertyCommand.IsNewValueValid: Boolean;
begin
  Result := True;
end;

procedure TdxGanttControlChangeResourcePropertyCommand.ResetAssignedValue;
var
  AHistoryItem: TdxGanttControlResourceResetAssignedValueHistoryItem;
  AHistory: TdxGanttControlHistory;
begin
  AHistory := Control.History;
  AHistoryItem := TdxGanttControlResourceResetAssignedValueHistoryItem.Create
    (AHistory, Resource);
  AHistoryItem.FAssignedValue := GetAssignedValue;
  AHistory.AddItem(AHistoryItem);
  AHistoryItem.Execute;
end;

procedure TdxGanttControlChangeResourcePropertyCommand.SetAssignedValue;
begin
  DoSetAssignedValue(GetAssignedValue);
end;

procedure TdxGanttControlChangeResourcePropertyCommand.SetValue;
var
  AHistoryItem: TdxGanttControlChangeResourcePropertyHistoryItem;
  ANewValue: Variant;
begin
  ANewValue := GetValidValue;
  AHistoryItem := GetChangeValueHistoryItemClass.Create(Control.History,
    Resource);
  AHistoryItem.FNewValue := ANewValue;
  Control.History.AddItem(AHistoryItem);
  AHistoryItem.Execute;
end;

function TdxGanttControlChangeResourcePropertyCommand.ValidateValue
  (const AValue: Variant): Variant;
begin
  Result := AValue;
end;

{ TdxGanttControlChangeResourceNameCommand }

function TdxGanttControlChangeResourceNameCommand.GetAssignedValue
  : TdxGanttResourceAssignedValue;
begin
  Result := TdxGanttResourceAssignedValue.Name;
end;

function TdxGanttControlChangeResourceNameCommand.GetChangeValueHistoryItemClass
  : TdxGanttControlChangeResourcePropertyHistoryItemClass;
begin
  Result := TdxGanttControlChangeResourceNameHistoryItem;
end;

{ TdxGanttControlChangeResourceTypeCommand }

function TdxGanttControlChangeResourceTypeCommand.GetAssignedValue
  : TdxGanttResourceAssignedValue;
begin
  Result := TdxGanttResourceAssignedValue.&Type;
end;

function TdxGanttControlChangeResourceTypeCommand.GetChangeValueHistoryItemClass
  : TdxGanttControlChangeResourcePropertyHistoryItemClass;
begin
  Result := TdxGanttControlChangeResourceTypeHistoryItem;
end;

function TdxGanttControlChangeResourceTypeCommand.GetValidValue: Variant;
begin
  if VarIsNull(NewValue) then
    Result := Null
  else if VarIsStr(NewValue) then
  begin
    if NewValue = cxGetResourceString(@sdxGanttControlResourceTypeMaterial) then
      Result := TdxGanttControlResourceType.Material
    else if NewValue = cxGetResourceString(@sdxGanttControlResourceTypeCost)
    then
      Result := TdxGanttControlResourceType.Cost
    else
      Result := TdxGanttControlResourceType.Work;
  end
  else
    Result := NewValue;
end;

{ TdxGanttControlChangeResourceGroupCommand }

function TdxGanttControlChangeResourceGroupCommand.GetAssignedValue
  : TdxGanttResourceAssignedValue;
begin
  Result := TdxGanttResourceAssignedValue.Group;
end;

function TdxGanttControlChangeResourceGroupCommand.
  GetChangeValueHistoryItemClass
  : TdxGanttControlChangeResourcePropertyHistoryItemClass;
begin
  Result := TdxGanttControlChangeResourceGroupHistoryItem;
end;

{ TdxGanttControlChangeResourceCalendarUIDCommand }

function TdxGanttControlChangeResourceCalendarUIDCommand.GetAssignedValue
  : TdxGanttResourceAssignedValue;
begin
  Result := TdxGanttResourceAssignedValue.CalendarUID;
end;

function TdxGanttControlChangeResourceCalendarUIDCommand.
  GetChangeValueHistoryItemClass
  : TdxGanttControlChangeResourcePropertyHistoryItemClass;
begin
  Result := TdxGanttControlChangeResourceCalendarUIDHistoryItem;
end;

function TdxGanttControlChangeResourceCalendarUIDCommand.GetValidValue: Variant;
var
  ACalendar: TdxGanttControlCalendar;
begin
  if VarIsStr(NewValue) then
  begin
    ACalendar := DataModel.Calendars.GetCalendarByName(NewValue);
    if ACalendar <> nil then
      Result := ACalendar.UID
    else
      Result := Null;
  end
  else
    Result := NewValue;
end;

{ TdxGanttControlDeleteResourceCommand }

procedure TdxGanttControlDeleteResourceCommand.AfterExecute;
var
  I: Integer;
begin
  if not FExecuteNeeded then
    Exit;
  if Resource.Blank then
    Exit;
  for I := DataModel.Assignments.Count - 1 downto 0 do
  begin
    if DataModel.Assignments[I].ResourceUID = Resource.UID then
      TdxGanttControlChangeResourceCustomCommand.DeleteAssignment(Control,
        DataModel.Assignments, I);
  end;
end;

procedure TdxGanttControlDeleteResourceCommand.BeforeExecute;
begin
  FExecuteNeeded := not Control.OptionsBehavior.ConfirmDelete or Resource.Blank;
  if not FExecuteNeeded then
    FExecuteNeeded := MessageDlg(GetDeletingResourceConfirmation,
      mtConfirmation, mbYesNo, 0) = mrYes;
  if FExecuteNeeded then
    FIsLast := Resource.Owner[Resource.Owner.Count - 1] = Resource;
end;

procedure TdxGanttControlDeleteResourceCommand.DoExecute;
var
  AHistoryItem: TdxGanttControlRemoveItemHistoryItem;
  AHistory: TdxGanttControlHistory;
begin
  if not FExecuteNeeded then
    Exit;
  AHistory := Control.History;
  AHistoryItem := TdxGanttControlRemoveItemHistoryItem.Create(AHistory,
    Resource.Owner);
  AHistoryItem.Index := Resource.ID;
  AHistory.AddItem(AHistoryItem);
  AHistoryItem.Execute;
end;

function TdxGanttControlDeleteResourceCommand.Enabled: Boolean;
begin
  Result := inherited Enabled and (Resource <> nil);
end;

function TdxGanttControlDeleteResourceCommand.
  GetDeletingResourceConfirmation: string;
begin
  Result := cxGetResourceString(@sdxGanttControlConfirmationDeleteResource);
  if Trim(Resource.Name) = '' then
    Result := Format(Result,
      [Format(cxGetResourceString(@sdxGanttControlResourceID), [Resource.ID])])
  else
    Result := Format(Result, [Resource.Name]);
end;

end.
