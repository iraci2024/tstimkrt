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

unit dxGanttControlCommands;

{$I cxVer.inc}
{$I dxGanttControl.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes, System.Contnrs,
{$ENDIF}
  Types, SysUtils, Classes, Graphics, ImgList, Windows, Controls,
  Generics.Defaults, Generics.Collections, Forms, StdCtrls, Variants,
  dxCoreClasses, cxGraphics, cxCustomCanvas, cxGeometry, cxClasses, cxControls,
  cxVariants, cxEdit, cxDrawTextUtils, dxGDIPlusClasses,
  dxGanttControlCustomClasses,
  dxGanttControlCustomDataModel,
  dxGanttControlDataModel,
  dxGanttControlTasks,
  dxGanttControlAssignments,
  dxGanttControlResources,
  dxGanttControlCalendars;

type
  { TdxGanttControlListItemHistoryItem }

  TdxGanttControlListItemHistoryItem = class abstract
    (TdxGanttControlHistoryItem)
  strict private
    FIndex: Integer;
    FList: TdxGanttControlModelElementList;
  protected
    FItem: TdxGanttControlModelElementListItem;

    function GetList: TdxGanttControlModelElementList; virtual;
    property List: TdxGanttControlModelElementList read GetList;
  public
    constructor Create(AHistory: TdxGanttControlHistory;
      AList: TdxGanttControlModelElementList); reintroduce; virtual;
    destructor Destroy; override;

    property Index: Integer read FIndex write FIndex;
  end;

  { TdxGanttControlCreateItemHistoryItem }

  TdxGanttControlCreateItemHistoryItem = class
    (TdxGanttControlListItemHistoryItem)
  protected
    procedure DoRedo; override;
    procedure DoUndo; override;
  end;

  { TdxGanttControlRemoveItemHistoryItem }

  TdxGanttControlRemoveItemHistoryItem = class
    (TdxGanttControlListItemHistoryItem)
  protected
    procedure DoRedo; override;
    procedure DoUndo; override;
  end;

  { TdxGanttControlListChangeIndexItem }

  TdxGanttControlListChangeIndexItem = class(TdxGanttControlListItemHistoryItem)
  strict private
    FNewIndex: Integer;
  protected
    procedure DoRedo; override;
    procedure DoUndo; override;
  public
    property NewIndex: Integer read FNewIndex write FNewIndex;
  end;

  { TdxGanttControlCreatePredecessorHistoryItem }

  TdxGanttControlCreatePredecessorHistoryItem = class
    (TdxGanttControlCreateItemHistoryItem)
  strict private
    FTask: TdxGanttControlTask;
  protected
    function GetList: TdxGanttControlModelElementList; override;
  public
    constructor Create(AHistory: TdxGanttControlHistory;
      ATask: TdxGanttControlTask); reintroduce;
  end;

  { TdxGanttControlRemovePredecessorHistoryItem }

  TdxGanttControlRemovePredecessorHistoryItem = class
    (TdxGanttControlRemoveItemHistoryItem)
  strict private
    FTask: TdxGanttControlTask;
  protected
    function GetList: TdxGanttControlModelElementList; override;
  public
    constructor Create(AHistory: TdxGanttControlHistory;
      ATask: TdxGanttControlTask); reintroduce;
  end;

  { TdxGanttControlDataModelPropertiesHistoryItem }

  TdxGanttControlDataModelPropertiesHistoryItem = class abstract
    (TdxGanttControlHistoryItem)
  strict private
    FProperties: TdxGanttControlDataModelProperties;
  protected
    property Properties: TdxGanttControlDataModelProperties read FProperties;
  public
    constructor Create(AHistory: TdxGanttControlHistory;
      AProperties: TdxGanttControlDataModelProperties); reintroduce; virtual;
  end;

  { TdxGanttControlDataModelPropertiesSetAssignedValueHistoryItem }

  TdxGanttControlDataModelPropertiesSetAssignedValueHistoryItem = class
    (TdxGanttControlDataModelPropertiesHistoryItem)
  protected
    FAssignedValue: TdxGanttPropertiesAssignedValue;
    procedure DoUndo; override;
  end;

  { TdxGanttControlDataModelPropertiesResetAssignedValueHistoryItem }

  TdxGanttControlDataModelPropertiesResetAssignedValueHistoryItem = class
    (TdxGanttControlDataModelPropertiesHistoryItem)
  protected
    FAssignedValue: TdxGanttPropertiesAssignedValue;
    procedure DoRedo; override;
  end;

  { TdxGanttControlChangeDataModelPropertyHistoryItem }

  TdxGanttControlChangeDataModelPropertyHistoryItem = class abstract
    (TdxGanttControlDataModelPropertiesHistoryItem)
  protected
    FNewValue: Variant;
    FOldValue: Variant;

    function GetValue: Variant; virtual; abstract;
    procedure SetValue(const Value: Variant); virtual; abstract;

    procedure DoRedo; override;
    procedure DoUndo; override;
  end;

  TdxGanttControlChangeDataModelPropertyHistoryItemClass = class of
    TdxGanttControlChangeDataModelPropertyHistoryItem;

  { TdxGanttControlChangeDataModelPropertyStartDateHistoryItem }

  TdxGanttControlChangeDataModelPropertyStartDateHistoryItem = class
    (TdxGanttControlChangeDataModelPropertyHistoryItem)
  protected
    function GetValue: Variant; override;
    procedure SetValue(const Value: Variant); override;
  end;

  { TdxGanttControlChangeDataModelPropertyFinishDateHistoryItem }

  TdxGanttControlChangeDataModelPropertyFinishDateHistoryItem = class
    (TdxGanttControlChangeDataModelPropertyHistoryItem)
  protected
    function GetValue: Variant; override;
    procedure SetValue(const Value: Variant); override;
  end;

  { TdxGanttControlTaskPredecessorLinkHistoryItem }

  TdxGanttControlTaskPredecessorLinkHistoryItem = class abstract
    (TdxGanttControlHistoryItem)
  strict private
    FLink: TdxGanttControlTaskPredecessorLink;
  protected
    property Link: TdxGanttControlTaskPredecessorLink read FLink;
  public
    constructor Create(AHistory: TdxGanttControlHistory;
      ALink: TdxGanttControlTaskPredecessorLink); reintroduce; virtual;
  end;

  { TdxGanttControlCommand }

  TdxGanttControlCommand = class
  strict private
    FControl: TdxGanttControlBase;
    function GetDataModel: TdxGanttControlDataModel; inline;
  protected
    procedure AfterExecute; virtual;
    procedure BeforeExecute; virtual;
    procedure DoExecute; virtual;

    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;

    property Control: TdxGanttControlBase read FControl;
    property DataModel: TdxGanttControlDataModel read GetDataModel;
  public
    constructor Create(AControl: TdxGanttControlBase);

    procedure Execute;

    function Enabled: Boolean; virtual;
    function Visible: Boolean; virtual;
  end;

  TdxGanttControlCommandClass = class of TdxGanttControlCommand;

  { TdxGanttControlUndoCommand }

  TdxGanttControlUndoCommand = class(TdxGanttControlCommand)
  protected
    procedure DoExecute; override;
  public
    function Enabled: Boolean; override;
  end;

  { TdxGanttControlRedoCommand }

  TdxGanttControlRedoCommand = class(TdxGanttControlCommand)
  protected
    procedure DoExecute; override;
  public
    function Enabled: Boolean; override;
  end;

  { TdxGanttControlChangeDataModelPropertyCommand }

  TdxGanttControlChangeDataModelPropertyCommand = class abstract
    (TdxGanttControlCommand)
  strict private
    FNewValue: Variant;
    function GetProperties: TdxGanttControlDataModelProperties; inline;
  protected
    procedure DoExecute; override;

    function GetAssignedValue: TdxGanttPropertiesAssignedValue;
      virtual; abstract;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeDataModelPropertyHistoryItemClass;
      virtual; abstract;
    function GetValue: Variant; virtual;
    procedure SetAssignedValue;
    procedure SetValue;
    procedure ResetAssignedValue;

    property NewValue: Variant read FNewValue;
    property Properties: TdxGanttControlDataModelProperties read GetProperties;
  public
    constructor Create(AControl: TdxGanttControlBase; const ANewValue: Variant);
      reintroduce;
    function Enabled: Boolean; override;
  end;

  { TdxGanttControlChangeDataModelPropertyStartDateCommand }

  TdxGanttControlChangeDataModelPropertyStartDateCommand = class
    (TdxGanttControlChangeDataModelPropertyCommand)
  protected
    function GetAssignedValue: TdxGanttPropertiesAssignedValue; override;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeDataModelPropertyHistoryItemClass; override;
  public
    function Enabled: Boolean; override;
  end;

  { TdxGanttControlChangeDataModelPropertyFinishDateCommand }

  TdxGanttControlChangeDataModelPropertyFinishDateCommand = class
    (TdxGanttControlChangeDataModelPropertyCommand)
  protected
    function GetAssignedValue: TdxGanttPropertiesAssignedValue; override;
    function GetChangeValueHistoryItemClass
      : TdxGanttControlChangeDataModelPropertyHistoryItemClass; override;
  public
    function Enabled: Boolean; override;
  end;

implementation

uses
  Math, DateUtils, RTLConsts,
  dxCore, cxDateUtils,
  dxGanttControl,
  dxGanttControlUtils,
  dxGanttControlStrs;

type
  TdxGanttControlElementCustomListAccess = class
    (TdxGanttControlModelElementList);

  { TdxGanttControlListItemHistoryItem }

constructor TdxGanttControlListItemHistoryItem.Create
  (AHistory: TdxGanttControlHistory; AList: TdxGanttControlModelElementList);
begin
  inherited Create(AHistory);
  FList := AList;
end;

destructor TdxGanttControlListItemHistoryItem.Destroy;
begin
  FreeAndNil(FItem);
  inherited Destroy;
end;

function TdxGanttControlListItemHistoryItem.GetList
  : TdxGanttControlModelElementList;
begin
  Result := FList;
end;

{ TdxGanttControlCreateItemHistoryItem }

procedure TdxGanttControlCreateItemHistoryItem.DoRedo;
var
  AList: TdxGanttControlElementCustomListAccess;
begin
  inherited DoRedo;
  AList := TdxGanttControlElementCustomListAccess(List);
  if FItem = nil then
    FItem := AList.CreateItem;
  AList.InternalInsert(Index, FItem);
  FItem := nil;
end;

procedure TdxGanttControlCreateItemHistoryItem.DoUndo;
var
  AList: TdxGanttControlElementCustomListAccess;
begin
  AList := TdxGanttControlElementCustomListAccess(List);
  FItem := AList[Index];
  AList.InternalExtract(FItem);
  inherited DoUndo;
end;

{ TdxGanttControlRemoveItemHistoryItem }

procedure TdxGanttControlRemoveItemHistoryItem.DoRedo;
var
  AList: TdxGanttControlElementCustomListAccess;
begin
  inherited DoRedo;
  AList := TdxGanttControlElementCustomListAccess(List);
  FItem := AList[Index];
  AList.InternalExtract(FItem);
end;

procedure TdxGanttControlRemoveItemHistoryItem.DoUndo;
var
  AList: TdxGanttControlElementCustomListAccess;
begin
  inherited DoRedo;
  AList := TdxGanttControlElementCustomListAccess(List);
  AList.InternalInsert(Index, FItem);
  FItem := nil;
end;

{ TdxGanttControlListChangeIndexItem }

procedure TdxGanttControlListChangeIndexItem.DoRedo;
begin
  inherited DoRedo;
  TdxGanttControlElementCustomListAccess(List).InternalMove(List[Index],
    NewIndex);
end;

procedure TdxGanttControlListChangeIndexItem.DoUndo;
begin
  inherited DoUndo;
  TdxGanttControlElementCustomListAccess(List)
    .InternalMove(List[FNewIndex], Index);
end;

{ TdxGanttControlCreatePredecessorHistoryItem }

constructor TdxGanttControlCreatePredecessorHistoryItem.Create
  (AHistory: TdxGanttControlHistory; ATask: TdxGanttControlTask);
begin
  inherited Create(AHistory, nil);
  FTask := ATask;
end;

function TdxGanttControlCreatePredecessorHistoryItem.GetList
  : TdxGanttControlModelElementList;
begin
  Result := FTask.PredecessorLinks;
end;

{ TdxGanttControlRemovePredecessorHistoryItem }

constructor TdxGanttControlRemovePredecessorHistoryItem.Create
  (AHistory: TdxGanttControlHistory; ATask: TdxGanttControlTask);
begin
  inherited Create(AHistory, nil);
  FTask := ATask;
end;

function TdxGanttControlRemovePredecessorHistoryItem.GetList
  : TdxGanttControlModelElementList;
begin
  Result := FTask.PredecessorLinks;
end;

{ TdxGanttControlDataModelPropertiesSetAssignedValueHistoryItem }

procedure TdxGanttControlDataModelPropertiesSetAssignedValueHistoryItem.DoUndo;
begin
  Properties.ResetValue(FAssignedValue);
  inherited DoUndo;
end;

{ TdxGanttControlDataModelPropertiesResetAssignedValueHistoryItem }

procedure TdxGanttControlDataModelPropertiesResetAssignedValueHistoryItem.
  DoRedo;
begin
  inherited;

end;

{ TdxGanttControlChangeDataModelPropertyHistoryItem }

procedure TdxGanttControlChangeDataModelPropertyHistoryItem.DoRedo;
begin
  inherited DoRedo;
  FOldValue := GetValue;
  SetValue(FNewValue);
end;

procedure TdxGanttControlChangeDataModelPropertyHistoryItem.DoUndo;
begin
  SetValue(FOldValue);
  inherited DoUndo;
end;

{ TdxGanttControlChangeDataModelPropertyStartDateHistoryItem }

function TdxGanttControlChangeDataModelPropertyStartDateHistoryItem.
  GetValue: Variant;
begin
  Result := Properties.ProjectStart;
end;

procedure TdxGanttControlChangeDataModelPropertyStartDateHistoryItem.SetValue
  (const Value: Variant);
begin
  Properties.ProjectStart := Value;
end;

{ TdxGanttControlChangeDataModelPropertyFinishDateHistoryItem }

function TdxGanttControlChangeDataModelPropertyFinishDateHistoryItem.
  GetValue: Variant;
begin
  Result := Properties.ProjectFinish;
end;

procedure TdxGanttControlChangeDataModelPropertyFinishDateHistoryItem.SetValue
  (const Value: Variant);
begin
  Properties.ProjectFinish := Value;
end;

{ TdxGanttControlDataModelPropertiesHistoryItem }

constructor TdxGanttControlDataModelPropertiesHistoryItem.Create
  (AHistory: TdxGanttControlHistory;
  AProperties: TdxGanttControlDataModelProperties);
begin
  inherited Create(AHistory);
  FProperties := AProperties;
end;

{ TdxGanttControlTaskPredecessorLinkHistoryItem }

constructor TdxGanttControlTaskPredecessorLinkHistoryItem.Create
  (AHistory: TdxGanttControlHistory; ALink: TdxGanttControlTaskPredecessorLink);
begin
  inherited Create(AHistory);
  FLink := ALink;
end;

{ TdxGanttControlCommand }

constructor TdxGanttControlCommand.Create(AControl: TdxGanttControlBase);
begin
  inherited Create;
  FControl := AControl;
end;

function TdxGanttControlCommand.Enabled: Boolean;
begin
  Result := Control.IsEditable;
end;

procedure TdxGanttControlCommand.EndUpdate;
begin
  Control.EndUpdate;
end;

function TdxGanttControlCommand.Visible: Boolean;
begin
  Result := True;
end;

procedure TdxGanttControlCommand.Execute;
begin
  if not Enabled or not Visible then
    Exit;
  BeginUpdate;
  try
    try
      BeforeExecute;
      DoExecute;
      AfterExecute;
    except
      Control.History.CancelTransaction;
      raise;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TdxGanttControlCommand.AfterExecute;
begin
  // do nothing
end;

procedure TdxGanttControlCommand.BeforeExecute;
begin
  // do nothing
end;

procedure TdxGanttControlCommand.BeginUpdate;
begin
  Control.BeginUpdate;
end;

procedure TdxGanttControlCommand.DoExecute;
begin
  // do nothing
end;

function TdxGanttControlCommand.GetDataModel: TdxGanttControlDataModel;
begin
  Result := TdxCustomGanttControl(Control).DataModel;
end;

{ TdxGanttControlUndoCommand }

procedure TdxGanttControlUndoCommand.DoExecute;
begin
  inherited DoExecute;
  Control.History.Undo;
end;

function TdxGanttControlUndoCommand.Enabled: Boolean;
begin
  Result := Control.IsEditable and Control.History.CanUndo;
end;

{ TdxGanttControlRedoCommand }

procedure TdxGanttControlRedoCommand.DoExecute;
begin
  inherited DoExecute;
  Control.History.Redo;
end;

function TdxGanttControlRedoCommand.Enabled: Boolean;
begin
  Result := Control.IsEditable and Control.History.CanRedo;
end;

{ TdxGanttControlChangeDataModelPropertyCommand }

constructor TdxGanttControlChangeDataModelPropertyCommand.Create
  (AControl: TdxGanttControlBase; const ANewValue: Variant);
begin
  inherited Create(AControl);
  FNewValue := ANewValue;
end;

procedure TdxGanttControlChangeDataModelPropertyCommand.DoExecute;
begin
  if not VarIsNull(FNewValue) and not Properties.IsValueAssigned
    (GetAssignedValue) then
    SetAssignedValue;
  if not VarIsNull(FNewValue) then
    SetValue;
  if VarIsNull(FNewValue) then
    ResetAssignedValue;
end;

function TdxGanttControlChangeDataModelPropertyCommand.Enabled: Boolean;
begin
  Result := inherited Enabled and
    (not Properties.IsValueAssigned(GetAssignedValue) or
    (GetValue <> NewValue));
end;

function TdxGanttControlChangeDataModelPropertyCommand.GetProperties
  : TdxGanttControlDataModelProperties;
begin
  Result := TdxCustomGanttControl(Control).DataModel.Properties;
end;

function TdxGanttControlChangeDataModelPropertyCommand.GetValue: Variant;
begin
  with GetChangeValueHistoryItemClass.Create(Control.History, Properties) do
    try
      Result := GetValue;
    finally
      Free;
    end;
end;

procedure TdxGanttControlChangeDataModelPropertyCommand.ResetAssignedValue;
var
  AHistoryItem: TdxGanttControlDataModelPropertiesResetAssignedValueHistoryItem;
  AHistory: TdxGanttControlHistory;
begin
  AHistory := Control.History;
  AHistoryItem :=
    TdxGanttControlDataModelPropertiesResetAssignedValueHistoryItem.Create
    (AHistory, Properties);
  AHistoryItem.FAssignedValue := GetAssignedValue;
  AHistory.AddItem(AHistoryItem);
  AHistoryItem.Execute;
end;

procedure TdxGanttControlChangeDataModelPropertyCommand.SetAssignedValue;
var
  AHistoryItem: TdxGanttControlDataModelPropertiesSetAssignedValueHistoryItem;
  AHistory: TdxGanttControlHistory;
begin
  AHistory := Control.History;
  AHistoryItem := TdxGanttControlDataModelPropertiesSetAssignedValueHistoryItem.
    Create(AHistory, Properties);
  AHistoryItem.FAssignedValue := GetAssignedValue;
  AHistory.AddItem(AHistoryItem);
  AHistoryItem.Execute;
end;

procedure TdxGanttControlChangeDataModelPropertyCommand.SetValue;
var
  AHistoryItem: TdxGanttControlChangeDataModelPropertyHistoryItem;
begin
  AHistoryItem := GetChangeValueHistoryItemClass.Create(Control.History,
    Properties);
  AHistoryItem.FNewValue := NewValue;
  Control.History.AddItem(AHistoryItem);
  AHistoryItem.Execute;
end;

{ TdxGanttControlChangeDataModelPropertyStartDateCommand }

function TdxGanttControlChangeDataModelPropertyStartDateCommand.Enabled
  : Boolean;
begin
  Result := inherited Enabled and
    (not Properties.IsValueAssigned(GetAssignedValue) or (GetValue > NewValue));
end;

function TdxGanttControlChangeDataModelPropertyStartDateCommand.GetAssignedValue
  : TdxGanttPropertiesAssignedValue;
begin
  Result := TdxGanttPropertiesAssignedValue.ProjectStart;
end;

function TdxGanttControlChangeDataModelPropertyStartDateCommand.
  GetChangeValueHistoryItemClass
  : TdxGanttControlChangeDataModelPropertyHistoryItemClass;
begin
  Result := TdxGanttControlChangeDataModelPropertyStartDateHistoryItem;
end;

{ TdxGanttControlChangeDataModelPropertyFinishDateCommand }

function TdxGanttControlChangeDataModelPropertyFinishDateCommand.
  Enabled: Boolean;
begin
  Result := inherited Enabled and
    (not Properties.IsValueAssigned(GetAssignedValue) or (GetValue < NewValue));
end;

function TdxGanttControlChangeDataModelPropertyFinishDateCommand.
  GetAssignedValue: TdxGanttPropertiesAssignedValue;
begin
  Result := TdxGanttPropertiesAssignedValue.ProjectFinish;
end;

function TdxGanttControlChangeDataModelPropertyFinishDateCommand.
  GetChangeValueHistoryItemClass
  : TdxGanttControlChangeDataModelPropertyHistoryItemClass;
begin
  Result := TdxGanttControlChangeDataModelPropertyFinishDateHistoryItem;
end;

end.
