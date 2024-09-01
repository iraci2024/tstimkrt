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

unit dxGanttControlReg;

{$I cxVer.inc}
{$I dxGanttControl.inc}

interface

uses
  Classes, SysUtils,
  DesignIntf, DesignEditors, DesignMenus, VCLEditors,
  cxDesignWindows, cxEditPropEditors, cxPropEditors;

procedure Register;

implementation

uses
  dxCoreReg,
  dxGanttControl,
  dxGanttControlCustomSheet,
  dxGanttControlViewChart,
  dxGanttControlViewChartSheetColumns,
  dxGanttControlViewResourceSheet,
  dxGanttControlResourceSheetColumns,
  dxGanttControlViewTimeline,
  dxGanttControlSheetColumnsEditor;

const
  sdxGanttControlDescription = 'ExpressGanttControl';

type
  TdxGanttControlSelectionEditor = class(TSelectionEditor)
  public
    procedure RequiresUnits(Proc: TGetStrProc); override;
  end;

  { TdxGanttControlComponentEditor }

  TdxGanttControlComponentEditor = class(TdxComponentEditor)
  strict private
    function GetComponent: TdxGanttControl;
  protected
    function GetProductName: string; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;
    function InternalGetVerbCount: Integer; override;
    function InternalGetVerb(AIndex: Integer): string; override;
  public
    property Component: TdxGanttControl read GetComponent;
  end;

  { TdxGanttControlSheetColumnsProperty }

  TdxGanttControlSheetColumnsProperty = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
  end;

  { TdxGanttControlViewChartEventsProperty }

  TdxGanttControlViewChartEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

  { TdxGanttControlViewResourceSheetEventsProperty }

  TdxGanttControlViewResourceSheetEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

  { TdxGanttControlViewTimelineEventsProperty }

  TdxGanttControlViewTimelineEventsProperty = class(TcxNestedEventProperty)
  protected
    function GetInstance: TPersistent; override;
  end;

  { TdxGanttControlSelectionEditor }

procedure TdxGanttControlSelectionEditor.RequiresUnits(Proc: TGetStrProc);
begin
  Proc('dxCore');
  Proc('cxEdit');
  Proc('dxGanttControl');
  Proc('dxGanttControlCustomSheet');
  Proc('dxGanttControlViewChart');
  Proc('dxGanttControlViewResourceSheet');
  Proc('dxGanttControlViewTimeline');
  Proc('dxGanttControlTasks');
  Proc('dxGanttControlAssignments');
  Proc('dxGanttControlResources');
end;

{ TdxGanttControlSheetColumnsProperty }

procedure TdxGanttControlSheetColumnsProperty.Edit;
begin
  dxShowGanttControlSheetColumnsEditor(Designer,
    TdxGanttControlSheetOptions(GetComponent(0)).Columns);
end;

function TdxGanttControlSheetColumnsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

{ TdxGanttControlComponentEditor }

function TdxGanttControlComponentEditor.GetComponent: TdxGanttControl;
begin
  Result := TdxGanttControl(inherited Component);
end;

function TdxGanttControlComponentEditor.GetProductName: string;
begin
  Result := sdxGanttControlDescription;
end;

procedure TdxGanttControlComponentEditor.InternalExecuteVerb(AIndex: Integer);
begin
  if AIndex = 0 then
  begin
    if Component.ViewChart.Active then
      dxShowGanttControlSheetColumnsEditor(Designer,
        Component.ViewChart.OptionsSheet.Columns)
    else
      dxShowGanttControlSheetColumnsEditor(Designer,
        Component.ViewResourceSheet.OptionsSheet.Columns);
  end
  else
    inherited InternalExecuteVerb(AIndex);
end;

function TdxGanttControlComponentEditor.InternalGetVerb
  (AIndex: Integer): string;
begin
  if AIndex = 0 then
    Result := 'Column Editor...'
  else
    Result := inherited InternalGetVerb(AIndex);
end;

function TdxGanttControlComponentEditor.InternalGetVerbCount: Integer;
begin
  Result := inherited InternalGetVerbCount;
  if Component.ViewChart.Active or Component.ViewResourceSheet.Active then
    Inc(Result);
end;

{ TdxGanttControlViewChartEventsProperty }

function TdxGanttControlViewChartEventsProperty.GetInstance: TPersistent;
begin
  if GetComponent(0) is TdxGanttControl then
    Result := TdxGanttControl(GetComponent(0)).ViewChart
  else
    Result := nil;
end;

{ TdxGanttControlViewResourceSheetEventsProperty }

function TdxGanttControlViewResourceSheetEventsProperty.GetInstance
  : TPersistent;
begin
  if GetComponent(0) is TdxGanttControl then
    Result := TdxGanttControl(GetComponent(0)).ViewResourceSheet
  else
    Result := nil;
end;

{ TdxGanttControlViewTimelineEventsProperty }

function TdxGanttControlViewTimelineEventsProperty.GetInstance: TPersistent;
begin
  if GetComponent(0) is TdxGanttControl then
    Result := TdxGanttControl(GetComponent(0)).ViewTimeline
  else
    Result := nil;
end;

procedure Register;
begin
  RegisterComponents(dxCoreLibraryProductPage, [TdxGanttControl]);
  RegisterComponentEditor(TdxCustomGanttControl,
    TdxGanttControlComponentEditor);
  RegisterPropertyEditor(TypeInfo(TdxGanttControlSheetColumns),
    TdxGanttControlSheetOptions, 'Columns',
    TdxGanttControlSheetColumnsProperty);

  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TdxGanttControl,
    'ViewChartEvents', TdxGanttControlViewChartEventsProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TdxGanttControl,
    'ViewResourceSheetEvents', TdxGanttControlViewResourceSheetEventsProperty);
  RegisterPropertyEditor(TypeInfo(TNotifyEvent), TdxGanttControl,
    'ViewTimelineEvents', TdxGanttControlViewTimelineEventsProperty);

  RegisterSelectionEditor(TdxGanttControl, TdxGanttControlSelectionEditor);
end;

end.
