unit FeaturesDemoMain;

interface

{$I cxVer.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ComCtrls, StdCtrls,
  dxCore, cxContainer, cxEdit, cxTextEdit, cxDropDownEdit, cxMaskEdit, cxSpinEdit,
  cxGraphics, cxClasses, cxControls,
{$IFDEF EXPRESSSKINS}
  dxSkinsdxRibbonPainter,
  dxSkinsdxBarPainter,
{$ENDIF}
  cxLookAndFeels, cxLookAndFeelPainters, dxLayoutControl,
  dxLayoutContainer, dxLayoutLookAndFeels, dxLayoutcxEditAdapters, DemoBasicMain,
  dxGanttControlCustomClasses, dxGanttControl, dxGanttControlCustomSheet, dxGanttControlViewChart,
  dxGanttControlViewResourceSheet, dxGanttControlViewTimeline, dxGanttControlTasks, dxGanttControlAssignments,
  dxGanttControlResources;

type
  TFeaturesDemoMainForm = class(TDemoBasicMainForm)
  public
    constructor Create(AOwner: TComponent); override;
  end;

var
  FeaturesDemoMainForm: TFeaturesDemoMainForm;

implementation

{$R *.dfm}

constructor TFeaturesDemoMainForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  GanttControl.DataModel.LoadFromFile('..\..\Data\SoftDev.xml');
end;

end.
