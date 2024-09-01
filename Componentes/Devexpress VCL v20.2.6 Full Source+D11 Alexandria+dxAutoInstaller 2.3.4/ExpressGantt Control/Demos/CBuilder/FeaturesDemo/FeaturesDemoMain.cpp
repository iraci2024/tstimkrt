//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "FeaturesDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxDropDownEdit"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxMaskEdit"
#pragma link "cxSpinEdit"
#pragma link "cxTextEdit"
#pragma link "dxCore"
#pragma link "dxGanttControl"
#pragma link "dxGanttControlAssignments"
#pragma link "dxGanttControlCustomClasses"
#pragma link "dxGanttControlCustomSheet"
#pragma link "dxGanttControlResources"
#pragma link "dxGanttControlTasks"
#pragma link "dxGanttControlViewChart"
#pragma link "dxGanttControlViewResourceSheet"
#pragma link "dxGanttControlViewTimeline"
#pragma link "dxLayoutContainer"
#pragma link "dxLayoutControl"
#pragma link "dxLayoutcxEditAdapters"
#pragma link "dxLayoutLookAndFeels"
#pragma resource "*.dfm"
TFeaturesDemoMainForm *FeaturesDemoMainForm;
//---------------------------------------------------------------------------
__fastcall TFeaturesDemoMainForm::TFeaturesDemoMainForm(TComponent* Owner)
	: TDemoBasicMainForm(Owner)
{
  GanttControl->DataModel->LoadFromFile("..\\..\\Data\\SoftDev.xml");
}
//---------------------------------------------------------------------------


