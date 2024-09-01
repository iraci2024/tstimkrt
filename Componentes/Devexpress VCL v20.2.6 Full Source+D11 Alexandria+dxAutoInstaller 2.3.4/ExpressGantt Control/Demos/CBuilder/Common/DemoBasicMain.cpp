//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "DemoBasicMain.h"
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
#include "DemoUtils.h"
TDemoBasicMainForm *DemoBasicMainForm;
//---------------------------------------------------------------------------
__fastcall TDemoBasicMainForm::TDemoBasicMainForm(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TDemoBasicMainForm::FileExitExecute(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------
void __fastcall TDemoBasicMainForm::New1Click(TObject *Sender)
{
  if (EnableDocumentOpen())
  {
	GanttControl->DataModel->Reset();
	FModified = true;
  }
}
//---------------------------------------------------------------------------
void __fastcall TDemoBasicMainForm::Export1Click(TObject *Sender)
{
  if (SaveDialog->Execute())
  {
	GanttControl->DataModel->SaveToFile(SaveDialog->FileName);
	FModified = false;
  }
}
//---------------------------------------------------------------------------
void __fastcall TDemoBasicMainForm::Import1Click(TObject *Sender)
{
  if (EnableDocumentOpen() && OpenDialog->Execute())
	GanttControl->DataModel->LoadFromFile(OpenDialog->FileName);
}
//---------------------------------------------------------------------------
void __fastcall TDemoBasicMainForm::miAboutClick(TObject *Sender)
{
  ShowAboutDemoForm();
}
//---------------------------------------------------------------------------
void __fastcall TDemoBasicMainForm::FormCreate(TObject *Sender)
{
  SetDefaultLookAndFeel();
  AddLookAndFeelMenu();
  WindowState = wsMaximized;
}
//---------------------------------------------------------------------------
void TDemoBasicMainForm::AddLookAndFeelMenu()
{
  mmMain->Items->Insert(mmMain->Items->IndexOf(miAbout),
    CreateLookAndFeelMenuItems(mmMain->Items, lfController));
}
//---------------------------------------------------------------------------
TcxLookAndFeelKind TDemoBasicMainForm::GetDefaultLookAndFeelKind()
{
  return lfUltraFlat;
}
//---------------------------------------------------------------------------
bool TDemoBasicMainForm::EnableDocumentOpen()
{
  if (!FModified)
    return true;

  int mr = MessageDlg("Do you want to save your changes?", mtConfirmation, mbYesNoCancel, 0);
  if (mr == mrCancel)
    return false;
  bool Result = (mr == mrNo);
  if (!Result) 
  {
    Result = SaveDialog->Execute();
    if (Result)
      GanttControl->DataModel->SaveToFile(SaveDialog->FileName);
  }
  return Result;
}
//---------------------------------------------------------------------------
bool TDemoBasicMainForm::IsNativeDefaultStyle()
{
  return false;
}
//---------------------------------------------------------------------------
void TDemoBasicMainForm::SetDefaultLookAndFeel()
{
//  lfController->NativeStyle = IsNativeDefaultStyle;
//  lfController->Kind = GetDefaultLookAndFeelKind;
}
//---------------------------------------------------------------------------
void __fastcall TDemoBasicMainForm::lchbDirectXClick(TObject *Sender)
{
  if (lchbDirectX->Checked)
	lfController->RenderMode = rmDirectX;
  else
	lfController->RenderMode = rmGDI;
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::cbSrcrollBarsPropertiesEditValueChanged(TObject *Sender)
{
  GanttControl->LookAndFeel->ScrollbarMode = TdxScrollbarMode(cbSrcrollBars->ItemIndex);
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::lgActiveViewTabChanged(TObject *Sender)
{
  if (lgActiveView->ItemIndex == 0)
	GanttControl->ViewChart->Active = true;
  else if (lgActiveView->ItemIndex == 1)
	GanttControl->ViewResourceSheet->Active = true;
  else
    GanttControl->ViewTimeline->Active = true;
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::lchbChartCellAutoHeightClick(TObject *Sender)
{
  GanttControl->ViewChart->OptionsSheet->CellAutoHeight = lchbChartCellAutoHeight->Checked;
  GanttControl->ViewChart->OptionsSheet->AllowColumnHide = lchbChartColumnHide->Checked;
  GanttControl->ViewChart->OptionsSheet->AllowColumnMove = lchbChartColumnMove->Checked;
  GanttControl->ViewChart->OptionsSheet->AllowColumnSize = lchbChartColumnSize->Checked;
  GanttControl->ViewChart->OptionsSheet->ColumnQuickCustomization = lchbChartColumnQuickCustomization->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::lchbChartVisibleClick(TObject *Sender)
{
  GanttControl->ViewChart->OptionsSheet->Visible = lchbChartVisible->Checked;
  lchbChartCellAutoHeight->Enabled = lchbChartVisible->Checked;
  lchbChartColumnHide->Enabled = lchbChartVisible->Checked;
  lchbChartColumnMove->Enabled = lchbChartVisible->Checked;
  lchbChartColumnSize->Enabled = lchbChartVisible->Checked;
  lchbChartColumnQuickCustomization->Enabled = lchbChartVisible->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::cmbChartTimescalePropertiesChange(TObject *Sender)
{
  GanttControl->ViewChart->TimescaleUnit = TdxGanttControlChartViewTimescaleUnit(cmbChartTimescale->ItemIndex);
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::lchbResourceSheetCellAutoHeightClick(TObject *Sender)
{
  GanttControl->ViewResourceSheet->OptionsSheet->CellAutoHeight = lchbResourceSheetCellAutoHeight->Checked;
  GanttControl->ViewResourceSheet->OptionsSheet->AllowColumnHide = lchbResourceSheetColumnHide->Checked;
  GanttControl->ViewResourceSheet->OptionsSheet->AllowColumnMove = lchbResourceSheetColumnMove->Checked;
  GanttControl->ViewResourceSheet->OptionsSheet->AllowColumnSize = lchbResourceSheetColumnSize->Checked;
  GanttControl->ViewResourceSheet->OptionsSheet->ColumnQuickCustomization = lchbResourceSheetColumnQuickCustomization->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::lchbShowOnlyExplicitlyAddedTasksClick(TObject *Sender)
{
  GanttControl->ViewTimeline->ShowOnlyExplicitlyAddedTasks = lchbShowOnlyExplicitlyAddedTasks->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::cmbTimelineScalePropertiesChange(TObject *Sender)
{
  GanttControl->ViewTimeline->TimescaleUnit = TdxGanttControlTimelineViewTimescaleUnit(cmbTimelineScale->ItemIndex);
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::seTimelineUnitMinWidthPropertiesChange(TObject *Sender)
{
  GanttControl->ViewTimeline->TimescaleUnitMinWidth = seTimelineUnitMinWidth->Value;
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::lchbDeleteConfirmClick(TObject *Sender)
{
  GanttControl->OptionsBehavior->ConfirmDelete = lchbDeleteConfirm->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::GanttControlActiveViewChanged(TObject *Sender)
{
  if (GanttControl->ViewChart->Active)
	lgActiveView->ItemIndex = 0;
  else if (GanttControl->ViewResourceSheet->Active)
	lgActiveView->ItemIndex = 1;
  else
	lgActiveView->ItemIndex = 2;
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::GanttControlDataModelLoaded(TObject *Sender)
{
  FModified = false;
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::GanttControlAssignmentChanged(TObject *Sender, TdxGanttControlAssignment *AAssignment)
{
  FModified = true;
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::GanttControlResourceChanged(TObject *Sender, TdxGanttControlResource *AResource)
{
  FModified = true;
}
//---------------------------------------------------------------------------

void __fastcall TDemoBasicMainForm::GanttControlTaskChanged(TObject *Sender, TdxGanttControlTask *ATask)
{
  FModified = true;
}
//---------------------------------------------------------------------------
