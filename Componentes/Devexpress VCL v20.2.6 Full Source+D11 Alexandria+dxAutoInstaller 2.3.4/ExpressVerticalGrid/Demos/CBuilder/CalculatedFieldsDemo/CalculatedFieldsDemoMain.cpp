//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "CalculatedFieldsDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDBData"
#pragma link "cxEdit"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxStyles"
#pragma link "cxLookAndFeels"
#pragma link "DemoBasicMain"
#pragma link "cxDBVGrid"
#pragma link "cxEditRepositoryItems"
#pragma link "cxInplaceContainer"
#pragma link "cxVGrid"
#pragma link "cxButtons"
#pragma link "cxCalendar"
#pragma link "cxContainer"
#pragma link "cxCurrencyEdit"
#pragma link "cxGroupBox"
#pragma link "cxImageList"
#pragma link "cxLabel"
#pragma link "cxMaskEdit"
#pragma link "cxLookAndFeelPainters"
#pragma link "dxGallery"
#pragma link "dxGalleryControl"
#pragma link "dxmdaset"
#pragma link "dxSpreadSheetTypes"
#pragma link "dxSpreadSheetUtils"
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
  String APath = ExtractFilePath(Application->ExeName) + "..\\..\\Data\\";
  cdsProducts2->LoadFromFile(APath + "Products2.cds");
  mdOrder2->LoadFromBinaryFile(APath + "Order2.dat");

  VerticalGrid->DataController->Groups->FullExpand();

  ((TdxGalleryControlOptionsBehaviorAccess*)galColumns->OptionsBehavior)->ItemHotTrack = false;
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::btnShowExpressionEditorClick(TObject *Sender)
{
  if (galColumns->Gallery->Groups->Groups[0]->Items->Items[0]->Checked)
	vgrTotal->Properties->ShowExpressionEditor();
  else
	vgrDiscountAmount->Properties->ShowExpressionEditor();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::ValidateExpressionCell(TcxCustomEditorRowProperties *Sender,
		  int ARecordIndex, const Variant &AValue, TcxEditValidateInfo *AData)

{
  int AErrorCode = VerticalGrid->DataController->ErrorCodes[ARecordIndex][Sender->ItemIndex];
  if (AErrorCode > 0) {
	AData->ErrorType = eetError;
	AData->ErrorText = dxSpreadSheetErrorCodeToString((TdxSpreadSheetFormulaErrorCode)AErrorCode);
  }
}
//---------------------------------------------------------------------------

