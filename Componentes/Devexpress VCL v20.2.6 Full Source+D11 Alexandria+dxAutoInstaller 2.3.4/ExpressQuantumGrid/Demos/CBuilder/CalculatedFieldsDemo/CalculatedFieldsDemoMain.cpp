//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "CalculatedFieldsDemoMain.h"
#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxEdit"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxGrid"
#pragma link "cxGridBandedTableView"
#pragma link "cxGridCustomPopupMenu"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridDBBandedTableView"
#pragma link "cxGridLevel"
#pragma link "cxGridPopupMenu"
#pragma link "cxGridTableView"
#pragma link "cxStyles"
#pragma link "cxLookAndFeels"
#pragma link "cxButtons"
#pragma link "cxContainer"
#pragma link "cxCurrencyEdit"
#pragma link "cxDataStorage"
#pragma link "cxDBData"
#pragma link "cxGridCardView"
#pragma link "cxGridDBTableView"
#pragma link "cxGroupBox"
#pragma link "cxLabel"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxNavigator"
#pragma link "cxSpinEdit"
#pragma link "dxDateRanges"
#pragma link "dxGallery"
#pragma link "dxGalleryControl"
#pragma link "cxImageList"
#pragma link "dxmdaset"
#pragma link "dxSpreadSheetTypes"
#pragma link "dxSpreadSheetUtils"
#pragma resource "*.dfm"
TfrmMain *frmMain;
//---------------------------------------------------------------------------
__fastcall TfrmMain::TfrmMain(TComponent* Owner)
  : TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------


void __fastcall TfrmMain::btnShowExpressionEditorClick(TObject *Sender)
{
  if (galColumns->Gallery->Groups->Groups[0]->Items->Items[0]->Checked)
	tvOrdersTotal->ShowExpressionEditor();
  else
	tvOrdersDiscountAmount->ShowExpressionEditor();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::FormCreate(TObject *Sender)
{
  String APath = ExtractFilePath(Application->ExeName) + "..\\..\\Data\\";
  cdsProducts->LoadFromFile(APath + "Products2.cds");
  mdOrder->LoadFromBinaryFile(APath + "Order2.dat");

  tvOrders->DataController->Groups->FullExpand();

  ((TdxGalleryControlOptionsBehaviorAccess*)galColumns->OptionsBehavior)->ItemHotTrack = false;
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::ValidateExpressionColumn(TcxCustomGridTableItem *Sender,
		  TcxCustomGridRecord *ARecord, const Variant &AValue,
          TcxEditValidateInfo *AData)
{
  if (ARecord->IsData) {
	int AErrorCode = Sender->GridView->DataController->ErrorCodes[ARecord->RecordIndex][Sender->Index];
	if (AErrorCode > 0) {
	  AData->ErrorType = eetError;
	  AData->ErrorText = dxSpreadSheetErrorCodeToString((TdxSpreadSheetFormulaErrorCode)AErrorCode);
	}
  }
}
//---------------------------------------------------------------------------

