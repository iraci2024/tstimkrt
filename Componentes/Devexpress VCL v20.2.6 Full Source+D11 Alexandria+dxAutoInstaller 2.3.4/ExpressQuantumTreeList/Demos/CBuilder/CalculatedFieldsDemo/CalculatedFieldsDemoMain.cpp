//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "CalculatedFieldsDemoMain.h"
#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDBData"
#pragma link "cxEdit"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxStyles"
#pragma link "cxLookAndFeels"
#pragma link "DemoBasicMain"
#pragma link "cxDBLookupComboBox"
#pragma link "cxDBTL"
#pragma link "cxEditRepositoryItems"
#pragma link "cxInplaceContainer"
#pragma link "cxMaskEdit"
#pragma link "cxTL"
#pragma link "cxTLData"
#pragma link "cxCheckBox"
#pragma link "cxButtons"
#pragma link "cxContainer"
#pragma link "cxCurrencyEdit"
#pragma link "cxGroupBox"
#pragma link "cxLabel"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxTLdxBarBuiltInMenu"
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
  cdsProducts->LoadFromFile(APath + "Products2.cds");
  mdOrder->LoadFromBinaryFile(APath + "Order2TL.dat");
  ((TdxGalleryControlOptionsBehaviorAccess*)galColumns->OptionsBehavior)->ItemHotTrack = false;
  TreeList->FullExpand();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::mdOrderCalcFields(TDataSet *DataSet)
{
  if (cdsProducts->FindKey(ARRAYOFCONST((mdOrder->FieldByName("ProductID")->AsInteger))))
	mdOrder->FieldByName("ProductName")->AsString = cdsProducts->FieldByName("ProductName")->AsString;
  else
	mdOrder->FieldByName("ProductName")->AsString = Format(AnsiString("Order #%d"), ARRAYOFCONST((mdOrder->FieldByName("OrderID")->AsInteger)));
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::btnShowExpressionEditorClick(TObject *Sender)
{
  if (galColumns->Gallery->Groups->Groups[0]->Items->Items[0]->Checked)
	tlcTotal->ShowExpressionEditor();
  else
	tlcAmountDiscount->ShowExpressionEditor();
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::TreeListEditing(TcxCustomTreeList *Sender, TcxTreeListColumn *AColumn,
          bool &Allow)
{
  Allow = (Allow && !(TreeList->FocusedNode->IsGroupNode));
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::TreeListStylesGetContentStyle(TcxCustomTreeList *Sender,
          TcxTreeListColumn *AColumn, TcxTreeListNode *ANode, TcxStyle *&AStyle)

{
  if (ANode->IsGroupNode)
        AStyle = stGroup;
}
//---------------------------------------------------------------------------

void __fastcall TfrmMain::TreeListSummary(TcxCustomTreeList *ASender, const TcxTreeListSummaryEventArguments &Arguments,
          TcxTreeListSummaryEventOutArguments &OutArguments)

{
  OutArguments.Done = Arguments.Node->IsGroupNode;
}
//---------------------------------------------------------------------------


void __fastcall TfrmMain::ValidateExpressionColumn(TcxTreeListColumn *Sender,
          TcxTreeListNode *ANode, const Variant &AValue, TcxEditValidateInfo *AData)

{
  int AErrorCode = ANode->ErrorCodes[Sender->ItemIndex];
  if (AErrorCode > 0) {
	AData->ErrorType = eetError;
	AData->ErrorText = dxSpreadSheetErrorCodeToString((TdxSpreadSheetFormulaErrorCode)AErrorCode);
  }
}
//---------------------------------------------------------------------------

