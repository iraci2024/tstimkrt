//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ScrollbarAnnotationsDemoMain.h"
#include "CustomAnnotationSettings.h"
#include "CarsData.h"
#include "ScrollbarAnnotationsDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "DemoBasicMain"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxDBTL"
#pragma link "cxGraphics"
#pragma link "cxInplaceContainer"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxMaskEdit"
#pragma link "cxStyles"
#pragma link "cxTextEdit"
#pragma link "cxTL"
#pragma link "cxTLData"
#pragma link "cxCheckBox"
#pragma resource "*.dfm"
TScrollbarAnnotationsDemoMainForm *ScrollbarAnnotationsDemoMainForm;
String SCustomAnnotationStrs[3];
//---------------------------------------------------------------------------
__fastcall TScrollbarAnnotationsDemoMainForm::TScrollbarAnnotationsDemoMainForm(TComponent* Owner)
	: TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::actScrollAnnotationsActiveExecute(TObject *Sender)

{
  cxDBTreeList1->ScrollbarAnnotations->Active = dynamic_cast<TAction *>(Sender)->Checked;
}
//---------------------------------------------------------------------------
Variant TScrollbarAnnotationsDemoMainForm::GetValue(int ARecordIndex, TcxTreeListColumn *AColumn)
{
  return (cxDBTreeList1->AbsoluteVisibleItems[ARecordIndex]->Values[AColumn->ItemIndex]);
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::actShowErrorsExecute(TObject *Sender)

{
  cxDBTreeList1->ScrollbarAnnotations->ShowErrors = dynamic_cast<TAction *>(Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::actShowSearchResultsExecute(TObject *Sender)

{
  cxDBTreeList1->ScrollbarAnnotations->ShowSearchResults = dynamic_cast<TAction *>(Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::actShowFocusedRowExecute(TObject *Sender)

{
  cxDBTreeList1->ScrollbarAnnotations->ShowFocusedRow = dynamic_cast<TAction *>(Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::actShowSelectedRowsExecute(TObject *Sender)

{
  cxDBTreeList1->ScrollbarAnnotations->ShowSelectedRows = dynamic_cast<TAction *>(Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::actCustomAnnotationSettingsExecute(TObject *Sender)

{
  if (!frmCustomAnnotationSettings)
  {
	Application->CreateForm(__classid(TfrmCustomAnnotationSettings), &frmCustomAnnotationSettings);
	frmCustomAnnotationSettings->Initialize(cxDBTreeList1->ScrollbarAnnotations->CustomAnnotations);
  };
  frmCustomAnnotationSettings->Show();
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::actVacancyExecute(TObject *Sender)

{
  cxDBTreeList1->ScrollbarAnnotations->CustomAnnotations->Items[dynamic_cast<TAction *>(Sender)->Tag]->Visible = dynamic_cast<TAction *>(Sender)->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TScrollbarAnnotationsDemoMainForm::cxDBTreeList1PopulateCustomScrollbarAnnotationRowIndexList(TObject *Sender,
          int AAnnotationIndex, TdxScrollbarAnnotationRowIndexList *ARowIndexList)
{
  bool ACondition = false;
  String s;
  Variant AValue;
  for (int i = 0; i < cxDBTreeList1->AbsoluteVisibleCount; i++)
  {
	switch (AAnnotationIndex)
	{
	  case 0:
		  AValue = GetValue(i, clVACANCY);
		  ACondition = !VarIsNull(AValue) && (AValue == VARIANT_TRUE);
		  break;
	  case 1:
		  ACondition = (GetValue(i, clBUDGET) > 30000);
		  break;
	  case 2:
		s = GetValue(i, clPHONE);
		ACondition = (s.Length() > 1) && (s.Pos("8") == 2);
		break;
	  default:
		ACondition = false;
	};
	if (ACondition)
	  ARowIndexList->Add(i);
  };
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::cxDBTreeList1GetScrollbarAnnotationHint(TObject *Sender,
		  TdxScrollbarAnnotationRowIndexLists *AAnnotationRowIndexLists, UnicodeString &AHint)

{
  System::DynamicArray<TdxScrollbarAnnotationKind> AKinds = AAnnotationRowIndexLists->Keys->ToArray();
  TdxScrollbarAnnotationKind AKind;
  int ACount;
  String AAnnotationHint;
  TdxScrollbarAnnotationRowIndexList* AList;
  int ARecordIndex;
  for (int i = 0; i < AKinds.Length; i++)
  {
	AKind = AKinds[i];
	AAnnotationRowIndexLists->TryGetValue(AKind, AList);
	ACount = Min(5, AList->Count);
	for (int j = 0; j < ACount; j++)
	{
	  ARecordIndex = AList->Items[j];
	  switch (AKind)
	  {
		case 0:
			AAnnotationHint = "has vacancy";
			break;
		case 1:
		case dxErrorScrollbarAnnotationID:
		   {
			float f = GetValue(ARecordIndex, clBUDGET);
			AAnnotationHint = "budget is " + FormatFloat("$,0;($,0)", f);
			break;
		   }
		case 2:
			AAnnotationHint = String::Format("phone number is %s", ARRAYOFCONST((GetValue(ARecordIndex, clPHONE))));
			break;
		default:
		    AAnnotationHint = "";
	  };
	  if (AAnnotationHint != "")
	  {
		AAnnotationHint = String::Format("%s %s", ARRAYOFCONST((GetValue(ARecordIndex, cxDBTreeListNAME), AAnnotationHint)));
		AHint = AHint + IfThen(AHint != "", dxCRLF) + AAnnotationHint;
	  };
	};
	if (AList->Count > ACount)
	  AHint = AHint + dxCRLF + "And " + IntToStr(AList->Count - ACount) +  " more records...";
  };
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::cxDBTreeList1KeyDown(TObject *Sender,
		  WORD &Key, TShiftState Shift)
{
  Set <TdxScrollbarAnnotationKind, 0, 255> AKinds;
  AKinds << dxErrorScrollbarAnnotationID;
  if (Key == VK_F4)
	cxDBTreeList1->GoToNextScrollbarAnnotation(AKinds, !Shift.Contains(ssShift));
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::clBUDGETValidateDrawValue(TcxTreeListColumn *Sender,
          TcxTreeListNode *ANode, const Variant &AValue, TcxEditValidateInfo *AData)

{
  if (AValue < 15000)
	AData->ErrorType = eetWarning;
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::FormCreate(TObject *Sender)
{
  SCustomAnnotationStrs[0] = "Has open vacancy";
  SCustomAnnotationStrs[1] = "Budget is Greater Than " + CurrToStrF(30000, ffCurrency, 0);
  SCustomAnnotationStrs[2] = "Phone number starts with 8";
  cxDBTreeList1->DataController->FindCriteria->Text = "00000";
  actVacancy->Caption = SCustomAnnotationStrs[0];
  actBudget->Caption = SCustomAnnotationStrs[1];
  actPhoneNumber->Caption = SCustomAnnotationStrs[2];
  cxDBTreeList1->AbsoluteVisibleItems[0]->Expand(false);
}
//---------------------------------------------------------------------------

