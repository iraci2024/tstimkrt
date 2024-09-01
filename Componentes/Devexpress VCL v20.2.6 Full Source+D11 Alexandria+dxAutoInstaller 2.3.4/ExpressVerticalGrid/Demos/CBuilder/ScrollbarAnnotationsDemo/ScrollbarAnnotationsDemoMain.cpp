//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ScrollbarAnnotationsDemoMain.h"
#include "CarsData.h"
#include "CustomAnnotationSettings.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "DemoBasicMain"
#pragma link "cxControls"
#pragma link "cxDBVGrid"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxInplaceContainer"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxStyles"
#pragma link "cxTextEdit"
#pragma link "cxVGrid"
#pragma resource "*.dfm"
TScrollbarAnnotationsDemoMainForm *ScrollbarAnnotationsDemoMainForm;
String SCustomAnnotationStrs[3];
//---------------------------------------------------------------------------
__fastcall TScrollbarAnnotationsDemoMainForm::TScrollbarAnnotationsDemoMainForm(TComponent* Owner)
	: TDemoBasicMainForm(Owner)
{
}
//---------------------------------------------------------------------------
Variant TScrollbarAnnotationsDemoMainForm::GetValue(int ARecordIndex, TcxDBEditorRow *AColumn)
{
  return AColumn->Properties->Values[ARecordIndex];
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::actScrollAnnotationsActiveExecute(TObject *Sender)

{
  cxDBVerticalGrid1->ScrollbarAnnotations->Active = dynamic_cast<TAction *>(Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::actDoorCountExecute(TObject *Sender)
{
  cxDBVerticalGrid1->ScrollbarAnnotations->CustomAnnotations->Items[dynamic_cast<TAction *>(Sender)->Tag]->Visible = dynamic_cast<TAction *>(Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::actShowErrorsExecute(TObject *Sender)
{
  cxDBVerticalGrid1->ScrollbarAnnotations->ShowErrors = dynamic_cast<TAction *>(Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::actShowSearchResultsExecute(TObject *Sender)

{
  cxDBVerticalGrid1->ScrollbarAnnotations->ShowSearchResults = dynamic_cast<TAction *>(Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::actShowFocusedRowExecute(TObject *Sender)
{
  cxDBVerticalGrid1->ScrollbarAnnotations->ShowFocusedRow = dynamic_cast<TAction *>(Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::actCustomAnnotationSettingsExecute(TObject *Sender)

{
  if (!frmCustomAnnotationSettings)
  {
	Application->CreateForm(__classid(TfrmCustomAnnotationSettings), &frmCustomAnnotationSettings);
	frmCustomAnnotationSettings->Initialize(cxDBVerticalGrid1->ScrollbarAnnotations->CustomAnnotations);
  };
  frmCustomAnnotationSettings->Show();
}
//---------------------------------------------------------------------------

void __fastcall TScrollbarAnnotationsDemoMainForm::cxDBVerticalGrid1PopulateCustomScrollbarAnnotationRowIndexList(TObject *Sender,
		  int AAnnotationIndex, TdxScrollbarAnnotationRowIndexList *ARowIndexList)
{
  bool ACondition = false;
  Variant AValue;
  for (int i = 0; i < cxDBVerticalGrid1->RecordCount; i++)
  {
	switch (AAnnotationIndex)
	{
	  case 0:
		  AValue = GetValue(i, clDoorCount);
		  ACondition = !VarIsNull(AValue) && (AValue == 2);
		  break;
	  case 1:
		ACondition = (GetValue(i, clPrice) > 100000);
		break;
	  case 2:
		ACondition = (GetValue(i, clCilinderCount) == 6);
		break;
	  default:
	    ACondition = false;
    };
	if (ACondition)
	  ARowIndexList->Add(i);
  };
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::cxDBVerticalGrid1GetScrollbarAnnotationHint(TObject *Sender,
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
	ACount = Math::Min(5, AList->Count);
	for (int j = 0; j < ACount; j++)
	{
	  ARecordIndex = AList->Items[j];
	  switch (AKind)
	  {
		case 0:
			AAnnotationHint = String::Format(" %s doors", ARRAYOFCONST((GetValue(ARecordIndex, clDoorCount))));
			break;
		case 1:
		   {
			float f = GetValue(ARecordIndex, clPrice);
			AAnnotationHint = " (price: " + FormatFloat("$,0;($,0)", f) + ")";
			break;
		   }
		case 2:
			AAnnotationHint = String::Format(" %s cylinders", ARRAYOFCONST((GetValue(ARecordIndex, clCilinderCount))));
			break;
		case dxErrorScrollbarAnnotationID:
			AAnnotationHint = " - MPG City is not specified!";
			break;
		default:
		    AAnnotationHint = "";
	  };
	  if (AAnnotationHint != "")
	  {
		AAnnotationHint = String::Format("%s %s%s", ARRAYOFCONST((GetValue(ARecordIndex, clName), GetValue(ARecordIndex, clModification), AAnnotationHint)));
		AHint = AHint + IfThen(AHint != "", dxCRLF) + AAnnotationHint;
	  };
	};
	if (AList->Count > ACount)
	  AHint = AHint + dxCRLF + "And " + IntToStr(AList->Count - ACount) +  " more records...";
  };
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::cxDBVerticalGrid1KeyDown(TObject *Sender, WORD &Key,
          TShiftState Shift)
{
  Set <TdxScrollbarAnnotationKind, 0, 255> AKinds;
  AKinds << dxErrorScrollbarAnnotationID;
  if (Key == VK_F4)
	cxDBVerticalGrid1->GoToNextScrollbarAnnotation(AKinds, !Shift.Contains(ssShift));
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::cxDBVerticalGrid1MPGCityPropertiesValidateDrawValue(TcxCustomEditorRowProperties *Sender,
          int ARecordIndex, const Variant &AValue, TcxEditValidateInfo *AData)

{
  if (VarIsNull(AValue))
	AData->ErrorType = eetError;
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::FormCreate(TObject *Sender)
{
  SCustomAnnotationStrs[0] = "2-Door Cars";
  SCustomAnnotationStrs[1] = "Price is Greater Than " + CurrToStrF(100000, ffCurrency, 0);
  SCustomAnnotationStrs[2] = "6-Cylinder Cars";
  cxDBVerticalGrid1->DataController->FindCriteria->Text = "V8";
  actDoorCount->Caption = SCustomAnnotationStrs[0];
  actPrice->Caption = SCustomAnnotationStrs[1];
  actCylinderCount->Caption = SCustomAnnotationStrs[2];
}
//---------------------------------------------------------------------------

