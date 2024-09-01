//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ScrollbarAnnotationsDemoMain.h"
#include "CarsData.h"
#include "CustomAnnotationSettings.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BaseForm"
#pragma link "cxClasses"
#pragma link "cxGridCardView"
#pragma link "cxGridTableView"
#pragma link "cxLookAndFeels"
#pragma link "cxStyles"
#pragma link "cxControls"
#pragma link "cxCustomData"
#pragma link "cxData"
#pragma link "cxDataStorage"
#pragma link "cxDBData"
#pragma link "cxEdit"
#pragma link "cxFilter"
#pragma link "cxGraphics"
#pragma link "cxGrid"
#pragma link "cxGridCustomTableView"
#pragma link "cxGridCustomView"
#pragma link "cxGridDBTableView"
#pragma link "cxGridLevel"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxNavigator"
#pragma link "cxTextEdit"
#pragma link "dxDateRanges"
#pragma resource "*.dfm"
TScrollbarAnnotationsDemoMainForm *ScrollbarAnnotationsDemoMainForm;
String SCustomAnnotationStrs[3];
//---------------------------------------------------------------------------
__fastcall TScrollbarAnnotationsDemoMainForm::TScrollbarAnnotationsDemoMainForm(TComponent* Owner)
	: TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::actScrollAnnotationsActiveExecute(TObject *Sender)

{
  cxGrid1DBTableView1->ScrollbarAnnotations->Active = dynamic_cast<TAction *>(Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::actShowErrorsExecute(TObject *Sender)

{
  cxGrid1DBTableView1->ScrollbarAnnotations->ShowErrors = dynamic_cast<TAction *>(Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::actShowSearchResultsExecute(TObject *Sender)

{
  cxGrid1DBTableView1->ScrollbarAnnotations->ShowSearchResults = dynamic_cast<TAction *>(Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::actShowFocusedRowExecute(TObject *Sender)

{
  cxGrid1DBTableView1->ScrollbarAnnotations->ShowFocusedRow = dynamic_cast<TAction *>(Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::actShowSelectedRowsExecute(TObject *Sender)

{
  cxGrid1DBTableView1->ScrollbarAnnotations->ShowSelectedRows = dynamic_cast<TAction *>(Sender)->Checked;
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::actCustomAnnotationSettingsExecute(TObject *Sender)

{
  if (!frmCustomAnnotationSettings)
  {
	Application->CreateForm(__classid(TfrmCustomAnnotationSettings), &frmCustomAnnotationSettings);
	frmCustomAnnotationSettings->Initialize(cxGrid1DBTableView1->ScrollbarAnnotations->CustomAnnotations);
  };
  frmCustomAnnotationSettings->Show();
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::cxGrid1DBTableView1KeyDown(TObject *Sender,
          WORD &Key, TShiftState Shift)
{
  Set <TdxScrollbarAnnotationKind, 0, 255> AKinds;
  AKinds << dxErrorScrollbarAnnotationID;
  if (Key == VK_F4)
	cxGrid1DBTableView1->GoToNextScrollbarAnnotation(AKinds, !Shift.Contains(ssShift));
}
//---------------------------------------------------------------------------
Variant TScrollbarAnnotationsDemoMainForm::GetValue(int ARecordIndex, TcxGridDBColumn *AColumn)
{
    if (cxGrid1DBTableView1->ViewData->Records[ARecordIndex]->IsData)
      return (cxGrid1DBTableView1->ViewData->Records[ARecordIndex]->Values[AColumn->Index]);
    else
	  return Null;
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::cxGrid1DBTableView1PopulateCustomScrollbarAnnotationRowIndexList(TcxCustomGridTableView *Sender,
		  int AAnnotationIndex, TdxScrollbarAnnotationRowIndexList *ARowIndexList)

{
  bool ACondition = false;
  Variant AValue;
  for (int i = 0; i < cxGrid1DBTableView1->ViewData->RecordCount; i++)
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
		ACondition = (GetValue(i, clCylinderCount) == 6);
		break;
	  default:
	    ACondition = false;
    };
	if (ACondition)
	  ARowIndexList->Add(i);
  };
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::cxGrid1DBTableView1GetScrollbarAnnotationHint(TcxCustomGridTableView *Sender,
		  TdxScrollbarAnnotationRowIndexLists *AScrollAnnotationRows,
		  UnicodeString &AHint)

{
  System::DynamicArray<TdxScrollbarAnnotationKind> AKinds = AScrollAnnotationRows->Keys->ToArray();
  TdxScrollbarAnnotationKind AKind;
  int ACount;
  String AAnnotationHint;
  TdxScrollbarAnnotationRowIndexList* AList;
  int ARecordIndex;
  for (int i = 0; i < AKinds.Length; i++)
  {
	AKind = AKinds[i];
	AScrollAnnotationRows->TryGetValue(AKind, AList);
	ACount = Min(5, AList->Count);
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
			AAnnotationHint = String::Format(" %s cylinders", ARRAYOFCONST((GetValue(ARecordIndex, clCylinderCount))));
			break;
		case dxErrorScrollbarAnnotationID:
			AAnnotationHint = " - MPG City is not specified!";
			break;
		default:
			AAnnotationHint = "";
	  };
	  if (AAnnotationHint != "")
	  {
		AAnnotationHint = String::Format("%s %s %s", ARRAYOFCONST((GetValue(ARecordIndex, clName), GetValue(ARecordIndex, clModification), AAnnotationHint)));
		AHint = AHint + IfThen(AHint != "", dxCRLF) + AAnnotationHint;
	  };
	};
	if (AList->Count > ACount)
	  AHint = AHint + dxCRLF + "And " + IntToStr(AList->Count - ACount) +  " more records...";
  };
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::cxGrid1DBTableView1MPGCityValidateDrawValue(TcxCustomGridTableItem *Sender,
          TcxCustomGridRecord *ARecord, const Variant &AValue,
          TcxEditValidateInfo *AData)
{
  if (VarIsStr(AValue) && (VarToStr(AValue).IsEmpty()) || (VarIsNull(AValue)))
	AData->ErrorType = eetError;
}
//---------------------------------------------------------------------------

void __fastcall TScrollbarAnnotationsDemoMainForm::FormCreate(TObject *Sender)
{
  SCustomAnnotationStrs[0] = "2-Door Cars";
  SCustomAnnotationStrs[1] = "Price is Greater Than " + CurrToStrF(100000, ffCurrency, 0);
  SCustomAnnotationStrs[2] = "6-Cylinder Cars";
  cxGrid1DBTableView1->DataController->FindCriteria->Text = "V8";
  actDoorCount->Caption = SCustomAnnotationStrs[0];
  actPrice->Caption = SCustomAnnotationStrs[1];
  actCylinderCount->Caption = SCustomAnnotationStrs[2];
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoMainForm::actCustomScrollAnnotationExecute(TObject *Sender)

{
  cxGrid1DBTableView1->ScrollbarAnnotations->CustomAnnotations->Items[dynamic_cast<TAction *>(Sender)->Tag]->Visible = dynamic_cast<TAction *>(Sender)->Checked;
}
//---------------------------------------------------------------------------

