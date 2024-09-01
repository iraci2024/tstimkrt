//---------------------------------------------------------------------------


#pragma hdrstop

#include "ScrollbarAnnotationsDemoData.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma classgroup "Vcl.Controls.TControl"
#pragma link "dxmdaset"
#pragma resource "*.dfm"
TScrollbarAnnotationsDemoDataDM *ScrollbarAnnotationsDemoDataDM;
//---------------------------------------------------------------------------
__fastcall TScrollbarAnnotationsDemoDataDM::TScrollbarAnnotationsDemoDataDM(TComponent* Owner)
	: TDataModule(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TScrollbarAnnotationsDemoDataDM::DataModuleCreate(TObject *Sender)
{
  mdDepartments->LoadFromBinaryFile("..\\..\\Data\\Departments.dat");
  mdDepartments->Open();
}
//---------------------------------------------------------------------------
