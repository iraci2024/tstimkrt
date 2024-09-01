//---------------------------------------------------------------------------

#ifndef ScrollbarAnnotationsDemoDataH
#define ScrollbarAnnotationsDemoDataH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include "dxmdaset.hpp"
#include <DB.hpp>
//---------------------------------------------------------------------------
class TScrollbarAnnotationsDemoDataDM : public TDataModule
{
__published:	// IDE-managed Components
	TdxMemData *mdDepartments;
	TAutoIncField *mdDepartmentsID;
	TIntegerField *mdDepartmentsPARENTID;
	TIntegerField *mdDepartmentsMANAGERID;
	TStringField *mdDepartmentsNAME;
	TFloatField *mdDepartmentsBUDGET;
	TStringField *mdDepartmentsLOCATION;
	TStringField *mdDepartmentsPHONE;
	TStringField *mdDepartmentsFAX;
	TStringField *mdDepartmentsEMAIL;
	TBooleanField *mdDepartmentsVACANCY;
	TDataSource *dsDepartments;
	void __fastcall DataModuleCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TScrollbarAnnotationsDemoDataDM(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TScrollbarAnnotationsDemoDataDM *ScrollbarAnnotationsDemoDataDM;
//---------------------------------------------------------------------------
#endif
