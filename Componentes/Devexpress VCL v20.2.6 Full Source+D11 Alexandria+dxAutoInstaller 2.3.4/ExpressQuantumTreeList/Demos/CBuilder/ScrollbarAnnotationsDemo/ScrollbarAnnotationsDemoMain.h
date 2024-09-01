//---------------------------------------------------------------------------

#ifndef ScrollbarAnnotationsDemoMainH
#define ScrollbarAnnotationsDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxLookAndFeels.hpp"
#include "DemoBasicMain.h"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxDBTL.hpp"
#include "cxGraphics.hpp"
#include "cxInplaceContainer.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxMaskEdit.hpp"
#include "cxStyles.hpp"
#include "cxTextEdit.hpp"
#include "cxTL.hpp"
#include "cxTLData.hpp"
#include "cxCheckBox.hpp"
//---------------------------------------------------------------------------
class TScrollbarAnnotationsDemoMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
	TActionList *ActionList1;
	TAction *actScrollAnnotationsActive;
	TAction *actVacancy;
	TAction *actBudget;
	TAction *actPhoneNumber;
	TAction *actShowErrors;
	TAction *actShowSearchResults;
	TAction *actShowFocusedRow;
	TAction *actShowSelectedRows;
	TAction *actCustomAnnotationSettings;
	TcxDBTreeList *cxDBTreeList1;
	TcxDBTreeListColumn *cxDBTreeListID;
	TcxDBTreeListColumn *cxDBTreeListPARENTID;
	TcxDBTreeListColumn *cxDBTreeListNAME;
	TcxDBTreeListColumn *clBUDGET;
	TcxDBTreeListColumn *clPHONE;
	TcxDBTreeListColumn *cxDBTreeListFAX;
	TcxDBTreeListColumn *cxDBTreeListEMAIL;
	TcxDBTreeListColumn *clVACANCY;
	TMenuItem *Scrollannotations1;
	TMenuItem *N1;
	TMenuItem *N2;
	TMenuItem *Active1;
	TMenuItem *Customannotationssettings1;
	TMenuItem *Budgetmorethen300001;
	TMenuItem *Phonenumberstartswith81;
	void __fastcall actScrollAnnotationsActiveExecute(TObject *Sender);
	void __fastcall actShowErrorsExecute(TObject *Sender);
	void __fastcall actShowSearchResultsExecute(TObject *Sender);
	void __fastcall actShowFocusedRowExecute(TObject *Sender);
	void __fastcall actShowSelectedRowsExecute(TObject *Sender);
	void __fastcall actCustomAnnotationSettingsExecute(TObject *Sender);
	void __fastcall actVacancyExecute(TObject *Sender);
	void __fastcall cxDBTreeList1PopulateCustomScrollbarAnnotationRowIndexList(TObject *Sender, int AAnnotationIndex,
          TdxScrollbarAnnotationRowIndexList *ARowIndexList);
	void __fastcall cxDBTreeList1GetScrollbarAnnotationHint(TObject *Sender, TdxScrollbarAnnotationRowIndexLists *AAnnotationRowIndexLists,
          UnicodeString &AHint);
	void __fastcall cxDBTreeList1KeyDown(TObject *Sender, WORD &Key, TShiftState Shift);
	void __fastcall clBUDGETValidateDrawValue(TcxTreeListColumn *Sender, TcxTreeListNode *ANode,
          const Variant &AValue, TcxEditValidateInfo *AData);
	void __fastcall FormCreate(TObject *Sender);


private:	// User declarations
  Variant GetValue(int ARecordIndex, TcxTreeListColumn *AColumn);
public:		// User declarations
	__fastcall TScrollbarAnnotationsDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TScrollbarAnnotationsDemoMainForm *ScrollbarAnnotationsDemoMainForm;
//---------------------------------------------------------------------------
#endif
