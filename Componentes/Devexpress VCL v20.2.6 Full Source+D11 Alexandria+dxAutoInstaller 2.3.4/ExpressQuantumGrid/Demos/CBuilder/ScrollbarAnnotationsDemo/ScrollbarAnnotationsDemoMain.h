//---------------------------------------------------------------------------

#ifndef ScrollbarAnnotationsDemoMainH
#define ScrollbarAnnotationsDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BaseForm.h"
#include "cxClasses.hpp"
#include "cxGridCardView.hpp"
#include "cxGridTableView.hpp"
#include "cxLookAndFeels.hpp"
#include "cxStyles.hpp"
#include <ComCtrls.hpp>
#include <Menus.hpp>
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxDataStorage.hpp"
#include "cxDBData.hpp"
#include "cxEdit.hpp"
#include "cxFilter.hpp"
#include "cxGraphics.hpp"
#include "cxGrid.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridDBTableView.hpp"
#include "cxGridLevel.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxNavigator.hpp"
#include "cxTextEdit.hpp"
#include "dxDateRanges.hpp"
#include <DB.hpp>
#include <ActnList.hpp>
//---------------------------------------------------------------------------
class TScrollbarAnnotationsDemoMainForm : public TfmBaseForm
{
__published:	// IDE-managed Components
	TcxGrid *cxGrid1;
	TcxGridDBTableView *cxGrid1DBTableView1;
	TcxGridDBColumn *clName;
	TcxGridDBColumn *clModification;
	TcxGridDBColumn *clPrice;
	TcxGridDBColumn *cxGrid1DBTableView1MPGCity;
	TcxGridDBColumn *cxGrid1DBTableView1MPGHighway;
	TcxGridDBColumn *clDoorCount;
	TcxGridDBColumn *clCylinderCount;
	TcxGridDBColumn *cxGrid1DBTableView1Horsepower;
	TcxGridDBColumn *cxGrid1DBTableView1TransmissionSpeeds;
	TcxGridDBColumn *clDescription;
	TcxGridLevel *cxGrid1Level1;
	TActionList *ActionList1;
	TAction *actScrollAnnotationsActive;
	TAction *actDoorCount;
	TAction *actPrice;
	TAction *actCylinderCount;
	TAction *actShowErrors;
	TAction *actShowSearchResults;
	TAction *actShowFocusedRow;
	TAction *actShowSelectedRows;
	TAction *actCustomAnnotationSettings;
	void __fastcall actScrollAnnotationsActiveExecute(TObject *Sender);
	void __fastcall actShowErrorsExecute(TObject *Sender);
	void __fastcall actShowSearchResultsExecute(TObject *Sender);
	void __fastcall actShowFocusedRowExecute(TObject *Sender);
	void __fastcall actShowSelectedRowsExecute(TObject *Sender);
	void __fastcall actCustomAnnotationSettingsExecute(TObject *Sender);
	void __fastcall cxGrid1DBTableView1KeyDown(TObject *Sender, WORD &Key, TShiftState Shift);
	void __fastcall cxGrid1DBTableView1PopulateCustomScrollbarAnnotationRowIndexList(TcxCustomGridTableView *Sender,
		  int AAnnotationIndex, TdxScrollbarAnnotationRowIndexList *ARowIndexList);
	void __fastcall cxGrid1DBTableView1GetScrollbarAnnotationHint(TcxCustomGridTableView *Sender,
          TdxScrollbarAnnotationRowIndexLists *AScrollAnnotationRows,
          UnicodeString &AHint);
	void __fastcall cxGrid1DBTableView1MPGCityValidateDrawValue(TcxCustomGridTableItem *Sender,
          TcxCustomGridRecord *ARecord, const Variant &AValue,
          TcxEditValidateInfo *AData);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall actCustomScrollAnnotationExecute(TObject *Sender);



private:	// User declarations
    Variant GetValue(int ARecordIndex, TcxGridDBColumn *AColumn);
public:		// User declarations
	__fastcall TScrollbarAnnotationsDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TScrollbarAnnotationsDemoMainForm *ScrollbarAnnotationsDemoMainForm;
//---------------------------------------------------------------------------
#endif
