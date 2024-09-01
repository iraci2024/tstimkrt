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
#include "cxDBVGrid.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxInplaceContainer.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxStyles.hpp"
#include "cxTextEdit.hpp"
#include "cxVGrid.hpp"
//---------------------------------------------------------------------------
class TScrollbarAnnotationsDemoMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
	TcxDBVerticalGrid *cxDBVerticalGrid1;
	TcxDBEditorRow *clName;
	TcxDBEditorRow *clModification;
	TcxDBEditorRow *clPrice;
	TcxDBEditorRow *cxDBVerticalGrid1MPGCity;
	TcxDBEditorRow *cxDBVerticalGrid1MPGHighway;
	TcxDBEditorRow *clDoorCount;
	TcxDBEditorRow *clCilinderCount;
	TcxDBEditorRow *cxDBVerticalGrid1Horsepower;
	TcxDBEditorRow *cxDBVerticalGrid1Torque;
	TcxDBEditorRow *cxDBVerticalGrid1TransmissionSpeeds;
	TcxDBEditorRow *cxDBVerticalGrid1TransmissionType;
	TcxDBEditorRow *cxDBVerticalGrid1Description;
	TcxDBEditorRow *cxDBVerticalGrid1DeliveryDate;
	TActionList *ActionList1;
	TAction *actScrollAnnotationsActive;
	TAction *actDoorCount;
	TAction *actPrice;
	TAction *actCylinderCount;
	TAction *actShowErrors;
	TAction *actShowSearchResults;
	TAction *actShowFocusedRow;
	TAction *actCustomAnnotationSettings;
	void __fastcall actScrollAnnotationsActiveExecute(TObject *Sender);
	void __fastcall actDoorCountExecute(TObject *Sender);
	void __fastcall actShowErrorsExecute(TObject *Sender);
	void __fastcall actShowSearchResultsExecute(TObject *Sender);
	void __fastcall actShowFocusedRowExecute(TObject *Sender);
	void __fastcall actCustomAnnotationSettingsExecute(TObject *Sender);
	void __fastcall cxDBVerticalGrid1PopulateCustomScrollbarAnnotationRowIndexList(TObject *Sender, int AAnnotationIndex,
          TdxScrollbarAnnotationRowIndexList *ARowIndexList);
	void __fastcall cxDBVerticalGrid1GetScrollbarAnnotationHint(TObject *Sender, TdxScrollbarAnnotationRowIndexLists *AAnnotationRowIndexLists,
          UnicodeString &AHint);
	void __fastcall cxDBVerticalGrid1KeyDown(TObject *Sender, WORD &Key, TShiftState Shift);
	void __fastcall cxDBVerticalGrid1MPGCityPropertiesValidateDrawValue(TcxCustomEditorRowProperties *Sender,
          int ARecordIndex, const Variant &AValue, TcxEditValidateInfo *AData);
	void __fastcall FormCreate(TObject *Sender);



private:	// User declarations
  Variant GetValue(int ARecordIndex, TcxDBEditorRow *AColumn);
public:		// User declarations
	__fastcall TScrollbarAnnotationsDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TScrollbarAnnotationsDemoMainForm *ScrollbarAnnotationsDemoMainForm;
//---------------------------------------------------------------------------
#endif
