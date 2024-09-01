//---------------------------------------------------------------------------

#ifndef CalculatedFieldsDemoMainH
#define CalculatedFieldsDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxCustomData.hpp"
#include "cxData.hpp"
#include "cxDBData.hpp"
#include "cxEdit.hpp"
#include "cxFilter.hpp"
#include "cxGraphics.hpp"
#include "cxStyles.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <DB.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "cxLookAndFeels.hpp"
#include "DemoBasicMain.h"
#include "cxDBVGrid.hpp"
#include "cxEditRepositoryItems.hpp"
#include "cxInplaceContainer.hpp"
#include "cxMaskEdit.hpp"
#include "cxVGrid.hpp"
#include "dxSkinsCore.hpp"
#include "cxButtons.hpp"
#include "cxCalendar.hpp"
#include "cxContainer.hpp"
#include "cxCurrencyEdit.hpp"
#include "cxGroupBox.hpp"
#include "cxImageList.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "dxGallery.hpp"
#include "dxGalleryControl.hpp"
#include "dxmdaset.hpp"
#include <DBClient.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
	TcxImageList *cxImageList1;
	TClientDataSet *cdsProducts2;
	TIntegerField *cdsProducts2ProductID;
	TStringField *cdsProducts2ProductName;
	TIntegerField *cdsProducts2SupplierID;
	TIntegerField *cdsProducts2CategoryID;
	TStringField *cdsProducts2QuantityPerUnit;
	TFloatField *cdsProducts2UnitPrice;
	TIntegerField *cdsProducts2UnitsInStock;
	TIntegerField *cdsProducts2UnitsOnOrder;
	TIntegerField *cdsProducts2ReorderLevel;
	TBooleanField *cdsProducts2Discontinued;
	TStringField *cdsProducts2EAN13;
	TdxMemData *mdOrder2;
	TIntegerField *mdOrder2OrderID;
	TIntegerField *mdOrder2ProductID;
	TFloatField *mdOrder2UnitPrice;
	TIntegerField *mdOrder2Quantity;
	TFloatField *mdOrder2Discount;
	TDateTimeField *mdOrder2OrderDate;
	TStringField *mdOrder2ProductName;
	TDataSource *dsOrder2;
	TcxGroupBox *cxGroupBox1;
	TcxGroupBox *cxGroupBox2;
	TcxButton *btnShowExpressionEditor;
	TdxGalleryControl *galColumns;
	TdxGalleryControlGroup *dxGalleryControl1Group1;
	TdxGalleryControlItem *dxGalleryControl1Group1Item1;
	TdxGalleryControlItem *dxGalleryControl1Group1Item2;
	TcxLabel *cxLabel1;
	TcxDBVerticalGrid *VerticalGrid;
	TcxCategoryRow *vgrCategoryRow1;
	TcxDBEditorRow *vgrOrderID;
	TcxDBEditorRow *vgrOrderDate;
	TcxCategoryRow *vgrCategoryRow2;
	TcxDBEditorRow *vgrProductName;
	TcxDBEditorRow *vgrUnitPrice;
	TcxDBEditorRow *vgrQuantity;
	TcxDBEditorRow *vgrDiscount;
	TcxCategoryRow *vgrCategoryRow3;
	TcxDBEditorRow *vgrDiscountAmount;
	TcxDBEditorRow *vgrTotal;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall btnShowExpressionEditorClick(TObject *Sender);
	void __fastcall ValidateExpressionCell(TcxCustomEditorRowProperties *Sender,
		  int ARecordIndex, const Variant &AValue, TcxEditValidateInfo *AData);

private:	// User declarations
public:		// User declarations
  __fastcall TfrmMain(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmMain *frmMain;
//---------------------------------------------------------------------------

class TdxGalleryControlOptionsBehaviorAccess : public TdxGalleryControlOptionsBehavior
{
public:
  __property ItemHotTrack;
};
//---------------------------------------------------------------------------
#endif
