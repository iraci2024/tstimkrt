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
#include "cxEdit.hpp"
#include "cxFilter.hpp"
#include "cxGraphics.hpp"
#include "cxGrid.hpp"
#include "cxGridBandedTableView.hpp"
#include "cxGridCustomPopupMenu.hpp"
#include "cxGridCustomTableView.hpp"
#include "cxGridCustomView.hpp"
#include "cxGridDBBandedTableView.hpp"
#include "cxGridLevel.hpp"
#include "cxGridPopupMenu.hpp"
#include "cxGridTableView.hpp"
#include "cxStyles.hpp"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <ImgList.hpp>
#include <Menus.hpp>
#include "cxLookAndFeels.hpp"
#include "BaseForm.h"
#include "cxButtons.hpp"
#include "cxContainer.hpp"
#include "cxCurrencyEdit.hpp"
#include "cxDataStorage.hpp"
#include "cxDBData.hpp"
#include "cxGridCardView.hpp"
#include "cxGridDBTableView.hpp"
#include "cxGroupBox.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxNavigator.hpp"
#include "cxSpinEdit.hpp"
#include "dxDateRanges.hpp"
#include "dxGallery.hpp"
#include "dxGalleryControl.hpp"
#include <DB.hpp>
#include "cxImageList.hpp"
#include "dxmdaset.hpp"
#include <DBClient.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TfmBaseForm
{
__published:  // IDE-managed Components
	TcxImageList *cxImageList1;
	TClientDataSet *cdsProducts;
	TIntegerField *cdsProductsProductID;
	TStringField *cdsProductsProductName;
	TIntegerField *cdsProductsSupplierID;
	TIntegerField *cdsProductsCategoryID;
	TStringField *cdsProductsQuantityPerUnit;
	TFloatField *cdsProductsUnitPrice;
	TIntegerField *cdsProductsUnitsInStock;
	TIntegerField *cdsProductsUnitsOnOrder;
	TIntegerField *cdsProductsReorderLevel;
	TBooleanField *cdsProductsDiscontinued;
	TStringField *cdsProductsEAN13;
	TdxMemData *mdOrder;
	TIntegerField *mdOrderOrderID;
	TIntegerField *mdOrderProductID;
	TFloatField *mdOrderUnitPrice;
	TIntegerField *mdOrderQuantity;
	TFloatField *mdOrderDiscount;
	TDateTimeField *mdOrderOrderDate;
	TStringField *mdOrderProductName;
	TDataSource *dsOrder;
	TcxGroupBox *cxGroupBox1;
	TcxGroupBox *cxGroupBox2;
	TcxButton *btnShowExpressionEditor;
	TdxGalleryControl *galColumns;
	TdxGalleryControlGroup *dxGalleryControl1Group1;
	TdxGalleryControlItem *dxGalleryControl1Group1Item1;
	TdxGalleryControlItem *dxGalleryControl1Group1Item2;
	TcxLabel *cxLabel1;
	TcxGrid *Grid;
	TcxGridDBTableView *tvOrders;
	TcxGridDBColumn *tvOrdersOrderID;
	TcxGridDBColumn *tvOrdersProductName;
	TcxGridDBColumn *tvOrdersUnitPrice;
	TcxGridDBColumn *tvOrdersQuantity;
	TcxGridDBColumn *tvOrdersDiscount;
	TcxGridDBColumn *tvOrdersDiscountAmount;
	TcxGridDBColumn *tvOrdersTotal;
	TcxGridLevel *GridLevel1;
	TcxGridPopupMenu *cxGridPopupMenu1;
	void __fastcall btnShowExpressionEditorClick(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall ValidateExpressionColumn(TcxCustomGridTableItem *Sender,
		  TcxCustomGridRecord *ARecord, const Variant &AValue,
		  TcxEditValidateInfo *AData);
private:  // User declarations
public:   // User declarations
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
#endif
