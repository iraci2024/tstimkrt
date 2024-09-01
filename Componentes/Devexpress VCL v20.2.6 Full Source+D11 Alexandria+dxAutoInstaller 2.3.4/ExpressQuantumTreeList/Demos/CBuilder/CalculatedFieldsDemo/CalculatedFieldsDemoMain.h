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
#include "cxDBLookupComboBox.hpp"
#include "cxDBTL.hpp"
#include "cxEditRepositoryItems.hpp"
#include "cxInplaceContainer.hpp"
#include "cxMaskEdit.hpp"
#include "cxTL.hpp"
#include "cxTLData.hpp"
#include "cxButtons.hpp"
#include "cxContainer.hpp"
#include "cxCurrencyEdit.hpp"
#include "cxGroupBox.hpp"
#include "cxLabel.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxTLdxBarBuiltInMenu.hpp"
#include "dxGallery.hpp"
#include "dxGalleryControl.hpp"
#include "dxmdaset.hpp"
#include <DBClient.hpp>
//---------------------------------------------------------------------------
class TfrmMain : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
	TDataSource *dsOrder;
	TdxMemData *mdOrder;
	TIntegerField *IntegerField1;
	TIntegerField *mdOrderParentOrderID;
	TIntegerField *IntegerField2;
	TFloatField *FloatField1;
	TIntegerField *IntegerField3;
	TFloatField *FloatField2;
	TDateTimeField *DateTimeField1;
	TStringField *mdOrderProductName;
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
	TcxStyleRepository *cxStyleRepository1;
	TcxStyle *stGroup;
	TcxGroupBox *cxGroupBox1;
	TcxDBTreeList *TreeList;
	TcxDBTreeListColumn *tlcParentOrderID;
	TcxDBTreeListColumn *tlcOrderID;
	TcxDBTreeListColumn *tlcProductName;
	TcxDBTreeListColumn *tlcUnitPrice;
	TcxDBTreeListColumn *tlcQuantity;
	TcxDBTreeListColumn *tlcDiscount;
	TcxDBTreeListColumn *tlcAmountDiscount;
	TcxDBTreeListColumn *tlcTotal;
	TcxGroupBox *cxGroupBox2;
	TcxButton *btnShowExpressionEditor;
	TdxGalleryControl *galColumns;
	TdxGalleryControlGroup *dxGalleryControl1Group1;
	TdxGalleryControlItem *dxGalleryControl1Group1Item1;
	TdxGalleryControlItem *dxGalleryControl1Group1Item2;
	TcxLabel *cxLabel1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall mdOrderCalcFields(TDataSet *DataSet);
	void __fastcall btnShowExpressionEditorClick(TObject *Sender);
	void __fastcall TreeListEditing(TcxCustomTreeList *Sender, TcxTreeListColumn *AColumn,
		  bool &Allow);
	void __fastcall TreeListStylesGetContentStyle(TcxCustomTreeList *Sender, TcxTreeListColumn *AColumn,
          TcxTreeListNode *ANode, TcxStyle *&AStyle);
	void __fastcall TreeListSummary(TcxCustomTreeList *ASender, const TcxTreeListSummaryEventArguments &Arguments,
		  TcxTreeListSummaryEventOutArguments &OutArguments);
	void __fastcall ValidateExpressionColumn(TcxTreeListColumn *Sender, TcxTreeListNode *ANode,
          const Variant &AValue, TcxEditValidateInfo *AData);

public:
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
