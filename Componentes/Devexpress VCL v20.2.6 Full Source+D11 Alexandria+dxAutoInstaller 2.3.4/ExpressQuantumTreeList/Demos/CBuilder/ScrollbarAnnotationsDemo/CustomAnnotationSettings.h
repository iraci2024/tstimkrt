//---------------------------------------------------------------------------

#ifndef CustomAnnotationSettingsH
#define CustomAnnotationSettingsH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtons.hpp"
#include "cxClasses.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxDropDownEdit.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxMaskEdit.hpp"
#include "cxSpinEdit.hpp"
#include "cxTextEdit.hpp"
#include "dxColorDialog.hpp"
#include "dxLayoutContainer.hpp"
#include "dxLayoutControl.hpp"
#include "dxLayoutControlAdapters.hpp"
#include "dxLayoutcxEditAdapters.hpp"
#include "dxLayoutLookAndFeels.hpp"
#include <ExtCtrls.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TfrmCustomAnnotationSettings : public TForm
{
__published:	// IDE-managed Components
	TcxEditStyleController *cxEditStyleController1;
	TdxLayoutLookAndFeelList *dxLayoutLookAndFeelList1;
	TdxLayoutCxLookAndFeel *dxLayoutCxLookAndFeel1;
	TdxColorDialog *dxColorDialog1;
	TdxLayoutControl *dxLayoutControl1;
	TcxButton *cxButton2;
	TcxSpinEdit *seMinHeight;
	TcxSpinEdit *seMaxHeight;
	TcxSpinEdit *seWidth;
	TcxSpinEdit *seOffset;
	TcxComboBox *cbAlignment;
	TPaintBox *pbColor;
	TcxComboBox *cbAnnotationKind;
	TdxLayoutGroup *dxLayoutControl1Group_Root;
	TdxLayoutItem *dxLayoutItem2;
	TdxLayoutItem *dxLayoutItem5;
	TdxLayoutItem *dxLayoutItem6;
	TdxLayoutItem *dxLayoutItem7;
	TdxLayoutItem *dxLayoutItem8;
	TdxLayoutItem *dxLayoutItem9;
	TdxLayoutItem *dxLayoutItem4;
	TdxLayoutItem *dxLayoutItem3;
	TdxLayoutGroup *dxLayoutGroup1;
	TdxLayoutGroup *dxLayoutGroup3;
	void __fastcall cxButton2Click(TObject *Sender);
	void __fastcall cbAnnotationKindPropertiesChange(TObject *Sender);
	void __fastcall cxButton3Click(TObject *Sender);
	void __fastcall pbColorPaint(TObject *Sender);
	void __fastcall cbAlignmentPropertiesChange(TObject *Sender);
	void __fastcall seMinHeightPropertiesEditValueChanged(TObject *Sender);
	void __fastcall seMaxHeightPropertiesEditValueChanged(TObject *Sender);
	void __fastcall seWidthPropertiesEditValueChanged(TObject *Sender);
	void __fastcall seOffsetPropertiesEditValueChanged(TObject *Sender);
private:	// User declarations
	TdxCustomScrollbarAnnotations* FCustomAnnotations;
	void __fastcall AnnotationChanged();
	TdxCustomScrollbarAnnotation* __fastcall GetCurrentAnnotation();
public:		// User declarations
	__fastcall TfrmCustomAnnotationSettings(TComponent* Owner);
	void __fastcall Initialize(TdxCustomScrollbarAnnotations *AAnnotations);
	__property TdxCustomScrollbarAnnotation* CurrentAnnotation = {read=GetCurrentAnnotation};
};


const String SAlignment[4] = {"saaNear", "saaCenter", "saaFar", "saaClient"};
//---------------------------------------------------------------------------
extern PACKAGE TfrmCustomAnnotationSettings *frmCustomAnnotationSettings;
//---------------------------------------------------------------------------
#endif
