//---------------------------------------------------------------------------

#ifndef FormattedLabelDemoMainH
#define FormattedLabelDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BaseForm.h"
#include <Menus.hpp>
#include "cxClasses.hpp"
#include "cxColorComboBox.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxDropDownEdit.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxImageList.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxMaskEdit.hpp"
#include "cxMemo.hpp"
#include "cxRichEdit.hpp"
#include "cxTextEdit.hpp"
#include "dxColorDialog.hpp"
#include "dxFormattedLabel.hpp"
#include "dxLayoutContainer.hpp"
#include "dxLayoutControl.hpp"
#include "dxLayoutcxEditAdapters.hpp"
#include "dxLayoutLookAndFeels.hpp"
#include "dxCoreGraphics.hpp"
#include "HyperlinkDialog.h"
#include <ActnList.hpp>
#include <ComCtrls.hpp>
#include <Dialogs.hpp>
#include <ImgList.hpp>
#include <ToolWin.hpp>
//---------------------------------------------------------------------------
class TdxFormattedLabelDemoForm : public TfmBaseForm
{
__published:	// IDE-managed Components
	TdxLayoutControl *dxLayoutControl1;
	TToolBar *ToolBar1;
	TToolButton *tbBold;
	TToolButton *tbItalic;
	TToolButton *tbUnderline;
	TToolButton *tbStrikeOut;
	TToolButton *tbSup;
	TToolButton *tbSub;
	TToolButton *ToolButton4;
	TToolButton *tbFont;
	TToolButton *tbFontColor;
	TToolButton *tbFill;
	TToolButton *ToolButton8;
	TToolButton *tbHyperlink;
	TToolButton *ToolButton5;
	TToolButton *tbNoparse;
	TcxRichEdit *reBBCode;
	TdxFormattedLabel *flMain;
	TcxColorComboBox *cbHyperlinkColor;
	TdxLayoutGroup *dxLayoutControl1Group_Root;
	TdxLayoutGroup *dxLayoutGroup1;
	TdxLayoutGroup *dxLayoutGroup2;
	TdxLayoutGroup *dxLayoutGroup3;
	TdxLayoutGroup *dxLayoutGroup4;
	TdxLayoutItem *dxLayoutItem1;
	TdxLayoutSplitterItem *dxLayoutSplitterItem1;
	TdxLayoutGroup *dxLayoutGroup5;
	TdxLayoutItem *dxLayoutItem2;
	TdxLayoutGroup *dxLayoutGroup6;
	TdxLayoutItem *dxLayoutItem3;
	TdxLayoutGroup *dxLayoutGroup7;
	TdxLayoutItem *dxLayoutItem4;
	TdxLayoutCheckBoxItem *cbWordWrap;
	TdxLayoutCheckBoxItem *cbShowEndEllipsis;
	TdxLayoutGroup *dxLayoutGroup8;
	TdxLayoutGroup *lgAlignmentHorizontal;
	TdxLayoutGroup *lgAlignmentVertical;
	TdxLayoutRadioButtonItem *lrbLeft;
	TdxLayoutRadioButtonItem *lrbRight;
	TdxLayoutRadioButtonItem *lrbCenterH;
	TdxLayoutRadioButtonItem *lrbTop;
	TdxLayoutRadioButtonItem *lrbBottom;
	TdxLayoutRadioButtonItem *lrbCenter;
	TcxImageList *cxImageList1;
	TFontDialog *FontDialog1;
	TdxColorDialog *dxColorDialog1;
	TActionList *ActionList1;
	TAction *acBold;
	TAction *acItalic;
	TAction *acUnderline;
	TAction *acStrikeout;
	TAction *acFont;
	TAction *acFontColor;
	TAction *acBackgroundColor;
	TAction *acHyperlink;
	TAction *acNoparse;
	TAction *acSup;
	TAction *acSub;
	TdxLayoutLookAndFeelList *dxLayoutLookAndFeelList1;
	TdxLayoutCxLookAndFeel *dxLayoutCxLookAndFeel1;
	void __fastcall acBoldExecute(TObject *Sender);
	void __fastcall acItalicExecute(TObject *Sender);
	void __fastcall acUnderlineExecute(TObject *Sender);
	void __fastcall acStrikeoutExecute(TObject *Sender);
	void __fastcall acFontExecute(TObject *Sender);
	void __fastcall acFontColorExecute(TObject *Sender);
	void __fastcall acBackgroundColorExecute(TObject *Sender);
	void __fastcall acHyperlinkExecute(TObject *Sender);
	void __fastcall acNoparseExecute(TObject *Sender);
	void __fastcall acSupExecute(TObject *Sender);
	void __fastcall acSubExecute(TObject *Sender);
	void __fastcall cbHyperlinkColorPropertiesEditValueChanged(TObject *Sender);
	void __fastcall cbWordWrapClick(TObject *Sender);
	void __fastcall cbShowEndEllipsisClick(TObject *Sender);
	void __fastcall lrbLeftClick(TObject *Sender);
	void __fastcall lrbTopClick(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall reBBCodePropertiesChange(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TdxFormattedLabelDemoForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TdxFormattedLabelDemoForm *dxFormattedLabelDemoForm;
//---------------------------------------------------------------------------
#endif
