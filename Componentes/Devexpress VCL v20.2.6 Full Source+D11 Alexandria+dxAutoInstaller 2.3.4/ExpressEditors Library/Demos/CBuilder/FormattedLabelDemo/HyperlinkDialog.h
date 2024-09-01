//---------------------------------------------------------------------------

#ifndef HyperlinkDialogH
#define HyperlinkDialogH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxButtonEdit.hpp"
#include "cxButtons.hpp"
#include "cxClasses.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxMaskEdit.hpp"
#include "cxTextEdit.hpp"
#include "dxLayoutContainer.hpp"
#include "dxLayoutControl.hpp"
#include "dxLayoutControlAdapters.hpp"
#include "dxLayoutcxEditAdapters.hpp"
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TfmHyperlinkDialog : public TForm
{
__published:	// IDE-managed Components
	TdxLayoutControl *dxLayoutControl1;
	TcxButton *btnOk;
	TcxButton *btnCancel;
	TcxTextEdit *edtTextToDisplay;
	TcxButtonEdit *edtAddress;
	TdxLayoutGroup *dxLayoutControl1Group_Root;
	TdxLayoutGroup *dxLayoutGroup1;
	TdxLayoutGroup *dxLayoutGroup2;
	TdxLayoutGroup *dxLayoutGroup3;
	TdxLayoutItem *dxLayoutItem1;
	TdxLayoutItem *dxLayoutItem2;
	TdxLayoutGroup *dxLayoutGroup4;
	TdxLayoutItem *dxLayoutItem3;
	TdxLayoutItem *dxLayoutItem4;
private:	// User declarations
public:		// User declarations
	__fastcall TfmHyperlinkDialog(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfmHyperlinkDialog *fmHyperlinkDialog;
//---------------------------------------------------------------------------
#endif
