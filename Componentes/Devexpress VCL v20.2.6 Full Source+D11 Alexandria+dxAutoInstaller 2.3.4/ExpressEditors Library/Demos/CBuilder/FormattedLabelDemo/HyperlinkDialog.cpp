//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "HyperlinkDialog.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtonEdit"
#pragma link "cxButtons"
#pragma link "cxClasses"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxMaskEdit"
#pragma link "cxTextEdit"
#pragma link "dxLayoutContainer"
#pragma link "dxLayoutControl"
#pragma link "dxLayoutControlAdapters"
#pragma link "dxLayoutcxEditAdapters"
#pragma resource "*.dfm"
TfmHyperlinkDialog *fmHyperlinkDialog;
//---------------------------------------------------------------------------
__fastcall TfmHyperlinkDialog::TfmHyperlinkDialog(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
