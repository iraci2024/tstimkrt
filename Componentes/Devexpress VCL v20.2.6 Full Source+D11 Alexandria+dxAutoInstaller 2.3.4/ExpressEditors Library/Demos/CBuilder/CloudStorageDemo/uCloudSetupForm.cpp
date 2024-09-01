//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "uCloudSetupForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtons"
#pragma link "cxClasses"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxMemo"
#pragma link "cxRichEdit"
#pragma link "cxTextEdit"
#pragma link "dxLayoutContainer"
#pragma link "dxLayoutControl"
#pragma link "dxLayoutControlAdapters"
#pragma link "dxLayoutcxEditAdapters"
#pragma link "dxLayoutLookAndFeels"
#pragma resource "*.dfm"
TfmCloudSetupWizard *fmCloudSetupWizard;
//---------------------------------------------------------------------------
__fastcall TfmCloudSetupWizard::TfmCloudSetupWizard(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfmCloudSetupWizard::teChange(TObject *Sender)
{
	UpdateStateButtons();
}
//---------------------------------------------------------------------------
void __fastcall TfmCloudSetupWizard::reURLClick(TcxCustomRichEdit *Sender, const UnicodeString URLText,
		  TMouseButton Button)
{
  	dxShellExecute(Sender->Handle, URLText);
}
//---------------------------------------------------------------------------
void __fastcall TfmCloudSetupWizard::FormCreate(TObject *Sender)
{
	reAbout->Lines->LoadFromFile(ExtractFilePath(Application->ExeName) + "Wizard.rtf");
	reMSGraph->Lines->LoadFromFile(ExtractFilePath(Application->ExeName) + "Wizard-MSGraph.rtf");
	reGoogleApi->Lines->LoadFromFile(ExtractFilePath(Application->ExeName) + "Wizard-GoogleAPI.rtf");
}
//---------------------------------------------------------------------------
void TfmCloudSetupWizard::UpdateStateButtons()
{
	btnStart->Enabled = ((Trim(teMSGraphClientID->Text) != "") && (Trim(teMSGraphClientSecret->Text) != "")) ||
		((Trim(teGoogleApiClientID->Text) != "") && (Trim(teGoogleApiClientSecret->Text) != ""));
}
//---------------------------------------------------------------------------

