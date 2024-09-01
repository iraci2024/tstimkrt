//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxControls"
#pragma link "cxClasses"
#pragma link "cxLookAndFeels"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxEdit"
#pragma link "cxMemo"
#pragma link "cxRichEdit"
#pragma link "cxTextEdit"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma resource "*.dfm"
TformAboutDemo *FForm;

//---------------------------------------------------------------------------

void ShowAboutDemoForm()
{
  if (FForm == NULL)
	FForm = new TformAboutDemo(Application);
  FForm->Show();
}
//---------------------------------------------------------------------------
__fastcall TformAboutDemo::TformAboutDemo(TComponent* Owner)
        : TForm(Owner)
{
  AssignBounds();
  redDescription->Lines->LoadFromFile(ExtractFilePath(Application->ExeName) + "About.txt");
}
//---------------------------------------------------------------------------
void __fastcall TformAboutDemo::AssignBounds()
{
  Left = Application->MainForm->BoundsRect.Right;
  Top = Application->MainForm->BoundsRect.Top;
  Height = Application->MainForm->Height;
  TRect ADesktopArea = GetDesktopWorkArea(Point(Left, Top));
  if (BoundsRect.Right > ADesktopArea.Right){
    int AOffset = BoundsRect.Right - ADesktopArea.Right;
    Left = Left - AOffset;
    if (Application->MainForm->Left > AOffset)
      Application->MainForm->Left = Application->MainForm->Left - AOffset;
    else
      Application->MainForm->Left = 0;
  }
}
