//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "FluentDesignDemoMainForm.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "dxCore"
#pragma link "cxControls"
#pragma link "cxGraphics"
#pragma link "cxClasses"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "dxSkinsCore"
#pragma link "cxContainer"
#pragma link "cxEdit"
#pragma link "dxNavBar"
#pragma link "dxLayoutLookAndFeels"
#pragma link "dxLayoutContainer"
#pragma link "dxLayoutControl"
#pragma link "dxSkinsForm"
#pragma link "dxSkinsFluentDesignForm"
#pragma link "dxSkinOffice2019Colorful"
#pragma link "dxNavBarBase"
#pragma link "dxNavBarCollns"
#pragma link "cxImageList"
#pragma resource "*.dfm"
#pragma resource "dxDPIAwareManifest.res"
TfrmFluentDesignDemo *frmFluentDesignDemo;
//---------------------------------------------------------------------------
__fastcall TfrmFluentDesignDemo::TfrmFluentDesignDemo(TComponent* Owner)
	: TdxFluentDesignForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmFluentDesignDemo::dxLayoutCheckBoxItem1Click(TObject *Sender)
{
  EnableAcrylicEffects = dxLayoutCheckBoxItem1->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfrmFluentDesignDemo::dxLayoutCheckBoxItem2Click(TObject *Sender)
{
  ExtendNavigationControlToCaption = dxLayoutCheckBoxItem2->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfrmFluentDesignDemo::dxLayoutCheckBoxItem3Click(TObject *Sender)
{
  dxNavBar1->OptionsBehavior->Common->RevealHighlight = dxLayoutCheckBoxItem3->Checked;
}
//---------------------------------------------------------------------------


void __fastcall TfrmFluentDesignDemo::dxLayoutRadioButtonItem5Click(TObject *Sender)
{
  dxNavBar1->OptionsView->HamburgerMenu->NavigationPaneMode = TdxNavBarHamburgerMenuNavigationPaneMode(((TComponent*)Sender)->Tag);
}
//---------------------------------------------------------------------------

void __fastcall TfrmFluentDesignDemo::dxNavBarItem4Click(TObject *Sender)
{
  //
}
//---------------------------------------------------------------------------
void __fastcall TfrmFluentDesignDemo::dxLayoutRadioButtonItem2Click(TObject *Sender)
{
  if (dxLayoutRadioButtonItem1->Checked)
	dxNavBar1->View = dxNavBarAccordionView;
  else
	dxNavBar1->View = dxNavBarHamburgerMenu;
  dxLayoutGroup4->Enabled = dxNavBar1->View == dxNavBarHamburgerMenu;
}
//---------------------------------------------------------------------------
void __fastcall TfrmFluentDesignDemo::nbgContactSelectedLinkChanged(TObject *Sender)
{
  ((TdxNavBarGroup*)Sender)->SelectedLinkIndex = -1;
}
//---------------------------------------------------------------------------
void __fastcall TfrmFluentDesignDemo::dxFluentDesignFormCreate(TObject *Sender)
{
  dxSkinController1->NativeStyle = false;
  if (!IsWinSupportsAcrylicEffect) {
    ShowMessage("Acrylic Material and Reveal Highlight effects are available on systems with Windows 10 Fall Creators Update (OS Build 17064 or later).");
  }
}
//---------------------------------------------------------------------------

