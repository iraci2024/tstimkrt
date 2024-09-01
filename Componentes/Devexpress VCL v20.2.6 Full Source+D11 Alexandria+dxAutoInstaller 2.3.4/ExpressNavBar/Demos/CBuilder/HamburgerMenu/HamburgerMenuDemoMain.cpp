//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "HamburgerMenuDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxControls"
#pragma link "cxGraphics"
#pragma link "cxImageList"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "dxGDIPlusClasses"
#pragma link "dxLayoutContainer"
#pragma link "dxLayoutControl"
#pragma link "dxNavBar"
#pragma link "dxNavBarBase"
#pragma link "dxNavBarCollns"
#pragma link "dxLayoutLookAndFeels"
#pragma resource "*.dfm"
TfrmHamburgerMenuDemo *frmHamburgerMenuDemo;
//---------------------------------------------------------------------------
__fastcall TfrmHamburgerMenuDemo::TfrmHamburgerMenuDemo(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
int TfrmHamburgerMenuDemo::GetLayoutRadioButtonTag(TObject *Sender)
{
  TdxLayoutRadioButtonItem *rgButton;
  rgButton = (TdxLayoutRadioButtonItem*)Sender;
  return rgButton->Tag;
}
//---------------------------------------------------------------------------
void TfrmHamburgerMenuDemo::ShowAboutDemoForm()
{

}
//---------------------------------------------------------------------------
void TfrmHamburgerMenuDemo::UpdateImages()
{
	if (lrgImagesSizeSmall->Checked)
	{
	  dxNavBar1->OptionsImage->SmallImages = ilSmall;
	}
	else
	  if (lrgImagesSizeLarge->Checked)
	  {
		dxNavBar1->OptionsImage->SmallImages = ilLarge;
	  }
	  else
		if (lrgImagesSizeMedium->Checked)
		{
		  dxNavBar1->OptionsImage->SmallImages = ilMedium;
		}
		else
		{
		  dxNavBar1->OptionsImage->SmallImages = NULL;
		}
}
//---------------------------------------------------------------------------
void __fastcall TfrmHamburgerMenuDemo::FormCreate(TObject *Sender)
{
  lcbAllowAnimation->Checked = True;
  lrgDisplayModeInline->Checked = True;
  lrgSelectedPopupModeDocked->Checked = True;
  lrgNavigationPaneModeCompact->Checked = True;
  lrgImagesSizeSmall->Checked = True;
}
//---------------------------------------------------------------------------

void __fastcall TfrmHamburgerMenuDemo::lcbAllowAnimationClick(TObject *Sender)
{
  dxNavBar1->OptionsBehavior->Common->AllowExpandAnimation = lcbAllowAnimation->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TfrmHamburgerMenuDemo::lrgDisplayModeInlineClick(TObject *Sender)
{
  TdxNavBarHamburgerMenuDisplayMode ADisplayMode = TdxNavBarHamburgerMenuDisplayMode(GetLayoutRadioButtonTag(Sender));
  if (ADisplayMode == dmOverlayMinimal)
  {
	dxNavBar1->OptionsBehavior->HamburgerMenu->DisplayMode = ADisplayMode;
	dxNavBar1->Align = alTop;
  }
  else
  {
	dxNavBar1->Align = alLeft;
	dxNavBar1->OptionsBehavior->HamburgerMenu->DisplayMode = ADisplayMode;
  }
  if (dxNavBar1->OptionsBehavior->HamburgerMenu->DisplayMode == dmInline)
  {
	dxNavBar1->OptionsBehavior->HamburgerMenu->Collapsed = False;
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmHamburgerMenuDemo::lrgImagesTypeBitmapClick(TObject *Sender)
{
  UpdateImages();
}
//---------------------------------------------------------------------------

void __fastcall TfrmHamburgerMenuDemo::lrgImagesSizeSmallClick(TObject *Sender)
{
  UpdateImages();
}
//---------------------------------------------------------------------------


void __fastcall TfrmHamburgerMenuDemo::dxNavBar1Item11Click(TObject *Sender)
{
  ShowAboutDemoForm();
}
//---------------------------------------------------------------------------

void __fastcall TfrmHamburgerMenuDemo::dxNavBar1OnCustomDrawLinkSelection(TObject *Sender,
          TCanvas *ACanvas, TdxNavBarLinkViewInfo *AViewInfo, bool &AHandled)
{
  TRect R;
  TdxNavBarObjectStates AStates;
  TdxNavBarHamburgerMenuPainter *APainter = (TdxNavBarHamburgerMenuPainter*)AViewInfo->ViewInfo->Painter;
  AStates = AViewInfo->State;
  AHandled = AStates.Contains(sSelected);
  if (AHandled)
  {
	AStates >> sSelected;
	R = AViewInfo->SelectionRect();
	APainter->DrawItemState(AViewInfo, R, AStates);
	R = AViewInfo->Rect;
	R.Right = R.Left + 3;
	ACanvas->Brush->Color = clWhite;
	ACanvas->FillRect(R);
  };
}
//---------------------------------------------------------------------------

void __fastcall TfrmHamburgerMenuDemo::lrgNavigationPaneModeCompactClick(TObject *Sender)

{
  dxNavBar1->OptionsView->HamburgerMenu->NavigationPaneMode =
	TdxNavBarHamburgerMenuNavigationPaneMode(GetLayoutRadioButtonTag(Sender));
}
//---------------------------------------------------------------------------

void __fastcall TfrmHamburgerMenuDemo::lrgSelectedPopupModeDockedClick(TObject *Sender)

{
  dxNavBar1->OptionsBehavior->HamburgerMenu->SelectedGroupPopupMode =
	TdxNavBarHamburgerMenuSelectedGroupPopupMode(GetLayoutRadioButtonTag(Sender));
}
//---------------------------------------------------------------------------

void __fastcall TfrmHamburgerMenuDemo::dxNavBar1LinkClick(TObject *Sender, TdxNavBarItemLink *ALink)

{
  ShowMessage("OnLinkCkick: " + ALink->Item->Caption);
}
//---------------------------------------------------------------------------

void __fastcall TfrmHamburgerMenuDemo::nbgContactSelectedLinkChanged(TObject *Sender)

{
   nbgContact ->SelectedLinkIndex = -1;
}
//---------------------------------------------------------------------------

void __fastcall TfrmHamburgerMenuDemo::nbgShedulerSelectedLinkChanged(TObject *Sender)

{
  nbgSheduler->SelectedLinkIndex = -1;
}
//---------------------------------------------------------------------------

void __fastcall TfrmHamburgerMenuDemo::nbgMailSelectedLinkChanged(TObject *Sender)

{
  nbgMail->SelectedLinkIndex = -1;
}
//---------------------------------------------------------------------------


void __fastcall TfrmHamburgerMenuDemo::dxNavBar1GetOverlaySize(TObject *Sender, int &AWidth, int &AHeight)

{
  AHeight = ClientHeight;
}
//---------------------------------------------------------------------------

