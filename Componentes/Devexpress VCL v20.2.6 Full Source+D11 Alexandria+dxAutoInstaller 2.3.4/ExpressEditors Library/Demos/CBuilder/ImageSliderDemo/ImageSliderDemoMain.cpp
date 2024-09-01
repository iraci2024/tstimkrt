//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "ImageSliderDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "BaseForm"
#pragma link "cxControls"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "dxImageSlider"
#pragma link "cxClasses"
#pragma link "dxmdaset"
#pragma resource "*.dfm"
TfrmImageSlider *frmImageSlider;
//---------------------------------------------------------------------------
__fastcall TfrmImageSlider::TfrmImageSlider(TComponent* Owner)
	: TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------

void TfrmImageSlider::PopulateImages()
{
  TdxSmartImage *AGraphic = new TdxSmartImage();
  try {
	mdsCarOrders->First();
	imgCollection->Items->BeginUpdate();
	try {
	  while (!mdsCarOrders->Eof)
	  {
		AGraphic->LoadFromFieldValue(mdsCarOrders->FieldByName("Photo")->Value);
		imgCollection->Items->Add()->Picture->Graphic = AGraphic;
		mdsCarOrders->Next();
	  }
	}
	__finally {
	  imgCollection->Items->EndUpdate();
	}
  }
  __finally {
	delete AGraphic;
  }
}


void __fastcall TfrmImageSlider::ScrollModeClick(TObject *Sender)
{
  ImageSlider->ScrollMode = TdxImageSliderScrollMode(((TMenuItem*)Sender)->Tag);
}
//---------------------------------------------------------------------------

void __fastcall TfrmImageSlider::PreviewPositionClick(TObject *Sender)
{
  ImageSlider->PreviewOptions->Position = (TcxPosition)(((TMenuItem*)Sender)->Tag);
}
//---------------------------------------------------------------------------

void __fastcall TfrmImageSlider::StretchModeClick(TObject *Sender)
{
  ImageSlider->ImageFitMode = (TcxImageFitMode)(((TMenuItem*)Sender)->Tag);
}
//---------------------------------------------------------------------------


void __fastcall TfrmImageSlider::TrasitionEffectClick(TObject *Sender)
{
  ImageSlider->TransitionEffect = (TdxImageSliderTransitionEffect)(((TMenuItem*)Sender)->Tag);
}
//---------------------------------------------------------------------------

void __fastcall TfrmImageSlider::Add1Click(TObject *Sender)
{
  if (!OpenPictureDialog1->Execute())
	return;

  TdxSmartImage *AGraphic = new TdxSmartImage();
  try {
	AGraphic->LoadFromFile(OpenPictureDialog1->FileName);
	imgCollection->AddFromMultiFrameImage(AGraphic);
  }
  __finally {
	delete AGraphic;
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmImageSlider::Savetofile1Click(TObject *Sender)
{
  if (!SavePictureDialog1->Execute())
	return;

  String AName = SavePictureDialog1->FileName;
  if (ExtractFileExt(AName) == "")
  {
	TStringList *AExtensions = new TStringList();
	try {
	  AExtensions->LineBreak = "|";
	  AExtensions->Text = SavePictureDialog1->Filter;
	  String AExt = AExtensions->Strings[SavePictureDialog1->FilterIndex * 2 - 1];
	  AExt = AExt.SubString(2, AExt.Length());
	  AName = AName + AExt;
  }
  __finally {
	  delete AExtensions;
	}
  }

  if (SameText(ExtractFileExt(AName), "->TIFF") || SameText(ExtractFileExt(AName), "->TIF") && (ImageSlider->Images->Count > 1))
  {
	TdxSmartImage *AImage = ImageSlider->Images->GetAsMultiFrameTIFF();
	try {
	  AImage->SaveToFile(AName);
	}
  __finally {
	  delete AImage;
	}
  }
  else
  {
	TdxSmartImage *AImage = new TdxSmartImage();
	try {
	  TcxImageCollectionItem *AItem = ImageSlider->Images->Items->Items[ImageSlider->ItemIndex];
	  AImage->Assign(AItem->Picture->Graphic);
	  AImage->SaveToFile(AName);
	}
  __finally {
	  delete AImage;
    }
  }
}
//---------------------------------------------------------------------------

void __fastcall TfrmImageSlider::FormCreate(TObject *Sender)
{
  PopulateImages();
}
//---------------------------------------------------------------------------

