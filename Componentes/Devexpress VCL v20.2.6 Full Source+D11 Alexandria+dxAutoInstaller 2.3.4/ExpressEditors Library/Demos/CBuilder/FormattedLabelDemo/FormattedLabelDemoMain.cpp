//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "FormattedLabelDemoMain.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxClasses"
#pragma link "cxColorComboBox"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxDropDownEdit"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxImageList"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxMaskEdit"
#pragma link "cxMemo"
#pragma link "cxRichEdit"
#pragma link "cxTextEdit"
#pragma link "dxColorDialog"
#pragma link "dxFormattedLabel"
#pragma link "dxLayoutContainer"
#pragma link "dxLayoutControl"
#pragma link "dxLayoutcxEditAdapters"
#pragma link "dxLayoutLookAndFeels"
#pragma link "dxCoreGraphics"
#pragma link "HyperlinkDialog"
#pragma resource "WindowsXP.res"
#pragma resource "*.dfm"
TdxFormattedLabelDemoForm *dxFormattedLabelDemoForm;
//---------------------------------------------------------------------------
__fastcall TdxFormattedLabelDemoForm::TdxFormattedLabelDemoForm(TComponent* Owner)
	: TfmBaseForm(Owner)
{
}
//---------------------------------------------------------------------------

void __fastcall TdxFormattedLabelDemoForm::acBoldExecute(TObject *Sender)
{
  const String FormatText = "[B]%s[/B]";
  TVarRec V[] = {reBBCode->SelText};
  reBBCode->SelText = Format(FormatText, V, 1);
  reBBCode->SelStart = reBBCode->SelStart - 4;
}
//---------------------------------------------------------------------------

void __fastcall TdxFormattedLabelDemoForm::acItalicExecute(TObject *Sender)
{
  const String FormatText = "[I]%s[/I]";
  TVarRec V[] = {reBBCode->SelText};
  reBBCode->SelText = Format(FormatText, V, 1);
  reBBCode->SelStart = reBBCode->SelStart - 4;
}
//---------------------------------------------------------------------------

void __fastcall TdxFormattedLabelDemoForm::acUnderlineExecute(TObject *Sender)
{
  const String FormatText = "[U]%s[/U]";
  TVarRec V[] = {reBBCode->SelText};
  reBBCode->SelText = Format(FormatText, V, 1);
  reBBCode->SelStart = reBBCode->SelStart - 4;
}
//---------------------------------------------------------------------------

void __fastcall TdxFormattedLabelDemoForm::acStrikeoutExecute(TObject *Sender)
{
  const String FormatText = "[S]%s[/S]";
  TVarRec V[] = {reBBCode->SelText};
  reBBCode->SelText = Format(FormatText, V, 1);
  reBBCode->SelStart = reBBCode->SelStart - 4;
}
//---------------------------------------------------------------------------

void __fastcall TdxFormattedLabelDemoForm::acFontExecute(TObject *Sender)
{
  if (FontDialog1->Execute(reBBCode->Handle))
  {
	int ASelStartIndent = 0;
	String ASelectedText = reBBCode->SelText;
	if (FontDialog1->Font->Name != reBBCode->Style->Font->Name)
	{
	  const String FormatText = "[FONT=%s]%s[/FONT]";
	  TVarRec V[] = {FontDialog1->Font->Name, ASelectedText};
	  ASelectedText = Format(FormatText, V, 2);
	  ASelStartIndent = ASelStartIndent + 7;
	}
	if (FontDialog1->Font->Size != reBBCode->Style->Font->Size)
	{
	  const String FormatText = "[SIZE=%s]%s[/SIZE]";
	  TVarRec V[] = {IntToStr(FontDialog1->Font->Size),  ASelectedText};
	  ASelectedText = Format(FormatText, V, 2);
	  ASelStartIndent = ASelStartIndent + 7;
	}
	if (FontDialog1->Font->Color != reBBCode->Style->Font->Color)
	{
	  const String FormatText = "[COLOR=#%->2x%->2x%->2x]%s[/COLOR]";
	  TVarRec V[] = {dxColorToRGBQuad(FontDialog1->Font->Color).rgbRed,
		dxColorToRGBQuad(FontDialog1->Font->Color).rgbGreen, dxColorToRGBQuad(FontDialog1->Font->Color).rgbBlue,  ASelectedText};
	  ASelectedText = Format(FormatText, V, 4);
	  ASelStartIndent = ASelStartIndent + 8;
	}
	if (FontDialog1->Font->Style.Contains(fsBold))
	{
	  const String FormatText = "[B]%s[/B]";
	  TVarRec V[] = {ASelectedText};
	  ASelectedText = Format(FormatText, V, 1);
	  ASelStartIndent = ASelStartIndent + 4;
	}
	if (FontDialog1->Font->Style.Contains(fsItalic))
	{
	  const String FormatText = "[I]%s[/I]";
	  TVarRec V[] = {ASelectedText};
	  ASelectedText = Format(FormatText, V, 1);
	  ASelStartIndent = ASelStartIndent + 4;
	}
	if (FontDialog1->Font->Style.Contains(fsUnderline))
	{
	  const String FormatText = "[U]%s[/U]";
	  TVarRec V[] = {ASelectedText};
	  ASelectedText = Format(FormatText, V, 1);
	  ASelStartIndent = ASelStartIndent + 4;
	}
	if (FontDialog1->Font->Style.Contains(fsStrikeOut))
	{
	  const String FormatText = "[S]%s[/S]";
	  TVarRec V[] = {ASelectedText};
	  ASelectedText = Format(FormatText, V, 1);
	  ASelStartIndent = ASelStartIndent + 4;
	}
	reBBCode->SelText = ASelectedText;
	reBBCode->SelStart = reBBCode->SelStart - ASelStartIndent;
  }
}
//---------------------------------------------------------------------------

void __fastcall TdxFormattedLabelDemoForm::acFontColorExecute(TObject *Sender)
{
  if (dxColorDialog1->Execute())
  {
	TdxAlphaColors *AlphaColors;
	const String FormatText = "[COLOR=#%s]%s[/COLOR]";
	TVarRec V[] = {AlphaColors->ToHexCode(dxColorDialog1->Color, False), reBBCode->SelText};
	reBBCode->SelText = Format(FormatText, V, 2);
	reBBCode->SelStart = reBBCode->SelStart - 8;
  }
}
//---------------------------------------------------------------------------

void __fastcall TdxFormattedLabelDemoForm::acBackgroundColorExecute(TObject *Sender)
{
  if (dxColorDialog1->Execute())
  {
	TdxAlphaColors *AlphaColors;
	const String FormatText = "[BACKCOLOR=#%s]%s[/BACKCOLOR]";
	TVarRec V[] = {AlphaColors->ToHexCode(dxColorDialog1->Color, False), reBBCode->SelText};
	reBBCode->SelText = Format(FormatText, V, 2);
	reBBCode->SelStart = reBBCode->SelStart - 12;
  }
}
//---------------------------------------------------------------------------

void __fastcall TdxFormattedLabelDemoForm::acHyperlinkExecute(TObject *Sender)
{
  TfmHyperlinkDialog *AHyperlinkDialog = new TfmHyperlinkDialog(NULL);
  try {
	AHyperlinkDialog->edtTextToDisplay->Text = reBBCode->SelText;
	if (AHyperlinkDialog->ShowModal() == mrOk)
	{
	  const String FormatText = "[URL=%s]%s[/URL]";
	  TVarRec V[] = {AHyperlinkDialog->edtAddress->Text, AHyperlinkDialog->edtTextToDisplay->Text};
	  reBBCode->SelText = Format(FormatText, V, 2);
	  reBBCode->SelStart = reBBCode->SelStart - 6;
	}
  }
  __finally {
	delete AHyperlinkDialog;
  }
}
//---------------------------------------------------------------------------

void __fastcall TdxFormattedLabelDemoForm::acNoparseExecute(TObject *Sender)
{
  const String FormatText = "[NOPARSE]%s[/NOPARSE]";
  TVarRec V[] = {reBBCode->SelText};
  reBBCode->SelText = Format(FormatText, V, 1);
  reBBCode->SelStart = reBBCode->SelStart - 10;
}
//---------------------------------------------------------------------------

void __fastcall TdxFormattedLabelDemoForm::acSupExecute(TObject *Sender)
{
  const String FormatText = "[SUP]%s[/SUP]";
  TVarRec V[] = {reBBCode->SelText};
  reBBCode->SelText = Format(FormatText, V, 1);
  reBBCode->SelStart = reBBCode->SelStart - 6;
}
//---------------------------------------------------------------------------

void __fastcall TdxFormattedLabelDemoForm::acSubExecute(TObject *Sender)
{
  const String FormatText = "[SUB]%s[/SUB]";
  TVarRec V[] = {reBBCode->SelText};
  reBBCode->SelText = Format(FormatText, V, 1);
  reBBCode->SelStart = reBBCode->SelStart - 6;
}
//---------------------------------------------------------------------------

void __fastcall TdxFormattedLabelDemoForm::cbHyperlinkColorPropertiesEditValueChanged(TObject *Sender)
{
  flMain->Properties->HyperlinkColor = cbHyperlinkColor->ColorValue;
}
//---------------------------------------------------------------------------

void __fastcall TdxFormattedLabelDemoForm::cbWordWrapClick(TObject *Sender)
{
  flMain->Properties->WordWrap = cbWordWrap->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TdxFormattedLabelDemoForm::cbShowEndEllipsisClick(TObject *Sender)
{
  flMain->Properties->ShowEndEllipsis = cbShowEndEllipsis->Checked;
}
//---------------------------------------------------------------------------

void __fastcall TdxFormattedLabelDemoForm::lrbLeftClick(TObject *Sender)
{
  flMain->Properties->Alignment->Horz = (TcxEditHorzAlignment)((TdxLayoutItem*)Sender)->Tag;
}
//---------------------------------------------------------------------------

void __fastcall TdxFormattedLabelDemoForm::lrbTopClick(TObject *Sender)
{
  flMain->Properties->Alignment->Vert = (TcxEditVertAlignment)((TdxLayoutItem*)Sender)->Tag;
}
//---------------------------------------------------------------------------

void __fastcall TdxFormattedLabelDemoForm::FormCreate(TObject *Sender)
{
  reBBCode->Text = "[B]Questions?[/B]\nVisit our [URL=https://www.devexpress.com/support/]Support Center[/URL]";
}
//---------------------------------------------------------------------------


void __fastcall TdxFormattedLabelDemoForm::reBBCodePropertiesChange(TObject *Sender)

{
  flMain->Caption = reBBCode->EditingText;
}
//---------------------------------------------------------------------------

