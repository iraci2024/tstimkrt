//---------------------------------------------------------------------------

#include <vcl.h>
#pragma hdrstop

#include "CustomAnnotationSettings.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma link "cxButtons"
#pragma link "cxClasses"
#pragma link "cxContainer"
#pragma link "cxControls"
#pragma link "cxDropDownEdit"
#pragma link "cxEdit"
#pragma link "cxGraphics"
#pragma link "cxLookAndFeelPainters"
#pragma link "cxLookAndFeels"
#pragma link "cxMaskEdit"
#pragma link "cxSpinEdit"
#pragma link "cxTextEdit"
#pragma link "dxColorDialog"
#pragma link "dxLayoutContainer"
#pragma link "dxLayoutControl"
#pragma link "dxLayoutControlAdapters"
#pragma link "dxLayoutcxEditAdapters"
#pragma link "dxLayoutLookAndFeels"
#pragma resource "*.dfm"
TfrmCustomAnnotationSettings *frmCustomAnnotationSettings;
extern String SCustomAnnotationStrs[3];
//---------------------------------------------------------------------------
__fastcall TfrmCustomAnnotationSettings::TfrmCustomAnnotationSettings(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TfrmCustomAnnotationSettings::AnnotationChanged()
{
  TdxCustomScrollbarAnnotation* AAnnotation = CurrentAnnotation;
  cbAlignment->ItemIndex = int(AAnnotation->Alignment);
  seMinHeight->EditValue = AAnnotation->MinHeight;
  seMaxHeight->EditValue = AAnnotation->MaxHeight;
  seWidth->EditValue = AAnnotation->Width;
  seOffset->EditValue = AAnnotation->Offset;
  pbColor->Refresh();
}
//---------------------------------------------------------------------------
TdxCustomScrollbarAnnotation* __fastcall TfrmCustomAnnotationSettings::GetCurrentAnnotation()
{
  return FCustomAnnotations->Items[cbAnnotationKind->ItemIndex];
}
//---------------------------------------------------------------------------
void __fastcall TfrmCustomAnnotationSettings::Initialize(TdxCustomScrollbarAnnotations *AAnnotations)
{
  FCustomAnnotations = AAnnotations;
  AutoSize = true;
  for (int i = 0; i < FCustomAnnotations->Count; i++)
	cbAnnotationKind->Properties->Items->Add(SCustomAnnotationStrs[i]);
  for (int i = saaNear; i <= saaClient; i++)
	cbAlignment->Properties->Items->Add(SAlignment[i]);
  cbAnnotationKind->ItemIndex = 0;
}
//---------------------------------------------------------------------------
void __fastcall TfrmCustomAnnotationSettings::cxButton2Click(TObject *Sender)
{
  Close();
}
//---------------------------------------------------------------------------
void __fastcall TfrmCustomAnnotationSettings::cbAnnotationKindPropertiesChange(TObject *Sender)

{
  AnnotationChanged();
}
//---------------------------------------------------------------------------
void __fastcall TfrmCustomAnnotationSettings::cxButton3Click(TObject *Sender)
{
  dxColorDialog1->Color = CurrentAnnotation->Color;
  if (dxColorDialog1->Execute())
  {
	CurrentAnnotation->Color = dxColorDialog1->Color;
	pbColor->Refresh();
  };
}
//---------------------------------------------------------------------------
void __fastcall TfrmCustomAnnotationSettings::pbColorPaint(TObject *Sender)
{
   cxDrawTransparencyCheckerboard(pbColor->Canvas->Handle, pbColor->ClientRect, 7);
   dxGPPaintCanvas()->BeginPaint(pbColor->Canvas->Handle, pbColor->ClientRect);
   try
	 {
	 dxGPPaintCanvas()->FillRectangle(pbColor->ClientRect, CurrentAnnotation->Color);
	 }
   __finally
   {
	 dxGPPaintCanvas()->EndPaint();
   };
}
//---------------------------------------------------------------------------
void __fastcall TfrmCustomAnnotationSettings::cbAlignmentPropertiesChange(TObject *Sender)

{
  CurrentAnnotation->Alignment = TdxScrollbarAnnotationAlignment(cbAlignment->ItemIndex);
}
//---------------------------------------------------------------------------
void __fastcall TfrmCustomAnnotationSettings::seMinHeightPropertiesEditValueChanged(TObject *Sender)

{
  CurrentAnnotation->MinHeight = seMinHeight->EditValue;
}
//---------------------------------------------------------------------------
void __fastcall TfrmCustomAnnotationSettings::seMaxHeightPropertiesEditValueChanged(TObject *Sender)

{
  CurrentAnnotation->MaxHeight = seMaxHeight->EditValue;
}
//---------------------------------------------------------------------------
void __fastcall TfrmCustomAnnotationSettings::seWidthPropertiesEditValueChanged(TObject *Sender)

{
  CurrentAnnotation->Width = seWidth->EditValue;
}
//---------------------------------------------------------------------------
void __fastcall TfrmCustomAnnotationSettings::seOffsetPropertiesEditValueChanged(TObject *Sender)

{
  CurrentAnnotation->Offset = seOffset->EditValue;
}
//---------------------------------------------------------------------------
