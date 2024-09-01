//---------------------------------------------------------------------------

#ifndef ImageSliderDemoMainH
#define ImageSliderDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "BaseForm.h"
#include <Menus.hpp>
#include "cxControls.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "dxImageSlider.hpp"
#include "cxClasses.hpp"
#include "dxmdaset.hpp"
#include <DB.hpp>
#include <Dialogs.hpp>
#include <ExtDlgs.hpp>
//---------------------------------------------------------------------------
class TfrmImageSlider : public TfmBaseForm
{
__published:	// IDE-managed Components
	TMenuItem *Add1;
	TMenuItem *N1;
	TMenuItem *ScrollMode1;
	TMenuItem *N2;
	TMenuItem *StretchMode1;
	TMenuItem *None1;
	TMenuItem *Left1;
	TMenuItem *Right1;
	TMenuItem *op1;
	TMenuItem *Bottom1;
	TMenuItem *Normal1;
	TMenuItem *Fill1;
	TMenuItem *ProportionalStretch1;
	TMenuItem *Stretch1;
	TMenuItem *None2;
	TMenuItem *SegmentedFade1;
	TdxImageSlider *ImageSlider;
	TdxMemData *mdsCarOrders;
	TIntegerField *mdsCarOrdersID;
	TStringField *mdsCarOrdersTrademark;
	TStringField *mdsCarOrdersModel;
	TStringField *mdsCarOrdersTrademark_Site;
	TBlobField *mdsCarOrdersPhoto;
	TCurrencyField *mdsCarOrdersPrice;
	TcxImageCollection *imgCollection;
	TSavePictureDialog *SavePictureDialog1;
	TOpenPictureDialog *OpenPictureDialog1;
	void __fastcall ScrollModeClick(TObject *Sender);
	void __fastcall PreviewPositionClick(TObject *Sender);
	void __fastcall StretchModeClick(TObject *Sender);
	void __fastcall TrasitionEffectClick(TObject *Sender);
	void __fastcall Add1Click(TObject *Sender);
	void __fastcall Savetofile1Click(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TfrmImageSlider(TComponent* Owner);
	void PopulateImages();
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmImageSlider *frmImageSlider;
//---------------------------------------------------------------------------
#endif
