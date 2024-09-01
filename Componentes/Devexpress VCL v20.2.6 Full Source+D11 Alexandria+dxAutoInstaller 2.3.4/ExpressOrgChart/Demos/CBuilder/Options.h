//---------------------------------------------------------------------------
#ifndef OptionsH
#define OptionsH
//---------------------------------------------------------------------------
#include <classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include <ExtCtrls.hpp>
#include <Buttons.hpp>
#include <Mask.hpp>
#include "main.h"
#include "cxControls.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include <Menus.hpp>
#include "cxContainer.hpp"
#include "cxEdit.hpp"
#include "cxTextEdit.hpp" 
#include "cxMaskEdit.hpp"
#include "cxSpinEdit.hpp"
#include "cxCheckBox.hpp" 
#include "cxGroupBox.hpp"
#include "cxLabel.hpp"
#include "dxBevel.hpp"
#include "cxButtons.hpp"
//---------------------------------------------------------------------------
class TOptionsForm : public TForm
{
__published:	// IDE-managed Components
	TdxBevel *Bevel1;
	TcxButton *BitBtn2;
	TcxButton *BitBtn1;
	TcxCheckBox *cbShowImages;
	TcxCheckBox *cbInsDel;
	TcxCheckBox *cbShowDrag;
	TcxCheckBox *cbCanDrag;
	TcxCheckBox *cbSelect;
	TcxCheckBox *cbFocus;
	TcxCheckBox *cbButtons;
	TcxCheckBox *cbEdit;
	TcxGroupBox *GroupBox1;
	TcxCheckBox *cbLeft;
	TcxCheckBox *cbCenter;
	TcxCheckBox *cbRight;
	TcxCheckBox *cbVCenter;
	TcxCheckBox *cbWrap;
	TcxCheckBox *cbUpper;
	TcxCheckBox *cbLower;
	TcxCheckBox *cbGrow;
	TcxLabel *Label3;
	TcxLabel *Label2;
	TcxLabel *Label1;
	TcxSpinEdit *seX;
	TcxSpinEdit *seY;
	TcxSpinEdit *seLineWidth;
	void __fastcall FormActivate(TObject *Sender);
	void __fastcall BitBtn2Click(TObject *Sender);
private:	// User declarations
public:		// User declarations
	__fastcall TOptionsForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern TOptionsForm *OptionsForm;
//---------------------------------------------------------------------------
#endif
