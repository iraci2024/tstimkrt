//---------------------------------------------------------------------------

#ifndef AboutDemoFormH
#define AboutDemoFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxEdit.hpp"
#include "cxMemo.hpp"
#include "cxRichEdit.hpp"
#include "cxTextEdit.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
//---------------------------------------------------------------------------
class TformAboutDemo : public TForm
{
__published:	// IDE-managed Components
        TcxRichEdit *redDescription;
private:	// User declarations
        void __fastcall AssignBounds();
public:		// User declarations
        __fastcall TformAboutDemo(TComponent* Owner);
};

void ShowAboutDemoForm();
//---------------------------------------------------------------------------
#endif
