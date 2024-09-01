//---------------------------------------------------------------------------

#ifndef FeaturesDemoMainH
#define FeaturesDemoMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "DemoBasicMain.h"
#include "cxClasses.hpp"
#include "cxContainer.hpp"
#include "cxControls.hpp"
#include "cxDropDownEdit.hpp"
#include "cxEdit.hpp"
#include "cxGraphics.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "cxMaskEdit.hpp"
#include "cxSpinEdit.hpp"
#include "cxTextEdit.hpp"
#include "dxCore.hpp"
#include "dxGanttControl.hpp"
#include "dxGanttControlAssignments.hpp"
#include "dxGanttControlCustomClasses.hpp"
#include "dxGanttControlCustomSheet.hpp"
#include "dxGanttControlResources.hpp"
#include "dxGanttControlTasks.hpp"
#include "dxGanttControlViewChart.hpp"
#include "dxGanttControlViewResourceSheet.hpp"
#include "dxGanttControlViewTimeline.hpp"
#include "dxLayoutContainer.hpp"
#include "dxLayoutControl.hpp"
#include "dxLayoutcxEditAdapters.hpp"
#include "dxLayoutLookAndFeels.hpp"
#include <Dialogs.hpp>
#include <Menus.hpp>
//---------------------------------------------------------------------------
class TFeaturesDemoMainForm : public TDemoBasicMainForm
{
__published:	// IDE-managed Components
private:	// User declarations
public:		// User declarations
	__fastcall TFeaturesDemoMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TFeaturesDemoMainForm *FeaturesDemoMainForm;
//---------------------------------------------------------------------------
#endif
