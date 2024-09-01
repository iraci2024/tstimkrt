//---------------------------------------------------------------------------

#ifndef DemoBasicMainH
#define DemoBasicMainH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
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
#include "cxGroupBox.hpp"
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
#include "AboutDemoForm.h"
//---------------------------------------------------------------------------
class TDemoBasicMainForm : public TForm
{
__published:	// IDE-managed Components
	TdxLayoutLookAndFeelList *dxLayoutLookAndFeelList1;
	TdxLayoutCxLookAndFeel *dxLayoutCxLookAndFeel1;
        TcxGroupBox *cxGroupBox1;
	TdxLayoutControl *lcMain;
	TdxGanttControl *GanttControl;
	TcxSpinEdit *seTimelineUnitMinWidth;
	TcxComboBox *cmbTimelineScale;
	TcxComboBox *cbSrcrollBars;
	TcxComboBox *cmbChartTimescale;
	TdxLayoutGroup *lcMainGroup_Root;
	TdxLayoutItem *dxLayoutItem1;
	TdxLayoutGroup *dxLayoutGroup1;
	TdxLayoutGroup *lgOptions;
	TdxLayoutGroup *lgActiveView;
	TdxLayoutCheckBoxItem *lchbDeleteConfirm;
	TdxLayoutCheckBoxItem *lchbDirectX;
	TdxLayoutCheckBoxItem *lchbChartCellAutoHeight;
	TdxLayoutCheckBoxItem *lchbChartColumnHide;
	TdxLayoutCheckBoxItem *lchbChartColumnMove;
	TdxLayoutCheckBoxItem *lchbChartColumnSize;
	TdxLayoutCheckBoxItem *lchbChartColumnQuickCustomization;
	TdxLayoutCheckBoxItem *lchbChartVisible;
	TdxLayoutGroup *dxLayoutGroup2;
	TdxLayoutGroup *dxLayoutGroup3;
	TdxLayoutGroup *dxLayoutGroup4;
	TdxLayoutGroup *dxLayoutGroup5;
	TdxLayoutCheckBoxItem *lchbResourceSheetCellAutoHeight;
	TdxLayoutCheckBoxItem *lchbResourceSheetColumnHide;
	TdxLayoutCheckBoxItem *lchbResourceSheetColumnMove;
	TdxLayoutCheckBoxItem *lchbResourceSheetColumnSize;
	TdxLayoutCheckBoxItem *lchbResourceSheetColumnQuickCustomization;
	TdxLayoutCheckBoxItem *lchbShowOnlyExplicitlyAddedTasks;
	TdxLayoutItem *dxLayoutItem4;
	TdxLayoutItem *dxLayoutItem3;
	TdxLayoutSeparatorItem *dxLayoutSeparatorItem1;
	TdxLayoutEmptySpaceItem *dxLayoutEmptySpaceItem1;
	TdxLayoutItem *dxLayoutItem5;
	TdxLayoutGroup *dxLayoutGroup6;
	TdxLayoutGroup *dxLayoutGroup7;
	TdxLayoutItem *dxLayoutItem2;
	TcxLookAndFeelController *lfController;
	TMainMenu *mmMain;
	TMenuItem *miFile;
	TMenuItem *Import1;
	TMenuItem *Export1;
	TMenuItem *Separator1;
	TMenuItem *miExit;
	TMenuItem *miAbout;
	TOpenDialog *OpenDialog;
	TSaveDialog *SaveDialog;
        TMenuItem *New1;
        TMenuItem *Separator2;
        TLabel *lbDescription;
	void __fastcall FileExitExecute(TObject *Sender);
	void __fastcall Export1Click(TObject *Sender);
	void __fastcall Import1Click(TObject *Sender);
	void __fastcall miAboutClick(TObject *Sender);
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall lchbDirectXClick(TObject *Sender);
	void __fastcall cbSrcrollBarsPropertiesEditValueChanged(TObject *Sender);
	void __fastcall lgActiveViewTabChanged(TObject *Sender);
	void __fastcall lchbChartCellAutoHeightClick(TObject *Sender);
	void __fastcall lchbChartVisibleClick(TObject *Sender);
	void __fastcall cmbChartTimescalePropertiesChange(TObject *Sender);
	void __fastcall lchbResourceSheetCellAutoHeightClick(TObject *Sender);
	void __fastcall lchbShowOnlyExplicitlyAddedTasksClick(TObject *Sender);
	void __fastcall cmbTimelineScalePropertiesChange(TObject *Sender);
	void __fastcall seTimelineUnitMinWidthPropertiesChange(TObject *Sender);
	void __fastcall lchbDeleteConfirmClick(TObject *Sender);
	void __fastcall GanttControlActiveViewChanged(TObject *Sender);
	void __fastcall New1Click(TObject *Sender);
	void __fastcall GanttControlDataModelLoaded(TObject *Sender);
	void __fastcall GanttControlAssignmentChanged(TObject *Sender, TdxGanttControlAssignment *AAssignment);
	void __fastcall GanttControlResourceChanged(TObject *Sender, TdxGanttControlResource *AResource);
	void __fastcall GanttControlTaskChanged(TObject *Sender, TdxGanttControlTask *ATask);
private:	// User declarations
        bool FModified;
protected:
	virtual void AddLookAndFeelMenu();
	virtual bool EnableDocumentOpen();
	virtual TcxLookAndFeelKind GetDefaultLookAndFeelKind();
	virtual bool IsNativeDefaultStyle();
	virtual void SetDefaultLookAndFeel();
public:		// User declarations
	__fastcall TDemoBasicMainForm(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TDemoBasicMainForm *DemoBasicMainForm;
//---------------------------------------------------------------------------
#endif
