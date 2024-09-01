//---------------------------------------------------------------------------

#ifndef HamburgerMenuDemoMainH
#define HamburgerMenuDemoMainH
//---------------------------------------------------------------------------
#include <Buttons.hpp>
#include <ExtCtrls.hpp>
#include <ImgList.hpp>
#include "cxGraphics.hpp"
#include "cxClasses.hpp"
#include "cxControls.hpp"
#include "cxImageList.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "dxGDIPlusClasses.hpp"
#include "dxLayoutContainer.hpp"
#include "dxLayoutControl.hpp"
#include "dxNavBar.hpp"
#include "dxNavBarBase.hpp"
#include "dxNavBarCollns.hpp"
#include "dxSkinsdxNavBarHamburgerMenuPainter.hpp"
#include <Classes.hpp>
#include <Controls.hpp>
#include <System.hpp>
#include "dxLayoutLookAndFeels.hpp"
//---------------------------------------------------------------------------
class TfrmHamburgerMenuDemo : public TForm
{
__published:	// IDE-managed Components
	TSpeedButton *sbView;
	TSpeedButton *sbHelp;
	TcxImageList *ilSmall;
	TcxImageList *ilLarge;
	TcxImageList *ilMedium;
	TdxNavBar *dxNavBar1;
	TdxNavBarGroup *nbgContact;
	TdxNavBarGroup *nbgSheduler;
	TdxNavBarGroup *nbgMail;
	TdxNavBarGroup *nbgSettings;
	TdxNavBarGroup *Contacts;
	TdxNavBarGroup *nbgFilterContacts;
	TdxNavBarGroup *nbgDevExpressAccount;
	TdxNavBarGroup *nbgMicrosoftAccount;
	TdxNavBarGroup *nbgAccount;
	TdxNavBarGroup *nbgFilterMailList;
	TdxNavBarItem *nbInplaceManageAccounts;
	TdxNavBarItem *nbInplacePersonalization;
	TdxNavBarItem *nbInplaceAutomaticReplies;
	TdxNavBarItem *nbInplaceFocusedInbox;
	TdxNavBarItem *nbInplaceMessageList;
	TdxNavBarItem *nbInplaceReadingPane;
	TdxNavBarItem *nbInplaceSingature;
	TdxNavBarItem *nbInplaceNotifications;
	TdxNavBarItem *nbInplaceAbout;
	TdxNavBarItem *nbiNewContact;
	TdxNavBarItem *nbiFilterContactsAll;
	TdxNavBarItem *nbiFilterContactsSales;
	TdxNavBarItem *nbiFilterContactsSupport;
	TdxNavBarItem *nbiFilterContactsShipping;
	TdxNavBarItem *nbiFilterContactsEngineering;
	TdxNavBarItem *nbiFilterContactsHumanResources;
	TdxNavBarItem *nbiFilterContactsManagement;
	TdxNavBarItem *nbiFilterContactsIT;
	TdxNavBarItem *nbiSchedulerNewEvent;
	TdxNavBarItem *nbiSchedulerCalendar;
	TdxNavBarItem *nbiSchedulerBithDate;
	TdxNavBarItem *nbiSchedulerMSCalendar;
	TdxNavBarItem *nbiMailNew;
	TdxNavBarItem *nbiMailAccount1;
	TdxNavBarItem *nbiMailAccount2;
	TdxNavBarItem *nbiMailAccount3;
	TdxNavBarItem *nbiFileterMailAll;
	TdxNavBarItem *nbiFileterMailRead;
	TdxNavBarItem *nbiFileterMailToday;
	TdxNavBarItem *nbiFileterMailYesterday;
	TdxNavBarItem *nbiFileterMailImportance;
	TdxNavBarItem *dxNavBarItem1;
	TdxNavBarItem *dxNavBarItem2;
	TdxNavBarItem *dxNavBarItem3;
	TdxNavBarItem *dxNavBarItem4;
	TdxNavBarItem *dxNavBar1Item1;
	TdxNavBarItem *dxNavBar1Item2;
	TdxNavBarItem *dxNavBar1Item3;
	TdxLayoutControl *dxLayoutControl4;
	TdxLayoutGroup *dxLayoutControl4Group_Root;
	TdxLayoutRadioButtonItem *lrgNavigationPaneModeCompact;
	TdxLayoutRadioButtonItem *lrgNavigationPaneModeFull;
	TdxLayoutRadioButtonItem *lrgNavigationPaneModeNone;
	TdxLayoutGroup *dxLayoutGroup2;
	TdxLayoutGroup *dxLayoutGroup1;
	TdxLayoutRadioButtonItem *lrgImagesSizeSmall;
	TdxLayoutRadioButtonItem *lrgImagesSizeMedium;
	TdxLayoutRadioButtonItem *lrgImagesSizeLarge;
	TdxLayoutRadioButtonItem *lrgImagesSizeNone;
	TdxLayoutGroup *dxLayoutGroup4;
	TdxLayoutRadioButtonItem *lrgDisplayModeInline;
	TdxLayoutRadioButtonItem *lrgDisplayModeOverlay;
	TdxLayoutRadioButtonItem *lrgDisplayModeMinimal;
	TdxLayoutGroup *dxLayoutGroup3;
	TdxLayoutRadioButtonItem *lrgSelectedPopupModeDocked;
	TdxLayoutRadioButtonItem *lrgSelectedPopupModeUndocked;
	TdxLayoutCheckBoxItem *lcbAllowAnimation;
	TdxLayoutGroup *dxLayoutGroup5;
	TdxLayoutLookAndFeelList *dxLayoutLookAndFeelList1;
	TdxLayoutGroup *dxLayoutControl1Group_Root;
	TdxLayoutControl *dxLayoutControl1;
	TdxLayoutImageItem *dxLayoutImageItem1;
	void __fastcall FormCreate(TObject *Sender);
	void __fastcall lcbAllowAnimationClick(TObject *Sender);
	void __fastcall lrgDisplayModeInlineClick(TObject *Sender);
	void __fastcall lrgImagesTypeBitmapClick(TObject *Sender);
	void __fastcall lrgImagesSizeSmallClick(TObject *Sender);
	void __fastcall dxNavBar1Item11Click(TObject *Sender);
	void __fastcall dxNavBar1OnCustomDrawLinkSelection(TObject *Sender, TCanvas *ACanvas,
          TdxNavBarLinkViewInfo *AViewInfo, bool &AHandled);
	void __fastcall lrgNavigationPaneModeCompactClick(TObject *Sender);
	void __fastcall lrgSelectedPopupModeDockedClick(TObject *Sender);
	void __fastcall dxNavBar1LinkClick(TObject *Sender, TdxNavBarItemLink *ALink);
	void __fastcall nbgContactSelectedLinkChanged(TObject *Sender);
	void __fastcall nbgShedulerSelectedLinkChanged(TObject *Sender);
	void __fastcall nbgMailSelectedLinkChanged(TObject *Sender);
	void __fastcall dxNavBar1GetOverlaySize(TObject *Sender, int &AWidth, int &AHeight);
private:	// User declarations
public:		// User declarations
	__fastcall TfrmHamburgerMenuDemo(TComponent* Owner);
	int GetLayoutRadioButtonTag(TObject *Sender);
	void UpdateImages();
	void ShowAboutDemoForm();
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmHamburgerMenuDemo *frmHamburgerMenuDemo;
//---------------------------------------------------------------------------
#endif
