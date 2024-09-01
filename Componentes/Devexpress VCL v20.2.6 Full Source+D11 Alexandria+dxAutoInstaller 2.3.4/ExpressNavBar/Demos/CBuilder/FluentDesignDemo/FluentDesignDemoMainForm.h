//---------------------------------------------------------------------------

#ifndef FluentDesignDemoMainFormH
#define FluentDesignDemoMainFormH
//---------------------------------------------------------------------------
#include <Classes.hpp>
#include <Controls.hpp>
#include <StdCtrls.hpp>
#include <Forms.hpp>
#include "cxControls.hpp"
#include "cxGraphics.hpp"
#include "cxClasses.hpp"
#include "cxLookAndFeelPainters.hpp"
#include "cxLookAndFeels.hpp"
#include "dxSkinsCore.hpp"
#include "cxContainer.hpp"
#include "cxEdit.hpp"
#include "dxNavBar.hpp"
#include "dxLayoutLookAndFeels.hpp"
#include "dxLayoutContainer.hpp"
#include "dxLayoutControl.hpp"
#include "dxSkinsForm.hpp"
#include "dxSkinsFluentDesignForm.hpp"
#include "dxSkinOffice2019Colorful.hpp"
#include "dxNavBarBase.hpp"
#include "dxNavBarCollns.hpp"
#include "cxImageList.hpp"
#include <ImgList.hpp>
//---------------------------------------------------------------------------
class TfrmFluentDesignDemo : public TdxFluentDesignForm
{
__published:	// IDE-managed Components
	TdxLayoutLookAndFeelList *dxLayoutLookAndFeelList1;
	TdxLayoutSkinLookAndFeel *dxLayoutSkinLookAndFeel1;
	TdxSkinController *dxSkinController1;
	TdxLayoutControl *dxLayoutControl1;
	TdxLayoutGroup *dxLayoutControl1Group_Root;
	TdxLayoutGroup *dxLayoutGroup1;
	TdxLayoutCheckBoxItem *dxLayoutCheckBoxItem1;
	TdxLayoutCheckBoxItem *dxLayoutCheckBoxItem2;
	TdxLayoutCheckBoxItem *dxLayoutCheckBoxItem3;
	TdxLayoutGroup *dxLayoutGroup2;
	TdxLayoutGroup *dxLayoutGroup3;
	TdxLayoutRadioButtonItem *dxLayoutRadioButtonItem1;
	TdxLayoutRadioButtonItem *dxLayoutRadioButtonItem2;
	TdxLayoutGroup *dxLayoutGroup4;
	TdxLayoutRadioButtonItem *dxLayoutRadioButtonItem3;
	TdxLayoutRadioButtonItem *dxLayoutRadioButtonItem4;
	TdxLayoutRadioButtonItem *dxLayoutRadioButtonItem5;
	TcxImageList *ilMedium;
	TdxNavBar *dxNavBar1;
	TdxNavBarGroup *nbgContact;
	TdxNavBarGroup *nbgScheduler;
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
	void __fastcall dxLayoutCheckBoxItem1Click(TObject *Sender);
	void __fastcall dxLayoutCheckBoxItem2Click(TObject *Sender);
	void __fastcall dxLayoutCheckBoxItem3Click(TObject *Sender);
	void __fastcall dxLayoutRadioButtonItem5Click(TObject *Sender);
	void __fastcall dxNavBarItem4Click(TObject *Sender);
	void __fastcall dxLayoutRadioButtonItem2Click(TObject *Sender);
	void __fastcall nbgContactSelectedLinkChanged(TObject *Sender);
	void __fastcall dxFluentDesignFormCreate(TObject *Sender);
private:	// User declarations
public:	// User declarations
	__fastcall TfrmFluentDesignDemo(TComponent* Owner);
};
//---------------------------------------------------------------------------
extern PACKAGE TfrmFluentDesignDemo *frmFluentDesignDemo;
//---------------------------------------------------------------------------
#endif
