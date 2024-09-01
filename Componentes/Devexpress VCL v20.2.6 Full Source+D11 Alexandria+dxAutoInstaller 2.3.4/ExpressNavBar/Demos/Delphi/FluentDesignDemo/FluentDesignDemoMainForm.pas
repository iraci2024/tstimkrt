unit FluentDesignDemoMainForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  cxControls, cxGraphics, cxLookAndFeelPainters, cxLookAndFeels,
  dxSkinsCore, cxContainer, cxEdit, dxNavBar, cxClasses,
  dxLayoutLookAndFeels, dxLayoutContainer, dxLayoutControl,
  dxSkinsForm, dxSkinsFluentDesignForm, dxSkinOffice2019Colorful,  dxNavBarCollns, dxNavBarBase,
  ImgList, cxImageList, dxNavBarConsts, dxCore;

type
  TfrmFluentDesignDemo = class(TdxFluentDesignForm)
    dxSkinController1: TdxSkinController;
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutControl1: TdxLayoutControl;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutSkinLookAndFeel1: TdxLayoutSkinLookAndFeel;
    ilMedium: TcxImageList;
    dxNavBar1: TdxNavBar;
    nbgContact: TdxNavBarGroup;
    nbgScheduler: TdxNavBarGroup;
    nbgMail: TdxNavBarGroup;
    nbgSettings: TdxNavBarGroup;
    Contacts: TdxNavBarGroup;
    nbgFilterContacts: TdxNavBarGroup;
    nbgDevExpressAccount: TdxNavBarGroup;
    nbgMicrosoftAccount: TdxNavBarGroup;
    nbgAccount: TdxNavBarGroup;
    nbgFilterMailList: TdxNavBarGroup;
    nbInplaceManageAccounts: TdxNavBarItem;
    nbInplacePersonalization: TdxNavBarItem;
    nbInplaceAutomaticReplies: TdxNavBarItem;
    nbInplaceFocusedInbox: TdxNavBarItem;
    nbInplaceMessageList: TdxNavBarItem;
    nbInplaceReadingPane: TdxNavBarItem;
    nbInplaceSingature: TdxNavBarItem;
    nbInplaceNotifications: TdxNavBarItem;
    nbInplaceAbout: TdxNavBarItem;
    nbiNewContact: TdxNavBarItem;
    nbiFilterContactsAll: TdxNavBarItem;
    nbiFilterContactsSales: TdxNavBarItem;
    nbiFilterContactsSupport: TdxNavBarItem;
    nbiFilterContactsShipping: TdxNavBarItem;
    nbiFilterContactsEngineering: TdxNavBarItem;
    nbiFilterContactsHumanResources: TdxNavBarItem;
    nbiFilterContactsManagement: TdxNavBarItem;
    nbiFilterContactsIT: TdxNavBarItem;
    nbiSchedulerNewEvent: TdxNavBarItem;
    nbiSchedulerCalendar: TdxNavBarItem;
    nbiSchedulerBithDate: TdxNavBarItem;
    nbiSchedulerMSCalendar: TdxNavBarItem;
    nbiMailNew: TdxNavBarItem;
    nbiMailAccount1: TdxNavBarItem;
    nbiMailAccount2: TdxNavBarItem;
    nbiMailAccount3: TdxNavBarItem;
    nbiFileterMailAll: TdxNavBarItem;
    nbiFileterMailRead: TdxNavBarItem;
    nbiFileterMailToday: TdxNavBarItem;
    nbiFileterMailYesterday: TdxNavBarItem;
    nbiFileterMailImportance: TdxNavBarItem;
    dxNavBarItem1: TdxNavBarItem;
    dxNavBarItem2: TdxNavBarItem;
    dxNavBarItem3: TdxNavBarItem;
    dxNavBarItem4: TdxNavBarItem;
    dxNavBar1Item1: TdxNavBarItem;
    dxNavBar1Item2: TdxNavBarItem;
    dxNavBar1Item3: TdxNavBarItem;
    dxLayoutGroup1: TdxLayoutGroup;
    dxLayoutCheckBoxItem1: TdxLayoutCheckBoxItem;
    dxLayoutCheckBoxItem2: TdxLayoutCheckBoxItem;
    dxLayoutCheckBoxItem3: TdxLayoutCheckBoxItem;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup3: TdxLayoutGroup;
    dxLayoutRadioButtonItem1: TdxLayoutRadioButtonItem;
    dxLayoutRadioButtonItem2: TdxLayoutRadioButtonItem;
    dxLayoutGroup4: TdxLayoutGroup;
    dxLayoutRadioButtonItem3: TdxLayoutRadioButtonItem;
    dxLayoutRadioButtonItem4: TdxLayoutRadioButtonItem;
    dxLayoutRadioButtonItem5: TdxLayoutRadioButtonItem;
    dxLayoutGroup5: TdxLayoutGroup;
    dxLayoutGroup6: TdxLayoutGroup;
    dxLayoutImageItem1: TdxLayoutImageItem;
    procedure dxFluentDesignFormCreate(Sender: TObject);
    procedure dxLayoutCheckBoxItem1Click(Sender: TObject);
    procedure dxLayoutCheckBoxItem2Click(Sender: TObject);
    procedure dxLayoutCheckBoxItem3Click(Sender: TObject);
    procedure dxLayoutRadioButtonItem2Click(Sender: TObject);
    procedure dxLayoutRadioButtonItem5Click(Sender: TObject);
    procedure dxNavBarItem4Click(Sender: TObject);
    procedure nbgContactSelectedLinkChanged(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmFluentDesignDemo: TfrmFluentDesignDemo;

implementation

{$R *.dfm}

uses AboutDemoForm;

procedure TfrmFluentDesignDemo.dxFluentDesignFormCreate(Sender: TObject);
begin
  dxSkinController1.NativeStyle := False;
  if not IsWinSupportsAcrylicEffect then
    ShowMessage('Acrylic Material and Reveal Highlight effects are available on systems with Windows 10 Fall Creators Update (OS Build 17064 or later).');
end;

procedure TfrmFluentDesignDemo.dxLayoutCheckBoxItem1Click(Sender: TObject);
begin
  EnableAcrylicEffects := dxLayoutCheckBoxItem1.Checked;
  dxLayoutCheckBoxItem3.Enabled := EnableAcrylicEffects;
end;

procedure TfrmFluentDesignDemo.dxLayoutCheckBoxItem2Click(Sender: TObject);
begin
  ExtendNavigationControlToCaption := dxLayoutCheckBoxItem2.Checked;
end;

procedure TfrmFluentDesignDemo.dxLayoutCheckBoxItem3Click(Sender: TObject);
begin
  dxNavBar1.OptionsBehavior.Common.RevealHighlight := dxLayoutCheckBoxItem3.Checked;
end;

procedure TfrmFluentDesignDemo.dxLayoutRadioButtonItem2Click(Sender: TObject);
begin
  if dxLayoutRadioButtonItem1.Checked then
    dxNavBar1.View := dxNavBarAccordionView
  else
    dxNavBar1.View := dxNavBarHamburgerMenu;
  dxLayoutGroup4.Enabled := dxNavBar1.View = dxNavBarHamburgerMenu;
end;

procedure TfrmFluentDesignDemo.dxLayoutRadioButtonItem5Click(Sender: TObject);
begin
  dxNavBar1.OptionsView.HamburgerMenu.NavigationPaneMode := TdxNavBarHamburgerMenuNavigationPaneMode((Sender as TComponent).Tag);
end;

procedure TfrmFluentDesignDemo.dxNavBarItem4Click(Sender: TObject);
begin
  ShowAboutDemoForm;
end;

procedure TfrmFluentDesignDemo.nbgContactSelectedLinkChanged(Sender: TObject);
begin
  (Sender as TdxNavBarGroup).SelectedLinkIndex := -1;
end;

end.
