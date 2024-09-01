unit HamburgerMenuDemoMain;

{$I cxVer.inc}

interface

uses
{$IFDEF EXPRESSSKINS}
  dxSkinsForm,
{$ENDIF}
  Windows, Messages, SysUtils, Forms, ComCtrls, ImgList, Controls, Classes, Buttons, ExtCtrls, Graphics, SkinDemoUtils,
  cxControls, cxLookAndFeels, dxGDIPlusClasses, cxClasses, cxEdit, dxForms, cxGraphics, cxLookAndFeelPainters,
  dxLayoutContainer, dxNavBarCollns, dxNavBar, dxNavBarBase, dxSkinsdxNavBarHamburgerMenuPainter, dxLayoutControl,
  cxImageList, dxLayoutLookAndFeels, Types;

type
  TfrmHamburgerMenuDemo = class(TdxForm)
    sbHelp: TSpeedButton;
    sbView: TSpeedButton;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
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
    ilSmall: TcxImageList;
    ilLarge: TcxImageList;
    lcbAllowAnimation: TdxLayoutCheckBoxItem;
    dxLayoutControl4: TdxLayoutControl;
    dxLayoutControl4Group_Root: TdxLayoutGroup;
    lrgNavigationPaneModeCompact: TdxLayoutRadioButtonItem;
    lrgNavigationPaneModeFull: TdxLayoutRadioButtonItem;
    lrgNavigationPaneModeNone: TdxLayoutRadioButtonItem;
    dxLayoutGroup2: TdxLayoutGroup;
    dxLayoutGroup1: TdxLayoutGroup;
    lrgImagesSizeSmall: TdxLayoutRadioButtonItem;
    lrgImagesSizeMedium: TdxLayoutRadioButtonItem;
    lrgImagesSizeLarge: TdxLayoutRadioButtonItem;
    lrgImagesSizeNone: TdxLayoutRadioButtonItem;
    dxLayoutGroup4: TdxLayoutGroup;
    lrgDisplayModeInline: TdxLayoutRadioButtonItem;
    lrgDisplayModeOverlay: TdxLayoutRadioButtonItem;
    lrgDisplayModeMinimal: TdxLayoutRadioButtonItem;
    dxLayoutGroup3: TdxLayoutGroup;
    lrgSelectedPopupModeDocked: TdxLayoutRadioButtonItem;
    lrgSelectedPopupModeUndocked: TdxLayoutRadioButtonItem;
    dxLayoutGroup5: TdxLayoutGroup;
    dxLayoutControl1Group_Root: TdxLayoutGroup;
    dxLayoutControl1: TdxLayoutControl;
    dxLayoutImageItem1: TdxLayoutImageItem;
    procedure FormCreate(Sender: TObject);
    procedure dxNavBar1OnCustomDrawLinkSelection(Sender: TObject; ACanvas: TCanvas; AViewInfo: TdxNavBarLinkViewInfo;
      var AHandled: Boolean);
    procedure rgImagesTypePropertiesChange(Sender: TObject);
    procedure rgImagesSizePropertiesChange(Sender: TObject);
    procedure dxNavBar1GetOverlaySize(Sender: TObject; var  AWidth, AHeight: Integer);
    procedure lrgDisplayModeInlineClick(Sender: TObject);
    procedure lrgSelectedPopupModeDockedClick(Sender: TObject);
    procedure lrgNavigationPaneModeCompactClick(Sender: TObject);
    procedure lrgImagesTypeSVGClick(Sender: TObject);
    procedure lrgImagesSizeSmallClick(Sender: TObject);
    procedure lcbAllowAnimationClick(Sender: TObject);
    procedure dxNavBar1LinkClick(Sender: TObject; ALink: TdxNavBarItemLink);
    procedure dxNavBarItem4Click(Sender: TObject);
    procedure nbgMailSelectedLinkChanged(Sender: TObject);
    procedure nbgSchedulerSelectedLinkChanged(Sender: TObject);
    procedure nbgContactSelectedLinkChanged(Sender: TObject);
  private
  {$IFDEF EXPRESSSKINS}
    FSkinController: TdxSkinController;
  {$ENDIF}
    function GetLayoutRadioButtonTag(Sender: TObject): Integer;
    procedure InitializeLookAndFeel;
    procedure UpdateImages;
  end;

var
  frmHamburgerMenuDemo: TfrmHamburgerMenuDemo;

implementation

uses
  AboutDemoForm, Dialogs;

{$R *.dfm}

{ TfrmHamburgerMenuDemo }

procedure TfrmHamburgerMenuDemo.lcbAllowAnimationClick(Sender: TObject);
begin
  dxNavBar1.OptionsBehavior.Common.AllowExpandAnimation := lcbAllowAnimation.Checked;
end;

procedure TfrmHamburgerMenuDemo.lrgDisplayModeInlineClick(Sender: TObject);
var
  ADisplayMode: TdxNavBarHamburgerMenuDisplayMode;
begin
  ADisplayMode := TdxNavBarHamburgerMenuDisplayMode(GetLayoutRadioButtonTag(Sender));
  if ADisplayMode = dmOverlayMinimal then
  begin
    dxNavBar1.OptionsBehavior.HamburgerMenu.DisplayMode := ADisplayMode;
    dxNavBar1.Align := alTop;
  end
  else
  begin
    dxNavBar1.Align := alLeft;
    dxNavBar1.OptionsBehavior.HamburgerMenu.DisplayMode := ADisplayMode;
  end;
end;

procedure TfrmHamburgerMenuDemo.lrgImagesSizeSmallClick(Sender: TObject);
begin
  UpdateImages;
end;

procedure TfrmHamburgerMenuDemo.lrgImagesTypeSVGClick(Sender: TObject);
begin
  UpdateImages;
end;

procedure TfrmHamburgerMenuDemo.lrgNavigationPaneModeCompactClick(Sender: TObject);
begin
  dxNavBar1.OptionsView.HamburgerMenu.NavigationPaneMode :=
    TdxNavBarHamburgerMenuNavigationPaneMode(GetLayoutRadioButtonTag(Sender));
end;

procedure TfrmHamburgerMenuDemo.UpdateImages;
begin
  if lrgImagesSizeSmall.Checked then
    dxNavBar1.OptionsImage.SmallImages := ilSmall
  else
    if lrgImagesSizeLarge.Checked then
      dxNavBar1.OptionsImage.SmallImages := ilLarge
    else
      if lrgImagesSizeMedium.Checked then
        dxNavBar1.OptionsImage.SmallImages := ilMedium
      else
        dxNavBar1.OptionsImage.SmallImages := nil;
end;

procedure TfrmHamburgerMenuDemo.rgImagesSizePropertiesChange(Sender: TObject);
begin
  UpdateImages;
end;

procedure TfrmHamburgerMenuDemo.rgImagesTypePropertiesChange(Sender: TObject);
begin
  UpdateImages;
end;

procedure TfrmHamburgerMenuDemo.FormCreate(Sender: TObject);
begin
  lcbAllowAnimation.Checked := True;
  lrgDisplayModeInline.Checked := True;
  lrgSelectedPopupModeDocked.Checked := True;
  lrgNavigationPaneModeCompact.Checked := True;
  lrgImagesSizeMedium.Checked := True;
  InitializeLookAndFeel;
end;

function TfrmHamburgerMenuDemo.GetLayoutRadioButtonTag(Sender: TObject): Integer;
begin
  Result := (Sender as TdxLayoutRadioButtonItem).Tag;
end;

procedure TfrmHamburgerMenuDemo.InitializeLookAndFeel;
begin
{$IFDEF EXPRESSSKINS}
  FSkinController := TdxSkinController.Create(Self);
  FSkinController.NativeStyle := False;
  FSkinController.SkinName := 'Basic';
{$ENDIF}
end;

procedure TfrmHamburgerMenuDemo.lrgSelectedPopupModeDockedClick(Sender: TObject);
begin
  dxNavBar1.OptionsBehavior.HamburgerMenu.SelectedGroupPopupMode :=
    TdxNavBarHamburgerMenuSelectedGroupPopupMode(GetLayoutRadioButtonTag(Sender));
end;

procedure TfrmHamburgerMenuDemo.nbgContactSelectedLinkChanged(Sender: TObject);
begin
  nbgContact.SelectedLinkIndex := -1;
end;

procedure TfrmHamburgerMenuDemo.nbgMailSelectedLinkChanged(Sender: TObject);
begin
  nbgMail.SelectedLinkIndex := -1;
end;

procedure TfrmHamburgerMenuDemo.nbgSchedulerSelectedLinkChanged(Sender: TObject);
begin
  nbgScheduler.SelectedLinkIndex := -1;
end;

procedure TfrmHamburgerMenuDemo.dxNavBar1GetOverlaySize(Sender: TObject; var AWidth, AHeight: Integer);
begin
  AHeight := ClientHeight;
end;

procedure TfrmHamburgerMenuDemo.dxNavBar1LinkClick(Sender: TObject; ALink: TdxNavBarItemLink);
begin
  ShowMessage('OnLinkClick: ' + ALink.Item.Caption);
end;

procedure TfrmHamburgerMenuDemo.dxNavBar1OnCustomDrawLinkSelection(Sender: TObject; ACanvas: TCanvas;
  AViewInfo: TdxNavBarLinkViewInfo; var AHandled: Boolean);
var
  R: TRect;
  AStates: TdxNavBarObjectStates;
  APainter: TdxNavBarHamburgerMenuPainter;
begin
  APainter := AViewInfo.ViewInfo.Painter as TdxNavBarHamburgerMenuPainter;
  AStates := AViewInfo.State;
  AHandled := sSelected in AStates;
  if AHandled then
  begin
    Exclude(AStates, sSelected);
    APainter.DrawItemState(AViewInfo, AViewInfo.SelectionRect, AStates);
    R := AViewInfo.Rect;
    R.Right := R.Left + 3;
    ACanvas.Brush.Color := clWhite;
    ACanvas.FillRect(R);
  end;
end;

procedure TfrmHamburgerMenuDemo.dxNavBarItem4Click(Sender: TObject);
begin
  ShowAboutDemoForm;
end;

end.
