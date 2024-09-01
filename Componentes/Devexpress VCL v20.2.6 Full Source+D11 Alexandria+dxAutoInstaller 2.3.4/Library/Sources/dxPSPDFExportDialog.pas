{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressPrinting System }
{ }
{ Copyright (C) 1998-2021 Developer Express Inc. }
{ ALL RIGHTS RESERVED }
{ }
{ The entire contents of this file is protected by U.S. and }
{ International Copyright Laws. Unauthorized reproduction, }
{ reverse-engineering, and distribution of all or any portion of }
{ the code contained in this file is strictly prohibited and may }
{ result in severe civil and criminal penalties and will be }
{ prosecuted to the maximum extent possible under the law. }
{ }
{ RESTRICTIONS }
{ }
{ THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES }
{ (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE }
{ SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS }
{ LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND }
{ ALL ACCOMPANYING VCL CONTROLS AS PART OF AN }
{ EXECUTABLE PROGRAM ONLY. }
{ }
{ THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED }
{ FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE }
{ COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE }
{ AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT }
{ AND PERMISSION FROM DEVELOPER EXPRESS INC. }
{ }
{ CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON }
{ ADDITIONAL RESTRICTIONS. }
{ }
{ ******************************************************************** }

unit dxPSPDFExportDialog;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  dxCore, dxX509Certificate, dxPSPDFExport, dxPSCore, dxPSEngn, dxPSGlbl,
  dxPrnDlg, cxTextEdit,
  StdCtrls, cxRadioGroup, cxControls, cxContainer, cxEdit, cxLabel, cxClasses,
  cxLookAndFeelPainters, cxGroupBox, cxPC, cxCheckBox, cxTrackBar, Menus,
  cxButtons, dxPSRes, dxPSForm, dxPSPDFExportCore, cxGraphics, cxMaskEdit,
  cxDropDownEdit, cxLookAndFeels, dxLayoutcxEditAdapters,
  dxLayoutControlAdapters, dxLayoutLookAndFeels,
  dxLayoutContainer, dxLayoutControl, cxCustomListBox, cxMCListBox, ComCtrls,
  cxListView;

type

  { TdxPSPDFExportDialogForm }

  TdxPSPDFExportDialogForm = class(TCustomdxPSForm)
    btnCancel: TcxButton;
    btnOk: TcxButton;
    cbCompressed: TcxCheckBox;
    cbEmbedFonts: TcxCheckBox;
    cbJpgCompress: TcxCheckBox;
    cbOpenAfterExport: TcxCheckBox;
    edPageRanges: TcxTextEdit;
    rbtnAllPages: TcxRadioButton;
    rbtnCurrentPage: TcxRadioButton;
    rbtnPageRanges: TcxRadioButton;
    tbJpgCompression: TcxTrackBar;
    teAuthor: TcxTextEdit;
    teCreator: TcxTextEdit;
    teKeywords: TcxTextEdit;
    teSubject: TcxTextEdit;
    teTitle: TcxTextEdit;
    cbUseCIDFonts: TcxCheckBox;
    cbAllowChanging: TcxCheckBox;
    cbAllowPrinting: TcxCheckBox;
    cbAllowComments: TcxCheckBox;
    cbAllowContentCopying: TcxCheckBox;
    edUserPassword: TcxTextEdit;
    edOwnerPassword: TcxTextEdit;
    cbAllowDocumentAssembly: TcxCheckBox;
    cbAllowPrintingHiResolution: TcxCheckBox;
    cbxMethod: TcxComboBox;
    lcMainGroup_Root: TdxLayoutGroup;
    lcMain: TdxLayoutControl;
    dxLayoutGroup2: TdxLayoutGroup;
    tbsExport: TdxLayoutGroup;
    gbExportSettings: TdxLayoutGroup;
    dxLayoutGroup6: TdxLayoutGroup;
    dxLayoutItem1: TdxLayoutItem;
    dxLayoutItem2: TdxLayoutItem;
    dxLayoutItem3: TdxLayoutItem;
    dxLayoutItem4: TdxLayoutItem;
    dxLayoutItem5: TdxLayoutItem;
    dxLayoutItem7: TdxLayoutItem;
    tbsPageRange: TdxLayoutGroup;
    dxLayoutGroup10: TdxLayoutGroup;
    dxLayoutGroup11: TdxLayoutGroup;
    dxLayoutItem8: TdxLayoutItem;
    dxLayoutItem9: TdxLayoutItem;
    dxLayoutGroup13: TdxLayoutGroup;
    dxLayoutItem10: TdxLayoutItem;
    dxLayoutItem11: TdxLayoutItem;
    lbDescription: TdxLayoutLabeledItem;
    tbsDocInfo: TdxLayoutGroup;
    dxLayoutGroup16: TdxLayoutGroup;
    lbTitle: TdxLayoutItem;
    lbAuthor: TdxLayoutItem;
    lbSubject: TdxLayoutItem;
    lbKeywords: TdxLayoutItem;
    lbCreator: TdxLayoutItem;
    tbsSecurity: TdxLayoutGroup;
    gbSecuritySettings: TdxLayoutGroup;
    dxLayoutGroup8: TdxLayoutGroup;
    dxLayoutItem19: TdxLayoutItem;
    dxLayoutGroup17: TdxLayoutGroup;
    lbUserPassword: TdxLayoutItem;
    lbOwnerPassword: TdxLayoutItem;
    lbMethod: TdxLayoutItem;
    dxLayoutItem23: TdxLayoutItem;
    dxLayoutItem24: TdxLayoutItem;
    dxLayoutItem25: TdxLayoutItem;
    licbAllowDocumentAssembly: TdxLayoutItem;
    licbAllowPrintingHiResolution: TdxLayoutItem;
    dxLayoutGroup18: TdxLayoutGroup;
    dxLayoutItem28: TdxLayoutItem;
    dxLayoutItem29: TdxLayoutItem;
    dxLayoutLookAndFeelList1: TdxLayoutLookAndFeelList;
    dxLayoutCxLookAndFeel1: TdxLayoutCxLookAndFeel;
    lbMaxQuality: TdxLayoutLabeledItem;
    lbMaxCompression: TdxLayoutLabeledItem;
    dxLayoutGroup1: TdxLayoutGroup;
    tbsSignature: TdxLayoutGroup;
    lgSignatureSettings: TdxLayoutGroup;
    teSignatureReason: TcxTextEdit;
    liteSignatureReason: TdxLayoutItem;
    teSignatureLocation: TcxTextEdit;
    liteSignatureLocation: TdxLayoutItem;
    teSignatureContactInfo: TcxTextEdit;
    liteSignatureContactInfo: TdxLayoutItem;
    tbsSignatureDetails: TdxLayoutGroup;
    tbsSignatureCertificate: TdxLayoutGroup;
    lrbSignatureUseCertificateFromSystemStore: TdxLayoutRadioButtonItem;
    lrbSignatureUseCertificateFromFile: TdxLayoutRadioButtonItem;
    lgSignatureSystemStorage: TdxLayoutGroup;
    lgSignatureFileStore: TdxLayoutGroup;
    dxLayoutItem12: TdxLayoutItem;
    btnSignatureViewCertificate: TcxButton;
    peSytemStorage: TcxPopupEdit;
    lipeSytemStorage: TdxLayoutItem;
    teSignatureCertificateFileName: TcxTextEdit;
    liteSignatureCertificateFileName: TdxLayoutItem;
    lvSystemStorage: TcxListView;
    procedure cbJpgCompressClick(Sender: TObject);
    procedure rbtnPageRangesClick(Sender: TObject);
    procedure edPageRangesKeyPress(Sender: TObject; var Key: Char);
    procedure cbxMethodPropertiesChange(Sender: TObject);
    procedure lrbSignatureUseCertificateFromSystemStoreClick(Sender: TObject);
    procedure lrbSignatureUseCertificateFromFileClick(Sender: TObject);

    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);

    procedure OnSecuritySettingsChangeHandler(Sender: TObject);
    procedure btnSignatureViewCertificateClick(Sender: TObject);
    procedure lbSystemStorageClick(Sender: TObject);
    procedure peSytemStoragePropertiesCloseUp(Sender: TObject);
    procedure peSytemStorageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure peSytemStorageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure lcMainClick(Sender: TObject);

    procedure OnSignatureFileNameBrowseButtonClockHandler(Sender: TObject;
      AButtonIndex: Integer);
    procedure teSignatureCertificateFileNamePropertiesEditValueChanged
      (Sender: TObject);
    procedure lvSystemStorageClick(Sender: TObject);
    procedure cbAllowPrintingPropertiesEditValueChanged(Sender: TObject);
  private
    FCertificateVerificationLockCount: Integer;
    FFileCertificate: TdxX509Certificate;
    FSignatureFileNameBrowseButton: TcxEditButton;
    FUserCertificate: TdxX509Certificate;

    FSystemCertificate: TdxX509Certificate;
    FCertificateList: TdxX509CertificateList;
    FIsSystemStoragePopupClosed: Boolean;

    function GetActualCertificate: TdxX509Certificate;
    procedure SetFileCertificate(AValue: TdxX509Certificate);
    procedure SetUserCertificate(AValue: TdxX509Certificate);
    procedure SetSystemCertificate(AValue: TdxX509Certificate);

    function CheckUserInput: Boolean;
    function IsCertificateVerificationLocked: Boolean;
    procedure CheckCertificatePassword(const AFileName: string);
    procedure LockCertificateChecking;
    procedure PopulateCertificateList;
    procedure ShowInvalidFileNameMessage(const AFileName: string);
    procedure UnlockCertificateChecking;
    procedure UpdateSignatureButtons;
    procedure UpdateSystemStorageCertificateCaption;
    procedure SyncPrintingPermissions;

    function IsSupportedCertificate(ACertificate: TdxX509Certificate)
      : Boolean; virtual;

    property FileCertificate: TdxX509Certificate read FFileCertificate
      write SetFileCertificate;
    property UserCertificate: TdxX509Certificate read FUserCertificate
      write SetUserCertificate;
    property SystemCertificate: TdxX509Certificate read FSystemCertificate
      write SetSystemCertificate;
  protected
    function GetPageRanges: TdxPageRanges;
    procedure LoadSecurityOptions(AOptions: TdxPSPDFSecurityOptions);
    procedure LoadSignatureOptions(AOptions: TdxPSPDFSignatureOptions);
    procedure SaveSecurityOptions(AOptions: TdxPSPDFSecurityOptions);
    procedure SaveSignatureOptions(AOptions: TdxPSPDFSignatureOptions);
    procedure SetPageRanges(AValue: TdxPageRanges);

    procedure FilterCertificateList(AList: TdxX509CertificateList);

    property ActualCertificate: TdxX509Certificate read GetActualCertificate;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadOptions(AOptions: TdxPSPDFReportExportOptions);
    procedure LoadTranslation;
    procedure SaveOptions(AOptions: TdxPSPDFReportExportOptions);
    //
    property PageRanges: TdxPageRanges read GetPageRanges write SetPageRanges;
  end;

function dxPSShowPDFSettingsDialog(AOptions: TdxPSPDFReportExportOptions;
  AIsDesignTime: Boolean = False): Boolean;

implementation

{$R *.dfm}

uses
  Math, cxDateUtils, cxGeometry, dxX509CertificatePasswordDialog, dxPSUtl;

type
  TcxPopupEditPropertiesAccess = class(TcxPopupEditProperties);

function dxPSShowPDFSettingsDialog(AOptions: TdxPSPDFReportExportOptions;
  AIsDesignTime: Boolean = False): Boolean;
begin
  with TdxPSPDFExportDialogForm.Create(nil) do
    try
      tbsPageRange.Visible := not AIsDesignTime;
      LoadOptions(AOptions);
      Result := ShowModal = mrOk;
      if Result then
        SaveOptions(AOptions);
    finally
      Free;
    end;
end;

{ TdxPSPDFExportDialogForm }

constructor TdxPSPDFExportDialogForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  dxLayoutGroup2.ItemIndex := tbsExport.Index;
  LoadTranslation;

  tbsExport.Visible := True;
  gbSecuritySettings.ButtonOptions.CheckBox.Checked := False;
  lgSignatureSettings.OnCheckBoxStateChanged := OnSecuritySettingsChangeHandler;
  teSignatureCertificateFileName.Text := '';
  peSytemStorage.Text := '';
  FIsSystemStoragePopupClosed := True;

  FFileCertificate := nil;
  FSystemCertificate := nil;

  FCertificateList := TdxX509CertificateList.Create;

  lvSystemStorage.ReadOnly := True;
  lvSystemStorage.Style.BorderStyle := cbsNone;
  lvSystemStorage.StyleDisabled.BorderStyle := cbsNone;
  lvSystemStorage.StyleHot.BorderStyle := cbsNone;
  lvSystemStorage.StyleFocused.BorderStyle := cbsNone;
  lvSystemStorage.MultiSelect := False;
  lvSystemStorage.ViewStyle := vsReport;
  lvSystemStorage.Width := peSytemStorage.Width - ScaleFactor.Apply(2);
  lvSystemStorage.RowSelect := True;
end;

destructor TdxPSPDFExportDialogForm.Destroy;
begin
  FreeAndNil(FCertificateList);
  FreeAndNil(FSystemCertificate);
  FreeAndNil(FFileCertificate);
  inherited Destroy;
end;

procedure TdxPSPDFExportDialogForm.cbxMethodPropertiesChange(Sender: TObject);
begin
  licbAllowDocumentAssembly.Visible := cbxMethod.ItemIndex = 1;
  licbAllowPrintingHiResolution.Enabled := cbxMethod.ItemIndex = 1;
  SyncPrintingPermissions;
end;

procedure TdxPSPDFExportDialogForm.LoadTranslation;
begin
  Caption := cxGetResourceString(@sdxPDFDialogCaption);
  gbExportSettings.Caption := cxGetResourceString(@sdxPDFDialogExportSettings);
  gbSecuritySettings.Caption := cxGetResourceString
    (@sdxPDFDialogSecuritySettings);
  btnCancel.Caption := cxGetResourceString(@sdxBtnCancel);
  btnOk.Caption := cxGetResourceString(@sdxBtnOK);

  tbsExport.Caption := cxGetResourceString(@sdxPDFDialogTabExport);
  tbsDocInfo.Caption := cxGetResourceString(@sdxPDFDialogTabDocInfo);
  tbsPageRange.Caption := cxGetResourceString(@sdxPDFDialogTabPages);
  tbsSecurity.Caption := cxGetResourceString(@sdxPDFDialogTabSecurity);

  cbCompressed.Caption := cxGetResourceString(@sdxPDFDialogCompressed);
  cbEmbedFonts.Caption := cxGetResourceString(@sdxPDFDialogEmbedFonts);
  cbJpgCompress.Caption := cxGetResourceString(@sdxPDFDialogUseJPEGCompression);
  cbOpenAfterExport.Caption := cxGetResourceString
    (@sdxPDFDialogOpenAfterExport);
  cbUseCIDFonts.Caption := cxGetResourceString(@sdxPDFDialogUseCIDFonts);

  lbAuthor.Caption := cxGetResourceString(@sdxPDFDialogAuthor);
  lbCreator.Caption := cxGetResourceString(@sdxPDFDialogCreator);
  lbKeywords.Caption := cxGetResourceString(@sdxPDFDialogKeywords);
  lbMaxCompression.Caption := cxGetResourceString(@sdxPDFDialogMaxCompression);
  lbMaxQuality.Caption := cxGetResourceString(@sdxPDFDialogMaxQuality);
  lbSubject.Caption := cxGetResourceString(@sdxPDFDialogSubject);
  lbTitle.Caption := cxGetResourceString(@sdxPDFDialogTitle);

  lbDescription.Caption := cxGetResourceString(@sdxPrintDialogRangeLegend);
  rbtnAllPages.Caption := cxGetResourceString(@sdxPrintDialogAll);
  rbtnCurrentPage.Caption := cxGetResourceString(@sdxPrintDialogCurrentPage);
  rbtnPageRanges.Caption := cxGetResourceString(@sdxPrintDialogPages);

  lbUserPassword.Caption := cxGetResourceString
    (@sdxPDFDialogSecurityUserPassword);
  lbOwnerPassword.Caption := cxGetResourceString
    (@sdxPDFDialogSecurityOwnerPassword);
  lbMethod.Caption := cxGetResourceString(@sdxPDFDialogSecurityMethod);
  cbAllowPrinting.Caption := cxGetResourceString
    (@sdxPDFDialogSecurityAllowPrint);
  cbAllowChanging.Caption := cxGetResourceString
    (@sdxPDFDialogSecurityAllowChanging);
  cbAllowComments.Caption := cxGetResourceString
    (@sdxPDFDialogSecurityAllowComments);
  cbAllowContentCopying.Caption :=
    cxGetResourceString(@sdxPDFDialogSecurityAllowCopy);
  cbAllowDocumentAssembly.Caption :=
    cxGetResourceString(@sdxPDFDialogSecurityAllowDocumentAssemble);
  cbAllowPrintingHiResolution.Caption :=
    cxGetResourceString(@sdxPDFDialogSecurityAllowPrintHiResolution);

  tbsSignature.Caption := cxGetResourceString(@sdxPDFDialogTabSignature);
  lgSignatureSettings.Caption := cxGetResourceString
    (@sdxPDFDialogSignatureSettings);
  tbsSignatureCertificate.Caption :=
    cxGetResourceString(@sdxPDFDialogSignatureDigitalID);
  lrbSignatureUseCertificateFromSystemStore.Caption :=
    cxGetResourceString(@sdxPDFDialogSignatureUseCertificateFromSystemStore);
  lrbSignatureUseCertificateFromFile.Caption :=
    cxGetResourceString(@sdxPDFDialogSignatureUseCertificateFromFileStore);
  lipeSytemStorage.Caption := cxGetResourceString(@sdxPDFDialogSignatureIssuer);
  liteSignatureCertificateFileName.Caption :=
    cxGetResourceString(@sdxPDFDialogSignatureFileName);
  tbsSignatureDetails.Caption := cxGetResourceString
    (@sdxPDFDialogSignatureDetails);
  liteSignatureReason.Caption := cxGetResourceString
    (@sdxPDFDialogSignatureReason);
  liteSignatureLocation.Caption :=
    cxGetResourceString(@sdxPDFDialogSignatureLocation);
  liteSignatureContactInfo.Caption :=
    cxGetResourceString(@sdxPDFDialogSignatureContactInfo);
  btnSignatureViewCertificate.Caption :=
    cxGetResourceString(@sdxPDFDialogSignatureViewCertificate);
  lvSystemStorage.Columns[0].Caption :=
    cxGetResourceString(@sdxPDFDialogSignatureIssuerColumnCaption);
  lvSystemStorage.Columns[1].Caption :=
    cxGetResourceString(@sdxPDFDialogSignatureExpirationDateColumnCaption);
end;

procedure TdxPSPDFExportDialogForm.lrbSignatureUseCertificateFromFileClick
  (Sender: TObject);
begin
  lgSignatureSystemStorage.Enabled :=
    not lrbSignatureUseCertificateFromFile.Checked;
  lgSignatureFileStore.Enabled := not lgSignatureSystemStorage.Enabled;
  UpdateSignatureButtons;
end;

function TdxPSPDFExportDialogForm.CheckUserInput: Boolean;
begin
  Result := True;
  if lgSignatureSettings.ButtonOptions.CheckBox.Checked then
  begin
    if lrbSignatureUseCertificateFromFile.Checked then
    begin
      Result := FFileCertificate <> nil;
      if not Result then
        ShowInvalidFileNameMessage(teSignatureCertificateFileName.Text);
    end;

    if lrbSignatureUseCertificateFromSystemStore.Checked then
    begin
      Result := FSystemCertificate <> nil;
      if not Result then
        MessageWarning
          (cxGetResourceString(@sdxPDFDialogSignatureRequiredDigitalID));
    end;
  end;
end;

function TdxPSPDFExportDialogForm.IsCertificateVerificationLocked: Boolean;
begin
  Result := FCertificateVerificationLockCount <> 0;
end;

procedure TdxPSPDFExportDialogForm.lbSystemStorageClick(Sender: TObject);
begin
  UpdateSystemStorageCertificateCaption;
end;

procedure TdxPSPDFExportDialogForm.lcMainClick(Sender: TObject);
begin
  FIsSystemStoragePopupClosed := True;
end;

procedure TdxPSPDFExportDialogForm.OnSignatureFileNameBrowseButtonClockHandler
  (Sender: TObject; AButtonIndex: Integer);
var
  AOpenDialog: TOpenDialog;
begin
  if FSignatureFileNameBrowseButton.Index = AButtonIndex then
  begin
    AOpenDialog := TOpenDialog.Create(nil);
    try
      AOpenDialog.Title := cxGetResourceString
        (@sdxPDFDialogSignatureOpenDlgTitle);
      AOpenDialog.Filter := Format('%s (*.pfx)|*.PFX',
        [cxGetResourceString(@sdxPDFDialogSignatureOpenDlgDigitalIDFile)]);
      AOpenDialog.FilterIndex := 1;
      AOpenDialog.DefaultExt := 'pfx';
      AOpenDialog.FileName := teSignatureCertificateFileName.Text;
      if AOpenDialog.Execute then
        CheckCertificatePassword(AOpenDialog.FileName);
    finally
      AOpenDialog.Free;
      UpdateSignatureButtons;
    end;
  end;
end;

procedure TdxPSPDFExportDialogForm.
  lrbSignatureUseCertificateFromSystemStoreClick(Sender: TObject);
begin
  lgSignatureFileStore.Enabled :=
    not lrbSignatureUseCertificateFromSystemStore.Checked;
  lgSignatureSystemStorage.Enabled := not lgSignatureFileStore.Enabled;
  UpdateSignatureButtons;
end;

procedure TdxPSPDFExportDialogForm.lvSystemStorageClick(Sender: TObject);
begin
  UpdateSystemStorageCertificateCaption;
end;

procedure TdxPSPDFExportDialogForm.LoadOptions
  (AOptions: TdxPSPDFReportExportOptions);
begin
  teTitle.Text := AOptions.Title;
  teAuthor.Text := AOptions.Author;
  teSubject.Text := AOptions.Subject;
  teCreator.Text := AOptions.Creator;
  teKeywords.Text := AOptions.Keywords;

  PageRanges := AOptions.PageRangeInfo.PageRanges;
  edPageRanges.Text := AOptions.PageRangeInfo.PageIndexesAsString;
  cbOpenAfterExport.Checked := AOptions.OpenDocumentAfterExport;
  cbCompressed.Checked := AOptions.CompressStreams;
  cbCompressed.Enabled := dxPDFCanCompressStreams;
  cbEmbedFonts.Checked := AOptions.EmbedFonts;
  tbJpgCompression.Position := AOptions.JPEGQuality;
  cbJpgCompress.Checked := AOptions.UseJPEGCompression;
  cbJpgCompress.Enabled := dxPDFCanUseJPEGCompression;
  cbUseCIDFonts.Checked := AOptions.UseCIDFonts;

  LoadSecurityOptions(AOptions.SecurityOptions);
  LoadSignatureOptions(AOptions.SignatureOptions);
  cbJpgCompressClick(nil);
  rbtnPageRangesClick(nil);
end;

procedure TdxPSPDFExportDialogForm.SaveOptions
  (AOptions: TdxPSPDFReportExportOptions);
begin
  AOptions.Title := teTitle.Text;
  AOptions.Author := teAuthor.Text;
  AOptions.Subject := teSubject.Text;
  AOptions.Creator := teCreator.Text;
  AOptions.Keywords := teKeywords.Text;

  AOptions.EmbedFonts := cbEmbedFonts.Checked;
  AOptions.UseCIDFonts := cbUseCIDFonts.Checked;
  AOptions.CompressStreams := cbCompressed.Checked;
  AOptions.UseJPEGCompression := cbJpgCompress.Checked;
  AOptions.OpenDocumentAfterExport := cbOpenAfterExport.Checked;
  AOptions.JPEGQuality := tbJpgCompression.Position;

  AOptions.PageRangeInfo.PageRanges := PageRanges;
  AOptions.PageRangeInfo.PageIndexesAsString := edPageRanges.Text;

  SaveSecurityOptions(AOptions.SecurityOptions);
  SaveSignatureOptions(AOptions.SignatureOptions);
end;

procedure TdxPSPDFExportDialogForm.LoadSecurityOptions
  (AOptions: TdxPSPDFSecurityOptions);
begin
  cbxMethod.ItemIndex := Ord(AOptions.KeyLength);
  cbAllowPrinting.Checked := pdaPrint in AOptions.AllowActions;
  cbAllowComments.Checked := pdaComment in AOptions.AllowActions;
  cbAllowChanging.Checked := pdaContentEdit in AOptions.AllowActions;
  cbAllowContentCopying.Checked := pdaContentCopy in AOptions.AllowActions;
  cbAllowDocumentAssembly.Checked := pdaDocumentAssemble
    in AOptions.AllowActions;
  cbAllowPrintingHiResolution.Checked := pdaPrintHighResolution
    in AOptions.AllowActions;
  edOwnerPassword.Text := AOptions.OwnerPassword;
  edUserPassword.Text := AOptions.UserPassword;
  gbSecuritySettings.ButtonOptions.CheckBox.Checked := AOptions.Enabled;
end;

procedure TdxPSPDFExportDialogForm.LoadSignatureOptions
  (AOptions: TdxPSPDFSignatureOptions);
begin
  teSignatureLocation.Text := AOptions.Location;
  teSignatureReason.Text := AOptions.Reason;
  teSignatureContactInfo.Text := AOptions.ContactInfo;
  UserCertificate := AOptions.Certificate;
  if (FUserCertificate <> nil) and (FUserCertificate.FileName <> '') then
  begin
    LockCertificateChecking;
    FileCertificate := AOptions.Certificate;
    teSignatureCertificateFileName.Text := FUserCertificate.FileName;
    UnlockCertificateChecking;
  end;
  FSignatureFileNameBrowseButton := teSignatureCertificateFileName.Properties.
    Buttons.Add;
  FSignatureFileNameBrowseButton.Kind := bkEllipsis;
  FSignatureFileNameBrowseButton.Visible := True;
  teSignatureCertificateFileName.Properties.OnButtonClick :=
    OnSignatureFileNameBrowseButtonClockHandler;

  PopulateCertificateList;
  lgSignatureSettings.ButtonOptions.CheckBox.Checked := AOptions.Enabled;
  lrbSignatureUseCertificateFromFile.Checked :=
    teSignatureCertificateFileName.Text <> '';
  lrbSignatureUseCertificateFromSystemStore.Checked :=
    not lrbSignatureUseCertificateFromFile.Checked;
  TcxPopupEditPropertiesAccess(peSytemStorage.Properties).HideCursor := True;
  UpdateSignatureButtons;

end;

procedure TdxPSPDFExportDialogForm.SaveSecurityOptions
  (AOptions: TdxPSPDFSecurityOptions);
var
  AAllowActions: TdxPSPDFDocumentActions;
begin
  AAllowActions := [];
  AOptions.Enabled := gbSecuritySettings.ButtonOptions.CheckBox.Checked;
  AOptions.KeyLength := TdxPSPDFEncryptKeyLength(cbxMethod.ItemIndex);
  if cbAllowPrinting.Checked then
    Include(AAllowActions, pdaPrint);
  if cbAllowComments.Checked then
    Include(AAllowActions, pdaComment);
  if cbAllowChanging.Checked then
    Include(AAllowActions, pdaContentEdit);
  if cbAllowContentCopying.Checked then
    Include(AAllowActions, pdaContentCopy);
  if cbAllowDocumentAssembly.Checked then
    Include(AAllowActions, pdaDocumentAssemble);
  if cbAllowPrintingHiResolution.Checked then
    Include(AAllowActions, pdaPrintHighResolution);
  AOptions.AllowActions := AAllowActions;
  AOptions.OwnerPassword := edOwnerPassword.Text;
  AOptions.UserPassword := edUserPassword.Text;
end;

procedure TdxPSPDFExportDialogForm.SaveSignatureOptions
  (AOptions: TdxPSPDFSignatureOptions);
begin
  AOptions.Enabled := lgSignatureSettings.ButtonOptions.CheckBox.Checked;
  AOptions.Location := teSignatureLocation.Text;
  AOptions.Reason := teSignatureReason.Text;
  AOptions.ContactInfo := teSignatureContactInfo.Text;
  AOptions.Certificate := ActualCertificate;
end;

function TdxPSPDFExportDialogForm.GetPageRanges: TdxPageRanges;
begin
  Result := prAll;
  if rbtnPageRanges.Checked then
    Result := prRange;
  if rbtnCurrentPage.Checked then
    Result := prCurrent;
end;

procedure TdxPSPDFExportDialogForm.SetPageRanges(AValue: TdxPageRanges);
begin
  rbtnAllPages.Checked := AValue = prAll;
  rbtnPageRanges.Checked := AValue = prRange;
  rbtnCurrentPage.Checked := AValue = prCurrent;
end;

procedure TdxPSPDFExportDialogForm.FilterCertificateList
  (AList: TdxX509CertificateList);
begin
end;

function TdxPSPDFExportDialogForm.GetActualCertificate: TdxX509Certificate;
begin
  if lrbSignatureUseCertificateFromSystemStore.Checked then
    Result := SystemCertificate
  else
    Result := FileCertificate;
end;

procedure TdxPSPDFExportDialogForm.SetFileCertificate
  (AValue: TdxX509Certificate);
begin
  if FFileCertificate <> AValue then
  begin
    FreeAndNil(FFileCertificate);
    FFileCertificate := TdxX509Certificate.Create(AValue);
  end;
end;

procedure TdxPSPDFExportDialogForm.SetUserCertificate
  (AValue: TdxX509Certificate);
begin
  if FUserCertificate <> AValue then
  begin
    FreeAndNil(FUserCertificate);
    FUserCertificate := TdxX509Certificate.Create(AValue);
  end;
end;

procedure TdxPSPDFExportDialogForm.SetSystemCertificate
  (AValue: TdxX509Certificate);
begin
  if FSystemCertificate <> AValue then
  begin
    FreeAndNil(FSystemCertificate);
    FSystemCertificate := TdxX509Certificate.Create(AValue);
  end;
end;

procedure TdxPSPDFExportDialogForm.
  teSignatureCertificateFileNamePropertiesEditValueChanged(Sender: TObject);
begin
  if (FFileCertificate <> nil) and
    (FFileCertificate.FileName <> teSignatureCertificateFileName.Text) then
    FreeAndNil(FFileCertificate);
  CheckCertificatePassword(teSignatureCertificateFileName.Text);
  UpdateSignatureButtons;
end;

procedure TdxPSPDFExportDialogForm.CheckCertificatePassword
  (const AFileName: string);

  function CheckFileName: Boolean;
  begin
    Result := AFileName <> '';
    if Result then
    begin
      Result := dxPSUtl.ValidateFileName(AFileName) and FileExists(AFileName);
      if not Result then
        ShowInvalidFileNameMessage(AFileName);
    end;
  end;

var
  ACertificateToVerify: TdxX509Certificate;
  APassword: string;
begin
  if not IsCertificateVerificationLocked and CheckFileName and
    TdxX509CertificatePasswordDialogForm.Execute(Self, nil, APassword) then
    try
      LockCertificateChecking;
      try
        ACertificateToVerify := nil;
        ACertificateToVerify := TdxX509Certificate.Create(AFileName, APassword);
        teSignatureCertificateFileName.Text := AFileName;
        FileCertificate := ACertificateToVerify;
      except
        FreeAndNil(ACertificateToVerify);
        MessageWarning
          (cxGetResourceString
          (@sdxPDFDialogSignatureRequiredDigitalIDPassword));
      end;
    finally
      FreeAndNil(ACertificateToVerify);
      UnlockCertificateChecking;
      UpdateSignatureButtons;
    end;
end;

procedure TdxPSPDFExportDialogForm.LockCertificateChecking;
begin
  Inc(FCertificateVerificationLockCount);
end;

procedure TdxPSPDFExportDialogForm.PopulateCertificateList;

  procedure LoadCertificateFromStorage(AName: TdxX509StoreName;
    ALocation: TdxX509StoreLocation);
  var
    AStorage: TdxX509Store;
  begin
    FCertificateList.Clear;
    AStorage := TdxX509Store.Create(AName, ALocation);
    try
      AStorage.Open;
      AStorage.Certificates.CopyTo(FCertificateList);
    finally
      AStorage.Free;
    end;
  end;

  procedure AddUserCertificate(out AUserCertificateIndex: Integer);
  var
    I: Integer;
  begin
    AUserCertificateIndex := -1;
    if FUserCertificate <> nil then
    begin
      for I := 0 to FCertificateList.Count - 1 do
        if SameText(FCertificateList[I].SerialNumber,
          FUserCertificate.SerialNumber) then
        begin
          AUserCertificateIndex := I;
          Break;
        end;
      if AUserCertificateIndex = -1 then
      begin
        FCertificateList.Insert(0, FUserCertificate);;
        AUserCertificateIndex := 0;
      end;
    end;
  end;

  procedure RemoveUnsupportedCertificates(var AUserCertificateIndex: Integer);
  var
    ACertificate: TdxX509Certificate;
    AList: TdxX509CertificateList;
    I: Integer;
  begin
    AList := TdxX509CertificateList.Create;
    try
      FCertificateList.CopyTo(AList);
      FCertificateList.Clear;
      for I := 0 to AList.Count - 1 do
      begin
        ACertificate := AList[I];
        if IsSupportedCertificate(ACertificate) then
        begin
          FCertificateList.Add(TdxX509Certificate.Create(ACertificate));
          if (I = AUserCertificateIndex) and
            (ACertificate.SerialNumber = FUserCertificate.SerialNumber) then
            AUserCertificateIndex := FCertificateList.Count - 1;
        end;
      end;
    finally
      AList.Free;
    end;
  end;

var
  AUserCertificateIndex: Integer;
  ACertificate: TdxX509Certificate;
  AItem: TListItem;
begin
  FCertificateList.Clear;
  LoadCertificateFromStorage(snMy, slCurrentUser);
  AddUserCertificate(AUserCertificateIndex);
  RemoveUnsupportedCertificates(AUserCertificateIndex);

  for ACertificate in FCertificateList do
  begin
    AItem := lvSystemStorage.Items.Add;
    AItem.Caption := ACertificate.IssuedBy;
    AItem.SubItems.Add(cxDateToStr(ACertificate.ValidTo));
    AItem.Data := Pointer(ACertificate);
  end;
  lvSystemStorage.ItemIndex := IfThen(lvSystemStorage.Items.Count > 0,
    IfThen(AUserCertificateIndex <> -1, AUserCertificateIndex), -1);
  UpdateSystemStorageCertificateCaption;
end;

procedure TdxPSPDFExportDialogForm.ShowInvalidFileNameMessage
  (const AFileName: string);
begin
  MessageWarning(Format(cxGetResourceString(@sdxInvalidFileName), [AFileName]))
end;

procedure TdxPSPDFExportDialogForm.UnlockCertificateChecking;
begin
  Dec(FCertificateVerificationLockCount);
end;

procedure TdxPSPDFExportDialogForm.UpdateSignatureButtons;
begin
  btnSignatureViewCertificate.Enabled :=
    lrbSignatureUseCertificateFromSystemStore.Checked and
    (FCertificateList.Count > 0) or lrbSignatureUseCertificateFromFile.Checked
    and (teSignatureCertificateFileName.Text <> '');
end;

procedure TdxPSPDFExportDialogForm.UpdateSystemStorageCertificateCaption;
begin
  if lvSystemStorage.ItemIndex > -1 then
  begin
    SystemCertificate := TdxX509Certificate
      (lvSystemStorage.Items[lvSystemStorage.ItemIndex].Data);
    peSytemStorage.Text := SystemCertificate.IssuedTo;
  end;
  peSytemStorage.DroppedDown := False;
end;

procedure TdxPSPDFExportDialogForm.SyncPrintingPermissions;
begin
  if cbxMethod.ItemIndex = 0 then
    cbAllowPrintingHiResolution.Checked := cbAllowPrinting.Checked;
end;

function TdxPSPDFExportDialogForm.IsSupportedCertificate
  (ACertificate: TdxX509Certificate): Boolean;
begin
  Result := (kufDigitalSignature in ACertificate.KeyUsage) and
    ACertificate.HasPrivateKey;
end;

procedure TdxPSPDFExportDialogForm.btnSignatureViewCertificateClick
  (Sender: TObject);
begin
  if lrbSignatureUseCertificateFromFile.Checked and (FileCertificate = nil) then
    CheckCertificatePassword(teSignatureCertificateFileName.Text);
  if ActualCertificate <> nil then
    dxX509DisplayCertificate(ActualCertificate, Handle);
end;

procedure TdxPSPDFExportDialogForm.cbAllowPrintingPropertiesEditValueChanged
  (Sender: TObject);
begin
  SyncPrintingPermissions;
end;

procedure TdxPSPDFExportDialogForm.cbJpgCompressClick(Sender: TObject);
begin
  lbMaxQuality.Enabled := cbJpgCompress.Enabled and cbJpgCompress.Checked;
  lbMaxCompression.Enabled := cbJpgCompress.Enabled and cbJpgCompress.Checked;
  tbJpgCompression.Enabled := cbJpgCompress.Enabled and cbJpgCompress.Checked;
end;

procedure TdxPSPDFExportDialogForm.rbtnPageRangesClick(Sender: TObject);
begin
  edPageRanges.Enabled := rbtnPageRanges.Checked;
end;

procedure TdxPSPDFExportDialogForm.edPageRangesKeyPress(Sender: TObject;
  var Key: Char);
begin
  if Ord(Key) <> VK_BACK then
  begin
    case Key of
      '0' .. '9', cPageRangeSeparator, cPageSeparator:
        ;
    else
      Key := #0;
    end;
  end;
end;

procedure TdxPSPDFExportDialogForm.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if ModalResult = mrOk then
    CanClose := CheckUserInput;
end;

procedure TdxPSPDFExportDialogForm.OnSecuritySettingsChangeHandler
  (Sender: TObject);
begin
  lrbSignatureUseCertificateFromSystemStoreClick(nil);
  lrbSignatureUseCertificateFromFileClick(nil);
end;

procedure TdxPSPDFExportDialogForm.peSytemStorageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FIsSystemStoragePopupClosed then
    peSytemStorage.DroppedDown := not peSytemStorage.DroppedDown
end;

procedure TdxPSPDFExportDialogForm.peSytemStorageMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FIsSystemStoragePopupClosed := True;
end;

procedure TdxPSPDFExportDialogForm.peSytemStoragePropertiesCloseUp
  (Sender: TObject);
begin
  FIsSystemStoragePopupClosed := False;
end;

end.
