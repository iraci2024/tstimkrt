
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//  Certificates Frame
//////////////////////////////////////////////////

{$I SB.inc}

unit ScCertificatesFrame;

interface

uses
{$IFDEF MSWINDOWS}
  Windows, Messages,
{$ENDIF}
  Graphics, Controls, Forms, Dialogs, StdCtrls, ExtCtrls,
  Classes, SysUtils, Buttons,
{$IFDEF FPC}
  LResources,
{$ENDIF}
  ScUtils, ScFunctions, ScBridge, ScTabEditor, ScColFrame;

type
  TScCertificateEdit = class
  public
    class function ImportCertificate(Cert: TScCertificate): boolean;
    class function ExportCertificate(Cert: TScCertificate): boolean;
  end;

  TScCertificateItem = class(TCollectionItem)
  protected
    FCert: TScCertificate;
    FIsNew: boolean;
    procedure Init(Cert: TScCertificate; IsNew: boolean);
  end;

  TScCertificatesFrame = class(TScColFrame)
    edCertName: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    Label2: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    cbIsPrivate: TCheckBox;
    btImportKey: TButton;
    btExportPrivate: TButton;
    btExportPublic: TButton;
    mmProperties: TMemo;
    Label4: TLabel;
    edAlgorithm: TEdit;
    edBitCount: TEdit;
    btImportCert: TButton;
    btExportCert: TButton;
    btNew: TButton;
    btDelete: TButton;
    procedure btImportKeyClick(Sender: TObject);
    procedure btExportPrivateClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure btNewClick(Sender: TObject);
    procedure btExportPublicClick(Sender: TObject);
    procedure lbItemNameDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure btImportCertClick(Sender: TObject);
    procedure btExportCertClick(Sender: TObject);
    procedure edCertNameExit(Sender: TObject);

  protected
    FItems: TCollection;

    function GetCurItem: TScCertificateItem;
    function GetItems: TCollection; override;
    function GetItemName(Item: TCollectionItem): string; override;
    procedure ItemToControls(Item: TCollectionItem); override;
    procedure ControlsToItem(Item: TCollectionItem); override;
    procedure UpdateControlsState; override;
    procedure SetCertName;

    procedure DoActivate; override;
    procedure DoFinish; override;
    property CurItem: TScCertificateItem read GetCurItem;
  end;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

uses
  TypInfo, ScAlgorithmSupport, ScCertificateExts, ScSSHUtils, ScConsts,
  ScKeysFrame;

{ TScCertificateEdit }

class function TScCertificateEdit.ImportCertificate(Cert: TScCertificate): boolean;
var
  FileOpen: TOpenDialog;
begin
  Assert(Cert <> nil);

  Result := False;
  FileOpen := TOpenDialog.Create(Application);
  try
    FileOpen.Title := 'Import certificate';
    FileOpen.Filename := '';
    FileOpen.Filter := 'All formats (*.pem;*.crt;*.cer)|*.pem;*.crt;*.cer|PEM format (*.pem;*.crt)|*.pem;*.crt|DER format (*.cer)|*.cer|All files (*.*)|*.*';
    FileOpen.Options := FileOpen.Options + [ofPathMustExist, ofFileMustExist];

    if FileOpen.Execute then begin
      Result := True;
      Cert.ImportFrom(FileOpen.Filename);
    end;
  finally
    FileOpen.Free;
  end;
end;

class function TScCertificateEdit.ExportCertificate(Cert: TScCertificate): boolean;
var
  FileSave: TSaveDialog;
  CertFormat: TScCertificateEncoding;
  Ext: string;
begin
  Assert(Cert <> nil);

  Result := False;
  FileSave := TSaveDialog.Create(Application);
  try
    FileSave.Title := 'Export certificate to...';
    FileSave.Filename := '';
    FileSave.Filter := 'PEM format (*.pem)|*.pem|DER format (*.cer)|*.cer';
    FileSave.Options := FileSave.Options + [ofPathMustExist];
    if FileSave.Execute then begin
      CertFormat := cfPEM;
      Ext := '';

      case FileSave.FilterIndex of
        1: begin
          CertFormat := cfPEM;
          Ext := '.pem';
        end;
        2: begin
          CertFormat := cfCER;
          Ext := '.cer';
        end;
      else
        Assert(False);
      end;

      Cert.ExportTo(ChangeFileExt(FileSave.Filename, Ext), CertFormat);
      Result := True;
    end;
  finally
    FileSave.Free;
  end;
end;

{ TScCertificateItem }

procedure TScCertificateItem.Init(Cert: TScCertificate; IsNew: boolean);
begin
  Assert(Cert <> nil);
  FCert := Cert;
  FIsNew := IsNew;
end;

{ TScCertificatesFrame }

function TScCertificatesFrame.GetItems: TCollection;
begin
  Result := FItems;
end;

function TScCertificatesFrame.GetItemName(Item: TCollectionItem): string;
begin
  Result := TScCertificateItem(Item).FCert.CertName;
end;

function TScCertificatesFrame.GetCurItem: TScCertificateItem;
begin
  Assert(lbItemName.ItemIndex <> - 1);
  if FOldItemIndex = -1 then
    Result := Items.Items[lbItemName.ItemIndex] as TScCertificateItem
  else
    Result := Items.Items[FOldItemIndex] as TScCertificateItem;
end;

procedure TScCertificatesFrame.ItemToControls(Item: TCollectionItem);
var
  Cert: TScCertificate;
  Ext: TScCertificateExtension;
  FingerPrint: string;
  ReadOnly: boolean;
  i: Integer;
begin
  Cert := (Item as TScCertificateItem).FCert;
{
  try
    Cert.Ready := True;
  except
    on E: Exception do
      if E.Message <> SObjectNotFound then
        ApplicationHandleException(E);
  end;
}
  edCertName.Text := Cert.CertName;
  mmProperties.Lines.Clear;

  if Cert.Ready then begin
    mmProperties.Lines.Add('Version: v' + IntToStr(Cert.Version));
    mmProperties.Lines.Add('SerialNumber: ' + Cert.SerialNumber);
    mmProperties.Lines.Add('SignatureAlgorithm: ' + Cert.SignatureAlgorithm.Algorithm.FriendlyName);
    mmProperties.Lines.Add('NotBefore: ' + DateTimeToStr(Cert.NotBefore));
    mmProperties.Lines.Add('NotAfter: ' + DateTimeToStr(Cert.NotAfter));
    mmProperties.Lines.Add('Issuer: ' + Cert.IssuerName.ToString);
    mmProperties.Lines.Add('Subject: ' + Cert.SubjectName.ToString);
    Cert.GetFingerprint(haSHA1, FingerPrint);
    mmProperties.Lines.Add('Thumbprint with SHA1: ' + FingerPrint);

    if Cert.Extensions.Count > 0 then
      mmProperties.Lines.Add('Extensions:');

    for i := 0 to Cert.Extensions.Count - 1 do begin
      Ext := Cert.Extensions.Extensions[i];
      mmProperties.Lines.Add('  ' + Ext.Oid.FriendlyName + ':');
      mmProperties.Lines.Add('    ' + Ext.ToString);
    end;
    mmProperties.SelStart := 0;
    mmProperties.SelLength := 0;

    try
      edAlgorithm.Text := CipherFactory.PublicKeyAlgorithmToName(Cert.Key.Algorithm);
      edBitCount.Text := IntToStr(Cert.Key.BitCount);
      cbIsPrivate.Checked := Cert.Key.IsPrivate;
    except
      edAlgorithm.Text := 'Unknown algorithm';
      edBitCount.Text := '';
      cbIsPrivate.Checked := False;
    end;
  end
  else begin
    edAlgorithm.Text := '';
    edBitCount.Text := '';
    cbIsPrivate.Checked := False;
  end;

  ReadOnly := (Editor.Component as TScStorage).ReadOnly;
  mmProperties.Enabled := Cert.Ready;
  edAlgorithm.Enabled := Cert.Ready;
  edBitCount.Enabled := Cert.Ready;
  btImportCert.Enabled := not Cert.Ready and not ReadOnly;
  btExportCert.Enabled := Cert.Ready;

  try
    btImportKey.Enabled := Cert.Ready and not ReadOnly;
    btExportPublic.Enabled := Cert.Ready;
    btExportPrivate.Enabled := Cert.Ready and Cert.Key.IsPrivate;
  except
    btImportKey.Enabled := False;
    btExportPublic.Enabled := False;
    btExportPrivate.Enabled := False;
  end;
  btDelete.Enabled := not ReadOnly;
end;

procedure TScCertificatesFrame.ControlsToItem(Item: TCollectionItem);
begin
  SetCertName;
end;

procedure TScCertificatesFrame.UpdateControlsState;
begin
  PanelItem.Enabled := Enabled;
  Enabled := True;

  btNew.Enabled := not (Editor.Component as TScStorage).ReadOnly;
  edCertName.ReadOnly := (Editor.Component as TScStorage).ReadOnly;
  cbIsPrivate.Enabled := False;

  if Items.Count = 0 then begin
    mmProperties.Enabled := False;
    edAlgorithm.Enabled := False;
    edBitCount.Enabled := False;
    btImportCert.Enabled := False;
    btExportCert.Enabled := False;
    btImportKey.Enabled := False;
    btExportPrivate.Enabled := False;
    btExportPublic.Enabled := False;
    btDelete.Enabled := False;
  end;

  inherited;
end;

procedure TScCertificatesFrame.DoActivate;
var
  Storage: TScStorage;
  List: TStringList;
  Cert: TScCertificate;
  ErrorMsg: string;
  i: Integer;
begin
  FItems := TCollection.Create(TScCertificateItem);

  Storage := Editor.Component as TScStorage;
  try
    Storage.Certificates.Refresh;
  except
    on E: EScError do begin
      if E.Message = SEmptyStorageName then begin
        ErrorMsg := 'For this CertProviderType the name of the store cannot be empty.'#13#10 +
          'Specify the CertStoreName property.';
        E.Message := ErrorMsg;
      end;

      raise;
    end;
  end;

  List := TStringList.Create;
  try
    for i := 0 to Storage.Certificates.Count - 1 do
      List.AddObject(Storage.Certificates.Certificates[i].CertName, Storage.Certificates.Certificates[i]);

    List.Sort;

    for i := 0 to List.Count - 1 do begin
      Cert := List.Objects[i] as TScCertificate;
      (FItems.Add as TScCertificateItem).Init(Cert, False);
    end;
  finally
    List.Free;
  end;

  inherited;
end;

procedure TScCertificatesFrame.DoFinish;
var
  i: Integer;
begin
  inherited;

  if not TScStorage(Editor.Component).ReadOnly then
    TScStorage(Editor.Component).Certificates.Flush;

  for i := 0 to Items.Count - 1 do
    if TScCertificateItem(Items.Items[i]).FIsNew then begin
      if MessageDlg('You have created new certificates, which were not initialized and will be deleted.'#13#10 +
        'Do you want to close the Storage editor?', mtWarning, [mbOk, mbCancel], 0) = mrCancel then
        Abort;
      Break;
    end;

  TScStorageUtils.Invalidate(TScStorage(Editor.Component));
  FreeAndNil(FItems);
end;

procedure TScCertificatesFrame.btImportKeyClick(Sender: TObject);
begin
  SetCertName;

  try
    if not TScKeyEdit.ImportKey(CurItem.FCert.Key) then
      CurItem.FCert.Key.Ready := True;
  finally
    SelectItem;
    lbItemName.Repaint;
  end;
end;

procedure TScCertificatesFrame.btExportPrivateClick(Sender: TObject);
begin
  Assert(CurItem.FCert.Ready);
  TScKeyEdit.ExportPrivateKey(CurItem.FCert.Key);
end;

procedure TScCertificatesFrame.btExportPublicClick(Sender: TObject);
begin
  Assert(CurItem.FCert.Ready);
  TScKeyEdit.ExportPublicKey(CurItem.FCert.Key);
end;

procedure TScCertificatesFrame.btDeleteClick(Sender: TObject);
begin
  SetCertName;

  if MessageDlg('Do you want delete "' + CurItem.FCert.CertName + '" certificate?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  try
    TScStorage(Editor.Component).Certificates.Remove(CurItem.FCert);
    CurItem.FCert.Free;
    CurItem.Free;
  finally
    InitItems;
  end;
end;

procedure TScCertificatesFrame.btNewClick(Sender: TObject);

  function IndexOf(const AName: string): Integer;
  begin
    for Result := 0 to FItems.Count - 1 do
      if AnsiCompareText(TScCertificateItem(FItems.Items[Result]).FCert.CertName, AName) = 0 then
        Exit;
    Result := -1;
  end;

var
  Cert: TScCertificate;
  NewCertname: string;
  CertItem: TScCertificateItem;
  i: Integer;
begin
  StoreItem;

  i := 0;
  repeat
    Inc(i);
    NewCertname := 'cert' + IntToStr(i);
  until IndexOf(NewCertname) = -1;

  Cert := TScCertificate.Create(TScStorage(Editor.Component).Certificates);
  Cert.CertName := NewCertname;
  CertItem := FItems.Add as TScCertificateItem;
  CertItem.Init(Cert, True);

  lbItemName.Items.Add(GetItemName(CertItem));
  lbItemName.ItemIndex := lbItemName.Items.Count - 1;

  SelectItem;
  lbItemName.Repaint;
end;

procedure TScCertificatesFrame.SetCertName;
begin
  if AnsiCompareText(edCertName.Text, CurItem.FCert.CertName) <> 0 then begin
    try
      CurItem.FCert.CertName := edCertName.Text;
      lbItemName.Items[CurItem.Index] := GetItemName(CurItem);
    except
      if Page.PageControl.ActivePage = Page then
        edCertName.SetFocus;
        
      raise;
    end;
  end;
end;

procedure TScCertificatesFrame.lbItemNameDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  with (Control as TListBox).Canvas do begin
    if (Items <> nil) and (Items.Items[Index] <> nil) then
      if TScCertificateItem(Items.Items[Index]).FIsNew then
        Font.Style := Font.Style + [fsBold]
      else
        Font.Style := Font.Style - [fsBold];

    FillRect(Rect);
    TextOut(Rect.Left + 2, Rect.Top, (Control as TListBox).Items[Index]);
  end;
end;

procedure TScCertificatesFrame.btImportCertClick(Sender: TObject);
begin
  SetCertName;
  Assert(not CurItem.FCert.Ready);

  try
    if TScCertificateEdit.ImportCertificate(CurItem.FCert) then
      CurItem.FIsNew := False;
  finally
    SelectItem;
    lbItemName.Repaint;
  end;
end;

procedure TScCertificatesFrame.btExportCertClick(Sender: TObject);
begin
  Assert(CurItem.FCert.Ready);
  TScCertificateEdit.ExportCertificate(CurItem.FCert);
end;

procedure TScCertificatesFrame.edCertNameExit(Sender: TObject);
begin
  SetCertName;
end;

end.

