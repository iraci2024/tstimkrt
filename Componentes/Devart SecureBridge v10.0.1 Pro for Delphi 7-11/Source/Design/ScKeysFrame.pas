
//////////////////////////////////////////////////
//  SecureBridge Components
//  Copyright © 2007-2021 Devart. All right reserved.
//  Keys Frame
//////////////////////////////////////////////////

{$I SB.inc}

unit ScKeysFrame;

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
  ScUtils, ScBridge, ScSSHUtils, ScAlgorithmSupport, ScTabEditor, ScColFrame, TypInfo;

type
  TScKeyEdit = class
  private
    class function ExportKey(Key: TScKey; const Filter: string; PublicOnly: boolean): boolean;
  public
    class function ImportKey(Key: TScKey): boolean;
    class function ExportPublicKey(Key: TScKey): boolean;
    class function ExportPrivateKey(Key: TScKey): boolean;
  end;

  TScKeyItem = class(TCollectionItem)
  protected
    FKey: TScKey;
    FECName: TScECName;
    FAlgorithm: TScAsymmetricAlgorithm;
    FBitCount: Integer;
    FChanged: boolean;
  public
    procedure Init(Key: TScKey; PrevKeyItem: TScKeyItem = nil);

    function GetReady: boolean;
    function GetAlgorithm: TScAsymmetricAlgorithm;
    function GetBitCount: integer;
    function GetECName: TScECName;
    function GetIsPrivate: boolean;
    function GetFingerprint: string;
  end;

  TScKeysFrame = class(TScColFrame)
    edKeyName: TEdit;
    edFingerPrint: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    lbKeyData: TLabel;
    Label5: TLabel;
    cbIsPrivate: TCheckBox;
    cbAlgorithm: TComboBox;
    cbBitCount: TComboBox;
    Label2: TLabel;
    btImport: TButton;
    btExportPrivate: TButton;
    btExportPublic: TButton;
    btGenerate: TButton;
    btNew: TButton;
    btDelete: TButton;
    cbECName: TComboBox;
    procedure btImportClick(Sender: TObject);
    procedure btExportPrivateClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure lbItemNameDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cbAlgorithmChange(Sender: TObject);
    procedure btGenerateClick(Sender: TObject);
    procedure btNewClick(Sender: TObject);
    procedure btExportPublicClick(Sender: TObject);
    procedure cbBitCountKeyPress(Sender: TObject; var Key: Char);
    procedure edKeyNameExit(Sender: TObject);

  protected
    FItems: TCollection;

    function GetCurItem: TScKeyItem;
    procedure ReGenerateAll;
    function GetItems: TCollection; override;
    function GetItemName(Item: TCollectionItem): string; override;
    procedure ItemToControls(Item: TCollectionItem); override;
    procedure ControlsToItem(Item: TCollectionItem); override;
    procedure UpdateControlsState; override;
    procedure SetKeyName;

    procedure DoActivate; override;
    procedure DoFinish; override;
    property CurItem: TScKeyItem read GetCurItem;
  end;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ELSE}
{$R *.lfm}
{$ENDIF}

const
  WrongKeyFormat = 'Wrong key format or wrong password!';

{ TScKeyEdit }

class function TScKeyEdit.ImportKey(Key: TScKey): boolean;
var
  FileOpen: TOpenDialog;
  Comment: string;
begin
  Assert(Key <> nil);

  Result := False;
  FileOpen := TOpenDialog.Create(Application);
  try
    FileOpen.Title := 'Import key';
    FileOpen.Filename := '';
    FileOpen.Filter := 'All formats |*.ssl;*.pem;*.ietf;*.pub;*.ietfpub|OpenSSL format (*.ssl)|*.ssl|PKCS8 format (*.pem)|*.pem|IETF format (*.ietf)|*.ietf|Public key (*.pub)|*.pub|Public IETF key (*.ietfpub)|*.ietfpub|PuTTY key (*.ppk)|*.ppk|All files (*.*)|*.*';
    FileOpen.Options := FileOpen.Options + [ofPathMustExist, ofFileMustExist];

    if FileOpen.Execute then begin
      Result := True;
      Key.ImportFrom(FileOpen.Filename, '', Comment);
    end;
  finally
    FileOpen.Free;
  end;
end;

class function TScKeyEdit.ExportKey(Key: TScKey; const Filter: string; PublicOnly: boolean): boolean;
var
  FileSave: TSaveDialog;
  KeyFormat: TScKeyFormat;
  Ext: string;
begin
  Assert(Key <> nil);

  Result := False;
  FileSave := TSaveDialog.Create(Application);
  try
    FileSave.Title := 'Export key to...';
    FileSave.Filename := '';
    FileSave.Filter := Filter;
    FileSave.Options := FileSave.Options + [ofPathMustExist];
    if FileSave.Execute then begin
      KeyFormat := kfDefault;
      Ext := '';

      if not PublicOnly then
        case FileSave.FilterIndex of
          1: begin
            KeyFormat := kfDefault;
            Ext := '.ssl';
          end;
          2: begin
            KeyFormat := kfPKCS8;
            Ext := '.pem';
          end;
          3: begin
            KeyFormat := kfIETF;
            Ext := '.ietf';
          end;
          else
            Assert(False);
        end
      else
        case FileSave.FilterIndex of
          1: begin
            KeyFormat := kfDefault;
            Ext := '.pub';
          end;
          2: begin
            KeyFormat := kfIETF;
            Ext := '.ietfpub';
          end;
          else
            Assert(False);
        end;

      Key.ExportTo(ChangeFileExt(FileSave.Filename, Ext), PublicOnly, '', saTripleDES_cbc, KeyFormat);
      Result := True;
    end;
  finally
    FileSave.Free;
  end;
end;

class function TScKeyEdit.ExportPublicKey(Key: TScKey): boolean;
var
  Filter: string;
begin
  Filter := 'OpenSSL format (*.pub)|*.pub|IETF format (*.ietfpub)|*.ietfpub';
  Result := ExportKey(Key, Filter, True);
end;

class function TScKeyEdit.ExportPrivateKey(Key: TScKey): boolean;
var
  Filter: string;
begin
  Filter := 'OpenSSL format (*.ssl)|*.ssl|PKCS8 format (*.pem)|*.pem|IETF format (*.ietf)|*.ietf';
  Result := ExportKey(Key, Filter, False);
end;

{ TScKeyItem }

function TScKeyItem.GetReady: boolean;
begin
  Result := not FChanged and FKey.Ready;
end;

function TScKeyItem.GetAlgorithm: TScAsymmetricAlgorithm;
begin
  if FChanged then
    Result := FAlgorithm
  else
    Result := FKey.Algorithm;
end;

function TScKeyItem.GetBitCount: integer;
begin
  if FChanged then
    Result := FBitCount
  else
    Result := FKey.BitCount;
end;

function TScKeyItem.GetECName: TScECName;
begin
  Result := FECName;
end;

function TScKeyItem.GetIsPrivate: boolean;
begin
  Result := not FChanged and FKey.Ready and FKey.IsPrivate;
end;

function TScKeyItem.GetFingerprint: string;
begin
  if FChanged then
    Result := ''
  else
  if FKey.Ready then
    FKey.GetFingerPrint(haMD5, Result)
  else
    Result := WrongKeyFormat;
end;

procedure TScKeyItem.Init(Key: TScKey; PrevKeyItem: TScKeyItem = nil);
begin
  FKey := Key;
  FChanged := False;

  if PrevKeyItem <> nil then begin
    FBitCount := PrevKeyItem.GetBitCount;
    FAlgorithm := PrevKeyItem.GetAlgorithm;
    FECName := PrevKeyItem.GetECName;
  end
  else begin
    FBitCount := 2048;
    FAlgorithm := aaRSA;
    FECName := x25519;
  end;
end;

{ TScKeysFrame }

function TScKeysFrame.GetItems: TCollection;
begin
  Result := FItems;
end;

function TScKeysFrame.GetItemName(Item: TCollectionItem): string;
begin
  Result := TScKeyItem(Item).FKey.KeyName;
end;

function TScKeysFrame.GetCurItem: TScKeyItem;
begin
  Assert(lbItemName.ItemIndex <> - 1);
  if FOldItemIndex = -1 then
    Result := Items.Items[lbItemName.ItemIndex] as TScKeyItem
  else
    Result := Items.Items[FOldItemIndex] as TScKeyItem;
end;

procedure TScKeysFrame.ItemToControls(Item: TCollectionItem);
var
  KeyItem: TScKeyItem;
  ReadOnly: boolean;
begin
  KeyItem := Item as TScKeyItem;
  Assert(KeyItem.FKey <> nil);

  try
    if not KeyItem.FChanged then
      KeyItem.FKey.Ready := True;
  except
    on E: Exception do
      ApplicationHandleException(E);
  end;

  edKeyName.Text := KeyItem.FKey.KeyName;
  cbIsPrivate.Checked := KeyItem.GetIsPrivate;
  cbAlgorithm.ItemIndex := cbAlgorithm.Items.IndexOf(CipherFactory.PublicKeyAlgorithmToName(KeyItem.GetAlgorithm));
  if KeyItem.GetAlgorithm = aaEC then begin
    lbKeyData.Caption := 'EC name';
    cbECName.Visible := True;
    cbBitCount.Visible := False;
    cbECName.ItemIndex := integer(KeyItem.GetECName);
  end
  else begin
    lbKeyData.Caption := 'Bit count';
    cbECName.Visible := False;
    cbBitCount.Visible := True;
    cbBitCount.Text := IntToStr(KeyItem.GetBitCount);
  end;
  cbBitCount.Text := IntToStr(KeyItem.GetBitCount);
  edFingerPrint.Text := KeyItem.GetFingerPrint;

  ReadOnly := (Editor.Component as TScStorage).ReadOnly;
  cbAlgorithm.Enabled := not ReadOnly;
  cbBitCount.Enabled := not ReadOnly;
  cbECName.Enabled := not ReadOnly;

  btDelete.Enabled := not ReadOnly;
  btGenerate.Enabled := not ReadOnly;
  btImport.Enabled := not ReadOnly;
  btExportPrivate.Enabled := KeyItem.GetIsPrivate;
  btExportPublic.Enabled := KeyItem.GetReady;
end;

procedure TScKeysFrame.ControlsToItem(Item: TCollectionItem);
var
  KeyItem: TScKeyItem;
  BadControl: TWinControl;
begin
  SetKeyName;

  BadControl := nil;
  try
    KeyItem := Item as TScKeyItem;

    BadControl := cbAlgorithm;
    if cbAlgorithm.ItemIndex > -1 then
      KeyItem.FAlgorithm := CipherFactory.NameToPublicKeyAlgorithm(cbAlgorithm.Items[cbAlgorithm.ItemIndex]);

    BadControl := cbBitCount;
    if cbBitCount.Text = '' then
      cbBitCount.Text := IntToStr(KeyItem.GetBitCount);
    KeyItem.FBitCount := StrToInt(cbBitCount.Text);
    if cbECName.ItemIndex <> -1 then
      KeyItem.FECName := TScECName(cbECName.ItemIndex)
    else
      KeyItem.FECName := TScECName(0);

    if KeyItem.FKey.Ready or not KeyItem.FChanged then begin
      KeyItem.FChanged := KeyItem.FAlgorithm <> KeyItem.FKey.Algorithm;
      if KeyItem.FAlgorithm = aaEC then
        KeyItem.FChanged := KeyItem.FChanged or (KeyItem.FECName <> KeyItem.FKey.ECData.ECName)
      else
        KeyItem.FChanged := KeyItem.FChanged or (KeyItem.FBitCount <> KeyItem.FKey.BitCount);
    end;

    lbItemName.Repaint;
  except
    if Page.PageControl.ActivePage = Page then begin
      lbItemName.ItemIndex := FOldItemIndex;

      if BadControl <> nil then
        BadControl.SetFocus;
    end;
    raise;
  end;
end;

procedure TScKeysFrame.UpdateControlsState;
begin
  PanelItem.Enabled := Enabled;
  Enabled := True;

  btNew.Enabled := not (Editor.Component as TScStorage).ReadOnly;
  edKeyName.ReadOnly := (Editor.Component as TScStorage).ReadOnly;

  if Items.Count = 0 then begin
    btDelete.Enabled := False;
    btGenerate.Enabled := False;
    btImport.Enabled := False;
    btExportPrivate.Enabled := False;
    btExportPublic.Enabled := False;
  end;

  inherited;
end;

procedure TScKeysFrame.DoActivate;
var
  Storage: TScStorage;
  i: Integer;
begin
  FItems := TCollection.Create(TScKeyItem);

  Storage := Editor.Component as TScStorage;
  Storage.Keys.Refresh;
  for i := 0 to Storage.Keys.Count - 1 do
    (FItems.Add as TScKeyItem).Init(Storage.Keys.Keys[i]);

  inherited;

  cbECName.Items.BeginUpdate;
  cbECName.Items.Clear;
  for i := integer(Low(TScECName)) to integer(High(TScECName)) do
    cbECName.Items.Add(GetEnumName(TypeInfo(TScECName), i));
  cbECName.Items.EndUpdate;
end;

procedure TScKeysFrame.DoFinish;
var
  Res: Integer;
  i: Integer;
begin
  inherited;

  for i := 0 to Items.Count - 1 do
    if TScKeyItem(Items.Items[i]).FChanged then begin
      Res := MessageDlg('Invalid keys were not saved. Do you want to generate and save them?', mtConfirmation, [mbYes, mbNo, mbCancel], 0);
      if Res = mrYes then
        ReGenerateAll
      else
      if Res = mrCancel then
        Abort;
      Break;
    end;

  FreeAndNil(FItems);
end;

procedure TScKeysFrame.btNewClick(Sender: TObject);

  function IndexOf(const AName: string): Integer;
  begin
    for Result := 0 to FItems.Count - 1 do
      if AnsiCompareText(GetItemName(FItems.Items[Result]), AName) = 0 then
        Exit;
    Result := -1;
  end;

var
  Key: TScKey;
  NewKeyname: string;
  KeyItem: TScKeyItem;
  OldKeyItem: TScKeyItem;
  i: Integer;
begin
  StoreItem;

  i := 0;
  repeat
    Inc(i);
    NewKeyname := 'key' + IntToStr(i);
  until IndexOf(NewKeyname) = -1;

  Key := TScKey.Create(TScStorage(Editor.Component).Keys);
  Key.KeyName := NewKeyname;
  KeyItem := FItems.Add as TScKeyItem;
  if lbItemName.ItemIndex = - 1 then
    OldKeyItem := nil
  else
    OldKeyItem := CurItem;
  KeyItem.Init(Key, OldKeyItem);
  KeyItem.FChanged := True;

  lbItemName.Items.Add(GetItemName(KeyItem));
  lbItemName.ItemIndex := lbItemName.Items.Count - 1;

  SelectItem;
  lbItemName.Repaint;
end;

procedure TScKeysFrame.btDeleteClick(Sender: TObject);
begin
  SetKeyName;

  if MessageDlg('Do you want delete "' + GetItemName(CurItem) + '" key?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  try
    TScStorage(Editor.Component).Keys.Remove(CurItem.FKey);
    CurItem.FKey.Free;
    CurItem.Free;
  finally
    InitItems;
  end;
end;

procedure TScKeysFrame.btImportClick(Sender: TObject);
begin
  SetKeyName;

  try
    if TScKeyEdit.ImportKey(CurItem.FKey) then
      CurItem.FChanged := False;
  finally
    SelectItem;
    lbItemName.Repaint;
  end;
end;

procedure TScKeysFrame.btExportPrivateClick(Sender: TObject);
begin
  Assert(CurItem.GetReady);
  TScKeyEdit.ExportPrivateKey(CurItem.FKey);
end;

procedure TScKeysFrame.btExportPublicClick(Sender: TObject);
begin
  Assert(CurItem.GetReady);
  TScKeyEdit.ExportPublicKey(CurItem.FKey);
end;

procedure TScKeysFrame.btGenerateClick(Sender: TObject);
var
  OldCursor: TCursor;
begin
  SetKeyName;
  if CurItem.GetFingerPrint <> '' then
    if MessageDlg('Do you want regenarate "' + GetItemName(CurItem) + '" key?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Exit;

  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;

    StoreItem;
    try
      if CurItem.GetAlgorithm = aaEC then
        CurItem.FKey.GenerateEC(CurItem.GetECName)
      else
        CurItem.FKey.Generate(CurItem.GetAlgorithm, CurItem.GetBitCount);
      CurItem.FChanged := False;
    finally
      SelectItem;
      lbItemName.Repaint;
    end;
  finally
    Screen.Cursor := OldCursor;
  end;
end;

procedure TScKeysFrame.ReGenerateAll;
var
  OldCursor: TCursor;
  KeyItem: TScKeyItem;
  i: Integer;
begin
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;

    for i := 0 to Items.Count - 1 do
      if TScKeyItem(Items.Items[i]).FChanged then begin
        KeyItem := Items.Items[i] as TScKeyItem;
        try
          if KeyItem.GetAlgorithm = aaEC then
            KeyItem.FKey.GenerateEC(KeyItem.GetECName)
          else
            KeyItem.FKey.Generate(KeyItem.GetAlgorithm, KeyItem.GetBitCount);
          KeyItem.FChanged := False;
        except
          on E: Exception do
            ApplicationHandleException(E);
        end;
      end;
  finally
    Screen.Cursor := OldCursor;
  end;
end;

procedure TScKeysFrame.SetKeyName;
begin
  if edKeyName.Text <> CurItem.FKey.KeyName then begin
    try
      CurItem.FKey.KeyName := edKeyName.Text;
      lbItemName.Items[CurItem.Index] := GetItemName(CurItem);
    except
      if Page.PageControl.ActivePage = Page then
        edKeyName.SetFocus;

      raise;
    end;
  end;
end;

procedure TScKeysFrame.lbItemNameDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);

  function ItemIsModified(Item: TCollectionItem): Boolean;
  begin
    Result := TScKeyItem(Item).FChanged;
    if Item = CurItem then begin
      Result := Result or (CipherFactory.PublicKeyAlgorithmToName(CurItem.GetAlgorithm) <> cbAlgorithm.Items[cbAlgorithm.ItemIndex]);
      if cbAlgorithm.Items[cbAlgorithm.ItemIndex] = CipherFactory.PublicKeyAlgorithmToName(aaEC) then
        Result := Result or (cbECName.ItemIndex <> integer(CurItem.GetECName))
      else
        Result := Result or (cbBitCount.Text <> IntToStr(CurItem.GetBitCount));
    end;
  end;

begin
  with (Control as TListBox).Canvas do begin
    if (Items <> nil) and (Items.Items[Index] <> nil) then
      if (lbItemName.ItemIndex <> -1) and ItemIsModified(Items.Items[Index]) then
        Font.Style := Font.Style + [fsBold]
      else
        Font.Style := Font.Style - [fsBold];

    FillRect(Rect);
    TextOut(Rect.Left + 2, Rect.Top, (Control as TListBox).Items[Index]);
  end;
end;

procedure TScKeysFrame.cbAlgorithmChange(Sender: TObject);
begin
  lbItemName.Repaint;
  if cbAlgorithm.Text = 'EC' then begin
    lbKeyData.Caption := 'EC name';
    cbECName.Visible := True;
    cbBitCount.Visible := False;
  end
  else begin
    lbKeyData.Caption := 'Bit count';
    cbECName.Visible := False;
    cbBitCount.Visible := True;
  end;
end;

procedure TScKeysFrame.cbBitCountKeyPress(Sender: TObject; var Key: Char);
begin
  if not (AnsiChar(Key) in ['0'..'9']) then
    Key := #0;
end;

procedure TScKeysFrame.edKeyNameExit(Sender: TObject);
begin
  SetKeyName;
end;

end.

