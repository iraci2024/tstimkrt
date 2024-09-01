unit ScKeysFrame;

{$I ..\Base\SBDemo.inc}
interface

uses
{$IFDEF CLR}
  System.ComponentModel,
{$ENDIF}
  Windows, Messages, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, Classes, SysUtils, Buttons,
  ScUtils, ScBridge, ScSSHUtils, ScAlgorithmSupport, ScColFrame;

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
    FKeyName: string;
    FAlgorithm: TScAsymmetricAlgorithm;
    FBitCount: Integer;
    FIsPrivate: boolean;
    FFingerPrint: string;
    FReady: boolean;
    FChanged: boolean;
    procedure SetFromKey(Key: TScKey; NewKey: Boolean; CurItem: TScKeyItem = nil);
    function IndexOf(const AName: string): Integer;
  end;

  TScKeysFrame = class(TScColFrame)
    edKeyName: TEdit;
    edFingerPrint: TEdit;
    Label1: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    cbIsPrivate: TCheckBox;
    cbAlgorithm: TComboBox;
    cbBitCount: TComboBox;
    Label2: TLabel;
    btImport: TButton;
    btExportPrivate: TButton;
    btExportPublic: TButton;
    btGenerate: TButton;
    procedure btImportClick(Sender: TObject);
    procedure btExportPrivateClick(Sender: TObject);
    procedure btDeleteClick(Sender: TObject);
    procedure lbItemNameDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure cbBitCountExit(Sender: TObject);
    procedure cbAlgorithmChange(Sender: TObject);
    procedure btGenerateClick(Sender: TObject);
    procedure btNewClick(Sender: TObject);
    procedure btExportPublicClick(Sender: TObject);
    procedure lbItemNameClick(Sender: TObject);

  protected
    FItems: TCollection;
    FStorage: TScStorage;

    function GetCurItem: TScKeyItem;
    procedure ReGenerate;
    function GetItems: TCollection; override;
    function GetItemName(Item: TCollectionItem): string; override;
    procedure ItemToControls(Item: TCollectionItem); override;
    procedure ControlsToItem(Item: TCollectionItem); override;
    procedure UpdateControlsState; override;
    procedure SetKeyName;

    procedure DoActivate; override;
    procedure DoFinish; override;
    function DoSave: Boolean; override;
    property CurItem: TScKeyItem read GetCurItem;
  public
    property Storage: TScStorage read FStorage write FStorage;
  end;

implementation

{$IFDEF CLR}
{$R *.nfm}
{$ELSE}
{$R *.dfm}
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
    FileOpen.Filter := 'All formats |*.ssl;*.pem;*.ietf;*.pub;*.ietfpub|OpenSSL format (*.ssl)|*.ssl|PKCS8 format (*.pem)|*.pem|IETF format (*.ietf)|*.ietf|Public key (*.pub)|*.pub|Public IETF key (*.ietfpub)|*.ietfpub|All files (*.*)|*.*';
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

function TScKeyItem.IndexOf(const AName: string): Integer;
begin
  for Result := 0 to Collection.Count - 1 do
    if AnsiCompareText(TScKeyItem(Collection.Items[Result]).FKeyName, AName) = 0 then
      Exit;
  Result := -1;
end;

procedure TScKeyItem.SetFromKey(Key: TScKey; NewKey: Boolean; CurItem: TScKeyItem = nil);
begin
  FKeyName := Key.KeyName;
  FReady := Key.Ready;
  FChanged := NewKey;

  if NewKey and (CurItem <> nil) then begin
    FBitCount := CurItem.FBitCount;
    FAlgorithm := CurItem.FAlgorithm;
  end
  else begin
    if NewKey then
      FBitCount := 2048
    else
      FBitCount := Key.BitCount;
    FAlgorithm := Key.Algorithm;
  end;

  if Key.Ready then begin
    FIsPrivate := Key.IsPrivate;
    Key.GetFingerPrint(haMD5, FFingerPrint);
  end
  else begin
    FIsPrivate := False;
    if NewKey then
      FFingerPrint := ''
    else
      FFingerPrint := WrongKeyFormat;
  end;
end;

{ TScKeysFrame }

function TScKeysFrame.GetItems: TCollection;
begin
  Result := FItems;
end;

function TScKeysFrame.GetItemName(Item: TCollectionItem): string;
begin
  Result := TScKeyItem(Item).FKeyName;
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
begin
  KeyItem := Item as TScKeyItem;

  edKeyName.Text := KeyItem.FKeyName;
  edFingerPrint.Text := KeyItem.FFingerPrint;
  cbIsPrivate.Checked := KeyItem.FIsPrivate;
  cbAlgorithm.ItemIndex := cbAlgorithm.Items.IndexOf(CipherFactory.PublicKeyAlgorithmToName(KeyItem.FAlgorithm));
  Assert(cbAlgorithm.ItemIndex <> -1);
  cbBitCount.Text := IntToStr(KeyItem.FBitCount);
end;

procedure TScKeysFrame.ControlsToItem(Item: TCollectionItem);
var
  KeyItem: TScKeyItem;
begin
  SetKeyName;
  KeyItem := Item as TScKeyItem;

  if (CipherFactory.PublicKeyAlgorithmToName(KeyItem.FAlgorithm) <> cbAlgorithm.Items[cbAlgorithm.ItemIndex]) or
     (cbBitCount.Text <> IntToStr(KeyItem.FBitCount)) then begin
    if cbAlgorithm.ItemIndex > -1 then
      KeyItem.FAlgorithm := CipherFactory.NameToPublicKeyAlgorithm(cbAlgorithm.Items[cbAlgorithm.ItemIndex]);

    if cbBitCount.Text <> '' then
      KeyItem.FBitCount := StrToInt(cbBitCount.Text)
    else
      KeyItem.FBitCount := 2048;

    if KeyItem.FReady then begin
      KeyItem.FChanged := True;
      KeyItem.FReady := False;
      KeyItem.FFingerPrint := '';
    end;
    lbItemName.Repaint;
  end;
end;

procedure TScKeysFrame.UpdateControlsState;
var
  ChangeAble: boolean;
begin
  ChangeAble := not FStorage.ReadOnly;
  btNew.Enabled := ChangeAble;
  edKeyName.ReadOnly := not ChangeAble;
  cbIsPrivate.Enabled := False;
  edFingerPrint.ReadOnly := True;

  if lbItemName.ItemIndex <> - 1 then begin
    btExportPrivate.Enabled := CurItem.FReady and CurItem.FIsPrivate;
    btExportPublic.Enabled := CurItem.FReady;
    btImport.Enabled := ChangeAble;
    btGenerate.Enabled := ChangeAble;
    btDelete.Enabled := ChangeAble;
  end
  else begin
    btExportPrivate.Enabled := False;
    btExportPublic.Enabled := False;
    btImport.Enabled := False;
    btGenerate.Enabled := False;
    btDelete.Enabled := False;
  end;

  inherited;

  cbAlgorithm.Enabled := ChangeAble;
  cbBitCount.Enabled := ChangeAble;
end;

procedure TScKeysFrame.DoActivate;
var
  Key: TScKey;
  i: Integer;
begin
  FItems := TCollection.Create(TScKeyItem);

  FStorage.Keys.Refresh;
  for i := 0 to FStorage.Keys.Count - 1 do begin
    Key := FStorage.Keys.Keys[i];
    try
      Key.Ready := True;
    except
      on E: Exception do
        ApplicationHandleException(E);
    end;
    (FItems.Add as TScKeyItem).SetFromKey(Key, False);
  end;

  inherited;
end;

procedure TScKeysFrame.DoFinish;
begin
  inherited;

  FreeAndNil(FItems);
  FModified := False;
end;

function TScKeysFrame.DoSave: Boolean;
var
  Res: Integer;
  i: Integer;
begin
  Result := True;
  StoreItem;

  for i := 0 to Items.Count - 1 do
    if TScKeyItem(Items.Items[i]).FChanged then begin
      Res := MessageDlg('Invalid keys will not be saved. Do you want to generate and save them?', mtConfirmation, [mbYes, mbNo, mbCancel], 0);
      if Res = mrYes then
        ReGenerate
      else
      if Res = mrCancel then
        Result := False;
        
      Exit;
    end;
end;

procedure TScKeysFrame.btImportClick(Sender: TObject);
var
  Key: TScKey;
  OldChanged: boolean;
begin
  SetKeyName;
  OldChanged := CurItem.FChanged;
  Key := FStorage.Keys.KeyByName(CurItem.FKeyName);

  try
    if not TScKeyEdit.ImportKey(Key) then
      Key.Ready := CurItem.FReady
    else
      OldChanged := False;
  finally
    if CurItem.FReady or Key.Ready then
      CurItem.SetFromKey(Key, False);
    CurItem.FChanged := OldChanged;
    SelectItem;
    lbItemName.Repaint;
  end;
end;

procedure TScKeysFrame.btExportPrivateClick(Sender: TObject);
var
  Key: TScKey;
begin
  Assert(CurItem.FReady);
  Key := FStorage.Keys.KeyByName(CurItem.FKeyName);
  Key.Ready := True;
  TScKeyEdit.ExportPrivateKey(Key);
end;

procedure TScKeysFrame.btExportPublicClick(Sender: TObject);
var
  Key: TScKey;
begin
  Assert(CurItem.FReady);
  Key := FStorage.Keys.KeyByName(CurItem.FKeyName);
  Key.Ready := True;
  TScKeyEdit.ExportPublicKey(Key);
end;

procedure TScKeysFrame.btDeleteClick(Sender: TObject);
var
  Key: TScKey;
begin
  SetKeyName;
  if MessageDlg('Do you want delete "' + CurItem.FKeyName + '" key?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
    Exit;

  Key := FStorage.Keys.KeyByName(CurItem.FKeyName);
  try
    FStorage.Keys.Remove(Key);
    CurItem.Free;
    Key.Free;
  finally
    InitItems;
  end;
end;

procedure TScKeysFrame.btGenerateClick(Sender: TObject);
var
  OldCursor: TCursor;
  Key: TScKey;
  Algorithm: TScAsymmetricAlgorithm;
  BitCount: integer;
begin
  SetKeyName;
  if CurItem.FFingerPrint <> '' then
    if MessageDlg('Do you want regenarate "' + CurItem.FKeyName + '" key?', mtConfirmation, [mbYes, mbNo], 0) <> mrYes then
      Exit;

  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;

    if cbAlgorithm.ItemIndex > -1 then
      Algorithm := CipherFactory.NameToPublicKeyAlgorithm(cbAlgorithm.Items[cbAlgorithm.ItemIndex])
    else
      Algorithm := aaRSA;
    if cbBitCount.Text <> '' then
      BitCount := StrToInt(cbBitCount.Text)
    else
      BitCount := 2048;

    Key := FStorage.Keys.KeyByName(CurItem.FKeyName);
    try
      Key.Generate(Algorithm, BitCount);
    finally
      CurItem.SetFromKey(Key, False);
      SelectItem;
      lbItemName.Repaint;
    end;
  finally
    Screen.Cursor := OldCursor;
  end;
end;

procedure TScKeysFrame.btNewClick(Sender: TObject);
var
  Key: TScKey;
  NewKeyname: string;
  KeyItem: TScKeyItem;
  OldKeyItem: TScKeyItem;
  i: Integer;
begin
  StoreItem;

  i := 1;
  NewKeyname := 'key' + IntToStr(i);
  if lbItemName.ItemIndex <> - 1 then
    while CurItem.IndexOf(NewKeyname) > -1 do begin
      Inc(i);
      NewKeyname := 'key' + IntToStr(i);
    end;

  Key := TScKey.Create(FStorage.Keys);
  Key.KeyName := NewKeyname;
  KeyItem := FItems.Add as TScKeyItem;
  if lbItemName.ItemIndex = - 1 then
    OldKeyItem := nil
  else
    OldKeyItem := CurItem;
  KeyItem.SetFromKey(Key, True, OldKeyItem);

  lbItemName.Items.Add(GetItemName(KeyItem));
  lbItemName.ItemIndex := lbItemName.Items.Count - 1;

  SelectItem;
end;

procedure TScKeysFrame.ReGenerate;
var
  OldCursor: TCursor;
  Key: TScKey;
  KeyItem: TScKeyItem;
  i: Integer;
begin
  OldCursor := Screen.Cursor;
  try
    Screen.Cursor := crHourGlass;

    for i := 0 to Items.Count - 1 do
      if TScKeyItem(Items.Items[i]).FChanged then begin
        KeyItem := Items.Items[i] as TScKeyItem;
        Key := FStorage.Keys.KeyByName(KeyItem.FKeyName);
        if KeyItem.FBitCount = 0 then
          KeyItem.FBitCount := 2048;
        Key.Generate(KeyItem.FAlgorithm, KeyItem.FBitCount);
        KeyItem.SetFromKey(Key, False);
      end;
  finally
    Screen.Cursor := OldCursor;
  end;
end;

procedure TScKeysFrame.lbItemNameDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  with (Control as TListBox).Canvas do begin
    if (Items <> nil) and (Items.Items[Index] <> nil) then
      if TScKeyItem(Items.Items[Index]).FChanged or
         (FModified and (Items.Items[Index] = CurItem)) then
        Font.Style := Font.Style + [fsBold]
      else
        Font.Style := Font.Style - [fsBold];

    FillRect(Rect);
    TextOut(Rect.Left + 2, Rect.Top, (Control as TListBox).Items[Index]);
  end;
end;

procedure TScKeysFrame.cbBitCountExit(Sender: TObject);
begin
  try
    if cbBitCount.Text <> '' then
      StrToInt(cbBitCount.Text);
  except
    cbBitCount.SetFocus;
    raise;
  end;
end;

procedure TScKeysFrame.cbAlgorithmChange(Sender: TObject);
begin
  FModified := False;

  if CurItem.FReady then
    if (CipherFactory.PublicKeyAlgorithmToName(CurItem.FAlgorithm) <> cbAlgorithm.Items[cbAlgorithm.ItemIndex]) or
       (cbBitCount.Text <> IntToStr(CurItem.FBitCount)) then
      FModified := True;
  lbItemName.Repaint;
end;

procedure TScKeysFrame.SetKeyName;
var
  Key: TScKey;
begin
  if edKeyName.Text <> CurItem.FKeyName then begin
    Key := FStorage.Keys.KeyByName(CurItem.FKeyName);
    try
      try
        Key.KeyName := edKeyName.Text;
      except
        edKeyName.SetFocus;
        raise;
      end;

      CurItem.FKeyName := edKeyName.Text;
    finally
      lbItemName.Items[CurItem.Index] := GetItemName(CurItem);
    end;
  end;
end;

procedure TScKeysFrame.lbItemNameClick(Sender: TObject);
begin
  if lbItemName.ItemIndex = FOldItemIndex then
    SetKeyName;

  inherited;
end;

end.

