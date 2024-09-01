unit sInternalSkins;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, Buttons, FileCtrl, ActnList, ExtCtrls,
  {$IFDEF DELPHI_XE2} UITypes,   {$ENDIF}
  sSkinManager, acntUtils, sSkinProvider, sPanel, sButton, sListBox, sLabel, ComCtrls,
  sBitBtn, sSpeedButton, sGroupBox;


type
  TFormInternalSkins = class(TForm)
    sSkinProvider1: TsSkinProvider;
    sPanel1: TsPanel;
    sPanel3: TsPanel;
    sPanel6: TsPanel;
    sSpeedButton1: TsSpeedButton;
    sSpeedButton2: TsSpeedButton;
    sSpeedButton3: TsSpeedButton;
    sSpeedButton4: TsSpeedButton;
    sSpeedButton5: TsSpeedButton;
    ListBox1: TsListBox;
    sGroupBox1: TsGroupBox;
    sLabel10: TsLabel;
    sLabel11: TsLabel;
    sLabel12: TsLabel;
    sLabel9: TsLabel;
    sLabel5: TsLabel;
    sLabel4: TsLabel;
    sLabel8: TsLabel;
    sLabel7: TsLabel;
    sLabel3: TsLabel;
    sBitBtn1: TsBitBtn;
    sGroupBox2: TsGroupBox;
    sButton6: TsBitBtn;
    sLabel1: TsLabel;
    procedure ListBox1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sSpeedButton1Click(Sender: TObject);
    procedure sSpeedButton2Click(Sender: TObject);
    procedure sSpeedButton3Click(Sender: TObject);
    procedure sButton6Click(Sender: TObject);
    procedure sBitBtn1Click(Sender: TObject);
    procedure sSpeedButton4Click(Sender: TObject);
    procedure sSpeedButton5Click(Sender: TObject);
  public
    NewDir,
    NewName: string;
    ListChanged: boolean;
    SkinManager: TsSkinManager;
    procedure AddNewSkin;
    procedure UpdateMyActions;
    procedure Loaded; override;
  end;


var
  FormInternalSkins: TFormInternalSkins;


implementation

uses
  IniFiles, math,
  {$IFNDEF ALITE} acSelectSkin, {$ENDIF}
  sStrings, sDefaults, sSkinProps, sConst, acDesignData;

{$R *.DFM}

function SafeStrToFloat(Value: string): real;
var
  SavedSeparator: char;
begin
  if (pos(s_Dot, Value) > 0) and ({$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}DecimalSeparator <> s_Dot) then begin
    SavedSeparator := {$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}DecimalSeparator;
    {$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}DecimalSeparator := s_Dot;
    Result := StrToFloat(Value);
    {$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}DecimalSeparator := SavedSeparator;
  end
  else
    Result := StrToFloat(Value);
end;


procedure TFormInternalSkins.AddNewSkin;
var
  TempSkinFile: TMemIniFile;
  sc: TacSkinConvertor;
  s, IntName: string;
  sl: TStringList;
  i: integer;
begin
  s := NormalDir(NewDir) + NewName + s_Dot + acSkinExt;
  if FileExists(s) then
    with SkinManager do begin
      IntName := NewName + s_Space + acs_InternalSkin;
      for i := 0 to InternalSkins.Count - 1 do
        if InternalSkins[i].Name = IntName then
          Exit;

      InternalSkins.Add;
      InternalSkins[InternalSkins.Count - 1].Name := IntName;
      InternalSkins[InternalSkins.Count - 1].OrigName := NewName + s_Dot + acSkinExt;
      InternalSkins[InternalSkins.Count - 1].PackedData.LoadFromFile(s);

      sc := nil;
      LoadSkinFromFile(s, sc, SkinManager.KeyList, SkinManager);
      if sc.Options <> nil then begin
        sl := TStringList.Create;
        {$IFDEF DELPHI6UP} sl.CaseSensitive := True; {$ENDIF}
        sc.Options.Seek(0, 0);
        sl.LoadFromStream(sc.Options);
        TempSkinFile := TMemIniFile.Create('');
        {$IFDEF DELPHI6UP} TempSkinFile.CaseSensitive := True; {$ENDIF}
        TempSkinFile.SetStrings(sl);
        SkinManager.InternalSkins[SkinManager.InternalSkins.Count - 1].Version := SafeStrToFloat(TempSkinFile.ReadString(s_GlobalInfo, s_Version, '0'));
        SkinManager.InternalSkins[InternalSkins.Count - 1].Author := TempSkinFile.ReadString(s_GlobalInfo, s_Author, '');
        SkinManager.InternalSkins[InternalSkins.Count - 1].Description := TempSkinFile.ReadString(s_GlobalInfo, s_Description, '');
        TempSkinFile.Free;
        FreeAndNil(sl);
      end;
      ListBox1.Items.Add(InternalSkins[InternalSkins.Count - 1].Name);
    end
  else begin
    s := s + s_Dot + acSkinExt + ' file with a skin data does not exists';
    ShowWarning(s);
  end;
end;


procedure TFormInternalSkins.ListBox1Click(Sender: TObject);
begin
  UpdateMyActions;
end;


procedure TFormInternalSkins.Loaded;
begin
  if DefaultManager <> nil then
    Scaled := DefaultManager.Options.ScaleMode = smVCL;

  inherited;
end;


procedure TFormInternalSkins.sBitBtn1Click(Sender: TObject);
begin
  Close;
end;


procedure TFormInternalSkins.sButton6Click(Sender: TObject);
var
  i, p: integer;
  sFileName, sn: string;
  sc: TacSkinConvertor;
  sl: TStringList;
  sFile: TMemIniFile;
begin
  for i := 0 to SkinManager.InternalSkins.Count - 1 do begin
    if SkinManager.InternalSkins[i].OrigName <> '' then
      sFileName := NormalDir(NewDir) + SkinManager.InternalSkins[i].OrigName
    else begin
      sFileName := SkinManager.InternalSkins[i].Name;
      p := Pos(s_Space + acs_InternalSkin, sFileName);
      if p > 0 then
        Delete(sFileName, p, Length(s_Space + acs_InternalSkin));

      sFileName := NormalDir(NewDir) + sFileName + s_Dot + acSkinExt;
    end;
    if FileExists(sFileName) then begin
      SkinManager.InternalSkins[i].PackedData.Clear;
      SkinManager.InternalSkins[i].PackedData.LoadFromFile(sFileName);

      sc := TacSkinConvertor.Create;
      sc.PackedData := SkinManager.InternalSkins[i].PackedData;
      ExtractPackedData(sc, SkinManager.KeyList, SkinManager);
      sc.PackedData := nil;
      sc.Options.Seek(0, 0);
      sl := TStringList.Create;
      sl.LoadFromStream(sc.Options);
      sFile := TMemIniFile.Create('');
      sFile.SetStrings(sl);
      FreeAndNil(sl);
      SkinManager.InternalSkins[i].OrigName := ExtractFileName(sFileName);
      SkinManager.InternalSkins[i].Version := SafeStrToFloat(sFile.ReadString(s_GlobalInfo, s_Version, '0'));
      SkinManager.InternalSkins[i].Author := sFile.ReadString(s_GlobalInfo, s_Author, '');
      SkinManager.InternalSkins[i].Description := sFile.ReadString(s_GlobalInfo, s_Description, '');
      sFile.Free;
      sc.Free;
    end
    else begin
      if sn <> '' then
        sn := sn + ', ';

      sn := sn + SkinManager.InternalSkins[i].Name;
    end;
  end;
  if sn <> '' then
    ShowWarning('These skins are not updated:' + s_0D0A + sn)
  else begin
    if ListBox1.ItemIndex >= 0 then
      ListBox1Click(ListBox1);

    MessageDlg('All internal skins were reloaded from disk!', mtInformation, [mbOk], 0);
    if Designer <> nil then
      Designer.Modified;

    ListChanged := True;
  end;
end;


procedure TFormInternalSkins.sSpeedButton1Click(Sender: TObject);
var
  sl: TStringList;
{$IFDEF ALITE}
  TempSkinFile: TMemIniFile;
  sc: TacSkinConvertor;
  fod: TOpenDialog;
{$ELSE}
  i: integer;
{$ENDIF}
begin
{$IFNDEF ALITE}
  sl := TStringList.Create;
  if NewName <> '' then
    sl.Add(NewName);

  if SelectSkin(sl, NewDir, stPacked) then begin
    SkinManager.SkinDirectory := NewDir;
    for i := 0 to sl.Count - 1 do begin
      NewName := sl[i];
      AddNewSkin;
    end;
    ListChanged := True;
  end;

  sLabel1.Caption := NewDir;
  FreeAndNil(sl);
{$ELSE}
  fod := TOpenDialog.Create(Application);
  fod.Filter := 'Packed AlphaSkins|*.' + acSkinExt;
  fod.InitialDir := NewDir;
  if fod.Execute then
    with SkinManager do begin
      ListChanged := True;
      NewDir := ExtractFilePath(fod.FileName);
      NewName := ExtractFileName(fod.FileName);
      Delete(NewName, Length(NewName) - 3, 4);
      InternalSkins.Add;
      InternalSkins[InternalSkins.Count - 1].Name := NewName + s_Space + acs_InternalSkin;
      InternalSkins[InternalSkins.Count - 1].OrigName := NewName + s_Dot + acSkinExt;
      InternalSkins[InternalSkins.Count - 1].PackedData.LoadFromFile(fod.FileName);

      sc := nil;
      LoadSkinFromFile(fod.FileName, sc, SkinManager.KeyList, SkinManager);
      if sc.Options <> nil then begin
        sl := TStringList.Create;
        {$IFDEF DELPHI6UP} sl.CaseSensitive := True; {$ENDIF}
        sl.LoadFromStream(sc.Options);
        TempSkinFile := TMemIniFile.Create('');
        {$IFDEF DELPHI6UP} TempSkinFile.CaseSensitive := True; {$ENDIF}
        TempSkinFile.SetStrings(sl);
        InternalSkins[InternalSkins.Count - 1].Version := SafeStrToFloat(TempSkinFile.ReadString(s_GlobalInfo, s_Version, '0'));
        InternalSkins[InternalSkins.Count - 1].Author := TempSkinFile.ReadString(s_GlobalInfo, s_Author, '');
        InternalSkins[InternalSkins.Count - 1].Description := TempSkinFile.ReadString(s_GlobalInfo, s_Description, '');
        TempSkinFile.Free;
        FreeAndNil(sl);
      end;

      ListBox1.Items.Add(InternalSkins[InternalSkins.Count - 1].Name);
    end;

  sLabel1.Caption := fod.InitialDir;
  fod.Free;
{$ENDIF}
  UpdateMyActions;
end;


procedure TFormInternalSkins.sSpeedButton2Click(Sender: TObject);
var
  i: integer;
  s: string;
begin
  s := ListBox1.Items[ListBox1.ItemIndex];
  if InputQuery('String input', 'Enter new name:', s) then begin
    if ListBox1.ItemIndex >= 0 then begin
      for i := 0 to SkinManager.InternalSkins.Count - 1 do
        if SkinManager.InternalSkins.Items[i].Name = ListBox1.Items[ListBox1.ItemIndex] then begin
          SkinManager.InternalSkins.Items[i].Name := s;
          break;
        end;

      ListBox1.Items[ListBox1.ItemIndex] := s;
    end;
    UpdateMyActions;
    ListChanged := True;
  end;
end;


procedure TFormInternalSkins.sSpeedButton3Click(Sender: TObject);
var
  SavedIndex, i: integer;
begin
  if ListBox1.ItemIndex >= 0 then begin
    SavedIndex := ListBox1.ItemIndex;
    for i := 0 to SkinManager.InternalSkins.Count - 1 do
      if SkinManager.InternalSkins.Items[i].Name = ListBox1.Items[ListBox1.ItemIndex] then begin
        SkinManager.InternalSkins.Delete(i);
        break;
      end;

    ListBox1.Items.Delete(ListBox1.ItemIndex);
    ListChanged := True;
    if ListBox1.Items.Count >= 0 then
      ListBox1.ItemIndex := min(SavedIndex, ListBox1.Items.Count - 1);
  end;
  UpdateMyActions;
end;


procedure TFormInternalSkins.sSpeedButton4Click(Sender: TObject);
begin
{$IFNDEF ALITE}
  if Customrequest('Do you really want to delete all internal skins?') then begin
{$ELSE}
  if MessageDlg('Do you really want to delete all internal skins?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
{$ENDIF}
    while SkinManager.InternalSkins.Count > 0 do
      SkinManager.InternalSkins.Delete(0);

    ListBox1.Items.Clear;
    UpdateMyActions;
    ListChanged := True;
  end;
end;


procedure TFormInternalSkins.sSpeedButton5Click(Sender: TObject);
var
  s: string;
begin
  s := s_Slash;
  SelectDirectory(s, [], -1);
  if s <> s_Slash then
    SkinManager.ExtractInternalSkin(ListBox1.Items[ListBox1.ItemIndex], s);
end;


procedure TFormInternalSkins.FormShow(Sender: TObject);
begin
  ListChanged := False;
  NewDir := DefaultManager.SkinDirectory;
  if not acDirExists(NewDir) then
    NewDir := DefSkinsDir;

  sLabel1.Caption := NewDir;
  UpdateMyActions;
  ReleaseCapture;
end;


procedure TFormInternalSkins.UpdateMyActions;
var
  i: integer;
  OldSeparator: char;
begin
  sSpeedButton4.Enabled := ListBox1.Items.Count > 0;
  sButton6.Enabled := sSpeedButton4.Enabled;
  sSpeedButton3.Enabled := sSpeedButton4.Enabled and (ListBox1.ItemIndex >= 0);
  sSpeedButton2.Enabled := sSpeedButton3.Enabled;
  sSpeedButton5.Enabled := sSpeedButton3.Enabled;
  if sSpeedButton3.Enabled then begin
    for i := 0 to SkinManager.InternalSkins.Count - 1 do
      if SkinManager.InternalSkins.Items[i].Name = ListBox1.Items[ListBox1.ItemIndex] then
        break;

    if SkinManager.InternalSkins.Items[i].Version <> 0 then begin
      OldSeparator := {$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}DecimalSeparator;
      {$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}DecimalSeparator := s_Dot;
      sLabel7.Caption := FloatToStr(SkinManager.InternalSkins.Items[i].Version);
      {$IFDEF DELPHI_XE}FormatSettings.{$ENDIF}DecimalSeparator := OldSeparator;
      sLabel8.Caption := SkinManager.InternalSkins.Items[i].Author;
      sLabel9.Caption := SkinManager.InternalSkins.Items[i].OrigName;
      sLabel10.Caption := 'Description: ' + SkinManager.InternalSkins.Items[i].Description;
    end
    else begin
      sLabel7.Caption := 'N/A';
      sLabel8.Caption := 'N/A';
      sLabel9.Caption := 'N/A';
      sLabel10.Caption := 'Description: N/A';
    end;
    sLabel12.Caption := FloatToStrF(SkinManager.InternalSkins.Items[i].PackedData.Size / 1024, ffNumber, 4, 2) + ' Kb';
  end
  else begin
    sLabel7.Caption := '';
    sLabel8.Caption := '';
    sLabel9.Caption := '';
    sLabel10.Caption := 'Description:';
    sLabel12.Caption := '';
  end;
end;

end.
