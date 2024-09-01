unit acFontStoreEditor;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Buttons, StdCtrls, Mask, ExtCtrls, ImgList, ComCtrls, 
  
  sSpeedButton, sMemo, sListBox, sCheckListBox, sButton, sCheckBox, sPanel, sEdit, sSkinProvider, acFontStore, sListView, acAlphaImageList, sBitBtn;


type
  TFormFontStore = class(TForm)
    sListBox1: TsListBox;
    sDirectoryEdit1: TsEdit;
    sPanel1: TsPanel;
    sCheckBox1: TsCheckBox;
    sCheckBox2: TsCheckBox;
    sButton1: TsBitBtn;
    sButton2: TsBitBtn;
    sFontStore1: TsFontStore;
    sSkinProvider1: TsSkinProvider;
    sListView1: TsListView;
    sButton3: TsBitBtn;
    sButton4: TsBitBtn;
    sButton5: TsBitBtn;
    sCheckBox3: TsCheckBox;
    sEdit1: TsEdit;
    sSpeedButton1: TsSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure sDirectoryEdit1Change(Sender: TObject);
    procedure sCheckBox1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure sCheckBox2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure sListView1Change(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure sPanel1Paint(Sender: TObject; Canvas: TCanvas);
    procedure sButton3Click(Sender: TObject);
    procedure sButton5Click(Sender: TObject);
    procedure sButton4Click(Sender: TObject);
    procedure sListBox1Click(Sender: TObject);
    procedure ItemUpdated;
    procedure sEdit1Change(Sender: TObject);
    procedure sCheckBox3Click(Sender: TObject);
    procedure sSpeedButton1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    WarningShown, Updating: boolean;
    FCurrentName: string;
    procedure SetCurrentName(const Value: string);
    property CurrentName: string read FCurrentName write SetCurrentName;
    procedure ClearListView;
  public
    FontStore: TsFontStore;
    procedure InitEditor(aFontStore: TsFontStore);
  end;

var
  FormFontStore: TFormFontStore;


implementation

uses
  math,
{$IFNDEF ALITE}
  acPathDialog,
  acImage,
  sDialogs,
//{$ELSE}
{$ENDIF}
  FileCtrl,
  sLabel, acgpUtils, sVCLUtils, acntUtils, sConst, sGraphUtils, acDesignData, sStoreUtils;

{$R *.dfm}

var
  sTestStr: acString;
  sDir: string = '';


//function GetFontResourceInfo(lpszFilename: PWideChar; var cbBuffer: DWORD; lpBuffer: PWideChar; dwQueryType: DWORD): DWORD; stdcall; external 'gdi32.dll'
//  name 'GetFontResourceInfoW';

function LoadFontFile(fName: acString): THandle;
var
  FontCount: DWord;
  Stream: TMemoryStream;
begin
  if FileExists(fName) then begin
    Stream := TMemoryStream.Create;
    Stream.LoadFromFile(fName);
    FontCount := 1;
    Result := AddFontMemResourceEx(Stream.Memory, Stream.Size, nil, @FontCount);
    Stream.Free;
  end
  else
    Result := 0;
end;


procedure TFormFontStore.FormCreate(Sender: TObject);
begin
  Screen.Fonts
end;


type
  TacFileData = class(TObject)
  public
    FileName: string;
  end;


procedure TFormFontStore.InitEditor(aFontStore: TsFontStore);
var
  i: integer;
  fd: TacFileData;
begin
  WarningShown := ReadRegString(HKEY_CURRENT_USER, 'SOFTWARE\' + s_RegName, 'OtfWarnShown') = '1';
  if sDir = '' then begin
    sDir := ReadRegString(HKEY_CURRENT_USER, 'SOFTWARE\' + s_RegName, 'FontsPath');
    if not acDirExists(sDir) then
      sDir := GetAppPath;
  end;
  Caption := aFontStore.Owner.Name + s_Dot + aFontStore.Name;
  sDirectoryEdit1.Text := sDir;
  sListView1.Checkboxes := True;
  sEdit1.Text := sTestStr;
  FontStore := aFontStore;
  sFontStore1.Assign(FontStore);
  sListBox1.Items.BeginUpdate;
  sListBox1.Items.Clear;
  for i := 0 to sFontStore1.Fonts.Count - 1 do begin
    fd := TacFileData.Create;
    fd.FileName := sFontStore1.Fonts[i].FileName;
    sListBox1.Items.AddObject(sFontStore1.Fonts[i].FontName, fd);
  end;
  sListBox1.Items.EndUpdate;
  if sListBox1.Items.Count > 0 then
    sListBox1.ItemIndex := 0;

  ItemUpdated;
end;


procedure TFormFontStore.ItemUpdated;
begin
  Updating := True;
  sButton5.Enabled := sListBox1.Items.Count > 0;
  sListBox1.Enabled := sListBox1.Items.Count > 0;
  sButton4.Enabled := sListBox1.ItemIndex >= 0;
  sCheckBox3.Enabled := sListBox1.ItemIndex >= 0;
  sCheckBox3.AllowGrayed := sListBox1.ItemIndex < 0;
  if IsValidIndex(sListBox1.ItemIndex, sFontStore1.Fonts.Count) then
    sCheckBox3.Checked := sFontStore1.Fonts[sListBox1.ItemIndex].FontData.Size > 0
  else
    sCheckBox3.State := cbGrayed;

  Updating := False;
end;


procedure TFormFontStore.sButton3Click(Sender: TObject);
var
  i: integer;
  fd: TacFileData;
begin
  sListBox1.Items.BeginUpdate;
  for i := 0 to sListView1.Items.Count - 1 do
    if sListView1.Items[i].Checked {and (sListBox1.Items.IndexOf(sListView1.Items[i].Caption) < 0)} then begin
      fd := TacFileData.Create;
      fd.FileName := NormalDir(sDirectoryEdit1.Text) + sListView1.Items[i].SubItems[0];
      sListBox1.Items.AddObject(sListView1.Items[i].Caption, fd);
      with TacEmbeddedFont(sFontStore1.Fonts.Add) do begin
        FileName := fd.FileName;
        FontData.LoadFromFile(fd.FileName);
        FontName := sListView1.Items[i].Caption;
      end;
    end;

  sListBox1.Items.EndUpdate;
  if (sListBox1.ItemIndex < 0) and (sListBox1.Items.Count > 0) then
    sListBox1.ItemIndex := 0;

  ItemUpdated;
end;


procedure TFormFontStore.sButton4Click(Sender: TObject);
var
  OldNdx: integer;
begin
  if sListBox1.ItemIndex >= 0 then begin
    OldNdx := sListBox1.ItemIndex;
    sFontStore1.Fonts.Delete(sListBox1.ItemIndex);
    sListBox1.Items.Delete(sListBox1.ItemIndex);
    sListBox1.ItemIndex := math.min(OldNdx, sListBox1.Items.Count - 1);
  end;
  ItemUpdated;
end;


procedure TFormFontStore.sButton5Click(Sender: TObject);
begin
  sListBox1.Clear;
  sFontStore1.Fonts.Clear;
  ItemUpdated;
end;


procedure TFormFontStore.sCheckBox1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  for i := 0 to sListView1.Items.Count - 1 do
    sListView1.Items[i].Checked := False;
end;


procedure TFormFontStore.sCheckBox2MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  for i := 0 to sListView1.Items.Count - 1 do
    sListView1.Items[i].Checked := True;
end;


procedure TFormFontStore.sCheckBox3Click(Sender: TObject);
begin
  if not Updating and (sListBox1.ItemIndex >= 0) then
    if sCheckBox3.Checked then
      if FileExists(TacFileData(sListBox1.Items.Objects[sListBox1.ItemIndex]).FileName) then
        sFontStore1.Fonts[sListBox1.ItemIndex].FontData.LoadFromFile(TacFileData(sListBox1.Items.Objects[sListBox1.ItemIndex]).FileName)
      else begin
        sCheckBox3.Checked := False;
        ShowError('Can`t find the font file: ' + TacFileData(sListBox1.Items.Objects[sListBox1.ItemIndex]).FileName + '.');
      end
    else
      sFontStore1.Fonts[sListBox1.ItemIndex].FontData.Clear;
end;


procedure TFormFontStore.sDirectoryEdit1Change(Sender: TObject);
const
  Margin = 30;
var
  FileInfo: TacSearchRec;
  DosCode: Integer;
  sExt, sp: string;
{$IFNDEF ALITE}
  b: boolean;
{$ENDIF}              

  procedure AddFont(sName: string);
  var
    s: acString;
    li: TListItem;
  begin
    s := acgpGetFontName(sp + sName);
    if s <> '' then begin
      li := sListView1.Items.Add;
      with li do begin
        li.Caption := s;
        li.SubItems.Add(sName);
        li.Data := Pointer(LoadFontFile(sp + sName));
        li.Checked := False;
      end;
    end;
  end;

begin
  sListView1.Items.BeginUpdate;
  ClearListView;
  sp := NormalDir(sDirectoryEdit1.Text);
  if acDirExists(sp) then begin
    sDir := sp;
    DosCode := acFindFirst(sp + '*.*', faDirectory, FileInfo);
    try
      while DosCode = 0 do begin
        if (FileInfo.Name[1] <> s_Dot) and (FileInfo.Attr and faDirectory = 0) then begin
          sExt := UpperCase(ExtractFileExt(FileInfo.Name));
          if sExt = '.TTF' then
            AddFont(FileInfo.Name)
          else
            if sExt = '.OTF' then begin
              AddFont(FileInfo.Name);
{$IFNDEF ALITE}
              if not WarningShown then begin
                WarningShown := True;
                b := False;
                ShowCheckForm('Warning', sOTFWarning, sWarnRead, b);
                if b then
                  WriteRegString(HKEY_CURRENT_USER, 'SOFTWARE\' + s_RegName, 'OtfWarnShown', '1');
              end;
{$ELSE}
              WriteRegString(HKEY_CURRENT_USER, 'SOFTWARE\' + s_RegName, 'OtfWarnShown', '1');
{$ENDIF}              
            end;
        end;
        DosCode := acFindNext(FileInfo);
      end;
    finally
      acFindClose(FileInfo);
    end;
  end;
  sListView1.Items.EndUpdate;
  if sListView1.Items.Count > 0 then begin
    sListBox1.Enabled := True;
    sCheckBox1.Enabled := True;
    sCheckBox2.Enabled := True;
    sListView1.Enabled := True;
    sButton3.Enabled := True;
    sEdit1.Visible := True;
    sListView1.Selected := sListView1.Items[0];
  end;
end;


procedure TFormFontStore.sEdit1Change(Sender: TObject);
begin
  sTestStr := sEdit1.Text;
  sPanel1.SkinData.Invalidate;
end;

procedure TFormFontStore.SetCurrentName(const Value: string);
begin
  FCurrentName := Value;
  sPanel1.SkinData.Invalidate;
end;


procedure TFormFontStore.sListBox1Click(Sender: TObject);
begin
  ItemUpdated;
  if sListBox1.ItemIndex >= 0 then
    CurrentName := sListBox1.Items[sListBox1.ItemIndex]
  else
    CurrentName := '';
end;


procedure TFormFontStore.sListView1Change(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  if sListView1.Selected <> nil then
    CurrentName := sListView1.Selected.Caption
  else
    CurrentName := '';
end;


procedure TFormFontStore.sPanel1Paint(Sender: TObject; Canvas: TCanvas);
var
  i, X, Y, cy: Integer;
  ws: WideString;
  R: TRect;
begin
  if CurrentName <> '' then begin
    R := MkRect(sPanel1);
    InflateRect(R, -3, -3);
    if not sPanel1.SkinData.Skinned then
      FillDC(Canvas.Handle, R, acColorToRGB(clWindow));

    Canvas.Font.Size := Font.Size;
    Canvas.Font.Name := CurrentName;
    if sPanel1.SkinData.Skinned then
      Canvas.Font.Color := sPanel1.SkinData.CommonSkinData.gd[sPanel1.SkinData.SkinIndex].Props[0].FontColor.Color
    else
      Canvas.Font.Color := sPanel1.Font.Color;

    Canvas.Brush.Style := bsClear;
    X := 10;
    Y := 10;
    for i := 0 to 5 do begin
      cy := GetFontHeight(Canvas.Font.Handle);
      ws := sTestStr;
      ExtTextOutW(Canvas.Handle, X, Y, 0, nil, PWideChar(ws), Length(ws), nil);
      Canvas.Font.Size := Round(Canvas.Font.Size * 1.5);
      inc(Y, cy);
    end;
  end;
end;


procedure TFormFontStore.sSpeedButton1Click(Sender: TObject);
var
  sPath: string;
{$IFNDEF ALITE}
  PathDlg: TsPathDialog;
{$ENDIF}
begin
  sPath := sDirectoryEdit1.Text;
{$IFNDEF ALITE}
  if not (csDesigning in FontStore.ComponentState) then begin
    PathDlg := TsPathDialog.Create(Application);
    PathDlg.Root := 'rfMyComputer';
    PathDlg.Path := sPath;
    PathDlg.ShowRootBtns := True;
    PathDlg.Caption := 'Path to external fonts:';
    if PathDlg.Execute then
      sDirectoryEdit1.Text := PathDlg.Path;

    PathDlg.Free;
  end
  else
{$ENDIF}
    if SelectDirectory(sPath, [], -1) then
      sDirectoryEdit1.Text := sPath;
end;


procedure TFormFontStore.ClearListView;
var
  i: integer;
begin
  for i := 0 to sListView1.Items.Count - 1 do
    if sListView1.Items[0].Data <> nil then
      RemoveFontMemResourceEx(THandle(sListView1.Items[0].Data));

  sListView1.Items.Clear;
end;


var
  Lib: HModule = 0;
  ResStringRec: TResStringRec;

procedure TFormFontStore.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  WriteRegString(HKEY_CURRENT_USER, 'SOFTWARE\' + s_RegName, 'FontsPath', sDir);
end;


initialization
  // Load a localized preview text
  Lib := LoadLibrary('fontext.dll');
  ResStringRec.Module := {$IFDEF DELPHI5}@acNativeInt(Lib){$ELSE}@Lib{$ENDIF};
  ResStringRec.Identifier := 1285;
  sTestStr := LoadResString(@ResStringRec);
  if sTestStr = '' then
    sTestStr := 'Preview text 1234567890~';

  FreeLibrary(Lib);

finalization

end.
