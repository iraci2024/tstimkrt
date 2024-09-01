unit sImgListEditor;
{$I sDefs.inc}

{$IFNDEF ALITE}
  {$IFDEF DELPHI7UP}
    {$DEFINE COLORBG}
  {$ENDIF}
{$ENDIF}


interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ExtCtrls, ActnList, Buttons, ImgList, ExtDlgs,
  {$IFDEF DELPHI_XE2}UITypes, {$ENDIF}
  {$IFDEF COLORBG}sComboBoxes, {$ENDIF}
  sPanel, sSpeedButton, acAlphaImageList, sSkinProvider, sListView, sBitBtn, sButton,
  sEdit, Menus;

type
  TFormImgListEditor = class(TForm)
    sBitBtn5: TsBitBtn;
    sBitBtn6: TsBitBtn;
    sBitBtn7: TsBitBtn;
    ImageList1: TsAlphaImageList;
    OpenPictureDialog1: TOpenPictureDialog;
    ListView1: TsListView;
    SpeedButton1: TsSpeedButton;
    SpeedButton2: TsSpeedButton;
    SpeedButton3: TsSpeedButton;
    SpeedButton4: TsSpeedButton;
    sSkinProvider1: TsSkinProvider;
    SaveDialog1: TSaveDialog;
    SpeedButton5: TsSpeedButton;
    SpeedButton6: TsSpeedButton;
    sEdit1: TsEdit;
    PopupMenu1: TPopupMenu;
    S1: TMenuItem;
    procedure ListView1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ListView1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure FormShow(Sender: TObject);
    procedure ListView1Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure sBitBtn5Click(Sender: TObject);
    procedure sBitBtn6Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sBitBtn7Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure CheckScroll(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure sEdit1Change(Sender: TObject);
    procedure sColorBox1Change(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure S1Click(Sender: TObject);
  public
{$IFDEF COLORBG}
    sColorBox1: TsColorBox;
{$ENDIF}
    sImageList: TsAlphaImageList;
    procedure Loaded; override;
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DropFiles;
    procedure ListViewRefresh(DoLock: boolean; SelectedIndex: integer);
    procedure InitFromImgList(ImgList: TsAlphaImageList);
  end;


var
  FormImgListEditor: TFormImgListEditor;
  ScrollTimer: TTimer = nil;


implementation

uses
  CommCtrl, ShellAPI,
  {$IFNDEF ALITE}acPathDialog, sDialogs, {$ELSE}FileCtrl, {$ENDIF}
  acntUtils, sStoreUtils, sConst, sVCLUtils, sGraphUtils, sAlphaGraph, acDesignData, sSkinManager, sCommonData;

{$R *.DFM}

const
  scBorder = 8;


procedure StartScroll;
begin
  if ScrollTimer = nil then begin
    ScrollTimer := TTimer.Create(nil);
    ScrollTimer.OnTimer := FormImgListEditor.CheckScroll;
    ScrollTimer.Interval := 10;
  end;
end;


procedure EndScroll;
begin
  if ScrollTimer <> nil then
    FreeAndNil(ScrollTimer);
end;


procedure DoScroll(X, Y: integer);
var
  dx: integer;
begin
  if X > FormImgListEditor.ListView1.Width - scBorder then
    dx := 10
  else
    dx := iff(X < scBorder, -10, 0);

  if dx <> 0 then begin
    FormImgListEditor.ListView1.Scroll(dx, 0);
    FormImgListEditor.ListView1.Invalidate;
  end;
end;


procedure TFormImgListEditor.Loaded;
begin
  if DefaultManager <> nil then
    Scaled := DefaultManager.Options.ScaleMode = smVCL;

  inherited;
end;


procedure TFormImgListEditor.S1Click(Sender: TObject);
begin
{$IFDEF DELPHI7UP}
  ListView1.SelectAll;
  ListView1Click(ListView1);
{$ENDIF}
end;


procedure TFormImgListEditor.InitFromImgList(ImgList: TsAlphaImageList);
begin
  ImageList1.DoubleData := True;
  ImageList1.AcBeginUpdate;
  ImageList1.Clear;

  sImageList := ImgList;
  Caption := ImgList.Owner.Name + s_Dot + ImgList.GetNamePath;

  ImageList1.Width := ImgList.Width;
  ImageList1.Height := ImgList.Height;

  ImageList_SetBkColor(ImageList1.Handle, ColorToRGB(ListView1.Color));

  ImageList1.CopyImages(ImgList);

  ImageList1.AcEndUpdate;
  ListViewRefresh(True, -1);
end;


procedure TFormImgListEditor.ListView1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  li: TListItem;
begin
  li := ListView1.GetItemAt(X, Y);
  Accept := (li <> nil) and (li <> ListView1.ItemFocused);
  if (X < scBorder) or (X > ListView1.Width - scBorder) then
    StartScroll
end;


procedure TFormImgListEditor.ListView1DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  li: TListItem;
begin
  li := ListView1.GetItemAt(X, Y);
  ImageList1.Items[ListView1.ItemFocused.Index].Index := li.Index;
  ImageList1.Move(ListView1.ItemFocused.Index, li.Index);
  EndScroll;
end;


procedure TFormImgListEditor.ListViewRefresh(DoLock: boolean; SelectedIndex: integer);
var
  i: integer;
begin
  if DoLock then
    SendMessage(ListView1.Handle, WM_SETREDRAW, 0, 0);

  ListView1.Items.Clear;
  for i := 0 to ImageList1.Count - 1 do begin
    ListView1.Items.Add.Selected;
    ListView1.Items.Item[i].ImageIndex := i;
    ListView1.Items.Item[i].Caption := IntToStr(i);
  end;
  if IsValidIndex(SelectedIndex, ListView1.Items.Count) then
    ListView1.Items[SelectedIndex].Selected := True;

  if DoLock then
    SendMessage(ListView1.Handle, WM_SETREDRAW, 1, 0);

  SpeedButton3.Enabled := ImageList1.Count > 0;
  SpeedButton4.Enabled := ImageList1.Count > 0;
end;


procedure TFormImgListEditor.FormShow(Sender: TObject);
var
  R: TRect;
  s: string;
  iPadding: integer;
begin
{$IFDEF COLORBG}
  sColorBox1 := TsColorBox.Create(Self);
  sColorBox1.Visible := False;
  sColorBox1.Parent := Self;
  sColorBox1.Left := 448;
  sColorBox1.Top := 114;
  sColorBox1.Width := ScaleInt(50, sColorBox1.SkinData);
  sColorBox1.Height := ScaleInt(20, sColorBox1.SkinData);
  sColorBox1.BoundLabel.Caption := 'BG color:';
  sColorBox1.BoundLabel.Active := True;
  sColorBox1.Style := [cbStandardColors, cbExtendedColors, cbSystemColors, cbIncludeNone, cbCustomColor];
  sColorBox1.ShowColorName := False;
  sColorBox1.NoneColorColor := clNone;
  sColorBox1.Selected := clNone;
  sColorBox1.PopupMode := pmPickColor;
  sColorBox1.Anchors := [akRight, akBottom];
  sColorBox1.OnChange := sColorBox1Change;
  sColorBox1.Visible := True;
{$ENDIF}
  DragAcceptFiles(Handle, True);
  s := ReadRegString(HKEY_CURRENT_USER, 'SOFTWARE\' + s_RegName, 'ImgListEditorLeft');
  if s <> '' then
    try
      R.Left := StrToInt(s);
      s := ReadRegString(HKEY_CURRENT_USER, 'SOFTWARE\' + s_RegName, 'ImgListEditorTop');
      R.Top := StrToInt(s);
      s := ReadRegString(HKEY_CURRENT_USER, 'SOFTWARE\' + s_RegName, 'ImgListEditorWidth');
      R.Right := StrToInt(s);
      s := ReadRegString(HKEY_CURRENT_USER, 'SOFTWARE\' + s_RegName, 'ImgListEditorHeight');
      R.Bottom := StrToInt(s);
      SetBounds(R.Left, R.Top, R.Right, R.Bottom);
    finally
    end;

  iPadding := ScaleInt(8, sSkinProvider1.SkinData);
  sBitBtn5.Left := ClientWidth - sBitBtn5.Width - iPadding;
  sBitBtn6.Left := sBitBtn5.Left;
  sBitBtn7.Left := sBitBtn5.Left;
  sEdit1.Top := ClientHeight - sEdit1.Height - iPadding;
{$IFDEF COLORBG}
  sColorBox1.Top := sEdit1.Top;
  sColorBox1.Left := sBitBtn5.Left;
  sColorBox1.Selected := clNone;
  sEdit1.Width := sColorBox1.Left - sEdit1.Left - ScaleInt(20, sSkinProvider1.SkinData) - sColorBox1.BoundLabel.FTheLabel.Width;
{$ELSE}
  sEdit1.Width := ClientWidth - sEdit1.Left - ScaleInt(60, sSkinProvider1.SkinData);
{$ENDIF}
  ListView1.Height := sEdit1.Top - iPadding - ListView1.Top;
  ListView1.Width := sBitBtn5.Left - iPadding - ListView1.Left;

  ListView1.Anchors := ListView1.Anchors + [akBottom, akRight];
  sBitBtn5.Anchors  := sBitBtn5.Anchors + [akRight] - [akLeft];
  sBitBtn6.Anchors  := sBitBtn6.Anchors + [akRight] - [akLeft];
  sBitBtn7.Anchors  := sBitBtn7.Anchors + [akRight] - [akLeft];
  sEdit1.Anchors := [akLeft, akBottom, akRight];

  if BiDiMode = bdRightToLeft then
    ReflectControls(Self, True);
end;


procedure TFormImgListEditor.ListView1Click(Sender: TObject);
begin
{$IFDEF DELPHI7UP}
  SpeedButton2.Enabled := ListView1.ItemIndex >= 0;
  if ListView1.ItemIndex >= 0 then
    sEdit1.Text := ImageList1.Items[ListView1.ItemIndex].Text
  else
    sEdit1.Text := '';
{$ELSE}
  SpeedButton2.Enabled := ListView1.Selected <> nil;
  if SpeedButton2.Enabled then
    sEdit1.Text := ImageList1.Items[ListView1.Selected.Index].Text
  else
    sEdit1.Text := '';
{$ENDIF}
  SpeedButton3.Enabled := ImageList1.Count > 0;
  SpeedButton4.Enabled := ImageList1.Count > 0;
  SpeedButton6.Enabled := SpeedButton2.Enabled;
  SpeedButton5.Enabled := SpeedButton2.Enabled;
  sEdit1.Enabled := ImageList1.Count > 0;
end;


procedure TFormImgListEditor.SpeedButton1Click(Sender: TObject);
var
  i: integer;
begin
  if OpenPictureDialog1.Execute then begin
    for i := 0 to OpenPictureDialog1.Files.Count - 1 do
      ImageList1.LoadFromfile(OpenPictureDialog1.Files[i]);

    ListViewRefresh(True, ImageList1.Count - 1);
  end;
end;


procedure TFormImgListEditor.SpeedButton2Click(Sender: TObject);
var
  Ndx, j: integer;
begin
  if ListView1.Selected <> nil then begin
    ListView1.Items.BeginUpdate;
    SendMessage(ListView1.Handle, WM_SETREDRAW, 0, 0);
    Ndx := ListView1.Selected.Index;
    ImageList1.AcBeginUpdate;
    for j := ListView1.Items.Count - 1 downto 0 do
      if ListView1.Items[j].Selected then begin
        ListView1.Items.Delete(j);
        ImageList1.Delete(j);
      end;

    if ListView1.Items.Count > 0 then
      for j := 0 to ListView1.Items.Count - 1 do begin
        ListView1.Items[j].Caption := IntToStr(j);
        if j >= Ndx then
          ListView1.Items[j].ImageIndex := ListView1.Items[j].ImageIndex - 1;
      end;

    ListView1.Arrange(arAlignTop);
    ImageList1.AcEndUpdate;
    if Ndx >= ListView1.Items.Count then
      Ndx := ListView1.Items.Count - 1;

    ListViewRefresh(False, Ndx);
    ListView1Click(ListView1); // Refresh controls accessibility

    SendMessage(ListView1.Handle, WM_SETREDRAW, 1, 0);
    ListView1.Items.EndUpdate;
  end;
end;


procedure TFormImgListEditor.SpeedButton3Click(Sender: TObject);
begin
  if MessageDlg('Do you want to remove all icons?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then begin
    ImageList1.AcBeginUpdate;
    ImageList1.Clear;
    ImageList1.Items.Clear;
    ImageList1.AcEndUpdate;
    ListViewRefresh(True, -1);
    ListView1Click(ListView1); // Refresh controls accessibility
  end;
end;


procedure TFormImgListEditor.sBitBtn5Click(Sender: TObject);
begin
  if Assigned(sImageList) then begin
    ImageList_SetBkColor(ImageList1.Handle, CLR_NONE);
    sImageList.AcBeginUpdate;
    sImageList.Clear;
    sImageList.CopyImages(ImageList1);
    sImageList.AcEndUpdate;
  end;
  Close;
end;


procedure TFormImgListEditor.sBitBtn6Click(Sender: TObject);
begin
  Close;
end;


procedure TFormImgListEditor.WMDropFiles(var Msg: TWMDropFiles);
const
  maxlen = 254;
var
  h: THandle;
  i, num: integer;
  pchr: array [0..maxlen] of acChar;
  fname: acString;
begin
  h := Msg.Drop;
  num := DragQueryFile(h, Cardinal(-1), nil, 0);
  if num > 0 then begin
    for i := 0 to num - 1 do begin
{$IFDEF TNTUNICODE}
      DragQueryFileW(h, i, pchr, maxlen);
{$ELSE}
      DragQueryFile(h, i, pchr, maxlen);
{$ENDIF}
      fname := acString(pchr);
      ImageList1.LoadFromfile(fname);
    end;
    ListViewRefresh(True, -1);
  end;
  DragFinish(h);
end;


procedure TFormImgListEditor.FormDestroy(Sender: TObject);
begin
  WriteRegInteger('SOFTWARE\' + s_RegName, 'ImgListEditorLeft', Left);
  WriteRegInteger('SOFTWARE\' + s_RegName, 'ImgListEditorTop', Top);
  WriteRegInteger('SOFTWARE\' + s_RegName, 'ImgListEditorWidth', Width);
  WriteRegInteger('SOFTWARE\' + s_RegName, 'ImgListEditorHeight', Height);

  DragAcceptFiles(Handle, False);
  EndScroll;
end;


procedure TFormImgListEditor.FormResize(Sender: TObject);
begin
  ListView1.Arrange(arAlignTop);
end;


procedure TFormImgListEditor.ListView1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = 46 then
    SpeedButton2.Click;
end;


procedure TFormImgListEditor.sBitBtn7Click(Sender: TObject);
begin
  if Assigned(sImageList) then begin
    ImageList_SetBkColor(ImageList1.Handle, CLR_NONE);
    sImageList.Assign(ImageList1);
  end;
end;


procedure TFormImgListEditor.sColorBox1Change(Sender: TObject);
begin
{$IFDEF COLORBG}
  ListView1.SkinData.CustomColor := True;
  ListView1.Color := sColorBox1.Selected;
{$ENDIF}
end;


procedure TFormImgListEditor.SpeedButton4Click(Sender: TObject);
var
  j: integer;
  s, sPath: string;
{$IFNDEF ALITE}
  PathDlg: TsPathDialog;
{$ENDIF}
begin
  sPath := '.\';
{$IFNDEF ALITE}
  PathDlg := TsPathDialog.Create(Application);
  PathDlg.Path := sPath;
  PathDlg.Root := 'rfMyComputer';
  PathDlg.ShowRootBtns := True;
  PathDlg.Caption := 'Export glyphs to:';
  if PathDlg.Execute then begin
    sPath := PathDlg.Path;
{$ELSE}
  if SelectDirectory(sPath, [], -1) then begin
{$ENDIF}
    for j := 0 to ListView1.Items.Count - 1 do
      if ImageList1.Items[j].ImgData <> nil then begin
        s := iff(ImageList1.Items[j].ImageName <> '', ImageList1.Items[j].ImageName, IntToStr(j));
        ImageList1.Items[j].ImgData.SaveToFile(sPath + s_Slash + s + s_Dot + iff(ImageList1.Items[j].ImageFormat = ifPNG, acPngExt, acIcoExt));
      end
  end;
{$IFNDEF ALITE}
  PathDlg.Free;
{$ENDIF}
end;


procedure TFormImgListEditor.SpeedButton5Click(Sender: TObject);
var
  Index: integer;
  Icon: HICON;
  iFormat: TsImageFormat;
  Bmp: TBitmap;
begin
  if OpenPictureDialog1.Execute then
    if GetImageFormat(OpenPictureDialog1.FileName, iFormat) then begin
      Icon := 0;
{$IFDEF DELPHI7UP}
      Index := ListView1.ItemIndex;
{$ELSE}
      Index := ListView1.Selected.Index;
{$ENDIF}
      if Index < 0 then
        Exit;

      ImageList1.Items[Index].ImageFormat := iFormat;
      ImageList1.Items[Index].ImgData.Clear;
      ImageList1.Items[Index].ImgData.LoadFromFile(OpenPictureDialog1.FileName);
      FreeAndNil(ImageList1.Items[Index].CacheBmp);
      case iFormat of
        ifPNG: begin
          Bmp := TBitmap.Create;
          LoadBmpFromPngFile(Bmp, OpenPictureDialog1.FileName);
          Icon := MakeIcon32(Bmp);
          Bmp.Free;
        end;

        ifICO:
          Icon := {$IFDEF TNTUNICODE}ExtractIconW{$ELSE}ExtractIcon{$ENDIF}(hInstance, PacChar(OpenPictureDialog1.FileName), 0);
      end;
      if Icon <> 0 then begin
        ImageList_ReplaceIcon(ImageList1.Handle, Index, Icon);
        DestroyIcon(Icon);
        ListViewRefresh(True, -1);
{$IFDEF DELPHI7UP}
        ListView1.ItemIndex := Index;
{$ELSE}
        ListView1.Selected := ListView1.Items[Index];
{$ENDIF}
      end;
    end
    else
      ShowError('Unsupported file format');
end;


procedure TFormImgListEditor.SpeedButton6Click(Sender: TObject);
var
  j: integer;
begin
  if ListView1.Selected <> nil then begin
    j := ListView1.Selected.Index;
    if ImageList1.Items[j].ImageFormat = ifPNG then begin
      SaveDialog1.Filter := 'Portable Network Graphics|*.png';
      SaveDialog1.DefaultExt := acPngExt;
    end
    else begin
      SaveDialog1.Filter := 'Icons|*.ico';
      SaveDialog1.DefaultExt := acIcoExt
    end;
    if ImageList1.Items[j].ImageName <> '' then
      SaveDialog1.FileName := ImageList1.Items[j].ImageName
    else
      SaveDialog1.FileName := IntToStr(j);

    if SaveDialog1.Execute then
      ImageList1.Items[j].ImgData.SaveToFile(SaveDialog1.FileName);
  end;
end;


procedure TFormImgListEditor.CheckScroll(Sender: TObject);
var
  P: TPoint;
begin
  if (Mouse.Capture <> 0) and (ScrollTimer <> nil) and not (csDestroying in ScrollTimer.ComponentState) and (FormImgListEditor <> nil) then begin
    ScrollTimer.Enabled := False;
    P := ListView1.ScreenToClient(acMousePos);
    DoScroll(P.X, P.Y);
    if ScrollTimer <> nil then
      ScrollTimer.Enabled := True;
  end;
end;


procedure TFormImgListEditor.sEdit1Change(Sender: TObject);
begin
{$IFDEF DELPHI7UP}
  if ListView1.ItemIndex >= 0 then
    ImageList1.Items[ListView1.ItemIndex].Text := sEdit1.Text;
{$ELSE}
  if ListView1.Selected <> nil then
    ImageList1.Items[ListView1.Selected.Index].Text := sEdit1.Text;
{$ENDIF}
end;

end.
