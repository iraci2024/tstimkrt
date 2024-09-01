unit acCharListEditor;
{$I sDefs.inc}

interface

uses
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  {$IFDEF  DELPHI_XE2} UITypes, {$ENDIF}
  Grids, Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, ExtCtrls, StdCtrls, ImgList, Buttons, Mask,
  acAlphaImageList, sListView, sPanel, sPageControl, sButton, sComboBox, sEdit, sComboBoxes,  sGroupBox, sCheckBox, sSkinProvider,
  sConst, sSpeedButton, sFontCtrls, sListBox, sMaskEdit, sCustomComboEdit, sToolEdit, sCheckListBox, sLabel, sStatusBar, sDialogs, sBitBtn,
  sTrackBar, Menus, acPopupCtrls;


type
  TacItemsIterProc = function(Item: TacCharListItem; Data: real): boolean;


  TFormCharListEditor = class(TForm)
    CharImageList: TsCharImageList;
    sSkinProvider1: TsSkinProvider;
    sOpenDialog1: TsOpenDialog;
    PopupMenu1: TPopupMenu;
    S1: TMenuItem;
    sPanel10: TsPanel;
    sPanel2: TsPanel;
    sPageControl1: TsPageControl;
    sTabSheet1: TsTabSheet;
    sListBox1: TsListBox;
    sComboBox1: TsComboBox;
    sTabSheet2: TsTabSheet;
    sListBox2: TsListBox;
    sPanel4: TsPanel;
    sSpeedButton5: TsSpeedButton;
    sSpeedButton6: TsSpeedButton;
    sSpeedButton7: TsSpeedButton;
    sPanel5: TsPanel;
    sPanel7: TsPanel;
    sButton1: TsBitBtn;
    sButton2: TsBitBtn;
    sPanel1: TsPanel;
    sPanel3: TsPanel;
    sPanel6: TsPanel;
    sSpeedButton1: TsSpeedButton;
    sSpeedButton2: TsSpeedButton;
    sSpeedButton4: TsSpeedButton;
    sSpeedButton3: TsSpeedButton;
    sListView1: TsListView;
    sPanel9: TsPanel;
    sPanel11: TsPanel;
    sPanel8: TsPanel;
    sPageControl2: TsPageControl;
    sTabSheet3: TsTabSheet;
    sLabel3: TsLabel;
    sLabel1: TsLabel;
    sLabel4: TsLabel;
    sLabel2: TsLabel;
    sColorBox1: TsColorBox;
    sTrackBar1: TsTrackBar;
    sCheckBox1: TsCheckBox;
    sTrackBar2: TsTrackBar;
    sTabSheet4: TsTabSheet;
    sTabSheet5: TsTabSheet;
    sTrackBar3: TsTrackBar;
    sLabel5: TsLabel;
    sLabel6: TsLabel;
    sGroupBox1: TsGroupBox;
    sColorBox3: TsColorBox;
    CharImageSmall: TsCharImageList;
    sSpeedButton8: TsSpeedButton;
    sLabel8: TsLabel;
    sSpeedButton9: TsSpeedButton;
    sColorBox2: TsColorBox;
    sTrackBar4: TsTrackBar;
    sLabel9: TsLabel;
    sLabel7: TsLabel;
    sSplitView1: TsSplitView;
    sPanel12: TsPanel;
    sSpeedButton10: TsSpeedButton;
    sSpeedButton11: TsSpeedButton;
    procedure FormShow(Sender: TObject);
    procedure sButton1Click(Sender: TObject);
    procedure sListBox1Click(Sender: TObject);
    procedure sListView1Click(Sender: TObject);
    procedure sComboBox1Change(Sender: TObject);
    procedure sSpeedButton2Click(Sender: TObject);
    procedure sSpeedButton4Click(Sender: TObject);
    procedure sSpeedButton3MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure sSpeedButton1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure sPageControl1Change(Sender: TObject);
    procedure sSpeedButton5Click(Sender: TObject);
    procedure sSpeedButton6Click(Sender: TObject);
    procedure sListBox2Click(Sender: TObject);
    function CanAddChar: boolean;
    procedure CheckScroll(Sender: TObject);
    procedure sListView1DragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure sListView1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure sSpeedButton7Click(Sender: TObject);
    procedure sListBox2BeforeItemDraw(aDC: HDC; aIndex: Integer; var aRect: TRect);
    procedure FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure sSpeedButton1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure sTrackBar1Change(Sender: TObject);
    procedure sTrackBar2Change(Sender: TObject);
    procedure sColorBox1Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sListView1Resize(Sender: TObject);
    procedure sCheckBox1Click(Sender: TObject);
    procedure S1Click(Sender: TObject);
    procedure sTrackBar3UserChange(Sender: TObject);
    procedure sColorBox3Change(Sender: TObject);
    procedure sPageControl2PageChanging(Sender: TObject; NewPage: TsTabSheet; var AllowChange: Boolean);
    procedure sSpeedButton8MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure sTrackBar4Change(Sender: TObject);
    procedure sSpeedButton9Click(Sender: TObject);
    procedure sColorBox2Change(Sender: TObject);
    procedure sPanel12Paint(Sender: TObject; Canvas: TCanvas);
    procedure sSpeedButton10Click(Sender: TObject);
    procedure sGroupBox1CheckBoxChanged(Sender: TObject);
    procedure sSpeedButton11Click(Sender: TObject);
  protected
    SrcImgListSize: TSize;
    ControlsUpdating: boolean;
    SourceList: TsCharImageList;
    procedure InitControls;
    procedure RefillListView;
    function FontIsEmbedded(AIndex: integer): boolean;
    procedure AddNewFont(FileName: string);
    procedure DeleteFont(Ndx: integer);
    procedure UpdateBtnsListBox2;
    procedure AddAwesome(AddList: boolean = True);
    procedure ShowCharMap(Sender: TsSpeedButton);

    procedure NewCharSelected (Sender: TObject; CharCode: Word; Charset: TFontCharset; FontName: string);
    procedure MainCharChanged (Sender: TObject; CharCode: Word; Charset: TFontCharset; FontName: string);
    procedure AddedCharChanged(Sender: TObject; CharCode: Word; Charset: TFontCharset; FontName: string);

    procedure MainFontChanged(Sender: TObject; iCharset, iFont: integer);

    procedure IterateItems(CallBack: TacItemsIterProc; Data: real);
    procedure UpdateAddedControls;
  public
    WarningShown: boolean;
    CharGrid: TDrawGrid;
    procedure Loaded; override;
    procedure InitFontsList;
    procedure SetFont(iCharSet, iFont: integer; var cid: TacCharItemData);
    procedure AddNewGlyph(aChar: Word; CharsetNdx: integer = -1; FontNdx: integer = -1);
    procedure InitFromImgList(ImgList: TsCharImageList);
  end;


const
  iEMBEDDED = 19;
  VisibleGlyphSize = 32;
  arCharsets: array [0 .. iEMBEDDED] of Byte = (ANSI_CHARSET, DEFAULT_CHARSET, SYMBOL_CHARSET, SHIFTJIS_CHARSET, HANGEUL_CHARSET, GB2312_CHARSET, CHINESEBIG5_CHARSET, OEM_CHARSET, JOHAB_CHARSET,
    HEBREW_CHARSET, ARABIC_CHARSET, GREEK_CHARSET, TURKISH_CHARSET, VIETNAMESE_CHARSET, THAI_CHARSET, EASTEUROPE_CHARSET, RUSSIAN_CHARSET, MAC_CHARSET, BALTIC_CHARSET, DEFAULT_CHARSET);

var
  FormCharListEditor: TFormCharListEditor;
  ScrollTimer: TTimer = nil;

function EnumFontCallback(lpelfe: PLogFont; lpntme: PNewTextMetricEX; FontType: DWORD; lp: LPARAM): Integer; stdcall;

implementation

uses math,
  {$IFDEF TNTUNICODE}TntComCtrls, {$ENDIF}
  acntUtils, acCharMap, acPopupController, sVCLUtils, acFontStore, acDesignData, CommCtrl, sGraphUtils, sSkinManager,
  acgpUtils, sCommonData, sStoreUtils, sStyleSimply;

{$R *.dfm}

procedure StartScroll;
begin
  if ScrollTimer = nil then begin
    ScrollTimer := TTimer.Create(nil);
    ScrollTimer.OnTimer := FormCharListEditor.CheckScroll;
    ScrollTimer.Interval := 10;
  end;
end;


procedure EndScroll;
begin
  if ScrollTimer <> nil then
    FreeAndNil(ScrollTimer);
end;


procedure TFormCharListEditor.AddAwesome(AddList: boolean = True);
begin
{$IFNDEF NOFONTRES}
  if CharImageList.EmbeddedFonts.Count = 0 then begin
    if AddList then
      sListBox2.Items.Add(s_FontAwesome);

    with TacEmbeddedFont(CharImageList.EmbeddedFonts.Add) do
      FontName := s_FontAwesome;
  end;
{$ENDIF}
end;


procedure TFormCharListEditor.AddedCharChanged(Sender: TObject; CharCode: Word; Charset: TFontCharset; FontName: string);
begin
  Delay(100);
  CharImageList.Items[FormCharListEditor.sListView1.Selected.Index].AddedChar := CharCode;
  CharImageList.Items[FormCharListEditor.sListView1.Selected.Index].AddedCharset := Charset;
  CharImageList.Items[FormCharListEditor.sListView1.Selected.Index].AddedFontName := FontName;
  CharImageList.UpdateStd(FormCharListEditor.sListView1.Selected.Index);
  FormCharMap.Close;
  UpdateAddedControls;
end;


procedure TFormCharListEditor.AddNewFont(FileName: string);
var
  s: string;
  b: boolean;
begin
  s := acgpGetFontName(FileName);
  if not WarningShown then begin
    WarningShown := True;
    b := False;
    ShowCheckForm('Warning', sOTFWarning, sWarnRead, b);
    if b then
      WriteRegString(HKEY_CURRENT_USER, 'SOFTWARE\' + s_RegName, 'OtfWarnShown', '1');
  end;
  if CharImageList.AddEmbeddedFont(FileName, s) then
    sListBox2.Items.Add(s);
end;


procedure TFormCharListEditor.AddNewGlyph(aChar: Word; CharsetNdx: integer = -1; FontNdx: integer = -1);
var
  cid: TacCharItemData;
  li: {$IFDEF TNTUNICODE}TTntListItem{$ELSE}TListItem{$ENDIF};
begin
  ControlsUpdating := True;
  SetFont(CharsetNdx, FontNdx, cid);

  cid.Orientation := 0;
  cid.Pitch := fpDefault;
  cid.ScalingFactor := GetDefScaling(cid.Charset, cid.FontName);
  cid.OffsetY := 0;
  cid.Style := [];
  cid.Char := aChar;
  cid.Color := clNone;
  cid.Angle := 0;
  cid.DrawContour := False;

  cid.AddedOffsetX := 0;
  cid.AddedOffsetY := 0;
  cid.AddedSize := 0;
  cid.AddedChar := 0;
  cid.SecondColor := clNone;
  cid.AddedColor := clNone;
  cid.AddedCharset := DEFAULT_CHARSET;

  li := sListView1.Items.Add;
  li.ImageIndex := CharImageList.AddItem(cid);
  li.Caption := IntToStr(li.Index);

  sListView1.Selected := nil;
  sListView1.Selected := li;
  ControlsUpdating := False;
  sListView1Click(sListView1);
end;


function TFormCharListEditor.CanAddChar: boolean;
begin
  Result := (sListBox1.ItemIndex >= 0) or (sListBox2.ItemIndex >= 0)
end;


const
  scBorder = 8;

procedure DoScroll(X, Y: integer);
var
  dx: integer;
begin
  if X > FormCharListEditor.sListView1.Width - scBorder then
    dx := 10
  else
    if X < scBorder then
      dx := -10
    else
      dx := 0;

  if dx <> 0 then begin
    FormCharListEditor.sListView1.Scroll(dx, 0);
    FormCharListEditor.sListView1.Invalidate;
  end;
end;


procedure TFormCharListEditor.CheckScroll(Sender: TObject);
var
  P: TPoint;
begin
  if (Mouse.Capture <> 0) and (ScrollTimer <> nil) and not (csDestroying in ScrollTimer.ComponentState) and (FormCharListEditor <> nil) then begin
    ScrollTimer.Enabled := False;
    P := sListView1.ScreenToClient(acMousePos);
    DoScroll(P.X, P.Y);
    if ScrollTimer <> nil then
      ScrollTimer.Enabled := True;
  end;
end;


procedure EnableCtrls(Ctrl: TControl; Data: Integer);
begin
  if Ctrl.Tag = 0 then
    Ctrl.Enabled := boolean(Data);
end;


function TFormCharListEditor.FontIsEmbedded(AIndex: integer): boolean;
begin
  Result := False;
end;


procedure TFormCharListEditor.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if FormCharMap <> nil then
    FreeAndNil(FormCharMap);
end;


procedure TFormCharListEditor.FormMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if (FormCharMap <> nil) and IsWindowVisible(FormCharMap.Handle) then begin
    if WheelDelta > 0 then
      FormCharMap.Grid.TopRow := max(0, FormCharMap.Grid.TopRow - 1)
    else
      FormCharMap.Grid.TopRow := min(FormCharMap.Grid.RowCount - FormCharMap.Grid.ClientHeight div FormCharMap.Grid.DefaultRowHeight, FormCharMap.Grid.TopRow + 1);

    Handled := True;
  end;
end;


type
  TAccessCharImageList = class(TsCharImageList);

procedure TFormCharListEditor.FormShow(Sender: TObject);
var
  i, j: integer;
  TmpItem: boolean;
  SrcList: TAccessCharImageList;
begin
  CharImageList.PixelsPerInch := 96;

  SrcList := TAccessCharImageList(SourceList);
  SrcImgListSize.cx := SrcList.SavedWidth;
  SrcImgListSize.cy := SrcList.SavedHeight;
  i := MulDiv(VisibleGlyphSize, GetPPI(sSkinProvider1.SkinData), 96);

  CharImageList.acBeginUpdate;
  CharImageList.AllowScale := False;
  CharImageList.Height := i;
  CharImageList.Width := CharImageList.Height * 110 div 100;
  TmpItem := CharImageList.Count = 0;
  if TmpItem then // Add temporary item for grid autosizing
    with CharImageList do
      TacCharListItem(Items.Add).Char := 0;

  CharImageList.GenerateStdList;
  CharImageList.acEndUpdate;

  sListView1.ViewStyle := vsIcon;
  if TmpItem then begin // Delete temporary item
    CharImageList.Clear;
    RefillListView;
  end;

  // Add fonts
  sListBox2.Items.BeginUpdate;
  AddAwesome(False);
  for i := 0 to CharImageList.EmbeddedFonts.Count - 1 do
    sListBox2.Items.Add(CharImageList.EmbeddedFonts[i].FontName);

  if acFontStoreList <> nil then
    for i := 0 to acFontStoreList.Count - 1 do
      for j := 0 to TsFontStore(acFontStoreList[i]).Fonts.Count - 1 do
        sListBox2.Items.Add(TsFontStore(acFontStoreList[i]).Fonts[j].FontName);

  sListBox2.Items.EndUpdate;

  // Select charset
  sComboBox1.ItemIndex := arCharsets[SYMBOL_CHARSET];
  // Get fonts with selected charset
  sComboBox1Change(sComboBox1);
  // Select default font
  sListBox2.ItemIndex := 0;
{$IFDEF DELPHI6UP}
  if sListView1.Items.Count > 0 then
    sListView1.ItemIndex := 0
  else
    IterateControls(sPanel2, 0, EnableCtrls);
{$ENDIF}

  sListView1Click(sListView1);
  if sListBox1.ItemIndex >= 0 then
    sListBox1Click(sListBox1)
  else
    if sListBox2.ItemIndex >= 0 then begin
      sListBox2Click(sListBox2);
      sPageControl1.ActivePageIndex := 1;
    end;

  UpdateBtnsListBox2;
  sPanel7.Top := Height;
end;


procedure TFormCharListEditor.InitControls;
var
  i: integer;
begin
  WarningShown := ReadRegString(HKEY_CURRENT_USER, 'SOFTWARE\' + s_RegName, 'OtfWarnShown') = '1';
  Caption := SourceList.Owner.Name + s_Dot + SourceList.Name + ' (' + IntToStr(SrcImgListSize.cx) + 'x' + IntToStr(SrcImgListSize.cy) + ')';
  sSpeedButton10.Caption := 'Preview in ' + IntToStr(SrcImgListSize.cx) + 'x' + IntToStr(SrcImgListSize.cy) + '';
{$IFDEF DELPHI6UP}
  if IsValidIndex(sListView1.ItemIndex, CharImageList.Items.Count) then begin
{$ELSE}
  if (sListView1.Selected <> nil) and IsValidIndex(sListView1.Selected.Index, CharImageList.Items.Count) then begin
{$ENDIF}
    ControlsUpdating := True;

    i := CharImageList.FontIndex(CharImageList.Items[sListView1.Selected.Index].FontName);
    if i >= 0 then begin // Embedded
      sListBox1.ItemIndex := -1;
      sListBox2.ItemIndex := i;
      sListBox2Click(sListBox2);
      sPageControl1.ActivePageIndex := 1;
    end
    else begin
      sListBox2.ItemIndex := -1;
      if CharImageList.Items[sListView1.Selected.Index].Charset <> arCharSets[sComboBox1.ItemIndex] then begin
        for i := 0 to sComboBox1.Items.Count - 1 do
          if CharImageList.Items[sListView1.Selected.Index].Charset = arCharSets[i] then begin
            sComboBox1.ItemIndex := i;
            Break;
          end;

        sComboBox1Change(sComboBox1);
      end;
      sListBox1.ItemIndex := sListBox1.Items.IndexOf(CharImageList.Items[sListView1.Selected.Index].FontName);
      sPageControl1.ActivePageIndex := 0;
    end;
    sTabSheet5.Caption := 'Char - 0x' + IntToHex(ord(CharImageList.Items[sListView1.Selected.Index].Char), 4);
    sTrackBar1.Position := Round((CharImageList.Items[sListView1.Selected.Index].ScalingFactor - 1) * 100);
    sTrackBar2.Position := CharImageList.Items[sListView1.Selected.Index].OffsetY;
    sTrackBar3.Position := CharImageList.Items[sListView1.Selected.Index].Angle;
    sTrackBar4.Position := CharImageList.Items[sListView1.Selected.Index].AddedSize;
    sCheckBox1.Checked := CharImageList.Items[sListView1.Selected.Index].DrawContour;
    sLabel3.Caption := 'X ' + FloatToStr(CharImageList.Items[sListView1.Selected.Index].ScalingFactor);
    sLabel4.Caption := FloatToStr(sTrackBar2.Position);
    sLabel6.Caption := FloatToStr(sTrackBar3.Position);
    sLabel7.Caption := IntToStr(sTrackBar4.Position) + '%';
    sColorBox1.Selected := CharImageList.Items[sListView1.Selected.Index].Color;
    sGroupBox1.Checked := (CharImageList.Items[sListView1.Selected.Index].SecondColor <> clNone);
    if sGroupBox1.Checked then
      sColorBox3.Selected := CharImageList.Items[sListView1.Selected.Index].SecondColor
    else
      sColorBox3.Selected := sColorBox1.Selected;

    sColorBox2.Selected := CharImageList.Items[sListView1.Selected.Index].AddedColor;
    ControlsUpdating := False;
  end
  else begin
    sTrackBar1.Position := 0;
    sTrackBar2.Position := 0;
    sTrackBar3.Position := 0;
    sTrackBar4.Position := 50;
    sCheckBox1.Checked := False;
    sLabel3.Caption := 'X 1';
    sLabel4.Caption := '0';
    sLabel6.Caption := '0';
    sLabel7.Caption := '%';
    sGroupBox1.Checked := False;
    sColorBox1.Selected := clNone;
    sColorBox2.Selected := clNone;
    sColorBox3.Selected := clNone;
    sTabSheet5.Caption := 'Char - none';
  end;
end;


procedure TFormCharListEditor.InitFontsList;
begin
  sPanel11.Visible := False;
  sPanel3.Visible := False;
  sPanel1.Visible := False;
  sPageControl2.Visible := False;
  Constraints.MinWidth := ScaleInt(360, sSkinProvider1.SkinData);
  Width := Constraints.MinWidth;
  Constraints.MinHeight := Width * 3 div 2;
  Height := Constraints.MinHeight;
  sPanel2.Align := alClient;
end;


procedure TFormCharListEditor.InitFromImgList(ImgList: TsCharImageList);
begin
  BidiMode := Application.BiDiMode;
  if BiDiMode = bdRightToLeft then
    ReflectControls(Self, True);

  SourceList := ImgList;
  CharImageList.Assign(ImgList);

  RefillListView;
  sListView1.SkinData.VertScrollData.ButtonsSize := 0;
end;


procedure TFormCharListEditor.IterateItems(CallBack: TacItemsIterProc; Data: real);
var
  lItem: TListItem;
  Indexes: array of integer;
  c: integer;
begin
  if not ControlsUpdating then begin
    lItem := sListView1.GetNextItem(nil, sdAll, [isSelected]);
    if lItem <> nil then begin
      Perform(WM_SETREDRAW, 0, 0);
      ValidateRect(sListView1.Handle, nil);
      c := 0;
      while lItem <> nil do begin
        if CallBack(CharImageList.Items[lItem.Index], Data) then begin
          CharImageList.UpdateStd(lItem.Index);
          inc(c);
          SetLength(Indexes, c);
          Indexes[c - 1] := lItem.Index;
        end;
        lItem := sListView1.GetNextItem(lItem, sdAll, [isSelected]);
      end;
      Perform(WM_SETREDRAW, 1, 0);
      for c := 0 to Length(Indexes) - 1 do
        sListView1.RedrawItem(Indexes[c]);
    end;
    sPanel12.SkinData.Invalidate;
  end;
end;


procedure TFormCharListEditor.Loaded;
begin
  if DefaultManager <> nil then
    Scaled := DefaultManager.Options.ScaleMode = smVCL;

  inherited;
end;


procedure TFormCharListEditor.MainCharChanged(Sender: TObject; CharCode: Word; Charset: TFontCharset; FontName: string);
begin
  Delay(100);
  CharImageList.Items[FormCharListEditor.sListView1.Selected.Index].Char := CharCode;
  sTabSheet5.Caption := 'Char - 0x' + IntToHex(CharCode, 4);
  CharImageList.UpdateStd(FormCharListEditor.sListView1.Selected.Index);
//  CharImageList.GenerateStdList;
  FormCharMap.Close;
end;


procedure TFormCharListEditor.MainFontChanged(Sender: TObject; iCharset, iFont: integer);
var
  cid: TacCharItemData;
begin
  FormCharListEditor.SetFont(iCharset, iFont, cid);
  if sListView1.Selected <> nil then begin
    CharImageList.Items[sListView1.Selected.Index].Charset := iCharset;

    if iCharSet = iEMBEDDED then begin
      cid.Charset := DEFAULT_CHARSET;
      if iFont <> -1 then
        CharImageList.Items[sListView1.Selected.Index].FontName := sListBox2.Items[iFont]
      else
        CharImageList.Items[sListView1.Selected.Index].FontName := '';
    end
    else begin
      CharImageList.Items[sListView1.Selected.Index].Charset := arCharSets[iCharset];
      if iFont <> -1 then
        CharImageList.Items[sListView1.Selected.Index].FontName := sListBox1.Items[iFont]
      else
        CharImageList.Items[sListView1.Selected.Index].FontName := '';
    end;
  end;
end;


procedure TFormCharListEditor.NewCharSelected(Sender: TObject; CharCode: Word; Charset: TFontCharset; FontName: string);
begin
  Delay(100);
  AddNewGlyph(CharCode, FormCharMap.sComboBox1.ItemIndex, FormCharMap.sComboBox2.ItemIndex);
  CharImageList.UpdateStd(FormCharListEditor.sListView1.Selected.Index);
//  CharImageList.GenerateStdList;
  FormCharMap.Close;
end;


procedure TFormCharListEditor.RefillListView;
var
  i: Integer;
begin
  sListView1.Items.BeginUpdate;
  sListView1.SkinData.BeginUpdate;
  sListView1.Items.Clear;
  for i := 0 to CharImageList.Items.Count - 1 do
    with sListView1.Items.Add do begin
      ImageIndex := i;
      Caption := IntToStr(i);
    end;

  sListView1.SkinData.EndUpdate;
  sListView1.Items.EndUpdate;
end;


procedure TFormCharListEditor.S1Click(Sender: TObject);
begin
{$IFDEF DELPHI6UP}
  sListView1.SelectAll;
{$ENDIF}  
end;


procedure TFormCharListEditor.sButton1Click(Sender: TObject);
begin
  SourceList.acBeginUpdate;
  SourceList.Assign(CharImageList);
  TAccessCharImageList(SourceList).SavedWidth := SrcImgListSize.cx;
  TAccessCharImageList(SourceList).SavedHeight := SrcImgListSize.cy;
  TAccessCharImageList(SourceList).Width := SrcImgListSize.cx;
  TAccessCharImageList(SourceList).Height := SrcImgListSize.cy;
  SourceList.acEndUpdate;
  SourceList.Change;
end;


var
  sLastFontName: string = '';

function EnumFontCallback(lpelfe: PLogFont; lpntme: PNewTextMetricEX; FontType: DWORD; lp: LPARAM): Integer; stdcall;
begin
  if (lpelfe^.lfFaceName[0] <> '@') and (lpelfe^.lfFaceName <> sLastFontName) then begin
    sLastFontName := lpelfe^.lfFaceName;
    TStrings(lp).Add(lpelfe^.lfFaceName);
  end;
  Result := 1;
end;


procedure TFormCharListEditor.sListBox2BeforeItemDraw(aDC: HDC; aIndex: Integer; var aRect: TRect);
{$IFNDEF NOFONTRES}
const
  s = WideString(WideChar($F08D));
var
  f: TFont;
  SavedDC: hdc;
{$ENDIF}
begin
{$IFNDEF NOFONTRES}
  if aIndex = 0 then begin
    SavedDC := SaveDC(aDC);
    f := TFont.Create;
    try
      GetCurrentObject(aDC, f.Handle);
      f.Name := s_FontAwesome;
      f.Size := sListBox2.Canvas.Font.Size + 3;
      inc(aRect.Left, sListBox1.SkinData.CommonSkinData.Spacing);
      SelectObject(aDC, f.Handle);
      DrawTextW(aDC, PWideChar(s), 1, aRect, DT_NOPREFIX or DT_VCENTER);
      inc(aRect.Left, abs(f.Height) + sListBox1.SkinData.CommonSkinData.Spacing);
    finally
      f.Free;
      RestoreDC(aDC, SavedDC);
    end;
  end;
{$ENDIF}
end;


procedure TFormCharListEditor.sListBox2Click(Sender: TObject);
begin
  if sListBox2.ItemIndex >= 0 then begin
    sListBox1.ItemIndex := -1;
{$IFDEF DELPHI6UP}
    if IsValidIndex(sListView1.ItemIndex, CharImageList.Items.Count) then begin
{$ELSE}
    if (sListView1.Selected <> nil) and IsValidIndex(sListView1.Selected.Index, CharImageList.Items.Count) then begin
{$ENDIF}
      CharImageList.Items[sListView1.Selected.Index].Charset := DEFAULT_CHARSET;
      CharImageList.Items[sListView1.Selected.Index].FontName := sListBox2.Items[sListBox2.ItemIndex];
      if not ControlsUpdating then
        CharImageList.GenerateStdList;
    end
    else
  end;
  UpdateBtnsListBox2;
  sSpeedButton1.Enabled := CanAddChar;
end;


function ChangeDrawContour(Item: TacCharListItem; Data: real): boolean;
begin
  Result := boolean(Round(Data)) <> Item.DrawContour;
  Item.DrawContour := boolean(Round(Data));
end;

procedure TFormCharListEditor.sCheckBox1Click(Sender: TObject);
begin
  IterateItems(ChangeDrawContour, integer(sCheckBox1.Checked));
end;


function ChangeColor(Item: TacCharListItem; Data: real): boolean;
begin
  Result := TColor(Round(Data)) <> Item.Color;
  Item.Color := TColor(Round(Data));
end;

procedure TFormCharListEditor.sColorBox1Change(Sender: TObject);
begin
  IterateItems(ChangeColor, integer(sColorBox1.Selected));
end;


function ChangeAddedColor(Item: TacCharListItem; Data: real): boolean;
begin
  Result := TColor(Round(Data)) <> Item.AddedColor;
  Item.AddedColor := TColor(Round(Data));
end;

procedure TFormCharListEditor.sColorBox2Change(Sender: TObject);
begin
  IterateItems(ChangeAddedColor, integer(sColorBox2.Selected));
end;


function ChangeSecondColor(Item: TacCharListItem; Data: real): boolean;
begin
  Result := TColor(Round(Data)) <> Item.SecondColor;
  Item.SecondColor := TColor(Round(Data));
end;

procedure TFormCharListEditor.sColorBox3Change(Sender: TObject);
begin
  IterateItems(ChangeSecondColor, integer(sColorBox3.Selected));
end;


procedure TFormCharListEditor.sComboBox1Change(Sender: TObject);
var
  sl: TStringList;
  lf: TLogFont;
  DC: hdc;
begin
  sListBox1.Items.BeginUpdate;
  sListBox1.Clear;
  ZeroMemory(@lf, SizeOf(TLogFont));
  lf.lfCharSet := arCharSets[sComboBox1.ItemIndex];
  DC := GetDC(0);
  try
    EnumFontFamiliesEx(DC, lf, @EnumFontCallback, {$IFDEF DELPHI7UP}NativeInt{$ELSE}LParam{$ENDIF}(sListBox1.Items), 0);
  finally
    ReleaseDC(0, DC);
  end;
  sl := TStringList.Create;
  sl.Assign(sListBox1.Items);
  sl.Sort;
  sListBox1.Items.Assign(sl);
  sl.Free;
  sListBox1.Items.EndUpdate;

{$IFDEF DELPHI6UP}
  if not ControlsUpdating and IsValidIndex(sListView1.ItemIndex, CharImageList.Items.Count) then begin
{$ELSE}
  if not ControlsUpdating and (sListView1.Selected <> nil) and IsValidIndex(sListView1.Selected.Index, CharImageList.Items.Count) then begin
{$ENDIF}
    if sListBox1.ItemIndex < 0 then
      sListBox1.ItemIndex := 0;

    CharImageList.AcBeginUpdate;
    if sListBox1.ItemIndex >= 0 then begin
      CharImageList.Items[sListView1.Selected.Index].Charset := arCharSets[sComboBox1.ItemIndex];
      CharImageList.Items[sListView1.Selected.Index].FontName := sListBox1.Items[sListBox1.ItemIndex]
    end
    else begin
      CharImageList.Items[sListView1.Selected.Index].CharSet := DEFAULT_CHARSET;
      CharImageList.Items[sListView1.Selected.Index].FontName := s_FontAwesome;
      FreeAndNil(CharImageList.Items[sListView1.Selected.Index].CacheBmp);
      CharImageList.Items[sListView1.Selected.Index].Char := $F128;
    end;
    CharImageList.AcEndUpdate;
  end;
  sSpeedButton1.Enabled := sListBox1.ItemIndex >= 0;
  sSpeedButton3.Enabled := sListBox1.ItemIndex >= 0;
  sSpeedButton1.Enabled := CanAddChar;
end;


procedure TFormCharListEditor.sListBox1Click(Sender: TObject);
begin
  if sListBox1.ItemIndex >= 0 then begin
    sListBox2.ItemIndex := -1;
{$IFDEF DELPHI6UP}
    if IsValidIndex(sListView1.ItemIndex, CharImageList.Items.Count) then begin
{$ELSE}
    if (sListView1.Selected <> nil) and IsValidIndex(sListView1.Selected.Index, CharImageList.Items.Count) then begin
{$ENDIF}
      CharImageList.Items[sListView1.Selected.Index].FontName := sListBox1.Items[sListBox1.ItemIndex];
    end;
  end;
  CharImageList.GenerateStdList;
  sSpeedButton1.Enabled := CanAddChar;
end;


procedure TFormCharListEditor.sListView1Click(Sender: TObject);
begin
  InitControls;
  sSpeedButton2.Enabled := sListView1.Selected <> nil;
  sSpeedButton3.Enabled := sListView1.Selected <> nil;
  sSpeedButton4.Enabled := sListView1.Items.Count > 0;
  IterateControls(sTabSheet3, Integer(sListView1.Selected <> nil), EnableCtrls);
  IterateControls(sPanel2, Integer(sListView1.Selected <> nil), EnableCtrls);
  UpdateAddedControls;

  sPanel2.Enabled := True;
  sPageControl1.Enabled := True;
  sTabSheet2.Enabled := True;
  IterateControls(sTabSheet2, 1, EnableCtrls);
  if sSplitView1.Opened then
    RedrawWindow(sSplitView1.Handle, nil, 0, RDWA_ALLNOW);
end;


procedure TFormCharListEditor.sListView1DragDrop(Sender, Source: TObject; X, Y: Integer);
var
  li: TListItem;
begin
  li := sListView1.GetItemAt(X, Y);
  CharImageList.Items[sListView1.ItemFocused.Index].Index := li.Index;
  CharImageList.Move(sListView1.ItemFocused.Index, li.Index);
  EndScroll;
end;


procedure TFormCharListEditor.sListView1DragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  li: TListItem;
begin
  li := sListView1.GetItemAt(X, Y);
  Accept := (li <> nil) and (li <> sListView1.ItemFocused);
  if (X < scBorder) or (X > sListView1.Width - scBorder) then
    StartScroll;
end;


procedure TFormCharListEditor.sListView1Resize(Sender: TObject);
begin
  sListView1.Arrange(arAlignTop);
end;


procedure TFormCharListEditor.sPageControl1Change(Sender: TObject);
begin
  if (sPageControl1.ActivePageIndex = 0) and (sComboBox1.ItemIndex = iEMBEDDED) then
    sComboBox1Change(sComboBox1);
end;


procedure EnableDisableAdded(Ctrl: TControl; Data: integer);
begin
  if Ctrl.Tag = 2 then
    Ctrl.Enabled := boolean(Data);
end;

procedure TFormCharListEditor.sPageControl2PageChanging(Sender: TObject; NewPage: TsTabSheet; var AllowChange: Boolean);
begin
  if (NewPage = sTabSheet5) and (CharGrid = nil) then begin
    CharGrid := TacCharGrid.Create(FormCharMap);
    TacCharGrid(CharGrid).InitCtrl(False);
    CharGrid.Align := alClient;
    CharGrid.Parent := sTabSheet5;
    TacCharGrid(CharGrid).FillList;
    if sSkinProvider1.SkinData.Skinned then
      sSkinProvider1.Adapter.AddNewItem(CharGrid, nil);
  end
  else
    if NewPage = sTabSheet4 then
      UpdateAddedControls;
end;


procedure TFormCharListEditor.sPanel12Paint(Sender: TObject; Canvas: TCanvas);
var
  R: TRect;
  Bmp, BGBmp: TBitmap;
  BGColor, CharColor: TColor;
begin
  R.Left := (sPanel12.Width - SrcImgListSize.cx) div 2;
  R.Top := (sPanel12.Height - SrcImgListSize.cy) div 2;
  R.Right := R.Left + SrcImgListSize.cx;
  R.Bottom := R.Top + SrcImgListSize.cy;
  BGColor := acColorToRGB(clWindow);
  CharColor := acColorToRGB(clWindowText);

  BGBmp := CreateBmp32(sPanel12);
  FillDC(BGBmp.Canvas.Handle, MkRect(BGBmp), BGColor);

{$IFDEF DELPHI6UP}
  if IsValidIndex(sListView1.ItemIndex, sListView1.Items.Count) then begin
{$ELSE}
  if (sListView1.Selected <> nil) then begin
{$ENDIF}

    BGBmp.Canvas.Pen.Color := BlendColors(BGColor, CharColor, 200);
    // Horz lines
    SetDCPenColor(BGBmp.Canvas.Handle, BlendColors(BGColor, CharColor, 50));
    acPaintLine(BGBmp.Canvas.Handle, 0, R.Top - 1, BgBmp.Width, R.Top - 1);
    acPaintLine(BGBmp.Canvas.Handle, 0, R.Bottom + 1, BgBmp.Width, R.Bottom + 1);
    // Vert lines
    acPaintLine(BGBmp.Canvas.Handle, R.Left - 1, 0, R.Left - 1, BgBmp.Height);
    acPaintLine(BGBmp.Canvas.Handle, R.Right + 1, 0, R.Right + 1, BgBmp.Height);

    Bmp := CreateBmp32(SrcImgListSize.cx, SrcImgListSize.cy);

{$IFDEF DELPHI6UP}
    CharImageList.SetCharColor(CharColor, sListView1.ItemIndex, False);
    CharImageList.GetBitmap32(sListView1.ItemIndex, Bmp, SrcImgListSize.cy);
{$ELSE}
    CharImageList.SetCharColor(CharColor, sListView1.Selected.Index, False);
    CharImageList.GetBitmap32(sListView1.Selected.Index, Bmp, SrcImgListSize.cy);
{$ENDIF}

    PaintBmpRect32(BGBmp, Bmp, MkRect(Bmp), Point(R.Left, R.Top));
    Bmp.Free;
  end;
  BitBlt(Canvas.Handle, 0, 0, BGBmp.Width, BGBmp.Height, BGBmp.Canvas.Handle, 0, 0, SRCCOPY);
  BGBmp.Free;
end;


procedure TFormCharListEditor.SetFont(iCharSet, iFont: integer; var cid: TacCharItemData);
begin
  if iCharSet = iEMBEDDED then begin
    cid.Charset := DEFAULT_CHARSET;
    if iFont <> -1 then begin
      sListBox1.ItemIndex := -1;
      sListBox2.ItemIndex := iFont;
      cid.FontName := sListBox2.Items[sListBox2.ItemIndex];
      sPageControl1.ActivePageIndex := 1;
    end
    else
      cid.FontName := '';
  end
  else begin
    if (iCharSet <> -1) and (sComboBox1.ItemIndex <> iCharSet) then begin
      sComboBox1.ItemIndex := iCharSet;
      sComboBox1Change(sComboBox1);
    end;
    cid.Charset := arCharSets[sComboBox1.ItemIndex];
    if iFont <> -1 then begin
      sListBox2.ItemIndex := -1;
      sListBox1.ItemIndex := iFont;
      sPageControl1.ActivePageIndex := 0;
      cid.FontName := sListBox1.Items[iFont];
    end
    else
      cid.FontName := '';
  end;
end;


procedure TFormCharListEditor.sGroupBox1CheckBoxChanged(Sender: TObject);
begin
  if not sGroupBox1.Checked then begin
    IterateItems(ChangeSecondColor, integer(clNone));
    sColorBox3.Selected := clNone;
  end;
end;


procedure TFormCharListEditor.ShowCharMap(Sender: TsSpeedButton);
begin
  if FormCharMap = nil then begin
    GetIntController.MousePressed := True;
    Sender.Down := True;
    FormCharMap := TFormCharMap.Create(Self);
    FormCharMap.PressedButton := Sender;

    if Sender = sSpeedButton1 then // New
      TacCharGrid(FormCharMap.Grid).OnCharSelected := NewCharSelected
    else
      if Sender = sSpeedButton3 then // Change
        TacCharGrid(FormCharMap.Grid).OnCharSelected := MainCharChanged
      else // Added char
        TacCharGrid(FormCharMap.Grid).OnCharSelected := AddedCharChanged;

    if Sender <> sSpeedButton8 then
      FormCharMap.OnFontChanged := MainFontChanged;

    if sListBox1.ItemIndex >= 0 then begin // Sys font used
      FormCharMap.sComboBox1.ItemIndex := sComboBox1.ItemIndex;
      FormCharMap.sComboBox1Change(FormCharMap.sComboBox1);
      FormCharMap.sComboBox2.ItemIndex := FormCharMap.sComboBox2.Items.IndexOf(sListBox1.Items[sListBox1.ItemIndex]);
    end
    else begin // Embedded font used
      FormCharMap.sComboBox1.ItemIndex := iEMBEDDED;
      FormCharMap.sComboBox1Change(FormCharMap.sComboBox1);
      if sListBox2.ItemIndex >= 0 then  // Sys font used
        FormCharMap.sComboBox2.ItemIndex := FormCharMap.sComboBox2.Items.IndexOf(sListBox2.Items[sListBox2.ItemIndex]);
    end;
    FormCharMap.Loading := False;
    FormCharMap.sComboBox2Change(FormCharMap.sComboBox2);
    GetIntController.IgnoreCapture := True;
    FormCharMap.BiDiMode := BiDiMode;
    if BiDiMode = bdRightToLeft then
      ReflectControls(FormCharMap, True);

    ShowPopupForm(FormCharMap, FormCharMap.PressedButton.ClientToScreen(Point(0, FormCharMap.PressedButton.Height)), True);//sSkinProvider1.SkinData.Skinned);
    GetIntController.IgnoreCapture := False;
    FormCharMap.PressedButton.Down := False;
  end
  else begin
    FormCharMap.Close;
    FreeAndNil(FormCharMap);
    Sender.Down := True; // Will be changed to False in std MouseDown handler
  end;
end;


procedure TFormCharListEditor.sSpeedButton10Click(Sender: TObject);
begin
  sSplitView1.Opened := not sSplitView1.Opened;
end;


procedure TFormCharListEditor.sSpeedButton11Click(Sender: TObject);
var
  s: string;
  i, NdxOffset: integer;
begin
  NdxOffset := {$IFNDEF NOFONTRES}1;{$ELSE}0;{$ENDIF}
  for i := NdxOffset to CharImageList.EmbeddedFonts.Count - 1 do
    if FileExists(CharImageList.EmbeddedFonts[i].FileName) then begin
      CharImageList.EmbeddedFonts[i].FontData.LoadFromFile(CharImageList.EmbeddedFonts[i].FileName);
      CharImageList.EmbeddedFonts[i].UnLoadFont;
      CharImageList.EmbeddedFonts[i].LoadFont;
    end
    else
      if s = '' then
        s := CharImageList.EmbeddedFonts[i].FileName
      else
        s := ', ' + CharImageList.EmbeddedFonts[i].FileName;

  if s <> '' then
    ShowWarning('These font files are not font on the disk: ' + s)
  else
    if NdxOffset < CharImageList.EmbeddedFonts.Count then
      ShowMessage('All embedded fonts are updated');
end;


procedure TFormCharListEditor.sSpeedButton1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowCharMap(sSpeedButton1);
end;


procedure TFormCharListEditor.sSpeedButton1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  GetIntController.MousePressed := False;
end;


procedure TFormCharListEditor.sSpeedButton2Click(Sender: TObject);
{$IFDEF DELPHI6UP}
var
  SavedIndex: integer;
{$ENDIF}
begin
{$IFDEF DELPHI6UP}
  if IsValidIndex(sListView1.ItemIndex, CharImageList.Items.Count) then begin
    SavedIndex := sListView1.Selected.Index;
{$ELSE}
  if (sListView1.Selected <> nil) and IsValidIndex(sListView1.Selected.Index, CharImageList.Items.Count) then begin
{$ENDIF}
    CharImageList.Items.Delete(sListView1.Selected.Index);
    sListView1.Items.Delete(sListView1.Selected.Index);
    CharImageList.GenerateStdList;
    RefillListView;
{$IFDEF DELPHI6UP}
    sListView1.ItemIndex := min(SavedIndex, sListView1.Items.Count - 1);
{$ENDIF}
    sListView1Click(sListView1);
  end;
end;


procedure TFormCharListEditor.sSpeedButton3MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
{$IFDEF DELPHI6UP}
  if IsValidIndex(sListView1.ItemIndex, CharImageList.Items.Count) then
{$ELSE}
  if (sListView1.Selected <> nil) and IsValidIndex(sListView1.Selected.Index, CharImageList.Items.Count) then
{$ENDIF}
    ShowCharMap(sSpeedButton3);
end;


procedure TFormCharListEditor.sSpeedButton4Click(Sender: TObject);
begin
  if CustomRequest('Really delete all items?') then begin
    CharImageList.Clear;
    RefillListView;
    if not sListView1.SkinData.Skinned then
      sListView1.Repaint;

    sListView1Click(sListView1);
  end;
end;


// Add new font
procedure TFormCharListEditor.sSpeedButton5Click(Sender: TObject);
var
  i: integer;
begin
  if sOpenDialog1.InitialDir = '' then
    sOpenDialog1.InitialDir := GetAppPath;

  if sOpenDialog1.Execute then begin
    sListBox2.Items.BeginUpdate;
    for i := 0 to sOpenDialog1.Files.Count - 1 do
      AddNewFont(sOpenDialog1.Files[i]);

    sListBox2.Items.EndUpdate;
  end;
  UpdateBtnsListBox2;
end;


// Clear all
procedure TFormCharListEditor.sSpeedButton6Click(Sender: TObject);
var
  i: integer;
begin
  sListBox2.Items.BeginUpdate;
  for i := sListBox2.Count - 1 downto 1 do
    DeleteFont(i);

  sListBox2.Items.EndUpdate;
  sListBox2.ItemIndex := 0;
  UpdateBtnsListBox2;
end;


procedure TFormCharListEditor.DeleteFont(Ndx: integer);
var
  i: integer;
  b: boolean;
begin
  b := False;
  for i := 0 to CharImageList.Count - 1 do
    if CharImageList.Items[i].FontName = sListBox2.Items[Ndx] then begin
      if not b then begin
        b := MessageDlg('Some chars requires this font: ' + sListBox2.Items[Ndx] + #13#10'Delete anyway?', mtConfirmation, [mbYes, mbNo], 0) = mrYes;
        if not b then
          Exit;
      end;
      CharImageList.Items[i].FontName := s_FontAwesome;
      CharImageList.Items[i].CharSet := DEFAULT_CHARSET;
      FreeAndNil(CharImageList.Items[i].CacheBmp);
      CharImageList.Items[i].Char := $F128;
    end;

  CharImageList.EmbeddedFonts.Delete(Ndx);
  sListBox2.Items.Delete(Ndx);
  if b then
    CharImageList.GenerateStdList;
end;


procedure TFormCharListEditor.sSpeedButton7Click(Sender: TObject);
var
  OldNdx: integer;
begin
  if sListBox2.ItemIndex > 0 then begin
    OldNdx := sListBox2.ItemIndex;
    DeleteFont(sListBox2.ItemIndex);
    sListBox2.ItemIndex := min(OldNdx, sListBox2.Items.Count - 1);
    UpdateBtnsListBox2;
  end;
end;


procedure TFormCharListEditor.sSpeedButton8MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  ShowCharMap(sSpeedButton8);
end;


procedure TFormCharListEditor.sSpeedButton9Click(Sender: TObject);
begin
  CharImageList.Items[FormCharListEditor.sListView1.Selected.Index].AddedChar := 0;
  CharImageList.UpdateStd(FormCharListEditor.sListView1.Selected.Index);
  UpdateAddedControls;
end;


function ChangeScalingFactor(Item: TacCharListItem; Data: real): boolean;
begin
  Result := Data <> Item.ScalingFactor;
  Item.ScalingFactor := Data;
end;

procedure TFormCharListEditor.sTrackBar1Change(Sender: TObject);
var
  r: Real;
begin
  r := 1 + (sTrackBar1.Position) * 0.01;
  sLabel3.Caption := 'X ' + FloatToStr(r);
  sLabel3.Repaint;
  IterateItems(ChangeScalingFactor, r);
end;


function ChangeOffsetY(Item: TacCharListItem; Data: real): boolean;
begin
  Result := Round(Data) <> Item.OffsetY;
  Item.OffsetY := Round(Data);
end;

procedure TFormCharListEditor.sTrackBar2Change(Sender: TObject);
begin
  sLabel4.Caption := FloatToStr(sTrackBar2.Position);
  sLabel4.Repaint;
  IterateItems(ChangeOffsetY, sTrackBar2.Position);
end;


function ChangeAngle(Item: TacCharListItem; Data: real): boolean;
begin
  Result := Round(Data) <> Item.Angle;
  Item.Angle := Round(Data);
end;

procedure TFormCharListEditor.sTrackBar3UserChange(Sender: TObject);
begin
  sLabel6.Caption := IntToStr(sTrackBar3.Position);
  sLabel6.Repaint;
  IterateItems(ChangeAngle, sTrackBar3.Position);
end;


function ChangeAddedSize(Item: TacCharListItem; Data: real): boolean;
begin
  Result := Round(Data) <> Item.AddedSize;
  Item.AddedSize := Round(Data);
end;

procedure TFormCharListEditor.sTrackBar4Change(Sender: TObject);
begin
  sLabel7.Caption := IntToStr(sTrackBar4.Position) + '%';
  sLabel7.Repaint;
  IterateItems(ChangeAddedSize, sTrackBar4.Position);
end;


procedure TFormCharListEditor.UpdateAddedControls;
var
  b: boolean;
begin
  b := (sListView1.Selected <> nil) and (CharImageList.Items[sListView1.Selected.Index].AddedChar <> 0);
  IterateControls(sTabSheet4, integer(b), EnableDisableAdded);
  sSpeedButton9.Enabled := b;
  if b then begin
    CharImageSmall.Items[0].Char := CharImageList.Items[sListView1.Selected.Index].AddedChar;
    CharImageSmall.Items[0].Charset := CharImageList.Items[sListView1.Selected.Index].AddedCharset;
    CharImageSmall.Items[0].FontName := CharImageList.Items[sListView1.Selected.Index].AddedFontName;
    sSpeedButton8.Images := CharImageSmall;
  end
  else
    sSpeedButton8.Images := nil;

  sPanel12.SkinData.Invalidate;
end;


procedure TFormCharListEditor.UpdateBtnsListBox2;
var
  NdxOffset: integer;
begin
  NdxOffset := {$IFNDEF NOFONTRES}1;{$ELSE}0;{$ENDIF}
  // "Delete" button
  if sListBox2.ItemIndex >= NdxOffset then
    sSpeedButton7.Enabled := sListBox2.ItemIndex < CharImageList.EmbeddedFonts.Count + NdxOffset - 1
  else
    sSpeedButton7.Enabled := False;

  sSpeedButton6.Enabled := sListBox2.Items.Count > NdxOffset;
  sSpeedButton11.Enabled := sSpeedButton6.Enabled;
end;


{$IFDEF NOFONTRES}
var
  ResStream: TResourceStream;
  FontsCount: integer;
  awFont: tHandle = 0;
{$ENDIF}

initialization

{$IFDEF NOFONTRES}
{$R acFontData.res}
  if awFont = 0 then begin
    ResStream := TResourceStream.Create(hInstance, sawFont, RT_RCDATA);
    awFont := AddFontMemResourceEx(ResStream.Memory, ResStream.Size, nil, @FontsCount);
    ResStream.Free();
  end;
{$ENDIF}

finalization
{$IFDEF NOFONTRES}
  if awFont <> 0 then
    RemoveFontMemResourceEx(awFont);
{$ENDIF}

end.
