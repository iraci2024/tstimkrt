unit acCharMap;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, ComCtrls, Grids, ExtCtrls, StdCtrls,
  {$IFDEF DELPHI7UP} Types, {$ENDIF}
  sListView, sSpeedButton, sPanel, sStatusBar, sSkinProvider, sComboBox, sLabel, sConst, sCommonData,
  Mask, sMaskEdit, sCustomComboEdit;


const
  iColCount = 11;
  iCharSize = 28;
  iCellSize = 48;

type
  TacCharRow = array [0 .. iColCount - 1] of Word;

  TOverItem = procedure(Sender: TObject; CharCode: integer) of object;
  TCharSelect = procedure(Sender: TObject; CharCode: Word; Charset: TFontCharset; FontName: string) of object;

  TacCharGrid = class(TDrawGrid)
  private
    FOnOverItem: TOverItem;
    FOnCharSelected: TCharSelect;
  protected
    procedure ChangeScale(M, D: Integer{$IFDEF DELPHI_10TOKYO}; isDpiChange: Boolean{$ENDIF}); override;

    procedure DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DoDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  public
    HotCell: TPoint;
    Clicked: boolean;
    SkinData: TsCommonData;
    CharArray: array of TacCharRow;
    procedure FillList;
    procedure CreateWnd; override;
    procedure InitCtrl(IsPopup: boolean);
    procedure WndProc(var Message: TMessage); override;
    property OnOverItem: TOverItem read FOnOverItem write FOnOverItem;
    property OnCharSelected: TCharSelect read FOnCharSelected write FOnCharSelected;
  end;


  TFontChanged = procedure(Sender: TObject; iCharset, iFont: integer) of object;

  TFormCharMap = class(TForm)
    sPanel1: TsPanel;
    sPanel2: TsPanel;
    sLabel1: TsLabel;
    sLabel2: TsLabel;
    sComboBox1: TsComboBox;
    sComboBox2: TsComboBox;
    sSkinProvider1: TsSkinProvider;
    procedure sComboBox1Change(Sender: TObject);
    procedure sComboBox2Change(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure sSkinProvider1SkinItemEx(Item: TComponent; var CanBeAdded: Boolean; SkinParams: PacSkinParams);
  private
    FOnFontChanged: TFontChanged;
    procedure OverItem(Sender: TObject; CharCode: integer);
  public
    Loading: boolean;
    Grid: TacCharGrid;
    PressedButton: TsSpeedButton;

    procedure Loaded; override;
    procedure AfterConstruction; override;
    procedure WndProc(var Message: TMessage); override;
    property OnFontChanged: TFontChanged read FOnFontChanged write FOnFontChanged;
  end;

var
  FormCharMap: TFormCharMap;

implementation

uses
  {$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  sMessages, acCharListEditor, sSkinManager, sGraphUtils, acntUtils, sVCLUtils, sSkinProps, sStyleSimply, acAlphaImageList,
  acgpUtils, acFontStore;

{$R *.dfm}


procedure TFormCharMap.AfterConstruction;
begin
  inherited;
  Loading := True;
  Grid := TacCharGrid.Create(Self);
  Grid.OnOverItem := OverItem;
  Grid.InitCtrl(True);
  Grid.Parent := sPanel1;
  if sSkinProvider1.SkinData.Skinned then
    sSkinProvider1.Adapter.AddNewItem(Grid, nil);
end;


procedure TFormCharMap.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FormCharMap := nil;
  PressedButton.SkinData.FMouseAbove := False;
  PressedButton.SkinData.BGChanged := True;
  PressedButton.Down := False;
  PressedButton.Repaint;
  Action := caFree;
end;


procedure TFormCharMap.Loaded;
begin
  if DefaultManager <> nil then
    Scaled := DefaultManager.Options.ScaleMode = smVCL;

  inherited;
end;


procedure TFormCharMap.OverItem(Sender: TObject; CharCode: integer);
begin
  if CharCode >= 0 then
    FormCharMap.sLabel2.Caption := '0x' + IntToHex(CharCode, 4)
  else
    FormCharMap.sLabel2.Caption := '';
end;


procedure TFormCharMap.sComboBox1Change(Sender: TObject);
var
  bInstalled: boolean;
  sl: TStringList;
  i, j: integer;
  lf: TLogFont;
  DC: hdc;
begin
  sComboBox2.Items.BeginUpdate;
  sComboBox2.Clear;
  // Fill sComboBox2
  bInstalled := sComboBox1.ItemIndex < sComboBox1.Items.Count - 1; // Not last item
  if bInstalled then begin
    ZeroMemory(@lf, SizeOf(TLogFont));
    lf.lfCharSet := arCharSets[sComboBox1.ItemIndex];
    DC := GetDC(0);
    try
      EnumFontFamiliesEx(DC, lf, @EnumFontCallback, {$IFDEF DELPHI7UP}NativeInt{$ELSE}LParam{$ENDIF}(sComboBox2.Items), 0);
    finally
      ReleaseDC(0, DC);
    end;
  end
  else begin
    for i := 0 to FormCharListEditor.CharImageList.EmbeddedFonts.Count - 1 do
      sComboBox2.Items.Add(FormCharListEditor.CharImageList.EmbeddedFonts[i].FontName);

    if acFontStoreList <> nil then
      for i := 0 to acFontStoreList.Count - 1 do
        for j := 0 to TsFontStore(acFontStoreList[i]).Fonts.Count - 1 do
          sComboBox2.Items.Add(TsFontStore(acFontStoreList[i]).Fonts[j].FontName);
  end;

  sComboBox2.Items.EndUpdate;
  sl := TStringList.Create;
  sl.Assign(sComboBox2.Items);
  if bInstalled then
    sl.Sort;

  sComboBox2.Items.Assign(sl);
  sl.Free;

  if not Loading then begin
    sComboBox2.ItemIndex := 0;
    sComboBox2Change(sComboBox2);
  end
  else
    if bInstalled then begin
      sComboBox2.ItemIndex := sComboBox2.Items.IndexOf(FormCharListEditor.sListBox1.Items[FormCharListEditor.sListBox1.ItemIndex]);
      if not Loading then
        OnFontChanged(Self, sComboBox1.ItemIndex, sComboBox2.ItemIndex);
    end;
end;


procedure TFormCharMap.sComboBox2Change(Sender: TObject);
begin
  if not Loading then begin
    if IsWindowVisible(sComboBox2.Handle) and Assigned(OnFontChanged) then
      OnFontChanged(Self, sComboBox1.ItemIndex, sComboBox2.ItemIndex);

    if sComboBox1.ItemIndex = iEMBEDDED then
      Grid.Font.Charset := DEFAULT_CHARSET
    else
      Grid.Font.Charset := arCharSets[sComboBox1.ItemIndex];

    Grid.Font.Name := sComboBox2.Text;
    TacCharGrid(Grid).FillList;
  end;
end;


procedure TFormCharMap.sSkinProvider1SkinItemEx(Item: TComponent; var CanBeAdded: Boolean; SkinParams: PacSkinParams);
begin
  if Item = Grid then begin
    SkinParams.VertScrollBtnSize := 0;
    SkinParams.VertScrollSize := sSkinProvider1.SkinData.CommonSkinData.Spacing * 3;
    SkinParams.SkinSection := s_Transparent;
  end;
end;


procedure TFormCharMap.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  inherited;
end;


type
  TAccessControl = class(TWinControl);


procedure TacCharGrid.ChangeScale(M, D: Integer{$IFDEF DELPHI_10TOKYO}; isDpiChange: Boolean{$ENDIF});
begin
  DefaultColWidth  := MulDiv(DefaultColWidth, M, D);
  DefaultRowHeight := MulDiv(DefaultRowHeight, M, D);
  Width := MulDiv(Width, M, D);
  Height := MulDiv(Height, M, D);
end;


procedure TacCharGrid.CreateWnd;
begin
  inherited;
  OnDrawCell := DoDrawCell;
  OnMouseMove := DoMouseMove;
  OnMouseDown := DoMouseDown;
end;


procedure TacCharGrid.DoDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  R: TRect;
  c: WideChar;
  p: PWideChar;
  Bmp: TBitmap;
  sd: TacSkinData;
  Ndx, aState: integer;
begin
  if (ACol >= 0) and (ARow >= 0) then begin
    if (HotCell.X = ACol) and (HotCell.Y = ARow) then
      aState := iff(Clicked, 2, 1)
    else
      aState := 0;

    c := WideChar(CharArray[ARow, ACol]);
    if DefaultManager <> nil then
      Font.Size := DefaultManager.ScaleInt(iCharSize)
    else
      Font.Size := iCharSize;

    if aState = 0 then begin
      FillDC(Canvas.Handle, Rect, Color);
      if (DefaultManager <> nil) and DefaultManager.IsActive then
        Canvas.Font.Color := DefaultManager.Palette[pcEditText]
      else
        Canvas.Font.Color := clWindowText;
    end
    else begin
      if (DefaultManager <> nil) and DefaultManager.IsActive then
        Ndx := DefaultManager.SkinCommonInfo.Sections[ssSelection]
      else
        Ndx := -1;

      if (Ndx >= 0) and (SkinData <> nil) then begin
        sd := SkinData.CommonSkinData;
        Bmp := CreateBmp32(WidthOf(Rect), HeightOf(Rect));
        PaintItemBGFast(Ndx, sd.gd[Ndx].Props[0].TextureIndex, sd.gd[Ndx].Props[1].TextureIndex, EmptyCI, aState, MkRect(Bmp), MkPoint, Bmp, sd);
        BitBlt(Canvas.Handle, Rect.Left, Rect.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
        Bmp.Free;
        Canvas.Font.Color := sd.gd[Ndx].Props[1].FontColor.Color;
      end
      else begin
        FillDC(Canvas.Handle, Rect, clHighlight);
        Canvas.Font.Color := clHighlightText;
      end;
    end;
    p := @c;
    SelectObject(Canvas.Handle, Canvas.Font.Handle);
{
    with acgpMeasureString(Canvas.Handle, c, False) do begin
      Rect.Top := Rect.Top + (HeightOf(Rect) - cy) div 2;
      Rect.Bottom := Rect.Top + abs(Canvas.Font.Height);
      acgpDrawString(Canvas.Handle, c + '', Rect, Cardinal(Canvas.Font.Color), taCenter, taVerticalCenter, 0, False, Canvas.Font.Name);
    end;
}
    Canvas.Brush.Style := bsClear;
    DrawTextW(Canvas.Handle, p, 1, R, DT_NOPREFIX or DT_CALCRECT or DT_EXTERNALLEADING);

    Rect.Top := Rect.Top + (HeightOf(Rect) - HeightOf(R)) div 2;
    Rect.Bottom := Rect.Top + abs(Canvas.Font.Height);
    DrawTextW(Canvas.Handle, p, 1, Rect, DT_NOPREFIX or DT_CENTER or DT_NOCLIP or DT_VCENTER);
  end;
end;


procedure TacCharGrid.DoMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
  r, c: integer;
begin
  p := ScreenToClient(acMousePos);
  MouseToCell(p.X, p.Y, c, r);
  Repaint;
  if IsValidIndex(c, iColCount) and IsValidIndex(r, RowCount) and (CharArray[r][c] <> 0) then
    if Assigned(OnCharSelected) then
      OnCharSelected(Self, CharArray[r][c], Font.Charset, Font.Name);
end;


procedure TacCharGrid.DoMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  cR: TRect;
  OldCell: TPoint;
  r, c: integer;
begin
  OldCell := HotCell;
  MouseToCell(x, y, c, r);
  if (HotCell.X <> c) or (HotCell.Y <> r) then begin
    HotCell := Point(c, r);
    if OldCell.Y >= 0 then begin
      cR := CellRect(OldCell.X, OldCell.Y);
      if DefaultRowHeight = HeightOf(cR) then
        OnDrawCell(Self, OldCell.X, OldCell.Y, cR, []);
    end;
    if (r >= 0) and (c >= 0) then begin
      cR := CellRect(c, r);
      OnOverItem(Self, CharArray[r, c]);
      if DefaultRowHeight = HeightOf(cR) then
        OnDrawCell(Self, HotCell.X, HotCell.Y, cR, []);
    end
    else begin
      OnOverItem(Self, -1);
      HotCell := Point(-1, -1);
    end;
  end;
end;

{$DEFINE DONT_SKIP_GLYPHS}

procedure TacCharGrid.FillList;
const
  FirstChar = $F000;
  MaxChar = $FFFE;
var
  k, x, y, i: integer;
  gs: PGlyphSet;
  R: PWCRange;
  wc: WChar;
  tm: TTextMetric;
{
  s_In: acString;
  c_Out: array[0..MaxChar - FirstChar - 1] of word;
  c_Count: integer;
}
begin
  Canvas.Font.Assign(Font);
(*
  // Prepare string
  for i := FirstChar to MaxChar do
    s_In := s_In + WideChar(i);

  GetGlyphIndicesW(Canvas.Handle, PacChar(s_In), MaxChar - FirstChar, @c_Out[0], GGI_MARK_NONEXISTING_GLYPHS);

  c_Count := 0;
  for i := 0 to Length(c_Out) - 1 do // Calc supported chars
    if {$IFNDEF DONT_SKIP_GLYPHS}(c_Out[i] <> $FFFF) and {$ENDIF}(c_Out[i] <> 0) then
      inc(c_Count);

  ColCount := iColCount;
  RowCount := c_Count div iColCount + 1;
  SetLength(CharArray, RowCount);
  y := 0;
  x := 0;
  for i := 0 to Length(c_Out) - 1 do
    if {$IFNDEF DONT_SKIP_GLYPHS}(c_Out[i] <> $FFFF) and {$ENDIF}(c_Out[i] <> 0) then
    begin
      if x = iColCount then begin // New row
        x := 0;
        inc(y);
      end;
      // Save char
      CharArray[y][x] := ord(s_in[i]);
      inc(x);
    end;
*)
  if GetTextMetrics(Canvas.Handle, tm) then begin
    // Size of ranges structure
    k := GetFontUnicodeRanges(Canvas.Handle, nil);
    GetMem(Pointer(gs), k);
    try
      // Init
      gs.cbThis := k;
      gs.flAccel := 0;
      gs.cGlyphsSupported := 0;
      gs.cRanges := 0;
      // Get ranges
      if GetFontUnicodeRanges(Canvas.Handle, gs) > 0 then begin
        k := 0;
        R := @GS.ranges[0];
        // Iterate ranges, calc count of glyphs
        for i := 0 to gs^.cRanges - 1 do begin
          inc(k, R^.cGlyphs);
          inc(R);
        end;
        // Calc row count
        i := k div iColCount + 1;
        SetLength(CharArray, i);
        RowCount := i;
        // Iterate ranges
        y := 0;
        x := 0;
        R := @GS.ranges[0];
        for i := 0 to gs.cRanges - 1 do begin
          // Receive lowest char from range
          wc := R^.wcLow;
          // Iterate chars from current range
          for k := 0 to R^.cGlyphs - 1 do begin
            if ord(wc) > 32 then begin // Do not include chars lower than tmFirstChar
              if x = iColCount then begin // New row
                x := 0;
                inc(y);
              end;
              // Save char
              CharArray[y][x] := ord(wc);
              inc(x);
            end;
            inc(wc);
          end;
          inc(R);
        end;
      end;
    finally
      FreeMem(Pointer(gs));
    end;
  end;
  if IsWindowVisible(Handle) then
    Repaint;
end;


procedure TacCharGrid.InitCtrl(IsPopup: boolean);
var
  i: integer;
begin
  SkinData := TsCommonData(Perform(SM_ALPHACMD, AC_GETSKINDATA_HI, 0));
  Clicked := False;
  ColCount := iColCount;
  FixedCols := 0;
  FixedRows := 0;
  BorderStyle := bsNone;
  DefaultColWidth := iCellSize;
  DefaultRowHeight := iCellSize;
  for i := 0 to iColCount - 1 do
      ColWidths[i] := iCellSize;

  ScrollBars := ssVertical;
  Options := Options - [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect] + [goThumbTracking];
  Align := alClient;
  HotCell := Point(-1, -1);
{$IFDEF DELPHI6UP}
  InvalidateGrid;
{$ELSE}
  Repaint;
{$ENDIF}
end;


{$IFNDEF DELPHI6UP}
{$HINTS OFF}
type
  TAccessStdGrid = class(TCustomControl)
  private
    FAnchor:        TGridCoord;
    FBorderStyle:   TBorderStyle;
    FCanEditModify: Boolean;
    FColCount:      Longint;
    FColWidths,
    FTabStops:      Pointer;
    FCurrent:       TGridCoord;
  end;
{$ENDIF}


procedure TacCharGrid.WndProc(var Message: TMessage);
var
  X, Y: integer;
begin
{$IFDEF LOGGED}
//  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    WM_LBUTTONDOWN:
      if not Clicked then begin
        Clicked := True;
        MouseToCell(TWMMouse(Message).XPos, TWMMouse(Message).YPos, X, Y);
        if IsValidIndex(X, ColCount) and IsValidIndex(Y, RowCount) then begin
{$IFDEF DELPHI6UP}
          FocusCell(X, Y, False);
{$ELSE}
          TAccessStdGrid(Self).FCurrent.X := X;
          TAccessStdGrid(Self).FCurrent.Y := Y;
          Click;
{$ENDIF}
          OnMouseDown(Self, mbLeft, [], X, Y);
        end;
        Clicked := False;
      end;

    else inherited;
  end;
end;

end.
