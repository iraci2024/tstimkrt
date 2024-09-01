unit sFontCtrls;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
 Windows, Messages, Classes, Graphics, Controls, StdCtrls,
 {$IFNDEF DELPHI5} Types, {$ENDIF}
 sListBox, sComboBox, acntTypes;


type
{$IFNDEF NOTFORHELP}
  FontTypes = (PS, TTF, RASTER, UNKNOWN);

  TFontClass = class
    FntName: string;
    FntType: FontTypes;
  end;

  TBitmapArray = array [0..3] of TBitmap;
  TFilterOption = (ShowTrueType, ShowPostScript, ShowRaster);
  TFilterOptions = set of TFilterOption;
  EValidateFont = procedure (Sender: TObject; Font: TFontClass; var accept: Boolean) of object;
{$ENDIF} // NOTFORHELP

{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsFontListBox = Class(TsCustomListBox)
{$IFNDEF NOTFORHELP}
  private
    FFilterOptions: TFilterOptions;
    FOnValidateFont: EValidateFont;
    FDrawFont: boolean;
    procedure SetDrawFont(const Value: boolean);
  protected
    FBitmaps: TBitmapArray;
    procedure ChangeScale(M, D: Integer{$IFDEF DELPHI_10BERLIN}; isDpiChange: Boolean{$ENDIF}); override;
    procedure SetFilterOptions(Value: TFilterOptions);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure WndProc(var Message: TMessage); override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
  published
    property Align;
    property BorderStyle;
    property Color;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property Font;
    property IntegralHeight;
    property ItemHeight;
    property Items stored False;
    property MultiSelect;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Style default lbOwnerDrawVariable;
    property TabOrder;
    property TabWidth;
    property Visible;
    property OnValidateFont: EValidateFont read FOnValidateFont write FOnValidateFont;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
{$ENDIF} // NOTFORHELP
    property DrawFont: boolean read FDrawFont write SetDrawFont default True;
    property FilterOptions: TFilterOptions read FFilterOptions write SetFilterOptions default [ShowTrueType, ShowPostScript, ShowRaster];
  end;


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsFontComboBox = Class(TsCustomComboBox)
{$IFNDEF NOTFORHELP}
  private
    FFilterOptions: TFilterOptions;
    FOnValidateFont: EValidateFont;
    FDrawFont: boolean;
    procedure SetDrawFont(const Value: boolean);
  protected
    FBitmaps: TBitmapArray;
    procedure SetFilterOptions(Value: TFilterOptions);
    procedure GetItemDrawData(var DrawData: TacItemDrawData); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CreateWnd; override;
    destructor Destroy; override;
    procedure WndProc (var Message: TMessage); override;
    property Style default csOwnerDrawVariable;
  published
    property Align;
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property Font;
    property ItemHeight;
    property Items stored False;
    property MaxLength;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted default True;
    property TabOrder;
    property TabStop;
    property Text stored TextStored;
    property Visible;
    property OnValidateFont: EValidateFont read FOnValidateFont write FOnValidateFont;
    property OnChange;
    property OnClick;
{$IFDEF DELPHI6UP}
    property OnCloseUp;
{$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
{$ENDIF} // NOTFORHELP
    property DrawFont: boolean read FDrawFont write SetDrawFont default True;
    property FilterOptions: TFilterOptions read FFilterOptions write SetFilterOptions default [ShowTrueType, ShowPostScript, ShowRaster];
  end;


implementation

uses
  SysUtils,
  {$IFDEF LOGGED}sDebugMsgs, {$ENDIF}
  sAlphaGraph, sConst, acntUtils, sGraphUtils, sVclUtils, sCommonData, sStyleSimply, sSkinManager,
  acgpUtils, acAlphaImageList;


type
  TFontsArray = array of TFontClass;

var
  fa: TFontsArray;
  fGlyphs: TsAlphaImageList = nil;


procedure MakeGlyphs(var aBitmaps: TBitmapArray; aScalePercent: integer);
var
  R: TRect;
  i: integer;
begin
  for i := 0 to Length(aBitmaps) - 1 do begin
    if aBitmaps[i] <> nil then
      FreeAndNil(aBitmaps[i]);

    aBitmaps[i] := TBitmap.Create;
    with aBitmaps[i] do begin
      Width := MulDiv(16, aScalePercent, 100);
      Height := Width;
      PixelFormat := pf24bit;
      Canvas.Brush.Style := bsClear;
      Canvas.Font.Height := -Height + 2;
      Canvas.Font.Style := [fsBold];
      FillDC(Canvas.Handle, MkRect(aBitmaps[i]), acColorToRGB(clWindow));
      case i of
        0: begin // PS
          Canvas.Font.Color := $33BB33;
          R := Rect(0, 0, MulDiv(Width, 2, 3), Height - 3);
          acDrawText(Canvas.Handle, 'P', R, 0);
          Canvas.Font.Color := $FF5555;
          R := Rect(MulDiv(Width, 1, 3), 0, Width, Height - 1);
          acDrawText(Canvas.Handle, 's', R, DT_CENTER or DT_VCENTER);
        end;

        1: begin // TTF
          aBitmaps[i].Canvas.Font.Name := 'Courier New';
          Canvas.Font.Color := $888888;
          R := Rect(0, 0, MulDiv(Width, 2, 3), Height - 3);
          acDrawText(Canvas.Handle, 'T', R, 0);
          Canvas.Font.Color := $FF5555;
          R := Rect(MulDiv(Width, 1, 3), 2, Width, Height - 1);
          acDrawText(Canvas.Handle, 'T', R, 0);
        end;

        2: begin // RASTER
          aBitmaps[i].Canvas.Font.Name := 'Courier New';
          Canvas.Font.Color := $0000CC;
          R := Rect(0, 0, Width, Height - 3);
          acDrawText(Canvas.Handle, 'A', R, DT_CENTER or DT_VCENTER);
        end;

        3: begin // UNKNOWN
          Canvas.Font.Color := $000000;
          R := Rect(0, 0, Width, Height - 3);
          acDrawText(Canvas.Handle, '?', R, DT_CENTER or DT_VCENTER);
        end;
      end;
      Transparent := True;
    end;
  end;
end;


procedure MakeImgList;
var
  R: TRect;
  i: integer;
  Bmp: TBitmap;
begin
  if fGlyphs = nil then
    fGlyphs := TsAlphaImageList.Create(nil);

  Bmp := CreateBmp32(32, 32);
  with Bmp do begin
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Height := -Height - 2;
    Canvas.Font.Style := [fsBold];
    for i := 0 to 3 do begin
      FillRect32(Bmp, MkRect(Bmp), 0, 0);
      case i of
        0: begin // PS
          R := Rect(0, 0, MulDiv(Width, 2, 3), Height - 3);
          acgpDrawString(Canvas.Handle, 'P', R, TColor($FF33BB33), taCenter, taVerticalCenter, 0, True);
          R := Rect(MulDiv(Width, 1, 3), 0, Width, Height - 1);
          acgpDrawString(Canvas.Handle, 's', R, TColor($FF5555FF), taCenter, taVerticalCenter, 0, True);
        end;

        1: begin // TTF
          Canvas.Font.Name := 'Courier New';
          R := Rect(0, 0, MulDiv(Width, 2, 3), Height - 3);
          acgpDrawString(Canvas.Handle, 'T', R, TColor($FF888888), taCenter, taVerticalCenter, 0, True);
          R := Rect(MulDiv(Width, 1, 3), 2, Width, Height - 1);
          acgpDrawString(Canvas.Handle, 'T', R, TColor($FF5555FF), taCenter, taVerticalCenter, 0, True);
        end;

        2: begin // RASTER
          Canvas.Font.Name := 'Courier New';
          R := Rect(0, 0, Width, Height - 3);
          acgpDrawString(Canvas.Handle, 'A', R, TColor($FFCC0000), taCenter, taVerticalCenter, 0, True);
        end;

        3: begin // UNKNOWN
          R := Rect(0, 0, Width, Height - 3);
          acgpDrawString(Canvas.Handle, '?', R, TColor($FF000000), taCenter, taVerticalCenter, 0, True);
        end;
      end;
      Transparent := True;
      fGlyphs.Add(Bmp, nil);
    end;
  end;
  Bmp.Free;
end;


function GetNearestFontHeight(ACanvas: TCanvas; AHeight: integer; DefHeight: integer): integer;
var
  Size: TSize;
begin
  ACanvas.Font.Height := DefHeight + 2;
  Result := ACanvas.Font.Height;
  if acGetTextExtent(ACanvas.Handle, s_Yy, Size) then
    while Size.cy < AHeight - 2 do begin
      Result := ACanvas.Font.Height;
      ACanvas.Font.Height := ACanvas.Font.Height - 1;
      if not acGetTextExtent(ACanvas.Handle, s_Yy, Size) then
        Exit;
    end;
end;


function EnumFontFamProc(var LogFont: TLogFont; var TextMetric: TTextMetric; FontType: Integer; Data: Pointer): Integer; StdCall;
var
  s: string;
  i, l: integer;
  FontClass: TFontClass;
begin
  Result := 1;
  s := LogFont.lfFaceName;
  l := Length(fa);
  for i := 0 to l - 1 do
    if (AnsiCompareText(fa[i].FntName, s) = 0) or (s = '') then
      Exit; // Skip duplicates Synchronize

  FontClass := TFontClass.Create;
  with FontClass do begin
    FntName := LogFont.lfFaceName;
    case FontType of
      1:   FntType := RASTER;
      2:   FntType := PS;
      4:   FntType := TTF;
      else FntType := UNKNOWN;
    end;
  end;
  SetLength(fa, Length(fa) + 1);
  fa[Length(fa) - 1] := FontClass;
end;


procedure GetFonts(Sender: TControl);
var
  i: integer;
  cont: Boolean;
  fc: TFontClass;
begin
  if not TWinControl(Sender).HandleAllocated then
    TWinControl(Sender).HandleNeeded;

  if Sender is TsFontListBox then
    with Sender as TsFontListBox do begin
      Items.BeginUpdate;
      Items.Clear;
      for i := 0 to Length(fa) - 1 do begin
        Cont := True;
        if Assigned(FOnValidateFont) then
          FOnValidateFont(Sender, fa[i], Cont);

        fc := fa[i];
        if Cont then
          with fa[i] do
            case FntType of
              PS:
                if ShowPostScript in FFilterOptions then
                  Items.AddObject(FntName, fa[i]);

              TTF:
                if ShowTrueType in FFilterOptions then
                  Items.AddObject(FntName, fa[i]);

              RASTER:
                if ShowRaster in FFilterOptions then
                  Items.AddObject(FntName, fc);

              else
                Items.AddObject(FntName, fa[i]);
            end;
      end;
      Items.EndUpdate;
    end
  else
    with Sender as TsFontComboBox do begin
      Items.BeginUpdate;
      Items.Clear;
      for i := 0 to Length(fa) - 1 do begin
        Cont := True;
        if Assigned(FOnValidateFont) then
          FOnValidateFont(Sender, fa[i], Cont);

        if Cont then
          with fa[i] do begin
            case FntType of
              PS:
                if ShowPostScript in FFilterOptions then
                  Items.AddObject(FntName, fa[i]);

              TTF:
                if ShowTrueType in FFilterOptions then
                  Items.AddObject(FntName, fa[i]);

              RASTER:
                if ShowRaster in FFilterOptions then
                  Items.AddObject(FntName, fa[i]);

              else
                Items.AddObject(FntName, fa[i]);
            end;
          end;
      end;
      Items.EndUpdate;
    end;
end;


procedure GetAllInstalledScreenFonts;
var
  DC: HDC;
begin
  DC := GetDC(0);
  EnumFontFamilies(DC, nil, @EnumFontFamProc, 0);
  ReleaseDC(0, DC);
end;


procedure UpdateGlobalFontList();
var
  iC: integer;
begin
  for iC := 0 to Length(fa) - 1 do
    TFontClass(fa[iC]).Free;

  SetLength(fa, 0);
  GetAllInstalledScreenFonts;
end;


constructor TsFontListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Sorted := True;
  FFilterOptions := [ShowTrueType, ShowPostScript, ShowRaster];
  FDrawFont := True;
end;


constructor TsFontComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Sorted := True;
  FRequestExtData := True;
  Style := csOwnerDrawVariable;
  FFilterOptions := [ShowTrueType, ShowPostScript, ShowRaster];
  FDrawFont := True;
end;


procedure TsFontComboBox.CreateWnd;
begin
  inherited;
  MakeImgList;
  Images := fGlyphs;
  GetFonts(Self);
end;


procedure TsFontListBox.SetFilterOptions(Value: TFilterOptions);
begin
  if FFilterOptions <> Value then begin
    FFilterOptions := Value;
    if not (csLoading in ComponentState) then
      GetFonts(Self);
  end;
end;


procedure TsFontComboBox.SetFilterOptions(Value: TFilterOptions);
begin
  if FFilterOptions <> Value then begin
    FFilterOptions := Value;
    if not (csLoading in ComponentState) then
      GetFonts(Self);
  end;
end;


destructor TsFontListBox.Destroy;
var
  i: integer;
begin
  for i := 0 to 3 do
    FreeAndNil(FBitmaps[i]);

  inherited;
end;


procedure TsFontListBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  TmpFontClass: TFontClass;
  DrawStyle: Cardinal;
  aRect, BRect: TRect;
  Bmp, fBmp: TBitmap;
  TmpColor: TColor;
  i, sNdx: integer;
  CI: TCacheInfo;
  ts: TSize;
begin
  TmpFontClass := nil;
  Bmp := nil;
  if IsValidIndex(Index, Items.Count) and (Items.Objects[Index] <> nil) then begin
    TmpFontClass := TFontClass(Items.Objects[Index]);
    fBmp := FBitmaps[ord(TmpFontClass.FntType)];
    aRect := MkRect(fBmp);
  end
  else
    fBmp := nil;

  if SkinData.Skinned then begin
    Bmp := CreateBmp32(Rect);
    Bmp.Canvas.Font.Assign(Font);
    if odSelected in State then begin
      CI.Bmp := nil;
      CI.Ready := False;
      CI.FillColor := Color;
      sNdx := SkinData.SkinManager.SkinCommonInfo.Sections[ssSelection];
      if sNdx < 0 then
        FillDC(Bmp.Canvas.Handle, MkRect(Bmp), SkinData.SkinManager.GetHighLightColor(odFocused in State))
      else
        PaintItem(sNdx, CI, True, integer(odFocused in State), MkRect(Bmp), MkPoint, Bmp, SkinData.CommonSkinData)
    end
    else begin
      sNdx := -1;
      FillDC(Bmp.Canvas.Handle, MkRect(Bmp), Color);
    end;
    if Assigned(TmpFontClass) and Assigned(fBmp) then begin
      bRect := MkRect(Bmp.Height, Bmp.Height);
      OffsetRect(bRect, 2, (Bmp.Height - fBmp.Height) div 2);
      Bmp.Canvas.Draw(bRect.Left, bRect.Top, fBmp);
      aRect.Right := Bmp.Width;
      aRect.Left := brect.right;
      aRect.Top := 0;
      aRect.Bottom := Bmp.Height;
      if DrawFont then
        Bmp.Canvas.Font.Name := TmpFontClass.FntName;

      Bmp.Canvas.Font.Height := GetNearestFontHeight(Bmp.Canvas, ItemHeight, Font.Height);
      DrawStyle := DT_NOPREFIX or DT_EXPANDTABS or DT_SINGLELINE or DT_VCENTER or DT_NOCLIP or DT_ALPHATEXT * Cardinal(SkinData.TranspMode = TM_FULL);
      InflateRect(aRect, -1, 0);
      if sNdx = -1 then begin
        if odSelected in State then
          Bmp.Canvas.Font.Color := SkinData.SkinManager.GetHighLightFontColor(odFocused in State)
        else
          with SkinData.CommonSkinData.gd[SkinData.SkinIndex] do begin
            i := integer(ControlIsActive(SkinData) and (States > 1));
            Bmp.Canvas.Font.Color := Props[i].FontColor.Color;
          end;

        Bmp.Canvas.Brush.Style := bsClear;
        AcDrawText(Bmp.Canvas.Handle, TmpFontClass.FntName, aRect, DrawStyle);
      end
      else
        WriteTextEx(Bmp.Canvas, PChar(TmpFontClass.FntName), True, aRect, DrawStyle, sNdx, (odFocused in State), SkinData.SkinManager);

      if SkinData.TranspMode = TM_PARTIAL then
        FillAlphaRect(Bmp, aRect);
     end;
    BitBlt(Canvas.Handle, Rect.Left, Rect.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
    FreeAndNil(Bmp);
  end
  else begin
    TmpColor := ColorToRGB(iff(odSelected in State, clHighLight, Color));
    FillDC(Canvas.Handle, Rect, TmpColor);
    if Assigned(TmpFontClass) then begin
      bRect := classes.Rect(Rect.Left, Rect.Top, Rect.Bottom - Rect.Top + Rect.Left, Rect.Bottom);
      OffsetRect(bRect, 2, (Rect.Bottom - Rect.Top - fBmp.Height) div 2);
      Canvas.Draw(bRect.Left, bRect.Top, fBmp);
      Rect.Left := Rect.Left + brect.right;
      if DrawFont then
        Canvas.Font.Name := TmpFontClass.FntName;

      Canvas.Font.Height := GetNearestFontHeight(Canvas, ItemHeight, Font.Height);
      Canvas.Font.Color := iff(odSelected in State, ColorToRGB(clHighlightText), Font.Color);
      GetTextExtentPoint32(Canvas.Handle, PChar(TmpFontClass.FntName), Length(TmpFontClass.FntName), ts);
      Canvas.Brush.Style := bsClear;
      Canvas.TextRect(Rect, Rect.Left + 1, Rect.Top + (HeightOf(Rect) - ts.cy) div 2, TmpFontClass.FntName);
    end;
  end;
end;


destructor TsFontComboBox.Destroy;
var
  i: integer;
begin
  for i := 0 to 3 do
    FreeAndNil(FBitmaps[i]);

  inherited;
end;


procedure TsFontComboBox.GetItemDrawData(var DrawData: TacItemDrawData);
var
  TmpFontClass: TFontClass;
begin
  DrawData.Text := Items[DrawData.ItemIndex];
  if Items.Objects[DrawData.ItemIndex] <> nil then begin
    TmpFontClass := TFontClass(Items.Objects[DrawData.ItemIndex]);
    DrawData.ImageIndex := ord(TmpFontClass.FntType);
    if DrawFont then
      DrawData.Font.Name := TmpFontClass.FntName;
  end;
end;


procedure TsFontListBox.ChangeScale(M, D: Integer{$IFDEF DELPHI_10BERLIN}; isDpiChange: Boolean{$ENDIF});
begin
  MakeGlyphs(FBitmaps, M);
  inherited;
end;


procedure TsFontListBox.SetDrawFont(const Value: boolean);
begin
  if FDrawFont <> Value then begin
    FDrawFont := Value;
    if not (csLoading in ComponentState) then
      Invalidate;
  end;
end;


procedure TsFontComboBox.SetDrawFont(const Value: boolean);
begin
  if FDrawFont <> Value then begin
    FDrawFont := Value;
    if not (csLoading in ComponentState) then
      Invalidate;
  end;
end;


procedure TsFontListBox.Loaded;
begin
  inherited;
  GetFonts(Self);
  MakeGlyphs(FBitmaps, GetPPI(SkinData));
end;


procedure TsFontComboBox.WndProc(var Message: TMessage);
var
  s: string;
  i: integer;
  CurrentFontName: string;
begin
  case Message.Msg of
    CM_FONTCHANGE, WM_FONTCHANGE: begin
      if ItemIndex >= 0 then
        CurrentFontName := Items[ItemIndex];

      UpdateGlobalFontList();
      GetFonts(Self);
      ItemIndex := Items.IndexOf(CurrentFontName);
    end;

    WM_SETTEXT:
      if not (csLoading in ComponentState) and not (csCreating in ControlState) and HandleAllocated and not SkinData.FUpdating {Control is not ready} then begin // Change a font when text is changed
        s := PChar(Message.LParam);
        inherited;
        i := Items.IndexOf(s);
        if ItemIndex <> i then
          ItemIndex := i;

        Exit;
      end;
  end;
  inherited;
end;


procedure TsFontListBox.WndProc(var Message: TMessage);
var
  CurrentFontName: string;
begin
  case Message.Msg of
    CM_FONTCHANGE, WM_FONTCHANGE: begin
      if ItemIndex >= 0 then
        CurrentFontName := Items[ItemIndex];

      UpdateGlobalFontList();
      GetFonts(Self);
      ItemIndex := Items.IndexOf(CurrentFontName);
    end;
  end;
  inherited;
end;


var
  iC: integer;

initialization
  GetAllInstalledScreenFonts;


finalization
{$WARNINGS OFF}
  for iC := 0 to Length(fa) - 1 do
    TFontClass(fa[iC]).Free;

  SetLength(fa, 0);

  if fGlyphs <> nil then
    fGlyphs.Free;

end.



