unit acSlider;
{$I sDefs.inc}

interface

uses
  Windows, SysUtils, Classes, Controls, ExtCtrls, Graphics, Messages, ImgList,
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  {$IFDEF DELPHI_XE2} UITypes, {$ENDIF}
  sMaskData, acntTypes, sCommonData, sPanel, sConst;


type
{$IFNDEF NOTFORHELP}
  TacSliderChangeEvent = procedure(Sender: TObject) of object;
  TacOnChangingEvent   = procedure(Sender: TObject; var CanChange: boolean) of object;
  TControlOrientation  = (coHorizontal, coVertical);
  TSliderContentPlacing = (scpThumb, scpBackground);
  TacImagesUsingMode = (iumGlyphsOnly, iumFullPaint);

  TacOffset = class(TPersistent)
  private
    FTop,
    FLeft,
    FRight,
    FBottom: TacIntProperty;

    FControl: TControl;
    procedure SetOffset(const Index: Integer; Value: TacIntProperty);
  public
    constructor Create(Control: TControl);
  published
    property Left:   TacIntProperty index 0 read FLeft   write SetOffset default 0;
    property Top:    TacIntProperty index 1 read FTop    write SetOffset default 0;
    property Right:  TacIntProperty index 2 read FRight  write SetOffset default 0;
    property Bottom: TacIntProperty index 3 read FBottom write SetOffset default 0;
  end;
{$ENDIF}


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsSlider = class(TsPanel)
{$IFNDEF NOTFORHELP}
  private
    Dragged,
    Capturing,
    FReversed,
    FStretched,
    FUseSymbols,
    FShowCaption,
    FMouseInGlyph,
    FKeepThumbAspectRatio: boolean;

    FImageIndexOn,
    FImageIndexOff,
    FThumbIndexOn,
    FThumbIndexOff,
    FGlyphIndexOn,
    FGlyphIndexOff: TImageIndex;

    FSliderCaptionOn,
    FSliderCaptionOff: acString;

    StretchedSrcBorderOn,
    StretchedSrcBorderOff,
    StretchedSrcThumbOn,
    StretchedSrcThumbOff: TsMaskData;

    ThumbSkinIndexes,
    SliderSkinIndexes: array [boolean] of integer;

    UpdateCount: integer;

    FFontOn: TFont;
    FColorOn: TColor;
    MouseDownSpot: TPoint;
    FThumbSection: string;
    FSliderCursor: TCursor;
    FImages: TCustomImageList;
    FBoundLabel: TsBoundLabel;
    FCustomThumbOffset: TacOffset;
    FThumbSizeInPercent: TPercent;
    FImageChangeLink: TChangeLink;
    FAnimatEvents: TacAnimatEvents;
    FOnChanging: TacOnChangingEvent;
    FOrientation: TControlOrientation;
    FImagesUsingMode: TacImagesUsingMode;
    FOnSliderChange: TacSliderChangeEvent;
    FContentPlacing: TSliderContentPlacing;
    procedure ImageListChange      (Sender: TObject);
    procedure FontOnChange         (Sender: TObject);
    procedure SetFontOn            (const Value: TFont);
    procedure SetThumbSizeInPercent(const Value: TPercent);
    procedure SetSliderCaptionOff  (const Value: acString);
    procedure SetSliderCaptionOn   (const Value: acString);
    procedure SetThumbSection      (const Value: String);
    procedure SetImages            (const Value: TCustomImageList);
    procedure SetOrientation       (const Value: TControlOrientation);
    procedure SetContentPlacing    (const Value: TSliderContentPlacing);
    procedure SetBoolean           (const Index: Integer; const Value: boolean);
    procedure SetImageIndex        (const Index: Integer; const Value: TImageIndex);
    procedure SetImagesUsingMode   (const Value: TacImagesUsingMode);
    procedure SetColorOn(const Value: TColor);
    procedure SetMouseInGlyph(const Value: boolean);
    function GlyphScreenRect(SkipClickArea: boolean): TRect;
  protected
    Pressed,
    Changing,
    FSliderOn: boolean;

    PrevMouse,
    ClickPoint: TPoint;

    StretchedSliderPPI,
    StretchedThumbPPI: integer;

    ThumbSize: TSize;
    ThumbLight: TBitmap;
    procedure UpdateButton;
    function GetThumbSkinIndex(ASliderOn: boolean): integer;
    function CanChange: boolean; virtual;
    function FontStored: boolean;
    function StateFromPos: boolean;
    function BtnInBeginning: boolean;
    function ThumbMargin(Side: TacSide): integer;
    procedure ChangeScale(M, D: Integer{$IFDEF DELPHI_10BERLIN}; isDpiChange: Boolean{$ENDIF}); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure ChangeValueAnim(DoChange: boolean = False);
    procedure ValueChanged; virtual;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp  (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure PaintContent(R: TRect; DstBmp: TBitmap; ASliderOn: boolean);
    procedure UpdateStretchedImages;
    procedure ClearStretchedImages;
    procedure UpdateIndexes;
    function ContentRect(ASliderOn: boolean): TRect;
    function CustomImageUsed: boolean;
    function ThumbImgIndex: integer;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function CurrentState: integer;
    function CurrentStdColor(ASliderOn: boolean): TColor;
    function CurrentSkinColor(ASliderOn: boolean): TColor;
    procedure PaintBG(Bmp: TBitmap; ASliderOn: boolean; CI: TCacheInfo);

    procedure UpdateLight(Sender: TObject; Data: PacAnimEventData);
    procedure HandleThumbLighting(ParentBmp, DstBmp: TBitmap; ASliderOn: boolean);
    property MouseInGlyph: boolean read FMouseInGlyph write SetMouseInGlyph;
  public
    ThumbRect: TRect;
    procedure PaintThumb(ABmp: TBitmap; ASliderOn: boolean);
    function PrepareCache: boolean; override;
    procedure DblClick; override;
    procedure Click; override;
    procedure Paint; override;
    procedure OurPaint(DC: HDC = 0; SendUpdated: boolean = True); override;
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure BeginUpdate;
    procedure EndUpdate(DoRepaint: boolean);
    procedure UpdateSize;
    procedure CreateWnd; override;
    procedure WndProc(var Message: TMessage); override;
  published
    property BevelOuter default bvLowered;
    property Height default 21;
    property Width default 57;
{$ENDIF}
    property AnimatEvents: TacAnimatEvents read FAnimatEvents write FAnimatEvents default [aeGlobalDef];
    property BoundLabel: TsBoundLabel read FBoundLabel write FBoundLabel;
    property Orientation: TControlOrientation read FOrientation write SetOrientation default coHorizontal;
    property Images: TCustomImageList read FImages write SetImages;
    property CustomThumbOffset: TacOffset read FCustomThumbOffset write FCustomThumbOffset;

    property Color default clDefault;
    property ColorOn: TColor read FColorOn write SetColorOn default clDefault;

    property GlyphIndexOff: TImageIndex index 0 read FGlyphIndexOff write SetImageIndex default -1;
    property GlyphIndexOn:  TImageIndex index 1 read FGlyphIndexOn  write SetImageIndex default -1;
    property ImageIndexOff: TImageIndex index 2 read FImageIndexOff write SetImageIndex default -1;
    property ImageIndexOn:  TImageIndex index 3 read FImageIndexOn  write SetImageIndex default -1;
    property ThumbIndexOff: TImageIndex index 4 read FThumbIndexOff write SetImageIndex default -1;
    property ThumbIndexOn:  TImageIndex index 5 read FThumbIndexOn  write SetImageIndex default -1;

    property FontOn: TFont read FFontOn write SetFontOn stored FontStored;
    property SliderCursor: TCursor read FSliderCursor write FSliderCursor default crDefault;
    property SliderCaptionOn:  acString read FSliderCaptionOn  write SetSliderCaptionOn;
    property SliderCaptionOff: acString read FSliderCaptionOff write SetSliderCaptionOff;
    property ThumbSection:     String read FThumbSection write SetThumbSection;
    property ContentPlacing: TSliderContentPlacing read FContentPlacing write SetContentPlacing default scpThumb;
    property ThumbSizeInPercent: TPercent read FThumbSizeInPercent write SetThumbSizeInPercent default 50;
    property ImagesUsingMode: TacImagesUsingMode read FImagesUsingMode write SetImagesUsingMode default iumGlyphsOnly;

    property Reversed:             boolean index 0 read FReversed    write SetBoolean default False;
    property ShowCaption:          boolean index 1 read FShowCaption write SetBoolean default True;
    property SliderOn:             boolean index 2 read FSliderOn    write SetBoolean default True;
    property UseSymbols:           boolean index 3 read FUseSymbols  write SetBoolean default False;
    property Stretched:            boolean index 4 read FStretched   write SetBoolean default True;
    property KeepThumbAspectRatio: boolean index 5 read FKeepThumbAspectRatio write SetBoolean default True;
    property OnChanging: TacOnChangingEvent read FOnChanging write FOnChanging;
    property OnSliderChange: TacSliderChangeEvent read FOnSliderChange write FOnSliderChange;
  end;


implementation

uses
  math,
  sAlphaGraph, sSkinProps, sStyleSimply, acntUtils, sGraphUtils, sMessages, sVCLUtils, sDefaults,
  acGlow, sSkinManager, acAlphaImageList, sGlyphUtils, sThirdParty, acgpUtils, acAnimation, acThdTimer;


const
  ContentMargin = 8;
  DefRadius = 10;


procedure TsSlider.AfterConstruction;
begin
  inherited;
  UpdateButton;
  if FFontOn.Height = 100 then
    FFontOn.Assign(Font);
end;


const
  BtnPosArray: array [boolean, boolean] of boolean = ((False, True), (True, False));

function TsSlider.BtnInBeginning: boolean;
begin
  Result := BtnPosArray[SliderOn, FReversed];
end;


procedure TsSlider.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Button = mbLeft then begin
    ClickPoint.X := X;
    ClickPoint.Y := Y;
    if PtInRect(ThumbRect, ClickPoint) then begin
      Capturing := True;
      MouseDownSpot.X := X;
      MouseDownSpot.Y := Y;
    end;
  end;
end;


procedure TsSlider.SetMouseInGlyph(const Value: boolean);
begin
  if FMouseInGlyph <> Value then begin
    FMouseInGlyph := Value;
    if Value then begin
      if (SkinData.CircleID < 0) and SkinData.Skinned and CircleClickMode(Self, SkinData, AnimatEvents) then
        ShowCircleIfNeeded(SkinData, MaxByte, True {If possible});
    end
    else
      if not (csLButtonDown in SkinData.FOwnerControl.ControlState) then
        HideCircle(SkinData.CircleID);
  end;
end;


const
  iRadiusDiff = 6;

function TsSlider.GlyphScreenRect(SkipClickArea: boolean): TRect;
var
  i: integer;
begin
  if SkipClickArea then
    Result := MkRect(Self)
  else begin
    Result := ThumbRect;
    i := iRadiusDiff * max(ThumbSize.cx, ThumbSize.cy) div 21;
    InflateRect(Result, i, i);
  end;
  with ClientToScreen(MkPoint) do
    OffsetRect(Result, X, Y);
end;


procedure TsSlider.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  OldState: boolean;
  Margin1, Margin2, Step, Size, ThSize, Pos: integer;
begin
  inherited;
  if Capturing then begin
    OldState := FSliderOn;
    Dragged := (Orientation = coHorizontal) and (ClickPoint.X <> X) or (Orientation <> coHorizontal) and (ClickPoint.Y <> Y);
    if Dragged then begin
      if Orientation = coHorizontal then begin
        Margin1 := ThumbMargin(asLeft);
        Margin2 := ThumbMargin(asRight);
        Size := Width;
        ThSize := ThumbSize.cx;
        Pos := ThumbRect.Left - (MouseDownSpot.X - X);
      end
      else begin
        Margin1 := ThumbMargin(asTop);
        Margin2 := ThumbMargin(asBottom);
        Size := Height;
        ThSize := ThumbSize.cy;
        Pos := ThumbRect.Top - (MouseDownSpot.Y - Y);
      end;

      Step := Size div 10;
      if BtnInBeginning then begin
        if (Pos > Size - ThSize - Step - Margin2) and (OldState = FSliderOn) then begin
          SliderOn := not SliderOn;
          Pos := Size - ThSize - Margin2
        end;
      end
      else
        if Pos < Step + Margin1 then begin
          SliderOn := not SliderOn;
          Pos := Margin1;
        end;

      if Pos > Size - ThSize - Margin2 then
        Pos := Size - ThSize - Margin2
      else
        if Pos < Margin1 then
          Pos := Margin1;

      if Orientation = coHorizontal then
        OffsetRect(ThumbRect, Pos - ThumbRect.Left, 0)
      else
        OffsetRect(ThumbRect, 0, Pos - ThumbRect.Top);

      if SkinData.CircleID >= 0 then
        RepaintGlow(SkinData.CircleID);

      SkinData.BGChanged := True;
      RedrawWindow(Handle, nil, 0, RDWA_ALLNOW);
      MouseDownSpot.X := X;
      MouseDownSpot.Y := Y;
    end;
  end;
end;


procedure TsSlider.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  Capturing := False;
  Dragged := False;
  UpdateButton;
  if EventEnabled(aeClick, FAnimatEvents) and not CircleClickMode(Self, SkinData, AnimatEvents) then
    DoClickAnimation(SkinData);
end;


procedure TsSlider.PaintThumb(ABmp: TBitmap; ASliderOn: boolean);
var
  bColor, C: TColor;
  Bmp: TBitmap;
  iPPI: integer;
  ndx, minSize, Offset, pw, r, w, h: integer;
  genData: TsGeneralData;

  procedure PaintImages(ActCanvas: TCanvas);
  var
    h, w: integer;
    DrawData: TacDrawGlyphData;
  begin
    h := GetImageHeight(Images, ThumbImgIndex, iPPI);
    w := GetImageWidth(Images, ThumbImgIndex, iPPI);

    DrawData.ImgRect.Left := ThumbRect.Left - ThumbSize.cx div 2;
    DrawData.ImgRect.Top := ThumbRect.Top;
    DrawData.ImgRect.Right := DrawData.ImgRect.Left + w;
    DrawData.ImgRect.Bottom := DrawData.ImgRect.Top + h;

    DrawData.Images     := Images;
    DrawData.Glyph      := nil;
    DrawData.ImageIndex := ThumbImgIndex;
    DrawData.NumGlyphs  := 1;
    DrawData.Enabled    := Enabled;
    DrawData.Blend      := 0;
    DrawData.Down       := False;
    DrawData.Grayed     := False;

    DrawData.CurrentState      := 0;
    DrawData.DisabledGlyphKind := DefDisabledGlyphKind;
    DrawData.Reflected         := False;

    DrawData.DstBmp := ABmp;
    DrawData.Canvas := ABmp.Canvas;

    DrawData.CommonSkinData := SkinData.CommonSkinData;
    DrawData.PPI := iPPI;
    acDrawGlyphEx(DrawData);
  end;

begin
  if CustomImageUsed then begin
    iPPI := SkinData.CommonSkinData.PPI;
    PaintImages(ABmp.Canvas);
  end
  else begin
    Bmp := CreateBmp32(ThumbSize);
    ndx := ThumbSkinIndexes[ASliderOn];
    if ndx >= 0 then
      genData := SkinData.SkinManager.gd[ndx];

    if not SkinData.CustomColor and (ndx >= 0) and (not Stretched or (genData.BorderIndex >= 0)) then
      if StretchedThumbPPI < 0 then
        PaintItem(ndx, MakeCacheInfo(ABmp), True, CurrentState, MkRect(Bmp), ThumbRect.TopLeft, Bmp, SkinData.CommonSkinData)
      else
        if ASliderOn then
          PaintItem(ndx, MakeCacheInfo(ABmp), True, CurrentState, MkRect(Bmp), ThumbRect.TopLeft, Bmp, SkinData.CommonSkinData, StretchedSrcThumbOn)
        else
          PaintItem(ndx, MakeCacheInfo(ABmp), True, CurrentState, MkRect(Bmp), ThumbRect.TopLeft, Bmp, SkinData.CommonSkinData, StretchedSrcThumbOff)
    else begin
      BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, ABmp.Canvas.Handle, ThumbRect.Left, ThumbRect.Top, SRCCOPY);
      minSize := min(Width, Height);// - 1;
      if Stretched then
        r := minSize div 2// + minSize mod 2
      else
        r := DefRadius;

      if (ndx >= 0) and (genData.BorderWidth > 0) and (genData.Props[CurrentState].BorderColor = SkinData.SkinManager.gd[SkinData.SkinIndex].Props[CurrentState].Color) then begin
        dec(r, genData.BorderWidth);
        Offset := genData.BorderWidth;
      end
      else
        Offset := 0;

      w := Bmp.Width;
      h := Bmp.Height;
      if (ndx < 0) or SkinData.CustomColor then begin
        pw := 0;
        bColor := 0;
        C := clWhite;
      end
      else
        with genData do begin
          pw := ScaleInt(BorderWidth, SkinData);
          C := Props[CurrentState].Color;
          bColor := Props[CurrentState].BorderColor;
        end;

      if Offset > 0 then begin // Transparent border
        bColor := clNone;
        pw := 0;
      end;
//      FillRect32(Bmp, Rect(Offset, Offset, w - Offset, h - Offset), 255);
//      FillDC(Bmp.Canvas.Handle, Rect(Offset, Offset, w - Offset, h - Offset), 255);
      acgpRoundRect(Bmp.Canvas.Handle, Offset, Offset, w - 2 * Offset{ - 1}, h - 2 * Offset{ - 1}, r, pw, C, bColor);
    end;
    BitBlt(ABmp.Canvas.Handle, ThumbRect.Left, ThumbRect.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
    Bmp.Free;
    if ContentPlacing <> scpBackground then
      PaintContent(ThumbRect, ABmp, ASliderOn);
  end;
end;


function TsSlider.CurrentSkinColor(ASliderOn: boolean): TColor;
begin
  if (SliderSkinIndexes[ASliderOn] < 0) then begin // not skinned
    if ASliderOn then
      Result := slBtnGreen
    else
      Result := slBtnRed;

    case CurrentState of
      0: Result := acColorToRGB(Result);
      1: Result := BlendColors($FFFFFF, acColorToRGB(Result), 30);
      else Result := BlendColors(0, acColorToRGB(Result), 50);
    end;
  end
  else
    Result := SkinData.CommonSkinData.gd[SliderSkinIndexes[ASliderOn]].Props[CurrentState].Color
end;


function TsSlider.CurrentState: integer;
begin
  if Pressed then
    Result := ACPRESSED
  else
    if not (csDesigning in ComponentState) and SkinData.FMouseAbove then
      Result := ACHOT
    else
      Result := ACNORMAL
end;


function TsSlider.CurrentStdColor(ASliderOn: boolean): TColor;
begin
  if ASliderOn then
    if ColorOn = clDefault then // If not custom
      case CurrentState of
        0: Result := acColorToRGB(slBtnGreen);
        1: Result := acColorToRGB(slBtnGreenActive)
        else Result := BlendColors(0, acColorToRGB(slBtnGreenActive), 50)
      end
    else
      Result := ColorOn
  else
    if Color = clDefault then // If not custom
      case CurrentState of
        0: Result := acColorToRGB(slBtnRed);
        1: Result := acColorToRGB(slBtnRedActive)
        else Result := BlendColors(0, acColorToRGB(slBtnRedActive), 50)
      end
    else
      Result := Color;

  Result := acColorToRGB(Result);
end;


function TsSlider.CustomImageUsed: boolean;
begin
  Result := (FImagesUsingMode = iumFullPaint) and (Images <> nil) and (ImageIndexOff >= 0) and (ImageIndexOn >= 0);
end;


function TsSlider.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
var
  cy, cx, h: integer;
begin
  if CustomImageUsed then begin
    NewWidth  := GetImageWidth (Images, -1, SkinData.CurrentPPI);
    NewHeight := GetImageHeight(Images, -1, SkinData.CurrentPPI);
  end
  else begin
    cx := 0;
    cy := 0;
    if Images <> nil then begin
      h := GetImageHeight(Images, -1, SkinData.CurrentPPI);
      if Orientation = coHorizontal then
        cx := GetImageWidth(Images, -1, SkinData.CurrentPPI) + 2
      else
        cy := h + 2;
    end
    else
      h := 8;

    if FShowCaption then begin
      NewWidth := (cx + max(Canvas.TextWidth(SliderCaptionOff), Canvas.TextWidth(SliderCaptionOn)) * 2 + ScaleInt(ContentMargin, SkinData) * 3);
      NewHeight := max(cy + Canvas.TextHeight(SliderCaptionOn), h) + ScaleInt(ContentMargin, SkinData);
    end
    else begin
      NewWidth := cx;
      NewHeight := h;
    end;
  end;
  Result := True;
end;


function TsSlider.CanChange: boolean;
begin
  Result := True;
  if Assigned(FOnChanging) then
    FOnChanging(Self, Result);
end;


procedure TsSlider.ChangeScale(M, D: Integer{$IFDEF DELPHI_10BERLIN}; isDpiChange: Boolean{$ENDIF});
begin
  SkinData.CommonSkinData := nil;
  inherited;
  FontOn.Height := MulDiv(FontOn.Height, M, D);
  SkinData.CurrentPPI := SkinData.CommonSkinData.PPI;
  if AutoSize then
    UpdateSize
  else
    UpdateButton;
end;


procedure TsSlider.ChangeValueAnim(DoChange: boolean = False);
var
  DC: HDC;
  lTicks: DWord;
  ThumbPos: real;
  CI: TCacheInfo;
  ResBmp1, ResBmp2, Bmp1, Bmp2: TBitmap;
  IsHorz, Changed: boolean;
  OldPos, b, Target: integer;
  ASliderOn: boolean;
begin
  if not Changing and CanChange then begin
    Changing := True;
    Changed := DoChange;
    CI := GetParentCacheHwnd(Handle);
    IsHorz := Orientation = coHorizontal;
    if IsHorz then
      if BtnInBeginning then
        Target := Width - ThumbSize.cx - ThumbMargin(asRight)
      else
        Target := ThumbMargin(asLeft)
    else
      if BtnInBeginning then
        Target := Height - ThumbSize.cy - ThumbMargin(asBottom)
      else
        Target := ThumbMargin(asTop);

    StopTimer(SkinData);
    if (SkinData.SkinManager = nil) or SkinData.SkinManager.Effects.AllowAnimation then begin
      OldPos := iff(IsHorz, ThumbRect.Left, ThumbRect.Top);
      ThumbPos := OldPos;
      Bmp1 := CreateBmpLike(SkinData.FCacheBmp);
      Bmp2 := CreateBmpLike(SkinData.FCacheBmp);
      PaintBG(Bmp1, FSliderOn, CI);
      HandleLighting(SkinData, CurrentState, PCacheInfo(@CI), Bmp1);

      Pressed := False; // Paint as not pressed
      SkinData.FMouseAbove := True;
      PaintBG(Bmp2, not FSliderOn, CI);
      SkinData.SkinIndex := SliderSkinIndexes[not FSliderOn];
      HandleLighting(SkinData, CurrentState, PCacheInfo(@CI), Bmp2);
      Pressed := True;
      if ContentPlacing = scpBackground then begin
        PaintContent(ContentRect(SliderOn), Bmp1, SliderOn);
        PaintContent(ContentRect(not SliderOn), Bmp2, not SliderOn);
      end;
      b := MaxByte;
      ASliderOn := SliderOn;
      while abs(ThumbPos - Target) > 0.1 do begin
        lTicks := GetTickCount;
        ThumbPos := ThumbPos - (ThumbPos - Target) / 3;
        if IsHorz then begin
          ThumbRect.Left := Round(ThumbPos);
          ThumbRect.Right := ThumbRect.Left + ThumbSize.cx;
        end
        else begin
          ThumbRect.Top := Round(ThumbPos);
          ThumbRect.Bottom := ThumbRect.Top + ThumbSize.cy;
        end;

        if SkinData.CircleID >= 0 then
          RepaintGlow(SkinData.CircleID);

        ResBmp1 := CreateBmpLike(SkinData.FCacheBmp);
        CopyBmp(ResBmp1, Bmp1);
        PaintThumb(ResBmp1, ASliderOn);

        ResBmp2 := CreateBmpLike(SkinData.FCacheBmp);
        CopyBmp(ResBmp2, Bmp2);
        Pressed := False; // Paint as not pressed
        PaintThumb(ResBmp2, not ASliderOn);
        Pressed := True;

        b := b * 2 div 3;
        SumBitmapsToDst(SkinData.FCacheBmp, ResBmp2, ResBmp1, b);
        if not Changed and (abs(ThumbPos - Target) < 1) then begin
          // Change value of the control
          Changed := True;
          FSliderOn := not FSliderOn;
          FreeAndNil(SkinData.HotCache);
          FreeAndNil(SkinData.LightMask);
          FreeAndNil(ThumbLight);
        end;

        ResBmp2.Free;
        ResBmp1.Free;
        DC := GetDC(Handle);
        try
          BitBlt(DC, 0, 0, Width, Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
        finally
          ReleaseDC(Handle, DC);
        end;
        WaitTicks(lTicks);
      end;
      Bmp1.Free;
      Bmp2.Free;
    end
    else begin
      FSliderOn := not FSliderOn;
      FreeAndNil(SkinData.HotCache);
      FreeAndNil(SkinData.LightMask);
      FreeAndNil(ThumbLight);
      UpdateButton;
      SkinData.BGChanged := True;
      RedrawWindow(Handle, nil, 0, RDWA_ALLNOW);
    end;
    ValueChanged;
    if Assigned(FOnSliderChange) then
      FOnSliderChange(Self);

    Changing := False;
  end;
end;


procedure TsSlider.ClearStretchedImages;
begin
  StretchedSliderPPI := -1;
  StretchedThumbPPI := -1;
  FreeAndNil(StretchedSrcBorderOn.Bmp);
  FreeAndNil(StretchedSrcThumbOn.Bmp);
  FreeAndNil(StretchedSrcBorderOff.Bmp);
  FreeAndNil(StretchedSrcThumbOff.Bmp);
end;


procedure TsSlider.Click;
begin
  inherited;
  if not Dragged then
    ChangeValueAnim;
end;


function TsSlider.ContentRect(ASliderOn: boolean): TRect;
begin
  Result := MkRect(Self);
  if FReversed and ASliderOn or not FReversed and not ASliderOn then // First rect
    if Orientation = coHorizontal then
      Result.Right := Width - ThumbSize.cx
    else
      Result.Bottom := Height - ThumbSize.cy
  else
    if Orientation = coHorizontal then
      Result.Left := ThumbSize.cx
    else
      Result.Top := ThumbSize.cy;
end;


constructor TsSlider.Create(AOwner: TComponent);
begin
  inherited;
  SkinData.COC := COC_TsSlider;
  FBoundLabel := TsBoundLabel.Create(Self, SkinData);
  FSliderOn := True;
  FShowCaption := True;
  FOrientation := coHorizontal;
  FUseSymbols := False;
  FSliderCursor := crDefault;
  TabStop := False;
  Pressed := False;
  BevelOuter := bvLowered;
  ThumbLight := nil;
  Width := 57;
  Height := 21;
  Changing := False;
  StretchedSliderPPI := -1;
  StretchedThumbPPI := -1;
  FAnimatEvents := [aeGlobalDef];
  CustomThumbOffset := TacOffset.Create(Self);
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FThumbSizeInPercent := 50;
  FContentPlacing := scpThumb;
  FReversed := False;
  FKeepThumbAspectRatio := True;
  FStretched := True;
  FImagesUsingMode := iumGlyphsOnly;

  Color := clDefault;
  FColorOn := clDefault;

  ThumbSkinIndexes[True] := -1;
  ThumbSkinIndexes[False] := -1;

  FGlyphIndexOff := -1;
  FGlyphIndexOn  := -1;
  FImageIndexOff := -1;
  FImageIndexOn  := -1;
  FThumbIndexOff := -1;
  FThumbIndexOn  := -1;

  FFontOn := TFont.Create;
  FFontOn.OnChange := FontOnChange;
  FFontOn.Height := 100;
  SkinData.OnUpdateLight := UpdateLight;
end;


procedure TsSlider.CreateWnd;
begin
  inherited;
  UpdateIndexes;
  UpdateSize;
end;


procedure TsSlider.DblClick;
begin
  inherited;
  if not Dragged then
    ChangeValueAnim;
end;


destructor TsSlider.Destroy;
begin
  FreeAndNil(FBoundLabel);

  FreeAndNil(StretchedSrcBorderOn.Bmp);
  FreeAndNil(StretchedSrcThumbOn.Bmp);
  FreeAndNil(StretchedSrcBorderOff.Bmp);
  FreeAndNil(StretchedSrcThumbOff.Bmp);
  FreeAndNil(ThumbLight);

  CustomThumbOffset.Free;
  FFontOn.Free;
  FreeAndNil(FImageChangeLink);
  inherited;
end;


procedure TsSlider.FontOnChange(Sender: TObject);
begin
  UpdateSize;
  if SkinData.Skinned then
    SkinData.Invalidate
  else
    Repaint;
end;


function TsSlider.FontStored: boolean;
begin
  Result := IsCustomFont(nil, FFontOn);
end;


procedure TsSlider.ImageListChange(Sender: TObject);
begin
  UpdateSize;
  if SkinData.Skinned then
    SkinData.Invalidate
  else
    Repaint;
end;


procedure TsSlider.Loaded;
begin
  inherited;
  UpdateIndexes;
  UpdateButton;
  if FFontOn.Height = 100 then
    FFontOn.Assign(Font);

  if CustomImageUsed then
    BevelOuter := bvNone;
end;


procedure TsSlider.OurPaint;
var
  Timer: TacThreadedTimer;
begin
  if UpdateCount <= 0 then
    if not SkinData.Skinned or SkinData.CustomColor then begin
      if TimerIsActive(SkinData) then begin
        if (SkinData.PulsTimer <> nil) and SkinData.PulsTimer.Enabled and (SkinData.PulsTimer.BmpFrom <> nil) then
          Timer := SkinData.PulsTimer
        else
          Timer := SkinData.AnimTimer;

        if (Timer <> nil) and Assigned(Timer.BmpOut) and (Timer.BmpOut.Width = Width) then
          BitBlt(DC, 0, 0, Width, Height, Timer.BmpOut.Canvas.Handle, 0, 0, SRCCOPY);
      end
      else begin
        InitCacheBmp(SkinData);
        PaintBG(SkinData.FCacheBmp, SliderOn, GetParentCacheHwnd(Handle));
        if ContentPlacing = scpBackground then
          PaintContent(ContentRect(SliderOn), SkinData.FCacheBmp, SliderOn);

        PaintThumb(SkinData.FCacheBmp, SliderOn);
        BitBlt(DC, 0, 0, Width, Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
      end;
    end
    else
      inherited; // PrepareCache works
end;


procedure TsSlider.Paint;
begin
  if csDesigning in ComponentState then
    inherited;

  if Showing and Assigned(OnPaint) then
    OnPaint(Self, Canvas)
end;


procedure TsSlider.PaintBG(Bmp: TBitmap; ASliderOn: boolean; CI: TCacheInfo);
var
  C, bColor: TColor;
  a, pw, r, w, h: integer;
  DrawData: TacDrawGlyphData;
begin
  if not SkinData.Skinned then
    UpdateIndexes;

  if CustomImageUsed then begin
    if CI.Ready then
      BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, CI.Bmp.Canvas.Handle, Left + CI.X, Top + CI.Y, SRCCOPY)
    else
      FillDC(Bmp.Canvas.Handle, MkRect(Self), ColorToRGB(Color));

    DrawData.Images     := Images;
    DrawData.Glyph      := nil;
    DrawData.ImageIndex := iff(ASliderOn, FImageIndexOn, FImageIndexOff);
    DrawData.ImgRect    := MkRect(Self);
    DrawData.NumGlyphs  := 1;
    DrawData.Enabled    := Enabled;
    DrawData.Blend      := 0;
    DrawData.Down       := False;
    DrawData.Grayed     := False;

    DrawData.CurrentState      := ACNORMAL;
    DrawData.DisabledGlyphKind := DefDisabledGlyphKind;
    DrawData.Reflected         := False;

    DrawData.DstBmp := Bmp;
    DrawData.Canvas := Bmp.Canvas;

    DrawData.CommonSkinData := SkinData.CommonSkinData;
    if SkinData.Skinned then
      DrawData.PPI := DrawData.CommonSkinData.PPI
    else
      DrawData.PPI := sSkinManager.CurrentPPI(SkinData.SkinManager, Self, False);

    acDrawGlyphEx(DrawData);
  end
  else
    if SkinData.CustomColor or (SliderSkinIndexes[ASliderOn] < 0) or Stretched and (SkinData.SkinManager.gd[SliderSkinIndexes[ASliderOn]].BorderIndex < 0) then begin
      if CI.Ready then
        BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, CI.Bmp.Canvas.Handle, Left + CI.X, Top + CI.Y, SRCCOPY)
      else
        FillDC(Bmp.Canvas.Handle, MkRect(Self), GetControlColor(Parent));

      if CustomImageUsed then begin
        SetImagesPPI(Images, GetPPI(SkinData));
        Images.Draw(Bmp.Canvas, 0, 0, iff(ASliderOn, FImageIndexOn, FImageIndexOff));
      end
      else begin
        if (SkinData.SkinIndex >= 0) and not SkinData.CustomColor then
          C := acColorToRGB(CurrentSkinColor(ASliderOn))
        else
          C := acColorToRGB(CurrentStdColor(ASliderOn));

        w := Width;// - 1;
        h := Height;// - 1;
        if Stretched then
          r := min(w, h) div 2
        else
          r := DefRadius;

        if (SliderSkinIndexes[ASliderOn] < 0) or SkinData.CustomColor then begin
          pw := 1;
          A := MaxByte;
          bColor := BlendColors(0, C, 50);
        end
        else begin
          pw := ScaleInt(SkinData.SkinManager.gd[SliderSkinIndexes[ASliderOn]].BorderWidth, SkinData);
          A := SkinData.SkinManager.gd[SliderSkinIndexes[ASliderOn]].Props[CurrentState].Transparency * MaxByte div 100;
          bColor := SkinData.SkinManager.gd[SliderSkinIndexes[ASliderOn]].Props[CurrentState].BorderColor;
        end;
        TsColor(C).A := MaxByte - A;
//        FillRect32(Bmp, Rect(0, 0, w, h), 255);
        acgpRoundRect(Bmp.Canvas.Handle, 0, 0, w, h, r, pw, C, bColor);
      end;
    end
    else
      if StretchedSliderPPI < 0 then
        PaintItem(SliderSkinIndexes[ASliderOn], CI, True, CurrentState, MkRect(Self), Point(Left, Top), Bmp, SkinData.CommonSkinData)
      else
        if ASliderOn then
          PaintItem(SkinData.SkinIndex, CI, True, CurrentState, MkRect(Self), Point(Left, Top), Bmp, SkinData.CommonSkinData, StretchedSrcBorderOn)
        else
          PaintItem(SkinData.SkinIndex, CI, True, CurrentState, MkRect(Self), Point(Left, Top), Bmp, SkinData.CommonSkinData, StretchedSrcBorderOff);
end;


procedure TsSlider.PaintContent(R: TRect; DstBmp: TBitmap; ASliderOn: boolean);
var
  x, y, Ndx, w, h, gSize: integer;
  TranspMode: byte;
  Bmp, BG: TBitmap;
  rPercent: real;
  gRect: TRect;
  s: acString;
  ts: TSize;
  C: TColor;
begin
  x := R.Left;
  y := R.Top;
  w := WidthOf(R);
  h := HeightOf(R);
  if not SkinData.CustomFont and SkinData.Skinned then
    if ContentPlacing = scpThumb then
      Ndx := ThumbSkinIndexes[ASliderOn]
    else
      Ndx := SliderSkinIndexes[ASliderOn]
  else
    Ndx := -1;

  if FUseSymbols then begin
    gSize := max(0, min(w, h) - 4);
    if Ndx >= 0 then
      C := SkinData.CommonSkinData.gd[Ndx].Props[CurrentState].FontColor.Color
    else
      if SkinData.SkinIndex >= 0 then
        if ASliderOn then
          C := acColorToRGB(slEditGreenText)
        else
          C := acColorToRGB(slEditRedText)
      else
        if ASliderOn then
          C := FontOn.Color
        else
          C := Font.Color;

    gRect.Left := R.Left + (WidthOf(R) - gSize) div 2;
    gRect.Top := R.Top + (HeightOf(R) - gSize) div 2;
    gRect.Right := gRect.Left + gSize;
    gRect.Bottom := gRect.Top + gSize;

    SetImagesPPI(acCharImages, GetPPI(SkinData));
    Bmp := acCharImages.CreateBitmap32Color(iff(ASliderOn, 19, 20), gSize, gSize, C);
    if Bmp <> nil then begin
      BG := CreateBmp32(gSize, gSize);
      BitBlt(BG.Canvas.Handle, 0, 0, gSize, gSize, DstBmp.Canvas.Handle, gRect.Left, gRect.Top, SRCCOPY);
      PaintBmpRect32(BG, Bmp, MkRect(Bmp), MkPoint);
      BitBlt(DstBmp.Canvas.Handle, gRect.Left, gRect.Top, gSize, gSize, BG.Canvas.Handle, 0, 0, SRCCOPY);
      BG.Free;
      Bmp.Free;
    end;
  end
  else begin
    if FShowCaption then
      if ASliderOn then begin
        s := FSliderCaptionOn;
        if s <> '' then begin
          SelectObject(DstBmp.Canvas.Handle, FontOn.Handle);
          DstBmp.Canvas.Font.Assign(FontOn);
        end;
      end
      else begin
        s := FSliderCaptionOff;
        if s <> '' then begin
          SelectObject(DstBmp.Canvas.Handle, Font.Handle);
          DstBmp.Canvas.Font.Assign(Font);
        end;
      end
    else
      s := '';

    if (Images <> nil) and (GlyphIndexOff >= 0) and (GlyphIndexOn >= 0) then begin
      rPercent := FThumbSizeInPercent / 100;
      SetImagesPPI(Images, GetPPI(SkinData));
      if Orientation = coHorizontal then begin
        if s = '' then
          x := R.Left + Round((w - Images.Width) * rPercent);

        Images.Draw(DstBmp.Canvas, x, R.Top + Round((h - Images.Height) * rPercent), iff(ASliderOn, GlyphIndexOn, GlyphIndexOff));
        inc(x, Images.Width + 2);
      end
      else begin
        Images.Draw(DstBmp.Canvas, R.Left + Round((w - Images.Width) * rPercent), y, iff(ASliderOn, GlyphIndexOn, GlyphIndexOff));
        inc(y, Images.Height + 2);
      end;
    end;
    if s <> '' then begin
      R.Left := x;
      R.Top := y;
      TranspMode := GetTransMode(SkinData, Ndx, CurrentState);
      ts := acTextExtent(DstBmp.Canvas, s);
      R.Left := R.Left + (WidthOf(R) - ts.cx) div 2;
      R.Top := R.Top + (HeightOf(R) - ts.cy) div 2;
      R.Right := R.Left + ts.cx;
      R.Bottom := R.Top + ts.cy;
      acWriteTextEx(DstBmp.Canvas, PacChar(s), Enabled, R, DT_SINGLELINE or DT_ALPHATEXT * Cardinal(TranspMode = TM_FULL), Ndx, False, SkinData.SkinManager);
      if TranspMode = TM_PARTIAL then
        FillAlphaRect(DstBmp, R);
    end;
  end;
end;


procedure TsSlider.HandleThumbLighting(ParentBmp, DstBmp: TBitmap; ASliderOn: boolean);
var
  C: TColor;
  tR, R: TRect;
  pMouse, pThumbCenter: TPoint;
  wd2, hd2, Ndx, w, h, L1, L2, X, Y, A: integer;
begin
  if SkinData.DoLighting and not CircleClickMode(Self, SkinData, AnimatEvents){ (SkinData.GlowID < 0)} then begin
    Ndx := ThumbSkinIndexes[ASliderOn];
    if Ndx < 0 then
      Ndx := SkinData.SkinIndex;

    if Ndx < 0 then
      Exit;

    tR := ThumbRect;
    w := WidthOf(tR) * 2;
    h := HeightOf(tR) * 2;
    C := SkinData.SkinManager.CommonSkinData.gd[Ndx].Props[CurrentState].LightColor;
    if C = clFuchsia then
      C := GetLightColor(SkinData, CurrentState);

    PrepareLightMask(ThumbLight, w, h, C);
    pMouse := ScreenToClient(acMousePos);
    // Relative coord of light bitmap center
    R.Left := pMouse.X - ThumbLight.Width div 2;
    R.Top := pMouse.Y - ThumbLight.Height div 2;
    // Calc source transparency
    if PtInRect(tR, pMouse) then // Light within the thumb coords?
      // Cursor is within control
      A := MaxByte
    else begin
      wd2 := WidthOf(tR) div 2;
      hd2 := HeightOf(tR) div 2;
      pThumbCenter.X := tR.Left + wd2;
      pThumbCenter.Y := tR.Top + hd2;
      X := abs(pMouse.X - pThumbCenter.X) - wd2;
      Y := abs(pMouse.Y - pThumbCenter.Y) - hd2;
      L1 := Round(Sqrt(Sqr(X) + Sqr(Y)));
      L2 := Round(Sqrt(Sqr(abs(pMouse.X - wd2 - tR.Left)) + Sqr(abs(pMouse.Y - hd2 - tR.Top))));
      A := MaxByte - min((L2 - L1) * MaxByte div acMouseLightSize, MaxByte);
    end;
    PaintTranspBmpRect32(DstBmp, ThumbLight, ParentBmp, clFuchsia, MkRect(ThumbLight), R.TopLeft, R.TopLeft, A);
  end;
end;


function TsSlider.PrepareCache: boolean;
var
  Ndx: integer;
  CI: TCacheInfo;
  TmpBmp: TBitmap;
begin
  if UpdateCount <= 0 then begin
    InitCacheBmp(SkinData);
    SkinData.UpdateIndexes(True);
    Ndx := SkinData.SkinIndex;

    CI := GetParentCacheHwnd(Handle);
    PaintBG(SkinData.FCacheBmp, SliderOn, CI);
    SkinData.SkinIndex := SliderSkinIndexes[SliderOn];

    HandleLighting(SkinData, CurrentState, PCacheInfo(@CI));
    SkinData.SkinIndex := Ndx;
    SkinData.UpdateIndexes(True);
    if ContentPlacing = scpBackground then
      PaintContent(ContentRect(SliderOn), SkinData.FCacheBmp, SliderOn);

    if Assigned(OnPaint) then
      OnPaint(Self, SkinData.FCacheBmp.Canvas);

    TmpBmp := CreateBmpLike(SkinData.FCacheBmp);
    CopyBmp(TmpBmp, SkinData.FCacheBmp);
    PaintThumb(SkinData.FCacheBmp, SliderOn);
    HandleThumbLighting(TmpBmp, SkinData.FCacheBmp, SliderOn);
    TmpBmp.Free;

    if not Enabled then
      BmpDisabledKind(SkinData.FCacheBmp, [dkBlended], Parent, CI, Point(Left, Top));

    SkinData.BGChanged := False;
    Result := True;
  end
  else
    Result := False;
end;


procedure TsSlider.SetFontOn(const Value: TFont);
begin
  FontOn.Assign(Value);
end;


procedure TsSlider.SetImageIndex(const Index: Integer; const Value: TImageIndex);

  procedure ChangeProp(var Prop: TImageIndex; Value: TImageIndex);
  begin
    if Prop <> Value then begin
      Prop := Value;
      UpdateButton;
      SkinData.Invalidate;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FGlyphIndexOff, Value);
    1: ChangeProp(FGlyphIndexOn,  Value);
    2: ChangeProp(FImageIndexOff, Value);
    3: ChangeProp(FImageIndexOn,  Value);
    4: ChangeProp(FThumbIndexOff, Value);
    5: ChangeProp(FThumbIndexOn,  Value);
  end;
end;


procedure TsSlider.SetImages(const Value: TCustomImageList);
begin
  if Images <> Value then begin
    if Images <> nil then
      Images.UnRegisterChanges(FImageChangeLink);

    FImages := Value;
    if Images <> nil then begin
      Images.RegisterChanges(FImageChangeLink);
      Images.FreeNotification(Self);
    end;
    if (Visible or (csDesigning in ComponentState)) and (SkinData.CtrlSkinState and ACS_LOCKED = 0) and not (csLoading in ComponentState) then begin
      UpdateSize;
      UpdateButton;
      SkinData.Invalidate;
    end;
  end;
end;


procedure TsSlider.SetImagesUsingMode(const Value: TacImagesUsingMode);
begin
  if FImagesUsingMode <> Value then begin
    FImagesUsingMode := Value;
    UpdateSize;
    SkinData.Invalidate;
    if not (csLoading in ComponentState) then
      SkinData.OuterEffects.Invalidate; // Update shadow if exists
  end;
end;


procedure TsSlider.SetOrientation(const Value: TControlOrientation);
begin
  if FOrientation <> Value then begin
    FOrientation := Value;
    UpdateButton;
  end;
end;


procedure TsSlider.SetSliderCaptionOff(const Value: acString);
begin
  if FSliderCaptionOff <> Value then begin
    FSliderCaptionOff := Value;
    UpdateSize;
    SkinData.Invalidate;
  end;
end;


procedure TsSlider.SetThumbSection(const Value: String);
begin
  if FThumbSection <> Value then begin
    FThumbSection := Value;
    UpdateButton;
    SkinData.Invalidate;
  end;
end;


procedure TsSlider.SetSliderCaptionOn(const Value: acString);
begin
  if FSliderCaptionOn <> Value then begin
    FSliderCaptionOn := Value;
    UpdateSize;
    SkinData.Invalidate;
  end;
end;


procedure TsSlider.SetColorOn(const Value: TColor);
begin
  if FColorOn <> Value then begin
    FColorOn := Value;
    SkinData.Invalidate;
  end;
end;


procedure TsSlider.SetContentPlacing(const Value: TSliderContentPlacing);
begin
  if FContentPlacing <> Value then begin
    FContentPlacing := Value;
    UpdateButton;
    SkinData.Invalidate;
  end;
end;


procedure TsSlider.SetThumbSizeInPercent(const Value: TPercent);
begin
  if FThumbSizeInPercent <> Value then begin
    FThumbSizeInPercent := Value;
    UpdateButton;
    SkinData.Invalidate;
  end;
end;


function TsSlider.StateFromPos: boolean;
begin
  if Orientation = coHorizontal then
    if not Reversed then
      Result := (ThumbRect.Left < Round((Width - ThumbSize.cx) * FThumbSizeInPercent / 100) - ThumbMargin(asRight))
    else
      Result := (ThumbRect.Left > Round(ThumbSize.cx * FThumbSizeInPercent / 100) + ThumbMargin(asLeft))
  else
    if not Reversed then
      Result := (ThumbRect.Top < Round((Height - ThumbSize.cy) * FThumbSizeInPercent / 100) - ThumbMargin(asBottom))
    else
      Result := (ThumbRect.Top < Round(ThumbSize.cy * FThumbSizeInPercent / 100) + ThumbMargin(asTop))
end;


function TsSlider.ThumbImgIndex: integer;
begin
  Result := iff(FSliderOn, FThumbIndexOn, FThumbIndexOff);
end;


procedure TsSlider.UpdateButton;
var
  oldRect: TRect;
begin
  oldRect := ThumbRect;
  if Orientation = coHorizontal then begin
    ThumbSize.cy := Height - ThumbMargin(asTop) - ThumbMargin(asBottom);
    if FKeepThumbAspectRatio then
      ThumbSize.cx := ThumbSize.cy
    else
      ThumbSize.cx := Width * FThumbSizeInPercent div 100;

    if BtnInBeginning then
      ThumbRect.Left := ThumbMargin(asLeft)
    else
      ThumbRect.Left := Width - ThumbSize.cx - ThumbMargin(asRight);

    ThumbRect.Top := ThumbMargin(asTop);
  end
  else begin
    ThumbSize.cx := Width - ThumbMargin(asLeft) - ThumbMargin(asRight);
    if FKeepThumbAspectRatio then
      ThumbSize.cy := ThumbSize.cx
    else
      ThumbSize.cy := Height * FThumbSizeInPercent div 100;

    ThumbRect.Left := ThumbMargin(asLeft);
    if BtnInBeginning then
      ThumbRect.Top := ThumbMargin(asTop)
    else
      ThumbRect.Top := Height - ThumbSize.cy - ThumbMargin(asBottom);
  end;
  ThumbRect.Right := ThumbRect.Left + ThumbSize.cx;
  ThumbRect.Bottom := ThumbRect.Top + ThumbSize.cy;
  if not EqualRect(oldRect, ThumbRect) then
    SkinData.Invalidate();
end;


procedure TsSlider.UpdateIndexes;
begin
  if SkinData.Skinned then begin
    ThumbSkinIndexes[True] := GetThumbSkinIndex(True);
    ThumbSkinIndexes[False] := GetThumbSkinIndex(False);
    SliderSkinIndexes[True] := SkinData.SkinManager.SkinCommonInfo.Sections[ssSlider_On];
    SliderSkinIndexes[False] := SkinData.SkinManager.SkinCommonInfo.Sections[ssSlider_Off];
  end
  else begin
    ThumbSkinIndexes[True] := -1;
    ThumbSkinIndexes[False] := -1;
    SliderSkinIndexes[True] := -1;
    SliderSkinIndexes[False] := -1;
  end;
end;


procedure TsSlider.UpdateLight(Sender: TObject; Data: PacAnimEventData);
begin
  if IsWindowVisible(Parent.Handle) and Visible and EventEnabled(aeLighting, FAnimatEvents) and not CustomImageUsed then
    DoUpdateLight(SkinData, Data.DoOutput)
  else
    SkinData.FDoLighting := False;

  SkinData.DestroyNeedlessLight;
  Data.DoLighting := SkinData.DoLighting;
end;


procedure TsSlider.UpdateStretchedImages;
var
  ndx, border, DefSize: integer;
begin
  if SkinData.Skinned then begin
    ndx := SliderSkinIndexes[SliderOn];
    if ndx >= 0 then
      border := SkinData.SkinManager.gd[ndx].BorderIndex
    else
      border := -1;
  end
  else
    border := -1;

  if Stretched and SkinData.Skinned and (border >= 0) then begin
    FreeAndNil(StretchedSrcBorderOn.Bmp);
    FreeAndNil(StretchedSrcThumbOn.Bmp);
    FreeAndNil(StretchedSrcBorderOff.Bmp);
    FreeAndNil(StretchedSrcThumbOff.Bmp);
    // Check slider
    if (SliderSkinIndexes[SliderOn] >= 0) and (border >= 0) then
      with SkinData.CommonSkinData.ma[border] do
        if FOrientation = coHorizontal then
          DefSize := WT + WB + 1 // Min height of mask
        else
          DefSize := WL + WR + 1 // Min width of mask
    else
      DefSize := 17;

    if FOrientation = coHorizontal then
      StretchedSliderPPI := Height * 96 div DefSize
    else
      StretchedSliderPPI := Width * 96 div DefSize;

    if StretchedSliderPPI > 772 then
      StretchedSliderPPI := 772;

    StretchedSliderPPI := SkinData.SkinManager.GetScaledMaskData(s_Slider_On, s_BordersMask, StretchedSliderPPI, StretchedSrcBorderOn);
    if (StretchedSliderPPI > 0) and (SliderSkinIndexes[True] >= 0) and (SliderSkinIndexes[False] >= 0) then
      StretchedSliderPPI := SkinData.SkinManager.GetScaledMaskData(s_Slider_Off, s_BordersMask, StretchedSliderPPI, StretchedSrcBorderOff);

    // Check thumb
    if (ThumbSkinIndexes[SliderOn] >= 0) and (SkinData.SkinManager.gd[ThumbSkinIndexes[SliderOn]].BorderIndex >= 0) then
      with SkinData.CommonSkinData.ma[SkinData.SkinManager.gd[ThumbSkinIndexes[SliderOn]].BorderIndex] do
        if FOrientation = coHorizontal then
          DefSize := WT + WB + 1 // Min height of mask
        else
          DefSize := WL + WR + 1 // Min width of mask
    else
      DefSize := 17;

    if FOrientation = coHorizontal then
      StretchedThumbPPI := Height * 96 div DefSize
    else
      StretchedThumbPPI := Width * 96 div DefSize;

    if StretchedThumbPPI > 772 then
      StretchedThumbPPI := 772;

    StretchedThumbPPI := SkinData.SkinManager.GetScaledMaskData(s_Thumb_On, s_BordersMask, StretchedThumbPPI, StretchedSrcThumbOn);
    if StretchedThumbPPI > 0 then // If first image was found
      StretchedThumbPPI := SkinData.SkinManager.GetScaledMaskData(s_Thumb_Off, s_BordersMask, StretchedThumbPPI, StretchedSrcThumbOff);
  end
  else
    ClearStretchedImages;
end;


procedure TsSlider.ValueChanged;
begin
//
end;


procedure TsSlider.UpdateSize;
var
  w, h: integer;
begin
  if AutoSize and not (csLoading in ComponentState) then begin
    if CustomImageUsed then begin
      w := Width;
      h := Height;
      if CanAutoSize(w, h) then begin
        Width := w;
        Height := h;
      end;
    end;
    AdjustSize;
    UpdateButton;
  end;
end;


const
  aSliderSections: array [boolean] of TacSection = (ssSlider_Off, ssSlider_On);


procedure TsSlider.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_GETDEFSECTION: begin
          Message.Result := 7 + 1;
          Exit;
        end;

        AC_GETDEFINDEX: begin
          if SkinData.SkinManager <> nil then begin
            Message.Result := SkinData.SkinManager.SkinCommonInfo.Sections[aSliderSections[SliderOn]] + 1;
            if Message.Result <= 0 then
              Message.Result := SkinData.SkinManager.SkinCommonInfo.Sections[ssPanelLow] + 1;
          end;
          Exit;
        end;

        AC_GETBG: begin
          PacBGInfo(Message.LParam).BgType := btCache;
          PacBGInfo(Message.LParam).Bmp    := SkinData.FCacheBmp;
          PacBGInfo(Message.LParam).Offset := Point(0, 0);
          Exit;
        end;

        AC_SETSCALE: begin
          SkinData.BGChanged := True;
          CommonMessage(Message, SkinData);
          if BoundLabel <> nil then
            BoundLabel.UpdateScale(Message.LParam);

          UpdateSize;
          Exit;
        end;

        AC_REFRESH: if RefreshNeeded(SkinData, Message) then begin
          inherited;
          ClearStretchedImages;
          UpdateButton;
          Exit;
        end;

        AC_SETCHANGEDIFNECESSARY: begin
          SkinData.BGChanged := True;
          if Message.WParamLo = 1 then
            RedrawWindow(Handle, nil, 0, RDW_NOERASE + RDW_NOINTERNALPAINT + RDW_INVALIDATE + RDW_ALLCHILDREN);

          Exit;
        end;

        AC_GETMOUSEAREA: begin
          PRect(Message.LParam)^ := GlyphScreenRect(boolean(Message.WParamLo));
          Message.Result := 1;
          Exit;
        end;

        AC_REMOVESKIN: begin
          inherited;
          UpdateIndexes;
          UpdateButton;
          Exit;
        end;

        AC_PAINTOUTER: begin
          if SliderSkinIndexes[SliderOn] < 0 then
            UpdateIndexes;

          if (ImagesUsingMode = iumFullPaint) or (SliderSkinIndexes[SliderOn] < 0) then
            Exit;
        end;
      end;

    WM_GETTEXTLENGTH: begin
      Message.Result := 0;
      Exit;
    end;

    WM_ERASEBKGND:
      if not (csDesigning in ComponentState) then
        Exit;

    WM_SIZE:
      if CustomImageUsed then begin
        Width := GetImageWidth(Images, SkinData.CommonSkinData.PPI);
        Height := GetImageHeight(Images, SkinData.CommonSkinData.PPI);
        UpdateButton;
        Exit;
      end;

    WM_MOUSEMOVE: if not (csDesigning in ComponentState) then
      if not SkinData.FMouseAbove then begin
        FreeAndNil(SkinData.LightMask);
        FreeAndNil(ThumbLight);
        SkinData.FMouseAbove := True;
        if CircleClickMode(Self, SkinData, AnimatEvents) then
          MouseInGlyph := PtInRect(GlyphScreenRect(False), acMousePos)
        else begin
          SkinData.BGChanged := False;
          DoChangePaint(SkinData, 1, UpdateWindow_CB, EventEnabled(aeMouseEnter, FAnimatEvents), False);
        end;
      end
      else
        if CircleClickMode(Self, SkinData, AnimatEvents) then
          MouseInGlyph := PtInRect(GlyphScreenRect(False), acMousePos);

    WM_MOUSELEAVE: if not (csDesigning in ComponentState) then
      if SkinData.FMouseAbove then begin
        SkinData.DestroyNeedlessLight(False);
        FreeAndNil(SkinData.LightMask);
        FreeAndNil(ThumbLight);
        SkinData.FMouseAbove := acMouseInControl(Self);
        SkinData.BGChanged := False;
        if CircleClickMode(Self, SkinData, AnimatEvents) then
          MouseInGlyph := False
        else
          DoChangePaint(SkinData, 0, UpdateWindow_CB, EventEnabled(aeMouseLeave, FAnimatEvents), False);
      end
      else
        MouseInGlyph := False
  end;
  inherited;
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_SETNEWSKIN, AC_REMOVESKIN: begin
          FreeAndNil(SkinData.LightMask);
          FreeAndNil(SkinData.HotCache);
          FreeAndNil(ThumbLight);
          UpdateIndexes;
        end;
      end;

    WM_SIZE: if IsWindowVisible(Handle) then begin
      UpdateStretchedImages;
      UpdateButton;
      SkinData.OuterEffects.Invalidate;
    end;

    CM_ENABLEDCHANGED:
      SkinData.Invalidate;

    WM_LBUTTONDOWN: if not (csDesigning in ComponentState){ and SkinData.Skinned} then begin
      Pressed := True;
      SkinData.FMouseAbove := True;
      SkinData.BGChanged := False;
      ControlState := ControlState + [csLButtonDown];
      if CircleClickMode(Self, SkinData, AnimatEvents) then begin
        SkinData.Invalidate(True);
        if not SkinIsChanging(SkinData) and not FadingForbidden and SkinData.SkinManager.Effects.AllowAnimation then begin // Animated
          ShowCircleIfNeeded(SkinData, MaxByte, True)
        end
        else begin // Without animation
          if SkinData.FMouseAbove then
            ShowCircleIfNeeded(SkinData)
          else
            if not SkinData.FMouseAbove then
              HideCircle(SkinData.CircleID);
        end
      end
      else
        DoChangePaint(SkinData, 2, UpdateWindow_CB, EventEnabled(aeMouseDown, FAnimatEvents), True, False);
    end;

    WM_LBUTTONUP: if not (csDesigning in ComponentState) and Pressed then begin
      Pressed := False;
      SkinData.FMouseAbove := acMouseInControl(Self);
      SkinData.BGChanged := True;
      if SkinData.Skinned then
        PrepareCache;

      if SkinData.AnimTimer <> nil then
        FreeAndNil(SkinData.AnimTimer.BmpOut);

      ControlState := ControlState - [csLButtonDown];
      if CircleClickMode(Self, SkinData, AnimatEvents) then begin
        if not SkinIsChanging(SkinData) and not FadingForbidden and SkinData.SkinManager.Effects.AllowAnimation then begin // Animated
          ShowCircleIfNeeded(SkinData, MaxByte, True)
        end
        else begin // Without animation
          if SkinData.FMouseAbove then
            ShowCircleIfNeeded(SkinData)
          else
            if not SkinData.FMouseAbove then
              HideCircle(SkinData.CircleID);
        end
      end
      else
        DoChangePaint(SkinData, 1, UpdateWindow_CB, EventEnabled(aeMouseUp, FAnimatEvents), True, False);
    end;
  end;
  if Assigned(BoundLabel) then
    BoundLabel.HandleOwnerMsg(Message, Self);
end;


procedure TsSlider.SetBoolean(const Index: Integer; const Value: boolean);

  procedure ChangeProp(var Prop: boolean; Value: boolean);
  begin
    if Prop <> Value then begin
      Prop := Value;
      UpdateButton;
      SkinData.Invalidate;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FReversed, Value);
    1: ChangeProp(FShowCaption, Value);
    3: ChangeProp(FUseSymbols, Value);
    5: ChangeProp(FKeepThumbAspectRatio, Value);
    4:
      if FStretched <> Value then begin
        FStretched := Value;
        UpdateButton;
        UpdateStretchedImages;
        SkinData.Invalidate;
      end;

    2:
      if FSliderOn <> Value then
        if CanChange then begin
          SkinData.BeginUpdate;
          FreeAndNil(SkinData.LightMask);
          FreeAndNil(SkinData.HotCache);
          FreeAndNil(ThumbLight);
          FSliderOn := Value;
          UpdateButton;
          SkinData.UpdateIndexes;
          SkinData.EndUpdate;
          ValueChanged;
          if not (csLoading in ComponentState) then begin
            SkinData.BGChanged := True;
            RedrawWindow(Handle, nil, 0, RDW_UPDATENOW or RDW_INVALIDATE or RDW_ALLCHILDREN);
            if SkinData.CircleID >= 0 then
              RepaintGlow(SkinData.CircleID);
          end;
          if Assigned(FOnSliderChange) then begin
            FOnSliderChange(Self);
            if GetCapture <> Handle then begin
              Dragged := False;
              Capturing := False;
              UpdateButton;
            end;
          end;
          RestartAttention(SkinData);
        end
        else
          if GetCapture <> Handle then begin
            Dragged := False;
            Capturing := False;
            UpdateButton;
          end;
  end;
end;


procedure TsSlider.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;


procedure TsSlider.BeginUpdate;
begin
  inc(UpdateCount);
end;


procedure TsSlider.EndUpdate(DoRepaint: boolean);
begin
  dec(UpdateCount);
  if DoRepaint then
    Repaint;
end;


function TsSlider.ThumbMargin(Side: TacSide): integer;
begin
  case Side of
    asLeft:  Result := CustomThumbOffset.Left;
    asTop:   Result := CustomThumbOffset.Top;
    asRight: Result := CustomThumbOffset.Right
    else     Result := CustomThumbOffset.Bottom;
  end;

  if (Images = nil) or (ThumbImgIndex < 0) then // If not custom img
    if SkinData.Skinned then
      if (StretchedThumbPPI < 0) or (SkinData.CommonSkinData.SliderMargin < 2) then
        inc(Result, SkinData.CommonSkinData.SliderMargin)
      else
        inc(Result, SkinData.CommonSkinData.SliderMargin * StretchedThumbPPI div 2 div 96)
    else
      Result := 2;
end;


function TsSlider.GetThumbSkinIndex(ASliderOn: boolean): integer;
begin
  if (Images <> nil) and (ThumbImgIndex >= 0) then
    Result := SkinData.SkinManager.SkinCommonInfo.Sections[ssCheckBox]
  else
    if FThumbSection <> '' then
      Result := SkinData.SkinManager.GetSkinIndex(FThumbSection)
    else
      if ASliderOn then
        Result := SkinData.SkinManager.SkinCommonInfo.Sections[ssThumb_On]
      else
        Result := SkinData.SkinManager.SkinCommonInfo.Sections[ssThumb_Off];
end;


constructor TacOffset.Create(Control: TControl);
begin
  inherited Create;
  FControl := Control;
  FLeft   := 0;
  FRight  := 0;
  FTop    := 0;
  FBottom := 0;
end;


procedure TacOffset.SetOffset(const Index: Integer; Value: TacIntProperty);

  procedure ChangeProp(var Prop: TacIntProperty; Value: TacIntProperty);
  begin
    if Prop <> Value then begin
      Prop := Value;
      if not (csLoading in FControl.ComponentState) and (FControl is TsSlider) then begin
        TsSlider(FControl).UpdateButton;
        TsSlider(FControl).SkinData.Invalidate;
      end;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FLeft,   Value);
    1: ChangeProp(FTop,    Value);
    2: ChangeProp(FRight,  Value);
    3: ChangeProp(FBottom, Value);
  end;
end;

end.
