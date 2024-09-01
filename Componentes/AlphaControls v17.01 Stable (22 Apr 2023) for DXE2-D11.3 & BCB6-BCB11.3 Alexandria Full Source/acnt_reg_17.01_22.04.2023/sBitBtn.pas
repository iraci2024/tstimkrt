unit sBitBtn;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Imglist, StdCtrls, Buttons,
  {$IFDEF LOGGED}     sDebugMsgs, {$ENDIF}
  {$IFNDEF DELPHI5}   Types,      {$ENDIF}
  {$IFDEF TNTUNICODE} TntButtons, {$ENDIF}
  {$IFDEF FPC}        LMessages,  {$ENDIF}
  sCommonData, sConst, sDefaults, acAnimation, acntTypes;


type
{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
{$IFDEF TNTUNICODE}
  TsBitBtn = class(TTntBitBtn)
  published
{$ELSE}
  TsBitBtn = class(TBitBtn)
{$ENDIF}
{$IFNDEF NOTFORHELP}
  private
    FDown,
    FGrayed,
    FShowFocus,
    FReflected,
    FShowCaption,
    FMouseClicked,
    FAcceptsControls,
    FDrawOverBorder: boolean;

    FOnMouseEnter,
    FOnMouseLeave: TNotifyEvent;

    FBlend,
    FOffset,
    FOldSpacing,
    FImageIndex,
    FFocusMargin: integer;

    FAlignment,
    FTextAlignment: TAlignment;

    FFocusedColor: TColor;
    FGlyphColorTone: TColor;
    FOnPaint: TBmpPaintEvent;
{$IFNDEF DELPHI_10SYDNEY}
    FImages: TCustomImageList;
{$ENDIF}
    FCommonData: TsCtrlSkinData;
    FImageChangeLink: TChangeLink;
    FDisabledKind: TsDisabledKind;
    FAnimatEvents: TacAnimatEvents;
    FOnGetColoring: TacGetColoring;
    FVerticalAlignment: TVerticalAlignment;
    FDisabledGlyphKind: TsDisabledGlyphKind;
{$IFNDEF DELPHI7UP}
    FWordWrap: boolean;
    procedure SetWordWrap(const Value: boolean);
{$ENDIF}
    procedure BmpChanged(Sender: TObject);
    function GetGrayed: boolean;
    function GetDown: boolean;
    procedure ImageListChange(Sender: TObject);

    procedure SetDisabledKind     (const Value: TsDisabledKind);
    procedure SetDisabledGlyphKind(const Value: TsDisabledGlyphKind);
    procedure SetImages           (const Value: TCustomImageList);
    procedure SetAlignment        (const Value: TAlignment);
    procedure SetTextAlignment    (const Value: TAlignment);
    procedure SetVerticalAlignment(const Value: TVerticalAlignment);

    procedure SetInteger          (const Index, Value: integer);
    procedure SetBoolean          (const Index: Integer; const Value: boolean);

    procedure SetDown             (const Value: boolean);
    procedure SetGrayed           (const Value: boolean);

    procedure SetGlyphColorTone   (const Value: TColor);

    procedure WMKeyUp    (var Message: TWMKey);       message WM_KEYUP;
    procedure CNDrawItem (var Message: TWMDrawItem);  message CN_DRAWITEM;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure SetFocusedColor(const Value: TColor);
  protected
    IsFocused,
    ControlsShifted: boolean;

    GlyphPos: TPoint;
    CaptionRect: TRect;
    OldLayout: TButtonLayout;
    function GetColorTone: TColor; virtual;
    procedure StdDrawItem(const DrawItemStruct: TDrawItemStruct);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp  (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure OurPaintHandler(aDC: hdc);
    procedure CopyImage(DC: HDC; Bmp: TBitmap);
    procedure DoDrawText(var Rect: TRect; Flags: Cardinal; CanGlow: boolean; TranspMode: integer);
    procedure DrawCaption(const Coloring: TacColoring; Canvas: TCanvas = nil);
    procedure ShiftControls(Offset: integer);
    function TextRectSize: TSize;
    function GlyphWidth: integer;
    function GlyphHeight: integer;

    function PrepareCache: boolean;
    procedure GetColoring(var Coloring: TacColoring);
    procedure UpdateLight(Sender: TObject; Data: PacAnimEventData); virtual;
  public
    function GetImages: TCustomImageList; virtual;
    function CurrentImageIndex: integer; virtual;
    function UseRightToLeftAlignment: Boolean; override;

    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
    function CurrentState: integer; virtual;
    destructor Destroy; override;
    procedure Click; override;
    procedure Loaded; override;
    function ImgRect: TRect;
    procedure Invalidate; override;
    procedure WndProc(var Message: TMessage); override;
    procedure SetButtonStyle(ADefault: Boolean); {$IFNDEF FPC} override; {$ENDIF}
  published
    property OnPaint: TBmpPaintEvent read FOnPaint write FOnPaint;
{$ENDIF} // NOTFORHELP
    property AcceptsControls: boolean Index 0 read FAcceptsControls write SetBoolean default False;
    property DrawOverBorder:  boolean Index 1 read FDrawOverBorder  write SetBoolean default True;
    property Reflected:       boolean Index 2 read FReflected       write SetBoolean default False;
    property ShowCaption:     boolean Index 3 read FShowCaption     write SetBoolean default True;
    property ShowFocus:       boolean Index 4 read FShowFocus       write SetBoolean default True;

    property Down:   boolean read GetDown   write SetDown   default False;
    property Grayed: boolean read GetGrayed write SetGrayed default False;

    property Blend:       integer Index 0 read FBlend       write SetInteger default 0;
    property FocusMargin: integer Index 1 read FFocusMargin write SetInteger default 1;
    property ImageIndex:  integer Index 2 read FImageIndex  write SetInteger default -1;
    property TextOffset:  integer Index 3 read FOffset      write SetInteger default 0;

    property Alignment:         TAlignment          read FAlignment         write SetAlignment         default taCenter;
    property AnimatEvents:      TacAnimatEvents     read FAnimatEvents      write FAnimatEvents        default [aeGlobalDef];
    property DisabledGlyphKind: TsDisabledGlyphKind read FDisabledGlyphKind write SetDisabledGlyphKind default DefDisabledGlyphKind;
    property DisabledKind:      TsDisabledKind      read FDisabledKind      write SetDisabledKind      default DefDisabledKind;
    property FocusedColor:      TColor              read FFocusedColor      write SetFocusedColor      default clNone;
    property GlyphColorTone:    TColor              read FGlyphColorTone    write SetGlyphColorTone    default clNone;
    property Images:            TCustomImageList    read GetImages          write SetImages;
    property VerticalAlignment: TVerticalAlignment  read FVerticalAlignment write SetVerticalAlignment default taVerticalCenter;
    property SkinData:          TsCtrlSkinData      read FCommonData        write FCommonData;
    property TextAlignment:     TAlignment          read FTextAlignment     write SetTextAlignment     default taCenter;
    property Margin default DefButtonMargin;

{$IFNDEF DELPHI7UP}
    property WordWrap: boolean read FWordWrap write SetWordWrap default True;
{$ELSE}
    property WordWrap default True;
{$ENDIF}
    property OnGetColoring: TacGetColoring read FOnGetColoring write FOnGetColoring;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  end;

implementation

uses
  math, ActnList,
  sVCLUtils, acThdTimer, sMessages, acntUtils, sGraphUtils, sAlphaGraph, acGlow, sThirdParty, sSkinManager, sStyleSimply,
  sMaskData, sGradient;


var
  bFocusChanging: boolean = False;


function IsImgListDefined(Btn: TsBitBtn): boolean;
begin
  with Btn do
    Result := (GetImages <> nil) and IsValidIndex(CurrentImageIndex, GetImageCount(GetImages));
end;


function MaxCaptionWidth(Button: TsBitBtn): integer;
begin
  with Button do
    if ShowCaption and (Caption <> '') then begin
      Result := Width - 2 * Margin;
      case Layout of
        blGlyphLeft, blGlyphRight:
          Result := Result - (Spacing + GlyphWidth) * integer(GlyphWidth <> 0);
      end;
    end
    else
      Result := 0
end;


procedure TsBitBtn.SetButtonStyle(ADefault: Boolean);
begin
  inherited;
  if ADefault <> IsFocused then begin
    IsFocused := ADefault;
    if (SkinData <> nil) and not (csDestroying in ComponentState) then
      SkinData.Invalidate;
  end;
end;


procedure TsBitBtn.AfterConstruction;
begin
  inherited;
  FCommonData.FCacheBmp.Canvas.Font.Assign(Font);
  FCommonData.Loaded;
end;


procedure TsBitBtn.BmpChanged(Sender: TObject);
begin
  SkinData.Invalidate;
end;


constructor TsBitBtn.Create(AOwner: TComponent);
begin
  FCommonData := TsCtrlSkinData.Create(Self, True);
  FCommonData.COC := COC_TsBitBtn;
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csOpaque];
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FFocusMargin := 1;
  FDown := False;
  FFocusedColor := clNone;
  Glyph.OnChange := BmpChanged;

  FImageIndex := -1;
  AcceptsControls := False;
  FGrayed := False;
  ControlsShifted := False;
  FBlend := 0;
  FDisabledGlyphKind := DefDisabledGlyphKind;
  FDisabledKind := DefDisabledKind;
  FDrawOverBorder := True;
  FVerticalAlignment := taVerticalCenter;
  FOffset := 0;
  FAlignment := taCenter;
  FGlyphColorTone := clNone;
  FShowCaption := True;
  FTextAlignment := taCenter;
  FShowFocus := True;
  FAnimatEvents := [aeGlobalDef];
  Margin := DefButtonMargin;
{$IFNDEF DELPHI7UP}
  FWordWrap := True;
{$ELSE}
  WordWrap := True;
{$ENDIF}
  FCommonData.OnUpdateLight := UpdateLight;
end;


function TsBitBtn.CurrentImageIndex: integer;
begin
{$IFDEF DELPHI_10SYDNEY}
  case CurrentState of
    1:
      if HotImageIndex >= 0 then
        Result := HotImageIndex
      else
        Result := FImageIndex;

    2:
      if PressedImageIndex >= 0 then
        Result := PressedImageIndex
      else
        Result := max(HotImageIndex, FImageIndex);

    else Result := FImageIndex;
  end;
{$ELSE}
  Result := FImageIndex;
{$ENDIF}
end;


function TsBitBtn.CurrentState: integer;
var
  Wnd: THandle;
begin
  if not (csDesigning in ComponentState) then
    if ((SendMessage(Handle, BM_GETSTATE, 0, 0) and BST_PUSHED = BST_PUSHED) or fGlobalFlag) and
         (SkinData.FMouseAbove or not (csLButtonDown in ControlState) or ((SkinData.SkinManager <> nil) and SkinData.SkinManager.Options.NoMouseHover)) or FDown then
      Result := 2
    else
      if ControlIsActive(FCommonData) then
        Result := 1
      else
        if Default then begin
          Wnd := GetFocus;
          // Focused control is a button
          if Wnd <> 0 then
            Result := iff(GetWindowLong(Wnd, GWL_STYLE) and BS_USERBUTTON = BS_USERBUTTON, 0, 3)
          else
            Result := 3
        end
        else
          Result := 0
  else
    Result := 0;
end;


destructor TsBitBtn.Destroy;
begin
  FreeAndNil(FImageChangeLink);
  FreeAndNil(FCommonData);
  inherited Destroy;
end;


procedure TsBitBtn.DoDrawText(var Rect: TRect; Flags: Cardinal; CanGlow: boolean; TranspMode: integer);
begin
{$IFNDEF FPC}
  Flags := Cardinal(DrawTextBiDiModeFlags(Flags)) or DT_ALPHATEXT * Cardinal(TranspMode = 2);
{$ENDIF}
  acWriteTextEx(FCommonData.FCacheBMP.Canvas, PacChar(Caption), True, Rect, Flags, FCommonData, boolean(CurrentState), SkinData.SkinManager, CanGlow);
end;


procedure TsBitBtn.DrawCaption;
var
  DrawStyle: Cardinal;
  TranspMode: byte;
  State: integer;
  R: TRect;
begin
  if ShowCaption then begin
    if Canvas = nil then
      Canvas := FCommonData.FCacheBmp.Canvas;

    Canvas.Font.Assign(Font);
    Canvas.Brush.Style := bsClear;
    R := CaptionRect;
    if (CurrentState = 2) and CanClickShift(SkinData.SkinIndex, SkinData.CommonSkinData) then
      OffsetRect(R, 1, 1);

    DrawStyle := DT_EXPANDTABS or GetStringFlags(Self, FTextAlignment) or DT_END_ELLIPSIS;
    if WordWrap then
      DrawStyle := DrawStyle or DT_WORDBREAK;

    if UseRightToLeftReading then
      DrawStyle := DrawStyle or DT_RTLREADING;

    TranspMode := GetTransMode(SkinData);
    if TranspMode = 2 then
      DrawStyle := DrawStyle or DT_ALPHATEXT;

    if SkinData.Skinned then begin
      if Coloring.ColorText <> clNone then begin
        Canvas.Font.Color := acColorToRGB(Coloring.ColorText);
        acDrawText(Canvas.Handle, PacChar(Caption), R, DrawStyle or DT_ALPHATEXT * Cardinal(TranspMode = 2));
      end
      else
        DoDrawText(R, DrawStyle, Coloring.ColorToneBG = clNone, TranspMode);

      if Enabled and not (csDesigning in ComponentState) and
           SkinData.SkinManager.ButtonsOptions.ShowFocusRect and
             Focused and
               (Caption <> '') and
                 ShowFocus and
                   ShowCaption then begin
        InflateRect(R, FocusMargin + 1, FocusMargin);
        State := min(ac_MaxPropsIndex, FCommonData.CommonSkinData.gd[FCommonData.SkinIndex].States);
        FocusRect(Canvas, R, FCommonData.CommonSkinData.gd[FCommonData.SkinIndex].Props[State].FontColor.Color, clNone);
      end;
      if TranspMode = 1 then
        FillAlphaRect(FCommonData.FCacheBmp, R);
    end
    else begin
      Canvas.Brush.Style := bsClear;
      if not Enabled then begin
        OffsetRect(R, 1, 1);
        Canvas.Font.Color := clBtnHighlight;
        acDrawText(Canvas.Handle, PacChar(Caption), R, DrawStyle);
        OffsetRect(R, -1, -1);
        Canvas.Font.Color := clBtnShadow;
      end;
      SetBkColor(Canvas.Handle, ColorToRGB(clBtnFace));
      acDrawText(Canvas.Handle, PacChar(Caption), R, DrawStyle);
    end;
  end;
end;


procedure TsBitBtn.GetColoring(var Coloring: TacColoring);
begin
  Coloring.ColorToneBG := clNone;
  Coloring.ColorGlyph := clNone;
  Coloring.ColorText := clNone;

  if (FFocusedColor <> clNone) and (FFocusedColor <> clBtnFace) and Focused then
    Coloring.ColorToneBG := FFocusedColor
  else
    if SkinData.ColorTone <> clNone then
      Coloring.ColorToneBG := SkinData.ColorTone
    else
      case ModalResult of
        idOK, idYes: begin
          if (SkinData.SkinManager = nil) or (bcGlyph in SkinData.SkinManager.ButtonsOptions.ModalButtonsColoring) then
            Coloring.ColorGlyph := iff(CurrentState = 0, slBtnGreenText, slBtnGreenTextActive);

          if SkinData.Skinned then begin
            if bcText in SkinData.SkinManager.ButtonsOptions.ModalButtonsColoring then
              Coloring.ColorText := iff(CurrentState = 0, slBtnGreenText, slBtnGreenTextActive);

            if bcBackground in SkinData.SkinManager.ButtonsOptions.ModalButtonsColoring then
              Coloring.ColorToneBG := iff(CurrentState = 0, slBtnGreen, slBtnGreenActive);
          end;
        end;
        idCancel, idNo: begin
          if (SkinData.SkinManager = nil) or (bcGlyph in SkinData.SkinManager.ButtonsOptions.ModalButtonsColoring) then
            Coloring.ColorGlyph := iff(CurrentState = 0, slBtnRedText, slBtnRedTextActive);

          if SkinData.Skinned then begin
            if bcText in SkinData.SkinManager.ButtonsOptions.ModalButtonsColoring then
              Coloring.ColorText := iff(CurrentState = 0, slBtnRedText, slBtnRedTextActive);

            if bcBackground in SkinData.SkinManager.ButtonsOptions.ModalButtonsColoring then
              Coloring.ColorToneBG := iff(CurrentState = 0, slBtnRed, slBtnRedActive);
          end;
        end;
      end;

    if Assigned(FOnGetColoring) then
      FOnGetColoring(Self, CurrentState, Coloring);
end;


function TsBitBtn.GetColorTone: TColor;
begin
  Result := GlyphColorTone;
end;


function TsBitBtn.GetDown: boolean;
begin
  Result := FDown;
end;


function TsBitBtn.GlyphHeight: integer;
begin
  if IsImgListDefined(Self) then
    Result := GetImageHeight(GetImages, CurrentImageIndex, GetPPI(SkinData), True)
  else
    if (Glyph <> nil) and (Glyph.Height > 0) then
      Result := Glyph.Height
    else
      Result := 0;
end;


function TsBitBtn.GlyphWidth: integer;
begin
  if IsImgListDefined(Self) then
    Result := GetImageWidth(GetImages, CurrentImageIndex, GetPPI(SkinData), True) div NumGlyphs
  else
    if (Glyph <> nil) and (Glyph.Width > 0) then
      Result := Glyph.Width div NumGlyphs
    else
      Result := 0;
end;


function TsBitBtn.ImgRect: TRect;
begin
  Result.Left   := GlyphPos.x;
  Result.Top    := GlyphPos.y;
  Result.Right  := GlyphPos.x + GlyphWidth;
  Result.Bottom := GlyphPos.y + GlyphHeight;
  if (CurrentState = 2) and CanClickShift(SkinData.SkinIndex, SkinData.CommonSkinData) then
    OffsetRect(Result, 1, 1);

  if Reflected then
    OffsetRect(Result, 0, - WidthOf(Result) div 6);
end;


procedure TsBitBtn.Invalidate;
begin
  if OldLayout <> Layout then begin
    OldLayout := Layout;
    FCommonData.BGChanged := True;
  end;
  inherited;
end;


procedure TsBitBtn.Loaded;
begin
  inherited;
  FCommonData.FCacheBmp.Canvas.Font.Assign(Font);
  FCommonData.Loaded;
  if IsImgListDefined(Self) then
    Glyph.Assign(nil);
end;


procedure TsBitBtn.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FCommonData.Skinned and Enabled and not (csDesigning in ComponentState) then begin
    FCommonData.Updating := False;
    if (Button = mbLeft) and not ShowHintStored then begin
      AppShowHint := Application.ShowHint;
      Application.ShowHint := False;
      ShowHintStored := True;
    end;
    FMouseClicked := True;
    if Button = mbLeft then
      if not Down then begin
        FCommonData.FUpdating := FCommonData.Updating;
        FCommonData.BGChanged := False;
        if FCommonData.SkinManager.Effects.AllowAnimation and (beClick in FCommonData.SkinManager.AnimEffects.Buttons.Events) and (FCommonData.SkinManager.AnimEffects.Buttons.ClickEffect in [ceCircle, ceCircleAndWave]) then
          DoChangePaint(FCommonData, 2, UpdateFillLight, True, False, False)
        else
          DoChangePaint(FCommonData, 2, UpdateWindow_CB, EventEnabled(aeMouseDown, FAnimatEvents), False{True}, False);
      end;

    if ShowHintStored then begin;
      Application.ShowHint := AppShowHint;
      ShowHintStored := False
    end;
    if (FCommonData.PulsTimer <> nil) and FCommonData.PulsTimer.Enabled then
      FCommonData.PulsTimer.PrepareImage;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;


procedure TsBitBtn.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not (csDestroying in ComponentState) then begin
    if (FCommonData <> nil) and FCommonData.Skinned and Enabled and not (csDesigning in ComponentState) then begin
      if Button = mbLeft then begin
        Application.ShowHint := AppShowHint;
        ShowHintStored := False;
      end;
      if FMouseClicked then begin
        FMouseClicked := False;
        if (Button = mbLeft) and Enabled then begin
          if FCommonData.SkinManager.Effects.AllowAnimation and (beClick in FCommonData.SkinManager.AnimEffects.Buttons.Events) and (FCommonData.SkinManager.AnimEffects.Buttons.ClickEffect in [ceCircle, ceCircleAndWave]) and (SkinData.AnimTimer <> nil) then
            if SkinData.AnimTimer.Enabled then begin // Animation is not finished
              SkinData.AnimTimer.State := 1;
              SkinData.BGChanged := True; // Change cache
              SkinData.AnimTimer.TimeHandler;
            end
            else begin// just repaint
              if (SkinData.AnimTimer.ThreadType = TT_FILLLIGHT) and (FCommonData.SkinManager.AnimEffects.Buttons.ClickEffect = ceCircleAndWave) then begin
                SkinData.AnimTimer.ThreadType := 0;
                if acMouseInControl(Self) then
                  DoClickAnimation(SkinData);
              end;
              SkinData.Invalidate;
            end
          else begin
            if (SkinData.AnimTimer <> nil) and SkinData.AnimTimer.Enabled then begin
              SkinData.AnimTimer.Enabled := False;
              FCommonData.BGChanged := True;
              fGlobalFlag := True;
              RedrawWindow(Handle, nil, 0, RDW_UPDATENOW or RDW_INVALIDATE);
              fGlobalFlag := False;
              Sleep(30);
            end;
            FCommonData.FUpdating := False;
            if (Self <> nil) and not (csDestroying in ComponentState) then begin
              FCommonData.BGChanged := False;
              if Assigned(FCommonData) and FCommonData.FMouseAbove then
                DoChangePaint(FCommonData, 1, UpdateWindow_CB, EventEnabled(aeMouseUp, FAnimatEvents), False{True});
            end;
          end;
        end;
      end
      else
        if SkinData.AnimTimer <> nil then
          FreeAndNil(SkinData.AnimTimer);

      if (FCommonData.PulsTimer <> nil) and FCommonData.PulsTimer.Enabled then
        FCommonData.PulsTimer.PrepareImage;
    end;
    inherited MouseUp(Button, Shift, X, Y);
  end;
end;


procedure TsBitBtn.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
{$IFNDEF DELPHI_10SYDNEY}
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
{$ENDIF}
end;


procedure TsBitBtn.OurPaintHandler;
var
  DC: hdc;
  PS: TPaintStruct;
  Timer: TacThreadedTimer;

  procedure UnInit;
  begin
    if aDC <> DC then
      ReleaseDC(Handle, DC);

    if not InanimationProcess and ((aDC <> SkinData.PrintDC) or (aDC = 0)) then
      EndPaint(Handle, {$IFDEF FPC}@{$ENDIF}PS);
  end;

begin
  if InAnimationProcess and ((aDC <> SkinData.PrintDC) or (aDC = 0)) then
    Exit;

  if aDC = 0 then
    DC := GetDC(Handle)
  else
    DC := aDC;

  if not InAnimationProcess and ((aDC <> SkinData.PrintDC) or (aDC = 0)) then
    BeginPaint(Handle, PS);

  try
    if not InUpdating(FCommonData) then
      if not TimerIsActive(SkinData) then begin
        FCommonData.BGChanged := FCommonData.BGChanged or FCommonData.HalfVisible or GetBoolMsg(Parent, AC_GETHALFVISIBLE);
        FCommonData.HalfVisible := not RectInRect(Parent.ClientRect, BoundsRect);
        if not FCommonData.BGChanged then
          if FOldSpacing <> Spacing then begin
            FCommonData.BGChanged := True;
            FOldSpacing := Spacing;
          end;

        with FCommonData.SkinManager do
          if (FCommonData.BGChanged or (csDesigning in ComponentState {for glyph changing})) then
            if not PrepareCache then begin
              UnInit;
              Exit;
            end;

        CopyImage(DC, FCommonData.FCacheBmp);
{
        sVCLUtils.PaintControls(DC, Self, True, MkPoint);
}
      end
      else begin
        if (SkinData.PulsTimer <> nil) and SkinData.PulsTimer.Enabled and (SkinData.PulsTimer.BmpFrom <> nil) then
          Timer := SkinData.PulsTimer
        else
          Timer := SkinData.AnimTimer;

        if (Timer <> nil) and (Timer.BmpOut <> nil) then
          CopyImage(DC, Timer.BmpOut);
      end;
  finally
    UnInit;
  end;
end;


function TsBitBtn.PrepareCache: boolean;
var
  CI: TCacheInfo;
  BorderNdx, bMode, State: integer;
  sm: TsSkinManager;
  BGInfo: TacBGInfo;
  Coloring: TacColoring;
  csd: TacSkinData;
begin
  if FCommonData.SkinIndex >= 0 then begin
    GetBGInfo(@BGInfo, Parent);
    if BGInfo.BgType = btNotReady then begin
      FCommonData.FUpdating := True;
      Result := False;
    end
    else begin
      Result := True;
      CI := BGInfoToCI(@BGInfo);
      InitCacheBmp(SkinData);
      sm := FCommonData.SkinManager;
      csd := FCommonData.CommonSkinData;
      if (CurrentState = 3 {Def/Focused}) and (csd.gd[FCommonData.SkinIndex].States < 4) then
        State := 1
      else
        State := min(CurrentState, csd.gd[FCommonData.SkinIndex].States - 1);

      PaintItemBG(FCommonData, CI, State, MkRect(Self), Point(Left, Top), FCommonData.FCacheBMP, integer(State = 2), integer(State = 2));
      if FDrawOverBorder then begin
        BorderNdx := csd.gd[SkinData.SkinIndex].BorderIndex;
        if SkinData.CustomBorder.IsChanged then
          bMode := 2
        else
          if sm.IsValidImgIndex(BorderNdx) then
            bMode := integer((State <> 0) or (csd.ma[BorderNdx].DrawMode and BDM_ACTIVEONLY = 0))
          else
            if csd.gd[SkinData.SkinIndex].Props[State].BorderColor <> clFuchsia then
              bMode := 2
            else
              bMode := 0; // No border

        inc(CI.X, Left);
        inc(CI.Y, Top);
        case bMode of
          1: DrawSkinRect(SkinData.FCacheBmp, MkRect(SkinData.FCacheBmp), CI, csd.ma[csd.gd[SkinData.SkinIndex].BorderIndex], State, False);
          2: TryPaintBorder(SkinData.FCacheBmp, MkRect(FCommonData.FCacheBmp), SkinData.SkinManager.gd[SkinData.SkinIndex], State, CI, SkinData.CustomBorder, SkinData.CurrentPPI, UpdateCtrlCornersProc);
          0: if SkinData.CommonSkinData.gd[SkinData.SkinIndex].BorderRadius > 0 then
               UpdateCorners(SkinData.CommonSkinData.gd[SkinData.SkinIndex], FCommonData.FCacheBmp, MkRect(FCommonData.FCacheBmp), CI);
        end;
        dec(CI.X, Left);
        dec(CI.Y, Top);
      end;

      HandleLighting(FCommonData, State);

      GetColoring(Coloring);
      UpdateBmpColors(FCommonData.FCacheBmp, SkinData, True, State, Coloring.ColorToneBG);
      CalcButtonLayout(ClientRect, Point(GlyphWidth, GlyphHeight), TextRectSize, Layout, Alignment, Margin, Spacing, GlyphPos,
                       CaptionRect, {$IFDEF FPC}0{$ELSE}DrawTextBiDiModeFlags(0){$ENDIF}, VerticalAlignment);

      if FOffset <> 0 then
        case Layout of
          blGlyphLeft, blGlyphRight: OffsetRect(CaptionRect, FOffset, 0);
          blGlyphTop, blGlyphBottom: OffsetRect(CaptionRect, 0, FOffset);
        end;

      DrawCaption(Coloring, nil);

      if (Coloring.ColorGlyph = clNone) and ((SkinData.SkinManager = nil) or not ((SkinData.SkinManager.Effects.DiscoloredGlyphs or Grayed) and (State = 0))) then
        Coloring.ColorGlyph := GetColorTone;

      DrawBtnGlyph(Self, FCommonData.FCacheBmp.Canvas, Coloring.ColorGlyph);
      if Assigned(FOnPaint) then
        FOnPaint(Self, FCommonData.FCacheBmp);

      if not FDrawOverBorder and sm.IsValidImgIndex(csd.gd[SkinData.SkinIndex].BorderIndex) then
        if (State <> 0) or (csd.ma[csd.gd[SkinData.SkinIndex].BorderIndex].DrawMode and BDM_ACTIVEONLY = 0) then
          DrawSkinRect(FCommonData.FCacheBmp, MkRect(FCommonData.FCacheBmp), CI, csd.ma[csd.gd[SkinData.SkinIndex].BorderIndex], State, True);

      if not Enabled or ((Action <> nil) and not Assigned(TAction(Action).OnExecute){ not TAction(Action).Enabled // Button not repainted immediately if Action.Enabled changed }) then
        BmpDisabledKind(FCommonData.FCacheBmp, FDisabledKind, Parent, CI, Point(Left, Top));

      FCommonData.BGChanged := False;
      if State = 2 then begin
        if not ControlsShifted then begin
          ControlsShifted := True;
          ShiftControls(1);
        end;
      end
      else
        if ControlsShifted then begin
          ControlsShifted := False;
          ShiftControls(-1);
        end;
    end;
  end
  else
    Result := False;
end;


procedure TsBitBtn.SetAlignment(const Value: TAlignment);
begin
  if FAlignment <> Value then begin
    FAlignment := Value;
    FCommonData.Invalidate;
  end;
end;


procedure TsBitBtn.SetBoolean(const Index: Integer; const Value: boolean);

  procedure ChangeProp(var Prop: boolean; Value: boolean);
  begin
    if Prop <> Value then begin
      Prop := Value;
      FCommonData.Invalidate;
    end;
  end;

begin
  case Index of
    0: if FAcceptsControls <> Value then begin
      FAcceptsControls := Value;
      if Value then
        ControlStyle := ControlStyle + [csAcceptsControls]
      else
        ControlStyle := ControlStyle - [csAcceptsControls]
    end;
    1: ChangeProp(FDrawOverBorder, Value);
    2: ChangeProp(FReflected,      Value);
    3: ChangeProp(FShowCaption,    Value);
    4: ChangeProp(FShowFocus,      Value);
  end;
end;


procedure TsBitBtn.SetDisabledGlyphKind(const Value: TsDisabledGlyphKind);
begin
  if FDisabledGlyphKind <> Value then begin
    FDisabledGlyphKind := Value;
    FCommonData.Invalidate;
  end;
end;


procedure TsBitBtn.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    FCommonData.Invalidate;
  end;
end;


{$IFNDEF DELPHI7UP}
procedure TsBitBtn.SetWordWrap(const Value: boolean);
begin
  if FWordWrap <> Value then begin
    FWordWrap := Value;
    FCommonData.Invalidate;
  end;
end;
{$ENDIF}


procedure TsBitBtn.SetDown(const Value: boolean);
begin
  if FDown <> Value then begin
    FDown := Value;
    FCommonData.Invalidate;
  end;
end;


procedure TsBitBtn.SetFocusedColor(const Value: TColor);
begin
  if FFocusedColor <> Value then begin
    FFocusedColor := Value;
    FCommonData.Invalidate;
  end;
end;


procedure TsBitBtn.SetGlyphColorTone(const Value: TColor);
begin
  if FGlyphColorTone <> Value then begin
    FGlyphColorTone := Value;
    FCommonData.Invalidate;
  end;
end;


procedure TsBitBtn.SetGrayed(const Value: boolean);
begin
  if FGrayed <> Value then begin
    FGrayed := Value;
    FCommonData.Invalidate;
  end;
end;


procedure TsBitBtn.SetImages(const Value: TCustomImageList);
begin
  if GetImages <> Value then begin
{$IFDEF DELPHI_10SYDNEY}
    inherited Images := Value;
{$ELSE}
    if Images <> nil then
      Images.UnRegisterChanges(FImageChangeLink);

    FImages := Value;
    if Images <> nil then begin
      Images.RegisterChanges(FImageChangeLink);
      Images.FreeNotification(Self);
    end;
{$ENDIF}
    FCommonData.Invalidate;
  end;
end;


procedure TsBitBtn.SetInteger(const Index, Value: integer);

  procedure ChangeProp(var Prop: integer; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      FCommonData.Invalidate;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FBlend, iff(Value < 0, 0, iff(Value > 100, 100, Value)));
    1: ChangeProp(FFocusMargin, Value);
    2: ChangeProp(FImageIndex, Value);
    3: ChangeProp(FOffset, Value);
  end;
end;


function TsBitBtn.TextRectSize: TSize;
var
  R: TRect;
  TranspMode: byte;
  DrawStyle: Cardinal;
begin
  R := MkRect(MaxCaptionWidth(Self), 0);
  DrawStyle := DT_EXPANDTABS or GetStringFlags(Self, FTextAlignment) or DT_CALCRECT;
  if WordWrap then
    DrawStyle := DrawStyle or DT_WORDBREAK;

  FCommonData.FCacheBMP.Canvas.Font.Assign(Font);
  TranspMode := GetTransMode(SkinData);
  acDrawText(FCommonData.FCacheBMP.Canvas.Handle, Caption, R, DrawStyle or DT_ALPHATEXT * Cardinal(TranspMode = 2));
  Result := MkSize(R);
end;


procedure TsBitBtn.UpdateLight(Sender: TObject; Data: PacAnimEventData);
begin
  if IsWindowVisible(Handle) and Enabled and EventEnabled(aeLighting, FAnimatEvents) and (ControlCount = 0) then
    DoUpdateLight(SkinData, Data.DoOutput)
  else
    SkinData.FDoLighting := False;

  SkinData.DestroyNeedlessLight;
  Data.DoLighting := SkinData.DoLighting;
end;


function TsBitBtn.UseRightToLeftAlignment: Boolean;
begin
  Result := BidiMode <> bdLeftToRight;
end;


procedure TsBitBtn.WMKeyUp(var Message: TWMKey);
begin
  inherited;
  if Assigned(FCommonData) and FCommonData.Skinned and (Message.CharCode = 32) then begin
    FCommonData.BGChanged := True;
    Repaint;
  end;
end;


procedure TsBitBtn.WndProc(var Message: TMessage);
var
  bFillLight: boolean;
  PS: TPaintStruct;
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  if (Message.Msg = WM_KILLFOCUS) and (csDestroying in ComponentState) then
    Exit;

  if Message.Msg = SM_ALPHACMD then
    case Message.WParamHi of
      AC_CTRLHANDLED: begin
        Message.Result := 1;
        Exit;
      end; // AlphaSkins is supported

      AC_SETNEWSKIN:
        if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
          CommonMessage(Message, FCommonData);
          AlphaBroadCast(Self, Message);
          Exit;
        end;

      AC_REMOVESKIN:
        if (ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager)) and not (csDestroying in ComponentState) then begin
          AlphaBroadCast(Self, Message);
          CommonMessage(Message, FCommonData);
          Repaint;
          Exit;
        end;

      AC_REFRESH:
        if RefreshNeeded(SkinData, Message) then begin
          CommonMessage(Message, FCommonData);
          if SkinData.PrintDC = 0 then
            Repaint;

          AlphaBroadCast(Self, Message);
          Exit;
        end;

      AC_PREPARECACHE: begin
        Message.Result := LRESULT(not PrepareCache);
        Exit;
      end;

      AC_GETDEFSECTION: begin
        Message.Result := 1 + 1;
        Exit;
      end;

      AC_GETDEFINDEX: begin
        if FCommonData.SkinManager <> nil then
          Message.Result := FCommonData.SkinManager.SkinCommonInfo.Sections[ssButton] + 1;

        Exit;
      end;

      AC_GETCOLORTONE: begin
        if Message.LParam <> 0 then begin
          GetColoring(PacColoring(Message.LParam)^);
          Message.Result := 1;
        end;
        Exit;
      end;

      AC_SETGLASSMODE: begin
        CommonMessage(Message, FCommonData);
        Exit;
      end;
    end;

  if not ControlIsReady(Self) or not FCommonData.Skinned(True) then begin
    case Message.Msg of
      CM_MOUSEENTER: begin
        SkinData.FMouseAbove := True;
        if Assigned(FOnMouseEnter) and Enabled and not (csDesigning in ComponentState) then
          FOnMouseEnter(Self);
      end;

      CM_MOUSELEAVE: begin
        SkinData.FMouseAbove := False;
        if Assigned(FOnMouseLeave) and Enabled and not (csDesigning in ComponentState) then
          FOnMouseLeave(Self);
      end;

      WM_PRINT: begin
        Perform(WM_PAINT, Message.WParam, Message.LParam);
        Exit;
      end;

      SM_ALPHACMD:
        if CommonMessage(Message, FCommonData) then
          Exit;
    end;
    inherited
  end
  else begin
    case Message.Msg of
      SM_ALPHACMD:
        case Message.WParamHi of
          AC_ENDPARENTUPDATE: begin
            if FCommonData.FUpdating then begin
              if not InUpdating(FCommonData, True) then
                Repaint;

              SetParentUpdated(Self);
            end;
            Exit;
          end

          else
            if CommonMessage(Message, FCommonData) then
              Exit;
        end;

      WM_WINDOWPOSCHANGED:
        SkinData.BGChanged := True;

      CM_UIACTIVATE:
        SkinData.Updating := False;

      CM_DIALOGCHAR:
        if Enabled and Focused and (TCMDialogChar(Message).CharCode = VK_SPACE) then begin
          StopTimer(SkinData);
          FCommonData.BGChanged := True;
          HideGlow(SkinData.GlowID);
          Repaint;
        end;

      CM_MOUSEENTER:
        if Enabled and not (csDesigning in ComponentState) then begin
          if not FCommonData.FMouseAbove and not SkinData.SkinManager.Options.NoMouseHover then begin
            FreeAndNil(SkinData.LightMask);
            if Assigned(FOnMouseEnter) then
              FOnMouseEnter(Self);

            FCommonData.FMouseAbove := True;
            FCommonData.BGChanged := False;
            DoChangePaint(FCommonData, 1, UpdateWindow_CB, EventEnabled(aeMouseEnter, FAnimatEvents), False);
          end;
        end;

      CM_MOUSELEAVE:
        if Enabled and not (csDesigning in ComponentState) then begin
          FreeAndNil(SkinData.LightMask);
          if Assigned(FOnMouseLeave) then
            FOnMouseLeave(Self);

          SkinData.DestroyNeedlessLight(False);
          FCommonData.FMouseAbove := False;
          FCommonData.BGChanged := False;
          if (FCommonData.AnimTimer <> nil) and (FCommonData.AnimTimer.ThreadType = TT_FILLLIGHT) then begin
            bFillLight := True;
            FCommonData.AnimTimer.ThreadType := 0;
          end
          else
            bFillLight := False;

          DoChangePaint(FCommonData, 0, UpdateWindow_CB, EventEnabled(aeMouseLeave, FAnimatEvents), False, False);
          if bFillLight and (FCommonData.SkinManager.AnimEffects.Buttons.ClickEffect = ceCircleAndWave) then
            DoClickAnimation(SkinData);
        end;

      WM_UPDATEUISTATE:
        if Visible or (csDesigning in ComponentState) then begin
          Message.Result := 0;
          Exit;
        end;

      WM_ERASEBKGND:
        if Visible or (csDesigning in ComponentState) then begin
          if not InUpdating(FCommonData) then begin
            if (TWMPaint(Message).DC <> 0) and (Skindata.FCacheBmp <> nil) and not FCommonData.BGChanged and ((SkinData.AnimTimer = nil) or not SkinData.AnimTimer.Enabled) then
              CopyWinControlCache(Self, FCommonData, MkRect, MkRect(Self), TWMPaint(Message).DC, False);

            Message.Result := 0;
          end;
          Exit;
        end;

      WM_SETTEXT:
        if Showing then begin
          FinishTimer(SkinData.AnimTimer);
          StopTimer(SkinData);
          if not (csDesigning in ComponentState) then begin
            SetRedraw(Handle, 0);
            inherited;
            SetRedraw(Handle, 1);
          end
          else
            inherited;

          RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
          Exit;
        end;

      CM_TEXTCHANGED:
        if not (csDestroying in ComponentState) and (Visible or (csDesigning in ComponentState)) then begin
          StopTimer(SkinData);
          FCommonData.Invalidate;
          Exit;
        end;

      WM_PRINT: begin
        FCommonData.FUpdating := False;
        OurPaintHandler(TWMPaint(Message).DC);
        Exit;
      end;

      WM_PAINT:
        if Visible or (csDesigning in ComponentState) then begin
          if Parent = nil then
            Exit;

          if InUpdating(FCommonData) then begin
            BeginPaint(Handle, PS);
            EndPaint(Handle, PS);
            Exit;
          end;
          OurPaintHandler(TWMPaint(Message).DC);
          if not (csDesigning in ComponentState) then
            Exit;
        end;

      CN_DRAWITEM: begin
        Message.WParam := 0;
        Message.LParam := 0;
        Message.Result := 1;
        Exit;
      end;

      WM_MOVE:
        if (FCommonData.CommonSkinData.gd[FCommonData.SkinIndex].Props[0].Transparency > 0) or
             ((FCommonData.CommonSkinData.gd[FCommonData.SkinIndex].Props[1].Transparency > 0) and
               ControlIsActive(FCommonData)) then begin
          FCommonData.BGChanged := True;
          Repaint;
        end;

      WM_SETFOCUS, CM_ENTER:
        if not (csDesigning in ComponentState) and Visible then begin
          if Enabled and not (csDestroying in ComponentState) and not bFocusChanging then begin
            Perform(WM_SETREDRAW, 0, 0);
            bFocusChanging := True;
            inherited;
            Perform(WM_SETREDRAW, 1, 0);
            bFocusChanging := False;
            if (SkinData.AnimTimer <> nil) and SkinData.AnimTimer.Enabled then begin
              SkinData.BGChanged := True;
              SkinData.AnimTimer.TimeHandler // Fast repaint
            end
            else
              FCommonData.Invalidate;

            if FCommonData.OuterEffects.Visibility = ovFocused then
              FCommonData.OuterEffects.Invalidate;
          end
          else
            inherited;

          Exit;
        end;

      WM_KILLFOCUS, CM_EXIT:
        if not (csDesigning in ComponentState) and Visible then begin
          if Enabled and not (csDestroying in ComponentState) then begin
            StopTimer(SkinData);
            Perform(WM_SETREDRAW, 0, 0);
            inherited;
            Perform(WM_SETREDRAW, 1, 0);
            if FCommonData.Skinned then begin
              FCommonData.FFocused := False;
              HideGlow(SkinData.GlowID);
              FCommonData.Invalidate(True);
            end;
            if FCommonData.OuterEffects.Visibility = ovFocused then
              FCommonData.OuterEffects.Invalidate;
          end
          else
            inherited;

          Exit
        end;

      CM_FOCUSCHANGED:
        if Visible then begin
          if not bFocusChanging and Focused { Desc 7 } then begin
            Perform(WM_SETREDRAW, 0, 0);
            inherited;
            Perform(WM_SETREDRAW, 1, 0);
            Repaint;
          end
          else
            inherited;

          Exit;
        end; // Blinking removing

      WM_ENABLE:
        Exit; // Blinking removing
    end;
    if CommonWndProc(Message, FCommonData) then
      Exit;

    inherited;
    case Message.Msg of
      CM_CANCELMODE:
        HideGlow(SkinData.GlowID);

      CM_ENABLEDCHANGED:
        if (Visible or (csDesigning in ComponentState)) and not (csDestroying in ComponentState) then begin
          FCommonData.FUpdating := False;
          StopTimer(SkinData);
          FCommonData.BGChanged := True;
          RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
        end;

      CM_VISIBLECHANGED:
        if not (csDestroying in ComponentState) and Visible then begin
          FCommonData.BGChanged := True;
          FCommonData.Updating := False;
          if Visible or (csDesigning in ComponentState) then
            Repaint;
        end;

      WM_SETFONT:
        if Visible or (csDesigning in ComponentState) then begin
          FCommonData.FUpdating := False;
          StopTimer(SkinData);
          FCommonData.Invalidate;
        end;

      CM_ACTIONUPDATE:
        if Action <> nil then
          Enabled := TCustomAction(Action).Enabled;
    end;
  end;
end;


procedure TsBitBtn.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
{$IFNDEF DELPHI_10SYDNEY}
  if not (csLoading in ComponentState) then begin
    if (TCustomAction(Sender).ActionList <> nil) and (Images = nil) then
      FImages := TCustomAction(Sender).ActionList.Images;

    if ImageIndex < 0 then
      FImageIndex := TCustomAction(Sender).ImageIndex;

    if not (csDestroying in ComponentState) then
      FCommonData.Invalidate;
  end;
{$ENDIF}
end;


procedure TsBitBtn.SetTextAlignment(const Value: TAlignment);
begin
  if FTextAlignment <> Value then begin
    FTextAlignment := Value;
    FCommonData.Invalidate
  end;
end;


procedure TsBitBtn.Click;
begin
  if EventEnabled(aeClick, FAnimatEvents) and (FCommonData.SkinManager.AnimEffects.Buttons.ClickEffect = ceWaveOut) then
    DoClickAnimation(SkinData);

  inherited;
end;


procedure TsBitBtn.CNDrawItem(var Message: TWMDrawItem);
begin
  if SkinData <> nil then
    if not SkinData.Skinned then
      StdDrawItem(Message.DrawItemStruct^)
    else
      inherited;
end;


procedure TsBitBtn.CopyImage(DC: HDC; Bmp: TBitmap);
var
//  MemDC,
  SavedDC: hdc;
//  TmpBmp: TBitmap;
begin
  SavedDC := SaveDC(DC);
  try
//    if ControlCount = 0 then begin
      BitBlt(DC, 0, 0, Width, Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
      sVCLUtils.PaintControls(DC, Self, True, MkPoint);
{
    end
    else begin
      TmpBmp := TBitmap.Create;
      CopyBmp(TmpBmp, Bmp);
      BitBlt(DC, 0, 0, Width, Height, TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);
      sVCLUtils.PaintControls(DC, Self, True, MkPoint);
      TmpBmp.Free;
    end;
}
  finally
    RestoreDC(DC, SavedDC);
  end;
end;


procedure TsBitBtn.WMNCHitTest(var Message: TWMNCHitTest);
var
  R: TRect;
begin
  if SkinData.Skinned and (SkinData.CommonSkinData.gd[SkinData.SkinIndex].BorderIndex >= 0) then begin
    GetWindowRect(Handle, R);
    if PtIsTransparent(Message.XPos - R.Left, Message.YPos - R.Top, FCommonData.CommonSkinData.ma[SkinData.CommonSkinData.gd[SkinData.SkinIndex].BorderIndex], Width, Height) then
      Message.Result := HTTRANSPARENT
    else
      inherited;
  end
  else
    inherited;
end;


procedure TsBitBtn.StdDrawItem(const DrawItemStruct: TDrawItemStruct);
var
  R: TRect;
  bWidth, State: integer;
  Bmp: TBitmap;
  Coloring: TacColoring;
  IsDown, IsDefault: Boolean;
begin
  if not (csDestroying in ComponentState) then begin
    CalcButtonLayout(ClientRect, Point(GlyphWidth, GlyphHeight), TextRectSize, Layout, Alignment, Margin, Spacing,
                     GlyphPos, CaptionRect, {$IFDEF FPC}0{$ELSE}DrawTextBiDiModeFlags(0){$ENDIF}, VerticalAlignment);

    if FOffset <> 0 then
      case Layout of
        blGlyphLeft, blGlyphRight: OffsetRect(CaptionRect, FOffset, 0);
        blGlyphTop, blGlyphBottom: OffsetRect(CaptionRect, 0, FOffset);
      end;

    R := ClientRect;
    InitCacheBmp(SkinData);
    FCommonData.FCacheBmp.Canvas.Font.Assign(Font);
    with DrawItemStruct do begin
      IsDefault := Self.Default;
      IsDown := (itemState and ODS_SELECTED <> 0) and not (csDesigning in ComponentState);
      IsFocused := (itemState and ODS_FOCUS <> 0) and not (csDesigning in ComponentState);
    end;

    if IsDown then
      State := 2
    else
      if SkinData.FMouseAbove and not (csDesigning in ComponentState) then
        State := 1
      else
        if IsFocused or IsDefault then
          State := 3
        else
          State := 0;

    acPaintStdBtn(SkinData.FCacheBmp.Canvas, R, Enabled, IsFocused {and IsDefault} and ShowFocus{((SkinData.SkinManager = nil) or SkinData.SkinManager.ButtonsOptions.ShowFocusRect and ShowFocus)}, State);
    GetColoring(Coloring);
    if (FFocusedColor <> clBtnFace) and IsFocused then begin
      bWidth := 2;
      if (Width > 2 * bWidth) and (Width > 2 * bWidth) then begin
        Bmp := CreateBmp32(Width - 2 * bWidth, Height - 2 * bWidth);
        BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, FCommonData.FCacheBmp.Canvas.Handle, bWidth, bWidth, SRCCOPY);
        UpdateBmpColors(Bmp, SkinData, False, State, FFocusedColor);
        BitBlt(FCommonData.FCacheBmp.Canvas.Handle, bWidth, bWidth, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
{$IFDEF DELPHI7UP}
        if not acThemesEnabled then
{$ENDIF}
          FillDCBorder(FCommonData.FCacheBmp.Canvas.Handle, R, 1, 1, 1, 1, clBlack);

        Bmp.Free;
      end;
    end;

    DrawCaption(Coloring, SkinData.FCacheBmp.Canvas);
    if (Coloring.ColorGlyph = clNone) and ((SkinData.SkinManager = nil) or not (SkinData.SkinManager.Effects.DiscoloredGlyphs and (State = 0))) then
      Coloring.ColorGlyph := GetColorTone;

    DrawBtnGlyph(Self, SkinData.FCacheBmp.Canvas, Coloring.ColorGlyph);
    if Assigned(OnPaint) then
      OnPaint(Self, SkinData.FCacheBmp);

    BitBlt(DrawItemStruct.hDC, 0, 0, Width, Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
  end;
end;


procedure TsBitBtn.ImageListChange(Sender: TObject);
begin
  if GetImages <> nil then
    FCommonData.Invalidate;
end;


function TsBitBtn.GetGrayed: boolean;
begin
  Result := FGrayed or SkinData.Skinned and (SkinData.SkinManager <> nil) and SkinData.SkinManager.Effects.DiscoloredGlyphs;
end;


function TsBitBtn.GetImages: TCustomImageList;
begin
{$IFDEF DELPHI_10SYDNEY}
  Result := inherited Images;
{$ELSE}
  Result := FImages;
{$ENDIF}
end;


procedure TsBitBtn.SetVerticalAlignment(const Value: TVerticalAlignment);
begin
  if FVerticalAlignment <> Value then begin
    FVerticalAlignment := Value;
    SkinData.Invalidate;
  end;
end;


procedure TsBitBtn.ShiftControls(Offset: integer);
var
  i: integer;
begin
  if CanClickShift(SkinData.SkinIndex, SkinData.CommonSkinData) then
    for i := 0 to ControlCount - 1 do
      Controls[i].SetBounds(Controls[i].Left + Offset, Controls[i].Top + Offset, Controls[i].Width, Controls[i].Height);
end;

end.
