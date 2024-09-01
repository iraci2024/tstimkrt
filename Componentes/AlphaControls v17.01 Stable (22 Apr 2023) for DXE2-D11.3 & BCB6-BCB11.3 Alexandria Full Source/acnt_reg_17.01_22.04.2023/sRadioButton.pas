unit sRadioButton;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface


uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  {$IFDEF TNTUNICODE} TntControls, TntActnList, TntForms, TntClasses, {$ENDIF}
  {$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  sCommonData, sConst, sDefaults, acAnimation;


type
{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsRadioButton = class(TRadioButton)
{$IFNDEF NOTFORHELP}
  private
    FGlyphChecked,
    FGlyphUnChecked: TBitmap;

    FMargin,
    FTextIndent:     integer;

    FShowFocus:      Boolean;
    FCommonData:     TsCommonData;
    FDisabledKind:   TsDisabledKind;
    FAnimatEvents:   TacAnimatEvents;
    FOnValueChanged: TNotifyEvent;
    FVerticalAlign:  TvaAlign;
    FMouseInGlyph: boolean;
  {$IFNDEF DELPHI7UP}
    FWordWrap: boolean;
    procedure SetWordWrap      (const Value: boolean);
  {$ENDIF}
    procedure SetDisabledKind  (const Value: TsDisabledKind);
    procedure SetTextIndent    (const Value: integer);
    procedure SetShowFocus     (const Value: Boolean);
    procedure SetMargin        (const Value: integer);
    procedure SetReadOnly      (const Value: boolean);
    procedure SetVerticalAlign (const Value: TvaAlign);
    procedure SetGlyph(const Index: Integer; const Value: TBitmap);
  {$IFDEF TNTUNICODE}
    procedure SetCaption       (const Value: TWideCaption);
    procedure SetHint          (const Value: WideString);
    function GetCaption: TWideCaption;
    function GetHint: WideString;
    function IsCaptionStored: Boolean;
    function IsHintStored: Boolean;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
  {$ENDIF}
    procedure SetMouseInGlyph(const Value: boolean);
    function GlyphScreenRect(SkipClickArea: boolean): TRect;
  protected
    FPressed,
    FReadOnly: boolean;
    function GetReadOnly: boolean;
    function CanModify: boolean; virtual;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure SetChecked(Value: Boolean); override;
    procedure PaintHandler(M: TWMPaint);
    procedure StdPaintHandler(M: TWMPaint);
    procedure PaintControl(DC: HDC);
    procedure StdPaintControl(aDC: HDC);
    procedure PaintStdBG;
    function CustomGlyph: boolean;
    procedure DrawCheckText;
    procedure DrawCheckArea;
    procedure DrawSkinGlyph(i: integer);
    function PaintCtrlState: integer;
    procedure PaintGlyph(Bmp: TBitmap; const Index: integer);
    function SkinGlyphWidth (i: integer): integer;
    function SkinGlyphHeight(i: integer): integer;
    function SkinCheckRect  (i: integer): TRect;
    function Glyph: TBitmap;

    function CheckRect: TRect;
    function GlyphWidth: integer;
    function GlyphHeight: integer;

    function GlyphMaskIndex(Checked: boolean): smallint;
    function PrepareCache: boolean;
  {$IFDEF TNTUNICODE}
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
  {$ENDIF}
  public
    function GetControlsAlignment: TAlignment; override;
    procedure AfterConstruction; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Invalidate; override;
    procedure Loaded; override;
    procedure WndProc(var Message: TMessage); override;
    property MouseInGlyph: boolean read FMouseInGlyph write SetMouseInGlyph;
  published
  {$IFDEF TNTUNICODE}
    property Caption: TWideCaption read GetCaption write SetCaption stored IsCaptionStored;
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
  {$ENDIF}
    property AutoSize default True;
{$ENDIF} // NOTFORHELP
    property AnimatEvents: TacAnimatEvents read FAnimatEvents write FAnimatEvents default [aeGlobalDef];
    property SkinData: TsCommonData read FCommonData write FCommonData;
    property DisabledKind: TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
    property GlyphChecked:   TBitmap Index 0 read FGlyphChecked   write SetGlyph;
    property GlyphUnChecked: TBitmap Index 1 read FGlyphUnChecked write SetGlyph;
    property ReadOnly: boolean read GetReadOnly write SetReadOnly default False;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus default True;
    property TextIndent: integer read FTextIndent write SetTextIndent default 2;
  {$IFNDEF DELPHI7UP}
    property WordWrap: boolean read FWordWrap write SetWordWrap default False;
  {$ELSE}
    property WordWrap default False;
  {$ENDIF}
    property VerticalAlign: TvaAlign read FVerticalAlign write SetVerticalAlign default vaMiddle;
    property OnValueChanged: TNotifyEvent read FOnValueChanged write FOnValueChanged;
  {$IFDEF D2007}
    property OnMouseEnter;
    property OnMouseLeave;
  {$ENDIF}
    property Margin: integer read FMargin write SetMargin default 0;
  end;

implementation

uses
  ExtCtrls, Math,
  {$IFDEF CHECKXP}UxTheme, {$ENDIF}
  acGlow, sGraphUtils, acntUtils, sAlphaGraph, sVclUtils, sMaskData, sStylesimply, sSkinProps, sMessages, sSKinManager, acThdTimer;


{$IFDEF TNTUNICODE}
procedure TsRadioButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
  inherited;
end;
{$ENDIF}


procedure TsRadioButton.AfterConstruction;
begin
  inherited;
  SkinData.Loaded;
end;


function TsRadioButton.GetControlsAlignment: TAlignment;
begin
  if not UseRightToLeftAlignment then
    Result := Alignment
  else
    if Alignment = taRightJustify then
      Result := taLeftJustify
    else
      Result := taRightJustify;
end;


function TsRadioButton.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
const
  iFocusRectW = 4;
  iFocusRectH = 4;
var
  ss, cSize: TSize;
  w, h: integer;
begin
  Result := False;
  if not (csLoading in ComponentState) and AutoSize then begin
    ss := GetStringSize(Font.Handle, Caption, DT_WORDBREAK, WordWrap);
    cSize := MkSize(CheckRect);
    Result := True;
    if Caption <> '' then begin
      NewWidth :=     cSize.cx + 2 * Margin + ss.cx + iFocusRectW + ScaleInt(FTextIndent, SkinData);
      NewHeight := Max(cSize.cy, 2 * Margin + ss.cy + iFocusRectH);
    end
    else begin
      NewWidth :=  cSize.cx + 2 * Margin;
      NewHeight := cSize.cy;
    end;
  end
  else
    if not FCommonData.Skinned then begin
      w := NewWidth;
      h := NewHeight;
      Result := inherited CanAutoSize(w, h);
      NewWidth := w;
      NewHeight := h;
    end;
end;


function TsRadioButton.Glyph: TBitmap;
begin
  if Checked then
    Result := GlyphChecked
  else
    Result := GlyphUnChecked;

  if Result.Empty then
    Result := nil;
end;


function TsRadioButton.CheckRect: TRect;
var
  GlyphTop: integer;

  function GetGlyphTop: integer;
  begin
    case FVerticalAlign of
      vaTop:    Result := 0;
      vaMiddle: Result := (Height - GlyphHeight) div 2
      else      Result := (Height - GlyphHeight);
    end;
  end;

begin
  if FGlyphChecked.Width > 0 then begin
    GlyphTop := GetGlyphTop;
    if GetControlsAlignment = taRightJustify then
      Result := Rect(Margin, GlyphTop, Margin + GlyphWidth, GlyphHeight + GlyphTop)
    else
      Result := Rect(Width - GlyphWidth - Margin, GlyphTop, Width - Margin, GlyphHeight + GlyphTop)
  end
  else
    if (FCommonData.SkinManager <> nil) and FCommonData.SkinManager.IsValidImgIndex(FCommonData.CommonSkinData.RadioButton[Checked]) then
      Result := SkinCheckRect(FCommonData.CommonSkinData.RadioButton[Checked])
    else
      Result := MkRect(16, 16);
end;


{$IFDEF TNTUNICODE}
procedure TsRadioButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsWideCharAccel(Message.CharCode, Caption) and CanFocus then begin
      SetFocus;
      Result := 1;
    end
    else
      Broadcast(Message);
end;
{$ENDIF}


constructor TsRadioButton.Create(AOwner: TComponent);
begin
  FCommonData := TsCommonData.Create(Self, False);
  FCommonData.COC := COC_TsRadioButton;
  inherited Create(AOwner);
  FCommonData.FOwnerControl := Self;
  FMargin := 0;
  FShowFocus := True;
  FTextIndent := 2;
  FDisabledKind := DefDisabledKind;
  FGlyphChecked := TBitmap.Create;
  FGlyphUnChecked := TBitmap.Create;
  FPressed := False;
  AutoSize := True;
  FAnimatEvents := [aeGlobalDef];
  FVerticalAlign := vaMiddle;
{$IFNDEF DELPHI7UP}
  FWordWrap := False;
{$ELSE}
  WordWrap := False;
{$ENDIF}
end;


function TsRadioButton.CustomGlyph: boolean;
begin
  Result := Glyph <> nil;
end;


{$IFDEF TNTUNICODE}
procedure TsRadioButton.CreateWindowHandle(const Params: TCreateParams);
begin
  CreateUnicodeHandle(Self, Params, 'BUTTON');
end;


procedure TsRadioButton.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;
{$ENDIF}


destructor TsRadioButton.Destroy;
begin
  FreeAndNil(FCommonData);
  if Assigned(FGlyphChecked) then
    FreeAndNil(FGlyphChecked);

  if Assigned(FGlyphUnchecked) then
    FreeAndNil(FGlyphUnChecked);
    
  inherited Destroy;
end;


procedure TsRadioButton.DrawCheckArea;
var
  GlyphCount, GlyphIndex: integer;
begin
  if Glyph <> nil then begin
    GlyphCount := Glyph.Width div Glyph.Height;
    if FPressed then
      GlyphIndex := min(2, GlyphCount - 1)
    else
      if ControlIsActive(FCommonData) and not ReadOnly then
        GlyphIndex := min(1, GlyphCount - 1)
      else
        GlyphIndex := 0;

    PaintGlyph(Glyph, GlyphIndex);
  end
  else
    with SkinData.SkinManager, SkinData.CommonSkinData do
      if IsValidImgIndex(RadioButton[Checked]) then
        DrawSkinGlyph(RadioButton[Checked]);
end;


procedure TsRadioButton.DrawCheckText;
var
  rText: TRect;
  TranspMode: byte;
  Fmt, State, t, b, w, h, dx: integer;
begin
  if Caption <> '' then begin
    w := Width - (WidthOf(CheckRect) + FTextIndent + 2 * Margin + 2);
    rText := MkRect(w, 0);
    TranspMode := GetTransMode(SkinData);
    Fmt := DT_CALCRECT or TextWrapping[WordWrap] or DT_ALPHATEXT * Cardinal(TranspMode = 2);
    acDrawText(FCommonData.FCacheBMP.Canvas.Handle, Caption, rText, Fmt);
    h := HeightOf(rText);
    dx := WidthOf(rText);
    t := Max((Height - h) div 2, Margin);
    b := t + h;
    Fmt := DT_TOP or DT_ALPHATEXT * Cardinal(TranspMode = 2);
    if Alignment = taRightJustify then
      if IsRightToLeft then begin
        rText.Right := Width - Margin - WidthOf(CheckRect) - FTextIndent - 4;
        rText.Left := rText.Right - dx;
        rText.Top := t;
        rText.Bottom := b;
        if not WordWrap then
          Fmt := DT_RIGHT;
      end
      else
        rText := Rect(Width - w - Margin, t, Width - w - Margin + dx, b)
    else
      rText := Rect(Margin, t, w + Margin, b);

    OffsetRect(rText, -integer(WordWrap), -1);
    Fmt := Fmt or iff(WordWrap, DT_WORDBREAK, DT_SINGLELINE);

    if UseRightToLeftReading then
      Fmt := Fmt or DT_RTLREADING;

    acWriteTextEx(FCommonData.FCacheBmp.Canvas, PacChar(Caption), True, rText, Fmt, FCommonData, ControlIsActive(FCommonData) and not ReadOnly);
    if TranspMode = 1 then
      FillAlphaRect(FCommonData.FCacheBmp, rText);

    FCommonData.FCacheBmp.Canvas.Pen.Style := psClear;
    FCommonData.FCacheBmp.Canvas.Brush.Style := bsSolid;
    if Focused and ShowFocus then begin
      inc(rText.Top);
      InflateRect(rText, 2, 1);
      rText.Left := max(0, rText.Left);
      rText.Top := max(0, rText.Top);
      rText.Right := min(Width, rText.Right);
      rText.Bottom := min(Height, rText.Bottom);
      if SkinData.Skinned then begin
        State := min(ac_MaxPropsIndex, SkinData.CommonSkinData.gd[SkinData.SkinIndex].States);
        FocusRect(SkinData.FCacheBMP.Canvas, rText, SkinData.CommonSkinData.gd[SkinData.SkinIndex].Props[State].FontColor.Color, clNone);
      end
      else
        DrawFocusRect(SkinData.FCacheBMP.Canvas.Handle, rText);
    end;
  end;
end;


procedure TsRadioButton.DrawSkinGlyph(i: integer);
begin
  with FCommonData do
    if FCacheBmp.Width > 0 then
      sAlphaGraph.DrawSkinGlyph(FCacheBmp, SkinCheckRect(i).TopLeft, PaintCtrlState, 1, SkinData.CommonSkinData.ma[i], MakeCacheInfo(FCacheBmp));
end;


{$IFDEF TNTUNICODE}
function TsRadioButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;


function TsRadioButton.GetCaption: TWideCaption;
begin
  Result := TntControl_GetText(Self)
end;


function TsRadioButton.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;
{$ENDIF}


function TsRadioButton.GetReadOnly: boolean;
begin
  Result := FReadOnly;
end;


function TsRadioButton.CanModify: boolean;
begin
  Result := True;
end;


function TsRadioButton.GlyphHeight: integer;
begin
  if Glyph <> nil then
    Result := Glyph.Height
  else
    Result := 16;
end;


function TsRadioButton.GlyphMaskIndex(Checked: boolean): smallint;
begin
  Result := FCommonData.SkinManager.GetMaskIndex(FCommonData.SkinManager.SkinCommonInfo.IndexGLobalInfo, acRadioGlyphs[Checked], FCommonData.CommonSkinData);
end;


function TsRadioButton.GlyphWidth: integer;
begin
  if Glyph <> nil then
    if Glyph.Width mod Glyph.Height = 0 then
      Result := Glyph.Width div (Glyph.Width div Glyph.Height)
    else
      Result := Glyph.Width
  else
    Result := 16;
end;


procedure TsRadioButton.Invalidate;
begin
  inherited;
end;


{$IFDEF TNTUNICODE}
function TsRadioButton.IsCaptionStored: Boolean;
begin
  Result := TntControl_IsCaptionStored(Self);
end;


function TsRadioButton.IsHintStored: Boolean;
begin
  Result := TntControl_IsHintStored(Self)
end;
{$ENDIF}


procedure TsRadioButton.Loaded;
begin
  inherited;
  SkinData.Loaded;
  AdjustSize;
end;


procedure TsRadioButton.PaintControl(DC: HDC);
var
  Timer: TacThreadedTimer;
begin
  {$IFDEF D2006}if not (csRecreating in ControlState) then{$ENDIF} begin
//  if not InUpdating(FCommonData) and not TimerIsActive(SkinData) then begin
//    if not SkinData.BGChanged or PrepareCache then begin
    if not FCommonData.Updating and not TimerIsActive(SkinData) then begin
      if PrepareCache then begin
        UpdateCorners(FCommonData, 0);
        BitBlt(DC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
      end;
    end
    else begin
      if (SkinData.PulsTimer <> nil) and SkinData.PulsTimer.Enabled and (SkinData.PulsTimer.BmpFrom <> nil) then
        Timer := SkinData.PulsTimer
      else
        if (SkinData.AnimTimer <> nil) and SkinData.AnimTimer.Enabled and (SkinData.AnimTimer.BmpFrom <> nil) then
          Timer := SkinData.AnimTimer
        else
          Timer := nil;

      if (Timer <> nil) and (Timer.BmpOut <> nil) then
        BitBlt(DC, 0, 0, Width, Height, Timer.BmpOut.Canvas.Handle, 0, 0, SRCCOPY)
      else
        if TimerIsActive(SkinData) and PrepareCache then
          BitBlt(DC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
    end;
  end;
end;


procedure TsRadioButton.PaintGlyph(Bmp: TBitmap; const Index: integer);
var
  R: TRect;
begin
  if FCommonData.FCacheBmp.Width <> 0 then begin
    R := CheckRect;
    if Bmp.PixelFormat = pfDevice then
      Bmp.HandleType := bmDIB;

    if Bmp.PixelFormat <> pf32bit then begin
      Bmp.PixelFormat := pf32bit;
      FillAlphaRect(Bmp, MkRect(Bmp), MaxByte);
    end;
    CopyByMask(R, Rect(GlyphWidth * Index, 0, GlyphWidth * (Index + 1), GlyphHeight), FCommonData.FCacheBmp, Bmp, EmptyCI, True);
  end;
end;


procedure TsRadioButton.PaintHandler(M: TWMPaint);
var
  PS: TPaintStruct;
  SavedDC: hdc;
  DC: hdc;
begin
  if M.DC = 0 then begin
    BeginPaint(Handle, PS);
    DC := GetDC(Handle);
  end
  else
    DC := M.DC;

  SavedDC := SaveDC(DC);
  try
    PaintControl(DC);
  finally
    RestoreDC(DC, SavedDC);
    if M.DC = 0 then begin
      ReleaseDC(Handle, DC);
      EndPaint(Handle, PS);
    end;
  end;
end;


procedure TsRadioButton.PaintStdBG;
begin
  FillDC(FCOmmonData.FCacheBmp.Canvas.Handle, MkRect(Self), Color);
end;


function TsRadioButton.PrepareCache: boolean;
var
  BGInfo: TacBGInfo;
begin
{$IFDEF D2006}
// Desc 6  if csRecreating in ControlState then begin
//    Result := False;
//    SkinData.FUpdating := True;
//  end
//  else
{$ENDIF}
  begin
    Result := True;
    InitCacheBmp(SkinData);
    FCommonData.FCacheBmp.Canvas.Font.Assign(Font);
    FCommonData.FCacheBmp.Canvas.Lock;
    BGInfo.PleaseDraw := False;
    BGInfo.Offset := Point(Left, Top);
    BGInfo.R := MkRect(Width, Height);
    GetBGInfo(@BGInfo, Parent);
    case BGInfo.BgType of
      btUnknown: begin // If parent is not AlphaControl
        BGInfo.Bmp := FCommonData.FCacheBmp;
        BGInfo.BgType := btCache;
      end;
      btNotReady: begin
        Result := False;
        SkinData.FUpdating := True;
        Exit;
      end;
    end;
    FCommonData.FCacheBmp.Canvas.Unlock;
    if SkinData.Skinned then
      PaintItem(FCommonData, BGInfoToCI(@BGInfo), True, PaintCtrlState, MkRect(FCommonData.FCacheBmp.Width, Height), Point(Left, Top), FCommonData.FCacheBmp, True)
    else
      PaintStdBG;

    DrawCheckText;
    DrawCheckArea;
    if not Enabled then
      BmpDisabledKind(FCommonData.FCacheBmp, FDisabledKind, Parent, BGInfoToCI(@BGInfo), Point(Left, Top));

    FCommonData.BGChanged := False
  end;
end;


{$IFDEF TNTUNICODE}
procedure TsRadioButton.SetCaption(const Value: TWideCaption);
begin
  TntControl_SetText(Self, Value);
end;
{$ENDIF}


{$IFNDEF DELPHI7UP}
procedure TsRadioButton.SetWordWrap(const Value: boolean);
begin
  if FWordWrap <> Value then begin
    FWordWrap := Value;
    FCommonData.BGChanged := True;
    if AutoSize then
      AutoSize := False;

    Repaint;
  end;
end;
{$ENDIF}


procedure TsRadioButton.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    FCommonData.Invalidate;
  end;
end;


procedure TsRadioButton.SetGlyph(const Index: Integer; const Value: TBitmap);

  procedure ChangeProp(var Prop: TBitmap; Value: TBitmap);
  begin
    if Prop <> Value then begin
      Prop := Value;
      if AutoSize then
        AdjustSize;

      if Checked then
        FCommonData.Invalidate;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FGlyphChecked, Value);
    1: ChangeProp(FGlyphUnChecked, Value);
  end;
end;


{$IFDEF TNTUNICODE}
procedure TsRadioButton.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;
{$ENDIF}


procedure TsRadioButton.SetMouseInGlyph(const Value: boolean);
begin
  if FMouseInGlyph <> Value then begin
    FMouseInGlyph := Value;
    if Value then begin
      if (SkinData.CircleID < 0) and SkinData.Skinned and CircleClickMode(Self, SkinData, AnimatEvents) then
        ShowCircleIfNeeded(SkinData, MaxByte, True {If possible});
    end
    else
      HideCircle(SkinData.CircleID);
  end;
end;


procedure TsRadioButton.SetMargin(const Value: integer);
begin
  if FMargin <> Value then begin
    FMargin := Value;
    if AutoSize then
      AdjustSize;

    Invalidate;
  end;
end;


procedure TsRadioButton.SetReadOnly(const Value: boolean);
begin
  FReadOnly := Value;
end;


procedure TsRadioButton.SetShowFocus(const Value: Boolean);
begin
  if FShowFocus <> Value then begin
    FShowFocus := Value;
    Invalidate;
  end;
end;


procedure TsRadioButton.SetTextIndent(const Value: integer);
begin
  if FTextIndent <> Value then begin
    FTextIndent := Value;
    if AutoSize then
      AdjustSize;

    SkinData.Invalidate;
  end;
end;


function TsRadioButton.GlyphScreenRect(SkipClickArea: boolean): TRect;
var
  i: integer;
begin
  Result := CheckRect;
  if not SkipClickArea and (SkinData.SkinManager <> nil) and (SkinData.SkinManager.AnimEffects.Buttons.ClickEffect <> ceWaveOut) then begin
    i := MaxCircleRadius(SkinData) - WidthOf(Result) div 2;
    InflateRect(Result, i, i);
  end;
  with ClientToScreen(MkPoint) do
    OffsetRect(Result, X, Y);
end;


function TsRadioButton.SkinCheckRect(i: integer): TRect;
var
  h, w, hdiv: integer;
begin
  h := SkinGlyphHeight(i);
  w := SkinGlyphWidth(i);
  case FVerticalAlign of
    vaTop:    hdiv := 0;
    vaMiddle: hdiv := (Height - h) div 2
    else      hdiv := (Height - h);
  end;
  if GetControlsAlignment = taRightJustify then
    Result := Rect(0, hdiv, Margin + w, h + hdiv)
  else
    Result := Rect(Width - w - Margin, hdiv, Width, h + hdiv);
end;


function TsRadioButton.SkinGlyphHeight(i: integer): integer;
begin
  Result := FCommonData.CommonSkinData.ma[i].Height;
end;


function TsRadioButton.SkinGlyphWidth(i: integer): integer;
begin
  Result := FCommonData.CommonSkinData.ma[i].Width;
end;


procedure TsRadioButton.StdPaintControl(aDC: HDC);
begin
  PrepareCache;
  BitBlt(aDC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
end;


procedure TsRadioButton.StdPaintHandler(M: TWMPaint);
var
  PS: TPaintStruct;
  DC, SavedDC: hdc;
begin
  DC := M.DC;
  if (DC = 0) or (SkinData.CtrlSkinState and ACS_PRINTING = 0) then
    DC := BeginPaint(Handle, PS);

  SavedDC := SaveDC(DC);
  try
//    if not InUpdating(FCommonData) then
    StdPaintControl(DC);
  finally
    RestoreDC(DC, SavedDC);
    if (M.DC = 0) or (SkinData.CtrlSkinState and ACS_PRINTING = 0) then
      EndPaint(Handle, PS);
  end;
end;


procedure TsRadioButton.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_CTRLHANDLED: begin
          Message.Result := 1;
          Exit;
        end; // AlphaSkins supported

        AC_GETAPPLICATION: begin
          Message.Result := LRESULT(Application);
          Exit;
        end;

        AC_REMOVESKIN:
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
            CommonWndProc(Message, FCommonData);
            if HandleAllocated then
              SendMessage(Handle, BM_SETCHECK, Integer(Checked), 0);

            if not (csDesigning in ComponentState) and (uxthemeLib <> 0) then
              Ac_SetWindowTheme(Handle, nil, nil);

            Repaint;
            Exit;
          end;

        AC_REFRESH:
          if RefreshNeeded(SkinData, Message) then begin
            SetClassLong(Handle, GCL_STYLE, GetClassLong(Handle, GCL_STYLE) and not CS_VREDRAW and not CS_HREDRAW);
            CommonWndProc(Message, FCommonData);
            AdjustSize;
            Repaint;
            Exit;
          end;

        AC_PREPARECACHE: begin
          PrepareCache;
          Exit;
        end;

        AC_GETMOUSEAREA: begin
          PRect(Message.LParam)^ := GlyphScreenRect(boolean(Message.WParamLo));
          Message.Result := 1;
          Exit;
        end;

        AC_GETDEFSECTION: begin
          Message.Result := 4 + 1;
          Exit;
        end;

        AC_GETDEFINDEX: begin
          if FCommonData.SkinManager <> nil then
            Message.Result := FCommonData.SkinManager.SkinCommonInfo.Sections[ssCheckBox] + 1;

          Exit;
        end;

        AC_SETNEWSKIN:
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
            CommonWndProc(Message, FCommonData);
            Exit;
          end;

        AC_SETGLASSMODE: begin
          CommonMessage(Message, FCommonData);
          Exit;
        end;
      end;

      CM_FONTCHANGED, CM_TEXTCHANGED:
        if AutoSize then begin
          if ([csDestroying, csLoading] * ComponentState = []) and (Parent <> nil) then
            HandleNeeded;

          if HandleAllocated then begin
            FCommonData.BGChanged := True;
            inherited;
            AdjustSize;
            Exit;
          end;
        end;
    end;

  if (FCommonData <> nil) and FCommonData.Skinned(True) then
    case Message.Msg of
      CM_MOUSEENTER:
        if Enabled and not (csDesigning in ComponentState) and Visible and not FCommonData.FMouseAbove then begin
          FCommonData.FMouseAbove := True;
          if CircleClickMode(Self, SkinData, AnimatEvents) then begin
            MouseInGlyph := PtInRect(GlyphScreenRect(False), acMousePos);
            ShowCircleIfNeeded(SkinData, MaxByte, True);
          end
          else
            DoChangePaint(FCommonData, 1, UpdateWindow_CB, EventEnabled(aeMouseEnter, FAnimatEvents), False);
        end;

      CM_MOUSELEAVE:
        if Enabled and not (csDesigning in ComponentState) and Visible and FCommonData.FMouseAbove then begin
          FCommonData.FMouseAbove := False;
          FPressed := False;
          if CircleClickMode(Self, SkinData, AnimatEvents) then
            MouseInGlyph := False
          else
            DoChangePaint(FCommonData, 0, UpdateWindow_CB, EventEnabled(aeMouseLeave, FAnimatEvents), False, False);

          inherited;
          Exit;
        end;

      WM_SETFOCUS, CM_ENTER:
        if not (csDesigning in ComponentState) then begin
          if Enabled then begin
            inherited;
            FCommonData.BGChanged := True;
            if (SkinData.AnimTimer <> nil) and SkinData.AnimTimer.Enabled then begin
              SkinData.BGChanged := True;
              SkinData.AnimTimer.TimeHandler; // Fast repaint
            end
            else
              Repaint
          end;
          Exit;
        end;

      WM_KILLFOCUS, CM_EXIT:
        if ComponentState * [csDesigning, csDestroying] = [] then
          if Enabled then begin
            if not CircleClickMode(Self, SkinData, AnimatEvents) then
              StopTimer(SkinData);

            Perform(WM_SETREDRAW, 0, 0);
            inherited;
            Perform(WM_SETREDRAW, 1, 0);
            FCommonData.FFocused := False;
            FCommonData.FMouseAbove := False;
            FCommonData.BGChanged := True;
            PrepareCache;
            RedrawWindow(Handle, nil, 0, RDWA_ALLNOW);
            Exit;
          end;
    end;

  if not ControlIsReady(Self) then
    inherited
  else begin
    if CommonWndProc(Message, FCommonData) then
      Exit;

    if FCommonData.Skinned(True) then
      case Message.Msg of
        SM_ALPHACMD:
          case Message.WParamHi of
            AC_ENDPARENTUPDATE:
              if FCommonData.Updating or FCommonData.HalfVisible then begin
                FCommonData.Updating := False;
                PaintHandler(TWMPaint(MakeMessage(WM_PAINT, 0, 0, 0)));
              end;
          end;

        WM_ENABLE, WM_NCPAINT:
          Exit; // Disabling of blinking when switched
{$IFDEF CHECKXP}
        WM_UPDATEUISTATE: begin
          if SkinData.Skinned and UseThemes and not (csDesigning in ComponentState) and (uxthemeLib <> 0) then
            Ac_SetWindowTheme(Handle, s_Space, s_Space);

          Exit;
        end;
{$ENDIF}
        CM_ENABLEDCHANGED: begin
          inherited;
          Repaint;
          Exit;
        end;

        WM_MOUSEMOVE:
          if CircleClickMode(Self, SkinData, AnimatEvents) then
            MouseInGlyph := PtInRect(GlyphScreenRect(False), acMousePos);

        BM_SETSTATE:
          Exit;

        CN_COMMAND:
          if not ((csCreating in ControlState) or (csLoading in ComponentState)) and ReadOnly then
            Exit;

        CM_FONTCHANGED, CM_TEXTCHANGED:
          SkinData.Invalidate;

        BM_SETCHECK: if IsWindowVisible(Handle) then begin
          Perform(WM_SETREDRAW, 0, 0);
          inherited;
          Perform(WM_SETREDRAW, 1, 0);
          if not CircleClickMode(Self, SkinData, AnimatEvents) then begin // ???
            FCommonData.BGChanged := True;
            if not TimerIsActive(SkinData) then begin
              StopTimer(SkinData);
              Repaint;
            end;
            Checked := Message.WParam = 1;
            StopTimer(SkinData);
          end
          else begin
            Checked := Message.WParam = 1;
            FCommonData.BGChanged := True;
            SkinData.FUpdating := False;
            PrepareCache;
            Repaint;
          end;
          if not FPressed then
            FCommonData.Invalidate;

          Exit;
        end;

        WM_ERASEBKGND: begin
          if (TWMPaint(Message).DC <> 0) and (Skindata.FCacheBmp <> nil) and not FCommonData.BGChanged and not TimerIsActive(SkinData) then
            CopyWinControlCache(Self, FCommonData, MkRect, MkRect(Self), TWMPaint(Message).DC, False);

          Message.Result := 1;
          Exit;
        end;

        WM_PRINT: begin
          SkinData.Updating := False;
          PaintHandler(TWMPaint(Message));
        end;

        WM_PAINT: begin
          PaintHandler(TWMPaint(Message));
//          if not (csDesigning in ComponentState) then
            Exit;
        end;

        WM_KEYDOWN:
          if Enabled and not (csDesigning in ComponentState) and (TWMKey(Message).CharCode = VK_SPACE) then begin
            if not ReadOnly then begin
              FPressed := True;
              if not Focused then begin
                ClicksDisabled := True;
                Windows.SetFocus(Handle);
                ClicksDisabled := False;
              end;
              Repaint;
              if Assigned(OnKeyDown) then
                OnKeydown(Self, TWMKeyDown(Message).CharCode, KeysToShiftState(word(TWMKeyDown(Message).KeyData)));
            end;
            Exit;
          end;

        WM_LBUTTONDBLCLK, WM_LBUTTONDOWN:
          if not (csDesigning in ComponentState) and Enabled and Visible and (DragMode = dmManual) then begin
            if not ReadOnly then begin
              FPressed := True;
              ControlState := ControlState + [csLButtonDown];
              if CircleClickMode(Self, SkinData, AnimatEvents) then begin
                if not SkinIsChanging(SkinData) and not FadingForbidden and SkinData.SkinManager.Effects.AllowAnimation then // Animated
                  ShowCircleIfNeeded(SkinData, MaxByte, True)
                else // Without animation
                  if SkinData.FMouseAbove then
                    ShowCircleIfNeeded(SkinData)
                  else
                    if not SkinData.FMouseAbove then
                      HideCircle(SkinData.CircleID);
              end;
              DoChangePaint(FCommonData, 2, UpdateWindow_CB, EventEnabled(aeMouseDown, FAnimatEvents), False);

              if not Focused then begin
                ClicksDisabled := True;
                Windows.SetFocus(Handle);
                ClicksDisabled := False;
              end;
              if WM_LBUTTONDBLCLK = Message.Msg then begin
                if Assigned(OnDblClick) then
                  OnDblClick(Self)
              end
              else
                if Assigned(OnMouseDown) then
                  OnMouseDown(Self, mbLeft, KeysToShiftState(TWMMouse(Message).Keys), TWMMouse(Message).XPos, TWMMouse(Message).YPos);
            end;
            Exit;
          end;

        WM_KEYUP:
          if Enabled and not (csDesigning in ComponentState) and (TWMKey(Message).CharCode = VK_SPACE) then begin
            if not ReadOnly then begin
              if FPressed then begin
                FPressed := False;
                Checked := True;
              end;
              Repaint;
              if Assigned(OnKeyUp) then
                OnKeyUp(Self, TWMKey(Message).CharCode, KeysToShiftState(TWMKey(Message).KeyData));
            end;
            Exit;
          end;

        WM_LBUTTONUP:
          if not (csDesigning in ComponentState) and Enabled then begin
            if not ReadOnly and CanModify and FPressed then begin
              ControlState := ControlState - [csLButtonDown];
              if CircleClickMode(Self, SkinData, AnimatEvents) then begin
                if not SkinIsChanging(SkinData) and not FadingForbidden and SkinData.SkinManager.Effects.AllowAnimation then // Animated
                  ShowCircleIfNeeded(SkinData, MaxByte, True)
                else // Without animation
                  if SkinData.FMouseAbove then
                    ShowCircleIfNeeded(SkinData)
                  else
                    if not SkinData.FMouseAbove then
                      HideCircle(SkinData.CircleID);
              end
              else
                if EventEnabled(aeClick, FAnimatEvents) then
                  if PtInRect(GlyphScreenRect(False), acMousePos) then
                    DoClickAnimation(SkinData);

              Checked := True;
              Perform(CN_COMMAND, MakeWParam(BN_CLICKED, 0), Handle);
            end;
            FPressed := False;
            if not CircleClickMode(Self, SkinData, AnimatEvents) then
              StopTimer(SkinData);

            Repaint;
            if Assigned(OnMouseUp) then
              OnMouseUp(Self, mbLeft, KeysToShiftState(TWMMouse(Message).Keys), TWMMouse(Message).XPos, TWMMouse(Message).YPos);

            Exit;
          end;
      end
    else
      case Message.Msg of
        WM_PAINT:
          if CustomGlyph and IsWindowVisible(Handle) then begin
            StdPaintHandler(TWMPaint(Message));
//            if not (csDesigning in ComponentState) then
              Exit;
          end;

        CM_FONTCHANGED, CM_TEXTCHANGED: begin
          inherited;
          if HandleAllocated then begin
            if AutoSize then
              AdjustSize;

            Repaint;
          end;
          Exit;
        end;

        WM_KEYDOWN, WM_LBUTTONDOWN:
          FPressed := True;

        WM_KEYUP, WM_LBUTTONUP:
          FPressed := False;

        WM_LBUTTONDBLCLK:
          if ReadOnly then
            Exit;

        BM_SETSTATE, BM_SETCHECK:
          if not ((csCreating in ControlState) or (csLoading in ComponentState)) and FPressed and ReadOnly then
            Exit;

        CN_COMMAND:
          if not ((csCreating in ControlState) or (csLoading in ComponentState)) and ReadOnly then
            Exit;
      end;

    inherited;
  end;
end;


procedure TsRadioButton.SetVerticalAlign(const Value: TvaAlign);
begin
  if FVerticalAlign <> Value then begin
    FVerticalAlign := Value;
    Repaint;
  end;
end;


procedure TsRadioButton.SetChecked(Value: Boolean);
var
  CurValue:Boolean;
begin
  SkinData.BGChanged := True;
  CurValue := Checked;
  inherited;
  if CurValue <> Checked then begin
    RestartAttention(SkinData);
    if Assigned(FOnValueChanged) then
      FOnValueChanged(self);
  end;
end;


function TsRadioButton.PaintCtrlState: integer;
begin
  if FPressed then
    Result := 2
  else
    Result := integer(ControlIsActive(FCommonData) and not ReadOnly);
end;

end.
