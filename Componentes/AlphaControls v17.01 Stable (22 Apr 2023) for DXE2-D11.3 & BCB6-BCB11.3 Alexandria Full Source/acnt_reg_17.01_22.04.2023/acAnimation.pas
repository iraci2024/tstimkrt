unit acAnimation;
{$I sDefs.inc}
//{$DEFINE LOGGED}
//{$DEFINE ACDEBUG}

interface
{$R+}

uses
  Windows, SysUtils, Classes, ExtCtrls, Graphics, Controls,
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  sCommonData, sConst, acThdTimer, sSkinManager, acntTypes;

type
  TacAttentionMode = (amColor, amVibration, amWavering);
{$IFNDEF NOTFORHELP}
  TacClickMode = (cm1, cm2);
  PacAttentionData = ^TacAttentionData;

  TacIterationData = record
    OutRect: TRect;
    Ctrl: TControl;
    BmpTo,
    BmpOut: TBitmap;
    Blend: integer;
  end;

  TacAnimIterationProc = procedure(var Data: TacIterationData);

  TacMoveTextData = record
    BoundLabel: TsBoundLabel;
    Flags: Cardinal;
    Text: acString;
    Ctrl: TControl;
    Color: TColor;
    Font: TFont;
    R: TRect;
  end;
{$ENDIF}

  TacAttentionData = record
    Color: TColor;
    Interval: integer;
    Mode: TacAttentionMode;
  end;

procedure DoClickAnimation(SkinData: TsCommonData; AMode: TacClickMode = cm1);
procedure StartAttention(Ctrl: TControl; Data: TacAttentionData);
procedure StopAttention(Ctrl: TControl); overload;
procedure StopAttention(SkinData: TsCommonData); overload;
procedure RestartAttention(SkinData: TsCommonData);

procedure PrepareForAnimation(const Ctrl: TWinControl; AnimType: TacAnimTypeCtrl = atcFade);
procedure AnimShowControl(Ctrl: TWinControl; wTime: word = 0; MaxTransparency: integer = MaxByte; AnimType: TacAnimTypeCtrl = atcFade; DoIteration: TacAnimIterationProc = nil);

// Lighting animation
procedure SetMouseLight(Active: boolean);
procedure AddLightControl(SkinData: TsCtrlSkinData);
procedure DelLightControl(SkinData: TsCtrlSkinData);
function LightingIsNear(SkinData: TsCtrlSkinData): boolean;

// Service functions
{$IFNDEF NOTFORHELP}
procedure HandleLighting(SkinData: TsCtrlSkinData; State: integer; ParentCI: PCacheInfo = nil; DstBmp: TBitmap = nil);
procedure DoUpdateLight(SkinData: TsCtrlSkinData; DoOutput: boolean);

procedure AnimMoveText(srcData, dstData: TacMoveTextData);

function EventEnabled(Event: TacAnimatEvent; CurrentProperty: TacAnimatEvents; SkinManager: TsSkinManager = nil): boolean;

procedure MakeLabelTextHint(SkinData: TsCommonData; BoundLabel: TsBoundLabel; TextHint: acString);
procedure DoChangePaint(Data: TsCommonData; State: integer; AAnimProc: TacAnimProc; AllowAnimation: boolean; Fast: boolean; MakeGlowing: boolean = True);
procedure AnimChange(SkinData: TsCommonData; NewState: integer; AAnimProc: TacAnimProc; Fast: boolean);
function UpdateGlowing_CB  (Data: TObject; iIteration: integer): boolean;
function UpdateGraphic_CB  (Data: TObject; iIteration: integer): boolean;
function UpdateDiagonal_CB (Data: TObject; iIteration: integer): boolean; // Diagonal anim function (for checkboxes)
function UpdateWindow_CB   (Data: TObject; iIteration: integer): boolean; // Common anim function (fading)
function UpdateComboAdv_CB (Data: TObject; iIteration: integer): boolean; // ComboEdits anim function
function UpdateFillLight   (Data: TObject; iIteration: integer): boolean;
{$ENDIF}


implementation

uses
  {$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  Forms, StdCtrls, Messages, math,
  {$IFNDEF ALITE}sCustomComboEdit, {$ENDIF}
  sSkinProvider, sBitBtn, sButton, sSpeedButton, sComboBox, sComboBoxes, sMessages, sStyleSimply, sVCLUtils, sGraphUtils, acGlow, acntUtils, sAlphaGraph,
  sCheckBox, sLabel, acgpUtils, sGradient;


const
  iIntervalBig = 500;
  iIntervalSmall = 20;
  iBlurOffs = 4; // Offset for blur

type
  TacMouseLightData = class(TPersistent)
  public
    MaskBmp: TBitmap;
    LightTimer: TTimer;
    InOnTimer: boolean;
    LightHandles,
    LightControls: TList;
    MouseLightSize: integer;

    constructor Create;
    procedure UpdateMask;
    procedure UpdateScale;
    destructor Destroy; override;
    procedure OnLightTimer(Sender: TObject);
  end;


  TacClickTimer = class(TacThreadedTimer)
  public
    Alpha: byte;
    Mode: TacClickMode;
    AForm: TacGlowForm;
    procedure IterateImage;
    destructor Destroy; override;
    procedure PrepareImage; override;
    procedure DisableTimer; override;
  end;


  TacPulsTimer = class(TacThreadedTimer)
  public
    Direct: integer;
    AForm: TacGlowForm;
    Busy, Shown: boolean;
    SkinData: TsCommonData;
    PaintData: TacAttentionData;
    procedure Init;
    destructor Destroy; override;
    procedure PrepareImage; override;
    procedure DisableTimer; override;
  end;

var
  acMouseLightData: TacMouseLightData = nil;


destructor TacClickTimer.Destroy;
begin
  FreeAndNil(AForm);
  inherited;
end;


procedure TacClickTimer.DisableTimer;
begin
  inherited;
  if not Destroyed then
    FreeAndNil(AForm);
end;


procedure TacClickTimer.IterateImage;
const
  AddSize = 2;
  AlphaMulFactor = 8;
  AlphaDivFactor = 9;
var
  Wnd: HWND;
  FBmpSize: TSize;
  ActBounds: TRect;
  Form: TCustomForm;
  TmpBmp, NewBmp: TBitmap;
begin
  // Current size of layered form
  FBmpSize.cx := WidthOf(AnimRect, True);
  FBmpSize.cy := HeightOf(AnimRect, True);
  // Actual position of a control
  if (AnimControl is TWinControl) and not IsWindowVisible(TWinControl(AnimControl).Handle) then
        Exit;

  if not (AnimControl is TWinControl) or (TrySendMessage(TWinControl(AnimControl).Handle, SM_ALPHACMD, MakeWParam(1, AC_GETMOUSEAREA), LParam(@ActBounds)) = 0) then
    ActBounds := GetCtrlScreenBounds(AnimControl);
  // New size for stretch
  inc(FBmpSize.cx, AddSize);
  inc(FBmpSize.cy, AddSize);
  // Make stretched image
  TmpBmp := CreateBmp32(FBmpSize);
  Stretch(BmpOut, TmpBmp, FBmpSize.cx, FBmpSize.cy, ftMitchell);
  // Prepare image for bluring
{$IFNDEF WIN64}
  if Mode = cm1 then begin
    FBmpSize.cx := FBmpSize.cx + iBlurOffs + 1;
    FBmpSize.cy := FBmpSize.cy + iBlurOffs + 1;
  end;
{$ENDIF}
  NewBmp := CreateBmp32(FBmpSize);
  FillRect32(NewBmp, MkRect(NewBmp), 0, 0);
{$IFNDEF WIN64}
  if Mode = cm1 then
    BitBlt(NewBmp.Canvas.Handle, 1, 1, TmpBmp.Width, TmpBmp.Height, TmpBmp.Canvas.Handle, 0, 0, SRCCOPY)
  else
{$ENDIF}
    BitBlt(NewBmp.Canvas.Handle, 0, 0, TmpBmp.Width, TmpBmp.Height, TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);

  TmpBmp.Free;

  BmpOut.Free;
  BmpOut := NewBmp;
  // Make blured image
  if Mode = cm1 then
    QBlur(BmpOut);

  // New bounds rect
  AnimRect.Left := ActBounds.Left - (FBmpSize.cx - WidthOf(ActBounds)) div 2;
  AnimRect.Top := ActBounds.Top - (FBmpSize.cy - HeightOf(ActBounds)) div 2;
  AnimRect.Right := AnimRect.Left + FBmpSize.cx;
  AnimRect.Bottom := AnimRect.Top + FBmpSize.cy;

  // New transparency
  Alpha := Alpha * 5 div 6;//AlphaMulFactor div AlphaDivFactor;

  Form := GetParentForm(AnimControl);
  if (Form = nil) or (GetWindowLong(Form.Handle, GWL_EXSTYLE) and WS_EX_TOPMOST <> 0) then
    Wnd := HWND_TOPMOST
  else
    Wnd := GetNextWindow(Form.Handle, GW_HWNDPREV);

  // Update and showing layered form
  if AForm = nil then begin
    AForm := TacGlowForm.CreateNew(nil);
    AForm.HandleNeeded;
    SetWindowLong(AForm.Handle, GWL_EXSTYLE, GetWindowLong(AForm.Handle, GWL_EXSTYLE) or WS_EX_TRANSPARENT);
  end;
  if AForm.HandleAllocated then begin
    SetFormBlendValue(AForm.Handle, BmpOut, Alpha, @(AnimRect.TopLeft));
    SetWindowPos(AForm.Handle, Wnd, AnimRect.Left, AnimRect.Top,
      0, 0, SWP_SHOWWINDOW or SWP_NOREDRAW or SWP_NOACTIVATE or SWP_NOOWNERZORDER or SWP_NOSENDCHANGING or SWP_NOSIZE {or SWP_NOZORDER {Preventing of windows positions changing});
  end;
end;


procedure TacClickTimer.PrepareImage;
var
  ScrDC: hdc;
  w, h: integer;
  BGBmp: TBitmap;
  BGInfo: TacBGInfo;
begin
  Alpha := MaxByte;
  // Bounds rect of control on the screen
  if not (AnimControl is TWinControl) or (TrySendMessage(TWinControl(AnimControl).Handle, SM_ALPHACMD, MakeWParam(1 {SkipClickArea}, AC_GETMOUSEAREA), LParam(@AnimRect)) = 0) then
    AnimRect := GetCtrlScreenBounds(AnimControl);

  w := WidthOf(AnimRect);
  h := HeightOf(AnimRect);

  if BmpOut = nil then
    BmpOut := CreateBmp32(w, h)
  else begin
    BmpOut.Width := w;
    BmpOut.Height := h;
  end;

  // Receive image of control from screen
  ScrDC := GetDC(0);
  try
    BitBlt(BmpOut.Canvas.Handle, 0, 0, w, h, ScrDC, AnimRect.Left, AnimRect.Top, SRCCOPY);
  finally
    ReleaseDC(0, ScrDC);
  end;
  // Receive BG image
  BGBmp := CreateBmpLike(BmpOut);
  BGInfo.DrawDC := BGBmp.Canvas.Handle;
  BGInfo.R := MkRect(w, h);
  BGInfo.Offset := Point(AnimControl.Left, AnimControl.Top);
  if Mode = cm2 then
    dec(BGInfo.Offset.X, 2);

  BGInfo.BgType := btUnknown;
  BGBmp.Canvas.Lock;
  GetBGInfo(@BGInfo, AnimControl.Parent.Handle, True);
  BGBmp.Canvas.Unlock;
  // Receive difference between 2 bitmaps
  DiffBmp32(BmpOut, BGBmp);
  BGBmp.Free;
end;


procedure AddLightControl(SkinData: TsCtrlSkinData);
begin
  if SkinData.FOwnerControl <> nil then begin
    if acMouseLightData.LightControls.IndexOf(SkinData.FOwnerControl) < 0 then
      acMouseLightData.LightControls.Add(SkinData.FOwnerControl);
  end
  else
    if acMouseLightData.LightHandles.IndexOf(@SkinData.FSWHandle) < 0 then
      acMouseLightData.LightHandles.Add(@SkinData.FSWHandle);
end;


procedure DelLightControl(SkinData: TsCtrlSkinData);
begin
  if acMouseLightData <> nil then
    if SkinData = nil then
      acMouseLightData.LightControls.Clear
    else
      if SkinData.FOwnerControl <> nil then
        acMouseLightData.LightControls.Remove(SkinData.FOwnerControl)
      else
        acMouseLightData.LightHandles.Remove(@SkinData.FSWHandle);
end;


procedure SetMouseLight(Active: boolean);
begin
  if (DefaultManager = nil) or (csDesigning in DefaultManager.ComponentState) then
    acMouseLightData.LightTimer.Enabled := False
  else begin
    acMouseLightData.LightTimer.Enabled := Active;
    if not Active then
      acMouseLightData.OnLightTimer(nil);
  end;
end;


procedure HandleLighting(SkinData: TsCtrlSkinData; State: integer; ParentCI: PCacheInfo = nil; DstBmp: TBitmap = nil);
var
  R: TRect;
  Bmp: TBitmap;
  bInW, bInH: boolean;
  w, h, L1, L2, X, Y, A, cx, cy: integer;
begin
  if SkinData.DoLighting and (SkinData.SkinIndex > 0) then begin
    if (SkinData.FOwnerControl <> nil) and not (csDesigning in SkinData.FOwnerControl.ComponentState) then begin
      cx := SkinData.FOwnerControl.Width;
      w := cx;
      cy := SkinData.FOwnerControl.Height;
      h := cy;
      case SkinData.FOwnerControl.Align of
        alClient: begin
          h := min(w, h);
          w := h;
        end;
        alTop, alBottom: h := w;
        alLeft, alRight: w := h;
      end;
    end
    else begin
      GetWindowRect(SkinData.FSWHandle, R);
      w := WidthOf(R);
      h := HeightOf(R);
      cx := w;
      cy := h;
    end;
    if (w <> 0) and (h <> 0) then begin
      PrepareLightMask(SkinData.LightMask, w, h, GetLightColor(SkinData, State));//, SkinData.TranspMode = 2 {Transparent});
      // Light within the control coords?
      bInW := abs(SkinData.LightDistanceX) < cx div 2;
      bInH := abs(SkinData.LightDistanceY) < cy div 2;
      // Distance between control and light image
      X := cx div 2 - SkinData.LightDistanceX;
      Y := cy div 2 - SkinData.LightDistanceY;
      // Relative coord of light bitmap center
      R.Left := X - SkinData.LightMask.Width div 2;
      R.Top := Y - SkinData.LightMask.Height div 2;
      // Calc source transparency
      if bInW and bInH then
        // Cursor is within control
        A := MaxByte
      else begin
        X := abs(X - cx div 2);
        Y := abs(Y - cy div 2);
        L1 := Round(Sqrt(Sqr(X) + Sqr(Y)));
        L2 := Round(Sqrt(Sqr(SkinData.LightDistanceX) + Sqr(SkinData.LightDistanceY)));
        A := MaxByte - (L2 - L1) * MaxByte div acMouseLightSize;
      end;
      if DstBmp <> nil then
        Bmp := DstBmp
      else
        Bmp := SkinData.FCacheBmp;

      if ParentCI = nil then
        PaintBmpRect32(Bmp, SkinData.LightMask, MkRect(SkinData.LightMask), R.TopLeft, A)
      else
        if SkinData.FOwnerControl <> nil then
          PaintTranspBmpRect32(Bmp, SkinData.LightMask, ParentCI^.Bmp, ParentCI^.FillColor,
            MkRect(SkinData.LightMask), R.TopLeft, Point(ParentCI^.X + SkinData.FOwnerControl.Left + R.Left, ParentCI^.Y + SkinData.FOwnerControl.Top + R.Top), A)
        else
          PaintTranspBmpRect32(Bmp, SkinData.LightMask, ParentCI^.Bmp, ParentCI^.FillColor,
            MkRect(SkinData.LightMask), R.TopLeft, Point(ParentCI^.X + R.Left, ParentCI^.Y + R.Top), A);
    end;
  end;
end;


procedure DoUpdateLight(SkinData: TsCtrlSkinData; DoOutput: boolean);
var
  p: TPoint;
  f: TCustomForm;
  b, DoUpdate: boolean;
begin
  b := SkinData.DoLighting;
  if SkinData.FOwnerControl <> nil then begin
    f := GetParentForm(SkinData.FOwnerControl);
    if (f = nil) or (f.WindowState = wsMinimized) or not IsWindowVisible(f.Handle) then
      SkinData.FDoLighting := False
    else
      if CtrlIsOverlapped(SkinData.FOwnerControl) then
        SkinData.FDoLighting := False
      else
        SkinData.FDoLighting := LightingIsNear(SkinData);
  end
  else
    SkinData.FDoLighting := LightingIsNear(SkinData);

  // Repaint if changed
  if b <> SkinData.DoLighting then begin
    FreeAndNil(SkinData.HotCache);
    DoUpdate := not Assigned(SkinData.AnimTimer) or not SkinData.AnimTimer.Enabled;
  end
  else begin
    p := acMousePos;
    if SkinData.DoLighting and ((SkinData.OldMousePos.X <> p.X) or (SkinData.OldMousePos.Y <> p.Y)) then
      DoUpdate := not Assigned(SkinData.AnimTimer) or not SkinData.AnimTimer.Enabled
    else
      DoUpdate := False;

    SkinData.OldMousePos := p;
  end;
  if DoUpdate then
    if SkinData.FOwnerControl <> nil then
      SkinData.Invalidate // All paintings are produced there
    else begin
      SkinData.BGChanged := True;
      RedrawWindow(SkinData.FSWHandle, nil, 0, RDWA_ALLNOW);
    end;
end;


procedure BlendColorByMask(DstBmp, MaskBmp: TBitmap; DstPoint: TPoint; SrcRect: TRect; Color: TsColor);
var
  M0, M: PByteArray;
  D0, D: PRGBAArray_D;
  bMask, bMask2: byte;
  X, Y, DeltaD, DeltaM: integer;
begin
  // Warning! Here is no bounds checking, maskBmp should be always smaller or equal dstBmp
  if InitLine(maskBmp, Pointer(M0), DeltaM) and InitLine(dstBmp, Pointer(D0), DeltaD) then
    for Y := 0 to HeightOf(SrcRect) - 1 do begin
      D := Pointer(PAnsiChar(D0) + DeltaD * (dstPoint.Y + Y));
      M := Pointer(PAnsiChar(M0) + DeltaM * (SrcRect.Top + Y));
      for X := 0 to WidthOf(SrcRect) - 1 do begin
        bMask := M[SrcRect.Left + X];
        if bMask <> MaxByte then begin
          bMask2 := MaxByte - bMask;
          with D[dstPoint.X + X], Color do begin
            DR := (R * bMask2 + DR * bMask) shr 8;
            DG := (G * bMask2 + DG * bMask) shr 8;
            DB := (B * bMask2 + DB * bMask) shr 8;
            DA := max(DA, bMask2 * A shr 8);
          end;
        end;
      end;
    end;
end;


procedure AnimMoveText(srcData, dstData: TacMoveTextData);
var
  FBmpSize: TSize;
  Flags: Cardinal;
  bShown: boolean;
  lTicks: longint;
  ObscForm: TacGlowForm;
  AnimForm: TacGlowForm;
  FBlend: TBlendFunction;
  Bmp1, Bmp2, ABmp: TBitmap;
  w, h, srcW, srcH, dstW, dstH: integer;
  lPos, tPos, Alpha, distance, distanceNew: real;
  R, RectMoved, srcRect, dstRect, srcTextRect, dstTextRect: TRect;
{$IFNDEF ALITE}
  SavedDC: hdc;
  SavedBool: boolean;
  SavedColor: TColor;
{$ENDIF}

  procedure UpdateAlphaBmp;
  var
    sColor: TsColor;

    function UpdateAlphaColor(C: TsColor; A: byte): TsColor;
    begin
      Result.R := C.R * A shr 8;
      Result.G := C.G * A shr 8;
      Result.B := C.B * A shr 8;
      Result.A := A;
    end;

  begin
    // Clear
    FillRect32(ABmp, R, 0, 0);
    // Output src text
    sColor := UpdateAlphaColor(TsColor(srcData.Color), Round(Alpha));
    BlendColorByMask(ABmp, Bmp1, srcTextRect.TopLeft, MkRect(srcW, srcH), sColor);
    // Output dst text
    sColor := UpdateAlphaColor(TsColor(dstData.Color), MaxByte - Round(Alpha));
    BlendColorByMask(ABmp, Bmp2, dstTextRect.TopLeft, MkRect(dstW, dstH), sColor);
  end;

begin
  // Update source rect without text
  ////// Move text //////
  // Calc bigger rect RectMoved
  srcW := WidthOf(srcData.R);
  dstW := WidthOf(dstData.R);
  srcH := HeightOf(srcData.R);
  dstH := HeightOf(dstData.R);

  w := Max(srcW, dstW);
  h := Max(srcH, dstH);
  RectMoved := MkRect(w, h);

  // Make bitmaps with src and dst text
  Bmp1 := TBitmap.Create;
  Bmp1.PixelFormat := pf8bit;
  Bmp1.Canvas.Brush.Color := clWhite;

  Bmp2 := TBitmap.Create;
  Bmp2.Assign(Bmp1);

  // Src
  Bmp1.Width := srcW;
  Bmp1.Height := srcH;
  R := MkRect(srcW, srcH);
  Bmp1.Canvas.FillRect(R);
  Bmp1.Canvas.Font.Assign(srcData.Font);
  SetTextColor(Bmp1.Canvas.Handle, 0);
  acDrawText(Bmp1.Canvas.Handle, srcData.Text, R, srcData.Flags);

  // Dst
  Bmp2.Width := dstW;
  Bmp2.Height := dstH;
  R := MkRect(dstW, dstH);
  Bmp2.Canvas.FillRect(R);
{$IFNDEF ALITE}
  if dstData.Ctrl is TsHTMLLabel then begin
    SavedBool := TsHTMLLabel(dstData.Ctrl).UseSkinColor;
    SavedColor := TsHTMLLabel(dstData.Ctrl).Font.Color;
    TsHTMLLabel(dstData.Ctrl).UseSkinColor := False;
    TsHTMLLabel(dstData.Ctrl).Font.Color := 0;

    SavedDC := TsHTMLLabel(dstData.Ctrl).Canvas.Handle;
    TsHTMLLabel(dstData.Ctrl).Canvas.Handle := Bmp2.Canvas.Handle;
    TsHTMLLabel(dstData.Ctrl).Canvas.Lock;
    R := MkRect(Bmp2);

    TsHTMLLabel(dstData.Ctrl).DoDrawText(R, dstData.Flags);
    TsHTMLLabel(dstData.Ctrl).Canvas.UnLock;
    TsHTMLLabel(dstData.Ctrl).Canvas.Handle := SavedDC;

    TsHTMLLabel(dstData.Ctrl).Font.Color := SavedColor;
    TsHTMLLabel(dstData.Ctrl).UseSkinColor := SavedBool;
  end
  else
{$ENDIF}
  begin
    SetTextColor(Bmp2.Canvas.Handle, 0);
    Bmp2.Canvas.Font.Assign(dstData.Font);
    acDrawText(Bmp2.Canvas.Handle, dstData.Text, R, dstData.Flags);
  end;

  // Calc new srcRect and dstRect
  srcRect := RectMoved;
  dstRect := RectMoved;
  OffsetRect(srcRect, srcData.R.Left + (srcW - w) div 2, srcData.R.Top + (srcH - h) div 2);
  OffsetRect(dstRect, dstData.R.Left + (dstW - w) div 2, dstData.R.Top + (dstH - h) div 2);
  // Calc srcTextRect and dstTextRect (rects of text in main rect)
  srcTextRect := srcData.R;
  OffsetRect(srcTextRect, -srcRect.Left , -srcRect.Top);
  dstTextRect := dstData.R;
  OffsetRect(dstTextRect, -dstRect.Left, -dstRect.Top);

  RectMoved := srcRect;
  // Create image with both texts with RectMoved bounds
  ABmp := CreateBmp32(w, h);
  FillRect32(ABmp, MkRect(w, h), 0, 0);
  ObscForm := nil;

  // Create layered form
  AnimForm := TacGlowForm.CreateNew(nil);
  SetWindowLong(AnimForm.Handle, GWL_EXSTYLE, GetWindowLong(AnimForm.Handle, GWL_EXSTYLE) or WS_EX_LAYERED or WS_EX_NOACTIVATE);
  try
    // Init moving data
    bShown := False;
    lPos := RectMoved.Left;
    tPos := RectMoved.Top;
    Alpha := MaxByte; // Visibility of Src text
    R := MkRect(w, h);
    FBmpSize := MkSize(w, h);
    InitBlendData(FBlend, MaxByte);
    Flags := SWP_NOACTIVATE or SWP_NOREDRAW or SWP_NOCOPYBITS or SWP_NOSENDCHANGING or SWP_NOOWNERZORDER or SWP_NOREDRAW;
    distance := sqrt(sqr(abs(srcRect.Left - dstRect.Left)) + sqr(abs(srcRect.Top - dstRect.Top)));

    if srcData.Ctrl <> nil then begin
      // Update dest rect with new text
      srcData.Ctrl.Perform(SM_ALPHACMD, AC_SETBGCHANGED_HI + 1, 0);
      if srcData.Ctrl is TWinControl then
        RedrawWindow(TWinControl(srcData.Ctrl).Handle, nil, 0, RDWA_ALLNOW)
      else
        srcData.Ctrl.Repaint; // Immediate repaint for avoiding of flickering
    end;

    // While (RectMoved <> dstRect) update layered form with ready image
    while True do begin
      // Calc new pos
      lTicks := GetTickCount;
      lPos := lPos - (lPos - dstRect.Left) / 6;
      tPos := tPos - (tPos - dstRect.Top) / 6;
      distanceNew := sqrt(sqr(abs(lPos - dstRect.Left)) + sqr(abs(tPos - dstRect.Top)));
      Alpha := Min(MaxByte, Round(MaxByte * distanceNew / distance));
      if (abs(lPos - dstRect.Left) < 0.5) and (abs(tPos - dstRect.Top) < 0.5) then
        Break;

      RectMoved.Left := Round(lPos);
      RectMoved.Top := Round(tPos);
      RectMoved.Right := RectMoved.Left + w;
      RectMoved.Bottom := RectMoved.Top + h;
      UpdateAlphaBmp;
      // Update layered form
      AnimForm.SetBounds(RectMoved.Left, RectMoved.Top, w, h);
      UpdateLayeredWnd(AnimForm.Handle, aBmp, @FBmpSize, @FBlend);
      if not bShown then begin
        bShown := True;
        ShowWindow(AnimForm.Handle, SW_SHOWNOACTIVATE);
      end;
      SetWindowPos(AnimForm.Handle, HWND_TOPMOST, AnimForm.Left, AnimForm.Top, FBmpSize.cx, FBmpSize.cy, Flags);
      WaitTicks(lTicks, acTimerInterval * 1);
    end;
    if dstData.Ctrl <> nil then begin
{$IFNDEF ALITE}
      // Update dest rect with new text
      if dstData.Ctrl is TsHTMLLabel then
        UpdateGlassMode(dstData.Ctrl, False);
{$ENDIF}
      if (dstData.Ctrl <> nil) and (dstData.Ctrl.Parent <> nil) then begin
        R.TopLeft := dstData.Ctrl.ClientToScreen(MkPoint);
        R.Right := R.Left + dstData.Ctrl.Width;
        R.Bottom := R.Top + dstData.Ctrl.Height;
        ObscForm := MakeCoverForm(dstData.Ctrl.Parent.Handle, nil, True, @R);
      end;

      dstData.Ctrl.Visible := True;
      // Immediate repaint for avoiding of flickering
      if dstData.Ctrl is TsCustomLabel then
        TsCustomLabel(dstData.Ctrl).Paint
      else
        dstData.Ctrl.Repaint;

      dstData.BoundLabel.PreventVisible := False;
    end;
  finally
    // Hide layered form
    AnimForm.Free;
    FreeAndNil(ObscForm);
    Bmp1.Free;
    Bmp2.Free;
    ABmp.Free;
  end;
end;


function LightingIsNear(SkinData: TsCtrlSkinData): boolean;
var
  Wnd: THandle;
  cR, R: TRect;
  pCenter, pMouse: TPoint;
  w, h, lSize: integer;
begin
  with TsCtrlSkinData(SkinData) do begin
    if FSWHandle = 0 then
      if FOwnerControl is TWinControl then
        Wnd := TWinControl(FOwnerControl).Handle
      else
        Wnd := FOwnerControl.Parent.Handle
    else
      Wnd := FSWHandle;

    if GetWindowRect(Wnd, cR) then begin
      R := cR;
      if FOwnerControl is TGraphicControl then begin
        OffsetRect(R, FOwnerControl.Left, FOwnerControl.Top);
        R.Right := R.Left + FOwnerControl.Width;
        R.Bottom := R.Top + FOwnerControl.Height;
      end;
      w := WidthOf(R);
      h := HeightOf(R);
      lSize := maxi(w, h);
      pCenter := Point(R.Left + w div 2, R.Top + h div 2);
      pMouse := acMousePos;
      LightDistanceX := pCenter.X - pMouse.X;
      LightDistanceY := pCenter.Y - pMouse.Y;
      Result := sqrt(sqr(maxi(0, abs(LightDistanceX) - w div 2 - lSize div 2)) + sqr(maxi(0, abs(LightDistanceY) - h div 2 - lSize div 2))) < acMouseLightSize {* (1 + 1 * integer(LightNewMode))};
    end
    else
      Result := False;
  end;
end;


destructor TacPulsTimer.Destroy;
begin
  FreeAndNil(AForm);
  inherited;
end;


procedure TacPulsTimer.PrepareImage;
var
  BGBmp: TBitmap;
  bIndex: integer;
  bTransp: boolean;
  sd: TsCommonData;
  BGInfo: TacBGInfo;
begin
  if PaintData.Mode = amVibration then
    Direct := 1
  else begin
    sd := TsCommonData(AnimData);
    InitCacheBmp(sd);
    if sd.SkinIndex >= 0 then
      sd.FOwnerControl.Perform(SM_ALPHACMD, AC_PREPARECACHE_HI, 0)
    else
      StdPaintTo(sd.FCacheBmp, sd.FOwnerControl);

    if (BmpOut = nil) or (sd.FCacheBmp.Width <> BmpOut.Width) or (sd.FCacheBmp.Height <> BmpOut.Height) then
      FreeAndNil(BmpOut);

    if (BmpFrom = nil) or (sd.FCacheBmp.Width <> BmpFrom.Width) or (sd.FCacheBmp.Height <> BmpFrom.Height) then
      FreeAndNil(BmpFrom);

    CopyFrom(BmpFrom, sd.FCacheBmp, MkRect(sd.FCacheBmp));

    // Receive BG image
    bTransp := (sd.SkinIndex >= 0) and (sd.SkinManager.gd[sd.SkinIndex].Props[0].Transparency = 100);
    if bTransp then begin
      bIndex := sd.SkinManager.gd[sd.SkinIndex].BorderIndex;
      bTransp := (bIndex < 0) or
                  (sd.SkinManager.CommonSkinData.ma[bIndex].DrawMode and BDM_ACTIVEONLY = 0) or
                  (sd.SkinManager.CommonSkinData.ma[bIndex].DrawMode and BDM_FILL = 0);
    end;
    if bTransp or (sd.SkinIndex < 0) and (sd.FOwnerControl is TGraphicControl) then begin
      FreeAndNil(BmpTo);
      BmpTo := CreateBmpLike(BmpFrom);
      BGInfo.DrawDC := BmpTo.Canvas.Handle;
      BGInfo.R := MkRect(sd.FOwnerControl);
      BGInfo.Offset := Point(sd.FOwnerControl.Left, sd.FOwnerControl.Top);
      BGInfo.BgType := btUnknown;
      BmpTo.Canvas.Lock;
      GetBGInfo(@BGInfo, sd.FOwnerControl.Parent.Handle, sd.SkinIndex >= 0);//True);
      if sd.SkinIndex < 0 then
        if (BGInfo.BgType = btCache) and (BGInfo.Bmp <> nil) then
          BitBlt(BmpTo.Canvas.Handle, BGInfo.Offset.X, BGInfo.Offset.Y, sd.FOwnerControl.Width, sd.FOwnerControl.Height, BGInfo.Bmp.Canvas.Handle, 0, 0, SRCCOPY)
        else
          FillDC(BmpTo.Canvas.Handle, BGInfo.R, ColorToRGB(BGInfo.Color));

      BmpTo.Canvas.Unlock;
      // Receive difference between 2 bitmaps
      FillAlphaRect(BmpFrom, MkRect(BmpFrom), MaxByte);
      BGBmp := CreateBmpLike(BmpFrom);
      CopyBmp(BGBmp, BmpFrom);
      DiffBmp32(BGBmp, BmpTo);
      if PaintData.Mode = amColor then
        ChangeBitmapPixels(BGBmp, ChangeColorTone, acColorToRGB(PaintData.Color), clFuchsia);

      PaintBmp32(BmpTo, BGBmp);
      BGBmp.Free;
    end
    else begin
      if PaintData.Mode = amColor then
        ChangeBitmapPixels(sd.FCacheBmp, ChangeColorTone, acColorToRGB(PaintData.Color), clFuchsia);

      if (sd.FOwnerControl is TButtonControl) and (TWinControl(sd.FOwnerControl).ControlCount > 0) then begin
        sd.FCacheBmp.Canvas.Lock;
        sVCLUtils.PaintControls(sd.FCacheBmp.Canvas.Handle, TWinControl(sd.FOwnerControl), True, MkPoint);
        sd.FCacheBmp.Canvas.UnLock;
      end;
      CopyFrom(BmpTo, sd.FCacheBmp, MkRect(sd.FCacheBmp));
    end;
  end;
end;


function ClickAnimProc(Data: TObject; Iteration: integer): boolean;
const
  MinAlpha = 2;
var
  sd: TsCommonData;
  b: boolean;
begin
  sd := TsCommonData(Data);
  if sd.FOwnerControl is TWinControl then
    b := IsWindowVisible(TWinControl(sd.FOwnerControl).Handle)
  else
    b := IsWindowVisible(sd.FOwnerControl.Parent.Handle);

  if (Iteration < sd.ClickTimer.Iterations) and (TacClickTimer(sd.ClickTimer).Alpha > MinAlpha) and b then begin
    if not CtrlDCIsVisible(sd.FOwnerControl) then
      Result := False
    else begin
      if not CtrlIsOverlapped(sd.FOwnerControl) then
        TacClickTimer(sd.ClickTimer).IterateImage;

      Result := True;
    end;
  end
  else begin
    sd.ClickTimer.DisableTimer;
    Result := False;
  end;
end;


procedure DoClickAnimation(SkinData: TsCommonData; AMode: TacClickMode = cm1);
begin
  if (SkinData.FOwnerControl <> nil) and (SkinData.FOwnerControl.ComponentState * [csReading, csLoading] = []) then begin
    if SkinData.ClickTimer = nil then
      SkinData.ClickTimer := TacClickTimer.Create(SkinData.FOwnerControl);

    with TacClickTimer(SkinData.ClickTimer) do begin
      Mode := AMode;
      InitData(SkinData, 0, ClickAnimProc, 0);
      AnimControl := SkinData.FOwnerControl;
      PrepareImage;
    end;
  end;
end;


procedure DoWavingAnimation(SkinData: TsCommonData);
begin
  if (SkinData.FOwnerControl <> nil) and (SkinData.FOwnerControl.ComponentState * [csReading, csLoading] = []) then begin
    if SkinData.ClickTimer = nil then
      SkinData.ClickTimer := TacClickTimer.Create(SkinData.FOwnerControl);

    with TacClickTimer(SkinData.ClickTimer) do begin
      Mode := cm2;
      InitData(SkinData, 0, ClickAnimProc, 0);
      AnimControl := SkinData.FOwnerControl;
      PrepareImage;
    end;
  end;
end;


function DoVibration(Data: TObject; Iteration: integer): boolean;
var
  rParent: TRect;
  DC, ScrDC: hdc;
  sd: TsCommonData;
  bInfo: TacBGInfo;
  PulsTimer: TacPulsTimer;

  procedure UpdateSrcBmp;
  var
    p: TPoint;
  begin
    with PulsTimer do begin
      if BmpOut = nil then
        BmpOut := CreateBmp32(AnimControl)
      else begin
        BmpOut.Width := AnimControl.Width;
        BmpOut.Height := AnimControl.Height;
      end;
      // Receive image of control from screen
      ScrDC := GetDC(0);
      try
        p := AnimControl.ClientToScreen(MkPoint);
        BitBlt(BmpOut.Canvas.Handle, 0, 0, BmpOut.Width, BmpOut.Height, ScrDC, p.x, p.y, SRCCOPY);
      finally
        ReleaseDC(0, ScrDC);
      end;
    end;
  end;

begin
  Result := False;
  if Data is TsCommonData then begin
    sd := TsCommonData(Data);
    if not (csDestroying in sd.FOwnerControl.ComponentState) and (sd.PulsTimer <> nil) and not sd.PulsTimer.Destroyed then begin
      if (sd.FOwnerControl is TWinControl) and not IsWindowVisible(TWinControl(sd.FOwnerControl).Handle) then
        Exit;

      PulsTimer := TacPulsTimer(sd.PulsTimer);
      with PulsTimer do
        if not Busy then begin
          Busy := True;
          if Shown then
            Shown := False
          else begin
            AnimRect := sd.FOwnerControl.BoundsRect;
            rParent := AnimRect;
            if BmpOut = nil then
              UpdateSrcBmp;

            OffsetRect(AnimRect, Direct, 0);
            Direct := -Direct;
            // Parent rect calc
            OffsetRect(rParent, Direct, 0);
            if rParent.Left < AnimControl.Left then
              rParent.Right := AnimRect.Left
            else
              rParent.Left := AnimRect.Right;

            bInfo.Offset := MkPoint;
            GetBGInfo(@bInfo, AnimControl.Parent.Handle);
            DC := GetWindowDC(AnimControl.Parent.Handle);
            try
              BitBlt(DC, AnimRect.Left + bInfo.Offset.X, AnimRect.Top + bInfo.Offset.Y, BmpOut.Width, BmpOut.Height, BmpOut.Canvas.Handle, 0, 0, SRCCOPY);
              if bInfo.BgType = btCache then
                BitBlt(DC, rParent.Left, rParent.Top, WidthOf(rParent), HeightOf(rParent), bInfo.Bmp.Canvas.Handle, rParent.Left + bInfo.Offset.X, rParent.Top + bInfo.Offset.Y, SRCCOPY)
              else
                FillDC(DC, rParent, ColorToRGB(bInfo.Color));
            finally
              ReleaseDC(AnimControl.Parent.Handle, DC);
            end;
            if Assigned(acMagnForm) then
              SendAMessage(acMagnForm.Handle, AC_REFRESH);

            Shown := True;
            // Change intervals
            if Interval = iIntervalBig then begin
              PulsTimer.Iteration := 1;
              Interval := iIntervalSmall;
              AnimControl.Repaint;
              UpdateSrcBmp;
            end
            else
              if PulsTimer.Iteration > iIntervalBig div iIntervalSmall then begin
                Interval := iIntervalBig;
                Shown := False;
              end;
          end;
          Busy := False;
          Result := True;
        end
        else
          PulsTimer.TimeHandler;
    end;
  end;
end;


function DoWait(Data: TObject; Iteration: integer): boolean;
begin
  // Wait while not ready and Init later
  if Data is TsCommonData then begin
    Result := CtrlDCIsVisible(TsCommonData(Data).FOwnerControl);
    if not TsCommonData(Data).Updating and not TsCommonData(Data).BGChanged and not InAnimationProcess then
      TacPulsTimer(TsCommonData(Data).PulsTimer).Init;
  end
  else
    Result := False;
end;


type
  TAccessGraphControl = class(TGraphicControl);


function DoColor(Data: TObject; Iteration: integer): boolean;
var
  Alpha: byte;
  Ctrl: TControl;
  sd: TsCommonData;
  DC: hdc;
begin
  Result := False;
  if Data is TsCommonData then begin
    sd := TsCommonData(Data);
    if not (csDestroying in sd.FOwnerControl.ComponentState) and (sd.PulsTimer <> nil) then
      with sd.PulsTimer do
        if not Destroyed and (Iterations > 0) and CtrlDCIsVisible(sd.FOwnerControl) then begin
          Ctrl := sd.FOwnerControl;
          // Prepare BmpOut
          if BmpOut <> nil then begin
            BmpOut.Width  := Ctrl.Width;
            BmpOut.Height := Ctrl.Height;
          end
          else
            BmpOut := CreateBmp32(Ctrl);
          // Copy BmpFrom to BmpOut
          BitBlt(BmpOut.Canvas.Handle, 0, 0, BmpOut.Width, BmpOut.Height, BmpFrom.Canvas.Handle, 0, 0, SRCCOPY);
          // Update iteration data
          Glow := Glow - GlowStep;

          Alpha := LimitIt(Round(Glow), 0, MaxByte);
          // Update BmpOut
          SumBmpRect(BmpOut, BmpTo, Alpha, MkRect(BmpOut), MkPoint);
          // Repaint
          if sd.SkinIndex >= 0 then begin
            SendAMessage(Ctrl, AC_UPDATING);
            SendAMessage(Ctrl, AC_INVALIDATE);
          end
          else begin
            if Ctrl is TGraphicControl then
              DC := TAccessGraphControl(Ctrl).Canvas.Handle
            else
              if Ctrl is TWinControl then begin
                DC := GetWindowDC(TWinControl(Ctrl).Handle);
              end
              else
                DC := 0;

            if DC <> 0 then
              try
                BitBlt(DC, 0, 0, BmpOut.Width, BmpOut.Height, BmpOut.Canvas.Handle, 0, 0, SRCCOPY);
              finally
                if not (Ctrl is TGraphicControl) then
                  ReleaseDC(TWinControl(Ctrl).Handle, DC);
              end;
          end;

          if Assigned(acMagnForm) then
            SendAMessage(acMagnForm.Handle, AC_REFRESH);

          if Iteration >= Iterations then begin
            Iteration := 0;
            GlowStep := -1 * GlowStep;
            // Change direction
            if State = 0 then
              State := 1
            else
              State := 0;
          end;
          Result := True;
        end;
  end;
end;


function DoWavering(Data: TObject; Iteration: integer): boolean;
var
  sd: TsCommonData;
begin
  Result := False;
  if Data is TsCommonData then begin
    sd := TsCommonData(Data);
    if not (csDestroying in sd.FOwnerControl.ComponentState) {and sd.Skinned }and (sd.PulsTimer <> nil) and CtrlDCIsVisible(sd.FOwnerControl) then
      if (sd.SkinManager = nil) or not (msSkinChanging in sd.SkinManager.ManagerState) then
        with sd.PulsTimer do
          if not Destroyed and (Iterations > 0) then
            try
              Result := True;
              DoWavingAnimation(sd);
            except
              Result := False;
            end;
  end;
end;


procedure TacPulsTimer.DisableTimer;
begin
  inherited;
  if not Destroyed then
    FreeAndNil(AForm);
end;


procedure TacPulsTimer.Init;
begin
  PrepareImage;
  case PaintData.Mode of
    amColor: begin
      InitData(SkinData, 0, DoColor, 0, False);
      if Interval <> 0 then
        Interval := PaintData.Interval;
    end;

    amVibration: begin
      InitData(SkinData, 0, DoVibration, 0, False);
      Iterations := MaxInt;
      Interval := iIntervalSmall;
    end;

    amWavering: begin
      InitData(SkinData, 0, DoWavering, 0, False);
      Iterations := MaxInt;
      if Interval <> 0 then
        Interval := PaintData.Interval;
    end;
  end;
end;


var
  acTimersList: TList;


procedure MakeAttentTimer(Ctrl: TControl; Data: TacAttentionData);
var
  i, Ndx: integer;
  NewTimer: TacPulsTimer;
begin
  if acTimersList = nil then
    acTimersList := TList.Create;

  Ndx := -1;
  for i := 0 to acTimersList.Count - 1 do
    if (acTimersList[i] <> nil) and (TacPulsTimer(acTimersList[i]).SkinData.FOwnerControl = Ctrl) then begin
      Ndx := i;
      Break;
    end;

  if Ndx < 0 then begin // Not found
    NewTimer := TacPulsTimer.Create(nil);
    NewTimer.SkinData := TsCommonData.Create(NewTimer, False);
    NewTimer.SkinData.FOwnerControl := Ctrl;
    NewTimer.AnimControl := Ctrl;
    NewTimer.SkinData.PulsTimer := NewTimer;
    NewTimer.AnimData := NewTimer.SkinData;
    Ndx := acTimersList.Add(NewTimer);
  end;
  TacPulsTimer(acTimersList[Ndx]).SkinData.BGChanged := False;
  TacPulsTimer(acTimersList[Ndx]).PaintData := Data;
  TacPulsTimer(acTimersList[Ndx]).Glow := MaxByte;
  TacPulsTimer(acTimersList[Ndx]).InitData(TacPulsTimer(acTimersList[Ndx]).SkinData, 0, DoWait, 0);
end;


procedure StartAttention(Ctrl: TControl; Data: TacAttentionData);
var
  sd: TsCommonData;
begin
  if (SendAMessage(Ctrl, AC_PREPARECACHE) = 0) and (Data.Mode in [low(TacAttentionMode)..high(TacAttentionMode)]) then begin
    sd := TsCommonData(Ctrl.Perform(SM_ALPHACMD, AC_GETSKINDATA_HI, 0));
    if sd <> nil then
      with sd do begin
        StopTimer(sd);
        if PulsTimer <> nil then
          PulsTimer.Free;

        PulsTimer := TacPulsTimer.Create(Ctrl);
        PulsTimer.AnimData := sd;
        PulsTimer.AnimControl := Ctrl;
        TacPulsTimer(PulsTimer).PaintData := Data;
        PulsTimer.Glow := MaxByte;
        TacPulsTimer(PulsTimer).SkinData := sd;
        TacPulsTimer(PulsTimer).InitData(sd, 0, DoWait, 0);
      end
    else
      MakeAttentTimer(Ctrl, Data);
  end;
end;


procedure StopAttention(Ctrl: TControl);
begin
  StopAttention(TsCommonData(Ctrl.Perform(SM_ALPHACMD, AC_GETSKINDATA_HI, 0)));
end;


procedure StopAttention(SkinData: TsCommonData); overload;
begin
  if (SkinData <> nil) and (SkinData.PulsTimer <> nil) then
    with SkinData do begin
      if PulsTimer <> nil then begin
        PulsTimer.AnimControl := nil;
        PulsTimer.Enabled := False; // Destroyed automatically
        FreeAndNil(TacPulsTimer(PulsTimer).AForm);
      end;
      if ClickTimer <> nil then begin
        ClickTimer.AnimControl := nil;
        ClickTimer.Enabled := False;
        FreeAndNil(TacClickTimer(ClickTimer).AForm);
      end;
      StopTimer(SkinData);
      BGChanged := True;
      Invalidate;
      FOwnerControl.Repaint;
    end;
end;


procedure RestartAttention(SkinData: TsCommonData);
var
  ad: TacAttentionData;
begin
  if (SkinData.PulsTimer <> nil) and SkinData.PulsTimer.Enabled and (SkinData.PulsTimer.BmpOut <> nil) then begin
    ad := TacPulsTimer(SkinData.PulsTimer).PaintData;
    StopAttention(SkinData);
    StartAttention(SkinData.FOwnerControl, ad);
  end;
end;


const
  acwDivider = 32;

type
  TAccessWinControl = class(TWinControl);
  TAccessProvider = class(TsSkinProvider);

var
  acAnimBmp:  TBitmap = nil;
  acAnimBmp2: TBitmap = nil;
  acCtrlRects1: TRects;

procedure PrepareForAnimation(const Ctrl: TWinControl; AnimType: TacAnimTypeCtrl = atcFade);
var
  Y, X, j, DeltaS, DeltaD: integer;
  CtrlsParent: TWinControl;
  D0, D: PRGBAArray_D;
  S0, S: PRGBAArray_S;
  ScrDC, SavedDC: hdc;
  BGInfo: TacBGInfo;
  R: TRect;
begin
  if IsWindowVisible(Ctrl.Handle) and not IsIconic(GetRootParent(Ctrl.Handle)) then begin
    GetWindowRect(Ctrl.Handle, R);
    if acAnimBmp = nil then
      acAnimBmp := CreateBmp32(Ctrl.width, Ctrl.Height);

    ScrDC := GetDC(0);
    BitBlt(acAnimBmp.Canvas.Handle, 0, 0, Ctrl.Width, Ctrl.Height, ScrDC, R.Left, R.Top, SRCCOPY);
    ReleaseDC(0, ScrDC);

    case AnimType of
      atcBlur: begin
        CtrlsParent := Ctrl;
        if AnimType = atcBlur then begin
          for j := 0 to Ctrl.ControlCount - 1 do
            if (Ctrl.Controls[j] is TWinControl) and Ctrl.Controls[j].Visible and (Ctrl.Controls[j].Align = alClient) then begin
              CtrlsParent := TWinControl(Ctrl.Controls[j]); // Found better parent
              Break;
            end;

          Y := 0;
          SetLength(acCtrlRects1, 0);
          for X := 0 to CtrlsParent.ControlCount - 1 do
            if CtrlsParent.Controls[X].Visible then begin
              inc(Y);
              SetLength(acCtrlRects1, Y);
              acCtrlRects1[Y - 1] := CtrlsParent.Controls[Y - 1].BoundsRect;
              with acCtrlRects1[Y - 1] do begin
                if Left < 0 then begin
                  Left := 0;
                  if Right < Left then
                    Right := Left;
                end
                else
                  if Right > CtrlsParent.Width then begin
                    Right := CtrlsParent.Width;
                    if Left > Right then
                      Left := Right;
                  end;

                if Top < 0 then begin
                  Top := 0;
                  if Bottom < Top then
                    Bottom := Top;
                end
                else
                  if Bottom > CtrlsParent.Height then begin
                    Bottom := CtrlsParent.Height;
                    if Top > Bottom then
                      Top := Bottom;
                  end;
              end;
            end;
        end;
        if acAnimBmp2 = nil then
          acAnimBmp2 := CreateBmpLike(acAnimBmp);
        // Draw BG of control
        SavedDC := SaveDC(acAnimBmp2.Canvas.Handle);
        try
          acAnimBmp2.Canvas.Lock;
          BGInfo.DrawDC := acAnimBmp2.Canvas.Handle;
          BGInfo.R := MkRect(Ctrl);
          BGInfo.Offset := MkPoint;
          BGInfo.BgType := btUnknown;
          GetBGInfo(@BGInfo, Ctrl.Handle, True);
          acAnimBmp2.Canvas.Unlock;
        finally
          RestoreDC(acAnimBmp2.Canvas.Handle, SavedDC);
        end;
        // Leave not equal pixels only
        if InitLine(acAnimBmp2, Pointer(S0), DeltaS) and InitLine(acAnimBmp, Pointer(D0), DeltaD) then
          for Y := 0 to acAnimBmp2.Height - 1 do begin
            D := Pointer(PAnsiChar(D0) + DeltaD * Y);
            S := Pointer(PAnsiChar(S0) + DeltaS * Y);
            for X := 0 to acAnimBmp2.Width - 1 do
              with S[X], D[X] do
                if (DR = SR) and (DG = SG) and (DB = SB) then
                  DA := 0
                else
                  DA := MaxByte;
          end;
      end;
    end;

    if ow = nil then
      ow := TOutPutWindow.Create(Application);

    if Ctrl.Parent <> nil then begin
      ow.Parent := Ctrl.Parent;
      if ow = nil then
        Exit;

      ow.BringToFront;
      ow.BoundsRect := Ctrl.BoundsRect;
    end
    else
      ow.BoundsRect := R;

    if ow.Parent = nil then
      SetWindowPos(ow.Handle, GetWindow(TWinControl(Ctrl).Handle, GW_HWNDPREV), 0, 0, 0, 0, SWP_NOZORDER or SWP_SHOWWINDOW or SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE)
    else
      ShowWindow(ow.Handle, SW_SHOWNA);
  end;
end;


procedure SetChildOrderAfter(Child: TWinControl; Control: TControl);
var
  i: Integer;
begin
  for i := 0 to Child.Parent.ControlCount do
    if Child.Parent.Controls[i] = Control then begin
      TAccessWinControl(Child.Parent).SetChildOrder(Child, i + 1);
      break;
    end;
end;


procedure AnimShowControl(Ctrl: TWinControl; wTime: word = 0; MaxTransparency: integer = MaxByte; AnimType: TacAnimTypeCtrl = atcFade; DoIteration: TacAnimIterationProc = nil);
var
{$IFDEF ANIMDEBUG}
  lc, lCommon: longint;
  sn: string;
{$ENDIF}
  h: hwnd;
  rgn: hrgn;
  fR: TRect;
  lTicks: DWord;
  Flags: Cardinal;
  FBmpSize: TSize;
  SavedDC, DC: hdc;
  BGInfo: TacBGInfo;
  S0, S: PRGBAArray_S;
  sp: TAccessProvider;
  D0, D: PRGBAArray_D;
  acCtrlRects2: TRects;
  FBlend: TBlendFunction;
  OldAlphaForm: TacGlowForm;
  bExtendedBorders: Boolean;
  iterData: TacIterationData;
  k, j, n, StepCount, cy, cx, Y, X, DeltaS, DeltaD: integer;
  dx, dy, l, t, rr, bb, trans, prc, Percent1, Percent2, p, BlurKef: real;
  NewBmp, BGBmp, BlurBmp, TmpBmp, AnimBmp, acDstBmp, OldAlphaBmp: TBitmap;

  procedure Anim_Init;
  begin
    trans := 0;
    p := MaxTransparency / StepCount;
    case AnimType of
      atcAero: begin
        l  := acDstBmp.Width  / acwDivider;
        t  := acDstBmp.Height / acwDivider;
        dx := l / StepCount;
        dy := t / StepCount;
        rr := acDstBmp.Width  - l;
        bb := acDstBmp.Height - t;
      end

      else begin
        dx := 0;
        dy := 0;
        l  := 0;
        t  := 0;
        rr := 0;
        bb := 0;
      end;
    end
  end;

  procedure Anim_DoNext;
  begin
    trans := min(trans + p, MaxTransparency);
    FBlend.SourceConstantAlpha := Round(trans);
    case AnimType of
      atcAero:
        if (l < 0) or (t < 0) then
          BitBlt(AnimBmp.Canvas.Handle, 0, 0, acDstBmp.Width, acDstBmp.Height, acDstBmp.Canvas.Handle, 0, 0, SRCCOPY)
        else
          StretchBlt(AnimBmp.Canvas.Handle, Round(l), Round(t), Round(rr - l), Round(bb - t), acDstBmp.Canvas.Handle, 0, 0, acDstBmp.Width, acDstBmp.Height, SRCCOPY);

      else
        if l = 0 then begin
          BitBlt(AnimBmp.Canvas.Handle, 0, 0, acDstBmp.Width, acDstBmp.Height, acDstBmp.Canvas.Handle, 0, 0, SRCCOPY);
          l := 1;
        end;
    end;
  end;

  procedure Anim_GoToNext;
  begin
    case AnimType of
      atcAero: begin
        l := l - dx;
        t := t - dy;
        rr := rr + dx;
        bb := bb + dy;
      end
    end
  end;

  function DoScale(Value: integer): integer;
  begin
    Result := Value * 7 div 8;
  end;

begin
  if IsWindowVisible(Ctrl.Handle) and not IsIconic(GetRootParent(Ctrl.Handle)) then begin
{$IFDEF ANIMDEBUG}
    AddToLog('');
    AddToLog('<<<<<<<< ' + Ctrl.Name + ' animation start <<<<<<<<');
    iLevel := 0;
    lCommon := GetTickCount;
{$ENDIF}
    InAnimationProcess := True;
    if Ctrl is TCustomForm then
      sp := TAccessProvider(TrySendMessage(Ctrl.Handle, SM_ALPHACMD, AC_GETPROVIDER_HI, 0))
    else
      sp := nil;

    bExtendedBorders := (sp <> nil) and (sp.BorderForm <> nil) and acLayered;
    if acLayered and (Ctrl is TCustomForm) and (sp <> nil) then begin
      if sp.BorderForm <> nil then
        if sp.CoverBmp <> nil then begin
          OldAlphaBmp := sp.CoverBmp;
          sp.CoverBmp := nil;
        end
        else begin
          OldAlphaBmp := CreateBmp32;
          OldAlphaBmp.Assign(sp.SkinData.FCacheBmp);
        end;

{$IFDEF ANIMDEBUG}
      AddToLog('Paint ' + Ctrl.Name + ': ');
      lc := GetTickCount;
{$ENDIF}
      sp.PaintAll;
{$IFDEF ANIMDEBUG}
      AddToLog(IntToStr(GetTickCount - lc));
{$ENDIF}
      if sp.SkinData.FCacheBmp = nil then begin
        InAnimationProcess := False;
        Exit;
      end;
      acDstBmp := CreateBmp32(sp.SkinData.FCacheBmp);
{$IFDEF ACDEBUG}
      acDebugAnimBmp := acDstBmp;
{$ENDIF}
      acDstBmp.Canvas.Lock;
      SkinPaintTo(acDstBmp, Ctrl, 0, 0, sp, True);

      if acDstBmp = nil then
        Exit;

      if sp.BorderForm = nil then
        if sp.UWPMode then
          FillAlphaRect(acDstBmp, sp.UWPClientRect, MaxByte)
        else
          FillAlphaRect(acDstBmp, MkRect(acDstBmp), MaxByte);

      acDstBmp.Canvas.UnLock;
      StepCount := wTime div acTimerInterval;
      Flags := SWP_NOACTIVATE or SWP_NOREDRAW or SWP_NOCOPYBITS or SWP_NOSIZE or SWP_NOMOVE; // or SWP_NOZORDER;

      FBmpSize := MkSize(acDstBmp);
      if StepCount > 0 then
        InitBlendData(FBlend, 0)
      else
        InitBlendData(FBlend, MaxTransparency);

      if sp.BorderForm <> nil then begin
        if sp.BorderForm.AForm = nil then
          sp.BorderForm.CreateNewForm;

        sp.BorderForm.AForm.WindowProc := sp.BorderForm.OldBorderProc;
        if sp.BorderForm.ParentHandle <> 0 then
          SetWindowLong(sp.BorderForm.AForm.Handle, GWL_HWNDPARENT, LONG_PTR(sp.BorderForm.ParentHandle)); // Patch for ReportBuilder and similar windows

        OldAlphaForm := sp.BorderForm.AForm;
        sp.BorderForm.CreateNewForm;

        if sp.FSysExHeight then
          cy := sp.ShadowSize.Top
        else
          with sp.SkinData.CommonSkinData do
            if sp.Form.BorderStyle = bsNone then
              cy := sp.ShadowSize.Top
            else begin
              cy := SkinTitleHeight(sp.BorderForm) + sp.ShadowSize.Top - SysCaptHeight(sp) - SysBorderWidth(sp.Form.Handle, sp.BorderForm, False);
              if IsZoomed(sp.Form.Handle) and (ExMaxHeight <> ExTitleHeight) then
                cy := cy + ExTitleHeight - ExMaxHeight
            end;

        cx := SkinBorderWidth(sp.BorderForm) - SysBorderWidth(sp.Form.Handle, sp.BorderForm, False) + sp.ShadowSize.Left;

        GetWindowRect(Ctrl.Handle, fR);

        sp.BorderForm.AForm.Left := fR.Left - cx;
        sp.BorderForm.AForm.Top := fr.Top - cy;
        sp.BorderForm.AForm.Width := FBmpSize.cx;
        sp.BorderForm.AForm.Height := FBmpSize.cy;

        h := GetNextWindow(sp.Form.Handle, GW_HWNDPREV);

        SetWindowPos(sp.BorderForm.AForm.Handle, h, fR.Left - cx, fr.Top - cy, FBmpSize.cx, FBmpSize.cy, Flags);

        UpdateLayeredWnd(sp.BorderForm.AForm.Handle, acDstBmp, @FBmpSize, @FBlend);
        ShowWindow(sp.BorderForm.AForm.Handle, SW_SHOWNOACTIVATE);

        SetWindowPos(sp.BorderForm.AForm.Handle, h, fR.Left - cx, fr.Top - cy, FBmpSize.cx, FBmpSize.cy, Flags);

        AnimBmp := CreateBmp32(FBmpSize);
        FillDC(AnimBmp.Canvas.Handle, MkRect(AnimBmp), 0);

        if StepCount > 0 then begin
          Anim_Init;
          j := 0;
          while j <= StepCount do begin
            lTicks := GetTickCount;
            Anim_DoNext;
            UpdateLayeredWnd(sp.BorderForm.AForm.Handle, AnimBmp, @FBmpSize, @FBlend);
            k := min(MaxByte, max(0, MaxTransparency - FBlend.SourceConstantAlpha));
            SetFormBlendValue(OldAlphaForm.Handle, OldAlphaBmp, k);
            Anim_GoToNext;
            inc(j);
            if StepCount > 0 then begin
              Sleep(0);
              WaitTicks(lTicks);
            end;
          end;
          FBlend.SourceConstantAlpha := MaxTransparency;
        end;

        InAnimationProcess := False;
{$IFDEF DELPHI7UP}
        if sp.Form.AlphaBlend then
          sp.Form.AlphaBlendValue := MaxTransparency;
{$ENDIF}
        Ctrl.Perform(WM_SETREDRAW, 1, 0); // Vista

        RedrawWindow(Ctrl.Handle, nil, 0, RDWA_ALLNOW);
        if sp.SystemBlur.Active then
          IterateControls(sp.Form, 0, RepaintOpaque);

        SetWindowRgn(sp.BorderForm.AForm.Handle, sp.BorderForm.MakeRgn, False);
        SetWindowPos(sp.BorderForm.AForm.Handle, 0, fR.Left - cx, fr.Top - cy, FBmpSize.cx, FBmpSize.cy, Flags or SWP_NOZORDER);
        sp.BorderForm.UpdateExBordersPos(False);
        FreeAndNil(OldAlphaBmp);
        FreeAndNil(OldAlphaForm);
      end
      else begin
        GetWindowRect(Ctrl.Handle, fR);
        TacGlowForm(ow) := TacGlowForm.CreateNew(nil);

        ow.Left := Ctrl.Left;
        ow.Top := Ctrl.Top;
        ow.Width := Ctrl.Width;
        ow.Height := Ctrl.Height;
        if GetWindowLong(Ctrl.Handle, GWL_EXSTYLE) and WS_EX_TOPMOST <> 0 then begin
          TForm(ow).FormStyle := fsStayOnTop;
          h := HWND_TOPMOST
        end
        else
          if sp.BorderForm <> nil then
            h := sp.Form.Handle
          else
            h := GetWindow(sp.Form.Handle, GW_HWNDPREV);

        SetWindowPos(ow.Handle, h, Ctrl.Left, Ctrl.Top, FBmpSize.cx, FBmpSize.cy, Flags and not SWP_NOMOVE);
        SetWindowLong(ow.Handle, GWL_EXSTYLE, GetWindowLong(ow.Handle, GWL_EXSTYLE) or WS_EX_LAYERED or WS_EX_NOACTIVATE);
        UpdateLayeredWnd(ow.Handle, acDstBmp, @FBmpSize, @FBlend);

        FillArOR(sp);
        rgn := GetRgnFromArOR(sp);

        SetWindowRgn(ow.Handle, rgn, False);

        ShowWindow(ow.Handle, SW_SHOWNOACTIVATE);

        AnimBmp := CreateBmp32(FBmpSize);
        FillDC(AnimBmp.Canvas.Handle, MkRect(AnimBmp), 0);

        if StepCount > 0 then begin
          Anim_Init;
          j := 0;
          while j <= StepCount do begin
            lTicks := GetTickCount;
            Anim_DoNext;
            UpdateLayeredWnd(ow.Handle, AnimBmp, @FBmpSize, @FBlend);
            Anim_GoToNext;
            inc(j);
            if StepCount > 0 then begin
              Sleep(0);
              WaitTicks(lTicks);
            end;
          end;
          FBlend.SourceConstantAlpha := MaxTransparency;
        end;
        InAnimationProcess := False;
    {$IFDEF DELPHI7UP}
        if sp.Form.AlphaBlend then
          sp.Form.AlphaBlendValue := MaxTransparency;
    {$ENDIF}
        Ctrl.Perform(WM_SETREDRAW, 1, 0); // Vista
        RedrawWindow(Ctrl.Handle, nil, 0, RDWA_ALLNOW);
        if sp.SystemBlur.Active then
          IterateControls(sp.Form, 0, RepaintOpaque);

        FreeAndNil(ow);
      end;
      FreeAndNil(AnimBmp);
      FreeAndNil(acDstBmp);
    end
    else begin
      if ow = nil then // If not initialized by developer
        PrepareForAnimation(Ctrl);

      if ow = nil then // Init was failed
        Exit;

      SetChildOrderAfter(ow, Ctrl);
      acDstBmp := CreateBmp32(Ctrl.width, Ctrl.Height);
{$IFDEF ACDEBUG}
      acDebugAnimBmp := acDstBmp;
{$ENDIF}
      // Finish picture
      acDstBmp.Canvas.Lock;
      SavedDC := SaveDC(acDstBmp.Canvas.Handle);
      try
        SkinPaintTo(acDstBmp, Ctrl, 0, 0, nil, True);
        if acDstBmp = nil then
          Exit;
      finally
        RestoreDC(acDstBmp.Canvas.Handle, SavedDC);
        acDstBmp.Canvas.UnLock;
      end;
      NewBmp := CreateBmp32(Ctrl.width, Ctrl.Height);
      StepCount := wTime div (acTimerInterval * 2);
      if not bExtendedBorders and (sp <> nil) then begin
        sSkinProvider.FillArOR(TsSkinProvider(sp));
        if ow = nil then
          Exit;

        SetWindowRgn(ow.Handle, sSkinProvider.GetRgnFromArOR(TsSkinProvider(sp)), False);
      end;
      case AnimType of
        atcRunup: begin
          BlurKef := 1;
          TmpBmp := nil;
          BGBmp := nil;
          BlurBmp := nil;
          dy := 16 * GetWndPPI(Ctrl.Handle) div 96;
        end;

        atcBlur2: begin
          BlurKef := 1;
          TmpBmp := CreateBmp32;
          BGBmp := nil;
          BlurBmp := nil;
        end;

        atcBlur: begin
          BlurKef := 1 + 0.0006 * (1000 - min(999, wTime));
          BGBmp := CreateBmpLike(acDstBmp);
          BlurBmp := CreateBmpLike(acDstBmp);
          TmpBmp := CreateBmp32;

          BGInfo.DrawDC := BGBmp.Canvas.Handle;
          BGInfo.R := MkRect(Ctrl);
          BGInfo.Offset := MkPoint;
          BGInfo.BgType := btUnknown;
          BGBmp.Canvas.Lock;
          GetBGInfo(@BGInfo, Ctrl.Handle, True);
          BGBmp.Canvas.Unlock;
          j := BGBmp.Width - 1;

          if InitLine(BGBmp, Pointer(S0), DeltaS) and InitLine(acDstBmp, Pointer(D0), DeltaD) then // Leave not equal pixels only
            for Y := 0 to BGBmp.Height - 1 do begin
              S := Pointer(PAnsiChar(S0) + DeltaS * Y);
              D := Pointer(PAnsiChar(D0) + DeltaD * Y);
              for X := 0 to j do
                with S[X], D[X] do
                  if (SR = DR) and (SG = DG) and (SB = DB) then
                    DA := 0
                  else
                    DA := MaxByte;
            end;

          BlurBmp.Assign(acDstBmp);
        end

        else begin
          BlurKef := 1;
          BGBmp := nil;
          BlurBmp := nil;
          TmpBmp := nil;
        end;
      end;

      DC := GetWindowDC(ow.Handle);
      if StepCount > 0 then begin
        prc := MaxByte / StepCount;
        Percent2 := MaxByte;
        Percent1 := MaxByte;
        j := 0;
        n := Length(acCtrlRects1);
        if AnimType = atcBlur then begin
          SetLength(acCtrlRects2, n);
          for k := 0 to n - 1 do
            acCtrlRects2[k] := acCtrlRects1[k];
        end;

        if Assigned(DoIteration) then begin
          iterData.Ctrl := Ctrl;
          iterData.BmpTo := acDstBmp;
          iterData.BmpOut := NewBmp;
        end;

        while Percent1 > 2 do begin
          lTicks := GetTickCount;
          // Animation
          case AnimType of
            atcRunup: begin
              if Percent1 < 1 then
                Break;

              BitBlt(NewBmp.Canvas.Handle, 0, 0, NewBmp.Width, NewBmp.Height, acAnimBmp.Canvas.Handle, 0, 0, SRCCOPY);
              iterData.OutRect := MkRect(acDstBmp);
              if Assigned(DoIteration) then
                DoIteration(iterData);

              SumBmpRect(NewBmp, acDstBmp, Round(Percent1), iterData.OutRect, Point(iterData.OutRect.Left, iterData.OutRect.Top + Round(dy)));
              Percent1 := Percent1 * 2 / 3;
              dy := dy / 2;
            end;

            atcBlur2: begin
              TmpBmp.Width := MulDiv(acAnimBmp.Width, 3, 6);
              TmpBmp.Height := MulDiv(acAnimBmp.Height, 3, 6);
              acgpStretchRect(TmpBmp, acAnimBmp, MkRect(TmpBmp.Width - 1, TmpBmp.Height - 1), MkRect(acAnimBmp.Width - 1, acAnimBmp.Height - 1));
              acgpStretchRect(acAnimBmp, TmpBmp, MkRect(acAnimBmp.Width - 1, acAnimBmp.Height - 1), Rect(2, 2, TmpBmp.Width - 3, TmpBmp.Height - 3));
              Percent1 := max(0, Percent1 - prc);
              Percent2 := Percent2 * 3 / 4;
              SumBitmapsByMask(NewBmp, acAnimBmp, acDstBmp, nil, max(0, Round(Percent2)));
              if Assigned(DoIteration) then
                DoIteration(iterData);
            end;

            atcBlur: begin
              // acAnimBmp - bitmap with controls (start)
              // acAnimBmp2 - bitmap with BG (start)
              // acDstBmp - bitmap with controls (finish)
              // BGBmp - bitmap with BG (finish)
              TmpBmp.Width := DoScale(acAnimBmp.Width);
              TmpBmp.Height := DoScale(acAnimBmp.Height);

              acgpStretchRect(TmpBmp, acAnimBmp, MkRect(TmpBmp.Width - 1, TmpBmp.Height - 1), MkRect(acAnimBmp.Width - 1, acAnimBmp.Height - 1));
              acAnimBmp.Assign(TmpBmp);
              for k := 0 to n - 1 do begin
                acCtrlRects1[k].Left   := DoScale(acCtrlRects1[k].Left   );
                acCtrlRects1[k].Top    := DoScale(acCtrlRects1[k].Top    );
                acCtrlRects1[k].Right  := DoScale(acCtrlRects1[k].Right  );
                acCtrlRects1[k].Bottom := DoScale(acCtrlRects1[k].Bottom );
              end;

              NewBmp.Assign(BGBmp);
              // Prepare image for each control
              if Percent1 > 1 then
                for k := 0 to n - 1 do begin
                  fR := acCtrlRects2[k];
                  cx := WidthOf(fR);
                  cy := HeightOf(fR);
                  InflateRect(fR, (j + 1) * cx shr 5, (j + 1) * cy shr 5);
                  TmpBmp.Width := WidthOf(fR, True);
                  TmpBmp.Height := HeightOf(fR, True);
                  acgpStretchRect(TmpBmp, acAnimBmp, MkRect(TmpBmp.Width - 1, TmpBmp.Height - 1), Rect(acCtrlRects1[k].Left, acCtrlRects1[k].Top, acCtrlRects1[k].Right - 1, acCtrlRects1[k].Bottom - 1));
                  // Copy Bmp to NewBmp
                  PaintBmpRect32(NewBmp, TmpBmp, fR, MkPoint, Round(Percent1));
                end;

              PaintBmpRect32(NewBmp, TmpBmp, MkRect(BlurBmp), MkPoint, MaxByte - Round(Percent2));
              // Blur End controls (prc - blend value)
              Percent1 := max(0, Percent1 / BlurKef);
              Percent2 := max(0, Percent2 - prc);
              if Assigned(DoIteration) then
                DoIteration(iterData);
            end

            else begin // atcFade or atcAero
              SumBitmapsByMask(NewBmp, acAnimBmp, acDstBmp, nil, max(0, Round(Percent1)));
              Percent1 := max(0, Percent1 - prc);
              if Assigned(DoIteration) then
                DoIteration(iterData);
            end;
          end;
          BitBlt(DC, 0, 0, Ctrl.Width, Ctrl.Height, NewBmp.Canvas.Handle, 0, 0, SRCCOPY);
          if Assigned(acMagnForm) then
            SendMessage(acMagnForm.Handle, SM_ALPHACMD, AC_REFRESH_HI, 0);

          inc(j);
          if StepCount > 0 then begin
            Sleep(0);
            WaitTicks(lTicks, acTimerInterval * 2);
          end;
        end;
      end;
      BitBlt(DC, 0, 0, Ctrl.width, Ctrl.Height, acDstBmp.Canvas.Handle, 0, 0, SRCCOPY);
      if Assigned(acMagnForm) then
        SendMessage(acMagnForm.Handle, SM_ALPHACMD, AC_REFRESH_HI, 0);

      InAnimationProcess := False;
      if Ctrl.Visible then begin
        if ow.Parent = nil then begin
          FillAlphaRect(acDstBmp, MkRect(acDstBmp), MaxByte);
          Ctrl.Perform(WM_SETREDRAW, 1, 0); // Vista
          SetFormBlendValue(ow.Handle, acDstBmp, MaxByte - 1);
        end
        else
          Ctrl.Perform(WM_SETREDRAW, 1, 0); // Vista

        if IsWinVistaUp then
          RedrawWindow(Ctrl.Handle, nil, 0, RDWA_ALLNOW);

        if not (Ctrl is TCustomForm) or (acNativeUInt(GetWindowLong(Ctrl.Handle, GWL_EXSTYLE)) and WS_EX_LAYERED = 0) then
          SetWindowPos(ow.Handle, 0, 0, 0, 0, 0, SWP_NOZORDER or SWP_HIDEWINDOW or SWP_NOREDRAW or SWP_NOACTIVATE);
      end;
      ReleaseDC(ow.Handle, DC);
      FreeAndnil(ow);
      FreeAndNil(NewBmp);
      FreeAndNil(acAnimBmp);
      FreeAndNil(acDstBmp);
      if Assigned(acAnimBmp2) then
        FreeAndNil(acAnimBmp2);

      if Assigned(BGBmp) then
        FreeAndNil(BGBmp);

      if Assigned(TmpBmp) then
        FreeAndNil(TmpBmp);

      if Assigned(BlurBmp) then
        FreeAndNil(BlurBmp);
    end;
{$IFDEF ANIMDEBUG}
    AddToLog('>>>>>>>> ' + Ctrl.Name + ' animation finished: ' + IntToStr(GetTickCount - lCommon) + ' >>>>>>>>');
    AddToLog('');
    dec(iLevel);
{$ENDIF}
  end;
end;


function EventEnabled(Event: TacAnimatEvent; CurrentProperty: TacAnimatEvents; SkinManager: TsSkinManager = nil): boolean;
begin
  if SkinManager = nil then
    SkinManager := DefaultManager;

  if SkinManager <> nil then
    Result := SkinManager.Effects.AllowAnimation and ((Event in CurrentProperty) and not (aeGlobalDef in CurrentProperty) or ((aeGlobalDef in CurrentProperty) and (acBtnEvents[Event] in SkinManager.AnimEffects.Buttons.Events)))
  else
    Result := Event <> aeClick;
end;


function UpdateGlowing_CB(Data: TObject; iIteration: integer): boolean;
var
  sd: TsCommonData;
begin
  Result := False;
  if Data is TsCommonData then begin
    sd := TsCommonData(Data);
    if (sd.FOwnerControl <> nil) and not (csDestroying in sd.FOwnerControl.ComponentState) and (sd.AnimTimer.Iterations > 0) then
      with sd.AnimTimer do
        if sd.AnimTimer.Iteration >= sd.AnimTimer.Iterations then begin
          if State <> 1 then
            HideGlow(sd.GlowID);

          if State <= 0 then
            StopTimer(sd);
        end
        else begin
          Result := True;
          if sd.GlowID >= 0 then begin
            if State = 1 then
              Glow := Glow + GlowStep
            else
              Glow := Glow - GlowStep;

            SetGlowAlpha(sd.GlowID, LimitIt(Round(sd.AnimTimer.Glow), 0, MaxByte));
          end;
        end;
  end;
end;


function UpdateGraphic_CB(Data: TObject; iIteration: integer): boolean;
var
  Alpha: byte;
  sd: TsCommonData;
  Ctrl: TGraphicControl;
  aeData: TacAnimEventData;
begin
  Result := False;
  if Data is TsCommonData then begin
    sd := TsCommonData(Data);
    if (sd.FOwnerControl is TGraphicControl) and not (csDestroying in sd.FOwnerControl.ComponentState) and (sd.AnimTimer <> nil) and (sd.AnimTimer.Iterations <> 0) and not sd.AnimTimer.Destroyed and (sd.AnimTimer.BmpFrom <> nil) then
      with sd.AnimTimer do
        if Iterations > 0 then begin
          Ctrl := TGraphicControl(sd.FOwnerControl);
          if BmpOut <> nil then begin
            BmpOut.Width  := Ctrl.Width;
            BmpOut.Height := Ctrl.Height;
          end
          else
            sd.AnimTimer.BmpOut := CreateBmp32(Ctrl);

          BitBlt(sd.AnimTimer.BmpOut.Canvas.Handle, 0, 0, BmpOut.Width, BmpOut.Height, BmpFrom.Canvas.Handle, 0, 0, SRCCOPY);
          case State of
            0, 2: Glow := Glow - GlowStep;
            1:    Glow := Glow + GlowStep
            else  Glow := MaxByte - (Iteration / Iterations) * MaxByte;
          end;
          Alpha := LimitIt(Round(Glow), 0, MaxByte);

          if acMouseLightData.LightTimer.Enabled and (sd is TsCtrlSkinData) and Assigned(TsCtrlSkinData(sd).OnUpdateLight) then begin
            aeData.DoLighting := False;
            TsCtrlSkinData(sd).OnUpdateLight(sd.FOwnerControl, @aeData);
            if aeData.DoLighting then
              // Update image
              case State of
                0, 1: if sd.FOwnerControl.Perform(SM_ALPHACMD, AC_PREPARECACHE_Hi, 0) = 0 then
                        CopyFrom(BmpTo, sd.FCacheBmp, MkRect(sd.FCacheBmp));
              end
            else
              if (TsCtrlSkinData(sd).LightDistanceX <> 0) or (TsCtrlSkinData(sd).LightDistanceY <> 0) then begin
                TsCtrlSkinData(sd).LightDistanceX := 0;
                TsCtrlSkinData(sd).LightDistanceY := 0;
                if sd.FOwnerControl.Perform(SM_ALPHACMD, AC_PREPARECACHE_Hi, 0) = 0 then begin
                  CopyFrom(BmpTo, sd.FCacheBmp, MkRect(sd.FCacheBmp));
                  if sd.FOwnerControl is TsSpeedButton then
                    CopyFrom(BmpOut, sd.FCacheBmp, MkRect(sd.FCacheBmp));
                end;
              end;
          end;
          SumBmpRect(BmpOut, BmpTo, iff(State = 1, MaxByte - Alpha, Alpha), MkRect(BmpOut), MkPoint);
          SendAMessage(Ctrl, AC_UPDATING);
          SendAMessage(Ctrl, AC_INVALIDATE);
          if Assigned(acMagnForm) then
            SendAMessage(acMagnForm.Handle, AC_REFRESH);

          SendAMessage(Ctrl, AC_ANIMPAINT);
          if sd.AnimTimer.Iteration >= sd.AnimTimer.Iterations then begin
            if (State = 0) and (Alpha > 0) then begin
              if sd.GlowID >= 0 then
                SetGlowAlpha(sd.GlowID, Alpha);

              Iteration := Iteration - 1;
              UpdateWindow_CB(Data, iIteration);
              Result := True; // Continue while Alpha > 0
            end
            else begin
              if State = 0 then
                StopTimer(sd);

              if State <> 1 then
                HideGlow(sd.GlowID);

              if (sd is TsCtrlSkinData) and Assigned(TsCtrlSkinData(sd).OnUpdateLight) then begin
                sd.BGChanged := True;
                sd.FOwnerControl.Repaint;
              end;
            end;
          end
          else begin
            Result := True;
            if sd.GlowID >= 0 then begin
              SetGlowAlpha(sd.GlowID, Alpha);
              if (State = 0) and (Alpha < 10) then begin
                Glow := 0;
                HideGlow(sd.GlowID);
              end;
            end;
          end;
        end; // if Iterations > 0 then
  end;
end;


procedure AnimChange(SkinData: TsCommonData; NewState: integer; AAnimProc: TacAnimProc; Fast: boolean);
var
  i: integer;
begin
  with SkinData do begin
    i := GetNewTimer(AnimTimer, FOwnerControl, NewState);
    if (AnimTimer.State = -1) or (NewState <> AnimTimer.State) then begin // If not started already
      if (AnimTimer.BmpOut = nil) or (SkinData.FCacheBmp.Width <> SkinData.AnimTimer.BmpOut.Width) or (SkinData.FCacheBmp.Height <> SkinData.AnimTimer.BmpOut.Height) then
        FreeAndNil(AnimTimer.BmpOut);

      if (AnimTimer.BmpOut <> nil) then // If animate back
        AnimTimer.CopyFrom(AnimTimer.BmpFrom, AnimTimer.BmpOut, MkRect(AnimTimer.BmpOut))
      else // If new animation
        AnimTimer.CopyFrom(AnimTimer.BmpFrom, FCacheBmp, MkRect(FCacheBmp));

      if BGChanged then
        if FOwnerControl <> nil then begin
          if SendAMessage(FOwnerControl, AC_PREPARECACHE) <> 0 then // BG is not ready
            AnimTimer.DisableTimer;

          if (FOwnerControl is TButtonControl) and (TWinControl(FOwnerControl).ControlCount > 0) then begin
            FCacheBmp.Canvas.Lock;
            sVCLUtils.PaintControls(FCacheBmp.Canvas.Handle, TWinControl(FOwnerControl), True, MkPoint);
            FCacheBmp.Canvas.UnLock;
          end;
        end
        else
          if SendAMessage(OwnerHandle, AC_PREPARECACHE) <> 0 then // BG is not ready
            AnimTimer.DisableTimer;

      AnimTimer.CopyFrom(AnimTimer.BmpTo, FCacheBmp, MkRect(FCacheBmp));
      AnimTimer.InitData(SkinData, i, AAnimProc, NewState, Fast);
      AnimTimer.TimeHandler;
    end;
  end;
end;


function GetHandleBounds(sd: TsCommonData; var R: TRect): THandle;
begin
  // Get handle
  if sd.FOwnerControl is TWinControl then begin
    R := TWinControl(sd.FOwnerControl).BoundsRect;
    if TWinControl(sd.FOwnerControl).HandleAllocated then
      Result := TWinControl(sd.FOwnerControl).Handle
    else
      Result := 0;
  end
  else
    if sd.OwnerHandle <> 0 then begin
      GetWindowRect(sd.OwnerHandle, R);
      Result := sd.OwnerHandle;
    end
    else
      Result := 0;
end;


function AnimAcceleration(ADistance, AIterations: integer): real;
begin
  Result := exp(1 / (AIterations - 1) * ln(ADistance div 2 {finish size}));
end;


function AnimOffset(ADistance, AIterations, AIteration: integer): real;
var
  accel: real;
begin
  accel := AnimAcceleration(ADistance, AIterations);
  Result := ADistance / (power(accel, AIteration));
end;


function UpdateDiagonal_CB(Data: TObject; iIteration: integer): boolean; // Diagonal anim function (for checkboxes)
const
  cIterations = 14;
var
  R, cRect: TRect;
  Handle: THandle;
  sd: TsCommonData;
  cw, ch, XShift: integer;

  procedure OutputToDC(Bmp: TBitmap);
  var
    DC: HDC;
  begin
    DC := GetWindowDC(Handle);
    try
      if Application.BiDiMode = bdRightToLeft then
        acSetLayout(DC, 0);

      BitBlt(DC, 0, 0, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      ReleaseDC(Handle, DC);
    end;
  end;

begin
  Result := False;
  if Data is TsCommonData then begin
    sd := TsCommonData(Data);
    if Assigned(sd.AnimTimer) and not sd.AnimTimer.Destroyed then begin
      Handle := GetHandleBounds(sd, R);
      with sd.AnimTimer do
        if (Handle <> 0) and (BmpFrom <> nil) then begin
          Iterations := cIterations;
          CheckBmp(BmpOut, sd.FCacheBmp);
          // Copy source bmp
          BitBlt(BmpOut.Canvas.Handle, 0, 0, BmpOut.Width, BmpOut.Height, BmpFrom.Canvas.Handle, 0, 0, SRCCOPY);
          if BmpOut <> nil then begin
            if (sd.PulsTimer = nil) or not sd.PulsTimer.Enabled then begin // Output if not in click animation
              cRect := TsCheckBox(sd.FOwnerControl).CheckRect(TsCheckBox(sd.FOwnerControl).GetControlsAlignment <> taLeftJustify);
              cw := WidthOf(cRect);
              ch := HeightOf(cRect);
              XShift := Round(AnimOffset(cw + ch, cIterations, Iteration));
              MixDiagBmpRect(BmpOut, BmpTo, (cw + ch) - XShift, cRect, cRect.TopLeft);
              OutputToDC(BmpOut);
            end;
            // If finishing
            if sd.AnimTimer.Iteration >= cIterations then
              StopTimer(sd)
            else // Continue execution
              Result := True;
          end;
        end;
    end;
  end;
end;


function UpdateWindow_CB(Data: TObject; iIteration: integer): boolean;
var
  DC: HDC;
  R: TRect;
  Alpha: byte;
  Handle: THandle;
  sd: TsCommonData;
  aeData: TacAnimEventData;
begin
  Result := False;
  if Data is TsCommonData then begin
    sd := TsCommonData(Data);
    if Assigned(sd.AnimTimer) and not sd.AnimTimer.Destroyed then begin
      Handle := GetHandleBounds(sd, R);
      with sd.AnimTimer do
        if (Iterations > 0) and (Handle <> 0) and (BmpFrom <> nil) then begin
          CheckBmp(BmpOut, sd.FCacheBmp);
          // Copy source bmp
          BitBlt(BmpOut.Canvas.Handle, 0, 0, BmpOut.Width, BmpOut.Height, BmpFrom.Canvas.Handle, 0, 0, SRCCOPY);
          case State of
            0, 2: Glow := Glow - GlowStep;
            1:    Glow := Glow + GlowStep
            else  Glow := MaxByte - (Iteration / Iterations) * MaxByte;
          end;
          Alpha := LimitIt(Round(Glow), 0, MaxByte);
          // Add lighting
          if acMouseLightData.LightTimer.Enabled and (sd is TsCtrlSkinData) and Assigned(TsCtrlSkinData(sd).OnUpdateLight) then begin
            aeData.DoLighting := False;
            TsCtrlSkinData(sd).OnUpdateLight(sd.FOwnerControl, @aeData);
            if aeData.DoLighting then
              // Update image
              case State of
                0, 1: begin
                  sd.BGChanged := True;
                  if sd.FOwnerControl <> nil then begin
                    if sd.FOwnerControl.Perform(SM_ALPHACMD, AC_PREPARECACHE_Hi, 0) = 0 then
                      CopyFrom(BmpTo, sd.FCacheBmp, MkRect(sd.FCacheBmp));
                  end
                  else
                    if SendAMessage(sd.FSWHandle, AC_PREPARECACHE) = 0 then
                      CopyFrom(BmpTo, sd.FCacheBmp, MkRect(sd.FCacheBmp));
                end;
              end;
          end;
          if BmpOut <> nil then begin
            // Prepare output bmp
            SumBmpRect(BmpOut, BmpTo, iff(State = 1, MaxByte - Alpha, Alpha), MkRect(BmpOut), MkPoint);
            // Output if not in click animation
            if (sd.PulsTimer = nil) or not sd.PulsTimer.Enabled then begin
              DC := GetWindowDC(Handle);
              try
                if Application.BiDiMode = bdRightToLeft then
                  acSetLayout(DC, 0);

                BitBlt(DC, 0, 0, BmpOut.Width, BmpOut.Height, BmpOut.Canvas.Handle, 0, 0, SRCCOPY);
              finally
                ReleaseDC(Handle, DC);
              end;
            end;
            // If finishing
            if (sd.AnimTimer.Iteration >= sd.AnimTimer.Iterations) then
              if (State = 0) and (Alpha > 0) then begin // If normal state but ALpha is not 0 still
                sd.AnimTimer.Iteration := sd.AnimTimer.Iterations - Round(Glow / GlowStep) - 1; // Continue
                Result := True;
              end
              else begin
                if State = 0 then
                  StopTimer(sd);

                if State <> 1 then
                  HideGlow(sd.GlowID);

                if (sd is TsCtrlSkinData) and Assigned(TsCtrlSkinData(sd).OnUpdateLight) then begin
                  sd.BGChanged := True;
                  if sd.FOwnerControl <> nil then
                    sd.FOwnerControl.Repaint
                  else
                    RedrawWindow(sd.FSWHandle, nil, 0, RDWA_REPAINT);
                end;
              end
            else begin // Continue execution
              Result := True;
              if sd.GlowID >= 0 then begin
                // Update glowing
                SetGlowAlpha(sd.GlowID, Alpha);
                if (State = 0) and (Alpha < 10) then begin
                  Glow := 0;
                  HideGlow(sd.GlowID);
                end;
              end;
            end;
          end;
          SendAMessage(Handle, AC_ANIMPAINT);
        end;
    end;
  end;
end;


function UpdateFillLight(Data: TObject; iIteration: integer): boolean;
var
  DC: hdc;
  R: TRect;
  p: TPoint;
  Alpha: byte;
  Handle: THandle;
  sd: TsCommonData;
  CacheBmp, MaskBmp: TBitmap;
  ExitLimit,
  Blur, MinSize, MaxSize: integer;
begin
  Result := False;
  if Data is TsCommonData then begin
    sd := TsCommonData(Data);
    if sd.Skinned and Assigned(sd.AnimTimer) and not sd.AnimTimer.Destroyed then begin
      if sd.FOwnerControl is TWinControl then
        if TWinControl(sd.FOwnerControl).HandleAllocated then
          Handle := TWinControl(sd.FOwnerControl).Handle
        else
          Exit
      else
        if sd.OwnerHandle <> 0 then
          Handle := sd.OwnerHandle
        else
          if (sd.FOwnerControl is TsSpeedButton) and (sd.FOwnerControl.Parent <> nil) and sd.FOwnerControl.Parent.HandleAllocated then
            Handle := 0
          else
            Exit;

      with sd.AnimTimer do begin
        R := MkRect(sd.FCacheBmp);
        if PtInRect(R, acMousePos(sd.FOwnerControl)) then begin // If Mouse in Control still
          MinSize := Round(sqrt((sqr(sd.FCacheBmp.Width) + sqr(sd.FCacheBmp.Height)))) div 4;
          MaxSize := Max(sd.FCacheBmp.Width, sd.FCacheBmp.Height) * 2;
          if (State = 2) then begin
            if Iterations = acMaxIterations then begin // Init MouseDown data
              ThreadType := TT_FILLLIGHT;
              AnimRect.TopLeft := acMousePos(sd.FOwnerControl);
              Iterations := MaxInt; // Make slower
              GlowStep := MaxByte div Iterations;
              Value := MinSize * 3 / 2;
              if BmpFrom <> nil then
                FreeAndNil(BmpFrom);
            end
            else
              Value := Value + (Value - MinSize) / 30 + 1;

            ExitLimit := 1;
            Alpha := 0;
          end
          else begin
            if sd.BGChanged then
              sd.FOwnerControl.Perform(SM_ALPHACMD, AC_PREPARECACHE_HI, 0);

            Value := Value + (MaxSize - Value) / 30 + 1;
            Alpha := Min(MaxByte, Max(0, Round(MaxByte * max(0, (MaxSize - Value)) / (MaxSize - MinSize))));
            if sd.SkinManager.AnimEffects.Buttons.ClickEffect = ceCircleAndWave then
              ExitLimit := 40
            else
              ExitLimit := 10;
          end;

          Blur := Max(1, Round((MinSize / 3) * max(0, (MaxSize - Value)) / (MaxSize - MinSize)));
          IntState := Max(0, Round(MaxByte * max(0, (MaxSize - Value)) / (MaxSize - MinSize)));
          MinSize := Round(Value);

          CheckBmp(BmpOut, sd.FCacheBmp);
          if {(BmpOut.Canvas <> nil) and }(BmpTo <> nil) then
            BitBlt(BmpOut.Canvas.Handle, 0, 0, BmpOut.Width, BmpOut.Height, BmpTo.Canvas.Handle, 0, 0, SRCCOPY);

          MaskBmp := MakeShadow(TColor($66000000 or GetLightColor(sd, State)), MinSize div 2, 0, 0, 0, Blur, False, False);

          p.X := AnimRect.TopLeft.X - MaskBmp.Width div 2;
          p.Y := AnimRect.TopLeft.Y - MaskBmp.Height div 2;
          PaintBmpRect32(BmpOut, MaskBmp, MkRect(MaskBmp), p, IntState div 2);

          if Alpha <> 0 then
            SumBitmaps(BmpOut, sd.FCacheBmp, Alpha);

          CacheBmp := sd.FCacheBmp;
          sd.FCacheBmp := BmpOut;
          UpdateCorners(sd, State);
          sd.FCacheBmp := CacheBmp;

          // Output
          if Handle <> 0 then
            DC := GetWindowDC(Handle)
          else
            DC := TsSpeedButton(sd.FOwnerControl).Canvas.Handle;

          try
            if Application.BiDiMode = bdRightToLeft then
              acSetLayout(DC, 0);

            BitBlt(DC, 0, 0, sd.FCacheBmp.Width, sd.FCacheBmp.Height, BmpOut.Canvas.Handle, 0, 0, SRCCOPY);
          finally
            ReleaseDC(Handle, DC);
          end;

          // Check for finish
          if (sd.AnimTimer.Iteration >= sd.AnimTimer.Iterations) or (MaxSize - MinSize < ExitLimit) then begin
            if State <> 1 then
              HideGlow(sd.GlowID);

            if State in [0, 1] then begin
              if sd.SkinManager.AnimEffects.Buttons.ClickEffect = ceCircleAndWave then begin
                sd.AnimTimer.ThreadType := 0;
                DoClickAnimation(sd);
              end;
              sd.Invalidate;
            end;
            StopTimer(sd);
          end
          else begin // Continue execution
            Result := True;
            if sd.GlowID >= 0 then begin
              SetGlowAlpha(sd.GlowID, IntState div 2);
              if (State = 0) and (IntState < 10) then begin
                Glow := 0;
                HideGlow(sd.GlowID);
              end;
            end;
          end;
          MaskBmp.Free;
        end
        else
          if (sd.AnimTimer = nil) or not (sd.AnimTimer.ThreadType in [TT_LAYEREDEFFECT, TT_FILLLIGHT]) then begin
            sd.Invalidate;
            StopTimer(sd);
          end;
      end; // with sd.AnimTimer
    end;
  end;
end;


{$IFNDEF ALITE}
type
  TacAccessComboEdit = class(TsCustomComboEdit);

function UpdateComboAdv_CB(Data: TObject; iIteration: integer): boolean;
var
  DC: HDC;
  R, bRect: TRect;
  Alpha: byte;
  Handle: THandle;
  sd: TsCommonData;
  cb: TacAccessComboEdit;
  aeData: TacAnimEventData;
begin
  Result := False;
  if Data is TsCommonData then begin
    sd := TsCommonData(Data);
    if (sd.AnimTimer <> nil) and (sd.FOwnerControl is TsCustomComboEdit) then begin
      cb := TacAccessComboEdit(sd.FOwnerControl);
      Handle := cb.Handle;
      with sd.AnimTimer do
        if (Iterations > 0) and (Handle <> 0) then begin
          CheckBmp(BmpOut, sd.FCacheBmp);
          if (BmpFrom = nil) or (BmpFrom.Width <> sd.FCacheBmp.Width) or (BmpFrom.Height <> sd.FCacheBmp.Height) then begin
            if BmpFrom = nil then
              BmpFrom := CreateBmp32;

            CopyBmp(BmpFrom, sd.FCacheBmp);
          end;

          bRect := cb.BodyRect;

          BitBlt(BmpOut.Canvas.Handle, 0, 0, BmpOut.Width, bRect.Bottom, BmpFrom.Canvas.Handle, 0, 0, SRCCOPY);
          sd.AnimTimer.Value := Value + ValueStep;
          if State in [0, 2] then
            Glow := Glow - GlowStep
          else
            sd.AnimTimer.Glow := Glow + GlowStep;

          Alpha := LimitIt(Round(Value), 0, MaxByte);
          if acMouseLightData.LightTimer.Enabled and (sd is TsCtrlSkinData) and Assigned(TsCtrlSkinData(sd).OnUpdateLight) then begin
            aeData.DoLighting := False;
            TsCtrlSkinData(sd).OnUpdateLight(sd.FOwnerControl, @aeData);
            if aeData.DoLighting then
              // Update image
              case State of
                0, 1: if sd.FOwnerControl.Perform(SM_ALPHACMD, AC_PREPARECACHE_Hi, 0) = 0 then
                        CopyFrom(BmpTo, sd.FCacheBmp, MkRect(sd.FCacheBmp));
              end;
          end;
          if BmpOut = nil then
            Exit;

          SumBmpRect(BmpOut, BmpTo, MaxByte - Alpha, MkRect(BmpOut), MkPoint);
          DC := GetWindowDC(Handle);
          try
            if cb.IsFullPaint then
              BitBlt(DC, 0, 0, BmpOut.Width, bRect.Bottom, BmpOut.Canvas.Handle, 0, 0, SRCCOPY)
            else begin
              BitBltBorder(DC, 0, 0, BmpOut.Width, bRect.Bottom, BmpOut.Canvas.Handle, 0, 0, 2);
              R := cb.BtnRect(False);
              BitBlt(DC, R.Left, R.Top, WidthOf(R), HeightOf(R), BmpOut.Canvas.Handle, R.Left, R.Top, SRCCOPY);
            end;
          finally
            ReleaseDC(Handle, DC);
          end;

          SendAMessage(Handle, AC_ANIMPAINT);
          Alpha := LimitIt(Round(Glow), 0, MaxByte);
          if sd.AnimTimer.Iteration >= sd.AnimTimer.Iterations then begin
            if (State = 0) and (Alpha > 0) then begin
              sd.AnimTimer.Iteration := sd.AnimTimer.Iterations - Round(Glow / GlowStep) - 1;
              // Continue
              Result := True;
            end
            else
              if (State = 2) and (Value < MaxByte) then begin
                Iteration := Iteration - 1;
                UpdateComboAdv_CB(Data, iIteration);
              end
              else begin
                if State = 0 then
                  StopTimer(sd);

                if not (State in [1, 3]) then
                  HideGlow(sd.GlowID);
              end;
          end
          else begin
            Result := True;
            if sd.GlowID >= 0 then
              SetGlowAlpha(sd.GlowID, Alpha);
          end;
        end;
    end;
  end;
end;
{$ENDIF}


procedure DoChangePaint(Data: TsCommonData; State: integer; AAnimProc: TacAnimProc; AllowAnimation: boolean; Fast: boolean; MakeGlowing: boolean = True);
var
  aeData: TacAnimEventData;
begin
  if Data.Skinned and ((Data.OwnerHandle = 0) or IsWindowVisible(Data.OwnerHandle)) then begin
    if Data is TsCtrlSkinData then
      if (Data.SkinManager.AnimEffects.Buttons.LightingMode = lmActiveControl) and ((Data.FOwnerControl = nil) or not (csDesigning in Data.FOwnerControl.ComponentState)) then begin
        if TsCtrlSkinData(Data).DoLighting(False) then
          LightingIsNear(TsCtrlSkinData(Data)); // Update internal coords of mouse
      end;

    if not SkinIsChanging(Data) and AllowAnimation and not FadingForbidden and (Data.FCacheBmp <> nil) and not Data.BGChanged and Data.SkinManager.Effects.AllowAnimation then begin // Animated
      if MakeGlowing and (State < 100) and (Data.GlowID < 0) then
        ShowGlowingIfNeeded(Data, False, Data.OwnerHandle, 0);

      Data.BGChanged := True;
      AnimChange(Data, State, AAnimProc, False{Fast});
    end
    else begin // Without animation
      aeData.DoLighting := False;
      if Data is TsCtrlSkinData then
        TsCtrlSkinData(Data).OnUpdateLight(Data.FOwnerControl, @aeData);

      if not aeData.DoLighting then
        Data.Invalidate(True);

      if Data.FMouseAbove then begin
        if MakeGlowing then begin
          if (State < 100) and (Data.GlowID < 0) then
            ShowGlowingIfNeeded(Data, False, Data.OwnerHandle);

          if Data.AnimTimer <> nil then
            Data.AnimTimer.Glow := MaxByte;
        end;
      end
      else begin
        HideGlow(Data.GlowID);
        if Data.AnimTimer <> nil then
          Data.AnimTimer.Glow := 0;
      end;
    end
  end
  else
    Data.Invalidate;
end;


constructor TacMouseLightData.Create;
begin
  LightControls := TList.Create;
  LightHandles := TList.Create;
  LightTimer := TTimer.Create(nil);
  LightTimer.Enabled := False;
  LightTimer.Interval := acTimerInterval;
  LightTimer.OnTimer := OnLightTimer;
  UpdateScale;
  UpdateMask;
end;


destructor TacMouseLightData.Destroy;
begin
  if MaskBmp <> nil then
    MaskBmp.Free;

  LightTimer.Free;
  LightControls.Free;
  LightHandles.Free;
  inherited;
end;


procedure TacMouseLightData.OnLightTimer(Sender: TObject);
var
  i: integer;
begin
  if not InOnTimer then begin
    // Prepare
    for i := 0 to LightControls.Count - 1 do
      if TObject(LightControls[i]) is TControl then
        SendAMessage(TControl(LightControls[i]), AC_UPDATELIGHT)
      else
        if LightControls[i] <> nil then
          SendAMessage(THandle(LightControls[i]^), AC_UPDATELIGHT);

    for i := 0 to LightHandles.Count - 1 do
      if LightHandles[i] <> nil then
        SendAMessage(THandle(LightHandles[i]^), AC_UPDATELIGHT);
    InOnTimer := False;
  end;
end;


procedure TacMouseLightData.UpdateMask;
begin
  MaskBmp := MakeShadow(TColor($FF000000), MouseLightSize div 2, 0, 0, 0, 32);
end;


procedure TacMouseLightData.UpdateScale;
begin
  if DefaultManager <> nil then
    MouseLightSize := DefaultManager.ScaleInt(acMouseLightSize)
  else
    MouseLightSize := acMouseLightSize;
end;


procedure ClearAttentTimers;
var
  it: integer;
begin
  if acTimersList <> nil then begin
    for it := acTimersList.Count - 1 downto 0 do
      if acTimersList[it] <> nil then begin
        with TacPulsTimer(acTimersList[it]) do begin
          SkinData.FOwnerControl := nil;
          AnimData := nil;
          SkinData.PulsTimer := nil;
          FreeAndNil(SkinData);
        end;
        TacPulsTimer(acTimersList[it]).Free;
        acTimersList.Delete(it);
      end;

    acTimersList.Free;
  end;
end;


procedure MakeLabelTextHint(SkinData: TsCommonData; BoundLabel: TsBoundLabel; TextHint: acString);
var
  R: TRect;
  Ctrl: TacAccessControl;
  srcData, dstData: TacMoveTextData;
begin
  if (TextHint <> '') and not BoundLabel.Active then begin
    Ctrl := TacAccessControl(SkinData.FOwnerControl);
    if (not (SkinData.FOwnerControl is TWinControl) or IsWindowVisible(TWinControl(Ctrl).Handle)) and not SkinData.Updating then begin
      if BoundLabel.Caption = '' then
        BoundLabel.Caption := TextHint;

      // Prepare Data1
      srcData.Text := TextHint;
      srcData.Font := Ctrl.Font;
      srcData.Color := TextHintColor(SkinData, integer(SkinData.FMouseAbove), False{TranspMode = TM_FULL});
      srcData.Ctrl := Ctrl;
      srcData.Flags := 0;
      srcData.BoundLabel := BoundLabel;
      R := GetCtrlScreenBounds(srcData.Ctrl);
      if not (TControl(Ctrl) is TWinControl) or (TrySendMessage(TWinControl(Ctrl).Handle, SM_ALPHACMD, MakeWParam(0, AC_GETTEXTRECT), LParam(@srcData.R)) = 0) then begin
        srcData.R := R;
        OffsetRect(srcData.R, 3, 3);
      end
      else
        OffsetRect(srcData.R, R.Left, R.Top);

      SkinData.FCacheBmp.Canvas.Font.Assign(Ctrl.Font);
      acDrawText(SkinData.FCacheBmp.Canvas.Handle, TextHint, srcData.R, DT_CALCRECT or DT_EXPANDTABS or DT_NOPREFIX);

      // Create Label
      dstData.Text := GetPlainText(BoundLabel.Caption);
      BoundLabel.PreventVisible := True;
      BoundLabel.Active := True;

      // Prepare Data2
      dstData.Ctrl := BoundLabel.FTheLabel;
      dstData.BoundLabel := BoundLabel;
      dstData.Font := BoundLabel.FTheLabel.Font;
      dstData.Color := acColorToRGB(BoundLabel.FTheLabel.TextColor);
      R := GetCtrlScreenBounds(dstData.Ctrl);
      dstData.R := BoundLabel.GetTextRect;
      OffsetRect(dstData.R, R.Left, R.Top);
      dstData.Flags := DT_EXPANDTABS;

      SkinData.CtrlSkinState := SkinData.CtrlSkinState or ACS_TEXTANIMATING;
      AnimMoveText(srcData, dstData);
      SkinData.CtrlSkinState := SkinData.CtrlSkinState and not ACS_TEXTANIMATING;
    end
    else begin
      BoundLabel.PreventVisible := False;
      BoundLabel.Active := True;
    end;
  end;
end;


initialization
  acMouseLightData := TacMouseLightData.Create;


finalization
  ClearAttentTimers;
  FreeAndNil(acAnimBmp);
  FreeAndNil(acAnimBmp2);
  if acMouseLightData <> nil then
    FreeAndNil(acMouseLightData);

end.

