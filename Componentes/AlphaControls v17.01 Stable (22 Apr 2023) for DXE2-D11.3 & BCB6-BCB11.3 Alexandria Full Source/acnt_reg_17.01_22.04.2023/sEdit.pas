unit sEdit;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  {$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  {$IFDEF TNTUNICODE} TntControls, TntActnList, TntStdCtrls, TntClasses, {$ENDIF}
  acntTypes, sCommonData, sConst, sDefaults, sGlyphUtils;


type
{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
{$IFDEF TNTUNICODE}
  TsEdit = class(TTntEdit)
{$ELSE}
  TsEdit = class(TEdit)
{$ENDIF}
{$IFNDEF NOTFORHELP}
  private
    FBoundLabel: TsBoundLabel;
    FAddedGlyph: TacAddedGlyph;
    FCommonData: TsCtrlSkinData;
    FDisabledKind: TsDisabledKind;
{$IFNDEF DELPHI6UP}
    FOnMouseLeave,
    FOnMouseEnter: TNotifyEvent;
{$ENDIF}
    NCDown: boolean;
    FPadding: TacPadding;
    FOnSelectionChanged: TNotifyEvent;
    FVerticalAlignment: TVerticalAlignment;
    FLabelFromTextHint: boolean;
{$IFNDEF D2009}
    FTextHint: acString;
    procedure SetTextHint(const Value: acString);
{$ENDIF}
    procedure SetDisabledKind(const Value: TsDisabledKind);
    function ColorStored: boolean;
    function FontStored: boolean;
    procedure WMNCLButtonDblClick (var Message: TMessage); message WM_NCLBUTTONDBLCLK;
    procedure WMNCLButtonDown(var Message: TWMNCLButtonDown); message WM_NCLBUTTONDOWN;
    procedure WMNCLButtonUp  (var Message: TWMNCLButtonDown); message WM_NCLBUTTONUP;
    procedure WMNCCalcSize   (var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCHitTest    (var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure SetVerticalAlignment(const Value: TVerticalAlignment);
  protected
    FAddedPadding,
    FAddedGlyphRect: TRect;
    function IsDefaultValue: boolean; virtual;
    function TextHeight: integer;
    function AddedGlyphVisible: boolean;
    function AddedGlyphSpace: integer;
    procedure PaintAddedGlyph;
    procedure PaintBorder;
    procedure PaintAddedGlyphStd;
    function AddedGlyphRect: TRect;
    function PrepareCache: boolean; virtual;
    function TextRect: TRect;
    procedure PaintText; virtual;
    procedure OurPaintHandler(aDC: hdc = 0);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PaddingChanged(Sender: TObject);
  public
    property OnSelectionChanged: TNotifyEvent read FOnSelectionChanged write FOnSelectionChanged;
    procedure AfterConstruction; override;
    constructor Create(AOwner: TComponent); override;
    procedure CreateWnd; override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure WndProc(var Message: TMessage); override;
  published
    property Align;
  {$IFDEF DELPHI7UP}
    property MaxLength;
  {$ENDIF}
    property ParentFont stored FontStored;
    property Color stored ColorStored;
    property Font stored FontStored;
  {$IFNDEF D2009}
    property TextHint: acString read FTextHint write SetTextHint;
  {$ENDIF}
  {$IFNDEF DELPHI6UP}
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  {$ENDIF}
{$ENDIF} // NOTFORHELP
    property AddedGlyph: TacAddedGlyph read FAddedGlyph write FAddedGlyph;
    property DisabledKind: TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
    property SkinData: TsCtrlSkinData read FCommonData write FCommonData;
    property BoundLabel: TsBoundLabel read FBoundLabel write FBoundLabel;
    property Padding: TacPadding read FPadding write FPadding;
    property LabelFromTextHint: boolean read FLabelFromTextHint write FLabelFromTextHint default False;
    property VerticalAlignment: TVerticalAlignment read FVerticalAlignment write SetVerticalAlignment default taAlignTop;
  end;


{$IFNDEF NOTFORHELP}
function EditBorderWidth(aEditCtrl: {$IFDEF TNTUNICODE}TTntEdit{$ELSE}TEdit{$ENDIF}): integer;
{$ENDIF} // NOTFORHELP

implementation

uses
  ExtCtrls,
  acAnimation, sStyleSimply, sVCLUtils, sMessages, sGraphUtils, sAlphaGraph, acntUtils, sThirdParty, acAlphaImageList, sSkinProps;


function EditBorderWidth(aEditCtrl: {$IFDEF TNTUNICODE}TTntEdit{$ELSE}TEdit{$ENDIF}): integer;
begin
  with aEditCtrl do begin
    Result := integer(BorderStyle <> bsNone) * (1 + integer(Ctl3d));
{$IFDEF DELPHI7UP}
    inc(Result, integer(BevelKind <> bkNone) * (integer(BevelOuter <> bvNone) + integer(BevelInner <> bvNone)));
{$ENDIF}
  end;
end;


function TsEdit.AddedGlyphRect: TRect;
var
  bWidth: integer;
  ScaledPadding: TRect;
begin
  bWidth := EditBorderWidth({$IFDEF TNTUNICODE}TTntEdit{$ELSE}TEdit{$ENDIF}(Self));
  ScaledPadding := PaddingToRect(Padding, SkinData.CommonSkinData.PPI);
  if IsRightToLeft then
    FAddedGlyphRect.Left := Width - bWidth - FAddedGlyph.Width - ScaledPadding.Right
  else
    FAddedGlyphRect.Left := bWidth + ScaledPadding.Left;

  FAddedGlyphRect.Right := FAddedGlyphRect.Left + FAddedGlyph.Width;
  FAddedGlyphRect.Top := bWidth + ScaledPadding.Top;
  FAddedGlyphRect.Bottom := Height - bWidth - ScaledPadding.Bottom;
  Result := FAddedGlyphRect;
end;


function TsEdit.AddedGlyphSpace: integer;
begin
  Result := integer(AddedGlyphVisible) * (FAddedGlyph.Width + SkinData.CommonSkinData.Spacing div 2);
end;


function TsEdit.AddedGlyphVisible: boolean;
begin
  Result := Assigned(AddedGlyph.Images) and IsValidIndex(AddedGlyph.ImageIndex, GetImageCount(AddedGlyph.Images));
end;


procedure TsEdit.AfterConstruction;
begin
  inherited AfterConstruction;
  FCommonData.Loaded;
end;


function TsEdit.ColorStored: boolean;
begin
  Result := not SkinData.Skinned or SkinData.CustomColor;
end;


constructor TsEdit.Create(AOwner: TComponent);
begin
  FCommonData := TsCtrlSkinData.Create(Self, True);
  FCommonData.COC := COC_TsEdit;
  inherited Create(AOwner);
  FAddedGlyph := TacAddedGlyph.Create(Self, FCommonData);
  FAddedGlyphRect.Left := -1;
  FCommonData.CtrlSkinState := FCommonData.CtrlSkinState or ACS_NCCHANGED;
  FDisabledKind := DefDisabledKind;
  FBoundLabel := TsBoundLabel.Create(Self, FCommonData);
  FPadding := TacPadding.Create(Self, PaddingChanged);
  FLabelFromTextHint := False;
  FVerticalAlignment := taAlignTop;
{$IFNDEF D2009}
  FTextHint := '';
{$ENDIF}
end;


{$IFNDEF D2009}
procedure TsEdit.SetTextHint(const Value: acString);
begin
  if FTextHint <> Value then begin
    FTextHint := Value;
    SkinData.Invalidate;
  end;
end;
{$ENDIF}


procedure TsEdit.CreateWnd;
begin
  inherited;
  if HandleAllocated and (csDesigning in ComponentState) then
    SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);
end;


destructor TsEdit.Destroy;
begin
  FreeAndNil(FBoundLabel);
  FreeAndNil(FCommonData);
  FreeAndNil(FAddedGlyph);
  FPadding.Free;
  inherited Destroy;
end;


procedure TsEdit.Loaded;
begin
  inherited Loaded;
  FCommonData.Loaded;
  if HandleAllocated then
    SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);
end;


procedure TsEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (FAddedGlyph <> nil) and (AComponent = FAddedGlyph.Images) then
    FAddedGlyph.Images := nil;
end;


procedure TsEdit.OurPaintHandler(aDC: hdc = 0);
var
  DC, SavedDC: hdc;
  PS: TPaintStruct;
begin
  if not InAnimationProcess and ((aDC <> SkinData.PrintDC) or (aDC = 0)) then
    BeginPaint(Handle, PS);

  if not InUpdating(FCommonData) then begin
    SavedDC := 0;
    if aDC = 0 then begin
      DC := GetWindowDC(Handle);
      SavedDC := SaveDC(DC);
    end
    else
      DC := aDC;

    try
      if (SkinData.PulsTimer <> nil) and SkinData.PulsTimer.Enabled and (SkinData.PulsTimer.BmpOut <> nil) then
        BitBlt(DC, 0, 0, Width, Height, SkinData.PulsTimer.BmpOut.Canvas.Handle, 0, 0, SRCCOPY)
      else begin
        FCommonData.BGChanged := FCommonData.BGChanged or FCommonData.HalfVisible or GetBoolMsg(Parent, AC_GETHALFVISIBLE);
        FCommonData.HalfVisible := not RectInRect(Parent.ClientRect, BoundsRect);
        if not FCommonData.BGChanged or PrepareCache then begin
          UpdateCorners(FCommonData, integer(ControlIsActive(FCommonData)));
          BitBlt(DC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
        end
        else
          FCommonData.FUpdating := True;
      end;
    finally
      if aDC = 0 then begin
        RestoreDC(DC, SavedDC);
        ReleaseDC(Handle, DC);
      end;
    end;
  end;
  if not InAnimationProcess and ((aDC <> SkinData.PrintDC) or (aDC = 0)) then
    EndPaint(Handle, PS);
end;


function TsEdit.PrepareCache: boolean;
var
  BGInfo: TacBGInfo;
begin
  Result := True;
  if SkinData.Skinned then begin
    BGInfo.BgType := btUnknown;
    GetBGInfo(@BGInfo, Parent);
    if BGInfo.BgType <> btNotReady then begin
      InitCacheBmp(SkinData);
      if BorderStyle = bsSingle then
        PaintItem(FCommonData, BGInfoToCI(@BGInfo), True, integer(ControlIsActive(FCommonData)), MkRect(Self), Point(Left, Top), FCommonData.FCacheBmp, False)
      else
        PaintItemBG(FCommonData, BGInfoToCI(@BGInfo), integer(ControlIsActive(FCommonData)), MkRect(Self), Point(Left, top), FCommonData.FCacheBmp);

      PaintText;
      if not Enabled then
        BmpDisabledKind(FCommonData.FCacheBmp, FDisabledKind, Parent, BGInfoToCI(@BGInfo), Point(Left, Top));

      FCommonData.BGChanged := False;
    end
    else begin
      Result := False;
      Exit;
    end;
  end
  else begin
    InitCacheBmp(SkinData);
    FillDC(FCommonData.FCacheBmp.Canvas.Handle, MkRect(Self), acColorToRGB(Color));
  end;
  if AddedGlyphVisible then
    PaintAddedGlyph;
end;


procedure TsEdit.PaddingChanged(Sender: TObject);
begin
  if not (csLoading in ComponentState) then begin
    FCommonData.CtrlSkinState := FCommonData.CtrlSkinState or ACS_NCCHANGED;
    FCommonData.Invalidate(True);
  end;
end;


procedure TsEdit.PaintAddedGlyph;
var
  DrawData: TacDrawGlyphData;
  h, w: integer;
  C: TColor;
  R: TRect;
begin
  R := AddedGlyphRect;
  DrawData.Glyph := nil;

  DrawData.Blend := AddedGlyph.Blend;
  DrawData.Images := AddedGlyph.Images;
  DrawData.NumGlyphs := 1;
  DrawData.ImageIndex := AddedGlyph.ImageIndex;
  if not IsValidIndex(DrawData.ImageIndex, GetImageCount(AddedGlyph.Images)) then
    DrawData.ImageIndex := AddedGlyph.ImageIndex;

  DrawData.CurrentState := integer(Focused or SkinData.FFocused);
  DrawData.Down := False;
  DrawData.Enabled := Enabled;
  DrawData.Reflected := False;
  DrawData.DstBmp := SkinData.FCacheBmp;
  DrawData.CommonSkinData := SkinData.CommonSkinData;
  DrawData.PPI := GetPPI(SkinData);
  DrawData.ImgRect := R;
  w := AddedGlyph.Width;
  h := AddedGlyph.Height;
  DrawData.ImgRect.Left   := R.Left + (WidthOf(R) - w) div 2;
  DrawData.ImgRect.Top    := R.Top + (HeightOf(R) - h) div 2;
  DrawData.ImgRect.Right  := DrawData.ImgRect.Left + w;
  DrawData.ImgRect.Bottom := DrawData.ImgRect.Top + h;

  DrawData.DisabledGlyphKind := DefDisabledGlyphKind;
  if SkinData.Skinned then begin
    DrawData.Canvas := SkinData.FCacheBmp.Canvas;
    DrawData.Grayed := (DrawData.CurrentState = 0) and (AddedGlyph.Grayed or SkinData.SkinManager.Effects.DiscoloredGlyphs);
    if DrawData.Grayed then
      DrawData.BGColor := SkinData.CommonSkinData.gd[SkinData.SkinIndex].Props[DrawData.CurrentState].Color
    else
      DrawData.BGColor := clNone;
  end
  else begin
    DrawData.Canvas := nil;
    DrawData.Grayed := AddedGlyph.Grayed;
    if DrawData.Grayed then
      DrawData.BGColor := Color
    else
      DrawData.BGColor := clNone;
  end;
  if not DrawData.Enabled then
    DrawData.Blend := DrawData.Blend + (100 - DrawData.Blend) div 2;

  C := clNone;
  if AddedGlyph.ColorTone <> clNone then
    if (DrawData.Images is TsCharImageList) or ((DrawData.Images is TsVirtualImageList) and (TsVirtualImageList(DrawData.Images).AlphaImageList is TsCharImageList)) then begin
      DrawData.Grayed := False;
      if DrawData.DstBmp <> nil then begin
        C := DrawData.DstBmp.Canvas.Font.Color;
        DrawData.DstBmp.Canvas.Font.Color := AddedGlyph.ColorTone;
        DrawData.DstBmp.Canvas.Font.Size := 0; // Def color is not allowed
      end;
    end
    else begin
      DrawData.Grayed := True;
      DrawData.BGColor := AddedGlyph.ColorTone;
    end;

  acDrawGlyphEx(DrawData);
  if C <> clNone then // Restore font color
    if DrawData.DstBmp <> nil then
      DrawData.DstBmp.Canvas.Font.Color := C;
end;


procedure TsEdit.PaintBorder;
var
  DC, SavedDC: HDC;
  BordWidth: integer;
begin
  if not InUpdating(FCommonData) then begin
    DC := GetWindowDC(Handle);
    SavedDC := SaveDC(DC);
    try
      if FCommonData.BGChanged then
        if not PrepareCache then begin
          RestoreDC(DC, SavedDC);
          ReleaseDC(Handle, DC);
          FCommonData.FUpdating := True;
          Exit;
        end;

      BordWidth := EditBorderWidth({$IFDEF TNTUNICODE}TTntEdit{$ELSE}TEdit{$ENDIF}(Self));
{$IFDEF DELPHI7UP}
      if BordWidth = 0 then begin
        if BevelInner <> bvNone then
          inc(BordWidth);

        if BevelOuter <> bvNone then
          inc(BordWidth);
      end;
{$ENDIF}
      UpdateCorners(FCommonData, integer(ControlIsActive(FCommonData)));
      BitBltBorder(DC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0,
                   BordWidth + FAddedPadding.Left, BordWidth + FAddedPadding.Top, BordWidth + FAddedPadding.Right, BordWidth + FAddedPadding.Bottom);

      if AddedGlyphVisible then
        if IsRightToLeft then
          BitBlt(DC, BordWidth, 0, AddedGlyphSpace, Height, SkinData.FCacheBmp.Canvas.Handle, BordWidth, 0, SRCCOPY)
        else
          with SkinData.CommonSkinData do
            BitBlt(DC, AddedGlyphRect.Left - Spacing div 2, 0, AddedGlyphSpace + Spacing div 2, Height, SkinData.FCacheBmp.Canvas.Handle, AddedGlyphRect.Left - Spacing div 2, 0, SRCCOPY);
{$IFDEF DYNAMICCACHE}
      if Assigned(FCommonData.FCacheBmp) then
        FreeAndNil(FCommonData.FCacheBmp);
{$ENDIF}
    finally
      RestoreDC(DC, SavedDC);
      ReleaseDC(Handle, DC);
    end;
  end;
end;


procedure TsEdit.PaintAddedGlyphStd;
var
  DC: hdc;
  bWidth: integer;
begin
  if not SkinData.Skinned then
    if (FAddedPadding.Left <> 0) or (FAddedPadding.Top <> 0) or (FAddedPadding.Right <> 0) or (FAddedPadding.Bottom <> 0) then begin
      bWidth := EditBorderWidth({$IFDEF TNTUNICODE}TTntEdit{$ELSE}TEdit{$ENDIF}(Self));
      PrepareCache;
      DC := GetWindowDC(Handle);
      try
        ExcludeClipRect(DC, 0, 0, Width, bWidth); // Top
        ExcludeClipRect(DC, 0, bWidth, bWidth, Height); // Left
        ExcludeClipRect(DC, bWidth, Height - bWidth, Width, Height); // Bottom
        ExcludeClipRect(DC, Width - bWidth, bWidth, Width, Height - bWidth); // Right
        if AddedGlyphVisible then
          with SkinData.CommonSkinData, AddedGlyphRect do
            if IsRightToLeft then
              BitBlt(DC, Left - Spacing div 2, Top, WidthOf(AddedGlyphRect) + Spacing div 2, HeightOf(AddedGlyphRect), SkinData.FCacheBmp.Canvas.Handle, Left - Spacing div 2, Top, SRCCOPY)
            else
              BitBlt(DC, Left, Top, WidthOf(AddedGlyphRect) + Spacing div 2, HeightOf(AddedGlyphRect), SkinData.FCacheBmp.Canvas.Handle, Left, Top, SRCCOPY);

        if FAddedPadding.Top <> 0 then
          BitBlt(DC, 0, 0, Width, FAddedPadding.Top + bWidth, SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);

        if FAddedPadding.Left <> 0 then
          BitBlt(DC, 0, 0, FAddedPadding.Left + bWidth, Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);

        if FAddedPadding.Right <> 0 then
          BitBlt(DC, Width - FAddedPadding.Right - bWidth, 0, FAddedPadding.Right + bWidth, Height, SkinData.FCacheBmp.Canvas.Handle, Width - FAddedPadding.Right - bWidth, 0, SRCCOPY);

        if FAddedPadding.Bottom <> 0 then
          BitBlt(DC, 0, Height - FAddedPadding.Bottom - bWidth, Width, FAddedPadding.Bottom + bWidth,
                 SkinData.FCacheBmp.Canvas.Handle, 0, Height - FAddedPadding.Bottom - bWidth, SRCCOPY);
      finally
        ReleaseDC(Handle, DC);
      end;
    end;
end;


procedure TsEdit.PaintText;
var
  R: TRect;
  aText: acString;
  Flags: Cardinal;
  TranspMode: byte;
begin
  FCommonData.FCacheBMP.Canvas.Font.Assign(Font);
  Flags := DT_NOPREFIX or DT_SINGLELINE;
  R := TextRect;
{$IFNDEF D2009}
  if IsRightToLeft then begin
    Flags := Flags or DT_RTLREADING or DT_RIGHT;
    dec(R.Right);
  end;
{$ENDIF}
  TranspMode := GetTransMode(SkinData);
  if TranspMode = TM_FULL then
    dec(R.Left, 2);

  Flags := Flags or DT_ALPHATEXT * Cardinal(TranspMode = TM_FULL);
{$IFDEF D2009}
  Flags := Flags or Cardinal(GetStringFlags(Self, Alignment)) and not DT_VCENTER;
{$ENDIF}
  if not IsDefaultValue then begin
    if PasswordChar <> #0 then
      acFillString(aText, Length(Text), iff(PasswordChar = '*', CharBullet, PasswordChar))
    else
      aText := Text;

    acWriteTextEx(FCommonData.FCacheBMP.Canvas, PacChar(aText), True, R, Flags, FCommonData, ControlIsActive(FCommonData));
  end
  else
    if not SkinData.FFocused then
      TextHintOut(SkinData, R, TextHint, Flags)
    else
      Exit;

  if TranspMode = TM_PARTIAL then
    FillAlphaRect(FCommonData.FCacheBmp, R);
end;


procedure TsEdit.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    FCommonData.Invalidate;
  end;
end;


procedure TsEdit.SetVerticalAlignment(const Value: TVerticalAlignment);
begin
  if FVerticalAlignment <> Value then begin
    FVerticalAlignment := Value;
    PaddingChanged(Self);
  end;
end;


function TsEdit.TextHeight: integer;
begin
  Result := GetFontHeight(Font.Handle);
end;


function TsEdit.TextRect: TRect;
var
  bw, iSingle: integer;
begin
  if BorderStyle <> bsNone then
    bw := EditBorderWidth({$IFDEF TNTUNICODE}TTntEdit{$ELSE}TEdit{$ENDIF}(Self)) {$IFDEF DELPHI7UP} + integer(BevelKind <> bkNone) * (integer(BevelOuter <> bvNone) + integer(BevelInner <> bvNone)) {$ENDIF}
  else
    bw := 0;

  Result := MkRect;
  iSingle := integer(BorderStyle = bsSingle);
  if IsRightToLeft then
    Result := Rect(bw + 2 * integer(not Ctl3D), bw + iSingle, Width - bw - AddedGlyphSpace - SkinData.CommonSkinData.Spacing div 2 + iSingle, Height - bw)
  else
    Result := Rect(AddedGlyphSpace + bw + iSingle, bw + iSingle, Width - bw - iSingle, Height - bw);

  AddPadding(Result, FAddedPadding);
end;


procedure TsEdit.WMNCCalcSize(var Message: TWMNCCalcSize);
var
  dh: integer;
begin
  inherited;
  if not (csLoading in ComponentState) then begin
    FAddedPadding := PaddingToRect(Padding, SkinData.CommonSkinData.PPI);
    if FVerticalAlignment <> taAlignTop then
      case FVerticalAlignment of
        taVerticalCenter: begin
          dh := Height - TextHeight - FAddedPadding.Top - FAddedPadding.Bottom;
          inc(FAddedPadding.Top, dh div 2);
          inc(FAddedPadding.Bottom, dh div 2);
        end;
        taAlignBottom: inc(FAddedPadding.Top, Height - TextHeight - FAddedPadding.Top - FAddedPadding.Bottom);
      end;

    AddPadding(Message.CalcSize_Params.rgrc[0], FAddedPadding);
    if AddedGlyphVisible then
      with Message.CalcSize_Params.rgrc[0] do begin
        dh := AddedGlyphSpace;
        if IsRightToLeft then begin
          dec(Right, dh);
          if SkinData.SkinIndex < 0 then
            inc(FAddedPadding.Right, dh);
        end
        else begin
          inc(Left, dh);
          if SkinData.SkinIndex < 0 then
            inc(FAddedPadding.Left, dh);
        end;
      end;
  end
  else
    FAddedPadding := MkRect;
end;


procedure TsEdit.WMNCHitTest(var Message: TWMNCHitTest);
var
  R: TRect;
  p: TPoint;
begin
  inherited;
  if AddedGlyphVisible then begin
    GetWindowRect(Handle, R);
    p.X := Message.XPos - R.Left;
    p.Y := Message.YPos - R.Top;
    if PtInRecT(AddedGlyphRect, p) then
      Message.Result := HTBORDER
    else
      Message.Result := HTCLIENT;
  end
  else
    Message.Result := HTCLIENT;
end;


procedure TsEdit.WMNCLButtonDblClick(var Message: TMessage);
begin
  inherited;
  if Assigned(OnDblClick) then
    OnDblClick(Self);
end;


procedure TsEdit.WMNCLButtonDown(var Message: TWMNCLButtonDown);
begin
  NCDown := True;
  inherited;
end;


procedure TsEdit.WMNCLButtonUp(var Message: TWMNCLButtonDown);
begin
  inherited;
  if NCDown then begin
    if Assigned(OnClick) then
      OnClick(Self);

    NCDown := False;
  end;
end;


procedure TsEdit.WndProc(var Message: TMessage);
var
  DC: hdc;
  bw: integer;
  PS: TPaintStruct;
  OldPos, NewPos: TPoint;
begin
{$IFDEF LOGGED}
  AddToLog(Message, IntToStr(Color));
{$ENDIF}
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_REMOVESKIN:
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
            CommonWndProc(Message, FCommonData);
            SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);
            Exit;
          end;

        AC_REFRESH:
          if RefreshNeeded(SkinData, Message) then begin
            CommonWndProc(Message, FCommonData);
            if not InAnimationProcess and HandleAllocated and Visible then
              RedrawWindow(Handle, nil, 0, RDWA_REPAINT);

            Exit;
          end;

        AC_SETNEWSKIN:
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
            CommonWndProc(Message, FCommonData);
            Exit;
          end;

        AC_CHILDCHANGED: begin
          Message.LParam := 0; // Repainting is not required  in internal buttons
          Exit;
        end;

        AC_GETCONTROLCOLOR:
          if not FCommonData.Skinned then begin
            Message.Result := acColorToRGB(Color);
            Exit;
          end;

        AC_PREPARECACHE: begin
          PrepareCache;
          Exit;
        end;

        AC_SETSCALE: begin
          FCommonData.CtrlSkinState := FCommonData.CtrlSkinState or ACS_NCCHANGED;
          CommonMessage(Message, SkinData);
          SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);
          if BoundLabel <> nil then
            BoundLabel.UpdateScale(Message.LParam);

          Exit;
        end;

        AC_GETTEXTRECT: begin
          PRect(Message.LParam)^ := TextRect;
          Message.Result := 1;
          Exit;
        end;

        AC_GETDEFSECTION: begin
          Message.Result := 0 + 1;
          Exit;
        end;

        AC_GETDEFINDEX: begin
          if FCommonData.SkinManager <> nil then
            Message.Result := FCommonData.SkinManager.SkinCommonInfo.Sections[ssEdit] + 1;

          Exit;
        end

        else
          if CommonMessage(Message, SkinData) then
            Exit;
      end;

    CM_CHANGED:
      SkinData.BGChanged := True;

    WM_SIZE, WM_WINDOWPOSCHANGED:
      FAddedGlyphRect.Left := -1;

    CM_ENABLEDCHANGED: begin
      FCommonData.Invalidate;
      inherited;
      if Assigned(BoundLabel) then
        BoundLabel.HandleOwnerMsg(Message, Self);

      Exit;
    end;

    WM_SETFOCUS, CM_ENTER: if not (csDesigning in ComponentState) and CanFocus then begin
      if not CommonWndProc(Message, FCommonData) then
        inherited;

      if SkinData.CtrlSkinState and ACS_FOCUSCHANGING = 0 then
        if LabelFromTextHint and IsDefaultValue then
          MakeLabelTextHint(SkinData, BoundLabel, TextHint);

      Exit;
    end;

    WM_KILLFOCUS, CM_EXIT:
      if IsDefaultValue and (TextHint <> '') then begin
        if LabelFromTextHint then
          HideBoundLabel(BoundLabel);

{$IFNDEF D2009}
        SkinData.Invalidate;
{$ENDIF}        
      end;
  end;

  if not ControlIsReady(Self) or not FCommonData.Skinned then begin
    case Message.Msg of
      WM_PRINT: begin
        PaintTo(TWMPaint(Message).DC, 0, 0);
        Exit;
      end;

      WM_NCPAINT: begin
        inherited;
        PaintAddedGlyphStd;
        Exit;
      end;
    end;
    inherited
  end
  else begin
    case Message.Msg of
      WM_ERASEBKGND, CN_DRAWITEM: begin
        if not InAnimationProcess and not InUpdating(SkinData) and Enabled and Focused then
          inherited;

        Exit;
      end;

      WM_NCPAINT: if IsWindowVisible(Handle) then begin
        PaintBorder;
        Exit;
      end;

      WM_PAINT: begin
        if FCommonData.CtrlSkinState and ACS_NCCHANGED <> 0 then begin
          FCommonData.CtrlSkinState := FCommonData.CtrlSkinState and not ACS_NCCHANGED;
          SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);
        end;
        if InAnimationProcess or InUpdating(SkinData) then begin // Exit if parent is not ready yet
          BeginPaint(Handle, PS);
          EndPaint(Handle, PS);
          Exit;
        end;
        if not (Enabled and TWinControl(FCommonData.FOwnerControl).Focused) or (SkinData.CtrlSkinState and ACS_TEXTANIMATING <> 0) then begin
          OurPaintHandler(TWMPaint(Message).DC);
          if csDesigning in ComponentState then
            inherited;

          Exit;
        end
      end;

      WM_PRINT: begin
        if FCommonData.CtrlSkinState and ACS_NCCHANGED <> 0 then begin
          FCommonData.CtrlSkinState := FCommonData.CtrlSkinState and not ACS_NCCHANGED;
          SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);
        end;
        SkinData.Updating := False;
        DC := TWMPaint(Message).DC;
        PrepareCache;
        UpdateCorners(SkinData, integer(ControlIsActive(FCommonData)));
        bw := integer(BorderStyle <> bsNone) * (1 + integer(Ctl3d));
        if PRF_CLIENT and Message.LParam = PRF_CLIENT then begin
          MoveWindowOrg(DC, -bw, -bw);
          IntersectClipRect(DC, 0, 0, SkinData.FCacheBmp.Width - bw, SkinData.FCacheBmp.Height - bw);
        end
        else
          BitBltBorder(DC, 0, 0, SkinData.FCacheBmp.Width, SkinData.FCacheBmp.Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, bw);

        OurPaintHandler(DC);
        Exit;
      end;

      CM_COLORCHANGED:
        if FCommonData.CustomColor then
          FCommonData.BGChanged := True;

      WM_KEYFIRST..WM_KEYLAST, WM_MOUSEFIRST..WM_MOUSELAST:
        if Assigned(FOnSelectionChanged) then
          Perform(EM_GETSEL, WPARAM(@oldpos.X), LPARAM(@oldpos.Y));
    end;
    if CommonWndProc(Message, FCommonData) then
      Exit;

    inherited;
    case Message.Msg of
      SM_ALPHACMD:
        case Message.WParamHi of
          AC_ENDPARENTUPDATE:
            if SkinData.FUpdating then
              if not InUpdating(FCommonData, True) then begin
                Repaint;
                SendMessage(Handle, WM_NCPAINT, 0, 0);
              end;
        end;

{$IFNDEF DELPHI6UP}
      CM_MOUSEENTER, CM_MOUSELEAVE:
        if not (csDestroying in ComponentState) and not (csDesigning in ComponentState) and Enabled then begin
          SkinData.FMouseAbove := Message.Msg = CM_MOUSEENTER;
          if SkinData.FMouseAbove then begin
            if Assigned(FOnMouseEnter) then
              FOnMouseEnter(Self);
          end
          else
            if Assigned(FOnMouseLeave) then
              FOnMouseLeave(Self);

          RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_NOERASE or RDW_UPDATENOW);
        end;
{$ENDIF}

      WM_SETTEXT, CM_TEXTCHANGED, CM_VISIBLECHANGED, CM_ENABLEDCHANGED:
        if not (csLoading in ComponentState) and not InAnimationProcess then begin
          FCommonData.BGChanged := True;
          Repaint;
          SendMessage(Handle, WM_NCPAINT, 0, 0);
        end;

      WM_SETFONT: begin
        if FVerticalAlignment <> taAlignTop then
          FCommonData.CtrlSkinState := FCommonData.CtrlSkinState or ACS_NCCHANGED;

        FCommonData.Invalidate;
      end;

      WM_SIZE:
        if (FVerticalAlignment <> taAlignTop) and not (csDesigning in ComponentState) then
          FCommonData.CtrlSkinState := FCommonData.CtrlSkinState or ACS_NCCHANGED;

      CM_FOCUSCHANGED:
        if SkinData.FFocused then
          SkinData.BGChanged := True;

      WM_KEYFIRST..WM_KEYLAST, WM_MOUSEFIRST..WM_MOUSELAST:
        if Assigned(FOnSelectionChanged) then begin
          Perform(EM_GETSEL, WPARAM(@newpos.X), LPARAM(@newpos.Y));
          if not CompareMem(@oldpos, @newpos, sizeof(TPoint)) then
            FOnSelectionChanged(self);
        end;
    end;
  end;
  if Assigned(BoundLabel) then
    BoundLabel.HandleOwnerMsg(Message, Self);
end;


function TsEdit.FontStored: boolean;
begin
  Result := IsCustomFont(Self, Font, not SkinData.Skinned or SkinData.CustomFont);
end;


function TsEdit.IsDefaultValue: boolean;
begin
  Result := Text = '';
end;

end.
