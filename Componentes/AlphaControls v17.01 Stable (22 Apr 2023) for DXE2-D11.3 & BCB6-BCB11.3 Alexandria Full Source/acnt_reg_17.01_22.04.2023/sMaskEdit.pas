unit sMaskEdit;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface


uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, Mask,
  {$IFNDEF DELPHI5} types, {$ENDIF}
  {$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  acntTypes, sCommonData, sConst, sDefaults, sGlyphUtils;

type
  TValidateErrorEvent = procedure(Sender: TObject; Text: string) of object;

  TacCustomMaskEdit = class(TCustomMaskEdit)
{$IFNDEF NOTFORHELP}
  private
    FCheckOnExit: boolean;
    FBoundLabel: TsBoundLabel;
    FAddedGlyph: TacAddedGlyph;
    FCommonData: TsCtrlSkinData;
    FDisabledKind: TsDisabledKind;
    FOnValidateError: TValidateErrorEvent;
    FVerticalAlignment: TVerticalAlignment;
    FPadding: TacPadding;
    FLabelFromTextHint: boolean;
  {$IFNDEF D2009}
    FTextHint: acString;
    procedure SetTextHint(const Value: acString);
  {$ENDIF}
    procedure SetDisabledKind(const Value: TsDisabledKind);
    function FontStored: boolean;
    function ColorStored: boolean;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure SetVerticalAlignment(const Value: TVerticalAlignment);
  protected
    FContentHidden: boolean;
    FAddedPadding,
    FAddedGlyphRect: TRect;
    function BodyRect: TRect; virtual;
    procedure TextPaintPrevent(Locked: boolean); virtual;
    function AllowBtnStyle: Boolean; virtual;
    function IsDefaultValue: boolean; virtual;
    function TextHeight: integer;
    function AddedGlyphVisible: boolean;
    function AddedGlyphSpace: integer;
    function AddedGlyphRect: TRect;
    procedure PaintAddedGlyph;
    procedure PaintAddedGlyphStd;
    function TextRect: TRect; virtual;
    function CanMoveTextHint(InFocusing: boolean): boolean; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Change; override;
    procedure PaintBorder(DC: hdc); virtual;
    function PrepareCache: boolean; virtual;
    procedure PaintText; virtual;
    procedure PaintBtn; virtual;
    procedure OurPaintHandler(aDC: hdc = 0); virtual;
    function BordWidth: integer;
    procedure PaddingChanged(Sender: TObject);
  public
    function CurrentState: integer; virtual;
    procedure AfterConstruction; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ValidateEdit; override;
    procedure Loaded; override;
    procedure WndProc(var Message: TMessage); override;
    property CheckOnExit: boolean read FCheckOnExit write FCheckOnExit;
    property OnValidateError: TValidateErrorEvent read FOnValidateError write FOnValidateError;
  published
    property Align;
{$IFDEF D2010}
    property Alignment;
{$ENDIF}
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Constraints;
    property Ctl3D;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
{$IFDEF DELPHI_XE3}
    property StyleElements;
{$ENDIF}
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  {$IFDEF D2005}
    property OnMouseActivate;
  {$ENDIF}
  {$IFNDEF D2009}
    property TextHint: acString read FTextHint write SetTextHint;
  {$ELSE}
    property ParentDoubleBuffered;
    property OnMouseEnter;
    property OnMouseLeave;
  {$ENDIF}
  {$IFDEF D2010}
    property Touch;
    property OnGesture;
  {$ENDIF}
    property Color stored ColorStored;
    property Font stored FontStored;
    property ParentFont stored FontStored;
{$ENDIF} // NOTFORHELP
    property AddedGlyph: TacAddedGlyph read FAddedGlyph write FAddedGlyph;
    property BoundLabel: TsBoundLabel read FBoundLabel write FBoundLabel;
    property DisabledKind: TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
    property SkinData: TsCtrlSkinData read FCommonData write FCommonData;
    property Padding: TacPadding read FPadding write FPadding;
    property VerticalAlignment: TVerticalAlignment read FVerticalAlignment write SetVerticalAlignment default taAlignTop;
    property LabelFromTextHint: boolean read FLabelFromTextHint write FLabelFromTextHint default False;
  end;

{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsMaskEdit = class(TacCustomMaskEdit)
  published
    property CheckOnExit;
    property EditMask;
    property Text;
    property TextHint;
    property HideSelection;
    property PasswordChar;
    property ReadOnly;
    property OnValidateError;
  end;


implementation

uses
  ExtCtrls,
  {$IFDEF DELPHI7UP} Themes, {$ENDIF}
  {$IFDEF TNTUNICODE} TntStdCtrls, {$ENDIF}
  acAnimation, sStyleSimply, sVCLUtils, sMessages, acntUtils, sGraphUtils, sAlphaGraph, sThirdParty, acAlphaImageList;

function TacCustomMaskEdit.AddedGlyphRect: TRect;
var
  bWidth: integer;
  ScaledPadding: TRect;
begin
  bWidth := BordWidth;
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


function TacCustomMaskEdit.AddedGlyphSpace: integer;
begin
  Result := integer(AddedGlyphVisible) * (FAddedGlyph.Width + SkinData.CommonSkinData.Spacing div 2);
end;


function TacCustomMaskEdit.AddedGlyphVisible: boolean;
begin
  Result := Assigned(AddedGlyph.Images) and IsValidIndex(AddedGlyph.ImageIndex, GetImageCount(AddedGlyph.Images));
end;


procedure TacCustomMaskEdit.AfterConstruction;
begin
  inherited AfterConstruction;
  FCommonData.Loaded;
end;


function TacCustomMaskEdit.AllowBtnStyle: Boolean;
begin
  Result := False;
end;


constructor TacCustomMaskEdit.Create(AOwner: TComponent);
begin
  FCommonData := TsCtrlSkinData.Create(Self, True);
  FCommonData.COC := COC_TsEdit;
  inherited Create(AOwner);
  FAddedGlyph := TacAddedGlyph.Create(Self, FCommonData);
  FAddedGlyphRect.Left := -1;
  FCheckOnExit := True;
  FContentHidden := False;
  FCommonData.CtrlSkinState := FCommonData.CtrlSkinState or ACS_NCCHANGED;
  FDisabledKind := DefDisabledKind;
  FBoundLabel := TsBoundLabel.Create(Self, FCommonData);
  FPadding := TacPadding.Create(Self, PaddingChanged);
  FVerticalAlignment := taAlignTop;
{$IFNDEF D2009}
  FTextHint := '';
{$ENDIF}
end;


function TacCustomMaskEdit.CurrentState: integer;
begin
  Result := integer(ControlIsActive(FCommonData));
end;


{$IFNDEF D2009}
procedure TacCustomMaskEdit.SetTextHint(const Value: acString);
begin
  if FTextHint <> Value then begin
    FTextHint := Value;
    SkinData.Invalidate;
  end;
end;
{$ENDIF}


destructor TacCustomMaskEdit.Destroy;
begin
  FreeAndNil(FBoundLabel);
  FreeAndNil(FCommonData);
  FreeAndNil(FAddedGlyph);
  FPadding.Free;
  inherited Destroy;
end;


procedure TacCustomMaskEdit.Loaded;
begin
  inherited Loaded;
  FCommonData.Loaded;
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);
end;


procedure TacCustomMaskEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (FAddedGlyph <> nil) and (AComponent = FAddedGlyph.Images) then
    FAddedGlyph.Images := nil;
end;


procedure TacCustomMaskEdit.OurPaintHandler(aDC: hdc = 0);
var
  DC, SavedDC: hdc;
  PS: TPaintStruct;
begin
  if not InAnimationProcess or (aDC = 0) then
    BeginPaint(Handle, PS);

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
      if not InUpdating(FCommonData) then begin
        FCommonData.BGChanged := FCommonData.BGChanged or FCommonData.HalfVisible or GetBoolMsg(Parent, AC_GETHALFVISIBLE);
        FCommonData.HalfVisible := not RectInRect(Parent.ClientRect, BoundsRect);
        if FCommonData.BGChanged then
          if not PrepareCache then begin
            if aDC = 0 then begin
              RestoreDC(DC, SavedDC);
              ReleaseDC(Handle, DC);
            end;
            if not InAnimationProcess then
              EndPaint(Handle, PS);

            FCommonData.FUpdating := True;
            Exit;
          end;

        UpdateCorners(FCommonData, integer(ControlIsActive(SkinData)));
        BitBlt(DC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
      end;
    end;
  finally
    if aDC = 0 then begin
      RestoreDC(DC, SavedDC);
      ReleaseDC(Handle, DC);
    end;
    if not InAnimationProcess or (aDC = 0) then
      EndPaint(Handle, PS);
  end;
end;


procedure TacCustomMaskEdit.PaddingChanged(Sender: TObject);
begin
  if not (csLoading in ComponentState) then begin
    FCommonData.CtrlSkinState := FCommonData.CtrlSkinState or ACS_NCCHANGED;
    FCommonData.Invalidate(True);
  end;
end;


procedure TacCustomMaskEdit.PaintAddedGlyph;
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
  DrawData.ImgRect := R;
  DrawData.CommonSkinData := SkinData.CommonSkinData;
  DrawData.PPI := DrawData.CommonSkinData.PPI;
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


procedure TacCustomMaskEdit.PaintAddedGlyphStd;
var
  DC: hdc;
  bWidth: integer;
begin
  if not SkinData.Skinned and AddedGlyphVisible then begin
    bWidth := BordWidth;
    DC := GetWindowDC(Handle);
    try
      PrepareCache;
      if not SkinData.Skinned then begin
        ExcludeClipRect(DC, 0, 0, Width, bWidth);
        ExcludeClipRect(DC, 0, bWidth, bWidth, Height);
        ExcludeClipRect(DC, bWidth, Height - bWidth, Width, Height);
        ExcludeClipRect(DC, Width - bWidth, bWidth, Width, Height - bWidth);
      end;
      with SkinData.CommonSkinData do
        if IsRightToLeft then
          BitBlt(DC, AddedGlyphRect.Left - Spacing div 2, AddedGlyphRect.Top, WidthOf(AddedGlyphRect) + Spacing div 2, HeightOf(AddedGlyphRect), SkinData.FCacheBmp.Canvas.Handle,
                     AddedGlyphRect.Left - Spacing div 2, AddedGlyphRect.Top, SRCCOPY)
        else
          BitBlt(DC, AddedGlyphRect.Left, AddedGlyphRect.Top, WidthOf(AddedGlyphRect) + Spacing div 2, HeightOf(AddedGlyphRect), SkinData.FCacheBmp.Canvas.Handle,
                     AddedGlyphRect.Left, AddedGlyphRect.Top, SRCCOPY);
    finally
      ReleaseDC(Handle, DC);
    end;
  end;
end;


procedure TacCustomMaskEdit.PaintBorder(DC: hdc);
var
  R: TRect;
  NewDC, SavedDC: HDC;
begin
  if Assigned(Parent) and Visible and Parent.Visible and not (csCreating in ControlState) and (BorderStyle <> bsNone) then
    if not InUpdating(SkinData) then begin
      if DC = 0 then
        NewDC := GetWindowDC(Handle)
      else
        NewDC := DC;

      SavedDC := SaveDC(NewDC);
      try
        if FCommonData.BGChanged then
          PrepareCache;

        R := BodyRect;
        UpdateCorners(FCommonData, integer(ControlIsActive(SkinData)));
        BitBltBorder(NewDC, 0, 0, Width, R.Bottom, FCommonData.FCacheBmp.Canvas.Handle, 0, 0,
                     BordWidth + FAddedPadding.Left, BordWidth + FAddedPadding.Top, BordWidth + FAddedPadding.Right, BordWidth + FAddedPadding.Bottom);

        if AddedGlyphVisible then
          with SkinData.CommonSkinData do
            if IsRightToLeft then
              BitBlt(DC, BordWidth, 0, AddedGlyphSpace, Height, SkinData.FCacheBmp.Canvas.Handle, BordWidth, 0, SRCCOPY)
            else
              BitBlt(DC, AddedGlyphRect.Left - Spacing div 2, 0, AddedGlyphSpace + Spacing div 2, Height, SkinData.FCacheBmp.Canvas.Handle, AddedGlyphRect.Left - Spacing div 2, 0, SRCCOPY);
      finally
        RestoreDC(NewDC, SavedDC);
        if DC = 0 then
          ReleaseDC(Handle, NewDC);
      end;
    end;
end;


procedure TacCustomMaskEdit.PaintBtn;
begin
  //
end;


procedure TacCustomMaskEdit.PaintText;
var
  R: TRect;
  aText: acString;
  Flags: Cardinal;
  TranspMode: byte;
begin
  FCommonData.FCacheBMP.Canvas.Font.Assign(Font);
  TranspMode := GetTransMode(SkinData);
  R := TextRect;
  if TranspMode = TM_FULL then
    dec(R.Left, 2);

  Flags := DT_NOPREFIX or DT_EXTERNALLEADING or GetStringFlags(Self, {$IFDEF D2009}Alignment{$ELSE}taLeftJustify{$ENDIF}) or DT_ALPHATEXT * Cardinal(TranspMode = TM_FULL);
  if not IsDefaultValue then begin
    if PasswordChar <> #0 then
      acFillString(aText, Length(EditText), iff(PasswordChar = '*', CharBullet, acChar(PasswordChar)))
    else
      aText := Text;//EditText;

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


{$IFDEF DELPHI7UP}
const
  {$IFDEF DELPHI_XE2}
  EditStates: array[0..1] of TThemedEdit = (teEditBorderNoScrollNormal, teEditBorderNoScrollFocused);//teEditBorderNoScrollHot);
  {$ELSE}
  EditStates: array[0..1] of TThemedEdit = (teEditTextNormal, teEditTextHot);
  {$ENDIF}
{$ENDIF}


function TacCustomMaskEdit.PrepareCache: boolean;
var
  R: TRect;
  BGInfo: TacBGInfo;
{$IFDEF DELPHI7UP}
  cEdit: TThemedEdit;
  Button: TThemedButton;
  Details: TThemedElementDetails;
{$ENDIF}
begin
  if FCommonData.CtrlSkinState and ACS_NCCHANGED <> 0 then begin
    SkinData.BGChanged := True;
    FCommonData.CtrlSkinState := FCommonData.CtrlSkinState and not ACS_NCCHANGED;
    SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED or SWP_NOREDRAW);
  end;
  Result := True;
  BGInfo.BgType := btUnknown;
  GetBGInfo(@BGInfo, Parent);
  if BGInfo.BgType <> btNotReady then begin
    InitCacheBmp(SkinData);
    if SkinData.Skinned then begin
      if BorderStyle = bsSingle then
        PaintItem(FCommonData, BGInfoToCI(@BGInfo), True, CurrentState, BodyRect, Point(Left, Top), FCommonData.FCacheBmp, True)//False)
      else
        PaintItemBG(FCommonData, BGInfoToCI(@BGInfo), 0, BodyRect, Point(Left, Top), FCommonData.FCacheBmp, 0, 0);

      if AllowBtnStyle and (CurrentState < 2) {not Dropped down} then
        HandleLighting(SkinData, CurrentState);
    end
    else begin
{$IFDEF DELPHI7UP}
      if acThemesEnabled then begin
        R := BodyRect;
        if AllowBtnStyle then begin
          case CurrentState of
            ACHOT: Button := tbPushButtonHot;
            ACPRESSED: Button := tbPushButtonPressed
            else Button := tbPushButtonNormal;
          end;
          Details := acThemeServices.GetElementDetails(Button);
          // Parent background
          acThemeServices.DrawParentBackground(Handle, FCommonData.FCacheBmp.Canvas.Handle, @Details, True);
          InflateRect(R, 1, 1);
        end
        else begin
          if BorderStyle = bsSingle then begin
            cEdit := EditStates[integer(ControlIsActive(SkinData))];
          end
          else begin
            cEdit := {$IFDEF DELPHI_XE2} teBackgroundNormal {$ELSE} EditStates[integer(ControlIsActive(SkinData))] {$ENDIF};
          end;
          Details := acThemeServices.GetElementDetails(cEdit);
        end;
        acThemeServices.DrawElement(FCommonData.FCacheBmp.Canvas.Handle, Details, R);
        if Color <> clWindow then
          FillDC(FCommonData.FCacheBmp.Canvas.Handle, BodyRect, Color);
      end
      else
{$ENDIF}
      begin
        FillDC(FCommonData.FCacheBmp.Canvas.Handle, MkRect(Self), Color);
        if (BorderStyle = bsSingle) {$IFDEF DELPHI7UP}and (not AllowBtnStyle or not acThemesEnabled){$ENDIF} then
          if not Ctl3D then
            FillDCBorder(FCommonData.FCacheBmp.Canvas.Handle, MkRect(Self), 1, 1, 1, 1, clWindowFrame)
          else begin
            R := BodyRect;
            DrawEdge(FCommonData.FCacheBmp.Canvas.Handle, R, EDGE_SUNKEN, BF_RECT);
          end;
      end;
    end;
    PaintText;
    if AddedGlyphVisible then
      PaintAddedGlyph;

    PaintBtn;
    if not Enabled then
      BmpDisabledKind(FCommonData.FCacheBmp, FDisabledKind, Parent, BGInfoToCI(@BGInfo), Point(Left, Top));

    SkinData.BGChanged := False;
  end
  else
    Result := False;
end;


procedure TacCustomMaskEdit.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    FCommonData.Invalidate;
  end;
end;


procedure TacCustomMaskEdit.SetVerticalAlignment(const Value: TVerticalAlignment);
begin
  if FVerticalAlignment <> Value then begin
    FVerticalAlignment := Value;
    PaddingChanged(Self);
  end;
end;


function TacCustomMaskEdit.TextHeight: integer;
begin
  Result := GetFontHeight(Font.Handle);
end;


procedure TacCustomMaskEdit.TextPaintPrevent(Locked: boolean);
begin
//
end;


function TacCustomMaskEdit.TextRect: TRect;
var
  bw: integer;
begin
  bw := BordWidth;
  if IsRightToLeft then
    Result := Rect(bw, bw, Width - bw - AddedGlyphSpace, Height - bw)
  else
    Result := Rect(AddedGlyphSpace + bw, bw, Width - bw, Height - bw);

  if AllowBtnStyle then
    InflateRect(Result, -1, 0);

  AddPadding(Result, FAddedPadding);
end;


procedure TacCustomMaskEdit.WMNCCalcSize(var Message: TWMNCCalcSize);
var
  dh: integer;
begin
  inherited;
  if not (csLoading in ComponentState) then begin
    // Add custom padding
    FAddedPadding := PaddingToRect(Padding, SkinData.CommonSkinData.PPI);
    // Calc vertical padding
    if FVerticalAlignment <> taAlignTop then
      case FVerticalAlignment of
        taVerticalCenter: begin
          dh := Height - TextHeight - FAddedPadding.Top - FAddedPadding.Bottom;
          inc(FAddedPadding.Top, dh div 2);
          inc(FAddedPadding.Bottom, dh div 2);
        end;
        taAlignBottom: inc(FAddedPadding.Top,  Height - TextHeight - FAddedPadding.Top - FAddedPadding.Bottom);
      end;
    // Change inherited NC rect
    AddPadding(Message.CalcSize_Params.rgrc[0], FAddedPadding);
    // Add horz padding to  inherited NC rect
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

    if FContentHidden then begin
      Message.CalcSize_Params.rgrc[0].Left := Width;
      Message.CalcSize_Params.rgrc[0].Right := Message.CalcSize_Params.rgrc[0].Left;
    end;
  end
  else
    FAddedPadding := MkRect;
end;


procedure TacCustomMaskEdit.WMNCHitTest(var Message: TWMNCHitTest);
begin
  inherited;
  Message.Result := HTCLIENT;
end;


procedure TacCustomMaskEdit.WndProc(var Message: TMessage);
var
  PS: TPaintStruct;
  DC, SavedDC: hdc;
  bw: integer;
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_SETSCALE: begin
          FCommonData.CtrlSkinState := FCommonData.CtrlSkinState or ACS_NCCHANGED;
          CommonMessage(Message, FCommonData);
          SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);
          if BoundLabel <> nil then
            BoundLabel.UpdateScale(Message.LParam);

          Exit;
        end;

        AC_CTRLHANDLED: begin
          Message.Result := 1;
          Exit;
        end; // AlphaSkins supported

        AC_SETNEWSKIN:
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
            CommonMessage(Message, FCommonData);
            Exit;
          end;

        AC_REFRESH:
          if RefreshNeeded(SkinData, Message) then begin
            CommonMessage(Message, FCommonData);
            if HandleAllocated then
              SendMessage(Handle, WM_NCPaint, 0, 0);

            Repaint;
            Exit;
          end;

        AC_REMOVESKIN:
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
            CommonMessage(Message, FCommonData);
            Invalidate;
            Exit
          end;

        AC_ENDPARENTUPDATE:
          if FCommonData.Updating then begin
            FCommonData.Updating := False;
            InvalidateRect(Handle, nil, True);
            SendMessage(Handle, WM_NCPAINT, 0, 0);
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
      if not CommonWndProc(Message, FCommonData) then begin
        if AllowBtnStyle then
          SetRedraw(Handle, 0);

        if AutoSelect and ReadOnly then begin
          AutoSelect := False;
          TextPaintPrevent(True);
          inherited;
          TextPaintPrevent(False);
          AutoSelect := True;
        end
        else
          inherited;

        if AllowBtnStyle then begin
          SetRedraw(Handle, 1);
          RedrawWindow(Handle, nil, 0, RDWA_ALLNOW);
        end;
      end;

      if (SkinData.CtrlSkinState and ACS_FOCUSCHANGING = 0) {and not IsDropDownList }then
        if LabelFromTextHint and IsDefaultValue and CanMoveTextHint(True) then
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

  if (FCommonData = nil) or not FCommonData.Skinned or not ControlIsReady(Self) then
    case Message.Msg of
      WM_PRINT:
        PaintTo(TWMPaint(Message).DC, 0, 0);

      CM_MOUSEENTER: begin
        SkinData.FMouseAbove := True;
        inherited;
      end;

      WM_KILLFOCUS: begin
        if not (csDestroying in ComponentState) then begin
          SkinData.FFocused := False;
          Perform(WM_NCPAINT, 0, 0);
        end;
        inherited;
      end;

      CM_MOUSELEAVE: begin
        SkinData.FMouseAbove := False;
        inherited;
      end;

      WM_ERASEBKGND:
        if AllowBtnStyle then
          Exit;

      WM_NCPAINT:
        if not SkinData.Skinned then begin
          if (Ctl3D or (BorderStyle <> bsSingle)) then begin
            DC := GetWindowDC(Handle);
            try
              PrepareCache;
              BitBltBorder(DC, 0, 0, SkinData.FCacheBmp.Width, SkinData.FCacheBmp.Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, BordWidth)
            finally
              ReleaseDC(Handle, DC);
            end;
          end;

          PaintAddedGlyphStd;
        end
        else
          if AllowBtnStyle then
            Exit;

      WM_PAINT:
        if SkinData.BGChanged then begin
          PrepareCache;
          if not Focused or (csDesigning in ComponentState) then begin
            BeginPaint(Handle, PS);
            if not Ctl3D and (BorderStyle = bsSingle) then
              bw := 0
            else
              bw := integer(BorderStyle <> bsNone) * 2;

            DC := GetWindowDC(Handle);
            try
              BitBlt(DC, bw, bw, SkinData.FCacheBmp.Width - 2 * bw, BodyRect.Bottom - 2 * bw, SkinData.FCacheBmp.Canvas.Handle, bw, bw, SRCCOPY);
            finally
              ReleaseDC(Handle, DC);
            end;
            EndPaint(Handle, PS);
          end
          else
            inherited;
        end
        else
          inherited
      else
        inherited
    end
  else begin
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

      WM_ERASEBKGND, CN_DRAWITEM: begin
        if not InAnimationProcess and not InUpdating(SkinData) then
          inherited;

        Exit;
      end;

      WM_NCPAINT: begin
        if not InAnimationProcess and not InUpdating(SkinData) then begin
          DC := GetWindowDC(Handle);
          SavedDC := SaveDC(DC);
          try
            PaintBorder(DC);
          finally
            RestoreDC(DC, SavedDC);
            ReleaseDC(Handle, DC);
          end;
        end;
        Exit;
      end;

      WM_PRINT: begin
        if FCommonData.CtrlSkinState and ACS_NCCHANGED <> 0 then begin
          FCommonData.CtrlSkinState := FCommonData.CtrlSkinState and not ACS_NCCHANGED;
          SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);
        end;
        SkinData.Updating := False;
        DC := TWMPaint(Message).DC;
        if SkinData.BGChanged then
          PrepareCache;

        UpdateCorners(SkinData, integer(ControlIsActive(SkinData)));
        bw := BordWidth;
        OurPaintHandler(DC);
        BitBltBorder(DC, 0, 0, SkinData.FCacheBmp.Width, SkinData.FCacheBmp.Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, bw);
        Exit;
      end;

      WM_PAINT: begin
        if FCommonData.CtrlSkinState and ACS_NCCHANGED <> 0 then begin
          SkinData.BGChanged := True;
          FCommonData.CtrlSkinState := FCommonData.CtrlSkinState and not ACS_NCCHANGED;
          SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);
          PrepareCache;
        end;
        if not Focused or (SkinData.CtrlSkinState and ACS_TEXTANIMATING <> 0) then begin
          OurPaintHandler(TWMPaint(Message).DC);
          if not (Enabled and ControlIsActive(FCommonData)) and (csDesigning in ComponentState) then
            inherited;
        end
        else begin
          inherited;
          PaintBorder(0);
        end;
        Exit;
      end;

      CM_COLORCHANGED, CM_CHANGED:
        FCommonData.BGChanged := True;

      WM_SETTEXT:
        FCommonData.BGChanged := True;
    end;

    if CommonWndProc(Message, FCommonData) then
      Exit;

    inherited;
    case Message.Msg of
      WM_SIZE: begin
        if FVerticalAlignment <> taAlignTop then
          FCommonData.CtrlSkinState := FCommonData.CtrlSkinState or ACS_NCCHANGED;

        SendMessage(Handle, WM_NCPAINT, 0, 0);
      end;

      CM_VISIBLECHANGED, WM_SETFONT: begin
        if FVerticalAlignment <> taAlignTop then
          FCommonData.CtrlSkinState := FCommonData.CtrlSkinState or ACS_NCCHANGED;

        FCommonData.Invalidate;
      end;
    end;
  end;
  if Assigned(BoundLabel) then
    BoundLabel.HandleOwnerMsg(Message, Self);
end;


function TacCustomMaskEdit.CanMoveTextHint(InFocusing: boolean): boolean;
begin
  Result := True;
end;


procedure TacCustomMaskEdit.Change;
begin
  if not (csLoading in ComponentState) then
    inherited;
end;


function TacCustomMaskEdit.ColorStored: boolean;
begin
  Result := not SkinData.Skinned or SkinData.CustomColor;
end;


function TacCustomMaskEdit.BodyRect: TRect;
begin
  Result := MkRect(Self);
end;


function TacCustomMaskEdit.BordWidth: integer;
begin
  Result := integer(BorderStyle <> bsNone) * (2 + integer(Ctl3d));
end;


function TacCustomMaskEdit.FontStored: boolean;
begin
  Result := IsCustomFont(Self, Font, not SkinData.Skinned or SkinData.CustomFont);
end;


function TacCustomMaskEdit.IsDefaultValue: boolean;
begin
  Result := Text = '';
end;


procedure TacCustomMaskEdit.ValidateEdit;
var
  sText: string;
begin
  if FCheckOnExit then
    try
      inherited ValidateEdit;
    except
      on E: EDBEditError do begin
        sText := E.Message;
        if Assigned(FOnValidateError) then
          FOnValidateError(Self, sText);

        raise EDBEditError.Create(sText);
      end;
    end;
end;

end.
