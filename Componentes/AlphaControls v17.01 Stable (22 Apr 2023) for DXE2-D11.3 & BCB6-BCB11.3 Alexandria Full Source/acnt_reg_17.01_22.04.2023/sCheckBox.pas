unit sCheckBox;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, imglist, StdCtrls,
  {$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  {$IFDEF TNTUNICODE} TntControls, TntActnList, TntForms, TntClasses, {$ENDIF}
  sCommonData, sConst, sDefaults, acAnimation;


type
{$IFNDEF NOTFORHELP}
  TsImageIndex = integer;
{$ENDIF} // NOTFORHELP


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsCheckBox = class(TCustomCheckBox)
{$IFNDEF NOTFORHELP}
  private
    FPressed,
    FShowFocus,
    FInCheckDrawing,
    FAnimCheckDrawing: boolean;

    FMargin,
    FTextIndent: integer;

    FGlyphChecked,
    FGlyphUnChecked: TBitmap;

    FImgGrayed,
    FImgChecked,
    FImgUnchecked: TsImageIndex;

    FVerticalAlign: TvaAlign;
    FCommonData: TsCommonData;
    FImages: TCustomImageList;
    FDisabledKind: TsDisabledKind;
    FImageChangeLink: TChangeLink;
    FAnimatEvents: TacAnimatEvents;
    FOnValueChanged: TNotifyEvent;
    FMouseInGlyph: boolean;
{$IFNDEF DELPHI7UP}
    FWordWrap: boolean;
    procedure SetWordWrap      (const Value: boolean);
{$ENDIF}
    procedure SetDisabledKind  (const Value: TsDisabledKind);
    procedure SetGlyphChecked  (const Value: TBitmap);
    procedure SetGlyphUnChecked(const Value: TBitmap);
    procedure SetTextIndent    (const Value: integer);
    procedure SetShowFocus     (const Value: Boolean);
    procedure SetMargin        (const Value: integer);
    procedure SetReadOnly      (const Value: boolean);
    procedure SetImages        (const Value: TCustomImageList);
    procedure SetVerticalAlign (const Value: TvaAlign);
    procedure SetImageIndex    (const Index: Integer; const Value: TsImageIndex);
{$IFDEF TNTUNICODE}
    procedure SetHint          (const Value: WideString);
    procedure SetCaption       (const Value: TWideCaption);
    function GetCaption:      TWideCaption;
    function GetHint:         WideString;
    function IsHintStored:    boolean;
    function IsCaptionStored: boolean;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
{$ENDIF}
    procedure ImageListChange(Sender: TObject);
    procedure SetMouseInGlyph(const Value: boolean);
    function GlyphScreenRect(SkipClickArea: boolean): TRect;
    function GetShowFocus: boolean;
  protected
    FReadOnly: boolean;
    function GetReadOnly: boolean;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure SetChecked(Value: Boolean); override;
    procedure PaintHandler(M: TWMPaint);
    procedure StdPaintHandler(M: TWMPaint);
    procedure PaintControl(DC: HDC);
    procedure StdPaintControl(aDC: HDC);
    procedure PaintStdBG;
    procedure DrawCheckText;
    procedure DrawCheckArea;
    procedure DrawSkinGlyph(i: integer);
    function PaintCtrlState: integer;
    procedure PaintGlyph(Bmp: TBitmap; const Index: integer);
    function SkinGlyphHeight(i: integer): integer;
    function SkinGlyphWidth (i: integer): integer;
    function SkinCheckRect  (i: integer): TRect;
    function Glyph(IsChecked: boolean): TBitmap;
    function CheckSize: TSize;
    function CustomGlyphSize: TSize;
    function PrepareCache: boolean;
    function ActState: TCheckBoxState; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
{$IFDEF TNTUNICODE}
    procedure CreateWindowHandle(const Params: TCreateParams); override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetActionLinkClass: TControlActionLinkClass; override;
{$ENDIF}
  public
    function CheckRect(LeftAligned: boolean): TRect;
    function GetControlsAlignment: TAlignment; override;
    procedure AfterConstruction; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure WndProc(var Message: TMessage); override;
    property MouseInGlyph: boolean read FMouseInGlyph write SetMouseInGlyph;
  published
{$IFDEF TNTUNICODE}
    property Caption: TWideCaption read GetCaption write SetCaption stored IsCaptionStored;
    property Hint: WideString read GetHint write SetHint stored IsHintStored;
{$ELSE}
    property Caption;
{$ENDIF}
    property Action;
    property Align;
    property Alignment;
    property AllowGrayed;
    property Anchors;
    property AutoSize default True;
    property BiDiMode;
    property Checked;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property State;
{$IFDEF DELPHI_XE3}
    property StyleElements;
{$ENDIF}
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
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
    property Margin: integer read FMargin write SetMargin default 0;
{$ENDIF} // NOTFORHELP
    property ImgChecked:   TsImageIndex Index 0 read FImgChecked   write SetImageIndex default 0;
    property ImgUnchecked: TsImageIndex Index 1 read FImgUnchecked write SetImageIndex default 0;
    property ImgGrayed:    TsImageIndex Index 2 read FImgGrayed    write SetImageIndex default 0;

    property GlyphChecked:   TBitmap read FGlyphChecked   write SetGlyphChecked;
    property GlyphUnChecked: TBitmap read FGlyphUnChecked write SetGlyphUnChecked;

    property AnimatEvents: TacAnimatEvents  read FAnimatEvents write FAnimatEvents   default [aeGlobalDef];
    property SkinData:     TsCommonData     read FCommonData   write FCommonData;
    property DisabledKind: TsDisabledKind   read FDisabledKind write SetDisabledKind default DefDisabledKind;
    property Images:       TCustomImageList read FImages       write SetImages;
    property AnimCheckDrawing: boolean  read FAnimCheckDrawing write FAnimCheckDrawing default True;
    property ReadOnly:     boolean          read GetReadOnly   write SetReadOnly     default False;
    property ShowFocus:    boolean          read GetShowFocus  write SetShowFocus    default True;
    property TextIndent:   integer          read FTextIndent   write SetTextIndent   default 4;
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
  end;


{$IFNDEF NOTFORHELP}
var
  PaintState: integer = -1; // Used in the TsDBCheckBox control
{$ENDIF}

implementation

uses Math,
  {$IFDEF CHECKXP}UxTheme, {$ENDIF}
  {$IFDEF DELPHI7UP}Themes, {$ENDIF}
  acntTypes, sGraphUtils, acntUtils, sAlphaGraph, sVclUtils, sStylesimply,
  acGlow, acAlphaImageList, sMaskData, sMessages, sSKinManager, sThirdParty, acThdTimer;


procedure TsCheckBox.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
{$IFDEF TNTUNICODE}
  TntControl_BeforeInherited_ActionChange(Self, Sender, CheckDefaults);
{$ENDIF}
  FCommonData.BGChanged := True;
  inherited;
  Repaint;
end;


function TsCheckBox.ActState: TCheckBoxState;
begin
  if FInCheckDrawing then
    Result := cbUnChecked
  else
    Result := State;
end;


procedure TsCheckBox.AfterConstruction;
begin
  inherited;
  SkinData.Loaded;
end;


function TsCheckBox.GetControlsAlignment: TAlignment;
begin
  if not UseRightToLeftAlignment then
    Result := Alignment
  else
    if Alignment = taRightJustify then
      Result := taLeftJustify
    else
      Result := taRightJustify;
end;


function TsCheckBox.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
const
  iFocusRectW = 4;
  iFocusRectH = 4;
var
  ss, cSize: TSize;
  w, h, m: integer;
begin
  Result := False;

  if not (csLoading in ComponentState) and AutoSize then begin
    cSize := CheckSize;
    Result := True;
    if Caption <> '' then begin
      m := cSize.cx + 2 * Margin + FTextIndent + SkinData.CommonSkinData.Spacing div 2 + iFocusRectW;
      NewWidth := NewWidth - m;
      if Align in [alClient, alTop, alBottom] then
        ss := GetStringSize(Font.Handle, Caption, DT_WORDBREAK, WordWrap, NewWidth)
      else begin
        ss := GetStringSize(Font.Handle, Caption, DT_WORDBREAK, WordWrap);
        NewWidth := ss.cx + m;
      end;
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


function TsCheckBox.CheckRect(LeftAligned: boolean): TRect;
var
  cSize: TSize;

  function GetGlyphTop: integer;
  begin
    case FVerticalAlign of
      vaTop:    Result := Margin;
      vaMiddle: Result := (Height - cSize.cy) div 2
      else      Result := Height - cSize.cy - Margin;
    end;
  end;

begin
  cSize := CheckSize;
  Result.Top := GetGlyphTop;
  Result.Left := Margin;
  Result.Bottom := Result.Top + cSize.cy;
  Result.Right := Result.Left + cSize.cx;
  if not LeftAligned and (GetControlsAlignment = taLeftJustify) then
    ReflectRect(Result, Width, 0, True);
end;


function TsCheckBox.CheckSize: TSize;
begin
  if Assigned(Images) and (FImgChecked >= 0) and (FImgUnChecked >= 0) or (FGlyphChecked.Width > 0) then
    Result := MkSize(CustomGlyphSize.cx, CustomGlyphSize.cy) // If custom glyph
  else
    if (FCommonData.Skinned) and FCommonData.SkinManager.IsValidImgIndex(FCommonData.CommonSkinData.CheckBox[State]) then
      Result := MkSize(SkinGlyphWidth(FCommonData.CommonSkinData.CheckBox[State]), SkinGlyphHeight(FCommonData.CommonSkinData.CheckBox[State]))
    else
{$IFDEF DELPHI7_UP}
      if acThemesEnabled then
        Result := MkSize(ScaleInt(16, SkinData), ScaleInt(16, SkinData))
      else
{$ENDIF}
        Result := MkSize(CheckBoxWidth(TacObject(SkinData)), CheckBoxHeight(TacObject(SkinData)));
end;


{$IFDEF TNTUNICODE}
procedure TsCheckBox.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsWideCharAccel(Message.CharCode, Caption) and CanFocus then begin
      SetFocus;
      if Focused then
        Toggle;
        
      Result := 1;
    end
    else
      Broadcast(Message);
end;
{$ENDIF}


constructor TsCheckBox.Create(AOwner: TComponent);
begin
  FCommonData := TsCommonData.Create(Self, False);
  FCommonData.COC := COC_TsCheckBox;
  inherited Create(AOwner);
  FMargin := 0;
  FShowFocus := True;
  FTextIndent := 4;
  FInCheckDrawing := False;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FDisabledKind := DefDisabledKind;
  FGlyphChecked := TBitmap.Create;
  FGlyphUnChecked := TBitmap.Create;
  FAnimatEvents := [aeGlobalDef];
  FVerticalAlign := vaMiddle;
  FAnimCheckDrawing := True;
  FImgChecked := 0;
  FImgUnChecked := 0;
  FImgGrayed := 0;
{$IFNDEF DELPHI7UP}
  FWordWrap := False;
{$ELSE}
  WordWrap := False;
{$ENDIF}
  FPressed := False;
  AutoSize := True;
end;

function TsCheckBox.CustomGlyphSize: TSize;
var
  glBmp: TBitmap;
begin
  if Assigned(Images) and (FImgChecked >= 0) and (FImgUnChecked >= 0) then begin
    Result.cy := GetImageHeight(Images, iff(Checked, FImgChecked, FImgUnChecked));
    if Images.Width mod Images.Height = 0 then
      Result.cx := GetImageWidth(Images, iff(Checked, FImgChecked, FImgUnChecked)) div (Images.Width div Images.Height)
    else
      Result.cx := GetImageWidth(Images, iff(Checked, FImgChecked, FImgUnChecked))
  end
  else begin
    glBmp := Glyph(Checked);
    if glBmp <> nil then begin
      Result.cy := glBmp.Height;
      if glBmp.Width mod glBmp.Height = 0 then
        Result.cx := glBmp.Width div (glBmp.Width div glBmp.Height)
      else
        Result.cx := glBmp.Width
    end
    else
      Result := MkSize(16, 16);
  end;

  Result.cx := Result.cx * GetPPI(SkinData) div 96;
  Result.cy := Result.cy * GetPPI(SkinData) div 96;
end;


{$IFDEF TNTUNICODE}
procedure TsCheckBox.CreateWindowHandle(const Params: TCreateParams);
begin
  CreateUnicodeHandle(Self, Params, 'BUTTON');
end;


procedure TsCheckBox.DefineProperties(Filer: TFiler);
begin
  inherited;
  TntPersistent_AfterInherited_DefineProperties(Filer, Self);
end;
{$ENDIF}


destructor TsCheckBox.Destroy;
begin
  FImageChangeLink.Free;
  FreeAndNil(FCommonData);
  FreeAndNil(FGlyphChecked);
  FreeAndNil(FGlyphUnChecked);
  inherited Destroy;
end;


procedure TsCheckBox.DrawCheckArea;
var
  i, ImgIndex, GlyphCount, GlyphIndex: integer;
  glBmp, TempBmp: TBitmap;
  R: TRect;
begin
  if Assigned(Images) and (FImgChecked >= 0) and (FImgUnChecked >= 0) then begin
    case ActState of
      cbChecked:      ImgIndex := FImgChecked;
      cbUnChecked:    ImgIndex := FImgUnChecked
      else {cbGrayed} ImgIndex := FImgGrayed;
    end;
    if ImgIndex >= 0 then begin
      SetImagesPPI(Images, GetPPI(SkinData));
      R := CheckRect(False);
      GlyphCount := Images.Width div Images.Height;
      if (GlyphCount > 1) and (Images.Width mod Images.Height = 0) then begin // If complex image
        TempBmp := TBitmap.Create;
        if Images is TsAlphaImageList then begin
          if not TsAlphaImageList(Images).GetBitmap32(ImgIndex, TempBmp) then
            Exit
        end
        else
  {$IFDEF DELPHI5}
          Images.GetBitmap(ImgIndex, TempBmp);
  {$ELSE}
          if not Images.GetBitmap(ImgIndex, TempBmp) then
            Exit;
  {$ENDIF}
        if FPressed then
          GlyphIndex := min(2, GlyphCount - 1)
        else
          if ControlIsActive(FCommonData) and not ReadOnly then
            GlyphIndex := min(1, GlyphCount - 1)
          else
            GlyphIndex := 0;

        PaintGlyph(TempBmp, GlyphIndex);
        FreeAndNil(TempBmp);
      end
      else begin
        if SkinData.Skinned then
          FCommonData.FCacheBmp.Canvas.Font.Color := GetFontColor(Self, SkinData.SkinIndex, FCommonData.SkinManager, PaintCtrlState)
        else
          FCommonData.FCacheBmp.Canvas.Font.Color := Font.Color;

        Images.Draw(FCommonData.FCacheBmp.Canvas, R.Left, R.Top, ImgIndex, True);
      end;
    end;
  end
  else begin
    glBmp := Glyph(ActState = cbChecked);
    if glBmp <> nil then begin
      GlyphCount := max(1, glBmp.Width div glBmp.Height);
      if FPressed then
        GlyphIndex := min(2, GlyphCount - 1)
      else
        if ControlIsActive(FCommonData) and not ReadOnly then
          GlyphIndex := min(1, GlyphCount - 1)
        else
          GlyphIndex := 0;

      if glBmp.Width <> 0 then
        PaintGlyph(glBmp, GlyphIndex);
    end
    else
      if SkinData.Skinned then begin
        if PaintState >= 0 then
          i := FCommonData.CommonSkinData.CheckBox[sConst.CheckBoxStates[PaintState]]
        else
          i := FCommonData.CommonSkinData.CheckBox[ActState];

        if SkinData.SkinManager.IsValidImgIndex(i) then
          DrawSkinGlyph(i);
      end
      else
        acDrawCheck(CheckRect(False), ActState, Enabled, FCommonData.FCacheBmp, EmptyCI, FCommonData.CommonSkinData, False, PaintCtrlState);
  end;
end;


procedure TsCheckBox.DrawCheckText;
var
  cSize: TSize;
  rText: TRect;
  TranspMode: byte;
  Fmt, State, w, h: integer;
begin
  cSize := CheckSize;
  rText := MkRect(Width - (cSize.cx + FTextIndent + 2 * Margin + 4 + SkinData.CommonSkinData.Spacing div 2), 0);
  TranspMode := GetTransMode(SkinData);
  Fmt := DT_CALCRECT or TextWrapping[WordWrap] or DT_ALPHATEXT * Cardinal(TranspMode = TM_FULL);
  AcDrawText(FCommonData.FCacheBMP.Canvas.Handle, Caption, rText, Fmt);
  h := HeightOf(rText);
  w := WidthOf(rText);
  rText.Top := Max((Height - h) div 2, Margin);
  rText.Bottom := rText.Top + h;
  rText.Left := Margin + cSize.cx + ScaleInt(FTextIndent, SkinData);
  rText.Right := rText.Left + w + 1;
  if Caption <> '' then begin
    OffsetRect(rText, -integer(WordWrap), -1);
    Fmt := TextWrapping[WordWrap] or Cardinal(UseRightToLeftReading) * DT_RTLREADING or DT_ALPHATEXT * Cardinal(TranspMode = TM_FULL);
    if GetControlsAlignment = taLeftJustify then
      ReflectRect(rText, Width, 0, True);

    acWriteTextEx(FCommonData.FCacheBmp.Canvas, PacChar(Caption), Enabled or (SkinData.SkinIndex >= 0), rText, Fmt, FCommonData, PaintCtrlState > 0);
    if TranspMode = TM_PARTIAL then
      FillAlphaRect(FCommonData.FCacheBmp, rText);
  end;
  if Focused and ShowFocus then begin
    FCommonData.FCacheBmp.Canvas.Pen.Style := psClear;
    FCommonData.FCacheBmp.Canvas.Brush.Style := bsSolid;
    inc(rText.Top);
    InflateRect(rText, 2, 1);
    rText.Left := max(0, rText.Left);
    rText.Top := max(0, rText.Top);
    rText.Right := min(Width, rText.Right);
    rText.Bottom := min(Height, rText.Bottom);
    if SkinData.Skinned then begin
      State := min(ac_MaxPropsIndex, SkinData.CommonSkinData.gd[SkinData.SkinIndex].States);
      FocusRect(SkinData.FCacheBmp.Canvas, rText, SkinData.CommonSkinData.gd[SkinData.SkinIndex].Props[State].FontColor.Color, clNone);
    end
{$IFDEF DELPHI_XE2}
    else
      DrawFocusRect(SkinData.FCacheBmp.Canvas.Handle, rText);
{$ENDIF}
  end;
end;


procedure TsCheckBox.DrawSkinGlyph(i: integer);
begin
  with FCommonData do
    if FCacheBmp.Width > 0 then
      sAlphaGraph.DrawSkinGlyph(FCacheBmp, SkinCheckRect(i).TopLeft, PaintCtrlState, 1, SkinData.CommonSkinData.ma[i], MakeCacheInfo(FCacheBmp))
end;


{$IFDEF TNTUNICODE}
function TsCheckBox.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TntControl_GetActionLinkClass(Self, inherited GetActionLinkClass);
end;


function TsCheckBox.GetCaption: TWideCaption;
begin
  Result := TntControl_GetText(Self)
end;


function TsCheckBox.GetHint: WideString;
begin
  Result := TntControl_GetHint(Self)
end;
{$ENDIF}


function TsCheckBox.GetReadOnly: boolean;
begin
  Result := FReadOnly;
end;


function TsCheckBox.GetShowFocus: boolean;
begin
  if csPaintCopy in ControlState then
    Result := False
  else
    Result := FShowFocus;
end;


procedure TsCheckBox.ImageListChange(Sender: TObject);
begin
  SkinData.Invalidate;
end;


{$IFDEF TNTUNICODE}
function TsCheckBox.IsCaptionStored: boolean;
begin
  Result := TntControl_IsCaptionStored(Self)
end;


function TsCheckBox.IsHintStored: boolean;
begin
  Result := TntControl_IsHintStored(Self)
end;
{$ENDIF}


procedure TsCheckBox.Loaded;
begin
  inherited;
  SkinData.Loaded;
  AdjustSize;
end;


procedure TsCheckBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = Images then
      Images := nil;
end;


procedure TsCheckBox.PaintControl(DC: HDC);
var
  Timer: TacThreadedTimer;
begin
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


procedure TsCheckBox.PaintGlyph(Bmp: TBitmap; const Index: integer);
var
  R: TRect;
  cSize: TSize;
begin
  if FCommonData.FCacheBmp.Width > 0 then begin
    R := CheckRect(False);
    cSize := MkSize(WidthOf(R), HeightOf(R));
    if Bmp.PixelFormat = pfDevice then
      Bmp.HandleType := bmDIB;

    if Bmp.PixelFormat <> pf32bit then begin
      Bmp.PixelFormat := pf32bit;
      FillAlphaRect(Bmp, MkRect(Bmp), MaxByte);
    end;
    CopyByMask(Rect(R.Left, R.Top, R.Right, R.Bottom), Rect(cSize.cx * Index, 0, cSize.cx * (Index + 1), cSize.cy), FCommonData.FCacheBmp, Bmp, EmptyCI, True);
  end;
end;


procedure TsCheckBox.PaintHandler(M: TWMPaint);
var
  PS: TPaintStruct;
  DC, SavedDC: hdc;
begin
  DC := M.DC;
  if (DC = 0) or (SkinData.CtrlSkinState and ACS_PRINTING = 0) and not (csPaintCopy in ControlState) then
    DC := BeginPaint(Handle, PS);

  SavedDC := SaveDC(DC);
  try
    if not InUpdating(FCommonData) then
      PaintControl(DC);
  finally
    RestoreDC(DC, SavedDC);
    if (M.DC = 0) or (SkinData.CtrlSkinState and ACS_PRINTING = 0) then
      EndPaint(Handle, PS);
  end;
end;


procedure TsCheckBox.PaintStdBG;
{$IFDEF DELPHI7UP}
  {$IFDEF DELPHI_XE2}
var
  Details: TThemedElementDetails;
  SavedDC: hdc;
  {$ENDIF}
{$ENDIF}
begin
  if ParentColor then begin
{$IFDEF DELPHI7UP}
    if acThemesEnabled then begin
    {$IFDEF DELPHI_XE2}
      // Transparent part drawing
      SavedDC := SaveDC(FCommonData.FCacheBmp.Canvas.Handle);
      try
        SelectClipRgn(FCommonData.FCacheBmp.Canvas.Handle, 0);
        Details := acThemeServices.GetElementDetails(ttBody);
        acThemeServices.DrawParentBackground(Handle, FCommonData.FCacheBmp.Canvas.Handle, @Details, False);
      finally
        RestoreDC(FCommonData.FCacheBmp.Canvas.Handle, SavedDC);
      end;
    {$ENDIF}
    end
    else
{$ENDIF}
      FillDC(FCOmmonData.FCacheBmp.Canvas.Handle, MkRect(Self), ColorToRGB(TacAccessControl(Parent).Color));
  end
  else
    FillDC(FCOmmonData.FCacheBmp.Canvas.Handle, MkRect(Self), ColorToRGB(Color));
end;


function TsCheckBox.PrepareCache: boolean;
var
  BGInfo: TacBGInfo;
begin
  Result := True;
  InitCacheBmp(SkinData);
  FCommonData.FCacheBmp.Canvas.Font.Assign(Font);
  FCommonData.FCacheBmp.Canvas.Lock;
  BGInfo.PleaseDraw := False;
  BGInfo.Offset := Point(Left, Top);
  BGInfo.R := MkRect(Self);
  GetBGInfo(@BGInfo, Parent);
  case BGInfo.BgType of
    btUnknown: begin // If parent is not AlphaControl
      BGInfo.Bmp := nil;
      BGInfo.BgType := btFill;
      BGInfo.Color := GetControlColor(Parent);
    end;
    btNotReady: begin
      Result := False;
      SkinData.FUpdating := True;
      Exit;
    end;
  end;
  FCommonData.FCacheBmp.Canvas.Unlock;
  if SkinData.Skinned then
    PaintItem(FCommonData, BGInfoToCI(@BGInfo), True, PaintCtrlState, MkRect(FCommonData.FCacheBmp), MkPoint(Self), FCommonData.FCacheBmp, True)
  else
    PaintStdBG;

  DrawCheckText;
  DrawCheckArea;
  if not Enabled and (SkinData.SkinIndex >= 0) then
    BmpDisabledKind(FCommonData.FCacheBmp, FDisabledKind, Parent, BGInfoToCI(@BGInfo), Point(Left, Top));

  FCommonData.BGChanged := False
end;


{$IFDEF TNTUNICODE}
procedure TsCheckBox.SetCaption(const Value: TWideCaption);
begin
  TntControl_SetText(Self, Value);
end;
{$ENDIF}


procedure TsCheckBox.SetChecked(Value: Boolean);
var
  b: boolean;
begin
  b := False;
  if not (csLoading in ComponentState) then begin
    if Value <> Checked then begin
      FCommonData.BGChanged := True;
      b := True;
    end;
    inherited;
    if FCommonData.BGChanged then
      Repaint;

    if b and Assigned(FOnValueChanged) then
      FOnValueChanged(self);
  end
  else
    inherited;
end;


{$IFNDEF DELPHI7UP}
procedure TsCheckBox.SetWordWrap(const Value: boolean);
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


procedure TsCheckBox.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    FCommonData.Invalidate;
  end;
end;


procedure TsCheckBox.SetGlyphChecked(const Value: TBitmap);
begin
  FGlyphChecked.Assign(Value);
  if AutoSize then
    AdjustSize;

  FCommonData.Invalidate;
end;


procedure TsCheckBox.SetGlyphUnChecked(const Value: TBitmap);
begin
  FGlyphUnChecked.Assign(Value);
  if AutoSize then
    AdjustSize;

  FCommonData.Invalidate;
end;


{$IFDEF TNTUNICODE}
procedure TsCheckBox.SetHint(const Value: WideString);
begin
  TntControl_SetHint(Self, Value);
end;
{$ENDIF}


procedure TsCheckBox.SetImageIndex(const Index: Integer; const Value: TsImageIndex);

  procedure ChangeProp(var Prop: TsImageIndex; Value: TsImageIndex);
  begin
    if Prop <> Value then begin
      Prop := Value;
      if AutoSize then
        AdjustSize;

      if Checked = boolean(Index) then
        SkinData.Invalidate;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FImgChecked,   Value);
    1: ChangeProp(FImgUnChecked, Value);
    2: ChangeProp(FImgGrayed,    Value);
  end;
end;


procedure TsCheckBox.SetImages(const Value: TCustomImageList);
begin
  if FImages <> Value then begin
    if Images <> nil then
      Images.UnRegisterChanges(FImageChangeLink);

    FImages := Value;
    if Images <> nil then begin
      Images.RegisterChanges(FImageChangeLink);
      Images.FreeNotification(Self);
    end;

    if AutoSize then
      AdjustSize;

    SkinData.Invalidate;
  end;
end;


procedure TsCheckBox.SetMargin(const Value: integer);
begin
  if FMargin <> Value then begin
    FMargin := Value;
    if AutoSize then
      AdjustSize;

    Invalidate;
  end;
end;


procedure TsCheckBox.SetMouseInGlyph(const Value: boolean);
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


procedure TsCheckBox.SetReadOnly(const Value: boolean);
begin
  FReadOnly := Value;
end;


procedure TsCheckBox.SetShowFocus(const Value: Boolean);
begin
  if FShowFocus <> Value then begin
    FShowFocus := Value;
    Invalidate;
  end;
end;


procedure TsCheckBox.SetTextIndent(const Value: integer);
begin
  if FTextIndent <> Value then begin
    FTextIndent := Value;
    if AutoSize then
      AdjustSize;

    Invalidate;
  end;
end;


function TsCheckBox.Glyph(IsChecked: boolean): TBitmap;
begin
  if IsChecked then
    Result := GlyphChecked
  else
    Result := GlyphUnChecked;

  if Result.Empty then
    Result := nil;
end;


function TsCheckBox.GlyphScreenRect(SkipClickArea: boolean): TRect;
var
  i: integer;
begin
  Result := CheckRect(False);
  if not SkipClickArea and (SkinData.SkinManager <> nil) and (SkinData.SkinManager.AnimEffects.Buttons.ClickEffect <> ceWaveOut) then begin
    i := MaxCircleRadius(SkinData) - WidthOf(Result) div 2;
    InflateRect(Result, i, i);
  end;
  with ClientToScreen(MkPoint) do
    OffsetRect(Result, X, Y);
end;


function TsCheckBox.SkinCheckRect(i: integer): TRect;
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
    Result := Rect(Margin, hdiv, Margin + w, h + hdiv)
  else
    Result := Rect(Width - w - Margin, hdiv, Width - Margin, h + hdiv);
end;


function TsCheckBox.SkinGlyphHeight(i: integer): integer;
begin
  Result := FCommonData.CommonSkinData.ma[i].Height;
end;


function TsCheckBox.SkinGlyphWidth(i: integer): integer;
begin
  Result := FCommonData.CommonSkinData.ma[i].Width;
end;


procedure TsCheckBox.StdPaintControl(aDC: HDC);
begin
  PrepareCache;
  BitBlt(aDC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
end;


procedure TsCheckBox.StdPaintHandler(M: TWMPaint);
var
  PS: TPaintStruct;
  DC, SavedDC: hdc;
begin
  DC := M.DC;
  if (DC = 0) or (SkinData.CtrlSkinState and ACS_PRINTING = 0) and not (csPaintCopy in ControlState) then
    DC := BeginPaint(Handle, PS);

  SavedDC := SaveDC(DC);
  try
    StdPaintControl(DC);
  finally
    RestoreDC(DC, SavedDC);
    if (M.DC = 0) or (SkinData.CtrlSkinState and ACS_PRINTING = 0) then
      EndPaint(Handle, PS);
  end;
end;


procedure TsCheckBox.WndProc(var Message: TMessage);
var
  b: boolean;
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
        end;

        AC_GETAPPLICATION: begin
          Message.Result := LRESULT(Application);
          Exit;
        end;

        AC_REMOVESKIN:
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
            CommonWndProc(Message, FCommonData);
            if HandleAllocated then
              SendMessage(Handle, BM_SETCHECK, LPARAM(State), 0);

            if not (csDesigning in ComponentState) and (uxthemeLib <> 0) then
              Ac_SetWindowTheme(Handle, nil, nil);

            AdjustSize;
            Repaint;
            Exit;
          end;

        AC_REFRESH:
          if RefreshNeeded(SkinData, Message) then begin
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

    CM_VISIBLECHANGED:
      SkinData.CommonSkinData := nil;

    CM_FONTCHANGED, CM_TEXTCHANGED:
      if AutoSize then begin
        if ([csDestroying, csLoading] * ComponentState = []) and (Parent <> nil) then
          HandleNeeded;

        if HandleAllocated then begin
          inherited;
          AdjustSize;
          Exit;
        end;
      end;

    WM_SETTEXT: if HandleAllocated and IsWindowVisible(Handle) then begin
      Perform(WM_SETREDRAW, 0, 0);
      inherited;
      Perform(WM_SETREDRAW, 1, 0);
      FCommonData.Invalidate;
      Exit;
    end;

    CM_MOUSEENTER: begin
      if Enabled and not (csDesigning in ComponentState) and not FCommonData.FMouseAbove then begin
        FCommonData.FMouseAbove := True;
        if CircleClickMode(Self, SkinData, AnimatEvents) then begin
          MouseInGlyph := PtInRect(GlyphScreenRect(False), acMousePos);
          ShowCircleIfNeeded(SkinData, MaxByte, True);
        end;
//        else
          DoChangePaint(FCommonData, 1, UpdateWindow_CB, EventEnabled(aeMouseEnter, FAnimatEvents), False);
      end;
      inherited;
      Exit;
    end;

    CM_MOUSELEAVE: begin
      if Enabled and not (csDesigning in ComponentState) and FCommonData.FMouseAbove then begin
        FCommonData.FMouseAbove := False;
        FPressed := False;
        if CircleClickMode(Self, SkinData, AnimatEvents) then
          MouseInGlyph := False;
//        else
          DoChangePaint(FCommonData, 0, UpdateWindow_CB, EventEnabled(aeMouseLeave, FAnimatEvents), False, False);
      end;
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
            Repaint;
        end;
        Exit;
      end;

    WM_KILLFOCUS, CM_EXIT:
      if ComponentState * [csDesigning, csDestroying] = [] then
        if Enabled then begin
          StopTimer(SkinData);
          SetRedraw(Handle);
          inherited;
          SetRedraw(Handle, 1);
          FCommonData.FFocused := False;
          FCommonData.FMouseAbove := False;
          FCommonData.Invalidate;
          Exit
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
              if FCommonData.FUpdating or FCommonData.HalfVisible then
                if not (csDesigning in ComponentState) and not InUpdating(FCommonData, True) then
                  Repaint;
          end;

        WM_ENABLE, WM_NCPAINT:
          Exit; // Disabling of blinking when switched

{$IFDEF CHECKXP}
        WM_UPDATEUISTATE: begin
          if SkinData.Skinned and UseThemes and not (csDesigning in ComponentState) and (uxthemeLib <> 0) then
            Ac_SetWindowTheme(Handle, s_Space, s_Space);

          Repaint;
          Exit;
        end;
{$ENDIF}

        CM_ENABLEDCHANGED: begin
          inherited;
          SkinData.FUpdating := False;
          SkinData.Invalidate(True);
          Exit;
        end;

        WM_MOUSEMOVE:
          if CircleClickMode(Self, SkinData, AnimatEvents) then
            MouseInGlyph := PtInRect(GlyphScreenRect(False), acMousePos);

        CM_CHANGED:
          if not (csDesigning in ComponentState) then begin
            if Checked and FAnimCheckDrawing then begin
              if FCommonData.AnimTimer <> nil then
                FCommonData.AnimTimer.Glow := 0;

              FInCheckDrawing := True;
              FCommonData.BGChanged := True;
              PrepareCache;
              FInCheckDrawing := False;
              DoChangePaint(FCommonData, PaintCtrlState, UpdateDiagonal_CB, EventEnabled(aeMouseUp, FAnimatEvents), False)
            end
            else
              DoChangePaint(FCommonData, PaintCtrlState, UpdateWindow_CB, EventEnabled(aeMouseUp, FAnimatEvents), False)
          end
          else
            FCommonData.Invalidate;

        BM_SETCHECK: begin
          StopTimer(SkinData);
          State := sConst.CheckBoxStates[Message.WParam];
          Repaint;
          Exit;
        end;

        WM_ERASEBKGND:
          if IsWindowVisible(Handle) then begin
            if (TWMPaint(Message).DC <> 0) and (Skindata.FCacheBmp <> nil) and not FCommonData.BGChanged and not TimerIsActive(SkinData) then
              CopyWinControlCache(Self, FCommonData, MkRect, MkRect(Self), TWMPaint(Message).DC, False);

            Message.Result := 1;
            Exit;
          end;

        WM_PRINT: begin
          SkinData.FUpdating := False;
          PaintHandler(TWMPaint(Message));
          Exit;
        end;

        WM_PAINT:
          if IsWindowVisible(Handle) then begin
            PaintHandler(TWMPaint(Message));
            Exit;
          end;

        CM_FONTCHANGED, CM_TEXTCHANGED: begin
          if HandleAllocated then begin
            if AutoSize then
              AdjustSize;

            Repaint;
          end;
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
          if not (csDesigning in ComponentState) and Enabled and (DragMode = dmManual) then begin
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
              DoChangePaint(FCommonData, 2, UpdateWindow_CB, EventEnabled(aeMouseDown, FAnimatEvents), True);

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
          if not (csDesigning in ComponentState) and Enabled and (TWMKey(Message).CharCode = VK_SPACE) then begin
            if not ReadOnly then begin
              if FPressed then begin
                FPressed := False;
                Toggle;
              end;
              if Assigned(OnKeyUp) then
                OnKeyUp(Self, TWMKey(Message).CharCode, KeysToShiftState(TWMKey(Message).KeyData));

              if TimerIsActive(SkinData) and (Width <> SkinData.FCacheBmp.Width) then begin // Repaint after animation if size of control is changed
                StopTimer(SkinData);
                Repaint;
              end;
            end;
            Exit;
          end;

        WM_LBUTTONUP:
          if not (csDesigning in ComponentState) and Enabled then begin
            if not ReadOnly then begin
              if FPressed then begin
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

                FPressed := False;
                if EventEnabled(aeMouseUp, FAnimatEvents) then begin
                  b := SkinData.FUpdating;
                  SkinData.FUpdating := True;
                  SkinData.BGChanged := False;
                  try
                    Toggle;
                  finally
                    SkinData.FUpdating := b;
                  end;
                end
                else
                  Toggle;
              end
              else
                FPressed := False;

              if Assigned(OnMouseUp) then
                OnMouseUp(Self, mbLeft, KeysToShiftState(TWMMouse(Message).Keys), TWMMouse(Message).XPos, TWMMouse(Message).YPos);

              if TimerIsActive(SkinData) and (Width <> SkinData.FCacheBmp.Width) then begin // Repaint after animation if size of control is changed
                StopTimer(SkinData);
                Repaint;
              end;
            end
            else
              if Assigned(OnMouseUp) then
                OnMouseUp(Self, mbLeft, KeysToShiftState(TWMMouse(Message).Keys), TWMMouse(Message).XPos, TWMMouse(Message).YPos);

            Exit;
          end;
      end
    else
      case Message.Msg of
        WM_PAINT:
          if IsWindowVisible(Handle) then begin
            StdPaintHandler(TWMPaint(Message));
            if not (csDesigning in ComponentState) then
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
          if not (csCreating in ControlState) and FPressed and ReadOnly then
            Exit;
      end;

    inherited;
  end;
end;


procedure TsCheckBox.SetVerticalAlign(const Value: TvaAlign);
begin
  if FVerticalAlign <> Value then begin
    FVerticalAlign := Value;
    Repaint;
  end;
end;


function TsCheckBox.PaintCtrlState: integer;
begin
  if not (csDesigning in ComponentState) then
    if FPressed then
      Result := 2
    else
      Result := integer(ControlIsActive(FCommonData) and not ReadOnly)
  else
    Result := 0;
end;

end.

