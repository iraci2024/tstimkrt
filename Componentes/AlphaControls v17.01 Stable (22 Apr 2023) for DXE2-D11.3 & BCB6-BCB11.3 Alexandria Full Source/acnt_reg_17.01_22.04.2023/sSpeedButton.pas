unit sSpeedButton;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Imglist, Buttons, comctrls, menus,
  {$IFNDEF DELPHI5}   Types,      {$ENDIF}
  {$IFDEF TNTUNICODE} TntButtons, {$ENDIF}
  {$IFDEF FPC}        LMessages,  {$ENDIF}
  acntTypes, sCommonData, sConst, sDefaults, acAnimation;


type
  TacSplitterStyle = (dsBorder, dsLine);

{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
{$IFDEF TNTUNICODE}
  TsSpeedButton = class(TTntSpeedButton)
{$ELSE}
  TsSpeedButton = class(TSpeedButton)
{$ENDIF}
  private
{$IFNDEF NOTFORHELP}
    FBlend,
    FTextOffset,
    FOldSpacing,
    FImageIndex,
    FOldNumGlyphs,
    FImageIndexHot,
    FImageIndexPressed,
    FImageIndexDisabled,
    FImageIndexSelected: integer;

    OldFlat,
    FWordWrap,
    FReflected,
    FStdParent,
    FBGCopying,
    FStoredDown,
    FUseEllipsis,
    FShowCaption,
    SkipDropDown,
    FDrawOverBorder: boolean;

    FOnMouseEnter,
    FOnMouseLeave: TNotifyEvent;

    FAlignment,
    FTextAlignment: TAlignment;

    FGlyphColorTone:    TColor;
    FStdBG:             TBitmap;
    FDropdownMenu:      TPopupMenu;
    FImageChangeLink:   TChangeLink;
    FGrayedMode:        TacGrayedMode;
    FCommonData:        TsCtrlSkinData;
    FDisabledKind:      TsDisabledKind;
    FOnPaint:           TBmpPaintEvent;
    FAnimatEvents:      TacAnimatEvents;
    FImages:            TCustomImageList;
    FButtonStyle:       TToolButtonStyle;
    FSplitterStyle:     TacSplitterStyle;
    FDisabledGlyphKind: TsDisabledGlyphKind;

    procedure SetGlyphColorTone   (const Value: TColor);
    procedure SetDropdownMenu     (const Value: TPopupMenu);
    procedure SetDisabledKind     (const Value: TsDisabledKind);
    procedure SetImages           (const Value: TCustomImageList);
    procedure SetButtonStyle      (const Value: TToolButtonStyle);
    procedure SetDisabledGlyphKind(const Value: TsDisabledGlyphKind);
    procedure SetInteger          (const Index: Integer; const Value: integer);
    procedure SetBoolean          (const Index: Integer; const Value: boolean);
    procedure SetAlignment        (const Index: integer; const Value: TAlignment);
    procedure ImageListChange(Sender: TObject);
    procedure SetSplitterStyle(const Value: TacSplitterStyle);
  protected
    FGrayed,
    FHotState,
    DroppedDown,
    MenuVisible,
    FMenuOwnerMode: boolean;

    OldMargin,
    ActMargin,
    ActSpacing,
    ActAddedWidth,
    ActArrowWidth,
    ActTextOffset,
    ActGlyphWidth,
    ActArrowSpacing: integer;

    ActTextSize: TSize;
    OldMousePos: TPoint;

    OldCaption: acString;
    OldLayout: TButtonLayout;
    OldOnChange: TNotifyEvent;
    procedure DrawCaption; virtual;
    procedure InitParams;
    function ArrowWidth: integer;
    function GenMargin: integer;
    procedure DoDrawText(var R: TRect; Flags: Cardinal); //virtual;
    procedure DrawGlyph; {$IFNDEF FPC}virtual;{$ENDIF}
    function GlyphWidth(Common: boolean = False): integer; virtual;
    function GlyphHeight: integer; virtual;
    procedure CopyFromCache(aDC: hdc); virtual;
    function FontStored: boolean;

    function PrepareCache: boolean; virtual;

    procedure PaintArrow(const pR: PRect; Mode: integer);
    procedure ActionChanged(Sender: TObject);
    procedure ActionChange (Sender: TObject; CheckDefaults: Boolean); override;
    procedure GlyphChanged (Sender: TObject);
    procedure Ac_CMMouseEnter; virtual;
    procedure Ac_CMMouseLeave; virtual;
    procedure StdPaint(PaintButton: boolean = True); virtual;
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp  (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
{$IFDEF DELPHI7UP}
    function GetActionLinkClass: TControlActionLinkClass; override;
{$ENDIF}
    function CaptionRect(ShiftClicked: boolean = True): TRect;
    function ArrowRect: TRect;

    function CanShiftContent: boolean; virtual;

    procedure GraphRepaint;
    procedure UpdateControl;
    procedure UpdateLight(Sender: TObject; Data: PacAnimEventData); virtual;
  public
    function TextRectSize: TSize;
    function GetImages: TCustomImageList; virtual;
    function CurrentImageIndex: integer;  virtual;
    function CurrentState: integer;       virtual;
    function ImgRect(ShiftClicked: boolean = True): TRect;

    procedure AfterConstruction; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure Loaded; override;
    procedure Invalidate; override;
    procedure WndProc(var Message: TMessage); override;
    property Canvas;
{$ENDIF}
  published
{$IFNDEF NOTFORHELP}
    property Align;
    property OnDragDrop;
    property OnDragOver;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Margin default DefButtonMargin;
    property Font stored FontStored;
    property ParentFont stored FontStored;
{:@event}
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
{:@event}
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
{$ENDIF} // NOTFORHELP
{:@event}
    property OnPaint: TBmpPaintEvent read FOnPaint write FOnPaint;

    property AnimatEvents:      TacAnimatEvents     read FAnimatEvents      write FAnimatEvents        default [aeGlobalDef];
    property ButtonStyle:       TToolButtonStyle    read FButtonStyle       write SetButtonStyle       default tbsButton;
    property SkinData:          TsCtrlSkinData      read FCommonData        write FCommonData;
    property DisabledGlyphKind: TsDisabledGlyphKind read FDisabledGlyphKind write SetDisabledGlyphKind default DefDisabledGlyphKind;
    property DisabledKind:      TsDisabledKind      read FDisabledKind      write SetDisabledKind      default DefDisabledKind;
    property DropdownMenu:      TPopupMenu          read FDropdownMenu      write SetDropdownMenu;
    property Images:            TCustomImageList    read FImages            write SetImages;
    property GrayedMode:        TacGrayedMode       read FGrayedMode        write FGrayedMode          default gmInactive;
    property GlyphColorTone:    TColor              read FGlyphColorTone    write SetGlyphColorTone    default clNone;

    property Alignment:       TAlignment index 0 read FAlignment     write SetAlignment default taCenter;
    property TextAlignment:   TAlignment index 1 read FTextAlignment write SetAlignment default taCenter;

    property Blend:              integer index 0 read FBlend      write SetInteger default 0;
    property ImageIndex:         integer index 1 read FImageIndex write SetInteger default -1;
    property TextOffset:         integer index 2 read FTextOffset     write SetInteger default 0;

    property ImageIndexHot:      integer index 3 read FImageIndexHot      write SetInteger default -1;
    property ImageIndexPressed:  integer index 4 read FImageIndexPressed  write SetInteger default -1;
    property ImageIndexDisabled: integer index 5 read FImageIndexDisabled write SetInteger default -1;
    property ImageIndexSelected: integer index 6 read FImageIndexSelected write SetInteger default -1;

    property DrawOverBorder: boolean index 0 read FDrawOverBorder write SetBoolean default True;
    property Grayed:         boolean index 1 read FGrayed         write SetBoolean default False;
    property Reflected:      boolean index 2 read FReflected      write SetBoolean default False;
    property ShowCaption:    boolean index 3 read FShowCaption    write SetBoolean default True;
    property WordWrap:       boolean index 4 read FWordWrap       write SetBoolean default True;
    property UseEllipsis:    boolean index 5 read FUseEllipsis    write SetBoolean default True;

    property SplitterStyle: TacSplitterStyle read FSplitterStyle write SetSplitterStyle default dsBorder;
  end;


{$IFNDEF NOTFORHELP}
  TacSysCommand = (acscClose, acscMinimize, acscMaxNorm);
{$ENDIF} // NOTFORHELP

{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsSysButton = class(TsSpeedButton)
{$IFNDEF NOTFORHELP}
  private
    FSysCommand: TacSysCommand;
    FDrawAsIcon: boolean;
    FVertIndent: integer;
    function ParentForm: TCustomForm;
    function CharIndex: integer;
    procedure UpdateSize;
    function GlyphSize: TSize;
    procedure SetDrawAsIcon(const Value: boolean);
    procedure SetSysCommand(const Value: TacSysCommand);
    procedure SetVertIndent(const Value: integer);
    function GlyphRect: TRect;
  protected
    procedure UpdateLight(Sender: TObject; Data: PacAnimEventData); override;
  public
    procedure Click; override;
    function SkinGlyphIndex: integer;
    constructor Create(AOwner: TComponent); override;
    function CanAutoSize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure StdPaint(PaintButton: boolean = True); override;
    procedure WndProc(var Message: TMessage); override;
    procedure Loaded; override;
    function PrepareCache: boolean; override;
  published
{$ENDIF} // NOTFORHELP
    property DrawAsIcon: boolean read FDrawAsIcon write SetDrawAsIcon default True;
    property VertIndent: integer read FVertIndent write SetVertIndent default -1;
    property SysCommand: TacSysCommand read FSysCommand write SetSysCommand default acscClose;
  end;


{$IFNDEF NOTFORHELP}
  {$IFDEF DELPHI7UP}
  TacSpeedButtonActionLink = class(TSpeedButtonActionLink)
  protected
    {$IFNDEF D2010}
    FImageIndex: integer;
    {$ENDIF}
    procedure SetImageIndex(Value: Integer); override;
  end;
  {$ENDIF}


  TsTimerSpeedButton = class(TsSpeedButton)
  private
    FAllowTimer: boolean;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property AllowTimer: boolean read FAllowTimer write FAllowTimer default True;
  end;
{$ENDIF} // NOTFORHELP


implementation

uses
  math, ActnList,
  {$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  {$IFDEF DELPHI7UP} Themes, {$ENDIF}
  sGraphUtils, sVCLUtils, sMessages, acntUtils, sAlphaGraph, sStyleSimply, sSkinProps,
  sThirdParty, sSkinManager, acGlow, acGPUtils, sMaskData, sGlyphUtils, acAlphaImageList, acThdTimer, sGradient;


const
  iDefAddedWidth = 16;
  iDefSysHeight = 24;
  iDefSysWidth = 36;


type
  TPopupActionBar_ = class(TPopupMenu)
{$IFDEF D2009}
    FActionManager: TObject;
{$ENDIF}
    FPopupMenu: TObject;
  end;


function IsImgListDefined(Btn: TsSpeedButton): boolean;
begin
  with Btn do
    Result := (GetImages <> nil) and IsValidIndex(CurrentImageIndex, GetImageCount(GetImages));
end;


procedure PaintParentControls(Button: TsSpeedButton; aCanvas: TCanvas);
var
  i: integer;
  R, cR: TRect;
  SavedDC: hdc;
begin
  with Button do begin
    R := BoundsRect;
    for i := 0 to Parent.ControlCount - 1 do
      if Parent.Controls[i] <> Button then
        with Parent.Controls[i] do
          if (Visible or (csDesigning in ComponentState)) and (csOpaque in ControlStyle) then begin
            cR := BoundsRect;
            if PtInRect(R, cR.TopLeft) or PtInRect(R, cR.BottomRight) then begin
              SavedDC := SaveDC(aCanvas.Handle);
              try
                aCanvas.Lock;
                ControlState := ControlState + [csPaintCopy];
                MoveWindowOrg(aCanvas.Handle, cR.Left - R.Left, cr.Top - R.Top);
                IntersectClipRect(aCanvas.Handle, 0, 0, Width, Height);
                Perform(WM_PAINT, WPARAM(aCanvas.Handle), 0);
                ControlState := ControlState - [csPaintCopy];
                aCanvas.UnLock;
              finally
                RestoreDC(aCanvas.Handle, SavedDC);
              end;
            end;
          end;
  end;
end;


function MaxCaptionWidth(Button: TsSpeedButton): integer;
begin
  with Button do
    if ShowCaption and (Caption <> '') then begin
      Result := Width;
      Result := Result - 2 * max(0, ActMargin) - ActArrowWidth - ActAddedWidth;
      if (Layout in [blGlyphLeft, blGlyphRight]) and (ActGlyphWidth <> 0) then
        Result := Result - (ActSpacing + ActGlyphWidth + FTextOffset);
    end
    else
      Result := 0;
end;


function TsSpeedButton.ArrowRect: TRect;
var
  m: integer;
  cr, ir: TRect;
begin
  if ButtonStyle = tbsDropDown then begin
    if not (Assigned(OnClick) and Assigned(DropDownMenu)) then begin
      ir := ImgRect(False);
      cr := CaptionRect(False);
      if WidthOf(cr) + WidthOf(ir) > 0 then
        m := ActArrowSpacing
      else
        m := 0;

      case Layout of
        blGlyphLeft:  Result.Left := cr.Right + m;
        blGlyphRight: Result.Left := ir.Right + m;
        else          Result.Left := max(ir.Right, cr.Right) + m
      end
    end
    else
      Result.Left := Width - ActAddedWidth;

    Result.Right := Result.Left + ActAddedWidth + ActArrowWidth;
    Result.Top := 1;
    Result.Bottom := Height - 1;

    if BiDiMode <> bdLeftToRight then
      ReflectRect(Result, Width, Height, True);

    if CanShiftContent and (not (Assigned(OnClick) and Assigned(DropDownMenu)) or DroppedDown) then
      OffsetRect(Result, 1, 1);
  end
  else
    Result := MkRect;
end;


function TsSpeedButton.ArrowWidth: integer;
begin
  Result := integer(ButtonStyle = tbsDropDown) * ActAddedWidth;
end;


procedure TsSpeedButton.AfterConstruction;
begin
  inherited;
  FCommonData.Loaded;
  if FCommonData.Skinned then
    ControlStyle := ControlStyle + [csOpaque];
end;


function TsSpeedButton.CanShiftContent: boolean;
begin
  with SkinData do
    Result := (CurrentState = 2) and CanClickShift(SkinIndex, CommonSkinData) and ((SkinManager = nil) or not Skinned or (CommonSkinData.gd[SkinIndex].BorderIndex >= 0) or (GetCapture <> 0)) {and acMouseInControl(Self)};
end;


function TsSpeedButton.CaptionRect(ShiftClicked: boolean = True): TRect;
var
  l, t, r, b, iGlyphHeight, dh, dw, emptyw: integer;
begin
  l := 0;
  t := 0;
  r := 0;
  b := 0;

  case Layout of
    blGlyphLeft: begin
      emptyw := Width - ActAddedWidth - ActArrowWidth - ActTextSize.cx - ActGlyphWidth - ActSpacing - ActMargin * 2; // Calc space for text
      t := (Height - ActTextSize.cy) div 2;
      b := t + ActTextSize.cy;
      case Alignment of
        taLeftJustify: begin
          case TextAlignment of
            taLeftJustify: l := ActMargin + ActGlyphWidth + ActSpacing;

            taCenter:
              if ActGlyphWidth = 0 then
                l := ActMargin
              else
                l := emptyw div 2 + ActGlyphWidth + ActMargin;

            taRightJustify:
              if ActGlyphWidth = 0 then
                l := ActMargin
              else
                l := Width - ActMargin - ActTextSize.cx - ActAddedWidth - ActArrowWidth;
          end;
          r := l + ActTextSize.cx;
        end;

        taCenter: begin
          l := ActMargin + emptyw div 2 + ActGlyphWidth + ActSpacing;
          r := l + ActTextSize.cx;
        end;

        taRightJustify: begin
          r := Width - ActAddedWidth - ActArrowWidth - ActMargin;
          l := r - ActTextSize.cx;
        end;
      end;
    end;

    blGlyphRight: begin
      emptyw := Width - ActAddedWidth - ActArrowWidth - ActGlyphWidth - ActSpacing - ActMargin * 2;
      t := (Height - ActTextSize.cy) div 2;
      b := t + ActTextSize.cy;
      case Alignment of
        taLeftJustify: begin
          l := ActMargin;
          r := ActMargin + ActTextSize.cx
        end;

        taCenter: begin
          dw := (Width - ActAddedWidth - ActArrowWidth - ActGlyphWidth - ActSpacing - ActTextSize.cx) div 2 - ActMargin;
          l := ActMargin + dw;
          r := ActMargin + dw + ActTextSize.cx
        end;

        taRightJustify: begin
          case TextAlignment of
            taCenter: begin
              if ActGlyphWidth = 0 then
                l := Width - ActMargin - ActTextSize.cx - ActAddedWidth - ActArrowWidth
              else
                l := (Width - ActMargin - ActGlyphWidth - ActTextSize.cx - ActAddedWidth - ActArrowWidth) div 2;

              r := l + ActTextSize.cx;
            end;

            taLeftJustify: begin
              if ActGlyphWidth = 0 then
                l := Width - ActMargin - ActTextSize.cx - ActAddedWidth - ActArrowWidth
              else
                l := ActMargin;

              r := ActMargin + emptyw;
            end;

            taRightJustify: begin
              l := Width - ActMargin - ActGlyphWidth - ActSpacing - ActTextSize.cx - ActAddedWidth - ActArrowWidth;
              r := ActMargin + emptyw;
            end;
          end;
        end;
      end;
    end;

    blGlyphTop: begin
      iGlyphHeight := GlyphHeight;
      if ActMargin = -1 then begin
        dh := (Height - iGlyphHeight - ActSpacing * integer((iGlyphHeight > 0) and (Caption <> '')) - ActTextSize.cy) div 2 - ActMargin;
        t := (dh + iGlyphHeight + ActSpacing * integer((iGlyphHeight > 0) and (Caption <> '')));
        b := Height - dh;
      end
      else begin
        t := (ActMargin + iGlyphHeight + ActSpacing * integer((iGlyphHeight > 0) and (Caption <> '')));
        b := t + ActTextSize.cy;
      end;
      l := (Width - ActTextSize.cx - ActAddedWidth - ActArrowWidth) div 2;
      r := Width - l - ActAddedWidth - ActArrowWidth;
    end;

    blGlyphBottom: begin
      iGlyphHeight := GlyphHeight;
      dh := (Height - iGlyphHeight - ActSpacing * integer((iGlyphHeight > 0) and (Caption <> '')) - ActTextSize.cy) div 2 - ActMargin;
      l := (Width - ActTextSize.cx - ActAddedWidth - ActArrowWidth) div 2;
      t := ActMargin + dh;
      r := Width - l - ActAddedWidth - ActArrowWidth;
      b := Height - dh - ActMargin - iGlyphHeight - ActSpacing * integer((iGlyphHeight > 0) and (Caption <> ''));
    end;
  end;
  case Layout of
    blGlyphLeft, blGlyphRight: Result := Rect(l + ActTextOffset, t, r + ActTextOffset, b);
    blGlyphTop, blGlyphBottom: Result := Rect(l, t + ActTextOffset, r, b + ActTextOffset);
  end;
  if ShiftClicked and ((ButtonStyle <> tbsDropDown) or not DroppedDown or not Assigned(OnClick)) then begin
    if BiDiMode <> bdLeftToRight then
      ReflectRect(Result, Width, Height, True);

    if CanShiftContent then
      OffsetRect(Result, 1, 1);
  end;
end;


procedure TsSpeedButton.Click;
begin
  if EventEnabled(aeClick, FAnimatEvents) and (FCommonData.SkinManager.AnimEffects.Buttons.ClickEffect = ceWaveOut) then
    DoClickAnimation(SkinData);

  inherited;
end;

procedure TsSpeedButton.CopyFromCache(aDC: hdc);
begin
  BitBlt(Canvas.Handle, 0, 0, Width, Height, aDC, 0, 0, SRCCOPY)
end;


constructor TsSpeedButton.Create(AOwner: TComponent);
begin
  FCommonData := TsCtrlSkinData.Create(Self, True);
  FCommonData.COC := COC_TsSpeedButton;
  inherited Create(AOwner);
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FButtonStyle := tbsButton;
  FWordWrap := True;

  FImageIndex         := -1;
  FImageIndexHot      := -1;
  FImageIndexPressed  := -1;
  FImageIndexDisabled := -1;
  FImageIndexSelected := -1;

  FGrayed := False;
  FBlend := 0;
  FMenuOwnerMode := False;
  MenuVisible := False;
  FStdParent := False;
  FBGCopying := False;
  FHotState := False;
  FStdBG := nil;
  FGrayedMode := gmInactive;
  FDisabledGlyphKind := DefDisabledGlyphKind;
  FDisabledKind := DefDisabledKind;
  FTextOffset := 0;
  FAlignment := taCenter;
  FGlyphColorTone := clNone;
  FShowCaption := True;
  FUseEllipsis := True;
  FDrawOverBorder := True;
  FTextAlignment := taCenter;
  FSplitterStyle := dsBorder;
  OldOnChange := Glyph.OnChange;
  OldMargin := -1;
  OldFlat := True;
  Glyph.OnChange := GlyphChanged;
  FAnimatEvents := [aeGlobalDef];
  Margin := DefButtonMargin;
  FCommonData.OnUpdateLight := UpdateLight;
end;


function TsSpeedButton.CurrentImageIndex: integer;
begin
  Result := -1;
  if not Enabled then
    Result := FImageIndexDisabled
  else
    if ((FState in [bsDown, bsExclusive]) or MenuVisible) and not FMenuOwnerMode then
      if not (FState = bsExclusive) then
        Result := FImageIndexPressed
      else
        Result := FImageIndexSelected
    else
      if SkinData.FMouseAbove then
        Result := FImageIndexHot;

  if Result = -1 then
    Result := FImageIndex;
end;


function TsSpeedButton.CurrentState: integer;
begin
  if ((FState in [bsDown, bsExclusive]) or MenuVisible or DroppedDown) and not FMenuOwnerMode then
    Result := 2
  else
    Result := integer(ControlIsActive(FCommonData) or FHotState or FMenuOwnerMode);
end;


type
  TAccessBasicAction = class(TBasicAction);


destructor TsSpeedButton.Destroy;
begin
  if (FCommonData.SkinManager <> nil) and (FCommonData.SkinManager.ActiveGraphControl = Self) then
    FCommonData.SkinManager.ActiveGraphControl := nil;

  FreeAndNil(FCommonData);
  FreeAndNil(FStdBG);
  FreeAndNil(FImageChangeLink);
  inherited Destroy;
end;


procedure TsSpeedButton.DoDrawText(var R: TRect; Flags: Cardinal);
var
  aRect: TRect;
  TranspMode: byte;
begin
  TranspMode := GetTransMode(SkinData);
  Flags := Flags or DT_NOCLIP or DT_ALPHATEXT * Cardinal(TranspMode = TM_FULL);
  if WordWrap then
    Flags := Flags and not DT_SINGLELINE;

  if SkinData.Skinned then begin
    acWriteTextEx(FCommonData.FCacheBMP.Canvas, PacChar(Caption), True, R, Flags, FCommonData, boolean(CurrentState));
    if TranspMode = TM_PARTIAL then
      FillAlphaRect(FCommonData.FCacheBmp, R);
  end
  else begin
    SkinData.FCacheBmp.Canvas.Font.Assign(Font);
    SkinData.FCacheBmp.Canvas.Brush.Style := bsClear;
    SelectObject(SkinData.FCacheBmp.Canvas.Handle, SkinData.FCacheBmp.Canvas.Font.Handle);
    if not Enabled and (DisabledKind <> []) then begin
      aRect := OffsRect(R, 1);
      SkinData.FCacheBmp.Canvas.Font.Color := clBtnHighlight;
      acDrawText(SkinData.FCacheBmp.Canvas.Handle, PacChar(Caption), aRect, Flags);
      SkinData.FCacheBmp.Canvas.Font.Color := clBtnShadow;
    end;
    SkinData.FCacheBmp.Canvas.Brush.Color := Color;
    SkinData.FCacheBmp.Canvas.Brush.Style := bsClear;
    acDrawText(SkinData.FCacheBmp.Canvas.Handle, PacChar(Caption), R, Flags);
  end;
end;


procedure TsSpeedButton.DrawCaption;
var
  R: TRect;
begin
  if ShowCaption then begin
    FCommonData.FCacheBmp.Canvas.Font.Assign(Font);
    FCommonData.FCacheBMP.Canvas.Brush.Style := bsClear;
    R := CaptionRect;
    DoDrawText(R, GetStringFlags(Self, FTextAlignment) or TextWrapping[WordWrap] or TextEllips[UseEllipsis]);
  end;
  if SkinData.Skinned and (ButtonStyle = tbsDropDown) then begin
    R := ArrowRect;
    PaintArrow(@R, CurrentState);
  end;
end;


procedure TsSpeedButton.DrawGlyph;
var
  C: TColor;
begin
  if (Glyph <> nil) and (Glyph.Width > 0) or IsImgListDefined(Self) then begin
    C := GlyphColorTone;
    DrawBtnGlyph(Self, SkinData.FCacheBmp.Canvas, acColorToRGB(C));
  end;
end;


function TsSpeedButton.FontStored: boolean;
begin
  Result := IsCustomFont(Self, Font, not SkinData.Skinned or SkinData.CustomFont);
end;


function TsSpeedButton.GenMargin: integer;
begin
  Result := iff(Margin = -1, -1, ScaleInt(Margin + 3, SkinData));
end;


function TsSpeedButton.GetImages: TCustomImageList;
begin
  Result := FImages;
end;


{$IFDEF DELPHI7UP}
function TsSpeedButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TacSpeedButtonActionLink;
end;
{$ENDIF}


function TsSpeedButton.GlyphHeight: integer;
begin
  if (Glyph <> nil) and (Glyph.Height > 0) then
    Result := Glyph.Height
  else
    if IsImgListDefined(Self) then
      Result := GetImageHeight(GetImages, CurrentImageIndex, GetPPI(SkinData), True)
    else
      Result := 0;
end;


function TsSpeedButton.GlyphWidth(Common: boolean = False): integer;
begin
  if (Glyph <> nil) and (Glyph.Width > 0) then
    Result := Glyph.Width div NumGlyphs
  else
    if IsImgListDefined(Self) then
      if Common then
        Result := GetImageWidth(GetImages, -1, GetPPI(SkinData)) div NumGlyphs
      else
        Result := GetImageWidth(GetImages, CurrentImageIndex, GetPPI(SkinData), True) div NumGlyphs
    else
      Result := 0;
end;


procedure TsSpeedButton.GraphRepaint;
var
  R: TRect;
  Timer: TacThreadedTimer;
begin
  if ([csDestroying, csLoading] * ComponentState = []) and not (csCreating in ControlState) and not InAnimationProcess then
    if Visible or (csDesigning in ComponentState) then
      if Assigned(Parent) and Parent.HandleAllocated and IsWindowVisible(Parent.Handle) then
        with Skindata do
          if Skinned and not (csDesigning in ComponentState) then begin
            if not Updating then begin
              if not (GetClipBox(Canvas.Handle, R) in [SIMPLEREGION, 3]) or IsRectEmpty(R) then
                Exit;

              if BGChanged then begin
                if not PrepareCache then
                  Exit;

                if not (csPaintCopy in ControlState) then
                  PaintParentControls(Self, FCacheBmp.Canvas)
              end;
              if TimerIsActive(SkinData) then begin
                if (SkinData.PulsTimer <> nil) and SkinData.PulsTimer.Enabled and (SkinData.PulsTimer.BmpFrom <> nil) then
                  Timer := SkinData.PulsTimer
                else
                  Timer := SkinData.AnimTimer;

                if (Timer <> nil) and (Timer.BmpOut <> nil) then
                  CopyFromCache(Timer.BmpOut.Canvas.Handle)
                else
                  CopyFromCache(FCacheBmp.Canvas.Handle);
              end
              else
                CopyFromCache(FCacheBmp.Canvas.Handle);
            end;
          end
          else
            Repaint;
end;


function TsSpeedButton.ImgRect(ShiftClicked: boolean = True): TRect;
var
  dh, dw, gh, gw: integer;
begin
  gh := GlyphHeight;
  gw := GlyphWidth;

  case Layout of
    blGlyphLeft: begin
      dw := (Width - ActAddedWidth - ActArrowSpacing - ActArrowWidth - gw - ActSpacing - ActTextSize.cx) div 2 - ActMargin; // Content offset
      case Alignment of
        taLeftJustify:  Result.Left := ActMargin;
        taCenter:       Result.Left := ActMargin + dw;
        else {taRightJustify:} Result.Left := ActMargin + 2 * dw;
      end;
      Result.Top := (Height - gh) div 2;
    end;

    blGlyphRight:
      case Alignment of
        taLeftJustify: begin
          Result.Left := CaptionRect.Right + ActSpacing;
          Result.Top := (Height - gh) div 2;
          if CurrentState = 2 then // Return back
            dec(Result.Left);
        end;
        taCenter: begin
          Result.Left := (Width - ActAddedWidth - ActArrowWidth - gw + ActSpacing + ActTextSize.cx) div 2;
          Result.Top := (Height - gh) div 2;
        end
        else {taRightJustify:} begin
          Result.Left := Width - ActAddedWidth - ActArrowWidth - gw - ActMargin;
          Result.Top := (Height - gh) div 2;
        end;
      end;

    blGlyphTop: begin
      Result.Left := (Width - ActAddedWidth - ActArrowWidth - gw) div 2 + 1;
      dh := (Height - gh - ActSpacing - ActTextSize.cy) div 2;
      if ActMargin = -1 then
        Result.Top := dh
      else
        Result.Top := ActMargin;
    end

    else {blGlyphBottom:} begin
      dh := (Height - gh - ActSpacing - ActTextSize.cy) div 2 - ActMargin;
      Result.Left := (Width - ActAddedWidth - ActArrowWidth - gw) div 2 + 1;
      Result.Top := Height - ActMargin - dh - gh;
    end;
  end;
  Result.Right := Result.Left + gw;
  Result.Bottom := Result.Top + gh;
  if ShiftClicked and ((ButtonStyle <> tbsDropDown) or not DroppedDown or not Assigned(OnClick)) then begin
    if BiDiMode <> bdLeftToRight then
      ReflectRect(Result, Width, Height, True);

    if CanShiftContent then
      OffsetRect(Result, 1, 1);
  end;
  if Reflected then
    OffsetRect(Result, 0, - HeightOf(Result) div 6);
end;


procedure TsSpeedButton.Loaded;
begin
  inherited;
  FCommonData.Loaded;
  if FCommonData.Skinned then
    ControlStyle := ControlStyle + [csOpaque];

  if IsImgListDefined(Self) then
    Glyph.Assign(nil);
end;


procedure TsSpeedButton.Paint;
var
  R: TRect;
  ParentDC: hdc;
  Timer: TacThreadedTimer;

  procedure CopyImage(Bmp: TBitmap);
  begin
    BitBlt(Canvas.Handle, 0, 0, Width, Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
  end;

begin
  if (Visible or (csDesigning in ComponentState)) and (Parent <> nil) then begin
    if Assigned(FCommonData.SkinManager) and FCommonData.SkinManager.Active and (FCommonData.SkinIndex < 1) then
      FCommonData.UpdateIndexes;

    if not FCommonData.Skinned then
      StdPaint
    else begin
      if (Width > 0) and (Height > 0) then
        if TimerIsActive(SkinData) then begin
          if (SkinData.PulsTimer <> nil) and SkinData.PulsTimer.Enabled and (SkinData.PulsTimer.BmpFrom <> nil) then
            Timer := SkinData.PulsTimer
          else
            Timer := SkinData.AnimTimer;

          if (Timer <> nil) and Assigned(Timer.BmpOut) and (Timer.BmpOut.Width = Width) then
            CopyImage(Timer.BmpOut);
        end
        else
          if not SkinData.FMouseAbove or PtInRect(MkRect(Self), ScreenToClient(acMousePos)) or (csDesigning in ComponentState) then begin // Do not repaint automatically if btn pressed
            if (SkinData.AnimTimer <> nil) and (SkinData.AnimTimer.Enabled) and DroppedDown then
              Exit;
{$IFDEF D2005}
            if CurrentState > 0 then
              FCommonData.BGChanged := True
            else
{$ENDIF}
              FCommonData.BGChanged := (FStoredDown <> Down) or FCommonData.BGChanged or FCommonData.HalfVisible;

            FStoredDown := Down;
            if Parent <> nil then begin
              ParentDC := GetWindowDC(Parent.Handle);
              FCommonData.HalfVisible := (GetClipBox(ParentDC, R) = 0) or IsRectEmpty(R) or (WidthOF(R) < (Left + Width)) or (HeightOF(R) < (Top + Height)) or (Top < 0) or (Left < 0);
              ReleaseDC(Parent.Handle, ParentDC)
            end
            else
              FCommonData.HalfVisible := False;

            if not FCommonData.BGChanged then begin
              if FOldNumGlyphs <> NumGlyphs then begin
                FCommonData.BGChanged := True;
                FOldNumGlyphs := NumGlyphs;
              end
              else
                if FOldSpacing <> Spacing then begin
                  FCommonData.BGChanged := True;
                  FOldSpacing := Spacing;
                end;
            end
            else begin
              FOldNumGlyphs := NumGlyphs;
              FOldSpacing := Spacing;
            end;
            if not FCommonData.BGChanged and not FStdParent or PrepareCache then
              CopyImage(FCommonData.FCacheBmp);
          end;
    end;
  end;
end;


function TsSpeedButton.PrepareCache: boolean;
var
  R: TRect;
  CI: TCacheInfo;
  BGInfo: TacBGInfo;
  si, mi, w, Mode1, Mode2: integer;

  procedure PaintControl;
  begin
    InitCacheBmp(SkinData);
    FCommonData.FCacheBmp.Canvas.Font.Assign(Font);
    if CI.Ready and CI.Bmp.Empty then
      FCommonData.BGChanged := False
    else begin
      case FButtonStyle of
        tbsDivider: begin
          if CI.Ready then
            BitBlt(FCommonData.FCacheBMP.Canvas.Handle, 0, 0, Width, Height, CI.Bmp.Canvas.Handle, Left + CI.X, Top + CI.Y, SRCCOPY)
          else
            FillDC(FCommonData.FCacheBMP.Canvas.Handle, ClientRect, CI.FillColor);

          si := FCommonData.SkinManager.SkinCommonInfo.Sections[ssDivider];
          if FCommonData.SkinManager.IsValidSkinIndex(si) then begin
            mi := FCommonData.SkinManager.GetMaskIndex(si, s_BordersMask, FCommonData.CommonSkinData);
            if Assigned(FCommonData.SkinManager) and FCommonData.SkinManager.IsValidImgIndex(mi) then begin
              w := Width div 2 - 1;
              DrawSkinRect(FCommonData.FCacheBmp, Rect(w, 0, w + 2, Height), CI, FCommonData.CommonSkinData.ma[mi], 0, False);
            end;
          end;
        end;

        tbsSeparator:
          if CI.Ready then
            BitBlt(FCommonData.FCacheBMP.Canvas.Handle, 0, 0, Width, Height, CI.Bmp.Canvas.Handle, Left, Top, SRCCOPY)
          else
            FillDC(FCommonData.FCacheBMP.Canvas.Handle, ClientRect, CI.FillColor)

        else begin
          InitParams;
          if (FButtonStyle = tbsDropDown) then
            if Assigned(DropDownMenu) and DroppedDown then begin
              Mode1 := iff(Assigned(OnClick) and Assigned(DropDownMenu), 1, 2);
              Mode2 := 2;
            end
            else
              if Down or (FState in [bsDown, bsExclusive]) then begin
                Mode1 := 2;
                Mode2 := 1;
              end
              else begin
                Mode1 := integer(ControlIsActive(FCommonData));
                Mode2 := Mode1;
              end
          else begin
            Mode1 := CurrentState;
            Mode2 := Mode1;
          end;

          R := MkRect(Self);
          if not FDrawOverBorder then begin
            PaintItemBG(FCommonData, ci, Mode1, MkRect(Self), Point(Left, Top), FCommonData.FCacheBMP, integer(Down), integer(Down));
            DrawCaption;
            DrawGlyph;
            if Assigned(FOnPaint) then
              FOnPaint(Self, FCommonData.FCacheBmp);

            if FCommonData.SkinManager.IsValidImgIndex(Skindata.CommonSkinData.gd[SkinData.SkinIndex].BorderIndex) then
              if (SplitterStyle = dsLine) or (ActAddedWidth = 0) then
                DrawSkinRect(FCommonData.FCacheBMP, R, ci, FCommonData.CommonSkinData.ma[SkinData.CommonSkinData.gd[SkinData.SkinIndex].BorderIndex], Mode1, False)
              else begin
                // Main rect
                if IsRightToLeft then
                  R.Left := ActAddedWidth
                else
                  R.Right := R.Right - ActAddedWidth;

                DrawSkinRect(FCommonData.FCacheBMP, R, ci, FCommonData.CommonSkinData.ma[SkinData.CommonSkinData.gd[SkinData.SkinIndex].BorderIndex], Mode1, False);
                // Arrow rect
                R := MkRect(Self);
                if IsRightToLeft then
                  R.Right := ActAddedWidth
                else
                  R.Left := R.Right - ActAddedWidth;

                DrawSkinRect(FCommonData.FCacheBMP, R, ci, FCommonData.CommonSkinData.ma[SkinData.CommonSkinData.gd[SkinData.SkinIndex].BorderIndex], Mode2, False);
              end;

            HandleLighting(FCommonData, Mode1);
          end
          else begin
            if (SplitterStyle = dsLine) or (ActAddedWidth = 0) then
              PaintItem(FCommonData, CI, True, Mode1, R, Point(Left, Top), FCommonData.FCacheBMP, True, integer(Down), integer(Down))
            else begin
              // Main rect
              if IsRightToLeft then
                R.Left := ActAddedWidth
              else
                R.Right := R.Right - ActAddedWidth;

              PaintItem(FCommonData, CI, True, Mode1, R, Point(Left, Top), FCommonData.FCacheBMP, True, integer(Down), integer(Down));
              // Arrow rect
              R := MkRect(Self);
              if IsRightToLeft then
                R.Right := ActAddedWidth
              else
                R.Left := R.Right - ActAddedWidth;

              PaintItem(FCommonData, CI, True, Mode2, R, Point(Left + R.Left, Top + R.Top), FCommonData.FCacheBMP, True, integer(Down), integer(Down));
            end;
            HandleLighting(FCommonData, Mode1);
            DrawCaption;
            DrawGlyph;
            if Assigned(FOnPaint) then
              FOnPaint(Self, FCommonData.FCacheBmp);
          end;
          if not Enabled then
            BmpDisabledKind(FCommonData.FCacheBmp, FDisabledKind, Parent, GetParentCache(FCommonData), Point(Left, Top));

          if SkinData.DoLighting then
            UpdateCorners(SkinData, Mode1);
        end;
      end;
      FCommonData.BGChanged := False;
    end;
  end;

begin
  if Self <> nil then begin
    Result := True;
    BGInfo.Bmp := nil;
    BGInfo.BgType := btUnknown;
    BGInfo.PleaseDraw := False;
    BGInfo.FillRect := MkRect;
    Parent.Perform(SM_ALPHACMD, AC_GETBG_HI, LPARAM(@BGInfo));
    if BGInfo.BgType = btUnknown then begin
      FStdParent := True;
      ControlStyle := ControlStyle + [csOpaque];
      InitCacheBmp(SkinData);
      CI.Ready := False;
      CI.Bmp := nil;
      CI.FillColor := ColorToRGB(TacAccessControl(Parent).Color);
      PaintControl;
    end
    else
      if BGInfo.BgType = btNotReady then begin
        FCommonData.FUpdating := True;
        Result := False;
      end
      else begin
        CI := BGInfoToCI(@BGInfo);
        PaintControl;
      end;
  end
  else
    Result := False;
end;


procedure TsSpeedButton.SetAlignment(const Index: integer; const Value: TAlignment);

  procedure ChangeProp(var Prop: TAlignment; Value: TAlignment);
  begin
    if Prop <> Value then begin
      Prop := Value;
      UpdateControl;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FAlignment, Value);
    1: ChangeProp(FTextAlignment, Value);
  end;
end;


procedure TsSpeedButton.SetButtonStyle(const Value: TToolButtonStyle);
begin
  if FButtonStyle <> Value then begin
    FButtonStyle := Value;
    UpdateControl;
  end;
end;


procedure TsSpeedButton.SetDisabledGlyphKind(const Value: TsDisabledGlyphKind);
begin
  if FDisabledGlyphKind <> Value then begin
    FDisabledGlyphKind := Value;
    UpdateControl;
  end;
end;


procedure TsSpeedButton.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    UpdateControl;
  end;
end;


procedure TsSpeedButton.SetSplitterStyle(const Value: TacSplitterStyle);
begin
  if FSplitterStyle <> Value then begin
    FSplitterStyle := Value;
    SkinData.Invalidate;
  end;
end;


procedure TsSpeedButton.SetImages(const Value: TCustomImageList);
begin
  if Images <> Value then begin
    if Images <> nil then
      Images.UnRegisterChanges(FImageChangeLink);

    FImages := Value;
    if Images <> nil then begin
      Images.RegisterChanges(FImageChangeLink);
      Images.FreeNotification(Self);
    end;
    UpdateControl;
  end;
end;


function TsSpeedButton.TextRectSize: TSize;
var
  R: TRect;
  TranspMode: byte;
begin
  if ShowCaption then begin
    R := MkRect(MaxCaptionWidth(Self), 0);
    TranspMode := GetTransMode(SkinData);
    acDrawText(FCommonData.FCacheBMP.Canvas.Handle, Caption, R, TextWrapping[FWordWrap] or TextEllips[UseEllipsis] or DT_EXPANDTABS or DT_CALCRECT or DT_ALPHATEXT * Cardinal(TranspMode = 2));
    Result := MkSize(R);
  end
  else
    Result := MkSize;
end;


procedure TsSpeedButton.UpdateControl;
begin
  if (Visible or (csDesigning in ComponentState)) and (SkinData.CtrlSkinState and ACS_LOCKED = 0) and (ComponentState * [csLoading, csDestroying] = []) then begin
    FCommonData.BGChanged := True;
    GraphRepaint;
  end;
end;


procedure TsSpeedButton.UpdateLight(Sender: TObject; Data: PacAnimEventData);
begin
  if IsWindowVisible(Parent.Handle) {and Visible {and Enabled} and EventEnabled(aeLighting, FAnimatEvents) and (ButtonStyle in [tbsButton, tbsCheck, tbsDropDown]) then
    DoUpdateLight(SkinData, Data.DoOutput)
  else
    SkinData.FDoLighting := False;

  SkinData.DestroyNeedlessLight;
  Data.DoLighting := SkinData.DoLighting;
end;


procedure TsSpeedButton.WndProc(var Message: TMessage);
var
  sd: TsCommonData;
begin
{$IFDEF LOGGED}
//  AddToLog(Message);
{$ENDIF}
  if Message.Msg = SM_ALPHACMD then
    case Message.WParamHi of
      AC_CTRLHANDLED: begin
        Message.Result := 1;
        Exit;
      end;

      AC_SETNEWSKIN:
        if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
          CommonMessage(Message, FCommonData);
          if FCommonData.Skinned then
            ControlStyle := ControlStyle + [csOpaque];

          Exit;
        end;

      AC_REMOVESKIN:
        if (ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager)) and not (csDestroying in ComponentState) then begin
          CommonMessage(Message, FCommonData);
          ControlStyle := ControlStyle - [csOpaque];
          if Visible or (csDesigning in ComponentState) then
            Repaint;
        end;

      AC_ENDPARENTUPDATE: begin
        if not InUpdating(FCommonData, True) and FCommonData.Skinned then
          GraphRepaint;

        Exit;
      end;

      AC_REFRESH:
        if RefreshNeeded(SkinData, Message) then begin
          CommonMessage(Message, FCommonData);
          if Visible or (csDesigning in ComponentState) then
            Repaint;

          Exit;
        end;

      AC_INVALIDATE: begin
        FCommonData.FUpdating := False;
        GraphRepaint;
        Exit;
      end;

      AC_MOUSELEAVE: begin
        Ac_CMMouseLeave;
        if Parent <> nil then // Standard mouseleave doesn't work after BDMode changing
          Parent.Perform(CM_MOUSELEAVE, 0, 0);

        Exit;
      end;

      AC_GETDEFSECTION: begin
        Message.Result := 14 + 1;
        Exit;
      end;

      AC_GETDEFINDEX: begin
        if FCommonData.SkinManager <> nil then
          with FCommonData.SkinManager.SkinCommonInfo do
            Message.Result := iff(Flat, Sections[ssToolButton], Sections[ssSpeedButton]) + 1;

        Exit;
      end;

      AC_SETGLASSMODE: begin
        CommonMessage(Message, FCommonData);
        Exit;
      end;
    end;

  if not ControlIsReady(Self) or not FCommonData.Skinned then begin
    case Message.Msg of
      CM_MOUSEENTER:
        if Enabled and ([csDesigning, csDestroying] * ComponentState = []) then begin
          FCommonData.FMouseAbove := True;
          if Assigned(FOnMouseEnter) then
            FOnMouseEnter(Self);

          if not Flat then begin
            FHotState := True;
            Paint;
            Exit;
          end;
        end;

      CM_MOUSELEAVE:
        if Enabled and ([csDesigning, csDestroying] * ComponentState = []) then begin
          FCommonData.FMouseAbove := False;
          if Assigned(FOnMouseLeave) then
            FOnMouseLeave(Self);

          if bsDown = FState then
            FState := bsUp;

          if not Flat then begin
            FHotState := False;
            Paint;
            Exit;
          end;
        end;
    end;
    inherited
  end
  else begin
    case Message.Msg of
      SM_ALPHACMD:
        case Message.WParamHi of
          AC_UPDATESECTION: begin
            GraphRepaint;
            Exit;
          end;

          AC_PREPARING: begin
            Message.Result := LRESULT(FCommonData.FUpdating);
            Exit;
          end;

          AC_PREPARECACHE: begin
            PrepareCache;
            Exit;
          end;
        end;

      CM_MOUSEENTER:
        if Enabled and not MouseInControl and not (csDesigning in ComponentState) and not InUpdating(SkinData) then begin
          FreeAndNil(SkinData.LightMask);
          Ac_CMMouseEnter;
          sd := TsCommonData(Parent.Perform(SM_ALPHACMD, AC_GETSKINDATA_HI, 0));
          if (sd <> nil) and (sd.CtrlSkinState and ACS_SIZING = 0) then begin
            SetRedraw(Parent.Handle, 0); // Prevent of the parent repaint
            inherited;
            SetRedraw(Parent.Handle, 1);
          end
          else
            inherited;

          RestartAttention(SkinData);
          Exit;
        end;

      WM_MOUSELEAVE, CM_MOUSELEAVE:
        if Enabled and not InAnimationProcess then begin
          SkinData.BGChanged := False;
          Ac_CMMouseLeave;
          inherited;
          RestartAttention(SkinData);
          Exit;
        end;

      WM_ERASEBKGND, WM_NCPAINT:
        Exit;

{$IFNDEF DYNAMICCACHE}
      CM_TEXTCHANGED:
        if ShowCaption then begin
          FCommonData.BGChanged := True;
          StopTimer(SkinData);
          GraphRepaint;
          Exit;
        end;
{$ENDIF}
      CM_PARENTCOLORCHANGED, WM_WINDOWPOSCHANGED, WM_SIZE:
        if Visible or (csDesigning in ComponentState) then
          FCommonData.BGChanged := True;

      WM_LBUTTONDBLCLK, WM_LBUTTONDOWN:
        if not (csDesigning in ComponentState) then
          if FMenuOwnerMode then
            Exit;

      WM_LBUTTONUP:
        if not (csDesigning in ComponentState) then begin
          if FMenuOwnerMode then
            Exit;

          if (SkinData.AnimTimer <> nil) and SkinData.AnimTimer.Enabled and (SkinData.AnimTimer.BmpFrom <> nil) then
            if Assigned(SkinData.AnimTimer.BmpOut) and (SkinData.AnimTimer.BmpOut.Width = Width) then begin
              PrepareCache;
              GraphRepaint; // Fast repainting if clicked quickly
            end;
        end;
    end;

    if not CommonWndProc(Message, FCommonData) then begin
      inherited;
      case Message.Msg of
        CM_VISIBLECHANGED: begin
          SkinData.FMouseAbove := False;
          SkinData.BGChanged := True;
        end;

        WM_MOUSEMOVE:
          if SkinData.FMouseAbove then
            if FCommonData.SkinManager.ActiveGraphControl <> Self then begin
              SkinData.FMouseAbove := False;
              Ac_CMMouseEnter;
              Perform(CM_MOUSEENTER, 0, 0);
            end;

        CM_BUTTONPRESSED: begin // If changed when GroupIndex <> 0
          if not FCommonData.SkinManager.Effects.AllowAnimation or not EventEnabled(aeClick, FAnimatEvents) or not (FCommonData.SkinManager.AnimEffects.Buttons.ClickEffect in [ceCircle, ceCircleAndWave]) or not (SkinData.AnimTimer <> nil) then
            StopTimer(SkinData);

          FCommonData.BGChanged := True;
          GraphRepaint;
        end;

        CM_FONTCHANGED: begin // If changed when GroupIndex <> 0
          StopTimer(SkinData);
          FCommonData.BGChanged := True;
          GraphRepaint;
        end;

        CM_PARENTFONTCHANGED:
          if not FCommonData.CustomFont and ParentFont then begin // If changed when GroupIndex <> 0
            StopTimer(SkinData);
            if not InAnimationProcess then begin
              FCommonData.BGChanged := True;
              GraphRepaint;
            end;
          end;

        WM_LBUTTONDBLCLK, WM_LBUTTONDOWN:
          if [csDesigning, csDestroying] * ComponentState = [] then begin
            FCommonData.FUpdating := FCommonData.Updating;
            FCommonData.BGChanged := False;
            if FCommonData.SkinManager.Effects.AllowAnimation and (beClick in FCommonData.SkinManager.AnimEffects.Buttons.Events) and
                (FCommonData.SkinManager.AnimEffects.Buttons.ClickEffect in [ceCircle, ceCircleAndWave]) and not (Self is TsSysButton) and (ButtonStyle in [tbsButton, tbsCheck, tbsDropDown]) then
              DoChangePaint(FCommonData, 2, UpdateFillLight, True, False, False)
            else
              DoChangePaint(SkinData, 2, UpdateGraphic_CB, EventEnabled(aeMouseDown, FAnimatEvents), True, False);
         end;

        WM_LBUTTONUP:
          if Visible and ([csDesigning, csDestroying] * ComponentState = []) and (SkinData <> nil) then
            if FCommonData.SkinManager.Effects.AllowAnimation and EventEnabled(aeClick, FAnimatEvents) and (FCommonData.SkinManager.AnimEffects.Buttons.ClickEffect in [ceCircle, ceCircleAndWave]) and (SkinData.AnimTimer <> nil) then
              if SkinData.AnimTimer.Enabled then begin // Animation is not finished
                SkinData.AnimTimer.State := 1;
                SkinData.BGChanged := True; // Change cache
                SkinData.AnimTimer.TimeHandler;
              end
              else begin // just repaint
                if (SkinData.AnimTimer.ThreadType = TT_FILLLIGHT) and (FCommonData.SkinManager.AnimEffects.Buttons.ClickEffect = ceCircleAndWave) then begin
                  SkinData.AnimTimer.ThreadType := 0;
                  if acMouseInControl(Self) then
                    DoClickAnimation(SkinData);
                end;
                SkinData.Invalidate;
              end
            else
              DoChangePaint(SkinData, integer(SkinData.FMouseAbove), UpdateGraphic_CB, EventEnabled(aeMouseUp, FAnimatEvents), True)
          else
            Message.Result := -1;

        CM_ENABLEDCHANGED:
          if Visible or (csDesigning in ComponentState) then begin
            if not Enabled then
              StopTimer(SkinData);

            FCommonData.BGChanged := True;
            if not (csLoading in ComponentState) then
              GraphRepaint;

            Exit;
          end;

        WM_MOVE:
          if (csDesigning in ComponentState) and not SkinData.Updating then
            Repaint;

        WM_SIZE, WM_WINDOWPOSCHANGED:
          if (csDesigning in ComponentState) and not SkinData.Updating then
            GraphRepaint;
      end;
    end;
  end;
end;


procedure TsSpeedButton.SetDropdownMenu(const Value: TPopupMenu);
begin
  if Value <> FDropdownMenu then begin
    FDropdownMenu := Value;
    if Value <> nil then
      Value.FreeNotification(Self);
  end;
end;


procedure TsSpeedButton.SetGlyphColorTone(const Value: TColor);
begin
  if FGlyphColorTone <> Value then begin
    FGlyphColorTone := Value;
    FCommonData.BGChanged := True;
    GraphRepaint;
  end;
end;


procedure TsSpeedButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  p: TPoint;
  c: TMouse;
begin
  if not (csDesigning in ComponentState) then begin
    if not ShowHintStored then begin
      AppShowHint := Application.ShowHint;
      Application.ShowHint := False;
      ShowHintStored := True;
    end;
    if (Button = mbLeft) and Enabled then
      if (ButtonStyle = tbsDropDown) and Assigned(DropDownMenu) and ((X > Width - ActAddedWidth) or not Assigned(OnClick)) then begin
        TempControl := pointer(Self);
        c := nil;
        StopTimer(SkinData);
        if not MenuVisible and not SkipDropDown then begin
{$IFNDEF FPC}
          if SkinData.SkinManager <> nil then
            SkinData.SkinManager.SkinableMenus.HookPopupMenu(DropDownMenu, SkinData.SkinManager.SkinnedPopups);
{$ENDIF}
          MenuVisible := True;
          DroppedDown := True;
          SkipDropDown := True;
          if not (Assigned(OnClick) and Assigned(DropDownMenu)) then
            FState := bsDown;

          FCommonData.FUpdating := FCommonData.Updating;
          FCommonData.BGChanged := True;

          GraphRepaint;
          HideGlow(SkinData.GlowID);
          p := ClientToScreen(Point(0, Height));
          DropDownMenu.PopupComponent := Self;
          if BiDiMode = bdLeftToRight then
            DropDownMenu.Popup(p.X, p.Y)
          else
            DropDownMenu.Popup(p.X + Width, p.Y);

          ControlState := ControlState - [csLButtonDown]; // Not changed automatically
          DroppedDown := False;
          MenuVisible := False;
          TempControl := nil;
          if not PtInRect(Rect(p.x, p.y - Height - 1, p.x + Width, p.y - 1), c.CursorPos) then
            Perform(CM_MOUSELEAVE, 0, 0);

          StopTimer(SkinData);
          if not (Assigned(OnClick) and Assigned(DropDownMenu)) then
            FState := bsUp;

          if not SkinData.Skinned then
            Repaint
          else
            GraphRepaint;

          if ShowHintStored then begin
            Application.ShowHint := AppShowHint;
            ShowHintStored := False;
          end;
          if not acMouseInControl(Self) then
            SkipDropDown := False;
        end
        else
          SkipDropDown := False;
      end
      else
        inherited
    else
      inherited;

    if (FCommonData.PulsTimer <> nil) and FCommonData.PulsTimer.Enabled then
      FCommonData.PulsTimer.PrepareImage;
  end
  else
    inherited;
end;


procedure TsSpeedButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if [csDesigning, csDestroying] * ComponentState = [] then begin
    Application.ShowHint := AppShowHint;
    ShowHintStored := False;
    if (Button = mbLeft) and Enabled and (ButtonStyle = tbsDropDown) then begin
      DroppedDown := False;
      TempControl := nil;
    end;
    inherited;
    if not (csDestroying in ComponentState) and (FCommonData <> nil) and (FCommonData.PulsTimer <> nil) and FCommonData.PulsTimer.Enabled then
      FCommonData.PulsTimer.PrepareImage;
  end;
end;


procedure TsSpeedButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = Images then
      Images := nil
    else
      if AComponent = DropDownMenu then
        DropDownMenu := nil;
end;


procedure TsSpeedButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
var
  b: boolean;
begin
  inherited ActionChange(Sender, CheckDefaults);
  if not (csLoading in ComponentState) then
    with TCustomAction(Sender) do begin
      if (ActionList <> nil) and (Self.Images <> ActionList.Images) then begin
        Self.FImages := ActionList.Images;
        b := True;
      end
      else
        b := False;

      if Self.FImageIndex <> ImageIndex then begin
        Self.FImageIndex := ImageIndex;
        b := True;
      end;
      if b and (Self.Images <> nil) and IsValidIndex(Self.ImageIndex, GetImageCount(Self.Images)) then
        Glyph.Assign(nil);

      if not (csDestroying in ComponentState) then
        FCommonData.Invalidate;
    end;
end;


procedure TsSpeedButton.GlyphChanged(Sender: TObject);
begin
  if Assigned(OldOnChange) then
    OldOnChange(Glyph);

  if ([csLoading, csDestroying] * ComponentState = []) and not (csCreating in ControlState) then
    FCommonData.Invalidate;
end;


procedure TsSpeedButton.ActionChanged(Sender: TObject);
var
  b: boolean;
begin
  with TCustomAction(Sender) do
    if not FCommonData.FUpdating and (Action <> nil) and (ActionList <> nil) then begin
      b := False;
      if Self.Images <> ActionList.Images then begin
        Self.Images := ActionList.Images;
        b := True;
      end;
{$IFDEF DELPHI7UP}
      if Self.ImageIndex <> TacSpeedButtonActionLink(ActionLink).FImageIndex then begin
        Self.ImageIndex := TacSpeedButtonActionLink(ActionLink).FImageIndex;
        b := True;
      end;
{$ENDIF}
      if b then
        Repaint;
    end;
end;


procedure TsSpeedButton.Ac_CMMouseEnter;
begin
  if not (csDesigning in ComponentState) then begin
    if Assigned(FOnMouseEnter) then
      FOnMouseEnter(Self);

    if not FCommonData.FMouseAbove and not (ButtonStyle in [tbsDivider, tbsSeparator]) and not SkinData.SkinManager.Options.NoMouseHover then begin
      FCommonData.FMouseAbove := True;
      FCommonData.SkinManager.ActiveGraphControl := Self;
      if not FMenuOwnerMode then
        DoChangePaint(SkinData, 1, UpdateGraphic_CB, EventEnabled(aeMouseEnter, FAnimatEvents), False, not (Self is TsSysButton))
      else
        FCommonData.BGChanged := True;
    end;
  end;
end;


type
  TAccessManager = class(TsSkinManager);


procedure TsSpeedButton.Ac_CMMouseLeave;
var
  bFillLight: boolean;
begin
  if FCommonData.FMouseAbove and ([csDesigning, csDestroying] * ComponentState = []) and not acMouseInControl(Self) then begin
    if Assigned(FOnMouseLeave) then
      FOnMouseLeave(Self);

    SkinData.DestroyNeedlessLight(False);
    FCommonData.FMouseAbove := False;
    if TAccessManager(FCommonData.SkinManager).FActiveGraphControl = Self then
      TAccessManager(FCommonData.SkinManager).FActiveGraphControl := nil;

    if bsDown = FState then
      FState := bsUp;

    if Visible and not (csDesigning in ComponentState) and not FMenuOwnerMode then begin
      if not DroppedDown then begin
        if FCommonData.BGChanged then
          PrepareCache;

        if (FCommonData.AnimTimer <> nil) and (FCommonData.AnimTimer.ThreadType = TT_FILLLIGHT) then begin
          bFillLight := True;
          FCommonData.AnimTimer.ThreadType := 0;
        end
        else
          bFillLight := False;

        DoChangePaint(SkinData, 0, UpdateGraphic_CB, EventEnabled(aeMouseLeave, FAnimatEvents), False, False);
        if bFillLight and (ButtonStyle in [tbsButton, tbsCheck, tbsDropDown]) and (FCommonData.SkinManager.AnimEffects.Buttons.ClickEffect = ceCircleAndWave) then
          DoClickAnimation(SkinData);
      end;
    end
    else
      FCommonData.BGChanged := True;
  end;
end;


procedure TsSpeedButton.InitParams;
begin
  ActTextOffset := ScaleInt(FTextOffset, SkinData);
  ActGlyphWidth := GlyphWidth(Alignment <> taCenter);
  ActSpacing := ScaleInt(Spacing * integer(ShowCaption and (ActGlyphWidth > 0) and (Caption <> '')), SkinData);
  ActMargin := GenMargin;
  if ButtonStyle = tbsDropDown then
    if not (Assigned(OnClick) and Assigned(DropDownMenu)) then begin
      ActAddedWidth := 0;
      ActArrowWidth := ScaleInt(iDefAddedWidth, SkinData);
      ActArrowSpacing := SkinData.CommonSkinData.Spacing;// div 2;
    end
    else begin
      ActAddedWidth := ScaleInt(iDefAddedWidth, SkinData);
      ActArrowWidth := 0;
      ActArrowSpacing := 0;
    end
  else begin
    ActArrowSpacing := 0;
    ActAddedWidth := 0;
    ActArrowWidth := 0;
  end;
  ActTextSize := TextRectSize;
end;


procedure TsSpeedButton.Invalidate;
begin
  if (OldLayout <> Layout) or (OldMargin <> Margin) or (OldFlat <> Flat) then begin
    OldLayout := Layout;
    OldMargin := Margin;
    OldFlat   := Flat;
    FCommonData.BGChanged := True;
    FCommonData.UpdateIndexes;
  end;
  if not (FState in [bsDown]) or not SkinData.Skinned then
    inherited;
end;


procedure TsSpeedButton.StdPaint(PaintButton: boolean = True);
const
  DownStyles: array[Boolean] of Integer = (BDR_RAISEDINNER, BDR_SUNKENOUTER);
  FillStyles: array[Boolean] of Integer = (BF_MIDDLE, 0);
var
  PaintRect: TRect;
  DrawFlags: Integer;
  TmpCanvas: TCanvas;
  ArrowPoints: array of TPoint;
{$IFDEF DELPHI7UP}
  Button: TThemedButton;
  ToolButton: TThemedToolBar;
  Details: TThemedElementDetails;
{$ENDIF}
begin
  InitCacheBmp(SkinData);
  TmpCanvas := SkinData.FCacheBmp.Canvas;
  if not Enabled then
    FState := bsDisabled
  else
    if FState = bsDisabled then
      if Down and (GroupIndex <> 0) then
        FState := bsExclusive
      else
        FState := bsUp;

  TmpCanvas.Font.Assign(Font);
  case ButtonStyle of
    tbsDivider: begin
{$IFDEF DELPHI7UP}
      PaintRect := ClientRect;
      PaintRect.Left := WidthOf(PaintRect) div 2 - 1;
      if acThemesEnabled then begin
        PaintRect.Right := PaintRect.Left + 4;
        PerformEraseBackground(Self, TmpCanvas.Handle);
        ToolButton := ttbSeparatorNormal;
        Details := acThemeServices.GetElementDetails(ToolButton);
        acThemeServices.DrawElement(TmpCanvas.Handle, Details, PaintRect);
      end
      else
{$ENDIF}
      begin
        PaintRect.Right := PaintRect.Left + 2;
        DrawEdge(TmpCanvas.Handle, PaintRect, BDR_SUNKENOUTER, BF_RECT);
      end;
    end;

    tbsSeparator:
    // Leaved as is

    else begin
      InitParams;
      if PaintButton then begin
        if (ButtonStyle = tbsDropDown) and (Assigned(OnClick) and Assigned(DropDownMenu)) then begin
          PaintRect := ClientRect;
          PaintRect.Right := PaintRect.Right - ArrowWidth;
        end
        else
          PaintRect := ClientRect;
{$IFDEF DELPHI7UP}
        if acThemesEnabled then begin
          PerformEraseBackground(Self, TmpCanvas.Handle);

          if not Enabled then
            Button := tbPushButtonDisabled
          else
            if FState in [bsDown, bsExclusive] then
              Button := tbPushButtonPressed
            else
              if MouseInControl or FHotState then
                Button := tbPushButtonHot
              else
                Button := tbPushButtonNormal;

          ToolButton := ttbToolbarDontCare;
          if Flat then
            case Button of
              tbPushButtonDisabled: Toolbutton := ttbButtonDisabled;
              tbPushButtonPressed : Toolbutton := ttbButtonPressed;
              tbPushButtonHot     : Toolbutton := ttbButtonHot;
              tbPushButtonNormal  : Toolbutton := ttbButtonNormal;
            end;

          if ToolButton = ttbToolbarDontCare then begin
            Details := acThemeServices.GetElementDetails(Button);
            acThemeServices.DrawElement(TmpCanvas.Handle, Details, PaintRect);
{$IFDEF DELPHI_XE2}
            acThemeServices.GetElementContentRect(TmpCanvas.Handle, Details, PaintRect, PaintRect);
{$ELSE}
            PaintRect := acThemeServices.ContentRect(TmpCanvas.Handle, Details, PaintRect);
{$ENDIF}
          end
          else begin
            Details := acThemeServices.GetElementDetails(ToolButton);
            acThemeServices.DrawElement(TmpCanvas.Handle, Details, PaintRect);
{$IFDEF DELPHI_XE2}
            acThemeServices.GetElementContentRect(TmpCanvas.Handle, Details, PaintRect, PaintRect);
{$ELSE}
            PaintRect := acThemeServices.ContentRect(TmpCanvas.Handle, Details, PaintRect);
{$ENDIF}
          end;
          if ButtonStyle = tbsDropDown then begin
            PaintRect := ClientRect;
            PaintRect.Left := PaintRect.Right - ArrowWidth;

            if Assigned(OnCLick) then begin
              if (Assigned(DropDownMenu) and DroppedDown) or Down or (FState in [bsDown, bsExclusive]) then
                if ToolButton = ttbToolbarDontCare then begin
                  Button := tbPushButtonPressed;
                  Details := acThemeServices.GetElementDetails(Button);
                end
                else begin
                  ToolButton := ttbButtonPressed;
                  Details := acThemeServices.GetElementDetails(ToolButton);
                end;

              acThemeServices.DrawElement(TmpCanvas.Handle, Details, PaintRect);
            end;

            SetLength(ArrowPoints, 3);

            ArrowPoints[0] := Point(Width - 12 + integer(Toolbutton = ttbButtonPressed), (HeightOf(PaintRect) - 5) div 2 + PaintRect.Top + integer(Toolbutton = ttbButtonPressed));
            ArrowPoints[1] := Point(ArrowPoints[0].X + 5, ArrowPoints[0].Y);
            ArrowPoints[2] := Point(ArrowPoints[0].X + 2, ArrowPoints[0].Y + 5);

            TmpCanvas.Brush.Style := bsSolid;
            TmpCanvas.Brush.Color := clBtnText;
            TmpCanvas.Pen.Color := clBtnText;
            TmpCanvas.Polygon(ArrowPoints);
          end;

        end
        else
{$ENDIF}
        begin
          if not Flat then begin
            DrawFlags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
            if FState in [bsDown, bsExclusive] then
              DrawFlags := DrawFlags or DFCS_PUSHED;

            DrawFrameControl(TmpCanvas.Handle, PaintRect, DFC_BUTTON, DrawFlags);
          end
          else begin
            if (FState in [bsDown, bsExclusive]) or (MouseInControl and (FState <> bsDisabled)) or (csDesigning in ComponentState) then begin
              if not Transparent then begin
                TmpCanvas.Brush.Color := Color;
                TmpCanvas.FillRect(PaintRect);
              end
              else
                BitBlt(TmpCanvas.Handle, 0, 0, Width, Height, Canvas.Handle, 0, 0, SRCCOPY);

              DrawEdge(TmpCanvas.Handle, PaintRect, DownStyles[FState in [bsDown, bsExclusive]], FillStyles[Transparent] or BF_RECT)
            end
            else
              if not Transparent then begin
                TmpCanvas.Brush.Color := Color;
                TmpCanvas.FillRect(PaintRect);
              end
              else
                BitBlt(TmpCanvas.Handle, 0, 0, Width, Height, Canvas.Handle, 0, 0, SRCCOPY);

            InflateRect(PaintRect, -1, -1);
          end;
{$IFDEF FPS}
          if FState in [bsDown, bsExclusive] then
            if (FState = bsExclusive) and (not Flat or not MouseInControl) then begin
              TmpCanvas.Brush.Bitmap := AllocPatternBitmap(clBtnFace, clBtnHighlight);
              TmpCanvas.FillRect(PaintRect);
            end;
{$ENDIF}

          if ButtonStyle = tbsDropDown then begin
            PaintRect := ClientRect;
            PaintRect.Left := PaintRect.Right - ArrowWidth;
            if Assigned(OnCLick) then
              if (Assigned(DropDownMenu) and DroppedDown) or Down or (FState in [bsDown, bsExclusive]) then
                if not Flat then begin
                  DrawFlags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT or DFCS_PUSHED;
                  DrawFrameControl(TmpCanvas.Handle, PaintRect, DFC_BUTTON, DrawFlags);
                end
                else
                  DrawEdge(TmpCanvas.Handle, PaintRect, DownStyles[True], FillStyles[Transparent] or BF_RECT)
              else
                if not Flat then begin
                  DrawFlags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
                  DrawFrameControl(TmpCanvas.Handle, PaintRect, DFC_BUTTON, DrawFlags);
                end
                else
                  if MouseInControl then
                    DrawEdge(TmpCanvas.Handle, PaintRect, DownStyles[FState in [bsDown, bsExclusive]], FillStyles[Transparent] or BF_RECT);

            SetLength(ArrowPoints, 3);
            ArrowPoints[0] := Point(Width - 12 + integer(FState in [bsDown, bsExclusive]), (HeightOf(PaintRect) - 5) div 2 + PaintRect.Top + integer(FState in [bsDown, bsExclusive]));
            ArrowPoints[1] := Point(ArrowPoints[0].X + 5, ArrowPoints[0].Y);
            ArrowPoints[2] := Point(ArrowPoints[0].X + 2, ArrowPoints[0].Y + 5);
            TmpCanvas.Brush.Style := bsSolid;
            TmpCanvas.Brush.Color := clBtnText;
            TmpCanvas.Pen.Color := clBtnText;
            TmpCanvas.Polygon(ArrowPoints);
          end;
        end;
      end;
      DrawCaption;
      DrawGlyph;
    end;
  end;
  BitBlt(Canvas.Handle, 0, 0, Width, Height, TmpCanvas.Handle, 0, 0, SRCCOPY);
end;


procedure TsSpeedButton.ImageListChange(Sender: TObject);
begin
  if SkinData.Skinned then begin
    SkinData.BGChanged := True;
    GraphRepaint;
  end
  else
    Repaint;
end;


procedure TsSpeedButton.SetBoolean(const Index: Integer; const Value: boolean);

  procedure ChangeProp(var Prop: boolean; Value: boolean);
  begin
    if Prop <> Value then begin
      Prop := Value;
      StopTimer(SkinData);
      UpdateControl;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FDrawOverBorder, Value);
    1: ChangeProp(FGrayed,         Value);
    2: ChangeProp(FReflected,      Value);
    3: ChangeProp(FShowCaption,    Value);
    4: ChangeProp(FWordWrap,       Value);
    5: ChangeProp(FUseEllipsis,    Value);
  end;
end;


procedure TsSpeedButton.PaintArrow(const pR: PRect; Mode: integer);
var
  C: TsColor;
  DoLine: boolean;
  iNdx, i: integer;
  ArrStyle: TacArrowsStyle;
begin
  with FCommonData do begin
    if Skinned then begin
      iNdx := GetFontIndex(Self, SkinIndex, SkinManager, Mode); // Receive parent font if needed
      ArrStyle := SkinManager.Options.ActualArrowStyle;
      if iNdx >= 0 then begin
        i := mini(Mode, ac_MaxPropsIndex);
        C.C := CommonSkinData.gd[iNdx].Props[i].FontColor.Color;
      end
      else
        C.C := Font.Color;
    end
    else begin
      ArrStyle := arsSolid1;
      C.C := Font.Color;
    end;
    DrawArrow(FCacheBmp, C.C, clNone, pR^, asBottom, 0, 0, 0, ArrStyle, GetPPI(SkinData));
    if (Assigned(OnClick) and Assigned(DropDownMenu)) then begin
      if SplitterStyle = dsLine then
        DoLine := Mode > 0
      else
        if FCommonData.SkinIndex < 0 then
          DoLine := True
        else
          if FCommonData.CommonSkinData.gd[FCommonData.SkinIndex].BorderIndex >= 0 then
            DoLine := (Mode > 0) or (FCommonData.CommonSkinData.ma[FCommonData.CommonSkinData.gd[FCommonData.SkinIndex].BorderIndex].DrawMode and BDM_ACTIVEONLY = 0)
          else
            DoLine := Mode > 0;

      if DoLine then begin
        C.A := 80;
        acgpDrawLine(FCacheBMP.Canvas.Handle, Width - ActAddedWidth, 4, Width - ActAddedWidth, Height - 4, C.C, 1);
      end;
    end;
  end;
end;


constructor TsTimerSpeedButton.Create(AOwner: TComponent);
begin
  inherited;
  Width := Height - 4;
end;


procedure TsSpeedButton.SetInteger(const Index: Integer; const Value: integer);

  procedure ChangeProp(var Prop: integer; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      UpdateControl;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FBlend,              LimitIt(Value, 0, 100));
    1: ChangeProp(FImageIndex,         Value);
    2: ChangeProp(FTextOffset,         Value);
    3: ChangeProp(FImageIndexHot,      Value);
    4: ChangeProp(FImageIndexPressed,  Value);
    5: ChangeProp(FImageIndexDisabled, Value);
    6: ChangeProp(FImageIndexSelected, Value);
  end;
end;


{$IFDEF DELPHI7UP}
procedure TacSpeedButtonActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked or FClient.Glyph.Empty then begin
    FImageIndex := Value;
    if (Action is TCustomAction) and (FClient is TsSpeedButton) then
      TsSpeedButton(FClient).ActionChanged(Action);
  end;
end;
{$ENDIF}


function TsSysButton.CanAutoSize(var NewWidth, NewHeight: Integer): Boolean;
var
  md: TsMaskData;
  tg: TacTitleGlyph;
begin
  Result := True;
  if DrawAsIcon then begin
    if (SkinData.SkinManager <> nil) and SkinData.SkinManager.IsActive then begin
      case SysCommand of
        acscClose: tg := tgClose;
        acscMinimize: tg := tgMin;
        else // acscMaxNorm:
          if ParentForm.WindowState = wsMaximized then
            tg := tgNormal
          else
            tg := tgMax;
      end;
      if SkinData.SkinManager.IsValidImgIndex(SkinData.CommonSkinData.TitleGlyphs[tg]) then
        md := SkinData.CommonSkinData.ma[SkinData.CommonSkinData.TitleGlyphs[tg]];

      NewWidth := md.Width;
      NewHeight := md.Height;
    end
    else begin
      NewWidth := ScaleInt(iDefSysWidth, Skindata);
      NewHeight := ScaleInt(iDefSysHeight, Skindata);
    end;
  end
  else
    inherited CanAutoSize(NewWidth, NewHeight);
end;


function TsSysButton.CharIndex: integer;
begin
  case SysCommand of
    acscClose: Result := iServCharClose;
    acscMinimize: Result := iServCharMin
    else // acscMaxNorm:
      if ParentForm.WindowState = wsMaximized then
        Result := iServCharRest
      else
        Result := iServCharMax;
  end;
end;


procedure TsSysButton.Click;
begin
  if Assigned(OnClick) then
    inherited
  else
    case SysCommand of
      acscClose: TrySendMessage(ParentForm.Handle, WM_SYSCOMMAND, SC_CLOSE);
      acscMinimize: TrySendMessage(ParentForm.Handle, WM_SYSCOMMAND, SC_MINIMIZE);
      else // acscMaxNorm:
        if ParentForm.WindowState = wsMaximized then
          TrySendMessage(ParentForm.Handle, WM_SYSCOMMAND, SC_RESTORE)
        else
          TrySendMessage(ParentForm.Handle, WM_SYSCOMMAND, SC_MAXIMIZE);
    end;
end;


constructor TsSysButton.Create(AOwner: TComponent);
begin
  inherited;
  FDrawAsIcon := True;
  FVertIndent := -1;
end;


function TsSysButton.GlyphRect: TRect;
var
  Size: TSize;
begin
  Size := GlyphSize;
  if SkinData.SkinManager.SkinCommonInfo.BIVAlign = 1 then
    Result.TopLeft := Point((Width - Size.cx) div 2, 0)
  else
    if FVertIndent >= 0 then
      Result.TopLeft := Point((Width - Size.cx) div 2, FVertIndent)
    else
      Result.TopLeft := Point((Width - Size.cx) div 2, (Height - Size.cy) div 2);

  Result.Right := Result.Left + Size.cx;
  Result.Bottom := Result.Top + Size.cy;
end;


function TsSysButton.GlyphSize: TSize;
var
  md: TsMaskData;
  tg: TacTitleGlyph;
begin
  if (SkinData.SkinManager <> nil) and (SkinData.SkinManager.IsActive or (msBroadCasting in SkinData.SkinManager.managerstate)) then begin
    case SysCommand of
      acscClose: tg := tgClose;
      acscMinimize: tg := tgMin;
      else // acscMaxNorm:
        if (ParentForm <> nil) and (ParentForm.WindowState = wsMaximized) then
          tg := tgNormal
        else
          tg := tgMax;
    end;
    if SkinData.CommonSkinData.TitleGlyphs[tg] >= 0 then begin
      if SkinData.SkinManager.IsValidImgIndex(SkinData.CommonSkinData.TitleGlyphs[tg]) then
        md := SkinData.CommonSkinData.ma[SkinData.CommonSkinData.TitleGlyphs[tg]];

      Result.cx := md.Width;
      Result.cy := md.Height;
    end
    else begin
      Result.cx := ScaleInt(iDefSysWidth, Skindata);
      Result.cy := ScaleInt(iDefSysHeight, Skindata);
    end;
  end
  else begin
    Result.cx := ScaleInt(iDefSysWidth, Skindata);
    Result.cy := ScaleInt(iDefSysHeight, Skindata);
  end;
end;


procedure TsSysButton.Loaded;
begin
  inherited;
  UpdateSize;
end;


function TsSysButton.ParentForm: TCustomForm;
begin
  Result := GetParentForm(Self);
end;


function TsSysButton.PrepareCache: boolean;
var
  BGInfo: TacBGInfo;
  md: TsMaskData;
  ndx: integer;
begin
  if DrawAsIcon then begin
    Result := True;
try
    InitCacheBmp(SkinData);
    SkinData.BGChanged := False;
finally

end;
    BGInfo.Bmp := nil;
    BGInfo.BgType := btUnknown;
    BGInfo.PleaseDraw := False;
    BGInfo.FillRect := MkRect;
    Parent.Perform(SM_ALPHACMD, AC_GETBG_HI, LPARAM(@BGInfo));
    case BGInfo.BgType of
      btUnknown: begin
        FStdParent := True;
        ControlStyle := ControlStyle + [csOpaque];
        FillDC(SkinData.FCacheBmp.Canvas.Handle, MkRect(Self), ColorToRGB(TacAccessControl(Parent).Color));
      end;
      btNotReady: begin
        FCommonData.FUpdating := True;
        Result := False;
      end;
      btCache:
        BitBlt(SkinData.FCacheBmp.Canvas.Handle, 0, 0, Width, Height, BGInfo.Bmp.Canvas.Handle, BGInfo.Offset.X + Left, BGInfo.Offset.Y + Top, SRCCOPY)
      else
        FillDC(SkinData.FCacheBmp.Canvas.Handle, MkRect(Self), ColorToRGB(BGInfo.Color));
    end;

    // Paint
    ndx := SkinGlyphIndex;
    if SkinData.SkinManager.IsValidImgIndex(ndx) then begin
      md := SkinData.CommonSkinData.ma[ndx];
        DrawSkinGlyph(SkinData.FCacheBmp, GlyphRect.TopLeft, CurrentState, 1, md,  BGInfoToCI(@BGInfo));
    end;
    if not Enabled then
      BmpDisabledKind(FCommonData.FCacheBmp, FDisabledKind, Parent, GetParentCache(FCommonData), Point(Left, Top));
  end
  else
    Result := inherited PrepareCache;
end;


procedure TsSysButton.SetDrawAsIcon(const Value: boolean);
begin
  if FDrawAsIcon <> Value then begin
    FDrawAsIcon := Value;
    SkinData.GlowID := -2;
    SkinData.CtrlSkinState := SkinData.CtrlSkinState or ACS_NCCHANGED;
    SkinData.Invalidate;
  end
  else
    SkinData.GlowID := -1;
end;


procedure TsSysButton.SetSysCommand(const Value: TacSysCommand);
begin
  if FSysCommand <> Value then begin
    FSysCommand := Value;
    if not (csLoading in ComponentState) then begin
      SkinData.CtrlSkinState := SkinData.CtrlSkinState or ACS_NCCHANGED;
      SkinData.Invalidate;
    end;
  end;
end;


procedure TsSysButton.SetVertIndent(const Value: integer);
begin
  if FVertIndent <> Value then begin
    FVertIndent := Value;
    SkinData.Invalidate;
  end;
end;


function TsSysButton.SkinGlyphIndex: integer;
var
  tg: TacTitleGlyph;
begin
  case SysCommand of
    acscClose: tg := tgClose;
    acscMinimize: tg := tgMin;
    else // acscMaxNorm:
      if ParentForm.WindowState = wsMaximized then
        tg := tgNormal
      else
        tg := tgMax;
  end;
  Result := SkinData.CommonSkinData.TitleGlyphs[tg];
end;


procedure TsSysButton.StdPaint(PaintButton: boolean);
const
  BGColors: array [boolean, 0..2] of TColor = (
    (cl3DLight, clHighLight, TColor($FF00001A){clHotLight}), // not Close btn
    (clRed, clRed, clMaroon)                                // Close btn
  );
  TxtColors: array [boolean, 0..2] of TColor = (
    (clBtnText, clHighLightText, clHighLightText), // not Close btn
    (clWhite, clWhite, clWhite)                   // Close btn
  );
var
  PPI: integer;

  procedure DrawServiceGlyph(gIndex: integer);
  var
    gR: TRect;
    h, w: integer;
  begin
    if gIndex >= 0 then begin
      h := GetImageHeight(acCharImages, gIndex, PPI);
      w := GetImageWidth(acCharImages, gIndex, PPI);
      gR.Top := (Height - h) div 2;
      gR.Bottom := gR.Top + h;
      gR.Left := (Width - w) div 2;
      gR.Right := gR.Left + w;
      SetImagesPPI(acCharImages, PPI);
      if Transparent and (CurrentState = 0) then
        SkinData.FCacheBmp.Canvas.Font.Color := TacAccessControl(Parent).Font.Color
      else
        SkinData.FCacheBmp.Canvas.Font.Color := TxtColors[SysCommand = acscClose, CurrentState];

      acCharImages.Draw(SkinData.FCacheBmp.Canvas, gR.Left, gR.Top, gIndex, True);
    end;
  end;

begin
  if DrawAsIcon then begin
    InitCacheBmp(SkinData);
    PPI := GetWndPPI(ParentForm.Handle);
    if Transparent and (CurrentState = 0) then
      FillDC(SkinData.FCacheBmp.Canvas.Handle, MkRect(Self), TacAccessControl(Parent).Color)
    else
      FillDC(SkinData.FCacheBmp.Canvas.Handle, MkRect(Self), BGColors[SysCommand = acscClose, CurrentState]);

    DrawServiceGlyph(CharIndex);
    BitBlt(Canvas.Handle, 0, 0, Width, Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
  end
  else
    inherited;
end;


procedure TsSysButton.UpdateLight(Sender: TObject; Data: PacAnimEventData);
begin
  Data.DoLighting := False;
end;


procedure TsSysButton.UpdateSize;
var
  w: integer;
begin
  if not (csLoading in ComponentState) and DrawAsIcon then
    with GlyphSize do begin
      w := cx;
      if not Flat and (SkinData.CommonSkinData.BISpacing > 0) then
        inc(w, SkinData.CommonSkinData.BISpacing);

      SetBounds(Left, Top, w, cy);
    end;
end;


procedure TsSysButton.WndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  AddToLog(Message);
{$ENDIF}
  case Message.Msg of
    WM_PAINT:
      if SkinData.CtrlSkinState and ACS_NCCHANGED <> 0 then begin
        UpdateSize;
        SkinData.CtrlSkinState := SkinData.CtrlSkinState and not ACS_NCCHANGED;
      end;

    CM_HITTEST:
      if SkinData.Skinned and not PtInRect(GlyphRect, Point(TWMMouse(Message).XPos, TWMMouse(Message).YPos)) then begin
        if SkinData.FMouseAbove then begin
          if Assigned(OnMouseLeave) then
            OnMouseLeave(Self);

          FCommonData.FMouseAbove := False;
          if TAccessManager(FCommonData.SkinManager).FActiveGraphControl = Self then
            TAccessManager(FCommonData.SkinManager).FActiveGraphControl := nil;

          if bsDown = FState then
            FState := bsUp;

          if Visible and not (csDesigning in ComponentState) and not FMenuOwnerMode then begin
            if not DroppedDown then
              DoChangePaint(SkinData, 0, UpdateGraphic_CB, EventEnabled(aeMouseLeave, FAnimatEvents), False, False)
          end
          else
            FCommonData.BGChanged := True;
        end;
        Message.Result := Windows.HTNOWHERE;
        Exit;
      end;

  end;
  inherited;
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_SETNEWSKIN:
          if (ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager)) and not (csDestroying in ComponentState) then begin
            SkinData.CtrlSkinState := SkinData.CtrlSkinState or ACS_NCCHANGED;
            UpdateSize;
          end;

        AC_REFRESH:
          if RefreshNeeded(SkinData, Message) then
            if not (csDestroying in ComponentState) then
              UpdateSize;

        AC_REMOVESKIN:
          if (ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager)) and not (csDestroying in ComponentState) then begin
            SkinData.CtrlSkinState := SkinData.CtrlSkinState or ACS_NCCHANGED;
            UpdateSize;
          end;
      end;
  end;
end;

end.
