unit sButton;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, ImgList, Menus, StdCtrls,
  {$IFNDEF DELPHI5}   Types,       {$ENDIF}
  {$IFDEF DELPHI_XE2} UITypes,     {$ENDIF}
  {$IFDEF LOGGED}     sDebugMsgs,  {$ENDIF}
  {$IFDEF TNTUNICODE} TntStdCtrls, {$ENDIF}
  {$IFDEF FPC}        LMessages, lcltype,   {$ENDIF}
  acntTypes, sCommonData, sConst, sDefaults, acAnimation, sSkinManager;


type
{$IFNDEF NOTFORHELP}
  TButtonStyle = (bsPushButton, bsCommandLink, bsSplitButton);
{$ENDIF}

{$IFNDEF D2009}
  {$IFNDEF NOTFORHELP}
  TImageAlignment = (iaLeft, iaRight, iaTop, iaBottom, iaCenter);

  TImageMargins = class(TPersistent)
  private
    FTop,
    FLeft,
    FRight,
    FBottom: Integer;
    FOnChange: TNotifyEvent;
    procedure SetMargin(Index, Value: Integer);
  protected
    procedure Change;
  public
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Left:   Integer index 0 read FLeft   write SetMargin default 0;
    property Top:    Integer index 1 read FTop    write SetMargin default 0;
    property Right:  Integer index 2 read FRight  write SetMargin default 0;
    property Bottom: Integer index 3 read FBottom write SetMargin default 0;
  end;
  {$ENDIF} // NOTFORHELP
{$ENDIF}


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
{$IFDEF TNTUNICODE}
  TsButton = class(TTntButton)
{$ELSE}
  TsButton = class(TButton)
{$ENDIF}
{$IFNDEF NOTFORHELP}
  private
    FOnMouseEnter,
    FOnMouseLeave: TNotifyEvent;

    FDown,
    FShowFocus,
    FReflected,
    FMouseClicked,
    FCustomCommandLinkFont: boolean;

    FFocusMargin,
    FContentMargin: integer;

    FStyle: TButtonStyle;
    FCommandLinkFont: TFont;
    FCommonData: TsCtrlSkinData;
    FDisabledKind: TsDisabledKind;
    FAnimatEvents: TacAnimatEvents;
    FOnGetColoring: TacGetColoring;
{$IFNDEF D2009}
    FImageIndex,
    FHotImageIndex,
    FPressedImageIndex,
    FDisabledImageIndex,
    FSelectedImageIndex: TImageIndex;

    FDropDownMenu: TPopupMenu;
    FImages: TCustomImageList;
    FCommandLinkHint: acString;
    FImageMargins: TImageMargins;
    FImageChangeLink: TChangeLink;
    FImageAlignment: TImageAlignment;
{$ELSE}
    OldImageIndex: integer;
{$ENDIF}
{$IFNDEF DELPHI7UP}
    FWordWrap: boolean;
    procedure SetWordWrap(const Value: boolean);
{$ENDIF}
    procedure SetFocusMargin   (const Value: integer);
    procedure SetContentMargin (const Value: integer);

    procedure SetStyle         (const Value: TButtonStyle);
    procedure SetDisabledKind  (const Value: TsDisabledKind);
    procedure WMKeyUp      (var Message: TWMKey);         message WM_KEYUP;
    procedure CNDrawItem   (var Message: TWMDrawItem);    message CN_DRAWITEM;
    procedure CNMeasureItem(var Message: TWMMeasureItem); message CN_MEASUREITEM;
    procedure SetCommandLinkFont(const Value: TFont);
{$IFNDEF D2009}
    procedure ImageListChange(Sender: TObject);
    procedure ImageMarginsChange(Sender: TObject);
    procedure SetImages         (const Value: TCustomImageList);
    procedure SetImageMargins   (const Value: TImageMargins);
    procedure SetImageAlignment (const Value: TImageAlignment);
    procedure SetCommandLinkHint(const Value: acString);
    procedure SetImageIndex     (const Index: integer; const Value: TImageIndex);
    procedure SetImgIndex       (const Value: TImageIndex);
{$ELSE}
    function GetImageIndex: TImageIndex;
    procedure SetImageIndex(const Value: TImageIndex);
{$ENDIF}
    function IsImageIndexStored: Boolean;
    procedure SetBoolean(const Index: Integer; const Value: boolean);
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    function IsCmdFontStored: boolean;
  protected
    IsFocused,
    DroppedDown: boolean;
{$IFDEF D2009}
    procedure UpdateImages; override;
    procedure UpdateImageList; override;
{$ELSE}
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
{$ENDIF}
    procedure StdDrawItem(const DrawItemStruct: TDrawItemStruct);
    procedure SetButtonStyle(ADefault: Boolean); {$IFNDEF FPC} override;{$ENDIF}
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp  (Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure OurPaintHandler(aDC: hdc);
    procedure DrawCaption(ColorText: TColor; Canvas: TCanvas = nil);
    procedure DrawGlyph  (ColorTones: TacColoring; Canvas: TCanvas = nil);
    procedure CustomFontChanged(Sender: TObject);
    function ScaledDropWidth: integer;
    function CaptionRect: TRect;
    function GlyphExist: boolean;
    function PrepareCache: boolean;
    function ImageSize: TSize;
    procedure GetColoring(var Coloring: TacColoring);
{$IFDEF D2010}
    procedure DrawElevation(ACanvas: TCanvas);
{$ENDIF}
    procedure UpdateLight(Sender: TObject; Data: PacAnimEventData);
  public
    function GetImages: TCustomImageList; virtual;
    function CurrentImageIndex: integer; virtual;
    constructor Create(AOwner: TComponent); override;
    procedure CreateParams(var Params: TCreateParams); override;
    function CurrentState: integer; virtual;
    function GlyphIndex: integer;
    function GlyphRect: TRect;
    destructor Destroy; override;
    procedure Click; override;
    procedure AfterConstruction; override;
    procedure Invalidate; override;
    procedure Loaded; override;
    procedure WndProc(var Message: TMessage); override;
  published
{$ENDIF} // NOTFORHELP
    property AnimatEvents: TacAnimatEvents read FAnimatEvents write FAnimatEvents default [aeGlobalDef];
    property SkinData: TsCtrlSkinData read FCommonData write FCommonData;
    property DisabledKind: TsDisabledKind read FDisabledKind write SetDisabledKind default DefDisabledKind;
    property FocusMargin: integer read FFocusMargin write SetFocusMargin default 1;
    property CommandLinkFont: TFont read FCommandLinkFont write SetCommandLinkFont stored IsCmdFontStored;

    property Down:                  boolean index 0 read FDown                  write SetBoolean default False;
    property ShowFocus:             boolean index 1 read FShowFocus             write SetBoolean default True;
    property Reflected:             boolean index 2 read FReflected             write SetBoolean default False;
    property CustomCommandLinkFont: boolean index 3 read FCustomCommandLinkFont write SetBoolean default False;

{$IFNDEF NOTFORHELP}
  {$IFNDEF D2009}
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property CommandLinkHint: acString read FCommandLinkHint write SetCommandLinkHint;
    property DropDownMenu: TPopupMenu read FDropDownMenu write FDropDownMenu;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageAlignment: TImageAlignment read FImageAlignment write SetImageAlignment default iaLeft;
    property ImageMargins: TImageMargins read FImageMargins write SetImageMargins;

    property ImageIndex:         TImageIndex read         FImageIndex         write SetImgIndex   stored IsImageIndexStored default -1;
    property HotImageIndex:      TImageIndex index 1 read FHotImageIndex      write SetImageIndex default -1;
    property DisabledImageIndex: TImageIndex index 2 read FDisabledImageIndex write SetImageIndex default -1;
    property PressedImageIndex:  TImageIndex index 3 read FPressedImageIndex  write SetImageIndex default -1;
    property SelectedImageIndex: TImageIndex index 4 read FSelectedImageIndex write SetImageIndex default -1;
  {$ELSE}
    property ImageIndex: TImageIndex read GetImageIndex write SetImageIndex stored IsImageIndexStored default -1;
  {$ENDIF}
    property Style: TButtonStyle read FStyle write SetStyle default bsPushButton;
{$ENDIF} // NOTFORHELP
    property OnGetColoring: TacGetColoring read FOnGetColoring write FOnGetColoring;
    property ContentMargin: integer read FContentMargin write SetContentMargin default 6;
    property WordWrap{$IFNDEF DELPHI7UP}: boolean read FWordWrap write SetWordWrap{$ENDIF} default True;
  end;


implementation

uses
  ActnList, math, Commctrl,
  {$IFDEF DELPHI7UP}Themes, {$ENDIF}
  sVCLUtils, sMessages, acntUtils, sGraphUtils, sAlphaGraph, sThirdParty,
  sStyleSimply, acGlow, sMaskData, acThdTimer, acgpUtils, sGradient;


{$IFNDEF D2009}
type
  TPushButtonActionLink = class(TButtonActionLink)
  protected
    function IsImageIndexLinked: Boolean; override;
    procedure SetImageIndex(Value: Integer); override;
  end;
{$ENDIF}


const
  iContentSpacing = 6;
  DropWidth = 16;


var
  bFocusChanging: boolean = False;
{$IFDEF D2010}
  icoElevat: TIcon = nil;
{$ENDIF}


function ContentSpacing(Button: TsButton): integer;
begin
  if Button.SkinData.SkinManager <> nil then
    Result := Button.SkinData.SkinManager.ScaleInt(iContentSpacing)
  else
    Result := iContentSpacing;
end;


function GetImageSize(Button: TsButton; AddMargins: boolean = True): TSize;
begin
  with Button do begin
    if (GetImages <> nil) and IsValidIndex(CurrentImageIndex, GetImageCount(GetImages)) then
      Result := MkSize(GetImageWidth(GetImages, -1, GetPPI(SkinData)), GetImageHeight(GetImages, -1, GetPPI(SkinData)))
    else
      Result := MkSize(32, 32);

    if AddMargins then
      Result := MkSize(Result.cx + ImageMargins.Left + ImageMargins.Right, Result.cy + ImageMargins.Top + ImageMargins.Bottom);
  end;
end;


procedure TsButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    Self.Enabled := TCustomAction(Sender).Enabled;
end;


procedure TsButton.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or BS_OWNERDRAW;
end;


procedure TsButton.CNMeasureItem(var Message: TWMMeasureItem);
begin
  with Message.MeasureItemStruct^ do begin
    itemWidth  := Width;
    itemHeight := Height;
  end;
end;


procedure TsButton.AfterConstruction;
begin
  inherited;
  FCommonData.Loaded;
end;


procedure TsButton.Invalidate;
begin
{$IFDEF D2009}
  if OldImageIndex <> ImageIndex then begin
    OldImageIndex := ImageIndex;
    FCommonData.BGChanged := True;
  end;
{$ENDIF}
  inherited;
end;


function TsButton.ScaledDropWidth: integer;
begin
  Result := DropWidth * SkinData.CurrentPPI div 96;
end;


procedure TsButton.SetBoolean(const Index: Integer; const Value: boolean);

  procedure ChangeProp(var Prop: boolean; Value: boolean);
  begin
    if Prop <> Value then begin
      Prop := Value;
      FCommonData.Invalidate;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FDown, Value);
    1: ChangeProp(FShowFocus, Value);
    2: ChangeProp(FReflected, Value);
    3: ChangeProp(FCustomCommandLinkFont, Value);
  end;
end;


procedure TsButton.SetButtonStyle(ADefault: Boolean);
begin
{$IFDEF DELPHI_10} // Desc 10
  inherited;
{$ENDIF}
  if ADefault <> IsFocused then begin
    IsFocused := ADefault;
    if SkinData <> nil then
      SkinData.Invalidate;
  end;
end;


function GetCaptionSize(Button: TsButton): TSize;
var
  R: TRect;
  Flags: Cardinal;
  TranspMode: byte;
  MaxWidth: integer;
begin
  with Button do
    if Caption <> '' then begin
      SelectObject(SkinData.FCacheBmp.Canvas.Handle, Button.Font.Handle);
      if Style = bsCommandLink then begin
        SkinData.FCacheBmp.Canvas.Font.Style := SkinData.FCacheBmp.Canvas.Font.Style + [fsBold];
        SkinData.FCacheBmp.Canvas.Font.Size := SkinData.FCacheBmp.Canvas.Font.Size + 2;
      end;
      TranspMode := GetTransMode(SkinData);
      Flags := DT_EXPANDTABS or {DT_CENTER or }DT_CALCRECT or DT_ALPHATEXT * Cardinal(TranspMode = 2);
      if WordWrap then
        Flags := Flags or DT_WORDBREAK
      else
        Flags := Flags or DT_SINGLELINE;

      MaxWidth := Width - 2 * FContentMargin;
      if GlyphExist {Style = bsPushButton} then
        R := Rect(0, 0, MaxWidth - GetImageSize(Button).cx - ContentSpacing(Button) - ImageMargins.Left - ImageMargins.Right{ - 2 * FContentMargin}, 0)
      else
        R := Rect(2 * FContentMargin, 0, MaxWidth, 0);

      acDrawText(SkinData.FCacheBmp.Canvas.Handle, Caption, R, Flags);
      if Style = bsCommandLink then begin
        SkinData.FCacheBmp.Canvas.Font.Style := SkinData.FCacheBmp.Canvas.Font.Style - [fsBold];
        SkinData.FCacheBmp.Canvas.Font.Size := SkinData.FCacheBmp.Canvas.Font.Size - 2;
      end;
      Result := MkSize(R);
    end
    else
      Result := MkSize;
end;


function TsButton.CaptionRect: TRect;
var
  Size: TSize;
  ImgW, ImgH, aWidth: integer;
begin
  Size := GetCaptionSize(Self);
  aWidth := Width;
  with ImageSize do begin
    ImgW := cx;
    ImgH := cy;
  end;
  if Style = bsSplitButton then
    dec(aWidth, ScaledDropWidth);

  if Style = bsCommandLink then begin
    Result.Left := ImageMargins.Left + FContentMargin + ImgW + ContentSpacing(Self) + ImageMargins.Right;
    Result.Right := aWidth - FContentMargin;
    Result.Top := FContentMargin;
    Result.Bottom := Result.Top + Size.cy;
  end
  else
    if GlyphExist and (ImageAlignment <> iaCenter) then
      case ImageAlignment of
        iaLeft: begin
          Result.Left := ImageMargins.Left + FContentMargin + ImgW + ContentSpacing(Self) + ImageMargins.Right;
          Result.Right := aWidth - FContentMargin;
          Result.Top := (Height - Size.cy) div 2;
          Result.Bottom := Height - Result.Top;
        end;

        iaRight: begin
          Result.Right := aWidth - (ImageMargins.Right + FContentMargin + ImgW + ImageMargins.Left);
          Result.Left := FContentMargin;
          Result.Top := (Height - Size.cy) div 2;
          Result.Bottom := Height - Result.Top;
        end;

        iaTop: begin
          Result.Top := ImageMargins.Top + 2 * FContentMargin + ImgH;
          Result.Bottom := Height - FContentMargin;

          Result.Top := Result.Top + (Result.Bottom - Result.Top - Size.cy) div 2;
          Result.Bottom := Result.Top + Size.cy;

          Result.Left := FContentMargin;
          Result.Right := aWidth - FContentMargin;
        end

        else {iaBottom} begin
          Result.Bottom := Height - (ImageMargins.Bottom + FContentMargin + ImgH + ImageMargins.Top);
          Result.Top := ImageMargins.Top + (Result.Bottom - ImageMargins.Top - Size.cy) div 2;
          Result.Bottom := Result.Top + Size.cy;

          Result.Left := FContentMargin;
          Result.Right := aWidth - FContentMargin;
        end;
      end
    else begin
      Result.Left   := (aWidth - Size.cx) div 2;
      Result.Right  := Result.Left + Size.cx;
      Result.Top    := (Height - Size.cy) div 2;
      Result.Bottom := Result.Top + Size.cy;
    end;
    
  if (CurrentState = 2) and CanClickShift(SkinData.SkinIndex, SkinData.CommonSkinData) then
    OffsetRect(Result, 1, 1);
end;


procedure TsButton.Click;
begin
  if EventEnabled(aeClick, FAnimatEvents) and (FCommonData.SkinManager.AnimEffects.Buttons.ClickEffect = ceWaveOut) then
    DoClickAnimation(SkinData);

  inherited;
end;


constructor TsButton.Create(AOwner: TComponent);
begin
  FCommonData := sCommonData.TsCtrlSkinData.Create(Self, True);
  FCommonData.COC := COC_TsButton;
  inherited Create(AOwner);
  ControlStyle := ControlStyle - [csOpaque, csDoubleClicks];
  FDisabledKind := DefDisabledKind;
  FFocusMargin := 1;
  FDown := False;
  FAnimatEvents := [aeGlobalDef];
  FShowFocus := True;
  FReflected := False;
  FContentMargin := 6;
  FCommandLinkFont := TFont.Create;
  FCommandLinkFont.OnChange := CustomFontChanged;
  FCustomCommandLinkFont := False;
  DroppedDown := False;
//  FAnimClickMode := cmFillLight;
{$IFNDEF DELPHI7UP}
  FWordWrap := True;
{$ELSE}
  WordWrap := True;
{$ENDIF}
{$IFNDEF D2009}
  FDropDownMenu := nil;
  FStyle := bsPushButton;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FImageMargins := TImageMargins.Create;
  FImageMargins.OnChange := ImageMarginsChange;
  FCommandLinkHint := '';
  FDisabledImageIndex := -1;
  FHotImageIndex := -1;
  FImageAlignment := iaLeft;
  FImageIndex := -1;
  FPressedImageIndex := -1;
  FSelectedImageIndex := -1;
{$ELSE}
  OldImageIndex := -1;
{$ENDIF}
  FCommonData.OnUpdateLight := UpdateLight;
end;


function TsButton.CurrentImageIndex: integer;
begin
  Result := GlyphIndex;
end;


function TsButton.CurrentState: integer;
begin
  if ((SendMessage(Handle, BM_GETSTATE, 0, 0) and BST_PUSHED = BST_PUSHED) or fGlobalFlag) and
         (SkinData.FMouseAbove or not (csLButtonDown in ControlState) or ((SkinData.SkinManager <> nil) and SkinData.SkinManager.Options.NoMouseHover)) or
           FDown or
             DroppedDown then
    Result := 2
  else
    if not (csDesigning in ComponentState) and ControlIsActive(FCommonData) then
      Result := 1
    else
      Result := integer(Default) * 3
end;


procedure TsButton.CustomFontChanged(Sender: TObject);
begin
  FCommonData.Invalidate(True);
end;


destructor TsButton.Destroy;
begin
{$IFNDEF D2009}
  FreeAndNil(FImageChangeLink);
  FreeAndNil(FImageMargins);
{$ENDIF}
  FreeAndNil(FCommandLinkFont);
  FreeAndNil(FCommonData);
  inherited Destroy;
end;


procedure TsButton.DrawCaption;
var
  R, hR: TRect;
  Al: TAlignment;
  State: integer;
  TranspMode: byte;
  DrawStyle: Cardinal;
begin
  if Canvas = nil then
    Canvas := FCommonData.FCacheBmp.Canvas;

  Canvas.Font.Assign(Font);
  Canvas.Brush.Style := bsClear;
  R := CaptionRect;
  { Calculate vertical layout }
  if Style = bsCommandLink then begin
    Al := taLeftJustify;//Center;
    Canvas.Font.Style := Canvas.Font.Style + [fsBold];
    Canvas.Font.Size := Canvas.Font.Size + 2;
    DrawStyle := DT_EXPANDTABS or DT_WORDBREAK;
  end
  else begin
    if GetWindowLong(Handle, GWL_STYLE) and BS_LEFT <> 0 then
      Al := taLeftJustify
    else
      if GetWindowLong(Handle, GWL_STYLE) and BS_RIGHT <> 0 then
        Al := taRightJustify
      else
        Al := taCenter;

    DrawStyle := DT_EXPANDTABS or DT_VCENTER or DT_NOCLIP;
    if WordWrap then
      DrawStyle := DrawStyle or DT_WORDBREAK
    else
      DrawStyle := DrawStyle or DT_SINGLELINE;
  end;
  if UseRightToLeftReading then
    DrawStyle := DrawStyle or DT_RTLREADING;

  TranspMode := GetTransMode(SkinData);
  if TranspMode = 2 then
    DrawStyle := DrawStyle or DT_ALPHATEXT;

  if ColorText <> clNone then begin
    Canvas.Font.Color := acColorToRGB(ColorText);
    acDrawText(Canvas.Handle, PacChar(Caption), R, DrawStyle or DT_ALPHATEXT * Cardinal(TranspMode = 2));
  end
  else
    acWriteTextEx(Canvas, PacChar(Caption), Enabled or SkinData.Skinned, R, Cardinal(DrawStyle or acAlignmentArray[Al]) or DT_ALPHATEXT * Cardinal(TranspMode = 2), FCommonData, boolean(CurrentState), SkinData.SkinManager);

  if TranspMode = 1 then
    FillAlphaRect(FCommonData.FCacheBmp, R);

  if Style = bsCommandLink then begin
    Canvas.Font.Style := Canvas.Font.Style - [fsBold];
    Canvas.Font.Size := Canvas.Font.Size - 2;
    if CommandLinkHint <> '' then begin
      hR := R;
      hR.Top := R.Bottom + ContentSpacing(Self);
      hR.Bottom := Height - FContentMargin;
      if FCustomCommandLinkFont then begin
        Canvas.Font.Assign(FCommandLinkFont);
        acWriteTextEx(Canvas, PacChar(CommandLinkHint), Enabled or SkinData.Skinned, hR, Cardinal(DrawStyle), -1, boolean(CurrentState), SkinData.SkinManager);
      end
      else
        if ColorText <> clNone then
          acDrawText(Canvas.Handle, PacChar(CommandLinkHint), hR, DrawStyle)
        else
          acWriteTextEx(Canvas, PacChar(CommandLinkHint), Enabled or SkinData.Skinned, hR, Cardinal(DrawStyle), FCommonData, boolean(CurrentState), SkinData.SkinManager);
    end;
    if TranspMode = 1 then
      FillAlphaRect(FCommonData.FCacheBmp, hR);
  end;

  if Focused and
       ((SkinData.SkinManager = nil) or SkinData.SkinManager.ButtonsOptions.ShowFocusRect {$IFDEF DELPHI7UP} or not acThemesEnabled{$ENDIF}) and
         Enabled and
           (Caption <> '') and
             ShowFocus and
               FCommonData.Skinned then begin
    InflateRect(R, FocusMargin + 1, FocusMargin);
    if R.Top < 3 then
      R.Top := 3;

    if R.Bottom > Height - 3 then
      R.Bottom := Height - 3;

    State := min(ac_MaxPropsIndex, SkinData.CommonSkinData.gd[SkinData.SkinIndex].States);
    FocusRect(Canvas, R, SkinData.CommonSkinData.gd[SkinData.SkinIndex].Props[State].FontColor.Color, clNone);
  end;
end;


{$IFDEF D2010}
procedure TsButton.DrawElevation(ACanvas: TCanvas);
var
  iconHandle: HICON;
  ICC: TInitCommonControlsEx;
begin
  if icoElevat = nil then begin
    ICC.dwSize := SizeOf(ICC);
    ICC.dwICC := 0;
    InitCommonControlsEx(ICC);
    if Succeeded(LoadIconWithScaleDown(0, MakeIntResource(IDI_SHIELD), 16, 16, iconHandle)) then begin
      icoElevat := TIcon.Create;
      icoElevat.Handle := iconHandle;
    end;
  end;
  if icoElevat <> nil then
    with GlyphRect do
      ACanvas.Draw(Left, Top, icoElevat);
end;
{$ENDIF}


procedure TsButton.DrawGlyph;
begin
{$IFDEF D2010}
  if ElevationRequired then
    DrawElevation(FCommonData.FCacheBmp.Canvas)
  else
{$ENDIF}
    if (GetImages <> nil) and IsValidIndex(CurrentImageIndex, GetImageCount(GetImages)) then
      DrawBtnGlyph(Self, FCommonData.FCacheBmp.Canvas, clNone);
end;


procedure TsButton.GetColoring(var Coloring: TacColoring);
begin
  Coloring.ColorToneBG := clNone;
  Coloring.ColorGlyph := clNone;
  Coloring.ColorText := clNone;
  if SkinData.ColorTone <> clNone then
    Coloring.ColorToneBG := SkinData.ColorTone
  else begin
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
  end;
  if Assigned(FOnGetColoring) then
    FOnGetColoring(Self, CurrentState, Coloring);
end;


function TsButton.GetImages: TCustomImageList;
begin
  Result := Images;
end;


function TsButton.GlyphExist: boolean;
begin
  Result := (GetImages <> nil) and IsValidIndex(CurrentImageIndex, GetImageCount(GetImages))
{$IFDEF D2010}
  or ElevationRequired
{$ENDIF}
end;


function TsButton.GlyphIndex: integer;
var
  State: integer;
begin
  if not Enabled then
    State := 4
  else
    if CurrentState = 2 then
      State := 2
    else
      State := iff(Focused, 3, CurrentState);

  case State of
    0: Result := ImageIndex;
    1: Result := iff(IsValidIndex(HotImageIndex,      GetImageCount(GetImages)), HotImageIndex,      ImageIndex);
    2: Result := iff(IsValidIndex(PressedImageIndex,  GetImageCount(GetImages)), PressedImageIndex,  ImageIndex);
    3: Result := iff(IsValidIndex(SelectedImageIndex, GetImageCount(GetImages)), SelectedImageIndex, ImageIndex);
    4: Result := iff(IsValidIndex(DisabledImageIndex, GetImageCount(GetImages)), DisabledImageIndex, ImageIndex)
    else Result := -1;
  end;
end;


function TsButton.GlyphRect: TRect;
var
  ImgW, ImgH, aWidth: integer;
begin
  if GlyphExist then begin
    aWidth := Width;
    with ImageSize do begin
      ImgW := cx;
      ImgH := cy;
    end;

    if Style = bsSplitButton then
      dec(aWidth, ScaledDropWidth);

    if Style = bsCommandLink then begin
      Result.Left := ImageMargins.Left + FContentMargin;
      Result.Right := Result.Left + ImgW;
      Result.Top := FContentMargin;
      Result.Bottom := Result.Top + ImgH;
    end
    else
      case ImageAlignment of
        iaLeft: begin
          Result.Left := ImageMargins.Left + FContentMargin;
          Result.Right := Result.Left + ImgW;
          Result.Top := (Height - ImgH) div 2;
          Result.Bottom := Result.Top + ImgH;
        end;

        iaRight: begin
          Result.Right := aWidth - ImageMargins.Right - FContentMargin;
          Result.Left := Result.Right - ImgW;
          Result.Top := (Height - ImgH) div 2;
          Result.Bottom := Result.Top + ImgH;
        end;

        iaTop: begin
          Result.Top := ImageMargins.Bottom + FContentMargin;
          Result.Bottom := Result.Top + ImgH;
          Result.Left := (aWidth - ImgW) div 2;
          Result.Right := Result.Left + ImgW;
        end;

        iaBottom: begin
          Result.Bottom := Height - ImageMargins.Bottom - FContentMargin;
          Result.Top := Result.Bottom - ImgH;
          Result.Left := (aWidth - ImgW) div 2;
          Result.Right := Result.Left + ImgW;
        end

        else begin
          Result.Top := (Height - ImgH) div 2;
          Result.Bottom := Result.Top + ImgH;
          Result.Left := (aWidth - ImgW) div 2;
          Result.Right := Result.Left + ImgW;
        end;
      end;
  end
  else
    Result := MkRect;

  if (CurrentState = 2) and CanClickShift(SkinData.SkinIndex, SkinData.CommonSkinData) then
    OffsetRect(Result, 1, 1);
end;


procedure TsButton.Loaded;
begin
  inherited;
  FCommonData.FCacheBmp.Canvas.Font.Assign(Font);
  FCommonData.Loaded;
end;


procedure TsButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
begin
  if (Style = bsSplitButton) and (DropDownMenu <> nil) and PtInRect(Rect(Width - ScaledDropWidth, 0, Width, Height), Point(X, Y)) then begin
    P := ClientToScreen(MkPoint);
    DroppedDown := True;
    FCommonData.Invalidate;
{$IFNDEF FPC}
    if (DefaultManager <> nil) and DefaultManager.IsActive then
      DefaultManager.SkinableMenus.HookPopupMenu(DropDownMenu, True);
{$ENDIF}
    DropDownMenu.PopupComponent := Self;
    DropDownMenu.Popup(P.X, P.Y + Height);
    DroppedDown := False;
    FCommonData.Invalidate;
  end
  else begin
    if FCommonData.Skinned and Enabled and not (csDesigning in ComponentState) then begin
      FCommonData.FUpdating := False;
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
            DoChangePaint(FCommonData, 2, UpdateWindow_CB, EventEnabled(aeMouseDown, FAnimatEvents), True, False);
        end;

      if ShowHintStored then begin;
        Application.ShowHint := AppShowHint;
        ShowHintStored := False
      end;
    end;
    inherited MouseDown(Button, Shift, X, Y);
  end;
  if (FCommonData.PulsTimer <> nil) and FCommonData.PulsTimer.Enabled then
    FCommonData.PulsTimer.PrepareImage;
end;


procedure TsButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not (csDestroying in ComponentState) then begin
    if FCommonData.Skinned and Enabled and not (csDesigning in ComponentState) then begin
      if (Button = mbLeft) and ShowHintStored then begin
        Application.ShowHint := AppShowHint;
        ShowHintStored := False;
      end;
      if not FMouseClicked or (csDestroying in ComponentState) then
        Exit;

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
            if TimerIsActive(SkinData) then begin
              StopTimer(SkinData);
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
                DoChangePaint(FCommonData, 1, UpdateWindow_CB, EventEnabled(aeMouseUp, FAnimatEvents), True);
            end;
          end;
      end;
    end;
    inherited MouseUp(Button, Shift, X, Y);
  end;
  if not (csDestroying in ComponentState) and (FCommonData.PulsTimer <> nil) and FCommonData.PulsTimer.Enabled then
    FCommonData.PulsTimer.PrepareImage;
end;


procedure TsButton.OurPaintHandler;
var
  aRect: TRect;
  DC, SavedDC: hdc;
  PS: TPaintStruct;
  Timer: TacThreadedTimer;

  procedure UnInit;
  begin
    if aDC <> DC then
      ReleaseDC(Handle, DC);

    if not InanimationProcess then
      EndPaint(Handle, {$IFDEF FPC}@{$ENDIF}PS);
  end;

  procedure CopyImage(Bmp: TBitmap);
  begin
    SavedDC := SaveDC(DC);
    try
      BitBlt(DC, 0, 0, Width, Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
    finally
      RestoreDC(DC, SavedDC);
    end;
  end;

begin
  if InAnimationProcess and ((aDC <> SkinData.PrintDC) or (aDC = 0)) then
    Exit;

  if aDC = 0 then
    DC := GetDC(Handle)
  else
    DC := aDC;

  if not InanimationProcess then
    BeginPaint(Handle, {$IFDEF FPC}@{$ENDIF}PS);

  try
    if not InUpdating(FCommonData) then
      if not TimerIsActive(SkinData) then begin
        if (GetClipBox(DC, aRect) = 0) {or IsRectEmpty(R) is not redrawn while resizing }then begin
          UnInit;
          Exit;
        end;
        FCommonData.BGChanged := FCommonData.BGChanged or FCommonData.HalfVisible;
        FCommonData.HalfVisible := (WidthOf(aRect, True) <> Width) or (HeightOf(aRect, True) <> Height);
        if FCommonData.BGChanged and not PrepareCache then begin
          UnInit;
          Exit;
        end;
        CopyImage(FCommonData.FCacheBmp);
      end
      else begin
        if (SkinData.PulsTimer <> nil) and SkinData.PulsTimer.Enabled {and (SkinData.PulsTimer.BmpOut <> nil) }then
          Timer := SkinData.PulsTimer
        else
          Timer := SkinData.AnimTimer;

        if Timer.BmpOut <> nil then
          CopyImage(Timer.BmpOut);
      end;
  finally
    UnInit;
  end;
end;


function TsButton.PrepareCache: boolean;
var
  CI: TCacheInfo;
  i, State: integer;
//  sm: TsSkinManager;
  BGInfo: TacBGInfo;
  Coloring: TacColoring;
begin
  if FCommonData.SkinIndex >= 0 then begin
    GetBGInfo(@BGInfo, Parent);
    if BGInfo.BgType = btNotReady then begin
      FCommonData.FUpdating := True;
      Result := False;
    end
    else
      with FCommonData.CommonSkinData do begin
        Result := True;
        CI := BGInfoToCI(@BGInfo);
        InitCacheBmp(SkinData);
        FCommonData.FCacheBmp.Canvas.Font.Assign(Font);
//        sm := FCommonData.SkinManager;
        if (CurrentState = 3 {Def/Focused}) and (gd[FCommonData.SkinIndex].States < 4) then
          State := 1
        else
          State := min(CurrentState, gd[FCommonData.SkinIndex].States - 1);
{
        PaintItemBG(FCommonData, CI, State, MkRect(Self), Point(Left, Top), FCommonData.FCacheBMP, 0, 0);
        if sm.IsValidImgIndex(gd[SkinData.SkinIndex].BorderIndex) then begin
          if (State <> 0) or (ma[gd[SkinData.SkinIndex].BorderIndex].DrawMode and BDM_ACTIVEONLY = 0) then begin
            inc(CI.X, Left);
            inc(CI.Y, Top);
            DrawSkinRect(FCommonData.FCacheBmp, MkRect(FCommonData.FCacheBmp), CI, ma[gd[SkinData.SkinIndex].BorderIndex], State, False);//True);
            dec(CI.X, Left);
            dec(CI.Y, Top);
          end;
        end
        else
          TryPaintBorder(FCommonData.FCacheBmp, MkRect(FCommonData.FCacheBmp), FCommonData, State, CI);
}
        PaintItem(SkinData, CI, True, State, MkRect(Self), Point(Left, Top), FCommonData.FCacheBMP, False, 0, 0, False);

        HandleLighting(FCommonData, State);

        GetColoring(Coloring);
        UpdateBmpColors(FCommonData.FCacheBmp, SkinData, True, State, Coloring.ColorToneBG);
        DrawCaption(Coloring.ColorText, nil);
        DrawGlyph(Coloring);
        if Style = bsSplitButton then
          with FCommonData, FCacheBmp.Canvas do begin
            i := integer(State <> 0);
            Pen.Color := ColorToRGB(gd[SkinIndex].Props[i].FontColor.Color);
            Pen.Width := 1;
            acPaintLine(FCacheBmp.Canvas.Handle, Width - ScaledDropWidth + 1, 4, Width - ScaledDropWidth + 1, Height - 4);
            DrawColorArrow(FCacheBmp, Pen.Color, Rect(Width - ScaledDropWidth, 0, Width, Height), asBottom, GetPPI(SkinData));
          end;

        if not Enabled or ((Action <> nil) and not Assigned(TAction(Action).OnExecute){ not TAction(Action).Enabled // Button not repainted immediately if Action.Enabled changed }) then
          BmpDisabledKind(FCommonData.FCacheBmp, FDisabledKind, Parent, CI, Point(Left, Top));

        FCommonData.BGChanged := False;
      end;
  end
  else
    Result := False;
end;


procedure TsButton.SetDisabledKind(const Value: TsDisabledKind);
begin
  if FDisabledKind <> Value then begin
    FDisabledKind := Value;
    if not (csLoading in ComponentState) then
      FCommonData.Invalidate;
  end;
end;


{$IFNDEF DELPHI7UP}
procedure TsButton.SetWordWrap(const Value: boolean);
begin
  if FWordWrap <> Value then begin
    FWordWrap := Value;
    if not (csLoading in ComponentState) then
      FCommonData.Invalidate;
  end;
end;
{$ENDIF}


procedure TsButton.SetFocusMargin(const Value: integer);
begin
  if FFocusMargin <> Value then
    FFocusMargin := Value;
end;


procedure TsButton.WMKeyUp(var Message: TWMKey);
begin
  inherited;
  if Assigned(FCommonData) and FCommonData.Skinned and (Message.CharCode = 32) then begin
    FCommonData.BGChanged := True;
    Repaint;
  end;
end;


procedure TsButton.WMNCHitTest(var Message: TWMNCHitTest);
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


procedure TsButton.WndProc(var Message: TMessage);
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
      end; // AlphaSkins supported

      AC_SETNEWSKIN:
        if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
          CommonMessage(Message, FCommonData);
          Exit;
        end;

      AC_REMOVESKIN:
        if (ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager)) and not (csDestroying in ComponentState) then begin
          CommonMessage(Message, FCommonData);
          Repaint;
          Exit;
        end;

      AC_REFRESH:
        if RefreshNeeded(SkinData, Message) then begin
          SetWindowRgn(Handle, 0, False);
          CommonMessage(Message, FCommonData);
          if SkinData.PrintDC = 0 then
            Repaint;

          Exit
        end;

      AC_PREPARECACHE: begin
        PrepareCache;
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

  if not ControlIsReady(Self) or (FCommonData = nil) or not FCommonData.Skinned(True) then begin
    inherited;
    case Message.Msg of
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

      CM_ENABLEDCHANGED:
        if (Visible or (csDesigning in ComponentState)) and not (csDestroying in ComponentState) then
          Repaint;

      CM_VISIBLECHANGED: 
        SetButtonStyle(Default);
    end;
  end
  else begin
    case Message.Msg of
      SM_ALPHACMD:
        case Message.WParamHi of
          AC_ENDPARENTUPDATE:
            if FCommonData.FUpdating then begin
              if not InUpdating(FCommonData, True) then
                Repaint;

              Exit
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
        if Enabled and not (csDesigning in ComponentState) then
          if not FCommonData.FMouseAbove and not SkinData.SkinManager.Options.NoMouseHover then begin
            FreeAndNil(FCommonData.LightMask);
            if Assigned(FOnMouseEnter) then
              FOnMouseEnter(Self);

            FCommonData.BGChanged := False;
            FCommonData.FMouseAbove := True;
            DoChangePaint(FCommonData, 1, UpdateWindow_CB, EventEnabled(aeMouseEnter, FAnimatEvents), False);
          end;

      CM_MOUSELEAVE:
        if Enabled and not (csDesigning in ComponentState) then begin
          FreeAndNil(FCommonData.LightMask);
          if Assigned(FOnMouseLeave) then
            FOnMouseLeave(Self);

          SkinData.DestroyNeedlessLight(False);
          FCommonData.BGChanged := False;
          FCommonData.FMouseAbove := False;
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

      WM_SIZE: begin
        HideGlow(SkinData.GlowID);
      end;

      WM_SETTEXT: begin
        if not (csDesigning in ComponentState) then begin
          SetRedraw(Handle, 0);
          inherited;
          SetRedraw(Handle, 1);
        end
        else
          inherited;

        SkinData.Invalidate(True);
        Exit;
      end;

      WM_UPDATEUISTATE:
        if Visible or (csDesigning in ComponentState) then begin
          Message.Result := 1;
          Exit;
        end;

      WM_ERASEBKGND:
        if Visible or (csDesigning in ComponentState) then begin
          if (TWMPaint(Message).DC <> 0) and (Skindata.FCacheBmp <> nil) and not InUpdating(SkinData) then begin
            if FCommonData.BGChanged then
              PrepareCache;

            if not FCommonData.BGChanged and not TimerIsActive(SkinData) then
              CopyWinControlCache(Self, FCommonData, MkRect, MkRect(Self), TWMPaint(Message).DC, False);
          end;
          Message.Result := 0;
          Exit;
        end;

      CM_TEXTCHANGED:
        if not (csDestroying in ComponentState) then begin
          FinishTimer(SkinData.AnimTimer);
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
          if (Parent = nil) then Exit;
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
        with FCommonData.CommonSkinData.gd[FCommonData.SkinIndex] do
          if (Props[0].Transparency > 0) or ((States > 0) and (Props[1].Transparency > 0) and ControlIsActive(FCommonData)) then begin
            FCommonData.BGChanged := True;
            Repaint
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
              FCommonData.Invalidate;
              HideGlow(SkinData.GlowID);
            end;
            if FCommonData.OuterEffects.Visibility = ovFocused then
              FCommonData.OuterEffects.Invalidate;
          end
          else
            inherited;

          Exit
        end;

      CM_FOCUSCHANGED:
        if Visible then
          if not bFocusChanging and Focused { Desc 7} then begin // Avoiding of blinking
            Perform(WM_SETREDRAW, 0, 0);
            inherited;
            Perform(WM_SETREDRAW, 1, 0);
            Repaint;
          end
          else
            inherited;

      WM_ENABLE:
        Exit; // Avoiding of blinking when switched

      BM_SETSTATE: {$IFDEF DELPHI7UP} if not acThemesEnabled then {$ENDIF} begin // Avoid std drawing without manifest
        SetRedraw(Handle, 0);
        inherited;
        SetRedraw(Handle, 1);
        Exit;
      end;
    end;
    if CommonWndProc(Message, FCommonData) then
      Exit;

    inherited;
    if not (csDestroying in ComponentState) then
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
          begin
            FCommonData.BGChanged := True;
            FCommonData.FUpdating := False;
            if Visible or (csDesigning in ComponentState) then begin
              SetButtonStyle(Default);
              Repaint;
            end
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


procedure TsButton.CNDrawItem(var Message: TWMDrawItem);
begin
  if not SkinData.Skinned then
    StdDrawItem(TDrawItemStruct(Message.DrawItemStruct^))
  else
    inherited;
end;


procedure TsButton.StdDrawItem(const DrawItemStruct: TDrawItemStruct);
var
  IsDown, IsDefault: Boolean;
  R: TRect;
  Flags: Cardinal;
{$IFDEF DELPHI7UP}
  Details: TThemedElementDetails;
  Button: TThemedButton;
{$ENDIF}
  Canvas: TCanvas;
  Coloring: TacColoring;
begin
  Canvas := TCanvas.Create;
  try
    R := ClientRect;
    with DrawItemStruct do begin
      Canvas.Handle := {$IFDEF FPC}_hDC{$ELSE}hDC{$ENDIF};
      Canvas.Font := Self.Font;
      IsDown := itemState and ODS_SELECTED <> 0;
      IsDefault := itemState and ODS_FOCUS <> 0;
    end;
{$IFDEF DELPHI7UP}
    if acThemesEnabled then begin
      if not Enabled then
        Button := tbPushButtonDisabled
      else
        if IsDown then
          Button := tbPushButtonPressed
        else
          if SkinData.FMouseAbove then
            Button := tbPushButtonHot
          else
            if IsFocused or IsDefault then
              Button := tbPushButtonDefaulted
            else
              Button := tbPushButtonNormal;

      Details := acThemeServices.GetElementDetails(Button);
      // Parent background
      acThemeServices.DrawParentBackground(Handle, DrawItemStruct.hDC, @Details, True);
      // Button shape
      acThemeServices.DrawElement(DrawItemStruct.hDC, Details, DrawItemStruct.rcItem);
  {$IFDEF DELPHI_XE2}
      acThemeServices.GetElementContentRect(Canvas.Handle, Details, DrawItemStruct.rcItem, R);
  {$ELSE}
      R := acThemeServices.ContentRect(Canvas.Handle, Details, DrawItemStruct.rcItem);
  {$ENDIF}
      if IsFocused and IsDefault and ShowFocus then begin
        if Style = bsSplitButton then
          R.Right := R.Right - ScaledDropWidth + 2;

        acDrawFocusRect(Canvas, R);
      end;
    end
    else
{$ENDIF} // DELPHI7UP
    begin
      Flags := DFCS_BUTTONPUSH or DFCS_ADJUSTRECT;
      if IsDown then
        Flags := Flags or DFCS_PUSHED;

      if DrawItemStruct.itemState and ODS_DISABLED <> 0 then
        Flags := Flags or DFCS_INACTIVE;
      { DrawFrameControl doesn't allow for drawing a button as the default button, so it must be done here. }
      if IsFocused or IsDefault then begin
        Canvas.Pen.Color := clWindowFrame;
        Canvas.Pen.Width := 1;
        Canvas.Brush.Style := bsClear;
        Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
        { DrawFrameControl must draw within this border }
        InflateRect(R, -1, -1);
      end;
      { DrawFrameControl does not draw a pressed button correctly }
      if IsDown then begin
        Canvas.Pen.Color := clBtnShadow;
        Canvas.Pen.Width := 1;
        Canvas.Brush.Color := clBtnFace;
        Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
        InflateRect(R, -1, -1);
      end
      else
        DrawFrameControl(DrawItemStruct.{$IFDEF FPC}_hDC{$ELSE}hDC{$ENDIF}, R, DFC_BUTTON, Flags);

      if IsFocused then begin
        R := ClientRect;
        InflateRect(R, -1, -1);
      end;
      Canvas.Font := Self.Font;
      if IsDown then
        OffsetRect(R, 1, 1);

      if IsFocused and IsDefault and ShowFocus{ ((SkinData.SkinManager = nil) or SkinData.SkinManager.ButtonsOptions.ShowFocusRect)} then begin
        R := ClientRect;
        InflateRect(R, -4, -4);
        Canvas.Pen.Color := clWindowFrame;
        Canvas.Brush.Color := clBtnFace;
        if Style = bsSplitButton then
          R.Right := R.Right - ScaledDropWidth + 2;

        DrawFocusRect(Canvas.Handle, R);
      end;
    end;
    GetColoring(Coloring);

    DrawCaption(Coloring.ColorText, Canvas);
{$IFDEF D2010}
    if ElevationRequired then
      DrawElevation(Canvas)
    else
{$ENDIF}
      DrawBtnGlyph(Self, Canvas, clNone);

    if Style = bsSplitButton then begin
      Canvas.Pen.Color := clWindowFrame;
      Canvas.Pen.Width := 1;
      Canvas.MoveTo(Width - ScaledDropWidth + 1, 4);
      Canvas.LineTo(Width - ScaledDropWidth + 1, Height - 4);
      DrawColorArrow(Canvas, Font.Color, Rect(Width - ScaledDropWidth, 0, Width, Height), asBottom, GetPPI(SkinData));
    end;
    Canvas.Handle := 0;
  finally
    FreeAndNil(Canvas);
  end;
end;


procedure TsButton.UpdateLight(Sender: TObject; Data: PacAnimEventData);
begin
  if IsWindowVisible(Handle) and Enabled and EventEnabled(aeLighting, FAnimatEvents) then
    DoUpdateLight(SkinData, Data.DoOutput)
  else
    SkinData.FDoLighting := False;

  SkinData.DestroyNeedlessLight;
  Data.DoLighting := SkinData.DoLighting;
end;


{$IFDEF D2009}
procedure TsButton.UpdateImageList;
begin
  inherited;
  if not (csDestroying in ComponentState) then
    FCommonData.Invalidate;
end;


procedure TsButton.UpdateImages;
begin
  inherited;
end;
{$ENDIF}


procedure TsButton.SetStyle(const Value: TButtonStyle);
begin
  if FStyle <> Value then begin
    FStyle := Value;
{$IFDEF D2009}
    inherited Style := TCustomButton.TButtonStyle(Value);
    SetButtonStyle(Default);
{$ENDIF}
    FCommonData.Invalidate;
  end;
end;


procedure TsButton.SetContentMargin(const Value: integer);
begin
  if FContentMargin <> Value then begin
    FContentMargin := Value;
    FCommonData.Invalidate;
  end;
end;


procedure TsButton.SetCommandLinkFont(const Value: TFont);
begin
  FCommandLinkFont.Assign(Value);
  CustomFontChanged(Self);
end;


function TsButton.IsCmdFontStored: boolean;
begin
  Result := IsCustomFont(nil, FCommandLinkFont);
end;


function TsButton.ImageSize: TSize;
begin
{$IFDEF D2010}
  if ElevationRequired then
    Result := MkSize(16, 16)
  else
{$ENDIF}
    if (GetImages <> nil) and IsValidIndex(CurrentImageIndex, GetImageCount(GetImages)) then
      Result := MkSize(GetImageWidth(GetImages, -1, GetPPI(SkinData), True), GetImageHeight(GetImages, -1, GetPPI(SkinData), True))
    else
      Result := MkSize(0, 0)
end;


{$IFNDEF D2009}
function TsButton.IsImageIndexStored: Boolean;
begin
  Result := (ActionLink = nil) or not TPushButtonActionLink(ActionLink).IsImageIndexLinked;
end;


procedure TsButton.SetImageAlignment(const Value: TImageAlignment);
begin
  if Value <> FImageAlignment then begin
    FImageAlignment := Value;
    if GetImages <> nil then
      FCommonData.Invalidate;
  end;
end;


procedure TsButton.SetImgIndex(const Value: TImageIndex);
begin
  if FImageIndex <> Value then begin
    FImageIndex := Value;
    if GetImages <> nil then
      FCommonData.Invalidate;
  end;
end;


procedure TsButton.SetImageIndex(const Index: integer; const Value: TImageIndex);

  procedure ChangeProp(var Prop: TImageIndex; Value: TImageIndex);
  begin
    if Prop <> Value then begin
      Prop := Value;
      if GetImages <> nil then
        FCommonData.Invalidate;
    end;
  end;

begin
  case Index of
    1: ChangeProp(FHotImageIndex,      Value);
    2: ChangeProp(FDisabledImageIndex, Value);
    3: ChangeProp(FPressedImageIndex,  Value);
    4: ChangeProp(FSelectedImageIndex, Value);
  end;
end;


procedure TsButton.SetImageMargins(const Value: TImageMargins);
begin
  FImageMargins.Assign(Value);
end;


procedure TsButton.SetImages(const Value: TCustomImageList);
begin
  if Value <> FImages then begin
    if Images <> nil then
      Images.UnRegisterChanges(FImageChangeLink);

    FImages := Value;
    if Images <> nil then begin
      Images.RegisterChanges(FImageChangeLink);
      Images.FreeNotification(Self);
    end;
    FCommonData.Invalidate;
  end;
end;


procedure TsButton.ImageMarginsChange(Sender: TObject);
begin
  if GetImages <> nil then
    FCommonData.Invalidate;
end;


procedure TsButton.ImageListChange(Sender: TObject);
begin
  if Images <> nil then
    FCommonData.Invalidate;
end;


procedure TsButton.SetCommandLinkHint(const Value: acString);
begin
  if FCommandLinkHint <> Value then begin
    FCommandLinkHint := Value;
    FCommonData.Invalidate;
  end;
end;


procedure TsButton.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if AComponent = Images then
      Images := nil
{$IFNDEF D2009}
    else
      if AComponent = DropDownMenu then
        DropDownMenu := nil;
{$ENDIF}
end;


procedure TImageMargins.Assign(Source: TPersistent);
begin
  if Source is TImageMargins then begin
    FLeft   := TImageMargins(Source).Left;
    FTop    := TImageMargins(Source).Top;
    FRight  := TImageMargins(Source).Right;
    FBottom := TImageMargins(Source).Bottom;
    Change;
  end
  else
    inherited Assign(Source);
end;


procedure TImageMargins.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;


procedure TImageMargins.SetMargin(Index, Value: Integer);

  procedure ChangeProp(var Prop: Integer; Value: Integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      Change;
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


function TPushButtonActionLink.IsImageIndexLinked: Boolean;
begin
  Result := inherited IsImageIndexLinked and (TsButton(FClient).ImageIndex = (Action as TCustomAction).ImageIndex);
end;


procedure TPushButtonActionLink.SetImageIndex(Value: Integer);
begin
  if IsImageIndexLinked then
    TsButton(FClient).ImageIndex := Value;
end;


{$ELSE}
procedure TsButton.SetImageIndex(const Value: TImageIndex);
begin
  inherited ImageIndex := Value;
  FCommonData.Invalidate(True);
end;


function TsButton.GetImageIndex: TImageIndex;
begin
  Result := inherited ImageIndex;
end;


type
  TAccessActionLink = class(TPushButtonActionLink);

function TsButton.IsImageIndexStored: Boolean;
begin
  Result := (ActionLink = nil) or not TAccessActionLink(ActionLink).IsImageIndexLinked;
end;
{$ENDIF}


initialization

finalization
{$IFDEF D2010}
  if icoElevat <> nil then
    FreeAndNil(icoElevat);
{$ENDIF}

end.


