unit sGroupBox;
{$I sDefs.inc}
//{$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, ImgList,
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  {$IFNDEF ALITE}sRadioButton, {$ENDIF}
  {$IFDEF TNTUNICODE} TntGraphics, TntControls, TntActnList, TntClasses, TntSysUtils, TntForms, TntStdCtrls, {$ENDIF}
  sCommonData, sConst;


type
{$IFNDEF NOTFORHELP}
  TsCaptionLayout = (clTopLeft, clTopCenter, clTopRight);
  TacChangeState = (csNone, csAsChecked, csAsNotChecked);
  TacBoxStyle = (bsDefault, bsStandard, bsCard);

  TsMargin = class(TPersistent)
  private
    FTop,
    FLeft,
    FRight,
    FBottom: TacIntProperty;

    FControl: TControl;
    procedure SetMargin(const Index: Integer; Value: TacIntProperty);
  public
    constructor Create(Control: TControl);
  published
    property Left:   TacIntProperty index 0 read FLeft   write SetMargin default 2;
    property Top:    TacIntProperty index 1 read FTop    write SetMargin default 0;
    property Right:  TacIntProperty index 2 read FRight  write SetMargin default 2;
    property Bottom: TacIntProperty index 3 read FBottom write SetMargin default 0;
  end;
{$ENDIF}


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
{$IFDEF TNTUNICODE}
  TsGroupBox = class(TTntGroupBox)
{$ELSE}
  TsGroupBox = class(TGroupBox)
{$ENDIF}
{$IFNDEF NOTFORHELP}
  private
    FChecked,
    FCheckBoxVisible: boolean;

    FImageIndex,
    FCaptionYOffset: integer;

    FCaptionMargin: TsMargin;
    FCommonData: TsCommonData;
    FCaptionSkin: TsSkinSection;
    FCaptionWidth: TacIntProperty;
    FCaptionLayout: TsCaptionLayout;

    FImages: TCustomImageList;
    FImageChangeLink: TChangeLink;
    FOnCheckBoxChanged: TNotifyEvent;
    FAutoChildrenStates: TacChangeState;
    FBoxStyle: TacBoxStyle;
    FMouseInGlyph: boolean;
    FCheckBoxRadio: boolean;
    procedure CaptionChanged; virtual;
    procedure WMFontChanged   (var Message: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure SetCaptionSkin  (const Value: TsSkinSection);
    procedure SetCaptionWidth (const Value: TacIntProperty);
    procedure SetCaptionLayout(const Value: TsCaptionLayout);
    procedure SetImages       (const Value: TCustomImageList);

    procedure SetInteger(const Index: Integer; const Value: integer); virtual;
    procedure SetBoolean(const Index: Integer; const Value: boolean); virtual;
    procedure SetBoxStyle(const Value: TacBoxStyle);
    procedure SetMouseInGlyph(const Value: boolean);
    function GlyphScreenRect: TRect;
  protected
    CheckHot,
    CheckPressed: boolean;

    TopRect,
    TextRect,
    GlyphRect,
    CheckBoxRect: TRect;

    TextIndex: integer;

    CaptionRect: TRect;
    CaptionHeight: integer;
    TranspMode: byte;

    procedure PaintToDC(DC: hdc);
    function ImageExists: boolean;
    procedure WriteText(R: TRect);
    function PrepareCache: boolean;
    function ModernStyle: boolean;

    procedure PaintCaptionArea(cRect: TRect; const CI: TCacheInfo; AState: integer);
    procedure InitPaintData;
    function TextHeight: integer;
    function CheckBoxIndex: integer;
    function CheckBoxWidth: integer;
    function CheckBoxHeight: integer;
    function MouseInCheckBox: boolean;
    function GetCaptionSkinIndex: integer;
    procedure CheckBoxChanged; virtual;
    procedure ImageListChange(Sender: TObject);
    procedure WndProc(var Message: TMessage); override;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure PaintCheckBoxSkin(Bmp: TBitmap; R: TRect; State: integer);
    procedure PaintCheckBoxStd (DC:  hdc;     R: TRect; State: integer);
    procedure RepaintCheckBox(State: integer; DoAnimation: boolean = False);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure Paint; override;
    procedure Loaded; override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    constructor Create(AOwner: TComponent); override;
    procedure UpdateAccessibilities;
    property MouseInGlyph: boolean read FMouseInGlyph write SetMouseInGlyph;
  published
    property AutoSize;
{$ENDIF} // NOTFORHELP
    property CaptionLayout: TsCaptionLayout read FCaptionLayout write SetCaptionLayout default clTopLeft;
    property CaptionMargin: TsMargin read FCaptionMargin write FCaptionMargin;
    property SkinData: TsCommonData read FCommonData write FCommonData;
    property CaptionSkin: TsSkinSection read FCaptionSkin write SetCaptionSkin;
    property CaptionWidth: TacIntProperty read FCaptionWidth write SetCaptionWidth default 0;
    property Images: TCustomImageList read FImages write SetImages;

    property AutoChildrenStates: TacChangeState read FAutoChildrenStates write FAutoChildrenStates default csNone;

    property CaptionYOffset: integer index 0 read FCaptionYOffset write SetInteger default 0;
    property ImageIndex:     integer index 1 read FImageIndex     write SetInteger default -1;

    property CheckBoxVisible: boolean index 0 read FCheckBoxVisible write SetBoolean default False;
    property Checked:         boolean index 1 read FChecked         write SetBoolean default False;
    property CheckBoxRadio:   boolean index 2 read FCheckBoxRadio   write SetBoolean default False;

    property BoxStyle: TacBoxStyle read FBoxStyle write SetBoxStyle default bsDefault;
{$IFDEF D2010}
    property Touch;
{$ENDIF}
    property OnCheckBoxChanged: TNotifyEvent read FOnCheckBoxChanged write FOnCheckBoxChanged;
{$IFNDEF NOTFORHELP}
    property OnKeyDown;
    property OnKeyPress;
{$ENDIF} // NOTFORHELP
  end;

{$IFNDEF ALITE}

{$IFNDEF NOTFORHELP}
  TacIndexChangingEvent = procedure(Sender: TObject; NewIndex: Integer; var AllowChange: Boolean) of object;
{$ENDIF}

{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]{$ENDIF}
  TsRadioGroup = class(TsGroupBox)
{$IFNDEF NOTFORHELP}
  private
    FReading,
    FUpdating,
    fArranging,
    FShowFocus,
    FDisableItemsIfUnchecked: boolean;

    FColumns,
    FItemIndex,
    FContentVOffset: integer;

    FOnChange: TNotifyEvent;
    FAnimatEvents: TacAnimatEvents;
    FOnChanging: TacIndexChangingEvent;
    FItems: {$IFDEF TNTUNICODE}TTntStrings{$ELSE}TStrings{$ENDIF};
    FContentHOffset: integer;

    procedure CaptionChanged; override;
    procedure UpdateButtons;
    procedure ArrangeButtons;
    procedure ButtonClick(Sender: TObject);
    procedure ItemsChange(Sender: TObject);
    procedure SetButtonCount(Value: Integer);
    procedure WMSize          (var Message: TWMSize);  message WM_SIZE;
    procedure CMFontChanged   (var Message: TMessage); message CM_FONTCHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    function GetButtons(Index: Integer): TsRadioButton;
    procedure SetItems(Value: {$IFDEF TNTUNICODE}TTntStrings{$ELSE}TStrings{$ENDIF});
    procedure SetInteger(const Index: integer; const Value: integer); override;
    procedure SetDisableItemsIfUnchecked(const Value: boolean);
  protected
    FButtons: TList;
    procedure ReadState(Reader: TReader); override;
    procedure WndProc(var Message: TMessage); override;
    procedure CheckBoxChanged; override;
    function CanModify(NewIndex: integer): boolean; virtual;
  public
    procedure CreateWnd; override;
    procedure Loaded; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FlipChildren(AllLevels: boolean); override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    property Buttons[Index: Integer]: TsRadioButton read GetButtons;
  published
    property AnimatEvents: TacAnimatEvents read FAnimatEvents write FAnimatEvents default [aeGlobalDef];
{$ENDIF} // NOTFORHELP
    property DisableItemsIfUnchecked: boolean read FDisableItemsIfUnchecked write SetDisableItemsIfUnchecked default False;
    property Columns:        integer index 10 read FColumns        write SetInteger default 1;
    property ItemIndex:      integer index 11 read FItemIndex      write SetInteger default -1;
    property ContentVOffset: integer index 12 read FContentVOffset write SetInteger default 0;
    property ContentHOffset: integer index 13 read FContentHOffset write SetInteger default 0;
    property Items: {$IFDEF TNTUNICODE}TTntStrings{$ELSE}TStrings{$ENDIF} read FItems write SetItems;
    property ShowFocus: boolean read FShowFocus write FShowFocus default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChanging: TacIndexChangingEvent read FOnChanging write FOnChanging;
  end;
{$ENDIF} // ALITE

implementation

uses
  math,
  {$IFDEF TNTUNICODE}TntWindows, {$ENDIF}
  {$IFDEF LOGGED}sDebugMsgs, {$ENDIF}
  {$IFDEF DELPHI7UP}Themes, {$ENDIF}
  acntUtils, sStyleSimply, sMessages, sVCLUtils, sGraphUtils, sSkinManager, sAlphaGraph,
  acGlow, acntTypes, sDefaults, acThdTimer, sThirdParty, acAlphaImageList, acAnimation;


const
  xOffset = 6;


function UpdateCheckBox_CB(Data: TObject; iIteration: integer): boolean;
var
  sd: TsCommonData;
  gb: TsGroupBox;
  cRect: TRect;
  Bmp: TBitmap;
  Alpha: byte;
  DC: HDC;
begin
  Result := False;
  if Data is TsCommonData then begin
    sd := TsCommonData(Data);
    if sd.FOwnerControl is TsGroupBox then begin
      gb := TsGroupBox(sd.FOwnerControl);
      cRect := SumRects(gb.CaptionRect, gb.CheckBoxRect);
      Bmp := CreateBmp32(cRect);
      BitBlt(Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, sd.AnimTimer.BmpFrom.Canvas.Handle, 0, 0, SRCCOPY);

      with sd.AnimTimer do
        case State of
          0, 2: Glow := Glow - GlowStep;
          1:    Glow := Glow + GlowStep
          else  Glow := MaxByte - (Iteration / Iterations) * MaxByte;
        end;

      Alpha := LimitIt(Round(sd.AnimTimer.Glow), 0, MaxByte);
      SumBmpRect(Bmp, sd.FCacheBmp, iff(sd.AnimTimer.State = 1, MaxByte - Alpha, Alpha), cRect, MkPoint);
      DC := GetDC(gb.Handle);
      try
        BitBlt(DC, cRect.Left, cRect.Top, WidthOf(cRect), HeightOf(cRect), Bmp.Canvas.Handle, 0, 0, SRCCOPY);
      finally
        ReleaseDC(gb.Handle, DC);
      end;
      Bmp.Free;
      with sd.AnimTimer do
        if sd.AnimTimer.Iteration >= sd.AnimTimer.Iterations then begin
          if (State = 0) and (Alpha > 0) then begin
            Iteration := Iteration - 1;
            UpdateCheckBox_CB(Data, iIteration);
          end
          else
            if State = 0 then
              StopTimer(sd);
        end
        else
          Result := True;
    end;
  end;
end;


procedure TsGroupBox.SetMouseInGlyph(const Value: boolean);
begin
  if FMouseInGlyph <> Value then begin
    FMouseInGlyph := Value;
    SkinData.FMouseAbove := True;
    if Value then begin
      if (SkinData.CircleID < 0) and SkinData.Skinned and CircleClickMode(Self, SkinData, [aeGlobalDef]{[aeClick]}) then
        ShowCircleIfNeeded(SkinData, MaxByte, True {If possible});
    end
    else
      if not (csLButtonDown in SkinData.FOwnerControl.ControlState) then
        HideCircle(SkinData.CircleID);
  end;
end;


function TsGroupBox.GlyphScreenRect: TRect;
var
  i: integer;
begin
  Result := CheckBoxRect;
  if (SkinData.SkinManager <> nil) and (SkinData.SkinManager.AnimEffects.Buttons.ClickEffect <> ceWaveOut) then begin
    i := MaxCircleRadius(SkinData) - WidthOf(Result) div 2;
    InflateRect(Result, i, i);
  end;
  with ClientToScreen(MkPoint) do
    OffsetRect(Result, X, Y);
end;


procedure TsGroupBox.AdjustClientRect(var Rect: TRect);
begin
  if not (csDestroying in ComponentState) then begin
    inherited AdjustClientRect(Rect);
    inc(Rect.Top, CaptionYOffset);
    if ModernStyle then
      inc(Rect.Top, SkinData.CommonSkinData.Spacing);
  end;
end;


procedure TsGroupBox.AfterConstruction;
begin
  inherited;
  FCommonData.Loaded;
end;


procedure EnableDisable(Ctrl: TControl; Data: integer);
begin
  Ctrl.Enabled := boolean(Data);
end;


procedure TsGroupBox.CaptionChanged;
begin
//
end;

procedure TsGroupBox.CheckBoxChanged;
begin
  if Assigned(OnCheckBoxChanged) then
    SkinData.BeginUpdate;

  UpdateAccessibilities;
  if Assigned(OnCheckBoxChanged) then begin
    OnCheckBoxChanged(Self);
    SkinData.EndUpdate(True);
  end;
end;


function TsGroupBox.CheckBoxHeight: integer;
var
  GlyphNdx: integer;
begin
  if SkinData.Skinned then begin
    GlyphNdx := CheckBoxIndex;
    if SkinData.SkinManager.IsValidImgIndex(GlyphNdx) then
      Result := FCommonData.CommonSkinData.ma[GlyphNdx].Height
    else
      Result := 0;
  end
  else
{$IFDEF DELPHI7UP}
    if acThemesEnabled then
      Result := 13 * GetPPI(SkinData) div 96
    else
{$ENDIF}
      Result := 13 * GetPPI(SkinData) div 96;
end;


function TsGroupBox.CheckBoxIndex: integer;
const
  GlyphStates: array [boolean] of TCheckBoxState = (cbUnchecked, cbChecked);
begin
    if FCheckBoxRadio then
      Result := FCommonData.CommonSkinData.RadioButton[Checked]
    else
      Result := FCommonData.CommonSkinData.CheckBox[GlyphStates[Checked]];
end;


function TsGroupBox.CheckBoxWidth: integer;
var
  GlyphNdx: integer;
begin
  if SkinData.Skinned then begin
    GlyphNdx := CheckBoxIndex;
    if SkinData.SkinManager.IsValidImgIndex(GlyphNdx) then
      Result := FCommonData.CommonSkinData.ma[GlyphNdx].Width
    else
      Result := 0;
  end
  else
{$IFDEF DELPHI7UP}
    if acThemesEnabled then
      Result := 16 * GetPPI(SkinData) div 96
    else
{$ENDIF}
      Result := 13 * GetPPI(SkinData) div 96;
end;

constructor TsMargin.Create(Control: TControl);
begin
  inherited Create;
  FControl := Control;
  FLeft   := 2;
  FRight  := 2;
  FTop    := 0;
  FBottom := 0;
end;


procedure TsMargin.SetMargin(const Index: Integer; Value: TacIntProperty);

  procedure ChangeProp(var Prop: TacIntProperty; Value: TacIntProperty);
  begin
    if Prop <> Value then begin
      Prop := Value;
      if not (csLoading in FControl.ComponentState) and (FControl is TsGroupBox) then begin
        TsGroupBox(FControl).CaptionChanged;
        TsGroupBox(FControl).SkinData.Invalidate;
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


constructor TsGroupBox.Create(AOwner: TComponent);
begin
  FCommonData := TsCommonData.Create(Self, True);
  FCommonData.COC := COC_TsGroupBox;
  inherited Create(AOwner);
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := ImageListChange;
  FCaptionLayout := clTopLeft;
  FAutoChildrenStates := csNone;
  FCaptionYOffset := 0;
  FCaptionMargin := TsMargin.Create(Self);
  FCaptionWidth := 0;
  FCheckBoxVisible := False;
  FImageIndex := -1;
  FChecked := False;
  FCheckBoxRadio := False;
  FBoxStyle := bsDefault;
  CheckPressed := False;
  CheckHot := False;
end;


destructor TsGroupBox.Destroy;
begin
  FreeAndNil(FCommonData);
  FreeAndNil(FImageChangeLink);
  FCaptionMargin.Free;
  inherited Destroy;
end;


function TsGroupBox.GetCaptionSkinIndex: integer;
begin
  if FCommonData.SkinManager <> nil then
    if ModernStyle then
      if FCaptionSkin = '' then
        Result := FCommonData.SkinManager.SkinCommonInfo.Sections[ssBarTitle]
      else
        Result := FCommonData.SkinManager.GetSkinIndex(FCaptionSkin)
    else
      if (Caption <> '') or ImageExists or CheckBoxVisible then
        if FCaptionSkin = '' then
          Result := FCommonData.SkinManager.SkinCommonInfo.Sections[ssTransparent]
        else
          Result := FCommonData.SkinManager.GetSkinIndex(FCaptionSkin)
      else
        Result := -1
  else
    Result := -1
end;


function TsGroupBox.ImageExists: boolean;
begin
  Result := (Images <> nil) and IsValidIndex(FImageIndex, GetImageCount(Images));
end;


procedure TsGroupBox.ImageListChange(Sender: TObject);
begin
  FCommonData.Invalidate;
end;


procedure TsGroupBox.InitPaintData;
var
  iPPI,
  iSpacing,
  iImgWidth,
  iImgHeight,
  iTextWidth,
  iCheckWidth,
  iCheckHeight,
  iContentWidth: integer;
  R: TRect;
begin
  InitCacheBmp(SkinData);
  TranspMode := GetTransMode(SkinData, GetCaptionSkinIndex);

  iPPI := GetPPI(SkinData);
  iImgHeight := GetImageHeight(Images, ImageIndex, iPPI);
  iImgWidth  := GetImageWidth (Images, ImageIndex, iPPI);

  CaptionHeight := TextHeight + FCaptionYOffset + 2;
  if ImageExists then
    CaptionHeight := max(CaptionHeight, iImgHeight);

  CaptionRect.Top := FCaptionMargin.Top;

  if FCaptionYOffset < 0 then
    inc(CaptionRect.Top, FCaptionYOffset);

  R.Left := 0;
  R.Right := Width;
  acDrawText(FCommonData.FCacheBmp.Canvas.Handle, Caption, R, DT_CALCRECT or DT_ALPHATEXT * Cardinal(TranspMode = 2));
  iTextWidth := WidthOf(R);
//  iTextWidth := acTextWidth(FCommonData.FCacheBmp.Canvas, Caption);
  iSpacing := SkinData.CommonSkinData.Spacing;
  if CaptionWidth = 0 then
    if ImageExists then
      iContentWidth := iTextWidth + iImgWidth + iSpacing
    else
      iContentWidth := iTextWidth
  else
    iContentWidth := CaptionWidth;

  if FCheckBoxVisible then begin
    iCheckWidth := CheckBoxWidth;
    iCheckHeight := CheckBoxHeight;
    inc(iContentWidth, iCheckWidth + iSpacing);
    CaptionHeight := max(CaptionHeight, iCheckHeight);
  end
  else begin
    iCheckWidth := 0;
    iCheckHeight := 0;
  end;

  if ModernStyle then begin
    inc(CaptionHeight, SkinData.CommonSkinData.Spacing);
    if CaptionRect.Top < 0 then begin
      inc(CaptionHeight, CaptionRect.Top);
      CaptionRect.Top := 0;
    end;
  end;

  CaptionRect.Bottom := CaptionRect.Top + CaptionHeight;

  if FCaptionLayout = clTopCenter then
    CaptionRect.Left := (Width - iContentWidth) div 2
  else
    if (FCaptionLayout = clTopLeft) and not UseRightToLeftAlignment or (FCaptionLayout = clTopRight) and UseRightToLeftAlignment then
      CaptionRect.Left := xOffset + FCaptionMargin.Left
    else
      CaptionRect.Left := Width - iContentWidth - FCaptionMargin.Right - xOffset;

  if CaptionRect.Left < 0 then
    CaptionRect.Left := 0;

  CaptionRect.Right := CaptionRect.Left + iContentWidth;

  if CaptionRect.Right >= Width then
    CaptionRect.Right := Width - 1;

  if (iContentWidth > 0) and (iContentWidth < 2 * BevelWidth) then begin
    CaptionRect.Left := CaptionRect.Left - BevelWidth;
    CaptionRect.Right := CaptionRect.Right + BevelWidth;
  end;

  if BiDiMode = bdLeftToRight then begin
    if CheckBoxVisible then begin
      CheckBoxRect.Left := CaptionRect.Left;
      CheckBoxRect.Right := CheckBoxRect.Left + iCheckWidth;
      if ImageExists then begin
        GlyphRect.Left := CheckBoxRect.Right + iSpacing;
        GlyphRect.Right := GlyphRect.Left + iImgWidth
      end
      else begin
        GlyphRect.Left := CheckBoxRect.Right;
        GlyphRect.Right := GlyphRect.Left;
      end;
      TextRect.Left := GlyphRect.Right + iSpacing;
    end
    else begin
      CheckBoxRect := Rect(CaptionRect.Left, 0, CaptionRect.Left, 0);
      GlyphRect.Left := CheckBoxRect.Right;
      if ImageExists then begin
        GlyphRect.Right := GlyphRect.Left + iImgWidth;
        TextRect.Left := GlyphRect.Right + iSpacing;
      end
      else begin
        GlyphRect.Right := GlyphRect.Left;
        TextRect.Left := GlyphRect.Right;
      end;
    end;
    TextRect.Right := TextRect.Left + iTextWidth;
  end
  else begin
    if CheckBoxVisible then begin
      CheckBoxRect.Right := CaptionRect.Right;
      CheckBoxRect.Left := CheckBoxRect.Right - iCheckWidth;
      if ImageExists then begin
        GlyphRect.Right := CheckBoxRect.Left - iSpacing;
        GlyphRect.Left := GlyphRect.Right - iImgWidth;
      end
      else begin
        GlyphRect.Right := CheckBoxRect.Left;
        GlyphRect.Left := GlyphRect.Right;
      end;
      TextRect.Right := GlyphRect.Left - iSpacing;
    end
    else begin
      CheckBoxRect := Rect(CaptionRect.Right, 0, CaptionRect.Right, 0);
      GlyphRect.Right := CheckBoxRect.Left;
      if ImageExists then
        GlyphRect.Left := GlyphRect.Right - iImgWidth
      else
        GlyphRect.Left := CheckBoxRect.Right;

      TextRect.Right := GlyphRect.Left;
    end;
    TextRect.Left := TextRect.Right - iTextWidth;
  end;

  if CheckBoxVisible then begin
    CheckBoxRect.Top := CaptionRect.Top + (CaptionHeight - iCheckHeight) div 2;
    CheckBoxRect.Bottom := CheckBoxRect.Top + iCheckHeight;
  end;
  if ImageExists then begin
    GlyphRect.Top := CaptionRect.Top + (CaptionHeight - iImgHeight) div 2;
    GlyphRect.Bottom := CaptionRect.Top + iCheckHeight;
  end
  else
    if not CheckBoxVisible then
      OffsetRect(TextRect, (iContentWidth - iTextWidth) div 2, 0);

  TextRect.Top := CaptionRect.Top + (CaptionHeight - TextHeight) div 2;
  TextRect.Bottom := TextRect.Top + TextHeight;

  TopRect := Rect(0, 0, Width, max(0, CaptionHeight div 2 + FCaptionMargin.Top));
end;


procedure TsGroupBox.Loaded;
begin
  inherited;
  FCommonData.Loaded;
end;


function TsGroupBox.ModernStyle: boolean;
begin
  Result := (SkinData.SkinManager <> nil) and SkinData.SkinManager.IsActive and ((SkinData.SkinManager.Options.GroupBoxStyle = gsCard) and (BoxStyle in [bsDefault, bsCard]) or (BoxStyle = bsCard));
end;


function TsGroupBox.MouseInCheckBox: boolean;
begin
  Result := not (csDesigning in ComponentState) and (PtInRect(CheckBoxRect, ScreenToClient(acMousePos)) or PtInRect(CaptionRect, ScreenToClient(acMousePos)));
end;


procedure TsGroupBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;


procedure TsGroupBox.Paint;
begin
  PaintToDC(Canvas.Handle)
end;


procedure TsGroupBox.PaintCaptionArea(cRect: TRect; const CI: TCacheInfo; AState: integer);
var
  R: TRect;
  C: TColor;

  function PaintCaptionBG: integer;
  var
    R: TRect;
    Bmp: TBitmap;
    State: integer;
    cOffset: TPoint;
    LocalCI: TCacheInfo;
  begin
    if ModernStyle then begin
      LocalCI := CI;
      cOffset := Point(Left, Top);
      if FCaptionSkin = '' then begin
        Result := FCommonData.SkinManager.SkinCommonInfo.Sections[ssBarTitle];
        State := 1;
      end
      else begin
        Result := FCommonData.SkinManager.GetSkinIndex(FCaptionSkin);
        State := 0;
      end;
      R := Rect(0, 0, Width, cRect.Bottom);
      Bmp := CreateBmp32(R);
      Bmp.Canvas.Font.Assign(Font);
      PaintItem(Result, LocalCI, True, State, MkRect(Bmp), Point(cOffset.X, cOffset.Y), Bmp, SkinData.CommonSkinData);
      BitBlt(FCommonData.FCacheBMP.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
      Bmp.Free;
//      TranspMode := GetTransMode(SkinData, Result);
      if NeedParentFont(SkinData, State) then
//      if NeedParentFont(SkinData.SkinManager, Result, State) then
        Result := GetFontIndex(Parent, Result, SkinData.SkinManager, State);
    end
    else
      if (Caption <> '') or ImageExists or CheckBoxVisible then begin
        LocalCI := CI;
        if FCaptionSkin = '' then begin
          Result := FCommonData.SkinManager.SkinCommonInfo.Sections[ssTransparent];
          cOffset := Point(Left, Top);
        end
        else begin
//          LocalCI := MakeCacheInfo(FCommonData.FCacheBmp); // Do not paint a line in the caption
          Result := FCommonData.SkinManager.GetSkinIndex(FCaptionSkin);
          cOffset := MkPoint;
        end;
        R.Left   := CaptionRect.Left   - CaptionMargin.Left;
        R.Top    := CaptionRect.Top    - CaptionMargin.Top;
        R.Right  := CaptionRect.Right  + CaptionMargin.Right;
        R.Bottom := CaptionRect.Bottom + CaptionMargin.Bottom + 1;

        Bmp := CreateBmp32(R);
        Bmp.Canvas.Font.Assign(Font);
        PaintItem(Result, LocalCI, True, integer(Focused or CheckHot), MkRect(Bmp), Point(cOffset.X + R.Left, cOffset.Y + R.Top), Bmp, SkinData.CommonSkinData);
        BitBlt(FCommonData.FCacheBMP.Canvas.Handle, R.Left, R.Top, Bmp.Width, Bmp.Height, Bmp.Canvas.Handle, 0, 0, SRCCOPY);
        Bmp.Free;
//        TranspMode := GetTransMode(SkinData, Result);
        if NeedParentFont(SkinData, integer(CheckHot)) then
//        if NeedParentFont(SkinData.SkinManager, Result, integer(CheckHot)) then
          Result := GetFontIndex(Parent, Result, SkinData.SkinManager, integer(CheckHot));
      end
      else
        Result := -1;
  end;

begin
  if ModernStyle then
    AState := 1;

  if SkinData.CustomColor then begin
    FillDC(FCommonData.FCacheBmp.Canvas.Handle, cRect, Color);
    TextIndex := SkinData.SkinIndex;
    TranspMode := TM_OPAQUE;
  end
  else begin
    TextIndex := PaintCaptionBG;
  end;

  if CheckBoxVisible then
    PaintCheckBoxSkin(FCommonData.FCacheBMP, CheckBoxRect, AState);

  if not SkinData.CustomFont then begin
    FCommonData.FCacheBMP.Canvas.Font.Assign(Font);
    if TextIndex >= 0 then begin
      C := SkinData.CommonSkinData.gd[TextIndex].Props[AState].Color;
      FCommonData.FCacheBMP.Canvas.Font.Color := SkinData.CommonSkinData.gd[TextIndex].Props[AState].FontColor.Color;
    end
    else
      C := 0;
  end
  else
    C := Font.Color;

  if ImageExists then
    if ((Images is TacImageList) or (Images is TsVirtualImageList)) and (SkinData.SkinManager <> nil) and SkinData.SkinManager.Effects.DiscoloredGlyphs then
      DrawAlphaImgList(Images, FCommonData.FCacheBMP, GlyphRect.Left, GlyphRect.Top, ImageIndex, 0, C, 0, 1, False, GetPPI(SkinData))
    else begin
      SetImagesPPI(Images, GetPPI(SkinData));
      if AState > 0 then begin
        SetImagesState(Images, AState);
        Images.Draw(FCommonData.FCacheBMP.Canvas, GlyphRect.Left, GlyphRect.Top, ImageIndex, True);
        SetImagesState(Images, 0);
      end
      else
        Images.Draw(FCommonData.FCacheBMP.Canvas, GlyphRect.Left, GlyphRect.Top, ImageIndex, True);
    end;

  if Caption <> '' then
    WriteText(TextRect);

  if not Enabled then begin
    if ModernStyle then
      R := Rect(0, 0, Width, cRect.Bottom)
    else
      R := cRect;

    if CI.Ready then begin
      cRect := R;
      OffsetRect(cRect, CI.X + Left, CI.Y + Top);
      BlendTransRectangle(FCommonData.FCacheBMP, R.Left, R.Top, CI.Bmp, cRect, DefBlendDisabled);
    end
    else
      FadeBmp(FCommonData.FCacheBMP, R, DefBlendDisabled, TsColor(CI.FillColor), 0, 0);
  end;
end;


procedure TsGroupBox.PaintCheckBoxSkin(Bmp: TBitmap; R: TRect; State: integer);
var
  GlyphNdx: integer;
begin
  GlyphNdx := CheckBoxIndex;
  if SkinData.SkinManager.IsValidImgIndex(GlyphNdx) then
    DrawSkinGlyph(Bmp, R.TopLeft, State, 1, FCommonData.CommonSkinData.ma[GlyphNdx], MakeCacheInfo(Bmp))
end;


procedure TsGroupBox.PaintCheckBoxStd(DC: hdc; R: TRect; State: integer);
const
  CheckStates: array [boolean] of Cardinal = (DFCS_ADJUSTRECT, DFCS_CHECKED);
{$IFDEF DELPHI7UP}
  ThemedChecks: array [boolean, 0..3] of TThemedButton = ((tbCheckBoxUncheckedNormal, tbCheckBoxUncheckedHot, tbCheckBoxUncheckedPressed, tbCheckBoxUncheckedDisabled),
                                                          (tbCheckBoxCheckedNormal,   tbCheckBoxCheckedHot,   tbCheckBoxCheckedPressed, tbCheckBoxCheckedDisabled));
{$ENDIF}
begin
{$IFDEF DELPHI7UP}
  if acThemesEnabled then
    acThemeServices.DrawElement(DC, acThemeServices.GetElementDetails(ThemedChecks[Checked, iff(Enabled, State, 3)]), R)
  else
{$ENDIF}
    DrawFrameControl(DC, R, DFC_BUTTON, CheckStates[Checked]);
end;


procedure TsGroupBox.PaintToDC(DC: hdc);
var
  R: TRect;
  b: boolean;
  i: integer;

{$IFDEF DELPHI7UP}
  procedure PaintThemedGroupBox;
  var
    SavedDC: hdc;
    cR, OuterRect: TRect;
    Box: TThemedButton;
    Details: TThemedElementDetails;
  begin
    with Canvas do begin
      OuterRect := ClientRect;
      OuterRect.Top := CaptionMargin.Top + CaptionHeight div 2;

      if not ParentColor then
        FillDC(Handle, MkRect(Self), Color); // Fill control BG

      SavedDC := SaveDC(Handle);
      cR := MkRect(CaptionRect.Left - CaptionMargin.Left,
                   CaptionRect.Top - CaptionMargin.Top,
                   CaptionRect.Right + CaptionMargin.Right,
                   CaptionRect.Bottom + CaptionMargin.Bottom);

      try
        if Enabled then
          Box := tbGroupBoxNormal
        else
          Box := tbGroupBoxDisabled;

        Details := acThemeServices.GetElementDetails(Box);
        acThemeServices.DrawElement(Handle, Details, OuterRect);
        SelectClipRgn(Handle, 0);
        IntersectClipRect(Handle, cR.Left, cR.Top, cR.Right, cR.Bottom);
        Details := acThemeServices.GetElementDetails(ttBody);
        acThemeServices.DrawParentBackground(Self.Handle, Handle, @Details, False);
        if not ParentColor then
          FillDC(Handle, cR, Color); // Fill caption area
      finally
        RestoreDC(Handle, SavedDC);
      end;

      if Text <> '' then begin
        SelectObject(DC, Font.Handle);
        SetTextColor(DC, ColorToRGB(Font.Color));
        SetBkMode(DC, TRANSPARENT);
        acDrawText(DC, Text, Self.TextRect, DrawTextBiDiModeFlags(DT_SINGLELINE or DT_CENTER));
      end;
    end;
  end;
{$ENDIF}

  procedure PaintGroupBox;
  var
    R: TRect;
    C: TColor;
    Flags: Cardinal;
  begin
    with Canvas do begin
      R := Rect(0, CaptionMargin.Top + CaptionHeight div 2 - 1, Width, Height);
      if Ctl3D then begin
        Inc(R.Left);
        Inc(R.Top);
        Brush.Color := clBtnHighlight;
        FrameRect(R);
        OffsetRect(R, -1, -1);
        Brush.Color := clBtnShadow;
      end
      else
        Brush.Color := clWindowFrame;

      FrameRect(R);
      if ParentColor then
        C := TacAccessControl(Parent).Color
      else
        C := Color;

      FillDC(Canvas.Handle, Rect(CaptionRect.Left - CaptionMargin.Left,
                                 CaptionRect.Top - CaptionMargin.Top,
                                 CaptionRect.Right + CaptionMargin.Right,
                                 CaptionRect.Bottom + CaptionMargin.Bottom), C);

      if Caption <> '' then begin
        Flags := DrawTextBiDiModeFlags(DT_SINGLELINE);
        Brush.Color := Color;
        acDrawText(Handle, PacChar(Caption), Self.TextRect, Flags);
      end;
    end;
  end;

begin
  if FCommonData.Skinned(True) then begin
    if not (csDestroying in ComponentState) and (Visible or (csDesigning in ComponentState)) then
      if not InAnimationProcess or (DC = SkinData.PrintDC) then
        if not InUpdating(FCommonData) then begin
          i := GetClipBox(DC, R);
          if i = ERROR {or IsRectEmpty(R) is not redrawn while resizing }then
            Exit;

          if i = NULLREGION then begin
            b := False;
            for I := 0 to ControlCount - 1 do
              if Controls[i].Align = alNone then begin
                b := True;
                Break;
              end;

            if b and RectVisible(DC, MkRect(Self)) then begin
              SkinData.FUpdating := True;
              Exit;
            end;
          end;
          // If transparent and form resizing processed
          b := FCommonData.HalfVisible or FCommonData.BGChanged;
          if SkinData.RepaintIfMoved and not (csPaintCopy in ControlState) then
            FCommonData.HalfVisible := (WidthOf(R, True) <> Width) or (HeightOf(R, True) <> Height)
          else
            FCommonData.HalfVisible := False;

          if b then
            PrepareCache;

          CopyWinControlCache(Self, FCommonData, MkRect, MkRect(Self), DC, False);
          sVCLUtils.PaintControls(DC, Self, b, MkPoint);
          SetParentUpdated(Self);
        end;
  end
  else begin
    Canvas.Font.Assign(Self.Font);
    InitPaintData;
{$IFDEF DELPHI7UP}
    if acThemesEnabled then
      PaintThemedGroupBox
    else
{$ENDIF}
      PaintGroupBox;

    if CheckBoxVisible then
      PaintCheckBoxStd(DC, CheckBoxRect, integer(CheckHot));

    if ImageExists then
      Images.Draw(Canvas, GlyphRect.Left, GlyphRect.Top, ImageIndex, True);
  end
end;


function TsGroupBox.PrepareCache: boolean;
var
  CI: TCacheInfo;
  BG: TacBGInfo;
begin
{$IFDEF D2006}
// Desc 6 if csRecreating in ControlState then begin
//    Result := False;
//    SkinData.FUpdating := True;
//  end
//  else
{$ENDIF}
  begin
    GetBGInfo(@BG, Parent);
    if BG.BgType = btNotReady then begin
      Result := False;
      SkinData.Updating := True;
    end
    else begin
      InitPaintData;
      if SkinData.CustomColor then begin
        FillDC(FCommonData.FCacheBmp.Canvas.Handle, MkRect(Self), Color);
        CI.FillColor := Color;
        CI.Ready := False;
      end
      else begin
        CI := BGInfoToCI(@BG);
        // Caption BG painting
        if CI.Ready then
          BitBlt(FCommonData.FCacheBmp.Canvas.Handle, TopRect.Left, TopRect.Top, Width, TopRect.Bottom, CI.Bmp.Canvas.Handle, Left + CI.X, Top + CI.Y, SRCCOPY)
        else
          if Parent <> nil then
            FillDC(FCommonData.FCacheBmp.Canvas.Handle, TopRect, CI.FillColor);
      end;

      PaintItem(FCommonData, CI, False, 0, Rect(0, TopRect.Bottom, Width, Height), Point(Left, Top + TopRect.Bottom), FCommonData.FCacheBMP, True);
      PaintCaptionArea(CaptionRect, CI, 0);
      SkinData.PaintOuterEffects(Self, MkPoint);
      FCommonData.BGChanged := False;
      Result := True;
    end;
  end;
end;


procedure TsGroupBox.RepaintCheckBox(State: integer; DoAnimation: boolean = False);
var
  DC: hdc;
  cRect: TRect;
  i: integer;
begin
  if SkinData.Skinned then begin
    cRect := CaptionRect;
//    cRect.Bottom := CaptionRect.Bottom + 1;
    if DoAnimation and SkinData.SkinManager.Effects.AllowAnimation and (State <> 2) and not ((SkinData.AnimTimer <> nil) and (SkinData.AnimTimer.ThreadType = TT_LAYEREDEFFECT)) then begin
      i := GetNewTimer(SkinData.AnimTimer, SkinData.FOwnerControl, State);
      if (SkinData.AnimTimer.State >= 0) and (State = SkinData.AnimTimer.State) then // Started already
        Exit;

      if SkinData.AnimTimer.BmpFrom <> nil then
        FreeAndNil(SkinData.AnimTimer.BmpFrom);

      SkinData.AnimTimer.BmpFrom := CreateBmp32(cRect);
      BitBlt(SkinData.AnimTimer.BmpFrom.Canvas.Handle, 0, 0, SkinData.AnimTimer.BmpFrom.Width, SkinData.AnimTimer.BmpFrom.Height, SkinData.FCacheBmp.Canvas.Handle, cRect.Left, cRect.Top, SRCCOPY);
      PaintCaptionArea(CaptionRect, GetParentCache(SkinData), State);
      SkinData.AnimTimer.InitData(SkinData, i, UpdateCheckBox_CB, State);
    end
    else begin
      if (SkinData.AnimTimer <> nil) and (SkinData.AnimTimer.ThreadType <> TT_LAYEREDEFFECT) then
        FreeAndNil(SkinData.AnimTimer);

      DC := GetDC(Handle);
      try
        PaintCaptionArea(cRect, GetParentCache(SkinData), State);
        BitBlt(DC, cRect.Left, cRect.Top, WidthOf(cRect), HeightOf(cRect), SkinData.FCacheBmp.Canvas.Handle, cRect.Left, cRect.Top, SRCCOPY);
      finally
        ReleaseDC(Handle, DC);
      end;
    end
  end
  else
    InvalidateRect(Handle, @CaptionRect, False);
end;


procedure TsGroupBox.SetBoolean(const Index: Integer; const Value: boolean);
begin
  case Index of
    0: if FCheckBoxVisible <> Value then begin
      FCheckBoxVisible := Value;
      CaptionChanged;
      FCommonData.Invalidate;
    end;

    1: if FChecked <> Value then begin
      FChecked := Value;
      if not (csLoading in ComponentState) then begin
        if not CheckPressed then
          FCommonData.Invalidate;

        CheckBoxChanged;
      end;
    end;

    2: if FCheckBoxRadio <> Value then begin
      FCheckBoxRadio := Value;
      CaptionChanged;
      FCommonData.Invalidate;
    end;
  end;
end;


procedure TsGroupBox.SetBoxStyle(const Value: TacBoxStyle);
begin
  if FBoxStyle <> Value then begin
    FBoxStyle := Value;
    SkinData.Invalidate;
  end;
end;


procedure TsGroupBox.SetCaptionLayout(const Value: TsCaptionLayout);
begin
  if FCaptionLayout <> Value then begin
    FCaptionLayout := Value;
    CaptionChanged;
    if Caption <> '' then
      SkinData.Invalidate;
  end;
end;


procedure TsGroupBox.SetCaptionSkin(const Value: TsSkinSection);
begin
  if FCaptionSkin <> Value then begin
    FCaptionSkin := Value;
    CaptionChanged;
    FCommonData.Invalidate
  end;
end;


procedure TsGroupBox.SetCaptionWidth(const Value: TacIntProperty);
begin
  if FCaptionWidth <> Value then begin
    FCaptionWidth := Value;
    CaptionChanged;
    FCommonData.Invalidate
  end;
end;


procedure TsGroupBox.SetImages(const Value: TCustomImageList);
begin
  if Images <> Value then begin
    if Images <> nil then
      Images.UnRegisterChanges(FImageChangeLink);

    FImages := Value;
    if Images <> nil then begin
      Images.RegisterChanges(FImageChangeLink);
      Images.FreeNotification(Self);
    end;
    CaptionChanged;
    FCommonData.Invalidate;
  end;
end;


procedure TsGroupBox.SetInteger(const Index, Value: integer);
begin
  case Index of
    0: if FCaptionYOffset <> Value then begin
      FCaptionYOffset := Value;
      CaptionChanged;
      SkinData.Invalidate;
    end;

    1: if FImageIndex <> Value then begin
      FImageIndex := Value;
      CaptionChanged;
      SkinData.Invalidate;
    end;
  end;
end;


function TsGroupBox.TextHeight: integer;
begin
  Result := Maxi(4, FCommonData.FCacheBmp.Canvas.TextHeight('W')) + 2;
end;


procedure TsGroupBox.UpdateAccessibilities;
begin
  case AutoChildrenStates of
    csAsChecked:    IterateControls(Self, Integer(FChecked), EnableDisable);
    csAsNotChecked: IterateControls(Self, Integer(not FChecked), EnableDisable);
  end;
end;


procedure TsGroupBox.WMFontChanged(var Message: TMessage);
begin
  inherited;
  FCommonData.FCacheBmp.Canvas.Font.Assign(Font);
  if Caption <> '' then
    FCommonData.Invalidate;
end;


procedure TsGroupBox.CMEnabledChanged(var Message: TMessage); 
begin
  inherited;
  FCommonData.Invalidate;
end;


procedure TsGroupBox.WndProc(var Message: TMessage);
var
  PS: TPaintStruct;
  DC: hdc;
begin
{$IFDEF LOGGED}
//  AddToLog(Message, Name);
{$ENDIF}
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_CTRLHANDLED: begin
          Message.Result := 1;
          Exit
        end;

        AC_SETSCALE:
          if SkinData.SkinManager <> nil then begin
            CaptionWidth := MulDiv(CaptionWidth, Message.LParam, SkinData.CurrentPPI);
            CommonMessage(Message, SkinData);
            Exit;
          end;

        AC_SETNEWSKIN: begin
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then
            CommonWndProc(Message, FCommonData);

          AlphaBroadCast(Self, Message);
          Exit;
        end;

        AC_GETBG:
          if CaptionSkin <> '' then
            with PacBGInfo(Message.LParam)^ do begin // If BG of groupbox used
              Bmp := SkinData.FCacheBmp;
              Offset := MkPoint;
              BgType := btCache;
              Exit;
            end
          else
            if CommonMessage(Message, FCommonData) then
			  Exit;

        AC_PREPARECACHE: begin
          if (SkinData <> nil) and SkinData.Skinned and not InUpdating(SkinData) and not PrepareCache then
            SkinData.FUpdating := True;

          Exit;
        end;

        AC_GETOUTRGN: begin
          PRect(Message.LParam)^ := MkRect(Width, Height);
          if FCaptionYOffset < 0 then
            PRect(Message.LParam)^.Top := 0
          else begin
            InitPaintData;
            PRect(Message.LParam)^.Top := CaptionMargin.Top + CaptionHeight div 2;
          end;
          OffsetRect(PRect(Message.LParam)^, Left, Top);
          Exit;
        end;

        AC_GETMOUSEAREA: begin
          PRect(Message.LParam)^ := GlyphScreenRect;
          Message.Result := 1;
          Exit;
        end;

        AC_MOUSELEAVE:
          if CheckBoxVisible then begin
            MouseInGlyph := False;
            if CheckHot then begin
              if CircleClickMode(Self, SkinData, [aeGlobalDef]{[aeClick]}) then
                MouseInGlyph := False;

              CheckHot := False;
              RepaintCheckBox(0);
            end;
          end;

        AC_REMOVESKIN: begin
          if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
            CommonWndProc(Message, FCommonData);
            AlphaBroadCast(Self, Message);
            Repaint;
          end
          else
            AlphaBroadCast(Self, Message);

          Exit;
        end;

        AC_REFRESH: begin
          if RefreshNeeded(SkinData, Message) then begin
            CommonWndProc(Message, FCommonData);
            AlphaBroadCast(Self, Message);
            Repaint;
          end
          else
            AlphaBroadCast(Self, Message);

          Exit;
        end;

        AC_GETDEFSECTION: begin
          Message.Result := 8 + 1;
          Exit;
        end;

        AC_GETDEFINDEX: begin
          if FCommonData.SkinManager <> nil then
            if ModernStyle then
              Message.Result := FCommonData.SkinManager.SkinCommonInfo.Sections[ssBarPanel] + 1
            else
              Message.Result := FCommonData.SkinManager.SkinCommonInfo.Sections[ssGroupBox] + 1;

          Exit;
        end;

        AC_SETGLASSMODE: begin
          CommonMessage(Message, FCommonData);
          Exit;
        end

        else
          if CommonMessage(Message, SkinData) then
            Exit;
      end;

    CM_MOUSELEAVE, WM_MOUSELEAVE: begin
      MouseInGlyph := False;
      if CheckBoxVisible then
        if CheckHot then begin
          CheckHot := False;
          RepaintCheckBox(0, True);
        end;
    end;

    CM_COLORCHANGED:
      if SkinData.CustomColor then
        SkinData.BGChanged := True;

    WM_MOUSEMOVE:
      if CheckBoxVisible then begin
        MouseInGlyph := PtInRect(GlyphScreenRect, acMousePos);
        if MouseInCheckBox then begin
          if not CheckHot then begin
            CheckHot := True;
            RepaintCheckBox(1, True);
          end;
        end
        else
          if CheckHot then begin
            CheckHot := False;
            RepaintCheckBox(0, True);
          end;
      end;

    WM_LBUTTONDBLCLK, WM_LBUTTONDOWN:
      if CheckBoxVisible then begin
        ControlState := ControlState + [csLButtonDown];
        if CircleClickMode(Self, SkinData, [aeGlobalDef]{[aeClick]}) then begin
          if not SkinIsChanging(SkinData) and not FadingForbidden and SkinData.SkinManager.Effects.AllowAnimation then begin // Animated
            ShowCircleIfNeeded(SkinData, MaxByte, True)
          end
          else // Without animation
            if SkinData.FMouseAbove then
              ShowCircleIfNeeded(SkinData)
            else
              if not SkinData.FMouseAbove then
                HideCircle(SkinData.CircleID);
        end;
        if MouseInCheckBox then begin
          CheckPressed := True;
          RepaintCheckBox(2);
        end;
      end;

    WM_LBUTTONUP:
      if CheckBoxVisible then begin
        if CheckPressed then begin
          if MouseInCheckBox then
            Checked := not Checked;

          CheckPressed := False;
          ControlState := ControlState - [csLButtonDown];
          if CircleClickMode(Self, SkinData, [aeGlobalDef]{[aeClick]}) then begin
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
            if EventEnabled(aeClick, [aeGlobalDef]){[aeClick])} then
              if PtInRect(GlyphScreenRect, acMousePos) then
                DoClickAnimation(SkinData);

          RepaintCheckBox(1);
        end;
      end;
  end;

  if not ControlIsReady(Self) or not FCommonData.Skinned then
    inherited
  else begin
    if Message.Msg = SM_ALPHACMD then
      case Message.WParamHi of
        AC_ENDPARENTUPDATE: begin
          if FCommonData.FUpdating then begin
            if Showing and not InUpdating(FCommonData, True) then begin
              RedrawWindow(Handle, nil, 0, RDWA_ALLNOW);
              SetParentUpdated(Self);
            end;
          end;
          Exit;
        end;

        AC_PREPARECACHE: begin
          if not PrepareCache then
            SkinData.FUpdating := True;

          Exit;
        end

        else begin
          CommonMessage(Message, FCommonData);
          Exit;
        end;
      end
    else
      case Message.Msg of
        WM_PAINT: begin
          BeginPaint(Handle, PS);
          if Message.WParam = 0 then
            DC := GetDC(Handle)
          else
            DC := hdc(Message.WParam);

          try
            PaintToDC(DC);
          finally
            if Message.WParam = 0 then
              ReleaseDC(Handle, DC);
          end;
          EndPaint(Handle, PS);
          Exit;
        end;

        WM_PRINT: begin
          SkinData.Updating := False;
          PaintToDC(TWMPaint(Message).DC);
          Exit;
        end;

        CM_TEXTCHANGED: begin
          SkinData.Invalidate;
          Exit;
        end;

        WM_ERASEBKGND: begin
          if not (csPaintCopy in ControlState) and (Message.WParam <> WParam(Message.LParam) {PerformEraseBackground, TntSpeedButtons}) then begin
            Message.Result := 1;
            if csDesigning in ComponentState then
              inherited; // Drawing in the BDS IDE
          end
          else
            if Message.WParam <> 0 then // From PaintTo
              if not FCommonData.BGChanged then
                BitBlt(TWMPaint(Message).DC, 0, 0, Width, Height, FCommonData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);

          Exit;
        end;

        CM_VISIBLECHANGED: begin
          FCommonData.Updating := False;
          FCommonData.BGChanged := True;
        end;

        WM_WINDOWPOSCHANGED:
          FCommonData.BGChanged := True;

        WM_KILLFOCUS, WM_SETFOCUS: begin
          inherited;
          Exit
        end;

        WM_MOVE:
          FCommonData.BGChanged := FCommonData.BGChanged or FCommonData.RepaintIfMoved;
      end;

    if not CommonWndProc(Message, FCommonData) then begin
      inherited;
      case Message.Msg of
        WM_SIZE:
          if csDesigning in ComponentState then
            SendMessage(Handle, WM_PAINT, 0, 0);
      end
    end;
  end;
end;


procedure TsGroupBox.WriteText(R: TRect);
var
  Flags: Cardinal;
begin
  Flags := DT_SINGLELINE or DT_VCENTER or iff((CaptionWidth = 0) or (CaptionLayout = clTopCenter), DT_CENTER, DT_END_ELLIPSIS) or DT_ALPHATEXT * integer(TranspMode = 2);
  if UseRightToLeftReading then
    Flags := Flags or DT_RTLREADING;

  FCommonData.FCacheBMP.Canvas.Brush.Style := bsClear;
  if not SkinData.CustomFont then
    acWriteTextEx(FCommonData.FCacheBMP.Canvas, PacChar(Caption), True, TextRect, Flags, TextIndex, CheckHot or ModernStyle, FCommonData.SkinManager)
  else
    acWriteText(FCommonData.FCacheBMP.Canvas, PacChar(Caption), True, TextRect, Flags);

  if TranspMode = 1 then
    FillAlphaRect(FCommonData.FCacheBmp, TextRect);
end;


{$IFNDEF ALITE}
type
  TsGroupButton = class(TsRadioButton)
  private
    FInClick,
    FInModifying: boolean;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
  protected
    function CanModify: boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor InternalCreate(RadioGroup: TsRadioGroup);
    destructor Destroy; override;
  end;


constructor TsGroupButton.InternalCreate(RadioGroup: TsRadioGroup);
begin
  inherited Create(RadioGroup);
  SkinData.SkinManager := RadioGroup.SkinData.SkinManager;
  AutoSize := False;
  ShowFocus := True;
  RadioGroup.FButtons.Add(Self);
  Visible := False;
  Enabled := RadioGroup.Enabled;
  ParentShowHint := False;
  OnClick := RadioGroup.ButtonClick;
  TabStop := False;
  SkinData.CustomFont := RadioGroup.SkinData.CustomFont;
  AnimatEvents := RadioGroup.AnimatEvents;
  Parent := RadioGroup;
  ControlStyle := ControlStyle + [csOpaque];
end;


destructor TsGroupButton.Destroy;
begin
  TsRadioGroup(Owner).FButtons.Remove(Self);
  inherited Destroy;
end;


procedure TsGroupButton.CNCommand(var Message: TWMCommand);
begin
  if not FInClick and not FInModifying and not (csLoading in ComponentState) then begin
    FInClick := True;
    try
      with TsRadioGroup(Parent) do
        if (Message.NotifyCode in [BN_CLICKED, BN_DOUBLECLICKED]) and CanModify(FButtons.IndexOf(Self)) then begin
          inherited;
          if Assigned(FOnChange) then
            FOnChange(Self.Parent);
        end;
    except
      Application.HandleException(Self);
    end;
    FInClick := False;
  end;
end;


procedure TsGroupButton.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  with TsRadioGroup(Parent) do begin
    KeyPress(Key);
    if ((Key = #8) or (Key = s_Space)) and not CanModify(FButtons.IndexOf(Self)) then
      Key := #0;
  end;
end;


procedure TsGroupButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  TsRadioGroup(Parent).KeyDown(Key, Shift);
end;


function TsGroupButton.CanModify: boolean;
begin
  FInModifying := True;
  Result := TsRadioGroup(Parent).CanModify(TsRadioGroup(Parent).FButtons.IndexOf(Self));
  FInModifying := False;
end;


type
  TAccessCommonData = class(TsCommonData);


procedure TsRadioGroup.ArrangeButtons;
var
  ALeft: Integer;
  FontSize: TSize;
  DeferHandle: THandle;
  ButtonsPerCol, ButtonWidth, ButtonHeight, TopMargin, I: Integer;
begin
  if (FButtons.Count <> 0) and not FReading and not fArranging and not (csLoading in ComponentState) then begin
    fArranging := True;
    acGetTextExtent(0, s_Yy, FontSize, Font.Handle);

    ButtonsPerCol := (FButtons.Count + FColumns - 1) div FColumns;
    ButtonWidth := (Width - FContentHOffset - 10) div FColumns;
    I := Height - 5 - FContentVOffset;
    if (Caption <> '') or (SkinData.SkinIndex < 0) or (SkinData.CommonSkinData.gd[SkinData.SkinIndex].BorderIndex >= 0) then
      dec(I, FontSize.cy);

    ButtonHeight := I div ButtonsPerCol;
    TopMargin := FContentVOffset + 1 + (I mod ButtonsPerCol) div 2;
    if (Caption <> '') or (SkinData.SkinIndex < 0) or (SkinData.CommonSkinData.gd[SkinData.SkinIndex].BorderIndex >= 0) then
      inc(TopMargin, FontSize.cy);

    DeferHandle := BeginDeferWindowPos(FButtons.Count);
    try
      for I := 0 to FButtons.Count - 1 do
        with TsGroupButton(FButtons[I]) do begin
          TAccessCommonData(SkinData).FSkinManager := SkinData.SkinManager;
          BiDiMode := Self.BiDiMode;
          ShowFocus := Self.ShowFocus;
          ALeft := (I div ButtonsPerCol) * ButtonWidth + SkinData.CommonSkinData.Spacing * 2 + FContentHOffset;
          if UseRightToLeftAlignment then
            ALeft := Self.Width - ALeft - ButtonWidth;

          DeferHandle := DeferWindowPos(DeferHandle, Handle, 0, ALeft, (I mod ButtonsPerCol) * ButtonHeight + TopMargin,
                                        ButtonWidth, ButtonHeight, SWP_NOZORDER or SWP_NOACTIVATE);

          SkinData.CommonSkinData := nil; // Reset SkinData after recreating
          Visible := True;
        end;
    finally
      EndDeferWindowPos(DeferHandle);
      fArranging := False;
    end;
  end;
end;


procedure TsRadioGroup.ButtonClick(Sender: TObject);
begin
  if not FUpdating then begin
    FItemIndex := FButtons.IndexOf(Sender);
    Changed;
    Click;
  end;
end;


function TsRadioGroup.CanModify(NewIndex: integer): boolean;
begin
  Result := True;
  if Assigned(FOnChanging) then
    FOnChanging(Self, NewIndex, Result);
end;


procedure TsRadioGroup.CaptionChanged;
var
  M: TMessage;
begin
  inherited;
  M := MakeMessage(SM_ALPHACMD, AC_SETBGCHANGED_HI + 1, 0, 0);
  BroadCast(M);
end;

procedure TsRadioGroup.CheckBoxChanged;
begin
  inherited;
end;


procedure TsRadioGroup.CMEnabledChanged(var Message: TMessage);
var
  I: Integer;
begin
  inherited;
  for I := 0 to FButtons.Count - 1 do begin
    TsGroupButton(FButtons[I]).Enabled := Enabled;
    TsGroupButton(FButtons[I]).SkinData.BGChanged := True;
  end;
end;


procedure TsRadioGroup.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ArrangeButtons;
end;


constructor TsRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csSetCaption, csDoubleClicks{$IFDEF DELPHI7UP}, csParentBackground{$ENDIF}];
  FButtons := TList.Create;
{$IFDEF TNTUNICODE}
  FItems := TTntStringList.Create;
  TTntStringList(FItems).OnChange := ItemsChange;
{$ELSE}
  FItems := TStringList.Create;
  TStringList(FItems).OnChange := ItemsChange;
{$ENDIF}
  FItemIndex := -1;
  FColumns := 1;
  FArranging := False;
  FContentHOffset := 0;
  FContentVOffset := 0;
  FDisableItemsIfUnchecked := False;
  FAnimatEvents := [aeGlobalDef];
  FShowFocus := True;
end;


procedure TsRadioGroup.CreateWnd;
{$IFDEF D2006}
var
  i: integer;
{$ENDIF}
begin
  inherited;
  UpdateButtons;
{$IFDEF D2006}
  if csRecreating in ControlState then
  begin
    i := FItemIndex;
    FItemIndex := -1;
    ItemIndex := i; // Initialize selected radiobutton
  end;
{$ENDIF}
end;


destructor TsRadioGroup.Destroy;
begin
  SetButtonCount(0);
{$IFDEF TNTUNICODE}
  TTntStringList(FItems).OnChange := Nil;
{$ELSE}
  TStringList(FItems).OnChange := Nil;
{$ENDIF}
  FreeAndNil(FItems);
  FButtons.Free;
  inherited Destroy;
end;


procedure TsRadioGroup.FlipChildren(AllLevels: Boolean);
begin
//
end;


function TsRadioGroup.GetButtons(Index: Integer): TsRadioButton;
begin
  Result := TsRadioButton(FButtons[Index]);
end;


procedure TsRadioGroup.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
//
end;


procedure TsRadioGroup.ItemsChange(Sender: TObject);
var
  i: integer;
begin
  if not FReading and not(csLoading in ComponentState) then begin
    if FItemIndex >= FItems.Count then
      FItemIndex := FItems.Count - 1;

    UpdateButtons;
  end;
  for i := 0 to FButtons.Count - 1 do
    TsRadioButton(FButtons[i]).Loaded;
end;


type
  TAccessRadioButton = class(TsRadioButton);

procedure TsRadioGroup.Loaded;
begin
  inherited Loaded;
  UpdateButtons;
  if IsValidIndex(ItemIndex, FButtons.Count) then
    with TAccessRadioButton(FButtons[ItemIndex]) do begin
      ClicksDisabled := True;
      Checked := True;
      ClicksDisabled := False;
    end;
end;


procedure TsRadioGroup.ReadState(Reader: TReader);
begin
  FReading := True;
  inherited ReadState(Reader);
  FReading := False;
  UpdateButtons;
end;


procedure TsRadioGroup.SetButtonCount(Value: Integer);
begin
  while FButtons.Count < Value do
    TsGroupButton.InternalCreate(Self);

  while FButtons.Count > Value do
    TsGroupButton(FButtons.Last).Free;
end;


procedure TsRadioGroup.SetDisableItemsIfUnchecked(const Value: boolean);
begin
  if FDisableItemsIfUnchecked <> Value then begin
    FDisableItemsIfUnchecked := Value;
    IterateControls(Self, integer(Checked or not Value), EnableDisable);
  end;
end;


procedure TsRadioGroup.SetInteger(const Index, Value: integer);
var
  i: integer;
begin
  case Index of
    10: begin
      if Value < 1 then
        i := 1
      else
        if Value > 16 then
          i := 16
        else
          i := Value;

      if FColumns <> i then begin
        FColumns := i;
        if ([csLoading] * ComponentState = []) then begin
          ArrangeButtons;
          Invalidate;
        end;
      end;
    end;

    11:
      if FReading then
        FItemIndex := Value
      else begin
        HandleNeeded;
        if Value < -1 then
          i := -1
        else
          if Value >= FButtons.Count then
            i := FButtons.Count - 1
          else
            i := Value;

        if FItemIndex <> i then
          if i >= 0 then begin
            TsGroupButton(FButtons[i]).ClicksDisabled := True; // Do not allow OnClick event here
            TsGroupButton(FButtons[i]).Checked := False;
            FItemIndex := i;
            TsGroupButton(FButtons[i]).Checked := True;
            TsGroupButton(FButtons[i]).ClicksDisabled := False;
          end
          else begin
            if FItemIndex >= 0 then
              TsGroupButton(FButtons[FItemIndex]).Checked := False;

            FItemIndex := i;
          end;

        if Assigned(FOnChange) then
          FOnChange(Self);
      end;

    12: begin
      if FContentVOffset <> Value then begin
        FContentVOffset := Value;
        if ([csLoading] * ComponentState = []) then begin
          CaptionChanged;
          ArrangeButtons;
          Invalidate;
        end;
      end;
    end;

    13: begin
      if FContentHOffset <> Value then begin
        FContentHOffset := Value;
        if ([csLoading] * ComponentState = []) then begin
          CaptionChanged;
          ArrangeButtons;
          Invalidate;
        end;
      end;
    end

    else
      inherited;
  end;
end;


procedure TsRadioGroup.SetItems(Value: {$IFDEF TNTUNICODE}TTntStrings{$ELSE}TStrings{$ENDIF});
begin
  FItems.Assign(Value);
end;


procedure TsRadioGroup.UpdateButtons;
var
  I: Integer;
begin
  if not FReading {and not (csLoading in ComponentState)} then begin
    SetButtonCount(FItems.Count);
    for I := 0 to FButtons.Count - 1 do begin
      TsGroupButton(FButtons[I]).Caption := FItems[I];
      TsGroupButton(FButtons[I]).Cursor := Cursor;
    end;
    ArrangeButtons;
    if not (csLoading in ComponentState) then
      Invalidate;
  end;
end;


procedure TsRadioGroup.WMSize(var Message: TWMSize);
begin
  inherited;
  ArrangeButtons;
end;


procedure TsRadioGroup.WndProc(var Message: TMessage);
var
  M: TMessage;
begin
{$IFDEF LOGGED}
  AddToLog(Message, Name);
{$ENDIF}
  case Message.Msg of
    SM_ALPHACMD:
      case Message.WParamHi of
        AC_REFRESH:
          if RefreshNeeded(SkinData, Message) then
            ArrangeButtons;
      end;

    CM_COLORCHANGED: begin
      M := MakeMessage(SM_ALPHACMD, MakeWParam(0, AC_SETBGCHANGED), 0, 0);
      AlphaBroadCast(Self, M);
    end;
  end;

  inherited;
end;
{$ENDIF} // ALITE

end.
