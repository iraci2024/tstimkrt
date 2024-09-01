unit sCustomComboEdit;
{$I sDefs.inc}
// {$DEFINE LOGGED}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls, Mask, buttons, menus, ExtCtrls,
{$IFDEF TNTUNICODE} TntControls, TntStdCtrls, {$ENDIF}
{$IFNDEF DELPHI5} Types, {$ENDIF}
{$IFDEF DELPHI6UP} Variants, {$ENDIF}
  acntUtils, sConst, sGraphUtils, sCommonData, sDefaults, sMaskEdit,
  sGlyphUtils, acntTypes;


type
{$IFNDEF NOTFORHELP}
  TCloseUpEvent = procedure(Sender: TObject; Accept: boolean) of object;


  TacHintTimer = class(TTimer)
  protected
    FOwner: TComponent;
  public
    procedure TimerEvent(Sender: TObject);
    constructor Create(AOwner: TComponent); override;
  end;
{$ENDIF} // NOTFORHELP


  TsCustomComboEdit = class(TacCustomMaskEdit)
{$IFNDEF NOTFORHELP}
  private
    FBtnState, FPopupWidth, FPopupHeight: integer;

    FReadOnly, FDirectInput, FShowButton: boolean;
{$IFDEF TNTUNICODE}
    HintWnd: TTntCustomHintWindow;
{$ELSE}
    HintWnd: THintWindow;
{$ENDIF}
    FBtnRect: TRect;
    FClickKey: TShortCut;
{$IFNDEF D2009}
    FAlignment: TAlignment;
{$ENDIF}
    HintTimer: TacHintTimer;
    FGlyphMode: TsGlyphMode;
    FPopupWindowAlign: TPopupWindowAlign;
    FButtonWidth: integer;
    FButtonMode: boolean;
    FShowFocus: boolean;
    FButtonSkinSection: string;
    function GetDroppedDown: boolean;
    procedure SetDirectInput(Value: boolean);
    procedure SetShowButton(const Value: boolean);
    procedure SetBtnState(const Value: integer);
{$IFNDEF D2009}
    procedure SetAlignment(Value: TAlignment);
{$ENDIF}
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure CMFocuseChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CMWantSpecialKey(var Message: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure WMCut(var Message: TWMCut); message WM_CUT;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonUp(var Message: TWMLButtonUp); message WM_LBUTTONUP;
    procedure WMLDblClick(var Message: TWMMouse); message WM_LBUTTONDBLCLK;
    procedure WMMouseMove(var Message: TWMMouse); message WM_MOUSEMOVE;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;

    function BtnOffset: integer;
    function GlyphEnabled: boolean;
    function GetBtnState: integer;
    procedure CacheToDC(R: TRect);
    procedure TryShowBtnHint;
    procedure HideHint;
    procedure SetButtonWidth(const Value: integer);
    procedure SetButtonMode(const Value: boolean);
    procedure SetButtonSkinSection(const Value: string);
    procedure SetArrowAngle(const Value: integer);
  protected
    FSkipDrop, FDroppedDown: boolean;
    FOnButtonClick: TNotifyEvent;
    FArrowAngle: integer;
    procedure UpdateCaret;
    procedure PaintText; override;
    function TextRect: TRect; override;
    function MouseInRect(R: TRect): boolean;
    function MouseInBtn: boolean;
    function GlyphSpace(AddSpacing: boolean): integer;
    function CanShowButton: boolean; virtual;

    procedure TextPaintPrevent(Locked: boolean); override;
    procedure PaintBorder(DC: hdc); override;
    procedure PaintNCArea(DC: hdc); virtual;
    procedure PaintBtn; override;
    procedure OurPaintHandler(DC: hdc); override;

    procedure SetReadOnly(Value: boolean); virtual;
    function GetReadOnly: boolean; virtual;
    procedure KeyPress(var Key: Char); override;
    function BtnRect(AddSpacing: boolean): TRect;
    procedure PaintBtnStd; virtual;

    function GlyphWidth: integer;
    function GlyphHeight: integer;

    procedure UpdateLight(Sender: TObject; Data: PacAnimEventData);
    procedure CloseForm;
    procedure PopupWindowShow; virtual;
    procedure PopupWindowClose; virtual; // Procedure called after closing of popupform
    procedure CreateParams(var Params: TCreateParams); override;
    function AllowBtnStyle: boolean; override;
    function IsFullPaint: boolean; virtual;
    procedure AnimateCtrl(AState: integer);

    // function CanDoChange: boolean; virtual;
    procedure DoMouseAbove;
    // procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure ButtonClick; dynamic;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
{$IFNDEF D2009}
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
{$ENDIF}
    property PopupAlign: TPopupWindowAlign read FPopupWindowAlign write FPopupWindowAlign default pwaRight;
    property BtnState: integer read GetBtnState write SetBtnState default 0;
    property ArrowAngle: integer read FArrowAngle write SetArrowAngle;
  public
    FDefBmpID: integer;
    FPopupWindow: TWinControl;
    function CurrentState: integer; override;
    constructor Create(AOwner: TComponent); override;
    function ComboBtn: boolean;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    procedure Loaded; override;

    procedure WndProc(var Message: TMessage); override;
    procedure DoClick;
    procedure SelectAll; virtual;
    property DroppedDown: boolean read GetDroppedDown;
    property PopupWidth: integer read FPopupWidth write FPopupWidth default 230;
    property PopupHeight: integer read FPopupHeight write FPopupHeight default 166;
    property ButtonMode: boolean read FButtonMode write SetButtonMode default False;
    property DirectInput: boolean read FDirectInput write SetDirectInput default True;

    property OnValidateError;
{$ENDIF} // NOTFORHELP
    property Text;
    property readonly: boolean read GetReadOnly write SetReadOnly default False;
  published
{$IFNDEF NOTFORHELP}
    property Align;
    property Anchors;
    property AutoSelect;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnContextPopup;
{$ENDIF} // NOTFORHELP
    property ButtonSkinSection: string read FButtonSkinSection write SetButtonSkinSection;

    property OnButtonClick: TNotifyEvent read FOnButtonClick write FOnButtonClick;
    property ClickKey: TShortCut read FClickKey write FClickKey default scAlt + vk_Down;
    property GlyphMode: TsGlyphMode read FGlyphMode write FGlyphMode;

    property ButtonWidth: integer read FButtonWidth write SetButtonWidth default 0;
    property ShowButton: boolean read FShowButton write SetShowButton default True;
    property ShowFocus: boolean read FShowFocus write FShowFocus default True;
  end;

{$IFNDEF NOTFORHELP}


function PopupAnimProc(var Data: TacAnimData): boolean;
{$ENDIF} // NOTFORHELP

implementation

uses
  math,
{$IFDEF LOGGED}sDebugMsgs, {$ENDIF}
{$IFDEF DELPHI7UP} Themes, {$ENDIF}
  sToolEdit, sVCLUtils, sMessages, sAlphaGraph, sThirdParty, acSBUtils, acAnimation,
  acPopupController, sStyleSimply, sSkinManager, acAlphaImageList, sEdit;


constructor TsCustomComboEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SkinData.COC := COC_TsComboEdit;
{$IFNDEF D2009}
  FAlignment := taLeftJustify;
{$ENDIF}
  FDirectInput := True;
  FClickKey := scAlt + vk_Down;
  FPopupWindowAlign := pwaRight;
  FPopupWindow := nil;
  FShowButton := True;
  FShowFocus := True;
  FArrowAngle := 0;
  HintWnd := nil;

  FDefBmpID := iBTN_ELLIPSIS;
  FDroppedDown := False;
  FButtonWidth := 0;
  FBtnRect.Left := -1;
  FBtnState := 0;
  SkinData.OnUpdateLight := UpdateLight;
  FGlyphMode := TsGlyphMode.Create(Self, SkinData);
  Height := 21;
  FPopupWidth := 230;
  FPopupHeight := 166;
end;


destructor TsCustomComboEdit.Destroy;
begin
  OnKeyDown := nil;
  FreeAndNil(FGlyphMode);
  inherited Destroy;
end;


function PopupAnimProc(var Data: TacAnimData): boolean;
begin
  Result := True;
  if (Data.Owner is TsCustomComboEdit) and TsCustomComboEdit(Data.Owner).SkinData.SkinManager.Options.RotateArrows then begin
    if Data.Iteration = 0 then
      Data.Iterations := Data.Iterations * 2; // More time needed

    TsCustomComboEdit(Data.Owner).ArrowAngle := 180 * Data.Iteration div Data.Iterations;
  end;
end;


procedure TsCustomComboEdit.PopupWindowShow;
var
  P: TPoint;
  Y: integer;
  Flags: Cardinal;
  Form: TCustomForm;
  ActAlign: TPopupWindowAlign;
begin
  if (FPopupWindow <> nil) and not(readonly or DroppedDown) then begin
    FPopupWindow.Visible := False;
    if (SkinData.SkinManager <> nil) and (SkinData.SkinManager.Options.ScaleMode = smVCL) then
      if IsWin10Up then begin
        FPopupWindow.Width := FPopupWidth;
        FPopupWindow.Height := FPopupHeight;
      end
      else begin
        FPopupWindow.Width := ScaleInt(FPopupWidth, SkinData);
        FPopupWindow.Height := ScaleInt(FPopupHeight, SkinData);
      end
    else begin
      FPopupWindow.Width := FPopupWidth;
      FPopupWindow.Height := FPopupHeight;
    end;
    P := Parent.ClientToScreen(Point(Left, Top));
    Y := P.Y + Height;

    if Y + FPopupWindow.Height > Screen.DesktopHeight then
      Y := P.Y - FPopupWindow.Height;

    ActAlign := FPopupWindowAlign;
    if BiDiMode = bdRightToLeft then begin
      case FPopupWindowAlign of
        pwaRight: ActAlign := pwaLeft;
        pwaLeft:  ActAlign := pwaRight;
      end;
      ReflectControls(FPopupWindow, True);
    end;

    case ActAlign of
      pwaRight: begin
          Dec(P.X, FPopupWindow.Width - Width);
          if P.X < Screen.DesktopLeft then
            Inc(P.X, FPopupWindow.Width - Width);
        end;

      pwaLeft:
        if P.X + FPopupWindow.Width > Screen.DesktopWidth then
          Dec(P.X, FPopupWindow.Width - Width);
    end;
    if P.X < Screen.DesktopLeft then
      P.X := Screen.DesktopLeft
    else
      if P.X + FPopupWindow.Width > Screen.DesktopWidth then
        P.X := Screen.DesktopWidth - FPopupWindow.Width;

    Form := GetParentForm(Self);
    if CanFocus then begin
      SetFocus;
      ValidateRect(Handle, nil);
    end;
    if Form <> nil then begin
      if (FPopupWindow is TForm) and (TForm(Form).FormStyle = fsStayOnTop) then
        TForm(FPopupWindow).FormStyle := fsStayOnTop;

      Flags := SWPA_ZORDER;
      SetWindowPos(FPopupWindow.Handle, TopWndAfter, P.X, Y, FPopupWindow.Width, FPopupWindow.Height, Flags);
    end;
    if FPopupWindow is TForm then
      ShowPopupForm(TForm(FPopupWindow), Self, -1, -1, True, PopupAnimProc)
    else
      FPopupWindow.Visible := True;
  end;
end;


procedure TsCustomComboEdit.CacheToDC(R: TRect);
var
  DC: hdc;
  bWidth: integer;
begin
  if SkinData.FCacheBmp <> nil then begin
    DC := GetWindowDC(Handle);
    try
      if not SkinData.Skinned then begin
        bWidth := BordWidth;
        ExcludeClipRect(DC, 0, 0, Width, bWidth);
        ExcludeClipRect(DC, 0, bWidth, bWidth, Height);
        ExcludeClipRect(DC, bWidth, Height - bWidth, Width, Height);
        ExcludeClipRect(DC, Width - bWidth, bWidth, Width, Height - bWidth);
      end;
      BitBlt(DC, R.Left, R.Top, WidthOf(R), HeightOf(R), SkinData.FCacheBmp.Canvas.Handle, R.Left, R.Top, SRCCOPY);
    finally
      ReleaseDC(DC, Handle);
    end;
  end;
end;


function TsCustomComboEdit.CanShowButton: boolean;
begin
  Result := FShowButton;
end;


procedure TsCustomComboEdit.CloseForm;
begin
  if Assigned(FPopupWindow) and TForm(FPopupWindow).Visible then
    TForm(FPopupWindow).Close;
end;


procedure TsCustomComboEdit.KeyDown(var Key: Word; Shift: TShiftState);
var
  M: tagMsg;
  sc: TShortCut;
begin
  inherited KeyDown(Key, Shift);
  sc := ShortCut(Key, Shift);
  if (sc = FClickKey) and (GlyphWidth > 0) then begin
    if GlyphEnabled then
      ButtonClick;

    Key := 0;
  end
  else
    if sc = scCtrl + ord('A') then begin
      SelectAll;
      Key := 0;
      PeekMessage(M, Handle, WM_CHAR, WM_CHAR, PM_REMOVE);
    end;
end;


procedure TsCustomComboEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer);
begin
  if not RestrictDrawing then
    SkinData.BGChanged := True;

  if DroppedDown then
    CloseForm;

  inherited MouseDown(Button, Shift, X, Y);
end;


function TsCustomComboEdit.MouseInBtn: boolean;
var
  R: TRect;
  P: TPoint;
begin
  if ButtonMode then
    Result := acMouseInControl(Self)
  else
    if CanShowButton then begin
      R := BtnRect(True);
      P := acMouseInWnd(Handle);
      if IsRightToLeft then
        Result := MouseInRect(Rect(0, 0, R.Right, Height))
      else
        Result := MouseInRect(Rect(R.Left, 0, Width, Height));
    end
    else
      Result := False;
end;


function TsCustomComboEdit.MouseInRect(R: TRect): boolean;
begin
  Result := PtInRect(R, acMouseInWnd(Handle));
end;


function TsCustomComboEdit.GetBtnState: integer;
begin
  if csDesigning in ComponentState then
    Result := 0
  else
    if ComboBtn then
      if DroppedDown then
        Result := ACPRESSED
      else
        if AllowBtnStyle then begin
          if SkinData.FMouseAbove then
            Result := 1
          else
            Result := inherited CurrentState
        end
        else
          Result := FBtnState
    else
      Result := FBtnState;
end;


function TsCustomComboEdit.GetDroppedDown: boolean;
begin
  Result := FDroppedDown or (FPopupWindow <> nil) and FPopupWindow.Visible;
end;


procedure TsCustomComboEdit.CMCancelMode(var Message: TCMCancelMode);
begin
  with message do
    if (Sender <> Self) and (Sender <> FPopupWindow) and (FPopupWindow <> nil) and IsWindowVisible(FPopupWindow.Handle) and not FPopupWindow.ContainsControl(Sender) then
      CloseForm;
end;


procedure TsCustomComboEdit.DoClick;
begin
  if GlyphEnabled and not ComboBtn then
    ButtonClick;
end;


procedure TsCustomComboEdit.DoMouseAbove;
begin
  if not SkinData.FMouseAbove then begin
    FreeAndNil(SkinData.LightMask);
    SkinData.FMouseAbove := True;
    if SkinData.Skinned then
      SkinData.SkinManager.ActiveControl := Handle;

    if AllowBtnStyle then begin
      FBtnState := ACHOT;
      AnimateCtrl(ACHOT);
    end
    else begin
      FBtnState := integer(MouseInBtn);
      AnimateCtrl(ACHOT + FBtnState * 2);
    end;
  end;
end;


function TsCustomComboEdit.AllowBtnStyle: boolean;
begin
  Result := ButtonMode;
end;


procedure TsCustomComboEdit.AnimateCtrl(AState: integer);
var
  R: TRect;
begin
  SkinData.BGChanged := False;
  SkinData.FMouseAbove := AState <> 0;
  if SkinData.Skinned then begin
    if (AState and ACHOT <> 0) and AllowBtnStyle then
      with SkinData.SkinManager, SkinCommonInfo do
        ShowGlowingIfNeeded(SkinData, AState > ACHOT, Handle, MaxByte * integer(not Effects.AllowAnimation), False, iff(Sections[ssComboNoEdit] >= 0, Sections[ssComboNoEdit], Sections[ssButton]));

    if (AState = ACHOT) and not AllowBtnStyle then begin // Refresh of background sometimes required
      R := Rect(2, 2, BtnRect(False).Left - 2, Height - 2);
      RedrawWindow(Handle, @R, 0, RDW_INVALIDATE or RDW_ERASE or RDW_UPDATENOW or RDW_NOFRAME);
    end;
    DoChangePaint(SkinData, AState, UpdateComboAdv_CB, SkinData.SkinManager.Effects.AllowAnimation, AState in [2, 4], not AllowBtnStyle and (SkinData.GlowID < 0));
  end
  else
    if Focused then
      RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_ERASE or RDW_UPDATENOW or RDW_FRAME)
    else
      RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_ERASE or RDW_UPDATENOW or RDW_NOFRAME);
end;


function TsCustomComboEdit.BtnOffset: integer;
begin
  if not ac_OldGlyphsMode or ComboBtn then
    if SkinData.Skinned then
      Result := SkinData.CommonSkinData.ComboBoxMargin
    else
      if (BorderStyle = bsSingle) and (Skindata.Skinned) then
        Result := 1 // BordWidth
      else
        Result := 0
  else
    Result := BordWidth;
end;


function TsCustomComboEdit.BtnRect(AddSpacing: boolean): TRect;
var
  bWidth, w: integer;
begin
  w := GlyphSpace(AddSpacing);
  bWidth := BtnOffset;

  if IsRightToLeft then
    FBtnRect.Left := bWidth
  else
    if SkinData.Skinned then
      FBtnRect.Left := Width - bWidth - w - FAddedPadding.Right
    else
      FBtnRect.Left := Width - bWidth - w;

  FBtnRect.Right := FBtnRect.Left + w;
  FBtnRect.Top := bWidth;
  FBtnRect.Bottom := Height - bWidth;
  Result := FBtnRect;
end;


procedure TsCustomComboEdit.PaintBtnStd;
var
  DC: hdc;
  R: TRect;
  bWidth: integer;
begin
  if not (csDestroying in ComponentState) then begin
    bWidth := EditBorderWidth({$IFDEF TNTUNICODE}TTntEdit{$ELSE}TEdit{$ENDIF}(Self));
    DC := GetWindowDC(Handle);
    try
      PrepareCache;
      if not SkinData.Skinned then begin
        ExcludeClipRect(DC, 0, 0, Width, bWidth);
        ExcludeClipRect(DC, 0, bWidth, bWidth, Height);
        ExcludeClipRect(DC, bWidth, Height - bWidth, Width, Height);
        ExcludeClipRect(DC, Width - bWidth, bWidth, Width, Height - bWidth);
      end;
      R := BtnRect(True);
      BitBlt(DC, R.Left - bWidth, R.Top, WidthOf(R) + 2 * bWidth, HeightOf(R), SkinData.FCacheBmp.Canvas.Handle, R.Left - bWidth, R.Top, SRCCOPY);
    finally
      ReleaseDC(Handle, DC);
    end;
  end;
end;


procedure TsCustomComboEdit.PaintNCArea(DC: hdc);
var
  R: TRect;
begin
  R := BodyRect;
  BitBltBorder(DC, 0, 0, Width, R.Bottom, SkinData.FCacheBmp.Canvas.Handle, 0, 0, BordWidth + FAddedPadding.Left, BordWidth + FAddedPadding.Top, BordWidth + FAddedPadding.Right,
    BordWidth + FAddedPadding.Bottom);

  if CanShowButton then
    with SkinData.CommonSkinData do
      if IsRightToLeft then
        BitBlt(DC, BtnOffset, 0, GlyphSpace(True), Height, SkinData.FCacheBmp.Canvas.Handle, BtnOffset, 0, SRCCOPY)
      else
        BitBlt(DC, BtnRect(True).Left - Spacing div 2, 0, GlyphSpace(True) + Spacing div 2, Height, SkinData.FCacheBmp.Canvas.Handle, BtnRect(True).Left - Spacing div 2, 0, SRCCOPY);

  if AddedGlyphVisible then
    with SkinData.CommonSkinData do
      if IsRightToLeft then
        BitBlt(DC, BordWidth, 0, AddedGlyphSpace, Height, SkinData.FCacheBmp.Canvas.Handle, BordWidth, 0, SRCCOPY)
      else
        BitBlt(DC, AddedGlyphRect.Left - Spacing div 2, 0, AddedGlyphSpace + Spacing div 2, Height, SkinData.FCacheBmp.Canvas.Handle, AddedGlyphRect.Left - Spacing div 2, 0, SRCCOPY);
end;


procedure TsCustomComboEdit.ButtonClick;
begin
  if Assigned(FOnButtonClick) then
    FOnButtonClick(Self);

  if DroppedDown then
    CloseForm
  else
    PopupWindowShow;
end;


procedure TsCustomComboEdit.SelectAll;
begin
  if not readonly and (Text <> '') and not ButtonMode then
    SendMessage(Handle, EM_SETSEL, 0, -1);
end;


procedure TsCustomComboEdit.SetDirectInput(Value: boolean);
begin
  inherited readonly := not Value or FReadOnly or ButtonMode;
  FDirectInput := Value;
end;


procedure TsCustomComboEdit.WMPaste(var Message: TWMPaste);
begin
  if FDirectInput and not readonly and not ButtonMode then
    inherited;
end;


procedure TsCustomComboEdit.WMCut(var Message: TWMCut);
begin
  if FDirectInput and not readonly and not ButtonMode then
    inherited;
end;


procedure TsCustomComboEdit.WMMouseMove(var Message: TWMMouse);
begin
  inherited;
  if MouseInBtn then begin
    if BtnState = 0 then begin
      if GlyphEnabled then
        BtnState := ACHOT;

      if HintTimer = nil then
        HintTimer := TacHintTimer.Create(Self)
      else
        HintTimer.Enabled := True;
    end;
  end
  else begin
    BtnState := 0;
    HideHint;
  end;
end;


procedure TsCustomComboEdit.WMLButtonDown(var Message: TWMLButtonDown);
var
  oldR: TRect;
begin
  if not(csDesigning in ComponentState) then begin
    oldR := BtnRect(True);
    if IsFullPaint then begin
      BtnState := ACPRESSED;
      if not SkinData.Skinned then
        CacheToDC(MkRect(Self));

      if ComboBtn then
        ButtonClick;
    end
    else
      if GlyphEnabled and MouseInRect(oldR) then begin
        BtnState := ACPRESSED;
        if not SkinData.Skinned then
          CacheToDC(oldR);

        if ComboBtn then
          ButtonClick;
      end
      else
        inherited;
  end
  else
    inherited;
end;


procedure TsCustomComboEdit.WMLButtonUp(var Message: TWMLButtonUp);
var
  oldR: TRect;
begin
  if not(csDesigning in ComponentState) then begin
    ReleaseCapture;
    if ButtonMode then begin
      if (FBtnState = ACPRESSED) and acMouseInControl(Self) then
        if not ComboBtn then begin
          BtnState := ACHOT;
          if not SkinData.Skinned then begin
            oldR := BtnRect(True);
            CacheToDC(oldR);
          end;
          ButtonClick;
        end;
    end
    else
      if (FBtnState = ACPRESSED) and MouseInBtn then
        if GlyphEnabled and not ComboBtn then begin
          FBtnState := ACHOT;
          SkinData.Invalidate(True);
          if not SkinData.Skinned then begin
            oldR := BtnRect(True);
            CacheToDC(oldR);
          end;
          ButtonClick;
        end;
  end
  else
    inherited;
end;


procedure TsCustomComboEdit.WMLDblClick(var Message: TWMMouse);
begin
  if not(csDesigning in ComponentState) and MouseInBtn then
    WMLButtonDown(message)
  else
    inherited;
end;


procedure TsCustomComboEdit.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
  FBtnRect.Left := -1;
  if CanShowButton then
    with message.CalcSize_Params.rgrc[0] do
      if IsRightToLeft then
        Inc(Left, GlyphSpace(True))
      else
        Dec(Right, GlyphSpace(True) + BtnOffset);
end;


procedure TsCustomComboEdit.SetReadOnly(Value: boolean);
begin
  if Value <> FReadOnly then begin
    FReadOnly := Value;
    inherited readonly := Value or not FDirectInput;
  end;
end;


{$IFNDEF D2009}


procedure TsCustomComboEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then begin
    FAlignment := Value;
    if HandleAllocated and Visible then
      SkinData.Invalidate;
  end;
end;
{$ENDIF}


procedure TsCustomComboEdit.SetArrowAngle(const Value: integer);
begin
  if FArrowAngle <> Value then begin
    FArrowAngle := Value;
    SkinData.BGChanged := True;
    PrepareCache;
    CacheToDC(MkRect(Self));
  end;
end;


procedure TsCustomComboEdit.SetBtnState(const Value: integer);
begin
  if (FBtnState <> ACPRESSED) or not DroppedDown then // If not pressed
    if FBtnState <> Value then begin
      FBtnState := Value;
      if [csLoading] * ComponentState = [] then begin
        if SkinData.Skinned then begin
          SkinData.BGChanged := True;
          PrepareCache;
        end;

        if ButtonMode then
          RedrawWindow(Handle, nil, 0, RDWA_ALLNOW)
        else
          SendMessage(Handle, WM_NCPAINT, 0, 0);
      end;
    end;
end;


procedure TsCustomComboEdit.SetButtonMode(const Value: boolean);
begin
  inherited readonly := not DirectInput or FReadOnly or Value;
  if FButtonMode <> Value then begin
    FButtonMode := Value;
    UpdateCaret;
    SkinData.UpdateIndexes;
    SkinData.Invalidate;
    SkinData.OuterEffects.Invalidate;
  end;
end;


procedure TsCustomComboEdit.SetButtonSkinSection(const Value: string);
begin
  if FButtonSkinSection <> Value then begin
    FButtonSkinSection := Value;
    SkinData.Invalidate;
  end;
end;


procedure TsCustomComboEdit.SetButtonWidth(const Value: integer);
begin
  if FButtonWidth <> Value then begin
    FButtonWidth := Value;
    PaddingChanged(Self);
  end;
end;


procedure TsCustomComboEdit.WndProc(var Message: TMessage);
var
  DC: hdc;
  R: TRect;
  bWidth: integer;
begin
{$IFDEF LOGGED}
  if Tag = 2 then
    AddToLog(message);
{$ENDIF}
  case message.Msg of
    SM_ALPHACMD:
      case message.WParamHi of
        AC_POPUPCLOSED: begin
            PopupWindowClose;
            Exit;
          end;

        AC_GETTEXTRECT: begin
            PRect(message.LParam)^ := TextRect;
            message.Result := 1;
            Exit;
          end;

        AC_GETMOUSEAREA: begin
            PRect(message.LParam)^ := BtnRect(False);
            with ClientToScreen(Point(0, 0)) do
              OffsetRect(PRect(message.LParam)^, X, Y);

            message.Result := 1;
            Exit;
          end;

        AC_DOSKIPCLICK: begin
            FSkipDrop := MouseInBtn;
            Exit;
          end;

        AC_GETDEFSECTION: begin
            if ButtonMode and (SkinData.SkinManager <> nil) then
              if (SkinData.SkinManager <> nil) and (SkinData.SkinManager.SkinCommonInfo.Sections[ssComboNoEdit] >= 0) then
                message.Result := 18 + 1 // COMBONOEDIT
              else
                message.Result := 1 + 1 // BUTTON
            else
              message.Result := 0 + 1;

            Exit;
          end;

        AC_GETDEFINDEX: begin
          if SkinData.SkinManager <> nil then
            with SkinData.SkinManager.SkinCommonInfo do
              message.Result := iff(AllowBtnStyle, iff(Sections[ssComboNoEdit] >= 0, Sections[ssComboNoEdit], Sections[ssButton]), Sections[ssEdit]) + 1;

          Exit;
        end;

        AC_PREPARECACHE: begin
          PrepareCache;
          UpdateCorners(SkinData, integer(ControlIsActive(SkinData)));
          Exit;
        end;
      end;

    WM_ERASEBKGND:
      if SkinData.Skinned then begin
        if IsWindowVisible(Handle) then
          Exit;
      end
      else
        if IsFullPaint then
          Exit;

    WM_MOUSEMOVE:
      if not (csDesigning in ComponentState) then begin
        CommonWndProc(message, SkinData);
        if SkinData.FMouseAbove then
          DoMouseAbove;

        inherited;
        Exit;
      end;

    CM_MOUSEENTER:
      if not (csDesigning in ComponentState) then begin
        if not DroppedDown then
          DoMouseAbove;
        // else
        inherited;
        Exit;
      end;

    CM_MOUSELEAVE:
      if not (csDesigning in ComponentState) then begin
        if SkinData.FMouseAbove then
          SkinData.DestroyNeedlessLight(False);
        if not DroppedDown then begin
          if not acMouseInControl(Self) then begin
            SkinData.FMouseAbove := False;
            FBtnState := 0;
            AnimateCtrl(0);
          end;
          Exit;
        end;
      end;

    WM_PAINT: begin
      if SkinData.CtrlSkinState and ACS_NCCHANGED <> 0 then begin
        SkinData.BGChanged := True;
        SkinData.CtrlSkinState := SkinData.CtrlSkinState and not ACS_NCCHANGED;
        SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);
        PrepareCache;
      end;
      if not Focused or (SkinData.CtrlSkinState and ACS_TEXTANIMATING <> 0) or ButtonMode then begin
        OurPaintHandler(TWMPaint(message).DC);
        if not (Enabled and ControlIsActive(SkinData)) and (csDesigning in ComponentState) then
          inherited;
      end
      else begin
        inherited;
        PaintBorder(0);
      end;
      Exit;
    end;

    WM_PRINT:
      if SkinData.Skinned then begin
        if SkinData.CtrlSkinState and ACS_NCCHANGED <> 0 then begin
          SkinData.CtrlSkinState := SkinData.CtrlSkinState and not ACS_NCCHANGED;
          SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);
        end;
        SkinData.Updating := False;
        DC := TWMPaint(message).DC;
        if SkinData.BGChanged then
          PrepareCache;

        UpdateCorners(SkinData, integer(ControlIsActive(SkinData)));
        OurPaintHandler(DC);
        bWidth := BordWidth;
        BitBltBorder(DC, 0, 0, SkinData.FCacheBmp.Width, SkinData.FCacheBmp.Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, bWidth);
        R := BtnRect(True);
        BitBlt(DC, R.Left, R.Top, GlyphSpace(True), GlyphHeight, SkinData.FCacheBmp.Canvas.Handle, R.Left, R.Top, SRCCOPY);
        Exit;
      end;

    WM_KILLFOCUS:
      if (Self is TsDateEdit) and (FPopupWindow <> nil) then
        TForm(FPopupWindow).Close;

    WM_NCPAINT:
      if not SkinData.Skinned then begin
        inherited;
        if not IsFullPaint then
          PaintBtnStd;

        Exit;
      end;

    WM_SETCURSOR:
      if [csDesigning] * ComponentState = [] then
        if MouseInBtn then begin
          if GlyphMode.Cursor <> crDefault then
            Windows.SetCursor(Screen.Cursors[GlyphMode.Cursor])
          else
            if Cursor <> crDefault then
              Windows.SetCursor(Screen.Cursors[Cursor])
            else
              Windows.SetCursor(Screen.Cursors[crArrow]);

          Exit;
        end;
  end;
  inherited;
  case message.Msg of
    SM_ALPHACMD:
      case message.WParamHi of
        AC_REMOVESKIN, AC_SETNEWSKIN:
          if ACUInt(message.LParam) = ACUInt(SkinData.SkinManager) then
            AlphaBroadcast(Self, message);

        AC_REFRESH: begin
            if RefreshNeeded(SkinData, message) then
              AlphaBroadcast(Self, message);

            if not HandleAllocated then
              HandleNeeded;

            FBtnRect.Left := -1;
            SkinData.CtrlSkinState := SkinData.CtrlSkinState or ACS_NCCHANGED;
          end;

        AC_PREPARING:
          message.Result := 0;
      end;

    WM_SETFOCUS: begin
        if AutoSelect then
          SelectAll;

        UpdateCaret;
      end;

    CM_EXIT:
      Repaint;

    WM_SIZE, WM_WINDOWPOSCHANGED:
      FBtnRect.Left := -1;
  end
end;


procedure TsCustomComboEdit.PopupWindowClose;
begin
  FPopupWindow := nil;
  FDroppedDown := False;
  FArrowAngle := 0;
  BtnState := integer(MouseInBtn);
  StopTimer(SkinData);
  SkinData.Invalidate(True);
end;


procedure TsCustomComboEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or acAlignments[Alignment] or WS_CLIPCHILDREN;
end;


function TsCustomComboEdit.CurrentState: integer;
begin
  if AllowBtnStyle or (SkinData.SkinSection <> '') then
    if DroppedDown or (FBtnState = ACPRESSED) then
      Result := ACPRESSED
    else
      if SkinData.FMouseAbove and not(csDesigning in ComponentState) then
        Result := 1
      else
        Result := inherited CurrentState
  else
    Result := inherited CurrentState;
end;


procedure TsCustomComboEdit.KeyPress(var Key: Char);
var
  Form: TCustomForm;
begin
  if (Key = Char(VK_RETURN)) or (Key = Char(VK_ESCAPE)) then begin
    { must catch and remove this, since is actually multi-line }
    Form := GetParentForm(Self);
    if Form.KeyPreview then
      Form.Perform(CM_DIALOGKEY, Byte(Key), 0);

    if Key = Char(VK_RETURN) then begin
      inherited KeyPress(Key);
      Key := #0;
      Exit;
    end;
  end;
  inherited KeyPress(Key);
end;


procedure TsCustomComboEdit.AfterConstruction;
begin
  inherited;
  SkinData.Loaded;
end;


procedure TsCustomComboEdit.Loaded;
begin
  inherited;
  SkinData.Loaded;
end;


function TsCustomComboEdit.GetReadOnly: boolean;
begin
  Result := FReadOnly;
end;


function TsCustomComboEdit.GlyphEnabled: boolean;
begin
  Result := Enabled and not FReadOnly and GlyphMode.Enabled;
end;


function TsCustomComboEdit.GlyphHeight: integer;
begin
  if ComboBtn then
    if SkinData.Skinned then
      Result := Height - 2 * SkinData.CommonSkinData.ComboBoxMargin
    else
      Result := Height
  else
    Result := FGlyphMode.Height;
end;


function TsCustomComboEdit.GlyphSpace(AddSpacing: boolean): integer;
begin
  if CanShowButton then begin
    Result := GlyphWidth;
    if not ComboBtn then
      if AddSpacing then
        Result := GlyphWidth + SkinData.CommonSkinData.Spacing
      else
        Result := GlyphWidth + SkinData.CommonSkinData.Spacing div 2
  end
  else
    Result := 0;
end;


function TsCustomComboEdit.GlyphWidth: integer;
begin
  if ComboBtn then
    if ButtonWidth = 0 then
      Result := GetComboBtnSize(SkinData.SkinManager, SkinData.CommonSkinData.PPI)
    else
      Result := ScaleInt(ButtonWidth, SkinData)
  else
    Result := FGlyphMode.Width;
end;


procedure TsCustomComboEdit.HideHint;
begin
  acHideHintWnd(HintWnd);
end;


function TsCustomComboEdit.IsFullPaint: boolean;
begin
  Result := ButtonMode;
end;


procedure TsCustomComboEdit.OurPaintHandler(DC: hdc);
var
  NewDC, SavedDC: hdc;
  PS: TPaintStruct;
  R: TRect;
begin
  if not InAnimationProcess then
    BeginPaint(Handle, PS);

  if DC = 0 then
    NewDC := GetWindowDC(Handle)
  else
    NewDC := DC;

  SavedDC := SaveDC(NewDC);
  try
    R := BodyRect;
    if SkinData.Skinned then begin
      if not InUpdating(SkinData) and not(InAnimationProcess and (DC <> SkinData.PrintDC) and (SkinData.PrintDC <> 0)) and ((SkinData.AnimTimer = nil) or not SkinData.AnimTimer.Enabled) then begin
        SkinData.BGChanged := SkinData.BGChanged or SkinData.HalfVisible or GetBoolMsg(Parent, AC_GETHALFVISIBLE);
        SkinData.HalfVisible := not RectInRect(Parent.ClientRect, BoundsRect);
        if SkinData.BGChanged then
          PrepareCache;

        UpdateCorners(SkinData, integer(ControlIsActive(SkinData)));
        BitBlt(NewDC, 0, 0, Width, R.Bottom, SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
      end;
    end
    else begin
      PrepareCache;
      if (BorderStyle = bsNone) or AllowBtnStyle then
        BitBlt(NewDC, 0, 0, Width, R.Bottom, SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY)
      else begin
        BitBlt(NewDC, 3, 3, Width - 6, R.Bottom - 6, SkinData.FCacheBmp.Canvas.Handle, 3, 3, SRCCOPY);
        if CanShowButton then begin
          R := BtnRect(False);
          BitBlt(NewDC, R.Left, R.Top, WidthOf(R, True), HeightOf(R, True), SkinData.FCacheBmp.Canvas.Handle, R.Left, R.Top, SRCCOPY);
        end;
      end;
    end;
  finally
    RestoreDC(NewDC, SavedDC);
    if DC = 0 then
      ReleaseDC(Handle, NewDC);

    if not InAnimationProcess then
      EndPaint(Handle, PS);
  end;
end;


procedure TsCustomComboEdit.PaintBorder(DC: hdc);
var
  NewDC, SavedDC: hdc;
begin
  if Assigned(Parent) and Visible and Parent.Visible and not(csCreating in ControlState) and not SkinData.Updating then begin
    if (BorderStyle = bsNone) and not SkinData.Skinned then begin
      inherited;
      Exit;
    end;

    if IsFullPaint or (SkinData.AnimTimer <> nil) and SkinData.AnimTimer.Enabled then
      Exit;

    if DC = 0 then
      NewDC := GetWindowDC(Handle)
    else
      NewDC := DC;

    SavedDC := SaveDC(NewDC);
    try
      if SkinData.BGChanged then begin
        PrepareCache;
        UpdateCorners(SkinData, integer(ControlIsActive(SkinData)));
      end;
      PaintNCArea(NewDC);
    finally
      RestoreDC(NewDC, SavedDC);
      if DC = 0 then
        ReleaseDC(Handle, NewDC);
    end;
  end;
end;


{$IFDEF DELPHI7UP}

const
{$IFDEF DELPHI_XE3}
  ComboStates: array [0 .. 2] of TThemedComboBox = (tcDropDownButtonRightNormal, tcDropDownButtonRightHot, tcDropDownButtonRightPressed);
{$ELSE}
  ComboStates: array [0 .. 2] of TThemedComboBox = (tcDropDownButtonNormal, tcDropDownButtonHot, tcDropDownButtonPressed);
{$ENDIF}
  BtnStates: array [0 .. 2] of TThemedHeader = (thHeaderItemNormal, thHeaderItemHot, thHeaderItemPressed);
{$ENDIF}


procedure TsCustomComboEdit.PaintBtn;
var
  DrawData: TacDrawGlyphData;
  h, w, Mode, Ndx: integer;
  C: TColor;
  R: TRect;
{$IFDEF DELPHI7UP}
  Details: TThemedElementDetails;
{$ENDIF}
begin
  if CanShowButton then begin
    R := BtnRect(False);
    if ComboBtn then begin
      with SkinData do
        if Skinned then begin
          Mode := min(BtnState, ac_MaxPropsIndex);
          with SkinManager, SkinData.CommonSkinData, ComboBtn do
            if not AllowBtnStyle then begin
              if ButtonSkinSection = '' then
                Ndx := SkinData.SkinManager.SkinCommonInfo.Sections[ssComboBtn]
              else
                Ndx := SkinData.SkinManager.GetSkinIndex(ButtonSkinSection, SkinData.CommonSkinData);

              if Ndx >= 0 then
                PaintItem(Ndx, MakeCacheInfo(FCacheBmp), True, BtnState, R, MkPoint, FCacheBmp, SkinData.CommonSkinData);

              if not gd[SkinData.SkinIndex].GiveOwnFont and IsValidImgIndex(GlyphIndex) then
                DrawSkinGlyph(FCacheBmp, Point(R.Left + (WidthOf(R) - ma[GlyphIndex].Width) div 2, (Height - ma[GlyphIndex].Height) div 2), Mode, 1, ma[GlyphIndex], MakeCacheInfo(FCacheBmp))
              else begin         // Paint without glyph
                if Ndx >= 0 then // If COMBOBTN used
                  if Ndx = SkinData.SkinManager.SkinCommonInfo.Sections[ssTransparent] then
                    C := GetFontColor(Self, SkinData.SkinIndex, SkinData.SkinManager, CurrentState)
                  else
                    C := SkinData.CommonSkinData.gd[Ndx].Props[Mode].FontColor.Color
                else
                  if SkinData.Skinned then
                    C := gd[SkinData.SkinIndex].Props[Mode].FontColor.Color
                  else
                    C := ColorToRGB(clWindowText);

                DrawArrow(FCacheBmp, C, clNone, R, asBottom, 0, FArrowAngle, 0, Options.ActualArrowStyle, GetPPI(SkinData));
              end
            end
            else begin
              if not gd[SkinData.SkinIndex].GiveOwnFont and IsValidImgIndex(GlyphIndex) then
                DrawSkinGlyph(FCacheBmp, Point(R.Left + (WidthOf(R) - ma[GlyphIndex].Width) div 2, (Height - ma[GlyphIndex].Height) div 2), Mode, 1, ma[GlyphIndex], MakeCacheInfo(FCacheBmp))
              else begin // Paint without glyph
                if SkinData.Skinned then
                  C := gd[SkinData.SkinIndex].Props[Mode].FontColor.Color
                else
                  C := ColorToRGB(clWindowText);

                DrawArrow(FCacheBmp, C, clNone, R, asBottom, 0, FArrowAngle, 0, Options.ActualArrowStyle, GetPPI(SkinData));
              end;
            end;
        end
        else begin
          FCacheBmp.PixelFormat := pf32bit;
          FCacheBmp.Width := Width;
          FCacheBmp.Height := Height;
{$IFDEF DELPHI7UP}
          if acThemesEnabled then begin

            if AllowBtnStyle then
              Details := acThemeServices.GetElementDetails({$IFDEF DELPHI_XE3}tcDropDownButtonRightNormal{$ELSE}tbPushButtonNormal{$ENDIF})
            else
              if BtnState <> 0 then begin
                Details := acThemeServices.GetElementDetails(ComboStates[BtnState]);
                acThemeServices.DrawElement(FCacheBmp.Canvas.Handle, Details, R);
              end;

            DrawArrow(FCacheBmp, Font.Color, clNone, R, asBottom, 0, 0, 0, arsSolid1, GetPPI(SkinData));
          end
          else
{$ENDIF}
          begin
            if BorderStyle = bsSingle then begin
              if Ctl3d then
                w := 2
              else
                w := 1;

              inc(R.Top, w);
              dec(R.Bottom, w);
              if IsRightToLeft then
                inc(R.Left, w)
              else
                dec(R.Right, w);
            end;
            FillDC(FCacheBmp.Canvas.Handle, R, ColorToRGB(Color));
            DrawArrow(FCacheBmp, Font.Color, clNone, R, asBottom, 0, 0, 0, arsSolid1, GetPPI(SkinData));
          end;
        end;
    end
    else begin
      if (BtnState > 0) and not ac_OldGlyphsMode and not ButtonMode then begin
        if SkinData.Skinned then begin
          if ButtonSkinSection = '' then
            Ndx := SkinData.SkinManager.SkinCommonInfo.Sections[ssComboBtn]
          else
            Ndx := SkinData.SkinManager.GetSkinIndex(ButtonSkinSection, SkinData.CommonSkinData);

          if Ndx < 0 then begin
            Ndx := SkinData.SkinManager.SkinCommonInfo.Sections[ssUpDown];
            if Ndx < 0 then
              Ndx := SkinData.SkinManager.SkinCommonInfo.Sections[ssSpeedButton_Small];
          end;
          if Ndx >= 0 then begin
            PaintItem(Ndx, MakeCacheInfo(SkinData.FCacheBmp), True, BtnState, R, MkPoint, SkinData.FCacheBmp, SkinData.CommonSkinData);
            SkinData.FCacheBmp.Canvas.Font.Color := SkinData.CommonSkinData.gd[Ndx].Props[BtnState].FontColor.Color;
          end;
        end
{$IFDEF DELPHI7UP}
        else
          if acThemesEnabled then begin
            Details := acThemeServices.GetElementDetails(BtnStates[BtnState]);
            Inc(R.Top);
            acThemeServices.DrawElement(SkinData.FCacheBmp.Canvas.Handle, Details, R);
            Dec(R.Top);
          end
{$ENDIF}
      end;
      DrawData.Glyph := nil;
      if Assigned(GlyphMode.Images) and IsValidIndex(GlyphMode.ImageIndex, GetImageCount(GlyphMode.Images)) then begin
        DrawData.Blend := GlyphMode.Blend;
        DrawData.Images := GlyphMode.Images;
        DrawData.NumGlyphs := 1;
        case BtnState of
          0:
            DrawData.ImageIndex := GlyphMode.ImageIndex;
          1:
            DrawData.ImageIndex := GlyphMode.ImageIndexHot;
          2:
            DrawData.ImageIndex := GlyphMode.ImageIndexPressed;
        end;
        if not IsValidIndex(DrawData.ImageIndex, GetImageCount(GlyphMode.Images)) then
          DrawData.ImageIndex := GlyphMode.ImageIndex;
      end
      else begin
        DrawData.Blend := GlyphMode.Blend;
        DrawData.Images := acResImgList;
        DrawData.ImageIndex := FDefBmpID;
        DrawData.NumGlyphs := DrawData.Images.Width div DrawData.Images.Height;
      end;
      DrawData.CurrentState := BtnState;
      DrawData.Down := False;
      DrawData.Enabled := Enabled;
      DrawData.Reflected := False;
      DrawData.DstBmp := SkinData.FCacheBmp;
      DrawData.CommonSkinData := SkinData.CommonSkinData;
      DrawData.PPI := GetPPI(SkinData);
      DrawData.ImgRect := R;
      w := GetImageWidth(DrawData.Images, DrawData.ImageIndex, DrawData.PPI) div DrawData.NumGlyphs;
      h := GetImageHeight(DrawData.Images, DrawData.ImageIndex, DrawData.PPI);
      DrawData.ImgRect.Left := R.Left + (WidthOf(R) - w) div 2 + 1;
      DrawData.ImgRect.Top := R.Top + (HeightOf(R) - h) div 2;
      DrawData.ImgRect.Right := DrawData.ImgRect.Left + w;
      DrawData.ImgRect.Bottom := DrawData.ImgRect.Top + h;

      DrawData.DisabledGlyphKind := DefDisabledGlyphKind;
      if SkinData.Skinned then begin
        DrawData.Canvas := SkinData.FCacheBmp.Canvas;
        DrawData.Grayed := (DrawData.CurrentState = 0) and (GlyphMode.Grayed or SkinData.SkinManager.Effects.DiscoloredGlyphs);
        if DrawData.Grayed then
          DrawData.BGColor := SkinData.CommonSkinData.gd[SkinData.SkinIndex].Props[DrawData.CurrentState].Color
        else
          DrawData.BGColor := clNone;
      end
      else begin
        DrawData.Canvas := nil;
        DrawData.Grayed := GlyphMode.Grayed;
        if DrawData.Grayed then
          DrawData.BGColor := Color
        else
          DrawData.BGColor := clNone;
      end;
      if not GlyphEnabled then
        DrawData.Blend := DrawData.Blend + (100 - DrawData.Blend) div 2;

      if GlyphMode.ColorTone <> clNone then
        if (DrawData.Images is TsCharImageList) or ((DrawData.Images is TsVirtualImageList) and (TsVirtualImageList(DrawData.Images).AlphaImageList is TsCharImageList)) then begin
          DrawData.Grayed := False;
          if DrawData.DstBmp <> nil then begin
            DrawData.DstBmp.Canvas.Font.Color := GlyphMode.ColorTone;
            DrawData.DstBmp.Canvas.Font.Size := 0; // Def color is not allowed
          end;
        end
        else begin
          DrawData.Grayed := True;
          DrawData.BGColor := GlyphMode.ColorTone;
        end;
      acDrawGlyphEx(DrawData);
    end;
  end;
end;


procedure TsCustomComboEdit.PaintText;
var
  R: TRect;
  SavedDC: hdc;
  aText: acString;
  Flags: Cardinal;
  TranspMode: Byte;
begin
  with SkinData.FCacheBmp do begin
    Canvas.Font.Assign(Font);
    Flags := DT_NOPREFIX or DT_SINGLELINE;
    R := TextRect;
{$IFNDEF D2009}
    if IsRightToLeft then begin
      Flags := Flags or DT_RTLREADING or DT_RIGHT;
      Dec(R.Right);
    end;
{$ENDIF}
    TranspMode := GetTransMode(SkinData);
    if TranspMode = TM_FULL then
      Dec(R.Left, 2);

    Flags := Flags or DT_ALPHATEXT * Cardinal(TranspMode = TM_FULL);
{$IFDEF D2009}
    Flags := Flags or Cardinal(GetStringFlags(Self, Alignment)) and not DT_VCENTER;
{$ENDIF}
    if not IsDefaultValue then begin
      if PasswordChar <> #0 then
        acFillString(aText, Length(Text), iff(PasswordChar = '*', CharBullet, acChar(PasswordChar)))
      else
        aText := Text;

      SavedDC := SaveDC(Canvas.Handle);
      IntersectClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
      try
        acWriteTextEx(Canvas, PacChar(aText), Enabled or SkinData.Skinned, R, Flags, SkinData, ControlIsActive(SkinData));
      finally
        RestoreDC(Canvas.Handle, SavedDC);
      end;
    end
    else
      if not SkinData.FFocused then
        TextHintOut(SkinData, R, TextHint, Flags)
      else
        Exit;
  end;

  if ButtonMode and ShowFocus and Focused and CanShowFocus(SkinData) then begin
    InflateRect(R, 1 + integer(not AllowBtnStyle), 0);
    acDrawFocusRect(SkinData.FCacheBmp.Canvas, R);
  end;

  if TranspMode = TM_PARTIAL then
    FillAlphaRect(SkinData.FCacheBmp, R);
end;


procedure TsCustomComboEdit.CMFocuseChanged(var Message: TCMFocusChanged);
begin
  if not(csDesigning in ComponentState) and DroppedDown and (message.Sender <> Self) then
    CloseForm;

  inherited;
end;


procedure TsCustomComboEdit.CMWantSpecialKey(var Message: TCMWantSpecialKey);
begin
  inherited;
  if (message.CharCode = VK_RETURN) and DroppedDown then
    message.Result := 1;
end;


function TsCustomComboEdit.ComboBtn: boolean;
begin
  Result := (FDefBmpID = iBTN_ARROW) and not((GlyphMode.Images <> nil) and IsValidIndex(GlyphMode.ImageIndex, GetImageCount(GlyphMode.Images)));
end;


procedure TsCustomComboEdit.SetShowButton(const Value: boolean);
begin
  if FShowButton <> Value then begin
    FShowButton := Value;
    SkinData.Invalidate;
    if not(csLoading in ComponentState) and HandleAllocated then
      SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);
  end;
end;


procedure TsCustomComboEdit.TextPaintPrevent(Locked: boolean);
begin
  FContentHidden := Locked;
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);
end;


function TsCustomComboEdit.TextRect: TRect;
begin
  Result := inherited TextRect;
  if IsRightToLeft then
    Result.Left := GlyphSpace(True) + BtnOffset + 1
  else
    Result.Right := Result.Right - BtnOffset - GlyphSpace(True) - 1;
end;


procedure TsCustomComboEdit.TryShowBtnHint;
begin
  if Application.ShowHint and ShowHint and (GlyphMode.Hint <> '') then
    if HintWnd = nil then
      if HintWindowClass = THintWindow then
        HintWnd := acShowHintWnd(GlyphMode.Hint, Point(acMousePos.X, acMousePos.Y))
      else
        acShowHintWnd(GlyphMode.Hint, Point(acMousePos.X, acMousePos.Y + 16));
end;


procedure TsCustomComboEdit.UpdateCaret;
begin
  if HandleAllocated then
    if ButtonMode then
      HideCaret(Handle)
    else
      ShowCaret(Handle);
end;


procedure TsCustomComboEdit.UpdateLight(Sender: TObject; Data: PacAnimEventData);
begin
  if AllowBtnStyle then begin
    if IsWindowVisible(Handle) and Enabled and not DroppedDown and ( { (aeLighting in AnimatEvents) or (aeGlobalDef in AnimatEvents) and (SkinData.SkinManager <> nil) and }
      (beLighting in SkinData.SkinManager.AnimEffects.buttons.Events)) then
      DoUpdateLight(SkinData, Data.DoOutput)
    else
      SkinData.FDoLighting := False;

    SkinData.DestroyNeedlessLight;
    Data.DoLighting := SkinData.DoLighting;
  end;
end;


procedure TsCustomComboEdit.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (FGlyphMode <> nil) and (AComponent = FGlyphMode.Images) then
    FGlyphMode.Images := nil;
end;


constructor TacHintTimer.Create(AOwner: TComponent);
begin
  inherited;
  FOwner := AOwner;
  Interval := Application.HintPause;
  OnTimer := TimerEvent;
  Enabled := True;
end;


procedure TacHintTimer.TimerEvent(Sender: TObject);
begin
  Enabled := False;
  TsCustomComboEdit(FOwner).TryShowBtnHint;
end;

end.
