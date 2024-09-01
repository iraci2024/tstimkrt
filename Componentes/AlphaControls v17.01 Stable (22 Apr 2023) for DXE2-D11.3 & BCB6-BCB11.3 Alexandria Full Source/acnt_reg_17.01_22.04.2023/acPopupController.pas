unit acPopupController;
{$I sDefs.inc}
// {$DEFINE LOGGED}
//{$DEFINE AC_NOSHADOW}
//{$DEFINE AC_NOHANDLECTRL}
//{$DEFINE AC_NOHANDLEFORM}

interface

uses
{$IFNDEF DELPHI5} Types, {$ENDIF}
{$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  Forms, Windows, Graphics, SysUtils, Classes, Controls, Messages,
  acThdTimer, sCommonData, acntTypes, sSkinManager;


type
  TsPopupController = class;


  TacFormHandler = class(TObject)
  protected
    ClientCtrl: TWinControl;
{$IFNDEF AC_NOHANDLEFORM}
    FormWndProc: TWndMethod;
{$ENDIF}
{$IFNDEF AC_NOHANDLECTRL}
    CtrlWndProc: TWndMethod;
{$ENDIF}
    Initialized, ClosingForbidden: boolean;
    CaptureHandle: THandle;
    OldOnClose: TCloseEvent;
    CheckTimer: TacThreadedTimer;
    Controller: TsPopupController;
    procedure UnInitControls;
    procedure DoTimer(Sender: TObject);
    procedure UpdateRgnBmp(aBmp: TBitmap);
    function CloseForm(CallHideCloseProc: boolean = True): TCloseAction;
    constructor Create(AForm: TForm; ACtrl: TWinControl);
    procedure InitControls(AForm: TForm; ACtrl: TWinControl);
{$IFNDEF AC_NOHANDLEFORM}
    procedure DoWndProc(var Message: TMessage);
{$ENDIF}
{$IFNDEF AC_NOHANDLECTRL}
    procedure DoCtrlProc(var Message: TMessage);
{$ENDIF}
  public
    Closed: boolean;
    PopupForm: TForm;
    ParentForm: TForm;
    PopupCtrl: TWinControl;
{$IFNDEF AC_NOSHADOW}
    ShadowForm: TCustomForm;
{$ENDIF}
    destructor Destroy; override;
  end;


{$IFDEF DELPHI_XE3}[ComponentPlatformsAttribute(pidWin32 or pidWin64)]
{$ENDIF}


  TsPopupController = class(TComponent)
  protected
    DoSetFocus, SkipOpen: boolean;
    AnimDirection: integer;
    FAnimProc: TacAnimProcEx;
    procedure DoDeactivate(Sender: TObject);
    procedure DoClose(Sender: TObject; var Action: TCloseAction);
    function InitFormHandler(AForm: TForm; Ctrl: TWinControl): integer;
    function HasChild(Parent: TForm): boolean;
    procedure AnimShowPopup(AForm: TForm; wTime: word = 0; BlendValue: byte = MaxByte);
    procedure ShowForm(AForm: TForm; AOwnerControl: TWinControl; ALeft: integer = -1; ATop: integer = -1; Animated: boolean = True; AnimProc: TacAnimProcEx = nil);
    procedure ShowFormPos(AForm: TForm; ALeftTop: TPoint; Animated: boolean = True; AnimProc: TacAnimProcEx = nil);
  public
    ZoomAnimation, MousePressed, IgnoreCapture: boolean;
    FormHandlers: array of TacFormHandler;
    procedure ClosingForbide(AForm: TForm);
    procedure ClosingAllow(AForm: TForm);
    function GetFormIndex(Form: TForm): integer; overload;
    function GetFormIndex(FormHandle: THandle): integer; overload;
    function PopupCount(ExceptForm: TForm): integer;
    function PopupChildCount(ExceptForm: TForm): integer;
    procedure KillAllForms(ExceptChild: TControl); // TForm);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


{$IFNDEF AC_NOSHADOW}


type
  TacShadowForm = class(TacGlowForm)
  protected
    sd: TsCommonData;
    FOwnerForm: TForm;
    FOwnerBlend: byte;
    FDestroyed: boolean;
    FOwnerHandle: THandle;
    OldWndProc: TWndMethod;
    function OwnerHandle: THandle;
    procedure AC_WMNCHitTest(var Message: TMessage); message WM_NCHITTEST;
  public
    Locked: boolean;
    FUserBlend: integer;
    procedure HideWnd;
    procedure KillShadow;
    procedure UpdateShadowPos;
    function ShadowSize: TRect;
    function SkinManager: TsSkinManager;
    function ShadowTemplate: TBitmap;
    procedure NewWndProc(var Message: TMessage);
    constructor CreateNew(AOwner: TComponent; Dummy: integer = 0); override;
    constructor CreateNoForm(AOwnerHandle: THandle; AOldWndProc: TWndMethod; ASkinData: TsCommonData; AOwnerBlendValue: byte = MaxByte);
    procedure SetNewPos(ALeft, ATop, aWidth, aHeight: integer; BlendValue: byte);
  end;
{$ENDIF}


function GetIntController: TsPopupController;
procedure ShowPopupForm(AForm: TForm; AOwnerControl: TWinControl; ALeft: integer = -1; ATop: integer = -1; Animated: boolean = True; AnimProc: TacAnimProcEx = nil; AZoomAnimation: boolean = False); overload;
procedure ShowPopupForm(AForm: TForm; ALeftTop: TPoint; Animated: boolean = True; AnimProc: TacAnimProcEx = nil); overload;
{$IFNDEF AC_NOSHADOW}
function AttachShadowForm(AForm: TForm; DoShow: boolean = True): TCustomForm;
function AttachShadowWnd(aHandle: THandle; AOldWndProc: TWndMethod; ASkinData: TsCommonData; AOwnerBlendValue: byte; DoShow: boolean = True): TCustomForm;
{$ENDIF}

implementation

uses
  math,
  sMessages, sGraphUtils, acntUtils, sVclUtils, StdCtrls, sComboBox, sConst, sSkinProvider,
  sAlphaGraph,
{$IFNDEF ALITE}
  acPopupCtrls, sToolEdit,
{$ENDIF}
  sComboBoxes, acSBUtils, sGradient, sStyleSimply;

var
  acIntController: TsPopupController = nil;


var
  rShadowBmp: TBitmap = nil;


function GetIntController: TsPopupController;
begin
  if acIntController = nil then
    acIntController := TsPopupController.Create(nil);

  Result := acIntController;
end;


procedure ShowPopupForm(AForm: TForm; AOwnerControl: TWinControl; ALeft: integer = -1; ATop: integer = -1; Animated: boolean = True; AnimProc: TacAnimProcEx = nil; AZoomAnimation: boolean = False);
begin
  with GetIntController do begin
    ZoomAnimation := AZoomAnimation;
    ShowForm(AForm, AOwnerControl, ALeft, ATop, Animated, AnimProc);
  end;
end;


procedure ShowPopupForm(AForm: TForm; ALeftTop: TPoint; Animated: boolean = True; AnimProc: TacAnimProcEx = nil);
begin
  GetIntController.KillAllForms(AForm);
  acIntController.ShowFormPos(AForm, ALeftTop, Animated, AnimProc);
end;


procedure TsPopupController.KillAllForms(ExceptChild: TControl); // TForm);
var
  i: integer;
  ParentForm: TForm;
  ClosedAll: boolean;
begin
  if ExceptChild <> nil then
    ParentForm := TForm(GetOwnerForm(ExceptChild.Owner))
  else
    ParentForm := nil;

  for i := 0 to Length(FormHandlers) - 1 do
    if (FormHandlers[i] <> nil) and not FormHandlers[i].ClosingForbidden and (FormHandlers[i].PopupForm <> nil) and (FormHandlers[i].PopupForm <> ParentForm) and FormHandlers[i].PopupForm.Visible then
      FormHandlers[i].PopupForm.Close;

  if PopupCount(nil) = 0 then begin
    ClosedAll := True;
    for i := 0 to Length(FormHandlers) - 1 do
      if FormHandlers[i] <> nil then
        if not FormHandlers[i].ClosingForbidden then
          FreeAndNil(FormHandlers[i])
        else
          ClosedAll := False;

    if ClosedAll then
      SetLength(FormHandlers, 0);
  end;
end;


function TsPopupController.PopupChildCount(ExceptForm: TForm): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Length(FormHandlers) - 1 do
    if (FormHandlers[i] <> nil) and (FormHandlers[i].PopupForm <> nil) and (FormHandlers[i].ParentForm = ExceptForm) and IsWindowVisible(FormHandlers[i].PopupForm.Handle) then
      inc(Result);
end;


function TsPopupController.PopupCount(ExceptForm: TForm): integer;
var
  i: integer;
begin
  Result := 0;
  for i := 0 to Length(FormHandlers) - 1 do
    if (FormHandlers[i] <> nil) and (FormHandlers[i].PopupForm <> nil) and (FormHandlers[i].PopupForm <> ExceptForm) and IsWindowVisible(FormHandlers[i].PopupForm.Handle) then
      inc(Result);
end;


function TsPopupController.GetFormIndex(Form: TForm): integer;
var
  i, l: integer;
begin
  Result := -1;
  l := Length(FormHandlers);
  for i := 0 to l - 1 do
    if (FormHandlers[i] <> nil) and (FormHandlers[i].PopupForm = Form) then begin
      Result := i;
      Exit;
    end;
end;


function TsPopupController.GetFormIndex(FormHandle: THandle): integer;
var
  i, l: integer;
begin
  Result := -1;
  l := Length(FormHandlers);
  for i := 0 to l - 1 do
    if (FormHandlers[i] <> nil) and (FormHandlers[i].PopupForm <> nil) and FormHandlers[i].PopupForm.HandleAllocated and (FormHandlers[i].PopupForm.Handle = FormHandle) then begin
      Result := i;
      Exit;
    end;
end;


function TsPopupController.HasChild(Parent: TForm): boolean;
var
  i: integer;
begin
  Result := False;
  for i := 0 to Length(FormHandlers) - 1 do
    if (FormHandlers[i].PopupForm <> Parent) and (FormHandlers[i].PopupCtrl <> nil) then
      if GetParentForm(FormHandlers[i].PopupCtrl) = Parent then begin
        Result := True;
        Exit;
      end;
end;


procedure TsPopupController.ClosingAllow(AForm: TForm);
var
  i: integer;
begin
  for i := 0 to Length(FormHandlers) - 1 do
    if (FormHandlers[i] <> nil) and (FormHandlers[i].PopupForm = AForm) then
      FormHandlers[i].ClosingForbidden := False;
end;


procedure TsPopupController.ClosingForbide(AForm: TForm);
var
  i: integer;
begin
  for i := 0 to Length(FormHandlers) - 1 do
    if (FormHandlers[i] <> nil) and (FormHandlers[i].PopupForm = AForm) then
      FormHandlers[i].ClosingForbidden := True;
end;


constructor TsPopupController.Create(AOwner: TComponent);
begin
  inherited;
  SkipOpen := False;
  DoSetFocus := False;
  ZoomAnimation := False;
end;


destructor TsPopupController.Destroy;
var
  i: integer;
begin
  for i := 0 to Length(FormHandlers) - 1 do
    FreeAndNil(FormHandlers[i]);

  SetLength(FormHandlers, 0);
  inherited;
end;


procedure TsPopupController.DoClose(Sender: TObject; var Action: TCloseAction);
var
  i: integer;
begin
  i := GetFormIndex(TForm(Sender));
  if i >= 0 then
    Action := FormHandlers[i].CloseForm(False);
end;


procedure TsPopupController.DoDeactivate(Sender: TObject);
var
  i: integer;
begin
  i := GetFormIndex(TForm(Sender));
  if i >= 0 then
    FormHandlers[i].CloseForm;
end;

// {$DEFINE ACDEBUG}


procedure TsPopupController.AnimShowPopup(AForm: TForm; wTime: word = 0; BlendValue: byte = MaxByte);
{$IFDEF ACDEBUG}
const
  DebugOffsX = 100;
  DebugOffsY = -100;
{$ENDIF}
var
  AnimBmp, acDstBmp: TBitmap;
  AnimForm: TacGlowForm;
  MaxTransparency: byte;
  PopupCtrl: TControl;
  acwPopupDivX, acwPopupDivY: real;

  DC: hdc;
  h: hwnd;
  fR: TRect;
  lTicks: DWord;
  Flags: Cardinal;
  FBmpSize: TSize;
  FBmpTopLeft: TPoint;
  FBlend: TBlendFunction;
  hNdx, i, StepCount: integer;
  dx, dy, l, t, r, b, trans, p: real;

  procedure Anim_Init;
  begin
    trans := 0;
    p := (MaxTransparency - 50) / StepCount;
    if PopupCtrl <> nil then
      r := PopupCtrl.Width
    else
      r := acDstBmp.Width;

    b := 0;
    p := (MaxTransparency - 50) / StepCount;
    acwPopupDivY := exp(1 / StepCount * ln(acDstBmp.Height div 2 { finish size } ));
    acwPopupDivX := exp(1 / StepCount * ln(acDstBmp.Width div 2 { finish size } ));
    if ZoomAnimation then begin
      acwPopupDivY := acwPopupDivY * 2;
      acwPopupDivX := acwPopupDivX * 2;
      l := acDstBmp.Width div 2;
      t := acDstBmp.Height div 2;
    end
    else begin
      l := 0;
      t := 0;
    end;
    dx := (acDstBmp.Width - r) / acwPopupDivX;
    dy := (acDstBmp.Height - b) / acwPopupDivY;
  end;

  procedure Anim_DoNext;
  var
    Data: TacAnimData;
  begin
    trans := min(trans + p, MaxTransparency);
    FBlend.SourceConstantAlpha := Round(trans);

    if Assigned(FAnimProc) then begin
      Data.Owner := PopupCtrl;
      Data.Iteration := i;
      Data.Iterations := StepCount;
      FAnimProc(Data);
      StepCount := Data.Iterations;
    end;
    if (l < 0) or (t < 0) then
      BitBlt(AnimBmp.Canvas.Handle, 0, 0, acDstBmp.Width, acDstBmp.Height, acDstBmp.Canvas.Handle, 0, 0, SRCCOPY)
    else
      StretchBlt(AnimBmp.Canvas.Handle, Round(l), Round(t), Round(r - l), Round(b - t), acDstBmp.Canvas.Handle, 0, 0, acDstBmp.Width, acDstBmp.Height, SRCCOPY);
  end;

  function Anim_GoToNext: boolean;
  begin
    if ZoomAnimation then begin
      l := dx;
      t := dy;
    end;
    r := acDstBmp.Width - dx;
    b := acDstBmp.Height - dy;
    dx := dx / acwPopupDivX;
    dy := dy / acwPopupDivY;
    Result := True;
  end;

  procedure UpdateShadowPos(aHandle: THandle; const OwnerBounds: TacBounds; ABlend: byte);
  var
    lPos: TacLayerPos;
  begin
    with lPos do begin
      Bounds := OwnerBounds;
      lPos.LayerBlend := ABlend;
    end;
    SendAMessage(aHandle, AC_UPDATESHADOW, LParam(@lPos));
  end;

begin
  AnimForm := TacGlowForm.CreateNew(nil);
  InAnimationProcess := True;
  acDstBmp := CreateBmp32(AForm);
  acDstBmp.Canvas.Lock;
  SkinPaintTo(acDstBmp, AForm);
  if acDstBmp <> nil then begin
    FillAlphaRect(acDstBmp, MkRect(acDstBmp), MaxByte);
    acDstBmp.Canvas.UnLock;
    FBmpSize := MkSize(acDstBmp);
    StepCount := wTime div acTimerInterval;
    FBmpTopLeft := MkPoint;
{$IFDEF DELPHI7UP}
    MaxTransparency := iff(AForm.AlphaBlend, AForm.AlphaBlendValue, MaxByte);
{$ELSE}
    MaxTransparency := MaxByte;
{$ENDIF}
    if StepCount > 0 then
      InitBlendData(FBlend, 0)
    else
      InitBlendData(FBlend, MaxTransparency);

    GetWindowRect(AForm.Handle, fR);
    h := TopWndAfter;

    if GetWindowLong(AnimForm.Handle, GWL_EXSTYLE) and WS_EX_LAYERED = 0 then
      SetWindowLong(AnimForm.Handle, GWL_EXSTYLE, GetWindowLong(AnimForm.Handle, GWL_EXSTYLE) or WS_EX_LAYERED);

    DC := GetDC(0);
    try
      UpdateLayeredWindow(AnimForm.Handle, DC, nil, @FBmpSize, acDstBmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA);
    finally
      ReleaseDC(0, DC);
    end;

    AnimForm.SetBounds(fR.Left{$IFDEF ACDEBUG} + DebugOffsX{$ENDIF}, fR.Top{$IFDEF ACDEBUG} + DebugOffsY{$ENDIF}, acDstBmp.Width, acDstBmp.Height);

    Flags := SWP_NOREDRAW or SWP_NOCOPYBITS or SWP_NOOWNERZORDER or SWP_NOSENDCHANGING;
    if not DoSetFocus then
      Flags := Flags or SWP_NOACTIVATE;

    SetWindowPos(AnimForm.Handle, h, AnimForm.Left, AnimForm.Top, FBmpSize.cx, FBmpSize.cy, Flags);

    hNdx := GetFormIndex(AForm);
    if hNdx >= 0 then begin
      FormHandlers[hNdx].UpdateRgnBmp(acDstBmp);
      PopupCtrl := FormHandlers[hNdx].PopupCtrl;
    end
    else
      PopupCtrl := nil;

    if DoSetFocus then
      ShowWindow(AnimForm.Handle, SW_SHOW)
    else
      ShowWindow(AnimForm.Handle, SW_SHOWNOACTIVATE);

    AnimBmp := CreateBmp32(FBmpSize);
    FillDC(AnimBmp.Canvas.Handle, MkRect(AnimBmp), 0);
    SetStretchBltMode(AnimBmp.Canvas.Handle, COLORONCOLOR);

    if StepCount > 0 then begin
      Anim_Init;
      i := 0;
      while i <= StepCount do begin
        Anim_DoNext;
        lTicks := GetTickCount;

        DC := GetDC(0);
        try
          UpdateLayeredWindow(AnimForm.Handle, DC, nil, @FBmpSize, AnimBmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA);
        finally
          ReleaseDC(0, DC);
        end;
        UpdateShadowPos(AForm.Handle, acBounds(AForm.Left + Round(l), AForm.Top + Round(t), Round(r - l), Round(b - t)), FBlend.SourceConstantAlpha);

        inc(i);
        Anim_GoToNext;
        if StepCount > 0 then
          WaitTicks(lTicks);
      end;
      FBlend.SourceConstantAlpha := MaxTransparency;
    end;
    SetWindowPos(AnimForm.Handle, 0, fR.Left{$IFDEF ACDEBUG} + DebugOffsX{$ENDIF}, fR.Top{$IFDEF ACDEBUG} + DebugOffsY{$ENDIF}, FBmpSize.cx, FBmpSize.cy, Flags or SWP_NOZORDER);
    DC := GetDC(0);
    try
      UpdateLayeredWindow(AnimForm.Handle, DC, nil, @FBmpSize, acDstBmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA);
    finally
      ReleaseDC(0, DC);
    end;
    FreeAndNil(AnimBmp);

    DoLayered(AForm.Handle, True, BlendValue);

    AForm.Visible := True;
    SetWindowPos(AForm.Handle, AnimForm.Handle, 0, 0, 0, 0, SWPA_ZORDER);
    if DoSetFocus then
      AForm.SetFocus;

    InAnimationProcess := False;
    while not RedrawWindow(AForm.Handle, nil, 0, RDWA_ALLNOW and not RDW_FRAME) do;
    FreeAndNil(acDstBmp);

    if AeroIsEnabled then
      Sleep(2 * acTimerInterval); // Removing of blinking in Aero

    SetWindowPos(AnimForm.Handle, AForm.Handle, 0, 0, 0, 0, SWPA_ZORDER);
    FreeAndNil(AnimForm);
  end;
end;


{$IFNDEF AC_NOSHADOW}


procedure TacShadowForm.AC_WMNCHitTest(var Message: TMessage);
begin
  message.Result := HTTRANSPARENT;
end;


constructor TacShadowForm.CreateNew(AOwner: TComponent; Dummy: integer = 0);
var
  sp: TsSkinProvider;
begin
  inherited;
  Tag := ExceptTag;
  FUserBlend := -1;
  if AOwner <> nil then begin // Skip if no form
    FOwnerForm := TForm(AOwner);
    FOwnerHandle := 0;
{$IFDEF DELPHI7UP}
    if FOwnerForm.AlphaBlend then
      FOwnerBlend := FOwnerForm.AlphaBlendValue
    else
{$ENDIF}
      FOwnerBlend := MaxByte;

    OldWndProc := FOwnerForm.WindowProc;
    FOwnerForm.WindowProc := NewWndProc;

    sp := GetSkinProvider(FOwnerForm);
    if sp <> nil then
      sd := sp.SkinData
    else
      sd := GetCommonData(FOwnerForm.Handle);

    if sd <> nil then
      sd.WndProc := NewWndProc;
  end;
  FDestroyed := False;
end;


constructor TacShadowForm.CreateNoForm(AOwnerHandle: THandle; AOldWndProc: TWndMethod; ASkinData: TsCommonData; AOwnerBlendValue: byte);
var
  NewWndProcInstance: Pointer;
begin
  FOwnerForm := nil;
  FOwnerHandle := AOwnerHandle;
  FOwnerBlend := AOwnerBlendValue;
  if Assigned(AOldWndProc) then begin
    OldWndProc := AOldWndProc;
    // FOwnerForm.WindowProc := NewWndProc;
    NewWndProcInstance := {$IFDEF DELPHI6UP}Classes.{$ENDIF}MakeObjectInstance(NewWndProc);
    SetWindowLong(AOwnerHandle, GWL_WNDPROC, LONG_PTR(NewWndProcInstance));
  end
  else
    OldWndProc := nil;

  sd := ASkinData;
  CreateNew(nil);
end;


procedure TacShadowForm.HideWnd;
begin
  Hide;
  ShowWindow(Handle, SW_HIDE);
  Locked := True;
end;


procedure TacShadowForm.SetNewPos(ALeft, ATop, aWidth, aHeight: integer; BlendValue: byte);

  procedure UpdateLayer;
  var
    DC: hdc;
    sbh, sbw: integer;
    ShSizes: TRect;
    FBmpSize: TSize;
    FBmpTopLeft: TPoint;
    FBlend: TBlendFunction;
    ShadowBmp, AlphaBmp: TBitmap;
  begin
    FUserBlend := BlendValue;
    with ShadowSize do
      FBmpSize := MkSize(aWidth + Left + Right, aHeight + Top + Bottom);

    AlphaBmp := CreateBmp32(FBmpSize);
    // Paint shadow bmp
    ShadowBmp := ShadowTemplate;
    ShSizes := ShadowSize;
    FillRect32(AlphaBmp, MkRect(AlphaBmp), 0, 0);
    sbw := (ShadowBmp.Width - 1) div 2;  // Source border width
    sbh := (ShadowBmp.Height - 1) div 2; // Source border height

    if (sd <> nil) and (sd.SkinManager <> nil) and (sd.SkinIndex >= 0) then
      with sd.SkinManager do
        if (sd.SkinIndex in [SkinCommonInfo.Sections[ssMainMenu], SkinCommonInfo.Sections[ssDialog], SkinCommonInfo.Sections[ssForm]]) then
          if (gd[sd.SkinIndex].BorderIndex >= 0) then
            with sd.CommonSkinData.ma[gd[sd.SkinIndex].BorderIndex] do begin
              inc(ShSizes.Left, WL);
              inc(ShSizes.Top, WT);
              inc(ShSizes.Right, WR);
              inc(ShSizes.Bottom, WB);
            end
          else begin
            inc(ShSizes.Left, gd[sd.SkinIndex].BorderRadius);
            inc(ShSizes.Top, gd[sd.SkinIndex].BorderRadius);
            inc(ShSizes.Right, gd[sd.SkinIndex].BorderRadius);
            inc(ShSizes.Bottom, gd[sd.SkinIndex].BorderRadius);
          end;

    PaintControlByTemplate(AlphaBmp, ShadowBmp, MkRect(AlphaBmp), MkRect(ShadowBmp), Rect(sbw, sbh, sbw, sbh), ShSizes, Rect(1, 1, 1, 1), False, False); // For internal shadows - stretch only allowed

    InitBlendData(FBlend, BlendValue);
    FBmpTopLeft := MkPoint;

    SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) or WS_EX_LAYERED);

    DC := GetDC(0);
    try
      UpdateLayeredWindow(Handle, DC, nil, @FBmpSize, AlphaBmp.Canvas.Handle, @FBmpTopLeft, clNone, @FBlend, ULW_ALPHA);
    finally
      ReleaseDC(0, DC);
    end;
    with ShadowSize do
      SetBounds(ALeft - Left, ATop - Top, FBmpSize.cx, FBmpSize.cy);

    ShowWindow(Handle, SW_SHOWNOACTIVATE);
    SetWindowPos(Handle, OwnerHandle, 0, 0, 0, 0, SWPA_SHOWZORDERONLY);
    AlphaBmp.Free;
  end;

begin
  if not FDestroyed then
    UpdateLayer;
end;


function TacShadowForm.ShadowTemplate: TBitmap;
var
  csd: TacSkinData;
begin
  if (SkinManager = nil) or not SkinManager.IsActive then begin
    if rShadowBmp = nil then
      rShadowBmp := MakeShadow($44000000, 17, 2, 8, 0, 0);

    Result := rShadowBmp;
  end
  else begin
    if (sd <> nil) and (sd.SkinIndex >= 0) then
      csd := sd.CommonSkinData
    else
      csd := SkinManager.CommonSkinData;

    Result := csd.GetPassiveShadow
  end;
end;


function TacShadowForm.SkinManager: TsSkinManager;
begin
  if sd = nil then
    Result := DefaultManager
  else
    Result := sd.SkinManager;
end;


function TacShadowForm.ShadowSize: TRect;
const
  sSize = 13;
begin
  if (SkinManager = nil) or not SkinManager.IsActive then
    Result := Rect(sSize, sSize, sSize, sSize)
  else
    if sd <> nil then
      with sd.CommonSkinData do
        Result := Rect(WndShadowSize, max(0, WndShadowSize - WndShadowOffset), WndShadowSize, max(0, WndShadowSize + WndShadowOffset))
    else
      with SkinManager.CommonSkinData do
        Result := Rect(WndShadowSize, max(0, WndShadowSize - WndShadowOffset), WndShadowSize, max(0, WndShadowSize + WndShadowOffset));
end;


procedure TacShadowForm.KillShadow;
var
  sd: TsCommonData;
begin
  if not FDestroyed then begin
    FDestroyed := True;
    if FOwnerForm <> nil then begin
      FOwnerForm.WindowProc := OldWndProc;

      sd := GetCommonData(FOwnerForm.Handle);
      if (sd <> nil) and Assigned(sd.WndProc) then
        sd.WndProc := OldWndProc;
    end;
    OldWndProc := nil;
    FOwnerForm := nil;
    HideWnd;
    Release;
  end;
end;


procedure TacShadowForm.NewWndProc(var Message: TMessage);
begin
  case message.Msg of
    SM_ALPHACMD:
      case message.WParamHi of
        AC_UPDATESHADOW: begin
            Locked := False;
            if message.LParam <> 0 then begin
              with PacLayerPos(message.LParam)^ do
                if Bounds.BHeight > 0 then
                  SetNewPos(Bounds.BLeft, Bounds.BTop, Bounds.BWidth, Bounds.BHeight, LayerBlend);
            end
            else
              UpdateShadowPos;

            Exit;
          end;
      end;

    WM_CLOSE:
      HideWnd;

    WM_SHOWWINDOW:
      if message.WParam = 0 then
        HideWnd;
  end;
  OldWndProc(message);
  case message.Msg of
    WM_WINDOWPOSCHANGED, WM_WINDOWPOSCHANGING, WM_SIZE:
      if not Locked and ((FOwnerForm = nil) and (FOwnerHandle = 0) or not IsWindowVisible(OwnerHandle)) then begin
        if HandleAllocated and IsWindowVisible(Handle) then
          HideWnd
      end
      else
        UpdateShadowPos;
  end;
end;


function TacShadowForm.OwnerHandle: THandle;
begin
  if FOwnerForm <> nil then
    if FOwnerForm.HandleAllocated then
      Result := FOwnerForm.Handle
    else
      Result := 0
  else
    Result := FOwnerHandle;
end;


procedure TacShadowForm.UpdateShadowPos;
var
  OwnerBounds: TSrcRect;
begin
  if not Locked and ((FOwnerForm <> nil) or (FOwnerHandle <> 0)) and IsWindowVisible(OwnerHandle) then begin
    GetWindowRect(OwnerHandle, TRect(OwnerBounds));
    with OwnerBounds do
      SetNewPos(SLeft, STop, WidthOf(TRect(OwnerBounds)), HeightOf(TRect(OwnerBounds)), iff(FUserBlend >= 0, FUserBlend, FOwnerBlend));
  end;
end;


function AttachShadowForm(AForm: TForm; DoShow: boolean = True): TCustomForm;
begin
  Result := TacShadowForm.CreateNew(AForm);
  TacShadowForm(Result).Locked := not DoShow;
end;


function AttachShadowWnd(aHandle: THandle; AOldWndProc: TWndMethod; ASkinData: TsCommonData; AOwnerBlendValue: byte; DoShow: boolean = True): TCustomForm;
begin
  Result := TacShadowForm.CreateNoForm(aHandle, AOldWndProc, ASkinData, AOwnerBlendValue);
  TacShadowForm(Result).Locked := not DoShow;
end;

{$ENDIF} // AC_NOSHADOW


procedure TsPopupController.ShowForm(AForm: TForm; AOwnerControl: TWinControl; ALeft: integer = -1; ATop: integer = -1; Animated: boolean = True; AnimProc: TacAnimProcEx = nil);
var
  c, h, HandlerIndex: integer;
  ctrlRect, formRect: TRect;
  bAlphaBlendValue: byte;
  bAnimated: boolean;
  sp: TsSkinProvider;
begin
  if (AForm <> nil) and (AOwnerControl <> nil) and not SkipOpen then begin
    FAnimProc := AnimProc;
    bAnimated := Animated and (DefaultManager <> nil) { and DefaultManager.Active } and DefaultManager.Effects.AllowAnimation;

    sp := GetSkinProvider(AForm);
    if sp <> nil then begin
      bAnimated := bAnimated and sp.AllowAnimation;
      sp.AllowAnimation := False;
    end;

    AForm.Visible := False;
    AForm.Position := poDesigned;
{$IFDEF DELPHI7UP}
    bAlphaBlendValue := iff(AForm.AlphaBlend, AForm.AlphaBlendValue, MaxByte);
{$ELSE}
    bAlphaBlendValue := MaxByte;
{$ENDIF}
    if AForm.BorderStyle <> bsNone then begin
      h := AForm.Height;
      if sp <> nil then
        c := SysCaptHeight(sp)
      else
        c := 32;

      AForm.BorderStyle := bsNone;
      AForm.Height := h - c;
    end;
    HandlerIndex := InitFormHandler(AForm, AOwnerControl);
{$IFNDEF AC_NOSHADOW}
    if FormHandlers[HandlerIndex].ShadowForm = nil then
      FormHandlers[HandlerIndex].ShadowForm := TacShadowForm(AttachShadowForm(AForm, False));
{$ENDIF}
    with FormHandlers[HandlerIndex] do begin
      if IgnoreCapture then
        CaptureHandle := 0
      else
        CaptureHandle := GetCapture;

      if CheckTimer = nil then begin
        CheckTimer := TacThreadedTimer.Create(nil);
        CheckTimer.Interval := 10;
        CheckTimer.OnTimer := DoTimer;
      end;
      CheckTimer.Enabled := True;
    end;
{$IFDEF DELPHI7UP}
    SetWindowLong(AForm.Handle, GWL_STYLE, GetWindowLong(AForm.Handle, GWL_STYLE) or WS_CLIPSIBLINGS or NativeInt(WS_POPUP));
{$ELSE}
    SetWindowLong(AForm.Handle, GWL_STYLE, GetWindowLong(AForm.Handle, GWL_STYLE) or Longint(WS_CLIPSIBLINGS or WS_POPUP));
{$ENDIF}
    SetWindowLong(AForm.Handle, GWL_EXSTYLE, GetWindowLong(AForm.Handle, GWL_EXSTYLE) or WS_EX_TOOLWINDOW);
    DoLayered(AForm.Handle, True);
    if ((sp = nil) or sp.AllowScale) and (DefaultManager <> nil) and not AForm.Scaled then
      DefaultManager.UpdateScale(AForm);

    GetWindowRect(AOwnerControl.Handle, ctrlRect);
    with TSrcRect(acWorkRect(ctrlRect.TopLeft)), TDstRect(formRect) do begin
      if (ALeft = -1) and (ATop = -1) then begin
        DTop := ctrlRect.Bottom; // 3;
        if AOwnerControl.BidiMode = bdRightToLeft then
          DLeft := ctrlRect.Right - AForm.Width
        else
          DLeft := ctrlRect.Left;
      end
      else begin
        DTop := ATop;
        DLeft := ALeft;
      end;
      DBottom := DTop + AForm.Height;
      DRight := DLeft + AForm.Width;
      AForm.SetBounds(DLeft, DTop, AForm.Width, AForm.Height);
      if DBottom > SBottom then begin
        DBottom := ctrlRect.Top - 1;
        DTop := DBottom - AForm.ClientHeight;
        if DTop < STop then begin
          DTop := STop;
          DBottom := DTop + AForm.Height;
        end;
        AForm.SetBounds(DLeft, DTop, AForm.Width, AForm.Height);
        AnimDirection := 1;
      end
      else
        AnimDirection := 0;

      if DRight > SRight then begin
        DRight := SRight;
        DLeft := DRight - AForm.ClientWidth;
        AForm.SetBounds(DLeft, DTop, AForm.Width, AForm.Height);
        AnimDirection := AnimDirection or 2;
      end;
    end;

    SetWindowPos(AForm.Handle, TopWndAfter, 0, 0, 0, 0, SWPA_SHOWZORDERONLY);
    AForm.Visible := True;
    if bAnimated then
      AnimShowPopup(AForm, 100, bAlphaBlendValue)
    else begin
      DoLayered(AForm.Handle, False);
      SetParentUpdated(AForm);
      if (sp <> nil) and sp.SkinData.Skinned then
        SetWindowPos(AForm.Handle, TopWndAfter, 0, 0, 0, 0, SWP_SHOWWINDOW or SWPA_SHOWZORDERONLY)
      else
        if DoSetFocus then
          ShowWindow(AForm.Handle, SW_SHOW)
        else
          ShowWindow(AForm.Handle, SW_SHOWNOACTIVATE);
{$IFNDEF AC_NOSHADOW}
      if HandlerIndex >= 0 then
        if FormHandlers[HandlerIndex].ShadowForm <> nil then
          TacShadowForm(FormHandlers[HandlerIndex].ShadowForm).Locked := False;
{$ENDIF}
    end;
    if sp <> nil then // If not animated
      sp.AllowAnimation := bAnimated;

{$IFNDEF AC_NOSHADOW}
    if not TacShadowForm(FormHandlers[HandlerIndex].ShadowForm).FDestroyed then
      TacShadowForm(FormHandlers[HandlerIndex].ShadowForm).UpdateShadowPos;
{$ENDIF}
  end;
  SkipOpen := False;
end;


procedure TsPopupController.ShowFormPos(AForm: TForm; ALeftTop: TPoint; Animated: boolean = True; AnimProc: TacAnimProcEx = nil);
var
  c, h, HandlerIndex: integer;
  ctrlRect, formRect: TRect;
  ParentForm: TCustomForm;
  bAlphaBlendValue: byte;
  bAnimated: boolean;
  sp: TsSkinProvider;
  b: boolean;
begin
  if (AForm <> nil) and not SkipOpen then begin
    FAnimProc := AnimProc;
    sp := GetSkinProvider(AForm);
    if sp <> nil then begin
      b := sp.AllowAnimation;
      sp.AllowAnimation := False;
    end
    else
      b := True;

    if Animated and (DefaultManager <> nil) { and DefaultManager.Active } and DefaultManager.Effects.AllowAnimation then
      bAnimated := True
    else
      bAnimated := False;

    AForm.Visible := False;
    AForm.Position := poDesigned;
{$IFDEF DELPHI7UP}
    bAlphaBlendValue := iff(AForm.AlphaBlend, AForm.AlphaBlendValue, MaxByte);
{$ELSE}
    bAlphaBlendValue := MaxByte;
{$ENDIF}
    if AForm.BorderStyle <> bsNone then begin
      h := AForm.Height;
      if sp <> nil then
        c := SysCaptHeight(sp)
      else
        c := 32;

      AForm.BorderStyle := bsNone;
      AForm.Height := h - c;
    end;
    HandlerIndex := InitFormHandler(AForm, nil);
    with FormHandlers[HandlerIndex] do begin
      if IgnoreCapture then
        CaptureHandle := 0
      else
        CaptureHandle := GetCapture;

      if CheckTimer = nil then begin
        CheckTimer := TacThreadedTimer.Create(nil);
        CheckTimer.Interval := 100;
        CheckTimer.OnTimer := DoTimer;
      end;
      CheckTimer.Enabled := True;
    end;
{$IFDEF DELPHI7UP}
    SetWindowLong(AForm.Handle, GWL_STYLE, GetWindowLong(AForm.Handle, GWL_STYLE) or WS_CLIPSIBLINGS or NativeInt(WS_POPUP));
{$ELSE}
    SetWindowLong(AForm.Handle, GWL_STYLE, GetWindowLong(AForm.Handle, GWL_STYLE) or Longint(WS_CLIPSIBLINGS or WS_POPUP));
{$ENDIF}
    SetWindowLong(AForm.Handle, GWL_EXSTYLE, GetWindowLong(AForm.Handle, GWL_EXSTYLE) or WS_EX_TOOLWINDOW);
    DoLayered(AForm.Handle, True);
    if DefaultManager <> nil then
      DefaultManager.UpdateScale(AForm);

    ctrlRect.TopLeft := ALeftTop;
    ctrlRect.BottomRight := ALeftTop;
    with TSrcRect(acWorkRect(ctrlRect.TopLeft)), TDstRect(formRect) do begin
      DTop := ctrlRect.Bottom - 2;
      DLeft := ctrlRect.Left;
      DBottom := DTop + AForm.Height;
      DRight := DLeft + AForm.Width;
      AForm.SetBounds(DLeft, DTop, AForm.Width, AForm.Height);
      if DBottom > SBottom then begin
        DBottom := ctrlRect.Top - 1;
        DTop := DBottom - AForm.ClientHeight;
        if DTop < STop then begin
          DTop := STop;
          DBottom := DTop + AForm.Height;
        end;
        AForm.SetBounds(DLeft, DTop, AForm.Width, AForm.Height);
        AnimDirection := 1;
      end
      else
        AnimDirection := 0;

      if DRight > SRight then begin
        DRight := SRight - 1;
        DLeft := DRight - AForm.ClientWidth;
        AForm.SetBounds(DLeft, DTop, AForm.Width, AForm.Height);
        AnimDirection := AnimDirection or 2;
      end;
    end;

    ParentForm := FormHandlers[HandlerIndex].ParentForm;
    if (ParentForm is TForm) and (TForm(ParentForm).FormStyle = fsStayOnTop) then
      AForm.FormStyle := fsStayOnTop;

    SetWindowPos(AForm.Handle, TopWndAfter, 0, 0, 0, 0, SWPA_SHOWZORDERONLY);

    AForm.Visible := True;
    if bAnimated then
      AnimShowPopup(AForm, 100, bAlphaBlendValue)
    else begin
      DoLayered(AForm.Handle, False);
      SetParentUpdated(AForm);
      if (sp <> nil) and sp.SkinData.Skinned then
        SetWindowPos(AForm.Handle, TopWndAfter, 0, 0, 0, 0, SWP_SHOWWINDOW or SWPA_SHOWZORDERONLY)
      else
        if DoSetFocus then
          ShowWindow(AForm.Handle, SW_SHOW)
        else
          ShowWindow(AForm.Handle, SW_SHOWNOACTIVATE);
{$IFNDEF AC_NOSHADOW}
      if HandlerIndex >= 0 then
        if FormHandlers[HandlerIndex].ShadowForm <> nil then
          TacShadowForm(FormHandlers[HandlerIndex].ShadowForm).Locked := False;
{$ENDIF}
    end;

{$IFNDEF AC_NOSHADOW}
    if not TacShadowForm(FormHandlers[HandlerIndex].ShadowForm).FDestroyed then
      TacShadowForm(FormHandlers[HandlerIndex].ShadowForm).UpdateShadowPos;
{$ENDIF}
    if sp <> nil then
      sp.AllowAnimation := b;
  end;
  SkipOpen := False;
end;


procedure TacFormHandler.UpdateRgnBmp(aBmp: TBitmap);
var
  Rgn: hrgn;
  i: integer;
  sd: TsCommonData;
begin
  ClientCtrl := nil;
  for i := 0 to PopupForm.ControlCount - 1 do
    if (PopupForm.Controls[i].Align = alClient) and (PopupForm.Controls[i] is TWinControl) then begin
      ClientCtrl := TWinControl(PopupForm.Controls[i]);
      Break;
    end;

  if (ClientCtrl <> nil) and (ClientCtrl.Width = aBmp.Width) and (ClientCtrl.Height = aBmp.Height) then begin
    sd := TsCommonData(ClientCtrl.Perform(SM_ALPHACMD, AC_GETSKINDATA_HI, 0));
    if (sd <> nil) and sd.Skinned then begin
      Rgn := GetTransCornersRgn(sd.SkinIndex, aBmp, sd.CommonSkinData);
      if Rgn <> 0 then
        SetWindowRgn(PopupForm.Handle, Rgn, False);
    end;
  end;
end;


type
  TAccessComboBox = class(TsCustomComboBox);
  TAccessComboBoxEx = class(TsCustomComboBoxEx);


function TacFormHandler.CloseForm(CallHideCloseProc: boolean = True): TCloseAction;
begin
  CheckTimer.Enabled := False;
  if not Closed and not ClosingForbidden and (PopupForm <> nil) and PopupForm.CloseQuery then begin
    Closed := True;
    Result := caHide;
    if Assigned(OldOnClose) then
      OldOnClose(PopupForm, Result);

    if Result = caNone then
      Closed := False
    else begin
      if CallHideCloseProc then
        case Result of
          caHide:
            PopupForm.Hide;

          caFree: begin
              PopupForm.Close;
              if not(csDestroying in PopupForm.ComponentState) then
                FreeAndNil(PopupForm);
            end;
        end;

      if PopupCtrl is TsCustomComboBox then
        with TAccessComboBox(PopupCtrl) do begin
          FDroppedDown := False;
          SkinData.Invalidate;
        end
      else
        if PopupCtrl <> nil then
          PopupCtrl.Perform(SM_ALPHACMD, AC_POPUPCLOSED shl 16, LParam(PopupForm));

      if not Application.Active and (ParentForm <> nil) then
        SendMessage(ParentForm.Handle, WM_NCACTIVATE, 0, 0); // Update caption as inactive
    end;
    UnInitControls;
  end;
end;


constructor TacFormHandler.Create(AForm: TForm; ACtrl: TWinControl);
begin
  inherited Create;
  Closed := False;
  ClosingForbidden := False;
  PopupCtrl := nil;
{$IFNDEF AC_NOSHADOW}
  ShadowForm := nil;
{$ENDIF}
  InitControls(AForm, ACtrl);
end;


destructor TacFormHandler.Destroy;
begin
  Controller := nil;
  FreeAndNil(CheckTimer);
  inherited;
end;


{$IFNDEF AC_NOHANDLECTRL}


procedure TacFormHandler.DoCtrlProc(var Message: TMessage);
var
  i: integer;
  // wRect: TRect;
  // TmpCtrl: TWinControl;
begin
{$IFDEF LOGGED}
  // AddToLog(Message);
{$ENDIF}
  case message.Msg of
    CN_KEYDOWN:
      if Closed or (message.WParam <> 27) then // Send to the parent form
        CtrlWndProc(message);

    WM_DESTROY: begin
{$IFNDEF AC_NOSHADOW}
        if ShadowForm <> nil then begin
          TacShadowForm(ShadowForm).KillShadow;
          ShadowForm := nil;
        end;
{$ENDIF}
{$IFNDEF AC_NOHANDLEFORM}
        FormWndProc(message);
{$ENDIF}
        UnInitControls;
        for i := 0 to Length(Controller.FormHandlers) do
          if Controller.FormHandlers[i] = Self then
            Controller.FormHandlers[i] := nil;
      end;

    WM_MOUSEWHEEL:
      PopupForm.DefaultHandler(message);

    CM_FOCUSCHANGED, CM_CANCELMODE, WM_RBUTTONDOWN, WM_LBUTTONDOWN, WM_LBUTTONDBLCLK: begin
        if not Closed and (PopupForm <> nil) then begin
{$IFNDEF ALITE}
          if (PopupCtrl is TsDateEdit) and Assigned(TsDateEdit(PopupCtrl).OnCloseUp) then
            TsDateEdit(PopupCtrl).OnCloseUp(PopupCtrl);
{$ENDIF}
          case message.Msg of
            WM_LBUTTONDBLCLK, WM_LBUTTONDOWN:
              TrySendMessage(PopupCtrl.Handle, SM_ALPHACMD, WParam(AC_DOSKIPCLICK shl 16), 0);
          end;
          CloseForm;
          // Avoiding problems with parent form activating Application.ProcessMessages;
        end;
        CtrlWndProc(message);
      end

  else
    CtrlWndProc(message);
  end;
end;
{$ENDIF} // AC_NOHANDLECTRL


procedure TacFormHandler.DoTimer(Sender: TObject);
var
  CaptHandle: THandle;
begin
  if (PopupForm <> nil) and not Controller.MousePressed then
    if not Application.Active then
      CloseForm
    else begin
      CaptHandle := GetCapture;
      if (SysPopupCount = 0) and (CaptHandle <> 0) and (CaptureHandle <> CaptHandle) then
        if not ContainsWnd(CaptHandle, PopupForm.Handle) and ((PopupCtrl = nil) or not ContainsWnd(CaptHandle, PopupCtrl.Handle)) then
          if (Controller <> nil) and not Controller.HasChild(PopupForm) then
            CloseForm;
    end;
end;


{$IFNDEF AC_NOHANDLEFORM}


procedure TacFormHandler.DoWndProc(var Message: TMessage);
begin
{$IFDEF LOGGED}
  AddToLog(message);
{$ENDIF}
  case message.Msg of
    SM_ALPHACMD:
      case message.WParamHi of
        AC_REMOVESKIN: begin
            if Assigned(FormWndProc) then
              FormWndProc(message);

            UnInitControls;
            Exit;
          end
      else
        FormWndProc(message);
      end;

    WM_DESTROY: begin
        if Assigned(FormWndProc) then
          FormWndProc(message);

        UnInitControls;
        Exit;
      end;

    WM_WINDOWPOSCHANGING: begin
        FormWndProc(message);
        if (TWMWindowPosMsg(message).WindowPos.Flags = SWP_NOACTIVATE or SWP_NOMOVE or SWP_NOSIZE) and (TWMWindowPosMsg(message).WindowPos.hwnd <> PopupForm.Handle) then
          CloseForm
        else
          if (TWMWindowPosMsg(message).WindowPos.Flags and SWP_HIDEWINDOW = SWP_HIDEWINDOW) and (PopupForm <> nil) and (TWMWindowPosMsg(message).WindowPos.hwnd = PopupForm.Handle) then
            CloseForm;
      end;

    WM_MOUSEACTIVATE: begin
        FormWndProc(message);
        message.Result := MA_NOACTIVATE
      end

  else
    if Assigned(FormWndProc) then
      FormWndProc(message);
  end;
end;
{$ENDIF} // AC_NOHANDLEFORM


procedure TacFormHandler.InitControls(AForm: TForm; ACtrl: TWinControl);
{$IFNDEF AC_NOHANDLEFORM}
var
  sd: TsCommonData;
  sp: TsSkinProvider;
{$ENDIF}
begin
  Closed := False;
  PopupForm := AForm;
  PopupCtrl := ACtrl;

  if ACtrl <> nil then
    ParentForm := TForm(GetParentForm(ACtrl))
  else
    ParentForm := nil;

  if (ParentForm is TForm) and (TForm(ParentForm).FormStyle = fsStayOnTop) then
    AForm.FormStyle := fsStayOnTop;

{$IFNDEF AC_NOHANDLEFORM}
  sp := GetSkinProvider(AForm);
  if sp <> nil then
    FreeAndNil(sp.ListSW);

  FormWndProc := AForm.WindowProc;
  AForm.WindowProc := DoWndProc;
  sd := GetCommonData(AForm.Handle);
  if sd <> nil then
    sd.WndProc := DoWndProc;
{$ENDIF}
  if ACtrl <> nil then begin
{$IFNDEF AC_NOHANDLECTRL}
    CtrlWndProc := ACtrl.WindowProc;
    PopupCtrl.WindowProc := DoCtrlProc;
{$ENDIF}
  end;

  Initialized := True;
end;


procedure TacFormHandler.UnInitControls;
{$IFNDEF AC_NOHANDLEFORM}
var
  sd: TsCommonData;
  sw: TacScrollWnd;
{$ENDIF}
begin
  if Initialized then begin
{$IFNDEF AC_NOHANDLEFORM}
    if PopupForm <> nil then begin
      if not(csDestroying in PopupForm.ComponentState) then begin
        sw := Ac_GetScrollWndFromHwnd(PopupForm.Handle);
        if sw <> nil then
          UninitializeACWnd(PopupForm.Handle, False, False, TacMainWnd(sw));
  {$IFNDEF AC_NOSHADOW}
        if ShadowForm <> nil then begin
          TacShadowForm(ShadowForm).KillShadow;
          ShadowForm := nil;
        end;
  {$ENDIF}
        sd := GetCommonData(PopupForm.Handle);
        if sd <> nil then
          sd.WndProc := nil;
      end;
      PopupForm.WindowProc := FormWndProc;
    end;
    FormWndProc := nil;
{$ENDIF} // AC_NOHANDLEFORM
{$IFNDEF AC_NOHANDLECTRL}
    if PopupCtrl <> nil then
      PopupCtrl.WindowProc := CtrlWndProc;
{$ENDIF}
    if PopupForm <> nil then begin
      PopupForm.OnDeactivate := nil;
      if Assigned(OldOnClose) then
        PopupForm.OnClose := OldOnClose
      else
        PopupForm.OnClose := nil;
    end;
    PopupForm := nil;
    // PopupCtrl := nil; Used later for a msg sending
    Initialized := False;
  end;
end;


function TsPopupController.InitFormHandler(AForm: TForm; Ctrl: TWinControl): integer;
begin
  Result := GetFormIndex(AForm);
  if Result < 0 then begin
    Result := Length(FormHandlers);
    SetLength(FormHandlers, Result + 1);
    FormHandlers[Result] := TacFormHandler.Create(AForm, Ctrl);
    FormHandlers[Result].Controller := Self;

    AForm.OnDeactivate := DoDeactivate;

    if Assigned(AForm.OnClose) and not Assigned(FormHandlers[Result].OldOnClose) then
      FormHandlers[Result].OldOnClose := AForm.OnClose;

    AForm.OnClose := DoClose;
  end;
  FormHandlers[Result].Closed := False;
{$IFNDEF AC_NOSHADOW}
  if FormHandlers[Result].ShadowForm = nil then
    FormHandlers[Result].ShadowForm := TacShadowForm(AttachShadowForm(AForm, False));
{$ENDIF}
end;


initialization

finalization
  FreeAndNil(rShadowBmp);
  FreeAndNil(acIntController);

end.
