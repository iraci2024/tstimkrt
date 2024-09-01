unit sVclUtils;
{$I sDefs.inc}
//{$DEFINE LOGGED}
//{$DEFINE ACDEBUG}
//{$DEFINE VIDEOCAPT} // Allow screen video making (tmp)

{$IFDEF LOGGED}
  {$DEFINE ANIMDEBUG}
  {$WARNINGS OFF}
{$ENDIF}

interface

uses
  Classes, Controls, SysUtils, StdCtrls, windows, Graphics, Forms, Messages, extctrls, comctrls,
  {$IFDEF FPC} lcltype, {$ENDIF}
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  {$IFDEF USEDB} db, dbgrids, dbCtrls, {$ENDIF}
  {$IFDEF DELPHI7UP} Themes, UxTheme, {$ENDIF}
  {$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  {$IFDEF TNTUNICODE} TntControls, {$ENDIF}
  acntTypes, sSkinProvider, acSBUtils, sConst, acntUtils, sCommonData, acDials, acThdTimer, sGraphUtils;


type
  TacIterProc = procedure(Ctrl: TControl; Data: integer);
  TacIterFun = function(Ctrl: TControl; var Data: integer): boolean; // Stop execution if True returned

const
  AlignToInt: array [TAlignment] of Longint = (DT_LEFT, DT_RIGHT, DT_CENTER); // TAlignment = (taLeftJustify, taRightJustify, taCenter)
  BidiAlign: array [boolean {if RTL}, TAlignment] of TAlignment = ((taLeftJustify, taRightJustify, taCenter), (taRightJustify, taLeftJustify, taCenter));


function acColorToRGB(Value: TColor; SkinManager: TObject = nil): TColor; // Converting of special colors (from sConst.pas) to colors from a current skin palette
function acMousePos(Ctrl: TControl = nil): TPoint;
function acMouseInControl(Control: TControl): boolean;
function acMouseInControlDC(Handle: THandle): boolean;
function CtrlUnderMouse: TControl;

function acGetFormImage(aForm: TForm): TBitmap;

function ContainsWnd(AHandle, AParent: THandle): boolean;
function GetRootParent(Handle: THandle): THandle;
function LeftToRight(const Control: TControl; const NormalAlignment: boolean = True): boolean;
procedure AddToAdapter(const Ctrl: TWinControl);
procedure BroadCastMsg(const Ctrl: hwnd; const Message: TMessage);
procedure IterateControls(Owner: TWinControl; Data: integer; CallBack: TacIterProc); overload;
function IterateControls(Owner: TWinControl; Data: integer; CallBack: TacIterFun): boolean; overload;
procedure EnableCtrl(Ctrl: TControl; Enabled: integer);

procedure SaveGraphCtrls(Wnd: TWinControl; var cArray: TacCtrlArray);
procedure RestoreGraphCtrls(Wnd: TWinControl; var cArray: TacCtrlArray);

procedure UpdateGlassMode(Ctrl: TControl; ChildInclude: boolean = True);

procedure BufferedPrintClient(Handle: HWND; Rect: TRect; BGColor: TColor = clWhite);
procedure DoBufferedOut(Canvas: TCanvas; Bmp: TBitmap; R: TRect);

function GetTransMode(ASkinData: TsCommonData; ASkinIndex: integer = -1; AState: integer = 0): Cardinal;

procedure PaintChildCtrls(Ctrl: TWinControl; Bmp: TBitmap);
procedure SkinPaintTo(const Bmp: TBitmap; const Ctrl: TControl; const Left: integer = 0; const Top: integer = 0; const SkinProvider: TComponent = nil; RootCtrl: boolean = False);
procedure StdPaintTo(const Bmp: TBitmap; const Ctrl: TWinControl); overload;
procedure StdPaintTo(const Bmp: TBitmap; Ctrl: TControl); overload;
function AlignmentFromInt(Value: UINT): TAlignment;
function VertAlignmentFromInt(Value: UINT): TVerticalAlignment;

procedure AnimShowDlg(ListSW: TacDialogWnd; wTime: word = 0; MaxTransparency: integer = MaxByte; AnimType: TacAnimType = atAero);
procedure AnimShowForm(sp: TsSkinProvider;  wTime: word = 0; MaxTransparency: integer = MaxByte; AnimType: TacAnimType = atAero);

procedure PrepareForAnimation(const Ctrl: TWinControl; AnimType: TacAnimTypeCtrl = atcFade);
procedure AnimShowControl(Ctrl: TWinControl; wTime: word = 0; MaxTransparency: integer = MaxByte; AnimType: TacAnimTypeCtrl = atcFade);

procedure AnimHideForm(SkinProvider: TObject; Time: integer{ = 0}; Mode: TacAnimType{ = atAero});
procedure PrintDlgClient(ListSW: TacDialogWnd; acDstBmp: TBitmap; CopyScreen: boolean = False);
procedure AnimHideDlg(ListSW: TacDialogWnd);

function DoLayered(FormHandle: Hwnd; Layered: boolean; AlphaValue: byte = 1): boolean;

function acShowHintWnd(const HintText: string; Pos: TPoint): {$IFDEF TNTUNICODE}TTntCustomHintWindow{$ELSE}THintWindow{$ENDIF};
{$IFNDEF ALITE}
procedure acHideHintWnd(var Wnd: {$IFDEF TNTUNICODE}TTntCustomHintWindow{$ELSE}THintWindow{$ENDIF});
{$ENDIF}
function acWorkRect(Form: TForm): TRect; overload;
function acWorkRect(Coord: TPoint): TRect; overload;
function GetCtrlScreenBounds(Ctrl: TControl): TRect;
function GetClientPos(Handle: THandle): TPoint;
function TopWndAfter: HWND;

function DefaultPPI: integer;
function GetControlPPI(Ctrl: TControl): integer;
function GetWndPPI(Handle: THandle): integer;
procedure EnableNCDpiScaling(WndHandle: THandle);

procedure SetParentUpdated(const wc: TWinControl); overload;
procedure SetParentUpdated(const pHwnd: hwnd); overload;
procedure ChangeControlColors(AControl: TControl; AFontColor, AColor: TColor); // clNone will reset a color to default
function GetControlColor(const Control: TControl): TColor; overload;
function GetControlColor(const Handle: THandle): TColor; overload;
function GetControlFontColor(const Control: TControl; SkinManager: TObject): TColor;
function IsCustomFont(Ctrl: TControl; AFont: TFont; SaveColor: boolean = True): boolean;
function AllEditSelected(Ctrl: TCustomEdit): Boolean;

procedure PaintControls(DC: HDC; OwnerControl: TWinControl; ChangeCache: boolean; Offset: TPoint; {AHandle: THandle = 0; }CheckVisible: boolean = True);
procedure PaintParentBG(AControl: TControl; ABitmap: TBitmap);

procedure SetRedraw(Handle: THandle;       Value: integer = 0); overload;
procedure SetRedraw(Ctrl: TGraphicControl; Value: integer = 0); overload;

function SendAMessage(const Handle: hwnd; const Cmd: Integer; LParam: LPARAM = 0): LRESULT; overload; // may be removed later
function SendAMessage(const Control: TControl; const Cmd: Integer; LParam: LPARAM = 0): LRESULT; overload;
function TrySendMessage(const Control: TControl; var Message: TMessage): LRESULT; overload;
function TrySendMessage(const Control: TControl; Msg: Cardinal; WParam: WPARAM; LParam: LPARAM = 0): LRESULT; overload;
function TrySendMessage(AHandle: THandle; Msg: Cardinal; WParam: WPARAM; LParam: LPARAM = 0): LRESULT; overload;
function GetBoolMsg(const Control: TWinControl; const Cmd: Cardinal): boolean; overload;
function GetBoolMsg(const CtrlHandle: hwnd; const Cmd: Cardinal): boolean; overload;
function ControlIsReady(const Control: TControl): boolean;
function GetOwnerForm (const Component: TComponent): TCustomForm;
function GetOwnerFrame(const Component: TComponent): TCustomFrame;
procedure SetControlsEnabled(Parent: TWinControl; Value: boolean; Recursion: boolean = False);
function GetStringFlags(const Control: TControl; const al: TAlignment): Cardinal;
procedure RepaintsControls(const Owner: TWinControl);
procedure AlphaBroadCast(const Control: TWinControl; var Message); overload;
procedure AlphaBroadCast(const Handle: hwnd; var Message); overload;
function acClientRect(const Handle: hwnd): TRect;
function acMouseInWnd(const Handle: hwnd): TPoint; overload;
function acMouseInWnd(const Handle: hwnd; X, Y: integer): TPoint; overload;
function GetAlignShift(const Ctrl: TWinControl; const Align: TAlign; const GraphCtrlsToo: boolean = False): integer;
function GetParentFormHandle(const CtrlHandle: hwnd): hwnd;
function TrySetSkinSection(const Control: TControl; const SectionName: string): boolean;
function GetWndClassName(const Hwnd: THandle): string;
procedure SetFormBlendValue(FormHandle: THandle; Bmp: TBitmap; Value: integer; NewPos: PPoint = nil);
function GetShiftState: TShiftState;
function GetWndText(hwnd: THandle): WideString;
function GetPlainText(HtmlText: acString; ConvertBreakLine: boolean = False): acString;

procedure ResetLastError;
procedure CheckLastError;
procedure ReflectControls(ParentWnd: TWinControl; Recursion: boolean);

function CtrlIsOverlapped(Ctrl: TControl): boolean;
function CtrlDCIsVisible(Ctrl: TControl): boolean;
function RepaintOpaque(Ctrl: TControl; var Data: integer): boolean;

type
  TOutputWindow = class(TCustomControl)
  private
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMNCPaint(var Message: TWmEraseBkgnd); message WM_NCPAINT;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Canvas;
  end;

  TacAccessDialogWnd = class(TacDialogWnd);

  TacHideTimer = class(TacThreadedTimer)
  protected
    Dlg: TacAccessDialogWnd;
    ParentWnd: THandle;
    DC: hdc;
    FBmpSize: TSize;
    FBlend: TBlendFunction;
    Trans, p, dx, dy, l, t, r, b: real;
    i, StartBlendValue, StepCount: integer;
    AnimType: TacAnimType;
    EventCalled: boolean;
    SrcBmp, DstBmp: TBitmap;
    acwPopupDiv: real;
    procedure Anim_Init;
    procedure CallEvent;
    procedure Anim_DoNext;
    function Anim_GoToNext: boolean;
    procedure OnTimerProc(Sender: TObject);
  public
    Form: TacGlowForm;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;


var
  ow: TOutPutwindow = nil;
  InAnimationProcess: boolean = False;
  acGraphPainting: boolean = False;
  uxthemeLib: HModule = 0;
  ac_SetWindowTheme: function(hwnd: HWND; pszSubAppName: LPCWSTR; pszSubIdList: LPCWSTR): HRESULT; stdcall;
{$IFNDEF FLUENTCOMP}
  BeginBufferedPaint: function (hdcTarget: HDC; const prcTarget: TRect; dwFormat: DWORD; pPaintParams: PBPPaintParams; var phdc: HDC): HPAINTBUFFER; stdcall;
  EndBufferedPaint: function (hBufferedPaint: HPAINTBUFFER; fUpdateTarget: BOOL): HResult; stdcall;
  BufferedPaintMakeOpaque: function(hBufferedPaint: HPAINTBUFFER; const prc: TRect): HResult; stdcall;
{$ENDIF}
  acHideTimer: TacHideTimer = nil;

{$IFDEF ACDEBUG}
  acDebugAnimBmp: TBitmap = nil;
{$ENDIF}

{$IFDEF DELPHI_XE2}
  acThemeServices: TCustomStyleServices;
{$ELSE}
  {$IFDEF DELPHI7UP}
    acThemeServices: TThemeServices;
  {$ENDIF}
{$ENDIF}


{$IFDEF DELPHI7UP}
function acThemesEnabled: boolean;
{$ENDIF}

{$IFNDEF NOTFORHELP}
procedure UpdateLayerVisibility(SkinData: TsCommonData; Handle: THandle);
{$ENDIF}


implementation

uses
  math, Buttons, MultiMon,
{$IFNDEF ALITE}
  sStatusBar,
{$ENDIF}
  sMemo, acAnimation, sSkinProps, sDefaults, acGPUtils, sStyleSimply, sMessages, sSkinManager, sThirdParty, sAlphaGraph,
  acPopupController, acGlow{$IFNDEF ALITE}, acAlphaHints{$ENDIF};


function RectIsOverlapped(aWnd: THandle; const aRect: TRect): boolean;
var
  R: TRect;
  NextWnd: THandle;
begin
  NextWnd := GetNextWindow(aWnd, GW_HWNDPREV);
  if NextWnd <> 0 then begin
    if IsWindowVisible(NextWnd) and (GetWindowLong(NextWnd, GWL_EXSTYLE) and (WS_EX_TRANSPARENT or WS_EX_LAYERED) = 0) then begin
      GetWindowRect(NextWnd, R);
      if RectInRect(R, aRect) then
        Result := True
      else
        Result := RectIsOverlapped(NextWnd, aRect);
    end
    else
      Result := RectIsOverlapped(NextWnd, aRect);
  end
  else
    Result := False;
end;


function CtrlIsOverlapped(Ctrl: TControl): boolean;
var
  R: TRect;
  f: TCustomForm;
begin
{$IFNDEF VIDEOCAPT}
  if Ctrl is TWinControl then begin
    GetWindowRect(TWinControl(Ctrl).Handle, R);
    f := GetParentForm(TWinControl(Ctrl));
  end
  else begin
    R := Ctrl.BoundsRect;
    with Ctrl.Parent.ClientToScreen(MkPoint) do
      OffsetRect(R, X, Y);

    f := GetParentForm(Ctrl.Parent);
  end;
  if f <> nil then
    Result := RectIsOverlapped(f.Handle, R)
  else
{$ENDIF}
    Result := False;
end;


function CtrlDCIsVisible(Ctrl: TControl): boolean;
var
  DC: hdc;
  R: TRect;
begin
  if not (Ctrl is TWinControl) then
    Ctrl := Ctrl.Parent;

  if Ctrl is TWinControl then begin
    DC := GetWindowDC(TWinControl(Ctrl).Handle); // Desc 13
    try
      Result := not (GetClipBox(DC, R) in [ERROR, NULLREGION]);
    finally
      ReleaseDC(TWinControl(Ctrl).Handle, DC);
    end;
    Exit;
  end;
  Result := False;
end;


function acMousePos(Ctrl: TControl = nil): TPoint;
var
  p: TPoint;
  R: TRect;
begin
  if Ctrl = nil then begin
    if not GetCursorPos(Result) then
      Result := MkPoint;
  end
  else begin
    R := GetCtrlScreenBounds(Ctrl);
    p := acMousePos;
    Result.X := p.X - R.Left;
    Result.Y := p.Y - R.Top;
  end;
end;


function acColorToRGB(Value: TColor; SkinManager: TObject = nil): TColor;
var
  sm: TsSkinManager;
begin
  if Value < 0 then begin
    if SkinManager = nil then
      sm := DefaultManager
    else
      sm := TsSkinManager(SkinManager);

    if (sm <> nil) and (sm.IsActive or (msSkinChanging in sm.ManagerState)) then
      case Value of
        slMainColor:          Result := sm.Palette[pcMainColor];

        slBtnRed:             Result := sm.Palette[pcBtnToneRed];
        slBtnRedActive:       Result := sm.Palette[pcBtnToneRedActive];
        slBtnGreen:           Result := sm.Palette[pcBtnToneGreen];
        slBtnGreenActive:     Result := sm.Palette[pcBtnToneGreenActive];
        slBtnBlue:            Result := sm.Palette[pcBtnToneBlue];
        slBtnBlueActive:      Result := sm.Palette[pcBtnToneBlueActive];
        slBtnYellow:          Result := sm.Palette[pcBtnToneYellow];
        slBtnYellowActive:    Result := sm.Palette[pcBtnToneYellowActive];

        slBtnRedText:         Result := sm.Palette[pcBtnRedText];
        slBtnRedTextActive:   Result := sm.Palette[pcBtnRedTextActive];
        slBtnGreenText:       Result := sm.Palette[pcBtnGreenText];
        slBtnGreenTextActive: Result := sm.Palette[pcBtnGreenTextActive];
        slBtnBlueText:        Result := sm.Palette[pcBtnBlueText];
        slBtnBlueTextActive:  Result := sm.Palette[pcBtnBlueTextActive];
        slBtnYellowText:      Result := sm.Palette[pcBtnYellowText];
        slBtnYellowTextActive:Result := sm.Palette[pcBtnYellowTextActive];

        slEditGreen:          Result := sm.Palette[pcEditBG_Ok];
        slEditYellow:         Result := sm.Palette[pcEditBG_Warning];
        slEditRed:            Result := sm.Palette[pcEditBG_Alert];

        slEditGreenText:      Result := sm.Palette[pcEditText_Ok];
        slEditYellowText:     Result := sm.Palette[pcEditText_Warning];
        slEditRedText:        Result := sm.Palette[pcEditText_Alert]
        else
//          if sm.Options.ChangeSysColors then
            case Value of
              // Some std colors
              clMenu,
      {$IFDEF DELPHI7UP}
              clMenuBar,
      {$ENDIF}
              clScrollBar,
              clBackground,
              clAppWorkSpace,
              clBtnFace:            Result := sm.Palette[pcMainColor];

              clMenuText,
              clBtnText:            Result := sm.Palette[pcLabelText];

              clWindow:             Result := sm.Palette[pcEditBG];
              clWindowText:         Result := sm.Palette[pcEditText];

              clActiveBorder,
              clInactiveBorder,
              clWindowFrame:        Result := sm.Palette[pcBorder];

      {$IFDEF DELPHI7UP}
              clGradientActiveCaption,
      {$ENDIF}
              clActiveCaption:
                if sm.SkinCommonInfo.Sections[ssFormTitle] >= 0 then
                  Result := sm.CommonSkinData.gd[sm.SkinCommonInfo.Sections[ssFormTitle]].Props[1].Color
                else
                  Result := sm.Palette[pcMainColor];

      {$IFDEF DELPHI7UP}
              clGradientInactiveCaption,
      {$ENDIF}
              clInactiveCaption:
                if sm.SkinCommonInfo.Sections[ssFormTitle] >= 0 then
                  Result := sm.CommonSkinData.gd[sm.SkinCommonInfo.Sections[ssFormTitle]].Props[0].Color
                else
                  Result := sm.Palette[pcMainColor];

              clCaptionText:
                if sm.SkinCommonInfo.Sections[ssFormTitle] >= 0 then
                  Result := sm.CommonSkinData.gd[sm.SkinCommonInfo.Sections[ssFormTitle]].Props[1].FontColor.Color
                else
                  Result := sm.Palette[pcLabelText];

              clInactiveCaptionText:
                if sm.SkinCommonInfo.Sections[ssFormTitle] >= 0 then
                  Result := sm.CommonSkinData.gd[sm.SkinCommonInfo.Sections[ssFormTitle]].Props[0].FontColor.Color
                else
                  Result := sm.Palette[pcLabelText];

      {$IFDEF DELPHI7UP}
              clMenuHighlight,
      {$ENDIF}
              clHighlight: Result := sm.Palette[pcSelectionBG_Focused];

              clHighlightText: Result := sm.Palette[pcSelectionText_Focused];
      {$IFDEF DELPHI7UP}
              clHotLight:  Result := sm.Palette[pcWebTextHot];
      {$ENDIF}

              cl3DDkShadow:   Result := BlendColors(sm.Palette[pcMainColor], clBlack, 210);
              clBtnShadow:    Result := BlendColors(sm.Palette[pcMainColor], clBlack, 230);
              clGrayText:     Result := BlendColors(sm.Palette[pcMainColor], sm.Palette[pcLabelText], 127);
              clBtnHighlight: Result := BlendColors(sm.Palette[pcMainColor], sm.Palette[pcSelectionBG_Focused], 127);
              cl3DLight:      Result := BlendColors(sm.Palette[pcMainColor], clWhite, 180);

              clInfoText:
                if sm.SkinCommonInfo.Sections[ssHint] >= 0 then
                  Result := sm.CommonSkinData.gd[sm.SkinCommonInfo.Sections[ssHint]].Props[0].FontColor.Color
                else
                  Result := sm.Palette[pcEditText];

              clInfoBk:
                if sm.SkinCommonInfo.Sections[ssHint] >= 0 then
                  Result := sm.CommonSkinData.gd[sm.SkinCommonInfo.Sections[ssHint]].Props[0].Color
                else
                  Result := sm.Palette[pcEditBG];

              else
                Result := ColorToRGB(Value);
            end
//          else
//            Result := ColorToRGB(Value);
      end
    else
      if $FFFFFF and Cardinal(Value) > 100 then
        Result := $FFFFFF and Value
      else
        Result := ColorToRGB(Value);
  end
  else
    Result := ColorToRGB(Value);
end;


function GetRootParent(Handle: THandle): THandle;
begin
  Result := GetParent(Handle);
  if Result <> 0 then
    Result := GetRootParent(Result)
  else
    Result := Handle;
end;


function acMouseInControl(Control: TControl): boolean;
var
  DC: hdc;
  R: TRect;
  p: TPoint;
  Form: TCustomForm;
  VclWnd: THandle;
begin
  if Control.Visible then begin
    if Control is TWinControl then begin
      GetWindowRect(TWinControl(Control).Handle, R);
      p := acMousePos;
      Result := PtInRect(R, p);
    end
    else
      if (Control.Parent <> nil) and Control.Parent.HandleAllocated then begin
        Form := GetParentForm(Control);
{$IFDEF D2010}
        if (Form <> nil) and (Application.ModalLevel > 0) and (Form.Handle <> Application.ActiveFormHandle) and (GetWindowLong(Form.Handle, GWL_STYLE) and WS_POPUP = 0) then // If parent form is under modal form
{$ELSE}
        if (Form <> nil) and not IsWindowEnabled(Form.Handle) then
{$ENDIF}
          Result := False
        else
          if (Form = nil) and not IsWindowEnabled(GetParentFormHandle(Control.Parent.Handle)) then
            Result := False
          else begin
            p := Control.ScreenToClient(acMousePos);
            Result := PtInRect(MkRect(Control), p);
          end;
      end
      else
        Result := False;

    if Result then
      if Control is TGraphicControl then begin
        DC := GetDC(Control.Parent.Handle);
        try
          if GetClipBox(DC, R) = 0 then
            Result := False
          else
            if (R.Left - Control.Left > p.X) or (R.Top - Control.Top > p.Y) then
              Result := False
            else
              if (R.Right < Control.Left + p.X) or (R.Bottom < Control.Top + p.Y) then
                Result := False;
        finally
          ReleaseDC(Control.Parent.Handle, DC);
        end;
      end
      else
        if (Control is TWinControl) and TWinControl(Control).HandleAllocated then begin
          // Find window under mouse
          VclWnd := WindowFromPoint(acMousePos);
          // Is this a child window of the Control?
          if VclWnd <> 0 then
            Result := ContainsWnd(VclWnd, TWinControl(Control).Handle);
        end
        else
          Result := False;
  end
  else
    Result := False;
end;


function acMouseInControlDC(Handle: THandle): boolean;
var
  DC: hdc;
  WndRect, R: TRect;
begin
  if IsWindowVisible(Handle) then begin
    DC := GetWindowDC(Handle);
    GetWindowRect(Handle, WndRect);
    try
      case GetClipBox(DC, R) of
        SIMPLEREGION: begin
          OffsetRect(R, WndRect.Left, WndRect.Top);
          Result := PtInRect(R, acMousePos);
        end
        else
          Result := False
      end;
    finally
      ReleaseDC(Handle, DC);
    end;
  end
  else
    Result := False;
end;


function acGetFormImage(aForm: TForm): TBitmap;
var
  SavedDC: hdc;
  BgBmp: TBitmap;
  SkinProvider: TsSkinProvider;
begin
  SkinProvider := TsSkinProvider(aForm.Perform(SM_ALPHACMD, AC_GETPROVIDER_HI, 0));
  if SkinProvider.SkinData.Skinned then begin
    Result := CreateBmp32(aForm.ClientRect);
    BgBmp := TBitmap.Create;
    try
      if SkinProvider.SkinData.BGChanged or SkinProvider.SkinData.FCacheBmp.Empty then
        SkinProvider.PaintAll;

      BgBmp.Assign(SkinProvider.SkinData.FCacheBmp);
      SavedDC := SaveDC(BgBmp.Canvas.Handle);
      SkinPaintTo(BgBmp, aForm, 0, 0, SkinProvider, True);
      RestoreDC(BgBmp.Canvas.Handle, SavedDC);
      BitBlt(Result.Canvas.Handle, 0, 0, Result.Width, Result.Height, BgBmp.Canvas.Handle, SkinProvider.OffsetX, SkinProvider.OffsetY, SRCCOPY);
    finally
      BgBmp.Free;
    end;
  end
  else
    Result := aForm.GetFormImage;
end;


function ContainsWnd(AHandle, AParent: THandle): boolean;
begin
  if AHandle = AParent then
    Result := True
  else
    if AHandle <> 0 then
      Result := ContainsWnd(GetParent(AHandle), AParent)
    else
      Result := False
end;


function CtrlUnderMouse: TControl;
var
  p: TPoint;
  i: integer;
begin
  if (DefaultManager <> nil) and (DefaultManager.ActiveGraphControl <> nil) then
    Result := DefaultManager.ActiveGraphControl
  else begin
    Result := FindVCLWindow(acMousePos);
    if Result <> nil then
      with TWinControl(Result) do begin
        p := ScreenToClient(acMousePos);
        for i := ControlCount - 1 downto 0 do
          if Controls[i].Visible and PtInRect(Controls[i].BoundsRect, p) then begin
            Result := Controls[i];
            Exit;
          end;
      end;
  end;
end;


function LeftToRight(const Control: TControl; const NormalAlignment: boolean = True): boolean;
begin
  if NormalAlignment then
    Result := (Control.BidiMode = bdLeftToRight) or not SysLocale.MiddleEast
  else
    Result := (Control.BidiMode <> bdLeftToRight) and SysLocale.MiddleEast;
end;


procedure AddToAdapter(const Ctrl: TWinControl);
var
  c: TWinControl;
begin
  if Ctrl <> nil then
    if [csDesigning, csLoading] * Ctrl.ComponentState = [] then begin
      c := GetParentForm(Ctrl);
      if c <> nil then
        TrySendMessage(c.Handle, SM_ALPHACMD, AC_CONTROLLOADED shl 16, LPARAM(Ctrl));
    end;
end;


procedure UpdateGlassMode(Ctrl: TControl; ChildInclude: boolean = True);
var
  sd: TsCommonData;
begin
  if (Ctrl <> nil) and (Ctrl.Parent <> nil) and Ctrl.Parent.HandleAllocated and ([csLoading] * Ctrl.Parent.ComponentState = []) then begin
    sd := GetCommonData(Ctrl.Parent.Handle);
    if (sd <> nil) and (sd.TranspMode <> TM_OPAQUE) then
      Ctrl.Perform(SM_ALPHACMD, WParam(AC_SETGLASSMODE shl 16), MakeLParam(integer(ChildInclude), sd.TranspMode))
    else
      if (sd = nil) and (Ctrl.Parent is TTabSheet) then begin
        sd := GetCommonData(Ctrl.Parent.Parent.Handle);
        if (sd <> nil) and (sd.TranspMode <> TM_OPAQUE) then
          Ctrl.Perform(SM_ALPHACMD, WParam(AC_SETGLASSMODE shl 16), MakeLParam(integer(ChildInclude), sd.TranspMode))
      end;
  end;
end;


procedure BroadCastMsg(const Ctrl: hwnd; const Message: TMessage);
var
  hCtrl: THandle;
begin
  hCtrl := GetTopWindow(Ctrl);
  while hCtrl <> 0 do begin
    if GetWindowLong(hCtrl, GWL_STYLE) and WS_CHILD <> 0 then
      TrySendMessage(hCtrl, Message.Msg, Message.WParam, Message.LParam);

    hCtrl := GetNextWindow(hCtrl, GW_HWNDNEXT);
  end;
end;


procedure IterateControls(Owner: TWinControl; Data: integer; CallBack: TacIterProc);
var
  i: integer;
begin
  for i := 0 to Owner.ControlCount - 1 do {if Owner.Controls[i] is TControl then} begin
    CallBack(TWinControl(Owner.Controls[i]), Data);
    if Owner.Controls[i] is TWinControl then
      IterateControls(TWinControl(Owner.Controls[i]), Data, CallBack);
  end;
end;


function IterateControls(Owner: TWinControl; Data: integer; CallBack: TacIterFun): boolean;
var
  i, ActData: integer;
begin
  Result := True;
  for i := 0 to Owner.ControlCount - 1 do begin
    ActData := Data;
    if CallBack(TWinControl(Owner.Controls[i]), ActData) then
      Exit;

    if Owner.Controls[i] is TWinControl then
      if IterateControls(TWinControl(Owner.Controls[i]), ActData, CallBack) then
        Exit;
  end;
  Result := False; // Normal finishing
end;


procedure EnableCtrl(Ctrl: TControl; Enabled: integer);
begin
  Ctrl.Enabled := boolean(Enabled);
end;


procedure SaveGraphCtrls(Wnd: TWinControl; var cArray: TacCtrlArray);
var
  i: integer;
begin
  Wnd.DoubleBuffered := True;
  for i := 0 to Wnd.ControlCount - 1 do
    if not (csOpaque in TGraphicControl(Wnd.Controls[i]).ControlStyle) then begin
      Wnd.Controls[i].ControlStyle := Wnd.Controls[i].ControlStyle + [csOpaque];
      SetLength(cArray, Length(cArray) + 1);
      cArray[Length(cArray) - 1] := Wnd.Controls[i];
    end;
end;


procedure RestoreGraphCtrls(Wnd: TWinControl; var cArray: TacCtrlArray);
var
  i: integer;
begin
  Wnd.DoubleBuffered := True;
  for i := 0 to Length(cArray) -1 do
    cArray[i].ControlStyle := cArray[i].ControlStyle - [csOpaque];

  SetLength(cArray, 0);
end;

{$IFNDEF FLUENTCOMP}
type
  HPAINTBUFFER = THandle;
{$ENDIF}

procedure BufferedPrintClient(Handle: HWND; Rect: TRect; BGColor: TColor = clWhite);
var
  DC, MemDC: HDC;
  PaintBuffer: HPAINTBUFFER;
begin
  if IsWinVistaUp {$IFNDEF FLUENTCOMP} and Assigned(BeginBufferedPaint){$ENDIF} then begin
    DC := GetDC(Handle);
    try
      PaintBuffer := BeginBufferedPaint(DC, Rect, BPBF_TOPDOWNDIB, nil, MemDC);
      try
        FillDC(MemDC, Rect, BGColor);
        SendMessage(Handle, WM_PRINTCLIENT, MemDC, PRF_CLIENT);
        BufferedPaintMakeOpaque(PaintBuffer, Rect);
      finally
        EndBufferedPaint(PaintBuffer, True);
      end;
    finally
      ReleaseDC(Handle, DC);
    end;
  end;
end;


procedure DoBufferedOut(Canvas: TCanvas; Bmp: TBitmap; R: TRect);
var
  MemDC: HDC;
  Rect: TRect;
  PaintBuffer: HPAINTBUFFER;
begin
  if IsWinVistaUp {$IFNDEF FLUENTCOMP} and Assigned(BeginBufferedPaint){$ENDIF} then begin
    Rect := R;
    PaintBuffer := BeginBufferedPaint(Canvas.Handle, Rect, BPBF_TOPDOWNDIB, nil, MemDC);
    try
      Canvas.Handle := MemDC;
      Canvas.Draw(R.Left, R.Top, Bmp);
    finally
      EndBufferedPaint(PaintBuffer, True);
    end;
  end;
end;


procedure PaintChildCtrls(Ctrl: TWinControl; Bmp: TBitmap);
var
  i: integer;
  SaveIndex: hdc;
begin
  for I := 0 to Ctrl.ControlCount - 1 do
    if Ctrl.Controls[I] is TWinControl then
      with TWinControl(Ctrl.Controls[I]) do
        if (Visible or (csDesigning in ComponentState)) and HandleAllocated then begin
          SaveIndex := SaveDC(Bmp.Canvas.Handle);
          try
            if not (Ctrl.Controls[I] is TCustomForm) or (Parent <> nil) then
              MoveWindowOrg(Bmp.Canvas.Handle, Left, Top);

            SkinPaintTo(Bmp, Ctrl.Controls[I], Left, Top);
          finally
            RestoreDC(Bmp.Canvas.Handle, SaveIndex);
          end;
        end;
end;


type
  TAccessWinControl = class(TWinControl);
  TAccessProvider = class(TsSkinProvider);

{$IFDEF ANIMDEBUG}
var
  iLevel: integer = 0;
{$ENDIF}

procedure SkinPaintTo(const Bmp: TBitmap; const Ctrl: TControl; const Left: integer = 0; const Top: integer = 0; const SkinProvider: TComponent = nil; RootCtrl: boolean = False);
var
{$IFDEF ANIMDEBUG}
  lc: longint;
  sc, sn: string;
{$ENDIF}
  DC: hdc;
  SaveIndex: hdc;
  ParentBG: TacBGInfo;
  bSkinned: boolean;
  RAct, RBig, RSmall, cR: TRect;
  I, Res, NextIndex: Integer;

  procedure PaintNextCtrls(AControl: TControl);
  var
    i: integer;
    SavedDC: hdc;
  begin
    if AControl.Parent <> nil then
      with TWinControl(AControl.Parent) do begin
        NextIndex := -1;
        for i := 0 to ControlCount - 1 do
          if Controls[i] = AControl then begin
            NextIndex := i + 1;
            Break;
          end;

        for i := NextIndex to ControlCount - 1 do
          if not (Controls[i] is TOutputWindow) and Controls[i].Visible then begin
            RBig := AControl.BoundsRect;
            RSmall := Controls[i].BoundsRect;
            if RectInRect(RBig, RSmall, False) then begin
              RAct := RSmall;
              OffsetRect(RAct, -RBig.Left, -RBig.Top);
              SavedDC := SaveDC(Bmp.Canvas.Handle);
              try
                MoveWindowOrg(Bmp.Canvas.Handle, RAct.Left, RAct.Top);
                SkinPaintTo(Bmp, Controls[i]);
                MoveWindowOrg(Bmp.Canvas.Handle, -RAct.Left, -RAct.Top);
              finally
                RestoreDC(Bmp.Canvas.Handle, SavedDC);
              end;
            end;
          end;
      end;
  end;

begin
  if (SkinProvider = nil) and (Ctrl.Parent <> nil) and not (Ctrl.Visible or (csDesigning in Ctrl.ComponentState)) then begin
    ParentBG.DrawDC := 0;
    ParentBG.PleaseDraw := False;
    GetBGInfo(@ParentBG, Ctrl.Parent);
    if ParentBG.BgType = btCache then
      BitBlt(Bmp.Canvas.Handle, Left, Top, Ctrl.Width, Ctrl.Height, ParentBG.Bmp.Canvas.Handle, ParentBG.Offset.X + Ctrl.Left, ParentBG.Offset.Y + Ctrl.Top, SRCCOPY)
    else
      FillDC(Bmp.Canvas.Handle, Rect(Left, Top, Ctrl.Width, Ctrl.Height), ParentBG.Color);
  end
  else begin
    DC := Bmp.Canvas.Handle;
    Bmp.Canvas.Lock;
    if (SkinProvider = nil) or (TsSkinProvider(SkinProvider).BorderForm = nil) then begin
      GetWindowRect(TWinControl(Ctrl).Handle, cR);
      IntersectClipRect(DC, 0, 0, Ctrl.Width, Ctrl.Height);
    end;
    if Ctrl is TWinControl then begin
      // If Ctrl is MdiChildForm
      if (Ctrl is TForm) and (TForm(Ctrl).FormStyle = fsMDIForm) then
        for I := 0 to TForm(Ctrl).MDIChildCount - 1 do begin
          SaveIndex := SaveDC(DC);
          MoveWindowOrg(DC, TForm(Ctrl).MDIChildren[i].Left, TForm(Ctrl).MDIChildren[i].Top);
          SkinPaintTo(Bmp, TForm(Ctrl).MDIChildren[i], TForm(Ctrl).MDIChildren[i].Left, TForm(Ctrl).MDIChildren[i].Top);
          RestoreDC(DC, SaveIndex);
        end;

//      if (Ctrl is TTabsheet) and (TTabSheet(Ctrl).BorderWidth <> 0) then
//        MoveWindowOrg(DC, TTabSheet(Ctrl).BorderWidth, TTabSheet(Ctrl).BorderWidth);

{$IFDEF ANIMDEBUG}
      if Ctrl.Name <> '' then
        sn := Ctrl.Name
      else
        sn := 'unnamed <' + Ctrl.ClassName + '>';

      sc := AddCharR(' ', AddChar(' ', '', iLevel * 2) + 'Paint ' + sn + ':', 50);
      lc := GetTickCount;
{$ENDIF}
      try
        Res := TrySendMessage(TWinControl(Ctrl).Handle, SM_ALPHACMD, MakeWParam(1, AC_PRINTING), LPARAM(DC));
        if (Res = 1) or WndIsSkinned(TWinControl(Ctrl).Handle) then begin
          bSkinned := True;
          Res := TrySendMessage(TWinControl(Ctrl).Handle, WM_PRINT, WPARAM(DC), 0);
          TrySendMessage(TWinControl(Ctrl).Handle, SM_ALPHACMD, MakeWParam(0, AC_PRINTING), LPARAM(DC));
          if (Ctrl is TsMemo) and (TsMemo(Ctrl).SkinData.TranspMode = TM_PARTIAL) then
            FillAlphaRect(Bmp, Rect(Left, Top, Left + Ctrl.Width, Top + Ctrl.Height), MaxByte); // ?
        end
        else begin
          TWinControl(Ctrl).PaintTo(DC, 0, 0);
          bSkinned := False;
        end;
      finally
      end;

{$IFDEF ANIMDEBUG}
      AddToLog(sc + IntToStr(GetTickCount - lc));
      inc(iLevel);
{$ENDIF}
      if bSkinned and (Res <> NOCHILDRENPRINT) then
        PaintChildCtrls(TWinControl(Ctrl), Bmp);
{$IFDEF ANIMDEBUG}
      dec(iLevel);
{$ENDIF}
      if SkinProvider <> nil then begin
        TAccessProvider(SkinProvider).FillAlphaClient(Bmp);
        SendAMessage(TWinControl(Ctrl).Handle, AC_PAINTFLOATITEMS, LPARAM(Bmp)); // Paint float buttons
      end
      else
        if (Ctrl is TTabsheet) and (TTabSheet(Ctrl).BorderWidth <> 0) then
          MoveWindowOrg(DC, -TTabSheet(Ctrl).BorderWidth, -TTabSheet(Ctrl).BorderWidth);
    end
    else begin
      TrySendMessage(Ctrl, SM_ALPHACMD, MakeWParam(1, AC_PRINTING), LPARAM(DC));
      Ctrl.Perform(WM_PRINT, WPARAM(DC), 0);
      TrySendMessage(Ctrl, SM_ALPHACMD, MakeWParam(0, AC_PRINTING), LPARAM(DC));
    end;
    Bmp.Canvas.Unlock;
  end;
  if RootCtrl then
    PaintNextCtrls(Ctrl);
end;


procedure PaintChildStdCtrls(Ctrl: TWinControl; Bmp: TBitmap);
var
  ChildWnd: TWinControl;
  R: TRect;
  i: integer;
  SaveIndex: hdc;
begin
  for I := 0 to Ctrl.ControlCount - 1 do
    if Ctrl.Controls[I] is TWinControl then begin
      ChildWnd := TWinControl(Ctrl.Controls[I]);
      with ChildWnd do
        if (Visible or (csDesigning in ComponentState)) and HandleAllocated then begin
          R := ChildWnd.BoundsRect;
            SaveIndex := SaveDC(Bmp.Canvas.Handle);
            try
              if not (ChildWnd is TCustomForm) or (ChildWnd.Parent <> nil) then
                MoveWindowOrg(Bmp.Canvas.Handle, ChildWnd.Left, ChildWnd.Top);

              ChildWnd.PaintTo(Bmp.Canvas.Handle, 0, 0);
              PaintChildStdCtrls(ChildWnd, Bmp);
            finally
              RestoreDC(Bmp.Canvas.Handle, SaveIndex);
            end;
          end;
    end
    else begin
      SaveIndex := SaveDC(Bmp.Canvas.Handle);
      try
        MoveWindowOrg(Bmp.Canvas.Handle, Ctrl.Controls[I].Left, Ctrl.Controls[I].Top);
        Ctrl.Controls[I].Perform(WM_PAINT, WPARAM(Bmp.Canvas.Handle), 0);
      finally
        RestoreDC(Bmp.Canvas.Handle, SaveIndex);
      end;
    end;
end;


procedure StdPaintTo(const Bmp: TBitmap; const Ctrl: TWinControl);
begin
  Bmp.Canvas.Lock;
  SendMessage(Ctrl.Handle, WM_ERASEBKGND, WParam(Bmp.Canvas.Handle), 0);

  if Ctrl is TWinControl then begin
    if (Ctrl is TTabsheet) and (TTabSheet(Ctrl).BorderWidth <> 0) then
      MoveWindowOrg(Bmp.Canvas.Handle, TTabSheet(Ctrl).BorderWidth, TTabSheet(Ctrl).BorderWidth);

    PaintChildStdCtrls(TWinControl(Ctrl), Bmp);
    if (Ctrl is TTabsheet) and (TTabSheet(Ctrl).BorderWidth <> 0) then
      MoveWindowOrg(Bmp.Canvas.Handle, -TTabSheet(Ctrl).BorderWidth, -TTabSheet(Ctrl).BorderWidth);
  end
  else
    Ctrl.Perform(WM_PRINT, WPARAM(Bmp.Canvas.Handle), 0);

  FillAlphaRect(Bmp, MkRect(Bmp), MaxByte); // Fill AlphaChannell in client area
  Bmp.Canvas.Unlock;
end;


procedure StdPaintTo(const Bmp: TBitmap; Ctrl: TControl);
var
  SavedDC: hdc;
begin
  if not (Ctrl is TWinControl) then begin
    SavedDC := SaveDC(Bmp.Canvas.Handle);
    try
      MoveWindowOrg(Bmp.Canvas.Handle, -Ctrl.Left, -Ctrl.Top);
      StdPaintTo(Bmp, Ctrl.Parent);
    finally
      RestoreDC(Bmp.Canvas.Handle, SavedDC);
    end;
  end
  else
    StdPaintTo(Bmp, TWinControl(Ctrl));
end;


function AlignmentFromInt(Value: UINT): TAlignment;
begin
  if Value and DT_RIGHT <> 0 then
    Result := taRightJustify
  else
    if Value and DT_CENTER <> 0 then
      Result := taCenter
    else
      Result := taLeftJustify;
end;


function VertAlignmentFromInt(Value: UINT): TVerticalAlignment;
begin
  if Value and DT_BOTTOM <> 0 then
    Result := taAlignBottom
  else
    if Value and DT_VCENTER <> 0 then
      Result := taVerticalCenter
    else
      Result := taAlignTop;
end;


function GetTransMode(ASkinData: TsCommonData; ASkinIndex: integer = -1; AState: integer = 0): Cardinal;
begin
  if (ASkinData.TranspMode > 0) and (ASkinData.FOwnerControl <> nil) { (ACS_GLASSPAINT and ASkinData.CtrlSkinState <> 0)} then begin
    if ASkinIndex < 0 then
      ASkinIndex := ASkinData.SkinIndex;

    if ASkinIndex < 0 then
      Result := TM_OPAQUE
    else begin
      if ASkinData.OpaqueMode = omAlphaChannel then
        Result := TM_FULL
      else
        if {(ASkinData.SkinSection = '') or} (ASkinData.TranspMode = TM_PARTIAL) or not NeedParentFont(ASkinData.SkinManager, ASkinIndex, AState) then
          Result := TM_PARTIAL
        else
          Result := TM_FULL
    end;
  end
  else
    Result := TM_OPAQUE;
end;


procedure CleanPixelsByRects(const Rects: TRects; Bmp: TBitmap);
var
  i: integer;
begin
  for i := 0 to Length(Rects) - 1 do
    FillRect32(Bmp, Rects[i], 0, 0);
end;


const
  cFlags = SWP_NOSENDCHANGING {$IFDEF D2005}+ SWP_NOREDRAW{$ENDIF} + SWP_NOOWNERZORDER + SWP_NOZORDER + SWP_NOACTIVATE + SWP_NOMOVE;

procedure UpdateLayerVisibility(SkinData: TsCommonData; Handle: THandle);
var
  R: TRect;
  h: integer;
  Form: TCustomForm;
  bUpdating: boolean;
  OldOnResize: TNotifyEvent;
  ConstraintsMinHeight: integer;
begin
  if ACAllowWorkaround1 then begin // Workaround for disappeared forms, when form is transparent still
    if (SkinData <> nil) and (SkinData.FOwnerObject is TsSkinProvider) then begin
      Form := TsSkinProvider(SkinData.FOwnerObject).Form;
      bUpdating := SkinData.FUpdating;
      SkinData.FUpdating := True;
    end
    else begin
//      Form := nil;
//      bUpdating := False;
      Exit;
    end;

    ConstraintsMinHeight := -1;
    if Form <> nil then begin
//      if (Form.Constraints.MinHeight <> 0) and (Form.Constraints.MinHeight = Form.Height) or
//        (Form.Constraints.MinWidth <> 0) and (Form.Constraints.MinWidth = Form.Width) then
//          Exit;
      if (Form.Constraints.MinHeight <> 0) and (Form.Constraints.MinHeight = Form.Height) then begin
        ConstraintsMinHeight := Form.Constraints.MinHeight;
        Form.Constraints.MinHeight := 0;
      end;
      OldOnResize := TForm(Form).OnResize;
      Form.DisableAlign;
    end
    else
      OldOnResize := nil;

    GetWindowRect(Handle, R);
    h := HeightOf(R);
    SetWindowPos(Handle, 0, 0, 0, WidthOf(R), h - 1, cFlags);

    if (Form = nil) or (Form.BorderStyle = bsNone) then begin
      SetWindowPos(Handle, 0, 0, 0, WidthOf(R), h, cFlags {and not SWP_NOREDRAW // Desc 1 });
      R := MkRect(Form);
      R.Top := R.Bottom - 1;
      RedrawWindow(Handle, @R, 0, RDW_ERASE or RDW_INVALIDATE or RDW_UPDATENOW);
    end
    else
      SetWindowPos(Handle, 0, 0, 0, WidthOf(R), h, cFlags);

    if Form <> nil then begin
      Form.EnableAlign;
      TForm(Form).OnResize := OldOnResize;
      if ConstraintsMinHeight <> -1 then // Restore
        Form.Constraints.MinHeight := ConstraintsMinHeight;
    end;
    if (SkinData <> nil) and (SkinData.FOwnerObject is TsSkinProvider) then
      SkinData.FUpdating := bUpdating;
  end;
end;


function RepaintOpaque(Ctrl: TControl; var Data: integer): boolean;
begin
  if Ctrl is TWinControl then
    Result := boolean(TrySendMessage(TWinControl(Ctrl).Handle, SM_ALPHACMD, MakeWParam(Data {Do repaint}, AC_FORCEOPAQUE), 0))
  else
    Result := False; // Continue
end;


const
  acwDivider = 32;
  ShowCommands: array[TWindowState] of Integer = (SW_SHOWNORMAL, SW_SHOWMINNOACTIVE, SW_SHOWMAXIMIZED);
//  DebugOffsX = 100; DebugOffsY = -100;
  DebugOffsX = 0; DebugOffsY = 0;

procedure AnimShowForm(sp: TsSkinProvider; wTime: word = 0; MaxTransparency: integer = MaxByte; AnimType: TacAnimType = atAero);
var
{$IFDEF ANIMDEBUG}
  lc, lCommon: longint;
{$ENDIF}
  h: hwnd;
  fR: TRect;
  lTicks: DWord;
  Flags: Cardinal;
  FBmpSize: TSize;
  acwPopupDiv: real;
  FBlend: TBlendFunction;
  AnimBmp, acDstBmp: TBitmap;
  i, StepCount, cy, cx: integer;
  ObscForm, AnimForm: TacGlowForm;
  dx, dy, l, t, r, b, trans, p: real;

  procedure Anim_Init;
  begin
    trans := 0;
    p := (MaxTransparency - 50) / StepCount;
    case AnimType of
      atDropDown: begin
        l := 0;
        t := 0;
        r := acDstBmp.Width;
        b := 0;
        p := (MaxTransparency - 50) / StepCount;
        acwPopupDiv := exp(1 / StepCount * ln(acDstBmp.Height div 2 {finish size}));
        dx := 0;
        dy := (acDstBmp.Height - b) / acwPopupDiv;
      end;

      atAero: begin
        l := acDstBmp.Width / acwDivider;
        t := acDstBmp.Height / acwDivider;
        dx := l / StepCount;
        dy := t / StepCount;
        r := acDstBmp.Width  - l;
        b := acDstBmp.Height - t;
      end

      else begin
        dx := 0;
        dy := 0;
        l := 0;
        t := 0;
        r := acDstBmp.Width;
        b := acDstBmp.Height;
      end;
    end
  end;

  procedure Anim_DoNext;
  var
    m: tagMSG;
    msg: TMessage;

    procedure HandleSysMsg(aMessage: Cardinal);
    begin
      if PeekMessage(M, sp.Form.Handle, aMessage, aMessage, PM_NOREMOVE) then begin
        msg := MakeMessage(aMessage, M.wParam, M.lParam, 0);
        sp.OldWndProc(msg);
      end;
    end;

  begin
    // Check some required sys messages
    HandleSysMsg(WM_QUERYUISTATE); // Showing of app icon

    trans := min(Trans + p, MaxTransparency);
    FBlend.SourceConstantAlpha := Round(Trans);
    case AnimType of
      atDropDown: begin
        FBlend.SourceConstantAlpha := MaxByte;
        if (l < 0) or (t < 0) then
          BitBlt(AnimBmp.Canvas.Handle, 0, 0, acDstBmp.Width, acDstBmp.Height, acDstBmp.Canvas.Handle, 0, 0, SRCCOPY)
        else
          StretchBlt(AnimBmp.Canvas.Handle, Round(l), Round(t), Round(r - l), Round(b - t), acDstBmp.Canvas.Handle, 0, 0, acDstBmp.Width, acDstBmp.Height, SRCCOPY);
      end;

      atAero:
        if (l < 0) or (t < 0) then
          BitBlt(AnimBmp.Canvas.Handle, 0, 0, acDstBmp.Width, acDstBmp.Height, acDstBmp.Canvas.Handle, 0, 0, SRCCOPY)
        else
          StretchBlt(AnimBmp.Canvas.Handle, Round(l), Round(t), Round(r - l), Round(b - t), acDstBmp.Canvas.Handle, 0, 0, acDstBmp.Width, acDstBmp.Height, SRCCOPY);

      else
        if l = 0 then begin
          BitBlt(AnimBmp.Canvas.Handle, 0, 0, acDstBmp.Width, acDstBmp.Height, acDstBmp.Canvas.Handle, 0, 0, SRCCOPY);
          l := 1;
        end
    end
  end;

  function Anim_GoToNext: boolean;
  begin
    Result := True;
    case AnimType of
      atDropDown: begin
        l := 0;
        t := 0;
        r := acDstBmp.Width - dx;
        b := acDstBmp.Height - dy;
        dx := dx / acwPopupDiv;
        dy := dy / acwPopupDiv;
        if (dx < 2) and (dy < 2) then
          Result := False;
      end;

      atAero: begin
        l := l - dx;
        t := t - dy;
        r := r + dx;
        b := b + dy;
      end
    end
  end;

  procedure UpdateBlend;
  begin
{$IFDEF DELPHI7UP}
    if sp.Form.AlphaBlend then
      DoLayered(sp.Form.Handle, True, sp.Form.AlphaBlendValue)
    else
{$ENDIF}
    begin
      if AeroIsEnabled then
        SetLayeredWindowAttributes(sp.Form.Handle, clNone, MaxByte, ULW_ALPHA);

      if SetWindowLong(sp.Form.Handle, GWL_EXSTYLE, GetWindowLong(sp.Form.Handle, GWL_EXSTYLE) and not WS_EX_LAYERED) = 0 then
        CheckLastError;
    end;
  end;

begin
  if (sp.SkinData <> nil) and not IsIconic(sp.Form.Handle) then begin
{$IFDEF ANIMDEBUG}
    AddToLog('');
    AddToLog('<<<<<<<< ' + sp.Form.Name + ' showing animation start <<<<<<<<');
    iLevel := 0;
    lCommon := GetTickCount;
    lc := lCommon;
{$ENDIF}

    InAnimationProcess := True;
    if sp.BorderForm <> nil then begin
      if sp.BorderForm.AForm = nil then
        sp.BorderForm.CreateNewForm;

      AnimForm := sp.BorderForm.AForm;
      if SetWindowRgn(AnimForm.Handle, 0, False) = 0 then
        CheckLastError;

      sp.BorderForm.PaintAll;
    end
    else begin
      TAccessProvider(sp).PaintAll;
      AnimForm := TacGlowForm.CreateNew(Application);
    end;
{$IFDEF ANIMDEBUG}
    AddToLog(AddCharR(' ', AddChar(' ', '', iLevel * 2) + 'Paint ' + sp.Form.Name + ':', 50) + IntToStr(GetTickCount - lc));
{$ENDIF}
    if sp.SkinData.FCacheBmp <> nil then begin
      acDstBmp := CreateBmp32(sp.SkinData.FCacheBmp);
{$IFDEF ACDEBUG}
      acDebugAnimBmp := acDstBmp;
{$ENDIF}
      if (sp.BorderForm = nil) and sp.UWPMode or TAccessProvider(sp).IsBlurred then
        FillRect32(acDstBmp, MkRect(acDstBmp), 0, 0);

      acDstBmp.Canvas.Lock;

      SkinPaintTo(acDstBmp, sp.Form, 0, 0, sp);
      if sp.BorderForm = nil then
        if sp.UWPMode then
          TAccessProvider(sp).FillAlphaClient(acDstBmp)
        else
          FillAlphaRect(acDstBmp, MkRect(acDstBmp), MaxByte);

      if acDstBmp <> nil then begin
        acDstBmp.Canvas.UnLock;
        FBmpSize := MkSize(acDstBmp);
        StepCount := wTime div acTimerInterval;
        Flags := SWP_NOACTIVATE or SWP_NOREDRAW or SWP_NOCOPYBITS or SWP_NOOWNERZORDER or SWP_NOSENDCHANGING;
        if StepCount > 0 then
          InitBlendData(FBlend, 0)
        else
          InitBlendData(FBlend, MaxTransparency);

        cy := 0;
        cx := 0;
        if sp.BorderForm <> nil then begin
          fr := sp.Form.BoundsRect;
          TAccessProvider(sp).FSysExHeight := IsZoomed(sp.Form.Handle) and (sp.CaptionHeight < SysCaptHeight(sp) + 4);
          if TAccessProvider(sp).FSysExHeight then
            cy := sp.ShadowSize.Top + DiffTitle(sp.BorderForm) + SysBorderWidth(sp.Form.Handle, sp.BorderForm, False) //  4
          else
            cy := sp.BorderForm.OffsetY;

          cx := SkinBorderWidth(sp.BorderForm) - SysBorderWidth(sp.Form.Handle, sp.BorderForm, False) + sp.ShadowSize.Left;// - sp.CXPaddedBorder;
        end
        else begin
          GetWindowRect(sp.Form.Handle, fR);
          FillArOR(sp);
          CleanPixelsByRects(TAccessProvider(sp).Rects, acDstBmp);
        end;

        if SetWindowLong(AnimForm.Handle, GWL_EXSTYLE, GetWindowLong(AnimForm.Handle, GWL_EXSTYLE) or WS_EX_NOACTIVATE) = 0 then
          CheckLastError;

        if GetWindowLong(sp.Form.Handle, GWL_EXSTYLE) and WS_EX_TOPMOST <> 0 then begin
          TForm(AnimForm).FormStyle := fsStayOnTop;
          AnimForm.Width := 0;
          AnimForm.Height := 0;
          if SetWindowLong(AnimForm.Handle, GWL_EXSTYLE, GetWindowLong(AnimForm.Handle, GWL_EXSTYLE) or WS_EX_LAYERED) = 0 then
            CheckLastError;

          h := HWND_TOPMOST;
        end
        else begin
          if fsModal in sp.Form.FormState then
            TForm(AnimForm).FormStyle := fsStayOnTop;

          h := sp.Form.Handle;
        end;

        if GetWindowLong(AnimForm.Handle, GWL_EXSTYLE) and WS_EX_LAYERED = 0 then
          if SetWindowLong(AnimForm.Handle, GWL_EXSTYLE, GetWindowLong(AnimForm.Handle, GWL_EXSTYLE) or WS_EX_LAYERED) = 0 then
            CheckLastError;

        UpdateLayeredWnd(AnimForm.Handle, acDstBmp, @FBmpSize, @FBlend);//, @p);
        AnimForm.SetBounds(fR.Left - cx + DebugOffsX{ + 300}, fR.Top - cy + DebugOffsY, acDstBmp.Width, acDstBmp.Height);

        if not SetWindowPos(AnimForm.Handle, 0, 0, 0, 0, 0, SWPA_SHOW or SWP_NOMOVE or SWP_NOSIZE) then
          CheckLastError;

        ShowWindow(AnimForm.Handle, SW_SHOWNOACTIVATE);

        if not SetWindowPos(AnimForm.Handle, h, AnimForm.Left, AnimForm.Top, FBmpSize.cx, FBmpSize.cy, Flags) then
          CheckLastError;

        AnimBmp := CreateBmp32(FBmpSize);
        FillDC(AnimBmp.Canvas.Handle, MkRect(AnimBmp), 0);
        SetStretchBltMode(AnimBmp.Canvas.Handle, COLORONCOLOR);

        if StepCount > 0 then begin
          Anim_Init;
          i := 0;
          while i <= StepCount do begin
            Anim_DoNext;
            lTicks := GetTickCount;
            UpdateLayeredWnd(AnimForm.Handle, AnimBmp, @FBmpSize, @FBlend);
            if not Anim_GoToNext then
              Break;

            inc(i);
            if StepCount > 0 then
              WaitTicks(lTicks);
          end;
          FBlend.SourceConstantAlpha := MaxTransparency;
        end;
        if not SetWindowPos(AnimForm.Handle, 0, fR.Left - cx + DebugOffsX{ + 800}, fr.Top - cy + DebugOffsY, FBmpSize.cx, FBmpSize.cy, Flags or SWP_NOZORDER) then
          CheckLastError;

        UpdateLayeredWnd(AnimForm.Handle, acDstBmp, @FBmpSize, @FBlend);
        FreeAndNil(AnimBmp);
        if sp <> nil then begin
          sp.FInAnimation := False;

          DoLayered(sp.Form.Handle, True, 0);
          ShowWindow(sp.Form.Handle, ShowCommands[sp.Form.WindowState]);
//    SendMessage(sp.Form.Handle, SM_ALPHACMD, AC_PAINTOUTER_HI, 1);

          if not SetWindowPos(sp.Form.Handle, AnimForm.Handle, 0, 0, 0, 0, SWPA_ZORDER) then
            CheckLastError;

          if sp.BorderForm <> nil then
            sSkinProvider.UpdateRgn(sp, True, True);

          // Make the obscuring form
          if acIsDPIAware and not IsFluentOS and IsWinVistaUp {$IFNDEF FLUENTCOMP}and Assigned(BeginBufferedPaint){$ENDIF}{AeroIsEnabled} then
            ObscForm := MakeCoverForm(AnimForm.Handle)
          else
            ObscForm := nil;

          UpdateBlend;
          InAnimationProcess := False;
          if sp.BorderForm <> nil then
            sp.BorderForm.ExBorderShowing := True; // Do not update extended borders

          // Make the obscuring form
          if acIsDPIAware and IsFluentOS then
            ObscForm := MakeCoverForm(AnimForm.Handle);

          UpdateLayerVisibility(sp.SkinData, sp.Form.Handle);
          if not sp.SkinData.FUpdating then
            SetParentUpdated(sp.Form);

          if (Win32MajorVersion = 6) and (Win32MinorVersion = 0) then // Patch for Vista
            if SetWindowLong(sp.Form.Handle, GWL_STYLE, GetWindowLong(sp.Form.Handle, GWL_STYLE) or WS_VISIBLE) = 0 then // Form must be visible
              CheckLastError;

          if sp.BorderForm <> nil then
            sSkinProvider.UpdateRgn(sp, True, True); // Guarantees that region is updated

          // Repaint form after animation
          if not RedrawWindow(sp.Form.Handle, nil, 0, RDWA_ALLNOW) then
            CheckLastError;

          if sp.BorderForm <> nil then begin
            sp.BorderForm.ExBorderShowing := False;
            if SetWindowRgn(AnimForm.Handle, sp.BorderForm.MakeRgn, False) = 0 then
              CheckLastError;
          end;
          if sp.SystemBlur.Active then
            IterateControls(sp.Form, 0, RepaintOpaque);

          FreeAndNil(acDstBmp);
//          if AeroIsEnabled then
//            Sleep(20); // Removing of blinking in Aero

          if not SetWindowPos(AnimForm.Handle, sp.Form.Handle, 0, 0, 0, 0, SWPA_ZORDER) then
            CheckLastError;

          // Destroy the obscuring form
          if ObscForm <> nil then begin
            Sleep(20); // Handle all painting messages for prevent of blinking
            ObscForm.Free;
          end;

          if sp.BorderForm = nil then
            FreeAndNil(AnimForm)
          else begin
            UpdateBlend;
            if SetWindowRgn(AnimForm.Handle, sp.BorderForm.MakeRgn, False) = 0 then
              CheckLastError;

            if sp.SkinData.SkinManager.SkinCommonInfo.UseAeroBluring and sp.SkinData.SkinManager.Effects.AllowAeroBluring and AeroIsEnabled then
              sp.BorderForm.UpdateExBordersPos;
          end;
          if Assigned(sp.OnAfterAnimation) then
            sp.OnAfterAnimation(aeShowing);
        end;
      end;
    end;
{$IFDEF ANIMDEBUG}
    AddToLog('>>>>>>>> ' + sp.Form.Name + ' showing animation finished: ' + IntToStr(GetTickCount - lCommon) + ' >>>>>>>>');
    AddToLog('');
    dec(iLevel);
{$ENDIF}
  end;
end;


procedure HideAnimForm(var Form: TacGlowForm; SrcBmp: TBitmap; ATime: integer; StartBlendValue: integer; AnimType: TacAnimType; ParentForm: TForm);
var
  h: hwnd;
  lTicks: DWord;
begin
  if (acHideTimer <> nil) and (acHideTimer.Form <> nil) then
    FreeAndNil(acHideTimer.Form);

  if ATime > 0 then begin
    if acHideTimer = nil then begin
      acHideTimer := TacHideTimer.Create(Application);
      acHideTimer.Enabled := False;
    end;

    if ParentForm <> nil then
      acHideTimer.ParentWnd := ParentForm.Handle
    else
      acHideTimer.ParentWnd := 0;

    acHideTimer.EventCalled := False;
    acHideTimer.i := 0;
    if (acHideTimer.SrcBmp <> nil) and (acHideTimer.SrcBmp <> SrcBmp) then
      FreeAndNil(acHideTimer.SrcBmp);

    acHideTimer.SrcBmp := SrcBmp;
    acHideTimer.StartBlendValue := StartBlendValue;
    acHideTimer.Form := Form;
    acHideTimer.AnimType := AnimType;
    acHideTimer.Interval := acTimerInterval;

    acHideTimer.StepCount := ATime div acTimerInterval;

    acHideTimer.FBmpSize := MkSize(acHideTimer.SrcBmp);

    InitBlendData(acHideTimer.FBlend, StartBlendValue);


    SetWindowLong(Form.Handle, GWL_EXSTYLE, GetWindowLong(Form.Handle, GWL_EXSTYLE) or WS_EX_LAYERED or WS_EX_NOACTIVATE);

    UpdateLayeredWnd(Form.Handle, SrcBmp, @acHideTimer.FBmpSize, @acHideTimer.FBlend);
    acHideTimer.DC := 0;
    SetWindowPos(Form.Handle, 0, 0, 0, 0, 0, SWPA_NOCOPYBITS);
    ShowWindow(Form.Handle, SW_SHOWNOACTIVATE);

    if GetWindowLong(Form.Handle, GWL_EXSTYLE) and WS_EX_TOPMOST <> 0 then
      h := HWND_TOPMOST
    else
      if (ParentForm = nil) or (fsModal in ParentForm.FormState) then
        h := HWND_TOPMOST
      else
        h := ParentForm.Handle;

    SetWindowPos(Form.Handle, h, Form.Left, Form.Top, acHideTimer.FBmpSize.cx, acHideTimer.FBmpSize.cy, SWP_NOACTIVATE or SWP_NOREDRAW or SWP_NOCOPYBITS or SWP_NOOWNERZORDER or SWP_NOREDRAW);
    FreeAndNil(acHideTimer.DstBmp);

    acHideTimer.DstBmp := CreateBmp32(acHideTimer.FBmpSize);
    acHideTimer.Anim_Init;
{    if not Application.Terminated and ((ParentForm = nil ) or not Assigned(ParentForm.OnCloseQuery)) then begin
      acHideTimer.OnTimer := acHideTimer.OnTimerProc;
      acHideTimer.i := 0;
      acHideTimer.OnTimerProc(acHideTimer);
      acHideTimer.Enabled := True;
    end
    else         }
      while acHideTimer.i <= acHideTimer.StepCount do begin
        lTicks := GetTickCount;
        acHideTimer.OnTimerProc(acHideTimer);
        WaitTicks(lTicks);
      end;

    if acHideTimer.Form = nil then
      Form := nil;
  end;
end;


procedure AnimHideForm(SkinProvider: TObject; Time: integer{ = 0}; Mode: TacAnimType{ = atAero});
var
  sp: TAccessProvider;
  AnimForm: TacGlowForm;
  acDstBmp: TBitmap;
  b: byte;
begin
  ClearGlows(True);
  sp := TAccessProvider(SkinProvider);
  if (sp <> nil) and (sp.Form.FormStyle <> fsMDIChild) then begin
    if sp.SkinData.FCacheBmp <> nil then begin
{$IFDEF DELPHI7UP}
      if sp.Form.AlphaBlend then
        b := sp.Form.AlphaBlendValue
      else
{$ENDIF}
        b := MaxByte;

      InAnimationProcess := True;
      if sp.FormState and FS_ANIMCLOSING = 0 then
        sp.PaintAll;

      if sp.BorderForm <> nil then begin
        if sp.BorderForm.AForm = nil then
          sp.BorderForm.CreateNewForm;

        AnimForm := sp.BorderForm.AForm;
        if sp.BorderForm.ParentHandle <> 0 then
          SetWindowLong(sp.BorderForm.AForm.Handle, GWL_HWNDPARENT, LONG_PTR(sp.BorderForm.ParentHandle));

        AnimForm.WindowProc := sp.BorderForm.OldBorderProc;
        acDstBmp := CreateBmp32(sp.SkinData.FCacheBmp);
{$IFDEF ACDEBUG}
        acDebugAnimBmp := acDstBmp;
{$ENDIF}
        BitBlt(acDstBmp.Canvas.Handle, 0, 0, sp.SkinData.FCacheBmp.Width, sp.SkinData.FCacheBmp.Height, sp.SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
        sp.BorderForm.AForm := nil;
{.$IFDEF D2011}
        if not AeroIsEnabled then
          SetWindowPos(sp.Form.Handle, 0, 0, 0, 0, 0, SWP_HIDEWINDOW or SWP_NOSIZE or SWP_NOMOVE)
        else
          DoLayered(sp.Form.Handle, True, 0);//1);
{.$ENDIF}
      end
      else begin
        if sp.CoverForm <> nil then begin
          AnimForm := sp.CoverForm;
          sp.CoverForm := nil;
        end
        else
          AnimForm := MakeCoverForm(sp.Form.Handle);

        DoLayered(sp.Form.Handle, True, 0);//1);
        acDstBmp := CreateBmp32(sp.SkinData.FCacheBmp);
{$IFDEF ACDEBUG}
        acDebugAnimBmp := acDstBmp;
{$ENDIF}
        BitBlt(acDstBmp.Canvas.Handle, 0, 0, sp.SkinData.FCacheBmp.Width, sp.SkinData.FCacheBmp.Height, sp.SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
        CleanPixelsByRects(sp.Rects, acDstBmp);
      end;
      ////////////////////////////////
{      if fsModal in sp.Form.FormState then
//        TForm(AnimForm).FormStyle := fsStayOnTop;
        HideAnimForm(AnimForm, acDstBmp, sp.SkinData.SkinManager.AnimEffects.FormHide.Time, b, sp.SkinData.SkinManager.AnimEffects.FormHide.Mode, HWND_TOPMOST)
      else}
        HideAnimForm(AnimForm, acDstBmp, {sp.SkinData.SkinManager.AnimEffects.FormHide.}Time, b{MaxByte}, {sp.SkinData.SkinManager.AnimEffects.FormHide.}Mode, sp.Form);

      if sp.Form <> nil then
        DoLayered(sp.Form.Handle, b <> MaxByte, b);

      if Application.Terminated then
        FreeAndNil(sp.BorderForm)
      else begin
        if (sp.BorderForm <> nil) and (sp.BorderForm.AForm <> nil) then begin
          FreeAndNil(sp.BorderForm.AForm);
          AnimForm := nil;
        end;
        if acHideTimer <> nil then
          acHideTimer.Form := nil;

        FreeAndNil(AnimForm);
      end;
      // Destroy CoverForm if not destroyed by HideAnimForm procedure
      if sp.CoverForm <> nil then
        FreeAndNil(sp.CoverForm);
      ////////////////////////////////
      InAnimationProcess := False;
    end;
  end;
end;


procedure PrintDlgClient(ListSW: TacDialogWnd; acDstBmp: TBitmap; CopyScreen: boolean = False);
var
  SavedDC, DC: hdc;
  R, cR, fR: TRect;
begin
  if CopyScreen then begin
    DC := GetWindowDC(ListSW.CtrlHandle);
    SavedDC := SaveDC(DC);
    try
      R := ACClientRect(ListSW.CtrlHandle);
      BitBlt(acDstBmp.Canvas.Handle, ListSW.OffsetX, ListSW.OffsetY, WidthOf(R), HeightOf(R), DC, R.Left, R.Top, SRCCOPY);
      FillAlphaRect(acDstBmp, Rect(ListSW.OffsetX, ListSW.OffsetY, min(ListSW.OffsetX + WidthOf(R), acDstBmp.Width), min(ListSW.OffsetY + HeightOf(R), acDstBmp.Height)), MaxByte);
    finally
      RestoreDC(DC, SavedDC);
      ReleaseDC(ListSW.CtrlHandle, DC);
    end;
  end
  else begin
    GetClientRect(ListSW.CtrlHandle, cR);
    acDstBmp.Canvas.Lock;
    SavedDC := SaveDC(acDstBmp.Canvas.Handle);

    fR.TopLeft := Point(ListSW.OffsetX, ListSW.OffsetY);

    MoveWindowOrg(acDstBmp.Canvas.Handle, fR.Left, fR.Top);
    IntersectClipRect(acDstBmp.Canvas.Handle, 0, 0, WidthOf(cR), HeightOf(cR));
    TacAccessDialogWnd(ListSW).Provider.PrintHwndControls(TacAccessDialogWnd(ListSW).CtrlHandle, acDstBmp.Canvas.Handle);

    if TacAccessDialogWnd(ListSW).BorderForm <> nil then begin
      fR.TopLeft := Point(ListSW.OffsetX, ListSW.OffsetY);
      fR.Right := fR.Left + WidthOf(cR);
      fR.Bottom := fR.Top + HeightOf(cR);
      FillAlphaRect(acDstBmp, fR, MaxByte);
    end
    else
      FillAlphaRect(acDstBmp, MkRect(acDstBmp), MaxByte);

    RestoreDC(acDstBmp.Canvas.Handle, SavedDC);
    acDstBmp.Canvas.UnLock;
  end;
end;


procedure AnimHideDlg(ListSW: TacDialogWnd);
var
  AnimForm: TacGlowForm;
  acDstBmp: TBitmap;
begin
  InAnimationProcess := True;
  ClearGlows;
  with TacAccessDialogWnd(ListSW) do begin
    if BorderForm <> nil then begin
      AnimForm := BorderForm.AForm;
      if BorderForm.ParentHandle <> 0 then
        SetWindowLong(BorderForm.AForm.Handle, GWL_HWNDPARENT, LONG_PTR(BorderForm.ParentHandle));

      AnimForm.WindowProc := BorderForm.OldBorderProc;
      acDstBmp := CreateBmp32(SkinData.FCacheBmp);
  {$IFDEF ACDEBUG}
        acDebugAnimBmp := acDstBmp;
  {$ENDIF}
      BitBlt(acDstBmp.Canvas.Handle, 0, 0, SkinData.FCacheBmp.Width, SkinData.FCacheBmp.Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
    end
    else begin
      if CoverForm <> nil then
        AnimForm := CoverForm
      else
        AnimForm := MakeCoverForm(CtrlHandle);

      acDstBmp := CreateBmp32(SkinData.FCacheBmp);
  {$IFDEF ACDEBUG}
      acDebugAnimBmp := acDstBmp;
  {$ENDIF}
      BitBlt(acDstBmp.Canvas.Handle, 0, 0, acDstBmp.Width, acDstBmp.Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
      if BorderForm = nil then
        CleanPixelsByRects(ArOR, acDstBmp);
    end;
    HideAnimForm(AnimForm, acDstBmp, SkinData.SkinManager.AnimEffects.DialogHide.Time, MaxByte, SkinData.SkinManager.AnimEffects.DialogHide.Mode, nil);
    if Application.Terminated then
      FreeAndNil(BorderForm)
    else begin
      if (BorderForm <> nil) and (BorderForm.AForm <> nil) then begin
        FreeAndNil(BorderForm.AForm);
        AnimForm := nil;
      end;
      if acHideTimer <> nil then
        acHideTimer.Form := nil;

      FreeAndNil(AnimForm);
    end;
  end;
  InAnimationProcess := False;
end;


function DoLayered(FormHandle: Hwnd; Layered: boolean; AlphaValue: byte = 1): boolean;
begin
  Result := False;
  if Layered and acLayered then begin
    if GetWindowLong(FormHandle, GWL_EXSTYLE) and WS_EX_LAYERED = 0 then begin
      if SetWindowLong(FormHandle, GWL_EXSTYLE, GetWindowLong(FormHandle, GWL_EXSTYLE) or WS_EX_LAYERED) = 0 then
        CheckLastError;

      Result := True;
    end;
    if not SetLayeredWindowAttributes(FormHandle, clNone, AlphaValue, ULW_ALPHA) then
      CheckLastError;
  end
  else begin
    if SetWindowLong(FormHandle, GWL_STYLE, GetWindowLong(FormHandle, GWL_STYLE) and not WS_VISIBLE) = 0 then // Avoid a form showing
      CheckLastError;

    if SetWindowLong(FormHandle, GWL_EXSTYLE, GetWindowLong(FormHandle, GWL_EXSTYLE) and not WS_EX_LAYERED) = 0 then
      CheckLastError;
  end;
end;


function acShowHintWnd(const HintText: string; Pos: TPoint): {$IFDEF TNTUNICODE}TTntCustomHintWindow;{$ELSE}THintWindow;{$ENDIF}
var
  R, wR: TRect;
{$IFNDEF DELPHI5}
  Animate: BOOL;
{$ENDIF}
begin
  Result := nil;
{$IFNDEF ALITE}
  if (Manager <> nil) and Manager.Active then begin
    Manager.ShowHint(Pos, HintText, Application.HintHidePause);
//    if Manager.HintWindowHandle <> 0 then
      Result := Manager.HintWindow
//    else
//      Result := nil;
  end
  else
{$ENDIF}
    if HintText <> '' then begin
{$IFDEF TNTUNICODE}
      Result := TTntHintWindow.Create(nil);
{$ELSE}
      Result := HintWindowClass.Create(nil);
{$ENDIF}
      with Result do begin
        Visible := False;
        Color := clInfoBk;
        R := CalcHintRect(800, HintText, nil);
        OffsetRect(R, Pos.X + 8, Pos.Y + 16);
        wR := acWorkRect(Pos);
        if R.Right > wR.Right then
          OffsetRect(R, wR.Right - R.Right, 0);

        if R.Bottom > wR.Bottom then
          OffsetRect(R, 0, wR.Bottom - R.Bottom);

        if TrySendMessage(Handle, SM_ALPHACMD, MakeWParam(0, 7 {AC_CTRLHANDLED}), 0) = 1 then
          ActivateHint(R, HintText)
        else begin
          // < Solving the "Owner Z-ordering" problem when BorderForm is used
          Caption := HintText;
          Inc(R.Bottom, 4);

          ParentWindow := {$IFNDEF FPC}Application.Handle{$ELSE}Application.MainFormHandle{$ENDIF};
          SetBounds(R.Left, R.Top, WidthOf(R, True), HeightOf(R, True));
          SetWindowPos(Handle, HWND_TOPMOST, R.Left, R.Top, Width, Height,
                       SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOMOVE or SWP_NOOWNERZORDER);
{$IFNDEF DELPHI5}
  {$IFNDEF FPC}
          if (Length(HintText) < 100) and Assigned(AnimateWindowProc) then begin
            SystemParametersInfo(SPI_GETTOOLTIPANIMATION, 0, {$IFNDEF CLR}@{$ENDIF}Animate, 0);
            if Animate then begin
              SystemParametersInfo(SPI_GETTOOLTIPFADE, 0, {$IFNDEF CLR}@{$ENDIF}Animate, 0);
              if Animate then
                AnimateWindowProc(Handle, 200, AW_BLEND);
            end;
          end;
  {$ENDIF}
{$ENDIF}
          ShowWindow(Handle, SW_SHOWNOACTIVATE);
          Invalidate;
          // >
        end;
      end;
    end;
end;


{$IFNDEF ALITE}
procedure acHideHintWnd(var Wnd: {$IFDEF TNTUNICODE}TTntCustomHintWindow{$ELSE}THintWindow{$ENDIF});
begin
  if (Manager <> nil) and Manager.Active and (Manager.HintWindow <> nil) then begin
    FreeAndNil(Manager.HintWindow);
    Wnd := nil;
  end
  else
    FreeAndNil(Wnd);
end;
{$ENDIF}


function acWorkRect(Form: TForm): TRect;
begin
{$IFDEF DELPHI6UP}
  Result := Form.Monitor.WorkareaRect;
{$ELSE}
  SystemParametersInfo(SPI_GETWORKAREA, 0, @Result, 0);
{$ENDIF}
end;


function acWorkRect(Coord: TPoint): TRect; overload;
{$IFDEF DELPHI6UP}
var
  i: integer;
{$ENDIF}
begin
{$IFDEF DELPHI6UP}
  Result := MkRect;
  for i := 0 to Screen.MonitorCount - 1 do
    if PtInRect(Screen.Monitors[i].WorkareaRect, Coord) then begin
      Result := Screen.Monitors[i].WorkareaRect;
      Exit;
    end;
{$ELSE}
  SystemParametersInfo(SPI_GETWORKAREA, 0, @Result, 0);
{$ENDIF}
end;


function GetCtrlScreenBounds(Ctrl: TControl): TRect;
var
  p: TPoint;
begin
  if Ctrl is TWinControl then
    GetWindowRect(TWinControl(Ctrl).Handle, Result)
  else begin
    Result := MkRect(Ctrl);
    p := Ctrl.Parent.ClientToScreen(MkPoint);
    OffsetRect(Result, p.X + Ctrl.Left, p.Y + Ctrl.Top);
  end;
end;


function GetClientPos(Handle: THandle): TPoint;
var
  R: TRect;
begin
  GetWindowRect(Handle, R);
  ScreenToClient(Handle, R.TopLeft);
  Result := Point(-R.Left, -R.Top);
end;


function TopWndAfter: HWND;
begin
{$IFDEF ACDEBUG}
  if IsIDERunning then
    Result := HWND_TOP
  else
{$ENDIF}
    Result := HWND_TOPMOST;
end;


function DefaultPPI: integer;
{$IFDEF ACDPIAWARE}
var
  Form: TForm;
  i: integer;
{$ENDIF}
begin
{$IFDEF ACDPIAWARE}
  Form := nil;
  if Application.MainForm <> nil then
    Form := Application.MainForm
  else
    for i := 0 to Application.ComponentCount - 1 do
      if Application.Components[i] is TCustomForm then begin
        Form := TForm(Application.Components[i]);
        Break;
      end;

  Result := GetControlPPI(Form);
{$ELSE}
  Result := 96;
{$ENDIF}
end;


{$IFNDEF DELPHI_10}
var
  ac_GetDpiForMonitor: function (hmonitor: HMONITOR; dpiType: integer{TacMonitorDpiType}; out dpiX: UINT; out dpiY: UINT): HRESULT; stdcall;
{$ENDIF}

function GetControlPPI(Ctrl: TControl): integer;
{$IFDEF DELPHI_10}
begin
  {$IFDEF ACDPIAWARE}
  if (Ctrl <> nil) and not (csDesigning in Ctrl.ComponentState) then begin
    if not (Ctrl is TWinControl) then
      Ctrl := Ctrl.Parent;

    if (Ctrl <> nil) and TWinControl(Ctrl).HandleAllocated then
      Result := GetWndPPI(TWinControl(Ctrl).Handle)
    else
      Result := 96;
  end
  else
  {$ENDIF}
    Result := 96;
{$ELSE}

  function GetScreenCaps: integer;
  var
    DC: hdc;
  begin
    DC := GetDC(0);
    Result := GetDeviceCaps(DC, LOGPIXELSY);
    ReleaseDC(0, DC);
  end;

  {$IFDEF ACDPIAWARE}
var
  Ydpi, Xdpi: UINT;
  HM: THandle;
  {$ENDIF}
begin
  Result := 96;
  {$IFDEF ACDPIAWARE}
  if IsWin10Up{CheckWin32Version(6, 3)} and (Ctrl <> nil) then begin
    if not (Ctrl is TWinControl) then begin
      Ctrl := Ctrl.Parent;
      if Ctrl = nil then
        Exit;
    end;
    if TWinControl(Ctrl).HandleAllocated then begin
      HM := MonitorFromWindow(TWinControl(Ctrl).Handle, MONITOR_DEFAULTTONEAREST);
      if HM <> 0 then begin
        if Assigned(ac_GetDpiForMonitor) and (ac_GetDpiForMonitor(HM, 0{MDT_EFFECTIVE_DPI}, Ydpi, Xdpi) = S_OK) then
          Result := Xdpi;
      end
      else
        Result := GetScreenCaps;
    end
    else
      Result := GetScreenCaps;
  end
  else
    Result := GetScreenCaps;
  {$ENDIF}
{$ENDIF}
end;


function GetWndPPI(Handle: THandle): integer;
{$IFDEF DELPHI_10}
var
  Monitor: TMonitor;
  R: TRect;
  DC: hdc;
begin
  if CheckWin32Version(6, 3) then begin
    GetWindowRect(Handle, R);
    inc(R.Left, WidthOf(R) div 2);
    inc(R.Top, HeightOf(R) div 2);
    Monitor := Screen.MonitorFromPoint(R.TopLeft);
    if Monitor <> nil then
      Result := Monitor.PixelsPerInch
{$ELSE}
var
  Ydpi, Xdpi: UINT;
  HM: THandle;
  DC: hdc;
begin
  if IsWin10Up then begin
    HM := MonitorFromWindow(Handle, MONITOR_DEFAULTTONEAREST);
    if HM <> 0 then
      if Assigned(ac_GetDpiForMonitor) and (ac_GetDpiForMonitor(HM, 0{MDT_EFFECTIVE_DPI}, Ydpi, Xdpi) = S_OK) then
        Result := Xdpi
      else
        Result := 96
{$ENDIF}
    else
      Result := 96;
  end
  else begin
    DC := GetDC(0);
    Result := GetDeviceCaps(DC, LOGPIXELSY);
    ReleaseDC(0, DC);
  end;
end;
(*
function GetWndPPI(Handle: THandle): integer;
{$IFDEF DELPHI_10}
var
  Monitor: TMonitor;
  fHandle: THandle;
  R: TRect;
  DC: hdc;
begin
  if CheckWin32Version(6, 3) then begin
    fHandle := GetParentFormHandle(Handle);
    GetWindowRect(FHandle, R);
    inc(R.Left, WidthOf(R) div 2);
    inc(R.Top, HeightOf(R) div 2);
    Monitor := Screen.MonitorFromPoint(R.TopLeft);
    if Monitor <> nil then
      Result := Monitor.PixelsPerInch
{$ELSE}
var
  Ydpi, Xdpi: UINT;
  HM: THandle;
  DC: hdc;
begin
  if Win32MajorVersion >= 10 then begin
    HM := MonitorFromWindow(Handle, MONITOR_DEFAULTTONEAREST);
    if HM <> 0 then
      if Assigned(ac_GetDpiForMonitor) and (ac_GetDpiForMonitor(HM, 0{MDT_EFFECTIVE_DPI}, Ydpi, Xdpi) = S_OK) then
        Result := Xdpi
      else
        Result := 96
{$ENDIF}
    else
      Result := 96;
  end
  else begin
    DC := GetDC(0);
    Result := GetDeviceCaps(DC, LOGPIXELSY);
    ReleaseDC(0, DC);
  end;
end;
*)

procedure EnableNCDpiScaling(WndHandle: THandle);
var
  DLLHandle: HModule;
  pEnableNonClientDpiScaling: function (h: THandle): BOOL; stdcall;
begin
  if IsWin10Up then begin
    DLLHandle := SafeLoadLibrary(user32);
    if DLLHandle <> 0 then
    try
      pEnableNonClientDpiScaling := GetProcAddress(DLLHandle, 'EnableNonClientDpiScaling');
      if Assigned(pEnableNonClientDpiScaling) then
        if not pEnableNonClientDpiScaling(WndHandle) then
          CheckLastError;
    finally
      FreeLibrary(DLLHandle);
    end;
  end;
end;


procedure AnimShowDlg(ListSW: TacDialogWnd; wTime: word = 0; MaxTransparency: integer = MaxByte; AnimType: TacAnimType = atAero);
var
  dx, dy, l, t, r, b, trans, p: real;
  cy, cx, i, StepCount: integer;
  DstBmp, AnimBmp: TBitmap;
  FBlend: TBlendFunction;
  ObscForm, AnimForm: TacGlowForm;
  PrintDC, DC: hdc;
  acwPopupDiv: real;
  Flags: Cardinal;
  FBmpSize: TSize;
  lTicks: DWord;
  fR, cR: TRect;
  h: hwnd;

  procedure Anim_Init;
  begin
    trans := 0;
    p := MaxTransparency / (StepCount + 1);
    case AnimType of
      atDropDown: begin
        l := 0;
        t := 0;
        r := DstBmp.Width;
        b := 0;
        acwPopupDiv := exp(1 / StepCount * ln(DstBmp.Height div 2 {finish size}));
        dx := (DstBmp.Width  - r) / acwPopupDiv;
        dy := (DstBmp.Height - b) / acwPopupDiv;
      end;

      atAero: begin
        l := DstBmp.Width / acwDivider;
        t := DstBmp.Height / acwDivider;
        dx := l / StepCount;
        dy := t / StepCount;
        r := DstBmp.Width - l;
        b := DstBmp.Height - t;
      end

      else begin
        dx := 0;
        dy := 0;
        l := 0;
        t := 0;
        r := 0;
        b := 0;
      end;
    end
  end;

  procedure Anim_DoNext;
  begin
    trans := min(trans + p, MaxTransparency);
    FBlend.SourceConstantAlpha := Round(trans);
    case AnimType of
      atDropDown: begin
        FBlend.SourceConstantAlpha := MaxByte;
        if (l < 0) or (t < 0) then
          BitBlt(AnimBmp.Canvas.Handle, 0, 0, DstBmp.Width, DstBmp.Height, DstBmp.Canvas.Handle, 0, 0, SRCCOPY)
        else
          StretchBlt(AnimBmp.Canvas.Handle, Round(l), Round(t), Round(r - l), Round(b - t), DstBmp.Canvas.Handle, 0, 0, DstBmp.Width, DstBmp.Height, SRCCOPY);
      end;

      atAero:
        if (l < 0) or (t < 0) then
          BitBlt(AnimBmp.Canvas.Handle, 0, 0, DstBmp.Width, DstBmp.Height, DstBmp.Canvas.Handle, 0, 0, SRCCOPY)
        else
          StretchBlt(AnimBmp.Canvas.Handle, Round(l), Round(t), Round(r - l), Round(b - t), DstBmp.Canvas.Handle, 0, 0, DstBmp.Width, DstBmp.Height, SRCCOPY);

      else
        if l = 0 then begin
          BitBlt(AnimBmp.Canvas.Handle, 0, 0, DstBmp.Width, DstBmp.Height, DstBmp.Canvas.Handle, 0, 0, SRCCOPY);
          l := 1;
        end
    end
  end;

  function Anim_GoToNext: boolean;
  begin
    Result := True;
    case AnimType of
      atDropDown: begin
        l := 0;
        t := 0;
        r := DstBmp.Width - dx;
        b := DstBmp.Height - dy;
        dx := dx / acwPopupDiv;
        dy := dy / acwPopupDiv;
        if (dx < 2) and (dy < 2) then
          Result := False;
      end;

      atAero: begin
        l := l - dx;
        t := t - dy;
        r := r + dx;
        b := b + dy;
      end
    end
  end;

begin
  InAnimationProcess := True;
  with TacAccessDialogWnd(ListSW) do begin
    if BorderForm <> nil then
      AnimForm := BorderForm.AForm
    else
      AnimForm := TacGlowForm.CreateNew(Application);

    PaintAll;
    GetClientRect(CtrlHandle, cR);
    DstBmp := CreateBmp32(SkinData.FCacheBmp);
    BitBlt(DstBmp.Canvas.Handle, 0, 0, DstBmp.Width, DstBmp.Height, SkinData.FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
    DstBmp.Canvas.Lock;
    PrintDC := DstBmp.Canvas.Handle;

    DC := SaveDC(PrintDC);
    fR.TopLeft := Point(OffsetX, OffsetY);
    MoveWindowOrg(PrintDC, fR.Left, fR.Top);
    IntersectClipRect(PrintDC, 0, 0, WidthOf(cR), HeightOf(cR));
    Provider.PrintHwndControls(CtrlHandle, PrintDC);

    if BorderForm <> nil then begin
      fR.TopLeft := Point(OffsetX, OffsetY);
      fR.Right := fR.Left + WidthOf(cR);
      fR.Bottom := fR.Top + HeightOf(cR);
      FillAlphaRect(DstBmp, fR, MaxByte);
    end
    else
      FillAlphaRect(DstBmp, MkRect(DstBmp), MaxByte);

    RestoreDC(PrintDC, DC);
    if DstBmp = nil then
      Exit;

    DstBmp.Canvas.UnLock;
    FBmpSize := MkSize(DstBmp);
    StepCount := wTime div acTimerInterval;
    Flags := SWP_NOACTIVATE or SWP_NOREDRAW or SWP_NOCOPYBITS or SWP_NOSENDCHANGING or SWP_NOOWNERZORDER;
    if StepCount > 0 then
      InitBlendData(FBlend, 0)
    else
      InitBlendData(FBlend, MaxTransparency);

    if BorderForm <> nil then begin
      cy := SkinTitleHeight(BorderForm) + ShadowSize.Top - CaptionHeight(False) - SysBorderWidth(CtrlHandle, BorderForm, False);
      cx := SkinBorderWidth(BorderForm) - SysBorderWidth(CtrlHandle, BorderForm, False) + ShadowSize.Left;
    end
    else begin
      cy := 0;
      cx := 0;
    end;
    GetWindowRect(CtrlHandle, fR);
    if GetWindowLong(CtrlHandle, GWL_EXSTYLE) and WS_EX_TOPMOST <> 0 then begin
      TForm(AnimForm).FormStyle := fsStayOnTop;
      h := HWND_TOPMOST;
    end
    else
      h := CtrlHandle;

    if BorderForm = nil then begin
      FillDlgArOR(ListSW);
      CleanPixelsByRects(ArOR, DstBmp);
    end;

    // Make the obscuring form
    if acIsDPIAware and not IsFluentOS and IsWinVistaUp {$IFNDEF FLUENTCOMP}and Assigned(BeginBufferedPaint){$ENDIF}{AeroIsEnabled} then
      ObscForm := MakeCoverForm(AnimForm.Handle)
    else
      ObscForm := nil;

    SetWindowLong(AnimForm.Handle, GWL_EXSTYLE, GetWindowLong(AnimForm.Handle, GWL_EXSTYLE) or WS_EX_LAYERED or WS_EX_NOACTIVATE);
    AnimForm.SetBounds(fR.Left - cx, fR.Top - cy, DstBmp.Width, DstBmp.Height);
    UpdateLayeredWnd(AnimForm.Handle, DstBmp, @FBmpSize, @FBlend);
    ShowWindow(AnimForm.Handle, SW_SHOWNOACTIVATE);

    SetWindowPos(AnimForm.Handle, h, AnimForm.Left, AnimForm.Top, FBmpSize.cx, FBmpSize.cy, Flags or SWP_NOREDRAW);
    AnimBmp := CreateBmp32(FBmpSize);
    FillDC(AnimBmp.Canvas.Handle, MkRect(AnimBmp), 0);
    SetStretchBltMode(AnimBmp.Canvas.Handle, COLORONCOLOR);

    if StepCount > 0 then begin
      Anim_Init;
      i := 0;
      while i <= StepCount do begin
        lTicks := GetTickCount;
        Anim_DoNext;
        UpdateLayeredWnd(AnimForm.Handle, AnimBmp, @FBmpSize, @FBlend);
        if not Anim_GoToNext then
          Break;

        inc(i);
        if StepCount > 0 then
          WaitTicks(lTicks);
      end;
      FBlend.SourceConstantAlpha := MaxTransparency;
    end;
    SetWindowRgn(AnimForm.Handle, 0, False);
    SetWindowPos(AnimForm.Handle, 0, 0, 0, 0, 0, Flags or SWP_NOSIZE or SWP_NOMOVE);
    UpdateLayeredWnd(AnimForm.Handle, DstBmp, @FBmpSize, @FBlend);

    if acIsDPIAware and IsFluentOS then
      ObscForm := MakeCoverForm(AnimForm.Handle);

    FreeAndNil(AnimBmp);
    acDials.UpdateRgn(ListSW, False);

    SetWindowPos(CtrlHandle, AnimForm.Handle, 0, 0, 0, 0, SWPA_ZORDER);
    SetWindowLong(CtrlHandle, GWL_STYLE, GetWindowLong(CtrlHandle, GWL_STYLE) or WS_VISIBLE); // Form must be visible

    ProcessMessage(WM_SETREDRAW, 1); // Vista

    InAnimationProcess := False;

    if BorderForm <> nil then
      BorderForm.ExBorderShowing := True;

    SkinData.BeginUpdate;
    UpdateLayerVisibility(nil, CtrlHandle);
    SkinData.EndUpdate;
    RedrawWindow(CtrlHandle, nil, 0, RDWA_ALLNOW);
  //    if AeroIsEnabled then
  //      Sleep(4 * acTimerInterval); // Blinking in Aero removing

    if BorderForm <> nil then begin
      acDials.UpdateRgn(ListSW, False);
      BorderForm.ExBorderShowing := False;
// Desc 15    BorderForm.UpdateExBordersPos(True); // Guarantees that client area will be visible
      SetWindowPos(AnimForm.Handle, CtrlHandle, 0, 0, 0, 0, Flags or SWP_NOSIZE or SWP_NOMOVE);
      acDials.UpdateRgn(ListSW, False{True}); // Guarantees that region is updated
    end
    else begin
      SetWindowPos(AnimForm.Handle, CtrlHandle, 0, 0, 0, 0, Flags or SWP_NOSIZE or SWP_NOMOVE);
      FreeAndNil(AnimForm);
    end;

    // Destroy the obscuring form
    if ObscForm <> nil then begin
      Sleep(20); // Handle all painting messages for prevent of blinking
      ObscForm.Free;
    end;
  end;
  FreeAndNil(DstBmp);
end;


procedure PrepareForAnimation(const Ctrl: TWinControl; AnimType: TacAnimTypeCtrl = atcFade);
begin
  acAnimation.PrepareForAnimation(Ctrl, AnimType);
end;


procedure AnimShowControl(Ctrl: TWinControl; wTime: word = 0; MaxTransparency: integer = MaxByte; AnimType: TacAnimTypeCtrl = atcFade);
begin
  acAnimation.AnimShowControl(Ctrl, wTime, MaxTransparency, AnimType);
end;

(*
procedure SumBmpRect32(BmpTo: TBitmap; RectTo: TRect; BmpFrom: TBitmap; PointFrom: TPoint; BlendValue: byte = MaxByte);
var
  DeltaS, DeltaD, Y, X, Y1, X1: integer;
  D, D0: PRGBAArray_D;
  S, S0: PRGBAArray_;
  hh, ww: integer;
  aa_, aa: byte;
begin
  // Correct Left
  if RectTo.Left < 0 then begin
    PointFrom.X := PointFrom.X - RectTo.Left;
    RectTo.Left := 0;
  end;
  if PointFrom.X < 0 then begin
    RectTo.Left := RectTo.Left - PointFrom.X;
    PointFrom.X := 0;
  end;
  // Correct Top
  if RectTo.Top < 0 then begin
    PointFrom.Y := PointFrom.Y - RectTo.Top;
    RectTo.Top := 0;
  end;
  if PointFrom.Y < 0 then begin
    RectTo.Top := RectTo.Top - PointFrom.Y;
    PointFrom.Y := 0;
  end;
  // Correct width
  ww := WidthOf(RectTo, True);
  if RectTo.Left + ww > BmpTo.Width then begin
    ww := BmpTo.Width - RectTo.Left;
    RectTo.Right := RectTo.Left + ww;
  end;
  if PointFrom.X + ww > BmpFrom.Width then begin
    ww := BmpFrom.Width - PointFrom.X;
    RectTo.Right := RectTo.Left + ww;
  end;
  if ww > 0 then begin
    // Correct height
    hh := HeightOf(RectTo, True);
    if RectTo.Top + hh > BmpTo.Height then begin
      hh := BmpTo.Height - RectTo.Top;
      RectTo.Bottom := RectTo.Top + hh;
    end;
    if PointFrom.Y + hh > BmpFrom.Height then begin
      hh := BmpFrom.Height - PointFrom.Y;
      RectTo.Bottom := RectTo.Top + hh;
    end;
    if hh > 0 then begin
      Y1 := PointFrom.Y;
      dec(RectTo.Bottom);
      dec(RectTo.Right);
      if InitLine(BmpFrom, Pointer(S0), DeltaS) and InitLine(BmpTo, Pointer(D0), DeltaD) then
        for Y := RectTo.Top to RectTo.Bottom do begin
          D := Pointer(PAnsiChar(D0) + DeltaD * Y);
          S := Pointer(PAnsiChar(S0) + DeltaS * Y1);
          X1 := PointFrom.X;
          for X := RectTo.Left to RectTo.Right do begin
            with S[X1], D[X] do
              if A <> 0 then begin
                aa := (A * BlendValue) shr 8;
                aa_ := MaxByte - aa;
                DR := (DR * aa_ + R * aa) shr 8;
                DG := (DG * aa_ + G * aa) shr 8;
                DB := (DB * aa_ + B * aa) shr 8;
              end;

            inc(X1);
          end;
          inc(Y1);
        end;
    end;
  end;
end;
*)

procedure SetParentUpdated(const wc: TWinControl); overload;
var
  i: integer;
begin
  if not (csDesigning in wc.ComponentState) and not InAnimationProcess then
    for i := 0 to wc.ControlCount - 1 do
      if not (csDestroying in wc.Controls[i].ComponentState) then
        if wc.Controls[i] is TWinControl then begin
          if TWinControl(wc.Controls[i]).HandleAllocated and TWinControl(wc.Controls[i]).Showing then
            TrySendMessage(TWinControl(wc.Controls[i]).Handle, SM_ALPHACMD, AC_ENDPARENTUPDATE_HI, 0);
        end
        else
          if wc.Controls[i] is TControl then
            TControl(wc.Controls[i]).Perform(SM_ALPHACMD, AC_ENDPARENTUPDATE_HI, 0);
end;


procedure SetParentUpdated(const pHwnd: hwnd); overload;
var
  hCtrl: THandle;
begin
  hCtrl := GetTopWindow(pHwnd);
  while hCtrl <> 0 do begin
    if GetWindowLong(hCtrl, GWL_STYLE) and WS_CHILD <> 0 then
      SendAMessage(hCtrl, AC_ENDPARENTUPDATE);

    hCtrl := GetNextWindow(hCtrl, GW_HWNDNEXT);
  end;
end;


procedure ChangeControlColors(AControl: TControl; AFontColor, AColor: TColor); // clNone will reset a color to default
var
  M: TMessage;
  SkinData: TsCommonData;
begin
  SkinData := TsCommonData(AControl.Perform(SM_ALPHACMD, AC_GETSKINDATA_HI, 0));
  if SkinData <> nil then begin
    SkinData.CustomFont := AFontColor <> clNone;
    SkinData.CustomColor := AColor <> clNone;
  end;
  if AFontColor <> clNone then
    TacAccessControl(AControl).Font.Color := acColorToRGB(AFontColor);

  if AColor <> clNone then begin
    TacAccessControl(AControl).Color := acColorToRGB(AColor);
    if AControl is TWinControl then
      SetBkColor(TWinControl(AControl).Handle, acColorToRGB(AColor));

    AControl.Perform(SM_ALPHACMD, WPARAM(AC_INVALIDATE shl 16) + 1, 0);
  end;
  if (AControl is TWinControl) and (TWinControl(AControl).ControlCount > 0) then begin
    M := MakeMessage(SM_ALPHACMD, AC_SETBGCHANGED_HI + 1, 0, 0);
    AlphaBroadCast(TWinControl(AControl), M);
    InvalidateRect(TWinControl(AControl).Handle, nil, False);
  end;
end;


function GetControlColor(const Control: TControl): TColor;
begin
  Result := clFuchsia;
  if Control <> nil then
    if CtrlIsSkinned(Control, True) then
      Result := ColorToRGB(Control.Perform(SM_ALPHACMD, AC_GETCONTROLCOLOR_HI, Result))
    else
      Result := ColorToRGB(TacAccessControl(Control).Color); // message is not supported by parent control
end;


function GetControlColor(const Handle: THandle): TColor; overload;
begin
  Result := clFuchsia;
  if Handle <> 0 then
    Result := ColorToRGB(TrySendMessage(Handle, SM_ALPHACMD, AC_GETCONTROLCOLOR_HI, Result));
end;


function GetControlFontColor(const Control: TControl; SkinManager: TObject): TColor;
var
  SkinIndex: integer;
  SM: TsSkinManager;
begin
  if SkinManager = nil then
    SM := DefaultManager
  else
    SM := TsSkinManager(SkinManager);

  SkinIndex := GetFontIndex(Control, -1, SM);
  if SM.IsActive then
    if SkinIndex >= 0 then
      Result := SM.CommonSkinData.gd[SkinIndex].Props[0].FontColor.Color
    else
      Result := TacAccessControl(Control.Parent).Font.Color // SM.Palette[pcLabelText]
  else
    Result := 0;
end;


function IsCustomFont(Ctrl: TControl; AFont: TFont; SaveColor: boolean = True): boolean;
var
  f: TFont;
begin
  if (Ctrl <> nil) and (TAccessWinControl(Ctrl).Parent <> nil) then
    if not SaveColor then begin
      F := TFont.Create;
      try
        F.Assign(TAccessWinControl(Ctrl.Parent).Font);
        F.Color := TAccessWinControl(Ctrl).Font.Color;
        Result := ((Ctrl.Parent is TCustomFrame) and not TAccessWinControl(Ctrl).ParentFont) or not FontsEqual(F, AFont);
      finally
        F.Free;
      end;
    end
    else
      Result := ((Ctrl.Parent is TCustomFrame) and not TAccessWinControl(Ctrl).ParentFont) or not FontsEqual(TAccessWinControl(Ctrl.Parent).Font, AFont)
  else begin
    F := TFont.Create;
    try
      if not SaveColor then
        F.Color := AFont.Color;

      Result := not FontsEqual(F, AFont);
    finally
      F.Free;
    end;
  end;
end;


function AllEditSelected(Ctrl: TCustomEdit): Boolean;
type
  TSelection = record
    StartPos,
    EndPos: Integer;
  end;

var
  Selection: TSelection;
begin
  SendMessage(Ctrl.Handle, EM_GETSEL, WPARAM(@Selection.StartPos), LPARAM(@Selection.EndPos));
  Result := (Selection.EndPos = Ctrl.GetTextLen) and (Ctrl.SelLength = Length(Ctrl.Text));
end;


type
  TAccessGraphicCtrl = class(TGraphicControl);


procedure PaintControls(DC: HDC; OwnerControl: TWinControl; ChangeCache: boolean; Offset: TPoint; {AHandle: THandle = 0; }CheckVisible: boolean = True);
var
  SaveIndex: hdc;
  R, bRect: TRect;
  tDC, MemDC: HDC;
  BGInfo: TacBGInfo;
  MemDCExists: boolean;
  I, J, d, Count: Integer;
  MemBitmap, OldBitmap: HBITMAP;

  function ControlIsReady(Control: TControl): boolean;
  begin
    with Control do begin
      Result := (Control is TGraphicControl) and (Visible or (csDesigning in ComponentState)) and (Width > 0) and (Height > 0);
      Result := Result and not (csNoDesignVisible in ControlStyle) and not (csDestroying in ComponentState) and
                not ((Control is TToolButton) and (TToolButton(Control).Style in [tbsCheck, tbsButton, tbsDropDown]));

      Result := Result and RectVisible(DC, BoundsRect);
    end;
  end;

begin
  if not acGraphPainting then begin
    acGraphPainting := True;
    MemDCExists := False;
    MemDC := 0;
    MemBitmap := 0;
    OldBitmap := 0;
    if (OwnerControl.Visible or (csDesigning in OwnerControl.ComponentState) or not CheckVisible) and (OwnerControl.ControlCount > 0) then
      try
        if (GetClipBox(DC, R) = NULLREGION) or (R.Left = R.Right) or (R.Top = R.Bottom) then begin
          SendAMessage(OwnerControl.Handle, AC_SETHALFVISIBLE);
          acGraphPainting := False;
          Exit;
        end;
        BGInfo.BgType := btUnknown;
        BGInfo.PleaseDraw := False;
        GetBGInfo(@BGInfo, OwnerControl);
        Count := OwnerControl.ControlCount;
        I := 0;
        while I < Count do begin
          if ControlIsReady(OwnerControl.Controls[I]) and RectInRect(OwnerControl.Controls[I].BoundsRect, R, False) then begin
            if OwnerControl is TForm then
              with TForm(OwnerControl) do
                if (FormStyle = fsMDIForm) and (Controls[I].Align <> alNone) and (Controls[I] is TGraphicControl) then
                  Controls[I].Perform(SM_ALPHACMD, AC_INVALIDATE shl 16, 0);

            if not MemDCExists then begin // Make BG image
              tDC := GetDC(0);
              MemBitmap := CreateCompatibleBitmap(tDC, OwnerControl.Width, OwnerControl.Height);
              ReleaseDC(0, tDC);
              MemDC := CreateCompatibleDC(0);
              OldBitmap := SelectObject(MemDC, MemBitmap);
              MemDCExists := True;
              for j := 0 to Count - 1 do // Copy parent BG
                with OwnerControl, Controls[J] do
                  if RectInRect(Controls[J].BoundsRect, R, False) and ControlIsReady(Controls[J]) then
                    if not (csOpaque in ControlStyle) or (Controls[J] is TGraphicControl) {// Desc 3} then
                      if (BGInfo.BgType = btCache) and (BGInfo.Bmp <> nil) then // If cache exists
                        BitBlt(MemDC, Left, Top, Width, Height, BGInfo.Bmp.Canvas.Handle, Left + BGInfo.Offset.X, Top + BGInfo.Offset.Y, SRCCOPY)
                      else begin
                        if BGInfo.Bmp <> nil then begin
                          bRect := BoundsRect;
                          FillDC(MemDC, bRect, BGInfo.Color);

                          if bRect.Top < BGInfo.FillRect.Top then
                            BitBlt(MemDC, bRect.Left, bRect.Top, Width, BGInfo.FillRect.Top - bRect.Top, BGInfo.Bmp.Canvas.Handle, bRect.Left + BGInfo.Offset.X, bRect.Top + BGInfo.Offset.Y, SRCCOPY);

                          if bRect.Left < BGInfo.FillRect.Left then
                            BitBlt(MemDC, bRect.Left, bRect.Top, BGInfo.FillRect.Left - bRect.Left, Height, BGInfo.Bmp.Canvas.Handle, bRect.Left + BGInfo.Offset.X, bRect.Top + BGInfo.Offset.Y, SRCCOPY);

                          if (bRect.Bottom + BGInfo.Offset.Y > (BGInfo.Bmp.Height - BGInfo.FillRect.Bottom)) and (BGInfo.Offset.Y >= 0 {Not scrolled}) then begin
                            d := bRect.Bottom - (BGInfo.Bmp.Height - BGInfo.FillRect.Bottom);
                            BitBlt(MemDC, bRect.Left, bRect.Bottom - d, Width, d, BGInfo.Bmp.Canvas.Handle, bRect.Left + BGInfo.Offset.X, bRect.Bottom + BGInfo.Offset.Y - d, SRCCOPY);
                          end;

                          if (bRect.Right + BGInfo.Offset.X > (BGInfo.Bmp.Width - BGInfo.FillRect.Right)) and (BGInfo.Offset.X >= 0 {Not scrolled}) then begin
                            d := bRect.Right - (BGInfo.Bmp.Width - BGInfo.FillRect.Right);
                            BitBlt(MemDC, bRect.Right - d, bRect.Top, d, Height, BGInfo.Bmp.Canvas.Handle, bRect.Right + BGInfo.Offset.X - d, bRect.Top + BGInfo.Offset.Y, SRCCOPY);
                          end;
                        end
                        else
                          FillDC(MemDC, BoundsRect, BGInfo.Color);
                      end
                    else
                      FillDC(MemDC, BoundsRect, GetControlColor(Controls[J]));
            end;
            SaveIndex := SaveDC(MemDC);
            if not RectVisible(DC, OwnerControl.Controls[I].BoundsRect) then
              SendAMessage(OwnerControl.Controls[I], AC_SETHALFVISIBLE);

            MoveWindowOrg(MemDC, OwnerControl.Controls[I].Left, OwnerControl.Controls[I].Top);
            IntersectClipRect(MemDC, 0, 0, OwnerControl.Controls[I].Width, OwnerControl.Controls[I].Height);

            if csPaintCopy in OwnerControl.ControlState then
              OwnerControl.Controls[I].ControlState := OwnerControl.Controls[I].ControlState + [csPaintCopy];

            try // Errors in dialogs are possible
              if (csDesigning in OwnerControl.ComponentState) and
{$IFNDEF FIXGRAPHERROR}
                (OwnerControl.Controls[I] is TSpeedButton) then
{$ELSE}
                (OwnerControl.Controls[I] is TGraphicControl) and not (OwnerControl.Controls[I] is TImage) then
{$ENDIF}
                  with TAccessGraphicCtrl(OwnerControl.Controls[I]) do begin
                    Canvas.Lock;
                    Canvas.Handle := MemDC;
                    try
                      Paint;
                    finally
                      Canvas.Handle := 0;
                      Canvas.Unlock;
                    end;
                  end
              else
                OwnerControl.Controls[I].Perform(WM_PAINT, WPARAM(MemDC), 0);
            finally
              if csPaintCopy in OwnerControl.ControlState then
                OwnerControl.Controls[I].ControlState := OwnerControl.Controls[I].ControlState - [csPaintCopy];

              MoveWindowOrg(MemDC, - OwnerControl.Controls[I].Left, - OwnerControl.Controls[I].Top);
              RestoreDC(MemDC, SaveIndex);
            end;
          end;
          inc(i);
        end;
        if MemDCExists then begin
          J := 0;
          while J < Count do begin // Copy graphic controls
            if ControlIsReady(OwnerControl.Controls[J]) and RectInRect(OwnerControl.Controls[J].BoundsRect, R, False) then
              with OwnerControl.Controls[J] do
                if GetPixel(MemDC, Left + Offset.X, Top + Offset.Y) <> DWord(clFuchsia) then
                  BitBlt(DC, Left + Offset.X, Top + Offset.Y, Width, Height, MemDC, Left, Top, SRCCOPY);

            inc(J);
          end;
        end;
      finally
        if MemDCExists then begin
          SelectObject(MemDC, OldBitmap);
          DeleteDC(MemDC);
          DeleteObject(MemBitmap);
        end;
      end;

    acGraphPainting := False;
  end;
end;


procedure PaintParentBG(AControl: TControl; ABitmap: TBitmap);
var
  BGInfo: TacBGInfo;
begin
  if (AControl <> nil) and (ABitmap <> nil) and (AControl.Parent <> nil) then begin
    BGInfo.PleaseDraw := False;
    GetBGInfo(@BGInfo, AControl.Parent.Handle);
    ABitmap.PixelFormat := pf32bit;
    ABitmap.Width  := AControl.Width;
    ABitmap.Height := AControl.Height;
    case BGInfo.BgType of
      btCache:
        BitBlt(ABitmap.Canvas.Handle, 0, 0, ABitmap.Width, ABitmap.Height, BGInfo.Bmp.Canvas.Handle, BGInfo.Offset.X + AControl.Left, BGInfo.Offset.Y + AControl.Top, SRCCOPY);

      btFill:
        FillDC(ABitmap.Canvas.Handle, MkRect(ABitmap), BGInfo.Color)

      else
        FillDC(ABitmap.Canvas.Handle, MkRect(ABitmap), GetControlColor(AControl));
    end;
  end;
end;


procedure SetRedraw(Handle: THandle; Value: integer = 0); overload;
begin
  TrySendMessage(Handle, WM_SETREDRAW, Value, 0);
end;


procedure SetRedraw(Ctrl: TGraphicControl; Value: integer = 0); overload;
begin
  Ctrl.Perform(WM_SETREDRAW, Value, 0);
end;


function SendAMessage(const Handle: hwnd; const Cmd: Integer; LParam: LPARAM = 0): LRESULT; overload;
begin
  Result := TrySendMessage(Handle, SM_ALPHACMD, WPARAM(Word(Cmd)) shl 16, LParam);
end;


function SendAMessage(const Control: TControl; const Cmd: Integer; LParam: LPARAM = 0): LRESULT; overload;
begin
  Result := TrySendMessage(Control, SM_ALPHACMD, WPARAM(Word(Cmd)) shl 16, LParam);
end;


function TrySendMessage(const Control: TControl; var Message: TMessage): LRESULT; overload;
var
  sd: TsCommonData;
begin
  if Control is TWinControl then
    with TWinControl(Control) do
      if not (csDestroying in ComponentState) and HandleAllocated then begin
        sd := GetCommonData(Handle);
        if (sd <> nil) and Assigned(sd.WndProc) then begin
          sd.WndProc(Message);
          Result := Message.Result;
        end
        else
          Result := SendMessage(Handle, Message.Msg, Message.WParam, Message.LParam);
      end
      else
        Result := 0
  else
    Result := Control.Perform(Message.Msg, Message.WParam, Message.LParam);
end;


function TrySendMessage(const Control: TControl; Msg: Cardinal; WParam: WPARAM; LParam: LPARAM = 0): LRESULT;
var
  M: TMessage;
  sd: TsCommonData;
begin
  if Control is TWinControl then
    with TWinControl(Control) do
      if not (csDestroying in ComponentState) and HandleAllocated then begin
        sd := GetCommonData(Handle);
        if (sd <> nil) then begin
          if not Assigned(sd.WndProc) and (sd.FOwnerControl <> nil) then
            sd.WndProc := TWinControl(Control).WindowProc
          else begin
            Result := SendMessage(Handle, Msg, WParam, LParam);
            Exit;
          end;
          M := MakeMessage(Msg, WPAram, LParam, 0);
          sd.WndProc(M);
          Result := M.Result;
        end
        else
          Result := SendMessage(Handle, Msg, WParam, LParam);
      end
      else
        Result := 0
  else
    Result := Control.Perform(Msg, WParam, LParam);
end;


function TrySendMessage(AHandle: THandle; Msg: Cardinal; WParam: WPARAM; LParam: LPARAM = 0): LRESULT; overload;
var
  M: TMessage;
  sd: TsCommonData;
begin
  sd := GetCommonData(AHandle);
  if (sd <> nil) and Assigned(sd.WndProc) then begin
    M := MakeMessage(Msg, WPAram, LParam, 0);
    try
      sd.WndProc(M);
      Result := M.Result;
    except
      Result := 0;
    end
  end
  else
    Result := SendMessage(AHandle, Msg, WParam, LParam);
end;


function GetBoolMsg(const Control: TWinControl; const Cmd: Cardinal): boolean;
begin
  Result := boolean(SendAMessage(Control, Cmd));
end;


function GetBoolMsg(const CtrlHandle: hwnd; const Cmd: Cardinal): boolean; overload;
var
  LParam: Cardinal;
begin
  LParam := 0;
  if SendAMessage(CtrlHandle, WPARAM(Cmd), LParam) = 1 then
    Result := True
  else
    Result := LParam = 1;
end;


function ControlIsReady(const Control: TControl): boolean;
begin
  if (Control <> nil) and (Control.Parent <> nil) and (not (Control is TWinControl) or TWinControl(Control).HandleAllocated) then
    with Control do
      Result := ([csLoading, csDestroying] * ComponentState = []) and ([csCreating, csReadingState] * ControlState = [])
  else
    Result := False;
end;


function GetOwnerForm(const Component: TComponent): TCustomForm;
var
  c: TComponent;
begin
  Result := nil;
  c := Component;
  while Assigned(c) and not (c is TCustomForm) do
    c := c.Owner;

  if c is TCustomForm then
    Result := TCustomForm(c);
end;


function GetOwnerFrame(const Component: TComponent): TCustomFrame;
var
  c: TComponent;
begin
  Result := nil;
  c := Component;
  while Assigned(c) and not (c is TCustomFrame) do
    c := c.Owner;

  if c is TCustomFrame then
    Result := TCustomFrame(c);
end;


procedure SetControlsEnabled(Parent: TWinControl; Value: boolean; Recursion: boolean = False);
var
   i: integer;
begin
  for i := 0 to Parent.ControlCount - 1 do begin
      Parent.Controls[i].Enabled := Value;
      if Recursion and (Parent.Controls[i] is TWinControl) then
        SetControlsEnabled(TWinControl(Parent.Controls[i]), Value, True);
    end;
end;


function GetStringFlags(const Control: TControl; const al: TAlignment): Cardinal;
begin
{$IFDEF FPC}
  Result := DT_EXPANDTABS or DT_VCENTER or AlignToInt[al];
{$ELSE}
  Result := Control.DrawTextBiDiModeFlags(DT_EXPANDTABS or DT_VCENTER or AlignToInt[al]);
{$ENDIF}
end;


procedure RepaintsControls(const Owner: TWinControl);
var
  i: Integer;
begin
  i := 0;
  while i <= Owner.ControlCount - 1 do begin
    if ControlIsReady(Owner.Controls[i]) then
      if not (Owner.Controls[i] is TGraphicControl) then
        Owner.Controls[i].Invalidate;

    inc(i);
  end;
end;


procedure AlphaBroadCast(const Control: TWinControl; var Message);
var
  i: integer;
begin
  i := 0;
  while i < Control.ControlCount do
    with TMessage(Message) do begin
      if (i >= Control.ControlCount) or (csDestroying in Control.Controls[i].ComponentState) then
        Exit;

      if Control.Controls[i] is TWincontrol then
        with TWinControl(Control.Controls[i]) do
          if not HandleAllocated then
            Perform(Msg, Wparam, LParam)
          else begin
            TrySendMessage(Handle, Msg, WParam, LParam);
            if not WndIsSkinned(TWinControl(Control.Controls[i]).Handle) then
              if not Assigned(CheckDevEx) or not CheckDevEx(Control.Controls[i]) then
                AlphaBroadCast(TWinControl(Control.Controls[i]), Message);
          end
      else
        Control.Controls[i].Perform(Msg, Wparam, LParam);

      inc(i);
    end;
end;


type
  TacMsgInfo = record
    Sender: hwnd;
    Message: TMessage;
  end;

  PacMsgInfo = ^TacMsgInfo;


function SendToChildren(Child: HWND; Data: LParam): BOOL; stdcall;
var
  MsgI: TacMsgInfo;
begin
  MsgI := PacMsgInfo(Data)^;
  if GetParent(Child) = MsgI.Sender then
    TrySendMessage(Child, MsgI.Message.Msg, MsgI.Message.WParam, MsgI.Message.LParam);

  Result := True;
end;


procedure AlphaBroadCast(const Handle: hwnd; var Message); overload;
var
  MsgI: TacMsgInfo;
begin
  MsgI.Sender := Handle;
  MsgI.Message := TMessage(Message);
  EnumChildWindows(Handle, @SendToChildren, LPARAM(@MsgI));
end;


function acClientRect(const Handle: hwnd): TRect;
var
  R: TRect;
  P: TPoint;
begin
  GetWindowRect(Handle, R);
  P := MkPoint;
  ClientToScreen(Handle, P);
  GetClientRect(Handle, Result);
  OffsetRect(Result, P.X - R.Left, P.Y - R.Top);
end;


function acMouseInWnd(const Handle: hwnd): TPoint; overload;
begin
  with acMousePos do
    Result := acMouseInWnd(Handle, X, Y);
end;


function acMouseInWnd(const Handle: hwnd; X, Y: integer): TPoint;
var
  R: TRect;
begin
  GetWindowRect(Handle, R);
  Result := Point(X, Y);
  dec(Result.X, R.Left);
  dec(Result.Y, R.Top);
end;


function TrySetSkinSection(const Control: TControl; const SectionName: string): boolean;
var
  si: TacSectionInfo;
  Ndx: integer;
begin
  Result := False;
  if Control <> nil then
    if (Control is TLabel) and (Control.Tag and ExceptTag = 0) and (DefaultManager <> nil) and DefaultManager.Active then
      with DefaultManager do begin
        Ndx := GetSkinIndex(SectionName);
        if Ndx >= 0 then
          TLabel(Control).Font.Color := DefaultManager.CommonSkinData.gd[Ndx].Props[0].FontColor.Color
        else
          TLabel(Control).Font.Color := DefaultManager.Palette[pcLabelText];

        Result := True;
      end
    else begin
      si.siName := SectionName;
      Result := Control.Perform(SM_ALPHACMD, AC_SETSECTION shl 16, LPARAM(@si)) = 1;
    end;
end;


function GetWndClassName(const Hwnd: THandle): string;
var
  Buf: array [0..128] of char;
begin
  GetClassName(Hwnd, Buf, 128);
  Result := StrPas(Buf);
end;


constructor TOutputWindow.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'acOW' + IntToStr(GetTickCount);
  Caption := Name;
  Visible := False;
  Color := clFuchsia;
  Tag := ExceptTag;
end;


procedure TOutputWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    if (Parent = nil) and (ParentWindow = 0) then begin
      Params.Style := WS_POPUP;
      if (Owner is TWinControl) and (ACNativeUInt(GetWindowLong(TWinControl(Owner).Handle, GWL_EXSTYLE)) and WS_EX_TOPMOST <> 0) then
        Params.ExStyle := ExStyle or WS_EX_TOPMOST;

{$IFDEF FPC}
      WndParent := Application.MainFormHandle;
{$ELSE}
      WndParent := Application.Handle;
{$ENDIF}
    end;
end;


procedure TOutputWindow.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;


procedure TOutputWindow.WMNCPaint(var Message: TWmEraseBkgnd);
begin
  Message.Result := 0;
end;


function GetAlignShift(const Ctrl: TWinControl; const Align: TAlign; const GraphCtrlsToo: boolean = False): integer;
var
  i: integer;
begin
  Result := 0;
  with Ctrl do
    for i := 0 to ControlCount - 1 do
      if Controls[i].Visible and (Controls[i].Align = Align) and (GraphCtrlsToo or not (Controls[i] is TGraphicControl)) then
        case Align of
          alLeft, alRight: inc(Result, Controls[i].Width);
          alTop, alBottom: inc(Result, Controls[i].Height);
        end;
end;


function GetParentFormHandle(const CtrlHandle: hwnd): hwnd;
var
  ph: hwnd;
begin
  ph := GetParent(CtrlHandle);
  if ph = 0 then
    Result := CtrlHandle
  else
    Result := GetParentFormHandle(ph);
end;


procedure TacHideTimer.Anim_DoNext;
var
//  j: integer;
  NewRect: TRect;
begin
  Trans := max(Trans - p, 0);
  FBlend.SourceConstantAlpha := Round(trans);
  case AnimType of
    atDropDown:
      if SrcBmp.Height = b then
        BitBlt(DstBmp.Canvas.Handle, 0, 0, DstBmp.Width, DstBmp.Height, SrcBmp.Canvas.Handle, 0, 0, SRCCOPY)
      else begin
        FillRect32(DstBmp, MkRect(DstBmp), 0, 0);
        NewRect := Rect(Round(l), Round(t), Round(r - l), Round(b - t));
        acgpStretchRect(DstBmp, SrcBmp, NewRect, MkRect(SrcBmp));
{
        j := GetIntController.GetFormIndex(ParentWnd);
        if j >= 0 then
          if GetIntController.FormHandlers[j].ShadowForm is TacShadowForm then begin
            TacShadowForm(GetIntController.FormHandlers[j].ShadowForm).FUserBlend := FBlend.SourceConstantAlpha;
            TacShadowForm(GetIntController.FormHandlers[j].ShadowForm).SetNewPos(
              Form.Left + NewRect.Left, Form.Left + NewRect.Top, WidthOf(NewRect), HeightOf(NewRect),
              FBlend.SourceConstantAlpha);
          end;
}
      end;

    atAero:
      if (l < 0) or (t < 0) then
        BitBlt(DstBmp.Canvas.Handle, 0, 0, SrcBmp.Width, SrcBmp.Height, SrcBmp.Canvas.Handle, 0, 0, SRCCOPY)
      else begin
        FillDC(DstBmp.Canvas.Handle, MkRect(DstBmp), 0);
        SetStretchBltMode(DstBmp.Canvas.Handle, COLORONCOLOR);
        StretchBlt(DstBmp.Canvas.Handle, Round(l), Round(t), Round(r - l), Round(b - t), SrcBmp.Canvas.Handle, 0, 0, SrcBmp.Width, SrcBmp.Height, SRCCOPY);
      end

    else
      if l = 0 then begin
        BitBlt(DstBmp.Canvas.Handle, 0, 0, SrcBmp.Width, SrcBmp.Height, SrcBmp.Canvas.Handle, 0, 0, SRCCOPY);
        l := 1;
      end;
  end
end;


function TacHideTimer.Anim_GoToNext: boolean;
begin
  Result := True;
  case AnimType of
    atDropDown: begin
      b := b / acwPopupDiv;
      if b < 2 then
        Result := False;
    end;

    atAero: begin
      l := l - dx;
      t := t - dy;
      r := r + dx;
      b := b + dy;
    end
  end
end;


procedure TacHideTimer.Anim_Init;
{$IFDEF AC_NOSHADOW}
var
  j: integer;
{$ENDIF}
begin
  Trans := StartBlendValue;
  p := StartBlendValue / StepCount;
  case AnimType of
    atDropDown: begin
      l := 0;
      t := 0;
      r := SrcBmp.Width;
      b := SrcBmp.Height;
      dx := 0;
      acwPopupDiv := exp(1 / StepCount * ln(SrcBmp.Height div 2 {finish size}));
      dy := SrcBmp.Height / acwPopupDiv;
      p := 0;
{$IFDEF AC_NOSHADOW}
      j := GetIntController.GetFormIndex(ParentWnd);
      if j >= 0 then
        if GetIntController.FormHandlers[j].ShadowForm is TacShadowForm then
          TacShadowForm(GetIntController.FormHandlers[j].ShadowForm).HideWnd;
{$ENDIF}
    end;

    atAero: begin
      dx := -SrcBmp.Width / (StepCount * acwDivider);
      dy := -SrcBmp.Height / (StepCount * acwDivider);
      l := 0;
      t := 0;
      r := SrcBmp.Width;
      b := SrcBmp.Height;
    end

    else begin
      dx := 0;
      dy := 0;
      l := 0;
      t := 0;
      r := 0;
      b := 0;
    end;
  end
end;


procedure TacHideTimer.CallEvent;
var
  sp: TsSkinProvider;
begin
  if not EventCalled and (ParentWnd <> 0) then begin
    sp := TsSkinProvider(SendAMessage(ParentWnd, AC_GETPROVIDER));
    if (sp <> nil) and Assigned(sp.OnAfterAnimation) and not (csDestroying in sp.ComponentState) then begin
      EventCalled := True;
      sp.OnAfterAnimation(aeHiding);
    end;
  end;
end;


constructor TacHideTimer.Create(AOwner: TComponent);
begin
  ParentWnd := 0;
  EventCalled := False;
  inherited;
end;


destructor TacHideTimer.Destroy;
begin
  inherited;
  CallEvent;
  FreeAndNil(SrcBmp);
  FreeAndNil(DstBmp);
  FreeAndNil(Form);
end;


procedure TacHideTimer.OnTimerProc(Sender: TObject);
var
  ToContinue: boolean;
begin
  if Form <> nil then begin
    if i <= StepCount then begin
      Anim_DoNext;
      if (Form <> nil) and Form.HandleAllocated then
        UpdateLayeredWnd(Form.Handle, DstBmp, @FBmpSize, @FBlend);

      ToContinue := Anim_GoToNext;
      inc(i);
    end
    else
      ToContinue := False;

    if not ToContinue then begin
      i := StepCount + 1;
      Enabled := False;
      CallEvent;
      OnTimer := nil;
      Interval := MaxWord;
      FreeAndNil(Form);
      FreeAndNil(SrcBmp);
      FreeAndNil(DstBmp);
    end;
  end;
end;


procedure SetFormBlendValue(FormHandle: THandle; Bmp: TBitmap; Value: integer; NewPos: PPoint = nil);
var
  R: TRect;
  b: boolean;
  FBmpSize: TSize;
  FBlend: TBlendFunction;
begin
  if not GetWindowRect(FormHandle, R) then
    CheckLastError;

  if Bmp = nil then begin
    Bmp := CreateBmp32(R);
    b := True;
  end
  else
    b := False;

  InitBlendData(FBlend, Value);
  FBmpSize := MkSize(Bmp);

  if GetWindowLong(FormHandle, GWL_EXSTYLE) and WS_EX_LAYERED = 0 then begin
    if not AeroIsEnabled then
      if SetWindowLong(FormHandle, GWL_STYLE, GetWindowLong(FormHandle, GWL_STYLE) and not WS_VISIBLE) = 0 then
        CheckLastError;

    if SetWindowLong(FormHandle, GWL_EXSTYLE, GetWindowLong(FormHandle, GWL_EXSTYLE) or WS_EX_LAYERED) = 0 then
      CheckLastError;

    if not AeroIsEnabled then begin
      RedrawWindow(0, @R, 0, RDW_INVALIDATE or RDW_UPDATENOW or RDW_ALLCHILDREN);
      if SetWindowLong(FormHandle, GWL_STYLE, GetWindowLong(FormHandle, GWL_STYLE) or WS_VISIBLE) = 0 then
        CheckLastError;
    end;
  end;
  if SetWindowLong(FormHandle, GWL_EXSTYLE, GetWindowLong(FormHandle, GWL_EXSTYLE) or WS_EX_LAYERED) = 0 then
    CheckLastError;

  UpdateLayeredWnd(FormHandle, Bmp, @FBmpSize, @FBlend, NewPos);
  if b and (Bmp <> nil) then
    FreeAndNil(Bmp);
end;


function GetShiftState: TShiftState;
begin
  Result := [];
  if GetAsyncKeyState(VK_SHIFT) < 0 then
    Include(Result, ssShift);

  if GetAsyncKeyState(VK_CONTROL) < 0 then
    Include(Result, ssCtrl);

  if GetAsyncKeyState(VK_MENU) < 0 then
    Include(Result, ssAlt);
end;


function GetWndText(hwnd: THandle): WideString;
var
  buf: array [0..1000] of char;
begin
  Result := '';
  if Win32Platform = VER_PLATFORM_WIN32_NT then begin
    SetLength(Result, GetWindowTextLengthW(hwnd) + 1);
    GetWindowTextW(hwnd, PWideChar(Result), Length(Result));
    SetLength(Result, Length(Result) - 1);
  end
  else begin
    SendMessage(hwnd, WM_GETTEXT, 1000, LPARAM(@buf));

    Result := StrPas(buf);
  end;
end;


function GetPlainText(HtmlText: acString; ConvertBreakLine: boolean = False): acString;
var
  c: acChar;
  Tag: acString;
  i, OpenedCount, TagStart: integer;
begin
  Result := '';
  OpenedCount := 0;
  TagStart := 0;
  for i := 1 to Length(HtmlText) do begin
    c := HtmlText[i];
    if OpenedCount > 0 then begin // Search end of Tag
      if c = '>' then begin
        dec(OpenedCount);
        if OpenedCount = 0 then begin
          Tag := Copy(HtmlText, TagStart, i - TagStart + 1);
          if ConvertBreakLine and SameText(Tag, '<br>') then
            Result := Result + s_0D0A;
        end;
      end;
    end
    else
      case c of
        '<': begin
          if OpenedCount = 0 then
            TagStart := i;

          inc(OpenedCount);
        end;
        #$0D, #$0A: // Skip
        else
          Result := Result + c;
      end;
  end;
end;


procedure ResetLastError;
begin
{$IFDEF ACDEBUG}
  SetLastError(0);
{$ENDIF}
end;


procedure CheckLastError;
{$IFDEF ACDEBUG}
var
  lError: Cardinal;
{$ENDIF}
begin
{$IFDEF ACDEBUG}
  lError := GetLastError;
  if not (lError in [0]) then
//    lError := 0;
    ShowError(SysErrorMessage(lError));
{$ENDIF}
end;


procedure ReflectControls(ParentWnd: TWinControl; Recursion: boolean);
var
  i: integer;
  LeftCtrl: TControl;

  procedure ReflectAnchors(Ctrl: TControl);
  begin
    if (akLeft in Ctrl.Anchors) and not (akRight in Ctrl.Anchors) then
      Ctrl.Anchors := Ctrl.Anchors + [akRight] - [akLeft]
    else
      if (akRight in Ctrl.Anchors) and not (akLeft in Ctrl.Anchors) then
        Ctrl.Anchors := Ctrl.Anchors + [akLeft] - [akRight];
  end;

  function LeftMostIndex(Ctrl: TWinControl): integer;
  var
    i, l: integer;
  begin
    Result := -1;
    with ParentWnd do begin
      l := Width + 1;
      for i := 0 to ControlCount - 1 do
        if (Controls[i].Align = alLeft) and (Controls[i].Left < l) and ((LeftCtrl = nil) or (Controls[i].Left < LeftCtrl.Left)) then begin
          l := Controls[i].Left;
          Result := i;
        end;
    end;
  end;

  function RightMostIndex(Ctrl: TWinControl): integer;
  var
    i, l: integer;
  begin
    Result := -1;
    l := -1;
    with ParentWnd do
      for i := 0 to ControlCount - 1 do
        if (Controls[i].Align = alRight) and (Controls[i].Left > l) then begin
          l := Controls[i].Left;
          Result := i;
        end;
  end;

begin
  if not (ParentWnd is TToolBar {Not supported}) then begin
{$IFNDEF ALITE}
    if ParentWnd is TsStatusBar then
      TsStatusBar(ParentWnd).ReflectPanels;
{$ENDIF}
    with ParentWnd do begin
      HandleNeeded;
      LeftCtrl := nil;
      for i := 0 to ControlCount - 1 do begin
        if Controls[i].Align = alNone then begin
          Controls[i].Left := ClientWidth - Controls[i].Left - Controls[i].Width;
          ReflectAnchors(Controls[i]);
        end;
        Controls[i].Perform(SM_ALPHACMD, MakeWParam(0, AC_REFLECTLAYOUT), 0);
      end;
      // Change right aligning to left
      while True do begin
        i := RightMostIndex(ParentWnd);
        if i >= 0 then begin
          Controls[i].Align := alLeft;
          if LeftCtrl = nil then
            LeftCtrl := Controls[i]; // Remember first changed control
        end
        else
          Break;
      end;
      // Change left aligning to right
      while True do begin
        i := LeftMostIndex(ParentWnd);
        if i >= 0 then
          Controls[i].Align := alRight
        else
          Break;
      end;
      // Doing recursion
      if Recursion then
        for i := 0 to ControlCount - 1 do
          if Controls[i] is TWinControl then
            ReflectControls(TWinControl(Controls[i]), True);
    end;
  end;
end;


{$IFDEF DELPHI7UP}
function acThemesEnabled: boolean;
begin
{$IFDEF DELPHI_XE2}
  Result := StyleServices.Enabled;
  acThemeServices := StyleServices;
{$ELSE}
  Result := ThemeServices.ThemesEnabled;
  acThemeServices := ThemeServices;
{$ENDIF}
end;
{$ENDIF}

{$IFNDEF DELPHI_10}
var
  lLib: HModule = 0;
{$ENDIF}

initialization
{$IFDEF DELPHI_XE2}
  acThemeServices := StyleServices;
{$ELSE}
  {$IFDEF DELPHI7UP}
  acThemeServices := ThemeServices;
  {$ENDIF}
{$ENDIF}
  uxthemeLib := LoadLibrary('UXTHEME');
  if uxthemeLib <= HINSTANCE_ERROR then
    uxthemeLib := 0;
{$IFNDEF FLUENTCOMP}
  @BeginBufferedPaint := nil;
  @EndBufferedPaint := nil;
  @BufferedPaintMakeOpaque := nil;
{$ENDIF}
  if uxthemeLib <> 0 then begin
    @ac_SetWindowTheme := GetProcAddress(uxthemeLib, 'SetWindowTheme');
{$IFNDEF FLUENTCOMP}
    @BeginBufferedPaint := GetProcAddress(uxthemeLib, 'BeginBufferedPaint');
    @EndBufferedPaint := GetProcAddress(uxthemeLib, 'EndBufferedPaint');
    @BufferedPaintMakeOpaque := GetProcAddress(uxthemeLib, 'BufferedPaintMakeOpaque');
{$ENDIF}
  end;

{$IFNDEF DELPHI_10}
  if IsWin10Up{CheckWin32Version(6, 3)} then begin
    lLib := LoadLibrary('ShCore');
    if lLib <= HINSTANCE_ERROR then
      lLib := 0;

    if lLib <> 0 then
      @ac_GetDpiForMonitor := GetProcAddress(lLib, 'GetDpiForMonitor');
  end;
{$ENDIF}


finalization
  if uxthemeLib <> 0 then
    FreeLibrary(uxthemeLib);

end.

