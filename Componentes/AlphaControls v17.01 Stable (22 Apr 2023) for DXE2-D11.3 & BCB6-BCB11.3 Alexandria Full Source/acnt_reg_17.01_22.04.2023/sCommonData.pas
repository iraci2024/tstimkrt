unit sCommonData;
{$I sDefs.inc}
//{$DEFINE ACDEBUG}
//{$DEFINE LOGGED}

interface

uses
  Windows, Graphics, Classes, Controls, SysUtils, extctrls, StdCtrls, Forms, Messages,
  {$IFDEF LOGGED} sDebugMsgs, {$ENDIF}
  {$IFDEF FPC} LMessages, {$ENDIF}
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  {$IFDEF DELPHI_XE2} UItypes, {$ENDIF}
  sDefaults, sSkinManager, acntUtils, sConst, sLabel, acntTypes, acThdTimer;


type
  TsCommonData = class;
  TacOuterEffects = class(TPersistent)
{$IFNDEF NOTFORHELP}
  private
    FOwner: TsCommonData;
    FVisibility: TacOuterVisibility;
    procedure SetVisibility(const Value: TacOuterVisibility);
  public
    procedure Invalidate;
    function IsShown: boolean;
    constructor Create(AOwner: TsCommonData);
{$ENDIF}
  published
    property Visibility: TacOuterVisibility read FVisibility write SetVisibility default ovNone;
  end;


  TsCommonData = class(TPersistent)
{$IFNDEF NOTFORHELP}
  private
    FCommonSkinData: TacSkinData;
    FOpaqueMode: TacOpaqueMode;
    FCustomBorder: TacPaintBorderData;
    function SectionStored: Boolean;
    procedure SetInteger(const Index, Value: integer);
    procedure SetCommonSkinData(const Value: TacSkinData);
    procedure SetBoolean(const Index: Integer; const Value: boolean);
    procedure SetOpaqueMode(const Value: TacOpaqueMode);
  protected
    FCustomFont,
    FCustomColor: boolean;
    FOuterCache: TBitmap;
    FSkinSection: TsSkinSection;
    FSkinManager: TsSkinManager;
    FOuterEffects: TacOuterEffects;
    function GetUpdating: boolean;
    procedure SetSkinSection(const Value: string);
    function IsTransparentSection: boolean;
{$IFDEF ACDEBUG}
    procedure SetBGChanged  (const Value: boolean);
    procedure SetFMouseAbove(const Value: boolean);
    procedure SetUpdating   (const Value: boolean);
    procedure SetSkinIndex  (const Value: integer);
    procedure SetFOwnerControl(const Value: TControl);
    procedure SetCtrlSkinState(const Value: dword);
{$ENDIF}
    procedure SetColorTone  (const Value: TColor);
    procedure SetSkinManager(const Value: TsSkinManager);
    procedure PaintOuter(PBGInfo: PacBGInfo; Data: Word);
    function GetSkinManager: TsSkinManager;
    function GetCommonSkinData: TacSkinData;
    function ManagerStored: boolean;
    procedure OnChanged(Sender: TObject);
  public
    RedrawLockCount: integer;

    FColorTone,
    SavedColor,
    SavedFontColor: TColor;

    FFocused,
    FUpdating,
    HalfVisible,
    Loading: boolean;
    GlassModeInit: boolean;

    COC,
    TranspMode: byte;

    GlowID,
    CircleID,
    PPIIndex,
    FHUEOffset,
    FSaturation,
    CurrentPPI,
    FUpdateCount: integer;

    PrintDC: hdc;
    FCacheBmp: TBitmap;
    FSWHandle: THandle;
    WndProc: TWndMethod;
    FOwnerObject: TObject;

    PulsTimer,
    ClickTimer: TacThreadedTimer;

    BGType: Word;

    InvalidRectH,
    InvalidRectV: TRect;
    AnimTimer: TacThreadedTimer;
    CircleTimer: TacThreadedTimer;
{$IFNDEF ACDEBUG}
    CtrlSkinState: dword;
    FMouseAbove,
    BGChanged: boolean;
    SkinIndex: integer;
    FOwnerControl: TControl;
    property Updating: boolean read GetUpdating write FUpdating default False;
{$ELSE}
    FAnimTimer: TacThreadedTimer;
    FCtrlSkinState: dword;
    FSkinIndex: integer;
    FFMouseAbove,
    FBGChanged: boolean;
    FFOwnerControl: TControl;
    property CtrlSkinState: dword read FCtrlSkinState write SetCtrlSkinState;
    property BGChanged: boolean read FBGChanged write SetBGChanged;
    property FMouseAbove: boolean read FFMouseAbove write SetFMouseAbove;
    property Updating: boolean read GetUpdating write SetUpdating default False;
    property SkinIndex: integer read FSkinIndex write SetSkinIndex;
    property FOwnerControl: TControl read FFOwnerControl write SetFOwnerControl;
{$ENDIF}
    constructor Create(AOwner: TObject; const CreateCacheBmp: boolean); virtual;
    procedure UpdateIndexes(UpdateMain: boolean = True);
    procedure PaintOuterEffects(OwnerCtrl: TWinControl; Offset: TPoint);
    destructor Destroy; override;
    procedure InitCommonProp;
    procedure RemoveCommonProp;
    function DoForceOpaque: boolean;
    function RepaintIfMoved: boolean;
    procedure ClearLinks;
    procedure Loaded(UpdateColors: boolean = True); virtual;
    function OwnerHandle: THandle;

    property CommonSkinData: TacSkinData read GetCommonSkinData write SetCommonSkinData;
{$ENDIF} // NOTFORHELP
    procedure BeginUpdate;
    procedure EndUpdate (const Repaint:   boolean = DefRepaintUpdate);
    procedure Invalidate(const UpdateNow: boolean = False);
    function Skinned(const CheckSkinActive: boolean = False): boolean;
    property HUEOffset:  integer index 0 read FHUEOffset  write SetInteger default 0;
    property Saturation: integer index 1 read FSaturation write SetInteger default 0;
    property ColorTone:  TColor  read FColorTone  write SetColorTone  default clNone;
  published
    property OpaqueMode: TacOpaqueMode read FOpaqueMode write SetOpaqueMode default omAuto;
    property CustomColor: boolean index 0 read FCustomColor write SetBoolean default False;
    property CustomFont:  boolean index 1 read FCustomFont  write SetBoolean default False;
    property SkinManager: TsSkinManager read GetSkinManager write SetSkinManager stored ManagerStored;
    property SkinSection: TsSkinSection read FSkinSection write SetSkinSection stored SectionStored;
    property OuterEffects: TacOuterEffects read FOuterEffects write FOuterEffects;

    property CustomBorder: TacPaintBorderData read FCustomBorder write FCustomBorder;
  end;


  TsCtrlSkinData = class(TsCommonData)
{$IFNDEF NOTFORHELP}
  public
    HotCache,
    LightMask: TBitmap;
    OnUpdateLight: TacAnimUpdateEvent;
    FDoLighting: boolean;
    LightDistanceX, LightDistanceY: integer;
    OldMousePos: TPoint;
    function DoLighting(CheckTimer: boolean = True): boolean;
    destructor Destroy; override;
    procedure Loaded(UpdateColors: boolean = True); override;
    procedure DestroyNeedlessLight(CheckTimer: boolean = True);
  published
{$ENDIF} // NOTFORHELP
    property HUEOffset;
    property Saturation;
    property ColorTone;
  end;


{$IFNDEF NOTFORHELP}
  TacScrollData = class(TPersistent)
  private
    FScrollWidth,
    FButtonsSize: integer;
    procedure SetSize(const Index, Value: integer);
  protected
    FOwner: TsCommonData;
  public
    constructor Create(AOwner: TsCommonData);
    procedure Invalidate;
  published
    property ScrollWidth: integer index 0 read FScrollWidth write SetSize default -1;
    property ButtonsSize: integer index 1 read FButtonsSize write SetSize default -1;
  end;


  TsScrollWndData = class(TsCommonData)
  private
    FHorzScrollData,
    FVertScrollData: TacScrollData;
  public
    constructor Create(AOwner: TObject; const CreateCacheBmp: boolean = True); override;
    destructor Destroy; override;
  published
    property VertScrollData: TacScrollData read FVertScrollData write FVertScrollData;
    property HorzScrollData: TacScrollData read FHorzScrollData write FHorzScrollData;
  end;
{$ENDIF} // NOTFORHELP


  TsBoundLabel = class(TPersistent)
{$IFNDEF NOTFORHELP}
  private
    FOffset,
    FIndent,
    FMaxWidth,
    CurrentPPI: integer;

    FUseHTML,
    FUseSkin,
    FParentFont,
    FAllowClick,
    FFontUpdating,
    FEnabledAlways: boolean;

    FFont: TFont;
    FText: acString;
    FLayout: TsCaptionLayout;
    procedure UpdateAlignment;
    procedure SetFont    (const Value: TFont);
    procedure SetUseSkin (const Value: boolean);
    procedure SetText    (const Value: acString);
    procedure SetLayout  (const Value: TsCaptionLayout);
    procedure SetInteger (const Index, Value: integer);
    procedure SetBoolean (const Index: integer; const Value: boolean);
    procedure SetParentFont(const Value: boolean);
  public
    FActive: boolean;
    FTheLabel: TsEditLabel;
    FOwnerControl: TControl;
    FCommonData: TsCommonData;
    PreventVisible: boolean;
    function GetTextRect: TRect;
    procedure AlignLabel;
    procedure UpdateVisibility;
    procedure DoClick(Sender: TObject);
    procedure UpdateFontOnce(ForceScaleUpdate: boolean = False);
    function DoStoreFont: boolean;
    constructor Create(AOwner: TObject; const CommonData: TsCommonData); overload;
    destructor Destroy; override;
    procedure UpdateScale(NewValue: integer);
    procedure HandleOwnerMsg(const Message: TMessage; const OwnerControl: TControl);
    procedure FontChanged(Sender: TObject);
  published
{$ENDIF} // NOTFORHELP
    property UseHTML:       boolean index 0 read FUseHTML       write SetBoolean default False;
    property Active:        boolean index 1 read FActive        write SetBoolean default False;
    property EnabledAlways: boolean index 3 read FEnabledAlways write SetBoolean default False;

    property UseSkinColor:  boolean read FUseSkin write SetUseSkin default True;

    property AllowClick:    boolean read FAllowClick write FAllowClick default False;

    property Indent:   integer index 0 read FIndent   write SetInteger default 0;
    property MaxWidth: integer index 1 read FMaxWidth write SetInteger default 0;
    property Offset:   integer index 2 read FOffset   write SetInteger default 0;

    property Caption: acString read FText write SetText;
    property Font: TFont read FFont write SetFont stored DoStoreFont;

    property Layout: TsCaptionLayout read FLayout write SetLayout default sclLeft;
    property ParentFont: boolean read FParentFont write SetParentFont default DefBoundLabelParentFont; // Must be declared after Font
  end;

{$IFNDEF NOTFORHELP}
var
  C1, C2: TsColor;
  RestrictDrawing: boolean = False;


function CtrlIsSkinned(const AControl: TControl; CheckActive: boolean = False): boolean;
function WndIsSkinned(const AHandle: THandle): boolean;
function GetCommonData(const AHandle: hwnd): TsCommonData;
function CurrentScale(const SkinData: TsCommonData): integer;
function CanClickShift(SkinIndex: integer; CommonSkinData: TacSkinData): boolean;

function ScaleInt(Value: integer; SkinData: TsCommonData = nil; CalcVCLMode: boolean = True): integer;
function GetPPI(const SkinData: TsCommonData): integer;
function GetSkinData(const SkinData: TsCommonData; PPI: integer = 0): TacSkinData;

function HaveOuterEffects(SkinData: TsCommonData): boolean;
function InUpdating (const SkinData: TsCommonData; const Reset: boolean = False): boolean;
procedure InitBGInfo(const SkinData: TsCommonData; const PBGInfo: PacBGInfo; const State: integer; Handle: THandle = 0);
function GetBGColor (const SkinData: TsCommonData; const State: integer; const Handle: THandle = 0): TColor;
function GetFontIndex(const Ctrl: TControl; const DefSkinIndex: integer; const SkinManager: TsSkinManager; const State: integer = 0): integer; overload;
function GetFontIndex(const Ctrl: TControl; pInfo: PacPaintInfo): integer; overload;
function GetFontIndex(const CtrlHandle: THandle; pInfo: PacPaintInfo): integer; overload;

function GetFontColor(const Ctrl: TControl;      const DefSkinIndex: integer; const SkinManager: TsSkinManager; const State: integer = 0; SkinData: TsCommonData = nil): TColor; overload;
function GetFontColor(const CtrlHandle: THandle; const DefSkinIndex: integer; const SkinManager: TsSkinManager; const State: integer = 0): TColor; overload;

function GetLightColor(const SkinData: TsCommonData; State: integer): TColor;

procedure ShowGlowingIfNeeded(const SkinData: TsCommonData; const Clicked: boolean = False; const CtrlHandle: HWND = 0; Alpha: byte = MaxByte; DoAnimation: boolean = False; ASkinIndex: integer = -1);
procedure UpdateBmpColors(Bmp: TBitmap; SkinData: TsCommonData; CheckCorners: boolean; State: integer; ColorToneBG: TColor);

procedure InitCacheBmp     (const SkinData: TsCommonData);
function SkinBorderMaxWidth(const SkinData: TsCommonData): integer;

procedure InitIndexes(const SkinData: TsCommonData; const Sections: array of string);
procedure InitBGType(const SkinData: TsCommonData);
procedure UpdateData (const SkinData: TsCommonData);
procedure UpdateControlColors(SkinData: TsCommonData; AllowRepaint: boolean = True);
function ControlIsActive (const SkinData: TsCommonData): boolean;
function CanShowFocus(const SkinData: TsCommonData): boolean;
procedure CopyWinControlCache(const Control: TWinControl; const SkinData: TsCommonData; const SrcRect, DstRect: TRect; const DstDC: HDC; const UpdateCorners: boolean;
                              const OffsetX: integer = 0; const OffsetY: integer = 0); overload;
procedure CopyHwndCache(const hwnd: THandle; const SkinData: TsCommonData; const SrcRect, DstRect: TRect; const DstDC: HDC; const UpdateCorners: boolean;
                        const OffsetX: integer = 0; const OffsetY: integer = 0); overload;

function CopyCustomBorder(DC: hdc; R: TRect; SkinData: TsCommonData): TRect;

function ChangeTranspMode(Ctrl: TControl; var Data: integer): boolean;
function CommonMessage(var Message: TMessage; SkinData: TsCommonData): boolean;
function CommonWndProc(var Message: TMessage; SkinData: TsCommonData): boolean;

function NeedParentFont(SkinData: TsCommonData; State: integer): boolean; overload;
function NeedParentFont(SkinManager: TsSkinManager; SkinIndex, State: integer; Ctrl: TControl = nil): boolean; overload;
function GetRgnFromSkin(ASectionIndex: integer; const CtrlSize: TSize; sd: TacSkinData): HRGN;
function GetTransCornersRgn(ASectionIndex: integer; aBmp: TBitmap; sd: TacSkinData): hrgn;

function FullRepaintOnly(SkinData: TsCommonData): boolean;
procedure RepaintImmediate(SkinData: TsCommonData);

function RefreshNeeded(SkinData: TsCommonData; Message: TMessage): boolean;
procedure InvalidateParentCache(SkinData: TsCommonData);
function ChildBgIsChanged(SkinData: TsCommonData): boolean;
procedure StopTimer(SkinData: TsCommonData); overload;
procedure StopTimer(var Timer: TacThreadedTimer); overload;
procedure StopCircleTimer(SkinData: TsCommonData);
function TimerIsActive(SkinData: TsCommonData): boolean;
procedure FinishTimer(ATimer: TacThreadedTimer);
function acMouseInWnd(Handle: THandle): boolean;
procedure HideBoundLabel(BoundLabel: TsBoundLabel);
function TextHintColor(SkinData: TsCommonData; AState: integer; DontBlend: boolean): TColor;
procedure TextHintOut(SkinData: TsCommonData; R: TRect; TextHint: acString; Flags: Cardinal; AState: integer = -1);
{$ENDIF} // NOTFORHELP
function GetParentCache(const SkinData: TsCommonData): TCacheInfo;
function GetParentCacheHwnd(const cHwnd: hwnd): TCacheInfo;
function SkinIsChanging(const SkinData: TsCommonData): boolean;


implementation

uses Math, ComCtrls,
  {$IFDEF CHECKXP} UxTheme, {$ENDIF}
  {$IFNDEF ALITE} sPageControl, sSplitter, acAlphaHints, sFrameAdapter,{$ENDIF}
  sVclUtils, acGlow, sStyleSimply, sSkinProps, sMessages, sMaskData, acSBUtils, acAnimation, sComboBox,
  acgpUtils, sTrackBar, sGraphUtils, sAlphaGraph, sSkinProvider, sSpeedButton, sSkinMenus, sGradient, acDials;


const
  acPropStr = 'ACSDATA';


{$IFDEF RUNIDEONLY}
var
  sTerminated: boolean = False;
{$ENDIF}

type
  TAccessLabel = class(TLabel);


procedure StopTimer(SkinData: TsCommonData);
begin
  if Assigned(SkinData) then begin
    if Assigned(SkinData.AnimTimer) then
      if not (csDestroying in SkinData.AnimTimer.ComponentState) and not SkinData.AnimTimer.Destroyed then begin
        SkinData.AnimTimer.Enabled := False;
        if SkinData.AnimTimer.State = 0 then
          SkinData.AnimTimer.Glow := 0;

        SkinData.AnimTimer.State := -1;
        FreeAndNil(SkinData.AnimTimer.BmpTo);
        FreeAndNil(SkinData.AnimTimer.BmpFrom);
        FreeAndNil(SkinData.AnimTimer.BmpOut);
      end;
{
    if Assigned(SkinData.CircleTimer) then
      if not (csDestroying in SkinData.CircleTimer.ComponentState) and not SkinData.CircleTimer.Destroyed then begin
        SkinData.CircleTimer.Enabled := False;
        if SkinData.CircleTimer.State = 0 then
          SkinData.CircleTimer.Glow := 0;

        SkinData.CircleTimer.State := -1;
        FreeAndNil(SkinData.CircleTimer.BmpTo);
        FreeAndNil(SkinData.CircleTimer.BmpFrom);
        FreeAndNil(SkinData.CircleTimer.BmpOut);
      end;}
  end;
end;


procedure StopCircleTimer(SkinData: TsCommonData);
begin
  if Assigned(SkinData.CircleTimer) then
    if not (csDestroying in SkinData.CircleTimer.ComponentState) and not SkinData.CircleTimer.Destroyed then begin
      SkinData.CircleTimer.Enabled := False;
      if SkinData.CircleTimer.State = 0 then
        SkinData.CircleTimer.Glow := 0;

      SkinData.CircleTimer.State := -1;
      FreeAndNil(SkinData.CircleTimer.BmpTo);
      FreeAndNil(SkinData.CircleTimer.BmpFrom);
      FreeAndNil(SkinData.CircleTimer.BmpOut);
    end;
end;


procedure StopTimer(var Timer: TacThreadedTimer);
begin
  if Assigned(Timer) then
    if not (csDestroying in Timer.ComponentState) and not Timer.Destroyed then begin
      Timer.Enabled := False;
      Timer.State := -1;
    end;
end;


procedure FinishTimer(ATimer: TacThreadedTimer);
begin
  if Assigned(ATimer) then
    with ATimer do
      if not (csDestroying in ComponentState) and not Destroyed then begin
        Iteration := Iterations - 1;
        Glow := iff(State <= 0, 0, MaxByte);
        Enabled := False;
        TimeHandler;
        if BmpOut <> nil then
          FreeAndNil(BmpOut);
      end;
end;


function TimerIsActive(SkinData: TsCommonData): boolean;
begin
  Result := Assigned(SkinData.AnimTimer) and SkinData.AnimTimer.Enabled {and (SkinData.AnimTimer.ThreadType = 0) }or Assigned(SkinData.PulsTimer) and SkinData.PulsTimer.Enabled and (SkinData.PulsTimer.BmpFrom <> nil);
end;


function CurrentScale(const SkinData: TsCommonData): integer;
begin
  Result := SkinData.CurrentPPI;
end;


function ScaleInt(Value: integer; SkinData: TsCommonData = nil; CalcVCLMode: boolean = True): integer;
{$IFDEF ACDPIAWARE}
var
  sm: TsSkinManager;
{$ENDIF}
begin
{$IFDEF ACDPIAWARE}
  if SkinData <> nil then begin
    if (SkinData.FOwnerControl <> nil) and (csDesigning in SkinData.FOwnerControl.ComponentState) then begin
      Result := Value;
      Exit;
    end;
    sm := SkinData.SkinManager
  end
  else
    sm := DefaultManager;

  if sm <> nil then
    case sm.Options.ScaleMode of
      smOldMode:   Result := Value;
      smVCL:
        if CalcVCLMode then
          if SkinData = nil then
            Result := MulDiv(Value, DefaultPPI, 96)
          else
            Result := MulDiv(Value, SkinData.CommonSkinData.PPI, 96)
        else
          Result := Value;

      smCustomPPI: Result := MulDiv(Value, sm.Options.PixelsPerInch, 96)
      // Auto or 100,125,150
      else         Result := ScaleInteger(Value, sm.GetScale);
    end
  else
    Result := MulDiv(Value, DefaultPPI, 96);
{$ELSE}
  Result := Value;
{$ENDIF}
end;


type
  TAccessControl = class(TControl);


function GetPPI(const SkinData: TsCommonData): integer;
begin
{$IFDEF ACDPIAWARE}
  if (SkinData.FOwnerControl <> nil) and (csDesigning in SkinData.FOwnerControl.ComponentState) then
    Result := 96
  else
    with SkinData do
      if (SkinManager = nil) or (SkinIndex < 0) or not SkinManager.SkinCommonInfo.Active or (Length(SkinManager.SkinDataArray) = 0) then begin
        if SkinManager <> nil then
          if SkinManager.Options.ScaleMode = smCustomPPI then
            Result := SkinManager.Options.PixelsPerInch
          else
            Result := DefaultPPI
        else
          if FOwnerControl <> nil then
            Result := GetControlPPI(FOwnerControl)
          else
            Result := DefaultPPI
      end
      else
        if FCommonSkinData <> nil then
          Result := FCommonSkinData.PPI
        else
          if FOwnerObject is TsSkinProvider then
            if [csLoading,csReading] * TsSkinProvider(FOwnerObject).Form.ComponentState = [] then
              Result := GetControlPPI(TsSkinProvider(FOwnerObject).Form)
            else
              Result := DefaultPPI
          else
            if (FOwnerControl is TControl) and ControlIsReady(FOwnerControl) then
              Result := GetControlPPI(FOwnerControl)
            else
              Result := DefaultPPI;
{$ELSE}
  Result := 96;
{$ENDIF}
end;


function GetSkinData(const SkinData: TsCommonData; PPI: integer = 0): TacSkinData;
begin
  if SkinData <> nil then
    Result := SkinData.CommonSkinData
  else
    if (DefaultManager <> nil) and DefaultManager.IsActive then
      Result := DefaultManager.CommonSkinData(PPI)
    else
      Result := StdScaleArray[PPIToIndex(PPI)]; // Add array later
end;


function GetCommonData(const AHandle: hwnd): TsCommonData;
begin
  Result := TsCommonData(GetProp(AHandle, acPropStr));
end;


function CanClickShift(SkinIndex: integer; CommonSkinData: TacSkinData): boolean;
begin
  if (CommonSkinData <> nil) and IsValidIndex(SkinIndex, Length(CommonSkinData.gd)) then
    Result := CommonSkinData.FOwner.ButtonsOptions.ShiftContentOnClick or CommonSkinData.gd[SkinIndex].ShiftOnClick
  else
    Result := True;
end;


function WndIsSkinned(const AHandle: THandle): boolean;
begin
  Result := (GetCommonData(AHandle) <> nil) or (Ac_GetScrollWndFromHwnd(AHandle) <> nil)
{$IFNDEF D2006}
   or GetBoolMsg(AHandle, AC_CTRLHANDLED);
{$ENDIF}
{$IFDEF D2007}
   or GetBoolMsg(AHandle, AC_CTRLHANDLED);
{$ENDIF}
end;


function CtrlIsSkinned(const AControl: TControl; CheckActive: boolean = False): boolean;
var
  cd: TsCommonData;
  lw: TacScrollWnd;
begin
  if (AControl is TWinControl) and TWinControl(AControl).HandleAllocated then
    with TWinControl(AControl) do begin
      cd := GetCommonData(Handle);
      if cd <> nil then
        if CheckActive then
          Result := cd.Skinned
        else
          Result := True
      else begin
        lw := Ac_GetScrollWndFromHwnd(Handle);
        if lw <> nil then
          if CheckActive then
            Result := lw.SkinData.Skinned
          else
            Result := True
        else
          Result := SendMessage(Handle, SM_ALPHACMD, AC_CTRLHANDLED_HI, 0) = 1;
      end;
    end
  else
    Result := AControl.Perform(SM_ALPHACMD, AC_CTRLHANDLED_HI, 0) = 1;
end;


function HaveOuterEffects(SkinData: TsCommonData): boolean;
begin
  with SkinData do
    if SkinManager.Effects.AllowOuterEffects and (FOwnerObject is TsSkinProvider) or (FOwnerControl is TControl{TWinControl}) then
      Result := OuterEffects.Visibility in [ovAlways]
    else
      Result := False;
end;


function InUpdating(const SkinData: TsCommonData; const Reset: boolean = False): boolean;
begin
  if Reset then
    SkinData.FUpdating := False;

  if SkinData.Updating then begin
    SkinData.Updating := True;
    Result := True;
  end
  else
    Result := False;
end;


procedure UpdateCtrlColors(const SkinData: TsCommonData; const Redraw: boolean);
var
  C: TColor;
  SavedOnChange: TNotifyEvent;
begin
  with SkinData do
    if (FOwnerControl <> nil) and IsValidIndex(SkinData.SkinIndex, Length(SkinData.CommonSkinData.gd)) then
      with TacAccessControl(FOwnerControl), CommonSkinData, gd[SkinData.SkinIndex] do begin
        if FOwnerControl is TWinControl then
          if not TWinControl(FOwnerControl).HandleAllocated {$IFNDEF DELPHI6UP} or not IsWindowVisible(TWinControl(FOwnerControl).Handle) {$ENDIF} then
            Exit;

        if Skinned {and not (csRecreating in FOwnerControl.ControlState) }then begin
          SkinData.CtrlSkinState := SkinData.CtrlSkinState or ACS_CHANGING;
          if (COC in sEditCtrls) and not ((Props[0].Transparency = 100) and (Props[1].Transparency = 100)) then
            if FColorTone <> clNone then begin
              if not CustomColor then begin
                C := ChangeSaturation(ChangeHUE(SkinData.HUEOffset, Props[0].Color), SkinData.Saturation);
                if Color <> C then
                  Color := C;
              end;
              if not CustomFont and (not (COC in [COC_TsListView]) or ac_AllowHotEdits) then begin
                C := ChangeSaturation(ChangeHUE(SkinData.HUEOffset, Props[0].FontColor.Color), SkinData.Saturation);
                if Font.Color <> C then
                  Font.Color := C;
              end;
            end
            else begin
              if not CustomColor then begin
                C := ChangeSaturation(ChangeHUE(SkinData.HUEOffset, Props[0].Color), SkinData.Saturation);
                if Color <> C then
                  Color := C;
              end;
              if not CustomFont and (not (COC in [COC_TsListView]) or ac_AllowHotEdits) then begin
                C := ChangeSaturation(ChangeHUE(SkinData.HUEOffset, Props[0].FontColor.Color), SkinData.Saturation);
                if Font.Color <> C then begin
                  SavedOnChange := Font.OnChange;
                  Font.OnChange := nil;
                  Font.Color := C;
                  Font.OnChange := SavedOnChange;
                end;
              end;
            end;

          if (FOwnerControl is TWinControl) and TWinControl(FOwnerControl).HandleAllocated then
            SetClassLong(TWinControl(FOwnerControl).Handle, GCL_HBRBACKGROUND, Integer(FOwner.Brushes[pcMainColor]));

          SkinData.CtrlSkinState := SkinData.CtrlSkinState and not ACS_CHANGING;
        end
        else
          if (FOwnerControl is TWinControl) and TWinControl(FOwnerControl).HandleAllocated and not (SkinData.COC in [COC_TsScrollBar]) then
            SetClassLong(TWinControl(FOwnerControl).Handle, GCL_HBRBACKGROUND, Integer(GetSysColorBrush(COLOR_BTNFACE)));

        if Redraw then
          SkinData.Invalidate(True);
      end;
end;


procedure InitBGType(const SkinData: TsCommonData);
var
  i: integer;
begin
  with SkinData do begin
    BGType := 0;
    if (SkinManager <> nil) and SkinManager.IsValidSkinIndex(SkinIndex) then
      for i := 0 to Length(SkinManager.SkinDataArray) - 1 do
        if SkinData.SkinManager.SkinDataArray[i] <> nil then
          with SkinManager.SkinDataArray[i], SkinData.SkinManager.SkinDataArray[i].gd[SkinIndex].Props[0] do begin
            if (ImagePercent > 0) and (TextureIndex >= 0) then begin
              if ma[TextureIndex].DrawMode in [1, 4, 5, 8, 9, 11, 12] then
                BGType := BGType or BGT_STRETCH;

              if ma[TextureIndex].DrawMode in [0, 1, 3, 5, 10, 12] then
                BGType := BGType or BGT_TEXTURELEFT;

              if ma[TextureIndex].DrawMode in [0, 1, 7, 9, 10, 12] then
                BGType := BGType or BGT_TEXTURERIGHT;

              if ma[TextureIndex].DrawMode in [0, 1, 2, 4, 10, 11] then
                BGType := BGType or BGT_TEXTURETOP;

              if ma[TextureIndex].DrawMode in [0, 1, 6, 8, 10, 11] then
                BGType := BGType or BGT_TEXTUREBOTTOM;
            end;
            if (Length(GradientArray) > 0) and ((GradientPercent > 0) or (GradientArray[0].Mode and PM_OVERLAY <> 0)) then
              case GradientArray[0].Mode and 3 of
                0: BGType := BGType or BGT_GRADIENTVERT;
                1: BGType := BGType or BGT_GRADIENTHORZ;
                2: BGType := BGType or BGT_GRADIENTHORZ or BGT_GRADIENTVERT;
              end;

            if (SkinData.COC = COC_TsSkinProvider) and (SkinData.FOwnerObject is TsSkinProvider) and Assigned(TsSkinProvider(FOwnerObject).Form.OnPaint) then
              BGType := BGType or BGT_STRETCH;
          end;
  end;
end;


procedure InitBGInfo(const SkinData: TsCommonData; const PBGInfo: PacBGInfo; const State: integer; Handle: THandle = 0);
var
  WndPos: TPoint;
  WndRect: TRect;
  Parent: TWinControl;
  aState, iTransparency, iGradient, iTexture: integer;

  procedure GiveOwnBmp;
  begin
    PBGInfo^.BgType := btCache;
    if PBGInfo^.PleaseDraw then
      with PBGInfo^ do
        BitBlt(DrawDC, R.Left, R.Top, WidthOf(R), HeightOf(R), SkinData.FCacheBmp.Canvas.Handle, Offset.X, Offset.Y, SRCCOPY)
    else begin
      if (SkinData.AnimTimer <> nil) and SkinData.AnimTimer.Enabled and (SkinData.AnimTimer.BmpOut <> nil) then
        PBGInfo^.Bmp := SkinData.AnimTimer.BmpOut
      else
        PBGInfo^.Bmp := SkinData.FCacheBmp;

      PBGInfo^.Offset := MkPoint;
    end;
  end;

begin
  with SkinData do
    if PBGInfo^.PleaseDraw then begin
      if BGChanged and (FOwnerControl <> nil) then
        FOwnerControl.Perform(SM_ALPHACMD, AC_PREPARECACHE_HI, 0);

      if not BGChanged then
        with PBGInfo^ do
          BitBlt(DrawDC, R.Left, R.Top, WidthOf(R), HeightOf(R), FCacheBmp.Canvas.Handle, Offset.X, Offset.Y, SRCCOPY);

      PBGInfo^.BgType := btCache;
    end
    else
      if (SkinData.SkinIndex >= 0) and (Length(SkinData.CommonSkinData.gd) > 0) then
        with SkinData.CommonSkinData, gd[SkinData.SkinIndex] do begin
          Parent := nil;
          PBGInfo^.Bmp := nil;
          if SkinData.Skinned and not SkinData.CustomColor then begin
            if State >= States then
              aState := States - 1
            else
              aState := State;

            if aState > ac_MaxPropsIndex then
              aState := ac_MaxPropsIndex;

            iTransparency := Props[aState].Transparency;
            iTexture      := Props[aState].ImagePercent;
            iGradient     := Props[aState].GradientPercent;
            case iTransparency of
              0: begin
                if (iGradient > 0) or (iTexture > 0) or (ImgTL >= 0) or (ImgBL >= 0) or (ImgBR >= 0) or (ImgTR >= 0) or HaveOuterEffects(SkinData) then begin
                  PBGInfo^.Color := FOwner.GetGlobalColor;
                  if BGChanged or (FCacheBmp = nil) or FCacheBmp.Empty or (FOwnerControl <> nil) and ((FCacheBmp.Width <> FOwnerControl.Width) or (FCacheBmp.Height <> FOwnerControl.Height)) then begin
                    BGChanged := True;
                    if FOwnerControl <> nil then begin
                      if FCacheBmp <> nil then
                        FCacheBmp.Assign(nil);

                      SendAMessage(FOwnerControl, AC_PREPARECACHE)
                    end
                    else
                      if FOwnerObject is TsSkinProvider then
                        if TsSkinProvider(FOwnerObject).FFormState and FS_BLENDMOVING <> 0 then begin
                          GiveOwnBmp;
                          Exit;
                        end
                        else begin
                          if FCacheBmp <> nil then
                            FCacheBmp.Assign(nil);

                          SendAMessage(TsSkinProvider(FOwnerObject).Form, AC_PREPARECACHE);
                        end;

                    if (FCacheBmp = nil) or FCacheBmp.Empty then begin
                      PBGInfo^.BgType := btNotReady;
                      Exit;
                    end;
                  end;
                  GiveOwnBmp;
                end
                else begin
                  PBGInfo^.Bmp := nil;
                  PBGInfo^.BgType := btFill;
                  PBGInfo^.Color := GetBGColor(SkinData, State);
                  if FOwner.IsValidImgIndex(BorderIndex) and (ma[BorderIndex].DrawMode and BDM_ACTIVEONLY = 0) then begin
                    with ma[BorderIndex] do
                      PBGInfo^.FillRect := Rect(WL, WT, WR, WB);

                    PBGInfo^.Bmp := FCacheBmp;
                    PBGInfo^.Offset := MkPoint;
                  end;
                  if PBGInfo^.PleaseDraw then
                    FillDC(PBGInfo^.DrawDC, PBGInfo^.R, PBGInfo^.Color);
                end
              end;

              100: begin
                if FCacheBmp <> nil then begin
                  if BGChanged and (FOwnerControl <> nil) then
                    FOwnerControl.Perform(SM_ALPHACMD, AC_PREPARECACHE_HI, 0);

                  if not BGChanged then begin // BG is ready
                    PBGInfo^.BgType := btCache;
                    if PBGInfo^.PleaseDraw then
                      with PBGInfo^ do
                        BitBlt(DrawDC, R.Left, R.Top, WidthOf(R), HeightOf(R), FCacheBmp.Canvas.Handle, Offset.X, Offset.Y, SRCCOPY)
                    else begin
                      PBGInfo^.Bmp := FCacheBmp;
                      PBGInfo^.Offset := MkPoint;
                    end;
                  end
                  else begin
                    PBGInfo^.Bmp := nil;
                    PBGInfo^.BgType := btFill;
                    PBGInfo^.Offset := MkPoint;
                    PBGInfo^.Color := GetBGColor(SkinData, 0);
                  end;
                  Exit;
                end;
                // Parent BG is returned if no borders and parent is ready
                if FOwnerControl <> nil then begin
                  Parent := FOwnerControl.Parent;
                  WndPos := MkPoint(FOwnerControl);
                end
                else
                  if FOwnerObject is TsSkinProvider then
                    with TsSkinProvider(FOwnerObject) do begin
                      if Form <> nil then begin
                        Parent := Form.Parent;
                        WndPos := MkPoint(Form);
                      end;
                    end
                  else
                    Parent := nil;

                if Parent <> nil then begin
                  GetBGInfo(PBGInfo, Parent, PBGInfo^.PleaseDraw);
                  if PBGInfo^.Bmp <> nil then begin // BgType = btCache or Bmp is assigned
                    inc(PBGInfo^.Offset.X, WndPos.X);
                    inc(PBGInfo^.Offset.Y, WndPos.Y);
                    dec(PBGInfo^.FillRect.Left, WndPos.X);
                    dec(PBGInfo^.FillRect.Top, WndPos.Y);
                  end;
                end
                else
                  if Handle <> 0 then begin
                    GetBGInfo(PBGInfo, GetParent(Handle));
                    if PBGInfo^.BgType = btCache then begin
                      GetWindowRect(Handle, WndRect);
                      WndPos := WndRect.TopLeft;
                      ScreenToClient(GetParent(Handle), WndPos);
                      inc(PBGInfo^.Offset.X, WndPos.X);
                      inc(PBGInfo^.Offset.Y, WndPos.Y);
                    end;
                  end
                  else begin
                    PBGInfo^.BgType := btFill;
                    PBGInfo^.Color := DefaultManager.GetGlobalColor
                  end;
              end

              else
                if FCacheBmp = nil then begin
                  PBGInfo^.Color := clFuchsia; // Debug
                  PBGInfo^.BgType := btFill;
                end
                else
                  if FCacheBmp.Empty then begin
                    if BGChanged and (FOwnerControl <> nil) then
                      FOwnerControl.Perform(SM_ALPHACMD, AC_PREPARECACHE_HI, 0);

                    if FCacheBmp.Empty then begin
                      SkinData.Updating := True;
                      PBGInfo^.BgType := btNotReady;
                    end
                    else
                      InitBGInfo(SkinData, PBGInfo, State, Handle);
                  end
                  else begin
                    PBGInfo^.BgType := btCache;
                    if PBGInfo^.PleaseDraw then
                      BitBlt(PBGInfo^.DrawDC, PBGInfo^.R.Left, PBGInfo^.R.Top, WidthOf(PBGInfo^.R), HeightOf(PBGInfo^.R), FCacheBmp.Canvas.Handle, PBGInfo^.Offset.X, PBGInfo^.Offset.Y, SRCCOPY)
                    else begin
                      PBGInfo^.Bmp := FCacheBmp;
                      PBGInfo^.Offset := MkPoint;
                    end;
                  end;
            end;
          end
          else begin
            PBGInfo^.BgType := btFill;
            if FOwnerControl <> nil then
              PBGInfo^.Color := ColorToRGB(TacAccessControl(SkinData.FOwnerControl).Color)
            else
              if FOwnerObject is TsSkinProvider then
                PBGInfo^.Color := ColorToRGB(TsSkinProvider(FOwnerObject).Form.Color);
          end;
        end
      else begin
        PBGInfo^.Bmp := nil;
        PBGInfo^.BgType := btFill;
        PBGInfo^.Color := GetControlColor(FOwnerControl);
      end;
end;


function GetBGColor(const SkinData: TsCommonData; const State: integer; const Handle: THandle = 0): TColor;
var
  i, C: integer;
  Hot: boolean;
  sColor: TsColor_;

  function StdColor: TColor;
  begin
    if SkinData.FOwnerControl <> nil then
      Result := TacAccessControl(SkinData.FOwnerControl).Color
    else
      if SkinData.FOwnerObject is TsSkinProvider then
        Result := TsSkinProvider(SkinData.FOwnerObject).Form.Color
      else
        Result := clBtnFace;
  end;

begin
  with SkinData do
    if Skinned then
      with CommonSkinData do begin
        Hot := (State > 0) and (gd[SkinIndex].States > 1);
        i := gd[SkinIndex].Props[integer(Hot)].Transparency;
        C := gd[SkinIndex].Props[integer(Hot)].Color;

        case i of
          0:
            if Skinned and not CustomColor then
              Result := C
            else
              Result := StdColor;

          100:
            if FOwnerControl <> nil then
              Result := GetControlColor(FOwnerControl.Parent)
            else
              if Handle <> 0 then
                Result := GetControlColor(GetParent(Handle))
              else
                Result := c;

          else
            if FOwnerControl <> nil then
              Result := BlendColors(c, GetControlColor(FOwnerControl.Parent), i * MaxByte div 100)
            else
              if Handle <> 0 then
                Result := BlendColors(c, GetControlColor(GetParent(Handle)), i * MaxByte div 100)
              else
                Result := clBtnFace;
        end;

        if ColorTone <> clNone then begin // Update color if ColorTone used
          sColor.C := Result;
          Result := SwapRedBlue(ChangeTone(sColor, TsColor_RGB(ColorTone)).C);
        end;

        if HUEOffset <> 0 then
          Result := ChangeHUE(HUEOffset, Result);

        if SkinData.Saturation <> 0 then begin
          sColor.C := SwapRedBlue(Result);
          Result := SwapRedBlue(ChangeSaturation(LimitIt(SkinData.Saturation * MaxByte div 100, -MaxByte, MaxByte), sColor).C);
        end;
      end
    else
      Result := StdColor;
end;


function GetFontIndex(const Ctrl: TControl; const DefSkinIndex: integer; const SkinManager: TsSkinManager; const State: integer = 0): integer;
var
  pi: TacPaintInfo;
  aState: integer;
begin
  Result := DefSkinIndex;
  aState := min(State, ac_MaxPropsIndex);
  if (aState = 0) or (DefSkinIndex < 0) or (SkinManager.CommonSkinData.gd[DefSkinIndex].States <= State) or (SkinManager.CommonSkinData.gd[DefSkinIndex].Props[aState].Transparency >= 40) then
    if Ctrl <> nil then begin
      pi.SkinManager := SkinManager;
      pi.R := Ctrl.BoundsRect;
      pi.State := State;
      pi.FontIndex := DefSkinIndex;//0;
      case Ctrl.Perform(SM_ALPHACMD, AC_GETFONTINDEX_HI, LParam(@pi)) of
        -1: ; // Own default font used (used in old skins)
        0: Result := -1 // Parent not skinned
        else
          if pi.FontIndex >= 0 then
            Result := pi.FontIndex;
      end;
    end;
end;


function GetFontIndex(const Ctrl: TControl; pInfo: PacPaintInfo): integer;
begin
  if Ctrl.Parent <> nil then
    OffsetRect(pInfo.R, Ctrl.Left, Ctrl.Top);

  if (Ctrl is TWinControl) and TWinControl(Ctrl).HandleAllocated then
    Result := TrySendMessage(TWinControl(Ctrl).Handle, SM_ALPHACMD, AC_GETFONTINDEX_HI, LParam(pInfo))
  else
    Result := Ctrl.Perform(SM_ALPHACMD, AC_GETFONTINDEX_HI, LParam(pInfo));

  if not (csDesigning in Ctrl.ComponentState) then // Lines will be removed in Beta
    case Result of
      -1, 0: Result := 0 // pInfo.FontIndex; // Ctrl not skinned
      else // will be used parent not skinned font color
    end;
end;


function GetFontIndex(const CtrlHandle: THandle; pInfo: PacPaintInfo): integer; overload;
begin
  Result := SendMessage(CtrlHandle, SM_ALPHACMD, AC_GETFONTINDEX_HI, LParam(pInfo))
end;


function GetFontColor(const Ctrl: TControl; const DefSkinIndex: integer; const SkinManager: TsSkinManager; const State: integer = 0; SkinData: TsCommonData = nil): TColor;
var
  pi: TacPaintInfo;
  Ndx: integer;
  b: boolean;
begin
  if (SkinData <> nil) and SkinData.CustomFont then
    Result := TacAccessControl(Ctrl).Font.Color
  else begin
    Ndx := DefSkinIndex;
    pi.State := min(sConst.ac_MaxPropsIndex, State);
    if IsValidIndex(DefSkinIndex, Length(SkinManager.CommonSkinData.gd)) then
      b := (SkinManager.CommonSkinData.gd[DefSkinIndex].States <= State) or (SkinManager.CommonSkinData.gd[DefSkinIndex].Props[pi.State].Transparency >= 40)
    else
      b := True;

    if b {Transparent} then
      if Ctrl <> nil then begin
        pi.SkinManager := SkinManager;
        pi.R := Ctrl.BoundsRect;
        pi.FontIndex := DefSkinIndex;
        case Ctrl.Perform(SM_ALPHACMD, AC_GETFONTINDEX_HI, LParam(@pi)) of
          -1: ; // Own default font used (used in old skins)
          0: begin
            if Ctrl.Parent <> nil then
              Result := TacAccessControl(Ctrl.Parent).Font.Color // Parent not skinned
            else
              Result := TacAccessControl(Ctrl).Font.Color;

            Exit;
          end
          else
            if pi.FontIndex >= 0 then
              Ndx := pi.FontIndex;
        end;
      end;

    if (SkinManager <> nil) and SkinManager.IsActive then
      with SkinManager do
        if IsValidIndex(Ndx, Length(SkinManager.CommonSkinData.gd)) then begin
          pi.State := min(min(SkinManager.CommonSkinData.gd[Ndx].States - 1, pi.State), ac_MaxPropsIndex);
          Result := SkinManager.CommonSkinData.gd[Ndx].Props[pi.State].FontColor.Color;
        end
        else
          Result := Palette[pcLabelText]
    else
      if Ctrl <> nil then
        Result := TacAccessControl(Ctrl).Font.Color
      else
        Result := clWindow;
  end;
end;


function GetFontColor(const CtrlHandle: THandle; const DefSkinIndex: integer; const SkinManager: TsSkinManager; const State: integer = 0): TColor; overload;
var
  pi: TacPaintInfo;
  Ndx: integer;
begin
  Ndx := DefSkinIndex;
  pi.State := State;
  if (State = 0) or (DefSkinIndex < 0) or (SkinManager.CommonSkinData.gd[DefSkinIndex].States <= State) or (SkinManager.CommonSkinData.gd[DefSkinIndex].Props[State].Transparency >= 40) then begin
    pi.SkinManager := SkinManager;
    pi.R := MkRect;
    pi.FontIndex := DefSkinIndex;
    case TrySendMessage(CtrlHAndle, SM_ALPHACMD, AC_GETFONTINDEX_HI, LParam(@pi)) of
      -1: ; // Own default font used (used in old skins)
      0: begin
        Result := clBtnText;
        Exit;
      end
      else
        if pi.FontIndex >= 0 then
          Ndx := pi.FontIndex;
    end;
  end;
  if (SkinManager <> nil) and SkinManager.IsActive then
    with SkinManager do
      if IsValidIndex(Ndx, LengthOfGD) then
        Result := SkinManager.CommonSkinData.gd[Ndx].Props[pi.State].FontColor.Color
      else
        Result := Palette[pcLabelText]
  else
    Result := clBtnText;
end;


function GetLightColor(const SkinData: TsCommonData; State: integer): TColor;
var
  sd: TsCommonData;
begin
  if (SkinData <> nil) and (SkinData.SkinIndex > 0) then
    with SkinData do
      with SkinManager.CommonSkinData do begin
        State := min(min(State, gd[SkinIndex].States - 1), min(1 + integer(gd[SkinIndex].UseState2), ac_MaxPropsIndex));
        if (gd[SkinIndex].Props[State].Transparency >= 40) and ((gd[SkinIndex].BorderIndex < 0) or (ma[gd[SkinIndex].BorderIndex].DrawMode and BDM_ACTIVEONLY <> 0) and (State = 0) or (ma[gd[SkinIndex].BorderIndex].DrawMode and BDM_FILL = 0)) {Transparent} then begin
          if (FOwnerControl <> nil) and (FOwnerControl.Parent <> nil) and FOwnerControl.Parent.HandleAllocated then begin
{$IFNDEF ALITE}
            if FOwnerControl.Parent is TsTabSheet then
              sd := TsPageControl(FOwnerControl.Parent.Parent).SkinData
            else
{$ENDIF}            
              sd := GetCommonData(FOwnerControl.Parent.Handle);

            if sd <> nil then
              Result := GetLightColor(sd, 0)
            else
              Result := SkinManager.SkinCommonInfo.DefLightColor; // -1{white};
          end
          else
            Result := SkinManager.SkinCommonInfo.DefLightColor; // -1{white};
        end
        else
          if gd[SkinIndex].Props[State].LightColor <> clFuchsia then
            Result := gd[SkinIndex].Props[State].LightColor
          else
            Result := SkinManager.SkinCommonInfo.DefLightColor; // -1{white};
      end
  else
    Result := -1;
end;


procedure ShowGlowingIfNeeded(const SkinData: TsCommonData; const Clicked: boolean = False; const CtrlHandle: HWND = 0; Alpha: byte = MaxByte; DoAnimation: boolean = False; ASkinIndex: integer = -1);
var
  DC: hdc;
  p: TPoint;
  WndHandle: HWND;
  GlowCount: integer;
  SectionName: string;
  ParentForm: TCustomForm;
  R, rBox, rWnd, RealBox: TRect;

  function GetWndHandle: HWND;
  begin
    if ParentForm.Parent <> nil then
      Result := ParentForm.Parent.Handle
    else
      if (TForm(ParentForm).FormStyle = fsMDIChild) and (MDISkinProvider <> nil) then
        Result := TsSkinProvider(MDISkinProvider).Form.Handle
      else
        Result := ParentForm.Handle;
  end;

begin
  if ASkinIndex < 0 then
    ASkinIndex := SkinData.SkinIndex;

  with SkinData do
    if SkinManager.Effects.AllowGlowing and not SkinManager.Options.NoMouseHover and (GetCapture = 0) and SkinManager.IsValidSkinIndex(ASkinIndex) then begin
      GlowCount := CommonSkinData.gd[ASkinIndex].GlowCount;
      if (GlowCount > 0) or ((SkinData.OuterEffects.Visibility = ovMouseHovered) and (SkinData.CommonSkinData.gd[SkinData.SkinIndex].OuterMode > 0)) then
        if FOwnerControl <> nil then begin
          if FMouseAbove then begin
            if not (csLButtonDown in FOwnerControl.ControlState) and (not Clicked or (GlowID = -1)) and not SkinData.CustomBorder.IsChanged then begin
              ParentForm := GetParentForm(FOwnerControl);
              if ParentForm <> nil then begin
                WndHandle := GetWndHandle;
                if not SkinData.SkinManager.Effects.AllowAnimation then
                  DoAnimation := False
                else
                  if DoAnimation then
                    Alpha := 0;

                if SkinData.GlowID = -1 then begin // Make a glowing effect if not existing yet
                  if FOwnerControl is TWinControl then begin
                    if GetWindowRect(TWinControl(FOwnerControl).Handle, rWnd) then begin
                      DC := GetWindowDC(TWinControl(FOwnerControl).Handle);
                      try
                        if GetClipBox(DC, RealBox) <> 0 then begin
                          OffsetRect(RealBox, rWnd.Left, rWnd.Top);
                          SectionName := CommonSkinData.gd[ASkinIndex].ClassName;
                          rBox := RectsAnd(rWnd, RealBox);
                        end
                        else
                          SectionName := '';
                      finally
                        ReleaseDC(TWinControl(FOwnerControl).Handle, DC);
                      end;
                    end
                    else
                      SectionName := '';
                  end
                  else begin
                    R.TopLeft := FOwnerControl.ClientToScreen(MkPoint);
                    R.BottomRight := Point(R.Left + FOwnerControl.Width, R.Top + FOwnerControl.Height);
                    GetWindowRect(FOwnerControl.Parent.Handle, rWnd);

                    DC := GetDC(FOwnerControl.Parent.Handle);
                    GetClipBox(DC, RealBox);
                    ReleaseDC(FOwnerControl.Parent.Handle, DC);

                    p := FOwnerControl.Parent.ClientToScreen(MkPoint);
                    OffsetRect(RealBox, p.X, p.Y);

                    rBox := RectsAnd(R, RealBox);
                    SectionName := SkinSection;
                  end;
                  if SectionName <> '' then
                    GlowID := ShowGlow(rBox, SectionName, s_Glow, CommonSkinData.gd[ASkinIndex].GlowMargin, Alpha, WndHandle, SkinData);
                end;

                // Do animated showing
                if DoAnimation and (SkinData.GlowID >= 0) then begin
                  SkinData.BGChanged := False;
                  DoChangePaint(SkinData, 1, UpdateGlowing_CB, True, False, False);
                  if SkinData.AnimTimer <> nil then
                    SkinData.AnimTimer.ThreadType := TT_LAYEREDEFFECT;
                end;
              end;
            end;
          end
          else // If mouse leave
            HideGlow(GlowID, DoAnimation);
        end
        else
          if CtrlHandle <> 0 then
            if FMouseAbove and not Clicked then begin
              if GlowID = -1 then begin
                GetWindowRect(CtrlHandle, R);
                DC := GetWindowDC(CtrlHandle);
                GetWindowRect(DC, R);
                GetClipBox(DC, RBox);
                OffsetRect(RBox, R.Left, R.Top);
                ReleaseDC(CtrlHandle, DC);
                GlowID := ShowGlow(RBox, SkinSection, s_Glow, CommonSkinData.gd[ASkinIndex].GlowMargin, Alpha, GetParentFormHandle(CtrlHandle), SkinData);
              end;
            end
            else
              HideGlow(GlowID);
    end;
end;


procedure UpdateBmpColors(Bmp: TBitmap; SkinData: TsCommonData; CheckCorners: boolean; State: integer; ColorToneBG: TColor);
begin
  if SkinData.COC in [COC_TsButton, COC_TsBitBtn] then begin
    if (ColorToneBG <> clNone) and (ColorToneBG <> 0) then
      ChangeBitmapPixels(Bmp, ChangeColorTone, acColorToRGB(ColorToneBG), clFuchsia);
  end
  else
    if SkinData.ColorTone <> clNone then
      ChangeBitmapPixels(Bmp, ChangeColorTone, acColorToRGB(SkinData.ColorTone), clFuchsia);

  if SkinData.HUEOffset <> 0 then
    ChangeBitmapPixels(Bmp, ChangeColorHUE, SkinData.HUEOffset, clFuchsia);

  if SkinData.Saturation <> 0 then
    ChangeBitmapPixels(Bmp, ChangeColorSaturation, LimitIt(SkinData.Saturation * MaxByte div 100, -MaxByte, MaxByte), clFuchsia);

//  if not SkinData.CustomBorder.IsChanged then
    UpdateCorners(SkinData, State);
end;


procedure InitCacheBmp(const SkinData: TsCommonData);
begin
  with SkinData do begin
    if not Assigned(FCacheBmp) then begin
      FCacheBmp := CreateBmp32;
      FCacheBmp.Canvas.OnChange := nil;
      FCacheBmp.Canvas.OnChanging := nil;
    end
    else
      FCacheBmp.PixelFormat := pf32bit; // Be sure it's 32bpp

    if FCacheBmp.HandleType <> bmDIB then
      FCacheBmp.HandleType := bmDIB;

    if Assigned(FOwnerControl) then begin
      if FCacheBmp.Width <> FOwnerControl.Width then
        FCacheBmp.Width := max(FOwnerControl.Width, 0);

      if FCacheBmp.Height <> FOwnerControl.Height then
        FCacheBmp.Height := max(FOwnerControl.Height, 0);
    end
  end;
end;


function SkinBorderMaxWidth(const SkinData: TsCommonData): integer;
begin
  Result := 0;
  with SkinData do
    if (CommonSkinData.gd[SkinIndex].BorderIndex >= 0) and (SkinData.CommonSkinData.ma[CommonSkinData.gd[SkinIndex].BorderIndex].DrawMode and BDM_ACTIVEONLY = 0) then
      with SkinData.CommonSkinData.ma[CommonSkinData.gd[SkinIndex].BorderIndex] do begin
        Result := max(WL, WT);
        if WR > Result then
          Result := WR;

        if WB > Result then
          Result := WB;
      end
end;


function GetParentCache(const SkinData: TsCommonData): TCacheInfo;
var
  BGInfo: TacBGInfo;
begin
  if SkinData.Skinned and Assigned(SkinData.FOwnerControl) then begin
    BGInfo.DrawDC := 0;
    BGInfo.PleaseDraw := False;
    BGInfo.BgType := btUnknown;
    if Assigned(SkinData.FOwnerControl.Parent) then
      GetBGInfo(@BGInfo, SkinData.FOwnerControl.Parent)
    else
      if SkinData.FOwnerControl is TWinControl then
        GetBGInfo(@BGInfo, GetParent(TWinControl(SkinData.FOwnerControl).Handle))
      else begin
        Result.X := 0;
        Result.Y := 0;
        Result.FillColor := GetBGColor(SkinData, 0);
        Result.Ready := False;
        Exit;
      end;

    if BGInfo.BgType = btNotReady then begin
      Result.FillColor := clFuchsia;
      Result.Ready := False;
    end
    else
      Result := BGInfoToCI(@BGInfo);
  end
  else begin
    Result.X := 0;
    Result.Y := 0;
    Result.FillColor := GetBGColor(SkinData, 0);
    Result.Ready := False;
  end;
end;


function GetParentCacheHwnd(const cHwnd: hwnd): TCacheInfo;
var
  pHwnd: hwnd;
  BGInfo: TacBGInfo;
begin
  Result.Ready := False;
  pHwnd := GetParent(cHwnd);
  if pHwnd <> 0 then begin
    BGInfo.PleaseDraw := False;
    GetBGInfo(@BGInfo, pHwnd);
    Result := BGInfoToCI(@BGInfo);
  end
  else
    Result := EmptyCI;
end;


procedure RepaintImmediate(SkinData: TsCommonData);
var
  DC: hdc;
begin
  with SkinData do
    if (FOwnerControl is TWinControl) and IsWindowVisible(TWinControl(FOwnerControl).Handle) then begin
      if BGChanged then
        SendAMessage(TWinControl(FOwnerControl).Handle, AC_PREPARECACHE);

      if not BGChanged then begin
        DC := GetWindowDC(TWinControl(FOwnerControl).Handle);
        try
          BitBlt(DC, 0, 0, FCacheBmp.Width, FCacheBmp.Height, FCacheBmp.Canvas.Handle, 0, 0, SRCCOPY);
        finally
          ReleaseDC(TWinControl(FOwnerControl).Handle, DC);
        end;
      end;
    end;
end;


procedure InvalidateParentCache(SkinData: TsCommonData);
begin
  if not InUpdating(SkinData) then
    with SkinData do
      if (FOwnerControl <> niL) and (FOwnerControl.Parent <> nil) and
            FOwnerControl.Parent.HandleAllocated and
              ([csDesigning] * FOwnerControl.ComponentState = []) then begin
        TrySendMessage(FOwnerControl.Parent.Handle, SM_ALPHACMD, AC_SETBGCHANGED_HI + 1, 0);
        if IsWindowVisible(FOwnerControl.Parent.Handle) then
          SendAMessage(FOwnerControl.Parent.Handle, AC_PREPARECACHE);
      end;
end;


procedure InitIndexes(const SkinData: TsCommonData; const Sections: array of string);
var
  i: integer;
begin
  with SkinData do
    if SkinManager <> nil then
      for i := 0 to Length(Sections) - 1 do begin
        if Sections[i] <> '' then begin
          SkinIndex := SkinManager.GetSkinIndex(Sections[i], CommonSkinData);
          if SkinIndex >= 0 then begin
            UpdateIndexes(False);
            Exit;
          end;
        end
      end
    else
      SkinIndex := -1;
end;


procedure UpdateData(const SkinData: TsCommonData);
begin
  with SkinData do
    if SkinSection = '' then
      case COC of
        // Defining of the SkinIndex only
        COC_TsTrackBar, COC_TsSplitter, COC_TsSpeedButton, COC_TsPanel, COC_TsTabControl,
        COC_TsComboBox, COC_TsColorBox, COC_TsPageControl, COC_TsTabSheet, COC_TsGroupBox,
        COC_TsCheckBox, COC_TsRadioButton, COC_TsButton, COC_TsBitBtn, COC_TsToolBar, COC_TsCoolBar,
        COC_TsTreeView, COC_TsCurrencyEdit, COC_TsSpinEdit..COC_TsListBox, COC_TsScrollBox,
        COC_TsDBEdit, COC_TsDBComboBox, COC_TsDBMemo, COC_TsDBListBox, COC_TsDBGrid,
        COC_TsStatusBar, COC_TsGauge, COC_TsMonthCalendar, COC_TsListView, COC_TsSlider, COC_TsFrameAdapter,
        COC_TsDBLookupListBox, COC_TsDBLookupComboBox, COC_TsFileDirEdit..COC_TsDateEdit,
        COC_TsCircleControl, COC_TsSkinProvider, COC_TsImage, COC_TsHeaderControl:
          UpdateIndexes;

        // Defining of the default SkinSection property
        COC_TsAdapter:
          SkinSection := s_Edit;

        COC_TsComboBoxEx:
          SkinSection := s_ComboBox;

        COC_TsNavButton:
          SkinSection := s_ToolButton;

        COC_TsDragBar:
          SkinSection := s_DragBar;

        COC_TsFrameBar:
          SkinSection := s_Bar;

        COC_TsBarTitle:
          SkinSection := s_BarTitle

        else
          if FOwnerObject <> nil then
            SkinSection := FOwnerObject.ClassName;
      end
    else
      UpdateIndexes;
end;


procedure UpdateControlColors(SkinData: TsCommonData; AllowRepaint: boolean = True);
var
  State: integer;
begin
  with SkinData do
    if FOwnerControl <> nil then
      with TacAccessControl(FOwnerControl) do begin
        if not AllowRepaint and Visible then
          Perform(WM_SETREDRAW, 0, 0);

        if IsValidIndex(SkinIndex, Length(CommonSkinData.gd)) then
          with CommonSkinData.gd[SkinIndex] do begin
            State := integer(ControlIsActive(SkinData));
            if not CustomColor then
              Color := Props[State].Color;

            if not CustomFont and (Font.Color <> Props[State].FontColor.Color) then begin
              SkinData.CtrlSkinState := SkinData.CtrlSkinState or ACS_CHANGING;
              Font.Color := Props[State].FontColor.Color;
              SkinData.CtrlSkinState := SkinData.CtrlSkinState and not ACS_CHANGING;
            end;
          end;

        if not AllowRepaint and Visible then
          Perform(WM_SETREDRAW, 1, 0);
      end;
end;


function ControlIsActive(const SkinData: TsCommonData): boolean;
begin
  Result := False;
  if SkinData <> nil then
    with SkinData do
      if Assigned(FOwnerControl) and ([csDestroying, csDesigning] * FOwnerControl.ComponentState = []) then
        if FOwnerControl.Enabled then
          if FFocused then
            Result := True
          else
            if (FOwnerControl is TWinControl) and TWinControl(FOwnerControl).HandleAllocated and (TWinControl(FOwnerControl).Handle = GetFocus) then
              Result := not (FOwnerControl is TCustomPanel)
            else
              if FMouseAbove and not (Assigned(SkinManager) and SkinManager.Options.NoMouseHover) then
                Result := not (COC in sForbidMouse) and (not (SkinData.COC in sNoHotEdits) or ac_AllowHotEdits);
end;


function CanShowFocus(const SkinData: TsCommonData): boolean;
begin
  Result := ((SkinData.SkinManager = nil) or not SkinData.SkinManager.IsActive or SkinData.SkinManager.ButtonsOptions.ShowFocusRect {$IFDEF DELPHI7UP} or not acThemesEnabled{$ENDIF});
end;


procedure TsCommonData.BeginUpdate;
begin
  Inc(FUpdateCount);
  Updating := True;
  CtrlSkinState := CtrlSkinState or ACS_LOCKED;
  if FOwnerControl <> nil then
    FOwnerControl.Perform(SM_ALPHACMD, AC_BEGINUPDATE_HI, 0)
  else
    if FOwnerObject is TsSkinProvider then
      TsSkinProvider(FOwnerObject).ProcessMessage(SM_ALPHACMD, AC_BEGINUPDATE_HI);
end;


constructor TsCommonData.Create(AOwner: TObject; const CreateCacheBmp: boolean);
begin
  SkinIndex := -1;
  GlowID := -1;
  CircleID := -1;
  PPIIndex := -1;
  AnimTimer := nil;
  CircleTimer := nil;
  ClickTimer := nil;
  CurrentPPI := 96;
  FSWHandle := 0;
  WndProc := nil;
  FCommonSkinData := nil;
  if AOwner is TControl then begin
    FOwnerControl := TControl(AOwner);
    if AOwner is TWinControl then
      WndProc := TWinControl(AOwner).WindowProc;
  end
  else
    FOwnerControl := nil;

  FOwnerObject := AOwner;
  Loading := True;
  FFocused := False;
  FMouseAbove := False;
  FUpdating := False;
  BGChanged := True;
  HalfVisible := False;
  FSkinManager := nil;
  FCacheBmp := nil;
  FOuterCache := nil;
  FUpdateCount := 0;
  RedrawLockCount := 0;
  FCustomFont := False;
  BGType := 0;
  PrintDC := 0;
  SavedColor := clNone;
  SavedFontColor := clNone;
  InvalidRectH.Right := 0;
  InvalidRectV.Bottom := 0;

  FOpaqueMode := omAuto;
  FSaturation := 0;
  FHUEOffset := 0;
  FColorTone := clNone;

  FOuterEffects := TacOuterEffects.Create(Self);
  FCustomBorder := TacPaintBorderData.Create(FOwnerControl, Self, OnChanged);
  if CreateCacheBmp then begin
    FCacheBmp := CreateBmp32;
    FCacheBmp.Canvas.OnChange := nil;
  end;
{$IFDEF RUNIDEONLY}
  if not IsIDERunning and not ((FOwnerObject is TComponent) and (csDesigning in TComponent(FOwnerObject).ComponentState)) and not sTerminated then begin
    sTerminated := True;
    ShowWarning(sIsRUNIDEONLYMessage);
  end;
{$ENDIF}
  InitCommonProp;
end;


destructor TsCommonData.Destroy;
begin
  RemoveCommonProp;
  SkinIndex := -1;
  FSkinManager := nil;
  ClearLinks;
  if PulsTimer <> nil then begin
    if PulsTimer.AnimData = Self then
      PulsTimer.AnimData := nil;

    if not PulsTimer.Destroyed then
      FreeAndNil(PulsTimer);
  end;
  if (AnimTimer <> nil) and not AnimTimer.Destroyed then
    FreeAndNil(AnimTimer);

  if (ClickTimer <> nil) and not ClickTimer.Destroyed then
    FreeAndNil(ClickTimer);

  if (CircleTimer <> nil) and not CircleTimer.Destroyed then
    FreeAndNil(CircleTimer);

  HideGlow(GlowID);
  HideCircle(CircleID);
  FreeAndNil(FCacheBmp);
  FreeAndNil(FOuterEffects);
  FreeAndNil(FCustomBorder);
  if FOuterCache <> nil then
    FreeAndNil(FOuterCache);

  inherited Destroy;
end;


function TsCommonData.DoForceOpaque: boolean;
begin
{$IFDEF FLUENTCOMP}
  if (omForceOpaque = OpaqueMode) and IsFluentOS then
    Result := True
  else
{$ENDIF}  
    Result := False;
{
  if (TranspMode = 1) and (COC in [COC_TsPanel, COC_TsDragBar, COC_TsScrollBox]) and (SkinIndex >= 0) then
    with SkinManager.CommonSkinData.gd[SkinIndex] do begin
      if (Props[0].Transparency = 0) then
        Result := True
      else
        Result := False;
    end
  else
    Result := False;
}
end;


procedure TsCommonData.EndUpdate(const Repaint: boolean = DefRepaintUpdate);
begin
  Dec(FUpdateCount);
  Updating := False;
  CtrlSkinState := CtrlSkinState and not ACS_LOCKED;
  if FUpdateCount <= 0 then begin
    if FOwnerControl <> nil then
      FOwnerControl.Perform(SM_ALPHACMD, AC_ENDUPDATE_HI, 0)
    else
      if FOwnerObject is TsSkinProvider then
        TsSkinProvider(FOwnerObject).ProcessMessage(SM_ALPHACMD, AC_ENDUPDATE_HI);

    if Repaint then
      Invalidate(True);
  end;
end;


function TsCommonData.GetCommonSkinData: TacSkinData;
begin
  if (SkinManager = nil) or (SkinIndex < 0) or not SkinManager.SkinCommonInfo.Active or (Length(SkinManager.SkinDataArray) = 0) then begin
    FCommonSkinData := nil;
    if SkinManager <> nil then
      if (FOwnerControl <> nil) and (csDesigning in FOwnerControl.ComponentState) then
        Result := SkinManager.CommonSkinData(96)
      else
        if SkinManager.Options.ScaleMode = smCustomPPI then
          Result := SkinManager.CommonSkinData(SkinManager.Options.PixelsPerInch)
        else
          Result := SkinManager.CommonSkinData(DefaultPPI)
    else
      if FOwnerControl <> nil then
        if csDesigning in FOwnerControl.ComponentState then
          Result := StdScaleArray[0]
        else
          Result := StdScaleArray[PPIToIndex(GetControlPPI(FOwnerControl))]
      else
        Result := StdScaleArray[PPIToIndex(DefaultPPI)]
  end
  else
    if FCommonSkinData <> nil then
      Result := FCommonSkinData
    else
      if (FOwnerObject is TsSkinProvider) and TsSkinProvider(FOwnerObject).Form.HandleAllocated then begin
        if [csLoading,csReading] * TsSkinProvider(FOwnerObject).Form.ComponentState = [] then
          FCommonSkinData := SkinManager.CommonSkinData(GetControlPPI(TsSkinProvider(FOwnerObject).Form))
        else
          FCommonSkinData := SkinManager.CommonSkinData(DefaultPPI);

        Result := FCommonSkinData;
        if SkinManager.Options.ScaleMode in [smVCL] then
          CurrentPPI := FCommonSkinData.PPI;
      end
      else
        if FOwnerControl is TControl then
          if ControlIsReady(FOwnerControl) and ((FOwnerControl.Parent <> nil) and FOwnerControl.Parent.Showing and FOwnerControl.Parent.HandleAllocated) then begin
            if msFormScaling in SkinManager.ManagerState then
              Result := SkinManager.CommonSkinData(CurrentPPI)
            else
              case SkinManager.Options.ScaleMode of
                smAuto,
                smVCL:       Result := SkinManager.CommonSkinData(GetControlPPI(FOwnerControl));
                smOldMode:   Result := SkinManager.CommonSkinData(96);
                smCustomPPI: Result := SkinManager.CommonSkinData(SkinManager.Options.PixelsPerInch);
                else         Result := SkinManager.CommonSkinData(aPPIArray[ord(SkinManager.Options.ScaleMode)]);
              end;

            FCommonSkinData := Result;
            PPIIndex := PPIToIndex(Result.ScaleValue);
          end
          else
            Result := SkinManager.CommonSkinData(DefaultPPI)
        else begin
          FCommonSkinData := SkinManager.CommonSkinData(DefaultPPI);
          Result := FCommonSkinData;
        end;
end;


function TsCommonData.GetSkinManager: TsSkinManager;
begin
  if FSkinManager <> nil then
    Result := FSkinManager
  else
    Result := DefaultManager;
end;


function TsCommonData.GetUpdating: boolean;
var
  sm: TsSkinManager;
begin
  if (Self = nil) or ((FOwnerControl <> nil) and (csDesigning in FOwnerControl.ComponentState)) then
    Result := False
  else begin
    sm := SkinManager;
    if sm = nil then
      Result := False
    else begin
      Result := FUpdating;
      if not Result then
        if FOwnerControl <> nil then begin
          if not (csDestroying in FOwnerControl.ComponentState) then
            if FUpdating then
              Result := True
            else
              if FOwnerControl.Parent <> nil then
                Result := GetBoolMsg(FOwnerControl.Parent, AC_PREPARING)
              else
                Result := False
        end
        else
          if (FOwnerObject is TsSkinProvider) and (TsSkinProvider(FOwnerObject).Form.Parent <> nil) then
            Result := GetBoolMsg(TsSkinProvider(FOwnerObject).Form.Parent, AC_PREPARING)
          else
            Result := False
    end;
  end;
end;


procedure TsCommonData.Invalidate(const UpdateNow: boolean = False);
begin
  BGChanged := True;
  if not Loading then
    if Assigned(FOwnerControl) then begin
      if [csDestroying, csLoading] * FOwnerControl.ComponentState = [] then
        if ControlIsReady(FOwnerControl) then begin
          if (AnimTimer <> nil) and (AnimTimer.ThreadType <> TT_LAYEREDEFFECT) then
            StopTimer(Self);

          if not (FOwnerControl is TWinControl) then
            if COC = COC_TsSpeedButton then
              FOwnerControl.Perform(SM_ALPHACMD, AC_INVALIDATE shl 16, integer(UpdateNow))
            else
              FOwnerControl.Repaint
          else
            if TWinControl(FOwnerControl).HandleAllocated then
              RedrawWindow(TWinControl(FOwnerControl).Handle, nil, 0, iff(UpdateNow, RDWA_ALLNOW, RDWA_ALL));
        end;
    end
    else
      if FOwnerObject is TsSkinProvider then
        if TsSkinProvider(FOwnerObject).Form.HandleAllocated then
          RedrawWindow(TsSkinProvider(FOwnerObject).Form.Handle, nil, 0, iff(UpdateNow, RDWA_ALLNOW, RDWA_ALL));
end;


function TsCommonData.IsTransparentSection: boolean;
begin
  if (SkinIndex >= 0) {or SkinData.SkinManager } then
    with SkinManager.SkinCommonInfo do
      Result := not (SkinIndex in [Sections[ssPanel]])
  else
    Result := False;
end;


procedure TsCommonData.Loaded(UpdateColors: boolean = True);
begin
  Loading := False;
  CommonSkinData := nil;
  UpdateData(Self);
  if Skinned and Assigned(FOwnerControl) and Assigned(FOwnerControl.Parent) and not (csLoading in FOwnerControl.ComponentState) then begin
    if FOwnerControl is TWinControl then
      AddToAdapter(TWinControl(FOwnerControl));

    if (FOwnerControl <> nil) and not GlassModeInit then
      UpdateGlassMode(FOwnerControl);

    if UpdateColors then
      UpdateCtrlColors(Self, False);
  end;
  InitCommonProp;
end;


function TsCommonData.ManagerStored: boolean;
begin
  Result := FSkinManager <> nil;
end;


procedure TsCommonData.OnChanged(Sender: TObject);
begin
  Invalidate;
end;


function TsCommonData.OwnerHandle: THandle;
begin
  try
    if (FOwnerObject is TsSkinProvider) and not (csDestroying in TsSkinProvider(FOwnerObject).ComponentState) and not (csDestroying in TsSkinProvider(FOwnerObject).Form.ComponentState) then
      if (TsSkinProvider(FOwnerObject).Form.HandleAllocated) then
        Result := TsSkinProvider(FOwnerObject).Form.Handle
      else
        Result := 0
    else
      if (FOwnerControl <> nil) and not (csDestroying in FOwnerControl.ComponentState) then
        if (FOwnerControl is TWinControl) and TWinControl(FOwnerControl).HandleAllocated then
          Result := TWinControl(FOwnerControl).Handle
        else
          Result := 0
      else
        Result := FSWHandle
  except
    Result := 0;
  end;
end;


const
  USECACHE = $100;


procedure TsCommonData.PaintOuter(PBGInfo: PacBGInfo; Data: Word);
var
  TmpBmp: TBitmap;
  BGInfo: TacBGInfo;
  op, mNdx, Ndx, Size: integer;
  oRect, WndRect, CacheRect: TRect;

  procedure PaintEffect(Bmp: TBitmap; R, OffsRect: TRect; Ndx, Opacity: integer);
  begin
    if OuterEffects.Visibility = ovFocused then
      Opacity := Min(MaxByte, Opacity * 2); // Make more contrast

    DrawSkinRect32Ex(Bmp, R, EmptyCI, Self.CommonSkinData.ma[Ndx], 0, oRect, Opacity);
  end;

begin
  with SkinManager, Self.CommonSkinData do
    if Skinned and OuterEffects.IsShown and IsValidSkinIndex(SkinIndex) and (gd[SkinIndex].OuterMode > 0) then begin
      mNdx := gd[SkinIndex].OuterMask;
      if (mNdx < 0) and (gd[SkinIndex].OuterMode < 3) then begin // If common effect
        Ndx := gd[SkinIndex].OuterMode - 1;
        if oe[Ndx].Source = 0 then
          Exit;

        with oe[Ndx] do begin
          mNdx := Mask;
          oRect := Rect(OffsetL, OffsetT, OffsetR, OffsetB);
          op := Opacity;
        end;
        if mNdx < 0 then
          Exit;
      end
      else begin
        oRect := gd[SkinIndex].OuterOffset;
        op := gd[SkinIndex].OuterOpacity;
      end;

      BGInfo := PBGInfo^;
      if (BGInfo.Bmp <> nil) and (FOwnerControl <> nil) then begin
        if FOwnerControl <> nil then
          SendAMessage(FOwnerControl, AC_GETOUTRGN, LPARAM(@WndRect))
        else
          if FOwnerObject is TsSkinProvider then
            WndRect := TsSkinProvider(FOwnerObject).Form.BoundsRect
          else
            Exit; // Unknown control

        OffsetRect(WndRect, BGInfo.Offset.X, BGInfo.Offset.Y);
        CacheRect := WndRect;
        with CacheRect do begin
          dec(Left,   oRect.Left);
          dec(Top,    oRect.Top);
          inc(Right,  oRect.Right);
          inc(Bottom, oRect.Bottom);
        end;

        if gd[SkinIndex].BorderIndex >= 0 then begin
          oRect.Left   := ma[gd[SkinIndex].BorderIndex].WL + oRect.Left;
          oRect.Top    := ma[gd[SkinIndex].BorderIndex].WT + oRect.Top;
          oRect.Right  := ma[gd[SkinIndex].BorderIndex].WR + oRect.Right;
          oRect.Bottom := ma[gd[SkinIndex].BorderIndex].WB + oRect.Bottom;
        end
        else
  //        if gd[SkinIndex].BorderWidth > 0 then
            if SkinIndex in [SkinCommonInfo.Sections[ssSlider_On], SkinCommonInfo.Sections[ssSlider_Off]] then begin
              Size := min(FOwnerControl.Width, FOwnerControl.Height) div 2;
              oRect.Left   := Size + oRect.Left;
              oRect.Top    := Size + oRect.Top;
              oRect.Right  := Size + oRect.Right;
              oRect.Bottom := Size + oRect.Bottom;
            end;

        if (Data and USECACHE <> 0) and (FOuterCache <> nil) then begin
          TmpBmp := FOuterCache;
          FOuterCache := CreateBmp32(CacheRect);
          BitBlt(FOuterCache.Canvas.Handle, 0, 0, FOuterCache.Width, FOuterCache.Height, BGInfo.Bmp.Canvas.Handle, CacheRect.Left, CacheRect.Top, SRCCOPY);
          PaintEffect(TmpBmp, MkRect(FOuterCache), oRect, mNdx, op);
          BitBlt(BGInfo.Bmp.Canvas.Handle, CacheRect.Left, CacheRect.Top, FOuterCache.Width, FOuterCache.Height, TmpBmp.Canvas.Handle, 0, 0, SRCCOPY);
          TmpBmp.Free;
        end
        else begin
          if FOuterCache = nil then
            FOuterCache := CreateBmp32(CacheRect)
          else begin
            FOuterCache.Width  := WidthOf (CacheRect);
            FOuterCache.Height := HeightOf(CacheRect);
          end;
          BitBlt(FOuterCache.Canvas.Handle, 0, 0, FOuterCache.Width, FOuterCache.Height, BGInfo.Bmp.Canvas.Handle, CacheRect.Left, CacheRect.Top, SRCCOPY);
          if mNdx > 0 then
            PaintEffect(BGInfo.Bmp, CacheRect, oRect, mNdx, op);
        end;
      end;
    end;
end;


procedure TsCommonData.PaintOuterEffects;
var
  BGInfo: TacBGInfo;
  Count, I: Integer;
begin
  if Skinned and SkinManager.Effects.AllowOuterEffects then begin
    BGInfo.BgType := btCache;
    BGInfo.Bmp := FCacheBmp;
    BGInfo.Offset := Offset;
    I := 0;
    Count := OwnerCtrl.ControlCount;
    while I < Count do begin
      if ControlIsReady(OwnerCtrl.Controls[I]) and OwnerCtrl.Controls[I].Visible then
        OwnerCtrl.Controls[I].Perform(SM_ALPHACMD, AC_PAINTOUTER_HI, LPARAM(@BGInfo));

      inc(I);
    end;
  end;
end;


function TsCommonData.RepaintIfMoved: boolean;
begin
  if (Self = nil) or not Skinned or{$IFNDEF ALITE}(FOwnerControl is TsTabSheet) or{$ENDIF} (FOwnerControl = nil) or SkinIsChanging(Self) then
    Result := True
  else
    if not FOwnerControl.Enabled then
      Result := True
    else
      if (CommonSkinData.gd[SkinIndex].Props[0].Transparency > 0) and Assigned(FOwnerControl.Parent) and FOwnerControl.Parent.HandleAllocated then
        Result := GetBoolMsg(FOwnerControl.Parent, AC_CHILDCHANGED)
      else
        Result := False;
end;


{$IFDEF ACDEBUG}
procedure TsCommonData.SetUpdating(const Value: boolean);
begin
  FUpdating := Value;
end;


procedure TsCommonData.SetSkinIndex(const Value: integer);
begin
  FSkinIndex := Value;
end;


procedure TsCommonData.SetFMouseAbove(const Value: boolean);
begin
  FFMouseAbove := Value;
end;


procedure TsCommonData.SetBGChanged(const Value: boolean);
begin
  FBGChanged := Value;
end;


procedure TsCommonData.SetFOwnerControl(const Value: TControl);
begin
  if FFOwnerControl <> Value then
    FFOwnerControl := Value;
end;


procedure TsCommonData.SetCtrlSkinState(const Value: dword);
begin
  if FCtrlSkinState <> Value then
    FCtrlSkinState := Value;
end;

{
procedure SetAnimTimer(const Value: TacThreadedTimer);
begin
  if FAnimTimer <> Value then
    FAnimTimer := Value;
end;
}
{$ENDIF}


function TsCommonData.SectionStored: Boolean;
var
  i: integer;
begin
  i := SendAMessage(FOwnerControl, AC_GETDEFSECTION) - 1;
  if IsValidIndex(i, Length(acDefSections)) then
    Result := (FSkinSection <> '') and (acDefSections[i] <> FSkinSection)
  else
    Result := True;
end;


procedure TsCommonData.SetBoolean(const Index: Integer; const Value: boolean);
begin
  case index of
    0: if FCustomColor <> Value then begin
      FCustomColor := Value;
      if (FOwnerControl <> nil) and not (csLoading in FOwnerControl.ComponentState) and not Loading then
        with TacAccessControl(FOwnerControl) do begin
          if not Value then
            SavedColor := Color
          else
            if SavedColor <> clNone then
              Color := SavedColor
            else
              SavedColor := Color;

          UpdateCtrlColors(Self, True);
        end;
    end;

    1: if FCustomFont <> Value then begin
      FCustomFont := Value;
      if (FOwnerControl <> nil) and not (csLoading in FOwnerControl.ComponentState) and not Loading then
        with TacAccessControl(FOwnerControl) do begin
          if not Value then
            SavedFontColor := Font.Color
          else
            if SavedFontColor <> clNone then
              Font.Color := SavedFontColor
            else
              SavedFontColor := Font.Color;

          UpdateCtrlColors(Self, True);
        end;
    end;
  end;
end;


procedure TsCommonData.SetColorTone(const Value: TColor);
begin
  if FColorTone <> Value then begin
    FColorTone := Value;
    Loaded;
    Invalidate;
  end;
end;


procedure TsCommonData.SetCommonSkinData(const Value: TacSkinData);
begin
  FCommonSkinData := Value;
  if FCommonSkinData = nil then
    PPIIndex := -1
  else
    PPIIndex := FCommonSkinData.ScaleValue;
end;

{
procedure TsCommonData.SetForceOpaque(const Value: boolean);
begin
  if FForceOpaque <> Value then begin
    FForceOpaque := Value;
    if (FOwnerControl is TWinControl) and ([csDesigning, csDestroying, csLoading] * FOwnerControl.ComponentState = []) then
      if IsFluentOS then
        FOwnerControl.Perform(CM_RECREATEWND, 0, 0);
  end;
end;
}

procedure TsCommonData.SetInteger(const Index, Value: integer);

  procedure ChangeProp(var Prop: integer; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      Loaded;
      Invalidate;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FHUEOffset, Value);
    1: ChangeProp(FSaturation, Value);
  end;
end;


procedure TsCommonData.SetOpaqueMode(const Value: TacOpaqueMode);
begin
  if FOpaqueMode <> Value then begin
    FOpaqueMode := Value;
    if (FOwnerControl is TWinControl) and ([csDesigning, csDestroying, csLoading] * FOwnerControl.ComponentState = []) then
      if IsFluentOS then
        FOwnerControl.Perform(CM_RECREATEWND, 0, 0);
  end;
end;


procedure TsCommonData.SetSkinManager(const Value: TsSkinManager);
begin
  if FSkinManager <> Value then begin
    FCommonSkinData := nil;
    if Value <> DefaultManager then
      FSkinManager := Value
    else
      FSkinManager := nil;

    if (FOwnerControl = nil) or not (csLoading in FOwnerControl.ComponentState) then begin
      if Assigned(Value) and (Value.LengthOfGD > 0) then
        UpdateIndexes
      else
        SkinIndex := -1;

      if not Assigned(FOwnerObject) or not (FOwnerObject is TsSkinProvider) then
        if (FOwnerControl <> nil) and (FOwnerControl.Parent <> nil) and not (csDestroying in FOwnerControl.ComponentState) then
          if Assigned(Value) and (Value.LengthOfGD > 0) and Value.IsValidSkinIndex(SkinIndex) then
            FOwnerControl.Perform(SM_ALPHACMD, AC_REFRESH_HI, ACNativeInt(Value))
          else
            FOwnerControl.Perform(SM_ALPHACMD, AC_REMOVESKIN_HI, ACNativeInt(Value));
    end;
  end;
end;


procedure TsCommonData.SetSkinSection(const Value: string);
var
  m: TMessage;
begin
  if FSkinSection <> Value then begin
    HideGlow(GlowID);
    HideCircle(CircleID);
    FSkinSection := UpperCase(Value);
    if Assigned(SkinManager) and (SkinManager.LengthOfGD > 0) then
      UpdateIndexes
    else
      SkinIndex := -1;

    if (FOwnerControl <> nil) and (FOwnerControl.Parent <> nil) and ([csLoading, csReading, csDestroying] * FOwnerControl.ComponentState = []) then begin
      if FOwnerControl is TWinControl then begin
        if FCacheBmp <> nil then
          FCacheBmp.Assign(nil);

        m := MakeMessage(SM_ALPHACMD, AC_SETBGCHANGED_HI + 1, 0, 0);
        AlphaBroadCast(TWinControl(FOwnerControl), m);
      end;
      if not FUpdating and not Loading then begin
//        StopTimer(AnimTimer);
        if Assigned(SkinManager) and SkinManager.IsValidSkinIndex(SkinIndex) then
          FOwnerControl.Perform(SM_ALPHACMD,  WParam(integer(AC_REFRESH_HI) + SkinIndex), ACNativeInt(SkinManager))
        else
          FOwnerControl.Perform(SM_ALPHACMD, AC_REMOVESKIN_HI, ACNativeInt(SkinManager));
      end
      else
        if not Loading then
          UpdateControlColors(Self, False);
    end
    else
      if FOwnerObject is TsSkinProvider then
        BGChanged := True;
  end;
end;


function TsCommonData.Skinned(const CheckSkinActive: boolean = False): boolean;
var
  SM: TsSkinManager;
begin
  try
    SM := SkinManager;
    if (SM <> nil) and not (csDestroying in SM.ComponentState) and (SkinIndex >= 0) and (CommonSkinData.FOwner <> nil) {Skinned} and  IsValidIndex(SkinIndex, Length(CommonSkinData.gd)) then
      if (FOwnerObject = nil) or not (csDestroying in TComponent(FOwnerObject).ComponentState) then
        if CheckSkinActive then
          Result := SM.IsActive
        else
          Result := True
      else
        Result := False
    else
      Result := False;
  except
    Result := False;
  end;
end;


function ChildBgIsChanged(SkinData: TsCommonData): boolean;
begin
  Result := True;
//  Result := (SkinData.BGType <> BGT_NONE) or SkinData.HalfVisible;
end;


function NeedParentFont(SkinData: TsCommonData; State: integer): boolean;
begin
  with SkinData do
    if not CustomFont then
      Result := NeedParentFont(SkinManager, SkinIndex, State, FOwnerControl)
    else
      Result := False;
end;


function NeedParentFont(SkinManager: TsSkinManager; SkinIndex, State: integer; Ctrl: TControl = nil): boolean;
var
  X, Y: integer;
begin
  with SkinManager.CommonSkinData do
    if ((Ctrl = nil) or (Ctrl.Parent <> nil)) and IsValidIndex(SkinIndex, Length(gd)) then
      with gd[SkinIndex] do begin
        if States <= State then
          State := States - 1;

        Result := Props[mini(State, ac_MaxPropsIndex)].Transparency > 50;
        if Result and (BorderIndex >= 0) then
          with ma[BorderIndex] do
            if State = 0 then
              Result := (DrawMode and BDM_FILL = 0) or (DrawMode and BDM_ACTIVEONLY <> 0)
            else
              if DrawMode and BDM_FILL = 0 then
                Result := True
              else
                if MaskType = 0 then
                  Result := False
                else begin
                  X := R.Left + State * Width + Width div 2;
                  Y := R.Bottom - Height div 2;
                  if Bmp <> nil then
                    Result := GetAPixel(Bmp, X, Y).R > 127
                  else
                    Result := GetAPixel(SkinManager.MasterBitmap, X, Y).R > 127
                end;
      end
    else
      Result := False;
end;


function GetRgnFromSkin(ASectionIndex: integer; const CtrlSize: TSize; sd: TacSkinData): HRGN;
var
  md: TsMaskData;
  Rects: TRects;
  l, i: integer;
  subrgn: HRGN;
begin
  if IsValidIndex(ASectionIndex, Length(sd.gd)) and (sd.gd[ASectionIndex].BorderIndex >= 0) then begin
    SetLength(Rects, 0);
    md := sd.ma[sd.gd[ASectionIndex].BorderIndex];
    AddRgn(Rects, CtrlSize.cx, md, 0, False);
    AddRgn(Rects, CtrlSize.cx, md, CtrlSize.cy - md.WB, True);

    l := Length(Rects);
    Result := CreateRectRgn(0, 0, CtrlSize.cx, CtrlSize.cy);
    if l > 0 then
      for i := 0 to l - 1 do begin
        with Rects[i] do
          subrgn := CreateRectRgn(Left, Top, Right, Bottom);

        CombineRgn(Result, Result, subrgn, RGN_DIFF);
        DeleteObject(subrgn);
      end;
  end
  else
    Result := 0;
end;


function GetTransCornersRgn(ASectionIndex: integer; aBmp: TBitmap; sd: TacSkinData): hrgn;
var
  md: TsMaskData;
  Rects: TRects;
  l, i: integer;
  SubRgn: hrgn;
begin
  if IsValidIndex(ASectionIndex, Length(sd.gd)) and (sd.gd[ASectionIndex].BorderIndex >= 0) then begin
    SetLength(Rects, 0);
    md := sd.ma[sd.gd[ASectionIndex].BorderIndex];
    AddRgn(Rects, aBmp.Width, md, 0, False);
    AddRgn(Rects, aBmp.Width, md, aBmp.Height - md.WB, True);
    l := Length(Rects);
    Result := CreateRectRgn(0, 0, aBmp.Width, aBmp.Height);
    if l > 0 then
      for i := 0 to l - 1 do begin
        FillRect32(aBmp, Rects[i], 0, 0);
        with Rects[i] do
          SubRgn := CreateRectRgn(Left, Top, Right, Bottom);

        if SubRgn <> 0 then begin
          CombineRgn(Result, Result, SubRgn, RGN_DIFF);
          DeleteObject(SubRgn);
        end;
      end;
  end
  else
    Result := 0;
end;


function FullRepaintOnly(SkinData: TsCommonData): boolean;
begin
  with SkinData.CommonSkinData.gd[SkinData.SkinIndex].Props[0] do
    if (SkinData.CtrlSkinState and ACS_REPAINTNEEDH <> 0) and (SkinData.BGType and (BGT_GRADIENTHORZ or BGT_STRETCHHORZ) <> 0) then
      Result := True
    else
      Result := (SkinData.CtrlSkinState and ACS_REPAINTNEEDV <> 0) and (SkinData.BGType and (BGT_GRADIENTVERT or BGT_STRETCHVERT) <> 0);
end;


function ChangeTranspMode(Ctrl: TControl; var Data: integer): boolean;
begin
  Result := False;
  Data := Ctrl.Perform(SM_ALPHACMD, WParam(AC_SETGLASSMODE shl 16), LParam(Data));
//  if Ctrl.Perform(SM_ALPHACMD, WParam(AC_SETGLASSMODE shl 16), LParam(Data)) <> AC_HANDLED then  // If not supported
//    if (LParam(Data) and $F > 0) and (Ctrl is TWinControl) then // Broadcast needed
//      IterateControls(TWinControl(Ctrl), Data, ChangeTranspMode);
end;


function CommonMessage(var Message: TMessage; SkinData: TsCommonData): boolean;
var
  i: integer;
  b: boolean;
  acM: TMessage;
  Ctrl: TControl;
  aeData: TacAnimEventData;
begin
  Result := True; // Msg is handled, stop after this function executing
  if SkinData <> nil then
    with SkinData do
      if Message.Msg = SM_ALPHACMD then
        case Message.WParamHi of
          AC_SETGLASSMODE:
            if not GlassModeInit then begin
              GlassModeInit := True;
              SkinData.BGChanged := True;
              if SkinData.OpaqueMode = omForceOpaque {DoForceOpaque} then begin
{$IFDEF FLUENTCOMP}
                if IsFluentOS then
                  TranspMode := TM_OPAQUE
                else
{$ENDIF}
                  TranspMode := TM_PARTIAL;
              end
              else
                TranspMode := Message.LParamHi;
{$IFDEF FLUENTCOMP}
              if (TranspMode > 0) and (FOwnerControl <> nil) and IsWinVistaUp then
                FOwnerControl.ControlState := FOwnerControl.ControlState + [csGlassPaint]
              else
                FOwnerControl.ControlState := FOwnerControl.ControlState - [csGlassPaint];
{$ENDIF}
              Message.Result := MakeLResult(Message.LParamLo, TranspMode);
{
              if (Message.LParamHi = TM_FULL) and SkinData.IsTransparentSection then
                TranspMode := Message.LParamHi // Continue with TM_FULL
              else
                TranspMode := TM_PARTIAL; // Section is not transparent
}

              if (Message.LParamLo = AC_DO_BROADCAST) and (FOwnerControl is TWinControl) then
                IterateControls(TWinControl(FOwnerControl), MakeWParam(AC_DO_BROADCAST, iff(SkinData.DoForceOpaque, TM_OPAQUE {Standard mode if parent is ForceOpaque}, TranspMode)), ChangeTranspMode);

              GlassModeInit := False;
            end;

          AC_FORCEOPAQUE: begin
            if SkinData.DoForceOpaque then begin
              RedrawWindow(TWinControl(SkinData.FOwnerControl).Handle, nil, 0, RDW_INVALIDATE + RDW_ERASE + RDW_FRAME + RDW_ALLCHILDREN + RDW_UPDATENOW);
              Message.Result := 1; // Stop
            end;
          end;

(*
          AC_SETGLASSMODE: begin
            case Message.LParamHi of
              TM_FULL: begin
                if (SkinIndex in [SkinManager.SkinCommonInfo.Sections[ssTransparent], SkinManager.SkinCommonInfo.Sections[ssGroupBox]]) or not (FOwnerControl is TWinControl) then begin
                  TranspMode := TM_FULL;
                end
                else
                  TranspMode := TM_OPAQUE;

                if TranspMode = TM_OPAQUE then begin
                  UpdateCtrlOpaque(FOwnerControl, TranspMode = TM_OPAQUE);
//                  FOwnerControl.Perform(CM_RECREATEWND, 0, 0);
                end;
              end;
              TM_PARTIAL: begin
                if SkinData.SkinIndex in [SkinManager.SkinCommonInfo.Sections[ssTransparent], SkinManager.SkinCommonInfo.Sections[ssGroupBox]] then begin
                  TranspMode := TM_FULL;
                end
                else
                  TranspMode := TM_OPAQUE;

                if TranspMode = TM_OPAQUE then begin
                  UpdateCtrlOpaque(FOwnerControl, TranspMode = TM_OPAQUE);
//                  FOwnerControl.Perform(CM_RECREATEWND, 0, 0);
                end;
              end
              else {TM_OPAQUE} begin
                TranspMode := TM_OPAQUE;
                if TranspMode = TM_OPAQUE then
                  UpdateCtrlOpaque(FOwnerControl, False);
              end;
            end;
            SkinData.BGChanged := True;

            Message.Result := 1; // Message is handled
            if (Message.LParamLo = 1) and (FOwnerControl is TWinControl) then
              IterateControls(TWinControl(FOwnerControl), MakeWParam(1 {BroadCast}, TranspMode), ChangeTranspMode);
          end;
*)
          AC_FONTSCHANGED:
            if ACUInt(Message.LParam) = ACUInt(SkinManager) then begin
              BGChanged := True;
              if FOwnerObject is TsSkinProvider then
                Ctrl := TsSkinProvider(FOwnerObject).Form
              else
                if FOwnerControl <> nil then
                  Ctrl := FOwnerControl
                else
                  Ctrl := nil;

              if Ctrl <> nil then begin
                SkinManager.UpdateFontName(Ctrl);
                if Ctrl is TWinControl then
                  AlphaBroadCast(TWinControl(Ctrl), Message);
              end;
            end;

          AC_SETSCALE: begin
            CurrentPPI := Message.LParam;
            if (SkinData.SkinManager <> nil) and SkinData.SkinManager.IsActive then
              FCommonSkinData := SkinData.SkinManager.CommonSkinData(CurrentPPI)
            else
              FCommonSkinData := nil;

            if FOwnerControl is TWinControl then
              AlphaBroadCast(TWinControl(FOwnerControl), Message);

            Result := False;
          end;

          AC_GETSCALE:
            Message.Result := SkinData.CurrentPPI;

          AC_REINITSCROLLS:
            if FOwnerControl is TWinControl then
              with TWinControl(FOwnerControl) do begin
                if HandleAllocated then
                  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);

                AlphaBroadCast(TWinControl(FOwnerControl), Message);
              end;

          AC_KILLTIMERS: begin
            if SkinData.FMouseAbove then
              if (DefaultManager <> nil) and (FOwnerControl is TWinControl) and (DefaultManager.ActiveControl = TWinControl(FOwnerControl).Handle) then
                DefaultManager.ActiveControl := 0;

            if SkinData.GlowID >= 0 then
              HideGlow(SkinData.GlowID);

            if SkinData.CircleID >= 0 then
              HideCircle(SkinData.CircleID);

            if FOwnerControl is TWinControl then
              AlphaBroadCast(TWinControl(FOwnerControl), Message)
{$IFNDEF ALITE}
            else
              if FOwnerObject is TsFrameAdapter then
                AlphaBroadCast(TsFrameAdapter(FOwnerObject).GetFrame, Message)
{$ENDIF};
          end;

          AC_PRINTING:
            if Message.WParamLo = 0 then begin
              CtrlSkinState := CtrlSkinState and not ACS_PRINTING;
              PrintDC := 0;
            end
            else begin
              CtrlSkinState := CtrlSkinState or ACS_PRINTING;
              PrintDC := hdc(Message.LParam);
            end;

          AC_UPDATESECTION:
            if (Message.LParam <> 0) and (UpperCase(SkinSection) = PChar(Message.LParam)^){ GlobalSectionName }then begin
              RestrictDrawing := False;
              Invalidate;
            end;

          AC_UPDATELIGHT:
            if (SkinData is TsCtrlSkinData) and Assigned(TsCtrlSkinData(SkinData).OnUpdateLight) then begin
              aeData.DoLighting := False;
              aeData.DoOutput := boolean(Message.WParamLo);
              TsCtrlSkinData(SkinData).OnUpdateLight(FOwnerControl, @aeData);
            end;

          AC_PREPARING:
            if InUpdating(SkinData) then
              Message.Result := 1
            else
              if not CustomColor then
                if FOwnerControl is TWinControl then
                  if (FCacheBmp = nil) or FCacheBmp.Empty or BGChanged then begin
                    SendAMessage(TWinControl(FOwnerControl).Handle, AC_PREPARECACHE);
                    Message.Result := integer((FCacheBmp = nil) or FCacheBmp.Empty);
                  end
                  else
                    Message.Result := LRESULT((FCacheBmp.Width <> FOwnerControl.Width) or (FCacheBmp.Height <> FOwnerControl.Height))
                else
                  Message.Result := 0
              else
                Message.Result := 0;

          AC_UPDATING:
            Updating := Message.WParamLo = 1;

          AC_PAINTOUTER:
            SkinData.PaintOuter(PacBGInfo(Message.LParam), Message.WParamLo);

          AC_GETOUTRGN:
            if FOwnerControl <> nil then
              PRect(Message.LParam)^ := FOwnerControl.BoundsRect
            else
              PRect(Message.LParam)^ := MkRect;

          AC_GETHALFVISIBLE:
            Message.Result := LRESULT(HalfVisible);

          AC_GETFONTINDEX:
            if Skinned then
              with CommonSkinData.gd[SkinData.SkinIndex] do
                if NeedParentFont(SkinData, PacPaintInfo(Message.LParam)^.State) then begin
//                  PacPaintInfo(Message.LParam).FontIndex := SkinIndex;
                  if FOwnerControl <> nil then
                    Message.Result := GetFontIndex(FOwnerControl.Parent, PacPaintInfo(Message.LParam))
                  else
                    Message.Result := GetFontIndex(GetParent(FSWHandle), PacPaintInfo(Message.LParam))
                end
                else begin
                  PacPaintInfo(Message.LParam)^.FontIndex := SkinIndex;
                  Message.Result := 1;
                end;

          AC_CLEARCACHE:
            if FCacheBmp <> nil then begin
              if FOwnerControl is TWinControl then
                with FOwnerControl as TWinControl do
                  for i := 0 to ControlCount - 1 do
                    if (Controls[i] is TWinControl) and TWinControl(Controls[i]).HandleAllocated then
                      SendAMessage(TWinControl(Controls[i]).Handle, AC_CLEARCACHE);

              if FCacheBmp <> nil then
                FCacheBmp.Assign(nil);

              BGChanged := True;
            end;

          AC_CHILDCHANGED:
            Message.Result := LRESULT(ChildBgIsChanged(SkinData));

          AC_SETNEWSKIN:
            if ACUInt(Message.LParam) = ACUInt(SkinManager) then begin
              FCommonSkinData := nil;
              if SkinData is TsCtrlSkinData then
                if COC in ssLightCtrls then
                  with TsCtrlSkinData(SkinData) do begin
                    FDoLighting := False;
                    LightDistanceX := 0;
                    LightDistanceY := 0;
                    FreeAndNil(HotCache);
                    FreeAndNil(LightMask);
                    AddLightControl(TsCtrlSkinData(SkinData));
                  end;

              // Check Attention animation
              RestartAttention(SkinData);

              StopTimer(SkinData);
              UpdateData(SkinData);
              InitIndexes(SkinData, [SkinSection]);
              if FCacheBmp <> nil then
                FCacheBmp.Assign(nil);

              if (SkinManager <> nil) and (SkinManager.Fonts.MainMode = fmCustom) and (FOwnerControl <> nil) then
                TAccessControl(FOwnerControl).Font.Name := SkinManager.Fonts.MainFont;

              RestrictDrawing := False;
            end;

          AC_SETBGCHANGED: begin
            BGChanged := True;
            StopTimer(SkinData);
            if SkinData.FOwnerControl is TWinControl then
              TWinControl(SkinData.FOwnerControl).BroadCast(Message);
          end;

          AC_SETHALFVISIBLE:
            HalfVisible := True;

          AC_GETSKINDATA:
            Message.Result := LRESULT(SkinData);

          AC_GETCOMMONSKINDATA: begin
            Message.Result := LRESULT(CommonSkinData);
            Message.LParam := LRESULT(FCommonSkinData);
            Result := True;
          end;

          AC_REFRESH:
            if RefreshNeeded(SkinData, Message) then begin
              if acPreviewNeeded then
                FCommonSkinData := nil; // Fix error in preview mode when scaling per monitors is changed

              if SkinData is TsCtrlSkinData then
                if COC in ssLightCtrls then
                  with TsCtrlSkinData(SkinData) do begin
                    FDoLighting := False;
                    LightDistanceX := 0;
                    LightDistanceY := 0;
                    FreeAndNil(HotCache);
                    FreeAndNil(LightMask);
                  end;

              StopTimer(SkinData);
              BGChanged := True;
              if (Message.WParamLo <> 0) and (SkinData.FOwnerControl is TWinControl) then begin // Section only
                acM := MakeMessage(SM_ALPHACMD, AC_SETCHANGEDIFNECESSARY shl 16 + 0, 0, 0);
                AlphaBroadCast(TWinControl(SkinData.FOwnerControl), acM);
              end;

              Updating := False;
              UpdateCtrlColors(SkinData, True);
              if (COC in ssScrolledEdits) and (FOwnerControl is TWinControl) then
                SetWindowPos(TWinControl(FOwnerControl).Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED);

              if (PulsTimer <> nil) and PulsTimer.Enabled then begin
                PulsTimer.PrepareImage;
                PulsTimer.TimeHandler;
              end;
            end
            else
              if SkinData.FOwnerControl is TWinControl then
                AlphaBroadCast(TWinControl(SkinData.FOwnerControl), Message);


          AC_REMOVESKIN:
            if ACUInt(Message.LParam) = ACUInt(SkinManager) then begin
              FCommonSkinData := nil;
              if SkinData is TsCtrlSkinData then
                if COC in ssLightCtrls then
                  with TsCtrlSkinData(SkinData) do begin
                    FDoLighting := False;
                    LightDistanceX := 0;
                    LightDistanceY := 0;
                    FreeAndNil(HotCache);
                    FreeAndNil(LightMask);
                    DelLightControl(TsCtrlSkinData(SkinData));
                  end;

              if SkinIndex >= 0 {SkinData.Skinned} then begin
                SkinIndex := -1;
                StopTimer(SkinData);
                if (SkinManager <> nil) and (csDestroying in SkinManager.ComponentState) then
                  FSkinManager := nil;

                Result := False;
                if Assigned(FCacheBmp) then
                  FCacheBmp.Assign(nil);

                if (FOwnerControl is TWinControl) and TWinControl(FOwnerControl).HandleAllocated then
                  SetClassLong(TWinControl(FOwnerControl).Handle, GCL_HBRBACKGROUND, Integer(GetSysColorBrush(COLOR_BTNFACE)));

                if FOwnerControl is TCustomEdit then begin
                  if not CustomColor then
                    TacAccessControl(FOwnerControl).Color := clWindow;

                  if not CustomFont then
                    TacAccessControl(FOwnerControl).Font.Color := clWindowText;

                  RedrawWindow(TCustomEdit(FOwnerControl).Handle, nil, 0, RDWA_FRAME);
                end
                else
                  if (FOwnerControl is TWinControl) then
                    with TWinControl(FOwnerControl) do
                      for i := 0 to ControlCount - 1 do
                        if Controls[i] is TLabel then
                          TLabel(Controls[i]).Font.Color := clWindowText;
              end;
            end;

          AC_GETCONTROLCOLOR:
            if Assigned(FOwnerControl) then
              if Skinned then
                with CommonSkinData.gd[Skinindex], Props[0], FOwnerControl do
                  case Transparency of
                    0:
                      Message.Result := Color;

                    100:
                      if Parent <> nil then
                        if WndIsSkinned(Parent.Handle) then
                          Message.Result := TrySendMessage(Parent.Handle, SM_ALPHACMD, AC_GETCONTROLCOLOR_HI, 0)
                        else
                          Message.Result := TacAccessControl(Parent).Color
                      else
                        Message.Result := ColorToRGB(TacAccessControl(FOwnerControl).Color);

                    else begin
                      if Parent <> nil then
                        Message.Result := TrySendMessage(Parent.Handle, SM_ALPHACMD, AC_GETCONTROLCOLOR_HI, 0)
                      else
                        Message.Result := ColorToRGB(TacAccessControl(FOwnerControl).Color);
                      // Mixing of colors
                      C1.C := Message.Result;
                      C2.C := Color;
                      C1.R := ((C1.R - C2.R) * Transparency + C2.R shl 8) shr 8;
                      C1.G := ((C1.G - C2.G) * Transparency + C2.G shl 8) shr 8;
                      C1.B := ((C1.B - C2.B) * Transparency + C2.B shl 8) shr 8;
                      Message.Result := C1.C;
                    end
                  end
              else
                Message.Result := ColorToRGB(TacAccessControl(FOwnerControl).Color);

          AC_SETCHANGEDIFNECESSARY: begin
            b := RepaintIfMoved;
            BGChanged := BGChanged or b;
            StopTimer(SkinData);
            if FOwnerControl is TWinControl then
              with TWinControl(FOwnerControl) do begin
                if Message.WParamLo = 1 then
                  RedrawWindow(Handle, nil, 0, RDWA_ALLCHILDREN);

                if b then
                  for i := 0 to ControlCount - 1 do
                    if not (csDestroying in Controls[i].ComponentState) then
                      Controls[i].Perform(SM_ALPHACMD, AC_SETCHANGEDIFNECESSARY shl 16 + 1, 0);
              end;
          end;

          AC_GETAPPLICATION:
            Message.Result := LRESULT(Application);

          AC_CTRLHANDLED: begin
            Message.Result := 1;
            Message.LParam := integer(SkinData.SkinIndex >= 0);
          end;

          AC_ISOPAQUE:
            if (SkinData.SkinManager <> nil) and SkinData.SkinManager.IsActive then
              Message.Result := 1
            else
              if SkinData.FOwnerControl <> nil then
                Message.Result := integer(not (csOpaque in SkinData.FOwnerControl.ControlStyle))
              else
                Message.Result := 1;

          AC_GETBG:
            InitBGInfo(SkinData, PacBGInfo(Message.LParam), integer(FFocused or FMouseAbove and not (COC in ssCanNotBeHot)));

          AC_STOPFADING:
            StopTimer(SkinData);

          AC_GETSKININDEX:
            PacSectionInfo(Message.LParam)^.siSkinIndex := SkinIndex;

          AC_PREPARECACHE:
            SkinData.BGChanged := False;

          AC_ENDPARENTUPDATE:
            if FUpdating and (FOwnerControl is TWinControl) then begin
              if not InUpdating(SkinData, True) then
                RedrawWindow(TWinControl(FOwnerControl).Handle, nil, 0, RDWA_ALLNOW);

              SetParentUpdated(TWinControl(FOwnerControl));
            end;

          AC_INVALIDATE:
            Invalidate(Message.WParamLo <> 0);

          AC_SETSECTION:
            if Message.LParam <> 0 then begin
              SkinSection := PacSectionInfo(Message.LParam).siName;
              Message.Result := 1;
            end
            else
              Message.Result := 0

          else
            Result := False;
  end;
end;


procedure TsCommonData.UpdateIndexes(UpdateMain: boolean = True);
begin
{$IFNDEF SKININDESIGN}
  if (FOwnerObject is TComponent) and (csDesigning in TComponent(FOwnerObject).ComponentState) and (GetOwnerFrame(TComponent(FOwnerObject)) <> nil) then
    SkinIndex  := -1
  else
{$ENDIF}
  begin
    BGChanged := True;
    if UpdateMain then begin
      FCommonSkinData := nil;
      if Assigned(SkinManager) and SkinManager.Active then
        if SkinSection <> '' then
          SkinIndex := SkinManager.GetSkinIndex(SkinSection)
        else
          if (FOwnerControl is TWinControl) and TWinControl(FOwnerControl).HandleAllocated then
            SkinIndex := SendAMessage(FOwnerControl, AC_GETDEFINDEX) - 1
          else
            if FOwnerControl <> nil then
              SkinIndex := FOwnerControl.Perform(SM_ALPHACMD, AC_GETDEFINDEX_HI, 0) - 1
            else
              if FOwnerObject is TsSkinProvider then
                SkinIndex := TsSkinProvider(FOwnerObject).Form.Perform(SM_ALPHACMD, AC_GETDEFINDEX_HI, 0) - 1
              else
                SkinIndex := -1
      else
        SkinIndex := -1;
    end;
    InitBGType(Self);
  end;
end;


function acMouseInControl(sd: TsCommonData): boolean;
var
  DC: hdc;
  p: TPoint;
  cR, wR: TRect;
  sw: TacMainWnd;
  Ctrl: TControl;
  AHandle: THandle;
begin
  Result := False;
  if sd.FOwnerControl is TWinControl then
    with TWinControl(sd.FOwnerControl) do begin
      AHandle := Handle;
      if IsWindowVisible(AHandle) then begin
        wR := ClientRect;
        p := ScreenToClient(acMousePos);
      end
      else
        Exit;
    end
  else begin
    AHandle := sd.OwnerHandle;
    if (AHandle <> 0) and IsWindowVisible(AHandle) then begin
      GetWindowRect(AHandle, wR);
      GetClientRect(AHandle, cR);
      p := acMousePos;
      p.X := p.X - wR.Left;
      p.Y := p.Y - wR.Top;
      wR := MkRect(WidthOf(cR), HeightOf(cR));
    end
    else
      Exit;
  end;
  sw := TacMainWnd(SendAMessage(AHandle, AC_GETLISTSW));
  if sw is TacScrollWnd then
    with TacScrollWnd(sw) do begin
      if sBarHorz.fScrollVisible then
        inc(wR.Bottom, integer(sbarHorz.fScrollVisible) * GetScrollMetric(sbarHorz, SM_SCROLLWIDTH));

      if sBarVert.fScrollVisible then
        inc(wR.Right, integer(sbarVert.fScrollVisible) * GetScrollMetric(sbarVert, SM_SCROLLWIDTH));
    end;

  Result := PtInRect(wR, p);
  if Result and (AHandle <> 0) then begin // Check if part of control is not visible
    DC := GetDC(AHandle);
    GetClipBox(DC, wR);
    Result := PtInRect(wR, p);
    ReleaseDC(AHandle, DC);
    if Result then begin
      Ctrl := CtrlUnderMouse;
      if (Ctrl is TWinControl) and (AHandle <> 0) and (Ctrl <> sd.FOwnerControl) then
        Result := ContainsWnd(TWinControl(Ctrl).Handle, AHandle);
    end
  end;
end;


function acMouseInWnd(Handle: THandle): boolean;
var
  p: TPoint;
  cR, wR: TRect;
  sw: TacMainWnd;
begin
  Result := False;
  if Handle <> 0 then begin
    GetWindowRect(Handle, wR);
    GetClientRect(Handle, cR);
    p := acMousePos;
    p.X := p.X - wR.Left;
    p.Y := p.Y - wR.Top;
    wR := MkRect(WidthOf(cR), HeightOf(cR));
    sw := TacMainWnd(SendAMessage(Handle, AC_GETLISTSW));
    if sw is TacScrollWnd then
      with TacScrollWnd(sw) do begin
        if sBarHorz.fScrollVisible then
          inc(wR.Bottom, integer(sbarHorz.fScrollVisible) * GetScrollMetric(sbarHorz, SM_SCROLLWIDTH));

        if sBarVert.fScrollVisible then
          inc(wR.Right, integer(sbarVert.fScrollVisible) * GetScrollMetric(sbarVert, SM_SCROLLWIDTH));
      end;

    Result := PtInRect(wR, p);
  end;
end;


{$IFNDEF ALITE}
type
  TAccessAlphaHints = class(TsAlphaHints);
{$ENDIF}

function CommonWndProc(var Message: TMessage; SkinData: TsCommonData): boolean;
var
  R: TRect;
{$IFNDEF ALITE}
  p: TPoint;
  bDisHintShown: boolean;
  MM: TWMMouse;
  wCtrl: TWinControl;
{$ENDIF}
  i: integer;
  b: boolean;
  m: TMessage;
begin
  Result := False; // Continue if False
  if SkinData <> nil then
    with SkinData do
      case Message.Msg of
        SM_ALPHACMD: // Common messages for all components
          Result := CommonMessage(Message, SkinData);

{$IFDEF DELPHI_10}
        WM_DPICHANGED:
          CommonSkinData := nil;
{$ENDIF}
        WM_MOUSEMOVE: begin
{$IFNDEF ALITE}
          // Trying to make hints for disabled controls
          if (Manager <> nil) and Manager.Active and Manager.HandleDisabledCtrls and
                not (csDestroying in Manager.ComponentState) and (SkinData.FOwnerControl <> nil) and
                  not (csDestroying in SkinData.FOwnerControl.ComponentState) then begin
            bDisHintShown := False;
            if SkinData.FOwnerControl is TWinControl then
              wCtrl := TWinControl(SkinData.FOwnerControl)
            else
              if SkinData.FOwnerObject is TsSkinProvider then
                wCtrl := TsSkinProvider(SkinData.FOwnerObject).Form
              else
                wCtrl := nil;

            if wCtrl <> nil then
              with wCtrl do
                for i := 0 to ControlCount - 1 do begin
                  p.X := TWMMouse(Message).XPos;
                  p.Y := TWMMouse(Message).YPos;
                  if not Controls[i].Enabled and PtInRect(wCtrl.Controls[i].BoundsRect, p) and Controls[i].ShowHint and (Controls[i].Hint <> '') then begin
                    p.X := p.X - Controls[i].Left;
                    p.Y := p.Y - Controls[i].Top;
                    MM := TWMMouse(Message);
                    MM.XPos := p.X;
                    MM.YPos := p.Y;
                    bDisHintShown := True;
                    if (TAccessAlphaHints(Manager).HintTimeCtrl <> Controls[i]) then begin
                      Application.CancelHint;
                      p.X := acMousePos.X;
                      p.Y := acMousePos.Y;// + 16;
                      TAccessAlphaHints(Manager).HintTimeCtrl := Controls[i];
                      Manager.ShowHint(p, Controls[i].Hint, Application.HintHidePause);
                    end;
                    Result := True; // Skip standard handler, do not show hint inherited from parent if exists
                  end;
                end;

            if not bDisHintShown and (TAccessAlphaHints(Manager).HintTimeCtrl <> nil) and not TAccessAlphaHints(Manager).HintTimeCtrl.Enabled then // Hide hint of disabled control when MouseLeave
              if (TAccessAlphaHints(Manager).HintTimeCtrl <> wCtrl) and (wCtrl <> nil) and wCtrl.ShowHint and (wCtrl.Hint <> '') then begin
                p.X := acMousePos.X;
                p.Y := acMousePos.Y;// + 16;
                TAccessAlphaHints(Manager).HintTimeCtrl := wCtrl;
                Manager.ShowHint(p, wCtrl.Hint, Application.HintHidePause);
              end
              else
                Manager.HideHint;

            if not bDisHintShown and Manager.Active and Manager.HandleDisabledCtrls and
                 (TAccessAlphaHints(Manager).HintTimeCtrl <> nil) and not TAccessAlphaHints(Manager).HintTimeCtrl.Enabled then
                   TAccessAlphaHints(Manager).HintTimeCtrl := nil; // Hint is hidden already, but timectrl is defined still
          end;
{$ENDIF}
          if (SkinData is TsCtrlSkinData) and TsCtrlSkinData(SkinData).DoLighting(False) then begin
            TsCtrlSkinData(SkinData).LightDistanceX := TWMMouseMove(Message).XPos - SkinData.FOwnerControl.Width div 2;
            TsCtrlSkinData(SkinData).LightDistanceY := TWMMouseMove(Message).YPos - SkinData.FOwnerControl.Height div 2;
            FreeAndNil(TsCtrlSkinData(SkinData).HotCache);
            SkinData.BGChanged := True;
            DoUpdateLight(TsCtrlSkinData(SkinData), True);
          end;
        end;

        WM_PRINT: begin
          UpdateGlassMode(SkinData.FOwnerControl);
          if not SkinData.Skinned and (Message.WParam <> 0) and (SkinData.FOwnerControl is TWinControl) then begin
            if GetWindowLong(TWinControl(SkinData.FOwnerControl).Handle, GWL_ExSTYLE) and WS_EX_CLIENTEDGE <> 0 then
              i := 2
            else
              i := 0;

            PaintControls(hdc(Message.WParam), TWinControl(SkinData.FOwnerControl), False, Point(i, i));
            Result := True;
          end;
        end;

        WM_SETREDRAW:
          if Message.WParam = 1 then
            if RedrawLockCount > 0 then begin
              dec(RedrawLockCount);
              if not SkinData.FOwnerControl.Visible and not (SkinData.COC in [COC_TsListView]) {Desc 11} then
                Result := True
            end
            else
// Desc 16             Result := True
          else
            inc(RedrawLockCount);

{$IFDEF CHECKXP}
        WM_UPDATEUISTATE:
          if Skinned then
            Result := True
          else
            if UseThemes and (FOwnerControl is TWinControl) then
              SetWindowTheme(TWinControl(FOwnerControl).Handle, nil, nil);
{$ENDIF}

{$IFNDEF FPC}
        WM_CONTEXTMENU:
          if (FOwnerControl <> nil) and (TacAccessControl(FOwnerControl).PopupMenu <> nil) and (SkinManager <> nil) then
            SkinManager.SkinableMenus.HookPopupMenu(TacAccessControl(FOwnerControl).PopupMenu, SkinManager.Active);
{$ENDIF}

        CM_VISIBLECHANGED: begin
          if {(GlowID >= 0) and }(Message.WParam = 0) then begin
            HideGlow(GlowID, False);
            HideCircle(CircleID);
            if (SkinData.FOwnerControl is TWinControl) and TWinControl(SkinData.FOwnerControl).HandleAllocated then
              BroadCastMsg(TWinControl(SkinData.FOwnerControl).Handle, MakeMessage(SM_ALPHACMD, MakeWParam(0, AC_KILLTIMERS), 0, 0)); // Hide glowing (and other effects)
          end;

          if (SkinManager <> nil) and (FOwnerControl <> nil) and
                not (csDestroying in FOwnerControl.ComponentState) and Assigned(FCacheBmp) then begin
            if SkinManager.Options.OptimizingPriority = opMemory then begin
              if Message.WParam = 0 then begin
                FCacheBmp.Width  := 0;
                FCacheBmp.Height := 0;
                if (FOwnerControl is TWinControl) then
                  with FOwnerControl as TWinControl do
                    for i := 0 to ControlCount - 1 do
                      if (Controls[i] is TWinControl) and TWinControl(Controls[i]).HandleAllocated then
                        SendAMessage(TWinControl(Controls[i]).Handle, AC_CLEARCACHE)
                      else
                        Controls[i].Perform(SM_ALPHACMD, AC_CLEARCACHE_HI, 0);
              end
              else
                Updating := False;

              BGChanged := True;
            end;
            if SkinData.OuterEffects.IsShown then
              SkinData.OuterEffects.Invalidate;
          end;
        end;

        CM_SHOWINGCHANGED:
          if Assigned(FOwnerControl) then begin
            if FOwnerControl.Visible then
              Loaded;

            if not GlassModeInit then
              UpdateGlassMode(FOwnerControl);
          end;

{$IFDEF TNTUNICODE}
        CM_INVALIDATE:
          if SkinData.FOwnerControl <> nil then // Solving of trouble with loss of WndProc
            SkinData.WndProc := SkinData.FOwnerControl.WindowProc;
{$ENDIF}

        WM_PARENTNOTIFY:
          if Message.WParamLo = WM_CREATE then
            if FOwnerControl is TWinControl then
              AddToAdapter(TWinControl(FOwnerControl));

        CM_PARENTFONTCHANGED:
          if FOwnerControl.Parent <> nil then begin
            if not InAnimationProcess then begin
              if not GlassModeInit then
                UpdateGlassMode(FOwnerControl);

              if FOwnerControl is TWinControl then begin
                m := MakeMessage(SM_ALPHACMD, AC_SETBGCHANGED_HI + 1, 0, 0);
                AlphaBroadCast(TWinControl(FOwnerControl), m);
              end;
            end;
          end
          else
            SkinData.TranspMode := TM_OPAQUE;

        WM_SETFOCUS:
          if Skinned and (FOwnerControl is TWinControl) then
            with TWinControl(FOwnerControl), CommonSkinData.gd[SkinIndex] do
              if CanFocus and TabStop and ((Props[0].Transparency <> 100) or (Props[1].Transparency <> 100)) then begin
                BGChanged := True;
                FFocused := True;
                if (COC in sEditCtrls) and (States > 1) then begin
                  BGChanged := True;
                  b := False;
                  if (TacAccessControl(FOwnerControl).Color <> Props[1].Color) and not CustomColor then begin
                    TacAccessControl(FOwnerControl).Color := Props[1].Color;
                    b := True;
                  end;
                  if (TacAccessControl(FOwnerControl).Font.Color <> Props[1].FontColor.Color) and not CustomFont then begin
                    CtrlSkinState := CtrlSkinState or ACS_CHANGING;
                    TacAccessControl(FOwnerControl).Font.Color := Props[1].FontColor.Color;
                    CtrlSkinState := CtrlSkinState and not ACS_CHANGING;
                    b := True;
                  end;
                  if not SkinData.FUpdating then begin
                    if b then
                      RedrawWindow(Handle, nil, 0, RDWA_ALLNOW);

                    TrySendMessage(Handle, WM_NCPAINT, 0, 0);
                  end;
                end;
                if OuterEffects.Visibility = ovFocused then
                  OuterEffects.Invalidate;

                RepaintGlow(SkinData.GlowID); // Update glow
              end;

        WM_KILLFOCUS:
          if Skinned and (FOwnerControl is TWinControl) then begin
            StopTimer(SkinData);
            with TWinControl(FOwnerControl), CommonSkinData.gd[SkinIndex] do
              if CanFocus {and ((Props[0].Transparency <> 100) or (Props[1].Transparency <> 100)) focused transparent combobox not repainted} then begin
                FFocused := False;
                b := False;
                if (COC in sEditCtrls) and not ControlIsActive(SkinData) then begin
                  if (TacAccessControl(FOwnerControl).Color <> Props[0].Color) and not CustomColor then begin
                    TacAccessControl(FOwnerControl).Color := Props[0].Color;
                    b := True;
                  end;
                  if (TacAccessControl(FOwnerControl).Font.Color <> Props[0].FontColor.Color) and not CustomFont then begin
                    CtrlSkinState := CtrlSkinState or ACS_CHANGING;
                    TacAccessControl(FOwnerControl).Font.Color := Props[0].FontColor.Color;
                    CtrlSkinState := CtrlSkinState and not ACS_CHANGING;
                    b := True;
                  end;
                  if not b and (FOwnerControl is TacCustomCombo) and TacCustomCombo(FOwnerControl).IsFullPaint then
                    b := True;
                end;
                BGChanged := True;
                StopTimer(SkinData);
                if FOwnerControl.Visible and (COC in sEditCtrls) then begin
                  if b then
                    RedrawWindow(Handle, nil, 0, RDWA_ALLNOW);

                  TrySendMessage(Handle, WM_NCPAINT, 0, 0);
                end;
                if OuterEffects.Visibility = ovFocused then
                  OuterEffects.Invalidate;
              end;
          end;

        CM_ENABLEDCHANGED, WM_FONTCHANGE: begin
          FMouseAbove := False;
          FFocused := False;
          BGChanged := True;
          if Assigned(FOwnerControl) and not FOwnerControl.Enabled then begin
            HideGlow(GlowID);
            HideCircle(CircleID);
          end;
        end;

        CM_CHANGED:
          if (PulsTimer <> nil) and PulsTimer.Enabled then
            PulsTimer.PrepareImage;

        CM_MOUSEENTER: begin
          if Skinned and not FMouseAbove then
            with SkinData.SkinManager, SkinData.CommonSkinData, gd[SkinIndex] do begin
              if not (csDesigning in ComponentState) and not Options.NoMouseHover and not ((Props[0].Transparency = 100) and (Props[1].Transparency = 100)) and (FOwnerControl is TWinControl) then
                with TWinControl(FOwnerControl) do begin
                  if not (csDesigning in ComponentState) then begin
                    for i := 0 to ControlCount - 1 do
                      if (Controls[i] is TsSpeedButton) and (Controls[i] <> FOwnerControl) and
                           (Controls[i] <> Pointer(Message.LParam)) and TsSpeedButton(Controls[i]).SkinData.FMouseAbove then
                        Controls[i].Perform(CM_MOUSELEAVE, 0, 0);

                    if not (COC in sForbidMouse) then
                      FMouseAbove := True;

                    DefaultManager.ActiveControl := TWinControl(FOwnerControl).Handle;
                    if not (COC in (ssCanNotBeHot + ssButtons)) then
                      ShowGlowingIfNeeded(SkinData, False, Handle, MaxByte * integer(not Effects.AllowAnimation), True);

                    if (COC in sEditCtrls) and (not (SkinData.COC in sNoHotEdits) or ac_AllowHotEdits) and (States > 1) then begin
                      BGChanged := True;
                      b := False;
                      if (TacAccessControl(FOwnerControl).Color <> Props[1].Color) and not CustomColor then begin
                        TacAccessControl(FOwnerControl).Color := Props[1].Color;
                        b := True;
                      end;
                      if (TacAccessControl(FOwnerControl).Font.Color <> Props[1].FontColor.Color) and not CustomFont then begin
                        CtrlSkinState := CtrlSkinState or ACS_CHANGING;
                        TacAccessControl(FOwnerControl).Font.Color := Props[1].FontColor.Color;
                        CtrlSkinState := CtrlSkinState and not ACS_CHANGING;
                        b := True;
                      end;
                      if b then
                        InvalidateRect(Handle, nil, True);

                      TrySendMessage(Handle, WM_NCPAINT, 0, 0);
                    end;
                  end;
                end
              else begin
                if FOwnerControl is TWinControl then
                  DefaultManager.ActiveControl := TWinControl(SkinData.FOwnerControl).Handle;

                if not (COC in sForbidMouse) then
                  FMouseAbove := True;
              end;
              RestartAttention(SkinData);
            end;

{$IFNDEF D2005}
  {$IFNDEF ALITE}
            if (acAlphaHints.Manager <> nil) and Assigned(acAlphaHints.Manager.AnimWindow) then
              Application.ActivateHint(acMousePos);
  {$ENDIF}
{$ENDIF}
          if (PulsTimer <> nil) and PulsTimer.Enabled then
            PulsTimer.PrepareImage;
        end;

        CM_MOUSELEAVE:
          if Skinned then
            if FOwnerControl <> nil then begin
{$IFNDEF ALITE}
              if (Manager <> nil) and Manager.Active and Manager.HandleDisabledCtrls and
                  (TAccessAlphaHints(Manager).HintTimeCtrl <> nil) and not TAccessAlphaHints(Manager).HintTimeCtrl.Enabled then
                TAccessAlphaHints(Manager).HintTimeCtrl := nil;
{$ENDIF}

              with TWinControl(FOwnerControl), SkinManager, SkinData.CommonSkinData.gd[SkinIndex] do
                if not Options.NoMouseHover and not (csDesigning in ComponentState) and not acMouseInControl(SkinData) then begin
                  if not (COC in sForbidMouse) then
                    FMouseAbove := False;

                  if not (COC in ssCanNotBeHot) and not (COC in ssButtons) then begin
                    HideGlow(GlowID, True);
                    HideCircle(CircleID);
                  end;

                  b := False;
                  if (COC in sEditCtrls) and not ControlIsActive(SkinData) and not ((Props[0].Transparency = 100) and (Props[1].Transparency = 100)) then begin
                    if (TacAccessControl(FOwnerControl).Color <> Props[0].Color) and not CustomColor then begin
                      TacAccessControl(FOwnerControl).Color := Props[0].Color;
                      b := True;
                    end;
                    if (TacAccessControl(FOwnerControl).Font.Color <> Props[0].FontColor.Color) and not CustomFont then begin
                      CtrlSkinState := CtrlSkinState or ACS_CHANGING;
                      TacAccessControl(FOwnerControl).Font.Color := Props[0].FontColor.Color;
                      CtrlSkinState := CtrlSkinState and not ACS_CHANGING;
                      b := True;
                    end;
                  end;
                  if FOwnerControl.Visible and (COC in sEditCtrls) then begin
                    BGChanged := True;
                    if b then
                      InvalidateRect(Handle, nil, True);

                    TrySendMessage(Handle, WM_NCPAINT, 0, 0);
                  end;
                end;

              if (PulsTimer <> nil) and PulsTimer.Enabled then
                PulsTimer.PrepareImage;

              RestartAttention(SkinData);
            end;

        WM_CREATE, WM_NCCREATE: // Reinit if recreated
          SkinData.InitCommonProp;

        WM_WINDOWPOSCHANGING: begin
          if GetWindowRect(OwnerHandle, R) then begin
            if TWMWindowPosChanging(Message).WindowPos.cx <> WidthOf(R) then
              SkinData.CtrlSkinState := SkinData.CtrlSkinState or ACS_REPAINTNEEDH;

            if TWMWindowPosChanging(Message).WindowPos.cy <> HeightOf(R) then
              SkinData.CtrlSkinState := SkinData.CtrlSkinState or ACS_REPAINTNEEDV;
          end;

          if Skinned then
            with TWindowPos(Pointer(Message.LParam)^) do
              if (flags and SWP_NOSIZE = 0) and (flags and SWP_NOMOVE = 0) then
                SkinData.CtrlSkinState := SkinData.CtrlSkinState or ACS_SIZING;
        end;

//        WM_SIZING:
//          if Skinned then
//            SkinData.CtrlSkinState := SkinData.CtrlSkinState or ACS_SIZING;

        WM_PAINT:
          if Skinned then
            SkinData.CtrlSkinState := SkinData.CtrlSkinState and not ACS_SIZING;

        WM_SIZE:
          if Skinned then begin
            BGChanged := True;
            if HaveOuterEffects(SkinData) and (SkinData.FOwnerControl is TWinControl) and IsWindowVisible(TWinControl(SkinData.FOwnerControl).Handle) then
              InvalidateParentCache(SkinData);

            if FullRepaintOnly(SkinData) then begin
{$IFNDEF ALITE}
              if FOwnerControl is TsPageControl then
                with TsPageControl(FOwnerControl), ActivePage do begin
                  if ActivePage <> nil then
                    for i := 0 to ControlCount - 1 do
                      if (i < ControlCount) and not (csDestroying in Controls[i].ComponentState) then
                        Controls[i].Perform(SM_ALPHACMD, AC_SETCHANGEDIFNECESSARY shl 16 + 1, 0);
                end
              else
{$ENDIF}
                if FOwnerControl is TWinControl then
                  with TWinControl(FOwnerControl) do
                    for i := 0 to ControlCount - 1 do
                      if (i < ControlCount) and not (csDestroying in Controls[i].ComponentState) then
                        Controls[i].Perform(SM_ALPHACMD, AC_SETCHANGEDIFNECESSARY shl 16 + 1, 0);

              SkinData.CtrlSkinState := SkinData.CtrlSkinState and not ACS_REPAINTNEEDH and not ACS_REPAINTNEEDV;
            end;
            if (AnimTimer = nil) or not AnimTimer.Enabled then begin
              HideGlow(GlowID);
              HideCircle(CircleID);
            end;
          end;

        WM_SHOWWINDOW:
          StopTimer(SkinData);

//        WM_WINDOWPOSCHANGED,
        WM_MOVE: if Skinned then begin
          if (FOwnerControl is TWinControl) then begin
            if HaveOuterEffects(SkinData) and IsWindowVisible(TWinControl(SkinData.FOwnerControl).Handle) then begin
//              InvalidateParentCache(SkinData);
              TrySendMessage(SkinData.FOwnerControl.Parent.Handle, SM_ALPHACMD, AC_SETBGCHANGED_HI + 1, 0);
              SendAMessage(SkinData.FOwnerControl.Parent.Handle, AC_PREPARECACHE);
              if csDesigning in FOwnerControl.ComponentState then
                RedrawWindow(FOwnerControl.Parent.Handle, nil, 0, RDWA_ERASENOW);
            end;
            if RepaintIfMoved then begin
              BGChanged := True;
              if not SkinIsChanging(SkinData) then begin
                FOwnerControl.Perform(SM_ALPHACMD, AC_SETCHANGEDIFNECESSARY shl 16 + 1, 0);
                BroadCastMsg(TWinControl(SkinData.FOwnerControl).Handle, MakeMessage(SM_ALPHACMD, MakeWParam(0, AC_KILLTIMERS), 0, 0)); // Hide glowing (and other effects)
              end;
            end;
          end
          else begin
            if HaveOuterEffects(SkinData) and (SkinData.FOwnerControl.Parent <> nil) and IsWindowVisible(TWinControl(SkinData.FOwnerControl.Parent).Handle) then begin
//              InvalidateParentCache(SkinData);

//              Alert;
              TrySendMessage(SkinData.FOwnerControl.Parent.Handle, SM_ALPHACMD, AC_SETBGCHANGED_HI + 1, 0);
              SendAMessage(SkinData.FOwnerControl.Parent.Handle, AC_PREPARECACHE);

              if csDesigning in FOwnerControl.ComponentState then
                RedrawWindow(FOwnerControl.Parent.Handle, nil, 0, RDWA_ERASENOW);
            end;
            BGChanged := True;
          end;

          HideGlow(GlowID);
          HideCircle(CircleID);
        end;

        WM_LBUTTONDOWN:
          RestartAttention(SkinData);

        WM_LBUTTONUP:
          RestartAttention(SkinData);
      end;
end;


procedure CopyWinControlCache(const Control: TWinControl; const SkinData: TsCommonData; const SrcRect, DstRect: TRect; const DstDC: HDC;
                              const UpdateCorners: boolean; const OffsetX: integer = 0; const OffsetY: integer = 0);
var
  R: TRect;
  i: integer;
  SaveIndex: HDC;
  Child: TControl;
begin
  if (SkinData.FCacheBmp <> nil) and not SkinData.FCacheBmp.Empty then begin
    if UpdateCorners and not SkinData.CustomBorder.IsChanged then
      sAlphaGraph.UpdateCorners(SkinData, 0);

    SaveIndex := SaveDC(DstDC);
    IntersectClipRect(DstDC, DstRect.Left, DstRect.Top, DstRect.Right, DstRect.Bottom);
    try
      for i := 0 to Control.ControlCount - 1 do begin
        Child := Control.Controls[i];
        if (Child is TGraphicControl) and (SkinData.SkinManager <> nil) and SkinData.SkinManager.Options.StdImgTransparency {$IFNDEF ALITE}or (Child is TsSplitter){$ENDIF} then
          Continue;

        with Child do begin
          R := BoundsRect;
          if Visible and RectIsVisible(DstRect, R) then
            // If BG is painted fully
            if (csOpaque in ControlStyle) or (Child is TGraphicControl) or (Child.Perform(SM_ALPHACMD, MakeWParam(0, AC_ISOPAQUE), 0) = 1) then
              ExcludeClipRect(DstDC, R.Left + OffsetX, R.Top + OffsetY, R.Right + OffsetX, R.Bottom + OffsetY);
        end;
      end;
      BitBlt(DstDC, DstRect.Left, DstRect.Top, WidthOf(DstRect), HeightOf(DstRect), SkinData.FCacheBmp.Canvas.Handle, SrcRect.Left, SrcRect.Top, SRCCOPY);
    finally
      RestoreDC(DstDC, SaveIndex);
    end;
  end;
end;


procedure CopyHwndCache(const hwnd: THandle; const SkinData: TsCommonData; const SrcRect, DstRect: TRect; const DstDC: HDC; const UpdateCorners: boolean;
                        const OffsetX: integer = 0; const OffsetY: integer = 0); overload;
var
  SaveIndex: HDC;
  hctrl: THandle;
  R, hR: TRect;
  Index: integer;
begin
  if UpdateCorners then
    sAlphaGraph.UpdateCorners(SkinData, 0);

  SaveIndex := SaveDC(DstDC);
  IntersectClipRect(DstDC, DstRect.Left, DstRect.Top, DstRect.Right, DstRect.Bottom);
  try
    hctrl := GetTopWindow(hwnd);
    GetWindowRect(hwnd, R);
    while hctrl <> 0 do begin
      if IsWindowVisible(hctrl) then begin
        GetWindowRect(hctrl, hR);
        OffsetRect(hR, -R.Left - OffsetX, -R.Top - OffsetY);
        if (GetWindowLong(hctrl, GWL_STYLE) and BS_GROUPBOX = BS_GROUPBOX) and
             (GetWindowLong(hctrl, GWL_EXSTYLE) and WS_EX_CLIENTEDGE = 0) {Prevent of Treeview filling} then begin

          if DefaultManager <> nil then
            with DefaultManager, SkinData.CommonSkinData, hR do begin
              Index := gd[SkinCommonInfo.Sections[ssGroupBox]].BorderIndex;
              ExcludeClipRect(DstDC, Left, Top, Right, hR.Top + SendAMessage(hctrl, AC_GETSERVICEINT));
              ExcludeClipRect(DstDC, Left, Top, Left + ma[Index].WL, Bottom);
              ExcludeClipRect(DstDC, Right - ma[Index].WR, Top, Right, Bottom);
              ExcludeClipRect(DstDC, Left, Bottom - ma[Index].WB, Right, Bottom);
            end;
        end
        else
          ExcludeClipRect(DstDC, hR.Left, hR.Top, hR.Right, hR.Bottom);
      end;
      hctrl := GetNextWindow(hctrl, GW_HWNDNEXT);
    end;
    BitBlt(DstDC, DstRect.Left, DstRect.Top, WidthOf(DstRect), HeightOf(DstRect), SkinData.FCacheBmp.Canvas.Handle, SrcRect.Left, SrcRect.Top, SRCCOPY);
  finally
    RestoreDC(DstDC, SaveIndex);
  end;
end;


function CopyCustomBorder(DC: hdc; R: TRect; SkinData: TsCommonData): TRect;
var
  l, t: integer;
begin
  Result.Top := max(SkinData.CustomBorder.RadiusTopLeft, SkinData.CustomBorder.RadiusTopRight);
  Result.Right := max(SkinData.CustomBorder.RadiusTopRight, SkinData.CustomBorder.RadiusBottomRight);
  Result.Bottom := max(SkinData.CustomBorder.RadiusBottomLeft, SkinData.CustomBorder.RadiusBottomRight);
  Result.Left := max(SkinData.CustomBorder.RadiusTopLeft, SkinData.CustomBorder.RadiusBottomLeft);
  // Top Border
  BitBlt(DC,
         R.Left, R.Top, SkinData.FCacheBmp.Width, Result.Left,
         SkinData.FCacheBmp.Canvas.Handle, R.Left, R.Top, SRCCOPY);
  // Right
  Result.Right := max(SkinData.CustomBorder.RadiusTopRight, SkinData.CustomBorder.RadiusBottomRight);
  l := R.Right - Result.Right;
  t := R.Top + SkinData.CustomBorder.RadiusTopLeft;
  BitBlt(DC,
         l, t, Result.Right, SkinData.FCacheBmp.Height - SkinData.CustomBorder.RadiusBottomRight,
         SkinData.FCacheBmp.Canvas.Handle, l, t, SRCCOPY);
  // Bottom
  l := R.Left + SkinData.CustomBorder.RadiusBottomLeft;
  t := R.Bottom - Result.Bottom;
  BitBlt(DC,
         l, t, R.Right - SkinData.CustomBorder.RadiusBottomRight, Result.Bottom,
         SkinData.FCacheBmp.Canvas.Handle, l, t, SRCCOPY);
  // Left
  t := R.Top + SkinData.CustomBorder.RadiusTopLeft;
  BitBlt(DC,
         R.Left, t, Result.Left, SkinData.FCacheBmp.Height - SkinData.CustomBorder.RadiusTopLeft,
         SkinData.FCacheBmp.Canvas.Handle, R.Left, t, SRCCOPY);

  Result.Right := SkinData.FCacheBmp.Width - Result.Right;
  Result.Bottom := SkinData.FCacheBmp.Height - Result.Bottom;
end;


procedure TsBoundLabel.AlignLabel;
var
  w: integer;
  ActLayout: TsCaptionLayout;
begin
  if Assigned(FTheLabel) and (FTheLabel.Parent <> nil) and
      (not (csLoading in FTheLabel.FocusControl.ComponentState) or (csDesigning in FTheLabel.FocusControl.ComponentState)) then
    with TAccessLabel(FTheLabel) do begin
      if Visible or (csDesigning in FTheLabel.FocusControl.ComponentState) or PreventVisible then begin
        Parent.DisableAlign;
        w := FMaxWidth;
        if Assigned(FCommonData) and (CurrentPPI <> 0) and ((FCommonData.SkinManager = nil) or not IsSysPPIUsed(FCommonData.SkinManager{.Options.ScaleMode = smVCL})) then
          w := ScaleInt(w, FCommonData);

        Constraints.MaxWidth := w;
        Width := iff(w > 0, w, 9999);
        Height := 0;
{$IFNDEF ALITE}
        TsHTMLLabel(FTheLabel).UseHTML := FUseHTML;
        TsHTMLLabel(FTheLabel).FitToWidth := True;
{$ENDIF}
        AutoSize := (Self.Layout <> sclLeftLeft) or (FMaxWidth = 0);
        WordWrap := AutoSize;
        AdjustBounds;
        Enabled := FOwnerControl.Enabled or FEnabledAlways;
        Align := alNone;
        ActLayout := Self.Layout;
        Layout := tlTop;
        if FocusControl.BidiMode = bdRightToLEft then
          case ActLayout of
            sclLeft:        ActLayout := sclRight;
            sclTopLeft:     ActLayout := sclTopRight;
            sclTopRight:    ActLayout := sclTopLeft;
            sclLeftTop:     ActLayout := sclRightTop;
            sclBottomLeft:  ActLayout := sclBottomRight;
            sclBottomRight: ActLayout := sclBottomLeft;
            sclRight:       ActLayout := sclLeft;
            sclRightTop:    ActLayout := sclLeftTop;
            sclLeftLeft:    ActLayout := sclRightRight;
          end;

        case ActLayout of
          sclLeft: begin
            if FocusControl.Align = alLeft then begin
              Align := alLeft;
              Layout := tlCenter;
              Left := FocusControl.Left - Width{$IFDEF D2007} - FocusControl.Margins.Left{$ENDIF};
            end
            else
              Left := FocusControl.Left - Width - GetSkinData(FCommonData).Spacing - Indent;

            Top  := FocusControl.Top + (FocusControl.Height - Height) div 2 - integer(FocusControl.ClientHeight <> FocusControl.Height {is TWinControl}) + FOffset;
          end;

          sclTopLeft: begin
            Left := FocusControl.Left + FOffset;
            Top  := FocusControl.Top - Height - Indent;
          end;

          sclTopCenter: begin
            Left := FocusControl.Left + (FocusControl.Width - Width) div 2 + FOffset;
            Top  := FocusControl.Top  - Height - Indent;
          end;

          sclTopRight: begin
            Left := FocusControl.Left + FocusControl.Width - Width + FOffset;
            Top  := FocusControl.Top  - Height - Indent;
          end;

          sclLeftTop: begin
            Left := FocusControl.Left - Width - GetSkinData(FCommonData).Spacing - Indent;
            Top  := FocusControl.Top + 3 + FOffset;
          end;

          sclBottomLeft: begin
            Left := FocusControl.Left + FOffset;
            Top  := FocusControl.Top + FocusControl.Height + Indent;
          end;

          sclBottomCenter: begin
            Left := FocusControl.Left + (FocusControl.Width - Width) div 2 + FOffset;
            Top  := FocusControl.Top  + FocusControl.Height + Indent;
          end;

          sclBottomRight: begin
            Left := FocusControl.Left + FocusControl.Width - Width + FOffset;
            Top  := FocusControl.Top  + FocusControl.Height + Indent;
          end;

          sclRight: begin
            Left := FocusControl.Left + FocusControl.Width + GetSkinData(FCommonData).Spacing + Indent;
            Top  := FocusControl.Top  + (FocusControl.Height - Height) div 2 - 1 + FOffset;
          end;

          sclRightTop: begin
            Left := FocusControl.Left + FocusControl.Width + GetSkinData(FCommonData).Spacing + Indent;
            Top  := FocusControl.Top  + 3 + FOffset;
          end;

          sclLeftLeft: begin
            Left := FocusControl.Left - Width - GetSkinData(FCommonData).Spacing + Indent;
            if FocusControl.Align = alLeft then begin
              Align := alLeft;
              Alignment := taLeftJustify;
            end;
            Height := GetFontHeight(Font.Handle, False);
            Layout := tlCenter;
            Top := FocusControl.Top + (FocusControl.Height - Height) div 2 - 1 + FOffset;
          end;

          sclRightRight: begin
            Left := FocusControl.Left + FocusControl.Width + GetSkinData(FCommonData).Spacing - Indent;
            if FocusControl.Align = alRight then begin
              Align := alRight;
              Alignment := taRightJustify;
            end;
            Height := Canvas.TextHeight(Caption);
            Top := FocusControl.Top + (FocusControl.Height - Height) div 2 - 1 + FOffset;
          end;
        end;
        Parent := FOwnerControl.Parent;
        Parent.EnableAlign;
      end;
    end;
end;


constructor TsBoundLabel.Create(AOwner: TObject; const CommonData: TsCommonData);
begin
  FCommonData := CommonData;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FActive := False;
  FLayout := sclLeft;
  FAllowClick := False;
  FFontUpdating := False;
  CurrentPPI := 96;
  FIndent := 0;
  FOffset := 0;
  FUseSkin := True;
  FParentFont := DefBoundLabelParentFont;
  FEnabledAlways := False;
  FMaxWidth := 0;
  FUseHTML := False;
  PreventVisible := False;
  if AOwner is TControl then
    FOwnerControl := TControl(AOwner);
end;


destructor TsBoundLabel.Destroy;
begin
  if Assigned(FTheLabel) and not (csDestroying in FTheLabel.ComponentState) then
    FreeAndNil(FTheLabel);

  FreeAndNil(FFont);
  inherited Destroy;
end;


function TsBoundLabel.DoStoreFont: boolean;
var
  ActFont, TmpFont: TFont;
begin
  if not ParentFont then begin
    TmpFont := TFont.Create;
    try
      if FTheLabel <> nil then
        ActFont := FTheLabel.Font
      else
        ActFont := FFont;

      Result := (ActFont.Color <> TmpFont.Color) or
                (ActFont.Size <> TmpFont.Size) or
  {$IFDEF D2005}
                (ActFont.Orientation <> TmpFont.Orientation) or
  {$ENDIF}
                (ActFont.Style <> TmpFont.Style) or
                (ActFont.Pitch <> TmpFont.Pitch) or
                (ActFont.Name <> TmpFont.Name);
    finally
      TmpFont.Free;
    end;
  end
  else
    Result := False;
end;


procedure TsBoundLabel.FontChanged(Sender: TObject);
begin
  if not (csLoading in FOwnerControl.ComponentState) and not FFontUpdating then begin
    ParentFont := False;
    if FTheLabel <> nil then
      FTheLabel.Font.Assign(FFont);
  end;
end;


function TsBoundLabel.GetTextRect: TRect;
begin
  if FTheLabel <> nil then begin
    Result := MkRect(FTheLabel);
    FTheLabel.DoDrawText(Result, DT_EXPANDTABS or DT_CALCRECT or DT_NOPREFIX or acAlignmentArray[FTheLabel.Alignment] or TextWrapping[FTheLabel.WordWrap]);
  end
  else
    Result := MkRect;
end;


procedure TsBoundLabel.HandleOwnerMsg(const Message: TMessage; const OwnerControl: TControl);
var
  iPPI: integer;
begin
{$IFDEF LOGGED}
//  if FOwnerControl.Tag = 5 then
    AddToLog(Message);
{$ENDIF}
  if Assigned(FTheLabel) then
    case Message.Msg of
      SM_ALPHACMD:
        if Message.WParamHi = AC_SETSCALE then
          UpdateScale(Message.LParam);

      WM_SIZE:
        if (FCommonData <> nil) and ((FCommonData.SkinManager = nil) or ([msScaleChanging, msFormScaling] * FCommonData.SkinManager.ManagerState = [])) then begin
          if FCommonData <> nil then
            iPPI := FCommonData.CommonSkinData.PPI
          else
            iPPI := GetControlPPI(FTheLabel);

          if iPPI <> CurrentPPI then
            UpdateScale(iPPI)
          else
            if SysPopupCount <= 0 then
              AlignLabel;
        end
        else
          if SysPopupCount <= 0 then
            AlignLabel;

      WM_WINDOWPOSCHANGED:
        if (FCommonData <> nil) and ((FCommonData.SkinManager = nil) or ([msScaleChanging, msFormScaling] * FCommonData.SkinManager.ManagerState = [])) then begin
          if (TWMWindowPosChanged(Message).WindowPos = nil) or (TWMWindowPosChanged(Message).WindowPos.Flags and SWP_NOSIZE <> SWP_NOSIZE) or (TWMWindowPosChanged(Message).WindowPos.Flags and SWP_NOMOVE <> SWP_NOMOVE) then begin
            if FCommonData <> nil then
              iPPI := FCommonData.CommonSkinData.PPI
            else
              iPPI := GetControlPPI(FTheLabel);

            if iPPI <> CurrentPPI then
              UpdateScale(iPPI)
            else
              if SysPopupCount <= 0 then
                AlignLabel;
          end;
        end
        else
          if SysPopupCount <= 0 then
            AlignLabel;

      CM_ENABLEDCHANGED: begin
        FTheLabel.Enabled := OwnerControl.Enabled or FEnabledAlways;
        AlignLabel;
      end;

      CM_BIDIMODECHANGED: begin
        FTheLabel.BiDiMode := OwnerControl.BiDiMode;
        AlignLabel;
      end;

      CM_VISIBLECHANGED, CM_SHOWINGCHANGED: begin
        UpdateVisibility;
        if FTheLabel <> nil then begin
          FTheLabel.Visible := OwnerControl.Visible or (csDesigning in FTheLabel.FocusControl.ComponentState);
          AlignLabel;
        end;
      end;

      CM_PARENTFONTCHANGED: begin
        if ParentFont then begin
          if FOwnerControl.Parent <> nil then
            Font.Assign(TAccessControl(FOwnerControl.Parent).Font);

          FParentFont := True;
        end;
        if FTheLabel <> nil then begin
          FTheLabel.Visible := OwnerControl.Visible or (csDesigning in FTheLabel.FocusControl.ComponentState);
          AlignLabel;
        end;
      end;
    end
  else
    case Message.Msg of
      WM_WINDOWPOSCHANGED:
        if (FCommonData <> nil) and ((FCommonData.SkinManager = nil) or ([msScaleChanging, msFormScaling] * FCommonData.SkinManager.ManagerState = [])) then
          if TWMWindowPosChanged(Message).WindowPos.Flags and SWP_SHOWWINDOW <> 0 then
            UpdateVisibility;

      CM_SHOWINGCHANGED, CM_VISIBLECHANGED:
        UpdateVisibility;

      CM_PARENTFONTCHANGED: begin
        if ParentFont and ([csLoading] * FOwnerControl.ComponentState = []) then begin
          if FOwnerControl.Parent <> nil then
            Font.Assign(TAccessControl(FOwnerControl.Parent).Font);

          FParentFont := True;
        end;
      end;
    end;
end;


procedure TsBoundLabel.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  if FTheLabel <> nil then begin
    FTheLabel.Font.Assign(Value);
    if FOwnerControl <> nil then
      FTheLabel.Parent := FOwnerControl.Parent;

    AlignLabel;
  end;
end;


procedure TsBoundLabel.SetInteger(const Index, Value: integer);

  procedure ChangeProp(var Prop: integer; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      if Active then
        AlignLabel;
    end;
  end;

begin
  case Index of
    0: ChangeProp(FIndent,   Value);
    1: ChangeProp(FMaxWidth, Value);
    2: ChangeProp(FOffset,   Value);
  end;
end;


procedure TsBoundLabel.SetLayout(const Value: TsCaptionLayout);
begin
  if FLayout <> Value then begin
    FLayout := Value;
    UpdateAlignment;
    if Active then
      AlignLabel;
  end;
end;


procedure TsBoundLabel.SetParentFont(const Value: boolean);
begin
  if Assigned(FTheLabel) then begin
    FTheLabel.ParentFont := Value;
    if not Value then
      FTheLabel.Font.Assign(Font)
    else
      Font.Assign(FTheLabel.Font);
  end;

  FParentFont := Value;
end;


procedure TsBoundLabel.SetText(const Value: acString);
begin
  if FText <> Value then begin
    FText := Value;
    if Active and (FTheLabel <> nil) then begin
      FTheLabel.Caption := Value;
      AlignLabel;
    end;
  end;
end;


procedure TsBoundLabel.SetUseSkin(const Value: boolean);
begin
  if FUseSkin <> Value then begin
    FUseSkin := Value;
    if Assigned(FTheLabel) then begin
      FTheLabel.UseSkinColor := Value;
      if not Value then
        FTheLabel.Font.Color := Font.Color;
    end;
  end;
end;


const
  LayoutsArray: array [TsCaptionLayout] of TAlignment = (taRightJustify, taLeftJustify, taCenter, taRightJustify,
           taLeftJustify,  taLeftJustify, taCenter, taRightJustify, taLeftJustify, taLeftJustify, taLeftJustify, taRightJustify);

procedure TsBoundLabel.UpdateAlignment;
begin
  if FTheLabel <> nil then
    TAccessLabel(FTheLabel).Alignment := LayoutsArray[FLayout];
end;


procedure TsBoundLabel.UpdateFontOnce(ForceScaleUpdate: boolean = False);
begin
  if not FParentFont then begin
    FTheLabel.ParentFont := False;
    FTheLabel.Font.Assign(FFont);
    if FCommonData <> nil then begin
      if (FCommonData.SkinManager = nil) or (FCommonData.SkinManager.Options.ScaleMode in [smOldMode, smVCL]) then begin // or ({ForceScaleUpdate and} (Self.CurrentPPI <> FCommonData.CommonSkinData.PPI)) then
        if not ParentFont then
          FTheLabel.Font.Height := ScaleInt(FTheLabel.Font.Height, FCommonData)
      end
      else // Changing to custom PPI value will be later
        if FCommonData.CommonSkinData.PPI = FCommonData.CurrentPPI then // If scale of OwnerControl is changed already, do it for label
          FTheLabel.Font.Height := ScaleInt(FTheLabel.Font.Height, FCommonData);

      Self.CurrentPPI := FCommonData.CommonSkinData.PPI;
    end
    else
      if (DefaultManager = nil) or (DefaultManager.Options.ScaleMode in [smOldMode, smVCL]) then begin
        Self.CurrentPPI := DefaultPPI;
        FTheLabel.Font.Height := MulDiv(FTheLabel.Font.Height, Self.CurrentPPI, 96);
      end;

    if not FUseSkin then begin
      FTheLabel.UseSkinColor := False;
      FTheLabel.Font.Color := Font.Color;
    end;
  end
  else begin
    FTheLabel.ParentFont := True;
    if FOwnerControl.Parent <> nil then begin
      FFontUpdating := True;
      Font.Assign(TAccessControl(FOwnerControl.Parent).Font);
      FFontUpdating := False;
    end;
  end;
end;


procedure TsBoundLabel.UpdateScale(NewValue: integer);
begin
  if CurrentPPI <> NewValue then
    if (FTheLabel <> nil) and Active then begin
      AlignLabel;
      CurrentPPI := NewValue;
    end;
end;


procedure TsBoundLabel.UpdateVisibility;
begin
  if ([csLoading, csReading] * FOwnerControl.ComponentState = []) or ([csReading, csDesigning] * FOwnerControl.ComponentState = [csReading, csDesigning]) then begin
    if FActive then begin
      if FTheLabel = nil then begin
        FTheLabel := TsEditLabel.InternalCreate(Self);
        UpdateFontOnce(True);
      end;
      with FTheLabel do begin
        Visible := False;
        FocusControl := TWinControl(FOwnerControl);
        UseHtml := Self.UseHTML;
        Name := FocusControl.Name + 'Label';
        if FText = '' then
          FText := FocusControl.Name;

        Caption := FText;
        Parent := FocusControl.Parent;
        UpdateAlignment;

        Visible := not PreventVisible and (FocusControl.Visible or (csDesigning in FOwnerControl.ComponentState));
        Enabled := FocusControl.Enabled or FEnabledAlways;
        AutoSize := True;
      end;
      AlignLabel;
    end
    else
      FreeAndNil(FTheLabel);
  end
end;


constructor TacScrollData.Create(AOwner: TsCommonData);
begin
  FOwner := AOwner;
  FScrollWidth := -1;
  FButtonsSize := -1;
end;


procedure TacScrollData.Invalidate;
begin
//
end;


procedure TacScrollData.SetSize(const Index, Value: integer);

  procedure ChangeProp(var Prop: integer; Value: integer);
  begin
    if Prop <> Value then begin
      Prop := Value;
      if (FOwner.FOwnerControl is TWinControl) and not (csLoading in FOwner.FOwnerControl.ComponentState) then
        if TWinControl(FOwner.FOwnerControl).HandleAllocated then
          if Index = 0 then
            SetWindowPos(TWinControl(FOwner.FOwnerControl).Handle, 0, 0, 0, 0, 0, SWPA_FRAMECHANGED)
          else
            TrySendMessage(TWinControl(FOwner.FOwnerControl).Handle, WM_NCPAINT, 0, 0);
    end;
  end;

begin
  case Index of
    0: ChangeProp(FScrollWidth, Value);
    1: ChangeProp(FButtonsSize, Value);
  end;
end;


constructor TsScrollWndData.Create(AOwner: TObject; const CreateCacheBmp: boolean = True);
begin
  inherited;
  FVertScrollData := TacScrollData.Create(Self);
  FHorzScrollData := TacScrollData.Create(Self);
end;


destructor TsScrollWndData.Destroy;
begin
  FreeAndNil(FVertScrollData);
  FreeAndNil(FHorzScrollData);
  inherited;
end;


constructor TacOuterEffects.Create(AOwner: TsCommonData);
begin
  FOwner := AOwner;
  FVisibility := ovNone;
end;


procedure TacOuterEffects.Invalidate;
begin
  if (FOwner.FOwnerControl <> nil) and (FOwner.FOwnerControl.Parent <> nil) then
    with FOwner.FOwnerControl.Parent do
      if HandleAllocated and ([csLoading, csDestroying] * ComponentState = []) then begin
        SendAMessage(Handle, AC_INVALIDATE);
        FOwner.Invalidate;
      end;
end;


function TacOuterEffects.IsShown: boolean;
begin
  with FOwner do
    Result := ((OuterEffects.Visibility = ovAlways) or (OuterEffects.Visibility = ovFocused) and (FFocused or (FOwnerControl is TWinControl) and TWinControl(FOwnerControl).Focused));
end;


procedure TacOuterEffects.SetVisibility(const Value: TacOuterVisibility);
begin
  if FVisibility <> Value then begin
    FVisibility := Value;
    if (FOwner.FOwnerControl <> nil) and not (csLoading in FOwner.FOwnerControl.ComponentState) then
      Invalidate;
  end;
end;


procedure TsBoundLabel.SetBoolean(const Index: integer; const Value: boolean);
begin
  case Index of
    0: if FUseHTML <> Value then begin
      FUseHTML := Value;
      AlignLabel;
    end;

    1: if FActive <> Value then begin
      FActive := Value;
      UpdateVisibility;
      SetBoolean(2, FAllowClick); // Init the event
    end;
{
    2: begin
      FAllowClick := Value;
      if FTheLabel <> nil then
        if Value then begin
          FTheLabel.OnClick := DoClick;
          FTheLabel.OnDblClick := DoClick;
        end
        else begin
          FTheLabel.OnClick := nil;
          FTheLabel.OnDblClick := nil;
        end;
    end;
}
    3: if FEnabledAlways <> Value then begin
      FEnabledAlways := Value;
      if Active then
        AlignLabel;
    end;
  end;
end;


procedure TsBoundLabel.DoClick(Sender: TObject);
begin
  if (FOwnerControl <> nil) and not (csDesigning in FOwnerControl.ComponentState) then
    TAccessControl(FOwnerControl).Click;
end;


type
  TacAccessListSW = class(TacScrollWnd);

procedure TsCommonData.InitCommonProp;
var
  h: THandle;
begin
  h := OwnerHandle;
  if h <> 0 then begin
    if FOwnerObject is TsSkinProvider then begin
      if TsSkinProvider(FOwnerObject).ListSW <> nil then
        WndProc := TacAccessListSW(TsSkinProvider(FOwnerObject).ListSW).acWndProc
    end
    else
      if not Assigned(WndProc) then
        if FOwnerControl is TWinControl then
          WndProc := TWinControl(FOwnerControl).WindowProc
        else
          WndProc := nil;

    SetProp(h, acPropStr, THandle(Self));
  end;
end;


function RefreshNeeded(SkinData: TsCommonData; Message: TMessage): boolean;

  function IsScroll: boolean;
  begin
    with SkinData, SkinManager.SkinCommonInfo do
      Result := Message.WParamLo in [Byte(Sections[ssScrollBtnLeft]),
                                     Byte(Sections[ssScrollBtnTop]),
                                     Byte(Sections[ssScrollBtnRight]),
                                     Byte(Sections[ssScrollBtnBottom]),
                                     Byte(Sections[ssScrollBar1H]),
                                     Byte(Sections[ssScrollBar1V]),
                                     Byte(Sections[ssScrollBar2H]),
                                     Byte(Sections[ssScrollBar2V]),
                                     Byte(Sections[ssScrollSliderH]),
                                     Byte(Sections[ssScrollSliderV])];
  end;

  function IsTab: boolean;
  var
    l: TacTabLayout;
    s: TacSide;
  begin
    for l := Low(l) to High(l) do
      for s := Low(s) to High(s) do
        with SkinData, SkinManager.SkinCommonInfo do
          if Message.WParamLo = SkinData.CommonSkinData.Tabs[l][s].SkinIndex then begin
            Result := True;
            Exit;
          end;

    Result := False;
  end;

begin
  if ACUInt(Message.LParam) = ACUInt(SkinData.SkinManager) then begin
    if Byte(Message.WParamLo) in [0, Byte(SkinData.SkinIndex)] then
      Result := True
    else
      // Check if additional sections are used
      with SkinData, SkinManager.SkinCommonInfo do
        case COC of
          COC_TsGauge:     Result := Message.WParamLo in [Byte(Sections[ssProgressH]), Byte(Sections[ssProgressV])];
          COC_TsSpinEdit:  Result := Message.WParamLo in [Byte(Sections[ssToolButton]), Byte(Sections[ssUpDown])];
          COC_TsSlider:    Result := Message.WParamLo in [Byte(Sections[ssSlider_On]), Byte(Sections[ssSlider_Off]), Byte(Sections[ssThumb_On]), Byte(Sections[ssThumb_Off])];
          COC_TsCoolBar:   Result := Message.WParamLo in [Byte(Sections[ssGripV]), Byte(Sections[ssGripH])];
          COC_TsToolBar:   Result := Message.WParamLo = Sections[ssDivider];
          COC_TsSplitter:  Result := Message.WParamLo = Sections[ssSpeedButton];
          COC_TsHeaderControl,
          COC_TsListView:  Result := IsScroll or (Message.WParamLo in [Byte(Sections[ssColHeader]), Byte(Sections[ssColHeaderA]), Byte(Sections[ssColHeaderL]), Byte(Sections[ssColHeaderR])]);

          COC_TsPageControl, COC_TsTabControl:
            Result := (Message.WParamLo = Sections[ssMenuButton]) or IsTab;

          COC_TacDialogWnd, COC_TsSkinProvider:
            Result := Message.WParamLo = Sections[ssFormTitle];
          // Controls with scrolls
          COC_TsMemo, COC_TsListBox, COC_TsDBGrid, COC_TsDBMemo, COC_TsDBListBox, COC_TsTreeView, COC_TsAdapterEdit, COC_TsScrollBox, COC_TsScrollBar:
            Result := IsScroll;
          // ComboBtn
          COC_TsColorBox, COC_TsComboBox, COC_TsComboBoxEx, COC_TsDBComboBox, COC_TsDBLookupComboBox:
            Result := Message.WParamLo = Sections[ssComboBtn];

          COC_TsFileDirEdit..COC_TsDateEdit:
            Result := Message.WParamLo in [Byte(Sections[ssComboBtn]), Byte(Sections[ssUpDown]), Byte(Sections[ssSpeedButton_Small])];

          else
            Result := False;
        end;
  end
  else
    Result := False;
end;


procedure TsCommonData.RemoveCommonProp;
var
  h: THandle;
begin
  if not Application.Terminated then begin
    h := OwnerHandle;
    if h <> 0 then
      RemoveProp(h, acPropStr);
  end;
end;


procedure TsCommonData.ClearLinks;
begin
  FOwnerControl := nil;
  FOwnerObject := nil;
  FSWHandle := 0;
end;


function SkinIsChanging(const SkinData: TsCommonData): boolean;
begin
  if SkinData.SkinManager <> nil then
    Result := msSkinChanging in SkinData.SkinManager.ManagerState
  else
    Result := False;
end;


function TextHintColor(SkinData: TsCommonData; AState: integer; DontBlend: boolean): TColor;
var
  Skinned: boolean;
begin
  Skinned := SkinData.Skinned;
  with TacAccessControl(SkinData.FOwnerControl) do begin
    if SkinData.CustomFont or not Skinned then
      Result := acColorToRGB(Font.Color)
    else
      Result := GetFontColor(SkinData.FOwnerControl, SkinData.SkinIndex, SkinData.SkinManager, iff(AState <= 0, integer(ControlIsActive(SkinData)), AState), SkinData);

    if not DontBlend then
      if SkinData.CustomColor or not Skinned then
        Result := BlendColors(acColorToRGB(Result), acColorToRGB(Color), ac_TextHintBlend)
      else
        Result := BlendColors(acColorToRGB(Result), GetControlColor(SkinData.FOwnerControl), ac_TextHintBlend);
  end;
end;


procedure TextHintOut(SkinData: TsCommonData; R: TRect; TextHint: acString; Flags: Cardinal; AState: integer = -1);
begin
  if TextHint <> '' then begin
    SkinData.FCacheBMP.Canvas.Font.Color := TextHintColor(SkinData, AState, Flags and DT_ALPHATEXT <> 0);
    SkinData.FCacheBMP.Canvas.Brush.Style := bsClear;
    if Flags and DT_ALPHATEXT = 0 then
      acDrawText(SkinData.FCacheBMP.Canvas.Handle, TextHint, R, Flags)
    else
      if not acgpDrawString(SkinData.FCacheBMP.Canvas.Handle, TextHint, R, (ac_TextHintBlend shl 24) or SwapRedBlue(SkinData.FCacheBMP.Canvas.Font.Color), AlignmentFromInt(Flags), VertAlignmentFromInt(Flags), 0, True) then
        // If GDI+ can't use current font, try again without DT_ALPHATEXT
        WriteColor(SkinData.FCacheBMP, PacChar(TextHint), True, R, Flags and not DT_ALPHATEXT, TextHintColor(SkinData, AState, False));
  end;
end;


procedure HideBoundLabel(BoundLabel: TsBoundLabel);
var
  R: TRect;
  h: THandle;
begin
  if BoundLabel.Active and (BoundLabel.FTheLabel.Visible) then begin
    R := BoundLabel.FTheLabel.BoundsRect;
    h := BoundLabel.FTheLabel.Parent.Handle;
    BoundLabel.FCommonData.CtrlSkinState := BoundLabel.FCommonData.CtrlSkinState and not ACS_TEXTANIMATING;
    BoundLabel.Active := False;
    RedrawWindow(h, @R, 0, RDW_ERASE or RDW_INVALIDATE or RDW_UPDATENOW);
    BoundLabel.FCommonData.Invalidate;
  end;
end;


destructor TsCtrlSkinData.Destroy;
begin
  DelLightControl(Self);
  FreeAndNil(LightMask);
  FreeAndNil(HotCache);
  inherited;
end;


procedure TsCtrlSkinData.DestroyNeedlessLight(CheckTimer: boolean = True);
begin
  if not CheckTimer then begin
    FreeAndNil(LightMask);
    FreeAndNil(HotCache);
    FDoLighting := False
  end
  else
    if (LightMask <> nil) and not DoLighting then begin
      FreeAndNil(LightMask);
      FreeAndNil(HotCache);
      BGChanged := True;
    end;
end;


function TsCtrlSkinData.DoLighting(CheckTimer: boolean = True): boolean;
begin
  Result := (CheckTimer and FDoLighting) or FMouseAbove and (DefaultManager <> nil) and DefaultManager.Effects.AllowAnimation and
    (beLighting in DefaultManager.AnimEffects.Buttons.Events) and (DefaultManager.AnimEffects.Buttons.LightingMode = lmActiveControl) and
    ((FOwnerControl = nil) or not (csDesigning in FOwnerControl.ComponentState));
end;


procedure TsCtrlSkinData.Loaded(UpdateColors: boolean = True);
begin
  inherited;
  if (FOwnerControl <> nil) and (COC in ssLightCtrls) then
    AddLightControl(Self);
end;

end.


