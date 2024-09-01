{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{           copyright (c)  2016 - 2022                               }
{            Email : info@tmssoftware.com                            }
{            Web : https://www.tmssoftware.com                       }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit FMX.TMSFNCHint;

{$I FMX.TMSFNCDefines.inc}

{$IFDEF FMXLIB}
{$HINTS OFF}
{$IF COMPILERVERSION > 29}
{$DEFINE SUPPORTSHINTS}
{$IFEND}
{$HINTS ON}
{$ENDIF}

{$IFDEF CMNLIB}
{$DEFINE SUPPORTSHINTS}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}
  {%H-}Windows,
  {$ENDIF}
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  MacApi.Foundation, MacApi.CocoaTypes, MacApi.AppKit, TypInfo, MacApi.ObjectiveC,
  {$ENDIF}
  {$ENDIF}
  Classes, FMX.Graphics, FMX.TMSFNCGraphics, FMX.Controls, FMX.TMSFNCCustomComponent,
  Types, FMX.Forms, FMX.TMSFNCTypes, FMX.TMSFNCBitmapContainer, FMX.TMSFNCGraphicsTypes
  {$IFDEF FMXLIB}
  ,FMX.Types, Messaging
  {$ENDIF}
  {$IFDEF FMXLIB}
  {$IFDEF SUPPORTSHINTS}
  {$IFDEF MSWINDOWS}
  ,FMX.Platform.Win
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,LCLType
  {$ENDIF}
  ;

{$IFDEF FMXLIB}
{$IFDEF SUPPORTSHINTS}
const
  HintDelay = 500;
  HintHideDelay = HintDelay * 5;
  HintOffsetX = 0;
  HintOffsetY = 20;
{$ENDIF}
{$ENDIF}

type
  TTMSFNCCustomHint = class;
  TTMSFNCHintWindow = class;

  {$IFNDEF SUPPORTSHINTS}
  TTMSFNCHintWindow = class
  {$ENDIF}
  {$IFDEF FMXLIB}
  {$IFDEF SUPPORTSHINTS}
  TTimerMode = (tmShow, tmHide);
  {$IFDEF MSWINDOWS}
  LInteger = LONG_PTR;
  IntPtr = Pointer;
  {$ENDIF}
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  NSHintView = interface(NSView)
  ['{A62014BA-D0DC-49A0-8212-7894F0F019E1}']
    procedure drawRect(dirtyRect: NSRect); cdecl;
  end;

  TTMSFNCNativeHintWindow = class(TOCLocal)
  private
    FView: TTMSFNCHintWindow;
  public
    constructor Create;
    function GetObjectiveCClass: PTypeInfo; override;
    procedure drawRect(dirtyRect: NSRect); cdecl;
  end;
  {$ENDIF}
  {$ENDIF}
  TTMSFNCHintWindow = class(THint)
    {$IFDEF MACOS}
    {$IFNDEF IOS}
  const
    InvalidToolTipHandle: NSToolTipTag = $FFFFFFFF;
    {$ENDIF}
    {$ENDIF}
  private
    FHintString: string;
    FTimerMode: TTimerMode;
    {$IFDEF MACOS}
    {$IFNDEF IOS}
    FNativeView: NSView;
    FNativeHint: TTMSFNCNativeHintWindow;
    FNativeHintView: NSView;
    FTimer: TTimer;
    {$ENDIF}
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    FWindowHandle: TWinWindowHandle;
    FTimerHandle: Cardinal;
    FNativeControlHandle: HWND;
    FToolTipHandle: HWND;
    {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$IFDEF CMNLIB}
  TTMSFNCHintWindow = class(THintWindow)
  private
  {$ENDIF}
    FHint: TTMSFNCCustomHint;
  protected
    function FindHintControl: TTMSFNCCustomHint;
    function Calculate(AHint: string): TRect; virtual;
    function Draw(ACanvas: TCanvas; ARect: TRect; AHint: String): TTMSFNCBitmap; virtual;
    {$IFDEF FMXLIB}
    {$IFDEF SUPPORTSHINTS}
    procedure SetEnabled(const Value: Boolean); override;
    {$IFDEF MACOS}
    {$IFNDEF IOS}
    function GetHintFrame(APosition: NSPoint; AOffsetX, AOffsetY: Integer): CGRect;
    {$ENDIF}
    {$ENDIF}
    {$IFNDEF MSWINDOWS}
    procedure HintTimerProc(Sender: TObject);
    {$ENDIF}
    procedure StartHintTimer(Value: NativeUInt; TimerMode: TTimerMode);
    procedure StopHintTimer;
    {$ENDIF}
    {$ENDIF}
  public
    {$IFDEF CMNLIB}
    constructor Create(const AOwner: TComponent; const {%H-}AShadow: Boolean); reintroduce; virtual;
    {$IFDEF MSWINDOWS}
    procedure CreateParams(var Params: TCreateParams); override;
    {$ENDIF}
    procedure Paint; override;
    {$IFDEF VCLLIB}
    function CalcHintRect(MaxWidth: Integer; const AHint: string; AData: TCustomData): TRect; override;
    {$ENDIF}
    {$IFDEF LCLLIB}
    function CalcHintRect(MaxWidth: Integer; const AHint: String; AData: pointer): TRect; override;
    {$ENDIF}
    {$ENDIF}
    {$IFDEF FMXLIB}
    {$IFDEF SUPPORTSHINTS}
    constructor Create(const AHandle: TWindowHandle; const AShadow: Boolean); reintroduce; virtual;
    destructor Destroy; override;
    procedure SetHint(const AString: string); override;
    procedure SetPosition(const X, Y: Single); override;
    {$ENDIF}
    {$ENDIF}
    procedure ShowHintAt(AHintControl: TTMSFNCCustomHint; AHint: string; X, Y: Integer); virtual;
    procedure HideHint; virtual;
  end;

  TTMSFNCHintBeforeDrawHintEvent = procedure(Sender: TObject; AGraphics: TTMSFNCGraphics; AHint: string; ARect: TRect; var ADefaultDraw: Boolean) of object;
  TTMSFNCHintAfterDrawHintEvent = procedure(Sender: TObject; AGraphics: TTMSFNCGraphics; AHint: string; ARect: TRect) of object;
  TTMSFNCHintCalculateHintRectEvent = procedure(Sender: TObject; AHint: String; var ARect: TRect) of object;

  TTMSFNCCustomHint = class(TTMSFNCCustomComponent, ITMSFNCBitmapContainer)
  private
    {$IFDEF CMNLIB}
    FPrevHintWindowClass: THintWindowClass;
    {$ENDIF}
    FBitmapContainer: TTMSFNCBitmapContainer;
    FFill: TTMSFNCGraphicsFill;
    FStroke: TTMSFNCGraphicsStroke;
    FFont: TTMSFNCGraphicsFont;
    FOnCalculateHintRect: TTMSFNCHintCalculateHintRectEvent;
    FOnBeforeDrawHint: TTMSFNCHintBeforeDrawHintEvent;
    FOnAfterDrawHint: TTMSFNCHintAfterDrawHintEvent;
    FShadow: Boolean;
    function GetBitmapContainer: TTMSFNCBitmapContainer;
    procedure SetBitmapContainer(const Value: TTMSFNCBitmapContainer);
    procedure SetFill(const Value: TTMSFNCGraphicsFill);
    procedure SetStroke(const Value: TTMSFNCGraphicsStroke);
    procedure SetFont(const Value: TTMSFNCGraphicsFont);
    procedure SetShadow(const Value: Boolean);
  protected
    function GetInstance: NativeUInt; override;
    function GetDocURL: string; override;
    procedure DoBeforeDrawHint(AGraphics: TTMSFNCGraphics; AHint: string; ARect: TRect; var ADefaultDraw: Boolean);
    procedure DoAfterDrawHint(AGraphics: TTMSFNCGraphics; AHint: string; ARect: TRect);
    procedure DoCalculateHintRect(AHint: String; var ARect: TRect);
    {$IFDEF FMXLIB}
    {$IFDEF SUPPORTSHINTS}
    procedure CreateFormsHandler(const Sender: TObject; const M: TMessage);
    {$ENDIF}
    {$ENDIF}
    procedure ChangeDPIScale({%H-}M, {%H-}D: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property BitmapContainer: TTMSFNCBitmapContainer read GetBitmapContainer write SetBitmapContainer;
    property Fill: TTMSFNCGraphicsFill read FFill write SetFill;
    property Stroke: TTMSFNCGraphicsStroke read FStroke write SetStroke;
    property Font: TTMSFNCGraphicsFont read FFont write SetFont;
    property Shadow: Boolean read FShadow write SetShadow default True;
    property OnBeforeDrawHint: TTMSFNCHintBeforeDrawHintEvent read FOnBeforeDrawHint write FOnBeforeDrawHint;
    property OnAfterDrawHint: TTMSFNCHintAfterDrawHintEvent read FOnAfterDrawHint write FOnAfterDrawHint;
    property OnCalculateHintRect: TTMSFNCHintCalculateHintRectEvent read FOnCalculateHintRect write FOnCalculateHintRect;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
    function Calculate(AHint: string): TRect; virtual;
  end;

  {$IFNDEF LCLLIB}
  [ComponentPlatformsAttribute(TMSPlatformsDesktop)]
  {$ENDIF}
  TTMSFNCHint = class(TTMSFNCCustomHint)
  private
    {$IFDEF SUPPORTSHINTS}
    FHintWindow: TTMSFNCHintWindow;
    {$ENDIF}
  protected
    procedure RegisterRuntimeClasses; override;
  public
    function IsShowing: Boolean;
    procedure ShowHintAt(AHint: string; X, Y: Integer);
    procedure HideHint;
    destructor Destroy; override;
  published
    property OnCalculateHintRect;
    property OnBeforeDrawHint;
    property OnAfterDrawHint;
    property BitmapContainer;
    property Fill;
    property Stroke;
    property Font;
    property Shadow;
  end;

implementation

{$R 'TMSFNCHint.res'}

uses
  SysUtils, FMX.TMSFNCUtils
  {$IFDEF FMXLIB}
  {$IFDEF SUPPORTSHINTS}
  ,Rtti
  {$IFDEF MSWINDOWS}
  ,CommCtrl, Messages
  {$ENDIF}
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  ,Macapi.Helpers, MacApi.CoreGraphics, FMX.Platform.Mac
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  ;

{$IFDEF FMXLIB}
{$IFDEF SUPPORTSHINTS}
{$IFDEF MSWINDOWS}
var
  PrevWndProc: LInteger;
{$ENDIF}
{$IFDEF MACOS}
{$IFNDEF IOS}
type
  TTMSFNCMacBitmap = class
  private
    {$HINTS OFF}
    FData: Pointer;
    {$HINTS ON}
    FContext: CGContextRef;
    FImage: CGImageRef;
    function GetImage: CGImageRef;
  end;

function TTMSFNCMacBitmap.GetImage: CGImageRef;
begin
  if FImage = nil then
    FImage := CGBitmapContextCreateImage(FContext);
  Result := FImage;
end;
{$ENDIF}
{$ENDIF}
{$ENDIF}
{$ENDIF}

{ TTMSFNCCustomHint }

function TTMSFNCHintWindow.FindHintControl: TTMSFNCCustomHint;
  {$IFDEF FMXLIB}
  function FindHintComponent(frm: TCommonCustomForm): TTMSFNCCustomHint;
  {$ENDIF}
  {$IFDEF CMNLIB}
  function FindHintComponent(frm: TForm): TTMSFNCCustomHint;
  {$ENDIF}
  var
    I: Integer;
  begin
    Result := nil;
    if Assigned(frm) then
    begin
      for I := 0 to frm.ComponentCount - 1 do
      begin
        if frm.Components[I] is TTMSFNCCustomHint then
        begin
          Result := TTMSFNCCustomHint(frm.Components[I]);
          Break;
        end;
      end;
    end;
  end;

begin
  Result := FindHintComponent(Screen.ActiveForm);
  if not Assigned(Result) then
    Result := FindHintComponent(Application.MainForm);
end;

{$IFDEF CMNLIB}
{$IFDEF VCLLIB}
function TTMSFNCHintWindow.CalcHintRect(MaxWidth: Integer; const AHint: string; AData: TCustomData): TRect;
{$ENDIF}
{$IFDEF LCLLIB}
function TTMSFNCHintWindow.CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect;
{$ENDIF}
begin
  inherited;
  if not Assigned(FHint) then
    FHint := FindHintControl;
  Result := Calculate(AHint);
end;

{$IFDEF MSWINDOWS}
procedure TTMSFNCHintWindow.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  if Assigned(FHint) and not FHint.Shadow then
    Params.WindowClass.Style := Params.WindowClass.style and not CS_DROPSHADOW;
end;
{$ENDIF}

procedure TTMSFNCHintWindow.Paint;
begin
  Draw(Canvas, ClientRect, Caption);
end;
{$ENDIF}

function TTMSFNCHintWindow.Calculate(AHint: String): TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if not Assigned(FHint) then
    Exit;
  Result := FHint.Calculate(AHint);
end;

function TTMSFNCHintWindow.Draw(ACanvas: TCanvas; ARect: TRect; AHint: String): TTMSFNCBitmap;
var
  g: TTMSFNCGraphics;
  b: Boolean;

  procedure DrawHTMLText(AGraphics: TTMSFNCGraphics);
  var
    r: TRectF;
  begin
    AGraphics.Font.Assign(FHint.Font);
    r := ConvertToRectF(ARect);
    AGraphics.DrawText(r, AHint, False, gtaCenter, gtaCenter);
  end;

begin
  Result := nil;
  if not Assigned(FHint) then
    Exit;

  if ACanvas = nil then
    g := TTMSFNCGraphics.CreateBitmapCanvas(ARect.Right - ARect.Left, ARect.Bottom - ARect.Top)
  else
    g := TTMSFNCGraphics.Create(ACanvas);

  try
    if ACanvas = nil then
      g.BeginScene;

    g.Fill.Assign(FHint.Fill);
    g.Stroke.Assign(FHint.Stroke);
    g.DrawRectangle(ConvertToRectF(ARect));
    b := True;
    FHint.DoBeforeDrawHint(g, AHint, ARect, b);
    if b then
    begin
      g.BitmapContainer := FHint.BitmapContainer;
      DrawHTMLText(g);
      FHint.DoAfterDrawHint(g, AHint, ARect);
    end;

    if ACanvas = nil then
    begin
      g.EndScene;
      Result := TTMSFNCBitmap.Create;
      Result.Assign(g.Bitmap);
    end;
  finally
    g.Free;
  end;
end;

{$IFDEF CMNLIB}
constructor TTMSFNCHintWindow.Create(const AOwner: TComponent; const AShadow: Boolean);
begin
  inherited Create(AOwner);
end;
{$ENDIF}

{$IFDEF FMXLIB}
{$IFDEF SUPPORTSHINTS}
{$IFDEF MSWINDOWS}
procedure HintTimerProc(Wnd: HWnd; Msg: UINT; TimerID: UINT_PTR; SysTime: DWORD); stdcall;
{$ELSE}
procedure TTMSFNCHintWindow.HintTimerProc(Sender: TObject);
{$ENDIF}
var
  {$IFDEF MSWINDOWS}
  r: TRect;
  p: TPointF;
  sz: TSize;
  sc: Single;
  {$ENDIF}
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  w: NSWindow;
  p: NSPoint;
  {$ENDIF}
  {$ENDIF}
  h: TTMSFNCHintWindow;
begin
  {$IFDEF MSWINDOWS}
  h := TTMSFNCHintWindow(GetWindowLongPtr(Wnd, GWL_USERDATA));
  {$ENDIF}
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  h := Self;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF ANDROID}
  h := nil;
  {$ENDIF}
  if Assigned(h) then
  begin
    h.StopHintTimer;

    case h.FTimerMode of
      tmShow:
      begin
        {$IFDEF MSWINDOWS}
        r := h.Calculate(h.FHintString);
        p := TTMSFNCUtils.GetMousePos;
        sc := TTMSFNCUtils.GetDPIScale(h.FHint);
        OffsetRect(r, Round((p.X + HintOffsetX) * sc), Round((p.Y + HintOffsetY) * sc));
        SetWindowPos(h.FToolTipHandle, HWND_TOPMOST, r.Left, r.Top, r.Width, r.Height, SWP_NOACTIVATE);
        sz.cx := r.Width;
        sz.cy := r.Height;
      	{$HINTS OFF}
        {$IF COMPILERVERSION > 30}
        h.FWindowHandle.ClientSize := sz;
        {$IFEND}
      	{$HINTS ON}
        ShowWindow(h.FToolTipHandle, SW_SHOWNOACTIVATE);
        InvalidateRect(h.FToolTipHandle, nil, True);
        {$ENDIF}
        {$IFDEF MACOS}
        {$IFNDEF IOS}
        w := FNativeView.window;
        if Assigned(w) then
        begin
          p := w.mouseLocationOutsideOfEventStream;
          FNativeHintView.setFrame(GetHintFrame(p, HintOffsetX, HintOffsetY));
          FNativeView.addSubview(FNativeHintView);
        end;
        {$ENDIF}
        {$ENDIF}
        h.StartHintTimer(HintHideDelay, tmHide);
      end;
      tmHide:
      begin
        {$IFDEF MSWINDOWS}
        ShowWindow(h.FToolTipHandle, SW_HIDE);
        {$ENDIF}
        {$IFDEF MACOS}
        {$IFNDEF IOS}
        FNativeHintView.removeFromSuperView;
        {$ENDIF}
        {$ENDIF}
      end;
    end;
  end;
end;

{$IFDEF MSWINDOWS}
function WindowProc(hwnd: HWND; uMsg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
begin
  Result := DefWindowProc(hwnd, uMsg, wParam, lParam);
end;

function HintWindowProc(hWnd: hWnd; uMsg: Integer; WParam: WParam;
  lParam: lParam): LRESULT; stdcall;
var
  ca: TCanvas;
  h: TTMSFNCHintWindow;
  r: TRect;
  {$IFDEF FMXLIB}
  hs: Single;
  {$ENDIF}
begin
  Result := CallWindowProc(IntPtr(PrevWndProc), hWnd, uMsg, WParam, lParam);
  if (uMsg = WM_PAINT) then
  begin
    h := TTMSFNCHintWindow(GetWindowLongPtr(hWnd, GWL_USERDATA));
    if Assigned(h) then
    begin
      GetClientRect(hWnd, r);
      {$IFDEF FMXLIB}
      hs := TTMSFNCUtils.GetDPIScale(h.FHint);
      {$ELSE}
      hs := 1;
      {$ENDIF}
      ca := TCanvasManager.CreateFromWindow(h.FWindowHandle, Round(r.Width * hs), Round(r.Height * hs));
      try
        if ca.BeginScene then
        begin
          r.Left := Round(r.Left / hs);
          r.Top := Round(r.Top / hs);
          r.Right := Round(r.Right / hs);
          r.Bottom := Round(r.Bottom / hs);
          h.Draw(ca, r, h.FHintString);
          ca.EndScene;
        end;
      finally
        ca.Free;
      end;
    end;
  end;
end;

const
  HintTimerDelegate: TFNTimerProc = @HintTimerProc;

{$ENDIF}

procedure TTMSFNCHintWindow.StartHintTimer(Value: NativeUInt; TimerMode: TTimerMode);
begin
  StopHintTimer;
  {$IFDEF MSWINDOWS}
  FTimerHandle := SetTimer(FToolTipHandle, 0, Value, HintTimerDelegate);
  if FTimerHandle = 0 then
    ShowWindow(FToolTipHandle, SW_HIDE);
  {$ENDIF}
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  FTimer := TTimer.Create(nil);
  FTimer.Interval := Value;
  FTimer.OnTimer := HintTimerProc;
  FTimer.Enabled := True;
  {$ENDIF}
  {$ENDIF}
  FTimerMode := TimerMode;
end;

procedure TTMSFNCHintWindow.StopHintTimer;
begin
  {$IFDEF MSWINDOWS}
  if FTimerHandle <> 0 then
  begin
    KillTimer(0, FTimerHandle);
    FTimerHandle := 0;
  end;
  {$ENDIF}
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  if Assigned(FTimer) then
  begin
    FTimer.Free;
    FTimer := nil;
    FNativeHintView.removeFromSuperview;
  end;
  {$ENDIF}
  {$ENDIF}
end;

procedure TTMSFNCHintWindow.SetEnabled(const Value: Boolean);
begin
  if Value <> FEnabled then
  begin
    inherited;
    if Value then
      StartHintTimer(HintDelay, tmShow)
    else
      StartHintTimer(0, tmHide);
  end;
end;

{$IFDEF MACOS}
{$IFNDEF IOS}
function TTMSFNCHintWindow.GetHintFrame(APosition: NSPoint; AOffsetX, AOffsetY: Integer): CGRect;
var
  r: TRect;
  p: NSPoint;
begin
  r := Calculate(FHintString);
  p := APosition;
  Result.origin.x := p.x + AOffsetX;
  Result.origin.y := p.y - r.Height - AOffsetY;
  Result.size.width := r.Width;
  Result.size.height := r.Height;
end;
{$ENDIF}
{$ENDIF}

constructor TTMSFNCHintWindow.Create(const AHandle: TWindowHandle; const AShadow: Boolean);
{$IFDEF MSWINDOWS}
var
  HintWndClass: TWndClassEx;
{$ENDIF}
begin
  inherited Create(AHandle);
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  FNativeView := TNSView.Wrap(WindowHandleToPlatform(AHandle).Wnd.contentView);
  FNativeHint := TTMSFNCNativeHintWindow.Create;
  FNativeHint.FView := Self;
  FNativeHintView := TNSView.Wrap(FNativeHint.GetObjectID);
  {$ENDIF}
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  FNativeControlHandle := WindowHandleToPlatform(AHandle).Wnd;
  if FNativeControlHandle <> 0 then
  begin
    HintWndClass.cbSize := SizeOf(TWndClassEx);
    HintWndClass.Style := CS_SAVEBITS;
    if AShadow then
      HintWndClass.style := HintWndClass.style or CS_DROPSHADOW;
    HintWndClass.lpfnWndProc := @WindowProc;
    HintWndClass.cbClsExtra := 0;
    HintWndClass.cbWndExtra := 0;
    HintWndClass.hInstance := hInstance;
    HintWndClass.hIcon := 0;
    HintWndClass.hCursor := LoadCursor(0, IDC_ARROW);
    HintWndClass.hbrBackground := COLOR_WINDOW;
    HintWndClass.lpszMenuName := nil;
    HintWndClass.lpszClassName := 'TTMSFNCHintWindow';
    HintWndClass.hIconSm := 0;
    Windows.RegisterClassEx(HintWndClass);
    FToolTipHandle := CreateWindowEx(WS_EX_TOOLWINDOW or WS_EX_TRANSPARENT, HintWndClass.lpszClassName, nil, WS_POPUP, 0, 0, 0, 0, FNativeControlHandle, 0, hInstance, nil);
    FWindowHandle := TWinWindowHandle.Create(nil, FToolTipHandle);
    PrevWndProc := GetWindowLongPtr(FToolTipHandle, GWL_WNDPROC);
    SetWindowLongPtr(FToolTipHandle, GWL_USERDATA, LInteger(Self));
    SetWindowLongPtr(FToolTipHandle, GWL_WNDPROC, LInteger(@HintWindowProc));
  end;
  {$ENDIF}
end;

destructor TTMSFNCHintWindow.Destroy;
begin
  FHint := nil;
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  if Assigned(FTimer) then
    FTimer.Free;

  if Assigned(FNativeHint) then
  begin
    FNativeHint.Free;
    FNativeHint := nil;
  end;

  if Assigned(FNativeHintView) then
  begin
    FNativeHintView.removeFromSuperview;
    FNativeHintView.release;
    FNativeHintView := nil;
  end;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  if Assigned(FWindowHandle) then
    FWindowHandle.Free;
  if FToolTipHandle <> 0 then
    DestroyWindow(FToolTipHandle);
  {$ENDIF}
  inherited;
end;

procedure TTMSFNCHintWindow.SetPosition(const X: Single; const Y: Single);
begin
  inherited;
end;

procedure TTMSFNCHintWindow.SetHint(const AString: string);
var
  LShortText: string;
begin
  if GetHint <> AString then
  begin
    inherited;
    LShortText := GetShortText;
    FHintString := LShortText;
    if not Assigned(FHint) then
      FHint := FindHintControl;
  end
  else
    inherited;
end;
{$ENDIF}
{$ENDIF}

{ TTMSFNCCustomHint }

procedure TTMSFNCCustomHint.DoCalculateHintRect(AHint: string; var ARect: TRect);
begin
  if Assigned(OnCalculateHintRect) then
    OnCalculateHintRect(Self, AHint, ARect);
end;

procedure TTMSFNCCustomHint.DoAfterDrawHint(AGraphics: TTMSFNCGraphics; AHint: string; ARect: TRect);
begin
  if Assigned(OnAfterDrawHint) then
    OnAfterDrawHint(Self, AGraphics, AHint, ARect);
end;

procedure TTMSFNCCustomHint.DoBeforeDrawHint(AGraphics: TTMSFNCGraphics; AHint: string; ARect: TRect; var ADefaultDraw: Boolean);
begin
  if Assigned(OnBeforeDrawHint) then
    OnBeforeDrawHint(Self, AGraphics, AHint, ARect, ADefaultDraw);
end;

{$IFDEF FMXLIB}
{$IFDEF SUPPORTSHINTS}
procedure TTMSFNCCustomHint.CreateFormsHandler(const Sender: TObject; const M: System.Messaging.TMessage);
var
  ctx: TRttiContext;
  f: TRttiField;
  h: THint;
  v: TValue;
  I: Integer;
  frm: TCommonCustomForm;

  procedure RegisterHintToForm;
  begin
    f := ctx.GetType(frm.ClassType).GetField('FSharedHint');
    if Assigned(f) then
    begin
      v := f.GetValue(frm);
      if v.IsType<THint> then
      begin
        h := v.AsType<THint>;
        if Assigned(h) then
          h.Free;
        f.SetValue(frm, TTMSFNCHintWindow.Create(frm.Handle, Shadow));
      end;
    end;

  end;
begin
  if csDestroying in ComponentState then
    Exit;

  for I := 0 to Screen.PopupFormCount - 1 do
  begin
    frm := Screen.PopupForms[I];
    RegisterHintToForm;
  end;

  for I := 0 to Screen.FormCount - 1 do
  begin
    frm := Screen.Forms[I];
    RegisterHintToForm;
  end;
end;
{$ENDIF}
{$ENDIF}

procedure TTMSFNCCustomHint.Assign(Source: TPersistent);
begin
  if (Source is TTMSFNCCustomHint) then
  begin
    FFill.Assign((Source as TTMSFNCCustomHint).Fill);
    FStroke.Assign((Source as TTMSFNCCustomHint).Stroke);
    FFont.Assign((Source as TTMSFNCCustomHint).Font);
    FShadow := (Source as TTMSFNCCustomHint).Shadow;
  end;
end;

function TTMSFNCCustomHint.Calculate(AHint: string): TRect;
var
  r: TRectF;

  function GetTextRect: TRectF;
  var
    g: TTMSFNCGraphics;
    R: TRectF;
  begin
    g := TTMSFNCGraphics.CreateBitmapCanvas;
    g.BitmapContainer := BitmapContainer;
    try
      g.Font.Assign(Font);
      R := RectF(0, 0, 10000, 10000);
      Result := g.CalculateText(AHint, R);
    finally
      g.Free;
    end;
  end;

begin
  try
    r := GetTextRect;
    Result := ConvertToRect(r);
    Result.Right := Result.Right + 4;
    Result.Bottom := Result.Bottom + 2;
    DoCalculateHintRect(AHint, Result);
  finally
  end;
end;

procedure TTMSFNCCustomHint.ChangeDPIScale(M, D: Integer);
begin
  inherited;

  FFont.Height := TTMSFNCUtils.MulDivInt(FFont.Height, M, D);
end;

constructor TTMSFNCCustomHint.Create(AOwner: TComponent);
begin
  inherited;
  FShadow := True;
  FFont := TTMSFNCGraphicsFont.Create;
  FFill := TTMSFNCGraphicsFill.Create;
  {$IFDEF FMXLIB}
  FStroke := TTMSFNCGraphicsStroke.Create;
  {$IFDEF SUPPORTSHINTS}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 31}
  TMessageManager.DefaultManager.SubscribeToMessage(TFormBeforeShownMessage, CreateFormsHandler);
  {$ELSE}
  TMessageManager.DefaultManager.SubscribeToMessage(TFormsCreatedMessage, CreateFormsHandler);
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  {$ENDIF}
  {$IFDEF CMNLIB}
  {$IFDEF VCLLIB}
  FStroke := TTMSFNCGraphicsStroke.Create(gskNone);
  {$ENDIF}
  {$IFDEF LCLLIB}
  FStroke := TTMSFNCGraphicsStroke.Create;
  {$ENDIF}
  FPrevHintWindowClass := HintWindowClass;
  HintWindowClass := TTMSFNCHintWindow;
  {$ENDIF}
end;

destructor TTMSFNCCustomHint.Destroy;
begin
  {$IFDEF CMNLIB}
  HintWindowClass := FPrevHintWindowClass;
  {$ENDIF}
  {$IFDEF FMXLIB}
  {$IFDEF SUPPORTSHINTS}
  TMessageManager.DefaultManager.Unsubscribe(TFormsCreatedMessage, CreateFormsHandler);
  {$ENDIF}
  {$ENDIF}
  FFont.Free;
  FFill.Free;
  FStroke.Free;
  inherited;
end;

function TTMSFNCCustomHint.GetBitmapContainer: TTMSFNCBitmapContainer;
begin
  Result := FBitmapContainer;
end;

function TTMSFNCCustomHint.GetDocURL: string;
begin
  Result := TTMSFNCBaseDocURL + 'tmsfncuipack/components/ttmsfnchint';
end;

function TTMSFNCCustomHint.GetInstance: NativeUInt;
begin
  Result := HInstance;
end;

procedure TTMSFNCCustomHint.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FBitmapContainer) then
    FBitmapContainer := nil;
end;

procedure TTMSFNCCustomHint.SetBitmapContainer(
  const Value: TTMSFNCBitmapContainer);
begin
  FBitmapContainer := Value;
end;

procedure TTMSFNCCustomHint.SetFill(const Value: TTMSFNCGraphicsFill);
begin
  FFill.Assign(Value);
end;

procedure TTMSFNCCustomHint.SetShadow(const Value: Boolean);
begin
  FShadow := Value;
end;

procedure TTMSFNCCustomHint.SetStroke(const Value: TTMSFNCGraphicsStroke);
begin
  FStroke.Assign(Value);
end;

procedure TTMSFNCCustomHint.SetFont(const Value: TTMSFNCGraphicsFont);
begin
  FFont.Assign(Value);
end;

{$IFDEF SUPPORTSHINTS}
{$IFDEF MACOS}
{$IFNDEF IOS}

{ TTMSFNCNativeHintWindow }

constructor TTMSFNCNativeHintWindow.Create;
var
  V: Pointer;
begin
  inherited Create;
  V := NSView(Super).init;
  if V <> GetObjectID then
    UpdateObjectID(V);
end;

procedure TTMSFNCNativeHintWindow.drawRect(dirtyRect: NSRect); cdecl;
var
  AutoReleasePool: NSAutoreleasePool;
  r: CGRect;
  g: CGContextRef;
  bmp: TTMSFNCBitmap;
  ImageRef: CGImageRef;
  NewDestRect: CGRect;
begin
  if Assigned(FView) then
  begin
    AutoReleasePool := TNSAutoreleasePool.Create;
    try
      r := dirtyRect;
      g := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext).graphicsPort;
      bmp := FView.Draw(nil, ConvertToRect(RectF(r.origin.x, r.origin.y, r.size.width, r.size.height)), FView.FHintString);
      if Assigned(bmp) then
      begin
        NewDestRect := r;
        if bmp.HandleAllocated then
        begin
          ImageRef := TTMSFNCMacBitmap(bmp.Handle).GetImage;
          if Assigned(ImageRef) then
          begin
            CGContextSaveGState(g);
            CGContextSetRGBFillColor(g, 1.0, 0.0, 0.0, 1.0);
            CGContextFillRect(g, r);
            CGContextSetInterpolationQuality(g, kCGInterpolationDefault);
            {$HINTS OFF}
            {$IF COMPILERVERSION < 34}
            CGContextSetAllowsAntialiasing(g, Ord(False));
            {$ELSE}
            CGContextSetAllowsAntialiasing(g, False);
            {$IFEND}
            CGContextDrawImage(g, NewDestRect, ImageRef);
            {$IF COMPILERVERSION < 34}
            CGContextSetAllowsAntialiasing(g, Ord(True));
            {$ELSE}
            CGContextSetAllowsAntialiasing(g, True);
            {$IFEND}
            {$HINTS ON}
            CGContextRestoreGState(g);
          end;
        end;
        bmp.Free;
      end;
    finally
      AutoReleasePool.release;
    end;
  end;
end;

function TTMSFNCNativeHintWindow.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(NSHintView);
end;

{$ENDIF}
{$ENDIF}
{$ENDIF}

procedure TTMSFNCHintWindow.HideHint;
begin
  {$IFDEF SUPPORTSHINTS}
  {$IFDEF FMXLIB}
  {$IFDEF MSWINDOWS}
  ShowWindow(FToolTipHandle, SW_HIDE);
  {$ENDIF}
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  FNativeHintView.removeFromSuperView;
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
  {$IFDEF CMNLIB}
  {$ENDIF}
  {$ENDIF}
end;

procedure TTMSFNCHintWindow.ShowHintAt(AHintControl: TTMSFNCCustomHint; AHint: string; X: Integer; Y: Integer);
{$IFDEF FMXLIB}
{$IFDEF SUPPORTSHINTS}
  {$IFDEF MSWINDOWS}
var
  r: TRect;
  p: TPoint;
  sz: TSize;
  {$ENDIF}
  {$IFDEF MACOS}
  {$IFNDEF IOS}
var
  w: NSWindow;
  p: NSPoint;
  {$ENDIF}
  {$ENDIF}
begin
  FHintString := AHint;
  FHint := AHintControl;
  {$IFDEF MSWINDOWS}
  r := Calculate(FHintString);
  p := Point(X, Y);
  OffsetRect(r, p.X, p.Y);
  SetWindowPos(FToolTipHandle, HWND_TOPMOST, r.Left, r.Top, r.Width, r.Height, SWP_NOACTIVATE);
  sz.cx := r.Width;
  sz.cy := r.Height;
  {$HINTS OFF}
  {$IF COMPILERVERSION > 30}
  FWindowHandle.ClientSize := sz;
  {$IFEND}
  {$HINTS ON}
  ShowWindow(FToolTipHandle, SW_SHOWNOACTIVATE);
  InvalidateRect(FToolTipHandle, nil, True);
  {$ENDIF}
  {$IFDEF MACOS}
  {$IFNDEF IOS}
  w := FNativeView.window;
  if Assigned(w) then
  begin
    p.x := X;
    p.y := FNativeView.frame.size.height - Y;
    FNativeHintView.setFrame(GetHintFrame(p, 0, 0));
    FNativeView.addSubview(FNativeHintView);
  end;
  {$ENDIF}
  {$ENDIF}
{$ELSE}
begin
{$ENDIF}
{$ENDIF}
{$IFDEF CMNLIB}
var
  r: TRect;
  p: TPoint;
begin
  FHint := AHintControl;
  r := Calculate(AHint);
  p := Point(X, Y);
  OffsetRect(r, p.X, p.Y);
  ActivateHint(r, AHint);
{$ENDIF}
end;

function TTMSFNCHint.IsShowing: Boolean;
begin
  {$IFDEF SUPPORTSHINTS}
  Result := Assigned(FHintWindow);
  {$ELSE}
  Result := False;
  {$ENDIF}
end;

procedure TTMSFNCHint.RegisterRuntimeClasses;
begin
  inherited;
  RegisterClass(TTMSFNCHint);
end;

destructor TTMSFNCHint.Destroy;
begin
  HideHint;
  inherited;
end;

procedure TTMSFNCHint.HideHint;
begin
  {$IFDEF SUPPORTSHINTS}
  if Assigned(FHintWindow) then
  begin
    FHintWindow.HideHint;
    FHintWindow.Free;
    FHintWindow := nil;
  end;
  {$ENDIF}
end;

procedure TTMSFNCHint.ShowHintAt(AHint: string; X: Integer; Y: Integer);
{$IFDEF SUPPORTSHINTS}
var
  frm: TCustomForm;
begin
  if Assigned(FHintWindow) then
    Exit;

  frm := TTMSFNCUtils.GetParentForm(Self);
  if not Assigned(frm) then
    frm := TTMSFNCUtils.GetOwnerForm(Self);

  if Assigned(frm) then
  begin
    {$IFDEF FMXLIB}
    FHintWindow := TTMSFNCHintWindow.Create(frm.Handle, Shadow);
    {$ENDIF}
    {$IFDEF CMNLIB}
    FHintWindow := TTMSFNCHintWindow.Create(frm, Shadow);
    FHintWindow.Color := Fill.Color;
    {$ENDIF}
    FHintWindow.ShowHintAt(Self, AHint, X, Y);
  end;
{$ELSE}
begin
{$ENDIF}
end;

initialization
begin
  {$IFDEF FMXLIB}
  {$IFDEF SUPPORTSHINTS}
  TTMSFNCHintWindow.RegisterClass(TTMSFNCHintWindow);
  {$ENDIF}
  {$ENDIF}
end;

end.
