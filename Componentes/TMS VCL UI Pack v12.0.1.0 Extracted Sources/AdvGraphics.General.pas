{********************************************************************}
{                                                                    }
{ written by TMS Software                                            }
{            copyright (c) 2017 - 2021                               }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The complete source code remains property of the author and may    }
{ not be distributed, published, given or sold in any form as such.  }
{ No parts of the source code can be included in any other component }
{ or application without written authorization of the author.        }
{********************************************************************}

unit AdvGraphics.General;

{$I TMSDEFS.INC}

{$IFDEF FMXLIB}
{$IFDEF MSWINDOWS}
{$DEFINE FMXWINDOWS}
{$ENDIF}
{$ENDIF}

{$IFDEF FMXLIB}
{$DEFINE FMXWEBLIB}
{$ENDIF}
{$IFDEF CMNLIB}
{$DEFINE CMNWEBLIB}
{$ENDIF}
{$IFDEF WEBLIB}
{$DEFINE CMNWEBLIB}
{$DEFINE FMXWEBLIB}
{$ENDIF}

{$IFDEF WEBLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}
{$IFDEF LCLLIB}
{$DEFINE LCLWEBLIB}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF}
  Classes, AdvGraphicsTypes,
  Types, Graphics, AdvGraphics,
  AdvTypes
  {$IFDEF FMXLIB}
  ,FMX.Types, FMX.TextLayout, System.Math.Vectors
  {$ENDIF}
  {$IFNDEF LCLWEBLIB}
  {$HINTS OFF}
  {$IF COMPILERVERSION > 22}
  ,UITypes
  {$IFEND}
  {$HINTS ON}
  {$ENDIF}
  ;

type
  TAdvGraphicsContextGeneral = class(TAdvGraphicsContext)
  private
    FPrinting: Boolean;
    {$IFNDEF WEBLIB}
    {%H-}FShowAcceleratorChar: Boolean;
    {$ENDIF}
    {$IFDEF CMNLIB}
    FOldPenHandle: THandle;
    {$ENDIF}
    {$IFDEF CMNWEBLIB}
    FOldPenStyle: TPenStyle;
    FOldPenWidth: Integer;
    {$ENDIF}
    {$IFDEF FMXLIB}
    FMatrix: TMatrix;
    FMatrixSaved: Boolean;
    FTextMatrix: TMatrix;
    FTextLayout: TTextLayout;
    {$ENDIF}
  protected
    function GetNativeCanvas: Pointer; override;
    {$IFDEF CMNWEBLIB}
    {$IFDEF WEBLIB}
    procedure DrawRotatedText(ACanvas: TCanvas; ARect: TRectF; AText: String; AAngle: Single; AHorizontalAlign, AVerticalAlign: TAdvGraphicsTextAlign);
    {$ENDIF}
    {$IFNDEF WEBLIB}
    procedure DrawRotatedText(ACanvas: TCanvas; ARect: TRect; AText: String; AAngle: Single; AHorizontalAlign, AVerticalAlign: TAdvGraphicsTextAlign);
    {$ENDIF}
    {$ENDIF}
    {$IFDEF FMXWINDOWS}
    function CalculateTextWin(AText: string; ARect: TRectF; AWordWrapping: Boolean): TRectF; virtual;
    {$ENDIF}
    {$IFDEF FMXLIB}
    procedure SaveMatrix;
    procedure RestoreMatrix;
    {$ENDIF}
  public
    destructor Destroy; override;
    function GetFillColor: TAdvGraphicsColor; override;
    function CalculateText(AText: string; ARect: TRectF; AWordWrapping: Boolean): TRectF; override;
    function SetTextAngle(ARect: TRectF; {%H-}AAngle: Single): TRectF; override;
    function CreatePath: Pointer; override;
    function GetMatrix: TAdvGraphicsMatrix; override;
    procedure Render; override;
    procedure PathOpen({%H-}APath: Pointer); override;
    procedure PathMoveTo(APath: Pointer; APoint: TPointF); override;
    procedure PathLineTo(APath: Pointer; APoint: TPointF); override;
    procedure PathClose(APath: Pointer); override;
    procedure ResetTransform; override;
    procedure ResetClip; override;
    procedure ScaleTransform({%H-}AX, {%H-}AY: Single); override;
    procedure RotateTransform({%H-}AAngle: Single); override;
    procedure TranslateTransform({%H-}AX, {%H-}AY: Single); override;
    procedure SetTextQuality({%H-}ATextQuality: TAdvGraphicsTextQuality); override;
    procedure SetAntiAliasing({%H-}AAntiAliasing: Boolean); override;
    procedure SetShowAcceleratorChar({%H-}AShowAcceleratorChar: Boolean); override;
    procedure SetSize({%H-}AWidth, {%H-}AHeight: Single); override;
    procedure ResetTextAngle({%H-}AAngle: Single); override;
    procedure BeginScene; override;
    procedure EndScene; override;
    procedure BeginPrinting; override;
    procedure EndPrinting; override;
    procedure StartSpecialPen; override;
    procedure StopSpecialPen; override;
    procedure RestoreState(AState: TAdvGraphicsSaveState); override;
    procedure SaveState({%H-}AState: TAdvGraphicsSaveState); override;
    procedure SetMatrix(AMatrix: TAdvGraphicsMatrix); override;
    procedure SetScale(AScale: Single); override;
    procedure SetFontSize(ASize: Integer); override;
    procedure SetFontColor(AColor: TAdvGraphicsColor); override;
    procedure SetFontName(AName: string); override;
    procedure SetFont(AFont: TAdvGraphicsFont); override;
    procedure SetFontStyles(AStyle: TFontStyles); override;
    procedure SetFill(AFill: TAdvGraphicsFill); override;
    procedure SetFillKind(AKind: TAdvGraphicsFillKind); override;
    procedure SetFillColor(AColor: TAdvGraphicsColor); override;
    procedure SetStroke(AStroke: TAdvGraphicsStroke); override;
    procedure SetStrokeKind(AKind: TAdvGraphicsStrokeKind); override;
    procedure SetStrokeColor(AColor: TAdvGraphicsColor); override;
    procedure SetStrokeWidth(AWidth: Single); override;
    procedure DrawLine({%H-}AStroke: TAdvGraphicsStroke; AFromPoint: TPointF; AToPoint: TPointF; {%H-}AModifyPointModeFrom: TAdvGraphicsModifyPointMode = gcpmRightDown; {%H-}AModifyPointModeTo: TAdvGraphicsModifyPointMode = gcpmRightDown); override;
    procedure DrawPolygon({%H-}AStroke: TAdvGraphicsStroke; APolygon: TAdvGraphicsPathPolygon); override;
    procedure FillPolygon({%H-}AFill: TAdvGraphicsFill; APolygon: TAdvGraphicsPathPolygon); override;
    procedure DrawPolyline({%H-}AStroke: TAdvGraphicsStroke; APolyline: TAdvGraphicsPathPolygon); override;
    procedure FillPolyline({%H-}AFill: TAdvGraphicsFill; APolyline: TAdvGraphicsPathPolygon); override;
    procedure FillArc({%H-}AFill: TAdvGraphicsFill; ACenter, ARadius: TPointF; AStartAngle, ASweepAngle: Single); override;
    procedure DrawArc({%H-}AStroke: TAdvGraphicsStroke; ACenter, ARadius: TPointF; AStartAngle, ASweepAngle: Single); override;
    procedure FillRect({%H-}AFill: TAdvGraphicsFill; ARect: TRectF; {%H-}AModifyRectMode: TAdvGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure DrawRect({%H-}AStroke: TAdvGraphicsStroke; ARect: TRectF; ASides: TAdvGraphicsSides; {%H-}AModifyRectMode: TAdvGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure FillRoundRect({%H-}AFill: TAdvGraphicsFill; ARect: TRectF; ARounding: Single; ACorners: TAdvGraphicsCorners; {%H-}AModifyRectMode: TAdvGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure DrawRoundRect({%H-}AStroke: TAdvGraphicsStroke; ARect: TRectF; ARounding: Single; ACorners: TAdvGraphicsCorners; {%H-}AModifyRectMode: TAdvGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure FillEllipse({%H-}AFill: TAdvGraphicsFill; ARect: TRectF; {%H-}AModifyRectMode: TAdvGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure DrawEllipse({%H-}AStroke: TAdvGraphicsStroke; ARect: TRectF; {%H-}AModifyRectMode: TAdvGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure DrawBitmap(ABitmap: TAdvDrawBitmap; {%H-}ASrcRect, ADstRect: TRectF; {%H-}AOpacity: Single); override;
    procedure ClipRect(ARect: TRectF); override;
    procedure ClipPath({%H-}APath: TAdvGraphicsPath); override;
    procedure DrawFocusPath({%H-}AStroke: TAdvGraphicsStroke; APath: TAdvGraphicsPath; AColor: TAdvGraphicsColor); override;
    procedure DrawFocusRectangle({%H-}AStroke: TAdvGraphicsStroke; ARect: TRectF; AColor: TAdvGraphicsColor; {%H-}AModifyRectMode: TAdvGraphicsModifyRectMode = gcrmShrinkAll); override;
    procedure DrawText(AText: string; ARect: TRectF; AWordWrapping: Boolean; AHorizontalAlign, AVerticalAlign: TAdvGraphicsTextAlign; ATrimming: TAdvGraphicsTextTrimming; AAngle: Single); override;
    procedure DrawPath({%H-}AStroke: TAdvGraphicsStroke; APath: TAdvGraphicsPath; APathMode: TAdvGraphicsPathDrawMode = pdmPolygon); override;
    procedure FillPath({%H-}AFill: TAdvGraphicsFill; APath: TAdvGraphicsPath; APathMode: TAdvGraphicsPathDrawMode = pdmPolygon); override;
  end;

function GetNativeContextClass: TAdvGraphicsContextClass;

implementation

uses
  Math
  {$IFDEF FMXWINDOWS}
  ,SysUtils, WinApi.D2D1, WinApi.D3D10, WinApi.Dxgi,WinApi.DxgiFormat,
  WinApi.ActiveX, FMX.Canvas.D2D, Winapi.D3D10_1,
  FMX.Consts
  {$ENDIF}
  {$IFDEF LCLLIB}
  ,LCLIntF, LCLType
  {$ENDIF}
  ;

{$IFDEF MSWINDOWS}
{$IFDEF LCLLIB}
function AbortPath(_para1:HDC):WINBOOL; external 'gdi32' name 'AbortPath';
{$ENDIF}
{$ENDIF}

{$IFDEF FMXWINDOWS}
type
  TDirect3DLevel = (lUndetermined, lUnsupported, lDirect3D10, lDirect3D10_1);
  TDirect3DSupport = (sUndetermined, sDirect3D10Legacy, sDirect3D10, sDirect3D10_1);

var
  FSharedDevice: ID3D10Device;
  FSharedDXGIFactory: IDXGIFactory;
  FSharedFactory: ID2D1Factory;
  FSharedDWriteFactory: IDWriteFactory;
  FDirect3DLevel: TDirect3DLevel;
  FDirect3DSupport: TDirect3DSupport;
  FDirect3DHardware: Boolean;
{$ENDIF}

{$IFDEF FMXWINDOWS}
function TAdvGraphicsContextGeneral.CalculateTextWin(AText: string; ARect: TRectF; AWordWrapping: Boolean): TRectF;
var
  TextRange: TDWriteTextRange;
  TextFormat: IDWriteTextFormat;
  LocaleName: string;
  TrimOptions: TDwriteTrimming;
  TrimmingSign: IDWriteInlineObject;
  FLayout: IDWriteTextLayout;
  FMetrics: TDWriteTextMetrics;
  FOverhangMetrics: TDwriteOverhangMetrics;
  PrevFPUState: TArithmeticExceptionMask;

  procedure SaveClearFPUState;
  begin
    PrevFPUState:= GetExceptionMask;
    SetExceptionMask(exAllArithmeticExceptions);
  end;

  procedure RestoreFPUState;
  begin
    SetExceptionMask(PrevFPUState);
  end;

  function D2FontWeight(const Style: TFontStyles): TDWriteFontWeight; inline;
  begin
    Result := DWRITE_FONT_WEIGHT_REGULAR;
    if TFontStyle.fsBold in Style then
      Result := DWRITE_FONT_WEIGHT_BOLD;
  end;

  function D2FontStyle(const Style: TFontStyles): TDWriteFontStyle; inline;
  begin
    Result := DWRITE_FONT_STYLE_NORMAL;
    if TFontStyle.fsItalic in Style then
      Result := DWRITE_FONT_STYLE_OBLIQUE;
  end;

  procedure UpdateDirect3DLevel;
  var
    DXLib: THandle;
  begin
    if FDirect3DLevel = TDirect3DLevel.lUndetermined then
    begin
      {$HINTS OFF}
      if {$IF COMPILERVERSION > 27}GlobalUseDX{$ELSE}GlobalUseDX10{$IFEND} then
      {$HINTS ON}
      begin
        FDirect3DLevel := TDirect3DLevel.lUndetermined;

        // Direct3D 10.1
        DXLib := LoadLibrary(D3D10_1_dll);
        if DXLib <> 0 then
        try
          if GetProcAddress(DXLib, 'D3D10CreateDevice1') <> nil then
            FDirect3DLevel := TDirect3DLevel.lDirect3D10_1;
        finally
          FreeLibrary(DXLib);
        end;

        if FDirect3DLevel = TDirect3DLevel.lUndetermined then
        begin // Direct3D 10.0
          DXLib := LoadLibrary(D3D10dll);
          if DXLib <> 0 then
          try
            if GetProcAddress(DXLib, 'D3D10CreateDevice') <> nil then
              FDirect3DLevel := TDirect3DLevel.lDirect3D10;
          finally
            FreeLibrary(DXLib);
          end;
        end;

        if FDirect3DLevel = TDirect3DLevel.lUndetermined then
          FDirect3DLevel := TDirect3DLevel.lUnsupported;
      end
      else
        FDirect3DLevel := TDirect3DLevel.lUnsupported;
    end;
  end;

  procedure CreateDirect3DDevice;

  function CreateDevice(const DriverType: D3D10_DRIVER_TYPE): HResult;
  var
    Flags: Cardinal;
  begin
    Result := S_OK;
    Flags := D3D10_CREATE_DEVICE_BGRA_SUPPORT;

    if FDirect3DLevel = TDirect3DLevel.lDirect3D10_1 then
    begin
      // Direct3D 10.1 with full hardware support
      Result := D3D10CreateDevice1(nil, DriverType, 0, Flags, D3D10_FEATURE_LEVEL_10_1,
        D3D10_1_SDK_VERSION, ID3D10Device1(FSharedDevice));

      if Succeeded(Result) then
        FDirect3DSupport := TDirect3DSupport.sDirect3D10_1;

      if Failed(Result) then
      begin
        // Direct3D 10.1 with hardware support of 10.0
        Result := D3D10CreateDevice1(nil, DriverType, 0, Flags, D3D10_FEATURE_LEVEL_10_0,
          D3D10_1_SDK_VERSION, ID3D10Device1(FSharedDevice));

        if Succeeded(Result) then
          FDirect3DSupport := TDirect3DSupport.sDirect3D10;
      end;
    end;

    if (FDirect3DLevel = TDirect3DLevel.lDirect3D10) or Failed(Result) then
    begin
      // Legacy Direct3D 10.0 (on unpatched version of Vista)
      Result := D3D10CreateDevice(nil, DriverType, 0, Flags, D3D10_SDK_VERSION, FSharedDevice);

      if Succeeded(Result) then
      begin
        FDirect3DLevel := TDirect3DLevel.lDirect3D10;
        FDirect3DSupport := TDirect3DSupport.sDirect3D10Legacy;
      end;
    end;
  end;

var
  Res: HResult;
begin
  if FDirect3DLevel < TDirect3DLevel.lDirect3D10 then
    raise Exception.CreateFmt('Cannot determine Direct3D support level.', [ClassType]);

  SaveClearFPUState;
  try
    {$HINTS OFF}
    if {$IF COMPILERVERSION > 27}GlobalUseDXSoftware{$ELSE}GlobalUseDX10Software{$IFEND} then
    {$HINTS ON}
    begin
      Res := CreateDevice(D3D10_DRIVER_TYPE_WARP);
      if Failed(Res) then
      begin
        // WARP device might not be supported on pre-DX10.1 system, which may still support DX10 in hardware.
        Res := CreateDevice(D3D10_DRIVER_TYPE_HARDWARE);
        if Succeeded(Res) then
          FDirect3DHardware := True;
      end
      else
        FDirect3DHardware := False;
    end
    else
    begin
      Res := CreateDevice(D3D10_DRIVER_TYPE_HARDWARE);
      if Failed(Res) then
      begin
        Res := CreateDevice(D3D10_DRIVER_TYPE_WARP);
        if Succeeded(Res) then
          FDirect3DHardware := False;
      end
      else
        FDirect3DHardware := True;
    end;

    if Failed(Res) then
      raise ECannotCreateD3DDevice.CreateFmt(SCannotCreateD3DDevice, [ClassType]);
  finally
    RestoreFPUState;
  end;
end;

procedure AcquireDXGIFactory;
var
  DXGIDevice: IDXGIDevice;
  DXGIAdapter: IDXGIAdapter;
begin
  if Succeeded(FSharedDevice.QueryInterface(IDXGIDevice, DXGIDevice)) and (DXGIDevice <> nil) and
    Succeeded(DXGIDevice.GetParent(IDXGIAdapter, DXGIAdapter)) and (DXGIAdapter <> nil) then
      DXGIAdapter.GetParent(IDXGIFactory, FSharedDXGIFactory);
end;

procedure CreateDirect2DFactory;
var
  FactoryOptions: PD2D1FactoryOptions;
  Res: HResult;
begin
  FactoryOptions := nil;
  Res := D2D1CreateFactory(D2D1_FACTORY_TYPE_MULTI_THREADED, ID2D1Factory, FactoryOptions, FSharedFactory);
  if Failed(Res) or (FSharedFactory = nil) then
    raise Exception.CreateFmt('Cannot create Direct2D Factory object for ''%s''.', [ClassType]);

  if Failed(DWriteCreateFactory(DWRITE_FACTORY_TYPE_SHARED, IDWriteFactory, IUnknown(FSharedDWriteFactory))) then
    raise Exception.CreateFmt('Cannot create DirectWrite Factory object for ''%s''.', [ClassType]);
end;

procedure CreateSharedResources;
begin
  UpdateDirect3DLevel;
  if FSharedDevice = nil then
  begin
    CreateDirect3DDevice;
    AcquireDXGIFactory;
  end;
  if FSharedFactory = nil then
    CreateDirect2DFactory;
end;

function SharedDWriteFactory: IDWriteFactory;
begin
  if FSharedDWriteFactory = nil then
    CreateSharedResources;
  Result := FSharedDWriteFactory;
end;

var
  r: TRectF;
  dwVersion:Dword;
  dwWindowsMajorVersion: Dword;
begin
  dwVersion := GetVersion;
  dwWindowsMajorVersion :=  DWORD(LOBYTE(LOWORD(dwVersion)));
  if (dwWindowsMajorVersion = 5) then
  begin
    r := ARect;
    Canvas.MeasureText(r, AText, AWordWrapping, [], TTextAlign.Leading, TTextAlign.Leading);
    Result := r;
  end
  else
  begin
    Result := RectF(0, 0, 0, 0);
    FLayout := nil;
    if Succeeded(SharedDWriteFactory.CreateTextFormat(PChar(Canvas.Font.Name), nil, D2FontWeight(Canvas.Font.Style),
      D2FontStyle(Canvas.Font.Style), DWRITE_FONT_STRETCH_NORMAL, Canvas.Font.Size, PChar(LocaleName), TextFormat)) then
    try
      TextFormat.SetReadingDirection(DWRITE_READING_DIRECTION_LEFT_TO_RIGHT);

      if not AWordWrapping then
        TextFormat.SetWordWrapping(DWRITE_WORD_WRAPPING_NO_WRAP)
      else
        TextFormat.SetWordWrapping(DWRITE_WORD_WRAPPING_WRAP);

      FillChar(TrimOptions, SizeOf(TDwriteTrimming), 0);
      TrimOptions.granularity := DWRITE_TRIMMING_GRANULARITY_NONE;

      TrimmingSign := nil;
      TextFormat.SetTrimming(TrimOptions, TrimmingSign);
      TrimmingSign := nil;

      if Succeeded(SharedDWriteFactory.CreateTextLayout(PChar(AText), AText.Length, TextFormat, ARect.Width,
        ARect.Height, FLayout)) then
      begin
        TextRange.StartPosition := 0;
        TextRange.Length := AText.Length;
        FLayout.SetStrikethrough(TFontStyle.fsStrikeOut in Canvas.Font.Style, TextRange);
        FLayout.SetUnderline(TFontStyle.fsUnderline in Canvas.Font.Style, TextRange);
        FLayout.GetMetrics(FMetrics);
        FLayout.GetOverhangMetrics(FOverhangMetrics);

        Result := TRectF.Create(FMetrics.left, FMetrics.top,
          FMetrics.left + {Max(}FMetrics.widthIncludingTrailingWhitespace{, FOverhangMetrics.Right + AMaxRect.Width)},
          FMetrics.top + Min(FMetrics.height, FMetrics.layoutHeight));
        Result.Offset(ARect.TopLeft);
        if FMetrics.top < 0 then
          Result.Offset(0, Abs(FMetrics.top));
      end;
    finally
      TextFormat := nil;
    end;
  end;
end;
{$ENDIF}

{$IFDEF FMXLIB}
procedure TAdvGraphicsContextGeneral.SaveMatrix;
begin
  if not FMatrixSaved then
  begin
    FMatrixSaved := True;
    FMatrix := Canvas.Matrix;
  end;
end;

procedure TAdvGraphicsContextGeneral.RestoreMatrix;
begin
  if FMatrixSaved then
  begin
    FMatrixSaved := False;
    Canvas.SetMatrix(FMatrix);
  end;
end;
{$ENDIF}

{$HINTS OFF}
{$IFDEF CMNWEBLIB}
{$IFDEF WEBLIB}
procedure TAdvGraphicsContextGeneral.DrawRotatedText(ACanvas: TCanvas; ARect: TRectF; AText: String; AAngle: Single; AHorizontalAlign, AVerticalAlign: TAdvGraphicsTextAlign);
{$ENDIF}
{$IFNDEF WEBLIB}
procedure TAdvGraphicsContextGeneral.DrawRotatedText(ACanvas: TCanvas; ARect: TRect; AText: String; AAngle: Single; AHorizontalAlign, AVerticalAlign: TAdvGraphicsTextAlign);
{$ENDIF}
var
  tw, th: Single;
  so: Integer;
  s: String;
  xs, ys, angle: Single;
  {$IFDEF WEBLIB}
  ha: TAlignment;
  va: TVerticalAlignment;
  r: TCanvasRectF;
  {$ENDIF}
begin
  s := AText;
  angle := -10 * AAngle;
  {$IFDEF CMNLIB}
  so := ACanvas.Font.Orientation;
  ACanvas.Font.Orientation := Round(angle);
  {$ENDIF}

  xs := ARect.Left;
  ys := ARect.Top;
  tw := ACanvas.TextWidth(s);
  th := ACanvas.TextHeight(s);

  {$IFDEF CMNLIB}
  if angle < 0 then
  begin
    case AHorizontalAlign of
      gtaCenter: ys := ys + (ARect.Bottom - ARect.Top - tw) / 2;
      gtaTrailing: ys := ys + (ARect.Bottom - ARect.Top - tw);
    end;

    case AVerticalAlign of
      gtaCenter: xs := xs + (ARect.Right - ARect.Left) / 2 + th / 2;
      gtaLeading: xs := xs + (ARect.Right - ARect.Left);
      gtaTrailing: xs := xs + th;
    end;
  end
  else
  begin
    case AHorizontalAlign of
      gtaCenter: ys := ys + (ARect.Bottom - ARect.Top - tw) / 2 + tw;
      gtaLeading: ys := ys + (ARect.Bottom - ARect.Top);
      gtaTrailing: ys := ys + tw;
    end;

    case AVerticalAlign of
      gtaCenter: xs := xs + (ARect.Right - ARect.Left) / 2 - th / 2;
      gtaTrailing: xs := xs + (ARect.Right - ARect.Left - th);
    end;
  end;
  {$ENDIF}

  {$IFDEF WEBLIB}
  case AHorizontalAlign of
    gtaLeading: ha := taLeftJustify;
    gtaCenter: ha := taCenter;
    gtaTrailing: ha := taRightJustify;
  end;

  case AVerticalAlign of
    gtaCenter: va := taVerticalCenter;
    gtaLeading: va := taAlignTop;
    gtaTrailing: va := taAlignBottom;
  end;

  r := CreateCanvasRectF(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);

  ACanvas.TextRect(r, s, False, False, ha, va);
  {$ENDIF}
  {$IFNDEF WEBLIB}
  ACanvas.TextOut(Round(xs), Round(ys), s);
  {$ENDIF}
  {$IFDEF CMNLIB}
  ACanvas.Font.Orientation := so;
  {$ENDIF}
end;
{$ENDIF}
{$HINTS ON}

{ TAdvGraphicsContext }

procedure TAdvGraphicsContextGeneral.BeginPrinting;
begin
  FPrinting := True;
end;

procedure TAdvGraphicsContextGeneral.BeginScene;
begin
  {$IFDEF FMXWEBLIB}
  if Assigned(Canvas) then
  begin
    Canvas.BeginScene;
    {$IFDEF FMXLIB}
    Canvas.Clear(gcNull);
    {$ELSE}
    Canvas.Clear;
    {$ENDIF}
  end;
  {$ENDIF}
end;

function TAdvGraphicsContextGeneral.CalculateText(AText: string; ARect: TRectF;
  AWordWrapping: Boolean): TRectF;
{$IFDEF CMNWEBLIB}
var
  {$IFDEF VCLLIB}
  r: TRect;
  dstyle: TTextFormat;
  {$ENDIF}
  {$IFDEF LCLLIB}
  r: TRect;
  dstyle: Word;
  {$ENDIF}
  {$IFDEF WEBLIB}
  r: TCanvasRectF;
  {$ENDIF}
{$ENDIF}
begin
  {$IFDEF FMXLIB}
  Result := ARect;
  {$IFDEF FMXWINDOWS}
  Result := CalculateTextWin(AText, Result, AWordWrapping)
  {$ELSE}
  Canvas.MeasureText(Result, AText, AWordWrapping, [], TTextAlign.Leading, TTextAlign.Leading)
  {$ENDIF}
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  {$IFDEF WEBLIB}
  r := CreateCanvasRectF(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  r := Canvas.TextRect(r, AText, AWordWrapping, True);
  {$ENDIF}
  {$IFDEF VCLLIB}
  r := Rect(Round(ARect.Left), Round(ARect.Top), Round(ARect.Right), Round(ARect.Bottom));
  dstyle := [tfCalcRect];
  if AWordWrapping then
    dstyle := dstyle + [tfWordBreak]
  else
    dstyle := dstyle + [tfSingleLine];

  if not FShowAcceleratorChar then
    dstyle := dstyle + [tfNoPrefix];

  Canvas.TextRect(r, AText, dstyle);
  {$ENDIF}
  {$IFDEF LCLLIB}
  r := Rect(Round(ARect.Left), Round(ARect.Top), Round(ARect.Right), Round(ARect.Bottom));
  dstyle := DT_CALCRECT;
  if AWordWrapping then
    dstyle := dstyle or DT_WORDBREAK
  else
    dstyle := dstyle or DT_SINGLELINE;

  if not FShowAcceleratorChar then
    dstyle := dstyle or DT_NOPREFIX;

  LCLIntF.DrawText(Canvas.Handle, PChar(AText), Length(AText), r, dstyle);
  {$ENDIF}
  Result := RectF(r.Left, r.Top, r.Right, r.Bottom);
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.ClipPath(APath: TAdvGraphicsPath);
begin
  if Assigned(APath) then
  begin
    {$IFDEF WEBLIB}
    Canvas.PathBegin;
    ConvertToPath(APath);
    Canvas.Clip;
    {$ELSE}
    ClipRect(APath.GetBounds);
    {$ENDIF}
  end;
end;

procedure TAdvGraphicsContextGeneral.ClipRect(ARect: TRectF);
begin
  {$IFDEF FMXLIB}
  Canvas.IntersectClipRect(ARect);
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  {$IFDEF VCLLIB}
  IntersectClipRect(Canvas.Handle, Round(ARect.Left), Round(ARect.Top), Round(ARect.Right), Round(ARect.Bottom));
  {$ENDIF}
  {$IFDEF LCLLIB}
  Canvas.Clipping := True;
  Canvas.ClipRect := Rect(Round(ARect.Left), Round(ARect.Top), Round(ARect.Right), Round(ARect.Bottom));
  {$ENDIF}
  {$IFDEF WEBLIB}
  Canvas.ClipRect := CreateCanvasRectF(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  {$ENDIF}
  {$ENDIF}
end;

function TAdvGraphicsContextGeneral.CreatePath: Pointer;
begin
  {$IFDEF FMXLIB}
  Result := TPathData.Create;
  {$ENDIF}
  {$IFDEF CMNLIB}
  {$IFDEF MSWINDOWS}
  Result := nil;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF WEBLIB}
  Result := nil;
  {$ENDIF}
end;

destructor TAdvGraphicsContextGeneral.Destroy;
begin
  {$IFDEF FMXLIB}
  if Assigned(FTextLayout) then
  begin
    FTextLayout.Free;
    FTextLayout := nil;
  end;
  {$ENDIF}

  inherited;
end;

procedure TAdvGraphicsContextGeneral.DrawArc(AStroke: TAdvGraphicsStroke; ACenter, ARadius: TPointF; AStartAngle, ASweepAngle: Single);
{$IFDEF CMNWEBLIB}
var
  {$IFDEF CMNLIB}
  x, y, xs, ys, sx, sy, sxs, sys: Integer;
  {$ENDIF}
  bs: TBrushStyle;
  c: TAdvGraphicsColor;
{$ENDIF}
begin
  {$IFDEF FMXLIB}
  Canvas.DrawArc(ACenter, ARadius, AStartAngle, ASweepAngle, AStroke.Opacity);
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  {$IFNDEF WEBLIB}
  sx := Floor(ACenter.X - ARadius.X);
  sy := Floor(ACenter.Y - ARadius.Y);
  sxs := Floor(ACenter.X + ARadius.X);
  sys := Floor(ACenter.Y + ARadius.Y);

  x := Floor(ACenter.X + (ARadius.X * COS(DegToRad(-AStartAngle - ASweepAngle))));
  y := Floor(ACenter.Y - (ARadius.Y * SIN(DegToRad(-AStartAngle - ASweepAngle))));
  xs := Floor(ACenter.X + (ARadius.X * COS(DegToRad(-AStartAngle))));
  ys := Floor(ACenter.Y - (ARadius.Y * SIN(DegToRad(- AStartAngle))));
  {$ENDIF}
  c := Canvas.Brush.Color;
  bs := Canvas.Brush.Style;
  Canvas.Brush.Style := bsClear;
  {$IFNDEF WEBLIB}
  Canvas.Arc(sx, sy, sxs, sys, x, y, xs, ys);
  {$ENDIF}
  {$IFDEF WEBLIB}
  Canvas.AngleArc(ACenter.X, ACenter.Y, ARadius.X, DegToRad(AStartAngle), DegToRad(ASweepAngle));
  {$ENDIF}
  Canvas.Brush.Style := bs;
  Canvas.Brush.Color := c;
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.DrawBitmap(ABitmap: TAdvDrawBitmap; ASrcRect, ADstRect: TRectF; AOpacity: Single);
begin
  {$IFDEF FMXLIB}
  Canvas.DrawBitmap(ABitmap, ASrcRect, ADstRect, AOpacity, True);
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  {$IFDEF WEBLIB}
  Canvas.StretchDraw(CreateCanvasRectF(ADstRect.Left, ADstRect.Top, ADstRect.Right, ADstRect.Bottom), ABitmap);
  {$ENDIF}
  {$IFNDEF WEBLIB}
  Canvas.StretchDraw(Bounds(Round(ADstRect.Left), Round(ADstRect.Top), Round(ADstRect.Right - ADstRect.Left), Round(ADstRect.Bottom - ADstRect.Top)), ABitmap);
  {$ENDIF}
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.DrawEllipse(AStroke: TAdvGraphicsStroke; ARect: TRectF;
  AModifyRectMode: TAdvGraphicsModifyRectMode);
var
{$IFDEF FMXLIB}
  r: TRectF;
{$ENDIF}
{$IFDEF CMNWEBLIB}
{$IFDEF WEBLIB}
  r: TCanvasRectF;
{$ENDIF}
{$IFNDEF WEBLIB}
  r: TRect;
{$ENDIF}
  bs: TBrushStyle;
  c: TAdvGraphicsColor;
{$ENDIF}
begin
  ARect := ModifyRect(ARect, AModifyRectMode);
  {$IFDEF FMXLIB}
  r := ARect;
  Canvas.DrawEllipse(r, AStroke.Opacity);
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  {$IFDEF WEBLIB}
  r := CreateCanvasRectF(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  {$ENDIF}
  {$IFNDEF WEBLIB}
  r := Rect(Round(ARect.Left), Round(ARect.Top), Round(ARect.Right), Round(ARect.Bottom));
  {$ENDIF}
  c := Canvas.Brush.Color;
  bs := Canvas.Brush.Style;
  Canvas.Brush.Style := bsClear;
  Canvas.Ellipse(r);
  Canvas.Brush.Style := bs;
  Canvas.Brush.Color := c;
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.DrawFocusPath(AStroke: TAdvGraphicsStroke; APath: TAdvGraphicsPath; AColor: TAdvGraphicsColor);
{$IFDEF CMNWEBLIB}
var
  bs: TBrushStyle;
  c: TAdvGraphicsColor;
{$ENDIF}
begin
  {$IFDEF FMXLIB}
  Canvas.Stroke.Kind := TBrushKind.Solid;
  Canvas.Stroke.Color := AColor;
  {$HINTS OFF}
  {$IF COMPILERVERSION > 31}
  Canvas.Stroke.Dash := TStrokeDash.Dot;
  {$ELSE}
  Canvas.StrokeDash := TStrokeDash.Dot;
  {$IFEND}
  {$HINTS ON}
  DrawPath(AStroke, APath);
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  c := Canvas.Brush.Color;
  bs := Canvas.Brush.Style;
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := AColor;
  StartSpecialPen;
  DrawPath(AStroke, APath);
  StopSpecialPen;
  Canvas.Brush.Color := c;
  Canvas.Brush.Style := bs;
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.DrawFocusRectangle(AStroke: TAdvGraphicsStroke; ARect: TRectF; AColor: TAdvGraphicsColor; AModifyRectMode: TAdvGraphicsModifyRectMode);
var
{$IFDEF FMXLIB}
  r: TRectF;
{$ENDIF}
{$IFDEF CMNWEBLIB}
{$IFDEF WEBLIB}
  r: TCanvasRectF;
{$ENDIF}
{$IFNDEF WEBLIB}
  r: TRect;
{$ENDIF}
  c: TAdvGraphicsColor;
  bs: TBrushStyle;
{$ENDIF}
begin
  ARect := ModifyRect(ARect, AModifyRectMode);
  {$IFDEF FMXLIB}
  r := ARect;
  Canvas.Stroke.Thickness := 1;
  Canvas.Stroke.Kind := TBrushKind.Solid;
  Canvas.Stroke.Color := AColor;
  {$HINTS OFF}
  {$IF COMPILERVERSION > 31}
  Canvas.Stroke.Dash := TStrokeDash.Dot;
  {$ELSE}
  Canvas.StrokeDash := TStrokeDash.Dot;
  {$IFEND}
  {$HINTS ON}
  Canvas.DrawRect(r, 0, 0, AllCorners, 1);
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  {$IFDEF WEBLIB}
  r := CreateCanvasRectF(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  {$ENDIF}
  {$IFNDEF WEBLIB}
  r := Rect(Round(ARect.Left), Round(ARect.Top), Round(ARect.Right), Round(ARect.Bottom));
  {$ENDIF}
  c := Canvas.Brush.Color;
  bs := Canvas.Brush.Style;
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Color := AColor;
  StartSpecialPen;
  Canvas.Rectangle(r);
  StopSpecialPen;
  Canvas.Brush.Color := c;
  Canvas.Brush.Style := bs;
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.DrawLine(AStroke: TAdvGraphicsStroke; AFromPoint, AToPoint: TPointF;
  AModifyPointModeFrom, AModifyPointModeTo: TAdvGraphicsModifyPointMode);
begin
  {$IFDEF FMXLIB}
  AFromPoint := ModifyPoint(AFromPoint, AModifyPointModeFrom);
  AToPoint := ModifyPoint(AToPoint, AModifyPointModeTo);
  Canvas.DrawLine(AFromPoint, AToPoint, AStroke.Opacity);
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  {$IFDEF WEBLIB}
  Canvas.MoveTo(AFromPoint.X, AFromPoint.Y);
  Canvas.LineTo(AToPoint.X, AToPoint.Y);
  {$ENDIF}
  {$IFNDEF WEBLIB}
  Canvas.MoveTo(Round(AFromPoint.X), Round(AFromPoint.Y));
  Canvas.LineTo(Round(AToPoint.X), Round(AToPoint.Y));
  {$ENDIF}
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.DrawPath(AStroke: TAdvGraphicsStroke; APath: TAdvGraphicsPath;
  APathMode: TAdvGraphicsPathDrawMode = pdmPolygon);
var
  p: TAdvGraphicsPathPolygon;
  {$IFDEF FMXLIB}
  pth: TPathData;
  {$ENDIF}
begin
  if Assigned(APath) then
  begin
    if APathMode = pdmPath then
    begin
      {$IFDEF FMXLIB}
      pth := TPathData(ConvertToPath(APath));
      try
        Canvas.DrawPath(pth, AStroke.Opacity);
      finally
        pth.Free;
      end;
      {$ENDIF}
      {$IFDEF CMNLIB}
      {$IFDEF MSWINDOWS}
      BeginPath(Canvas.Handle);
      ConvertToPath(APath);
      EndPath(Canvas.Handle);
      Windows.StrokePath(Canvas.Handle);
      AbortPath(Canvas.Handle);
      {$ENDIF}
      {$ENDIF}
      {$IFDEF WEBLIB}
      Canvas.PathBegin;
      ConvertToPath(APath);
      Canvas.PathStroke;
      {$ENDIF}
    end
    else
    begin
      SetLength(p, 0);
      APath.FlattenToPolygon(p);
      case APathMode of
        pdmPolygon: DrawPolygon(AStroke, p);
        pdmPolyline: DrawPolyline(AStroke, p);
      end;
    end;
  end;
end;

procedure TAdvGraphicsContextGeneral.DrawPolygon(AStroke: TAdvGraphicsStroke; APolygon: TAdvGraphicsPathPolygon);
{$IFDEF CMNWEBLIB}
var
{$IFDEF WEBLIB}
  pts: array of TCanvasPointF;
{$ENDIF}
{$IFNDEF WEBLIB}
  pts: array of TPoint;
{$ENDIF}
  I: Integer;
  bs: TBrushStyle;
  c: TAdvGraphicsColor;
{$ENDIF}
begin
  {$IFDEF FMXLIB}
  Canvas.DrawPolygon(TPolygon(APolygon), AStroke.Opacity);
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  SetLength(pts, Length(APolygon));
  for I := 0 to Length(APolygon) - 1 do
  begin
    {$IFDEF WEBLIB}
    pts[I] := CreateCanvasPointF(APolygon[I].X, APolygon[I].Y);
    {$ENDIF}
    {$IFNDEF WEBLIB}
    pts[I] := Point(Round(APolygon[I].X), Round(APolygon[I].Y));
    {$ENDIF}
  end;

  c := Canvas.Brush.Color;
  bs := Canvas.Brush.Style;
  Canvas.Brush.Style := bsClear;
  Canvas.Polygon(pts);
  Canvas.Brush.Style := bs;
  Canvas.Brush.Color := c;
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.DrawPolyline(AStroke: TAdvGraphicsStroke; APolyline: TAdvGraphicsPathPolygon);
var
  I: Integer;
{$IFDEF CMNWEBLIB}
{$IFDEF WEBLIB}
  pts: array of TCanvasPointF;
{$ENDIF}
{$IFNDEF WEBLIB}
  pts: array of TPoint;
{$ENDIF}
  bs: TBrushStyle;
  c: TAdvGraphicsColor;
{$ENDIF}
begin
  {$IFDEF FMXLIB}
  for I := 0 to High(APolyline) - 1 do
    DrawLine(AStroke, APolyline[I], APolyline[I + 1]);
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  SetLength(pts, Length(APolyline));
  for I := 0 to Length(APolyline) - 1 do
  begin
    {$IFDEF WEBLIB}
    pts[I] := CreateCanvasPointF(APolyline[I].X, APolyline[I].Y);
    {$ENDIF}
    {$IFNDEF WEBLIB}
    pts[I] := Point(Round(APolyline[I].X), Round(APolyline[I].Y));
    {$ENDIF}
  end;

  c := Canvas.Brush.Color;
  bs := Canvas.Brush.Style;
  Canvas.Brush.Style := bsClear;
  Canvas.Polyline(pts);
  Canvas.Brush.Style := bs;
  Canvas.Brush.Color := c;
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.DrawRect(AStroke: TAdvGraphicsStroke; ARect: TRectF; ASides: TAdvGraphicsSides; AModifyRectMode: TAdvGraphicsModifyRectMode);
var
{$IFDEF FMXWEBLIB}
  r: TRectF;
{$ENDIF}
{$IFDEF CMNLIB}
  r: TRect;
{$ENDIF}
begin
  ARect := ModifyRect(ARect, AModifyRectMode);
  {$IFDEF FMXWEBLIB}
  r := ARect;
  if gsTop in ASides then
    DrawLine(AStroke, PointF(r.Left, r.Top), PointF(r.Right, r.Top));
  if gsLeft in ASides then
    DrawLine(AStroke, PointF(r.Left, r.Top), PointF(r.Left, r.Bottom));
  if gsBottom in ASides then
    DrawLine(AStroke, PointF(r.Left, r.Bottom), PointF(r.Right, r.Bottom));
  if gsRight in ASides then
    DrawLine(AStroke, PointF(r.Right, r.Top), PointF(r.Right, r.Bottom));
  {$ENDIF}
  {$IFDEF CMNLIB}
  r := Rect(Round(ARect.Left), Round(ARect.Top), Round(ARect.Right), Round(ARect.Bottom));
  if gsTop in ASides then
    DrawLine(AStroke, PointF(r.Left, r.Top), PointF(r.Right - 1, r.Top));
  if gsLeft in ASides then
    DrawLine(AStroke, PointF(r.Left, r.Top), PointF(r.Left, r.Bottom - 1));
  if gsBottom in ASides then
    DrawLine(AStroke, PointF(r.Left, r.Bottom - 1), PointF(r.Right - 1, r.Bottom - 1));
  if gsRight in ASides then
    DrawLine(AStroke, PointF(r.Right - 1, r.Top), PointF(r.Right - 1, r.Bottom));
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.DrawRoundRect(AStroke: TAdvGraphicsStroke; ARect: TRectF; ARounding: Single;
  ACorners: TAdvGraphicsCorners; AModifyRectMode: TAdvGraphicsModifyRectMode);
var
{$IFDEF FMXLIB}
  r: TRectF;
  c: TCorners;
{$ENDIF}
{$IFDEF CMNWEBLIB}
  {$IFDEF WEBLIB}
  r, rg: TCanvasRectF;
  rc: Single;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  r, rg: TRect;
  rc: Integer;
  {$ENDIF}
  cl: TAdvGraphicsColor;
  bs: TBrushStyle;

  {$IFDEF WEBLIB}
  procedure CanvasArc(ACanvas: TCanvas; Center, Radius: TPointF; StartAngle, SweepAngle: Integer);
  begin
    ACanvas.AngleArc(Center.X, Center.Y, Radius.X, DegToRad(StartAngle), DegToRad(SweepAngle));
  {$ENDIF}
  {$IFNDEF WEBLIB}
  procedure CanvasArc(ACanvas: TCanvas; Center, Radius: TPoint; StartAngle, SweepAngle: Integer);
  var
    sx, sy, sxs, sys, x, y, xs, ys: Integer;
  begin
    sx := Center.X - Radius.X;
    sy := Center.Y - Radius.Y;
    sxs := Center.X + Radius.X;
    sys := Center.Y + Radius.Y;

    x := Center.X + Round(Radius.X * COS(DegToRad(-StartAngle - SweepAngle)));
    y := Center.Y - Round(Radius.Y * SIN(DegToRad(-StartAngle - SweepAngle)));
    xs := Center.X + Round(Radius.X * COS(DegToRad(-StartAngle)));
    ys := Center.Y - Round(Radius.Y * SIN(DegToRad(- StartAngle)));

    ACanvas.Arc(sx, sy, sxs, sys, x, y, xs, ys);
  {$ENDIF}
  end;
{$ENDIF}
begin
  ARect := ModifyRect(ARect, AModifyRectMode);
  {$IFDEF FMXLIB}
  r := ARect;

  c := [];
  if gcTopLeft in ACorners then
    c := c + [TCorner.TopLeft];

  if gcTopRight in ACorners then
    c := c + [TCorner.TopRight];

  if gcBottomLeft in ACorners then
    c := c + [TCorner.BottomLeft];

  if gcBottomRight in ACorners then
    c := c + [TCorner.BottomRight];

  Canvas.DrawRect(r, ARounding, ARounding, c, AStroke.Opacity);
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  {$IFDEF WEBLIB}
  r := CreateCanvasRectF(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  {$ENDIF}
  {$IFNDEF WEBLIB}
  r := Rect(Round(ARect.Left), Round(ARect.Top), Round(ARect.Right), Round(ARect.Bottom));
  {$ENDIF}

  rg := r;
  rg.Bottom := rg.Bottom - 1;
  rg.Right := rg.Right - 1;

  cl := Canvas.Brush.Color;
  bs := Canvas.Brush.Style;
  Canvas.Brush.Style := bsClear;

  rc := Round(ARounding);
  if ACorners = [gcTopLeft, gcTopRight, gcBottomLeft, gcBottomRight] then
    Canvas.RoundRect(r, rc * 2, rc * 2)
  else if ACorners = [] then
    Canvas.Rectangle(r)
  else
  begin
    {$IFNDEF WEBLIB}
    r.Right := r.Right - 1;
    r.Bottom := r.Bottom - 1;
    rc := rc - 1;
    {$ENDIF}
    if gcBottomLeft in ACorners then
    begin
      Canvas.MoveTo(r.Left + rc, r.Bottom);
      {$IFDEF WEBLIB}
      CanvasArc(Canvas, PointF(r.Left + rc, r.Bottom - rc), PointF(rc, rc), -270, 90);
      {$ENDIF}
      {$IFNDEF WEBLIB}
      CanvasArc(Canvas, Point(r.Left + rc, r.Bottom - rc), Point(rc, rc), -270, 90);
      {$ENDIF}
      Canvas.MoveTo(r.Left, r.Bottom - rc);
    end
    else
    begin
      Canvas.MoveTo(r.Left, r.Bottom);
    end;

    if gcTopLeft in ACorners then
    begin
      {$IFDEF WEBLIB}
      Canvas.LineTo(r.Left, r.Top + rc);
      CanvasArc(Canvas, PointF(r.Left + rc, r.Top + rc), PointF(rc, rc), -180, 90);
      {$ENDIF}
      {$IFNDEF WEBLIB}
      Canvas.LineTo(r.Left, r.Top + rc - 2);
      CanvasArc(Canvas, Point(r.Left + rc, r.Top + rc), Point(rc, rc), -180, 90);
      {$ENDIF}
      Canvas.MoveTo(r.Left + rc, r.Top);
    end
    else
      Canvas.LineTo(r.Left, r.Top);

    if gcTopRight in ACorners then
    begin
      Canvas.LineTo(r.Right - rc, r.Top);
      {$IFDEF WEBLIB}
      CanvasArc(Canvas, PointF(r.Right - rc, r.Top + rc), PointF(rc, rc), -90, 90);
      {$ENDIF}
      {$IFNDEF WEBLIB}
      CanvasArc(Canvas, Point(r.Right - rc, r.Top + rc), Point(rc, rc), -90, 90);
      {$ENDIF}
      Canvas.MoveTo(r.Right, r.Top + rc);
    end
    else
      Canvas.LineTo(r.Right, r.Top);

    if gcBottomRight in ACorners then
    begin
      {$IFDEF WEBLIB}
      Canvas.LineTo(r.Right, r.Bottom - rc);
      CanvasArc(Canvas, PointF(r.Right - rc, r.Bottom - rc), PointF(rc, rc), 0, 90);
      {$ENDIF}
      {$IFNDEF WEBLIB}
      Canvas.LineTo(r.Right, r.Bottom - rc + 1);
      CanvasArc(Canvas, Point(r.Right - rc, r.Bottom - rc + 1), Point(rc, rc), 0, 90);
      {$ENDIF}
      Canvas.MoveTo(r.Right - rc, r.Bottom);
    end
    else
    begin
      Canvas.LineTo(r.Right, r.Bottom);
    end;

    if gcBottomLeft in ACorners then
    begin
      {$IFDEF WEBLIB}
      Canvas.LineTo(r.Left + rc, r.Bottom)
      {$ENDIF}
      {$IFNDEF WEBLIB}
      Canvas.LineTo(r.Left + rc - 2, r.Bottom)
      {$ENDIF}
    end
    else
      Canvas.LineTo(r.Left, r.Bottom);
  end;

  Canvas.Brush.Color := cl;
  Canvas.Brush.Style := bs;
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.DrawText(AText: string; ARect: TRectF;
  AWordWrapping: Boolean; AHorizontalAlign,
  AVerticalAlign: TAdvGraphicsTextAlign;
  ATrimming: TAdvGraphicsTextTrimming; AAngle: Single);
{$IFDEF CMNWEBLIB}
var
  rcalc: TRectF;
  rh: Integer;
  {$IFDEF WEBLIB}
  r: TRectF;
  st: TAdvGraphicsSaveState;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  r: TRect;
  {$ENDIF}
  {$IFDEF LCLLIB}
  {%H-}dstyle: TTextStyle;
  {$IFNDEF MSWINDOWS}       
  crr: TRect;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF VCLLIB}
  dstyle: TTextFormat;
  {$ENDIF}
  c: TAdvGraphicsColor;
  bs: TBrushStyle;
{$ENDIF}
{$IFDEF FMXLIB}
var
  ha, va: TTextAlign;
{$ENDIF}
begin
  {$IFDEF FMXLIB}
  if FPrinting then
  begin
    ha := TTextAlign.Leading;
    va := TTextAlign.Leading;

    case AHorizontalAlign of
      gtaCenter: ha := TTextAlign.Center;
      gtaTrailing: ha := TTextAlign.Trailing;
    end;

    case AVerticalAlign of
      gtaCenter: va := TTextAlign.Center;
      gtaTrailing: va := TTextAlign.Trailing;
    end;

    Canvas.FillText(ARect, AText, AWordWrapping, 255, [], ha, va);

    Exit;
  end;
  {$ENDIF}

  {$IFDEF FMXLIB}

  if not Assigned(FTextLayout) then
    FTextLayout := TTextLayoutManager.DefaultTextLayout.Create(Canvas);

  FTextLayout.BeginUpdate;
  try
    FTextLayout.TopLeft := PointF(Round(ARect.Left), Round(ARect.Top));
    FTextLayout.Text := AText;
    FTextLayout.MaxSize := PointF(Round(ARect.Width), Round(ARect.Height));
    FTextLayout.WordWrap := AWordWrapping;
    FTextLayout.Opacity := 1;
    FTextLayout.HorizontalAlign := TTextAlign(Integer(AHorizontalAlign));
    FTextLayout.VerticalAlign := TTextAlign(Integer(AVerticalAlign));
    FTextLayout.Font.Assign(Canvas.Font);
    FTextLayout.Color := Canvas.Fill.Color;
    if not AWordWrapping then
      FTextLayout.Trimming := TTextTrimming(Integer(ATrimming))
    else
      FTextLayout.Trimming := TTextTrimming.None;
  finally
    FTextLayout.EndUpdate;
  end;
  FTextLayout.RenderLayout(Canvas);
  {$ENDIF}

  {$IFDEF CMNWEBLIB}
  c := Canvas.Brush.Color;
  bs := Canvas.Brush.Style;
  Canvas.Brush.Style := bsClear;

  {$IFDEF WEBLIB}
  r := RectF(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  {$ENDIF}
  {$IFNDEF WEBLIB}
  r := Rect(Round(ARect.Left), Round(ARect.Top), Round(ARect.Right), Round(ARect.Bottom));
  {$ENDIF}
  {$IFDEF VCLLIB}
  dstyle := [];
  if AWordWrapping then
    dstyle := dstyle + [tfWordBreak]
  else
    dstyle := dstyle + [tfSingleLine];

  if not FShowAcceleratorChar then
    dstyle := dstyle + [tfNoPrefix];

  case AHorizontalAlign of
    gtaCenter: dstyle := dstyle + [tfCenter];
    gtaLeading: dstyle := dstyle + [tfLeft];
    gtaTrailing: dstyle := dstyle + [tfRight];
  end;

  case AVerticalAlign of
    gtaCenter: dstyle := dstyle + [tfVerticalCenter];
    gtaLeading: dstyle := dstyle + [tfTop];
    gtaTrailing: dstyle := dstyle + [tfBottom];
  end;

  case ATrimming of
    gttCharacter: dstyle := dstyle + [tfEndEllipsis];
    {$HINTS OFF}
    {$IF COMPILERVERSION > 22}
    gttWord: dstyle := dstyle + [tfWordEllipsis];
    {$IFEND}
    {$HINTS ON}
  end;
  {$ENDIF}

  {$IFDEF LCLLIB}
  dStyle.RightToLeft := False;
  dStyle.WordBreak := AWordWrapping;
  dStyle.SingleLine := not AWordWrapping;
  dStyle.Clipping := True;
  dStyle.EndEllipsis := False;
  dStyle.SystemFont := False;
  dStyle.ShowPrefix := FShowAcceleratorChar;

  case AHorizontalAlign of
    gtaCenter: dstyle.Alignment := taCenter;
    gtaLeading: dstyle.Alignment := taLeftJustify;
    gtaTrailing: dstyle.Alignment := taRightJustify;
  end;

  case AVerticalAlign of
    gtaCenter: dstyle.Layout := tlCenter;
    gtaLeading: dstyle.Layout := tlTop;
    gtaTrailing: dstyle.Layout := tlBottom;
  end;

  case ATrimming of
    gttCharacter: dstyle.EndEllipsis := True;
  end;
  {$ENDIF}

  if AWordWrapping then
  begin
    case AVerticalAlign of
      gtaCenter:
      begin
        rcalc := CalculateText(AText, ARect, AWordWrapping);
        rh := Round(rcalc.Bottom - rcalc.Top);
        {$IFDEF WEBLIB}
        r.Top := r.Top + Max(0, ((r.Bottom - r.Top) - rh) / 2);
        {$ENDIF}
        {$IFNDEF WEBLIB}
        r.Top := r.Top + Max(0, ((r.Bottom - r.Top) - rh) div 2);
        {$ENDIF}
        r.Bottom := Max(0, Min(Round(ARect.Bottom), r.Top + rh));
      end;
      gtaTrailing:
      begin
        rcalc := CalculateText(AText, ARect, AWordWrapping);
        rh := Round(rcalc.Bottom - rcalc.Top);
        r.Top := r.Bottom - rh;
        r.Bottom := r.Top + rh;
      end;
    end;
  end;

  if AAngle <> 0 then
    DrawRotatedText(Canvas, r, AText, AAngle, AHorizontalAlign, AVerticalAlign)
  else
  begin
  {$IFDEF GDIPLUSDRAWING}
    b := CreateGDIPFontFill(Font);
    ft := CreateGDIPFont(Font);
    sf := CreateGPStringFormat(AHorizontalAlign, AVerticalAlign, AWordWrapping);
    try
      rt := MakeRect(ARect.Left, ARect.Top, ARect.Width, ARect.Height);
      FGDIPCanvas.DrawString(AText, Length(AText), ft, rt, sf, b);
    finally
      sf.Free;
      ft.Free;
      b.Free;
    end;
  {$ENDIF}
  {$IFNDEF GDIPLUSDRAWING}
  {$IFDEF WEBLIB}
    st := TAdvGraphicsSaveState.Create;
    try
      SaveState(st);
      ClipRect(r);
      case AHorizontalAlign of
        gtaLeading:
        begin
          case AVerticalAlign of
            gtaLeading: Canvas.TextRect(CreateCanvasRectF(r.Left, r.Top, r.Right, r.Bottom), AText, AWordWrapping, False, taLeftJustify, taAlignTop);
            gtaCenter: Canvas.TextRect(CreateCanvasRectF(r.Left, r.Top, r.Right, r.Bottom), AText, AWordWrapping, False, taLeftJustify, taVerticalCenter);
            gtaTrailing: Canvas.TextRect(CreateCanvasRectF(r.Left, r.Top, r.Right, r.Bottom), AText, AWordWrapping, False, taLeftJustify, taAlignBottom);
          end;
        end;
        gtaCenter:
        begin
          case AVerticalAlign of
            gtaLeading: Canvas.TextRect(CreateCanvasRectF(r.Left, r.Top, r.Right, r.Bottom), AText, AWordWrapping, False, taCenter, taAlignTop);
            gtaCenter: Canvas.TextRect(CreateCanvasRectF(r.Left, r.Top, r.Right, r.Bottom), AText, AWordWrapping, False, taCenter, taVerticalCenter);
            gtaTrailing: Canvas.TextRect(CreateCanvasRectF(r.Left, r.Top, r.Right, r.Bottom), AText, AWordWrapping, False, taCenter, taAlignBottom);
          end;
        end;
        gtaTrailing:
        begin
          case AVerticalAlign of
            gtaLeading: Canvas.TextRect(CreateCanvasRectF(r.Left, r.Top, r.Right, r.Bottom), AText, AWordWrapping, False, taRightJustify, taAlignTop);
            gtaCenter: Canvas.TextRect(CreateCanvasRectF(r.Left, r.Top, r.Right, r.Bottom), AText, AWordWrapping, False, taRightJustify, taVerticalCenter);
            gtaTrailing: Canvas.TextRect(CreateCanvasRectF(r.Left, r.Top, r.Right, r.Bottom), AText, AWordWrapping, False, taRightJustify, taAlignBottom);
          end;
        end;
      end;
    finally
      RestoreState(st);
    end;
  {$ENDIF}
  {$IFDEF VCLLIB}
    Canvas.TextRect(r, AText, dstyle);
  {$ENDIF}
  {$IFDEF LCLLIB}
    {$IFNDEF MSWINDOWS}
    crr := Rect(0, 0, 0, 0);
    if not Canvas.Clipping or (IntersectRect(crr, Canvas.ClipRect, r) and Canvas.Clipping) then
    {$ENDIF}
      Canvas.TextRect(r, r.Left, r.Top, AText, dstyle);
  {$ENDIF}
  {$ENDIF}
  end;

  Canvas.Brush.Color := c;
  Canvas.Brush.Style := bs;
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.EndPrinting;
begin
  if FPrinting then
    FPrinting := False;
end;

procedure TAdvGraphicsContextGeneral.EndScene;
begin
  {$IFDEF FMXWEBLIB}
  Canvas.EndScene;
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.FillArc(AFill: TAdvGraphicsFill; ACenter, ARadius: TPointF; AStartAngle, ASweepAngle: Single);
{$IFDEF CMNWEBLIB}
var
  {$IFNDEF WEBLIB}
  x, y, xs, ys, sx, sy, sxs, sys: Integer;
  {$ENDIF}
  ps: TPenStyle;
{$ENDIF}
begin
  {$IFDEF FMXLIB}
  Canvas.FillArc(ACenter, ARadius, AStartAngle, ASweepAngle, AFill.Opacity);
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  {$IFNDEF WEBLIB}
  sx := Floor(ACenter.X - ARadius.X);
  sy := Floor(ACenter.Y - ARadius.Y);
  sxs := Floor(ACenter.X + ARadius.X);
  sys := Floor(ACenter.Y + ARadius.Y);

  x := Floor(ACenter.X + (ARadius.X * COS(DegToRad(-AStartAngle - ASweepAngle))));
  y := Floor(ACenter.Y - (ARadius.Y * SIN(DegToRad(-AStartAngle - ASweepAngle))));
  xs := Floor(ACenter.X + (ARadius.X * COS(DegToRad(-AStartAngle))));
  ys := Floor(ACenter.Y - (ARadius.Y * SIN(DegToRad(- AStartAngle))));
  {$ENDIF}

  ps := Canvas.Pen.Style;
  Canvas.Pen.Style := psClear;
  {$IFNDEF WEBLIB}
  Canvas.Arc(sx, sy, sxs, sys, x, y, xs, ys);
  {$ENDIF}
  {$IFDEF WEBLIB}
  Canvas.AngleArc(ACenter.X, ACenter.Y, ARadius.X, DegToRad(AStartAngle), DegToRad(ASweepAngle));
  {$ENDIF}
  Canvas.Pen.Style := ps;
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.FillEllipse(AFill: TAdvGraphicsFill; ARect: TRectF; AModifyRectMode: TAdvGraphicsModifyRectMode);
var
{$IFDEF FMXLIB}
  r: TRectF;
{$ENDIF}
{$IFDEF CMNWEBLIB}
{$IFDEF WEBLIB}
  r: TCanvasRectF;
{$ENDIF}
{$IFNDEF WEBLIB}
  r: TRect;
{$ENDIF}
  ps: TPenStyle;
{$ENDIF}
begin
  ARect := ModifyRect(ARect, AModifyRectMode);
  {$IFDEF FMXLIB}
  r := ARect;
  Canvas.FillEllipse(r, AFill.Opacity);
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  {$IFDEF WEBLIB}
  r := CreateCanvasRectF(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  {$ENDIF}
  {$IFNDEF WEBLIB}
  r := Rect(Round(ARect.Left), Round(ARect.Top), Round(ARect.Right), Round(ARect.Bottom));
  {$ENDIF}

  ps := Canvas.Pen.Style;
  Canvas.Pen.Style := psClear;
  Canvas.Ellipse(r);
  Canvas.Pen.Style := ps;
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.FillPath(AFill: TAdvGraphicsFill;
  APath: TAdvGraphicsPath; APathMode: TAdvGraphicsPathDrawMode = pdmPolygon);
var
  p: TAdvGraphicsPathPolygon;
  {$IFDEF FMXLIB}
  pth: TPathData;
  {$ENDIF}
begin
  if Assigned(APath) then
  begin
    if APathMode = pdmPath then
    begin
      {$IFDEF FMXLIB}
      pth := TPathData(ConvertToPath(APath));
      try
        Canvas.FillPath(pth, AFill.Opacity);
      finally
        pth.Free;
      end;
      {$ENDIF}
      {$IFDEF CMNLIB}
      {$IFDEF MSWINDOWS}
      BeginPath(Canvas.Handle);
      ConvertToPath(APath);
      EndPath(Canvas.Handle);
      Windows.FillPath(Canvas.Handle);
      AbortPath(Canvas.Handle);
      {$ENDIF}
      {$ENDIF}
      {$IFDEF WEBLIB}
      Canvas.PathBegin;
      ConvertToPath(APath);
      Canvas.PathFill;
      {$ENDIF}
    end
    else
    begin
      SetLength(p, 0);
      APath.FlattenToPolygon(p);
      case APathMode of
        pdmPolygon: FillPolygon(AFill, p);
        pdmPolyline, pdmPath: FillPolyline(AFill, p);
      end;
    end;
  end;
end;

procedure TAdvGraphicsContextGeneral.FillPolygon(AFill: TAdvGraphicsFill; APolygon: TAdvGraphicsPathPolygon);
{$IFDEF CMNWEBLIB}
var
  {$IFDEF WEBLIB}
  pts: array of TCanvasPointF;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  pts: array of TPoint;
  {$ENDIF}
  I: Integer;
  ps: TPenStyle;
{$ENDIF}
begin
  {$IFDEF FMXLIB}
  Canvas.FillPolygon(TPolygon(APolygon), AFill.Opacity);
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  SetLength(pts, Length(APolygon));
  for I := 0 to Length(APolygon) - 1 do
  begin
    {$IFDEF WEBLIB}
    pts[I] := CreateCanvasPointF(APolygon[I].X, APolygon[I].Y);
    {$ENDIF}
    {$IFNDEF WEBLIB}
    pts[I] := Point(Round(APolygon[I].X), Round(APolygon[I].Y));
    {$ENDIF}
  end;

  ps := Canvas.Pen.Style;
  Canvas.Pen.Style := psClear;
  Canvas.Polygon(pts);
  Canvas.Pen.Style := ps;
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.FillPolyline(AFill: TAdvGraphicsFill; APolyline: TAdvGraphicsPathPolygon);
{$IFDEF CMNWEBLIB}
var
  {$IFDEF WEBLIB}
  pts: array of TCanvasPointF;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  pts: array of TPoint;
  {$ENDIF}
  I: Integer;
  ps: TPenStyle;
{$ENDIF}
begin
  {$IFDEF FMXLIB}
  Canvas.FillPolygon(TPolygon(APolyline), AFill.Opacity);
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  SetLength(pts, Length(APolyline));
  for I := 0 to Length(APolyline) - 1 do
  begin
    {$IFDEF WEBLIB}
    pts[I] := CreateCanvasPointF(APolyline[I].X, APolyline[I].Y);
    {$ENDIF}
    {$IFNDEF WEBLIB}
    pts[I] := Point(Round(APolyline[I].X), Round(APolyline[I].Y));
    {$ENDIF}
  end;

  ps := Canvas.Pen.Style;
  Canvas.Pen.Style := psClear;
  Canvas.Polyline(pts);
  Canvas.Pen.Style := ps;
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.FillRect(AFill: TAdvGraphicsFill; ARect: TRectF; AModifyRectMode: TAdvGraphicsModifyRectMode);
var
{$IFDEF FMXLIB}
  r: TRectF;
{$ENDIF}
{$IFDEF CMNWEBLIB}
  {$IFDEF WEBLIB}
  r: TRectF;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  r: TRect;
  {$ENDIF}
  c: TAdvGraphicsColor;
  bs: TBrushStyle;
  ps: TPenStyle;
{$ENDIF}
begin
  ARect := ModifyRect(ARect, AModifyRectMode);
  {$IFDEF FMXLIB}
  r := ARect;
  Canvas.FillRect(r, 0, 0, AllCorners, AFill.Opacity);
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  {$IFDEF WEBLIB}
  r := RectF(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  {$ENDIF}
  {$IFNDEF WEBLIB}
  r := Rect(Round(ARect.Left), Round(ARect.Top), Round(ARect.Right), Round(ARect.Bottom));
  {$ENDIF}

  c := gcNull;
  bs := bsClear;
  if (AFill.Kind = gfkGradient) then
  begin
    if (AFill.Color <> gcNull) and (AFill.ColorTo <> gcNull) then
    begin
      if (AFill.ColorMirror <> gcNull) and (AFill.ColorMirrorTo <> gcNull) then
      begin
        case AFill.Orientation of
          gfoHorizontal:
          begin
            {$IFDEF WEBLIB}
            DrawGradient(Canvas, AFill.Color, AFill.ColorTo, RectF(r.Left, r.Top, r.Left + (r.Right - r.Left) / 2, r.Bottom), 0, [], True);
            DrawGradient(Canvas, AFill.ColorMirror, AFill.ColorMirrorTo, RectF(r.Left + (r.Right - r.Left) / 2, r.Top, r.Right, r.Bottom), 0, [], True);
            {$ENDIF}
            {$IFNDEF WEBLIB}
            DrawGradient(Canvas, AFill.Color, AFill.ColorTo, Rect(r.Left, r.Top, r.Left + (r.Right - r.Left) div 2, r.Bottom), 0, [], True);
            DrawGradient(Canvas, AFill.ColorMirror, AFill.ColorMirrorTo, Rect(r.Left + (r.Right - r.Left) div 2, r.Top, r.Right, r.Bottom), 0, [], True);
            {$ENDIF}
          end;
          gfoVertical:
          begin
            {$IFDEF WEBLIB}
            DrawGradient(Canvas, AFill.Color, AFill.ColorTo, RectF(r.Left, r.Top, r.Right, r.Top + (r.Bottom - r.Top) / 2), 0, [], False);
            DrawGradient(Canvas, AFill.ColorMirror, AFill.ColorMirrorTo, RectF(r.Left, r.Top + (r.Bottom - r.Top) / 2, r.Right, r.Bottom), 0, [], False);
            {$ENDIF}
            {$IFNDEF WEBLIB}
            DrawGradient(Canvas, AFill.Color, AFill.ColorTo, Rect(r.Left, r.Top, r.Right, r.Top + (r.Bottom - r.Top) div 2), 0, [], False);
            DrawGradient(Canvas, AFill.ColorMirror, AFill.ColorMirrorTo, Rect(r.Left, r.Top + (r.Bottom - r.Top) div 2, r.Right, r.Bottom), 0, [], False);
            {$ENDIF}
          end;
        end;
      end
      else
        DrawGradient(Canvas, AFill.Color, AFill.ColorTo, r, 0, [], AFill.Orientation = gfoHorizontal);
    end;

    c := Canvas.Brush.Color;
    bs := Canvas.Brush.Style;
    Canvas.Brush.Style := bsClear;
  end;

  ps := Canvas.Pen.Style;
  Canvas.Pen.Style := psClear;
  if (AFill.Color <> gcNull) and (AFill.Kind = gfkSolid) then
  begin
    {$IFDEF WEBLIB}
    Canvas.Rectangle(CreateCanvasRectF(r.Left, r.Top, r.Right, r.Bottom));
    {$ENDIF}
    {$IFNDEF WEBLIB}
    Canvas.Rectangle(r);
    {$ENDIF}
  end;
  Canvas.Pen.Style := ps;

  if AFill.Kind = gfkGradient then
  begin
    Canvas.Brush.Color := c;
    Canvas.Brush.Style := bs;
  end;
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.FillRoundRect(AFill: TAdvGraphicsFill; ARect: TRectF; ARounding: Single;
  ACorners: TAdvGraphicsCorners; AModifyRectMode: TAdvGraphicsModifyRectMode);
var
{$IFDEF FMXLIB}
  r: TRectF;
  c: TCorners;
{$ENDIF}
{$IFDEF CMNWEBLIB}
  {$IFDEF WEBLIB}
  r, rg: TRectF;
  {$ENDIF}
  {$IFNDEF WEBLIB}
  r, rg: TRect;
  {$ENDIF}
  c: TAdvGraphicsCorners;
{$ENDIF}
begin
  ARect := ModifyRect(ARect, AModifyRectMode);
  {$IFDEF FMXLIB}
  r := ARect;

  c := [];
  if gcTopLeft in ACorners then
    c := c + [TCorner.TopLeft];

  if gcTopRight in ACorners then
    c := c + [TCorner.TopRight];

  if gcBottomLeft in ACorners then
    c := c + [TCorner.BottomLeft];

  if gcBottomRight in ACorners then
    c := c + [TCorner.BottomRight];

  Canvas.FillRect(r, ARounding, ARounding, c, AFill.Opacity);
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  {$IFDEF WEBLIB}
  r := RectF(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  {$ENDIF}
  {$IFNDEF WEBLIB}
  r := Rect(Round(ARect.Left), Round(ARect.Top), Round(ARect.Right), Round(ARect.Bottom));
  {$ENDIF}

  rg := r;
  rg.Bottom := rg.Bottom - 1;
  rg.Right := rg.Right - 1;

  if (AFill.Kind = gfkGradient) then
  begin
    if (AFill.Color <> gcNull) and (AFill.ColorTo <> gcNull) then
    begin
      if (AFill.ColorMirror <> gcNull) and (AFill.ColorMirrorTo <> gcNull) then
      begin
        case AFill.Orientation of
          gfoHorizontal:
          begin
            c := [];
            if gcTopLeft in ACorners then
              c := c + [gcTopLeft];
            if gcBottomLeft in ACorners then
              c := c + [gcBottomLeft];

            {$IFDEF WEBLIB}
            DrawGradient(Canvas, AFill.Color, AFill.ColorTo, RectF(rg.Left, rg.Top, rg.Left + (rg.Right - rg.Left) / 2, r.Bottom), ARounding, c, True);
            {$ENDIF}
            {$IFNDEF WEBLIB}
            DrawGradient(Canvas, AFill.Color, AFill.ColorTo, Rect(rg.Left, rg.Top, rg.Left + (rg.Right - rg.Left) div 2, r.Bottom), ARounding, c, True);
            {$ENDIF}

            c := [];
            if gcTopRight in ACorners then
              c := c + [gcTopRight];
            if gcBottomRight in ACorners then
              c := c + [gcBottomRight];

            {$IFDEF WEBLIB}
            DrawGradient(Canvas, AFill.ColorMirror, AFill.ColorMirrorTo, RectF(rg.Left + (rg.Right - r.Left) / 2, rg.Top, rg.Right, rg.Bottom), ARounding, c, True);
            {$ENDIF}
            {$IFNDEF WEBLIB}
            DrawGradient(Canvas, AFill.ColorMirror, AFill.ColorMirrorTo, Rect(rg.Left + (rg.Right - r.Left) div 2, rg.Top, rg.Right, rg.Bottom), ARounding, c, True);
            {$ENDIF}
          end;
          gfoVertical:
          begin
            c := [];
            if gcTopLeft in ACorners then
              c := c + [gcTopLeft];
            if gcTopRight in ACorners then
              c := c + [gcTopRight];

            {$IFDEF WEBLIB}
            DrawGradient(Canvas, AFill.Color, AFill.ColorTo, RectF(rg.Left, rg.Top, rg.Right, rg.Top + (rg.Bottom - rg.Top) / 2), ARounding, c, False);
            {$ENDIF}
            {$IFNDEF WEBLIB}
            DrawGradient(Canvas, AFill.Color, AFill.ColorTo, Rect(rg.Left, rg.Top, rg.Right, rg.Top + (rg.Bottom - rg.Top) div 2), ARounding, c, False);
            {$ENDIF}

            c := [];
            if gcBottomLeft in ACorners then
              c := c + [gcBottomLeft];
            if gcBottomRight in ACorners then
              c := c + [gcBottomRight];

            {$IFDEF WEBLIB}
            DrawGradient(Canvas, AFill.ColorMirror, AFill.ColorMirrorTo, RectF(rg.Left, rg.Top + (rg.Bottom - rg.Top) / 2, rg.Right, rg.Bottom), ARounding, c, False);
            {$ENDIF}
            {$IFNDEF WEBLIB}
            DrawGradient(Canvas, AFill.ColorMirror, AFill.ColorMirrorTo, Rect(rg.Left, rg.Top + (rg.Bottom - rg.Top) div 2, rg.Right, rg.Bottom), ARounding, c, False);
            {$ENDIF}
          end;
        end;
      end
      else
        DrawGradient(Canvas, AFill.Color, AFill.ColorTo, rg, ARounding, ACorners, AFill.Orientation = gfoHorizontal);
    end;
  end
  else if (AFill.Color <> gcNull) and (AFill.Kind = gfkSolid) then
    DrawGradient(Canvas, AFill.Color, AFill.Color, r, ARounding, ACorners, AFill.Orientation = gfoHorizontal);
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.StartSpecialPen;
{$IFDEF CMNLIB}
{$IFNDEF DARWIN}
var
  hpen: THandle;
  lb: TLogBrush;
{$ENDIF}
{$ENDIF}
begin
  {$IFDEF CMNWEBLIB}
  {$IFNDEF WEBLIB}
  if FPrinting then
  {$ENDIF}
  begin
    FOldPenStyle := Canvas.Pen.Style;
    FOldPenWidth := Canvas.Pen.Width;
    Canvas.Pen.Style := psDot;
    Canvas.Pen.Width := 1;
  end
  {$IFNDEF WEBLIB}
  {$IFNDEF DARWIN}
  else
  begin
    lb.lbColor := ColorToRGB(Canvas.Pen.Color);
    lb.lbStyle := BS_SOLID;

    hpen := ExtCreatePen(PS_COSMETIC or PS_ALTERNATE, 1, lb, 0, nil);
    FOldPenHandle := SelectObject(Canvas.Handle, hpen);
  end;
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.StopSpecialPen;
begin
  {$IFDEF CMNWEBLIB}
  {$IFNDEF WEBLIB}
  if FPrinting then
  {$ENDIF}
  begin
    Canvas.Pen.Style := FOldPenStyle;
    Canvas.Pen.Width := FOldPenWidth;
  end
  {$IFNDEF WEBLIB}
  {$IFNDEF DARWIN}
  else
  begin
    DeleteObject(SelectObject(Canvas.Handle, FOldPenHandle));
  end;
  {$ENDIF}
  {$ENDIF}
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.TranslateTransform(AX, AY: Single);
{$IFDEF WEBLIB}
var
  m: TAdvGraphicsMatrix;
{$ENDIF}
begin
  {$IFDEF FMXLIB}
  SaveMatrix;
  Canvas.MultiplyMatrix(TMatrix.CreateTranslation(AX, AY));
  {$ENDIF}
  {$IFDEF WEBLIB}
  m := MatrixCreateTranslation(AX, AY);
  Canvas.Transform(m.m11, m.m12, m.m21, m.m22, m.m31, m.m32);
  {$ENDIF}
end;

function TAdvGraphicsContextGeneral.GetFillColor: TAdvGraphicsColor;
begin
  {$IFDEF FMXLIB}
  Result := Canvas.Fill.Color;
  {$ENDIF}
  {$IFNDEF FMXLIB}
  Result := Canvas.Brush.Color;
  {$ENDIF}
end;

function TAdvGraphicsContextGeneral.GetMatrix: TAdvGraphicsMatrix;
{$IFDEF FMXLIB}
var
  M: TMatrix;
{$ENDIF}
begin
  Result := MatrixIdentity;
  {$IFDEF FMXLIB}
  M := Canvas.Matrix;
  Result.m11 := M.m11;
  Result.m12 := M.m12;
  Result.m13 := M.m13;
  Result.m21 := M.m21;
  Result.m22 := M.m22;
  Result.m23 := M.m23;
  Result.m31 := M.m31;
  Result.m32 := M.m32;
  Result.m33 := M.m33;
  {$ENDIF}
end;

function TAdvGraphicsContextGeneral.GetNativeCanvas: Pointer;
begin
  Result := Canvas;
end;

procedure TAdvGraphicsContextGeneral.PathClose(APath: Pointer);
begin
  {$IFDEF FMXLIB}
  TPathData(APath).ClosePath;
  {$ENDIF}
  {$IFDEF CMNLIB}
  {$IFDEF MSWINDOWS}
  CloseFigure(Canvas.Handle);
  {$ENDIF}
  {$ENDIF}
  {$IFDEF WEBLIB}
  Canvas.PathClose;
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.PathLineTo(APath: Pointer;
  APoint: TPointF);
begin
  {$IFDEF FMXLIB}
  TPathData(APath).LineTo(APoint);
  {$ENDIF}
  {$IFDEF CMNLIB}
  {$IFDEF MSWINDOWS}
  Canvas.LineTo(Round(APoint.X), Round(APoint.Y));;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF WEBLIB}
  Canvas.PathLineTo(APoint.X, APoint.Y);
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.PathMoveTo(APath: Pointer;
  APoint: TPointF);
begin
  {$IFDEF FMXLIB}
  TPathData(APath).MoveTo(APoint);
  {$ENDIF}
  {$IFDEF CMNLIB}
  {$IFDEF MSWINDOWS}
  Canvas.MoveTo(Round(APoint.X), Round(APoint.Y));
  {$ENDIF}
  {$ENDIF}
  {$IFDEF WEBLIB}
  Canvas.PathMoveTo(APoint.X, APoint.Y);
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.PathOpen(APath: Pointer);
begin

end;

procedure TAdvGraphicsContextGeneral.Render;
begin

end;

procedure TAdvGraphicsContextGeneral.ResetClip;
begin
  {$IFDEF WEBLIB}
  RestoreState(nil);
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.ResetTextAngle(AAngle: Single);
begin
  {$IFDEF FMXLIB}
  if AAngle <> 0 then
    Canvas.SetMatrix(FTextMatrix);
  {$ENDIF}
  {$IFDEF WEBLIB}
  if AAngle <> 0 then
    Canvas.SetTransform(1, 0, 0, 1, 0, 0);
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.ResetTransform;
begin
  {$IFDEF FMXLIB}
  RestoreMatrix;
  {$ENDIF}
  {$IFDEF WEBLIB}
  Canvas.SetTransform(1, 0, 0, 1, 0, 0);
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.RestoreState(AState: TAdvGraphicsSaveState);
begin
  {$IFDEF CMNWEBLIB}
  {$IFDEF VCLLIB}
  RestoreDC(Canvas.Handle, AState.SaveDC);
  {$ENDIF}
  {$IFDEF LCLLIB}
  Canvas.Clipping := False;
  Canvas.RestoreHandleState;
  {$ENDIF}
  if Assigned(AState) then
    AState.SaveDC := 0;
  {$ENDIF}
  {$IFDEF FMXLIB}
  Canvas.RestoreState(AState.SaveDC);
  AState.SaveDC := nil;
  {$ENDIF}
  {$IFDEF WEBLIB}
  Canvas.Restore;
  {$ENDIF}

  {$IFNDEF GDIPLUSDRAWING}
  {$IFDEF CMNWEBLIB}
  Canvas.Refresh;
  {$ENDIF}
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.RotateTransform(AAngle: Single);
{$IFDEF WEBLIB}
var
  m: TAdvGraphicsMatrix;
{$ENDIF}
begin
  {$IFDEF FMXLIB}
  SaveMatrix;
  Canvas.MultiplyMatrix(TMatrix.CreateRotation(DegToRad(AAngle)));
  {$ENDIF}
  {$IFDEF WEBLIB}
  m := MatrixCreateRotation(DegToRad(AAngle));
  Canvas.Transform(m.m11, m.m12, m.m21, m.m22, m.m31, m.m32);
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.SaveState(AState: TAdvGraphicsSaveState);
begin
  {$IFDEF CMNWEBLIB}
  Canvas.Refresh;
  {$ENDIF}
  {$IFDEF FMXLIB}
  AState.SaveDC := Canvas.SaveState;
  {$ENDIF}
  {$IFDEF VCLLIB}
  AState.SaveDC := SaveDC(Canvas.Handle);
  {$ENDIF}
  {$IFDEF LCLLIB}
  Canvas.SaveHandleState;
  {$ENDIF}
  {$IFDEF WEBLIB}
  Canvas.Save;
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.ScaleTransform(AX, AY: Single);
{$IFDEF WEBLIB}
var
  m: TAdvGraphicsMatrix;
{$ENDIF}
begin
  {$IFDEF FMXLIB}
  SaveMatrix;
  Canvas.MultiplyMatrix(TMatrix.CreateScaling(AX, AY));
  {$ENDIF}
  {$IFDEF WEBLIB}
  m := MatrixCreateScaling(AX, AY);
  Canvas.Transform(m.m11, m.m12, m.m21, m.m22, m.m31, m.m32);
  {$ENDIF}
end;

function TAdvGraphicsContextGeneral.SetTextAngle(ARect: TRectF; AAngle: Single): TRectF;
{$IFDEF FMXWEBLIB}
var
  ar: Single;
  cx: TPointF;
  {$IFDEF FMXLIB}
  rm: TMatrix;
  {$ENDIF}
  c, s: Single;
  w, cw, h, ch: Single;
{$ENDIF}
begin
  Result := ARect;
  {$IFDEF FMXWEBLIB}
  if AAngle <> 0 then
  begin
    ar := DegToRad(AAngle);
    cx.X := Result.Left + (Result.Right - Result.Left) / 2;
    cx.Y := Result.Top + (Result.Bottom - Result.Top) / 2;
    {$IFDEF FMXLIB}
    FTextMatrix := Canvas.Matrix;
    rm := TMatrix.CreateRotation(ar) * TMatrix.CreateTranslation(cx.X, cx.Y);
    Canvas.MultiplyMatrix(rm);
    {$ENDIF}
    {$IFDEF WEBLIB}
    Canvas.Transform(Cos(ar), Sin(ar), -Sin(ar), Cos(ar), cx.X, cx.Y);
    {$ENDIF}

    w := Result.Right - Result.Left;
    h := Result.Bottom - Result.Top;
    c := Cos(ar);
    s := Sin(ar);

    cw := (Abs(w * c) + Abs(h * s));
    ch := (Abs(w * s) + Abs(h * c));

    Result := RectF(-cw / 2, -ch / 2, cw / 2, ch / 2);
  end;
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.SetTextQuality(
  ATextQuality: TAdvGraphicsTextQuality);
begin
end;

procedure TAdvGraphicsContextGeneral.SetSize(AWidth, AHeight: Single);
begin
end;

procedure TAdvGraphicsContextGeneral.SetShowAcceleratorChar(AShowAcceleratorChar: Boolean);
begin
  {$IFNDEF WEBLIB}
  FShowAcceleratorChar := AShowAcceleratorChar;
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.SetAntiAliasing(AAntiAliasing: Boolean);
begin
end;

procedure TAdvGraphicsContextGeneral.SetFill(AFill: TAdvGraphicsFill);
{$IFDEF FMXLIB}
var
  pt: TGradientPoint;
{$ENDIF}
begin
  {$IFDEF FMXLIB}
  Canvas.Fill.Color := AFill.Color;
  case AFill.Kind of
    gfkSolid:
    begin
      Canvas.Fill.Kind := TBrushKind.Solid;
    end;
    gfkGradient:
    begin
      Canvas.Fill.Kind := TBrushKind.Gradient;
      Canvas.Fill.Gradient.Points.Clear;

      if (AFill.ColorMirror <> gcNull) and (AFill.ColorMirrorTo <> gcNull) then
      begin
        pt := TGradientPoint(Canvas.Fill.Gradient.Points.Add);
        pt.Color := AFill.Color;
        pt.Offset := 0;

        pt := TGradientPoint(Canvas.Fill.Gradient.Points.Add);
        pt.Color := AFill.ColorTo;
        pt.Offset := 0.5;

        pt := TGradientPoint(Canvas.Fill.Gradient.Points.Add);
        pt.Color := AFill.ColorMirror;
        pt.Offset := 0.5;

        pt := TGradientPoint(Canvas.Fill.Gradient.Points.Add);
        pt.Color := AFill.ColorMirrorTo;
        pt.Offset := 1;
      end
      else
      begin
        pt := TGradientPoint(Canvas.Fill.Gradient.Points.Add);
        pt.Color := AFill.Color;
        pt.Offset := 0;

        pt := TGradientPoint(Canvas.Fill.Gradient.Points.Add);
        pt.Color := AFill.ColorTo;
        pt.Offset := 1;
      end;

      case AFill.Orientation of
        gfoHorizontal:
        begin
          Canvas.Fill.Gradient.StartPosition.Point := PointF(0, 0.5);
          Canvas.Fill.Gradient.StopPosition.Point := PointF(1, 0.5);
        end;
        gfoVertical:
        begin
          Canvas.Fill.Gradient.StartPosition.Point := PointF(0.5, 0);
          Canvas.Fill.Gradient.StopPosition.Point := PointF(0.5, 1);
        end;
      end;
    end
    else
      Canvas.Fill.Kind := TBrushKind.None;
  end;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  Canvas.Brush.Color := AFill.Color;
  {$IFDEF WEBLIB}
  Canvas.Brush.Gradient := bgNone;
  Canvas.Brush.ClearGradient;
  {$ENDIF}
  case AFill.Kind of
    {$IFDEF WEBLIB}
    gfkSolid: Canvas.Brush.Style := bsSolid;
    gfkGradient:
    begin
      Canvas.Brush.ClearGradient;

      if (AFill.ColorMirror <> gcNull) and (AFill.ColorMirrorTo <> gcNull) then
      begin
        Canvas.Brush.AddGradientColor(AFill.Color, 0);
        Canvas.Brush.AddGradientColor(AFill.ColorTo, 0.5);
        Canvas.Brush.AddGradientColor(AFill.ColorMirror, 0.5);
        Canvas.Brush.AddGradientColor(AFill.ColorMirrorTo, 1);
      end
      else
      begin
        Canvas.Brush.AddGradientColor(AFill.Color, 0);
        Canvas.Brush.AddGradientColor(AFill.ColorTo, 1);
      end;

      case AFill.Orientation of
        gfoHorizontal: Canvas.Brush.Gradient := bgLinearHorz;
        gfoVertical: Canvas.Brush.Gradient := bgLinearVert;
      end;
    end;
    {$ELSE}
    gfkSolid, gfkGradient: Canvas.Brush.Style := bsSolid;
    {$ENDIF}
    gfkNone: Canvas.Brush.Style := bsClear;
  end;
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.SetFillColor(AColor: TAdvGraphicsColor);
begin
  {$IFDEF FMXLIB}
  Canvas.Fill.Color := AColor;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  Canvas.Brush.Color := AColor;
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.SetFillKind(AKind: TAdvGraphicsFillKind);
begin
  {$IFDEF FMXLIB}
  Canvas.Fill.Kind := TBrushKind(Integer(AKind));
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  case AKind of
    gfkNone: Canvas.Brush.Style := bsClear;
    gfkSolid, gfkGradient: Canvas.Brush.Style := bsSolid;
  end;
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.SetFont(AFont: TAdvGraphicsFont);
begin
  Canvas.Font.Assign(AFont);
end;

procedure TAdvGraphicsContextGeneral.SetFontColor(AColor: TAdvGraphicsColor);
begin
  {$IFDEF FMXLIB}
  Canvas.Fill.Color := AColor;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  Canvas.Font.Color := AColor;
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.SetFontName(AName: string);
begin
  {$IFDEF FMXLIB}
  Canvas.Font.Name := AName;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  Canvas.Font.Name := AName;
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.SetFontSize(ASize: Integer);
begin
  Canvas.Font.Size := ASize;
end;

procedure TAdvGraphicsContextGeneral.SetFontStyles(AStyle: TFontStyles);
begin
  Canvas.Font.Style := AStyle;
end;

procedure TAdvGraphicsContextGeneral.SetScale(AScale: Single);
begin
end;

procedure TAdvGraphicsContextGeneral.SetMatrix(
  AMatrix: TAdvGraphicsMatrix);
{$IFDEF FMXLIB}
var
  M: TMatrix;
{$ENDIF}
begin
  {$IFDEF FMXLIB}
  M := TMatrix.Identity;
  M.m11 := AMatrix.m11;
  M.m12 := AMatrix.m12;
  M.m13 := AMatrix.m13;
  M.m21 := AMatrix.m21;
  M.m22 := AMatrix.m22;
  M.m23 := AMatrix.m23;
  M.m31 := AMatrix.m31;
  M.m32 := AMatrix.m32;
  M.m33 := AMatrix.m33;
  Canvas.SetMatrix(M);
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.SetStroke(AStroke: TAdvGraphicsStroke);
begin
  {$IFDEF FMXLIB}
  Canvas.Stroke.Color := AStroke.Color;
  Canvas.Stroke.Thickness := AStroke.Width;
  case AStroke.Kind of
    gskSolid:
    begin
      Canvas.Stroke.Kind := TBrushKind.Solid;
      Canvas.Stroke.Dash := TStrokeDash.Solid;
    end;
    gskNone: Canvas.Stroke.Kind := TBrushKind.None;
    else
      Canvas.Stroke.Kind := TBrushKind.Solid;
  end;

  case AStroke.Kind of
    gskDash: Canvas.Stroke.Dash := TStrokeDash.Dash;
    gskDot: Canvas.Stroke.Dash := TStrokeDash.Dot;
    gskDashDot: Canvas.Stroke.Dash := TStrokeDash.DashDot;
    gskDashDotDot: Canvas.Stroke.Dash := TStrokeDash.DashDotDot;
  end;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  Canvas.Pen.Color := AStroke.Color;
  Canvas.Pen.Width := Round(AStroke.Width);
  case AStroke.Kind of
    gskSolid: Canvas.Pen.Style := psSolid;
    gskNone: Canvas.Pen.Style := psClear;
    gskDash: Canvas.Pen.Style := psDash;
    gskDot: Canvas.Pen.Style := psDot;
    gskDashDot: Canvas.Pen.Style := psDashDot;
    gskDashDotDot: Canvas.Pen.Style := psDashDotDot;
  end;
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.SetStrokeColor(AColor: TAdvGraphicsColor);
begin
  {$IFDEF FMXLIB}
  Canvas.Stroke.Color := AColor;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  Canvas.Pen.Color := AColor;
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.SetStrokeKind(
  AKind: TAdvGraphicsStrokeKind);
begin
  {$IFDEF FMXLIB}
  case AKind of
    gskNone: Canvas.Stroke.Kind := TBrushKind.None;
    gskSolid:
    begin
      {$HINTS OFF}
      {$IF COMPILERVERSION > 31}
      Canvas.Stroke.Dash := TStrokeDash.Solid;
      {$ELSE}
      Canvas.StrokeDash := TStrokeDash.Solid;
      {$IFEND}
      {$HINTS ON}
      Canvas.Stroke.Kind := TBrushKind.Solid;
    end;
    gskDash:
    begin
      {$HINTS OFF}
      {$IF COMPILERVERSION > 31}
      Canvas.Stroke.Dash := TStrokeDash.Dash;
      {$ELSE}
      Canvas.StrokeDash := TStrokeDash.Dash;
      {$IFEND}
      {$HINTS ON}
      Canvas.Stroke.Kind := TBrushKind.Solid;
    end;
    gskDot:
    begin
      {$HINTS OFF}
      {$IF COMPILERVERSION > 31}
      Canvas.Stroke.Dash := TStrokeDash.Dot;
      {$ELSE}
      Canvas.StrokeDash := TStrokeDash.Dot;
      {$IFEND}
      {$HINTS ON}
      Canvas.Stroke.Kind := TBrushKind.Solid;
    end;
    gskDashDot:
    begin
      {$HINTS OFF}
      {$IF COMPILERVERSION > 31}
      Canvas.Stroke.Dash := TStrokeDash.DashDot;
      {$ELSE}
      Canvas.StrokeDash := TStrokeDash.DashDot;
      {$IFEND}
      {$HINTS ON}
      Canvas.Stroke.Kind := TBrushKind.Solid;
    end;
    gskDashDotDot:
    begin
      {$HINTS OFF}
      {$IF COMPILERVERSION > 31}
      Canvas.Stroke.Dash := TStrokeDash.DashDotDot;
      {$ELSE}
      Canvas.StrokeDash := TStrokeDash.DashDotDot;
      {$IFEND}
      {$HINTS ON}
      Canvas.Stroke.Kind := TBrushKind.Solid;
    end;
  end;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  case AKind of
    gskNone: Canvas.Pen.Style := psClear;
    gskSolid: Canvas.Pen.Style := psSolid;
    gskDash: Canvas.Pen.Style := psDash;
    gskDot: Canvas.Pen.Style := psDot;
    gskDashDot: Canvas.Pen.Style := psDashDot;
    gskDashDotDot: Canvas.Pen.Style := psDashDotDot;
  end;
  {$ENDIF}
end;

procedure TAdvGraphicsContextGeneral.SetStrokeWidth(AWidth: Single);
begin
  {$IFDEF FMXLIB}
  Canvas.Stroke.Thickness := AWidth;
  {$ENDIF}
  {$IFDEF CMNWEBLIB}
  Canvas.Pen.Width := Round(AWidth);
  {$ENDIF}
end;

function GetNativeContextClass: TAdvGraphicsContextClass;
begin
  Result := TAdvGraphicsContextGeneral;
end;

end.
