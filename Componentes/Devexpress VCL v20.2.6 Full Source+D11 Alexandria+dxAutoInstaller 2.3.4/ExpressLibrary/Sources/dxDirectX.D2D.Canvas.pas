{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ Express Cross Platform Library controls }
{ }
{ Copyright (c) 2000-2021 Developer Express Inc. }
{ ALL RIGHTS RESERVED }
{ }
{ The entire contents of this file is protected by U.S. and }
{ International Copyright Laws. Unauthorized reproduction, }
{ reverse-engineering, and distribution of all or any portion of }
{ the code contained in this file is strictly prohibited and may }
{ result in severe civil and criminal penalties and will be }
{ prosecuted to the maximum extent possible under the law. }
{ }
{ RESTRICTIONS }
{ }
{ THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES }
{ (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE }
{ SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS }
{ LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL }
{ ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM }
{ ONLY. }
{ }
{ THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED }
{ FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE }
{ COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE }
{ AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT }
{ AND PERMISSION FROM DEVELOPER EXPRESS INC. }
{ }
{ CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON }
{ ADDITIONAL RESTRICTIONS. }
{ }
{ ******************************************************************** }

unit dxDirectX.D2D.Canvas;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  UITypes,
{$ENDIF}
  Types, Windows, SysUtils, Generics.Collections, Generics.Defaults, Classes,
  Graphics, D2D1, DXGiFormat, Controls, Contnrs,
  Math, cxCustomCanvas, cxGeometry, cxGraphics, dxCore, dxCoreClasses,
  dxCoreGraphics, dxGDIPlusClasses, dxGDIPlusAPI,
  dxDirectX.D2D.Types, dxDirectX.D2D.Classes, dxDirectX.D2D.Utils;

type
  TdxCustomDirect2DCanvas = class;

  { TdxDWriteTextMetrics }

  TdxDWriteTextMetrics = record
    Height: Single;
    LineCount: Integer;
    LineHeight: Single;
    Width: Single;
    WidthIncludingTrailingWhitespace: Single;
  end;

  { TdxDirect2DBasedFont }

  TdxDirect2DBasedFont = class(TcxCanvasBasedFont)
  strict private
    FEndEllipsisSign: IDWriteInlineObject;
    FNativeHandle: IDWriteTextFormat;
    FStyle: TFontStyles;

    function GetEndEllipsisSign: IDWriteInlineObject; inline;
  public
    constructor Create(AFont: TFont);
    function Equals(Obj: TObject): Boolean; override;

    property EndEllipsisSign: IDWriteInlineObject read GetEndEllipsisSign;
    property NativeHandle: IDWriteTextFormat read FNativeHandle;
    property Style: TFontStyles read FStyle;
  end; // for internal use only

  { TdxDirect2DBasedImage }

  TdxDirect2DBasedImage = class(TcxCanvasBasedImage)
  strict private
    FHandle: ID2D1Bitmap;
  protected
    procedure Release;
  public
    constructor Create(ACanvas: TdxCustomDirect2DCanvas; AHandle: ID2D1Bitmap;
      AWidth, AHeight: Integer);
    destructor Destroy; override;
    procedure Draw(const ATargetRect, ASourceRect: TRect;
      AAlpha: Byte = MaxByte); override;
  end; // for internal use only

  { TdxDirect2DBasedTextLayout }

  TdxDirect2DBasedTextLayout = class(TcxCanvasBasedTextLayout)
  strict private
  const
    MaxSize = MaxWord;
  strict private
    FColor: TdxAlphaColor;
    FFont: TdxDirect2DBasedFont;
    FFontOwnership: TdxObjectOwnership;
    FHandle: IDWriteTextLayout;
    FHasEndEllipsis: Boolean;
    FMinWidth: Single;

    procedure CalculateTextMetrics(out AMetrics: TdxDWriteTextMetrics);
    procedure HandleNeeded; inline;
    function GetMinWidth: Single; inline;
    procedure ReleaseFont; inline;
  protected
    procedure ApplyFlags; override;
    procedure DoCalculateLayout; override;
    procedure DoDraw(const R: TRect); override;
    procedure TextChanged; override;
  public
    constructor Create(ACanvas: TdxCustomDirect2DCanvas);
    destructor Destroy; override;
    procedure SetColor(ATextColor: TColor); override;
    procedure SetFont(AFont: TcxCanvasBasedFont;
      AOwnership: TdxObjectOwnership = ooReferenced); override;
  end; // for internal use only

  { TdxDirect2DResourceCache }

  TdxDirect2DResourceCache<X, Y> = class(TdxValueCacheManager<X, Y>)
  protected
    FOwner: TdxCustomDirect2DCanvas;
  public
    constructor Create(AOwner: TdxCustomDirect2DCanvas; ACapacity: Integer);
  end; // for internal use only

  { IdxDirect2DCanvasOwner }

  IdxDirect2DCanvasOwner = interface
    ['{08D21768-F2CA-4F1B-99D6-AE30A59216CB}']
    procedure RecreateNeeded;
  end; // for internal use only

  { TdxCustomDirect2DCanvas }

  TdxCustomDirect2DCanvas = class(TcxCustomCanvas, IcxCanvasCacheControl)
  strict private
  const
    InterpolationModeMap: array [TcxCanvasImageStretchQuality]
      of TD2D1InterpolationMode = (D2D1_INTERPOLATION_MODE_NEAREST_NEIGHBOR,
      D2D1_INTERPOLATION_MODE_ANISOTROPIC);
  strict private
    FClipRect: TRect;
    FClipRectModified: Boolean;
    FClipRects: TStack<TRect>;
    FDeviceContext: ID2D1DeviceContext;
    FMaxBitmapSize: Integer;
    FOrigin: TPoint;
    FWindowOrgs: TStack<TPoint>;
    FWorldTransforms: TStack<TD2D1Matrix3x2F>;

    procedure RollbackClipRectChanges;
    procedure SetDeviceContext(const Value: ID2D1DeviceContext);
  protected
    FCacheNativeObjects
      : TdxDirect2DResourceCache<TcxCanvasBasedResourceCacheKey, ID2D1Bitmap>;
    FCacheSolidBrushes: TdxDirect2DResourceCache<TdxAlphaColor,
      ID2D1SolidColorBrush>;
    FImages: TList;
    FIsGDICompatible: Boolean;
    FRecreateContextNeeded: Boolean;

    procedure DoBeginDraw(const AClipRect: TRect);
    procedure DoEndDraw;

    function GetWindowOrg: TPoint; override;
    procedure SetWindowOrg(const P: TPoint); override;

    function CacheGetPenStyle(const AStyle: TPenStyle): ID2D1StrokeStyle;
    function CacheGetSolidBrush(const AColor: TdxAlphaColor)
      : ID2D1SolidColorBrush;
    function CreatePathGeometry(const APoints: array of TPoint;
      AFigureBegin: TD2D1FigureBegin; AFigureEnd: TD2D1_FigureEnd)
      : ID2D1PathGeometry1;

    function CreateNativeObjectTexture(const ARect, AClippedRect: TRect;
      AProc: TcxCanvasNativeDrawProc): ID2D1Bitmap; overload;
    function CreateNativeObjectTexture(const ARect, AClippedRect: TRect;
      AProc: TcxCanvasNativeDrawExProc): ID2D1Bitmap; overload;
    function GetGdiInteropRenderTarget(out ATarget: ID2D1GdiInteropRenderTarget;
      out DC: HDC): Boolean;
    function IsLargeForTexture(const R: TRect): Boolean; inline;

    function InflateRectF(const R: TD2D1RectF; ADelta: Single): TD2D1RectF;
      overload; inline;
    function InflateRectF(const R: TD2D1RectF; ADeltaX, ADeltaY: Single)
      : TD2D1RectF; overload; inline;
    function TranslatePointF(const P: TPoint): TD2D1Point2F; overload; inline;
    function TranslatePointF(X, Y: Integer): TD2D1Point2F; overload; inline;
    function TranslateRect(const R: TRect): TRect; inline;
    function TranslateRectF(const R: TRect): TD2D1RectF; inline;

    procedure ReleaseDevice; virtual;
    procedure ReleaseResources;

    property DeviceContext: ID2D1DeviceContext read FDeviceContext
      write SetDeviceContext;
  public
    constructor Create;
    destructor Destroy; override;
    // IcxCanvasCacheControl
    procedure FlushCache;
    procedure DrawNativeObject(const R: TRect;
      const ACacheKey: TcxCanvasBasedResourceCacheKey;
      AProc: TcxCanvasNativeDrawProc); override;
    procedure DrawNativeObject(const R: TRect;
      const ACacheKey: TcxCanvasBasedResourceCacheKey;
      AProc: TcxCanvasNativeDrawExProc); override;
    function CreateFonT(AFont: TFont): TcxCanvasBasedFont; override;
    function CreateImage(ABitmap: TBitmap;
      AAlphaFormat: TAlphaFormat = afIgnored): TcxCanvasBasedImage; override;
    function CreateImage(ABitmap: TdxCustomFastDIB;
      AAlphaFormat: TAlphaFormat = afIgnored): TcxCanvasBasedImage; override;
    function CreateTextLayout: TcxCanvasBasedTextLayout; override;
    procedure Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer; AColor: TColor;
      APenWidth: Integer = 1); override;
    procedure FillPolygon(const P: array of TPoint; AColor: TColor); override;
    procedure FillRect(const R: TRect; AColor: TColor); override;
    procedure FillRect(const R: TRect; AColor: TdxAlphaColor); override;
    procedure FillRectByGradient(const ARect: TRect;
      AColor1, AColor2: TdxAlphaColor; AMode: TdxGpLinearGradientMode);
      override;
    procedure FocusRectangle(const R: TRect); override;
    procedure Polygon(const P: array of TPoint;
      ABrushColor, APenColor: TColor); override;
    procedure Polyline(const P: array of TPoint; AColor: TColor;
      APenWidth: Integer = 1; APenStyle: TPenStyle = psSolid); override;
    procedure Rectangle(const R: TRect; ABrushColor, APenColor: TColor;
      APenStyle: TPenStyle; APenWidth: Integer = 1); override;
    procedure DrawBitmap(const AHandle: ID2D1Bitmap;
      const ATargetRect, ASourceRect: TRect; AAlpha: Byte);
      reintroduce; overload;
    procedure DrawBitmap(const AHandle: ID2D1Bitmap; const ATargetRect: TRect);
      reintroduce; overload;
    procedure DrawTextLayout(const AHandle: IDWriteTextLayout; X, Y: Integer;
      AColor: TdxAlphaColor);
    procedure Geometry(const AHandle: ID2D1Geometry; ABrushColor: TColor;
      APenColor: TColor; APenWidth: Single = 1;
      APenStyle: ID2D1StrokeStyle = nil);
    procedure IntersectClipRect(const R: TRect); override;
    function RectVisible(const R: TRect): Boolean; override;
    procedure RestoreClipRegion; override;
    procedure SaveClipRegion; override;
    procedure ModifyWorldTransform(const AForm: TXForm); override;
    procedure RestoreWorldTransform; override;
    procedure SaveWorldTransform; override;
    procedure RestoreState; override;
    procedure SaveState; override;
  end;

  { TdxDirect2DGdiCompatibleCanvas }

  TdxDirect2DGdiCompatibleCanvas = class(TdxCustomDirect2DCanvas)
  strict private
    FRenderTarget: ID2D1DCRenderTarget;

    procedure CreateRenderTarget;
  public
    constructor Create;
    procedure BeginPaint(DC: HDC; const R: TRect);
    procedure EndPaint;
  end;

  { TdxDirect2DHwndBasedCanvas }

  TdxDirect2DHwndBasedCanvas = class(TdxCustomDirect2DCanvas,
    IcxControlDirectCanvas, IcxControlCanvas)
  strict private
    FDevice: IDXGIDevice1;
    FDevice3D: ID3D11Device;
    FDevice3DContext: ID3D11DeviceContext;
    FFrontBufferContent: ID3D11Texture2D;
    FFrontBufferContentSize: TSize;
    FFrontBufferSurface: IDXGISurface;
    FOwner: IdxDirect2DCanvasOwner;
    FPaintStruct: TPaintStruct;
    FPresentParameters: TDXGIPresentParameters;
    FSwapChain: IDXGISwapChain1;
    FTextureSize: TSize;
    FUpdateRect: TRect;
    FWinControl: TWinControl;
    FWindowHandle: HWND;

    procedure CheckCreateFrontBufferContent;
    procedure DoPresentBuffer;
  protected
    function GetDefaultUseRightToLeftAlignment: Boolean; override;
    // Texture
    procedure CreateTexture;
    procedure ReleaseDevice; override;
    procedure ReleaseTexture;
    // IcxControlCanvas
    procedure BeginPaint;
    procedure EndPaint;
    // IcxControlDirectCanvas
    procedure CopyToDC(DC: HDC); overload;
    procedure CopyToDC(DC: HDC; const ATargetRect, ASourceRect: TRect);
      overload;
    procedure SetWndHandle(AHandle: HWND);

    property Device: IDXGIDevice1 read FDevice;
    property Device3D: ID3D11Device read FDevice3D;
    property Device3DContext: ID3D11DeviceContext read FDevice3DContext;
  public
    constructor Create(const AOwner: IdxDirect2DCanvasOwner;
      const ADevice: IDXGIDevice1; const AContext: ID2D1DeviceContext;
      const ADevice3D: ID3D11Device;
      const ADevice3DContext: ID3D11DeviceContext);
  end;

  TdxDirectXSwapChainSize = 2 .. 16; // for internal use

var
  dxDirectXSwapChainSize: TdxDirectXSwapChainSize = 2; // for internal use

function dxCreateDirect2DCanvas(const AOwner: IdxDirect2DCanvasOwner;
  out ACanvas: TcxCustomCanvas): Boolean;
function dxCopyDirectCanvasContentToGdiCanvas(AActualCanvas: TcxCustomCanvas;
  AGdiCanvas: TcxGdiBasedCanvas): Boolean;

implementation

uses
  ActiveX, cxDrawTextUtils, dxFading, cxControls;

function dxCopyDirectCanvasContentToGdiCanvas(AActualCanvas: TcxCustomCanvas;
  AGdiCanvas: TcxGdiBasedCanvas): Boolean;
var
  ADirectCanvas: IcxControlDirectCanvas;
begin
  Result := (AActualCanvas <> AGdiCanvas) and
    Supports(AActualCanvas, IcxControlDirectCanvas, ADirectCanvas);
  if Result then
    ADirectCanvas.CopyToDC(AGdiCanvas.Handle);
end;

function dxCreateDevice3DContext(out ADevice: ID3D11Device;
  out ADeviceContext: ID3D11DeviceContext): Boolean;
const
  Windows8Features: array [0 .. 1] of TD3DFeatureLevel =
    (D3D_FEATURE_LEVEL_11_0, D3D_FEATURE_LEVEL_11_1);
var
  AErrorCode: HRESULT;
  AFeatureCount: Integer;
  AFeatures: PD3DFeatureLevel;
begin
  if IsWin8OrLater then
  begin
    AFeatures := @Windows8Features[0];
    AFeatureCount := Length(Windows8Features);
  end
  else
  begin
    AFeatures := nil;
    AFeatureCount := 0;
  end;
  AErrorCode := D3D11CreateDevice(nil, D3D_DRIVER_TYPE_HARDWARE, 0,
    D3D11_CREATE_DEVICE_BGRA_SUPPORT or D3D11_CREATE_DEVICE_SINGLETHREADED,
    AFeatures, AFeatureCount, D3D11_SDK_VERSION, ADevice, nil, ADeviceContext);
  CheckNeedSwitchToGdiRenderMode(AErrorCode);
  Result := AErrorCode = S_OK;
end;

function dxCreateDirect2DCanvas(const AOwner: IdxDirect2DCanvasOwner;
  out ACanvas: TcxCustomCanvas): Boolean;
var
  AContext: ID2D1DeviceContext;
  ADevice: IDXGIDevice1;
  ADevice2D: ID2D1Device;
  ADevice3D: ID3D11Device;
  ADevice3DContext: ID3D11DeviceContext;
begin
  if not IsDirectD2Available then
    Exit(False);
  if not dxCreateDevice3DContext(ADevice3D, ADevice3DContext) then
    Exit(False);
  if not Supports(ADevice3D, IDXGIDevice1, ADevice) then
    Exit(False);
  if Failed(D2D1Factory1.CreateDevice(ADevice, ADevice2D)) then
    Exit(False);
  if Failed(ADevice2D.CreateDeviceContext(D2D1_DEVICE_CONTEXT_OPTIONS_NONE,
    AContext)) then
    Exit(False);
  ACanvas := TdxDirect2DHwndBasedCanvas.Create(AOwner, ADevice, AContext,
    ADevice3D, ADevice3DContext);
  Result := True;
end;

{ TdxDirect2DResourceCache<X, Y> }

constructor TdxDirect2DResourceCache<X, Y>.Create
  (AOwner: TdxCustomDirect2DCanvas; ACapacity: Integer);
begin
  inherited Create(ACapacity);
  FOwner := AOwner;
end;

{ TdxCustomDirect2DCanvas }

constructor TdxCustomDirect2DCanvas.Create;
begin
  FIsGDICompatible := True;
  FClipRects := TStack<TRect>.Create;
  FWindowOrgs := TStack<TPoint>.Create;
  FWorldTransforms := TStack<TD2D1Matrix3x2F>.Create;
  FCacheSolidBrushes := TdxDirect2DResourceCache<TdxAlphaColor,
    ID2D1SolidColorBrush>.Create(Self, 128);
  FCacheNativeObjects :=
    TdxDirect2DResourceCache<TcxCanvasBasedResourceCacheKey, ID2D1Bitmap>.Create
    (Self, 256);
  FImages := TList.Create;
  FImages.Capacity := 1024;
end;

destructor TdxCustomDirect2DCanvas.Destroy;
begin
  ReleaseDevice;
  FreeAndNil(FCacheNativeObjects);
  FreeAndNil(FCacheSolidBrushes);
  FreeAndNil(FWorldTransforms);
  FreeAndNil(FWindowOrgs);
  FreeAndNil(FClipRects);
  FreeAndNil(FImages);
  inherited;
end;

procedure TdxCustomDirect2DCanvas.DrawNativeObject(const R: TRect;
  const ACacheKey: TcxCanvasBasedResourceCacheKey;
  AProc: TcxCanvasNativeDrawProc);
var
  AClippedRect: TRect;
  AGdiRenderTarget: ID2D1GdiInteropRenderTarget;
  AHandle: ID2D1Bitmap;
  ATargetDC: HDC;
begin
  if not RectVisible(R) then
    Exit;

  if GetGdiInteropRenderTarget(AGdiRenderTarget, ATargetDC) then
    try
      cxPaintCanvas.BeginPaint(ATargetDC);
      try
        cxPaintCanvas.WindowOrg := FOrigin;
        AProc(cxPaintCanvas, R);
      finally
        cxPaintCanvas.EndPaint;
      end;
    finally
      AGdiRenderTarget.ReleaseDC(TranslateRect(R));
    end
  else if IsLargeForTexture(R) then
  begin
    if cxRectIntersect(AClippedRect, FClipRect, R) then
      DrawBitmap(CreateNativeObjectTexture(R, AClippedRect, AProc),
        AClippedRect);
  end
  else
  begin
    if not FCacheNativeObjects.Get(ACacheKey, AHandle) then
    begin
      AHandle := CreateNativeObjectTexture(R, R, AProc);
      FCacheNativeObjects.Add(ACacheKey, AHandle);
    end;
    DrawBitmap(AHandle, R);
  end;
end;

procedure TdxCustomDirect2DCanvas.DrawNativeObject(const R: TRect;
  const ACacheKey: TcxCanvasBasedResourceCacheKey;
  AProc: TcxCanvasNativeDrawExProc);
var
  AClippedRect: TRect;
  AGdiRenderTarget: ID2D1GdiInteropRenderTarget;
  AHandle: ID2D1Bitmap;
  ATargetDC: HDC;
begin
  if not RectVisible(R) then
    Exit;

  if GetGdiInteropRenderTarget(AGdiRenderTarget, ATargetDC) then
    try
      dxGPPaintCanvas.BeginPaint(ATargetDC, R);
      try
        dxGPPaintCanvas.TranslateWorldTransform(FOrigin.X, FOrigin.Y);
        AProc(dxGPPaintCanvas, R);
      finally
        dxGPPaintCanvas.EndPaint;
      end;
    finally
      AGdiRenderTarget.ReleaseDC(TranslateRect(R));
    end
  else if IsLargeForTexture(R) then
  begin
    if cxRectIntersect(AClippedRect, FClipRect, R) then
      DrawBitmap(CreateNativeObjectTexture(R, AClippedRect, AProc),
        AClippedRect);
  end
  else
  begin
    if not FCacheNativeObjects.Get(ACacheKey, AHandle) then
    begin
      AHandle := CreateNativeObjectTexture(R, R, AProc);
      FCacheNativeObjects.Add(ACacheKey, AHandle);
    end;
    DrawBitmap(AHandle, R);
  end;
end;

procedure TdxCustomDirect2DCanvas.DrawTextLayout(const AHandle
  : IDWriteTextLayout; X, Y: Integer; AColor: TdxAlphaColor);
var
  ATextPoint: TD2D1Point2F;
begin
  ATextPoint := TranslatePointF(X, Y);
  ATextPoint.X := ATextPoint.X - 0.5;
  ATextPoint.Y := ATextPoint.Y - 0.5;
  DeviceContext.DrawTextLayout(ATextPoint, AHandle, CacheGetSolidBrush(AColor),
    D2D1_DRAW_TEXT_OPTIONS_CLIP);
end;

procedure TdxCustomDirect2DCanvas.Geometry(const AHandle: ID2D1Geometry;
  ABrushColor: TColor; APenColor: TColor; APenWidth: Single = 1;
  APenStyle: ID2D1StrokeStyle = nil);
begin
  if AHandle = nil then
    Exit;
  if cxColorIsValid(ABrushColor) then
    DeviceContext.FillGeometry(AHandle,
      CacheGetSolidBrush(dxColorToAlphaColor(ABrushColor)));
  if cxColorIsValid(APenColor) then
    DeviceContext.DrawGeometry(AHandle,
      CacheGetSolidBrush(dxColorToAlphaColor(APenColor)), APenWidth, APenStyle);
end;

function TdxCustomDirect2DCanvas.CreateFonT(AFont: TFont): TcxCanvasBasedFont;
begin
  Result := TdxDirect2DBasedFont.Create(AFont);
end;

function TdxCustomDirect2DCanvas.CreateImage(ABitmap: TdxCustomFastDIB;
  AAlphaFormat: TAlphaFormat): TcxCanvasBasedImage;
begin
  Result := TdxDirect2DBasedImage.Create(Self, D2D1Bitmap(DeviceContext,
    ABitmap, AAlphaFormat), ABitmap.Width, ABitmap.Height);
end;

function TdxCustomDirect2DCanvas.CreateImage(ABitmap: TBitmap;
  AAlphaFormat: TAlphaFormat): TcxCanvasBasedImage;
begin
  Result := TdxDirect2DBasedImage.Create(Self, D2D1Bitmap(DeviceContext,
    ABitmap, AAlphaFormat), ABitmap.Width, ABitmap.Height);
end;

function TdxCustomDirect2DCanvas.CreateTextLayout: TcxCanvasBasedTextLayout;
begin
  Result := TdxDirect2DBasedTextLayout.Create(Self);
end;

procedure TdxCustomDirect2DCanvas.Arc(X1, Y1, X2, Y2, X3, Y3, X4, Y4: Integer;
  AColor: TColor; APenWidth: Integer = 1);
var
  A, B, C: TD2D1Point2F;
  AArcSegment: TD2D1ArcSegment;
  AGeometry: ID2D1PathGeometry1;
  ASink: ID2D1GeometrySink;
begin
  if Succeeded(D2D1Factory1.CreatePathGeometry(AGeometry)) then
  begin
    AArcSegment := D2D1PointsToArcSegment(TranslatePointF(X1, Y1),
      TranslatePointF(X2, Y2), TranslatePointF(X3, Y3),
      TranslatePointF(X4, Y4), C, A, B);

    AGeometry.Open(ASink);
    try
      ASink.BeginFigure(A, D2D1_FIGURE_BEGIN_FILLED);
      try
        ASink.AddArc(AArcSegment);
      finally
        ASink.EndFigure(D2D1_FIGURE_END_OPEN);
      end;
    finally
      ASink.Close;
    end;
    Geometry(AGeometry, clNone, AColor, APenWidth);
  end;
end;

procedure TdxCustomDirect2DCanvas.FillPolygon(const P: array of TPoint;
  AColor: TColor);
begin
  Polygon(P, AColor, clNone);
end;

procedure TdxCustomDirect2DCanvas.FillRect(const R: TRect; AColor: TColor);
begin
  FillRect(R, dxColorToAlphaColor(AColor));
end;

procedure TdxCustomDirect2DCanvas.FillRect(const R: TRect;
  AColor: TdxAlphaColor);
begin
  if dxAlphaColorIsValid(AColor) then
    DeviceContext.FillRectangle(TranslateRectF(R), CacheGetSolidBrush(AColor));
end;

procedure TdxCustomDirect2DCanvas.FillRectByGradient(const ARect: TRect;
  AColor1, AColor2: TdxAlphaColor; AMode: TdxGpLinearGradientMode);
var
  AGradientBrush: ID2D1LinearGradientBrush;
  AGradientBrushProperties: TD2D1LinearGradientBrushProperties;
  AGradientStop: array [0 .. 1] of TD2D1GradientStop;
  AGradientStopCollection: ID2D1GradientStopCollection;
  ATargetRect: TD2D1RectF;
begin
  if (AColor1 = AColor2) or (AColor2 = dxacDefault) then
  begin
    FillRect(ARect, AColor1);
    Exit;
  end;

  if not(dxAlphaColorIsValid(AColor1) or dxAlphaColorIsValid(AColor2)) then
    Exit;

  ATargetRect := TranslateRectF(ARect);

  AGradientBrushProperties.startPoint := D2D1PointF(ATargetRect.left,
    ATargetRect.top);
  case AMode of
    LinearGradientModeVertical:
      AGradientBrushProperties.endPoint := D2D1PointF(ATargetRect.left,
        ATargetRect.bottom);
    LinearGradientModeForwardDiagonal:
      AGradientBrushProperties.endPoint := D2D1PointF(ATargetRect.right,
        ATargetRect.bottom);
    LinearGradientModeBackwardDiagonal:
      begin
        AGradientBrushProperties.startPoint := D2D1PointF(ATargetRect.right,
          ATargetRect.bottom);
        AGradientBrushProperties.endPoint := D2D1PointF(ATargetRect.left,
          ATargetRect.top);
      end;
  else
    AGradientBrushProperties.endPoint := D2D1PointF(ATargetRect.right,
      ATargetRect.top);
  end;

  AGradientStop[0].position := 0;
  AGradientStop[0].color := D2D1ColorF(AColor1);
  AGradientStop[1].position := 1;
  AGradientStop[1].color := D2D1ColorF(AColor2);

  CheckD2D1Result(DeviceContext.CreateGradientStopCollection(@AGradientStop[0],
    Length(AGradientStop), D2D1_GAMMA_2_2, D2D1_EXTEND_MODE_CLAMP,
    AGradientStopCollection));
  CheckD2D1Result(DeviceContext.CreateLinearGradientBrush
    (AGradientBrushProperties, nil, AGradientStopCollection, AGradientBrush));
  DeviceContext.FillRectangle(ATargetRect, AGradientBrush);
end;

procedure TdxCustomDirect2DCanvas.FocusRectangle(const R: TRect);
begin
  Rectangle(R, clNone, clBlack, psDash);
end;

procedure TdxCustomDirect2DCanvas.Rectangle(const R: TRect; ABrushColor: TColor;
  APenColor: TColor; APenStyle: TPenStyle; APenWidth: Integer = 1);
var
  ARect: TD2DRectF;
begin
  ARect := InflateRectF(TranslateRectF(R), -0.5);
  DeviceContext.FillRectangle(ARect,
    CacheGetSolidBrush(dxColorToAlphaColor(ABrushColor)));
  if cxColorIsValid(APenColor) and (APenWidth > 0) then
    DeviceContext.DrawRectangle(ARect,
      CacheGetSolidBrush(dxColorToAlphaColor(APenColor)), APenWidth,
      CacheGetPenStyle(APenStyle));
end;

procedure TdxCustomDirect2DCanvas.Polygon(const P: array of TPoint;
  ABrushColor, APenColor: TColor);
begin
  if cxColorIsValid(ABrushColor) or cxColorIsValid(APenColor) then
    Geometry(CreatePathGeometry(P, D2D1_FIGURE_BEGIN_FILLED,
      D2D1_FIGURE_END_CLOSED), ABrushColor, APenColor);
end;

procedure TdxCustomDirect2DCanvas.Polyline(const P: array of TPoint;
  AColor: TColor; APenWidth: Integer = 1; APenStyle: TPenStyle = psSolid);
begin
  if cxColorIsValid(AColor) then
    Geometry(CreatePathGeometry(P, D2D1_FIGURE_BEGIN_HOLLOW,
      D2D1_FIGURE_END_OPEN), clNone, AColor, APenWidth,
      CacheGetPenStyle(APenStyle));
end;

procedure TdxCustomDirect2DCanvas.DrawBitmap(const AHandle: ID2D1Bitmap;
  const ATargetRect: TRect);
var
  ATarget: TD2D1RectF;
begin
  ATarget := TranslateRectF(ATargetRect);
  DeviceContext.DrawBitmap(AHandle, @ATarget, 1,
    InterpolationModeMap[ImageStretchQuality]);
end;

procedure TdxCustomDirect2DCanvas.DrawBitmap(const AHandle: ID2D1Bitmap;
  const ATargetRect, ASourceRect: TRect; AAlpha: Byte);
var
  ASource: TD2D1RectF;
  ATarget: TD2D1RectF;
begin
  ASource := D2D1Rect(ASourceRect);
  ATarget := TranslateRectF(ATargetRect);
  DeviceContext.DrawBitmap(AHandle, @ATarget, AAlpha / MaxByte,
    InterpolationModeMap[ImageStretchQuality], @ASource);
end;

procedure TdxCustomDirect2DCanvas.IntersectClipRect(const R: TRect);
begin
  RollbackClipRectChanges;
  cxRectIntersect(FClipRect, TranslateRect(R), FClipRect);
  DeviceContext.PushAxisAlignedClip(D2D1Rect(FClipRect),
    D2D1_ANTIALIAS_MODE_ALIASED);
  FClipRectModified := True;
end;

function TdxCustomDirect2DCanvas.RectVisible(const R: TRect): Boolean;
begin
  Result := cxRectIntersect(R, FClipRect);
end;

procedure TdxCustomDirect2DCanvas.RestoreClipRegion;
begin
  RollbackClipRectChanges;
  FClipRect := FClipRects.Pop;
  DeviceContext.PopAxisAlignedClip;
  IntersectClipRect(FClipRect); // to apply restored ClipRect
end;

procedure TdxCustomDirect2DCanvas.SaveClipRegion;
begin
  RollbackClipRectChanges;
  FClipRects.Push(FClipRect);
  DeviceContext.PushAxisAlignedClip(D2D1Rect(FClipRect),
    D2D1_ANTIALIAS_MODE_ALIASED);
end;

procedure TdxCustomDirect2DCanvas.ModifyWorldTransform(const AForm: TXForm);
var
  ATransform: TD2D1Matrix3x2F;
begin
  DeviceContext.GetTransform(ATransform);
  ATransform := D2D1Matrix3x2(AForm) * ATransform;
  DeviceContext.SetTransform(ATransform);
end;

procedure TdxCustomDirect2DCanvas.RestoreWorldTransform;
begin
  DeviceContext.SetTransform(FWorldTransforms.Pop);
end;

procedure TdxCustomDirect2DCanvas.SaveWorldTransform;
var
  ATransform: TD2D1Matrix3x2F;
begin
  DeviceContext.GetTransform(ATransform);
  FWorldTransforms.Push(ATransform);
end;

procedure TdxCustomDirect2DCanvas.RestoreState;
begin
  WindowOrg := FWindowOrgs.Pop;
  RestoreWorldTransform;
  RestoreClipRegion;
end;

procedure TdxCustomDirect2DCanvas.SaveState;
begin
  FWindowOrgs.Push(WindowOrg);
  SaveWorldTransform;
  SaveClipRegion;
end;

procedure TdxCustomDirect2DCanvas.FlushCache;
begin
  FCacheNativeObjects.Clear;
  FCacheSolidBrushes.Clear;
end;

procedure TdxCustomDirect2DCanvas.ReleaseDevice;
begin
  ReleaseResources;
  DeviceContext := nil;
end;

procedure TdxCustomDirect2DCanvas.ReleaseResources;
var
  I: Integer;
begin
  for I := FImages.Count - 1 downto 0 do
    TdxDirect2DBasedImage(FImages.List[I]).Release;
  FImages.Count := 0;
  FlushCache;
end;

procedure TdxCustomDirect2DCanvas.DoBeginDraw(const AClipRect: TRect);
begin
  FClipRect := AClipRect;
  DeviceContext.BeginDraw;
  IntersectClipRect(FClipRect);
end;

procedure TdxCustomDirect2DCanvas.DoEndDraw;
begin
  RollbackClipRectChanges;
  case DeviceContext.EndDraw of
    DXGI_ERROR_DEVICE_REMOVED, DXGI_ERROR_DEVICE_RESET, D2DERR_RECREATE_TARGET:
      FRecreateContextNeeded := True;
  end;
end;

function TdxCustomDirect2DCanvas.GetWindowOrg: TPoint;
begin
  Result := FOrigin;
end;

procedure TdxCustomDirect2DCanvas.SetDeviceContext
  (const Value: ID2D1DeviceContext);
begin
  FDeviceContext := Value;
  if DeviceContext <> nil then
    FMaxBitmapSize := DeviceContext.GetMaximumBitmapSize
  else
    FMaxBitmapSize := 0;
end;

procedure TdxCustomDirect2DCanvas.SetWindowOrg(const P: TPoint);
begin
  FOrigin := P;
end;

function TdxCustomDirect2DCanvas.CacheGetPenStyle(const AStyle: TPenStyle)
  : ID2D1StrokeStyle;
begin
  Result := TdxDirect2DPenStyleCache.Get(AStyle);
end;

function TdxCustomDirect2DCanvas.CacheGetSolidBrush(const AColor: TdxAlphaColor)
  : ID2D1SolidColorBrush;
begin
  if not FCacheSolidBrushes.Get(AColor, Result) then
  begin
    DeviceContext.CreateSolidColorBrush(D2D1ColorF(AColor), nil, Result);
    FCacheSolidBrushes.Add(AColor, Result);
  end;
end;

function TdxCustomDirect2DCanvas.CreatePathGeometry(const APoints
  : array of TPoint; AFigureBegin: TD2D1FigureBegin;
  AFigureEnd: TD2D1_FigureEnd): ID2D1PathGeometry1;
var
  ASink: ID2D1GeometrySink;
  I: Integer;
begin
  if Length(APoints) = 0 then
    Exit(nil);
  if Failed(D2D1Factory1.CreatePathGeometry(Result)) then
    Exit(nil);
  if Succeeded(Result.Open(ASink)) then
    try
      ASink.BeginFigure(TranslatePointF(APoints[0]), AFigureBegin);
      for I := 1 to Length(APoints) - 1 do
        ASink.AddLine(TranslatePointF(APoints[I]));
      ASink.EndFigure(AFigureEnd);
    finally
      ASink.Close;
    end;
end;

function TdxCustomDirect2DCanvas.CreateNativeObjectTexture(const ARect,
  AClippedRect: TRect; AProc: TcxCanvasNativeDrawProc): ID2D1Bitmap;
var
  ABitmap: TcxBitmap32;
begin
  ABitmap := TcxBitmap32.CreateSize(AClippedRect, True);
  try
    ABitmap.cxCanvas.WindowOrg := AClippedRect.TopLeft;
    AProc(ABitmap.cxCanvas, ARect);
    Result := D2D1Bitmap(DeviceContext, ABitmap, afPremultiplied);
  finally
    ABitmap.Free;
  end;
end;

function TdxCustomDirect2DCanvas.CreateNativeObjectTexture(const ARect,
  AClippedRect: TRect; AProc: TcxCanvasNativeDrawExProc): ID2D1Bitmap;
var
  ABitmap: GpBitmap;
  ABitmapCanvas: GpGraphics;
  AData: TBitmapData;
begin
  ABitmap := dxGpCreateBitmap(AClippedRect);
  try
    GdipCheck(GdipGetImageGraphicsContext(ABitmap, ABitmapCanvas));
    try
      dxGPPaintCanvas.BeginPaint(ABitmapCanvas);
      try
        dxGPPaintCanvas.TranslateWorldTransform(-AClippedRect.left,
          -AClippedRect.top);
        AProc(dxGPPaintCanvas, ARect);
      finally
        dxGPPaintCanvas.EndPaint;
      end;
    finally
      GdipCheck(GdipDeleteGraphics(ABitmapCanvas));
    end;

    GdipCheck(GdipBitmapLockBits(ABitmap, nil, ImageLockModeRead,
      PixelFormat32bppPARGB, @AData));
    try
      Result := D2D1Bitmap(DeviceContext, AData.Scan0, AData.Width,
        AData.Height, AData.Stride, afPremultiplied);
    finally
      GdipCheck(GdipBitmapUnlockBits(ABitmap, @AData));
    end;
  finally
    GdipCheck(GdipDisposeImage(ABitmap));
  end;
end;

function TdxCustomDirect2DCanvas.GetGdiInteropRenderTarget
  (out ATarget: ID2D1GdiInteropRenderTarget; out DC: HDC): Boolean;
begin
  if FIsGDICompatible and Supports(DeviceContext, ID2D1GdiInteropRenderTarget,
    ATarget) then
    FIsGDICompatible :=
      Succeeded(ATarget.GetDC(D2D1_DC_INITIALIZE_MODE_COPY, DC));
  Result := FIsGDICompatible;
end;

function TdxCustomDirect2DCanvas.IsLargeForTexture(const R: TRect): Boolean;
begin
  Result := Max(cxRectWidth(R), cxRectHeight(R)) > FMaxBitmapSize;
end;

function TdxCustomDirect2DCanvas.TranslatePointF(X, Y: Integer): TD2D1Point2F;
begin
  Result.X := X - FOrigin.X;
  Result.Y := Y - FOrigin.Y;
end;

function TdxCustomDirect2DCanvas.InflateRectF(const R: TD2D1RectF;
  ADelta: Single): TD2D1RectF;
begin
  Result := InflateRectF(R, ADelta, ADelta);
end;

function TdxCustomDirect2DCanvas.InflateRectF(const R: TD2D1RectF;
  ADeltaX, ADeltaY: Single): TD2D1RectF;
begin
  Result.bottom := R.bottom - ADeltaY;
  Result.left := R.left + ADeltaX;
  Result.right := R.right - ADeltaX;
  Result.top := R.top + ADeltaY;
end;

function TdxCustomDirect2DCanvas.TranslatePointF(const P: TPoint): TD2D1Point2F;
begin
  Result := TranslatePointF(P.X, P.Y);
end;

function TdxCustomDirect2DCanvas.TranslateRect(const R: TRect): TRect;
begin
  Result := cxRectOffset(R, FOrigin, False);
end;

function TdxCustomDirect2DCanvas.TranslateRectF(const R: TRect): TD2D1RectF;
begin
  Result := D2D1Rect(TranslateRect(R));
end;

procedure TdxCustomDirect2DCanvas.RollbackClipRectChanges;
begin
  if FClipRectModified then
  begin
    DeviceContext.PopAxisAlignedClip;
    FClipRectModified := False;
  end;
end;

{ TdxDirect2DBasedFont }

constructor TdxDirect2DBasedFont.Create(AFont: TFont);
begin
  FNativeHandle := TdxDirect2DFontCache.Get(AFont);
  FStyle := AFont.Style;
end;

function TdxDirect2DBasedFont.Equals(Obj: TObject): Boolean;
begin
  Result := (ClassType = Obj.ClassType) and
    (NativeHandle = TdxDirect2DBasedFont(Obj).NativeHandle) and
    (Style = TdxDirect2DBasedFont(Obj).Style);
end;

function TdxDirect2DBasedFont.GetEndEllipsisSign: IDWriteInlineObject;
begin
  if FEndEllipsisSign = nil then
    DWriteFactory.CreateEllipsisTrimmingSign(NativeHandle, FEndEllipsisSign);
  Result := FEndEllipsisSign;
end;

{ TdxDirect2DBasedImage }

constructor TdxDirect2DBasedImage.Create(ACanvas: TdxCustomDirect2DCanvas;
  AHandle: ID2D1Bitmap; AWidth, AHeight: Integer);
begin
  inherited Create(ACanvas, AWidth, AHeight);
  FHandle := AHandle;
  TdxCustomDirect2DCanvas(Canvas).FImages.Add(Self);
end;

destructor TdxDirect2DBasedImage.Destroy;
begin
  if Canvas <> nil then
    TdxCustomDirect2DCanvas(Canvas).FImages.Remove(Self);
  Release;
  inherited Destroy;
end;

procedure TdxDirect2DBasedImage.Draw(const ATargetRect, ASourceRect: TRect;
  AAlpha: Byte);
begin
  if FHandle <> nil then
    TdxCustomDirect2DCanvas(Canvas).DrawBitmap(FHandle, ATargetRect,
      ASourceRect, AAlpha);
end;

procedure TdxDirect2DBasedImage.Release;
begin
  FHandle := nil;
  FCanvas := nil;
end;

{ TdxDirect2DBasedTextLayout }

constructor TdxDirect2DBasedTextLayout.Create(ACanvas: TdxCustomDirect2DCanvas);
begin
  inherited Create(ACanvas);
end;

destructor TdxDirect2DBasedTextLayout.Destroy;
begin
  ReleaseFont;
  inherited;
end;

procedure TdxDirect2DBasedTextLayout.SetColor(ATextColor: TColor);
begin
  FColor := dxColorToAlphaColor(ATextColor);
end;

procedure TdxDirect2DBasedTextLayout.SetFont(AFont: TcxCanvasBasedFont;
  AOwnership: TdxObjectOwnership);
begin
  if (FFont = nil) or not FFont.Equals(AFont) then
  begin
    ReleaseFont;
    FFont := TdxDirect2DBasedFont(AFont);
    FFontOwnership := AOwnership;
    if AOwnership = ooCloned then
      raise EInvalidArgument.Create('ooCloned was not supported here');
    TextChanged;
  end;
end;

procedure TdxDirect2DBasedTextLayout.DoCalculateLayout;

  procedure CalculateOptimialLayoutLimitedByRowCount;
  var
    AOptimalWidth: Single;
    ATextMetrics: TDWriteTextMetrics;
    AWidth, AWidthLow, AWidthHigh: Single;
  begin
    FHandle.SetMaxHeight(0);
    FHandle.SetMaxWidth(MaxWord);
    FHandle.GetMetrics(ATextMetrics);

    AOptimalWidth := MaxWord;
    AWidthHigh := ATextMetrics.WidthIncludingTrailingWhitespace;
    AWidthLow := AWidthHigh / (Length(FText) + 1);
    while AWidthLow <= AWidthHigh do
    begin
      AWidth := (AWidthLow + AWidthHigh) / 2;
      FHandle.SetMaxWidth(AWidth);
      FHandle.GetMetrics(ATextMetrics);
      if ATextMetrics.LineCount <= Cardinal(FMaxRowCount) then
      begin
        AWidthHigh := AWidth - 1;
        if ATextMetrics.LineCount = Cardinal(FMaxRowCount) then
        begin
          if AWidth > AOptimalWidth then
            Break;
          AOptimalWidth := AWidth;
        end;
      end
      else
        AWidthLow := AWidth + 1;
    end;
    FHandle.SetMaxWidth(AOptimalWidth);
  end;

  function GetActualMaxHeight(AMaxHeight: Single): Single;
  var
    ATextMetrics: TdxDWriteTextMetrics;
  begin
    if AMaxHeight > 0 then
      Result := Min(AMaxHeight, MaxSize)
    else if FMaxRowCount > 0 then
    begin
      CalculateTextMetrics(ATextMetrics);
      Result := Ceil(FMaxRowCount * ATextMetrics.LineHeight);
    end
    else
      Result := 1;
  end;

  function GetActualMaxWidth(AMaxWidth: Single): Single;
  begin
    if AMaxWidth <= 0 then
      Exit(MaxSize);

    Result := Min(AMaxWidth, MaxSize);
    if FFlags and CXTO_CHARBREAK = 0 then
      Result := Max(Result, GetMinWidth);
  end;

var
  AMaxHeight: Integer;
  AMaxWidth: Integer;
  ATextMetrics: TdxDWriteTextMetrics;
begin
  HandleNeeded;

  if FText = '' then
  begin
    FSize := cxNullSize;
    FRowCount := 0;
    Exit;
  end;

  AMaxHeight := FMaxHeight;
  AMaxWidth := FMaxWidth;

  if IsVertical then
    ExchangeIntegers(AMaxWidth, AMaxHeight);

  if (AMaxWidth = 0) and (FMaxRowCount > 1) then
    CalculateOptimialLayoutLimitedByRowCount
  else
  begin
    FHandle.SetMaxWidth(GetActualMaxWidth(AMaxWidth));
    FHandle.SetMaxHeight(GetActualMaxHeight(AMaxHeight));
  end;

  CalculateTextMetrics(ATextMetrics);
  if FMaxRowCount > 0 then
    ATextMetrics.LineCount := Min(ATextMetrics.LineCount, FMaxRowCount);
  if AMaxHeight > 0 then
    ATextMetrics.LineCount := Min(ATextMetrics.LineCount,
      CalculateRowCount(ATextMetrics.LineHeight));

  FSize.cx := Ceil(ATextMetrics.WidthIncludingTrailingWhitespace);
  FSize.cy := Ceil(ATextMetrics.LineCount * ATextMetrics.LineHeight);
  FRowCount := ATextMetrics.LineCount;

  FHandle.SetMaxWidth(FSize.cx);
  FHandle.SetMaxHeight(FSize.cy);

  if IsVertical then
    FSize := cxSize(FSize.cy, FSize.cx);
end;

procedure TdxDirect2DBasedTextLayout.DoDraw(const R: TRect);
begin
  TdxCustomDirect2DCanvas(Canvas).DrawTextLayout(FHandle, R.left,
    R.top, FColor);
end;

procedure TdxDirect2DBasedTextLayout.TextChanged;
begin
  FHandle := nil;
  inherited;
end;

procedure TdxDirect2DBasedTextLayout.ApplyFlags;
var
  ARtlReading: Boolean;
  ATrimming: TDwriteTrimming;
begin
  if FHandle = nil then
    Exit;

  ARtlReading := FFlags and CXTO_RTLREADING = CXTO_RTLREADING;

  if ARtlReading then
    FHandle.SetReadingDirection(DWRITE_READING_DIRECTION_RIGHT_TO_LEFT)
  else
    FHandle.SetReadingDirection(DWRITE_READING_DIRECTION_LEFT_TO_RIGHT);

  if FFlags and CXTO_CENTER_HORIZONTALLY = CXTO_CENTER_HORIZONTALLY then
    FHandle.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_CENTER)
  else if (FFlags and CXTO_RIGHT = CXTO_RIGHT) = not ARtlReading then
    FHandle.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_TRAILING)
  else
    FHandle.SetTextAlignment(DWRITE_TEXT_ALIGNMENT_LEADING);

  if FFlags and CXTO_WORDBREAK = CXTO_WORDBREAK then
    FHandle.SetWordWrapping(DWRITE_WORD_WRAPPING_WRAP)
  else
    FHandle.SetWordWrapping(DWRITE_WORD_WRAPPING_NO_WRAP);

  ATrimming.delimiter := 0;
  ATrimming.delimiterCount := 0;
  FHasEndEllipsis := FFlags and CXTO_END_ELLIPSIS = CXTO_END_ELLIPSIS;
  if FHasEndEllipsis then
  begin
    ATrimming.granularity := DWRITE_TRIMMING_GRANULARITY_CHARACTER;
    FHandle.SetTrimming(ATrimming, FFont.EndEllipsisSign);
  end
  else
  begin
    ATrimming.granularity := DWRITE_TRIMMING_GRANULARITY_NONE;
    FHandle.SetTrimming(ATrimming, nil);
  end;
end;

procedure TdxDirect2DBasedTextLayout.CalculateTextMetrics
  (out AMetrics: TdxDWriteTextMetrics);
var
  I: Integer;
  ALineCount: Cardinal;
  ALineMetrics: array of TDwriteLineMetrics;
  ATextMetrics: TDWriteTextMetrics;
begin
  FHandle.GetMetrics(ATextMetrics);

  ZeroMemory(@AMetrics, SizeOf(AMetrics));
  AMetrics.Height := ATextMetrics.Height;
  AMetrics.Width := ATextMetrics.Width;
  AMetrics.WidthIncludingTrailingWhitespace :=
    ATextMetrics.WidthIncludingTrailingWhitespace;

  if FHasEndEllipsis then
  begin
    FHandle.GetLineMetrics(nil, 0, ALineCount);
    if ALineCount > 0 then
    begin
      SetLength(ALineMetrics, ALineCount);
      FHandle.GetLineMetrics(@ALineMetrics[0], ALineCount, ALineCount);
      AMetrics.LineHeight := ALineMetrics[0].Height;
      for I := 0 to ALineCount - 1 do
      begin
        if ALineMetrics[I].Height > 0 then
          Inc(AMetrics.LineCount)
        else
          Break;
      end;
    end;
  end
  else
  begin
    AMetrics.LineCount := ATextMetrics.LineCount;
    AMetrics.LineHeight := ATextMetrics.Height / Max(1, ATextMetrics.LineCount);
  end;
end;

procedure TdxDirect2DBasedTextLayout.HandleNeeded;
var
  AErrorCode: HRESULT;
  ATextRange: TDwriteTextRange;
begin
  if FHandle = nil then
  begin
    FMinWidth := -1;
    AErrorCode := DWriteFactory.CreateTextLayout(PWideChar(FText),
      Length(FText), FFont.NativeHandle, 0, 0, FHandle);
    if FHandle = nil then
      raise EdxDirectXError.Create(AErrorCode);

    ATextRange.StartPosition := 0;
    ATextRange.Length := Length(FText);
    if fsUnderline in FFont.Style then
      FHandle.SetUnderline(True, ATextRange);
    if fsStrikeOut in FFont.Style then
      FHandle.SetStrikethrough(True, ATextRange);
    ApplyFlags;
  end;
end;

function TdxDirect2DBasedTextLayout.GetMinWidth: Single;
begin
  if FMinWidth < 0 then
    FHandle.DetermineMinWidth(FMinWidth);
  Result := FMinWidth;
end;

procedure TdxDirect2DBasedTextLayout.ReleaseFont;
begin
  if FFontOwnership = ooOwned then
    FreeAndNil(FFont);
  FFont := nil;
end;

{ TdxDirect2DGdiCompatibleCanvas }

constructor TdxDirect2DGdiCompatibleCanvas.Create;
begin
  inherited Create;
  CreateRenderTarget;
end;

procedure TdxDirect2DGdiCompatibleCanvas.BeginPaint(DC: HDC; const R: TRect);
begin
  FRenderTarget.BindDC(DC, R);
  DoBeginDraw(R);
end;

procedure TdxDirect2DGdiCompatibleCanvas.EndPaint;
begin
  DoEndDraw;
  if FRecreateContextNeeded then
    CreateRenderTarget;
end;

procedure TdxDirect2DGdiCompatibleCanvas.CreateRenderTarget;
var
  AProperties: TD2D1RenderTargetProperties;
begin
  ZeroMemory(@AProperties, SizeOf(AProperties));
  AProperties.&type := D2D1_RENDER_TARGET_TYPE_DEFAULT;
  AProperties.pixelFormat := D2D1PixelFormat(DXGI_FORMAT_B8G8R8A8_UNORM,
    D2D1_ALPHA_MODE_PREMULTIPLIED);
  AProperties.usage := D2D1_RENDER_TARGET_USAGE_GDI_COMPATIBLE;
  CheckD2D1Result(D2D1Factory1.CreateDCRenderTarget(AProperties,
    FRenderTarget));
  DeviceContext := FRenderTarget as ID2D1DeviceContext;
  FRecreateContextNeeded := False;
end;

{ TdxDirect2DHwndBasedCanvas }

constructor TdxDirect2DHwndBasedCanvas.Create(const AOwner
  : IdxDirect2DCanvasOwner; const ADevice: IDXGIDevice1;
  const AContext: ID2D1DeviceContext; const ADevice3D: ID3D11Device;
  const ADevice3DContext: ID3D11DeviceContext);
begin
  inherited Create;
  FOwner := AOwner;
  FDevice := ADevice;
  FDevice3D := ADevice3D;
  FDevice3DContext := ADevice3DContext;
  DeviceContext := AContext;

  FPresentParameters.DirtyRectsCount := 1;
  FPresentParameters.pDirtyRects := @FUpdateRect;
end;

procedure TdxDirect2DHwndBasedCanvas.BeginPaint;
var
  AWindowSize: TSize;
begin
  Windows.BeginPaint(FWindowHandle, FPaintStruct);
  FUpdateRect := FPaintStruct.rcPaint;
  AWindowSize := cxSize(cxGetClientRect(FWindowHandle));

  if not cxSizeIsEqual(FTextureSize, AWindowSize) then
  begin
    ReleaseTexture;
    FTextureSize := AWindowSize;
    CheckD2D1Result(FSwapChain.ResizeBuffers(0, FTextureSize.cx,
      FTextureSize.cy, DXGI_FORMAT_UNKNOWN, 0));
    CreateTexture;
    FUpdateRect := cxRect(FTextureSize);
  end;

  DoBeginDraw(FUpdateRect);
end;

procedure TdxDirect2DHwndBasedCanvas.EndPaint;
begin
  DoEndDraw;
  DoPresentBuffer;
  Windows.EndPaint(FWindowHandle, FPaintStruct);
  if FRecreateContextNeeded then
    FOwner.RecreateNeeded;
end;

procedure TdxDirect2DHwndBasedCanvas.CopyToDC(DC: HDC);
var
  R: TRect;
begin
  R := cxRect(FTextureSize);
  if not cxRectIsEmpty(R) then
    CopyToDC(DC, R, R);
end;

procedure TdxDirect2DHwndBasedCanvas.CopyToDC(DC: HDC;
  const ATargetRect, ASourceRect: TRect);
var
  ASourceBox: TD3D11Box;
  ASourceDC: HDC;
  ASurface: IDXGISurface1;
begin
  CheckCreateFrontBufferContent;

  ASourceBox := TD3D11Box.Create(ASourceRect);
  Device3DContext.CopySubresourceRegion(FFrontBufferContent, 0,
    ASourceRect.left, ASourceRect.top, 0, FFrontBufferSurface as ID3D11Resource,
    0, @ASourceBox);

  if Supports(FFrontBufferContent, IDXGISurface1, ASurface) then
  begin
    ASurface.GetDC(False, ASourceDC);
    cxBitBlt(DC, ASourceDC, ATargetRect, ASourceRect.TopLeft, SRCCOPY);
    ASurface.ReleaseDC(nil);
  end;
end;

procedure TdxDirect2DHwndBasedCanvas.SetWndHandle(AHandle: HWND);
const
  ScalingMode: array [Boolean] of TDXGIScaling = (DXGI_SCALING_STRETCH,
    DXGI_SCALING_NONE);
var
  AAdapter: IDXGIAdapter;
  AFactory: IDXGIFactory2;
  ASwapChainDescription: TDXGISwapChainDesc1;
begin
  if FWindowHandle <> AHandle then
  begin
    if FWindowHandle <> 0 then
    begin
      ReleaseTexture;
      FSwapChain := nil;
      FWindowHandle := 0;
      FWinControl := nil;
    end;
    if AHandle <> 0 then
    begin
      FWindowHandle := AHandle;
      FWinControl := FindControl(FWindowHandle);

      CheckD2D1Result(FDevice.SetMaximumFrameLatency
        (Ord(dxDirectXSwapChainSize) - 1));
      CheckD2D1Result(FDevice.GetAdapter(AAdapter));
      CheckD2D1Result(AAdapter.GetParent(IDXGIFactory2, AFactory));

      ZeroMemory(@ASwapChainDescription, SizeOf(ASwapChainDescription));
      ASwapChainDescription.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
      ASwapChainDescription.SampleDesc.Count := 1;
      ASwapChainDescription.BufferUsage := DXGI_USAGE_RENDER_TARGET_OUTPUT;
      ASwapChainDescription.BufferCount := dxDirectXSwapChainSize;
      ASwapChainDescription.SwapEffect := DXGI_SWAP_EFFECT_FLIP_SEQUENTIAL;
      ASwapChainDescription.Scaling := ScalingMode[IsWin8OrLater];

      CheckD2D1Result(AFactory.CreateSwapChainForHwnd(FDevice3D, AHandle,
        @ASwapChainDescription, nil, nil, FSwapChain));
    end;
  end;
end;

function TdxDirect2DHwndBasedCanvas.GetDefaultUseRightToLeftAlignment: Boolean;
begin
  Result := (FWinControl <> nil) and FWinControl.UseRightToLeftAlignment;
end;

procedure TdxDirect2DHwndBasedCanvas.CreateTexture;
var
  ABufferProperties: TD2D1BitmapProperties1;
  ACanvasTarget: ID2D1Bitmap1;
  ASurface: IDXGISurface;
begin
  FSwapChain.GetBuffer(0, IDXGISurface, ASurface);
  FSwapChain.GetBuffer(1, IDXGISurface, FFrontBufferSurface);

  ZeroMemory(@ABufferProperties, SizeOf(ABufferProperties));
  ABufferProperties.pixelFormat.Format := DXGI_FORMAT_B8G8R8A8_UNORM;
  ABufferProperties.pixelFormat.alphaMode := D2D1_ALPHA_MODE_PREMULTIPLIED;
  ABufferProperties.bitmapOptions := D2D1_BITMAP_OPTIONS_TARGET or
    D2D1_BITMAP_OPTIONS_CANNOT_DRAW;
  CheckD2D1Result(DeviceContext.CreateBitmapFromDxgiSurface(ASurface,
    ABufferProperties, ACanvasTarget));
  DeviceContext.SetTarget(ACanvasTarget);
end;

procedure TdxDirect2DHwndBasedCanvas.ReleaseDevice;
begin
  ReleaseTexture;
  inherited;
end;

procedure TdxDirect2DHwndBasedCanvas.ReleaseTexture;
begin
  FFrontBufferSurface := nil;
  FFrontBufferContent := nil;
  FFrontBufferContentSize := cxNullSize;
  DeviceContext.SetTarget(nil);
  FTextureSize := cxNullSize;
end;

procedure TdxDirect2DHwndBasedCanvas.CheckCreateFrontBufferContent;
var
  ATextureDescription: TD3D11Texture2DDesc;
  AFrontBufferTexture: ID3D11Texture2D;
begin
  if not cxSizeIsEqual(FTextureSize, FFrontBufferContentSize) then
    FFrontBufferContent := nil;

  if FFrontBufferContent = nil then
  begin
    AFrontBufferTexture := FFrontBufferSurface as ID3D11Texture2D;
    AFrontBufferTexture.GetDesc(ATextureDescription);

    ATextureDescription.BindFlags := D3D11_BIND_RENDER_TARGET;
    ATextureDescription.usage := D3D11_USAGE_DEFAULT;
    ATextureDescription.MiscFlags := D3D11_RESOURCE_MISC_GDI_COMPATIBLE;
    ATextureDescription.MipLevels := 1;
    ATextureDescription.SampleDesc.Count := 1;
    ATextureDescription.SampleDesc.Quality := 0;

    CheckD2D1Result(Device3D.CreateTexture2D(ATextureDescription, nil,
      FFrontBufferContent));
    FFrontBufferContentSize := FTextureSize;
  end;
end;

procedure TdxDirect2DHwndBasedCanvas.DoPresentBuffer;
begin
  if not cxRectIsEmpty(FUpdateRect) then
    try
      FSwapChain.Present1(0, 0 { DXGI_PRESENT_DO_NOT_WAIT } ,
        @FPresentParameters);
    except
      on E: Exception do
      begin
        if CheckNeedSwitchToGdiRenderMode(E) then
          FRecreateContextNeeded := True
        else
          raise;
      end;
    end;
end;

initialization

if IsWin8OrLater and not IsWin10OrLater then
  dxDirectXSwapChainSize := 4;

end.
