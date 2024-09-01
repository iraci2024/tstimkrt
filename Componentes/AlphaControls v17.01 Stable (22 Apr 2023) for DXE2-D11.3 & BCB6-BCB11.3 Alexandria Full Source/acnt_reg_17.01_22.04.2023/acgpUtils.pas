unit acgpUtils;
{$I sDefs.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  {$IFNDEF DELPHI5} Types, {$ENDIF}
  acntTypes, sConst, sGradient;

type
{$IFNDEF NOTFORHELP}
  TacPointF = record
    X: Single;
    Y: Single;
  end;
  PacPointF = ^TacPointF;

  PacGpPoints = ^TacGpPoints;
  TacGpPoints = array of TacPointF;
{$ENDIF} // NOTFORHELP

procedure acgpDrawLine   (DC: hdc; X1, Y1, X2, Y2: Single; Color: TColor; PenWidth: Single = 1);
procedure acgpDrawEllipse(DC: hdc; X, Y, Width, Height: Single; Color: TColor; PenWidth: Single = 1);
procedure acgpFillEllipse(DC: hdc; X, Y, Width, Height: Single; Color: TColor);
procedure acgpRoundRect  (DC: hdc; X, Y, Width, Height, Radius, BorderWidth: integer; FillColor, BorderColor: TColor); overload;
procedure acgpRoundRect  (DC: hdc; X, Y, Width, Height, BorderWidth: integer; BorderColor: TColor; PRadiuses: PacIntValues; PSides: PacSides); overload; // (asLeft, asTop, asRight, asBottom)

procedure acgpDrawRect(DC: hdc; R: TRect; BorderWidth: integer; BorderColor: TColor; PSides: PacSides);

procedure acgpDrawArc    (DC: hdc; X, Y, Width, Height, StartAngle, SweepAngle: Single; Color: TColor; PenWidth: Single = 1);
procedure acgpStretchRect(DstBmp, SrcBmp: TBitmap; SrcX, SrcY, SrcWidth, SrcHeight: integer); overload;
procedure acgpStretchRect(DstBmp, SrcBmp: TBitmap; DstRect, SrcRect: TRect); overload;
procedure acgpBlur       (Bmp: TBitmap; Radius: Single);

{$IFNDEF NOTFORHELP}
function acgpCreateLineBrush(DC: hdc; Point1, Point2: TPoint; Color1, Color2: TColor): Pointer;
{$ENDIF} // NOTFORHELP

procedure acgpFillRect          (DC: hdc; Color: TColor; R: TRect; AlphaValue: integer);
procedure acgpFillRectangle     (DC: hdc; Brush: Pointer; X, Y, Width, Height: Single);
procedure acgpGradientEllipse   (DC: hdc; X, Y, Width, Height: Single; Color1, Color2: TColor);
procedure acgpGradientRing      (DC: hdc; X, Y, Width, Height: Single; Color1, Color2: TColor; PenWidth: Single = 1);
procedure acgpGradientRectangleH(DC: hdc; X, Y, Width, Height: Single; Color1, Color2: TColor);
procedure acgpGradientRectangleV(DC: hdc; X, Y, Width, Height: Single; Color1, Color2: TColor);
procedure acgpGradientRectangle (DC: hdc; X, Y, Width, Height: Single; const Points: TGradPoints; const Colors: TGradColors; const CenterColor: TsColor);

procedure acgpDrawBeziers(DC: hdc; Color: TColor; Points: PacPointF; PointCount: integer; LineWidth: integer = 1);
procedure acgpDrawPolygon(DC: hdc; Color: TColor; Points: PacPointF; PointCount: integer; LineWidth: integer = 1);
procedure acgpFillPolygon(DC: hdc; Color: TColor; Points: PPoint; PointCount: integer); overload;
procedure acgpFillPolygon(DC: hdc; Color: TColor; Points: PacPointF; PointCount: integer); overload;
procedure acgpGradientPolygon(DC: hdc; Color1, Color2: TColor; BeginSide: TacSide; Points: PPoint; PointCount: integer);

procedure acgpDrawRotatedBitmap(DC: hdc; SrcBmp: TBitmap; R: TRect; Angle: integer);
procedure acgpRotatePoints(ARect: PacGpPoints; const ACenter: TacPointF; ACount, Angle: integer);

function acgpGetFontName(const FileName: acString): acString;
function acgpDrawString(DC: hdc; Text: WideString; aRect: TRect; Color: TColor; Alignment: TAlignment; VertAlignment: TVerticalAlignment; Angle: integer; AlphaLayer: boolean = False; FontName: WideString = ''): boolean;
//procedure acgpTextOut(DC: hdc; X, Y: integer; aRect: TRect; Text: acString; Color: TColor; Angle: integer);
function acgpMeasureString(DC: hdc; Text: acString; Wrapped: boolean; MaxWidth: integer = MaxInt): TSize; overload;
function acgpMeasureString(DC: hdc; Text: acString; AFont: TFont; Wrapped: boolean; MaxWidth: integer = MaxInt): TSize; overload;
{$IFDEF GDIPLUS_PNG}
procedure acgpBmpFromStream(Bmp: TBitmap; Stream: TStream);
{$ENDIF} // GDIPLUS_PNG


implementation

uses
  Math,
{$IFDEF GDIPLUS_PNG}
  ActiveX,
{$ENDIF} // GDIPLUS_PNG
  sVCLUtils, sSkinProvider, sAlphaGraph, acntUtils, sGraphUtils;

type
  TStatus = (Ok, GenericError, InvalidParameter, OutOfMemory, ObjectBusy, InsufficientBuffer, NotImplemented, Win32Error, WrongState,
    Aborted, FileNotFound, ValueOverflow, AccessDenied, UnknownImageFormat, FontFamilyNotFound, FontStyleNotFound, NotTrueTypeFont,
    UnsupportedGdiplusVersion, GdiplusNotInitialized, PropertyNotFound, PropertyNotSupported
  );

  TUnit = (UnitWorld, UnitDisplay, UnitPixel, UnitPoint, UnitInch, UnitDocument, UnitMillimeter);
  TDebugEventLevel = (DebugEventLevelFatal, DebugEventLevelWarning);

  TacgpWrapMode = (
    WrapModeTile,        // 0
    WrapModeTileFlipX,   // 1
    WrapModeTileFlipY,   // 2
    WrapModeTileFlipXY,  // 3
    WrapModeClamp        // 4
  );

  TGpPen      = Pointer;
  TGpGraphics = Pointer;
  TGpBrush    = Pointer;
  TGpBitmap   = Pointer;
  TGpImage    = Pointer;

  PARGB = ^ARGB;
  ARGB  = DWORD;

  NotificationUnhookProc = procedure (token: ULONG); stdcall;
  DebugEventProc         = procedure (level: TDebugEventLevel; message: PChar); stdcall;
  NotificationHookProc   = function (out token: ULONG): TStatus; stdcall;

  TGdiplusStartupInput = record
    GdiplusVersion: Cardinal;
    DebugEventCallback: DebugEventProc;
    SuppressBackgroundThread: BOOL;
    SuppressExternalCodecs: BOOL;
  end;
  PGdiplusStartupInput = ^TGdiplusStartupInput;

  TGdiplusStartupOutput = record
    NotificationHook  : NotificationHookProc;
    NotificationUnhook: NotificationUnhookProc;
  end;
  PGdiplusStartupOutput = ^TGdiplusStartupOutput;

  ImageAbort = function: BOOL; stdcall;
  DrawImageAbort = ImageAbort;

  TCompositingMode = (
    CompositingModeSourceOver, // 0
    CompositingModeSourceCopy  // 1 - faster
  );

{$IFDEF DELPHI6UP}
  TQualityMode = (
    QualityModeInvalid = -1,
    QualityModeDefault = 0,
    QualityModeLow     = 1,
    QualityModeHigh    = 2
  );

  TSmoothingMode = (
    SmoothingModeInvalid     = -1,
    SmoothingModeDefault     = 0,
    SmoothingModeHighSpeed   = 1,
    SmoothingModeHighQuality = 2,
    SmoothingModeNone,
    SmoothingModeAntiAlias8x4,
    SmoothingModeAntiAlias = SmoothingModeAntiAlias8x4,
    SmoothingModeAntiAlias8x8
  );

  TCompositingQuality = (
    CompositingQualityInvalid          = ord(QualityModeInvalid),
    CompositingQualityDefault          = ord(QualityModeDefault),
    CompositingQualityHighSpeed        = ord(QualityModeLow),
    CompositingQualityHighQuality      = ord(QualityModeHigh),
    CompositingQualityGammaCorrected,
    CompositingQualityAssumeLinear
  );

  TInterpolationMode = (
    InterpolationModeInvalid          = ord(QualityModeInvalid),
    InterpolationModeDefault          = ord(QualityModeDefault),
    InterpolationModeLowQuality       = ord(QualityModeLow),
    InterpolationModeHighQuality      = ord(QualityModeHigh),
    InterpolationModeBilinear,
    InterpolationModeBicubic,
    InterpolationModeNearestNeighbor,
    InterpolationModeHighQualityBilinear,
    InterpolationModeHighQualityBicubic
  );

{$ELSE}
  TQualityMode = Integer;
  TSmoothingMode = Integer;
  TCompositingQuality = Integer;
  TInterpolationMode = Integer;

const
  QualityModeInvalid = -1;
  QualityModeDefault = 0;
  QualityModeLow     = 1;
  QualityModeHigh    = 2;

  SmoothingModeInvalid     = -1;
  SmoothingModeDefault     = 0;
  SmoothingModeHighSpeed   = 1;
  SmoothingModeHighQuality = 2;
  SmoothingModeNone        = 3;
  SmoothingModeAntiAlias   = 4;

  CompositingQualityInvalid          = QualityModeInvalid;
  CompositingQualityDefault          = QualityModeDefault;
  CompositingQualityHighSpeed        = QualityModeLow;
  CompositingQualityHighQuality      = QualityModeHigh;
  CompositingQualityGammaCorrected   = 3;
  CompositingQualityAssumeLinear     = 4;

  InterpolationModeInvalid          = QualityModeInvalid;
  InterpolationModeDefault          = QualityModeDefault;
  InterpolationModeLowQuality       = QualityModeLow;
  InterpolationModeHighQuality      = QualityModeHigh;
  InterpolationModeBilinear         = 3;
  InterpolationModeBicubic          = 4;
  InterpolationModeNearestNeighbor     = 5;
  InterpolationModeHighQualityBilinear = 6;
  InterpolationModeHighQualityBicubic  = 7;

{$ENDIF}

type
  MatrixOrder = (
    MatrixOrderPrepend,
    MatrixOrderAppend
  );
  TMatrixOrder = MatrixOrder;

  PGPRect = ^TGPRect;
  TGPRect = record
    X     : Integer;
    Y     : Integer;
    Width : Integer;
    Height: Integer;
  end;

  PGPRectF = ^TGPRectF;
  TGPRectF = record
    X     : Single;
    Y     : Single;
    Width : Single;
    Height: Single;
  end;

  TBitmapData = record
    Width       : UINT;
    Height      : UINT;
    Stride      : Integer;
    PixelFormat : integer;
    Scan0       : Pointer;
    Reserved    : {$IFDEF DELPHI7UP}NativeUInt{$ELSE}Cardinal{$ENDIF};
  end;
  PBitmapData = ^TBitmapData;

  TStringAlignment = (
    StringAlignmentNear,
    StringAlignmentCenter,
    StringAlignmentFar
  );

  TTextRenderingHint = (
    TextRenderingHintSystemDefault,                // Glyph with system default rendering hint
    TextRenderingHintSingleBitPerPixelGridFit,     // Glyph bitmap with hinting
    TextRenderingHintSingleBitPerPixel,            // Glyph bitmap without hinting
    TextRenderingHintAntiAliasGridFit,             // Glyph anti-alias bitmap with hinting
    TextRenderingHintAntiAlias,                    // Glyph anti-alias bitmap without hinting
    TextRenderingHintClearTypeGridFit              // Glyph CT bitmap with hinting
  );

const
  acAlignArray: array [TAlignment] of TStringAlignment = (
    StringAlignmentNear,
    StringAlignmentFar,
    StringAlignmentCenter
  );

  acVertAlignArray: array [TVerticalAlignment] of TStringAlignment = (
    StringAlignmentNear,
    StringAlignmentFar,
    StringAlignmentCenter
  );

var
  acDefaultSmoothingMode: TSmoothingMode = SmoothingModeHighQuality;
  StartupInput: TGDIPlusStartupInput;
  gdiplusToken: ULONG = 0;
  hGP: HMODULE = 0;

  // Initializing
  acGdiplusStartup:            function (out token: ULONG; input: PGdiplusStartupInput; output: PGdiplusStartupOutput): TStatus; stdcall;
  acGdiplusShutdown:           procedure (token: ULONG); stdcall;
  // Objects creation
  acGdipCreateFromHDC:         function (hdc: HDC; out graphics: TGpGraphics): TStatus; stdcall;
  acGdipCreatePen1:            function (Color: ARGB; Width: Single; unit_: TUnit; out pen: TGpPen): TStatus; stdcall;
  acGdipCreateSolidFill:       function (Color: ARGB; out brush: TGpBrush): TStatus; stdcall;
  acGdipCreateBitmapFromScan0: function (Width: Integer; height: Integer; stride: Integer; format: integer; scan0: PBYTE; out bitmap: TGpBitmap): TStatus; stdcall;
  acGdipCreateLineBrush:       function (Point1, Point2: PacPointF; Color1, Color2: ARGB; WrapMode: TacgpWrapMode; out LineGradient: Pointer): TStatus; stdcall;
  // Objects destroying
  acGdipDeleteGraphics:        function (Graphics: TGpGraphics): TStatus; stdcall;
  acGdipDisposeImage:          function (Image: Pointer): TStatus; stdcall;
  acGdipDeleteBrush:           function (Brush: TGpBrush): TStatus; stdcall;
  acGdipDeletePen:             function (Pen: TGpPen): TStatus; stdcall;
  // Primitives
  acGdipDrawPolygon:           function (Graphics: TGpGraphics; Pen: TGpPen; Points: PacPointF; Count: Integer): TStatus; stdcall;
  acGdipFillPolygon:           function (Graphics: TGpGraphics; Brush: TGpBrush; Points: PacPointF; Count: Integer; FillMode: uint): TStatus; stdcall;
  acGdipDrawBeziers:           function (Graphics: TGpGraphics; Pen: TGpPen; Points: PacPointF; Count: Integer): TStatus; stdcall;
  acGdipDrawImagePoints:       function (Graphics: TGpGraphics; image: TGPImage; dstpoints: PacGpPoints; count: Integer): TStatus; stdcall;

  acGdipDrawEllipse:           function (Graphics: TGpGraphics; pen: TGpPen; X, Y, Width, Height: Single): TStatus; stdcall;
  acGdipFillEllipse:           function (Graphics: TGpGraphics; brush: TGpBrush; x, y, width, height: Single): TStatus; stdcall;

  acGdipDrawLine:              function (Graphics: TGpGraphics; pen: TGpPen; x1, y1, x2, y2: Single): TStatus; stdcall;
  acGdipDrawArc:               function (Graphics: TGpGraphics; pen: TGpPen; X, Y, Width, Height, StartAngle, SweepAngle: Single): TStatus; stdcall;
  acGdipFillPie:               function (Graphics: TGpGraphics; Brush: TGpBrush; X: Single; Y: Single; Width: Single; Height: Single; StartAngle: Single; SweepAngle: Single): TStatus; stdcall;
  acGdipFillRectangle:         function (Graphics: TGpGraphics; Brush: TGpBrush; X: Single; Y: Single; Width: Single; Height: Single): TStatus; stdcall;
  // Effects
  acGdipCreateEffect:          function (Guid: TGUID; out Effect: Pointer): TStatus; stdcall;
  acGdipSetEffectParameters:   function (Effect: Pointer; const Params: Pointer; const Size: UINT): TStatus; stdcall;
  // Gradients
  acSetPenBrushFill:           function (Pen: TGpPen; Brush: TGpBrush): TStatus; stdcall;
  acGdipCreatePathGradient:    function (const Points: PacPointF; Count: Integer; WrapMode: TacgpWrapMode; out PolyGradient: Pointer): TStatus; stdcall;
  acGdipSetPathGradientCenterColor: function (Brush: TGpBrush; Color: ARGB): TStatus; stdcall;
  acGdipSetPathGradientSurroundColorsWithCount: function (Brush: TGpBrush; const Color: PARGB; out Count: Integer): TStatus; stdcall;

  acGdipBitmapCreateApplyEffect: function (const InputBitmaps: Pointer; NumInputs: Integer; Effect: Pointer; Roi: Windows.PRect; OutputRect: Windows.PRect;
                                           out OutputBitmap: Pointer; UseAuxData: Bool; out AuxData: Pointer; out AuxDataSize: Integer): TStatus; stdcall;
  // Bitmaps manipulations
  acGdipCreateHBITMAPFromBitmap: function(Bitmap: Pointer; out HbmReturn: HBitmap; Background: ARGB): TStatus; stdcall;
  acGdipCreateBitmapFromHBITMAP: function(Hbm: HBitmap; Hpal: HPalette; out Bitmap: Pointer): TStatus; stdcall;
{$IFDEF GDIPLUS_PNG}
  acGdipCreateBitmapFromStream: function(stream: IStream; out bitmap: TGpBitmap): TStatus; stdcall;
{$ENDIF} // GDIPLUS_PNG
  acGdipBitmapLockBits:        function (Bitmap: TGpBitmap; rect: PGPRect; flags: UINT; format: integer; lockedBitmapData: PBITMAPDATA): TStatus; stdcall;
  acGdipBitmapUnlockBits:      function (Bitmap: TGpBitmap; lockedBitmapData: PBITMAPDATA): TStatus; stdcall;
{
  acGdipDrawImageRectRect:       function(Graphics: TGpGraphics; image: TGpImage;
                                          dstx: Single; dsty: Single; dstwidth: Single; dstheight: Single;
                                          srcx: Single; srcy: Single; srcwidth: Single; srcheight: Single;
                                          srcUnit: TUnit; imageAttributes: Pointer; callback: DRAWIMAGEABORT; callbackData: Pointer): TStatus; stdcall;
}
  // Stretch image
  acGdipDrawImageRectRect: function(Graphics: TGpGraphics; image: TGpImage;
                                    dstx: integer; dsty: integer; dstwidth: integer; dstheight: integer;
                                    srcx: integer; srcy: integer; srcwidth: integer; srcheight: integer;
                                    srcUnit: TUnit; imageAttributes: Pointer; callback: DRAWIMAGEABORT; callbackData: Pointer): TStatus; stdcall;
  // Fonts
  acGdipNewPrivateFontCollection: function(out fontCollection: Pointer): TStatus; stdcall;
  acGdipPrivateAddFontFile: function(fontCollection: Pointer; filename: PWChar): TStatus; stdcall;
  acGdipGetFontCollectionFamilyList: function(fontCollection: Pointer; numSought: Integer; gpfamilies: Pointer; var numFound: Integer): TStatus; stdcall;
  acGdipGetFamilyName: function (family: Pointer; name: PWideChar; language: LANGID): TStatus; stdcall;

  // Draw modes init
  acGdipSetInterpolationMode:  function (Graphics: TGpGraphics; InterpolationMode: TInterpolationMode): TStatus; stdcall;
  acGdipSetSmoothingMode:      function (Graphics: TGpGraphics; smoothingMode: TSmoothingMode): TStatus; stdcall;
  acGdipSetCompositingQuality: function (Graphics: TGpGraphics; compositingQuality: TCompositingQuality): TStatus; stdcall;
  acGdipSetCompositingMode:    function (Graphics: TGpGraphics; compositingMode: TCompositingMode): TStatus; stdcall;
//  acGdipSetPenBrushFill:      function (pen: TGpPen; brush: TGpBrush): TStatus; stdcall;

  // Strings
  acGdipCreateStringFormat: function(formatAttributes: Integer; language: LANGID; out Format: Pointer): TStatus; stdcall;
  acGdipCreateFontFromDC: function(hdc: HDC; out font: Pointer): TStatus; stdcall;
  acGdipCreateFont: function(fontFamily: Pointer; emSize: Single; style: Integer; unit_: Integer; out font: Pointer): TStatus; stdcall;
  acGdipCreateFontFamilyFromName: function(name: PWCHAR; fontCollection: Pointer; out FontFamily: Pointer): TStatus; stdcall;
  acGdipTranslateWorldTransform: function(Graphics: TGpGraphics; dx: Single; dy: Single; order: TMatrixOrder): TStatus; stdcall;
  acGdipRotateWorldTransform: function (Graphics: TGpGraphics; angle: Single; order: TMatrixOrder): TStatus; stdcall;
  acGdipSetStringFormatAlign: function (Format: Pointer; Align: TStringAlignment): TStatus; stdcall;
  acGdipSetStringFormatLineAlign: function (Format: Pointer; Align: TStringAlignment): TStatus; stdcall;
  acGdipSetTextRenderingHint: function (Graphics: TGpGraphics; Mode: TTextRenderingHint): TStatus; stdcall;
  acGdipSetStringFormatFlags: function (Format: Pointer; Flags: Integer): TStatus; stdcall;
  acGdipDeleteStringFormat: function(format: Pointer): TStatus; stdcall;
  acGdipDeleteFont: function (font: Pointer): TStatus; stdcall;
  acGdipDrawString: function(Graphics: TGpGraphics; String_: PWChar; Length: Integer; Font: Pointer; layoutRect: PGPRectF; stringFormat: Pointer; brush: TGpBrush): TStatus; stdcall;
  acGdipMeasureString: function (Graphics: TGpGraphics; String_: PWChar; Length: Integer; Font: Pointer; LayoutRect: PGPRectF;
                                 stringFormat: Pointer; boundingBox: PGPRectF; codepointsFitted: PInteger; linesFilled: PInteger): TStatus; stdcall;


function acGDIBegin(Allowed: boolean): boolean;
begin
  Result := False;
  if hGP <> 0 then begin
    if Allowed then
      if Assigned(acGdiplusStartup) then begin
        StartupInput.DebugEventCallback := nil;
        StartupInput.SuppressBackgroundThread := False;
        StartupInput.SuppressExternalCodecs := False;
        StartupInput.GdiplusVersion := 1;
        acGdiplusStartup(gdiplusToken, @StartupInput, nil);
      end;

    Result := gdiplusToken <> 0;
  end;
end;


procedure acGDIEnd(Allowed: boolean);
begin
  if Allowed and (hGP <> 0) and (gdiplusToken <> 0) then
    acGdiplusShutdown(gdiplusToken);
end;


procedure acgpDrawEllipse(DC: hdc; X, Y, Width, Height: Single; Color: TColor; PenWidth: Single = 1);
var
  GpP: TGpPen;
  GpG: TGpGraphics;
begin
  if acGDIBegin(IsLibrary) then
    try
      if Assigned(acGdipDrawEllipse) and (acGdipCreateFromHDC(DC, GpG) = Ok) then
        try
          acGdipSetSmoothingMode(GpG, acDefaultSmoothingMode);
          if acGdipCreatePen1($FF000000 or Cardinal(SwapRedBlue(Color)), PenWidth, UnitWorld, GpP) = Ok then begin
            acGdipDrawEllipse(GpG, GpP, X, Y, Width, Height);
            acGdipDeletePen(GpP);
          end;
        finally
          acGdipDeleteGraphics(GpG);
        end;
    finally
      acGDIEnd(IsLibrary);
    end;
end;


procedure acgpDrawLine(DC: hdc; X1, Y1, X2, Y2: Single; Color: TColor; PenWidth: Single = 1);
var
  C: TsColor;
  GpP: TGpPen;
  GpG: TGpGraphics;
begin
  if acGDIBegin(IsLibrary) then
    try
      if Assigned(acGdipDrawLine) and (acGdipCreateFromHDC(DC, GpG) = Ok) then
        try
          acGdipSetSmoothingMode(GpG, acDefaultSmoothingMode);
          C.I := SwapRedBlue(Color);
          if C.A = 0 then
            C.I := integer($FF000000 or Cardinal(C.C));

          if acGdipCreatePen1(Cardinal(C.I), PenWidth, UnitWorld, GpP) = Ok then begin
            acGdipDrawLine(GpG, GpP, X1, Y1, X2, Y2);
            acGdipDeletePen(GpP);
          end;
        finally
          acGdipDeleteGraphics(GpG);
        end;
    finally
      acGDIEnd(IsLibrary);
    end;
end;


procedure acgpFillEllipse(DC: hdc; X, Y, Width, Height: Single; Color: TColor);
var
  GpB: TGpBrush;
  GpG: TGpGraphics;
begin
  if acGDIBegin(IsLibrary) then
    try
      if Assigned(acGdipFillEllipse) and (acGdipCreateFromHDC(DC, GpG) = Ok) then
        try
          acGdipSetSmoothingMode(GpG, acDefaultSmoothingMode);
          if Color and $FF000000 = 0 then
            Color := TColor(Cardinal(Color) or $FF000000);

          if acGdipCreateSolidFill(Cardinal(SwapRedBlue(Color)), GpB) = Ok then begin
            acGdipFillEllipse(GpG, GpB, X, Y, Width, Height);
            acGdipDeleteBrush(GpB);
          end;
        finally
          acGdipDeleteGraphics(GpG);
        end;
    finally
      acGDIEnd(IsLibrary);
    end;
end;


procedure acgpRoundRect(DC: hdc; X, Y, Width, Height, Radius, BorderWidth: integer; FillColor, BorderColor: TColor);
var
  bw: integer;
  GpP: TGpPen;
  GpB: TGpBrush;
  GpG: TGpGraphics;
  sd2, Size: Single;
  borderExists: boolean;
begin
  if acGDIBegin(IsLibrary) then
    try
      if Assigned(acGdipFillEllipse) and (acGdipCreateFromHDC(DC, GpG) = Ok) then
        try
          dec(Width);
          dec(Height);
          borderExists := (BorderColor <> clNone) and (BorderWidth > 0);
          if borderExists then begin
            bw := BorderWidth div 2;
            inc(X, bw);
            inc(Y, bw);
            dec(Width, BorderWidth - 1);
            dec(Height, BorderWidth - 1);
          end
          else begin
//            dec(Width);
//            dec(Height);
          end;

          acGdipSetSmoothingMode(GpG, acDefaultSmoothingMode);

          if Radius > Width div 2 then
            Radius := Width div 2;

          if Radius > Height div 2 then
            Radius := Height div 2;

          FillColor := SwapRedBlue(FillColor);
          if TsColor(FillColor).A = 0 then
            TsColor(FillColor).A := {100;//}MAXBYTE;

          sd2 := Radius;
          Size := Radius * 2;
          if (FillColor <> clNone) and (acGdipCreateSolidFill(Cardinal(FillColor), GpB) = Ok) then begin
            // LeftTop corner
            acGdipFillPie(GpG, GpB, X, Y, Size, Size, 180, 90);
            // LeftBottom corner
            acGdipFillPie(GpG, GpB, X, Y + Height - Size, Size, Size, 90, 90);
            // RightTop corner
            acGdipFillPie(GpG, GpB, X + Width - Size, Y, Size, Size, 270, 90);
            // RightBottom corner
            acGdipFillPie(GpG, GpB, X + Width - Size, Y + Height - Size, Size, Size, 0, 90);
            // Fill vert center
//            if X + sd2 - 1 < Width - Size + 2 then
            acGdipFillRectangle(GpG, GpB, X + sd2 - 1, Y, Width - Size + 2, Height);
            // Fill Horz center
//            if Y + sd2 - 1 < Height - Size +  2 then
            acGdipFillRectangle(GpG, GpB, X, Y + sd2 - 1, Width, Height - Size + 2);
            // Fill horz center left
//            acGdipFillRectangle(GpG, GpB, X, Y + sd2 - 1, sd2 - 1, Height - Size + 2);
            // Fill horz center right
//            acGdipFillRectangle(GpG, GpB, X + Width - sd2 + 1, Y + sd2 - 1, sd2, Height - Size + 2);
            acGdipDeleteBrush(GpB);
          end;
          if borderExists then begin
            if (Radius > 0) and (acGdipCreatePen1($FF000000 or Cardinal(SwapRedBlue(BorderColor)), BorderWidth, UnitWorld, GpP) = Ok) then begin
              // LeftTop corner
              acGdipDrawArc(GpG, GpP, X, Y, Size, Size, 180, 90);
              // LeftBottom corner
              acGdipDrawArc(GpG, GpP, X, Y + Height - Size, Size, Size, 90, 90);
              // RightTop corner
              acGdipDrawArc(GpG, GpP, X + Width - Size, Y, Size, Size, 270, 90);
              // RightBottom corner
              acGdipDrawArc(GpG, GpP, X + Width - Size, Y + Height - Size, Size, Size, 0, 90);
              // Fill horz line1
              acGdipDrawLine(GpG, GpP, X + sd2, Y, X + Width - sd2, Y);
              // Fill horz line2
              acGdipDrawLine(GpG, GpP, X + sd2, Y + Height, X + Width - sd2, Y + Height);
              // Fill vert line1
              acGdipDrawLine(GpG, GpP, X, Y + sd2, X, Y + Height - sd2);
              // Fill vert line2
              acGdipDrawLine(GpG, GpP, X + Width, Y + sd2, X + Width, Y + Height - sd2);
            end
            else
              FillDCBorder(DC, Rect(X, Y, X + Width, Y + Height), BorderWidth, BorderWidth, BorderWidth, BorderWidth, BorderColor);
          end;
        finally
          acGdipDeleteGraphics(GpG);
        end;
    finally
      acGDIEnd(IsLibrary);
    end;
end;


procedure acgpRoundRect(DC: hdc; X, Y, Width, Height, BorderWidth: integer; BorderColor: TColor; PRadiuses: PacIntValues; PSides: PacSides); overload;
var
  bw: integer;
  GpP: TGpPen;
  Size: Single;
  GpG: TGpGraphics;
  WidthOffset: integer;
  WidthIsEven: boolean;
  TL, TR, BL, BR: TPoint;
begin
  if acGDIBegin(IsLibrary) then
    try
      if Assigned(acGdipFillEllipse) and (acGdipCreateFromHDC(DC, GpG) = Ok) then
        try
          if (BorderColor <> clNone) and (BorderWidth > 0) then begin
            // Correct system coords
            bw := BorderWidth div 2;
            WidthIsEven := (BorderWidth mod 2) = 0;
            inc(X, bw);
            inc(Y, bw);
            dec(Width, BorderWidth);
            dec(Height, BorderWidth);
            acGdipSetSmoothingMode(GpG, acDefaultSmoothingMode);
            if acGdipCreatePen1($FF000000 or Cardinal(SwapRedBlue(BorderColor)), BorderWidth, UnitWorld, GpP) = Ok then begin
              TL := Point(X, Y);
              TR := Point(X + Width, Y);
              BL := Point(X, Y + Height);
              BR := Point(X + Width, Y + Height);
              if PRadiuses <> nil then with PRadiuses^ do begin
                // TopLeft corner
                if Value0 > 0 then begin
                  inc(TL.X, Value0);
                  inc(TL.Y, Value0);
                  Size := Value0 * 2;
                  acGdipDrawArc(GpG, GpP, X, Y, Size, Size, 180, 90);
                end;
                // TopRight corner
                if Value1 > 0 then begin
                  dec(TR.X, Value1);
                  inc(TR.Y, Value1);
                  Size := Value1 * 2;
                  acGdipDrawArc(GpG, GpP, X + Width - Size - integer(WidthIsEven), Y, Size, Size, 270, 90);
                end;
                // BottomLeft corner
                if Value2 > 0 then begin
                  inc(BL.X, Value2);
                  dec(BL.Y, Value2);
                  Size := Value2 * 2;
                  acGdipDrawArc(GpG, GpP, X, Y + Height - Size - integer(WidthIsEven), Size, Size, 90, 90);
                end;
                // BottomRight corner
                if Value3 > 0 then begin
                  dec(BR.X, Value3);
                  dec(BR.Y, Value3);
                  Size := Value3 * 2;
                  acGdipDrawArc(GpG, GpP, X + Width - Size - integer(WidthIsEven), Y + Height - Size - integer(WidthIsEven), Size, Size, 0, 90);
                end;
              end;
              WidthOffset := max(0, BorderWidth div 2);// - max(0, BorderWidth mod 2) + 1;

              // Fill horz line1
              if ((PSides = nil) or (asTop in PSides^)) and (TR.X > TL.X) then
                acGdipDrawLine(GpG, GpP, TL.X - WidthOffset, Y, TR.X + WidthOffset, Y);
              // Fill horz line2
              if ((PSides = nil) or (asBottom in PSides^)) and (BR.X > BL.X) then
                acGdipDrawLine(GpG, GpP, BL.X - WidthOffset, Y + Height - integer(WidthIsEven), BR.X + WidthOffset, Y + Height - integer(WidthIsEven));
//                acGdipDrawLine(GpG, GpP, BL.X - WidthOffset, Y + Height, BR.X + WidthOffset, Y + Height);

              // Fill vert line1
              if ((PSides = nil) or (asLeft in PSides^)) and (BL.Y > TL.Y) then
                acGdipDrawLine(GpG, GpP, X, TL.Y - WidthOffset, X, BL.Y + WidthOffset);
              // Fill vert line2
              if ((PSides = nil) or (asRight in PSides^)) and (BR.Y > TR.Y) then
                acGdipDrawLine(GpG, GpP, X + Width - integer(WidthIsEven), TR.Y - WidthOffset, X + Width - integer(WidthIsEven), BR.Y + WidthOffset);
            end;
          end;
        finally
          acGdipDeleteGraphics(GpG);
        end;
    finally
      acGDIEnd(IsLibrary);
    end;
end;


procedure acgpDrawRect(DC: hdc; R: TRect; BorderWidth: integer; BorderColor: TColor; PSides: PacSides);
var
  bw: integer;
  GpP: TGpPen;
//  Size: Single;
  GpG: TGpGraphics;
  WidthOffset: integer;
//  TL, TR, BL, BR: TPoint;
begin
  if (BorderColor <> clNone) and (BorderWidth > 0) then
    if acGDIBegin(IsLibrary) then
      try
        if (acGdipCreateFromHDC(DC, GpG) = Ok) then
          try
            // Correct system coords
            bw := BorderWidth div 2;
            inc(R.Left, bw);
            inc(R.Top, bw);
            dec(R.Right, BorderWidth);
            dec(R.Bottom, BorderWidth);
            acGdipSetSmoothingMode(GpG, acDefaultSmoothingMode);
            if acGdipCreatePen1($FF000000 or Cardinal(SwapRedBlue(BorderColor)), BorderWidth, UnitWorld, GpP) = Ok then begin
//              TL := Point(R.Left, R.Top);
//              TR := Point(R.Right, R.Bottom);
//              BL := Point(R.Left, R.Bottom);
//              BR := Point(R.Right, R.Bottom);
              WidthOffset := 0;//max(0, BorderWidth div 2);// - 1);

              // Fill horz line1
              if ((PSides = nil) or (asTop in PSides^)) and (R.Right > R.Left) then
                acGdipDrawLine(GpG, GpP, R.Left - WidthOffset, R.Top, R.Right + WidthOffset, R.Top);

              // Fill horz line2
              if ((PSides = nil) or (asBottom in PSides^)) and (R.Right > R.Left) then
                acGdipDrawLine(GpG, GpP, R.Left - WidthOffset, R.Bottom, R.Right + WidthOffset, R.Bottom);

              // Fill vert line1
              if ((PSides = nil) or (asLeft in PSides^)) and (R.Bottom > R.Top) then
                acGdipDrawLine(GpG, GpP, R.Left, R.Top - WidthOffset, R.Left, R.Bottom + WidthOffset);

              // Fill vert line2
              if ((PSides = nil) or (asRight in PSides^)) and (R.Bottom > R.Top) then
                acGdipDrawLine(GpG, GpP, R.Right, R.Top - WidthOffset, R.Right, R.Bottom + WidthOffset);
            end;
          finally
            acGdipDeleteGraphics(GpG);
          end;
      finally
        acGDIEnd(IsLibrary);
      end;
end;


procedure acgpDrawArc(DC: hdc; X, Y, Width, Height, StartAngle, SweepAngle: Single; Color: TColor; PenWidth: Single = 1);
var
  GpP: TGpPen;
  GpG: TGpGraphics;
begin
  if acGDIBegin(IsLibrary) then
    try
      if Assigned(acGdipDrawEllipse) and (acGdipCreateFromHDC(DC, GpG) = Ok) then
        try
          acGdipSetSmoothingMode(GpG, acDefaultSmoothingMode);
          if acGdipCreatePen1($FF000000 or Cardinal(SwapRedBlue(Color)), PenWidth, UnitWorld, GpP) = Ok then begin
            acGdipDrawArc(GpG, GpP, X, Y, Width, Height, StartAngle, SweepAngle);
            acGdipDeletePen(GpP);
          end;
        finally
          acGdipDeleteGraphics(GpG);
        end;
    finally
      acGDIEnd(IsLibrary);
    end;
end;


procedure acgpStretchRect(DstBmp, SrcBmp: TBitmap; SrcX, SrcY, SrcWidth, SrcHeight: integer);
begin
  acgpStretchRect(DstBmp, SrcBmp, MkRect(DstBmp.Width, DstBmp.Height), Rect(SrcX, SrcY, SrcX + SrcWidth, SrcY + SrcHeight));
end;


procedure acgpStretchRect(DstBmp, SrcBmp: TBitmap; DstRect, SrcRect: TRect); overload;
const
  PixelFormat32bppARGB = 2498570;
type
  PGPColorRec = ^TsColor_;
var
//  Status: TStatus;
  GpG: TGpGraphics;
  bitmap: TGpBitmap;
  S0, S: PRGBAArray_;
  bmData: TBitmapData;
  Delta, X, Y: integer;
  gpaPColor: PGPColorRec;
begin
  if (SrcBmp.PixelFormat = pf32bit) and acGDIBegin(IsLibrary) then
    try
      if acGdipCreateFromHDC(DstBmp.Canvas.Handle, GpG) = Ok then begin
        try
          if acGdipCreateBitmapFromScan0(SrcBmp.Width, SrcBmp.Height, SrcBmp.Width * 4, PixelFormat32bppARGB, nil, bitmap) = OK then begin
            try
              if InitLine(SrcBmp, Pointer(S0), Delta) then begin
                acGdipBitmapLockBits(bitmap, nil, 3{ImageLockModeRead or ImageLockModeWrite}, PixelFormat32bppARGB, @bmData);
                for Y := 0 to SrcBmp.Height - 1 do begin
                  S := Pointer(PAnsiChar(S0) + Delta * Y);
                  gpaPColor := Pointer(PAnsiChar(bmData.Scan0) + Y * bmData.Stride);
                  for X := 0 to SrcBmp.Width - 1 do begin
                    gpaPColor.C := S[X].C;
                    gpaPColor.A := 127 + S[X].A shr 1;
                    inc(gpaPColor);
                  end;
                end;
                acGdipBitmapUnlockBits(bitmap, @bmData);
              end;
              if acGdipSetCompositingMode(GpG, CompositingModeSourceCopy) = OK then
                if acGdipSetCompositingQuality(GpG, CompositingQualityHighSpeed) = OK then
                  acGdipDrawImageRectRect(GpG, bitmap, DstRect.Left, DstRect.Top, WidthOf(DstRect), HeightOf(DstRect), SrcRect.Left, SrcRect.Top, WidthOf(SrcRect), HeightOf(SrcRect), UnitPixel, nil, nil, nil);
            finally
              acGdipDisposeImage(bitmap);
            end;
          end;
        finally
          acGdipDeleteGraphics(GpG);
        end;
        if InitLine(DstBmp, Pointer(S0), Delta) then
          for Y := 0 to DstBmp.Height - 1 do begin
            S := Pointer(PAnsiChar(S0) + Delta * Y);
            for X := 0 to DstBmp.Width - 1 do
              with S[X] do
                if A <> 0 then begin
                  R := min(MaxByte, R shl 8 div A);
                  G := min(MaxByte, G shl 8 div A);
                  B := min(MaxByte, B shl 8 div A);
                  A := max(0, (A + 1) shl 1 - 257);
                end;
          end;
      end;
    finally
      acGDIEnd(IsLibrary);
    end;
end;


const
  BlurEffectGuid: TGUID = '{633C80A4-1843-482b-9EF2-BE2834C5FDD4}';

type
  TBlurParams = record
    Radius: Single;
    ExpandEdge: BOOL;
  end;
  PBlurParams = ^TBlurParams;


procedure acgpBlur(Bmp: TBitmap; Radius: Single);
var
  OutBitmap, GPBitmap, OutAux: Pointer;
  NativeInputs: array of Pointer;
  BlurParams: TBlurParams;
  NativeHandle: Pointer;
  OutAuxSize: integer;
  actRadius, muldiv: Single;
  OutBmp: HBitmap;
  SavedBitmap: TBitmap;
  R: TRect;
begin
  if not Bmp.Empty and acGDIBegin(IsLibrary) then
    try
      if Assigned(acGdipCreateEffect) then begin
        actRadius := max(20, Radius);
        if (Radius < 20) and (Bmp.Width < 96) and (Bmp.Height < 96) then begin // Workaround of the BlurEffectGuid bug for small images (glyphs)
          muldiv := actRadius / max(4, Radius);
          SavedBitmap := Bmp;
          Bmp := CreateBmp32(Round(SavedBitmap.Width * muldiv), Round(SavedBitmap.Height * muldiv));
          R := MkRect(Bmp);
          FillRect32(Bmp, R, 255, $0);
          InflateRect(R, -1, -1);
          FillRect32(Bmp, R, $FFFFFF, $0);
          acgpStretchRect(Bmp, SavedBitmap, 0, 0, SavedBitmap.Width, SavedBitmap.Height);
        end
        else begin
          if (Radius > 15) and (Radius < 20) then // Change radius to 20 if near
            actRadius := 20
          else
            actRadius := Radius;

          SavedBitmap := nil;
        end;

        acGdipCreateEffect(BlurEffectGuid, NativeHandle);
        BlurParams.Radius := actRadius;
        BlurParams.ExpandEdge := False;
        acGdipSetEffectParameters(NativeHandle, @BlurParams, SizeOf(BlurParams));
        acGdipCreateBitmapFromHBITMAP(Bmp.Handle, Bmp.Palette, GPBitmap);
        R := MkRect(Bmp);
        if Assigned(acGdipBitmapCreateApplyEffect) then begin
          SetLength(NativeInputs, 1);
          NativeInputs[0] := GPBitmap;
          acGdipBitmapCreateApplyEffect(@NativeInputs[0], 1, NativeHandle, @R, nil, OutBitmap, False, OutAux, OutAuxSize);
          acGdipCreateHBITMAPFromBitmap(OutBitmap, OutBmp, 0);
          Bmp.Handle := OutBmp;
          acGdipDisposeImage(OutBitmap);
        end;
        acGdipDisposeImage(GPBitmap);

        if SavedBitmap <> nil then begin
          acgpStretchRect(SavedBitmap, Bmp, 0, 0, Bmp.Width, Bmp.Height);
          Bmp.Free;
        end;
      end
      else
        QBlur(Bmp);
    finally
      acGDIEnd(IsLibrary);
    end
end;


function acgpCreateLineBrush(DC: hdc; Point1, Point2: TPoint; Color1, Color2: TColor): Pointer;
var
  p1, p2: TacPointF;
begin
  if acGDIBegin(IsLibrary) then
    try
      p1.X := Point1.X;
      p1.Y := Point1.Y;
      p2.X := Point2.X;
      p2.Y := Point2.Y;
      acGdipCreateLineBrush(@p1, @p2, Color1, Color2, WrapModeTileFlipXY, Result);
    finally
      acGDIEnd(IsLibrary);
    end;
end;


procedure acgpFillRect(DC: hdc; Color: TColor; R: TRect; AlphaValue: integer);
var
  GpG: TGpGraphics;
  GpB: Pointer;
begin
  if acGDIBegin(IsLibrary) then
    try
      if acGdipCreateFromHDC(DC, GpG) = Ok then
        try
          if acGdipCreateSolidFill(Cardinal(AlphaValue) shl 24 or Cardinal(SwapRedBlue(Color)), GpB) = Ok then begin
            acGdipFillRectangle(GpG, GpB, R.Left, R.Top, WidthOf(R), HeightOf(R));
            acGdipDeleteBrush(GpB);
          end;
        finally
          acGdipDeleteGraphics(GpG);
        end;
    finally
      acGDIEnd(IsLibrary);
    end;
end;


procedure acgpFillRectangle(DC: hdc; Brush: Pointer; X, Y, Width, Height: Single);
var
  GpG: TGpGraphics;
begin
  if acGDIBegin(IsLibrary) then
    try
      if acGdipCreateFromHDC(DC, GpG) = Ok then
        try
          acGdipFillRectangle(GpG, Brush, X, Y, Width, Height);
        finally
          acGdipDeleteGraphics(GpG);
        end;
    finally
      acGDIEnd(IsLibrary);
    end;
end;


procedure acgpGradientRing(DC: hdc; X, Y, Width, Height: Single; Color1, Color2: TColor; PenWidth: Single = 1);
var
  GpP: TGpPen;
  p1, p2: TacPointF;
  GpG: TGpGraphics;
  Brush: TGpBrush;
begin
  if acGDIBegin(IsLibrary) then
    try
      if Assigned(acGdipDrawEllipse) and (acGdipCreateFromHDC(DC, GpG) = Ok) then
        try
          p1.X := X;
          p1.Y := Y - PenWidth;
          p2.X := X;
          p2.Y := Y + Height + PenWidth;
          acGdipCreateLineBrush(@p1, @p2, Cardinal(SwapRedBlue(Color1)), Cardinal(SwapRedBlue(Color2)), WrapModeTile, Brush);
          if acGdipCreatePen1($FFFFFFFF, PenWidth, UnitWorld, GpP) = Ok then
            if acSetPenBrushFill(GpP, Brush) = Ok then begin;
              acGdipSetSmoothingMode(GpG, acDefaultSmoothingMode);
              acGdipDrawEllipse(GpG, GpP, X, Y, Width, Height);
              acGdipDeletePen(GpP);
            end;
        finally
          acGdipDeleteBrush(Brush);
          acGdipDeleteGraphics(GpG);
        end;
    finally
      acGDIEnd(IsLibrary);
    end;
end;


procedure acgpGradientEllipse(DC: hdc; X, Y, Width, Height: Single; Color1, Color2: TColor);
var
  GpG: TGpGraphics;
  p1, p2: TacPointF;
  Brush: Pointer;
begin
  if acGDIBegin(IsLibrary) then
    try
      if acGdipCreateFromHDC(DC, GpG) = Ok then
        try
          acGdipSetSmoothingMode(GpG, acDefaultSmoothingMode);
          p1.X := X;
          p1.Y := Y;
          p2.X := X;
          p2.Y := Y + Height;
          acGdipCreateLineBrush(@p1, @p2, Cardinal(SwapRedBlue(Color1)), Cardinal(SwapRedBlue(Color2)), WrapModeTile, Brush);
          acGdipFillEllipse(GpG, Brush, X, Y, Width, Height);
        finally
          acGdipDeleteBrush(Brush);
          acGdipDeleteGraphics(GpG);
        end;
    finally
      acGDIEnd(IsLibrary);
    end;
end;


procedure acgpGradientRectangleH(DC: hdc; X, Y, Width, Height: Single; Color1, Color2: TColor);
var
  GpG: TGpGraphics;
  p1, p2: TacPointF;
  Brush: Pointer;
begin
  if acGDIBegin(IsLibrary) then
    try
      if acGdipCreateFromHDC(DC, GpG) = Ok then
        try
          p1.X := X;
          p1.Y := Y;
          p2.X := X + Width;
          p2.Y := Y;
          acGdipCreateLineBrush(@p1, @p2, Cardinal(SwapRedBlue(Color1)), Cardinal(SwapRedBlue(Color2)), WrapModeTile, Brush);
          acGdipFillRectangle(GpG, Brush, X, Y, Width, Height);
        finally
          acGdipDeleteBrush(Brush);
          acGdipDeleteGraphics(GpG);
        end;
    finally
      acGDIEnd(IsLibrary);
    end;
end;


procedure acgpGradientRectangleV(DC: hdc; X, Y, Width, Height: Single; Color1, Color2: TColor);
var
  GpG: TGpGraphics;
  p1, p2: TacPointF;
  Brush: Pointer;
begin
  if acGDIBegin(IsLibrary) then
    try
      if acGdipCreateFromHDC(DC, GpG) = Ok then
        try
          p1.X := X;
          p1.Y := Y;
          p2.X := X;
          p2.Y := Y + Height;
          acGdipCreateLineBrush(@p1, @p2, Cardinal(SwapRedBlue(Color1)), Cardinal(SwapRedBlue(Color2)), WrapModeTile, Brush);
          acGdipFillRectangle(GpG, Brush, X, Y, Width, Height);
        finally
          acGdipDeleteBrush(Brush);
          acGdipDeleteGraphics(GpG);
        end;
    finally
      acGDIEnd(IsLibrary);
    end;
end;


procedure acgpGradientRectangle(DC: hdc; X, Y, Width, Height: Single; const Points: TGradPoints; const Colors: TGradColors; const CenterColor: TsColor);
var
  GpG: TGpGraphics;
  Brush: Pointer;
  GpPoints: array [0..3] of TacPointF;
  GpColors: array [0..3] of ARGB;
  c, i: integer;
begin
  if acGDIBegin(IsLibrary) then
    try
      if acGdipCreateFromHDC(DC, GpG) = Ok then
        try
          for i := 0 to 3 do begin
            GpPoints[i].X := Points[i].X;
            GpPoints[i].Y := Points[i].Y;
            GpColors[i] := DWORD(SwapRedBlue(Colors[i].I));
          end;

          acGdipCreatePathGradient(@GpPoints[0], Length(GpPoints), WrapModeClamp, Brush);
          acGdipSetPathGradientCenterColor(Brush, Cardinal(SwapRedBlue(CenterColor.I)));
          c := 4;
          acGdipSetPathGradientSurroundColorsWithCount(Brush, @GpColors[0], c);
          acGdipFillRectangle(GpG, Brush, X, Y, Width, Height);
        finally
          acGdipDeleteBrush(Brush);
          acGdipDeleteGraphics(GpG);
        end;
    finally
      acGDIEnd(IsLibrary);
    end;
end;


function acgpGetFontName(const FileName: acString): acString;
var
  numfound: Integer;
  nativeFontCollection: Pointer;
  nativeFamilyList: array of Pointer;
  str: array[0..LF_FACESIZE - 1] of WideChar;
  ws: WideString;
begin
  Result := '';
{$WARNINGS OFF}
  if acGdipNewPrivateFontCollection(nativeFontCollection) = OK then begin
    ws := FileName;
    if acGdipPrivateAddFontFile(nativeFontCollection, PWideChar(ws)) = OK then begin
      numFound := 0;
      SetLength(nativeFamilyList, 1);
      if acGdipGetFontCollectionFamilyList(nativeFontCollection, 1, nativeFamilyList, numFound) = OK then
        if (Length(nativeFamilyList) > 0) and (acGdipGetFamilyName(nativeFamilyList[0], PWideChar(@str), 0) = OK) then
          Result := str;
    end;
  end;
end;


procedure acgpDrawPolygon(DC: hdc; Color: TColor; Points: PacPointF; PointCount: integer; LineWidth: integer = 1);
var
  C: TsColor;
  GpP: TGpPen;
  GpG: TGpGraphics;
begin
  if acGDIBegin(IsLibrary) then
    try
      if acGdipCreateFromHDC(DC, GpG) = Ok then
        try
          acGdipSetSmoothingMode(GpG, acDefaultSmoothingMode);
          C.I := SwapRedBlue(Color);
          if C.A = 0 then
            C.I := integer($FF000000 or Cardinal(C.C));

          if acGdipCreatePen1(Cardinal(C.I), LineWidth, UnitWorld, GpP) = Ok then begin
            acGdipDrawPolygon(GpG, GpP, Points, PointCount);
            acGdipDeletePen(GpP);
          end;
        finally
          acGdipDeleteGraphics(GpG);
        end;
    finally
      acGDIEnd(IsLibrary);
    end;
end;


procedure acgpDrawBeziers(DC: hdc; Color: TColor; Points: PacPointF; PointCount: integer; LineWidth: integer = 1);
var
  C: TsColor;
  GpP: TGpPen;
  GpG: TGpGraphics;
begin
  if acGDIBegin(IsLibrary) then
    try
      if acGdipCreateFromHDC(DC, GpG) = Ok then
        try
          acGdipSetSmoothingMode(GpG, acDefaultSmoothingMode);
          C.I := SwapRedBlue(Color);
          if C.A = 0 then
            C.I := integer($FF000000 or Cardinal(C.C));

          if acGdipCreatePen1(Cardinal(C.I), LineWidth, UnitWorld, GpP) = Ok then begin
            acGdipDrawBeziers(GpG, GpP, Points, PointCount);
            acGdipDeletePen(GpP);
          end;
        finally
          acGdipDeleteGraphics(GpG);
        end;
    finally
      acGDIEnd(IsLibrary);
    end;
end;


procedure acgpFillPolygon(DC: hdc; Color: TColor; Points: PPoint; PointCount: integer);
var
  pps: TacGpPoints;
  i: integer;
begin
  SetLength(pps, PointCount);
  for i := 0 to PointCount - 1 do
    with Points^ do begin
      pps[i].X := X;
      pps[i].Y := Y;
      inc(Points);
    end;

  acgpFillPolygon(DC, Color, PacPointF(@pps[0]), PointCount);
end;


procedure acgpFillPolygon(DC: hdc; Color: TColor; Points: PacPointF; PointCount: integer);
var
  C: TsColor;
  GpB: TGpBrush;
  GpG: TGpGraphics;
begin
  if acGDIBegin(IsLibrary) then
    try
      if acGdipCreateFromHDC(DC, GpG) = Ok then
        try
          acGdipSetSmoothingMode(GpG, acDefaultSmoothingMode);
          C.I := SwapRedBlue(Color);
          if C.A = 0 then
            C.I := integer($FF000000 or Cardinal(C.C));

          if acGdipCreateSolidFill(Cardinal(C.C){FF000000 or Cardinal(SwapRedBlue(Color))}, GpB) = Ok then begin
            acGdipFillPolygon(GpG, GpB, Points, PointCount, 1);
            acGdipDeletePen(GpB);
          end;
        finally
          acGdipDeleteGraphics(GpG);
        end;
    finally
      acGDIEnd(IsLibrary);
    end;
end;


procedure acgpGradientPolygon(DC: hdc; Color1, Color2: TColor; BeginSide: TacSide; Points: PPoint; PointCount: integer);
var
  pps: TacGpPoints;
  i: integer;
  ps: TPoints;
  C1, C2: TsColor;
  Brush: TGpBrush;
  GpG: TGpGraphics;
  p1, p2: TacPointF;
  MinX, MaxX, MinY, MaxY: Single;
begin
  if acGDIBegin(IsLibrary) then
    try
      if acGdipCreateFromHDC(DC, GpG) = Ok then
        try
          acGdipSetSmoothingMode(GpG, acDefaultSmoothingMode);
          if TsColor(Color1).A = 0 then
            C1.I := integer($FF000000 or Cardinal(Color1))
          else
            C1.I := SwapRedBlue(Color1);

          if TsColor(Color2).A = 0 then
            C2.I := integer($FF000000 or Cardinal(Color2))
          else
            C2.I := SwapRedBlue(Color2);

          ps := PPoints(PAnsiChar(Points))^;
          SetLength(pps, PointCount);
          MinX := MaxInt;
          MinY := MaxInt;
          for i := 0 to PointCount - 1 do
            with Points^ do begin
              pps[i].X := X;
              pps[i].Y := Y;
              if MinX > X then
                MinX := X;

              if MinY > Y then
                MinY := Y;

              if MaxX < X then
                MaxX := X;

              if MaxY < Y then
                MaxY := Y;

              inc(Points);
            end;

          // Create gradient
          case BeginSide of
            asLeft: begin
              p1.X := MinX;
              p1.Y := MinY;
              p2.X := MinX;
              p2.Y := MaxY;
            end;
            asTop: begin
              p1.X := MinX;
              p1.Y := MinY;
              p2.X := MinX;
              p2.Y := MaxY;
            end;
            asRight: begin
              p1.X := MinX;
              p1.Y := MinY;
              p2.X := MaxX;
              p2.Y := MinY;
            end
            else {asLeft} begin
              p1.X := MinX;
              p1.Y := MinY;
              p2.X := MaxX;
              p2.Y := MinY;
            end;
          end;
          if acGdipCreateLineBrush(@p1, @p2, Cardinal(C1.C), Cardinal(C2.C), WrapModeTile, Brush) = Ok then begin
            acGdipFillPolygon(GpG, Brush, @pps[0], PointCount, 1);
            acGdipDeleteBrush(Brush);
          end;
        finally
          acGdipDeleteGraphics(GpG);
        end;
    finally
      acGDIEnd(IsLibrary);
    end;
end;


procedure acgpRotatePoints(ARect: PacGpPoints; const ACenter: TacPointF; ACount, Angle: integer);
var
  T: TacPointF;
  S, C, X, Y: Single;
  I: Integer;
begin
  for i := 0 to ACount - 1 do begin
    T.X := ARect^[i].X - ACenter.X;
    T.Y := ARect^[i].Y - ACenter.Y;
    S := acSin(Angle);
    C := acCos(Angle);

    X :=  T.X * C + T.Y * S;
    Y := -T.X * S + T.Y * C;

    ARect^[i].X := ACenter.X + X;
    ARect^[i].Y := ACenter.Y + Y;
  end;
end;


procedure acgpDrawRotatedBitmap(DC: hdc; SrcBmp: TBitmap; R: TRect; Angle: integer);
Var
  GpG: TGpGraphics;
  GPBitmap: Pointer;
  Center: TacPointF;
  Points: TacGpPoints;
begin
  Angle := 360 - Angle;
  if Angle < 0 then
    inc(Angle, 360)
  else
    if Angle >= 360 then
      dec(Angle, 360);

  SetLength(Points, 4);
  Points[0].X := R.Left;
  Points[0].Y := R.Top;

  Points[1].X := R.Left + SrcBmp.Width;
  Points[1].Y := R.Top;

  Points[2].X := R.Left;
  Points[2].Y := R.Top + SrcBmp.Height;

  Points[3].X := R.Left + SrcBmp.Width;
  Points[3].Y := R.Top + SrcBmp.Height;

  Center.X := (R.Left + R.Right) / 2;
  Center.Y := (R.Top + R.Bottom) / 2;
  acgpRotatePoints(@Points, Center, 4, Angle);

  if acGDIBegin(IsLibrary) then
    try
      if acGdipCreateFromHDC(DC, GpG) = Ok then
        try
          acGdipSetInterpolationMode(GpG, InterpolationModeHighQuality);
          acGdipCreateBitmapFromHBITMAP(SrcBmp.Handle, SrcBmp.Palette, GPBitmap);
          acGdipDrawImagePoints(GpG, GPBitmap, PacGpPoints(@Points[0]), 3);
        finally
          acGdipDeleteGraphics(GpG);
        end;
    finally
      acGDIEnd(IsLibrary);
    end;
end;

{$IFDEF GDIPLUS_PNG}
procedure acgpBmpFromStream(Bmp: TBitmap; Stream: TStream);
var
  Png: TGPBitmap;
  OutBmp: HBITMAP;
  StreamAdapter: IStream;
begin
  if acGDIBegin(IsLibrary) then
    try
      StreamAdapter := TStreamAdapter.Create(Stream);
      Png := nil;
      OutBmp := 0;
      if acGdipCreateBitmapFromStream(StreamAdapter, Png) = Ok then begin
        if acGdipCreateHBITMAPFromBitmap(Png, OutBmp, 0) = Ok then
          Bmp.Handle := OutBmp;

        acGdipDisposeImage(Png);
      end;
    finally
      acGDIEnd(IsLibrary);
    end;
end;
{$ENDIF} // GDIPLUS_PNG


function acgpDrawString(DC: hdc; Text: WideString; aRect: TRect; Color: TColor; Alignment: TAlignment; VertAlignment: TVerticalAlignment; Angle: integer; AlphaLayer: boolean = False; FontName: WideString = ''): boolean;
var
  Font: Pointer;
  Center: TPoint;
  FontFamily, Format: Pointer;
  Brush: TGpBrush;
  GpG: TGpGraphics;
  outR, R: TGPRectF;
  RenderType: TTextRenderingHint;
begin
  Result := False;
  if acGDIBegin(IsLibrary) then
    try
      if acGdipCreateFromHDC(DC, GpG) = Ok then
        try
          if acGdipCreateStringFormat(0, 0, Format) = Ok then begin
            try
              if FontName <> '' then begin
                if acGdipCreateFontFamilyFromName(PWideChar(FontName), nil, FontFamily) = Ok then begin
                  if acGdipCreateFont(FontFamily, 32{Canvas.Font.Size}, 0, 0, Font) <> Ok then
                    Font := nil;
                end
                else
                  Font := nil;
              end
              else
                if acGdipCreateFontFromDC(DC, Font) <> Ok then
                  Font := nil;

              if Font <> nil{acGdipCreateFontFromDC(DC, Font) = Ok} then // Not TrueType fonts may be not supported, paint using GDI then
                try
                  if Color and $FF000000 = 0 then
                    Color := TColor(Color or $FF000000); // "TColor(" - Prevent an error under Win64

                  if acGdipCreateSolidFill(ARGB(Color), Brush) = Ok then begin
                    R.X := aRect.Left;
                    R.Y := aRect.Top;
                    R.Width := WidthOf(aRect) + 1 {Compensation for rounding};
                    R.Height := HeightOf(aRect) + 1;
                    if Angle <> 0 then begin
                      acGdipMeasureString(GpG, PWideChar(Text), Length(Text), Font, @R, Format, @outR, nil, nil);
                      case Alignment of
                        taCenter: begin
                          R.Width := outR.Width;
                          R.Height := outR.Height;
                          R.X := R.X - R.Width / 2;
                          R.Y := R.Y - R.Height / 2;
                          Center := Point(aRect.Left + WidthOf(aRect) div 2, aRect.Top + HeightOf(aRect) div 2);
                        end;

                        taLeftJustify: begin
                          R.Width := outR.Width;
                          R.Height := outR.Height + 60;
                          Center := Point(0, 0);
                        end;
                      end;
                      acGdipTranslateWorldTransform(GpG, Center.X, Center.Y, MatrixOrderPrepend);
                      acGdipRotateWorldTransform(GpG, -Angle, MatrixOrderPrepend);
                    end
                    else
                      acGdipSetStringFormatLineAlign(Format, acVertAlignArray[VertAlignment]);

                    acGdipSetStringFormatAlign(Format, acAlignArray[Alignment]);

                    if AlphaLayer then
                      RenderType := TextRenderingHintAntiAliasGridFit
                    else
                      RenderType := TextRenderingHintClearTypeGridFit;

                    acGdipSetTextRenderingHint(GpG, RenderType);

                    acGdipDrawString(GpG, PWideChar(Text), Length(Text), Font, @R, Format, Brush);
                    acGdipDeleteBrush(Brush);
                    Result := True;
                  end;
                finally
                  acGdipDeleteFont(Font);
                end;
            finally
              acGdipDeleteStringFormat(Format);
            end;
          end;
        finally
          acGdipDeleteGraphics(GpG);
        end;
    finally
      acGDIEnd(IsLibrary);
    end;
end;

(*
procedure acgpTextOut(DC: hdc; X, Y: integer; aRect: TRect; Text: acString; Color: TColor; Angle: integer);
var
  GpG: TGpGraphics;
  Format: Pointer;
  Font: Pointer;
  Brush: TGpBrush;
  outR, R: TGPRectF;
  Center: TPoint;
begin
  if acGDIBegin(IsLibrary) then
    try
      if acGdipCreateFromHDC(DC, GpG) = Ok then
        try
          if acGdipCreateStringFormat({ $00004000{NoClip or $00001000{NoWrap}0, 0, Format) = Ok then
            try
              if acGdipCreateFontFromDC(DC, Font) = Ok then
                try
                  if acGdipCreateSolidFill($FF000000 or Color, Brush) = Ok then begin
                    R.Width := WidthOf(aRect);
                    R.Height := HeightOf(aRect);
                    R.X := R.X - R.Width / 2;
                    R.Y := R.Y - R.Height / 2;
                    Center := Point(aRect.Left + WidthOf(aRect) div 2, aRect.Top + HeightOf(aRect) div 2);
                    acGdipTranslateWorldTransform(GpG, Center.X, Center.Y, MatrixOrderPrepend);
                    acGdipRotateWorldTransform(GpG, -Angle, MatrixOrderPrepend);

                    acGdipSetTextRenderingHint(GpG, TextRenderingHintClearTypeGridFit);

                    acGdipDrawString(GpG, PWideChar(Text), Length(Text), Font, @R, Format, Brush);
                    acGdipDeleteBrush(Brush);
                  end;
                finally
                  acGdipDeleteFont(Font);
                end;
            finally
              acGdipDeleteStringFormat(Format);
            end;
        finally
          acGdipDeleteGraphics(GpG);
        end;
    finally
      acGDIEnd(IsLibrary);
    end;
end;
*)

function acgpMeasureString(DC: hdc; Text: acString; Wrapped: boolean; MaxWidth: integer = MaxInt): TSize;
var
  outR, R: TGPRectF;
  GpG: TGpGraphics;
  Format: Pointer;
  Font: Pointer;
  Err: TStatus;
{$IFNDEF WIDETEXT}
  P: PWideChar;
  i: integer;
{$ENDIF}
begin
  Result := MkSize;
  if Text <> '' then
    if acGDIBegin(IsLibrary) then
      try
        if acGdipCreateFromHDC(DC, GpG) = Ok then
          try
            if acGdipCreateStringFormat(0, 0, Format) = Ok then
              try
                Err := acGdipCreateFontFromDC(DC, Font);
                if Err = Ok then
                  try
                    if Wrapped then
                      R.Width := MaxWidth
                    else
                      R.Width := MaxInt;

                    R.Height := 0;
                    R.X := 0;
                    R.Y := 0;
                    if not Wrapped then
                      acGdipSetStringFormatFlags(Format, $00001000 {StringFormatFlagsNoWrap});
{$IFNDEF WIDETEXT}
                    P := PWideChar(PACChar(Text));
                    i := min(Length(P), Length(Text));
                    acGdipMeasureString(GpG, P, i, Font, @R, Format, @outR, nil, nil);
{$ELSE}
                    acGdipMeasureString(GpG, PWideChar(Text), Length(Text), Font, @R, Format, @outR, nil, nil);
{$ENDIF}
                    Result := MkSize(Round(outR.Width) + 1, Round(outR.Height) + 1);
                  finally
                    acGdipDeleteFont(Font);
                  end;
              finally
                acGdipDeleteStringFormat(Format);
              end;
          finally
            acGdipDeleteGraphics(GpG);
          end;
      finally
        acGDIEnd(IsLibrary);
      end;
end;


function acgpMeasureString(DC: hdc; Text: acString; AFont: TFont; Wrapped: boolean; MaxWidth: integer = MaxInt): TSize;
var
  outR, R: TGPRectF;
  GpG: TGpGraphics;
  Format: Pointer;
  Font: Pointer;
begin
  Result := MkSize;
  if acGDIBegin(IsLibrary) then
    try
      if acGdipCreateFromHDC(DC, GpG) = Ok then
        try
          if acGdipCreateStringFormat(0, 0, Format) = Ok then
            try
              if acGdipCreateFontFromDC(DC, Font) = Ok then
                try
                  R.Width := MaxWidth;
                  R.Height := 0;
                  R.X := 0;
                  R.Y := 0;
                  if not Wrapped then
                    acGdipSetStringFormatFlags(Format, $00001000 {StringFormatFlagsNoWrap});

                  acGdipMeasureString(GpG, PWideChar(Text), Length(Text), Font, @R, Format, @outR, nil, nil);
                  Result := MkSize(Round(outR.Width) + 1, Round(outR.Height) + 1);
                finally
                  acGdipDeleteFont(Font);
                end
              else
                Result := GetStringSize(AFont.Handle, Text);
            finally
              acGdipDeleteStringFormat(Format);
            end;
        finally
          acGdipDeleteGraphics(GpG);
        end;
    finally
      acGDIEnd(IsLibrary);
    end;
end;


initialization
  if hGP = 0 then
    hGP := LoadLibrary('gdiplus.dll');

  if (hGP <> 0) and not Assigned(acGdipCreateFromHDC) then begin
    acGdipDrawArc                 := GetProcAddress(hGP, 'GdipDrawArc');
    acGdipFillPie                 := GetProcAddress(hGP, 'GdipFillPie');
    acGdipDrawLine                := GetProcAddress(hGP, 'GdipDrawLine');
    acGdipCreatePen1              := GetProcAddress(hGP, 'GdipCreatePen1');
//    acGdipSetPenBrushFill         := GetProcAddress(hGP, 'GdipSetPenBrushFill');
    acGdiplusStartup              := GetProcAddress(hGP, 'GdiplusStartup');
    acGdiplusShutdown             := GetProcAddress(hGP, 'GdiplusShutdown');
    acGdipDrawEllipse             := GetProcAddress(hGP, 'GdipDrawEllipse');
    acGdipFillEllipse             := GetProcAddress(hGP, 'GdipFillEllipse');
    acGdipFillRectangle           := GetProcAddress(hGP, 'GdipFillRectangle');
    acGdipCreateFromHDC           := GetProcAddress(hGP, 'GdipCreateFromHDC');
    acGdipCreateSolidFill         := GetProcAddress(hGP, 'GdipCreateSolidFill');
    acGdipSetSmoothingMode        := GetProcAddress(hGP, 'GdipSetSmoothingMode');
    acGdipDrawImageRectRect       := GetProcAddress(hGP, 'GdipDrawImageRectRectI');
    acGdipCreateLineBrush         := GetProcAddress(hGP, 'GdipCreateLineBrush');
    acGdipSetCompositingMode      := GetProcAddress(hGP, 'GdipSetCompositingMode');
    acGdipSetCompositingQuality   := GetProcAddress(hGP, 'GdipSetCompositingQuality');
    acGdipDeleteGraphics          := GetProcAddress(hGP, 'GdipDeleteGraphics');
    acGdipDisposeImage            := GetProcAddress(hGP, 'GdipDisposeImage');
    acGdipBitmapLockBits          := GetProcAddress(hGP, 'GdipBitmapLockBits');
    acGdipCreateBitmapFromScan0   := GetProcAddress(hGP, 'GdipCreateBitmapFromScan0');
    acGdipBitmapUnlockBits        := GetProcAddress(hGP, 'GdipBitmapUnlockBits');
    acGdipCreatePathGradient      := GetProcAddress(hGP, 'GdipCreatePathGradient');
    acGdipDeleteBrush             := GetProcAddress(hGP, 'GdipDeleteBrush');
    acGdipDeletePen               := GetProcAddress(hGP, 'GdipDeletePen');
    acGdipCreateEffect            := GetProcAddress(hGP, 'GdipCreateEffect');
    acGdipSetEffectParameters     := GetProcAddress(hGP, 'GdipSetEffectParameters');
    acGdipBitmapCreateApplyEffect := GetProcAddress(hGP, 'GdipBitmapCreateApplyEffect');
    acGdipCreateBitmapFromHBITMAP := GetProcAddress(hGP, 'GdipCreateBitmapFromHBITMAP');
    acGdipCreateHBITMAPFromBitmap := GetProcAddress(hGP, 'GdipCreateHBITMAPFromBitmap');
    acSetPenBrushFill             := GetProcAddress(hGP, 'GdipSetPenBrushFill');
//    acGdipCreatePathGradientFromPath := GetProcAddress(hGP, 'GdipCreatePathGradientFromPath');
    acGdipSetPathGradientCenterColor := GetProcAddress(hGP, 'GdipSetPathGradientCenterColor');
    acGdipSetPathGradientSurroundColorsWithCount := GetProcAddress(hGP, 'GdipSetPathGradientSurroundColorsWithCount');

    acGdipNewPrivateFontCollection    := GetProcAddress(hGP, 'GdipNewPrivateFontCollection');
    acGdipPrivateAddFontFile          := GetProcAddress(hGP, 'GdipPrivateAddFontFile');
    acGdipGetFontCollectionFamilyList := GetProcAddress(hGP, 'GdipGetFontCollectionFamilyList');
    acGdipGetFamilyName               := GetProcAddress(hGP, 'GdipGetFamilyName');

    acGdipDrawPolygon                 := GetProcAddress(hGP, 'GdipDrawPolygon');
    acGdipFillPolygon                 := GetProcAddress(hGP, 'GdipFillPolygon');

    acGdipDrawImagePoints             := GetProcAddress(hGP, 'GdipDrawImagePoints');
    acGdipDrawBeziers                 := GetProcAddress(hGP, 'GdipDrawBeziers');

    acGdipSetInterpolationMode        := GetProcAddress(hGP, 'GdipSetInterpolationMode');
{$IFDEF GDIPLUS_PNG}
    acGdipCreateBitmapFromStream      := GetProcAddress(hGP, 'GdipCreateBitmapFromStream');
{$ENDIF} // GDIPLUS_PNG

    acGdipCreateStringFormat          := GetProcAddress(hGP, 'GdipCreateStringFormat');
    acGdipDeleteStringFormat          := GetProcAddress(hGP, 'GdipDeleteStringFormat');
    acGdipCreateFontFromDC            := GetProcAddress(hGP, 'GdipCreateFontFromDC');
    acGdipCreateFontFamilyFromName    := GetProcAddress(hGP, 'GdipCreateFontFamilyFromName');
    acGdipCreateFont                  := GetProcAddress(hGP, 'GdipCreateFont');
    acGdipDeleteFont                  := GetProcAddress(hGP, 'GdipDeleteFont');
    acGdipDrawString                  := GetProcAddress(hGP, 'GdipDrawString');
    acGdipSetStringFormatAlign        := GetProcAddress(hGP, 'GdipSetStringFormatAlign');
    acGdipSetStringFormatLineAlign    := GetProcAddress(hGP, 'GdipSetStringFormatLineAlign');
    acGdipSetTextRenderingHint        := GetProcAddress(hGP, 'GdipSetTextRenderingHint');
    acGdipMeasureString               := GetProcAddress(hGP, 'GdipMeasureString');
    acGdipSetStringFormatFlags        := GetProcAddress(hGP, 'GdipSetStringFormatFlags');

    acGdipTranslateWorldTransform     := GetProcAddress(hGP, 'GdipTranslateWorldTransform');
    acGdipRotateWorldTransform        := GetProcAddress(hGP, 'GdipRotateWorldTransform');

    acGDIBegin(not IsLibrary);
  end;


finalization
  acGDIEnd(not IsLibrary);
  if hGP <> 0 then
    FreeLibrary(hGP);


end.

