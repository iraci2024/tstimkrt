{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ Express Cross Platform Library classes }
{ }
{ Copyright (c) 2001-2021 Developer Express Inc. }
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
{ ACCOMPANYING VCL AND CLX CONTROLS AS PART OF AN EXECUTABLE }
{ PROGRAM ONLY. }
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

unit dxDocumentLayoutUnitConverter;

{$I cxVer.inc}
{$SCOPEDENUMS ON}

interface

uses
  Types, dxCoreClasses, cxGeometry, dxCoreGraphics, dxGDIPlusClasses;

type

  TdxDocumentLayoutUnit = (Document, Twip, Pixel);

  TdxLayoutGraphicsUnit = (Pixel = 2, Point = 3, Document = 5);

  { TdxDocumentModelDpi }

  TdxDocumentModelDpi = class
  public
    class function DpiX: Single; static;
    class function DpiY: Single; static;
    class function Dpi: Single; static;
  end;

  { TdxDpiSupport }

  TdxDpiSupport = class abstract(TcxIUnknownObject)
  strict private
    FScreenDpiX: Single;
    FScreenDpiY: Single;
    FScreenDpi: Single;
  public
    constructor Create(AScreenDpiX, AScreenDpiY, AScreenDpi: Single); overload;
    constructor Create(AScreenDpiX, AScreenDpiY: Single); overload;
    constructor Create; overload;

    property ScreenDpiX: Single read FScreenDpiX;
    property ScreenDpiY: Single read FScreenDpiY;
    property ScreenDpi: Single read FScreenDpi;
  end;

  { TdxGraphicsDpi }

  TdxGraphicsDpi = class
  public const
    Display = 75.0;
    Inch = 1.0;
    Document = 300.0;
    Millimeter = 25.4;
    Point = 72.0;
    HundredthsOfAnInch = 100.0;
    TenthsOfAMillimeter = 254.0;
    Twips = 1440.0;
    EMU = 914400.0;
    DeviceIndependentPixel = 96.0;
  strict private
    class var FPixel: Single;
    class constructor Initialize;
  public
    class function GetGraphicsDpi(AGraphics: TdxGPCanvas): Single; static;
    class function UnitToDpi(AUnit: TdxGraphicUnit): Single; static;
    class function DpiToUnit(ADpi: Single): TdxGraphicUnit; static;

    class property Pixel: Single read FPixel;
  end;

  { TdxDocumentLayoutUnitConverter }

  TdxDocumentLayoutUnitConverter = class abstract(TdxDpiSupport)
  protected
    function GetDpi: Single; virtual; abstract;
    function GetFontSizeScale: Single; virtual; abstract;
    function GetFontSizeScaleForPrinting: Single; virtual;
    function GetFontUnit: TdxLayoutGraphicsUnit; virtual; abstract;
    function GetGraphicsPageScale: Single; virtual; abstract;
    function GetGraphicsPageUnit: TdxLayoutGraphicsUnit; virtual; abstract;
  public
    function Equals(Obj: TObject): Boolean; override;
    class function CreateConverter(const AUnit: TdxDocumentLayoutUnit;
      const ADpi: Single): TdxDocumentLayoutUnitConverter;

    function DocumentsToFontUnitsF(const AValue: Single): Single;
      virtual; abstract;
    function DocumentsToLayoutUnits(const AValue: Integer): Integer; overload;
      virtual; abstract;
    function DocumentsToLayoutUnits(const AValue: TdxRectF): TdxRectF; overload;
      virtual; abstract;
    function DocumentsToLayoutUnits(const AValue: TRect): TRect; overload;
      virtual; abstract;
    function InchesToFontUnitsF(const AValue: Single): Single; virtual;
      abstract;
    function LayoutUnitsToDocuments(const AValue: TdxRectF): TdxRectF; overload;
      virtual; abstract;
    function LayoutUnitsToDocuments(const AValue: TRect): TRect; overload;
      virtual; abstract;
    function LayoutUnitsToHundredthsOfInch(const AValue: Integer): Integer;
      overload; virtual; abstract;
    function LayoutUnitsToHundredthsOfInch(const AValue: TSize): TSize;
      overload; virtual; abstract;
    function LayoutUnitsToPixels(const AValue: Integer; const ADpi: Single)
      : Integer; overload; virtual; abstract;
    function LayoutUnitsToPixels(const AValue: Integer): Integer; overload;
    function LayoutUnitsToPixels(const AValue: TSize): TSize; overload;
    function LayoutUnitsToPixels(const AValue: TSize;
      const ADpiX, ADpiY: Single): TSize; overload; virtual; abstract;
    function LayoutUnitsToPixels(const AValue: TPoint;
      const ADpiX, ADpiY: Single): TPoint; overload; virtual; abstract;
    function LayoutUnitsToPixels(const AValue: TRect;
      const ADpiX, ADpiY: Single): TRect; overload; virtual; abstract;
    function LayoutUnitsToPixelsF(const AValue: Single; const ADpi: Single)
      : Single; virtual; abstract;
    function LayoutUnitsToPointsF(const AValue: Single): Single;
      virtual; abstract;
    function LayoutUnitsToTwips(const AValue: Int64): Int64; overload;
      virtual; abstract;
    function LayoutUnitsToTwips(const AValue: Integer): Integer; overload;
      virtual; abstract;
    function MillimetersToFontUnitsF(const AValue: Single): Single;
      virtual; abstract;
    function PixelsToLayoutUnits(const AValue: Integer): Integer; overload;
    function PixelsToLayoutUnits(const AValue: Integer; const ADpi: Single)
      : Integer; overload; virtual; abstract;
    function PixelsToLayoutUnits(const AValue: TPoint;
      const ADpiX, ADpiY: Single): TPoint; overload; virtual;
    function PixelsToLayoutUnits(const AValue: TSize;
      const ADpiX, ADpiY: Single): TSize; overload; virtual; abstract;
    function PixelsToLayoutUnits(const AValue: TRect): TRect; overload;
    function PixelsToLayoutUnits(const AValue: TRect;
      const ADpiX, ADpiY: Single): TRect; overload; virtual; abstract;
    function PixelsToLayoutUnitsF(const AValue: Single; const ADpi: Single)
      : Single; virtual; abstract;
    function PointsToFontUnits(const AValue: Integer): Integer;
      virtual; abstract;
    function PointsToFontUnitsF(const AValue: Single): Single; virtual;
      abstract;
    function PointsToLayoutUnits(const AValue: Integer): Integer;
      virtual; abstract;
    function PointsToLayoutUnitsF(const AValue: Single): Single;
      virtual; abstract;
    function SnapToPixels(const AValue: Integer; const ADpi: Single): Integer;
      virtual; abstract;
    function TwipsToLayoutUnits(const AValue: Int64): Int64; overload;
      virtual; abstract;
    function TwipsToLayoutUnits(const AValue: Integer): Integer; overload;
      virtual; abstract;
    property Dpi: Single read GetDpi;
    property FontSizeScale: Single read GetFontSizeScale;
    property FontSizeScaleForPrinting: Single read GetFontSizeScaleForPrinting;
    // # VCL: for the support twips in PS link
    property FontUnit: TdxLayoutGraphicsUnit read GetFontUnit;
    property GraphicsPageScale: Single read GetGraphicsPageScale;
    property GraphicsPageUnit: TdxLayoutGraphicsUnit read GetGraphicsPageUnit;
  end;

  { TdxDocumentLayoutUnitPixelsConverter }

  TdxDocumentLayoutUnitPixelsConverter = class sealed
    (TdxDocumentLayoutUnitConverter)
  private
    FDpi: Single;
  protected
    function GetDpi: Single; override;
    function GetFontSizeScale: Single; override;
    function GetFontUnit: TdxLayoutGraphicsUnit; override;
    function GetGraphicsPageScale: Single; override;
    function GetGraphicsPageUnit: TdxLayoutGraphicsUnit; override;
  public
    constructor Create(ADpi: Single);
    function DocumentsToFontUnitsF(const AValue: Single): Single; override;
    function DocumentsToLayoutUnits(const AValue: Integer): Integer;
      overload; override;
    function DocumentsToLayoutUnits(const AValue: TdxRectF): TdxRectF;
      overload; override;
    function DocumentsToLayoutUnits(const AValue: TRect): TRect;
      overload; override;
    function InchesToFontUnitsF(const AValue: Single): Single; override;
    function LayoutUnitsToDocuments(const AValue: TdxRectF): TdxRectF;
      overload; override;
    function LayoutUnitsToDocuments(const AValue: TRect): TRect;
      overload; override;
    function LayoutUnitsToHundredthsOfInch(const AValue: Integer): Integer;
      overload; override;
    function LayoutUnitsToHundredthsOfInch(const AValue: TSize): TSize;
      overload; override;
    function LayoutUnitsToPixels(const AValue: Integer; const ADpi: Single)
      : Integer; overload; override;
    function LayoutUnitsToPixels(const AValue: TSize;
      const ADpiX, ADpiY: Single): TSize; overload; override;
    function LayoutUnitsToPixels(const AValue: TPoint;
      const ADpiX, ADpiY: Single): TPoint; overload; override;
    function LayoutUnitsToPixels(const AValue: TRect;
      const ADpiX, ADpiY: Single): TRect; overload; override;
    function LayoutUnitsToPixelsF(const AValue: Single; const ADpi: Single)
      : Single; override;
    function LayoutUnitsToPointsF(const AValue: Single): Single; override;
    function LayoutUnitsToTwips(const AValue: Int64): Int64; overload; override;
    function LayoutUnitsToTwips(const AValue: Integer): Integer;
      overload; override;
    function MillimetersToFontUnitsF(const AValue: Single): Single; override;
    function PixelsToLayoutUnits(const AValue: Integer; const ADpi: Single)
      : Integer; overload; override;
    function PixelsToLayoutUnits(const AValue: TSize;
      const ADpiX, ADpiY: Single): TSize; overload; override;
    function PixelsToLayoutUnits(const AValue: TRect;
      const ADpiX, ADpiY: Single): TRect; overload; override;
    function PixelsToLayoutUnitsF(const AValue: Single; const ADpi: Single)
      : Single; override;
    function PointsToFontUnits(const AValue: Integer): Integer; override;
    function PointsToFontUnitsF(const AValue: Single): Single; override;
    function PointsToLayoutUnits(const AValue: Integer): Integer; override;
    function PointsToLayoutUnitsF(const AValue: Single): Single; override;
    function SnapToPixels(const AValue: Integer; const ADpi: Single)
      : Integer; override;
    function TwipsToLayoutUnits(const AValue: Int64): Int64; overload; override;
    function TwipsToLayoutUnits(const AValue: Integer): Integer;
      overload; override;
  end;

  { TdxDocumentLayoutUnitTwipsConverter }

  TdxDocumentLayoutUnitTwipsConverter = class sealed
    (TdxDocumentLayoutUnitConverter)
  protected
    function GetDpi: Single; override;
    function GetFontSizeScale: Single; override;
    function GetFontSizeScaleForPrinting: Single; override;
    function GetFontUnit: TdxLayoutGraphicsUnit; override;
    function GetGraphicsPageScale: Single; override;
    function GetGraphicsPageUnit: TdxLayoutGraphicsUnit; override;
  public
    function DocumentsToFontUnitsF(const AValue: Single): Single; override;
    function DocumentsToLayoutUnits(const AValue: Integer): Integer;
      overload; override;
    function DocumentsToLayoutUnits(const AValue: TdxRectF): TdxRectF;
      overload; override;
    function DocumentsToLayoutUnits(const AValue: TRect): TRect;
      overload; override;
    function InchesToFontUnitsF(const AValue: Single): Single; override;
    function LayoutUnitsToDocuments(const AValue: TdxRectF): TdxRectF;
      overload; override;
    function LayoutUnitsToDocuments(const AValue: TRect): TRect;
      overload; override;
    function LayoutUnitsToHundredthsOfInch(const AValue: Integer): Integer;
      overload; override;
    function LayoutUnitsToHundredthsOfInch(const AValue: TSize): TSize;
      overload; override;
    function LayoutUnitsToPixels(const AValue: Integer; const ADpi: Single)
      : Integer; overload; override;
    function LayoutUnitsToPixels(const AValue: TSize;
      const ADpiX, ADpiY: Single): TSize; overload; override;
    function LayoutUnitsToPixels(const AValue: TPoint;
      const ADpiX, ADpiY: Single): TPoint; overload; override;
    function LayoutUnitsToPixels(const AValue: TRect;
      const ADpiX, ADpiY: Single): TRect; overload; override;
    function LayoutUnitsToPixelsF(const AValue: Single; const ADpi: Single)
      : Single; override;
    function LayoutUnitsToPointsF(const AValue: Single): Single; override;
    function LayoutUnitsToTwips(const AValue: Int64): Int64; overload; override;
    function LayoutUnitsToTwips(const AValue: Integer): Integer;
      overload; override;
    function MillimetersToFontUnitsF(const AValue: Single): Single; override;
    function PixelsToLayoutUnits(const AValue: Integer; const ADpi: Single)
      : Integer; overload; override;
    function PixelsToLayoutUnits(const AValue: TSize;
      const ADpiX, ADpiY: Single): TSize; overload; override;
    function PixelsToLayoutUnits(const AValue: TRect;
      const ADpiX, ADpiY: Single): TRect; overload; override;
    function PixelsToLayoutUnitsF(const AValue: Single; const ADpi: Single)
      : Single; override;
    function PointsToFontUnits(const AValue: Integer): Integer; override;
    function PointsToFontUnitsF(const AValue: Single): Single; override;
    function PointsToLayoutUnits(const AValue: Integer): Integer; override;
    function PointsToLayoutUnitsF(const AValue: Single): Single; override;
    function SnapToPixels(const AValue: Integer; const ADpi: Single)
      : Integer; override;
    function TwipsToLayoutUnits(const AValue: Int64): Int64; overload; override;
    function TwipsToLayoutUnits(const AValue: Integer): Integer;
      overload; override;
  end;

  { TdxDocumentLayoutUnitDocumentConverter }

  TdxDocumentLayoutUnitDocumentConverter = class sealed
    (TdxDocumentLayoutUnitConverter)
  public const
    DefaultDpi = 300;
  protected
    function GetDpi: Single; override;
    function GetFontSizeScale: Single; override;
    function GetFontUnit: TdxLayoutGraphicsUnit; override;
    function GetGraphicsPageScale: Single; override;
    function GetGraphicsPageUnit: TdxLayoutGraphicsUnit; override;
  public
    function DocumentsToFontUnitsF(const AValue: Single): Single; override;
    function DocumentsToLayoutUnits(const AValue: Integer): Integer;
      overload; override;
    function DocumentsToLayoutUnits(const AValue: TdxRectF): TdxRectF;
      overload; override;
    function DocumentsToLayoutUnits(const AValue: TRect): TRect;
      overload; override;
    function InchesToFontUnitsF(const AValue: Single): Single; override;
    function LayoutUnitsToDocuments(const AValue: TdxRectF): TdxRectF;
      overload; override;
    function LayoutUnitsToDocuments(const AValue: TRect): TRect;
      overload; override;
    function LayoutUnitsToHundredthsOfInch(const AValue: Integer): Integer;
      overload; override;
    function LayoutUnitsToHundredthsOfInch(const AValue: Size): Size;
      overload; override;
    function LayoutUnitsToPixels(const AValue: Integer; const ADpi: Single)
      : Integer; overload; override;
    function LayoutUnitsToPixels(const AValue: Size; const ADpiX, ADpiY: Single)
      : Size; overload; override;
    function LayoutUnitsToPixels(const AValue: TPoint;
      const ADpiX, ADpiY: Single): TPoint; overload; override;
    function LayoutUnitsToPixels(const AValue: TRect;
      const ADpiX, ADpiY: Single): TRect; overload; override;
    function LayoutUnitsToPixelsF(const AValue: Single; const ADpi: Single)
      : Single; override;
    function LayoutUnitsToPointsF(const AValue: Single): Single; override;
    function LayoutUnitsToTwips(const AValue: Int64): Int64; overload; override;
    function LayoutUnitsToTwips(const AValue: Integer): Integer;
      overload; override;
    function MillimetersToFontUnitsF(const AValue: Single): Single; override;
    function PixelsToLayoutUnits(const AValue: Integer; const ADpi: Single)
      : Integer; overload; override;
    function PixelsToLayoutUnits(const AValue: Size; const ADpiX, ADpiY: Single)
      : Size; overload; override;
    function PixelsToLayoutUnits(const AValue: TRect;
      const ADpiX, ADpiY: Single): TRect; overload; override;
    function PixelsToLayoutUnitsF(const AValue: Single; const ADpi: Single)
      : Single; override;
    function PointsToFontUnits(const AValue: Integer): Integer; override;
    function PointsToFontUnitsF(const AValue: Single): Single; override;
    function PointsToLayoutUnits(const AValue: Integer): Integer; override;
    function PointsToLayoutUnitsF(const AValue: Single): Single; override;
    function SnapToPixels(const AValue: Integer; const ADpi: Single)
      : Integer; override;
    function TwipsToLayoutUnits(const AValue: Int64): Int64; overload; override;
    function TwipsToLayoutUnits(const AValue: Integer): Integer;
      overload; override;
  end;

implementation

uses
  Windows, Math, dxTypeHelpers, dxMeasurementUnits;

{ TdxDocumentModelDpi }

class function TdxDocumentModelDpi.Dpi: Single;
begin
  Result := TdxGraphicsDpi.Pixel;
end;

class function TdxDocumentModelDpi.DpiX: Single;
begin
  Result := TdxGraphicsDpi.Pixel;
end;

class function TdxDocumentModelDpi.DpiY: Single;
begin
  Result := TdxGraphicsDpi.Pixel;
end;

{ TdxGraphicsDpi }

class constructor TdxGraphicsDpi.Initialize;
var
  DC: HDC;
begin
  DC := GetDC(0);
  FPixel := GetDeviceCaps(DC, LOGPIXELSY);
  ReleaseDC(0, DC);
end;

class function TdxGraphicsDpi.GetGraphicsDpi(AGraphics: TdxGPCanvas): Single;
begin
  if AGraphics.PageUnit = TdxGraphicUnit.guDisplay then
    Result := AGraphics.DpiX
  else
    Result := UnitToDpi(AGraphics.PageUnit);
end;

class function TdxGraphicsDpi.UnitToDpi(AUnit: TdxGraphicUnit): Single;
begin
  case AUnit of
    TdxGraphicUnit.guDocument:
      Exit(Document);
    TdxGraphicUnit.guInch:
      Exit(Inch);
    TdxGraphicUnit.guMillimeter:
      Exit(Millimeter);
    TdxGraphicUnit.guPixel, TdxGraphicUnit.guWorld:
      Exit(Pixel);
    TdxGraphicUnit.guPoint:
      Exit(Point);
  else
    Exit(Display);
  end;
end;

class function TdxGraphicsDpi.DpiToUnit(ADpi: Single): TdxGraphicUnit;
begin
  if SameValue(ADpi, Display) then
    Exit(TdxGraphicUnit.guDisplay);
  if SameValue(ADpi, Inch) then
    Exit(TdxGraphicUnit.guInch);
  if SameValue(ADpi, Document) then
    Exit(TdxGraphicUnit.guDocument);
  if SameValue(ADpi, Millimeter) then
    Exit(TdxGraphicUnit.guMillimeter);
  if SameValue(ADpi, Pixel) then
    Exit(TdxGraphicUnit.guPixel);
  if SameValue(ADpi, Point) then
    Exit(TdxGraphicUnit.guPoint);
  Result := TdxGraphicUnit.guDisplay;
  Assert(False, 'Must never reach this line.');
end;

{ TdxDpiSupport }

constructor TdxDpiSupport.Create;
begin
  Create(TdxDocumentModelDpi.DpiX, TdxDocumentModelDpi.DpiY,
    TdxDocumentModelDpi.Dpi);
end;

constructor TdxDpiSupport.Create(AScreenDpiX, AScreenDpiY: Single);
begin
  Create(AScreenDpiX, AScreenDpiY, AScreenDpiX);
end;

constructor TdxDpiSupport.Create(AScreenDpiX, AScreenDpiY, AScreenDpi: Single);
begin
  inherited Create;
  FScreenDpiX := AScreenDpiX;
  FScreenDpiY := AScreenDpiY;
  FScreenDpi := AScreenDpi;
end;

function TdxDocumentLayoutUnitConverter.Equals(Obj: TObject): Boolean;
begin
  Result := (Obj = Self) or ((Obj <> nil) and (Obj.ClassType = ClassType) and
    (TdxDocumentLayoutUnitConverter(Obj).Dpi = Dpi));
end;

class function TdxDocumentLayoutUnitConverter.CreateConverter
  (const AUnit: TdxDocumentLayoutUnit; const ADpi: Single)
  : TdxDocumentLayoutUnitConverter;
begin
  case AUnit of
    TdxDocumentLayoutUnit.Document:
      Result := TdxDocumentLayoutUnitDocumentConverter.Create;
    TdxDocumentLayoutUnit.Twip:
      Result := TdxDocumentLayoutUnitTwipsConverter.Create;
    // #    TdxDocumentLayoutUnit.Print:
    // #      Result := TdxDocumentLayoutUnitPixelsConverter.Create(4800); //# must be equals to FUnitsPerInch in dxPSCore.pas
  else
    // # TdxDocumentLayoutUnit.Pixel:
    Result := TdxDocumentLayoutUnitPixelsConverter.Create(ADpi);
  end;
end;

function TdxDocumentLayoutUnitConverter.GetFontSizeScaleForPrinting: Single;
begin
  Result := 1.0;
end;

function TdxDocumentLayoutUnitConverter.LayoutUnitsToPixels
  (const AValue: Integer): Integer;
begin
  Result := LayoutUnitsToPixels(AValue, ScreenDpi);
end;

function TdxDocumentLayoutUnitConverter.LayoutUnitsToPixels
  (const AValue: TSize): TSize;
begin
  Result := LayoutUnitsToPixels(AValue, ScreenDpiX, ScreenDpiY);
end;

function TdxDocumentLayoutUnitConverter.PixelsToLayoutUnits
  (const AValue: TPoint; const ADpiX, ADpiY: Single): TPoint;
begin
  Result.Init(PixelsToLayoutUnits(AValue.X, ADpiX),
    PixelsToLayoutUnits(AValue.Y, ADpiY));
end;

function TdxDocumentLayoutUnitConverter.PixelsToLayoutUnits
  (const AValue: Integer): Integer;
begin
  Result := PixelsToLayoutUnits(AValue, ScreenDpi);
end;

function TdxDocumentLayoutUnitConverter.PixelsToLayoutUnits
  (const AValue: TRect): TRect;
begin
  Result := PixelsToLayoutUnits(AValue, ScreenDpiX, ScreenDpiY);
end;

{ TdxDocumentLayoutUnitPixelsConverter }

constructor TdxDocumentLayoutUnitPixelsConverter.Create(ADpi: Single);
begin
  inherited Create;
  FDpi := ADpi;
end;

function TdxDocumentLayoutUnitPixelsConverter.DocumentsToFontUnitsF
  (const AValue: Single): Single;
begin
  Result := DocumentsToPointsF(AValue);
end;

function TdxDocumentLayoutUnitPixelsConverter.DocumentsToLayoutUnits
  (const AValue: Integer): Integer;
begin
  Result := DocumentsToPixels(AValue, FDpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.DocumentsToLayoutUnits
  (const AValue: TdxRectF): TdxRectF;
begin
  Result := DocumentsToPixels(AValue, FDpi, FDpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.DocumentsToLayoutUnits
  (const AValue: TRect): TRect;
begin
  Result := DocumentsToPixels(AValue, FDpi, FDpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.InchesToFontUnitsF
  (const AValue: Single): Single;
begin
  Result := InchesToPointsF(AValue);
end;

function TdxDocumentLayoutUnitPixelsConverter.LayoutUnitsToDocuments
  (const AValue: TdxRectF): TdxRectF;
begin
  Result := PixelsToDocuments(AValue, FDpi, FDpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.LayoutUnitsToDocuments
  (const AValue: TRect): TRect;
begin
  Result := PixelsToDocuments(AValue, FDpi, FDpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.LayoutUnitsToHundredthsOfInch
  (const AValue: Integer): Integer;
begin
  Result := PixelsToHundredthsOfInch(AValue, FDpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.LayoutUnitsToHundredthsOfInch
  (const AValue: TSize): TSize;
begin
  Result := PixelsToHundredthsOfInch(AValue, FDpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.LayoutUnitsToPixels
  (const AValue: Integer; const ADpi: Single): Integer;
begin
  Result := MulDiv(AValue, FDpi, ADpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.LayoutUnitsToPixels
  (const AValue: TSize; const ADpiX, ADpiY: Single): TSize;
begin
  Result.Init(MulDiv(AValue.cx, FDpi, ADpiX), MulDiv(AValue.cy, FDpi, ADpiY));
end;

function TdxDocumentLayoutUnitPixelsConverter.LayoutUnitsToPixels
  (const AValue: TPoint; const ADpiX, ADpiY: Single): TPoint;
begin
  Result.Init(MulDiv(AValue.X, FDpi, ADpiX), MulDiv(AValue.Y, FDpi, ADpiY));
end;

function TdxDocumentLayoutUnitPixelsConverter.LayoutUnitsToPixels
  (const AValue: TRect; const ADpiX, ADpiY: Single): TRect;
begin
  Result.Init(MulDiv(AValue.Left, FDpi, ADpiX), MulDiv(AValue.Top, FDpi, ADpiY),
    MulDiv(AValue.Right, FDpi, ADpiX), MulDiv(AValue.Bottom, FDpi, ADpiY));
end;

function TdxDocumentLayoutUnitPixelsConverter.LayoutUnitsToPixelsF
  (const AValue: Single; const ADpi: Single): Single;
begin
  Result := AValue * FDpi / ADpi;
end;

function TdxDocumentLayoutUnitPixelsConverter.LayoutUnitsToPointsF
  (const AValue: Single): Single;
begin
  Result := PixelsToPointsF(AValue, FDpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.LayoutUnitsToTwips
  (const AValue: Int64): Int64;
begin
  Result := PixelsToTwipsL(AValue, FDpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.LayoutUnitsToTwips
  (const AValue: Integer): Integer;
begin
  Result := PixelsToTwips(AValue, FDpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.MillimetersToFontUnitsF
  (const AValue: Single): Single;
begin
  Result := MillimetersToPointsF(AValue);
end;

function TdxDocumentLayoutUnitPixelsConverter.PixelsToLayoutUnits
  (const AValue: Integer; const ADpi: Single): Integer;
begin
  Result := MulDiv(AValue, FDpi, ADpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.PixelsToLayoutUnits
  (const AValue: TSize; const ADpiX, ADpiY: Single): TSize;
begin
  Result.Init(MulDiv(AValue.cx, FDpi, ADpiX), MulDiv(AValue.cy, FDpi, ADpiY));
end;

function TdxDocumentLayoutUnitPixelsConverter.PixelsToLayoutUnits
  (const AValue: TRect; const ADpiX, ADpiY: Single): TRect;
begin
  Result.Init(MulDiv(AValue.Left, FDpi, ADpiX), MulDiv(AValue.Top, FDpi, ADpiY),
    MulDiv(AValue.Right, FDpi, ADpiX), MulDiv(AValue.Bottom, FDpi, ADpiY));
end;

function TdxDocumentLayoutUnitPixelsConverter.PixelsToLayoutUnitsF
  (const AValue: Single; const ADpi: Single): Single;
begin
  Result := AValue * FDpi / ADpi;
end;

function TdxDocumentLayoutUnitPixelsConverter.PointsToFontUnits
  (const AValue: Integer): Integer;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitPixelsConverter.PointsToFontUnitsF
  (const AValue: Single): Single;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitPixelsConverter.PointsToLayoutUnits
  (const AValue: Integer): Integer;
begin
  Result := PointsToPixels(AValue, FDpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.PointsToLayoutUnitsF
  (const AValue: Single): Single;
begin
  Result := PointsToPixelsF(AValue, FDpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.SnapToPixels
  (const AValue: Integer; const ADpi: Single): Integer;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitPixelsConverter.TwipsToLayoutUnits
  (const AValue: Int64): Int64;
begin
  Result := TwipsToPixelsL(AValue, FDpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.TwipsToLayoutUnits
  (const AValue: Integer): Integer;
begin
  Result := TwipsToPixels(AValue, FDpi);
end;

function TdxDocumentLayoutUnitPixelsConverter.GetDpi: Single;
begin
  Result := FDpi;
end;

function TdxDocumentLayoutUnitPixelsConverter.GetFontSizeScale: Single;
begin
  Result := 72 / FDpi;
end;

function TdxDocumentLayoutUnitPixelsConverter.GetFontUnit
  : TdxLayoutGraphicsUnit;
begin
  Result := TdxLayoutGraphicsUnit.Point;
end;

function TdxDocumentLayoutUnitPixelsConverter.GetGraphicsPageScale: Single;
begin
  Result := 1;
end;

function TdxDocumentLayoutUnitPixelsConverter.GetGraphicsPageUnit
  : TdxLayoutGraphicsUnit;
begin
  Result := TdxLayoutGraphicsUnit.Pixel;
end;

{ TdxDocumentLayoutUnitTwipsConverter }

function TdxDocumentLayoutUnitTwipsConverter.DocumentsToFontUnitsF
  (const AValue: Single): Single;
begin
  Result := DocumentsToPointsF(AValue);
end;

function TdxDocumentLayoutUnitTwipsConverter.DocumentsToLayoutUnits
  (const AValue: Integer): Integer;
begin
  Result := DocumentsToTwips(AValue);
end;

function TdxDocumentLayoutUnitTwipsConverter.DocumentsToLayoutUnits
  (const AValue: TdxRectF): TdxRectF;
begin
  Result := DocumentsToTwips(AValue);
end;

function TdxDocumentLayoutUnitTwipsConverter.DocumentsToLayoutUnits
  (const AValue: TRect): TRect;
begin
  Result := DocumentsToTwips(AValue);
end;

function TdxDocumentLayoutUnitTwipsConverter.InchesToFontUnitsF
  (const AValue: Single): Single;
begin
  Result := InchesToPointsF(AValue);
end;

function TdxDocumentLayoutUnitTwipsConverter.LayoutUnitsToDocuments
  (const AValue: TdxRectF): TdxRectF;
begin
  Result := TwipsToDocuments(AValue);
end;

function TdxDocumentLayoutUnitTwipsConverter.LayoutUnitsToDocuments
  (const AValue: TRect): TRect;
begin
  Result := TwipsToDocuments(AValue);
end;

function TdxDocumentLayoutUnitTwipsConverter.LayoutUnitsToHundredthsOfInch
  (const AValue: Integer): Integer;
begin
  Result := TwipsToHundredthsOfInch(AValue);
end;

function TdxDocumentLayoutUnitTwipsConverter.LayoutUnitsToHundredthsOfInch
  (const AValue: TSize): TSize;
begin
  Result := TwipsToHundredthsOfInch(AValue);
end;

function TdxDocumentLayoutUnitTwipsConverter.LayoutUnitsToPixels
  (const AValue: Integer; const ADpi: Single): Integer;
begin
  Result := TwipsToPixels(AValue, ADpi);
end;

function TdxDocumentLayoutUnitTwipsConverter.LayoutUnitsToPixels
  (const AValue: TSize; const ADpiX, ADpiY: Single): TSize;
begin
  Result := TwipsToPixels(AValue, ADpiX, ADpiY);
end;

function TdxDocumentLayoutUnitTwipsConverter.LayoutUnitsToPixels
  (const AValue: TPoint; const ADpiX, ADpiY: Single): TPoint;
begin
  Result := TwipsToPixels(AValue, ADpiX, ADpiY);
end;

function TdxDocumentLayoutUnitTwipsConverter.LayoutUnitsToPixels
  (const AValue: TRect; const ADpiX, ADpiY: Single): TRect;
begin
  Result := TwipsToPixels(AValue, ADpiX, ADpiY);
end;

function TdxDocumentLayoutUnitTwipsConverter.LayoutUnitsToPixelsF
  (const AValue: Single; const ADpi: Single): Single;
begin
  Result := TwipsToPixelsF(AValue, ADpi);
end;

function TdxDocumentLayoutUnitTwipsConverter.LayoutUnitsToPointsF
  (const AValue: Single): Single;
begin
  Result := TwipsToPointsF(AValue);
end;

function TdxDocumentLayoutUnitTwipsConverter.LayoutUnitsToTwips
  (const AValue: Int64): Int64;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitTwipsConverter.LayoutUnitsToTwips
  (const AValue: Integer): Integer;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitTwipsConverter.MillimetersToFontUnitsF
  (const AValue: Single): Single;
begin
  Result := MillimetersToPointsF(AValue);
end;

function TdxDocumentLayoutUnitTwipsConverter.PixelsToLayoutUnits
  (const AValue: Integer; const ADpi: Single): Integer;
begin
  Result := PixelsToTwips(AValue, ADpi);
end;

function TdxDocumentLayoutUnitTwipsConverter.PixelsToLayoutUnits
  (const AValue: TSize; const ADpiX, ADpiY: Single): TSize;
begin
  Result := PixelsToTwips(AValue, ADpiX, ADpiY);
end;

function TdxDocumentLayoutUnitTwipsConverter.PixelsToLayoutUnits
  (const AValue: TRect; const ADpiX, ADpiY: Single): TRect;
begin
  Result := PixelsToTwips(AValue, ADpiX, ADpiY);
end;

function TdxDocumentLayoutUnitTwipsConverter.PixelsToLayoutUnitsF
  (const AValue: Single; const ADpi: Single): Single;
begin
  Result := PixelsToTwipsF(AValue, ADpi);
end;

function TdxDocumentLayoutUnitTwipsConverter.PointsToFontUnits
  (const AValue: Integer): Integer;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitTwipsConverter.PointsToFontUnitsF
  (const AValue: Single): Single;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitTwipsConverter.PointsToLayoutUnits
  (const AValue: Integer): Integer;
begin
  Result := PointsToTwips(AValue);
end;

function TdxDocumentLayoutUnitTwipsConverter.PointsToLayoutUnitsF
  (const AValue: Single): Single;
begin
  Result := PointsToTwipsF(AValue);
end;

function TdxDocumentLayoutUnitTwipsConverter.SnapToPixels(const AValue: Integer;
  const ADpi: Single): Integer;
begin
  Result := Round(Dpi * Round(ADpi * AValue / Dpi) / ADpi);
end;

function TdxDocumentLayoutUnitTwipsConverter.TwipsToLayoutUnits
  (const AValue: Int64): Int64;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitTwipsConverter.TwipsToLayoutUnits
  (const AValue: Integer): Integer;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitTwipsConverter.GetDpi: Single;
begin
  Result := 1440;
end;

function TdxDocumentLayoutUnitTwipsConverter.GetFontSizeScale: Single;
begin
  Result := 1 / 20;
end;

function TdxDocumentLayoutUnitTwipsConverter.
  GetFontSizeScaleForPrinting: Single;
begin
  Result := 20;
end;

function TdxDocumentLayoutUnitTwipsConverter.GetFontUnit: TdxLayoutGraphicsUnit;
begin
  Result := TdxLayoutGraphicsUnit.Point;
end;

function TdxDocumentLayoutUnitTwipsConverter.GetGraphicsPageScale: Single;
begin
  Result := 1 / 20;
end;

function TdxDocumentLayoutUnitTwipsConverter.GetGraphicsPageUnit
  : TdxLayoutGraphicsUnit;
begin
  Result := TdxLayoutGraphicsUnit.Point;
end;

{ TdxDocumentLayoutUnitDocumentConverter }

function TdxDocumentLayoutUnitDocumentConverter.DocumentsToFontUnitsF
  (const AValue: Single): Single;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitDocumentConverter.DocumentsToLayoutUnits
  (const AValue: Integer): Integer;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitDocumentConverter.DocumentsToLayoutUnits
  (const AValue: TdxRectF): TdxRectF;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitDocumentConverter.DocumentsToLayoutUnits
  (const AValue: TRect): TRect;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitDocumentConverter.InchesToFontUnitsF
  (const AValue: Single): Single;
begin
  Result := InchesToDocumentsF(AValue);
end;

function TdxDocumentLayoutUnitDocumentConverter.LayoutUnitsToDocuments
  (const AValue: TdxRectF): TdxRectF;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitDocumentConverter.LayoutUnitsToDocuments
  (const AValue: TRect): TRect;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitDocumentConverter.LayoutUnitsToHundredthsOfInch
  (const AValue: Integer): Integer;
begin
  Result := DocumentsToHundredthsOfInch(AValue);
end;

function TdxDocumentLayoutUnitDocumentConverter.LayoutUnitsToHundredthsOfInch
  (const AValue: Size): Size;
begin
  Result := DocumentsToHundredthsOfInch(AValue);
end;

function TdxDocumentLayoutUnitDocumentConverter.LayoutUnitsToPixels
  (const AValue: Integer; const ADpi: Single): Integer;
begin
  Result := DocumentsToPixels(AValue, ADpi);
end;

function TdxDocumentLayoutUnitDocumentConverter.LayoutUnitsToPixels
  (const AValue: Size; const ADpiX, ADpiY: Single): Size;
begin
  Result := DocumentsToPixels(AValue, ADpiX, ADpiY);
end;

function TdxDocumentLayoutUnitDocumentConverter.LayoutUnitsToPixels
  (const AValue: TPoint; const ADpiX, ADpiY: Single): TPoint;
begin
  Result := DocumentsToPixels(AValue, ADpiX, ADpiY);
end;

function TdxDocumentLayoutUnitDocumentConverter.LayoutUnitsToPixels
  (const AValue: TRect; const ADpiX, ADpiY: Single): TRect;
begin
  Result := DocumentsToPixels(AValue, ADpiX, ADpiY);
end;

function TdxDocumentLayoutUnitDocumentConverter.LayoutUnitsToPixelsF
  (const AValue: Single; const ADpi: Single): Single;
begin
  Result := DocumentsToPixelsF(AValue, ADpi);
end;

function TdxDocumentLayoutUnitDocumentConverter.LayoutUnitsToPointsF
  (const AValue: Single): Single;
begin
  Result := DocumentsToPointsF(AValue);
end;

function TdxDocumentLayoutUnitDocumentConverter.LayoutUnitsToTwips
  (const AValue: Int64): Int64;
begin
  Result := DocumentsToTwipsL(AValue);
end;

function TdxDocumentLayoutUnitDocumentConverter.LayoutUnitsToTwips
  (const AValue: Integer): Integer;
begin
  Result := DocumentsToTwips(AValue);
end;

function TdxDocumentLayoutUnitDocumentConverter.MillimetersToFontUnitsF
  (const AValue: Single): Single;
begin
  Result := MillimetersToDocumentsF(AValue);
end;

function TdxDocumentLayoutUnitDocumentConverter.PixelsToLayoutUnits
  (const AValue: Integer; const ADpi: Single): Integer;
begin
  Result := PixelsToDocuments(AValue, ADpi);
end;

function TdxDocumentLayoutUnitDocumentConverter.PixelsToLayoutUnits
  (const AValue: Size; const ADpiX, ADpiY: Single): Size;
begin
  Result := PixelsToDocuments(AValue, ADpiX, ADpiY);
end;

function TdxDocumentLayoutUnitDocumentConverter.PixelsToLayoutUnits
  (const AValue: TRect; const ADpiX, ADpiY: Single): TRect;
begin
  Result := PixelsToDocuments(AValue, ADpiX, ADpiY);
end;

function TdxDocumentLayoutUnitDocumentConverter.PixelsToLayoutUnitsF
  (const AValue: Single; const ADpi: Single): Single;
begin
  Result := PixelsToDocumentsF(AValue, ADpi);
end;

function TdxDocumentLayoutUnitDocumentConverter.PointsToFontUnits
  (const AValue: Integer): Integer;
begin
  Result := PointsToDocuments(AValue);
end;

function TdxDocumentLayoutUnitDocumentConverter.PointsToFontUnitsF
  (const AValue: Single): Single;
begin
  Result := PointsToDocumentsF(AValue);
end;

function TdxDocumentLayoutUnitDocumentConverter.PointsToLayoutUnits
  (const AValue: Integer): Integer;
begin
  Result := PointsToDocuments(AValue);
end;

function TdxDocumentLayoutUnitDocumentConverter.PointsToLayoutUnitsF
  (const AValue: Single): Single;
begin
  Result := PointsToDocumentsF(AValue);
end;

function TdxDocumentLayoutUnitDocumentConverter.SnapToPixels
  (const AValue: Integer; const ADpi: Single): Integer;
begin
  Result := AValue;
end;

function TdxDocumentLayoutUnitDocumentConverter.TwipsToLayoutUnits
  (const AValue: Int64): Int64;
begin
  Result := TwipsToDocumentsL(AValue);
end;

function TdxDocumentLayoutUnitDocumentConverter.TwipsToLayoutUnits
  (const AValue: Integer): Integer;
begin
  Result := TwipsToDocuments(AValue);
end;

function TdxDocumentLayoutUnitDocumentConverter.GetDpi: Single;
begin
  Result := DefaultDpi;
end;

function TdxDocumentLayoutUnitDocumentConverter.GetFontSizeScale: Single;
begin
  Result := 1;
end;

function TdxDocumentLayoutUnitDocumentConverter.GetFontUnit
  : TdxLayoutGraphicsUnit;
begin
  Result := TdxLayoutGraphicsUnit.Document;
end;

function TdxDocumentLayoutUnitDocumentConverter.GetGraphicsPageScale: Single;
begin
  Result := 1;
end;

function TdxDocumentLayoutUnitDocumentConverter.GetGraphicsPageUnit
  : TdxLayoutGraphicsUnit;
begin
  Result := TdxLayoutGraphicsUnit.Document;
end;

end.
