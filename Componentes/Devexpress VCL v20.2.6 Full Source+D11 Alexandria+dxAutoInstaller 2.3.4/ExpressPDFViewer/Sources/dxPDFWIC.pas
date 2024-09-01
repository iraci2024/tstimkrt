{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ ExpressPDFViewer }
{ }
{ Copyright (c) 2015-2021 Developer Express Inc. }
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
{ LICENSED TO DISTRIBUTE THE EXPRESSPDFVIEWER AND ALL }
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

unit dxPDFWIC; // for internal use

{$I cxVer.inc}

interface

uses
  Windows, Types, SysUtils, Wincodec, dxPDFDCTDecoder, dxPDFImageUtils;

type
  { TdxWICImagingFactory }

  TdxWICImagingFactory = class
  strict private
  class var
    FImagingFactory: IWICImagingFactory;
    FEncoderContainerFormat: TGUID;
  protected
    class function GetImagingFactory: IWICImagingFactory;
    class procedure Initialize;
    class procedure Finalize;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateBitmapScaler: IWICBitmapScaler;
    function CreateFormatConverter: IWICFormatConverter;
    function CreateJPEGDecoder: IWICBitmapDecoder;
    function CreatePalette: IWICPalette;
  end;

  { TdxPDFWICBitmapSource }

  TdxPDFWICBitmapSource = class(TInterfacedObject, IWICBitmapSource)
  strict private
    FHeight: Integer;
    FHorizontalOffset: Integer;
    FPalette: TIntegerDynArray;
    FPixelFormat: WICPixelFormatGUID;
    FScanline: TBytes;
    FSource: IdxPDFImageScanlineSource;
    FWidth: Integer;
    // IWICBitmapSource
    function GetSize(var puiWidth: UINT; var puiHeight: UINT): HRESULT; stdcall;
    function GetPixelFormat(var pPixelFormat: WICPixelFormatGUID)
      : HRESULT; stdcall;
    function GetResolution(var pDpiX: Double; var pDpiY: Double)
      : HRESULT; stdcall;
    function CopyPalette(pIPalette: IWICPalette): HRESULT; stdcall;
    function CopyPixels(prc: PWICRect; cbStride: UINT; cbBufferSize: UINT;
      pbBuffer: PByte): HRESULT; stdcall;
  public
    constructor Create(const ASource: IdxPDFImageScanlineSource;
      AWidth, AHeight: Integer; const APalette: TIntegerDynArray); overload;
    constructor Create(const ASource: IdxPDFImageScanlineSource;
      APixelFormat: WICPixelFormatGUID; AWidth, AHeight: Integer); overload;
    constructor Create(const ASource: IdxPDFImageScanlineSource;
      APixelFormat: WICPixelFormatGUID; AWidth, AHeight, ASourceWidth,
      AHorizontalOffset, AVerticalOffset: Integer); overload;
  end;

  { TdxPDFWICScanlineSourceFactory }

  TdxPDFWICScanlineSourceFactory = class(TdxPDFDefaultScanlineSourceFactory)
  strict private
    FFactory: TdxWICImagingFactory;
  protected
    property Factory: TdxWICImagingFactory read FFactory;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateInterpolator(const ASource: IdxPDFImageScanlineSource;
      ATargetWidth, ATargetHeight, ASourceWidth: Integer;
      ASourceHeight: Integer; AShouldInterpolate: Boolean)
      : IdxPDFImageScanlineSource; override;
    function CreateIndexedScanlineSource(const ASource
      : IdxPDFImageScanlineSource; AWidth, AHeight, ABitsPerComponent: Integer;
      const ALookupTable: TBytes; ABaseColorSpaceComponentCount: Integer)
      : IdxPDFImageScanlineSource; override;
  end;

  { TdxPDFWICImageInterpolator }

  TdxPDFWICImageInterpolator = class(TInterfacedObject,
    IdxPDFImageScanlineSource)
  strict private
    FCurrentY: Integer;
    FFormatConverter: IWICFormatConverter;
    FScaler: IWICBitmapScaler;
    FScanlineSource: IdxPDFImageScanlineSource;
    FSource: TdxPDFWICBitmapSource;
    FTargetWidth: Integer;
    // IdxPDFImageScanlineSource
    function GetComponentCount: Integer;
    function GetHasAlpha: Boolean;
    procedure FillBuffer(var ABuffer: TBytes; AScanlineCount: Integer);
    procedure FillNextScanline(var AScanline: TBytes);
  public
    constructor Create(const AScanlineSource: IdxPDFImageScanlineSource;
      ATargetWidth, ATargetHeight, ASourceWidth, ASourceHeight: Integer;
      AFactory: TdxWICImagingFactory; AShouldInterpolate: Boolean);
  end;

  { TdxPDFWICIndexedImageScanlineSource }

  TdxPDFWICIndexedImageScanlineSource = class(TInterfacedObject,
    IdxPDFImageScanlineSource)
  strict private
    FAlphaMask: Int64;
    FBitmapSource: TdxPDFWICBitmapSource;
    FConverter: IWICFormatConverter;
    FCurrentY: Integer;
    FPalette: IWICPalette;
    FWidth: Integer;
    // IdxPDFImageScanlineSource
    function GetComponentCount: Integer;
    function GetHasAlpha: Boolean;
    procedure FillBuffer(var ABuffer: TBytes; AScanlineCount: Integer);
    procedure FillNextScanline(var AScanline: TBytes);
  public
    constructor Create(AFactory: TdxWICImagingFactory;
      const ASource: IdxPDFImageScanlineSource;
      AWidth, AHeight, ABitsPerComponent: Integer; const ALookupTable: TBytes);
  end;

  { TdxPDFWICJPEGScanlineSource }

  TdxPDFWICJPEGScanlineSource = class(TInterfacedObject,
    IdxPDFImageScanlineSource)
  strict private
    FBitmap: IWICBitmapSource;
    FWidth: Integer;
    FComponentCount: Integer;
    FIsYCCK: Boolean;
    FColumn: Integer;
  protected
    class function CreateSource(const AImage: IdxPDFDocumentImage;
      const AData: TBytes): IdxPDFImageScanlineSource; static;
    // IdxPDFImageScanlineSource
    function GetComponentCount: Integer; virtual;
    function GetHasAlpha: Boolean; virtual;
    procedure FillBuffer(var ABuffer: TBytes; AScanlineCount: Integer);
    procedure FillNextScanline(var AScanline: TBytes); virtual;
  public
    constructor Create(const AImage: IdxPDFDocumentImage;
      const ABitmap: IWICBitmapSource; AIsYCCK: Boolean); virtual;
    class function CreateScanlineSource(const AImage: IdxPDFDocumentImage;
      const AData: TBytes): IdxPDFImageScanlineSource; overload; static;
    class function CreateScanlineSource(const AImage: IdxPDFDocumentImage;
      const ABitmap: IWICBitmapSource; AIsYCCK: Boolean)
      : IdxPDFImageScanlineSource; overload; static;
  end;

  { TdxPDFWICJPEGColorKeyedScanlineSource }

  TdxPDFWICJPEGColorKeyedScanlineSource = class(TdxPDFWICJPEGScanlineSource)
  strict private
    FBuffer: TBytes;
    FDecoder: IdxPDFImageScanlineDecoder;
  protected
    function GetComponentCount: Integer; override;
    function GetHasAlpha: Boolean; override;
    procedure FillNextScanline(var AScanline: TBytes); override;
  public
    constructor Create(const AImage: IdxPDFDocumentImage;
      const ABitmap: IWICBitmapSource; AIsYCCK: Boolean); override;
  end;

  { TdxPDFWICDCTDecoderFactory }

  TdxPDFWICDCTDecoderFactory = class(TdxPDFDCTDecoderFactory)
  public
    function CreateSource(const AImageData: TBytes;
      const AImage: IdxPDFDocumentImage): IdxPDFImageScanlineSource; override;
  end;

function dxWICRect(ALeft, ATop, AWidth, AHeight: Integer): WICRect;
procedure dxWICCheck(Result: HRESULT);

implementation

uses
  Math, Classes, Graphics, ComObj, ActiveX, dxCore, cxVariants, dxJPX,
  dxPDFUtils;

type
  TdxPDFRecognizedImageFormat = (rifUnrecognized, rifMetafile, rifRGBJpeg,
    rifYCCKJpeg, rifTiff);

  { TdxPDFRecognizedImageInfo }

  TdxPDFRecognizedImageInfo = record
  private
    FHeight: Integer;
    FType: TdxPDFRecognizedImageFormat;
    FWidth: Integer;
  public
    class function Create(AType: TdxPDFRecognizedImageFormat)
      : TdxPDFRecognizedImageInfo; overload; static;
    class function Create(AType: TdxPDFRecognizedImageFormat;
      AWidth, AHeight: Integer): TdxPDFRecognizedImageInfo; overload; static;
    class function DetectImage(AData: TStream)
      : TdxPDFRecognizedImageInfo; static;

    property ImageType: TdxPDFRecognizedImageFormat read FType;
    property JPEGHeight: Integer read FHeight;
    property JPEGWidth: Integer read FWidth;
  end;

var
  FNeedFinalize: Boolean;

procedure dxWICCheck(Result: HRESULT);
begin
  if Failed(Result) then
    raise EInvalidGraphic.Create('Invalid image format');
end;

function dxWICRect(ALeft, ATop, AWidth, AHeight: Integer): WICRect;
begin
  Result.X := ALeft;
  Result.Y := ATop;
  Result.Width := AWidth;
  Result.Height := AHeight;
end;

{ TdxPDFRecognizedImageInfo }

class function TdxPDFRecognizedImageInfo.Create
  (AType: TdxPDFRecognizedImageFormat): TdxPDFRecognizedImageInfo;
begin
  Result := Create(AType, 0, 0);
end;

class function TdxPDFRecognizedImageInfo.Create
  (AType: TdxPDFRecognizedImageFormat; AWidth, AHeight: Integer)
  : TdxPDFRecognizedImageInfo;
begin
  Result.FType := AType;
  Result.FWidth := AWidth;
  Result.FHeight := AHeight;
end;

class function TdxPDFRecognizedImageInfo.DetectImage(AData: TStream)
  : TdxPDFRecognizedImageInfo;
var
  AStream: TdxBigEndianStreamReader;
  AColorComponentCount, AColorTransform, AHead, AHeight, ALength,
    AWidth: Integer;
begin
  AData.Position := 0;
  AStream := TdxBigEndianStreamReader.Create(AData);
  try
    case AStream.ReadByte of
      $01:
        if AStream.ReadByte = $00 then
        begin
          case AStream.ReadByte of
            $09, $00:
              if AStream.ReadByte = $00 then
                Exit(TdxPDFRecognizedImageInfo.Create(rifMetafile));
          end;
        end;
      $02:
        if ((AStream.ReadByte = $00) and (AStream.ReadByte = $09)) and
          (AStream.ReadByte = $00) then
          Exit(TdxPDFRecognizedImageInfo.Create(rifMetafile));
      $49:
        if ((AStream.ReadByte = $49) and (AStream.ReadByte = 42)) and
          (AStream.ReadByte = 0) then
          Exit(TdxPDFRecognizedImageInfo.Create(rifTiff));
      $4D:
        if (AStream.ReadByte = $4D) and (AStream.ReadInt16 = 42) then
          Exit(TdxPDFRecognizedImageInfo.Create(rifTiff));
      $D7:
        if ((AStream.ReadByte = $CD) and (AStream.ReadByte = $C6)) and
          (AStream.ReadByte = $9A) then
          Exit(TdxPDFRecognizedImageInfo.Create(rifMetafile));
      $FF:
        if AStream.ReadByte = $D8 then
        begin
          AHeight := 0;
          AWidth := 0;
          AColorTransform := 0;
          AColorComponentCount := 0;
          while not AStream.IsEOF do
          begin
            AHead := AStream.ReadInt16;
            ALength := AStream.ReadInt16;
            case AHead of
              $FFEE:
                if ALength >= 12 then
                begin
                  AStream.Skip(11);
                  AColorTransform := AStream.ReadByte;
                  Dec(ALength, 12);
                end;
              $FFC0, $FFC1, $FFC2, $FFC3, $FFC5, $FFC6, $FFC7, $FFC9, $FFCA,
                $FFCB, $FFCD, $FFCE, $FFCF:
                begin
                  AStream.ReadByte;
                  AHeight := AStream.ReadInt16;
                  AWidth := AStream.ReadInt16;
                  AColorComponentCount := AStream.ReadByte;
                  Dec(ALength, 6);
                end;
            else
            end;
            Dec(ALength, 2);
            if ALength > 0 then
            begin
              ALength := Min(AStream.Length - AStream.Position, ALength);
              AStream.Skip(ALength);
            end;
          end;
          case AColorComponentCount of
            3:
              begin
                if AColorTransform = 0 then
                  Exit(TdxPDFRecognizedImageInfo.Create(rifRGBJpeg,
                    AWidth, AHeight))
                else
                  Exit(TdxPDFRecognizedImageInfo.Create(rifUnrecognized, AWidth,
                    AHeight));
              end;
            4:
              case AColorTransform of
                0, 2:
                  Exit(TdxPDFRecognizedImageInfo.Create(rifYCCKJpeg, AWidth,
                    AHeight));
              else
                Exit(TdxPDFRecognizedImageInfo.Create(rifUnrecognized, AWidth,
                  AHeight));
              end;
          else
            Exit(TdxPDFRecognizedImageInfo.Create(rifUnrecognized, AWidth,
              AHeight));
          end;
        end;
    end;
    Exit(TdxPDFRecognizedImageInfo.Create(rifUnrecognized));
  finally
    AStream.Free;
    AData.Position := 0;
  end;
end;

{ TdxWICImagingFactory }

constructor TdxWICImagingFactory.Create;
begin
  inherited Create;
  if FImagingFactory = nil then
    Initialize
  else
    FImagingFactory._AddRef;
  FEncoderContainerFormat := GUID_ContainerFormatJpeg;
end;

destructor TdxWICImagingFactory.Destroy;
begin
  Finalize;
  inherited Destroy;
end;

class function TdxWICImagingFactory.GetImagingFactory: IWICImagingFactory;
begin
  Result := FImagingFactory;
end;

class procedure TdxWICImagingFactory.Initialize;
begin
  if TdxPDFUtils.UseWIC then
  begin
    FImagingFactory := CreateComObject(CLSID_WICImagingFactory)
      as IWICImagingFactory;
    dxTestCheck(FImagingFactory <> nil, 'FImagingFactory = nil');
  end;
end;

class procedure TdxWICImagingFactory.Finalize;
begin
  if (FImagingFactory <> nil) and (FImagingFactory._Release = 0) then
    Pointer(FImagingFactory) := nil;
end;

function TdxWICImagingFactory.CreateBitmapScaler: IWICBitmapScaler;
begin
  dxWICCheck(FImagingFactory.CreateBitmapScaler(Result));
end;

function TdxWICImagingFactory.CreateFormatConverter: IWICFormatConverter;
begin
  dxWICCheck(FImagingFactory.CreateFormatConverter(Result));
end;

function TdxWICImagingFactory.CreateJPEGDecoder: IWICBitmapDecoder;
begin
  dxWICCheck(FImagingFactory.CreateDecoder(GUID_ContainerFormatJpeg,
    GUID_NULL, Result));
end;

function TdxWICImagingFactory.CreatePalette: IWICPalette;
begin
  dxWICCheck(FImagingFactory.CreatePalette(Result));
end;

{ TdxPDFWICBitmapSource }

constructor TdxPDFWICBitmapSource.Create(const ASource
  : IdxPDFImageScanlineSource; APixelFormat: WICPixelFormatGUID;
  AWidth, AHeight, ASourceWidth, AHorizontalOffset, AVerticalOffset: Integer);
var
  I: Integer;
begin
  inherited Create;
  FSource := ASource;
  FWidth := AWidth;
  FHeight := AHeight;
  FPixelFormat := APixelFormat;
  FHorizontalOffset := AHorizontalOffset * ASource.ComponentCount;
  SetLength(FScanline, ASourceWidth * ASource.ComponentCount);
  for I := 0 to AVerticalOffset - 1 do
    ASource.FillNextScanline(FScanline);
end;

constructor TdxPDFWICBitmapSource.Create(const ASource
  : IdxPDFImageScanlineSource; APixelFormat: WICPixelFormatGUID;
  AWidth, AHeight: Integer);
begin
  Create(ASource, APixelFormat, AWidth, AHeight, AWidth, 0, 0);
end;

constructor TdxPDFWICBitmapSource.Create(const ASource
  : IdxPDFImageScanlineSource; AWidth, AHeight: Integer;
  const APalette: TIntegerDynArray);
begin
  Create(ASource, GUID_WICPixelFormat8bppIndexed, AWidth, AHeight);
  FPalette := APalette;
end;

function TdxPDFWICBitmapSource.GetSize(var puiWidth: UINT;
  var puiHeight: UINT): HRESULT;
begin
  puiWidth := FWidth;
  puiHeight := FHeight;
  Result := 0;
end;

function TdxPDFWICBitmapSource.GetPixelFormat(var pPixelFormat
  : WICPixelFormatGUID): HRESULT;
begin
  pPixelFormat := FPixelFormat;
  Result := 0;
end;

function TdxPDFWICBitmapSource.GetResolution(var pDpiX: Double;
  var pDpiY: Double): HRESULT;
begin
  pDpiX := 96;
  pDpiY := 96;
  Result := 0;
end;

function TdxPDFWICBitmapSource.CopyPalette(pIPalette: IWICPalette): HRESULT;
begin
  pIPalette.InitializeCustom(@FPalette[0], Length(FPalette));
  Result := 0;
end;

function TdxPDFWICBitmapSource.CopyPixels(prc: PWICRect; cbStride: UINT;
  cbBufferSize: UINT; pbBuffer: PByte): HRESULT;
var
  AActualBuffer: PByte;
  AScanlineSize, I: Integer;
begin
  AActualBuffer := pbBuffer;
  AScanlineSize := FWidth * FSource.ComponentCount;
  if Integer(cbBufferSize) < AScanlineSize * prc.Height then
    Exit(WINCODEC_ERR_INVALIDPARAMETER);
  for I := 0 to prc.Height - 1 do
  begin
    FSource.FillNextScanline(FScanline);
    cxCopyData(@FScanline[0], AActualBuffer, FHorizontalOffset, 0,
      AScanlineSize);
    Inc(AActualBuffer, cbStride);
  end;
  Result := 0;
end;

{ TdxPDFWICScanlineSourceFactory }

constructor TdxPDFWICScanlineSourceFactory.Create;
begin
  inherited Create;
  FFactory := TdxWICImagingFactory.Create;
  if FFactory.GetImagingFactory = nil then
    TdxPDFUtils.Abort;
end;

destructor TdxPDFWICScanlineSourceFactory.Destroy;
begin
  FreeAndNil(FFactory);
  inherited Destroy;
end;

function TdxPDFWICScanlineSourceFactory.CreateInterpolator
  (const ASource: IdxPDFImageScanlineSource; ATargetWidth, ATargetHeight,
  ASourceWidth, ASourceHeight: Integer; AShouldInterpolate: Boolean)
  : IdxPDFImageScanlineSource;

  function CreateDefaultInterpolator: IdxPDFImageScanlineSource;
  begin
    Result := inherited CreateInterpolator(ASource, ATargetWidth, ATargetHeight,
      ASourceWidth, ASourceHeight, AShouldInterpolate);
  end;

  function CreateWICInterpolator: IdxPDFImageScanlineSource;
  begin
    Result := TdxPDFWICImageInterpolator.Create(ASource, ATargetWidth,
      ATargetHeight, ASourceWidth, ASourceHeight, FFactory, AShouldInterpolate);
  end;

begin
  case ASource.ComponentCount of
    1, 3:
      Result := CreateWICInterpolator;
    4:
      if ASource.HasAlpha then
        Result := CreateWICInterpolator
      else
        Result := CreateDefaultInterpolator;
  else
    Result := CreateDefaultInterpolator;
  end;
end;

function TdxPDFWICScanlineSourceFactory.CreateIndexedScanlineSource
  (const ASource: IdxPDFImageScanlineSource;
  AWidth, AHeight, ABitsPerComponent: Integer; const ALookupTable: TBytes;
  ABaseColorSpaceComponentCount: Integer): IdxPDFImageScanlineSource;
begin
  if (ABaseColorSpaceComponentCount = 3) and not ASource.HasAlpha then
    Result := TdxPDFWICIndexedImageScanlineSource.Create(FFactory, ASource,
      AWidth, AHeight, ABitsPerComponent, ALookupTable)
  else
    Result := inherited CreateIndexedScanlineSource(ASource, AWidth, AHeight,
      ABitsPerComponent, ALookupTable, ABaseColorSpaceComponentCount);
end;

{ TdxPDFWICImageInterpolator }

constructor TdxPDFWICImageInterpolator.Create(const AScanlineSource
  : IdxPDFImageScanlineSource; ATargetWidth, ATargetHeight, ASourceWidth,
  ASourceHeight: Integer; AFactory: TdxWICImagingFactory;
  AShouldInterpolate: Boolean);
var
  AActualPixelFormat, APixelFormat: WICPixelFormatGUID;
  AInterpolationMode: WICBitmapInterpolationMode;
begin
  inherited Create;
  FCurrentY := 0;
  FTargetWidth := ATargetWidth;
  FScanlineSource := AScanlineSource;
  case AScanlineSource.ComponentCount of
    1:
      APixelFormat := GUID_WICPixelFormat8bppGray;
    4:
      APixelFormat := GUID_WICPixelFormat32bppRGBA;
  else
    APixelFormat := GUID_WICPixelFormat24bppRGB;
  end;
  FSource := TdxPDFWICBitmapSource.Create(AScanlineSource, APixelFormat,
    ASourceWidth, ASourceHeight);
  FScaler := AFactory.CreateBitmapScaler;
  if AShouldInterpolate then
    AInterpolationMode := WICBitmapInterpolationModeFant
  else
    AInterpolationMode := WICBitmapInterpolationModeNearestNeighbor;
  FScaler.Initialize(FSource, ATargetWidth, ATargetHeight, AInterpolationMode);
  FScaler.GetPixelFormat(AActualPixelFormat);
  if not IsEqualGUID(AActualPixelFormat, APixelFormat) then
  begin
    FFormatConverter := AFactory.CreateFormatConverter;
    FFormatConverter.Initialize(FScaler, APixelFormat, WICBitmapDitherTypeNone,
      nil, 0, WICBitmapPaletteTypeCustom);
  end;
end;

function TdxPDFWICImageInterpolator.GetComponentCount: Integer;
begin
  Result := FScanlineSource.ComponentCount;
end;

function TdxPDFWICImageInterpolator.GetHasAlpha: Boolean;
begin
  Result := FScanlineSource.HasAlpha;
end;

procedure TdxPDFWICImageInterpolator.FillBuffer(var ABuffer: TBytes;
  AScanlineCount: Integer);
begin
  // do nothing
end;

procedure TdxPDFWICImageInterpolator.FillNextScanline(var AScanline: TBytes);
var
  ARectangle: WICRect;
begin
  ARectangle := dxWICRect(0, FCurrentY, FTargetWidth, 1);
  Inc(FCurrentY);
  if FFormatConverter <> nil then
    FFormatConverter.CopyPixels(@ARectangle, FTargetWidth * GetComponentCount,
      Length(AScanline), @AScanline[0])
  else
    FScaler.CopyPixels(@ARectangle, FTargetWidth * GetComponentCount,
      Length(AScanline), @AScanline[0]);
end;

{ TdxPDFWICIndexedImageScanlineSource }

constructor TdxPDFWICIndexedImageScanlineSource.Create
  (AFactory: TdxWICImagingFactory; const ASource: IdxPDFImageScanlineSource;
  AWidth, AHeight, ABitsPerComponent: Integer; const ALookupTable: TBytes);
var
  AActualPaletteSize, AComponentRange, AFactor, AOffset, AValue, I, J: Integer;
  APaletteData: TIntegerDynArray;
begin
  inherited Create;
  FAlphaMask := $FF000000;
  FWidth := AWidth;

  SetLength(APaletteData, 256);
  AComponentRange := 1 shl ABitsPerComponent;
  AFactor := 255 div (AComponentRange - 1);
  AActualPaletteSize := Min(Length(ALookupTable) div 3, AComponentRange);

  AOffset := 0;
  for I := 0 to AActualPaletteSize - 1 do
  begin
    AValue := FAlphaMask;
    for J := 2 downto 0 do
    begin
      AValue := AValue or (ALookupTable[AOffset] shl (J * 8));
      Inc(AOffset);
    end;
    APaletteData[I * AFactor] := AValue;
  end;
  FBitmapSource := TdxPDFWICBitmapSource.Create(ASource, AWidth, AHeight,
    APaletteData);
  FPalette := AFactory.CreatePalette;
  FPalette.InitializeCustom(@APaletteData[0], Length(APaletteData));
  FConverter := AFactory.CreateFormatConverter;
  FConverter.Initialize(FBitmapSource, GUID_WICPixelFormat24bppRGB,
    WICBitmapDitherTypeNone, nil, 0, WICBitmapPaletteTypeCustom);
end;

function TdxPDFWICIndexedImageScanlineSource.GetComponentCount: Integer;
begin
  Result := 3;
end;

function TdxPDFWICIndexedImageScanlineSource.GetHasAlpha: Boolean;
begin
  Result := False;
end;

procedure TdxPDFWICIndexedImageScanlineSource.FillBuffer(var ABuffer: TBytes;
  AScanlineCount: Integer);
begin
  // do nothing
end;

procedure TdxPDFWICIndexedImageScanlineSource.FillNextScanline
  (var AScanline: TBytes);
var
  ARect: WICRect;
begin
  ARect := dxWICRect(0, FCurrentY, FWidth, 1);
  Inc(FCurrentY);
  FConverter.CopyPixels(@ARect, FWidth * 3, Length(AScanline), @AScanline[0]);
end;

{ TdxPDFWICJPEGScanlineSource }

constructor TdxPDFWICJPEGScanlineSource.Create(const AImage
  : IdxPDFDocumentImage; const ABitmap: IWICBitmapSource; AIsYCCK: Boolean);
begin
  inherited Create;
  FWidth := AImage.GetWidth;
  FComponentCount := AImage.GetColorSpaceComponentCount;
  FBitmap := ABitmap;
  FIsYCCK := AIsYCCK;
end;

class function TdxPDFWICJPEGScanlineSource.CreateScanlineSource
  (const AImage: IdxPDFDocumentImage; const AData: TBytes)
  : IdxPDFImageScanlineSource;
begin
  try
    Result := CreateSource(AImage, AData);
  except
    Result := CreateSource(AImage, TdxPDFDCTDataValidator.ChangeImageHeight
      (AData, AImage.GetHeight));
  end;
end;

class function TdxPDFWICJPEGScanlineSource.CreateScanlineSource
  (const AImage: IdxPDFDocumentImage; const ABitmap: IWICBitmapSource;
  AIsYCCK: Boolean): IdxPDFImageScanlineSource;
begin
  if Length(AImage.GetColorKeyMask) > 0 then
    Result := TdxPDFWICJPEGColorKeyedScanlineSource.Create(AImage,
      ABitmap, AIsYCCK)
  else
    Result := TdxPDFWICJPEGScanlineSource.Create(AImage, ABitmap, AIsYCCK);
end;

class function TdxPDFWICJPEGScanlineSource.CreateSource
  (const AImage: IdxPDFDocumentImage; const AData: TBytes)
  : IdxPDFImageScanlineSource;
var
  AFrameDecode: IWICBitmapFrameDecode;
  AConverter: IWICFormatConverter;
  ADecoder: IWICBitmapDecoder;
  AFactory: TdxWICImagingFactory;
  APixelFormat: TGUID;
  AStream: TStreamAdapter;
  ATemp, ABytesStream: TBytesStream;
begin
  ABytesStream := TBytesStream.Create(AData);
  AStream := TStreamAdapter.Create(ABytesStream, soOwned);
  AFactory := TdxWICImagingFactory.Create;
  try
    ADecoder := AFactory.CreateJPEGDecoder;
    dxWICCheck(ADecoder.Initialize(AStream, WICDecodeMetadataCacheOnDemand));
    dxWICCheck(ADecoder.GetFrame(0, AFrameDecode));
    dxWICCheck(AFrameDecode.GetPixelFormat(APixelFormat));
    if IsEqualGUID(APixelFormat, GUID_WICPixelFormat24bppBGR) then
    begin
      AConverter := AFactory.CreateFormatConverter;
      dxWICCheck(AConverter.Initialize(AFrameDecode,
        GUID_WICPixelFormat24bppRGB, WICBitmapDitherTypeNone, nil, 0,
        WICBitmapPaletteTypeCustom));
      Exit(CreateScanlineSource(AImage, AConverter, False));
    end
    else if IsEqualGUID(APixelFormat, GUID_WICPixelFormat32bppCMYK) then
    begin
      ATemp := TBytesStream.Create(AData);
      try
        Exit(CreateScanlineSource(AImage, AFrameDecode,
          TdxPDFRecognizedImageInfo.DetectImage(ATemp)
          .ImageType = rifYCCKJpeg));
      finally
        ATemp.Free;
      end;
    end;
    Result := CreateScanlineSource(AImage, AFrameDecode, False);
  finally
    AFactory.Free;
  end;
end;

function TdxPDFWICJPEGScanlineSource.GetComponentCount: Integer;
begin
  Result := FComponentCount;
end;

function TdxPDFWICJPEGScanlineSource.GetHasAlpha: Boolean;
begin
  Result := False;
end;

procedure TdxPDFWICJPEGScanlineSource.FillBuffer(var ABuffer: TBytes;
  AScanlineCount: Integer);
begin
  // do nothing
end;

procedure TdxPDFWICJPEGScanlineSource.FillNextScanline(var AScanline: TBytes);
var
  I, ALength: Integer;
  ARect: WICRect;
begin
  ARect := dxWICRect(0, FColumn, FWidth, 1);
  Inc(FColumn);
  ALength := Length(AScanline);
  FBitmap.CopyPixels(@ARect, ALength, ALength, @AScanline[0]);
  if FIsYCCK then
    for I := 0 to FWidth * FComponentCount - 1 do
      AScanline[I] := 255 - AScanline[I];
end;

{ TdxPDFWICJPEGColorKeyedScanlineSource }

constructor TdxPDFWICJPEGColorKeyedScanlineSource.Create
  (const AImage: IdxPDFDocumentImage; const ABitmap: IWICBitmapSource;
  AIsYCCK: Boolean);
var
  AComponentCount: Integer;
begin
  inherited Create(AImage, ABitmap, AIsYCCK);
  AComponentCount := AImage.GetColorSpaceComponentCount;
  FDecoder := TdxPDFByteAlignedImageScanlineDecoder.Create(AImage,
    AComponentCount, 1);
  SetLength(FBuffer, AImage.GetWidth * AComponentCount);
end;

function TdxPDFWICJPEGColorKeyedScanlineSource.GetComponentCount: Integer;
begin
  Result := inherited GetComponentCount + 1;
end;

function TdxPDFWICJPEGColorKeyedScanlineSource.GetHasAlpha: Boolean;
begin
  Result := True;
end;

procedure TdxPDFWICJPEGColorKeyedScanlineSource.FillNextScanline
  (var AScanline: TBytes);
begin
  inherited FillNextScanline(FBuffer);
  FDecoder.FillNextScanline(AScanline, FBuffer, 0);
end;

{ TdxPDFWICDCTDecoderFactory }

function TdxPDFWICDCTDecoderFactory.CreateSource(const AImageData: TBytes;
  const AImage: IdxPDFDocumentImage): IdxPDFImageScanlineSource;
begin
  Result := TdxPDFWICJPEGScanlineSource.CreateScanlineSource(AImage,
    AImageData);
end;

procedure Initialize;
begin
  TdxWICImagingFactory.Initialize;
end;

procedure Finalize;
begin
  TdxWICImagingFactory.Finalize;
end;

initialization

FNeedFinalize := Succeeded(CoInitialize(nil));
dxUnitsLoader.AddUnit(@Initialize, @Finalize);

finalization

dxUnitsLoader.RemoveUnit(@Finalize);
if FNeedFinalize then
  CoUninitialize;

end.
