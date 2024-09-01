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

unit dxPDFImageUtils; // for internal use

{$I cxVer.inc}
{$IFNDEF DELPHIXE3}
{$M-}
{$ENDIF}

interface

uses
  Types, SysUtils, Generics.Defaults, Generics.Collections, dxPDFTypes, dxJPX;

type
  TdxPDFImageDataCacheItem = class;

  { IdxPDFImageScanlineSource }

  IdxPDFImageScanlineSource = interface
    ['{1F3BD991-9208-4F55-9B81-669621150AEC}']
    function GetComponentCount: Integer;
    function GetHasAlpha: Boolean;
    procedure FillBuffer(var ABuffer: TBytes; AScanlineCount: Integer);
    procedure FillNextScanline(var AScanline: TBytes);

    property ComponentCount: Integer read GetComponentCount;
    property HasAlpha: Boolean read GetHasAlpha;
  end;

  { TdxIPDFImageScanlineSourceFactory }

  IdxPDFImageScanlineSourceFactory = interface
    ['{0758F0A0-F1AC-4751-A0EA-80AB39E190F3}']
    function CreateInterpolator(const ASource: IdxPDFImageScanlineSource;
      ATargetWidth, ATargetHeight, ASourceWidth: Integer;
      ASourceHeight: Integer; AShouldInterpolate: Boolean)
      : IdxPDFImageScanlineSource;
    function CreateIndexedScanlineSource(const ASource
      : IdxPDFImageScanlineSource; AWidth, AHeight, ABitsPerComponent: Integer;
      const ALookupTable: TBytes; ABaseColorSpaceComponentCount: Integer)
      : IdxPDFImageScanlineSource;
  end;

  { IdxPDFImageScanlineDecoder }

  IdxPDFImageScanlineDecoder = interface
    ['{24869DAE-B34D-48F4-BB3C-0EC5E7391AD9}']
    function GetComponentCount: Integer;
    function GetIsColorKeyPresent: Boolean;
    function GetStride: Integer;
    procedure FillNextScanline(var AScanline: TBytes; const ASourceData: TBytes;
      ASourceOffset: Integer);

    property ComponentCount: Integer read GetComponentCount;
    property IsColorKeyPresent: Boolean read GetIsColorKeyPresent;
    property Stride: Integer read GetStride;
  end;

  { IdxPDFImageCachePainter }

  IdxPDFImageCachePainter = interface
    ['{3C6FB502-B564-4BBD-AA3F-5CFDD91C375B}']
    procedure DrawImage(AImageData: TdxPDFImageDataCacheItem;
      const AData: TBytes); overload;
    procedure DrawImage(AImageData: TdxPDFImageDataCacheItem;
      const ADataSource: IdxPDFImageScanlineSource); overload;
  end;

  { TdxPDFImageParameters }

  TdxPDFImageParameters = record
  strict private
    FHeight: Integer;
    FShouldInterpolate: Boolean;
    FWidth: Integer;
    FIsNull: Boolean;
  public
    class function Create: TdxPDFImageParameters; overload; static;
    class function Create(AWidth, AHeight: Integer): TdxPDFImageParameters;
      overload; static;
    class function Create(AWidth, AHeight: Integer; AShouldInterpolate: Boolean)
      : TdxPDFImageParameters; overload; static;
    function IsNull: Boolean;

    property Height: Integer read FHeight;
    property ShouldInterpolate: Boolean read FShouldInterpolate;
    property Width: Integer read FWidth;
  end;

  { TdxPDFImageData }

  TdxPDFImageData = record
  strict private
    FData: IdxPDFImageScanlineSource;
    FHeight: Integer;
    FPalette: TdxPDFARGBColorArray;
    FPixelFormat: TdxPDFPixelFormat;
    FStride: Integer;
    FWidth: Integer;
  public
    class function Create(const AData: IdxPDFImageScanlineSource;
      AWidth, AHeight, AStride: Integer; APixelFormat: TdxPDFPixelFormat;
      const APalette: TdxPDFARGBColorArray): TdxPDFImageData; static;

    property Data: IdxPDFImageScanlineSource read FData write FData;
    property Height: Integer read FHeight;
    property Palette: TdxPDFARGBColorArray read FPalette;
    property PixelFormat: TdxPDFPixelFormat read FPixelFormat;
    property Stride: Integer read FStride;
    property Width: Integer read FWidth;
  end;

  { IdxPDFDocumentImage }

  IdxPDFDocumentImage = interface
    ['{020C9471-D8A1-48FE-A9CC-7707CA69A54C}']
    function GetActualData(const AParameters: TdxPDFImageParameters;
      AInvertRGB: Boolean): TdxPDFImageData;
    function GetActualSize(const AParameters: TdxPDFImageParameters)
      : TdxPDFImageParameters;
    function GetBitsPerComponent: Integer;
    function GetColorKeyMask: TdxPDFRanges;
    function GetColorSpaceComponentCount: Integer;
    function GetDecodeRanges: TdxPDFRanges;
    function GetInterpolatedScanlineSource(const AData
      : IdxPDFImageScanlineSource; const AParameters: TdxPDFImageParameters)
      : IdxPDFImageScanlineSource;
    function GetHeight: Integer;
    function GetWidth: Integer;
    function HasSMaskInData: Boolean;
    function IsMask: Boolean;
  end;

  { TdxPDFImageScanline }

  TdxPDFImageScanline = record
  strict private
    FMaskData: TBytes;
    FScanlineData: TBytes;
  public
    class function Create(AWidth, AComponentCount: Integer)
      : TdxPDFImageScanline; static;

    property MaskData: TBytes read FMaskData;
    property ScanlineData: TBytes read FScanlineData;
  end;

  { TdxPDFSourceImagePixelInfo }

  TdxPDFSourceImagePixelInfo = record
  strict private
    FFactor: TdxPDFFixedPointNumber;
    FIndex: Integer;
  public
    class function Create(AIndex: Integer; AFactor: TdxPDFFixedPointNumber)
      : TdxPDFSourceImagePixelInfo; static;
    property Factor: TdxPDFFixedPointNumber read FFactor;
    property Index: Integer read FIndex;
  end;

  TdxPDFSourceImagePixelInfoArray = array of TdxPDFSourceImagePixelInfo;

  { TdxPDFInvertedImageScanlineSource }

  TdxPDFInvertedImageScanlineSource = class(TInterfacedObject,
    IdxPDFImageScanlineSource)
  strict private
    FSource: IdxPDFImageScanlineSource;
    // IdxPDFImageScanlineSource
    function GetComponentCount: Integer;
    function GetHasAlpha: Boolean;
    procedure FillBuffer(var ABuffer: TBytes; AScanlineCount: Integer);
    procedure FillNextScanline(var AScanline: TBytes);
  public
    constructor Create(const ASource: IdxPDFImageScanlineSource);
    destructor Destroy; override;
  end;

  { TdxPDFCommonImageScanlineSource }

  TdxPDFCommonImageScanlineSource = class(TInterfacedObject,
    IdxPDFImageScanlineSource)
  strict private
    FScanlineDecoder: IdxPDFImageScanlineDecoder;
    FSourceData: TBytes;
    FSourceOffset: Integer;

    function GetComponentCount: Integer;
    function GetHasAlpha: Boolean;
    procedure FillBuffer(var ABuffer: TBytes; AScanlineCount: Integer);
    procedure FillNextScanline(var AScanline: TBytes);
  strict protected
    property HasAlpha: Boolean read GetHasAlpha;
  public
    constructor Create(const ASourceData: TBytes;
      AScanlineDecoder: IdxPDFImageScanlineDecoder);
    class function CreateImageScanlineSource(const ASourceData: TBytes;
      AImage: IdxPDFDocumentImage; AComponentCount: Integer)
      : TdxPDFCommonImageScanlineSource; static;
  end;

  { TdxPDFImageScanlineDecoder }

  TdxPDFImageScanlineDecoder = class(TInterfacedObject,
    IdxPDFImageScanlineDecoder)
  strict private
    FColorKey: TdxPDFRanges;
    FComponentCount: Integer;
    FIsColorKeyPresent: Boolean;
    FWidth: Integer;
  protected
    // IdxPDFImageScanlineDecoder
    function GetComponentCount: Integer;
    function GetIsColorKeyPresent: Boolean;
    function GetStride: Integer; virtual; abstract;
    procedure FillNextScanline(var AScanline: TBytes; const ASourceData: TBytes;
      ASourceOffset: Integer); virtual; abstract;

    property ColorKey: TdxPDFRanges read FColorKey;
    property ComponentCount: Integer read GetComponentCount;
    property IsColorKeyPresent: Boolean read GetIsColorKeyPresent;
    property Width: Integer read FWidth;
  public
    constructor Create(const AImage: IdxPDFDocumentImage;
      AComponentCount: Integer); virtual;
    class function CreateImageScanlineDecoder(const AImage: IdxPDFDocumentImage;
      AComponentCount: Integer): TdxPDFImageScanlineDecoder; static;
  end;

  { TdxPDFImageScanlineSourceDecorator }

  TdxPDFImageScanlineSourceDecorator = class(TInterfacedObject,
    IdxPDFImageScanlineSource)
  strict private
    FSource: IdxPDFImageScanlineSource;
    FSourceWidth: Integer;
  strict protected
    // IdxPDFImageScanlineSource
    function GetComponentCount: Integer; virtual;
    function GetHasAlpha: Boolean;
    procedure FillBuffer(var ABuffer: TBytes; AScanlineCount: Integer);
    procedure FillNextScanline(var AScanline: TBytes); virtual; abstract;

    property ComponentCount: Integer read GetComponentCount;
    property HasAlpha: Boolean read GetHasAlpha;
    property Source: IdxPDFImageScanlineSource read FSource;
    property SourceWidth: Integer read FSourceWidth;
  public
    constructor Create(ASource: IdxPDFImageScanlineSource;
      ASourceWidth: Integer); virtual;
    destructor Destroy; override;
  end;

  { TdxPDFDecodeImageScanlineSource }

  TdxPDFDecodeImageScanlineSource = class(TdxPDFImageScanlineSourceDecorator)
  strict private
    FDecode: TdxPDFRanges;
    FFactor: Double;
  strict protected
    procedure FillNextScanline(var AScanline: TBytes); override;
  public
    constructor Create(const ADecode: TdxPDFRanges; AImageWidth: Integer;
      const ASource: IdxPDFImageScanlineSource); reintroduce;
  end;

  { TdxPDFCMYKToRGBImageScanlineSource }

  TdxPDFCMYKToRGBImageScanlineSource = class(TdxPDFImageScanlineSourceDecorator)
  strict private
    FSourceScanline: TBytes;
  strict protected
    function GetComponentCount: Integer; override;
    procedure FillNextScanline(var AScanline: TBytes); override;
  public
    constructor Create(ASource: IdxPDFImageScanlineSource;
      ASourceWidth: Integer); override;
    class procedure ConvertCMYKToRGB(ACyan, AMagenta, AYellow, ABlack: Byte;
      ADestinationOffset: Integer; var ADestination: TBytes); static;
  end;

  { TdxPDFGrayToRGBImageScanlineSource }

  TdxPDFGrayToRGBImageScanlineSource = class(TdxPDFImageScanlineSourceDecorator)
  strict private
    FSourceScanline: TBytes;
  strict protected
    function GetComponentCount: Integer; override;
    procedure FillNextScanline(var AScanline: TBytes); override;
  public
    constructor Create(ASource: IdxPDFImageScanlineSource;
      ASourceWidth: Integer); override;
  end;

  { TdxPDFCIEBasedImageScanlineSource }

  TdxPDFCIEBasedImageScanlineSource = class(TdxPDFImageScanlineSourceDecorator)
  strict private
    FColorSpace: TObject;
    FSourceComponentCount: Integer;
    FSourceScanline: TBytes;
  strict protected
    function GetComponentCount: Integer; override;
    procedure FillNextScanline(var AScanline: TBytes); override;

    procedure Decode(const APixelBuffer: TDoubleDynArray; const AData: TBytes;
      AOffset: Integer); virtual;
  public
    constructor Create(const ASource: IdxPDFImageScanlineSource;
      AColorSpace: TObject; AWidth, ASourceComponentCount: Integer);
      reintroduce;
  end;

  { TdxPDFIndexedColorSpaceImageScanlineSource }

  TdxPDFIndexedColorSpaceImageScanlineSource = class
    (TdxPDFImageScanlineSourceDecorator)
  strict private
    FBaseColorSpaceComponentCount: Integer;
    FBuffer: TBytes;
    FLookupTable: TBytes;
    FShift: Integer;
  protected
    function GetComponentCount: Integer; override;
    procedure FillNextScanline(var AScanline: TBytes); override;
  public
    constructor Create(const ASource: IdxPDFImageScanlineSource;
      AWidth, ABitsPerComponent: Integer; const ALookupTable: TBytes;
      ABaseColorSpaceComponentCount: Integer); reintroduce;
  end;

  { TdxPDFLabColorSpaceImageScanlineSource }

  TdxPDFLabColorSpaceImageScanlineSource = class
    (TdxPDFCIEBasedImageScanlineSource)
  strict private
    FDecode: TdxPDFRanges;
  protected
    procedure Decode(const APixelBuffer: TDoubleDynArray; const AData: TBytes;
      AOffset: Integer); override;
  public
    constructor Create(const ASource: IdxPDFImageScanlineSource;
      AColorSpace: TObject; const ADecode: TdxPDFRanges; AWidth: Integer);
      reintroduce;
  end;

  { TdxPDFImageDataSource }

  TdxPDFImageDataSource = class(TInterfacedObject, IdxPDFImageScanlineSource)
  strict private
    FSource: IdxPDFImageScanlineSource;
    FSourceScanline: TBytes;
    FWidth: Integer;
  strict protected
  const
    DefaultComponentCount = 3;
  strict protected
    // IdxPDFImageScanlineSource
    function GetComponentCount: Integer; virtual;
    function GetHasAlpha: Boolean; virtual;
    procedure FillNextScanline(var AScanline: TBytes);
    function GetNextSourceScanline: TBytes;

    property Width: Integer read FWidth;
  public
    constructor Create(const ASource: IdxPDFImageScanlineSource;
      AWidth, ASourceScanlineLength: Integer); virtual;
    destructor Destroy; override;
    procedure FillBuffer(var ABuffer: TBytes; AScanlineCount: Integer);
      virtual; abstract;
  end;

  { TdxPDFScanlineTransformationResult }

  TdxPDFScanlineTransformationResult = record
  strict private
    FPixelFormat: TdxPDFPixelFormat;
    FScanlineSource: IdxPDFImageScanlineSource;
    FIsNull: Boolean;
  public
    class function Create(const AScanlineSource: IdxPDFImageScanlineSource)
      : TdxPDFScanlineTransformationResult; overload; static;
    class function Create(const AScanlineSource: IdxPDFImageScanlineSource;
      APixelFormat: TdxPDFPixelFormat): TdxPDFScanlineTransformationResult;
      overload; static;
    class function Null: TdxPDFScanlineTransformationResult; static;
    function IsNull: Boolean;

    property PixelFormat: TdxPDFPixelFormat read FPixelFormat;
    property ScanlineSource: IdxPDFImageScanlineSource read FScanlineSource;
  end;

  { TdxPDFBGRImageDataSource }

  TdxPDFBGRImageDataSource = class(TdxPDFImageDataSource)
  strict private
    FComponentCount: Integer;
    FStride: Integer;
  strict protected
    function GetComponentCount: Integer; override;
  public
    constructor Create(const ASource: IdxPDFImageScanlineSource;
      AWidth, AStride, AComponentCount: Integer); reintroduce;
    procedure FillBuffer(var ABuffer: TBytes; AScanlineCount: Integer);
      override;
  end;

  { TdxPDFRGBImageDataSource }

  TdxPDFRGBImageDataSource = class(TdxPDFImageDataSource)
  strict private
    FComponentCount: Integer;
    FStride: Integer;
  strict protected
    function GetComponentCount: Integer; override;
  public
    constructor Create(const ASource: IdxPDFImageScanlineSource;
      AWidth, AStride, AComponentCount: Integer); reintroduce;
    procedure FillBuffer(var ABuffer: TBytes; AScanlineCount: Integer);
      override;
  end;

  { TdxPDFColorKeyMaskedImageDataSource }

  TdxPDFColorKeyMaskedImageDataSource = class(TdxPDFImageDataSource)
  strict protected
    function GetComponentCount: Integer; override;
    function GetHasAlpha: Boolean; override;
  public
    constructor Create(const ASource: IdxPDFImageScanlineSource;
      AWidth: Integer); reintroduce;
    procedure FillBuffer(var ABuffer: TBytes; AScanlineCount: Integer);
      override;
  end;

  { TdxPDFTransparentImageDataSource }

  TdxPDFTransparentImageDataSource = class(TdxPDFImageDataSource)
  strict private
    FMaskScanline: TBytes;
    FMaskSource: IdxPDFImageScanlineSource;
  strict protected
    function GetComponentCount: Integer; override;
    function GetHasAlpha: Boolean; override;
  public
    constructor Create(const ASource, AMaskSource: IdxPDFImageScanlineSource;
      AWidth: Integer); reintroduce;
    procedure FillBuffer(var ABuffer: TBytes; AScanlineCount: Integer);
      override;
  end;

  { TdxPDFTransparentMatteImageDataSource }

  TdxPDFTransparentMatteImageDataSource = class(TdxPDFImageDataSource)
  strict private
    FMaskScanline: TBytes;
    FMaskSource: IdxPDFImageScanlineSource;
    FMatte: TDoubleDynArray;
  strict protected
    function GetComponentCount: Integer; override;
    function GetHasAlpha: Boolean; override;
  public
    constructor Create(const ASource, AMaskSource: IdxPDFImageScanlineSource;
      AWidth: Integer; const AMatte: TDoubleDynArray); reintroduce;
    destructor Destroy; override;
    procedure FillBuffer(var ABuffer: TBytes; AScanlineCount: Integer);
      override;
  end;

  { TdxPDFNonByteAlignedImageScanlineDecoder }

  TdxPDFNonByteAlignedImageScanlineDecoder = class(TdxPDFImageScanlineDecoder)
  strict private
    FAlignedStride: Integer;
    FBitsPerComponent: Integer;
    FFactor: Integer;
    FMask: Integer;
  strict protected
    function GetStride: Integer; override;
    procedure FillNextScanline(var AScanline: TBytes; const ASourceData: TBytes;
      ASourceOffset: Integer); override;
  public
    constructor Create(const AImage: IdxPDFDocumentImage;
      AComponentCount: Integer); override;
  end;

  { TdxPDFByteAlignedImageScanlineDecoder }

  TdxPDFByteAlignedImageScanlineDecoder = class(TdxPDFImageScanlineDecoder)
  strict private
    FStep: Integer;
  strict protected
    function GetStride: Integer; override;
    procedure FillNextScanline(var AScanline: TBytes; const ASourceData: TBytes;
      ASourceOffset: Integer); override;
  public
    constructor Create(const AImage: IdxPDFDocumentImage;
      AComponentCount, AStep: Integer); reintroduce;
  end;

  { TdxPDFImageScanlineSourceFactory }

  TdxPDFImageScanlineSourceFactory = class
  strict private
    FFactory: IdxPDFImageScanlineSourceFactory;
    function GetFactory: IdxPDFImageScanlineSourceFactory;
  public
    destructor Destroy; override;
    function CreateInterpolator(const ASource: IdxPDFImageScanlineSource;
      ATargetWidth, ATargetHeight, ASourceWidth, ASourceHeight: Integer;
      AShouldInterpolate: Boolean): IdxPDFImageScanlineSource;
    function CreateIndexedScanlineSource(const ASource
      : IdxPDFImageScanlineSource; AWidth, AHeight, ABitsPerComponent: Integer;
      const ALookupTable: TBytes; ABaseColorSpaceComponentCount: Integer)
      : IdxPDFImageScanlineSource;
  end;

  { TdxPDFDefaultScanlineSourceFactory }

  TdxPDFDefaultScanlineSourceFactory = class(TInterfacedObject,
    IdxPDFImageScanlineSourceFactory)
  public
    function CreateInterpolator(const ASource: IdxPDFImageScanlineSource;
      ATargetWidth, ATargetHeight, ASourceWidth: Integer;
      ASourceHeight: Integer; AShouldInterpolate: Boolean)
      : IdxPDFImageScanlineSource; virtual;
    function CreateIndexedScanlineSource(const ASource
      : IdxPDFImageScanlineSource; AWidth, AHeight, ABitsPerComponent: Integer;
      const ALookupTable: TBytes; ABaseColorSpaceComponentCount: Integer)
      : IdxPDFImageScanlineSource; virtual;
  end;

  { TdxPDFImageDataCacheItem }

  TdxPDFImageDataCacheItem = class
  strict private
    FImage: IdxPDFDocumentImage;
    FPalette: TdxPDFARGBColorArray;
    FParameters: TdxPDFImageParameters;
    FPixelFormat: TdxPDFPixelFormat;
    FStride: Integer;
  public
    constructor Create(const AImageData: TdxPDFImageData;
      const AImage: IdxPDFDocumentImage);
    procedure DrawImage(const APainter: IdxPDFImageCachePainter); virtual;

    property Image: IdxPDFDocumentImage read FImage;
    property Palette: TdxPDFARGBColorArray read FPalette;
    property Parameters: TdxPDFImageParameters read FParameters
      write FParameters;
    property PixelFormat: TdxPDFPixelFormat read FPixelFormat;
    property Stride: Integer read FStride;
  end;

  { TdxPDFImageDataRasterCacheItem }

  TdxPDFImageDataRasterCacheItem = class(TdxPDFImageDataCacheItem)
  strict private
    FData: TBytes;
  public
    constructor Create(const AImageData: TdxPDFImageData;
      const AImage: IdxPDFDocumentImage; const AData: TBytes);
    procedure DrawImage(const APainter: IdxPDFImageCachePainter); override;
  end;

  { TdxPDFImageDataSourceCacheItem }

  TdxPDFImageDataSourceCacheItem = class(TdxPDFImageDataCacheItem)
  strict private
    FDataSource: IdxPDFImageScanlineSource;
  public
    constructor Create(const AImageData: TdxPDFImageData;
      const AImage: IdxPDFDocumentImage);
    destructor Destroy; override;
    procedure DrawImage(const APainter: IdxPDFImageCachePainter); override;
  end;

  { TdxPDFCacheItem }

  TdxPDFImageCacheItem = record
  strict private
    FData: TdxPDFImageDataCacheItem;
    FIsNull: Boolean;
    FShouldDispose: Boolean;
  public
    class function Create(AData: TdxPDFImageDataCacheItem;
      AShouldDispose: Boolean): TdxPDFImageCacheItem; static;
    class function Null: TdxPDFImageCacheItem; static;
    function IsNull: Boolean;

    property Data: TdxPDFImageDataCacheItem read FData;
    property ShouldDispose: Boolean read FShouldDispose;
  end;

  { TdxPDFCacheItem }

  TdxPDFCacheItem = record
  strict private
    FIsNull: Boolean;
    FNode: IdxPDFDocumentImage;
    FParameters: TdxPDFImageParameters;
    FSize: Int64;
    FValue: TdxPDFImageDataCacheItem;
  public
    class function Create: TdxPDFCacheItem; overload; static;
    class function Create(AValue: TdxPDFImageDataCacheItem;
      const ANode: IdxPDFDocumentImage;
      const AParameters: TdxPDFImageParameters; ASize: Int64): TdxPDFCacheItem;
      overload; static;
    function IsNull: Boolean;

    property Node: IdxPDFDocumentImage read FNode;
    property Parameters: TdxPDFImageParameters read FParameters
      write FParameters;
    property Size: Int64 read FSize;
    property Value: TdxPDFImageDataCacheItem read FValue;
  end;

  { TdxPDFImageCache }

  TdxPDFImageCache = class
  strict private
    FCapacity: Int64;
    FObjectStorage: TDictionary<IdxPDFDocumentImage, TdxPDFCacheItem>;
    FRecentImages: TQueue<IdxPDFDocumentImage>;
    procedure SetCapacity(const AValue: Int64);
  strict protected
    FSize: Int64;
  protected
    function CreateValue(const AImage: IdxPDFDocumentImage;
      const AImageParameters: TdxPDFImageParameters): TdxPDFCacheItem;
      virtual; abstract;
    function ShouldReplaceImage(const AImageParameters, AOldParameters
      : TdxPDFImageParameters): Boolean; virtual;
    procedure CreateSubClasses; virtual;
    procedure DestroySubClasses; virtual;
    procedure RemoveItem(const AImage: IdxPDFDocumentImage;
      AValue: TdxPDFCacheItem); overload;
    procedure TrimCache(const ARecentImage: IdxPDFDocumentImage);
  public
    constructor Create(ACapacity: Int64); virtual;
    destructor Destroy; override;

    function GetImage(const AImage: IdxPDFDocumentImage;
      const AImageParameters: TdxPDFImageParameters): TdxPDFImageCacheItem;
    procedure Clear;
    procedure RemoveItem(const AImage: IdxPDFDocumentImage); overload;

    property Capacity: Int64 read FCapacity write SetCapacity;
    property Size: Int64 read FSize;
  end;

  { TdxPDFImageDataCache }

  TdxPDFImageDataCache = class(TdxPDFImageCache)
  protected
    function CreateValue(const AImage: IdxPDFDocumentImage;
      const AImageParameters: TdxPDFImageParameters): TdxPDFCacheItem; override;
  public
    class function GetImageRaster(const AImageData: TdxPDFImageData)
      : TBytes; static;
  end;

  { TdxPDFJPXImageScanlineSource }

  TdxPDFJPXImageScanlineSource = class(TInterfacedObject,
    IdxPDFImageScanlineSource)
  strict private
    FComponentCount: Integer;
    FImage: TdxJPXImage;
    FOffset: Integer;
    FStartTileIndex: Integer;
    FTiles: TArray<TdxJPXTile>;
    FTilesToDestroy: TObjectList<TdxJPXTile>;
    FHasAlpha: Boolean;

    function GetComponentCount: Integer;
    function GetHasAlpha: Boolean;
    procedure FillBuffer(var ABuffer: TBytes; AScanlineCount: Integer);
    procedure FillNextScanline(var AScanline: TBytes);
  public
    constructor Create(AImage: TdxJPXImage; AComponentCount: Integer;
      AHasAlpha: Boolean);
    destructor Destroy; override;

    procedure FillNextTileRow;
  end;

function dxPDFImageScanlineSourceFactory: TdxPDFImageScanlineSourceFactory;

implementation

uses
  Math, dxCore, dxPDFCore, dxPDFUtils, dxPDFImageInterpolator, dxPDFWIC;

var
  dxgPDFImageScanlineSourceFactory: TdxPDFImageScanlineSourceFactory;

function dxPDFImageScanlineSourceFactory: TdxPDFImageScanlineSourceFactory;
begin
  if dxgPDFImageScanlineSourceFactory = nil then
    dxgPDFImageScanlineSourceFactory := TdxPDFImageScanlineSourceFactory.Create;
  Result := dxgPDFImageScanlineSourceFactory;
end;

{ TdxPDFJPXImageScanlineSource }

constructor TdxPDFJPXImageScanlineSource.Create(AImage: TdxJPXImage;
  AComponentCount: Integer; AHasAlpha: Boolean);
begin
  inherited Create;
  FStartTileIndex := -1;
  FImage := AImage;
  FComponentCount := AComponentCount;
  FHasAlpha := AHasAlpha;
  FTilesToDestroy := TObjectList<TdxJPXTile>.Create;
  SetLength(FTiles, AImage.NumXTiles);
end;

destructor TdxPDFJPXImageScanlineSource.Destroy;
begin
  FreeAndNil(FTilesToDestroy);
  FreeAndNil(FImage);
  inherited Destroy;
end;

procedure TdxPDFJPXImageScanlineSource.FillNextTileRow;
var
  I: Integer;
begin
  FTilesToDestroy.Clear;
  TdxPDFTask.ForEach(0, FImage.NumXTiles, procedure(AIndex: Integer)
    begin
      FTiles[AIndex] := FImage.CreateTile(AIndex + FStartTileIndex);
    end);
  for I := 0 to FImage.NumXTiles - 1 do
    FTilesToDestroy.Add(FTiles[I]);
end;

function TdxPDFJPXImageScanlineSource.GetComponentCount: Integer;
begin
  Result := FComponentCount;
end;

function TdxPDFJPXImageScanlineSource.GetHasAlpha: Boolean;
begin
  Result := FHasAlpha;
end;

procedure TdxPDFJPXImageScanlineSource.FillBuffer(var ABuffer: TBytes;
AScanlineCount: Integer);
begin
  // do nothing
end;

procedure TdxPDFJPXImageScanlineSource.FillNextScanline(var AScanline: TBytes);
var
  AComponentCount, ADataLength, ANewStartTileIndex, I, Y: Integer;
  ATile: TdxJPXTile;
begin
  ADataLength := Length(AScanline);
  Y := FOffset div ADataLength;
  ANewStartTileIndex := FImage.NumXTiles * (Y div FImage.Size.TileHeight);
  if ANewStartTileIndex <> FStartTileIndex then
  begin
    FStartTileIndex := ANewStartTileIndex;
    FillNextTileRow;
  end;
  AComponentCount := Length(FImage.CodingStyleComponents);
  for I := 0 to FImage.NumXTiles - 1 do
  begin
    ATile := FTiles[I];
    ATile.ColorTransformation.TransformLine(AScanline,
      AComponentCount * ATile.X0, Y mod FImage.Size.TileHeight);
  end;
  Inc(FOffset, ADataLength);
end;

{ TdxPDFImageScanline }

class function TdxPDFImageScanline.Create(AWidth, AComponentCount: Integer)
  : TdxPDFImageScanline;
begin
  SetLength(Result.FMaskData, AWidth);
  SetLength(Result.FScanlineData, AWidth * AComponentCount);
end;

{ TdxPDFSourceImagePixelInfo }

class function TdxPDFSourceImagePixelInfo.Create(AIndex: Integer;
AFactor: TdxPDFFixedPointNumber): TdxPDFSourceImagePixelInfo;
begin
  Result.FIndex := AIndex;
  Result.FFactor := AFactor;
end;

{ TdxPDFInvertedImageScanlineSource }

constructor TdxPDFInvertedImageScanlineSource.Create(const ASource
  : IdxPDFImageScanlineSource);
begin
  inherited Create;
  FSource := ASource;
end;

destructor TdxPDFInvertedImageScanlineSource.Destroy;
begin
  inherited Destroy;
end;

function TdxPDFInvertedImageScanlineSource.GetComponentCount: Integer;
begin
  Result := FSource.ComponentCount;
end;

function TdxPDFInvertedImageScanlineSource.GetHasAlpha: Boolean;
begin
  Result := FSource.HasAlpha;
end;

procedure TdxPDFInvertedImageScanlineSource.FillBuffer(var ABuffer: TBytes;
AScanlineCount: Integer);
begin
  // do nothing
end;

procedure TdxPDFInvertedImageScanlineSource.FillNextScanline
  (var AScanline: TBytes);
var
  I: Integer;
begin
  FSource.FillNextScanline(AScanline);
  for I := 0 to Length(AScanline) - 1 do
    AScanline[I] := 255 xor AScanline[I];
end;

{ TdxPDFCommonImageScanlineSource }

constructor TdxPDFCommonImageScanlineSource.Create(const ASourceData: TBytes;
AScanlineDecoder: IdxPDFImageScanlineDecoder);
begin
  inherited Create;
  FSourceData := ASourceData;
  FScanlineDecoder := AScanlineDecoder;
end;

class function TdxPDFCommonImageScanlineSource.CreateImageScanlineSource
  (const ASourceData: TBytes; AImage: IdxPDFDocumentImage;
AComponentCount: Integer): TdxPDFCommonImageScanlineSource;
begin
  Result := TdxPDFCommonImageScanlineSource.Create(ASourceData,
    TdxPDFImageScanlineDecoder.CreateImageScanlineDecoder(AImage,
    AComponentCount));
end;

function TdxPDFCommonImageScanlineSource.GetComponentCount: Integer;
begin
  Result := FScanlineDecoder.ComponentCount + IfThen(HasAlpha, 1, 0);
end;

function TdxPDFCommonImageScanlineSource.GetHasAlpha: Boolean;
begin
  Result := FScanlineDecoder.IsColorKeyPresent;
end;

procedure TdxPDFCommonImageScanlineSource.FillBuffer(var ABuffer: TBytes;
AScanlineCount: Integer);
begin
  // do nothing
end;

procedure TdxPDFCommonImageScanlineSource.FillNextScanline
  (var AScanline: TBytes);
begin
  FScanlineDecoder.FillNextScanline(AScanline, FSourceData, FSourceOffset);
  FSourceOffset := FSourceOffset + FScanlineDecoder.Stride;
end;

{ TdxPDFImageScanlineDecoder }

constructor TdxPDFImageScanlineDecoder.Create(const AImage: IdxPDFDocumentImage;
AComponentCount: Integer);
begin
  inherited Create;
  FComponentCount := AComponentCount;
  FWidth := AImage.GetWidth;
  FColorKey := AImage.GetColorKeyMask;
  FIsColorKeyPresent := (FColorKey <> nil) and (Length(FColorKey) > 0);
end;

class function TdxPDFImageScanlineDecoder.CreateImageScanlineDecoder
  (const AImage: IdxPDFDocumentImage; AComponentCount: Integer)
  : TdxPDFImageScanlineDecoder;
begin
  case AImage.GetBitsPerComponent of
    1, 2, 4:
      Result := TdxPDFNonByteAlignedImageScanlineDecoder.Create(AImage,
        AComponentCount);
    16:
      Result := TdxPDFByteAlignedImageScanlineDecoder.Create(AImage,
        AComponentCount, 2);
  else
    Result := TdxPDFByteAlignedImageScanlineDecoder.Create(AImage,
      AComponentCount, 1);
  end;
end;

function TdxPDFImageScanlineDecoder.GetComponentCount: Integer;
begin
  Result := FComponentCount;
end;

function TdxPDFImageScanlineDecoder.GetIsColorKeyPresent: Boolean;
begin
  Result := FIsColorKeyPresent;
end;

{ TdxPDFNonByteAlignedImageScanlineDecoder }

constructor TdxPDFNonByteAlignedImageScanlineDecoder.Create
  (const AImage: IdxPDFDocumentImage; AComponentCount: Integer);
begin
  inherited Create(AImage, AComponentCount);
  FBitsPerComponent := AImage.GetBitsPerComponent;
  FMask := (1 shl FBitsPerComponent) - 1;
  FFactor := 255 div FMask;
  FAlignedStride := Ceil(Width * AComponentCount * FBitsPerComponent / 8);
end;

function TdxPDFNonByteAlignedImageScanlineDecoder.GetStride: Integer;
begin
  Result := FAlignedStride;
end;

procedure TdxPDFNonByteAlignedImageScanlineDecoder.FillNextScanline
  (var AScanline: TBytes; const ASourceData: TBytes; ASourceOffset: Integer);
var
  ABitOffset, I, ADataOffset, AComponent, AComponentCount, ABitsOffset, AValue,
    ASourceDataLength: Integer;
  AIsMasked, AIsColorKeyPresent: Boolean;
begin
  ASourceDataLength := Length(ASourceData);
  ABitOffset := ASourceOffset * 8;
  ADataOffset := 0;
  AIsColorKeyPresent := IsColorKeyPresent;
  AComponentCount := ComponentCount;
  for I := 0 to Width - 1 do
  begin
    AIsMasked := AIsColorKeyPresent;
    for AComponent := 0 to AComponentCount - 1 do
    begin
      ABitsOffset := 8 - FBitsPerComponent - ABitOffset mod 8;
      if ASourceDataLength > 0 then
        AValue := (ASourceData[ABitOffset div 8] shr ABitsOffset) and FMask
      else
        AValue := (255 shr ABitsOffset) and FMask;
      AIsMasked := AIsMasked and AIsColorKeyPresent and ColorKey[AComponent]
        .Contains(AValue);
      AScanline[ADataOffset] := AValue * FFactor;
      Inc(ADataOffset);
      Inc(ABitOffset, FBitsPerComponent);
    end;
    if AIsColorKeyPresent then
    begin
      if AIsMasked then
        AScanline[ADataOffset] := 0
      else
        AScanline[ADataOffset] := 255;
      Inc(ADataOffset);
    end;
  end;
end;

{ TdxPDFByteAlignedImageScanlineDecoder }

constructor TdxPDFByteAlignedImageScanlineDecoder.Create
  (const AImage: IdxPDFDocumentImage; AComponentCount, AStep: Integer);
begin
  inherited Create(AImage, AComponentCount);
  FStep := AStep;
end;

function TdxPDFByteAlignedImageScanlineDecoder.GetStride: Integer;
begin
  Result := Width * ComponentCount * FStep;
end;

procedure TdxPDFByteAlignedImageScanlineDecoder.FillNextScanline
  (var AScanline: TBytes; const ASourceData: TBytes; ASourceOffset: Integer);
var
  I, ADataOffset, AComponent, AComponentCount, ASourceDataLength: Integer;
  AIsMasked, AIsColorKeyPresent: Boolean;
  AValue: Byte;
begin
  ASourceDataLength := Length(ASourceData);
  ADataOffset := 0;
  AIsColorKeyPresent := IsColorKeyPresent;
  AComponentCount := ComponentCount;
  for I := 0 to Width - 1 do
  begin
    AIsMasked := AIsColorKeyPresent;
    for AComponent := 0 to AComponentCount - 1 do
    begin
      if ASourceDataLength > 0 then
        AValue := ASourceData[ASourceOffset]
      else
        AValue := 255;
      AIsMasked := AIsMasked and AIsColorKeyPresent and ColorKey[AComponent]
        .Contains(AValue);
      AScanline[ADataOffset] := AValue;
      Inc(ADataOffset);
      Inc(ASourceOffset, FStep);
    end;
    if AIsColorKeyPresent then
    begin
      if AIsMasked then
        AScanline[ADataOffset] := 0
      else
        AScanline[ADataOffset] := 255;
      Inc(ADataOffset);
    end;
  end;
end;

{ TdxPDFImageScanlineSourceDecorator }

constructor TdxPDFImageScanlineSourceDecorator.Create
  (ASource: IdxPDFImageScanlineSource; ASourceWidth: Integer);
begin
  inherited Create;
  FSource := ASource;
  FSourceWidth := ASourceWidth;
end;

destructor TdxPDFImageScanlineSourceDecorator.Destroy;
begin
  FSource := nil;
  inherited Destroy;
end;

function TdxPDFImageScanlineSourceDecorator.GetComponentCount: Integer;
begin
  Result := FSource.ComponentCount;
end;

function TdxPDFImageScanlineSourceDecorator.GetHasAlpha: Boolean;
begin
  Result := FSource.HasAlpha;
end;

procedure TdxPDFImageScanlineSourceDecorator.FillBuffer(var ABuffer: TBytes;
AScanlineCount: Integer);
begin
  // do nothing
end;

{ TdxPDFDecodeImageScanlineSource }

constructor TdxPDFDecodeImageScanlineSource.Create(const ADecode: TdxPDFRanges;
AImageWidth: Integer; const ASource: IdxPDFImageScanlineSource);
begin
  inherited Create(ASource, AImageWidth);
  FFactor := 1 / 255.0;
  FDecode := ADecode;
end;

procedure TdxPDFDecodeImageScanlineSource.FillNextScanline
  (var AScanline: TBytes);
var
  I, AComponentCount, AStep, W, ASrc: Integer;
  ADecodeArrayEntry: TdxPDFRange;
  AMin, AMax: Double;
begin
  Source.FillNextScanline(AScanline);
  AComponentCount := Length(FDecode);
  AStep := Source.ComponentCount - AComponentCount;
  ASrc := 0;
  for W := 0 to SourceWidth - 1 do
  begin
    for I := 0 to AComponentCount - 1 do
    begin
      ADecodeArrayEntry := FDecode[I];
      AMin := ADecodeArrayEntry.Min;
      AMax := ADecodeArrayEntry.Max;
      AScanline[ASrc] := TdxPDFUtils.ConvertToByte
        ((AMin + AScanline[ASrc] * (AMax - AMin) * FFactor) * 255);
      Inc(ASrc);
    end;
    Inc(ASrc, AStep);
  end;
end;

{ TdxPDFCMYKToRGBImageScanlineSource }

constructor TdxPDFCMYKToRGBImageScanlineSource.Create
  (ASource: IdxPDFImageScanlineSource; ASourceWidth: Integer);
begin
  inherited Create(ASource, ASourceWidth);
  SetLength(FSourceScanline, ASourceWidth * ASource.ComponentCount);
end;

class procedure TdxPDFCMYKToRGBImageScanlineSource.ConvertCMYKToRGB(ACyan,
  AMagenta, AYellow, ABlack: Byte; ADestinationOffset: Integer;
var ADestination: TBytes);
var
  ACyanComplement, AMagentaComplement, AYellowComplement, ABlackComplement,
    ABlackDiv, AAddition, ARed, AGreen, ABlue: Double;
  D: Integer;
begin
  if ABlack <> 255 then
  begin
    ACyanComplement := 255 - ACyan;
    AMagentaComplement := 255 - AMagenta;
    AYellowComplement := 255 - AYellow;
    ABlackComplement := 255 - ABlack;
    ABlackDiv := ABlack / ABlackComplement;

    AAddition := ACyanComplement * AMagentaComplement * AYellowComplement *
      ABlackComplement;
    ARed := AAddition;
    AGreen := AAddition;
    ABlue := AAddition;

    AAddition := AAddition * ABlackDiv;
    ARed := ARed + 0.1373 * AAddition;
    AGreen := AGreen + 0.1216 * AAddition;
    ABlue := ABlue + 0.1255 * AAddition;
    AAddition := ACyanComplement * AMagentaComplement * AYellow *
      ABlackComplement;
    ARed := ARed + AAddition;
    AGreen := AGreen + 0.9490 * AAddition;

    AAddition := AAddition * ABlackDiv;
    ARed := ARed + 0.1098 * AAddition;
    AGreen := AGreen + 0.1020 * AAddition;
    AAddition := ACyanComplement * AMagenta * AYellowComplement *
      ABlackComplement;
    ARed := ARed + 0.9255 * AAddition;
    ABlue := ABlue + 0.5490 * AAddition;

    ARed := ARed + 0.1412 * (AAddition * ABlackDiv);
    AAddition := ACyanComplement * AMagenta * AYellow * ABlackComplement;
    ARed := ARed + 0.9294 * AAddition;
    AGreen := AGreen + 0.1098 * AAddition;
    ABlue := ABlue + 0.1412 * AAddition;
    ARed := ARed + 0.1333 * AAddition * ABlackDiv;
    AAddition := ACyan * AMagentaComplement * AYellowComplement *
      ABlackComplement;
    AGreen := AGreen + 0.6784 * AAddition;
    ABlue := ABlue + 0.9373 * AAddition;

    AAddition := AAddition * ABlackDiv;
    AGreen := AGreen + 0.0588 * AAddition;
    ABlue := ABlue + 0.1412 * AAddition;
    AAddition := ACyan * AMagentaComplement * AYellow * ABlackComplement;
    AGreen := AGreen + 0.6510 * AAddition;
    ABlue := ABlue + 0.3137 * AAddition;
    AGreen := AGreen + 0.0745 * (ACyan * AMagentaComplement * AYellow * ABlack);
    AAddition := ACyan * AMagenta * AYellowComplement * ABlackComplement;
    ARed := ARed + 0.1804 * AAddition;
    AGreen := AGreen + 0.1922 * AAddition;
    ABlue := ABlue + 0.5725 * AAddition;
    ABlue := ABlue + 0.0078 * (AAddition * ABlackDiv);
    AAddition := ACyan * AMagenta * AYellow * ABlackComplement;

    D := 16581375;
    ADestination[ADestinationOffset] := Trunc((ARed + 0.2118 * AAddition) / D);
    ADestination[ADestinationOffset + 1] :=
      Trunc((AGreen + 0.2119 * AAddition) / D);
    ADestination[ADestinationOffset + 2] :=
      Trunc((ABlue + 0.2235 * AAddition) / D);
  end
  else
  begin
    ADestination[ADestinationOffset] := 0;
    ADestination[ADestinationOffset + 1] := 0;
    ADestination[ADestinationOffset + 2] := 0;
  end;
end;

function TdxPDFCMYKToRGBImageScanlineSource.GetComponentCount: Integer;
begin
  Result := inherited GetComponentCount - 1;
end;

procedure TdxPDFCMYKToRGBImageScanlineSource.FillNextScanline
  (var AScanline: TBytes);
var
  I, ASourceOffset, ADestinationOffset: Integer;
begin
  Source.FillNextScanline(FSourceScanline);
  ASourceOffset := 0;
  ADestinationOffset := 0;
  for I := 0 to SourceWidth - 1 do
  begin
    ConvertCMYKToRGB(FSourceScanline[ASourceOffset],
      FSourceScanline[ASourceOffset + 1], FSourceScanline[ASourceOffset + 2],
      FSourceScanline[ASourceOffset + 3], ADestinationOffset, AScanline);
    Inc(ASourceOffset, 4);
    if HasAlpha then
    begin
      AScanline[ADestinationOffset + 3] := FSourceScanline[ASourceOffset];
      Inc(ASourceOffset);
    end;
    Inc(ADestinationOffset, ComponentCount);
  end;
end;

{ TdxPDFGrayToRGBImageScanlineSource }

constructor TdxPDFGrayToRGBImageScanlineSource.Create
  (ASource: IdxPDFImageScanlineSource; ASourceWidth: Integer);
begin
  inherited Create(ASource, ASourceWidth);
  SetLength(FSourceScanline, SourceWidth * ASource.ComponentCount);
end;

function TdxPDFGrayToRGBImageScanlineSource.GetComponentCount: Integer;
begin
  Result := inherited GetComponentCount + 2;
end;

procedure TdxPDFGrayToRGBImageScanlineSource.FillNextScanline
  (var AScanline: TBytes);
var
  I, J, ADestinationOffset, ASourceOffset: Integer;
  AValue: Byte;
begin
  Source.FillNextScanline(FSourceScanline);
  ADestinationOffset := 0;
  ASourceOffset := 0;
  for I := 0 to SourceWidth - 1 do
  begin
    AValue := FSourceScanline[ASourceOffset];
    Inc(ASourceOffset);
    for J := 0 to 3 - 1 do
    begin
      AScanline[ADestinationOffset] := AValue;
      Inc(ADestinationOffset);
    end;
    if HasAlpha then
    begin
      AScanline[ADestinationOffset] := FSourceScanline[ASourceOffset];
      Inc(ADestinationOffset);
      Inc(ASourceOffset);
    end;
  end;
end;

{ TdxPDFCIEBasedImageScanlineSource }

constructor TdxPDFCIEBasedImageScanlineSource.Create(const ASource
  : IdxPDFImageScanlineSource; AColorSpace: TObject;
AWidth, ASourceComponentCount: Integer);
begin
  inherited Create(ASource, AWidth);
  FColorSpace := AColorSpace;
  FSourceComponentCount := ASourceComponentCount;
  SetLength(FSourceScanline, AWidth * ASource.ComponentCount);
end;

function TdxPDFCIEBasedImageScanlineSource.GetComponentCount: Integer;
begin
  Result := IfThen(HasAlpha, 4, 3);
end;

procedure TdxPDFCIEBasedImageScanlineSource.FillNextScanline
  (var AScanline: TBytes);
var
  I, J, ASourceOffset, AResultOffset: Integer;
  ASourcePixelData, AResultComponents: TDoubleDynArray;
begin
  Source.FillNextScanline(FSourceScanline);
  SetLength(ASourcePixelData, FSourceComponentCount);
  ASourceOffset := 0;
  AResultOffset := 0;
  for I := 0 to SourceWidth - 1 do
  begin
    Decode(ASourcePixelData, FSourceScanline, ASourceOffset);
    Inc(ASourceOffset, FSourceComponentCount);
    AResultComponents := (FColorSpace as TdxPDFCustomColorSpace)
      .Transform(TdxPDFColor.Create(ASourcePixelData)).Components;
    for J := 0 to 3 - 1 do
    begin
      AScanline[AResultOffset] := TdxPDFUtils.ConvertToByte
        (AResultComponents[J] * 255);
      Inc(AResultOffset);
    end;
    if HasAlpha then
    begin
      AScanline[AResultOffset] := FSourceScanline[ASourceOffset];
      Inc(AResultOffset);
      Inc(ASourceOffset);
    end;
  end;
end;

procedure TdxPDFCIEBasedImageScanlineSource.Decode(const APixelBuffer
  : TDoubleDynArray; const AData: TBytes; AOffset: Integer);
var
  J: Integer;
begin
  for J := 0 to FSourceComponentCount - 1 do
  begin
    APixelBuffer[J] := AData[AOffset] / 255.0;
    Inc(AOffset);
  end;
end;

{ TdxPDFIndexedColorSpaceImageScanlineSource }

constructor TdxPDFIndexedColorSpaceImageScanlineSource.Create
  (const ASource: IdxPDFImageScanlineSource; AWidth, ABitsPerComponent: Integer;
const ALookupTable: TBytes; ABaseColorSpaceComponentCount: Integer);
begin
  inherited Create(ASource, AWidth);
  FLookupTable := ALookupTable;
  SetLength(FBuffer, AWidth * ASource.ComponentCount);
  FBaseColorSpaceComponentCount := ABaseColorSpaceComponentCount;
  FShift := IfThen(ABitsPerComponent > 8, 0, 8 - ABitsPerComponent);
end;

function TdxPDFIndexedColorSpaceImageScanlineSource.GetComponentCount: Integer;
begin
  Result := inherited GetComponentCount + FBaseColorSpaceComponentCount - 1;
end;

procedure TdxPDFIndexedColorSpaceImageScanlineSource.FillNextScanline
  (var AScanline: TBytes);
var
  I, J, AIndex, ATargetIndex, ASourceOffset: Integer;
begin
  Source.FillNextScanline(FBuffer);
  ATargetIndex := 0;
  ASourceOffset := 0;
  for I := 0 to SourceWidth - 1 do
  begin
    AIndex := (FBuffer[ASourceOffset] shr FShift) *
      FBaseColorSpaceComponentCount;
    Inc(ASourceOffset);
    for J := 0 to FBaseColorSpaceComponentCount - 1 do
    begin
      AScanline[ATargetIndex] := FLookupTable[AIndex + J];
      Inc(ATargetIndex);
    end;
    if HasAlpha then
    begin
      AScanline[ATargetIndex] := FBuffer[ASourceOffset];
      Inc(ATargetIndex);
      Inc(ASourceOffset);
    end;
  end;
end;

{ TdxPDFLabColorSpaceImageScanlineSource }

constructor TdxPDFLabColorSpaceImageScanlineSource.Create
  (const ASource: IdxPDFImageScanlineSource; AColorSpace: TObject;
const ADecode: TdxPDFRanges; AWidth: Integer);
begin
  inherited Create(ASource, AColorSpace, AWidth, 3);
  FDecode := ADecode;
end;

procedure TdxPDFLabColorSpaceImageScanlineSource.Decode(const APixelBuffer
  : TDoubleDynArray; const AData: TBytes; AOffset: Integer);
var
  I: Integer;
  ADecodeArrayEntry: TdxPDFRange;
  AMin, AMax, AValue: Double;
begin
  for I := 0 to 3 - 1 do
  begin
    ADecodeArrayEntry := FDecode[I];
    AMin := ADecodeArrayEntry.Min;
    AMax := ADecodeArrayEntry.Max;
    AValue := AMin + AData[AOffset] * (AMax - AMin) / 255.0;
    Inc(AOffset);
    if AValue < AMin then
      APixelBuffer[I] := AMin
    else if AValue > AMax then
      APixelBuffer[I] := AMax
    else
      APixelBuffer[I] := AValue;
  end;
end;

{ TdxPDFImageData }

class function TdxPDFImageData.Create(const AData: IdxPDFImageScanlineSource;
AWidth, AHeight, AStride: Integer; APixelFormat: TdxPDFPixelFormat;
const APalette: TdxPDFARGBColorArray): TdxPDFImageData;
begin
  Result.FData := AData;
  Result.FHeight := AHeight;
  Result.FPalette := APalette;
  Result.FPixelFormat := APixelFormat;
  Result.FStride := AStride;
  Result.FWidth := AWidth;
end;

{ TdxPDFImageParameters }

class function TdxPDFImageParameters.Create: TdxPDFImageParameters;
begin
  Result.FIsNull := True;
end;

class function TdxPDFImageParameters.Create(AWidth, AHeight: Integer)
  : TdxPDFImageParameters;
begin
  Result := TdxPDFImageParameters.Create(AWidth, AHeight, True);
end;

class function TdxPDFImageParameters.Create(AWidth, AHeight: Integer;
AShouldInterpolate: Boolean): TdxPDFImageParameters;
begin
  Result.FHeight := AHeight;
  Result.FShouldInterpolate := AShouldInterpolate;
  Result.FWidth := AWidth;
  Result.FIsNull := False;
end;

function TdxPDFImageParameters.IsNull: Boolean;
begin
  Result := FIsNull;
end;

{ TdxPDFScanlineTransformationResult }

class function TdxPDFScanlineTransformationResult.Create(const AScanlineSource
  : IdxPDFImageScanlineSource): TdxPDFScanlineTransformationResult;
begin
  Result := Create(AScanlineSource, pfArgb24bpp);
end;

class function TdxPDFScanlineTransformationResult.Create(const AScanlineSource
  : IdxPDFImageScanlineSource; APixelFormat: TdxPDFPixelFormat)
  : TdxPDFScanlineTransformationResult;
begin
  Result.FPixelFormat := APixelFormat;
  Result.FScanlineSource := AScanlineSource;
  Result.FIsNull := False;
end;

class function TdxPDFScanlineTransformationResult.Null
  : TdxPDFScanlineTransformationResult;
begin
  Result.FIsNull := True;
end;

function TdxPDFScanlineTransformationResult.IsNull: Boolean;
begin
  Result := FIsNull;
end;

{ TdxPDFDefaultScanlineSourceFactory }

function TdxPDFDefaultScanlineSourceFactory.CreateInterpolator
  (const ASource: IdxPDFImageScanlineSource; ATargetWidth, ATargetHeight,
  ASourceWidth, ASourceHeight: Integer; AShouldInterpolate: Boolean)
  : IdxPDFImageScanlineSource;
var
  AResult: IdxPDFImageScanlineSource;
  AHorizontalSizeFactor, AVerticalSizeFactor: Double;
begin
  AHorizontalSizeFactor := ATargetWidth / ASourceWidth;
  AVerticalSizeFactor := ATargetHeight / ASourceHeight;
  AResult := ASource;
  if AHorizontalSizeFactor > 1 then
    AResult := TdxPDFBilinearUpsamplingHorizontalInterpolator.Create(AResult,
      ATargetWidth, ASourceWidth)
  else if AHorizontalSizeFactor < 1 then
    AResult := TdxPDFSuperSamplingHorizontalInterpolator.Create(AResult,
      ATargetWidth, ASourceWidth);
  if AVerticalSizeFactor > 1 then
    AResult := TdxPDFBilinearUpsamplingVerticalInterpolator.Create(AResult,
      ATargetWidth, ATargetHeight, ASourceHeight)
  else if AVerticalSizeFactor < 1 then
    AResult := TdxPDFSuperSamplingVerticalInterpolator.Create(AResult,
      ATargetWidth, ATargetHeight, ASourceHeight);
  Result := AResult;
end;

function TdxPDFDefaultScanlineSourceFactory.CreateIndexedScanlineSource
  (const ASource: IdxPDFImageScanlineSource;
AWidth, AHeight, ABitsPerComponent: Integer; const ALookupTable: TBytes;
ABaseColorSpaceComponentCount: Integer): IdxPDFImageScanlineSource;
begin
  Result := TdxPDFIndexedColorSpaceImageScanlineSource.Create(ASource, AWidth,
    ABitsPerComponent, ALookupTable, ABaseColorSpaceComponentCount);
end;

{ TdxPDFImageScanlineSourceFactory }

destructor TdxPDFImageScanlineSourceFactory.Destroy;
begin
  FFactory := nil;
  inherited Destroy;
end;

function TdxPDFImageScanlineSourceFactory.CreateInterpolator
  (const ASource: IdxPDFImageScanlineSource; ATargetWidth, ATargetHeight,
  ASourceWidth, ASourceHeight: Integer; AShouldInterpolate: Boolean)
  : IdxPDFImageScanlineSource;
begin
  Result := GetFactory.CreateInterpolator(ASource, ATargetWidth, ATargetHeight,
    ASourceWidth, ASourceHeight, AShouldInterpolate);
end;

function TdxPDFImageScanlineSourceFactory.CreateIndexedScanlineSource
  (const ASource: IdxPDFImageScanlineSource;
AWidth, AHeight, ABitsPerComponent: Integer; const ALookupTable: TBytes;
ABaseColorSpaceComponentCount: Integer): IdxPDFImageScanlineSource;
begin
  Result := GetFactory.CreateIndexedScanlineSource(ASource, AWidth, AHeight,
    ABitsPerComponent, ALookupTable, ABaseColorSpaceComponentCount);
end;

function TdxPDFImageScanlineSourceFactory.GetFactory
  : IdxPDFImageScanlineSourceFactory;
begin
  if FFactory = nil then
    try
      if TdxPDFUtils.UseWIC then
        FFactory := TdxPDFWICScanlineSourceFactory.Create
      else
        FFactory := TdxPDFDefaultScanlineSourceFactory.Create;
    except
      FFactory := TdxPDFDefaultScanlineSourceFactory.Create;
    end;
  Result := FFactory;
end;

{ TdxPDFImageDataSource }

constructor TdxPDFImageDataSource.Create(const ASource
  : IdxPDFImageScanlineSource; AWidth, ASourceScanlineLength: Integer);
begin
  inherited Create;
  FSource := ASource;
  FWidth := AWidth;
  SetLength(FSourceScanline, ASourceScanlineLength);
end;

destructor TdxPDFImageDataSource.Destroy;
begin
  inherited Destroy;
end;

function TdxPDFImageDataSource.GetComponentCount: Integer;
begin
  Result := 0;
end;

function TdxPDFImageDataSource.GetHasAlpha: Boolean;
begin
  Result := False;
end;

procedure TdxPDFImageDataSource.FillNextScanline(var AScanline: TBytes);
begin
  FillBuffer(AScanline, 1);
end;

function TdxPDFImageDataSource.GetNextSourceScanline: TBytes;
begin
  FSource.FillNextScanline(FSourceScanline);
  Result := FSourceScanline;
end;

{ TdxPDFBGRImageDataSource }

constructor TdxPDFBGRImageDataSource.Create(const ASource
  : IdxPDFImageScanlineSource; AWidth, AStride, AComponentCount: Integer);
begin
  inherited Create(ASource, AWidth, AWidth * AComponentCount);
  FComponentCount := AComponentCount;
  FStride := AStride;
end;

function TdxPDFBGRImageDataSource.GetComponentCount: Integer;
begin
  Result := FComponentCount;
end;

procedure TdxPDFBGRImageDataSource.FillBuffer(var ABuffer: TBytes;
AScanlineCount: Integer);
var
  AComponentsData, ASourceScanline: TBytes;
  ALastComponentIndex, AScanline, ADestinationOffset,
    ACurrentDestinationOffset: Integer;
  ASourceOffset, I, J, AComponentOffset: Integer;
begin
  SetLength(AComponentsData, FComponentCount);
  ALastComponentIndex := FComponentCount - 1;
  ADestinationOffset := 0;
  for AScanline := 0 to AScanlineCount - 1 do
  begin
    ACurrentDestinationOffset := ADestinationOffset;
    ASourceOffset := 0;
    ASourceScanline := GetNextSourceScanline;
    for I := 0 to Width - 1 do
    begin
      for J := 0 to FComponentCount - 1 do
      begin
        AComponentsData[J] := ASourceScanline[ASourceOffset];
        Inc(ASourceOffset);
      end;
      AComponentOffset := ALastComponentIndex;
      for J := 0 to FComponentCount - 1 do
      begin
        ABuffer[ACurrentDestinationOffset] := AComponentsData[AComponentOffset];
        Inc(ACurrentDestinationOffset);
        Dec(AComponentOffset);
      end;
    end;
    Inc(ADestinationOffset, FStride);
  end;
end;

{ TdxPDFRGBImageDataSource }

constructor TdxPDFRGBImageDataSource.Create(const ASource
  : IdxPDFImageScanlineSource; AWidth, AStride, AComponentCount: Integer);
begin
  inherited Create(ASource, AWidth, AWidth * AComponentCount);
  FComponentCount := AComponentCount;
  FStride := AStride;
end;

function TdxPDFRGBImageDataSource.GetComponentCount: Integer;
begin
  Result := FComponentCount;
end;

procedure TdxPDFRGBImageDataSource.FillBuffer(var ABuffer: TBytes;
AScanlineCount: Integer);
var
  AScanlineLength, AScanlineIndex, ADestinationOffset: Integer;
begin
  ADestinationOffset := 0;
  AScanlineLength := Width * FComponentCount;
  for AScanlineIndex := 0 to AScanlineCount - 1 do
  begin
    TdxPDFUtils.CopyData(GetNextSourceScanline, 0, ABuffer, ADestinationOffset,
      AScanlineLength);
    Inc(ADestinationOffset, FStride);
  end;
end;

{ TdxPDFColorKeyMaskedImageDataSource }

constructor TdxPDFColorKeyMaskedImageDataSource.Create(const ASource
  : IdxPDFImageScanlineSource; AWidth: Integer);
begin
  inherited Create(ASource, AWidth, AWidth * (DefaultComponentCount + 1));
end;

function TdxPDFColorKeyMaskedImageDataSource.GetComponentCount: Integer;
begin
  Result := 4;
end;

function TdxPDFColorKeyMaskedImageDataSource.GetHasAlpha: Boolean;
begin
  Result := True;
end;

procedure TdxPDFColorKeyMaskedImageDataSource.FillBuffer(var ABuffer: TBytes;
AScanlineCount: Integer);
var
  AComponentsData, ASourceScanline: TBytes;
  AScanline, ADestinationOffset, ASourceOffset, I, J, AComponentOffset: Integer;
begin
  SetLength(AComponentsData, DefaultComponentCount);
  ADestinationOffset := 0;
  for AScanline := 0 to AScanlineCount - 1 do
  begin
    ASourceOffset := 0;
    ASourceScanline := GetNextSourceScanline;
    for I := 0 to Width - 1 do
    begin
      for J := 0 to DefaultComponentCount - 1 do
      begin
        AComponentsData[J] := ASourceScanline[ASourceOffset];
        Inc(ASourceOffset);
      end;
      AComponentOffset := DefaultComponentCount - 1;
      for J := 0 to DefaultComponentCount - 1 do
      begin
        ABuffer[ADestinationOffset] := AComponentsData[AComponentOffset];
        Inc(ADestinationOffset);
        Dec(AComponentOffset);
      end;
      ABuffer[ADestinationOffset] := ASourceScanline[ASourceOffset];
      Inc(ADestinationOffset);
      Inc(ASourceOffset);
    end;
  end;
end;

{ TdxPDFTransparentImageDataSource }

constructor TdxPDFTransparentImageDataSource.Create(const ASource,
  AMaskSource: IdxPDFImageScanlineSource; AWidth: Integer);
begin
  inherited Create(ASource, AWidth, AWidth * DefaultComponentCount);
  SetLength(FMaskScanline, AWidth);
  FMaskSource := AMaskSource;
end;

function TdxPDFTransparentImageDataSource.GetComponentCount: Integer;
begin
  Result := 4;
end;

function TdxPDFTransparentImageDataSource.GetHasAlpha: Boolean;
begin
  Result := True;
end;

procedure TdxPDFTransparentImageDataSource.FillBuffer(var ABuffer: TBytes;
AScanlineCount: Integer);
var
  AComponentsData, ASourceScanline: TBytes;
  I, J, AScanline, ADestinationOffset, ASourceOffset, AComponentOffset: Integer;
begin
  SetLength(AComponentsData, DefaultComponentCount);
  ADestinationOffset := 0;
  for AScanline := 0 to AScanlineCount - 1 do
  begin
    ASourceOffset := 0;
    ASourceScanline := GetNextSourceScanline;
    FMaskSource.FillNextScanline(FMaskScanline);
    for I := 0 to Width - 1 do
    begin
      for J := 0 to DefaultComponentCount - 1 do
      begin
        AComponentsData[J] := ASourceScanline[ASourceOffset];
        Inc(ASourceOffset);
      end;
      AComponentOffset := DefaultComponentCount - 1;
      for J := 0 to DefaultComponentCount - 1 do
      begin
        ABuffer[ADestinationOffset] := AComponentsData[AComponentOffset];
        Inc(ADestinationOffset);
        Dec(AComponentOffset);
      end;
      ABuffer[ADestinationOffset] := FMaskScanline[I];
      Inc(ADestinationOffset);
    end;
  end;
end;

{ TdxPDFTransparentMatteImageDataSource }

constructor TdxPDFTransparentMatteImageDataSource.Create(const ASource,
  AMaskSource: IdxPDFImageScanlineSource; AWidth: Integer;
const AMatte: TDoubleDynArray);
begin
  inherited Create(ASource, AWidth, AWidth * DefaultComponentCount);
  FMaskSource := AMaskSource;
  SetLength(FMaskScanline, AWidth);
  FMatte := AMatte;
end;

destructor TdxPDFTransparentMatteImageDataSource.Destroy;
begin
  inherited Destroy;
end;

function TdxPDFTransparentMatteImageDataSource.GetComponentCount: Integer;
begin
  Result := 4;
end;

function TdxPDFTransparentMatteImageDataSource.GetHasAlpha: Boolean;
begin
  Result := True;
end;

procedure TdxPDFTransparentMatteImageDataSource.FillBuffer(var ABuffer: TBytes;
AScanlineCount: Integer);
var
  AComponentsData, ASourceScanline: TBytes;
  AScanline, ADestinationOffset, ASourceOffset, I, J, AComponentOffset: Integer;
  ASrcColor, AMatteComponent, AMaskValue, AResult: Single;
begin
  SetLength(AComponentsData, DefaultComponentCount);
  ADestinationOffset := 0;
  for AScanline := 0 to AScanlineCount - 1 do
  begin
    ASourceOffset := 0;
    ASourceScanline := GetNextSourceScanline;
    FMaskSource.FillNextScanline(FMaskScanline);
    for I := 0 to Width - 1 do
    begin
      AComponentOffset := DefaultComponentCount - 1;
      for J := 0 to DefaultComponentCount - 1 do
      begin
        ASrcColor := ASourceScanline[ASourceOffset] / 255;
        Inc(ASourceOffset);
        AMatteComponent := FMatte[J];
        AMaskValue := FMaskScanline[I] / 255;
        if AMaskValue <> 0 then
          AResult := (ASrcColor - AMatteComponent) / AMaskValue +
            AMatteComponent
        else
          AResult := 1;
        if AResult < 0 then
          AResult := 0
        else if AResult > 1 then
          AResult := 1;
        ABuffer[ADestinationOffset + AComponentOffset] := Trunc(AResult * 255);
        Dec(AComponentOffset);
      end;
      Inc(ADestinationOffset, DefaultComponentCount);
      ABuffer[ADestinationOffset] := FMaskScanline[I];
      Inc(ADestinationOffset);
    end;
  end;
end;

{ TdxPDFImageDataCacheItem }

constructor TdxPDFImageDataCacheItem.Create(const AImageData: TdxPDFImageData;
const AImage: IdxPDFDocumentImage);
begin
  inherited Create;
  FParameters := TdxPDFImageParameters.Create(AImageData.Width,
    AImageData.Height);
  FStride := AImageData.Stride;
  FPixelFormat := AImageData.PixelFormat;
  FPalette := AImageData.Palette;
  FImage := AImage;
end;

procedure TdxPDFImageDataCacheItem.DrawImage(const APainter
  : IdxPDFImageCachePainter);
begin
  // do nothing
end;

{ TdxPDFImageDataRasterCacheItem }

constructor TdxPDFImageDataRasterCacheItem.Create(const AImageData
  : TdxPDFImageData; const AImage: IdxPDFDocumentImage; const AData: TBytes);
begin
  inherited Create(AImageData, AImage);
  FData := AData;
end;

procedure TdxPDFImageDataRasterCacheItem.DrawImage(const APainter
  : IdxPDFImageCachePainter);
begin
  APainter.DrawImage(Self, FData);
end;

{ TdxPDFImageDataSourceCacheItem }

constructor TdxPDFImageDataSourceCacheItem.Create(const AImageData
  : TdxPDFImageData; const AImage: IdxPDFDocumentImage);
begin
  inherited Create(AImageData, AImage);
  FDataSource := AImageData.Data;
end;

destructor TdxPDFImageDataSourceCacheItem.Destroy;
begin
  FDataSource := nil;
  inherited Destroy;
end;

procedure TdxPDFImageDataSourceCacheItem.DrawImage(const APainter
  : IdxPDFImageCachePainter);
begin
  APainter.DrawImage(Self, FDataSource);
end;

{ TdxPDFImageCacheItem }

class function TdxPDFImageCacheItem.Create(AData: TdxPDFImageDataCacheItem;
AShouldDispose: Boolean): TdxPDFImageCacheItem;
begin
  Result.FData := AData;
  Result.FShouldDispose := AShouldDispose;
  Result.FIsNull := False;
end;

class function TdxPDFImageCacheItem.Null: TdxPDFImageCacheItem;
begin
  Result.FIsNull := True;
end;

function TdxPDFImageCacheItem.IsNull: Boolean;
begin
  Result := FIsNull;
end;

{ TdxPDFCacheItem }

class function TdxPDFCacheItem.Create: TdxPDFCacheItem;
begin
  Result.FIsNull := True;
end;

class function TdxPDFCacheItem.Create(AValue: TdxPDFImageDataCacheItem;
const ANode: IdxPDFDocumentImage; const AParameters: TdxPDFImageParameters;
ASize: Int64): TdxPDFCacheItem;
begin
  Result.FValue := AValue;
  Result.FNode := ANode;
  Result.FParameters := AParameters;
  Result.FSize := ASize;
  Result.FIsNull := False;
end;

function TdxPDFCacheItem.IsNull: Boolean;
begin
  Result := FIsNull;
end;

{ TdxPDFImageCache }

constructor TdxPDFImageCache.Create(ACapacity: Int64);
begin
  inherited Create;
  FCapacity := ACapacity;
  CreateSubClasses;
end;

destructor TdxPDFImageCache.Destroy;
begin
  Clear;
  DestroySubClasses;
  inherited Destroy;
end;

procedure TdxPDFImageCache.SetCapacity(const AValue: Int64);
begin
  FCapacity := Max(AValue, 0);
  TrimCache(nil);
end;

function TdxPDFImageCache.GetImage(const AImage: IdxPDFDocumentImage;
const AImageParameters: TdxPDFImageParameters): TdxPDFImageCacheItem;
var
  AActualImageParameters: TdxPDFImageParameters;
  AValue, ANewValue: TdxPDFCacheItem;
begin
  AActualImageParameters := AImage.GetActualSize(AImageParameters);
  if FObjectStorage.TryGetValue(AImage, AValue) then
  begin
    if not ShouldReplaceImage(AActualImageParameters, AValue.Parameters) then
    begin
      FRecentImages.Enqueue(AValue.Node);
      Exit(TdxPDFImageCacheItem.Create(AValue.Value, False));
    end;
  end
  else
    AValue := TdxPDFCacheItem.Create;
  ANewValue := CreateValue(AImage, AActualImageParameters);
  if ANewValue.IsNull then
    Exit(TdxPDFImageCacheItem.Null);
  if (Capacity <> 0) and (ANewValue.Size > Capacity) then
    Exit(TdxPDFImageCacheItem.Create(ANewValue.Value, True));
  if not AValue.IsNull then
    RemoveItem(AImage, AValue);
  AValue := ANewValue;
  FSize := FSize + AValue.Size;
  FObjectStorage.Add(AImage, AValue);
  FRecentImages.Enqueue(AValue.Node);
  TrimCache(AImage);
  Result := TdxPDFImageCacheItem.Create(AValue.Value, False);
end;

function TdxPDFImageCache.ShouldReplaceImage(const AImageParameters,
  AOldParameters: TdxPDFImageParameters): Boolean;
begin
  Result := (AImageParameters.Width > AOldParameters.Width) or
    (AImageParameters.Height > AOldParameters.Height);
end;

procedure TdxPDFImageCache.CreateSubClasses;
begin
  FObjectStorage := TDictionary<IdxPDFDocumentImage, TdxPDFCacheItem>.Create;
  FRecentImages := TQueue<IdxPDFDocumentImage>.Create;
end;

procedure TdxPDFImageCache.DestroySubClasses;
begin
  FreeAndNil(FRecentImages);
  FreeAndNil(FObjectStorage);
end;

procedure TdxPDFImageCache.RemoveItem(const AImage: IdxPDFDocumentImage);
var
  AValue: TdxPDFCacheItem;
begin
  if not FObjectStorage.TryGetValue(AImage, AValue) then
    AValue := TdxPDFCacheItem.Create;
  RemoveItem(AImage, AValue);
end;

procedure TdxPDFImageCache.RemoveItem(const AImage: IdxPDFDocumentImage;
AValue: TdxPDFCacheItem);
begin
  FObjectStorage.Remove(AImage);
  if not AValue.IsNull and (AValue.Value <> nil) then
  begin
    FSize := FSize - AValue.Size;
    AValue.Value.Free;
  end;
end;

procedure TdxPDFImageCache.TrimCache(const ARecentImage: IdxPDFDocumentImage);
var
  AImage: IdxPDFDocumentImage;
begin
  if FCapacity <> 0 then
    while (FSize > Capacity) and (FSize > 0) do
    begin
      AImage := FRecentImages.Extract;
      if (ARecentImage = nil) or (ARecentImage <> AImage) then
        RemoveItem(AImage);
    end;
end;

procedure TdxPDFImageCache.Clear;
var
  ADataCache: TdxPDFCacheItem;
begin
  for ADataCache in FObjectStorage.Values do
    ADataCache.Value.Free;
  FObjectStorage.Clear;
  FRecentImages.Clear;
  FSize := 0;
end;

{ TdxPDFImageDataCache }

class function TdxPDFImageDataCache.GetImageRaster(const AImageData
  : TdxPDFImageData): TBytes;
var
  ADataSource: IdxPDFImageScanlineSource;
begin
  SetLength(Result, AImageData.Stride * AImageData.Height);
  ADataSource := AImageData.Data;
  ADataSource.FillBuffer(Result, AImageData.Height);
end;

function TdxPDFImageDataCache.CreateValue(const AImage: IdxPDFDocumentImage;
const AImageParameters: TdxPDFImageParameters): TdxPDFCacheItem;
var
  AImageData: TdxPDFImageData;
  ASize: Int64;
  AImageRaster: TBytes;
begin
  AImageData := AImage.GetActualData(AImageParameters, True);
  try
    ASize := AImageData.Stride * AImageData.Height;
    if ASize > Capacity then
      Result := TdxPDFCacheItem.Create(TdxPDFImageDataSourceCacheItem.Create
        (AImageData, AImage), AImage, AImageParameters, ASize)
    else
      try
        AImageRaster := GetImageRaster(AImageData);
        Result := TdxPDFCacheItem.Create
          (TdxPDFImageDataRasterCacheItem.Create(AImageData, AImage,
          AImageRaster), AImage, AImageParameters, ASize);
      except
        Result := TdxPDFCacheItem.Create
          (TdxPDFImageDataSourceCacheItem.Create(AImageData, AImage), AImage,
          AImageParameters, ASize);
      end;
  finally
    AImageData.Data := nil;
  end;
end;

initialization

finalization

FreeAndNil(dxgPDFImageScanlineSourceFactory);

end.
