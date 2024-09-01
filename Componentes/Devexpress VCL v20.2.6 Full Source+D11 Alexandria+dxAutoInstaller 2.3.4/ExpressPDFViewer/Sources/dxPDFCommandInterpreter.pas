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

unit dxPDFCommandInterpreter;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, SysUtils, Types, Graphics, Classes, Generics.Defaults,
  Generics.Collections, dxCoreClasses, cxGeometry,
  cxGraphics, dxCoreGraphics, dxGDIPlusAPI, dxGDIPlusClasses, dxPDFCore,
  dxPDFFontEncoding, dxPDFBase, dxPDFTypes,
  dxPDFFont, dxPDFFontUtils, dxPDFInteractivity, dxPDFText, dxPDFFunction,
  dxPDFRecognizedObject, dxPDFImageUtils;

type
  TdxPDFCustomCommandInterpreter = class;
  TdxPDFExportParametersClass = class of TdxPDFExportParameters;
  TdxPDFFillPathInfo = class;
  TdxPDFGraphicsPath = class;
  TdxPDFGraphicsPathBuilder = class;
  TdxPDFPolygonClipper = class;

  TdxPDFPolygonClipperEdge = (ceNone = 0, ceLeft = 1, ceRight = 2, ceBottom = 4,
    ceTop = 8);

  { TdxPDFRenderParameters }

  TdxPDFRenderParameters = class(TdxPDFExportParameters)
  public
    Canvas: TCanvas;
    Rect: TRect;
    Position: TdxPointF;
  end;

  { TdxPDFCustomCommandInterpreter }

  TdxPDFCustomCommandInterpreterClass = class of TdxPDFCustomCommandInterpreter;

  TdxPDFCustomCommandInterpreter = class(TcxIUnknownObject,
    IdxPDFCommandInterpreter, IdxPDFImageCachePainter)
  strict private
    FActualSize: TSize;
    FAngle: Integer;
    FBoundsOffset: TdxPointF;
    FCurrentFormNumber: Integer;
    FGraphicsState: TdxPDFGraphicsState;
    FTilingTransformMatrix: TdxPDFTransformationMatrix;
    FTilingTransformMatrixStack: TStack<TdxPDFTransformationMatrix>;
    FTransformedBoundingBox: TdxPDFPoints;
    FClipUseNonZeroWindingRule: TdxNullableBoolean;
    FPaths: TObjectList<TdxPDFGraphicsPath>;
    function GetActualSize: TSize;
    function GetBounds: TdxRectF;
    function GetCurrentPath: TdxPDFGraphicsPath;
    function GetDeviceTransformationMatrix: TdxPDFTransformationMatrix;
    function GetDocumentState: TdxPDFDocumentState;
    function GetRotationAngle: Single;
    function GetTextState: TdxPDFTextState; inline;
    function GetTextStateFont: TdxPDFCustomFont; inline;
    function GetTransformMatrix: TdxPDFTransformationMatrix; inline;
    function NeedDrawAnnotation(AAnnotation: TdxPDFCustomAnnotation): Boolean;
    procedure DrawAnnotation(AAnnotation: TdxPDFCustomAnnotation);
    procedure ExportAnnotation;
    procedure InitializeTransformedBoundingBox;
    procedure RecreatePaths;
  strict protected
  const
    GlyphPositionToTextSpaceFactor = 0.001;
  strict protected
    FGraphicsStateStack: TObjectStack<TdxPDFGraphicsState>;
    FGraphicsStateStackLock: Integer;
    FPageClippingRect: TdxRectF;
  protected
    FParameters: TdxPDFExportParameters;
    function CheckRenderingMode: Boolean; virtual;
    function CreateTilingBitmap(APattern: TdxPDFTilingPattern;
      const ASize, AKeySize: TSize; const AColor: TdxPDFColor)
      : TcxBitmap32; virtual;
    function CalculateAngle(const P1, P2: TdxPointF): Single;
    function CalculateDistance(const P1, P2: TdxPointF): Integer;
    function GetBoundsOffset: TdxPointF; virtual;
    function IsCanceled: Boolean;
    function IsNotRotated: Boolean;
    function IsType3Font: Boolean; inline;
    function MinGraphicsStateCount: Integer; virtual;
    function ToCanvasPoint(const P: TdxPointF): TdxPointF; inline;
    function TransformSize(const AMatrix: TdxPDFTransformationMatrix;
      const ABoundingBox: TdxPDFRectangle): TdxSizeF;
    function TransformShadingPoint(APoint: TdxPointF): TdxPointF;
    function VectorLength(const AMatrix: TdxPDFTransformationMatrix;
      X, Y: Single): Single;
    function UseRectangularGraphicsPath: Boolean;
    procedure AfterExport; virtual;
    procedure AfterExportPage(APage: TdxPDFPage); virtual;
    procedure AppendPathBezierSegment(const P1, P2, P3: TdxPointF);
      overload; virtual;
    procedure AppendPathBezierSegment(const P2, AEndPoint: TdxPointF);
      overload; virtual;
    procedure AppendPathLineSegment(const AEndPoint: TdxPointF); virtual;
    procedure AppendRectangle(X, Y, AWidth, AHeight: Double); virtual;
    procedure ApplyGraphicsStateParameters(AParameters
      : TdxPDFGraphicsStateParameters); virtual;
    procedure BeforeExport; virtual;
    procedure BeforeExportPage(AData: TdxPDFPageData;
      AParameters: TdxPDFExportParameters); virtual;
    procedure BeginPath(const AStartPoint: TdxPointF); virtual;
    procedure BeginText; virtual;
    procedure Clip(AUseNonzeroWindingRule: Boolean);
    procedure ClipAndClearPaths; virtual;
    procedure ClipPaths; virtual;
    procedure ClipRectangle(const ARect: TdxPDFRectangle);
    procedure ClosePath; virtual;
    procedure CreateSubClasses; virtual;
    procedure DestroySubClasses; virtual;
    procedure DoExport(ACommands: TdxPDFCommandList); virtual;
    procedure DrawForm(AForm: TdxPDFForm);
    procedure DrawImage(ABitsPerComponent, APixelFormat, AImageWidth,
      AImageHeight: Integer; ABitmap: GpBitmap); overload; virtual;
    procedure DrawImage(AImage: TdxPDFDocumentImage); overload; virtual;
    procedure DrawImage(AImageData: TdxPDFImageDataCacheItem;
      const AData: TBytes); overload; virtual;
    procedure DrawImage(AImageData: TdxPDFImageDataCacheItem;
      const ADataSource: IdxPDFImageScanlineSource); overload; virtual;
    procedure DrawShading(AShading: TdxPDFCustomShading); virtual;
    procedure DrawString(const AData: TBytes;
      const ADataOffsets: TDoubleDynArray); overload;
    procedure DrawString(const AData: TdxPDFStringData); overload; virtual;
    procedure DrawTransparencyGroup(AForm: TdxPDFGroupForm); virtual;
    procedure DrawType3FontString(const AStringData: TdxPDFStringData); virtual;
    procedure EndText; virtual;
    procedure ExecuteCommand(const AInterpreter: IdxPDFCommandInterpreter;
      ACommands: TdxPDFCommandList); overload;
    procedure Export(APages: TdxPDFPages;
      AParameters: TdxPDFExportParameters); overload;
    procedure FillPaths(AUseNonzeroWindingRule: Boolean); virtual;
    procedure Finalize; virtual;
    procedure Initialize; virtual;
    procedure InitializeClipBounds(const ATopLeft, ABottomRight
      : TdxPointF); virtual;
    procedure InitializeTransformMatrix; virtual;
    procedure MoveToNextLine;
    procedure RestoreGraphicsState; virtual;
    procedure SaveGraphicsState; virtual;
    procedure SetCharacterSpacing(ASpacing: Single);
    procedure SetColorForNonStrokingOperations(const AColor
      : TdxPDFColor); virtual;
    procedure SetColorForStrokingOperations(const AColor: TdxPDFColor); virtual;
    procedure SetColorSpaceForNonStrokingOperations
      (AColorSpace: TdxPDFCustomColorSpace);
    procedure SetColorSpaceForStrokingOperations(AColorSpace
      : TdxPDFCustomColorSpace);
    procedure SetFlatnessTolerance(AValue: Double);
    procedure SetLineCapStyle(ALineCapStyle: TdxPDFLineCapStyle);
    procedure SetLineJoinStyle(ALineJoinStyle: TdxPDFLineJoinStyle);
    procedure SetLineStyle(ALineStyle: TdxPDFLineStyle);
    procedure SetLineWidth(ALineWidth: Single);
    procedure SetMiterLimit(AMiterLimit: Single);
    procedure SetRenderingIntent(AValue: TdxPDFRenderingIntent);
    procedure SetTextFont(AFont: TdxPDFCustomFont; AFontSize: Double); virtual;
    procedure SetTextHorizontalScaling(AValue: Double);
    procedure SetTextLeading(ALeading: Double);
    procedure SetTextMatrix(const AMatrix: TdxPDFTransformationMatrix);
      overload; virtual;
    procedure SetTextMatrix(const AOffset: TdxPointF); overload;
    procedure SetTextRenderingMode(AMode: TdxPDFTextRenderingMode); virtual;
    procedure SetTextRise(AValue: Double);
    procedure SetTransformationMatrix(const AMatrix
      : TdxPDFTransformationMatrix); virtual;
    procedure SetWordSpacing(AWordSpacing: Double);
    procedure StrokePaths; virtual;
    procedure UnknownCommand(const AName: string);
    procedure UpdateGraphicsState(AParameters
      : TdxPDFGraphicsStateParameters); virtual;
    procedure UpdateTilingTransformationMatrix;
    procedure UpdateTransformationMatrix(const AMatrix
      : TdxPDFTransformationMatrix); virtual;
    { IFDEF CXTEST }
  public
    { ENDIF }
    procedure ExecuteCommand(ACommands: TdxPDFCommandList); overload;

    property ActualSize: TSize read GetActualSize;
    property Bounds: TdxRectF read GetBounds;
    property BoundsOffset: TdxPointF read GetBoundsOffset;
    property ClipUseNonZeroWindingRule: TdxNullableBoolean
      read FClipUseNonZeroWindingRule;
    property DeviceTransformMatrix: TdxPDFTransformationMatrix
      read GetDeviceTransformationMatrix;
    property DocumentState: TdxPDFDocumentState read GetDocumentState;
    property GraphicsState: TdxPDFGraphicsState read FGraphicsState;
    property GraphicsStateStack: TObjectStack<TdxPDFGraphicsState>
      read FGraphicsStateStack;
    property PageClippingRect: TdxRectF read FPageClippingRect;
    property Paths: TObjectList<TdxPDFGraphicsPath> read FPaths;
    property RotationAngle: Single read GetRotationAngle;
    property TextState: TdxPDFTextState read GetTextState;
    property TextStateFont: TdxPDFCustomFont read GetTextStateFont;
    property TilingTransformMatrix: TdxPDFTransformationMatrix
      read FTilingTransformMatrix;
    property TransformedBoundingBox: TdxPDFPoints read FTransformedBoundingBox;
    property TransformMatrix: TdxPDFTransformationMatrix
      read GetTransformMatrix;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure Export(APage: TdxPDFPage; AParameters: TdxPDFExportParameters);
      overload; virtual;
    procedure Export(ACommands: TdxPDFCommandList;
      AParameters: TdxPDFExportParameters); overload;
    procedure ExportAndPack(APage: TdxPDFPage;
      AParameters: TdxPDFExportParameters);
  end;

  { TdxPDFRenderingInterpreter }

  TdxPDFRenderingInterpreter = class(TdxPDFCustomCommandInterpreter)
  strict private
  const
    TextRenderingPrecision = 10000;
  strict private
    FActualFontNameCache: TdxPDFStringStringDictionary;
    FBoundingBoxClipper: TdxPDFPolygonClipper;
    FClipRegion: TdxGPRegion;
    FCorrectedImageDestinationPoints: TdxPDFPoints;
    FCurrentClipRegion: TdxGPRegion;
    FFontQuality: Word;
    FGraphics: TdxGPCanvas;
    FHDC: HDC;
    FImageDestinationPoints: TdxPDFPoints;
    FInitialClipRegion: TdxGPRegion;
    FNonStrokingBrush: TdxGPBrush;
    FNonStrokingBrushStack: TObjectStack<TdxGPBrush>;
    FPrevAlpha: Integer;
    FPrevBkMode: Cardinal;
    FPrevClipRegion: Cardinal;
    FPrevFontQuality: Word;
    FPrevGraphics: TdxGPCanvas;
    FPrevGraphicsMode: Cardinal;
    FPrevMappingMode: Word;
    FPrevTextAlign: Cardinal;
    FPrevTextColor: Cardinal;
    FPrevTextFont: Cardinal;
    FPrevViewportSize: TSize;
    FPrevWindowSize: TSize;
    FRegionStack: TObjectStack<TdxGPRegion>;
    FSemitransparentTextBitmap: TdxSmartImage;
    FSolidBrushCache: TObjectDictionary<TdxAlphaColor, TdxGPBrush>;
    FStrokingBrush: TdxGPBrush;
    FStrokingBrushStack: TObjectStack<TdxGPBrush>;
    FUseEmbeddedFontEncoding: Boolean;

    function CalculateBitmapTransformationMatrix(ABitmapWidth,
      ABitmapHeight: Integer; const ABoundingBox: TdxRectF)
      : TdxPDFTransformationMatrix;
    function CalculateLineExtendFactor(const AStartPoint,
      AEndPoint: TdxPointF): Single;
    function CalculateLineWidth(const AStartPoint,
      AEndPoint: TdxPointF): Single;
    function CalculatePenWidth(const AStartPoint, AEndPoint: TdxPointF): Single;
    function CalculateRotationAngle(const AStartPoint,
      AEndPoint: TdxPointF): Single;
    function CalculateVectorLength(X, Y: Single): Single;
    function Clone(ABrush: TdxGPBrush): TdxGPBrush; overload;
    function Clone(ARegion: TdxGPRegion): TdxGPRegion; overload;
    function ConvertPalette(AIsMask: Boolean;
      const AImagePalette: TdxPDFARGBColorArray)
      : TdxAlphaColorDynArray; overload;
    function CreateBoundingRectangle(const APoints: TdxPDFPoints)
      : TdxRectF; overload;
    function CreateBoundingRectangle(const R: TdxPDFRectangle)
      : TdxRectF; overload;
    function CreateGraphicsDeviceParameters(ACanvas: TCanvas;
      const ASize: TSize): TdxPDFRenderParameters;
    function CreateTransparencyGroupBitmap(AForm: TdxPDFGroupForm;
      const ABitmapSize: TSize;
      const ATransformationMatrix: TdxPDFTransformationMatrix;
      const AClipRect: TdxPDFRectangle): TdxSmartImage; overload;
    function CreateTransparencyGroupBitmap(AForm: TdxPDFGroupForm;
      const ABoundingBox, AClipRect: TdxPDFRectangle): TdxSmartImage; overload;
    function ExtractBackdropBitmap(X, Y, AWidth, AHeight: Integer)
      : TdxSmartImage;
    function GetActualFontName(const ASourceFontName: string): string;
    function GetFillMode(AUseNonzeroWindingRule: Boolean)
      : TdxGPFillMode; overload;
    function GetFontDataStorage: TdxPDFFontDataStorage;
    function GetImageData(AImage: TdxPDFDocumentImage; AWidth, AHeight: Integer)
      : TdxPDFImageCacheItem;
    function GetImageDataStorage: TdxPDFDocumentImageDataStorage;
    function GetImageMatrix(const ALocation: TdxPointF)
      : TdxPDFTransformationMatrix;
    function GetNonStrokingColorAlpha: Word;
    function IsTextRendering: Boolean; inline;
    function NeedExtendingLineSize: Boolean;
    function SupportCurrentBlendMode: Boolean;
    function TrimBoundingBox(const ABoundingBox: TdxPDFRectangle)
      : TdxPDFRectangle;
    function UpdateTextColor: Cardinal;
    procedure ApplySemitransparentText;
    procedure ApplySoftMask(ASoftMask: TdxPDFCustomSoftMask;
      ABitmap, ASoftMaskBitmap: TdxSmartImage); overload;
    procedure ApplySoftMask(ASoftMask: TdxPDFCustomSoftMask;
      ABitmap: TdxSmartImage; const AOffset: TdxPointF;
      AHeight: Integer); overload;
    procedure BlendWithBackground(ABitmap: TdxSmartImage; const ABounds: TRect);
    procedure CalculatePenLineStyle(const AStartPoint, AEndPoint: TdxPointF;
      APen: TdxGPPen);
    procedure DestroyBrush(var ABrush: TdxGPBrush);
    procedure DoBeginText;
    procedure DoDrawShading(AShading: TdxPDFCustomShading);
    procedure DoEndText;
    procedure DrawBitmap(ABitmap: GpBitmap; const ASize: TSize;
      const APoints: TdxPointsF; AAttributes: TdxGPImageAttributes); overload;
    procedure DrawBitmap(ABitmap: TcxBitmap32;
      const ABoundingBox: TdxPDFRectangle); overload;
    procedure DrawBitmap(ABitmap: TdxSmartImage;
      const ABoundingBox: TdxPDFRectangle); overload;
    procedure DrawString(const AGlyphs: TWordDynArray;
      const ASpacing: TIntegerDynArray; const AXForm: TXForm); overload;
    procedure DrawString(const AStringData: TWordDynArray;
      const AOffsets: TDoubleDynArray); overload;
    procedure DrawTilingCell(ABitmap: TcxBitmap32;
      APattern: TdxPDFTilingPattern; const ASize: TSize;
      const AColor: TdxPDFColor); overload;
    procedure InitializeClipRegion;
    procedure InitializeSemitransparentText;
    procedure PerformGraphicsOperation(AProc: TProc);
    procedure PerformRendering(const ABounds: TdxRectF; AProc: TProc); overload;
    procedure SetGraphics(const AValue: TdxGPCanvas);
    procedure SetSmoothingMode(AMode: TdxGPSmoothingMode);
    procedure StrokeLine(const AStartPoint, AEndPoint: TdxPointF;
      APen: TdxGPPen);
    procedure StrokePath(APath: TdxPDFGraphicsPath; APen: TdxGPPen;
      APrevSmoothingMode: TdxGPSmoothingMode);
    procedure StrokeRectangle(APath: TdxPDFGraphicsPath; APen: TdxGPPen);
    procedure UpdateBrushes;
    procedure UpdateClip;
    procedure UpdateCurrentFont;
    procedure UpdateNonStrokingBrush;
    procedure UpdatePenDashPattern(APen: TdxGPPen; ALineWidth: Double);
    procedure UpdateSmoothingMode(AIsFilling: Boolean);
    procedure UpdateStrokingBrush;
    class function ConvertPalette(AIsMask: Boolean;
      const AImagePalette: TdxPDFARGBColorArray; ACurrentColor: TdxAlphaColor)
      : TdxAlphaColorDynArray; overload;
    class procedure PerformRendering(const AData: TBytes;
      AWidth, AHeight, AStride, APixelFormat: Integer;
      const AImagePalette: TdxAlphaColorDynArray; AAction: TProc<GpBitmap>);
      overload; static;
  protected
    class function UpdateWorldTransform(AGraphics: TdxGPCanvas;
      ATransform: TdxGPMatrix): Boolean;
    function CreateBoundsClippedTransformedPaths
      : TObjectList<TdxPDFGraphicsPath>;
    function CreateGraphics: TdxGPCanvas; virtual;
    function CreateImageAttributes: TdxGPImageAttributes;
    function CreatePen: TdxGPPen;
    function CreateTilingBitmap(APattern: TdxPDFTilingPattern;
      const ASize, AKeySize: TSize; const AColor: TdxPDFColor)
      : TcxBitmap32; override;
    function CreateTransformedPaths: TObjectList<TdxPDFGraphicsPath>;
    function GetBackdropBitmap(const ABoundingBox: TdxPDFRectangle;
      ABitmapWidth, ABitmapHeight: Integer): TdxSmartImage;
    function GetBoundsOffset: TdxPointF; override;
    function GetFillMode: TdxGPFillMode; overload;
    function GetInterpolationMode(AImageWidth, AImageHeight, ABitsPerComponent
      : Integer): TdxGPInterpolationMode;
    function GetRenderParameters: TdxPDFRenderParameters;
    function IsPrinting: Boolean; virtual;
    function MinGraphicsStateCount: Integer; override;
    function ShouldCorrectInterpolationGaps(ABitsPerComponent,
      APixelFormat: Integer): Boolean;
    procedure ApplyGraphicsStateParameters(AParameters
      : TdxPDFGraphicsStateParameters); override;
    procedure BeginText; override;
    procedure ClipPaths; override;
    procedure CreateSubClasses; override;
    procedure DestroyGraphics; virtual;
    procedure DestroySubClasses; override;
    procedure DrawImage(ABitmap: GpBitmap; AWidth, AHeight: Integer;
      const APoints: TdxPointsF); overload;
    procedure DrawImage(ABitsPerComponent, APixelFormat, AImageWidth,
      AImageHeight: Integer; ABitmap: GpBitmap); override;
    procedure DrawImage(AImage: TdxPDFDocumentImage); override;
    procedure DrawImage(AImageData: TdxPDFImageDataCacheItem;
      AWidth, AHeight: Integer; ABitmap: GpBitmap;
      ATop, ABottom: Single); overload;
    procedure DrawImage(AImageData: TdxPDFImageDataCacheItem;
      const AData: TBytes); override;
    procedure DrawImage(AImageData: TdxPDFImageDataCacheItem;
      const ADataSource: IdxPDFImageScanlineSource); override;
    procedure DrawShading(AShading: TdxPDFCustomShading); override;
    procedure DrawString(const AStringData: TdxPDFStringData); override;
    procedure DrawTilingCell(APattern: TdxPDFTilingPattern;
      const AColor: TdxPDFColor; AParameters: TdxPDFExportParameters); overload;
    procedure DrawTransparencyGroup(AForm: TdxPDFGroupForm); override;
    procedure DrawType3FontString(const AStringData: TdxPDFStringData);
      override;
    procedure EndText; override;
    procedure FillPaths(AUseNonzeroWindingRule: Boolean); override;
    procedure Finalize; override;
    procedure Initialize; override;
    procedure InitializeClipBounds(const ATopLeft, ABottomRight
      : TdxPointF); override;
    procedure InitializeGraphics; virtual;
    procedure RestoreGraphicsState; override;
    procedure SaveGraphicsState; override;
    procedure SetColorForNonStrokingOperations(const AColor
      : TdxPDFColor); override;
    procedure SetColorForStrokingOperations(const AColor: TdxPDFColor);
      override;
    procedure SetTextFont(AFont: TdxPDFCustomFont; AFontSize: Double); override;
    procedure SetTextMatrix(const AMatrix: TdxPDFTransformationMatrix);
      overload; override;
    procedure SetTextRenderingMode(AMode: TdxPDFTextRenderingMode); override;
    procedure StrokePaths; override;

    property BoundingBoxClipper: TdxPDFPolygonClipper read FBoundingBoxClipper;
    property FontDataStorage: TdxPDFFontDataStorage read GetFontDataStorage;
    property FontQuality: Word read FFontQuality write FFontQuality;
    property Graphics: TdxGPCanvas read FGraphics write SetGraphics;
    property ImageDataStorage: TdxPDFDocumentImageDataStorage
      read GetImageDataStorage;
    property SolidBrushCache: TObjectDictionary<TdxAlphaColor, TdxGPBrush>
      read FSolidBrushCache;
  public
    class procedure CreateBitmap(const AImageData: TdxPDFImageData;
      AIsMask: Boolean; AAction: TProc<GpBitmap>); static;
  end;

  { TdxPDFGraphicsPathSegment }

  TdxPDFGraphicsPathSegment = class
  strict private
    FEndPoint: TdxPointF;
  protected
    function GetFlat: Boolean; virtual; abstract;
    procedure Transform(const AMatrix: TdxPDFTransformationMatrix);
      overload; virtual;

    property Flat: Boolean read GetFlat;
  public
    constructor Create(const AEndPoint: TdxPointF);
    class function Transform(ASegment: TdxPDFGraphicsPathSegment;
      const AMatrix: TdxPDFTransformationMatrix): TdxPDFGraphicsPathSegment;
      overload; static;
    function Equal(AValue: TdxPDFGraphicsPathSegment): Boolean; virtual;
    property EndPoint: TdxPointF read FEndPoint;
  end;

  { TdxPDFLineGraphicsPathSegment }

  TdxPDFLineGraphicsPathSegment = class(TdxPDFGraphicsPathSegment)
  protected
    function GetFlat: Boolean; override;
  end;

  { TdxPDFBezierGraphicsPathSegment }

  TdxPDFBezierGraphicsPathSegment = class(TdxPDFGraphicsPathSegment)
  strict private
    FControlPoint1: TdxPointF;
    FControlPoint2: TdxPointF;
  protected
    function GetFlat: Boolean; override;
    procedure Transform(const AMatrix: TdxPDFTransformationMatrix); override;
  public
    constructor Create(const AControlPoint1, AControlPoint2,
      AEndPoint: TdxPointF);
    function Equal(AValue: TdxPDFGraphicsPathSegment): Boolean; override;

    property ControlPoint1: TdxPointF read FControlPoint1;
    property ControlPoint2: TdxPointF read FControlPoint2;
  end;

  { TdxPDFGraphicsPath }

  TdxPDFGraphicsPath = class
  strict private
    FIsClosed: Boolean;
    FSegments: TObjectList<TdxPDFGraphicsPathSegment>;
    FStartPoint: TdxPointF;

    function GetEndPoint: TdxPointF;
  public
    constructor Create(const AStartPoint: TdxPointF);
    destructor Destroy; override;
    class function Transform(APaths: TList<TdxPDFGraphicsPath>;
      const AMatrix: TdxPDFTransformationMatrix)
      : TObjectList<TdxPDFGraphicsPath>; overload; static;
    function Equal(AValue: TdxPDFGraphicsPath): Boolean; virtual;
    function IsFlat(AIsFilling: Boolean): Boolean; virtual;
    procedure AppendLineSegment(const AEndPoint: TdxPointF);
    procedure AppendBezierSegment(const AControlPoint1, AControlPoint2,
      AEndPoint: TdxPointF);

    property EndPoint: TdxPointF read GetEndPoint;
    property IsClosed: Boolean read FIsClosed write FIsClosed;
    property Segments: TObjectList<TdxPDFGraphicsPathSegment> read FSegments;
    property StartPoint: TdxPointF read FStartPoint write FStartPoint;
  end;

  { TdxPDFRectangularGraphicsPath }

  TdxPDFRectangularGraphicsPath = class(TdxPDFGraphicsPath)
  strict private
    FRectangle: TdxPDFRectangle;
  public
    constructor Create(ALeft, ABottom, AWidth, AHeight: Double);

    function Equal(AValue: TdxPDFGraphicsPath): Boolean; override;
    function IsFlat(AIsFilling: Boolean): Boolean; override;

    property Rectangle: TdxPDFRectangle read FRectangle;
  end;

  { TdxPDFGraphicsPathBuilder }

  TdxPDFGraphicsPathBuilder = class
  strict private
    FInterpreter: TdxPDFRenderingInterpreter;
    FUseRectangularGraphicsPath: Boolean;
    function CreateRegionFromPath(AFillPath: TdxPDFFillPathInfo;
      AFillMode: TdxGPFillMode): TdxGPRegion;
    function InternalCreatePath(APath: TdxPDFGraphicsPath; AExtendSize: Double)
      : TdxGPPath;
    procedure AppendBezier(AGraphicsPath: TdxGPPath;
      const AStartPoint, AEndPoint: TdxPointF;
      ASegment: TdxPDFBezierGraphicsPathSegment);
    procedure AppendLine(AStrokePath: TdxGPPath;
      const AStartPoint, AEndPoint: TdxPointF);
    procedure AppendExtendLine(AGraphicsPath: TdxGPPath;
      const AStartPoint, AEndPoint: TdxPointF; ASize: Double;
      AFromLeft: Boolean);
    procedure AppendPath(AGPPath: TdxGPPath; APath: TdxPDFGraphicsPath;
      AStrokePath: TdxGPPath);
    procedure AppendSegment(AGPPath: TdxGPPath; APath: TdxPDFGraphicsPath;
      AStrokePath: TdxGPPath);
    procedure AppendSegments(AGPPath: TdxGPPath; APath: TdxPDFGraphicsPath;
      AStrokePath: TdxGPPath);
  public
    constructor Create(AInterpreter: TdxPDFRenderingInterpreter);
    class function CreateFillPath(AInterpreter: TdxPDFRenderingInterpreter;
      APaths: TList<TdxPDFGraphicsPath>): TdxPDFFillPathInfo; overload; static;
    class function CreatePath(AInterpreter: TdxPDFRenderingInterpreter;
      APath: TdxPDFGraphicsPath; AExtendSize: Double): TdxGPPath; static;
    class function CreateRegion(AInterpreter: TdxPDFRenderingInterpreter)
      : TdxGPRegion; static;
    function CreateFillPath(APaths: TList<TdxPDFGraphicsPath>)
      : TdxPDFFillPathInfo; overload;
    function CreateRectangle(APath: TdxPDFGraphicsPath): TdxRectF;
  end;

  { TdxPDFGraphicsDevice }

  TdxPDFGraphicsDevice = class(TdxPDFRenderingInterpreter)
  strict private
    procedure ExportBackground;
  protected
    procedure ExportContent(ACommands: TdxPDFCommandList); virtual;
    procedure DoExport(ACommands: TdxPDFCommandList); override;
  end;

  { TdxPDFDocumentContentRecognizer }

  TdxPDFDocumentContentRecognizer = class(TdxPDFCustomCommandInterpreter)
  strict private
    FContent: TdxPDFRecognizedContent;
    FTextParser: TdxPDFTextParser;
    FRecognitionObjects: TdxPDFRecognitionObjects;
    function AllowRecognizeAnnotations: Boolean;
    function AllowRecognizeImage: Boolean;
    function AllowRecognizeText: Boolean;
  protected
    function CheckRenderingMode: Boolean; override;
    procedure AfterExportPage(APage: TdxPDFPage); override;
    procedure AppendPathBezierSegment(const P1, P2, P3: TdxPointF); override;
    procedure AppendPathBezierSegment(const P2, AEndPoint: TdxPointF); override;
    procedure AppendPathLineSegment(const AEndPoint: TdxPointF); override;
    procedure AppendRectangle(X, Y, AWidth, AHeight: Double); override;
    procedure BeginPath(const AStartPoint: TdxPointF); override;
    procedure ClipAndClearPaths; override;
    procedure ClipPaths; override;
    procedure ClosePath; override;
    procedure DoExport(ACommands: TdxPDFCommandList); override;
    procedure DrawImage(AImage: TdxPDFDocumentImage); override;
    procedure DrawString(const AData: TdxPDFStringData); override;
    procedure InitializeTransformMatrix; override;
    procedure StrokePaths; override;
  public
    constructor Create(AContent: TdxPDFRecognizedContent;
      ARecognitionObjects: TdxPDFRecognitionObjects); reintroduce;
  end;

  { TdxPDFAttachmentUnpacker }

  TdxPDFAttachmentUnpacker = class(TdxPDFDocumentContentRecognizer)
  protected
    procedure BeforeExportPage(AData: TdxPDFPageData;
      AParameters: TdxPDFExportParameters); override;
    procedure DoExport(ACommands: TdxPDFCommandList); override;
  end;

  { TdxPDFPolygon }

  TdxPDFPolygon = class
  strict private
    FPoints: TList<TdxPointF>;
    FLastPoint: TdxPointF;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddPoint(X, Y: Single);

    property Points: TList<TdxPointF> read FPoints;
  end;

  { TdxPDFPolygonClipper }

  TdxPDFPolygonClipper = class
  strict private
    FBounds: TdxPDFRectangle;
    FEdges: TArray<TdxPDFPolygonClipperEdge>;
    FPointEdges: TDictionary<TdxPointF, TdxPDFPolygonClipperEdge>;
    FPolygon: TdxPDFPolygon;
    function ClipEdge(AEdge: TdxPDFPolygonClipperEdge;
      AIsPreviousPointInside: Boolean;
      const APreviousPoint, ACurrentPoint: TdxPointF): Boolean;
    procedure AddIntersection(AEdge: TdxPDFPolygonClipperEdge;
      const APoint1, APoint2: TdxPointF);
  public
    constructor Create(const ABounds: TdxPDFRectangle);
    destructor Destroy; override;

    class function Clip(const R: TdxPDFRectangle; APath: TdxPDFGraphicsPath)
      : TdxPDFGraphicsPath; overload; static;
    function Clip(APath: TdxPDFGraphicsPath): TdxPDFGraphicsPath; overload;
    function IsInside(const APoint: TdxPointF;
      AEdge: TdxPDFPolygonClipperEdge): Boolean;

    property Bounds: TdxPDFRectangle read FBounds;
  end;

  { TdxPDFFillPathInfo }

  TdxPDFFillPathInfo = class
  strict private
    FGraphicsPath: TdxGPPath;
    FStrokePath: TdxGPPath;
  public
    constructor Create(AGraphicsPath, AStrokePath: TdxGPPath);
    destructor Destroy; override;

    property GraphicsPath: TdxGPPath read FGraphicsPath;
    property StrokePath: TdxGPPath read FStrokePath;
  end;

implementation

uses
  Math, Forms, dxCore, dxTypeHelpers, dxPDFColorSpace, dxPDFShading, dxFontFile,
  dxPDFUtils;

const
  dxPDFMinDisplayedLineWidth = 0.1;

type
  TdxGpImageAttributesAccess = class(TdxGPImageAttributes);
  TdxPDFCustomAnnotationAccess = class(TdxPDFCustomAnnotation);
  TdxPDFCustomSoftMaskAccess = class(TdxPDFCustomSoftMask);
  TdxPDFHyperlinkAccess = class(TdxPDFHyperlink);
  TdxPDFImageAccess = class(TdxPDFImage);
  TdxPDFPageAccess = class(TdxPDFPage);
  TdxPDFPageDataAccess = class(TdxPDFPageData);
  TdxPDFRecognizedContentAccess = class(TdxPDFRecognizedContent);
  TdxPDFWidgetAnnotationAccess = class(TdxPDFWidgetAnnotation);

  { TdxPDFBrushHelper }

  TdxPDFBrushHelper = class
  strict private
    class function CreateShadingBrush(AInterpreter: TdxPDFRenderingInterpreter;
      const AColor: TdxPDFColor; AAlpha: Double): TdxGPBrush;
    class function CreateSolidBrush(AInterpreter: TdxPDFRenderingInterpreter;
      const AColor: TdxPDFColor; AAlpha: Double): TdxGPBrush;
    class function CreateTextureBrush(ATexture: TcxBitmap32; AAlpha: Double)
      : TdxGPBrush;
    class function CreateTillingBrush(AInterpreter: TdxPDFRenderingInterpreter;
      const AColor: TdxPDFColor; AAlpha: Double): TdxGPBrush;
  public
    class function CreateBrush(AInterpreter: TdxPDFRenderingInterpreter;
      const AColor: TdxPDFColor; AAlpha: Double): TdxGPBrush;
  end;

  { TdxPDFImageBlender }

  TdxPDFImageBlender = class
  strict private
  const
    ComponentCount = 4;
  strict private
    FBitmapHeight: Integer;
    FBitmapWidth: Integer;
    FSourceBitmap: TdxSmartImage;
    FSourceBitmapData: TBitmapData;
    FSourceBitmapDataReleased: Boolean;
    FSourceBitmapStride: Integer;
    FSourceData: TBytes;
    FSourceDataLength: Integer;
    FTargetBitmap: TdxSmartImage;
    FTargetBitmapData: TBitmapData;
    FTargetBitmapDataReleased: Boolean;
    FTargetBitmapStride: Integer;
    FTargetData: TBytes;
    FTargetDataLength: Integer;
    procedure ReleaseBitmaps;
  strict protected
    FTransferFunc: TdxPDFCustomFunction;
  protected
    procedure BlendPixel(ATargetOffset, ASourceOffset: Integer;
      var ATargetData, ASourceData: TBytes); virtual; abstract;
    procedure Blend;
  public
    constructor Create(ATransferFunc: TdxPDFCustomFunction;
      ATarget, ASource: TdxSmartImage);
    destructor Destroy; override;
  end;

  { TdxPDFBackdropImageBlender }

  TdxPDFBackdropImageBlender = class(TdxPDFImageBlender)
  public
    class function Supports(AMode: TdxPDFBlendMode): Boolean; static;
    class procedure Blend(AInterpreter: TdxPDFRenderingInterpreter;
      const ABoundingBox: TdxPDFRectangle; ABitmap: TdxSmartImage);
      overload; static;
    class procedure Blend(AMode: TdxPDFBlendMode;
      ABitmap, ABackdropBitmap: TdxSmartImage); overload; static;
  end;

  { TdxPDFMultiplyImageBlender }

  TdxPDFMultiplyImageBlender = class(TdxPDFBackdropImageBlender)
  protected
    procedure BlendPixel(ATargetOffset, ASourceOffset: Integer;
      var ATargetData, ASourceData: TBytes); override;
  end;

  { TdxPDFAlphaMaskBlender }

  TdxPDFAlphaMaskBlender = class(TdxPDFImageBlender)
  protected
    procedure BlendPixel(ATargetOffset, ASourceOffset: Integer;
      var ATargetData, ASourceData: TBytes); override;
  public
    class procedure Blend(ATransferFunc: TdxPDFCustomFunction;
      ATarget, ASource: TdxSmartImage); overload; static;
  end;

  { TdxPDFLuminosityMaskBlender }

  TdxPDFLuminosityMaskBlender = class(TdxPDFImageBlender)
  protected
    procedure BlendPixel(ATargetOffset, ASourceOffset: Integer;
      var ATargetData, ASourceData: TBytes); override;
  public
    class procedure Blend(ATransferFunc: TdxPDFCustomFunction;
      ATarget, ASource: TdxSmartImage); overload; static;
  end;

  { TdxPDFBackdropMatrixCalculator }

  TdxPDFBackdropMatrixCalculator = class
  strict private
  const
    ColumnCount = 7;
    RowCount = 6;
  strict private
    FRows: array of TDoubleDynArray;
    function Solve: TdxGPMatrix;
    procedure Swap(ARowIndex: Integer);
  public
    constructor Create(X, Y: Integer; const APoints: TdxPDFPoints;
      const ADestinationCoordinates: TIntegerDynArray);
    class function CalculateTransformationMatrix(X, Y: Integer;
      const APoints: TdxPDFPoints;
      const ADestinationCoordinates: TIntegerDynArray): TdxGPMatrix;
  end;

var
  dxgPDFSystemFontList: TStringList;

function dxPDFSystemFontList: TStringList;
var
  I: Integer;
  AFontName: string;
begin
  if dxgPDFSystemFontList = nil then
  begin
    dxgPDFSystemFontList := TStringList.Create;
    for I := 0 to Screen.Fonts.Count - 1 do
    begin
      AFontName := StringReplace(Screen.Fonts.Strings[I], ' ', '',
        [rfReplaceAll]);
      dxgPDFSystemFontList.Values[AFontName] := Screen.Fonts.Strings[I];
    end;
  end;
  Result := dxgPDFSystemFontList;
end;

{ TdxPDFBrushHelper }

class function TdxPDFBrushHelper.CreateTillingBrush(AInterpreter
  : TdxPDFRenderingInterpreter; const AColor: TdxPDFColor; AAlpha: Double)
  : TdxGPBrush;
var
  ABitmap: TcxBitmap32;
  APattern: TdxPDFTilingPattern;
  ABrushTransform, APatternTransform: TdxPDFTransformationMatrix;
  ABitmapSize, AKeyCellSize: TSize;
  M: TdxGPMatrix;
begin
  Result := nil;
  APattern := AColor.Pattern as TdxPDFTilingPattern;
  AKeyCellSize := cxSize(AInterpreter.TransformSize(APattern.Matrix,
    APattern.BoundingBox));
  ABitmapSize := cxSize(AInterpreter.TransformSize(APattern.Matrix,
    TdxPDFRectangle.Create(0, 0, Abs(APattern.XStep), Abs(APattern.YStep))));
  if (AInterpreter.RotationAngle = 90) or (AInterpreter.RotationAngle = 270)
  then
    ABitmapSize := cxSize(Min(ABitmapSize.cx, Max(AKeyCellSize.cx,
      AInterpreter.ActualSize.cy)), Min(ABitmapSize.cy, Max(AKeyCellSize.cy,
      AInterpreter.ActualSize.cx)))
  else
    ABitmapSize := cxSize(Min(ABitmapSize.cx, Max(AKeyCellSize.cx,
      AInterpreter.ActualSize.cx)), Min(ABitmapSize.cy, Max(AKeyCellSize.cy,
      AInterpreter.ActualSize.cy)));
  ABitmap := AInterpreter.CreateTilingBitmap(APattern, ABitmapSize,
    AKeyCellSize, AColor);
  if ABitmap <> nil then
  begin
    ABitmap.Canvas.Lock;
    try
      APatternTransform := TdxPDFTransformationMatrix.Create;
      APatternTransform.Assign(APattern.BoundingBox.Width / AKeyCellSize.cx, 0,
        0, -Abs(APattern.BoundingBox.Height) / AKeyCellSize.cy,
        APattern.BoundingBox.Left, APattern.BoundingBox.Top);
      ABrushTransform := TdxPDFTransformationMatrix.Multiply(APatternTransform,
        APattern.Matrix);
      ABrushTransform.Multiply(AInterpreter.TilingTransformMatrix, moAppend);
      ABrushTransform.Multiply(AInterpreter.DeviceTransformMatrix, moAppend);
      Result := CreateTextureBrush(ABitmap, AAlpha);
      M := TdxPDFUtils.ConvertToGpMatrix(ABrushTransform);
      try
        Result.TextureTransform.Assign(M);
      finally
        M.Free;
      end;
    finally
      ABitmap.Canvas.Unlock;
      ABitmap.Free;
    end;
  end;
end;

class function TdxPDFBrushHelper.CreateBrush(AInterpreter
  : TdxPDFRenderingInterpreter; const AColor: TdxPDFColor; AAlpha: Double)
  : TdxGPBrush;
begin
  Result := nil;
  if not AColor.IsNull and (AColor.Pattern <> nil) then
  begin
    if AColor.Pattern is TdxPDFShadingPattern then
      Result := CreateShadingBrush(AInterpreter, AColor, AAlpha)
    else if AColor.Pattern is TdxPDFTilingPattern then
      Result := CreateTillingBrush(AInterpreter, AColor, AAlpha);
  end
  else
    Result := CreateSolidBrush(AInterpreter, AColor, AAlpha);
end;

class function TdxPDFBrushHelper.CreateShadingBrush(AInterpreter
  : TdxPDFRenderingInterpreter; const AColor: TdxPDFColor; AAlpha: Double)
  : TdxGPBrush;
var
  ABitmap: TcxBitmap32;
begin
  Result := nil;
  ABitmap := TdxPDFShadingPainter.CreateBitmap(AInterpreter,
    AColor.Pattern as TdxPDFShadingPattern);
  if ABitmap <> nil then
  begin
    ABitmap.Canvas.Lock;
    try
      Result := CreateTextureBrush(ABitmap, AAlpha);
      Result.TextureTransform.Translate(AInterpreter.Bounds.BottomRight);
    finally
      ABitmap.Canvas.Unlock;
      ABitmap.Free;
    end;
  end;
end;

class function TdxPDFBrushHelper.CreateSolidBrush(AInterpreter
  : TdxPDFRenderingInterpreter; const AColor: TdxPDFColor; AAlpha: Double)
  : TdxGPBrush;
var
  AAlphaColor: TdxAlphaColor;
begin
  AAlphaColor := TdxPDFUtils.ConvertToAlphaColor(AColor, AAlpha);
  if not AInterpreter.SolidBrushCache.TryGetValue(AAlphaColor, Result) then
  begin
    Result := TdxGPBrush.Create;
    Result.Style := gpbsSolid;
    Result.Color := AAlphaColor;
    AInterpreter.SolidBrushCache.Add(AAlphaColor, Result);
  end;
end;

class function TdxPDFBrushHelper.CreateTextureBrush(ATexture: TcxBitmap32;
  AAlpha: Double): TdxGPBrush;
begin
  Result := TdxGPBrush.Create;
  Result.Style := gpbsTexture;
  Result.Texture.Assign(ATexture);
end;

{ TdxPDFImageBlender }

constructor TdxPDFImageBlender.Create(ATransferFunc: TdxPDFCustomFunction;
  ATarget, ASource: TdxSmartImage);
var
  ABitmapRectangle: TRect;
begin
  inherited Create;
  FTransferFunc := ATransferFunc;
  FTargetBitmap := ATarget;
  FSourceBitmap := ASource;
  FBitmapWidth := FTargetBitmap.Width;
  FBitmapHeight := FTargetBitmap.Height;

  ABitmapRectangle := cxRect(0, 0, FBitmapWidth, FBitmapHeight);

  GdipCheck(GdipBitmapLockBits(FTargetBitmap.Handle, @ABitmapRectangle, 3,
    TdxPDFUtils.ConvertToGpPixelFormat(TdxPDFPixelFormat.pfArgb32bpp),
    @FTargetBitmapData));
  FTargetBitmapStride := FTargetBitmapData.Stride;
  FTargetDataLength := FTargetBitmapStride * FBitmapHeight;
  SetLength(FTargetData, FTargetDataLength);
  cxCopyData(FTargetBitmapData.Scan0, @FTargetData[0], FTargetDataLength);

  GdipCheck(GdipBitmapLockBits(FSourceBitmap.Handle, @ABitmapRectangle, 1,
    TdxPDFUtils.ConvertToGpPixelFormat(TdxPDFPixelFormat.pfArgb32bpp),
    @FSourceBitmapData));
  FSourceBitmapStride := FSourceBitmapData.Stride;
  FSourceDataLength := FSourceBitmapStride * FBitmapHeight;
  SetLength(FSourceData, FSourceDataLength);
  cxCopyData(FSourceBitmapData.Scan0, @FSourceData[0], FSourceDataLength);
end;

destructor TdxPDFImageBlender.Destroy;
begin
  ReleaseBitmaps;
  inherited Destroy;
end;

procedure TdxPDFImageBlender.Blend;
var
  ATargetColorData, ASourceColorData: TBytes;
  Y, AInitialTargetOffset, AInitialSourceOffset, X, ATargetOffset,
    ASourceOffset: Integer;
begin
  SetLength(ATargetColorData, ComponentCount);
  SetLength(ASourceColorData, ComponentCount);
  AInitialTargetOffset := 0;
  AInitialSourceOffset := 0;
  for Y := 0 to FBitmapHeight - 1 do
  begin
    ATargetOffset := AInitialTargetOffset;
    ASourceOffset := AInitialSourceOffset;
    for X := 0 to FBitmapWidth - 1 do
    begin
      BlendPixel(ATargetOffset, ASourceOffset, FTargetData, FSourceData);
      Inc(ATargetOffset, ComponentCount);
      Inc(ASourceOffset, ComponentCount);
    end;
    Inc(AInitialTargetOffset, FTargetBitmapStride);
    Inc(AInitialSourceOffset, FSourceBitmapStride);
  end;
  cxCopyData(@FTargetData[0], FTargetBitmapData.Scan0, FTargetDataLength);
  ReleaseBitmaps;
end;

procedure TdxPDFImageBlender.ReleaseBitmaps;
begin
  if not FSourceBitmapDataReleased then
  begin
    GdipCheck(GdipBitmapUnlockBits(FSourceBitmap.Handle, @FSourceBitmapData));
    FSourceBitmapDataReleased := True;
  end;
  if not FTargetBitmapDataReleased then
  begin
    GdipCheck(GdipBitmapUnlockBits(FTargetBitmap.Handle, @FTargetBitmapData));
    FTargetBitmapDataReleased := True;
  end;
end;

{ TdxPDFBackdropImageBlender }

class function TdxPDFBackdropImageBlender.Supports
  (AMode: TdxPDFBlendMode): Boolean;
begin
  Result := AMode = bmMultiply;
end;

class procedure TdxPDFBackdropImageBlender.Blend(AInterpreter
  : TdxPDFRenderingInterpreter; const ABoundingBox: TdxPDFRectangle;
  ABitmap: TdxSmartImage);
var
  ABackdropBitmap: TdxSmartImage;
begin
  if Supports(AInterpreter.GraphicsState.Parameters.BlendMode) then
  begin
    ABackdropBitmap := AInterpreter.GetBackdropBitmap(ABoundingBox,
      ABitmap.Width, ABitmap.Height);
    try
      Blend(AInterpreter.GraphicsState.Parameters.BlendMode, ABitmap,
        ABackdropBitmap);
    finally
      ABackdropBitmap.Free;
    end;
  end;
end;

class procedure TdxPDFBackdropImageBlender.Blend(AMode: TdxPDFBlendMode;
  ABitmap, ABackdropBitmap: TdxSmartImage);
var
  ABlender: TdxPDFMultiplyImageBlender;
begin
  if AMode = bmMultiply then
  begin
    ABlender := TdxPDFMultiplyImageBlender.Create(nil, ABitmap,
      ABackdropBitmap);
    try
      ABlender.Blend;
    finally
      ABlender.Free;
    end;
  end;
end;

{ TdxPDFMultiplyImageBlender }

procedure TdxPDFMultiplyImageBlender.BlendPixel(ATargetOffset,
  ASourceOffset: Integer; var ATargetData, ASourceData: TBytes);
var
  I: Integer;
begin
  if ASourceData[ASourceOffset + 3] <> 0 then
    for I := 0 to 3 do
    begin
      ATargetData[ATargetOffset] := ASourceData[ASourceOffset] * ATargetData
        [ATargetOffset] div 255;
      Inc(ATargetOffset);
      Inc(ASourceOffset);
    end;
end;

{ TdxPDFAlphaMaskBlender }

class procedure TdxPDFAlphaMaskBlender.Blend(ATransferFunc
  : TdxPDFCustomFunction; ATarget, ASource: TdxSmartImage);
var
  ABlender: TdxPDFAlphaMaskBlender;
begin
  ABlender := TdxPDFAlphaMaskBlender.Create(ATransferFunc, ATarget, ASource);
  try
    ABlender.Blend;
  finally
    ABlender.Free;
  end;
end;

procedure TdxPDFAlphaMaskBlender.BlendPixel(ATargetOffset,
  ASourceOffset: Integer; var ATargetData, ASourceData: TBytes);
var
  AActualTargetOffset: Integer;
begin
  AActualTargetOffset := ATargetOffset + 3;
  ATargetData[AActualTargetOffset] :=
    Byte(ATargetData[AActualTargetOffset] * ASourceData
    [ASourceOffset + 3] div 255);
end;

{ TdxPDFLuminosityMaskBlender }

class procedure TdxPDFLuminosityMaskBlender.Blend(ATransferFunc
  : TdxPDFCustomFunction; ATarget, ASource: TdxSmartImage);
var
  ABlender: TdxPDFLuminosityMaskBlender;
begin
  ABlender := TdxPDFLuminosityMaskBlender.Create(ATransferFunc,
    ATarget, ASource);
  try
    ABlender.Blend;
  finally
    ABlender.Free;
  end;
end;

procedure TdxPDFLuminosityMaskBlender.BlendPixel(ATargetOffset,
  ASourceOffset: Integer; var ATargetData, ASourceData: TBytes);
var
  AActualTargetOffset: Integer;
  AComponents: TDoubleDynArray;
begin
  AActualTargetOffset := ATargetOffset + 3;
  if FTransferFunc <> nil then
  begin
    SetLength(AComponents, 1);
    AComponents[0] := ASourceData[ASourceOffset] / 255;
    AComponents := FTransferFunc.CreateTransformedComponents(AComponents);
    ATargetData[AActualTargetOffset] := TdxPDFUtils.ConvertToByte
      (ATargetData[AActualTargetOffset] * AComponents[0]);
    SetLength(AComponents, 0);
  end
  else
    ATargetData[AActualTargetOffset] :=
      Byte(ATargetData[AActualTargetOffset] * ASourceData
      [ASourceOffset] div 255);
end;

{ TdxPDFBackdropMatrixCalculator }

constructor TdxPDFBackdropMatrixCalculator.Create(X, Y: Integer;
  const APoints: TdxPDFPoints; const ADestinationCoordinates: TIntegerDynArray);
var
  APoint: TdxPointF;
  ARowOffset, I: Integer;
  ATransformedX, ATransformedY: Double;
begin
  inherited Create;
  ARowOffset := 0;
  SetLength(FRows, RowCount);
  for I := 0 to 2 do
  begin
    APoint := APoints[I];
    ATransformedX := APoint.X - X;
    ATransformedY := APoint.Y - Y;

    SetLength(FRows[ARowOffset], 7);
    FRows[ARowOffset][0] := ATransformedX;
    FRows[ARowOffset][1] := 0;
    FRows[ARowOffset][2] := ATransformedY;
    FRows[ARowOffset][3] := 0;
    FRows[ARowOffset][4] := 1;
    FRows[ARowOffset][5] := 0;
    FRows[ARowOffset][6] := ADestinationCoordinates[ARowOffset];
    Inc(ARowOffset);

    SetLength(FRows[ARowOffset], 7);
    FRows[ARowOffset][0] := 0;
    FRows[ARowOffset][1] := ATransformedX;
    FRows[ARowOffset][2] := 0;
    FRows[ARowOffset][3] := ATransformedY;
    FRows[ARowOffset][4] := 0;
    FRows[ARowOffset][5] := 1;
    FRows[ARowOffset][6] := ADestinationCoordinates[ARowOffset];
    Inc(ARowOffset);
  end;
end;

class function TdxPDFBackdropMatrixCalculator.CalculateTransformationMatrix(X,
  Y: Integer; const APoints: TdxPDFPoints;
  const ADestinationCoordinates: TIntegerDynArray): TdxGPMatrix;
var
  ACalculator: TdxPDFBackdropMatrixCalculator;
begin
  ACalculator := TdxPDFBackdropMatrixCalculator.Create(X, Y, APoints,
    ADestinationCoordinates);
  try
    Result := ACalculator.Solve;
  finally
    ACalculator.Free;
  end;
end;

function TdxPDFBackdropMatrixCalculator.Solve: TdxGPMatrix;
var
  I, J, K, ALastColumnIndex: Integer;
  ACurrentRow, ANextRow: TDoubleDynArray;
  AFactor, AValue: Double;
  AElements: TSingleDynArray;
begin
  for I := 0 to RowCount - 2 do
  begin
    Swap(I);
    ACurrentRow := FRows[I];
    for J := I + 1 to RowCount - 1 do
    begin
      ANextRow := FRows[J];
      AFactor := -ANextRow[I] / ACurrentRow[I];
      for K := I to ColumnCount - 1 do
        ANextRow[K] := ANextRow[K] + ACurrentRow[K] * AFactor;
    end;
  end;
  SetLength(AElements, RowCount);
  ALastColumnIndex := ColumnCount - 1;
  for I := RowCount - 1 downto 0 do
  begin
    ACurrentRow := FRows[I];
    AValue := ACurrentRow[ALastColumnIndex];
    for J := I + 1 to RowCount - 1 do
      AValue := AValue - ACurrentRow[J] * AElements[J];
    AElements[I] := AValue / ACurrentRow[I];
  end;
  Result := TdxGPMatrix.CreateEx(AElements[0], AElements[1], AElements[2],
    AElements[3], AElements[4], AElements[5]);
end;

procedure TdxPDFBackdropMatrixCalculator.Swap(ARowIndex: Integer);
var
  ARowWithMaxElement, I: Integer;
  AMaxValue, ACurrentValue: Double;
  ARowToSwap: TDoubleDynArray;
begin
  ARowWithMaxElement := ARowIndex;
  AMaxValue := Abs(FRows[ARowWithMaxElement][ARowIndex]);
  for I := ARowIndex + 1 to RowCount - 1 do
  begin
    ACurrentValue := Abs(FRows[I][ARowIndex]);
    if ACurrentValue > AMaxValue then
    begin
      ARowWithMaxElement := I;
      AMaxValue := ACurrentValue;
    end;
  end;
  ARowToSwap := FRows[ARowIndex];
  FRows[ARowIndex] := FRows[ARowWithMaxElement];
  FRows[ARowWithMaxElement] := ARowToSwap;
end;

{ TdxPDFCustomCommandInterpreter }

constructor TdxPDFCustomCommandInterpreter.Create;
begin
  inherited Create;
  CreateSubClasses;
end;

destructor TdxPDFCustomCommandInterpreter.Destroy;
begin
  DestroySubClasses;
  inherited Destroy;
end;

procedure TdxPDFCustomCommandInterpreter.Export(APage: TdxPDFPage;
  AParameters: TdxPDFExportParameters);
begin
  TdxPDFPageAccess(APage).Export(Self, AParameters);
end;

function TdxPDFCustomCommandInterpreter.CheckRenderingMode: Boolean;
begin
  Result := TextState.RenderingMode <> trmInvisible;
end;

function TdxPDFCustomCommandInterpreter.CreateTilingBitmap
  (APattern: TdxPDFTilingPattern; const ASize, AKeySize: TSize;
  const AColor: TdxPDFColor): TcxBitmap32;
begin
  Result := nil;
end;

function TdxPDFCustomCommandInterpreter.GetBoundsOffset: TdxPointF;
begin
  Result := dxPointF(cxNullPoint);
end;

function TdxPDFCustomCommandInterpreter.IsCanceled: Boolean;
begin
  Result := FParameters.IsCanceled;
end;

function TdxPDFCustomCommandInterpreter.MinGraphicsStateCount: Integer;
begin
  Result := 0;
end;

procedure TdxPDFCustomCommandInterpreter.AfterExport;
begin
  Finalize;
end;

procedure TdxPDFCustomCommandInterpreter.AfterExportPage(APage: TdxPDFPage);
begin
  // do nothing
end;

procedure TdxPDFCustomCommandInterpreter.AppendPathBezierSegment(const P2,
  AEndPoint: TdxPointF);
var
  APath: TdxPDFGraphicsPath;
begin
  APath := GetCurrentPath;
  if APath <> nil then
    APath.AppendBezierSegment(APath.EndPoint, P2, AEndPoint);
end;

procedure TdxPDFCustomCommandInterpreter.AppendPathBezierSegment(const P1, P2,
  P3: TdxPointF);
var
  APath: TdxPDFGraphicsPath;
begin
  APath := GetCurrentPath;
  if APath <> nil then
    APath.AppendBezierSegment(P1, P2, P3);
end;

procedure TdxPDFCustomCommandInterpreter.AppendPathLineSegment(const AEndPoint
  : TdxPointF);
var
  APath: TdxPDFGraphicsPath;
begin
  APath := GetCurrentPath;
  if APath <> nil then
    APath.AppendLineSegment(AEndPoint);
end;

procedure TdxPDFCustomCommandInterpreter.AppendRectangle(X, Y, AWidth,
  AHeight: Double);
begin
  FPaths.Add(TdxPDFRectangularGraphicsPath.Create(X, Y, AWidth, AHeight));
end;

procedure TdxPDFCustomCommandInterpreter.ApplyGraphicsStateParameters
  (AParameters: TdxPDFGraphicsStateParameters);
begin
  UpdateGraphicsState(AParameters);
end;

procedure TdxPDFCustomCommandInterpreter.BeginPath(const AStartPoint
  : TdxPointF);
begin
  FPaths.Add(TdxPDFGraphicsPath.Create(AStartPoint));
end;

procedure TdxPDFCustomCommandInterpreter.BeginText;
begin
  SetTextMatrix(TdxPDFTransformationMatrix.Create);
end;

procedure TdxPDFCustomCommandInterpreter.BeforeExport;
begin
  Initialize;
end;

procedure TdxPDFCustomCommandInterpreter.BeforeExportPage(AData: TdxPDFPageData;
  AParameters: TdxPDFExportParameters);
begin
  AData.ExtractCommands;
  AParameters.Annotations := AData.Annotations;
end;

function TdxPDFCustomCommandInterpreter.UseRectangularGraphicsPath: Boolean;
begin
  Result := (FPaths.Count <= 1) and IsNotRotated;
end;

procedure TdxPDFCustomCommandInterpreter.ClosePath;
var
  APath: TdxPDFGraphicsPath;
begin
  APath := GetCurrentPath;
  if APath <> nil then
  begin
    if (APath.StartPoint.X <> APath.EndPoint.X) or
      (APath.StartPoint.Y <> APath.EndPoint.Y) then
      APath.AppendLineSegment(APath.StartPoint);
    APath.IsClosed := True;
  end;
end;

procedure TdxPDFCustomCommandInterpreter.ClipAndClearPaths;
begin
  if FClipUseNonZeroWindingRule.HasValue then
  begin
    if FPaths.Count > 0 then
      ClipPaths;
    FClipUseNonZeroWindingRule.Reset;
  end;
  FreeAndNil(FPaths);
  FPaths := TObjectList<TdxPDFGraphicsPath>.Create;
end;

procedure TdxPDFCustomCommandInterpreter.ClipPaths;
begin
  // do nothing
end;

procedure TdxPDFCustomCommandInterpreter.ClipRectangle
  (const ARect: TdxPDFRectangle);
begin
  RecreatePaths;
  AppendRectangle(ARect.Left, ARect.Bottom, ARect.Width, ARect.Height);
  Clip(True);
  ClipAndClearPaths;
end;

procedure TdxPDFCustomCommandInterpreter.SetTransformationMatrix
  (const AMatrix: TdxPDFTransformationMatrix);
begin
  FGraphicsState.TransformMatrix.Multiply(AMatrix);
end;

procedure TdxPDFCustomCommandInterpreter.CreateSubClasses;
begin
  FGraphicsState := TdxPDFGraphicsState.Create;
  FGraphicsStateStack := TObjectStack<TdxPDFGraphicsState>.Create;
  FTilingTransformMatrix := TdxPDFTransformationMatrix.Create;
  FTilingTransformMatrixStack := TStack<TdxPDFTransformationMatrix>.Create;
  FPaths := TObjectList<TdxPDFGraphicsPath>.Create;
end;

procedure TdxPDFCustomCommandInterpreter.DestroySubClasses;
begin
  FreeAndNil(FPaths);
  FreeAndNil(FTilingTransformMatrixStack);
  FreeAndNil(FGraphicsStateStack);
  FreeAndNil(FGraphicsState);
end;

procedure TdxPDFCustomCommandInterpreter.DoExport(ACommands: TdxPDFCommandList);
begin
  ExecuteCommand(ACommands);
  ExportAnnotation;
end;

procedure TdxPDFCustomCommandInterpreter.DrawImage
  (AImageData: TdxPDFImageDataCacheItem; const AData: TBytes);
begin
  // do nothing
end;

procedure TdxPDFCustomCommandInterpreter.DrawImage
  (AImageData: TdxPDFImageDataCacheItem;
  const ADataSource: IdxPDFImageScanlineSource);
begin
  // do nothing
end;

procedure TdxPDFCustomCommandInterpreter.DrawImage(ABitsPerComponent,
  APixelFormat, AImageWidth, AImageHeight: Integer; ABitmap: GpBitmap);
begin
  // do nothing
end;

procedure TdxPDFCustomCommandInterpreter.DrawImage(AImage: TdxPDFDocumentImage);
begin
  // do nothing
end;

procedure TdxPDFCustomCommandInterpreter.DrawShading
  (AShading: TdxPDFCustomShading);
begin
  // do nothing
end;

procedure TdxPDFCustomCommandInterpreter.DrawType3FontString(const AStringData
  : TdxPDFStringData);
begin
  // do nothing
end;

procedure TdxPDFCustomCommandInterpreter.EndText;
begin
  // do nothing
end;

procedure TdxPDFCustomCommandInterpreter.Export(ACommands: TdxPDFCommandList;
  AParameters: TdxPDFExportParameters);
begin
  FParameters := AParameters;
  BeforeExport;
  try
    DoExport(ACommands);
  finally
    AfterExport;
  end;
end;

procedure TdxPDFCustomCommandInterpreter.ExportAndPack(APage: TdxPDFPage;
  AParameters: TdxPDFExportParameters);
var
  AData: TdxPDFPageData;
begin
  AParameters.Bounds := APage.Bounds;
  FAngle := APage.CalculateRotationAngle(AParameters.Angle);
  if not AParameters.IsCanceled then
  begin
    AData := TdxPDFPageAccess(APage).Data;
    if AData <> nil then
    begin
      if not AParameters.IsCanceled then
      begin
        BeforeExportPage(AData, AParameters);
        Export(AData.Commands, AParameters);
      end;
      AfterExportPage(APage);
    end;
  end;
end;

procedure TdxPDFCustomCommandInterpreter.Initialize;
var
  AScale: Double;
  ABottomRight, ATopLeft: TdxPointF;
begin
  FBoundsOffset := GetBoundsOffset;
  AScale := FParameters.ScaleFactor;
  if (FAngle = 90) or (FAngle = 270) then
  begin
    FActualSize := cxSize(Trunc(Abs(Bounds.Height) * AScale),
      Trunc(Abs(Bounds.Width) * AScale));
    ATopLeft := dxPointF(cxNullPoint);
    ABottomRight := dxPointF(Abs(Bounds.Height), -Abs(Bounds.Width));
  end
  else
  begin
    FActualSize := cxSize(Trunc(Abs(Bounds.Width) * AScale),
      Trunc(Abs(Bounds.Height) * AScale));
    FBoundsOffset.Y := FBoundsOffset.Y + Abs(Bounds.Height) * AScale;
    ATopLeft := dxPointF(0, Abs(Bounds.Height));
    ABottomRight := dxPointF(Abs(Bounds.Width), 0);
  end;
  FCurrentFormNumber := dxPDFInvalidValue;
  InitializeTransformMatrix;
  InitializeClipBounds(ATopLeft, ABottomRight);
  InitializeTransformedBoundingBox;
  UpdateTilingTransformationMatrix;
end;

procedure TdxPDFCustomCommandInterpreter.InitializeClipBounds(const ATopLeft,
  ABottomRight: TdxPointF);
var
  P1, P2: TdxPointF;
begin
  P1 := ToCanvasPoint(ATopLeft);
  P2 := ToCanvasPoint(ABottomRight);
  FPageClippingRect := dxRectF(P1.X, P1.Y, P2.X, P2.Y);
end;

procedure TdxPDFCustomCommandInterpreter.InitializeTransformMatrix;
var
  ABounds: TdxRectF;
begin
  ABounds := Bounds;
  case FAngle of
    90:
      FGraphicsState.TransformMatrix.Assign(0, -1, 1, 0, -ABounds.Bottom,
        ABounds.Left);
    180:
      FGraphicsState.TransformMatrix.Assign(-1, 0, 0, -1, ABounds.Right,
        ABounds.Top);
    270:
      FGraphicsState.TransformMatrix.Assign(0, 1, -1, 0, ABounds.Top,
        -ABounds.Right);
  else
    FGraphicsState.TransformMatrix.Assign(1, 0, 0, 1, -ABounds.Left,
      -ABounds.Bottom);
  end;
end;

procedure TdxPDFCustomCommandInterpreter.FillPaths(AUseNonzeroWindingRule
  : Boolean);
begin
  // do nothing
end;

procedure TdxPDFCustomCommandInterpreter.Finalize;
begin
  // do nothing
end;

procedure TdxPDFCustomCommandInterpreter.RestoreGraphicsState;
begin
  if FGraphicsStateStack.Count > MinGraphicsStateCount then
  begin
    FGraphicsState.Free;
    FGraphicsState := FGraphicsStateStack.Extract;
  end
end;

procedure TdxPDFCustomCommandInterpreter.SaveGraphicsState;
var
  AState: TdxPDFGraphicsState;
begin
  AState := TdxPDFGraphicsState.Create;
  AState.Assign(FGraphicsState);
  FGraphicsStateStack.Push(AState);
end;

procedure TdxPDFCustomCommandInterpreter.SetColorForNonStrokingOperations
  (const AColor: TdxPDFColor);
begin
  if GraphicsState.NonStrokingColorSpace <> nil then
    FGraphicsState.NonStrokingColor := FGraphicsState.NonStrokingColorSpace.
      Transform(AColor);
end;

procedure TdxPDFCustomCommandInterpreter.SetColorForStrokingOperations
  (const AColor: TdxPDFColor);
begin
  if FGraphicsState.StrokingColorSpace <> nil then
    FGraphicsState.StrokingColor := FGraphicsState.StrokingColorSpace.
      Transform(AColor);
end;

procedure TdxPDFCustomCommandInterpreter.SetTextFont(AFont: TdxPDFCustomFont;
  AFontSize: Double);
begin
  FGraphicsState.TextState.Font := AFont;
  FGraphicsState.TextState.FontSize := AFontSize;
end;

procedure TdxPDFCustomCommandInterpreter.SetTextMatrix(const AMatrix
  : TdxPDFTransformationMatrix);
var
  AState: TdxPDFTextState;
begin
  AState := TextState;
  AState.TextMatrix.Assign(AMatrix);
  AState.TextLineMatrix.Assign(AMatrix);
end;

procedure TdxPDFCustomCommandInterpreter.SetTextRenderingMode
  (AMode: TdxPDFTextRenderingMode);
begin
  TextState.RenderingMode := AMode;
end;

procedure TdxPDFCustomCommandInterpreter.UpdateGraphicsState
  (AParameters: TdxPDFGraphicsStateParameters);
begin
  FGraphicsState.ApplyParameters(AParameters);
end;

procedure TdxPDFCustomCommandInterpreter.UpdateTilingTransformationMatrix;
begin
  FTilingTransformMatrix := TdxPDFTransformationMatrix.Create(TransformMatrix);
end;

function TdxPDFCustomCommandInterpreter.CalculateAngle(const P1,
  P2: TdxPointF): Single;
begin
  Result := ArcTan2(P2.X - P1.X, P2.Y - P1.Y);
end;

function TdxPDFCustomCommandInterpreter.IsNotRotated: Boolean;
begin
  Result := not TransformMatrix.IsRotated;
end;

function TdxPDFCustomCommandInterpreter.IsType3Font: Boolean;
begin
  Result := FGraphicsState.TextState.Font is TdxPDFType3Font;
end;

function TdxPDFCustomCommandInterpreter.ToCanvasPoint(const P: TdxPointF)
  : TdxPointF;
begin
  Result.X := FBoundsOffset.X + FParameters.ScaleFactor * P.X;
  Result.Y := FBoundsOffset.Y - FParameters.ScaleFactor * P.Y;
end;

function TdxPDFCustomCommandInterpreter.TransformSize(const AMatrix
  : TdxPDFTransformationMatrix; const ABoundingBox: TdxPDFRectangle): TdxSizeF;

  function CalculateDistance(const APoint1, APoint2: TdxPointF): Double;
  var
    ADx, ADy: Double;
  begin
    ADx := APoint2.X - APoint1.X;
    ADy := APoint2.Y - APoint1.Y;
    Result := ADx * ADx + ADy * ADy;
  end;

var
  ATopLeft, ATopRight, ABottomRight, ABottomLeft: TdxPointF;
begin
  if (ABoundingBox.Width <> 0) and (ABoundingBox.Height <> 0) then
  begin
    ATopLeft := ToCanvasPoint(AMatrix.Transform(ABoundingBox.TopLeft));
    ATopRight := ToCanvasPoint(AMatrix.Transform(ABoundingBox.TopRight));
    ABottomRight := ToCanvasPoint(AMatrix.Transform(ABoundingBox.BottomRight));
    ABottomLeft := ToCanvasPoint(AMatrix.Transform(ABoundingBox.BottomLeft));
    Result.cx := Max(1, Sqrt(Max(CalculateDistance(ATopRight, ATopLeft),
      CalculateDistance(ABottomRight, ABottomLeft))));
    Result.cy := Max(1, Sqrt(Max(CalculateDistance(ATopLeft, ABottomLeft),
      CalculateDistance(ABottomRight, ATopRight))));
  end
  else
    Result := dxSizeF(0, 0);
end;

function TdxPDFCustomCommandInterpreter.TransformShadingPoint(APoint: TdxPointF)
  : TdxPointF;
var
  ABounds: TdxRectF;
  ATransformedPoint: TdxPointF;
begin
  ATransformedPoint := ToCanvasPoint(APoint);
  ABounds := Bounds;
  Result := dxPointF(ATransformedPoint.X - ABounds.Left,
    ATransformedPoint.Y - ABounds.Bottom);
end;

function TdxPDFCustomCommandInterpreter.VectorLength(const AMatrix
  : TdxPDFTransformationMatrix; X, Y: Single): Single;
var
  P: TdxPointF;
begin
  P := AMatrix.Transform(dxPointF(X, Y));
  Result := Sqrt(P.X * P.X + P.Y * P.Y);
end;

procedure TdxPDFCustomCommandInterpreter.Clip(AUseNonzeroWindingRule: Boolean);
begin
  FClipUseNonZeroWindingRule := AUseNonzeroWindingRule;
end;

procedure TdxPDFCustomCommandInterpreter.DrawForm(AForm: TdxPDFForm);

  function CanDrawForm: Boolean;
  begin
    Result := not IsCanceled and ((FCurrentFormNumber <> AForm.Number) or
      not TdxPDFUtils.IsIntegerValid(AForm.Number))
  end;

var
  ATilingMatrix: TdxPDFTransformationMatrix;
  I, APreviousGraphicsStateStackLock, ACountToRestore, APrevFormNumber: Integer;
begin
  if CanDrawForm then
  begin
    SaveGraphicsState;
    UpdateTransformationMatrix(AForm.Matrix);
    ATilingMatrix := TdxPDFTransformationMatrix.Create;
    ATilingMatrix.Assign(FTilingTransformMatrix);
    FTilingTransformMatrixStack.Push(ATilingMatrix);
    APreviousGraphicsStateStackLock := FGraphicsStateStackLock;
    APrevFormNumber := FCurrentFormNumber;
    FCurrentFormNumber := AForm.Number;
    FGraphicsStateStackLock := FGraphicsStateStack.Count;
    try
      UpdateTilingTransformationMatrix;
      ClipRectangle(AForm.BBox);
      ExecuteCommand(AForm.Commands);
    finally
      FCurrentFormNumber := APrevFormNumber;
      ACountToRestore := FGraphicsStateStack.Count - FGraphicsStateStackLock;
      for I := 0 to ACountToRestore - 1 do
        RestoreGraphicsState;
      FGraphicsStateStackLock := APreviousGraphicsStateStackLock;
      if FTilingTransformMatrixStack.Count > 0 then
        FTilingTransformMatrix := FTilingTransformMatrixStack.Extract;
      RestoreGraphicsState;
    end;
  end;
end;

procedure TdxPDFCustomCommandInterpreter.DrawString(const AData: TBytes;
  const ADataOffsets: TDoubleDynArray);
var
  ACharacterSpacing, AWordSpacing, AHorizontalScaling, AFontSize, AFontFactor,
    AWidth: Double;
  ACharCodes: TdxPDFBytesDynArray;
  ACodePointData: TdxPDFStringCommandData;
  AFont: TdxPDFCustomFont;
  AGlyphAdvanceSum, AGlyphAdvance, AFirstGlyphAdvance: Double;
  AGlyphOffsets, AGlyphAdvances, AWidths: TDoubleDynArray;
  AOffsets: TDoubleDynArray;
  AStr: TWordDynArray;
  AStringLength, I, J: Integer;
  ATextMatrix: TdxPDFTransformationMatrix;
  ATextState: TdxPDFTextState;
begin
  ATextState := TextState;
  AFont := TextStateFont;
  if AFont <> nil then
  begin
    AOffsets := ADataOffsets;
    ACodePointData := AFont.Encoding.GetStringData(AData, AOffsets);
    ACharCodes := ACodePointData.CharCodes;
    AStr := ACodePointData.Str;
    AGlyphOffsets := ACodePointData.Offsets;
    AStringLength := Length(AStr);
    SetLength(AGlyphAdvances, AStringLength);
    SetLength(AWidths, AStringLength);
    ACharacterSpacing := ATextState.CharacterSpacing;
    AWordSpacing := ATextState.WordSpacing;
    AHorizontalScaling := ATextState.HorizontalScaling / 100;
    AFontSize := ATextState.FontSize;
    AFontFactor := AFont.WidthFactor;
    AGlyphAdvanceSum := 0;
    J := 1;
    for I := 0 to AStringLength - 1 do
    begin
      AWidth := AFont.GetCharacterWidth(AStr[I]) * AFontFactor * AFontSize;
      AGlyphAdvance := AWidth - AGlyphOffsets[J] *
        GlyphPositionToTextSpaceFactor * AFontSize + ACharacterSpacing;
      if (Length(ACharCodes[I]) = 1) and (ACharCodes[I][0] = 32) then
        AGlyphAdvance := AGlyphAdvance + AWordSpacing;
      AGlyphAdvance := AGlyphAdvance * AHorizontalScaling;
      AGlyphAdvances[I] := AGlyphAdvance;
      AWidths[I] := AWidth;
      AGlyphAdvanceSum := AGlyphAdvanceSum + AGlyphAdvance;
      Inc(J);
    end;
    AFirstGlyphAdvance := AGlyphOffsets[0] * GlyphPositionToTextSpaceFactor *
      AFontSize;
    ATextMatrix := TextState.TextMatrix;
    FGraphicsState.TextState.TextMatrix := TdxPDFTransformationMatrix.Multiply
      (TdxPDFTransformationMatrix.Create(1, 0, 0, 1, -AFirstGlyphAdvance, 0),
      ATextMatrix);
    DrawString(TdxPDFStringData.Create(ACodePointData, AWidths,
      AGlyphAdvances));
    FGraphicsState.TextState.TextMatrix := TdxPDFTransformationMatrix.Multiply
      (TdxPDFTransformationMatrix.Create(1, 0, 0, 1,
      AGlyphAdvanceSum - AFirstGlyphAdvance, 0), ATextMatrix);
  end;
end;

procedure TdxPDFCustomCommandInterpreter.DrawString
  (const AData: TdxPDFStringData);
begin
  // do nothing
end;

procedure TdxPDFCustomCommandInterpreter.DrawTransparencyGroup
  (AForm: TdxPDFGroupForm);
begin
  DrawForm(AForm);
end;

procedure TdxPDFCustomCommandInterpreter.Export(APages: TdxPDFPages;
  AParameters: TdxPDFExportParameters);
var
  I: Integer;
begin
  for I := 0 to APages.Count - 1 do
    Export(APages[I], AParameters);
end;

procedure TdxPDFCustomCommandInterpreter.ExecuteCommand
  (ACommands: TdxPDFCommandList);
begin
  ExecuteCommand(Self, ACommands);
end;

procedure TdxPDFCustomCommandInterpreter.ExecuteCommand(const AInterpreter
  : IdxPDFCommandInterpreter; ACommands: TdxPDFCommandList);
var
  I: Integer;
begin
  for I := 0 to ACommands.Count - 1 do
  begin
    if Assigned(FParameters.CancelCallback) and FParameters.CancelCallback then
      Exit;

    try
      ACommands[I].Execute(AInterpreter);
    except
      on EdxGdipException do;
      on EdxPDFException do;
      on EdxPDFAbortException do;
      else
        raise;
    end;
  end;
end;

procedure TdxPDFCustomCommandInterpreter.UpdateTransformationMatrix
  (const AMatrix: TdxPDFTransformationMatrix);
begin
  SetTransformationMatrix(AMatrix);
end;

procedure TdxPDFCustomCommandInterpreter.MoveToNextLine;
begin
  SetTextMatrix(dxPointF(0, -TextState.Leading));
end;

procedure TdxPDFCustomCommandInterpreter.SetCharacterSpacing(ASpacing: Single);
begin
  FGraphicsState.TextState.CharacterSpacing := ASpacing;
end;

procedure TdxPDFCustomCommandInterpreter.SetColorSpaceForNonStrokingOperations
  (AColorSpace: TdxPDFCustomColorSpace);
begin
  FGraphicsState.NonStrokingColorSpace := AColorSpace;
end;

procedure TdxPDFCustomCommandInterpreter.SetColorSpaceForStrokingOperations
  (AColorSpace: TdxPDFCustomColorSpace);
begin
  FGraphicsState.StrokingColorSpace := AColorSpace;
end;

procedure TdxPDFCustomCommandInterpreter.SetFlatnessTolerance(AValue: Double);
begin
  FGraphicsState.Parameters.FlatnessTolerance := AValue;
end;

procedure TdxPDFCustomCommandInterpreter.SetLineCapStyle(ALineCapStyle
  : TdxPDFLineCapStyle);
begin
  FGraphicsState.Parameters.LineCapStyle := ALineCapStyle;
end;

procedure TdxPDFCustomCommandInterpreter.SetLineJoinStyle(ALineJoinStyle
  : TdxPDFLineJoinStyle);
begin
  FGraphicsState.Parameters.LineJoinStyle := ALineJoinStyle;
end;

procedure TdxPDFCustomCommandInterpreter.SetLineStyle
  (ALineStyle: TdxPDFLineStyle);
begin
  FGraphicsState.Parameters.LineStyle := ALineStyle;
end;

procedure TdxPDFCustomCommandInterpreter.SetLineWidth(ALineWidth: Single);
begin
  FGraphicsState.Parameters.LineWidth := ALineWidth;
end;

procedure TdxPDFCustomCommandInterpreter.SetMiterLimit(AMiterLimit: Single);
begin
  FGraphicsState.Parameters.MiterLimit := AMiterLimit;
end;

procedure TdxPDFCustomCommandInterpreter.SetRenderingIntent
  (AValue: TdxPDFRenderingIntent);
begin
  GraphicsState.Parameters.RenderingIntent := AValue;
end;

procedure TdxPDFCustomCommandInterpreter.SetTextLeading(ALeading: Double);
begin
  FGraphicsState.TextState.Leading := ALeading;
end;

procedure TdxPDFCustomCommandInterpreter.SetTextHorizontalScaling
  (AValue: Double);
begin
  FGraphicsState.TextState.HorizontalScaling := AValue;
end;

procedure TdxPDFCustomCommandInterpreter.SetTextMatrix(const AOffset
  : TdxPointF);
var
  M: TdxPDFTransformationMatrix;
begin
  M := TdxPDFTransformationMatrix.Create(1, 0, 0, 1, AOffset.X, AOffset.Y);
  M.Multiply(TextState.TextLineMatrix, moAppend);
  SetTextMatrix(M);
end;

procedure TdxPDFCustomCommandInterpreter.SetTextRise(AValue: Double);
begin
  TextState.Rise := AValue;
end;

procedure TdxPDFCustomCommandInterpreter.SetWordSpacing(AWordSpacing: Double);
begin
  FGraphicsState.TextState.WordSpacing := AWordSpacing;
end;

procedure TdxPDFCustomCommandInterpreter.StrokePaths;
begin
  // do nothing
end;

procedure TdxPDFCustomCommandInterpreter.UnknownCommand(const AName: string);
begin
  // do nothing
end;

function TdxPDFCustomCommandInterpreter.CalculateDistance(const P1,
  P2: TdxPointF): Integer;

  function CalculateDistanceSquare(const P1, P2: TdxPointF): Double;
  var
    ADx, ADy: Double;
  begin
    ADx := P2.X - P1.X;
    ADy := P2.Y - P1.Y;
    Result := ADx * ADx + ADy * ADy;
  end;

begin
  Result := Ceil(Sqrt(CalculateDistanceSquare(P1, P2)));
end;

function TdxPDFCustomCommandInterpreter.GetActualSize: TSize;
begin
  Result := FActualSize;
end;

function TdxPDFCustomCommandInterpreter.GetBounds: TdxRectF;
begin
  Result := FParameters.Bounds;
end;

function TdxPDFCustomCommandInterpreter.GetCurrentPath: TdxPDFGraphicsPath;
begin
  if FPaths.Count = 0 then
    Result := nil
  else
    Result := FPaths.Last;
end;

function TdxPDFCustomCommandInterpreter.GetDeviceTransformationMatrix
  : TdxPDFTransformationMatrix;
var
  AScale: Double;
begin
  AScale := FParameters.ScaleFactor;
  FGraphicsState.DeviceTransformMatrix.Assign(AScale, 0, 0, -AScale,
    FBoundsOffset.X, FBoundsOffset.Y);
  Result := FGraphicsState.DeviceTransformMatrix;
end;

function TdxPDFCustomCommandInterpreter.GetDocumentState: TdxPDFDocumentState;
begin
  Result := FParameters.DocumentState;
end;

function TdxPDFCustomCommandInterpreter.GetRotationAngle: Single;
begin
  Result := FAngle * PI / 180;
end;

function TdxPDFCustomCommandInterpreter.GetTextState: TdxPDFTextState;
begin
  Result := FGraphicsState.TextState;
end;

function TdxPDFCustomCommandInterpreter.GetTextStateFont: TdxPDFCustomFont;
begin
  Result := TextState.Font as TdxPDFCustomFont;
end;

function TdxPDFCustomCommandInterpreter.GetTransformMatrix
  : TdxPDFTransformationMatrix;
begin
  Result := FGraphicsState.TransformMatrix;
end;

function TdxPDFCustomCommandInterpreter.NeedDrawAnnotation
  (AAnnotation: TdxPDFCustomAnnotation): Boolean;
begin
  Result := not IsCanceled and TdxPDFCustomAnnotationAccess
    (AAnnotation).Visible;
end;

procedure TdxPDFCustomCommandInterpreter.DrawAnnotation
  (AAnnotation: TdxPDFCustomAnnotation);
var
  AForm: TdxPDFForm;
begin
  if NeedDrawAnnotation(AAnnotation) then
  begin
    AForm := TdxPDFCustomAnnotationAccess(AAnnotation)
      .GetAppearanceForm(DocumentState);
    if AForm <> nil then
    begin
      SaveGraphicsState;
      try
        InitializeTransformMatrix;
        UpdateTransformationMatrix
          (AForm.GetTransformationMatrix(TdxPDFCustomAnnotationAccess
          (AAnnotation).Rect));
        DrawForm(AForm);
      finally
        RestoreGraphicsState;
      end;
    end;
  end;
end;

procedure TdxPDFCustomCommandInterpreter.ExportAnnotation;
var
  AAnnotation: TdxPDFReferencedObject;
begin
  if (FParameters.Annotations <> nil) and not IsCanceled then
  begin
    GraphicsState.Reset;
    for AAnnotation in FParameters.Annotations do
      DrawAnnotation(AAnnotation as TdxPDFCustomAnnotation);
  end;
end;

procedure TdxPDFCustomCommandInterpreter.InitializeTransformedBoundingBox;
var
  ABottomLeft, ATopRight: TdxPointF;
  ABounds: TdxRectF;
begin
  ABounds := Bounds;
  ABottomLeft := TransformMatrix.Transform(Bounds.BottomLeft);
  ATopRight := TransformMatrix.Transform(Bounds.TopRight);
  SetLength(FTransformedBoundingBox, 4);
  FTransformedBoundingBox[0] := ABottomLeft;
  FTransformedBoundingBox[1] := dxPointF(ABottomLeft.X, ATopRight.Y);
  FTransformedBoundingBox[2] := ATopRight;
  FTransformedBoundingBox[3] := dxPointF(ATopRight.X, ABottomLeft.Y);
end;

procedure TdxPDFCustomCommandInterpreter.RecreatePaths;
begin
  FreeAndNil(FPaths);
  FPaths := TObjectList<TdxPDFGraphicsPath>.Create;
end;

{ TdxPDFRenderingInterpreter }

procedure TdxPDFRenderingInterpreter.EndText;
begin
  inherited EndText;
  DoEndText;
  ApplySemitransparentText;
end;

procedure TdxPDFRenderingInterpreter.Initialize;
begin
  FGraphics := CreateGraphics;
  FFontQuality := DEFAULT_QUALITY;
  FPrevAlpha := 255;
  InitializeGraphics;
  InitializeClipRegion;
  inherited Initialize;
  UpdateClip;
  UpdateBrushes;

  SetLength(FImageDestinationPoints, 3);
  FImageDestinationPoints[0] := dxPointF(0, 0);
  FImageDestinationPoints[1] := dxPointF(1, 0);
  FImageDestinationPoints[2] := dxPointF(0, 1);

  SetLength(FCorrectedImageDestinationPoints, 3);
  FCorrectedImageDestinationPoints[0] := dxPointF(-0.0008, -0.0008);
  FCorrectedImageDestinationPoints[1] := dxPointF(1.0008, -0.0008);
  FCorrectedImageDestinationPoints[2] := dxPointF(-0.0008, 1.0008);
end;

procedure TdxPDFRenderingInterpreter.InitializeClipBounds(const ATopLeft,
  ABottomRight: TdxPointF);
var
  ABottomLeft, ATopRight: TdxPointF;
begin
  inherited InitializeClipBounds(ATopLeft, ABottomRight);
  ABottomLeft.X := Bounds.Left;
  ABottomLeft.Y := Bounds.Bottom;
  ATopRight.X := Bounds.Right;
  ATopRight.Y := Bounds.Top;

  FBoundingBoxClipper := TdxPDFPolygonClipper.Create
    (TdxPDFRectangle.Create(TransformMatrix.Transform(ABottomLeft),
    TransformMatrix.Transform(ATopRight)));
end;

procedure TdxPDFRenderingInterpreter.FillPaths(AUseNonzeroWindingRule: Boolean);
var
  ABounds: TdxRectF;
  APathFillInfo: TdxPDFFillPathInfo;
  APen: TdxGPPen;
  ATransformedPaths: TObjectList<TdxPDFGraphicsPath>;
begin
  ATransformedPaths := CreateTransformedPaths;
  try
    APathFillInfo := TdxPDFGraphicsPathBuilder.CreateFillPath(Self,
      ATransformedPaths);
    try
      APathFillInfo.GraphicsPath.FillMode :=
        GetFillMode(AUseNonzeroWindingRule);
      APen := CreatePen;
      try
        APen.Brush.Assign(FNonStrokingBrush);
        APen.Width := 1;
        if APathFillInfo.GraphicsPath.TryGetBoundsF(ABounds, APen) = Ok then
          PerformRendering(ABounds, procedure
            var
              ASmoothingMode: TdxGPSmoothingMode;
            begin
              ASmoothingMode := Graphics.SmoothingMode;
              try
                UpdateSmoothingMode(True);
                if FNonStrokingBrush <> nil then
                  GdipFillPath(Graphics.Handle, FNonStrokingBrush.Handle,
                    APathFillInfo.GraphicsPath.Handle);
                if APathFillInfo.StrokePath <> nil then
                  GdipDrawPath(Graphics.Handle, APen.Handle,
                    APathFillInfo.StrokePath.Handle);
              finally
                FGraphics.SmoothingMode := ASmoothingMode;
              end;
            end);
      finally
        APen.Free;
      end;
    finally
      APathFillInfo.Free;
    end;
  finally
    ATransformedPaths.Free;
  end;
end;

procedure TdxPDFRenderingInterpreter.Finalize;
begin
  EndText;
  DestroyGraphics;
  inherited Finalize;
end;

procedure TdxPDFRenderingInterpreter.RestoreGraphicsState;

  function RestoreBrush(AStack: TObjectStack<TdxGPBrush>; ABrush: TdxGPBrush)
    : TdxGPBrush;
  begin
    if AStack.Count > 0 then
    begin
      DestroyBrush(ABrush);
      Result := AStack.Extract;
    end
    else
      Result := ABrush;
  end;

  procedure RestoreBrushes;
  begin
    FStrokingBrush := RestoreBrush(FStrokingBrushStack, FStrokingBrush);
    FNonStrokingBrush := RestoreBrush(FNonStrokingBrushStack,
      FNonStrokingBrush);
  end;

begin
  inherited RestoreGraphicsState;
  RestoreBrushes;
  if FCurrentClipRegion <> nil then
    FCurrentClipRegion.Free;
  if FRegionStack.Count > 0 then
    FCurrentClipRegion := FRegionStack.Extract
  else
    FCurrentClipRegion := nil;
  UpdateClip;
end;

procedure TdxPDFRenderingInterpreter.SaveGraphicsState;

  procedure SaveClipRegion;
  begin
    if FCurrentClipRegion <> nil then
      FRegionStack.Push(Clone(FCurrentClipRegion));
  end;

  procedure SaveBrushes;
  begin
    FStrokingBrushStack.Push(Clone(FStrokingBrush));
    FNonStrokingBrushStack.Push(Clone(FNonStrokingBrush));
  end;

begin
  inherited SaveGraphicsState;
  SaveClipRegion;
  SaveBrushes;
end;

procedure TdxPDFRenderingInterpreter.SetColorForNonStrokingOperations
  (const AColor: TdxPDFColor);
begin
  inherited SetColorForNonStrokingOperations(AColor);
  UpdateNonStrokingBrush;
end;

procedure TdxPDFRenderingInterpreter.SetColorForStrokingOperations
  (const AColor: TdxPDFColor);
begin
  inherited SetColorForStrokingOperations(AColor);
  UpdateStrokingBrush;
end;

procedure TdxPDFRenderingInterpreter.SetTextFont(AFont: TdxPDFCustomFont;
AFontSize: Double);
begin
  inherited SetTextFont(AFont, AFontSize);
  UpdateCurrentFont;
end;

procedure TdxPDFRenderingInterpreter.SetTextMatrix(const AMatrix
  : TdxPDFTransformationMatrix);
begin
  inherited SetTextMatrix(AMatrix);
  UpdateCurrentFont;
end;

procedure TdxPDFRenderingInterpreter.SetTextRenderingMode
  (AMode: TdxPDFTextRenderingMode);
begin
  inherited SetTextRenderingMode(AMode);
  UpdateCurrentFont;
end;

function TdxPDFRenderingInterpreter.CreateTilingBitmap
  (APattern: TdxPDFTilingPattern; const ASize, AKeySize: TSize;
const AColor: TdxPDFColor): TcxBitmap32;
var
  ABitmap: TcxBitmap32;
  I, J: Integer;
  Y, X: Single;
  AStep: TdxPointF;
  AStepCount, AMaxStepCount: TPoint;
begin
  if (AKeySize.cx = 0) or (AKeySize.cy = 0) or (ASize.cx = 0) or (ASize.cy = 0)
  then
    Exit(nil);
  Result := TcxBitmap32.CreateSize(ASize.cx, ASize.cy);
  if (ASize.cx >= AKeySize.cx) and (ASize.cy >= AKeySize.cy) then
    DrawTilingCell(Result, APattern, AKeySize, AColor)
  else
  begin
    ABitmap := TcxBitmap32.CreateSize(AKeySize.cx, AKeySize.cy);
    ABitmap.Canvas.Lock;
    try
      DrawTilingCell(ABitmap, APattern, AKeySize, AColor);
      AStepCount := cxPoint(Ceil(AKeySize.cx / ASize.cx - 1),
        Ceil(AKeySize.cx / ASize.cy - 1));
      AStep := dxPointF(IfThen(APattern.XStep < 0, -ASize.cx, ASize.cx),
        IfThen(APattern.YStep > 0, -ASize.cy, ASize.cy));
      AMaxStepCount := cxPoint(AStepCount.X * 2, AStepCount.Y * 2);
      Y := -AStep.Y * AStepCount.Y;
      cxPaintCanvas.BeginPaint(Result.Canvas.Handle);
      try
        for I := 0 to AMaxStepCount.Y - 1 do
        begin
          X := -AStep.X * AStepCount.X;
          for J := 0 to AMaxStepCount.X - 1 do
          begin
            X := X + AStep.X;
            cxPaintCanvas.Draw(Trunc(X), Trunc(Y), ABitmap);
          end;
          Y := Y + AStep.Y;
        end;
      finally
        cxPaintCanvas.EndPaint;
      end;
    finally
      ABitmap.Canvas.Unlock;
      ABitmap.Free;
    end;
  end;
end;

function TdxPDFRenderingInterpreter.GetBoundsOffset: TdxPointF;
begin
  Result := GetRenderParameters.Position;
end;

function TdxPDFRenderingInterpreter.MinGraphicsStateCount: Integer;
begin
  Result := FGraphicsStateStackLock;
end;

procedure TdxPDFRenderingInterpreter.ApplyGraphicsStateParameters
  (AParameters: TdxPDFGraphicsStateParameters);
begin
  inherited ApplyGraphicsStateParameters(AParameters);
  if IsTextRendering and (GetNonStrokingColorAlpha <> FPrevAlpha) then
  begin
    DoEndText;
    ApplySemitransparentText;
    InitializeSemitransparentText;
    DoBeginText;
  end;
  UpdateBrushes;
end;

procedure TdxPDFRenderingInterpreter.BeginText;
begin
  inherited BeginText;
end;

function TdxPDFRenderingInterpreter.CreateBoundsClippedTransformedPaths
  : TObjectList<TdxPDFGraphicsPath>;
var
  APath: TdxPDFGraphicsPath;
  ATransformedPaths: TList<TdxPDFGraphicsPath>;
begin
  Result := TObjectList<TdxPDFGraphicsPath>.Create;
  ATransformedPaths := CreateTransformedPaths;
  try
    for APath in ATransformedPaths do
      Result.Add(FBoundingBoxClipper.Clip(APath));
    for APath in Result do
      ATransformedPaths.Extract(APath);
  finally
    ATransformedPaths.Free;
  end;
end;

function TdxPDFRenderingInterpreter.CreateTransformedPaths
  : TObjectList<TdxPDFGraphicsPath>;
begin
  Result := TdxPDFGraphicsPath.Transform(Paths, GraphicsState.TransformMatrix);
end;

procedure TdxPDFRenderingInterpreter.ClipPaths;
var
  APathRegion: TdxGPRegion;
begin
  APathRegion := TdxPDFGraphicsPathBuilder.CreateRegion(Self);
  if APathRegion <> nil then
  begin
    if FCurrentClipRegion = nil then
      FCurrentClipRegion := APathRegion
    else
    begin
      FCurrentClipRegion.CombineRegionRegion(APathRegion, gmIntersect);
      APathRegion.Free;
    end;
  end;
  UpdateClip;
end;

procedure TdxPDFRenderingInterpreter.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FSolidBrushCache := TObjectDictionary<TdxAlphaColor, TdxGPBrush>.Create
    ([doOwnsValues]);
  FStrokingBrush := TdxGPBrush.Create;
  FNonStrokingBrush := TdxGPBrush.Create;
  FActualFontNameCache := TdxPDFStringStringDictionary.Create;
  FStrokingBrushStack := TObjectStack<TdxGPBrush>.Create;
  FNonStrokingBrushStack := TObjectStack<TdxGPBrush>.Create;
  FRegionStack := TObjectStack<TdxGPRegion>.Create;
end;

procedure TdxPDFRenderingInterpreter.DestroySubClasses;

  procedure ClearBrushStack(AStack: TObjectStack<TdxGPBrush>);
  var
    ABrush: TdxGPBrush;
  begin
    while (AStack <> nil) and (AStack.Count > 0) do
    begin
      ABrush := AStack.Extract;
      DestroyBrush(ABrush);
    end;
  end;

begin
  FreeAndNil(FRegionStack);
  ClearBrushStack(FNonStrokingBrushStack);
  ClearBrushStack(FStrokingBrushStack);
  FreeAndNil(FNonStrokingBrushStack);
  FreeAndNil(FStrokingBrushStack);
  FreeAndNil(FActualFontNameCache);
  FreeAndNil(FClipRegion);
  FreeAndNil(FCurrentClipRegion);
  FreeAndNil(FInitialClipRegion);
  FreeAndNil(FBoundingBoxClipper);
  DestroyBrush(FNonStrokingBrush);
  DestroyBrush(FStrokingBrush);
  FreeAndNil(FSolidBrushCache);
  FreeAndNil(FSemitransparentTextBitmap);
  inherited DestroySubClasses;
end;

function TdxPDFRenderingInterpreter.ShouldCorrectInterpolationGaps
  (ABitsPerComponent, APixelFormat: Integer): Boolean;
begin
  Result := (ABitsPerComponent > 1) and
    (GraphicsState.Parameters.NonStrokingColorAlpha = 1) and
    (APixelFormat <> PixelFormat32bppARGB);
end;

function TdxPDFRenderingInterpreter.CreateImageAttributes: TdxGPImageAttributes;
var
  AMatrix: TdxGpColorMatrix;
begin
  Result := TdxGPImageAttributes.Create;
  if GraphicsState.Parameters.NonStrokingColorAlpha <> 1 then
  begin
    AMatrix := dxGpDefaultColorMatrix;
    AMatrix[3, 3] := GraphicsState.Parameters.NonStrokingColorAlpha;
    Result.SetColorMatrix(@AMatrix, ColorMatrixFlagsDefault,
      ColorAdjustTypeBitmap);
  end;
  Result.SetWrapMode(WrapModeTileFlipXY);
end;

function TdxPDFRenderingInterpreter.GetFillMode: TdxGPFillMode;
begin
  Result := GetFillMode(ClipUseNonZeroWindingRule.Value);
end;

function TdxPDFRenderingInterpreter.GetInterpolationMode(AImageWidth,
  AImageHeight, ABitsPerComponent: Integer): TdxGPInterpolationMode;
var
  AMatrix: TdxPDFTransformationMatrix;
  ALocation: TdxPointF;
  AWidth, AHeight: Double;
begin
  if FParameters.ScaleFactor < 96 / 72 then
    Result := imHighQualityBicubic
  else
  begin
    AMatrix := GraphicsState.TransformMatrix;
    ALocation := AMatrix.Transform(dxNullPointF);
    AWidth := TdxPDFUtils.Distance(AMatrix.Transform(dxPointF(1, 0)),
      ALocation);
    AHeight := TdxPDFUtils.Distance(AMatrix.Transform(dxPointF(0, 1)),
      ALocation);
    if (ABitsPerComponent = 1) and ((AImageWidth < AWidth * 1.35) or
      (AImageHeight < AHeight * 1.35)) then
      Result := imNearestNeighbor
    else
      Result := imHighQualityBicubic;
  end;
end;

procedure TdxPDFRenderingInterpreter.DrawImage(AImage: TdxPDFDocumentImage);
var
  AImageDataItem: TdxPDFImageCacheItem;
  APoints: TdxPDFPoints;
  ATopLeft: TdxPointF;
  ATransform: TdxPDFTransformationMatrix;
begin
  ATransform := GetImageMatrix
    (ToCanvasPoint(TransformMatrix.Transform(dxPointF(0, 0))));
  ATransform.Translate(0, -1);
  APoints := ATransform.TransformPoints(FImageDestinationPoints);
  ATopLeft := APoints[0];
  AImageDataItem := GetImageData(AImage, CalculateDistance(ATopLeft, APoints[1]
    ), CalculateDistance(ATopLeft, APoints[2]));
  if not AImageDataItem.IsNull then
    try
      PerformRendering(CreateBoundingRectangle(APoints), procedure
        var
          APrevInterpolationMode: TdxGPInterpolationMode;
          AImageTransform, AWorldTransform: TdxGPMatrix;
        begin
          APrevInterpolationMode := Graphics.InterpolationMode;
          AImageTransform := TdxPDFUtils.ConvertToGpMatrix(ATransform);
          AWorldTransform := Graphics.GetWorldTransform;
          Graphics.SaveWorldTransform;
          try
            AWorldTransform.Multiply(AImageTransform);
            if UpdateWorldTransform(Graphics, AWorldTransform) then
              AImageDataItem.Data.DrawImage(Self);
          finally
            AWorldTransform.Free;
            AImageTransform.Free;
            Graphics.RestoreWorldTransform;
            Graphics.InterpolationMode := APrevInterpolationMode;
          end;
        end);
    finally
      if AImageDataItem.ShouldDispose then
        AImageDataItem.Data.Free;
    end;
end;

class procedure TdxPDFRenderingInterpreter.PerformRendering(const AData: TBytes;
AWidth, AHeight, AStride, APixelFormat: Integer;
const AImagePalette: TdxAlphaColorDynArray; AAction: TProc<GpBitmap>);
var
  ABitmap: GpBitmap;
  AEntry: PdxAlphaColor;
  APalette: PGpColorPalette;
  APaletteSize, I: Integer;
begin
  GdipCheck(GdipCreateBitmapFromScan0(AWidth, AHeight, AStride, APixelFormat,
    @AData[0], ABitmap));
  try
    GdipCheck(GdipGetImagePaletteSize(ABitmap, APaletteSize));
    GetMem(APalette, APaletteSize);
    try
      GdipCheck(GdipGetImagePalette(ABitmap, APalette, APaletteSize));
      if AImagePalette <> nil then
      begin
        AEntry := @APalette.Entries;
        for I := 0 to Length(AImagePalette) - 1 do
        begin
          AEntry^ := AImagePalette[I];
          Inc(AEntry);
        end;
        GdipCheck(GdipSetImagePalette(ABitmap, APalette));
      end;
      AAction(ABitmap);
    finally
      FreeMem(APalette);
    end;
  finally
    GdipDisposeImage(ABitmap);
  end;
end;

class procedure TdxPDFRenderingInterpreter.CreateBitmap(const AImageData
  : TdxPDFImageData; AIsMask: Boolean; AAction: TProc<GpBitmap>);
begin
  PerformRendering(TdxPDFImageDataCache.GetImageRaster(AImageData),
    AImageData.Width, AImageData.Height, AImageData.Stride,
    TdxPDFUtils.ConvertToGpPixelFormat(AImageData.PixelFormat),
    ConvertPalette(AIsMask, AImageData.Palette, TdxAlphaColors.Default),
    procedure(ABitmap: GpBitmap)
    begin
      if Assigned(AAction) then
        AAction(ABitmap);
    end);
end;

procedure TdxPDFRenderingInterpreter.DrawImage
  (AImageData: TdxPDFImageDataCacheItem; const AData: TBytes);
var
  APixelFormat: Integer;
begin
  APixelFormat := TdxPDFUtils.ConvertToGpPixelFormat(AImageData.PixelFormat);
  PerformRendering(AData, AImageData.Parameters.Width,
    AImageData.Parameters.Height, AImageData.Stride, APixelFormat,
    ConvertPalette(AImageData.Image.IsMask, AImageData.Palette),
    procedure(ABitmap: GpBitmap)
    begin
      DrawImage(AImageData.Image.GetBitsPerComponent, APixelFormat,
        AImageData.Parameters.Width, AImageData.Parameters.Height, ABitmap);
    end);
end;

procedure TdxPDFRenderingInterpreter.DrawImage
  (AImageData: TdxPDFImageDataCacheItem;
const ADataSource: IdxPDFImageScanlineSource);
var
  ABuffer: TBytes;
  AFactor, APosition: Single;
  AImage: IdxPDFDocumentImage;
  AMaxBufferSize, AStride, AHeight, AImageRasterSize, AScanlineInBufferCount,
    AWidth, I, AActualHeight: Integer;
  APalette: TdxAlphaColorDynArray;
  APixelFormat: TdxGpPixelFormat;
begin
  AMaxBufferSize := 50 * 1024 * 1024;
  APixelFormat := TdxPDFUtils.ConvertToGpPixelFormat(AImageData.PixelFormat);
  AStride := AImageData.Stride;
  AHeight := AImageData.Parameters.Height;
  AImageRasterSize := AStride * AHeight;
  AImage := AImageData.Image;
  APalette := ConvertPalette(AImage.IsMask, AImageData.Palette);
  if AImageRasterSize < AMaxBufferSize then
  begin
    SetLength(ABuffer, AImageRasterSize);
    ADataSource.FillBuffer(ABuffer, AHeight);
    PerformRendering(ABuffer, AImageData.Parameters.Width, AHeight, AStride,
      APixelFormat, APalette, procedure(ABitmap: GpBitmap)
      begin
        DrawImage(AImageData, AImageData.Parameters.Width, AHeight,
          ABitmap, 0, 1);
      end);
  end
  else
  begin
    AScanlineInBufferCount := Max(1, AMaxBufferSize div AStride);
    SetLength(ABuffer, AScanlineInBufferCount * AStride);
    AFactor := AScanlineInBufferCount / AHeight;
    APosition := 0;
    AWidth := AImageData.Parameters.Width;
    I := 0;
    while I < AHeight do
    begin
      AActualHeight := Min(AScanlineInBufferCount, AHeight - I);
      ADataSource.FillBuffer(ABuffer, AActualHeight);
      PerformRendering(ABuffer, AWidth, AActualHeight, AStride, APixelFormat,
        APalette, procedure(ABitmap: GpBitmap)
        begin
          DrawImage(AImageData, AImageData.Parameters.Width, AHeight, ABitmap,
            APosition, Min(1, APosition + AFactor));
        end);
      APosition := APosition + AFactor;
      Inc(I, AScanlineInBufferCount);
    end;
  end;
end;

procedure TdxPDFRenderingInterpreter.DrawImage(ABitsPerComponent, APixelFormat,
  AImageWidth, AImageHeight: Integer; ABitmap: GpBitmap);
var
  AAttributes: TdxGPImageAttributes;
  AImageSize: TSize;
begin
  AImageSize := cxSize(AImageWidth, AImageHeight);
  if ShouldCorrectInterpolationGaps(ABitsPerComponent, APixelFormat) then
  begin
    AAttributes := CreateImageAttributes;
    try
      FGraphics.InterpolationMode := imNearestNeighbor;
      DrawBitmap(ABitmap, AImageSize, FCorrectedImageDestinationPoints, nil);
      Graphics.InterpolationMode := imHighQualityBicubic;
      DrawBitmap(ABitmap, AImageSize, FImageDestinationPoints, AAttributes);
    finally
      AAttributes.Free;
    end;
  end
  else
  begin
    Graphics.InterpolationMode := GetInterpolationMode(AImageWidth,
      AImageHeight, ABitsPerComponent);
    DrawBitmap(ABitmap, AImageSize, FImageDestinationPoints, nil);
  end;
end;

procedure TdxPDFRenderingInterpreter.DrawImage
  (AImageData: TdxPDFImageDataCacheItem; AWidth, AHeight: Integer;
ABitmap: GpBitmap; ATop, ABottom: Single);
var
  ABitsPerComponent: Integer;
  AImage: IdxPDFDocumentImage;
  APixelFormat: TdxGpPixelFormat;
  APoints: TdxPointsF;
begin
  AImage := AImageData.Image;
  APixelFormat := TdxPDFUtils.ConvertToGpPixelFormat(AImageData.PixelFormat);
  ABitsPerComponent := AImage.GetBitsPerComponent;
  if ShouldCorrectInterpolationGaps(ABitsPerComponent, APixelFormat) then
    Graphics.InterpolationMode := imDefault
  else
    Graphics.InterpolationMode := GetInterpolationMode(AWidth, AHeight,
      ABitsPerComponent);
  SetLength(APoints, 3);
  APoints[0] := dxPointF(0, ATop);
  APoints[1] := dxPointF(1, ATop);
  APoints[2] := dxPointF(0, ABottom);
  DrawImage(ABitmap, AWidth, AHeight, APoints);
end;

procedure TdxPDFRenderingInterpreter.DrawImage(ABitmap: GpBitmap;
AWidth, AHeight: Integer; const APoints: TdxPointsF);
var
  AAttributes: TdxGPImageAttributes;
begin
  if GraphicsState.Parameters.NonStrokingColorAlpha = 1 then
    DrawBitmap(ABitmap, cxSize(AWidth, AHeight), APoints, nil)
  else
  begin
    AAttributes := CreateImageAttributes;
    try
      DrawBitmap(ABitmap, cxSize(AWidth, AHeight), APoints, AAttributes);
    finally
      AAttributes.Free;
    end;
  end;
end;

procedure TdxPDFRenderingInterpreter.DrawString(const AGlyphs: TWordDynArray;
const ASpacing: TIntegerDynArray; const AXForm: TXForm);

  procedure TextOut(AOptions: Integer; AGlyphs: PSmallInt; AGlyphCount: Integer;
  ASpacing: PInteger);
  var
    R: TRect;
    ASavedWorldTransform: TXForm;
  begin
    GetWorldTransform(FHDC, ASavedWorldTransform);
    try
      SetWorldTransform(FHDC, AXForm);
      ExtTextOut(FHDC, 0, 0, AOptions, @R, PChar(AGlyphs), AGlyphCount,
        ASpacing);
    finally
      SetWorldTransform(FHDC, ASavedWorldTransform);
    end;
  end;

var
  AFontProgramFacade: TdxPDFFontProgramFacade;
  AGlyphCount: Integer;
  AStr: TSmallIntDynArray;
begin
  AGlyphCount := Length(AGlyphs);
  SetLength(AStr, AGlyphCount);
  cxCopyData(@AGlyphs[0], @AStr[0], 0, 0, AGlyphCount * SizeOf(Word));
  AFontProgramFacade := TextStateFont.FontProgramFacade as
    TdxPDFFontProgramFacade;
  TextOut(IfThen(AFontProgramFacade.UpdateCodePoints(AStr,
    FUseEmbeddedFontEncoding), ETO_GLYPH_INDEX, ETO_IGNORELANGUAGE), @AStr[0],
    AGlyphCount, @ASpacing[0]);
end;

procedure TdxPDFRenderingInterpreter.DrawString(const AStringData
  : TWordDynArray; const AOffsets: TDoubleDynArray);
var
  AAdvance, ASpacingScale, ASpacingScaleDenominator, ADiff: Double;
  ANext: Integer;
  AOffset: TdxPointF;
  ASpacing: TIntegerDynArray;
  AStringLength, APreviousX, I: Integer;
  ATransform: TdxPDFTransformationMatrix;
  AXForm: TXForm;
begin
  if not IsTextRendering and (GraphicsState.Parameters.NonStrokingColorAlpha <>
    1) and (FSemitransparentTextBitmap = nil) then
    InitializeSemitransparentText;
  DoBeginText;
  ASpacingScaleDenominator := TextState.FontSize * TextState.HorizontalScaling;
  if ASpacingScaleDenominator = 0 then
    Exit;
  ATransform := TdxPDFTransformationMatrix.Create(1, 0, 0, -1, 0, 0);
  ATransform.Multiply(TextState.TextSpaceMatrix, moAppend);
  ATransform.Multiply(TextState.TextMatrix, moAppend);
  ATransform.Multiply(GraphicsState.TransformMatrix, moAppend);
  ATransform.Multiply(DeviceTransformMatrix, moAppend);
  AOffset.X := ATransform.E;
  AOffset.Y := ATransform.F;
  AXForm := TXForm.CreateMatrix(ATransform.A, ATransform.B, ATransform.C,
    ATransform.D, AOffset.X * TextRenderingPrecision,
    AOffset.Y * TextRenderingPrecision);
  ADiff := 0;
  APreviousX := 0;
  AAdvance := 0;
  ASpacingScale := (1 / (ASpacingScaleDenominator / 100)) *
    TextRenderingPrecision;
  AStringLength := Length(AStringData);
  SetLength(ASpacing, AStringLength);
  for I := 0 to AStringLength - 1 do
  begin
    AAdvance := AAdvance + AOffsets[I] * ASpacingScale;
    ANext := Round(AAdvance + ADiff) - APreviousX;
    ASpacing[I] := ANext;
    Inc(APreviousX, ANext);
  end;
  DrawString(AStringData, ASpacing, AXForm);
end;

procedure TdxPDFRenderingInterpreter.DrawString(const AStringData
  : TdxPDFStringData);
begin
  if TextState.RenderingMode <> trmInvisible then
  begin
    if not IsType3Font then
      DrawString(AStringData.Str, AStringData.Advances)
    else
    begin
      DoEndText;
      DrawType3FontString(AStringData);
    end;
  end;
end;

procedure TdxPDFRenderingInterpreter.DoDrawShading
  (AShading: TdxPDFCustomShading);
var
  ABitmap: TcxBitmap32;
  AMatrix: TdxPDFTransformationMatrix;
  APoints: TdxPointsF;
  ABoundingBox: TdxPDFRectangle;
begin
  ABoundingBox := AShading.BoundingBox;
  if ABoundingBox.IsNull then
  begin
    PerformRendering(dxRectF(BoundsOffset.X, BoundsOffset.Y, ActualSize.cx,
      ActualSize.cy), procedure
      begin
        TdxPDFShadingPainter.Draw(Graphics, Self, AShading);
      end);
  end
  else
  begin
    AMatrix := TdxPDFTransformationMatrix.Multiply
      (GraphicsState.TransformMatrix, DeviceTransformMatrix);
    SetLength(APoints, 4);
    APoints[0] := ABoundingBox.TopLeft;
    APoints[1] := ABoundingBox.BottomLeft;
    APoints[2] := ABoundingBox.BottomRight;
    APoints[3] := ABoundingBox.TopRight;
    PerformRendering(CreateBoundingRectangle(AMatrix.TransformPoints(APoints)),
      procedure
      begin
        ABitmap := TdxPDFShadingPainter.CreateBitmap(Self, AShading);
        if ABitmap <> nil then
          try
            DrawBitmap(ABitmap, ABoundingBox);
          finally
            ABitmap.Free;
          end;
      end);
  end;
end;

procedure TdxPDFRenderingInterpreter.DrawShading(AShading: TdxPDFCustomShading);
begin
  PerformGraphicsOperation(procedure
    begin
      if not IsCanceled then
        DoDrawShading(AShading);
    end);
end;

procedure TdxPDFRenderingInterpreter.DrawTransparencyGroup
  (AForm: TdxPDFGroupForm);
var
  AFormBoundingBox, ASoftMaskBoundingBox, AIntersection: TdxPDFRectangle;
  ABitmap, ASoftMaskBitmap: TdxSmartImage;
  ASoftMask: TdxPDFCustomSoftMask;
  ATransparencyGroup: TdxPDFGroupForm;
begin
  if IsPrinting then
    inherited DrawTransparencyGroup(AForm)
  else
  begin
    SaveGraphicsState;
    try
      UpdateTransformationMatrix(AForm.Matrix);
      AFormBoundingBox := TrimBoundingBox(AForm.BBox);
      if (AFormBoundingBox.Width <> 0) and (AFormBoundingBox.Height <> 0) then
      begin
        ABitmap := CreateTransparencyGroupBitmap(AForm, AFormBoundingBox,
          TdxPDFRectangle.Null);
        try
          ASoftMask := GraphicsState.Parameters.SoftMask;
          if (ASoftMask = nil) or (ASoftMask <> nil) and
            (ASoftMask is TdxPDFEmptySoftMask) then
            TdxPDFBackdropImageBlender.Blend(Self, AFormBoundingBox, ABitmap)
          else
          begin
            ATransparencyGroup := TdxPDFCustomSoftMaskAccess(ASoftMask)
              .TransparencyGroup;
            ASoftMaskBoundingBox := TrimBoundingBox(ATransparencyGroup.BBox);
            if not ASoftMaskBoundingBox.IsNull then
            begin
              AIntersection := ASoftMaskBoundingBox.Trim(AFormBoundingBox);
              if AIntersection.IsNull then
                Exit;
              ASoftMaskBitmap := CreateTransparencyGroupBitmap
                (ATransparencyGroup, AFormBoundingBox, AIntersection);
              try
                ApplySoftMask(ASoftMask, ABitmap, ASoftMaskBitmap);
              finally
                ASoftMaskBitmap.Free;
              end;
            end;
          end;
          DrawBitmap(ABitmap, AFormBoundingBox);
        finally
          ABitmap.Free;
        end;
      end;
    finally
      RestoreGraphicsState;
    end;
  end;
end;

function TdxPDFRenderingInterpreter.CreateGraphics: TdxGPCanvas;
begin
  if GetRenderParameters.Canvas <> nil then
  begin
    GetRenderParameters.Canvas.Lock;
    Result := dxGpBeginPaint(GetRenderParameters.Canvas.Handle,
      GetRenderParameters.Rect);
  end
  else
    Result := nil;
end;

function TdxPDFRenderingInterpreter.IsPrinting: Boolean;
begin
  Result := False;
end;

procedure TdxPDFRenderingInterpreter.DestroyGraphics;
begin
  if GetRenderParameters.Canvas <> nil then
  begin
    GetRenderParameters.Canvas.Unlock;
    (FGraphics as TdxGPCustomPaintCanvas).EndPaint;
    FreeAndNil(FGraphics);
  end;
end;

procedure TdxPDFRenderingInterpreter.InitializeGraphics;
begin
  if Graphics <> nil then
  begin
    GdipCheck(GdipSetPageUnit(Graphics.Handle, guPixel));
    SetSmoothingMode(smAntiAlias);
    GdipCheck(GdipSetPixelOffsetMode(Graphics.Handle,
      TdxGpPixelOffsetMode.PixelOffsetModeHighQuality));
    Graphics.InterpolationMode := imHighQualityBicubic;
  end;
end;

class function TdxPDFRenderingInterpreter.UpdateWorldTransform
  (AGraphics: TdxGPCanvas; ATransform: TdxGPMatrix): Boolean;
begin
  Result := (AGraphics <> nil) and (ATransform <> nil);
  if Result then
    Result := GdipSetWorldTransform(AGraphics.Handle, ATransform.Handle) = Ok;
end;

function TdxPDFRenderingInterpreter.GetBackdropBitmap(const ABoundingBox
  : TdxPDFRectangle; ABitmapWidth, ABitmapHeight: Integer): TdxSmartImage;
var
  AActualTransform: TdxGPMatrix;
  ABackdropGraphics: TdxGPCanvas;
  ABoundingBoxWidth, ABoundingBoxHeight: Single;
  ADestinationPoints: TIntegerDynArray;
  AMatrix: TdxPDFTransformationMatrix;
  AMinX, AMaxX, AMinY, AMaxY: Double;
  APoint: TdxPointF;
  APoints: TdxPDFPoints;
  ATransformationMatrix: TdxPDFTransformationMatrix;
  X, Y: Integer;
begin
  GdipCheck(GdipFlush(FGraphics.Handle,
    TdxGpFlushIntention.FlushIntentionFlush));
  ATransformationMatrix := GraphicsState.TransformMatrix;
  AMatrix := GetImageMatrix
    (ToCanvasPoint(ATransformationMatrix.Transform(ABoundingBox.TopLeft)));

  ABoundingBoxWidth := ABoundingBox.Width;
  ABoundingBoxHeight := Abs(ABoundingBox.Height);
  SetLength(APoints, 4);
  APoints[1] := dxPointF(ABoundingBoxWidth, 0);
  APoints[2] := dxPointF(0, ABoundingBoxHeight);
  APoints[3] := dxPointF(ABoundingBoxWidth, ABoundingBoxHeight);

  APoints := AMatrix.TransformPoints(APoints);
  AMinX := MaxDouble;
  AMaxX := MinDouble;
  AMinY := MaxDouble;
  AMaxY := MinDouble;
  for APoint in APoints do
  begin
    AMinX := TdxPDFUtils.Min(AMinX, APoint.X);
    AMaxX := TdxPDFUtils.Max(AMaxX, APoint.X);
    AMinY := TdxPDFUtils.Min(AMinY, APoint.Y);
    AMaxY := TdxPDFUtils.Max(AMaxY, APoint.Y);
  end;
  X := Trunc(AMinX);
  Y := Trunc(AMinY);
  Result := ExtractBackdropBitmap(X, Y, ABitmapWidth, ABitmapHeight);

  ABackdropGraphics := Result.CreateCanvas;
  try
    ABackdropGraphics.PixelOffsetMode :=
      TdxGpPixelOffsetMode.PixelOffsetModeHalf;
    if IsNotRotated then
      ABackdropGraphics.InterpolationMode := imNearestNeighbor;

    SetLength(ADestinationPoints, 6);
    ADestinationPoints[2] := ABitmapWidth;
    ADestinationPoints[5] := ABitmapHeight;

    AActualTransform := TdxPDFBackdropMatrixCalculator.
      CalculateTransformationMatrix(X, Y, APoints, ADestinationPoints);
    try
      ABackdropGraphics.SetWorldTransform(AActualTransform);
      ABackdropGraphics.Draw(Result, Result.ClientRect);
    finally
      AActualTransform.Free;
    end;
  finally
    ABackdropGraphics.Free;
  end;
end;

function TdxPDFRenderingInterpreter.GetRenderParameters: TdxPDFRenderParameters;
begin
  Result := FParameters as TdxPDFRenderParameters;
end;

procedure TdxPDFRenderingInterpreter.DrawTilingCell
  (APattern: TdxPDFTilingPattern; const AColor: TdxPDFColor;
AParameters: TdxPDFExportParameters);
begin
  FParameters := AParameters;
  if not IsCanceled then
  begin
    Initialize;
    SaveGraphicsState;
    try
      UpdateTransformationMatrix(APattern.CreateMatrix(Trunc(Abs(Bounds.Width)),
        Trunc(Abs(Bounds.Height))));
      if not APattern.Colored then
        SetColorForNonStrokingOperations(TdxPDFColor.Create(AColor.Components));
      ExecuteCommand(APattern.Commands);
    finally
      RestoreGraphicsState;
      Finalize;
    end;
  end;
end;

function TdxPDFRenderingInterpreter.CalculateLineWidth(const AStartPoint,
  AEndPoint: TdxPointF): Single;
var
  AAngle: Double;
begin
  Result := GraphicsState.Parameters.LineWidth;
  if Result < dxPDFMinDisplayedLineWidth then
  begin
    AAngle := CalculateRotationAngle(AStartPoint, AEndPoint);
    Result := 1 / (CalculateVectorLength(Cos(AAngle), Sin(AAngle)) *
      FParameters.ScaleFactor);
  end;
end;

procedure TdxPDFRenderingInterpreter.SetGraphics(const AValue: TdxGPCanvas);
begin
  if FGraphics <> AValue then
  begin
    FGraphics := AValue;
    InitializeGraphics;
    InitializeClipRegion;
    UpdateClip;
    UpdateBrushes;
  end;
end;

function TdxPDFRenderingInterpreter.CalculateBitmapTransformationMatrix
  (ABitmapWidth, ABitmapHeight: Integer; const ABoundingBox: TdxRectF)
  : TdxPDFTransformationMatrix;
var
  AFactor: TdxPointF;
begin
  AFactor.X := ABitmapWidth / ABoundingBox.Width;
  AFactor.Y := ABitmapHeight / Abs(ABoundingBox.Height);
  Result := TdxPDFTransformationMatrix.Create(AFactor.X, 0, 0, AFactor.Y,
    -ABoundingBox.Left * AFactor.X, -ABoundingBox.Bottom * AFactor.Y);
end;

function TdxPDFRenderingInterpreter.CalculateLineExtendFactor(const AStartPoint,
  AEndPoint: TdxPointF): Single;
var
  AValue, AAngle, ASin, ACos: Single;
begin
  AAngle := CalculateRotationAngle(AStartPoint, AEndPoint);
  ASin := Sin(AAngle);
  ACos := Cos(AAngle);
  Result := CalculateVectorLength(ASin, ACos);
  AValue := CalculateVectorLength(ACos, ASin);
  if AValue <> 0 then
    Result := Result / AValue;
end;

function TdxPDFRenderingInterpreter.CalculatePenWidth(const AStartPoint,
  AEndPoint: TdxPointF): Single;
var
  AAngle: Double;
begin
  AAngle := CalculateRotationAngle(AStartPoint, AEndPoint);
  if GraphicsState.Parameters.LineWidth < dxPDFMinDisplayedLineWidth then
    Result := 1
  else
    Result := GraphicsState.Parameters.LineWidth * CalculateVectorLength
      (Cos(AAngle), Sin(AAngle)) * FParameters.ScaleFactor;
end;

function TdxPDFRenderingInterpreter.CalculateRotationAngle(const AStartPoint,
  AEndPoint: TdxPointF): Single;
begin
  Result := TdxPDFUtils.NormalizeAngle(CalculateAngle(AStartPoint, AEndPoint) +
    RotationAngle);
end;

function TdxPDFRenderingInterpreter.CalculateVectorLength(X, Y: Single): Single;
var
  AMatrix: TdxPDFTransformationMatrix;
begin
  AMatrix := TdxPDFTransformationMatrix.Create(TransformMatrix.A,
    TransformMatrix.B, TransformMatrix.C, TransformMatrix.D, 0, 0);
  Result := VectorLength(AMatrix, X, Y);
end;

function TdxPDFRenderingInterpreter.Clone(ABrush: TdxGPBrush): TdxGPBrush;
begin
  if ABrush <> nil then
  begin
    if ABrush.Style = gpbsSolid then
      Result := ABrush
    else
    begin
      Result := TdxGPBrush.Create;
      Result.Assign(ABrush);
    end;
  end
  else
    Result := TdxGPBrush.Create;
end;

function TdxPDFRenderingInterpreter.Clone(ARegion: TdxGPRegion): TdxGPRegion;
begin
  Result := TdxGPRegion.CreateFromRegion(ARegion.Handle);
end;

function TdxPDFRenderingInterpreter.CreateBoundingRectangle
  (const APoints: TdxPDFPoints): TdxRectF;
begin
  Result := CreateBoundingRectangle(TdxPDFRectangle.CreateBoundingRectangle
    (APoints));
end;

function TdxPDFRenderingInterpreter.CreateBoundingRectangle
  (const R: TdxPDFRectangle): TdxRectF;
begin
  Result := TdxRectF.Create(R.Left, R.Bottom, R.Right, R.Top);
end;

function TdxPDFRenderingInterpreter.CreateGraphicsDeviceParameters
  (ACanvas: TCanvas; const ASize: TSize): TdxPDFRenderParameters;
begin
  Result := TdxPDFRenderParameters.Create(DocumentState);
  Result.Canvas := ACanvas;
  Result.Angle := ra0;
  Result.Bounds := dxRectF(0, ASize.cy, ASize.cx, 0);
  Result.Position := cxRectAdjustF(Result.Bounds).TopLeft;
  Result.ScaleFactor := 1;
  Result.CancelCallback := FParameters.CancelCallback;
end;

function TdxPDFRenderingInterpreter.CreateTransparencyGroupBitmap
  (AForm: TdxPDFGroupForm; const ABoundingBox, AClipRect: TdxPDFRectangle)
  : TdxSmartImage;
var
  ASize: TdxSizeF;
  P: TPoint;
begin
  ASize := TransformSize(GraphicsState.TransformMatrix, ABoundingBox);
  P.X := Round(ASize.cx);
  P.Y := Round(ASize.cy);
  Result := CreateTransparencyGroupBitmap(AForm, cxSize(P.X, P.Y),
    CalculateBitmapTransformationMatrix(P.X, P.Y, ABoundingBox.ToRectF),
    AClipRect);
end;

function TdxPDFRenderingInterpreter.CreateTransparencyGroupBitmap
  (AForm: TdxPDFGroupForm; const ABitmapSize: TSize;
const ATransformationMatrix: TdxPDFTransformationMatrix;
const AClipRect: TdxPDFRectangle): TdxSmartImage;
var
  ACanvas: TdxGPCanvas;
  ADevice: TdxPDFGraphicsDevice;
  AParameters: TdxPDFRenderParameters;
begin
  Result := TdxSmartImage.CreateSize(ABitmapSize.cx, ABitmapSize.cy);
  ACanvas := Result.CreateCanvas;
  AParameters := CreateGraphicsDeviceParameters(nil, ABitmapSize);
  ADevice := TdxPDFGraphicsDevice.Create;
  try
    ADevice.FParameters := AParameters;
    ADevice.Initialize;
    ADevice.Graphics := ACanvas;
    ADevice.FontQuality := NONANTIALIASED_QUALITY;
    ADevice.UpdateTransformationMatrix(ATransformationMatrix);
    ADevice.UpdateTilingTransformationMatrix;
    if not AClipRect.IsNull then
      ClipRectangle(AClipRect);
    ADevice.Graphics.Clear(clNone);
    if AForm <> nil then
      ADevice.ExecuteCommand(AForm.Commands);
  finally
    ADevice.AfterExport;
    ADevice.Free;
    AParameters.Free;
    ACanvas.Free;
  end;
end;

function TdxPDFRenderingInterpreter.ExtractBackdropBitmap(X, Y, AWidth,
  AHeight: Integer): TdxSmartImage;
var
  ABackdropDC, AGraphicsHDC: HDC;
  ABackdropCanvas: TdxGPCanvas;
begin
  Result := TdxSmartImage.CreateSize(AWidth, AHeight);
  AGraphicsHDC := Graphics.GetHDC;
  ABackdropCanvas := Result.CreateCanvas;
  ABackdropDC := ABackdropCanvas.GetHDC;
  try
    cxBitBlt(ABackdropDC, AGraphicsHDC, cxRect(0, 0, AWidth, AHeight),
      cxPoint(X, Y), SRCCOPY);
  finally
    ABackdropCanvas.ReleaseHDC(ABackdropDC);
    ABackdropCanvas.Free;
    Graphics.ReleaseHDC(AGraphicsHDC);
  end;
end;

function TdxPDFRenderingInterpreter.GetFillMode(AUseNonzeroWindingRule: Boolean)
  : TdxGPFillMode;
begin
  if AUseNonzeroWindingRule then
    Result := gpfmWinding
  else
    Result := gpfmAlternate;
end;

function TdxPDFRenderingInterpreter.IsTextRendering: Boolean;
begin
  Result := FHDC <> 0;
end;

function TdxPDFRenderingInterpreter.GetActualFontName(const ASourceFontName
  : string): string;
begin
  if not FActualFontNameCache.TryGetValue(ASourceFontName, Result) then
  begin
    Result := dxPDFSystemFontList.Values[ASourceFontName];
    if Result = '' then
      Result := ASourceFontName;
    FActualFontNameCache.Add(ASourceFontName, Result);
  end;
end;

function TdxPDFRenderingInterpreter.GetImageDataStorage
  : TdxPDFDocumentImageDataStorage;
begin
  Result := FParameters.DocumentState.ImageDataStorage;
end;

function TdxPDFRenderingInterpreter.ConvertPalette(AIsMask: Boolean;
const AImagePalette: TdxPDFARGBColorArray): TdxAlphaColorDynArray;
begin
  Result := ConvertPalette(AIsMask, AImagePalette,
    TdxPDFUtils.ConvertToAlphaColor(GraphicsState.NonStrokingColor,
    GraphicsState.Parameters.NonStrokingColorAlpha));
end;

class function TdxPDFRenderingInterpreter.ConvertPalette(AIsMask: Boolean;
const AImagePalette: TdxPDFARGBColorArray; ACurrentColor: TdxAlphaColor)
  : TdxAlphaColorDynArray;
var
  AColor: TdxPDFARGBColor;
  AStartAlpha, I, ALength: Integer;
  AStep: Double;
begin
  if AIsMask then
  begin
    SetLength(Result, 256);
    AStartAlpha := dxGetAlpha(ACurrentColor);
    AStep := AStartAlpha / 255.0;
    Result[0] := ACurrentColor;
    for I := 1 to 255 - 1 do
      Result[I] := TdxAlphaColors.FromArgb(Round(AStartAlpha - I * AStep),
        ACurrentColor);
    Result[255] := TdxAlphaColors.FromArgb(0, 0, 0, 0);
  end
  else if AImagePalette <> nil then
  begin
    ALength := Length(AImagePalette);
    SetLength(Result, ALength);
    for I := 0 to ALength - 1 do
    begin
      AColor := AImagePalette[I];
      Result[I] := TdxAlphaColors.FromArgb(AColor.Alpha, Trunc(AColor.Red),
        Trunc(AColor.Green), Trunc(AColor.Blue));
    end;
  end
  else
    SetLength(Result, 0);
end;

function TdxPDFRenderingInterpreter.GetImageData(AImage: TdxPDFDocumentImage;
AWidth, AHeight: Integer): TdxPDFImageCacheItem;
begin
  Result := ImageDataStorage.GetImage(AImage,
    TdxPDFImageParameters.Create(AWidth, AHeight));
end;

function TdxPDFRenderingInterpreter.GetImageMatrix(const ALocation: TdxPointF)
  : TdxPDFTransformationMatrix;
var
  AScale: Double;
begin
  AScale := FParameters.ScaleFactor;
  Result := TdxPDFTransformationMatrix.Create(GraphicsState.TransformMatrix.A *
    AScale, -GraphicsState.TransformMatrix.B * AScale,
    -GraphicsState.TransformMatrix.C * AScale, GraphicsState.TransformMatrix.D *
    AScale, ALocation.X, ALocation.Y);
end;

function TdxPDFRenderingInterpreter.GetNonStrokingColorAlpha: Word;
begin
  Result := TdxPDFUtils.ConvertToByte
    (GraphicsState.Parameters.NonStrokingColorAlpha * 255);
end;

function TdxPDFRenderingInterpreter.GetFontDataStorage: TdxPDFFontDataStorage;
begin
  Result := DocumentState.FontDataStorage;
end;

function TdxPDFRenderingInterpreter.NeedExtendingLineSize: Boolean;
begin
  Result := GraphicsState.Parameters.LineCapStyle
    in [lcsProjectingSquare, lcsRound];
end;

function TdxPDFRenderingInterpreter.SupportCurrentBlendMode: Boolean;
begin
  Result := TdxPDFBackdropImageBlender.Supports
    (GraphicsState.Parameters.BlendMode);
end;

function TdxPDFRenderingInterpreter.TrimBoundingBox(const ABoundingBox
  : TdxPDFRectangle): TdxPDFRectangle;
var
  AMatrix: TdxPDFTransformationMatrix;
begin
  AMatrix := GraphicsState.TransformMatrix;
  if AMatrix.IsInvertable then
    Result := ABoundingBox.Trim(TdxPDFRectangle.CreateBoundingRectangle
      (TdxPDFTransformationMatrix.Invert(AMatrix)
      .TransformPoints(TransformedBoundingBox)))
  else
    Result := ABoundingBox;
end;

procedure TdxPDFRenderingInterpreter.ApplySoftMask
  (ASoftMask: TdxPDFCustomSoftMask; ABitmap: TdxSmartImage;
const AOffset: TdxPointF; AHeight: Integer);
var
  AMaskBitmap: TdxSmartImage;
  ATransform, AMaskTransformation: TdxPDFTransformationMatrix;
begin
  ATransform := TdxPDFTransformationMatrix.Multiply
    (GraphicsState.SoftMaskTransformMatrix, DeviceTransformMatrix);
  AMaskTransformation := TdxPDFTransformationMatrix.Multiply
    (TdxPDFTransformationMatrix.Translate(ATransform, AOffset),
    TdxPDFTransformationMatrix.Create(1, 0, 0, -1, 0, AHeight));
  AMaskBitmap := CreateTransparencyGroupBitmap(ASoftMask.TransparencyGroup,
    ABitmap.Size, AMaskTransformation, TdxPDFRectangle.Null);
  try
    ApplySoftMask(ASoftMask, ABitmap, AMaskBitmap);
  finally
    AMaskBitmap.Free;
  end;
end;

procedure TdxPDFRenderingInterpreter.ApplySoftMask
  (ASoftMask: TdxPDFCustomSoftMask; ABitmap, ASoftMaskBitmap: TdxSmartImage);
begin
  if ASoftMask is TdxPDFLuminositySoftMask then
    TdxPDFLuminosityMaskBlender.Blend
      (ASoftMask.TransparencyFunction as TdxPDFCustomFunction, ABitmap,
      ASoftMaskBitmap)
  else
    TdxPDFAlphaMaskBlender.Blend
      (ASoftMask.TransparencyFunction as TdxPDFCustomFunction, ABitmap,
      ASoftMaskBitmap);
end;

procedure TdxPDFRenderingInterpreter.ApplySemitransparentText;
var
  AImageAttributes: TdxGPImageAttributes;
  ARect: TdxRectF;
begin
  if FSemitransparentTextBitmap <> nil then
  begin
    FFontQuality := FPrevFontQuality;
    FGraphics.Free;
    FGraphics := FPrevGraphics;
    FPrevGraphics := nil;
    ARect := cxRectOffset(dxRectF(0, 0, ActualSize.cx, ActualSize.cy),
      BoundsOffset);
    AImageAttributes := CreateImageAttributes;
    try
      Graphics.Draw(FSemitransparentTextBitmap, ARect, ARect, AImageAttributes);
    finally
      AImageAttributes.Free;
    end;
    FreeAndNil(FSemitransparentTextBitmap);
  end;
  FPrevAlpha := 255;
end;

procedure TdxPDFRenderingInterpreter.BlendWithBackground(ABitmap: TdxSmartImage;
const ABounds: TRect);
var
  ABackdropBitmap: TdxSmartImage;
begin
  ABackdropBitmap := ExtractBackdropBitmap(ABounds.Left, ABounds.Top,
    ABounds.Width, ABounds.Height);
  try
    TdxPDFBackdropImageBlender.Blend(GraphicsState.Parameters.BlendMode,
      ABitmap, ABackdropBitmap);
  finally
    ABackdropBitmap.Free;
  end;
end;

procedure TdxPDFRenderingInterpreter.CalculatePenLineStyle(const AStartPoint,
  AEndPoint: TdxPointF; APen: TdxGPPen);
begin
  APen.Width := CalculatePenWidth(AStartPoint, AEndPoint);
  UpdatePenDashPattern(APen, CalculateLineWidth(AStartPoint, AEndPoint));
  APen.MiterLimit := GraphicsState.Parameters.MiterLimit;
end;

procedure TdxPDFRenderingInterpreter.DestroyBrush(var ABrush: TdxGPBrush);
begin
  if (ABrush <> nil) and not SolidBrushCache.ContainsValue(ABrush) then
    FreeAndNil(ABrush);
end;

procedure TdxPDFRenderingInterpreter.DoBeginText;
begin
  if IsTextRendering then
    Exit;
  FPrevClipRegion := FClipRegion.GetGdiRegionHandle(Graphics);
  FHDC := Graphics.GetHDC;
  UpdateCurrentFont;
  SelectClipRgn(FHDC, FPrevClipRegion);
  FPrevGraphicsMode := SetGraphicsMode(FHDC, GM_ADVANCED);
  FPrevTextAlign := SetTextAlign(FHDC, TA_BASELINE);
  FPrevBkMode := SetBkMode(FHDC, TRANSPARENT);
  FPrevTextColor := UpdateTextColor;
  FPrevMappingMode := SetMapMode(FHDC, MM_ANISOTROPIC);
  SetWindowExtEx(FHDC, ActualSize.cx * TextRenderingPrecision,
    ActualSize.cy * TextRenderingPrecision, @FPrevWindowSize);
  SetViewportExtEx(FHDC, ActualSize.cx, ActualSize.cy, @FPrevViewportSize);
end;

procedure TdxPDFRenderingInterpreter.DoEndText;
begin
  if IsTextRendering then
  begin
    if FPrevTextFont <> 0 then
    begin
      DeleteObject(SelectObject(FHDC, FPrevTextFont));
      FPrevTextFont := 0;
    end;
    SelectClipRgn(FHDC, 0);
    SetTextAlign(FHDC, FPrevTextAlign);
    SetBkMode(FHDC, FPrevBkMode);
    SetTextColor(FHDC, FPrevTextColor);
    SetGraphicsMode(FHDC, FPrevGraphicsMode);
    DeleteObject(FPrevClipRegion);
    SetMapMode(FHDC, FPrevMappingMode);
    SetWindowExtEx(FHDC, FPrevWindowSize.cx, FPrevWindowSize.cy,
      @FPrevWindowSize);
    SetViewportExtEx(FHDC, FPrevViewportSize.cx, FPrevViewportSize.cy,
      @FPrevViewportSize);
    Graphics.ReleaseHDC(FHDC);
    FHDC := 0;
  end;
end;

procedure TdxPDFRenderingInterpreter.DrawType3FontString(const AStringData
  : TdxPDFStringData);
var
  ACharProcs: TDictionary<string, TdxPDFCommandList>;
  ACommands: TdxPDFCommandList;
  ACurrentTransformationMatrix: TdxPDFTransformationMatrix;
  ADelta: TdxPointF;
  AEncoding: TdxPDFSimpleFontEncoding;
  AFont: TdxPDFType3Font;
  AGlyphAdvances: TDoubleDynArray;
  AInitialTransformationMatrix: TdxPDFTransformationMatrix;
  ALength, I: Integer;
  AOffset: Double;
  ATextLineMatrix: TdxPDFTransformationMatrix;
  ATextMatrix: TdxPDFTransformationMatrix;
  AToUserSpaceMatrix: TdxPDFTransformationMatrix;
begin
  DoEndText;
  if not IsCanceled then
  begin
    AGlyphAdvances := AStringData.Advances;
    AFont := TextState.Font as TdxPDFType3Font;
    ACurrentTransformationMatrix := TdxPDFTransformationMatrix.Create
      (GraphicsState.TransformMatrix);
    ATextMatrix := TdxPDFTransformationMatrix.Create(TextState.TextMatrix);
    ATextLineMatrix := TdxPDFTransformationMatrix.Create
      (TextState.TextLineMatrix);
    AInitialTransformationMatrix := TdxPDFTransformationMatrix.Create;
    AInitialTransformationMatrix.Assign(TextState.TextSpaceMatrix);
    AInitialTransformationMatrix.Multiply(AFont.Matrix);
    AInitialTransformationMatrix.Multiply(TextState.TextMatrix, moAppend);
    AInitialTransformationMatrix.Multiply(ACurrentTransformationMatrix,
      moAppend);
    GraphicsState.TransformMatrix.Assign(AInitialTransformationMatrix);
    AToUserSpaceMatrix := TdxPDFTransformationMatrix.Multiply
      (TextState.TextMatrix, ACurrentTransformationMatrix);
    AToUserSpaceMatrix := TdxPDFTransformationMatrix.Create
      (AToUserSpaceMatrix.A, AToUserSpaceMatrix.B, AToUserSpaceMatrix.C,
      AToUserSpaceMatrix.D, 0, 0);
    AOffset := 0;
    ACharProcs := AFont.CharProcs;
    AEncoding := AFont.Encoding as TdxPDFSimpleFontEncoding;
    try
      ALength := Length(AStringData.Str);
      for I := 0 to ALength - 1 do
      begin
        if not IsCanceled then
        begin
          ACommands := nil;
          if ACharProcs.TryGetValue
            (AEncoding.GetGlyphName(Byte(AStringData.Str[I])), ACommands) and
            (ACommands <> nil) then
          begin
            SaveGraphicsState;
            try
              ExecuteCommand(ACommands);
            finally
              RestoreGraphicsState;
            end;
          end;
          AOffset := AOffset + AGlyphAdvances[I];
          ADelta := AToUserSpaceMatrix.Transform(dxPointF(AOffset, 0));
          GraphicsState.TransformMatrix := TdxPDFTransformationMatrix.Translate
            (AInitialTransformationMatrix, dxPointF(ADelta.X, ADelta.Y));
        end;
      end;
    finally
      TextState.TextMatrix.Assign(ATextMatrix);
      TextState.TextLineMatrix.Assign(ATextLineMatrix);
      GraphicsState.TransformMatrix.Assign(ACurrentTransformationMatrix);
      DoBeginText;
    end;
  end;
end;

procedure TdxPDFRenderingInterpreter.DrawBitmap(ABitmap: TdxSmartImage;
const ABoundingBox: TdxPDFRectangle);
var
  ABox: TdxPDFRectangle;
begin
  ABox := ABoundingBox;
  PerformGraphicsOperation(procedure
    var
      AAttributes: TdxGPImageAttributes;
      AMatrix: TdxGPMatrix;
      APoints: TdxPointsF;
      ASavedInterpolationMode: TdxGPInterpolationMode;
      AWorldTransform: TdxGPMatrix;
    begin
      ASavedInterpolationMode := Graphics.InterpolationMode;
      Graphics.SaveWorldTransform;
      AWorldTransform := Graphics.GetWorldTransform;
      try
        AMatrix := TdxPDFUtils.ConvertToGpMatrix
          (GetImageMatrix(ToCanvasPoint(GraphicsState.TransformMatrix.Transform
          (ABox.TopLeft))));
        try
          AWorldTransform.Multiply(AMatrix, MatrixOrderAppend);
          Graphics.SetWorldTransform(AWorldTransform);
        finally
          AMatrix.Free;
        end;
        if IsNotRotated then
          Graphics.InterpolationMode := imNearestNeighbor;
        SetLength(APoints, 3);
        APoints[0] := dxPointF(0, 0);
        APoints[1] := dxPointF(ABox.Width, 0);
        APoints[2] := dxPointF(0, ABox.Height);
        if GraphicsState.Parameters.NonStrokingColorAlpha = 1 then
          DrawBitmap(ABitmap.Handle, ABitmap.Size, APoints, nil)
        else
        begin
          AAttributes := CreateImageAttributes;
          try
            DrawBitmap(ABitmap.Handle, ABitmap.Size, APoints, AAttributes);
          finally
            AAttributes.Free;
          end;
        end;
      finally
        Graphics.InterpolationMode := ASavedInterpolationMode;
        Graphics.RestoreWorldTransform;
        AWorldTransform.Free;
      end;
    end);
end;

procedure TdxPDFRenderingInterpreter.DrawBitmap(ABitmap: TcxBitmap32;
const ABoundingBox: TdxPDFRectangle);
var
  AImage: TdxSmartImage;
begin
  AImage := TdxSmartImage.CreateFromBitmap(ABitmap);
  try
    DrawBitmap(AImage, ABoundingBox);
  finally
    AImage.Free;
  end;
end;

procedure TdxPDFRenderingInterpreter.DrawBitmap(ABitmap: GpBitmap;
const ASize: TSize; const APoints: TdxPointsF;
AAttributes: TdxGPImageAttributes);
begin
  if AAttributes = nil then
    GdipCheck(GdipDrawImagePoints(Graphics.Handle, ABitmap, @APoints[0],
      Length(APoints)))
  else
    GdipCheck(GdipDrawImagePointsRect(Graphics.Handle, ABitmap, @APoints[0],
      Length(APoints), 0, 0, ASize.cx, ASize.cy, guPixel,
      TdxGpImageAttributesAccess(AAttributes).Handle, nil, nil));
end;

procedure TdxPDFRenderingInterpreter.DrawTilingCell(ABitmap: TcxBitmap32;
APattern: TdxPDFTilingPattern; const ASize: TSize; const AColor: TdxPDFColor);
var
  ADevice: TdxPDFGraphicsDevice;
  AParameters: TdxPDFRenderParameters;
begin
  if ABitmap <> nil then
  begin
    AParameters := CreateGraphicsDeviceParameters(ABitmap.Canvas, ASize);
    ADevice := TdxPDFGraphicsDevice.Create;
    try
      ADevice.DrawTilingCell(APattern, AColor, AParameters);
    finally
      ADevice.Free;
      AParameters.Free;
    end;
  end;
end;

procedure TdxPDFRenderingInterpreter.InitializeClipRegion;
var
  ARegion, ARegion2: GpRegion;
begin
  if Graphics <> nil then
  begin
    GdipCheck(GdipCreateRegion(ARegion));
    GdipCheck(GdipGetClip(Graphics.Handle, ARegion));
    GdipCheck(GdipCloneRegion(ARegion, ARegion2));
    FInitialClipRegion := TdxGPRegion.CreateFromRegion(ARegion2);
  end;
end;

procedure TdxPDFRenderingInterpreter.InitializeSemitransparentText;
begin
  FPrevAlpha := GetNonStrokingColorAlpha;
  if FPrevAlpha <> 255 then
  begin
    FPrevGraphics := Graphics;
    FPrevFontQuality := FFontQuality;
    FSemitransparentTextBitmap := TdxSmartImage.CreateSize
      (Round(BoundsOffset.X + ActualSize.cx),
      Round(BoundsOffset.Y + ActualSize.cy));
    Graphics := FSemitransparentTextBitmap.CreateCanvas;
    FFontQuality := NONANTIALIASED_QUALITY;
  end;
end;

procedure TdxPDFRenderingInterpreter.PerformGraphicsOperation(AProc: TProc);
var
  AShouldResetTextDrawing: Boolean;
begin
  AShouldResetTextDrawing := IsTextRendering;
  if AShouldResetTextDrawing then
    DoEndText;
  try
    AProc;
  finally
    if AShouldResetTextDrawing then
      DoBeginText;
  end;
end;

procedure TdxPDFRenderingInterpreter.PerformRendering(const ABounds: TdxRectF;
AProc: TProc);
var
  R: TdxRectF;
begin
  R := ABounds;
  PerformGraphicsOperation(procedure
    var
      AActualBounds: TRect;
      ASoftMask: TdxPDFCustomSoftMask;
      AActualGraphics, ABitmapGraphics: TdxGPCanvas;
      ABitmap: TdxSmartImage;
      AUseSoftMask, ANeedBlended: Boolean;
    begin
      if IsCanceled then
        Exit;
      AUseSoftMask := (GraphicsState.Parameters.SoftMask <> nil) and
        not(GraphicsState.Parameters.SoftMask is TdxPDFEmptySoftMask);
      ANeedBlended := SupportCurrentBlendMode;
      if ANeedBlended or AUseSoftMask then
      begin
        AActualBounds := cxRect(R);
        if (AActualBounds.Width = 0) or (AActualBounds.Height = 0) then
          Exit;
        ABitmap := TdxSmartImage.CreateSize(AActualBounds);
        ABitmapGraphics := ABitmap.CreateCanvas;
        try
          AActualGraphics := Graphics;
          ABitmapGraphics.TranslateWorldTransform(-AActualBounds.Left,
            -AActualBounds.Top);
          try
            FGraphics := ABitmapGraphics;
            if not IsCanceled then
              AProc
            else
              Exit;
          finally
            FGraphics := AActualGraphics;
          end;
          ASoftMask := GraphicsState.Parameters.SoftMask;
          if AUseSoftMask then
            ApplySoftMask(ASoftMask, ABitmap, dxPointF(-AActualBounds.Left,
              -AActualBounds.Top), AActualBounds.Height);
          if ANeedBlended and not IsCanceled then
            BlendWithBackground(ABitmap, AActualBounds);
          if not IsCanceled then
            GdipCheck(GdipDrawImageI(Graphics.Handle, ABitmap.Handle,
              AActualBounds.Left, AActualBounds.Top));
        finally
          ABitmapGraphics.Free;
          ABitmap.Free;
        end;
      end
      else
        AProc;
    end);
end;

procedure TdxPDFRenderingInterpreter.SetSmoothingMode
  (AMode: TdxGPSmoothingMode);
begin
  Graphics.SmoothingMode := AMode;
end;

procedure TdxPDFRenderingInterpreter.StrokeLine(const AStartPoint,
  AEndPoint: TdxPointF; APen: TdxGPPen);
var
  P1, P2: TdxPointF;
begin
  P1 := ToCanvasPoint(dxPointF(AStartPoint.X, AStartPoint.Y));
  P2 := ToCanvasPoint(dxPointF(AEndPoint.X, AEndPoint.Y));
  Graphics.Line(P1.X, P1.Y, P2.X, P2.Y, APen);
end;

procedure TdxPDFRenderingInterpreter.StrokePath(APath: TdxPDFGraphicsPath;
APen: TdxGPPen; APrevSmoothingMode: TdxGPSmoothingMode);
var
  ALineWidth, ALineExtendFactor: Single;
  AGPPath: TdxGPPath;
  AEndPoint: TdxPointF;
  ABounds: TdxRectF;
  AWholeSmoothingMode: TdxGPSmoothingMode;
begin
  if APath.Segments.Count > 0 then
  begin
    AEndPoint := APath.Segments[0].EndPoint;
    ALineWidth := 0;
    if NeedExtendingLineSize and
      not(APath.IsClosed or cxPointIsEqual(APath.StartPoint, APath.EndPoint))
    then
    begin
      ALineExtendFactor := CalculateLineExtendFactor(APath.StartPoint,
        AEndPoint);
      if ALineExtendFactor > 2 then
        ALineWidth := CalculateLineWidth(APath.StartPoint, AEndPoint) *
          ALineExtendFactor * 0.5
      else
        ALineWidth := 0;
    end
    else
      StrokeRectangle(APath, APen);
    CalculatePenLineStyle(APath.StartPoint, AEndPoint, APen);
    AGPPath := TdxPDFGraphicsPathBuilder.CreatePath(Self, APath, ALineWidth);
    AWholeSmoothingMode := Graphics.SmoothingMode;
    try
      if APen.Width < 1 then
        SetSmoothingMode(APrevSmoothingMode);
      if AGPPath.TryGetBoundsF(ABounds, APen) = Ok then
        PerformRendering(ABounds, procedure
          begin
            GdipDrawPath(Graphics.Handle, APen.Handle, AGPPath.Handle);
          end);
    finally
      SetSmoothingMode(AWholeSmoothingMode);
      AGPPath.Free;
    end;
  end;
end;

procedure TdxPDFRenderingInterpreter.StrokeRectangle(APath: TdxPDFGraphicsPath;
APen: TdxGPPen);
var
  R: TdxPDFRectangle;
  ABottomLeft, ABottomRight, ATopLeft: TdxPointF;
  ALeft, ARight, ABottom, ATop: Double;
  AExtend, AScaleFactor, ALineExtendFactor, AActualLeft, AActualRight,
    AActualBottom, AActualTop: Double;
  ALineCap: TdxGPPenLineCapStyle;
begin
  if APath is TdxPDFRectangularGraphicsPath then
  begin
    R := TdxPDFRectangularGraphicsPath(APath).Rectangle;
    ALeft := R.Left;
    ARight := R.Right;
    ABottom := R.Bottom;
    ABottomLeft := dxPointF(ALeft, ABottom);
    ABottomRight := dxPointF(ARight, ABottom);
    ALineExtendFactor := CalculateLineExtendFactor(ABottomLeft, ABottomRight);
    if (ALineExtendFactor <= 0.5) or (ALineExtendFactor >= 2) then
    begin
      ALineCap := APen.LineStartCapStyle;
      try
        AScaleFactor := FParameters.ScaleFactor * 2;
        APen.LineStartCapStyle := gpcsFlat;
        APen.LineEndCapStyle := gpcsFlat;
        ATop := R.Top;
        ATopLeft := dxPointF(ALeft, ATop);
        CalculatePenLineStyle(ABottomLeft, ABottomRight, APen);
        AExtend := CalculatePenWidth(ABottomLeft, ATopLeft) / AScaleFactor;
        AActualLeft := ALeft - AExtend;
        AActualRight := ARight + AExtend;
        StrokeLine(dxPointF(AActualLeft, ABottom),
          dxPointF(AActualRight, ABottom), APen);
        StrokeLine(dxPointF(AActualLeft, ATop),
          dxPointF(AActualRight, ATop), APen);
        CalculatePenLineStyle(ABottomLeft, ATopLeft, APen);
        AExtend := CalculatePenWidth(ABottomLeft, ABottomRight) / AScaleFactor;
        AActualBottom := ABottom - AExtend;
        AActualTop := R.Top + AExtend;
        StrokeLine(dxPointF(ALeft, AActualBottom),
          dxPointF(ALeft, AActualTop), APen);
        StrokeLine(dxPointF(ARight, AActualBottom),
          dxPointF(ARight, AActualTop), APen);
      finally
        APen.LineStartCapStyle := ALineCap;
        APen.LineEndCapStyle := ALineCap;
      end;
    end;
  end;
end;

procedure TdxPDFRenderingInterpreter.UpdateBrushes;
begin
  UpdateStrokingBrush;
  UpdateNonStrokingBrush;
end;

procedure TdxPDFRenderingInterpreter.UpdateClip;
begin
  if Graphics <> nil then
    PerformGraphicsOperation(procedure
      begin
        if FClipRegion <> nil then
          FClipRegion.Free;
        FClipRegion := TdxGPRegion.CreateFromRegion(FInitialClipRegion.Handle);
        FClipRegion.CombineRegionRect(PageClippingRect, gmIntersect);
        if FCurrentClipRegion <> nil then
          FClipRegion.CombineRegionRegion(FCurrentClipRegion, gmIntersect);
        Graphics.SetClipRegion(FClipRegion, gmReplace);
      end);
end;

procedure TdxPDFRenderingInterpreter.UpdateCurrentFont;

  procedure UpdateFont(const AFontName: string;
  const AData: TdxPDFFontRegistrationData);
  var
    AWeight: Integer;
    AHandle, APreviousFont: THandle;
  begin
    AWeight := IfThen(TextState.RenderingMode = TdxPDFTextRenderingMode.
      trmFillAndStroke, TdxPDFGDIFontSubstitutionHelper.BoldWeight,
      AData.Weight);
    AHandle := CreateFont(-TextRenderingPrecision, 0, 0, 0, AWeight,
      Cardinal(AData.Italic), 0, 0, DEFAULT_CHARSET, OUT_DEFAULT_PRECIS,
      CLIP_DEFAULT_PRECIS, FFontQuality, AData.PitchAndFamily,
      PChar(AFontName));
    if AHandle <> 0 then
    begin
      APreviousFont := SelectObject(FHDC, AHandle);
      if FPrevTextFont = 0 then
        FPrevTextFont := APreviousFont
      else
        DeleteObject(APreviousFont);
    end;
  end;

var
  AData: TdxPDFFontRegistrationData;
  ATextState: TdxPDFTextState;
begin
  ATextState := TextState;
  if IsTextRendering and (ATextState.Font <> nil) and not IsType3Font then
  begin
    AData := FontDataStorage.Add(TextStateFont);
    FUseEmbeddedFontEncoding := AData.UseEmbeddedEncoding;
    UpdateFont(GetActualFontName(AData.Name), AData);
  end;
end;

procedure TdxPDFRenderingInterpreter.UpdateNonStrokingBrush;
begin
  DestroyBrush(FNonStrokingBrush);
  FNonStrokingBrush := TdxPDFBrushHelper.CreateBrush(Self,
    GraphicsState.NonStrokingColor,
    GraphicsState.Parameters.NonStrokingColorAlpha);
  UpdateTextColor;
end;

procedure TdxPDFRenderingInterpreter.UpdatePenDashPattern(APen: TdxGPPen;
ALineWidth: Double);
var
  I, ALength: Integer;
  AData: TSingleDynArray;
  ADashPattern: TDoubleDynArray;
  V: Single;
begin
  if (GraphicsState.Parameters.LineStyle <> nil) and
    GraphicsState.Parameters.LineStyle.IsDashed then
  begin
    SetLength(ADashPattern, 0);
    TdxPDFUtils.AddData(GraphicsState.Parameters.LineStyle.Pattern,
      ADashPattern);
    ALength := Length(ADashPattern);
    if (ALength = 2) and (ADashPattern[0] = 0) then
      ADashPattern[1] := ADashPattern[1] - 1;
    SetLength(AData, 0);
    for I := 0 to ALength - 1 do
    begin
      V := ADashPattern[I] / ALineWidth;
      if V <= 0 then
      begin
        if GraphicsState.Parameters.LineCapStyle <> lcsButt then
          TdxPDFUtils.AddValue(1, AData);
      end
      else
        TdxPDFUtils.AddValue(V, AData);
    end;
    ALength := Length(AData);
    if ALength > 0 then
    begin
      if ALength = 1 then
        TdxPDFUtils.AddValue(AData[0], AData);
      GdipCheck(GdipSetPenDashOffset(APen.Handle,
        GraphicsState.Parameters.LineStyle.Phase));
      APen.SetDashArray(AData);
    end;
  end;
end;

procedure TdxPDFRenderingInterpreter.UpdateSmoothingMode(AIsFilling: Boolean);
var
  APath: TdxPDFGraphicsPath;
begin
  if UseRectangularGraphicsPath and
    (GraphicsState.Parameters.SmoothnessTolerance = 0) then
  begin
    for APath in Paths do
      if not APath.IsFlat(AIsFilling) then
        Exit;
    SetSmoothingMode(smNone);
  end;
end;

procedure TdxPDFRenderingInterpreter.UpdateStrokingBrush;
begin
  DestroyBrush(FStrokingBrush);
  FStrokingBrush := TdxPDFBrushHelper.CreateBrush(Self,
    GraphicsState.StrokingColor, GraphicsState.Parameters.StrokingColorAlpha);
end;

function TdxPDFRenderingInterpreter.UpdateTextColor: Cardinal;
var
  ATextColor: Integer;
  AColor: TdxPDFARGBColor;
begin
  AColor := TdxPDFARGBColor.Create(GraphicsState.NonStrokingColor);
  ATextColor := Trunc(AColor.Blue * 255) shl 16 + Trunc(AColor.Green * 255)
    shl 8 + Trunc(AColor.Red * 255);
  Result := SetTextColor(FHDC, ATextColor);
end;

procedure TdxPDFRenderingInterpreter.StrokePaths;
var
  APen: TdxGPPen;
begin
  APen := CreatePen;
  try
    PerformGraphicsOperation(procedure
      var
        APath: TdxPDFGraphicsPath;
        APrevSmoothingMode: TdxGPSmoothingMode;
        APaths: TObjectList<TdxPDFGraphicsPath>;
      begin
        APrevSmoothingMode := Graphics.SmoothingMode;
        try
          UpdateSmoothingMode(False);
          APaths := CreateTransformedPaths;
          try
            for APath in APaths do
              StrokePath(APath, APen, APrevSmoothingMode);
          finally
            APaths.Free;
          end;
        finally
          SetSmoothingMode(APrevSmoothingMode);
        end;
      end);
  finally
    APen.Free;
  end;
end;

function TdxPDFRenderingInterpreter.CreatePen: TdxGPPen;
const
  LineCapStyleMap: array [TdxPDFLineCapStyle] of TdxGPPenLineCapStyle =
    (gpcsFlat, gpcsRound, gpcsSquare);
  LineDashStyleMap: array [TdxPDFLineCapStyle] of TdxGPPenDashCapStyle =
    (gpdcFlat, gpdcRound, gpdcFlat);
  LineJoinMap: array [TdxPDFLineJoinStyle] of TdxGpLineJoin =
    (LineJoinMiterClipped, LineJoinRound, LineJoinBevel);
begin
  Result := TdxGPPen.Create;
  Result.Brush.Assign(FStrokingBrush);
  Result.DashCapStyle := LineDashStyleMap
    [GraphicsState.Parameters.LineCapStyle];
  Result.LineStartCapStyle := LineCapStyleMap
    [GraphicsState.Parameters.LineCapStyle];
  Result.LineEndCapStyle := Result.LineStartCapStyle;
  Result.MiterLimit := GraphicsState.Parameters.MiterLimit;
  GdipCheck(GdipSetPenLineJoin(Result.Handle,
    LineJoinMap[GraphicsState.Parameters.LineJoinStyle]));
end;

{ TdxPDFGraphicsPathSegment }

constructor TdxPDFGraphicsPathSegment.Create(const AEndPoint: TdxPointF);
begin
  inherited Create;
  FEndPoint := AEndPoint;
end;

function TdxPDFGraphicsPathSegment.Equal
  (AValue: TdxPDFGraphicsPathSegment): Boolean;
begin
  Result := (ClassType = AValue.ClassType) and
    cxPointIsEqual(EndPoint, AValue.EndPoint);
end;

class function TdxPDFGraphicsPathSegment.Transform
  (ASegment: TdxPDFGraphicsPathSegment;
const AMatrix: TdxPDFTransformationMatrix): TdxPDFGraphicsPathSegment;
var
  AEndPoint: TdxPointF;
  ABezierSegment: TdxPDFBezierGraphicsPathSegment;
begin
  AEndPoint := AMatrix.Transform(ASegment.EndPoint);
  if ASegment is TdxPDFBezierGraphicsPathSegment then
  begin
    ABezierSegment := TdxPDFBezierGraphicsPathSegment(ASegment);
    Result := TdxPDFGraphicsPathSegment(TdxPDFBezierGraphicsPathSegment.Create
      (AMatrix.Transform(ABezierSegment.ControlPoint1),
      AMatrix.Transform(ABezierSegment.ControlPoint2), AEndPoint));
  end
  else
    Result := TdxPDFGraphicsPathSegment
      (TdxPDFLineGraphicsPathSegment.Create(AEndPoint));
end;

procedure TdxPDFGraphicsPathSegment.Transform(const AMatrix
  : TdxPDFTransformationMatrix);
begin
  FEndPoint := AMatrix.Transform(FEndPoint);
end;

{ TdxPDFLineGraphicsPathSegment }

function TdxPDFLineGraphicsPathSegment.GetFlat: Boolean;
begin
  Result := True;
end;

{ TdxPDFBezierGraphicsPathSegment }

constructor TdxPDFBezierGraphicsPathSegment.Create(const AControlPoint1,
  AControlPoint2, AEndPoint: TdxPointF);
begin
  inherited Create(AEndPoint);
  FControlPoint1 := AControlPoint1;
  FControlPoint2 := AControlPoint2;
end;

function TdxPDFBezierGraphicsPathSegment.Equal
  (AValue: TdxPDFGraphicsPathSegment): Boolean;
var
  ABezierSegment: TdxPDFBezierGraphicsPathSegment;
begin
  ABezierSegment := AValue as TdxPDFBezierGraphicsPathSegment;
  Result := inherited Equal(AValue) and cxPointIsEqual(ControlPoint1,
    ABezierSegment.ControlPoint1) and cxPointIsEqual(ControlPoint2,
    ABezierSegment.ControlPoint2);
end;

function TdxPDFBezierGraphicsPathSegment.GetFlat: Boolean;
begin
  Result := False;
end;

procedure TdxPDFBezierGraphicsPathSegment.Transform(const AMatrix
  : TdxPDFTransformationMatrix);
begin
  inherited Transform(AMatrix);
  FControlPoint1 := AMatrix.Transform(FControlPoint1);
  FControlPoint2 := AMatrix.Transform(FControlPoint2);
end;

{ TdxPDFGraphicsPath }

constructor TdxPDFGraphicsPath.Create(const AStartPoint: TdxPointF);
begin
  inherited Create;
  FSegments := TObjectList<TdxPDFGraphicsPathSegment>.Create;
  FStartPoint := AStartPoint;
end;

destructor TdxPDFGraphicsPath.Destroy;
begin
  FreeAndNil(FSegments);
  inherited Destroy;
end;

function TdxPDFGraphicsPath.GetEndPoint: TdxPointF;
var
  ACount: Integer;
begin
  ACount := FSegments.Count;
  if ACount = 0 then
    Result := FStartPoint
  else
    Result := FSegments[ACount - 1].EndPoint;
end;

function TdxPDFGraphicsPath.Equal(AValue: TdxPDFGraphicsPath): Boolean;
var
  I: Integer;
begin
  Result := (IsClosed = AValue.IsClosed) and
    cxPointIsEqual(EndPoint, AValue.EndPoint) and
    cxPointIsEqual(StartPoint, AValue.StartPoint) and
    (Segments.Count = AValue.Segments.Count);
  if Result then
    for I := 0 to AValue.Segments.Count - 1 do
    begin
      Result := Segments[I].Equal(AValue.Segments[I]);
      if not Result then
        Break;
    end;
end;

function TdxPDFGraphicsPath.IsFlat(AIsFilling: Boolean): Boolean;
var
  X, Y: Double;
  AEnd: TdxPointDouble;
  ASegment: TdxPDFGraphicsPathSegment;
begin
  X := FStartPoint.X;
  Y := FStartPoint.Y;
  for ASegment in FSegments do
  begin
    if not ASegment.Flat then
      Exit(False);
    AEnd := dxPointDouble(ASegment.EndPoint);
    if (AEnd.X <> X) and (AEnd.Y <> Y) then
      Exit(False);
    X := AEnd.X;
    Y := AEnd.Y;
  end;
  Result := not AIsFilling or FIsClosed or (StartPoint.X = X) and
    (StartPoint.Y = Y);
end;

procedure TdxPDFGraphicsPath.AppendLineSegment(const AEndPoint: TdxPointF);
begin
  FSegments.Add(TdxPDFLineGraphicsPathSegment.Create(AEndPoint));
end;

procedure TdxPDFGraphicsPath.AppendBezierSegment(const AControlPoint1,
  AControlPoint2, AEndPoint: TdxPointF);
begin
  FSegments.Add(TdxPDFBezierGraphicsPathSegment.Create(AControlPoint1,
    AControlPoint2, AEndPoint));
end;

class function TdxPDFGraphicsPath.Transform(APaths: TList<TdxPDFGraphicsPath>;
const AMatrix: TdxPDFTransformationMatrix): TObjectList<TdxPDFGraphicsPath>;
var
  APathsCount: Integer;
  ARectangularGraphicsPath: TdxPDFRectangularGraphicsPath;
  ARectangle, ATransformedRectangle: TdxPDFRectangle;
  APath, ATransformedPath: TdxPDFGraphicsPath;
  ASegment: TdxPDFGraphicsPathSegment;
begin
  APathsCount := APaths.Count;
  Result := TObjectList<TdxPDFGraphicsPath>.Create;
  if (APathsCount = 1) and not AMatrix.IsRotated then
  begin
    if APaths[0] is TdxPDFRectangularGraphicsPath then
    begin
      ARectangularGraphicsPath := TdxPDFRectangularGraphicsPath(APaths[0]);
      ARectangle := ARectangularGraphicsPath.Rectangle;
      ATransformedRectangle := TdxPDFRectangle.Create
        (AMatrix.Transform(ARectangle.BottomLeft),
        AMatrix.Transform(ARectangle.TopRight));
      Result.Add(TdxPDFRectangularGraphicsPath.Create
        (ATransformedRectangle.Left, ATransformedRectangle.Bottom,
        ATransformedRectangle.Width, ATransformedRectangle.Height));
      Exit;
    end;
  end;
  for APath in APaths do
  begin
    ATransformedPath := TdxPDFGraphicsPath.Create
      (AMatrix.Transform(APath.StartPoint));
    for ASegment in APath.Segments do
      ATransformedPath.Segments.Add(TdxPDFGraphicsPathSegment.Transform
        (ASegment, AMatrix));
    ATransformedPath.IsClosed := APath.IsClosed;
    Result.Add(ATransformedPath);
  end;
end;

{ TdxPDFRectangularGraphicsPath }

constructor TdxPDFRectangularGraphicsPath.Create(ALeft, ABottom, AWidth,
  AHeight: Double);
var
  ARight, ATop: Double;
begin
  inherited Create(dxPointF(ALeft, ABottom));
  ARight := ALeft + AWidth;
  ATop := ABottom + AHeight;
  AppendLineSegment(dxPointF(ARight, ABottom));
  AppendLineSegment(dxPointF(ARight, ATop));
  AppendLineSegment(dxPointF(ALeft, ATop));
  AppendLineSegment(dxPointF(ALeft, ABottom));
  FRectangle := TdxPDFRectangle.Create(dxPointF(ALeft, ABottom),
    dxPointF(ARight, ATop));
  IsClosed := True;
end;

function TdxPDFRectangularGraphicsPath.Equal
  (AValue: TdxPDFGraphicsPath): Boolean;
begin
  Result := inherited Equal(AValue) and
    Rectangle.Equal(TdxPDFRectangularGraphicsPath(AValue).Rectangle);
end;

function TdxPDFRectangularGraphicsPath.IsFlat(AIsFilling: Boolean): Boolean;
begin
  Result := True;
end;

{ TdxPDFGraphicsPathBuilder }

constructor TdxPDFGraphicsPathBuilder.Create(AInterpreter
  : TdxPDFRenderingInterpreter);
begin
  inherited Create;
  FInterpreter := AInterpreter;
  FUseRectangularGraphicsPath := AInterpreter.UseRectangularGraphicsPath;
end;

class function TdxPDFGraphicsPathBuilder.CreateFillPath
  (AInterpreter: TdxPDFRenderingInterpreter; APaths: TList<TdxPDFGraphicsPath>)
  : TdxPDFFillPathInfo;
var
  ABuilder: TdxPDFGraphicsPathBuilder;
begin
  ABuilder := TdxPDFGraphicsPathBuilder.Create(AInterpreter);
  try
    Result := ABuilder.CreateFillPath(APaths);
  finally
    ABuilder.Free;
  end;
end;

class function TdxPDFGraphicsPathBuilder.CreatePath(AInterpreter
  : TdxPDFRenderingInterpreter; APath: TdxPDFGraphicsPath; AExtendSize: Double)
  : TdxGPPath;
var
  ABuilder: TdxPDFGraphicsPathBuilder;
begin
  ABuilder := TdxPDFGraphicsPathBuilder.Create(AInterpreter);
  try
    Result := ABuilder.InternalCreatePath(APath, AExtendSize);
  finally
    ABuilder.Free;
  end;
end;

class function TdxPDFGraphicsPathBuilder.CreateRegion
  (AInterpreter: TdxPDFRenderingInterpreter): TdxGPRegion;
var
  ABuilder: TdxPDFGraphicsPathBuilder;
  AClipRect: TdxRectF;
  AFillPath: TdxPDFFillPathInfo;
  APaths: TObjectList<TdxPDFGraphicsPath>;
begin
  APaths := AInterpreter.CreateBoundsClippedTransformedPaths;
  try
    ABuilder := TdxPDFGraphicsPathBuilder.Create(AInterpreter);
    try
      AClipRect := ABuilder.CreateRectangle(APaths.First);
      if TdxPDFUtils.IsRectEmpty(AClipRect) then
      begin
        AFillPath := ABuilder.CreateFillPath(APaths);
        try
          if AFillPath.GraphicsPath.GetPointCount = 0 then
          begin
            Result := TdxGPRegion.Create;
            Result.MakeEmpty;
          end
          else
            Result := ABuilder.CreateRegionFromPath(AFillPath,
              AInterpreter.GetFillMode);
        finally
          AFillPath.Free;
        end;
      end
      else
        Result := TdxGPRegion.Create(AClipRect);
    finally
      ABuilder.Free;
    end;
  finally
    APaths.Free;
  end;
end;

function TdxPDFGraphicsPathBuilder.CreateFillPath
  (APaths: TList<TdxPDFGraphicsPath>): TdxPDFFillPathInfo;
var
  AGraphicsPath, AStrokePath: TdxGPPath;
  APath: TdxPDFGraphicsPath;
begin
  AGraphicsPath := TdxGPPath.Create;
  AStrokePath := TdxGPPath.Create;
  for APath in APaths do
  begin
    if not APath.IsClosed and not cxPointIsEqual(APath.StartPoint,
      APath.EndPoint) then
      APath.AppendLineSegment(APath.StartPoint);
    AppendPath(AGraphicsPath, APath, AStrokePath);
  end;
  if AStrokePath.GetPointCount = 0 then
    FreeAndNil(AStrokePath);
  Result := TdxPDFFillPathInfo.Create(AGraphicsPath, AStrokePath);
end;

function TdxPDFGraphicsPathBuilder.CreateRectangle(APath: TdxPDFGraphicsPath)
  : TdxRectF;
var
  P1, P2: TdxPointF;
  R: TdxPDFRectangle;
  X1, Y1, X2, Y2, ATemp: Double;
begin
  if FUseRectangularGraphicsPath and (APath is TdxPDFRectangularGraphicsPath)
  then
  begin
    R := TdxPDFRectangularGraphicsPath(APath).Rectangle;
    P1 := FInterpreter.ToCanvasPoint(dxPointF(R.Left, R.Bottom));
    P2 := FInterpreter.ToCanvasPoint(dxPointF(R.Right, R.Top));
    X1 := P1.X;
    X2 := P2.X;
    if X1 > X2 then
    begin
      ATemp := X1;
      X1 := X2;
      X2 := ATemp;
    end;
    X2 := Max(X2, X1 + 1);
    Y1 := P1.Y;
    Y2 := P2.Y;
    if Y1 > Y2 then
    begin
      ATemp := Y1;
      Y1 := Y2;
      Y2 := ATemp;
    end;
    Result := TdxRectF.CreateSize(X1, Y1, Max(X2 - X1, 1), Max(Y2 - Y1, 1));
  end
  else
    Result := dxNullRectF;
end;

function TdxPDFGraphicsPathBuilder.CreateRegionFromPath
  (AFillPath: TdxPDFFillPathInfo; AFillMode: TdxGPFillMode): TdxGPRegion;
const
  Flatness = 0.6666667;
var
  AMatrix: TdxGPMatrix;
  APen: TdxGPPen;
begin
  AFillPath.GraphicsPath.FillMode := AFillMode;
  Result := TdxGPRegion.Create(AFillPath.GraphicsPath.Handle);
  if AFillPath.StrokePath <> nil then
  begin
    AMatrix := TdxGPMatrix.Create;
    APen := TdxGPPen.Create;
    try
      APen.Brush.Color := TdxAlphaColors.Black;
      try
        GdipWidenPath(AFillPath.StrokePath.Handle, APen.Handle, AMatrix.Handle,
          Flatness);
        Result.CombineRegionPath(AFillPath.StrokePath, gmUnion);
      except
      end;
    finally
      APen.Free;
      AMatrix.Free;
    end;
  end;
end;

function TdxPDFGraphicsPathBuilder.InternalCreatePath(APath: TdxPDFGraphicsPath;
AExtendSize: Double): TdxGPPath;

  function NeedAppendExtendLine(ASegment: TdxPDFGraphicsPathSegment): Boolean;
  begin
    Result := (AExtendSize > 0) and (ASegment is TdxPDFLineGraphicsPathSegment);
  end;

var
  P: TdxPointF;
  ALastSegmentIndex: Integer;
begin
  Result := TdxGPPath.Create;
  ALastSegmentIndex := APath.Segments.Count - 1;
  if ALastSegmentIndex >= 0 then
  begin
    if NeedAppendExtendLine(APath.Segments[0]) then
      AppendExtendLine(Result, APath.StartPoint, APath.Segments[0].EndPoint,
        AExtendSize, True);
    AppendPath(Result, APath, nil);
    if NeedAppendExtendLine(APath.Segments[0]) then
    begin
      P := APath.Segments[ALastSegmentIndex].EndPoint;
      if ALastSegmentIndex = 0 then
        P := APath.StartPoint;
      AppendExtendLine(Result, P, APath.Segments[ALastSegmentIndex].EndPoint,
        AExtendSize, False)
    end;
  end;
end;

procedure TdxPDFGraphicsPathBuilder.AppendBezier(AGraphicsPath: TdxGPPath;
const AStartPoint, AEndPoint: TdxPointF;
ASegment: TdxPDFBezierGraphicsPathSegment);
var
  P1, P2: TdxPointF;
begin
  P1 := FInterpreter.ToCanvasPoint(ASegment.ControlPoint1);
  P2 := FInterpreter.ToCanvasPoint(ASegment.ControlPoint2);
  AGraphicsPath.AddBezier(AStartPoint, P1, P2, AEndPoint)
end;

procedure TdxPDFGraphicsPathBuilder.AppendExtendLine(AGraphicsPath: TdxGPPath;
const AStartPoint, AEndPoint: TdxPointF; ASize: Double; AFromLeft: Boolean);
var
  AAngle: Double;
  AExtendSize, P1, P2: TdxPointF;
begin
  AAngle := FInterpreter.CalculateAngle(AStartPoint, AEndPoint);
  AExtendSize := dxPointF(Sin(AAngle) * ASize, Cos(AAngle) * ASize);
  if AFromLeft then
  begin
    P1 := cxPointOffset(AStartPoint, AExtendSize, False);
    P2 := AStartPoint;
  end
  else
  begin
    P1 := AEndPoint;
    P2 := cxPointOffset(AEndPoint, AExtendSize);
  end;
  P1 := FInterpreter.ToCanvasPoint(P1);
  P2 := FInterpreter.ToCanvasPoint(P2);
  AGraphicsPath.AddLine(P1.X, P1.Y, P2.X, P2.Y);
end;

procedure TdxPDFGraphicsPathBuilder.AppendLine(AStrokePath: TdxGPPath;
const AStartPoint, AEndPoint: TdxPointF);
begin
  if AStrokePath <> nil then
  begin
    AStrokePath.FigureStart;
    AStrokePath.AddLine(AStartPoint.X, AStartPoint.Y, AEndPoint.X, AEndPoint.Y);
  end;
end;

procedure TdxPDFGraphicsPathBuilder.AppendPath(AGPPath: TdxGPPath;
APath: TdxPDFGraphicsPath; AStrokePath: TdxGPPath);
var
  R: TdxRectF;
begin
  R := CreateRectangle(APath);
  if not R.IsEmpty then
    AGPPath.AddRect(R)
  else if APath.Segments.Count = 1 then
    AppendSegment(AGPPath, APath, AStrokePath)
  else
    AppendSegments(AGPPath, APath, AStrokePath);
  if APath.IsClosed then
    AGPPath.FigureFinish
  else
    AGPPath.FigureStart;
end;

procedure TdxPDFGraphicsPathBuilder.AppendSegment(AGPPath: TdxGPPath;
APath: TdxPDFGraphicsPath; AStrokePath: TdxGPPath);
var
  P, AStartPoint, AEndPoint: TdxPointF;
  ASegment: TdxPDFGraphicsPathSegment;
begin
  AStartPoint := FInterpreter.ToCanvasPoint(APath.StartPoint);
  ASegment := APath.Segments[0];
  AEndPoint := FInterpreter.ToCanvasPoint(ASegment.EndPoint);
  if not(ASegment is TdxPDFBezierGraphicsPathSegment) then
  begin
    if cxPointIsEqual(AStartPoint, AEndPoint) then
    begin
      if (FInterpreter.RotationAngle = 90) or (FInterpreter.RotationAngle = 270)
      then
        P := dxPointF(AEndPoint.X, AEndPoint.Y + 1)
      else
        P := dxPointF(AEndPoint.X + 1, AEndPoint.Y);
      AGPPath.AddLine(AStartPoint.X, AStartPoint.Y, P.X, P.Y);
      AppendLine(AStrokePath, AStartPoint, P);
    end
    else
    begin
      AGPPath.AddLine(AStartPoint.X, AStartPoint.Y, AEndPoint.X, AEndPoint.Y);
      AppendLine(AStrokePath, AStartPoint, AEndPoint);
    end
  end
  else
  begin
    AppendBezier(AGPPath, AStartPoint, AEndPoint,
      ASegment as TdxPDFBezierGraphicsPathSegment);
    if AStrokePath <> nil then
    begin
      AStrokePath.FigureStart;
      AppendBezier(AStrokePath, AStartPoint, AEndPoint,
        ASegment as TdxPDFBezierGraphicsPathSegment);
    end;
  end;
end;

procedure TdxPDFGraphicsPathBuilder.AppendSegments(AGPPath: TdxGPPath;
APath: TdxPDFGraphicsPath; AStrokePath: TdxGPPath);

  function ComparePoints(const P1, P2: TdxPointF): Boolean;
  begin
    Result := cxPointIsEqual(cxPoint(P1), cxPoint(P2));
  end;

  procedure AppendStrokeBezier(ASegment: TdxPDFGraphicsPathSegment;
  AStrokePath: TdxGPPath;
  AStack: TStack<TPair<TdxPointF, TdxPDFGraphicsPathSegment>>;
  const AStartPoint, AEndPoint: TdxPointF);
  var
    APreviousBezierSegment: TdxPDFBezierGraphicsPathSegment;
    AControlPoint1, AControlPoint2: TdxPointF;
    APair: TPair<TdxPointF, TdxPDFGraphicsPathSegment>;
    ABezierSegment: TdxPDFBezierGraphicsPathSegment;
  begin
    if (AStack.Count > 0) and ComparePoints(AStack.Peek.Key, AEndPoint) then
    begin
      APair := AStack.Pop;
      AStrokePath.FigureStart;
      APreviousBezierSegment := Safe<TdxPDFBezierGraphicsPathSegment>.Cast
        (APair.Value);
      ABezierSegment := Safe<TdxPDFBezierGraphicsPathSegment>.Cast(ASegment);
      if (ABezierSegment = nil) and (APreviousBezierSegment = nil) then
        AStrokePath.AddLine(AStartPoint.X, AStartPoint.Y, AEndPoint.X,
          AEndPoint.Y)
      else if (ABezierSegment <> nil) and (APreviousBezierSegment <> nil) then
      begin
        AControlPoint1 := FInterpreter.ToCanvasPoint
          (ABezierSegment.ControlPoint1);
        AControlPoint2 := FInterpreter.ToCanvasPoint
          (ABezierSegment.ControlPoint2);
        if ComparePoints(AControlPoint1,
          FInterpreter.ToCanvasPoint(APreviousBezierSegment.ControlPoint2)) and
          ComparePoints(AControlPoint2,
          FInterpreter.ToCanvasPoint(APreviousBezierSegment.ControlPoint1)) then
          AStrokePath.AddBezier(AStartPoint, AControlPoint1, AControlPoint2,
            AEndPoint);
      end;
    end
    else if not ComparePoints(AStartPoint, AEndPoint) then
      AStack.Push(TPair<TdxPointF, TdxPDFGraphicsPathSegment>.Create
        (AStartPoint, ASegment));
  end;

var
  AStartPoint, AEndPoint: TdxPointF;
  ASegment: TdxPDFGraphicsPathSegment;
  AStack: TStack<TPair<TdxPointF, TdxPDFGraphicsPathSegment>>;
begin
  AStartPoint := FInterpreter.ToCanvasPoint(APath.StartPoint);
  AStack := TStack < TPair < TdxPointF, TdxPDFGraphicsPathSegment >>.Create;
  try
    for ASegment in APath.Segments do
    begin
      AEndPoint := FInterpreter.ToCanvasPoint(ASegment.EndPoint);
      if AStrokePath <> nil then
        AppendStrokeBezier(ASegment, AStrokePath, AStack, AStartPoint,
          AEndPoint);
      if not(ASegment is TdxPDFBezierGraphicsPathSegment) then
        AGPPath.AddLine(AStartPoint.X, AStartPoint.Y, AEndPoint.X, AEndPoint.Y)
      else
        AppendBezier(AGPPath, AStartPoint, AEndPoint,
          ASegment as TdxPDFBezierGraphicsPathSegment);
      AStartPoint := AEndPoint;
    end;
  finally
    AStack.Free;
  end;
end;

{ TdxPDFGraphicsDevice }

procedure TdxPDFGraphicsDevice.DoExport(ACommands: TdxPDFCommandList);
var
  AParameters: TdxPDFRenderParameters;
begin
  AParameters := GetRenderParameters;
  AParameters.Canvas.Lock;
  try
    ExportBackground;
    ExportContent(ACommands);
  finally
    AParameters.Canvas.Unlock;
  end;
end;

procedure TdxPDFGraphicsDevice.ExportContent(ACommands: TdxPDFCommandList);
begin
  inherited DoExport(ACommands);
end;

procedure TdxPDFGraphicsDevice.ExportBackground;
var
  AParameters: TdxPDFRenderParameters;
  APrevColor: TColor;
begin
  AParameters := GetRenderParameters;
  APrevColor := AParameters.Canvas.Brush.Color;
  try
    AParameters.Canvas.Brush.Color := clWhite;
    AParameters.Canvas.FillRect(AParameters.Rect);
  finally
    AParameters.Canvas.Brush.Color := APrevColor;
  end;
end;

{ TdxPDFDocumentContentRecognizer }

constructor TdxPDFDocumentContentRecognizer.Create
  (AContent: TdxPDFRecognizedContent;
ARecognitionObjects: TdxPDFRecognitionObjects);
begin
  inherited Create;
  FRecognitionObjects := ARecognitionObjects;
  FContent := AContent;
end;

function TdxPDFDocumentContentRecognizer.CheckRenderingMode: Boolean;
begin
  Result := True;
end;

procedure TdxPDFDocumentContentRecognizer.AfterExportPage(APage: TdxPDFPage);

  procedure AddField(AField: TdxPDFAcroFormField);
  begin
    TdxPDFRecognizedContentAccess(FContent).AddAnnotationField
      (TdxPDFAnnotationField(AField));
  end;

var
  AAnnotation: TdxPDFReferencedObject;
  AData: TdxPDFPageData;
  AField: TdxPDFAcroFormField;
begin
  inherited AfterExportPage(APage);
  AData := TdxPDFPageAccess(APage).Data;
  for AAnnotation in AData.Annotations do
  begin
    AField := TdxPDFCustomAnnotationAccess(AAnnotation)
      .CreateField(DocumentState);
    if AField <> nil then
      case AField.HitCode of
        hcHyperlink:
          FContent.Hyperlinks.Add(TdxPDFHyperlink(AField));
        hcAttachment, hcAnnotationObject:
          if AllowRecognizeAnnotations then
            AddField(AField);
      else
        FContent.AcroFormFields.Add(AField);
      end;
  end;
end;

procedure TdxPDFDocumentContentRecognizer.AppendPathBezierSegment(const P2,
  AEndPoint: TdxPointF);
begin
  // do nothing
end;

procedure TdxPDFDocumentContentRecognizer.AppendPathBezierSegment(const P1, P2,
  P3: TdxPointF);
begin
  // do nothing
end;

procedure TdxPDFDocumentContentRecognizer.AppendPathLineSegment(const AEndPoint
  : TdxPointF);
begin
  // do nothing
end;

procedure TdxPDFDocumentContentRecognizer.AppendRectangle(X, Y, AWidth,
  AHeight: Double);
begin
  // do nothing
end;

procedure TdxPDFDocumentContentRecognizer.BeginPath(const AStartPoint
  : TdxPointF);
begin
  // do nothing
end;

procedure TdxPDFDocumentContentRecognizer.ClipAndClearPaths;
begin
  // do nothing
end;

procedure TdxPDFDocumentContentRecognizer.ClipPaths;
begin
  // do nothing
end;

procedure TdxPDFDocumentContentRecognizer.ClosePath;
begin
  // do nothing
end;

procedure TdxPDFDocumentContentRecognizer.DoExport
  (ACommands: TdxPDFCommandList);
begin
  FTextParser := TdxPDFTextParser.Create(FParameters.Bounds, FContent);
  try
    inherited DoExport(ACommands);
    FTextParser.Parse;
  finally
    FTextParser.Free;
  end;
end;

procedure TdxPDFDocumentContentRecognizer.DrawImage
  (AImage: TdxPDFDocumentImage);
var
  ACorners: TdxPointsF;
  ACorner, P, ABottomLeft: TdxPointF;
  I: Integer;
  X, Y, AMinX, AMinY, AMaxX, AMaxY: Single;
  ATempImage: TdxPDFImage;
begin
  if AllowRecognizeImage then
  begin
    SetLength(ACorners, 3);
    ACorners[0] := dxPointF(0, 1);
    ACorners[1] := dxPointF(1, 0);
    ACorners[2] := dxPointF(1, 1);

    ABottomLeft := dxPointF(cxNullPoint);
    P := TransformMatrix.Transform(ABottomLeft);
    AMinX := P.X;
    AMaxX := AMinX;
    AMinY := P.Y;
    AMaxY := AMinY;
    for I := 0 to 2 do
    begin
      ACorner := ACorners[I];
      P := TransformMatrix.Transform(ACorner);
      X := P.X;
      if X < AMinX then
        AMinX := X
      else if X > AMaxX then
        AMaxX := X;
      Y := P.Y;
      if Y < AMinY then
        AMinY := Y
      else if Y > AMaxY then
        AMaxY := Y;
    end;
    ATempImage := TdxPDFImage.Create;
    TdxPDFImageAccess(ATempImage)
      .SetBounds(dxRectF(AMinX, AMinY, AMaxX, AMaxY));
    TdxPDFImageAccess(ATempImage).DocumentImage := AImage;
    FContent.Images.Insert(0, ATempImage);
  end;
end;

procedure TdxPDFDocumentContentRecognizer.DrawString
  (const AData: TdxPDFStringData);
begin
  if AllowRecognizeText then
    FTextParser.AddBlock(AData, GraphicsState);
end;

procedure TdxPDFDocumentContentRecognizer.InitializeTransformMatrix;
begin
  GraphicsState.TransformMatrix.Assign(1, 0, 0, 1, -FParameters.Bounds.Left,
    -FParameters.Bounds.Bottom);
end;

procedure TdxPDFDocumentContentRecognizer.StrokePaths;
begin
  // do nothing
end;

function TdxPDFDocumentContentRecognizer.AllowRecognizeAnnotations: Boolean;
begin
  Result := rmAnnotations in FRecognitionObjects;
end;

function TdxPDFDocumentContentRecognizer.AllowRecognizeImage: Boolean;
begin
  Result := rmImages in FRecognitionObjects;
end;

function TdxPDFDocumentContentRecognizer.AllowRecognizeText: Boolean;
begin
  Result := rmText in FRecognitionObjects;
end;

{ TdxPDFAttachmentUnpacker }

procedure TdxPDFAttachmentUnpacker.BeforeExportPage(AData: TdxPDFPageData;
AParameters: TdxPDFExportParameters);
begin
  // do nothing;
end;

procedure TdxPDFAttachmentUnpacker.DoExport(ACommands: TdxPDFCommandList);
begin
  // do nothing;
end;

{ TdxPDFPolygon }

constructor TdxPDFPolygon.Create;
const
  PolygonMinDouble = -1.7976931348623157081E+308;
begin
  inherited Create;
  FPoints := TList<TdxPointF>.Create;
  FLastPoint := TdxPointF.Create(PolygonMinDouble, PolygonMinDouble);
end;

destructor TdxPDFPolygon.Destroy;
begin
  FreeAndNil(FPoints);
  inherited Destroy;
end;

procedure TdxPDFPolygon.AddPoint(X, Y: Single);
var
  APoint: TdxPointF;
begin
  APoint := TdxPointF.Create(X, Y);
  if not cxPointIsEqual(APoint, FLastPoint) then
  begin
    FLastPoint := APoint;
    FPoints.Add(APoint);
  end;
end;

{ TdxPDFPolygonClipper }

constructor TdxPDFPolygonClipper.Create(const ABounds: TdxPDFRectangle);
begin
  inherited Create;
  FPointEdges := TDictionary<TdxPointF, TdxPDFPolygonClipperEdge>.Create;
  FBounds := ABounds;
  SetLength(FEdges, 4);
  FEdges[0] := ceLeft;
  FEdges[1] := ceRight;
  FEdges[2] := ceBottom;
  FEdges[3] := ceTop;
end;

destructor TdxPDFPolygonClipper.Destroy;
begin
  FreeAndNil(FPointEdges);
  inherited Destroy;
end;

class function TdxPDFPolygonClipper.Clip(const R: TdxPDFRectangle;
APath: TdxPDFGraphicsPath): TdxPDFGraphicsPath;
var
  AClipper: TdxPDFPolygonClipper;
begin
  AClipper := TdxPDFPolygonClipper.Create(R);
  try
    Result := AClipper.Clip(APath);
  finally
    AClipper.Free;
  end;
end;

function TdxPDFPolygonClipper.Clip(APath: TdxPDFGraphicsPath)
  : TdxPDFGraphicsPath;
var
  AClippedRect: TdxPDFRectangle;
  APoints: TList<TdxPointF>;
  ASegment: TdxPDFGraphicsPathSegment;
  APointsCount, I: Integer;
  AEdge: TdxPDFPolygonClipperEdge;
  AFirstPoint, APreviousPoint, ACurrentPoint: TdxPointF;
  AIsPreviousPointInside: Boolean;
  ASegments: TList<TdxPDFGraphicsPathSegment>;
begin
  if APath is TdxPDFRectangularGraphicsPath then
  begin
    AClippedRect := TdxPDFRectangle.Intersect(FBounds,
      TdxPDFRectangularGraphicsPath(APath).Rectangle);
    if AClippedRect.IsNull then
      Result := APath
    else
      Result := TdxPDFRectangularGraphicsPath.Create(AClippedRect.Left,
        AClippedRect.Bottom, AClippedRect.Width, AClippedRect.Height);
    Exit;
  end;
  APoints := TList<TdxPointF>.Create;
  try
    APoints.Add(APath.StartPoint);
    for ASegment in APath.Segments do
    begin
      if not(ASegment is TdxPDFLineGraphicsPathSegment) then
        Exit(APath);
      APoints.Add(ASegment.EndPoint);
    end;
    APointsCount := APoints.Count;
    for AEdge in FEdges do
    begin
      if APointsCount < 2 then
        Break;
      FPolygon := TdxPDFPolygon.Create;
      try
        AFirstPoint := APoints[0];
        APreviousPoint := AFirstPoint;
        AIsPreviousPointInside := IsInside(APreviousPoint, AEdge);
        for I := 1 to APointsCount - 1 do
        begin
          ACurrentPoint := APoints[I];
          AIsPreviousPointInside := ClipEdge(AEdge, AIsPreviousPointInside,
            APreviousPoint, ACurrentPoint);
          APreviousPoint := ACurrentPoint;
        end;
        ClipEdge(AEdge, AIsPreviousPointInside, APreviousPoint, AFirstPoint);
        APoints.Clear;
        APoints.AddRange(FPolygon.Points);
        APointsCount := APoints.Count;
      finally
        FPolygon.Free;
      end;
    end;
    if APointsCount < 2 then
      Exit(APath);
    Result := TdxPDFGraphicsPath.Create(APoints[0]);
    ASegments := Result.Segments;
    for I := 1 to APointsCount - 1 do
      ASegments.Add(TdxPDFLineGraphicsPathSegment.Create(APoints[I]));
    Result.IsClosed := APath.IsClosed;
  finally
    APoints.Free;
  end;
end;

function TdxPDFPolygonClipper.IsInside(const APoint: TdxPointF;
AEdge: TdxPDFPolygonClipperEdge): Boolean;
var
  APointEdge: TdxPDFPolygonClipperEdge;
begin
  if not FPointEdges.TryGetValue(APoint, APointEdge) then
  begin
    if APoint.X < FBounds.Left then
      APointEdge := TdxPDFPolygonClipperEdge(Integer(APointEdge) or
        Integer(ceLeft))
    else if APoint.X > FBounds.Right then
      APointEdge := TdxPDFPolygonClipperEdge(Integer(APointEdge) or
        Integer(ceRight));
    if APoint.Y < FBounds.Bottom then
      APointEdge := TdxPDFPolygonClipperEdge(Integer(APointEdge) or
        Integer(ceBottom))
    else if APoint.Y > FBounds.Top then
      APointEdge := TdxPDFPolygonClipperEdge(Integer(APointEdge) or
        Integer(ceTop));
    FPointEdges.Add(APoint, APointEdge);
  end;
  Result := (Integer(APointEdge) and Integer(AEdge)) = Integer(ceNone);
end;

function TdxPDFPolygonClipper.ClipEdge(AEdge: TdxPDFPolygonClipperEdge;
AIsPreviousPointInside: Boolean; const APreviousPoint, ACurrentPoint
  : TdxPointF): Boolean;
var
  AIsInside: Boolean;
begin
  AIsInside := IsInside(ACurrentPoint, AEdge);
  if AIsInside then
  begin
    if not AIsPreviousPointInside then
      AddIntersection(AEdge, APreviousPoint, ACurrentPoint);
    FPolygon.AddPoint(ACurrentPoint.X, ACurrentPoint.Y);
  end
  else if AIsPreviousPointInside then
    AddIntersection(AEdge, APreviousPoint, ACurrentPoint);
  Result := AIsInside;
end;

procedure TdxPDFPolygonClipper.AddIntersection(AEdge: TdxPDFPolygonClipperEdge;
const APoint1, APoint2: TdxPointF);
var
  X, Y, AFactor: Double;
begin
  X := APoint1.X;
  Y := APoint1.Y;
  if APoint2.X - X <> 0 then
    AFactor := (APoint2.Y - Y) / (APoint2.X - X)
  else
    AFactor := 0;
  case AEdge of
    ceLeft:
      if AFactor = 0 then
        FPolygon.AddPoint(FBounds.Left, Y)
      else
        FPolygon.AddPoint(FBounds.Left, Y + AFactor * (FBounds.Left - X));
    ceRight:
      if AFactor = 0 then
        FPolygon.AddPoint(FBounds.Right, Y)
      else
        FPolygon.AddPoint(FBounds.Right, Y + AFactor * (FBounds.Right - X));
    ceBottom:
      if AFactor = 0 then
        FPolygon.AddPoint(X, FBounds.Bottom)
      else
        FPolygon.AddPoint(X + (FBounds.Bottom - Y) / AFactor, FBounds.Bottom);
  else
    if AFactor = 0 then
      FPolygon.AddPoint(X, FBounds.Top)
    else
      FPolygon.AddPoint(X + (FBounds.Top - Y) / AFactor, FBounds.Top);
  end;
end;

{ TdxPDFFillPathInfo }

constructor TdxPDFFillPathInfo.Create(AGraphicsPath, AStrokePath: TdxGPPath);
begin
  inherited Create;
  FGraphicsPath := AGraphicsPath;
  FStrokePath := AStrokePath;
end;

destructor TdxPDFFillPathInfo.Destroy;
begin
  FreeAndNil(FGraphicsPath);
  FreeAndNil(FStrokePath);
  inherited Destroy;
end;

initialization

finalization

FreeAndNil(dxgPDFSystemFontList);

end.
