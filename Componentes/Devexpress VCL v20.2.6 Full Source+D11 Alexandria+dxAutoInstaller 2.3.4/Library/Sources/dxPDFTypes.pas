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

unit dxPDFTypes;

{$I cxVer.inc}

interface

uses
  Types, SysUtils, Classes, Windows, Generics.Defaults, Generics.Collections,
  Math, dxCore, dxCoreClasses, cxClasses,
  cxGraphics, dxCoreGraphics, cxGeometry, dxGDIPlusClasses, dxProtectionUtils,
  dxPDFBase;

type
  TdxPDFPoints = TdxPointsF;

  EdxPDFAbortException = class(EAbort);
  TdxPDFArray = class;
  TdxPDFDictionary = class;
  TdxPDFNumericObject = class;
  TdxPDFStream = class;
  EdxPDFException = class(EdxException);
  EdxPDFExceptionClass = class of EdxPDFException;
  EdxPDFEncryptionException = class(EdxPDFException);
  TdxPDFCustomRepository = class;
  TdxPDFCustomRepositoryClass = class of TdxPDFCustomRepository;
  TdxPDFReference = class;

  TdxPDFBlendMode = (bmNormal, bmCompatible, bmMultiply, bmScreen, bmOverlay,
    bmDarken, bmLighten, bmColorDodge, bmColorBurn, bmHardLight, bmSoftLight,
    bmDifference, bmExclusion, bmHue, bmSaturation, bmColor, bmLuminosity);
  TdxPDFLineCapStyle = (lcsButt, lcsRound, lcsProjectingSquare);
  TdxPDFLineJoinStyle = (ljsMiter, ljsRound, ljsBevel);
  TdxPDFPixelFormat = (pfUnknown, pfGray1bit, pfGray8bit, pfArgb24bpp,
    pfArgb32bpp);
  TdxPDFRenderingIntent = (riAbsoluteColorimetric, riRelativeColorimetric,
    riSaturation, riPerceptual);
  TdxPDFSignatureFlag = (sfNone, sfSignaturesExist, sfAppendOnly);
  // for internal use
  TdxPDFSignatureFlags = set of TdxPDFSignatureFlag; // for internal use
  TdxPDFTextRenderingMode = (trmFill, trdStroke, trmFillAndStroke, trmInvisible,
    trmFillAndClip, trmStrokeAndClip, trmFillStrokeAndClip, trmClip);
  TdxPDFVersion = (v1_0, v1_1, v1_2, v1_3, v1_4, v1_5, v1_6, v1_7);

  TdxPDFDocumentDecodedImageData = record
    Data: TBytes;
    PixelFormat: TdxPDFPixelFormat;
  end;

  TdxPDFBytesDynArray = array of TBytes;

  TdxPDFStringCommandData = record
  private
    FCharCodes: TdxPDFBytesDynArray;
    FOffsets: TDoubleDynArray;
    FStr: TWordDynArray;
  public
    class function Create(const ACharCodes: TdxPDFBytesDynArray;
      const AStr: TWordDynArray; const AOffsets: TDoubleDynArray)
      : TdxPDFStringCommandData; static;

    property CharCodes: TdxPDFBytesDynArray read FCharCodes;
    property Offsets: TDoubleDynArray read FOffsets;
    property Str: TWordDynArray read FStr;
  end;

  TdxPDFStringData = record
  strict private
    FAdvances: TDoubleDynArray;
    FCommandData: TdxPDFStringCommandData;
    FWidths: TDoubleDynArray;
    function GetCharCodes: TdxPDFBytesDynArray;
    function GetStr: TWordDynArray;
    function GetOffsets: TDoubleDynArray;
  public
    class function Create(const ACodePointData: TdxPDFStringCommandData;
      const AWidths, AAdvances: TDoubleDynArray): TdxPDFStringData; static;

    property Advances: TDoubleDynArray read FAdvances;
    property CharCodes: TdxPDFBytesDynArray read GetCharCodes;
    property Offsets: TDoubleDynArray read GetOffsets;
    property Str: TWordDynArray read GetStr;
    property Widths: TDoubleDynArray read FWidths;
  end;

  TdxPDFPosition = record
    PageIndex: Integer;
    Point: TdxPointF;
  end;

  TdxPDFDocumentArea = record
    Rect: TdxRectF;
    PageIndex: Integer;
  end;

  TdxPDFOrientedRect = record
  public
    Angle: Single;
    Top: Single;
    Left: Single;
    Height: Single;
    Width: Single;
  end;

  { TdxPDFRange }

  TdxPDFRange = record
  public
    Min: Double;
    Max: Double;

    class function Create(AMin, AMax: Double): TdxPDFRange; static;
    class function Invalid: TdxPDFRange; static;
    function Contains(AValue: Integer): Boolean;
    function IsSame(ARange: TdxPDFRange): Boolean;
  end;

  TdxPDFDocumentID = array [0 .. 1] of TBytes;
  TdxPDFOrientedRectList = class(TList<TdxPDFOrientedRect>);
  TdxPDFRanges = array of TdxPDFRange;

  { TdxPDFRectangle }

  TdxPDFRectangle = record
  private
    FLeft: Single;
    FBottom: Single;
    FRight: Single;
    FTop: Single;
    function GetBottomLeft: TdxPointF;
    function GetBottomRight: TdxPointF;
    function GetTopLeft: TdxPointF;
    function GetTopRight: TdxPointF;
    function GetHeight: Single;
    function GetWidth: Single;
    class function IsSame(A, B, ADelta: Double): Boolean; static;
  public
    class function Create(ALeft, ABottom, ARight, ATop: Single)
      : TdxPDFRectangle; overload; static;
    class function Create(const P1, P2: TdxPointF): TdxPDFRectangle;
      overload; static;
    // class function Create(const R: TdxRectF): TdxPDFRectangle; overload; static;
    class function CreateBoundingRectangle(const APoints: TdxPDFPoints)
      : TdxPDFRectangle; static;
    class function Equal(const R1, R2: TdxPDFRectangle; ADelta: Double)
      : Boolean; overload; static;
    class function Inflate(const R: TdxPDFRectangle; AAmount: Double)
      : TdxPDFRectangle; static;
    class function Intersect(const R1, R2: TdxPDFRectangle)
      : TdxPDFRectangle; static;
    class function Null: TdxPDFRectangle; static;
    class function Parse(AArray: TdxPDFArray): TdxPDFRectangle; static;
    function Equal(const R: TdxPDFRectangle): Boolean; overload;
    function Intersects(const R: TdxPDFRectangle): Boolean;
    function IsNull: Boolean;
    function Trim(const ARectangle: TdxPDFRectangle): TdxPDFRectangle;
    function ToRectF: TdxRectF;

    property Bottom: Single read FBottom;
    property BottomLeft: TdxPointF read GetBottomLeft;
    property BottomRight: TdxPointF read GetBottomRight;
    property Height: Single read GetHeight;
    property Left: Single read FLeft;
    property Right: Single read FRight;
    property Top: Single read FTop;
    property TopLeft: TdxPointF read GetTopLeft;
    property TopRight: TdxPointF read GetTopRight;
    property Width: Single read GetWidth;
  end;

  { TdxPDFOrientedRectHelper }

  TdxPDFOrientedRectHelper = record helper for TdxPDFOrientedRect
  strict private
    function GetBottom: Single;
    function GetRect: TdxRectF;
    function GetRight: Single;
    function GetRotatedRect: TdxRectF;
    function GetTopLeft: TdxPointF; inline;
    function GetTopRight: TdxPointF;
  protected
    class function Invalid: TdxPDFOrientedRect; static;
  public
    class function Create: TdxPDFOrientedRect; overload; static;
    class function Create(const ATopLeft: TdxPointF;
      AWidth, AHeight, AAngle: Single): TdxPDFOrientedRect; overload; static;
    class function Create(const ARect: TdxRectF): TdxPDFOrientedRect;
      overload; static;
    class function Create(const ARect: TdxPDFRectangle): TdxPDFOrientedRect;
      overload; static;
    class function Create(const ARect: TdxRectF; AAngle: Single)
      : TdxPDFOrientedRect; overload; static;

    function IsValid: Boolean;
    function Overlap(const R: TdxPDFOrientedRect): Boolean;
    function PtInRect(const APoint: TdxPointF; AExpandX: Single = 0;
      AExpandY: Single = 0): Boolean;

    property Bottom: Single read GetBottom;
    property Rect: TdxRectF read GetRect;
    property Right: Single read GetRight;
    property RotatedRect: TdxRectF read GetRotatedRect;
    property TopLeft: TdxPointF read GetTopLeft;
    property TopRight: TdxPointF read GetTopRight;
  end;

  { TdxPDFDocumentAreaHelper }

  TdxPDFDocumentAreaHelper = record helper for TdxPDFDocumentArea
  public
    class function Create(APageIndex: Integer; const R: TdxRectF)
      : TdxPDFDocumentArea; overload; static;
    class function Create(const P1, P2: TdxPDFPosition): TdxPDFDocumentArea;
      overload; static;
    class function Empty: TdxPDFDocumentArea; static;
  end;

  { TdxPDFPositionHelper }

  TdxPDFPositionHelper = record helper for TdxPDFPosition
  public
    class function Create: TdxPDFPosition; overload; static;
    class function Create(APageNumber: Integer; const P: TdxPointF)
      : TdxPDFPosition; overload; static;
    function NearTo(const APosition: TdxPDFPosition): Boolean;
    function IsValid: Boolean;
    procedure Invalid;
  end;

  { TdxPDFFixedPointNumber }

  TdxPDFFixedPointNumber = record
  strict private
  const
    FractionPartSize = 22;
    FloatToFixedFactor = 1 shl FractionPartSize;
    Half = 1 shl (FractionPartSize - 1);
  strict private
    FValue: Integer;
  private
    class function Create(AValue: Integer): TdxPDFFixedPointNumber;
      overload; static;
  public
    class function Create(AValue: Single): TdxPDFFixedPointNumber;
      overload; static;
    class operator Add(const A, B: TdxPDFFixedPointNumber)
      : TdxPDFFixedPointNumber;
    class operator Multiply(A: Integer; const B: TdxPDFFixedPointNumber)
      : TdxPDFFixedPointNumber;
    function RoundToByte: Byte;
  end;

  TdxPDFFixedPointNumbers = array of TdxPDFFixedPointNumber;

  { TdxPDFNamedObjectDictionary }

  TdxPDFNamedObjectDictionary = class
  strict private
    FDictionary: TdxPDFStringIntegerDictionary;
    FResourceKey: string;
    FPrefix: string;
    FNextResourceNumber: Integer;
  public
    constructor Create(const AResourceKey, APrefix: string);
    destructor Destroy; override;

    function GetNewResourceName(ADictionary: TdxPDFReferencedObjectDictionary)
      : string; overload;
    function GetNewResourceName(ADictionary: TdxPDFReferencedObjectDictionary;
      const AName: string): string; overload;
    function ContainsValue(AValue: Integer): Boolean;
    procedure ClearResourceNames;

    property ResourceKey: string read FResourceKey;
  end;

  { TdxPDFArray }

  TdxPDFArray = class(TdxPDFBase)
  strict private
    FElementList: TdxPDFBaseList;

    function GetCount: Integer;
    function GetElement(AIndex: Integer): TdxPDFBase;
    procedure SetElement(Index: Integer; const Value: TdxPDFBase);
  protected
    class function GetObjectType: TdxPDFBaseType; override;
    procedure Write(AWriter: TdxPDFWriter); override;
  public
    constructor Create; overload; override;
    constructor Create(const AValues: array of Integer); overload;
    constructor Create(const AValues: array of Single); overload;
    constructor Create(const AValues: array of string); overload;
    destructor Destroy; override;

    procedure Add(const AValue: Boolean); overload;
    procedure Add(const AValue: Double); overload;
    procedure Add(const AValue: Integer); overload;
    procedure Add(const AValue: Single); overload;
    procedure Add(const AValue: TdxPDFBase); overload;
    procedure Add(const AValue: TdxPDFRange); overload;
    procedure Add(const AValue: string); overload;
    procedure Add(const AValue: array of Double); overload;
    procedure Add(const AValue: array of Integer); overload;
    procedure Add(const AValue: array of Single); overload;
    procedure Add(const AValue: array of string); overload;
    procedure AddBytes(const AValue: TBytes);
    procedure AddName(const AValue: string);
    procedure AddNull;
    procedure AddReference(ANumber: Integer; AGeneration: Integer = 0);
    procedure Clear;

    property Count: Integer read GetCount;
    property Elements[Index: Integer]: TdxPDFBase read GetElement
      write SetElement; default;
    property ElementList: TdxPDFBaseList read FElementList;
  end;

  { TdxPDFKeywords }

  TdxPDFKeywords = class
  public const
{$REGION 'public const'}
    AcroForm = 'AcroForm';
    Action = 'Action';
    ActionDestination = 'D';
    ActionFirst = 'First';
    ActionName = 'N';
    ActionNamed = 'Named';
    ActionNext = 'Next';
    ActionType = 'S';
    AESCryptMethodName = 'AESV3';
    Alternate = 'Alternate';
    Annotation = 'Annot';
    AnnotationAppearance = 'AP';
    AnnotationAppearanceDown = 'D';
    AnnotationAppearanceName = 'AS';
    AnnotationAppearanceNormal = 'N';
    AnnotationAppearanceRollover = 'R';
    AnnotationBorderStyle = 'BS';
    AnnotationColor = 'C';
    AnnotationFlags = 'F';
    AnnotationHighlightingMode = 'H';
    AnnotationName = 'NM';
    Annotations = 'Annots';
    AntiAlias = 'AntiAlias';
    AppearanceCharacteristics = 'MK';
    AppearanceCharacteristicsCaption = 'CA';
    AppearanceCharacteristicsBackgroundColor = 'BG';
    AppearanceCharacteristicsBorderColor = 'BC';
    AppearanceCharacteristicsRotation = 'R';
    ArialBoldFontName = 'Arial,Bold';
    ArialFontName = 'Arial';
    ArialItalicFontName = 'Arial,Italic';
    ArialUnicodeMS = 'Arial Unicode MS';
    ArtBox = 'ArtBox';
    AssociatedFileRelationship = 'AFRelationship';
    Author = 'Author';
    BackdropColor = 'BC';
    Background = 'Background';
    BaseEncoding = 'BaseEncoding';
    BaseFont = 'BaseFont';
    BaseVersion = 'BaseVersion';
    BBox = 'BBox';
    BitsPerComponent = 'BitsPerComponent';
    BitsPerSample = 'BitsPerSample';
    BlackPoint = 'BlackPoint';
    BleedBox = 'BleedBox';
    BlendMode = 'BM';
    Bold = 'Bold';
    Border = 'Border';
    BorderStyle = 'BorderStyle';
    BorderStyleName = 'S';
    BorderStyleWidth = 'W';
    Bounds = 'Bounds';
    ByteRange = 'ByteRange';
    C0 = 'C0';
    C1 = 'C1';
    CaretAnnotation = 'Caret';
    Catalog = 'Catalog';
    CCITTFaxDecodeFilterBlackIs1 = 'BlackIs1';
    CCITTFaxDecodeFilterColumns = 'Columns';
    CCITTFaxDecodeFilterDamagedRowsBeforeError = 'DamagedRowsBeforeError';
    CCITTFaxDecodeFilterEncodedByteAlign = 'EncodedByteAlign';
    CCITTFaxDecodeFilterEncodingScheme = 'K';
    CCITTFaxDecodeFilterEndOfBlock = 'EndOfBlock';
    CCITTFaxDecodeFilterEndOfLine = 'EndOfLine';
    CCITTFaxDecodeFilterRows = 'Rows';
    CharProcs = 'CharProcs';
    CIDFontType0 = 'CIDFontType0';
    CIDFontType0C = 'CIDFontType0C';
    CIDFontType2 = 'CIDFontType2';
    CIDSystemInfo = 'CIDSystemInfo';
    CIDToGIDMap = 'CIDToGIDMap';
    CircleAnnotation = 'Circle';
    CMap = 'CMap';
    CMapName = 'CMapName';
    CollectionItem = 'CI';
    Colorants = 'Colorants';
    Colors = 'Colors';
    ColorSpace = 'ColorSpace';
    ColorTransform = 'ColorTransform';
    Columns = 'Columns';
    Components = 'Components';
    Contents = 'Contents';
    Coords = 'Coords';
    Count = 'N';
    ContactInfo = 'ContactInfo';
    CountFull = 'Count';
    CourierBoldFontName = 'Courier-Bold';
    CourierBoldObliqueFontName = 'Courier-BoldOblique';
    CourierFontName = 'Courier';
    CourierNewBoldFontName = 'CourierNew,Bold';
    CourierNewFontName = 'CourierNew';
    CourierNewFontName2 = 'Courier New';
    CourierNewItalicFontName = 'CourierNew,Italic';
    CourierObliqueFontName = 'Courier-Oblique';
    CreationDate = 'CreationDate';
    Creator = 'Creator';
    CropBox = 'CropBox';
    CryptFilterMode = 'CFM';
    CryptFilters = 'CF';
    Decode = 'Decode';
    DecodeParameters = 'DecodeParms';
    DefaultWidth = 'DW';
    Destination = 'Dest';
    DescendantFonts = 'DescendantFonts';
    Destinations = 'Dests';
    DictionaryAppearance = 'DA';
    DictionaryResources = 'DR';
    Differences = 'Differences';
    DisplayDuration = 'Dur';
    DocumentInfo = 'Info';
    Domain = 'Domain';
    EarlyChange = 'EarlyChange';
    EmbeddedFile = 'EmbeddedFile';
    EmbeddedFileReference = 'EF';
    EmbeddedFiles = 'EmbeddedFiles';
    EmbeddedFilesCryptFilter = 'EFF';
    Encode = 'Encode';
    EncodedOwnerPassword = 'OE';
    EncodedUserPassword = 'UE';
    Encoding = 'Encoding';
    Encrypt = 'Encrypt';
    EncryptedPermissions = 'Perms';
    EncryptMetadata = 'EncryptMetadata';
    EndStream = 'endstream';
    Extend = 'Extend';
    ExtensionLevel = 'ExtensionLevel';
    ExtGState = 'ExtGState';
    EOF = '%%EOF';
    FDF = 'FDF';
    FDFField = 'T';
    FieldFlags = 'Ff';
    FieldOptions = 'Opt';
    FieldType = 'FT';
    Fields = 'Fields';
    FileAttachmentAnnotationDesc = 'Desc';
    FileAttachmentAnnotationName = 'Name';
    FileCreationDate = 'CreationDate';
    FileData = 'F';
    FileDescription = 'Desc';
    FileIndex = 'Index';
    FileModificationDate = 'ModDate';
    FileName = 'F';
    FileParameters = 'Params';
    FileSize = 'Size';
    FileSpec = 'Filespec';
    FileSystem = 'FS';
    Filter = 'Filter';
    FirstChar = 'FirstChar';
    FlateDecode = 'FlateDecode';
    FlatnessTolerance = 'FL';
    Font = 'Font';
    FontDescriptor = 'FontDescriptor';
    FontDescriptorAscent = 'Ascent';
    FontDescriptorAvgWidth = 'AvgWidth';
    FontDescriptorBBox = 'FontBBox';
    FontDescriptorCapHeight = 'CapHeight';
    FontDescriptorCharSet = 'CharSet';
    FontDescriptorCIDSet = 'CIDSet';
    FontDescriptorDescent = 'Descent';
    FontDescriptorFamily = 'FontFamily';
    FontDescriptorFlags = 'Flags';
    FontDescriptorItalicAngle = 'ItalicAngle';
    FontDescriptorLeading = 'Leading';
    FontDescriptorMaxWidth = 'MaxWidth';
    FontDescriptorMissingWidth = 'MissingWidth';
    FontDescriptorStemH = 'StemH';
    FontDescriptorStemV = 'StemV';
    FontDescriptorWeight = 'FontWeight';
    FontDescriptorWeightNormal = 400;
    FontDescriptorWeightRegular = 'Regular';
    FontDescriptorXHeight = 'XHeight';
    FontFile = 'FontFile';
    FontFile2 = 'FontFile2';
    FontFile3 = 'FontFile3';
    FontFileDictionaryKey = 'FontFile';
    FontMatrix = 'FontMatrix';
    FontMMType1 = 'MMType1';
    FontMTSuffix = 'MT';
    FontName = 'FontName';
    FontStretch = 'FontStretch';
    FontType0 = 'Type0';
    FontType1 = 'Type1';
    FontType3 = 'Type3';
    Form = 'Form';
    FormType = 'FormType';
    FreeTextAnnotation = 'FreeText';
    &Function = 'Function';
    FunctionType = 'FunctionType';
    Functions = 'Functions';
    Gamma = 'Gamma';
    Group = 'Group';
    GroupType = 'G';
    Height = 'Height';
    HelveticaBoldFontName = 'Helvetica-Bold';
    HelveticaBoldObliqueFontName = 'Helvetica-BoldOblique';
    HelveticaFontName = 'Helvetica';
    HelveticaObliqueFontName = 'Helvetica-Oblique';
    HighlightAnnotation = 'Highlight';
    ID = 'ID';
    Identity = 'Identity';
    IdentityH = 'Identity-H';
    IdentityV = 'Identity-V';
    InlineImageBegin = 'BI';
    InlineImageEnd = 'EI';
    ImageMask = 'ImageMask';
    InlineImageData = 'ID';
    Info = 'Info';
    InkAnnotation = 'Ink';
    Intent = 'Intent';
    Interpolate = 'Interpolate';
    Italic = 'Italic';
    JBIG2Globals = 'JBIG2Globals';
    Keywords = 'Keywords';
    Kids = 'Kids';
    LastChar = 'LastChar';
    LastModified = 'LastModified';
    Length = 'Length';
    Length1 = 'Length1';
    Length2 = 'Length2';
    Length3 = 'Length3';
    LineAnnotation = 'Line';
    LineCap = 'LC';
    LineJoinStyle = 'LJ';
    LineStyle = 'D';
    LineWidth = 'LW';
    Location = 'Location';
    Lock = 'Lock';
    LZWDecode = 'LZWDecode';
    Marked = 'Marked';
    MarkupAnnotationCreationDate = 'CreationDate';
    MarkupAnnotationIntent = 'IT';
    MarkupAnnotationOpacity = 'CA';
    MarkupAnnotationRichText = 'RC';
    MarkupAnnotationSubject = 'Subj';
    MarkupAnnotationTitle = 'T';
    Mask = 'Mask';
    MaskStyle = 'S';
    Matrix = 'Matrix';
    Matte = 'Matte';
    MaxLength = 'MaxLen';
    Metadata = 'Metadata';
    MediaBox = 'MediaBox';
    MiterLimit = 'ML';
    ModDate = 'ModDate';
    Modified = 'M';
    Name = 'Name';
    Names = 'Names';
    NeedAppearances = 'NeedAppearances';
    NonStrokingColorAlpha = 'ca';
    ObjectStream = 'ObjStm';
    Oblique = 'Oblique';
    OpenAction = 'OpenAction';
    OpenTypeFont = 'OpenType';
    Order = 'Order';
    Ordering = 'Ordering';
    Outline = 'Outline';
    OutlineAction = 'A';
    OutlineColor = 'C';
    OutlineCount = 'Count';
    OutlineDestination = Destination;
    OutlineFirst = 'First';
    OutlineFlags = 'F';
    OutlineLast = 'Last';
    OutlineNext = 'Next';
    OutlinePrev = 'Prev';
    Outlines = 'Outlines';
    OutlineTitle = 'Title';
    OwnerPasswordHash = 'O';
    Page = 'Page';
    Pages = 'Pages';
    PaintType = 'PaintType';
    Parent = 'Parent';
    Pattern = 'Pattern';
    PatternType = 'PatternType';
    Permissions = 'P';
    PolygonAnnotation = 'Polygon';
    PolyLineAnnotation = 'PolyLine';
    Predictor = 'Predictor';
    PreferredZoom = 'PZ';
    Process = 'Process';
    Producer = 'Producer';
    Reason = 'Reason';
    Range = 'Range';
    Rect = 'Rect';
    RedactAnnotation = 'Redact';
    Registry = 'Registry';
    Resources = 'Resources';
    Revision = 'R';
    Root = 'Root';
    Rotate = 'Rotate';
    Shading = 'Shading';
    ShadingType = 'ShadingType';
    ShortAction = 'A';
    ShortBitsPerComponent = 'BPC';
    ShortColorSpace = 'CS';
    ShortDecode = 'D';
    ShortDecodeParameters = 'DP';
    ShortDefaultValue = 'DV';
    ShortFilter = 'F';
    ShortHeight = 'H';
    ShortImageMask = 'IM';
    ShortInterpolate = 'I';
    ShortPage = 'P';
    ShortValue = 'V';
    ShortWidth = 'W';
    ShortWidths = 'W';
    SigFlags = 'SigFlags';
    Size = 'Size';
    SMaskInData = 'SMaskInData';
    SmoothnessTolerance = 'SM';
    SoftMask = 'SMask';
    SquareAnnotation = 'Square';
    SquigglyAnnotation = 'Squiggly';
    StampAnnotation = 'Stamp';
    Standard = 'Standard';
    StandardEncoding = 'StandardEncoding';
    StandardFilterName = 'StdCF';
    StartXRef = 'startxref';
    StdCF = 'StdCF';
    Stream = 'stream';
    StreamCryptFilter = 'StmF';
    StrikeOutAnnotation = 'StrikeOut';
    StringCryptFilter = 'StrF';
    StrokingColorAlpha = 'CA';
    StructParent = 'StructParent';
    StructParents = 'StructParents';
    Subject = 'Subject';
    SubFilter = 'SubFilter';
    Subtype = 'Subtype';
    Supplement = 'Supplement';
    Suspects = 'Suspects';
    SymbolFontName = 'Symbol';
    Text = 'T';
    TextAlternate = 'TU';
    TextAnnotation = 'Text';
    TextDefaultValue = 'DV';
    TextJustification = 'Q';
    TextKnockout = 'TK';
    TextMapping = 'TM';
    TextValue = 'V';
    TilingType = 'TilingType';
    TimesBoldFontName = 'Times-Bold';
    TimesBoldItalicFontName = 'Times-BoldItalic';
    TimesFontFamilyName = 'Times';
    TimesItalicFontName = 'Times-Italic';
    TimesNewRomanBoldFontName = 'TimesNewRoman,Bold';
    TimesNewRomanFontName = 'TimesNewRoman';
    TimesNewRomanFontName2 = 'Times New Roman';
    TimesNewRomanPSMTPrefix = 'TimesNewRomanPS';
    TimesRomanFontName = 'Times-Roman';
    Title = 'Title';
    ChoiceTopIndex = 'TI';
    ChoiceSelectedIndexes = 'I';
    ToUnicode = 'ToUnicode';
    Trailer = 'trailer';
    TransferFunction = 'TR';
    Transparency = 'Transparency';
    TrimBox = 'TrimBox';
    TrueType = 'TrueType';
    Type1C = 'Type1C';
    TypeKey = 'Type';
    UnderlineAnnotation = 'Underline';
    UseCMap = 'UseCMap';
    UserPasswordHash = 'U';
    UserProperties = 'UserProperties';
    UserUnit = 'UserUnit';
    Version = 'V';
    VersionSignature = '%PDF-';
    WhitePoint = 'WhitePoint';
    Width = 'Width';
    Widths = 'Widths';
    WinAnsiEncoding = 'WinAnsiEncoding';
    WMode = 'WMode';
    XFA = 'XFA';
    XObject = 'XObject';
    XObject2 = 'Xobject';
    XStep = 'XStep';
    XYZDestination = 'XYZ';
    YStep = 'YStep';
    ZapfDingbatsFontName = 'ZapfDingbats';
{$ENDREGION}
  end;

  { TdxPDFStream }

  TdxPDFStream = class(TdxPDFBaseStream)
  strict private
    FEncryptionInfo: IdxPDFEncryptionInfo;
    FDictionary: TdxPDFDictionary;

    function GetDecryptedData: TBytes;
    function GetUncompressedData: TBytes;
    procedure SetDictionary(const AValue: TdxPDFDictionary);
  protected
    class function GetObjectType: TdxPDFBaseType; override;
    procedure Write(AWriter: TdxPDFWriter); override;
    function UncompressData(ADecryptData: Boolean): TBytes;
  public
    constructor Create(const AData: TBytes; ADictionary: TdxPDFDictionary);
      reintroduce; overload;
    constructor Create(const AData: TBytes; ADictionary: TdxPDFDictionary;
      AEncryptionInfo: IdxPDFEncryptionInfo); reintroduce; overload;
    destructor Destroy; override;

    property DecryptedData: TBytes read GetDecryptedData;
    property Dictionary: TdxPDFDictionary read FDictionary write SetDictionary;
    property EncryptionInfo: IdxPDFEncryptionInfo read FEncryptionInfo
      write FEncryptionInfo;
    property UncompressedData: TBytes read GetUncompressedData;
  end;

  { TdxPDFTransformationMatrix }

  TdxPDFTransformationMatrix = record
  private
    FMatrix: TdxNullableValue<TXForm>;

    function GetA: Single;
    function GetB: Single;
    function GetC: Single;
    function GetD: Single;
    function GetE: Single;
    function GetF: Single;
    function GetDeterminant: Single;
    function GetIsInvertable: Boolean;
    function GetIsRotated: Boolean;
    function GetXForm: TXForm;
    procedure SetXForm(const AValue: TXForm);

    procedure DoMultiply(const AXForm: TXForm;
      AOrder: TdxTransformationOrder = moPrepend);
  public
    class function Create: TdxPDFTransformationMatrix; overload; static;
    class function Create(const M: TdxPDFTransformationMatrix)
      : TdxPDFTransformationMatrix; overload; static;
    class function Create(M11, M12, M21, M22, DX, DY: Single)
      : TdxPDFTransformationMatrix; overload; static;
    class function CreateRotate(ADegree: Single)
      : TdxPDFTransformationMatrix; static;
    class function CreateScale(AScaleX, AScaleY: Single)
      : TdxPDFTransformationMatrix; static;
    class function Invert(const M: TdxPDFTransformationMatrix)
      : TdxPDFTransformationMatrix; static;
    class function Multiply(const M1, M2: TdxPDFTransformationMatrix)
      : TdxPDFTransformationMatrix; overload; static;
    class function Null: TdxPDFTransformationMatrix; static;
    class function Rotate(const M: TdxPDFTransformationMatrix; ADegree: Single)
      : TdxPDFTransformationMatrix; static;
    class function Translate(const M: TdxPDFTransformationMatrix;
      const AOffset: TdxPointF): TdxPDFTransformationMatrix; overload; static;

    function IsIdentity: Boolean;
    function IsNull: Boolean;
    function Transform(const APoint: TdxPointF): TdxPointF;
    function TransformPoints(const APoints: TdxPDFPoints): TdxPDFPoints;
    procedure Assign(const M: TdxPDFTransformationMatrix); overload;
    procedure Assign(M11, M12, M21, M22, DX, DY: Single); overload;
    procedure Multiply(const M: TdxPDFTransformationMatrix;
      AOrder: TdxTransformationOrder = moPrepend); overload;
    procedure Reset;
    procedure Translate(DX, DY: Single;
      AOrder: TdxTransformationOrder = moPrepend); overload;
    procedure Write(AWriter: TdxPDFWriter);

    property A: Single read GetA;
    property B: Single read GetB;
    property C: Single read GetC;
    property D: Single read GetD;
    property E: Single read GetE;
    property F: Single read GetF;
    property IsInvertable: Boolean read GetIsInvertable;
    property IsRotated: Boolean read GetIsRotated;
    property XForm: TXForm read GetXForm write SetXForm;
  end;

  { TdxPDFDictionary }

  TdxPDFDictionary = class(TdxPDFBase) // for internal use
  strict private
    FEncryptionInfo: IdxPDFEncryptionInfo;
    FObjects: TdxPDFReferencedObjectDictionary;
    FStreamRef: TdxPDFStream;

    function GetItems: TdxPDFStringReferencedObjectDictionary;
    function GetValue(const AKey: string): TdxPDFBase;
    procedure SetValue(const AKey: string; AValue: TdxPDFBase);
    function TryGetValue<T: TdxPDFBase>(const AKey: string;
      out AValue: T): Boolean;
    function ParseDateTime(ADateTime: string): TDateTime;
    procedure ParseDateTimeComponent(var ADateTime: string;
      AComponent: PInteger; ADefaultValue: Byte = 0);
  protected
    class function GetObjectType: TdxPDFBaseType; override;
    function GetCount: Integer;
    procedure Write(AWriter: TdxPDFWriter); override;
    procedure WriteContent(AWriter: TdxPDFWriter);
    procedure WriteStream(AWriter: TdxPDFWriter); virtual;
    procedure WriteStreamData(AWriter: TdxPDFWriter; const AData: TBytes);
    property Items: TdxPDFStringReferencedObjectDictionary read GetItems;
  public
    constructor Create; override;
    destructor Destroy; override;

    function GetObject(const AKey: string): TdxPDFBase; virtual;

    function Contains(const AKey: string): Boolean;

    procedure Add(const AKey: string; AValue, ADefaultValue: Double); overload;
    procedure Add(const AKey: string; AValue, ADefaultValue: Integer); overload;
    procedure Add(const AKey: string; AValue, ADefaultValue: Boolean); overload;
    procedure Add(const AKey: string; AValue: Boolean); overload;
    procedure Add(const AKey: string; AValue: Double); overload;
    procedure Add(const AKey: string; AValue: Integer); overload;
    procedure Add(const AKey: string; AValue: TdxPDFBase); overload;
    procedure Add(const AKey: string; const AMatrix: TdxPDFTransformationMatrix;
      ASkipNullOrIdentity: Boolean = True); overload;
    procedure Add(const AKey: string;
      const AValue, ADefaultValue: TdxRectF); overload;
    procedure Add(const AKey: string; const AValue: string); overload;
    procedure Add(const AKey: string; const AValue: string;
      AEncoding: TEncoding); overload;
    procedure Add(const AKey: string; const AValue: TBytes); overload;
    procedure Add(const AKey: string; const AValue: TDoubleDynArray;
      ASkipIfNull: Boolean = True); overload;
    procedure Add(const AKey: string; const AValue: TdxPDFRanges); overload;
    procedure Add(const AKey: string; const AValue: TdxPDFRectangle;
      ASkipIfNull: Boolean = True); overload;
    procedure Add(const AKey: string; const AValue: TdxRectF;
      ACheckRect: Boolean = True; ASkipIfNull: Boolean = True); overload;
    procedure Add(const AKey: string; const AValue: TIntegerDynArray); overload;
    procedure Add(const AKey: string;
      const AValue: TdxPDFSignatureFlags); overload;
    procedure AddBytes(const AKey: string; const AValue: TBytes);
    procedure AddDate(const AKey: string; AValue: TDateTime);
    procedure AddName(const AKey: string; const AValue: string;
      ASkipIfNull: Boolean = True); overload;
    procedure AddName(const AKey: string;
      const AValue: TdxPDFRenderingIntent); overload;
    procedure AddReference(const AKey: string; ANumber: Integer;
      AGeneration: Integer = 0); overload;
    procedure AddReference(const AKey: string; const AData: TBytes;
      ASkipIfNull: Boolean = True); overload; virtual;
    procedure Clear;

    function CreateNumericList(const AKey: string): TDoubleDynArray;
    function GetArray(const AKey: string): TdxPDFArray; overload;
    function GetArray(const AKey, AAlternativeKey: string)
      : TdxPDFArray; overload;
    function GetArray(const AKey: string; out AArray: TdxPDFArray)
      : Boolean; overload;
    function GetBoolean(const AKey: string;
      ADefaultValue: Boolean = False): Boolean;
    function GetBytes(const AKey: string): TBytes;
    function GetDate(const AKey: string): TDateTime;
    function GetDictionary(const AKey: string): TdxPDFDictionary;
    function GetDouble(const AKey: string;
      ADefaultValue: Double = dxPDFInvalidValue): Double;
    function GetDoubleArray(const AKey: string): TDoubleDynArray;
    function GetInteger(const AKey, AAlternativeKey: string): Integer; overload;
    function GetInteger(const AKey: string;
      ADefaultValue: Integer = dxPDFInvalidValue): Integer; overload;
    function GetMatrix(const AKey: string): TdxPDFTransformationMatrix;
    function GetRectangle(const AKey: string): TdxRectF; overload;
    function GetRectangle(const AKey: string; const ADefaultValue: TdxRectF)
      : TdxRectF; overload;
    function GetRectangleEx(const AKey: string): TdxPDFRectangle;
    function GetRenderingIntent(const AKey: string): TdxPDFRenderingIntent;
    function GetSignatureFlags(const AKey: string): TdxPDFSignatureFlags;
    function GetSmallInt(const AKey: string; ADefaultValue: SmallInt): SmallInt;
    function GetStream(const AKey: string): TdxPDFStream;
    function GetString(const AKey, ADefaultValue: string): string; overload;
    function GetString(const AKey: string): string; overload;
    function GetTextString(const AKey: string): string;

    function TryGetArray(const AKey: string; out AValue: TdxPDFArray): Boolean;
    function TryGetBoolean(const AKey: string; out AValue: Boolean): Boolean;
    function TryGetDictionary(const AKey: string;
      out AValue: TdxPDFDictionary): Boolean;
    function TryGetObject(const AKey: string; out AValue: TdxPDFBase): Boolean;
    function TryGetReference(const AKey: string; out AValue: Integer): Boolean;
    function TryGetStream(const AKey: string; out AValue: TdxPDFStream)
      : Boolean;
    function TryGetString(const AKey: string; out AValue: string): Boolean;
    function TryGetTextString(const AKey: string; out AValue: string): Boolean;

    procedure Remove(const AKey: string);

    property Count: Integer read GetCount;
    property EncryptionInfo: IdxPDFEncryptionInfo read FEncryptionInfo;
    property StreamRef: TdxPDFStream read FStreamRef write FStreamRef;
    property Value[const AKey: string]: TdxPDFBase read GetValue
      write SetValue; default;
  end;

  { TdxPDFValue }

  TdxPDFValue = class(TdxPDFBase)
  strict private
    FValue: Variant;
  protected
    procedure InternalSetValue(const AValue: Variant);

    property InternalValue: Variant read FValue;
  public
    constructor Create(ANumber, AGeneration: Integer; const AValue: Variant);
      reintroduce; overload;
    constructor Create(const AValue: Variant); reintroduce; overload;
  end;

  { TdxPDFBoolean }

  TdxPDFBoolean = class(TdxPDFValue)
  strict private
    function GetValue: Boolean; inline;
    procedure SetValue(const AValue: Boolean);
  protected
    class function GetObjectType: TdxPDFBaseType; override;
    procedure Write(AWriter: TdxPDFWriter); override;
  public
    property Value: Boolean read GetValue write SetValue;
  end;

  { TdxPDFNull }

  TdxPDFNull = class(TdxPDFValue)
  strict private
    function GetValue: Variant; inline;
  protected
    procedure Write(AWriter: TdxPDFWriter); override;
  public
    property Value: Variant read GetValue;
  end;

  { TdxPDFCustomBytes }

  TdxPDFCustomBytes = class(TdxPDFValue)
  protected
    FData: TBytes;

    procedure Write(AWriter: TdxPDFWriter); override;
  public
    constructor Create(const AValue: TBytes); reintroduce;
  end;

  { TdxPDFBytes }

  TdxPDFBytes = class(TdxPDFCustomBytes)
  protected
    procedure Write(AWriter: TdxPDFWriter); override;
  end;

  { TdxPDFPlaceHolder }

  TdxPDFPlaceHolder = class(TdxPDFCustomBytes)
  strict private
    FOffset: Int64;
    function GetSize: Int64;
  protected
    procedure Write(AWriter: TdxPDFWriter); override;
  public
    constructor Create(ALength: Int64); reintroduce;
    function IsValid: Boolean;
    //
    property Size: Int64 read GetSize;
    property Offset: Int64 read FOffset;
  end;

  { TdxPDFSpecialBytes }

  TdxPDFSpecialBytes = class(TdxPDFCustomBytes);

  { TdxPDFString }

  TdxPDFString = class(TdxPDFValue)
  strict private
    function GetText: string;
    function GetValue: string;
    procedure SetValue(const AValue: string);
  protected
    class function GetObjectType: TdxPDFBaseType; override;
    procedure Write(AWriter: TdxPDFWriter); override;
  public
    property Text: string read GetText;
    property Value: string read GetValue write SetValue;
  end;

  { TdxPDFComment }

  TdxPDFComment = class(TdxPDFString)
  protected
    class function GetObjectType: TdxPDFBaseType; override;
    procedure Write(AWriter: TdxPDFWriter); override;
  end;

  { TdxPDFName }

  TdxPDFName = class(TdxPDFString)
  protected
    class function GetObjectType: TdxPDFBaseType; override;
    procedure Write(AWriter: TdxPDFWriter); override;
  end;

  { TdxPDFNumericObject }

  TdxPDFNumericObject = class(TdxPDFValue)
  protected
    class function GetObjectType: TdxPDFBaseType; override;
    procedure Write(AWriter: TdxPDFWriter); override;
    function GetValue: Variant; inline;
  end;

  { TdxPDFInteger }

  TdxPDFInteger = class(TdxPDFNumericObject)
  strict private
    function GetValue: Integer;
    procedure SetValue(const AValue: Integer);
  protected
    class function GetObjectType: TdxPDFBaseType; override;
    procedure Write(AWriter: TdxPDFWriter); override;
  public
    property Value: Integer read GetValue write SetValue;
  end;

  { TdxPDFDouble }

  TdxPDFDouble = class(TdxPDFNumericObject)
  strict private
    function GetValue: Double;
    procedure SetValue(AValue: Double);
  public
    property Value: Double read GetValue write SetValue;
  end;

  { TdxPDFReference }

  TdxPDFReference = class(TdxPDFBase)
  strict private
    FIsFree: Boolean;
    FIsSlot: Boolean;
    FOffset: Int64;
  protected
    class function GetObjectType: TdxPDFBaseType; override;
    procedure Write(AWriter: TdxPDFWriter); override;
  public
    constructor Create(ANumber, AGeneration: Integer; AOffset: Int64 = 0;
      AIsFree: Boolean = False; AIsSlot: Boolean = False); reintroduce;

    property IsFree: Boolean read FIsFree;
    property IsSlot: Boolean read FIsSlot;
    property Offset: Int64 read FOffset write FOffset;
  end;

  { TdxPDFIndirectObject }

  TdxPDFIndirectObject = class(TdxPDFBase)
  strict private
    FData: TBytes;
  protected
    class function GetObjectType: TdxPDFBaseType; override;
  public
    constructor Create(ANumber, AGeneration: Integer; const AData: TBytes);
    destructor Destroy; override;

    property Data: TBytes read FData;
  end;

  { TdxPDFTokenDescription }

  TdxPDFTokenDescription = class
  strict private
    FCurrentComparingSymbol: Byte;
    FIndexToCompare: Integer;
    FSequence: TBytes;
    FSequenceLength: Integer;

    procedure BeginCompare; overload; inline;
  public
    constructor Create(const ADescription: string); overload;
    constructor Create(const ASequence: TBytes); overload;

    class function BeginCompare(AToken: TdxPDFTokenDescription)
      : TdxPDFTokenDescription; overload; inline;
    function IsStartWithComment: Boolean; inline;
    function Compare(ASymbol: Byte): Boolean; inline;

    property Sequence: TBytes read FSequence;
    property SequenceLength: Integer read FSequenceLength;
  end;

  { TdxPDFStreamElement }

  TdxPDFStreamElement = class(TdxPDFReference)
  strict private
    FIndex: Integer;
  protected
    class function GetObjectType: TdxPDFBaseType; override;
  public
    constructor Create(ANumber: Integer; AIndex: Integer);

    property Index: Integer read FIndex;
  end;

  { TdxPDFCustomRepository }

  TdxPDFCustomRepository = class(TdxPDFReferencedObject)
  strict private
    FReferences: TdxPDFBaseReferences;
    function GetMaxObjectNumber: Integer;
  protected
    function ResolveObject(ANumber: Integer): TdxPDFReferencedObject; virtual;
    procedure CreateSubClasses; virtual;
    procedure DestroySubClasses; virtual;

    procedure Replace(ANumber: Integer; AObject: TdxPDFReferencedObject);
    procedure TryAdd(ANumber: Integer; AObject: TdxPDFReferencedObject;
      ACanReplace: Boolean);

    property References: TdxPDFBaseReferences read FReferences;
  public
    constructor Create;
    destructor Destroy; override;

    function DecryptString(const S: string): string; virtual;

    function GetArray(ANumber: Integer): TdxPDFArray;
    function GetDictionary(ANumber: Integer): TdxPDFDictionary;
    function GetInteger(ANumber: Integer): TdxPDFInteger;
    function GetNumericObject(ANumber: Integer): TdxPDFNumericObject;
    function GetObject(ANumber: Integer): TdxPDFReferencedObject;
    function GetStream(ANumber: Integer): TdxPDFStream;
    function ResolveReference(AObject: TdxPDFBase): TdxPDFBase;

    procedure Add(ANumber: Integer; AObject: TdxPDFReferencedObject;
      ACanReplace: Boolean = True);
    procedure Clear; virtual;
    procedure Remove(ANumber: Integer);
    procedure TrimExcess;

    property MaxObjectNumber: Integer read GetMaxObjectNumber;
  end;

  { TdxPDFColor }

  TdxPDFColor = record
  strict private
    FComponents: TDoubleDynArray;
    FPattern: TdxPDFReferencedObject;
    FIsNull: Boolean;

    procedure AddComponent(const AValue: Double);
    procedure SetPattern(const AValue: TdxPDFReferencedObject);
  public
    class function Create: TdxPDFColor; overload; static;
    class function Create(APattern: TdxPDFReferencedObject;
      const AComponents: TDoubleDynArray): TdxPDFColor; overload; static;
    class function Create(const AArray: TdxPDFArray): TdxPDFColor;
      overload; static;
    class function Create(const AColor: TdxPDFColor): TdxPDFColor;
      overload; static;
    class function Create(const X, Y, Z, AWhitePointZ: Double): TdxPDFColor;
      overload; static;
    class function Create(const C1, C2, C3: Double): TdxPDFColor;
      overload; static;
    class function Create(const AComponents: TDoubleDynArray): TdxPDFColor;
      overload; static;
    class function ClipColorComponent(const AComponent: Double): Double; static;
    class function ColorComponentTransferFunction(const AComponent: Double)
      : Double; static;
    class function GetComponents(const X, Y, Z, AWhitePointZ: Double)
      : TDoubleDynArray; static;
    class function Null: TdxPDFColor; static;

    procedure Assign(const AColor: TdxPDFColor);
    procedure AssignAndTransferComponents(const AComponents: TDoubleDynArray);
    function IsNull: Boolean;

    property Components: TDoubleDynArray read FComponents;
    property Pattern: TdxPDFReferencedObject read FPattern write SetPattern;
  end;

  { TdxPDFARGBColor }

  TdxPDFARGBColor = record
  private
    FAlpha: Byte;
    FBlue: Double;
    FGreen: Double;
    FRed: Double;
  public
    class function Create(const AColor: TdxPDFColor): TdxPDFARGBColor;
      overload; static;
    class function CreateFromRGB(ARed, AGreen, ABlue: Double;
      AAlpha: Byte = 255): TdxPDFARGBColor; overload; static;
    class function CreateFromCMYK(ACyan, AMagenta, AYellow, ABlack: Double)
      : TdxPDFARGBColor; overload; static;
    class function Convert(const AColor: TdxPDFARGBColor): TdxPDFColor; static;
    class function ConvertToBytes(ACyan, AMagenta, AYellow, ABlack: Byte)
      : TBytes; inline; static;
    class function ConvertToRGB(const AData: TBytes;
      APixelFormat: TdxPDFPixelFormat): TBytes; static;

    property Alpha: Byte read FAlpha;
    property Blue: Double read FBlue;
    property Green: Double read FGreen;
    property Red: Double read FRed;
  end;

  TdxPDFARGBColorArray = array of TdxPDFARGBColor;

  { TdxPDFCustomProperties }

  TdxPDFCustomProperties = class(TdxPDFBase)
  strict private
    FDictionary: TdxPDFDictionary;

    procedure SetDictionary(const AValue: TdxPDFDictionary);
  protected
    procedure Write(AWriter: TdxPDFWriter); override;
    //
    property Dictionary: TdxPDFDictionary read FDictionary write SetDictionary;
  public
    constructor Create(ADictionary: TdxPDFDictionary);
    destructor Destroy; override;
  end;

  { TdxPDFBlendModeDictionary }

  TdxPDFBlendModeDictionary = class
  strict private
  const
    Map: array [TdxPDFBlendMode] of string = ('Normal', 'Compatible',
      'Multiply', 'Screen', 'Overlay', 'Darken', 'Lighten', 'ColorDodge',
      'ColorBurn', 'HardLight', 'SoftLight', 'Difference', 'Exclusion', 'Hue',
      'Saturation', 'Color', 'Luminosity');
  public
    class function ToString(AValue: TdxPDFBlendMode): string; reintroduce;
    class function ToValue(const AString: string): TdxPDFBlendMode;
    class function TryGetValue(const AKey: string;
      out AValue: TdxPDFBlendMode): Boolean;
  end;

  { TdxPDFCommandOperandStack }

  TdxPDFCommandOperandStack = class(TdxPDFReferencedObject) // for internal use
  strict private
    FStack: TObjectStack<TdxPDFBase>;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    function TryPopLastName: string;
    function PopAsArray: TdxPDFArray;
    function PopAsBytes: TBytes;
    function PopAsInteger: Integer;
    function PopAsObject: TdxPDFBase;
    function PopAsSingle: Single;
    function PopAsString: string;
    procedure Clear;
    procedure Push(AObject: TdxPDFBase);

    property Count: Integer read GetCount;
  end;

  { TdxPDFFontMetricsMetadata }

  TdxPDFFontMetricsMetadata = record
  strict private
    FAscent: Double;
    FDescent: Double;
    FEmSize: Double;
    FIsNull: Boolean;
    function GetHeight: Double;
  public
    class function Create: TdxPDFFontMetricsMetadata; overload; static;
    class function Create(AAscent, ADescent, AEmSize: Double)
      : TdxPDFFontMetricsMetadata; overload; static;
    function IsNull: Boolean;

    property Ascent: Double read FAscent;
    property Descent: Double read FDescent;
    property EmSize: Double read FEmSize;
    property Height: Double read GetHeight;
  end;

function dxPDFChangeValue(AValue: TdxPDFReferencedObject;
  var ACurrentValue: TdxPDFReferencedObject): Boolean; inline;
function dxPDFGenerateDocumentID: TdxPDFDocumentID;
procedure dxPDFFreeObject(AObject: TdxPDFReferencedObject);

implementation

uses
  StrUtils, Variants, dxCharacters, dxStringHelper, cxDateUtils, dxGDIPlusAPI,
  dxPDFCore, dxPDFStreamFilter, dxPDFUtils,
  dxHash;

type
  TdxPDFBaseAccess = class(TdxPDFBase);

function dxPDFChangeValue(AValue: TdxPDFReferencedObject;
  var ACurrentValue: TdxPDFReferencedObject): Boolean;
var
  APreviousValue: TdxPDFReferencedObject;
begin
  Result := ACurrentValue <> AValue;
  if Result then
  begin
    APreviousValue := ACurrentValue;
    ACurrentValue := AValue;

    if ACurrentValue <> nil then
      ACurrentValue.Reference;

    if APreviousValue <> nil then
      APreviousValue.Release;
  end;
end;

function dxPDFGenerateDocumentID: TdxPDFDocumentID;

  function Generate: TBytes;
  var
    I: Integer;
    S: string;
  begin
    SetLength(Result, 16);
    S := LowerCase(dxMD5CalcStr(dxGenerateGUID));
    for I := 0 to 15 do
      Result[I] := StrToInt('$' + S[2 * I + 1] + S[2 * (I + 1)]);
  end;

begin
  Result[0] := Generate;
  Result[1] := Generate;
end;

procedure dxPDFFreeObject(AObject: TdxPDFReferencedObject);
begin
  dxPDFChangeValue(nil, AObject);
end;

{ TdxPDFOrientedRectHelper }

class function TdxPDFOrientedRectHelper.Create: TdxPDFOrientedRect;
begin
  Result := Invalid;
end;

class function TdxPDFOrientedRectHelper.Create(const ATopLeft: TdxPointF;
  AWidth, AHeight, AAngle: Single): TdxPDFOrientedRect;
begin
  Result.Angle := TdxPDFUtils.NormalizeAngle(AAngle);
  Result.Top := ATopLeft.Y;
  Result.Left := ATopLeft.X;
  Result.Width := AWidth;
  Result.Height := AHeight;
end;

class function TdxPDFOrientedRectHelper.Create(const ARect: TdxRectF)
  : TdxPDFOrientedRect;
begin
  Result := Create(ARect, 0);
end;

class function TdxPDFOrientedRectHelper.Create(const ARect: TdxPDFRectangle)
  : TdxPDFOrientedRect;
begin
  Result := Create(TdxRectF.Create(ARect.Left, ARect.Bottom, ARect.Right,
    ARect.Top), 0);
end;

class function TdxPDFOrientedRectHelper.Create(const ARect: TdxRectF;
  AAngle: Single): TdxPDFOrientedRect;
begin
  Result.Angle := AAngle;
  Result.Top := ARect.Top;
  Result.Left := ARect.Left;
  Result.Width := Abs(ARect.Width);
  Result.Height := Abs(ARect.Height);
end;

class function TdxPDFOrientedRectHelper.Invalid: TdxPDFOrientedRect;
begin
  Result.Angle := -1;
  Result.Top := -1;
  Result.Left := -1;
  Result.Width := -1;
  Result.Height := -1;
end;

function TdxPDFOrientedRectHelper.GetTopLeft: TdxPointF;
begin
  Result := dxPointF(Left, Top);
end;

function TdxPDFOrientedRectHelper.GetBottom: Single;
begin
  Result := Top + Width * Sin(Angle) - Height * Cos(Angle);
end;

function TdxPDFOrientedRectHelper.GetRect: TdxRectF;
begin
  Result.Left := Left;
  Result.Top := Bottom;
  Result.Width := Width;
  Result.Height := Height;
end;

function TdxPDFOrientedRectHelper.GetRight: Single;
begin
  Result := Left + Width * Cos(Angle) + Height * Sin(Angle);
end;

function TdxPDFOrientedRectHelper.GetRotatedRect: TdxRectF;
var
  ARealTopLeft, ARotatedTopLeft, ARealTopRight, ARealBottomLeft,
    ARealBottomRight: TdxPointF;
begin
  ARealTopLeft := TopLeft;
  ARotatedTopLeft := TdxPDFTextUtils.RotatePoint(ARealTopLeft, -Angle);
  ARealTopRight := TdxPDFTextUtils.RotatePoint
    (dxPointF(ARotatedTopLeft.X + Width, ARotatedTopLeft.Y), Angle);
  ARealBottomLeft := TdxPDFTextUtils.RotatePoint(dxPointF(ARotatedTopLeft.X,
    ARotatedTopLeft.Y - Height), Angle);
  ARealBottomRight := TdxPDFTextUtils.RotatePoint
    (dxPointF(ARotatedTopLeft.X + Width, ARotatedTopLeft.Y - Height), Angle);

  Result.Left := Min(Min(ARealTopLeft.X, ARealTopRight.X),
    Min(ARealBottomLeft.X, ARealBottomRight.X));
  Result.Top := Min(Min(ARealTopLeft.Y, ARealTopRight.Y),
    Min(ARealBottomLeft.Y, ARealBottomRight.Y));
  Result.Right := Max(Max(ARealTopLeft.X, ARealTopRight.X),
    Max(ARealBottomLeft.X, ARealBottomRight.X));
  Result.Bottom := Max(Max(ARealTopLeft.Y, ARealTopRight.Y),
    Max(ARealBottomLeft.Y, ARealBottomRight.Y));
end;

function TdxPDFOrientedRectHelper.GetTopRight: TdxPointF;
var
  ARotatedTopLeft: TdxPointF;
begin
  ARotatedTopLeft := TdxPDFTextUtils.RotatePoint(TopLeft, -Angle);
  Result := TdxPDFTextUtils.RotatePoint(dxPointF(ARotatedTopLeft.X + Width,
    ARotatedTopLeft.Y), Angle);
end;

function TdxPDFOrientedRectHelper.IsValid: Boolean;
var
  AInvalidRect: TdxPDFOrientedRect;
begin
  AInvalidRect := Invalid;
  Result := (AInvalidRect.Angle <> Angle) and (AInvalidRect.Height <> Angle) and
    (AInvalidRect.Width <> Width);
end;

function TdxPDFOrientedRectHelper.Overlap(const R: TdxPDFOrientedRect): Boolean;
const
  Distance = 1;
begin
  Result := (Abs(R.Left - Left) < Distance) and (Abs(R.Top - Top) < Distance)
    and (Abs(R.Width - Width) < Distance) and
    (Abs(R.Height - Height) < Distance) and (Angle = R.Angle);
end;

function TdxPDFOrientedRectHelper.PtInRect(const APoint: TdxPointF;
  AExpandX: Single = 0; AExpandY: Single = 0): Boolean;
var
  ARotatedPoint, ARotatedTopLeft: TdxPointF;
begin
  ARotatedPoint := TdxPDFTextUtils.RotatePoint(APoint, -Angle);
  ARotatedTopLeft := TdxPDFTextUtils.RotatePoint(TopLeft, -Angle);
  Result := (((ARotatedPoint.X >= (ARotatedTopLeft.X - AExpandX)) and
    (ARotatedPoint.X <= (ARotatedTopLeft.X + Width + AExpandX))) and
    (ARotatedPoint.Y <= (ARotatedTopLeft.Y + AExpandY))) and
    (ARotatedPoint.Y >= (ARotatedTopLeft.Y - Height - AExpandY));
end;

{ TdxPDFDocumentAreaHelper }

class function TdxPDFDocumentAreaHelper.Create(APageIndex: Integer;
  const R: TdxRectF): TdxPDFDocumentArea;
begin
  Result.Rect := R;
  Result.PageIndex := APageIndex;
end;

class function TdxPDFDocumentAreaHelper.Create(const P1, P2: TdxPDFPosition)
  : TdxPDFDocumentArea;
var
  APoint1, APoint2: TdxPointF;
begin
  try
    if P1.PageIndex <> P2.PageIndex then
      Exit(Empty);
    APoint1 := P1.Point;
    APoint2 := P2.Point;
    Exit(TdxPDFDocumentArea.Create(P1.PageIndex,
      dxRectF(TdxPDFUtils.Min(APoint1.X, APoint2.X), TdxPDFUtils.Min(APoint1.Y,
      APoint2.Y), TdxPDFUtils.Max(APoint1.X, APoint2.X),
      TdxPDFUtils.Max(APoint1.Y, APoint2.Y))));
  except
    Exit(Empty)
  end;
end;

class function TdxPDFDocumentAreaHelper.Empty: TdxPDFDocumentArea;
begin
  Result.Rect := dxNullRectF;
  Result.PageIndex := -1;
end;

{ TdxPDFPositionHelper }

class function TdxPDFPositionHelper.Create: TdxPDFPosition;
begin
  Result.Invalid;
end;

class function TdxPDFPositionHelper.Create(APageNumber: Integer;
  const P: TdxPointF): TdxPDFPosition;
begin
  Result.PageIndex := APageNumber;
  Result.Point := P;
end;

function TdxPDFPositionHelper.NearTo(const APosition: TdxPDFPosition): Boolean;
const
  NearDistance = 3;
var
  AOtherPoint: TdxPointF;
begin
  AOtherPoint := APosition.Point;
  Result := (PageIndex = APosition.PageIndex) and
    (Abs(Point.X - AOtherPoint.X) <= NearDistance) and
    (Abs(Point.Y - AOtherPoint.Y) <= NearDistance);
end;

function TdxPDFPositionHelper.IsValid: Boolean;
begin
  Result := PageIndex >= 0;
end;

procedure TdxPDFPositionHelper.Invalid;
begin
  PageIndex := -1;
  Point := dxNullPointF;
end;

{ TdxPDFFixedPointNumber }

class function TdxPDFFixedPointNumber.Create(AValue: Single)
  : TdxPDFFixedPointNumber;
begin
  Result.FValue := Trunc(AValue * Result.FloatToFixedFactor);
end;

class operator TdxPDFFixedPointNumber.Add(const A, B: TdxPDFFixedPointNumber)
  : TdxPDFFixedPointNumber;
begin
  Result := TdxPDFFixedPointNumber.Create(A.FValue + B.FValue);
end;

class operator TdxPDFFixedPointNumber.Multiply(A: Integer;
  const B: TdxPDFFixedPointNumber): TdxPDFFixedPointNumber;
begin
  Result := TdxPDFFixedPointNumber.Create(A * B.FValue);
end;

class function TdxPDFFixedPointNumber.Create(AValue: Integer)
  : TdxPDFFixedPointNumber;
begin
  Result.FValue := AValue;
end;

function TdxPDFFixedPointNumber.RoundToByte: Byte;
var
  AActualValue: Integer;
begin
  AActualValue := (FValue + Half) shr FractionPartSize;
  Result := IfThen(AActualValue > 255, 255, AActualValue);
end;

{ TdxPDFNamedObjectDictionary }

constructor TdxPDFNamedObjectDictionary.Create(const AResourceKey: string;
  const APrefix: string);
begin
  inherited Create;
  FDictionary := TdxPDFStringIntegerDictionary.Create;
  FNextResourceNumber := 0;
  FResourceKey := AResourceKey;
  FPrefix := APrefix;
end;

destructor TdxPDFNamedObjectDictionary.Destroy;
begin
  FreeAndNil(FDictionary);
  inherited Destroy;
end;

function TdxPDFNamedObjectDictionary.GetNewResourceName
  (ADictionary: TdxPDFReferencedObjectDictionary): string;
begin
  Result := GetNewResourceName(ADictionary,
    FPrefix + IntToStr(FNextResourceNumber));
  Inc(FNextResourceNumber);
end;

function TdxPDFNamedObjectDictionary.GetNewResourceName
  (ADictionary: TdxPDFReferencedObjectDictionary; const AName: string): string;
begin
  Result := AName;
  while ADictionary.ContainsKey(Result) do
  begin
    Result := FPrefix + IntToStr(FNextResourceNumber);
    Inc(FNextResourceNumber);
  end;
end;

function TdxPDFNamedObjectDictionary.ContainsValue(AValue: Integer): Boolean;
begin
  Result := FDictionary.ContainsValue(AValue)
end;

procedure TdxPDFNamedObjectDictionary.ClearResourceNames;
begin
  FDictionary.Clear;
  FNextResourceNumber := 0;
end;

{ TdxPDFArray }

constructor TdxPDFArray.Create;
begin
  inherited Create;
  FElementList := TdxPDFBaseList.Create;
end;

constructor TdxPDFArray.Create(const AValues: array of Integer);
begin
  Create;
  Add(AValues);
end;

constructor TdxPDFArray.Create(const AValues: array of Single);
begin
  Create;
  Add(AValues);
end;

constructor TdxPDFArray.Create(const AValues: array of string);
begin
  Create;
  Add(AValues);
end;

destructor TdxPDFArray.Destroy;
begin
  FreeAndNil(FElementList);
  inherited Destroy;
end;

procedure TdxPDFArray.Add(const AValue: TdxPDFBase);
begin
  FElementList.Add(AValue);
end;

procedure TdxPDFArray.Add(const AValue: TdxPDFRange);
begin
  Add(AValue.Min);
  Add(AValue.Max);
end;

procedure TdxPDFArray.Add(const AValue: Integer);
begin
  Add(TdxPDFInteger.Create(AValue));
end;

procedure TdxPDFArray.Add(const AValue: Double);
begin
  Add(TdxPDFDouble.Create(AValue));
end;

procedure TdxPDFArray.Add(const AValue: Single);
begin
  Add(TdxPDFDouble.Create(AValue));
end;

procedure TdxPDFArray.Add(const AValue: string);
begin
  Add(TdxPDFString.Create(AValue));
end;

procedure TdxPDFArray.AddReference(ANumber, AGeneration: Integer);
begin
  FElementList.Add(TdxPDFReference.Create(ANumber, 0));
end;

procedure TdxPDFArray.Add(const AValue: array of Integer);
var
  ACurrentValue: Integer;
begin
  for ACurrentValue in AValue do
    Add(ACurrentValue);
end;

procedure TdxPDFArray.Add(const AValue: array of Single);
var
  ACurrentValue: Single;
begin
  for ACurrentValue in AValue do
    Add(ACurrentValue);
end;

procedure TdxPDFArray.Add(const AValue: array of string);
var
  ACurrentValue: string;
begin
  for ACurrentValue in AValue do
    Add(ACurrentValue);
end;

procedure TdxPDFArray.Add(const AValue: array of Double);
var
  ACurrentValue: Double;
begin
  for ACurrentValue in AValue do
    Add(ACurrentValue);
end;

procedure TdxPDFArray.Add(const AValue: Boolean);
begin
  Add(TdxPDFBoolean.Create(AValue));
end;

procedure TdxPDFArray.AddBytes(const AValue: TBytes);
begin
  Add(TdxPDFBytes.Create(AValue));
end;

procedure TdxPDFArray.AddName(const AValue: string);
begin
  Add(TdxPDFName.Create(AValue));
end;

procedure TdxPDFArray.AddNull;
begin
  Add(TdxPDFNull.Create);
end;

procedure TdxPDFArray.Clear;
begin
  ElementList.Clear;
end;

class function TdxPDFArray.GetObjectType: TdxPDFBaseType;
begin
  Result := otArray;
end;

procedure TdxPDFArray.Write(AWriter: TdxPDFWriter);
var
  I: Integer;
begin
  AWriter.WriteOpenBracket;
  for I := 0 to ElementList.Count - 1 do
  begin
    if I > 0 then
      AWriter.WriteSpace;
    TdxPDFBaseAccess(ElementList[I]).Write(AWriter);
  end;
  AWriter.WriteCloseBracket;
end;

function TdxPDFArray.GetElement(AIndex: Integer): TdxPDFBase;
begin
  Result := FElementList[AIndex];
end;

function TdxPDFArray.GetCount: Integer;
begin
  Result := FElementList.Count;
end;

procedure TdxPDFArray.SetElement(Index: Integer; const Value: TdxPDFBase);
begin
  FElementList[Index] := Value;
end;

{ TdxPDFStream }

constructor TdxPDFStream.Create(const AData: TBytes;
  ADictionary: TdxPDFDictionary);
begin
  Create(AData, ADictionary, nil);
end;

constructor TdxPDFStream.Create(const AData: TBytes;
  ADictionary: TdxPDFDictionary; AEncryptionInfo: IdxPDFEncryptionInfo);
begin
  inherited Create(AData);
  Dictionary := ADictionary;
  EncryptionInfo := AEncryptionInfo;
end;

destructor TdxPDFStream.Destroy;
begin
  Dictionary := nil;
  inherited Destroy;
end;

class function TdxPDFStream.GetObjectType: TdxPDFBaseType;
begin
  Result := otStream;
end;

procedure TdxPDFStream.Write(AWriter: TdxPDFWriter);
begin
  Dictionary.Write(AWriter);
end;

function TdxPDFStream.UncompressData(ADecryptData: Boolean): TBytes;
var
  AData: TBytes;
  AFilters: TdxPDFStreamFilters;
  I: Integer;
begin
  if ADecryptData then
    Result := DecryptedData
  else
    Result := Data;

  AFilters := dxPDFCreateFilterList(Dictionary);
  try
    if AFilters.Count > 0 then
    begin
      AData := Result;
      SetLength(Result, 0);
      for I := 0 to AFilters.Count - 1 do
      begin
        TdxPDFUtils.AddData(AFilters[I].Decode(AData), Result);
        SetLength(AData, 0);
        TdxPDFUtils.AddData(Result, AData);
        SetLength(Result, 0);
      end;
      Result := AData;
    end;
  finally
    AFilters.Free;
  end;
end;

function TdxPDFStream.GetDecryptedData: TBytes;
begin
  if (EncryptionInfo <> nil) and (Length(Data) > 0) then
    Result := EncryptionInfo.Decrypt(Data, Number)
  else
    Result := Data;
end;

function TdxPDFStream.GetUncompressedData: TBytes;
begin
  Result := UncompressData(True);
end;

procedure TdxPDFStream.SetDictionary(const AValue: TdxPDFDictionary);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FDictionary))
end;

{ TdxPDFTransformationMatrix }

class function TdxPDFTransformationMatrix.Create: TdxPDFTransformationMatrix;
begin
  Result.FMatrix := TXForm.CreateIdentityMatrix;
end;

class function TdxPDFTransformationMatrix.Create
  (const M: TdxPDFTransformationMatrix): TdxPDFTransformationMatrix;
begin
  Result.FMatrix := TXForm.CreateMatrix(M.A, M.B, M.C, M.D, M.E, M.F);
end;

class function TdxPDFTransformationMatrix.Create(M11, M12, M21, M22, DX,
  DY: Single): TdxPDFTransformationMatrix;
begin
  Result.Assign(M11, M12, M21, M22, DX, DY);
end;

class function TdxPDFTransformationMatrix.CreateRotate(ADegree: Single)
  : TdxPDFTransformationMatrix;
var
  ARadians, ASin, ACos: Single;
begin
  ARadians := ADegree / (180 / PI);
  ASin := Sin(ARadians);
  ACos := Cos(ARadians);
  Result := Create(ACos, ASin, -ASin, ACos, 0, 0);
end;

class function TdxPDFTransformationMatrix.CreateScale(AScaleX, AScaleY: Single)
  : TdxPDFTransformationMatrix;
begin
  Result := TdxPDFTransformationMatrix.Create(AScaleX, 0, 0, AScaleY, 0, 0);
end;

class function TdxPDFTransformationMatrix.Invert
  (const M: TdxPDFTransformationMatrix): TdxPDFTransformationMatrix;
var
  ADeterminant: Single;
begin
  ADeterminant := M.GetDeterminant;
  Result := Create(M.D / ADeterminant, -M.B / ADeterminant, -M.C / ADeterminant,
    M.A / ADeterminant, (M.C * M.F - M.E * M.D) / ADeterminant,
    (M.B * M.E - M.F * M.A) / ADeterminant);
end;

class function TdxPDFTransformationMatrix.Multiply(const M1,
  M2: TdxPDFTransformationMatrix): TdxPDFTransformationMatrix;
var
  AMatrix1A, AMatrix1B, AMatrix1C, AMatrix1D, AMatrix1E, AMatrix1F, AMatrix2A,
    AMatrix2B, AMatrix2C, AMatrix2D: Single;
begin
  AMatrix1A := M1.A;
  AMatrix1B := M1.B;
  AMatrix1C := M1.C;
  AMatrix1D := M1.D;
  AMatrix1E := M1.E;
  AMatrix1F := M1.F;
  AMatrix2A := M2.A;
  AMatrix2B := M2.B;
  AMatrix2C := M2.C;
  AMatrix2D := M2.D;
  Result := Create;
  Result.Assign(AMatrix1A * AMatrix2A + AMatrix1B * AMatrix2C,
    AMatrix1A * AMatrix2B + AMatrix1B * AMatrix2D, AMatrix1C * AMatrix2A +
    AMatrix1D * AMatrix2C, AMatrix1C * AMatrix2B + AMatrix1D * AMatrix2D,
    AMatrix1E * AMatrix2A + AMatrix1F * AMatrix2C + M2.E,
    AMatrix1E * AMatrix2B + AMatrix1F * AMatrix2D + M2.F);
end;

class function TdxPDFTransformationMatrix.Null: TdxPDFTransformationMatrix;
begin
  Result.FMatrix.Reset;
end;

class function TdxPDFTransformationMatrix.Rotate
  (const M: TdxPDFTransformationMatrix; ADegree: Single)
  : TdxPDFTransformationMatrix;
begin
  Result := Multiply(CreateRotate(ADegree), M);
end;

class function TdxPDFTransformationMatrix.Translate
  (const M: TdxPDFTransformationMatrix; const AOffset: TdxPointF)
  : TdxPDFTransformationMatrix;
begin
  Result := Create(M.A, M.B, M.C, M.D, M.E + AOffset.X, M.F + AOffset.Y);
end;

function TdxPDFTransformationMatrix.IsIdentity: Boolean;
var
  AXForm: TXForm;
begin
  AXForm := XForm;
  Result := (AXForm.eM11 = 1) and (AXForm.eM12 = 0) and (AXForm.eDx = 0) and
    (AXForm.eM21 = 0) and (AXForm.eM22 = 1) and (AXForm.eDy = 0);
end;

function TdxPDFTransformationMatrix.IsNull: Boolean;
begin
  Result := FMatrix.IsNull;
end;

function TdxPDFTransformationMatrix.Transform(const APoint: TdxPointF)
  : TdxPointF;
var
  AXForm: TXForm;
begin
  AXForm := XForm;
  Result.X := APoint.X * AXForm.eM11 + APoint.Y * AXForm.eM21 + AXForm.eDx;
  Result.Y := APoint.X * AXForm.eM12 + APoint.Y * AXForm.eM22 + AXForm.eDy;
end;

function TdxPDFTransformationMatrix.TransformPoints(const APoints: TdxPDFPoints)
  : TdxPDFPoints;
var
  ALength, I: Integer;
begin
  ALength := Length(APoints);
  SetLength(Result, ALength);
  for I := 0 to ALength - 1 do
    Result[I] := Transform(APoints[I]);
end;

procedure TdxPDFTransformationMatrix.Assign
  (const M: TdxPDFTransformationMatrix);
begin
  FMatrix := M.FMatrix;
end;

procedure TdxPDFTransformationMatrix.Assign(M11, M12, M21, M22, DX, DY: Single);
begin
  FMatrix := TXForm.CreateMatrix(M11, M12, M21, M22, DX, DY);
end;

procedure TdxPDFTransformationMatrix.Multiply
  (const M: TdxPDFTransformationMatrix;
  AOrder: TdxTransformationOrder = moPrepend);
begin
  DoMultiply(M.FMatrix, AOrder);
end;

procedure TdxPDFTransformationMatrix.Reset;
begin
  FMatrix := TXForm.CreateIdentityMatrix;
end;

procedure TdxPDFTransformationMatrix.Translate(DX, DY: Single;
  AOrder: TdxTransformationOrder = moPrepend);
begin
  DoMultiply(TXForm.CreateTranslateMatrix(DX, DY), AOrder);
end;

procedure TdxPDFTransformationMatrix.Write(AWriter: TdxPDFWriter);
begin
  AWriter.WriteSpace;
  AWriter.WriteDouble(A);
  AWriter.WriteSpace;
  AWriter.WriteDouble(B);
  AWriter.WriteSpace;
  AWriter.WriteDouble(C);
  AWriter.WriteSpace;
  AWriter.WriteDouble(D);
  AWriter.WriteSpace;
  AWriter.WriteDouble(E);
  AWriter.WriteSpace;
  AWriter.WriteDouble(F);
end;

function TdxPDFTransformationMatrix.GetA: Single;
begin
  Result := XForm.eM11;
end;

function TdxPDFTransformationMatrix.GetB: Single;
begin
  Result := XForm.eM12;
end;

function TdxPDFTransformationMatrix.GetC: Single;
begin
  Result := XForm.eM21;
end;

function TdxPDFTransformationMatrix.GetD: Single;
begin
  Result := XForm.eM22;
end;

function TdxPDFTransformationMatrix.GetE: Single;
begin
  Result := XForm.eDx;
end;

function TdxPDFTransformationMatrix.GetF: Single;
begin
  Result := XForm.eDy;
end;

function TdxPDFTransformationMatrix.GetDeterminant: Single;
begin
  Result := A * D - B * C;
end;

function TdxPDFTransformationMatrix.GetIsInvertable: Boolean;
var
  AMax: Single;
begin
  AMax := TdxPDFUtils.Max(Abs(C * F - E * D), Abs(B * E - F * A));
  AMax := TdxPDFUtils.Max(AMax, TdxPDFUtils.Max(Abs(A), Abs(B)));
  AMax := TdxPDFUtils.Max(AMax, TdxPDFUtils.Max(Abs(C), Abs(D)));
  Result := (AMax + GetDeterminant <> AMax);
end;

function TdxPDFTransformationMatrix.GetIsRotated: Boolean;

  function IsZeroComponent(AComponent: Single): Boolean;
  begin
    Result := Abs(AComponent) < 1E-6;
  end;

begin
  Result := not(IsZeroComponent(A) and IsZeroComponent(D) or IsZeroComponent(B)
    and IsZeroComponent(C));
end;

function TdxPDFTransformationMatrix.GetXForm: TXForm;
begin
  Result := FMatrix.Value;
end;

procedure TdxPDFTransformationMatrix.SetXForm(const AValue: TXForm);
begin
  FMatrix := AValue;
end;

procedure TdxPDFTransformationMatrix.DoMultiply(const AXForm: TXForm;
  AOrder: TdxTransformationOrder = moPrepend);
begin
  if AOrder = moPrepend then
    FMatrix := TXForm.Combine(AXForm, FMatrix)
  else
    FMatrix := TXForm.Combine(FMatrix, AXForm);
end;

{ TdxPDFDictionary }

constructor TdxPDFDictionary.Create;
begin
  inherited Create;
  FObjects := TdxPDFReferencedObjectDictionary.Create;
  FStreamRef := nil;
end;

destructor TdxPDFDictionary.Destroy;
begin
  FStreamRef := nil;
  FreeAndNil(FObjects);
  inherited Destroy;
end;

function TdxPDFDictionary.GetObject(const AKey: string): TdxPDFBase;
var
  AResult: TdxPDFReferencedObject;
begin
  if FObjects.TryGetValue(AKey, AResult) then
    Result := AResult as TdxPDFBase
  else
    Result := nil;
end;

function TdxPDFDictionary.Contains(const AKey: string): Boolean;
begin
  Result := FObjects.ContainsKey(AKey);
end;

function TdxPDFDictionary.CreateNumericList(const AKey: string)
  : TDoubleDynArray;
var
  AArray: TdxPDFArray;
begin
  AArray := GetArray(AKey);
  if AArray <> nil then
    Result := TdxPDFUtils.ArrayToDoubleArray(AArray)
  else
    Result := nil;
end;

function TdxPDFDictionary.GetArray(const AKey: string): TdxPDFArray;
var
  AObject: TdxPDFBase;
begin
  AObject := GetObject(AKey);
  if (AObject <> nil) and (AObject.ObjectType = otArray) then
    Result := AObject as TdxPDFArray
  else
    Result := nil;
end;

function TdxPDFDictionary.GetArray(const AKey, AAlternativeKey: string)
  : TdxPDFArray;
begin
  Result := GetArray(AKey);
  if Result = nil then
    Result := GetArray(AAlternativeKey);
end;

function TdxPDFDictionary.GetArray(const AKey: string;
  out AArray: TdxPDFArray): Boolean;
begin
  AArray := GetArray(AKey);
  Result := AArray <> nil;
end;

function TdxPDFDictionary.GetBoolean(const AKey: string;
  ADefaultValue: Boolean = False): Boolean;
begin
  Result := ADefaultValue;
  if Contains(AKey) then
    Result := TdxPDFBoolean(GetObject(AKey)).Value;
end;

function TdxPDFDictionary.GetBytes(const AKey: string): TBytes;
var
  AObject: TdxPDFBase;
begin
  SetLength(Result, 0);
  if Contains(AKey) then
  begin
    AObject := GetObject(AKey);
    if (AObject <> nil) and (AObject is TdxPDFString) then
      Result := TdxPDFUtils.StrToByteArray(TdxPDFString(AObject).Value);
  end;
end;

function TdxPDFDictionary.GetDate(const AKey: string): TDateTime;
var
  APosition: Integer;
  S: string;
begin
  S := GetString(AKey);
  APosition := Pos('D', S);
  if APosition > 0 then
    S := Copy(S, APosition, MaxInt)
  else
    S := '';

  if S <> '' then
    try
      Result := ParseDateTime(S);
    except
      on E: EConvertError do
        Result := NullDate
      else
        raise;
    end
  else
    Result := NullDate;
end;

function TdxPDFDictionary.GetDictionary(const AKey: string): TdxPDFDictionary;
var
  AObject: TdxPDFBase;
begin
  AObject := GetObject(AKey);
  if (AObject <> nil) and (AObject.ObjectType = otDictionary) then
    Result := AObject as TdxPDFDictionary
  else
    Result := nil;
end;

function TdxPDFDictionary.GetDouble(const AKey: string;
  ADefaultValue: Double = dxPDFInvalidValue): Double;
var
  AObject: TdxPDFBase;
begin
  if TryGetObject(AKey, AObject) and (AObject is TdxPDFNumericObject) then
    Result := TdxPDFNumericObject(AObject).GetValue
  else
    Result := ADefaultValue;
end;

function TdxPDFDictionary.GetDoubleArray(const AKey: string): TDoubleDynArray;
var
  I: Integer;
  AArray: TdxPDFArray;
begin
  SetLength(Result, 0);
  AArray := GetArray(AKey);
  if AArray <> nil then
  begin
    SetLength(Result, AArray.Count);
    for I := 0 to AArray.Count - 1 do
      Result[I] := TdxPDFUtils.ConvertToDouble(AArray.Elements[I]);
  end;
end;

function TdxPDFDictionary.GetInteger(const AKey: string;
  ADefaultValue: Integer = dxPDFInvalidValue): Integer;
begin
  Result := ADefaultValue;
  if Contains(AKey) then
    Result := TdxPDFInteger(GetObject(AKey)).Value;
end;

function TdxPDFDictionary.GetInteger(const AKey, AAlternativeKey
  : string): Integer;
begin
  Result := GetInteger(AKey);
  if not TdxPDFUtils.IsIntegerValid(Result) then
    Result := GetInteger(AAlternativeKey);
end;

function TdxPDFDictionary.GetRectangle(const AKey: string): TdxRectF;
begin
  Result := TdxPDFUtils.ArrayToRectF(GetObject(AKey) as TdxPDFArray);
  if Result.Top < Result.Bottom then
    ExchangeSingle(Result.Top, Result.Bottom);
end;

function TdxPDFDictionary.GetRectangle(const AKey: string;
  const ADefaultValue: TdxRectF): TdxRectF;
begin
  if Contains(AKey) then
    Result := GetRectangle(AKey)
  else
    Result := ADefaultValue;
end;

function TdxPDFDictionary.GetRectangleEx(const AKey: string): TdxPDFRectangle;
begin
  Result := TdxPDFUtils.ArrayToRectangle(GetObject(AKey) as TdxPDFArray);
end;

function TdxPDFDictionary.GetRenderingIntent(const AKey: string)
  : TdxPDFRenderingIntent;
begin
  Result := TdxPDFUtils.ConvertToRenderingIntent(GetString(AKey));
end;

function TdxPDFDictionary.GetSignatureFlags(const AKey: string)
  : TdxPDFSignatureFlags;
var
  AValue: Integer;
begin
  AValue := GetInteger(AKey, 0);
  if AValue > 2 then
    Result := [sfSignaturesExist, sfAppendOnly]
  else
    Result := [TdxPDFSignatureFlag(AValue)];
end;

function TdxPDFDictionary.GetSmallInt(const AKey: string;
  ADefaultValue: SmallInt): SmallInt;
var
  AValue: Double;
begin
  AValue := GetDouble(AKey, ADefaultValue);
  if InRange(AValue, Low(SmallInt), High(SmallInt)) then
    Result := Trunc(AValue)
  else
    Result := ADefaultValue;
end;

function TdxPDFDictionary.GetStream(const AKey: string): TdxPDFStream;
begin
  Result := GetObject(AKey) as TdxPDFStream;
end;

function TdxPDFDictionary.GetString(const AKey: string): string;
var
  AData: TBytes;
begin
  AData := GetBytes(AKey);
  if Length(AData) <> 0 then
    Result := TdxPDFUtils.ConvertToStr(AData)
  else
    Result := '';
end;

function TdxPDFDictionary.GetString(const AKey, ADefaultValue: string): string;
begin
  Result := GetString(AKey);
  if Result = '' then
    Result := ADefaultValue;
end;

function TdxPDFDictionary.GetTextString(const AKey: string): string;
var
  AData: TBytes;
begin
  AData := GetBytes(AKey);
  if Length(AData) <> 0 then
    Result := TdxPDFUtils.ConvertToText(AData)
  else
    Result := '';
end;

function TdxPDFDictionary.TryGetArray(const AKey: string;
  out AValue: TdxPDFArray): Boolean;
begin
  Result := TryGetValue<TdxPDFArray>(AKey, AValue);
end;

function TdxPDFDictionary.TryGetBoolean(const AKey: string;
  out AValue: Boolean): Boolean;
var
  AObject: TdxPDFBoolean;
begin
  Result := TryGetValue<TdxPDFBoolean>(AKey, AObject);
  if Result then
    AValue := AObject.Value;
end;

function TdxPDFDictionary.TryGetDictionary(const AKey: string;
  out AValue: TdxPDFDictionary): Boolean;
begin
  Result := TryGetValue<TdxPDFDictionary>(AKey, AValue);
end;

function TdxPDFDictionary.TryGetObject(const AKey: string;
  out AValue: TdxPDFBase): Boolean;
begin
  Result := TryGetValue<TdxPDFBase>(AKey, AValue);
end;

function TdxPDFDictionary.TryGetReference(const AKey: string;
  out AValue: Integer): Boolean;
var
  AObject: TdxPDFBase;
begin
  Result := TryGetObject(AKey, AObject);
  if Result then
    AValue := AObject.Number;
end;

function TdxPDFDictionary.TryGetStream(const AKey: string;
  out AValue: TdxPDFStream): Boolean;
var
  AObject: TdxPDFBase;
begin
  AValue := nil;
  AObject := GetObject(AKey);
  if AObject <> nil then
    case AObject.ObjectType of
      otStream:
        AValue := AObject as TdxPDFStream;
      otDictionary:
        AValue := TdxPDFDictionary(AObject).StreamRef;
    else
      AValue := nil;
    end;
  Result := AValue <> nil;
end;

function TdxPDFDictionary.TryGetString(const AKey: string;
  out AValue: string): Boolean;
var
  AObject: TdxPDFString;
begin
  Result := TryGetValue<TdxPDFString>(AKey, AObject);
  if Result then
    AValue := AObject.Value;
end;

function TdxPDFDictionary.TryGetTextString(const AKey: string;
  out AValue: string): Boolean;
begin
  Result := Contains(AKey);
  if Result then
    AValue := GetTextString(AKey)
  else
    AValue := '';
end;

procedure TdxPDFDictionary.Add(const AKey: string;
  const AValue: TDoubleDynArray; ASkipIfNull: Boolean = True);
var
  AArray: TdxPDFArray;
  ALength: Integer;
  I: Integer;
begin
  ALength := Length(AValue);
  if not ASkipIfNull or (ALength > 0) then
  begin
    AArray := TdxPDFArray.Create;
    for I := 0 to ALength - 1 do
      AArray.Add(AValue[I]);
    Add(AKey, AArray);
  end;
end;

procedure TdxPDFDictionary.Add(const AKey: string;
  const AValue: TIntegerDynArray);
var
  AArray: TdxPDFArray;
  ALength: Integer;
  I: Integer;
begin
  ALength := Length(AValue);
  if ALength > 0 then
  begin
    AArray := TdxPDFArray.Create;
    for I := 0 to ALength - 1 do
      AArray.Add(AValue[I]);
    Add(AKey, AArray);
  end;
end;

procedure TdxPDFDictionary.Add(const AKey: string;
  const AValue: TdxPDFSignatureFlags);
var
  AFlag: TdxPDFSignatureFlag;
  AValueAsInteger: Integer;
begin
  AValueAsInteger := 0;
  for AFlag in AValue do
    Inc(AValueAsInteger, Ord(AFlag));
  Add(AKey, AValueAsInteger);
end;

procedure TdxPDFDictionary.AddBytes(const AKey: string; const AValue: TBytes);
begin
  if Length(AValue) > 0 then
    Add(AKey, TdxPDFBytes.Create(AValue));
end;

procedure TdxPDFDictionary.Add(const AKey: string; const AValue: TBytes);
begin
  if Length(AValue) > 0 then
    Add(AKey, TdxPDFString.Create(TdxPDFUtils.BytesToStr(AValue)));
end;

procedure TdxPDFDictionary.Add(const AKey: string; AValue: TdxPDFBase);
var
  ATemp: TdxPDFReferencedObject;
begin
  ATemp := nil;
  if FObjects.TryGetValue(AKey, ATemp) and (AValue <> ATemp) or (ATemp = nil)
  then
  begin
    if AValue = nil then
      FObjects.AddOrSetValue(AKey, TdxPDFNull.Create)
    else
      FObjects.AddOrSetValue(AKey, AValue);
  end;
end;

procedure TdxPDFDictionary.Add(const AKey: string; AValue: Integer);
begin
  if dxPDFIsIntegerValid(AValue) then
    Add(AKey, TdxPDFInteger.Create(AValue));
end;

procedure TdxPDFDictionary.Add(const AKey: string;
  AValue, ADefaultValue: Integer);
begin
  if AValue <> ADefaultValue then
    Add(AKey, AValue);
end;

procedure TdxPDFDictionary.Add(const AKey: string;
  AValue, ADefaultValue: Boolean);
begin
  if ADefaultValue <> AValue then
    Add(AKey, AValue);
end;

procedure TdxPDFDictionary.Add(const AKey: string; AValue: Double);
begin
  if dxPDFIsDoubleValid(AValue) then
    Add(AKey, TdxPDFNumericObject.Create(AValue));
end;

procedure TdxPDFDictionary.Add(const AKey: string;
  AValue, ADefaultValue: Double);
begin
  if not SameValue(AValue, ADefaultValue) then
    Add(AKey, AValue);
end;

procedure TdxPDFDictionary.Add(const AKey: string; AValue: Boolean);
begin
  Add(AKey, TdxPDFBoolean.Create(AValue));
end;

procedure TdxPDFDictionary.Add(const AKey, AValue: string);
begin
  if AValue <> '' then
    Add(AKey, TdxPDFString.Create(AValue));
end;

procedure TdxPDFDictionary.Add(const AKey: string; const AValue: string;
  AEncoding: TEncoding);
begin
  if AValue <> '' then
    Add(AKey, AEncoding.GetBytes(AValue));
end;

procedure TdxPDFDictionary.Add(const AKey: string; const AValue: TdxRectF;
  ACheckRect: Boolean = True; ASkipIfNull: Boolean = True);
begin
  if not ASkipIfNull or not TdxPDFUtils.IsRectEmpty(AValue) then
    Add(AKey, TdxPDFUtils.RectToArray(AValue, ACheckRect));
end;

procedure TdxPDFDictionary.Add(const AKey: string; const AValue: TdxPDFRanges);
var
  AArray: TdxPDFArray;
  I, ALength: Integer;
begin
  ALength := Length(AValue);
  if ALength > 0 then
  begin
    AArray := TdxPDFArray.Create;
    for I := 0 to ALength - 1 do
      AArray.Add(AValue[I]);
    Add(AKey, AArray);
  end;
end;

procedure TdxPDFDictionary.Add(const AKey: string;
  const AMatrix: TdxPDFTransformationMatrix;
  ASkipNullOrIdentity: Boolean = True);
begin
  if not(ASkipNullOrIdentity and (AMatrix.IsNull or AMatrix.IsIdentity)) then
    Add(AKey, TdxPDFUtils.MatrixToArray(AMatrix));
end;

procedure TdxPDFDictionary.Add(const AKey: string;
  const AValue: TdxPDFRectangle; ASkipIfNull: Boolean = True);
var
  AArray: TdxPDFArray;
begin
  if not(ASkipIfNull and AValue.IsNull) then
  begin
    AArray := TdxPDFArray.Create;
    AArray.Add(AValue.Left);
    AArray.Add(AValue.Bottom);
    AArray.Add(AValue.Right);
    AArray.Add(AValue.Top);
    Add(AKey, AArray);
  end;
end;

procedure TdxPDFDictionary.Add(const AKey: string;
  const AValue, ADefaultValue: TdxRectF);
begin
  if not TdxPDFUtils.RectIsEqual(AValue, ADefaultValue, 0.001) then
    Add(AKey, AValue);
end;

procedure TdxPDFDictionary.AddDate(const AKey: string; AValue: TDateTime);
begin
  if AValue <> NullDate then
    Add(AKey, TdxPDFUtils.ConvertToStr(AValue));
end;

procedure TdxPDFDictionary.AddName(const AKey: string; const AValue: string;
  ASkipIfNull: Boolean = True);
begin
  if not(ASkipIfNull and (AValue = '')) then
    Add(AKey, TdxPDFName.Create(AValue));
end;

procedure TdxPDFDictionary.AddName(const AKey: string;
  const AValue: TdxPDFRenderingIntent);
begin
  AddName(AKey, TdxPDFUtils.ConvertToStr(AValue));
end;

procedure TdxPDFDictionary.AddReference(const AKey: string; ANumber: Integer;
  AGeneration: Integer = 0);
begin
  Add(AKey, TdxPDFReference.Create(ANumber, AGeneration));
end;

procedure TdxPDFDictionary.AddReference(const AKey: string; const AData: TBytes;
  ASkipIfNull: Boolean = True);
begin
  TdxPDFUtils.RaiseException('AddReference(TBytes) is not supported');
end;

procedure TdxPDFDictionary.Clear;
begin
  FObjects.Clear;
end;

procedure TdxPDFDictionary.Remove(const AKey: string);
begin
  FObjects.Remove(AKey);
end;

class function TdxPDFDictionary.GetObjectType: TdxPDFBaseType;
begin
  Result := otDictionary;
end;

function TdxPDFDictionary.GetCount: Integer;
begin
  Result := FObjects.Count;
end;

procedure TdxPDFDictionary.Write(AWriter: TdxPDFWriter);
begin
  AWriter.WriteOpenDictionary;
  WriteContent(AWriter);
  AWriter.WriteCloseDictionary;
  WriteStream(AWriter);
end;

procedure TdxPDFDictionary.WriteContent(AWriter: TdxPDFWriter);
var
  APair: TPair<string, TdxPDFReferencedObject>;
  AName: TdxPDFName;
  ACount: Integer;
begin
  AName := TdxPDFName.Create;
  try
    ACount := Items.Count;
    for APair in Items do
    begin
      Dec(ACount);
      if APair.Value is TdxPDFBase then
      begin
        AName.Value := APair.Key;
        AName.Write(AWriter);
        AWriter.WriteSpace;
        TdxPDFBaseAccess(APair.Value).Write(AWriter);
        if ACount > 0 then
          AWriter.WriteSpace;
      end;
    end;
  finally
    AName.Free;
  end;
end;

procedure TdxPDFDictionary.WriteStream(AWriter: TdxPDFWriter);
begin
  if StreamRef <> nil then
    WriteStreamData(AWriter, StreamRef.Data);
end;

procedure TdxPDFDictionary.WriteStreamData(AWriter: TdxPDFWriter;
  const AData: TBytes);
begin
  if Length(AData) > 0 then
  begin
    AWriter.WriteLineFeed;
    AWriter.WriteString(TdxPDFKeywords.Stream, True);
    AWriter.WriteBytes(AData);
    AWriter.WriteLineFeed;
    AWriter.WriteString(TdxPDFKeywords.EndStream);
  end;
end;

function TdxPDFDictionary.GetItems: TdxPDFStringReferencedObjectDictionary;
begin
  Result := FObjects.Items;
end;

function TdxPDFDictionary.GetMatrix(const AKey: string)
  : TdxPDFTransformationMatrix;
begin
  Result := TdxPDFUtils.ArrayToMatrix(GetArray(AKey));
end;

function TdxPDFDictionary.GetValue(const AKey: string): TdxPDFBase;
begin
  if not TryGetValue<TdxPDFBase>(AKey, Result) then
    Result := nil;
end;

procedure TdxPDFDictionary.SetValue(const AKey: string; AValue: TdxPDFBase);
begin
  Add(AKey, AValue);
end;

function TdxPDFDictionary.ParseDateTime(ADateTime: string): TDateTime;
var
  AUTC: TcxDateTime;
begin
  if (Length(ADateTime) > 2) and (ADateTime[1] = 'D') and (ADateTime[2] = ':')
  then
    Delete(ADateTime, 1, 2);
  ADateTime := StringReplace(ADateTime, '"', '', [rfReplaceAll]);
  ADateTime := StringReplace(ADateTime, '''', '', [rfReplaceAll]);
  if Length(ADateTime) >= 4 then
  begin
    AUTC.Year := TdxPDFUtils.ConvertToDigit(Byte(ADateTime[1])) * 1000 +
      TdxPDFUtils.ConvertToDigit(Byte(ADateTime[2])) * 100 +
      TdxPDFUtils.ConvertToDigit(Byte(ADateTime[3])) * 10 +
      TdxPDFUtils.ConvertToDigit(Byte(ADateTime[4]));
    Delete(ADateTime, 1, 4);
    ParseDateTimeComponent(ADateTime, PInteger(@AUTC.Month), 1);
    ParseDateTimeComponent(ADateTime, PInteger(@AUTC.Day), 1);
    ParseDateTimeComponent(ADateTime, PInteger(@AUTC.Hours));
    ParseDateTimeComponent(ADateTime, PInteger(@AUTC.Minutes));
    ParseDateTimeComponent(ADateTime, PInteger(@AUTC.Seconds));
    Result := cxGetLocalCalendar.ToDateTime(AUTC);
  end
  else
    Result := NullDate;
end;

function TdxPDFDictionary.TryGetValue<T>(const AKey: string;
  out AValue: T): Boolean;
var
  ATemp: TdxPDFReferencedObject;
begin
  Result := FObjects.TryGetValue(AKey, ATemp);
  if Result then
  begin
    AValue := Safe<T>.Cast(ATemp);
    Result := AValue <> nil;
  end;
end;

procedure TdxPDFDictionary.ParseDateTimeComponent(var ADateTime: string;
  AComponent: PInteger; ADefaultValue: Byte = 0);
begin
  if Length(ADateTime) >= 2 then
  begin
    AComponent^ := TdxPDFUtils.ConvertToDigit(Byte(ADateTime[1])) * 10 +
      TdxPDFUtils.ConvertToDigit(Byte(ADateTime[2]));
    Delete(ADateTime, 1, 2);
  end
  else
    AComponent^ := ADefaultValue;
end;

{ TdxPDFValue }

constructor TdxPDFValue.Create(ANumber, AGeneration: Integer;
  const AValue: Variant);
begin
  inherited Create(ANumber, AGeneration);
  InternalSetValue(AValue);
end;

constructor TdxPDFValue.Create(const AValue: Variant);
begin
  inherited Create;
  InternalSetValue(AValue);
end;

procedure TdxPDFValue.InternalSetValue(const AValue: Variant);
begin
  FValue := AValue;
end;

{ TdxPDFBoolean }

class function TdxPDFBoolean.GetObjectType: TdxPDFBaseType;
begin
  Result := otBoolean;
end;

procedure TdxPDFBoolean.Write(AWriter: TdxPDFWriter);
const
  Map: array [Boolean] of string = ('false', 'true');
begin
  AWriter.WriteString(Map[Value]);
end;

function TdxPDFBoolean.GetValue: Boolean;
begin
  Result := InternalValue;
end;

procedure TdxPDFBoolean.SetValue(const AValue: Boolean);
begin
  SetValue(AValue);
end;

{ TdxPDFNull }

procedure TdxPDFNull.Write(AWriter: TdxPDFWriter);
begin
  AWriter.WriteString('null', False);
end;

function TdxPDFNull.GetValue: Variant;
begin
  Result := varEmpty;
end;

{ TdxPDFCustomBytes }

constructor TdxPDFCustomBytes.Create(const AValue: TBytes);
begin
  inherited Create;
  FData := AValue;
end;

procedure TdxPDFCustomBytes.Write(AWriter: TdxPDFWriter);
begin
  AWriter.WriteHexadecimalString(FData);
end;

{ TdxPDFBytes }

procedure TdxPDFBytes.Write(AWriter: TdxPDFWriter);
begin
  AWriter.WriteHexadecimalString(AWriter.Encrypt(FData));
end;

{ TdxPDFPlaceHolder }

constructor TdxPDFPlaceHolder.Create(ALength: Int64);
var
  AData: TBytes;
begin
  SetLength(AData, ALength);
  inherited Create(AData);
  FOffset := -1;
end;

function TdxPDFPlaceHolder.IsValid: Boolean;
begin
  Result := FOffset <> -1;
end;

procedure TdxPDFPlaceHolder.Write(AWriter: TdxPDFWriter);
begin
  FOffset := AWriter.Stream.Position;
  AWriter.WriteBytes(FData);
end;

function TdxPDFPlaceHolder.GetSize: Int64;
begin
  Result := Length(FData);
end;

{ TdxPDFString }

class function TdxPDFString.GetObjectType: TdxPDFBaseType;
begin
  Result := otString;
end;

procedure TdxPDFString.Write(AWriter: TdxPDFWriter);
var
  AStream: TdxPDFMemoryStream;
  AValue: string;
begin
  AValue := Value;
  if TdxPDFUtils.IsUnicodeString(AValue) then
  begin
    AStream := TdxPDFMemoryStream.Create;
    try
      AStream.WriteByte(254);
      AStream.WriteByte(255);
      AStream.WriteArray(TEncoding.BigEndianUnicode.GetBytes(AValue));
      AWriter.WriteHexadecimalString(AWriter.Encrypt(AStream.Data));
    finally
      AStream.Free;
    end;
  end
  else
    AWriter.WriteStringValue(AWriter.Encrypt(TEncoding.
      Default.GetBytes(AValue)));
end;

function TdxPDFString.GetText: string;
begin
  Result := TdxPDFUtils.ConvertToText(TdxPDFUtils.StrToByteArray(Value));
end;

function TdxPDFString.GetValue: string;
begin
  Result := VarToStr(InternalValue);
end;

procedure TdxPDFString.SetValue(const AValue: string);
begin
  InternalSetValue(AValue);
end;

{ TdxPDFComment }

class function TdxPDFComment.GetObjectType: TdxPDFBaseType;
begin
  Result := otComment;
end;

procedure TdxPDFComment.Write(AWriter: TdxPDFWriter);
begin
  AWriter.WriteString('%' + Value);
end;

{ TdxPDFName }

class function TdxPDFName.GetObjectType: TdxPDFBaseType;
begin
  Result := otName;
end;

procedure TdxPDFName.Write(AWriter: TdxPDFWriter);
begin
  AWriter.WriteName(Value);
end;

{ TdxPDFNumericObject }

class function TdxPDFNumericObject.GetObjectType: TdxPDFBaseType;
begin
  Result := otDouble;
end;

procedure TdxPDFNumericObject.Write(AWriter: TdxPDFWriter);
begin
  AWriter.WriteDouble(InternalValue);
end;

function TdxPDFNumericObject.GetValue: Variant;
begin
  Result := InternalValue;
end;

{ TdxPDFInteger }

class function TdxPDFInteger.GetObjectType: TdxPDFBaseType;
begin
  Result := otInteger;
end;

procedure TdxPDFInteger.Write(AWriter: TdxPDFWriter);
begin
  AWriter.WriteInteger(Value);
end;

function TdxPDFInteger.GetValue: Integer;
begin
  Result := InternalValue;
end;

procedure TdxPDFInteger.SetValue(const AValue: Integer);
begin
  InternalSetValue(AValue);
end;

{ TdxPDFDouble }

function TdxPDFDouble.GetValue: Double;
begin
  Result := InternalValue;
end;

procedure TdxPDFDouble.SetValue(AValue: Double);
begin
  InternalSetValue(AValue);
end;

{ TdxPDFReference }

constructor TdxPDFReference.Create(ANumber, AGeneration: Integer;
  AOffset: Int64 = 0; AIsFree: Boolean = False; AIsSlot: Boolean = False);
begin
  inherited Create(ANumber, AGeneration);
  FIsFree := AIsFree;
  FIsSlot := AIsSlot;
  FOffset := AOffset;
end;

class function TdxPDFReference.GetObjectType: TdxPDFBaseType;
begin
  Result := otIndirectReference;
end;

procedure TdxPDFReference.Write(AWriter: TdxPDFWriter);
begin
  AWriter.WriteString(IntToStr(Number) + ' ' + IntToStr(Generation) + ' R');
end;

{ TdxPDFIndirectObject }

constructor TdxPDFIndirectObject.Create(ANumber, AGeneration: Integer;
  const AData: TBytes);
begin
  inherited Create(ANumber, AGeneration);
  FData := AData;
end;

destructor TdxPDFIndirectObject.Destroy;
begin
  SetLength(FData, 0);
  inherited Destroy;
end;

class function TdxPDFIndirectObject.GetObjectType: TdxPDFBaseType;
begin
  Result := otIndirectObject;
end;

{ TdxPDFTokenDescription }

constructor TdxPDFTokenDescription.Create(const ADescription: string);
begin
  Create(TdxPDFUtils.StrToByteArray(ADescription));
end;

constructor TdxPDFTokenDescription.Create(const ASequence: TBytes);
begin
  FSequence := ASequence;
  FSequenceLength := Length(FSequence);
  BeginCompare;
end;

class function TdxPDFTokenDescription.BeginCompare
  (AToken: TdxPDFTokenDescription): TdxPDFTokenDescription;
begin
  Result := TdxPDFTokenDescription.Create(AToken.Sequence);
end;

function TdxPDFTokenDescription.Compare(ASymbol: Byte): Boolean;
begin
  if ASymbol = FCurrentComparingSymbol then
  begin
    if FIndexToCompare = FSequenceLength - 1 then
      Exit(True);
    Inc(FIndexToCompare);
    FCurrentComparingSymbol := FSequence[FIndexToCompare];
  end
  else if FIndexToCompare <> 0 then
    BeginCompare;
  Result := False;
end;

function TdxPDFTokenDescription.IsStartWithComment: Boolean;
begin
  Result := FSequence[0] = TdxPDFDefinedSymbols.Comment;
end;

procedure TdxPDFTokenDescription.BeginCompare;
begin
  FIndexToCompare := 0;
  FCurrentComparingSymbol := FSequence[0];
end;

{ TdxPDFStreamElement }

constructor TdxPDFStreamElement.Create(ANumber: Integer; AIndex: Integer);
begin
  inherited Create(ANumber, 0, 0, False);
  FIndex := AIndex;
end;

class function TdxPDFStreamElement.GetObjectType: TdxPDFBaseType;
begin
  Result := otStreamElement;
end;

{ TdxPDFCustomRepository }

constructor TdxPDFCustomRepository.Create;
begin
  inherited Create;
  CreateSubClasses;
end;

destructor TdxPDFCustomRepository.Destroy;
begin
  DestroySubClasses;
  inherited Destroy;
end;

function TdxPDFCustomRepository.DecryptString(const S: string): string;
begin
  Result := S;
end;

function TdxPDFCustomRepository.GetArray(ANumber: Integer): TdxPDFArray;
var
  AResult: TdxPDFReferencedObject;
begin
  AResult := GetObject(ANumber);
  if TdxPDFBase(AResult).ObjectType = otArray then
    Result := TdxPDFArray(AResult)
  else
    Result := nil;
end;

function TdxPDFCustomRepository.GetDictionary(ANumber: Integer)
  : TdxPDFDictionary;
var
  AResult: TdxPDFReferencedObject;
begin
  AResult := ResolveObject(ANumber);
  Result := nil;
  if (AResult <> nil) and (AResult is TdxPDFBase) then
    case TdxPDFBase(AResult).ObjectType of
      otStream:
        Result := TdxPDFStream(AResult).Dictionary;
      otDictionary:
        Result := AResult as TdxPDFDictionary;
    end;
end;

function TdxPDFCustomRepository.GetInteger(ANumber: Integer): TdxPDFInteger;
begin
  Result := GetObject(ANumber) as TdxPDFInteger;
end;

function TdxPDFCustomRepository.GetNumericObject(ANumber: Integer)
  : TdxPDFNumericObject;
begin
  Result := GetObject(ANumber) as TdxPDFNumericObject;
end;

function TdxPDFCustomRepository.GetObject(ANumber: Integer)
  : TdxPDFReferencedObject;
begin
  Result := ResolveObject(ANumber);
end;

function TdxPDFCustomRepository.GetStream(ANumber: Integer): TdxPDFStream;
var
  AObject: TdxPDFBase;
begin
  Result := nil;
  AObject := GetObject(ANumber) as TdxPDFBase;
  if AObject <> nil then
    if AObject.ObjectType = otDictionary then
      Result := TdxPDFDictionary(AObject).StreamRef
    else
      Result := AObject as TdxPDFStream;
end;

function TdxPDFCustomRepository.ResolveReference(AObject: TdxPDFBase)
  : TdxPDFBase;
begin
  if TdxPDFUtils.IsReference(AObject) then
    Result := GetObject(AObject.Number) as TdxPDFBase
  else
    Result := AObject;
end;

procedure TdxPDFCustomRepository.Add(ANumber: Integer;
  AObject: TdxPDFReferencedObject; ACanReplace: Boolean = True);
begin
  TryAdd(ANumber, AObject, ACanReplace);
end;

procedure TdxPDFCustomRepository.Clear;
begin
  References.Clear;
end;

procedure TdxPDFCustomRepository.Remove(ANumber: Integer);
begin
  References.Remove(ANumber);
end;

procedure TdxPDFCustomRepository.TrimExcess;
begin
  FReferences.TrimExcess;
end;

function TdxPDFCustomRepository.ResolveObject(ANumber: Integer)
  : TdxPDFReferencedObject;
begin
  if not References.TryGetValue(ANumber, Result) then
    Result := nil;
end;

procedure TdxPDFCustomRepository.CreateSubClasses;
begin
  FReferences := TdxPDFBaseReferences.Create;
end;

procedure TdxPDFCustomRepository.DestroySubClasses;
begin
  FreeAndNil(FReferences);
end;

procedure TdxPDFCustomRepository.Replace(ANumber: Integer;
  AObject: TdxPDFReferencedObject);
var
  ATemp: TdxPDFReferencedObject;
begin
  if References.TryGetValue(ANumber, ATemp) and (ATemp <> AObject) or
    (ATemp = nil) then
  begin
    Remove(ANumber);
    Add(ANumber, AObject);
  end;
end;

procedure TdxPDFCustomRepository.TryAdd(ANumber: Integer;
  AObject: TdxPDFReferencedObject; ACanReplace: Boolean);
begin
  if not References.ContainsKey(ANumber) then
    References.Add(ANumber, AObject)
  else if ACanReplace then
    Replace(ANumber, AObject)
  else
    dxPDFFreeObject(AObject);
end;

function TdxPDFCustomRepository.GetMaxObjectNumber: Integer;
begin
  Result := References.MaxKey;
end;

{ TdxPDFStringCommandData }

class function TdxPDFStringCommandData.Create(const ACharCodes
  : TdxPDFBytesDynArray; const AStr: TWordDynArray;
  const AOffsets: TDoubleDynArray): TdxPDFStringCommandData;
begin
  Result.FCharCodes := ACharCodes;
  Result.FStr := AStr;
  Result.FOffsets := AOffsets;
end;

{ TdxPDFStringData }

class function TdxPDFStringData.Create(const ACodePointData
  : TdxPDFStringCommandData; const AWidths, AAdvances: TDoubleDynArray)
  : TdxPDFStringData;
begin
  Result.FCommandData := ACodePointData;
  Result.FWidths := AWidths;
  Result.FAdvances := AAdvances;
end;

function TdxPDFStringData.GetCharCodes: TdxPDFBytesDynArray;
begin
  Result := FCommandData.CharCodes;
end;

function TdxPDFStringData.GetStr: TWordDynArray;
begin
  Result := FCommandData.Str;
end;

function TdxPDFStringData.GetOffsets: TDoubleDynArray;
begin
  Result := FCommandData.Offsets;
end;

{ TdxPDFRange }

class function TdxPDFRange.Create(AMin, AMax: Double): TdxPDFRange;
begin
  Result.Min := AMin;
  Result.Max := AMax;
end;

class function TdxPDFRange.Invalid: TdxPDFRange;
begin
  Result := Create(-MaxInt, -MaxInt);
end;

function TdxPDFRange.Contains(AValue: Integer): Boolean;
begin
  Result := InRange(AValue, Min, Max);
end;

function TdxPDFRange.IsSame(ARange: TdxPDFRange): Boolean;
begin
  Result := SameValue(Min, ARange.Min) and SameValue(Max, ARange.Max);
end;

{ TdxPDFRectangle }

class function TdxPDFRectangle.Create(ALeft, ABottom, ARight, ATop: Single)
  : TdxPDFRectangle;
begin
  Result.FLeft := ALeft;
  Result.FBottom := ABottom;
  Result.FRight := ARight;
  Result.FTop := ATop;
end;

class function TdxPDFRectangle.Create(const P1, P2: TdxPointF): TdxPDFRectangle;
var
  AX1, AX2, AY1, AY2: Double;
begin
  AX1 := P1.X;
  AX2 := P2.X;
  if AX1 < AX2 then
  begin
    Result.FLeft := AX1;
    Result.FRight := AX2;
  end
  else
  begin
    Result.FLeft := AX2;
    Result.FRight := AX1;
  end;
  AY1 := P1.Y;
  AY2 := P2.Y;
  if AY1 < AY2 then
  begin
    Result.FBottom := AY1;
    Result.FTop := AY2;
  end
  else
  begin
    Result.FBottom := AY2;
    Result.FTop := AY1;
  end;
end;

// class function TdxPDFRectangle.Create(const R: TdxRectF): TdxPDFRectangle;
// begin
// Result := Create(R.Left, R.Top, R.Right, R.Bottom);
// end;

class function TdxPDFRectangle.CreateBoundingRectangle(const APoints
  : TdxPDFPoints): TdxPDFRectangle;
var
  ACount, I: Integer;
  APoint: TdxPointF;
  AXMin, AXMax, AYMin, AYMax, X, Y: Double;
begin
  ACount := Length(APoints);
  if ACount = 0 then
    Exit(TdxPDFRectangle.Null);
  APoint := APoints[0];
  AXMin := APoint.X;
  AXMax := AXMin;
  AYMin := APoint.Y;
  AYMax := AYMin;
  for I := 1 to ACount - 1 do
  begin
    APoint := APoints[I];
    X := APoint.X;
    if X < AXMin then
      AXMin := X
    else if X > AXMax then
      AXMax := X;
    Y := APoint.Y;
    if Y < AYMin then
      AYMin := Y
    else if Y > AYMax then
      AYMax := Y;
  end;
  Result := TdxPDFRectangle.Create(AXMin, AYMin, AXMax, AYMax);
end;

class function TdxPDFRectangle.Equal(const R1, R2: TdxPDFRectangle;
  ADelta: Double): Boolean;
begin
  Result := IsSame(R1.Left, R2.Left, ADelta) and IsSame(R1.Bottom, R2.Bottom,
    ADelta) and IsSame(R1.Right, R2.Right, ADelta) and
    IsSame(R1.Top, R2.Top, ADelta);
end;

class function TdxPDFRectangle.Inflate(const R: TdxPDFRectangle;
  AAmount: Double): TdxPDFRectangle;
var
  ALeft, ABottom, ARight, ATop: Double;
begin
  if (AAmount * 2 > R.Width) or (AAmount * 2 > R.Height) then
    Exit(R);
  ALeft := R.Left + AAmount;
  ABottom := R.Bottom + AAmount;
  ARight := R.Right - AAmount;
  ATop := R.Top - AAmount;
  Result := TdxPDFRectangle.Create(ALeft, ABottom, ARight, ATop);
end;

class function TdxPDFRectangle.Intersect(const R1, R2: TdxPDFRectangle)
  : TdxPDFRectangle;
begin
  if R1.Intersects(R2) then
    Result := TdxPDFRectangle.Create(Max(R1.Left, R2.Left),
      Max(R1.Bottom, R2.Bottom), Min(R1.Right, R2.Right), Min(R1.Top, R2.Top))
  else
    Result := TdxPDFRectangle.Null;
end;

class function TdxPDFRectangle.Null: TdxPDFRectangle;
begin
  Result := TdxPDFRectangle.Create(dxNullRectF.Left, dxNullRectF.Top,
    dxNullRectF.Right, dxNullRectF.Bottom);
end;

class function TdxPDFRectangle.Parse(AArray: TdxPDFArray): TdxPDFRectangle;
var
  ALeft, ABottom, ARight, ATop, ATemp: Double;
begin
  if (AArray <> nil) and (AArray.Count = 4) then
  begin
    ALeft := TdxPDFDouble(AArray[0]).Value;
    ABottom := TdxPDFDouble(AArray[1]).Value;
    ARight := TdxPDFDouble(AArray[2]).Value;
    ATop := TdxPDFDouble(AArray[3]).Value;
    if ARight < ALeft then
    begin
      ATemp := ARight;
      ARight := ALeft;
      ALeft := ATemp;
    end;
    if ATop < ABottom then
    begin
      ATemp := ABottom;
      ABottom := ATop;
      ATop := ATemp;
    end;
    Result := TdxPDFRectangle.Create(ALeft, ABottom, ARight, ATop);
  end
  else
    Result := TdxPDFRectangle.Null;
end;

function TdxPDFRectangle.Equal(const R: TdxPDFRectangle): Boolean;
begin
  Result := TdxPDFUtils.RectIsEqual(ToRectF, R.ToRectF, 0.001);
end;

function TdxPDFRectangle.Intersects(const R: TdxPDFRectangle): Boolean;
begin
  Result := (FLeft <= R.Right) and (FRight >= R.Left) and (FTop >= R.Bottom) and
    (FBottom <= R.Top);
end;

function TdxPDFRectangle.IsNull: Boolean;
begin
  Result := ToRectF.IsZero;
end;

function TdxPDFRectangle.Trim(const ARectangle: TdxPDFRectangle)
  : TdxPDFRectangle;
var
  ALeft, ABottom, ARight, ATop: Double;
begin
  ALeft := Max(FLeft, ARectangle.Left);
  ABottom := Max(FBottom, ARectangle.Bottom);
  ARight := Min(FRight, ARectangle.Right);
  ATop := Min(FTop, ARectangle.Top);
  if (ALeft > ARight) or (ABottom > ATop) then
    Result := TdxPDFRectangle.Null
  else
    Result := TdxPDFRectangle.Create(ALeft, ABottom, ARight, ATop);
end;

function TdxPDFRectangle.ToRectF: TdxRectF;
begin
  Result := dxRectF(FLeft, FTop, FRight, FBottom);
end;

function TdxPDFRectangle.GetBottomLeft: TdxPointF;
begin
  Result := dxPointF(FLeft, FBottom);
end;

function TdxPDFRectangle.GetBottomRight: TdxPointF;
begin
  Result := dxPointF(FRight, FBottom);
end;

function TdxPDFRectangle.GetTopLeft: TdxPointF;
begin
  Result := dxPointF(FLeft, FTop);
end;

function TdxPDFRectangle.GetTopRight: TdxPointF;
begin
  Result := dxPointF(FRight, FTop);
end;

function TdxPDFRectangle.GetHeight: Single;
begin
  Result := FTop - FBottom;
end;

function TdxPDFRectangle.GetWidth: Single;
begin
  Result := FRight - FLeft;
end;

class function TdxPDFRectangle.IsSame(A, B, ADelta: Double): Boolean;
begin
  Result := Abs(A - B) <= ADelta;
end;

{ TdxPDFColor }

class function TdxPDFColor.Create: TdxPDFColor;
begin
  Result.FPattern := nil;
  Result.FIsNull := False;
  SetLength(Result.FComponents, 0);
end;

class function TdxPDFColor.Create(APattern: TdxPDFReferencedObject;
  const AComponents: TDoubleDynArray): TdxPDFColor;
begin
  Result := Create(AComponents);
  Result.FPattern := APattern;
end;

class function TdxPDFColor.Create(const AComponents: TDoubleDynArray)
  : TdxPDFColor;
begin
  Result := Create;
  TdxPDFUtils.AddData(AComponents, Result.FComponents);
end;

class function TdxPDFColor.Create(const AArray: TdxPDFArray): TdxPDFColor;
var
  AComponent: Double;
  AComponents: TDoubleDynArray;
  I: Integer;
begin
  SetLength(AComponents, AArray.ElementList.Count);
  for I := 0 to AArray.ElementList.Count - 1 do
  begin
    AComponent := TdxPDFUtils.ConvertToDouble(AArray.ElementList[I]);
    AComponent := TdxPDFColor.ClipColorComponent(AComponent);
    AComponents[I] := AComponent;
  end;
  Result := Create(AComponents);
end;

class function TdxPDFColor.Create(const AColor: TdxPDFColor): TdxPDFColor;
begin
  Result := Create;
  Result.Assign(AColor);
end;

class function TdxPDFColor.Create(const X, Y, Z, AWhitePointZ: Double)
  : TdxPDFColor;
begin
  Result := Create(GetComponents(X, Y, X, AWhitePointZ));
end;

class function TdxPDFColor.Create(const C1, C2, C3: Double): TdxPDFColor;
begin
  Result := Create;
  Result.AddComponent(C1);
  Result.AddComponent(C2);
  Result.AddComponent(C3);
end;

class function TdxPDFColor.GetComponents(const X, Y, Z, AWhitePointZ: Double)
  : TDoubleDynArray;
var
  ARed, AGreen, ABlue: Double;
begin
  if AWhitePointZ < 1 then
  begin
    ARed := X * 3.1339 + Y * -1.6170 + Z * -0.4906;
    AGreen := X * -0.9785 + Y * 1.9160 + Z * 0.0333;
    ABlue := X * 0.0720 + Y * -0.2290 + Z * 1.4057;
  end
  else
  begin
    ARed := X * 3.2406 + Y * -1.5372 + Z * -0.4986;
    AGreen := X * -0.9689 + Y * 1.8758 + Z * 0.0415;
    ABlue := X * 0.0557 + Y * -0.2040 + Z * 1.0570;
  end;
  SetLength(Result, 3);
  Result[0] := ColorComponentTransferFunction(ARed);
  Result[1] := ColorComponentTransferFunction(AGreen);
  Result[2] := ColorComponentTransferFunction(ABlue);
end;

class function TdxPDFColor.Null: TdxPDFColor;
begin
  Result := Create;
  Result.FIsNull := True;
end;

procedure TdxPDFColor.Assign(const AColor: TdxPDFColor);
begin
  Pattern := AColor.Pattern;
  SetLength(FComponents, 0);
  TdxPDFUtils.AddData(AColor.Components, FComponents);
end;

class function TdxPDFColor.ClipColorComponent(const AComponent: Double): Double;
begin
  Result := TdxPDFUtils.Min(1, TdxPDFUtils.Max(0, AComponent));
end;

class function TdxPDFColor.ColorComponentTransferFunction(const AComponent
  : Double): Double;
var
  ATemp: Double;
begin
  ATemp := ClipColorComponent(AComponent);
  if ATemp > 0.0031308 then
    Result := ClipColorComponent(Power(ATemp, 1 / 2.4) * 1.055 - 0.055)
  else
    Result := ClipColorComponent(ATemp * 12.92);
end;

function TdxPDFColor.IsNull: Boolean;
begin
  Result := FIsNull;
end;

procedure TdxPDFColor.AssignAndTransferComponents(const AComponents
  : TDoubleDynArray);
var
  AValue: Double;
begin
  SetLength(FComponents, 0);
  if AComponents <> nil then
    for AValue in AComponents do
      AddComponent(AValue);
end;

procedure TdxPDFColor.AddComponent(const AValue: Double);
begin
  TdxPDFUtils.AddValue(ColorComponentTransferFunction(AValue), FComponents);
end;

procedure TdxPDFColor.SetPattern(const AValue: TdxPDFReferencedObject);
begin
  FPattern := AValue;
end;

{ TdxPDFARGBColor }

class function TdxPDFARGBColor.Create(const AColor: TdxPDFColor)
  : TdxPDFARGBColor;
begin
  if not AColor.IsNull then
    case Length(AColor.Components) of
      1:
        Result := CreateFromRGB(AColor.Components[0], AColor.Components[0],
          AColor.Components[0]);
      3:
        Result := CreateFromRGB(AColor.Components[0], AColor.Components[1],
          AColor.Components[2]);
      4:
        Result := CreateFromCMYK(AColor.Components[0], AColor.Components[1],
          AColor.Components[2], AColor.Components[3]);
    else
      Result := CreateFromRGB(0, 0, 0);
    end
  else
    TdxPDFUtils.RaiseTestException;
end;

class function TdxPDFARGBColor.CreateFromRGB(ARed, AGreen, ABlue: Double;
  AAlpha: Byte = 255): TdxPDFARGBColor;
begin
  Result.FRed := ARed;
  Result.FGreen := AGreen;
  Result.FBlue := ABlue;
  Result.FAlpha := AAlpha;
end;

class function TdxPDFARGBColor.CreateFromCMYK(ACyan: Double; AMagenta: Double;
  AYellow: Double; ABlack: Double): TdxPDFARGBColor;
var
  ACyanComplement, AMagentaComplement, AYellowComplement, ABlackComplement,
    AAddition, ARed, AGreen, ABlue: Double;
begin
  ACyanComplement := 1 - ACyan;
  AMagentaComplement := 1 - AMagenta;
  AYellowComplement := 1 - AYellow;
  ABlackComplement := 1 - ABlack;
  AAddition := ACyanComplement * AMagentaComplement * AYellowComplement *
    ABlackComplement;
  ARed := AAddition;
  AGreen := AAddition;
  ABlue := AAddition;
  AAddition := ACyanComplement * AMagentaComplement * AYellowComplement
    * ABlack;

  ARed := ARed + 0.1373 * AAddition;
  AGreen := AGreen + 0.1216 * AAddition;
  ABlue := ABlue + 0.1255 * AAddition;
  AAddition := ACyanComplement * AMagentaComplement * AYellow *
    ABlackComplement;

  ARed := ARed + AAddition;
  AGreen := AGreen + 0.9490 * AAddition;
  AAddition := ACyanComplement * AMagentaComplement * AYellow * ABlack;

  ARed := ARed + 0.1098 * AAddition;
  AGreen := AGreen + 0.1020 * AAddition;
  AAddition := ACyanComplement * AMagenta * AYellowComplement *
    ABlackComplement;

  ARed := ARed + 0.9255 * AAddition;
  ABlue := ABlue + 0.5490 * AAddition;
  ARed := ARed + 0.1412 * (ACyanComplement * AMagenta * AYellowComplement
    * ABlack);
  AAddition := ACyanComplement * AMagenta * AYellow * ABlackComplement;

  ARed := ARed + 0.9294 * AAddition;
  AGreen := AGreen + 0.1098 * AAddition;
  ABlue := ABlue + 0.1412 * AAddition;
  ARed := ARed + 0.1333 * (ACyanComplement * AMagenta * AYellow * ABlack);
  AAddition := ACyan * AMagentaComplement * AYellowComplement *
    ABlackComplement;

  AGreen := AGreen + 0.6784 * AAddition;
  ABlue := ABlue + 0.9373 * AAddition;
  AAddition := ACyan * AMagentaComplement * AYellowComplement * ABlack;

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
  ABlue := ABlue + 0.0078 * (ACyan * AMagenta * AYellowComplement * ABlack);
  AAddition := ACyan * AMagenta * AYellow * ABlackComplement;

  Result.FRed := TdxPDFColor.ClipColorComponent(ARed + 0.2118 * AAddition);
  Result.FGreen := TdxPDFColor.ClipColorComponent(AGreen + 0.2119 * AAddition);
  Result.FBlue := TdxPDFColor.ClipColorComponent(ABlue + 0.2235 * AAddition);
end;

class function TdxPDFARGBColor.Convert(const AColor: TdxPDFARGBColor)
  : TdxPDFColor;
var
  AComponents: TDoubleDynArray;
begin
  SetLength(AComponents, 3);
  AComponents[0] := AColor.Red;
  AComponents[1] := AColor.Green;
  AComponents[2] := AColor.Blue;
  Result := TdxPDFColor.Create(AComponents);
end;

class function TdxPDFARGBColor.ConvertToBytes(ACyan, AMagenta, AYellow,
  ABlack: Byte): TBytes;
var
  ACyanComplement, AMagentaComplement, AYellowComplement, ABlackComplement,
    ABlackDiv, AAddition, ARed, AGreen, ABlue: Double;
  D: Integer;
begin
  SetLength(Result, 3);
  Result[0] := 0;
  Result[1] := 0;
  Result[2] := 0;
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
    SetLength(Result, 3);
    Result[0] := Trunc((ARed + 0.2118 * AAddition) / D);
    Result[1] := Trunc((AGreen + 0.2119 * AAddition) / D);
    Result[2] := Trunc((ABlue + 0.2235 * AAddition) / D);
  end;
end;

class function TdxPDFARGBColor.ConvertToRGB(const AData: TBytes;
  APixelFormat: TdxPDFPixelFormat): TBytes;
var
  I, AIndex: Integer;
begin
  case APixelFormat of
    pfGray1bit:
      SetLength(Result, 0);
    pfGray8bit:
      begin
        AIndex := 0;
        SetLength(Result, Length(AData) * 3);
        for I := 0 to Length(AData) - 1 do
        begin
          TdxPDFUtils.CopyData(AData, I, Result, AIndex, 3);
          Inc(AIndex, 3);
        end;
      end;
  else
    Result := AData;
  end;
end;

{ TdxPDFCustomProperties }

constructor TdxPDFCustomProperties.Create(ADictionary: TdxPDFDictionary);
var
  AKey: string;
begin
  inherited Create;
  FDictionary := TdxPDFDictionary.Create;
  for AKey in ADictionary.Items.Keys do
    Dictionary.Add(AKey, ADictionary.GetObject(AKey));
end;

destructor TdxPDFCustomProperties.Destroy;
begin
  Dictionary := nil;
  inherited Destroy;
end;

procedure TdxPDFCustomProperties.Write(AWriter: TdxPDFWriter);
begin
  Dictionary.Write(AWriter);
end;

procedure TdxPDFCustomProperties.SetDictionary(const AValue: TdxPDFDictionary);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FDictionary));
end;

{ TdxPDFBlendModeDictionary }

class function TdxPDFBlendModeDictionary.ToString
  (AValue: TdxPDFBlendMode): string;
begin
  Result := Map[AValue];
end;

class function TdxPDFBlendModeDictionary.ToValue(const AString: string)
  : TdxPDFBlendMode;
begin
  if not TryGetValue(AString, Result) then
    Result := bmNormal;
end;

class function TdxPDFBlendModeDictionary.TryGetValue(const AKey: string;
  out AValue: TdxPDFBlendMode): Boolean;
var
  AIndex: TdxPDFBlendMode;
begin
  for AIndex := Low(Map) to High(Map) do
    if Map[AIndex] = AKey then
    begin
      AValue := AIndex;
      Exit(True);
    end;
  Result := False;
end;

{ TdxPDFCommandOperandStack }

constructor TdxPDFCommandOperandStack.Create;
begin
  inherited Create;
  FStack := TObjectStack<TdxPDFBase>.Create;
end;

destructor TdxPDFCommandOperandStack.Destroy;
begin
  FreeAndNil(FStack);
  inherited Destroy;
end;

function TdxPDFCommandOperandStack.TryPopLastName: string;
var
  ALastIndex: Integer;
  ALastParameter: TdxPDFBase;
begin
  ALastIndex := FStack.Count - 1;
  if ALastIndex < 0 then
    TdxPDFUtils.RaiseTestException('List index out of bounds');
  ALastParameter := PopAsObject;
  if ALastParameter <> nil then
  begin
    if (ALastParameter <> nil) and
      (ALastParameter.ObjectType in [otName, otString]) then
    begin
      Result := TdxPDFString(ALastParameter).Value;
      dxPDFFreeObject(ALastParameter);
      Exit;
    end;
    Result := '';
    Push(ALastParameter);
  end;
end;

function TdxPDFCommandOperandStack.PopAsArray: TdxPDFArray;
var
  AObject: TdxPDFBase;
begin
  AObject := PopAsObject;
  if (AObject <> nil) and (AObject.ObjectType = otArray) then
    Result := TdxPDFArray(AObject)
  else
    Result := nil;
end;

function TdxPDFCommandOperandStack.PopAsBytes: TBytes;
var
  AResult: string;
  I: Integer;
begin
  AResult := PopAsString;
  SetLength(Result, Length(AResult));
  for I := 0 to Length(Result) - 1 do
    Result[I] := Byte(AResult[I + 1]);
end;

function TdxPDFCommandOperandStack.PopAsInteger: Integer;
var
  AObject: TdxPDFBase;
begin
  Result := 0;
  AObject := PopAsObject;
  if AObject <> nil then
    try
      if AObject.ObjectType = otInteger then
        Result := TdxPDFInteger(AObject).Value
    finally
      dxPDFFreeObject(AObject);
    end;
end;

function TdxPDFCommandOperandStack.PopAsObject: TdxPDFBase;
begin
  Result := nil;
  if FStack.Count > 0 then
    Result := FStack.Extract
  else
    try
      TdxPDFUtils.RaiseTestException('Stack is empty');
    except
    end;
end;

function TdxPDFCommandOperandStack.PopAsSingle: Single;
var
  AObject: TdxPDFBase;
begin
  Result := 0;
  AObject := PopAsObject;
  if AObject <> nil then
    try
      case AObject.ObjectType of
        otDouble:
          Result := TdxPDFDouble(AObject).Value;
        otInteger:
          Result := TdxPDFInteger(AObject).Value;
      end;
    finally
      dxPDFFreeObject(AObject);
    end;
end;

function TdxPDFCommandOperandStack.PopAsString: string;
var
  AObject: TdxPDFBase;
begin
  Result := '';
  AObject := PopAsObject;
  if AObject <> nil then
    try
      case AObject.ObjectType of
        otName:
          Result := TdxPDFName(AObject).Value;
        otString:
          Result := TdxPDFString(AObject).Value;
      end;
    finally
      dxPDFFreeObject(AObject);
    end;
end;

procedure TdxPDFCommandOperandStack.Clear;
begin
  FStack.Clear;
end;

procedure TdxPDFCommandOperandStack.Push(AObject: TdxPDFBase);
begin
  FStack.Push(AObject);
end;

function TdxPDFCommandOperandStack.GetCount: Integer;
begin
  Result := FStack.Count;
end;

{ TdxPDFFontMetricsMetadata }

class function TdxPDFFontMetricsMetadata.Create: TdxPDFFontMetricsMetadata;
begin
  Result.FIsNull := True;
end;

class function TdxPDFFontMetricsMetadata.Create(AAscent, ADescent,
  AEmSize: Double): TdxPDFFontMetricsMetadata;
begin
  Result.FAscent := AAscent;
  Result.FDescent := ADescent;
  Result.FEmSize := AEmSize;
  Result.FIsNull := False;
end;

function TdxPDFFontMetricsMetadata.IsNull: Boolean;
begin
  Result := FIsNull;
end;

function TdxPDFFontMetricsMetadata.GetHeight: Double;
begin
  Result := Max(1, FAscent - FDescent);
end;

end.
