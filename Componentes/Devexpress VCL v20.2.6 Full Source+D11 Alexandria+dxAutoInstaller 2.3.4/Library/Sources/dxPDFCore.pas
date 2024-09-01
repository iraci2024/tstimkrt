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

unit dxPDFCore;

{$I cxVer.inc}
{$IFNDEF DELPHIXE3}
{$M-}
{$ENDIF}
{ .$DEFINE DXPDF_DONT_COMPRESS_STREAMS }

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, SysUtils, Graphics, Windows, Classes, Controls, Generics.Defaults,
  Generics.Collections, dxCoreClasses,
  cxClasses, cxGraphics, cxGeometry, dxGDIPlusAPI, dxGDIPlusClasses,
  dxProtectionUtils, dxThreading, dxPDFParser,
  dxPDFStreamFilter, dxPDFCharacterMapping, dxPDFBase, dxPDFTypes, dxPDFText,
  dxFontFile, dxPDFEncryption,
  dxPDFRecognizedObject, dxPDFImageUtils, dxXMLDoc, dxX509Certificate;

const
  // HitTests bits
  hcButton = 2; // for internal use
  hcHyperlink = 8;
  hcText = 4;
  hcTextBox = 12; // for internal use
  hcAnnotationObject = 16; // for internal use
  hcAttachment = 8192;

  dxPDFDocumentFontCacheSize: Integer = 32; // for internal use
  dxPDFAllRecognitionObjects = [rmAnnotations, rmImages, rmText];

type
  TdxPDFAcroFormActionField = class;
  TdxPDFAcroFormField = class;
  TdxPDFAcroFormFieldClass = class of TdxPDFAcroFormField;
  TdxPDFCatalog = class;
  TdxPDFCommandList = class;
  TdxPDFCustomAction = class;
  TdxPDFCustomAnnotation = class;
  TdxPDFCustomColorSpace = class;
  TdxPDFCustomCommand = class;
  TdxPDFCustomCommandClass = class of TdxPDFCustomCommand;
  TdxPDFCustomDestination = class;
  TdxPDFCustomEncoding = class;
  TdxPDFCustomFont = class;
  TdxPDFCustomShading = class;
  TdxPDFCustomSoftMask = class;
  TdxPDFCustomTree = class;
  TdxPDFDocumentImage = class;
  TdxPDFDocumentImageData = class;
  TdxPDFDocumentImageDataStorage = class;
  TdxPDFDocumentRepository = class;
  TdxPDFDocumentState = class;
  TdxPDFFileAttachment = class;
  TdxPDFFileAttachmentList = class;
  TdxPDFFontDataStorage = class;
  TdxPDFForm = class;
  TdxPDFGraphicsStateParameters = class;
  TdxPDFGroupForm = class;
  TdxPDFHyperlink = class;
  TdxPDFHyperlinkList = class;
  TdxPDFInteractiveForm = class;
  TdxPDFInteractiveFormField = class;
  TdxPDFInteractiveFormFieldCollection = class;
  TdxPDFLineStyle = class;
  TdxPDFObject = class;
  TdxPDFObjectClass = class of TdxPDFObject;
  TdxPDFObjectList = class;
  TdxPDFPage = class;
  TdxPDFPages = class;
  TdxPDFPageTreeObject = class;
  TdxPDFPageTreeObjectList = class;
  TdxPDFReaderDictionary = class;
  TdxPDFRecognizedContent = class;
  TdxPDFResources = class;
  TdxPDFTilingPattern = class;
  TdxPDFWriterHelper = class;

  TdxPDFAnnotationFlags = (afNone = $000, afInvisible = $001, afHidden = $002,
    afPrint = $004, afNoZoom = $008, afNoRotate = $010, afNoView = $020,
    afReadOnly = $040, afLocked = $080, afToggleNoView = $100,
    afLockedContents = $200); // for internal use
  TdxPDFAnnotationHighlightingMode = (ahmNone, ahmInvert, ahmOutline, ahmPush,
    ahmToggle); // for internal use
  TdxPDFAssociatedFileRelationship = (frSource, frData, frAlternative,
    frSupplement, frEncryptedPayload, frUnspecified);
  TdxPDFInteractiveFormFieldFlags = (ffNone = $0000000, ffReadOnly = $0000001,
    ffRequired = $0000002, ffNoExport = $0000004, ffMultiline = $0001000,
    ffPassword = $0002000, ffNoToggleToOff = $0004000, ffRadio = $0008000,
    ffPushButton = $0010000, ffCombo = $0020000, ffEdit = $0040000,
    ffSort = $0080000, ffFileSelect = $0100000, ffMultiSelect = $0200000,
    ffDoNotSpellCheck = $0400000, ffDoNotScroll = $0800000, ffComb = $1000000,
    ffRichText = $2000000, ffRadiosInUnison = $2000000,
    ffCommitOnSelChange = $4000000); // for internal use
  TdxPDFDocumentImageSMaskInDataType = (dtNone = 0, dtFromImage = 1,
    dtFromImagePreBlended = 2);
  TdxPDFTargetMode = (tmXYZ, tmFit, tmFitHorizontally, tmFitVertically,
    tmFitRectangle, tmFitBBox, tmFitBBoxHorizontally, tmFitBBoxVertically);
  // for internal use
  TdxPDFTextJustification = (tjLeftJustified, tjCentered, tjRightJustified);
  // for internal use
  TdxPDFTilingType = (ttConstantSpacing, ttNoDistortion, ttFasterTiling);
  // for internal use

  TdxPDFDeferredObjectInfo = record // for internal use
    Key: string;
    Name: string;
    Number: Integer;
    SourceObject: TdxPDFBase;
  end;

  TdxPDFExportParameters = class // for internal use
  strict private
    FDocumentState: TdxPDFDocumentState;
  public
    Angle: TcxRotationAngle;
    Annotations: TdxPDFReferencedObjects;
    Bounds: TdxRectF;
    CancelCallback: TdxTaskCancelCallback;
    ScaleFactor: Single;

    constructor Create; overload;
    constructor Create(AState: TdxPDFDocumentState); overload;
    function IsCanceled: Boolean;

    property DocumentState: TdxPDFDocumentState read FDocumentState;
  end;

  { TdxPDFFontRegistratorParameters }

  TdxPDFFontRegistratorParameters = record // for internal use
  strict private
    FIsItalic: Boolean;
    FName: string;
    FWeight: Integer;
  public
    class function Create(const AName: string; AWeight: Integer;
      AIsItalic: Boolean): TdxPDFFontRegistratorParameters; static;
    property IsItalic: Boolean read FIsItalic;
    property Name: string read FName;
    property Weight: Integer read FWeight;
  end;

  { TdxPDFFontDescriptorData }

  TdxPDFFontDescriptorData = record // for internal use
  strict private
    FAscent: Double;
    FBBox: TdxRectF;
    FBold: Boolean;
    FDescent: Double;
    FFontFlags: Integer;
    FItalicAngle: Double;
    FNumGlyphs: Integer;
  public
    class function Create(AFontMetrics: TdxFontFileFontMetrics;
      AFontFlags: Integer; const AItalicAngle: Double; ABold: Boolean;
      ANumGlyphs: Integer): TdxPDFFontDescriptorData; static;

    property Ascent: Double read FAscent;
    property BBox: TdxRectF read FBBox;
    property Bold: Boolean read FBold;
    property Descent: Double read FDescent;
    property Flags: Integer read FFontFlags;
    property ItalicAngle: Double read FItalicAngle;
    property NumGlyphs: Integer read FNumGlyphs;
  end;

  { TdxPDFDestinationInfo }

  TdxPDFDestinationInfo = record // for internal use
  strict private
    FDestination: TdxPDFCustomDestination;
    FName: string;
  public
    class function Create(ADestination: TdxPDFCustomDestination)
      : TdxPDFDestinationInfo; overload; static;
    class function Create(const AName: string): TdxPDFDestinationInfo;
      overload; static;
    class function Invalid: TdxPDFDestinationInfo; static;
    procedure Finalize;

    function GetDestination(ACatalog: TdxPDFCatalog; AInternal: Boolean)
      : TdxPDFCustomDestination;
    function IsValid: Boolean;
    function Write(AHelper: TdxPDFWriterHelper): TdxPDFBase;

    property Name: string read FName;
  end;

  { TdxPDFTarget }

  TdxPDFTarget = record // for internal use
  strict private
    FHeight: Double;
    FMode: TdxPDFTargetMode;
    FPageIndex: Integer;
    FWidth: Double;
    FX: Single;
    FY: Single;
    FZoom: Single;
  public
    class function Create(APageIndex: Integer; X, Y, AZoom: Single)
      : TdxPDFTarget; overload; static;
    class function Create(AMode: TdxPDFTargetMode; APageIndex: Integer)
      : TdxPDFTarget; overload; static;
    class function Create(AMode: TdxPDFTargetMode; APageIndex: Integer;
      const R: TdxRectF): TdxPDFTarget; overload; static;
    class function Create(AMode: TdxPDFTargetMode; APageIndex: Integer;
      X, Y: Single): TdxPDFTarget; overload; static;
    class function CreateEx(AMode: TdxPDFTargetMode; APageIndex: Integer;
      X, Y, AWidth, AHeight, AZoom: Single): TdxPDFTarget; overload; static;
    class function Invalid: TdxPDFTarget; static;
    function IsValid: Boolean;

    property Height: Double read FHeight;
    property Mode: TdxPDFTargetMode read FMode;
    property PageIndex: Integer read FPageIndex;
    property Width: Double read FWidth;
    property X: Single read FX;
    property Y: Single read FY;
    property Zoom: Single read FZoom;
  end;

  { TdxPDFInteractiveOperation }

  TdxPDFInteractiveOperation = record // for internal use
  strict private
    FAction: TdxPDFCustomAction;
    FDestination: TdxPDFCustomDestination;
    function GetTarget: TdxPDFTarget;

    property Destination: TdxPDFCustomDestination read FDestination;
  public
    class function Create(AAction: TdxPDFCustomAction)
      : TdxPDFInteractiveOperation; overload; static;
    class function Create(AAction: TdxPDFCustomAction;
      ADestination: TdxPDFCustomDestination): TdxPDFInteractiveOperation;
      overload; static;
    class function Invalid: TdxPDFInteractiveOperation; static;
    function IsValid: Boolean;

    property Action: TdxPDFCustomAction read FAction;
    property Target: TdxPDFTarget read GetTarget;
  end;

  { IdxPDFInteractivityController }

  IdxPDFInteractivityController = interface // for internal use
    ['{12BCE71F-D47A-4354-8049-A88730C9EDF3}']
    procedure ExecuteOperation(AField: TdxPDFAcroFormActionField);
    procedure GoToFirstPage;
    procedure GoToLastPage;
    procedure GoToNextPage;
    procedure GoToPrevPage;
    procedure OpenURI(const AURI: string);
    procedure ShowDocumentPosition(const ATarget: TdxPDFTarget);
  end;

  { IdxPDFAnnotationAppearanceBuilder }

  IdxPDFAnnotationAppearanceBuilder = interface // for internal use
    ['{8341D820-A735-44F5-9AC8-4DBF060702D9}']
    procedure RebuildAppearance(AForm: TdxPDFForm);
  end;

  { IdxPDFCommandInterpreter }

  IdxPDFCommandInterpreter = interface // for internal use
    ['{0F9503DE-2E5A-4785-A6CE-8FC4B2F51D75}']
    function GetActualSize: TSize;
    function GetBounds: TdxRectF;
    function GetDeviceTransformationMatrix: TdxPDFTransformationMatrix;
    function GetRotationAngle: Single;
    function GetTransformMatrix: TdxPDFTransformationMatrix;

    function CreateTilingBitmap(APattern: TdxPDFTilingPattern;
      const ASize, AKeySize: TSize; const AColor: TdxPDFColor): TcxBitmap32;
    function TransformSize(const AMatrix: TdxPDFTransformationMatrix;
      const ABoundingBox: TdxPDFRectangle): TdxSizeF;

    procedure ExecuteCommand(ACommands: TdxPDFCommandList); overload;
    procedure ExecuteCommand(const AInterpreter: IdxPDFCommandInterpreter;
      ACommands: TdxPDFCommandList); overload;
    procedure UnknownCommand(const AName: string);

    // Drawing
    procedure AppendPathBezierSegment(const P2, AEndPoint: TdxPointF); overload;
    procedure AppendPathBezierSegment(const P1, P2, P3: TdxPointF); overload;
    procedure AppendPathLineSegment(const AEndPoint: TdxPointF);
    procedure AppendRectangle(X, Y, AWidth, AHeight: Double);
    procedure BeginPath(const AStartPoint: TdxPointF);
    procedure Clip(AUseNonzeroWindingRule: Boolean);
    procedure ClosePath;
    procedure ClipAndClearPaths;
    procedure DrawImage(AImage: TdxPDFDocumentImage);
    procedure DrawForm(AForm: TdxPDFForm);
    procedure DrawShading(AShading: TdxPDFCustomShading);
    procedure DrawTransparencyGroup(AForm: TdxPDFGroupForm);
    procedure FillPaths(AUseNonzeroWindingRule: Boolean);
    procedure SetColorForNonStrokingOperations(const AColor: TdxPDFColor);
    procedure SetColorForStrokingOperations(const AColor: TdxPDFColor);
    procedure SetColorSpaceForNonStrokingOperations
      (AColorSpace: TdxPDFCustomColorSpace);
    procedure SetColorSpaceForStrokingOperations(AColorSpace
      : TdxPDFCustomColorSpace);
    procedure SetFlatnessTolerance(AValue: Double);
    procedure SetLineCapStyle(ALineCapStyle: TdxPDFLineCapStyle);
    procedure SetLineJoinStyle(ALineJoinStyle: TdxPDFLineJoinStyle);
    procedure SetLineStyle(ALineStyle: TdxPDFLineStyle);
    procedure SetLineWidth(ALineWidth: Single);
    procedure StrokePaths;
    function TransformShadingPoint(APoint: TdxPointF): TdxPointF;
    // Text
    procedure BeginText;
    procedure EndText;
    procedure SetCharacterSpacing(ASpacing: Single);
    procedure SetMiterLimit(AMiterLimit: Single);
    procedure SetTextFont(AFont: TdxPDFCustomFont; AFontSize: Double);
    procedure SetTextLeading(ALeading: Double);
    procedure SetTextHorizontalScaling(AValue: Double);
    procedure SetTextMatrix(const AOffset: TdxPointF); overload;
    procedure SetTextMatrix(const AMatrix: TdxPDFTransformationMatrix);
      overload;
    procedure SetTextRenderingMode(AMode: TdxPDFTextRenderingMode);
    procedure SetTextRise(AValue: Double);
    procedure SetWordSpacing(AWordSpacing: Double);
    procedure DrawString(const AData: TBytes; const AOffsets: TDoubleDynArray);
    procedure MoveToNextLine;
    // Graphics State
    procedure ApplyGraphicsStateParameters(AParameters
      : TdxPDFGraphicsStateParameters);
    procedure UpdateTransformationMatrix(const AMatrix
      : TdxPDFTransformationMatrix);
    procedure SaveGraphicsState;
    procedure SetRenderingIntent(AValue: TdxPDFRenderingIntent);
    procedure RestoreGraphicsState;

    property ActualSize: TSize read GetActualSize;
    property Bounds: TdxRectF read GetBounds;
    property DeviceTransformMatrix: TdxPDFTransformationMatrix
      read GetDeviceTransformationMatrix;
    property RotationAngle: Single read GetRotationAngle;
    property TransformMatrix: TdxPDFTransformationMatrix
      read GetTransformMatrix;
  end;

  TdxPDFShadingInfo = record // for internal use
    Graphics: TdxGPCanvas;
    Interpreter: IdxPDFCommandInterpreter;
    NeedDrawBackground: Boolean;
    Shading: TdxPDFCustomShading;
    Size: TdxSizeF;
    TransformMatrix: TdxPDFTransformationMatrix;
    UseTransparency: Boolean;
  end;

  TdxPDFFontInfo = record // for internal use
  strict private
    function GetFontLineSize: Single;
  public
    FontData: TObject;
    FontSize: Single;
    property FontLineSize: Single read GetFontLineSize;
  end;

  { IdxPDFShadingPainter }

  IdxPDFShadingPainter = interface // for internal use
    ['{07E917B6-A92E-4B0D-B5ED-2DB7C94B0182}']
    procedure Draw(const AShadingInfo: TdxPDFShadingInfo);
  end;

  { IdxPDFTillingPainter }

  IdxPDFTillingPainter = interface // for internal use
    ['{07E917B6-A92E-4B0D-B5ED-2DB7C94B0182}']
    function CreateTilingBitmap(APattern: TdxPDFTilingPattern;
      const ASize, AKeySize: TSize; const AColor: TdxPDFColor): TcxBitmap32;
    procedure Draw(const AShadingInfo: TdxPDFShadingInfo);
  end;

  { IdxPDFInteractiveObject }

  IdxPDFInteractiveObject = interface // for internal use
    ['{C19910F1-1382-4619-BFAA-83E9DBA6139F}']
    function GetCursor: TCursor; // for internal use
    function GetPageIndex: Integer; // for internal use
    function GetRect: TdxRectF; // for internal use
    function IsResetFocusingNeeded: Boolean; // for internal use
    procedure ExecuteOperation(const AController
      : IdxPDFInteractivityController); // for internal use
  end;

  { IdxPDFCodePointMapping }

  IdxPDFCodePointMapping = interface // for internal use
    ['{610A2970-EF02-4556-A15B-EA43DADF4128}']
    function UpdateCodePoints(const ACodePoints: TSmallIntDynArray;
      AUseEmbeddedFontEncoding: Boolean): Boolean;
  end;

  { IdxPDFHintableObject }

  IdxPDFHintableObject = interface(IdxPDFInteractiveObject) // for internal use
    ['{D2D693F3-0536-42BE-8626-AD245A592E22}']
    function GetHint: string; // for internal use
  end;

  TdxPDFWriterGetObjectNumber = function(AObject: TdxPDFObject)
    : Integer of object; // for internal use

  { TdxPDFWriterArray }

  TdxPDFWriterArray = class(TdxPDFArray) // for internal use
  strict private
    FHelper: TdxPDFWriterHelper;
  public
    constructor Create(AHelper: TdxPDFWriterHelper);
    procedure AddReference(const AData: TdxPDFBase); overload;
    procedure AddReference(const AObject: TdxPDFObject); overload;
  end;

  { TdxPDFWriterDictionary }

  TdxPDFWriterDictionary = class(TdxPDFDictionary) // for internal use
  strict private
    FHelper: TdxPDFWriterHelper;
    FStreamData: TBytes;
    FStreamDataCanEncrypt: Boolean;
  protected
    procedure Write(AWriter: TdxPDFWriter); override;
    procedure WriteStream(AWriter: TdxPDFWriter); override;
  public
    constructor Create(AHelper: TdxPDFWriterHelper);
    procedure Add(const AKey: string; AEncoding: TdxPDFCustomEncoding);
      overload;
    procedure Add(const AKey: string; AMask: TdxPDFCustomSoftMask); overload;
    procedure Add(const AKey: string; AObject: TdxPDFCustomColorSpace);
      overload;
    procedure Add(const AKey: string; AObjectList: TdxPDFObjectList); overload;
    procedure Add(const AKey: string; const AColor: TdxPDFColor); overload;
    procedure Add(const AKey: string;
      const ADestinationInfo: TdxPDFDestinationInfo); overload;
    procedure Add(const AKey: string; const AList: TStringList); overload;
    procedure AddInline(const AKey: string;
      const ATree: TdxPDFCustomTree); overload;
    procedure AddInline(const AKey: string; AObject: TdxPDFObject); overload;
    procedure AddNameOrReference(const AKey: string; AObject: TdxPDFObject);
    procedure AddReference(const AKey: string; AData: TdxPDFBase); overload;
    procedure AddReference(const AKey: string; AObject: TdxPDFObject); overload;
    procedure AddReference(const AKey: string; const AData: TBytes;
      ASkipIfNull: Boolean = True); overload; override;
    procedure SetAppearance(AResources: TdxPDFResources;
      ACommands: TdxPDFCommandList);
    procedure SetStreamData(const AData: TBytes); overload;
    procedure SetStreamData(const AData: TBytes;
      ACanCompress, ACanEncrypt: Boolean); overload;
    procedure SetTextJustification(AValue: TdxPDFTextJustification);
  end;

  { TdxPDFWriterHelper }

  TdxPDFWriterHelper = class // for internal use
  strict private
    FEncryptionInfo: IdxPDFEncryptionInfo;
    FGetObjectNumberFunc: TdxPDFWriterGetObjectNumber;
    FTemporaryObjects: TcxObjectList;

    function GetEncryptMetadata: Boolean;
  public
    constructor Create(AGetObjectNumberFunc: TdxPDFWriterGetObjectNumber;
      const AEncryptionInfo: IdxPDFEncryptionInfo = nil);
    destructor Destroy; override;
    function CreateArray: TdxPDFWriterArray;
    function CreateDictionary: TdxPDFWriterDictionary;
    function CreateIndirectObject(AData: TdxPDFBase): TdxPDFObject;
    function CreateStream(ADictionary: TdxPDFWriterDictionary;
      const AData: TBytes): TdxPDFObject; overload;
    function CreateStream(const AData: TBytes): TdxPDFObject; overload;
    function GetNameOrReference(AObject: TdxPDFObject): TdxPDFBase;
    function PrepareToWrite(AColorSpace: TdxPDFCustomColorSpace)
      : TdxPDFBase; overload;
    function RegisterIndirectObject(AObject: TdxPDFObject): Integer;
    //
    property EncryptMetadata: Boolean read GetEncryptMetadata;
  end;

  { TdxPDFObject }

  TdxPDFObject = class(TdxPDFBase)
  strict private
    FParent: TObject;
    FRepository: TdxPDFDocumentRepository;
  protected
    function GetObject(const AName: string; ASourceDictionary: TdxPDFDictionary;
      out AObject: TdxPDFBase): Boolean; // for internal use
    function GetRepository: TdxPDFDocumentRepository; virtual;
    // for internal use
    function Write(AHelper: TdxPDFWriterHelper): TdxPDFBase; reintroduce;
      overload; virtual;
    procedure CreateSubClasses; virtual; // for internal use
    procedure DestroySubClasses; virtual; // for internal use
    procedure Initialize; virtual; // for internal use
    procedure Read(ADictionary: TdxPDFReaderDictionary); virtual;
    // for internal use
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); virtual;
    // for internal use
    procedure SetParent(const AValue: TObject); virtual; // for internal use
    procedure SetRepository(const AValue: TdxPDFDocumentRepository); virtual;
    // for internal use
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); reintroduce; overload; virtual;

    procedure RaiseWriteNotImplementedException;

    property Parent: TObject read FParent write SetParent; // for internal use
    property Repository: TdxPDFDocumentRepository read GetRepository
      write SetRepository; // for internal use
  public
    class function GetTypeName: string; virtual; // for internal use
    constructor Create; overload; override;
    constructor Create(AParent: TObject); overload; virtual; // for internal use
    destructor Destroy; override;
  end;

  { TdxPDFGlyphMapper }

  TdxPDFGlyphMapper = class // for internal use
  public
    function CreateGlyphRun: TdxPDFGlyphRun; virtual; abstract;
    function GetGlyphIndex(ACh: Char): Integer; virtual; abstract;
    function MapString(const S: string; AFlags: TdxPDFGlyphMappingFlags)
      : TdxPDFGlyphMappingResult; virtual; abstract;
  end;

  { TdxPDFFullTrustGlyphMapper }

  TdxPDFFullTrustGlyphMapper = class(TdxPDFGlyphMapper) // for internal use
  strict private
    FCMapTables: TList<TdxFontFileCMapCustomFormatRecord>;
    FFactor: Single;
    FFontFile: TdxFontFile;
    FMappedGlyphsCache: TdxPDFIntegerIntegerDictionary;

    function MapStringWithoutCTL(const AStr: string;
      AFlags: TdxPDFGlyphMappingFlags): TdxPDFGlyphMappingResult;
  protected
    function IsWritingOrderControl(AChar: Char): Boolean;

    property FontFile: TdxFontFile read FFontFile;
  public
    constructor Create(AFontFile: TdxFontFile);
    destructor Destroy; override;

    function CreateGlyph(AGlyphIndex: Integer; ACh: Char;
      AWidth, AGlyphOffset: Double): TdxPDFGlyph; virtual; abstract;
    function CreateGlyphRun(const AGlyphs: TdxPDFGlyphList): TdxPDFGlyphRun;
      reintroduce; overload; virtual; abstract;

    class function GetCMapEntryPriority
      (AEntry: TdxFontFileCMapCustomFormatRecord; AIsSymbolic: Boolean)
      : Integer;
    function GetGlyphIndex(ACharacter: Char): Integer; override;
    function MapString(const AStr: string; AFlags: TdxPDFGlyphMappingFlags)
      : TdxPDFGlyphMappingResult; override;
  end;

  { TdxPDFEmbeddedGlyphMapper }

  TdxPDFEmbeddedGlyphMapper = class(TdxPDFFullTrustGlyphMapper)
  // for internal use
  public
    function CreateGlyphRun: TdxPDFGlyphRun; overload; override;
    function CreateGlyph(AGlyphIndex: Integer; ACh: Char;
      AWidth, AGlyphOffset: Double): TdxPDFGlyph; override;
    function CreateGlyphRun(const AGlyphs: TdxPDFGlyphList): TdxPDFGlyphRun;
      overload; override;
  end;

  { TdxPDFDocumentState }

  TdxPDFDocumentState = class(TdxPDFObject) // for internal use
  strict private
    FRotationAngle: TcxRotationAngle;
    FOnRotationAngleChanged: TNotifyEvent;
    function GetImageDataStorage: TdxPDFDocumentImageDataStorage;
    function GetFontDataStorage: TdxPDFFontDataStorage;
    procedure SetRotationAngle(const AValue: TcxRotationAngle);
    procedure CalculateFontParameters(ACommand: TdxPDFCustomCommand;
      out AFontName: string; out AFontStyle: TdxGPFontStyle;
      var APitchAndFamily: Byte; var AIsEmptyFontName: Boolean);
  protected
    function GetRepository: TdxPDFDocumentRepository; override;

    function CreateFontData(const AFontFamilyName: string;
      AFontStyle: TdxGPFontStyle): TObject;
    function GetPageIndex(APage: TdxPDFPage): Integer;
    function SearchFontData(AFontCommand: TdxPDFCustomCommand): TObject;
  public
    property FontDataStorage: TdxPDFFontDataStorage read GetFontDataStorage;
    property ImageDataStorage: TdxPDFDocumentImageDataStorage
      read GetImageDataStorage;
    property RotationAngle: TcxRotationAngle read FRotationAngle
      write SetRotationAngle;

    property OnRotationAngleChanged: TNotifyEvent read FOnRotationAngleChanged
      write FOnRotationAngleChanged;
  end;

  { TdxPDFStreamObject }

  TdxPDFStreamObject = class(TdxPDFObject) // for internal use
  strict private
    FStream: TdxPDFStream;
    function GetUncompressedData: TBytes;
    procedure SetStream(const AValue: TdxPDFStream);
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
    procedure WriteData(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); virtual;

    function GetData: TBytes; virtual;

    property Stream: TdxPDFStream read FStream write SetStream;
    property UncompressedData: TBytes read GetUncompressedData;
  end;

  { TdxPDFCustomEncoding }

  TdxPDFCustomEncoding = class(TdxPDFObject) // for internal use
  strict private
    FFontFileEncoding: TdxFontFileCustomEncoding;
  protected
    function Write(AHelper: TdxPDFWriterHelper): TdxPDFBase; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;

    function GetShortName: string; virtual;
    function GetFontFileEncodingClass: TdxFontFileCustomEncodingClass; virtual;
    function UseShortWrite: Boolean; virtual;
  public
    function GetStringData(const ABytes: TBytes;
      const AGlyphOffsets: TDoubleDynArray): TdxPDFStringCommandData;
      virtual; abstract;
    function ShouldUseEmbeddedFontEncoding: Boolean; virtual; abstract;

    property FontFileEncoding: TdxFontFileCustomEncoding read FFontFileEncoding;
  end;

  { TdxPDFCustomFontDescriptor }

  TdxPDFCustomFontDescriptorClass = class of TdxPDFCustomFontDescriptor;

  TdxPDFCustomFontDescriptor = class(TdxPDFObject) // for internal use
  strict private
  type
    TdxPDFPopulateStandardFontDescriptorProc = procedure
      (ADictionary: TdxPDFDictionary) of object;
  strict private
    FAscent: Double;
    FAvgWidth: Double;
    FCapHeight: SmallInt;
    FCharSet: string;
    FCIDSetData: TBytes;
    FDescent: Double;
    FFlags: Integer;
    FFontBBox: TdxPDFRectangle;
    FFontFamily: string;
    FFontName: string;
    FFontStretch: TdxFontFileStretch;
    FFontWeight: Integer;
    FHasData: Boolean;
    FItalicAngle: Double;
    FLeading: Double;
    FMaxWidth: Double;
    FMissingWidth: Double;
    FStemH: Double;
    FStemV: Double;
    FXHeight: SmallInt;

    function GetActualAscent: Double;
    function GetActualDescent: Double;
    function GetFont: TdxPDFCustomFont;
    function GetFontBBox: TdxRectF;
    function GetHeight: Double;
    function IsFontMetricsInvalid: Boolean;
    procedure ReadFontStretch(ADictionary: TdxPDFDictionary);
    procedure ReadFontWeight(ADictionary: TdxPDFDictionary);

    class function GetActualFontName(const AFontName: string): string;
    class function GetFontDescriptorPopulationProc(const AFontName: string)
      : TdxPDFPopulateStandardFontDescriptorProc;
    class procedure PopulateCourierFontDescriptorDictionary
      (ADictionary: TdxPDFDictionary);
    class procedure PopulateCourierBoldFontDescriptorDictionary
      (ADictionary: TdxPDFDictionary);
    class procedure PopulateCourierBoldObliqueFontDescriptorDictionary
      (ADictionary: TdxPDFDictionary);
    class procedure PopulateCourierObliqueFontDescriptorDictionary
      (ADictionary: TdxPDFDictionary);
    class procedure PopulateHelveticaFontDescriptorDictionary
      (ADictionary: TdxPDFDictionary);
    class procedure PopulateHelveticaBoldFontDescriptorDictionary
      (ADictionary: TdxPDFDictionary);
    class procedure PopulateHelveticaBoldObliqueFontDescriptorDictionary
      (ADictionary: TdxPDFDictionary);
    class procedure PopulateHelveticaObliqueFontDescriptorDictionary
      (ADictionary: TdxPDFDictionary);
    class procedure PopulateStandardFontMap(AStandardFontNameMap
      : TdxPDFStringStringDictionary);
    class procedure PopulateSymbolFontDescriptorDictionary
      (ADictionary: TdxPDFDictionary);
    class procedure PopulateTimesBoldFontDescriptorDictionary
      (ADictionary: TdxPDFDictionary);
    class procedure PopulateTimesBoldItalicFontDescriptorDictionary
      (ADictionary: TdxPDFDictionary);
    class procedure PopulateTimesItalicFontDescriptorDictionary
      (ADictionary: TdxPDFDictionary);
    class procedure PopulateTimesNewRomanFontDescriptorDictionary
      (ADictionary: TdxPDFDictionary);
    class procedure PopulateZapfDingbatsFontDescriptorDictionary
      (ADictionary: TdxPDFDictionary);
  strict protected
    function GetOpenTypeFontFileData(ADictionary: TdxPDFReaderDictionary;
      ASuppressException: Boolean): TBytes;
    function GetStream(const AKey: string; ADictionary: TdxPDFReaderDictionary)
      : TdxPDFStream;
    function WriteOpenTypeFontData(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary; const AData: TBytes): Boolean;
  protected
    class function GetNormalWeight: Integer;
    procedure Initialize; override;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
    procedure WriteFontFile(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); virtual;

    property Font: TdxPDFCustomFont read GetFont;
  public
    class function CreateStandardDictionary(ARepository: TdxPDFCustomRepository;
      const AFontName: string): TdxPDFDictionary;
    class function GetTypeName: string; override;
    constructor Create(const ADescriptorData
      : TdxPDFFontDescriptorData); overload;

    property Ascent: Double read GetActualAscent write FAscent;
    property AvgWidth: Double read FAvgWidth write FAvgWidth;
    property CapHeight: SmallInt read FCapHeight;
    property Descent: Double read GetActualDescent write FDescent;
    property Flags: Integer read FFlags;
    property FontBBox: TdxRectF read GetFontBBox;
    property FontName: string read FFontName write FFontName;
    property FontStretch: TdxFontFileStretch read FFontStretch;
    property FontWeight: Integer read FFontWeight write FFontWeight;
    property HasData: Boolean read FHasData;
    property Height: Double read GetHeight;
    property ItalicAngle: Double read FItalicAngle;
    property MaxWidth: Double read FMaxWidth;
    property MissingWidth: Double read FMissingWidth;
    property XHeight: SmallInt read FXHeight;
  end;

  { TdxPDFCIDSystemInfo }

  TdxPDFCIDSystemInfo = class(TdxPDFObject)
  strict private
    FOrdering: string;
    FRegistry: string;
    FSupplement: Integer;
  protected
    procedure Initialize; override;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
  public
    class function GetTypeName: string; override;
    property Ordering: string read FOrdering;
    property Registry: string read FRegistry;
    property Supplement: Integer read FSupplement;
  end;

  { TdxPDFCustomFont }

  TdxPDFCustomFontClass = class of TdxPDFCustomFont;

  TdxPDFCustomFont = class(TdxPDFObject) // for internal use
  strict private
    FAvgGlyphWidth: SmallInt;
    FCMap: TdxPDFCharacterMapping;
    FEncoding: TdxPDFCustomEncoding;
    FFontProgramFacade: TObject;
    FListeners: TInterfaceList;
    FMetrics: TdxPDFFontMetricsMetadata;
    FName: string;
    FSubsetName: string;
    FToUnicode: TdxPDFToUnicodeMap;
    FUniqueName: string;

    function GetBoldWeight: Integer;
    function GetCMap: TdxPDFCharacterMapping;
    function GetItalic: Boolean;
    function GetFontBBox: TdxRectF;
    function GetFontProgramFacade: TObject;
    function GetForceBold: Boolean;
    function GetMaxGlyphWidth: Double;
    function GetMetrics: TdxPDFFontMetricsMetadata;
    function GetPitchAndFamily: Byte;
    function GetShouldUseEmbeddedFontEncoding: Boolean;
    function GetSubsetNameLength: Integer;
    function GetSubsetPrefixLength: Integer;
    function GetSymbolic: Boolean;
    function GetWeight: Integer;
    procedure SetCMap(const AValue: TdxPDFCharacterMapping);

    procedure ReadWidths(ADictionary: TdxPDFReaderDictionary);
  strict protected
    FBaseFont: string;
    FFontDescriptor: TdxPDFCustomFontDescriptor;
    FWidths: TDoubleDynArray;

    function HasFlag(AFlag: TdxFontFileFlags): Boolean;
    procedure SetEncoding(const AValue: TdxPDFCustomEncoding);
    procedure SetFontDescriptor(const AValue: TdxPDFCustomFontDescriptor);
    procedure ReadFontName;
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Initialize; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;

    function CreateFontProgramFacade: TObject; virtual;
    function CreateValidatedMetrics: TdxPDFFontMetricsMetadata; virtual;
    function GetFontDescriptorClass: TdxPDFCustomFontDescriptorClass; virtual;
    function GetFontDescriptorDictionary(ADictionary: TdxPDFReaderDictionary)
      : TdxPDFReaderDictionary; virtual;
    function GetFontDictionary(ADictionary: TdxPDFReaderDictionary)
      : TdxPDFReaderDictionary; virtual;
    function GetHeightFactor: Double; virtual;
    function GetWidthFactor: Double; virtual;
    function HasSizeAttributes: Boolean; virtual;
    function NeedWriteFontDescriptor: Boolean; virtual;
    procedure DoReadWidths(ADictionary: TdxPDFReaderDictionary); virtual;
    procedure ReadEncoding(ASourceObject: TdxPDFBase); virtual;
    procedure ReadFontDescriptor(ADictionary: TdxPDFReaderDictionary); virtual;
    procedure ReadToUnicode(ADictionary: TdxPDFReaderDictionary); virtual;
    procedure AddListener(AListener: IdxPDFDocumentSharedObjectListener);
    procedure RemoveListener(AListener: IdxPDFDocumentSharedObjectListener);
  public
    class function GetSubTypeName: string; virtual;
    class function GetTypeName: string; override;
    class function Parse(AOwner: TdxPDFObject;
      ADictionary: TdxPDFReaderDictionary): TdxPDFCustomFont; static;
    constructor Create(AOwner: TObject); overload; override;
    function GetCharacterWidth(ACharCode: Integer): Double; virtual; abstract;

    property AvgGlyphWidth: SmallInt read FAvgGlyphWidth;
    property BaseFont: string read FBaseFont;
    property BoldWeight: Integer read GetBoldWeight;
    property CMap: TdxPDFCharacterMapping read GetCMap write SetCMap;
    property Encoding: TdxPDFCustomEncoding read FEncoding;
    property FontBBox: TdxRectF read GetFontBBox;
    property FontDescriptor: TdxPDFCustomFontDescriptor read FFontDescriptor;
    property FontProgramFacade: TObject read GetFontProgramFacade;
    property ForceBold: Boolean read GetForceBold;
    property HeightFactor: Double read GetHeightFactor;
    property Italic: Boolean read GetItalic;
    property MaxGlyphWidth: Double read GetMaxGlyphWidth;
    property Metrics: TdxPDFFontMetricsMetadata read GetMetrics;
    property Name: string read FName;
    property PitchAndFamily: Byte read GetPitchAndFamily;
    property ShouldUseEmbeddedFontEncoding: Boolean
      read GetShouldUseEmbeddedFontEncoding;
    property Symbolic: Boolean read GetSymbolic;
    property UniqueName: string read FUniqueName;
    property Weight: Integer read GetWeight;
    property WidthFactor: Double read GetWidthFactor;
    property Widths: TDoubleDynArray read FWidths;
  end;

  { TdxPDFCustomColorSpace }

  TdxPDFCustomColorSpace = class(TdxPDFObject) // for internal use
  strict private
    FName: string;
    FAlternateColorSpace: TdxPDFCustomColorSpace;
    FComponentCount: Integer;
    procedure SetAlternateColorSpace(const AValue: TdxPDFCustomColorSpace);
    procedure SetComponentCount(const AValue: Integer);
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Initialize; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); overload; override;
    function Write(AHelper: TdxPDFWriterHelper): TdxPDFBase; override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;

    function GetComponentCount: Integer; virtual;
    function CanRead(ASize: Integer): Boolean; virtual;
    procedure CheckComponentCount; virtual;
    procedure DoRead(AArray: TdxPDFArray); virtual;
    procedure DoWrite(AHelper: TdxPDFWriterHelper;
      AArray: TdxPDFWriterArray); virtual;

    function GetDecodedImageScanlineSource(const ADecoratingSource
      : IdxPDFImageScanlineSource; const AImage: IdxPDFDocumentImage;
      AWidth: Integer): IdxPDFImageScanlineSource; virtual;

    property Name: string read FName write FName;
  public
    class function CreateColorSpace(const AName: string;
      AResources: TdxPDFResources): TdxPDFCustomColorSpace;
    class function Parse(ARepository: TdxPDFDocumentRepository;
      AObject: TdxPDFBase; AResources: TdxPDFResources = nil)
      : TdxPDFCustomColorSpace;

    function AlternateTransform(const AColor: TdxPDFColor)
      : TdxPDFColor; virtual;
    function CreateDefaultDecodeArray(ABitsPerComponent: Integer)
      : TdxPDFRanges; virtual;
    function Transform(const AComponents: TDoubleDynArray): TDoubleDynArray;
      overload; virtual;
    function Transform(const AColor: TdxPDFColor): TdxPDFColor; overload;
    function Transform(const AImage: IdxPDFDocumentImage;
      const AData: IdxPDFImageScanlineSource;
      const AParameters: TdxPDFImageParameters)
      : TdxPDFScanlineTransformationResult; overload; virtual;
    function Transform(const AData: IdxPDFImageScanlineSource; AWidth: Integer)
      : TdxPDFScanlineTransformationResult; overload; virtual;

    property AlternateColorSpace: TdxPDFCustomColorSpace
      read FAlternateColorSpace write SetAlternateColorSpace;
    property ComponentCount: Integer read GetComponentCount
      write SetComponentCount;
  end;

  { TdxPDFDeferredObject }

  TdxPDFDeferredObject = class(TdxPDFObject) // for internal use
  strict private
    FInfo: TdxPDFDeferredObjectInfo;
    FResolvedObject: TdxPDFObject;

    function GetResolvedObject: TdxPDFObject;
    function GetSourceObject: TdxPDFBase;
    procedure SetResolvedObject(const AValue: TdxPDFObject);
    procedure SetSourceObject(const AValue: TdxPDFBase);
    procedure ResolveObject;
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;

    function IsResolved: Boolean;

    property SourceObject: TdxPDFBase read GetSourceObject
      write SetSourceObject;
  public
    constructor Create(AOwner: TdxPDFObject;
      const AInfo: TdxPDFDeferredObjectInfo); reintroduce; overload;
    constructor Create(AOwner, AResolvedObject: TdxPDFObject);
      reintroduce; overload;

    property ResolvedObject: TdxPDFObject read GetResolvedObject
      write SetResolvedObject;
  end;

  { TdxPDFObjectList }

  TdxPDFObjectListClass = class of TdxPDFObjectList;

  TdxPDFObjectList = class(TdxPDFObject) // for internal use
  strict private
    FInternalObjects: TdxPDFReferencedObjectDictionary;
    FNames: TdxPDFNamedObjectDictionary;

    function GetCount: Integer;
    procedure DoAdd(AObjectList: TdxPDFObjectList; ANeedClear: Boolean);
    procedure DoReadObject(const AObjectName: string;
      ADictionary: TdxPDFReaderDictionary);
  private
    property InternalObjects: TdxPDFReferencedObjectDictionary
      read FInternalObjects;
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;

    class function GetTypePrefix: string; virtual;
    function GetTypeDictionaryKey: string; virtual;
    procedure ReadObject(AStructureObject: TdxPDFBase; ANumber: Integer;
      const AName: string); virtual;

    function GetObject(const AName: string): TdxPDFObject;
    function TryGetObjectName(const AObject: TdxPDFObject;
      out AName: string): Boolean;
    procedure Clear;
    procedure InternalAdd(const AName: string; AObject: TdxPDFObject);
    procedure ReadList(ADictionary: TdxPDFReaderDictionary);
    procedure WriteList(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary);
    procedure ResolveObjects;
  public type
    TEnumProc = reference to procedure(AObject: TdxPDFObject);
  public
    function Add(AObject: TdxPDFObject): string;
    function AddReference(ANumber: Integer): string;
    procedure Append(AList: TdxPDFObjectList);
    procedure Assign(AList: TdxPDFObjectList);
    function Contains(const AName: string): Boolean;
    procedure Enum(AProc: TEnumProc);

    property Count: Integer read GetCount;
  end;

  { TdxPDFFonts }

  TdxPDFFonts = class(TdxPDFObjectList) // for internal use
  protected
    class function GetTypePrefix: string; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
  public
    class function GetTypeName: string; override;
    function GetFont(const AName: string): TdxPDFCustomFont;
  end;

  { TdxPDFGraphicsStateParameters }

  TdxPDFGraphicsStateParameters = class(TdxPDFObject) // for internal use
  strict private
  type
    TAssignedValue = (gspFlatnessTolerance, gspFont, gspFontSize,
      gspLineCapStyle, gspLineJoinStyle, gspLineStyle, gspLineWidth,
      gspMiterLimit, gspNonStrokingColorAlpha, gspRenderingIntent, gspSoftMask,
      gspSmoothnessTolerance, gspStrokingColorAlpha, gspTextKnockout,
      gspBlendMode);
    TAssignedValues = set of TAssignedValue;
  strict private
    FAssignedValues: TAssignedValues;
    FBlendMode: TdxPDFBlendMode;
    FFlatnessTolerance: Double;
    FFont: TdxPDFCustomFont;
    FFontSize: Double;
    FIsSoftMaskChanged: Boolean;
    FLineCapStyle: TdxPDFLineCapStyle;
    FLineJoinStyle: TdxPDFLineJoinStyle;
    FLineStyle: TdxPDFLineStyle;
    FLineWidth: Double;
    FMiterLimit: Double;
    FNonStrokingColorAlpha: Double;
    FRenderingIntent: TdxPDFRenderingIntent;
    FSmoothnessTolerance: Double;
    FSoftMask: TdxPDFCustomSoftMask;
    FStrokingColorAlpha: Double;
    FTextKnockout: Boolean;

    procedure SetFont(const AValue: TdxPDFCustomFont);
    procedure SetLineStyle(const AValue: TdxPDFLineStyle);
    procedure SetLineWidth(const AValue: Double);
    procedure SetSoftMask(const AValue: TdxPDFCustomSoftMask);
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Initialize; override;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;

    property AssignedValues: TAssignedValues read FAssignedValues;
    property IsSoftMaskChanged: Boolean read FIsSoftMaskChanged;
  public
    class function GetTypeName: string; override;
    class function Parse(ADictionary: TdxPDFReaderDictionary)
      : TdxPDFGraphicsStateParameters;
    procedure Assign(AParameters: TdxPDFGraphicsStateParameters;
      ACheckAssignedValues: Boolean = True);

    property BlendMode: TdxPDFBlendMode read FBlendMode write FBlendMode;
    property Font: TdxPDFCustomFont read FFont write SetFont;
    property FontSize: Double read FFontSize write FFontSize;
    property LineCapStyle: TdxPDFLineCapStyle read FLineCapStyle
      write FLineCapStyle;
    property LineJoinStyle: TdxPDFLineJoinStyle read FLineJoinStyle
      write FLineJoinStyle;
    property LineStyle: TdxPDFLineStyle read FLineStyle write SetLineStyle;
    property LineWidth: Double read FLineWidth write SetLineWidth;
    property MiterLimit: Double read FMiterLimit write FMiterLimit;
    property NonStrokingColorAlpha: Double read FNonStrokingColorAlpha
      write FNonStrokingColorAlpha;
    property FlatnessTolerance: Double read FFlatnessTolerance
      write FFlatnessTolerance;
    property RenderingIntent: TdxPDFRenderingIntent read FRenderingIntent
      write FRenderingIntent;
    property SmoothnessTolerance: Double read FSmoothnessTolerance
      write FSmoothnessTolerance;
    property SoftMask: TdxPDFCustomSoftMask read FSoftMask write SetSoftMask;
    property StrokingColorAlpha: Double read FStrokingColorAlpha
      write FStrokingColorAlpha;
    property TextKnockout: Boolean read FTextKnockout write FTextKnockout;
  end;

  { TdxPDFGraphicsStateParametersList }

  TdxPDFGraphicsStateParametersList = class(TdxPDFObjectList)
  // for internal use
  protected
    class function GetTypePrefix: string; override;
    function GetTypeDictionaryKey: string; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
  public
    class function GetTypeName: string; override;
    function GetParameters(const AName: string): TdxPDFGraphicsStateParameters;
    procedure Add(const AName: string;
      AStateParameters: TdxPDFGraphicsStateParameters); overload;
  end;

  { TdxPDFPageContentItem }

  TdxPDFPageContentItem = class(TdxPDFStreamObject) // for internal use
  public
    class function GetTypeName: string; override;
  end;

  { TdxPDFXObject }

  TdxPDFXObject = class(TdxPDFStreamObject) // for internal use
  strict private
    FLock: TRTLCriticalSection;
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure InternalDraw(const AInterpreter
      : IdxPDFCommandInterpreter); virtual;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
  public
    class function GetTypeName: string; override;
    class function Parse(ARepository: TdxPDFCustomRepository;
      AStream: TdxPDFStream; const ASubtype: string = ''): TdxPDFXObject;
    procedure Draw(const AInterpreter: IdxPDFCommandInterpreter);
  end;

  { TdxPDFForm }

  TdxPDFForm = class(TdxPDFXObject) // for internal use
  strict private
    FBBox: TdxPDFRectangle;
    FCommands: TdxPDFCommandList;
    FMatrix: TdxPDFTransformationMatrix;
    FRepository: TdxPDFCustomRepository;
    FResources: TdxPDFResources;
    FStreamRef: TdxPDFStream;
    FUseOwnResources: Boolean;

    procedure CheckFormType(ADictionary: TdxPDFDictionary);
    function GetActualStream: TdxPDFStream;
    function GetCommands: TdxPDFCommandList;
    procedure SetMatrix(const AValue: TdxPDFTransformationMatrix);
    procedure SetResources(const AValue: TdxPDFResources);
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure InternalDraw(const AInterpreter
      : IdxPDFCommandInterpreter); override;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
    procedure WriteData(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
    //
    procedure ReleaseCircularReferencesAndFree;
    //
    property ActualStream: TdxPDFStream read GetActualStream;
  public
    class function CreateForm(ADictionary: TdxPDFDictionary;
      AParentResources: TdxPDFResources): TdxPDFForm; overload;
    class function CreateForm(AStream: TdxPDFStream;
      AParentResources: TdxPDFResources): TdxPDFForm; overload;
    class function GetTypeName: string; override;
    constructor Create(AOwner: TObject); overload; override;
    constructor Create(ACatalog: TdxPDFCatalog; const ABBox: TdxPDFRectangle);
      reintroduce; overload;
    function GetTransformationMatrix(const ARect: TdxPDFRectangle)
      : TdxPDFTransformationMatrix;
    procedure ReplaceCommands(const ACommandData: TBytes);

    property BBox: TdxPDFRectangle read FBBox write FBBox;
    property Commands: TdxPDFCommandList read GetCommands;
    property Matrix: TdxPDFTransformationMatrix read FMatrix write SetMatrix;
    property Resources: TdxPDFResources read FResources write SetResources;
  end;

  { TdxPDFCustomSoftMask }

  TdxPDFCustomSoftMask = class abstract(TdxPDFObject) // for internal use
  strict private
    FTransparencyFunction: TdxPDFObject;
    FTransparencyGroup: TdxPDFGroupForm;

    procedure SetTransparencyGroup(const AValue: TdxPDFGroupForm);
    procedure SetTransparencyFunction(const AValue: TdxPDFObject);
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
  public
    class function GetTypeName: string; override;
    class function Parse(ARepository: TdxPDFCustomRepository;
      ASourceObject: TdxPDFBase): TdxPDFCustomSoftMask;
    function IsSame(AMask: TdxPDFCustomSoftMask): Boolean; virtual;

    property TransparencyGroup: TdxPDFGroupForm read FTransparencyGroup
      write SetTransparencyGroup;
    property TransparencyFunction: TdxPDFObject read FTransparencyFunction
      write SetTransparencyFunction;
  end;

  { TdxPDFLuminositySoftMask }

  TdxPDFLuminositySoftMask = class(TdxPDFCustomSoftMask) // for internal use
  strict private
    FBackdropColor: TdxPDFColor;
  protected
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
  public
    class function GetTypeName: string; override;
  end;

  { TdxPDFAlphaSoftMask }

  TdxPDFAlphaSoftMask = class(TdxPDFCustomSoftMask) // for internal use
  public
    class function GetTypeName: string; override;
  end;

  { TdxPDFEmptySoftMask }

  TdxPDFEmptySoftMask = class(TdxPDFCustomSoftMask)
  protected
    function Write(AHelper: TdxPDFWriterHelper): TdxPDFBase; override;
  public
    class function GetTypeName: string; override;
    function IsSame(AMask: TdxPDFCustomSoftMask): Boolean; override;
  end;

  { TdxPDFTransparencyGroup }

  TdxPDFTransparencyGroup = class(TdxPDFObject) // for internal use
  strict private
  const
    ColorSpaceKey = 'CS';
    IsolatedKey = 'I';
    KnockoutKey = 'K';
    SubtypeKey = 'S';
  strict private
    FColorSpace: TdxPDFCustomColorSpace;
    FIsolated: Boolean;
    FKnockout: Boolean;
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
  public
    class function GetTypeName: string; override;
    property ColorSpace: TdxPDFCustomColorSpace read FColorSpace;
    property Isolated: Boolean read FIsolated;
    property Knockout: Boolean read FKnockout;
  end;

  { TdxPDFGroupForm }

  TdxPDFGroupForm = class(TdxPDFForm) // for internal use
  strict private
    FGroup: TdxPDFTransparencyGroup;

    function GetColorSpace: TdxPDFCustomColorSpace;
  protected
    procedure DestroySubClasses; override;
    procedure InternalDraw(const AInterpreter
      : IdxPDFCommandInterpreter); override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;

    property ColorSpace: TdxPDFCustomColorSpace read GetColorSpace;
  public
    class function GetTypeName: string; override;
  end;

  { TdxPDFDocumentImage }

  TdxPDFDocumentImage = class(TdxPDFXObject, IdxPDFDocumentImage)
  // for internal use
  strict private
    FBitsPerComponent: Integer;
    FColorKeyMask: TdxPDFRanges;
    FColorSpace: TdxPDFCustomColorSpace;
    FComponentCount: Integer;
    FDecodeRanges: TdxPDFRanges;
    FFilters: TdxPDFStreamFilters;
    FGUID: string;
    FHasMask: Boolean;
    FHeight: Integer;
    FID: string;
    FIntent: TdxPDFRenderingIntent;
    FListeners: TInterfaceList;
    FMask: TdxPDFDocumentImage;
    FMatte: TDoubleDynArray;
    FNeedInterpolate: Boolean;
    FSMaskInData: TdxPDFDocumentImageSMaskInDataType;
    FSoftMask: TdxPDFDocumentImage;
    FStructParent: Integer;
    FWidth: Integer;

    function GetFilters: TdxPDFStreamFilters;
    procedure SetColorSpace(const AValue: TdxPDFCustomColorSpace);
    procedure SetMask(const AValue: TdxPDFDocumentImage);
    procedure SetSoftMask(const AValue: TdxPDFDocumentImage);

    function GetCompressedData: TBytes;
    function HasValidStencilMask: Boolean;
    procedure CalculateComponentCount(ADictionary: TdxPDFDictionary);
    procedure DoRead(ADictionary: TdxPDFReaderDictionary);
    procedure ReadColorSpace(ADictionary: TdxPDFReaderDictionary);
    procedure ReadDecodeRanges(ADictionary: TdxPDFDictionary);
    procedure ReadMask(ADictionary: TdxPDFReaderDictionary);
    procedure ReadMatte(ADictionary: TdxPDFDictionary);
    procedure ReadSoftMask(ADictionary: TdxPDFReaderDictionary);
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    // IdxPDFDocumentImage
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
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Initialize; override;
    procedure InternalDraw(const AInterpreter
      : IdxPDFCommandInterpreter); override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); overload; override;
    procedure Read(ARepository: TdxPDFCustomRepository; AStream: TdxPDFStream);
      reintroduce; overload;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
    procedure WriteAsInline(AWriter: TdxPDFWriter; AResources: TdxPDFResources);
    procedure WriteData(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
    procedure WriteFilters(ADictionary: TdxPDFDictionary;
      AUseShortNames: Boolean; AHelper: TdxPDFWriterHelper = nil);

    function ApplyMask(const AMaskScanlineSource: IdxPDFImageScanlineSource;
      const AParameters: TdxPDFImageParameters; const AMatte: TDoubleDynArray)
      : TdxPDFImageData;
    function GetActualData(const AParameters: TdxPDFImageParameters;
      AInvertRGB: Boolean): TdxPDFImageData;
    function GetActualSize(const AParameters: TdxPDFImageParameters)
      : TdxPDFImageParameters;
    function GetAsBitmap: Graphics.TBitmap;
    function GetTransformedData(const AParameters: TdxPDFImageParameters)
      : TdxPDFScanlineTransformationResult;
    function IsMask: Boolean;
    procedure AddListener(AListener: IdxPDFDocumentSharedObjectListener);
    procedure RemoveListener(AListener: IdxPDFDocumentSharedObjectListener);
    procedure ReadColorKeyMask(AArray: TdxPDFArray);

    property BitsPerComponent: Integer read GetBitsPerComponent;
    property ColorKeyMask: TdxPDFRanges read GetColorKeyMask;
    property ColorSpace: TdxPDFCustomColorSpace read FColorSpace
      write SetColorSpace;
    property DecodeRanges: TdxPDFRanges read FDecodeRanges;
    property Filters: TdxPDFStreamFilters read GetFilters;
    property GUID: string read FGUID;
    property HasMask: Boolean read FHasMask;
    property Intent: TdxPDFRenderingIntent read FIntent;
    property Mask: TdxPDFDocumentImage read FMask write SetMask;
    property Matte: TDoubleDynArray read FMatte write FMatte;
    property SoftMask: TdxPDFDocumentImage read FSoftMask write SetSoftMask;
  public
    class function GetTypeName: string; override;
    constructor Create(const AData: TBytes; AColorSpace: TdxPDFCustomColorSpace;
      AFilters: TdxPDFStreamFilters;
      AWidth, AHeight, ABitsPerComponent: Integer; AHasMask: Boolean;
      ADictionary: TdxPDFDictionary); reintroduce; overload;
    property Height: Integer read FHeight;
    property Width: Integer read GetWidth;
  end;

  { TdxPDFDocumentImageData }

  TdxPDFDocumentImageData = class(TdxPDFReferencedObject) // for internal use
  strict private
    FBitmap: GpBitmap;
    FData: TBytes;
    FHeight: Integer;
    FPalette: TdxPDFARGBColorArray;
    FPixelFormat: TdxPDFPixelFormat;
    FStride: Integer;
    FWidth: Integer;

    procedure AddPaletteColor(const AColor: TdxPDFARGBColor;
      var APalette: TdxPDFARGBColorArray);
    procedure CopyPalette(const ASource: TdxPDFARGBColorArray;
      var ADestination: TdxPDFARGBColorArray);
  public
    destructor Destroy; override;

    function Clone: TdxPDFDocumentImageData;
    procedure Assign(AImageData: TdxPDFDocumentImageData);
    procedure CalculateParameters(out AComponentCount, ASourceStride,
      AActualWidth: Integer);
    procedure CalculateStride(AWidth, AComponentCount: Integer);
    procedure PopulateData(const ASourceData: TBytes;
      AWidth, ASourceStride, AComponentCount: Integer);

    property Bitmap: GpBitmap read FBitmap write FBitmap;
    property Data: TBytes read FData write FData;
    property Height: Integer read FHeight write FHeight;
    property Palette: TdxPDFARGBColorArray read FPalette;
    property PixelFormat: TdxPDFPixelFormat read FPixelFormat
      write FPixelFormat;
    property Stride: Integer read FStride write FStride;
    property Width: Integer read FWidth write FWidth;
  end;

  { TdxPDFDocumentImageDataStorage }

  TdxPDFDocumentImageDataStorage = class(TcxIUnknownObject,
    IdxPDFDocumentSharedObjectListener) // for internal use
  strict private
    FCache: TdxPDFImageDataCache;
    FImageList: TList<TdxPDFDocumentImage>;
    FReferences: TdxPDFUniqueReferences;
    // IdxPDFDocumentSharedObjectListener
    procedure IdxPDFDocumentSharedObjectListener.DestroyHandler =
      ImageDestroyHandler;
    procedure ImageDestroyHandler(Sender: TdxPDFBase);

    procedure AddReference(AImage: TdxPDFDocumentImage);
    procedure RemoveListener(AImage: TdxPDFDocumentImage);
  public
    constructor Create(ALimit: Int64);
    destructor Destroy; override;

    function GetImage(AImage: TdxPDFDocumentImage;
      const AImageParameters: TdxPDFImageParameters): TdxPDFImageCacheItem;
    function TryGetReference(ANumber: Integer;
      out AImage: TdxPDFDocumentImage): Boolean;
    procedure Add(AImage: TdxPDFDocumentImage);
    procedure Clear;
  end;

  { TdxFontFamilyInfo }

  TdxFontFamilyInfo = class
  strict private
    FAdditionalStyles: TdxPDFStringStringDictionary;
    FSystemFontName: string;
  public
    constructor Create; overload;
    constructor Create(const ASystemFontName: string); overload;
    destructor Destroy; override;

    property AdditionalStyles: TdxPDFStringStringDictionary
      read FAdditionalStyles;
    property SystemFontName: string read FSystemFontName;
  end;

  { TdxFontFamilyInfos }

  TdxFontFamilyInfos = class
  strict private
    FAdditionalStylePattern: TStringDynArray;
    FFamilies: TStringList;
    FInfos: TObjectDictionary<string, TdxFontFamilyInfo>;
    FInstalledFontCollection: TdxGPInstalledFontCollection;
    FSegoeUIPresent: Boolean;
    FStylePattern: TStringDynArray;

    function GetFamilies: TStringList;
    function GetFontStyle(const AFontName: string): string;
    function Normalize(const AName: string): string;
    function MatchPattern(const S: string; const APattern: string)
      : string; overload;
    function MatchPattern(const S: string; const APatternArray: TStringDynArray)
      : string; overload;
    function RemovePattern(const S: string;
      const APatternArray: TStringDynArray): string;
    procedure AddFamilyIfNotExists(const AKey, AValue: string);
    procedure PopulateInfos;
  protected
    property Families: TStringList read GetFamilies;
  public
    constructor Create;
    destructor Destroy; override;

    function ContainsBoldStyle(const AFontStyle: string): Boolean;
    function ContainsItalicStyle(const AFontStyle: string): Boolean;
    function ExtractAdditionalStyles(const AActualStyle: string)
      : TStringDynArray;
    function GetFontFamily(const AFontName: string): string;
    function GetNormalizedFontFamily(const AFontName: string): string;
    function GetNormalizedFontStyle(const AFontName: string): string;
    function TryGetValue(const AFamilyName: string;
      out AInfo: TdxFontFamilyInfo): Boolean;

    function Contains(const AFamily: string): Boolean;
  end;

  { TdxPDFGDIFontSubstitutionHelper }

  TdxPDFGDIFontSubstitutionHelper = class // for internal use
  public const
    BoldWeight = 700;
    CourierNewFontName = 'Courier New';
    NormalWeight = 400;
    TimesNewRomanFontName = 'Times New Roman';
  strict private
    FFontFamilyInfos: TdxFontFamilyInfos;
    function GetFontStyle(AFont: TdxPDFCustomFont; out AFamily: string): string;
    function GetFontWeight(AFont: TdxPDFCustomFont): Integer;
  protected
    property FontFamilyInfos: TdxFontFamilyInfos read FFontFamilyInfos;
  public
    constructor Create;
    destructor Destroy; override;
    function GetSubstituteFontParameters(AFont: TdxPDFCustomFont)
      : TdxPDFFontRegistratorParameters; overload;
    function GetSubstituteFontParameters(AFont: TdxPDFCustomFont;
      AFontFamilyFilter: TFunc<string, Boolean>)
      : TdxPDFFontRegistratorParameters; overload;
  end;

  { TdxPDFFontDataStorage }

  TdxPDFFontDataStorage = class(TcxIUnknownObject,
    IdxPDFDocumentSharedObjectListener) // for internal use
  strict private
    FFontCache: TObject;
    FDictionary: TDictionary<TdxPDFCustomFont, TdxPDFFontRegistrationData>;
    FFolderName: string;
    FFontSubstitutionHelper: TdxPDFGDIFontSubstitutionHelper;
    FLastRegisteredFont: TdxPDFCustomFont;
    FLastRegisteredFontData: TdxPDFFontRegistrationData;
    FLock: TRTLCriticalSection;
    FQueue: TList<TdxPDFCustomFont>;
    FReferences: TdxPDFUniqueReferences;

    function GetFontFamilyInfos: TdxFontFamilyInfos;
    procedure InternalAdd(AFont: TdxPDFCustomFont);
    procedure RemoveListener(AFont: TdxPDFCustomFont);
    // IdxPDFDocumentSharedObjectListener
    procedure IdxPDFDocumentSharedObjectListener.DestroyHandler =
      FontDestroyHandler;
    procedure FontDestroyHandler(Sender: TdxPDFBase);
  protected
    function CreateFontRegistrator(AFont: TdxPDFCustomFont): TObject;
    property FontSubstitutionHelper: TdxPDFGDIFontSubstitutionHelper
      read FFontSubstitutionHelper;
  public
    constructor Create(const ATempFolder: string);
    destructor Destroy; override;

    function Add(AFont: TdxPDFCustomFont): TdxPDFFontRegistrationData;
    function CreateSubstituteFontData(AFont: TdxPDFCustomFont)
      : TdxPDFFontRegistrationData;
    function SearchFontData(const AFontFamilyName: string;
      AFontStyle: TdxGPFontStyle): TObject;
    function TryGetValue(ANumber: Integer; out AFont: TdxPDFCustomFont)
      : Boolean;
    procedure Clear;
    procedure Delete(AFont: TdxPDFCustomFont);

    property FontFamilyInfos: TdxFontFamilyInfos read GetFontFamilyInfos;
  end;

  { TdxPDFXObjects }

  TdxPDFXObjects = class(TdxPDFObjectList) // for internal use
  protected
    class function GetTypePrefix: string; override;
  public
    class function GetTypeName: string; override;
    function GetXObject(const AName: string): TdxPDFXObject;
  end;

  { TdxPDFColorSpaces }

  TdxPDFColorSpaces = class(TdxPDFObjectList) // for internal use
  protected
    class function GetTypePrefix: string; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
  public
    class function GetTypeName: string; override;
    function GetColorSpace(const AName: string): TdxPDFCustomColorSpace;
  end;

  { TdxPDFCustomShading }

  TdxPDFCustomShading = class(TdxPDFObject) // for internal use
  strict private
    FBackgroundColor: TdxPDFColor;
    FBoundingBox: TdxPDFRectangle;
    FColorSpace: TdxPDFCustomColorSpace;
    FFunctions: TdxPDFReferencedObjects;
    FUseAntiAliasing: Boolean;

    function CreateFunctions(ASourceObject: TdxPDFBase)
      : TdxPDFReferencedObjects;
    procedure ReadBackgroundColor(AArray: TdxPDFArray);
    procedure ReadColorSpace(ASourceObject: TdxPDFBase);
    procedure ReadFunctions(AObject: TdxPDFBase);
    procedure WriteFunctions(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary);
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;

    class function GetShadingType: Integer; virtual;
    function GetPainter: IdxPDFShadingPainter; virtual; abstract;
    function GetDomainDimension: Integer; virtual;
    function IsFunctionRequired: Boolean; virtual;
  public
    class function GetTypeName: string; override;
    class function Parse(ARepository: TdxPDFCustomRepository;
      ASourceObject: TdxPDFBase): TdxPDFCustomShading;

    function TransformFunction(const AArguments: TDoubleDynArray): TdxPDFColor;

    property BackgroundColor: TdxPDFColor read FBackgroundColor;
    property BoundingBox: TdxPDFRectangle read FBoundingBox;
    property ColorSpace: TdxPDFCustomColorSpace read FColorSpace;
    property Functions: TdxPDFReferencedObjects read FFunctions;
    property UseAntiAliasing: Boolean read FUseAntiAliasing;
  end;

  { TdxPDFShadings }

  TdxPDFShadings = class(TdxPDFObjectList) // for internal use
  protected
    class function GetTypePrefix: string; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
  public
    class function GetTypeName: string; override;
    function GetShading(const AName: string): TdxPDFCustomShading;
  end;

  { TdxPDFCustomPattern }

  TdxPDFCustomPattern = class(TdxPDFObject) // for internal use
  strict private
    FMatrix: TdxPDFTransformationMatrix;
  protected
    class function GetPatternType: Integer; virtual;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
  public
    class function GetTypeName: string; override;
    class function Parse(ARepository: TdxPDFCustomRepository;
      ASourceObject: TdxPDFBase): TdxPDFCustomPattern;

    property Matrix: TdxPDFTransformationMatrix read FMatrix;
  end;

  { TdxPDFPatterns }

  TdxPDFPatterns = class(TdxPDFObjectList) // for internal use
  protected
    class function GetTypePrefix: string; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
  public
    class function GetTypeName: string; override;
    function GetPattern(const AName: string): TdxPDFCustomPattern;
  end;

  { TdxPDFShadingPattern }

  TdxPDFShadingPattern = class(TdxPDFCustomPattern) // for internal use
  strict private
    FGraphicsState: TdxPDFGraphicsStateParameters;
    FShading: TdxPDFCustomShading;

    procedure SetGraphicsStateParameters(const AValue
      : TdxPDFGraphicsStateParameters);
    procedure SetShading(const AValue: TdxPDFCustomShading);
  protected
    class function GetPatternType: Integer; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
  public
    property GraphicsState: TdxPDFGraphicsStateParameters read FGraphicsState
      write SetGraphicsStateParameters;
    property Shading: TdxPDFCustomShading read FShading write SetShading;
  end;

  { TdxPDFTilingPattern }

  TdxPDFTilingPattern = class(TdxPDFCustomPattern) // for internal use
  strict private
    FBoundingBox: TdxPDFRectangle;
    FColored: Boolean;
    FColoredPaintType: Integer;
    FCommands: TdxPDFCommandList;
    FResources: TdxPDFResources;
    FTilingType: TdxPDFTilingType;
    FUncoloredPaintType: Integer;
    FXStep: Double;
    FYStep: Double;

    procedure ReadCommands(ADictionary: TdxPDFReaderDictionary);
    procedure ReadStep(ADictionary: TdxPDFDictionary);
    procedure ReadTilingType(ADictionary: TdxPDFDictionary);
  protected
    class function GetPatternType: Integer; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
    procedure SetParent(const AValue: TObject); override;
  public
    function CreateMatrix(AWidth: Integer; AHeight: Integer)
      : TdxPDFTransformationMatrix;

    property BoundingBox: TdxPDFRectangle read FBoundingBox;
    property Colored: Boolean read FColored;
    property Commands: TdxPDFCommandList read FCommands;
    property Resources: TdxPDFResources read FResources write FResources;
    property TilingType: TdxPDFTilingType read FTilingType;
    property XStep: Double read FXStep;
    property YStep: Double read FYStep;
  end;

  { TdxPDFResources }

  TdxPDFResources = class(TdxPDFObject) // for internal use
  strict private
  type
    TListInitProc = reference to procedure(AList: TdxPDFObjectList);
  strict private
    FColorSpaces: TdxPDFColorSpaces;
    FDictionary: TdxPDFReaderDictionary;
    FFonts: TdxPDFFonts;
    FGraphicStatesParametersList: TdxPDFGraphicsStateParametersList;
    FLock: TRTLCriticalSection;
    FID: string;
    FPatterns: TdxPDFPatterns;
    FShadings: TdxPDFShadings;
    FXObjects: TdxPDFXObjects;

    function GetColorSpaces: TdxPDFColorSpaces;
    function GetFonts: TdxPDFFonts;
    procedure GetList(var AVariable; AClass: TdxPDFObjectListClass;
      const AKey: string; AInitProc: TListInitProc = nil);
    function GetGraphicStatesParametersList: TdxPDFGraphicsStateParametersList;
    function GetPatterns: TdxPDFPatterns;
    function GetShadings: TdxPDFShadings;
    function GetXObjects: TdxPDFXObjects;
    procedure InitXObjects(AList: TdxPDFObjectList);
    procedure SetColorSpaces(const AValue: TdxPDFColorSpaces);
    procedure SetDictionary(const AValue: TdxPDFReaderDictionary);
    procedure SetFonts(const AValue: TdxPDFFonts);
    procedure SetGraphicStatesParametersList(const AValue
      : TdxPDFGraphicsStateParametersList);
    procedure SetPatterns(const AValue: TdxPDFPatterns);
    procedure SetShadings(const AValue: TdxPDFShadings);
    procedure SetXObjects(const AValue: TdxPDFXObjects);
    procedure Clear;
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Initialize; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;

    function GetOwnerResources: TdxPDFResources; virtual;

    function InternalGetColorSpace(const AName: string): TdxPDFCustomColorSpace;
    function InternalGetFont(const AName: string): TdxPDFCustomFont;
    function InternalGetGraphicsStateParameters(const AName: string)
      : TdxPDFGraphicsStateParameters;
    function InternalGetPattern(const AName: string): TdxPDFCustomPattern;
    function InternalGetShading(const AName: string): TdxPDFCustomShading;
    function InternalGetXObject(const AName: string): TdxPDFXObject;
    function TryGetColorSpaceName(AObject: TdxPDFCustomColorSpace;
      out AName: string): Boolean;
    function TryGetResourceName(AResources: TdxPDFObjectList;
      AObject: TdxPDFCustomColorSpace; out AName: string): Boolean;
    function AddGraphicsStateParameters(AParameters
      : TdxPDFGraphicsStateParameters): string;
    function AddPattern(APattern: TdxPDFCustomPattern): string;
    function AddXObject(ANumber: Integer): string;

    property ColorSpaces: TdxPDFColorSpaces read GetColorSpaces
      write SetColorSpaces;
    property Dictionary: TdxPDFReaderDictionary read FDictionary
      write SetDictionary;
    property Fonts: TdxPDFFonts read GetFonts write SetFonts;
    property GraphicStatesParametersList: TdxPDFGraphicsStateParametersList
      read GetGraphicStatesParametersList write SetGraphicStatesParametersList;
    property ID: string read FID;
    property Patterns: TdxPDFPatterns read GetPatterns write SetPatterns;
    property Shadings: TdxPDFShadings read GetShadings write SetShadings;
    property XObjects: TdxPDFXObjects read GetXObjects write SetXObjects;
  public
    class function GetTypeName: string; override;
    function AddFont(AFont: TdxPDFCustomFont): string;
    function GetColorSpace(const AName: string)
      : TdxPDFCustomColorSpace; virtual;
    function GetFont(const AName: string): TdxPDFCustomFont; virtual;
    function GetGraphicsStateParameters(const AName: string)
      : TdxPDFGraphicsStateParameters; virtual;
    function GetPattern(const AName: string): TdxPDFCustomPattern; virtual;
    function GetShading(const AName: string): TdxPDFCustomShading; virtual;
    function GetXObject(const AName: string): TdxPDFXObject; virtual;
    function GetProperties(const AName: string)
      : TdxPDFCustomProperties; virtual;
    procedure Append(AResources: TdxPDFResources);
    procedure Pack;
  end;

  { TdxPDFPageContents }

  TdxPDFPageContents = class(TdxPDFStreamObject) // for internal use
  strict private
    FCommands: TdxPDFCommandList;
    FContentList: TdxPDFReferencedObjects;

    function GetCommandCount: Integer;
    function GetResources: TdxPDFResources;
    procedure ReadItem(AStream: TdxPDFStream);
    procedure ReadContentList(ADictionary: TdxPDFReaderDictionary;
      AContentObject: TdxPDFBase);
  protected
    function GetData: TBytes; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
    procedure WriteData(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;

    procedure ClearCommands;
    procedure PopulateCommands(AResources: TdxPDFResources);

    property CommandCount: Integer read GetCommandCount;
    property Commands: TdxPDFCommandList read FCommands;
    property ContentList: TdxPDFReferencedObjects read FContentList;
    property Resources: TdxPDFResources read GetResources;
  public
    class function GetTypeName: string; override;
  end;

  { TdxPDFPageData }

  TdxPDFPageData = class(TdxPDFObject) // for internal use
  strict private
    FAnnotations: TdxPDFReferencedObjects;
    FAnnotationsLoaded: Boolean;
    FContents: TdxPDFPageContents;
    FDictionary: TdxPDFReaderDictionary;
    FTransparencyGroup: TdxPDFTransparencyGroup;

    function GetAnnotations: TdxPDFReferencedObjects;
    function GetCommands: TdxPDFCommandList;
    function GetPage: TdxPDFPage;
    function GetResources: TdxPDFResources;
    procedure SetContents(const AValue: TdxPDFPageContents);
    procedure ReadAnnotations;
    procedure ReadGroup(ADictionary: TdxPDFReaderDictionary);
    procedure WriteAnnotations(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary);
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Initialize; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;

    property Contents: TdxPDFPageContents read FContents write SetContents;
    property Resources: TdxPDFResources read GetResources;
    property TransparencyGroup: TdxPDFTransparencyGroup read FTransparencyGroup;
  public
    class function GetTypeName: string; override;

    procedure ClearCommands;
    procedure ExtractCommands;

    property Annotations: TdxPDFReferencedObjects read GetAnnotations;
    property AnnotationsLoaded: Boolean read FAnnotationsLoaded;
    property Commands: TdxPDFCommandList read GetCommands;
  end;

  { TdxPDFPageTreeObject }

  TdxPDFPageTreeObject = class(TdxPDFObject) // for internal use
  strict private
    FArtBox: TdxRectF;
    FBleedBox: TdxRectF;
    FTrimBox: TdxRectF;
    FUserUnit: Integer;
    FUseParentRotationAngle: Boolean;
    FResources: TdxPDFResources;
    function GetArtBox: TdxRectF;
    function GetBleedBox: TdxRectF;
    function GetCropBox: TdxRectF;
    function GetMediaBox: TdxRectF;
    function GetTrimBox: TdxRectF;
    function GetUserUnit: Integer;
    function GetParentNode: TdxPDFPageTreeObject;
    function GetParentNodeArtBox: TdxRectF;
    function GetParentNodeBleedBox: TdxRectF;
    function GetParentNodeMediaBox: TdxRectF;
    function GetParentNodeRotationAngle: Integer;
    function GetParentNodeTrimBox: TdxRectF;
    function GetParentNodeUserUnit: Integer;
    function GetResources: TdxPDFResources;
    function GetRotationAngle: Integer;
    function GetParentNodeResources: TdxPDFResources;
    procedure SetResources(const AValue: TdxPDFResources);
    procedure ReadResources(ADictionary: TdxPDFReaderDictionary);
  strict protected
    FCropBox: TdxRectF;
    FMediaBox: TdxRectF;
  protected
    FRotationAngle: Integer;

    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;

    function GetLeafCount: Integer; virtual;
    procedure Clear; virtual;

    property ArtBox: TdxRectF read GetArtBox;
    property BleedBox: TdxRectF read GetBleedBox;
    property CropBox: TdxRectF read GetCropBox;
    property MediaBox: TdxRectF read GetMediaBox;
    property ParentNode: TdxPDFPageTreeObject read GetParentNode;
    property TrimBox: TdxRectF read GetTrimBox;
    property UseParentRotationAngle: Boolean read FUseParentRotationAngle;

    property ParentNodeResources: TdxPDFResources read GetParentNodeResources;
    property Resources: TdxPDFResources read GetResources write SetResources;
  public
    property RotationAngle: Integer read GetRotationAngle;
    property UserUnit: Integer read GetUserUnit;
  end;

  { TdxPDFPage }

  TdxPDFPage = class(TdxPDFPageTreeObject) // for internal use
  strict private
    FDeferredData: TdxPDFDeferredObject;
    FDisplayDuration: Double;
    FLastModified: TDateTime;
    FLock: TRTLCriticalSection;
    FID: TBytes;
    FPreferredZoomFactor: Integer;
    FThumbnailImage: TdxPDFDocumentImage;
    FRecognizedContent: TdxPDFRecognizedContent;
    FRecognizedContentLockCount: Integer;
    FStructParents: Integer;
    FOnPack: TNotifyEvent;

    function GetBounds: TdxRectF;
    function GetData: TdxPDFPageData;
    function GetDocumentState: TdxPDFDocumentState;
    function GetNormalizedRotationAngle: Integer;
    function GetRecognizedContent: TdxPDFRecognizedContent; overload;
    function GetRecognizedContent(ARecognitionObjects: TdxPDFRecognitionObjects)
      : TdxPDFRecognizedContent; overload;
    function GetSize: TdxPointF;
    procedure SetData(const AValue: TdxPDFPageData);
    procedure SetThumbnailImage(const AValue: TdxPDFDocumentImage);

    function GetTextExpansionFactor(const AScaleFactor: TdxPointF): TdxPointF;
    function TryGetAcroForm(AField: TdxPDFAcroFormField;
      const APosition: TdxPointF; out AResult: TdxPDFAcroFormField): Boolean;
    procedure PackData;
    procedure RecognizeContent(AContent: TdxPDFRecognizedContent;
      ARecognitionObjects: TdxPDFRecognitionObjects);
  protected
    Locked: Boolean;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Initialize; override;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;

    function IsRecognizedContentLocked: Boolean;
    procedure LockRecognizedContent;
    procedure UnLockRecognizedContent(AReset: Boolean = False);

    function ScaleFactor(const ADPI, AScaleFactor: TdxPointF): TdxPointF;
    function UserSpaceFactor(const ADPI: TdxPointF): TdxPointF;
    procedure AddAnnotation(AAnnotation: TdxPDFCustomAnnotation);
    procedure Export(ADevice: TObject;
      AParameters: TdxPDFExportParameters); overload;
    procedure Export(AParameters: TdxPDFExportParameters;
      AStream: TStream); overload;
    procedure LockAndExecute(AProc: TProc; ATryLock: Boolean = False);
    procedure PopulateAttachmentList(AList: TdxPDFFileAttachmentList);

    property Data: TdxPDFPageData read GetData write SetData;
    property DocumentState: TdxPDFDocumentState read GetDocumentState;
    property NormalizedRotationAngle: Integer read GetNormalizedRotationAngle;
    property ThumbnailImage: TdxPDFDocumentImage read FThumbnailImage
      write SetThumbnailImage;
  public
    class function GetTypeName: string; override;
    constructor Create(AOwner: TdxPDFObject; AInfo: TdxPDFDeferredObjectInfo);
      reintroduce; overload;
    constructor Create(APages: TdxPDFPages; AMediaBox, ACropBox: TdxRectF;
      ARotationAngle: Integer); reintroduce; overload;
    destructor Destroy; override;

    function CalculateRotationAngle(ARotationAngle: TcxRotationAngle): Integer;

    function Find(const APosition: TdxPDFPosition;
      const AScaleFactor: TdxPointF;
      ARecognitionObjects: TdxPDFRecognitionObjects =
      dxPDFAllRecognitionObjects): TdxPDFRecognizedObject;
    function FindHyperlink(const P: TdxPointF; const AScaleFactor: TdxPointF;
      out AHyperlink: TdxPDFHyperlink;
      ARecognitionObjects: TdxPDFRecognitionObjects =
      dxPDFAllRecognitionObjects): Boolean;
    function FindImage(const P: TdxPointF;
      ARecognitionObjects: TdxPDFRecognitionObjects =
      dxPDFAllRecognitionObjects): TdxPDFImage;
    function FindInteractiveObject(const P: TdxPointF;
      const AScaleFactor: TdxPointF; out AField: TdxPDFAcroFormField;
      ARecognitionObjects: TdxPDFRecognitionObjects =
      dxPDFAllRecognitionObjects): Boolean;
    function FindLine(const APosition: TdxPDFPosition;
      const AScaleFactor: TdxPointF; out ALine: TdxPDFTextLine;
      ARecognitionObjects: TdxPDFRecognitionObjects =
      dxPDFAllRecognitionObjects): Boolean;
    function FindStartTextPosition(const APosition: TdxPDFPosition;
      const AScaleFactor: TdxPointF;
      ARecognitionObjects: TdxPDFRecognitionObjects =
      dxPDFAllRecognitionObjects): TdxPDFTextPosition;

    function GetTopLeft(AAngle: TcxRotationAngle): TdxPointF;
    function FromUserSpace(const P, ADPI, AScaleFactor: TdxPointF;
      const ABounds: TdxRectF; AAngle: TcxRotationAngle): TdxPointF; overload;
    function FromUserSpace(const R: TdxRectF; ADPI, AScaleFactor: TdxPointF;
      const ABounds: TdxRectF; AAngle: TcxRotationAngle): TdxRectF; overload;
    function ToUserSpace(const P, ADPI, AScaleFactor: TdxPointF;
      const ABounds: TdxRectF; AAngle: TcxRotationAngle): TdxPointF; overload;
    function ToUserSpace(const R: TdxRectF; const ADPI, AScaleFactor: TdxPointF;
      const ABounds: TdxRectF; AAngle: TcxRotationAngle): TdxRectF; overload;
    procedure Pack;
    procedure PackRecognizedContent(AForce: Boolean = False);

    property Bounds: TdxRectF read GetBounds;
    property RecognizedContent: TdxPDFRecognizedContent
      read GetRecognizedContent;
    property Size: TdxPointF read GetSize;
    property OnPack: TNotifyEvent read FOnPack write FOnPack;
  end;

  { TdxPDFPageTreeObjectList }

  TdxPDFPageTreeOnCreatePageNodeEvent = function
    (AOwner: TdxPDFPageTreeObjectList; ADictionary: TdxPDFReaderDictionary)
    : TdxPDFPageTreeObject of object; // for internal use

  TdxPDFPageTreeObjectList = class(TdxPDFPageTreeObject) // for internal use
  strict private
    FChildren: TObjectList<TdxPDFPageTreeObject>;
    FOnCreatePageNode: TdxPDFPageTreeOnCreatePageNodeEvent;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TdxPDFPageTreeObject;
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;

    procedure Add(AChild: TdxPDFPageTreeObject);

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxPDFPageTreeObject read GetItem; default;
    property OnCreatePageNode: TdxPDFPageTreeOnCreatePageNodeEvent
      read FOnCreatePageNode write FOnCreatePageNode;
  public
    class function GetTypeName: string; override;
  end;

  { TdxPDFPageTreeNode }

  TdxPDFPageTreeNode = class(TdxPDFPageTreeObject) // for internal use
  strict private
    FNodeList: TdxPDFPageTreeObjectList;
    FOnCreatePageNode: TdxPDFPageTreeOnCreatePageNodeEvent;
    function OnCreatePageNodeHandler(AOwner: TdxPDFPageTreeObjectList;
      ADictionary: TdxPDFReaderDictionary): TdxPDFPageTreeObject;
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;

    function GetLeafCount: Integer; override;
    procedure AddNode(AChild: TdxPDFPageTreeObject);

    property NodeList: TdxPDFPageTreeObjectList read FNodeList;
    property OnCreatePageNode: TdxPDFPageTreeOnCreatePageNodeEvent
      read FOnCreatePageNode write FOnCreatePageNode;
  end;

  { TdxPDFPages }

  TdxPDFPages = class(TdxPDFPageTreeNode) // for internal use
  strict private
    FDictionary: TDictionary<Integer, TdxPDFPage>;
    FPageList: TList<TdxPDFPage>;
    FOnPagePack: TNotifyEvent;

    function GetCount: Integer;
    function GetPage(AIndex: Integer): TdxPDFPage;
    function OnCreatePageNodeHandler(AOwner: TdxPDFPageTreeObjectList;
      ADictionary: TdxPDFReaderDictionary): TdxPDFPageTreeObject;
  protected
    procedure Clear; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;

    function FindPage(ANumber: Integer): TdxPDFPage;
    function IndexOf(APage: TdxPDFPage): Integer;
    procedure AddPage(APage: TdxPDFPage);

    property OnPagePack: TNotifyEvent read FOnPagePack write FOnPagePack;
  public
    class function GetTypeName: string; override;

    property Count: Integer read GetCount;
    property Page[AIndex: Integer]: TdxPDFPage read GetPage; default;
  end;

  { TdxPDFCustomDestination }

  TdxPDFCustomDestinationClass = class of TdxPDFCustomDestination;

  TdxPDFCustomDestination = class(TdxPDFObject) // for internal use
  strict private
    FCatalog: TdxPDFCatalog;
    FPage: TdxPDFPage;
    FPageID: Integer;
    FPageIndex: Integer;
    FPageObject: TdxPDFBase;

    function GetPage: TdxPDFPage;
    function GetPageIndex: Integer;
    function GetPages: TdxPDFPages;
    procedure SetPageObject(const AValue: TdxPDFBase);
    procedure ResolvePage;
  protected
    procedure DestroySubClasses; override;
    procedure Initialize; override;
    procedure Read(ACatalog: TdxPDFCatalog; AArray: TdxPDFArray); reintroduce;
    function Write(AHelper: TdxPDFWriterHelper): TdxPDFBase; override;

    function GetTarget: TdxPDFTarget; virtual; abstract;
    procedure ReadParameters(AArray: TdxPDFArray); virtual;
    procedure WriteParameters(AHelper: TdxPDFWriterHelper;
      AArray: TdxPDFWriterArray); virtual;
    procedure WriteSingleValue(AArray: TdxPDFWriterArray; const AValue: Single);

    function IsSame(ADestination: TdxPDFCustomDestination): Boolean;
    procedure ResolveInternalPage;

    class function GetSingleValue(AArray: TdxPDFArray): Single; static;
    function CalculatePageIndex(APages: TdxPDFPages): Integer;
    function ValidateVerticalCoordinate(ATop: Single): Single;

    property Catalog: TdxPDFCatalog read FCatalog;
    property Page: TdxPDFPage read GetPage;
    property PageID: Integer read FPageID;
    property PageIndex: Integer read GetPageIndex;
    property PageObject: TdxPDFBase read FPageObject write SetPageObject;
    property Pages: TdxPDFPages read GetPages;
  public
    class function GetTypeName: string; override;
    class function Parse(ACatalog: TdxPDFCatalog; AObject: TdxPDFBase)
      : TdxPDFCustomDestination; static;
  end;

  { TdxPDFFileSpecificationData }

  TdxPDFFileSpecificationData = class(TdxPDFObject) // for internal use
  strict private
    FCreationDate: TDateTime;
    FData: TBytes;
    FDataStreamRef: TdxPDFStream;
    FMimeType: string;
    FModificationDate: TDateTime;
    FSize: Integer;

    function GetData: TBytes;
    function GetHasModificationDate: Boolean;
    procedure SetData(const AValue: TBytes);
    procedure ResolveData;
    procedure ReadParams(ADictionary: TdxPDFReaderDictionary);
    procedure WriteParams(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary);
  protected
    procedure CreateSubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
  public
    class function GetTypeName: string; override;

    property CreationDate: TDateTime read FCreationDate write FCreationDate;
    property Data: TBytes read GetData write SetData;
    property HasModificationDate: Boolean read GetHasModificationDate;
    property MimeType: string read FMimeType write FMimeType;
    property ModificationDate: TDateTime read FModificationDate
      write FModificationDate;
    property Size: Integer read FSize;
  end;

  { TdxPDFFileSpecification }

  TdxPDFFileSpecification = class(TdxPDFObject) // for internal use
  strict private
  const
    AssociatedFileRelationshipNameMap: array [TdxPDFAssociatedFileRelationship]
      of string = ('', 'Data', 'Alternative', 'Supplement', 'EncryptedPayload',
      'Unspecified');
  strict private
    FAttachment: TdxPDFFileAttachment;
    FDescription: string;
    FFileName: string;
    FFileSpecificationData: TdxPDFFileSpecificationData;
    FFileSystem: string;
    FIndex: Integer;
    FRelationship: TdxPDFAssociatedFileRelationship;

    function GetAttachment: TdxPDFFileAttachment;
    function GetCreationDate: TDateTime;
    function GetFileData: TBytes;
    function GetMimeType: string;
    function GetModificationDate: TDateTime;
    function GetHasModificationDate: Boolean;
    function GetSize: Integer;
    procedure SetAttachment(const AValue: TdxPDFFileAttachment);
    procedure SetCreationDate(const AValue: TDateTime);
    procedure SetMimeType(const AValue: string);
    procedure SetModificationDate(const AValue: TDateTime);
    procedure SetFileData(const AValue: TBytes);
    //
    procedure ReadAssociatedFileRelationship(ADictionary
      : TdxPDFReaderDictionary);
    procedure ReadFileIndex(ADictionary: TdxPDFReaderDictionary);
    procedure ReadFileName(ADictionary: TdxPDFReaderDictionary);
    procedure ReadFileSpecificationData(ADictionary: TdxPDFReaderDictionary);
    procedure WriteFileIndex(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary);
    function WriteFileSpecificationData(AHelper: TdxPDFWriterHelper)
      : TdxPDFBase;
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
  public
    class function GetTypeName: string; override;
    class function Parse(ADictionary: TdxPDFReaderDictionary)
      : TdxPDFFileSpecification; static;
    constructor Create(const AFileName: string); reintroduce;

    property Attachment: TdxPDFFileAttachment read GetAttachment
      write SetAttachment;
    property CreationDate: TDateTime read GetCreationDate write SetCreationDate;
    property Description: string read FDescription write FDescription;
    property FileData: TBytes read GetFileData write SetFileData;
    property FileName: string read FFileName write FFileName;
    property FileSystem: string read FFileSystem;
    property HasModificationDate: Boolean read GetHasModificationDate;
    property Index: Integer read FIndex;
    property MimeType: string read GetMimeType write SetMimeType;
    property ModificationDate: TDateTime read GetModificationDate
      write SetModificationDate;
    property Relationship: TdxPDFAssociatedFileRelationship read FRelationship
      write FRelationship;
    property Size: Integer read GetSize;
  end;

  { TdxPDFCustomAction }

  TdxPDFActionList = class(TList<TdxPDFCustomAction>); // for internal use

  TdxPDFCustomAction = class(TdxPDFObject) // for internal use
  strict private
    FCatalog: TdxPDFCatalog;
    FNext: TdxPDFActionList;
    FNextValue: TdxPDFBase;
    function GetNext: TdxPDFActionList;
    procedure SetNextValue(const AValue: TdxPDFBase);
    procedure EnsureNextActions;
    procedure WriteNextActions(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary);
  protected
    procedure DestroySubClasses; override;
    procedure Execute(const AController
      : IdxPDFInteractivityController); virtual;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;

    property Catalog: TdxPDFCatalog read FCatalog;
    property Next: TdxPDFActionList read GetNext;
    property NextValue: TdxPDFBase read FNextValue write SetNextValue;
  public
    class function Parse(ADictionary: TdxPDFReaderDictionary)
      : TdxPDFCustomAction; static;
  end;

  { TdxPDFCustomTree }

  TdxPDFCustomTree = class // for internal use
  strict private
    FDictionary: TdxPDFReferencedObjectDictionary;

    function CreateDeferredObject(ARepository: TdxPDFDocumentRepository;
      AValue: TdxPDFBase): TdxPDFDeferredObject;
    function CreateKey(AValue: TdxPDFBase): string;
    function GetCount: Integer;
    function GetItems: TdxPDFStringReferencedObjectDictionary;
    procedure ReadBranch(ARepository: TdxPDFDocumentRepository;
      AReferences: TdxPDFArray);
    procedure ReadKids(ARepository: TdxPDFDocumentRepository;
      AReferences: TdxPDFArray);
    procedure ReadNode(ARepository: TdxPDFDocumentRepository;
      APageObject: TdxPDFBase);
    function WriteBranch(AHelper: TdxPDFWriterHelper): TdxPDFBase;
  strict protected
    function GetDeferredObjectKey: string; virtual; abstract;
    function GetNodeName: string; virtual;
    function InternalGetValue(const AKey: string): TdxPDFObject;
  protected
    procedure Read(ADictionary: TdxPDFReaderDictionary);
    function Write(AHelper: TdxPDFWriterHelper): TdxPDFBase;

    property Items: TdxPDFStringReferencedObjectDictionary read GetItems;
  public
    constructor Create;
    destructor Destroy; override;

    property Count: Integer read GetCount;
  end;

  { TdxPDFDestinationTree }

  TdxPDFDestinationTree = class(TdxPDFCustomTree) // for internal use
  strict protected
    function GetDeferredObjectKey: string; override;
  public
    function GetValue(const AKey: string): TdxPDFCustomDestination;
  end;

  { TdxPDFEmbeddedFileSpecificationTree }

  TdxPDFEmbeddedFileSpecificationTree = class(TdxPDFCustomTree)
  // for internal use
  strict protected
    function GetDeferredObjectKey: string; override;
  public
    function GetValue(const AKey: string): TdxPDFFileSpecification;
  end;

  { TdxPDFAnnotationAppearance }

  TdxPDFAnnotationAppearance = class(TdxPDFObject) // for internal use
  strict private
    FDefaultForm: TdxPDFForm;
    FForms: TDictionary<string, TdxPDFForm>;
    procedure SetDefaultForm(const AValue: TdxPDFForm);
  protected
    function Write(AHelper: TdxPDFWriterHelper): TdxPDFBase; override;
    procedure DestroySubClasses; override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;

    procedure SetForm(const AName: string; AForm: TdxPDFForm);

    property DefaultForm: TdxPDFForm read FDefaultForm write SetDefaultForm;
    property Forms: TDictionary<string, TdxPDFForm> read FForms;
  public
    class function Parse(ADictionary: TdxPDFReaderDictionary;
      const AKey: string): TdxPDFAnnotationAppearance; static;
    constructor Create(ACatalog: TdxPDFCatalog; const ABBox: TdxPDFRectangle);
      reintroduce; overload;
    constructor Create(ADefaultForm: TdxPDFForm;
      AForms: TDictionary<string, TdxPDFForm>); reintroduce; overload;
  end;

  { TdxPDFAnnotationAppearances }

  TdxPDFAnnotationAppearances = class(TdxPDFObject) // for internal use
  strict private
    FForm: TdxPDFForm;
    FDown: TdxPDFAnnotationAppearance;
    FNormal: TdxPDFAnnotationAppearance;
    FRollover: TdxPDFAnnotationAppearance;
    procedure SetForm(const AValue: TdxPDFForm);
  protected
    function Write(AHelper: TdxPDFWriterHelper): TdxPDFBase; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary;
      const AParentBox: TdxPDFRectangle); reintroduce; overload;
    procedure Read(AForm: TdxPDFForm); reintroduce; overload;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;

    procedure SetAnnotationForm(const AName: string; AForm: TdxPDFForm);

    property Normal: TdxPDFAnnotationAppearance read FNormal;
    property Form: TdxPDFForm read FForm write SetForm;
  end;

  { TdxPDFAnnotationBorderStyle }

  TdxPDFAnnotationBorderStyle = class(TdxPDFObject) // for internal use
  strict private
    FLineStyle: TdxPDFLineStyle;
    FStyleName: string;
    FWidth: Single;
    procedure SetLineStyle(const AValue: TdxPDFLineStyle);
  protected
    function Write(AHelper: TdxPDFWriterHelper): TdxPDFBase; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
  public
    class function Parse(ADictionary: TdxPDFReaderDictionary)
      : TdxPDFAnnotationBorderStyle; static;
    class function ParseLineStyle(AArray: TdxPDFArray): TdxPDFLineStyle; static;

    property LineStyle: TdxPDFLineStyle read FLineStyle write SetLineStyle;
    property StyleName: string read FStyleName;
    property Width: Single read FWidth;
  end;

  { TdxPDFAnnotationBorder }

  TdxPDFAnnotationBorder = class(TdxPDFObject) // for internal use
  strict private
  const
    DefaultHorizontalCornerRadius = 0;
    DefaultVerticalCornerRadius = 0;
    DefaultLineWidth = 1;
  strict private
    FHorizontalCornerRadius: Double;
    FVerticalCornerRadius: Double;
    FLineWidth: Double;
    FLineStyle: TdxPDFLineStyle;
    function GetIsDefault: Boolean;
    procedure SetLineStyle(const AValue: TdxPDFLineStyle);
  protected
    function Write(AHelper: TdxPDFWriterHelper): TdxPDFBase; override;
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Initialize; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
  public
    property HorizontalCornerRadius: Double read FHorizontalCornerRadius;
    property IsDefault: Boolean read GetIsDefault;
    property LineStyle: TdxPDFLineStyle read FLineStyle write SetLineStyle;
    property LineWidth: Double read FLineWidth;
    property VerticalCornerRadius: Double read FVerticalCornerRadius;
  end;

  { TdxPDFCustomAnnotation }

  TdxPDFCustomAnnotation = class(TdxPDFObject) // for internal use
  strict private
    FAppearance: TdxPDFAnnotationAppearances;
    FAppearanceName: string;
    FBorder: TdxPDFAnnotationBorder;
    FCatalog: TdxPDFCatalog;
    FColor: TdxPDFColor;
    FContents: string;
    FDictionary: TdxPDFReaderDictionary;
    FFlags: TdxPDFAnnotationFlags;
    FModified: TDateTime;
    FName: string;
    FPage: TdxPDFPage;
    FRect: TdxPDFRectangle;
    FResolved: Boolean;
    FStructParent: Integer;

    function GetAppearanceName: string;
    function GetBorder: TdxPDFAnnotationBorder;
    function GetColor: TdxPDFColor;
    function GetContents: string;
    function GetName: string;
    function GetRect: TdxPDFRectangle;
    procedure SetAppearance(const AValue: TdxPDFAnnotationAppearances);
    procedure SetAppearanceName(const AValue: string);
    procedure SetBorder(const AValue: TdxPDFAnnotationBorder);
    procedure SetColor(const AValue: TdxPDFColor);
    procedure SetContents(const AValue: string);
    procedure SetDictionary(const AValue: TdxPDFReaderDictionary);
    procedure SetName(const AValue: string);

    function EnsureAppearance(AState: TdxPDFDocumentState; AForm: TdxPDFForm)
      : TdxPDFForm; overload;
    function GetActualAppearanceForm: TdxPDFForm;
  protected
    procedure DestroySubClasses; override;
    procedure Initialize; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary; APage: TdxPDFPage);
      reintroduce; virtual;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;

    function CreateAppearanceForm(const AName: string): TdxPDFForm; virtual;
    function CreateAppearanceBuilder(AState: TdxPDFDocumentState)
      : TObject; virtual;
    function CreateField(AState: TdxPDFDocumentState): TdxPDFAcroFormField;
      virtual; abstract;
    function GetAppearanceFormBoundingBox: TdxPDFRectangle; virtual;
    function GetUseDefaultForm: Boolean; virtual;
    function GetVisible: Boolean; virtual;
    function NeedCreateAppearance(AForm: TdxPDFForm): Boolean; virtual;
    procedure Ensure; virtual;
    procedure Resolve(ADictionary: TdxPDFReaderDictionary); virtual;

    function GetAppearanceForm(AState: TdxPDFDocumentState): TdxPDFForm;
    function HasFlag(AFlags: TdxPDFAnnotationFlags): Boolean;

    property Appearance: TdxPDFAnnotationAppearances read FAppearance
      write SetAppearance;
    property AppearanceName: string read GetAppearanceName
      write SetAppearanceName;
    property Catalog: TdxPDFCatalog read FCatalog;
    property Color: TdxPDFColor read GetColor write SetColor;
    property Contents: string read GetContents write SetContents;
    property Dictionary: TdxPDFReaderDictionary read FDictionary
      write SetDictionary;
    property Name: string read GetName write SetName;
    property Page: TdxPDFPage read FPage;
    property Rect: TdxPDFRectangle read GetRect;
    property UseDefaultForm: Boolean read GetUseDefaultForm;
    property Visible: Boolean read GetVisible;
  public
    class function GetTypeName: string; override;
    class function Parse(ADictionary: TdxPDFReaderDictionary)
      : TdxPDFCustomAnnotation; static;
    constructor Create(APage: TdxPDFPage; const ARect: TdxPDFRectangle;
      AFlags: TdxPDFAnnotationFlags); // for internal use
    procedure EnsureAppearance(AState: TdxPDFDocumentState); overload;

    property Border: TdxPDFAnnotationBorder read GetBorder write SetBorder;
    property Modified: TDateTime read FModified write FModified;
  end;

  { TdxPDFFileAttachment }

  TdxPDFFileAttachment = class
  strict private
    FFileSpecification: TdxPDFFileSpecification;
    function GetCreationDate: TDateTime;
    function GetData: TBytes;
    function GetDescription: string;
    function GetFileName: string;
    function GetMimeType: string;
    function GetModificationDate: TDateTime;
    function GetRelationship: TdxPDFAssociatedFileRelationship;
    function GetSize: Integer;
  strict protected
    property FileSpecification: TdxPDFFileSpecification read FFileSpecification;
  protected
    constructor Create(AFileSpecification: TdxPDFFileSpecification);

    function GetModificationDateAsString: string;
    function GetSizeAsString: string;

    property MimeType: string read GetMimeType;
    property Relationship: TdxPDFAssociatedFileRelationship
      read GetRelationship;
  public
    property CreationDate: TDateTime read GetCreationDate;
    property Data: TBytes read GetData;
    property Description: string read GetDescription;
    property FileName: string read GetFileName;
    property ModificationDate: TDateTime read GetModificationDate;
    property Size: Integer read GetSize;
  end;

  { TdxPDFFileAttachmentList }

  TdxPDFFileAttachmentList = class(TList<TdxPDFFileAttachment>)
  protected
    procedure Populate(ACatalog: TdxPDFCatalog);
  end;

  { TdxPDFDocumentNames }

  TdxPDFDocumentNames = class(TdxPDFObject) // for internal use
  strict private
    FEmbeddedFileSpecifications: TdxPDFEmbeddedFileSpecificationTree;
    FPageDestinations: TdxPDFDestinationTree;
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;

    function GetEmbeddedFileSpecification(const AName: string)
      : TdxPDFFileSpecification;
    function GetPageDestination(const AName: string): TdxPDFCustomDestination;
    procedure PopulateAttachmentList(AList: TdxPDFFileAttachmentList);
  public
    property EmbeddedFileSpecifications: TdxPDFEmbeddedFileSpecificationTree
      read FEmbeddedFileSpecifications;
    property PageDestinations: TdxPDFDestinationTree read FPageDestinations;
  end;

  { TdxPDFInteractiveFormFieldTextState }

  TdxPDFInteractiveFormFieldTextState = class(TdxPDFObject) // for internal use
  strict private
  const
    DefaultFontSize = 12;
  strict private
    FCharacterSpacing: Double;
    FCommandsAsBytes: TBytes;
    FFontCommand: TdxPDFCustomCommand;
    FHorizontalScaling: Double;
    FWordSpacing: Double;
    function GetAppearanceCommandsInheritable
      (AFormField: TdxPDFInteractiveFormField): TdxPDFCommandList;
    function GetFontSize: Double;
    function FindSetTextFontCommand(ACommands: TdxPDFReferencedObjects)
      : TdxPDFCustomCommand;
    procedure ConvertCommandsToBytes(AField: TdxPDFInteractiveFormField);
  protected
    procedure DestroySubClasses; override;
    procedure Initialize; override;
  public
    constructor Create(AField: TdxPDFInteractiveFormField); reintroduce;

    property CharacterSpacing: Double read FCharacterSpacing;
    property CommandsAsBytes: TBytes read FCommandsAsBytes;
    property FontCommand: TdxPDFCustomCommand read FFontCommand;
    property FontSize: Double read GetFontSize;
    property HorizontalScaling: Double read FHorizontalScaling;
    property WordSpacing: Double read FWordSpacing;
  end;

  { TdxPDFInteractiveFormField }

  TdxPDFInteractiveFormField = class(TdxPDFObject) // for internal use
  strict private
    FAlternateName: string;
    FAppearanceCommands: TdxPDFCommandList;
    FFlags: TdxPDFInteractiveFormFieldFlags;
    FForm: TdxPDFInteractiveForm;
    FFormCreated: Boolean;
    FKids: TdxPDFInteractiveFormFieldCollection;
    FKidsResolved: Boolean;
    FMappingName: string;
    FParent: TdxPDFInteractiveFormField;
    FResources: TdxPDFResources;
    FTextJustification: TdxPDFTextJustification;
    FTextState: TdxPDFInteractiveFormFieldTextState;
    FValuesProvider: TdxPDFInteractiveFormField;
    FWidget: TdxPDFCustomAnnotation { TdxPDFWidgetAnnotation };

    function GetFullName: string;
    function GetTextState: TdxPDFInteractiveFormFieldTextState;
    procedure SetResources(const AValue: TdxPDFResources);
    procedure ReadKids(ADictionary: TdxPDFReaderDictionary;
      AWidgetNumber: Integer);
    procedure WriteKids(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary);
  strict protected
    FName: string;
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Initialize(AForm: TdxPDFInteractiveForm;
      AParent: TdxPDFInteractiveFormField); reintroduce; virtual;
    procedure Read(ADictionary: TdxPDFReaderDictionary; ANumber: Integer);
      reintroduce; virtual;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
    procedure WriteProperties(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); virtual;

    function CreateAppearanceBuilder(AState: TdxPDFDocumentState)
      : TObject; virtual;
    function GetAcroFieldClass: TdxPDFAcroFormFieldClass; virtual;
    function GetDefaultValue: Variant; virtual;
    function GetName: string; virtual;
    procedure InternalSetValue(const AValue: Variant;
      AState: TdxPDFDocumentState); virtual;

    function CreateAcroField(AState: TdxPDFDocumentState): TdxPDFAcroFormField;
    procedure Changed(AState: TdxPDFDocumentState);

    property AlternateName: string read FAlternateName;
    property Form: TdxPDFInteractiveForm read FForm;
    property Resources: TdxPDFResources read FResources write SetResources;
    property ValuesProvider: TdxPDFInteractiveFormField read FValuesProvider;
  public
    class function Parse(AParent: TdxPDFInteractiveFormField;
      ADictionary: TdxPDFReaderDictionary; ANumber: Integer)
      : TdxPDFInteractiveFormField; static;
    constructor Create(AForm: TdxPDFInteractiveForm;
      AWidget: TdxPDFCustomAnnotation); overload; virtual;

    function GetFontInfo(AState: TdxPDFDocumentState): TdxPDFFontInfo;
    function GetValue: Variant; virtual;
    function HasFlag(AFlags: TdxPDFInteractiveFormFieldFlags): Boolean;
    function UseDefaultAppearanceForm: Boolean; virtual;
    procedure SetValue(const AValue: Variant; AState: TdxPDFDocumentState);

    property AppearanceCommands: TdxPDFCommandList read FAppearanceCommands;
    property DefaultValue: Variant read GetDefaultValue;
    property FullName: string read GetFullName;
    property Flags: TdxPDFInteractiveFormFieldFlags read FFlags;
    property Kids: TdxPDFInteractiveFormFieldCollection read FKids;
    property Name: string read GetName;
    property Parent: TdxPDFInteractiveFormField read FParent;
    property TextJustification: TdxPDFTextJustification read FTextJustification;
    property TextState: TdxPDFInteractiveFormFieldTextState read GetTextState;
    property Widget: TdxPDFCustomAnnotation read FWidget;
  end;

  { TdxPDFInteractiveFormFieldCollection }

  TdxPDFInteractiveFormFieldCollection = class(TdxPDFObject) // for internal use
  strict private
    FItems: TdxPDFReferencedObjects;
    function GetCount: Integer;
    function GetItem(Index: Integer): TdxPDFInteractiveFormField;
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary;
      AFieldArray: TdxPDFArray; AForm: TdxPDFInteractiveForm;
      AParent: TdxPDFInteractiveFormField); reintroduce;
    function Write(AHelper: TdxPDFWriterHelper): TdxPDFBase; override;
    procedure Add(AField: TdxPDFInteractiveFormField);
    procedure Delete(AField: TdxPDFInteractiveFormField);
  public
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxPDFInteractiveFormField
      read GetItem; default;
  end;

  { TdxPDFXFAForm }

  TdxPDFXFAForm = record // for internal use
  public
    Content: string;

    procedure Initialize(const AData: TBytes); overload;
    procedure Initialize(ARepository: TdxPDFDocumentRepository;
      AArray: TdxPDFArray); overload;
    function ToByteArray: TBytes;
  end;

  { TdxPDFInteractiveForm }

  TdxPDFInteractiveForm = class(TdxPDFObject) // for internal use
  strict private
    FFields: TdxPDFInteractiveFormFieldCollection;
    FDefaultAppearanceCommands: TdxPDFCommandList;
    FDefaultTextJustification: TdxPDFTextJustification;
    FNeedAppearances: Boolean;
    FResources: TdxPDFResources;
    FSignatureFlags: TdxPDFSignatureFlags;
    FXFAForm: TdxPDFXFAForm;
    //
    procedure SetResources(const AValue: TdxPDFResources);
    procedure ReadXFAForm(ADictionary: TdxPDFReaderDictionary);
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;

    function AddSignatureField(AWidget: TdxPDFCustomAnnotation;
      ASignature: TObject): TdxPDFInteractiveFormField;
    procedure Delete(AField: TdxPDFInteractiveFormField);
  public
    function HasSignatureFields: Boolean;
    //
    property DefaultAppearanceCommands: TdxPDFCommandList
      read FDefaultAppearanceCommands;
    property Fields: TdxPDFInteractiveFormFieldCollection read FFields;
    property NeedAppearances: Boolean read FNeedAppearances
      write FNeedAppearances;
    property Resources: TdxPDFResources read FResources write SetResources;
    property SignatureFlags: TdxPDFSignatureFlags read FSignatureFlags
      write FSignatureFlags;
  end;

  { TdxPDFDocumentInformation }

  TdxPDFDocumentInformation = class(TdxPDFObject)
  strict private
    FApplication: string;
    FAuthor: string;
    FCreationDate: TDateTime;
    FKeywords: string;
    FModificationDate: TDateTime;
    FProducer: string;
    FSubject: string;
    FTitle: string;
  protected
    FFileName: string;
    FFileSize: Int64;
    FVersion: TdxPDFVersion;

    procedure Initialize; override;
    procedure ReadProperties(ADictionary: TdxPDFReaderDictionary); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
  public
    class function GetTypeName: string; override;

    property Application: string read FApplication write FApplication;
    property Author: string read FAuthor write FAuthor;
    property CreationDate: TDateTime read FCreationDate;
    property Keywords: string read FKeywords write FKeywords;
    property ModificationDate: TDateTime read FModificationDate;
    property FileName: string read FFileName;
    property FileSize: Int64 read FFileSize;
    property Producer: string read FProducer write FProducer;
    property Subject: string read FSubject write FSubject;
    property Version: TdxPDFVersion read FVersion;
    property Title: string read FTitle write FTitle;
  end;

  { TdxPDFXMLPacket }

  TdxPDFXMLPacket = class(TdxXMLDocument)
  protected
    function GetFooterText: TdxXMLString; override;
    function GetHeaderText: TdxXMLString; override;
  public
    procedure AfterConstruction; override;
  end;

  { TdxPDFMetadata }

  TdxPDFMetadata = class(TdxPDFObject)
  protected
    FData: TdxPDFXMLPacket;

    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Read(AStream: TdxPDFStream); reintroduce; virtual;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;
  public
    class function GetTypeName: string; override;
    procedure Clear;
    procedure Update(AInformation: TdxPDFDocumentInformation);
  end;

  { TdxPDFCatalog }

  TdxPDFCatalog = class(TdxPDFObject) // for internal use
  strict private
    FAcroForm: TdxPDFInteractiveForm;
    FDictionary: TdxPDFReaderDictionary;
    FMetadata: TdxPDFMetadata;
    FNames: TdxPDFDocumentNames;
    FNeedReadNames: Boolean;
    FNeedReadOutlines: Boolean;
    FOpenAction: TdxPDFCustomAction;
    FOpenDestination: TdxPDFCustomDestination;
    FOutlines: TdxPDFObject;
    FPages: TdxPDFPages;

    function GetAcroForm: TdxPDFInteractiveForm;
    function GetNames: TdxPDFDocumentNames;
    function GetOutlines: TdxPDFObject;
    procedure SetAcroForm(const AValue: TdxPDFInteractiveForm);
    procedure SetPages(const AValue: TdxPDFPages);

    function CanWriteAcroForm: Boolean;
    procedure ReadAcroForm;
    procedure ReadInteractiveObjects;
    procedure ReadMetadata;
    procedure ReadNames(ADictionary: TdxPDFReaderDictionary);
    procedure ReadOutlines(ADictionary: TdxPDFReaderDictionary);
    procedure ReadPages;
    procedure WriteInteractiveObjects(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary);
  protected
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    procedure Initialize; override;
    procedure Read(ADictionary: TdxPDFReaderDictionary); override;
    procedure SetRepository(const AValue: TdxPDFDocumentRepository); override;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary); override;

    function AddSignatureField(AState: TdxPDFDocumentState; ASignature: TObject)
      : TdxPDFInteractiveFormField;
    function GetEmbeddedFileSpecification(const AName: string)
      : TdxPDFFileSpecification;
    function GetDestination(const AName: string): TdxPDFCustomDestination;
    procedure DeleteField(AField: TdxPDFInteractiveFormField);
    procedure PopulateAttachmentList(AList: TdxPDFFileAttachmentList);

    property OpenAction: TdxPDFCustomAction read FOpenAction;
    property OpenDestination: TdxPDFCustomDestination read FOpenDestination;
    property Outlines: TdxPDFObject read GetOutlines;
  public
    class function GetTypeName: string; override;

    property AcroForm: TdxPDFInteractiveForm read GetAcroForm write SetAcroForm;
    property Metadata: TdxPDFMetadata read FMetadata;
    property Names: TdxPDFDocumentNames read GetNames;
    property Pages: TdxPDFPages read FPages write SetPages;
  end;

  { TdxPDFCustomCommand }

  TdxPDFCustomCommand = class(TdxPDFBase) // for internal use
  strict private
    function EnsureRange(const AValue: Double): Double; overload;
    function EnsureRange(const AValue: Integer): Integer; overload;
  protected
    class function GetObjectType: TdxPDFBaseType; override;
    procedure Write(AWriter: TdxPDFWriter; AResources: TdxPDFResources);
      reintroduce; virtual;

    procedure WriteCommandName(AWriter: TdxPDFWriter);
    procedure WriteOperand(AWriter: TdxPDFWriter;
      const AOperand: Double); overload;
    procedure WriteOperand(AWriter: TdxPDFWriter;
      const AOperand: Integer); overload;
    procedure WriteOperand(AWriter: TdxPDFWriter;
      const AOperand: TdxSizeF); overload;
    procedure WriteOperand(AWriter: TdxPDFWriter;
      const AOperand: TdxPointF); overload;
    procedure WriteOperand(AWriter: TdxPDFWriter;
      const AOperand: string); overload;
    procedure WriteUnaryCommand(AWriter: TdxPDFWriter;
      const AOperand: Single); overload;
    procedure WriteUnaryCommand(AWriter: TdxPDFWriter;
      const AOperand: Integer); overload;
    procedure WriteUnaryCommand(AWriter: TdxPDFWriter;
      const AOperand: string); overload;
    procedure WriteUnaryCommand(AWriter: TdxPDFWriter;
      const AOperand: TdxPointF); overload;
  public
    constructor Create; overload; override;
    constructor Create(AOperands: TdxPDFCommandOperandStack;
      AResources: TdxPDFResources); overload; virtual;

    class function GetName: string; virtual;

    procedure Execute(ACommandInterpreter: IdxPDFCommandInterpreter);
      virtual; abstract;
    function GetCommandCount: Integer; virtual;
  end;

  { TdxPDFCommandList }

  TdxPDFCommandList = class(TdxPDFReferencedObjects)
  strict private
    function GetItem(Index: Integer): TdxPDFCustomCommand; inline;
  protected
    procedure Read(AData: TBytes; ARepository: TdxPDFCustomRepository;
      AResources: TdxPDFResources); overload;
    procedure Read(AStream: TdxPDFStream; ARepository: TdxPDFCustomRepository;
      AResources: TdxPDFResources); overload;
    procedure Write(AHelper: TdxPDFWriterHelper;
      ADictionary: TdxPDFWriterDictionary; AResources: TdxPDFResources);
  public
    function GetCommandCount: Integer;
    function ToByteArray: TBytes; overload;
    function ToByteArray(AResources: TdxPDFResources): TBytes; overload;
    //
    property Items[Index: Integer]: TdxPDFCustomCommand read GetItem; default;
  end;

  { TdxPDFLineStyle }

  TdxPDFLineStyle = class(TdxPDFReferencedObject) // for internal use
  strict private
    FPattern: TDoubleDynArray;
    FPhase: Double;

    function AsDouble(AValue: TdxPDFReferencedObject): Double;
    procedure ReadPattern(APattern: TdxPDFArray);
  protected
    function Write: TdxPDFArray;
    function WritePattern: TdxPDFArray;
  public
    class function CreateSolid: TdxPDFLineStyle; static;
    class function Parse(AParameters: TdxPDFArray): TdxPDFLineStyle; static;
    constructor Create(const APattern: TDoubleDynArray; APhase: Double);
      reintroduce; overload;
    constructor Create(APattern: TdxPDFReferencedObject); reintroduce; overload;
    constructor Create(APattern, APhase: TdxPDFReferencedObject);
      reintroduce; overload;
    constructor Create(AParameters: TdxPDFArray); reintroduce; overload;

    function IsDashed: Boolean;

    property Pattern: TDoubleDynArray read FPattern;
    property Phase: Double read FPhase;
  end;

  { TdxPDFDocumentRepository }

  TdxPDFDocumentRepository = class(TdxPDFCustomRepository) // for internal use
  strict private
    FCatalog: TdxPDFCatalog;
    FEncryptionInfo: TdxPDFEncryptionInfo;
    FFolderName: string;
    FFontDataStorage: TdxPDFFontDataStorage;
    FImageDataStorage: TdxPDFDocumentImageDataStorage;

    FLock: TRTLCriticalSection;
    FObjectHolder: TdxPDFReferencedObjects;
    FParser: TdxPDFDocumentParser;
    FResolvedForms: TdxFastList; // T965896
    FResolvedInteractiveFormFields: TdxPDFObjectIndex;
    FResolvedWidgets: TdxPDFObjectIndex;
    FSharedResources: TdxPDFUniqueReferences;
    FStream: TStream;
    FXReferences: TDictionary<Integer, Int64>;

    function CreateObjectParser(AObjectNumber: Integer): TdxPDFStructureParser;
    function GetObjectCount: Integer;
    function ResolveIndirectObject(AObject: TdxPDFIndirectObject)
      : TdxPDFReferencedObject;
    function ResolveIndirectReference(AReference: TdxPDFReference)
      : TdxPDFReferencedObject;
    function ResolveStreamElement(AElement: TdxPDFStreamElement)
      : TdxPDFReferencedObject;
    procedure ReplaceObject(ANumber: Integer; AObject: TdxPDFReferencedObject);
  protected
    OnAddField: TNotifyEvent;
    OnDeleteField: TNotifyEvent;

    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;

    function CreateAcroForm: TdxPDFInteractiveForm;
    function CreateWidgetAnnotation(APage: TdxPDFPage;
      const ARect: TdxPDFRectangle; AFlags: TdxPDFAnnotationFlags)
      : TdxPDFCustomAnnotation;
    function GetResources(ADictionary: TdxPDFReaderDictionary): TdxPDFResources;
    function IsResourcesShared(AResources: TdxPDFResources): Boolean;
    function ResolveObject(ANumber: Integer): TdxPDFReferencedObject; override;
    procedure AddObject(AObject: TdxPDFObject);
    procedure DeleteObject(AObject: TdxPDFObject);
    procedure RemoveResolvedForm(AForm: TdxPDFForm);
  public
    constructor Create(AStream: TStream);

    procedure Clear; override;

    function CheckPassword(AAttemptsLimit: Integer;
      AOnGetPasswordEvent: TdxGetPasswordEvent): Boolean;
    function CreateFont(AOwner: TdxPDFObject;
      ADictionary: TdxPDFReaderDictionary): TdxPDFCustomFont; virtual;
    function CreateForm(ADictionary: TdxPDFReaderDictionary): TdxPDFForm;
    function CreateImage(AOwner: TdxPDFObject;
      ADictionary: TdxPDFReaderDictionary): TdxPDFDocumentImage;
    function GetAction(ANumber: Integer): TdxPDFCustomAction; overload;
    function GetAction(AObject: TdxPDFBase): TdxPDFCustomAction; overload;
    function GetAnnotation(ANumber: Integer; APage: TdxPDFPage)
      : TdxPDFCustomAnnotation; overload;
    function GetAnnotation(AObject: TdxPDFBase; APage: TdxPDFPage)
      : TdxPDFCustomAnnotation; overload;
    function GetDestination(ANumber: Integer): TdxPDFCustomDestination;
      overload;
    function GetDestination(AObject: TdxPDFBase)
      : TdxPDFCustomDestination; overload;
    function GetDictionary(ANumber: Integer): TdxPDFReaderDictionary; overload;
    function GetInteractiveFormField(AForm: TdxPDFInteractiveForm;
      AParent: TdxPDFInteractiveFormField; ANumber: Integer)
      : TdxPDFInteractiveFormField;
    function ResolveInteractiveFormField(ANumber: Integer)
      : TdxPDFInteractiveFormField;
    function GetPage(ANumber: Integer): TdxPDFPage;
    function GetString(ANumber: Integer): string;
    function GetWidget(ANumber: Integer): TdxPDFCustomAnnotation;
    function IsSharedResources(AResources: TdxPDFResources): Boolean;
    function IsValidReferences: Boolean;
    function TryGetAnnotation(ANumber: Integer;
      out AAnnotation: TdxPDFCustomAnnotation): Boolean;
    function TryGetDictionary(ANumber: Integer;
      out ADictionary: TdxPDFDictionary): Boolean;
    procedure AddStreamElement(ANumber: Integer;
      AObject: TdxPDFReferencedObject);
    procedure ReadEncryptionInfo(ADictionary: TdxPDFDictionary;
      const ADocumentID: TdxPDFDocumentID);
    procedure RemoveCorruptedObjects;

    property Catalog: TdxPDFCatalog read FCatalog write FCatalog;
    property EncryptionInfo: TdxPDFEncryptionInfo read FEncryptionInfo;
    property FolderName: string read FFolderName;
    property FontDataStorage: TdxPDFFontDataStorage read FFontDataStorage;
    property ImageDataStorage: TdxPDFDocumentImageDataStorage
      read FImageDataStorage;
    property ObjectCount: Integer read GetObjectCount;
    property Parser: TdxPDFDocumentParser read FParser;
    property Stream: TStream read FStream;
  end;

  { TdxPDFReaderDictionary }

  TdxPDFReaderDictionary = class(TdxPDFDictionary) // for internal use
  strict private
    FRepository: TdxPDFDocumentRepository;
  public
    constructor Create(ARepository: TdxPDFDocumentRepository);

    function GetAction(const AKey: string): TdxPDFCustomAction;
    function GetAnnotationAppearance(AObject: TdxPDFBase;
      const AParentBBox: TdxPDFRectangle): TdxPDFAnnotationAppearances;
    function GetAnnotationHighlightingMode: TdxPDFAnnotationHighlightingMode;
    function GetAppearance(AResources: TdxPDFResources): TdxPDFCommandList;
    function GetColor(const AKey: string): TdxPDFColor;
    function GetDeferredFormFieldCollection(const AKey: string)
      : TdxPDFInteractiveFormFieldCollection;
    function GetDestinationInfo(const AKey: string): TdxPDFDestinationInfo;
    function GetDictionary(const AKey: string; const AAlternateKey: string = '')
      : TdxPDFReaderDictionary; overload;
    function GetForm(ANumber: Integer): TdxPDFForm;
    function GetObject(const AKey: string): TdxPDFBase; override;
    function GetObjectNumber(const AKey: string): Integer;
    function GetTextJustification: TdxPDFTextJustification;
    function TryGetDictionary(const AKey: string;
      out AValue: TdxPDFReaderDictionary): Boolean;
    function TryGetStreamDictionary(const AKey: string;
      out AValue: TdxPDFReaderDictionary): Boolean;
    function GetResources(const AKey: string): TdxPDFResources;
    procedure PopulateList(const AKey: string; AList: TStringList);

    property Repository: TdxPDFDocumentRepository read FRepository;
  end;

  { TdxPDFAcroFormField }

  TdxPDFAcroFormField = class(TdxPDFRecognizedObject)
  strict private
    FDocumentState: TdxPDFDocumentState;
    FField: TdxPDFInteractiveFormField;
    function GetName: string;
    procedure SetField(const AValue: TdxPDFInteractiveFormField);
  strict protected
    function GetAnnotation: TdxPDFCustomAnnotation; virtual;
    function GetBounds: TdxPDFOrientedRect; virtual;
    function GetCursor: TCursor; virtual;
    function GetFlags: TdxPDFInteractiveFormFieldFlags; virtual;
    function GetHint: string; virtual;
    function GetInteractiveOperation: TdxPDFInteractiveOperation; virtual;

    function GetPage: TdxPDFPage;
    function GetPageIndex: Integer;
    function GetRect: TdxRectF;

    property Annotation: TdxPDFCustomAnnotation read GetAnnotation;
    property Flags: TdxPDFInteractiveFormFieldFlags read GetFlags;
    property Hint: string read GetHint;
    property Name: string read GetName;
    property Page: TdxPDFPage read GetPage;
  protected
    property Bounds: TdxPDFOrientedRect read GetBounds;
    property DocumentState: TdxPDFDocumentState read FDocumentState
      write FDocumentState;
    property InteractiveOperation: TdxPDFInteractiveOperation
      read GetInteractiveOperation;
    property Field: TdxPDFInteractiveFormField read FField write SetField;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  { TdxPDFAcroFormActionField }

  TdxPDFAcroFormActionField = class(TdxPDFAcroFormField,
    IdxPDFInteractiveObject, IdxPDFHintableObject)
  strict protected
    function GetCursor: TCursor; override;
    function GetInteractiveOperation: TdxPDFInteractiveOperation; override;

    function IsResetFocusingNeeded: Boolean; virtual;
  protected
    procedure ExecuteOperation(const AController
      : IdxPDFInteractivityController); // for internal use
  end;

  { TdxPDFAnnotationField }

  TdxPDFAnnotationFieldClass = class of TdxPDFAnnotationField;

  TdxPDFAnnotationField = class(TdxPDFAcroFormActionField)
  strict protected
    FAnnotation: TdxPDFCustomAnnotation;
    function GetAnnotation: TdxPDFCustomAnnotation; override;
    function GetBounds: TdxPDFOrientedRect; override;
    function GetHint: string; override;
    function GetHitCode: Integer; override;
  protected
    procedure SetAnnotation(const AValue: TdxPDFCustomAnnotation);
  public
    destructor Destroy; override;
  end;

  { TdxPDFFileAttachmentAnnotationField }

  TdxPDFFileAttachmentAnnotationField = class(TdxPDFAnnotationField)
  strict private
    function GetAttachment: TdxPDFFileAttachment;
  strict protected
    function GetCursor: TCursor; override;
    function GetHitCode: Integer; override;
    function IsResetFocusingNeeded: Boolean; override;
  protected
    property Attachment: TdxPDFFileAttachment read GetAttachment;
  end;

  { TdxPDFHyperlink }

  TdxPDFHyperlink = class(TdxPDFAnnotationField)
  protected
    function GetHint: string; override;
    function GetHitCode: Integer; override;
  public
    property Hint;
  end;

  TdxPDFAcroFormFieldList = class
    (TdxPDFRecognizedObjectList<TdxPDFAcroFormField>); // for internal use
  TdxPDFAnnotationFieldList = class
    (TdxPDFRecognizedObjectList<TdxPDFAnnotationField>); // for internal use
  TdxPDFHyperlinkList = class(TdxPDFRecognizedObjectList<TdxPDFHyperlink>);

  { TdxPDFRecognizedContent }

  TdxPDFRecognizedContent = class // for internal use
  strict private
    FAcroFormFields: TdxPDFAcroFormFieldList;
    FAnnotationFields: TdxPDFAnnotationFieldList;
    FAttachments: TList<TdxPDFFileAttachment>;
    FHyperlinks: TdxPDFHyperlinkList;
    FImages: TdxPDFImageList;
    FTextLines: TdxPDFTextLineList;
    function GetText: string;
  protected
    procedure AddAnnotationField(AField: TdxPDFAnnotationField);

    property Attachments: TList<TdxPDFFileAttachment> read FAttachments;
    // for internal use
    property AnnotationFields: TdxPDFAnnotationFieldList read FAnnotationFields;
  public
    constructor Create;
    destructor Destroy; override;

    property AcroFormFields: TdxPDFAcroFormFieldList read FAcroFormFields;
    // for internal use
    property Hyperlinks: TdxPDFHyperlinkList read FHyperlinks;
    // for internal use
    property Images: TdxPDFImageList read FImages; // for internal use
    property Text: string read GetText; // for internal use
    property TextLines: TdxPDFTextLineList read FTextLines; // for internal use
  end;

  { TdxPDFGraphicsState }

  TdxPDFGraphicsState = class(TdxPDFReferencedObject) // for internal use
  strict private
    FNonStrokingColor: TdxPDFColor;
    FNonStrokingColorSpace: TdxPDFCustomColorSpace;
    FParameters: TdxPDFGraphicsStateParameters;
    FStrokingColor: TdxPDFColor;
    FStrokingColorSpace: TdxPDFCustomColorSpace;
    FTextState: TdxPDFTextState;

    FDeviceTransformMatrix: TdxPDFTransformationMatrix;
    FSoftMaskTransformMatrix: TdxPDFTransformationMatrix;
    FTransformMatrix: TdxPDFTransformationMatrix;

    procedure SetNonStrokingColorSpace(const AValue: TdxPDFCustomColorSpace);
    procedure SetStrokingColorSpace(const AValue: TdxPDFCustomColorSpace);
    procedure RecreateTextState;
    procedure RecreateParameters;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Assign(AGraphicsState: TdxPDFGraphicsState);
    procedure ApplyParameters(AParameters: TdxPDFGraphicsStateParameters);
    procedure Reset;

    property DeviceTransformMatrix: TdxPDFTransformationMatrix
      read FDeviceTransformMatrix;
    property NonStrokingColor: TdxPDFColor read FNonStrokingColor
      write FNonStrokingColor;
    property NonStrokingColorSpace: TdxPDFCustomColorSpace
      read FNonStrokingColorSpace write SetNonStrokingColorSpace;
    property Parameters: TdxPDFGraphicsStateParameters read FParameters;
    property SoftMaskTransformMatrix: TdxPDFTransformationMatrix
      read FSoftMaskTransformMatrix write FSoftMaskTransformMatrix;
    property StrokingColor: TdxPDFColor read FStrokingColor
      write FStrokingColor;
    property StrokingColorSpace: TdxPDFCustomColorSpace read FStrokingColorSpace
      write SetStrokingColorSpace;
    property TextState: TdxPDFTextState read FTextState write FTextState;
    property TransformMatrix: TdxPDFTransformationMatrix read FTransformMatrix
      write FTransformMatrix;
  end;

  { TdxPDFTextParser }

  TdxPDFTextParser = class // for internal use
  strict private
    FContent: TdxPDFRecognizedContent;
    FFontDataStorage: TObjectDictionary<TdxPDFCustomFont, TdxPDFFontData>;
    FPageBlocks: TObjectList<TdxPDFTextBlock>;
    FPageCropBox: TdxRectF;
    FParserState: TdxPDFTextParserState;
  public
    constructor Create(const APageCropBox: TdxRectF;
      AContent: TdxPDFRecognizedContent);
    destructor Destroy; override;

    procedure AddBlock(const AStringData: TdxPDFStringData;
      AState: TdxPDFGraphicsState);
    procedure Parse;
  end;

  { TdxPDFTextUtils }

  TdxPDFTextUtils = class(TdxPDFRecognizedTextUtils)
  public
    class function ConvertToString(const ARanges: TdxPDFPageTextRanges;
      APages: TdxPDFPages): string; overload;
  end;

function dxPDFCreateDocumentObject(AOwner: TdxPDFObject;
  ADictionary: TdxPDFDictionary; const ATypeName: string;
  ARepository: TdxPDFCustomRepository): TdxPDFObject; // for internal use
function dxPDFGetDocumentObjectClass(const ATypeName: string)
  : TdxPDFObjectClass; // for internal use
function dxPDFTryGetDocumentObjectClass(const ATypeName: string;
  out AClass: TdxPDFObjectClass): Boolean; // for internal use
procedure dxPDFRegisterDocumentObjectClass(AClass: TdxPDFObjectClass); overload;
// for internal use
procedure dxPDFRegisterDocumentObjectClass(const AName: string;
  AClass: TdxPDFObjectClass); overload; // for internal use
procedure dxPDFUnregisterDocumentObjectClass(AClass: TdxPDFObjectClass);
  overload; // for internal use
procedure dxPDFUnregisterDocumentObjectClass(const AName: string;
  AClass: TdxPDFObjectClass); overload; // for internal use

implementation

uses
  RTLConsts, Variants, Math, TypInfo, Character, Contnrs, IOUtils,
  dxDPIAwareUtils, cxDateUtils, StrUtils, cxLibraryConsts,
  dxCore, dxCoreGraphics, dxStringHelper, dxTypeHelpers, dxHash, dxGenerics,
  dxPDFDocument, dxPDFCommandInterpreter,
  dxPDFColorSpace, dxPDFFont, dxPDFFontEncoding, dxPDFCommand, dxPDFFunction,
  dxPDFShading, dxPDFType1Font,
  dxPDFFontUtils, dxPDFUtils, dxPDFInteractivity, dxPDFFlateLZW, dxPDFSignature;

type
  TdxPDFBaseAccess = class(TdxPDFBase);
  TdxPDFBaseStreamAccess = class(TdxPDFBaseStream);
  TdxPDFCustomCommandAccess = class(TdxPDFCustomCommand);
  TdxPDFCustomCommandInterpreterAccess = class(TdxPDFCustomCommandInterpreter);
  TdxPDFCustomObjectAccess = class(TdxPDFBase);
  TdxPDFDictionaryAccess = class(TdxPDFDictionary);
  TdxPDFDocumentAccess = class(TdxPDFDocument);
  TdxPDFDocumentPageContentsAccess = class(TdxPDFPageContents);
  TdxPDFDocumentPagesAccess = class(TdxPDFPages);
  TdxPDFDocumentStreamObjectAccess = class(TdxPDFStreamObject);
  TdxPDFEmbeddedFileSpecificationTreeAccess = class
    (TdxPDFEmbeddedFileSpecificationTree);
  TdxPDFImageAccess = class(TdxPDFImage);
  TdxPDFJumpActionAccess = class(TdxPDFJumpAction);
  TdxPDFLinkAnnotationAccess = class(TdxPDFLinkAnnotation);
  TdxPDFLineStyleAccess = class(TdxPDFLineStyle);
  TdxPDFMarkupAnnotationAccess = class(TdxPDFMarkupAnnotation);
  TdxPDFModifyTransformationMatrixCommandAccess = class
    (TdxPDFModifyTransformationMatrixCommand);
  TdxPDFNumericObjectAccess = class(TdxPDFNumericObject);
  TdxPDFRecognizedObjectAccess = class(TdxPDFReferencedObject);
  TdxPDFReferencedObjectDictionaryAccess = class
    (TdxPDFReferencedObjectDictionary);
  TdxPDFStreamAccess = class(TdxPDFStream);
  TdxPDFTextLineAccess = class(TdxPDFTextLine);
  TdxPDFWidgetAnnotationAccess = class(TdxPDFWidgetAnnotation);
  TdxPDFWriterAccess = class(TdxPDFWriter);

  EdxPDFCommandOperandsException = class(EdxPDFException);
  TdxPDFBaseParserAccess = class(TdxPDFBaseParser);

  { TdxPDFWriterDataObject }

  TdxPDFWriterDataObject = class(TdxPDFObject)
  strict private
    FData: TdxPDFBase;

    procedure SetData(AValue: TdxPDFBase);
  protected
    function Write(AHelper: TdxPDFWriterHelper): TdxPDFBase; override;
  public
    constructor Create(const AData: TdxPDFBase);
    destructor Destroy; override;
    //
    property Data: TdxPDFBase read FData write SetData;
  end;

  { TdxPDFObjectStreamElementInfo }

  TdxPDFObjectStreamElementInfo = record
  private
    FNumber: Integer;
    FOffset: Integer;
  public
    class function Create(ANumber, AOffset: Integer)
      : TdxPDFObjectStreamElementInfo; static;
    property Number: Integer read FNumber;
    property Offset: Integer read FOffset;
  end;

  { TdxPDFTextWordCharacterComparer }

  TdxPDFTextWordCharacterComparer = class(TComparer<TdxPDFTextCharacter>)
  public
    function Compare(const Left, Right: TdxPDFTextCharacter): Integer; override;
  end;

  { TdxPDFFileAttachmentComparer }

  TdxPDFFileAttachmentComparer = class(TInterfacedObject,
    IComparer<TdxPDFFileAttachment>)
  strict private
    function Compare(const Left, Right: TdxPDFFileAttachment): Integer;
  end;

  { TdxPDFObjectFactory }

  TdxPDFObjectFactory = class(TdxPDFFactory<TdxPDFObjectClass>)
  public
    function GetObjectClass(const AType: string): TdxPDFObjectClass;
    procedure RegisterClass(AClass: TdxPDFObjectClass); overload;
    procedure UnregisterClass(AClass: TdxPDFObjectClass); overload;
  end;

  { TdxPDFReaderObjectStream }

  TdxPDFReaderObjectStream = class(TdxPDFBase)
  strict private
    FObjects: TdxPDFReferencedObjects;
  protected
    class function GetObjectType: TdxPDFBaseType; override;
  public
    constructor Create(ANumber: Integer; AStream: TdxPDFStream);
    destructor Destroy; override;

    property Objects: TdxPDFReferencedObjects read FObjects;
  end;

var
  dxgPDFDocumentObjectFactory: TdxPDFObjectFactory;

function HasFlag(ASourceFlags, AFlags: TdxPDFGlyphMappingFlags): Boolean;
begin
  Result := (Integer(ASourceFlags) and Integer(AFlags)) <> 0;
end;

function dxPDFIsObjectSupported(AOwner: TdxPDFObject;
  ADictionary: TdxPDFDictionary; const ATypeName: string;
  ARepository: TdxPDFCustomRepository): Boolean;
begin
  Result := dxPDFGetDocumentObjectClass
    (ADictionary.GetString(ATypeName)) <> nil;
end;

function dxPDFDocumentObjectFactory: TdxPDFObjectFactory;
begin
  if dxgPDFDocumentObjectFactory = nil then
    dxgPDFDocumentObjectFactory := TdxPDFObjectFactory.Create;
  Result := dxgPDFDocumentObjectFactory;
end;

function dxPDFCreateDocumentObject(AOwner: TdxPDFObject;
  ADictionary: TdxPDFDictionary; const ATypeName: string;
  ARepository: TdxPDFCustomRepository): TdxPDFObject;
var
  AClass: TdxPDFObjectClass;
begin
  if dxPDFTryGetDocumentObjectClass(ADictionary.GetString(ATypeName), AClass)
  then
    Result := AClass.Create(AOwner)
  else
    Result := nil;
end;

function dxPDFGetDocumentObjectClass(const ATypeName: string)
  : TdxPDFObjectClass;
begin
  Result := dxPDFDocumentObjectFactory.GetObjectClass(ATypeName);
end;

function dxPDFTryGetDocumentObjectClass(const ATypeName: string;
  out AClass: TdxPDFObjectClass): Boolean;
begin
  AClass := dxPDFGetDocumentObjectClass(ATypeName);
  Result := AClass <> nil;
end;

procedure dxPDFUnregisterDocumentObjectClass(const AName: string;
  AClass: TdxPDFObjectClass);
begin
  if dxgPDFDocumentObjectFactory <> nil then
    dxPDFDocumentObjectFactory.UnregisterClass(AName);
end;

procedure dxPDFUnregisterDocumentObjectClass(AClass: TdxPDFObjectClass);
begin
  dxPDFUnregisterDocumentObjectClass(AClass.GetTypeName, AClass);
end;

procedure dxPDFRegisterDocumentObjectClass(AClass: TdxPDFObjectClass);
begin
  dxPDFRegisterDocumentObjectClass(AClass.GetTypeName, AClass);
end;

procedure dxPDFRegisterDocumentObjectClass(const AName: string;
  AClass: TdxPDFObjectClass);
begin
  dxPDFDocumentObjectFactory.Register(AName, AClass);
end;

{ TdxPDFWriterDataObject }

constructor TdxPDFWriterDataObject.Create(const AData: TdxPDFBase);
begin
  Data := AData;
end;

destructor TdxPDFWriterDataObject.Destroy;
begin
  Data := nil;
  inherited;
end;

procedure TdxPDFWriterDataObject.SetData(AValue: TdxPDFBase);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FData));
end;

function TdxPDFWriterDataObject.Write(AHelper: TdxPDFWriterHelper): TdxPDFBase;
begin
  Result := Data;
end;

{ TdxPDFObjectStreamElementInfo }

class function TdxPDFObjectStreamElementInfo.Create(ANumber, AOffset: Integer)
  : TdxPDFObjectStreamElementInfo;
begin
  Result.FNumber := ANumber;
  Result.FOffset := AOffset;
end;

{ TdxPDFTextWordCharacterComparer }

function TdxPDFTextWordCharacterComparer.Compare(const Left,
  Right: TdxPDFTextCharacter): Integer;
begin
  Result := IfThen(TdxPDFTextUtils.GetOrientedDistance(Left.Bounds.TopLeft,
    Right.Bounds.TopLeft, Left.Bounds.Angle) < 0, 1, -1);
end;

{ TdxPDFFileAttachmentComparer }

function TdxPDFFileAttachmentComparer.Compare(const Left,
  Right: TdxPDFFileAttachment): Integer;
begin
  Result := CompareStr(Left.FileName, Right.FileName);
end;

{ TdxPDFExportParameters }

constructor TdxPDFExportParameters.Create;
begin
  inherited Create;
end;

constructor TdxPDFExportParameters.Create(AState: TdxPDFDocumentState);
begin
  inherited Create;
  FDocumentState := AState;
  Angle := FDocumentState.RotationAngle;
end;

function TdxPDFExportParameters.IsCanceled: Boolean;
begin
  Result := Assigned(CancelCallback) and CancelCallback;
end;

{ TdxPDFObjectFactory }

function TdxPDFObjectFactory.GetObjectClass(const AType: string)
  : TdxPDFObjectClass;
begin
  if not TryGetClass(AType, Result) then
    Result := nil;
end;

procedure TdxPDFObjectFactory.RegisterClass(AClass: TdxPDFObjectClass);
begin
  Register(AClass.GetTypeName, AClass);
end;

procedure TdxPDFObjectFactory.UnregisterClass(AClass: TdxPDFObjectClass);
begin
  UnregisterClass(AClass.GetTypeName);
end;

{ TdxPDFReaderObjectStream }

constructor TdxPDFReaderObjectStream.Create(ANumber: Integer;
  AStream: TdxPDFStream);

  procedure ReadObjectInfos(const AData: TBytes; AObjectCount: Integer;
    AObjectInfos: TList<TdxPDFObjectStreamElementInfo>);
  var
    AParser: TdxPDFBaseParserAccess;
    I, AObjectNumber, AObjectOffset: Integer;
  begin
    AParser := TdxPDFBaseParserAccess(TdxPDFBaseParser.Create(AData, 0));
    try
      for I := 0 to AObjectCount - 1 do
      begin
        AParser.SkipSpaces;
        AObjectNumber := AParser.ReadInteger;
        AParser.ReadNext;
        AObjectOffset := AParser.ReadInteger;
        AParser.ReadNext;
        if TdxPDFUtils.IsIntegerValid(AObjectNumber) and
          TdxPDFUtils.IsIntegerValid(AObjectOffset) then
          AObjectInfos.Add(TdxPDFObjectStreamElementInfo.Create(AObjectNumber,
            AObjectOffset))
      end;
    finally
      AParser.Free;
    end;
  end;

  procedure ReadObjects(const AData: TBytes;
    AElementInfos: TList<TdxPDFObjectStreamElementInfo>;
    AObjectsStart: Integer);
  var
    AObject: TdxPDFBase;
    AParser: TdxPDFStructureParser;
    I: Integer;
  begin
    AParser := TdxPDFStructureParser.Create
      (TdxPDFReaderDictionary(AStream.Dictionary).Repository);
    try
      for I := 0 to AElementInfos.Count - 1 do
      begin
        AObject := AParser.ReadObject(AData, AObjectsStart + AElementInfos
          [I].Offset);
        AObject.Number := AElementInfos[I].Number;
        if AObject <> nil then
          FObjects.Add(AObject);
      end;
      FObjects.TrimExcess;
    finally
      AParser.Free;
    end;
  end;

var
  AData: TBytes;
  AObjectCount, AObjectsStart: Integer;
  AObjectInfos: TList<TdxPDFObjectStreamElementInfo>;
begin
  inherited Create(ANumber, ANumber);
  AObjectCount := AStream.Dictionary.GetInteger(TdxPDFKeywords.Count);
  AObjectsStart := AStream.Dictionary.GetInteger('First');
  if TdxPDFUtils.IsIntegerValid(AObjectCount) or
    TdxPDFUtils.IsIntegerValid(AObjectsStart) and
    (AStream.Dictionary.GetString(TdxPDFKeywords.TypeKey)
    = TdxPDFKeywords.ObjectStream) then
  begin
    FObjects := TdxPDFReferencedObjects.Create;
    AData := AStream.UncompressedData;
    AObjectInfos := TList<TdxPDFObjectStreamElementInfo>.Create;
    try
      ReadObjectInfos(AData, AObjectCount, AObjectInfos);
      ReadObjects(AData, AObjectInfos, AObjectsStart);
    finally
      AObjectInfos.Free;
    end;
  end;
end;

destructor TdxPDFReaderObjectStream.Destroy;
begin
  FreeAndNil(FObjects);
  inherited Destroy;
end;

class function TdxPDFReaderObjectStream.GetObjectType: TdxPDFBaseType;
begin
  Result := otObjectStream;
end;

{ TdxPDFCommandList }

function TdxPDFCommandList.GetCommandCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
    Inc(Result, Items[I].GetCommandCount);
end;

function TdxPDFCommandList.ToByteArray: TBytes;
var
  AResources: TdxPDFResources;
begin
  AResources := TdxPDFResources.Create(nil);
  try
    Result := ToByteArray(AResources);
  finally
    dxPDFFreeObject(AResources);
  end;
end;

function TdxPDFCommandList.ToByteArray(AResources: TdxPDFResources): TBytes;
var
  AWriter: TdxPDFWriter;
  I: Integer;
begin
  AWriter := TdxPDFWriter.Create(TdxPDFMemoryStream.Create, True);
  try
    for I := 0 to Count - 1 do
    begin
      Items[I].Write(AWriter, AResources);
      AWriter.WriteLineFeed;
    end;
    Result := TdxPDFMemoryStream(AWriter.Stream).Data;
  finally
    AWriter.Free;
  end;
end;

procedure TdxPDFCommandList.Read(AData: TBytes;
  ARepository: TdxPDFCustomRepository; AResources: TdxPDFResources);
begin
  TdxPDFCommandStreamParser.Parse(ARepository, AData, Self, AResources)
end;

procedure TdxPDFCommandList.Read(AStream: TdxPDFStream;
  ARepository: TdxPDFCustomRepository; AResources: TdxPDFResources);
begin
  if AStream <> nil then
    Read(AStream.UncompressedData, ARepository, AResources)
  else
    Clear;
end;

procedure TdxPDFCommandList.Write(AHelper: TdxPDFWriterHelper;
  ADictionary: TdxPDFWriterDictionary; AResources: TdxPDFResources);
begin
  ADictionary.SetStreamData(ToByteArray(AResources));
end;

function TdxPDFCommandList.GetItem(Index: Integer): TdxPDFCustomCommand;
begin
  Result := inherited Items[Index] as TdxPDFCustomCommand;
end;

{ TdxPDFFontInfo }

function TdxPDFFontInfo.GetFontLineSize: Single;
begin
  Result := FontSize / 14.0;
end;

{ TdxPDFWriterArray }

constructor TdxPDFWriterArray.Create(AHelper: TdxPDFWriterHelper);
begin
  inherited Create;
  FHelper := AHelper;
end;

procedure TdxPDFWriterArray.AddReference(const AData: TdxPDFBase);
begin
  if AData <> nil then
    AddReference(FHelper.CreateIndirectObject(AData))
  else
    Add(0);
end;

procedure TdxPDFWriterArray.AddReference(const AObject: TdxPDFObject);
begin
  if AObject <> nil then
    AddReference(FHelper.RegisterIndirectObject(AObject))
  else
    Add(0);
end;

{ TdxPDFWriterDictionary }

constructor TdxPDFWriterDictionary.Create(AHelper: TdxPDFWriterHelper);
begin
  inherited Create;
  FHelper := AHelper;
end;

procedure TdxPDFWriterDictionary.Add(const AKey: string;
  AObjectList: TdxPDFObjectList);
begin
  if AObjectList.Count > 0 then
    AddInline(AKey, AObjectList);
end;

procedure TdxPDFWriterDictionary.Add(const AKey: string;
  AObject: TdxPDFCustomColorSpace);
begin
  if AObject <> nil then
    Add(AKey, FHelper.PrepareToWrite(AObject));
end;

procedure TdxPDFWriterDictionary.Add(const AKey: string;
  const AColor: TdxPDFColor);
begin
{$IFDEF DEBUG}
  if AColor.Pattern <> nil then
    TdxPDFUtils.RaiseTestException;
{$ENDIF}
  if not AColor.IsNull then
    Add(AKey, AColor.Components);
end;

procedure TdxPDFWriterDictionary.Add(const AKey: string;
  const ADestinationInfo: TdxPDFDestinationInfo);
begin
  if ADestinationInfo.IsValid then
    Add(AKey, ADestinationInfo.Write(FHelper));
end;

procedure TdxPDFWriterDictionary.Add(const AKey: string;
  const AList: TStringList);
var
  I: Integer;
  AArray: TdxPDFArray;
begin
  if AList.Count = 0 then
    Exit;
  AArray := FHelper.CreateArray;
  for I := 0 to AList.Count - 1 do
    AArray.Add(AList[I]);
  Add(AKey, AArray);
end;

procedure TdxPDFWriterDictionary.AddInline(const AKey: string;
  const ATree: TdxPDFCustomTree);
begin
  if ATree <> nil then
    Add(AKey, ATree.Write(FHelper));
end;

procedure TdxPDFWriterDictionary.Add(const AKey: string;
  AMask: TdxPDFCustomSoftMask);
begin
  AddNameOrReference(AKey, AMask);
end;

procedure TdxPDFWriterDictionary.Add(const AKey: string;
  AEncoding: TdxPDFCustomEncoding);
begin
  AddNameOrReference(AKey, AEncoding);
end;

procedure TdxPDFWriterDictionary.AddInline(const AKey: string;
  AObject: TdxPDFObject);
begin
  if AObject <> nil then
    Add(AKey, AObject.Write(FHelper));
end;

procedure TdxPDFWriterDictionary.AddNameOrReference(const AKey: string;
  AObject: TdxPDFObject);
var
  AData: TdxPDFBase;
begin
  if AObject <> nil then
  begin
    AData := FHelper.GetNameOrReference(AObject);
    if AData <> nil then
      Add(AKey, AData);
  end;
end;

procedure TdxPDFWriterDictionary.AddReference(const AKey: string;
  AData: TdxPDFBase);
begin
  if AData <> nil then
    AddReference(AKey, FHelper.CreateIndirectObject(AData));
end;

procedure TdxPDFWriterDictionary.AddReference(const AKey: string;
  AObject: TdxPDFObject);
begin
  if AObject <> nil then
    AddReference(AKey, FHelper.RegisterIndirectObject(AObject));
end;

procedure TdxPDFWriterDictionary.AddReference(const AKey: string;
  const AData: TBytes; ASkipIfNull: Boolean = True);
begin
  if not ASkipIfNull or (Length(AData) > 0) then
    AddReference(AKey, FHelper.CreateStream(AData));
end;

procedure TdxPDFWriterDictionary.SetAppearance(AResources: TdxPDFResources;
  ACommands: TdxPDFCommandList);
begin
  if ACommands <> nil then
    AddBytes(TdxPDFKeywords.DictionaryAppearance,
      ACommands.ToByteArray(AResources));
end;

procedure TdxPDFWriterDictionary.SetStreamData(const AData: TBytes);
begin
  SetStreamData(AData, True, True);
end;

procedure TdxPDFWriterDictionary.SetStreamData(const AData: TBytes;
  ACanCompress, ACanEncrypt: Boolean);
begin
{$IFNDEF DXPDF_DONT_COMPRESS_STREAMS}
  if Contains(TdxPDFKeywords.Filter) then
{$ENDIF}
    ACanCompress := False;

  if ACanCompress then
  begin
    FStreamData := TdxPDFFlateEncoder.Encode(AData);
    if Length(FStreamData) > 0 then
      AddName(TdxPDFKeywords.Filter, TdxPDFKeywords.FlateDecode);
  end
  else
    FStreamData := AData;

  Add(TdxPDFKeywords.Length, Length(FStreamData));
  FStreamDataCanEncrypt := ACanEncrypt;
end;

procedure TdxPDFWriterDictionary.SetTextJustification
  (AValue: TdxPDFTextJustification);
begin
  Add(TdxPDFKeywords.TextJustification, Integer(AValue));
end;

procedure TdxPDFWriterDictionary.Write(AWriter: TdxPDFWriter);
begin
  if FStreamDataCanEncrypt then
  begin
    FStreamData := AWriter.Encrypt(FStreamData);
    Add(TdxPDFKeywords.Length, Length(FStreamData));
  end;
  inherited Write(AWriter);
end;

procedure TdxPDFWriterDictionary.WriteStream(AWriter: TdxPDFWriter);
begin
  if StreamRef <> nil then
    TdxPDFUtils.RaiseTestException('StreamRef <> nil');
  WriteStreamData(AWriter, FStreamData);
end;

{ TdxPDFWriterHelper }

constructor TdxPDFWriterHelper.Create(AGetObjectNumberFunc
  : TdxPDFWriterGetObjectNumber;
  const AEncryptionInfo: IdxPDFEncryptionInfo = nil);
begin
  inherited Create;
  FEncryptionInfo := AEncryptionInfo;
  FTemporaryObjects := TcxObjectList.Create;
  FGetObjectNumberFunc := AGetObjectNumberFunc;
end;

destructor TdxPDFWriterHelper.Destroy;
begin
  FreeAndNil(FTemporaryObjects);
  inherited Destroy;
end;

function TdxPDFWriterHelper.CreateArray: TdxPDFWriterArray;
begin
  Result := TdxPDFWriterArray.Create(Self);
end;

function TdxPDFWriterHelper.CreateDictionary: TdxPDFWriterDictionary;
begin
  Result := TdxPDFWriterDictionary.Create(Self);
end;

function TdxPDFWriterHelper.CreateIndirectObject(AData: TdxPDFBase)
  : TdxPDFObject;
begin
  Result := TdxPDFWriterDataObject.Create(AData);
  FTemporaryObjects.Add(Result);
end;

function TdxPDFWriterHelper.CreateStream(const AData: TBytes): TdxPDFObject;
begin
  Result := CreateStream(CreateDictionary, AData);
end;

function TdxPDFWriterHelper.CreateStream(ADictionary: TdxPDFWriterDictionary;
  const AData: TBytes): TdxPDFObject;
begin
  ADictionary.SetStreamData(AData);
  Result := CreateIndirectObject(ADictionary);
end;

function TdxPDFWriterHelper.GetNameOrReference(AObject: TdxPDFObject)
  : TdxPDFBase;
begin
  Result := AObject.Write(Self);
  if (Result <> nil) and (Result.ObjectType <> otName) then
    Result := TdxPDFReference.Create
      (RegisterIndirectObject(CreateIndirectObject(Result)), 0);
end;

function TdxPDFWriterHelper.PrepareToWrite(AColorSpace: TdxPDFCustomColorSpace)
  : TdxPDFBase;
var
  AInlineData: TdxPDFBase;
begin
  if AColorSpace = nil then
    Exit(nil);

  AInlineData := AColorSpace.Write(Self);
  try
    if (AInlineData is TdxPDFArray) and (TdxPDFArray(AInlineData).Count = 1)
    then
    begin
      Result := TdxPDFArray(AInlineData).Elements[0];
      TdxPDFArray(AInlineData).ElementList.Extract(Result);
      TdxPDFBaseAccess(Result).ResetReferenceCount;
    end
    else
      Result := TdxPDFReference.Create(RegisterIndirectObject(AColorSpace), 0);
  finally
    AInlineData.Free;
  end;
end;

function TdxPDFWriterHelper.RegisterIndirectObject
  (AObject: TdxPDFObject): Integer;
begin
  if AObject <> nil then
    Result := FGetObjectNumberFunc(AObject)
  else
    Result := -1;
end;

function TdxPDFWriterHelper.GetEncryptMetadata: Boolean;
begin
  Result := (FEncryptionInfo <> nil) and FEncryptionInfo.EncryptMetadata;
end;

{ TdxPDFObject }

constructor TdxPDFObject.Create;
begin
  inherited Create;
  CreateSubClasses;
  Initialize;
end;

constructor TdxPDFObject.Create(AParent: TObject);
begin
  FParent := AParent;
  Create;
end;

destructor TdxPDFObject.Destroy;
begin
  DestroySubClasses;
  inherited Destroy;
end;

class function TdxPDFObject.GetTypeName: string;
begin
  Result := '';
end;

function TdxPDFObject.GetObject(const AName: string;
  ASourceDictionary: TdxPDFDictionary; out AObject: TdxPDFBase): Boolean;
begin
  AObject := ASourceDictionary.GetObject(AName);
  Result := AObject <> nil;
  if Result and TdxPDFUtils.IsIntegerValid((AObject as TdxPDFBase).Number) then
    AObject := Repository.GetObject((AObject as TdxPDFBase).Number)
      as TdxPDFBase;
end;

function TdxPDFObject.GetRepository: TdxPDFDocumentRepository;
begin
  Result := FRepository;
end;

function TdxPDFObject.Write(AHelper: TdxPDFWriterHelper): TdxPDFBase;
var
  ADictionary: TdxPDFWriterDictionary;
begin
  ADictionary := AHelper.CreateDictionary;
  Write(AHelper, ADictionary);
  Result := ADictionary;
end;

procedure TdxPDFObject.CreateSubClasses;
begin
  Repository := nil;
end;

procedure TdxPDFObject.DestroySubClasses;
begin
  Repository := nil;
end;

procedure TdxPDFObject.Initialize;
begin
  // do nothing
end;

procedure TdxPDFObject.Read(ADictionary: TdxPDFReaderDictionary);
begin
  if ADictionary <> nil then
  begin
    Repository := ADictionary.Repository;
    Number := ADictionary.Number;
    ReadProperties(ADictionary);
  end;
end;

procedure TdxPDFObject.ReadProperties(ADictionary: TdxPDFReaderDictionary);
begin
  // do nothing
end;

procedure TdxPDFObject.SetParent(const AValue: TObject);
begin
  FParent := AValue;
end;

procedure TdxPDFObject.SetRepository(const AValue: TdxPDFDocumentRepository);
begin
  FRepository := AValue;
end;

procedure TdxPDFObject.Write(AHelper: TdxPDFWriterHelper;
  ADictionary: TdxPDFWriterDictionary);
begin
  ADictionary.AddName(TdxPDFKeywords.TypeKey, GetTypeName);
end;

procedure TdxPDFObject.RaiseWriteNotImplementedException;
begin
  raise EdxPDFException.Create
    (ClassName + '.Write is not supported. Use overload method instead');
end;

{ TdxPDFPageData }

class function TdxPDFPageData.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Page;
end;

procedure TdxPDFPageData.ClearCommands;
begin
  FContents.ClearCommands;
end;

procedure TdxPDFPageData.ExtractCommands;
begin
  if FContents.CommandCount = 0 then
    FContents.PopulateCommands(Resources);
end;

procedure TdxPDFPageData.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FAnnotations := TdxPDFReferencedObjects.Create;
  FContents := TdxPDFPageContents.Create(Self);
end;

procedure TdxPDFPageData.DestroySubClasses;
begin
  Contents := nil;
  FreeAndNil(FTransparencyGroup);
  FreeAndNil(FAnnotations);
  inherited DestroySubClasses;
end;

procedure TdxPDFPageData.Initialize;
begin
  inherited Initialize;
  FAnnotationsLoaded := False;
end;

procedure TdxPDFPageData.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  FDictionary := ADictionary;
  Contents.Read(ADictionary);
  ReadGroup(FDictionary.GetDictionary(TdxPDFKeywords.Group));
end;

procedure TdxPDFPageData.Write(AHelper: TdxPDFWriterHelper;
  ADictionary: TdxPDFWriterDictionary);
begin
  ExtractCommands;
  inherited;
  ADictionary.AddReference(TdxPDFKeywords.Contents, Contents);
  ADictionary.AddReference(TdxPDFKeywords.Group, TransparencyGroup);
  WriteAnnotations(AHelper, ADictionary);
end;

function TdxPDFPageData.GetAnnotations: TdxPDFReferencedObjects;
begin
  if not FAnnotationsLoaded then
  begin
    FAnnotationsLoaded := True;
    ReadAnnotations;
  end;
  Result := FAnnotations;
end;

function TdxPDFPageData.GetCommands: TdxPDFCommandList;
begin
  Result := Contents.Commands;
end;

function TdxPDFPageData.GetPage: TdxPDFPage;
begin
  Result := Parent as TdxPDFPage;
end;

function TdxPDFPageData.GetResources: TdxPDFResources;
begin
  Result := GetPage.Resources;
end;

procedure TdxPDFPageData.SetContents(const AValue: TdxPDFPageContents);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FContents));
end;

procedure TdxPDFPageData.ReadAnnotations;
var
  I: Integer;
  AAnnotation: TdxPDFCustomAnnotation;
  AArray: TdxPDFArray;
  AObject: TdxPDFBase;
begin
  if FDictionary <> nil then
  begin
    AArray := FDictionary.GetArray(TdxPDFKeywords.Annotations);
    if AArray <> nil then
    begin
      FAnnotations.Capacity := FAnnotations.Count + AArray.Count;
      for AObject in AArray.ElementList do
      begin
        AAnnotation := FDictionary.Repository.GetAnnotation(AObject, GetPage);
        if AAnnotation <> nil then
          FAnnotations.Add(AAnnotation as TdxPDFCustomAnnotation);
      end;
      for I := 0 to FAnnotations.Count - 1 do
        TdxPDFCustomAnnotation(FAnnotations[I]).Ensure;
    end;
  end;
end;

procedure TdxPDFPageData.ReadGroup(ADictionary: TdxPDFReaderDictionary);
begin
  if ADictionary <> nil then
  begin
    FTransparencyGroup := TdxPDFTransparencyGroup.Create(nil);
    FTransparencyGroup.Read(ADictionary);
  end;
end;

procedure TdxPDFPageData.WriteAnnotations(AHelper: TdxPDFWriterHelper;
  ADictionary: TdxPDFWriterDictionary);
var
  AAnnotation: TdxPDFCustomAnnotation;
  AAnnotationArray: TdxPDFWriterArray;
  AAnnotations: TdxPDFReferencedObjects;
  I: Integer;
begin
  AAnnotations := Annotations;
  if AAnnotations.Count > 0 then
  begin
    AAnnotationArray := AHelper.CreateArray;
    for I := 0 to AAnnotations.Count - 1 do
    begin
      AAnnotation := AAnnotations[I] as TdxPDFCustomAnnotation;
      if AAnnotation is TdxPDFWidgetAnnotation then
        AAnnotationArray.AddReference(TdxPDFWidgetAnnotation(AAnnotation)
          .InteractiveFormField)
      else
        AAnnotationArray.AddReference(AAnnotation);
    end;
    ADictionary.Add(TdxPDFKeywords.Annotations, AAnnotationArray);
  end;
end;

{ TdxPDFPage }

class function TdxPDFPage.GetTypeName: string;
begin
  Result := 'FakePage';
end;

constructor TdxPDFPage.Create(AOwner: TdxPDFObject;
  AInfo: TdxPDFDeferredObjectInfo);
begin
  inherited Create(AOwner);
  FDeferredData := TdxPDFDeferredObject.Create(Self, AInfo);
end;

constructor TdxPDFPage.Create(APages: TdxPDFPages;
  AMediaBox, ACropBox: TdxRectF; ARotationAngle: Integer);
begin
  inherited Create(APages);
  FMediaBox := AMediaBox;
  FCropBox := ACropBox;
  FRotationAngle := ARotationAngle;
  FDeferredData := TdxPDFDeferredObject.Create(Self,
    TdxPDFPageData.Create(Self));
end;

destructor TdxPDFPage.Destroy;
begin
  Pack;
  FreeAndNil(FDeferredData);
  inherited Destroy;
end;

function TdxPDFPage.CalculateRotationAngle(ARotationAngle
  : TcxRotationAngle): Integer;
begin
  Result := TdxPDFUtils.NormalizeRotate
    (RotationAngle - TdxPDFUtils.ConvertToIntEx(ARotationAngle));
end;

function TdxPDFPage.Find(const APosition: TdxPDFPosition;
  const AScaleFactor: TdxPointF;
  ARecognitionObjects: TdxPDFRecognitionObjects = dxPDFAllRecognitionObjects)
  : TdxPDFRecognizedObject;
var
  AField: TdxPDFAcroFormField;
  AResult: TdxPDFRecognizedObject;
  ADocumentPosition: TdxPDFPosition;
  ADocumentScaleFactor: TdxPointF;
begin
  ADocumentPosition := APosition;
  ADocumentScaleFactor := AScaleFactor;
  LockAndExecute(procedure
    var
      ALine: TdxPDFTextLine;
    begin
      if FindInteractiveObject(ADocumentPosition.Point, ADocumentScaleFactor,
        AField, ARecognitionObjects) then
        AResult := AField
      else if FindLine(ADocumentPosition, ADocumentScaleFactor, ALine,
        ARecognitionObjects) then
        AResult := ALine
      else
        AResult := FindImage(ADocumentPosition.Point, ARecognitionObjects);
    end, True);
  Result := AResult;
end;

function TdxPDFPage.FindHyperlink(const P: TdxPointF;
const AScaleFactor: TdxPointF; out AHyperlink: TdxPDFHyperlink;
ARecognitionObjects: TdxPDFRecognitionObjects =
  dxPDFAllRecognitionObjects): Boolean;
var
  ADocumentHyperlink: TdxPDFHyperlink;
  ADocumentPosition: TdxPointF;
  ADocumentScaleFactor: TdxPointF;
begin
  ADocumentPosition := P;
  ADocumentScaleFactor := AScaleFactor;
  LockAndExecute(procedure
    var
      ALink: TdxPDFHyperlink;
    begin
      ADocumentHyperlink := nil;
      for ALink in GetRecognizedContent(ARecognitionObjects).Hyperlinks do
        if ALink.Bounds.PtInRect(ADocumentPosition) then
        begin
          ADocumentHyperlink := ALink;
          Break;
        end;
    end, True);
  AHyperlink := ADocumentHyperlink;
  Result := AHyperlink <> nil;
end;

function TdxPDFPage.FindImage(const P: TdxPointF;
ARecognitionObjects: TdxPDFRecognitionObjects = dxPDFAllRecognitionObjects)
  : TdxPDFImage;
var
  AImage: TdxPDFImage;
begin
  Result := nil;
  for AImage in GetRecognizedContent(ARecognitionObjects).Images do
    if PtInRect(cxRect(TdxPDFImageAccess(AImage).Bounds), cxPoint(P)) then
      Exit(AImage);
end;

function TdxPDFPage.FindInteractiveObject(const P: TdxPointF;
const AScaleFactor: TdxPointF; out AField: TdxPDFAcroFormField;
ARecognitionObjects: TdxPDFRecognitionObjects =
  dxPDFAllRecognitionObjects): Boolean;
var
  AHyperlink: TdxPDFHyperlink;
  AFormField: TdxPDFAcroFormField;
  ADocumentPosition: TdxPointF;
  ADocumentScaleFactor: TdxPointF;
  ARecognizedContent: TdxPDFRecognizedContent;
begin
  Result := FindHyperlink(P, AScaleFactor, AHyperlink, ARecognitionObjects);
  if not Result then
  begin
    ADocumentPosition := P;
    ADocumentScaleFactor := AScaleFactor;
    LockAndExecute(procedure
      var
        F: TdxPDFAcroFormField;
      begin
        AFormField := nil;
        ARecognizedContent := GetRecognizedContent(ARecognitionObjects);
        for F in ARecognizedContent.AcroFormFields do
          if TryGetAcroForm(F, ADocumentPosition, AFormField) then
            Exit;
        for F in ARecognizedContent.AnnotationFields do
          if TryGetAcroForm(F, ADocumentPosition, AFormField) then
            Exit;
      end, True);
    AField := AFormField;
    Result := AField <> nil;
  end
  else
    AField := AHyperlink;
end;

function TdxPDFPage.FindLine(const APosition: TdxPDFPosition;
const AScaleFactor: TdxPointF; out ALine: TdxPDFTextLine;
ARecognitionObjects: TdxPDFRecognitionObjects =
  dxPDFAllRecognitionObjects): Boolean;
begin
  Result := GetRecognizedContent(ARecognitionObjects).TextLines.Find(APosition,
    GetTextExpansionFactor(AScaleFactor), ALine);
end;

function TdxPDFPage.FindStartTextPosition(const APosition: TdxPDFPosition;
const AScaleFactor: TdxPointF;
ARecognitionObjects: TdxPDFRecognitionObjects = dxPDFAllRecognitionObjects)
  : TdxPDFTextPosition;
var
  ALine: TdxPDFTextLine;
begin
  if FindLine(APosition, AScaleFactor, ALine, ARecognitionObjects) then
    Result := TdxPDFTextLineAccess(ALine).GetPosition(APosition.PageIndex,
      APosition.Point)
  else
    Result := TdxPDFTextPosition.Invalid;
end;

function TdxPDFPage.GetTopLeft(AAngle: TcxRotationAngle): TdxPointF;
var
  ACropBox: TdxRectF;
begin
  ACropBox := CropBox;
  case CalculateRotationAngle(AAngle) of
    90:
      Result := dxPointF(ACropBox.Left, ACropBox.Bottom);
    180:
      Result := ACropBox.BottomRight;
    270:
      Result := dxPointF(ACropBox.Right, ACropBox.Top);
  else
    Result := ACropBox.TopLeft;
  end;
end;

function TdxPDFPage.FromUserSpace(const P, ADPI, AScaleFactor: TdxPointF;
const ABounds: TdxRectF; AAngle: TcxRotationAngle): TdxPointF;

  function Convert(const P, ADPI: TdxPointF; AAngle: TcxRotationAngle)
    : TdxPointF;
  var
    AUserScapeFactor: TdxPointF;
  begin
    AUserScapeFactor := UserSpaceFactor(ADPI);
    case CalculateRotationAngle(AAngle) of
      90:
        begin
          Result.X := P.Y / AUserScapeFactor.Y;
          Result.Y := P.X / AUserScapeFactor.X;
        end;
      180:
        begin
          Result.X := Abs(Bounds.Width) - P.X / AUserScapeFactor.X;
          Result.Y := P.Y / AUserScapeFactor.Y;
        end;
      270:
        begin
          Result.X := Abs(Bounds.Width) - P.Y / AUserScapeFactor.Y;
          Result.Y := Abs(Bounds.Height) - P.X / AUserScapeFactor.X;
        end;
    else
      Result.X := P.X / AUserScapeFactor.X;
      Result.Y := Abs(Bounds.Height) - P.Y / AUserScapeFactor.Y;
    end;
  end;

var
  AMousePoint, AUserScapeFactor: TdxPointF;
begin
  AMousePoint := P;
  AUserScapeFactor := UserSpaceFactor(ADPI);
  AMousePoint.X := (AMousePoint.X - ABounds.Left) / (AScaleFactor.X);
  AMousePoint.Y := (AMousePoint.Y - ABounds.Top) / (AScaleFactor.Y);
  Result := Convert(AMousePoint, ADPI, AAngle);
end;

function TdxPDFPage.FromUserSpace(const R: TdxRectF;
ADPI, AScaleFactor: TdxPointF; const ABounds: TdxRectF;
AAngle: TcxRotationAngle): TdxRectF;
begin
  Result.TopLeft := FromUserSpace(R.TopLeft, ADPI, AScaleFactor,
    ABounds, AAngle);
  Result.BottomRight := FromUserSpace(R.BottomRight, ADPI, AScaleFactor,
    ABounds, AAngle);
end;

function TdxPDFPage.ToUserSpace(const R: TdxRectF;
const ADPI, AScaleFactor: TdxPointF; const ABounds: TdxRectF;
AAngle: TcxRotationAngle): TdxRectF;
begin
  Result.TopLeft := ToUserSpace(R.TopLeft, ADPI, AScaleFactor, ABounds, AAngle);
  Result.BottomRight := ToUserSpace(R.BottomRight, ADPI, AScaleFactor,
    ABounds, AAngle);
  Result := cxRectAdjustF(Result);
end;

procedure TdxPDFPage.Pack;
begin
  PackData;
  PackRecognizedContent;
end;

procedure TdxPDFPage.PackRecognizedContent(AForce: Boolean = False);
begin
  if AForce then
    UnLockRecognizedContent(AForce);
  LockAndExecute(procedure
    begin
      if not IsRecognizedContentLocked then
        FreeAndNil(FRecognizedContent);
    end);
end;

function TdxPDFPage.ToUserSpace(const P, ADPI, AScaleFactor: TdxPointF;
const ABounds: TdxRectF; AAngle: TcxRotationAngle): TdxPointF;
var
  ARealScaleFactor, AUserScapeFactor: TdxPointF;
begin
  AUserScapeFactor := UserSpaceFactor(ADPI);
  ARealScaleFactor.X := AUserScapeFactor.X * AScaleFactor.X;
  ARealScaleFactor.Y := AUserScapeFactor.Y * AScaleFactor.Y;
  case CalculateRotationAngle(AAngle) of
    90:
      begin
        Result.X := P.Y * ARealScaleFactor.X;
        Result.Y := P.X * ARealScaleFactor.Y;
      end;
    180:
      begin
        Result.X := (Abs(Bounds.Width) - P.X) * ARealScaleFactor.X;
        Result.Y := P.Y * ARealScaleFactor.Y;
      end;
    270:
      begin
        Result.X := (Abs(Bounds.Height) - P.Y) * ARealScaleFactor.X;
        Result.Y := (Abs(Bounds.Width) - P.X) * ARealScaleFactor.Y;
      end;
  else
    Result.X := P.X * ARealScaleFactor.X;
    Result.Y := (Abs(Bounds.Height) - P.Y) * ARealScaleFactor.Y;
  end;

  Result := cxPointOffset(Result, ABounds.TopLeft);
end;

procedure TdxPDFPage.CreateSubClasses;
begin
  inherited CreateSubClasses;
  InitializeCriticalSectionAndSpinCount(FLock, 1024);
end;

procedure TdxPDFPage.DestroySubClasses;
begin
  ThumbnailImage := nil;
  DeleteCriticalSection(FLock);
  inherited DestroySubClasses;
end;

procedure TdxPDFPage.Initialize;
begin
  inherited Initialize;
  FDisplayDuration := -1;
end;

procedure TdxPDFPage.ReadProperties(ADictionary: TdxPDFReaderDictionary);
begin
  inherited ReadProperties(ADictionary);
  FLastModified := ADictionary.GetDate(TdxPDFKeywords.LastModified);
  FDisplayDuration := ADictionary.GetDouble(TdxPDFKeywords.DisplayDuration, -1);
  FStructParents := ADictionary.GetInteger(TdxPDFKeywords.StructParents);
  FPreferredZoomFactor := ADictionary.GetInteger(TdxPDFKeywords.PreferredZoom);
  FID := ADictionary.GetBytes(TdxPDFKeywords.ID);
end;

procedure TdxPDFPage.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
var
  ACropBox: TdxRectF;
begin
  inherited;
  ACropBox := CropBox;
  ADictionary.AddDate(TdxPDFKeywords.LastModified, FLastModified);
  ADictionary.Add(TdxPDFKeywords.BleedBox, BleedBox, ACropBox);
  ADictionary.Add(TdxPDFKeywords.TrimBox, TrimBox, ACropBox);
  ADictionary.Add(TdxPDFKeywords.ArtBox, ArtBox, ACropBox);
  ADictionary.Add(TdxPDFKeywords.DisplayDuration, FDisplayDuration, -1);
  ADictionary.AddReference(TdxPDFKeywords.Resources, Resources);
  ADictionary.Add(TdxPDFKeywords.StructParents, FStructParents, 0);
  ADictionary.Add(TdxPDFKeywords.PreferredZoom, FPreferredZoomFactor);
  ADictionary.Add(TdxPDFKeywords.ID, FID);
  ADictionary.Add(TdxPDFKeywords.UserUnit, UserUnit, 1);
  Data.Write(AHelper, ADictionary);
end;

function TdxPDFPage.IsRecognizedContentLocked: Boolean;
begin
  Result := FRecognizedContentLockCount > 0;
end;

procedure TdxPDFPage.LockRecognizedContent;
begin
  Inc(FRecognizedContentLockCount);
end;

procedure TdxPDFPage.UnLockRecognizedContent(AReset: Boolean = False);
begin
  if AReset then
    FRecognizedContentLockCount := 0
  else
    Dec(FRecognizedContentLockCount);
end;

function TdxPDFPage.ScaleFactor(const ADPI, AScaleFactor: TdxPointF): TdxPointF;
var
  ASpaceFactor: TdxPointF;
begin
  ASpaceFactor := UserSpaceFactor(ADPI);
  Result.X := ASpaceFactor.X * AScaleFactor.X;
  Result.Y := ASpaceFactor.Y * AScaleFactor.Y;
end;

function TdxPDFPage.UserSpaceFactor(const ADPI: TdxPointF): TdxPointF;
begin
  Result := dxPointF(ADPI.X / 72 * UserUnit, ADPI.Y / 72 * UserUnit);
end;

procedure TdxPDFPage.AddAnnotation(AAnnotation: TdxPDFCustomAnnotation);
begin
  Data.Annotations.Add(AAnnotation);
end;

procedure TdxPDFPage.Export(ADevice: TObject;
AParameters: TdxPDFExportParameters);
begin
  LockAndExecute(procedure
    begin
      (ADevice as TdxPDFCustomCommandInterpreter).ExportAndPack(Self,
        AParameters);
    end);
end;

procedure TdxPDFPage.Export(AParameters: TdxPDFExportParameters;
AStream: TStream);
var
  ABitmap: TcxBitmap;
  ADevice: TdxPDFGraphicsDevice;
  ARenderParameters: TdxPDFRenderParameters;
begin
  ARenderParameters := AParameters as TdxPDFRenderParameters;
  if not ARenderParameters.IsCanceled then
  begin
    ABitmap := TcxBitmap.CreateSize(ARenderParameters.Rect);
    try
      ABitmap.PixelFormat := pf24bit;
      ARenderParameters.Canvas := ABitmap.Canvas;
      ARenderParameters.Canvas.Lock;
      ADevice := TdxPDFGraphicsDevice.Create;
      try
        ADevice.Export(Self, ARenderParameters);
        ABitmap.SaveToStream(AStream);
      finally
        ARenderParameters.Canvas.Unlock;
        ADevice.Free;
      end;
    finally
      ABitmap.Free;
    end;
  end;
end;

procedure TdxPDFPage.LockAndExecute(AProc: TProc; ATryLock: Boolean = False);
var
  ALocked: Boolean;
begin
  ALocked := True;
  if ATryLock then
    ALocked := TryEnterCriticalSection(FLock)
  else
    EnterCriticalSection(FLock);
  if ALocked then
    try
      Locked := True;
      AProc;
    finally
      LeaveCriticalSection(FLock);
      Locked := False;
    end;
end;

procedure TdxPDFPage.PopulateAttachmentList(AList: TdxPDFFileAttachmentList);
begin
  LockAndExecute(procedure
    var
      AContent: TdxPDFRecognizedContent;
    begin
      AContent := TdxPDFRecognizedContent.Create;
      try
        RecognizeContent(AContent, [rmAnnotations]);
        AList.AddRange(AContent.Attachments);
      finally
        AContent.Free;
      end;
    end, True);
end;

function TdxPDFPage.GetBounds: TdxRectF;
begin
  if TdxPDFUtils.IsRectEmpty(CropBox) then
    Result := MediaBox
  else
    Result := CropBox;
end;

function TdxPDFPage.GetData: TdxPDFPageData;
begin
  Result := FDeferredData.ResolvedObject as TdxPDFPageData
end;

function TdxPDFPage.GetDocumentState: TdxPDFDocumentState;
begin
  Result := TdxPDFDocumentAccess
    (Repository.Catalog.Parent as TdxPDFDocument).State;
end;

function TdxPDFPage.GetNormalizedRotationAngle: Integer;
begin
  Result := TdxPDFUtils.NormalizeRotate(RotationAngle);
end;

function TdxPDFPage.GetRecognizedContent: TdxPDFRecognizedContent;
begin
  Result := GetRecognizedContent(dxPDFAllRecognitionObjects);
end;

function TdxPDFPage.GetRecognizedContent(ARecognitionObjects
  : TdxPDFRecognitionObjects): TdxPDFRecognizedContent;
begin
  if FRecognizedContent = nil then
  begin
    FRecognizedContent := TdxPDFRecognizedContent.Create;
    RecognizeContent(FRecognizedContent, ARecognitionObjects);
  end;
  Result := FRecognizedContent;
end;

function TdxPDFPage.GetSize: TdxPointF;
var
  ABounds: TdxRectF;
begin
  ABounds := Bounds;
  if (NormalizedRotationAngle = 90) or (NormalizedRotationAngle = 270) then
    Result := dxPointF(Abs(ABounds.Height), Abs(ABounds.Width))
  else
    Result := dxPointF(Abs(ABounds.Width), Abs(ABounds.Height));
end;

procedure TdxPDFPage.SetData(const AValue: TdxPDFPageData);
begin
  FDeferredData.ResolvedObject := nil;
end;

procedure TdxPDFPage.SetThumbnailImage(const AValue: TdxPDFDocumentImage);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FThumbnailImage));
end;

function TdxPDFPage.GetTextExpansionFactor(const AScaleFactor: TdxPointF)
  : TdxPointF;
begin
  Result.X := 15 / AScaleFactor.X;
  Result.Y := 5 / AScaleFactor.Y;
end;

function TdxPDFPage.TryGetAcroForm(AField: TdxPDFAcroFormField;
const APosition: TdxPointF; out AResult: TdxPDFAcroFormField): Boolean;
begin
  Result := Supports(AField, IdxPDFInteractiveObject) and
    AField.Bounds.PtInRect(APosition);
  if Result then
    AResult := AField
  else
    AResult := nil;
end;

procedure TdxPDFPage.PackData;
begin
  LockAndExecute(procedure
    begin
      if FDeferredData.IsResolved then
        Data := nil;
      if Resources <> nil then
        Resources.Pack;
    end);
end;

procedure TdxPDFPage.RecognizeContent(AContent: TdxPDFRecognizedContent;
ARecognitionObjects: TdxPDFRecognitionObjects);

  function CreateRenderParameters: TdxPDFRenderParameters;
  begin
    Result := TdxPDFRenderParameters.Create(GetDocumentState);
    Result.Angle := ra0;
    Result.ScaleFactor := cxGetCurrentDPI / 72;
  end;

  function CreateRecognizer: TdxPDFDocumentContentRecognizer;
  begin
    if ARecognitionObjects = [rmAnnotations] then
      Result := TdxPDFAttachmentUnpacker.Create(AContent, ARecognitionObjects)
    else
      Result := TdxPDFDocumentContentRecognizer.Create(AContent,
        ARecognitionObjects);
  end;

var
  AParameters: TdxPDFRenderParameters;
  ARecognizer: TdxPDFDocumentContentRecognizer;
begin
  if AContent <> nil then
  begin
    AParameters := CreateRenderParameters;
    ARecognizer := CreateRecognizer;
    try
      LockAndExecute(procedure
        begin
          ARecognizer.ExportAndPack(Self, AParameters);
        end, True);
    finally
      ARecognizer.Free;
      AParameters.Free;
    end;
  end;
end;

{ TdxPDFInteractiveFormFieldTextState }

procedure TdxPDFInteractiveFormFieldTextState.DestroySubClasses;
begin
  inherited DestroySubClasses;
end;

procedure TdxPDFInteractiveFormFieldTextState.Initialize;
begin
  inherited Initialize;
  FHorizontalScaling := 100;
end;

constructor TdxPDFInteractiveFormFieldTextState.Create
  (AField: TdxPDFInteractiveFormField);
var
  AAppearance: TdxPDFAnnotationAppearances;
  AWidgetCommands: TdxPDFReferencedObjects;
begin
  inherited Create(nil);
  ConvertCommandsToBytes(AField);
  if (FFontCommand = nil) or (TdxPDFSetTextFontCommand(FFontCommand).Font = nil)
  then
  begin
    if AField.Widget = nil then
      AAppearance := nil
    else
      AAppearance := AField.Widget.Appearance;
    if (AAppearance = nil) or (AAppearance.Normal = nil) or
      (AAppearance.Normal.DefaultForm = nil) then
      AWidgetCommands := nil
    else
      AWidgetCommands := AAppearance.Normal.DefaultForm.Commands;
    if AWidgetCommands <> nil then
      FFontCommand := FindSetTextFontCommand(AWidgetCommands);
  end;
end;

function TdxPDFInteractiveFormFieldTextState.GetFontSize: Double;
begin
  if FFontCommand = nil then
    Result := DefaultFontSize
  else
    Result := (FFontCommand as TdxPDFSetTextFontCommand).FontSize;
end;

function TdxPDFInteractiveFormFieldTextState.FindSetTextFontCommand
  (ACommands: TdxPDFReferencedObjects): TdxPDFCustomCommand;
var
  ACommandObject: TdxPDFReferencedObject;
  AFontCommand: TdxPDFCustomCommand;
begin
  Result := nil;
  for ACommandObject in ACommands do
    if ACommandObject is TdxPDFSetTextFontCommand then
      Exit(TdxPDFSetTextFontCommand(ACommandObject))
    else if ACommandObject is TdxPDFMarkedContentCommand then
    begin
      AFontCommand := FindSetTextFontCommand
        (TdxPDFMarkedContentCommand(ACommandObject).Commands);
      if AFontCommand <> nil then
        Exit(AFontCommand);
    end;
end;

procedure TdxPDFInteractiveFormFieldTextState.ConvertCommandsToBytes
  (AField: TdxPDFInteractiveFormField);
var
  AAppearanceCommand: TdxPDFCustomCommand;
  ACommands: TdxPDFCommandList;
  ACommandsToFillList: TdxPDFCommandList;
  I: Integer;
begin
  ACommands := GetAppearanceCommandsInheritable(AField);
  if ACommands <> nil then
  begin
    ACommandsToFillList := TdxPDFCommandList.Create;
    try
      for I := 0 to ACommands.Count - 1 do
      begin
        AAppearanceCommand := ACommands[I];
        if AAppearanceCommand is TdxPDFSetWordSpacingCommand then
          FWordSpacing := TdxPDFSetWordSpacingCommand
            (AAppearanceCommand).Spacing
        else if AAppearanceCommand is TdxPDFSetCharacterSpacingCommand then
          FCharacterSpacing := TdxPDFSetCharacterSpacingCommand
            (AAppearanceCommand).Spacing
        else if AAppearanceCommand is TdxPDFSetTextHorizontalScalingCommand then
          FHorizontalScaling := TdxPDFSetTextHorizontalScalingCommand
            (AAppearanceCommand).HorizontalScaling
        else if AAppearanceCommand is TdxPDFSetTextFontCommand then
          FFontCommand := TdxPDFSetTextFontCommand(AAppearanceCommand)
        else
          ACommandsToFillList.Add(AAppearanceCommand);
      end;
      FCommandsAsBytes := ACommandsToFillList.ToByteArray;
    finally
      ACommandsToFillList.Free;
    end;
  end;
end;

function TdxPDFInteractiveFormFieldTextState.GetAppearanceCommandsInheritable
  (AFormField: TdxPDFInteractiveFormField): TdxPDFCommandList;
begin
  if AFormField = nil then
    Result := nil
  else if AFormField.AppearanceCommands <> nil then
    Result := AFormField.AppearanceCommands
  else if (AFormField.Parent = nil) and (AFormField.Form <> nil) then
    Result := AFormField.Form.DefaultAppearanceCommands
  else
    Result := GetAppearanceCommandsInheritable(AFormField.Parent);
end;

{ TdxPDFInteractiveFormField }

class function TdxPDFInteractiveFormField.Parse
  (AParent: TdxPDFInteractiveFormField; ADictionary: TdxPDFReaderDictionary;
ANumber: Integer): TdxPDFInteractiveFormField;
var
  AClass: TdxPDFObjectClass;
  AType: string;
begin
  Result := nil;
  if ADictionary <> nil then
  begin
    AType := ADictionary.GetString(TdxPDFKeywords.FieldType);
    if (AType = '') and (AParent <> nil) then
      AType := AParent.GetTypeName;
    if AType = '' then
    begin
      if ADictionary.Contains(TdxPDFKeywords.Kids) then
        Result := TdxPDFInteractiveFormField.Create(nil);
    end
    else if dxPDFTryGetDocumentObjectClass(AType, AClass) then
      Result := AClass.Create(nil) as TdxPDFInteractiveFormField;
  end;
end;

constructor TdxPDFInteractiveFormField.Create(AForm: TdxPDFInteractiveForm;
AWidget: TdxPDFCustomAnnotation);
begin
  Create;
  FForm := AForm;
  FWidget := AWidget as TdxPDFWidgetAnnotation;
  TdxPDFWidgetAnnotation(FWidget).InteractiveFormField := Self;
  FForm.Fields.Add(Self);
end;

procedure TdxPDFInteractiveFormField.CreateSubClasses;
begin
  inherited CreateSubClasses;
  Resources := nil;
end;

procedure TdxPDFInteractiveFormField.DestroySubClasses;
begin
  Resources := nil;
  FWidget := nil;
  FParent := nil;
  FreeAndNil(FAppearanceCommands);
  FreeAndNil(FKids);
  FreeAndNil(FTextState);
  inherited DestroySubClasses;
end;

procedure TdxPDFInteractiveFormField.Initialize(AForm: TdxPDFInteractiveForm;
AParent: TdxPDFInteractiveFormField);
begin
  FForm := AForm;
  FParent := AParent;
end;

procedure TdxPDFInteractiveFormField.Read(ADictionary: TdxPDFReaderDictionary;
ANumber: Integer);
begin
  inherited Read(ADictionary);

  if ADictionary <> nil then
  begin
    if not FKidsResolved then
      ReadKids(ADictionary, ANumber);

    if (FName = '') and (FParent <> nil) then
      FValuesProvider := FParent
    else
      FValuesProvider := Self;

    if Form <> nil then
      Resources := Form.Resources
    else
      Resources := TdxPDFResources.Create(nil);

    FAppearanceCommands := ADictionary.GetAppearance(Resources);
  end;
end;

procedure TdxPDFInteractiveFormField.ReadProperties
  (ADictionary: TdxPDFReaderDictionary);
begin
  inherited ReadProperties(ADictionary);

  FName := ADictionary.GetTextString(TdxPDFKeywords.Text);
  FAlternateName := ADictionary.GetTextString(TdxPDFKeywords.TextAlternate);
  FMappingName := ADictionary.GetTextString(TdxPDFKeywords.TextMapping);
  if ADictionary.Contains(TdxPDFKeywords.FieldFlags) then
    FFlags := TdxPDFInteractiveFormFieldFlags
      (ADictionary.GetInteger(TdxPDFKeywords.FieldFlags));
  FTextJustification := ADictionary.GetTextJustification;
end;

procedure TdxPDFInteractiveFormField.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  ADictionary.AddName(TdxPDFKeywords.TypeKey, TdxPDFKeywords.Annotation);
  ADictionary.AddName(TdxPDFKeywords.FieldType, GetTypeName);
  ADictionary.AddReference(TdxPDFKeywords.Parent, Parent);
  WriteProperties(AHelper, ADictionary);
  WriteKids(AHelper, ADictionary);
  ADictionary.SetAppearance(Resources, AppearanceCommands);
end;

procedure TdxPDFInteractiveFormField.WriteProperties
  (AHelper: TdxPDFWriterHelper; ADictionary: TdxPDFWriterDictionary);
begin
  ADictionary.Add(TdxPDFKeywords.Text, Name);
  ADictionary.Add(TdxPDFKeywords.TextAlternate, FAlternateName);
  ADictionary.Add(TdxPDFKeywords.TextMapping, FMappingName);
  ADictionary.Add(TdxPDFKeywords.FieldFlags, Integer(FFlags));
  ADictionary.SetTextJustification(FTextJustification);
end;

function TdxPDFInteractiveFormField.CreateAppearanceBuilder
  (AState: TdxPDFDocumentState): TObject;
begin
  Result := nil;
end;

function TdxPDFInteractiveFormField.GetAcroFieldClass: TdxPDFAcroFormFieldClass;
begin
  Result := nil;
end;

function TdxPDFInteractiveFormField.GetDefaultValue: Variant;
begin
  Result := '';
end;

function TdxPDFInteractiveFormField.GetName: string;
begin
  Result := FName;
end;

procedure TdxPDFInteractiveFormField.InternalSetValue(const AValue: Variant;
AState: TdxPDFDocumentState);
begin
  // do nothing
end;

function TdxPDFInteractiveFormField.CreateAcroField(AState: TdxPDFDocumentState)
  : TdxPDFAcroFormField;
var
  AClass: TdxPDFAcroFormFieldClass;
begin
  AClass := GetAcroFieldClass;
  if AClass <> nil then
  begin
    Result := AClass.Create;
    Result.DocumentState := AState;
    Result.Field := Self;
  end
  else
    Result := nil;
end;

procedure TdxPDFInteractiveFormField.Changed(AState: TdxPDFDocumentState);
var
  ABuilder: TObject;
  AAppearanceForm: TdxPDFForm;
  AIntf: IdxPDFAnnotationAppearanceBuilder;
begin
  if (Form <> nil) and (Widget <> nil) then
  begin
    ABuilder := CreateAppearanceBuilder(AState);
    if Supports(ABuilder, IdxPDFAnnotationAppearanceBuilder, AIntf) then
    begin
      if FFormCreated then
        AAppearanceForm := Widget.GetAppearanceForm(AState)
      else
      begin
        AAppearanceForm := Widget.CreateAppearanceForm(Widget.AppearanceName);
        FFormCreated := True;
      end;
      AIntf.RebuildAppearance(AAppearanceForm);
      AIntf := nil;
    end
    else
      ABuilder.Free;
  end;
end;

function TdxPDFInteractiveFormField.GetFontInfo(AState: TdxPDFDocumentState)
  : TdxPDFFontInfo;
begin
  Result.FontSize := TextState.FontSize;
  Result.FontData := AState.SearchFontData(TextState.FontCommand)
    as TdxPDFEditableFontData;
end;

function TdxPDFInteractiveFormField.GetValue: Variant;
begin
  Result := '';
end;

function TdxPDFInteractiveFormField.HasFlag
  (AFlags: TdxPDFInteractiveFormFieldFlags): Boolean;
begin
  Result := (Integer(Flags) and Integer(AFlags)) <> 0;
end;

function TdxPDFInteractiveFormField.UseDefaultAppearanceForm: Boolean;
begin
  Result := True;
end;

procedure TdxPDFInteractiveFormField.SetValue(const AValue: Variant;
AState: TdxPDFDocumentState);
begin
  FValuesProvider.InternalSetValue(AValue, AState);
end;

function TdxPDFInteractiveFormField.GetFullName: string;
var
  AParentName: string;
begin
  Result := FName;
  if FParent <> nil then
  begin
    AParentName := Parent.FullName;
    if AParentName = '' then
      Result := FName
    else
      Result := Result + AParentName;

    if FName <> '' then
      Result := Result + '.' + FName;
  end;
end;

function TdxPDFInteractiveFormField.GetTextState
  : TdxPDFInteractiveFormFieldTextState;
begin
  if FTextState = nil then
    FTextState := TdxPDFInteractiveFormFieldTextState.Create(Self);
  Result := FTextState;
end;

procedure TdxPDFInteractiveFormField.SetResources(const AValue
  : TdxPDFResources);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FResources));
end;

procedure TdxPDFInteractiveFormField.ReadKids(ADictionary
  : TdxPDFReaderDictionary; AWidgetNumber: Integer);

  function GetPage(ADictionary: TdxPDFReaderDictionary): TdxPDFPage;
  var
    ANumber: Integer;
  begin
    if ADictionary.TryGetReference('P', ANumber) then
      Result := Repository.GetPage(ANumber)
    else
      Result := nil;
  end;

var
  AKidArray: TdxPDFArray;
  AWidget: TdxPDFWidgetAnnotation;
begin
  AKidArray := ADictionary.GetArray(TdxPDFKeywords.Kids);
  if AKidArray = nil then
  begin
    AWidget := Repository.GetWidget(AWidgetNumber) as TdxPDFWidgetAnnotation;
    if AWidget = nil then
      AWidget := Repository.GetAnnotation(AWidgetNumber, GetPage(ADictionary))
        as TdxPDFWidgetAnnotation;
    if AWidget <> nil then
      AWidget.InteractiveFormField := Self;
    FWidget := AWidget;
  end
  else
  begin
    FKids := TdxPDFInteractiveFormFieldCollection.Create(nil);
    FKids.Read(ADictionary, AKidArray, FForm, Self);
    FKidsResolved := True;
  end;
end;

procedure TdxPDFInteractiveFormField.WriteKids(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  if Widget <> nil then
    Widget.Write(AHelper, ADictionary)
  else if Kids <> nil then
    ADictionary.AddInline(TdxPDFKeywords.Kids, Kids);
end;

{ TdxPDFInteractiveFormFieldCollection }

procedure TdxPDFInteractiveFormFieldCollection.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FItems := TdxPDFReferencedObjects.Create;
end;

procedure TdxPDFInteractiveFormFieldCollection.DestroySubClasses;
begin
  FreeAndNil(FItems);
  inherited DestroySubClasses;
end;

procedure TdxPDFInteractiveFormFieldCollection.
  Read(ADictionary: TdxPDFReaderDictionary; AFieldArray: TdxPDFArray;
AForm: TdxPDFInteractiveForm; AParent: TdxPDFInteractiveFormField);
var
  I: Integer;
begin
  FItems.Clear;
  inherited Read(ADictionary);
  if AFieldArray <> nil then
    for I := 0 to AFieldArray.Count - 1 do
      Add(Repository.GetInteractiveFormField(AForm, AParent,
        AFieldArray.ElementList[I].Number));
end;

function TdxPDFInteractiveFormFieldCollection.Write(AHelper: TdxPDFWriterHelper)
  : TdxPDFBase;
var
  AArray: TdxPDFWriterArray;
  I: Integer;
begin
  AArray := AHelper.CreateArray;
  for I := 0 to Count - 1 do
    AArray.AddReference(Items[I]);
  Result := AArray;
end;

procedure TdxPDFInteractiveFormFieldCollection.Add
  (AField: TdxPDFInteractiveFormField);
begin
  if AField <> nil then
    FItems.Add(AField);
end;

procedure TdxPDFInteractiveFormFieldCollection.Delete
  (AField: TdxPDFInteractiveFormField);
begin
  FItems.Delete(FItems.IndexOf(AField));
end;

function TdxPDFInteractiveFormFieldCollection.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxPDFInteractiveFormFieldCollection.GetItem(Index: Integer)
  : TdxPDFInteractiveFormField;
begin
  Result := FItems[Index] as TdxPDFInteractiveFormField;
end;

{ TdxPDFXFAForm }

procedure TdxPDFXFAForm.Initialize(const AData: TBytes);
begin
  Content := Content + TdxPDFUtils.ConvertToUTF8String(AData);
end;

procedure TdxPDFXFAForm.Initialize(ARepository: TdxPDFDocumentRepository;
AArray: TdxPDFArray);
var
  AStream: TdxPDFStream;
  I, AIndex: Integer;
begin
  if AArray <> nil then
  begin
    if (AArray.Count mod 2 <> 0) then
      TdxPDFUtils.RaiseException;
    Content := '';
    AIndex := 0;
    for I := 0 to (AArray.Count div 2) - 1 do
    begin
      if not(AArray[AIndex] is TdxPDFString) then
        TdxPDFUtils.RaiseException;
      Inc(AIndex);
      if AArray[AIndex].ObjectType <> otIndirectReference then
        TdxPDFUtils.RaiseException;
      AStream := ARepository.GetStream(AArray[AIndex].Number);
      Inc(AIndex);
      if AStream = nil then
        TdxPDFUtils.RaiseException;
      if I > 0 then
        Content := Content + '\n';
      Initialize(AStream.UncompressedData);
    end;
  end;
end;

function TdxPDFXFAForm.ToByteArray: TBytes;
begin
  Result := TEncoding.UTF8.GetBytes(Content);
end;

{ TdxPDFInteractiveForm }

procedure TdxPDFInteractiveForm.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FFields := TdxPDFInteractiveFormFieldCollection.Create;
  Resources := nil;
end;

procedure TdxPDFInteractiveForm.DestroySubClasses;
begin
  FreeAndNil(FFields);
  FreeAndNil(FDefaultAppearanceCommands);
  Resources := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFInteractiveForm.ReadProperties(ADictionary
  : TdxPDFReaderDictionary);
begin
  inherited;

  Resources := ADictionary.GetResources(TdxPDFKeywords.DictionaryResources);
  FNeedAppearances := ADictionary.GetBoolean(TdxPDFKeywords.NeedAppearances);
  FSignatureFlags := ADictionary.GetSignatureFlags(TdxPDFKeywords.SigFlags);
  FDefaultAppearanceCommands := ADictionary.GetAppearance(Resources);
  FDefaultTextJustification := ADictionary.GetTextJustification;
  ReadXFAForm(ADictionary);
  FFields.Read(ADictionary, ADictionary.GetArray(TdxPDFKeywords.Fields),
    Self, nil);
end;

procedure TdxPDFInteractiveForm.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  inherited;

  ADictionary.AddReference(TdxPDFKeywords.DictionaryResources, Resources);
  ADictionary.Add(TdxPDFKeywords.NeedAppearances, NeedAppearances);
  ADictionary.Add(TdxPDFKeywords.SigFlags, FSignatureFlags);
  ADictionary.SetAppearance(Resources, DefaultAppearanceCommands);
  ADictionary.SetTextJustification(FDefaultTextJustification);
  ADictionary.AddInline(TdxPDFKeywords.Fields, Fields);
  ADictionary.AddReference(TdxPDFKeywords.XFA, FXFAForm.ToByteArray);
end;

function TdxPDFInteractiveForm.AddSignatureField
  (AWidget: TdxPDFCustomAnnotation; ASignature: TObject)
  : TdxPDFInteractiveFormField;
begin
  Result := TdxPDFSignatureField.Create(Self, AWidget,
    ASignature as TdxPDFSignature);
  NeedAppearances := False;
end;

procedure TdxPDFInteractiveForm.Delete(AField: TdxPDFInteractiveFormField);
begin
  FFields.Delete(AField);
end;

procedure TdxPDFInteractiveForm.SetResources(const AValue: TdxPDFResources);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FResources));
end;

procedure TdxPDFInteractiveForm.ReadXFAForm(ADictionary
  : TdxPDFReaderDictionary);
var
  AArray: TdxPDFArray;
  AStream: TdxPDFStream;
begin
  if ADictionary.TryGetArray(TdxPDFKeywords.XFA, AArray) then
    FXFAForm.Initialize(Repository, AArray)
  else if ADictionary.TryGetStream(TdxPDFKeywords.XFA, AStream) then
    FXFAForm.Initialize(AStream.UncompressedData);
end;

function TdxPDFInteractiveForm.HasSignatureFields: Boolean;
begin
  Result := sfSignaturesExist in SignatureFlags;
end;

{ TdxPDFXMLPacket }

procedure TdxPDFXMLPacket.AfterConstruction;
begin
  inherited;
  AutoIndent := True;
end;

function TdxPDFXMLPacket.GetFooterText: TdxXMLString;
begin
  Result := #13#10'<?xpacket end="w"?>';
end;

function TdxPDFXMLPacket.GetHeaderText: TdxXMLString;
begin
  Result := '<?xpacket begin="'#$EF#$BB#$BF'" id="W5M0MpCehiHzreSzNTczkc9d"?>'#13#10;
end;

{ TdxPDFMetadata }

class function TdxPDFMetadata.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Metadata;
end;

procedure TdxPDFMetadata.Clear;
begin
  FData.Root.Clear;
end;

procedure TdxPDFMetadata.Update(AInformation: TdxPDFDocumentInformation);

  function AddDescription(ANode: TdxXMLNode): TdxXMLNode;
  begin
    Result := ANode.AddChild('rdf:Description');
    Result.Attributes.Add('rdf:about', '');
  end;

  procedure AddLocalizedText(ANode: TdxXMLNode;
  const AName, APrefix: TdxXMLString; const AValue: string);
  begin
    if AValue <> '' then
    begin
      ANode := ANode.AddChild(AName).AddChild('rdf:' + APrefix)
        .AddChild('rdf:li');
      if APrefix = 'Alt' then
        ANode.Attributes.Add('xml:lang', 'x-default');
      ANode.TextAsString := AValue;
    end;
  end;

  procedure AddText(ANode: TdxXMLNode; const AName: TdxXMLString;
  const AValue: string);
  begin
    if AValue <> '' then
      ANode.AddChild(AName).TextAsString := AValue;
  end;

  procedure AddDate(ANode: TdxXMLNode; const AName: TdxXMLString;
  const AValue: TDateTime);
  begin
    if AValue <> NullDate then
      AddText(ANode, AName, TdxXMLDateTime.Create(AValue, True).ToString);
  end;

  procedure AddInformation(ANode: TdxXMLNode;
  AInformation: TdxPDFDocumentInformation);
  begin
    ANode := AddDescription(ANode);
    ANode.Attributes.Add('xmlns:dc', 'http://purl.org/dc/elements/1.1/');
    ANode.Attributes.Add('xmlns:pdf', 'http://ns.adobe.com/pdf/1.3/');
    ANode.Attributes.Add('xmlns:xmp', 'http://ns.adobe.com/xap/1.0/');

    AddDate(ANode, 'xmp:CreateDate', AInformation.CreationDate);
    AddText(ANode, 'xmp:CreatorTool', AInformation.Application);
    AddDate(ANode, 'xmp:ModifyDate', AInformation.ModificationDate);
    AddText(ANode, 'pdf:Producer', AInformation.Producer);
    AddText(ANode, 'pdf:Keywords', AInformation.Keywords);
    AddLocalizedText(ANode, 'dc:creator', 'Seq', AInformation.Author);
    AddLocalizedText(ANode, 'dc:description', 'Alt', AInformation.Subject);
    AddLocalizedText(ANode, 'dc:title', 'Alt', AInformation.Title);
    AddText(ANode, 'dc:format', 'application/pdf');
  end;

  procedure AddConformance(ANode: TdxXMLNode);
  begin
    ANode := AddDescription(ANode);
    ANode.Attributes.Add('xmlns:pdfaid', 'http://www.aiim.org/pdfa/ns/id/');
    ANode.Attributes.Add('pdfaid:conformance', 'A');
    ANode.Attributes.Add('pdfaid:part', 1);
  end;

var
  ANode: TdxXMLNode;
begin
  Clear;

  ANode := FData.Root.AddChild('x:xmpmeta');
  ANode.Attributes.Add('xmlns:x', 'adobe:ns:meta/');
  ANode.Attributes.Add('x:xmptk', 'XMP Core 5.5.0');

  ANode := ANode.AddChild('rdf:RDF');
  ANode.Attributes.Add('xmlns:rdf',
    'http://www.w3.org/1999/02/22-rdf-syntax-ns#');

  AddInformation(ANode, AInformation);
  // AddConformance(ANode);
end;

procedure TdxPDFMetadata.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FData := TdxPDFXMLPacket.Create;
end;

procedure TdxPDFMetadata.DestroySubClasses;
begin
  FreeAndNil(FData);
  inherited DestroySubClasses;
end;

procedure TdxPDFMetadata.Read(AStream: TdxPDFStream);
var
  ABytesStream: TBytesStream;
  AIsEncrypted: Boolean;
begin
  Clear;
  if AStream <> nil then
  begin
    AIsEncrypted := (AStream.EncryptionInfo <> nil) and
      AStream.EncryptionInfo.EncryptMetadata;
    ABytesStream := TBytesStream.Create(TdxPDFStreamAccess(AStream)
      .UncompressData(AIsEncrypted));
    try
      FData.LoadFromStream(ABytesStream);
    finally
      ABytesStream.Free;
    end;
  end;
end;

procedure TdxPDFMetadata.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
var
  ABytesStream: TBytesStream;
  AData: TBytes;
begin
  inherited Write(AHelper, ADictionary);

  ABytesStream := TBytesStream.Create;
  try
    FData.SaveToStream(ABytesStream);
    AData := ABytesStream.Bytes;
    SetLength(AData, ABytesStream.Size);

    ADictionary.AddName(TdxPDFKeywords.Subtype, 'XML');
    ADictionary.SetStreamData(AData, False, AHelper.EncryptMetadata);
  finally
    ABytesStream.Free;
  end;
end;

{ TdxPDFCatalog }

class function TdxPDFCatalog.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Catalog;
end;

procedure TdxPDFCatalog.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FPages := TdxPDFPages.Create(Self);
  FMetadata := TdxPDFMetadata.Create(Self);
end;

procedure TdxPDFCatalog.DestroySubClasses;
begin
  FreeAndNil(FMetadata);
  FreeAndNil(FOutlines);
  FreeAndNil(FOpenDestination);
  FreeAndNil(FOpenAction);
  FreeAndNil(FNames);
  FreeAndNil(FPages);
  AcroForm := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFCatalog.Initialize;
begin
  inherited Initialize;
  FNeedReadNames := True;
  FNeedReadOutlines := True;
  FAcroForm := nil;
  FNames := nil;
  FOpenAction := nil;
  FOpenDestination := nil;
end;

procedure TdxPDFCatalog.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  if ADictionary <> nil then
  begin
    FDictionary := ADictionary;
    ReadPages;
    ReadInteractiveObjects;
    ReadMetadata;
  end
  else
    TdxPDFUtils.Abort;
end;

procedure TdxPDFCatalog.SetRepository(const AValue: TdxPDFDocumentRepository);
begin
  inherited SetRepository(AValue);
  if Repository <> nil then
    Repository.Catalog := Self;
end;

procedure TdxPDFCatalog.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  inherited;

  ADictionary.AddReference(TdxPDFKeywords.Pages, Pages);
  ADictionary.AddReference(TdxPDFKeywords.Outlines, Outlines);
  ADictionary.AddReference(TdxPDFKeywords.Metadata, Metadata);
  ADictionary.AddInline(TdxPDFKeywords.Names, Names);
  WriteInteractiveObjects(AHelper, ADictionary);
end;

function TdxPDFCatalog.AddSignatureField(AState: TdxPDFDocumentState;
ASignature: TObject): TdxPDFInteractiveFormField;
var
  AWidget: TdxPDFWidgetAnnotation;
  AWidgetFlags: TdxPDFAnnotationFlags;
  AWidgetPage: TdxPDFPage;
  AWidgetRect: TdxPDFRectangle;
begin
  AWidgetFlags := TdxPDFAnnotationFlags(Integer(afPrint) or Integer(afLocked));
  AWidgetPage := Pages[0];
  AWidgetRect := TdxPDFRectangle.Create(0, 0, 0, 0);
  AWidget := Repository.CreateWidgetAnnotation(AWidgetPage, AWidgetRect,
    AWidgetFlags) as TdxPDFWidgetAnnotation;
  AWidget.EnsureAppearance(AState);
  Result := AcroForm.AddSignatureField(AWidget, ASignature as TdxPDFSignature);
  dxCallNotify(Repository.OnAddField, Result);
end;

function TdxPDFCatalog.GetEmbeddedFileSpecification(const AName: string)
  : TdxPDFFileSpecification;
begin
  if Names <> nil then
    Result := Names.GetEmbeddedFileSpecification(AName)
  else
    Result := nil;
end;

function TdxPDFCatalog.GetDestination(const AName: string)
  : TdxPDFCustomDestination;
begin
  if Names <> nil then
    Result := Names.GetPageDestination(AName)
  else
    Result := nil;
end;

procedure TdxPDFCatalog.DeleteField(AField: TdxPDFInteractiveFormField);
begin
  if AField.Widget <> nil then
    AField.Widget.Page.Data.Annotations.Remove(AField.Widget);
  AcroForm.Delete(AField);
  if (AField is TdxPDFInteractiveFormField) and
    Assigned(Repository.OnDeleteField) then
    dxCallNotify(Repository.OnDeleteField, AField);
  Repository.DeleteObject(AField.Form);
end;

procedure TdxPDFCatalog.PopulateAttachmentList(AList: TdxPDFFileAttachmentList);
var
  I: Integer;
begin
  AList.Clear;
  if Names <> nil then
    Names.PopulateAttachmentList(AList);
  for I := 0 to Pages.Count - 1 do
    Pages[I].PopulateAttachmentList(AList);
end;

procedure TdxPDFCatalog.SetAcroForm(const AValue: TdxPDFInteractiveForm);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FAcroForm));
end;

procedure TdxPDFCatalog.SetPages(const AValue: TdxPDFPages);
begin
  FreeAndNil(FPages);
  FPages := AValue;
end;

function TdxPDFCatalog.CanWriteAcroForm: Boolean;
begin
  Result := (AcroForm.Fields <> nil) and (AcroForm.Fields.Count > 0);
end;

procedure TdxPDFCatalog.ReadAcroForm;
var
  ADictionary: TdxPDFReaderDictionary;
begin
  if FDictionary.TryGetDictionary(TdxPDFKeywords.AcroForm, ADictionary) then
    AcroForm.Read(ADictionary);
end;

function TdxPDFCatalog.GetAcroForm: TdxPDFInteractiveForm;
begin
  if FAcroForm = nil then
    SetAcroForm(Repository.CreateAcroForm);
  Result := FAcroForm;
end;

function TdxPDFCatalog.GetNames: TdxPDFDocumentNames;
begin
  ReadNames(FDictionary.GetDictionary(TdxPDFKeywords.Names));
  Result := FNames;
end;

function TdxPDFCatalog.GetOutlines: TdxPDFObject;
begin
  ReadOutlines(FDictionary.GetDictionary(TdxPDFKeywords.Outlines));
  Result := FOutlines;
end;

procedure TdxPDFCatalog.ReadInteractiveObjects;
var
  AObject: TdxPDFBase;
begin
  AObject := FDictionary.GetObject(TdxPDFKeywords.OpenAction);
  if AObject <> nil then
  begin
    if AObject.ObjectType <> otArray then
      FOpenAction := FDictionary.GetAction(TdxPDFKeywords.OpenAction)
    else
    begin
      FOpenDestination := Repository.GetDestination(AObject);
      if FOpenDestination <> nil then
        FOpenDestination.ResolveInternalPage;
    end;
  end;
  ReadAcroForm;
end;

procedure TdxPDFCatalog.ReadMetadata;
begin
  FMetadata.Read(FDictionary.GetStream(TdxPDFKeywords.Metadata));
end;

procedure TdxPDFCatalog.ReadNames(ADictionary: TdxPDFReaderDictionary);
begin
  if FNeedReadNames and (ADictionary <> nil) then
  begin
    FNames := TdxPDFDocumentNames.Create(Self);
    FNames.Read(ADictionary);
    FNeedReadNames := False;
  end;
end;

procedure TdxPDFCatalog.ReadOutlines(ADictionary: TdxPDFReaderDictionary);
begin
  if FNeedReadOutlines and (ADictionary <> nil) then
  begin
    FOutlines := TdxPDFOutlines.Create(Self);
    FOutlines.Read(ADictionary);
    FNeedReadOutlines := False;
  end;
end;

procedure TdxPDFCatalog.ReadPages;
var
  ADictionary: TdxPDFReaderDictionary;
begin
  ADictionary := FDictionary.GetDictionary(TdxPDFKeywords.Pages);
  if ADictionary <> nil then
  begin
    Pages.Clear;
    Pages.Read(ADictionary);
  end
  else
    TdxPDFUtils.Abort;
end;

procedure TdxPDFCatalog.WriteInteractiveObjects(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  if OpenAction <> nil then
    ADictionary.AddReference(TdxPDFKeywords.OpenAction, OpenAction)
  else if OpenDestination <> nil then
    ADictionary.AddInline(TdxPDFKeywords.OpenAction, OpenDestination);

  if CanWriteAcroForm then
    ADictionary.AddReference(TdxPDFKeywords.AcroForm, AcroForm);
end;

{ TdxPDFCustomFont }

class function TdxPDFFonts.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Font;
end;

function TdxPDFFonts.GetFont(const AName: string): TdxPDFCustomFont;
begin
  Result := GetObject(AName) as TdxPDFCustomFont;
end;

class function TdxPDFFonts.GetTypePrefix: string;
begin
  Result := 'F';
end;

procedure TdxPDFFonts.Read(ADictionary: TdxPDFReaderDictionary);
begin
  ReadList(ADictionary);
end;

{ TdxPDFGraphicsStateParametersList }

class function TdxPDFGraphicsStateParametersList.GetTypeName: string;
begin
  Result := TdxPDFKeywords.ExtGState;
end;

function TdxPDFGraphicsStateParametersList.GetParameters(const AName: string)
  : TdxPDFGraphicsStateParameters;
begin
  Result := GetObject(AName) as TdxPDFGraphicsStateParameters;
end;

procedure TdxPDFGraphicsStateParametersList.Add(const AName: string;
AStateParameters: TdxPDFGraphicsStateParameters);
begin
  InternalAdd(AName, AStateParameters);
end;

class function TdxPDFGraphicsStateParametersList.GetTypePrefix: string;
begin
  Result := 'P';
end;

function TdxPDFGraphicsStateParametersList.GetTypeDictionaryKey: string;
begin
  Result := TdxPDFKeywords.TypeKey;
end;

procedure TdxPDFGraphicsStateParametersList.
  Read(ADictionary: TdxPDFReaderDictionary);
begin
  ReadList(ADictionary);
end;

procedure TdxPDFGraphicsStateParametersList.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  WriteList(AHelper, ADictionary);
end;

{ TdxPDFPageContentItem }

class function TdxPDFPageContentItem.GetTypeName: string;
begin
  Result := '';
end;

{ TdxPDFXObject }

class function TdxPDFXObject.GetTypeName: string;
begin
  Result := TdxPDFKeywords.XObject;
end;

class function TdxPDFXObject.Parse(ARepository: TdxPDFCustomRepository;
AStream: TdxPDFStream; const ASubtype: string = ''): TdxPDFXObject;
var
  AType, ADefaultSubtype: string;
begin
  AType := AStream.Dictionary.GetString(TdxPDFKeywords.TypeKey);
  ADefaultSubtype := AStream.Dictionary.GetString(TdxPDFKeywords.Subtype);
  if ADefaultSubtype = '' then
    ADefaultSubtype := ASubtype;

  if ((AType <> '') and (AType <> TdxPDFKeywords.XObject) and
    (AType <> TdxPDFKeywords.XObject2)) or (ASubtype = '') then
    TdxPDFUtils.RaiseTestException;

  if ADefaultSubtype = TdxPDFDocumentImage.GetTypeName then
  begin
    Result := TdxPDFDocumentImage.Create(nil);
    TdxPDFDocumentImage(Result).Read(ARepository, AStream);
  end
  else if ADefaultSubtype = 'Form' then
    Result := TdxPDFForm.CreateForm(AStream, nil)
  else
    Result := nil;
end;

procedure TdxPDFXObject.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  ADictionary.AddName(TdxPDFKeywords.TypeKey, TdxPDFKeywords.XObject);
  ADictionary.AddName(TdxPDFKeywords.Subtype, GetTypeName);
  WriteData(AHelper, ADictionary);
end;

procedure TdxPDFXObject.Draw(const AInterpreter: IdxPDFCommandInterpreter);
begin
  EnterCriticalSection(FLock);
  try
    InternalDraw(AInterpreter);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TdxPDFXObject.CreateSubClasses;
begin
  inherited CreateSubClasses;
  InitializeCriticalSectionAndSpinCount(FLock, 1024);
end;

procedure TdxPDFXObject.DestroySubClasses;
begin
  DeleteCriticalSection(FLock);
  inherited DestroySubClasses;
end;

procedure TdxPDFXObject.InternalDraw(const AInterpreter
  : IdxPDFCommandInterpreter);
begin
  // do nothing
end;

{ TdxPDFForm }

class function TdxPDFForm.CreateForm(ADictionary: TdxPDFDictionary;
AParentResources: TdxPDFResources): TdxPDFForm;
begin
  if ADictionary.Contains('Group') then
    Result := TdxPDFGroupForm.Create(nil)
  else
    Result := TdxPDFForm.Create(nil);
  Result.Read(ADictionary as TdxPDFReaderDictionary);
end;

class function TdxPDFForm.CreateForm(AStream: TdxPDFStream;
AParentResources: TdxPDFResources): TdxPDFForm;
begin
  AStream.Dictionary.StreamRef := AStream;
  Result := CreateForm(AStream.Dictionary, AParentResources);
end;

class function TdxPDFForm.GetTypeName: string;
begin
  Result := 'Form';
end;

constructor TdxPDFForm.Create(AOwner: TObject);
begin
  inherited Create(AOwner);
end;

constructor TdxPDFForm.Create(ACatalog: TdxPDFCatalog;
const ABBox: TdxPDFRectangle);
begin
  Create(nil);
  Repository := ACatalog.Repository;
  FBBox := ABBox;
  FMatrix := TdxPDFTransformationMatrix.Create;
  Stream := TdxPDFStream.Create(nil);
  Resources := TdxPDFResources.Create(nil);
  FUseOwnResources := True;
end;

function TdxPDFForm.GetCommands: TdxPDFCommandList;
begin
  if FCommands = nil then
  begin
    FCommands := TdxPDFCommandList.Create;
    FCommands.Read(ActualStream, FRepository, Resources);
  end;
  Result := FCommands;
end;

function TdxPDFForm.GetTransformationMatrix(const ARect: TdxPDFRectangle)
  : TdxPDFTransformationMatrix;
var
  ABoundingBox: TdxPDFRectangle;
  I: Integer;
  ALeft, ARight, ATop, ABottom, AXMin, AXMax, AYMin, AYMax, X, Y, AScaleX,
    AScaleY: Double;
  ABottomLeft, APoint, AOffset: TdxPointF;
  APoints: TdxPDFPoints;
begin
  ABoundingBox := BBox;
  ALeft := ABoundingBox.Left;
  ARight := ABoundingBox.Right;
  ATop := ABoundingBox.Top;
  ABottom := ABoundingBox.Bottom;
  ABottomLeft := FMatrix.Transform(dxPointF(ALeft, ABottom));
  AXMin := ABottomLeft.X;
  AXMax := ABottomLeft.X;
  AYMin := ABottomLeft.Y;
  AYMax := ABottomLeft.Y;
  SetLength(APoints, 3);
  APoints[0] := FMatrix.Transform(dxPointF(ALeft, ATop));
  APoints[1] := FMatrix.Transform(dxPointF(ARight, ATop));
  APoints[2] := FMatrix.Transform(dxPointF(ARight, ABottom));

  for I := Low(APoints) to High(APoints) do
  begin
    APoint := APoints[I];
    X := APoint.X;
    AXMin := TdxPDFUtils.Min(AXMin, X);
    AXMax := TdxPDFUtils.Max(AXMax, X);
    Y := APoint.Y;
    AYMin := TdxPDFUtils.Min(AYMin, Y);
    AYMax := TdxPDFUtils.Max(AYMax, Y);
  end;

  if (AXMax - AXMin) <> 0 then
  begin
    AScaleX := ARect.Width / (AXMax - AXMin);
    AOffset.X := ARect.Left - AXMin * AScaleX;
  end
  else
  begin
    AScaleX := 1;
    AOffset.X := 0;
  end;

  if (AYMax - AYMin) <> 0 then
  begin
    AScaleY := Abs(ARect.Height) / (AYMax - AYMin);
    AOffset.Y := ARect.Bottom - AYMin * AScaleY;
  end
  else
  begin
    AScaleY := 1;
    AOffset.Y := 0;
  end;

  Result := TdxPDFTransformationMatrix.Create(AScaleX, 0, 0, AScaleY, AOffset.X,
    AOffset.Y);
end;

procedure TdxPDFForm.ReplaceCommands(const ACommandData: TBytes);
begin
  FreeAndNil(FCommands);
  if Stream = nil then
    Stream := TdxPDFStream.Create(ACommandData)
  else
    TdxPDFBaseStreamAccess(Stream).SetData(ACommandData);
end;

procedure TdxPDFForm.InternalDraw(const AInterpreter: IdxPDFCommandInterpreter);
begin
  AInterpreter.DrawForm(Self);
end;

procedure TdxPDFForm.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FCommands := nil;
  FRepository := nil;
  Resources := nil;
end;

procedure TdxPDFForm.DestroySubClasses;
begin
  Repository.RemoveResolvedForm(Self);
  Resources := nil;
  FRepository := nil;
  FreeAndNil(FCommands);
  inherited DestroySubClasses;
end;

procedure TdxPDFForm.ReadProperties(ADictionary: TdxPDFReaderDictionary);
begin
  inherited;

  FRepository := Repository;
  FStreamRef := ADictionary.StreamRef;
  if ADictionary.Contains(TdxPDFKeywords.BBox) then
    FBBox := ADictionary.GetRectangleEx(TdxPDFKeywords.BBox)
  else
    FBBox := TdxPDFRectangle.Create(0, 0, 0, 0);

  CheckFormType(ADictionary);
  FMatrix := ADictionary.GetMatrix(TdxPDFKeywords.Matrix);
  Resources := Repository.GetResources(ADictionary);
end;

procedure TdxPDFForm.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  inherited;
  ADictionary.Add(TdxPDFKeywords.BBox, BBox, False);
  ADictionary.Add(TdxPDFKeywords.FormType, 1);
  ADictionary.Add(TdxPDFKeywords.Matrix, FMatrix);
  ADictionary.AddReference(TdxPDFKeywords.Resources, Resources);
end;

procedure TdxPDFForm.WriteData(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  Commands.Write(AHelper, ADictionary, Resources);
end;

procedure TdxPDFForm.ReleaseCircularReferencesAndFree;
var
  AData: TBytes;
begin
  reference;
  SetLength(AData, 0);
  ReplaceCommands(AData);
  Release;
end;

procedure TdxPDFForm.CheckFormType(ADictionary: TdxPDFDictionary);
var
  AFormType: Integer;
begin
  AFormType := ADictionary.GetInteger(TdxPDFKeywords.FormType);
  if (TdxPDFUtils.IsIntegerValid(AFormType) and (AFormType <> 1)) then
    TdxPDFUtils.RaiseTestException;
end;

function TdxPDFForm.GetActualStream: TdxPDFStream;
begin
  if Stream <> nil then
    Result := Stream
  else
    Result := FStreamRef;
end;

procedure TdxPDFForm.SetMatrix(const AValue: TdxPDFTransformationMatrix);
begin
  FMatrix := AValue;
end;

procedure TdxPDFForm.SetResources(const AValue: TdxPDFResources);
begin
  if AValue <> FResources then
  begin
    if FUseOwnResources then
      dxPDFChangeValue(AValue, TdxPDFReferencedObject(FResources))
    else
      FResources := AValue;
  end;
end;

{ TdxPDFCustomSoftMask }

class function TdxPDFCustomSoftMask.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Mask;
end;

class function TdxPDFCustomSoftMask.Parse(ARepository: TdxPDFCustomRepository;
ASourceObject: TdxPDFBase): TdxPDFCustomSoftMask;
var
  ADictionary: TdxPDFReaderDictionary;
  AType, ASoftMaskType: string;
begin
  Result := nil;
  if ASourceObject <> nil then
  begin
    case ASourceObject.ObjectType of
      otIndirectReference:
        Result := TdxPDFCustomSoftMask.Parse(ARepository,
          ARepository.GetObject(ASourceObject.Number) as TdxPDFBase);
      otName:
        begin
          AType := TdxPDFString(ASourceObject).Value;
          if AType <> TdxPDFEmptySoftMask.GetTypeName then
            TdxPDFUtils.RaiseTestException;
          Result := TdxPDFEmptySoftMask.Create(nil);
        end;
      otDictionary:
        begin
          ADictionary := TdxPDFReaderDictionary(ASourceObject);
          AType := ADictionary.GetString(TdxPDFKeywords.TypeKey);
          ASoftMaskType := ADictionary.GetString(TdxPDFKeywords.MaskStyle);
          if ASoftMaskType = TdxPDFLuminositySoftMask.GetTypeName then
            Result := TdxPDFLuminositySoftMask.Create(nil)
          else if ASoftMaskType = TdxPDFAlphaSoftMask.GetTypeName then
            Result := TdxPDFAlphaSoftMask.Create(nil);
          if Result <> nil then
            Result.Read(ADictionary);
        end;
    else
      Result := nil;
    end;
  end;
end;

function TdxPDFCustomSoftMask.IsSame(AMask: TdxPDFCustomSoftMask): Boolean;
begin
  Result := False;
end;

procedure TdxPDFCustomSoftMask.CreateSubClasses;
begin
  inherited CreateSubClasses;
  TransparencyGroup := nil;
  TransparencyFunction := nil;
end;

procedure TdxPDFCustomSoftMask.DestroySubClasses;
begin
  TransparencyFunction := nil;
  TransparencyGroup := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFCustomSoftMask.ReadProperties(ADictionary
  : TdxPDFReaderDictionary);
var
  AObject: TdxPDFBase;
  AValue: TdxPDFStream;
begin
  inherited;

  AValue := ADictionary.GetStream(TdxPDFKeywords.GroupType);
  if AValue <> nil then
    TransparencyGroup := TdxPDFXObject.Parse(Repository, AValue,
      TdxPDFGroupForm.GetTypeName) as TdxPDFGroupForm;

  if ADictionary.TryGetObject(TdxPDFKeywords.TransferFunction, AObject) then
    TransparencyFunction := TdxPDFCustomFunction.Parse(Repository, AObject)
  else
    TransparencyFunction := TdxPDFIdentityFunction.Create(nil);
end;

procedure TdxPDFCustomSoftMask.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  ADictionary.AddName(TdxPDFKeywords.TypeKey, TdxPDFKeywords.Mask);
  ADictionary.AddName(TdxPDFKeywords.MaskStyle, GetTypeName);
  if (TransparencyGroup <> nil) then
    ADictionary.AddReference(TdxPDFKeywords.GroupType, TransparencyGroup);
  if (TransparencyFunction <> nil) and
    not(TransparencyFunction is TdxPDFIdentityFunction) then
    ADictionary.AddReference(TdxPDFKeywords.TransferFunction,
      TransparencyFunction);
end;

procedure TdxPDFCustomSoftMask.SetTransparencyGroup(const AValue
  : TdxPDFGroupForm);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FTransparencyGroup));
end;

procedure TdxPDFCustomSoftMask.SetTransparencyFunction
  (const AValue: TdxPDFObject);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FTransparencyFunction));
end;

{ TdxPDFLuminositySoftMask }

class function TdxPDFLuminositySoftMask.GetTypeName: string;
begin
  Result := 'Luminosity';
end;

procedure TdxPDFLuminositySoftMask.ReadProperties
  (ADictionary: TdxPDFReaderDictionary);
var
  AColorArray: TdxPDFArray;
begin
  inherited;

  AColorArray := ADictionary.GetArray(TdxPDFKeywords.BackdropColor);
  if (AColorArray <> nil) and
    (AColorArray.Count = TransparencyGroup.ColorSpace.ComponentCount) then
    FBackdropColor := TdxPDFColor.Create(AColorArray)
  else
    FBackdropColor := TdxPDFColor.Null;
end;

procedure TdxPDFLuminositySoftMask.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  inherited;
  ADictionary.Add(TdxPDFKeywords.BackdropColor, FBackdropColor);
end;

{ TdxPDFAlphaSoftMask }

class function TdxPDFAlphaSoftMask.GetTypeName: string;
begin
  Result := 'Alpha';
end;

{ TdxPDFEmptySoftMask }

class function TdxPDFEmptySoftMask.GetTypeName: string;
begin
  Result := 'None';
end;

function TdxPDFEmptySoftMask.IsSame(AMask: TdxPDFCustomSoftMask): Boolean;
begin
  Result := AMask = Self;
end;

function TdxPDFEmptySoftMask.Write(AHelper: TdxPDFWriterHelper): TdxPDFBase;
begin
  Result := TdxPDFName.Create(GetTypeName);
end;

{ TdxPDFTransparencyGroup }

class function TdxPDFTransparencyGroup.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Group;
end;

procedure TdxPDFTransparencyGroup.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FColorSpace := nil;
end;

procedure TdxPDFTransparencyGroup.DestroySubClasses;
begin
  FreeAndNil(FColorSpace);
  inherited DestroySubClasses;
end;

procedure TdxPDFTransparencyGroup.Read(ADictionary: TdxPDFReaderDictionary);
var
  AObject: TdxPDFBase;
  AType, ASubtype: string;
begin
  inherited Read(ADictionary);
  AType := ADictionary.GetString(TdxPDFKeywords.TypeKey);
  ASubtype := ADictionary.GetString(SubtypeKey);
  if (AType <> '') and (AType <> TdxPDFKeywords.Group) or (ASubtype <> '') and
    (ASubtype <> TdxPDFKeywords.Transparency) then
    TdxPDFUtils.RaiseTestException;
  if ADictionary.TryGetObject(ColorSpaceKey, AObject) then
    FColorSpace := TdxPDFCustomColorSpace.Parse(Repository, AObject);
  FIsolated := ADictionary.GetBoolean(IsolatedKey);
  FKnockout := ADictionary.GetBoolean(KnockoutKey);
end;

procedure TdxPDFTransparencyGroup.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  inherited;
  ADictionary.AddName(TdxPDFKeywords.TypeKey, TdxPDFKeywords.Group);
  ADictionary.AddName(SubtypeKey, TdxPDFKeywords.Transparency);
  ADictionary.AddReference(ColorSpaceKey, ColorSpace);
  ADictionary.Add(IsolatedKey, FIsolated);
  ADictionary.Add(KnockoutKey, FKnockout);
end;

{ TdxPDFGroupForm }

class function TdxPDFGroupForm.GetTypeName: string;
begin
  Result := 'Group';
end;

procedure TdxPDFGroupForm.DestroySubClasses;
begin
  FreeAndNil(FGroup);
  inherited DestroySubClasses;
end;

procedure TdxPDFGroupForm.InternalDraw(const AInterpreter
  : IdxPDFCommandInterpreter);
begin
  AInterpreter.DrawTransparencyGroup(Self);
end;

procedure TdxPDFGroupForm.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  FGroup := TdxPDFTransparencyGroup.Create(nil);
  FGroup.Read(ADictionary.GetDictionary(TdxPDFKeywords.Group));
end;

procedure TdxPDFGroupForm.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  inherited;
  ADictionary.AddName(TdxPDFKeywords.Subtype, TdxPDFForm.GetTypeName);
  ADictionary.AddReference(TdxPDFKeywords.Group, FGroup);
end;

function TdxPDFGroupForm.GetColorSpace: TdxPDFCustomColorSpace;
begin
  if FGroup <> nil then
    Result := FGroup.ColorSpace
  else
    Result := nil;
end;

{ TdxPDFDocumentImage }

class function TdxPDFDocumentImage.GetTypeName: string;
begin
  Result := 'Image';
end;

constructor TdxPDFDocumentImage.Create(const AData: TBytes;
AColorSpace: TdxPDFCustomColorSpace; AFilters: TdxPDFStreamFilters;
AWidth, AHeight, ABitsPerComponent: Integer; AHasMask: Boolean;
ADictionary: TdxPDFDictionary);
begin
  Create(nil);
  FWidth := AWidth;
  FHeight := AHeight;
  FBitsPerComponent := ABitsPerComponent;
  ColorSpace := AColorSpace;
  Stream := TdxPDFStream.Create(AData);
  if AFilters <> nil then
    Filters.AddRange(AFilters);
  FHasMask := AHasMask;
  if HasMask then
  begin
    if AColorSpace = nil then
      ColorSpace := TdxPDFGrayDeviceColorSpace.Create(Self)
    else if not(ColorSpace is TdxPDFCustomDeviceColorSpace) then
      TdxPDFUtils.RaiseTestException('Create inline image');
  end
  else if AColorSpace = nil then
    TdxPDFUtils.RaiseTestException('Create inline image');
  ReadDecodeRanges(ADictionary);
  if ADictionary <> nil then
    (ADictionary as TdxPDFReaderDictionary)
      .Repository.ImageDataStorage.Add(Self);
end;

function TdxPDFDocumentImage.GetTransformedData(const AParameters
  : TdxPDFImageParameters): TdxPDFScanlineTransformationResult;
var
  AComponentCount: Integer;
  AData: TBytes;
  AFilters: TdxPDFStreamFilters;
  ALastFilterIndex: Integer;
  AScanlineSource: IdxPDFImageScanlineSource;
  ATransformationResult: TdxPDFScanlineTransformationResult;
  I: Integer;
begin
  AFilters := Filters;
  ALastFilterIndex := AFilters.Count - 1;
  if ALastFilterIndex >= 0 then
  begin
    AData := Stream.DecryptedData;
    for I := 0 to ALastFilterIndex - 1 do
      AData := AFilters[I].Decode(AData);
    AComponentCount := 1;
    if ColorSpace <> nil then
      AComponentCount := ColorSpace.ComponentCount;
    ATransformationResult := AFilters[ALastFilterIndex].CreateScanlineSource
      (Self, AComponentCount, AData);
    if FColorSpace = nil then
    begin
      Exit(TdxPDFScanlineTransformationResult.Create
        (GetInterpolatedScanlineSource(ATransformationResult.ScanlineSource,
        AParameters), ATransformationResult.PixelFormat));
    end;
    AScanlineSource := ATransformationResult.ScanlineSource;
  end
  else
    AScanlineSource := TdxPDFCommonImageScanlineSource.CreateImageScanlineSource
      (GetCompressedData, Self, FColorSpace.ComponentCount);

  Result := FColorSpace.Transform(Self, AScanlineSource, AParameters);
end;

function TdxPDFDocumentImage.IsMask: Boolean;
begin
  Result := FHasMask;
end;

procedure TdxPDFDocumentImage.AddListener
  (AListener: IdxPDFDocumentSharedObjectListener);
begin
  if FListeners.IndexOf(AListener) = -1 then
    FListeners.Add(AListener);
end;

procedure TdxPDFDocumentImage.RemoveListener
  (AListener: IdxPDFDocumentSharedObjectListener);
begin
  FListeners.Remove(AListener);
end;

procedure TdxPDFDocumentImage.ReadColorKeyMask(AArray: TdxPDFArray);
var
  I, AIndex, AMin, AMax, AMaxMaskValue: Integer;
  AMaxSourceObject, AMinSourceObject: TdxPDFBase;
begin
  AMaxMaskValue := (1 shl FBitsPerComponent) - 1;
  AIndex := 0;
  SetLength(FColorKeyMask, 0);
  for I := 0 to FComponentCount - 1 do
  begin
    AMinSourceObject := AArray[AIndex];
    AMaxSourceObject := AArray[AIndex + 1];
    Inc(AIndex, 2);
    if (AMinSourceObject.ObjectType <> otInteger) or
      (AMaxSourceObject.ObjectType <> otInteger) then
      TdxPDFUtils.RaiseTestException;
    AMin := TdxPDFInteger(AMinSourceObject).Value;
    AMin := IfThen(AMin < 0, Integer(AMin = 0), AMin);
    AMax := TdxPDFInteger(AMaxSourceObject).Value;
    AMax := IfThen(AMax > AMaxMaskValue, AMaxMaskValue, AMax);
    if AMax < AMin then
      TdxPDFUtils.RaiseTestException;
    TdxPDFUtils.AddRange(TdxPDFRange.Create(AMin, AMax), FColorKeyMask);
  end;
end;

procedure TdxPDFDocumentImage.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FFilters := nil;
  FColorSpace := nil;
  FMask := nil;
  FSoftMask := nil;
  FListeners := TInterfaceList.Create;
end;

procedure TdxPDFDocumentImage.DestroySubClasses;
var
  I: Integer;
begin
  for I := 0 to FListeners.Count - 1 do
    (FListeners[I] as IdxPDFDocumentSharedObjectListener).DestroyHandler(Self);
  FreeAndNil(FListeners);
  ColorSpace := nil;
  Mask := nil;
  SoftMask := nil;
  FreeAndNil(FFilters);
  inherited DestroySubClasses;
end;

procedure TdxPDFDocumentImage.Initialize;
begin
  inherited Initialize;
  FGUID := TdxPDFUtils.GenerateGUID;
  FBitsPerComponent := 1;
end;

procedure TdxPDFDocumentImage.InternalDraw(const AInterpreter
  : IdxPDFCommandInterpreter);
begin
  AInterpreter.DrawImage(Self)
end;

procedure TdxPDFDocumentImage.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  DoRead(ADictionary);
end;

procedure TdxPDFDocumentImage.Read(ARepository: TdxPDFCustomRepository;
AStream: TdxPDFStream);
var
  ADictionary: TdxPDFReaderDictionary;
begin
  ADictionary := AStream.Dictionary as TdxPDFReaderDictionary;
  if ADictionary <> nil then
  begin
    inherited Read(ADictionary);
    Stream := AStream;
    DoRead(ADictionary);
  end;
end;

procedure TdxPDFDocumentImage.ReadProperties(ADictionary
  : TdxPDFReaderDictionary);
begin
  inherited ReadProperties(ADictionary);
  FBitsPerComponent := ADictionary.GetInteger(TdxPDFKeywords.BitsPerComponent);
  FHeight := ADictionary.GetInteger(TdxPDFKeywords.Height, 0);
  FWidth := ADictionary.GetInteger(TdxPDFKeywords.Width, 0);
  FID := ADictionary.GetString(TdxPDFKeywords.ID);
  FHasMask := ADictionary.GetBoolean(TdxPDFKeywords.ImageMask);
  FIntent := ADictionary.GetRenderingIntent(TdxPDFKeywords.Intent);
  FNeedInterpolate := ADictionary.GetBoolean(TdxPDFKeywords.Interpolate);
  FStructParent := ADictionary.GetInteger(TdxPDFKeywords.StructParent);
  FSMaskInData := TdxPDFDocumentImageSMaskInDataType
    (ADictionary.GetInteger(TdxPDFKeywords.SMaskInData, 0));
end;

procedure TdxPDFDocumentImage.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  inherited Write(AHelper, ADictionary);

  ADictionary.Add(TdxPDFKeywords.Height, FHeight);
  ADictionary.Add(TdxPDFKeywords.Width, FWidth);
  ADictionary.Add(TdxPDFKeywords.BitsPerComponent, FBitsPerComponent);

  if IsMask then
    ADictionary.Add(TdxPDFKeywords.ImageMask, True)
  else
  begin
    ADictionary.Add(TdxPDFKeywords.ColorSpace, ColorSpace);
    if FMask <> nil then
      ADictionary.AddReference(TdxPDFKeywords.Mask, FMask)
    else if Length(FColorKeyMask) > 0 then
      ADictionary.Add(TdxPDFKeywords.Mask, FColorKeyMask);
  end;

  ADictionary.Add(TdxPDFKeywords.Decode, FDecodeRanges);
  ADictionary.Add(TdxPDFKeywords.Matte, FMatte);
  ADictionary.AddReference(TdxPDFKeywords.SoftMask, SoftMask);
  ADictionary.Add(TdxPDFKeywords.Interpolate, FNeedInterpolate);
  if Intent <> riAbsoluteColorimetric then
    ADictionary.AddName(TdxPDFKeywords.Intent, Intent);
  if FSMaskInData <> dtNone then
    ADictionary.Add(TdxPDFKeywords.SMaskInData, Ord(FSMaskInData));
end;

procedure TdxPDFDocumentImage.WriteAsInline(AWriter: TdxPDFWriter;
AResources: TdxPDFResources);
var
  ADictionary: TdxPDFDictionary;
  AColorSpaceName: string;
  AColorSpaceObject: TdxPDFBase;
  AWriterHelper: TdxPDFWriterHelper;
begin
  AWriter.WriteSpace;
  AWriter.WriteString(TdxPDFKeywords.InlineImageBegin, True);
  AWriter.WriteSpace;

  if ColorSpace <> nil then
  begin
    AWriter.WriteName(TdxPDFKeywords.ShortColorSpace);
    AWriter.WriteSpace;
    if AResources.TryGetColorSpaceName(ColorSpace, AColorSpaceName) then
      AColorSpaceObject := TdxPDFName.Create(AColorSpaceName)
    else
    begin
      AWriterHelper := TdxPDFWriterHelper.Create(nil);
      try
        AColorSpaceObject := ColorSpace.Write(AWriterHelper);
      finally
        AWriterHelper.Free;
      end;
    end;
    TdxPDFBaseAccess(AColorSpaceObject).Write(AWriter);
    AColorSpaceObject.Free;
    AWriter.WriteSpace;
  end;

  ADictionary := TdxPDFDictionary.Create;
  try
    ADictionary.Add(TdxPDFKeywords.ShortWidth, Width);
    ADictionary.Add(TdxPDFKeywords.ShortHeight, Height);
    ADictionary.Add(TdxPDFKeywords.ShortBitsPerComponent, BitsPerComponent);
    ADictionary.Add(TdxPDFKeywords.ShortDecode, DecodeRanges);
    ADictionary.Add(TdxPDFKeywords.ShortImageMask, IsMask);
    ADictionary.AddName(TdxPDFKeywords.Intent, Intent);
    ADictionary.Add(TdxPDFKeywords.ShortInterpolate, FNeedInterpolate);
    WriteFilters(ADictionary, True);

    TdxPDFDictionaryAccess(ADictionary).WriteContent(AWriter);
  finally
    ADictionary.Free;
  end;

  AWriter.WriteSpace;
  AWriter.WriteString(TdxPDFKeywords.InlineImageData);
  AWriter.WriteSpace;
  AWriter.WriteBytes(GetData);
  AWriter.WriteString(TdxPDFKeywords.InlineImageEnd);
end;

procedure TdxPDFDocumentImage.WriteData(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  WriteFilters(ADictionary, False, AHelper); // before inherited
  inherited WriteData(AHelper, ADictionary);
end;

procedure TdxPDFDocumentImage.WriteFilters(ADictionary: TdxPDFDictionary;
AUseShortNames: Boolean; AHelper: TdxPDFWriterHelper = nil);

  function GetNameKey: string;
  begin
    if AUseShortNames then
      Result := TdxPDFKeywords.ShortFilter
    else
      Result := TdxPDFKeywords.Filter;
  end;

  function GetParamsKey: string;
  begin
    if AUseShortNames then
      Result := TdxPDFKeywords.ShortDecodeParameters
    else
      Result := TdxPDFKeywords.DecodeParameters;
  end;

  function CreateDictionary: TdxPDFDictionary;
  begin
    if AHelper <> nil then
      Result := AHelper.CreateDictionary
    else
      Result := TdxPDFDictionary.Create;
  end;

var
  AFilter: TdxPDFCustomStreamFilter;
  AFilterParams: TdxPDFDictionary;
  ANames: TdxPDFArray;
  AParams: TdxPDFArray;
  I: Integer;
begin
  if Filters.Count = 0 then
    Exit;

  ANames := TdxPDFArray.Create;
  AParams := TdxPDFArray.Create;

  for I := 0 to Filters.Count - 1 do
  begin
    AFilter := Filters[I];
    AFilterParams := CreateDictionary;
    AFilter.Write(AFilterParams);
    AParams.Add(AFilterParams);
    if AUseShortNames then
      ANames.AddName(AFilter.GetShortName)
    else
      ANames.AddName(AFilter.GetName);
  end;

  if ANames.Count > 1 then
  begin
    ADictionary.Add(GetNameKey, ANames);
    ADictionary.Add(GetParamsKey, AParams);
  end
  else
    try
      if ANames.Count = 1 then
      begin
        ADictionary.Add(GetNameKey, ANames[0]);
        if TdxPDFDictionary(AParams[0]).Count > 0 then
          ADictionary.Add(GetParamsKey, AParams[0]);
      end;
    finally
      AParams.Free;
      ANames.Free;
    end;
end;

function TdxPDFDocumentImage.ApplyMask(const AMaskScanlineSource
  : IdxPDFImageScanlineSource; const AParameters: TdxPDFImageParameters;
const AMatte: TDoubleDynArray): TdxPDFImageData;
var
  AActualMatte: TDoubleDynArray;
  AScanlineSource: IdxPDFImageScanlineSource;
  ATransformedData: TdxPDFScanlineTransformationResult;
  ATransparentSource: TdxPDFImageDataSource;
begin
  ATransformedData := GetTransformedData(AParameters);
  AScanlineSource := ATransformedData.ScanlineSource;
  if ATransformedData.PixelFormat = pfGray8bit then
    AScanlineSource := TdxPDFGrayToRGBImageScanlineSource.Create
      (AScanlineSource, AParameters.Width);
  if (FColorSpace <> nil) and (Length(AMatte) > 0) then
    AActualMatte := FColorSpace.AlternateTransform(TdxPDFColor.Create(AMatte))
      .Components
  else
    AActualMatte := AMatte;

  if Length(AActualMatte) = AScanlineSource.ComponentCount then
  begin
    ATransparentSource := TdxPDFImageDataSource
      (TdxPDFTransparentMatteImageDataSource.Create(AScanlineSource,
      AMaskScanlineSource, AParameters.Width, AActualMatte));
  end
  else
    ATransparentSource := TdxPDFTransparentImageDataSource.Create
      (AScanlineSource, AMaskScanlineSource, AParameters.Width);

  Result := TdxPDFImageData.Create(ATransparentSource, AParameters.Width,
    AParameters.Height, AParameters.Width * 4, pfArgb32bpp, nil);
end;

function TdxPDFDocumentImage.GetActualData(const AParameters
  : TdxPDFImageParameters; AInvertRGB: Boolean): TdxPDFImageData;
var
  ATransformationResult: TdxPDFScanlineTransformationResult;
  AWidth, AHeight, AComponentCount, ATemp, ARemain, AStride, I: Integer;
  AImageScanlineSource: IdxPDFImageScanlineSource;
  APalette: TdxPDFARGBColorArray;
  AImageDataSource: TdxPDFImageDataSource;
begin
  if SoftMask <> nil then
    Exit(ApplyMask(SoftMask.GetTransformedData(AParameters).ScanlineSource,
      AParameters, SoftMask.Matte));
  if HasValidStencilMask then
    Exit(ApplyMask(TdxPDFInvertedImageScanlineSource.Create
      (FMask.GetTransformedData(AParameters).ScanlineSource),
      AParameters, nil));
  ATransformationResult := GetTransformedData(AParameters);
  AWidth := AParameters.Width;
  AHeight := AParameters.Height;
  if ATransformationResult.ScanlineSource.HasAlpha then
  begin
    AImageScanlineSource := ATransformationResult.ScanlineSource;
    if ATransformationResult.PixelFormat = pfGray8bit then
      AImageScanlineSource := TdxPDFGrayToRGBImageScanlineSource.Create
        (AImageScanlineSource, AWidth);
    Exit(TdxPDFImageData.Create(TdxPDFColorKeyMaskedImageDataSource.Create
      (AImageScanlineSource, AWidth), AWidth, AHeight, AWidth * 4,
      pfArgb32bpp, nil));
  end;
  if ATransformationResult.PixelFormat = pfGray8bit then
  begin
    AComponentCount := 1;
    SetLength(APalette, 256);
    for I := 0 to 256 - 1 do
      APalette[I] := TdxPDFARGBColor.CreateFromRGB(Byte(I), Byte(I), Byte(I));
  end
  else
  begin
    AComponentCount := 3;
    APalette := nil;
  end;
  ATemp := AWidth * AComponentCount;
  ARemain := ATemp mod 4;
  AStride := IfThen(ARemain > 0, ATemp + 4 - ARemain, ATemp);
  if AInvertRGB then
    AImageDataSource := TdxPDFBGRImageDataSource.Create
      (ATransformationResult.ScanlineSource, AWidth, AStride, AComponentCount)
  else
    AImageDataSource := TdxPDFRGBImageDataSource.Create
      (ATransformationResult.ScanlineSource, AWidth, AStride, AComponentCount);
  Result := TdxPDFImageData.Create(AImageDataSource, AWidth, AHeight, AStride,
    ATransformationResult.PixelFormat, APalette);
end;

function TdxPDFDocumentImage.GetActualSize(const AParameters
  : TdxPDFImageParameters): TdxPDFImageParameters;
var
  AActualWidth, AActualHeight, AMaskWidth, AMaskHeight: Integer;
begin
  AActualWidth := Min(FWidth, AParameters.Width);
  AActualHeight := Min(FHeight, AParameters.Height);
  if (AActualWidth = FWidth) and (AActualHeight = FHeight) then
  begin
    AMaskWidth := 0;
    AMaskHeight := 0;
    if HasValidStencilMask then
    begin
      AMaskWidth := FMask.Width;
      AMaskHeight := FMask.Height;
    end
    else if SoftMask <> nil then
    begin
      AMaskWidth := SoftMask.Width;
      AMaskHeight := SoftMask.Height;
    end;
    if (AMaskWidth > FWidth) or (AMaskHeight > FHeight) then
      Exit(TdxPDFImageParameters.Create(AMaskWidth, AMaskHeight,
        AParameters.ShouldInterpolate));
  end;
  if FWidth * FHeight <= 300 * 300 then
    Result := TdxPDFImageParameters.Create(FWidth, FHeight,
      AParameters.ShouldInterpolate)
  else
    Result := TdxPDFImageParameters.Create(AActualWidth, AActualHeight,
      AParameters.ShouldInterpolate);
end;

function TdxPDFDocumentImage.GetAsBitmap: Graphics.TBitmap;
var
  AImageData: TdxPDFImageData;
  ABitmap: Graphics.TBitmap;
begin
  AImageData := GetActualData(TdxPDFImageParameters.Create(Width, Height,
    True), True);
  TdxPDFRenderingInterpreter.CreateBitmap(AImageData, IsMask,
    procedure(AHandle: GpBitmap)
    var
      ATempImage: TdxSmartImage;
    begin
      ATempImage := TdxSmartImage.Create;
      try
        ATempImage.Handle := AHandle;
        ABitmap := ATempImage.GetAsBitmap;
      finally
        ATempImage.Free;
      end;
    end);
  Result := ABitmap;
end;

function TdxPDFDocumentImage.GetFilters: TdxPDFStreamFilters;
begin
  if (FFilters = nil) and (Stream <> nil) then
    FFilters := dxPDFCreateFilterList(Stream.Dictionary);
  Result := FFilters;
end;

procedure TdxPDFDocumentImage.SetColorSpace(const AValue
  : TdxPDFCustomColorSpace);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FColorSpace));
end;

procedure TdxPDFDocumentImage.SetMask(const AValue: TdxPDFDocumentImage);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FMask));
end;

procedure TdxPDFDocumentImage.SetSoftMask(const AValue: TdxPDFDocumentImage);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FSoftMask));
end;

function TdxPDFDocumentImage.GetCompressedData: TBytes;
begin
  Result := Stream.Data;
end;

procedure TdxPDFDocumentImage.CalculateComponentCount
  (ADictionary: TdxPDFDictionary);
var
  ALastFilterIndex: Integer;
begin
  if not HasMask then
  begin
    if not ADictionary.Contains(TdxPDFKeywords.ColorSpace) or (ColorSpace = nil)
      or not TdxPDFUtils.IsIntegerValid(FBitsPerComponent) then
    begin
      if Filters <> nil then
      begin
        ALastFilterIndex := Filters.Count - 1;
        if (ALastFilterIndex < 0) or
          not(Filters[ALastFilterIndex] is TdxPDFJPXDecodeFilter) then
          TdxPDFUtils.RaiseTestException
            (ClassName + ': CalculateComponentCount');
      end;
      FComponentCount := 0;
    end
    else
      FComponentCount := ColorSpace.ComponentCount;
  end
  else
    FComponentCount := 1;
end;

procedure TdxPDFDocumentImage.DoRead(ADictionary: TdxPDFReaderDictionary);
begin
  if ADictionary <> nil then
  begin
    ReadColorSpace(ADictionary);
    ReadDecodeRanges(ADictionary);
    ReadMatte(ADictionary);
    CalculateComponentCount(ADictionary);
    ReadSoftMask(ADictionary);
    ReadMask(ADictionary);
  end;
end;

procedure TdxPDFDocumentImage.ReadColorSpace(ADictionary
  : TdxPDFReaderDictionary);
var
  AObject: TdxPDFBase;
begin
  if HasMask then
  begin
    FBitsPerComponent := IfThen(TdxPDFUtils.IsIntegerValid(FBitsPerComponent),
      FBitsPerComponent, 1);
    if ADictionary.TryGetObject(TdxPDFKeywords.ColorSpace, AObject) then
    begin
      ColorSpace := TdxPDFCustomColorSpace.Parse(Repository, AObject);
      if not(ColorSpace is TdxPDFGrayDeviceColorSpace) then
        TdxPDFUtils.RaiseTestException('TdxPDFDocumentImage.ReadColorSpace');
    end
    else
      ColorSpace := TdxPDFGrayDeviceColorSpace.Create(Self);
  end
  else if ADictionary.TryGetObject(TdxPDFKeywords.ColorSpace, AObject) then
    ColorSpace := TdxPDFCustomColorSpace.Parse(Repository, AObject);
end;

procedure TdxPDFDocumentImage.ReadDecodeRanges(ADictionary: TdxPDFDictionary);
var
  AArray: TdxPDFArray;
  AComponentCount, I, AIndex: Integer;
begin
  SetLength(FDecodeRanges, 0);
  AArray := ADictionary.GetArray(TdxPDFKeywords.Decode,
    TdxPDFKeywords.ShortDecode);
  if AArray = nil then
  begin
    if FColorSpace <> nil then
      FDecodeRanges := FColorSpace.CreateDefaultDecodeArray(FBitsPerComponent)
    else
      TdxPDFUtils.AddRange(TdxPDFRange.Create(0, 1), FDecodeRanges);
  end
  else
  begin
    if FColorSpace <> nil then
      AComponentCount := FColorSpace.ComponentCount
    else
      AComponentCount := 1;

    if AArray.Count < AComponentCount * 2 then
      TdxPDFUtils.RaiseTestException;

    AIndex := 0;
    for I := 0 to AComponentCount - 1 do
    begin
      TdxPDFUtils.AddRange
        (TdxPDFRange.Create(TdxPDFUtils.ConvertToDouble(AArray[AIndex]),
        TdxPDFUtils.ConvertToDouble(AArray[AIndex + 1])), FDecodeRanges);
      Inc(AIndex, 2);
    end;
  end;
end;

procedure TdxPDFDocumentImage.ReadMask(ADictionary: TdxPDFReaderDictionary);
var
  AArray: TdxPDFArray;
  ASourceObject: TdxPDFBase;
begin
  if not HasMask and ADictionary.TryGetObject(TdxPDFKeywords.Mask, ASourceObject)
  then
    if ASourceObject.ObjectType = otArray then
    begin
      AArray := ASourceObject as TdxPDFArray;
      if (FBitsPerComponent = 0) or (FComponentCount = 0) or
        (AArray.Count <> FComponentCount * 2) then
        TdxPDFUtils.RaiseTestException;
      ReadColorKeyMask(AArray);
    end
    else
    begin
      ASourceObject := Repository.ResolveReference(ASourceObject);
      if not(ASourceObject is TdxPDFNull) then
        Mask := TdxPDFXObject.Parse(Repository, ASourceObject as TdxPDFStream,
          TdxPDFDocumentImage.GetTypeName) as TdxPDFDocumentImage;
    end;
end;

procedure TdxPDFDocumentImage.ReadMatte(ADictionary: TdxPDFDictionary);
var
  I: Integer;
  AArray: TdxPDFArray;
begin
  AArray := ADictionary.GetArray(TdxPDFKeywords.Matte);
  if not HasMask and (AArray <> nil) then
  begin
    SetLength(FMatte, AArray.Count);
    for I := 0 to AArray.Count - 1 do
      FMatte[I] := TdxPDFUtils.ConvertToDouble(AArray[I]);
  end;
end;

procedure TdxPDFDocumentImage.ReadSoftMask(ADictionary: TdxPDFReaderDictionary);
var
  ASoftMaskObject: TdxPDFBase;
  AStream: TdxPDFStream;
begin
  if ADictionary.TryGetObject(TdxPDFKeywords.SoftMask, ASoftMaskObject) then
  begin
    case ASoftMaskObject.ObjectType of
      otIndirectReference:
        AStream := Repository.GetStream
          ((ASoftMaskObject as TdxPDFReference).Number);
      otStream:
        AStream := ASoftMaskObject as TdxPDFStream;
    else
      AStream := nil;
    end;
    if AStream <> nil then
    begin
      SoftMask := TdxPDFXObject.Parse(Repository, AStream,
        TdxPDFDocumentImage.GetTypeName) as TdxPDFDocumentImage;
      if (SoftMask = nil) or (FComponentCount = 0) or
        (SoftMask.ColorSpace = nil) or
        not(SoftMask.ColorSpace is TdxPDFGrayDeviceColorSpace) or
        (SoftMask.Matte <> nil) and (Length(SoftMask.Matte) <> FComponentCount)
      then
        TdxPDFUtils.RaiseTestException(ClassName + ': Error reading soft mask');
    end;
  end;
end;

function TdxPDFDocumentImage.HasValidStencilMask: Boolean;
begin
  Result := (FMask <> nil) and (FMask.BitsPerComponent = 1) and
    ((FMask.ColorSpace = nil) or (FMask.ColorSpace.ComponentCount = 1));
end;

function TdxPDFDocumentImage.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  if GetInterface(IID, Obj) then
    Result := S_OK
  else
    Result := E_NOINTERFACE;
end;

function TdxPDFDocumentImage._AddRef: Integer;
begin
  Result := -1;
end;

function TdxPDFDocumentImage._Release: Integer;
begin
  Result := -1;
end;

function TdxPDFDocumentImage.GetBitsPerComponent: Integer;
begin
  Result := FBitsPerComponent;
end;

function TdxPDFDocumentImage.GetColorKeyMask: TdxPDFRanges;
begin
  Result := FColorKeyMask;
end;

function TdxPDFDocumentImage.GetColorSpaceComponentCount: Integer;
begin
  if ColorSpace <> nil then
    Result := ColorSpace.ComponentCount
  else
    Result := 0;
end;

function TdxPDFDocumentImage.GetDecodeRanges: TdxPDFRanges;
begin
  Result := FDecodeRanges;
end;

function TdxPDFDocumentImage.GetInterpolatedScanlineSource
  (const AData: IdxPDFImageScanlineSource;
const AParameters: TdxPDFImageParameters): IdxPDFImageScanlineSource;
begin
  if (AParameters.Width <> FWidth) or (AParameters.Height <> FHeight) then
    Result := dxPDFImageScanlineSourceFactory.CreateInterpolator(AData,
      AParameters.Width, AParameters.Height, FWidth, FHeight,
      AParameters.ShouldInterpolate)
  else
    Result := AData;
end;

function TdxPDFDocumentImage.GetHeight: Integer;
begin
  Result := FHeight;
end;

function TdxPDFDocumentImage.GetWidth: Integer;
begin
  Result := FWidth;
end;

function TdxPDFDocumentImage.HasSMaskInData: Boolean;
begin
  Result := FSMaskInData <> dtNone;
end;

{ TdxPDFDocumentImageData }

destructor TdxPDFDocumentImageData.Destroy;
begin
  if Assigned(FBitmap) then
    GdipCheck(GdipDisposeImage(FBitmap));
  SetLength(FPalette, 0);
  inherited Destroy;
end;

function TdxPDFDocumentImageData.Clone: TdxPDFDocumentImageData;
begin
  Result := TdxPDFDocumentImageData.Create;
  Result.Data := Data;
  Result.Width := Width;
  Result.Height := Height;
  Result.Stride := Stride;
  Result.PixelFormat := PixelFormat;
  CopyPalette(FPalette, Result.FPalette)
end;

procedure TdxPDFDocumentImageData.Assign(AImageData: TdxPDFDocumentImageData);
begin
  SetLength(FData, Length(AImageData.Data));
  TdxPDFUtils.CopyData(AImageData.Data, 0, FData, 0, Length(AImageData.Data));
  Width := AImageData.Width;
  Height := AImageData.Height;
  Stride := AImageData.Stride;
  PixelFormat := AImageData.PixelFormat;
  SetLength(FPalette, 0);
  if AImageData.Palette <> nil then
    CopyPalette(AImageData.Palette, FPalette);
end;

procedure TdxPDFDocumentImageData.CalculateStride(AWidth, AComponentCount
  : Integer);
var
  ATemp, ARemain: Integer;
begin
  ATemp := AWidth * AComponentCount;
  ARemain := ATemp mod 4;
  Stride := IfThen(ARemain > 0, ATemp + 4 - ARemain, ATemp);
end;

procedure TdxPDFDocumentImageData.PopulateData(const ASourceData: TBytes;
AWidth, ASourceStride, AComponentCount: Integer);
var
  AComponents: TBytes;
  ARowIndex, ACurrentSourceIndex, ACurrentDestinationIndex, AColumnIndex,
    ADestinationIndex, ASourceIndex, I: Integer;
begin
  ACurrentSourceIndex := 0;
  ACurrentDestinationIndex := 0;
  SetLength(AComponents, AComponentCount);
  SetLength(FData, Stride * Height);
  for ARowIndex := 0 to Height - 1 do
  begin
    ASourceIndex := ACurrentSourceIndex;
    ADestinationIndex := ACurrentDestinationIndex;
    for AColumnIndex := 0 to AWidth - 1 do
    begin
      TdxPDFUtils.CopyData(ASourceData, ASourceIndex, AComponents, 0,
        AComponentCount);
      Inc(ASourceIndex, AComponentCount);
      for I := AComponentCount - 1 downto 0 do
      begin
        FData[ADestinationIndex] := AComponents[I];
        Inc(ADestinationIndex);
      end;
    end;
    Inc(ACurrentSourceIndex, ASourceStride);
    Inc(ACurrentDestinationIndex, Stride);
  end;
end;

procedure TdxPDFDocumentImageData.AddPaletteColor(const AColor: TdxPDFARGBColor;
var APalette: TdxPDFARGBColorArray);
var
  L: Integer;
begin
  L := Length(APalette);
  SetLength(APalette, L + 1);
  APalette[L] := AColor;
end;

procedure TdxPDFDocumentImageData.CopyPalette(const ASource
  : TdxPDFARGBColorArray; var ADestination: TdxPDFARGBColorArray);
var
  I, L: Integer;
begin
  L := Length(ASource);
  SetLength(ADestination, L);
  for I := 0 to L - 1 do
    cxCopyData(@ASource[I], @ADestination[I], SizeOf(ASource[I]));
end;

procedure TdxPDFDocumentImageData.CalculateParameters(out AComponentCount,
  ASourceStride, AActualWidth: Integer);
var
  I: Integer;
begin
  SetLength(FPalette, 0);
  AComponentCount := 1;
  case PixelFormat of
    pfGray1bit:
      begin
        AActualWidth := Width div 8;
        if Width mod 8 > 0 then
          Inc(AActualWidth);
        ASourceStride := AActualWidth;
        AddPaletteColor(TdxPDFARGBColor.CreateFromRGB(0, 0, 0, 255), FPalette);
        AddPaletteColor(TdxPDFARGBColor.CreateFromRGB(255, 255, 255, 255),
          FPalette);
      end;
    pfGray8bit:
      begin
        AActualWidth := Width;
        ASourceStride := AActualWidth;
        for I := 0 to 256 - 1 do
          AddPaletteColor(TdxPDFARGBColor.CreateFromRGB(I, I, I, 255),
            FPalette);
      end;
  else
    AComponentCount := 3;
    AActualWidth := Width;
    ASourceStride := AActualWidth * 3;
  end;
  CalculateStride(AActualWidth, AComponentCount);
end;

{ TdxPDFCustomEncoding }

function TdxPDFCustomEncoding.Write(AHelper: TdxPDFWriterHelper): TdxPDFBase;
var
  AShortEncodingName: string;
begin
  if UseShortWrite then
  begin
    AShortEncodingName := GetShortName;
    if AShortEncodingName <> '' then
      Result := TdxPDFName.Create(AShortEncodingName)
    else
      Result := nil;
  end
  else
    Result := inherited;
end;

procedure TdxPDFCustomEncoding.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FFontFileEncoding := GetFontFileEncodingClass.Create;
end;

procedure TdxPDFCustomEncoding.DestroySubClasses;
begin
  FreeAndNil(FFontFileEncoding);
  inherited DestroySubClasses;
end;

function TdxPDFCustomEncoding.GetShortName: string;
begin
  Result := GetFontFileEncodingClass.GetName;
end;

function TdxPDFCustomEncoding.GetFontFileEncodingClass
  : TdxFontFileCustomEncodingClass;
begin
  Result := TdxFontFileCustomEncoding;
end;

function TdxPDFCustomEncoding.UseShortWrite: Boolean;
begin
  Result := False;
end;

{ TdxPDFCustomFontDescriptor }

class function TdxPDFCustomFontDescriptor.CreateStandardDictionary
  (ARepository: TdxPDFCustomRepository; const AFontName: string)
  : TdxPDFDictionary;
var
  AProc: TdxPDFPopulateStandardFontDescriptorProc;
begin
  Result := TdxPDFReaderDictionary.Create
    (ARepository as TdxPDFDocumentRepository);
  Result.Add(TdxPDFKeywords.TypeKey, TdxPDFKeywords.FontDescriptor);
  Result.Add(TdxPDFKeywords.FontName, AFontName);
  AProc := GetFontDescriptorPopulationProc(GetActualFontName(AFontName));
  if Assigned(AProc) then
    AProc(Result)
  else
    FreeAndNil(Result);
end;

class function TdxPDFCustomFontDescriptor.GetTypeName: string;
begin
  Result := TdxPDFKeywords.FontDescriptor;
end;

constructor TdxPDFCustomFontDescriptor.Create(const ADescriptorData
  : TdxPDFFontDescriptorData);
begin
  inherited Create;
  FAscent := Round(ADescriptorData.Ascent);
  FCapHeight := 500;
  FDescent := Round(-ADescriptorData.Descent);
  FFlags := ADescriptorData.Flags;
  FFontStretch := fsNormal;
  FFontWeight := IfThen(ADescriptorData.Bold,
    TdxPDFGDIFontSubstitutionHelper.BoldWeight, GetNormalWeight);
  FItalicAngle := ADescriptorData.ItalicAngle;
  FFontBBox := TdxPDFRectangle.Create(ADescriptorData.BBox.Left,
    ADescriptorData.BBox.Bottom, ADescriptorData.BBox.Right,
    ADescriptorData.BBox.Top);
end;

class function TdxPDFCustomFontDescriptor.GetNormalWeight: Integer;
begin
  Result := TdxPDFGDIFontSubstitutionHelper.NormalWeight;
end;

procedure TdxPDFCustomFontDescriptor.Initialize;
begin
  inherited Initialize;
  FFontStretch := fsNormal;
  FFontWeight := GetNormalWeight;
end;

procedure TdxPDFCustomFontDescriptor.ReadProperties
  (ADictionary: TdxPDFReaderDictionary);
var
  AFont: TdxPDFCustomFont;
  AStream: TdxPDFStream;
begin
  FHasData := True;
  inherited ReadProperties(ADictionary);
  Number := ADictionary.Number;
  ReadFontStretch(ADictionary);
  FAscent := Abs(ADictionary.GetDouble(TdxPDFKeywords.FontDescriptorAscent, 0));
  FDescent :=
    -Abs(ADictionary.GetDouble(TdxPDFKeywords.FontDescriptorDescent, 0));
  FAvgWidth := ADictionary.GetDouble(TdxPDFKeywords.FontDescriptorAvgWidth, 0);
  FCapHeight := ADictionary.GetSmallInt
    (TdxPDFKeywords.FontDescriptorCapHeight, 0);
  FCharSet := ADictionary.GetString(TdxPDFKeywords.FontDescriptorCharSet);
  FFlags := ADictionary.GetInteger(TdxPDFKeywords.FontDescriptorFlags, 0);
  FFontBBox := ADictionary.GetRectangleEx(TdxPDFKeywords.FontDescriptorBBox);
  FFontFamily := ADictionary.GetString(TdxPDFKeywords.FontDescriptorFamily);
  FFontName := ADictionary.GetString(TdxPDFKeywords.FontName);
  FItalicAngle := ADictionary.GetDouble
    (TdxPDFKeywords.FontDescriptorItalicAngle, 0);
  FLeading := ADictionary.GetDouble(TdxPDFKeywords.FontDescriptorLeading, 0);
  FMaxWidth := ADictionary.GetDouble(TdxPDFKeywords.FontDescriptorMaxWidth, 0);
  FMissingWidth := ADictionary.GetDouble
    (TdxPDFKeywords.FontDescriptorMissingWidth, 0);
  FStemH := ADictionary.GetDouble(TdxPDFKeywords.FontDescriptorStemH, 0);
  FStemV := ADictionary.GetDouble(TdxPDFKeywords.FontDescriptorStemV, 0);
  FXHeight := ADictionary.GetSmallInt(TdxPDFKeywords.FontDescriptorXHeight, 0);
  ReadFontWeight(ADictionary);

  AFont := Font;
  if FontName = '' then
    FontName := AFont.BaseFont;
  if FontWeight = 0 then
    FontWeight := 400;
  if Ascent < 0 then
    Ascent := -Ascent;
  if Descent > 0 then
    Descent := -Descent;
  if ADictionary.TryGetStream(TdxPDFKeywords.FontDescriptorCIDSet, AStream) then
    FCIDSetData := AStream.UncompressedData;
end;

procedure TdxPDFCustomFontDescriptor.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
var
  AFont: TdxPDFCustomFont;
begin
  inherited;
  ADictionary.AddName(TdxPDFKeywords.FontName, FontName, False);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorCharSet, FCharSet);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorAvgWidth, AvgWidth, 0);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFlags, FFlags);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFamily, FFontFamily);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorItalicAngle, ItalicAngle);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorLeading, FLeading, 0);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorMaxWidth, MaxWidth, 0);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorMissingWidth, MissingWidth, 0);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorStemH, FStemH, 0);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorXHeight, XHeight, 0);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorWeight, FFontWeight,
    GetNormalWeight);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorBBox, FFontBBox, False);

  if FFontStretch <> fsNormal then
    ADictionary.AddName(TdxPDFKeywords.FontStretch,
      TdxFontStretchToStringMap[FFontStretch]);

  AFont := GetFont;
  if AFont.HasSizeAttributes then
  begin
    ADictionary.Add(TdxPDFKeywords.FontDescriptorAscent, Ascent);
    ADictionary.Add(TdxPDFKeywords.FontDescriptorDescent, Descent);
    ADictionary.Add(TdxPDFKeywords.FontDescriptorCapHeight, CapHeight);
    ADictionary.Add(TdxPDFKeywords.FontDescriptorStemV, FStemV);
  end;
  ADictionary.AddReference(TdxPDFKeywords.FontDescriptorCIDSet, FCIDSetData);
  WriteFontFile(AHelper, ADictionary);
end;

procedure TdxPDFCustomFontDescriptor.WriteFontFile(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  // do nothing
end;

function TdxPDFCustomFontDescriptor.GetOpenTypeFontFileData
  (ADictionary: TdxPDFReaderDictionary; ASuppressException: Boolean): TBytes;
var
  AStream: TdxPDFStream;
begin
  SetLength(Result, 0);
  AStream := GetStream(TdxPDFKeywords.FontFile3, ADictionary);
  if AStream = nil then
    Exit(nil);
  if AStream.Dictionary.GetString(TdxPDFKeywords.Subtype) = TdxPDFKeywords.OpenTypeFont
  then
    Result := AStream.UncompressedData
  else if not ASuppressException then
    TdxPDFUtils.RaiseTestException('TdxPDFType1Font.ReadOpenTypeFontFileData');
end;

function TdxPDFCustomFontDescriptor.GetStream(const AKey: string;
ADictionary: TdxPDFReaderDictionary): TdxPDFStream;
var
  AObject: TdxPDFBase;
begin
  Result := nil;
  AObject := ADictionary.GetObject(AKey);
  if AObject <> nil then
    case AObject.ObjectType of
      otStream:
        Result := TdxPDFStream(AObject);
      otIndirectReference:
        Result := Repository.GetStream(AObject.Number);
    end;
end;

function TdxPDFCustomFontDescriptor.WriteOpenTypeFontData
  (AHelper: TdxPDFWriterHelper; ADictionary: TdxPDFWriterDictionary;
const AData: TBytes): Boolean;
var
  AFontDictionary: TdxPDFWriterDictionary;
begin
  Result := Length(AData) > 0;
  if Result then
  begin
    AFontDictionary := AHelper.CreateDictionary;
    AFontDictionary.AddName(TdxPDFKeywords.Subtype,
      TdxPDFKeywords.OpenTypeFont);
    AFontDictionary.SetStreamData(AData);
    ADictionary.AddReference(TdxPDFKeywords.FontFile3, AFontDictionary);
  end;
end;

function TdxPDFCustomFontDescriptor.GetActualAscent: Double;
begin
  if IsFontMetricsInvalid then
    Result := Max(0, FFontBBox.Top)
  else
    Result := FAscent;
end;

function TdxPDFCustomFontDescriptor.GetActualDescent: Double;
begin
  if IsFontMetricsInvalid then
    Result := Math.Min(0, FFontBBox.Bottom)
  else
    Result := FDescent;
end;

function TdxPDFCustomFontDescriptor.GetFont: TdxPDFCustomFont;
begin
  Result := Parent as TdxPDFCustomFont;
end;

function TdxPDFCustomFontDescriptor.GetFontBBox: TdxRectF;
begin
  Result := FFontBBox.ToRectF;
end;

function TdxPDFCustomFontDescriptor.GetHeight: Double;
var
  AHeight: Double;
begin
  if not FFontBBox.IsNull then
  begin
    AHeight := FFontBBox.Height;
    if AHeight <> 0 then
      Exit(AHeight);
  end;
  Result := FAscent - FDescent;
end;

function TdxPDFCustomFontDescriptor.IsFontMetricsInvalid: Boolean;
begin
  Result := ((FAscent = 0) or (FDescent >= 0)) and not FFontBBox.IsNull;
end;

procedure TdxPDFCustomFontDescriptor.ReadFontStretch
  (ADictionary: TdxPDFDictionary);
var
  I: TdxFontFileStretch;
  AFontStretch: string;
begin
  FFontStretch := fsNormal;
  AFontStretch := ADictionary.GetString(TdxPDFKeywords.FontStretch);
  if AFontStretch <> '' then
    for I := Low(TdxFontFileStretch) to High(TdxFontFileStretch) do
      if TdxFontStretchToStringMap[I] = AFontStretch then
      begin
        FFontStretch := I;
        Break;
      end;
end;

procedure TdxPDFCustomFontDescriptor.ReadFontWeight
  (ADictionary: TdxPDFDictionary);
var
  AObject: TdxPDFBase;
  AName: string;
begin
  AObject := ADictionary.GetObject(TdxPDFKeywords.FontDescriptorWeight);
  if AObject <> nil then
    case AObject.ObjectType of
      otInteger:
        FFontWeight := TdxPDFInteger(AObject).Value;
      otName, otString:
        begin
          AName := TdxPDFString(AObject).Value;
          if AName = TdxPDFKeywords.Italic then
            FFlags := Integer(FFlags) or Integer(TdxFontFileFlags.ffItalic)
          else if AName = TdxPDFKeywords.Bold then
            FFontWeight := TdxPDFGDIFontSubstitutionHelper.BoldWeight;
        end;
    end;
end;

class function TdxPDFCustomFontDescriptor.GetActualFontName(const AFontName
  : string): string;
var
  AStandardFontNameMap: TdxPDFStringStringDictionary;
begin
  AStandardFontNameMap := TdxPDFStringStringDictionary.Create;
  try
    PopulateStandardFontMap(AStandardFontNameMap);
    if not AStandardFontNameMap.TryGetValue(AFontName, Result) then
      Result := AFontName;
  finally
    FreeAndNil(AStandardFontNameMap);
  end;
end;

class function TdxPDFCustomFontDescriptor.GetFontDescriptorPopulationProc
  (const AFontName: string): TdxPDFPopulateStandardFontDescriptorProc;
var
  APopulationProcDictionary
    : TDictionary<string, TdxPDFPopulateStandardFontDescriptorProc>;
begin
  APopulationProcDictionary :=
    TDictionary<string, TdxPDFPopulateStandardFontDescriptorProc>.Create;
  APopulationProcDictionary.Add(TdxPDFKeywords.TimesRomanFontName,
    PopulateTimesNewRomanFontDescriptorDictionary);
  APopulationProcDictionary.Add(TdxPDFKeywords.SymbolFontName,
    PopulateSymbolFontDescriptorDictionary);
  APopulationProcDictionary.Add(TdxPDFKeywords.TimesBoldFontName,
    PopulateTimesBoldFontDescriptorDictionary);
  APopulationProcDictionary.Add(TdxPDFKeywords.TimesItalicFontName,
    PopulateTimesItalicFontDescriptorDictionary);
  APopulationProcDictionary.Add(TdxPDFKeywords.TimesBoldItalicFontName,
    PopulateTimesBoldItalicFontDescriptorDictionary);
  APopulationProcDictionary.Add(TdxPDFKeywords.ArialFontName,
    PopulateHelveticaFontDescriptorDictionary);
  APopulationProcDictionary.Add(TdxPDFKeywords.HelveticaFontName,
    PopulateHelveticaFontDescriptorDictionary);
  APopulationProcDictionary.Add(TdxPDFKeywords.HelveticaObliqueFontName,
    PopulateHelveticaObliqueFontDescriptorDictionary);
  APopulationProcDictionary.Add(TdxPDFKeywords.HelveticaBoldFontName,
    PopulateHelveticaBoldFontDescriptorDictionary);
  APopulationProcDictionary.Add(TdxPDFKeywords.HelveticaBoldObliqueFontName,
    PopulateHelveticaBoldObliqueFontDescriptorDictionary);
  APopulationProcDictionary.Add(TdxPDFKeywords.CourierFontName,
    PopulateCourierFontDescriptorDictionary);
  APopulationProcDictionary.Add(TdxPDFKeywords.CourierNewFontName,
    PopulateCourierFontDescriptorDictionary);
  APopulationProcDictionary.Add(TdxPDFKeywords.CourierNewBoldFontName,
    PopulateCourierFontDescriptorDictionary);
  APopulationProcDictionary.Add(TdxPDFKeywords.CourierObliqueFontName,
    PopulateCourierObliqueFontDescriptorDictionary);
  APopulationProcDictionary.Add(TdxPDFKeywords.CourierBoldFontName,
    PopulateCourierBoldFontDescriptorDictionary);
  APopulationProcDictionary.Add(TdxPDFKeywords.CourierBoldObliqueFontName,
    PopulateCourierBoldObliqueFontDescriptorDictionary);
  APopulationProcDictionary.Add(TdxPDFKeywords.ZapfDingbatsFontName,
    PopulateZapfDingbatsFontDescriptorDictionary);
  APopulationProcDictionary.TrimExcess;
  try
    if not APopulationProcDictionary.TryGetValue(AFontName, Result) then
      Result := nil;
  finally
    FreeAndNil(APopulationProcDictionary);
  end;
end;

class procedure TdxPDFCustomFontDescriptor.
  PopulateHelveticaObliqueFontDescriptorDictionary
  (ADictionary: TdxPDFDictionary);
begin
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFamily,
    TdxPDFKeywords.HelveticaFontName);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFlags, Integer(ffNonSymbolic) or
    Integer(ffItalic) or Integer(ffSmallCap));
  ADictionary.Add(TdxPDFKeywords.FontDescriptorBBox,
    dxRectF(-170, -225, 1116, 931), False);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorItalicAngle, -12);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorAscent, 718);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorDescent, -207);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorCapHeight, 718);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorXHeight, 523);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorStemV, 88);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorStemH, 76);
end;

class procedure TdxPDFCustomFontDescriptor.PopulateStandardFontMap
  (AStandardFontNameMap: TdxPDFStringStringDictionary);
begin
  AStandardFontNameMap.Add('Arial', 'Helvetica');
  AStandardFontNameMap.Add('Arial,Bold', 'Helvetica-Bold');
  AStandardFontNameMap.Add('Arial,BoldItalic', 'Helvetica-BoldOblique');
  AStandardFontNameMap.Add('Arial,Italic', 'Helvetica-Oblique');
  AStandardFontNameMap.Add('Arial-Bold', 'Helvetica-Bold');
  AStandardFontNameMap.Add('Arial-BoldItalic', 'Helvetica-BoldOblique');
  AStandardFontNameMap.Add('Arial-BoldItalicMT', 'Helvetica-BoldOblique');
  AStandardFontNameMap.Add('Arial-BoldMT', 'Helvetica-Bold');
  AStandardFontNameMap.Add('Arial-Italic', 'Helvetica-Oblique');
  AStandardFontNameMap.Add('Arial-ItalicMT', 'Helvetica-Oblique');
  AStandardFontNameMap.Add('Helvetica-Compressed', 'Helvetica');
  AStandardFontNameMap.Add('ArialMT', 'Helvetica');
  AStandardFontNameMap.Add('CourierNew', 'Courier');
  AStandardFontNameMap.Add('CourierNew,Bold', 'Courier-Bold');
  AStandardFontNameMap.Add('CourierNew,BoldItalic', 'Courier-BoldOblique');
  AStandardFontNameMap.Add('CourierNew,Italic', 'Courier-Oblique');
  AStandardFontNameMap.Add('CourierNew-Bold', 'Courier-Bold');
  AStandardFontNameMap.Add('CourierNew-BoldItalic', 'Courier-BoldOblique');
  AStandardFontNameMap.Add('CourierNew-Italic', 'Courier-Oblique');
  AStandardFontNameMap.Add('CourierNewPS-BoldItalicMT', 'Courier-BoldOblique');
  AStandardFontNameMap.Add('CourierNewPS-BoldMT', 'Courier-Bold');
  AStandardFontNameMap.Add('CourierNewPS-ItalicMT', 'Courier-Oblique');
  AStandardFontNameMap.Add('CourierNewPSMT', 'Courier');
  AStandardFontNameMap.Add('Helvetica,Bold', 'Helvetica-Bold');
  AStandardFontNameMap.Add('Helvetica,BoldItalic', 'Helvetica-BoldOblique');
  AStandardFontNameMap.Add('Helvetica,Italic', 'Helvetica-Oblique');
  AStandardFontNameMap.Add('Helvetica-BoldItalic', 'Helvetica-BoldOblique');
  AStandardFontNameMap.Add('Helvetica-Italic', 'Helvetica-Oblique');
  AStandardFontNameMap.Add('Symbol,Bold', 'Symbol');
  AStandardFontNameMap.Add('Symbol,BoldItalic', 'Symbol');
  AStandardFontNameMap.Add('Symbol,Italic', 'Symbol');
  AStandardFontNameMap.Add('TimesNewRoman', 'Times-Roman');
  AStandardFontNameMap.Add('TimesNewRoman,Bold', 'Times-Bold');
  AStandardFontNameMap.Add('TimesNewRoman,BoldItalic', 'Times-BoldItalic');
  AStandardFontNameMap.Add('TimesNewRoman,Italic', 'Times-Italic');
  AStandardFontNameMap.Add('TimesNewRoman-Bold', 'Times-Bold');
  AStandardFontNameMap.Add('TimesNewRoman-BoldItalic', 'Times-BoldItalic');
  AStandardFontNameMap.Add('TimesNewRoman-Italic', 'Times-Italic');
  AStandardFontNameMap.Add('TimesNewRomanPS', 'Times-Roman');
  AStandardFontNameMap.Add('TimesNewRomanPS-Bold', 'Times-Bold');
  AStandardFontNameMap.Add('TimesNewRomanPS-BoldItalic', 'Times-BoldItalic');
  AStandardFontNameMap.Add('TimesNewRomanPS-BoldItalicMT', 'Times-BoldItalic');
  AStandardFontNameMap.Add('TimesNewRomanPS-BoldMT', 'Times-Bold');
  AStandardFontNameMap.Add('TimesNewRomanPS-Italic', 'Times-Italic');
  AStandardFontNameMap.Add('TimesNewRomanPS-ItalicMT', 'Times-Italic');
  AStandardFontNameMap.Add('TimesNewRomanPSMT', 'Times-Roman');
  AStandardFontNameMap.Add('TimesNewRomanPSMT,Bold', 'Times-Bold');
  AStandardFontNameMap.Add('TimesNewRomanPSMT,BoldItalic', 'Times-BoldItalic');
  AStandardFontNameMap.Add('TimesNewRomanPSMT,Italic', 'Times-Italic');
  AStandardFontNameMap.TrimExcess;
end;

class procedure TdxPDFCustomFontDescriptor.
  PopulateCourierFontDescriptorDictionary(ADictionary: TdxPDFDictionary);
begin
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFamily,
    TdxPDFKeywords.CourierFontName);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFlags, Integer(ffFixedPitch) or
    Integer(ffSerif) or Integer(ffNonSymbolic) or Integer(ffSmallCap));
  ADictionary.Add(TdxPDFKeywords.FontDescriptorBBox,
    dxRectF(-23, -250, 715, 805), False);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorItalicAngle, 0);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorAscent, 629);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorDescent, -157);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorCapHeight, 562);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorXHeight, 426);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorStemV, 51);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorStemH, 51);
end;

class procedure TdxPDFCustomFontDescriptor.
  PopulateCourierBoldFontDescriptorDictionary(ADictionary: TdxPDFDictionary);
begin
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFamily,
    TdxPDFKeywords.CourierFontName);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFlags, Integer(ffFixedPitch) or
    Integer(ffSerif) or Integer(ffNonSymbolic) or Integer(ffSmallCap) or
    Integer(ffForceBold));
  ADictionary.Add(TdxPDFKeywords.FontDescriptorBBox,
    dxRectF(-113, -250, 749, 801), False);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorItalicAngle, 0);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorAscent, 629);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorDescent, -157);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorCapHeight, 562);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorXHeight, 439);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorStemV, 106);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorStemH, 84);
end;

class procedure TdxPDFCustomFontDescriptor.
  PopulateCourierBoldObliqueFontDescriptorDictionary
  (ADictionary: TdxPDFDictionary);
begin
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFamily,
    TdxPDFKeywords.CourierFontName);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFlags, Integer(ffFixedPitch) or
    Integer(ffSerif) or Integer(ffNonSymbolic) or Integer(ffItalic) or
    Integer(ffSmallCap) or Integer(ffForceBold));
  ADictionary.Add(TdxPDFKeywords.FontDescriptorBBox,
    dxRectF(-57, -250, 869, 801), False);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorItalicAngle, -12);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorAscent, 629);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorDescent, -157);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorCapHeight, 562);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorXHeight, 439);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorStemV, 106);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorStemH, 84);
end;

class procedure TdxPDFCustomFontDescriptor.
  PopulateCourierObliqueFontDescriptorDictionary(ADictionary: TdxPDFDictionary);
begin
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFamily,
    TdxPDFKeywords.CourierFontName);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFlags, Integer(ffFixedPitch) or
    Integer(ffSerif) or Integer(ffNonSymbolic) or Integer(ffItalic) or
    Integer(ffSmallCap));
  ADictionary.Add(TdxPDFKeywords.FontDescriptorBBox,
    dxRectF(-27, -250, 849, 805), False);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorItalicAngle, -12);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorAscent, 629);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorDescent, -157);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorCapHeight, 562);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorXHeight, 426);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorStemV, 51);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorStemH, 51);
end;

class procedure TdxPDFCustomFontDescriptor.
  PopulateHelveticaFontDescriptorDictionary(ADictionary: TdxPDFDictionary);
begin
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFamily,
    TdxPDFKeywords.HelveticaFontName);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFlags, Integer(ffNonSymbolic) or
    Integer(ffSmallCap));
  ADictionary.Add(TdxPDFKeywords.FontDescriptorBBox,
    dxRectF(-166, -225, 1000, 931), False);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorItalicAngle, 0);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorAscent, 718);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorDescent, -207);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorCapHeight, 718);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorXHeight, 523);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorStemV, 88);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorStemH, 76);
end;

class procedure TdxPDFCustomFontDescriptor.
  PopulateHelveticaBoldFontDescriptorDictionary(ADictionary: TdxPDFDictionary);
begin
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFamily,
    TdxPDFKeywords.HelveticaFontName);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFlags, Integer(ffNonSymbolic) or
    Integer(ffSmallCap) or Integer(ffForceBold));
  ADictionary.Add(TdxPDFKeywords.FontDescriptorBBox,
    dxRectF(-170, -228, 1003, 962), False);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorItalicAngle, 0);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorAscent, 718);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorDescent, -207);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorCapHeight, 718);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorXHeight, 532);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorStemV, 140);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorStemH, 118);
end;

class procedure TdxPDFCustomFontDescriptor.
  PopulateHelveticaBoldObliqueFontDescriptorDictionary
  (ADictionary: TdxPDFDictionary);
begin
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFamily,
    TdxPDFKeywords.HelveticaFontName);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFlags, Integer(ffNonSymbolic) or
    Integer(ffItalic) or Integer(ffSmallCap) or Integer(ffForceBold));
  ADictionary.Add(TdxPDFKeywords.FontDescriptorBBox,
    dxRectF(-174, -228, 1114, 962), False);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorItalicAngle, -12);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorAscent, 718);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorDescent, -207);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorCapHeight, 718);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorXHeight, 532);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorStemV, 140);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorStemH, 118);
end;

class procedure TdxPDFCustomFontDescriptor.
  PopulateSymbolFontDescriptorDictionary(ADictionary: TdxPDFDictionary);
begin
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFamily,
    TdxPDFKeywords.SymbolFontName);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFlags, Integer(ffSymbolic));
  ADictionary.Add(TdxPDFKeywords.FontDescriptorBBox,
    dxRectF(-180, -293, 1090, 1010), False);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorItalicAngle, 0);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorAscent, 0);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorDescent, 0);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorCapHeight, 0);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorXHeight, 0);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorStemV, 85);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorStemH, 92);
end;

class procedure TdxPDFCustomFontDescriptor.
  PopulateTimesBoldFontDescriptorDictionary(ADictionary: TdxPDFDictionary);
begin
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFamily,
    TdxPDFKeywords.TimesFontFamilyName);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFlags, Integer(ffSerif) or
    Integer(ffNonSymbolic) or Integer(ffSmallCap) or Integer(ffForceBold));
  ADictionary.Add(TdxPDFKeywords.FontDescriptorBBox,
    dxRectF(-168, -218, 1000, 935), False);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorItalicAngle, 0);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorAscent, 683);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorDescent, -217);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorCapHeight, 676);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorXHeight, 461);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorStemV, 139);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorStemH, 44);
end;

class procedure TdxPDFCustomFontDescriptor.
  PopulateTimesBoldItalicFontDescriptorDictionary
  (ADictionary: TdxPDFDictionary);
begin
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFamily,
    TdxPDFKeywords.TimesFontFamilyName);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFlags, Integer(ffSerif) or
    Integer(ffNonSymbolic) or Integer(ffItalic) or Integer(ffSmallCap) or
    Integer(ffForceBold));
  ADictionary.Add(TdxPDFKeywords.FontDescriptorBBox,
    dxRectF(-200, -218, 996, 921), False);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorItalicAngle, -15);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorAscent, 683);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorDescent, -217);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorCapHeight, 669);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorXHeight, 462);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorStemV, 121);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorStemH, 42);
end;

class procedure TdxPDFCustomFontDescriptor.
  PopulateTimesItalicFontDescriptorDictionary(ADictionary: TdxPDFDictionary);
begin
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFamily,
    TdxPDFKeywords.TimesFontFamilyName);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFlags, Integer(ffSerif) or
    Integer(ffNonSymbolic) or Integer(ffItalic) or Integer(ffSmallCap));
  ADictionary.Add(TdxPDFKeywords.FontDescriptorBBox,
    dxRectF(-169, -217, 1010, 883), False);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorItalicAngle, -15.5);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorAscent, 683);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorDescent, -217);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorCapHeight, 653);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorXHeight, 441);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorStemV, 76);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorStemH, 32);
end;

class procedure TdxPDFCustomFontDescriptor.
  PopulateTimesNewRomanFontDescriptorDictionary(ADictionary: TdxPDFDictionary);
begin
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFamily,
    TdxPDFKeywords.TimesFontFamilyName);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFlags, Integer(ffSerif) or
    Integer(ffNonSymbolic) or Integer(ffSmallCap));
  ADictionary.Add(TdxPDFKeywords.FontDescriptorBBox,
    dxRectF(-168, -218, 1000, 898), False);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorItalicAngle, 0);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorAscent, 683);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorDescent, -217);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorCapHeight, 662);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorXHeight, 450);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorStemV, 84);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorStemH, 28);
end;

class procedure TdxPDFCustomFontDescriptor.
  PopulateZapfDingbatsFontDescriptorDictionary(ADictionary: TdxPDFDictionary);
begin
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFamily,
    TdxPDFKeywords.ZapfDingbatsFontName);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorFlags, Integer(ffSymbolic));
  ADictionary.Add(TdxPDFKeywords.FontDescriptorBBox,
    dxRectF(-1, -143, 981, 820), False);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorItalicAngle, 0);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorAscent, 0);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorDescent, 0);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorCapHeight, 0);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorXHeight, 0);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorStemV, 90);
  ADictionary.Add(TdxPDFKeywords.FontDescriptorStemH, 28);
end;

{ TdxPDFCIDSystemInfo }

class function TdxPDFCIDSystemInfo.GetTypeName: string;
begin
  Result := TdxPDFKeywords.CIDSystemInfo;
end;

procedure TdxPDFCIDSystemInfo.Initialize;
begin
  inherited Initialize;
  FRegistry := 'Adobe';
  FOrdering := 'Identity';
end;

procedure TdxPDFCIDSystemInfo.ReadProperties(ADictionary
  : TdxPDFReaderDictionary);
begin
  inherited ReadProperties(ADictionary);
  FRegistry := ADictionary.GetString(TdxPDFKeywords.Registry, FRegistry);
  FOrdering := ADictionary.GetString(TdxPDFKeywords.Ordering, FOrdering);
  FSupplement := ADictionary.GetInteger(TdxPDFKeywords.Supplement, FSupplement);
end;

procedure TdxPDFCIDSystemInfo.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  ADictionary.Add(TdxPDFKeywords.Registry, Registry);
  ADictionary.Add(TdxPDFKeywords.Ordering, Ordering);
  ADictionary.Add(TdxPDFKeywords.Supplement, Supplement);
end;

{ TdxPDFCustomFont }

constructor TdxPDFCustomFont.Create(AOwner: TObject);
begin
  inherited Create(AOwner);
  FUniqueName := TdxPDFUtils.GenerateGUID;
end;

class function TdxPDFCustomFont.Parse(AOwner: TdxPDFObject;
ADictionary: TdxPDFReaderDictionary): TdxPDFCustomFont;
var
  AType, ASubtype, ABaseFont: string;
  AClass: TdxPDFCustomFontClass;
  AFontDictionary: TdxPDFReaderDictionary;
  AHasBaseFontName, AHasSubtypeName: Boolean;
begin
  Result := nil;
  if (ADictionary <> nil) and not ADictionary.Repository.FontDataStorage.
    TryGetValue(ADictionary.Number, Result) then
  begin
    AType := ADictionary.GetString(TdxPDFKeywords.TypeKey);

    AHasSubtypeName := ADictionary.Contains(TdxPDFKeywords.Subtype);
    if AHasSubtypeName then
      ASubtype := ADictionary.GetString(TdxPDFKeywords.Subtype);

    AHasBaseFontName := ADictionary.Contains(TdxPDFKeywords.BaseFont);
    if AHasBaseFontName then
      ABaseFont := ADictionary.GetString(TdxPDFKeywords.BaseFont);

    AClass := nil;
    if AHasSubtypeName or AHasBaseFontName then
    begin
      if (ASubtype = TdxPDFType3Font.GetSubTypeName) or AHasBaseFontName then
      begin
        if (ASubtype = TdxPDFType0Font.GetSubTypeName) and AHasSubtypeName then
        begin
          AFontDictionary := TdxPDFType0Font.GetDictionary(ADictionary);
          if AFontDictionary <> nil then
            ASubtype := AFontDictionary.GetString(TdxPDFKeywords.Subtype)
          else
            ASubtype := '';
        end;
        if not dxPDFFontFactory.TryGetClass(ASubtype, AClass) then
          AClass := TdxPDFUnknownFont;
      end
      else
        TdxPDFUtils.RaiseTestException;
    end;

    if AClass <> nil then
    begin
      Result := AClass.Create(nil);
      Result.FBaseFont := ABaseFont;
      Result.Number := ADictionary.Number;
      Result.Read(ADictionary);
      ADictionary.Repository.FontDataStorage.Add(Result);
    end;
  end;
end;

procedure TdxPDFCustomFont.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FFontDescriptor := GetFontDescriptorClass.Create(Self);
  FListeners := TInterfaceList.Create;
end;

procedure TdxPDFCustomFont.DestroySubClasses;
var
  I: Integer;
begin
  for I := 0 to FListeners.Count - 1 do
    (FListeners[I] as IdxPDFDocumentSharedObjectListener).DestroyHandler(Self);
  FreeAndNil(FListeners);
  FreeAndNil(FCMap);
  FreeAndNil(FToUnicode);
  SetEncoding(nil);
  SetFontDescriptor(nil);
  FreeAndNil(FFontProgramFacade);
  inherited DestroySubClasses;
end;

procedure TdxPDFCustomFont.Initialize;
begin
  inherited Initialize;
  FMetrics := TdxPDFFontMetricsMetadata.Create;
end;

procedure TdxPDFCustomFont.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  ReadFontName;
  ReadEncoding(ADictionary.GetObject(TdxPDFKeywords.Encoding));
  ReadWidths(GetFontDictionary(ADictionary));
  ReadToUnicode(ADictionary);
  ReadFontDescriptor(GetFontDescriptorDictionary(ADictionary));
end;

procedure TdxPDFCustomFont.ReadProperties(ADictionary: TdxPDFReaderDictionary);
begin
  inherited ReadProperties(ADictionary);
  FBaseFont := ADictionary.GetString(TdxPDFKeywords.BaseFont);
end;

procedure TdxPDFCustomFont.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  inherited Write(AHelper, ADictionary);
  ADictionary.AddName(TdxPDFKeywords.Subtype, GetSubTypeName);
  ADictionary.AddName(TdxPDFKeywords.BaseFont, FBaseFont, False);
  ADictionary.Add(TdxPDFKeywords.Encoding, Encoding);
  if NeedWriteFontDescriptor then
    ADictionary.AddReference(TdxPDFKeywords.FontDescriptor, FontDescriptor);
  if (FToUnicode <> nil) then
    ADictionary.AddReference(TdxPDFKeywords.ToUnicode, FToUnicode.Data);
end;

class function TdxPDFCustomFont.GetSubTypeName: string;
begin
  Result := '';
end;

class function TdxPDFCustomFont.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Font;
end;

function TdxPDFCustomFont.GetFontDictionary(ADictionary: TdxPDFReaderDictionary)
  : TdxPDFReaderDictionary;
begin
  Result := ADictionary;
end;

function TdxPDFCustomFont.GetFontDescriptorDictionary
  (ADictionary: TdxPDFReaderDictionary): TdxPDFReaderDictionary;
begin
  Result := ADictionary.GetDictionary(TdxPDFKeywords.FontDescriptor);
end;

function TdxPDFCustomFont.CreateFontProgramFacade: TObject;
begin
  Result := nil;
end;

function TdxPDFCustomFont.CreateValidatedMetrics: TdxPDFFontMetricsMetadata;
var
  AAscent, ADescent: Double;
  AFontProgramFacade: TdxPDFFontProgramFacade;
begin
  AAscent := 0;
  ADescent := 0;
  AFontProgramFacade := FontProgramFacade as TdxPDFFontProgramFacade;
  if (FontDescriptor <> nil) and TdxPDFUtils.IsDoubleValid
    (AFontProgramFacade.Top) and TdxPDFUtils.IsDoubleValid
    (AFontProgramFacade.Bottom) then
  begin
    AAscent := AFontProgramFacade.Top;
    ADescent := AFontProgramFacade.Bottom;
  end;
  if (FontDescriptor <> nil) and ((AAscent = 0) or (ADescent = 0)) then
  begin
    AAscent := FontDescriptor.Ascent;
    ADescent := FontDescriptor.Descent;
    if AAscent - ADescent > 2048 then
    begin
      AAscent := AAscent / 2048.0 * 1000;
      ADescent := ADescent / 2048.0 * 1000;
    end;
  end;
  if not AFontProgramFacade.FontBBox.IsNull and ((AAscent = 0) or (ADescent = 0))
  then
  begin
    AAscent := AFontProgramFacade.FontBBox.Top;
    ADescent := AFontProgramFacade.FontBBox.Bottom;
  end;
  if (FontDescriptor <> nil) and ((AAscent = 0) or (ADescent = 0)) then
  begin
    AAscent := FontDescriptor.Ascent;
    ADescent := FontDescriptor.Descent;
  end;
  Result := TdxPDFFontMetricsMetadata.Create(AAscent, ADescent, 1000);
end;

function TdxPDFCustomFont.GetFontDescriptorClass
  : TdxPDFCustomFontDescriptorClass;
begin
  Result := TdxPDFCustomFontDescriptor;
end;

function TdxPDFCustomFont.GetHeightFactor: Double;
begin
  Result := GetWidthFactor;
end;

function TdxPDFCustomFont.GetWidthFactor: Double;
begin
  Result := 0.001;
end;

function TdxPDFCustomFont.HasSizeAttributes: Boolean;
begin
  Result := True;
end;

function TdxPDFCustomFont.NeedWriteFontDescriptor: Boolean;
begin
  Result := (FontDescriptor <> nil) and FontDescriptor.HasData;
end;

procedure TdxPDFCustomFont.DoReadWidths(ADictionary: TdxPDFReaderDictionary);
begin
  // do nothing
end;

procedure TdxPDFCustomFont.ReadEncoding(ASourceObject: TdxPDFBase);
begin
  SetEncoding(nil);
end;

procedure TdxPDFCustomFont.ReadFontDescriptor(ADictionary
  : TdxPDFReaderDictionary);
begin
  if ADictionary <> nil then
    FontDescriptor.Read(ADictionary);
end;

procedure TdxPDFCustomFont.ReadToUnicode(ADictionary: TdxPDFReaderDictionary);

  procedure DoRead(AStream: TdxPDFStream);
  begin
    FToUnicode := TdxPDFToUnicodeMap.Create(AStream.UncompressedData);
  end;

var
  AObject: TdxPDFBase;
begin
  AObject := ADictionary.GetObject(TdxPDFKeywords.ToUnicode);
  if (AObject <> nil) and (AObject.ObjectType <> otName) then
    case AObject.ObjectType of
      otStream:
        DoRead(TdxPDFStream(AObject));
      otIndirectReference:
        DoRead(Repository.GetStream(AObject.Number));
    end;
end;

procedure TdxPDFCustomFont.ReadWidths(ADictionary: TdxPDFReaderDictionary);

  function GetAvgGlyphWidth: SmallInt;
  var
    ASum, AWidth: Double;
    ACount: Integer;
  begin
    ASum := 0.0;
    ACount := 0;
    for AWidth in FWidths do
      if AWidth <> 0 then
      begin
        ASum := ASum + AWidth;
        Inc(ACount);
      end;
    if ACount <> 0 then
      Result := Ceil(ASum / ACount)
    else
      Result := 0;
  end;

begin
  DoReadWidths(ADictionary);
  FAvgGlyphWidth := GetAvgGlyphWidth;
end;

procedure TdxPDFCustomFont.AddListener(AListener
  : IdxPDFDocumentSharedObjectListener);
begin
  if FListeners.IndexOf(AListener) = -1 then
    FListeners.Add(AListener);
end;

procedure TdxPDFCustomFont.RemoveListener
  (AListener: IdxPDFDocumentSharedObjectListener);
begin
  FListeners.Remove(AListener);
end;

function TdxPDFCustomFont.GetBoldWeight: Integer;
begin
  Result := TdxPDFGDIFontSubstitutionHelper.BoldWeight;
end;

function TdxPDFCustomFont.GetCMap: TdxPDFCharacterMapping;
begin
  if (FCMap = nil) and (FToUnicode <> nil) then
    try
      FCMap := TdxPDFCMapStreamParser.Parse(Repository, FToUnicode.Data);
    except
    end;
  Result := FCMap;
end;

function TdxPDFCustomFont.GetItalic: Boolean;
begin
  Result := (FontDescriptor <> nil) and (FontDescriptor.ItalicAngle <> 0);
end;

function TdxPDFCustomFont.GetFontBBox: TdxRectF;
begin
  Result := FFontDescriptor.FontBBox;
end;

function TdxPDFCustomFont.GetFontProgramFacade: TObject;
begin
  if FFontProgramFacade = nil then
    FFontProgramFacade := CreateFontProgramFacade;
  Result := FFontProgramFacade;
end;

function TdxPDFCustomFont.GetForceBold: Boolean;
begin
  Result := (FontDescriptor <> nil) and HasFlag(ffForceBold);
end;

function TdxPDFCustomFont.GetMaxGlyphWidth: Double;
begin
  if Length(FWidths) > 0 then
    Result := MaxValue(FWidths)
  else
    Result := 0;
end;

function TdxPDFCustomFont.GetMetrics: TdxPDFFontMetricsMetadata;
begin
  if FMetrics.IsNull then
    FMetrics := CreateValidatedMetrics;
  Result := FMetrics;
end;

function TdxPDFCustomFont.GetPitchAndFamily: Byte;
begin
  Result := VARIABLE_PITCH;
  if FontDescriptor <> nil then
    if HasFlag(ffFixedPitch) then
      Result := FIXED_PITCH;
  if (Integer(FontDescriptor.Flags) and Integer(ffSerif)) > 0 then
    Result := Result or FF_ROMAN;
  if (Integer(FontDescriptor.Flags) and Integer(ffScript)) > 0 then
    Result := Result or FF_SCRIPT;
end;

function TdxPDFCustomFont.GetShouldUseEmbeddedFontEncoding: Boolean;
begin
  Result := (FEncoding <> nil) and FEncoding.ShouldUseEmbeddedFontEncoding;
end;

function TdxPDFCustomFont.GetSubsetNameLength: Integer;
begin
  Result := 6;
end;

function TdxPDFCustomFont.GetSubsetPrefixLength: Integer;
begin
  Result := GetSubsetNameLength + 1;
end;

function TdxPDFCustomFont.GetSymbolic: Boolean;
begin
  Result := HasFlag(ffSymbolic);
end;

function TdxPDFCustomFont.GetWeight: Integer;
begin
  if FontDescriptor = nil then
    Result := TdxPDFCustomFontDescriptor.GetNormalWeight
  else if ForceBold then
    Result := TdxPDFGDIFontSubstitutionHelper.BoldWeight
  else
    Result := FontDescriptor.FontWeight;
end;

procedure TdxPDFCustomFont.SetCMap(const AValue: TdxPDFCharacterMapping);
begin
  if FCMap <> nil then
    FreeAndNil(FCMap);
  FCMap := AValue;
end;

function TdxPDFCustomFont.HasFlag(AFlag: TdxFontFileFlags): Boolean;
begin
  Result := (FontDescriptor <> nil) and
    ((Integer(FontDescriptor.Flags) and Integer(AFlag)) = Integer(AFlag));
end;

procedure TdxPDFCustomFont.SetEncoding(const AValue: TdxPDFCustomEncoding);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FEncoding));
end;

procedure TdxPDFCustomFont.SetFontDescriptor(const AValue
  : TdxPDFCustomFontDescriptor);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FFontDescriptor));
end;

procedure TdxPDFCustomFont.ReadFontName;
var
  I, ASubsetPrefixLength, ASubsetNameLength: Integer;
begin
  ASubsetPrefixLength := GetSubsetPrefixLength;
  ASubsetNameLength := GetSubsetNameLength;
  if (Length(BaseFont) >= ASubsetPrefixLength) and
    (BaseFont[ASubsetNameLength + 1] = '+') then
  begin
    FSubsetName := Copy(BaseFont, 1, ASubsetNameLength);
    for I := 1 to Length(FSubsetName) do
      if FSubsetName[I] <> UpperCase(FSubsetName[I]) then
      begin
        FSubsetName := '';
        Break;
      end;
  end;
  if FSubsetName = '' then
    FName := BaseFont
  else
    FName := Copy(BaseFont, ASubsetPrefixLength + 1, MaxInt);
end;

{ TdxPDFCustomColorSpace }

class function TdxPDFCustomColorSpace.CreateColorSpace(const AName: string;
AResources: TdxPDFResources): TdxPDFCustomColorSpace;
var
  AClass: TdxPDFObjectClass;
begin
  Result := nil;
  if dxPDFTryGetDocumentObjectClass(AName, AClass) then
    Result := AClass.Create(nil) as TdxPDFCustomColorSpace
  else if AResources <> nil then
    Result := AResources.GetColorSpace(AName);
end;

class function TdxPDFCustomColorSpace.Parse(ARepository
  : TdxPDFDocumentRepository; AObject: TdxPDFBase; AResources: TdxPDFResources)
  : TdxPDFCustomColorSpace;
var
  AAttributesDictionary: TdxPDFDictionary;
  AParameters: TdxPDFBase;
  AReference: TdxPDFReference;
  AArray: TdxPDFArray;
  ATempName: string;
  AName: TdxPDFName;
  AObjectClass: TdxPDFObjectClass;
begin
  Result := nil;
  case AObject.ObjectType of
    otIndirectReference:
      begin
        AReference := AObject as TdxPDFReference;
        AParameters := ARepository.GetObject(AReference.Number) as TdxPDFBase;
        Result := Parse(ARepository, AParameters, AResources);
      end;
    otString, otName:
      Result := CreateColorSpace(TdxPDFName(AObject).Value, AResources);
  else
    begin
      AArray := AObject as TdxPDFArray;
      if AArray[0].ObjectType = otName then
      begin
        AName := AArray[0] as TdxPDFName;
        AObjectClass := dxPDFGetDocumentObjectClass(AName.Value);
        if AArray.Count = 5 then
        begin
          if AArray[4].ObjectType <> otDictionary then
          begin
            AAttributesDictionary := ARepository.GetDictionary
              ((AArray[4] as TdxPDFReference).Number);
            if AAttributesDictionary = nil then
              TdxPDFUtils.RaiseTestException;
          end
          else
            AAttributesDictionary := AArray[4] as TdxPDFDictionary;
          ATempName := AAttributesDictionary.GetString(TdxPDFKeywords.Subtype);
          if ATempName = '' then
            AObjectClass := TdxPDFDeviceNColorSpace
          else
            AObjectClass := dxPDFGetDocumentObjectClass(ATempName);
        end;
        if AObjectClass <> nil then
        begin
          Result := AObjectClass.Create(nil) as TdxPDFCustomColorSpace;
          Result.Repository := ARepository;
          Result.DoRead(AArray);
        end
        else
          TdxPDFUtils.RaiseTestException('Unknown color space:' + AName.Value);
      end
      else
        TdxPDFUtils.RaiseTestException;
    end;
  end;
end;

procedure TdxPDFCustomColorSpace.Read(ADictionary: TdxPDFReaderDictionary);
var
  AArray: TdxPDFArray;
begin
  inherited Read(ADictionary);
  if (ADictionary <> nil) and ADictionary.TryGetArray(Name, AArray) and
    CanRead(AArray.Count) then
    DoRead(AArray)
end;

function TdxPDFCustomColorSpace.Write(AHelper: TdxPDFWriterHelper): TdxPDFBase;
var
  AArray: TdxPDFWriterArray;
begin
  AArray := AHelper.CreateArray;
  DoWrite(AHelper, AArray);
  Result := AArray;
end;

procedure TdxPDFCustomColorSpace.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  RaiseWriteNotImplementedException;
end;

procedure TdxPDFCustomColorSpace.DoRead(AArray: TdxPDFArray);
begin
  // do nothing
end;

procedure TdxPDFCustomColorSpace.DoWrite(AHelper: TdxPDFWriterHelper;
AArray: TdxPDFWriterArray);
begin
  AArray.AddName(GetTypeName);
end;

function TdxPDFCustomColorSpace.GetDecodedImageScanlineSource
  (const ADecoratingSource: IdxPDFImageScanlineSource;
const AImage: IdxPDFDocumentImage; AWidth: Integer): IdxPDFImageScanlineSource;
var
  ADecodeArray, ADefaultDecodeArray: TdxPDFRanges;
  ASize, I: Integer;
begin
  ADecodeArray := AImage.GetDecodeRanges;
  if ADecodeArray <> nil then
  begin
    ADefaultDecodeArray := CreateDefaultDecodeArray(AImage.GetBitsPerComponent);
    ASize := Length(ADefaultDecodeArray);
    if ASize = Length(ADecodeArray) then
      for I := 0 to ASize - 1 do
        if not ADefaultDecodeArray[I].IsSame(ADecodeArray[I]) then
          Exit(TdxPDFDecodeImageScanlineSource.Create(ADecodeArray, AWidth,
            ADecoratingSource));
  end;
  Result := ADecoratingSource;
end;

procedure TdxPDFCustomColorSpace.CreateSubClasses;
begin
  inherited CreateSubClasses;
  AlternateColorSpace := nil;
end;

procedure TdxPDFCustomColorSpace.DestroySubClasses;
begin
  AlternateColorSpace := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFCustomColorSpace.Initialize;
begin
  inherited Initialize;
  FComponentCount := GetComponentCount;
end;

function TdxPDFCustomColorSpace.GetComponentCount: Integer;
begin
  Result := FComponentCount;
end;

function TdxPDFCustomColorSpace.CanRead(ASize: Integer): Boolean;
begin
  Result := True;
end;

procedure TdxPDFCustomColorSpace.CheckComponentCount;
begin
  if not(ComponentCount in [1, 3, 4]) then
    TdxPDFUtils.RaiseTestException('Invalid color space component count');
end;

function TdxPDFCustomColorSpace.AlternateTransform(const AColor: TdxPDFColor)
  : TdxPDFColor;
begin
  Result := Transform(AColor);
end;

function TdxPDFCustomColorSpace.CreateDefaultDecodeArray(ABitsPerComponent
  : Integer): TdxPDFRanges;
var
  I: Integer;
begin
  SetLength(Result, ComponentCount);
  for I := 0 to ComponentCount - 1 do
    Result[I] := TdxPDFRange.Create(0, 1);
end;

function TdxPDFCustomColorSpace.Transform(const AComponents: TDoubleDynArray)
  : TDoubleDynArray;
begin
  Result := AComponents;
end;

function TdxPDFCustomColorSpace.Transform(const AColor: TdxPDFColor)
  : TdxPDFColor;
begin
  Result := TdxPDFColor.Create(AColor.Pattern, Transform(AColor.Components));
end;

function TdxPDFCustomColorSpace.Transform(const AImage: IdxPDFDocumentImage;
const AData: IdxPDFImageScanlineSource;
const AParameters: TdxPDFImageParameters): TdxPDFScanlineTransformationResult;
var
  ATransformationResult: TdxPDFScanlineTransformationResult;
begin
  if (AParameters.Width > AImage.GetWidth) and
    (AParameters.Height > AImage.GetHeight) then
  begin
    ATransformationResult := Transform(GetDecodedImageScanlineSource(AData,
      AImage, AImage.GetWidth), AImage.GetWidth);
    Result := TdxPDFScanlineTransformationResult.Create
      (AImage.GetInterpolatedScanlineSource
      (ATransformationResult.ScanlineSource, AParameters),
      ATransformationResult.PixelFormat);
  end
  else
    Result := Transform(GetDecodedImageScanlineSource
      (AImage.GetInterpolatedScanlineSource(AData, AParameters), AImage,
      AParameters.Width), AParameters.Width);
  dxTestCheck(not Result.IsNull, 'ColorSpace transformation result is null');
end;

function TdxPDFCustomColorSpace.Transform(const AData
  : IdxPDFImageScanlineSource; AWidth: Integer)
  : TdxPDFScanlineTransformationResult;
begin
  Result := TdxPDFScanlineTransformationResult.Null;
end;

procedure TdxPDFCustomColorSpace.SetAlternateColorSpace
  (const AValue: TdxPDFCustomColorSpace);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FAlternateColorSpace));
end;

procedure TdxPDFCustomColorSpace.SetComponentCount(const AValue: Integer);
begin
  if FComponentCount <> AValue then
  begin
    FComponentCount := AValue;
    CheckComponentCount;
  end;
end;

{ TdxPDFDeferredObject }

constructor TdxPDFDeferredObject.Create(AOwner: TdxPDFObject;
const AInfo: TdxPDFDeferredObjectInfo);
begin
  inherited Create(AOwner);
  SourceObject := AInfo.SourceObject;
  if AOwner <> nil then
    Repository := AOwner.Repository;
  FInfo.Name := AInfo.Name;
  FInfo.Key := AInfo.Key;
  FInfo.Number := AInfo.Number;
end;

constructor TdxPDFDeferredObject.Create(AOwner, AResolvedObject: TdxPDFObject);
begin
  inherited Create(AOwner);
  ResolvedObject := AResolvedObject;
  FInfo.Number := FResolvedObject.Number;
  FInfo.Key := FResolvedObject.GetTypeName;
end;

procedure TdxPDFDeferredObject.CreateSubClasses;
begin
  inherited CreateSubClasses;
  SourceObject := nil;
  ResolvedObject := nil;
end;

procedure TdxPDFDeferredObject.DestroySubClasses;
begin
  ResolvedObject := nil;
  SourceObject := nil;
  inherited DestroySubClasses;
end;

function TdxPDFDeferredObject.IsResolved: Boolean;
begin
  Result := FResolvedObject <> nil;
end;

procedure TdxPDFDeferredObject.ResolveObject;
var
  ADictionary: TdxPDFReaderDictionary;
begin
  ADictionary := nil;
  if SourceObject <> nil then
    case SourceObject.ObjectType of
      otDictionary:
        ADictionary := SourceObject as TdxPDFReaderDictionary;
      otIndirectReference:
        ADictionary := Repository.GetDictionary(SourceObject.Number);
    else
      ADictionary := nil;
    end;
  if FInfo.Key = TdxPDFCustomDestination.GetTypeName then
    ResolvedObject := TdxPDFCustomDestination.Parse(Repository.Catalog,
      SourceObject)
  else if ADictionary <> nil then
  begin
    ADictionary.Number := FInfo.Number;
    if FInfo.Key = TdxPDFColorSpaces.GetTypeName then
      ResolvedObject := TdxPDFCustomColorSpace.Parse(Repository,
        ADictionary.GetObject(FInfo.Name))
    else if FInfo.Key = TdxPDFPatterns.GetTypeName then
      ResolvedObject := TdxPDFCustomPattern.Parse(Repository,
        ADictionary.GetObject(FInfo.Name))
    else if FInfo.Key = TdxPDFShadings.GetTypeName then
      ResolvedObject := TdxPDFCustomShading.Parse(Repository,
        ADictionary.GetObject(FInfo.Name))
    else if FInfo.Key = TdxPDFCustomFont.GetTypeName then
    begin
      ADictionary := ADictionary.GetDictionary(FInfo.Name);
      ADictionary.Number := FInfo.Number;
      ResolvedObject := Repository.CreateFont(Parent as TdxPDFObject,
        ADictionary);
    end
    else if FInfo.Key = TdxPDFGraphicsStateParametersList.GetTypeName then
      ResolvedObject := TdxPDFGraphicsStateParameters.Parse
        (ADictionary.GetDictionary(FInfo.Name))
    else if ADictionary.GetString(FInfo.Key) = TdxPDFForm.GetTypeName then
      ResolvedObject := Repository.CreateForm(ADictionary)
    else if ADictionary.GetString(FInfo.Key) = TdxPDFDocumentImage.GetTypeName
    then
      ResolvedObject := Repository.CreateImage(Parent as TdxPDFObject,
        ADictionary)
    else if FInfo.Key = TdxPDFFileSpecification.GetTypeName then
      ResolvedObject := TdxPDFFileSpecification.Parse(ADictionary)
    else
    begin
      ResolvedObject := dxPDFCreateDocumentObject(Parent as TdxPDFObject,
        ADictionary, FInfo.Key, Repository);
      if IsResolved then
        ResolvedObject.Read(ADictionary);
    end;
  end;
  if IsResolved then
    ResolvedObject.Parent := Parent;
end;

function TdxPDFDeferredObject.GetResolvedObject: TdxPDFObject;
begin
  if FResolvedObject = nil then
    ResolveObject;
  Result := FResolvedObject;
end;

function TdxPDFDeferredObject.GetSourceObject: TdxPDFBase;
begin
  Result := FInfo.SourceObject;
end;

procedure TdxPDFDeferredObject.SetResolvedObject(const AValue: TdxPDFObject);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FResolvedObject));
end;

procedure TdxPDFDeferredObject.SetSourceObject(const AValue: TdxPDFBase);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FInfo.SourceObject));
end;

{ TdxPDFGraphicsStateParameters }

class function TdxPDFGraphicsStateParameters.GetTypeName: string;
begin
  Result := TdxPDFKeywords.ExtGState;
end;

class function TdxPDFGraphicsStateParameters.Parse
  (ADictionary: TdxPDFReaderDictionary): TdxPDFGraphicsStateParameters;
begin
  Result := TdxPDFGraphicsStateParameters.Create(nil);
  Result.Read(ADictionary);
end;

procedure TdxPDFGraphicsStateParameters.Assign(AParameters
  : TdxPDFGraphicsStateParameters; ACheckAssignedValues: Boolean = True);
begin
  if not ACheckAssignedValues or
    (gspStrokingColorAlpha in AParameters.AssignedValues) then
    StrokingColorAlpha := AParameters.StrokingColorAlpha;
  if not ACheckAssignedValues or
    (gspNonStrokingColorAlpha in AParameters.AssignedValues) then
    NonStrokingColorAlpha := AParameters.NonStrokingColorAlpha;
  if not ACheckAssignedValues or (gspLineCapStyle in AParameters.AssignedValues)
  then
    LineCapStyle := AParameters.LineCapStyle;
  if not ACheckAssignedValues or (gspLineJoinStyle in AParameters.AssignedValues)
  then
    LineJoinStyle := AParameters.LineJoinStyle;
  if not ACheckAssignedValues or (gspLineStyle in AParameters.AssignedValues)
  then
    LineStyle := AParameters.LineStyle;
  if not ACheckAssignedValues or (gspLineWidth in AParameters.AssignedValues)
  then
    LineWidth := AParameters.LineWidth;
  if not ACheckAssignedValues or (gspMiterLimit in AParameters.AssignedValues)
  then
    MiterLimit := AParameters.MiterLimit;
  if not ACheckAssignedValues or
    (gspSmoothnessTolerance in AParameters.AssignedValues) then
    SmoothnessTolerance := AParameters.SmoothnessTolerance;
  if not ACheckAssignedValues or (gspTextKnockout in AParameters.AssignedValues)
  then
    TextKnockout := AParameters.TextKnockout;
  if not ACheckAssignedValues or
    (gspRenderingIntent in AParameters.AssignedValues) then
    RenderingIntent := AParameters.RenderingIntent;
  FIsSoftMaskChanged := not ACheckAssignedValues or
    (gspSoftMask in AParameters.AssignedValues) and
    not SoftMask.IsSame(AParameters.SoftMask);
  if FIsSoftMaskChanged then
    SoftMask := AParameters.SoftMask;
  if not ACheckAssignedValues or (gspBlendMode in AParameters.AssignedValues)
  then
    BlendMode := AParameters.BlendMode;
end;

procedure TdxPDFGraphicsStateParameters.SetFont(const AValue: TdxPDFCustomFont);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FFont));
end;

procedure TdxPDFGraphicsStateParameters.SetLineStyle(const AValue
  : TdxPDFLineStyle);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FLineStyle));
end;

procedure TdxPDFGraphicsStateParameters.SetLineWidth(const AValue: Double);
begin
  FLineWidth := AValue;
  Include(FAssignedValues, gspLineWidth);
end;

procedure TdxPDFGraphicsStateParameters.SetSoftMask(const AValue
  : TdxPDFCustomSoftMask);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FSoftMask));
end;

procedure TdxPDFGraphicsStateParameters.CreateSubClasses;
begin
  inherited CreateSubClasses;
  LineStyle := nil;
  SoftMask := TdxPDFEmptySoftMask.Create;
end;

procedure TdxPDFGraphicsStateParameters.DestroySubClasses;
begin
  SoftMask := nil;
  LineStyle := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFGraphicsStateParameters.Initialize;
begin
  inherited Initialize;
  FFlatnessTolerance := 1.0;
  FLineWidth := 1;
  FLineCapStyle := lcsButt;
  FMiterLimit := 10.0;
  FNonStrokingColorAlpha := 1.0;
  FTextKnockout := True;
  FStrokingColorAlpha := 1.0;
  FSmoothnessTolerance := 0.0;
  FRenderingIntent := riRelativeColorimetric;
  FBlendMode := bmNormal;
end;

procedure TdxPDFGraphicsStateParameters.ReadProperties
  (ADictionary: TdxPDFReaderDictionary);
begin
  inherited ReadProperties(ADictionary);
  FAssignedValues := [];

  if ADictionary.Contains(TdxPDFKeywords.LineCap) then
  begin
    LineCapStyle := TdxPDFLineCapStyle
      (ADictionary.GetInteger(TdxPDFKeywords.LineCap));
    Include(FAssignedValues, gspLineCapStyle);
  end;
  if ADictionary.Contains(TdxPDFKeywords.LineJoinStyle) then
  begin
    LineJoinStyle := TdxPDFLineJoinStyle
      (ADictionary.GetInteger(TdxPDFKeywords.LineJoinStyle));
    Include(FAssignedValues, gspLineJoinStyle);
  end;
  if ADictionary.Contains(TdxPDFKeywords.LineStyle) then
  begin
    LineStyle := TdxPDFLineStyle.Create
      (ADictionary.GetArray(TdxPDFKeywords.LineStyle));
    Include(FAssignedValues, gspLineStyle);
  end;
  if ADictionary.Contains(TdxPDFKeywords.LineWidth) then
  begin
    LineWidth := ADictionary.GetInteger(TdxPDFKeywords.LineWidth);
    Include(FAssignedValues, gspLineWidth);
  end;
  if ADictionary.Contains(TdxPDFKeywords.FlatnessTolerance) then
  begin
    FlatnessTolerance := ADictionary.GetDouble
      (TdxPDFKeywords.FlatnessTolerance);
    Include(FAssignedValues, gspFlatnessTolerance);
  end;
  if ADictionary.Contains(TdxPDFKeywords.SmoothnessTolerance) then
  begin
    SmoothnessTolerance := ADictionary.GetDouble
      (TdxPDFKeywords.SmoothnessTolerance);
    Include(FAssignedValues, gspSmoothnessTolerance);
  end;
  if ADictionary.Contains(TdxPDFKeywords.StrokingColorAlpha) then
  begin
    StrokingColorAlpha := ADictionary.GetDouble
      (TdxPDFKeywords.StrokingColorAlpha);
    Include(FAssignedValues, gspStrokingColorAlpha);
  end;
  if ADictionary.Contains(TdxPDFKeywords.NonStrokingColorAlpha) then
  begin
    NonStrokingColorAlpha := ADictionary.GetDouble
      (TdxPDFKeywords.NonStrokingColorAlpha);
    Include(FAssignedValues, gspNonStrokingColorAlpha);
  end;
  if ADictionary.Contains(TdxPDFKeywords.MiterLimit) then
  begin
    MiterLimit := ADictionary.GetInteger(TdxPDFKeywords.MiterLimit);
    Include(FAssignedValues, gspMiterLimit);
  end;
  if ADictionary.Contains(TdxPDFKeywords.TextKnockout) then
  begin
    TextKnockout := ADictionary.GetBoolean(TdxPDFKeywords.TextKnockout);
    Include(FAssignedValues, gspTextKnockout);
  end;
  if ADictionary.Contains(TdxPDFKeywords.BlendMode) then
  begin
    BlendMode := TdxPDFBlendModeDictionary.ToValue
      (ADictionary.GetString(TdxPDFKeywords.BlendMode));
    Include(FAssignedValues, gspBlendMode);
  end;
  if ADictionary.Contains(TdxPDFKeywords.SoftMask) then
  begin
    SoftMask := TdxPDFCustomSoftMask.Parse(Repository,
      ADictionary.GetObject(TdxPDFKeywords.SoftMask));
    Include(FAssignedValues, gspSoftMask);
  end;
end;

procedure TdxPDFGraphicsStateParameters.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  inherited;

  if gspLineCapStyle in AssignedValues then
    ADictionary.Add(TdxPDFKeywords.LineCap, Ord(LineCapStyle));
  if gspLineJoinStyle in AssignedValues then
    ADictionary.Add(TdxPDFKeywords.LineJoinStyle, Ord(LineJoinStyle));
  if gspLineStyle in AssignedValues then
    ADictionary.Add(TdxPDFKeywords.LineStyle, LineStyle.Write);
  if gspLineWidth in AssignedValues then
    ADictionary.Add(TdxPDFKeywords.LineWidth, LineWidth);
  if gspFlatnessTolerance in AssignedValues then
    ADictionary.Add(TdxPDFKeywords.FlatnessTolerance, FlatnessTolerance);
  if gspSmoothnessTolerance in AssignedValues then
    ADictionary.Add(TdxPDFKeywords.SmoothnessTolerance, SmoothnessTolerance);
  if gspStrokingColorAlpha in AssignedValues then
    ADictionary.Add(TdxPDFKeywords.StrokingColorAlpha, StrokingColorAlpha);
  if gspNonStrokingColorAlpha in AssignedValues then
    ADictionary.Add(TdxPDFKeywords.NonStrokingColorAlpha,
      NonStrokingColorAlpha);
  if gspMiterLimit in AssignedValues then
    ADictionary.Add(TdxPDFKeywords.MiterLimit, MiterLimit);
  if gspTextKnockout in AssignedValues then
    ADictionary.Add(TdxPDFKeywords.TextKnockout, TextKnockout);
  if gspBlendMode in AssignedValues then
    ADictionary.AddName(TdxPDFKeywords.BlendMode,
      TdxPDFBlendModeDictionary.ToString(BlendMode));
  if gspSoftMask in AssignedValues then
    ADictionary.Add(TdxPDFKeywords.SoftMask, SoftMask);
end;

{ TdxPDFObjectList }

function TdxPDFObjectList.Add(AObject: TdxPDFObject): string;
begin
  Result := '';
  if not FNames.ContainsValue(AObject.Number) then
  begin
    Result := FNames.GetNewResourceName(InternalObjects);
    InternalAdd(Result, AObject);
  end;
end;

function TdxPDFObjectList.AddReference(ANumber: Integer): string;
begin
  Result := FNames.GetNewResourceName(InternalObjects);
  InternalObjects.Add(Result, TdxPDFReference.Create(ANumber, 0));
end;

procedure TdxPDFObjectList.Append(AList: TdxPDFObjectList);
begin
  DoAdd(AList, False);
end;

procedure TdxPDFObjectList.Assign(AList: TdxPDFObjectList);
begin
  if AList is TdxPDFObjectList then
    DoAdd(AList, True);
end;

function TdxPDFObjectList.Contains(const AName: string): Boolean;
begin
  Result := InternalObjects.ContainsKey(AName);
end;

procedure TdxPDFObjectList.Enum(AProc: TEnumProc);
var
  AKey: string;
  AObject: TdxPDFObject;
begin
  for AKey in InternalObjects.Items.Keys do
  begin
    AObject := GetObject(AKey);
    if AObject <> nil then
      AProc(AObject);
  end;
end;

procedure TdxPDFObjectList.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FInternalObjects := TdxPDFReferencedObjectDictionary.Create;
  FNames := TdxPDFNamedObjectDictionary.Create(GetTypeName, GetTypePrefix);
end;

procedure TdxPDFObjectList.DestroySubClasses;
begin
  FreeAndNil(FNames);
  FreeAndNil(FInternalObjects);
  inherited DestroySubClasses;
end;

procedure TdxPDFObjectList.Read(ADictionary: TdxPDFReaderDictionary);
var
  AName: string;
  ANumber: Integer;
begin
  inherited Read(ADictionary);
  Clear;
  if ADictionary <> nil then
    for AName in TdxPDFDictionaryAccess(ADictionary).Items.Keys do
    begin
      ANumber := ADictionary.GetObjectNumber(AName);
      if TdxPDFUtils.IsIntegerValid(ANumber) then
        ReadObject(ADictionary.GetObject(AName), ANumber, AName);
    end;
end;

procedure TdxPDFObjectList.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  WriteList(AHelper, ADictionary);
end;

function TdxPDFObjectList.GetObject(const AName: string): TdxPDFObject;
var
  AResult: TdxPDFReferencedObject;
begin
  if InternalObjects.TryGetValue(AName, AResult) then
  begin
    if AResult is TdxPDFDeferredObject then
      Result := TdxPDFDeferredObject(AResult).ResolvedObject
    else
      Result := AResult as TdxPDFObject
  end
  else
    Result := nil;
end;

function TdxPDFObjectList.TryGetObjectName(const AObject: TdxPDFObject;
out AName: string): Boolean;
var
  APair: TPair<string, TdxPDFReferencedObject>;
begin
  Result := False;
  for APair in InternalObjects.Items do
  begin
    Result := AObject = APair.Value;
    if Result then
    begin
      AName := APair.Key;
      Break;
    end;
  end;
end;

class function TdxPDFObjectList.GetTypePrefix: string;
begin
  Result := 'ResourcePrefix';
end;

function TdxPDFObjectList.GetTypeDictionaryKey: string;
begin
  Result := TdxPDFKeywords.Subtype;
end;

procedure TdxPDFObjectList.ReadObject(AStructureObject: TdxPDFBase;
ANumber: Integer; const AName: string);
var
  AShareObject: TdxPDFBase;
begin
  if AStructureObject <> nil then
  begin
    AShareObject := AStructureObject;
    AShareObject.Number := ANumber;
    AStructureObject := Repository.GetDictionary(AStructureObject.Number);
    if (AStructureObject = nil) and (AShareObject.ObjectType = otDictionary)
    then
    begin
      if dxPDFIsObjectSupported(Self, AShareObject as TdxPDFDictionary,
        GetTypeDictionaryKey, Repository) then
        DoReadObject(AName, AShareObject as TdxPDFReaderDictionary);
    end
    else if AStructureObject <> nil then
    begin
      AStructureObject.Number := AShareObject.Number;
      DoReadObject(AName, AStructureObject as TdxPDFReaderDictionary);
    end;
  end;
end;

function TdxPDFObjectList.GetCount: Integer;
begin
  Result := InternalObjects.Count;
end;

procedure TdxPDFObjectList.DoAdd(AObjectList: TdxPDFObjectList;
ANeedClear: Boolean);
var
  APair: TPair<string, TdxPDFReferencedObject>;
begin
  if AObjectList is TdxPDFObjectList then
  begin
    if ANeedClear then
      Clear;
    for APair in TdxPDFReferencedObjectDictionaryAccess
      (AObjectList.InternalObjects).Items do
    begin
      if InternalObjects.ContainsKey(APair.Key) then
        InternalObjects.Remove(APair.Key);
      InternalObjects.Add(APair.Key, APair.Value);
    end;
  end;
end;

procedure TdxPDFObjectList.DoReadObject(const AObjectName: string;
ADictionary: TdxPDFReaderDictionary);
var
  AInfo: TdxPDFDeferredObjectInfo;
  AItem: TdxPDFDeferredObject;
begin
  AInfo.Name := AObjectName;
  AInfo.Key := GetTypeDictionaryKey;
  AInfo.Number := ADictionary.Number;
  AInfo.SourceObject := ADictionary;
  AItem := TdxPDFDeferredObject.Create(Parent as TdxPDFObject, AInfo);
  InternalAdd(AObjectName, AItem);
end;

procedure TdxPDFObjectList.Clear;
begin
  InternalObjects.Clear;
end;

procedure TdxPDFObjectList.InternalAdd(const AName: string;
AObject: TdxPDFObject);
begin
  InternalObjects.Add(AName, AObject);
end;

procedure TdxPDFObjectList.ReadList(ADictionary: TdxPDFReaderDictionary);
var
  AInfo: TdxPDFDeferredObjectInfo;
  APair: TPair<string, TdxPDFReferencedObject>;
begin
  if ADictionary <> nil then
  begin
    AInfo.Key := GetTypeName;
    AInfo.SourceObject := ADictionary;
    AInfo.Number := ADictionary.Number;
    for APair in TdxPDFDictionaryAccess(ADictionary).Items do
    begin
      AInfo.Name := APair.Key;
      AInfo.Number := (APair.Value as TdxPDFBase).Number;
      InternalAdd(APair.Key, TdxPDFDeferredObject.Create
        (Parent as TdxPDFObject, AInfo));
    end;
  end;
end;

procedure TdxPDFObjectList.WriteList(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
var
  AKey: string;
  AObject: TdxPDFObject;
begin
  for AKey in InternalObjects.Items.Keys do
  begin
    AObject := GetObject(AKey);
    if AObject <> nil then
      ADictionary.AddReference(AKey, AObject);
  end;
end;

procedure TdxPDFObjectList.ResolveObjects;
var
  AObject: TdxPDFReferencedObject;
begin
  for AObject in InternalObjects.Items.Values do
    if AObject is TdxPDFDeferredObject then
      TdxPDFDeferredObject(AObject).ResolvedObject;
end;

{ TdxPDFDocumentImageDataStorage }

constructor TdxPDFDocumentImageDataStorage.Create(ALimit: Int64);
begin
  inherited Create;
  FCache := TdxPDFImageDataCache.Create(ALimit * 1024 * 1024);
  FImageList := TList<TdxPDFDocumentImage>.Create;
  FReferences := TdxPDFUniqueReferences.Create;
end;

destructor TdxPDFDocumentImageDataStorage.Destroy;
begin
  Clear;
  FreeAndNil(FCache);
  FreeAndNil(FImageList);
  FreeAndNil(FReferences);
  inherited Destroy;
end;

procedure TdxPDFDocumentImageDataStorage.Clear;
var
  AImage: TdxPDFDocumentImage;
begin
  FCache.Clear;
  for AImage in FImageList do
    RemoveListener(AImage);
  FImageList.Clear;
  FReferences.Clear;
end;

function TdxPDFDocumentImageDataStorage.GetImage(AImage: TdxPDFDocumentImage;
const AImageParameters: TdxPDFImageParameters): TdxPDFImageCacheItem;
begin
  Result := FCache.GetImage(AImage, AImageParameters);
end;

procedure TdxPDFDocumentImageDataStorage.Add(AImage: TdxPDFDocumentImage);
begin
  AImage.AddListener(Self);
  FImageList.Add(AImage);
  AddReference(AImage);
end;

function TdxPDFDocumentImageDataStorage.TryGetReference(ANumber: Integer;
out AImage: TdxPDFDocumentImage): Boolean;
var
  AObject: TdxPDFBase;
begin
  AImage := nil;
  Result := FReferences.TryGetValue(ANumber, AObject);
  if Result then
    AImage := AObject as TdxPDFDocumentImage;
end;

procedure TdxPDFDocumentImageDataStorage.AddReference
  (AImage: TdxPDFDocumentImage);
begin
  if not FReferences.ContainsKey(AImage.GUID, AImage.Number) then
    FReferences.Add(AImage.GUID, AImage.Number, AImage);
end;

procedure TdxPDFDocumentImageDataStorage.RemoveListener
  (AImage: TdxPDFDocumentImage);
begin
  AImage.RemoveListener(Self);
end;

procedure TdxPDFDocumentImageDataStorage.ImageDestroyHandler
  (Sender: TdxPDFBase);
var
  AImage: TdxPDFDocumentImage;
begin
  AImage := Sender as TdxPDFDocumentImage;
  FCache.RemoveItem(AImage);
  FImageList.Remove(AImage);
end;

{ TdxFontFamilyInfo }

constructor TdxFontFamilyInfo.Create;
begin
  inherited Create;
  FAdditionalStyles := TdxPDFStringStringDictionary.Create;
end;

constructor TdxFontFamilyInfo.Create(const ASystemFontName: string);
begin
  Create;
  FSystemFontName := ASystemFontName;
end;

destructor TdxFontFamilyInfo.Destroy;
begin
  FreeAndNil(FAdditionalStyles);
  inherited Destroy;
end;

{ TdxFontFamilyInfos }

constructor TdxFontFamilyInfos.Create;
begin
  inherited Create;
  FFamilies := nil;
  FInstalledFontCollection := TdxGPInstalledFontCollection.Create;
  FInfos := TObjectDictionary<string, TdxFontFamilyInfo>.Create([doOwnsValues]);
  FStylePattern := TdxPDFUtils.Split
    ('semibold|semilight|demi|light|black|bold|italic|oblique|md|sb|bd|it|scn|mt',
    '|');
  FAdditionalStylePattern := TdxPDFUtils.Split
    ('semibold|semilight|demibold|demi|light|black|md|bd|italic|sb|scn', '|');
  PopulateInfos;
end;

destructor TdxFontFamilyInfos.Destroy;
begin
  FreeAndNil(FInfos);
  FreeAndNil(FInstalledFontCollection);
  FreeAndNil(FFamilies);
  inherited Destroy;
end;

function TdxFontFamilyInfos.Contains(const AFamily: string): Boolean;
begin
  Result := Families.IndexOf(AFamily) > -1;
end;

function TdxFontFamilyInfos.Normalize(const AName: string): string;
begin
  Result := StringReplace(AName, ' ', '', [rfReplaceAll]);
  Result := StringReplace(Result, '-', '', [rfReplaceAll]);
  Result := LowerCase(StringReplace(Result, ',', '', [rfReplaceAll]));
end;

function TdxFontFamilyInfos.MatchPattern(const S: string;
const APattern: string): string;
begin
  Result := MatchPattern(S, TdxPDFUtils.Split(APattern, '|'));
end;

function TdxFontFamilyInfos.MatchPattern(const S: string;
const APatternArray: TStringDynArray): string;
var
  ATemp, APattern: string;
begin
  Result := '';
  ATemp := LowerCase(S);
  for APattern in APatternArray do
    if Pos(APattern, ATemp) > 0 then
    begin
      ATemp := StringReplace(ATemp, APattern, '', [rfReplaceAll]);
      Result := Result + APattern;
    end;
end;

function TdxFontFamilyInfos.RemovePattern(const S: string;
const APatternArray: TStringDynArray): string;
var
  I, L: Integer;
  APattern: string;
begin
  Result := S;
  for APattern in APatternArray do
    Result := StringReplace(Result, APattern, '', [rfReplaceAll, rfIgnoreCase]);
  L := Length(Result);
  for I := L downto 1 do
    if not CharInSet(Result[I], [' ', '-', ',']) then
    begin
      if I <> L then
        Result := Copy(Result, 1, I);
      Break;
    end;
end;

function TdxFontFamilyInfos.GetFontFamily(const AFontName: string): string;
begin
  Result := RemovePattern(AFontName, FStylePattern);
  Result := StringReplace(Result, ',', '', [rfReplaceAll]);
end;

function TdxFontFamilyInfos.GetFamilies: TStringList;
var
  AGPFamily: TdxGPFontFamily;
begin
  if FFamilies = nil then
  begin
    FFamilies := TStringList.Create;
    for AGPFamily in FInstalledFontCollection.Families do
      FFamilies.Add(AGPFamily.Name);
  end;
  Result := FFamilies;
end;

function TdxFontFamilyInfos.GetFontStyle(const AFontName: string): string;
begin
  Result := MatchPattern(AFontName, FStylePattern);
end;

function TdxFontFamilyInfos.ContainsBoldStyle(const AFontStyle: string)
  : Boolean;
begin
  Result := (AFontStyle <> '') and
    (MatchPattern(AFontStyle, 'demibold|demi|black|bold|bd') <> '');
end;

function TdxFontFamilyInfos.ContainsItalicStyle(const AFontStyle
  : string): Boolean;
begin
  Result := (AFontStyle <> '') and
    (MatchPattern(AFontStyle, 'italic|oblique|it') <> '');
end;

function TdxFontFamilyInfos.ExtractAdditionalStyles(const AActualStyle: string)
  : TStringDynArray;
var
  AResult: TStringBuilder;
  ATemp, APattern: string;
begin
  AResult := TdxStringBuilderManager.Get;
  try
    ATemp := LowerCase(AActualStyle);
    for APattern in FAdditionalStylePattern do
    begin
      if Pos(APattern, ATemp) > 0 then
      begin
        ATemp := StringReplace(ATemp, APattern, '', [rfReplaceAll]);
        AResult.Append('|');
        AResult.Append(APattern);
      end;
    end;
    Result := TdxPDFUtils.Split(AResult.ToString, '|');
  finally
    TdxStringBuilderManager.Release(AResult);
  end;
end;

function TdxFontFamilyInfos.GetNormalizedFontFamily(const AFontName
  : string): string;
begin
  Result := Normalize(GetFontFamily(AFontName));
end;

function TdxFontFamilyInfos.GetNormalizedFontStyle(const AFontName
  : string): string;
begin
  Result := Normalize(GetFontStyle(AFontName));
end;

function TdxFontFamilyInfos.TryGetValue(const AFamilyName: string;
out AInfo: TdxFontFamilyInfo): Boolean;
begin
  Result := FInfos.TryGetValue(AFamilyName, AInfo);
end;

procedure TdxFontFamilyInfos.AddFamilyIfNotExists(const AKey, AValue: string);
var
  ANormalized: string;
begin
  ANormalized := GetNormalizedFontFamily(AKey);
  if not FInfos.ContainsKey(ANormalized) then
    FInfos.Add(ANormalized, TdxFontFamilyInfo.Create(AValue));
end;

procedure TdxFontFamilyInfos.PopulateInfos;
var
  AGPFamily: TdxGPFontFamily;
  AFamilyName, AActualFamily, AActualStyle: string;
  AInfo: TdxFontFamilyInfo;
begin
  for AGPFamily in FInstalledFontCollection.Families do
  begin
    AFamilyName := AGPFamily.Name;
    AActualFamily := GetNormalizedFontFamily(AFamilyName);
    AActualStyle := GetNormalizedFontStyle(AFamilyName);
    if not FInfos.TryGetValue(AActualFamily, AInfo) then
    begin
      AInfo := TdxFontFamilyInfo.Create(AFamilyName);
      FInfos.Add(AActualFamily, AInfo);
    end;
    if AActualStyle <> '' then
      AInfo.AdditionalStyles.AddOrSetValue(AActualStyle, AFamilyName);
    if TdxStringHelper.Contains(AFamilyName, 'Segoe UI') then
      FSegoeUIPresent := True;
  end;
  AddFamilyIfNotExists(TdxPDFKeywords.CourierFontName,
    TdxPDFKeywords.CourierNewFontName2);
  AddFamilyIfNotExists(TdxPDFKeywords.CourierNewFontName,
    TdxPDFKeywords.CourierNewFontName2);
  AddFamilyIfNotExists('CourierNewPS', TdxPDFKeywords.CourierNewFontName2);
  AddFamilyIfNotExists('CourierNewPS' + TdxPDFKeywords.FontMTSuffix,
    TdxPDFKeywords.CourierNewFontName2);

  AddFamilyIfNotExists(TdxPDFKeywords.TimesRomanFontName,
    TdxPDFKeywords.TimesNewRomanFontName2);
  AddFamilyIfNotExists('Times', TdxPDFKeywords.TimesNewRomanFontName2);
  AddFamilyIfNotExists('TimesNewRomanPS',
    TdxPDFKeywords.TimesNewRomanFontName2);
  AddFamilyIfNotExists('TimesNewRomanPS' + TdxPDFKeywords.FontMTSuffix,
    TdxPDFKeywords.TimesNewRomanFontName2);

  AddFamilyIfNotExists(TdxPDFKeywords.ArialFontName +
    TdxPDFKeywords.FontMTSuffix, TdxPDFKeywords.ArialFontName);

  AddFamilyIfNotExists('TallPaul', 'Gabriola');
  AddFamilyIfNotExists('CenturyGothic', 'Century Gothic');
  AddFamilyIfNotExists('GothicText', 'MS Gothic');
  AddFamilyIfNotExists('Flama', 'Tahoma');
  AddFamilyIfNotExists('FlamaBook', 'Tahoma');

  AddFamilyIfNotExists(TdxPDFKeywords.HelveticaFontName,
    TdxPDFKeywords.ArialFontName);

  AFamilyName := Normalize(TdxPDFKeywords.SymbolFontName);
  if FSegoeUIPresent then
    FInfos.AddOrSetValue(AFamilyName, TdxFontFamilyInfo.Create('Segoe UI'))
  else
    FInfos.AddOrSetValue(AFamilyName,
      TdxFontFamilyInfo.Create(TdxPDFKeywords.ArialUnicodeMS));
  AddFamilyIfNotExists(TdxPDFKeywords.ZapfDingbatsFontName, 'MS Gothic');
end;

{ TdxPDFGDIFontSubstitutionHelper }

constructor TdxPDFGDIFontSubstitutionHelper.Create;
begin
  inherited Create;
  FFontFamilyInfos := TdxFontFamilyInfos.Create;
end;

destructor TdxPDFGDIFontSubstitutionHelper.Destroy;
begin
  FreeAndNil(FFontFamilyInfos);
  inherited Destroy;
end;

function TdxPDFGDIFontSubstitutionHelper.GetSubstituteFontParameters
  (AFont: TdxPDFCustomFont): TdxPDFFontRegistratorParameters;
begin
  Result := GetSubstituteFontParameters(AFont, nil);
end;

function TdxPDFGDIFontSubstitutionHelper.GetSubstituteFontParameters
  (AFont: TdxPDFCustomFont; AFontFamilyFilter: TFunc<string, Boolean>)
  : TdxPDFFontRegistratorParameters;
var
  AActualFamily, AActualStyle, AActualName, AAdditionalStyle: string;
  AFontWeight: Integer;
  AAdditionalStyleFound, AIsItalic: Boolean;
  AInfo: TdxFontFamilyInfo;
begin
  AActualStyle := GetFontStyle(AFont, AActualFamily);
  AActualName := AFont.Name;
  AAdditionalStyleFound := False;
  if FontFamilyInfos.TryGetValue(AActualFamily, AInfo) then
  begin
    if not Assigned(AFontFamilyFilter) or AFontFamilyFilter(AActualFamily) then
    begin
      for AAdditionalStyle in FontFamilyInfos.ExtractAdditionalStyles
        (AActualStyle) do
        if AInfo.AdditionalStyles.TryGetValue(AAdditionalStyle, AActualName)
        then
        begin
          AAdditionalStyleFound := True;
          Break;
        end;
      if not AAdditionalStyleFound then
        AActualName := AInfo.SystemFontName;
    end
    else
      AActualName := FontFamilyInfos.GetFontFamily(AFont.Name);
  end
  else
    AActualName := FontFamilyInfos.GetFontFamily(AFont.Name);
  if not AAdditionalStyleFound and FontFamilyInfos.ContainsBoldStyle
    (AActualStyle) or (AActualStyle <> '') and AFont.ForceBold then
    AFontWeight := BoldWeight
  else
    AFontWeight := GetFontWeight(AFont);
  AIsItalic := False;
  if not AAdditionalStyleFound then
    AIsItalic := FontFamilyInfos.ContainsItalicStyle(AActualStyle);
  Result := TdxPDFFontRegistratorParameters.Create(AActualName, AFontWeight,
    AIsItalic);
end;

function TdxPDFGDIFontSubstitutionHelper.GetFontStyle(AFont: TdxPDFCustomFont;
out AFamily: string): string;
var
  I: Integer;
  AFontNames: TStringList;
begin
  Result := '';
  AFontNames := TStringList.Create;
  try
    AFontNames.Add(AFont.Name);
    AFontNames.Add(AFont.BaseFont);
    if AFont.FontDescriptor <> nil then
      AFontNames.Add(AFont.FontDescriptor.FontName);
    AFamily := FontFamilyInfos.GetNormalizedFontFamily(AFont.Name);
    for I := 0 to AFontNames.Count - 1 do
    begin
      if AFontNames[I] <> '' then
        Result := FontFamilyInfos.GetNormalizedFontStyle(AFontNames[I]);
      if Result <> '' then
        Break;
    end;
  finally
    AFontNames.Free;
  end;
end;

function TdxPDFGDIFontSubstitutionHelper.GetFontWeight
  (AFont: TdxPDFCustomFont): Integer;
begin
  if (AFont <> nil) and (AFont.FontDescriptor <> nil) then
    Result := AFont.FontDescriptor.FontWeight
  else
    Result := NormalWeight;
end;

{ TdxPDFFontDataStorage }

constructor TdxPDFFontDataStorage.Create(const ATempFolder: string);
begin
  inherited Create;
  FFolderName := ATempFolder;
  FDictionary := TDictionary<TdxPDFCustomFont,
    TdxPDFFontRegistrationData>.Create;
  FReferences := TdxPDFUniqueReferences.Create;
  InitializeCriticalSectionAndSpinCount(FLock, 1024);
  FFontSubstitutionHelper := TdxPDFGDIFontSubstitutionHelper.Create;
  FFontCache := TdxPDFGDIEditableFontDataCache.Create(FontFamilyInfos);
  FQueue := TList<TdxPDFCustomFont>.Create;
end;

function TdxPDFFontDataStorage.CreateFontRegistrator
  (AFont: TdxPDFCustomFont): TObject;
begin
  Result := TdxPDFFontCustomRegistrator.CreateRegistrator
    (FFontSubstitutionHelper, AFont, FFolderName);
end;

destructor TdxPDFFontDataStorage.Destroy;
begin
  Clear;
  FreeAndNil(FQueue);
  FreeAndNil(FFontCache);
  FreeAndNil(FReferences);
  FreeAndNil(FDictionary);
  FreeAndNil(FFontSubstitutionHelper);
  DeleteCriticalSection(FLock);
  inherited Destroy;
end;

function TdxPDFFontDataStorage.Add(AFont: TdxPDFCustomFont)
  : TdxPDFFontRegistrationData;
var
  ARegistrator: TdxPDFFontCustomRegistrator;
begin
  EnterCriticalSection(FLock);
  try
    if AFont = FLastRegisteredFont then
      Result := FLastRegisteredFontData
    else
    begin
      FLastRegisteredFont := AFont;
      if FReferences.ContainsKey(AFont.UniqueName, AFont.Number) and
        FDictionary.TryGetValue(AFont, FLastRegisteredFontData) then
        Exit(FLastRegisteredFontData);
      FLastRegisteredFont := AFont;
      ARegistrator := CreateFontRegistrator(AFont)
        as TdxPDFFontCustomRegistrator;
      if ARegistrator <> nil then
      begin
        FLastRegisteredFontData := ARegistrator.Register;
        if FLastRegisteredFontData.Registrator = nil then
          ARegistrator.Free;
      end
      else
        FLastRegisteredFontData := TdxPDFFontRegistrationData.Create(AFont.Name,
          AFont.Weight, AFont.Italic, AFont.PitchAndFamily, False, nil,
          AFont is TdxPDFType3Font);
      InternalAdd(AFont);
      Result := FLastRegisteredFontData;
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TdxPDFFontDataStorage.CreateSubstituteFontData(AFont: TdxPDFCustomFont)
  : TdxPDFFontRegistrationData;
var
  AFontRegistrator: TdxPDFFontCustomRegistrator;
begin
  AFontRegistrator := TdxPDFFontCustomRegistrator.CreateRegistrator
    (FFontSubstitutionHelper, AFont, FFolderName);
  try
    Result := AFontRegistrator.CreateSubstituteFontData;
  finally
    AFontRegistrator.Free;
  end;
end;

function TdxPDFFontDataStorage.SearchFontData(const AFontFamilyName: string;
AFontStyle: TdxGPFontStyle): TObject;
begin
  Result := (FFontCache as TdxPDFGDIEditableFontDataCache)
    .SearchFontData(AFontFamilyName, AFontStyle);
end;

function TdxPDFFontDataStorage.TryGetValue(ANumber: Integer;
out AFont: TdxPDFCustomFont): Boolean;
var
  AObject: TdxPDFBase;
begin
  AFont := nil;
  Result := FReferences.TryGetValue(ANumber, AObject);
  if Result then
    AFont := AObject as TdxPDFCustomFont;
end;

procedure TdxPDFFontDataStorage.Clear;
var
  AData: TdxPDFFontRegistrationData;
  AFont: TdxPDFCustomFont;
begin
  FQueue.Clear;
  for AFont in FDictionary.Keys do
    RemoveListener(AFont);
  for AData in FDictionary.Values do
    if AData.Registrator <> nil then
      AData.Registrator.Free;
  FDictionary.Clear;
  FReferences.Clear;
  (FFontCache as TdxPDFGDIEditableFontDataCache).Clear;
end;

procedure TdxPDFFontDataStorage.Delete(AFont: TdxPDFCustomFont);
var
  ARegistrationData: TdxPDFFontRegistrationData;
  ARegistrator: TdxPDFFontCustomRegistrator;
begin
  if FDictionary.TryGetValue(AFont, ARegistrationData) then
  begin
    if FLastRegisteredFont = AFont then
      FLastRegisteredFont := nil;
    FDictionary.Remove(AFont);
    FReferences.Remove(AFont.UniqueName);
    FQueue.Remove(AFont);
    ARegistrator := ARegistrationData.Registrator as
      TdxPDFFontCustomRegistrator;
    if ARegistrator <> nil then
      ARegistrator.Free;
  end;
end;

function TdxPDFFontDataStorage.GetFontFamilyInfos: TdxFontFamilyInfos;
begin
  Result := FFontSubstitutionHelper.FontFamilyInfos;
end;

procedure TdxPDFFontDataStorage.InternalAdd(AFont: TdxPDFCustomFont);
var
  I: Integer;
begin
  if FQueue.Count > dxPDFDocumentFontCacheSize then
    for I := 0 to FQueue.Count - 1 do
      Delete(FQueue[0]);
  FReferences.Add(AFont.UniqueName, AFont.Number, AFont);
  FDictionary.Add(AFont, FLastRegisteredFontData);
  FQueue.Add(AFont);
  AFont.AddListener(Self);
end;

procedure TdxPDFFontDataStorage.RemoveListener(AFont: TdxPDFCustomFont);
begin
  AFont.RemoveListener(Self);
end;

procedure TdxPDFFontDataStorage.FontDestroyHandler(Sender: TdxPDFBase);
begin
  Delete(Sender as TdxPDFCustomFont);
end;

{ TdxPDFXObjects }

class function TdxPDFXObjects.GetTypeName: string;
begin
  Result := TdxPDFKeywords.XObject;
end;

class function TdxPDFXObjects.GetTypePrefix: string;
begin
  Result := 'O';
end;

function TdxPDFXObjects.GetXObject(const AName: string): TdxPDFXObject;
begin
  Result := GetObject(AName) as TdxPDFXObject;
end;

{ TdxPDFColorSpaces }

class function TdxPDFColorSpaces.GetTypeName: string;
begin
  Result := TdxPDFKeywords.ColorSpace;
end;

function TdxPDFColorSpaces.GetColorSpace(const AName: string)
  : TdxPDFCustomColorSpace;
begin
  Result := GetObject(AName) as TdxPDFCustomColorSpace;
  if Result = nil then
    Result := TdxPDFCustomColorSpace.CreateColorSpace(AName, nil);
end;

class function TdxPDFColorSpaces.GetTypePrefix: string;
begin
  Result := 'CS';
end;

procedure TdxPDFColorSpaces.Read(ADictionary: TdxPDFReaderDictionary);
begin
  ReadList(ADictionary);
end;

procedure TdxPDFColorSpaces.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  WriteList(AHelper, ADictionary);
end;

{ TdxPDFCustomShading }

class function TdxPDFCustomShading.GetTypeName: string;
begin
  Result := 'ShadingItem';
end;

class function TdxPDFCustomShading.Parse(ARepository: TdxPDFCustomRepository;
ASourceObject: TdxPDFBase): TdxPDFCustomShading;
var
  AType: Integer;
  ADictionary: TdxPDFDictionary;
begin
  case ASourceObject.ObjectType of
    otDictionary:
      ADictionary := ASourceObject as TdxPDFDictionary;
    otStream:
      ADictionary := TdxPDFStream(ASourceObject).Dictionary;
    otIndirectReference:
      ADictionary := ARepository.GetDictionary
        (TdxPDFReference(ASourceObject).Number);
  else
    ADictionary := nil;
  end;

  if ADictionary <> nil then
  begin
    AType := ADictionary.GetInteger(TdxPDFKeywords.ShadingType);
    if not TdxPDFUtils.IsIntegerValid(AType) then
      TdxPDFUtils.RaiseTestException('Incorrect shading type');
    case AType of
      1:
        Result := TdxPDFFunctionBasedShading.Create(nil);
      2:
        Result := TdxPDFAxialShading.Create(nil);
      3:
        Result := TdxPDFRadialShading.Create(nil);
    else
      Result := nil;
    end;
    if Result <> nil then
      Result.Read(ADictionary as TdxPDFReaderDictionary);
  end
  else
    Result := nil;
end;

function TdxPDFCustomShading.TransformFunction(const AArguments
  : TDoubleDynArray): TdxPDFColor;
var
  AIndex: Integer;
  AComponents, AColorComponents: TDoubleDynArray;
begin
  if (FFunctions = nil) or (FFunctions.Count = 0) then
    Result := TdxPDFColor.Create(FColorSpace.Transform(AArguments))
  else
  begin
    if FFunctions.Count = 1 then
      AColorComponents := (FFunctions[0] as TdxPDFCustomFunction)
        .CreateTransformedComponents(AArguments)
    else
    begin
      SetLength(AColorComponents, FFunctions.Count);
      SetLength(AComponents, 1);
      AComponents[0] := 0;
      for AIndex := 0 to FFunctions.Count - 1 do
        TdxPDFUtils.AddData((FFunctions[AIndex] as TdxPDFCustomFunction)
          .CreateTransformedComponents(AComponents), AColorComponents);
    end;
    Result := TdxPDFColor.Create(FColorSpace.Transform(AColorComponents));
  end;
end;

procedure TdxPDFCustomShading.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FColorSpace := nil;
  FBackgroundColor := TdxPDFColor.Null;
  FFunctions := nil;
end;

procedure TdxPDFCustomShading.DestroySubClasses;
begin
  FreeAndNil(FColorSpace);
  FreeAndNil(FFunctions);
  inherited DestroySubClasses;
end;

procedure TdxPDFCustomShading.ReadProperties(ADictionary
  : TdxPDFReaderDictionary);
begin
  inherited;

  if ADictionary.Contains(TdxPDFKeywords.ColorSpace) then
  begin
    ReadColorSpace(ADictionary.GetObject(TdxPDFKeywords.ColorSpace));
    ReadBackgroundColor(ADictionary.GetArray(TdxPDFKeywords.Background));
    ReadFunctions(ADictionary.GetObject(TdxPDFKeywords.Function));
    FBoundingBox := ADictionary.GetRectangleEx(TdxPDFKeywords.BBox);
    FUseAntiAliasing := ADictionary.GetBoolean(TdxPDFKeywords.AntiAlias, False);
  end;
end;

procedure TdxPDFCustomShading.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  ADictionary.Add(TdxPDFKeywords.ShadingType, GetShadingType);
  ADictionary.Add(TdxPDFKeywords.ColorSpace, ColorSpace);
  ADictionary.Add(TdxPDFKeywords.Background, BackgroundColor);
  ADictionary.Add(TdxPDFKeywords.BBox, BoundingBox);
  ADictionary.Add(TdxPDFKeywords.AntiAlias, UseAntiAliasing);
  WriteFunctions(AHelper, ADictionary);
end;

class function TdxPDFCustomShading.GetShadingType: Integer;
begin
  Result := -1;
end;

function TdxPDFCustomShading.GetDomainDimension: Integer;
begin
  Result := 1;
end;

function TdxPDFCustomShading.IsFunctionRequired: Boolean;
begin
  Result := True;
end;

function TdxPDFCustomShading.CreateFunctions(ASourceObject: TdxPDFBase)
  : TdxPDFReferencedObjects;
var
  ATempObject: TdxPDFBase;
begin
  Result := TdxPDFReferencedObjects.Create;
  if ASourceObject.ObjectType = otArray then
    for ATempObject in TdxPDFArray(ASourceObject).ElementList do
      Result.Add(TdxPDFCustomFunction.Parse(Repository, ATempObject))
  else
    Result.Add(TdxPDFCustomFunction.Parse(Repository, ASourceObject));
end;

procedure TdxPDFCustomShading.ReadBackgroundColor(AArray: TdxPDFArray);
begin
  if AArray <> nil then
  begin
    FBackgroundColor := TdxPDFColor.Create(AArray);
    if Length(FBackgroundColor.Components) <> AArray.Count then
      TdxPDFUtils.RaiseTestException
        ('Incorrect background color component count');
  end;
end;

procedure TdxPDFCustomShading.ReadColorSpace(ASourceObject: TdxPDFBase);
begin
  FColorSpace := TdxPDFCustomColorSpace.Parse(Repository, ASourceObject);
  if FColorSpace is TdxPDFPatternColorSpace then
    TdxPDFUtils.RaiseTestException;
end;

procedure TdxPDFCustomShading.ReadFunctions(AObject: TdxPDFBase);
var
  AFunction: TdxPDFCustomFunction;
  I: Integer;
begin
  if AObject <> nil then
  begin
    FFunctions.Free;
    FFunctions := CreateFunctions(Repository.ResolveReference(AObject));
    if FColorSpace is TdxPDFIndexedColorSpace then
      TdxPDFUtils.RaiseTestException;
    if FFunctions.Count = 1 then
    begin
      AFunction := FFunctions[0] as TdxPDFCustomFunction;
      if (Length(AFunction.Domain) <> GetDomainDimension) or
        (AFunction.RangeCount <> FColorSpace.ComponentCount) then
        TdxPDFUtils.RaiseTestException;
    end
    else if FFunctions.Count <> FColorSpace.ComponentCount then
      for I := 0 to FFunctions.Count - 1 do
      begin
        AFunction := FFunctions[0] as TdxPDFCustomFunction;
        if (Length(AFunction.Domain) <> GetDomainDimension) or
          (AFunction.RangeCount <> 1) then
          TdxPDFUtils.RaiseTestException;
      end
    else
      TdxPDFUtils.RaiseTestException;
  end
  else
  begin
    if IsFunctionRequired then
      TdxPDFUtils.RaiseTestException;
  end;
end;

procedure TdxPDFCustomShading.WriteFunctions(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
var
  AArray: TdxPDFWriterArray;
  I: Integer;
begin
  if FFunctions.Count > 1 then
  begin
    AArray := AHelper.CreateArray;
    for I := 0 to FFunctions.Count - 1 do
      AArray.AddReference(FFunctions[I] as TdxPDFCustomFunction);
  end
  else if FFunctions.Count = 1 then
    ADictionary.AddReference(TdxPDFKeywords.Function,
      FFunctions[0] as TdxPDFCustomFunction);
end;

{ TdxPDFShadings }

class function TdxPDFShadings.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Shading;
end;

function TdxPDFShadings.GetShading(const AName: string): TdxPDFCustomShading;
begin
  Result := GetObject(AName) as TdxPDFCustomShading;
end;

class function TdxPDFShadings.GetTypePrefix: string;
begin
  Result := 'S';
end;

procedure TdxPDFShadings.Read(ADictionary: TdxPDFReaderDictionary);
begin
  ReadList(ADictionary);
end;

procedure TdxPDFShadings.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  WriteList(AHelper, ADictionary);
end;

{ TdxPDFCustomPattern }

class function TdxPDFCustomPattern.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Pattern;
end;

class function TdxPDFCustomPattern.Parse(ARepository: TdxPDFCustomRepository;
ASourceObject: TdxPDFBase): TdxPDFCustomPattern;
begin
  Result := nil;
  if ASourceObject <> nil then
    case ASourceObject.ObjectType of
      otDictionary:
        begin
          Result := TdxPDFShadingPattern.Create(nil);
          Result.Read(ASourceObject as TdxPDFReaderDictionary);
        end;
      otStream:
        begin
          Result := TdxPDFTilingPattern.Create(nil);
          TdxPDFStream(ASourceObject).Dictionary.StreamRef :=
            TdxPDFStream(ASourceObject);
          TdxPDFStream(ASourceObject).Dictionary.Number := ASourceObject.Number;
          Result.Read((ASourceObject as TdxPDFStream)
            .Dictionary as TdxPDFReaderDictionary);
        end;
    else
      TdxPDFUtils.RaiseTestException('Incorrect pattern source object');
    end;
end;

class function TdxPDFCustomPattern.GetPatternType: Integer;
begin
  Result := -MaxInt;
end;

procedure TdxPDFCustomPattern.Read(ADictionary: TdxPDFReaderDictionary);
var
  APatternType: Integer;
  AType: string;
begin
  inherited;

  if ADictionary <> nil then
  begin
    AType := ADictionary.GetString(TdxPDFKeywords.TypeKey);
    if (AType <> '') and (AType <> TdxPDFKeywords.Pattern) then
      TdxPDFUtils.RaiseTestException('Incorrect type');

    APatternType := ADictionary.GetInteger(TdxPDFKeywords.PatternType);
    if not TdxPDFUtils.IsIntegerValid(APatternType) or
      (APatternType <> GetPatternType) then
      TdxPDFUtils.RaiseTestException('Incorrect pattern type');

    FMatrix := ADictionary.GetMatrix(TdxPDFKeywords.Matrix);
  end;
end;

procedure TdxPDFCustomPattern.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  ADictionary.Add(TdxPDFKeywords.PatternType, GetPatternType);
  ADictionary.Add(TdxPDFKeywords.Matrix, Matrix);
end;

{ TdxPDFPatterns }

class function TdxPDFPatterns.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Pattern;
end;

function TdxPDFPatterns.GetPattern(const AName: string): TdxPDFCustomPattern;
begin
  Result := GetObject(AName) as TdxPDFCustomPattern;
end;

procedure TdxPDFPatterns.Read(ADictionary: TdxPDFReaderDictionary);
begin
  ReadList(ADictionary);
end;

procedure TdxPDFPatterns.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  WriteList(AHelper, ADictionary);
end;

class function TdxPDFPatterns.GetTypePrefix: string;
begin
  Result := 'Ptrn';
end;

{ TdxPDFShadingPattern }

class function TdxPDFShadingPattern.GetPatternType: Integer;
begin
  Result := 2;
end;

procedure TdxPDFShadingPattern.CreateSubClasses;
begin
  Shading := nil;
  GraphicsState := nil;
  inherited CreateSubClasses;
end;

procedure TdxPDFShadingPattern.DestroySubClasses;
begin
  GraphicsState := nil;
  Shading := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFShadingPattern.ReadProperties(ADictionary
  : TdxPDFReaderDictionary);
var
  AObject: TdxPDFBase;
begin
  if not ADictionary.TryGetObject(TdxPDFKeywords.Shading, AObject) then
    TdxPDFUtils.RaiseTestException;
  Shading := TdxPDFCustomShading.Parse(Repository, AObject);
  GraphicsState := TdxPDFGraphicsStateParameters.Create(Self);
  GraphicsState.Read(ADictionary.GetDictionary(TdxPDFKeywords.ExtGState));
end;

procedure TdxPDFShadingPattern.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  inherited;
  ADictionary.AddInline(TdxPDFKeywords.Shading, Shading);
  if GraphicsState.AssignedValues <> [] then
    ADictionary.AddInline(TdxPDFKeywords.ExtGState, GraphicsState);
end;

procedure TdxPDFShadingPattern.SetGraphicsStateParameters
  (const AValue: TdxPDFGraphicsStateParameters);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FGraphicsState));
end;

procedure TdxPDFShadingPattern.SetShading(const AValue: TdxPDFCustomShading);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FShading));
end;

{ TdxPDFTilingPattern }

function TdxPDFTilingPattern.CreateMatrix(AWidth: Integer; AHeight: Integer)
  : TdxPDFTransformationMatrix;
var
  AFactorX, AFactorY: Double;
begin
  AFactorX := AWidth / Abs(FBoundingBox.Width);
  AFactorY := AHeight / Abs(FBoundingBox.Height);
  Result := TdxPDFTransformationMatrix.Create;
  Result.Assign(AFactorX, 0, 0, AFactorY, -FBoundingBox.Left * AFactorX,
    -FBoundingBox.Bottom * AFactorY);
end;

class function TdxPDFTilingPattern.GetPatternType: Integer;
begin
  Result := 1;
end;

procedure TdxPDFTilingPattern.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FCommands := nil;
  FColoredPaintType := 1;
  FUncoloredPaintType := 2;
  FColored := True;
  Resources := nil;
end;

procedure TdxPDFTilingPattern.DestroySubClasses;
begin
  Resources := nil;
  FreeAndNil(FCommands);
  inherited DestroySubClasses;
end;

procedure TdxPDFTilingPattern.ReadProperties(ADictionary
  : TdxPDFReaderDictionary);
var
  APaintType: Integer;
begin
  inherited;

  FColoredPaintType := 1;
  FUncoloredPaintType := 2;

  APaintType := ADictionary.GetInteger(TdxPDFKeywords.PaintType);
  ReadTilingType(ADictionary);
  FBoundingBox := ADictionary.GetRectangleEx(TdxPDFKeywords.BBox);

  if not TdxPDFUtils.IsIntegerValid(APaintType) or FBoundingBox.IsNull then
    TdxPDFUtils.RaiseTestException('Error reading tiling pattern');

  case APaintType of
    1:
      FColored := True;
    2:
      FColored := False;
  else
    TdxPDFUtils.RaiseTestException('Incorrect tiling pattern type');
  end;

  Resources := Repository.GetResources(ADictionary);
  ReadStep(ADictionary);
  ReadCommands(ADictionary);
end;

procedure TdxPDFTilingPattern.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  inherited;
  ADictionary.Add(TdxPDFKeywords.TilingType, Ord(FTilingType));
  ADictionary.Add(TdxPDFKeywords.PaintType, 1 + Ord(FColored = False));
  ADictionary.Add(TdxPDFKeywords.BBox, FBoundingBox);
  ADictionary.Add(TdxPDFKeywords.XStep, FXStep);
  ADictionary.Add(TdxPDFKeywords.YStep, FYStep);
  ADictionary.AddReference(TdxPDFKeywords.Resources, Resources);
  Commands.Write(AHelper, ADictionary, Resources);
end;

procedure TdxPDFTilingPattern.SetParent(const AValue: TObject);
begin
  inherited SetParent(AValue);
  if Resources <> nil then
    Resources.Parent := Parent;
end;

procedure TdxPDFTilingPattern.ReadCommands(ADictionary: TdxPDFReaderDictionary);
begin
  FCommands := TdxPDFCommandList.Create;
  FCommands.Read(ADictionary.StreamRef, Repository, Resources);
end;

procedure TdxPDFTilingPattern.ReadStep(ADictionary: TdxPDFDictionary);
const
  sdxErrorMessage = 'Error reading tiling pattern steps';
var
  AXStepValue, AYStepValue: Double;
begin
  AXStepValue := ADictionary.GetDouble(TdxPDFKeywords.XStep);
  AYStepValue := ADictionary.GetDouble(TdxPDFKeywords.YStep);
  if not TdxPDFUtils.IsDoubleValid(AXStepValue) or not TdxPDFUtils.IsDoubleValid
    (AYStepValue) then
    TdxPDFUtils.RaiseTestException(sdxErrorMessage);
  FXStep := AXStepValue;
  FYStep := AYStepValue;
  if (FXStep = 0) or (FYStep = 0) then
    TdxPDFUtils.RaiseTestException(sdxErrorMessage);
end;

procedure TdxPDFTilingPattern.ReadTilingType(ADictionary: TdxPDFDictionary);
var
  AType: Integer;
begin
  AType := ADictionary.GetInteger(TdxPDFKeywords.TilingType);
  if TdxPDFUtils.IsIntegerValid(AType) then
    FTilingType := TdxPDFTilingType(AType)
  else
    TdxPDFUtils.RaiseTestException('Incorrect tiling type');
end;

{ TdxPDFFullTrustGlyphMapper }

constructor TdxPDFFullTrustGlyphMapper.Create(AFontFile: TdxFontFile);
var
  AIsSymbolic: Boolean;
  AComparison: TComparison<TdxFontFileCMapCustomFormatRecord>;
begin
  inherited Create;
  FFontFile := AFontFile;
  FMappedGlyphsCache := TdxPDFIntegerIntegerDictionary.Create;

  if FFontFile.HheaTable = nil then
    FFactor := 1000 / 2048
  else
    FFactor := 1000.0 / AFontFile.HeadTable.UnitsPerEm;

  FCMapTables := TList<TdxFontFileCMapCustomFormatRecord>.Create;
  if AFontFile.CMapTable <> nil then
  begin
    FCMapTables.AddRange(AFontFile.CMapTable.CMapTables);
    AIsSymbolic := (AFontFile.OS2Table <> nil) and
      AFontFile.OS2Table.IsSymbolic;
    AComparison :=
        function(const Left, Right: TdxFontFileCMapCustomFormatRecord): Integer
      begin
        Result := GetCMapEntryPriority(Left, AIsSymbolic) -
          GetCMapEntryPriority(Right, AIsSymbolic);
      end;
    FCMapTables.Sort(TComparer<TdxFontFileCMapCustomFormatRecord>.Construct
      (AComparison));
  end;
end;

destructor TdxPDFFullTrustGlyphMapper.Destroy;
begin
  FreeAndNil(FCMapTables);
  FreeAndNil(FMappedGlyphsCache);
  inherited Destroy;
end;

class function TdxPDFFullTrustGlyphMapper.GetCMapEntryPriority
  (AEntry: TdxFontFileCMapCustomFormatRecord; AIsSymbolic: Boolean): Integer;
begin
  case AEntry.PlatformId of
    TdxFontFilePlatformID.Microsoft:
      Result := 0;
    TdxFontFilePlatformID.ISO:
      Result := 100;
  else
    Result := 200;
  end;
  case AEntry.EncodingId of
    TdxFontFileEncodingID.UGL:
      Inc(Result, IfThen(AIsSymbolic, 10, 0));
    TdxFontFileEncodingID.Undefined:
      Inc(Result, IfThen(AIsSymbolic, 0, 10));
  else
    Inc(Result, 20);
  end;
  if not(AEntry is TdxFontFileCMapSegmentMappingRecord) then
    Inc(Result, 1);
end;

function TdxPDFFullTrustGlyphMapper.GetGlyphIndex(ACharacter: Char): Integer;
var
  ACMap: TdxFontFileCMapCustomFormatRecord;
begin
  Result := 0;
  if not FMappedGlyphsCache.TryGetValue(Integer(ACharacter), Result) then
  begin
    for ACMap in FCMapTables do
    begin
      Result := ACMap.MapCode(ACharacter);
      if Result <> TdxFontFileCMapCustomFormatRecord.NotdefGlyphIndex then
        Break;
    end;
    FMappedGlyphsCache.Add(Integer(ACharacter), Result);
  end;
end;

function TdxPDFFullTrustGlyphMapper.MapString(const AStr: string;
AFlags: TdxPDFGlyphMappingFlags): TdxPDFGlyphMappingResult;
begin
  Result := MapStringWithoutCTL(AStr, AFlags);
end;

function TdxPDFFullTrustGlyphMapper.IsWritingOrderControl(AChar: Char): Boolean;
begin
  Result := False;
end;

function TdxPDFFullTrustGlyphMapper.MapStringWithoutCTL(const AStr: string;
AFlags: TdxPDFGlyphMappingFlags): TdxPDFGlyphMappingResult;
var
  AActualText: string;
  ABuilder: TStringBuilder;
  ACh: Char;
  ACodes: TBytes;
  AGlyph: TdxPDFGlyph;
  AGlyphIndices: TdxPDFIntegerList;
  AGlyphOffset: Double;
  AGlyphs: TdxPDFGlyphList;
  AKern: TdxFontFileKernTable;
  AKerningShouldBeUsed: Boolean;
  ALength, I, AVal: Integer;
  AResult: TdxPDFIntegerStringDictionary;
begin
  ALength := Length(AStr);
  AGlyphIndices := TdxPDFIntegerList.Create;
  try
    ABuilder := TdxStringBuilderManager.Get(ALength);
    try
      if FCMapTables <> nil then
        for I := 1 to ALength do
        begin
          ACh := AStr[I];
          if not IsWritingOrderControl(ACh) then
          begin
            AGlyphIndices.Add(GetGlyphIndex(ACh));
            ABuilder.Append(ACh);
          end;
        end;
      AResult := TdxPDFIntegerStringDictionary.Create(ALength);
      AGlyphs := TdxPDFGlyphList.Create;
      ALength := AGlyphIndices.Count;
      SetLength(ACodes, ALength * 2);

      AKern := FFontFile.KernTable;
      AKerningShouldBeUsed := HasFlag(AFlags, mfUseKerning) and (AKern <> nil)
        and (AGlyphIndices <> nil);
      AActualText := ABuilder.ToString;
    finally
      TdxStringBuilderManager.Release(ABuilder);
    end;

    for I := 0 to ALength - 1 do
    begin
      ACh := AActualText[I + 1];
      if AGlyphIndices = nil then
        AVal := Integer(ACh)
      else
        AVal := AGlyphIndices[I];
      AGlyphOffset := 0;
      if AKerningShouldBeUsed and (I > 0) then
        AGlyphOffset := -AKern.GetKerning(AGlyphIndices[I - 1], AVal) * FFactor;
      AGlyph := CreateGlyph(AVal, ACh, FFontFile.GetCharacterWidth(AVal),
        AGlyphOffset);
      if not AResult.ContainsKey(AGlyph.Index) then
        AResult.Add(AGlyph.Index, ACh);
      AGlyphs.Add(AGlyph);
    end;
    Result := TdxPDFGlyphMappingResult.Create(CreateGlyphRun(AGlyphs), AResult);
  finally
    AGlyphIndices.Free;
  end;
end;

{ TdxPDFEmbeddedGlyphMapper }

function TdxPDFEmbeddedGlyphMapper.CreateGlyphRun: TdxPDFGlyphRun;
begin
  Result := TdxPDFCompositeFontGlyphRun.Create;
end;

function TdxPDFEmbeddedGlyphMapper.CreateGlyph(AGlyphIndex: Integer; ACh: Char;
AWidth, AGlyphOffset: Double): TdxPDFGlyph;
begin
  Result := TdxPDFGlyph.Create(AGlyphIndex, AWidth, AGlyphOffset);
end;

function TdxPDFEmbeddedGlyphMapper.CreateGlyphRun(const AGlyphs
  : TdxPDFGlyphList): TdxPDFGlyphRun;
begin
  Result := TdxPDFCompositeFontGlyphRun2.Create(AGlyphs);
end;

{ TdxPDFDocumentState }

function TdxPDFDocumentState.GetRepository: TdxPDFDocumentRepository;
begin
  Result := TdxPDFDocumentAccess(Parent as TdxPDFDocument).Repository;
end;

function TdxPDFDocumentState.CreateFontData(const AFontFamilyName: string;
AFontStyle: TdxGPFontStyle): TObject;
begin
  Result := FontDataStorage.SearchFontData(AFontFamilyName, AFontStyle);
end;

function TdxPDFDocumentState.GetPageIndex(APage: TdxPDFPage): Integer;
begin
  Result := TdxPDFDocumentAccess(Parent as TdxPDFDocument).Pages.IndexOf(APage);
end;

function TdxPDFDocumentState.SearchFontData(AFontCommand
  : TdxPDFCustomCommand): TObject;
var
  AFontName: string;
  AFontStyle: TdxGPFontStyle;
  AIsEmptyFontName: Boolean;
  APitchAndFamily: Byte;
begin
  if AFontCommand <> nil then
  begin
    AIsEmptyFontName := False;
    APitchAndFamily := DEFAULT_PITCH;
    CalculateFontParameters(AFontCommand, AFontName, AFontStyle,
      APitchAndFamily, AIsEmptyFontName);
    Result := CreateFontData(AFontName, AFontStyle);
    if (Result = nil) and not AIsEmptyFontName then
      if TdxStringHelper.Contains(AFontName,
        TdxPDFKeywords.TimesNewRomanFontName2) or
        TdxStringHelper.Contains(AFontName, TdxPDFKeywords.TimesNewRomanFontName)
      then
        Result := CreateFontData(TdxPDFKeywords.TimesNewRomanFontName,
          AFontStyle)
      else if TdxStringHelper.Contains(AFontName, TdxPDFKeywords.CourierFontName)
      then
        Result := CreateFontData(TdxPDFKeywords.CourierFontName, AFontStyle)
      else if TdxStringHelper.Contains(AFontName, TdxPDFKeywords.ArialFontName)
      then
        Result := CreateFontData(TdxPDFKeywords.ArialFontName, AFontStyle)
      else if APitchAndFamily = FF_ROMAN then
        Result := CreateFontData(TdxPDFKeywords.TimesNewRomanFontName2,
          AFontStyle)
      else if APitchAndFamily = FIXED_PITCH then
        Result := CreateFontData(TdxPDFKeywords.CourierNewFontName2, AFontStyle)
      else
        Result := CreateFontData(TdxPDFKeywords.ArialFontName, AFontStyle);
  end
  else
    Result := CreateFontData(TdxPDFKeywords.TimesNewRomanFontName2,
      TdxGPFontStyle.FontStyleRegular);
end;

function TdxPDFDocumentState.GetImageDataStorage
  : TdxPDFDocumentImageDataStorage;
begin
  Result := GetRepository.ImageDataStorage;
end;

function TdxPDFDocumentState.GetFontDataStorage: TdxPDFFontDataStorage;
begin
  Result := GetRepository.FontDataStorage;
end;

procedure TdxPDFDocumentState.SetRotationAngle(const AValue: TcxRotationAngle);
begin
  if FRotationAngle <> AValue then
  begin
    FRotationAngle := AValue;
    dxCallNotify(OnRotationAngleChanged, Self);
  end;
end;

procedure TdxPDFDocumentState.CalculateFontParameters
  (ACommand: TdxPDFCustomCommand; out AFontName: string;
out AFontStyle: TdxGPFontStyle; var APitchAndFamily: Byte;
var AIsEmptyFontName: Boolean);
var
  ARegistrationData: TdxPDFFontRegistrationData;
begin
  AFontStyle := TdxGPFontStyle.FontStyleRegular;
  if (ACommand is TdxPDFSetTextFontCommand) and
    (TdxPDFSetTextFontCommand(ACommand).Font <> nil) then
  begin
    ARegistrationData := FontDataStorage.CreateSubstituteFontData
      (TdxPDFSetTextFontCommand(ACommand).Font);
    if ARegistrationData.Weight > 400 then
      AFontStyle := TdxGPFontStyle(Integer(AFontStyle) or
        Integer(TdxGPFontStyle.FontStyleBold));
    if ARegistrationData.Italic then
      AFontStyle := TdxGPFontStyle(Integer(AFontStyle) or
        Integer(TdxGPFontStyle.FontStyleItalic));
    AFontName := ARegistrationData.Name;
    APitchAndFamily := ARegistrationData.PitchAndFamily;
  end
  else
    AFontName := TdxPDFSetTextFontCommand(ACommand).FontName;
  AIsEmptyFontName := AFontName = '';
  if AIsEmptyFontName then
    AFontName := TdxPDFKeywords.TimesNewRomanFontName2;
end;

{ TdxPDFStreamObject }

procedure TdxPDFStreamObject.CreateSubClasses;
begin
  inherited CreateSubClasses;
  Stream := nil;
end;

procedure TdxPDFStreamObject.DestroySubClasses;
begin
  Stream := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFStreamObject.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  Stream := Repository.GetStream(ADictionary.Number);
end;

procedure TdxPDFStreamObject.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  inherited;
  WriteData(AHelper, ADictionary);
end;

procedure TdxPDFStreamObject.WriteData(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  ADictionary.SetStreamData(GetData);
end;

function TdxPDFStreamObject.GetData: TBytes;
begin
  Result := Stream.DecryptedData;
end;

function TdxPDFStreamObject.GetUncompressedData: TBytes;
begin
  Result := Stream.UncompressedData;
end;

procedure TdxPDFStreamObject.SetStream(const AValue: TdxPDFStream);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FStream));
end;

{ TdxPDFResources }

class function TdxPDFResources.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Resources;
end;

function TdxPDFResources.AddFont(AFont: TdxPDFCustomFont): string;
begin
  Result := Fonts.Add(AFont);
end;

function TdxPDFResources.GetColorSpace(const AName: string)
  : TdxPDFCustomColorSpace;
var
  AOwnerResources: TdxPDFResources;
begin
  Result := InternalGetColorSpace(AName);
  if Result = nil then
  begin
    AOwnerResources := GetOwnerResources;
    if AOwnerResources <> nil then
      Result := AOwnerResources.GetColorSpace(AName);
  end;
end;

function TdxPDFResources.GetFont(const AName: string): TdxPDFCustomFont;
var
  AOwnerResources: TdxPDFResources;
begin
  Result := InternalGetFont(AName);
  if Result = nil then
  begin
    AOwnerResources := GetOwnerResources;
    if AOwnerResources <> nil then
      Result := AOwnerResources.GetFont(AName);
  end;
end;

function TdxPDFResources.GetGraphicsStateParameters(const AName: string)
  : TdxPDFGraphicsStateParameters;
var
  AOwnerResources: TdxPDFResources;
begin
  Result := InternalGetGraphicsStateParameters(AName);
  if Result = nil then
  begin
    AOwnerResources := GetOwnerResources;
    if AOwnerResources <> nil then
      Result := AOwnerResources.GetGraphicsStateParameters(AName);
  end;
end;

function TdxPDFResources.GetPattern(const AName: string): TdxPDFCustomPattern;
var
  AOwnerResources: TdxPDFResources;
begin
  Result := InternalGetPattern(AName);
  if Result = nil then
  begin
    AOwnerResources := GetOwnerResources;
    if AOwnerResources <> nil then
      Result := AOwnerResources.GetPattern(AName);
  end;
end;

function TdxPDFResources.GetShading(const AName: string): TdxPDFCustomShading;
var
  AOwnerResources: TdxPDFResources;
begin
  Result := InternalGetShading(AName);
  if Result = nil then
  begin
    AOwnerResources := GetOwnerResources;
    if AOwnerResources <> nil then
      Result := AOwnerResources.GetShading(AName);
  end;
end;

function TdxPDFResources.GetXObject(const AName: string): TdxPDFXObject;
var
  AOwnerResources: TdxPDFResources;
begin
  Result := InternalGetXObject(AName);
  if Result = nil then
  begin
    AOwnerResources := GetOwnerResources;
    if AOwnerResources <> nil then
      Result := AOwnerResources.GetXObject(AName);
  end;
end;

function TdxPDFResources.GetProperties(const AName: string)
  : TdxPDFCustomProperties;
begin
  Result := nil;
end;

procedure TdxPDFResources.Append(AResources: TdxPDFResources);
begin
  if AResources.Fonts.Count > 0 then
    Fonts.Append(AResources.Fonts);
  if AResources.GraphicStatesParametersList.Count > 0 then
    GraphicStatesParametersList.Append(AResources.GraphicStatesParametersList);
  if AResources.XObjects.Count > 0 then
    XObjects.Append(AResources.XObjects);
  if AResources.ColorSpaces.Count > 0 then
    ColorSpaces.Append(AResources.ColorSpaces);
end;

procedure TdxPDFResources.Pack;
begin
  if (ReferenceCount = 1) or Repository.IsResourcesShared(Self) and
    (ReferenceCount <= 2) then
    Clear;
end;

procedure TdxPDFResources.CreateSubClasses;
begin
  inherited CreateSubClasses;
  InitializeCriticalSectionAndSpinCount(FLock, 1024);
end;

procedure TdxPDFResources.DestroySubClasses;
begin
  Clear;
  Dictionary := nil;
  DeleteCriticalSection(FLock);
  inherited DestroySubClasses;
end;

procedure TdxPDFResources.Initialize;
begin
  inherited Initialize;
  FID := TdxPDFUtils.GenerateGUID;
end;

procedure TdxPDFResources.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  if ADictionary <> nil then
    Dictionary := ADictionary;
end;

procedure TdxPDFResources.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
var
  AArray: TdxPDFArray;
begin
  inherited Write(AHelper, ADictionary);

  AArray := TdxPDFArray.Create;
  AArray.AddName('PDF');
  AArray.AddName('Text');
  AArray.AddName('ImageB');
  AArray.AddName('ImageC');
  AArray.AddName('ImageI');
  ADictionary.Add('ProcSet', AArray);

  ADictionary.Add(TdxPDFColorSpaces.GetTypeName, ColorSpaces);
  ADictionary.Add(TdxPDFFonts.GetTypeName, Fonts);
  ADictionary.Add(TdxPDFGraphicsStateParametersList.GetTypeName,
    GraphicStatesParametersList);
  ADictionary.Add(TdxPDFPatterns.GetTypeName, Patterns);
  ADictionary.Add(TdxPDFShadings.GetTypeName, Shadings);
  ADictionary.Add(TdxPDFXObjects.GetTypeName, XObjects);
end;

function TdxPDFResources.GetOwnerResources: TdxPDFResources;
begin
  if Parent is TdxPDFPageData then
    Result := TdxPDFPageData(Parent).Resources
  else if Parent is TdxPDFPages then
    Result := TdxPDFPages(Parent).Resources
  else
    Result := nil;
end;

function TdxPDFResources.InternalGetColorSpace(const AName: string)
  : TdxPDFCustomColorSpace;
begin
  Result := ColorSpaces.GetColorSpace(AName);
end;

function TdxPDFResources.InternalGetFont(const AName: string): TdxPDFCustomFont;
begin
  Result := Fonts.GetFont(AName);
end;

function TdxPDFResources.InternalGetGraphicsStateParameters(const AName: string)
  : TdxPDFGraphicsStateParameters;
begin
  Result := GraphicStatesParametersList.GetParameters(AName)
    as TdxPDFGraphicsStateParameters;
end;

function TdxPDFResources.InternalGetPattern(const AName: string)
  : TdxPDFCustomPattern;
begin
  Result := Patterns.GetPattern(AName);
end;

function TdxPDFResources.InternalGetShading(const AName: string)
  : TdxPDFCustomShading;
begin
  Result := Shadings.GetShading(AName);
end;

function TdxPDFResources.InternalGetXObject(const AName: string): TdxPDFXObject;
begin
  Result := XObjects.GetXObject(AName);
end;

function TdxPDFResources.TryGetColorSpaceName(AObject: TdxPDFCustomColorSpace;
out AName: string): Boolean;
begin
  Result := TryGetResourceName(FColorSpaces, AObject, AName);
end;

function TdxPDFResources.TryGetResourceName(AResources: TdxPDFObjectList;
AObject: TdxPDFCustomColorSpace; out AName: string): Boolean;
begin
  Result := (AResources <> nil) and AResources.TryGetObjectName(AObject, AName);
end;

function TdxPDFResources.AddGraphicsStateParameters
  (AParameters: TdxPDFGraphicsStateParameters): string;
begin
  Result := FGraphicStatesParametersList.Add(AParameters);
end;

function TdxPDFResources.AddPattern(APattern: TdxPDFCustomPattern): string;
begin
  Result := FPatterns.Add(APattern);
end;

function TdxPDFResources.AddXObject(ANumber: Integer): string;
begin
  Result := FXObjects.AddReference(ANumber);
end;

function TdxPDFResources.GetColorSpaces: TdxPDFColorSpaces;
begin
  GetList(FColorSpaces, TdxPDFColorSpaces, TdxPDFKeywords.ColorSpace);
  Result := FColorSpaces;
end;

function TdxPDFResources.GetFonts: TdxPDFFonts;
begin
  GetList(FFonts, TdxPDFFonts, TdxPDFKeywords.Font);
  Result := FFonts;
end;

procedure TdxPDFResources.GetList(var AVariable; AClass: TdxPDFObjectListClass;
const AKey: string; AInitProc: TListInitProc = nil);
begin
  if TdxPDFObjectList(AVariable) = nil then
  begin
    EnterCriticalSection(FLock);
    try
      if TdxPDFObjectList(AVariable) = nil then
      begin
        TdxPDFObjectList(AVariable) := AClass.Create(Self);
        if Dictionary <> nil then
          TdxPDFObjectList(AVariable).Read(Dictionary.GetDictionary(AKey));
        if Assigned(AInitProc) then
          AInitProc(TdxPDFObjectList(AVariable));
      end;
    finally
      LeaveCriticalSection(FLock);
    end;
  end;
end;

function TdxPDFResources.GetGraphicStatesParametersList
  : TdxPDFGraphicsStateParametersList;
begin
  GetList(FGraphicStatesParametersList, TdxPDFGraphicsStateParametersList,
    TdxPDFKeywords.ExtGState);
  Result := FGraphicStatesParametersList;
end;

function TdxPDFResources.GetPatterns: TdxPDFPatterns;
begin
  GetList(FPatterns, TdxPDFPatterns, TdxPDFKeywords.Pattern);
  Result := FPatterns;
end;

function TdxPDFResources.GetShadings: TdxPDFShadings;
begin
  GetList(FShadings, TdxPDFShadings, TdxPDFKeywords.Shading);
  Result := FShadings;
end;

function TdxPDFResources.GetXObjects: TdxPDFXObjects;
begin
  GetList(FXObjects, TdxPDFXObjects, TdxPDFKeywords.XObject, InitXObjects);
  Result := FXObjects;
end;

procedure TdxPDFResources.InitXObjects(AList: TdxPDFObjectList);
begin
  AList.Enum(procedure(AObject: TdxPDFObject)
    begin
      if AObject is TdxPDFForm then
      begin
        if TdxPDFForm(AObject).Resources = nil then
          TdxPDFForm(AObject).Resources := Self;
      end;
    end);
end;

procedure TdxPDFResources.SetColorSpaces(const AValue: TdxPDFColorSpaces);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FColorSpaces));
end;

procedure TdxPDFResources.SetDictionary(const AValue: TdxPDFReaderDictionary);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FDictionary));
end;

procedure TdxPDFResources.SetFonts(const AValue: TdxPDFFonts);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FFonts));
end;

procedure TdxPDFResources.SetGraphicStatesParametersList
  (const AValue: TdxPDFGraphicsStateParametersList);
begin
  dxPDFChangeValue(AValue,
    TdxPDFReferencedObject(FGraphicStatesParametersList));
end;

procedure TdxPDFResources.SetPatterns(const AValue: TdxPDFPatterns);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FPatterns));
end;

procedure TdxPDFResources.SetShadings(const AValue: TdxPDFShadings);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FShadings));
end;

procedure TdxPDFResources.SetXObjects(const AValue: TdxPDFXObjects);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FXObjects));
end;

procedure TdxPDFResources.Clear;
begin
  Patterns := nil;
  Shadings := nil;
  XObjects := nil;
  GraphicStatesParametersList := nil;
  Fonts := nil;
  ColorSpaces := nil;
end;

{ TdxPDFPageContents }

class function TdxPDFPageContents.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Contents;
end;

function TdxPDFPageContents.GetData: TBytes;
var
  I: Integer;
begin
  SetLength(Result, 0);
  for I := 0 to ContentList.Count - 1 do
  begin
    TdxPDFUtils.AddData((ContentList[I] as TdxPDFStreamObject)
      .Stream.UncompressedData, Result);
    TdxPDFUtils.AddByte(TdxPDFDefinedSymbols.Space, Result);
  end;
end;

procedure TdxPDFPageContents.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FCommands := TdxPDFCommandList.Create;
  FContentList := TdxPDFReferencedObjects.Create;
end;

procedure TdxPDFPageContents.DestroySubClasses;
begin
  FreeAndNil(FContentList);
  FreeAndNil(FCommands);
  inherited DestroySubClasses;
end;

procedure TdxPDFPageContents.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  ReadContentList(ADictionary, ADictionary.GetObject(TdxPDFKeywords.Contents));
end;

procedure TdxPDFPageContents.WriteData(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  ADictionary.SetStreamData(Commands.ToByteArray(Resources));
end;

procedure TdxPDFPageContents.ClearCommands;
begin
  Commands.Clear;
end;

procedure TdxPDFPageContents.PopulateCommands(AResources: TdxPDFResources);
begin
  Commands.Read(GetData, Repository, AResources);
end;

function TdxPDFPageContents.GetCommandCount: Integer;
begin
  Result := Commands.GetCommandCount;
end;

function TdxPDFPageContents.GetResources: TdxPDFResources;
begin
  Result := (Parent as TdxPDFPageData).Resources;
end;

procedure TdxPDFPageContents.ReadItem(AStream: TdxPDFStream);
var
  AContentItem: TdxPDFPageContentItem;
begin
  AContentItem := TdxPDFPageContentItem.Create(Self);
  AStream.Dictionary.Number := AStream.Number;
  AContentItem.Read(AStream.Dictionary as TdxPDFReaderDictionary);
  TdxPDFDocumentStreamObjectAccess(AContentItem).Stream := AStream;
  ContentList.Add(AContentItem);
end;

procedure TdxPDFPageContents.ReadContentList(ADictionary
  : TdxPDFReaderDictionary; AContentObject: TdxPDFBase);
var
  I: Integer;
  AStream: TdxPDFBase;
begin
  if (AContentObject <> nil) and not((AContentObject.ObjectType = otDictionary)
    and (TdxPDFDictionary(AContentObject).Count = 0)) then
    case AContentObject.ObjectType of
      otIndirectReference, otDictionary, otStream:
        if GetObject(TdxPDFKeywords.Contents, ADictionary, AStream) then
          if AStream.ObjectType = otStream then
            ReadItem(AStream as TdxPDFStream)
          else
            ReadContentList(ADictionary, AStream);
      otArray:
        for I := 0 to (AContentObject as TdxPDFArray).Count - 1 do
        begin
          AStream := Repository.GetStream
            ((TdxPDFArray(AContentObject)[I] as TdxPDFBase).Number);
          if AStream <> nil then
          begin
            AStream.Number :=
              (TdxPDFArray(AContentObject)[I] as TdxPDFBase).Number;
            if AStream <> nil then
              ReadItem(TdxPDFStream(AStream));
          end;
        end;
    end;
end;

{ TdxPDFPageTreeObject }

procedure TdxPDFPageTreeObject.DestroySubClasses;
begin
  Resources := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFPageTreeObject.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
var
  AParentNode: TdxPDFPageTreeObject;
begin
  inherited Write(AHelper, ADictionary);

  AParentNode := ParentNode;
  if AParentNode = nil then
  begin
    ADictionary.Add(TdxPDFKeywords.MediaBox, MediaBox);
    ADictionary.Add(TdxPDFKeywords.CropBox, CropBox, MediaBox);
    ADictionary.Add(TdxPDFKeywords.Rotate, RotationAngle, 0);
  end
  else
  begin
    ADictionary.Add(TdxPDFKeywords.MediaBox, MediaBox, GetParentNodeMediaBox);
    ADictionary.Add(TdxPDFKeywords.CropBox, CropBox, AParentNode.CropBox);
    ADictionary.Add(TdxPDFKeywords.Rotate, RotationAngle,
      AParentNode.RotationAngle);
    if AParentNode is TdxPDFPageTreeObjectList then
      ADictionary.AddReference(TdxPDFKeywords.Parent,
        TdxPDFPageTreeObjectList(ParentNode).Parent as TdxPDFObject);
  end;
end;

procedure TdxPDFPageTreeObject.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  ReadResources(ADictionary);
end;

procedure TdxPDFPageTreeObject.ReadProperties(ADictionary
  : TdxPDFReaderDictionary);
begin
  inherited ReadProperties(ADictionary);

  Repository := ADictionary.Repository;
  FArtBox := ADictionary.GetRectangle(TdxPDFKeywords.ArtBox, FArtBox);
  FBleedBox := ADictionary.GetRectangle(TdxPDFKeywords.BleedBox, FBleedBox);
  FCropBox := ADictionary.GetRectangle(TdxPDFKeywords.CropBox, FCropBox);
  FMediaBox := ADictionary.GetRectangle(TdxPDFKeywords.MediaBox, FMediaBox);
  FTrimBox := ADictionary.GetRectangle(TdxPDFKeywords.TrimBox, FTrimBox);
  FUserUnit := ADictionary.GetInteger(TdxPDFKeywords.UserUnit, 1);
  if ADictionary.Contains(TdxPDFKeywords.Rotate) then
  begin
    FRotationAngle := ADictionary.GetInteger(TdxPDFKeywords.Rotate);
    FUseParentRotationAngle := False;
  end
  else
  begin
    FUseParentRotationAngle := True;
    FRotationAngle := 0;
  end;
end;

function TdxPDFPageTreeObject.GetLeafCount: Integer;
begin
  Result := 1;
end;

procedure TdxPDFPageTreeObject.Clear;
begin
  DestroySubClasses;
  CreateSubClasses;
end;

function TdxPDFPageTreeObject.GetArtBox: TdxRectF;
begin
  Result := FArtBox;
  if TdxPDFUtils.IsRectEmpty(FArtBox) then
    Result := GetParentNodeArtBox;
  if TdxPDFUtils.IsRectEmpty(Result) then
    Result := CropBox;
end;

function TdxPDFPageTreeObject.GetBleedBox: TdxRectF;
begin
  if TdxPDFUtils.IsRectEmpty(FBleedBox) then
    Result := GetParentNodeBleedBox
  else
    Result := FBleedBox;

  if TdxPDFUtils.IsRectEmpty(Result) then
    Result := CropBox;
end;

function TdxPDFPageTreeObject.GetCropBox: TdxRectF;
begin
  if TdxPDFUtils.IsRectEmpty(FCropBox) then
    Result := MediaBox
  else
    Result := FCropBox;
end;

function TdxPDFPageTreeObject.GetMediaBox: TdxRectF;
begin
  if TdxPDFUtils.IsRectEmpty(FMediaBox) then
    Result := GetParentNodeMediaBox
  else
    Result := FMediaBox;
end;

function TdxPDFPageTreeObject.GetTrimBox: TdxRectF;
begin
  if TdxPDFUtils.IsRectEmpty(FTrimBox) then
    Result := GetParentNodeTrimBox
  else
    Result := FTrimBox;
end;

function TdxPDFPageTreeObject.GetUserUnit: Integer;
begin
  if FUserUnit = 0 then
    Result := GetParentNodeUserUnit
  else
    Result := FUserUnit;
end;

function TdxPDFPageTreeObject.GetParentNode: TdxPDFPageTreeObject;
begin
  if Parent is TdxPDFPageTreeObject then
    Result := TdxPDFPageTreeObject(Parent)
  else
    Result := nil;
end;

function TdxPDFPageTreeObject.GetParentNodeArtBox: TdxRectF;
begin
  if ParentNode <> nil then
    Result := ParentNode.ArtBox
  else
    Result := dxNullRectF;
end;

function TdxPDFPageTreeObject.GetParentNodeBleedBox: TdxRectF;
begin
  if GetParentNode <> nil then
    Result := GetParentNode.BleedBox
  else
    Result := dxNullRectF;
end;

function TdxPDFPageTreeObject.GetParentNodeMediaBox: TdxRectF;
begin
  if ParentNode <> nil then
    Result := ParentNode.MediaBox
  else
    Result := dxNullRectF;
end;

function TdxPDFPageTreeObject.GetParentNodeRotationAngle: Integer;
begin
  if ParentNode <> nil then
    Result := ParentNode.RotationAngle
  else
    Result := 0;
end;

function TdxPDFPageTreeObject.GetParentNodeTrimBox: TdxRectF;
begin
  if ParentNode <> nil then
    Result := ParentNode.TrimBox
  else
    Result := dxNullRectF;
end;

function TdxPDFPageTreeObject.GetParentNodeUserUnit: Integer;
begin
  if ParentNode <> nil then
    Result := ParentNode.UserUnit
  else
    Result := 1;
end;

function TdxPDFPageTreeObject.GetResources: TdxPDFResources;
begin
  if FResources <> nil then
    Result := FResources
  else
    Result := GetParentNodeResources;
end;

function TdxPDFPageTreeObject.GetRotationAngle: Integer;
begin
  if UseParentRotationAngle then
    Result := GetParentNodeRotationAngle
  else
    Result := FRotationAngle;
end;

function TdxPDFPageTreeObject.GetParentNodeResources: TdxPDFResources;
begin
  if Parent is TdxPDFPageTreeObject then
    Result := TdxPDFPageTreeObject(Parent).Resources
  else
    Result := nil;
end;

procedure TdxPDFPageTreeObject.SetResources(const AValue: TdxPDFResources);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FResources));
end;

procedure TdxPDFPageTreeObject.ReadResources(ADictionary
  : TdxPDFReaderDictionary);
begin
  Resources := Repository.GetResources(ADictionary);
end;

{ TdxPDFPageTreeObjectList }

class function TdxPDFPageTreeObjectList.GetTypeName: string;
begin
  Result := TdxPDFPages.GetTypeName;
end;

procedure TdxPDFPageTreeObjectList.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FChildren := TObjectList<TdxPDFPageTreeObject>.Create;
end;

procedure TdxPDFPageTreeObjectList.DestroySubClasses;
begin
  FreeAndNil(FChildren);
  inherited DestroySubClasses;
end;

procedure TdxPDFPageTreeObjectList.Read(ADictionary: TdxPDFReaderDictionary);

  procedure AddNode(ANodeDictionary: TdxPDFReaderDictionary; ANumber: Integer);
  var
    ANode: TdxPDFPageTreeObject;
    ANodeType: string;
  begin
    if ANodeDictionary <> nil then
    begin
      ANodeDictionary.Number := ANumber;
      ANodeType := ANodeDictionary.GetString(TdxPDFKeywords.TypeKey);
      if ANodeType = TdxPDFKeywords.Pages then
      begin
        ANode := TdxPDFPageTreeNode.Create(Self);
        TdxPDFPageTreeNode(ANode).OnCreatePageNode := OnCreatePageNode;
        ANode.Read(ANodeDictionary);
        Add(ANode);
      end
      else if Assigned(OnCreatePageNode) then
        Add(OnCreatePageNode(Self, ANodeDictionary));
    end;
  end;

var
  I: Integer;
  ANodeDictionary: TdxPDFReaderDictionary;
  AType: string;
  AKidReferences: TdxPDFArray;
begin
  if ADictionary <> nil then
  begin
    ReadProperties(ADictionary);
    AType := ADictionary.GetString(TdxPDFKeywords.TypeKey);
    AKidReferences := ADictionary.GetArray(TdxPDFKeywords.Kids);
    if ((AType = TdxPDFKeywords.Pages) or (AType = '')) and
      (AKidReferences <> nil) then
      for I := 0 to AKidReferences.Count - 1 do
      begin
        ANodeDictionary := Repository.GetDictionary(AKidReferences[I].Number);
        AddNode(ANodeDictionary, AKidReferences[I].Number);
      end
    else if AType = TdxPDFKeywords.Page then
      AddNode(ADictionary, ADictionary.Number);
  end;
end;

procedure TdxPDFPageTreeObjectList.Add(AChild: TdxPDFPageTreeObject);
begin
  FChildren.Add(AChild);
end;

function TdxPDFPageTreeObjectList.GetCount: Integer;
begin
  Result := FChildren.Count;
end;

function TdxPDFPageTreeObjectList.GetItem(AIndex: Integer)
  : TdxPDFPageTreeObject;
begin
  Result := FChildren[AIndex];
end;

{ TdxPDFPageTreeNode }

procedure TdxPDFPageTreeNode.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FNodeList := TdxPDFPageTreeObjectList.Create(Self);
  FNodeList.OnCreatePageNode := OnCreatePageNodeHandler;
end;

procedure TdxPDFPageTreeNode.DestroySubClasses;
begin
  FreeAndNil(FNodeList);
  inherited DestroySubClasses;
end;

procedure TdxPDFPageTreeNode.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);
  if ADictionary <> nil then
    FNodeList.Read(ADictionary);
end;

procedure TdxPDFPageTreeNode.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
var
  AArray: TdxPDFWriterArray;
  APageIndex: Integer;
  APageNode: TdxPDFPageTreeObject;
begin
  inherited Write(AHelper, ADictionary);

  AArray := AHelper.CreateArray;
  for APageIndex := 0 to FNodeList.Count - 1 do
  begin
    APageNode := FNodeList[APageIndex];
    if APageNode.GetLeafCount > 0 then
      AArray.AddReference(APageNode);
  end;
  if AArray.Count > 0 then
  begin
    ADictionary.AddName(TdxPDFKeywords.TypeKey, TdxPDFKeywords.Pages);
    ADictionary.Add(TdxPDFKeywords.Kids, AArray);
  end
  else
    AArray.Free;

  ADictionary.Add(TdxPDFKeywords.CountFull, GetLeafCount);
end;

function TdxPDFPageTreeNode.GetLeafCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to FNodeList.Count - 1 do
    Inc(Result, FNodeList[I].GetLeafCount);
end;

procedure TdxPDFPageTreeNode.AddNode(AChild: TdxPDFPageTreeObject);
begin
  FNodeList.Add(AChild);
end;

function TdxPDFPageTreeNode.OnCreatePageNodeHandler
  (AOwner: TdxPDFPageTreeObjectList; ADictionary: TdxPDFReaderDictionary)
  : TdxPDFPageTreeObject;
begin
  if Assigned(OnCreatePageNode) then
    Result := OnCreatePageNode(AOwner, ADictionary)
  else
    Result := TdxPDFPageTreeObject.Create(AOwner);
end;

{ TdxPDFPages }

class function TdxPDFPages.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Pages;
end;

procedure TdxPDFPages.Clear;
begin
  inherited Clear;
  FDictionary.Clear;
end;

procedure TdxPDFPages.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FPageList := TList<TdxPDFPage>.Create;
  OnCreatePageNode := OnCreatePageNodeHandler;
  FDictionary := TDictionary<Integer, TdxPDFPage>.Create;
end;

procedure TdxPDFPages.DestroySubClasses;
begin
  FreeAndNil(FDictionary);
  FreeAndNil(FPageList);
  inherited DestroySubClasses;
end;

function TdxPDFPages.FindPage(ANumber: Integer): TdxPDFPage;
begin
  if not FDictionary.TryGetValue(ANumber, Result) then
    Result := nil;
end;

function TdxPDFPages.IndexOf(APage: TdxPDFPage): Integer;
begin
  Result := FPageList.IndexOf(APage);
end;

procedure TdxPDFPages.AddPage(APage: TdxPDFPage);
begin
  FPageList.Add(APage);
  FDictionary.Add(APage.Number, APage);
end;

function TdxPDFPages.GetCount: Integer;
begin
  Result := FPageList.Count;
end;

function TdxPDFPages.GetPage(AIndex: Integer): TdxPDFPage;
begin
  Result := FPageList[AIndex] as TdxPDFPage;
end;

function TdxPDFPages.OnCreatePageNodeHandler(AOwner: TdxPDFPageTreeObjectList;
ADictionary: TdxPDFReaderDictionary): TdxPDFPageTreeObject;

  function CreatePage(const AInfo: TdxPDFDeferredObjectInfo): TdxPDFPage;
  begin
    Result := TdxPDFPage.Create(AOwner, AInfo);
    Result.OnPack := OnPagePack;
    Result.Read(ADictionary);
    AddPage(Result);
  end;

var
  AInfo: TdxPDFDeferredObjectInfo;
begin
  AInfo.Name := GetTypeName;
  AInfo.Key := TdxPDFKeywords.TypeKey;
  AInfo.Number := ADictionary.Number;
  AInfo.SourceObject := ADictionary;
  Result := CreatePage(AInfo);
end;

{ TdxPDFFileSpecificationData }

procedure TdxPDFFileSpecificationData.CreateSubClasses;
begin
  inherited;
  FCreationDate := NullDate;
  FModificationDate := NullDate;
end;

class function TdxPDFFileSpecificationData.GetTypeName: string;
begin
  Result := TdxPDFKeywords.EmbeddedFile;
end;

procedure TdxPDFFileSpecificationData.Read(ADictionary: TdxPDFReaderDictionary);
begin
  inherited Read(ADictionary);

  SetLength(FData, 0);
  FDataStreamRef := ADictionary.StreamRef;
  FMimeType := ADictionary.GetString(TdxPDFKeywords.Subtype);
  ReadParams(ADictionary.GetDictionary(TdxPDFKeywords.FileParameters));
end;

procedure TdxPDFFileSpecificationData.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  inherited;
  ADictionary.AddName(TdxPDFKeywords.Subtype, FMimeType);
  ADictionary.SetStreamData(GetData);
  WriteParams(AHelper, ADictionary);
end;

function TdxPDFFileSpecificationData.GetData: TBytes;
begin
  if Length(FData) = 0 then
    ResolveData;
  Result := FData;
end;

function TdxPDFFileSpecificationData.GetHasModificationDate: Boolean;
begin
  Result := FModificationDate <> NullDate;
end;

procedure TdxPDFFileSpecificationData.SetData(const AValue: TBytes);
begin
  FData := AValue;
  FSize := Length(FData);
end;

procedure TdxPDFFileSpecificationData.ResolveData;
begin
  if FDataStreamRef <> nil then
    FData := FDataStreamRef.UncompressedData;
end;

procedure TdxPDFFileSpecificationData.ReadParams(ADictionary
  : TdxPDFReaderDictionary);
begin
  if ADictionary <> nil then
  begin
    FSize := ADictionary.GetInteger(TdxPDFKeywords.FileSize, 0);
    FCreationDate := ADictionary.GetDate(TdxPDFKeywords.FileCreationDate);
    FModificationDate := ADictionary.GetDate
      (TdxPDFKeywords.FileModificationDate);
  end;
end;

procedure TdxPDFFileSpecificationData.WriteParams(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
var
  AParams: TdxPDFWriterDictionary;
begin
  AParams := AHelper.CreateDictionary;
  if FSize > 0 then
    AParams.Add(TdxPDFKeywords.FileSize, FSize);
  if FCreationDate > 0 then
    AParams.AddDate(TdxPDFKeywords.FileCreationDate, FCreationDate);
  if FModificationDate > 0 then
    AParams.AddDate(TdxPDFKeywords.FileModificationDate, FModificationDate);
  if AParams.Count > 0 then
    ADictionary.Add(TdxPDFKeywords.FileParameters, AParams)
  else
    AParams.Free;
end;

{ TdxPDFCustomDestination }

class function TdxPDFCustomDestination.GetTypeName: string;
begin
  Result := 'CustomDestination';
end;

class function TdxPDFCustomDestination.Parse(ACatalog: TdxPDFCatalog;
AObject: TdxPDFBase): TdxPDFCustomDestination;

  function GetClassName(AArray: TdxPDFArray): string;
  begin
    if AArray.Count < 1 then
      Result := ''
    else
    begin
      Result := TdxPDFKeywords.XYZDestination;
      if (AArray.Count > 1) and (AArray[1] is TdxPDFString) then
        Result := TdxPDFString(AArray[1]).Value;
    end;
  end;

var
  AArray: TdxPDFArray;
  AClass: TdxPDFObjectClass;
  ADictionary: TdxPDFReaderDictionary;
begin
  Result := nil;
  if AObject <> nil then
  begin
    case AObject.ObjectType of
      otDictionary:
        AArray := TdxPDFDictionary(AObject).GetArray('D');
      otArray:
        AArray := TdxPDFArray(AObject);
      otIndirectReference:
        begin
          if ACatalog <> nil then
          begin
            AArray := ACatalog.Repository.GetArray(AObject.Number);
            if AArray = nil then
            begin
              ADictionary := ACatalog.Repository.GetDictionary(AObject.Number);
              if ADictionary <> nil then
                Result := TdxPDFCustomDestination.Parse(ACatalog,
                  ADictionary.GetObject('D'));
            end
            else
              Result := TdxPDFCustomDestination.Parse(ACatalog, AArray);
          end;
          AArray := nil;
        end;
    else
      AArray := nil;
    end;
    if (AArray <> nil) and dxPDFTryGetDocumentObjectClass(GetClassName(AArray),
      AClass) then
    begin
      Result := AClass.Create(nil) as TdxPDFCustomDestination;
      Result.Read(ACatalog, AArray);
    end;
  end;
end;

procedure TdxPDFCustomDestination.DestroySubClasses;
begin
  PageObject := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFCustomDestination.Initialize;
begin
  inherited Initialize;
  PageObject := nil;
  FPageIndex := -1;
end;

procedure TdxPDFCustomDestination.Read(ACatalog: TdxPDFCatalog;
AArray: TdxPDFArray);
begin
  if AArray <> nil then
  begin
    FCatalog := ACatalog;
    if AArray.Count < 1 then
      TdxPDFUtils.RaiseException;
    ReadParameters(AArray);
  end;
end;

function TdxPDFCustomDestination.Write(AHelper: TdxPDFWriterHelper): TdxPDFBase;
var
  AArray: TdxPDFWriterArray;
begin
  AArray := AHelper.CreateArray;
  WriteParameters(AHelper, AArray);
  Result := AArray;
end;

procedure TdxPDFCustomDestination.ReadParameters(AArray: TdxPDFArray);
begin
  PageObject := AArray[0];
end;

procedure TdxPDFCustomDestination.WriteParameters(AHelper: TdxPDFWriterHelper;
AArray: TdxPDFWriterArray);
begin
  AArray.AddReference(Page);
  AArray.AddName(GetTypeName);
end;

procedure TdxPDFCustomDestination.WriteSingleValue(AArray: TdxPDFWriterArray;
const AValue: Single);
begin
  if SameValue(AValue, dxPDFInvalidValue) then
    AArray.AddNull
  else
    AArray.Add(AValue);
end;

function TdxPDFCustomDestination.IsSame(ADestination
  : TdxPDFCustomDestination): Boolean;
begin
  Result := (FPage = ADestination.Page) and
    (FPageIndex = ADestination.PageIndex);
end;

procedure TdxPDFCustomDestination.ResolveInternalPage;
begin
  ResolvePage;
  if (FPage = nil) and (Catalog <> nil) and (FPageIndex >= 0) and
    (FPageIndex < Catalog.Pages.Count) then
  begin
    FPage := Catalog.Pages[FPageIndex];
    FPageIndex := -1;
  end;
end;

class function TdxPDFCustomDestination.GetSingleValue
  (AArray: TdxPDFArray): Single;
begin
  case AArray.Count of
    2:
      Result := dxPDFInvalidValue;
    3:
      Result := TdxPDFUtils.ConvertToSingle(AArray[2]);
  else
    TdxPDFUtils.RaiseException;
    Result := dxPDFInvalidValue;
  end;
end;

function TdxPDFCustomDestination.CalculatePageIndex
  (APages: TdxPDFPages): Integer;
begin
  if Page = nil then
    Result := FPageIndex
  else
    Result := APages.IndexOf(FPage);
end;

function TdxPDFCustomDestination.ValidateVerticalCoordinate
  (ATop: Single): Single;
begin
  Result := ATop;
  if TdxPDFUtils.IsDoubleValid(ATop) and (FPage <> nil) then
    Result := TdxPDFUtils.Min(ATop, Abs(FPage.CropBox.Height));
end;

function TdxPDFCustomDestination.GetPage: TdxPDFPage;
begin
  ResolvePage;
  Result := FPage;
end;

function TdxPDFCustomDestination.GetPageIndex: Integer;
begin
  ResolvePage;
  if FPageIndex >= 0 then
    Result := FPageIndex
  else
    Result := CalculatePageIndex(FCatalog.Pages);
end;

function TdxPDFCustomDestination.GetPages: TdxPDFPages;
begin
  if FCatalog <> nil then
    Result := FCatalog.Pages
  else
    Result := nil;
end;

procedure TdxPDFCustomDestination.SetPageObject(const AValue: TdxPDFBase);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FPageObject));
end;

procedure TdxPDFCustomDestination.ResolvePage;
begin
  if FPageObject <> nil then
  begin
    if TdxPDFUtils.IsReference(FPageObject) then
      FPage := Catalog.Pages.FindPage(FPageObject.Number)
    else if FPageObject.ObjectType = otInteger then
      FPageIndex := TdxPDFInteger(FPageObject).Value;
    PageObject := nil;
  end;
end;

{ TdxPDFFileSpecification }

class function TdxPDFFileSpecification.GetTypeName: string;
begin
  Result := TdxPDFKeywords.FileSpec;
end;

class function TdxPDFFileSpecification.Parse(ADictionary
  : TdxPDFReaderDictionary): TdxPDFFileSpecification;
begin
  Result := nil;
  if ADictionary <> nil then
  begin
    Result := TdxPDFFileSpecification.Create('');
    Result.Read(ADictionary);
  end;
end;

constructor TdxPDFFileSpecification.Create(const AFileName: string);
begin
  inherited Create(nil);
  FFileName := AFileName;
end;

procedure TdxPDFFileSpecification.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FFileSpecificationData := TdxPDFFileSpecificationData.Create(nil);
  FAttachment := nil;
end;

procedure TdxPDFFileSpecification.DestroySubClasses;
begin
  FreeAndNil(FAttachment);
  FreeAndNil(FFileSpecificationData);
  inherited DestroySubClasses;
end;

procedure TdxPDFFileSpecification.ReadProperties(ADictionary
  : TdxPDFReaderDictionary);
var
  AType: string;
begin
  inherited;

  AType := ADictionary.GetString(TdxPDFKeywords.TypeKey);
  if (AType <> '') and ((AType = TdxPDFKeywords.FileSpec) or
    (AType = GetTypeName) or (AType <> TdxPDFKeywords.FileName)) then
  begin
    FDescription := ADictionary.GetTextString(TdxPDFKeywords.FileDescription);
    FFileSystem := ADictionary.GetString(TdxPDFKeywords.FileSystem);
    ReadFileName(ADictionary);
    ReadFileSpecificationData
      (ADictionary.GetDictionary(TdxPDFKeywords.EmbeddedFileReference));
    ReadFileIndex(ADictionary.GetDictionary(TdxPDFKeywords.CollectionItem));
    ReadAssociatedFileRelationship(ADictionary);
  end;
end;

procedure TdxPDFFileSpecification.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  inherited;
  ADictionary.Add(TdxPDFKeywords.FileDescription, FDescription);
  ADictionary.Add(TdxPDFKeywords.FileSystem, FFileSystem);
  ADictionary.Add(TdxPDFKeywords.FileName, FFileName);
  ADictionary.Add(TdxPDFKeywords.AssociatedFileRelationship,
    AssociatedFileRelationshipNameMap[FRelationship]);
  ADictionary.Add(TdxPDFKeywords.EmbeddedFileReference,
    WriteFileSpecificationData(AHelper));
  WriteFileIndex(AHelper, ADictionary);
end;

function TdxPDFFileSpecification.GetMimeType: string;
begin
  Result := FFileSpecificationData.MimeType;
end;

procedure TdxPDFFileSpecification.SetMimeType(const AValue: string);
begin
  FFileSpecificationData.MimeType := AValue;
end;

function TdxPDFFileSpecification.GetCreationDate: TDateTime;
begin
  Result := FFileSpecificationData.CreationDate;
end;

procedure TdxPDFFileSpecification.SetCreationDate(const AValue: TDateTime);
begin
  FFileSpecificationData.CreationDate := AValue;
end;

function TdxPDFFileSpecification.GetModificationDate: TDateTime;
begin
  Result := FFileSpecificationData.ModificationDate;
end;

function TdxPDFFileSpecification.GetHasModificationDate: Boolean;
begin
  Result := FFileSpecificationData.HasModificationDate;
end;

procedure TdxPDFFileSpecification.SetModificationDate(const AValue: TDateTime);
begin
  FFileSpecificationData.ModificationDate := AValue;
end;

function TdxPDFFileSpecification.GetFileData: TBytes;
begin
  Result := FFileSpecificationData.Data;
end;

procedure TdxPDFFileSpecification.SetFileData(const AValue: TBytes);
begin
  FFileSpecificationData.Data := AValue;
end;

function TdxPDFFileSpecification.GetSize: Integer;
begin
  if TdxPDFUtils.IsIntegerValid(FFileSpecificationData.Size) then
    Result := FFileSpecificationData.Size
  else
    Result := 0;
end;

function TdxPDFFileSpecification.GetAttachment: TdxPDFFileAttachment;
begin
  if FAttachment = nil then
    FAttachment := TdxPDFFileAttachment.Create(Self);
  Result := FAttachment;
end;

procedure TdxPDFFileSpecification.SetAttachment(const AValue
  : TdxPDFFileAttachment);
begin
  FAttachment := AValue;
end;

procedure TdxPDFFileSpecification.ReadAssociatedFileRelationship
  (ADictionary: TdxPDFReaderDictionary);
var
  I: TdxPDFAssociatedFileRelationship;
  S: string;
begin
  FRelationship := frSource;
  S := ADictionary.GetString(TdxPDFKeywords.AssociatedFileRelationship);
  for I := Low(TdxPDFAssociatedFileRelationship)
    to High(TdxPDFAssociatedFileRelationship) do
    if AssociatedFileRelationshipNameMap[I] = S then
    begin
      FRelationship := I;
      Break;
    end;
end;

procedure TdxPDFFileSpecification.ReadFileIndex(ADictionary
  : TdxPDFReaderDictionary);
begin
  if ADictionary <> nil then
    FIndex := ADictionary.GetInteger(TdxPDFKeywords.FileIndex, 0);
end;

procedure TdxPDFFileSpecification.ReadFileName(ADictionary
  : TdxPDFReaderDictionary);
begin
  if not((ADictionary <> nil) and (ADictionary.TryGetTextString('UF', FFileName)
    or ADictionary.TryGetTextString('F', FFileName) or
    ADictionary.TryGetTextString('DOS', FFileName) or
    ADictionary.TryGetTextString('Mac', FFileName) or
    ADictionary.TryGetTextString('Unix', FFileName))) then
    FFileName := '';
end;

procedure TdxPDFFileSpecification.ReadFileSpecificationData
  (ADictionary: TdxPDFReaderDictionary);
var
  AEmbeddedStreamDictionary: TdxPDFReaderDictionary;
begin
  if ADictionary <> nil then
  begin
    if ADictionary.TryGetStreamDictionary('F', AEmbeddedStreamDictionary) or
      ADictionary.TryGetStreamDictionary('DOS', AEmbeddedStreamDictionary) or
      ADictionary.TryGetStreamDictionary('Unix', AEmbeddedStreamDictionary) then
      FFileSpecificationData.Read(AEmbeddedStreamDictionary);
  end;
end;

procedure TdxPDFFileSpecification.WriteFileIndex(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
var
  AData: TdxPDFWriterDictionary;
begin
  if FIndex > 0 then
  begin
    AData := AHelper.CreateDictionary;
    AData.Add(TdxPDFKeywords.FileIndex, FIndex);
    ADictionary.AddReference(TdxPDFKeywords.CollectionItem, AData);
  end;
end;

function TdxPDFFileSpecification.WriteFileSpecificationData
  (AHelper: TdxPDFWriterHelper): TdxPDFBase;
var
  ADictionary: TdxPDFWriterDictionary;
begin
  ADictionary := AHelper.CreateDictionary;
  ADictionary.AddReference(TdxPDFKeywords.FileData, FFileSpecificationData);
  Result := ADictionary;
end;

{ TdxPDFCustomAction }

class function TdxPDFCustomAction.Parse(ADictionary: TdxPDFReaderDictionary)
  : TdxPDFCustomAction;
var
  AActionType, AType: string;
  AClass: TdxPDFObjectClass;
begin
  Result := nil;
  if ADictionary <> nil then
  begin
    AActionType := ADictionary.GetString(TdxPDFKeywords.ActionType);
    AType := ADictionary.GetString(TdxPDFKeywords.TypeKey);
    if (AType <> '') and (AType <> TdxPDFKeywords.Action) and (AType <> 'A')
    then
      TdxPDFUtils.RaiseException;
    if dxPDFTryGetDocumentObjectClass(AActionType, AClass) then
    begin
      Result := AClass.Create(nil) as TdxPDFCustomAction;
      Result.Read(ADictionary);
    end;
  end;
end;

function TdxPDFCustomAction.GetNext: TdxPDFActionList;
begin
  EnsureNextActions;
  Result := FNext;
end;

procedure TdxPDFCustomAction.SetNextValue(const AValue: TdxPDFBase);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FNextValue));
end;

procedure TdxPDFCustomAction.EnsureNextActions;
var
  AActionArray: TdxPDFArray;
  I: Integer;
begin
  if (FNext = nil) and (FNextValue <> nil) then
  begin
    FNext := TdxPDFActionList.Create;
    case FNextValue.ObjectType of
      otIndirectReference:
        AActionArray := Repository.GetArray(FNextValue.Number);
      otArray:
        AActionArray := TdxPDFArray(FNextValue);
    else
      AActionArray := nil;
    end;

    if AActionArray <> nil then
    begin
      for I := 0 to AActionArray.Count - 1 do
        FNext.Add(FCatalog.Repository.GetAction(AActionArray[I]));
    end
    else
      FNext.Add(FCatalog.Repository.GetAction(FNextValue));
  end;
end;

procedure TdxPDFCustomAction.DestroySubClasses;
begin
  NextValue := nil;
  FreeAndNil(FNext);
  inherited DestroySubClasses;
end;

procedure TdxPDFCustomAction.Execute(const AController
  : IdxPDFInteractivityController);
begin
  // do nothing
end;

procedure TdxPDFCustomAction.ReadProperties(ADictionary
  : TdxPDFReaderDictionary);
begin
  inherited;
  FCatalog := Repository.Catalog;
  Number := ADictionary.Number;
  NextValue := ADictionary.GetObject(TdxPDFKeywords.ActionNext);
end;

procedure TdxPDFCustomAction.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  ADictionary.AddName(TdxPDFKeywords.TypeKey, TdxPDFKeywords.Action);
  ADictionary.AddName(TdxPDFKeywords.ActionType, GetTypeName);
  WriteNextActions(AHelper, ADictionary);
end;

procedure TdxPDFCustomAction.WriteNextActions(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
var
  AArray: TdxPDFWriterArray;
  I: Integer;
begin
  if Next <> nil then
  begin
    AArray := AHelper.CreateArray;
    for I := 0 to Next.Count - 1 do
      AArray.AddReference(Next[I]);
    ADictionary.AddReference(TdxPDFKeywords.ActionNext, AArray);
  end;
end;

{ TdxPDFCustomTree }

constructor TdxPDFCustomTree.Create;
begin
  inherited Create;
  FDictionary := TdxPDFReferencedObjectDictionary.Create;
end;

destructor TdxPDFCustomTree.Destroy;
begin
  FreeAndNil(FDictionary);
  inherited Destroy;
end;

function TdxPDFCustomTree.GetNodeName: string;
begin
  Result := TdxPDFKeywords.Names;
end;

function TdxPDFCustomTree.InternalGetValue(const AKey: string): TdxPDFObject;
var
  AObject: TdxPDFReferencedObject;
begin
  if FDictionary.TryGetValue(AKey, AObject) then
  begin
    if AObject is TdxPDFDeferredObject then
      Result := TdxPDFDeferredObject(AObject).ResolvedObject as TdxPDFObject
    else
      Result := AObject as TdxPDFObject;
  end
  else
    Result := nil;
end;

procedure TdxPDFCustomTree.Read(ADictionary: TdxPDFReaderDictionary);
var
  ANodeObject: TdxPDFBase;
begin
  if ADictionary <> nil then
  begin
    ANodeObject := ADictionary.GetObject(GetNodeName);
    if ANodeObject = nil then
      ReadNode(ADictionary.Repository, ADictionary)
    else
      case ANodeObject.ObjectType of
        otArray:
          ReadBranch(ADictionary.Repository, TdxPDFArray(ANodeObject));
        otDictionary:
          ReadNode(ADictionary.Repository, ANodeObject);
      end;
  end;
end;

function TdxPDFCustomTree.Write(AHelper: TdxPDFWriterHelper): TdxPDFBase;
begin
  if Count > 0 then
    Result := WriteBranch(AHelper)
  else
    Result := nil;
end;

function TdxPDFCustomTree.CreateKey(AValue: TdxPDFBase): string;
begin
  if AValue is TdxPDFString then
    Result := TdxPDFString(AValue).Value
  else
    Result := '';
end;

function TdxPDFCustomTree.GetCount: Integer;
begin
  Result := FDictionary.Count;
end;

function TdxPDFCustomTree.GetItems: TdxPDFStringReferencedObjectDictionary;
begin
  Result := FDictionary.Items;
end;

function TdxPDFCustomTree.CreateDeferredObject(ARepository
  : TdxPDFDocumentRepository; AValue: TdxPDFBase): TdxPDFDeferredObject;
var
  AInfo: TdxPDFDeferredObjectInfo;
begin
  AInfo.Name := '';
  AInfo.Key := GetDeferredObjectKey;
  AInfo.Number := AValue.Number;
  AInfo.SourceObject := AValue;
  Result := TdxPDFDeferredObject.Create(nil, AInfo);
  Result.Repository := ARepository;
end;

procedure TdxPDFCustomTree.ReadBranch(ARepository: TdxPDFDocumentRepository;
AReferences: TdxPDFArray);
var
  I, AIndex: Integer;
  AValue: TdxPDFBase;
  AKey: string;
begin
  AIndex := 0;
  for I := 0 to AReferences.Count div 2 - 1 do
  begin
    AValue := ARepository.ResolveReference(AReferences[AIndex]);
    AKey := CreateKey(AValue);
    FDictionary.AddOrSetValue(AKey, CreateDeferredObject(ARepository,
      AReferences[AIndex + 1]));
    Inc(AIndex, 2);
  end;
end;

procedure TdxPDFCustomTree.ReadKids(ARepository: TdxPDFDocumentRepository;
AReferences: TdxPDFArray);
var
  I: Integer;
  ADictionary: TdxPDFDictionary;
  AValue: TdxPDFBase;
begin
  for I := 0 to AReferences.Count - 1 do
  begin
    AValue := AReferences[I];
    case AValue.ObjectType of
      otIndirectReference:
        ADictionary := ARepository.GetDictionary(AValue.Number);
      otDictionary:
        ADictionary := AValue as TdxPDFDictionary;
    else
      ADictionary := nil;
      TdxPDFUtils.Abort;
    end;
    ReadNode(ARepository, ADictionary);
  end;
end;

procedure TdxPDFCustomTree.ReadNode(ARepository: TdxPDFDocumentRepository;
APageObject: TdxPDFBase);
var
  ADictionary: TdxPDFReaderDictionary;
  AKids: TdxPDFArray;
begin
  if APageObject <> nil then
    case APageObject.ObjectType of
      otArray:
        ReadBranch(ARepository, TdxPDFArray(APageObject));
      otDictionary:
        begin
          ADictionary := APageObject as TdxPDFReaderDictionary;
          if ADictionary.Count > 0 then
          begin
            AKids := ADictionary.GetArray(TdxPDFKeywords.Kids);
            if AKids <> nil then
              ReadKids(ARepository, AKids)
            else
              Read(ADictionary);
          end;
        end;
    end;
end;

function TdxPDFCustomTree.WriteBranch(AHelper: TdxPDFWriterHelper): TdxPDFBase;
var
  ADictionary: TdxPDFWriterDictionary;
  AKey: string;
  ANames: TdxPDFWriterArray;
begin
  ANames := AHelper.CreateArray;
  for AKey in Items.Keys do
  begin
    ANames.Add(AKey);
    ANames.AddReference(InternalGetValue(AKey));
  end;
  ADictionary := AHelper.CreateDictionary;
  ADictionary.Add(GetNodeName, ANames);
  Result := ADictionary;
end;

{ TdxPDFDestinationTree }

function TdxPDFDestinationTree.GetValue(const AKey: string)
  : TdxPDFCustomDestination;
begin
  Result := InternalGetValue(AKey) as TdxPDFCustomDestination;
end;

function TdxPDFDestinationTree.GetDeferredObjectKey: string;
begin
  Result := TdxPDFCustomDestination.GetTypeName;
end;

{ TdxPDFEmbeddedFileSpecificationTree }

function TdxPDFEmbeddedFileSpecificationTree.GetValue(const AKey: string)
  : TdxPDFFileSpecification;
begin
  Result := InternalGetValue(AKey) as TdxPDFFileSpecification;
end;

function TdxPDFEmbeddedFileSpecificationTree.GetDeferredObjectKey: string;
begin
  Result := TdxPDFFileSpecification.GetTypeName;
end;

{ TdxPDFAnnotationAppearance }

class function TdxPDFAnnotationAppearance.Parse(ADictionary
  : TdxPDFReaderDictionary; const AKey: string): TdxPDFAnnotationAppearance;
var
  AAppearanceSubDictionary: TdxPDFReaderDictionary;
  AValue: TdxPDFBase;
  APair: TPair<string, TdxPDFReferencedObject>;
  ADefaultForm: TdxPDFForm;
  AForms: TDictionary<string, TdxPDFForm>;
  AName: string;
  AForm: TdxPDFForm;
begin
  Result := nil;
  AValue := ADictionary.GetObject(AKey);
  if AValue <> nil then
  begin
    AForms := nil;
    ADefaultForm := nil;
    case AValue.ObjectType of
      otStream:
        begin
          ADefaultForm := ADictionary.GetForm(AValue.Number);
          if ADefaultForm = nil then
            Exit(nil);
        end;
      otDictionary:
        begin
          ADefaultForm := nil;
          AForms := TObjectDictionary<string, TdxPDFForm>.Create
            ([doOwnsValues]);
          AAppearanceSubDictionary := TdxPDFReaderDictionary(AValue);
          for APair in AAppearanceSubDictionary.Items do
          begin
            AName := APair.Key;
            if APair.Value = nil then
              AForms.Add(AName, nil)
            else
            begin
              AForm := ADictionary.GetForm((APair.Value as TdxPDFBase).Number);
              AForms.Add(AName, AForm);
              if (AName = 'On') or (ADefaultForm = nil) and (AName <> 'Off')
              then
                ADefaultForm := AForm;
            end;
          end;
        end;
    end;
    Result := TdxPDFAnnotationAppearance.Create(ADefaultForm, AForms);
  end;
end;

constructor TdxPDFAnnotationAppearance.Create(ACatalog: TdxPDFCatalog;
const ABBox: TdxPDFRectangle);
begin
  inherited Create(nil);
  FDefaultForm := TdxPDFForm.Create(ACatalog, ABBox);
end;

constructor TdxPDFAnnotationAppearance.Create(ADefaultForm: TdxPDFForm;
AForms: TDictionary<string, TdxPDFForm>);
begin
  inherited Create(nil);
  FForms := AForms;
  DefaultForm := ADefaultForm;
end;

procedure TdxPDFAnnotationAppearance.DestroySubClasses;
var
  APair: TPair<string, TdxPDFForm>;
begin
  if FForms <> nil then
    for APair in FForms do
      if APair.Value = FDefaultForm then
      begin
        FForms.ExtractPair(APair.Key);
        DefaultForm := nil;
      end;
  DefaultForm := nil;
  FreeAndNil(FForms);
  inherited DestroySubClasses;
end;

function TdxPDFAnnotationAppearance.Write(AHelper: TdxPDFWriterHelper)
  : TdxPDFBase;
var
  ADictionary: TdxPDFWriterDictionary;
  APair: TPair<string, TdxPDFForm>;
begin
  ADictionary := AHelper.CreateDictionary;
  if FForms = nil then
    FDefaultForm.Write(AHelper, ADictionary)
  else
    for APair in FForms do
      ADictionary.AddReference(APair.Key, APair.Value);
  Result := ADictionary;
end;

procedure TdxPDFAnnotationAppearance.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  RaiseWriteNotImplementedException;
end;

procedure TdxPDFAnnotationAppearance.SetForm(const AName: string;
AForm: TdxPDFForm);
begin
  if AName = '' then
    DefaultForm := AForm
  else
  begin
    if FForms = nil then
      FForms := TDictionary<string, TdxPDFForm>.Create;
    FForms.Add(AName, AForm);
  end;
end;

procedure TdxPDFAnnotationAppearance.SetDefaultForm(const AValue: TdxPDFForm);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FDefaultForm));
end;

{ TdxPDFAnnotationAppearances }

function TdxPDFAnnotationAppearances.Write(AHelper: TdxPDFWriterHelper)
  : TdxPDFBase;
var
  ADictionary: TdxPDFWriterDictionary;
begin
  ADictionary := AHelper.CreateDictionary;
  ADictionary.AddReference(TdxPDFKeywords.AnnotationAppearanceNormal, FNormal);
  ADictionary.AddReference(TdxPDFKeywords.AnnotationAppearanceRollover,
    FRollover);
  ADictionary.AddReference(TdxPDFKeywords.AnnotationAppearanceDown, FDown);
  Result := ADictionary;
end;

procedure TdxPDFAnnotationAppearances.CreateSubClasses;
begin
  inherited CreateSubClasses;
  Form := nil;
end;

procedure TdxPDFAnnotationAppearances.DestroySubClasses;
begin
  Form := nil;
  FreeAndNil(FRollover);
  FreeAndNil(FNormal);
  FreeAndNil(FDown);
  inherited DestroySubClasses;
end;

procedure TdxPDFAnnotationAppearances.Read(ADictionary: TdxPDFReaderDictionary;
const AParentBox: TdxPDFRectangle);
begin
  inherited Read(ADictionary);
  FNormal := TdxPDFAnnotationAppearance.Parse(ADictionary,
    TdxPDFKeywords.AnnotationAppearanceNormal);
  if (FNormal = nil) and not AParentBox.IsNull then
    FNormal := TdxPDFAnnotationAppearance.Create(ADictionary.Repository.Catalog,
      AParentBox);
  FRollover := TdxPDFAnnotationAppearance.Parse(ADictionary,
    TdxPDFKeywords.AnnotationAppearanceRollover);
  FDown := TdxPDFAnnotationAppearance.Parse(ADictionary,
    TdxPDFKeywords.AnnotationAppearanceDown);
end;

procedure TdxPDFAnnotationAppearances.Read(AForm: TdxPDFForm);
begin
  Form := AForm;
end;

procedure TdxPDFAnnotationAppearances.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  RaiseWriteNotImplementedException;
end;

procedure TdxPDFAnnotationAppearances.SetAnnotationForm(const AName: string;
AForm: TdxPDFForm);
begin
  if FNormal = nil then
    FNormal := TdxPDFAnnotationAppearance.Create(nil);
  FNormal.SetForm(AName, AForm);
end;

procedure TdxPDFAnnotationAppearances.SetForm(const AValue: TdxPDFForm);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FForm));
end;

{ TdxPDFAnnotationBorderStyle }

class function TdxPDFAnnotationBorderStyle.Parse(ADictionary
  : TdxPDFReaderDictionary): TdxPDFAnnotationBorderStyle;
var
  ABorderStyleDictionary: TdxPDFReaderDictionary;
begin
  Result := nil;
  if (ADictionary <> nil) and ADictionary.TryGetDictionary
    (TdxPDFKeywords.AnnotationBorderStyle, ABorderStyleDictionary) then
  begin
    Result := TdxPDFAnnotationBorderStyle.Create(nil);
    Result.Read(ABorderStyleDictionary);
  end;
end;

class function TdxPDFAnnotationBorderStyle.ParseLineStyle(AArray: TdxPDFArray)
  : TdxPDFLineStyle;
begin
  Result := TdxPDFLineStyle.Parse(AArray);
end;

function TdxPDFAnnotationBorderStyle.Write(AHelper: TdxPDFWriterHelper)
  : TdxPDFBase;
var
  ADictionary: TdxPDFWriterDictionary;
begin
  ADictionary := AHelper.CreateDictionary;
  ADictionary.Add(TdxPDFKeywords.BorderStyleWidth, FWidth);
  ADictionary.Add(TdxPDFKeywords.BorderStyleName, FStyleName);
  if LineStyle <> nil then
    ADictionary.Add(TdxPDFKeywords.LineStyle, LineStyle.Write);
  Result := ADictionary;
end;

procedure TdxPDFAnnotationBorderStyle.CreateSubClasses;
begin
  inherited CreateSubClasses;
  LineStyle := nil;
end;

procedure TdxPDFAnnotationBorderStyle.DestroySubClasses;
begin
  LineStyle := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFAnnotationBorderStyle.Read(ADictionary: TdxPDFReaderDictionary);
var
  AType: string;
begin
  inherited Read(ADictionary);
  if ADictionary <> nil then
  begin
    AType := ADictionary.GetString(TdxPDFKeywords.TypeKey);
    FWidth := ADictionary.GetDouble(TdxPDFKeywords.BorderStyleWidth, 1);
    if (AType = TdxPDFKeywords.Border) or (AType = TdxPDFKeywords.BorderStyle)
    then
    begin
      FStyleName := ADictionary.GetString(TdxPDFKeywords.BorderStyleName,
        TdxPDFKeywords.BorderStyleName);
      FLineStyle := ParseLineStyle
        (ADictionary.GetArray(TdxPDFKeywords.LineStyle));
    end;
  end;
end;

procedure TdxPDFAnnotationBorderStyle.SetLineStyle(const AValue
  : TdxPDFLineStyle);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FLineStyle));
end;

{ TdxPDFAnnotationBorder }

function TdxPDFAnnotationBorder.Write(AHelper: TdxPDFWriterHelper): TdxPDFBase;
var
  AArray: TdxPDFWriterArray;
begin
  AArray := AHelper.CreateArray;
  AArray.Add(FHorizontalCornerRadius);
  AArray.Add(FVerticalCornerRadius);
  AArray.Add(LineWidth);
  if (LineStyle <> nil) and LineStyle.IsDashed then
    AArray.Add(TdxPDFLineStyleAccess(LineStyle).WritePattern);
  Result := AArray;
end;

procedure TdxPDFAnnotationBorder.CreateSubClasses;
begin
  inherited CreateSubClasses;
  LineStyle := TdxPDFLineStyle.CreateSolid;
end;

procedure TdxPDFAnnotationBorder.DestroySubClasses;
begin
  LineStyle := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFAnnotationBorder.Initialize;
begin
  inherited Initialize;
  FLineWidth := DefaultLineWidth;
  FHorizontalCornerRadius := DefaultHorizontalCornerRadius;
  FVerticalCornerRadius := DefaultVerticalCornerRadius;
end;

procedure TdxPDFAnnotationBorder.Read(ADictionary: TdxPDFReaderDictionary);
var
  AArray, ALineStyleArray: TdxPDFArray;
  AValue: TdxPDFBase;
begin
  inherited Read(ADictionary);
  if ADictionary.TryGetArray(TdxPDFKeywords.Border, AArray) and
    (AArray.Count >= 3) then
  begin
    FHorizontalCornerRadius := TdxPDFUtils.ConvertToDouble(AArray[0]);
    FVerticalCornerRadius := TdxPDFUtils.ConvertToDouble(AArray[1]);
    FLineWidth := TdxPDFUtils.ConvertToDouble(AArray[2]);
    if (FHorizontalCornerRadius < 0) or (FVerticalCornerRadius < 0) or
      (FLineWidth < 0) then
      TdxPDFUtils.Abort;
    if AArray.Count = 4 then
    begin
      AValue := AArray[3] as TdxPDFBase;
      ALineStyleArray := nil;
      try
        if AValue.ObjectType <> otArray then
        begin
          ALineStyleArray := TdxPDFArray.Create;
          if AValue.ObjectType in [otInteger, otDouble] then
            ALineStyleArray.Add(AValue);
        end
        else
        begin
          ALineStyleArray := TdxPDFArray(AValue);
          ALineStyleArray.reference;
        end;
        LineStyle := TdxPDFAnnotationBorderStyle.ParseLineStyle(ALineStyleArray)
      finally
        dxPDFFreeObject(ALineStyleArray);
      end;
    end
    else
      LineStyle := TdxPDFLineStyle.CreateSolid;
  end;
end;

function TdxPDFAnnotationBorder.GetIsDefault: Boolean;
begin
  Result := not LineStyle.IsDashed and SameValue(HorizontalCornerRadius,
    DefaultHorizontalCornerRadius) and SameValue(LineWidth, DefaultLineWidth)
    and SameValue(VerticalCornerRadius, DefaultVerticalCornerRadius);
end;

procedure TdxPDFAnnotationBorder.SetLineStyle(const AValue: TdxPDFLineStyle);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FLineStyle));
end;

{ TdxPDFCustomAnnotation }

class function TdxPDFCustomAnnotation.GetTypeName: string;
begin
  Result := TdxPDFKeywords.Annotation;
end;

class function TdxPDFCustomAnnotation.Parse(ADictionary: TdxPDFReaderDictionary)
  : TdxPDFCustomAnnotation;
var
  AClass: TdxPDFObjectClass;
  AType: string;
begin
  Result := nil;
  if ADictionary <> nil then
  begin
    AType := ADictionary.GetString(TdxPDFKeywords.Subtype);
    if AType = '' then
      AType := ADictionary.GetString(TdxPDFKeywords.TypeKey);
    if dxPDFTryGetDocumentObjectClass(AType, AClass) then
      Result := AClass.Create as TdxPDFCustomAnnotation;
  end;
end;

constructor TdxPDFCustomAnnotation.Create(APage: TdxPDFPage;
const ARect: TdxPDFRectangle; AFlags: TdxPDFAnnotationFlags);
begin
  inherited Create;
  FPage := APage;
  FRect := ARect;
  FFlags := AFlags;
end;

procedure TdxPDFCustomAnnotation.EnsureAppearance(AState: TdxPDFDocumentState);
begin
  GetAppearanceForm(AState);
end;

function TdxPDFCustomAnnotation.GetRect: TdxPDFRectangle;
begin
  Ensure;
  Result := FRect;
end;

function TdxPDFCustomAnnotation.GetContents: string;
begin
  Ensure;
  Result := FContents;
end;

function TdxPDFCustomAnnotation.GetName: string;
begin
  Ensure;
  Result := FName;
end;

procedure TdxPDFCustomAnnotation.SetAppearance(const AValue
  : TdxPDFAnnotationAppearances);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FAppearance));
end;

function TdxPDFCustomAnnotation.GetColor: TdxPDFColor;
begin
  Ensure;
  Result := FColor;
end;

procedure TdxPDFCustomAnnotation.SetColor(const AValue: TdxPDFColor);
begin
  Ensure;
  FColor := AValue;
end;

procedure TdxPDFCustomAnnotation.SetContents(const AValue: string);
begin
  Ensure;
  FContents := AValue;
end;

procedure TdxPDFCustomAnnotation.SetDictionary(const AValue
  : TdxPDFReaderDictionary);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FDictionary));
end;

procedure TdxPDFCustomAnnotation.SetName(const AValue: string);
begin
  Ensure;
  FName := AValue;
end;

function TdxPDFCustomAnnotation.EnsureAppearance(AState: TdxPDFDocumentState;
AForm: TdxPDFForm): TdxPDFForm;
var
  ABuilder: TObject;
  ACurrentForm: TdxPDFForm;
  AIntf: IdxPDFAnnotationAppearanceBuilder;
  ANeedCreateAppearance: Boolean;
begin
  try
    ANeedCreateAppearance := NeedCreateAppearance(AForm);
  except
    ANeedCreateAppearance := True;
  end;
  ACurrentForm := AForm;
  if ANeedCreateAppearance then
  begin
    ABuilder := CreateAppearanceBuilder(AState);
    if ABuilder <> nil then
    begin
      if Supports(ABuilder, IdxPDFAnnotationAppearanceBuilder, AIntf) then
      begin
        if ACurrentForm = nil then
          ACurrentForm := CreateAppearanceForm('');
        if ACurrentForm <> nil then
          AIntf.RebuildAppearance(ACurrentForm);
        AIntf := nil;
      end
      else
        ABuilder.Free;
    end;
  end;
  Result := ACurrentForm;
end;

function TdxPDFCustomAnnotation.GetActualAppearanceForm: TdxPDFForm;
var
  AForms: TDictionary<string, TdxPDFForm>;
begin
  Ensure;
  if FAppearance <> nil then
  begin
    Result := nil;
    if FAppearanceName <> '' then
    begin
      AForms := FAppearance.Normal.Forms;
      if (AForms <> nil) and not AForms.TryGetValue(FAppearanceName, Result)
      then
        Result := nil;
    end;
    if (Result = nil) and UseDefaultForm then
      Result := FAppearance.Normal.DefaultForm;
  end
  else
    Result := nil;
end;

function TdxPDFCustomAnnotation.GetAppearanceName: string;
begin
  Ensure;
  Result := FAppearanceName;
end;

function TdxPDFCustomAnnotation.GetBorder: TdxPDFAnnotationBorder;
begin
  Ensure;
  Result := FBorder;
end;

procedure TdxPDFCustomAnnotation.SetAppearanceName(const AValue: string);
begin
  Ensure;
  FAppearanceName := AValue;
end;

procedure TdxPDFCustomAnnotation.SetBorder(const AValue
  : TdxPDFAnnotationBorder);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FBorder));
end;

procedure TdxPDFCustomAnnotation.DestroySubClasses;
begin
  FResolved := True;
  Border := nil;
  Color := TdxPDFColor.Null;
  Appearance := nil;
  Dictionary := nil;
  inherited DestroySubClasses;
end;

procedure TdxPDFCustomAnnotation.Initialize;
begin
  inherited Initialize;
  Appearance := nil;
end;

procedure TdxPDFCustomAnnotation.Read(ADictionary: TdxPDFReaderDictionary;
APage: TdxPDFPage);
begin
  inherited Read(ADictionary);
  FPage := APage;
  Dictionary := ADictionary;
  Number := ADictionary.Number;
end;

procedure TdxPDFCustomAnnotation.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  Ensure;
  ADictionary.AddName(TdxPDFKeywords.TypeKey, TdxPDFKeywords.Annotation);
  ADictionary.AddName(TdxPDFKeywords.Subtype, GetTypeName);
  ADictionary.AddReference(TdxPDFKeywords.ShortPage, FPage);
  ADictionary.Add(TdxPDFKeywords.Rect, FRect, False);
  ADictionary.Add(TdxPDFKeywords.Contents, FContents);
  ADictionary.Add(TdxPDFKeywords.AnnotationName, Name);
  ADictionary.AddDate(TdxPDFKeywords.Modified, FModified);
  if FFlags <> afNone then
    ADictionary.Add(TdxPDFKeywords.AnnotationFlags, Ord(FFlags));
  ADictionary.AddName(TdxPDFKeywords.AnnotationAppearanceName,
    FAppearanceName, False);
  ADictionary.AddInline(TdxPDFKeywords.AnnotationAppearance, FAppearance);
  if (FBorder <> nil) and not FBorder.IsDefault then
    ADictionary.AddInline(TdxPDFKeywords.Border, FBorder);
  ADictionary.Add(TdxPDFKeywords.AnnotationColor, FColor);
  ADictionary.Add(TdxPDFKeywords.StructParent, FStructParent, 0);
end;

function TdxPDFCustomAnnotation.GetUseDefaultForm: Boolean;
begin
  Result := True;
end;

function TdxPDFCustomAnnotation.GetVisible: Boolean;
begin
  Result := not HasFlag(TdxPDFAnnotationFlags.afHidden) and (Rect.Width <> 0)
    and (Rect.Height <> 0);
end;

function TdxPDFCustomAnnotation.NeedCreateAppearance(AForm: TdxPDFForm)
  : Boolean;
begin
  Result := (AForm = nil) or (AForm.Commands.Count = 0);
end;

procedure TdxPDFCustomAnnotation.Ensure;
begin
  if not FResolved and (FDictionary <> nil) then
  begin
    Resolve(FDictionary);
    FResolved := True;
  end;
end;

function TdxPDFCustomAnnotation.CreateAppearanceForm(const AName: string)
  : TdxPDFForm;
begin
  Ensure;
  Result := TdxPDFForm.Create(Repository.Catalog, GetAppearanceFormBoundingBox);
  if FAppearance = nil then
    FAppearance := TdxPDFAnnotationAppearances.Create(nil);
  FAppearance.SetAnnotationForm(AName, Result);
end;

function TdxPDFCustomAnnotation.CreateAppearanceBuilder
  (AState: TdxPDFDocumentState): TObject;
begin
  Result := nil;
end;

function TdxPDFCustomAnnotation.GetAppearanceFormBoundingBox: TdxPDFRectangle;
begin
  Result := TdxPDFRectangle.Create(0, 0, Rect.Width, Rect.Height);
end;

procedure TdxPDFCustomAnnotation.Resolve(ADictionary: TdxPDFReaderDictionary);
var
  AObject: TdxPDFBase;
begin
  if ADictionary.Contains(TdxPDFKeywords.Rect) then
    FRect := ADictionary.GetRectangleEx(TdxPDFKeywords.Rect)
  else
    TdxPDFUtils.Abort;

  FContents := ADictionary.GetTextString(TdxPDFKeywords.Contents);
  FName := ADictionary.GetTextString(TdxPDFKeywords.AnnotationName);
  FModified := ADictionary.GetDate(TdxPDFKeywords.Modified);
  FFlags := TdxPDFAnnotationFlags
    (ADictionary.GetInteger(TdxPDFKeywords.AnnotationFlags, 0));
  FAppearanceName := ADictionary.GetString
    (TdxPDFKeywords.AnnotationAppearanceName);
  FStructParent := ADictionary.GetInteger(TdxPDFKeywords.StructParent);
  FColor := ADictionary.GetColor(TdxPDFKeywords.AnnotationColor);
  if ADictionary.TryGetObject(TdxPDFKeywords.AnnotationAppearance, AObject) then
    Appearance := ADictionary.GetAnnotationAppearance(AObject, FRect);
  FBorder := TdxPDFAnnotationBorder.Create(nil);
  FBorder.Read(ADictionary);
  FBorder.reference;
end;

function TdxPDFCustomAnnotation.GetAppearanceForm(AState: TdxPDFDocumentState)
  : TdxPDFForm;
begin
  Result := EnsureAppearance(AState, GetActualAppearanceForm);
end;

function TdxPDFCustomAnnotation.HasFlag(AFlags: TdxPDFAnnotationFlags): Boolean;
begin
  Result := (Integer(FFlags) and Integer(AFlags)) <> 0;
end;

{ TdxPDFFileAttachment }

constructor TdxPDFFileAttachment.Create(AFileSpecification
  : TdxPDFFileSpecification);
begin
  inherited Create;
  FFileSpecification := AFileSpecification;
  FFileSpecification.Attachment := Self;
end;

function TdxPDFFileAttachment.GetCreationDate: TDateTime;
begin
  Result := FFileSpecification.CreationDate;
end;

function TdxPDFFileAttachment.GetModificationDate: TDateTime;
begin
  Result := FFileSpecification.ModificationDate;
end;

function TdxPDFFileAttachment.GetMimeType: string;
begin
  Result := FFileSpecification.MimeType;
end;

function TdxPDFFileAttachment.GetData: TBytes;
begin
  Result := FFileSpecification.FileData;
end;

function TdxPDFFileAttachment.GetSize: Integer;
begin
  Result := FFileSpecification.Size;
end;

function TdxPDFFileAttachment.GetFileName: string;
begin
  Result := FFileSpecification.FileName;
end;

function TdxPDFFileAttachment.GetRelationship: TdxPDFAssociatedFileRelationship;
begin
  Result := FFileSpecification.Relationship;
end;

function TdxPDFFileAttachment.GetDescription: string;
begin
  Result := FFileSpecification.Description;
end;

function TdxPDFFileAttachment.GetModificationDateAsString: string;
begin
  if FFileSpecification.HasModificationDate then
    Result := DateTimeToStr(ModificationDate)
  else
    Result := '';
end;

function TdxPDFFileAttachment.GetSizeAsString: string;
begin
  if Size > 0 then
    Result := TdxPDFUtils.FormatFileSize(Size)
  else
    Result := '';
end;

{ TdxPDFFileAttachmentList }

procedure TdxPDFFileAttachmentList.Populate(ACatalog: TdxPDFCatalog);
var
  AComparer: IComparer<TdxPDFFileAttachment>;
begin
  if ACatalog <> nil then
  begin
    ACatalog.PopulateAttachmentList(Self);
    AComparer := TdxPDFFileAttachmentComparer.Create;
    Sort(AComparer);
  end;
end;

{ TdxPDFDocumentNames }

procedure TdxPDFDocumentNames.CreateSubClasses;
begin
  inherited CreateSubClasses;
  FPageDestinations := TdxPDFDestinationTree.Create;
  FEmbeddedFileSpecifications := TdxPDFEmbeddedFileSpecificationTree.Create;
end;

procedure TdxPDFDocumentNames.DestroySubClasses;
begin
  FreeAndNil(FEmbeddedFileSpecifications);
  FreeAndNil(FPageDestinations);
  inherited DestroySubClasses;
end;

procedure TdxPDFDocumentNames.ReadProperties(ADictionary
  : TdxPDFReaderDictionary);
begin
  inherited;
  FPageDestinations.
    Read(ADictionary.GetDictionary(TdxPDFKeywords.Destinations));
  FEmbeddedFileSpecifications.
    Read(ADictionary.GetDictionary(TdxPDFKeywords.EmbeddedFiles));
end;

procedure TdxPDFDocumentNames.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
begin
  inherited;
  ADictionary.AddInline(TdxPDFKeywords.Destinations, FPageDestinations);
  ADictionary.AddInline(TdxPDFKeywords.EmbeddedFiles,
    FEmbeddedFileSpecifications);
end;

function TdxPDFDocumentNames.GetEmbeddedFileSpecification(const AName: string)
  : TdxPDFFileSpecification;
begin
  Result := FEmbeddedFileSpecifications.GetValue(AName);
end;

function TdxPDFDocumentNames.GetPageDestination(const AName: string)
  : TdxPDFCustomDestination;
begin
  Result := FPageDestinations.GetValue(AName);
end;

procedure TdxPDFDocumentNames.PopulateAttachmentList
  (AList: TdxPDFFileAttachmentList);
var
  APair: TPair<string, TdxPDFReferencedObject>;
  ASpecification: TdxPDFFileSpecification;
begin
  for APair in FEmbeddedFileSpecifications.Items do
  begin
    ASpecification := FEmbeddedFileSpecifications.GetValue(APair.Key);
    if ASpecification <> nil then
      AList.Add(ASpecification.Attachment);
  end;
end;

{ TdxPDFDocumentInformation }

class function TdxPDFDocumentInformation.GetTypeName: string;
begin
  Result := TdxPDFKeywords.DocumentInfo;
end;

procedure TdxPDFDocumentInformation.Initialize;
begin
  inherited Initialize;
  FVersion := v1_7;
end;

procedure TdxPDFDocumentInformation.ReadProperties
  (ADictionary: TdxPDFReaderDictionary);
begin
  inherited ReadProperties(ADictionary);
  FAuthor := ADictionary.GetTextString(TdxPDFKeywords.Author);
  FCreationDate := ADictionary.GetDate(TdxPDFKeywords.CreationDate);
  FApplication := ADictionary.GetTextString(TdxPDFKeywords.Creator);
  FKeywords := ADictionary.GetTextString(TdxPDFKeywords.Keywords);
  FModificationDate := ADictionary.GetDate(TdxPDFKeywords.ModDate);
  FProducer := ADictionary.GetTextString(TdxPDFKeywords.Producer);
  FSubject := ADictionary.GetTextString(TdxPDFKeywords.Subject);
  FTitle := ADictionary.GetTextString(TdxPDFKeywords.Title);
end;

procedure TdxPDFDocumentInformation.Write(AHelper: TdxPDFWriterHelper;
ADictionary: TdxPDFWriterDictionary);
var
  ADate: TDateTime;
begin
  ADate := Now;
  FCreationDate := ADate;
  FModificationDate := ADate;

  ADictionary.Add(TdxPDFKeywords.Author, FAuthor);
  ADictionary.Add(TdxPDFKeywords.Creator, FApplication);
  ADictionary.Add(TdxPDFKeywords.Keywords, FKeywords);
  ADictionary.Add(TdxPDFKeywords.Producer, FProducer);
  ADictionary.Add(TdxPDFKeywords.Subject, FSubject);
  ADictionary.Add(TdxPDFKeywords.Title, FTitle);
  ADictionary.AddDate(TdxPDFKeywords.CreationDate, FCreationDate);
  ADictionary.AddDate(TdxPDFKeywords.ModDate, FModificationDate);
end;

{ TdxPDFCustomCommand }

constructor TdxPDFCustomCommand.Create;
begin
  inherited Create;
end;

constructor TdxPDFCustomCommand.Create(AOperands: TdxPDFCommandOperandStack;
AResources: TdxPDFResources);
begin
  Create;
end;

function TdxPDFCustomCommand.EnsureRange(const AValue: Double): Double;
begin
  Result := Max(Min(AValue, MaxInt), MinInt);
end;

function TdxPDFCustomCommand.EnsureRange(const AValue: Integer): Integer;
begin
  Result := Max(Min(AValue, MaxInt), MinInt);
end;

class function TdxPDFCustomCommand.GetObjectType: TdxPDFBaseType;
begin
  Result := otCommand;
end;

procedure TdxPDFCustomCommand.Write(AWriter: TdxPDFWriter;
AResources: TdxPDFResources);
begin
  TdxPDFUtils.RaiseException(ClassName + '.Write is not implemented');
end;

procedure TdxPDFCustomCommand.WriteCommandName(AWriter: TdxPDFWriter);
begin
  AWriter.WriteSpace;
  AWriter.WriteString(GetName);
end;

procedure TdxPDFCustomCommand.WriteOperand(AWriter: TdxPDFWriter;
const AOperand: Double);
begin
  AWriter.WriteSpace;
  AWriter.WriteDouble(EnsureRange(AOperand));
end;

procedure TdxPDFCustomCommand.WriteOperand(AWriter: TdxPDFWriter;
const AOperand: Integer);
begin
  AWriter.WriteSpace;
  AWriter.WriteInteger(EnsureRange(AOperand));
end;

procedure TdxPDFCustomCommand.WriteOperand(AWriter: TdxPDFWriter;
const AOperand: TdxSizeF);
begin
  WriteOperand(AWriter, AOperand.cx);
  WriteOperand(AWriter, AOperand.cy);
end;

procedure TdxPDFCustomCommand.WriteOperand(AWriter: TdxPDFWriter;
const AOperand: TdxPointF);
begin
  WriteOperand(AWriter, AOperand.X);
  WriteOperand(AWriter, AOperand.Y);
end;

procedure TdxPDFCustomCommand.WriteOperand(AWriter: TdxPDFWriter;
const AOperand: string);
begin
  AWriter.WriteSpace;
  AWriter.WriteString(AOperand);
end;

procedure TdxPDFCustomCommand.WriteUnaryCommand(AWriter: TdxPDFWriter;
const AOperand: Single);
begin
  WriteOperand(AWriter, AOperand);
  WriteCommandName(AWriter);
end;

procedure TdxPDFCustomCommand.WriteUnaryCommand(AWriter: TdxPDFWriter;
const AOperand: Integer);
begin
  WriteOperand(AWriter, AOperand);
  WriteCommandName(AWriter);
end;

procedure TdxPDFCustomCommand.WriteUnaryCommand(AWriter: TdxPDFWriter;
const AOperand: string);
begin
  WriteOperand(AWriter, AOperand);
  WriteCommandName(AWriter);
end;

procedure TdxPDFCustomCommand.WriteUnaryCommand(AWriter: TdxPDFWriter;
const AOperand: TdxPointF);
begin
  WriteOperand(AWriter, AOperand);
  WriteCommandName(AWriter);
end;

class function TdxPDFCustomCommand.GetName: string;
begin
  Result := '';
end;

function TdxPDFCustomCommand.GetCommandCount: Integer;
begin
  Result := 1;
end;

{ TdxPDFLineStyle }

class function TdxPDFLineStyle.CreateSolid: TdxPDFLineStyle;
begin
  Result := TdxPDFLineStyle.Create;
end;

class function TdxPDFLineStyle.Parse(AParameters: TdxPDFArray): TdxPDFLineStyle;
begin
  if (AParameters = nil) or (AParameters.Count <> 2) or (AParameters.Count = 2)
    and (AParameters[0].ObjectType <> otArray) then
    Result := CreateSolid
  else
    Result := TdxPDFLineStyle.Create(AParameters);
end;

constructor TdxPDFLineStyle.Create(const APattern: TDoubleDynArray;
APhase: Double);
begin
  inherited Create;
  FPattern := APattern;
  FPhase := APhase;
end;

constructor TdxPDFLineStyle.Create(APattern: TdxPDFReferencedObject);
var
  APhase: TdxPDFInteger;
begin
  APhase := TdxPDFInteger.Create(0);
  try
    Create(APattern, APhase);
  finally
    APhase.Free;
  end;
end;

constructor TdxPDFLineStyle.Create(APattern, APhase: TdxPDFReferencedObject);
begin
  inherited Create;
  if APhase <> nil then
    FPhase := AsDouble(APhase);
  if APattern <> nil then
    ReadPattern(APattern as TdxPDFArray);
end;

constructor TdxPDFLineStyle.Create(AParameters: TdxPDFArray);
begin
  inherited Create;
  if AParameters.Count = 2 then
    Create(AParameters[0], AParameters[1]);
end;

function TdxPDFLineStyle.IsDashed: Boolean;
begin
  Result := Length(FPattern) <> 0;
end;

function TdxPDFLineStyle.Write: TdxPDFArray;
begin
  if IsDashed then
  begin
    Result := TdxPDFArray.Create;
    Result.Add(WritePattern);
    Result.Add(FPhase);
  end
  else
    Result := nil;
end;

function TdxPDFLineStyle.AsDouble(AValue: TdxPDFReferencedObject): Double;
begin
  Result := TdxPDFNumericObjectAccess(AValue as TdxPDFNumericObject)
    .InternalValue;
end;

procedure TdxPDFLineStyle.ReadPattern(APattern: TdxPDFArray);
var
  I: Integer;
begin
  SetLength(FPattern, APattern.Count);
  for I := 0 to APattern.Count - 1 do
    FPattern[I] := AsDouble(APattern[I] as TdxPDFNumericObject);
end;

function TdxPDFLineStyle.WritePattern: TdxPDFArray;
var
  I: Integer;
begin
  Result := TdxPDFArray.Create;
  for I := 0 to Length(FPattern) - 1 do
    Result.Add(FPattern[I]);
end;

{ TdxPDFDocumentRepository }

constructor TdxPDFDocumentRepository.Create(AStream: TStream);
begin
  FStream := AStream;
  inherited Create;
end;

procedure TdxPDFDocumentRepository.Clear;
begin
  inherited Clear;
  FResolvedInteractiveFormFields.Clear;
  FResolvedWidgets.Clear;
  FFontDataStorage.Clear;
  FSharedResources.Clear;
  FObjectHolder.Clear;
  while FResolvedForms.Count > 0 do
    TdxPDFForm(FResolvedForms[0]).ReleaseCircularReferencesAndFree;
  dxTestCheck(FResolvedForms.Count = 0, 'FResolvedForms.Count <> 0');
end;

function TdxPDFDocumentRepository.CheckPassword(AAttemptsLimit: Integer;
AOnGetPasswordEvent: TdxGetPasswordEvent): Boolean;
begin
  Result := FEncryptionInfo.CheckPassword(AAttemptsLimit, AOnGetPasswordEvent);
end;

function TdxPDFDocumentRepository.CreateFont(AOwner: TdxPDFObject;
ADictionary: TdxPDFReaderDictionary): TdxPDFCustomFont;
begin
  EnterCriticalSection(FLock);
  try
    Result := TdxPDFCustomFont.Parse(AOwner, ADictionary);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TdxPDFDocumentRepository.CreateForm(ADictionary
  : TdxPDFReaderDictionary): TdxPDFForm;
begin
  Result := TdxPDFForm.CreateForm(ADictionary, nil);
  FResolvedForms.Add(Result);
end;

function TdxPDFDocumentRepository.CreateImage(AOwner: TdxPDFObject;
ADictionary: TdxPDFReaderDictionary): TdxPDFDocumentImage;
begin
  EnterCriticalSection(FLock);
  try
    if (ADictionary <> nil) and not FImageDataStorage.TryGetReference
      (ADictionary.Number, Result) then
    begin
      Result := TdxPDFDocumentImage.Create(AOwner);
      Result.Read(ADictionary);
      FImageDataStorage.Add(Result);
    end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TdxPDFDocumentRepository.GetAction(ANumber: Integer)
  : TdxPDFCustomAction;
var
  AObject: TdxPDFReferencedObject;
begin
  AObject := GetObject(ANumber);
  if (AObject <> nil) and (AObject is TdxPDFCustomAction) then
    Result := TdxPDFCustomAction(AObject)
  else if AObject is TdxPDFReaderDictionary then
  begin
    Result := TdxPDFCustomAction.Parse(TdxPDFReaderDictionary(AObject));
    ReplaceObject(ANumber, Result);
  end
  else
    Result := nil;
end;

function TdxPDFDocumentRepository.GetAction(AObject: TdxPDFBase)
  : TdxPDFCustomAction;
begin
  Result := GetAction(AObject.Number);
end;

function TdxPDFDocumentRepository.GetAnnotation(ANumber: Integer;
APage: TdxPDFPage): TdxPDFCustomAnnotation;
var
  ADictionary: TdxPDFReaderDictionary;
begin
  if not TryGetAnnotation(ANumber, Result) then
  begin
    ADictionary := GetDictionary(ANumber);
    Result := TdxPDFCustomAnnotation.Parse(ADictionary);
    if Result <> nil then
    begin
      Result.Number := ANumber;
      ADictionary.reference;
      ReplaceObject(Result.Number, Result);
      Result.Read(ADictionary, APage);
      dxPDFFreeObject(ADictionary);
    end;
  end;
end;

function TdxPDFDocumentRepository.GetAnnotation(AObject: TdxPDFBase;
APage: TdxPDFPage): TdxPDFCustomAnnotation;
begin
  Result := nil;
  if AObject <> nil then
  begin
    if TdxPDFUtils.IsIntegerValid(AObject.Number) then
      Result := GetAnnotation(AObject.Number, APage)
    else if AObject is TdxPDFReaderDictionary then
    begin
      Result := TdxPDFCustomAnnotation.Parse(TdxPDFReaderDictionary(AObject));
      Result.Read(TdxPDFReaderDictionary(AObject), APage);
      AddObject(Result);
    end;
  end;
end;

function TdxPDFDocumentRepository.GetDestination(ANumber: Integer)
  : TdxPDFCustomDestination;
var
  AAction: TdxPDFJumpAction;
  AActionType, AType: string;
  ADictionary: TdxPDFDictionary;
begin
  ADictionary := GetDictionary(ANumber);
  if ADictionary <> nil then
  begin
    AActionType := ADictionary.GetString(TdxPDFKeywords.ActionType);
    AType := ADictionary.GetString(TdxPDFKeywords.TypeKey);
    if (AActionType <> '') and ((AType = '') or (AType = TdxPDFKeywords.Action)
      or (AType = 'A')) then
    begin
      AAction := GetAction(ANumber) as TdxPDFJumpAction;
      if AAction <> nil then
      begin
        ReplaceObject(ANumber, AAction);
        Exit(TdxPDFJumpActionAccess(AAction).Destination);
      end;
    end;
  end;
  Result := GetDestination(GetObject(ANumber) as TdxPDFBase);
  if Result <> nil then
    ReplaceObject(ANumber, Result);
end;

function TdxPDFDocumentRepository.GetDestination(AObject: TdxPDFBase)
  : TdxPDFCustomDestination;
begin
  Result := TdxPDFCustomDestination.Parse(Catalog, AObject);
end;

function TdxPDFDocumentRepository.GetDictionary(ANumber: Integer)
  : TdxPDFReaderDictionary;
begin
  Result := inherited GetDictionary(ANumber) as TdxPDFReaderDictionary;
end;

function TdxPDFDocumentRepository.GetInteractiveFormField
  (AForm: TdxPDFInteractiveForm; AParent: TdxPDFInteractiveFormField;
ANumber: Integer): TdxPDFInteractiveFormField;

  procedure RegisterInteractiveFormField(AField: TdxPDFInteractiveFormField);
  begin
    FResolvedInteractiveFormFields.Add(ANumber, AField);
  end;

  function CreateInteractiveFormField(ADictionary: TdxPDFReaderDictionary)
    : TdxPDFInteractiveFormField;
  begin
    Result := TdxPDFInteractiveFormField.Parse(AParent, ADictionary, ANumber);
    if Result <> nil then
    begin
      AddObject(Result);
      Result.Initialize(AForm, AParent);
      RegisterInteractiveFormField(Result);
      Result.Read(ADictionary, ANumber);
      dxCallNotify(OnAddField, Result);
    end;
  end;

var
  ADictionary: TdxPDFReaderDictionary;
  AWidget: TdxPDFWidgetAnnotation;
begin
  Result := nil;
  if TdxPDFUtils.IsIntegerValid(ANumber) and
    not FResolvedInteractiveFormFields.TryGetValue(ANumber, TObject(Result))
  then
  begin
    ADictionary := GetDictionary(ANumber);
    if ADictionary <> nil then
      Exit(CreateInteractiveFormField(ADictionary));

    AWidget := GetWidget(ANumber) as TdxPDFWidgetAnnotation;
    if AWidget <> nil then
    begin
      if AWidget.InteractiveFormField <> nil then
      begin
        Result := AWidget.InteractiveFormField;
        RegisterInteractiveFormField(Result);
      end
      else
        Result := CreateInteractiveFormField
          (TdxPDFWidgetAnnotationAccess(AWidget).Dictionary);
    end;
  end;
end;

function TdxPDFDocumentRepository.ResolveInteractiveFormField(ANumber: Integer)
  : TdxPDFInteractiveFormField;
var
  ADictionary: TdxPDFReaderDictionary;
  AObject: TdxPDFReferencedObject;
  AParentNumber: Integer;
begin
  Result := nil;
  AObject := GetObject(ANumber);
  if AObject is TdxPDFCustomAnnotation then
    ADictionary := TdxPDFCustomAnnotation(AObject).Dictionary
  else
    ADictionary := GetDictionary(ANumber);

  if ADictionary <> nil then
  begin
    if ADictionary.TryGetReference(TdxPDFKeywords.Parent, AParentNumber) and
      TdxPDFUtils.IsIntegerValid(AParentNumber) then
      Result := GetInteractiveFormField(Catalog.AcroForm,
        ResolveInteractiveFormField(AParentNumber), ANumber)
    else
      Result := GetInteractiveFormField(Catalog.AcroForm, nil, ANumber);
  end;
end;

function TdxPDFDocumentRepository.GetPage(ANumber: Integer): TdxPDFPage;
begin
  Result := Catalog.Pages.FindPage(ANumber);
end;

function TdxPDFDocumentRepository.GetString(ANumber: Integer): string;
var
  AObject: TdxPDFBase;
begin
  AObject := GetObject(ANumber) as TdxPDFBase;
  if AObject is TdxPDFString then
    Result := TdxPDFString(AObject).Value
  else
    Result := '';
end;

function TdxPDFDocumentRepository.GetWidget(ANumber: Integer)
  : TdxPDFCustomAnnotation;

  function CheckIsPageWidgetsLoaded(APageData: TdxPDFPageData): Boolean;
  var
    AAnnotation: TdxPDFReferencedObject;
  begin
    Result := not APageData.AnnotationsLoaded;
    if Result then
    begin
      for AAnnotation in APageData.Annotations do
      begin
        if AAnnotation is TdxPDFWidgetAnnotation then
          FResolvedWidgets.AddOrSetValue(TdxPDFWidgetAnnotation(AAnnotation)
            .Number, AAnnotation);
      end;
    end;
  end;

  function FindWidget(ANumber: Integer): TdxPDFCustomAnnotation;
  var
    AHasChanges: Boolean;
    AObject: TObject;
    I: Integer;
  begin
    AObject := GetObject(ANumber);
    if AObject is TdxPDFWidgetAnnotation then
      Exit(TdxPDFWidgetAnnotation(AObject));

    AHasChanges := False;
    for I := 0 to Catalog.Pages.Count - 1 do
      AHasChanges := CheckIsPageWidgetsLoaded(Catalog.Pages[I].Data) or
        AHasChanges;
    if not(AHasChanges and FResolvedWidgets.TryGetValue(ANumber,
      TObject(Result))) then
      Result := nil;
  end;

begin
  if not FResolvedWidgets.TryGetValue(ANumber, TObject(Result)) then
  begin
    Result := FindWidget(ANumber);
    FResolvedWidgets.AddOrSetValue(ANumber, Result);
  end;
end;

function TdxPDFDocumentRepository.IsSharedResources
  (AResources: TdxPDFResources): Boolean;
begin
  Result := (AResources <> nil) and FSharedResources.ContainsKey(AResources.ID,
    AResources.Number);
end;

function TdxPDFDocumentRepository.IsValidReferences: Boolean;
var
  AKey: Integer;
  AReference: TdxPDFReference;
begin
  Result := True;
  for AKey in References.Keys do
    if References[AKey] is TdxPDFReference then
    begin
      AReference := TdxPDFReference(References[AKey]);
      if AReference.IsSlot and (AReference.Offset <> 0) and
        not FParser.IsValidReference(AReference) then
      begin
        Remove(AKey);
        Result := False;
      end;
    end;
end;

function TdxPDFDocumentRepository.TryGetAnnotation(ANumber: Integer;
out AAnnotation: TdxPDFCustomAnnotation): Boolean;
var
  AObject: TdxPDFReferencedObject;
begin
  AObject := GetObject(ANumber);
  Result := AObject is TdxPDFCustomAnnotation;
  if Result then
    AAnnotation := TdxPDFCustomAnnotation(AObject)
  else
    AAnnotation := nil;
end;

function TdxPDFDocumentRepository.TryGetDictionary(ANumber: Integer;
out ADictionary: TdxPDFDictionary): Boolean;
begin
  ADictionary := GetDictionary(ANumber);
  Result := ADictionary <> nil;
end;

procedure TdxPDFDocumentRepository.AddStreamElement(ANumber: Integer;
AObject: TdxPDFReferencedObject);
begin
  TryAdd(ANumber, AObject, False);
end;

procedure TdxPDFDocumentRepository.ReadEncryptionInfo
  (ADictionary: TdxPDFDictionary; const ADocumentID: TdxPDFDocumentID);
begin
  FreeAndNil(FEncryptionInfo);
  FEncryptionInfo := TdxPDFEncryptionInfo.Create(ADocumentID, ADictionary);
end;

procedure TdxPDFDocumentRepository.RemoveCorruptedObjects;
var
  AKey: Integer;
  AList: TdxPDFIntegerList;
  AValue: TdxPDFReferencedObject;
  I: Integer;
begin
  AList := TdxPDFIntegerList.Create;
  try
    AList.AddRange(References.Keys);
    for I := 0 to AList.Count - 1 do
    begin
      AKey := AList.Items[I];
      if References.TryGetValue(AKey, AValue) and
        not(AValue is TdxPDFStreamElement) then
        Remove(AKey);
    end;
  finally
    AList.Free;
  end;
end;

function TdxPDFDocumentRepository.ResolveObject(ANumber: Integer)
  : TdxPDFReferencedObject;
begin
  EnterCriticalSection(FLock);
  try
    if not References.TryGetValue(ANumber, Result) then
      if not References.TryGetValue(ANumber, Result) then
        Exit(nil);
    if Result is TdxPDFBase then
      case TdxPDFBase(Result).ObjectType of
        otIndirectObject:
          Result := ResolveIndirectObject(TdxPDFIndirectObject(Result));
        otIndirectReference:
          Result := ResolveIndirectReference(TdxPDFReference(Result));
        otStreamElement:
          Result := ResolveStreamElement(TdxPDFStreamElement(Result));
      end;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

procedure TdxPDFDocumentRepository.AddObject(AObject: TdxPDFObject);
begin
  FObjectHolder.Add(AObject);
end;

procedure TdxPDFDocumentRepository.DeleteObject(AObject: TdxPDFObject);
begin
  FObjectHolder.Remove(AObject);
end;

procedure TdxPDFDocumentRepository.RemoveResolvedForm(AForm: TdxPDFForm);
begin
  FResolvedForms.Remove(AForm);
end;

procedure TdxPDFDocumentRepository.CreateSubClasses;

  function GetTempFolderPath: string;
  begin
    if sdxPDFTempFolder = '' then
      Result := TPath.GetTempPath
    else
      Result := sdxPDFTempFolder;
  end;

begin
  inherited CreateSubClasses;
  InitializeCriticalSectionAndSpinCount(FLock, 1024);
  FParser := TdxPDFDocumentParser.Create(Self, FStream);
  FEncryptionInfo := nil;
  FSharedResources := TdxPDFUniqueReferences.Create;
  FFolderName := GetTempFolderPath + dxGenerateID +
    TPath.DirectorySeparatorChar;
  FFontDataStorage := TdxPDFFontDataStorage.Create(FFolderName);
  FImageDataStorage := TdxPDFDocumentImageDataStorage.Create(300);
  FObjectHolder := TdxPDFReferencedObjects.Create;
  FResolvedInteractiveFormFields := TdxPDFObjectIndex.Create(1024);
  FResolvedWidgets := TdxPDFObjectIndex.Create(1024);
  FXReferences := TDictionary<Integer, Int64>.Create;
  FResolvedForms := TdxFastList.Create;
end;

procedure TdxPDFDocumentRepository.DestroySubClasses;
begin
  FreeAndNil(FXReferences);
  FreeAndNil(FResolvedWidgets);
  FreeAndNil(FResolvedInteractiveFormFields);
  FreeAndNil(FImageDataStorage);
  FreeAndNil(FFontDataStorage);
  FreeAndNil(FEncryptionInfo);
  FreeAndNil(FStream);
  FreeAndNil(FSharedResources);
  FreeAndNil(FParser);
  if DirectoryExists(FFolderName) then
    RemoveDir(FFolderName);
  FreeAndNil(FObjectHolder);
  FreeAndNil(FResolvedForms);
  DeleteCriticalSection(FLock);
  inherited DestroySubClasses;
end;

function TdxPDFDocumentRepository.IsResourcesShared
  (AResources: TdxPDFResources): Boolean;
begin
  Result := FSharedResources.ContainsValue(AResources);
end;

function TdxPDFDocumentRepository.CreateAcroForm: TdxPDFInteractiveForm;
begin
  Result := TdxPDFInteractiveForm.Create;
  AddObject(Result);
end;

function TdxPDFDocumentRepository.CreateWidgetAnnotation(APage: TdxPDFPage;
const ARect: TdxPDFRectangle; AFlags: TdxPDFAnnotationFlags)
  : TdxPDFCustomAnnotation;
begin
  Result := TdxPDFWidgetAnnotation.Create(APage, ARect, AFlags);
  APage.AddAnnotation(Result);
  AddObject(Result);
end;

function TdxPDFDocumentRepository.GetResources(ADictionary
  : TdxPDFReaderDictionary): TdxPDFResources;

  function DoRead(ANumber: Integer; ADictionary: TdxPDFReaderDictionary)
    : TdxPDFResources;
  var
    AObject: TdxPDFBase;
    ATempResources: TdxPDFResources;
  begin
    if not TdxPDFUtils.IsIntegerValid(ANumber) or
      not FSharedResources.TryGetValue(ANumber, AObject) then
    begin
      ATempResources := TdxPDFResources.Create(nil);
      ATempResources.Read(ADictionary);
      FSharedResources.Add(ATempResources.ID, ANumber, ATempResources)
    end
    else
      ATempResources := AObject as TdxPDFResources;
    Result := ATempResources;
  end;

var
  ANumber: Integer;
  AResourcesDictionary: TdxPDFReaderDictionary;
begin
  Result := nil;
  AResourcesDictionary := ADictionary.GetDictionary(TdxPDFKeywords.Resources);
  if AResourcesDictionary <> nil then
  begin
    ANumber := AResourcesDictionary.Number;
    if not TdxPDFUtils.IsIntegerValid(ANumber) then
    begin
      ANumber := ADictionary.Number;
      if not TdxPDFUtils.IsIntegerValid(ANumber) and
        (ADictionary.StreamRef <> nil) then
        ANumber := ADictionary.StreamRef.Number;
    end;
    Result := DoRead(ANumber, AResourcesDictionary);
  end;
end;

function TdxPDFDocumentRepository.CreateObjectParser(AObjectNumber: Integer)
  : TdxPDFStructureParser;
begin
  if EncryptionInfo <> nil then
    Result := TdxPDFEncryptedStructureParser.Create(Self, AObjectNumber)
  else
    Result := TdxPDFStructureParser.Create(Self);
end;

function TdxPDFDocumentRepository.GetObjectCount: Integer;
begin
  Result := References.Count;
end;

function TdxPDFDocumentRepository.ResolveIndirectObject
  (AObject: TdxPDFIndirectObject): TdxPDFReferencedObject;
var
  AParser: TdxPDFStructureParser;
begin
  AParser := CreateObjectParser(AObject.Number);
  try
    Result := AParser.ReadObject(AObject.Data);
    ReplaceObject(AObject.Number, Result);
  finally
    AParser.Free;
  end;
end;

function TdxPDFDocumentRepository.ResolveIndirectReference
  (AReference: TdxPDFReference): TdxPDFReferencedObject;

  procedure FindAndUpdateInvalidReference(AReference: TdxPDFReference);
  var
    AOffset: Int64;
    ASlot: TdxPDFReference;
  begin
    if Parser.TryFindObjectOffset(AReference.Number, AReference.Generation,
      AOffset) then
    begin
      ASlot := Parser.ReadObjectSlot(AOffset);
      try
        FXReferences.AddOrSetValue(ASlot.Number, ASlot.Offset);
      finally
        ASlot.Free;
      end;
    end;
  end;

  function InternalReadObject(AOffset: Int64): TdxPDFReferencedObject;
  var
    AObject: TdxPDFIndirectObject;
  begin
    AObject := FParser.ReadIndirectObject(AOffset);
    try
      Result := ResolveIndirectObject(AObject as TdxPDFIndirectObject);
    finally
      FreeAndNil(AObject);
    end;
  end;

var
  AOffset: Int64;
begin
  if AReference.IsFree then
    Exit(nil);
  if Parser.IsValidReference(AReference) then
    Result := InternalReadObject(AReference.Offset)
  else
  begin
    FindAndUpdateInvalidReference(AReference);
    if FXReferences.TryGetValue(AReference.Number, AOffset) then
      Result := InternalReadObject(AOffset)
    else
      Result := nil;
  end;
end;

function TdxPDFDocumentRepository.ResolveStreamElement
  (AElement: TdxPDFStreamElement): TdxPDFReferencedObject;
var
  AStream: TdxPDFStream;
begin
  Result := GetObject(AElement.Number);
  if Result is TdxPDFBase then
  begin
    if TdxPDFBase(Result).ObjectType <> otObjectStream then
    begin
      if TdxPDFBase(Result).ObjectType = otStream then
        AStream := Result as TdxPDFStream
      else
        AStream := GetStream(AElement.Number);
      TdxPDFBase(AStream).Number := AElement.Number;
      Result := TdxPDFReaderObjectStream.Create(AElement.Number, AStream);
      Replace(AElement.Number, Result);
    end;
    if TdxPDFBase(Result).ObjectType <> otStream then
      Result := TdxPDFReaderObjectStream(Result).Objects[AElement.Index]
        as TdxPDFBase
    else
      Result := AElement;
  end;
end;

procedure TdxPDFDocumentRepository.ReplaceObject(ANumber: Integer;
AObject: TdxPDFReferencedObject);
begin
  if AObject <> nil then
  begin
    Replace(ANumber, AObject);
    TdxPDFBase(AObject).Number := ANumber;
  end;
end;

{ TdxPDFReaderDictionary }

constructor TdxPDFReaderDictionary.Create(ARepository
  : TdxPDFDocumentRepository);
begin
  inherited Create;
  FRepository := ARepository;
end;

function TdxPDFReaderDictionary.GetAction(const AKey: string)
  : TdxPDFCustomAction;
var
  AObject: TdxPDFBase;
  ADictionary: TdxPDFReaderDictionary;
begin
  Result := nil;
  if TryGetObject(AKey, AObject) then
  begin
    case AObject.ObjectType of
      otIndirectReference:
        ADictionary := Repository.GetObject(AObject.Number)
          as TdxPDFReaderDictionary;
      otDictionary:
        ADictionary := AObject as TdxPDFReaderDictionary;
    else
      ADictionary := nil;
    end;
    if ADictionary <> nil then
      Result := TdxPDFCustomAction.Parse(ADictionary);
  end;
end;

function TdxPDFReaderDictionary.GetAnnotationAppearance(AObject: TdxPDFBase;
const AParentBBox: TdxPDFRectangle): TdxPDFAnnotationAppearances;
var
  AResolvedObject: TdxPDFBase;
begin
  Result := nil;
  if AObject <> nil then
    case AObject.ObjectType of
      otDictionary:
        begin
          Result := TdxPDFAnnotationAppearances.Create(nil);
          Result.Read(TdxPDFReaderDictionary(AObject), AParentBBox);
        end;
      otStream:
        Result := TdxPDFAnnotationAppearances.Create
          (TdxPDFForm.CreateForm(TdxPDFStream(AObject), nil));
      otIndirectReference:
        begin
          AResolvedObject := Repository.GetObject(AObject.Number) as TdxPDFBase;
          if not(AResolvedObject is TdxPDFAnnotationAppearances) then
          begin
            if AResolvedObject.ObjectType = otStream then
            begin
              Result := TdxPDFAnnotationAppearances.Create(nil);
              Result.Read(GetForm(AResolvedObject.Number));
            end
            else
              Result := GetAnnotationAppearance(AResolvedObject, AParentBBox);
          end
          else
            Result := TdxPDFAnnotationAppearances(AResolvedObject);
        end
    end;
end;

function TdxPDFReaderDictionary.GetAnnotationHighlightingMode
  : TdxPDFAnnotationHighlightingMode;
var
  AName: string;
  AMode: TdxPDFAnnotationHighlightingMode;
begin
  AName := GetString(TdxPDFKeywords.AnnotationHighlightingMode);
  for AMode := Low(TdxPDFAnnotationHighlightingMode)
    to High(TdxPDFAnnotationHighlightingMode) do
  begin
    if dxPDFAnnotationHighlightingModeIdMap[AMode] = AName then
      Exit(AMode);
  end;
  Result := ahmNone;
end;

function TdxPDFReaderDictionary.GetAppearance(AResources: TdxPDFResources)
  : TdxPDFCommandList;
var
  AData: TBytes;
begin
  AData := GetBytes(TdxPDFKeywords.DictionaryAppearance);
  if (Length(AData) > 0) and (AResources <> nil) then
  begin
    Result := TdxPDFCommandList.Create;
    TdxPDFCommandStreamParser.Parse(Repository, AData, Result, AResources);
  end
  else
    Result := nil;
end;

function TdxPDFReaderDictionary.GetColor(const AKey: string): TdxPDFColor;
var
  AArray: TdxPDFArray;
begin
  AArray := GetArray(AKey);
  if (AArray <> nil) and (AArray.Count in [1, 3, 4]) then
    Result := TdxPDFColor.Create(AArray)
  else
    Result := TdxPDFColor.Null;
end;

function TdxPDFReaderDictionary.GetDeferredFormFieldCollection
  (const AKey: string): TdxPDFInteractiveFormFieldCollection;
begin
  Result := nil;
end;

function TdxPDFReaderDictionary.GetDestinationInfo(const AKey: string)
  : TdxPDFDestinationInfo;
var
  ANumber: Integer;
  AObject: TdxPDFBase;
begin
  Result := TdxPDFDestinationInfo.Invalid;
  if TryGetObject(AKey, AObject) then
    case AObject.ObjectType of
      otIndirectReference:
        begin
          ANumber := AObject.Number;
          AObject := Repository.GetDestination(ANumber);
          if AObject <> nil then
            Result := TdxPDFDestinationInfo.Create
              (AObject as TdxPDFCustomDestination)
          else
            Result := TdxPDFDestinationInfo.Create
              (Repository.GetString(ANumber))
        end;
      otString, otName:
        Result := TdxPDFDestinationInfo.Create(TdxPDFString(AObject).Value);
      otArray:
        Result := TdxPDFDestinationInfo.Create
          (Repository.GetDestination(TdxPDFArray(AObject)));
    else
      TdxPDFUtils.RaiseException;
    end;
end;

function TdxPDFReaderDictionary.GetDictionary(const AKey: string;
const AAlternateKey: string = ''): TdxPDFReaderDictionary;
begin
  Result := inherited GetDictionary(AKey) as TdxPDFReaderDictionary;
  if (Result = nil) and (AAlternateKey <> '') then
    Result := GetDictionary(AAlternateKey);
end;

function TdxPDFReaderDictionary.GetForm(ANumber: Integer): TdxPDFForm;
var
  AObjectType: string;
  AStream: TdxPDFStream;
begin
  Result := nil;
  AStream := Repository.GetStream(ANumber);
  if AStream <> nil then
  begin
    AObjectType := AStream.Dictionary.GetString(TdxPDFKeywords.TypeKey);
    if (AObjectType = '') or (AObjectType = TdxPDFKeywords.XObject) then
    begin
      Result := TdxPDFForm.CreateForm(AStream, nil);
      Result.Number := ANumber;
    end;
  end;
end;

function TdxPDFReaderDictionary.GetObject(const AKey: string): TdxPDFBase;
begin
  Result := inherited GetObject(AKey);
  if FRepository <> nil then
    Result := FRepository.ResolveReference(Result);
end;

function TdxPDFReaderDictionary.GetObjectNumber(const AKey: string): Integer;
begin
  if Contains(AKey) then
  begin
    Result := inherited GetObject(AKey).Number;
    if Result = 0 then
      Result := dxPDFInvalidValue;
  end
  else
    Result := dxPDFInvalidValue;
end;

function TdxPDFReaderDictionary.GetTextJustification: TdxPDFTextJustification;
begin
  Result := TdxPDFTextJustification(GetInteger('Q', 0));
end;

procedure TdxPDFReaderDictionary.PopulateList(const AKey: string;
AList: TStringList);
var
  AValue, AElement: TdxPDFBase;
begin
  AList.Clear;
  if not TryGetObject(AKey, AValue) then
    Exit;
  if AValue.ObjectType = otString then
    AList.Add((AValue as TdxPDFString).Text)
  else if AValue.ObjectType = otArray then
    for AElement in TdxPDFArray(AValue).ElementList do
      AList.Add((AElement as TdxPDFString).Text);
end;

function TdxPDFReaderDictionary.TryGetDictionary(const AKey: string;
out AValue: TdxPDFReaderDictionary): Boolean;
begin
  AValue := GetDictionary(AKey);
  Result := AValue <> nil;
end;

function TdxPDFReaderDictionary.TryGetStreamDictionary(const AKey: string;
out AValue: TdxPDFReaderDictionary): Boolean;
var
  AStream: TdxPDFStream;
begin
  Result := TryGetStream(AKey, AStream);
  if Result then
  begin
    AValue := AStream.Dictionary as TdxPDFReaderDictionary;
    AValue.StreamRef := AStream;
  end
  else
    Result := TryGetDictionary(AKey, AValue);
end;

function TdxPDFReaderDictionary.GetResources(const AKey: string)
  : TdxPDFResources;
var
  AObject: TdxPDFBase;
  ADictionary: TdxPDFReaderDictionary;
begin
  Result := TdxPDFResources.Create(nil);
  if TryGetObject(AKey, AObject) then
  begin
    ADictionary := Repository.GetDictionary(AObject.Number);
    if (ADictionary = nil) and (AObject.ObjectType = otDictionary) then
      ADictionary := AObject as TdxPDFReaderDictionary;
    Result.Read(ADictionary);
  end;
end;

{ TdxPDFAcroFormField }

constructor TdxPDFAcroFormField.Create;
begin
  inherited Create;
  FDocumentState := nil;
  Field := nil;
end;

destructor TdxPDFAcroFormField.Destroy;
begin
  Field := nil;
  inherited Destroy;
end;

function TdxPDFAcroFormField.GetAnnotation: TdxPDFCustomAnnotation;
begin
  if Field <> nil then
    Result := Field.Widget
  else
    Result := nil;
end;

function TdxPDFAcroFormField.GetBounds: TdxPDFOrientedRect;
var
  AAnnotation: TdxPDFCustomAnnotation;
  R: TdxPDFRectangle;
begin
  AAnnotation := Annotation;
  if AAnnotation = nil then
    R := TdxPDFRectangle.Null
  else
    R := AAnnotation.Rect;
  Result := TdxPDFOrientedRect.Create(R);
end;

function TdxPDFAcroFormField.GetCursor: TCursor;
begin
  Result := crDefault;
end;

function TdxPDFAcroFormField.GetFlags: TdxPDFInteractiveFormFieldFlags;
begin
  Result := ffNone;
end;

function TdxPDFAcroFormField.GetHint: string;
begin
  Result := '';
end;

function TdxPDFAcroFormField.GetInteractiveOperation
  : TdxPDFInteractiveOperation;
begin
  Result := Result.Invalid;
end;

function TdxPDFAcroFormField.GetName: string;
begin
  if Field <> nil then
    Result := Field.FullName
  else
    Result := '';
end;

procedure TdxPDFAcroFormField.SetField(const AValue
  : TdxPDFInteractiveFormField);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FField));
end;

function TdxPDFAcroFormField.GetPage: TdxPDFPage;
begin
  if Annotation <> nil then
    Result := Annotation.Page
  else
    Result := nil;
end;

function TdxPDFAcroFormField.GetPageIndex: Integer;
begin
  Result := FDocumentState.GetPageIndex(Page);
end;

function TdxPDFAcroFormField.GetRect: TdxRectF;
begin
  Result := Bounds.Rect;
end;

{ TdxPDFAcroFormActionField }

function TdxPDFAcroFormActionField.GetCursor: TCursor;
begin
  if GetInteractiveOperation.IsValid then
    Result := crHandPoint
  else
    Result := inherited GetCursor;
end;

function TdxPDFAcroFormActionField.GetInteractiveOperation
  : TdxPDFInteractiveOperation;
begin
  Result := (Annotation as TdxPDFActionAnnotation).InteractiveOperation;
end;

function TdxPDFAcroFormActionField.IsResetFocusingNeeded: Boolean;
begin
  Result := True;
end;

procedure TdxPDFAcroFormActionField.ExecuteOperation(const AController
  : IdxPDFInteractivityController);
begin
  AController.ExecuteOperation(Self);
end;

{ TdxPDFAnnotationField }

destructor TdxPDFAnnotationField.Destroy;
begin
  SetAnnotation(nil);
  inherited Destroy;
end;

procedure TdxPDFAnnotationField.SetAnnotation(const AValue
  : TdxPDFCustomAnnotation);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FAnnotation));
end;

function TdxPDFAnnotationField.GetAnnotation: TdxPDFCustomAnnotation;
begin
  Result := FAnnotation;
end;

function TdxPDFAnnotationField.GetBounds: TdxPDFOrientedRect;
begin
  Result := TdxPDFOrientedRect.Create
    (TdxPDFMarkupAnnotationAccess(FAnnotation).Bounds);
end;

function TdxPDFAnnotationField.GetHint: string;
begin
  Result := TdxPDFMarkupAnnotationAccess(FAnnotation).Hint;
end;

function TdxPDFAnnotationField.GetHitCode: Integer;
begin
  Result := hcAnnotationObject;
end;

{ TdxPDFHyperlink }

function TdxPDFHyperlink.GetHitCode: Integer;
begin
  Result := hcHyperlink;
end;

function TdxPDFHyperlink.GetHint: string;
begin
  Result := TdxPDFLinkAnnotation(FAnnotation).Hint;
end;

{ TdxPDFRecognizedContent }

constructor TdxPDFRecognizedContent.Create;
begin
  inherited Create;
  FAcroFormFields := TdxPDFAcroFormFieldList.Create;
  FAnnotationFields := TdxPDFAnnotationFieldList.Create;
  FAttachments := TList<TdxPDFFileAttachment>.Create;
  FHyperlinks := TdxPDFHyperlinkList.Create;
  FImages := TdxPDFImageList.Create;
  FTextLines := TdxPDFTextLineList.Create;
end;

destructor TdxPDFRecognizedContent.Destroy;
begin
  FreeAndNil(FTextLines);
  FreeAndNil(FImages);
  FreeAndNil(FHyperlinks);
  FreeAndNil(FAttachments);
  FreeAndNil(FAnnotationFields);
  FreeAndNil(FAcroFormFields);
  inherited Destroy;
end;

function TdxPDFRecognizedContent.GetText: string;
var
  ARange: TdxPDFPageTextRange;
begin
  ARange := TdxPDFPageTextRange.Create(0);
  Result := TdxPDFTextUtils.ConvertToString(ARange.StartPosition,
    ARange.EndPosition, FTextLines);
end;

procedure TdxPDFRecognizedContent.AddAnnotationField
  (AField: TdxPDFAnnotationField);
begin
  FAnnotationFields.Add(AField);
  if AField.HitCode = hcAttachment then
    FAttachments.Add(TdxPDFFileAttachmentAnnotationField(AField).Attachment);
end;

{ TdxPDFGraphicsState }

constructor TdxPDFGraphicsState.Create;
var
  AComponents: TDoubleDynArray;
begin
  inherited Create;
  RecreateParameters;
  RecreateTextState;
  FDeviceTransformMatrix := TdxPDFTransformationMatrix.Create;
  FTransformMatrix := TdxPDFTransformationMatrix.Create;
  SetLength(AComponents, 1);
  AComponents[0] := 0;
  StrokingColor := TdxPDFColor.Create(AComponents);
  NonStrokingColor := TdxPDFColor.Create(AComponents);
  StrokingColorSpace := TdxPDFGrayDeviceColorSpace.Create(nil);
  NonStrokingColorSpace := TdxPDFGrayDeviceColorSpace.Create(nil);
end;

destructor TdxPDFGraphicsState.Destroy;
begin
  NonStrokingColorSpace := nil;
  StrokingColorSpace := nil;
  FreeAndNil(FTextState);
  FreeAndNil(FParameters);
  inherited Destroy;
end;

procedure TdxPDFGraphicsState.Assign(AGraphicsState: TdxPDFGraphicsState);
begin
  if AGraphicsState <> nil then
  begin
    ApplyParameters(AGraphicsState.Parameters);
    StrokingColor := AGraphicsState.StrokingColor;
    StrokingColorSpace := AGraphicsState.StrokingColorSpace;
    NonStrokingColor := AGraphicsState.NonStrokingColor;
    NonStrokingColorSpace := AGraphicsState.NonStrokingColorSpace;
    TextState.Assign(AGraphicsState.TextState);
    TransformMatrix.Assign(AGraphicsState.TransformMatrix);
  end;
end;

procedure TdxPDFGraphicsState.ApplyParameters(AParameters
  : TdxPDFGraphicsStateParameters);
begin
  if AParameters <> nil then
  begin
    Parameters.Assign(AParameters);
    if Parameters.IsSoftMaskChanged then
      SoftMaskTransformMatrix := TransformMatrix;
    if AParameters.Font <> nil then
    begin
      TextState.Font := AParameters.Font;
      TextState.FontSize := AParameters.FontSize;
    end;
  end;
end;

procedure TdxPDFGraphicsState.Reset;
begin
  TransformMatrix.Reset;
  RecreateParameters;
  RecreateTextState;
end;

procedure TdxPDFGraphicsState.RecreateTextState;
begin
  FreeAndNil(FTextState);
  FTextState := TdxPDFTextState.Create;
end;

procedure TdxPDFGraphicsState.RecreateParameters;
begin
  FreeAndNil(FParameters);
  FParameters := TdxPDFGraphicsStateParameters.Create(nil);
end;

procedure TdxPDFGraphicsState.SetNonStrokingColorSpace
  (const AValue: TdxPDFCustomColorSpace);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FNonStrokingColorSpace));
end;

procedure TdxPDFGraphicsState.SetStrokingColorSpace(const AValue
  : TdxPDFCustomColorSpace);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FStrokingColorSpace));
end;

{ TdxPDFFontRegistratorParameters }

class function TdxPDFFontRegistratorParameters.Create(const AName: string;
AWeight: Integer; AIsItalic: Boolean): TdxPDFFontRegistratorParameters;
begin
  Result.FIsItalic := AIsItalic;
  Result.FName := AName;
  Result.FWeight := AWeight;
end;

{ TdxPDFFontDescriptorData }

class function TdxPDFFontDescriptorData.Create(AFontMetrics
  : TdxFontFileFontMetrics; AFontFlags: Integer; const AItalicAngle: Double;
ABold: Boolean; ANumGlyphs: Integer): TdxPDFFontDescriptorData;
begin
  Result.FAscent := AFontMetrics.EmAscent;
  Result.FBBox := AFontMetrics.EmBBox;
  Result.FBold := ABold;
  Result.FDescent := AFontMetrics.EmDescent;
  Result.FFontFlags := AFontFlags;
  Result.FItalicAngle := AItalicAngle;
  Result.FNumGlyphs := ANumGlyphs;
end;

{ TdxPDFDestinationInfo }

class function TdxPDFDestinationInfo.Create(ADestination
  : TdxPDFCustomDestination): TdxPDFDestinationInfo;
begin
  Result := Invalid;
  Result.FDestination := ADestination;
  ADestination.reference;
end;

class function TdxPDFDestinationInfo.Create(const AName: string)
  : TdxPDFDestinationInfo;
begin
  Result := Invalid;
  Result.FName := AName;
end;

class function TdxPDFDestinationInfo.Invalid: TdxPDFDestinationInfo;
begin
  Result.FDestination := nil;
  Result.FName := '';
end;

procedure TdxPDFDestinationInfo.Finalize;
begin
  dxPDFFreeObject(FDestination);
end;

function TdxPDFDestinationInfo.GetDestination(ACatalog: TdxPDFCatalog;
AInternal: Boolean): TdxPDFCustomDestination;
begin
  if (FDestination = nil) and (FName <> '') then
  begin
    FDestination := ACatalog.GetDestination(FName);
    if FDestination <> nil then
      FDestination.reference;
  end;
  if AInternal and (FDestination <> nil) then
    FDestination.ResolveInternalPage;
  Result := FDestination;
end;

function TdxPDFDestinationInfo.IsValid: Boolean;
begin
  Result := (FDestination <> nil) or (FName <> '');
end;

function TdxPDFDestinationInfo.Write(AHelper: TdxPDFWriterHelper): TdxPDFBase;
begin
  if FDestination <> nil then
    Result := FDestination.Write(AHelper)
  else if FName <> '' then
    Result := TdxPDFString.Create(FName)
  else
    Result := nil;
end;

{ TdxPDFTarget }

class function TdxPDFTarget.Create(AMode: TdxPDFTargetMode; APageIndex: Integer;
X, Y: Single): TdxPDFTarget;
begin
  Result := CreateEx(AMode, APageIndex, X, Y, 0, 0, dxPDFInvalidValue);
end;

class function TdxPDFTarget.Create(AMode: TdxPDFTargetMode; APageIndex: Integer)
  : TdxPDFTarget;
begin
  Result := CreateEx(AMode, APageIndex, dxPDFInvalidValue, dxPDFInvalidValue, 0,
    0, dxPDFInvalidValue);
end;

class function TdxPDFTarget.Create(AMode: TdxPDFTargetMode; APageIndex: Integer;
const R: TdxRectF): TdxPDFTarget;
begin
  Result := Invalid;
  Result.FMode := AMode;
  Result.FPageIndex := APageIndex;
  Result.FX := R.Left;
  Result.FY := R.Top;
  Result.FWidth := R.Width;
  Result.FHeight := R.Height;
  Result.FZoom := dxPDFInvalidValue;
end;

class function TdxPDFTarget.Create(APageIndex: Integer; X, Y, AZoom: Single)
  : TdxPDFTarget;
begin
  Result := CreateEx(tmXYZ, APageIndex, X, Y, 0, 0, AZoom);
end;

class function TdxPDFTarget.CreateEx(AMode: TdxPDFTargetMode;
APageIndex: Integer; X, Y, AWidth, AHeight, AZoom: Single): TdxPDFTarget;
begin
  Result := Invalid;
  Result.FMode := AMode;
  Result.FPageIndex := APageIndex;
  Result.FX := X;
  Result.FY := Y;
  Result.FWidth := AWidth;
  Result.FHeight := AHeight;
  Result.FZoom := AZoom;
end;

class function TdxPDFTarget.Invalid: TdxPDFTarget;
begin
  Result.FPageIndex := dxPDFInvalidValue;
  Result.FMode := tmXYZ;
  Result.FHeight := dxPDFInvalidValue;
  Result.FWidth := dxPDFInvalidValue;
  Result.FX := dxPDFInvalidValue;
  Result.FY := dxPDFInvalidValue;
  Result.FZoom := dxPDFInvalidValue;
end;

function TdxPDFTarget.IsValid: Boolean;
begin
  Result := TdxPDFUtils.IsIntegerValid(FPageIndex);
end;

{ TdxPDFInteractiveOperation }

class function TdxPDFInteractiveOperation.Create(AAction: TdxPDFCustomAction)
  : TdxPDFInteractiveOperation;
begin
  Result := Invalid;
  Result.FAction := AAction;
end;

class function TdxPDFInteractiveOperation.Create(AAction: TdxPDFCustomAction;
ADestination: TdxPDFCustomDestination): TdxPDFInteractiveOperation;
begin
  Result.FAction := AAction;
  Result.FDestination := ADestination;
end;

class function TdxPDFInteractiveOperation.Invalid: TdxPDFInteractiveOperation;
begin
  Result.FAction := nil;
  Result.FDestination := nil;
end;

function TdxPDFInteractiveOperation.IsValid: Boolean;
begin
  Result := (FDestination <> nil) or (FAction <> nil);
end;

function TdxPDFInteractiveOperation.GetTarget: TdxPDFTarget;
begin
  if Destination <> nil then
    Result := Destination.GetTarget
  else if (Action <> nil) and (Action is TdxPDFGoToAction) then
    Result := TdxPDFGoToAction(Action).Destination.GetTarget
  else
    Result := TdxPDFTarget.Invalid;
end;

{ TdxPDFTextParser }

constructor TdxPDFTextParser.Create(const APageCropBox: TdxRectF;
AContent: TdxPDFRecognizedContent);
begin
  inherited Create;
  FPageBlocks := TObjectList<TdxPDFTextBlock>.Create;
  FFontDataStorage := TObjectDictionary<TdxPDFCustomFont, TdxPDFFontData>.Create
    ([doOwnsValues]);
  FPageCropBox := cxRectAdjustF(APageCropBox);
  FContent := AContent;
end;

destructor TdxPDFTextParser.Destroy;
begin
  FreeAndNil(FFontDataStorage);
  FreeAndNil(FPageBlocks);
  inherited Destroy;
end;

procedure TdxPDFTextParser.AddBlock(const AStringData: TdxPDFStringData;
AState: TdxPDFGraphicsState);
var
  AFontData: TdxPDFFontData;
  AFont: TdxPDFCustomFont;
begin
  AFont := AState.TextState.Font as TdxPDFCustomFont;
  if (Length(AStringData.CharCodes) > 0) and (Length(AStringData.Advances) > 0)
  then
  begin
    if not FFontDataStorage.TryGetValue(AFont, AFontData) then
    begin
      AFontData := TdxPDFFontData.CreateFontData(AFont);
      FFontDataStorage.Add(AFont, AFontData);
    end;
    FPageBlocks.Add(TdxPDFTextBlock.Create(AStringData, AFontData, AState));
  end;
end;

procedure TdxPDFTextParser.Parse;
var
  ABuilder: TdxPDFPageTextLineBuilder;
begin
  if (FPageBlocks <> nil) and (FPageBlocks.Count >= 1) then
  begin
    FParserState := TdxPDFTextParserState.Create(FPageBlocks,
      dxRectF(0, 0, FPageCropBox.Width, FPageCropBox.Height));
    try
      ABuilder := TdxPDFPageTextLineBuilder.Create(FParserState);
      try
        ABuilder.Populate(FContent.TextLines);
      finally
        ABuilder.Free;
      end;
    finally
      FParserState.Free;
    end;
  end;
end;

{ TdxPDFTextUtils }

class function TdxPDFTextUtils.ConvertToString(const ARanges
  : TdxPDFPageTextRanges; APages: TdxPDFPages): string;
var
  I, ALength: Integer;
  APage: TdxPDFPage;
  ARange: TdxPDFPageTextRange;
  ATextBuilder: TdxBiDiStringBuilder;
begin
  ATextBuilder := TdxBiDiStringBuilder.Create;
  try
    ALength := Length(ARanges);
    for I := 0 to ALength - 1 do
    begin
      ARange := ARanges[I];
      APage := APages[ARange.PageIndex];
      Append(ATextBuilder, ARange.StartPosition, ARange.EndPosition,
        APage.RecognizedContent.TextLines);
      if (I <> ALength - 1) and (ARange.PageIndex = (ARanges[I + 1]).PageIndex)
        and not ATextBuilder.Empty and not ATextBuilder.EndsWithNewLine then
        ATextBuilder.AppendLine;
    end;
    Result := ATextBuilder.EndCurrentLineAndGetString;
  finally
    ATextBuilder.Free;
  end;
end;

{ TdxPDFFileAttachmentAnnotationField }

function TdxPDFFileAttachmentAnnotationField.GetAttachment
  : TdxPDFFileAttachment;
begin
  Result := (FAnnotation as TdxPDFFileAttachmentAnnotation)
    .FileSpecification.Attachment;
end;

function TdxPDFFileAttachmentAnnotationField.GetCursor: TCursor;
begin
  Result := crdxPDFViewerContext;
end;

function TdxPDFFileAttachmentAnnotationField.GetHitCode: Integer;
begin
  Result := hcAttachment;
end;

function TdxPDFFileAttachmentAnnotationField.IsResetFocusingNeeded: Boolean;
begin
  Result := False;
end;

initialization

dxPDFRegisterDocumentObjectClass(TdxPDFCatalog);
dxPDFRegisterDocumentObjectClass(TdxPDFColorSpaces);
dxPDFRegisterDocumentObjectClass(TdxPDFDocumentImage);
dxPDFRegisterDocumentObjectClass(TdxPDFForm);
dxPDFRegisterDocumentObjectClass(TdxPDFDocumentInformation);
dxPDFRegisterDocumentObjectClass(TdxPDFFonts);
dxPDFRegisterDocumentObjectClass(TdxPDFGraphicsStateParameters);
dxPDFRegisterDocumentObjectClass(TdxPDFPageData);
dxPDFRegisterDocumentObjectClass(TdxPDFPageContents);
dxPDFRegisterDocumentObjectClass(TdxPDFPages);
dxPDFRegisterDocumentObjectClass(TdxPDFResources);
dxPDFRegisterDocumentObjectClass(TdxPDFXObjects);
dxPDFRegisterDocumentObjectClass(TdxPDFFileSpecification);

finalization

dxPDFUnregisterDocumentObjectClass(TdxPDFFileSpecification);
dxPDFUnregisterDocumentObjectClass(TdxPDFXObjects);
dxPDFUnregisterDocumentObjectClass(TdxPDFResources);
dxPDFUnregisterDocumentObjectClass(TdxPDFPages);
dxPDFUnregisterDocumentObjectClass(TdxPDFPageContents);
dxPDFUnregisterDocumentObjectClass(TdxPDFPageData);
dxPDFUnregisterDocumentObjectClass(TdxPDFGraphicsStateParameters);

dxPDFUnregisterDocumentObjectClass(TdxPDFFonts);
dxPDFUnregisterDocumentObjectClass(TdxPDFForm);
dxPDFUnregisterDocumentObjectClass(TdxPDFDocumentImage);
dxPDFUnregisterDocumentObjectClass(TdxPDFDocumentInformation);
dxPDFUnregisterDocumentObjectClass(TdxPDFColorSpaces);
dxPDFUnregisterDocumentObjectClass(TdxPDFCatalog);

FreeAndNil(dxgPDFDocumentObjectFactory);

end.
