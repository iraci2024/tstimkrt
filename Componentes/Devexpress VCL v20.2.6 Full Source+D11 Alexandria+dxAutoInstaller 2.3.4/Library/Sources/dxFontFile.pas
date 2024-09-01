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

unit dxFontFile; // for internal use

{$I cxVer.inc}
{$IFNDEF DELPHIXE3}
{$M-}
{$ENDIF}

interface

uses
  Windows, Classes, SysUtils, Types, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, cxGeometry,
  cxVariants, dxPDFBase;

type
  TdxFontFile = class;

  TdxFontFileEncodingID = (Undefined = 0, UGL = 1, Big5 = 3);
  TdxFontFileLanguageID = (English, EnglishUnitedStates = $0409);
  TdxFontFileNameID = (Copyright = 0, FontFamily = 1, FontSubfamily = 2,
    UniqueFontId = 3, FullFontName = 4, Version = 5, PostscriptName = 6,
    Trademark = 7);
  TdxFontFilePlatformID = (AppleUnicode, Macintosh, ISO, Microsoft);

  TdxFontFileCMapFormatID = (ByteEncoding = 0, HighByteMappingThrough = 2,
    SegmentMapping = 4, TrimmedMapping = 6, MixedCoverage = 8,
    TrimmedArray = 10, SegmentedCoverage = 12, ManyToOneRangeMapping = 13,
    UnicodeVariationSequences = 14);

  TdxFontFileHeadTableFlags = (BaselineForFontAt0 = $0001,
    LeftSideBearingPointAt0 = $0002, InstructionsMayDependOnPointSize = $0004,
    ForcePPEMToIntegerValues = $0008, InstructionsMayAlterAdvanceWidth = $0008,
    FontDataIsLossless = $0800, ProduceCompatibleMetrics = $1000,
    OptimizedForClearType = $2000, LastResort = $4000);

  TdxFontFileHeadTableMacStyle = (emsBold = $01, emsItalic = $02,
    Underline = $04, Outline = $08, Shadow = $10, Condensed = $20,
    Extended = $40);

  TdxFontFileDirectionHint = (FullyMixedDirectionalGlyphs = 0,
    OnlyStronglyLeftToRight = 1,
    OnlyStronglyLeftToRightButAlsoContainsNeutrals = 2,
    OnlyStronglyRightToLeft = -1,
    OnlyStronglyRightToLeftButAlsoContainsNeutrals = -2);

  TdxFontFileIndexToLocFormat = (Short, Long);

  TdxFontFileVersion = (TrueType_1_5, TrueType_1_66, OpenType_1_2, OpenType_1_4,
    OpenType_1_5);

  TdxFontFileOS2EmbeddingType = (InstallableEmbedding = 0,
    RestrictedLicense = $002, PreviewPrintEmbedding = $004,
    EditableEmbedding = $008, NoSubSetting = $100, BitmapEmbeddingOnly = $200);
  TdxFontFileOS2WidthClass = (wcUltraCondensed, wcExtraCondensed, wcCondensed,
    wcSemiCondensed, wcMedium, wcSemiExpanded, wcExpanded, wcExtraExpanded,
    wcUltraExpanded);

  TdxFontFileFlags = (ffNone = $00000, ffFixedPitch = $00001, ffSerif = $00002,
    ffSymbolic = $00004, ffScript = $00008, ffNonSymbolic = $00020,
    ffItalic = $00040, ffAllCap = $10000, ffSmallCap = $20000,
    ffForceBold = $40000);
  TdxFontFileStretch = (fsUltraCondensed, fsExtraCondensed, fsCondensed,
    fsSemiCondensed, fsNormal, fsSemiExpanded, fsExpanded, fsExtraExpanded,
    fsUltraExpanded);

  TdxFontFileOS2FamilyClass = (ffcNoClassification = $000,
    ffcOldStyleSerifNoClassification = $100,
    ffcOldStyleSerifIBMRoundedLegibility = $101, ffcOldStyleSerifGaralde = $102,
    ffcOldStyleSerifVenetian = $103, ffcOldStyleSerifModifiedVenetian = $104,
    ffcOldStyleSerifDutchModern = $105, ffcOldStyleSerifDutchTraditional = $106,
    ffcOldStyleSerifContemporary = $107, ffcOldStyleSerifCalligraphic = $108,
    ffcOldStyleSerifMiscellaneous = $10F,
    ffcTransitionalSerifNoClassification = $200,
    ffcTransitionalSerifDirectLine = $201, ffcTransitionalSerifScript = $202,
    ffcTransitionalSerifMiscellaneous = $20F,
    ffcModernSerifNoClassification = $300, ffcModernSerifItalian = $301,
    ffcModernSerifScript = $302, ffcModernSerifMiscellaneous = $30F,
    ffcClarendonSerifNoClassification = $400, ffcClarendonSerifClarendon = $401,
    ffcClarendonSerifModern = $402, ffcClarendonSerifTraditional = $403,
    ffcClarendonSerifNewspaper = $404, ffcClarendonSerifStubSerif = $405,
    ffcClarendonSerifMonotone = $406, ffcClarendonSerifTypewriter = $407,
    ClarendonSerifMiscellaneous = $40F, ffcSlabSerifNoClassification = $500,
    ffcSlabSerifMonotone = $501, ffcSlabSerifHumanist = $502,
    ffcSlabSerifGeometric = $503, ffcSlabSerifSwiss = $504,
    ffcSlabSerifTypewriter = $505, ffcSlabSerifMiscellaneous = $50F,
    ffcFreeFormSerifNoClassification = $700, ffcFreeFormSerifModern = $701,
    ffcFreeFormSerifMiscellaneous = $70F, ffcSansSerifNoClassification = $800,
    ffcSansSerifIBMNewGrotesqueGothic = $801, ffcSansSerifHumanist = $802,
    ffcSansSerifLowXRoundGeometric = $803,
    ffcSansSerifHighXRoundGeometric = $804,
    ffcSansSerifNeoGrotesqueGothic = $805,
    ffcSansSerifModifiedNeoGrotesqueGothic = $806,
    ffcSansSerifTypewriterGothic = $809, ffcSansSerifMatrix = $80A,
    ffcSansSerifMiscellaneous = $80F, ffcOrnamentalNoClassification = $900,
    ffcOrnamentalEngraver = $901, ffcOrnamentalBlackLetter = $902,
    OrnamentalDecorative = $903, OrnamentalThreeDimensional = $904,
    ffcOrnamentalMiscellaneous = $90F, ffcScriptNoClassification = $A00,
    ffcScriptUncial = $A01, ffcScriptBrushJoined = $A02,
    ffcScriptFormalJoined = $A03, ffcScriptMonotoneJoined = $A04,
    ffcScriptCalligraphic = $A05, ScriptBrushUnjoined = $A06,
    ffcScriptFormalUnjoined = $A07, ffcScriptMonotoneUnjoined = $A08,
    ffcScriptMiscellaneous = $A0F, ffcSymbolicNoClassification = $C00,
    ffcSymbolicMixedSerif = $C03, ffcSymbolicOldstyleSerif = $C06,
    ffcSymbolicNeoGrotesqueSansSerif = $C07, ffcSymbolicMiscellaneous = $C0F);

  TdxFontFileUnicodeRange1 = (BasicLatin = $00000001,
    Latin1Supplement = $00000002, LatinExtendedA = $00000004,
    LatinExtendedB = $00000008, IPAExtensions = $00000010,
    SpacingModifiersLetters = $00000020, CombiningDiacriticalMarks = $00000040,
    GreekAndCoptic = $00000080, Coptic = $00000100, Cyrillic = $00000200,
    Armenian = $00000400, Hebrew = $00000800, Vai = $00001000,
    Arabic = $00002000, NKo = $00004000, Devanagari = $00008000,
    Bengali = $00010000, Gurmukhi = $00020000, Gujarati = $00040000,
    Oriya = $00080000, Tamil = $00100000, Telugu = $00200000,
    Kannada = $00400000, Malayalam = $00800000, Thai = $01000000,
    Lao = $02000000, Georgian = $04000000, Balinese = $08000000,
    HangulJamo = $10000000, LatinExtendedAdditional = $20000000,
    GreekExtended = $40000000, GeneralPunctuation = $7FFFFFFF);

  TdxFontFileUnicodeRange2 = (SuperscriptsAndSubscripts = $00000001,
    CurrencySymbols = $00000002,
    CombiningDiacriticalMarksForSymbols = $00000004,
    LetterlikeSymbols = $00000008, NumberForms = $00000010, Arrows = $00000020,
    MathematicalOperators = $00000040, MiscellaneousTechnical = $00000080,
    ControlPictures = $00000100, OpticalCharacterRecognition = $00000200,
    EnclosedAlphanumerics = $00000400, BoxDrawing = $00000800,
    BlockElements = $00001000, GeometricShapes = $00002000,
    MiscellaneousSymbols = $00004000, Dingbats = $00008000,
    CJKSymbolsAndPunctuation = $00010000, Hiragana = $00020000,
    Katakana = $00040000, Bopomofo = $00080000,
    HangulCompatibilityJamo = $00100000, PhagsPa = $00200000,
    EnclosedCJKLettersAndMonths = $00400000, CJKCompatibility = $00800000,
    HangulSyllables = $01000000, NonPlane0 = $02000000, Phoenician = $04000000,
    CJKUnifiedIdeographs = $08000000, PrivateUseAreaPlane0 = $10000000,
    CJKStrokes = $20000000, AlphabeticPresentationForms = $40000000,
    ArabicPresentationFormsA = $7FFFFFFF);

  TdxFontFileUnicodeRange3 = (CombiningHalfMarks = $00000001,
    VerticalForms = $00000002, SmallFormsVariants = $00000004,
    ArabicPresentationFormsB = $00000008,
    HalfWidthAndFullWidthForms = $00000010, Specials = $00000020,
    Tibetan = $00000040, Syriac = $00000080, Thaana = $00000100,
    Sinhala = $00000200, Myanmar = $00000400, Ethiopic = $00000800,
    Cherokee = $00001000, UnifiedCanadianAboriginalSyllabics = $00002000,
    Ogham = $00004000, Runic = $00008000, Khmer = $00010000,
    Mongolian = $00020000, BraillePatterns = $00040000, YiSyllables = $00080000,
    Tagalog = $00100000, OldItalic = $00200000, Gothic = $00400000,
    Deseret = $00800000, MusicalSymbols = $01000000,
    MathematicalAlphanumericSymbols = $02000000,
    PrivateUsePlane15_16 = $04000000, VariationSelectors = $08000000,
    Tags = $10000000, Limbu = $20000000, TaiLe = $40000000,
    NewTaiLe = $7FFFFFFF);

  TdxFontFileUnicodeRange4 = (Buginese = $00000001, Glagolitic = $00000002,
    Tifinagh = $00000004, YijingHexagramSymbols = $00000008,
    SylotiNagri = $00000010, LinearBSyllabary = $00000020,
    AncientGreekNumbers = $00000040, Ugaritic = $00000080,
    OldPersian = $00000100, Shavian = $00000200, Osmanya = $00000400,
    CypriotSyllabary = $00000800, Kharoshthi = $00001000,
    TaiXuanJingSymbols = $00002000, Cuneiform = $00004000,
    CountingRodNumerals = $00008000, Sundanese = $00010000, Lepcha = $00020000,
    OlChiki = $00040000, Saurashtra = $00080000, KayahLi = $00100000,
    Rejang = $00200000, Cham = $00400000, AncientSymbols = $00800000,
    PhaistosDisc = $01000000, Carian = $02000000, DominoTiles = $04000000);

  TdxFontFileSelection = (Empty = $000, ITALIC = $001, UNDERSCORE = $002,
    NEGATIVE = $004, OUTLINED = $008, STRIKEOUT = $010, BOLD = $020,
    REGULAR = $040, USE_TYPO_METRICS = $080, WWS = $100, OBLIQUE = $200);

  TdxFontFileCodePageRange1 = (Latin1 = $00000001,
    Latin2EasternEurope = $00000002, r1Cyrillic = $00000004, Greek = $00000008,
    Turkish = $00000010, r1Hebrew = $00000020, r1Arabic = $00000040,
    WindowsBaltic = $00000080, Vietnamese = $00000100, r1Thai = $00010000,
    JISJapan = $00020000, ChineseSimplified = $00040000,
    KoreanWansung = $00080000, ChineseTraditional = $00100000,
    KoreanJohab = $00200000, MacintoshCharacterSet = $20000000,
    OEMCharacterSet = $40000000, SymbolCharacterSet = $7FFFFFFF);

  TdxFontFileCodePageRange2 = (IBMGreek = $00010000, MSDOSRussian = $00020000,
    MSDOSNordic = $00040000, r2Arabic = $00080000,
    MSDOSCanadianFrench = $00100000, r2Hebrew = $00200000,
    MSDOSIcelandic = $00400000, MSDOSPortuguese = $00800000,
    IBMTurkish = $01000000, IBMCyrillic = $02000000, Latin2 = $04000000,
    MSDOSBaltic = $08000000, GreekFormer437G = $10000000,
    ArabicASMO708 = $20000000, WELatin1 = $40000000, US = $7FFFFFFF);

  TdxFontFilePanoseFamilyKind = (fkAny = 0, fkNoFit = 1, fkLatinText = 2,
    fkLatinHandWritten = 3, fkLatinDecorative = 4, fkLatinSymbol = 5);
  TdxFontFilePanoseSerifStyle = (ssAny = 0, ssNoFit = 1, ssCove = 2,
    ssObtuseCove = 3, ssSquareCove = 4, ssObtuseSquareCove = 5, ssSquare = 6,
    ssThin = 7, ssOval = 8, ssExaggerated = 9, ssTriangle = 10,
    ssNormalSans = 11, ssObtuseSans = 12, ssPerpendicularSans = 13,
    ssFlared = 14, ssRounded = 15);
  TdxFontFilePanoseWeight = (wAny = 0, wNoFit = 1, wVeryLight = 2, wLight = 3,
    wThin = 4, wBook = 5, wMedium = 6, wDemi = 7, wBold = 8, wHeavy = 9,
    wBlack = 10, wExtraBlack = 11);
  TdxFontFilePanoseProportion = (pAny = 0, pNoFit = 1, pOldStyle = 2,
    pModern = 3, pEvenWidth = 4, pExtended = 5, pCondensed = 6,
    pVeryExtended = 7, pVeryCondensed = 8, pMonospaced = 9);
  TdxFontFilePanoseContrast = (cAny = 0, cNoFit = 1, cNone = 2, cVeryLow = 3,
    cLow = 4, cMediumLow = 5, cMedium = 6, cMediumHigh = 7, cHigh = 8,
    cVeryHigh = 9);
  TdxFontFilePanoseStrokeVariation = (svAny = 0, svNoFit = 1, svNoVariation = 2,
    svGradualDiagonal = 3, svGradualTransitional = 4, svGradualVertical = 5,
    svGradualHorizontal = 6, svRapidVertical = 7, svRapidHorizontal = 8,
    svInstantVertical = 9, svInstantHorizontal = 10);
  TdxFontFilePanoseArmStyle = (asAny = 0, asNoFit = 1,
    asStraightArmsHorizontal = 2, asStraightArmsWedge = 3,
    asStraightArmsVertical = 4, asStraightArmsSingleSerif = 5,
    asStraightArmsDoubleSerif = 6, asNonStraightHorizontal = 7,
    asNonStraightWedge = 8, asNonStraightVertical = 9,
    asNonStraightSingleSerif = 10, asNonStraightDoubleSerif = 11);
  TdxFontFilePanoseLetterForm = (lfAny = 0, lfNoFit = 1, lfNormalContact = 2,
    lfNormalWeighted = 3, lfNormalBoxed = 4, lfNormalFlattened = 5,
    lfNormalRounded = 6, lfNormalOffCenter = 7, lfNormalSquare = 8,
    lfObliqueContact = 9, lfObliqueWeighted = 10, lfObliqueBoxed = 11,
    lfObliqueFlattened = 12, lfObliqueRounded = 13, lfObliqueOffCenter = 14,
    lfObliqueSquare = 15);
  TdxFontFilePanoseMidline = (mAny = 0, mNoFit = 1, mStandardTrimmer = 2,
    mStandardPointed = 3, mStandardSerifed = 4, mHighTrimmed = 5,
    mHighPointed = 6, mHighSerifed = 7, mConstantTrimmed = 8,
    mConstantPointed = 9, mConstantSerifed = 10, mLowTrimmed = 11,
    mLowPointed = 12, mLowSerifed = 13);
  TdxFontFilePanoseXHeight = (xhAny = 0, xhNoFit = 1, xhConstantSmall = 2,
    xhConstantStandard = 3, xhConstantLarge = 4, xhDuckingSmall = 5,
    xhDuckingStandard = 6, xhDuckingLarge = 7);

  TdxFontFileStream = class(TdxPDFMemoryStream);

  TdxFontFileCMapGlyphRange = record
    EndValue: SmallInt;
    StartValue: SmallInt;
  end;

  TdxFontFileCMapRow = record
    EndCode: SmallInt;
    StartCode: SmallInt;
    IdDelta: SmallInt;
    IdRangeOffset: SmallInt;
  end;

  { TdxFontFileGlyphDescription }

  TdxFontFileGlyphDescription = record
  strict private
  const
    ARG_1_AND_2_ARE_WORDS = 1;
    MORE_COMPONENTS = 32;
    WE_HAVE_A_SCALE = 8;
    WE_HAVE_A_TWO_BY_TWO = 128;
    WE_HAVE_AN_X_AND_Y_SCALE = 64;
  strict private
    FBoundingBoxParsed: Boolean;
    FXMin: SmallInt;
    FYMin: SmallInt;
    FXMax: SmallInt;
    FYMax: SmallInt;
    function GetSize: Integer;
    function GetXMin: SmallInt;
    function GetYMin: SmallInt;
    function GetXMax: SmallInt;
    function GetYMax: SmallInt;
    procedure ParseBoundingBox;
  public const
    HeaderSize = 10;
  public
    Data: TBytes;
    GlyphIndexList: TIntegerDynArray;
    NumberOfContours: SmallInt;

    class function Create(AStream: TdxFontFileStream; AGlyphDataSize: Integer)
      : TdxFontFileGlyphDescription; static;

    function IsEmpty: Boolean;
    procedure Write(AStream: TdxFontFileStream);
    property Size: Integer read GetSize;
    property XMin: SmallInt read GetXMin;
    property YMin: SmallInt read GetYMin;
    property XMax: SmallInt read GetXMax;
    property YMax: SmallInt read GetYMax;
  end;

  TdxFontFileSubsetGlyph = record
    Index: Integer;
    Description: TdxFontFileGlyphDescription;
  end;

  TdxFontFilePanose = record
  public
    ArmStyle: TdxFontFilePanoseArmStyle;
    Contrast: TdxFontFilePanoseContrast;
    FamilyKind: TdxFontFilePanoseFamilyKind;
    LetterForm: TdxFontFilePanoseLetterForm;
    Midline: TdxFontFilePanoseMidline;
    Proportion: TdxFontFilePanoseProportion;
    SerifStyle: TdxFontFilePanoseSerifStyle;
    StrokeVariation: TdxFontFilePanoseStrokeVariation;
    Weight: TdxFontFilePanoseWeight;
    XHeight: TdxFontFilePanoseXHeight;

    class function Create(AStream: TdxFontFileStream)
      : TdxFontFilePanose; static;
    function IsDefault: Boolean;
    procedure Write(AStream: TdxFontFileStream);
  end;

  { TdxType1FontGlyphMapping }

  TdxType1FontGlyphMapping = class
  strict private
    FEncoding: TdxPDFByteStringDictionary;
    FMapping: TdxPDFStringSmallIntegerDictionary;
  public
    constructor Create; overload;
    constructor Create(AMapping: TdxPDFStringSmallIntegerDictionary;
      AEncoding: TdxPDFByteStringDictionary); overload;
    destructor Destroy; override;

    property Encoding: TdxPDFByteStringDictionary read FEncoding;
    property Mapping: TdxPDFStringSmallIntegerDictionary read FMapping;
  end;

  { TdxGlyphNames }

  TdxGlyphNames = class
  public const
{$REGION 'public const'}
    _notdef = '.notdef';
    _001_000 = '_001_000';
    _001_001 = '_001_001';
    _001_002 = '_001_002';
    _001_003 = '_001_003';
    A = 'A';
    AE = 'AE';
    AEacute = 'AEacute';
    AEsmall = 'AEsmall';
    Aacute = 'Aacute';
    Aacutesmall = 'Aacutesmall';
    Abreve = 'Abreve';
    Acircumflex = 'Acircumflex';
    Acircumflexsmall = 'Acircumflexsmall';
    Acutesmall = 'Acutesmall';
    Adieresis = 'Adieresis';
    Adieresissmall = 'Adieresissmall';
    Agrave = 'Agrave';
    Agravesmall = 'Agravesmall';
    Alpha = 'Alpha';
    Alphatonos = 'Alphatonos';
    Amacron = 'Amacron';
    Aogonek = 'Aogonek';
    Aring = 'Aring';
    Aringacute = 'Aringacute';
    Aringsmall = 'Aringsmall';
    Asmall = 'Asmall';
    Atilde = 'Atilde';
    Atildesmall = 'Atildesmall';
    B = 'B';
    Beta = 'Beta';
    Black = 'Black';
    BOLD = 'Bold';
    Book = 'Book';
    Brevesmall = 'Brevesmall';
    Bsmall = 'Bsmall';
    C = 'C';
    Cacute = 'Cacute';
    Caronsmall = 'Caronsmall';
    Ccaron = 'Ccaron';
    Ccedilla = 'Ccedilla';
    Ccedillasmall = 'Ccedillasmall';
    Ccircumflex = 'Ccircumflex';
    Cdot = 'Cdot';
    Cedillasmall = 'Cedillasmall';
    Chi = 'Chi';
    Circumflexsmall = 'Circumflexsmall';
    Csmall = 'Csmall';
    D = 'D';
    Dcaron = 'Dcaron';
    Dcroat = 'Dcroat';
    Delta = 'Delta';
    Dieresissmall = 'Dieresissmall';
    Dotaccentsmall = 'Dotaccentsmall';
    Dsmall = 'Dsmall';
    E = 'E';
    Eacute = 'Eacute';
    Eacutesmall = 'Eacutesmall';
    Ebreve = 'Ebreve';
    Ecaron = 'Ecaron';
    Ecircumflex = 'Ecircumflex';
    Ecircumflexsmall = 'Ecircumflexsmall';
    Edieresis = 'Edieresis';
    Edieresissmall = 'Edieresissmall';
    Edot = 'Edot';
    Edotaccent = 'Edotaccent';
    Egrave = 'Egrave';
    Egravesmall = 'Egravesmall';
    Emacron = 'Emacron';
    Eng = 'Eng';
    Eogonek = 'Eogonek';
    Epsilon = 'Epsilon';
    Epsilontonos = 'Epsilontonos';
    Esmall = 'Esmall';
    Eta = 'Eta';
    Etatonos = 'Etatonos';
    Eth = 'Eth';
    Ethsmall = 'Ethsmall';
    Euro = 'Euro';
    F = 'F';
    Fsmall = 'Fsmall';
    G = 'G';
    Gamma = 'Gamma';
    Gbreve = 'Gbreve';
    Gcedilla = 'Gcedilla';
    Gcircumflex = 'Gcircumflex';
    Gcommaaccent = 'Gcommaaccent';
    Gdot = 'Gdot';
    Gravesmall = 'Gravesmall';
    Gsmall = 'Gsmall';
    H = 'H';
    H18533 = 'H18533';
    H18543 = 'H18543';
    H18551 = 'H18551';
    H22073 = 'H22073';
    Hbar = 'Hbar';
    Hcircumflex = 'Hcircumflex';
    Hsmall = 'Hsmall';
    Hungarumlautsmall = 'Hungarumlautsmall';
    I = 'I';
    IJ = 'IJ';
    Iacute = 'Iacute';
    Iacutesmall = 'Iacutesmall';
    Ibreve = 'Ibreve';
    Icircumflex = 'Icircumflex';
    Icircumflexsmall = 'Icircumflexsmall';
    Idieresis = 'Idieresis';
    Idieresissmall = 'Idieresissmall';
    Idot = 'Idot';
    Idotaccent = 'Idotaccent';
    Ifraktur = 'Ifraktur';
    Igrave = 'Igrave';
    Igravesmall = 'Igravesmall';
    Imacron = 'Imacron';
    Iogonek = 'Iogonek';
    Iota = 'Iota';
    Iotadieresis = 'Iotadieresis';
    Iotatonos = 'Iotatonos';
    Ismall = 'Ismall';
    Itilde = 'Itilde';
    J = 'J';
    Jcircumflex = 'Jcircumflex';
    Jsmall = 'Jsmall';
    K = 'K';
    Kappa = 'Kappa';
    Kcedilla = 'Kcedilla';
    Kcommaaccent = 'Kcommaaccent';
    Ksmall = 'Ksmall';
    L = 'L';
    Lacute = 'Lacute';
    Lambda = 'Lambda';
    Lcaron = 'Lcaron';
    Lcedilla = 'Lcedilla';
    Lcommaaccent = 'Lcommaaccent';
    Ldot = 'Ldot';
    Light = 'Light';
    Lslash = 'Lslash';
    Lslashsmall = 'Lslashsmall';
    Lsmall = 'Lsmall';
    M = 'M';
    Macronsmall = 'Macronsmall';
    Medium = 'Medium';
    Msmall = 'Msmall';
    Mu = 'Mu';
    N = 'N';
    Nacute = 'Nacute';
    Ncaron = 'Ncaron';
    Ncedilla = 'Ncedilla';
    Ncommaaccent = 'Ncommaaccent';
    Nsmall = 'Nsmall';
    Ntilde = 'Ntilde';
    Ntildesmall = 'Ntildesmall';
    Nu = 'Nu';
    O = 'O';
    OE = 'OE';
    OEsmall = 'OEsmall';
    Oacute = 'Oacute';
    Oacutesmall = 'Oacutesmall';
    Obreve = 'Obreve';
    Ocircumflex = 'Ocircumflex';
    Ocircumflexsmall = 'Ocircumflexsmall';
    Odblacute = 'Odblacute';
    Odieresis = 'Odieresis';
    Odieresissmall = 'Odieresissmall';
    Ogoneksmall = 'Ogoneksmall';
    Ograve = 'Ograve';
    Ogravesmall = 'Ogravesmall';
    Ohm = 'Ohm';
    Ohungarumlaut = 'Ohungarumlaut';
    Omacron = 'Omacron';
    Omega = 'Omega';
    Omegatonos = 'Omegatonos';
    Omicron = 'Omicron';
    Omicrontonos = 'Omicrontonos';
    Oslash = 'Oslash';
    Oslashacute = 'Oslashacute';
    Oslashsmall = 'Oslashsmall';
    Osmall = 'Osmall';
    Otilde = 'Otilde';
    Otildesmall = 'Otildesmall';
    P = 'P';
    Phi = 'Phi';
    Pi = 'Pi';
    Psi = 'Psi';
    Psmall = 'Psmall';
    Q = 'Q';
    Qsmall = 'Qsmall';
    R = 'R';
    Racute = 'Racute';
    Rcaron = 'Rcaron';
    Rcedilla = 'Rcedilla';
    Rcommaaccent = 'Rcommaaccent';
    REGULAR = 'Regular';
    Rfraktur = 'Rfraktur';
    Rho = 'Rho';
    Ringsmall = 'Ringsmall';
    Roman = 'Roman';
    Rsmall = 'Rsmall';
    S = 'S';
    SF010000 = 'SF010000';
    SF020000 = 'SF020000';
    SF030000 = 'SF030000';
    SF040000 = 'SF040000';
    SF050000 = 'SF050000';
    SF060000 = 'SF060000';
    SF070000 = 'SF070000';
    SF080000 = 'SF080000';
    SF090000 = 'SF090000';
    SF100000 = 'SF100000';
    SF110000 = 'SF110000';
    SF190000 = 'SF190000';
    SF200000 = 'SF200000';
    SF210000 = 'SF210000';
    SF220000 = 'SF220000';
    SF230000 = 'SF230000';
    SF240000 = 'SF240000';
    SF250000 = 'SF250000';
    SF260000 = 'SF260000';
    SF270000 = 'SF270000';
    SF280000 = 'SF280000';
    SF360000 = 'SF360000';
    SF370000 = 'SF370000';
    SF380000 = 'SF380000';
    SF390000 = 'SF390000';
    SF400000 = 'SF400000';
    SF410000 = 'SF410000';
    SF420000 = 'SF420000';
    SF430000 = 'SF430000';
    SF440000 = 'SF440000';
    SF450000 = 'SF450000';
    SF460000 = 'SF460000';
    SF470000 = 'SF470000';
    SF480000 = 'SF480000';
    SF490000 = 'SF490000';
    SF500000 = 'SF500000';
    SF510000 = 'SF510000';
    SF520000 = 'SF520000';
    SF530000 = 'SF530000';
    SF540000 = 'SF540000';
    Sacute = 'Sacute';
    Scaron = 'Scaron';
    Scaronsmall = 'Scaronsmall';
    Scedilla = 'Scedilla';
    Scircumflex = 'Scircumflex';
    Scommaaccent = 'Scommaaccent';
    Semibold = 'Semibold';
    Sigma = 'Sigma';
    Ssmall = 'Ssmall';
    Space = ' ';
    T = 'T';
    Tau = 'Tau';
    Tbar = 'Tbar';
    Tcaron = 'Tcaron';
    Tcedilla = 'Tcedilla';
    Tcommaaccent = 'Tcommaaccent';
    Theta = 'Theta';
    Thorn = 'Thorn';
    Thornsmall = 'Thornsmall';
    Tildesmall = 'Tildesmall';
    Tsmall = 'Tsmall';
    U = 'U';
    Uacute = 'Uacute';
    Uacutesmall = 'Uacutesmall';
    Ubreve = 'Ubreve';
    Ucircumflex = 'Ucircumflex';
    Ucircumflexsmall = 'Ucircumflexsmall';
    Udblacute = 'Udblacute';
    Udieresis = 'Udieresis';
    Udieresissmall = 'Udieresissmall';
    Ugrave = 'Ugrave';
    Ugravesmall = 'Ugravesmall';
    Uhungarumlaut = 'Uhungarumlaut';
    Umacron = 'Umacron';
    Uogonek = 'Uogonek';
    Upsilon = 'Upsilon';
    Upsilon1 = 'Upsilon1';
    Upsilondieresis = 'Upsilondieresis';
    Upsilontonos = 'Upsilontonos';
    Uring = 'Uring';
    Usmall = 'Usmall';
    Utilde = 'Utilde';
    V = 'V';
    Vsmall = 'Vsmall';
    W = 'W';
    Wacute = 'Wacute';
    Wcircumflex = 'Wcircumflex';
    Wdieresis = 'Wdieresis';
    Wgrave = 'Wgrave';
    Wsmall = 'Wsmall';
    X = 'X';
    Xi = 'Xi';
    Xsmall = 'Xsmall';
    Y = 'Y';
    Yacute = 'Yacute';
    Yacutesmall = 'Yacutesmall';
    Ycircumflex = 'Ycircumflex';
    Ydieresis = 'Ydieresis';
    Ydieresissmall = 'Ydieresissmall';
    Ygrave = 'Ygrave';
    Ysmall = 'Ysmall';
    Z = 'Z';
    Zacute = 'Zacute';
    Zcaron = 'Zcaron';
    Zcaronsmall = 'Zcaronsmall';
    Zdot = 'Zdot';
    Zdotaccent = 'Zdotaccent';
    Zeta = 'Zeta';
    Zsmall = 'Zsmall';
    LowerA = 'a';
    LowerA1 = 'a1';
    LowerA10 = 'a10';
    LowerA100 = 'a100';
    LowerA101 = 'a101';
    LowerA102 = 'a102';
    LowerA103 = 'a103';
    LowerA104 = 'a104';
    LowerA105 = 'a105';
    LowerA106 = 'a106';
    LowerA107 = 'a107';
    LowerA108 = 'a108';
    LowerA109 = 'a109';
    LowerA11 = 'a11';
    LowerA110 = 'a110';
    LowerA111 = 'a111';
    LowerA112 = 'a112';
    LowerA117 = 'a117';
    LowerA118 = 'a118';
    LowerA119 = 'a119';
    LowerA12 = 'a12';
    LowerA120 = 'a120';
    LowerA121 = 'a121';
    LowerA122 = 'a122';
    LowerA123 = 'a123';
    LowerA124 = 'a124';
    LowerA125 = 'a125';
    LowerA126 = 'a126';
    LowerA127 = 'a127';
    LowerA128 = 'a128';
    LowerA129 = 'a129';
    LowerA13 = 'a13';
    LowerA130 = 'a130';
    LowerA131 = 'a131';
    LowerA132 = 'a132';
    LowerA133 = 'a133';
    LowerA134 = 'a134';
    LowerA135 = 'a135';
    LowerA136 = 'a136';
    LowerA137 = 'a137';
    LowerA138 = 'a138';
    LowerA139 = 'a139';
    LowerA14 = 'a14';
    LowerA140 = 'a140';
    LowerA141 = 'a141';
    LowerA142 = 'a142';
    LowerA143 = 'a143';
    LowerA144 = 'a144';
    LowerA145 = 'a145';
    LowerA146 = 'a146';
    LowerA147 = 'a147';
    LowerA148 = 'a148';
    LowerA149 = 'a149';
    LowerA15 = 'a15';
    LowerA150 = 'a150';
    LowerA151 = 'a151';
    LowerA152 = 'a152';
    LowerA153 = 'a153';
    LowerA154 = 'a154';
    LowerA155 = 'a155';
    LowerA156 = 'a156';
    LowerA157 = 'a157';
    LowerA158 = 'a158';
    LowerA159 = 'a159';
    LowerA16 = 'a16';
    LowerA160 = 'a160';
    LowerA161 = 'a161';
    LowerA162 = 'a162';
    LowerA163 = 'a163';
    LowerA164 = 'a164';
    LowerA165 = 'a165';
    LowerA166 = 'a166';
    LowerA167 = 'a167';
    LowerA168 = 'a168';
    LowerA169 = 'a169';
    LowerA17 = 'a17';
    LowerA170 = 'a170';
    LowerA171 = 'a171';
    LowerA172 = 'a172';
    LowerA173 = 'a173';
    LowerA174 = 'a174';
    LowerA175 = 'a175';
    LowerA176 = 'a176';
    LowerA177 = 'a177';
    LowerA178 = 'a178';
    LowerA179 = 'a179';
    LowerA18 = 'a18';
    LowerA180 = 'a180';
    LowerA181 = 'a181';
    LowerA182 = 'a182';
    LowerA183 = 'a183';
    LowerA184 = 'a184';
    LowerA185 = 'a185';
    LowerA186 = 'a186';
    LowerA187 = 'a187';
    LowerA188 = 'a188';
    LowerA189 = 'a189';
    LowerA19 = 'a19';
    LowerA190 = 'a190';
    LowerA191 = 'a191';
    LowerA192 = 'a192';
    LowerA193 = 'a193';
    LowerA194 = 'a194';
    LowerA195 = 'a195';
    LowerA196 = 'a196';
    LowerA197 = 'a197';
    LowerA198 = 'a198';
    LowerA199 = 'a199';
    LowerA2 = 'a2';
    LowerA20 = 'a20';
    LowerA200 = 'a200';
    LowerA201 = 'a201';
    LowerA202 = 'a202';
    LowerA203 = 'a203';
    LowerA204 = 'a204';
    LowerA205 = 'a205';
    LowerA206 = 'a206';
    LowerA21 = 'a21';
    LowerA22 = 'a22';
    LowerA23 = 'a23';
    LowerA24 = 'a24';
    LowerA25 = 'a25';
    LowerA26 = 'a26';
    LowerA27 = 'a27';
    LowerA28 = 'a28';
    LowerA29 = 'a29';
    LowerA3 = 'a3';
    LowerA30 = 'a30';
    LowerA31 = 'a31';
    LowerA32 = 'a32';
    LowerA33 = 'a33';
    LowerA34 = 'a34';
    LowerA35 = 'a35';
    LowerA36 = 'a36';
    LowerA37 = 'a37';
    LowerA38 = 'a38';
    LowerA39 = 'a39';
    LowerA4 = 'a4';
    LowerA40 = 'a40';
    LowerA41 = 'a41';
    LowerA42 = 'a42';
    LowerA43 = 'a43';
    LowerA44 = 'a44';
    LowerA45 = 'a45';
    LowerA46 = 'a46';
    LowerA47 = 'a47';
    LowerA48 = 'a48';
    LowerA49 = 'a49';
    LowerA5 = 'a5';
    LowerA50 = 'a50';
    LowerA51 = 'a51';
    LowerA52 = 'a52';
    LowerA53 = 'a53';
    LowerA54 = 'a54';
    LowerA55 = 'a55';
    LowerA56 = 'a56';
    LowerA57 = 'a57';
    LowerA58 = 'a58';
    LowerA59 = 'a59';
    LowerA6 = 'a6';
    LowerA60 = 'a60';
    LowerA61 = 'a61';
    LowerA62 = 'a62';
    LowerA63 = 'a63';
    LowerA64 = 'a64';
    LowerA65 = 'a65';
    LowerA66 = 'a66';
    LowerA67 = 'a67';
    LowerA68 = 'a68';
    LowerA69 = 'a69';
    LowerA7 = 'a7';
    LowerA70 = 'a70';
    LowerA71 = 'a71';
    LowerA72 = 'a72';
    LowerA73 = 'a73';
    LowerA74 = 'a74';
    LowerA75 = 'a75';
    LowerA76 = 'a76';
    LowerA77 = 'a77';
    LowerA78 = 'a78';
    LowerA79 = 'a79';
    LowerA8 = 'a8';
    LowerA81 = 'a81';
    LowerA82 = 'a82';
    LowerA83 = 'a83';
    LowerA84 = 'a84';
    LowerA85 = 'a85';
    LowerA86 = 'a86';
    LowerA87 = 'a87';
    LowerA88 = 'a88';
    LowerA89 = 'a89';
    LowerA9 = 'a9';
    LowerA90 = 'a90';
    LowerA91 = 'a91';
    LowerA92 = 'a92';
    LowerA93 = 'a93';
    LowerA94 = 'a94';
    LowerA95 = 'a95';
    LowerA96 = 'a96';
    LowerA97 = 'a97';
    LowerA98 = 'a98';
    LowerA99 = 'a99';
    LowerAacute = 'aacute';
    LowerAbreve = 'abreve';
    LowerAcircumflex = 'acircumflex';
    LowerAcute = 'acute';
    LowerAdieresis = 'adieresis';
    LowerAe = 'ae';
    LowerAeacute = 'aeacute';
    LowerAfii00208 = 'afii00208';
    LowerAfii08941 = 'afii08941';
    LowerAfii10017 = 'afii10017';
    LowerAfii10018 = 'afii10018';
    LowerAfii10019 = 'afii10019';
    LowerAfii10020 = 'afii10020';
    LowerAfii10021 = 'afii10021';
    LowerAfii10022 = 'afii10022';
    LowerAfii10023 = 'afii10023';
    LowerAfii10024 = 'afii10024';
    LowerAfii10025 = 'afii10025';
    LowerAfii10026 = 'afii10026';
    LowerAfii10027 = 'afii10027';
    LowerAfii10028 = 'afii10028';
    LowerAfii10029 = 'afii10029';
    LowerAfii10030 = 'afii10030';
    LowerAfii10031 = 'afii10031';
    LowerAfii10032 = 'afii10032';
    LowerAfii10033 = 'afii10033';
    LowerAfii10034 = 'afii10034';
    LowerAfii10035 = 'afii10035';
    LowerAfii10036 = 'afii10036';
    LowerAfii10037 = 'afii10037';
    LowerAfii10038 = 'afii10038';
    LowerAfii10039 = 'afii10039';
    LowerAfii10040 = 'afii10040';
    LowerAfii10041 = 'afii10041';
    LowerAfii10042 = 'afii10042';
    LowerAfii10043 = 'afii10043';
    LowerAfii10044 = 'afii10044';
    LowerAfii10045 = 'afii10045';
    LowerAfii10046 = 'afii10046';
    LowerAfii10047 = 'afii10047';
    LowerAfii10048 = 'afii10048';
    LowerAfii10049 = 'afii10049';
    LowerAfii10050 = 'afii10050';
    LowerAfii10051 = 'afii10051';
    LowerAfii10052 = 'afii10052';
    LowerAfii10053 = 'afii10053';
    LowerAfii10054 = 'afii10054';
    LowerAfii10055 = 'afii10055';
    LowerAfii10056 = 'afii10056';
    LowerAfii10057 = 'afii10057';
    LowerAfii10058 = 'afii10058';
    LowerAfii10059 = 'afii10059';
    LowerAfii10060 = 'afii10060';
    LowerAfii10061 = 'afii10061';
    LowerAfii10062 = 'afii10062';
    LowerAfii10065 = 'afii10065';
    LowerAfii10066 = 'afii10066';
    LowerAfii10067 = 'afii10067';
    LowerAfii10068 = 'afii10068';
    LowerAfii10069 = 'afii10069';
    LowerAfii10070 = 'afii10070';
    LowerAfii10071 = 'afii10071';
    LowerAfii10072 = 'afii10072';
    LowerAfii10073 = 'afii10073';
    LowerAfii10074 = 'afii10074';
    LowerAfii10075 = 'afii10075';
    LowerAfii10076 = 'afii10076';
    LowerAfii10077 = 'afii10077';
    LowerAfii10078 = 'afii10078';
    LowerAfii10079 = 'afii10079';
    LowerAfii10080 = 'afii10080';
    LowerAfii10081 = 'afii10081';
    LowerAfii10082 = 'afii10082';
    LowerAfii10083 = 'afii10083';
    LowerAfii10084 = 'afii10084';
    LowerAfii10085 = 'afii10085';
    LowerAfii10086 = 'afii10086';
    LowerAfii10087 = 'afii10087';
    LowerAfii10088 = 'afii10088';
    LowerAfii10089 = 'afii10089';
    LowerAfii10090 = 'afii10090';
    LowerAfii10091 = 'afii10091';
    LowerAfii10092 = 'afii10092';
    LowerAfii10093 = 'afii10093';
    LowerAfii10094 = 'afii10094';
    LowerAfii10095 = 'afii10095';
    LowerAfii10096 = 'afii10096';
    LowerAfii10097 = 'afii10097';
    LowerAfii10098 = 'afii10098';
    LowerAfii10099 = 'afii10099';
    LowerAfii10100 = 'afii10100';
    LowerAfii10101 = 'afii10101';
    LowerAfii10102 = 'afii10102';
    LowerAfii10103 = 'afii10103';
    LowerAfii10104 = 'afii10104';
    LowerAfii10105 = 'afii10105';
    LowerAfii10106 = 'afii10106';
    LowerAfii10107 = 'afii10107';
    LowerAfii10108 = 'afii10108';
    LowerAfii10109 = 'afii10109';
    LowerAfii10110 = 'afii10110';
    LowerAfii10145 = 'afii10145';
    LowerAfii10193 = 'afii10193';
    LowerAfii61248 = 'afii61248';
    LowerAfii61289 = 'afii61289';
    LowerAfii61352 = 'afii61352';
    LowerAgrave = 'agrave';
    LowerAleph = 'aleph';
    LowerAlpha = 'alpha';
    LowerAlphatonos = 'alphatonos';
    LowerAmacron = 'amacron';
    LowerAmpersand = 'ampersand';
    LowerAmpersandsmall = 'ampersandsmall';
    LowerAngle = 'angle';
    LowerAngleleft = 'angleleft';
    LowerAngleright = 'angleright';
    LowerAnoteleia = 'anoteleia';
    LowerAogonek = 'aogonek';
    LowerApple = 'apple';
    LowerApplelogo = 'applelogo';
    LowerApproxequal = 'approxequal';
    LowerAring = 'aring';
    LowerAringacute = 'aringacute';
    LowerArrowboth = 'arrowboth';
    LowerArrowdblboth = 'arrowdblboth';
    LowerArrowdbldown = 'arrowdbldown';
    LowerArrowdblleft = 'arrowdblleft';
    LowerArrowdblright = 'arrowdblright';
    LowerArrowdblup = 'arrowdblup';
    LowerArrowdown = 'arrowdown';
    LowerArrowhorizex = 'arrowhorizex';
    LowerArrowleft = 'arrowleft';
    LowerArrowright = 'arrowright';
    LowerArrowup = 'arrowup';
    LowerArrowupdn = 'arrowupdn';
    LowerArrowupdnbse = 'arrowupdnbse';
    LowerArrowvertex = 'arrowvertex';
    LowerAsciicircum = 'asciicircum';
    LowerAsciitilde = 'asciitilde';
    LowerAsterisk = 'asterisk';
    LowerAsteriskmath = 'asteriskmath';
    LowerAsuperior = 'asuperior';
    LowerAt = 'at';
    LowerAtilde = 'atilde';
    LowerB = 'b';
    LowerBackslash = 'backslash';
    LowerBar = 'bar';
    LowerBeta = 'beta';
    LowerBlock = 'block';
    LowerBraceex = 'braceex';
    LowerBraceleft = 'braceleft';
    LowerBraceleftbt = 'braceleftbt';
    LowerBraceleftmid = 'braceleftmid';
    LowerBracelefttp = 'bracelefttp';
    LowerBraceright = 'braceright';
    LowerBracerightbt = 'bracerightbt';
    LowerBracerightmid = 'bracerightmid';
    LowerBracerighttp = 'bracerighttp';
    LowerBracketleft = 'bracketleft';
    LowerBracketleftbt = 'bracketleftbt';
    LowerBracketleftex = 'bracketleftex';
    LowerBracketlefttp = 'bracketlefttp';
    LowerBracketright = 'bracketright';
    LowerBracketrightbt = 'bracketrightbt';
    LowerBracketrightex = 'bracketrightex';
    LowerBracketrighttp = 'bracketrighttp';
    LowerBreve = 'breve';
    LowerBrokenbar = 'brokenbar';
    LowerBsuperior = 'bsuperior';
    LowerBullet = 'bullet';
    LowerC = 'c';
    LowerCacute = 'cacute';
    LowerCaron = 'caron';
    LowerCarriagereturn = 'carriagereturn';
    LowerCcaron = 'ccaron';
    LowerCcedilla = 'ccedilla';
    LowerCcircumflex = 'ccircumflex';
    LowerCdot = 'cdot';
    LowerCedilla = 'cedilla';
    LowerCent = 'cent';
    LowerCentinferior = 'centinferior';
    LowerCentoldstyle = 'centoldstyle';
    LowerCentsuperior = 'centsuperior';
    LowerChi = 'chi';
    LowerCircle = 'circle';
    LowerCirclemultiply = 'circlemultiply';
    LowerCircleplus = 'circleplus';
    LowerCircumflex = 'circumflex';
    LowerClub = 'club';
    LowerColon = 'colon';
    LowerColonmonetary = 'colonmonetary';
    LowerComma = 'comma';
    LowerCommaaccent = 'commaaccent';
    LowerCommainferior = 'commainferior';
    LowerCommasuperior = 'commasuperior';
    LowerCongruent = 'congruent';
    LowerCopyright = 'copyright';
    LowerCopyrightsans = 'copyrightsans';
    LowerCopyrightserif = 'copyrightserif';
    LowerCurrency = 'currency';
    LowerD = 'd';
    LowerDagger = 'dagger';
    LowerDaggerdbl = 'daggerdbl';
    LowerDcaron = 'dcaron';
    LowerDcroat = 'dcroat';
    LowerDegree = 'degree';
    LowerDelta = 'delta';
    LowerDiamond = 'diamond';
    LowerDieresis = 'dieresis';
    LowerDieresistonos = 'dieresistonos';
    LowerDivide = 'divide';
    LowerDkshade = 'dkshade';
    LowerDnblock = 'dnblock';
    LowerDollar = 'dollar';
    LowerDollarinferior = 'dollarinferior';
    LowerDollaroldstyle = 'dollaroldstyle';
    LowerDollarsuperior = 'dollarsuperior';
    LowerDotaccent = 'dotaccent';
    LowerDotlessi = 'dotlessi';
    LowerDotmath = 'dotmath';
    LowerDsuperior = 'dsuperior';
    LowerE = 'e';
    LowerEacute = 'eacute';
    LowerEbreve = 'ebreve';
    LowerEcaron = 'ecaron';
    LowerEcircumflex = 'ecircumflex';
    LowerEdieresis = 'edieresis';
    LowerEdot = 'edot';
    LowerEdotaccent = 'edotaccent';
    LowerEgrave = 'egrave';
    LowerEight = 'eight';
    LowerEightinferior = 'eightinferior';
    LowerEightoldstyle = 'eightoldstyle';
    LowerEightsuperior = 'eightsuperior';
    LowerElement = 'element';
    LowerEllipsis = 'ellipsis';
    LowerEmacron = 'emacron';
    LowerEmdash = 'emdash';
    LowerEmptyset = 'emptyset';
    LowerEndash = 'endash';
    LowerEng = 'eng';
    LowerEogonek = 'eogonek';
    LowerEpsilon = 'epsilon';
    LowerEpsilontonos = 'epsilontonos';
    LowerEqual = 'equal';
    LowerEquivalence = 'equivalence';
    LowerEstimated = 'estimated';
    LowerEsuperior = 'esuperior';
    LowerEta = 'eta';
    LowerEtatonos = 'etatonos';
    LowerEth = 'eth';
    LowerExclam = 'exclam';
    LowerExclamdbl = 'exclamdbl';
    LowerExclamdown = 'exclamdown';
    LowerExclamdownsmall = 'exclamdownsmall';
    LowerExclamsmall = 'exclamsmall';
    LowerExistential = 'existential';
    LowerF = 'f';
    LowerFemale = 'female';
    LowerFf = 'ff';
    LowerFfi = 'ffi';
    LowerFfl = 'ffl';
    LowerFi = 'fi';
    LowerFiguredash = 'figuredash';
    LowerFilledbox = 'filledbox';
    LowerFilledrect = 'filledrect';
    LowerFive = 'five';
    LowerFiveeighths = 'fiveeighths';
    LowerFiveinferior = 'fiveinferior';
    LowerFiveoldstyle = 'fiveoldstyle';
    LowerFivesuperior = 'fivesuperior';
    LowerFl = 'fl';
    LowerFlorin = 'florin';
    LowerFour = 'four';
    LowerFourinferior = 'fourinferior';
    LowerFouroldstyle = 'fouroldstyle';
    LowerFoursuperior = 'foursuperior';
    LowerFraction = 'fraction';
    LowerFranc = 'franc';
    LowerG = 'g';
    LowerGamma = 'gamma';
    LowerGbreve = 'gbreve';
    LowerGcedilla = 'gcedilla';
    LowerGcircumflex = 'gcircumflex';
    LowerGcommaaccent = 'gcommaaccent';
    LowerGdot = 'gdot';
    LowerGermandbls = 'germandbls';
    LowerGradient = 'gradient';
    LowerGrave = 'grave';
    LowerGreater = 'greater';
    LowerGreaterequal = 'greaterequal';
    LowerGuillemotleft = 'guillemotleft';
    LowerGuillemotright = 'guillemotright';
    LowerGuilsinglleft = 'guilsinglleft';
    LowerGuilsinglright = 'guilsinglright';
    LowerH = 'h';
    LowerHbar = 'hbar';
    LowerHcircumflex = 'hcircumflex';
    LowerHeart = 'heart';
    LowerHouse = 'house';
    LowerHungarumlaut = 'hungarumlaut';
    LowerHyphen = 'hyphen';
    LowerHypheninferior = 'hypheninferior';
    LowerHyphensuperior = 'hyphensuperior';
    LowerI = 'i';
    LowerIacute = 'iacute';
    LowerIbreve = 'ibreve';
    LowerIcircumflex = 'icircumflex';
    LowerIdieresis = 'idieresis';
    LowerIgrave = 'igrave';
    LowerIj = 'ij';
    LowerImacron = 'imacron';
    LowerIncrement = 'increment';
    LowerInfinity = 'infinity';
    LowerIntegral = 'integral';
    LowerIntegralbt = 'integralbt';
    LowerIntegralex = 'integralex';
    LowerIntegraltp = 'integraltp';
    LowerIntersection = 'intersection';
    LowerInvbullet = 'invbullet';
    LowerInvcircle = 'invcircle';
    LowerInvsmileface = 'invsmileface';
    LowerIogonek = 'iogonek';
    LowerIota = 'iota';
    LowerIotadieresis = 'iotadieresis';
    LowerIotadieresistonos = 'iotadieresistonos';
    LowerIotatonos = 'iotatonos';
    LowerIsuperior = 'isuperior';
    LowerItilde = 'itilde';
    LowerJ = 'j';
    LowerJcircumflex = 'jcircumflex';
    LowerK = 'k';
    LowerKappa = 'kappa';
    LowerKcedilla = 'kcedilla';
    LowerKcommaaccent = 'kcommaaccent';
    LowerKgreenlandic = 'kgreenlandic';
    LowerL = 'l';
    LowerLacute = 'lacute';
    LowerLambda = 'lambda';
    LowerLcaron = 'lcaron';
    LowerLcedilla = 'lcedilla';
    LowerLcommaaccent = 'lcommaaccent';
    LowerLdot = 'ldot';
    LowerLess = 'less';
    LowerLessequal = 'lessequal';
    LowerLfblock = 'lfblock';
    LowerLogicaland = 'logicaland';
    LowerLogicalnot = 'logicalnot';
    LowerLogicalor = 'logicalor';
    LowerLongs = 'longs';
    LowerLozenge = 'lozenge';
    LowerLslash = 'lslash';
    LowerLsuperior = 'lsuperior';
    LowerLtshade = 'ltshade';
    LowerM = 'm';
    LowerMacron = 'macron';
    LowerMale = 'male';
    LowerMinus = 'minus';
    LowerMinute = 'minute';
    LowerMsuperior = 'msuperior';
    LowerMu = 'mu';
    LowerMultiply = 'multiply';
    LowerMusicalnote = 'musicalnote';
    LowerMusicalnotedbl = 'musicalnotedbl';
    LowerN = 'n';
    LowerNacute = 'nacute';
    LowerNapostrophe = 'napostrophe';
    LowerNbspace = 'nbspace';
    LowerNcaron = 'ncaron';
    LowerNcedilla = 'ncedilla';
    LowerNcommaaccent = 'ncommaaccent';
    LowerNine = 'nine';
    LowerNineinferior = 'nineinferior';
    LowerNineoldstyle = 'nineoldstyle';
    LowerNinesuperior = 'ninesuperior';
    LowerNonbreakingspace = 'nonbreakingspace';
    LowerNonmarkingreturn = 'nonmarkingreturn';
    LowerNotelement = 'notelement';
    LowerNotequal = 'notequal';
    LowerNotsubset = 'notsubset';
    LowerNsuperior = 'nsuperior';
    LowerNtilde = 'ntilde';
    LowerNu = 'nu';
    Null = 'null';
    LowerNumbersign = 'numbersign';
    LowerO = 'o';
    LowerOacute = 'oacute';
    LowerObreve = 'obreve';
    LowerOcircumflex = 'ocircumflex';
    LowerOdblacute = 'odblacute';
    LowerOdieresis = 'odieresis';
    LowerOe = 'oe';
    LowerOgonek = 'ogonek';
    LowerOgrave = 'ograve';
    LowerOhungarumlaut = 'ohungarumlaut';
    LowerOmacron = 'omacron';
    LowerOmega = 'omega';
    LowerOmega1 = 'omega1';
    LowerOmegatonos = 'omegatonos';
    LowerOmicron = 'omicron';
    LowerOmicrontonos = 'omicrontonos';
    LowerOne = 'one';
    LowerOnedotenleader = 'onedotenleader';
    LowerOneeighth = 'oneeighth';
    LowerOnefitted = 'onefitted';
    LowerOnehalf = 'onehalf';
    LowerOneinferior = 'oneinferior';
    LowerOneoldstyle = 'oneoldstyle';
    LowerOnequarter = 'onequarter';
    LowerOnesuperior = 'onesuperior';
    LowerOnethird = 'onethird';
    LowerOpenbullet = 'openbullet';
    LowerOrdfeminine = 'ordfeminine';
    LowerOrdmasculine = 'ordmasculine';
    LowerOrthogonal = 'orthogonal';
    LowerOslash = 'oslash';
    LowerOslashacute = 'oslashacute';
    LowerOsuperior = 'osuperior';
    LowerOtilde = 'otilde';
    LowerP = 'p';
    LowerParagraph = 'paragraph';
    LowerParenleft = 'parenleft';
    LowerParenleftbt = 'parenleftbt';
    LowerParenleftex = 'parenleftex';
    LowerParenleftinferior = 'parenleftinferior';
    LowerParenleftsuperior = 'parenleftsuperior';
    LowerParenlefttp = 'parenlefttp';
    LowerParenright = 'parenright';
    LowerParenrightbt = 'parenrightbt';
    LowerParenrightex = 'parenrightex';
    LowerParenrightinferior = 'parenrightinferior';
    LowerParenrightsuperior = 'parenrightsuperior';
    LowerParenrighttp = 'parenrighttp';
    LowerPartialdiff = 'partialdiff';
    LowerPercent = 'percent';
    LowerPeriod = 'period';
    LowerPeriodcentered = 'periodcentered';
    LowerPeriodinferior = 'periodinferior';
    LowerPeriodsuperior = 'periodsuperior';
    LowerPerpendicular = 'perpendicular';
    LowerPerthousand = 'perthousand';
    LowerPeseta = 'peseta';
    LowerPhi = 'phi';
    LowerPhi1 = 'phi1';
    LowerPi = 'pi';
    LowerPlus = 'plus';
    LowerPlusminus = 'plusminus';
    LowerProduct = 'product';
    LowerPropersubset = 'propersubset';
    LowerPropersuperset = 'propersuperset';
    LowerProportional = 'proportional';
    LowerPsi = 'psi';
    LowerQ = 'q';
    LowerQuestion = 'question';
    LowerQuestiondown = 'questiondown';
    LowerQuestiondownsmall = 'questiondownsmall';
    LowerQuestionsmall = 'questionsmall';
    LowerQuotedbl = 'quotedbl';
    LowerQuotedblbase = 'quotedblbase';
    LowerQuotedblleft = 'quotedblleft';
    LowerQuotedblright = 'quotedblright';
    LowerQuoteleft = 'quoteleft';
    LowerQuotereversed = 'quotereversed';
    LowerQuoteright = 'quoteright';
    LowerQuotesinglbase = 'quotesinglbase';
    LowerQuotesingle = 'quotesingle';
    LowerR = 'r';
    LowerRacute = 'racute';
    LowerRadical = 'radical';
    LowerRadicalex = 'radicalex';
    LowerRcaron = 'rcaron';
    LowerRcedilla = 'rcedilla';
    LowerRcommaaccent = 'rcommaaccent';
    LowerReflexsubset = 'reflexsubset';
    LowerReflexsuperset = 'reflexsuperset';
    LowerRegistered = 'registered';
    LowerRegistersans = 'registersans';
    LowerRegisterserif = 'registerserif';
    LowerRevlogicalnot = 'revlogicalnot';
    LowerRho = 'rho';
    LowerRing = 'ring';
    LowerRsuperior = 'rsuperior';
    LowerRtblock = 'rtblock';
    LowerRupiah = 'rupiah';
    LowerS = 's';
    LowerSacute = 'sacute';
    LowerScaron = 'scaron';
    LowerScedilla = 'scedilla';
    LowerScircumflex = 'scircumflex';
    LowerScommaaccent = 'scommaaccent';
    LowerSecond = 'second';
    LowerSection = 'section';
    LowerSemicolon = 'semicolon';
    LowerSeven = 'seven';
    LowerSeveneighths = 'seveneighths';
    LowerSeveninferior = 'seveninferior';
    LowerSevenoldstyle = 'sevenoldstyle';
    LowerSevensuperior = 'sevensuperior';
    LowerSfthyphen = 'sfthyphen';
    LowerShade = 'shade';
    LowerSigma = 'sigma';
    LowerSigma1 = 'sigma1';
    LowerSimilar = 'similar';
    LowerSix = 'six';
    LowerSixinferior = 'sixinferior';
    LowerSixoldstyle = 'sixoldstyle';
    LowerSixsuperior = 'sixsuperior';
    LowerSlash = 'slash';
    LowerSmileface = 'smileface';
    LowerSpace = 'space';
    LowerSpade = 'spade';
    LowerSsuperior = 'ssuperior';
    LowerSterling = 'sterling';
    LowerSuchthat = 'suchthat';
    LowerSummation = 'summation';
    LowerSun = 'sun';
    LowerT = 't';
    LowerTau = 'tau';
    LowerTbar = 'tbar';
    LowerTcaron = 'tcaron';
    LowerTcedilla = 'tcedilla';
    LowerTcommaaccent = 'tcommaaccent';
    LowerTherefore = 'therefore';
    LowerTheta = 'theta';
    LowerTheta1 = 'theta1';
    LowerThorn = 'thorn';
    LowerThree = 'three';
    LowerThreeeighths = 'threeeighths';
    LowerThreeinferior = 'threeinferior';
    LowerThreeoldstyle = 'threeoldstyle';
    LowerThreequarters = 'threequarters';
    LowerThreequartersemdash = 'threequartersemdash';
    LowerThreesuperior = 'threesuperior';
    LowerTilde = 'tilde';
    LowerTonos = 'tonos';
    LowerTrademark = 'trademark';
    LowerTrademarksans = 'trademarksans';
    LowerTrademarkserif = 'trademarkserif';
    LowerTriagdn = 'triagdn';
    LowerTriaglf = 'triaglf';
    LowerTriagrt = 'triagrt';
    LowerTriagup = 'triagup';
    LowerTsuperior = 'tsuperior';
    LowerTwo = 'two';
    LowerTwodotenleader = 'twodotenleader';
    LowerTwoinferior = 'twoinferior';
    LowerTwooldstyle = 'twooldstyle';
    LowerTwosuperior = 'twosuperior';
    LowerTwothirds = 'twothirds';
    LowerU = 'u';
    LowerUacute = 'uacute';
    LowerUbreve = 'ubreve';
    LowerUcircumflex = 'ucircumflex';
    LowerUdblacute = 'udblacute';
    LowerUdieresis = 'udieresis';
    LowerUgrave = 'ugrave';
    LowerUhungarumlaut = 'uhungarumlaut';
    LowerUmacron = 'umacron';
    LowerUnderscore = 'underscore';
    LowerUnderscoredbl = 'underscoredbl';
    LowerUnion = 'union';
    LowerUniversal = 'universal';
    LowerUogonek = 'uogonek';
    LowerUpblock = 'upblock';
    LowerUpsilon = 'upsilon';
    LowerUpsilondieresis = 'upsilondieresis';
    LowerUpsilondieresistonos = 'upsilondieresistonos';
    LowerUpsilontonos = 'upsilontonos';
    LowerUring = 'uring';
    LowerUtilde = 'utilde';
    LowerV = 'v';
    LowerW = 'w';
    LowerWacute = 'wacute';
    LowerWcircumflex = 'wcircumflex';
    LowerWdieresis = 'wdieresis';
    LowerWeierstrass = 'weierstrass';
    LowerWgrave = 'wgrave';
    LowerX = 'x';
    LowerXi = 'xi';
    LowerY = 'y';
    LowerYacute = 'yacute';
    LowerYcircumflex = 'ycircumflex';
    LowerYdieresis = 'ydieresis';
    LowerYen = 'yen';
    LowerYgrave = 'ygrave';
    LowerZ = 'z';
    LowerZacute = 'zacute';
    LowerZcaron = 'zcaron';
    LowerZdot = 'zdot';
    LowerZdotaccent = 'zdotaccent';
    LowerZero = 'zero';
    LowerZeroinferior = 'zeroinferior';
    LowerZerooldstyle = 'zerooldstyle';
    LowerZerosuperior = 'zerosuperior';
    LowerZeta = 'zeta';
{$ENDREGION}
  end;

  { TdxFontFileCustomEncoding }

  TdxFontFileCustomEncoding = class
  strict private
    FDictionary: TdxPDFByteStringDictionary;
  protected
    procedure Initialize; virtual;
  public
    class function GetName: string; virtual;
    constructor Create; virtual;
    destructor Destroy; override;

    property Dictionary: TdxPDFByteStringDictionary read FDictionary;
  end;

  TdxFontFileCustomEncodingClass = class of TdxFontFileCustomEncoding;

  { TdxFontFileUnicodeConverter }

  TdxFontFileUnicodeConverter = class // for internal use
  strict private
    FGlyphCodes: TdxPDFWordDictionary;
    function InternalFindCode(ACode: Word; AEncoding: TdxFontFileCustomEncoding;
      AGlyphCodes: TdxPDFWordDictionary; out AResult: Word): Boolean;
    procedure InitializePack1;
    procedure InitializePack2;
    procedure InitializePack3;
    procedure InitializePack4;
  public
    constructor Create;
    destructor Destroy; override;

    function FindCode(const AName: string; out ACode: Word): Boolean; overload;
    function FindCode(AEncoding: TdxFontFileCustomEncoding; ACode: Word;
      out AResult: Word): Boolean; overload;
    function FindCode(AEncoding: TdxFontFileCustomEncoding;
      AGlyphCodes: TdxPDFWordDictionary; ACode: Word; out AResult: Word)
      : Boolean; overload;

    property GlyphCodes: TdxPDFWordDictionary read FGlyphCodes;
  end;

  { TdxFontFileMacRomanEncoding }

  TdxFontFileMacRomanEncoding = class(TdxFontFileCustomEncoding)
  protected
    procedure Initialize; override;
  public
    class function GetName: string; override;
  end;

  { TdxFontFileMacRomanReversedEncoding }

  TdxFontFileMacRomanReversedEncoding = class(TdxFontFileCustomEncoding)
  strict private
    FReversedDictionary: TdxPDFWordDictionary;
  protected
    procedure Initialize; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    property ReversedDictionary: TdxPDFWordDictionary read FReversedDictionary;
  end;

  { TdxFontFileStandardEncoding }

  TdxFontFileStandardEncoding = class(TdxFontFileCustomEncoding)
  protected
    procedure Initialize; override;
  public
    class function GetName: string; override;
  end;

  { TdxFontFileWinAnsiEncoding }

  TdxFontFileWinAnsiEncoding = class(TdxFontFileCustomEncoding)
  protected
    procedure Initialize; override;
  public
    class function GetName: string; override;
  end;

  { TdxFontFileSymbolEncoding }

  TdxFontFileSymbolEncoding = class(TdxFontFileCustomEncoding)
  protected
    procedure Initialize; override;
  public
    class function GetName: string; override;
  end;

  { TdxFontFileZapfDingbatsEncoding }

  TdxFontFileZapfDingbatsEncoding = class(TdxFontFileCustomEncoding)
  protected
    procedure Initialize; override;
  public
    class function GetName: string; override;
  end;

  { TdxFontFileBinaryTable }

  TdxFontFileBinaryTable = class
  strict private
    FDataStream: TdxFontFileStream;
    FName: string;
    FNeedWrite: Boolean;
    function GetTableData: TBytes;
    function GetDataSize: Integer;
    procedure CalculateCheckSum(var ACheckSum: Integer);
  protected
    procedure DoApplyChanges; virtual;

    function Write(AStream: TdxFontFileStream; AOffset: Integer): Integer;
    procedure ApplyChanges; virtual;
    procedure Changed(AIsChanged: Boolean = True);
    procedure RecreateStream;

    property DataStream: TdxFontFileStream read FDataStream;
    property Name: string read FName write FName;
  public
    constructor Create; overload;
    constructor Create(const AData: TBytes); overload; virtual;
    destructor Destroy; override;

    class function Tag: string; virtual;
    function AlignedTableData: TBytes;

    property Data: TBytes read GetTableData;
    property DataSize: Integer read GetDataSize;
    property NeedWrite: Boolean read FNeedWrite;
  end;

  { TdxFontFileLocaTable }

  TdxFontFileLocaTable = class(TdxFontFileBinaryTable)
  strict private
    FIsShortFormat: Boolean;
    FGlyphOffsets: TIntegerDynArray;
    procedure SetGlyphOffsets(const AValue: TIntegerDynArray);
  protected
    procedure DoApplyChanges; override;
  public
    class function Tag: string; override;
    procedure ReadOffsets(AFontFile: TdxFontFile);

    property GlyphOffsets: TIntegerDynArray read FGlyphOffsets
      write SetGlyphOffsets;
  end;

  { TdxFontFileCFFTable }

  TdxFontFileCFFTable = class(TdxFontFileBinaryTable)
  public
    class function Tag: string; override;
  end;

  { TdxFontFilePostTable }

  TdxFontFilePostTable = class(TdxFontFileBinaryTable)
  strict private
  const
    StandardMacCharacterSet: array [0 .. 257] of string =
      (TdxGlyphNames._notdef, TdxGlyphNames.Null,
      TdxGlyphNames.LowerNonmarkingreturn, TdxGlyphNames.LowerSpace,
      TdxGlyphNames.LowerExclam, TdxGlyphNames.LowerQuotedbl,
      TdxGlyphNames.LowerNumbersign, TdxGlyphNames.LowerDollar,
      TdxGlyphNames.LowerPercent, TdxGlyphNames.LowerAmpersand,
      TdxGlyphNames.LowerQuotesingle, TdxGlyphNames.LowerParenleft,
      TdxGlyphNames.LowerParenright, TdxGlyphNames.LowerAsterisk,
      TdxGlyphNames.LowerPlus, TdxGlyphNames.LowerComma,
      TdxGlyphNames.LowerHyphen, TdxGlyphNames.LowerPeriod,
      TdxGlyphNames.LowerSlash, TdxGlyphNames.LowerZero, TdxGlyphNames.LowerOne,
      TdxGlyphNames.LowerTwo, TdxGlyphNames.LowerThree, TdxGlyphNames.LowerFour,
      TdxGlyphNames.LowerFive, TdxGlyphNames.LowerSix, TdxGlyphNames.LowerSeven,
      TdxGlyphNames.LowerEight, TdxGlyphNames.LowerNine,
      TdxGlyphNames.LowerColon, TdxGlyphNames.LowerSemicolon,
      TdxGlyphNames.LowerLess, TdxGlyphNames.LowerEqual,
      TdxGlyphNames.LowerGreater, TdxGlyphNames.LowerQuestion,
      TdxGlyphNames.LowerAt, TdxGlyphNames.A, TdxGlyphNames.B, TdxGlyphNames.C,
      TdxGlyphNames.D, TdxGlyphNames.E, TdxGlyphNames.F, TdxGlyphNames.G,
      TdxGlyphNames.H, TdxGlyphNames.I, TdxGlyphNames.J, TdxGlyphNames.K,
      TdxGlyphNames.L, TdxGlyphNames.M, TdxGlyphNames.N, TdxGlyphNames.O,
      TdxGlyphNames.P, TdxGlyphNames.Q, TdxGlyphNames.R, TdxGlyphNames.S,
      TdxGlyphNames.T, TdxGlyphNames.U, TdxGlyphNames.V, TdxGlyphNames.W,
      TdxGlyphNames.X, TdxGlyphNames.Y, TdxGlyphNames.Z,
      TdxGlyphNames.LowerBracketleft, TdxGlyphNames.LowerBackslash,
      TdxGlyphNames.LowerBracketright, TdxGlyphNames.LowerAsciicircum,
      TdxGlyphNames.LowerUnderscore, TdxGlyphNames.LowerGrave,
      TdxGlyphNames.LowerA, TdxGlyphNames.LowerB, TdxGlyphNames.LowerC,
      TdxGlyphNames.LowerD, TdxGlyphNames.LowerE, TdxGlyphNames.LowerF,
      TdxGlyphNames.LowerG, TdxGlyphNames.LowerH, TdxGlyphNames.LowerI,
      TdxGlyphNames.LowerJ, TdxGlyphNames.LowerK, TdxGlyphNames.LowerL,
      TdxGlyphNames.LowerM, TdxGlyphNames.LowerN, TdxGlyphNames.LowerO,
      TdxGlyphNames.LowerP, TdxGlyphNames.LowerQ, TdxGlyphNames.LowerR,
      TdxGlyphNames.LowerS, TdxGlyphNames.LowerT, TdxGlyphNames.LowerU,
      TdxGlyphNames.LowerV, TdxGlyphNames.LowerW, TdxGlyphNames.LowerX,
      TdxGlyphNames.LowerY, TdxGlyphNames.LowerZ, TdxGlyphNames.LowerBraceleft,
      TdxGlyphNames.LowerBar, TdxGlyphNames.LowerBraceright,
      TdxGlyphNames.LowerAsciitilde, TdxGlyphNames.Adieresis,
      TdxGlyphNames.Aring, TdxGlyphNames.Ccedilla, TdxGlyphNames.Eacute,
      TdxGlyphNames.Ntilde, TdxGlyphNames.Odieresis, TdxGlyphNames.Udieresis,
      TdxGlyphNames.LowerAacute, TdxGlyphNames.LowerAgrave,
      TdxGlyphNames.LowerAcircumflex, TdxGlyphNames.LowerAdieresis,
      TdxGlyphNames.LowerAtilde, TdxGlyphNames.LowerAring,
      TdxGlyphNames.LowerCcedilla, TdxGlyphNames.LowerEacute,
      TdxGlyphNames.LowerEgrave, TdxGlyphNames.LowerEcircumflex,
      TdxGlyphNames.LowerEdieresis, TdxGlyphNames.LowerIacute,
      TdxGlyphNames.LowerIgrave, TdxGlyphNames.LowerIcircumflex,
      TdxGlyphNames.LowerIdieresis, TdxGlyphNames.LowerNtilde,
      TdxGlyphNames.LowerOacute, TdxGlyphNames.LowerOgrave,
      TdxGlyphNames.LowerOcircumflex, TdxGlyphNames.LowerOdieresis,
      TdxGlyphNames.LowerOtilde, TdxGlyphNames.LowerUacute,
      TdxGlyphNames.LowerUgrave, TdxGlyphNames.LowerUcircumflex,
      TdxGlyphNames.LowerUdieresis, TdxGlyphNames.LowerDagger,
      TdxGlyphNames.LowerDegree, TdxGlyphNames.LowerCent,
      TdxGlyphNames.LowerSterling, TdxGlyphNames.LowerSection,
      TdxGlyphNames.LowerBullet, TdxGlyphNames.LowerParagraph,
      TdxGlyphNames.LowerGermandbls, TdxGlyphNames.LowerRegistered,
      TdxGlyphNames.LowerCopyright, TdxGlyphNames.LowerTrademark,
      TdxGlyphNames.LowerAcute, TdxGlyphNames.LowerDieresis,
      TdxGlyphNames.LowerNotequal, TdxGlyphNames.AE, TdxGlyphNames.Oslash,
      TdxGlyphNames.LowerInfinity, TdxGlyphNames.LowerPlusminus,
      TdxGlyphNames.LowerLessequal, TdxGlyphNames.LowerGreaterequal,
      TdxGlyphNames.LowerYen, TdxGlyphNames.LowerMu,
      TdxGlyphNames.LowerPartialdiff, TdxGlyphNames.LowerSummation,
      TdxGlyphNames.LowerProduct, TdxGlyphNames.LowerPi,
      TdxGlyphNames.LowerIntegral, TdxGlyphNames.LowerOrdfeminine,
      TdxGlyphNames.LowerOrdmasculine, TdxGlyphNames.Omega,
      TdxGlyphNames.LowerAe, TdxGlyphNames.LowerOslash,
      TdxGlyphNames.LowerQuestiondown, TdxGlyphNames.LowerExclamdown,
      TdxGlyphNames.LowerLogicalnot, TdxGlyphNames.LowerRadical,
      TdxGlyphNames.LowerFlorin, TdxGlyphNames.LowerApproxequal,
      TdxGlyphNames.LowerIncrement, TdxGlyphNames.LowerGuillemotleft,
      TdxGlyphNames.LowerGuillemotright, TdxGlyphNames.LowerEllipsis,
      TdxGlyphNames.LowerNonbreakingspace, TdxGlyphNames.Agrave,
      TdxGlyphNames.Atilde, TdxGlyphNames.Otilde, TdxGlyphNames.OE,
      TdxGlyphNames.LowerOe, TdxGlyphNames.LowerEndash,
      TdxGlyphNames.LowerEmdash, TdxGlyphNames.LowerQuotedblleft,
      TdxGlyphNames.LowerQuotedblright, TdxGlyphNames.LowerQuoteleft,
      TdxGlyphNames.LowerQuoteright, TdxGlyphNames.LowerDivide,
      TdxGlyphNames.LowerLozenge, TdxGlyphNames.LowerYdieresis,
      TdxGlyphNames.Ydieresis, TdxGlyphNames.LowerFraction,
      TdxGlyphNames.LowerCurrency, TdxGlyphNames.LowerGuilsinglleft,
      TdxGlyphNames.LowerGuilsinglright, TdxGlyphNames.LowerFi,
      TdxGlyphNames.LowerFl, TdxGlyphNames.LowerDaggerdbl,
      TdxGlyphNames.LowerPeriodcentered, TdxGlyphNames.LowerQuotesinglbase,
      TdxGlyphNames.LowerQuotedblbase, TdxGlyphNames.LowerPerthousand,
      TdxGlyphNames.Acircumflex, TdxGlyphNames.Ecircumflex,
      TdxGlyphNames.Aacute, TdxGlyphNames.Edieresis, TdxGlyphNames.Egrave,
      TdxGlyphNames.Iacute, TdxGlyphNames.Icircumflex, TdxGlyphNames.Idieresis,
      TdxGlyphNames.Igrave, TdxGlyphNames.Oacute, TdxGlyphNames.Ocircumflex,
      TdxGlyphNames.LowerApple, TdxGlyphNames.Ograve, TdxGlyphNames.Uacute,
      TdxGlyphNames.Ucircumflex, TdxGlyphNames.Ugrave,
      TdxGlyphNames.LowerDotlessi, TdxGlyphNames.LowerCircumflex,
      TdxGlyphNames.LowerTilde, TdxGlyphNames.LowerMacron,
      TdxGlyphNames.LowerBreve, TdxGlyphNames.LowerDotaccent,
      TdxGlyphNames.LowerRing, TdxGlyphNames.LowerCedilla,
      TdxGlyphNames.LowerHungarumlaut, TdxGlyphNames.LowerOgonek,
      TdxGlyphNames.LowerCaron, TdxGlyphNames.Lslash, TdxGlyphNames.LowerLslash,
      TdxGlyphNames.Scaron, TdxGlyphNames.LowerScaron, TdxGlyphNames.Zcaron,
      TdxGlyphNames.LowerZcaron, TdxGlyphNames.LowerBrokenbar,
      TdxGlyphNames.Eth, TdxGlyphNames.LowerEth, TdxGlyphNames.Yacute,
      TdxGlyphNames.LowerYacute, TdxGlyphNames.Thorn, TdxGlyphNames.LowerThorn,
      TdxGlyphNames.LowerMinus, TdxGlyphNames.LowerMultiply,
      TdxGlyphNames.LowerOnesuperior, TdxGlyphNames.LowerTwosuperior,
      TdxGlyphNames.LowerThreesuperior, TdxGlyphNames.LowerOnehalf,
      TdxGlyphNames.LowerOnequarter, TdxGlyphNames.LowerThreequarters,
      TdxGlyphNames.LowerFranc, TdxGlyphNames.Gbreve, TdxGlyphNames.LowerGbreve,
      TdxGlyphNames.Idotaccent, TdxGlyphNames.Scedilla,
      TdxGlyphNames.LowerScedilla, TdxGlyphNames.Cacute,
      TdxGlyphNames.LowerCacute, TdxGlyphNames.Ccaron,
      TdxGlyphNames.LowerCcaron, TdxGlyphNames.LowerDcroat);
  protected
    FIsFixedPitch: Boolean;
    FItalicAngle: Single;
    FMaxMemType42: Integer;
    FMinMemType42: Integer;
    FMaxMemType1: Integer;
    FMinMemType1: Integer;
    FUnderlinePosition: SmallInt;
    FUnderlineThickness: SmallInt;
    FGlyphNames: TStringList;
  public const
    FontIsMonospaced = 1;
    FontIsProportionallySpaced = 0;
    MaxMemType1 = 0;
    MaxMemType42 = 0;
    MinMemType1 = 0;
    MinMemType42 = 0;
    HeaderSize = 32;
    Version = $00030000;
  public
    constructor Create(const AData: TBytes); override;
    destructor Destroy; override;
    class function Tag: string; override;

    property ItalicAngle: Single read FItalicAngle;
    property GlyphNames: TStringList read FGlyphNames;
    property UnderlinePosition: SmallInt read FUnderlinePosition;
  end;

  { TdxFontFileOS2Table }

  TdxFontFileOS2Table = class(TdxFontFileBinaryTable)
  strict private
  const
    BreakChar = 32;
    DefaultChar = 0;
    MaxContext = 0;
  strict private
    function GetIsSymbolic: Boolean;
    function GetUseTypoMetrics: Boolean;
    procedure SetWinAscent(const AValue: SmallInt);
    procedure SetWinDescent(const AValue: SmallInt);
  protected
    FAvgCharWidth: SmallInt;
    FBreakChar: SmallInt;
    FCapHeight: SmallInt;
    FCodePageRange1: TdxFontFileCodePageRange1;
    FCodePageRange2: TdxFontFileCodePageRange2;
    FDefaultChar: SmallInt;
    FEmbeddingType: TdxFontFileOS2EmbeddingType;
    FFamilyClass: TdxFontFileOS2FamilyClass;
    FFirstCharIndex: Integer;
    FLastCharIndex: Integer;
    FMaxContext: SmallInt;
    FPanose: TdxFontFilePanose;
    FSelection: TdxFontFileSelection;
    FStrikeoutPosition: SmallInt;
    FStrikeoutSize: SmallInt;
    FSubscriptXOffset: SmallInt;
    FSubscriptXSize: SmallInt;
    FSubscriptYOffset: SmallInt;
    FSubscriptYSize: SmallInt;
    FSuperscriptXOffset: SmallInt;
    FSuperscriptXSize: SmallInt;
    FSuperscriptYOffset: SmallInt;
    FSuperscriptYSize: SmallInt;
    FTypoAscender: SmallInt;
    FTypoDescender: SmallInt;
    FTypoLineGap: SmallInt;
    FUnicodeRange1: TdxFontFileUnicodeRange1;
    FUnicodeRange2: TdxFontFileUnicodeRange2;
    FUnicodeRange3: TdxFontFileUnicodeRange3;
    FUnicodeRange4: TdxFontFileUnicodeRange4;
    FVendor: string;
    FVersion: TdxFontFileVersion;
    FWeightClass: SmallInt;
    FWidthClass: TdxFontFileOS2WidthClass;
    FWinAscent: SmallInt;
    FWinDescent: SmallInt;
    FXHeight: SmallInt;

    procedure DoApplyChanges; override;
  public const
    BoldFontWeight = 700;
    NormalFontWeight = 400;
  public
    constructor Create(const AData: TBytes); overload; override;

    class function Tag: string; override;

    property IsSymbolic: Boolean read GetIsSymbolic;
    property WinAscent: SmallInt read FWinAscent write SetWinAscent;
    property WinDescent: SmallInt read FWinDescent write SetWinDescent;
    property Panose: TdxFontFilePanose read FPanose;
    property TypoAscender: SmallInt read FTypoAscender;
    property TypoDescender: SmallInt read FTypoDescender;
    property TypoLineGap: SmallInt read FTypoLineGap;
    property UseTypoMetrics: Boolean read GetUseTypoMetrics;
  end;

  { TdxFontFileMaxpTable }

  TdxFontFileMaxpTable = class(TdxFontFileBinaryTable)
  strict private
  const
    NumGlyphsOffset = 4;
  strict private
    function GetNumGlyphs: Integer;
    procedure SetNumGlyphs(const AValue: Integer);
  public
    constructor Create(AGlyphCount: Integer);
    class function Tag: string; override;

    property NumGlyphs: Integer read GetNumGlyphs write SetNumGlyphs;
  end;

  { TdxFontFileKernTable }

  TdxFontFileKernTable = class(TdxFontFileBinaryTable)
  strict private
    FKerning: TDictionary<Integer, SmallInt>;
  public
    constructor Create(const AData: TBytes); override;
    destructor Destroy; override;
    class function Tag: string; override;

    function GetKerning(AGlyphIndex1: Integer; AGlyphIndex2: Integer): SmallInt;
  end;

  { TdxFontFileHmtxTable }

  TdxFontFileHmtxTable = class(TdxFontFileBinaryTable)
  strict private
    FAdvanceWidths: TSmallIntDynArray;
    FExpectedDataSize: Integer;
  protected
    procedure ApplyChanges; override;
  public
    constructor Create(AGlyphCount: Integer);

    class function Tag: string; override;
    function FillAdvanceWidths(AHMetricCount, AGlyphCount: Integer)
      : TSmallIntDynArray;
    procedure Validate(AFontFile: TdxFontFile);

    property AdvanceWidths: TSmallIntDynArray read FAdvanceWidths
      write FAdvanceWidths;
  end;

  { TdxFontFileHheaTable }

  TdxFontFileHheaTable = class(TdxFontFileBinaryTable)
  protected
    FAdvanceWidthMax: SmallInt;
    FAscender: SmallInt;
    FCaretSlopeRise: SmallInt;
    FCaretSlopeRun: SmallInt;
    FDescender: SmallInt;
    FLineGap: SmallInt;
    FMetricDataFormat: SmallInt;
    FMinLeftSideBearing: SmallInt;
    FMinRightSideBearing: SmallInt;
    FNumberOfHMetrics: Integer;
    FVersion: Integer;
    FXMaxExtent: SmallInt;

    procedure DoApplyChanges; override;
  public
    constructor Create(const AData: TBytes); override;
    class function Tag: string; override;

    procedure Validate;

    property AdvanceWidthMax: SmallInt read FAdvanceWidthMax;
    property Ascender: SmallInt read FAscender;
    property CaretSlopeRise: SmallInt read FCaretSlopeRise;
    property CaretSlopeRun: SmallInt read FCaretSlopeRun;
    property Descender: SmallInt read FDescender;
    property LineGap: SmallInt read FLineGap;
    property MetricDataFormat: SmallInt read FMetricDataFormat;
    property MinLeftSideBearing: SmallInt read FMinLeftSideBearing;
    property MinRightSideBearing: SmallInt read FMinRightSideBearing;
    property NumberOfHMetrics: Integer read FNumberOfHMetrics;
    property Version: Integer read FVersion;
    property XMaxExtent: SmallInt read FXMaxExtent;
  end;

  { TdxFontFileHeadTable }

  TdxFontFileHeadTable = class(TdxFontFileBinaryTable)
  strict private
  const
    DefaultUnitsPerEm = 2048;
  protected
    FCheckSumAdjustment: Integer;
    FCreated: Int64;
    FFlags: TdxFontFileHeadTableFlags;
    FFontDirectionHint: TdxFontFileDirectionHint;
    FFontRevision: Integer;
    FGlyphDataFormat: SmallInt;
    FIndexToLocFormat: TdxFontFileIndexToLocFormat;
    FLowestRecPPEM: SmallInt;
    FMacStyle: TdxFontFileHeadTableMacStyle;
    FMagicNumber: Integer;
    FModified: Int64;
    FUnitsPerEm: SmallInt;
    FVersion: Integer;
    FXMax: SmallInt;
    FXMin: SmallInt;
    FYMax: SmallInt;
    FYMin: SmallInt;

    procedure DoApplyChanges; override;
  public
    constructor Create(const AData: TBytes); override;
    class function Tag: string; override;

    procedure Validate(AGlyphs: TDictionary<Integer,
      TdxFontFileGlyphDescription>);

    property CheckSumAdjustment: Integer read FCheckSumAdjustment;
    property Created: Int64 read FCreated;
    property Flags: TdxFontFileHeadTableFlags read FFlags;
    property FontDirectionHint: TdxFontFileDirectionHint
      read FFontDirectionHint;
    property FontRevision: Integer read FFontRevision;
    property GlyphDataFormat: SmallInt read FGlyphDataFormat;
    property IndexToLocFormat: TdxFontFileIndexToLocFormat
      read FIndexToLocFormat;
    property LowestRecPPEM: SmallInt read FLowestRecPPEM;
    property MacStyle: TdxFontFileHeadTableMacStyle read FMacStyle;
    property MagicNumber: Integer read FMagicNumber;
    property Modified: Int64 read FModified;
    property UnitsPerEm: SmallInt read FUnitsPerEm;
    property Version: Integer read FVersion;
    property XMax: SmallInt read FXMax;
    property XMin: SmallInt read FXMin;
    property YMax: SmallInt read FYMax;
    property YMin: SmallInt read FYMin;
  end;

  { TdxFontFileCMapCustomFormatRecord }

  TdxFontFileCMapCustomFormatRecord = class
  private const
    MissingGlyphIndex = 0;
  strict private
    FEncodingId: TdxFontFileEncodingID;
    FLanguage: Integer;
    FPlatformId: TdxFontFilePlatformID;
  protected
    function GetFormat: TdxFontFileCMapFormatID; virtual; abstract;
    function GetSize: Integer; virtual; abstract;
    procedure UpdateEncoding(var AEncoding: TSmallIntDynArray); virtual;
    procedure Write(AStream: TdxFontFileStream); virtual;

    class procedure UpdateEncodingValue(const AEncoding: TSmallIntDynArray;
      AIndex, AValue: SmallInt);

    property Language: Integer read FLanguage write FLanguage;
    property Format: TdxFontFileCMapFormatID read GetFormat;
  public const
    NotdefGlyphIndex = 0;
  public
    constructor Create(APlatformId: TdxFontFilePlatformID;
      AEncodingId: TdxFontFileEncodingID); overload;
    constructor Create(APlatformId: TdxFontFilePlatformID;
      AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream);
      overload; virtual;

    function MapCode(ACharacter: Char): Integer; virtual;

    property EncodingId: TdxFontFileEncodingID read FEncodingId;
    property PlatformId: TdxFontFilePlatformID read FPlatformId;
    property Size: Integer read GetSize;
  end;

  { TdxFontFileCMapShortFormatRecord }

  TdxFontFileCMapShortFormatRecord = class(TdxFontFileCMapCustomFormatRecord)
  private const
    HeaderLength = 6;
  strict private
    FBodyLength: Integer;
  protected
    function GetSize: Integer; override;
    procedure Write(ATableStream: TdxFontFileStream); override;

    property BodyLength: Integer read FBodyLength;
  public
    constructor Create(APlatformId: TdxFontFilePlatformID;
      AEncodingId: TdxFontFileEncodingID; ALanguage: SmallInt);
      overload; virtual;
    constructor Create(APlatformId: TdxFontFilePlatformID;
      AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream);
      overload; override;
  end;

  { TdxFontFileCMapTrimmedMappingRecord }

  TdxFontFileCMapTrimmedMappingRecord = class(TdxFontFileCMapShortFormatRecord)
  strict private
    FFirstCode: SmallInt;
    FEntryCount: SmallInt;
    FGlyphIdArray: TSmallIntDynArray;
  protected
    function GetFormat: TdxFontFileCMapFormatID; override;
    function GetSize: Integer; override;
    procedure UpdateEncoding(var AEncoding: TSmallIntDynArray); override;
  public
    constructor Create(APlatformId: TdxFontFilePlatformID;
      AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream); override;

    function MapCode(ACharacter: Char): Integer; override;
    procedure Write(ATableStream: TdxFontFileStream); override;

    property EntryCount: SmallInt read FEntryCount;
    property FirstCode: SmallInt read FFirstCode;
    property GlyphIdArray: TSmallIntDynArray read FGlyphIdArray;
  end;

  { TdxFontFileCMapByteEncodingRecord }

  TdxFontFileCMapByteEncodingRecord = class(TdxFontFileCMapShortFormatRecord)
  strict private
    FGlyphIdArray: TBytes;
  protected
    function GetFormat: TdxFontFileCMapFormatID; override;
    procedure UpdateEncoding(var AEncoding: TSmallIntDynArray); override;
  public
    constructor Create(APlatformId: TdxFontFilePlatformID;
      AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream); override;

    function MapCode(ACharacter: Char): Integer; override;
    procedure Write(ATableStream: TdxFontFileStream); override;

    property GlyphIdArray: TBytes read FGlyphIdArray;
  end;

  { TdxFontFileCMapHighByteMappingThroughSubHeader }

  TdxFontFileCMapHighByteMappingThroughSubHeader = class
  strict private
    FEntryCount: SmallInt;
    FFirstCode: SmallInt;
    FGlyphOffset: Integer;
    FIdDelta: SmallInt;
    FIdRangeOffset: SmallInt;
  public
    constructor Create(AFirstCode, AEntryCount, AIdDelta, AIdRangeOffset
      : SmallInt; AGlyphOffset: Integer);
    function CalcGlyphIndexArraySize(AOffset: Integer): Integer;

    property EntryCount: SmallInt read FEntryCount;
    property FirstCode: SmallInt read FFirstCode;
    property GlyphOffset: Integer read FGlyphOffset;
    property IdDelta: SmallInt read FIdDelta;
    property IdRangeOffset: SmallInt read FIdRangeOffset;
  end;

  TdxFontFileCMapHighByteMappingThroughSubHeaders = array of
    TdxFontFileCMapHighByteMappingThroughSubHeader;

  { TdxFontFileCMapHighByteMappingThroughRecord }

  TdxFontFileCMapHighByteMappingThroughRecord = class
    (TdxFontFileCMapShortFormatRecord)
  strict private
  const
    SubHeaderKeysLength = 512;
    SubHeaderLength = 8;
  strict private
    FGlyphIndexArray: TSmallIntDynArray;
    FSubHeaderKeys: TSmallIntDynArray;
    FSubHeaders: TdxFontFileCMapHighByteMappingThroughSubHeaders;
  protected
    function GetFormat: TdxFontFileCMapFormatID; override;
    function GetSize: Integer; override;
    procedure UpdateEncoding(var AEncoding: TSmallIntDynArray); override;

    function ReadSubHeader(AStream: TdxFontFileStream;
      AEndOfSubheadersPosition: Integer)
      : TdxFontFileCMapHighByteMappingThroughSubHeader;
  public
    constructor Create(APlatformId: TdxFontFilePlatformID;
      AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream); override;
    destructor Destroy; override;

    function MapCode(ACharacter: Char): Integer; override;
    procedure Write(ATableStream: TdxFontFileStream); override;

    property GlyphIndexArray: TSmallIntDynArray read FGlyphIndexArray;
    property SubHeaderKeys: TSmallIntDynArray read FSubHeaderKeys;
    property SubHeaders: TdxFontFileCMapHighByteMappingThroughSubHeaders
      read FSubHeaders;
  end;

  { TdxFontFileCMapSegmentMappingRecord }

  TdxFontFileCMapSegmentMappingRecord = class(TdxFontFileCMapShortFormatRecord)
  strict private
  const
    FinalCode = -1;
    FinalDelta = 1;
    SymbolicEncodingMicrosoftOffset = $F000;
  strict private
  type
    TMap = record
      CharCode: Char;
      GID: SmallInt;
      class function Create(ACharCode: Char; AGID: SmallInt): TMap; static;
    end;
  strict private
    FEndCode: TSmallIntDynArray;
    FGlyphIdArray: TSmallIntDynArray;
    FGlyphRanges: TList<TdxFontFileCMapGlyphRange>;
    FIdDelta: TSmallIntDynArray;
    FIdRangeOffset: TSmallIntDynArray;
    FSegCount: Integer;
    FSegmentOffsets: TIntegerDynArray;
    FStartCode: TSmallIntDynArray;
    function GetSegmentsLength: Integer;
    function IsUndefinedEncoding: Boolean;
    function ReadSegmentsArray(ACMapStream: TdxFontFileStream)
      : TSmallIntDynArray;
  protected
    function GetFormat: TdxFontFileCMapFormatID; override;
    function GetSize: Integer; override;
    procedure UpdateEncoding(var AEncoding: TSmallIntDynArray); override;
    function IsSymbolEncoding: Boolean;

    property SegmentsLength: Integer read GetSegmentsLength;
  public
    constructor Create(APlatformId: TdxFontFilePlatformID;
      AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream); override;
    constructor CreateDefault(AEncodingId: TdxFontFileEncodingID);
    constructor CreateFromCharset(ACharset: TdxPDFSmallIntegerDictionary);
    constructor CreateFromTrimmedMapping(AEncodingId: TdxFontFileEncodingID;
      AFormatEntry: TdxFontFileCMapTrimmedMappingRecord);
    constructor CreateFromByteEncoding(AEncodingId: TdxFontFileEncodingID;
      AFormatEntry: TdxFontFileCMapByteEncodingRecord);
    constructor CreateFromSegmentMapping(AEncodingId: TdxFontFileEncodingID;
      AFormatEntry: TdxFontFileCMapSegmentMappingRecord);
    destructor Destroy; override;

    function MapCode(ACharacterCode: Char): Integer; override;
    procedure Write(ATableStream: TdxFontFileStream); override;

    function CreateGlyphMapping(AGlyphNames: TStringList): TdxPDFWordDictionary;
    function GetGlyphRanges: TList<TdxFontFileCMapGlyphRange>;
    function Validate: Boolean;

    property EndCode: TSmallIntDynArray read FEndCode;
    property GlyphIdArray: TSmallIntDynArray read FGlyphIdArray;
    property GlyphRanges: TList<TdxFontFileCMapGlyphRange> read GetGlyphRanges;
    property IdDelta: TSmallIntDynArray read FIdDelta;
    property IdRangeOffset: TSmallIntDynArray read FIdRangeOffset;
    property SegCount: Integer read FSegCount;
    property StartCode: TSmallIntDynArray read FStartCode;
  end;

  { TdxFontFileCMapLongRecord }

  TdxFontFileCMapLongRecord = class(TdxFontFileCMapCustomFormatRecord)
  protected
    procedure Write(ATableStream: TdxFontFileStream); override;
  public
    constructor Create(APlatformId: TdxFontFilePlatformID;
      AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream); override;
  end;

  { TdxFontFileCMapGroup }

  TdxFontFileCMapGroup = record
  strict private
    FEndCharCode: Integer;
    FGlyphID: Integer;
    FStartCharCode: Integer;
  public
    class function Create(AStartCharCode, AEndCharCode, AGlyphID: Integer)
      : TdxFontFileCMapGroup; static;
    class function ReadGroups(AStream: TdxFontFileStream; AGroupsCount: Integer)
      : TArray<TdxFontFileCMapGroup>; static;
    class procedure WriteGroups(const AGroups: TArray<TdxFontFileCMapGroup>;
      ATableStream: TdxFontFileStream); static;

    property EndCharCode: Integer read FEndCharCode;
    property GlyphID: Integer read FGlyphID;
    property StartCharCode: Integer read FStartCharCode;
  end;

  { TdxFontFileCMapRangeMappingRecord }

  TdxFontFileCMapRangeMappingRecord = class(TdxFontFileCMapLongRecord)
  strict private
  const
    HeaderLength = 16;
  strict private
    FGroups: TArray<TdxFontFileCMapGroup>;
  protected
    function GetSize: Integer; override;
    procedure Write(ATableStream: TdxFontFileStream); override;

    property Groups: TArray<TdxFontFileCMapGroup> read FGroups;
  public
    constructor Create(APlatformId: TdxFontFilePlatformID;
      AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream); override;
  end;

  { TdxFontFileCMapSegmentedCoverageRecord }

  TdxFontFileCMapSegmentedCoverageRecord = class
    (TdxFontFileCMapRangeMappingRecord)
  protected
    function GetFormat: TdxFontFileCMapFormatID; override;
  end;

  { TdxFontFileCMapManyToOneRangeMappingRecord }

  TdxFontFileCMapManyToOneRangeMappingRecord = class
    (TdxFontFileCMapRangeMappingRecord)
  protected
    function GetFormat: TdxFontFileCMapFormatID; override;
  end;

  { TdxFontFileCMapMixedCoverageRecord }

  TdxFontFileCMapMixedCoverageRecord = class(TdxFontFileCMapLongRecord)
  strict private
  const
    HeaderLength = 8208;
  strict private
    FIs32: TBytes;
    FGroups: TArray<TdxFontFileCMapGroup>;
  protected
    function GetFormat: TdxFontFileCMapFormatID; override;
    function GetSize: Integer; override;
  public
    constructor Create(APlatformId: TdxFontFilePlatformID;
      AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream); override;
    procedure Write(ATableStream: TdxFontFileStream); override;

    property Is32: TBytes read FIs32;
    property Groups: TArray<TdxFontFileCMapGroup> read FGroups;
  end;

  { TdxFontFileCMapTrimmedArrayRecord }

  TdxFontFileCMapTrimmedArrayRecord = class(TdxFontFileCMapLongRecord)
  strict private
  const
    HeaderLength = 20;
  strict private
    FCharacterCount: Integer;
    FGlyphs: TSmallIntDynArray;
    FStartCharacterCode: Integer;
  protected
    function GetFormat: TdxFontFileCMapFormatID; override;
    function GetSize: Integer; override;
  public
    constructor Create(APlatformId: TdxFontFilePlatformID;
      AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream); override;
    procedure Write(ATableStream: TdxFontFileStream); override;

    property CharacterCount: Integer read FCharacterCount;
    property Glyphs: TSmallIntDynArray read FGlyphs;
    property StartCharacterCode: Integer read FStartCharacterCode;
  end;

  { TdxDefaultUVSTable }

  TdxDefaultUVSTable = class
  strict private
    FAdditionalCount: Byte;
    FStartUnicodeValue: Integer;
  public
    constructor Create(AStartUnicodeValue: Integer; AAdditionalCount: Byte);
    procedure Write(ATableStream: TdxFontFileStream);

    property AdditionalCount: Byte read FAdditionalCount;
    property StartUnicodeValue: Integer read FStartUnicodeValue;
  end;

  TdxDefaultUVSTables = array of TdxDefaultUVSTable;

  { TdxNonDefaultUVSTable }

  TdxNonDefaultUVSTable = class
  strict private
    FGlyphID: SmallInt;
    FUnicodeValue: Integer;
  public
    constructor Create(AUnicodeValue: Integer; AGlyphID: SmallInt);
    procedure Write(ATableStream: TdxFontFileStream);

    property GlyphID: SmallInt read FGlyphID;
    property UnicodeValue: Integer read FUnicodeValue;
  end;

  TdxNonDefaultUVSTables = array of TdxNonDefaultUVSTable;

  { TdxFontFileCMapUnicodeVariationSelectorRecord }

  TdxFontFileCMapUnicodeVariationSelectorRecord = class
  strict private
    FDefaultUVSTables: TdxDefaultUVSTables;
    FNonDefaultUVSTables: TdxNonDefaultUVSTables;
    FVarSelector: Integer;
  public
    constructor Create(AVarSelector: Integer;
      const ADefaultUVSTables: TdxDefaultUVSTables;
      const ANonDefaultUVSTables: TdxNonDefaultUVSTables);
    destructor Destroy; override;
    function Write(ATableStream: TdxFontFileStream; AOffset: Integer): Integer;

    property DefaultUVSTables: TdxDefaultUVSTables read FDefaultUVSTables;
    property NonDefaultUVSTables: TdxNonDefaultUVSTables
      read FNonDefaultUVSTables;
    property VarSelector: Integer read FVarSelector;
  end;

  TdxFontFileCMapUnicodeVariationSelectorRecords = array of
    TdxFontFileCMapUnicodeVariationSelectorRecord;

  { TdxFontFileCMapUnicodeVariationSequenciesRecord }

  TdxFontFileCMapUnicodeVariationSequenciesRecord = class
    (TdxFontFileCMapCustomFormatRecord)
  strict private
  const
    DefaultUVSTableSize = 4;
    HeaderLength = 10;
    NonDefaultUVSTableSize = 5;
    VariationSelectorRecordSize = 11;
  strict private
    FDefaultUVSTableSize: Integer;
    FHeaderLength: Integer;
    FNonDefaultUVSTableSize: Integer;
    FVariationSelectorRecords: TdxFontFileCMapUnicodeVariationSelectorRecords;
    FVariationSelectorRecordSize: Integer;

    class function GetInt24(const AArray: TBytes): Integer; static;
  protected
    function GetFormat: TdxFontFileCMapFormatID; override;
    function GetSize: Integer; override;
  public
    constructor Create(APlatformId: TdxFontFilePlatformID;
      AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream); override;
    destructor Destroy; override;

    procedure Write(ATableStream: TdxFontFileStream); override;

    property VariationSelectorRecords
      : TdxFontFileCMapUnicodeVariationSelectorRecords
      read FVariationSelectorRecords;
  end;

  { TdxFontFileCMapTable }

  TdxFontFileCMapTable = class(TdxFontFileBinaryTable)
  strict private
  type
    TChooseTableFunc = reference to function
      (ATable: TdxFontFileCMapCustomFormatRecord): Boolean;
  strict private
    FCMapTables: TObjectList<TdxFontFileCMapCustomFormatRecord>;
    FMappedGlyphCache: TdxPDFIntegerIntegerDictionary;
    FVersion: SmallInt;
    function CreateRecord(APlatformId: TdxFontFilePlatformID;
      AEncodingId: TdxFontFileEncodingID; AFormat: TdxFontFileCMapFormatID;
      AStream: TdxFontFileStream): TdxFontFileCMapCustomFormatRecord;
    procedure UpdateEncoding(var AEncoding: TSmallIntDynArray;
      AChooseTableFunc: TChooseTableFunc);
  protected
    procedure DoApplyChanges; override;
  public
    constructor Create(const AData: TBytes); overload; override;
    constructor Create(ASegmentMappingFormatEntry
      : TdxFontFileCMapSegmentMappingRecord); overload;
    constructor CreateFromCharset(ACharset
      : TdxPDFSmallIntegerDictionary); overload;
    destructor Destroy; override;

    class function Tag: string; override;

    function Validate(ASkipEncodingValidation: Boolean; AIsSymbolic: Boolean)
      : TdxFontFileCMapSegmentMappingRecord;
    function MapCodes(const AStr: string): TIntegerDynArray;
    function MapCode(ACharacter: Char): Integer;
    procedure PopulateEncoding(var AEncoding: TSmallIntDynArray);

    property CMapTables: TObjectList<TdxFontFileCMapCustomFormatRecord>
      read FCMapTables;
  end;

  { TdxFontFileNameRecord }

  TdxFontFileNameRecord = record
  strict private
    FEncodingId: TdxFontFileEncodingID;
    FLanguageID: TdxFontFileLanguageID;
    FName: string;
    FNameBytes: TBytes;
    FNameID: TdxFontFileNameID;
    FPlatformId: TdxFontFilePlatformID;
  public
    class function Create(APlatformId: TdxFontFilePlatformID;
      ALanguageID: TdxFontFileLanguageID; ANameID: TdxFontFileNameID;
      AEncodingId: TdxFontFileEncodingID; const ANameBytes: TBytes)
      : TdxFontFileNameRecord; overload; static;
    class function Create(AStream: TdxFontFileStream; ADataOffset: Integer)
      : TdxFontFileNameRecord; overload; static;

    property EncodingId: TdxFontFileEncodingID read FEncodingId;
    property LanguageID: TdxFontFileLanguageID read FLanguageID;
    property Name: string read FName;
    property NameBytes: TBytes read FNameBytes;
    property NameID: TdxFontFileNameID read FNameID;
    property PlatformId: TdxFontFilePlatformID read FPlatformId;
  end;

  { TdxFontFileNameTable }

  TdxFontFileNameTable = class(TdxFontFileBinaryTable)
  strict private
  const
    MaxNameLength = 31;
    NameFontSubfamily = 'Regular';
    NameVersion = '0.0';
  strict private
    FFamilyName: string;
    FMacFamilyName: string;
    FNamingTable: TList<TdxFontFileNameRecord>;
    FPostScriptName: string;
    function GetFamilyName: string;
    function GetMacFamilyName: string;
    function GetPostScriptName: string;
  protected
    procedure DoApplyChanges; override;
  public
    constructor Create(const AData: TBytes); overload; override;
    constructor Create(ACMapEntry: TdxFontFileCMapTable;
      const AFontName: string); overload;
    destructor Destroy; override;

    class function Tag: string; override;
    function FindName(APlatform: TdxFontFilePlatformID;
      AEncoding: TdxFontFileEncodingID; ALanguage: TdxFontFileLanguageID;
      AId: TdxFontFileNameID): string;
    procedure AddName(ACMapEntry: TdxFontFileCMapTable;
      const AFontName: string);

    property FamilyName: string read GetFamilyName;
    property MacFamilyName: string read GetMacFamilyName;
    property PostscriptName: string read GetPostScriptName;
  end;

  { TdxFontFileGlyphTable }

  TdxFontFileGlyphTable = class(TdxFontFileBinaryTable)
  strict private
    FGlyphs: TDictionary<Integer, TdxFontFileGlyphDescription>;
    FGlyphOffsets: TIntegerDynArray;
    FSubsetGlyphs: TList<TdxFontFileSubsetGlyph>;
    function Pad4(AValue: Integer): Integer;
    procedure SortSubsetGlyphs;
  protected
    procedure DoApplyChanges; override;
  public
    constructor Create(const AData: TBytes); override;
    destructor Destroy; override;

    class function Tag: string; override;
    function CalculateOffsets(AGlyphCount: Integer): TIntegerDynArray;
    procedure CreateSubset(AFontFile: TdxFontFile;
      AMapping: TdxPDFIntegerStringDictionary);
    procedure ReadGlyphs(AFontFile: TdxFontFile);

    property Glyphs: TDictionary<Integer, TdxFontFileGlyphDescription>
      read FGlyphs;
  end;

  { TdxFontFile }

  TdxFontFile = class
  strict private
  const
    OpenTypeVersion: array [0 .. 3] of Byte = ($4F, $54, $54, $4F);
    TrueTypeFontToFactor = 1000 / 2048;
    TrueTypeVersion: array [0 .. 3] of Byte = ($0, $1, $0, $0);
    TableDirectoryOffset = 12;
  strict private
    FCMap: TdxFontFileCMapTable;
    FHhea: TdxFontFileHheaTable;
    FHmtx: TdxFontFileHmtxTable;
    FInitalFontSize: Int64;
    FKern: TdxFontFileKernTable;
    FTableDictionary: TObjectDictionary<string, TdxFontFileBinaryTable>;
    FTTFToFontFileFactor: Single;
    FVersion: TBytes;

    function GetHeadTable: TdxFontFileHeadTable;
    function GetMaxpTable: TdxFontFileMaxpTable;
    function GetOS2Table: TdxFontFileOS2Table;
    function GetPostTable: TdxFontFilePostTable;
    function GetNameTable: TdxFontFileNameTable;
    function GetLocaTable: TdxFontFileLocaTable;
    function GetGlyphTable: TdxFontFileGlyphTable;
    function GetHheaTable: TdxFontFileHheaTable;
    function GetCMapTable: TdxFontFileCMapTable;
    function GetKernTable: TdxFontFileKernTable;
    function GetHmtxTable: TdxFontFileHmtxTable;

    function CreateTable(const ATag: string; const AArray: TBytes)
      : TdxFontFileBinaryTable;
    function InternalGetData(ATablesToWrite: TStringList): TBytes;
    procedure ReadTables(AStream: TdxFontFileStream);
  protected
    property Table: TObjectDictionary<string, TdxFontFileBinaryTable>
      read FTableDictionary;

    property InitalFontSize: Int64 read FInitalFontSize;
  public
    constructor Create(AStream: TdxFontFileStream); overload;
    constructor Create(const AData: TBytes;
      AIsOpenType: Boolean = False); overload;
    destructor Destroy; override;

    class function GetCFFData(const AFontFileData: TBytes): TBytes; static;
    class function IsEqual(AFontFile1, AFontFile2: TdxFontFile)
      : Boolean; static;
    function CreateSubset(AMapping: TdxPDFIntegerStringDictionary): TBytes;
    function GetCharacterWidth(AGlyphIndex: Integer): Single;
    function GetData: TBytes; overload;
    function IsTrueTypeFont: Boolean;
    procedure AddTable(ATable: TdxFontFileBinaryTable);

    property CMapTable: TdxFontFileCMapTable read GetCMapTable;
    property GlyphTable: TdxFontFileGlyphTable read GetGlyphTable;
    property HeadTable: TdxFontFileHeadTable read GetHeadTable;
    property HheaTable: TdxFontFileHheaTable read GetHheaTable;
    property HmtxTable: TdxFontFileHmtxTable read GetHmtxTable;
    property KernTable: TdxFontFileKernTable read GetKernTable;
    property LocaTable: TdxFontFileLocaTable read GetLocaTable;
    property MaxpTable: TdxFontFileMaxpTable read GetMaxpTable;
    property NameTable: TdxFontFileNameTable read GetNameTable;
    property OS2Table: TdxFontFileOS2Table read GetOS2Table;
    property PostTable: TdxFontFilePostTable read GetPostTable;
  end;

  { TdxFontFileFontMetrics }

  TdxFontFileFontMetrics = record
  strict private
    FAscent: Double;
    FDescent: Double;
    FLineSpacing: Double;
    FEmBBox: TdxRectF;
    function GetEmAscent: Double;
    function GetEmDescent: Double;
  public
    class function Create(AFontFile: TdxFontFile): TdxFontFileFontMetrics;
      overload; static;
    class function Create(AAscent, ADescent, ALineSpacing, AUnitsPerEm: Double)
      : TdxFontFileFontMetrics; overload; static;

    function GetAscent(AFontSize: Double): Double;
    function GetDescent(AFontSize: Double): Double;
    function GetLineSpacing(AFontSize: Double): Double;

    property EmAscent: Double read GetEmAscent;
    property EmDescent: Double read GetEmDescent;
    property EmBBox: TdxRectF read FEmBBox;
  end;

function dxFontFileUnicodeConverter: TdxFontFileUnicodeConverter;

const
  TdxFontStretchToStringMap: array [TdxFontFileStretch] of string =
    ('UltraCondensed', 'ExtraCondensed', 'Condensed', 'SemiCondensed', 'Normal',
    'SemiExpanded', 'Expanded', 'ExtraExpanded', 'UltraExpanded');
  // for internal use

implementation

uses
  Math, dxTypeHelpers, dxStringHelper, dxGenerics, dxPDFTypes, dxPDFUtils;

type
  TEncodingPair = record
    Count: SmallInt;
    Code: SmallInt;
  end;

  TdxFontFileTableClass = class of TdxFontFileBinaryTable;

var
  dxgFontFileUnicodeConverter: TdxFontFileUnicodeConverter;
  dxgFontFileSupportedTables: TDictionary<string, TdxFontFileTableClass>;

function dxFontFileUnicodeConverter: TdxFontFileUnicodeConverter;
begin
  Result := dxgFontFileUnicodeConverter;
end;

{ TdxFontFilePanose }

class function TdxFontFilePanose.Create(AStream: TdxFontFileStream)
  : TdxFontFilePanose;
var
  AData: TBytes;
begin
  AData := AStream.ReadArray(10);
  Result.FamilyKind := TdxFontFilePanoseFamilyKind(AData[0]);
  Result.SerifStyle := TdxFontFilePanoseSerifStyle(AData[1]);
  Result.Weight := TdxFontFilePanoseWeight(AData[2]);
  Result.Proportion := TdxFontFilePanoseProportion(AData[3]);
  Result.Contrast := TdxFontFilePanoseContrast(AData[4]);
  Result.StrokeVariation := TdxFontFilePanoseStrokeVariation(AData[5]);
  Result.ArmStyle := TdxFontFilePanoseArmStyle(AData[6]);
  Result.LetterForm := TdxFontFilePanoseLetterForm(AData[7]);
  Result.Midline := TdxFontFilePanoseMidline(AData[8]);
  Result.XHeight := TdxFontFilePanoseXHeight(AData[9]);
end;

function TdxFontFilePanose.IsDefault: Boolean;
begin
  Result := (FamilyKind = fkAny) and (SerifStyle = ssAny) and (Weight = wAny)
    and (Proportion = pAny) and (Contrast = cAny) and (StrokeVariation = svAny)
    and (ArmStyle = asAny) and (LetterForm = lfAny) and (Midline = mAny) and
    (XHeight = xhAny);
end;

procedure TdxFontFilePanose.Write(AStream: TdxFontFileStream);
var
  AData: TBytes;
begin
  SetLength(AData, 10);
  AData[0] := Byte(FamilyKind);
  AData[1] := Byte(SerifStyle);
  AData[2] := Byte(Weight);
  AData[3] := Byte(Proportion);
  AData[4] := Byte(Contrast);
  AData[5] := Byte(StrokeVariation);
  AData[6] := Byte(ArmStyle);
  AData[7] := Byte(LetterForm);
  AData[8] := Byte(Midline);
  AData[9] := Byte(XHeight);
  AStream.WriteArray(AData);
end;

{ TdxType1FontGlyphMapping }

constructor TdxType1FontGlyphMapping.Create;
begin
  inherited Create;
end;

constructor TdxType1FontGlyphMapping.Create
  (AMapping: TdxPDFStringSmallIntegerDictionary;
  AEncoding: TdxPDFByteStringDictionary);
var
  AMappingPair: TPair<string, SmallInt>;
  AEncodingPair: TPair<Byte, string>;
begin
  Create;
  if AMapping <> nil then
  begin
    FMapping := TdxPDFStringSmallIntegerDictionary.Create;
    for AMappingPair in AMapping do
      FMapping.Add(AMappingPair.Key, AMappingPair.Value);
  end;
  if AEncoding <> nil then
  begin
    FEncoding := TdxPDFByteStringDictionary.Create;
    for AEncodingPair in AEncoding do
      FEncoding.Add(AEncodingPair.Key, AEncodingPair.Value);
  end;
end;

destructor TdxType1FontGlyphMapping.Destroy;
begin
  FreeAndNil(FMapping);
  FreeAndNil(FEncoding);
  inherited Destroy;
end;

{ TdxFontFileCustomEncoding }

class function TdxFontFileCustomEncoding.GetName: string;
begin
  Result := '';
end;

constructor TdxFontFileCustomEncoding.Create;
begin
  inherited Create;
  FDictionary := TdxPDFByteStringDictionary.Create;
  Initialize;
end;

destructor TdxFontFileCustomEncoding.Destroy;
begin
  FreeAndNil(FDictionary);
  inherited Destroy;
end;

procedure TdxFontFileCustomEncoding.Initialize;
begin
  // do nothing;
end;

{ TdxFontFileUnicodeConverter }

constructor TdxFontFileUnicodeConverter.Create;
begin
  inherited Create;
  FGlyphCodes := TdxPDFWordDictionary.Create;
  InitializePack1;
  InitializePack2;
  InitializePack3;
  InitializePack4;
  FGlyphCodes.TrimExcess;
end;

destructor TdxFontFileUnicodeConverter.Destroy;
begin
  FreeAndNil(FGlyphCodes);
  inherited Destroy;
end;

function TdxFontFileUnicodeConverter.FindCode(const AName: string;
  out ACode: Word): Boolean;

  function HexToSmallInt(const AValue: string): SmallInt;
  begin
    Result := SmallInt(StrToInt('$' + AValue));
  end;

var
  L: Integer;
begin
  Result := FGlyphCodes.TryGetValue(AName, ACode);
  if not Result then
  begin
    L := Length(AName);
    Result := (L = 7) and TdxStringHelper.StartsWith(AName, 'uni');
    if Result then
      ACode := HexToSmallInt(TdxStringHelper.Substring(AName, 3))
    else
    begin
      Result := (L = 5) and TdxStringHelper.StartsWith(AName, 'u');
      if Result then
        ACode := HexToSmallInt(TdxStringHelper.Substring(AName, 1));
    end;
  end;
end;

function TdxFontFileUnicodeConverter.FindCode
  (AEncoding: TdxFontFileCustomEncoding; ACode: Word;
  out AResult: Word): Boolean;
begin
  Result := InternalFindCode(ACode, AEncoding, FGlyphCodes, AResult);
end;

function TdxFontFileUnicodeConverter.FindCode
  (AEncoding: TdxFontFileCustomEncoding; AGlyphCodes: TdxPDFWordDictionary;
  ACode: Word; out AResult: Word): Boolean;
begin
  Result := InternalFindCode(ACode, AEncoding, AGlyphCodes, AResult);
end;

function TdxFontFileUnicodeConverter.InternalFindCode(ACode: Word;
  AEncoding: TdxFontFileCustomEncoding; AGlyphCodes: TdxPDFWordDictionary;
  out AResult: Word): Boolean;
var
  AName: string;
begin
  if not AEncoding.Dictionary.TryGetValue(ACode, AName) then
    AName := TdxGlyphNames._notdef;
  Result := AGlyphCodes.TryGetValue(AName, AResult);
  if not Result and (AName = TdxGlyphNames.Zdotaccent) then
  begin
    Result := AGlyphCodes.TryGetValue(TdxGlyphNames.Zdot, AResult);
    if not Result and (AName = TdxGlyphNames.LowerZdotaccent) then
    begin
      Result := AGlyphCodes.TryGetValue(TdxGlyphNames.Zdot, AResult);
      if not Result then
        AResult := ACode;
    end;
  end;
end;

procedure TdxFontFileUnicodeConverter.InitializePack1;
begin
  FGlyphCodes.Add('A', 65);
  FGlyphCodes.Add('AE', 198);
  FGlyphCodes.Add('AEacute', 508);
  FGlyphCodes.Add('AEmacron', 482);
  FGlyphCodes.Add('AEsmall', 63462);
  FGlyphCodes.Add('Aacute', 193);
  FGlyphCodes.Add('Aacutesmall', 63457);
  FGlyphCodes.Add('Abreve', 258);
  FGlyphCodes.Add('Abreveacute', 7854);
  FGlyphCodes.Add('Abrevecyrillic', 1232);
  FGlyphCodes.Add('Abrevedotbelow', 7862);
  FGlyphCodes.Add('Abrevegrave', 7856);
  FGlyphCodes.Add('Abrevehookabove', 7858);
  FGlyphCodes.Add('Abrevetilde', 7860);
  FGlyphCodes.Add('Acaron', 461);
  FGlyphCodes.Add('Acircle', 9398);
  FGlyphCodes.Add('Acircumflex', 194);
  FGlyphCodes.Add('Acircumflexacute', 7844);
  FGlyphCodes.Add('Acircumflexdotbelow', 7852);
  FGlyphCodes.Add('Acircumflexgrave', 7846);
  FGlyphCodes.Add('Acircumflexhookabove', 7848);
  FGlyphCodes.Add('Acircumflexsmall', 63458);
  FGlyphCodes.Add('Acircumflextilde', 7850);
  FGlyphCodes.Add('Acute', 63177);
  FGlyphCodes.Add('Acutesmall', 63412);
  FGlyphCodes.Add('Acyrillic', 1040);
  FGlyphCodes.Add('Adblgrave', 512);
  FGlyphCodes.Add('Adieresis', 196);
  FGlyphCodes.Add('Adieresiscyrillic', 1234);
  FGlyphCodes.Add('Adieresismacron', 478);
  FGlyphCodes.Add('Adieresissmall', 63460);
  FGlyphCodes.Add('Adotbelow', 7840);
  FGlyphCodes.Add('Adotmacron', 480);
  FGlyphCodes.Add('Agrave', 192);
  FGlyphCodes.Add('Agravesmall', 63456);
  FGlyphCodes.Add('Ahookabove', 7842);
  FGlyphCodes.Add('Aiecyrillic', 1236);
  FGlyphCodes.Add('Ainvertedbreve', 514);
  FGlyphCodes.Add('Alpha', 913);
  FGlyphCodes.Add('Alphatonos', 902);
  FGlyphCodes.Add('Amacron', 256);
  FGlyphCodes.Add('Amonospace', 65313);
  FGlyphCodes.Add('Aogonek', 260);
  FGlyphCodes.Add('Aring', 197);
  FGlyphCodes.Add('Aringacute', 506);
  FGlyphCodes.Add('Aringbelow', 7680);
  FGlyphCodes.Add('Aringsmall', 63461);
  FGlyphCodes.Add('Asmall', 63329);
  FGlyphCodes.Add('Atilde', 195);
  FGlyphCodes.Add('Atildesmall', 63459);
  FGlyphCodes.Add('Aybarmenian', 1329);
  FGlyphCodes.Add('B', 66);
  FGlyphCodes.Add('Bcircle', 9399);
  FGlyphCodes.Add('Bdotaccent', 7682);
  FGlyphCodes.Add('Bdotbelow', 7684);
  FGlyphCodes.Add('Becyrillic', 1041);
  FGlyphCodes.Add('Benarmenian', 1330);
  FGlyphCodes.Add('Beta', 914);
  FGlyphCodes.Add('Bhook', 385);
  FGlyphCodes.Add('Blinebelow', 7686);
  FGlyphCodes.Add('Bmonospace', 65314);
  FGlyphCodes.Add('Brevesmall', 63220);
  FGlyphCodes.Add('Bsmall', 63330);
  FGlyphCodes.Add('Btopbar', 386);
  FGlyphCodes.Add('C', 67);
  FGlyphCodes.Add('Caarmenian', 1342);
  FGlyphCodes.Add('Cacute', 262);
  FGlyphCodes.Add('Caron', 63178);
  FGlyphCodes.Add('Caronsmall', 63221);
  FGlyphCodes.Add('Ccaron', 268);
  FGlyphCodes.Add('Ccedilla', 199);
  FGlyphCodes.Add('Ccedillaacute', 7688);
  FGlyphCodes.Add('Ccedillasmall', 63463);
  FGlyphCodes.Add('Ccircle', 9400);
  FGlyphCodes.Add('Ccircumflex', 264);
  FGlyphCodes.Add('Cdot', 266);
  FGlyphCodes.Add('Cdotaccent', 266);
  FGlyphCodes.Add('Cedillasmall', 63416);
  FGlyphCodes.Add('Chaarmenian', 1353);
  FGlyphCodes.Add('Cheabkhasiancyrillic', 1212);
  FGlyphCodes.Add('Checyrillic', 1063);
  FGlyphCodes.Add('Chedescenderabkhasiancyrillic', 1214);
  FGlyphCodes.Add('Chedescendercyrillic', 1206);
  FGlyphCodes.Add('Chedieresiscyrillic', 1268);
  FGlyphCodes.Add('Cheharmenian', 1347);
  FGlyphCodes.Add('Chekhakassiancyrillic', 1227);
  FGlyphCodes.Add('Cheverticalstrokecyrillic', 1208);
  FGlyphCodes.Add('Chi', 935);
  FGlyphCodes.Add('Chook', 391);
  FGlyphCodes.Add('Circumflexsmall', 63222);
  FGlyphCodes.Add('Cmonospace', 65315);
  FGlyphCodes.Add('Coarmenian', 1361);
  FGlyphCodes.Add('D', 68);
  FGlyphCodes.Add('DZ', 497);
  FGlyphCodes.Add('DZcaron', 452);
  FGlyphCodes.Add('Daarmenian', 1332);
  FGlyphCodes.Add('Dafrican', 393);
  FGlyphCodes.Add('Dcaron', 270);
  FGlyphCodes.Add('Dcedilla', 7696);
  FGlyphCodes.Add('Dcircle', 9401);
  FGlyphCodes.Add('Dcircumflexbelow', 7698);
  FGlyphCodes.Add('Dcroat', 272);
  FGlyphCodes.Add('Ddotaccent', 7690);
  FGlyphCodes.Add('Ddotbelow', 7692);
  FGlyphCodes.Add('Decyrillic', 1044);
  FGlyphCodes.Add('Deicoptic', 1006);
  FGlyphCodes.Add('Delta', 8710);
  FGlyphCodes.Add('Deltagreek', 916);
  FGlyphCodes.Add('Dhook', 394);
  FGlyphCodes.Add('Dieresis', 63179);
  FGlyphCodes.Add('DieresisAcute', 63180);
  FGlyphCodes.Add('DieresisGrave', 63181);
  FGlyphCodes.Add('Dieresissmall', 63400);
  FGlyphCodes.Add('Digammagreek', 988);
  FGlyphCodes.Add('Djecyrillic', 1026);
  FGlyphCodes.Add('Dlinebelow', 7694);
  FGlyphCodes.Add('Dmonospace', 65316);
  FGlyphCodes.Add('Dotaccentsmall', 63223);
  FGlyphCodes.Add('Dslash', 272);
  FGlyphCodes.Add('Dsmall', 63332);
  FGlyphCodes.Add('Dtopbar', 395);
  FGlyphCodes.Add('Dz', 498);
  FGlyphCodes.Add('Dzcaron', 453);
  FGlyphCodes.Add('Dzeabkhasiancyrillic', 1248);
  FGlyphCodes.Add('Dzecyrillic', 1029);
  FGlyphCodes.Add('Dzhecyrillic', 1039);
  FGlyphCodes.Add('E', 69);
  FGlyphCodes.Add('Eacute', 201);
  FGlyphCodes.Add('Eacutesmall', 63465);
  FGlyphCodes.Add('Ebreve', 276);
  FGlyphCodes.Add('Ecaron', 282);
  FGlyphCodes.Add('Ecedillabreve', 7708);
  FGlyphCodes.Add('Echarmenian', 1333);
  FGlyphCodes.Add('Ecircle', 9402);
  FGlyphCodes.Add('Ecircumflex', 202);
  FGlyphCodes.Add('Ecircumflexacute', 7870);
  FGlyphCodes.Add('Ecircumflexbelow', 7704);
  FGlyphCodes.Add('Ecircumflexdotbelow', 7878);
  FGlyphCodes.Add('Ecircumflexgrave', 7872);
  FGlyphCodes.Add('Ecircumflexhookabove', 7874);
  FGlyphCodes.Add('Ecircumflexsmall', 63466);
  FGlyphCodes.Add('Ecircumflextilde', 7876);
  FGlyphCodes.Add('Ecyrillic', 1028);
  FGlyphCodes.Add('Edblgrave', 516);
  FGlyphCodes.Add('Edieresis', 203);
  FGlyphCodes.Add('Edieresissmall', 63467);
  FGlyphCodes.Add('Edot', 278);
  FGlyphCodes.Add('Edotaccent', 278);
  FGlyphCodes.Add('Edotbelow', 7864);
  FGlyphCodes.Add('Efcyrillic', 1060);
  FGlyphCodes.Add('Egrave', 200);
  FGlyphCodes.Add('Egravesmall', 63464);
  FGlyphCodes.Add('Eharmenian', 1335);
  FGlyphCodes.Add('Ehookabove', 7866);
  FGlyphCodes.Add('Eightroman', 8551);
  FGlyphCodes.Add('Einvertedbreve', 518);
  FGlyphCodes.Add('Eiotifiedcyrillic', 1124);
  FGlyphCodes.Add('Elcyrillic', 1051);
  FGlyphCodes.Add('Elevenroman', 8554);
  FGlyphCodes.Add('Emacron', 274);
  FGlyphCodes.Add('Emacronacute', 7702);
  FGlyphCodes.Add('Emacrongrave', 7700);
  FGlyphCodes.Add('Emcyrillic', 1052);
  FGlyphCodes.Add('Emonospace', 65317);
  FGlyphCodes.Add('Encyrillic', 1053);
  FGlyphCodes.Add('Endescendercyrillic', 1186);
  FGlyphCodes.Add('Eng', 330);
  FGlyphCodes.Add('Enghecyrillic', 1188);
  FGlyphCodes.Add('Enhookcyrillic', 1223);
  FGlyphCodes.Add('Eogonek', 280);
  FGlyphCodes.Add('Eopen', 400);
  FGlyphCodes.Add('Epsilon', 917);
  FGlyphCodes.Add('Epsilontonos', 904);
  FGlyphCodes.Add('Ercyrillic', 1056);
  FGlyphCodes.Add('Ereversed', 398);
  FGlyphCodes.Add('Ereversedcyrillic', 1069);
  FGlyphCodes.Add('Escyrillic', 1057);
  FGlyphCodes.Add('Esdescendercyrillic', 1194);
  FGlyphCodes.Add('Esh', 425);
  FGlyphCodes.Add('Esmall', 63333);
  FGlyphCodes.Add('Eta', 919);
  FGlyphCodes.Add('Etarmenian', 1336);
  FGlyphCodes.Add('Etatonos', 905);
  FGlyphCodes.Add('Eth', 208);
  FGlyphCodes.Add('Ethsmall', 63472);
  FGlyphCodes.Add('Etilde', 7868);
  FGlyphCodes.Add('Etildebelow', 7706);
  FGlyphCodes.Add('Euro', 8364);
  FGlyphCodes.Add('Ezh', 439);
  FGlyphCodes.Add('Ezhcaron', 494);
  FGlyphCodes.Add('Ezhreversed', 440);
  FGlyphCodes.Add('F', 70);
  FGlyphCodes.Add('Fcircle', 9403);
  FGlyphCodes.Add('Fdotaccent', 7710);
  FGlyphCodes.Add('Feharmenian', 1366);
  FGlyphCodes.Add('Feicoptic', 996);
  FGlyphCodes.Add('Fhook', 401);
  FGlyphCodes.Add('Fitacyrillic', 1138);
  FGlyphCodes.Add('Fiveroman', 8548);
  FGlyphCodes.Add('Fmonospace', 65318);
  FGlyphCodes.Add('Fsmall', 63334);
  FGlyphCodes.Add('G', 71);
  FGlyphCodes.Add('GBsquare', 13191);
  FGlyphCodes.Add('Gacute', 500);
  FGlyphCodes.Add('Gamma', 915);
  FGlyphCodes.Add('Gammaafrican', 404);
  FGlyphCodes.Add('Gangiacoptic', 1002);
  FGlyphCodes.Add('Gbreve', 286);
  FGlyphCodes.Add('Gcaron', 486);
  FGlyphCodes.Add('Gcedilla', 290);
  FGlyphCodes.Add('Gcircle', 9404);
  FGlyphCodes.Add('Gcircumflex', 284);
  FGlyphCodes.Add('Gcommaaccent', 290);
  FGlyphCodes.Add('Gdot', 288);
  FGlyphCodes.Add('Gdotaccent', 288);
  FGlyphCodes.Add('Gecyrillic', 1043);
  FGlyphCodes.Add('Ghemiddlehookcyrillic', 1172);
  FGlyphCodes.Add('Ghestrokecyrillic', 1170);
  FGlyphCodes.Add('Gheupturncyrillic', 1168);
  FGlyphCodes.Add('Ghook', 403);
  FGlyphCodes.Add('Gimarmenian', 1331);
  FGlyphCodes.Add('Gjecyrillic', 1027);
  FGlyphCodes.Add('Gmacron', 7712);
  FGlyphCodes.Add('Gmonospace', 65319);
  FGlyphCodes.Add('Grave', 63182);
  FGlyphCodes.Add('Gravesmall', 63328);
  FGlyphCodes.Add('Gsmall', 63335);
  FGlyphCodes.Add('Gsmallhook', 667);
  FGlyphCodes.Add('Gstroke', 484);
  FGlyphCodes.Add('H', 72);
  FGlyphCodes.Add('H18533', 9679);
  FGlyphCodes.Add('H18543', 9642);
  FGlyphCodes.Add('H18551', 9643);
  FGlyphCodes.Add('H22073', 9633);
  FGlyphCodes.Add('HPsquare', 13259);
  FGlyphCodes.Add('Haabkhasiancyrillic', 1192);
  FGlyphCodes.Add('Hadescendercyrillic', 1202);
  FGlyphCodes.Add('Hardsigncyrillic', 1066);
  FGlyphCodes.Add('Hbar', 294);
  FGlyphCodes.Add('Hbrevebelow', 7722);
  FGlyphCodes.Add('Hcedilla', 7720);
  FGlyphCodes.Add('Hcircle', 9405);
  FGlyphCodes.Add('Hcircumflex', 292);
  FGlyphCodes.Add('Hdieresis', 7718);
  FGlyphCodes.Add('Hdotaccent', 7714);
  FGlyphCodes.Add('Hdotbelow', 7716);
  FGlyphCodes.Add('Hmonospace', 65320);
  FGlyphCodes.Add('Hoarmenian', 1344);
  FGlyphCodes.Add('Horicoptic', 1000);
  FGlyphCodes.Add('Hsmall', 63336);
  FGlyphCodes.Add('Hungarumlaut', 63183);
  FGlyphCodes.Add('Hungarumlautsmall', 63224);
  FGlyphCodes.Add('Hzsquare', 13200);
  FGlyphCodes.Add('I', 73);
  FGlyphCodes.Add('IAcyrillic', 1071);
  FGlyphCodes.Add('IJ', 306);
  FGlyphCodes.Add('IUcyrillic', 1070);
  FGlyphCodes.Add('Iacute', 205);
  FGlyphCodes.Add('Iacutesmall', 63469);
  FGlyphCodes.Add('Ibreve', 300);
  FGlyphCodes.Add('Icaron', 463);
  FGlyphCodes.Add('Icircle', 9406);
  FGlyphCodes.Add('Icircumflex', 206);
  FGlyphCodes.Add('Icircumflexsmall', 63470);
  FGlyphCodes.Add('Icyrillic', 1030);
  FGlyphCodes.Add('Idblgrave', 520);
  FGlyphCodes.Add('Idieresis', 207);
  FGlyphCodes.Add('Idieresisacute', 7726);
  FGlyphCodes.Add('Idieresiscyrillic', 1252);
  FGlyphCodes.Add('Idieresissmall', 63471);
  FGlyphCodes.Add('Idot', 304);
  FGlyphCodes.Add('Idotaccent', 304);
  FGlyphCodes.Add('Idotbelow', 7882);
  FGlyphCodes.Add('Iebrevecyrillic', 1238);
  FGlyphCodes.Add('Iecyrillic', 1045);
  FGlyphCodes.Add('Ifraktur', 8465);
  FGlyphCodes.Add('Igrave', 204);
  FGlyphCodes.Add('Igravesmall', 63468);
  FGlyphCodes.Add('Ihookabove', 7880);
  FGlyphCodes.Add('Iicyrillic', 1048);
  FGlyphCodes.Add('Iinvertedbreve', 522);
  FGlyphCodes.Add('Iishortcyrillic', 1049);
  FGlyphCodes.Add('Imacron', 298);
  FGlyphCodes.Add('Imacroncyrillic', 1250);
  FGlyphCodes.Add('Imonospace', 65321);
  FGlyphCodes.Add('Iniarmenian', 1339);
  FGlyphCodes.Add('Iocyrillic', 1025);
  FGlyphCodes.Add('Iogonek', 302);
  FGlyphCodes.Add('Iota', 921);
  FGlyphCodes.Add('Iotaafrican', 406);
  FGlyphCodes.Add('Iotadieresis', 938);
  FGlyphCodes.Add('Iotatonos', 906);
  FGlyphCodes.Add('Ismall', 63337);
  FGlyphCodes.Add('Istroke', 407);
  FGlyphCodes.Add('Itilde', 296);
  FGlyphCodes.Add('Itildebelow', 7724);
  FGlyphCodes.Add('Izhitsacyrillic', 1140);
  FGlyphCodes.Add('Izhitsadblgravecyrillic', 1142);
  FGlyphCodes.Add('J', 74);
  FGlyphCodes.Add('Jaarmenian', 1345);
  FGlyphCodes.Add('Jcircle', 9407);
  FGlyphCodes.Add('Jcircumflex', 308);
  FGlyphCodes.Add('Jecyrillic', 1032);
  FGlyphCodes.Add('Jheharmenian', 1355);
  FGlyphCodes.Add('Jmonospace', 65322);
  FGlyphCodes.Add('Jsmall', 63338);
  FGlyphCodes.Add('K', 75);
  FGlyphCodes.Add('KBsquare', 13189);
  FGlyphCodes.Add('KKsquare', 13261);
  FGlyphCodes.Add('Kabashkircyrillic', 1184);
  FGlyphCodes.Add('Kacute', 7728);
  FGlyphCodes.Add('Kacyrillic', 1050);
  FGlyphCodes.Add('Kadescendercyrillic', 1178);
  FGlyphCodes.Add('Kahookcyrillic', 1219);
  FGlyphCodes.Add('Kappa', 922);
  FGlyphCodes.Add('Kastrokecyrillic', 1182);
  FGlyphCodes.Add('Kaverticalstrokecyrillic', 1180);
  FGlyphCodes.Add('Kcaron', 488);
  FGlyphCodes.Add('Kcedilla', 310);
  FGlyphCodes.Add('Kcircle', 9408);
  FGlyphCodes.Add('Kcommaaccent', 310);
  FGlyphCodes.Add('Kdotbelow', 7730);
  FGlyphCodes.Add('Keharmenian', 1364);
  FGlyphCodes.Add('Kenarmenian', 1343);
  FGlyphCodes.Add('Khacyrillic', 1061);
  FGlyphCodes.Add('Kheicoptic', 998);
  FGlyphCodes.Add('Khook', 408);
  FGlyphCodes.Add('Kjecyrillic', 1036);
  FGlyphCodes.Add('Klinebelow', 7732);
  FGlyphCodes.Add('Kmonospace', 65323);
  FGlyphCodes.Add('Koppacyrillic', 1152);
  FGlyphCodes.Add('Koppagreek', 990);
  FGlyphCodes.Add('Ksicyrillic', 1134);
  FGlyphCodes.Add('Ksmall', 63339);
  FGlyphCodes.Add('L', 76);
  FGlyphCodes.Add('LJ', 455);
  FGlyphCodes.Add('LL', 63167);
  FGlyphCodes.Add('Lacute', 313);
  FGlyphCodes.Add('Lambda', 923);
  FGlyphCodes.Add('Lcaron', 317);
  FGlyphCodes.Add('Lcedilla', 315);
  FGlyphCodes.Add('Lcircle', 9409);
  FGlyphCodes.Add('Lcircumflexbelow', 7740);
  FGlyphCodes.Add('Lcommaaccent', 315);
  FGlyphCodes.Add('Ldot', 319);
  FGlyphCodes.Add('Ldotaccent', 319);
  FGlyphCodes.Add('Ldotbelow', 7734);
  FGlyphCodes.Add('Ldotbelowmacron', 7736);
  FGlyphCodes.Add('Liwnarmenian', 1340);
  FGlyphCodes.Add('Lj', 456);
  FGlyphCodes.Add('Ljecyrillic', 1033);
  FGlyphCodes.Add('Llinebelow', 7738);
  FGlyphCodes.Add('Lmonospace', 65324);
  FGlyphCodes.Add('Lslash', 321);
  FGlyphCodes.Add('Lslashsmall', 63225);
  FGlyphCodes.Add('Lsmall', 63340);
  FGlyphCodes.Add('M', 77);
  FGlyphCodes.Add('MBsquare', 13190);
  FGlyphCodes.Add('Macron', 63184);
  FGlyphCodes.Add('Macronsmall', 63407);
  FGlyphCodes.Add('Macute', 7742);
  FGlyphCodes.Add('Mcircle', 9410);
  FGlyphCodes.Add('Mdotaccent', 7744);
  FGlyphCodes.Add('Mdotbelow', 7746);
  FGlyphCodes.Add('Menarmenian', 1348);
  FGlyphCodes.Add('Mmonospace', 65325);
  FGlyphCodes.Add('Msmall', 63341);
  FGlyphCodes.Add('Mturned', 412);
  FGlyphCodes.Add('Mu', 924);
  FGlyphCodes.Add('N', 78);
  FGlyphCodes.Add('NJ', 458);
  FGlyphCodes.Add('Nacute', 323);
  FGlyphCodes.Add('Ncaron', 327);
  FGlyphCodes.Add('Ncedilla', 325);
  FGlyphCodes.Add('Ncircle', 9411);
  FGlyphCodes.Add('Ncircumflexbelow', 7754);
  FGlyphCodes.Add('Ncommaaccent', 325);
  FGlyphCodes.Add('Ndotaccent', 7748);
  FGlyphCodes.Add('Ndotbelow', 7750);
  FGlyphCodes.Add('Nhookleft', 413);
  FGlyphCodes.Add('Nineroman', 8552);
  FGlyphCodes.Add('Nj', 459);
  FGlyphCodes.Add('Njecyrillic', 1034);
  FGlyphCodes.Add('Nlinebelow', 7752);
  FGlyphCodes.Add('Nmonospace', 65326);
  FGlyphCodes.Add('Nowarmenian', 1350);
  FGlyphCodes.Add('Nsmall', 63342);
  FGlyphCodes.Add('Ntilde', 209);
  FGlyphCodes.Add('Ntildesmall', 63473);
  FGlyphCodes.Add('Nu', 925);
  FGlyphCodes.Add('O', 79);
  FGlyphCodes.Add('OE', 338);
  FGlyphCodes.Add('OEsmall', 63226);
  FGlyphCodes.Add('Oacute', 211);
  FGlyphCodes.Add('Oacutesmall', 63475);
  FGlyphCodes.Add('Obarredcyrillic', 1256);
  FGlyphCodes.Add('Obarreddieresiscyrillic', 1258);
  FGlyphCodes.Add('Obreve', 334);
  FGlyphCodes.Add('Ocaron', 465);
  FGlyphCodes.Add('Ocenteredtilde', 415);
  FGlyphCodes.Add('Ocircle', 9412);
  FGlyphCodes.Add('Ocircumflex', 212);
  FGlyphCodes.Add('Ocircumflexacute', 7888);
  FGlyphCodes.Add('Ocircumflexdotbelow', 7896);
  FGlyphCodes.Add('Ocircumflexgrave', 7890);
  FGlyphCodes.Add('Ocircumflexhookabove', 7892);
  FGlyphCodes.Add('Ocircumflexsmall', 63476);
  FGlyphCodes.Add('Ocircumflextilde', 7894);
  FGlyphCodes.Add('Ocyrillic', 1054);
  FGlyphCodes.Add('Odblacute', 336);
  FGlyphCodes.Add('Odblgrave', 524);
  FGlyphCodes.Add('Odieresis', 214);
  FGlyphCodes.Add('Odieresiscyrillic', 1254);
  FGlyphCodes.Add('Odieresissmall', 63478);
  FGlyphCodes.Add('Odotbelow', 7884);
  FGlyphCodes.Add('Ogoneksmall', 63227);
  FGlyphCodes.Add('Ograve', 210);
  FGlyphCodes.Add('Ogravesmall', 63474);
  FGlyphCodes.Add('Oharmenian', 1365);
  FGlyphCodes.Add('Ohm', 8486);
  FGlyphCodes.Add('Ohookabove', 7886);
  FGlyphCodes.Add('Ohorn', 416);
  FGlyphCodes.Add('Ohornacute', 7898);
  FGlyphCodes.Add('Ohorndotbelow', 7906);
  FGlyphCodes.Add('Ohorngrave', 7900);
  FGlyphCodes.Add('Ohornhookabove', 7902);
  FGlyphCodes.Add('Ohorntilde', 7904);
  FGlyphCodes.Add('Ohungarumlaut', 336);
  FGlyphCodes.Add('Oi', 418);
  FGlyphCodes.Add('Oinvertedbreve', 526);
  FGlyphCodes.Add('Omacron', 332);
  FGlyphCodes.Add('Omacronacute', 7762);
  FGlyphCodes.Add('Omacrongrave', 7760);
  FGlyphCodes.Add('Omega', 8486);
  FGlyphCodes.Add('Omegacyrillic', 1120);
  FGlyphCodes.Add('Omegagreek', 937);
  FGlyphCodes.Add('Omegaroundcyrillic', 1146);
  FGlyphCodes.Add('Omegatitlocyrillic', 1148);
  FGlyphCodes.Add('Omegatonos', 911);
  FGlyphCodes.Add('Omicron', 927);
  FGlyphCodes.Add('Omicrontonos', 908);
  FGlyphCodes.Add('Omonospace', 65327);
  FGlyphCodes.Add('Oneroman', 8544);
  FGlyphCodes.Add('Oogonek', 490);
  FGlyphCodes.Add('Oogonekmacron', 492);
  FGlyphCodes.Add('Oopen', 390);
  FGlyphCodes.Add('Oslash', 216);
  FGlyphCodes.Add('Oslashacute', 510);
  FGlyphCodes.Add('Oslashsmall', 63480);
  FGlyphCodes.Add('Osmall', 63343);
  FGlyphCodes.Add('Ostrokeacute', 510);
  FGlyphCodes.Add('Otcyrillic', 1150);
  FGlyphCodes.Add('Otilde', 213);
  FGlyphCodes.Add('Otildeacute', 7756);
  FGlyphCodes.Add('Otildedieresis', 7758);
  FGlyphCodes.Add('Otildesmall', 63477);
  FGlyphCodes.Add('P', 80);
  FGlyphCodes.Add('Pacute', 7764);
  FGlyphCodes.Add('Pcircle', 9413);
  FGlyphCodes.Add('Pdotaccent', 7766);
  FGlyphCodes.Add('Pecyrillic', 1055);
  FGlyphCodes.Add('Peharmenian', 1354);
  FGlyphCodes.Add('Pemiddlehookcyrillic', 1190);
  FGlyphCodes.Add('Phi', 934);
  FGlyphCodes.Add('Phook', 420);
  FGlyphCodes.Add('Pi', 928);
  FGlyphCodes.Add('Pmonospace', 65328);
  FGlyphCodes.Add('Psi', 936);
  FGlyphCodes.Add('Psicyrillic', 1136);
  FGlyphCodes.Add('Psmall', 63344);
  FGlyphCodes.Add('Q', 81);
  FGlyphCodes.Add('Qcircle', 9414);
  FGlyphCodes.Add('Qmonospace', 65329);
  FGlyphCodes.Add('Qsmall', 63345);
  FGlyphCodes.Add('R', 82);
  FGlyphCodes.Add('Raarmenian', 1356);
  FGlyphCodes.Add('Racute', 340);
  FGlyphCodes.Add('Rcaron', 344);
  FGlyphCodes.Add('Rcedilla', 342);
  FGlyphCodes.Add('Rcircle', 9415);
  FGlyphCodes.Add('Rcommaaccent', 342);
  FGlyphCodes.Add('Rdblgrave', 528);
  FGlyphCodes.Add('Rdotaccent', 7768);
  FGlyphCodes.Add('Rdotbelow', 7770);
  FGlyphCodes.Add('Rdotbelowmacron', 7772);
  FGlyphCodes.Add('Reharmenian', 1360);
  FGlyphCodes.Add('Rfraktur', 8476);
  FGlyphCodes.Add('Rho', 929);
  FGlyphCodes.Add('Ringsmall', 63228);
  FGlyphCodes.Add('Rinvertedbreve', 530);
  FGlyphCodes.Add('Rlinebelow', 7774);
  FGlyphCodes.Add('Rmonospace', 65330);
  FGlyphCodes.Add('Rsmall', 63346);
  FGlyphCodes.Add('Rsmallinverted', 641);
  FGlyphCodes.Add('Rsmallinvertedsuperior', 694);
  FGlyphCodes.Add('S', 83);
  FGlyphCodes.Add('SF010000', 9484);
  FGlyphCodes.Add('SF020000', 9492);
  FGlyphCodes.Add('SF030000', 9488);
  FGlyphCodes.Add('SF040000', 9496);
  FGlyphCodes.Add('SF050000', 9532);
  FGlyphCodes.Add('SF060000', 9516);
  FGlyphCodes.Add('SF070000', 9524);
  FGlyphCodes.Add('SF080000', 9500);
  FGlyphCodes.Add('SF090000', 9508);
  FGlyphCodes.Add('SF100000', 9472);
  FGlyphCodes.Add('SF110000', 9474);
  FGlyphCodes.Add('SF190000', 9569);
  FGlyphCodes.Add('SF200000', 9570);
  FGlyphCodes.Add('SF210000', 9558);
  FGlyphCodes.Add('SF220000', 9557);
  FGlyphCodes.Add('SF230000', 9571);
  FGlyphCodes.Add('SF240000', 9553);
  FGlyphCodes.Add('SF250000', 9559);
  FGlyphCodes.Add('SF260000', 9565);
  FGlyphCodes.Add('SF270000', 9564);
  FGlyphCodes.Add('SF280000', 9563);
  FGlyphCodes.Add('SF360000', 9566);
  FGlyphCodes.Add('SF370000', 9567);
  FGlyphCodes.Add('SF380000', 9562);
  FGlyphCodes.Add('SF390000', 9556);
  FGlyphCodes.Add('SF400000', 9577);
  FGlyphCodes.Add('SF410000', 9574);
  FGlyphCodes.Add('SF420000', 9568);
  FGlyphCodes.Add('SF430000', 9552);
  FGlyphCodes.Add('SF440000', 9580);
  FGlyphCodes.Add('SF450000', 9575);
  FGlyphCodes.Add('SF460000', 9576);
  FGlyphCodes.Add('SF470000', 9572);
  FGlyphCodes.Add('SF480000', 9573);
  FGlyphCodes.Add('SF490000', 9561);
  FGlyphCodes.Add('SF500000', 9560);
  FGlyphCodes.Add('SF510000', 9554);
  FGlyphCodes.Add('SF520000', 9555);
  FGlyphCodes.Add('SF530000', 9579);
  FGlyphCodes.Add('SF540000', 9578);
  FGlyphCodes.Add('Sacute', 346);
  FGlyphCodes.Add('Sacutedotaccent', 7780);
  FGlyphCodes.Add('Sampigreek', 992);
  FGlyphCodes.Add('Scaron', 352);
  FGlyphCodes.Add('Scarondotaccent', 7782);
  FGlyphCodes.Add('Scaronsmall', 63229);
  FGlyphCodes.Add('Scedilla', 350);
  FGlyphCodes.Add('Schwa', 399);
  FGlyphCodes.Add('Schwacyrillic', 1240);
  FGlyphCodes.Add('Schwadieresiscyrillic', 1242);
  FGlyphCodes.Add('Scircle', 9416);
  FGlyphCodes.Add('Scircumflex', 348);
  FGlyphCodes.Add('Scommaaccent', 536);
  FGlyphCodes.Add('Sdotaccent', 7776);
  FGlyphCodes.Add('Sdotbelow', 7778);
  FGlyphCodes.Add('Sdotbelowdotaccent', 7784);
  FGlyphCodes.Add('Seharmenian', 1357);
  FGlyphCodes.Add('Sevenroman', 8550);
  FGlyphCodes.Add('Shaarmenian', 1351);
  FGlyphCodes.Add('Shacyrillic', 1064);
  FGlyphCodes.Add('Shchacyrillic', 1065);
  FGlyphCodes.Add('Sheicoptic', 994);
  FGlyphCodes.Add('Shhacyrillic', 1210);
  FGlyphCodes.Add('Shimacoptic', 1004);
  FGlyphCodes.Add('Sigma', 931);
  FGlyphCodes.Add('Sixroman', 8549);
  FGlyphCodes.Add('Smonospace', 65331);
  FGlyphCodes.Add('Softsigncyrillic', 1068);
  FGlyphCodes.Add('Ssmall', 63347);
  FGlyphCodes.Add('Stigmagreek', 986);
  FGlyphCodes.Add('T', 84);
  FGlyphCodes.Add('Tau', 932);
  FGlyphCodes.Add('Tbar', 358);
  FGlyphCodes.Add('Tcaron', 356);
  FGlyphCodes.Add('Tcedilla', 354);
  FGlyphCodes.Add('Tcircle', 9417);
  FGlyphCodes.Add('Tcircumflexbelow', 7792);
  FGlyphCodes.Add('Tcommaaccent', 354);
  FGlyphCodes.Add('Tdotaccent', 7786);
  FGlyphCodes.Add('Tdotbelow', 7788);
  FGlyphCodes.Add('Tecyrillic', 1058);
  FGlyphCodes.Add('Tedescendercyrillic', 1196);
  FGlyphCodes.Add('Tenroman', 8553);
  FGlyphCodes.Add('Tetsecyrillic', 1204);
  FGlyphCodes.Add('Theta', 920);
  FGlyphCodes.Add('Thook', 428);
  FGlyphCodes.Add('Thorn', 222);
  FGlyphCodes.Add('Thornsmall', 63486);
  FGlyphCodes.Add('Threeroman', 8546);
  FGlyphCodes.Add('Tildesmall', 63230);
  FGlyphCodes.Add('Tiwnarmenian', 1359);
  FGlyphCodes.Add('Tlinebelow', 7790);
  FGlyphCodes.Add('Tmonospace', 65332);
  FGlyphCodes.Add('Toarmenian', 1337);
  FGlyphCodes.Add('Tonefive', 444);
  FGlyphCodes.Add('Tonesix', 388);
  FGlyphCodes.Add('Tonetwo', 423);
  FGlyphCodes.Add('Tretroflexhook', 430);
  FGlyphCodes.Add('Tsecyrillic', 1062);
  FGlyphCodes.Add('Tshecyrillic', 1035);
  FGlyphCodes.Add('Tsmall', 63348);
  FGlyphCodes.Add('Twelveroman', 8555);
  FGlyphCodes.Add('Tworoman', 8545);
  FGlyphCodes.Add('U', 85);
  FGlyphCodes.Add('Uacute', 218);
  FGlyphCodes.Add('Uacutesmall', 63482);
  FGlyphCodes.Add('Ubreve', 364);
  FGlyphCodes.Add('Ucaron', 467);
  FGlyphCodes.Add('Ucircle', 9418);
  FGlyphCodes.Add('Ucircumflex', 219);
  FGlyphCodes.Add('Ucircumflexbelow', 7798);
  FGlyphCodes.Add('Ucircumflexsmall', 63483);
  FGlyphCodes.Add('Ucyrillic', 1059);
  FGlyphCodes.Add('Udblacute', 368);
  FGlyphCodes.Add('Udblgrave', 532);
  FGlyphCodes.Add('Udieresis', 220);
  FGlyphCodes.Add('Udieresisacute', 471);
  FGlyphCodes.Add('Udieresisbelow', 7794);
  FGlyphCodes.Add('Udieresiscaron', 473);
  FGlyphCodes.Add('Udieresiscyrillic', 1264);
  FGlyphCodes.Add('Udieresisgrave', 475);
  FGlyphCodes.Add('Udieresismacron', 469);
  FGlyphCodes.Add('Udieresissmall', 63484);
  FGlyphCodes.Add('Udotbelow', 7908);
  FGlyphCodes.Add('Ugrave', 217);
  FGlyphCodes.Add('Ugravesmall', 63481);
  FGlyphCodes.Add('Uhookabove', 7910);
  FGlyphCodes.Add('Uhorn', 431);
  FGlyphCodes.Add('Uhornacute', 7912);
  FGlyphCodes.Add('Uhorndotbelow', 7920);
  FGlyphCodes.Add('Uhorngrave', 7914);
  FGlyphCodes.Add('Uhornhookabove', 7916);
  FGlyphCodes.Add('Uhorntilde', 7918);
  FGlyphCodes.Add('Uhungarumlaut', 368);
  FGlyphCodes.Add('Uhungarumlautcyrillic', 1266);
  FGlyphCodes.Add('Uinvertedbreve', 534);
  FGlyphCodes.Add('Ukcyrillic', 1144);
  FGlyphCodes.Add('Umacron', 362);
  FGlyphCodes.Add('Umacroncyrillic', 1262);
  FGlyphCodes.Add('Umacrondieresis', 7802);
  FGlyphCodes.Add('Umonospace', 65333);
  FGlyphCodes.Add('Uogonek', 370);
  FGlyphCodes.Add('Upsilon', 933);
  FGlyphCodes.Add('Upsilon1', 978);
  FGlyphCodes.Add('Upsilonacutehooksymbolgreek', 979);
  FGlyphCodes.Add('Upsilonafrican', 433);
  FGlyphCodes.Add('Upsilondieresis', 939);
  FGlyphCodes.Add('Upsilondieresishooksymbolgreek', 980);
  FGlyphCodes.Add('Upsilonhooksymbol', 978);
  FGlyphCodes.Add('Upsilontonos', 910);
  FGlyphCodes.Add('Uring', 366);
  FGlyphCodes.Add('Ushortcyrillic', 1038);
  FGlyphCodes.Add('Usmall', 63349);
  FGlyphCodes.Add('Ustraightcyrillic', 1198);
  FGlyphCodes.Add('Ustraightstrokecyrillic', 1200);
  FGlyphCodes.Add('Utilde', 360);
  FGlyphCodes.Add('Utildeacute', 7800);
  FGlyphCodes.Add('Utildebelow', 7796);
  FGlyphCodes.Add('V', 86);
  FGlyphCodes.Add('Vcircle', 9419);
  FGlyphCodes.Add('Vdotbelow', 7806);
  FGlyphCodes.Add('Vecyrillic', 1042);
  FGlyphCodes.Add('Vewarmenian', 1358);
  FGlyphCodes.Add('Vhook', 434);
  FGlyphCodes.Add('Vmonospace', 65334);
  FGlyphCodes.Add('Voarmenian', 1352);
  FGlyphCodes.Add('Vsmall', 63350);
  FGlyphCodes.Add('Vtilde', 7804);
  FGlyphCodes.Add('W', 87);
  FGlyphCodes.Add('Wacute', 7810);
  FGlyphCodes.Add('Wcircle', 9420);
  FGlyphCodes.Add('Wcircumflex', 372);
  FGlyphCodes.Add('Wdieresis', 7812);
  FGlyphCodes.Add('Wdotaccent', 7814);
  FGlyphCodes.Add('Wdotbelow', 7816);
  FGlyphCodes.Add('Wgrave', 7808);
  FGlyphCodes.Add('Wmonospace', 65335);
  FGlyphCodes.Add('Wsmall', 63351);
  FGlyphCodes.Add('X', 88);
  FGlyphCodes.Add('Xcircle', 9421);
  FGlyphCodes.Add('Xdieresis', 7820);
  FGlyphCodes.Add('Xdotaccent', 7818);
  FGlyphCodes.Add('Xeharmenian', 1341);
  FGlyphCodes.Add('Xi', 926);
  FGlyphCodes.Add('Xmonospace', 65336);
  FGlyphCodes.Add('Xsmall', 63352);
  FGlyphCodes.Add('Y', 89);
  FGlyphCodes.Add('Yacute', 221);
  FGlyphCodes.Add('Yacutesmall', 63485);
  FGlyphCodes.Add('Yatcyrillic', 1122);
  FGlyphCodes.Add('Ycircle', 9422);
  FGlyphCodes.Add('Ycircumflex', 374);
  FGlyphCodes.Add('Ydieresis', 376);
  FGlyphCodes.Add('Ydieresissmall', 63487);
  FGlyphCodes.Add('Ydotaccent', 7822);
  FGlyphCodes.Add('Ydotbelow', 7924);
  FGlyphCodes.Add('Yericyrillic', 1067);
  FGlyphCodes.Add('Yerudieresiscyrillic', 1272);
  FGlyphCodes.Add('Ygrave', 7922);
  FGlyphCodes.Add('Yhook', 435);
  FGlyphCodes.Add('Yhookabove', 7926);
  FGlyphCodes.Add('Yiarmenian', 1349);
  FGlyphCodes.Add('Yicyrillic', 1031);
  FGlyphCodes.Add('Yiwnarmenian', 1362);
  FGlyphCodes.Add('Ymonospace', 65337);
  FGlyphCodes.Add('Ysmall', 63353);
  FGlyphCodes.Add('Ytilde', 7928);
  FGlyphCodes.Add('Yusbigcyrillic', 1130);
  FGlyphCodes.Add('Yusbigiotifiedcyrillic', 1132);
  FGlyphCodes.Add('Yuslittlecyrillic', 1126);
  FGlyphCodes.Add('Yuslittleiotifiedcyrillic', 1128);
  FGlyphCodes.Add('Z', 90);
  FGlyphCodes.Add('Zaarmenian', 1334);
  FGlyphCodes.Add('Zacute', 377);
  FGlyphCodes.Add('Zcaron', 381);
  FGlyphCodes.Add('Zcaronsmall', 63231);
  FGlyphCodes.Add('Zcircle', 9423);
  FGlyphCodes.Add('Zcircumflex', 7824);
  FGlyphCodes.Add('Zdot', 379);
  FGlyphCodes.Add('Zdotaccent', 379);
  FGlyphCodes.Add('Zdotbelow', 7826);
  FGlyphCodes.Add('Zecyrillic', 1047);
  FGlyphCodes.Add('Zedescendercyrillic', 1176);
  FGlyphCodes.Add('Zedieresiscyrillic', 1246);
  FGlyphCodes.Add('Zeta', 918);
  FGlyphCodes.Add('Zhearmenian', 1338);
  FGlyphCodes.Add('Zhebrevecyrillic', 1217);
  FGlyphCodes.Add('Zhecyrillic', 1046);
  FGlyphCodes.Add('Zhedescendercyrillic', 1174);
  FGlyphCodes.Add('Zhedieresiscyrillic', 1244);
  FGlyphCodes.Add('Zlinebelow', 7828);
  FGlyphCodes.Add('Zmonospace', 65338);
  FGlyphCodes.Add('Zsmall', 63354);
  FGlyphCodes.Add('Zstroke', 437);
end;

procedure TdxFontFileUnicodeConverter.InitializePack2;
begin
  FGlyphCodes.Add('a', 97);
  FGlyphCodes.Add('a1', 9985);
  FGlyphCodes.Add('a10', 10017);
  FGlyphCodes.Add('a100', 10078);
  FGlyphCodes.Add('a101', 10081);
  FGlyphCodes.Add('a102', 10082);
  FGlyphCodes.Add('a103', 10083);
  FGlyphCodes.Add('a104', 10084);
  FGlyphCodes.Add('a105', 10000);
  FGlyphCodes.Add('a106', 10085);
  FGlyphCodes.Add('a107', 10086);
  FGlyphCodes.Add('a108', 10087);
  FGlyphCodes.Add('a109', 9824);
  FGlyphCodes.Add('a11', 9755);
  FGlyphCodes.Add('a110', 9829);
  FGlyphCodes.Add('a111', 9830);
  FGlyphCodes.Add('a112', 9827);
  FGlyphCodes.Add('a117', 9993);
  FGlyphCodes.Add('a118', 9992);
  FGlyphCodes.Add('a119', 9991);
  FGlyphCodes.Add('a12', 9758);
  FGlyphCodes.Add('a120', 9312);
  FGlyphCodes.Add('a121', 9313);
  FGlyphCodes.Add('a122', 9314);
  FGlyphCodes.Add('a123', 9315);
  FGlyphCodes.Add('a124', 9316);
  FGlyphCodes.Add('a125', 9317);
  FGlyphCodes.Add('a126', 9318);
  FGlyphCodes.Add('a127', 9319);
  FGlyphCodes.Add('a128', 9320);
  FGlyphCodes.Add('a129', 9321);
  FGlyphCodes.Add('a13', 9996);
  FGlyphCodes.Add('a130', 10102);
  FGlyphCodes.Add('a131', 10103);
  FGlyphCodes.Add('a132', 10104);
  FGlyphCodes.Add('a133', 10105);
  FGlyphCodes.Add('a134', 10106);
  FGlyphCodes.Add('a135', 10107);
  FGlyphCodes.Add('a136', 10108);
  FGlyphCodes.Add('a137', 10109);
  FGlyphCodes.Add('a138', 10110);
  FGlyphCodes.Add('a139', 10111);
  FGlyphCodes.Add('a14', 9997);
  FGlyphCodes.Add('a140', 10112);
  FGlyphCodes.Add('a141', 10113);
  FGlyphCodes.Add('a142', 10114);
  FGlyphCodes.Add('a143', 10115);
  FGlyphCodes.Add('a144', 10116);
  FGlyphCodes.Add('a145', 10117);
  FGlyphCodes.Add('a146', 10118);
  FGlyphCodes.Add('a147', 10119);
  FGlyphCodes.Add('a148', 10120);
  FGlyphCodes.Add('a149', 10121);
  FGlyphCodes.Add('a15', 9998);
  FGlyphCodes.Add('a150', 10122);
  FGlyphCodes.Add('a151', 10123);
  FGlyphCodes.Add('a152', 10124);
  FGlyphCodes.Add('a153', 10125);
  FGlyphCodes.Add('a154', 10126);
  FGlyphCodes.Add('a155', 10127);
  FGlyphCodes.Add('a156', 10128);
  FGlyphCodes.Add('a157', 10129);
  FGlyphCodes.Add('a158', 10130);
  FGlyphCodes.Add('a159', 10131);
  FGlyphCodes.Add('a16', 9999);
  FGlyphCodes.Add('a160', 10132);
  FGlyphCodes.Add('a161', 8594);
  FGlyphCodes.Add('a162', 10147);
  FGlyphCodes.Add('a163', 8596);
  FGlyphCodes.Add('a164', 8597);
  FGlyphCodes.Add('a165', 10137);
  FGlyphCodes.Add('a166', 10139);
  FGlyphCodes.Add('a167', 10140);
  FGlyphCodes.Add('a168', 10141);
  FGlyphCodes.Add('a169', 10142);
  FGlyphCodes.Add('a17', 10001);
  FGlyphCodes.Add('a170', 10143);
  FGlyphCodes.Add('a171', 10144);
  FGlyphCodes.Add('a172', 10145);
  FGlyphCodes.Add('a173', 10146);
  FGlyphCodes.Add('a174', 10148);
  FGlyphCodes.Add('a175', 10149);
  FGlyphCodes.Add('a176', 10150);
  FGlyphCodes.Add('a177', 10151);
  FGlyphCodes.Add('a178', 10152);
  FGlyphCodes.Add('a179', 10153);
  FGlyphCodes.Add('a18', 10002);
  FGlyphCodes.Add('a180', 10155);
  FGlyphCodes.Add('a181', 10157);
  FGlyphCodes.Add('a182', 10159);
  FGlyphCodes.Add('a183', 10162);
  FGlyphCodes.Add('a184', 10163);
  FGlyphCodes.Add('a185', 10165);
  FGlyphCodes.Add('a186', 10168);
  FGlyphCodes.Add('a187', 10170);
  FGlyphCodes.Add('a188', 10171);
  FGlyphCodes.Add('a189', 10172);
  FGlyphCodes.Add('a19', 10003);
  FGlyphCodes.Add('a190', 10173);
  FGlyphCodes.Add('a191', 10174);
  FGlyphCodes.Add('a192', 10138);
  FGlyphCodes.Add('a193', 10154);
  FGlyphCodes.Add('a194', 10166);
  FGlyphCodes.Add('a195', 10169);
  FGlyphCodes.Add('a196', 10136);
  FGlyphCodes.Add('a197', 10164);
  FGlyphCodes.Add('a198', 10167);
  FGlyphCodes.Add('a199', 10156);
  FGlyphCodes.Add('a2', 9986);
  FGlyphCodes.Add('a20', 10004);
  FGlyphCodes.Add('a200', 10158);
  FGlyphCodes.Add('a201', 10161);
  FGlyphCodes.Add('a202', 9987);
  FGlyphCodes.Add('a203', 10064);
  FGlyphCodes.Add('a204', 10066);
  FGlyphCodes.Add('a205', 10094);
  FGlyphCodes.Add('a206', 10096);
  FGlyphCodes.Add('a21', 10005);
  FGlyphCodes.Add('a22', 10006);
  FGlyphCodes.Add('a23', 10007);
  FGlyphCodes.Add('a24', 10008);
  FGlyphCodes.Add('a25', 10009);
  FGlyphCodes.Add('a26', 10010);
  FGlyphCodes.Add('a27', 10011);
  FGlyphCodes.Add('a28', 10012);
  FGlyphCodes.Add('a29', 10018);
  FGlyphCodes.Add('a3', 9988);
  FGlyphCodes.Add('a30', 10019);
  FGlyphCodes.Add('a31', 10020);
  FGlyphCodes.Add('a32', 10021);
  FGlyphCodes.Add('a33', 10022);
  FGlyphCodes.Add('a34', 10023);
  FGlyphCodes.Add('a35', 9733);
  FGlyphCodes.Add('a36', 10025);
  FGlyphCodes.Add('a37', 10026);
  FGlyphCodes.Add('a38', 10027);
  FGlyphCodes.Add('a39', 10028);
  FGlyphCodes.Add('a4', 9742);
  FGlyphCodes.Add('a40', 10029);
  FGlyphCodes.Add('a41', 10030);
  FGlyphCodes.Add('a42', 10031);
  FGlyphCodes.Add('a43', 10032);
  FGlyphCodes.Add('a44', 10033);
  FGlyphCodes.Add('a45', 10034);
  FGlyphCodes.Add('a46', 10035);
  FGlyphCodes.Add('a47', 10036);
  FGlyphCodes.Add('a48', 10037);
  FGlyphCodes.Add('a49', 10038);
  FGlyphCodes.Add('a5', 9990);
  FGlyphCodes.Add('a50', 10039);
  FGlyphCodes.Add('a51', 10040);
  FGlyphCodes.Add('a52', 10041);
  FGlyphCodes.Add('a53', 10042);
  FGlyphCodes.Add('a54', 10043);
  FGlyphCodes.Add('a55', 10044);
  FGlyphCodes.Add('a56', 10045);
  FGlyphCodes.Add('a57', 10046);
  FGlyphCodes.Add('a58', 10047);
  FGlyphCodes.Add('a59', 10048);
  FGlyphCodes.Add('a6', 10013);
  FGlyphCodes.Add('a60', 10049);
  FGlyphCodes.Add('a61', 10050);
  FGlyphCodes.Add('a62', 10051);
  FGlyphCodes.Add('a63', 10052);
  FGlyphCodes.Add('a64', 10053);
  FGlyphCodes.Add('a65', 10054);
  FGlyphCodes.Add('a66', 10055);
  FGlyphCodes.Add('a67', 10056);
  FGlyphCodes.Add('a68', 10057);
  FGlyphCodes.Add('a69', 10058);
  FGlyphCodes.Add('a7', 10014);
  FGlyphCodes.Add('a70', 10059);
  FGlyphCodes.Add('a71', 9679);
  FGlyphCodes.Add('a72', 10061);
  FGlyphCodes.Add('a73', 9632);
  FGlyphCodes.Add('a74', 10063);
  FGlyphCodes.Add('a75', 10065);
  FGlyphCodes.Add('a76', 9650);
  FGlyphCodes.Add('a77', 9660);
  FGlyphCodes.Add('a78', 9670);
  FGlyphCodes.Add('a79', 10070);
  FGlyphCodes.Add('a8', 10015);
  FGlyphCodes.Add('a81', 9687);
  FGlyphCodes.Add('a82', 10072);
  FGlyphCodes.Add('a83', 10073);
  FGlyphCodes.Add('a84', 10074);
  FGlyphCodes.Add('a85', 10095);
  FGlyphCodes.Add('a86', 10097);
  FGlyphCodes.Add('a87', 10098);
  FGlyphCodes.Add('a88', 10099);
  FGlyphCodes.Add('a89', 10088);
  FGlyphCodes.Add('a9', 10016);
  FGlyphCodes.Add('a90', 10089);
  FGlyphCodes.Add('a91', 10092);
  FGlyphCodes.Add('a92', 10093);
  FGlyphCodes.Add('a93', 10090);
  FGlyphCodes.Add('a94', 10091);
  FGlyphCodes.Add('a95', 10100);
  FGlyphCodes.Add('a96', 10101);
  FGlyphCodes.Add('a97', 10075);
  FGlyphCodes.Add('a98', 10076);
  FGlyphCodes.Add('a99', 10077);
  FGlyphCodes.Add('aabengali', 2438);
  FGlyphCodes.Add('aacute', 225);
  FGlyphCodes.Add('aadeva', 2310);
  FGlyphCodes.Add('aagujarati', 2694);
  FGlyphCodes.Add('aagurmukhi', 2566);
  FGlyphCodes.Add('aamatragurmukhi', 2622);
  FGlyphCodes.Add('aarusquare', 13059);
  FGlyphCodes.Add('aavowelsignbengali', 2494);
  FGlyphCodes.Add('aavowelsigndeva', 2366);
  FGlyphCodes.Add('aavowelsigngujarati', 2750);
  FGlyphCodes.Add('abbreviationmarkarmenian', 1375);
  FGlyphCodes.Add('abbreviationsigndeva', 2416);
  FGlyphCodes.Add('abengali', 2437);
  FGlyphCodes.Add('abopomofo', 12570);
  FGlyphCodes.Add('abreve', 259);
  FGlyphCodes.Add('abreveacute', 7855);
  FGlyphCodes.Add('abrevecyrillic', 1233);
  FGlyphCodes.Add('abrevedotbelow', 7863);
  FGlyphCodes.Add('abrevegrave', 7857);
  FGlyphCodes.Add('abrevehookabove', 7859);
  FGlyphCodes.Add('abrevetilde', 7861);
  FGlyphCodes.Add('acaron', 462);
  FGlyphCodes.Add('acircle', 9424);
  FGlyphCodes.Add('acircumflex', 226);
  FGlyphCodes.Add('acircumflexacute', 7845);
  FGlyphCodes.Add('acircumflexdotbelow', 7853);
  FGlyphCodes.Add('acircumflexgrave', 7847);
  FGlyphCodes.Add('acircumflexhookabove', 7849);
  FGlyphCodes.Add('acircumflextilde', 7851);
  FGlyphCodes.Add('acute', 180);
  FGlyphCodes.Add('acutebelowcmb', 791);
  FGlyphCodes.Add('acutecmb', 769);
  FGlyphCodes.Add('acutecomb', 769);
  FGlyphCodes.Add('acutedeva', 2388);
  FGlyphCodes.Add('acutelowmod', 719);
  FGlyphCodes.Add('acutetonecmb', 833);
  FGlyphCodes.Add('acyrillic', 1072);
  FGlyphCodes.Add('adblgrave', 513);
  FGlyphCodes.Add('addakgurmukhi', 2673);
  FGlyphCodes.Add('adeva', 2309);
  FGlyphCodes.Add('adieresis', 228);
  FGlyphCodes.Add('adieresiscyrillic', 1235);
  FGlyphCodes.Add('adieresismacron', 479);
  FGlyphCodes.Add('adotbelow', 7841);
  FGlyphCodes.Add('adotmacron', 481);
  FGlyphCodes.Add('ae', 230);
  FGlyphCodes.Add('aeacute', 509);
  FGlyphCodes.Add('aekorean', 12624);
  FGlyphCodes.Add('aemacron', 483);
  FGlyphCodes.Add('afii00208', 8213);
  FGlyphCodes.Add('afii08941', 8356);
  FGlyphCodes.Add('afii10017', 1040);
  FGlyphCodes.Add('afii10018', 1041);
  FGlyphCodes.Add('afii10019', 1042);
  FGlyphCodes.Add('afii10020', 1043);
  FGlyphCodes.Add('afii10021', 1044);
  FGlyphCodes.Add('afii10022', 1045);
  FGlyphCodes.Add('afii10023', 1025);
  FGlyphCodes.Add('afii10024', 1046);
  FGlyphCodes.Add('afii10025', 1047);
  FGlyphCodes.Add('afii10026', 1048);
  FGlyphCodes.Add('afii10027', 1049);
  FGlyphCodes.Add('afii10028', 1050);
  FGlyphCodes.Add('afii10029', 1051);
  FGlyphCodes.Add('afii10030', 1052);
  FGlyphCodes.Add('afii10031', 1053);
  FGlyphCodes.Add('afii10032', 1054);
  FGlyphCodes.Add('afii10033', 1055);
  FGlyphCodes.Add('afii10034', 1056);
  FGlyphCodes.Add('afii10035', 1057);
  FGlyphCodes.Add('afii10036', 1058);
  FGlyphCodes.Add('afii10037', 1059);
  FGlyphCodes.Add('afii10038', 1060);
  FGlyphCodes.Add('afii10039', 1061);
  FGlyphCodes.Add('afii10040', 1062);
  FGlyphCodes.Add('afii10041', 1063);
  FGlyphCodes.Add('afii10042', 1064);
  FGlyphCodes.Add('afii10043', 1065);
  FGlyphCodes.Add('afii10044', 1066);
  FGlyphCodes.Add('afii10045', 1067);
  FGlyphCodes.Add('afii10046', 1068);
  FGlyphCodes.Add('afii10047', 1069);
  FGlyphCodes.Add('afii10048', 1070);
  FGlyphCodes.Add('afii10049', 1071);
  FGlyphCodes.Add('afii10050', 1168);
  FGlyphCodes.Add('afii10051', 1026);
  FGlyphCodes.Add('afii10052', 1027);
  FGlyphCodes.Add('afii10053', 1028);
  FGlyphCodes.Add('afii10054', 1029);
  FGlyphCodes.Add('afii10055', 1030);
  FGlyphCodes.Add('afii10056', 1031);
  FGlyphCodes.Add('afii10057', 1032);
  FGlyphCodes.Add('afii10058', 1033);
  FGlyphCodes.Add('afii10059', 1034);
  FGlyphCodes.Add('afii10060', 1035);
  FGlyphCodes.Add('afii10061', 1036);
  FGlyphCodes.Add('afii10062', 1038);
  FGlyphCodes.Add('afii10063', 63172);
  FGlyphCodes.Add('afii10064', 63173);
  FGlyphCodes.Add('afii10065', 1072);
  FGlyphCodes.Add('afii10066', 1073);
  FGlyphCodes.Add('afii10067', 1074);
  FGlyphCodes.Add('afii10068', 1075);
  FGlyphCodes.Add('afii10069', 1076);
  FGlyphCodes.Add('afii10070', 1077);
  FGlyphCodes.Add('afii10071', 1105);
  FGlyphCodes.Add('afii10072', 1078);
  FGlyphCodes.Add('afii10073', 1079);
  FGlyphCodes.Add('afii10074', 1080);
  FGlyphCodes.Add('afii10075', 1081);
  FGlyphCodes.Add('afii10076', 1082);
  FGlyphCodes.Add('afii10077', 1083);
  FGlyphCodes.Add('afii10078', 1084);
  FGlyphCodes.Add('afii10079', 1085);
  FGlyphCodes.Add('afii10080', 1086);
  FGlyphCodes.Add('afii10081', 1087);
  FGlyphCodes.Add('afii10082', 1088);
  FGlyphCodes.Add('afii10083', 1089);
  FGlyphCodes.Add('afii10084', 1090);
  FGlyphCodes.Add('afii10085', 1091);
  FGlyphCodes.Add('afii10086', 1092);
  FGlyphCodes.Add('afii10087', 1093);
  FGlyphCodes.Add('afii10088', 1094);
  FGlyphCodes.Add('afii10089', 1095);
  FGlyphCodes.Add('afii10090', 1096);
  FGlyphCodes.Add('afii10091', 1097);
  FGlyphCodes.Add('afii10092', 1098);
  FGlyphCodes.Add('afii10093', 1099);
  FGlyphCodes.Add('afii10094', 1100);
  FGlyphCodes.Add('afii10095', 1101);
  FGlyphCodes.Add('afii10096', 1102);
  FGlyphCodes.Add('afii10097', 1103);
  FGlyphCodes.Add('afii10098', 1169);
  FGlyphCodes.Add('afii10099', 1106);
  FGlyphCodes.Add('afii10100', 1107);
  FGlyphCodes.Add('afii10101', 1108);
  FGlyphCodes.Add('afii10102', 1109);
  FGlyphCodes.Add('afii10103', 1110);
  FGlyphCodes.Add('afii10104', 1111);
  FGlyphCodes.Add('afii10105', 1112);
  FGlyphCodes.Add('afii10106', 1113);
  FGlyphCodes.Add('afii10107', 1114);
  FGlyphCodes.Add('afii10108', 1115);
  FGlyphCodes.Add('afii10109', 1116);
  FGlyphCodes.Add('afii10110', 1118);
  FGlyphCodes.Add('afii10145', 1039);
  FGlyphCodes.Add('afii10146', 1122);
  FGlyphCodes.Add('afii10147', 1138);
  FGlyphCodes.Add('afii10148', 1140);
  FGlyphCodes.Add('afii10192', 63174);
  FGlyphCodes.Add('afii10193', 1119);
  FGlyphCodes.Add('afii10194', 1123);
  FGlyphCodes.Add('afii10195', 1139);
  FGlyphCodes.Add('afii10196', 1141);
  FGlyphCodes.Add('afii10831', 63175);
  FGlyphCodes.Add('afii10832', 63176);
  FGlyphCodes.Add('afii10846', 1241);
  FGlyphCodes.Add('afii299', 8206);
  FGlyphCodes.Add('afii300', 8207);
  FGlyphCodes.Add('afii301', 8205);
  FGlyphCodes.Add('afii57381', 1642);
  FGlyphCodes.Add('afii57388', 1548);
  FGlyphCodes.Add('afii57392', 1632);
  FGlyphCodes.Add('afii57393', 1633);
  FGlyphCodes.Add('afii57394', 1634);
  FGlyphCodes.Add('afii57395', 1635);
  FGlyphCodes.Add('afii57396', 1636);
  FGlyphCodes.Add('afii57397', 1637);
  FGlyphCodes.Add('afii57398', 1638);
  FGlyphCodes.Add('afii57399', 1639);
  FGlyphCodes.Add('afii57400', 1640);
  FGlyphCodes.Add('afii57401', 1641);
  FGlyphCodes.Add('afii57403', 1563);
  FGlyphCodes.Add('afii57407', 1567);
  FGlyphCodes.Add('afii57409', 1569);
  FGlyphCodes.Add('afii57410', 1570);
  FGlyphCodes.Add('afii57411', 1571);
  FGlyphCodes.Add('afii57412', 1572);
  FGlyphCodes.Add('afii57413', 1573);
  FGlyphCodes.Add('afii57414', 1574);
  FGlyphCodes.Add('afii57415', 1575);
  FGlyphCodes.Add('afii57416', 1576);
  FGlyphCodes.Add('afii57417', 1577);
  FGlyphCodes.Add('afii57418', 1578);
  FGlyphCodes.Add('afii57419', 1579);
  FGlyphCodes.Add('afii57420', 1580);
  FGlyphCodes.Add('afii57421', 1581);
  FGlyphCodes.Add('afii57422', 1582);
  FGlyphCodes.Add('afii57423', 1583);
  FGlyphCodes.Add('afii57424', 1584);
  FGlyphCodes.Add('afii57425', 1585);
  FGlyphCodes.Add('afii57426', 1586);
  FGlyphCodes.Add('afii57427', 1587);
  FGlyphCodes.Add('afii57428', 1588);
  FGlyphCodes.Add('afii57429', 1589);
  FGlyphCodes.Add('afii57430', 1590);
  FGlyphCodes.Add('afii57431', 1591);
  FGlyphCodes.Add('afii57432', 1592);
  FGlyphCodes.Add('afii57433', 1593);
  FGlyphCodes.Add('afii57434', 1594);
  FGlyphCodes.Add('afii57440', 1600);
  FGlyphCodes.Add('afii57441', 1601);
  FGlyphCodes.Add('afii57442', 1602);
  FGlyphCodes.Add('afii57443', 1603);
  FGlyphCodes.Add('afii57444', 1604);
  FGlyphCodes.Add('afii57445', 1605);
  FGlyphCodes.Add('afii57446', 1606);
  FGlyphCodes.Add('afii57448', 1608);
  FGlyphCodes.Add('afii57449', 1609);
  FGlyphCodes.Add('afii57450', 1610);
  FGlyphCodes.Add('afii57451', 1611);
  FGlyphCodes.Add('afii57452', 1612);
  FGlyphCodes.Add('afii57453', 1613);
  FGlyphCodes.Add('afii57454', 1614);
  FGlyphCodes.Add('afii57455', 1615);
  FGlyphCodes.Add('afii57456', 1616);
  FGlyphCodes.Add('afii57457', 1617);
  FGlyphCodes.Add('afii57458', 1618);
  FGlyphCodes.Add('afii57470', 1607);
  FGlyphCodes.Add('afii57505', 1700);
  FGlyphCodes.Add('afii57506', 1662);
  FGlyphCodes.Add('afii57507', 1670);
  FGlyphCodes.Add('afii57508', 1688);
  FGlyphCodes.Add('afii57509', 1711);
  FGlyphCodes.Add('afii57511', 1657);
  FGlyphCodes.Add('afii57512', 1672);
  FGlyphCodes.Add('afii57513', 1681);
  FGlyphCodes.Add('afii57514', 1722);
  FGlyphCodes.Add('afii57519', 1746);
  FGlyphCodes.Add('afii57534', 1749);
  FGlyphCodes.Add('afii57636', 8362);
  FGlyphCodes.Add('afii57645', 1470);
  FGlyphCodes.Add('afii57658', 1475);
  FGlyphCodes.Add('afii57664', 1488);
  FGlyphCodes.Add('afii57665', 1489);
  FGlyphCodes.Add('afii57666', 1490);
  FGlyphCodes.Add('afii57667', 1491);
  FGlyphCodes.Add('afii57668', 1492);
  FGlyphCodes.Add('afii57669', 1493);
  FGlyphCodes.Add('afii57670', 1494);
  FGlyphCodes.Add('afii57671', 1495);
  FGlyphCodes.Add('afii57672', 1496);
  FGlyphCodes.Add('afii57673', 1497);
  FGlyphCodes.Add('afii57674', 1498);
  FGlyphCodes.Add('afii57675', 1499);
  FGlyphCodes.Add('afii57676', 1500);
  FGlyphCodes.Add('afii57677', 1501);
  FGlyphCodes.Add('afii57678', 1502);
  FGlyphCodes.Add('afii57679', 1503);
  FGlyphCodes.Add('afii57680', 1504);
  FGlyphCodes.Add('afii57681', 1505);
  FGlyphCodes.Add('afii57682', 1506);
  FGlyphCodes.Add('afii57683', 1507);
  FGlyphCodes.Add('afii57684', 1508);
  FGlyphCodes.Add('afii57685', 1509);
  FGlyphCodes.Add('afii57686', 1510);
  FGlyphCodes.Add('afii57687', 1511);
  FGlyphCodes.Add('afii57688', 1512);
  FGlyphCodes.Add('afii57689', 1513);
  FGlyphCodes.Add('afii57690', 1514);
  FGlyphCodes.Add('afii57694', 64298);
  FGlyphCodes.Add('afii57695', 64299);
  FGlyphCodes.Add('afii57700', 64331);
  FGlyphCodes.Add('afii57705', 64287);
  FGlyphCodes.Add('afii57716', 1520);
  FGlyphCodes.Add('afii57717', 1521);
  FGlyphCodes.Add('afii57718', 1522);
  FGlyphCodes.Add('afii57723', 64309);
  FGlyphCodes.Add('afii57793', 1460);
  FGlyphCodes.Add('afii57794', 1461);
  FGlyphCodes.Add('afii57795', 1462);
  FGlyphCodes.Add('afii57796', 1467);
  FGlyphCodes.Add('afii57797', 1464);
  FGlyphCodes.Add('afii57798', 1463);
  FGlyphCodes.Add('afii57799', 1456);
  FGlyphCodes.Add('afii57800', 1458);
  FGlyphCodes.Add('afii57801', 1457);
  FGlyphCodes.Add('afii57802', 1459);
  FGlyphCodes.Add('afii57803', 1474);
  FGlyphCodes.Add('afii57804', 1473);
  FGlyphCodes.Add('afii57806', 1465);
  FGlyphCodes.Add('afii57807', 1468);
  FGlyphCodes.Add('afii57839', 1469);
  FGlyphCodes.Add('afii57841', 1471);
  FGlyphCodes.Add('afii57842', 1472);
  FGlyphCodes.Add('afii57929', 700);
  FGlyphCodes.Add('afii61248', 8453);
  FGlyphCodes.Add('afii61289', 8467);
  FGlyphCodes.Add('afii61352', 8470);
  FGlyphCodes.Add('afii61573', 8236);
  FGlyphCodes.Add('afii61574', 8237);
  FGlyphCodes.Add('afii61575', 8238);
  FGlyphCodes.Add('afii61664', 8204);
  FGlyphCodes.Add('afii63167', 1645);
  FGlyphCodes.Add('afii64937', 701);
  FGlyphCodes.Add('agrave', 224);
  FGlyphCodes.Add('agujarati', 2693);
  FGlyphCodes.Add('agurmukhi', 2565);
  FGlyphCodes.Add('ahiragana', 12354);
  FGlyphCodes.Add('ahookabove', 7843);
  FGlyphCodes.Add('aibengali', 2448);
  FGlyphCodes.Add('aibopomofo', 12574);
  FGlyphCodes.Add('aideva', 2320);
  FGlyphCodes.Add('aiecyrillic', 1237);
  FGlyphCodes.Add('aigujarati', 2704);
  FGlyphCodes.Add('aigurmukhi', 2576);
  FGlyphCodes.Add('aimatragurmukhi', 2632);
  FGlyphCodes.Add('ainarabic', 1593);
  FGlyphCodes.Add('ainfinalarabic', 65226);
  FGlyphCodes.Add('aininitialarabic', 65227);
  FGlyphCodes.Add('ainmedialarabic', 65228);
  FGlyphCodes.Add('ainvertedbreve', 515);
  FGlyphCodes.Add('aivowelsignbengali', 2504);
  FGlyphCodes.Add('aivowelsigndeva', 2376);
  FGlyphCodes.Add('aivowelsigngujarati', 2760);
  FGlyphCodes.Add('akatakana', 12450);
  FGlyphCodes.Add('akatakanahalfwidth', 65393);
  FGlyphCodes.Add('akorean', 12623);
  FGlyphCodes.Add('alef', 1488);
  FGlyphCodes.Add('alefarabic', 1575);
  FGlyphCodes.Add('alefdageshhebrew', 64304);
  FGlyphCodes.Add('aleffinalarabic', 65166);
  FGlyphCodes.Add('alefhamzaabovearabic', 1571);
  FGlyphCodes.Add('alefhamzaabovefinalarabic', 65156);
  FGlyphCodes.Add('alefhamzabelowarabic', 1573);
  FGlyphCodes.Add('alefhamzabelowfinalarabic', 65160);
  FGlyphCodes.Add('alefhebrew', 1488);
  FGlyphCodes.Add('aleflamedhebrew', 64335);
  FGlyphCodes.Add('alefmaddaabovearabic', 1570);
  FGlyphCodes.Add('alefmaddaabovefinalarabic', 65154);
  FGlyphCodes.Add('alefmaksuraarabic', 1609);
  FGlyphCodes.Add('alefmaksurafinalarabic', 65264);
  FGlyphCodes.Add('alefmaksurainitialarabic', 65267);
  FGlyphCodes.Add('alefmaksuramedialarabic', 65268);
  FGlyphCodes.Add('alefpatahhebrew', 64302);
  FGlyphCodes.Add('alefqamatshebrew', 64303);
  FGlyphCodes.Add('aleph', 8501);
  FGlyphCodes.Add('allequal', 8780);
  FGlyphCodes.Add('alpha', 945);
  FGlyphCodes.Add('alphatonos', 940);
  FGlyphCodes.Add('amacron', 257);
  FGlyphCodes.Add('amonospace', 65345);
  FGlyphCodes.Add('ampersand', 38);
  FGlyphCodes.Add('ampersandmonospace', 65286);
  FGlyphCodes.Add('ampersandsmall', 63270);
  FGlyphCodes.Add('amsquare', 13250);
  FGlyphCodes.Add('anbopomofo', 12578);
  FGlyphCodes.Add('angbopomofo', 12580);
  FGlyphCodes.Add('angkhankhuthai', 3674);
  FGlyphCodes.Add('angle', 8736);
  FGlyphCodes.Add('anglebracketleft', 12296);
  FGlyphCodes.Add('anglebracketleftvertical', 65087);
  FGlyphCodes.Add('anglebracketright', 12297);
  FGlyphCodes.Add('anglebracketrightvertical', 65088);
  FGlyphCodes.Add('angleleft', 9001);
  FGlyphCodes.Add('angleright', 9002);
  FGlyphCodes.Add('angstrom', 8491);
  FGlyphCodes.Add('anoteleia', 903);
  FGlyphCodes.Add('anudattadeva', 2386);
  FGlyphCodes.Add('anusvarabengali', 2434);
  FGlyphCodes.Add('anusvaradeva', 2306);
  FGlyphCodes.Add('anusvaragujarati', 2690);
  FGlyphCodes.Add('aogonek', 261);
  FGlyphCodes.Add('apaatosquare', 13056);
  FGlyphCodes.Add('aparen', 9372);
  FGlyphCodes.Add('apostrophearmenian', 1370);
  FGlyphCodes.Add('apostrophemod', 700);
  FGlyphCodes.Add('apple', 63743);
  FGlyphCodes.Add('approaches', 8784);
  FGlyphCodes.Add('approxequal', 8776);
  FGlyphCodes.Add('approxequalorimage', 8786);
  FGlyphCodes.Add('approximatelyequal', 8773);
  FGlyphCodes.Add('araeaekorean', 12686);
  FGlyphCodes.Add('araeakorean', 12685);
  FGlyphCodes.Add('arc', 8978);
  FGlyphCodes.Add('arighthalfring', 7834);
  FGlyphCodes.Add('aring', 229);
  FGlyphCodes.Add('aringacute', 507);
  FGlyphCodes.Add('aringbelow', 7681);
  FGlyphCodes.Add('arrowboth', 8596);
  FGlyphCodes.Add('arrowdashdown', 8675);
  FGlyphCodes.Add('arrowdashleft', 8672);
  FGlyphCodes.Add('arrowdashright', 8674);
  FGlyphCodes.Add('arrowdashup', 8673);
  FGlyphCodes.Add('arrowdblboth', 8660);
  FGlyphCodes.Add('arrowdbldown', 8659);
  FGlyphCodes.Add('arrowdblleft', 8656);
  FGlyphCodes.Add('arrowdblright', 8658);
  FGlyphCodes.Add('arrowdblup', 8657);
  FGlyphCodes.Add('arrowdown', 8595);
  FGlyphCodes.Add('arrowdownleft', 8601);
  FGlyphCodes.Add('arrowdownright', 8600);
  FGlyphCodes.Add('arrowdownwhite', 8681);
  FGlyphCodes.Add('arrowheaddownmod', 709);
  FGlyphCodes.Add('arrowheadleftmod', 706);
  FGlyphCodes.Add('arrowheadrightmod', 707);
  FGlyphCodes.Add('arrowheadupmod', 708);
  FGlyphCodes.Add('arrowhorizex', 63719);
  FGlyphCodes.Add('arrowleft', 8592);
  FGlyphCodes.Add('arrowleftdbl', 8656);
  FGlyphCodes.Add('arrowleftdblstroke', 8653);
  FGlyphCodes.Add('arrowleftoverright', 8646);
  FGlyphCodes.Add('arrowleftwhite', 8678);
  FGlyphCodes.Add('arrowright', 8594);
  FGlyphCodes.Add('arrowrightdblstroke', 8655);
  FGlyphCodes.Add('arrowrightheavy', 10142);
  FGlyphCodes.Add('arrowrightoverleft', 8644);
  FGlyphCodes.Add('arrowrightwhite', 8680);
  FGlyphCodes.Add('arrowtableft', 8676);
  FGlyphCodes.Add('arrowtabright', 8677);
  FGlyphCodes.Add('arrowup', 8593);
  FGlyphCodes.Add('arrowupdn', 8597);
  FGlyphCodes.Add('arrowupdnbse', 8616);
  FGlyphCodes.Add('arrowupdownbase', 8616);
  FGlyphCodes.Add('arrowupleft', 8598);
  FGlyphCodes.Add('arrowupleftofdown', 8645);
  FGlyphCodes.Add('arrowupright', 8599);
  FGlyphCodes.Add('arrowupwhite', 8679);
  FGlyphCodes.Add('arrowvertex', 63718);
  FGlyphCodes.Add('asciicircum', 94);
  FGlyphCodes.Add('asciicircummonospace', 65342);
  FGlyphCodes.Add('asciitilde', 126);
  FGlyphCodes.Add('asciitildemonospace', 65374);
  FGlyphCodes.Add('ascript', 593);
  FGlyphCodes.Add('ascriptturned', 594);
  FGlyphCodes.Add('asmallhiragana', 12353);
  FGlyphCodes.Add('asmallkatakana', 12449);
  FGlyphCodes.Add('asmallkatakanahalfwidth', 65383);
  FGlyphCodes.Add('asterisk', 42);
  FGlyphCodes.Add('asteriskaltonearabic', 1645);
  FGlyphCodes.Add('asteriskarabic', 1645);
  FGlyphCodes.Add('asteriskmath', 8727);
  FGlyphCodes.Add('asteriskmonospace', 65290);
  FGlyphCodes.Add('asterisksmall', 65121);
  FGlyphCodes.Add('asterism', 8258);
  FGlyphCodes.Add('asuperior', 63209);
  FGlyphCodes.Add('asymptoticallyequal', 8771);
  FGlyphCodes.Add('at', 64);
  FGlyphCodes.Add('atilde', 227);
  FGlyphCodes.Add('atmonospace', 65312);
  FGlyphCodes.Add('atsmall', 65131);
  FGlyphCodes.Add('aturned', 592);
  FGlyphCodes.Add('aubengali', 2452);
  FGlyphCodes.Add('aubopomofo', 12576);
  FGlyphCodes.Add('audeva', 2324);
  FGlyphCodes.Add('augujarati', 2708);
  FGlyphCodes.Add('augurmukhi', 2580);
  FGlyphCodes.Add('aulengthmarkbengali', 2519);
  FGlyphCodes.Add('aumatragurmukhi', 2636);
  FGlyphCodes.Add('auvowelsignbengali', 2508);
  FGlyphCodes.Add('auvowelsigndeva', 2380);
  FGlyphCodes.Add('auvowelsigngujarati', 2764);
  FGlyphCodes.Add('avagrahadeva', 2365);
  FGlyphCodes.Add('aybarmenian', 1377);
  FGlyphCodes.Add('ayin', 1506);
  FGlyphCodes.Add('ayinaltonehebrew', 64288);
  FGlyphCodes.Add('ayinhebrew', 1506);
  FGlyphCodes.Add('b', 98);
  FGlyphCodes.Add('babengali', 2476);
  FGlyphCodes.Add('backslash', 92);
  FGlyphCodes.Add('backslashmonospace', 65340);
  FGlyphCodes.Add('badeva', 2348);
  FGlyphCodes.Add('bagujarati', 2732);
  FGlyphCodes.Add('bagurmukhi', 2604);
  FGlyphCodes.Add('bahiragana', 12400);
  FGlyphCodes.Add('bahtthai', 3647);
  FGlyphCodes.Add('bakatakana', 12496);
  FGlyphCodes.Add('bar', 124);
  FGlyphCodes.Add('barmonospace', 65372);
  FGlyphCodes.Add('bbopomofo', 12549);
  FGlyphCodes.Add('bcircle', 9425);
  FGlyphCodes.Add('bdotaccent', 7683);
  FGlyphCodes.Add('bdotbelow', 7685);
  FGlyphCodes.Add('beamedsixteenthnotes', 9836);
  FGlyphCodes.Add('because', 8757);
  FGlyphCodes.Add('becyrillic', 1073);
  FGlyphCodes.Add('beharabic', 1576);
  FGlyphCodes.Add('behfinalarabic', 65168);
  FGlyphCodes.Add('behinitialarabic', 65169);
  FGlyphCodes.Add('behiragana', 12409);
  FGlyphCodes.Add('behmedialarabic', 65170);
  FGlyphCodes.Add('behmeeminitialarabic', 64671);
  FGlyphCodes.Add('behmeemisolatedarabic', 64520);
  FGlyphCodes.Add('behnoonfinalarabic', 64621);
  FGlyphCodes.Add('bekatakana', 12505);
  FGlyphCodes.Add('benarmenian', 1378);
  FGlyphCodes.Add('bet', 1489);
  FGlyphCodes.Add('beta', 946);
  FGlyphCodes.Add('betasymbolgreek', 976);
  FGlyphCodes.Add('betdagesh', 64305);
  FGlyphCodes.Add('betdageshhebrew', 64305);
  FGlyphCodes.Add('bethebrew', 1489);
  FGlyphCodes.Add('betrafehebrew', 64332);
  FGlyphCodes.Add('bhabengali', 2477);
  FGlyphCodes.Add('bhadeva', 2349);
  FGlyphCodes.Add('bhagujarati', 2733);
  FGlyphCodes.Add('bhagurmukhi', 2605);
  FGlyphCodes.Add('bhook', 595);
  FGlyphCodes.Add('bihiragana', 12403);
  FGlyphCodes.Add('bikatakana', 12499);
  FGlyphCodes.Add('bilabialclick', 664);
  FGlyphCodes.Add('bindigurmukhi', 2562);
  FGlyphCodes.Add('birusquare', 13105);
  FGlyphCodes.Add('blackcircle', 9679);
  FGlyphCodes.Add('blackdiamond', 9670);
  FGlyphCodes.Add('blackdownpointingtriangle', 9660);
  FGlyphCodes.Add('blackleftpointingpointer', 9668);
  FGlyphCodes.Add('blackleftpointingtriangle', 9664);
  FGlyphCodes.Add('blacklenticularbracketleft', 12304);
  FGlyphCodes.Add('blacklenticularbracketleftvertical', 65083);
  FGlyphCodes.Add('blacklenticularbracketright', 12305);
  FGlyphCodes.Add('blacklenticularbracketrightvertical', 65084);
  FGlyphCodes.Add('blacklowerlefttriangle', 9699);
  FGlyphCodes.Add('blacklowerrighttriangle', 9698);
  FGlyphCodes.Add('blackrectangle', 9644);
  FGlyphCodes.Add('blackrightpointingpointer', 9658);
  FGlyphCodes.Add('blackrightpointingtriangle', 9654);
  FGlyphCodes.Add('blacksmallsquare', 9642);
  FGlyphCodes.Add('blacksmilingface', 9787);
  FGlyphCodes.Add('blacksquare', 9632);
  FGlyphCodes.Add('blackstar', 9733);
  FGlyphCodes.Add('blackupperlefttriangle', 9700);
  FGlyphCodes.Add('blackupperrighttriangle', 9701);
  FGlyphCodes.Add('blackuppointingsmalltriangle', 9652);
  FGlyphCodes.Add('blackuppointingtriangle', 9650);
  FGlyphCodes.Add('blank', 9251);
  FGlyphCodes.Add('blinebelow', 7687);
  FGlyphCodes.Add('block', 9608);
  FGlyphCodes.Add('bmonospace', 65346);
  FGlyphCodes.Add('bobaimaithai', 3610);
  FGlyphCodes.Add('bohiragana', 12412);
  FGlyphCodes.Add('bokatakana', 12508);
  FGlyphCodes.Add('bparen', 9373);
  FGlyphCodes.Add('bqsquare', 13251);
  FGlyphCodes.Add('braceex', 63732);
  FGlyphCodes.Add('braceleft', 123);
  FGlyphCodes.Add('braceleftbt', 63731);
  FGlyphCodes.Add('braceleftmid', 63730);
  FGlyphCodes.Add('braceleftmonospace', 65371);
  FGlyphCodes.Add('braceleftsmall', 65115);
  FGlyphCodes.Add('bracelefttp', 63729);
  FGlyphCodes.Add('braceleftvertical', 65079);
  FGlyphCodes.Add('braceright', 125);
  FGlyphCodes.Add('bracerightbt', 63742);
  FGlyphCodes.Add('bracerightmid', 63741);
  FGlyphCodes.Add('bracerightmonospace', 65373);
  FGlyphCodes.Add('bracerightsmall', 65116);
  FGlyphCodes.Add('bracerighttp', 63740);
  FGlyphCodes.Add('bracerightvertical', 65080);
  FGlyphCodes.Add('bracketleft', 91);
  FGlyphCodes.Add('bracketleftbt', 63728);
  FGlyphCodes.Add('bracketleftex', 63727);
  FGlyphCodes.Add('bracketleftmonospace', 65339);
  FGlyphCodes.Add('bracketlefttp', 63726);
  FGlyphCodes.Add('bracketright', 93);
  FGlyphCodes.Add('bracketrightbt', 63739);
  FGlyphCodes.Add('bracketrightex', 63738);
  FGlyphCodes.Add('bracketrightmonospace', 65341);
  FGlyphCodes.Add('bracketrighttp', 63737);
  FGlyphCodes.Add('breve', 728);
  FGlyphCodes.Add('brevebelowcmb', 814);
  FGlyphCodes.Add('brevecmb', 774);
  FGlyphCodes.Add('breveinvertedbelowcmb', 815);
  FGlyphCodes.Add('breveinvertedcmb', 785);
  FGlyphCodes.Add('breveinverteddoublecmb', 865);
  FGlyphCodes.Add('bridgebelowcmb', 810);
  FGlyphCodes.Add('bridgeinvertedbelowcmb', 826);
  FGlyphCodes.Add('brokenbar', 166);
  FGlyphCodes.Add('bstroke', 384);
  FGlyphCodes.Add('bsuperior', 63210);
  FGlyphCodes.Add('btopbar', 387);
  FGlyphCodes.Add('buhiragana', 12406);
  FGlyphCodes.Add('bukatakana', 12502);
  FGlyphCodes.Add('bullet', 8226);
  FGlyphCodes.Add('bulletinverse', 9688);
  FGlyphCodes.Add('bulletoperator', 8729);
  FGlyphCodes.Add('bullseye', 9678);
  FGlyphCodes.Add('c', 99);
  FGlyphCodes.Add('caarmenian', 1390);
  FGlyphCodes.Add('cabengali', 2458);
  FGlyphCodes.Add('cacute', 263);
  FGlyphCodes.Add('cadeva', 2330);
  FGlyphCodes.Add('cagujarati', 2714);
  FGlyphCodes.Add('cagurmukhi', 2586);
  FGlyphCodes.Add('calsquare', 13192);
  FGlyphCodes.Add('candrabindubengali', 2433);
  FGlyphCodes.Add('candrabinducmb', 784);
  FGlyphCodes.Add('candrabindudeva', 2305);
  FGlyphCodes.Add('candrabindugujarati', 2689);
  FGlyphCodes.Add('capslock', 8682);
  FGlyphCodes.Add('careof', 8453);
  FGlyphCodes.Add('caron', 711);
  FGlyphCodes.Add('caronbelowcmb', 812);
  FGlyphCodes.Add('caroncmb', 780);
  FGlyphCodes.Add('carriagereturn', 8629);
  FGlyphCodes.Add('cbopomofo', 12568);
  FGlyphCodes.Add('ccaron', 269);
  FGlyphCodes.Add('ccedilla', 231);
  FGlyphCodes.Add('ccedillaacute', 7689);
  FGlyphCodes.Add('ccircle', 9426);
  FGlyphCodes.Add('ccircumflex', 265);
  FGlyphCodes.Add('ccurl', 597);
  FGlyphCodes.Add('cdot', 267);
  FGlyphCodes.Add('cdotaccent', 267);
  FGlyphCodes.Add('cdsquare', 13253);
  FGlyphCodes.Add('cedilla', 184);
  FGlyphCodes.Add('cedillacmb', 807);
  FGlyphCodes.Add('cent', 162);
  FGlyphCodes.Add('centigrade', 8451);
  FGlyphCodes.Add('centinferior', 63199);
  FGlyphCodes.Add('centmonospace', 65504);
  FGlyphCodes.Add('centoldstyle', 63394);
  FGlyphCodes.Add('centsuperior', 63200);
  FGlyphCodes.Add('chaarmenian', 1401);
  FGlyphCodes.Add('chabengali', 2459);
  FGlyphCodes.Add('chadeva', 2331);
  FGlyphCodes.Add('chagujarati', 2715);
  FGlyphCodes.Add('chagurmukhi', 2587);
  FGlyphCodes.Add('chbopomofo', 12564);
  FGlyphCodes.Add('cheabkhasiancyrillic', 1213);
  FGlyphCodes.Add('checkmark', 10003);
  FGlyphCodes.Add('checyrillic', 1095);
  FGlyphCodes.Add('chedescenderabkhasiancyrillic', 1215);
  FGlyphCodes.Add('chedescendercyrillic', 1207);
  FGlyphCodes.Add('chedieresiscyrillic', 1269);
  FGlyphCodes.Add('cheharmenian', 1395);
  FGlyphCodes.Add('chekhakassiancyrillic', 1228);
  FGlyphCodes.Add('cheverticalstrokecyrillic', 1209);
  FGlyphCodes.Add('chi', 967);
  FGlyphCodes.Add('chieuchacirclekorean', 12919);
  FGlyphCodes.Add('chieuchaparenkorean', 12823);
  FGlyphCodes.Add('chieuchcirclekorean', 12905);
  FGlyphCodes.Add('chieuchkorean', 12618);
  FGlyphCodes.Add('chieuchparenkorean', 12809);
  FGlyphCodes.Add('chochangthai', 3594);
  FGlyphCodes.Add('chochanthai', 3592);
  FGlyphCodes.Add('chochingthai', 3593);
  FGlyphCodes.Add('chochoethai', 3596);
  FGlyphCodes.Add('chook', 392);
  FGlyphCodes.Add('cieucacirclekorean', 12918);
  FGlyphCodes.Add('cieucaparenkorean', 12822);
  FGlyphCodes.Add('cieuccirclekorean', 12904);
  FGlyphCodes.Add('cieuckorean', 12616);
  FGlyphCodes.Add('cieucparenkorean', 12808);
  FGlyphCodes.Add('cieucuparenkorean', 12828);
  FGlyphCodes.Add('circle', 9675);
  FGlyphCodes.Add('circlemultiply', 8855);
  FGlyphCodes.Add('circleot', 8857);
  FGlyphCodes.Add('circleplus', 8853);
  FGlyphCodes.Add('circlepostalmark', 12342);
  FGlyphCodes.Add('circlewithlefthalfblack', 9680);
  FGlyphCodes.Add('circlewithrighthalfblack', 9681);
  FGlyphCodes.Add('circumflex', 710);
  FGlyphCodes.Add('circumflexbelowcmb', 813);
  FGlyphCodes.Add('circumflexcmb', 770);
  FGlyphCodes.Add('clear', 8999);
  FGlyphCodes.Add('clickalveolar', 450);
  FGlyphCodes.Add('clickdental', 448);
  FGlyphCodes.Add('clicklateral', 449);
  FGlyphCodes.Add('clickretroflex', 451);
  FGlyphCodes.Add('club', 9827);
  FGlyphCodes.Add('clubsuitblack', 9827);
  FGlyphCodes.Add('clubsuitwhite', 9831);
  FGlyphCodes.Add('cmcubedsquare', 13220);
  FGlyphCodes.Add('cmonospace', 65347);
  FGlyphCodes.Add('cmsquaredsquare', 13216);
  FGlyphCodes.Add('coarmenian', 1409);
  FGlyphCodes.Add('colon', 58);
  FGlyphCodes.Add('colonmonetary', 8353);
  FGlyphCodes.Add('colonmonospace', 65306);
  FGlyphCodes.Add('colonsign', 8353);
  FGlyphCodes.Add('colonsmall', 65109);
  FGlyphCodes.Add('colontriangularhalfmod', 721);
  FGlyphCodes.Add('colontriangularmod', 720);
  FGlyphCodes.Add('comma', 44);
  FGlyphCodes.Add('commaabovecmb', 787);
  FGlyphCodes.Add('commaaboverightcmb', 789);
  FGlyphCodes.Add('commaaccent', 63171);
  FGlyphCodes.Add('commaarabic', 1548);
  FGlyphCodes.Add('commaarmenian', 1373);
  FGlyphCodes.Add('commainferior', 63201);
  FGlyphCodes.Add('commamonospace', 65292);
  FGlyphCodes.Add('commareversedabovecmb', 788);
  FGlyphCodes.Add('commareversedmod', 701);
  FGlyphCodes.Add('commasmall', 65104);
  FGlyphCodes.Add('commasuperior', 63202);
  FGlyphCodes.Add('commaturnedabovecmb', 786);
  FGlyphCodes.Add('commaturnedmod', 699);
  FGlyphCodes.Add('compass', 9788);
  FGlyphCodes.Add('congruent', 8773);
  FGlyphCodes.Add('contourintegral', 8750);
  FGlyphCodes.Add('control', 8963);
  FGlyphCodes.Add('controlACK', 6);
  FGlyphCodes.Add('controlBEL', 7);
  FGlyphCodes.Add('controlBS', 8);
  FGlyphCodes.Add('controlCAN', 24);
  FGlyphCodes.Add('controlCR', 13);
  FGlyphCodes.Add('controlDC1', 17);
  FGlyphCodes.Add('controlDC2', 18);
  FGlyphCodes.Add('controlDC3', 19);
  FGlyphCodes.Add('controlDC4', 20);
  FGlyphCodes.Add('controlDEL', 127);
  FGlyphCodes.Add('controlDLE', 16);
  FGlyphCodes.Add('controlEM', 25);
  FGlyphCodes.Add('controlENQ', 5);
  FGlyphCodes.Add('controlEOT', 4);
  FGlyphCodes.Add('controlESC', 27);
  FGlyphCodes.Add('controlETB', 23);
  FGlyphCodes.Add('controlETX', 3);
  FGlyphCodes.Add('controlFF', 12);
  FGlyphCodes.Add('controlFS', 28);
  FGlyphCodes.Add('controlGS', 29);
  FGlyphCodes.Add('controlHT', 9);
  FGlyphCodes.Add('controlLF', 10);
  FGlyphCodes.Add('controlNAK', 21);
  FGlyphCodes.Add('controlRS', 30);
  FGlyphCodes.Add('controlSI', 15);
  FGlyphCodes.Add('controlSO', 14);
  FGlyphCodes.Add('controlSOT', 2);
  FGlyphCodes.Add('controlSTX', 1);
  FGlyphCodes.Add('controlSUB', 26);
  FGlyphCodes.Add('controlSYN', 22);
  FGlyphCodes.Add('controlUS', 31);
  FGlyphCodes.Add('controlVT', 11);
  FGlyphCodes.Add('copyright', 169);
  FGlyphCodes.Add('copyrightsans', 63721);
  FGlyphCodes.Add('copyrightserif', 63193);
  FGlyphCodes.Add('cornerbracketleft', 12300);
  FGlyphCodes.Add('cornerbracketlefthalfwidth', 65378);
  FGlyphCodes.Add('cornerbracketleftvertical', 65089);
  FGlyphCodes.Add('cornerbracketright', 12301);
  FGlyphCodes.Add('cornerbracketrighthalfwidth', 65379);
  FGlyphCodes.Add('cornerbracketrightvertical', 65090);
  FGlyphCodes.Add('corporationsquare', 13183);
  FGlyphCodes.Add('cosquare', 13255);
  FGlyphCodes.Add('coverkgsquare', 13254);
  FGlyphCodes.Add('cparen', 9374);
  FGlyphCodes.Add('cruzeiro', 8354);
  FGlyphCodes.Add('Csmall', 63331);
  FGlyphCodes.Add('cstretched', 663);
  FGlyphCodes.Add('curlyand', 8911);
  FGlyphCodes.Add('curlyor', 8910);
  FGlyphCodes.Add('currency', 164);
  FGlyphCodes.Add('cyrbreve', 63188);
  FGlyphCodes.Add('cyrBreve', 63185);
  FGlyphCodes.Add('cyrflex', 63189);
  FGlyphCodes.Add('cyrFlex', 63186);
  FGlyphCodes.Add('d', 100);
end;

procedure TdxFontFileUnicodeConverter.InitializePack3;
begin
  FGlyphCodes.Add('daarmenian', 1380);
  FGlyphCodes.Add('dabengali', 2470);
  FGlyphCodes.Add('dadarabic', 1590);
  FGlyphCodes.Add('dadeva', 2342);
  FGlyphCodes.Add('dadfinalarabic', 65214);
  FGlyphCodes.Add('dadinitialarabic', 65215);
  FGlyphCodes.Add('dadmedialarabic', 65216);
  FGlyphCodes.Add('dagesh', 1468);
  FGlyphCodes.Add('dageshhebrew', 1468);
  FGlyphCodes.Add('dagger', 8224);
  FGlyphCodes.Add('daggerdbl', 8225);
  FGlyphCodes.Add('dagujarati', 2726);
  FGlyphCodes.Add('dagurmukhi', 2598);
  FGlyphCodes.Add('dahiragana', 12384);
  FGlyphCodes.Add('dakatakana', 12480);
  FGlyphCodes.Add('dalarabic', 1583);
  FGlyphCodes.Add('dalet', 1491);
  FGlyphCodes.Add('daletdagesh', 64307);
  FGlyphCodes.Add('daletdageshhebrew', 64307);
  FGlyphCodes.Add('dalethebrew', 1491);
  FGlyphCodes.Add('dalfinalarabic', 65194);
  FGlyphCodes.Add('dammaarabic', 1615);
  FGlyphCodes.Add('dammalowarabic', 1615);
  FGlyphCodes.Add('dammatanaltonearabic', 1612);
  FGlyphCodes.Add('dammatanarabic', 1612);
  FGlyphCodes.Add('danda', 2404);
  FGlyphCodes.Add('dargahebrew', 1447);
  FGlyphCodes.Add('dargalefthebrew', 1447);
  FGlyphCodes.Add('dasiapneumatacyrilliccmb', 1157);
  FGlyphCodes.Add('dblanglebracketleft', 12298);
  FGlyphCodes.Add('dblanglebracketleftvertical', 65085);
  FGlyphCodes.Add('dblanglebracketright', 12299);
  FGlyphCodes.Add('dblanglebracketrightvertical', 65086);
  FGlyphCodes.Add('dblarchinvertedbelowcmb', 811);
  FGlyphCodes.Add('dblarrowleft', 8660);
  FGlyphCodes.Add('dblarrowright', 8658);
  FGlyphCodes.Add('dbldanda', 2405);
  FGlyphCodes.Add('dblgrave', 63190);
  FGlyphCodes.Add('dblGrave', 63187);
  FGlyphCodes.Add('dblgravecmb', 783);
  FGlyphCodes.Add('dblintegral', 8748);
  FGlyphCodes.Add('dbllowline', 8215);
  FGlyphCodes.Add('dbllowlinecmb', 819);
  FGlyphCodes.Add('dbloverlinecmb', 831);
  FGlyphCodes.Add('dblprimemod', 698);
  FGlyphCodes.Add('dblverticalbar', 8214);
  FGlyphCodes.Add('dblverticallineabovecmb', 782);
  FGlyphCodes.Add('dbopomofo', 12553);
  FGlyphCodes.Add('dbsquare', 13256);
  FGlyphCodes.Add('dcaron', 271);
  FGlyphCodes.Add('dcedilla', 7697);
  FGlyphCodes.Add('dcircle', 9427);
  FGlyphCodes.Add('dcircumflexbelow', 7699);
  FGlyphCodes.Add('dcroat', 273);
  FGlyphCodes.Add('ddabengali', 2465);
  FGlyphCodes.Add('ddadeva', 2337);
  FGlyphCodes.Add('ddagujarati', 2721);
  FGlyphCodes.Add('ddagurmukhi', 2593);
  FGlyphCodes.Add('ddalarabic', 1672);
  FGlyphCodes.Add('ddalfinalarabic', 64393);
  FGlyphCodes.Add('dddhadeva', 2396);
  FGlyphCodes.Add('ddhabengali', 2466);
  FGlyphCodes.Add('ddhadeva', 2338);
  FGlyphCodes.Add('ddhagujarati', 2722);
  FGlyphCodes.Add('ddhagurmukhi', 2594);
  FGlyphCodes.Add('ddotaccent', 7691);
  FGlyphCodes.Add('ddotbelow', 7693);
  FGlyphCodes.Add('decimalseparatorarabic', 1643);
  FGlyphCodes.Add('decimalseparatorpersian', 1643);
  FGlyphCodes.Add('decyrillic', 1076);
  FGlyphCodes.Add('degree', 176);
  FGlyphCodes.Add('dehihebrew', 1453);
  FGlyphCodes.Add('dehiragana', 12391);
  FGlyphCodes.Add('deicoptic', 1007);
  FGlyphCodes.Add('dekatakana', 12487);
  FGlyphCodes.Add('deleteleft', 9003);
  FGlyphCodes.Add('deleteright', 8998);
  FGlyphCodes.Add('delta', 948);
  FGlyphCodes.Add('deltaturned', 397);
  FGlyphCodes.Add('denominatorminusonenumeratorbengali', 2552);
  FGlyphCodes.Add('dezh', 676);
  FGlyphCodes.Add('dhabengali', 2471);
  FGlyphCodes.Add('dhadeva', 2343);
  FGlyphCodes.Add('dhagujarati', 2727);
  FGlyphCodes.Add('dhagurmukhi', 2599);
  FGlyphCodes.Add('dhook', 599);
  FGlyphCodes.Add('dialytikatonos', 901);
  FGlyphCodes.Add('dialytikatonoscmb', 836);
  FGlyphCodes.Add('diamond', 9830);
  FGlyphCodes.Add('diamondsuitwhite', 9826);
  FGlyphCodes.Add('dieresis', 168);
  FGlyphCodes.Add('dieresisacute', 63191);
  FGlyphCodes.Add('dieresisbelowcmb', 804);
  FGlyphCodes.Add('dieresiscmb', 776);
  FGlyphCodes.Add('dieresisgrave', 63192);
  FGlyphCodes.Add('dieresistonos', 901);
  FGlyphCodes.Add('dihiragana', 12386);
  FGlyphCodes.Add('dikatakana', 12482);
  FGlyphCodes.Add('dittomark', 12291);
  FGlyphCodes.Add('divide', 247);
  FGlyphCodes.Add('divides', 8739);
  FGlyphCodes.Add('divisionslash', 8725);
  FGlyphCodes.Add('djecyrillic', 1106);
  FGlyphCodes.Add('dkshade', 9619);
  FGlyphCodes.Add('dlinebelow', 7695);
  FGlyphCodes.Add('dlsquare', 13207);
  FGlyphCodes.Add('dmacron', 273);
  FGlyphCodes.Add('dmonospace', 65348);
  FGlyphCodes.Add('dnblock', 9604);
  FGlyphCodes.Add('dochadathai', 3598);
  FGlyphCodes.Add('dodekthai', 3604);
  FGlyphCodes.Add('dohiragana', 12393);
  FGlyphCodes.Add('dokatakana', 12489);
  FGlyphCodes.Add('dollar', 36);
  FGlyphCodes.Add('dollarinferior', 63203);
  FGlyphCodes.Add('dollarmonospace', 65284);
  FGlyphCodes.Add('dollaroldstyle', 63268);
  FGlyphCodes.Add('dollarsmall', 65129);
  FGlyphCodes.Add('dollarsuperior', 63204);
  FGlyphCodes.Add('dong', 8363);
  FGlyphCodes.Add('dorusquare', 13094);
  FGlyphCodes.Add('dotaccent', 729);
  FGlyphCodes.Add('dotaccentcmb', 775);
  FGlyphCodes.Add('dotbelowcmb', 803);
  FGlyphCodes.Add('dotbelowcomb', 803);
  FGlyphCodes.Add('dotkatakana', 12539);
  FGlyphCodes.Add('dotlessi', 305);
  FGlyphCodes.Add('dotlessj', 63166);
  FGlyphCodes.Add('dotlessjstrokehook', 644);
  FGlyphCodes.Add('dotmath', 8901);
  FGlyphCodes.Add('dottedcircle', 9676);
  FGlyphCodes.Add('doubleyodpatah', 64287);
  FGlyphCodes.Add('doubleyodpatahhebrew', 64287);
  FGlyphCodes.Add('downtackbelowcmb', 798);
  FGlyphCodes.Add('downtackmod', 725);
  FGlyphCodes.Add('dparen', 9375);
  FGlyphCodes.Add('dsuperior', 63211);
  FGlyphCodes.Add('dtail', 598);
  FGlyphCodes.Add('dtopbar', 396);
  FGlyphCodes.Add('duhiragana', 12389);
  FGlyphCodes.Add('dukatakana', 12485);
  FGlyphCodes.Add('dz', 499);
  FGlyphCodes.Add('dzaltone', 675);
  FGlyphCodes.Add('dzcaron', 454);
  FGlyphCodes.Add('dzcurl', 677);
  FGlyphCodes.Add('dzeabkhasiancyrillic', 1249);
  FGlyphCodes.Add('dzecyrillic', 1109);
  FGlyphCodes.Add('dzhecyrillic', 1119);
  FGlyphCodes.Add('e', 101);
  FGlyphCodes.Add('eacute', 233);
  FGlyphCodes.Add('earth', 9793);
  FGlyphCodes.Add('ebengali', 2447);
  FGlyphCodes.Add('ebopomofo', 12572);
  FGlyphCodes.Add('ebreve', 277);
  FGlyphCodes.Add('ecandradeva', 2317);
  FGlyphCodes.Add('ecandragujarati', 2701);
  FGlyphCodes.Add('ecandravowelsigndeva', 2373);
  FGlyphCodes.Add('ecandravowelsigngujarati', 2757);
  FGlyphCodes.Add('ecaron', 283);
  FGlyphCodes.Add('ecedillabreve', 7709);
  FGlyphCodes.Add('echarmenian', 1381);
  FGlyphCodes.Add('echyiwnarmenian', 1415);
  FGlyphCodes.Add('ecircle', 9428);
  FGlyphCodes.Add('ecircumflex', 234);
  FGlyphCodes.Add('ecircumflexacute', 7871);
  FGlyphCodes.Add('ecircumflexbelow', 7705);
  FGlyphCodes.Add('ecircumflexdotbelow', 7879);
  FGlyphCodes.Add('ecircumflexgrave', 7873);
  FGlyphCodes.Add('ecircumflexhookabove', 7875);
  FGlyphCodes.Add('ecircumflextilde', 7877);
  FGlyphCodes.Add('ecyrillic', 1108);
  FGlyphCodes.Add('edblgrave', 517);
  FGlyphCodes.Add('edeva', 2319);
  FGlyphCodes.Add('edieresis', 235);
  FGlyphCodes.Add('edot', 279);
  FGlyphCodes.Add('edotaccent', 279);
  FGlyphCodes.Add('edotbelow', 7865);
  FGlyphCodes.Add('eegurmukhi', 2575);
  FGlyphCodes.Add('eematragurmukhi', 2631);
  FGlyphCodes.Add('efcyrillic', 1092);
  FGlyphCodes.Add('egrave', 232);
  FGlyphCodes.Add('egujarati', 2703);
  FGlyphCodes.Add('eharmenian', 1383);
  FGlyphCodes.Add('ehbopomofo', 12573);
  FGlyphCodes.Add('ehiragana', 12360);
  FGlyphCodes.Add('ehookabove', 7867);
  FGlyphCodes.Add('eibopomofo', 12575);
  FGlyphCodes.Add('eight', 56);
  FGlyphCodes.Add('eightarabic', 1640);
  FGlyphCodes.Add('eightbengali', 2542);
  FGlyphCodes.Add('eightcircle', 9319);
  FGlyphCodes.Add('eightcircleinversesansserif', 10129);
  FGlyphCodes.Add('eightdeva', 2414);
  FGlyphCodes.Add('eighteencircle', 9329);
  FGlyphCodes.Add('eighteenparen', 9349);
  FGlyphCodes.Add('eighteenperiod', 9369);
  FGlyphCodes.Add('eightgujarati', 2798);
  FGlyphCodes.Add('eightgurmukhi', 2670);
  FGlyphCodes.Add('eighthackarabic', 1640);
  FGlyphCodes.Add('eighthangzhou', 12328);
  FGlyphCodes.Add('eighthnotebeamed', 9835);
  FGlyphCodes.Add('eightideographicparen', 12839);
  FGlyphCodes.Add('eightinferior', 8328);
  FGlyphCodes.Add('eightmonospace', 65304);
  FGlyphCodes.Add('eightoldstyle', 63288);
  FGlyphCodes.Add('eightparen', 9339);
  FGlyphCodes.Add('eightperiod', 9359);
  FGlyphCodes.Add('eightpersian', 1784);
  FGlyphCodes.Add('eightroman', 8567);
  FGlyphCodes.Add('eightsuperior', 8312);
  FGlyphCodes.Add('eightthai', 3672);
  FGlyphCodes.Add('einvertedbreve', 519);
  FGlyphCodes.Add('eiotifiedcyrillic', 1125);
  FGlyphCodes.Add('ekatakana', 12456);
  FGlyphCodes.Add('ekatakanahalfwidth', 65396);
  FGlyphCodes.Add('ekonkargurmukhi', 2676);
  FGlyphCodes.Add('ekorean', 12628);
  FGlyphCodes.Add('elcyrillic', 1083);
  FGlyphCodes.Add('element', 8712);
  FGlyphCodes.Add('elevencircle', 9322);
  FGlyphCodes.Add('elevenparen', 9342);
  FGlyphCodes.Add('elevenperiod', 9362);
  FGlyphCodes.Add('elevenroman', 8570);
  FGlyphCodes.Add('ellipsis', 8230);
  FGlyphCodes.Add('ellipsisvertical', 8942);
  FGlyphCodes.Add('emacron', 275);
  FGlyphCodes.Add('emacronacute', 7703);
  FGlyphCodes.Add('emacrongrave', 7701);
  FGlyphCodes.Add('emcyrillic', 1084);
  FGlyphCodes.Add('emdash', 8212);
  FGlyphCodes.Add('emdashvertical', 65073);
  FGlyphCodes.Add('emonospace', 65349);
  FGlyphCodes.Add('emphasismarkarmenian', 1371);
  FGlyphCodes.Add('emptyset', 8709);
  FGlyphCodes.Add('enbopomofo', 12579);
  FGlyphCodes.Add('encyrillic', 1085);
  FGlyphCodes.Add('endash', 8211);
  FGlyphCodes.Add('endashvertical', 65074);
  FGlyphCodes.Add('endescendercyrillic', 1187);
  FGlyphCodes.Add('eng', 331);
  FGlyphCodes.Add('engbopomofo', 12581);
  FGlyphCodes.Add('enghecyrillic', 1189);
  FGlyphCodes.Add('enhookcyrillic', 1224);
  FGlyphCodes.Add('enspace', 8194);
  FGlyphCodes.Add('eogonek', 281);
  FGlyphCodes.Add('eokorean', 12627);
  FGlyphCodes.Add('eopen', 603);
  FGlyphCodes.Add('eopenclosed', 666);
  FGlyphCodes.Add('eopenreversed', 604);
  FGlyphCodes.Add('eopenreversedclosed', 606);
  FGlyphCodes.Add('eopenreversedhook', 605);
  FGlyphCodes.Add('eparen', 9376);
  FGlyphCodes.Add('epsilon', 949);
  FGlyphCodes.Add('epsilontonos', 941);
  FGlyphCodes.Add('equal', 61);
  FGlyphCodes.Add('equalmonospace', 65309);
  FGlyphCodes.Add('equalsmall', 65126);
  FGlyphCodes.Add('equalsuperior', 8316);
  FGlyphCodes.Add('equivalence', 8801);
  FGlyphCodes.Add('erbopomofo', 12582);
  FGlyphCodes.Add('ercyrillic', 1088);
  FGlyphCodes.Add('ereversed', 600);
  FGlyphCodes.Add('ereversedcyrillic', 1101);
  FGlyphCodes.Add('escyrillic', 1089);
  FGlyphCodes.Add('esdescendercyrillic', 1195);
  FGlyphCodes.Add('esh', 643);
  FGlyphCodes.Add('eshcurl', 646);
  FGlyphCodes.Add('eshortdeva', 2318);
  FGlyphCodes.Add('eshortvowelsigndeva', 2374);
  FGlyphCodes.Add('eshreversedloop', 426);
  FGlyphCodes.Add('eshsquatreversed', 645);
  FGlyphCodes.Add('esmallhiragana', 12359);
  FGlyphCodes.Add('esmallkatakana', 12455);
  FGlyphCodes.Add('esmallkatakanahalfwidth', 65386);
  FGlyphCodes.Add('estimated', 8494);
  FGlyphCodes.Add('esuperior', 63212);
  FGlyphCodes.Add('eta', 951);
  FGlyphCodes.Add('etarmenian', 1384);
  FGlyphCodes.Add('etatonos', 942);
  FGlyphCodes.Add('eth', 240);
  FGlyphCodes.Add('etilde', 7869);
  FGlyphCodes.Add('etildebelow', 7707);
  FGlyphCodes.Add('etnahtafoukhhebrew', 1425);
  FGlyphCodes.Add('etnahtafoukhlefthebrew', 1425);
  FGlyphCodes.Add('etnahtahebrew', 1425);
  FGlyphCodes.Add('etnahtalefthebrew', 1425);
  FGlyphCodes.Add('eturned', 477);
  FGlyphCodes.Add('eukorean', 12641);
  FGlyphCodes.Add('euro', 8364);
  FGlyphCodes.Add('evowelsignbengali', 2503);
  FGlyphCodes.Add('evowelsigndeva', 2375);
  FGlyphCodes.Add('evowelsigngujarati', 2759);
  FGlyphCodes.Add('exclam', 33);
  FGlyphCodes.Add('exclamarmenian', 1372);
  FGlyphCodes.Add('exclamdbl', 8252);
  FGlyphCodes.Add('exclamdown', 161);
  FGlyphCodes.Add('exclamdownsmall', 63393);
  FGlyphCodes.Add('exclammonospace', 65281);
  FGlyphCodes.Add('exclamsmall', 63265);
  FGlyphCodes.Add('existential', 8707);
  FGlyphCodes.Add('ezh', 658);
  FGlyphCodes.Add('ezhcaron', 495);
  FGlyphCodes.Add('ezhcurl', 659);
  FGlyphCodes.Add('ezhreversed', 441);
  FGlyphCodes.Add('ezhtail', 442);
  FGlyphCodes.Add('f', 102);
  FGlyphCodes.Add('fadeva', 2398);
  FGlyphCodes.Add('fagurmukhi', 2654);
  FGlyphCodes.Add('fahrenheit', 8457);
  FGlyphCodes.Add('fathaarabic', 1614);
  FGlyphCodes.Add('fathalowarabic', 1614);
  FGlyphCodes.Add('fathatanarabic', 1611);
  FGlyphCodes.Add('fbopomofo', 12552);
  FGlyphCodes.Add('fcircle', 9429);
  FGlyphCodes.Add('fdotaccent', 7711);
  FGlyphCodes.Add('feharabic', 1601);
  FGlyphCodes.Add('feharmenian', 1414);
  FGlyphCodes.Add('fehfinalarabic', 65234);
  FGlyphCodes.Add('fehinitialarabic', 65235);
  FGlyphCodes.Add('fehmedialarabic', 65236);
  FGlyphCodes.Add('feicoptic', 997);
  FGlyphCodes.Add('female', 9792);
  FGlyphCodes.Add('ff', 64256);
  FGlyphCodes.Add('ffi', 64259);
  FGlyphCodes.Add('ffl', 64260);
  FGlyphCodes.Add('fi', 64257);
  FGlyphCodes.Add('fifteencircle', 9326);
  FGlyphCodes.Add('fifteenparen', 9346);
  FGlyphCodes.Add('fifteenperiod', 9366);
  FGlyphCodes.Add('figuredash', 8210);
  FGlyphCodes.Add('filledbox', 9632);
  FGlyphCodes.Add('filledrect', 9644);
  FGlyphCodes.Add('finalkaf', 1498);
  FGlyphCodes.Add('finalkafdagesh', 64314);
  FGlyphCodes.Add('finalkafdageshhebrew', 64314);
  FGlyphCodes.Add('finalkafhebrew', 1498);
  FGlyphCodes.Add('finalmem', 1501);
  FGlyphCodes.Add('finalmemhebrew', 1501);
  FGlyphCodes.Add('finalnun', 1503);
  FGlyphCodes.Add('finalnunhebrew', 1503);
  FGlyphCodes.Add('finalpe', 1507);
  FGlyphCodes.Add('finalpehebrew', 1507);
  FGlyphCodes.Add('finaltsadi', 1509);
  FGlyphCodes.Add('finaltsadihebrew', 1509);
  FGlyphCodes.Add('firsttonechinese', 713);
  FGlyphCodes.Add('fisheye', 9673);
  FGlyphCodes.Add('fitacyrillic', 1139);
  FGlyphCodes.Add('five', 53);
  FGlyphCodes.Add('fivearabic', 1637);
  FGlyphCodes.Add('fivebengali', 2539);
  FGlyphCodes.Add('fivecircle', 9316);
  FGlyphCodes.Add('fivecircleinversesansserif', 10126);
  FGlyphCodes.Add('fivedeva', 2411);
  FGlyphCodes.Add('fiveeighths', 8541);
  FGlyphCodes.Add('fivegujarati', 2795);
  FGlyphCodes.Add('fivegurmukhi', 2667);
  FGlyphCodes.Add('fivehackarabic', 1637);
  FGlyphCodes.Add('fivehangzhou', 12325);
  FGlyphCodes.Add('fiveideographicparen', 12836);
  FGlyphCodes.Add('fiveinferior', 8325);
  FGlyphCodes.Add('fivemonospace', 65301);
  FGlyphCodes.Add('fiveoldstyle', 63285);
  FGlyphCodes.Add('fiveparen', 9336);
  FGlyphCodes.Add('fiveperiod', 9356);
  FGlyphCodes.Add('fivepersian', 1781);
  FGlyphCodes.Add('fiveroman', 8564);
  FGlyphCodes.Add('fivesuperior', 8309);
  FGlyphCodes.Add('fivethai', 3669);
  FGlyphCodes.Add('fl', 64258);
  FGlyphCodes.Add('florin', 402);
  FGlyphCodes.Add('fmonospace', 65350);
  FGlyphCodes.Add('fmsquare', 13209);
  FGlyphCodes.Add('fofanthai', 3615);
  FGlyphCodes.Add('fofathai', 3613);
  FGlyphCodes.Add('fongmanthai', 3663);
  FGlyphCodes.Add('forall', 8704);
  FGlyphCodes.Add('four', 52);
  FGlyphCodes.Add('fourarabic', 1636);
  FGlyphCodes.Add('fourbengali', 2538);
  FGlyphCodes.Add('fourcircle', 9315);
  FGlyphCodes.Add('fourcircleinversesansserif', 10125);
  FGlyphCodes.Add('fourdeva', 2410);
  FGlyphCodes.Add('fourgujarati', 2794);
  FGlyphCodes.Add('fourgurmukhi', 2666);
  FGlyphCodes.Add('fourhackarabic', 1636);
  FGlyphCodes.Add('fourhangzhou', 12324);
  FGlyphCodes.Add('fourideographicparen', 12835);
  FGlyphCodes.Add('fourinferior', 8324);
  FGlyphCodes.Add('fourmonospace', 65300);
  FGlyphCodes.Add('fournumeratorbengali', 2551);
  FGlyphCodes.Add('fouroldstyle', 63284);
  FGlyphCodes.Add('fourparen', 9335);
  FGlyphCodes.Add('fourperiod', 9355);
  FGlyphCodes.Add('fourpersian', 1780);
  FGlyphCodes.Add('fourroman', 8563);
  FGlyphCodes.Add('Fourroman', 8547);
  FGlyphCodes.Add('foursuperior', 8308);
  FGlyphCodes.Add('fourteencircle', 9325);
  FGlyphCodes.Add('fourteenparen', 9345);
  FGlyphCodes.Add('fourteenperiod', 9365);
  FGlyphCodes.Add('fourthai', 3668);
  FGlyphCodes.Add('fourthtonechinese', 715);
  FGlyphCodes.Add('fparen', 9377);
  FGlyphCodes.Add('fraction', 8260);
  FGlyphCodes.Add('franc', 8355);
  FGlyphCodes.Add('g', 103);
  FGlyphCodes.Add('gabengali', 2455);
  FGlyphCodes.Add('gacute', 501);
  FGlyphCodes.Add('gadeva', 2327);
  FGlyphCodes.Add('gafarabic', 1711);
  FGlyphCodes.Add('gaffinalarabic', 64403);
  FGlyphCodes.Add('gafinitialarabic', 64404);
  FGlyphCodes.Add('gafmedialarabic', 64405);
  FGlyphCodes.Add('gagujarati', 2711);
  FGlyphCodes.Add('gagurmukhi', 2583);
  FGlyphCodes.Add('gahiragana', 12364);
  FGlyphCodes.Add('gakatakana', 12460);
  FGlyphCodes.Add('gamma', 947);
  FGlyphCodes.Add('gammalatinsmall', 611);
  FGlyphCodes.Add('gammasuperior', 736);
  FGlyphCodes.Add('gangiacoptic', 1003);
  FGlyphCodes.Add('gbopomofo', 12557);
  FGlyphCodes.Add('gbreve', 287);
  FGlyphCodes.Add('gcaron', 487);
  FGlyphCodes.Add('gcedilla', 291);
  FGlyphCodes.Add('gcircle', 9430);
  FGlyphCodes.Add('gcircumflex', 285);
  FGlyphCodes.Add('gcommaaccent', 291);
  FGlyphCodes.Add('gdot', 289);
  FGlyphCodes.Add('gdotaccent', 289);
  FGlyphCodes.Add('gecyrillic', 1075);
  FGlyphCodes.Add('gehiragana', 12370);
  FGlyphCodes.Add('gekatakana', 12466);
  FGlyphCodes.Add('geometricallyequal', 8785);
  FGlyphCodes.Add('gereshaccenthebrew', 1436);
  FGlyphCodes.Add('gereshhebrew', 1523);
  FGlyphCodes.Add('gereshmuqdamhebrew', 1437);
  FGlyphCodes.Add('germandbls', 223);
  FGlyphCodes.Add('gershayimaccenthebrew', 1438);
  FGlyphCodes.Add('gershayimhebrew', 1524);
  FGlyphCodes.Add('getamark', 12307);
  FGlyphCodes.Add('ghabengali', 2456);
  FGlyphCodes.Add('ghadarmenian', 1394);
  FGlyphCodes.Add('Ghadarmenian', 1346);
  FGlyphCodes.Add('ghadeva', 2328);
  FGlyphCodes.Add('ghagujarati', 2712);
  FGlyphCodes.Add('ghagurmukhi', 2584);
  FGlyphCodes.Add('ghainarabic', 1594);
  FGlyphCodes.Add('ghainfinalarabic', 65230);
  FGlyphCodes.Add('ghaininitialarabic', 65231);
  FGlyphCodes.Add('ghainmedialarabic', 65232);
  FGlyphCodes.Add('ghemiddlehookcyrillic', 1173);
  FGlyphCodes.Add('ghestrokecyrillic', 1171);
  FGlyphCodes.Add('gheupturncyrillic', 1169);
  FGlyphCodes.Add('ghhadeva', 2394);
  FGlyphCodes.Add('ghhagurmukhi', 2650);
  FGlyphCodes.Add('ghook', 608);
  FGlyphCodes.Add('ghzsquare', 13203);
  FGlyphCodes.Add('gihiragana', 12366);
  FGlyphCodes.Add('gikatakana', 12462);
  FGlyphCodes.Add('gimarmenian', 1379);
  FGlyphCodes.Add('gimel', 1490);
  FGlyphCodes.Add('gimeldagesh', 64306);
  FGlyphCodes.Add('gimeldageshhebrew', 64306);
  FGlyphCodes.Add('gimelhebrew', 1490);
  FGlyphCodes.Add('gjecyrillic', 1107);
  FGlyphCodes.Add('glottalinvertedstroke', 446);
  FGlyphCodes.Add('glottalstop', 660);
  FGlyphCodes.Add('glottalstopinverted', 662);
  FGlyphCodes.Add('glottalstopmod', 704);
  FGlyphCodes.Add('glottalstopreversed', 661);
  FGlyphCodes.Add('glottalstopreversedmod', 705);
  FGlyphCodes.Add('glottalstopreversedsuperior', 740);
  FGlyphCodes.Add('glottalstopstroke', 673);
  FGlyphCodes.Add('glottalstopstrokereversed', 674);
  FGlyphCodes.Add('gmacron', 7713);
  FGlyphCodes.Add('gmonospace', 65351);
  FGlyphCodes.Add('gohiragana', 12372);
  FGlyphCodes.Add('gokatakana', 12468);
  FGlyphCodes.Add('gparen', 9378);
  FGlyphCodes.Add('gpasquare', 13228);
  FGlyphCodes.Add('gradient', 8711);
  FGlyphCodes.Add('grave', 96);
  FGlyphCodes.Add('gravebelowcmb', 790);
  FGlyphCodes.Add('gravecmb', 768);
  FGlyphCodes.Add('gravecomb', 768);
  FGlyphCodes.Add('gravedeva', 2387);
  FGlyphCodes.Add('gravelowmod', 718);
  FGlyphCodes.Add('gravemonospace', 65344);
  FGlyphCodes.Add('gravetonecmb', 832);
  FGlyphCodes.Add('greater', 62);
  FGlyphCodes.Add('greaterequal', 8805);
  FGlyphCodes.Add('greaterequalorless', 8923);
  FGlyphCodes.Add('greatermonospace', 65310);
  FGlyphCodes.Add('greaterorequivalent', 8819);
  FGlyphCodes.Add('greaterorless', 8823);
  FGlyphCodes.Add('greateroverequal', 8807);
  FGlyphCodes.Add('greatersmall', 65125);
  FGlyphCodes.Add('gscript', 609);
  FGlyphCodes.Add('gstroke', 485);
  FGlyphCodes.Add('guhiragana', 12368);
  FGlyphCodes.Add('guillemotleft', 171);
  FGlyphCodes.Add('guillemotright', 187);
  FGlyphCodes.Add('guilsinglleft', 8249);
  FGlyphCodes.Add('guilsinglright', 8250);
  FGlyphCodes.Add('gukatakana', 12464);
  FGlyphCodes.Add('guramusquare', 13080);
  FGlyphCodes.Add('gysquare', 13257);
  FGlyphCodes.Add('h', 104);
  FGlyphCodes.Add('haabkhasiancyrillic', 1193);
  FGlyphCodes.Add('haaltonearabic', 1729);
  FGlyphCodes.Add('habengali', 2489);
  FGlyphCodes.Add('hadescendercyrillic', 1203);
  FGlyphCodes.Add('hadeva', 2361);
  FGlyphCodes.Add('hagujarati', 2745);
  FGlyphCodes.Add('hagurmukhi', 2617);
  FGlyphCodes.Add('haharabic', 1581);
  FGlyphCodes.Add('hahfinalarabic', 65186);
  FGlyphCodes.Add('hahinitialarabic', 65187);
  FGlyphCodes.Add('hahiragana', 12399);
  FGlyphCodes.Add('hahmedialarabic', 65188);
  FGlyphCodes.Add('haitusquare', 13098);
  FGlyphCodes.Add('hakatakana', 12495);
  FGlyphCodes.Add('hakatakanahalfwidth', 65418);
  FGlyphCodes.Add('halantgurmukhi', 2637);
  FGlyphCodes.Add('hamzaarabic', 1569);
  FGlyphCodes.Add('hamzalowarabic', 1569);
  FGlyphCodes.Add('hangulfiller', 12644);
  FGlyphCodes.Add('hardsigncyrillic', 1098);
  FGlyphCodes.Add('harpoonleftbarbup', 8636);
  FGlyphCodes.Add('harpoonrightbarbup', 8640);
  FGlyphCodes.Add('hasquare', 13258);
  FGlyphCodes.Add('hatafpatah', 1458);
  FGlyphCodes.Add('hatafpatah16', 1458);
  FGlyphCodes.Add('hatafpatah23', 1458);
  FGlyphCodes.Add('hatafpatah2f', 1458);
  FGlyphCodes.Add('hatafpatahhebrew', 1458);
  FGlyphCodes.Add('hatafpatahnarrowhebrew', 1458);
  FGlyphCodes.Add('hatafpatahquarterhebrew', 1458);
  FGlyphCodes.Add('hatafpatahwidehebrew', 1458);
  FGlyphCodes.Add('hatafqamats', 1459);
  FGlyphCodes.Add('hatafqamats1b', 1459);
  FGlyphCodes.Add('hatafqamats28', 1459);
  FGlyphCodes.Add('hatafqamats34', 1459);
  FGlyphCodes.Add('hatafqamatshebrew', 1459);
  FGlyphCodes.Add('hatafqamatsnarrowhebrew', 1459);
  FGlyphCodes.Add('hatafqamatsquarterhebrew', 1459);
  FGlyphCodes.Add('hatafqamatswidehebrew', 1459);
  FGlyphCodes.Add('hatafsegol', 1457);
  FGlyphCodes.Add('hatafsegol17', 1457);
  FGlyphCodes.Add('hatafsegol24', 1457);
  FGlyphCodes.Add('hatafsegol30', 1457);
  FGlyphCodes.Add('hatafsegolhebrew', 1457);
  FGlyphCodes.Add('hatafsegolnarrowhebrew', 1457);
  FGlyphCodes.Add('hatafsegolquarterhebrew', 1457);
  FGlyphCodes.Add('hatafsegolwidehebrew', 1457);
  FGlyphCodes.Add('hbar', 295);
  FGlyphCodes.Add('hbopomofo', 12559);
  FGlyphCodes.Add('hbrevebelow', 7723);
  FGlyphCodes.Add('hcedilla', 7721);
  FGlyphCodes.Add('hcircle', 9431);
  FGlyphCodes.Add('hcircumflex', 293);
  FGlyphCodes.Add('hdieresis', 7719);
  FGlyphCodes.Add('hdotaccent', 7715);
  FGlyphCodes.Add('hdotbelow', 7717);
  FGlyphCodes.Add('he', 1492);
  FGlyphCodes.Add('heart', 9829);
  FGlyphCodes.Add('heartsuitblack', 9829);
  FGlyphCodes.Add('heartsuitwhite', 9825);
  FGlyphCodes.Add('hedagesh', 64308);
  FGlyphCodes.Add('hedageshhebrew', 64308);
  FGlyphCodes.Add('hehaltonearabic', 1729);
  FGlyphCodes.Add('heharabic', 1607);
  FGlyphCodes.Add('hehebrew', 1492);
  FGlyphCodes.Add('hehfinalaltonearabic', 64423);
  FGlyphCodes.Add('hehfinalalttwoarabic', 65258);
  FGlyphCodes.Add('hehfinalarabic', 65258);
  FGlyphCodes.Add('hehhamzaabovefinalarabic', 64421);
  FGlyphCodes.Add('hehhamzaaboveisolatedarabic', 64420);
  FGlyphCodes.Add('hehinitialaltonearabic', 64424);
  FGlyphCodes.Add('hehinitialarabic', 65259);
  FGlyphCodes.Add('hehiragana', 12408);
  FGlyphCodes.Add('hehmedialaltonearabic', 64425);
  FGlyphCodes.Add('hehmedialarabic', 65260);
  FGlyphCodes.Add('heiseierasquare', 13179);
  FGlyphCodes.Add('hekatakana', 12504);
  FGlyphCodes.Add('hekatakanahalfwidth', 65421);
  FGlyphCodes.Add('hekutaarusquare', 13110);
  FGlyphCodes.Add('henghook', 615);
  FGlyphCodes.Add('herutusquare', 13113);
  FGlyphCodes.Add('het', 1495);
  FGlyphCodes.Add('hethebrew', 1495);
  FGlyphCodes.Add('hhook', 614);
  FGlyphCodes.Add('hhooksuperior', 689);
  FGlyphCodes.Add('hieuhacirclekorean', 12923);
  FGlyphCodes.Add('hieuhaparenkorean', 12827);
  FGlyphCodes.Add('hieuhcirclekorean', 12909);
  FGlyphCodes.Add('hieuhkorean', 12622);
  FGlyphCodes.Add('hieuhparenkorean', 12813);
  FGlyphCodes.Add('hihiragana', 12402);
  FGlyphCodes.Add('hikatakana', 12498);
  FGlyphCodes.Add('hikatakanahalfwidth', 65419);
  FGlyphCodes.Add('hiriq', 1460);
  FGlyphCodes.Add('hiriq14', 1460);
  FGlyphCodes.Add('hiriq21', 1460);
  FGlyphCodes.Add('hiriq2d', 1460);
  FGlyphCodes.Add('hiriqhebrew', 1460);
  FGlyphCodes.Add('hiriqnarrowhebrew', 1460);
  FGlyphCodes.Add('hiriqquarterhebrew', 1460);
  FGlyphCodes.Add('hiriqwidehebrew', 1460);
  FGlyphCodes.Add('hlinebelow', 7830);
  FGlyphCodes.Add('hmonospace', 65352);
  FGlyphCodes.Add('hoarmenian', 1392);
  FGlyphCodes.Add('hohipthai', 3627);
  FGlyphCodes.Add('hohiragana', 12411);
  FGlyphCodes.Add('hokatakana', 12507);
  FGlyphCodes.Add('hokatakanahalfwidth', 65422);
  FGlyphCodes.Add('holam', 1465);
  FGlyphCodes.Add('holam19', 1465);
  FGlyphCodes.Add('holam26', 1465);
  FGlyphCodes.Add('holam32', 1465);
  FGlyphCodes.Add('holamhebrew', 1465);
  FGlyphCodes.Add('holamnarrowhebrew', 1465);
  FGlyphCodes.Add('holamquarterhebrew', 1465);
  FGlyphCodes.Add('holamwidehebrew', 1465);
  FGlyphCodes.Add('honokhukthai', 3630);
  FGlyphCodes.Add('hookabovecomb', 777);
  FGlyphCodes.Add('hookcmb', 777);
  FGlyphCodes.Add('hookpalatalizedbelowcmb', 801);
  FGlyphCodes.Add('hookretroflexbelowcmb', 802);
  FGlyphCodes.Add('hoonsquare', 13122);
  FGlyphCodes.Add('horicoptic', 1001);
  FGlyphCodes.Add('horizontalbar', 8213);
  FGlyphCodes.Add('horncmb', 795);
  FGlyphCodes.Add('hotsprings', 9832);
  FGlyphCodes.Add('house', 8962);
  FGlyphCodes.Add('hparen', 9379);
  FGlyphCodes.Add('hsuperior', 688);
  FGlyphCodes.Add('hturned', 613);
  FGlyphCodes.Add('huhiragana', 12405);
  FGlyphCodes.Add('huiitosquare', 13107);
  FGlyphCodes.Add('hukatakana', 12501);
  FGlyphCodes.Add('hukatakanahalfwidth', 65420);
  FGlyphCodes.Add('hungarumlaut', 733);
  FGlyphCodes.Add('hungarumlautcmb', 779);
  FGlyphCodes.Add('hv', 405);
  FGlyphCodes.Add('hyphen', 45);
  FGlyphCodes.Add('hypheninferior', 63205);
  FGlyphCodes.Add('hyphenmonospace', 65293);
  FGlyphCodes.Add('hyphensmall', 65123);
  FGlyphCodes.Add('hyphensuperior', 63206);
  FGlyphCodes.Add('hyphentwo', 8208);
  FGlyphCodes.Add('i', 105);
  FGlyphCodes.Add('iacute', 237);
  FGlyphCodes.Add('iacyrillic', 1103);
  FGlyphCodes.Add('ibengali', 2439);
  FGlyphCodes.Add('ibopomofo', 12583);
  FGlyphCodes.Add('ibreve', 301);
  FGlyphCodes.Add('icaron', 464);
  FGlyphCodes.Add('icircle', 9432);
  FGlyphCodes.Add('icircumflex', 238);
  FGlyphCodes.Add('icyrillic', 1110);
  FGlyphCodes.Add('idblgrave', 521);
  FGlyphCodes.Add('ideographearthcircle', 12943);
  FGlyphCodes.Add('ideographfirecircle', 12939);
  FGlyphCodes.Add('ideographicallianceparen', 12863);
  FGlyphCodes.Add('ideographiccallparen', 12858);
  FGlyphCodes.Add('ideographiccentrecircle', 12965);
  FGlyphCodes.Add('ideographicclose', 12294);
  FGlyphCodes.Add('ideographiccomma', 12289);
  FGlyphCodes.Add('ideographiccommaleft', 65380);
  FGlyphCodes.Add('ideographiccongratulationparen', 12855);
  FGlyphCodes.Add('ideographiccorrectcircle', 12963);
  FGlyphCodes.Add('ideographicearthparen', 12847);
  FGlyphCodes.Add('ideographicenterpriseparen', 12861);
  FGlyphCodes.Add('ideographicexcellentcircle', 12957);
  FGlyphCodes.Add('ideographicfestivalparen', 12864);
  FGlyphCodes.Add('ideographicfinancialcircle', 12950);
  FGlyphCodes.Add('ideographicfinancialparen', 12854);
  FGlyphCodes.Add('ideographicfireparen', 12843);
  FGlyphCodes.Add('ideographichaveparen', 12850);
  FGlyphCodes.Add('ideographichighcircle', 12964);
  FGlyphCodes.Add('ideographiciterationmark', 12293);
  FGlyphCodes.Add('ideographiclaborcircle', 12952);
  FGlyphCodes.Add('ideographiclaborparen', 12856);
  FGlyphCodes.Add('ideographicleftcircle', 12967);
  FGlyphCodes.Add('ideographiclowcircle', 12966);
  FGlyphCodes.Add('ideographicmedicinecircle', 12969);
  FGlyphCodes.Add('ideographicmetalparen', 12846);
  FGlyphCodes.Add('ideographicmoonparen', 12842);
  FGlyphCodes.Add('ideographicnameparen', 12852);
  FGlyphCodes.Add('ideographicperiod', 12290);
  FGlyphCodes.Add('ideographicprintcircle', 12958);
  FGlyphCodes.Add('ideographicreachparen', 12867);
  FGlyphCodes.Add('ideographicrepresentparen', 12857);
  FGlyphCodes.Add('ideographicresourceparen', 12862);
  FGlyphCodes.Add('ideographicrightcircle', 12968);
  FGlyphCodes.Add('ideographicsecretcircle', 12953);
  FGlyphCodes.Add('ideographicselfparen', 12866);
  FGlyphCodes.Add('ideographicsocietyparen', 12851);
  FGlyphCodes.Add('ideographicspace', 12288);
  FGlyphCodes.Add('ideographicspecialparen', 12853);
  FGlyphCodes.Add('ideographicstockparen', 12849);
  FGlyphCodes.Add('ideographicstudyparen', 12859);
  FGlyphCodes.Add('ideographicsunparen', 12848);
  FGlyphCodes.Add('ideographicsuperviseparen', 12860);
  FGlyphCodes.Add('ideographicwaterparen', 12844);
  FGlyphCodes.Add('ideographicwoodparen', 12845);
  FGlyphCodes.Add('ideographiczero', 12295);
  FGlyphCodes.Add('ideographmetalcircle', 12942);
  FGlyphCodes.Add('ideographmooncircle', 12938);
  FGlyphCodes.Add('ideographnamecircle', 12948);
  FGlyphCodes.Add('ideographsuncircle', 12944);
  FGlyphCodes.Add('ideographwatercircle', 12940);
  FGlyphCodes.Add('ideographwoodcircle', 12941);
  FGlyphCodes.Add('ideva', 2311);
  FGlyphCodes.Add('idieresis', 239);
  FGlyphCodes.Add('idieresisacute', 7727);
  FGlyphCodes.Add('idieresiscyrillic', 1253);
  FGlyphCodes.Add('idotbelow', 7883);
  FGlyphCodes.Add('iebrevecyrillic', 1239);
  FGlyphCodes.Add('iecyrillic', 1077);
  FGlyphCodes.Add('ieungacirclekorean', 12917);
  FGlyphCodes.Add('ieungaparenkorean', 12821);
  FGlyphCodes.Add('ieungcirclekorean', 12903);
  FGlyphCodes.Add('ieungkorean', 12615);
  FGlyphCodes.Add('ieungparenkorean', 12807);
  FGlyphCodes.Add('igrave', 236);
  FGlyphCodes.Add('igujarati', 2695);
  FGlyphCodes.Add('igurmukhi', 2567);
  FGlyphCodes.Add('ihiragana', 12356);
  FGlyphCodes.Add('ihookabove', 7881);
  FGlyphCodes.Add('iibengali', 2440);
  FGlyphCodes.Add('iicyrillic', 1080);
  FGlyphCodes.Add('iideva', 2312);
  FGlyphCodes.Add('iigujarati', 2696);
  FGlyphCodes.Add('iigurmukhi', 2568);
  FGlyphCodes.Add('iimatragurmukhi', 2624);
  FGlyphCodes.Add('iinvertedbreve', 523);
  FGlyphCodes.Add('iishortcyrillic', 1081);
  FGlyphCodes.Add('iivowelsignbengali', 2496);
  FGlyphCodes.Add('iivowelsigndeva', 2368);
  FGlyphCodes.Add('iivowelsigngujarati', 2752);
  FGlyphCodes.Add('ij', 307);
  FGlyphCodes.Add('ikatakana', 12452);
  FGlyphCodes.Add('ikatakanahalfwidth', 65394);
  FGlyphCodes.Add('ikorean', 12643);
  FGlyphCodes.Add('ilde', 732);
  FGlyphCodes.Add('iluyhebrew', 1452);
  FGlyphCodes.Add('imacron', 299);
  FGlyphCodes.Add('imacroncyrillic', 1251);
  FGlyphCodes.Add('imageorapproximatelyequal', 8787);
  FGlyphCodes.Add('imatragurmukhi', 2623);
  FGlyphCodes.Add('imonospace', 65353);
  FGlyphCodes.Add('increment', 8710);
  FGlyphCodes.Add('infinity', 8734);
  FGlyphCodes.Add('iniarmenian', 1387);
  FGlyphCodes.Add('integral', 8747);
  FGlyphCodes.Add('integralbottom', 8993);
  FGlyphCodes.Add('integralbt', 8993);
  FGlyphCodes.Add('integralex', 63733);
  FGlyphCodes.Add('integraltop', 8992);
  FGlyphCodes.Add('integraltp', 8992);
  FGlyphCodes.Add('intersection', 8745);
  FGlyphCodes.Add('intisquare', 13061);
  FGlyphCodes.Add('invbullet', 9688);
  FGlyphCodes.Add('invcircle', 9689);
  FGlyphCodes.Add('invsmileface', 9787);
  FGlyphCodes.Add('iocyrillic', 1105);
  FGlyphCodes.Add('iogonek', 303);
  FGlyphCodes.Add('iota', 953);
  FGlyphCodes.Add('iotadieresis', 970);
  FGlyphCodes.Add('iotadieresistonos', 912);
  FGlyphCodes.Add('iotalatin', 617);
  FGlyphCodes.Add('iotatonos', 943);
  FGlyphCodes.Add('iparen', 9380);
  FGlyphCodes.Add('irigurmukhi', 2674);
  FGlyphCodes.Add('ismallhiragana', 12355);
  FGlyphCodes.Add('ismallkatakana', 12451);
  FGlyphCodes.Add('ismallkatakanahalfwidth', 65384);
  FGlyphCodes.Add('issharbengali', 2554);
  FGlyphCodes.Add('istroke', 616);
  FGlyphCodes.Add('isuperior', 63213);
  FGlyphCodes.Add('iterationhiragana', 12445);
  FGlyphCodes.Add('iterationkatakana', 12541);
  FGlyphCodes.Add('itilde', 297);
  FGlyphCodes.Add('itildebelow', 7725);
  FGlyphCodes.Add('iubopomofo', 12585);
  FGlyphCodes.Add('iucyrillic', 1102);
  FGlyphCodes.Add('ivowelsignbengali', 2495);
  FGlyphCodes.Add('ivowelsigndeva', 2367);
  FGlyphCodes.Add('ivowelsigngujarati', 2751);
  FGlyphCodes.Add('izhitsacyrillic', 1141);
  FGlyphCodes.Add('izhitsadblgravecyrillic', 1143);
  FGlyphCodes.Add('j', 106);
  FGlyphCodes.Add('jaarmenian', 1393);
  FGlyphCodes.Add('jabengali', 2460);
  FGlyphCodes.Add('jadeva', 2332);
  FGlyphCodes.Add('jagujarati', 2716);
  FGlyphCodes.Add('jagurmukhi', 2588);
  FGlyphCodes.Add('jbopomofo', 12560);
  FGlyphCodes.Add('jcaron', 496);
  FGlyphCodes.Add('jcircle', 9433);
  FGlyphCodes.Add('jcircumflex', 309);
  FGlyphCodes.Add('jcrossedtail', 669);
  FGlyphCodes.Add('jdotlessstroke', 607);
  FGlyphCodes.Add('jecyrillic', 1112);
  FGlyphCodes.Add('jeemarabic', 1580);
  FGlyphCodes.Add('jeemfinalarabic', 65182);
  FGlyphCodes.Add('jeeminitialarabic', 65183);
  FGlyphCodes.Add('jeemmedialarabic', 65184);
  FGlyphCodes.Add('jeharabic', 1688);
  FGlyphCodes.Add('jehfinalarabic', 64395);
  FGlyphCodes.Add('jhabengali', 2461);
  FGlyphCodes.Add('jhadeva', 2333);
  FGlyphCodes.Add('jhagujarati', 2717);
  FGlyphCodes.Add('jhagurmukhi', 2589);
  FGlyphCodes.Add('jheharmenian', 1403);
  FGlyphCodes.Add('jis', 12292);
  FGlyphCodes.Add('jmonospace', 65354);
  FGlyphCodes.Add('jparen', 9381);
  FGlyphCodes.Add('jsuperior', 690);
  FGlyphCodes.Add('k', 107);
  FGlyphCodes.Add('kabashkircyrillic', 1185);
  FGlyphCodes.Add('kabengali', 2453);
  FGlyphCodes.Add('kacute', 7729);
  FGlyphCodes.Add('kacyrillic', 1082);
  FGlyphCodes.Add('kadescendercyrillic', 1179);
  FGlyphCodes.Add('kadeva', 2325);
  FGlyphCodes.Add('kaf', 1499);
  FGlyphCodes.Add('kafarabic', 1603);
  FGlyphCodes.Add('kafdagesh', 64315);
  FGlyphCodes.Add('kafdageshhebrew', 64315);
  FGlyphCodes.Add('kaffinalarabic', 65242);
  FGlyphCodes.Add('kafhebrew', 1499);
  FGlyphCodes.Add('kafinitialarabic', 65243);
  FGlyphCodes.Add('kafmedialarabic', 65244);
  FGlyphCodes.Add('kafrafehebrew', 64333);
  FGlyphCodes.Add('kagujarati', 2709);
  FGlyphCodes.Add('kagurmukhi', 2581);
  FGlyphCodes.Add('kahiragana', 12363);
  FGlyphCodes.Add('kahookcyrillic', 1220);
  FGlyphCodes.Add('kakatakana', 12459);
  FGlyphCodes.Add('kakatakanahalfwidth', 65398);
  FGlyphCodes.Add('kappa', 954);
  FGlyphCodes.Add('kappasymbolgreek', 1008);
  FGlyphCodes.Add('kapyeounmieumkorean', 12657);
  FGlyphCodes.Add('kapyeounphieuphkorean', 12676);
  FGlyphCodes.Add('kapyeounpieupkorean', 12664);
  FGlyphCodes.Add('kapyeounssangpieupkorean', 12665);
  FGlyphCodes.Add('karoriisquare', 13069);
  FGlyphCodes.Add('kashidaautoarabic', 1600);
  FGlyphCodes.Add('kashidaautonosidebearingarabic', 1600);
  FGlyphCodes.Add('kasmallkatakana', 12533);
  FGlyphCodes.Add('kasquare', 13188);
  FGlyphCodes.Add('kasraarabic', 1616);
  FGlyphCodes.Add('kasratanarabic', 1613);
  FGlyphCodes.Add('kastrokecyrillic', 1183);
  FGlyphCodes.Add('katahiraprolongmarkhalfwidth', 65392);
  FGlyphCodes.Add('kaverticalstrokecyrillic', 1181);
  FGlyphCodes.Add('kbopomofo', 12558);
  FGlyphCodes.Add('kcalsquare', 13193);
  FGlyphCodes.Add('kcaron', 489);
  FGlyphCodes.Add('kcedilla', 311);
  FGlyphCodes.Add('kcircle', 9434);
  FGlyphCodes.Add('kcommaaccent', 311);
  FGlyphCodes.Add('kdotbelow', 7731);
  FGlyphCodes.Add('keharmenian', 1412);
  FGlyphCodes.Add('kehiragana', 12369);
  FGlyphCodes.Add('kekatakana', 12465);
  FGlyphCodes.Add('kekatakanahalfwidth', 65401);
  FGlyphCodes.Add('kenarmenian', 1391);
  FGlyphCodes.Add('kesmallkatakana', 12534);
  FGlyphCodes.Add('kgreenlandic', 312);
  FGlyphCodes.Add('khabengali', 2454);
  FGlyphCodes.Add('khacyrillic', 1093);
  FGlyphCodes.Add('khadeva', 2326);
  FGlyphCodes.Add('khagujarati', 2710);
  FGlyphCodes.Add('khagurmukhi', 2582);
  FGlyphCodes.Add('khaharabic', 1582);
  FGlyphCodes.Add('khahfinalarabic', 65190);
  FGlyphCodes.Add('khahinitialarabic', 65191);
  FGlyphCodes.Add('khahmedialarabic', 65192);
  FGlyphCodes.Add('kheicoptic', 999);
  FGlyphCodes.Add('khhadeva', 2393);
  FGlyphCodes.Add('khhagurmukhi', 2649);
  FGlyphCodes.Add('khieukhacirclekorean', 12920);
  FGlyphCodes.Add('khieukhaparenkorean', 12824);
  FGlyphCodes.Add('khieukhcirclekorean', 12906);
  FGlyphCodes.Add('khieukhkorean', 12619);
  FGlyphCodes.Add('khieukhparenkorean', 12810);
  FGlyphCodes.Add('khokhaithai', 3586);
  FGlyphCodes.Add('khokhonthai', 3589);
  FGlyphCodes.Add('khokhuatthai', 3587);
  FGlyphCodes.Add('khokhwaithai', 3588);
  FGlyphCodes.Add('khomutthai', 3675);
  FGlyphCodes.Add('khook', 409);
  FGlyphCodes.Add('khorakhangthai', 3590);
  FGlyphCodes.Add('khzsquare', 13201);
  FGlyphCodes.Add('kihiragana', 12365);
  FGlyphCodes.Add('kikatakana', 12461);
  FGlyphCodes.Add('kikatakanahalfwidth', 65399);
  FGlyphCodes.Add('kiroguramusquare', 13077);
  FGlyphCodes.Add('kiromeetorusquare', 13078);
  FGlyphCodes.Add('kirosquare', 13076);
  FGlyphCodes.Add('kiyeokacirclekorean', 12910);
  FGlyphCodes.Add('kiyeokaparenkorean', 12814);
  FGlyphCodes.Add('kiyeokcirclekorean', 12896);
  FGlyphCodes.Add('kiyeokkorean', 12593);
  FGlyphCodes.Add('kiyeokparenkorean', 12800);
  FGlyphCodes.Add('kiyeoksioskorean', 12595);
  FGlyphCodes.Add('kjecyrillic', 1116);
  FGlyphCodes.Add('klinebelow', 7733);
  FGlyphCodes.Add('klsquare', 13208);
  FGlyphCodes.Add('kmcubedsquare', 13222);
  FGlyphCodes.Add('kmonospace', 65355);
  FGlyphCodes.Add('kmsquaredsquare', 13218);
  FGlyphCodes.Add('kohiragana', 12371);
  FGlyphCodes.Add('kohmsquare', 13248);
  FGlyphCodes.Add('kokaithai', 3585);
  FGlyphCodes.Add('kokatakana', 12467);
  FGlyphCodes.Add('kokatakanahalfwidth', 65402);
  FGlyphCodes.Add('kooposquare', 13086);
  FGlyphCodes.Add('koppacyrillic', 1153);
  FGlyphCodes.Add('koreanstandardsymbol', 12927);
  FGlyphCodes.Add('koroniscmb', 835);
  FGlyphCodes.Add('kparen', 9382);
  FGlyphCodes.Add('kpasquare', 13226);
  FGlyphCodes.Add('ksicyrillic', 1135);
  FGlyphCodes.Add('ktsquare', 13263);
  FGlyphCodes.Add('kturned', 670);
  FGlyphCodes.Add('kuhiragana', 12367);
  FGlyphCodes.Add('kukatakana', 12463);
  FGlyphCodes.Add('kukatakanahalfwidth', 65400);
  FGlyphCodes.Add('kvsquare', 13240);
  FGlyphCodes.Add('kwsquare', 13246);
  FGlyphCodes.Add('l', 108);
  FGlyphCodes.Add('labengali', 2482);
  FGlyphCodes.Add('lacute', 314);
  FGlyphCodes.Add('ladeva', 2354);
  FGlyphCodes.Add('lagujarati', 2738);
  FGlyphCodes.Add('lagurmukhi', 2610);
  FGlyphCodes.Add('lakkhangyaothai', 3653);
  FGlyphCodes.Add('lamaleffinalarabic', 65276);
  FGlyphCodes.Add('lamalefhamzaabovefinalarabic', 65272);
  FGlyphCodes.Add('lamalefhamzaaboveisolatedarabic', 65271);
  FGlyphCodes.Add('lamalefhamzabelowfinalarabic', 65274);
  FGlyphCodes.Add('lamalefhamzabelowisolatedarabic', 65273);
  FGlyphCodes.Add('lamalefisolatedarabic', 65275);
  FGlyphCodes.Add('lamalefmaddaabovefinalarabic', 65270);
  FGlyphCodes.Add('lamalefmaddaaboveisolatedarabic', 65269);
  FGlyphCodes.Add('lamarabic', 1604);
  FGlyphCodes.Add('lambda', 955);
  FGlyphCodes.Add('lambdastroke', 411);
  FGlyphCodes.Add('lamed', 1500);
  FGlyphCodes.Add('lameddagesh', 64316);
  FGlyphCodes.Add('lameddageshhebrew', 64316);
  FGlyphCodes.Add('lamedhebrew', 1500);
  FGlyphCodes.Add('lamfinalarabic', 65246);
  FGlyphCodes.Add('lamhahinitialarabic', 64714);
  FGlyphCodes.Add('laminitialarabic', 65247);
  FGlyphCodes.Add('lamjeeminitialarabic', 64713);
  FGlyphCodes.Add('lamkhahinitialarabic', 64715);
  FGlyphCodes.Add('lamlamhehisolatedarabic', 65010);
  FGlyphCodes.Add('lammedialarabic', 65248);
  FGlyphCodes.Add('lammeemhahinitialarabic', 64904);
  FGlyphCodes.Add('lammeeminitialarabic', 64716);
  FGlyphCodes.Add('largecircle', 9711);
  FGlyphCodes.Add('lbar', 410);
  FGlyphCodes.Add('lbelt', 620);
  FGlyphCodes.Add('lbopomofo', 12556);
  FGlyphCodes.Add('lcaron', 318);
  FGlyphCodes.Add('lcedilla', 316);
  FGlyphCodes.Add('lcircle', 9435);
  FGlyphCodes.Add('lcircumflexbelow', 7741);
  FGlyphCodes.Add('lcommaaccent', 316);
  FGlyphCodes.Add('ldot', 320);
  FGlyphCodes.Add('ldotaccent', 320);
  FGlyphCodes.Add('ldotbelow', 7735);
  FGlyphCodes.Add('ldotbelowmacron', 7737);
  FGlyphCodes.Add('leftangleabovecmb', 794);
  FGlyphCodes.Add('lefttackbelowcmb', 792);
  FGlyphCodes.Add('less', 60);
  FGlyphCodes.Add('lessequal', 8804);
  FGlyphCodes.Add('lessequalorgreater', 8922);
  FGlyphCodes.Add('lessmonospace', 65308);
  FGlyphCodes.Add('lessorequivalent', 8818);
  FGlyphCodes.Add('lessorgreater', 8822);
  FGlyphCodes.Add('lessoverequal', 8806);
  FGlyphCodes.Add('lesssmall', 65124);
  FGlyphCodes.Add('lezh', 622);
  FGlyphCodes.Add('lfblock', 9612);
  FGlyphCodes.Add('lhookretroflex', 621);
  FGlyphCodes.Add('lira', 8356);
  FGlyphCodes.Add('liwnarmenian', 1388);
  FGlyphCodes.Add('lj', 457);
  FGlyphCodes.Add('ljecyrillic', 1113);
  FGlyphCodes.Add('ll', 63168);
  FGlyphCodes.Add('lladeva', 2355);
  FGlyphCodes.Add('llagujarati', 2739);
  FGlyphCodes.Add('llinebelow', 7739);
  FGlyphCodes.Add('llladeva', 2356);
  FGlyphCodes.Add('llvocalicbengali', 2529);
  FGlyphCodes.Add('llvocalicdeva', 2401);
  FGlyphCodes.Add('llvocalicvowelsignbengali', 2531);
  FGlyphCodes.Add('llvocalicvowelsigndeva', 2403);
  FGlyphCodes.Add('lmiddletilde', 619);
  FGlyphCodes.Add('lmonospace', 65356);
  FGlyphCodes.Add('lmsquare', 13264);
  FGlyphCodes.Add('lochulathai', 3628);
  FGlyphCodes.Add('logicaland', 8743);
  FGlyphCodes.Add('logicalnot', 172);
  FGlyphCodes.Add('logicalnotreversed', 8976);
  FGlyphCodes.Add('logicalor', 8744);
  FGlyphCodes.Add('lolingthai', 3621);
  FGlyphCodes.Add('longs', 383);
  FGlyphCodes.Add('lowlinecenterline', 65102);
  FGlyphCodes.Add('lowlinecmb', 818);
  FGlyphCodes.Add('lowlinedashed', 65101);
  FGlyphCodes.Add('lozenge', 9674);
  FGlyphCodes.Add('lparen', 9383);
  FGlyphCodes.Add('lslash', 322);
  FGlyphCodes.Add('lsquare', 8467);
  FGlyphCodes.Add('lsuperior', 63214);
  FGlyphCodes.Add('ltshade', 9617);
  FGlyphCodes.Add('luthai', 3622);
  FGlyphCodes.Add('lvocalicbengali', 2444);
  FGlyphCodes.Add('lvocalicdeva', 2316);
  FGlyphCodes.Add('lvocalicvowelsignbengali', 2530);
  FGlyphCodes.Add('lvocalicvowelsigndeva', 2402);
  FGlyphCodes.Add('lxsquare', 13267);
  FGlyphCodes.Add('m', 109);
  FGlyphCodes.Add('mabengali', 2478);
  FGlyphCodes.Add('macron', 175);
  FGlyphCodes.Add('macronbelowcmb', 817);
  FGlyphCodes.Add('macroncmb', 772);
  FGlyphCodes.Add('macronlowmod', 717);
  FGlyphCodes.Add('macronmonospace', 65507);
  FGlyphCodes.Add('macute', 7743);
  FGlyphCodes.Add('madeva', 2350);
  FGlyphCodes.Add('magujarati', 2734);
  FGlyphCodes.Add('magurmukhi', 2606);
  FGlyphCodes.Add('mahapakhhebrew', 1444);
  FGlyphCodes.Add('mahapakhlefthebrew', 1444);
  FGlyphCodes.Add('mahiragana', 12414);
  FGlyphCodes.Add('maichattawalowleftthai', 63637);
  FGlyphCodes.Add('maichattawalowrightthai', 63636);
  FGlyphCodes.Add('maichattawathai', 3659);
  FGlyphCodes.Add('maichattawaupperleftthai', 63635);
  FGlyphCodes.Add('maieklowleftthai', 63628);
  FGlyphCodes.Add('maieklowrightthai', 63627);
  FGlyphCodes.Add('maiekthai', 3656);
  FGlyphCodes.Add('maiekupperleftthai', 63626);
  FGlyphCodes.Add('maihanakatleftthai', 63620);
  FGlyphCodes.Add('maihanakatthai', 3633);
  FGlyphCodes.Add('maitaikhuleftthai', 63625);
  FGlyphCodes.Add('maitaikhuthai', 3655);
  FGlyphCodes.Add('maitholowleftthai', 63631);
  FGlyphCodes.Add('maitholowrightthai', 63630);
  FGlyphCodes.Add('maithothai', 3657);
  FGlyphCodes.Add('maithoupperleftthai', 63629);
  FGlyphCodes.Add('maitrilowleftthai', 63634);
  FGlyphCodes.Add('maitrilowrightthai', 63633);
  FGlyphCodes.Add('maitrithai', 3658);
  FGlyphCodes.Add('maitriupperleftthai', 63632);
  FGlyphCodes.Add('maiyamokthai', 3654);
  FGlyphCodes.Add('makatakana', 12510);
  FGlyphCodes.Add('makatakanahalfwidth', 65423);
  FGlyphCodes.Add('male', 9794);
  FGlyphCodes.Add('mansyonsquare', 13127);
  FGlyphCodes.Add('maqafhebrew', 1470);
  FGlyphCodes.Add('mars', 9794);
  FGlyphCodes.Add('masoracirclehebrew', 1455);
  FGlyphCodes.Add('masquare', 13187);
  FGlyphCodes.Add('mbopomofo', 12551);
  FGlyphCodes.Add('mbsquare', 13268);
  FGlyphCodes.Add('mcircle', 9436);
  FGlyphCodes.Add('mcubedsquare', 13221);
  FGlyphCodes.Add('mdotaccent', 7745);
  FGlyphCodes.Add('mdotbelow', 7747);
  FGlyphCodes.Add('meemarabic', 1605);
  FGlyphCodes.Add('meemfinalarabic', 65250);
  FGlyphCodes.Add('meeminitialarabic', 65251);
  FGlyphCodes.Add('meemmedialarabic', 65252);
  FGlyphCodes.Add('meemmeeminitialarabic', 64721);
  FGlyphCodes.Add('meemmeemisolatedarabic', 64584);
  FGlyphCodes.Add('meetorusquare', 13133);
  FGlyphCodes.Add('mehiragana', 12417);
  FGlyphCodes.Add('meizierasquare', 13182);
  FGlyphCodes.Add('mekatakana', 12513);
  FGlyphCodes.Add('mekatakanahalfwidth', 65426);
  FGlyphCodes.Add('mem', 1502);
  FGlyphCodes.Add('memdagesh', 64318);
  FGlyphCodes.Add('memdageshhebrew', 64318);
  FGlyphCodes.Add('memhebrew', 1502);
  FGlyphCodes.Add('menarmenian', 1396);
  FGlyphCodes.Add('merkhahebrew', 1445);
  FGlyphCodes.Add('merkhakefulahebrew', 1446);
  FGlyphCodes.Add('merkhakefulalefthebrew', 1446);
  FGlyphCodes.Add('merkhalefthebrew', 1445);
  FGlyphCodes.Add('mhook', 625);
  FGlyphCodes.Add('mhzsquare', 13202);
  FGlyphCodes.Add('middledotkatakanahalfwidth', 65381);
  FGlyphCodes.Add('middot', 183);
  FGlyphCodes.Add('mieumacirclekorean', 12914);
  FGlyphCodes.Add('mieumaparenkorean', 12818);
  FGlyphCodes.Add('mieumcirclekorean', 12900);
  FGlyphCodes.Add('mieumkorean', 12609);
  FGlyphCodes.Add('mieumpansioskorean', 12656);
  FGlyphCodes.Add('mieumparenkorean', 12804);
  FGlyphCodes.Add('mieumpieupkorean', 12654);
  FGlyphCodes.Add('mieumsioskorean', 12655);
  FGlyphCodes.Add('mihiragana', 12415);
  FGlyphCodes.Add('mikatakana', 12511);
  FGlyphCodes.Add('mikatakanahalfwidth', 65424);
  FGlyphCodes.Add('minus', 8722);
  FGlyphCodes.Add('minusbelowcmb', 800);
  FGlyphCodes.Add('minuscircle', 8854);
  FGlyphCodes.Add('minusmod', 727);
  FGlyphCodes.Add('minusplus', 8723);
  FGlyphCodes.Add('minute', 8242);
  FGlyphCodes.Add('miribaarusquare', 13130);
  FGlyphCodes.Add('mirisquare', 13129);
  FGlyphCodes.Add('mlonglegturned', 624);
  FGlyphCodes.Add('mlsquare', 13206);
  FGlyphCodes.Add('mmcubedsquare', 13219);
  FGlyphCodes.Add('mmonospace', 65357);
  FGlyphCodes.Add('mmsquaredsquare', 13215);
  FGlyphCodes.Add('mohiragana', 12418);
  FGlyphCodes.Add('mohmsquare', 13249);
  FGlyphCodes.Add('mokatakana', 12514);
  FGlyphCodes.Add('mokatakanahalfwidth', 65427);
  FGlyphCodes.Add('molsquare', 13270);
  FGlyphCodes.Add('momathai', 3617);
  FGlyphCodes.Add('moverssquare', 13223);
  FGlyphCodes.Add('moverssquaredsquare', 13224);
  FGlyphCodes.Add('mparen', 9384);
  FGlyphCodes.Add('mpasquare', 13227);
  FGlyphCodes.Add('mssquare', 13235);
  FGlyphCodes.Add('msuperior', 63215);
  FGlyphCodes.Add('mturned', 623);
  FGlyphCodes.Add('mu', 181);
  FGlyphCodes.Add('mu1', 181);
  FGlyphCodes.Add('muasquare', 13186);
  FGlyphCodes.Add('muchgreater', 8811);
  FGlyphCodes.Add('muchless', 8810);
  FGlyphCodes.Add('mufsquare', 13196);
  FGlyphCodes.Add('mugreek', 956);
  FGlyphCodes.Add('mugsquare', 13197);
  FGlyphCodes.Add('muhiragana', 12416);
  FGlyphCodes.Add('mukatakana', 12512);
  FGlyphCodes.Add('mukatakanahalfwidth', 65425);
  FGlyphCodes.Add('mulsquare', 13205);
  FGlyphCodes.Add('multiply', 215);
  FGlyphCodes.Add('mumsquare', 13211);
  FGlyphCodes.Add('munahhebrew', 1443);
  FGlyphCodes.Add('munahlefthebrew', 1443);
  FGlyphCodes.Add('musicalnote', 9834);
  FGlyphCodes.Add('musicalnotedbl', 9835);
  FGlyphCodes.Add('musicflatsign', 9837);
  FGlyphCodes.Add('musicsharpsign', 9839);
  FGlyphCodes.Add('mussquare', 13234);
  FGlyphCodes.Add('muvsquare', 13238);
  FGlyphCodes.Add('muwsquare', 13244);
  FGlyphCodes.Add('mvmegasquare', 13241);
  FGlyphCodes.Add('mvsquare', 13239);
  FGlyphCodes.Add('mwmegasquare', 13247);
  FGlyphCodes.Add('mwsquare', 13245);
  FGlyphCodes.Add('n', 110);
  FGlyphCodes.Add('nabengali', 2472);
  FGlyphCodes.Add('nabla', 8711);
  FGlyphCodes.Add('nacute', 324);
  FGlyphCodes.Add('nadeva', 2344);
  FGlyphCodes.Add('nagujarati', 2728);
  FGlyphCodes.Add('nagurmukhi', 2600);
  FGlyphCodes.Add('nahiragana', 12394);
  FGlyphCodes.Add('nakatakana', 12490);
  FGlyphCodes.Add('nakatakanahalfwidth', 65413);
  FGlyphCodes.Add('napostrophe', 329);
  FGlyphCodes.Add('nasquare', 13185);
  FGlyphCodes.Add('nbopomofo', 12555);
  FGlyphCodes.Add('nbspace', 160);
  FGlyphCodes.Add('ncaron', 328);
  FGlyphCodes.Add('ncedilla', 326);
  FGlyphCodes.Add('ncircle', 9437);
  FGlyphCodes.Add('ncircumflexbelow', 7755);
  FGlyphCodes.Add('ncommaaccent', 326);
  FGlyphCodes.Add('ndotaccent', 7749);
  FGlyphCodes.Add('ndotbelow', 7751);
  FGlyphCodes.Add('nehiragana', 12397);
  FGlyphCodes.Add('nekatakana', 12493);
  FGlyphCodes.Add('nekatakanahalfwidth', 65416);
  FGlyphCodes.Add('newsheqelsign', 8362);
  FGlyphCodes.Add('nfsquare', 13195);
  FGlyphCodes.Add('ngabengali', 2457);
  FGlyphCodes.Add('ngadeva', 2329);
  FGlyphCodes.Add('ngagujarati', 2713);
  FGlyphCodes.Add('ngagurmukhi', 2585);
  FGlyphCodes.Add('ngonguthai', 3591);
  FGlyphCodes.Add('nhiragana', 12435);
  FGlyphCodes.Add('nhookleft', 626);
  FGlyphCodes.Add('nhookretroflex', 627);
  FGlyphCodes.Add('nieunacirclekorean', 12911);
  FGlyphCodes.Add('nieunaparenkorean', 12815);
  FGlyphCodes.Add('nieuncieuckorean', 12597);
  FGlyphCodes.Add('nieuncirclekorean', 12897);
  FGlyphCodes.Add('nieunhieuhkorean', 12598);
  FGlyphCodes.Add('nieunkorean', 12596);
  FGlyphCodes.Add('nieunpansioskorean', 12648);
  FGlyphCodes.Add('nieunparenkorean', 12801);
  FGlyphCodes.Add('nieunsioskorean', 12647);
  FGlyphCodes.Add('nieuntikeutkorean', 12646);
  FGlyphCodes.Add('nihiragana', 12395);
  FGlyphCodes.Add('nikatakana', 12491);
  FGlyphCodes.Add('nikatakanahalfwidth', 65414);
  FGlyphCodes.Add('nikhahitleftthai', 63641);
  FGlyphCodes.Add('nikhahitthai', 3661);
  FGlyphCodes.Add('nine', 57);
  FGlyphCodes.Add('ninearabic', 1641);
  FGlyphCodes.Add('ninebengali', 2543);
  FGlyphCodes.Add('ninecircle', 9320);
  FGlyphCodes.Add('ninecircleinversesansserif', 10130);
  FGlyphCodes.Add('ninedeva', 2415);
  FGlyphCodes.Add('ninegujarati', 2799);
  FGlyphCodes.Add('ninegurmukhi', 2671);
  FGlyphCodes.Add('ninehackarabic', 1641);
  FGlyphCodes.Add('ninehangzhou', 12329);
  FGlyphCodes.Add('nineideographicparen', 12840);
  FGlyphCodes.Add('nineinferior', 8329);
  FGlyphCodes.Add('ninemonospace', 65305);
  FGlyphCodes.Add('nineoldstyle', 63289);
  FGlyphCodes.Add('nineparen', 9340);
  FGlyphCodes.Add('nineperiod', 9360);
  FGlyphCodes.Add('ninepersian', 1785);
  FGlyphCodes.Add('nineroman', 8568);
  FGlyphCodes.Add('ninesuperior', 8313);
  FGlyphCodes.Add('nineteencircle', 9330);
  FGlyphCodes.Add('nineteenparen', 9350);
  FGlyphCodes.Add('nineteenperiod', 9370);
  FGlyphCodes.Add('ninethai', 3673);
  FGlyphCodes.Add('nj', 460);
  FGlyphCodes.Add('njecyrillic', 1114);
  FGlyphCodes.Add('nkatakana', 12531);
  FGlyphCodes.Add('nkatakanahalfwidth', 65437);
  FGlyphCodes.Add('nlegrightlong', 414);
  FGlyphCodes.Add('nlinebelow', 7753);
  FGlyphCodes.Add('nmonospace', 65358);
  FGlyphCodes.Add('nmsquare', 13210);
  FGlyphCodes.Add('nnabengali', 2467);
  FGlyphCodes.Add('nnadeva', 2339);
  FGlyphCodes.Add('nnagujarati', 2723);
  FGlyphCodes.Add('nnagurmukhi', 2595);
  FGlyphCodes.Add('nnnadeva', 2345);
  FGlyphCodes.Add('nohiragana', 12398);
  FGlyphCodes.Add('nokatakana', 12494);
  FGlyphCodes.Add('nokatakanahalfwidth', 65417);
  FGlyphCodes.Add('nonbreakingspace', 160);
  FGlyphCodes.Add('nonenthai', 3603);
  FGlyphCodes.Add('nonuthai', 3609);
  FGlyphCodes.Add('noonarabic', 1606);
  FGlyphCodes.Add('noonfinalarabic', 65254);
  FGlyphCodes.Add('noonghunnaarabic', 1722);
  FGlyphCodes.Add('noonghunnafinalarabic', 64415);
  FGlyphCodes.Add('nooninitialarabic', 65255);
  FGlyphCodes.Add('noonjeeminitialarabic', 64722);
  FGlyphCodes.Add('noonjeemisolatedarabic', 64587);
  FGlyphCodes.Add('noonmedialarabic', 65256);
  FGlyphCodes.Add('noonmeeminitialarabic', 64725);
  FGlyphCodes.Add('noonmeemisolatedarabic', 64590);
  FGlyphCodes.Add('noonnoonfinalarabic', 64653);
  FGlyphCodes.Add('notcontains', 8716);
  FGlyphCodes.Add('notelement', 8713);
  FGlyphCodes.Add('notelementof', 8713);
  FGlyphCodes.Add('notequal', 8800);
  FGlyphCodes.Add('notgreater', 8815);
  FGlyphCodes.Add('notgreaternorequal', 8817);
  FGlyphCodes.Add('notgreaternorless', 8825);
  FGlyphCodes.Add('notidentical', 8802);
  FGlyphCodes.Add('notless', 8814);
  FGlyphCodes.Add('notlessnorequal', 8816);
  FGlyphCodes.Add('notparallel', 8742);
  FGlyphCodes.Add('notprecedes', 8832);
  FGlyphCodes.Add('notsubset', 8836);
  FGlyphCodes.Add('notsucceeds', 8833);
  FGlyphCodes.Add('notsuperset', 8837);
  FGlyphCodes.Add('nowarmenian', 1398);
  FGlyphCodes.Add('nparen', 9385);
  FGlyphCodes.Add('nssquare', 13233);
  FGlyphCodes.Add('nsuperior', 8319);
  FGlyphCodes.Add('ntilde', 241);
  FGlyphCodes.Add('nu', 957);
  FGlyphCodes.Add('nuhiragana', 12396);
  FGlyphCodes.Add('nukatakana', 12492);
  FGlyphCodes.Add('nukatakanahalfwidth', 65415);
  FGlyphCodes.Add('nuktabengali', 2492);
  FGlyphCodes.Add('nuktadeva', 2364);
  FGlyphCodes.Add('nuktagujarati', 2748);
  FGlyphCodes.Add('nuktagurmukhi', 2620);
  FGlyphCodes.Add('numbersign', 35);
  FGlyphCodes.Add('numbersignmonospace', 65283);
  FGlyphCodes.Add('numbersignsmall', 65119);
  FGlyphCodes.Add('numeralsigngreek', 884);
  FGlyphCodes.Add('numeralsignlowergreek', 885);
  FGlyphCodes.Add('numero', 8470);
  FGlyphCodes.Add('nun', 1504);
  FGlyphCodes.Add('nundagesh', 64320);
  FGlyphCodes.Add('nundageshhebrew', 64320);
  FGlyphCodes.Add('nunhebrew', 1504);
  FGlyphCodes.Add('nvsquare', 13237);
  FGlyphCodes.Add('nwsquare', 13243);
  FGlyphCodes.Add('nyabengali', 2462);
  FGlyphCodes.Add('nyadeva', 2334);
  FGlyphCodes.Add('nyagujarati', 2718);
  FGlyphCodes.Add('nyagurmukhi', 2590);
  FGlyphCodes.Add('o', 111);
  FGlyphCodes.Add('oacute', 243);
  FGlyphCodes.Add('oangthai', 3629);
  FGlyphCodes.Add('obarred', 629);
  FGlyphCodes.Add('obarredcyrillic', 1257);
  FGlyphCodes.Add('obarreddieresiscyrillic', 1259);
  FGlyphCodes.Add('obengali', 2451);
  FGlyphCodes.Add('obopomofo', 12571);
  FGlyphCodes.Add('obreve', 335);
  FGlyphCodes.Add('ocandradeva', 2321);
  FGlyphCodes.Add('ocandragujarati', 2705);
  FGlyphCodes.Add('ocandravowelsigndeva', 2377);
  FGlyphCodes.Add('ocandravowelsigngujarati', 2761);
  FGlyphCodes.Add('ocaron', 466);
  FGlyphCodes.Add('ocircle', 9438);
  FGlyphCodes.Add('ocircumflex', 244);
  FGlyphCodes.Add('ocircumflexacute', 7889);
  FGlyphCodes.Add('ocircumflexdotbelow', 7897);
  FGlyphCodes.Add('ocircumflexgrave', 7891);
  FGlyphCodes.Add('ocircumflexhookabove', 7893);
  FGlyphCodes.Add('ocircumflextilde', 7895);
  FGlyphCodes.Add('ocyrillic', 1086);
  FGlyphCodes.Add('odblacute', 337);
  FGlyphCodes.Add('odblgrave', 525);
  FGlyphCodes.Add('odeva', 2323);
  FGlyphCodes.Add('odieresis', 246);
  FGlyphCodes.Add('odieresiscyrillic', 1255);
  FGlyphCodes.Add('odotbelow', 7885);
  FGlyphCodes.Add('oe', 339);
  FGlyphCodes.Add('oekorean', 12634);
  FGlyphCodes.Add('ogonek', 731);
  FGlyphCodes.Add('ogonekcmb', 808);
  FGlyphCodes.Add('ograve', 242);
  FGlyphCodes.Add('ogujarati', 2707);
  FGlyphCodes.Add('oharmenian', 1413);
  FGlyphCodes.Add('ohiragana', 12362);
  FGlyphCodes.Add('ohookabove', 7887);
  FGlyphCodes.Add('ohorn', 417);
  FGlyphCodes.Add('ohornacute', 7899);
  FGlyphCodes.Add('ohorndotbelow', 7907);
  FGlyphCodes.Add('ohorngrave', 7901);
  FGlyphCodes.Add('ohornhookabove', 7903);
  FGlyphCodes.Add('ohorntilde', 7905);
  FGlyphCodes.Add('ohungarumlaut', 337);
  FGlyphCodes.Add('oi', 419);
  FGlyphCodes.Add('oinvertedbreve', 527);
  FGlyphCodes.Add('okatakana', 12458);
  FGlyphCodes.Add('okatakanahalfwidth', 65397);
  FGlyphCodes.Add('okorean', 12631);
  FGlyphCodes.Add('olehebrew', 1451);
  FGlyphCodes.Add('omacron', 333);
  FGlyphCodes.Add('omacronacute', 7763);
  FGlyphCodes.Add('omacrongrave', 7761);
  FGlyphCodes.Add('omdeva', 2384);
  FGlyphCodes.Add('omega', 969);
  FGlyphCodes.Add('omega1', 982);
  FGlyphCodes.Add('omegacyrillic', 1121);
  FGlyphCodes.Add('omegalatinclosed', 631);
  FGlyphCodes.Add('omegaroundcyrillic', 1147);
  FGlyphCodes.Add('omegatitlocyrillic', 1149);
  FGlyphCodes.Add('omegatonos', 974);
  FGlyphCodes.Add('omgujarati', 2768);
  FGlyphCodes.Add('omicron', 959);
  FGlyphCodes.Add('omicrontonos', 972);
  FGlyphCodes.Add('omonospace', 65359);
  FGlyphCodes.Add('one', 49);
  FGlyphCodes.Add('onearabic', 1633);
  FGlyphCodes.Add('onebengali', 2535);
  FGlyphCodes.Add('onecircle', 9312);
  FGlyphCodes.Add('onecircleinversesansserif', 10122);
  FGlyphCodes.Add('onedeva', 2407);
  FGlyphCodes.Add('onedotenleader', 8228);
  FGlyphCodes.Add('oneeighth', 8539);
  FGlyphCodes.Add('onefitted', 63196);
  FGlyphCodes.Add('onegujarati', 2791);
  FGlyphCodes.Add('onegurmukhi', 2663);
  FGlyphCodes.Add('onehackarabic', 1633);
  FGlyphCodes.Add('onehalf', 189);
  FGlyphCodes.Add('onehangzhou', 12321);
  FGlyphCodes.Add('oneideographicparen', 12832);
  FGlyphCodes.Add('oneinferior', 8321);
  FGlyphCodes.Add('onemonospace', 65297);
  FGlyphCodes.Add('onenumeratorbengali', 2548);
  FGlyphCodes.Add('oneoldstyle', 63281);
  FGlyphCodes.Add('oneparen', 9332);
  FGlyphCodes.Add('oneperiod', 9352);
  FGlyphCodes.Add('onepersian', 1777);
  FGlyphCodes.Add('onequarter', 188);
  FGlyphCodes.Add('oneroman', 8560);
  FGlyphCodes.Add('onesuperior', 185);
  FGlyphCodes.Add('onethai', 3665);
  FGlyphCodes.Add('onethird', 8531);
  FGlyphCodes.Add('oogonek', 491);
  FGlyphCodes.Add('oogonekmacron', 493);
  FGlyphCodes.Add('oogurmukhi', 2579);
  FGlyphCodes.Add('oomatragurmukhi', 2635);
  FGlyphCodes.Add('oopen', 596);
  FGlyphCodes.Add('oparen', 9386);
  FGlyphCodes.Add('openbullet', 9702);
  FGlyphCodes.Add('option', 8997);
  FGlyphCodes.Add('ordfeminine', 170);
  FGlyphCodes.Add('ordmasculine', 186);
  FGlyphCodes.Add('orthogonal', 8735);
  FGlyphCodes.Add('oshortdeva', 2322);
  FGlyphCodes.Add('oshortvowelsigndeva', 2378);
  FGlyphCodes.Add('oslash', 248);
  FGlyphCodes.Add('oslashacute', 511);
  FGlyphCodes.Add('osmallhiragana', 12361);
  FGlyphCodes.Add('osmallkatakana', 12457);
  FGlyphCodes.Add('osmallkatakanahalfwidth', 65387);
  FGlyphCodes.Add('ostrokeacute', 511);
  FGlyphCodes.Add('osuperior', 63216);
  FGlyphCodes.Add('otcyrillic', 1151);
  FGlyphCodes.Add('otilde', 245);
  FGlyphCodes.Add('otildeacute', 7757);
  FGlyphCodes.Add('otildedieresis', 7759);
  FGlyphCodes.Add('oubopomofo', 12577);
  FGlyphCodes.Add('overline', 8254);
  FGlyphCodes.Add('overlinecenterline', 65098);
  FGlyphCodes.Add('overlinecmb', 773);
  FGlyphCodes.Add('overlinedashed', 65097);
  FGlyphCodes.Add('overlinedblwavy', 65100);
  FGlyphCodes.Add('overlinewavy', 65099);
  FGlyphCodes.Add('overscore', 175);
  FGlyphCodes.Add('ovowelsignbengali', 2507);
  FGlyphCodes.Add('ovowelsigndeva', 2379);
  FGlyphCodes.Add('ovowelsigngujarati', 2763);
  FGlyphCodes.Add('p', 112);
  FGlyphCodes.Add('paampssquare', 13184);
  FGlyphCodes.Add('paasentosquare', 13099);
  FGlyphCodes.Add('pabengali', 2474);
  FGlyphCodes.Add('pacute', 7765);
  FGlyphCodes.Add('padeva', 2346);
  FGlyphCodes.Add('pagedown', 8671);
  FGlyphCodes.Add('pageup', 8670);
  FGlyphCodes.Add('pagujarati', 2730);
  FGlyphCodes.Add('pagurmukhi', 2602);
  FGlyphCodes.Add('pahiragana', 12401);
  FGlyphCodes.Add('paiyannoithai', 3631);
  FGlyphCodes.Add('pakatakana', 12497);
  FGlyphCodes.Add('palatalizationcyrilliccmb', 1156);
  FGlyphCodes.Add('palochkacyrillic', 1216);
  FGlyphCodes.Add('pansioskorean', 12671);
  FGlyphCodes.Add('paragraph', 182);
  FGlyphCodes.Add('parallel', 8741);
  FGlyphCodes.Add('parenleft', 40);
  FGlyphCodes.Add('parenleftaltonearabic', 64830);
  FGlyphCodes.Add('parenleftbt', 63725);
  FGlyphCodes.Add('parenleftex', 63724);
  FGlyphCodes.Add('parenleftinferior', 8333);
  FGlyphCodes.Add('parenleftmonospace', 65288);
  FGlyphCodes.Add('parenleftsmall', 65113);
  FGlyphCodes.Add('parenleftsuperior', 8317);
  FGlyphCodes.Add('parenlefttp', 63723);
  FGlyphCodes.Add('parenleftvertical', 65077);
  FGlyphCodes.Add('parenright', 41);
  FGlyphCodes.Add('parenrightaltonearabic', 64831);
  FGlyphCodes.Add('parenrightbt', 63736);
  FGlyphCodes.Add('parenrightex', 63735);
  FGlyphCodes.Add('parenrightinferior', 8334);
  FGlyphCodes.Add('parenrightmonospace', 65289);
  FGlyphCodes.Add('parenrightsmall', 65114);
  FGlyphCodes.Add('parenrightsuperior', 8318);
  FGlyphCodes.Add('parenrighttp', 63734);
  FGlyphCodes.Add('parenrightvertical', 65078);
  FGlyphCodes.Add('partialdiff', 8706);
  FGlyphCodes.Add('paseqhebrew', 1472);
  FGlyphCodes.Add('pashtahebrew', 1433);
  FGlyphCodes.Add('pasquare', 13225);
  FGlyphCodes.Add('patah', 1463);
  FGlyphCodes.Add('patah11', 1463);
  FGlyphCodes.Add('patah1d', 1463);
  FGlyphCodes.Add('patah2a', 1463);
  FGlyphCodes.Add('patahhebrew', 1463);
  FGlyphCodes.Add('patahnarrowhebrew', 1463);
  FGlyphCodes.Add('patahquarterhebrew', 1463);
  FGlyphCodes.Add('patahwidehebrew', 1463);
  FGlyphCodes.Add('pazerhebrew', 1441);
  FGlyphCodes.Add('pbopomofo', 12550);
  FGlyphCodes.Add('pcircle', 9439);
  FGlyphCodes.Add('pdotaccent', 7767);
  FGlyphCodes.Add('pe', 1508);
  FGlyphCodes.Add('pecyrillic', 1087);
  FGlyphCodes.Add('pedagesh', 64324);
  FGlyphCodes.Add('pedageshhebrew', 64324);
  FGlyphCodes.Add('peezisquare', 13115);
  FGlyphCodes.Add('pefinaldageshhebrew', 64323);
  FGlyphCodes.Add('peharabic', 1662);
  FGlyphCodes.Add('peharmenian', 1402);
  FGlyphCodes.Add('pehebrew', 1508);
  FGlyphCodes.Add('pehfinalarabic', 64343);
  FGlyphCodes.Add('pehinitialarabic', 64344);
  FGlyphCodes.Add('pehiragana', 12410);
  FGlyphCodes.Add('pehmedialarabic', 64345);
  FGlyphCodes.Add('pekatakana', 12506);
  FGlyphCodes.Add('pemiddlehookcyrillic', 1191);
  FGlyphCodes.Add('perafehebrew', 64334);
  FGlyphCodes.Add('percent', 37);
  FGlyphCodes.Add('percentarabic', 1642);
  FGlyphCodes.Add('percentmonospace', 65285);
  FGlyphCodes.Add('percentsmall', 65130);
  FGlyphCodes.Add('period', 46);
  FGlyphCodes.Add('periodarmenian', 1417);
  FGlyphCodes.Add('periodcentered', 183);
  FGlyphCodes.Add('periodhalfwidth', 65377);
  FGlyphCodes.Add('periodinferior', 63207);
  FGlyphCodes.Add('periodmonospace', 65294);
  FGlyphCodes.Add('periodsmall', 65106);
  FGlyphCodes.Add('periodsuperior', 63208);
  FGlyphCodes.Add('perispomenigreekcmb', 834);
  FGlyphCodes.Add('perpendicular', 8869);
  FGlyphCodes.Add('perthousand', 8240);
  FGlyphCodes.Add('peseta', 8359);
  FGlyphCodes.Add('pfsquare', 13194);
  FGlyphCodes.Add('phabengali', 2475);
  FGlyphCodes.Add('phadeva', 2347);
  FGlyphCodes.Add('phagujarati', 2731);
  FGlyphCodes.Add('phagurmukhi', 2603);
  FGlyphCodes.Add('phi', 966);
  FGlyphCodes.Add('phi1', 981);
  FGlyphCodes.Add('phieuphacirclekorean', 12922);
  FGlyphCodes.Add('phieuphaparenkorean', 12826);
  FGlyphCodes.Add('phieuphcirclekorean', 12908);
  FGlyphCodes.Add('phieuphkorean', 12621);
  FGlyphCodes.Add('phieuphparenkorean', 12812);
  FGlyphCodes.Add('philatin', 632);
  FGlyphCodes.Add('phinthuthai', 3642);
  FGlyphCodes.Add('phisymbolgreek', 981);
  FGlyphCodes.Add('phook', 421);
  FGlyphCodes.Add('phophanthai', 3614);
  FGlyphCodes.Add('phophungthai', 3612);
  FGlyphCodes.Add('phosamphaothai', 3616);
  FGlyphCodes.Add('pi', 960);
  FGlyphCodes.Add('pieupacirclekorean', 12915);
  FGlyphCodes.Add('pieupaparenkorean', 12819);
  FGlyphCodes.Add('pieupcieuckorean', 12662);
  FGlyphCodes.Add('pieupcirclekorean', 12901);
  FGlyphCodes.Add('pieupkiyeokkorean', 12658);
  FGlyphCodes.Add('pieupkorean', 12610);
  FGlyphCodes.Add('pieupparenkorean', 12805);
  FGlyphCodes.Add('pieupsioskiyeokkorean', 12660);
  FGlyphCodes.Add('pieupsioskorean', 12612);
  FGlyphCodes.Add('pieupsiostikeutkorean', 12661);
  FGlyphCodes.Add('pieupthieuthkorean', 12663);
  FGlyphCodes.Add('pieuptikeutkorean', 12659);
  FGlyphCodes.Add('pihiragana', 12404);
  FGlyphCodes.Add('pikatakana', 12500);
  FGlyphCodes.Add('pisymbolgreek', 982);
  FGlyphCodes.Add('piwrarmenian', 1411);
  FGlyphCodes.Add('Piwrarmenian', 1363);
  FGlyphCodes.Add('plus', 43);
  FGlyphCodes.Add('plusbelowcmb', 799);
  FGlyphCodes.Add('pluscircle', 8853);
  FGlyphCodes.Add('plusminus', 177);
  FGlyphCodes.Add('plusmod', 726);
  FGlyphCodes.Add('plusmonospace', 65291);
  FGlyphCodes.Add('plussmall', 65122);
  FGlyphCodes.Add('plussuperior', 8314);
  FGlyphCodes.Add('pmonospace', 65360);
  FGlyphCodes.Add('pmsquare', 13272);
  FGlyphCodes.Add('pohiragana', 12413);
  FGlyphCodes.Add('pointingindexdownwhite', 9759);
  FGlyphCodes.Add('pointingindexleftwhite', 9756);
  FGlyphCodes.Add('pointingindexrightwhite', 9758);
  FGlyphCodes.Add('pointingindexupwhite', 9757);
  FGlyphCodes.Add('pokatakana', 12509);
end;

procedure TdxFontFileUnicodeConverter.InitializePack4;
begin
  FGlyphCodes.Add('poplathai', 3611);
  FGlyphCodes.Add('postalmark', 12306);
  FGlyphCodes.Add('postalmarkface', 12320);
  FGlyphCodes.Add('pparen', 9387);
  FGlyphCodes.Add('precedes', 8826);
  FGlyphCodes.Add('prescription', 8478);
  FGlyphCodes.Add('primemod', 697);
  FGlyphCodes.Add('primereversed', 8245);
  FGlyphCodes.Add('product', 8719);
  FGlyphCodes.Add('projective', 8965);
  FGlyphCodes.Add('prolongedkana', 12540);
  FGlyphCodes.Add('propellor', 8984);
  FGlyphCodes.Add('propersubset', 8834);
  FGlyphCodes.Add('propersuperset', 8835);
  FGlyphCodes.Add('proportion', 8759);
  FGlyphCodes.Add('proportional', 8733);
  FGlyphCodes.Add('psi', 968);
  FGlyphCodes.Add('psicyrillic', 1137);
  FGlyphCodes.Add('psilipneumatacyrilliccmb', 1158);
  FGlyphCodes.Add('pssquare', 13232);
  FGlyphCodes.Add('puhiragana', 12407);
  FGlyphCodes.Add('pukatakana', 12503);
  FGlyphCodes.Add('pvsquare', 13236);
  FGlyphCodes.Add('pwsquare', 13242);
  FGlyphCodes.Add('q', 113);
  FGlyphCodes.Add('qadeva', 2392);
  FGlyphCodes.Add('qadmahebrew', 1448);
  FGlyphCodes.Add('qafarabic', 1602);
  FGlyphCodes.Add('qaffinalarabic', 65238);
  FGlyphCodes.Add('qafinitialarabic', 65239);
  FGlyphCodes.Add('qafmedialarabic', 65240);
  FGlyphCodes.Add('qamats', 1464);
  FGlyphCodes.Add('qamats10', 1464);
  FGlyphCodes.Add('qamats1a', 1464);
  FGlyphCodes.Add('qamats1c', 1464);
  FGlyphCodes.Add('qamats27', 1464);
  FGlyphCodes.Add('qamats29', 1464);
  FGlyphCodes.Add('qamats33', 1464);
  FGlyphCodes.Add('qamatsde', 1464);
  FGlyphCodes.Add('qamatshebrew', 1464);
  FGlyphCodes.Add('qamatsnarrowhebrew', 1464);
  FGlyphCodes.Add('qamatsqatanhebrew', 1464);
  FGlyphCodes.Add('qamatsqatannarrowhebrew', 1464);
  FGlyphCodes.Add('qamatsqatanquarterhebrew', 1464);
  FGlyphCodes.Add('qamatsqatanwidehebrew', 1464);
  FGlyphCodes.Add('qamatsquarterhebrew', 1464);
  FGlyphCodes.Add('qamatswidehebrew', 1464);
  FGlyphCodes.Add('qarneyparahebrew', 1439);
  FGlyphCodes.Add('qbopomofo', 12561);
  FGlyphCodes.Add('qcircle', 9440);
  FGlyphCodes.Add('qhook', 672);
  FGlyphCodes.Add('qmonospace', 65361);
  FGlyphCodes.Add('qof', 1511);
  FGlyphCodes.Add('qofdagesh', 64327);
  FGlyphCodes.Add('qofdageshhebrew', 64327);
  FGlyphCodes.Add('qofhebrew', 1511);
  FGlyphCodes.Add('qparen', 9388);
  FGlyphCodes.Add('quarternote', 9833);
  FGlyphCodes.Add('qubuts', 1467);
  FGlyphCodes.Add('qubuts18', 1467);
  FGlyphCodes.Add('qubuts25', 1467);
  FGlyphCodes.Add('qubuts31', 1467);
  FGlyphCodes.Add('qubutshebrew', 1467);
  FGlyphCodes.Add('qubutsnarrowhebrew', 1467);
  FGlyphCodes.Add('qubutsquarterhebrew', 1467);
  FGlyphCodes.Add('qubutswidehebrew', 1467);
  FGlyphCodes.Add('question', 63);
  FGlyphCodes.Add('questionarabic', 1567);
  FGlyphCodes.Add('questionarmenian', 1374);
  FGlyphCodes.Add('questiondown', 191);
  FGlyphCodes.Add('questiondownsmall', 63423);
  FGlyphCodes.Add('questiongreek', 894);
  FGlyphCodes.Add('questionmonospace', 65311);
  FGlyphCodes.Add('questionsmall', 63295);
  FGlyphCodes.Add('quotedbl', 34);
  FGlyphCodes.Add('quotedblbase', 8222);
  FGlyphCodes.Add('quotedblleft', 8220);
  FGlyphCodes.Add('quotedblmonospace', 65282);
  FGlyphCodes.Add('quotedblprime', 12318);
  FGlyphCodes.Add('quotedblprimereversed', 12317);
  FGlyphCodes.Add('quotedblright', 8221);
  FGlyphCodes.Add('quoteleft', 8216);
  FGlyphCodes.Add('quoteleftreversed', 8219);
  FGlyphCodes.Add('quotereversed', 8219);
  FGlyphCodes.Add('quoteright', 8217);
  FGlyphCodes.Add('quoterightn', 329);
  FGlyphCodes.Add('quotesinglbase', 8218);
  FGlyphCodes.Add('quotesingle', 39);
  FGlyphCodes.Add('quotesinglemonospace', 65287);
  FGlyphCodes.Add('r', 114);
  FGlyphCodes.Add('raarmenian', 1404);
  FGlyphCodes.Add('rabengali', 2480);
  FGlyphCodes.Add('racute', 341);
  FGlyphCodes.Add('radeva', 2352);
  FGlyphCodes.Add('radical', 8730);
  FGlyphCodes.Add('radicalex', 8254);
  FGlyphCodes.Add('radoverssquare', 13230);
  FGlyphCodes.Add('radoverssquaredsquare', 13231);
  FGlyphCodes.Add('radsquare', 13229);
  FGlyphCodes.Add('rafe', 1471);
  FGlyphCodes.Add('rafehebrew', 1471);
  FGlyphCodes.Add('ragujarati', 2736);
  FGlyphCodes.Add('ragurmukhi', 2608);
  FGlyphCodes.Add('rahiragana', 12425);
  FGlyphCodes.Add('rakatakana', 12521);
  FGlyphCodes.Add('rakatakanahalfwidth', 65431);
  FGlyphCodes.Add('ralowerdiagonalbengali', 2545);
  FGlyphCodes.Add('ramiddlediagonalbengali', 2544);
  FGlyphCodes.Add('ramshorn', 612);
  FGlyphCodes.Add('ratio', 8758);
  FGlyphCodes.Add('rbopomofo', 12566);
  FGlyphCodes.Add('rcaron', 345);
  FGlyphCodes.Add('rcedilla', 343);
  FGlyphCodes.Add('rcircle', 9441);
  FGlyphCodes.Add('rcommaaccent', 343);
  FGlyphCodes.Add('rdblgrave', 529);
  FGlyphCodes.Add('rdotaccent', 7769);
  FGlyphCodes.Add('rdotbelow', 7771);
  FGlyphCodes.Add('rdotbelowmacron', 7773);
  FGlyphCodes.Add('referencemark', 8251);
  FGlyphCodes.Add('reflexsubset', 8838);
  FGlyphCodes.Add('reflexsuperset', 8839);
  FGlyphCodes.Add('registered', 174);
  FGlyphCodes.Add('registersans', 63720);
  FGlyphCodes.Add('registerserif', 63194);
  FGlyphCodes.Add('reharabic', 1585);
  FGlyphCodes.Add('reharmenian', 1408);
  FGlyphCodes.Add('rehfinalarabic', 65198);
  FGlyphCodes.Add('rehiragana', 12428);
  FGlyphCodes.Add('rekatakana', 12524);
  FGlyphCodes.Add('rekatakanahalfwidth', 65434);
  FGlyphCodes.Add('resh', 1512);
  FGlyphCodes.Add('reshdageshhebrew', 64328);
  FGlyphCodes.Add('reshhebrew', 1512);
  FGlyphCodes.Add('reversedtilde', 8765);
  FGlyphCodes.Add('reviahebrew', 1431);
  FGlyphCodes.Add('reviamugrashhebrew', 1431);
  FGlyphCodes.Add('revlogicalnot', 8976);
  FGlyphCodes.Add('rfishhook', 638);
  FGlyphCodes.Add('rfishhookreversed', 639);
  FGlyphCodes.Add('rhabengali', 2525);
  FGlyphCodes.Add('rhadeva', 2397);
  FGlyphCodes.Add('rho', 961);
  FGlyphCodes.Add('rhook', 637);
  FGlyphCodes.Add('rhookturned', 635);
  FGlyphCodes.Add('rhookturnedsuperior', 693);
  FGlyphCodes.Add('rhosymbolgreek', 1009);
  FGlyphCodes.Add('rhotichookmod', 734);
  FGlyphCodes.Add('rieulacirclekorean', 12913);
  FGlyphCodes.Add('rieulaparenkorean', 12817);
  FGlyphCodes.Add('rieulcirclekorean', 12899);
  FGlyphCodes.Add('rieulhieuhkorean', 12608);
  FGlyphCodes.Add('rieulkiyeokkorean', 12602);
  FGlyphCodes.Add('rieulkiyeoksioskorean', 12649);
  FGlyphCodes.Add('rieulkorean', 12601);
  FGlyphCodes.Add('rieulmieumkorean', 12603);
  FGlyphCodes.Add('rieulpansioskorean', 12652);
  FGlyphCodes.Add('rieulparenkorean', 12803);
  FGlyphCodes.Add('rieulphieuphkorean', 12607);
  FGlyphCodes.Add('rieulpieupkorean', 12604);
  FGlyphCodes.Add('rieulpieupsioskorean', 12651);
  FGlyphCodes.Add('rieulsioskorean', 12605);
  FGlyphCodes.Add('rieulthieuthkorean', 12606);
  FGlyphCodes.Add('rieultikeutkorean', 12650);
  FGlyphCodes.Add('rieulyeorinhieuhkorean', 12653);
  FGlyphCodes.Add('rightangle', 8735);
  FGlyphCodes.Add('righttackbelowcmb', 793);
  FGlyphCodes.Add('righttriangle', 8895);
  FGlyphCodes.Add('rihiragana', 12426);
  FGlyphCodes.Add('rikatakana', 12522);
  FGlyphCodes.Add('rikatakanahalfwidth', 65432);
  FGlyphCodes.Add('ring', 730);
  FGlyphCodes.Add('ringbelowcmb', 805);
  FGlyphCodes.Add('ringcmb', 778);
  FGlyphCodes.Add('ringhalfleft', 703);
  FGlyphCodes.Add('ringhalfleftarmenian', 1369);
  FGlyphCodes.Add('ringhalfleftbelowcmb', 796);
  FGlyphCodes.Add('ringhalfleftcentered', 723);
  FGlyphCodes.Add('ringhalfright', 702);
  FGlyphCodes.Add('ringhalfrightbelowcmb', 825);
  FGlyphCodes.Add('ringhalfrightcentered', 722);
  FGlyphCodes.Add('rinvertedbreve', 531);
  FGlyphCodes.Add('rittorusquare', 13137);
  FGlyphCodes.Add('rlinebelow', 7775);
  FGlyphCodes.Add('rlongleg', 636);
  FGlyphCodes.Add('rlonglegturned', 634);
  FGlyphCodes.Add('rmonospace', 65362);
  FGlyphCodes.Add('rohiragana', 12429);
  FGlyphCodes.Add('rokatakana', 12525);
  FGlyphCodes.Add('rokatakanahalfwidth', 65435);
  FGlyphCodes.Add('roruathai', 3619);
  FGlyphCodes.Add('rparen', 9389);
  FGlyphCodes.Add('rrabengali', 2524);
  FGlyphCodes.Add('rradeva', 2353);
  FGlyphCodes.Add('rragurmukhi', 2652);
  FGlyphCodes.Add('rreharabic', 1681);
  FGlyphCodes.Add('rrehfinalarabic', 64397);
  FGlyphCodes.Add('rrvocalicbengali', 2528);
  FGlyphCodes.Add('rrvocalicdeva', 2400);
  FGlyphCodes.Add('rrvocalicgujarati', 2784);
  FGlyphCodes.Add('rrvocalicvowelsignbengali', 2500);
  FGlyphCodes.Add('rrvocalicvowelsigndeva', 2372);
  FGlyphCodes.Add('rrvocalicvowelsigngujarati', 2756);
  FGlyphCodes.Add('rsuperior', 63217);
  FGlyphCodes.Add('rtblock', 9616);
  FGlyphCodes.Add('rturned', 633);
  FGlyphCodes.Add('rturnedsuperior', 692);
  FGlyphCodes.Add('ruhiragana', 12427);
  FGlyphCodes.Add('rukatakana', 12523);
  FGlyphCodes.Add('rukatakanahalfwidth', 65433);
  FGlyphCodes.Add('rupeemarkbengali', 2546);
  FGlyphCodes.Add('rupeesignbengali', 2547);
  FGlyphCodes.Add('rupiah', 63197);
  FGlyphCodes.Add('ruthai', 3620);
  FGlyphCodes.Add('rvocalicbengali', 2443);
  FGlyphCodes.Add('rvocalicdeva', 2315);
  FGlyphCodes.Add('rvocalicgujarati', 2699);
  FGlyphCodes.Add('rvocalicvowelsignbengali', 2499);
  FGlyphCodes.Add('rvocalicvowelsigndeva', 2371);
  FGlyphCodes.Add('rvocalicvowelsigngujarati', 2755);
  FGlyphCodes.Add('s', 115);
  FGlyphCodes.Add('sabengali', 2488);
  FGlyphCodes.Add('sacute', 347);
  FGlyphCodes.Add('sacutedotaccent', 7781);
  FGlyphCodes.Add('sadarabic', 1589);
  FGlyphCodes.Add('sadeva', 2360);
  FGlyphCodes.Add('sadfinalarabic', 65210);
  FGlyphCodes.Add('sadinitialarabic', 65211);
  FGlyphCodes.Add('sadmedialarabic', 65212);
  FGlyphCodes.Add('sagujarati', 2744);
  FGlyphCodes.Add('sagurmukhi', 2616);
  FGlyphCodes.Add('sahiragana', 12373);
  FGlyphCodes.Add('sakatakana', 12469);
  FGlyphCodes.Add('sakatakanahalfwidth', 65403);
  FGlyphCodes.Add('sallallahoualayhewasallamarabic', 65018);
  FGlyphCodes.Add('samekh', 1505);
  FGlyphCodes.Add('samekhdagesh', 64321);
  FGlyphCodes.Add('samekhdageshhebrew', 64321);
  FGlyphCodes.Add('samekhhebrew', 1505);
  FGlyphCodes.Add('saraaathai', 3634);
  FGlyphCodes.Add('saraaethai', 3649);
  FGlyphCodes.Add('saraaimaimalaithai', 3652);
  FGlyphCodes.Add('saraaimaimuanthai', 3651);
  FGlyphCodes.Add('saraamthai', 3635);
  FGlyphCodes.Add('saraathai', 3632);
  FGlyphCodes.Add('saraethai', 3648);
  FGlyphCodes.Add('saraiileftthai', 63622);
  FGlyphCodes.Add('saraiithai', 3637);
  FGlyphCodes.Add('saraileftthai', 63621);
  FGlyphCodes.Add('saraithai', 3636);
  FGlyphCodes.Add('saraothai', 3650);
  FGlyphCodes.Add('saraueeleftthai', 63624);
  FGlyphCodes.Add('saraueethai', 3639);
  FGlyphCodes.Add('saraueleftthai', 63623);
  FGlyphCodes.Add('sarauethai', 3638);
  FGlyphCodes.Add('sarauthai', 3640);
  FGlyphCodes.Add('sarauuthai', 3641);
  FGlyphCodes.Add('sbopomofo', 12569);
  FGlyphCodes.Add('scaron', 353);
  FGlyphCodes.Add('scarondotaccent', 7783);
  FGlyphCodes.Add('scedilla', 351);
  FGlyphCodes.Add('schwa', 601);
  FGlyphCodes.Add('schwacyrillic', 1241);
  FGlyphCodes.Add('schwadieresiscyrillic', 1243);
  FGlyphCodes.Add('schwahook', 602);
  FGlyphCodes.Add('scircle', 9442);
  FGlyphCodes.Add('scircumflex', 349);
  FGlyphCodes.Add('scommaaccent', 537);
  FGlyphCodes.Add('sdotaccent', 7777);
  FGlyphCodes.Add('sdotbelow', 7779);
  FGlyphCodes.Add('sdotbelowdotaccent', 7785);
  FGlyphCodes.Add('seagullbelowcmb', 828);
  FGlyphCodes.Add('second', 8243);
  FGlyphCodes.Add('secondtonechinese', 714);
  FGlyphCodes.Add('section', 167);
  FGlyphCodes.Add('seenarabic', 1587);
  FGlyphCodes.Add('seenfinalarabic', 65202);
  FGlyphCodes.Add('seeninitialarabic', 65203);
  FGlyphCodes.Add('seenmedialarabic', 65204);
  FGlyphCodes.Add('segol', 1462);
  FGlyphCodes.Add('segol13', 1462);
  FGlyphCodes.Add('segol1f', 1462);
  FGlyphCodes.Add('segol2c', 1462);
  FGlyphCodes.Add('segolhebrew', 1462);
  FGlyphCodes.Add('segolnarrowhebrew', 1462);
  FGlyphCodes.Add('segolquarterhebrew', 1462);
  FGlyphCodes.Add('segoltahebrew', 1426);
  FGlyphCodes.Add('segolwidehebrew', 1462);
  FGlyphCodes.Add('seharmenian', 1405);
  FGlyphCodes.Add('sehiragana', 12379);
  FGlyphCodes.Add('sekatakana', 12475);
  FGlyphCodes.Add('sekatakanahalfwidth', 65406);
  FGlyphCodes.Add('semicolon', 59);
  FGlyphCodes.Add('semicolonarabic', 1563);
  FGlyphCodes.Add('semicolonmonospace', 65307);
  FGlyphCodes.Add('semicolonsmall', 65108);
  FGlyphCodes.Add('semivoicedmarkkana', 12444);
  FGlyphCodes.Add('semivoicedmarkkanahalfwidth', 65439);
  FGlyphCodes.Add('sentisquare', 13090);
  FGlyphCodes.Add('sentosquare', 13091);
  FGlyphCodes.Add('seven', 55);
  FGlyphCodes.Add('sevenarabic', 1639);
  FGlyphCodes.Add('sevenbengali', 2541);
  FGlyphCodes.Add('sevencircle', 9318);
  FGlyphCodes.Add('sevencircleinversesansserif', 10128);
  FGlyphCodes.Add('sevendeva', 2413);
  FGlyphCodes.Add('seveneighths', 8542);
  FGlyphCodes.Add('sevengujarati', 2797);
  FGlyphCodes.Add('sevengurmukhi', 2669);
  FGlyphCodes.Add('sevenhackarabic', 1639);
  FGlyphCodes.Add('sevenhangzhou', 12327);
  FGlyphCodes.Add('sevenideographicparen', 12838);
  FGlyphCodes.Add('seveninferior', 8327);
  FGlyphCodes.Add('sevenmonospace', 65303);
  FGlyphCodes.Add('sevenoldstyle', 63287);
  FGlyphCodes.Add('sevenparen', 9338);
  FGlyphCodes.Add('sevenperiod', 9358);
  FGlyphCodes.Add('sevenpersian', 1783);
  FGlyphCodes.Add('sevenroman', 8566);
  FGlyphCodes.Add('sevensuperior', 8311);
  FGlyphCodes.Add('seventeencircle', 9328);
  FGlyphCodes.Add('seventeenparen', 9348);
  FGlyphCodes.Add('seventeenperiod', 9368);
  FGlyphCodes.Add('seventhai', 3671);
  FGlyphCodes.Add('sfthyphen', 173);
  FGlyphCodes.Add('shaarmenian', 1399);
  FGlyphCodes.Add('shabengali', 2486);
  FGlyphCodes.Add('shacyrillic', 1096);
  FGlyphCodes.Add('shaddaarabic', 1617);
  FGlyphCodes.Add('shaddadammaarabic', 64609);
  FGlyphCodes.Add('shaddadammatanarabic', 64606);
  FGlyphCodes.Add('shaddafathaarabic', 64608);
  FGlyphCodes.Add('shaddakasraarabic', 64610);
  FGlyphCodes.Add('shaddakasratanarabic', 64607);
  FGlyphCodes.Add('shade', 9618);
  FGlyphCodes.Add('shadedark', 9619);
  FGlyphCodes.Add('shadelight', 9617);
  FGlyphCodes.Add('shademedium', 9618);
  FGlyphCodes.Add('shadeva', 2358);
  FGlyphCodes.Add('shagujarati', 2742);
  FGlyphCodes.Add('shagurmukhi', 2614);
  FGlyphCodes.Add('shalshelethebrew', 1427);
  FGlyphCodes.Add('shbopomofo', 12565);
  FGlyphCodes.Add('shchacyrillic', 1097);
  FGlyphCodes.Add('sheenarabic', 1588);
  FGlyphCodes.Add('sheenfinalarabic', 65206);
  FGlyphCodes.Add('sheeninitialarabic', 65207);
  FGlyphCodes.Add('sheenmedialarabic', 65208);
  FGlyphCodes.Add('sheicoptic', 995);
  FGlyphCodes.Add('sheqel', 8362);
  FGlyphCodes.Add('sheqelhebrew', 8362);
  FGlyphCodes.Add('sheva', 1456);
  FGlyphCodes.Add('sheva115', 1456);
  FGlyphCodes.Add('sheva15', 1456);
  FGlyphCodes.Add('sheva22', 1456);
  FGlyphCodes.Add('sheva2e', 1456);
  FGlyphCodes.Add('shevahebrew', 1456);
  FGlyphCodes.Add('shevanarrowhebrew', 1456);
  FGlyphCodes.Add('shevaquarterhebrew', 1456);
  FGlyphCodes.Add('shevawidehebrew', 1456);
  FGlyphCodes.Add('shhacyrillic', 1211);
  FGlyphCodes.Add('shimacoptic', 1005);
  FGlyphCodes.Add('shin', 1513);
  FGlyphCodes.Add('shindagesh', 64329);
  FGlyphCodes.Add('shindageshhebrew', 64329);
  FGlyphCodes.Add('shindageshshindot', 64300);
  FGlyphCodes.Add('shindageshshindothebrew', 64300);
  FGlyphCodes.Add('shindageshsindot', 64301);
  FGlyphCodes.Add('shindageshsindothebrew', 64301);
  FGlyphCodes.Add('shindothebrew', 1473);
  FGlyphCodes.Add('shinhebrew', 1513);
  FGlyphCodes.Add('shinshindot', 64298);
  FGlyphCodes.Add('shinshindothebrew', 64298);
  FGlyphCodes.Add('shinsindot', 64299);
  FGlyphCodes.Add('shinsindothebrew', 64299);
  FGlyphCodes.Add('shook', 642);
  FGlyphCodes.Add('sigma', 963);
  FGlyphCodes.Add('sigma1', 962);
  FGlyphCodes.Add('sigmafinal', 962);
  FGlyphCodes.Add('sigmalunatesymbolgreek', 1010);
  FGlyphCodes.Add('sihiragana', 12375);
  FGlyphCodes.Add('sikatakana', 12471);
  FGlyphCodes.Add('sikatakanahalfwidth', 65404);
  FGlyphCodes.Add('siluqhebrew', 1469);
  FGlyphCodes.Add('siluqlefthebrew', 1469);
  FGlyphCodes.Add('similar', 8764);
  FGlyphCodes.Add('sindothebrew', 1474);
  FGlyphCodes.Add('siosacirclekorean', 12916);
  FGlyphCodes.Add('siosaparenkorean', 12820);
  FGlyphCodes.Add('sioscieuckorean', 12670);
  FGlyphCodes.Add('sioscirclekorean', 12902);
  FGlyphCodes.Add('sioskiyeokkorean', 12666);
  FGlyphCodes.Add('sioskorean', 12613);
  FGlyphCodes.Add('siosnieunkorean', 12667);
  FGlyphCodes.Add('siosparenkorean', 12806);
  FGlyphCodes.Add('siospieupkorean', 12669);
  FGlyphCodes.Add('siostikeutkorean', 12668);
  FGlyphCodes.Add('six', 54);
  FGlyphCodes.Add('sixarabic', 1638);
  FGlyphCodes.Add('sixbengali', 2540);
  FGlyphCodes.Add('sixcircle', 9317);
  FGlyphCodes.Add('sixcircleinversesansserif', 10127);
  FGlyphCodes.Add('sixdeva', 2412);
  FGlyphCodes.Add('sixgujarati', 2796);
  FGlyphCodes.Add('sixgurmukhi', 2668);
  FGlyphCodes.Add('sixhackarabic', 1638);
  FGlyphCodes.Add('sixhangzhou', 12326);
  FGlyphCodes.Add('sixideographicparen', 12837);
  FGlyphCodes.Add('sixinferior', 8326);
  FGlyphCodes.Add('sixmonospace', 65302);
  FGlyphCodes.Add('sixoldstyle', 63286);
  FGlyphCodes.Add('sixparen', 9337);
  FGlyphCodes.Add('sixperiod', 9357);
  FGlyphCodes.Add('sixpersian', 1782);
  FGlyphCodes.Add('sixroman', 8565);
  FGlyphCodes.Add('sixsuperior', 8310);
  FGlyphCodes.Add('sixteencircle', 9327);
  FGlyphCodes.Add('sixteencurrencydenominatorbengali', 2553);
  FGlyphCodes.Add('sixteenparen', 9347);
  FGlyphCodes.Add('sixteenperiod', 9367);
  FGlyphCodes.Add('sixthai', 3670);
  FGlyphCodes.Add('slash', 47);
  FGlyphCodes.Add('slashmonospace', 65295);
  FGlyphCodes.Add('slong', 383);
  FGlyphCodes.Add('slongdotaccent', 7835);
  FGlyphCodes.Add('smileface', 9786);
  FGlyphCodes.Add('smonospace', 65363);
  FGlyphCodes.Add('sofpasuqhebrew', 1475);
  FGlyphCodes.Add('softhyphen', 173);
  FGlyphCodes.Add('softsigncyrillic', 1100);
  FGlyphCodes.Add('sohiragana', 12381);
  FGlyphCodes.Add('sokatakana', 12477);
  FGlyphCodes.Add('sokatakanahalfwidth', 65407);
  FGlyphCodes.Add('soliduslongoverlaycmb', 824);
  FGlyphCodes.Add('solidusshortoverlaycmb', 823);
  FGlyphCodes.Add('sorusithai', 3625);
  FGlyphCodes.Add('sosalathai', 3624);
  FGlyphCodes.Add('sosothai', 3595);
  FGlyphCodes.Add('sosuathai', 3626);
  FGlyphCodes.Add('space', 32);
  FGlyphCodes.Add('spacehackarabic', 32);
  FGlyphCodes.Add('spade', 9824);
  FGlyphCodes.Add('spadesuitblack', 9824);
  FGlyphCodes.Add('spadesuitwhite', 9828);
  FGlyphCodes.Add('sparen', 9390);
  FGlyphCodes.Add('squarebelowcmb', 827);
  FGlyphCodes.Add('squarecc', 13252);
  FGlyphCodes.Add('squarecm', 13213);
  FGlyphCodes.Add('squarediagonalcrosshatchfill', 9641);
  FGlyphCodes.Add('squarehorizontalfill', 9636);
  FGlyphCodes.Add('squarekg', 13199);
  FGlyphCodes.Add('squarekm', 13214);
  FGlyphCodes.Add('squarekmcapital', 13262);
  FGlyphCodes.Add('squareln', 13265);
  FGlyphCodes.Add('squarelog', 13266);
  FGlyphCodes.Add('squaremg', 13198);
  FGlyphCodes.Add('squaremil', 13269);
  FGlyphCodes.Add('squaremm', 13212);
  FGlyphCodes.Add('squaremsquared', 13217);
  FGlyphCodes.Add('squareorthogonalcrosshatchfill', 9638);
  FGlyphCodes.Add('squareupperlefttolowerrightfill', 9639);
  FGlyphCodes.Add('squareupperrighttolowerleftfill', 9640);
  FGlyphCodes.Add('squareverticalfill', 9637);
  FGlyphCodes.Add('squarewhitewithsmallblack', 9635);
  FGlyphCodes.Add('srsquare', 13275);
  FGlyphCodes.Add('ssabengali', 2487);
  FGlyphCodes.Add('ssadeva', 2359);
  FGlyphCodes.Add('ssagujarati', 2743);
  FGlyphCodes.Add('ssangcieuckorean', 12617);
  FGlyphCodes.Add('ssanghieuhkorean', 12677);
  FGlyphCodes.Add('ssangieungkorean', 12672);
  FGlyphCodes.Add('ssangkiyeokkorean', 12594);
  FGlyphCodes.Add('ssangnieunkorean', 12645);
  FGlyphCodes.Add('ssangpieupkorean', 12611);
  FGlyphCodes.Add('ssangsioskorean', 12614);
  FGlyphCodes.Add('ssangtikeutkorean', 12600);
  FGlyphCodes.Add('ssuperior', 63218);
  FGlyphCodes.Add('sterling', 163);
  FGlyphCodes.Add('sterlingmonospace', 65505);
  FGlyphCodes.Add('strokelongoverlaycmb', 822);
  FGlyphCodes.Add('strokeshortoverlaycmb', 821);
  FGlyphCodes.Add('subset', 8834);
  FGlyphCodes.Add('subsetnotequal', 8842);
  FGlyphCodes.Add('subsetorequal', 8838);
  FGlyphCodes.Add('succeeds', 8827);
  FGlyphCodes.Add('suchthat', 8715);
  FGlyphCodes.Add('suhiragana', 12377);
  FGlyphCodes.Add('sukatakana', 12473);
  FGlyphCodes.Add('sukatakanahalfwidth', 65405);
  FGlyphCodes.Add('sukunarabic', 1618);
  FGlyphCodes.Add('summation', 8721);
  FGlyphCodes.Add('sun', 9788);
  FGlyphCodes.Add('superset', 8835);
  FGlyphCodes.Add('supersetnotequal', 8843);
  FGlyphCodes.Add('supersetorequal', 8839);
  FGlyphCodes.Add('svsquare', 13276);
  FGlyphCodes.Add('syouwaerasquare', 13180);
  FGlyphCodes.Add('t', 116);
  FGlyphCodes.Add('tabengali', 2468);
  FGlyphCodes.Add('tackdown', 8868);
  FGlyphCodes.Add('tackleft', 8867);
  FGlyphCodes.Add('tadeva', 2340);
  FGlyphCodes.Add('tagujarati', 2724);
  FGlyphCodes.Add('tagurmukhi', 2596);
  FGlyphCodes.Add('taharabic', 1591);
  FGlyphCodes.Add('tahfinalarabic', 65218);
  FGlyphCodes.Add('tahinitialarabic', 65219);
  FGlyphCodes.Add('tahiragana', 12383);
  FGlyphCodes.Add('tahmedialarabic', 65220);
  FGlyphCodes.Add('taisyouerasquare', 13181);
  FGlyphCodes.Add('takatakana', 12479);
  FGlyphCodes.Add('takatakanahalfwidth', 65408);
  FGlyphCodes.Add('tatweelarabic', 1600);
  FGlyphCodes.Add('tau', 964);
  FGlyphCodes.Add('tav', 1514);
  FGlyphCodes.Add('tavdages', 64330);
  FGlyphCodes.Add('tavdagesh', 64330);
  FGlyphCodes.Add('tavdageshhebrew', 64330);
  FGlyphCodes.Add('tavhebrew', 1514);
  FGlyphCodes.Add('tbar', 359);
  FGlyphCodes.Add('tbopomofo', 12554);
  FGlyphCodes.Add('tcaron', 357);
  FGlyphCodes.Add('tccurl', 680);
  FGlyphCodes.Add('tcedilla', 355);
  FGlyphCodes.Add('tcheharabic', 1670);
  FGlyphCodes.Add('tchehfinalarabic', 64379);
  FGlyphCodes.Add('tchehinitialarabic', 64380);
  FGlyphCodes.Add('tchehmedialarabic', 64381);
  FGlyphCodes.Add('tcircle', 9443);
  FGlyphCodes.Add('tcircumflexbelow', 7793);
  FGlyphCodes.Add('tcommaaccent', 355);
  FGlyphCodes.Add('tdieresis', 7831);
  FGlyphCodes.Add('tdotaccent', 7787);
  FGlyphCodes.Add('tdotbelow', 7789);
  FGlyphCodes.Add('tecyrillic', 1090);
  FGlyphCodes.Add('tedescendercyrillic', 1197);
  FGlyphCodes.Add('teharabic', 1578);
  FGlyphCodes.Add('tehfinalarabic', 65174);
  FGlyphCodes.Add('tehhahinitialarabic', 64674);
  FGlyphCodes.Add('tehhahisolatedarabic', 64524);
  FGlyphCodes.Add('tehinitialarabic', 65175);
  FGlyphCodes.Add('tehiragana', 12390);
  FGlyphCodes.Add('tehjeeminitialarabic', 64673);
  FGlyphCodes.Add('tehjeemisolatedarabic', 64523);
  FGlyphCodes.Add('tehmarbutaarabic', 1577);
  FGlyphCodes.Add('tehmarbutafinalarabic', 65172);
  FGlyphCodes.Add('tehmedialarabic', 65176);
  FGlyphCodes.Add('tehmeeminitialarabic', 64676);
  FGlyphCodes.Add('tehmeemisolatedarabic', 64526);
  FGlyphCodes.Add('tehnoonfinalarabic', 64627);
  FGlyphCodes.Add('tekatakana', 12486);
  FGlyphCodes.Add('tekatakanahalfwidth', 65411);
  FGlyphCodes.Add('telephone', 8481);
  FGlyphCodes.Add('telephoneblack', 9742);
  FGlyphCodes.Add('telishagedolahebrew', 1440);
  FGlyphCodes.Add('telishaqetanahebrew', 1449);
  FGlyphCodes.Add('tencircle', 9321);
  FGlyphCodes.Add('tenideographicparen', 12841);
  FGlyphCodes.Add('tenparen', 9341);
  FGlyphCodes.Add('tenperiod', 9361);
  FGlyphCodes.Add('tenroman', 8569);
  FGlyphCodes.Add('tesh', 679);
  FGlyphCodes.Add('tet', 1496);
  FGlyphCodes.Add('tetdagesh', 64312);
  FGlyphCodes.Add('tetdageshhebrew', 64312);
  FGlyphCodes.Add('tethebrew', 1496);
  FGlyphCodes.Add('tetsecyrillic', 1205);
  FGlyphCodes.Add('tevirhebrew', 1435);
  FGlyphCodes.Add('tevirlefthebrew', 1435);
  FGlyphCodes.Add('thabengali', 2469);
  FGlyphCodes.Add('thadeva', 2341);
  FGlyphCodes.Add('thagujarati', 2725);
  FGlyphCodes.Add('thagurmukhi', 2597);
  FGlyphCodes.Add('thalarabic', 1584);
  FGlyphCodes.Add('thalfinalarabic', 65196);
  FGlyphCodes.Add('thanthakhatlowleftthai', 63640);
  FGlyphCodes.Add('thanthakhatlowrightthai', 63639);
  FGlyphCodes.Add('thanthakhatthai', 3660);
  FGlyphCodes.Add('thanthakhatupperleftthai', 63638);
  FGlyphCodes.Add('theharabic', 1579);
  FGlyphCodes.Add('thehfinalarabic', 65178);
  FGlyphCodes.Add('thehinitialarabic', 65179);
  FGlyphCodes.Add('thehmedialarabic', 65180);
  FGlyphCodes.Add('thereexists', 8707);
  FGlyphCodes.Add('therefore', 8756);
  FGlyphCodes.Add('theta', 952);
  FGlyphCodes.Add('theta1', 977);
  FGlyphCodes.Add('thetasymbolgreek', 977);
  FGlyphCodes.Add('thieuthacirclekorean', 12921);
  FGlyphCodes.Add('thieuthaparenkorean', 12825);
  FGlyphCodes.Add('thieuthcirclekorean', 12907);
  FGlyphCodes.Add('thieuthkorean', 12620);
  FGlyphCodes.Add('thieuthparenkorean', 12811);
  FGlyphCodes.Add('thirteencircle', 9324);
  FGlyphCodes.Add('thirteenparen', 9344);
  FGlyphCodes.Add('thirteenperiod', 9364);
  FGlyphCodes.Add('thonangmonthothai', 3601);
  FGlyphCodes.Add('thook', 429);
  FGlyphCodes.Add('thophuthaothai', 3602);
  FGlyphCodes.Add('thorn', 254);
  FGlyphCodes.Add('thothahanthai', 3607);
  FGlyphCodes.Add('thothanthai', 3600);
  FGlyphCodes.Add('thothongthai', 3608);
  FGlyphCodes.Add('thothungthai', 3606);
  FGlyphCodes.Add('thousandcyrillic', 1154);
  FGlyphCodes.Add('thousandsseparatorarabic', 1644);
  FGlyphCodes.Add('thousandsseparatorpersian', 1644);
  FGlyphCodes.Add('three', 51);
  FGlyphCodes.Add('threearabic', 1635);
  FGlyphCodes.Add('threebengali', 2537);
  FGlyphCodes.Add('threecircle', 9314);
  FGlyphCodes.Add('threecircleinversesansserif', 10124);
  FGlyphCodes.Add('threedeva', 2409);
  FGlyphCodes.Add('threeeighths', 8540);
  FGlyphCodes.Add('threegujarati', 2793);
  FGlyphCodes.Add('threegurmukhi', 2665);
  FGlyphCodes.Add('threehackarabic', 1635);
  FGlyphCodes.Add('threehangzhou', 12323);
  FGlyphCodes.Add('threeideographicparen', 12834);
  FGlyphCodes.Add('threeinferior', 8323);
  FGlyphCodes.Add('threemonospace', 65299);
  FGlyphCodes.Add('threenumeratorbengali', 2550);
  FGlyphCodes.Add('threeoldstyle', 63283);
  FGlyphCodes.Add('threeparen', 9334);
  FGlyphCodes.Add('threeperiod', 9354);
  FGlyphCodes.Add('threepersian', 1779);
  FGlyphCodes.Add('threequarters', 190);
  FGlyphCodes.Add('threequartersemdash', 63198);
  FGlyphCodes.Add('threeroman', 8562);
  FGlyphCodes.Add('threesuperior', 179);
  FGlyphCodes.Add('threethai', 3667);
  FGlyphCodes.Add('thzsquare', 13204);
  FGlyphCodes.Add('tihiragana', 12385);
  FGlyphCodes.Add('tikatakana', 12481);
  FGlyphCodes.Add('tikatakanahalfwidth', 65409);
  FGlyphCodes.Add('tikeutacirclekorean', 12912);
  FGlyphCodes.Add('tikeutaparenkorean', 12816);
  FGlyphCodes.Add('tikeutcirclekorean', 12898);
  FGlyphCodes.Add('tikeutkorean', 12599);
  FGlyphCodes.Add('tikeutparenkorean', 12802);
  FGlyphCodes.Add('tilde', 732);
  FGlyphCodes.Add('tildebelowcmb', 816);
  FGlyphCodes.Add('tildecmb', 771);
  FGlyphCodes.Add('tildecomb', 771);
  FGlyphCodes.Add('tildedoublecmb', 864);
  FGlyphCodes.Add('tildeoperator', 8764);
  FGlyphCodes.Add('tildeoverlaycmb', 820);
  FGlyphCodes.Add('tildeverticalcmb', 830);
  FGlyphCodes.Add('timescircle', 8855);
  FGlyphCodes.Add('tipehahebrew', 1430);
  FGlyphCodes.Add('tipehalefthebrew', 1430);
  FGlyphCodes.Add('tippigurmukhi', 2672);
  FGlyphCodes.Add('titlocyrilliccmb', 1155);
  FGlyphCodes.Add('tiwnarmenian', 1407);
  FGlyphCodes.Add('tlinebelow', 7791);
  FGlyphCodes.Add('tmonospace', 65364);
  FGlyphCodes.Add('toarmenian', 1385);
  FGlyphCodes.Add('tohiragana', 12392);
  FGlyphCodes.Add('tokatakana', 12488);
  FGlyphCodes.Add('tokatakanahalfwidth', 65412);
  FGlyphCodes.Add('tonebarextrahighmod', 741);
  FGlyphCodes.Add('tonebarextralowmod', 745);
  FGlyphCodes.Add('tonebarhighmod', 742);
  FGlyphCodes.Add('tonebarlowmod', 744);
  FGlyphCodes.Add('tonebarmidmod', 743);
  FGlyphCodes.Add('tonefive', 445);
  FGlyphCodes.Add('tonesix', 389);
  FGlyphCodes.Add('tonetwo', 424);
  FGlyphCodes.Add('tonos', 900);
  FGlyphCodes.Add('tonsquare', 13095);
  FGlyphCodes.Add('topatakthai', 3599);
  FGlyphCodes.Add('tortoiseshellbracketleft', 12308);
  FGlyphCodes.Add('tortoiseshellbracketleftsmall', 65117);
  FGlyphCodes.Add('tortoiseshellbracketleftvertical', 65081);
  FGlyphCodes.Add('tortoiseshellbracketright', 12309);
  FGlyphCodes.Add('tortoiseshellbracketrightsmall', 65118);
  FGlyphCodes.Add('tortoiseshellbracketrightvertical', 65082);
  FGlyphCodes.Add('totaothai', 3605);
  FGlyphCodes.Add('tpalatalhook', 427);
  FGlyphCodes.Add('tparen', 9391);
  FGlyphCodes.Add('trademark', 8482);
  FGlyphCodes.Add('trademarksans', 63722);
  FGlyphCodes.Add('trademarkserif', 63195);
  FGlyphCodes.Add('tretroflexhook', 648);
  FGlyphCodes.Add('triagdn', 9660);
  FGlyphCodes.Add('triaglf', 9668);
  FGlyphCodes.Add('triagrt', 9658);
  FGlyphCodes.Add('triagup', 9650);
  FGlyphCodes.Add('ts', 678);
  FGlyphCodes.Add('tsadi', 1510);
  FGlyphCodes.Add('tsadidagesh', 64326);
  FGlyphCodes.Add('tsadidageshhebrew', 64326);
  FGlyphCodes.Add('tsadihebrew', 1510);
  FGlyphCodes.Add('tsecyrillic', 1094);
  FGlyphCodes.Add('tsere', 1461);
  FGlyphCodes.Add('tsere12', 1461);
  FGlyphCodes.Add('tsere1e', 1461);
  FGlyphCodes.Add('tsere2b', 1461);
  FGlyphCodes.Add('tserehebrew', 1461);
  FGlyphCodes.Add('tserenarrowhebrew', 1461);
  FGlyphCodes.Add('tserequarterhebrew', 1461);
  FGlyphCodes.Add('tserewidehebrew', 1461);
  FGlyphCodes.Add('tshecyrillic', 1115);
  FGlyphCodes.Add('tsuperior', 63219);
  FGlyphCodes.Add('ttabengali', 2463);
  FGlyphCodes.Add('ttadeva', 2335);
  FGlyphCodes.Add('ttagujarati', 2719);
  FGlyphCodes.Add('ttagurmukhi', 2591);
  FGlyphCodes.Add('tteharabic', 1657);
  FGlyphCodes.Add('ttehfinalarabic', 64359);
  FGlyphCodes.Add('ttehinitialarabic', 64360);
  FGlyphCodes.Add('ttehmedialarabic', 64361);
  FGlyphCodes.Add('tthabengali', 2464);
  FGlyphCodes.Add('tthadeva', 2336);
  FGlyphCodes.Add('tthagujarati', 2720);
  FGlyphCodes.Add('tthagurmukhi', 2592);
  FGlyphCodes.Add('tturned', 647);
  FGlyphCodes.Add('tuhiragana', 12388);
  FGlyphCodes.Add('tukatakana', 12484);
  FGlyphCodes.Add('tukatakanahalfwidth', 65410);
  FGlyphCodes.Add('tusmallhiragana', 12387);
  FGlyphCodes.Add('tusmallkatakana', 12483);
  FGlyphCodes.Add('tusmallkatakanahalfwidth', 65391);
  FGlyphCodes.Add('twelvecircle', 9323);
  FGlyphCodes.Add('twelveparen', 9343);
  FGlyphCodes.Add('twelveperiod', 9363);
  FGlyphCodes.Add('twelveroman', 8571);
  FGlyphCodes.Add('twentycircle', 9331);
  FGlyphCodes.Add('twentyhangzhou', 21316);
  FGlyphCodes.Add('twentyparen', 9351);
  FGlyphCodes.Add('twentyperiod', 9371);
  FGlyphCodes.Add('two', 50);
  FGlyphCodes.Add('twoarabic', 1634);
  FGlyphCodes.Add('twobengali', 2536);
  FGlyphCodes.Add('twocircle', 9313);
  FGlyphCodes.Add('twocircleinversesansserif', 10123);
  FGlyphCodes.Add('twodeva', 2408);
  FGlyphCodes.Add('twodotenleader', 8229);
  FGlyphCodes.Add('twodotleader', 8229);
  FGlyphCodes.Add('twodotleadervertical', 65072);
  FGlyphCodes.Add('twogujarati', 2792);
  FGlyphCodes.Add('twogurmukhi', 2664);
  FGlyphCodes.Add('twohackarabic', 1634);
  FGlyphCodes.Add('twohangzhou', 12322);
  FGlyphCodes.Add('twoideographicparen', 12833);
  FGlyphCodes.Add('twoinferior', 8322);
  FGlyphCodes.Add('twomonospace', 65298);
  FGlyphCodes.Add('twonumeratorbengali', 2549);
  FGlyphCodes.Add('twooldstyle', 63282);
  FGlyphCodes.Add('twoparen', 9333);
  FGlyphCodes.Add('twoperiod', 9353);
  FGlyphCodes.Add('twopersian', 1778);
  FGlyphCodes.Add('tworoman', 8561);
  FGlyphCodes.Add('twostroke', 443);
  FGlyphCodes.Add('twosuperior', 178);
  FGlyphCodes.Add('twothai', 3666);
  FGlyphCodes.Add('twothirds', 8532);
  FGlyphCodes.Add('u', 117);
  FGlyphCodes.Add('uacute', 250);
  FGlyphCodes.Add('ubar', 649);
  FGlyphCodes.Add('ubengali', 2441);
  FGlyphCodes.Add('ubopomofo', 12584);
  FGlyphCodes.Add('ubreve', 365);
  FGlyphCodes.Add('ucaron', 468);
  FGlyphCodes.Add('ucircle', 9444);
  FGlyphCodes.Add('ucircumflex', 251);
  FGlyphCodes.Add('ucircumflexbelow', 7799);
  FGlyphCodes.Add('ucyrillic', 1091);
  FGlyphCodes.Add('udattadeva', 2385);
  FGlyphCodes.Add('udblacute', 369);
  FGlyphCodes.Add('udblgrave', 533);
  FGlyphCodes.Add('udeva', 2313);
  FGlyphCodes.Add('udieresis', 252);
  FGlyphCodes.Add('udieresisacute', 472);
  FGlyphCodes.Add('udieresisbelow', 7795);
  FGlyphCodes.Add('udieresiscaron', 474);
  FGlyphCodes.Add('udieresiscyrillic', 1265);
  FGlyphCodes.Add('udieresisgrave', 476);
  FGlyphCodes.Add('udieresismacron', 470);
  FGlyphCodes.Add('udotbelow', 7909);
  FGlyphCodes.Add('ugrave', 249);
  FGlyphCodes.Add('ugujarati', 2697);
  FGlyphCodes.Add('ugurmukhi', 2569);
  FGlyphCodes.Add('uhiragana', 12358);
  FGlyphCodes.Add('uhookabove', 7911);
  FGlyphCodes.Add('uhorn', 432);
  FGlyphCodes.Add('uhornacute', 7913);
  FGlyphCodes.Add('uhorndotbelow', 7921);
  FGlyphCodes.Add('uhorngrave', 7915);
  FGlyphCodes.Add('uhornhookabove', 7917);
  FGlyphCodes.Add('uhorntilde', 7919);
  FGlyphCodes.Add('uhungarumlaut', 369);
  FGlyphCodes.Add('uhungarumlautcyrillic', 1267);
  FGlyphCodes.Add('uinvertedbreve', 535);
  FGlyphCodes.Add('ukatakana', 12454);
  FGlyphCodes.Add('ukatakanahalfwidth', 65395);
  FGlyphCodes.Add('ukcyrillic', 1145);
  FGlyphCodes.Add('ukorean', 12636);
  FGlyphCodes.Add('umacron', 363);
  FGlyphCodes.Add('umacroncyrillic', 1263);
  FGlyphCodes.Add('umacrondieresis', 7803);
  FGlyphCodes.Add('umatragurmukhi', 2625);
  FGlyphCodes.Add('umonospace', 65365);
  FGlyphCodes.Add('underscore', 95);
  FGlyphCodes.Add('underscoredbl', 8215);
  FGlyphCodes.Add('underscoremonospace', 65343);
  FGlyphCodes.Add('underscorevertical', 65075);
  FGlyphCodes.Add('underscorewavy', 65103);
  FGlyphCodes.Add('uni00A0', 160);
  FGlyphCodes.Add('union', 8746);
  FGlyphCodes.Add('universal', 8704);
  FGlyphCodes.Add('uogonek', 371);
  FGlyphCodes.Add('uparen', 9392);
  FGlyphCodes.Add('upblock', 9600);
  FGlyphCodes.Add('upperdothebrew', 1476);
  FGlyphCodes.Add('upsilon', 965);
  FGlyphCodes.Add('upsilondieresis', 971);
  FGlyphCodes.Add('upsilondieresistonos', 944);
  FGlyphCodes.Add('upsilonlatin', 650);
  FGlyphCodes.Add('upsilontonos', 973);
  FGlyphCodes.Add('uptackbelowcmb', 797);
  FGlyphCodes.Add('uptackmod', 724);
  FGlyphCodes.Add('uragurmukhi', 2675);
  FGlyphCodes.Add('uring', 367);
  FGlyphCodes.Add('ushortcyrillic', 1118);
  FGlyphCodes.Add('usmallhiragana', 12357);
  FGlyphCodes.Add('usmallkatakana', 12453);
  FGlyphCodes.Add('usmallkatakanahalfwidth', 65385);
  FGlyphCodes.Add('ustraightcyrillic', 1199);
  FGlyphCodes.Add('ustraightstrokecyrillic', 1201);
  FGlyphCodes.Add('utilde', 361);
  FGlyphCodes.Add('utildeacute', 7801);
  FGlyphCodes.Add('utildebelow', 7797);
  FGlyphCodes.Add('uubengali', 2442);
  FGlyphCodes.Add('uudeva', 2314);
  FGlyphCodes.Add('uugujarati', 2698);
  FGlyphCodes.Add('uugurmukhi', 2570);
  FGlyphCodes.Add('uumatragurmukhi', 2626);
  FGlyphCodes.Add('uuvowelsignbengali', 2498);
  FGlyphCodes.Add('uuvowelsigndeva', 2370);
  FGlyphCodes.Add('uuvowelsigngujarati', 2754);
  FGlyphCodes.Add('uvowelsignbengali', 2497);
  FGlyphCodes.Add('uvowelsigndeva', 2369);
  FGlyphCodes.Add('uvowelsigngujarati', 2753);
  FGlyphCodes.Add('v', 118);
  FGlyphCodes.Add('vadeva', 2357);
  FGlyphCodes.Add('vagujarati', 2741);
  FGlyphCodes.Add('vagurmukhi', 2613);
  FGlyphCodes.Add('vakatakana', 12535);
  FGlyphCodes.Add('vav', 1493);
  FGlyphCodes.Add('vavdagesh', 64309);
  FGlyphCodes.Add('vavdagesh65', 64309);
  FGlyphCodes.Add('vavdageshhebrew', 64309);
  FGlyphCodes.Add('vavhebrew', 1493);
  FGlyphCodes.Add('vavholam', 64331);
  FGlyphCodes.Add('vavholamhebrew', 64331);
  FGlyphCodes.Add('vavvavhebrew', 1520);
  FGlyphCodes.Add('vavyodhebrew', 1521);
  FGlyphCodes.Add('vcircle', 9445);
  FGlyphCodes.Add('vdotbelow', 7807);
  FGlyphCodes.Add('vecyrillic', 1074);
  FGlyphCodes.Add('veharabic', 1700);
  FGlyphCodes.Add('vehfinalarabic', 64363);
  FGlyphCodes.Add('vehinitialarabic', 64364);
  FGlyphCodes.Add('vehmedialarabic', 64365);
  FGlyphCodes.Add('vekatakana', 12537);
  FGlyphCodes.Add('venus', 9792);
  FGlyphCodes.Add('verticalbar', 124);
  FGlyphCodes.Add('verticallineabovecmb', 781);
  FGlyphCodes.Add('verticallinebelowcmb', 809);
  FGlyphCodes.Add('verticallinelowmod', 716);
  FGlyphCodes.Add('verticallinemod', 712);
  FGlyphCodes.Add('vewarmenian', 1406);
  FGlyphCodes.Add('vhook', 651);
  FGlyphCodes.Add('vikatakana', 12536);
  FGlyphCodes.Add('viramabengali', 2509);
  FGlyphCodes.Add('viramadeva', 2381);
  FGlyphCodes.Add('viramagujarati', 2765);
  FGlyphCodes.Add('visargabengali', 2435);
  FGlyphCodes.Add('visargadeva', 2307);
  FGlyphCodes.Add('visargagujarati', 2691);
  FGlyphCodes.Add('vmonospace', 65366);
  FGlyphCodes.Add('voarmenian', 1400);
  FGlyphCodes.Add('voicediterationhiragana', 12446);
  FGlyphCodes.Add('voicediterationkatakana', 12542);
  FGlyphCodes.Add('voicedmarkkana', 12443);
  FGlyphCodes.Add('voicedmarkkanahalfwidth', 65438);
  FGlyphCodes.Add('vokatakana', 12538);
  FGlyphCodes.Add('vparen', 9393);
  FGlyphCodes.Add('vtilde', 7805);
  FGlyphCodes.Add('vturned', 652);
  FGlyphCodes.Add('vuhiragana', 12436);
  FGlyphCodes.Add('vukatakana', 12532);
  FGlyphCodes.Add('w', 119);
  FGlyphCodes.Add('wacute', 7811);
  FGlyphCodes.Add('waekorean', 12633);
  FGlyphCodes.Add('wahiragana', 12431);
  FGlyphCodes.Add('wakatakana', 12527);
  FGlyphCodes.Add('wakatakanahalfwidth', 65436);
  FGlyphCodes.Add('wakorean', 12632);
  FGlyphCodes.Add('wasmallhiragana', 12430);
  FGlyphCodes.Add('wasmallkatakana', 12526);
  FGlyphCodes.Add('wattosquare', 13143);
  FGlyphCodes.Add('wavedash', 12316);
  FGlyphCodes.Add('wavyunderscorevertical', 65076);
  FGlyphCodes.Add('wawarabic', 1608);
  FGlyphCodes.Add('wawfinalarabic', 65262);
  FGlyphCodes.Add('wawhamzaabovearabic', 1572);
  FGlyphCodes.Add('wawhamzaabovefinalarabic', 65158);
  FGlyphCodes.Add('wbsquare', 13277);
  FGlyphCodes.Add('wcircle', 9446);
  FGlyphCodes.Add('wcircumflex', 373);
  FGlyphCodes.Add('wdieresis', 7813);
  FGlyphCodes.Add('wdotaccent', 7815);
  FGlyphCodes.Add('wdotbelow', 7817);
  FGlyphCodes.Add('wehiragana', 12433);
  FGlyphCodes.Add('weierstrass', 8472);
  FGlyphCodes.Add('wekatakana', 12529);
  FGlyphCodes.Add('wekorean', 12638);
  FGlyphCodes.Add('weokorean', 12637);
  FGlyphCodes.Add('wgrave', 7809);
  FGlyphCodes.Add('whitebullet', 9702);
  FGlyphCodes.Add('whitecircle', 9675);
  FGlyphCodes.Add('whitecircleinverse', 9689);
  FGlyphCodes.Add('whitecornerbracketleft', 12302);
  FGlyphCodes.Add('whitecornerbracketleftvertical', 65091);
  FGlyphCodes.Add('whitecornerbracketright', 12303);
  FGlyphCodes.Add('whitecornerbracketrightvertical', 65092);
  FGlyphCodes.Add('whitediamond', 9671);
  FGlyphCodes.Add('whitediamondcontainingblacksmalldiamond', 9672);
  FGlyphCodes.Add('whitedownpointingsmalltriangle', 9663);
  FGlyphCodes.Add('whitedownpointingtriangle', 9661);
  FGlyphCodes.Add('whiteleftpointingsmalltriangle', 9667);
  FGlyphCodes.Add('whiteleftpointingtriangle', 9665);
  FGlyphCodes.Add('whitelenticularbracketleft', 12310);
  FGlyphCodes.Add('whitelenticularbracketright', 12311);
  FGlyphCodes.Add('whiterightpointingsmalltriangle', 9657);
  FGlyphCodes.Add('whiterightpointingtriangle', 9655);
  FGlyphCodes.Add('whitesmallsquare', 9643);
  FGlyphCodes.Add('whitesmilingface', 9786);
  FGlyphCodes.Add('whitesquare', 9633);
  FGlyphCodes.Add('whitestar', 9734);
  FGlyphCodes.Add('whitetelephone', 9743);
  FGlyphCodes.Add('whitetortoiseshellbracketleft', 12312);
  FGlyphCodes.Add('whitetortoiseshellbracketright', 12313);
  FGlyphCodes.Add('whiteuppointingsmalltriangle', 9653);
  FGlyphCodes.Add('whiteuppointingtriangle', 9651);
  FGlyphCodes.Add('wihiragana', 12432);
  FGlyphCodes.Add('wikatakana', 12528);
  FGlyphCodes.Add('wikorean', 12639);
  FGlyphCodes.Add('wmonospace', 65367);
  FGlyphCodes.Add('wohiragana', 12434);
  FGlyphCodes.Add('wokatakana', 12530);
  FGlyphCodes.Add('wokatakanahalfwidth', 65382);
  FGlyphCodes.Add('won', 8361);
  FGlyphCodes.Add('wonmonospace', 65510);
  FGlyphCodes.Add('wowaenthai', 3623);
  FGlyphCodes.Add('wparen', 9394);
  FGlyphCodes.Add('wring', 7832);
  FGlyphCodes.Add('wsuperior', 695);
  FGlyphCodes.Add('wturned', 653);
  FGlyphCodes.Add('wynn', 447);
  FGlyphCodes.Add('x', 120);
  FGlyphCodes.Add('xabovecmb', 829);
  FGlyphCodes.Add('xbopomofo', 12562);
  FGlyphCodes.Add('xcircle', 9447);
  FGlyphCodes.Add('xdieresis', 7821);
  FGlyphCodes.Add('xdotaccent', 7819);
  FGlyphCodes.Add('xeharmenian', 1389);
  FGlyphCodes.Add('xi', 958);
  FGlyphCodes.Add('xmonospace', 65368);
  FGlyphCodes.Add('xparen', 9395);
  FGlyphCodes.Add('xsuperior', 739);
  FGlyphCodes.Add('y', 121);
  FGlyphCodes.Add('yaadosquare', 13134);
  FGlyphCodes.Add('yabengali', 2479);
  FGlyphCodes.Add('yacute', 253);
  FGlyphCodes.Add('yadeva', 2351);
  FGlyphCodes.Add('yaekorean', 12626);
  FGlyphCodes.Add('yagujarati', 2735);
  FGlyphCodes.Add('yagurmukhi', 2607);
  FGlyphCodes.Add('yahiragana', 12420);
  FGlyphCodes.Add('yakatakana', 12516);
  FGlyphCodes.Add('yakatakanahalfwidth', 65428);
  FGlyphCodes.Add('yakorean', 12625);
  FGlyphCodes.Add('yamakkanthai', 3662);
  FGlyphCodes.Add('yasmallhiragana', 12419);
  FGlyphCodes.Add('yasmallkatakana', 12515);
  FGlyphCodes.Add('yasmallkatakanahalfwidth', 65388);
  FGlyphCodes.Add('yatcyrillic', 1123);
  FGlyphCodes.Add('ycircle', 9448);
  FGlyphCodes.Add('ycircumflex', 375);
  FGlyphCodes.Add('ydieresis', 255);
  FGlyphCodes.Add('ydotaccent', 7823);
  FGlyphCodes.Add('ydotbelow', 7925);
  FGlyphCodes.Add('yeharabic', 1610);
  FGlyphCodes.Add('yehbarreearabic', 1746);
  FGlyphCodes.Add('yehbarreefinalarabic', 64431);
  FGlyphCodes.Add('yehfinalarabic', 65266);
  FGlyphCodes.Add('yehhamzaabovearabic', 1574);
  FGlyphCodes.Add('yehhamzaabovefinalarabic', 65162);
  FGlyphCodes.Add('yehhamzaaboveinitialarabic', 65163);
  FGlyphCodes.Add('yehhamzaabovemedialarabic', 65164);
  FGlyphCodes.Add('yehinitialarabic', 65267);
  FGlyphCodes.Add('yehmedialarabic', 65268);
  FGlyphCodes.Add('yehmeeminitialarabic', 64733);
  FGlyphCodes.Add('yehmeemisolatedarabic', 64600);
  FGlyphCodes.Add('yehnoonfinalarabic', 64660);
  FGlyphCodes.Add('yehthreedotsbelowarabic', 1745);
  FGlyphCodes.Add('yekorean', 12630);
  FGlyphCodes.Add('yen', 165);
  FGlyphCodes.Add('yenmonospace', 65509);
  FGlyphCodes.Add('yeokorean', 12629);
  FGlyphCodes.Add('yeorinhieuhkorean', 12678);
  FGlyphCodes.Add('yerahbenyomohebrew', 1450);
  FGlyphCodes.Add('yerahbenyomolefthebrew', 1450);
  FGlyphCodes.Add('yericyrillic', 1099);
  FGlyphCodes.Add('yerudieresiscyrillic', 1273);
  FGlyphCodes.Add('yesieungkorean', 12673);
  FGlyphCodes.Add('yesieungpansioskorean', 12675);
  FGlyphCodes.Add('yesieungsioskorean', 12674);
  FGlyphCodes.Add('yetivhebrew', 1434);
  FGlyphCodes.Add('ygrave', 7923);
  FGlyphCodes.Add('yhook', 436);
  FGlyphCodes.Add('yhookabove', 7927);
  FGlyphCodes.Add('yiarmenian', 1397);
  FGlyphCodes.Add('yicyrillic', 1111);
  FGlyphCodes.Add('yikorean', 12642);
  FGlyphCodes.Add('yinyang', 9775);
  FGlyphCodes.Add('yiwnarmenian', 1410);
  FGlyphCodes.Add('ymonospace', 65369);
  FGlyphCodes.Add('yod', 1497);
  FGlyphCodes.Add('yoddagesh', 64313);
  FGlyphCodes.Add('yoddageshhebrew', 64313);
  FGlyphCodes.Add('yodhebrew', 1497);
  FGlyphCodes.Add('yodyodhebrew', 1522);
  FGlyphCodes.Add('yodyodpatahhebrew', 64287);
  FGlyphCodes.Add('yohiragana', 12424);
  FGlyphCodes.Add('yoikorean', 12681);
  FGlyphCodes.Add('yokatakana', 12520);
  FGlyphCodes.Add('yokatakanahalfwidth', 65430);
  FGlyphCodes.Add('yokorean', 12635);
  FGlyphCodes.Add('yosmallhiragana', 12423);
  FGlyphCodes.Add('yosmallkatakana', 12519);
  FGlyphCodes.Add('yosmallkatakanahalfwidth', 65390);
  FGlyphCodes.Add('yotgreek', 1011);
  FGlyphCodes.Add('yoyaekorean', 12680);
  FGlyphCodes.Add('yoyakorean', 12679);
  FGlyphCodes.Add('yoyakthai', 3618);
  FGlyphCodes.Add('yoyingthai', 3597);
  FGlyphCodes.Add('yparen', 9396);
  FGlyphCodes.Add('ypogegrammeni', 890);
  FGlyphCodes.Add('ypogegrammenigreekcmb', 837);
  FGlyphCodes.Add('yr', 422);
  FGlyphCodes.Add('yring', 7833);
  FGlyphCodes.Add('ysuperior', 696);
  FGlyphCodes.Add('ytilde', 7929);
  FGlyphCodes.Add('yturned', 654);
  FGlyphCodes.Add('yuhiragana', 12422);
  FGlyphCodes.Add('yuikorean', 12684);
  FGlyphCodes.Add('yukatakana', 12518);
  FGlyphCodes.Add('yukatakanahalfwidth', 65429);
  FGlyphCodes.Add('yukorean', 12640);
  FGlyphCodes.Add('yusbigcyrillic', 1131);
  FGlyphCodes.Add('yusbigiotifiedcyrillic', 1133);
  FGlyphCodes.Add('yuslittlecyrillic', 1127);
  FGlyphCodes.Add('yuslittleiotifiedcyrillic', 1129);
  FGlyphCodes.Add('yusmallhiragana', 12421);
  FGlyphCodes.Add('yusmallkatakana', 12517);
  FGlyphCodes.Add('yusmallkatakanahalfwidth', 65389);
  FGlyphCodes.Add('yuyekorean', 12683);
  FGlyphCodes.Add('yuyeokorean', 12682);
  FGlyphCodes.Add('yyabengali', 2527);
  FGlyphCodes.Add('yyadeva', 2399);
  FGlyphCodes.Add('z', 122);
  FGlyphCodes.Add('zaarmenian', 1382);
  FGlyphCodes.Add('zacute', 378);
  FGlyphCodes.Add('zadeva', 2395);
  FGlyphCodes.Add('zagurmukhi', 2651);
  FGlyphCodes.Add('zaharabic', 1592);
  FGlyphCodes.Add('zahfinalarabic', 65222);
  FGlyphCodes.Add('zahinitialarabic', 65223);
  FGlyphCodes.Add('zahiragana', 12374);
  FGlyphCodes.Add('zahmedialarabic', 65224);
  FGlyphCodes.Add('zainarabic', 1586);
  FGlyphCodes.Add('zainfinalarabic', 65200);
  FGlyphCodes.Add('zakatakana', 12470);
  FGlyphCodes.Add('zaqefgadolhebrew', 1429);
  FGlyphCodes.Add('zaqefqatanhebrew', 1428);
  FGlyphCodes.Add('zarqahebrew', 1432);
  FGlyphCodes.Add('zayin', 1494);
  FGlyphCodes.Add('zayindagesh', 64310);
  FGlyphCodes.Add('zayindageshhebrew', 64310);
  FGlyphCodes.Add('zayinhebrew', 1494);
  FGlyphCodes.Add('zbopomofo', 12567);
  FGlyphCodes.Add('zcaron', 382);
  FGlyphCodes.Add('zcircle', 9449);
  FGlyphCodes.Add('zcircumflex', 7825);
  FGlyphCodes.Add('zcurl', 657);
  FGlyphCodes.Add('zdot', 380);
  FGlyphCodes.Add('zdotaccent', 380);
  FGlyphCodes.Add('zdotbelow', 7827);
  FGlyphCodes.Add('zecyrillic', 1079);
  FGlyphCodes.Add('zedescendercyrillic', 1177);
  FGlyphCodes.Add('zedieresiscyrillic', 1247);
  FGlyphCodes.Add('zehiragana', 12380);
  FGlyphCodes.Add('zekatakana', 12476);
  FGlyphCodes.Add('zero', 48);
  FGlyphCodes.Add('zeroarabic', 1632);
  FGlyphCodes.Add('zerobengali', 2534);
  FGlyphCodes.Add('zerodeva', 2406);
  FGlyphCodes.Add('zerogujarati', 2790);
  FGlyphCodes.Add('zerogurmukhi', 2662);
  FGlyphCodes.Add('zerohackarabic', 1632);
  FGlyphCodes.Add('zeroinferior', 8320);
  FGlyphCodes.Add('zeromonospace', 65296);
  FGlyphCodes.Add('zerooldstyle', 63280);
  FGlyphCodes.Add('zeropersian', 1776);
  FGlyphCodes.Add('zerosuperior', 8304);
  FGlyphCodes.Add('zerothai', 3664);
  FGlyphCodes.Add('zerowidthjoiner', 65279);
  FGlyphCodes.Add('zerowidthnonjoiner', 8204);
  FGlyphCodes.Add('zerowidthspace', 8203);
  FGlyphCodes.Add('zeta', 950);
  FGlyphCodes.Add('zhbopomofo', 12563);
  FGlyphCodes.Add('zhearmenian', 1386);
  FGlyphCodes.Add('zhebrevecyrillic', 1218);
  FGlyphCodes.Add('zhecyrillic', 1078);
  FGlyphCodes.Add('zhedescendercyrillic', 1175);
  FGlyphCodes.Add('zhedieresiscyrillic', 1245);
  FGlyphCodes.Add('zihiragana', 12376);
  FGlyphCodes.Add('zikatakana', 12472);
  FGlyphCodes.Add('zinorhebrew', 1454);
  FGlyphCodes.Add('zlinebelow', 7829);
  FGlyphCodes.Add('zmonospace', 65370);
  FGlyphCodes.Add('zohiragana', 12382);
  FGlyphCodes.Add('zokatakana', 12478);
  FGlyphCodes.Add('zparen', 9397);
  FGlyphCodes.Add('zretroflexhook', 656);
  FGlyphCodes.Add('zstroke', 438);
  FGlyphCodes.Add('zuhiragana', 12378);
  FGlyphCodes.Add('zukatakana', 12474);
end;

{ TdxFontFileMacRomanEncoding }

class function TdxFontFileMacRomanEncoding.GetName: string;
begin
  Result := 'MacRomanEncoding';
end;

procedure TdxFontFileMacRomanEncoding.Initialize;
begin
  Dictionary.Add(65, TdxGlyphNames.A);
  Dictionary.Add(174, TdxGlyphNames.AE);
  Dictionary.Add(231, TdxGlyphNames.Aacute);
  Dictionary.Add(229, TdxGlyphNames.Acircumflex);
  Dictionary.Add(128, TdxGlyphNames.Adieresis);
  Dictionary.Add(203, TdxGlyphNames.Agrave);
  Dictionary.Add(129, TdxGlyphNames.Aring);
  Dictionary.Add(204, TdxGlyphNames.Atilde);
  Dictionary.Add(66, TdxGlyphNames.B);
  Dictionary.Add(67, TdxGlyphNames.C);
  Dictionary.Add(130, TdxGlyphNames.Ccedilla);
  Dictionary.Add(68, TdxGlyphNames.D);
  Dictionary.Add(69, TdxGlyphNames.E);
  Dictionary.Add(131, TdxGlyphNames.Eacute);
  Dictionary.Add(230, TdxGlyphNames.Ecircumflex);
  Dictionary.Add(232, TdxGlyphNames.Edieresis);
  Dictionary.Add(233, TdxGlyphNames.Egrave);
  Dictionary.Add(70, TdxGlyphNames.F);
  Dictionary.Add(71, TdxGlyphNames.G);
  Dictionary.Add(72, TdxGlyphNames.H);
  Dictionary.Add(73, TdxGlyphNames.I);
  Dictionary.Add(234, TdxGlyphNames.Iacute);
  Dictionary.Add(235, TdxGlyphNames.Icircumflex);
  Dictionary.Add(236, TdxGlyphNames.Idieresis);
  Dictionary.Add(237, TdxGlyphNames.Igrave);
  Dictionary.Add(74, TdxGlyphNames.J);
  Dictionary.Add(75, TdxGlyphNames.K);
  Dictionary.Add(76, TdxGlyphNames.L);
  Dictionary.Add(77, TdxGlyphNames.M);
  Dictionary.Add(78, TdxGlyphNames.N);
  Dictionary.Add(132, TdxGlyphNames.Ntilde);
  Dictionary.Add(79, TdxGlyphNames.O);
  Dictionary.Add(206, TdxGlyphNames.OE);
  Dictionary.Add(238, TdxGlyphNames.Oacute);
  Dictionary.Add(239, TdxGlyphNames.Ocircumflex);
  Dictionary.Add(133, TdxGlyphNames.Odieresis);
  Dictionary.Add(241, TdxGlyphNames.Ograve);
  Dictionary.Add(175, TdxGlyphNames.Oslash);
  Dictionary.Add(205, TdxGlyphNames.Otilde);
  Dictionary.Add(80, TdxGlyphNames.P);
  Dictionary.Add(81, TdxGlyphNames.Q);
  Dictionary.Add(82, TdxGlyphNames.R);
  Dictionary.Add(83, TdxGlyphNames.S);
  Dictionary.Add(84, TdxGlyphNames.T);
  Dictionary.Add(85, TdxGlyphNames.U);
  Dictionary.Add(242, TdxGlyphNames.Uacute);
  Dictionary.Add(243, TdxGlyphNames.Ucircumflex);
  Dictionary.Add(134, TdxGlyphNames.Udieresis);
  Dictionary.Add(244, TdxGlyphNames.Ugrave);
  Dictionary.Add(86, TdxGlyphNames.V);
  Dictionary.Add(87, TdxGlyphNames.W);
  Dictionary.Add(88, TdxGlyphNames.X);
  Dictionary.Add(89, TdxGlyphNames.Y);
  Dictionary.Add(217, TdxGlyphNames.Ydieresis);
  Dictionary.Add(90, TdxGlyphNames.Z);
  Dictionary.Add(97, TdxGlyphNames.LowerA);
  Dictionary.Add(135, TdxGlyphNames.LowerAacute);
  Dictionary.Add(137, TdxGlyphNames.LowerAcircumflex);
  Dictionary.Add(171, TdxGlyphNames.LowerAcute);
  Dictionary.Add(138, TdxGlyphNames.LowerAdieresis);
  Dictionary.Add(190, TdxGlyphNames.LowerAe);
  Dictionary.Add(136, TdxGlyphNames.LowerAgrave);
  Dictionary.Add(38, TdxGlyphNames.LowerAmpersand);
  Dictionary.Add(140, TdxGlyphNames.LowerAring);
  Dictionary.Add(94, TdxGlyphNames.LowerAsciicircum);
  Dictionary.Add(126, TdxGlyphNames.LowerAsciitilde);
  Dictionary.Add(42, TdxGlyphNames.LowerAsterisk);
  Dictionary.Add(64, TdxGlyphNames.LowerAt);
  Dictionary.Add(139, TdxGlyphNames.LowerAtilde);
  Dictionary.Add(98, TdxGlyphNames.LowerB);
  Dictionary.Add(92, TdxGlyphNames.LowerBackslash);
  Dictionary.Add(124, TdxGlyphNames.LowerBar);
  Dictionary.Add(123, TdxGlyphNames.LowerBraceleft);
  Dictionary.Add(125, TdxGlyphNames.LowerBraceright);
  Dictionary.Add(91, TdxGlyphNames.LowerBracketleft);
  Dictionary.Add(93, TdxGlyphNames.LowerBracketright);
  Dictionary.Add(249, TdxGlyphNames.LowerBreve);
  Dictionary.Add(165, TdxGlyphNames.LowerBullet);
  Dictionary.Add(99, TdxGlyphNames.LowerC);
  Dictionary.Add(255, TdxGlyphNames.LowerCaron);
  Dictionary.Add(141, TdxGlyphNames.LowerCcedilla);
  Dictionary.Add(252, TdxGlyphNames.LowerCedilla);
  Dictionary.Add(162, TdxGlyphNames.LowerCent);
  Dictionary.Add(246, TdxGlyphNames.LowerCircumflex);
  Dictionary.Add(58, TdxGlyphNames.LowerColon);
  Dictionary.Add(44, TdxGlyphNames.LowerComma);
  Dictionary.Add(169, TdxGlyphNames.LowerCopyright);
  Dictionary.Add(219, TdxGlyphNames.LowerCurrency);
  Dictionary.Add(100, TdxGlyphNames.LowerD);
  Dictionary.Add(160, TdxGlyphNames.LowerDagger);
  Dictionary.Add(224, TdxGlyphNames.LowerDaggerdbl);
  Dictionary.Add(161, TdxGlyphNames.LowerDegree);
  Dictionary.Add(172, TdxGlyphNames.LowerDieresis);
  Dictionary.Add(214, TdxGlyphNames.LowerDivide);
  Dictionary.Add(36, TdxGlyphNames.LowerDollar);
  Dictionary.Add(250, TdxGlyphNames.LowerDotaccent);
  Dictionary.Add(245, TdxGlyphNames.LowerDotlessi);
  Dictionary.Add(101, TdxGlyphNames.LowerE);
  Dictionary.Add(142, TdxGlyphNames.LowerEacute);
  Dictionary.Add(144, TdxGlyphNames.LowerEcircumflex);
  Dictionary.Add(145, TdxGlyphNames.LowerEdieresis);
  Dictionary.Add(143, TdxGlyphNames.LowerEgrave);
  Dictionary.Add(56, TdxGlyphNames.LowerEight);
  Dictionary.Add(201, TdxGlyphNames.LowerEllipsis);
  Dictionary.Add(209, TdxGlyphNames.LowerEmdash);
  Dictionary.Add(208, TdxGlyphNames.LowerEndash);
  Dictionary.Add(61, TdxGlyphNames.LowerEqual);
  Dictionary.Add(33, TdxGlyphNames.LowerExclam);
  Dictionary.Add(193, TdxGlyphNames.LowerExclamdown);
  Dictionary.Add(102, TdxGlyphNames.LowerF);
  Dictionary.Add(222, TdxGlyphNames.LowerFi);
  Dictionary.Add(53, TdxGlyphNames.LowerFive);
  Dictionary.Add(223, TdxGlyphNames.LowerFl);
  Dictionary.Add(196, TdxGlyphNames.LowerFlorin);
  Dictionary.Add(52, TdxGlyphNames.LowerFour);
  Dictionary.Add(218, TdxGlyphNames.LowerFraction);
  Dictionary.Add(103, TdxGlyphNames.LowerG);
  Dictionary.Add(167, TdxGlyphNames.LowerGermandbls);
  Dictionary.Add(96, TdxGlyphNames.LowerGrave);
  Dictionary.Add(62, TdxGlyphNames.LowerGreater);
  Dictionary.Add(199, TdxGlyphNames.LowerGuillemotleft);
  Dictionary.Add(200, TdxGlyphNames.LowerGuillemotright);
  Dictionary.Add(220, TdxGlyphNames.LowerGuilsinglleft);
  Dictionary.Add(221, TdxGlyphNames.LowerGuilsinglright);
  Dictionary.Add(104, TdxGlyphNames.LowerH);
  Dictionary.Add(253, TdxGlyphNames.LowerHungarumlaut);
  Dictionary.Add(45, TdxGlyphNames.LowerHyphen);
  Dictionary.Add(105, TdxGlyphNames.LowerI);
  Dictionary.Add(146, TdxGlyphNames.LowerIacute);
  Dictionary.Add(148, TdxGlyphNames.LowerIcircumflex);
  Dictionary.Add(149, TdxGlyphNames.LowerIdieresis);
  Dictionary.Add(147, TdxGlyphNames.LowerIgrave);
  Dictionary.Add(106, TdxGlyphNames.LowerJ);
  Dictionary.Add(107, TdxGlyphNames.LowerK);
  Dictionary.Add(108, TdxGlyphNames.LowerL);
  Dictionary.Add(60, TdxGlyphNames.LowerLess);
  Dictionary.Add(194, TdxGlyphNames.LowerLogicalnot);
  Dictionary.Add(109, TdxGlyphNames.LowerM);
  Dictionary.Add(248, TdxGlyphNames.LowerMacron);
  Dictionary.Add(181, TdxGlyphNames.LowerMu);
  Dictionary.Add(110, TdxGlyphNames.LowerN);
  Dictionary.Add(57, TdxGlyphNames.LowerNine);
  Dictionary.Add(150, TdxGlyphNames.LowerNtilde);
  Dictionary.Add(35, TdxGlyphNames.LowerNumbersign);
  Dictionary.Add(111, TdxGlyphNames.LowerO);
  Dictionary.Add(151, TdxGlyphNames.LowerOacute);
  Dictionary.Add(153, TdxGlyphNames.LowerOcircumflex);
  Dictionary.Add(154, TdxGlyphNames.LowerOdieresis);
  Dictionary.Add(207, TdxGlyphNames.LowerOe);
  Dictionary.Add(254, TdxGlyphNames.LowerOgonek);
  Dictionary.Add(152, TdxGlyphNames.LowerOgrave);
  Dictionary.Add(49, TdxGlyphNames.LowerOne);
  Dictionary.Add(187, TdxGlyphNames.LowerOrdfeminine);
  Dictionary.Add(188, TdxGlyphNames.LowerOrdmasculine);
  Dictionary.Add(191, TdxGlyphNames.LowerOslash);
  Dictionary.Add(155, TdxGlyphNames.LowerOtilde);
  Dictionary.Add(112, TdxGlyphNames.LowerP);
  Dictionary.Add(166, TdxGlyphNames.LowerParagraph);
  Dictionary.Add(40, TdxGlyphNames.LowerParenleft);
  Dictionary.Add(41, TdxGlyphNames.LowerParenright);
  Dictionary.Add(37, TdxGlyphNames.LowerPercent);
  Dictionary.Add(46, TdxGlyphNames.LowerPeriod);
  Dictionary.Add(225, TdxGlyphNames.LowerPeriodcentered);
  Dictionary.Add(228, TdxGlyphNames.LowerPerthousand);
  Dictionary.Add(43, TdxGlyphNames.LowerPlus);
  Dictionary.Add(177, TdxGlyphNames.LowerPlusminus);
  Dictionary.Add(113, TdxGlyphNames.LowerQ);
  Dictionary.Add(63, TdxGlyphNames.LowerQuestion);
  Dictionary.Add(192, TdxGlyphNames.LowerQuestiondown);
  Dictionary.Add(34, TdxGlyphNames.LowerQuotedbl);
  Dictionary.Add(227, TdxGlyphNames.LowerQuotedblbase);
  Dictionary.Add(210, TdxGlyphNames.LowerQuotedblleft);
  Dictionary.Add(211, TdxGlyphNames.LowerQuotedblright);
  Dictionary.Add(212, TdxGlyphNames.LowerQuoteleft);
  Dictionary.Add(213, TdxGlyphNames.LowerQuoteright);
  Dictionary.Add(226, TdxGlyphNames.LowerQuotesinglbase);
  Dictionary.Add(39, TdxGlyphNames.LowerQuotesingle);
  Dictionary.Add(114, TdxGlyphNames.LowerR);
  Dictionary.Add(168, TdxGlyphNames.LowerRegistered);
  Dictionary.Add(251, TdxGlyphNames.LowerRing);
  Dictionary.Add(115, TdxGlyphNames.LowerS);
  Dictionary.Add(164, TdxGlyphNames.LowerSection);
  Dictionary.Add(59, TdxGlyphNames.LowerSemicolon);
  Dictionary.Add(55, TdxGlyphNames.LowerSeven);
  Dictionary.Add(54, TdxGlyphNames.LowerSix);
  Dictionary.Add(47, TdxGlyphNames.LowerSlash);
  Dictionary.Add(32, TdxGlyphNames.LowerSpace);
  Dictionary.Add(163, TdxGlyphNames.LowerSterling);
  Dictionary.Add(116, TdxGlyphNames.LowerT);
  Dictionary.Add(51, TdxGlyphNames.LowerThree);
  Dictionary.Add(247, TdxGlyphNames.LowerTilde);
  Dictionary.Add(170, TdxGlyphNames.LowerTrademark);
  Dictionary.Add(50, TdxGlyphNames.LowerTwo);
  Dictionary.Add(117, TdxGlyphNames.LowerU);
  Dictionary.Add(156, TdxGlyphNames.LowerUacute);
  Dictionary.Add(158, TdxGlyphNames.LowerUcircumflex);
  Dictionary.Add(159, TdxGlyphNames.LowerUdieresis);
  Dictionary.Add(157, TdxGlyphNames.LowerUgrave);
  Dictionary.Add(95, TdxGlyphNames.LowerUnderscore);
  Dictionary.Add(118, TdxGlyphNames.LowerV);
  Dictionary.Add(119, TdxGlyphNames.LowerW);
  Dictionary.Add(120, TdxGlyphNames.LowerX);
  Dictionary.Add(121, TdxGlyphNames.LowerY);
  Dictionary.Add(216, TdxGlyphNames.LowerYdieresis);
  Dictionary.Add(180, TdxGlyphNames.LowerYen);
  Dictionary.Add(122, TdxGlyphNames.LowerZ);
  Dictionary.Add(48, TdxGlyphNames.LowerZero);
  Dictionary.TrimExcess;
end;

{ TdxFontFileMacRomanReversedEncoding }

constructor TdxFontFileMacRomanReversedEncoding.Create;
begin
  inherited Create;
  FReversedDictionary := TdxPDFWordDictionary.Create;
end;

destructor TdxFontFileMacRomanReversedEncoding.Destroy;
begin
  FreeAndNil(FReversedDictionary);
  inherited Destroy;
end;

procedure TdxFontFileMacRomanReversedEncoding.Initialize;
var
  APair: TPair<Byte, string>;
begin
  inherited Initialize;;
  Dictionary.TrimExcess;
  for APair in Dictionary do
    FReversedDictionary.Add(APair.Value, APair.Key);
  Dictionary.Clear;
end;

{ TdxFontFileStandardEncoding }

class function TdxFontFileStandardEncoding.GetName: string;
begin
  Result := 'StandardEncoding';
end;

procedure TdxFontFileStandardEncoding.Initialize;
begin
  Dictionary.Add(32, 'space');
  Dictionary.Add(33, 'exclam');
  Dictionary.Add(34, 'quotedbl');
  Dictionary.Add(35, 'numbersign');
  Dictionary.Add(36, 'dollar');
  Dictionary.Add(37, 'percent');
  Dictionary.Add(38, 'ampersand');
  Dictionary.Add(39, 'quoteright');
  Dictionary.Add(40, 'parenleft');
  Dictionary.Add(41, 'parenright');
  Dictionary.Add(42, 'asterisk');
  Dictionary.Add(43, 'plus');
  Dictionary.Add(44, 'comma');
  Dictionary.Add(45, 'hyphen');
  Dictionary.Add(46, 'period');
  Dictionary.Add(47, 'slash');
  Dictionary.Add(48, 'zero');
  Dictionary.Add(49, 'one');
  Dictionary.Add(50, 'two');
  Dictionary.Add(51, 'three');
  Dictionary.Add(52, 'four');
  Dictionary.Add(53, 'five');
  Dictionary.Add(54, 'six');
  Dictionary.Add(55, 'seven');
  Dictionary.Add(56, 'eight');
  Dictionary.Add(57, 'nine');
  Dictionary.Add(58, 'colon');
  Dictionary.Add(59, 'semicolon');
  Dictionary.Add(60, 'less');
  Dictionary.Add(61, 'equal');
  Dictionary.Add(62, 'greater');
  Dictionary.Add(63, 'question');
  Dictionary.Add(64, 'at');
  Dictionary.Add(65, 'A');
  Dictionary.Add(66, 'B');
  Dictionary.Add(67, 'C');
  Dictionary.Add(68, 'D');
  Dictionary.Add(69, 'E');
  Dictionary.Add(70, 'F');
  Dictionary.Add(71, 'G');
  Dictionary.Add(72, 'H');
  Dictionary.Add(73, 'I');
  Dictionary.Add(74, 'J');
  Dictionary.Add(75, 'K');
  Dictionary.Add(76, 'L');
  Dictionary.Add(77, 'M');
  Dictionary.Add(78, 'N');
  Dictionary.Add(79, 'O');
  Dictionary.Add(80, 'P');
  Dictionary.Add(81, 'Q');
  Dictionary.Add(82, 'R');
  Dictionary.Add(83, 'S');
  Dictionary.Add(84, 'T');
  Dictionary.Add(85, 'U');
  Dictionary.Add(86, 'V');
  Dictionary.Add(87, 'W');
  Dictionary.Add(88, 'X');
  Dictionary.Add(89, 'Y');
  Dictionary.Add(90, 'Z');
  Dictionary.Add(91, 'bracketleft');
  Dictionary.Add(92, 'backslash');
  Dictionary.Add(93, 'bracketright');
  Dictionary.Add(94, 'asciicircum');
  Dictionary.Add(95, 'underscore');
  Dictionary.Add(96, 'quoteleft');
  Dictionary.Add(97, 'a');
  Dictionary.Add(98, 'b');
  Dictionary.Add(99, 'c');
  Dictionary.Add(100, 'd');
  Dictionary.Add(101, 'e');
  Dictionary.Add(102, 'f');
  Dictionary.Add(103, 'g');
  Dictionary.Add(104, 'h');
  Dictionary.Add(105, 'i');
  Dictionary.Add(106, 'j');
  Dictionary.Add(107, 'k');
  Dictionary.Add(108, 'l');
  Dictionary.Add(109, 'm');
  Dictionary.Add(110, 'n');
  Dictionary.Add(111, 'o');
  Dictionary.Add(112, 'p');
  Dictionary.Add(113, 'q');
  Dictionary.Add(114, 'r');
  Dictionary.Add(115, 's');
  Dictionary.Add(116, 't');
  Dictionary.Add(117, 'u');
  Dictionary.Add(118, 'v');
  Dictionary.Add(119, 'w');
  Dictionary.Add(120, 'x');
  Dictionary.Add(121, 'y');
  Dictionary.Add(122, 'z');
  Dictionary.Add(123, 'braceleft');
  Dictionary.Add(124, 'bar');
  Dictionary.Add(125, 'braceright');
  Dictionary.Add(126, 'asciitilde');
  Dictionary.Add(161, 'exclamdown');
  Dictionary.Add(162, 'cent');
  Dictionary.Add(163, 'sterling');
  Dictionary.Add(164, 'fraction');
  Dictionary.Add(165, 'yen');
  Dictionary.Add(166, 'florin');
  Dictionary.Add(167, 'section');
  Dictionary.Add(168, 'currency');
  Dictionary.Add(169, 'quotesingle');
  Dictionary.Add(170, 'quotedblleft');
  Dictionary.Add(171, 'guillemotleft');
  Dictionary.Add(172, 'guilsinglleft');
  Dictionary.Add(173, 'guilsinglright');
  Dictionary.Add(174, 'fi');
  Dictionary.Add(175, 'fl');
  Dictionary.Add(177, 'endash');
  Dictionary.Add(178, 'dagger');
  Dictionary.Add(179, 'daggerdbl');
  Dictionary.Add(180, 'periodcentered');
  Dictionary.Add(182, 'paragraph');
  Dictionary.Add(183, 'bullet');
  Dictionary.Add(184, 'quotesinglbase');
  Dictionary.Add(185, 'quotedblbase');
  Dictionary.Add(186, 'quotedblright');
  Dictionary.Add(187, 'guillemotright');
  Dictionary.Add(188, 'ellipsis');
  Dictionary.Add(189, 'perthousand');
  Dictionary.Add(191, 'questiondown');
  Dictionary.Add(193, 'grave');
  Dictionary.Add(194, 'acute');
  Dictionary.Add(195, 'circumflex');
  Dictionary.Add(196, 'tilde');
  Dictionary.Add(197, 'macron');
  Dictionary.Add(198, 'breve');
  Dictionary.Add(199, 'dotaccent');
  Dictionary.Add(200, 'dieresis');
  Dictionary.Add(202, 'ring');
  Dictionary.Add(203, 'cedilla');
  Dictionary.Add(205, 'hungarumlaut');
  Dictionary.Add(206, 'ogonek');
  Dictionary.Add(207, 'caron');
  Dictionary.Add(208, 'emdash');
  Dictionary.Add(225, 'AE');
  Dictionary.Add(227, 'ordfeminine');
  Dictionary.Add(232, 'Lslash');
  Dictionary.Add(233, 'Oslash');
  Dictionary.Add(234, 'OE');
  Dictionary.Add(235, 'ordmasculine');
  Dictionary.Add(241, 'ae');
  Dictionary.Add(245, 'dotlessi');
  Dictionary.Add(248, 'lslash');
  Dictionary.Add(249, 'oslash');
  Dictionary.Add(250, 'oe');
  Dictionary.Add(251, 'germandbls');
  Dictionary.TrimExcess;
end;

{ TdxFontFileWinAnsiEncoding }

class function TdxFontFileWinAnsiEncoding.GetName: string;
begin
  Result := 'WinAnsiEncoding';
end;

procedure TdxFontFileWinAnsiEncoding.Initialize;
begin
  Dictionary.Add(65, TdxGlyphNames.A);
  Dictionary.Add(198, TdxGlyphNames.AE);
  Dictionary.Add(193, TdxGlyphNames.Aacute);
  Dictionary.Add(194, TdxGlyphNames.Acircumflex);
  Dictionary.Add(196, TdxGlyphNames.Adieresis);
  Dictionary.Add(192, TdxGlyphNames.Agrave);
  Dictionary.Add(197, TdxGlyphNames.Aring);
  Dictionary.Add(195, TdxGlyphNames.Atilde);
  Dictionary.Add(66, TdxGlyphNames.B);
  Dictionary.Add(67, TdxGlyphNames.C);
  Dictionary.Add(199, TdxGlyphNames.Ccedilla);
  Dictionary.Add(68, TdxGlyphNames.D);
  Dictionary.Add(69, TdxGlyphNames.E);
  Dictionary.Add(201, TdxGlyphNames.Eacute);
  Dictionary.Add(202, TdxGlyphNames.Ecircumflex);
  Dictionary.Add(203, TdxGlyphNames.Edieresis);
  Dictionary.Add(200, TdxGlyphNames.Egrave);
  Dictionary.Add(208, TdxGlyphNames.Eth);
  Dictionary.Add(128, TdxGlyphNames.Euro);
  Dictionary.Add(70, TdxGlyphNames.F);
  Dictionary.Add(71, TdxGlyphNames.G);
  Dictionary.Add(72, TdxGlyphNames.H);
  Dictionary.Add(73, TdxGlyphNames.I);
  Dictionary.Add(205, TdxGlyphNames.Iacute);
  Dictionary.Add(206, TdxGlyphNames.Icircumflex);
  Dictionary.Add(207, TdxGlyphNames.Idieresis);
  Dictionary.Add(204, TdxGlyphNames.Igrave);
  Dictionary.Add(74, TdxGlyphNames.J);
  Dictionary.Add(75, TdxGlyphNames.K);
  Dictionary.Add(76, TdxGlyphNames.L);
  Dictionary.Add(77, TdxGlyphNames.M);
  Dictionary.Add(78, TdxGlyphNames.N);
  Dictionary.Add(209, TdxGlyphNames.Ntilde);
  Dictionary.Add(79, TdxGlyphNames.O);
  Dictionary.Add(140, TdxGlyphNames.OE);
  Dictionary.Add(211, TdxGlyphNames.Oacute);
  Dictionary.Add(212, TdxGlyphNames.Ocircumflex);
  Dictionary.Add(214, TdxGlyphNames.Odieresis);
  Dictionary.Add(210, TdxGlyphNames.Ograve);
  Dictionary.Add(216, TdxGlyphNames.Oslash);
  Dictionary.Add(213, TdxGlyphNames.Otilde);
  Dictionary.Add(80, TdxGlyphNames.P);
  Dictionary.Add(81, TdxGlyphNames.Q);
  Dictionary.Add(82, TdxGlyphNames.R);
  Dictionary.Add(83, TdxGlyphNames.S);
  Dictionary.Add(138, TdxGlyphNames.Scaron);
  Dictionary.Add(84, TdxGlyphNames.T);
  Dictionary.Add(222, TdxGlyphNames.Thorn);
  Dictionary.Add(85, TdxGlyphNames.U);
  Dictionary.Add(218, TdxGlyphNames.Uacute);
  Dictionary.Add(219, TdxGlyphNames.Ucircumflex);
  Dictionary.Add(220, TdxGlyphNames.Udieresis);
  Dictionary.Add(217, TdxGlyphNames.Ugrave);
  Dictionary.Add(86, TdxGlyphNames.V);
  Dictionary.Add(87, TdxGlyphNames.W);
  Dictionary.Add(88, TdxGlyphNames.X);
  Dictionary.Add(89, TdxGlyphNames.Y);
  Dictionary.Add(221, TdxGlyphNames.Yacute);
  Dictionary.Add(159, TdxGlyphNames.Ydieresis);
  Dictionary.Add(90, TdxGlyphNames.Z);
  Dictionary.Add(142, TdxGlyphNames.Zcaron);
  Dictionary.Add(97, TdxGlyphNames.LowerA);
  Dictionary.Add(225, TdxGlyphNames.LowerAacute);
  Dictionary.Add(226, TdxGlyphNames.LowerAcircumflex);
  Dictionary.Add(180, TdxGlyphNames.LowerAcute);
  Dictionary.Add(228, TdxGlyphNames.LowerAdieresis);
  Dictionary.Add(230, TdxGlyphNames.LowerAe);
  Dictionary.Add(224, TdxGlyphNames.LowerAgrave);
  Dictionary.Add(38, TdxGlyphNames.LowerAmpersand);
  Dictionary.Add(229, TdxGlyphNames.LowerAring);
  Dictionary.Add(94, TdxGlyphNames.LowerAsciicircum);
  Dictionary.Add(126, TdxGlyphNames.LowerAsciitilde);
  Dictionary.Add(42, TdxGlyphNames.LowerAsterisk);
  Dictionary.Add(64, TdxGlyphNames.LowerAt);
  Dictionary.Add(227, TdxGlyphNames.LowerAtilde);
  Dictionary.Add(98, TdxGlyphNames.LowerB);
  Dictionary.Add(92, TdxGlyphNames.LowerBackslash);
  Dictionary.Add(124, TdxGlyphNames.LowerBar);
  Dictionary.Add(123, TdxGlyphNames.LowerBraceleft);
  Dictionary.Add(125, TdxGlyphNames.LowerBraceright);
  Dictionary.Add(91, TdxGlyphNames.LowerBracketleft);
  Dictionary.Add(93, TdxGlyphNames.LowerBracketright);
  Dictionary.Add(166, TdxGlyphNames.LowerBrokenbar);
  Dictionary.Add(127, TdxGlyphNames.LowerBullet);
  Dictionary.Add(149, TdxGlyphNames.LowerBullet);
  Dictionary.Add(99, TdxGlyphNames.LowerC);
  Dictionary.Add(231, TdxGlyphNames.LowerCcedilla);
  Dictionary.Add(184, TdxGlyphNames.LowerCedilla);
  Dictionary.Add(162, TdxGlyphNames.LowerCent);
  Dictionary.Add(136, TdxGlyphNames.LowerCircumflex);
  Dictionary.Add(58, TdxGlyphNames.LowerColon);
  Dictionary.Add(44, TdxGlyphNames.LowerComma);
  Dictionary.Add(169, TdxGlyphNames.LowerCopyright);
  Dictionary.Add(164, TdxGlyphNames.LowerCurrency);
  Dictionary.Add(100, TdxGlyphNames.LowerD);
  Dictionary.Add(134, TdxGlyphNames.LowerDagger);
  Dictionary.Add(135, TdxGlyphNames.LowerDaggerdbl);
  Dictionary.Add(176, TdxGlyphNames.LowerDegree);
  Dictionary.Add(168, TdxGlyphNames.LowerDieresis);
  Dictionary.Add(247, TdxGlyphNames.LowerDivide);
  Dictionary.Add(36, TdxGlyphNames.LowerDollar);
  Dictionary.Add(101, TdxGlyphNames.LowerE);
  Dictionary.Add(233, TdxGlyphNames.LowerEacute);
  Dictionary.Add(234, TdxGlyphNames.LowerEcircumflex);
  Dictionary.Add(235, TdxGlyphNames.LowerEdieresis);
  Dictionary.Add(232, TdxGlyphNames.LowerEgrave);
  Dictionary.Add(56, TdxGlyphNames.LowerEight);
  Dictionary.Add(133, TdxGlyphNames.LowerEllipsis);
  Dictionary.Add(151, TdxGlyphNames.LowerEmdash);
  Dictionary.Add(150, TdxGlyphNames.LowerEndash);
  Dictionary.Add(61, TdxGlyphNames.LowerEqual);
  Dictionary.Add(240, TdxGlyphNames.LowerEth);
  Dictionary.Add(33, TdxGlyphNames.LowerExclam);
  Dictionary.Add(161, TdxGlyphNames.LowerExclamdown);
  Dictionary.Add(102, TdxGlyphNames.LowerF);
  Dictionary.Add(53, TdxGlyphNames.LowerFive);
  Dictionary.Add(131, TdxGlyphNames.LowerFlorin);
  Dictionary.Add(52, TdxGlyphNames.LowerFour);
  Dictionary.Add(103, TdxGlyphNames.LowerG);
  Dictionary.Add(223, TdxGlyphNames.LowerGermandbls);
  Dictionary.Add(96, TdxGlyphNames.LowerGrave);
  Dictionary.Add(62, TdxGlyphNames.LowerGreater);
  Dictionary.Add(171, TdxGlyphNames.LowerGuillemotleft);
  Dictionary.Add(187, TdxGlyphNames.LowerGuillemotright);
  Dictionary.Add(139, TdxGlyphNames.LowerGuilsinglleft);
  Dictionary.Add(155, TdxGlyphNames.LowerGuilsinglright);
  Dictionary.Add(104, TdxGlyphNames.LowerH);
  Dictionary.Add(45, TdxGlyphNames.LowerHyphen);
  Dictionary.Add(105, TdxGlyphNames.LowerI);
  Dictionary.Add(237, TdxGlyphNames.LowerIacute);
  Dictionary.Add(238, TdxGlyphNames.LowerIcircumflex);
  Dictionary.Add(239, TdxGlyphNames.LowerIdieresis);
  Dictionary.Add(236, TdxGlyphNames.LowerIgrave);
  Dictionary.Add(106, TdxGlyphNames.LowerJ);
  Dictionary.Add(107, TdxGlyphNames.LowerK);
  Dictionary.Add(108, TdxGlyphNames.LowerL);
  Dictionary.Add(60, TdxGlyphNames.LowerLess);
  Dictionary.Add(172, TdxGlyphNames.LowerLogicalnot);
  Dictionary.Add(109, TdxGlyphNames.LowerM);
  Dictionary.Add(175, TdxGlyphNames.LowerMacron);
  Dictionary.Add(181, TdxGlyphNames.LowerMu);
  Dictionary.Add(215, TdxGlyphNames.LowerMultiply);
  Dictionary.Add(110, TdxGlyphNames.LowerN);
  Dictionary.Add(57, TdxGlyphNames.LowerNine);
  Dictionary.Add(241, TdxGlyphNames.LowerNtilde);
  Dictionary.Add(35, TdxGlyphNames.LowerNumbersign);
  Dictionary.Add(111, TdxGlyphNames.LowerO);
  Dictionary.Add(243, TdxGlyphNames.LowerOacute);
  Dictionary.Add(244, TdxGlyphNames.LowerOcircumflex);
  Dictionary.Add(246, TdxGlyphNames.LowerOdieresis);
  Dictionary.Add(156, TdxGlyphNames.LowerOe);
  Dictionary.Add(242, TdxGlyphNames.LowerOgrave);
  Dictionary.Add(49, TdxGlyphNames.LowerOne);
  Dictionary.Add(189, TdxGlyphNames.LowerOnehalf);
  Dictionary.Add(188, TdxGlyphNames.LowerOnequarter);
  Dictionary.Add(185, TdxGlyphNames.LowerOnesuperior);
  Dictionary.Add(170, TdxGlyphNames.LowerOrdfeminine);
  Dictionary.Add(186, TdxGlyphNames.LowerOrdmasculine);
  Dictionary.Add(248, TdxGlyphNames.LowerOslash);
  Dictionary.Add(245, TdxGlyphNames.LowerOtilde);
  Dictionary.Add(112, TdxGlyphNames.LowerP);
  Dictionary.Add(182, TdxGlyphNames.LowerParagraph);
  Dictionary.Add(40, TdxGlyphNames.LowerParenleft);
  Dictionary.Add(41, TdxGlyphNames.LowerParenright);
  Dictionary.Add(37, TdxGlyphNames.LowerPercent);
  Dictionary.Add(46, TdxGlyphNames.LowerPeriod);
  Dictionary.Add(183, TdxGlyphNames.LowerPeriodcentered);
  Dictionary.Add(137, TdxGlyphNames.LowerPerthousand);
  Dictionary.Add(43, TdxGlyphNames.LowerPlus);
  Dictionary.Add(177, TdxGlyphNames.LowerPlusminus);
  Dictionary.Add(113, TdxGlyphNames.LowerQ);
  Dictionary.Add(63, TdxGlyphNames.LowerQuestion);
  Dictionary.Add(191, TdxGlyphNames.LowerQuestiondown);
  Dictionary.Add(34, TdxGlyphNames.LowerQuotedbl);
  Dictionary.Add(132, TdxGlyphNames.LowerQuotedblbase);
  Dictionary.Add(147, TdxGlyphNames.LowerQuotedblleft);
  Dictionary.Add(148, TdxGlyphNames.LowerQuotedblright);
  Dictionary.Add(145, TdxGlyphNames.LowerQuoteleft);
  Dictionary.Add(146, TdxGlyphNames.LowerQuoteright);
  Dictionary.Add(130, TdxGlyphNames.LowerQuotesinglbase);
  Dictionary.Add(39, TdxGlyphNames.LowerQuotesingle);
  Dictionary.Add(114, TdxGlyphNames.LowerR);
  Dictionary.Add(174, TdxGlyphNames.LowerRegistered);
  Dictionary.Add(115, TdxGlyphNames.LowerS);
  Dictionary.Add(154, TdxGlyphNames.LowerScaron);
  Dictionary.Add(167, TdxGlyphNames.LowerSection);
  Dictionary.Add(59, TdxGlyphNames.LowerSemicolon);
  Dictionary.Add(55, TdxGlyphNames.LowerSeven);
  Dictionary.Add(54, TdxGlyphNames.LowerSix);
  Dictionary.Add(47, TdxGlyphNames.LowerSlash);
  Dictionary.Add(10, TdxGlyphNames.LowerSpace);
  Dictionary.Add(13, TdxGlyphNames.LowerSpace);
  Dictionary.Add(32, TdxGlyphNames.LowerSpace);
  Dictionary.Add(160, TdxGlyphNames.LowerSpace);
  Dictionary.Add(163, TdxGlyphNames.LowerSterling);
  Dictionary.Add(116, TdxGlyphNames.LowerT);
  Dictionary.Add(254, TdxGlyphNames.LowerThorn);
  Dictionary.Add(51, TdxGlyphNames.LowerThree);
  Dictionary.Add(190, TdxGlyphNames.LowerThreequarters);
  Dictionary.Add(179, TdxGlyphNames.LowerThreesuperior);
  Dictionary.Add(152, TdxGlyphNames.LowerTilde);
  Dictionary.Add(153, TdxGlyphNames.LowerTrademark);
  Dictionary.Add(50, TdxGlyphNames.LowerTwo);
  Dictionary.Add(178, TdxGlyphNames.LowerTwosuperior);
  Dictionary.Add(117, TdxGlyphNames.LowerU);
  Dictionary.Add(250, TdxGlyphNames.LowerUacute);
  Dictionary.Add(251, TdxGlyphNames.LowerUcircumflex);
  Dictionary.Add(252, TdxGlyphNames.LowerUdieresis);
  Dictionary.Add(249, TdxGlyphNames.LowerUgrave);
  Dictionary.Add(95, TdxGlyphNames.LowerUnderscore);
  Dictionary.Add(118, TdxGlyphNames.LowerV);
  Dictionary.Add(119, TdxGlyphNames.LowerW);
  Dictionary.Add(120, TdxGlyphNames.LowerX);
  Dictionary.Add(121, TdxGlyphNames.LowerY);
  Dictionary.Add(253, TdxGlyphNames.LowerYacute);
  Dictionary.Add(255, TdxGlyphNames.LowerYdieresis);
  Dictionary.Add(165, TdxGlyphNames.LowerYen);
  Dictionary.Add(122, TdxGlyphNames.LowerZ);
  Dictionary.Add(158, TdxGlyphNames.LowerZcaron);
  Dictionary.Add(48, TdxGlyphNames.LowerZero);
  Dictionary.Add(129, TdxGlyphNames.LowerBullet);
  Dictionary.Add(141, TdxGlyphNames.LowerBullet);
  Dictionary.Add(143, TdxGlyphNames.LowerBullet);
  Dictionary.Add(144, TdxGlyphNames.LowerBullet);
  Dictionary.Add(157, TdxGlyphNames.LowerBullet);
  Dictionary.TrimExcess;
end;

{ TdxFontFileSymbolEncoding }

class function TdxFontFileSymbolEncoding.GetName: string;
begin
  Result := 'Symbol';
end;

procedure TdxFontFileSymbolEncoding.Initialize;
begin
  Dictionary.Add(65, TdxGlyphNames.Alpha);
  Dictionary.Add(66, TdxGlyphNames.Beta);
  Dictionary.Add(67, TdxGlyphNames.Chi);
  Dictionary.Add(68, TdxGlyphNames.Delta);
  Dictionary.Add(69, TdxGlyphNames.Epsilon);
  Dictionary.Add(72, TdxGlyphNames.Eta);
  Dictionary.Add(160, TdxGlyphNames.Euro);
  Dictionary.Add(71, TdxGlyphNames.Gamma);
  Dictionary.Add(193, TdxGlyphNames.Ifraktur);
  Dictionary.Add(73, TdxGlyphNames.Iota);
  Dictionary.Add(75, TdxGlyphNames.Kappa);
  Dictionary.Add(76, TdxGlyphNames.Lambda);
  Dictionary.Add(77, TdxGlyphNames.Mu);
  Dictionary.Add(78, TdxGlyphNames.Nu);
  Dictionary.Add(87, TdxGlyphNames.Omega);
  Dictionary.Add(79, TdxGlyphNames.Omicron);
  Dictionary.Add(70, TdxGlyphNames.Phi);
  Dictionary.Add(80, TdxGlyphNames.Pi);
  Dictionary.Add(89, TdxGlyphNames.Psi);
  Dictionary.Add(194, TdxGlyphNames.Rfraktur);
  Dictionary.Add(82, TdxGlyphNames.Rho);
  Dictionary.Add(83, TdxGlyphNames.Sigma);
  Dictionary.Add(84, TdxGlyphNames.Tau);
  Dictionary.Add(81, TdxGlyphNames.Theta);
  Dictionary.Add(85, TdxGlyphNames.Upsilon);
  Dictionary.Add(161, TdxGlyphNames.Upsilon1);
  Dictionary.Add(88, TdxGlyphNames.Xi);
  Dictionary.Add(90, TdxGlyphNames.Zeta);
  Dictionary.Add(192, TdxGlyphNames.LowerAleph);
  Dictionary.Add(97, TdxGlyphNames.LowerAlpha);
  Dictionary.Add(38, TdxGlyphNames.LowerAmpersand);
  Dictionary.Add(208, TdxGlyphNames.LowerAngle);
  Dictionary.Add(225, TdxGlyphNames.LowerAngleleft);
  Dictionary.Add(241, TdxGlyphNames.LowerAngleright);
  Dictionary.Add(187, TdxGlyphNames.LowerApproxequal);
  Dictionary.Add(171, TdxGlyphNames.LowerArrowboth);
  Dictionary.Add(219, TdxGlyphNames.LowerArrowdblboth);
  Dictionary.Add(223, TdxGlyphNames.LowerArrowdbldown);
  Dictionary.Add(220, TdxGlyphNames.LowerArrowdblleft);
  Dictionary.Add(222, TdxGlyphNames.LowerArrowdblright);
  Dictionary.Add(221, TdxGlyphNames.LowerArrowdblup);
  Dictionary.Add(175, TdxGlyphNames.LowerArrowdown);
  Dictionary.Add(190, TdxGlyphNames.LowerArrowhorizex);
  Dictionary.Add(172, TdxGlyphNames.LowerArrowleft);
  Dictionary.Add(174, TdxGlyphNames.LowerArrowright);
  Dictionary.Add(173, TdxGlyphNames.LowerArrowup);
  Dictionary.Add(189, TdxGlyphNames.LowerArrowvertex);
  Dictionary.Add(42, TdxGlyphNames.LowerAsteriskmath);
  Dictionary.Add(124, TdxGlyphNames.LowerBar);
  Dictionary.Add(98, TdxGlyphNames.LowerBeta);
  Dictionary.Add(123, TdxGlyphNames.LowerBraceleft);
  Dictionary.Add(125, TdxGlyphNames.LowerBraceright);
  Dictionary.Add(236, TdxGlyphNames.LowerBracelefttp);
  Dictionary.Add(237, TdxGlyphNames.LowerBraceleftmid);
  Dictionary.Add(238, TdxGlyphNames.LowerBraceleftbt);
  Dictionary.Add(252, TdxGlyphNames.LowerBracerighttp);
  Dictionary.Add(253, TdxGlyphNames.LowerBracerightmid);
  Dictionary.Add(254, TdxGlyphNames.LowerBracerightbt);
  Dictionary.Add(239, TdxGlyphNames.LowerBraceex);
  Dictionary.Add(91, TdxGlyphNames.LowerBracketleft);
  Dictionary.Add(93, TdxGlyphNames.LowerBracketright);
  Dictionary.Add(233, TdxGlyphNames.LowerBracketlefttp);
  Dictionary.Add(234, TdxGlyphNames.LowerBracketleftex);
  Dictionary.Add(235, TdxGlyphNames.LowerBracketleftbt);
  Dictionary.Add(249, TdxGlyphNames.LowerBracketrighttp);
  Dictionary.Add(250, TdxGlyphNames.LowerBracketrightex);
  Dictionary.Add(251, TdxGlyphNames.LowerBracketrightbt);
  Dictionary.Add(183, TdxGlyphNames.LowerBullet);
  Dictionary.Add(191, TdxGlyphNames.LowerCarriagereturn);
  Dictionary.Add(99, TdxGlyphNames.LowerChi);
  Dictionary.Add(196, TdxGlyphNames.LowerCirclemultiply);
  Dictionary.Add(197, TdxGlyphNames.LowerCircleplus);
  Dictionary.Add(167, TdxGlyphNames.LowerClub);
  Dictionary.Add(58, TdxGlyphNames.LowerColon);
  Dictionary.Add(44, TdxGlyphNames.LowerComma);
  Dictionary.Add(64, TdxGlyphNames.LowerCongruent);
  Dictionary.Add(227, TdxGlyphNames.LowerCopyrightsans);
  Dictionary.Add(211, TdxGlyphNames.LowerCopyrightserif);
  Dictionary.Add(176, TdxGlyphNames.LowerDegree);
  Dictionary.Add(100, TdxGlyphNames.LowerDelta);
  Dictionary.Add(168, TdxGlyphNames.LowerDiamond);
  Dictionary.Add(184, TdxGlyphNames.LowerDivide);
  Dictionary.Add(215, TdxGlyphNames.LowerDotmath);
  Dictionary.Add(56, TdxGlyphNames.LowerEight);
  Dictionary.Add(206, TdxGlyphNames.LowerElement);
  Dictionary.Add(188, TdxGlyphNames.LowerEllipsis);
  Dictionary.Add(198, TdxGlyphNames.LowerEmptyset);
  Dictionary.Add(101, TdxGlyphNames.LowerEpsilon);
  Dictionary.Add(61, TdxGlyphNames.LowerEqual);
  Dictionary.Add(186, TdxGlyphNames.LowerEquivalence);
  Dictionary.Add(104, TdxGlyphNames.LowerEta);
  Dictionary.Add(33, TdxGlyphNames.LowerExclam);
  Dictionary.Add(36, TdxGlyphNames.LowerExistential);
  Dictionary.Add(53, TdxGlyphNames.LowerFive);
  Dictionary.Add(166, TdxGlyphNames.LowerFlorin);
  Dictionary.Add(52, TdxGlyphNames.LowerFour);
  Dictionary.Add(164, TdxGlyphNames.LowerFraction);
  Dictionary.Add(103, TdxGlyphNames.LowerGamma);
  Dictionary.Add(209, TdxGlyphNames.LowerGradient);
  Dictionary.Add(62, TdxGlyphNames.LowerGreater);
  Dictionary.Add(179, TdxGlyphNames.LowerGreaterequal);
  Dictionary.Add(169, TdxGlyphNames.LowerHeart);
  Dictionary.Add(165, TdxGlyphNames.LowerInfinity);
  Dictionary.Add(242, TdxGlyphNames.LowerIntegral);
  Dictionary.Add(243, TdxGlyphNames.LowerIntegraltp);
  Dictionary.Add(244, TdxGlyphNames.LowerIntegralex);
  Dictionary.Add(245, TdxGlyphNames.LowerIntegralbt);
  Dictionary.Add(199, TdxGlyphNames.LowerIntersection);
  Dictionary.Add(105, TdxGlyphNames.LowerIota);
  Dictionary.Add(107, TdxGlyphNames.LowerKappa);
  Dictionary.Add(108, TdxGlyphNames.LowerLambda);
  Dictionary.Add(60, TdxGlyphNames.LowerLess);
  Dictionary.Add(163, TdxGlyphNames.LowerLessequal);
  Dictionary.Add(217, TdxGlyphNames.LowerLogicaland);
  Dictionary.Add(216, TdxGlyphNames.LowerLogicalnot);
  Dictionary.Add(218, TdxGlyphNames.LowerLogicalor);
  Dictionary.Add(224, TdxGlyphNames.LowerLozenge);
  Dictionary.Add(45, TdxGlyphNames.LowerMinus);
  Dictionary.Add(162, TdxGlyphNames.LowerMinute);
  Dictionary.Add(109, TdxGlyphNames.LowerMu);
  Dictionary.Add(180, TdxGlyphNames.LowerMultiply);
  Dictionary.Add(57, TdxGlyphNames.LowerNine);
  Dictionary.Add(207, TdxGlyphNames.LowerNotelement);
  Dictionary.Add(185, TdxGlyphNames.LowerNotequal);
  Dictionary.Add(203, TdxGlyphNames.LowerNotsubset);
  Dictionary.Add(110, TdxGlyphNames.LowerNu);
  Dictionary.Add(35, TdxGlyphNames.LowerNumbersign);
  Dictionary.Add(119, TdxGlyphNames.LowerOmega);
  Dictionary.Add(118, TdxGlyphNames.LowerOmega1);
  Dictionary.Add(111, TdxGlyphNames.LowerOmicron);
  Dictionary.Add(49, TdxGlyphNames.LowerOne);
  Dictionary.Add(40, TdxGlyphNames.LowerParenleft);
  Dictionary.Add(41, TdxGlyphNames.LowerParenright);
  Dictionary.Add(230, TdxGlyphNames.LowerParenlefttp);
  Dictionary.Add(231, TdxGlyphNames.LowerParenleftex);
  Dictionary.Add(232, TdxGlyphNames.LowerParenleftbt);
  Dictionary.Add(246, TdxGlyphNames.LowerParenrighttp);
  Dictionary.Add(247, TdxGlyphNames.LowerParenrightex);
  Dictionary.Add(248, TdxGlyphNames.LowerParenrightbt);
  Dictionary.Add(182, TdxGlyphNames.LowerPartialdiff);
  Dictionary.Add(37, TdxGlyphNames.LowerPercent);
  Dictionary.Add(46, TdxGlyphNames.LowerPeriod);
  Dictionary.Add(94, TdxGlyphNames.LowerPerpendicular);
  Dictionary.Add(102, TdxGlyphNames.LowerPhi);
  Dictionary.Add(106, TdxGlyphNames.LowerPhi1);
  Dictionary.Add(112, TdxGlyphNames.LowerPi);
  Dictionary.Add(43, TdxGlyphNames.LowerPlus);
  Dictionary.Add(177, TdxGlyphNames.LowerPlusminus);
  Dictionary.Add(213, TdxGlyphNames.LowerProduct);
  Dictionary.Add(204, TdxGlyphNames.LowerPropersubset);
  Dictionary.Add(201, TdxGlyphNames.LowerPropersuperset);
  Dictionary.Add(181, TdxGlyphNames.LowerProportional);
  Dictionary.Add(121, TdxGlyphNames.LowerPsi);
  Dictionary.Add(63, TdxGlyphNames.LowerQuestion);
  Dictionary.Add(214, TdxGlyphNames.LowerRadical);
  Dictionary.Add(96, TdxGlyphNames.LowerRadicalex);
  Dictionary.Add(205, TdxGlyphNames.LowerReflexsubset);
  Dictionary.Add(202, TdxGlyphNames.LowerReflexsuperset);
  Dictionary.Add(226, TdxGlyphNames.LowerRegistersans);
  Dictionary.Add(210, TdxGlyphNames.LowerRegisterserif);
  Dictionary.Add(114, TdxGlyphNames.LowerRho);
  Dictionary.Add(178, TdxGlyphNames.LowerSecond);
  Dictionary.Add(59, TdxGlyphNames.LowerSemicolon);
  Dictionary.Add(55, TdxGlyphNames.LowerSeven);
  Dictionary.Add(115, TdxGlyphNames.LowerSigma);
  Dictionary.Add(86, TdxGlyphNames.LowerSigma1);
  Dictionary.Add(126, TdxGlyphNames.LowerSimilar);
  Dictionary.Add(54, TdxGlyphNames.LowerSix);
  Dictionary.Add(47, TdxGlyphNames.LowerSlash);
  Dictionary.Add(32, TdxGlyphNames.LowerSpace);
  Dictionary.Add(170, TdxGlyphNames.LowerSpade);
  Dictionary.Add(39, TdxGlyphNames.LowerSuchthat);
  Dictionary.Add(229, TdxGlyphNames.LowerSummation);
  Dictionary.Add(116, TdxGlyphNames.LowerTau);
  Dictionary.Add(92, TdxGlyphNames.LowerTherefore);
  Dictionary.Add(113, TdxGlyphNames.LowerTheta);
  Dictionary.Add(74, TdxGlyphNames.LowerTheta1);
  Dictionary.Add(51, TdxGlyphNames.LowerThree);
  Dictionary.Add(228, TdxGlyphNames.LowerTrademarksans);
  Dictionary.Add(212, TdxGlyphNames.LowerTrademarkserif);
  Dictionary.Add(50, TdxGlyphNames.LowerTwo);
  Dictionary.Add(95, TdxGlyphNames.LowerUnderscore);
  Dictionary.Add(200, TdxGlyphNames.LowerUnion);
  Dictionary.Add(34, TdxGlyphNames.LowerUniversal);
  Dictionary.Add(117, TdxGlyphNames.LowerUpsilon);
  Dictionary.Add(195, TdxGlyphNames.LowerWeierstrass);
  Dictionary.Add(120, TdxGlyphNames.LowerXi);
  Dictionary.Add(48, TdxGlyphNames.LowerZero);
  Dictionary.Add(122, TdxGlyphNames.LowerZeta);
  Dictionary.TrimExcess;
end;

{ TdxFontFileZapfDingbatsEncoding }

class function TdxFontFileZapfDingbatsEncoding.GetName: string;
begin
  Result := 'ZapfDingbats';
end;

procedure TdxFontFileZapfDingbatsEncoding.Initialize;
begin
  Dictionary.Add(32, TdxGlyphNames.LowerSpace);
  Dictionary.Add(33, TdxGlyphNames.LowerA1);
  Dictionary.Add(34, TdxGlyphNames.LowerA2);
  Dictionary.Add(35, TdxGlyphNames.LowerA202);
  Dictionary.Add(36, TdxGlyphNames.LowerA3);
  Dictionary.Add(37, TdxGlyphNames.LowerA4);
  Dictionary.Add(38, TdxGlyphNames.LowerA5);
  Dictionary.Add(39, TdxGlyphNames.LowerA119);
  Dictionary.Add(40, TdxGlyphNames.LowerA118);
  Dictionary.Add(41, TdxGlyphNames.LowerA117);
  Dictionary.Add(42, TdxGlyphNames.LowerA11);
  Dictionary.Add(43, TdxGlyphNames.LowerA12);
  Dictionary.Add(44, TdxGlyphNames.LowerA13);
  Dictionary.Add(45, TdxGlyphNames.LowerA14);
  Dictionary.Add(46, TdxGlyphNames.LowerA15);
  Dictionary.Add(47, TdxGlyphNames.LowerA16);
  Dictionary.Add(48, TdxGlyphNames.LowerA105);
  Dictionary.Add(49, TdxGlyphNames.LowerA17);
  Dictionary.Add(50, TdxGlyphNames.LowerA18);
  Dictionary.Add(51, TdxGlyphNames.LowerA19);
  Dictionary.Add(52, TdxGlyphNames.LowerA20);
  Dictionary.Add(53, TdxGlyphNames.LowerA21);
  Dictionary.Add(54, TdxGlyphNames.LowerA22);
  Dictionary.Add(55, TdxGlyphNames.LowerA23);
  Dictionary.Add(56, TdxGlyphNames.LowerA24);
  Dictionary.Add(57, TdxGlyphNames.LowerA25);
  Dictionary.Add(58, TdxGlyphNames.LowerA26);
  Dictionary.Add(59, TdxGlyphNames.LowerA27);
  Dictionary.Add(60, TdxGlyphNames.LowerA28);
  Dictionary.Add(61, TdxGlyphNames.LowerA6);
  Dictionary.Add(62, TdxGlyphNames.LowerA7);
  Dictionary.Add(63, TdxGlyphNames.LowerA8);
  Dictionary.Add(64, TdxGlyphNames.LowerA9);
  Dictionary.Add(65, TdxGlyphNames.LowerA10);
  Dictionary.Add(66, TdxGlyphNames.LowerA29);
  Dictionary.Add(67, TdxGlyphNames.LowerA30);
  Dictionary.Add(68, TdxGlyphNames.LowerA31);
  Dictionary.Add(69, TdxGlyphNames.LowerA32);
  Dictionary.Add(70, TdxGlyphNames.LowerA33);
  Dictionary.Add(71, TdxGlyphNames.LowerA34);
  Dictionary.Add(72, TdxGlyphNames.LowerA35);
  Dictionary.Add(73, TdxGlyphNames.LowerA36);
  Dictionary.Add(74, TdxGlyphNames.LowerA37);
  Dictionary.Add(75, TdxGlyphNames.LowerA38);
  Dictionary.Add(76, TdxGlyphNames.LowerA39);
  Dictionary.Add(77, TdxGlyphNames.LowerA40);
  Dictionary.Add(78, TdxGlyphNames.LowerA41);
  Dictionary.Add(79, TdxGlyphNames.LowerA42);
  Dictionary.Add(80, TdxGlyphNames.LowerA43);
  Dictionary.Add(81, TdxGlyphNames.LowerA44);
  Dictionary.Add(82, TdxGlyphNames.LowerA45);
  Dictionary.Add(83, TdxGlyphNames.LowerA46);
  Dictionary.Add(84, TdxGlyphNames.LowerA47);
  Dictionary.Add(85, TdxGlyphNames.LowerA48);
  Dictionary.Add(86, TdxGlyphNames.LowerA49);
  Dictionary.Add(87, TdxGlyphNames.LowerA50);
  Dictionary.Add(88, TdxGlyphNames.LowerA51);
  Dictionary.Add(89, TdxGlyphNames.LowerA52);
  Dictionary.Add(90, TdxGlyphNames.LowerA53);
  Dictionary.Add(91, TdxGlyphNames.LowerA54);
  Dictionary.Add(92, TdxGlyphNames.LowerA55);
  Dictionary.Add(93, TdxGlyphNames.LowerA56);
  Dictionary.Add(94, TdxGlyphNames.LowerA57);
  Dictionary.Add(95, TdxGlyphNames.LowerA58);
  Dictionary.Add(96, TdxGlyphNames.LowerA59);
  Dictionary.Add(97, TdxGlyphNames.LowerA60);
  Dictionary.Add(98, TdxGlyphNames.LowerA61);
  Dictionary.Add(99, TdxGlyphNames.LowerA62);
  Dictionary.Add(100, TdxGlyphNames.LowerA63);
  Dictionary.Add(101, TdxGlyphNames.LowerA64);
  Dictionary.Add(102, TdxGlyphNames.LowerA65);
  Dictionary.Add(103, TdxGlyphNames.LowerA66);
  Dictionary.Add(104, TdxGlyphNames.LowerA67);
  Dictionary.Add(105, TdxGlyphNames.LowerA68);
  Dictionary.Add(106, TdxGlyphNames.LowerA69);
  Dictionary.Add(107, TdxGlyphNames.LowerA70);
  Dictionary.Add(108, TdxGlyphNames.LowerA71);
  Dictionary.Add(109, TdxGlyphNames.LowerA72);
  Dictionary.Add(110, TdxGlyphNames.LowerA73);
  Dictionary.Add(111, TdxGlyphNames.LowerA74);
  Dictionary.Add(112, TdxGlyphNames.LowerA203);
  Dictionary.Add(113, TdxGlyphNames.LowerA75);
  Dictionary.Add(114, TdxGlyphNames.LowerA204);
  Dictionary.Add(115, TdxGlyphNames.LowerA76);
  Dictionary.Add(116, TdxGlyphNames.LowerA77);
  Dictionary.Add(117, TdxGlyphNames.LowerA78);
  Dictionary.Add(118, TdxGlyphNames.LowerA79);
  Dictionary.Add(119, TdxGlyphNames.LowerA81);
  Dictionary.Add(120, TdxGlyphNames.LowerA82);
  Dictionary.Add(121, TdxGlyphNames.LowerA83);
  Dictionary.Add(122, TdxGlyphNames.LowerA84);
  Dictionary.Add(123, TdxGlyphNames.LowerA97);
  Dictionary.Add(124, TdxGlyphNames.LowerA98);
  Dictionary.Add(125, TdxGlyphNames.LowerA99);
  Dictionary.Add(126, TdxGlyphNames.LowerA100);
  Dictionary.Add(161, TdxGlyphNames.LowerA101);
  Dictionary.Add(162, TdxGlyphNames.LowerA102);
  Dictionary.Add(163, TdxGlyphNames.LowerA103);
  Dictionary.Add(164, TdxGlyphNames.LowerA104);
  Dictionary.Add(165, TdxGlyphNames.LowerA106);
  Dictionary.Add(166, TdxGlyphNames.LowerA107);
  Dictionary.Add(167, TdxGlyphNames.LowerA108);
  Dictionary.Add(168, TdxGlyphNames.LowerA112);
  Dictionary.Add(169, TdxGlyphNames.LowerA111);
  Dictionary.Add(170, TdxGlyphNames.LowerA110);
  Dictionary.Add(171, TdxGlyphNames.LowerA109);
  Dictionary.Add(172, TdxGlyphNames.LowerA120);
  Dictionary.Add(173, TdxGlyphNames.LowerA121);
  Dictionary.Add(174, TdxGlyphNames.LowerA122);
  Dictionary.Add(175, TdxGlyphNames.LowerA123);
  Dictionary.Add(176, TdxGlyphNames.LowerA124);
  Dictionary.Add(177, TdxGlyphNames.LowerA125);
  Dictionary.Add(178, TdxGlyphNames.LowerA126);
  Dictionary.Add(179, TdxGlyphNames.LowerA127);
  Dictionary.Add(180, TdxGlyphNames.LowerA128);
  Dictionary.Add(181, TdxGlyphNames.LowerA129);
  Dictionary.Add(182, TdxGlyphNames.LowerA130);
  Dictionary.Add(183, TdxGlyphNames.LowerA131);
  Dictionary.Add(184, TdxGlyphNames.LowerA132);
  Dictionary.Add(185, TdxGlyphNames.LowerA133);
  Dictionary.Add(186, TdxGlyphNames.LowerA134);
  Dictionary.Add(187, TdxGlyphNames.LowerA135);
  Dictionary.Add(188, TdxGlyphNames.LowerA136);
  Dictionary.Add(189, TdxGlyphNames.LowerA137);
  Dictionary.Add(190, TdxGlyphNames.LowerA138);
  Dictionary.Add(191, TdxGlyphNames.LowerA139);
  Dictionary.Add(192, TdxGlyphNames.LowerA140);
  Dictionary.Add(193, TdxGlyphNames.LowerA141);
  Dictionary.Add(194, TdxGlyphNames.LowerA142);
  Dictionary.Add(195, TdxGlyphNames.LowerA143);
  Dictionary.Add(196, TdxGlyphNames.LowerA144);
  Dictionary.Add(197, TdxGlyphNames.LowerA145);
  Dictionary.Add(198, TdxGlyphNames.LowerA146);
  Dictionary.Add(199, TdxGlyphNames.LowerA147);
  Dictionary.Add(200, TdxGlyphNames.LowerA148);
  Dictionary.Add(201, TdxGlyphNames.LowerA149);
  Dictionary.Add(202, TdxGlyphNames.LowerA150);
  Dictionary.Add(203, TdxGlyphNames.LowerA151);
  Dictionary.Add(204, TdxGlyphNames.LowerA152);
  Dictionary.Add(205, TdxGlyphNames.LowerA153);
  Dictionary.Add(206, TdxGlyphNames.LowerA154);
  Dictionary.Add(207, TdxGlyphNames.LowerA155);
  Dictionary.Add(208, TdxGlyphNames.LowerA156);
  Dictionary.Add(209, TdxGlyphNames.LowerA157);
  Dictionary.Add(210, TdxGlyphNames.LowerA158);
  Dictionary.Add(211, TdxGlyphNames.LowerA159);
  Dictionary.Add(212, TdxGlyphNames.LowerA160);
  Dictionary.Add(213, TdxGlyphNames.LowerA161);
  Dictionary.Add(214, TdxGlyphNames.LowerA163);
  Dictionary.Add(215, TdxGlyphNames.LowerA164);
  Dictionary.Add(216, TdxGlyphNames.LowerA196);
  Dictionary.Add(217, TdxGlyphNames.LowerA165);
  Dictionary.Add(218, TdxGlyphNames.LowerA192);
  Dictionary.Add(219, TdxGlyphNames.LowerA166);
  Dictionary.Add(220, TdxGlyphNames.LowerA167);
  Dictionary.Add(221, TdxGlyphNames.LowerA168);
  Dictionary.Add(222, TdxGlyphNames.LowerA169);
  Dictionary.Add(223, TdxGlyphNames.LowerA170);
  Dictionary.Add(224, TdxGlyphNames.LowerA171);
  Dictionary.Add(225, TdxGlyphNames.LowerA172);
  Dictionary.Add(226, TdxGlyphNames.LowerA173);
  Dictionary.Add(227, TdxGlyphNames.LowerA162);
  Dictionary.Add(228, TdxGlyphNames.LowerA174);
  Dictionary.Add(229, TdxGlyphNames.LowerA175);
  Dictionary.Add(230, TdxGlyphNames.LowerA176);
  Dictionary.Add(231, TdxGlyphNames.LowerA177);
  Dictionary.Add(232, TdxGlyphNames.LowerA178);
  Dictionary.Add(233, TdxGlyphNames.LowerA179);
  Dictionary.Add(234, TdxGlyphNames.LowerA193);
  Dictionary.Add(235, TdxGlyphNames.LowerA180);
  Dictionary.Add(236, TdxGlyphNames.LowerA199);
  Dictionary.Add(237, TdxGlyphNames.LowerA181);
  Dictionary.Add(238, TdxGlyphNames.LowerA200);
  Dictionary.Add(239, TdxGlyphNames.LowerA182);
  Dictionary.Add(240, TdxGlyphNames.LowerA201);
  Dictionary.Add(241, TdxGlyphNames.LowerA183);
  Dictionary.Add(242, TdxGlyphNames.LowerA184);
  Dictionary.Add(243, TdxGlyphNames.LowerA197);
  Dictionary.Add(244, TdxGlyphNames.LowerA185);
  Dictionary.Add(245, TdxGlyphNames.LowerA194);
  Dictionary.Add(246, TdxGlyphNames.LowerA198);
  Dictionary.Add(247, TdxGlyphNames.LowerA186);
  Dictionary.Add(249, TdxGlyphNames.LowerA195);
  Dictionary.Add(250, TdxGlyphNames.LowerA187);
  Dictionary.Add(251, TdxGlyphNames.LowerA188);
  Dictionary.Add(252, TdxGlyphNames.LowerA189);
  Dictionary.Add(253, TdxGlyphNames.LowerA190);
  Dictionary.Add(254, TdxGlyphNames.LowerA191);
  Dictionary.TrimExcess;
end;

constructor TdxFontFileCMapSegmentMappingRecord.CreateFromSegmentMapping
  (AEncodingId: TdxFontFileEncodingID;
  AFormatEntry: TdxFontFileCMapSegmentMappingRecord);
begin
  inherited Create(TdxFontFilePlatformID.Microsoft, AEncodingId,
    AFormatEntry.Language);
  FSegCount := AFormatEntry.SegCount;
  FEndCode := AFormatEntry.EndCode;
  FStartCode := AFormatEntry.StartCode;
  FIdDelta := AFormatEntry.IdDelta;
  FIdRangeOffset := AFormatEntry.IdRangeOffset;
  FGlyphIdArray := AFormatEntry.GlyphIdArray;
end;

constructor TdxFontFileCMapSegmentMappingRecord.CreateFromByteEncoding
  (AEncodingId: TdxFontFileEncodingID;
  AFormatEntry: TdxFontFileCMapByteEncodingRecord);
var
  AGlyphIndexes: TList<TEncodingPair>;
  AGlyphInfo: TEncodingPair;

  AGlyphArray: TBytes;
  I, AGlyphIndex, AGlyphCode: SmallInt;
  AGlyphName: string;
  AActualGlyphCode: Word;
  ALength, AIndex: Integer;
  APair: TEncodingPair;

  AComparison: TComparison<TEncodingPair>;
  AMacRomanEncoding: TdxFontFileMacRomanEncoding;
begin
  inherited Create(TdxFontFilePlatformID.Microsoft, AEncodingId,
    AFormatEntry.Language);
  AGlyphArray := AFormatEntry.GlyphIdArray;

  AGlyphIndexes := TList<TEncodingPair>.Create;

  if AEncodingId = TdxFontFileEncodingID.Undefined then
    for I := 0 to Length(AGlyphArray) - 1 do
    begin
      AGlyphIndex := AGlyphArray[I];
      if AGlyphIndex <> 0 then
      begin
        AGlyphInfo.Count := SymbolicEncodingMicrosoftOffset + I;
        AGlyphInfo.Code := AGlyphIndex;
        AGlyphIndexes.Add(AGlyphInfo);
      end;
    end
  else if AFormatEntry.PlatformId = TdxFontFilePlatformID.Macintosh then
  begin
    AMacRomanEncoding := TdxFontFileMacRomanEncoding.Create;
    try
      for I := 0 to Length(AGlyphArray) - 1 do
      begin
        AGlyphIndex := AGlyphArray[I];
        if AGlyphIndex <> 0 then
        begin
          if AMacRomanEncoding.Dictionary.TryGetValue(Byte(I), AGlyphName) and
            dxFontFileUnicodeConverter.FindCode(AGlyphName, AActualGlyphCode)
          then
          begin
            AGlyphInfo.Count := SmallInt(AActualGlyphCode);
            AGlyphInfo.Code := AGlyphIndex;
            AGlyphIndexes.Add(AGlyphInfo);
          end;
        end;
      end;
    finally
      AMacRomanEncoding.Free;
    end;
  end
  else
    for I := 0 to Length(AGlyphArray) - 1 do
    begin
      AGlyphIndex := AGlyphArray[I];
      if AGlyphIndex <> 0 then
      begin
        AGlyphInfo.Count := I;
        AGlyphInfo.Code := AGlyphIndex;
        AGlyphIndexes.Add(AGlyphInfo);
      end;
    end;
  ALength := AGlyphIndexes.Count;
  FSegCount := ALength + 1;
  SetLength(FStartCode, FSegCount);
  SetLength(FEndCode, FSegCount);
  SetLength(FIdDelta, FSegCount);
  SetLength(FIdRangeOffset, FSegCount);
  AIndex := 0;

  AComparison := function(const Left, Right: TEncodingPair): Integer
    begin
      Result := Left.Count - Right.Count;
    end;

  AGlyphIndexes.Sort(TComparer<TEncodingPair>.Construct(AComparison));

  for APair in AGlyphIndexes do
  begin
    AGlyphCode := APair.Count;
    FStartCode[AIndex] := AGlyphCode;
    FEndCode[AIndex] := AGlyphCode;
    FIdDelta[AIndex] := SmallInt((APair.Code - AGlyphCode));
    Inc(AIndex);
  end;
  AGlyphIndexes.Free;
  FStartCode[ALength] := FinalCode;
  FEndCode[ALength] := FinalCode;
  FIdDelta[ALength] := FinalDelta;
  SetLength(FGlyphIdArray, 0);
end;

constructor TdxFontFileCMapSegmentMappingRecord.CreateFromTrimmedMapping
  (AEncodingId: TdxFontFileEncodingID;
  AFormatEntry: TdxFontFileCMapTrimmedMappingRecord);
var
  AFirstCode: SmallInt;
begin
  inherited Create(TdxFontFilePlatformID.Microsoft, AEncodingId,
    AFormatEntry.Language);
  FSegCount := 2;
  AFirstCode := AFormatEntry.FirstCode;
  if (AEncodingId = TdxFontFileEncodingID.Undefined) and
    (AFirstCode + AFormatEntry.EntryCount < 4096) then
    AFirstCode := AFirstCode + SymbolicEncodingMicrosoftOffset;

  SetLength(FEndCode, 2);
  FEndCode[0] := SmallInt(AFirstCode + AFormatEntry.EntryCount - 1);
  FEndCode[1] := FinalCode;

  SetLength(FStartCode, 2);
  FStartCode[0] := AFirstCode;
  FStartCode[1] := FinalCode;

  SetLength(FIdDelta, 2);
  FIdDelta[0] := 0;
  FIdDelta[1] := FinalDelta;

  SetLength(FIdRangeOffset, 2);
  FIdRangeOffset[0] := 4;
  FIdRangeOffset[1] := 0;

  FGlyphIdArray := AFormatEntry.GlyphIdArray;
end;

constructor TdxFontFileCMapSegmentMappingRecord.CreateDefault
  (AEncodingId: TdxFontFileEncodingID);
begin
  Create(TdxFontFilePlatformID.Microsoft, AEncodingId, 0);
  FSegCount := 2;
  SetLength(FEndCode, 2);
  FEndCode[0] := 0;
  FEndCode[1] := FinalCode;

  SetLength(FStartCode, 2);
  FStartCode[0] := 0;
  FStartCode[1] := FinalCode;

  SetLength(FIdDelta, 2);
  FIdDelta[0] := 0;
  FIdDelta[1] := FinalDelta;

  SetLength(FIdRangeOffset, 2);
  FIdRangeOffset[0] := 4;
  FIdRangeOffset[1] := 0;

  SetLength(FGlyphIdArray, 1);
  FGlyphIdArray[0] := 0
end;

destructor TdxFontFileCMapSegmentMappingRecord.Destroy;
begin
  FreeAndNil(FGlyphRanges);
  inherited Destroy;
end;

function TdxFontFileCMapSegmentMappingRecord.GetSegmentsLength: Integer;
begin
  Result := 10 + FSegCount * 8;
end;

function TdxFontFileCMapSegmentMappingRecord.IsUndefinedEncoding: Boolean;
begin
  Result := (PlatformId = TdxFontFilePlatformID.Microsoft) and
    (EncodingId = TdxFontFileEncodingID.Undefined);
end;

function TdxFontFileCMapSegmentMappingRecord.GetFormat: TdxFontFileCMapFormatID;
begin
  Result := TdxFontFileCMapFormatID.SegmentMapping;
end;

function TdxFontFileCMapSegmentMappingRecord.GetSize: Integer;
begin
  Result := HeaderLength + SegmentsLength + Length(FGlyphIdArray) * 2;
end;

procedure TdxFontFileCMapSegmentMappingRecord.UpdateEncoding
  (var AEncoding: TSmallIntDynArray);
var
  AIsUndefinedEncoding: Boolean;
  AGlyphIdArrayLength, I, AIndex, APosition: Integer;
  AStart, AEnd, AOffset: Word;
  ADelta, AGlyphIndex: SmallInt;
begin
  AIsUndefinedEncoding := IsUndefinedEncoding;
  AGlyphIdArrayLength := Length(FGlyphIdArray);
  for I := 0 to FSegCount - 1 do
  begin
    AStart := FStartCode[I];
    ADelta := FIdDelta[I];
    AEnd := FEndCode[I];
    if AStart >= 256 then
    begin
      if AIsUndefinedEncoding and (AStart >= SymbolicEncodingMicrosoftOffset)
      then
      begin
        Dec(AStart, SymbolicEncodingMicrosoftOffset);
        Dec(AEnd, SymbolicEncodingMicrosoftOffset);
        ADelta := ADelta + SymbolicEncodingMicrosoftOffset;
      end;
      if AStart > 256 then
        Break;
    end;
    if AEnd >= 256 then
      AEnd := 255;
    AOffset := FIdRangeOffset[I];
    if AOffset = 0 then
    begin
      AGlyphIndex := AStart + ADelta;
      for AIndex := AStart to AEnd do
      begin
        UpdateEncodingValue(AEncoding, AIndex, AGlyphIndex);
        Inc(AGlyphIndex);
      end;
    end
    else
    begin
      APosition := AOffset div 2 - FSegCount + I;
      for AIndex := AStart to AEnd do
      begin
        if (APosition >= 0) and (APosition < AGlyphIdArrayLength) then
          UpdateEncodingValue(AEncoding, AIndex, FGlyphIdArray[APosition]);
        Inc(APosition);
      end;
    end;
  end;
end;

function TdxFontFileCMapSegmentMappingRecord.IsSymbolEncoding: Boolean;
begin
  Result := (PlatformId = TdxFontFilePlatformID.Microsoft) and
    (EncodingId = TdxFontFileEncodingID.Undefined);
end;

function TdxFontFileCMapSegmentMappingRecord.MapCode(ACharacterCode
  : Char): Integer;
var
  AIsSymbolEncoding, ANeedSymbolEncodingRemapping: Boolean;
  I, AIndex, APosition: Integer;
  ADelta, AOffset: SmallInt;
  ACharacter: Integer;
  AEnd, AStart: USHORT;
begin
  AIsSymbolEncoding := IsSymbolEncoding;
  ACharacter := Integer(ACharacterCode);
  for I := 0 to FSegCount - 1 do
  begin
    AEnd := USHORT(FEndCode[I]);
    AStart := USHORT(FStartCode[I]);
    ADelta := FIdDelta[I];
    ANeedSymbolEncodingRemapping := AIsSymbolEncoding and
      (AStart >= SymbolicEncodingMicrosoftOffset) and
      ((ACharacter < $F000) or (ACharacter > $F0FF));
    if ANeedSymbolEncodingRemapping then
    begin
      Dec(AStart, SymbolicEncodingMicrosoftOffset);
      Dec(AEnd, SymbolicEncodingMicrosoftOffset);
    end;
    if ACharacter <= AEnd then
    begin
      AIndex := ACharacter - AStart;
      if AIndex >= 0 then
      begin
        AOffset := FIdRangeOffset[I];
        if AOffset = 0 then
        begin
          if ANeedSymbolEncodingRemapping then
            ADelta := ADelta + SymbolicEncodingMicrosoftOffset;
          if ACharacter = NotdefGlyphIndex then
            Exit(ACharacter)
          else
            Exit((ACharacter + ADelta) mod 65536);
        end;
        APosition := AOffset div 2 + AIndex + I - FSegCount;
        if (APosition < 0) or (APosition >= Length(FGlyphIdArray)) then
          Exit(NotdefGlyphIndex);
        Exit(FGlyphIdArray[APosition] + ADelta);
      end;
    end;
  end;
  Result := NotdefGlyphIndex;
end;

function TdxFontFileCMapSegmentMappingRecord.CreateGlyphMapping
  (AGlyphNames: TStringList): TdxPDFWordDictionary;
var
  AGlyphNamesCount, AGlyphIdCount, I, AOffset, AStartIndex, AIdIndex: Integer;
  AStart, AEnd, ARangeOffset, ACode, AGlyphID: SmallInt;
begin
  Result := TdxPDFWordDictionary.Create;
  if (AGlyphNames = nil) or (AGlyphNames.Count = 0) then
    Exit;
  AGlyphNamesCount := AGlyphNames.Count;
  AGlyphIdCount := Length(FGlyphIdArray);

  AOffset := FSegCount;
  for I := 0 to FSegCount - 1 do
  begin
    AStart := FStartCode[I];
    AEnd := FEndCode[I];
    if AStart <> -1 then
    begin
      ARangeOffset := FIdRangeOffset[I];
      if ARangeOffset > 0 then
      begin
        AStartIndex := ARangeOffset div 2 - AStart - AOffset;
        AIdIndex := AStart + AStartIndex;

        ACode := AStart;
        while (ACode <= AEnd) and (AIdIndex < AGlyphIdCount) do
        begin
          AGlyphID := FGlyphIdArray[AIdIndex];
          if (AGlyphID < 0) or (AGlyphID >= AGlyphNamesCount) then
            Exit(nil);
          Result.AddOrSetValue(AGlyphNames[AGlyphID], Word(ACode));
          Inc(ACode);
          Inc(AIdIndex);
        end;
      end
      else
      begin
        AGlyphID := AStart + FIdDelta[I];

        ACode := AStart;
        while ACode <= AEnd do
        begin
          if AGlyphID >= AGlyphNamesCount then
          begin
            Result.Free;
            Exit(nil);
          end;
          Result.AddOrSetValue(AGlyphNames[AGlyphID], ACode);
          Inc(ACode);
          Inc(AGlyphID);
        end;
      end;
    end;
    Dec(AOffset);
  end;
end;

function TdxFontFileCMapSegmentMappingRecord.GetGlyphRanges
  : TList<TdxFontFileCMapGlyphRange>;
var
  I: Integer;
  AEnd: SmallInt;
  Var1: TdxFontFileCMapGlyphRange;
begin
  if FGlyphRanges = nil then
  begin
    FGlyphRanges := TList<TdxFontFileCMapGlyphRange>.Create;
    for I := 0 to FSegCount - 1 do
    begin
      AEnd := FEndCode[I];
      if AEnd = -1 then
        Break;
      Var1.StartValue :=
        FStartCode[I];
      Var1.EndValue := AEnd;
      FGlyphRanges.Add(Var1);
    end;
  end;
  Result := FGlyphRanges;
end;

function TdxFontFileCMapSegmentMappingRecord.Validate: Boolean;
var
  AComparison: TComparison<TdxFontFileCMapRow>;

  AMaxIndex, AIndex, I: Integer;
  APrevious, AValue: Word;
  AList: TList<TdxFontFileCMapRow>;
  ARow: TdxFontFileCMapRow;
begin
  if FSegCount <= 0 then
    Exit(False);
  AMaxIndex := FSegCount - 1;
  APrevious := Word(FEndCode[0]);

  AIndex := 1;
  while AIndex < AMaxIndex do
  begin
    AValue := Word(FEndCode[AIndex]);
    if AValue < APrevious then
    begin
      AList := TList<TdxFontFileCMapRow>.Create;
      try
        AList.Capacity := AMaxIndex;
        for I := 0 to AMaxIndex - 1 do
        begin
          ARow.EndCode := FEndCode[I];
          ARow.StartCode := FStartCode[I];
          ARow.IdDelta := FIdDelta[I];
          ARow.IdRangeOffset := FIdRangeOffset[I];
          AList.Add(ARow);
        end;
        AComparison :=
            function(const ALeft, ARigth: TdxFontFileCMapRow): Integer
          var
            AResult: Integer;
          begin
            AResult := Word(ALeft.EndCode) - Word(ARigth.EndCode);
            if AResult = 0 then
            begin
              AResult := Word(ALeft.StartCode) - Word(ARigth.StartCode);
              if AResult = 0 then
                AResult := Word(ALeft.IdDelta) - Word(ARigth.IdDelta);
            end;
            Result := AResult;
          end;

        AList.Sort(TComparer<TdxFontFileCMapRow>.Construct(AComparison));
        for I := 0 to AMaxIndex - 1 do
        begin
          ARow := AList[I];
          FEndCode[I] := ARow.EndCode;
          FStartCode[I] := ARow.StartCode;
          FIdDelta[I] := ARow.IdDelta;
          FIdRangeOffset[I] := ARow.IdRangeOffset;
        end;
      finally
        AList.Free;
      end;
      Exit(True);
    end;
    APrevious := AValue;
    Inc(AIndex);
  end;
  Result := False;
end;

function TdxFontFileCMapSegmentMappingRecord.ReadSegmentsArray
  (ACMapStream: TdxFontFileStream): TSmallIntDynArray;
var
  AResult: TSmallIntDynArray;
  I: Integer;
begin
  SetLength(AResult, FSegCount);
  for I := 0 to FSegCount - 1 do
    AResult[I] := ACMapStream.ReadShort;
  Result := AResult;
end;

class function TdxFontFileCMapSegmentMappingRecord.TMap.Create(ACharCode: Char;
  AGID: SmallInt): TMap;
begin
  Result.CharCode := ACharCode;
  Result.GID := AGID;
end;

{ TdxFontFileCMapCustomFormatRecord }

constructor TdxFontFileCMapCustomFormatRecord.Create
  (APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID);
begin
  inherited Create;
  FPlatformId := APlatformId;
  FEncodingId := AEncodingId;
end;

constructor TdxFontFileCMapCustomFormatRecord.Create
  (APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID;
  AStream: TdxFontFileStream);
begin
  Create(APlatformId, AEncodingId);
end;

function TdxFontFileCMapCustomFormatRecord.MapCode(ACharacter: Char): Integer;
begin
  Result := Integer(ACharacter);
end;

procedure TdxFontFileCMapCustomFormatRecord.UpdateEncoding
  (var AEncoding: TSmallIntDynArray);
begin
  TdxPDFUtils.RaiseTestException;
end;

procedure TdxFontFileCMapCustomFormatRecord.Write(AStream: TdxFontFileStream);
begin
  AStream.WriteShort(SmallInt(Format));
end;

class procedure TdxFontFileCMapCustomFormatRecord.UpdateEncodingValue
  (const AEncoding: TSmallIntDynArray; AIndex, AValue: SmallInt);
var
  AExistingValue: SmallInt;
begin
  if AValue <> 0 then
  begin
    AExistingValue := AEncoding[AIndex];
    if AExistingValue = 0 then
      AEncoding[AIndex] := AValue;
  end;
end;

{ TdxFontFileCMapShortFormatRecord }

constructor TdxFontFileCMapShortFormatRecord.Create
  (APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID;
  ALanguage: SmallInt);
begin
  Create(APlatformId, AEncodingId);
end;

constructor TdxFontFileCMapShortFormatRecord.Create
  (APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID;
  AStream: TdxFontFileStream);
begin
  inherited Create(APlatformId, AEncodingId, AStream);
  FBodyLength := Max(AStream.ReadUshort - HeaderLength, 0);
  Language := SmallInt(AStream.ReadShort);
end;

procedure TdxFontFileCMapShortFormatRecord.
  Write(ATableStream: TdxFontFileStream);
begin
  inherited Write(ATableStream);
  ATableStream.WriteShort(SmallInt(Size));
  ATableStream.WriteShort(Language);
end;

function TdxFontFileCMapShortFormatRecord.GetSize: Integer;
begin
  Result := HeaderLength + FBodyLength;
end;

{ TdxFontFileCMapTrimmedMappingRecord }

constructor TdxFontFileCMapTrimmedMappingRecord.Create
  (APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID;
  AStream: TdxFontFileStream);
begin
  inherited Create(APlatformId, AEncodingId, AStream);
  FFirstCode := AStream.ReadShort;
  FEntryCount := AStream.ReadShort;
  FGlyphIdArray := AStream.ReadShortArray(FEntryCount);
end;

function TdxFontFileCMapTrimmedMappingRecord.MapCode(ACharacter: Char): Integer;
var
  ACode: Integer;
begin
  ACode := Integer(ACharacter) - FFirstCode;
  if (ACode >= 0) and (ACode < FEntryCount) then
    Result := Word(FGlyphIdArray[ACode])
  else
    Result := MissingGlyphIndex;
end;

procedure TdxFontFileCMapTrimmedMappingRecord.
  Write(ATableStream: TdxFontFileStream);
begin
  inherited Write(ATableStream);
  ATableStream.WriteShort(FFirstCode);
  ATableStream.WriteShort(FEntryCount);
  ATableStream.WriteShortArray(FGlyphIdArray);
end;

function TdxFontFileCMapTrimmedMappingRecord.GetFormat: TdxFontFileCMapFormatID;
begin
  Result := TdxFontFileCMapFormatID.TrimmedMapping;
end;

function TdxFontFileCMapTrimmedMappingRecord.GetSize: Integer;
begin
  Result := HeaderLength + 4 + Length(FGlyphIdArray) * 2;
end;

procedure TdxFontFileCMapTrimmedMappingRecord.UpdateEncoding
  (var AEncoding: TSmallIntDynArray);
var
  I, ACode: SmallInt;
begin
  ACode := FFirstCode;
  for I := 0 to FEntryCount - 1 do
  begin
    if ACode >= 256 then
      Break;
    UpdateEncodingValue(AEncoding, ACode, FGlyphIdArray[I]);
    Inc(ACode);
  end;
end;

{ TdxFontFileBinaryTable }

constructor TdxFontFileBinaryTable.Create;
begin
  inherited Create;
  FDataStream := TdxFontFileStream.Create;
end;

constructor TdxFontFileBinaryTable.Create(const AData: TBytes);
begin
  Create;
  FDataStream.WriteArray(AData);
  FDataStream.Position := 0;
  Name := Tag;
end;

destructor TdxFontFileBinaryTable.Destroy;
begin
  FreeAndNil(FDataStream);
  inherited Destroy;
end;

class function TdxFontFileBinaryTable.Tag: string;
begin
  Result := '';
end;

function TdxFontFileBinaryTable.AlignedTableData: TBytes;
begin
  Result := FDataStream.ToAlignedArray;
end;

function TdxFontFileBinaryTable.GetDataSize: Integer;
begin
  Result := FDataStream.Size;
end;

function TdxFontFileBinaryTable.GetTableData: TBytes;
begin
  Result := FDataStream.Data;
end;

procedure TdxFontFileBinaryTable.CalculateCheckSum(var ACheckSum: Integer);
var
  I, AIndex, AElement, ACount: Integer;
  AData: TBytes;
begin
  AData := AlignedTableData;
  AIndex := 0;
  ACheckSum := 0;
  ACount := Length(AData) div 4;
  for I := 0 to ACount - 1 do
  begin
    AElement := AData[AIndex] shl 24;
    AElement := AElement + AData[AIndex + 1] shl 16;
    AElement := AElement + AData[AIndex + 2] shl 8;
    AElement := AElement + AData[AIndex + 3];
    Inc(ACheckSum, AElement);
    Inc(AIndex, 4);
  end;
end;

function TdxFontFileBinaryTable.Write(AStream: TdxFontFileStream;
  AOffset: Integer): Integer;
var
  ALength, AFactor, AAdditionalLength, ACheckSum: Integer;
begin
  ApplyChanges;
  ALength := DataSize;
  if ALength = 0 then
  begin
    DataStream.WriteInt(0);
    ALength := 4;
  end;
  AFactor := ALength mod 4;
  AAdditionalLength := 0;
  if AFactor <> 0 then
  begin
    AAdditionalLength := 4 - AFactor;
    DataStream.Position := ALength;
    DataStream.WriteEmptyArray(AAdditionalLength);
  end;
  AStream.WriteString(Name);
  CalculateCheckSum(ACheckSum);
  AStream.WriteInt(ACheckSum);
  AStream.WriteInt(AOffset);
  AStream.WriteInt(ALength);
  Result := ALength + AAdditionalLength;
end;

procedure TdxFontFileBinaryTable.DoApplyChanges;
begin
  // do nothing
end;

procedure TdxFontFileBinaryTable.ApplyChanges;
begin
  if FNeedWrite then
  begin
    RecreateStream;
    DoApplyChanges;
  end;
end;

procedure TdxFontFileBinaryTable.Changed(AIsChanged: Boolean = True);
begin
  FNeedWrite := AIsChanged;
end;

procedure TdxFontFileBinaryTable.RecreateStream;
begin
  FDataStream.Free;
  FDataStream := TdxFontFileStream.Create;
end;

{ TdxFontFileLocaTable }

class function TdxFontFileLocaTable.Tag: string;
begin
  Result := 'loca';
end;

procedure TdxFontFileLocaTable.SetGlyphOffsets(const AValue: TIntegerDynArray);
begin
  FGlyphOffsets := AValue;
  Changed;
end;

procedure TdxFontFileLocaTable.ReadOffsets(AFontFile: TdxFontFile);
var
  AOffsetCount, AMaxpCount, I: Integer;
begin
  FIsShortFormat := (AFontFile.HeadTable <> nil) and
    (AFontFile.HeadTable.IndexToLocFormat = TdxFontFileIndexToLocFormat.Short);
  AOffsetCount := Length(Data) div IfThen(FIsShortFormat, 2, 4);
  if AOffsetCount > 1 then
  begin
    if AFontFile.MaxpTable <> nil then
    begin
      AMaxpCount := AFontFile.MaxpTable.NumGlyphs + 1;
      if AOffsetCount > AMaxpCount then
        AOffsetCount := AMaxpCount
      else if AOffsetCount < AMaxpCount then
        AFontFile.MaxpTable.NumGlyphs := AOffsetCount - 1;
    end;
    DataStream.Position := 0;
    SetLength(FGlyphOffsets, AOffsetCount);
    if FIsShortFormat then
      for I := 0 to AOffsetCount - 1 do
        FGlyphOffsets[I] := DataStream.ReadUshort * 2
    else
      for I := 0 to AOffsetCount - 1 do
        FGlyphOffsets[I] := DataStream.ReadInt;
  end
  else
    SetLength(FGlyphOffsets, 0);
end;

procedure TdxFontFileLocaTable.DoApplyChanges;
var
  ACount, I: Integer;
begin
  inherited DoApplyChanges;
  ACount := Length(FGlyphOffsets);
  if FIsShortFormat then
    for I := 0 to ACount - 1 do
      DataStream.WriteShort(FGlyphOffsets[I] div 2)
  else
    for I := 0 to ACount - 1 do
      DataStream.WriteInt(FGlyphOffsets[I]);
end;

{ TdxFontFileCFFTable }

class function TdxFontFileCFFTable.Tag: string;
begin
  Result := 'CFF ';
end;

function TdxFontFileCMapTable.Validate(ASkipEncodingValidation: Boolean;
  AIsSymbolic: Boolean): TdxFontFileCMapSegmentMappingRecord;
var
  AEncodingId: TdxFontFileEncodingID;
  ASegmentMappingFormatEntry, AUncompatibleSegmentMappingFormatEntry
    : TdxFontFileCMapSegmentMappingRecord;
  AActualTrimmedMappingFormatEntry: TdxFontFileCMapTrimmedMappingRecord;
  AActualByteEncodingFormatEntry: TdxFontFileCMapByteEncodingRecord;
  AFormatEntry: TdxFontFileCMapCustomFormatRecord;
begin
  if AIsSymbolic then
    AEncodingId := TdxFontFileEncodingID.Undefined
  else
    AEncodingId := TdxFontFileEncodingID.UGL;

  AActualTrimmedMappingFormatEntry := nil;
  AActualByteEncodingFormatEntry := nil;
  AUncompatibleSegmentMappingFormatEntry := nil;

  for AFormatEntry in FCMapTables do
  begin
    if AFormatEntry is TdxFontFileCMapSegmentMappingRecord then
    begin
      ASegmentMappingFormatEntry :=
        AFormatEntry as TdxFontFileCMapSegmentMappingRecord;
      if (ASegmentMappingFormatEntry <> nil) then
      begin
        if ASegmentMappingFormatEntry.PlatformId = TdxFontFilePlatformID.Microsoft
        then
          if ((ASkipEncodingValidation) or
            (ASegmentMappingFormatEntry.EncodingId = AEncodingId)) then
          begin
            Changed(ASegmentMappingFormatEntry.Validate);
            Exit(ASegmentMappingFormatEntry);
          end;
        AUncompatibleSegmentMappingFormatEntry := ASegmentMappingFormatEntry;
      end;
    end;
    if AFormatEntry is TdxFontFileCMapTrimmedMappingRecord then
      AActualTrimmedMappingFormatEntry := TdxFontFileCMapTrimmedMappingRecord
        (AFormatEntry);

    if AFormatEntry is TdxFontFileCMapByteEncodingRecord then
      AActualByteEncodingFormatEntry := TdxFontFileCMapByteEncodingRecord
        (AFormatEntry);
  end;

  if AActualTrimmedMappingFormatEntry <> nil then
    ASegmentMappingFormatEntry := TdxFontFileCMapSegmentMappingRecord.
      CreateFromTrimmedMapping(AEncodingId, AActualTrimmedMappingFormatEntry)
  else if AActualByteEncodingFormatEntry <> nil then
    ASegmentMappingFormatEntry := TdxFontFileCMapSegmentMappingRecord.
      CreateFromByteEncoding(AEncodingId, AActualByteEncodingFormatEntry)
  else if AUncompatibleSegmentMappingFormatEntry <> nil then
    ASegmentMappingFormatEntry := TdxFontFileCMapSegmentMappingRecord.
      CreateFromSegmentMapping(AEncodingId,
      AUncompatibleSegmentMappingFormatEntry)
  else
    ASegmentMappingFormatEntry := TdxFontFileCMapSegmentMappingRecord.
      CreateDefault(AEncodingId);
  FCMapTables.Clear;
  FCMapTables.Add(ASegmentMappingFormatEntry);
  Changed;
  Result := ASegmentMappingFormatEntry;
end;

function TdxFontFileCMapTable.MapCodes(const AStr: string): TIntegerDynArray;
var
  ACount, I: Integer;
  ACodes: TIntegerDynArray;
begin
  if AStr = '' then
    Exit;
  ACount := Length(AStr);
  SetLength(ACodes, ACount);
  for I := 0 to ACount - 1 do
    ACodes[I] := MapCode(AStr[I]);
  Result := ACodes;
end;

function TdxFontFileCMapTable.MapCode(ACharacter: Char): Integer;
var
  AGlyph: Integer;
  ACMap: TdxFontFileCMapCustomFormatRecord;
begin
  AGlyph := 0;
  if FMappedGlyphCache.TryGetValue(Integer(ACharacter), AGlyph) then
    Exit(AGlyph);
  for ACMap in FCMapTables do
  begin
    AGlyph := ACMap.MapCode(ACharacter);
    if AGlyph <> TdxFontFileCMapCustomFormatRecord.MissingGlyphIndex then
      Break;
  end;
  FMappedGlyphCache.Add(Integer(ACharacter), AGlyph);
  Result := AGlyph;
end;

procedure TdxFontFileCMapTable.PopulateEncoding(var AEncoding
  : TSmallIntDynArray);
begin
  SetLength(AEncoding, 256);
  UpdateEncoding(AEncoding,
    function(ATable: TdxFontFileCMapCustomFormatRecord): Boolean
    begin
      Result := (ATable.EncodingId = TdxFontFileEncodingID.Undefined) and
        (ATable is TdxFontFileCMapSegmentMappingRecord);
    end);

  UpdateEncoding(AEncoding,
    function(ATable: TdxFontFileCMapCustomFormatRecord): Boolean
    begin
      Result := (ATable.EncodingId = TdxFontFileEncodingID.Undefined) and
        not(ATable is TdxFontFileCMapSegmentMappingRecord);
    end);

  UpdateEncoding(AEncoding,
    function(ATable: TdxFontFileCMapCustomFormatRecord): Boolean
    begin
      Result := ATable.EncodingId = TdxFontFileEncodingID.Undefined;
    end);
end;

procedure TdxFontFileCMapTable.DoApplyChanges;
var
  AOffset: Integer;
  ACMapTableCount: SmallInt;
  ACMapRecord: TdxFontFileCMapCustomFormatRecord;
begin
  inherited DoApplyChanges;
  DataStream.WriteShort(FVersion);
  ACMapTableCount := SmallInt(FCMapTables.Count);
  DataStream.WriteShort(ACMapTableCount);
  AOffset := 4 + ACMapTableCount * 8;
  for ACMapRecord in FCMapTables do
  begin
    DataStream.WriteShort(SmallInt(ACMapRecord.PlatformId));
    DataStream.WriteShort(SmallInt(ACMapRecord.EncodingId));
    DataStream.WriteInt(AOffset);
    Inc(AOffset, ACMapRecord.Size);
  end;
  for ACMapRecord in FCMapTables do
    ACMapRecord.Write(DataStream);
end;

function TdxFontFileCMapTable.CreateRecord(APlatformId: TdxFontFilePlatformID;
AEncodingId: TdxFontFileEncodingID; AFormat: TdxFontFileCMapFormatID;
AStream: TdxFontFileStream): TdxFontFileCMapCustomFormatRecord;
begin
  Result := nil;
  case AFormat of
    TdxFontFileCMapFormatID.ByteEncoding:
      Result := TdxFontFileCMapByteEncodingRecord.Create(APlatformId,
        AEncodingId, AStream);
    TdxFontFileCMapFormatID.HighByteMappingThrough:
      Result := TdxFontFileCMapHighByteMappingThroughRecord.Create(APlatformId,
        AEncodingId, AStream);
    TdxFontFileCMapFormatID.SegmentMapping:
      Result := TdxFontFileCMapSegmentMappingRecord.Create(APlatformId,
        AEncodingId, AStream);
    TdxFontFileCMapFormatID.TrimmedMapping:
      Result := TdxFontFileCMapTrimmedMappingRecord.Create(APlatformId,
        AEncodingId, AStream);
    TdxFontFileCMapFormatID.MixedCoverage:
      Result := TdxFontFileCMapMixedCoverageRecord.Create(APlatformId,
        AEncodingId, AStream);
    TdxFontFileCMapFormatID.TrimmedArray:
      Result := TdxFontFileCMapTrimmedArrayRecord.Create(APlatformId,
        AEncodingId, AStream);
    TdxFontFileCMapFormatID.SegmentedCoverage:
      Result := TdxFontFileCMapSegmentedCoverageRecord.Create(APlatformId,
        AEncodingId, AStream);
    TdxFontFileCMapFormatID.ManyToOneRangeMapping:
      Result := TdxFontFileCMapManyToOneRangeMappingRecord.Create(APlatformId,
        AEncodingId, AStream);
    TdxFontFileCMapFormatID.UnicodeVariationSequences:
      Result := TdxFontFileCMapUnicodeVariationSequenciesRecord.Create
        (APlatformId, AEncodingId, AStream);
  else
    TdxPDFUtils.RaiseTestException('Error creating CMap record');
  end;
end;

procedure TdxFontFileCMapTable.UpdateEncoding(var AEncoding: TSmallIntDynArray;
AChooseTableFunc: TChooseTableFunc);
var
  ATable: TdxFontFileCMapCustomFormatRecord;
begin
  for ATable in FCMapTables do
    if AChooseTableFunc(ATable) then
      ATable.UpdateEncoding(AEncoding);
end;

{ TdxFontFileNameRecord }

class function TdxFontFileNameRecord.Create(AStream: TdxFontFileStream;
ADataOffset: Integer): TdxFontFileNameRecord;
var
  ALength, AOffset: Integer;
  APosition, ASavedPosition: Int64;
begin
  Result.FPlatformId := TdxFontFilePlatformID(AStream.ReadUshort);
  Result.FEncodingId := TdxFontFileEncodingID(AStream.ReadUshort);
  Result.FLanguageID := TdxFontFileLanguageID(AStream.ReadUshort);
  Result.FNameID := TdxFontFileNameID(AStream.ReadUshort);
  ALength := AStream.ReadUshort;
  AOffset := AStream.ReadUshort;
  APosition := ADataOffset + AOffset;
  if APosition + ALength <= AStream.Size then
  begin
    ASavedPosition := AStream.Position;
    AStream.Position := APosition;
    try
      Result.FNameBytes := AStream.ReadArray(ALength);
      if Result.PlatformId = TdxFontFilePlatformID.Microsoft then
        Result.FName := TdxPDFUtils.ConvertToStr(Result.FNameBytes,
          Length(Result.FNameBytes))
      else
        Result.FName := TdxPDFUtils.ConvertToASCIIString(Result.FNameBytes);
    finally
      AStream.Position := ASavedPosition;
    end;
  end;
end;

class function TdxFontFileNameRecord.Create(APlatformId: TdxFontFilePlatformID;
ALanguageID: TdxFontFileLanguageID; ANameID: TdxFontFileNameID;
AEncodingId: TdxFontFileEncodingID; const ANameBytes: TBytes)
  : TdxFontFileNameRecord;
begin
  Result.FPlatformId := APlatformId;
  Result.FLanguageID := ALanguageID;
  Result.FNameID := ANameID;
  Result.FEncodingId := AEncodingId;
  Result.FNameBytes := ANameBytes;
  if APlatformId = TdxFontFilePlatformID.Microsoft then
    Result.FName := TdxPDFUtils.ConvertToStr(Result.FNameBytes,
      Length(Result.FNameBytes))
  else
    Result.FName := TdxPDFUtils.ConvertToASCIIString(Result.FNameBytes);
end;

{ TdxFontFileNameTable }

constructor TdxFontFileNameTable.Create(const AData: TBytes);
var
  I: Integer;
  ACount, AOffset: SmallInt;
begin
  inherited Create(AData);
  FNamingTable := TList<TdxFontFileNameRecord>.Create;
  FFamilyName := '';
  FMacFamilyName := '';
  FPostScriptName := '';
  if DataSize > 6 then
  begin
    DataStream.ReadShort;
    ACount := DataStream.ReadShort;
    AOffset := DataStream.ReadShort;
    for I := 0 to ACount - 1 do
      FNamingTable.Add(TdxFontFileNameRecord.Create(DataStream, AOffset));
  end;
end;

constructor TdxFontFileNameTable.Create(ACMapEntry: TdxFontFileCMapTable;
const AFontName: string);
begin
  Create(nil);
  AddName(ACMapEntry, AFontName);
end;

destructor TdxFontFileNameTable.Destroy;
begin
  FreeAndNil(FNamingTable);
  inherited Destroy;
end;

function TdxFontFileNameTable.GetFamilyName: string;
begin
  if FFamilyName = '' then
    FFamilyName := FindName(TdxFontFilePlatformID.Microsoft,
      TdxFontFileEncodingID.UGL, TdxFontFileLanguageID.EnglishUnitedStates,
      TdxFontFileNameID.FontFamily);
  Result := FFamilyName;
end;

function TdxFontFileNameTable.GetMacFamilyName: string;
begin
  if FMacFamilyName = '' then
    FMacFamilyName := FindName(TdxFontFilePlatformID.Macintosh,
      TdxFontFileEncodingID.Undefined, TdxFontFileLanguageID.English,
      TdxFontFileNameID.FontFamily);
  Result := FMacFamilyName;
end;

function TdxFontFileNameTable.GetPostScriptName: string;
begin
  if FPostScriptName = '' then
    FPostScriptName := FindName(TdxFontFilePlatformID.Macintosh,
      TdxFontFileEncodingID.Undefined, TdxFontFileLanguageID.English,
      TdxFontFileNameID.PostscriptName);
  Result := FPostScriptName;
end;

class function TdxFontFileNameTable.Tag: string;
begin
  Result := 'name';
end;

function TdxFontFileNameTable.FindName(APlatform: TdxFontFilePlatformID;
AEncoding: TdxFontFileEncodingID; ALanguage: TdxFontFileLanguageID;
AId: TdxFontFileNameID): string;
var
  ARecord: TdxFontFileNameRecord;
begin
  for ARecord in FNamingTable do
    if (((ARecord.PlatformId = APlatform) and (ARecord.EncodingId = AEncoding))
      and (ARecord.LanguageID = ALanguage)) and (ARecord.NameID = AId) then
      Exit(ARecord.Name);
  Result := '';
end;

procedure TdxFontFileNameTable.AddName(ACMapEntry: TdxFontFileCMapTable;
const AFontName: string);
var
  ANamesDictionary: TDictionary<TdxFontFileNameID, TBytes>;
  AEncoding: TEncoding;
  AName: string;
  AFontNameBytes: TBytes;
  ACMapRecord: TdxFontFileCMapCustomFormatRecord;
  APlatformId: TdxFontFilePlatformID;
  AEncodingId: TdxFontFileEncodingID;
  ALanguageID: TdxFontFileLanguageID;
  AEntry: TPair<TdxFontFileNameID, TBytes>;
begin
  if Length(AFontName) > MaxNameLength then
    AName := Copy(AFontName, 1, MaxNameLength)
  else
    AName := AFontName;

  ANamesDictionary := TDictionary<TdxFontFileNameID, TBytes>.Create;
  try
    AEncoding := TEncoding.BigEndianUnicode;
    AFontNameBytes := AEncoding.GetBytes(AName);
    ANamesDictionary.Add(TdxFontFileNameID.FontFamily, AFontNameBytes);
    ANamesDictionary.Add(TdxFontFileNameID.FontSubfamily,
      AEncoding.GetBytes(NameFontSubfamily));
    ANamesDictionary.Add(TdxFontFileNameID.FullFontName, AFontNameBytes);
    ANamesDictionary.Add(TdxFontFileNameID.UniqueFontId, AFontNameBytes);
    ANamesDictionary.Add(TdxFontFileNameID.Version,
      AEncoding.GetBytes(NameVersion));
    ANamesDictionary.Add(TdxFontFileNameID.PostscriptName, AFontNameBytes);
    FNamingTable.Clear;
    for ACMapRecord in ACMapEntry.CMapTables do
    begin
      APlatformId := ACMapRecord.PlatformId;
      AEncodingId := ACMapRecord.EncodingId;
      if APlatformId = TdxFontFilePlatformID.Microsoft then
        ALanguageID := TdxFontFileLanguageID.EnglishUnitedStates
      else
        ALanguageID := TdxFontFileLanguageID.English;
      for AEntry in ANamesDictionary do
        FNamingTable.Add(TdxFontFileNameRecord.Create(APlatformId, ALanguageID,
          AEntry.Key, AEncodingId, AEntry.Value));
    end;
  finally
    ANamesDictionary.Free;
  end;
  Changed;
end;

procedure TdxFontFileNameTable.DoApplyChanges;
var
  I: Integer;
  ANameBytes: TBytes;
  ANameRecord: TdxFontFileNameRecord;
  ANameOffset, ANameLength: SmallInt;
begin
  inherited DoApplyChanges;
  DataStream.WriteShort(0);
  DataStream.WriteShort(SmallInt(FNamingTable.Count));
  DataStream.WriteShort(SmallInt((6 + FNamingTable.Count * 12)));
  ANameOffset := 0;
  for I := 0 to FNamingTable.Count - 1 do
  begin
    ANameRecord := FNamingTable[I];
    DataStream.WriteShort(SmallInt(ANameRecord.PlatformId));
    DataStream.WriteShort(SmallInt(ANameRecord.EncodingId));
    DataStream.WriteShort(SmallInt(ANameRecord.LanguageID));
    DataStream.WriteShort(SmallInt(ANameRecord.NameID));
    ANameBytes := ANameRecord.NameBytes;
    ANameLength := SmallInt((Length(ANameRecord.NameBytes)));
    DataStream.WriteShort(ANameLength);
    DataStream.WriteShort(ANameOffset);
    Inc(ANameOffset, ANameLength);
  end;
  for I := 0 to FNamingTable.Count - 1 do
  begin
    ANameRecord := FNamingTable[I];
    if ANameRecord.NameBytes <> nil then
      DataStream.WriteArray(ANameRecord.NameBytes);
  end;
end;

{ TdxFontFilePostTable }

constructor TdxFontFilePostTable.Create(const AData: TBytes);
var
  AVersion, AGlyphCount, I: Integer;
  AStringID, AIndex: SmallInt;
  AStringIDs: TSmallIntDynArray;
  ANames: TStringList;
begin
  inherited Create(AData);
  FGlyphNames := TStringList.Create;
  if Length(AData) > 0 then
  begin
    AVersion := DataStream.ReadInt;
    FItalicAngle := DataStream.ReadFixed;
    FUnderlinePosition := DataStream.ReadShort;
    FUnderlineThickness := DataStream.ReadShort;
    FIsFixedPitch := DataStream.ReadInt <> 0;
    FMinMemType42 := DataStream.ReadInt;
    FMaxMemType42 := DataStream.ReadInt;
    FMinMemType1 := DataStream.ReadInt;
    FMaxMemType1 := DataStream.ReadInt;
    case AVersion of
      $10000:
        for I := 0 to 257 do
          FGlyphNames.Add(StandardMacCharacterSet[I]);
      $20000:
        if DataStream.Size > 32 then
        begin
          AGlyphCount := DataStream.ReadUshort;
          AStringIDs := DataStream.ReadShortArray(AGlyphCount);
          ANames := TStringList.Create;
          try
            while DataStream.Position < DataStream.Size do
              ANames.Add(DataStream.ReadString(DataStream.ReadByte));
            FGlyphNames.Capacity := AGlyphCount;
            for I := 0 to AGlyphCount - 1 do
            begin
              AStringID := AStringIDs[I];
              if (AStringID >= 0) and (AStringID < 258) then
                FGlyphNames.Add(StandardMacCharacterSet[AStringID])
              else
              begin
                AIndex := SmallInt(AStringID - 258);
                if (AIndex >= 0) and (AIndex < ANames.Count) then
                  FGlyphNames.Add(ANames[AIndex])
                else
                  FGlyphNames.Add(TdxGlyphNames._notdef);
              end;
            end;
          finally
            ANames.Free;
          end;
        end;
    end;
  end;
end;

destructor TdxFontFilePostTable.Destroy;
begin
  FreeAndNil(FGlyphNames);
  inherited Destroy;
end;

class function TdxFontFilePostTable.Tag: string;
begin
  Result := 'post';
end;

{ TdxFontFileOS2Table }

constructor TdxFontFileOS2Table.Create(const AData: TBytes);
const
  EmptyValue = $00000000;
begin
  inherited Create(AData);
  if Length(AData) > 0 then
  begin
    FVersion := TdxFontFileVersion(DataStream.ReadShort);
    FAvgCharWidth := DataStream.ReadShort;
    FWeightClass := DataStream.ReadShort;
    FWidthClass := TdxFontFileOS2WidthClass(DataStream.ReadShort);
    FEmbeddingType := TdxFontFileOS2EmbeddingType(DataStream.ReadShort);
    FSubscriptXSize := DataStream.ReadShort;
    FSubscriptYSize := DataStream.ReadShort;
    FSubscriptXOffset := DataStream.ReadShort;
    FSubscriptYOffset := DataStream.ReadShort;
    FSuperscriptXSize := DataStream.ReadShort;
    FSuperscriptYSize := DataStream.ReadShort;
    FSuperscriptXOffset := DataStream.ReadShort;
    FSuperscriptYOffset := DataStream.ReadShort;
    FStrikeoutSize := DataStream.ReadShort;
    FStrikeoutPosition := DataStream.ReadShort;
    FFamilyClass := TdxFontFileOS2FamilyClass(DataStream.ReadShort);
    FPanose := TdxFontFilePanose.Create(DataStream);
    FUnicodeRange1 := TdxFontFileUnicodeRange1(DataStream.ReadInt);
    FUnicodeRange2 := TdxFontFileUnicodeRange2(DataStream.ReadInt);
    FUnicodeRange3 := TdxFontFileUnicodeRange3(DataStream.ReadInt);
    FUnicodeRange4 := TdxFontFileUnicodeRange4(DataStream.ReadInt);
    FVendor := DataStream.ReadString(4);
    FSelection := TdxFontFileSelection(DataStream.ReadShort);
    FFirstCharIndex := DataStream.ReadUshort;
    FLastCharIndex := DataStream.ReadUshort;
    FTypoAscender := DataStream.ReadShort;
    FTypoDescender := DataStream.ReadShort;
    FTypoLineGap := DataStream.ReadShort;
    FWinAscent := DataStream.ReadShort;
    FWinDescent := DataStream.ReadShort;
    if FVersion > TdxFontFileVersion.TrueType_1_5 then
    begin
      FCodePageRange1 := TdxFontFileCodePageRange1(DataStream.ReadInt);
      FCodePageRange2 := TdxFontFileCodePageRange2(DataStream.ReadInt);
    end
    else
    begin
      FCodePageRange1 := TdxFontFileCodePageRange1(EmptyValue);
      FCodePageRange2 := TdxFontFileCodePageRange2(EmptyValue);
    end;
  end;
end;

class function TdxFontFileOS2Table.Tag: string;
begin
  Result := 'OS/2';
end;

procedure TdxFontFileOS2Table.DoApplyChanges;
begin
  inherited DoApplyChanges;
  DataStream.WriteShort(SmallInt(FVersion));
  DataStream.WriteShort(FAvgCharWidth);
  DataStream.WriteShort(FWeightClass);
  DataStream.WriteShort(SmallInt(FWidthClass));
  DataStream.WriteShort(SmallInt(FEmbeddingType));
  DataStream.WriteShort(FSubscriptXSize);
  DataStream.WriteShort(FSubscriptYSize);
  DataStream.WriteShort(FSubscriptXOffset);
  DataStream.WriteShort(FSubscriptYOffset);
  DataStream.WriteShort(FSuperscriptXSize);
  DataStream.WriteShort(FSuperscriptYSize);
  DataStream.WriteShort(FSuperscriptXOffset);
  DataStream.WriteShort(FSuperscriptYOffset);
  DataStream.WriteShort(FStrikeoutSize);
  DataStream.WriteShort(FStrikeoutPosition);
  DataStream.WriteShort(SmallInt(FFamilyClass));
  FPanose.Write(DataStream);
  DataStream.WriteInt(Integer(FUnicodeRange1));
  DataStream.WriteInt(Integer(FUnicodeRange2));
  DataStream.WriteInt(Integer(FUnicodeRange3));
  DataStream.WriteInt(Integer(FUnicodeRange4));
  DataStream.WriteString(FVendor);
  DataStream.WriteShort(SmallInt(FSelection));
  DataStream.WriteShort(SmallInt(FFirstCharIndex));
  DataStream.WriteShort(SmallInt(FLastCharIndex));
  DataStream.WriteShort(FTypoAscender);
  DataStream.WriteShort(FTypoDescender);
  DataStream.WriteShort(FTypoLineGap);
  DataStream.WriteShort(FWinAscent);
  DataStream.WriteShort(FWinDescent);
  if FVersion >= TdxFontFileVersion.TrueType_1_66 then
  begin
    DataStream.WriteInt(Integer(FCodePageRange1));
    DataStream.WriteInt(Integer(FCodePageRange2));
    if FVersion >= TdxFontFileVersion.OpenType_1_2 then
    begin
      DataStream.WriteShort(FXHeight);
      DataStream.WriteShort(FCapHeight);
      DataStream.WriteShort(FDefaultChar);
      DataStream.WriteShort(FBreakChar);
      DataStream.WriteShort(FMaxContext);
    end;
  end;
end;

function TdxFontFileOS2Table.GetIsSymbolic: Boolean;
begin
  Result := (Integer(FCodePageRange1) and
    Integer(TdxFontFileCodePageRange1.SymbolCharacterSet)) <> 0;
end;

function TdxFontFileOS2Table.GetUseTypoMetrics: Boolean;
begin
  Result := (Integer(FSelection) and
    Integer(TdxFontFileSelection.USE_TYPO_METRICS)) <> 0;
end;

procedure TdxFontFileOS2Table.SetWinAscent(const AValue: SmallInt);
begin
  FWinAscent := AValue;
  Changed;
end;

procedure TdxFontFileOS2Table.SetWinDescent(const AValue: SmallInt);
begin
  FWinDescent := AValue;
  Changed;
end;

{ TdxFontFileMaxpTable }

constructor TdxFontFileMaxpTable.Create(AGlyphCount: Integer);
var
  AData: TBytes;
begin
  SetLength(AData, 0);
  inherited Create(AData);
  DataStream.WriteInt($00005000);
  DataStream.WriteShort(AGlyphCount);
end;

class function TdxFontFileMaxpTable.Tag: string;
begin
  Result := 'maxp';
end;

function TdxFontFileMaxpTable.GetNumGlyphs: Integer;
begin
  DataStream.Position := NumGlyphsOffset;
  Result := DataStream.ReadUshort;
end;

procedure TdxFontFileMaxpTable.SetNumGlyphs(const AValue: Integer);
begin
  DataStream.Position := NumGlyphsOffset;
  DataStream.WriteShort(SmallInt(AValue));
end;

{ TdxFontFileKernTable }

constructor TdxFontFileKernTable.Create(const AData: TBytes);
var
  ATablesCount, I, AFormat, APairCount, J, APair: Integer;
  ATableStart, ALastTableLength: Int64;
begin
  inherited Create(AData);
  FKerning := TDictionary<Integer, SmallInt>.Create;

  DataStream.ReadUshort;
  ATablesCount := DataStream.ReadUshort;
  ATableStart := DataStream.Position;
  ALastTableLength := 0;
  for I := 0 to ATablesCount - 1 do
  begin
    Inc(ATableStart, ALastTableLength);
    DataStream.Position := ATableStart + 2;
    ALastTableLength := DataStream.ReadUshort;
    AFormat := DataStream.ReadUshort and $FFF7;
    if AFormat = 1 then
    begin
      APairCount := DataStream.ReadUshort;
      DataStream.ReadArray(6);
      for J := 0 to APairCount - 1 do
      begin
        APair := DataStream.ReadInt;
        if not FKerning.ContainsKey(APair) then
          FKerning.Add(APair, DataStream.ReadShort);
      end;
    end;
  end;
end;

destructor TdxFontFileKernTable.Destroy;
begin
  FreeAndNil(FKerning);
  inherited Destroy;
end;

class function TdxFontFileKernTable.Tag: string;
begin
  Result := 'kern';
end;

function TdxFontFileKernTable.GetKerning(AGlyphIndex1: Integer;
AGlyphIndex2: Integer): SmallInt;
begin
  if not FKerning.TryGetValue((AGlyphIndex1 shl 16) + AGlyphIndex2, Result) then
    Result := 0;
end;

procedure TdxFontFileCMapSegmentMappingRecord.
  Write(ATableStream: TdxFontFileStream);
var
  ADoubleSegCount, ASearchRange: SmallInt;
begin
  inherited Write(ATableStream);
  ADoubleSegCount := SmallInt((FSegCount * 2));
  ATableStream.WriteShort(ADoubleSegCount);
  ASearchRange := SmallInt(Trunc(2 * Power(2, Floor(Log2(FSegCount)))));
  ATableStream.WriteShort(ASearchRange);
  ATableStream.WriteShort(SmallInt(Floor(Log2(FSegCount))));
  ATableStream.WriteShort(SmallInt((ADoubleSegCount - ASearchRange)));
  ATableStream.WriteShortArray(FEndCode);
  ATableStream.WriteShort(0);
  ATableStream.WriteShortArray(FStartCode);
  ATableStream.WriteShortArray(FIdDelta);
  ATableStream.WriteShortArray(FIdRangeOffset);
  ATableStream.WriteShortArray(FGlyphIdArray);
end;

{ TdxFontFileCMapLongRecord }

constructor TdxFontFileCMapLongRecord.Create(APlatformId: TdxFontFilePlatformID;
AEncodingId: TdxFontFileEncodingID; AStream: TdxFontFileStream);
begin
  inherited Create(APlatformId, AEncodingId);
  AStream.ReadShort;
  AStream.ReadInt;
  Language := AStream.ReadInt;
end;

procedure TdxFontFileCMapLongRecord.Write(ATableStream: TdxFontFileStream);
begin
  inherited Write(ATableStream);
  ATableStream.WriteShort(0);
  ATableStream.WriteInt(Size);
  ATableStream.WriteInt(Language);
end;

{ TdxFontFileCMapGroup }

class function TdxFontFileCMapGroup.Create(AStartCharCode, AEndCharCode,
  AGlyphID: Integer): TdxFontFileCMapGroup;
begin
  Result.FStartCharCode := AStartCharCode;
  Result.FEndCharCode := AEndCharCode;
  Result.FGlyphID := AGlyphID;
end;

class function TdxFontFileCMapGroup.ReadGroups(AStream: TdxFontFileStream;
AGroupsCount: Integer): TArray<TdxFontFileCMapGroup>;
var
  I, AStartCharCode, AEndCharCode, AGlyphID: Integer;
begin
  SetLength(Result, AGroupsCount);
  for I := 0 to AGroupsCount - 1 do
  begin
    AStartCharCode := AStream.ReadInt;
    AEndCharCode := AStream.ReadInt;
    AGlyphID := AStream.ReadInt;
    Result[I] := TdxFontFileCMapGroup.Create(AStartCharCode, AEndCharCode,
      AGlyphID);
  end;
end;

class procedure TdxFontFileCMapGroup.WriteGroups(const AGroups
  : TArray<TdxFontFileCMapGroup>; ATableStream: TdxFontFileStream);
var
  AGroup: TdxFontFileCMapGroup;
begin
  for AGroup in AGroups do
  begin
    ATableStream.WriteInt(AGroup.StartCharCode);
    ATableStream.WriteInt(AGroup.EndCharCode);
    ATableStream.WriteInt(AGroup.GlyphID);
  end;
end;

procedure TdxFontFileHheaTable.DoApplyChanges;
begin
  inherited DoApplyChanges;
  DataStream.WriteInt(FVersion);
  DataStream.WriteShort(FAscender);
  DataStream.WriteShort(FDescender);
  DataStream.WriteShort(FLineGap);
  DataStream.WriteShort(FAdvanceWidthMax);
  DataStream.WriteShort(FMinLeftSideBearing);
  DataStream.WriteShort(FMinRightSideBearing);
  DataStream.WriteShort(FXMaxExtent);
  DataStream.WriteShort(FCaretSlopeRise);
  DataStream.WriteShort(FCaretSlopeRun);
  DataStream.WriteShort(0);
  DataStream.WriteShort(0);
  DataStream.WriteShort(0);
  DataStream.WriteShort(0);
  DataStream.WriteShort(0);
  DataStream.WriteShort(FMetricDataFormat);
  DataStream.WriteShort(SmallInt(FNumberOfHMetrics));
end;

{ TdxFontFileHeadTable }

constructor TdxFontFileHeadTable.Create(const AData: TBytes);
var
  AValue: SmallInt;
begin
  inherited Create(AData);
  if Length(AData) > 0 then
  begin
    FUnitsPerEm := DefaultUnitsPerEm;
    FVersion := DataStream.ReadInt;
    FFontRevision := DataStream.ReadInt;
    FCheckSumAdjustment := DataStream.ReadInt;
    FMagicNumber := DataStream.ReadInt;
    FFlags := TdxFontFileHeadTableFlags(DataStream.ReadShort);
    FUnitsPerEm := DataStream.ReadShort;
    FCreated := DataStream.ReadLong;
    FModified := DataStream.ReadLong;
    FXMin := DataStream.ReadShort;
    FYMin := DataStream.ReadShort;
    FXMax := DataStream.ReadShort;
    FYMax := DataStream.ReadShort;
    FMacStyle := TdxFontFileHeadTableMacStyle(DataStream.ReadShort);
    FLowestRecPPEM := DataStream.ReadShort;
    FFontDirectionHint := TdxFontFileDirectionHint(DataStream.ReadShort);

    AValue := DataStream.ReadShort;
    if AValue = 0 then
      FIndexToLocFormat := TdxFontFileIndexToLocFormat.Short
    else
      FIndexToLocFormat := TdxFontFileIndexToLocFormat.Long;
    FGlyphDataFormat := DataStream.ReadShort;
  end;
end;

class function TdxFontFileHeadTable.Tag: string;
const
  Var1 = 'head';
begin
  Result := Var1;
end;

procedure TdxFontFileHeadTable.Validate
  (AGlyphs: TDictionary<Integer, TdxFontFileGlyphDescription>);
var
  AOldXMin, AOldYMin, AOldXMax, AOldYMax: SmallInt;
  AGlyph: TdxFontFileGlyphDescription;
begin
  AOldXMin := FXMin;
  AOldYMin := FYMin;
  AOldXMax := FXMax;
  AOldYMax := FYMax;
  for AGlyph in AGlyphs.Values do
  begin
    FXMin := Min(AGlyph.XMin, FXMin);
    FYMin := Min(AGlyph.YMin, FYMin);
    FXMax := Max(AGlyph.XMax, FXMax);
    FYMax := Max(AGlyph.YMax, FYMax);
  end;
  if (AOldXMin <> FXMin) or (AOldYMin <> FYMin) or (AOldXMax <> FXMax) or
    (AOldYMax <> FYMax) then
    Changed;
end;

procedure TdxFontFileHeadTable.DoApplyChanges;
begin
  inherited DoApplyChanges;
  DataStream.WriteInt(FVersion);
  DataStream.WriteInt(FFontRevision);
  DataStream.WriteInt(FCheckSumAdjustment);
  DataStream.WriteInt(FMagicNumber);
  DataStream.WriteShort(SmallInt(FFlags));
  DataStream.WriteShort(FUnitsPerEm);
  DataStream.WriteLong(FCreated);
  DataStream.WriteLong(FModified);
  DataStream.WriteShort(FXMin);
  DataStream.WriteShort(FYMin);
  DataStream.WriteShort(FXMax);
  DataStream.WriteShort(FYMax);
  DataStream.WriteShort(SmallInt(FMacStyle));
  DataStream.WriteShort(FLowestRecPPEM);
  DataStream.WriteShort(SmallInt(FFontDirectionHint));
  DataStream.WriteShort(SmallInt(FIndexToLocFormat));
  DataStream.WriteShort(FGlyphDataFormat);
end;

{ TdxFontFileCMapRangeMappingRecord }

constructor TdxFontFileCMapRangeMappingRecord.Create
  (APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID;
AStream: TdxFontFileStream);
begin
  inherited Create(APlatformId, AEncodingId, AStream);
  FGroups := TdxFontFileCMapGroup.ReadGroups(AStream, AStream.ReadInt);
end;

procedure TdxFontFileCMapRangeMappingRecord.
  Write(ATableStream: TdxFontFileStream);
begin
  inherited Write(ATableStream);
  ATableStream.WriteInt(Length(FGroups));
  TdxFontFileCMapGroup.WriteGroups(FGroups, ATableStream);
end;

function TdxFontFileCMapRangeMappingRecord.GetSize: Integer;
begin
  Result := HeaderLength + Length(FGroups) * 12;
end;

{ TdxFontFileCMapSegmentedCoverageRecord }

function TdxFontFileCMapSegmentedCoverageRecord.GetFormat
  : TdxFontFileCMapFormatID;
begin
  Result := TdxFontFileCMapFormatID.SegmentedCoverage;
end;

{ TdxFontFileCMapManyToOneRangeMappingRecord }

function TdxFontFileCMapManyToOneRangeMappingRecord.GetFormat
  : TdxFontFileCMapFormatID;
begin
  Result := TdxFontFileCMapFormatID.ManyToOneRangeMapping;
end;

{ TdxFontFileCMapMixedCoverageRecord }

constructor TdxFontFileCMapMixedCoverageRecord.Create
  (APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID;
AStream: TdxFontFileStream);
begin
  inherited Create(APlatformId, AEncodingId, AStream);
  FIs32 := AStream.ReadArray(8192);
  FGroups := TdxFontFileCMapGroup.ReadGroups(AStream, AStream.ReadInt);
end;

procedure TdxFontFileCMapMixedCoverageRecord.
  Write(ATableStream: TdxFontFileStream);
begin
  inherited Write(ATableStream);
  ATableStream.WriteArray(FIs32);
  ATableStream.WriteInt(Length(FGroups));
  TdxFontFileCMapGroup.WriteGroups(FGroups, ATableStream);
end;

function TdxFontFileCMapMixedCoverageRecord.GetFormat: TdxFontFileCMapFormatID;
begin
  Result := TdxFontFileCMapFormatID.MixedCoverage;
end;

function TdxFontFileCMapMixedCoverageRecord.GetSize: Integer;
begin
  Result := HeaderLength + Length(FGroups) * 12;
end;

{ TdxFontFileCMapTrimmedArrayRecord }

constructor TdxFontFileCMapTrimmedArrayRecord.Create
  (APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID;
AStream: TdxFontFileStream);
begin
  inherited Create(APlatformId, AEncodingId, AStream);
  FStartCharacterCode := AStream.ReadInt;
  FCharacterCount := AStream.ReadInt;
  FGlyphs := AStream.ReadShortArray(FCharacterCount);
end;

procedure TdxFontFileCMapTrimmedArrayRecord.
  Write(ATableStream: TdxFontFileStream);
begin
  inherited Write(ATableStream);
  ATableStream.WriteInt(FStartCharacterCode);
  ATableStream.WriteInt(FCharacterCount);
  ATableStream.WriteShortArray(FGlyphs);
end;

function TdxFontFileCMapTrimmedArrayRecord.GetFormat: TdxFontFileCMapFormatID;
begin
  Result := TdxFontFileCMapFormatID.TrimmedArray;
end;

function TdxFontFileCMapTrimmedArrayRecord.GetSize: Integer;
begin
  Result := HeaderLength + Length(FGlyphs) * 2;
end;

{ TdxDefaultUVSTable }

constructor TdxDefaultUVSTable.Create(AStartUnicodeValue: Integer;
AAdditionalCount: Byte);
begin
  inherited Create;
  FStartUnicodeValue := AStartUnicodeValue;
  FAdditionalCount := AAdditionalCount;
end;

procedure TdxDefaultUVSTable.Write(ATableStream: TdxFontFileStream);
var
  AStartUnicodeValueBytes: TBytes;
begin
  SetLength(AStartUnicodeValueBytes, 3);
  AStartUnicodeValueBytes[2] := Byte((FStartUnicodeValue and $FF));
  AStartUnicodeValueBytes[1] := Byte(((FStartUnicodeValue and $FF00) shr 8));
  AStartUnicodeValueBytes[0] := Byte(((FStartUnicodeValue and $FF0000) shr 16));
  ATableStream.WriteArray(AStartUnicodeValueBytes);
  ATableStream.WriteByte(FAdditionalCount);
end;

{ TdxNonDefaultUVSTable }

constructor TdxNonDefaultUVSTable.Create(AUnicodeValue: Integer;
AGlyphID: SmallInt);
begin
  inherited Create;
  FUnicodeValue := AUnicodeValue;
  FGlyphID := AGlyphID;
end;

procedure TdxNonDefaultUVSTable.Write(ATableStream: TdxFontFileStream);
var
  AUnicodeValueBytes: TBytes;
begin
  SetLength(AUnicodeValueBytes, 3);
  AUnicodeValueBytes[2] := Byte((FUnicodeValue and $FF));
  AUnicodeValueBytes[1] := Byte(((FUnicodeValue and $FF00) shr 8));
  AUnicodeValueBytes[0] := Byte(((FUnicodeValue and $FF0000) shr 16));
  ATableStream.WriteArray(AUnicodeValueBytes);
  ATableStream.WriteShort(FGlyphID);
end;

{ TdxFontFileCMapUnicodeVariationSelectorRecord }

constructor TdxFontFileCMapUnicodeVariationSelectorRecord.Create
  (AVarSelector: Integer; const ADefaultUVSTables: TdxDefaultUVSTables;
const ANonDefaultUVSTables: TdxNonDefaultUVSTables);
begin
  inherited Create;
  FVarSelector := AVarSelector;
  FDefaultUVSTables := ADefaultUVSTables;
  FNonDefaultUVSTables := ANonDefaultUVSTables;
end;

destructor TdxFontFileCMapUnicodeVariationSelectorRecord.Destroy;

  procedure DestroyDefaultUVSTables;
  var
    I: Integer;
  begin
    for I := 0 to Length(FDefaultUVSTables) - 1 do
      FDefaultUVSTables[I].Free;
  end;

  procedure DestroyNonDefaultUVSTables;
  var
    I: Integer;
  begin
    for I := 0 to Length(FNonDefaultUVSTables) - 1 do
      FNonDefaultUVSTables[I].Free;
  end;

begin
  DestroyDefaultUVSTables;
  DestroyNonDefaultUVSTables;
  inherited Destroy;
end;

{ TdxFontFileHmtxTable }

constructor TdxFontFileHmtxTable.Create(AGlyphCount: Integer);
var
  I: Integer;
  Var1: TBytes;
begin
  SetLength(Var1, 0);
  inherited Create(Var1);
  for I := 0 to AGlyphCount * 4 - 1 do
    DataStream.WriteByte(0);
end;

class function TdxFontFileHmtxTable.Tag: string;
begin
  Result := 'hmtx';
end;

function TdxFontFileHmtxTable.FillAdvanceWidths(AHMetricCount,
  AGlyphCount: Integer): TSmallIntDynArray;
var
  ASize, I: Integer;
  ALastAdvanceWidth: SmallInt;
begin
  DataStream.Position := 0;
  ASize := Max(AHMetricCount, AGlyphCount);
  SetLength(FAdvanceWidths, ASize);
  if DataSize >= AHMetricCount * 4 then
  begin
    for I := 0 to AHMetricCount - 1 do
    begin
      FAdvanceWidths[I] := DataStream.ReadShort;
      DataStream.ReadShort;
    end;
    ALastAdvanceWidth := FAdvanceWidths[AHMetricCount - 1];
    for I := AHMetricCount to ASize - 1 do
      FAdvanceWidths[I] := ALastAdvanceWidth;
  end;
  Result := FAdvanceWidths;
end;

procedure TdxFontFileHmtxTable.Validate(AFontFile: TdxFontFile);
var
  AHheaSize, AMaxpSize: Integer;
begin
  if AFontFile.HheaTable = nil then
    AHheaSize := 0
  else
    AHheaSize := AFontFile.HheaTable.NumberOfHMetrics;
  if AFontFile.MaxpTable = nil then
    AMaxpSize := 0
  else
    AMaxpSize := AFontFile.MaxpTable.NumGlyphs;
  FExpectedDataSize := Max(AHheaSize, AMaxpSize) * 4;
  if DataStream.Size < FExpectedDataSize then
    Changed;
end;

procedure TdxFontFileHmtxTable.ApplyChanges;
var
  ACount: Integer;
  APrevData, AData: TBytes;
begin
  if NeedWrite then
  begin
    APrevData := Data;
    RecreateStream;
    ACount := FExpectedDataSize - Length(APrevData);
    DataStream.WriteArray(APrevData);
    SetLength(AData, ACount);
    DataStream.WriteArray(AData);
  end;
end;

{ TdxFontFileHheaTable }

constructor TdxFontFileHheaTable.Create(const AData: TBytes);
begin
  inherited Create(AData);
  if Length(AData) > 0 then
  begin
    FVersion := DataStream.ReadInt;
    FAscender := DataStream.ReadShort;
    FDescender := DataStream.ReadShort;
    FLineGap := DataStream.ReadShort;
    FAdvanceWidthMax := DataStream.ReadShort;
    FMinLeftSideBearing := DataStream.ReadShort;
    FMinRightSideBearing := DataStream.ReadShort;
    FXMaxExtent := DataStream.ReadShort;
    FCaretSlopeRise := DataStream.ReadShort;
    FCaretSlopeRun := DataStream.ReadShort;
    DataStream.ReadShort;
    DataStream.ReadShort;
    DataStream.ReadShort;
    DataStream.ReadShort;
    DataStream.ReadShort;
    FMetricDataFormat := DataStream.ReadShort;
    FNumberOfHMetrics := DataStream.ReadUshort;
  end;
end;

class function TdxFontFileHheaTable.Tag: string;
begin
  Result := 'hhea';
end;

procedure TdxFontFileHheaTable.Validate;
begin
  if (FAscender = 0) and (FDescender = 0) then
  begin
    FAscender := 1;
    FDescender := -1;
    Changed;
  end;
end;

{ TdxFontFileGlyphDescription }

class function TdxFontFileGlyphDescription.Create(AStream: TdxFontFileStream;
AGlyphDataSize: Integer): TdxFontFileGlyphDescription;
var
  AGlyphStart: Int64;
  AFlags: Word;
begin
  SetLength(Result.GlyphIndexList, 0);

  AGlyphStart := AStream.Position;
  Result.FBoundingBoxParsed := False;
  Result.NumberOfContours := AStream.ReadShort;
  Result.Data := AStream.ReadArray(AGlyphDataSize - 2);
  if Result.NumberOfContours < 0 then
  begin
    AStream.Position := AGlyphStart + HeaderSize;
    repeat
      AFlags := Word(AStream.ReadUshort);
      TdxPDFUtils.AddValue(AStream.ReadUshort, Result.GlyphIndexList);
      if (AFlags and ARG_1_AND_2_ARE_WORDS) <> 0 then
        AStream.Position := AStream.Position + 4
      else
        AStream.Position := AStream.Position + 2;
      if (AFlags and WE_HAVE_A_SCALE) <> 0 then
        AStream.Position := AStream.Position + 2
      else if (AFlags and WE_HAVE_AN_X_AND_Y_SCALE) <> 0 then
        AStream.Position := AStream.Position + 4
      else if (AFlags and WE_HAVE_A_TWO_BY_TWO) <> 0 then
        AStream.Position := AStream.Position + 8;
    until not((AFlags and MORE_COMPONENTS) <> 0);
  end;
end;

function TdxFontFileGlyphDescription.GetSize: Integer;
begin
  Result := Length(Data) + 2;
end;

function TdxFontFileGlyphDescription.GetXMin: SmallInt;
begin
  if not FBoundingBoxParsed then
    ParseBoundingBox;
  Result := FXMin;
end;

function TdxFontFileGlyphDescription.GetYMin: SmallInt;
begin
  if not FBoundingBoxParsed then
    ParseBoundingBox;
  Result := FYMin;
end;

function TdxFontFileGlyphDescription.GetXMax: SmallInt;
begin
  if not FBoundingBoxParsed then
    ParseBoundingBox;
  Result := FXMax;
end;

function TdxFontFileGlyphDescription.GetYMax: SmallInt;
begin
  if not FBoundingBoxParsed then
    ParseBoundingBox;
  Result := FYMax;
end;

procedure TdxFontFileGlyphDescription.ParseBoundingBox;
var
  AStream: TdxPDFMemoryStream;
begin
  if Length(Data) >= 8 then
  begin
    AStream := TdxPDFMemoryStream.Create(Data);
    try
      FXMin := AStream.ReadShort;
      FYMin := AStream.ReadShort;
      FXMax := AStream.ReadShort;
      FYMax := AStream.ReadShort;
    finally
      AStream.Free;
    end;
    FBoundingBoxParsed := True;
  end;
end;

function TdxFontFileGlyphDescription.IsEmpty: Boolean;
begin
  Result := NumberOfContours = 0;
end;

procedure TdxFontFileGlyphDescription.Write(AStream: TdxFontFileStream);
begin
  AStream.WriteShort(NumberOfContours);
  AStream.WriteArray(Data);
end;

{ TdxFontFileGlyphTable }

constructor TdxFontFileGlyphTable.Create(const AData: TBytes);
begin
  inherited Create(AData);
  FGlyphs := TDictionary<Integer, TdxFontFileGlyphDescription>.Create;
  FSubsetGlyphs := nil;
end;

destructor TdxFontFileGlyphTable.Destroy;
begin
  FreeAndNil(FSubsetGlyphs);
  FreeAndNil(FGlyphs);
  inherited Destroy;
end;

class function TdxFontFileGlyphTable.Tag: string;
begin
  Result := 'glyf';
end;

function TdxFontFileGlyphTable.Pad4(AValue: Integer): Integer;
var
  ADiff: Integer;
begin
  ADiff := AValue mod 4;
  Result := IfThen(ADiff = 0, AValue, AValue + 4 - AValue mod 4);
end;

procedure TdxFontFileGlyphTable.SortSubsetGlyphs;
var
  AComparison: TComparison<TdxFontFileSubsetGlyph>;
begin
  AComparison := function(const Left, Right: TdxFontFileSubsetGlyph): Integer
    begin
      Result := Left.Index - Right.Index;
    end;

  FSubsetGlyphs.Sort(TComparer<TdxFontFileSubsetGlyph>.Construct(AComparison));
end;

procedure TdxFontFileGlyphTable.CreateSubset(AFontFile: TdxFontFile;
AMapping: TdxPDFIntegerStringDictionary);

  function IndexExists(AIndex: Integer): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to FSubsetGlyphs.Count - 1 do
    begin
      Result := FSubsetGlyphs[I].Index = AIndex;
      if Result then
        Break;
    end;
  end;

var
  AGlyph: TdxFontFileGlyphDescription;
  AGlyphIndex, AIndex: Integer;
  ALoca: TdxFontFileLocaTable;
  ASubsetGlyph, ATempSubsetGlyph: TdxFontFileSubsetGlyph;
begin
  if FSubsetGlyphs = nil then
    FSubsetGlyphs := TList<TdxFontFileSubsetGlyph>.Create;
  FSubsetGlyphs.Clear;

  for AGlyphIndex in AMapping.Keys do
    if FGlyphs.TryGetValue(AGlyphIndex, AGlyph) then
    begin
      for AIndex in AGlyph.GlyphIndexList do
        if not IndexExists(AIndex) and FGlyphs.ContainsKey(AIndex) then
        begin
          ASubsetGlyph.Index := AIndex;
          ASubsetGlyph.Description := FGlyphs[AIndex];
          FSubsetGlyphs.Add(ASubsetGlyph);
        end;
      ATempSubsetGlyph.Index := AGlyphIndex;
      ATempSubsetGlyph.Description := AGlyph;
      FSubsetGlyphs.Add(ATempSubsetGlyph);
    end;

  SortSubsetGlyphs;

  FGlyphOffsets := CalculateOffsets(Length(FGlyphOffsets) - 1);

  ALoca := AFontFile.LocaTable;
  if ALoca <> nil then
    ALoca.GlyphOffsets := FGlyphOffsets;
  Changed;
end;

procedure TdxFontFileGlyphTable.ReadGlyphs(AFontFile: TdxFontFile);
var
  AStreamLength, ACount, ANextOffset, ANextOffsetIndex, AOffset, ACurrentOffset,
    ASize, AGlyphOffset, AIndex, I, ANextGlyphOffset: Integer;
  AOffsets: TdxPDFIntegerList;
  AGlyphDescriptions: TDictionary<Integer, TdxFontFileGlyphDescription>;
  AGlyph: TdxFontFileGlyphDescription;
  AKey: Integer;
  ATemp: TdxFontFileSubsetGlyph;
  AValue: TdxFontFileGlyphDescription;
begin
  FGlyphs.Clear;
  if AFontFile.LocaTable <> nil then
  begin
    DataStream.Position := 0;
    AStreamLength := DataSize;
    FGlyphOffsets := AFontFile.LocaTable.GlyphOffsets;
    ACount := Length(FGlyphOffsets) - 1;

    AOffsets := TdxPDFIntegerList.Create;
    try
      AOffsets.Capacity := Length(FGlyphOffsets);
      for I := 0 to Length(FGlyphOffsets) - 1 do
        AOffsets.Add(FGlyphOffsets[I]);
      AOffsets.Sort;

      AGlyphDescriptions :=
        TDictionary<Integer, TdxFontFileGlyphDescription>.Create;
      try
        ANextOffset := AOffsets[0];
        if AOffsets[0] <> FGlyphOffsets[0] then
          Changed;

        ANextOffsetIndex := 1;
        for I := 0 to ACount - 1 do
        begin
          AOffset := ANextOffset;
          ACurrentOffset := FGlyphOffsets[ANextOffsetIndex];
          ANextOffset := Min(AOffsets[ANextOffsetIndex], AStreamLength);
          Inc(ANextOffsetIndex);
          if ANextOffset <> ACurrentOffset then
            Changed;
          ASize := ANextOffset - AOffset;
          if ASize >= TdxFontFileGlyphDescription.HeaderSize then
          begin
            DataStream.Position := AOffset;
            AGlyph := TdxFontFileGlyphDescription.Create(DataStream, ASize);
            if AGlyph.IsEmpty then
              Changed
            else
              AGlyphDescriptions.Add(AOffset, AGlyph);
          end
          else if ASize <> 0 then
          begin
            ANextOffset := AOffset;
            Changed;
          end;
        end;

        AGlyphOffset := FGlyphOffsets[0];

        AIndex := 0;
        for I := 1 to ACount do
        begin
          ANextGlyphOffset := FGlyphOffsets[I];
          if (ANextGlyphOffset - AGlyphOffset <> 0) and
            AGlyphDescriptions.TryGetValue(FGlyphOffsets[AIndex], AGlyph) then
            FGlyphs.Add(AIndex, AGlyph);
          AGlyphOffset := ANextGlyphOffset;
          Inc(AIndex);
        end;
      finally
        AGlyphDescriptions.Free;
      end;
    finally
      AOffsets.Free;
    end;

    FSubsetGlyphs := TList<TdxFontFileSubsetGlyph>.Create;
    for AKey in FGlyphs.Keys do
      if FGlyphs.TryGetValue(AKey, AValue) then
      begin
        ATemp.Index := AKey;
        ATemp.Description := AValue;
        FSubsetGlyphs.Add(ATemp);
      end;

    SortSubsetGlyphs;

    FGlyphOffsets := CalculateOffsets(ACount);
    if NeedWrite then
    begin
      AFontFile.LocaTable.GlyphOffsets := FGlyphOffsets;
      if AFontFile.MaxpTable <> nil then
        AFontFile.MaxpTable.NumGlyphs := SmallInt(ACount);
      Changed;
    end;
  end;
end;

function TdxFontFileGlyphTable.CalculateOffsets(AGlyphCount: Integer)
  : TIntegerDynArray;
var
  I, AIndex, AOffset, AOffsetIndex, AGlyphIndex: Integer;
  ASubsetGlyph: TdxFontFileSubsetGlyph;
begin
  SetLength(Result, AGlyphCount + 1);
  AOffset := 0;
  AOffsetIndex := 0;
  for AIndex := 0 to FSubsetGlyphs.Count - 1 do
  begin
    ASubsetGlyph := FSubsetGlyphs[AIndex];
    AGlyphIndex := ASubsetGlyph.Index;
    for I := AOffsetIndex to AGlyphIndex do
    begin
      Result[I] := AOffset;
      Inc(AOffsetIndex);
    end;
    Inc(AOffset, Pad4(ASubsetGlyph.Description.Size));
  end;
  for I := AOffsetIndex to AGlyphCount do
    Result[I] := AOffset;
end;

function TdxFontFileCMapUnicodeVariationSelectorRecord.
  Write(ATableStream: TdxFontFileStream; AOffset: Integer): Integer;
var
  ADefaultUVSTableSize, ANonDefaultUVSTableSize: Integer;
  AVarSelectorBytes: TBytes;
begin
  ADefaultUVSTableSize := 4;
  ANonDefaultUVSTableSize := 5;
  SetLength(AVarSelectorBytes, 3);
  AVarSelectorBytes[2] := Byte((FVarSelector and $FF));
  AVarSelectorBytes[1] := Byte(((FVarSelector and $FF00) shr 8));
  AVarSelectorBytes[0] := Byte(((FVarSelector and $FF0000) shr 16));
  ATableStream.WriteArray(AVarSelectorBytes);
  if FDefaultUVSTables = nil then
    ATableStream.WriteInt(0)
  else
  begin
    ATableStream.WriteInt(AOffset);
    Inc(AOffset, (ADefaultUVSTableSize * Length(FDefaultUVSTables)) + 4);
  end;
  if FNonDefaultUVSTables = nil then
    ATableStream.WriteInt(0)
  else
  begin
    ATableStream.WriteInt(AOffset);
    Inc(AOffset, (ANonDefaultUVSTableSize * Length(FNonDefaultUVSTables) + 4));
  end;
  Result := AOffset;
end;

{ TdxFontFileCMapUnicodeVariationSequenciesRecord }

constructor TdxFontFileCMapUnicodeVariationSequenciesRecord.Create
  (APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID;
AStream: TdxFontFileStream);
var
  AStartPosition, APos: Int64;
  AVariationSelectorRecordsCount, I, AVarSelector, ADefaultUVSOffset: Integer;
  ANonDefaultUVSOffset, ANumUnicodeValueRanges, J, ANumUVSMappings: Integer;
  ADefaultUVSTables: TdxDefaultUVSTables;
  ANonDefaultUVSTables: TdxNonDefaultUVSTables;
begin
  inherited Create(APlatformId, AEncodingId);
  AStartPosition := AStream.Position - 2;
  AStream.ReadInt;
  AVariationSelectorRecordsCount := AStream.ReadInt;
  SetLength(FVariationSelectorRecords, AVariationSelectorRecordsCount);
  for I := 0 to AVariationSelectorRecordsCount - 1 do
  begin
    AVarSelector := GetInt24(AStream.ReadArray(3));
    ADefaultUVSOffset := AStream.ReadInt;
    ANonDefaultUVSOffset := AStream.ReadInt;
    APos := AStream.Position;
    ADefaultUVSTables := nil;
    ANonDefaultUVSTables := nil;
    if ADefaultUVSOffset <> 0 then
    begin
      AStream.Position := AStartPosition + ADefaultUVSOffset;
      ANumUnicodeValueRanges := AStream.ReadInt;
      SetLength(ADefaultUVSTables, ANumUnicodeValueRanges);
      for J := 0 to ANumUnicodeValueRanges - 1 do
        ADefaultUVSTables[J] := TdxDefaultUVSTable.Create
          (GetInt24(AStream.ReadArray(3)), AStream.ReadByte);
    end;
    if ANonDefaultUVSOffset <> 0 then
    begin
      AStream.Position := AStartPosition + ANonDefaultUVSOffset;
      ANumUVSMappings := AStream.ReadInt;
      SetLength(ANonDefaultUVSTables, ANumUVSMappings);
      for J := 0 to ANumUVSMappings - 1 do
        ANonDefaultUVSTables[J] := TdxNonDefaultUVSTable.Create
          (GetInt24(AStream.ReadArray(3)), AStream.ReadShort);
    end;
    AStream.Position := APos;
    FVariationSelectorRecords[I] :=
      TdxFontFileCMapUnicodeVariationSelectorRecord.Create(AVarSelector,
      ADefaultUVSTables, ANonDefaultUVSTables);
  end;
  AStream.Position := AStartPosition + Size;
end;

destructor TdxFontFileCMapUnicodeVariationSequenciesRecord.Destroy;
var
  Var1: Integer;
begin
  for Var1 :=
    0 to Length(FVariationSelectorRecords) - 1 do
    FVariationSelectorRecords
      [Var1].Free;
  inherited Destroy;
end;

class function TdxFontFileCMapUnicodeVariationSequenciesRecord.GetInt24
  (const AArray: TBytes): Integer;
begin
  if (AArray <> nil) and (Length(AArray) = 3) then
    Result := (AArray[0] shl 16) + (AArray[1] shl 8) + AArray[2]
  else
    Result := 0;
end;

procedure TdxFontFileCMapUnicodeVariationSequenciesRecord.
  Write(ATableStream: TdxFontFileStream);
var
  AOffset: Integer;
  AVarSelectorRecord: TdxFontFileCMapUnicodeVariationSelectorRecord;
  ADTables: TdxDefaultUVSTables;
  ADTable: TdxDefaultUVSTable;
  ANdTables: TdxNonDefaultUVSTables;
  ANdTable: TdxNonDefaultUVSTable;
begin
  inherited Write(ATableStream);
  ATableStream.WriteInt(Size);
  ATableStream.WriteInt(Length(FVariationSelectorRecords));
  AOffset := FHeaderLength + Length(FVariationSelectorRecords) *
    FVariationSelectorRecordSize;
  for AVarSelectorRecord in FVariationSelectorRecords do
    Inc(AOffset, AVarSelectorRecord.Write(ATableStream, AOffset));
  for AVarSelectorRecord in FVariationSelectorRecords do
  begin
    ADTables := AVarSelectorRecord.DefaultUVSTables;
    if ADTables <> nil then
    begin
      ATableStream.WriteInt(Length(ADTables));
      for ADTable in ADTables do
        ADTable.Write(ATableStream);
    end;
    ANdTables := AVarSelectorRecord.NonDefaultUVSTables;
    if ANdTables <> nil then
    begin
      ATableStream.WriteInt(Length(ANdTables));
      for ANdTable in ANdTables do
        ANdTable.Write(ATableStream);
    end;
  end;
end;

function TdxFontFileCMapUnicodeVariationSequenciesRecord.GetFormat
  : TdxFontFileCMapFormatID;
begin
  Result := TdxFontFileCMapFormatID.UnicodeVariationSequences;
end;

function TdxFontFileCMapUnicodeVariationSequenciesRecord.GetSize: Integer;
var
  ALength: Integer;
  AVariationSelectorRecord: TdxFontFileCMapUnicodeVariationSelectorRecord;
  ADTables: TdxDefaultUVSTables;
  ANdTables: TdxNonDefaultUVSTables;
begin
  ALength := FHeaderLength + Length(FVariationSelectorRecords) *
    FVariationSelectorRecordSize;
  for AVariationSelectorRecord in FVariationSelectorRecords do
  begin
    ADTables := AVariationSelectorRecord.DefaultUVSTables;
    if ADTables <> nil then
      Inc(ALength, (4 + Length(ADTables) * FDefaultUVSTableSize));
    ANdTables := AVariationSelectorRecord.NonDefaultUVSTables;
    if ANdTables <> nil then
      Inc(ALength, (4 + Length(ANdTables) * FNonDefaultUVSTableSize));
  end;
  Result := ALength;
end;

{ TdxFontFileCMapTable }

constructor TdxFontFileCMapTable.Create(ASegmentMappingFormatEntry
  : TdxFontFileCMapSegmentMappingRecord);
begin
  inherited Create(nil);
  FCMapTables := TObjectList<TdxFontFileCMapCustomFormatRecord>.Create;
  FMappedGlyphCache := TdxPDFIntegerIntegerDictionary.Create;

  FCMapTables.Add(ASegmentMappingFormatEntry);
  Changed;
end;

constructor TdxFontFileCMapTable.CreateFromCharset
  (ACharset: TdxPDFSmallIntegerDictionary);
var
  ACMapEntry: TdxFontFileCMapSegmentMappingRecord;
begin
  ACMapEntry := TdxFontFileCMapSegmentMappingRecord.CreateFromCharset(ACharset);
  Create(ACMapEntry);
end;

constructor TdxFontFileCMapTable.Create(const AData: TBytes);
var
  ANumberOfEncodingTables: SmallInt;
  I, AOffset: Integer;
  APlatformId: TdxFontFilePlatformID;
  AEncodingId: TdxFontFileEncodingID;
  APosition: Int64;
begin
  inherited Create(AData);
  FCMapTables := TObjectList<TdxFontFileCMapCustomFormatRecord>.Create;
  FMappedGlyphCache := TdxPDFIntegerIntegerDictionary.Create;

  FVersion := DataStream.ReadShort;
  ANumberOfEncodingTables := DataStream.ReadShort;
  FCMapTables.Capacity := ANumberOfEncodingTables;
  for I := 0 to ANumberOfEncodingTables - 1 do
  begin
    APlatformId := TdxFontFilePlatformID(DataStream.ReadShort);
    AEncodingId := TdxFontFileEncodingID(DataStream.ReadShort);
    AOffset := DataStream.ReadInt;
    APosition := DataStream.Position;
    DataStream.Position := AOffset;
    FCMapTables.Add(CreateRecord(APlatformId, AEncodingId,
      TdxFontFileCMapFormatID(DataStream.ReadShort), DataStream));
    DataStream.Position := APosition;
  end;
end;

destructor TdxFontFileCMapTable.Destroy;
begin
  FreeAndNil(FMappedGlyphCache);
  FreeAndNil(FCMapTables);
  inherited Destroy;
end;

class function TdxFontFileCMapTable.Tag: string;
begin
  Result := 'cmap';
end;

{ TdxFontFileCMapByteEncodingRecord }

constructor TdxFontFileCMapByteEncodingRecord.Create
  (APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID;
AStream: TdxFontFileStream);
begin
  inherited Create(APlatformId, AEncodingId, AStream);
  FGlyphIdArray := AStream.ReadArray(BodyLength);
end;

function TdxFontFileCMapByteEncodingRecord.MapCode(ACharacter: Char): Integer;
begin
  if (Integer(ACharacter) > 255) or
    (Integer(ACharacter) >= Length(FGlyphIdArray)) then
    Result := MissingGlyphIndex
  else
    Result := SmallInt(FGlyphIdArray[Integer(ACharacter)]);
end;

procedure TdxFontFileCMapByteEncodingRecord.
  Write(ATableStream: TdxFontFileStream);
begin
  inherited Write(ATableStream);
  ATableStream.WriteArray(FGlyphIdArray);
end;

function TdxFontFileCMapByteEncodingRecord.GetFormat: TdxFontFileCMapFormatID;
begin
  Result := TdxFontFileCMapFormatID.ByteEncoding;
end;

procedure TdxFontFileCMapByteEncodingRecord.UpdateEncoding
  (var AEncoding: TSmallIntDynArray);
var
  ALength, I: Integer;
begin
  ALength := Math.Min(Length(AEncoding), Length(FGlyphIdArray));
  for I := 0 to ALength - 1 do
    UpdateEncodingValue(AEncoding, I, FGlyphIdArray[I]);
end;

{ TdxFontFileCMapHighByteMappingThroughSubHeader }

constructor TdxFontFileCMapHighByteMappingThroughSubHeader.Create(AFirstCode,
  AEntryCount, AIdDelta, AIdRangeOffset: SmallInt; AGlyphOffset: Integer);
begin
  inherited Create;
  FFirstCode := AFirstCode;
  FEntryCount := AEntryCount;
  FIdDelta := AIdDelta;
  FIdRangeOffset := AIdRangeOffset;
  FGlyphOffset := AGlyphOffset;
end;

function TdxFontFileCMapHighByteMappingThroughSubHeader.CalcGlyphIndexArraySize
  (AOffset: Integer): Integer;
begin
  Result := (FIdRangeOffset + FEntryCount * 2 - AOffset) div 2;
end;

{ TdxFontFileCMapHighByteMappingThroughRecord }

constructor TdxFontFileCMapHighByteMappingThroughRecord.Create
  (APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID;
AStream: TdxFontFileStream);
var
  ASubHeaderIndexes: TdxPDFIntegerIntegerDictionary;
  AKey, ASubHeaderCount, AEndOfSubheadersPosition, AOffset,
    AGlyphIndexArrayCount, I: Integer;
  AFirstSubheader, ASubHeader: TdxFontFileCMapHighByteMappingThroughSubHeader;
begin
  inherited Create(APlatformId, AEncodingId, AStream);

  FSubHeaderKeys := AStream.ReadShortArray(256);
  ASubHeaderIndexes := TdxPDFIntegerIntegerDictionary.Create;
  try
    for AKey in FSubHeaderKeys do
      if not ASubHeaderIndexes.ContainsKey(AKey) then
        ASubHeaderIndexes.Add(AKey, AKey);
    ASubHeaderCount := ASubHeaderIndexes.Count;
  finally
    ASubHeaderIndexes.Free;
  end;

  SetLength(FSubHeaders, ASubHeaderCount);
  AEndOfSubheadersPosition := Integer(AStream.Position) + ASubHeaderCount * 8;
  AOffset := ASubHeaderCount * 8 - 6;
  AFirstSubheader := ReadSubHeader(AStream, AEndOfSubheadersPosition);
  AGlyphIndexArrayCount := AFirstSubheader.CalcGlyphIndexArraySize(AOffset);
  FSubHeaders[0] := AFirstSubheader;
  Dec(AOffset, 8);
  for I := 1 to ASubHeaderCount - 1 do
  begin
    ASubHeader := ReadSubHeader(AStream, AEndOfSubheadersPosition);
    FSubHeaders[I] := ASubHeader;
    AGlyphIndexArrayCount := Max(AGlyphIndexArrayCount,
      ASubHeader.CalcGlyphIndexArraySize(AOffset));
    Dec(AOffset, 8);
  end;
  FGlyphIndexArray := AStream.ReadShortArray(AGlyphIndexArrayCount);
end;

destructor TdxFontFileCMapHighByteMappingThroughRecord.Destroy;
var
  I: Integer;
begin
  for I := 0 to Length(FSubHeaders) - 1 do
    FSubHeaders[I].Free;
  inherited Destroy;
end;

function TdxFontFileCMapHighByteMappingThroughRecord.GetFormat
  : TdxFontFileCMapFormatID;
begin
  Result := TdxFontFileCMapFormatID.HighByteMappingThrough;
end;

function TdxFontFileCMapHighByteMappingThroughRecord.GetSize: Integer;
begin
  Result := HeaderLength + SubHeaderKeysLength + SubHeaderLength *
    Length(FSubHeaders) + Length(FGlyphIndexArray) * 2;
end;

procedure TdxFontFileCMapHighByteMappingThroughRecord.UpdateEncoding
  (var AEncoding: TSmallIntDynArray);
var
  AIndex: SmallInt;
begin
  for AIndex := 0 to 256 - 1 do
    if FSubHeaderKeys[AIndex] = 0 then
      UpdateEncodingValue(AEncoding, AIndex, FGlyphIndexArray[AIndex]);
end;

function TdxFontFileCMapHighByteMappingThroughRecord.ReadSubHeader
  (AStream: TdxFontFileStream; AEndOfSubheadersPosition: Integer)
  : TdxFontFileCMapHighByteMappingThroughSubHeader;
var
  AFirstCode, AEntryCount, AIdDelta, AIdRangeOffset: SmallInt;
  APos: Integer;
begin
  AFirstCode := AStream.ReadShort;
  AEntryCount := AStream.ReadShort;
  AIdDelta := AStream.ReadShort;
  APos := Integer(AStream.Position);
  AIdRangeOffset := AStream.ReadShort;
  Result := TdxFontFileCMapHighByteMappingThroughSubHeader.Create(AFirstCode,
    AEntryCount, AIdDelta, AIdRangeOffset,
    (AIdRangeOffset - (AEndOfSubheadersPosition - APos)) div 2);
end;

procedure TdxFontFileCMapHighByteMappingThroughRecord.
  Write(ATableStream: TdxFontFileStream);
var
  ASubHeader: TdxFontFileCMapHighByteMappingThroughSubHeader;
begin
  inherited Write(ATableStream);
  ATableStream.WriteShortArray(FSubHeaderKeys);
  for ASubHeader in FSubHeaders do
  begin
    ATableStream.WriteShort(ASubHeader.FirstCode);
    ATableStream.WriteShort(ASubHeader.EntryCount);
    ATableStream.WriteShort(ASubHeader.IdDelta);
    ATableStream.WriteShort(ASubHeader.IdRangeOffset);
  end;
  ATableStream.WriteShortArray(FGlyphIndexArray);
end;

function TdxFontFileCMapHighByteMappingThroughRecord.MapCode
  (ACharacter: Char): Integer;
var
  AHigh, ALow: Byte;
  ASubheaderIndex, AIndex, P: Integer;
  AGlyph: Word;
  ASubHeader: TdxFontFileCMapHighByteMappingThroughSubHeader;
  AFirstCode: SmallInt;
begin
  AHigh := Byte(ACharacter) shr 8;
  ALow := Byte(ACharacter) and $FF;

  ASubheaderIndex := FSubHeaderKeys[AHigh] div 8;
  if ASubheaderIndex = 0 then
  begin
    AGlyph := Word(FGlyphIndexArray[ALow]);
    if ((FSubHeaders <> nil) and (Length(FSubHeaders) > 0)) and
      (AGlyph <> MissingGlyphIndex) then
      Exit((AGlyph + FSubHeaders[0].IdDelta) mod 65536)
    else
      Exit(AGlyph);
  end;
  if ASubheaderIndex > Length(FSubHeaders) then
    Exit(MissingGlyphIndex);
  ASubHeader := FSubHeaders[ASubheaderIndex];
  AFirstCode := ASubHeader.FirstCode;
  if (AFirstCode > ALow) or (ALow >= AFirstCode + ASubHeader.EntryCount) then
    Exit(MissingGlyphIndex);
  AIndex := ASubHeader.GlyphOffset + ALow;
  if AIndex > Length(FGlyphIndexArray) then
    Exit(MissingGlyphIndex);
  P := Word(FGlyphIndexArray[AIndex]);
  if P <> MissingGlyphIndex then
    Inc(P, ASubHeader.IdDelta);
  Result := P mod 65536;
end;

{ TdxFontFileCMapSegmentMappingRecord }

constructor TdxFontFileCMapSegmentMappingRecord.Create
  (APlatformId: TdxFontFilePlatformID; AEncodingId: TdxFontFileEncodingID;
AStream: TdxFontFileStream);
var
  I, ASeg, AGlyphIdArrayLength: Integer;
begin
  inherited Create(APlatformId, AEncodingId, AStream);
  FSegCount := AStream.ReadShort div 2;
  AStream.ReadShort;
  AStream.ReadShort;
  AStream.ReadShort;
  FEndCode := ReadSegmentsArray(AStream);
  AStream.ReadShort;
  FStartCode := ReadSegmentsArray(AStream);
  FIdDelta := ReadSegmentsArray(AStream);
  FIdRangeOffset := ReadSegmentsArray(AStream);
  AGlyphIdArrayLength := (BodyLength - SegmentsLength) div 2;
  if AGlyphIdArrayLength < 0 then
    AGlyphIdArrayLength := 0;
  SetLength(FGlyphIdArray, AGlyphIdArrayLength);
  for I := 0 to AGlyphIdArrayLength - 1 do
    FGlyphIdArray[I] := AStream.ReadShort;
  SetLength(FSegmentOffsets, FSegCount);
  ASeg := 1;
  for I := 0 to FSegCount - 1 do
  begin
    FSegmentOffsets[I] := (FIdRangeOffset[I] - (FSegCount - ASeg) - 2) div 2;
    Inc(ASeg);
  end;
end;

constructor TdxFontFileCMapSegmentMappingRecord.CreateFromCharset
  (ACharset: TdxPDFSmallIntegerDictionary);
var
  I, ALength, AIndex: Integer;
  AComparison: TComparison<TMap>;
  ACharCode: Char;
  APair: TMap;
  AGID: SmallInt;
  AEncodingUnitKey: Char;
  AEncodingUnitValue: Byte;
  AStandardEncodingUnicodeToSID: TDictionary<Char, Byte>;
  AMapping: TList<TMap>;
begin
  inherited Create(TdxFontFilePlatformID.Microsoft,
    TdxFontFileEncodingID.UGL, 0);

  AMapping := TList<TMap>.Create;
  AStandardEncodingUnicodeToSID := TDictionary<Char, Byte>.Create;
  try
    AStandardEncodingUnicodeToSID.Add(Char(32), 1);
    AStandardEncodingUnicodeToSID.Add(Char(33), 2);
    AStandardEncodingUnicodeToSID.Add(Char(34), 3);
    AStandardEncodingUnicodeToSID.Add(Char(35), 4);
    AStandardEncodingUnicodeToSID.Add(Char(36), 5);
    AStandardEncodingUnicodeToSID.Add(Char(37), 6);
    AStandardEncodingUnicodeToSID.Add(Char(38), 7);
    AStandardEncodingUnicodeToSID.Add(Char(8217), 8);
    AStandardEncodingUnicodeToSID.Add(Char(40), 9);
    AStandardEncodingUnicodeToSID.Add(Char(41), 10);
    AStandardEncodingUnicodeToSID.Add(Char(42), 11);
    AStandardEncodingUnicodeToSID.Add(Char(43), 12);
    AStandardEncodingUnicodeToSID.Add(Char(44), 13);
    AStandardEncodingUnicodeToSID.Add(Char(45), 14);
    AStandardEncodingUnicodeToSID.Add(Char(46), 15);
    AStandardEncodingUnicodeToSID.Add(Char(47), 16);
    AStandardEncodingUnicodeToSID.Add(Char(48), 17);
    AStandardEncodingUnicodeToSID.Add(Char(49), 18);
    AStandardEncodingUnicodeToSID.Add(Char(50), 19);
    AStandardEncodingUnicodeToSID.Add(Char(51), 20);
    AStandardEncodingUnicodeToSID.Add(Char(52), 21);
    AStandardEncodingUnicodeToSID.Add(Char(53), 22);
    AStandardEncodingUnicodeToSID.Add(Char(54), 23);
    AStandardEncodingUnicodeToSID.Add(Char(55), 24);
    AStandardEncodingUnicodeToSID.Add(Char(56), 25);
    AStandardEncodingUnicodeToSID.Add(Char(57), 26);
    AStandardEncodingUnicodeToSID.Add(Char(58), 27);
    AStandardEncodingUnicodeToSID.Add(Char(59), 28);
    AStandardEncodingUnicodeToSID.Add(Char(60), 29);
    AStandardEncodingUnicodeToSID.Add(Char(61), 30);
    AStandardEncodingUnicodeToSID.Add(Char(62), 31);
    AStandardEncodingUnicodeToSID.Add(Char(63), 32);
    AStandardEncodingUnicodeToSID.Add(Char(64), 33);
    AStandardEncodingUnicodeToSID.Add(Char(65), 34);
    AStandardEncodingUnicodeToSID.Add(Char(66), 35);
    AStandardEncodingUnicodeToSID.Add(Char(67), 36);
    AStandardEncodingUnicodeToSID.Add(Char(68), 37);
    AStandardEncodingUnicodeToSID.Add(Char(69), 38);
    AStandardEncodingUnicodeToSID.Add(Char(70), 39);
    AStandardEncodingUnicodeToSID.Add(Char(71), 40);
    AStandardEncodingUnicodeToSID.Add(Char(72), 41);
    AStandardEncodingUnicodeToSID.Add(Char(73), 42);
    AStandardEncodingUnicodeToSID.Add(Char(74), 43);
    AStandardEncodingUnicodeToSID.Add(Char(75), 44);
    AStandardEncodingUnicodeToSID.Add(Char(76), 45);
    AStandardEncodingUnicodeToSID.Add(Char(77), 46);
    AStandardEncodingUnicodeToSID.Add(Char(78), 47);
    AStandardEncodingUnicodeToSID.Add(Char(79), 48);
    AStandardEncodingUnicodeToSID.Add(Char(80), 49);
    AStandardEncodingUnicodeToSID.Add(Char(81), 50);
    AStandardEncodingUnicodeToSID.Add(Char(82), 51);
    AStandardEncodingUnicodeToSID.Add(Char(83), 52);
    AStandardEncodingUnicodeToSID.Add(Char(84), 53);
    AStandardEncodingUnicodeToSID.Add(Char(85), 54);
    AStandardEncodingUnicodeToSID.Add(Char(86), 55);
    AStandardEncodingUnicodeToSID.Add(Char(87), 56);
    AStandardEncodingUnicodeToSID.Add(Char(88), 57);
    AStandardEncodingUnicodeToSID.Add(Char(89), 58);
    AStandardEncodingUnicodeToSID.Add(Char(90), 59);
    AStandardEncodingUnicodeToSID.Add(Char(91), 60);
    AStandardEncodingUnicodeToSID.Add(Char(92), 61);
    AStandardEncodingUnicodeToSID.Add(Char(93), 62);
    AStandardEncodingUnicodeToSID.Add(Char(94), 63);
    AStandardEncodingUnicodeToSID.Add(Char(95), 64);
    AStandardEncodingUnicodeToSID.Add(Char(8216), 65);
    AStandardEncodingUnicodeToSID.Add(Char(97), 66);
    AStandardEncodingUnicodeToSID.Add(Char(98), 67);
    AStandardEncodingUnicodeToSID.Add(Char(99), 68);
    AStandardEncodingUnicodeToSID.Add(Char(100), 69);
    AStandardEncodingUnicodeToSID.Add(Char(101), 70);
    AStandardEncodingUnicodeToSID.Add(Char(102), 71);
    AStandardEncodingUnicodeToSID.Add(Char(103), 72);
    AStandardEncodingUnicodeToSID.Add(Char(104), 73);
    AStandardEncodingUnicodeToSID.Add(Char(105), 74);
    AStandardEncodingUnicodeToSID.Add(Char(106), 75);
    AStandardEncodingUnicodeToSID.Add(Char(107), 76);
    AStandardEncodingUnicodeToSID.Add(Char(108), 77);
    AStandardEncodingUnicodeToSID.Add(Char(109), 78);
    AStandardEncodingUnicodeToSID.Add(Char(110), 79);
    AStandardEncodingUnicodeToSID.Add(Char(111), 80);
    AStandardEncodingUnicodeToSID.Add(Char(112), 81);
    AStandardEncodingUnicodeToSID.Add(Char(113), 82);
    AStandardEncodingUnicodeToSID.Add(Char(114), 83);
    AStandardEncodingUnicodeToSID.Add(Char(115), 84);
    AStandardEncodingUnicodeToSID.Add(Char(116), 85);
    AStandardEncodingUnicodeToSID.Add(Char(117), 86);
    AStandardEncodingUnicodeToSID.Add(Char(118), 87);
    AStandardEncodingUnicodeToSID.Add(Char(119), 88);
    AStandardEncodingUnicodeToSID.Add(Char(120), 89);
    AStandardEncodingUnicodeToSID.Add(Char(121), 90);
    AStandardEncodingUnicodeToSID.Add(Char(122), 91);
    AStandardEncodingUnicodeToSID.Add(Char(123), 92);
    AStandardEncodingUnicodeToSID.Add(Char(124), 93);
    AStandardEncodingUnicodeToSID.Add(Char(125), 94);
    AStandardEncodingUnicodeToSID.Add(Char(126), 95);
    AStandardEncodingUnicodeToSID.Add(Char(161), 96);
    AStandardEncodingUnicodeToSID.Add(Char(162), 97);
    AStandardEncodingUnicodeToSID.Add(Char(163), 98);
    AStandardEncodingUnicodeToSID.Add(Char(8260), 99);
    AStandardEncodingUnicodeToSID.Add(Char(165), 100);
    AStandardEncodingUnicodeToSID.Add(Char(402), 101);
    AStandardEncodingUnicodeToSID.Add(Char(167), 102);
    AStandardEncodingUnicodeToSID.Add(Char(164), 103);
    AStandardEncodingUnicodeToSID.Add(Char(39), 104);
    AStandardEncodingUnicodeToSID.Add(Char(8220), 105);
    AStandardEncodingUnicodeToSID.Add(Char(171), 106);
    AStandardEncodingUnicodeToSID.Add(Char(8249), 107);
    AStandardEncodingUnicodeToSID.Add(Char(8250), 108);
    AStandardEncodingUnicodeToSID.Add(Char(64257), 109);
    AStandardEncodingUnicodeToSID.Add(Char(64258), 110);
    AStandardEncodingUnicodeToSID.Add(Char(8211), 111);
    AStandardEncodingUnicodeToSID.Add(Char(8224), 112);
    AStandardEncodingUnicodeToSID.Add(Char(8225), 113);
    AStandardEncodingUnicodeToSID.Add(Char(183), 114);
    AStandardEncodingUnicodeToSID.Add(Char(182), 115);
    AStandardEncodingUnicodeToSID.Add(Char(8226), 116);
    AStandardEncodingUnicodeToSID.Add(Char(8218), 117);
    AStandardEncodingUnicodeToSID.Add(Char(8222), 118);
    AStandardEncodingUnicodeToSID.Add(Char(8221), 119);
    AStandardEncodingUnicodeToSID.Add(Char(187), 120);
    AStandardEncodingUnicodeToSID.Add(Char(8230), 121);
    AStandardEncodingUnicodeToSID.Add(Char(8240), 122);
    AStandardEncodingUnicodeToSID.Add(Char(191), 123);
    AStandardEncodingUnicodeToSID.Add(Char(96), 124);
    AStandardEncodingUnicodeToSID.Add(Char(180), 125);
    AStandardEncodingUnicodeToSID.Add(Char(710), 126);
    AStandardEncodingUnicodeToSID.Add(Char(732), 127);
    AStandardEncodingUnicodeToSID.Add(Char(175), 128);
    AStandardEncodingUnicodeToSID.Add(Char(728), 129);
    AStandardEncodingUnicodeToSID.Add(Char(729), 130);
    AStandardEncodingUnicodeToSID.Add(Char(168), 131);
    AStandardEncodingUnicodeToSID.Add(Char(730), 132);
    AStandardEncodingUnicodeToSID.Add(Char(184), 133);
    AStandardEncodingUnicodeToSID.Add(Char(733), 134);
    AStandardEncodingUnicodeToSID.Add(Char(731), 135);
    AStandardEncodingUnicodeToSID.Add(Char(711), 136);
    AStandardEncodingUnicodeToSID.Add(Char(8212), 137);
    AStandardEncodingUnicodeToSID.Add(Char(198), 138);
    AStandardEncodingUnicodeToSID.Add(Char(170), 139);
    AStandardEncodingUnicodeToSID.Add(Char(321), 140);
    AStandardEncodingUnicodeToSID.Add(Char(216), 141);
    AStandardEncodingUnicodeToSID.Add(Char(338), 142);
    AStandardEncodingUnicodeToSID.Add(Char(186), 143);
    AStandardEncodingUnicodeToSID.Add(Char(230), 144);
    AStandardEncodingUnicodeToSID.Add(Char(305), 145);
    AStandardEncodingUnicodeToSID.Add(Char(322), 146);
    AStandardEncodingUnicodeToSID.Add(Char(248), 147);
    AStandardEncodingUnicodeToSID.Add(Char(339), 148);
    AStandardEncodingUnicodeToSID.Add(Char(223), 149);
    for AEncodingUnitKey in AStandardEncodingUnicodeToSID.Keys do
      if AStandardEncodingUnicodeToSID.TryGetValue(AEncodingUnitKey,
        AEncodingUnitValue) then
        if ACharset.TryGetValue(AEncodingUnitValue, AGID) then
          AMapping.Add(TMap.Create(AEncodingUnitKey, AGID));

    AComparison := function(const Left, Right: TMap): Integer
      begin
        Result := Ord(Left.CharCode) - Ord(Right.CharCode);
      end;
    AMapping.Sort(TComparer<TMap>.Construct(AComparison));
  finally
    AStandardEncodingUnicodeToSID.Free;
  end;
  ALength := Max(AMapping.Count, 1);
  FSegCount := ALength + 1;
  SetLength(FStartCode, FSegCount);
  SetLength(FEndCode, FSegCount);
  SetLength(FIdDelta, FSegCount);
  SetLength(FIdRangeOffset, FSegCount);
  AIndex := 0;
  for I := 0 to AMapping.Count - 1 do
  begin
    APair := AMapping[I];
    ACharCode := APair.CharCode;
    FStartCode[AIndex] := SmallInt(ACharCode);
    FEndCode[AIndex] := SmallInt(ACharCode);
    FIdDelta[AIndex] := SmallInt(APair.GID - SmallInt(ACharCode));
    Inc(AIndex);
  end;
  FStartCode[ALength] := FinalCode;
  FEndCode[ALength] := FinalCode;
  FIdDelta[ALength] := FinalDelta;
  SetLength(FGlyphIdArray, 0);
  AMapping.Free;
end;

procedure TdxFontFileGlyphTable.DoApplyChanges;
var
  I, APos, AEnd: Integer;
  ASubsetGlyph: TdxFontFileSubsetGlyph;
begin
  inherited DoApplyChanges;
  for ASubsetGlyph in FSubsetGlyphs do
  begin
    DataStream.Position := FGlyphOffsets[ASubsetGlyph.Index];
    ASubsetGlyph.Description.Write(DataStream);
    APos := Integer(DataStream.Position);
    AEnd := Pad4(APos);
    for I := APos to AEnd - 1 do
      DataStream.WriteByte(0);
  end;
end;

{ TdxFontFile }

constructor TdxFontFile.Create(AStream: TdxFontFileStream);
begin
  Create(AStream.Data);
end;

constructor TdxFontFile.Create(const AData: TBytes;
AIsOpenType: Boolean = False);
var
  AStream: TdxFontFileStream;
begin
  inherited Create;
  SetLength(FVersion, 4);
  if AIsOpenType then
  begin
    FVersion[0] := OpenTypeVersion[0];
    FVersion[1] := OpenTypeVersion[1];
    FVersion[2] := OpenTypeVersion[2];
    FVersion[3] := OpenTypeVersion[3];
  end
  else
  begin
    FVersion[0] := TrueTypeVersion[0];
    FVersion[1] := TrueTypeVersion[1];
    FVersion[2] := TrueTypeVersion[2];
    FVersion[3] := TrueTypeVersion[3];
  end;
  FTableDictionary := TObjectDictionary<string, TdxFontFileBinaryTable>.Create
    ([doOwnsValues]);
  if Length(AData) > 0 then
  begin
    AStream := TdxFontFileStream.Create(AData);
    try
      ReadTables(AStream);
    finally
      AStream.Free;
    end;
  end;
end;

destructor TdxFontFile.Destroy;
begin
  FreeAndNil(FTableDictionary);
  inherited Destroy;
end;

class function TdxFontFile.GetCFFData(const AFontFileData: TBytes): TBytes;
var
  ALength: Integer;
  AFile: TdxFontFile;
  ACFF: TdxFontFileBinaryTable;
begin
  Result := nil;
  ALength := Length(AFontFileData);
  if (ALength >= Length(OpenTypeVersion)) and
    (AFontFileData[0] = OpenTypeVersion[0]) and
    (AFontFileData[1] = OpenTypeVersion[1]) and
    (AFontFileData[2] = OpenTypeVersion[2]) and
    (AFontFileData[3] = OpenTypeVersion[3]) then
  begin
    AFile := TdxFontFile.Create(AFontFileData, True);
    try
      if (AFile <> nil) and AFile.Table.TryGetValue(TdxFontFileCFFTable.Tag,
        ACFF) then
        Result := ACFF.Data;
    finally
      AFile.Free;
    end;
  end;
end;

class function TdxFontFile.IsEqual(AFontFile1, AFontFile2: TdxFontFile)
  : Boolean;
begin
  if (AFontFile1 = nil) and (AFontFile2 = nil) then
    Exit(False);
  if ((AFontFile1 = nil) and (AFontFile2 <> nil)) or
    ((AFontFile1 <> nil) and (AFontFile2 = nil)) then
    Exit(False);
  Result := AFontFile1.InitalFontSize = AFontFile2.InitalFontSize;
end;

function TdxFontFile.CreateSubset(AMapping
  : TdxPDFIntegerStringDictionary): TBytes;
var
  AList: TStringList;
begin
  SetLength(Result, 0);
  if GlyphTable <> nil then
    GlyphTable.CreateSubset(Self, AMapping);
  AList := TStringList.Create;
  try
    AList.Add(TdxFontFileHeadTable.Tag);
    AList.Add(TdxFontFileHheaTable.Tag);
    AList.Add(TdxFontFileMaxpTable.Tag);
    AList.Add(TdxFontFileHmtxTable.Tag);
    AList.Add(TdxFontFileGlyphTable.Tag);
    AList.Add(TdxFontFileLocaTable.Tag);
    AList.Add('cvt ');
    AList.Add('fpgm');
    AList.Add('prep');
    Result := InternalGetData(AList);
  finally
    AList.Free;
  end;
end;

function TdxFontFile.GetCharacterWidth(AGlyphIndex: Integer): Single;
var
  AGlyphCount: Integer;
begin
  if HmtxTable = nil then
    Exit(0);
  if HmtxTable.AdvanceWidths = nil then
  begin
    if HheaTable = nil then
      Exit(0);
    if MaxpTable = nil then
      AGlyphCount := 0
    else
      AGlyphCount := MaxpTable.NumGlyphs;
    HmtxTable.AdvanceWidths := HmtxTable.FillAdvanceWidths
      (HheaTable.NumberOfHMetrics, AGlyphCount);
  end;
  if AGlyphIndex < Length(HmtxTable.AdvanceWidths) then
    Result := HmtxTable.AdvanceWidths[AGlyphIndex] * FTTFToFontFileFactor
  else
    Result := 0;
end;

function TdxFontFile.GetData: TBytes;
var
  AKey: string;
  AList: TStringList;
begin
  AList := TStringList.Create;
  try
    for AKey in Table.Keys do
      AList.Add(AKey);
    Result := InternalGetData(AList);
  finally
    AList.Free;
  end;
end;

function TdxFontFile.IsTrueTypeFont: Boolean;
begin
  Result := GlyphTable <> nil;
end;

procedure TdxFontFile.AddTable(ATable: TdxFontFileBinaryTable);
begin
  FTableDictionary.Add(ATable.Name, ATable);
end;

function TdxFontFile.GetHeadTable: TdxFontFileHeadTable;
var
  AValue: TdxFontFileBinaryTable;
begin
  if Table.TryGetValue(TdxFontFileHeadTable.Tag, AValue) then
    Result := AValue as TdxFontFileHeadTable
  else
    Result := nil;
end;

function TdxFontFile.GetMaxpTable: TdxFontFileMaxpTable;
var
  AValue: TdxFontFileBinaryTable;
begin
  if Table.TryGetValue(TdxFontFileMaxpTable.Tag, AValue) then
    Result := AValue as TdxFontFileMaxpTable
  else
    Result := nil;
end;

function TdxFontFile.GetOS2Table: TdxFontFileOS2Table;
var
  AValue: TdxFontFileBinaryTable;
begin
  if Table.TryGetValue(TdxFontFileOS2Table.Tag, AValue) then
    Result := AValue as TdxFontFileOS2Table
  else
    Result := nil;
end;

function TdxFontFile.GetPostTable: TdxFontFilePostTable;
var
  AValue: TdxFontFileBinaryTable;
begin
  if Table.TryGetValue(TdxFontFilePostTable.Tag, AValue) then
    Result := AValue as TdxFontFilePostTable
  else
    Result := nil;
end;

function TdxFontFile.GetNameTable: TdxFontFileNameTable;
var
  AValue: TdxFontFileBinaryTable;
begin
  if Table.TryGetValue(TdxFontFileNameTable.Tag, AValue) then
    Result := AValue as TdxFontFileNameTable
  else
    Result := nil;
end;

function TdxFontFile.GetLocaTable: TdxFontFileLocaTable;
var
  AValue: TdxFontFileBinaryTable;
begin
  if Table.TryGetValue(TdxFontFileLocaTable.Tag, AValue) then
    Result := AValue as TdxFontFileLocaTable
  else
    Result := nil;
end;

function TdxFontFile.GetGlyphTable: TdxFontFileGlyphTable;
var
  AValue: TdxFontFileBinaryTable;
begin
  if Table.TryGetValue(TdxFontFileGlyphTable.Tag, AValue) then
    Result := AValue as TdxFontFileGlyphTable
  else
    Result := nil;
end;

function TdxFontFile.GetHheaTable: TdxFontFileHheaTable;
var
  AValue: TdxFontFileBinaryTable;
begin
  if FHhea = nil then
    if Table.TryGetValue(TdxFontFileHheaTable.Tag, AValue) then
      FHhea := AValue as TdxFontFileHheaTable
    else
      FHhea := nil;
  Result := FHhea;
end;

function TdxFontFile.GetCMapTable: TdxFontFileCMapTable;
var
  AValue: TdxFontFileBinaryTable;
begin
  if FCMap = nil then
    if Table.TryGetValue(TdxFontFileCMapTable.Tag, AValue) then
      FCMap := AValue as TdxFontFileCMapTable
    else
      FCMap := nil;
  Result := FCMap;
end;

function TdxFontFile.GetKernTable: TdxFontFileKernTable;
var
  AValue: TdxFontFileBinaryTable;
begin
  if FKern = nil then
    if Table.TryGetValue(TdxFontFileKernTable.Tag, AValue) then
      FKern := AValue as TdxFontFileKernTable
    else
      FKern := nil;
  Result := FKern;
end;

function TdxFontFile.GetHmtxTable: TdxFontFileHmtxTable;
var
  AValue: TdxFontFileBinaryTable;
begin
  if FHmtx = nil then
    if Table.TryGetValue(TdxFontFileHmtxTable.Tag, AValue) then
      FHmtx := AValue as TdxFontFileHmtxTable
    else
      FHmtx := nil;
  Result := FHmtx;
end;

procedure TdxFontFile.ReadTables(AStream: TdxFontFileStream);
var
  AStartOffset, AStreamLength, AOffset, ACurrentPosition: Int64;
  ANumTables, I: SmallInt;
  ATag: string;
  ALength: Integer;
begin
  FInitalFontSize := AStream.Size;
  AStartOffset := AStream.Position;
  AStream.ReadInt;
  ANumTables := AStream.ReadShort;
  AStream.ReadShort;
  AStream.ReadShort;
  AStream.Position := AStartOffset + TableDirectoryOffset;
  AStreamLength := AStream.Size;
  for I := 0 to ANumTables - 1 do
  begin
    ATag := AStream.ReadString(4);
    AStream.ReadInt;
    AOffset := AStream.ReadInt;
    ALength := AStream.ReadInt;
    if (ATag = TdxFontFileGlyphTable.Tag) and (ALength = 0) then
      ALength := AStreamLength - AOffset;
    if (AOffset > 0) and (ALength > 0) and (AOffset + ALength <= AStreamLength)
      and (ATag <> 'EBLC') then
    begin
      ACurrentPosition := AStream.Position;
      AStream.Position := AOffset;
      AddTable(CreateTable(ATag, AStream.ReadArray(ALength)));
      AStream.Position := ACurrentPosition;
    end;
  end;
  if HeadTable <> nil then
    FTTFToFontFileFactor := 1000 / HeadTable.UnitsPerEm;
  if LocaTable <> nil then
    LocaTable.ReadOffsets(Self);
  if GlyphTable <> nil then
    GlyphTable.ReadGlyphs(Self);
end;

function TdxFontFile.CreateTable(const ATag: string; const AArray: TBytes)
  : TdxFontFileBinaryTable;
var
  AClass: TdxFontFileTableClass;
begin
  if (Length(AArray) = 0) or not dxgFontFileSupportedTables.TryGetValue(ATag,
    AClass) then
    AClass := TdxFontFileBinaryTable;
  Result := AClass.Create(AArray);
  Result.Name := ATag;
end;

function TdxFontFile.InternalGetData(ATablesToWrite: TStringList): TBytes;
var
  I, ANumTables, ASearchRange: SmallInt;
  AEntry: TPair<string, TdxFontFileBinaryTable>;
  AEntryOffset: Integer;
  AStream: TdxFontFileStream;
  ACurrentTable: TdxFontFileBinaryTable;
begin
  ATablesToWrite.Sort;
  ANumTables := 0;
  for AEntry in FTableDictionary do
    if ATablesToWrite.IndexOf(AEntry.Key) <> -1 then
      Inc(ANumTables);

  AEntryOffset := TableDirectoryOffset + ANumTables * 16;
  AStream := TdxFontFileStream.Create;
  try
    AStream.WriteArray(FVersion);
    AStream.WriteShort(ANumTables);
    ASearchRange := Trunc(Power(2, Floor(Log2(ANumTables))));
    AStream.WriteShort(ASearchRange * 16);
    AStream.WriteShort(Trunc(Log2(ANumTables)));
    AStream.WriteShort(SmallInt((ANumTables * 16 - ASearchRange * 16)));

    for I := 0 to ATablesToWrite.Count - 1 do
      if FTableDictionary.TryGetValue(ATablesToWrite[I], ACurrentTable) then
        Inc(AEntryOffset, ACurrentTable.Write(AStream, AEntryOffset));

    for I := 0 to ATablesToWrite.Count - 1 do
      if FTableDictionary.TryGetValue(ATablesToWrite[I], ACurrentTable) then
        AStream.WriteArray(ACurrentTable.AlignedTableData);

    AStream.Position := 0;
    Result := AStream.ReadArray(Integer(AStream.Size));
  finally
    AStream.Free;
  end;
end;

{ TdxFontFileFontMetrics }

class function TdxFontFileFontMetrics.Create(AFontFile: TdxFontFile)
  : TdxFontFileFontMetrics;
var
  AUnitsPerEm, AFactor: Double;
  AHead: TdxFontFileHeadTable;
  AOs2: TdxFontFileOS2Table;
  AHhea: TdxFontFileHheaTable;
begin
  AHead := AFontFile.HeadTable;
  if AHead = nil then
  begin
    AUnitsPerEm := 2048;
    Result.FEmBBox := TdxRectF.CreateSize(0, 0, 0, 0);
  end
  else
  begin
    AUnitsPerEm := AHead.UnitsPerEm;
    AFactor := 1000 / AUnitsPerEm;
    Result.FEmBBox := TdxRectF.CreateSize(Round(AHead.XMin * AFactor),
      Round(AHead.YMin * AFactor), Round(AHead.XMax * AFactor),
      Round(AHead.YMax * AFactor));
  end;
  AOs2 := AFontFile.OS2Table;
  AHhea := AFontFile.HheaTable;
  if AOs2 <> nil then
  begin
    if AOs2.UseTypoMetrics then
    begin
      Result.FAscent := AOs2.TypoAscender / AUnitsPerEm;
      Result.FDescent := -AOs2.TypoDescender / AUnitsPerEm;
      Result.FLineSpacing := Result.FAscent + Result.FDescent + AOs2.TypoLineGap
        / AUnitsPerEm;
    end
    else
    begin
      Result.FAscent := AOs2.WinAscent / AUnitsPerEm;
      Result.FDescent := AOs2.WinDescent / AUnitsPerEm;
      Result.FLineSpacing := Result.FAscent + Result.FDescent;
      if AHhea <> nil then
        Result.FLineSpacing := Result.FLineSpacing +
          TdxPDFUtils.Max(0,
          (AHhea.LineGap - ((AOs2.WinAscent + AOs2.WinDescent) - (AHhea.Ascender
          - AHhea.Descender))) / AUnitsPerEm);
    end;
  end
  else if AHhea <> nil then
  begin
    Result.FAscent := AHhea.Ascender / AUnitsPerEm;
    Result.FDescent := -AHhea.Descender / AUnitsPerEm;
    Result.FLineSpacing := Result.FAscent + Result.FDescent + AHhea.LineGap /
      AUnitsPerEm;
  end;
end;

class function TdxFontFileFontMetrics.Create(AAscent, ADescent, ALineSpacing,
  AUnitsPerEm: Double): TdxFontFileFontMetrics;
begin
  Result.FAscent := AAscent / AUnitsPerEm;
  Result.FDescent := ADescent / AUnitsPerEm;
  Result.FLineSpacing := ALineSpacing / AUnitsPerEm;
  Result.FEmBBox := TdxRectF.CreateSize(0, 0, 0, 0);
end;

function TdxFontFileFontMetrics.GetEmAscent: Double;
begin
  Result := FAscent * 1000;
end;

function TdxFontFileFontMetrics.GetEmDescent: Double;
begin
  Result := FDescent * 1000;
end;

function TdxFontFileFontMetrics.GetAscent(AFontSize: Double): Double;
begin
  Result := FAscent * AFontSize;
end;

function TdxFontFileFontMetrics.GetDescent(AFontSize: Double): Double;
begin
  Result := FDescent * AFontSize;
end;

function TdxFontFileFontMetrics.GetLineSpacing(AFontSize: Double): Double;
begin
  Result := FLineSpacing * AFontSize;
end;

initialization

dxgFontFileUnicodeConverter := TdxFontFileUnicodeConverter.Create;
dxgFontFileSupportedTables := TDictionary<string, TdxFontFileTableClass>.Create;

dxgFontFileSupportedTables.Add(TdxFontFileCMapTable.Tag, TdxFontFileCMapTable);
dxgFontFileSupportedTables.Add(TdxFontFileCFFTable.Tag, TdxFontFileCFFTable);
dxgFontFileSupportedTables.Add(TdxFontFileGlyphTable.Tag,
  TdxFontFileGlyphTable);
dxgFontFileSupportedTables.Add(TdxFontFileHeadTable.Tag, TdxFontFileHeadTable);
dxgFontFileSupportedTables.Add(TdxFontFileHheaTable.Tag, TdxFontFileHheaTable);
dxgFontFileSupportedTables.Add(TdxFontFileHmtxTable.Tag, TdxFontFileHmtxTable);
dxgFontFileSupportedTables.Add(TdxFontFileLocaTable.Tag, TdxFontFileLocaTable);
dxgFontFileSupportedTables.Add(TdxFontFileKernTable.Tag, TdxFontFileKernTable);
dxgFontFileSupportedTables.Add(TdxFontFileMaxpTable.Tag, TdxFontFileMaxpTable);
dxgFontFileSupportedTables.Add(TdxFontFileNameTable.Tag, TdxFontFileNameTable);
dxgFontFileSupportedTables.Add(TdxFontFileOS2Table.Tag, TdxFontFileOS2Table);
dxgFontFileSupportedTables.Add(TdxFontFilePostTable.Tag, TdxFontFilePostTable);

finalization

FreeAndNil(dxgFontFileUnicodeConverter);
FreeAndNil(dxgFontFileSupportedTables);

end.
