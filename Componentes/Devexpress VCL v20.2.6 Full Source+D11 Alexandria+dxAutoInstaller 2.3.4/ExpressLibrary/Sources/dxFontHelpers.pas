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

unit dxFontHelpers; // for internal use

interface

{$I cxVer.inc}
{$IFDEF CPUX64}
{$OPTIMIZATION OFF}
{$ENDIF CPUX64}

uses
  Types, Windows, Graphics, dxCoreClasses, Classes, Generics.Defaults,
  Generics.Collections,
  cxClasses, dxCoreGraphics, dxDocumentLayoutUnitConverter, cxGraphics;

type
  TdxFontInfo = class;
  TdxFontCache = class;
  TdxFontInfoMeasurer = class;

  TdxTextDirection = (LeftToRightTopToBottom = 0, TopToBottomRightToLeft = 1,
    TopToBottomLeftToRightRotated = 2, BottomToTopLeftToRight = 3,
    LeftToRightTopToBottomRotated = 4, TopToBottomRightToLeftRotated = 5);

  TdxVerticalAlignment = (Top = 0, Both = 1, Center = 2, Bottom = 3);

  TdxUnicodeSubrange = record
    LowValue: Char;
    HiValue: Char;
    Bit: Integer;
    function ContainsChar(ACharacter: Char): Boolean;
  end;

  PdxUnicodeSubrange = ^TdxUnicodeSubrange;

  TdxUnicodeRangeInfo = class
  private
    FCapacity: Integer;
    FCount: Integer;
    FRanges: TArray<TdxUnicodeSubrange>;
    procedure SetCapacity(AValue: Integer);
  protected
    procedure AddSubrange(AStartCharacter, AEndCharacter: Char; ABit: Integer);
    procedure Grow;
    procedure PopulateSubranges;
  public
    constructor Create;
    destructor Destroy; override;
    function LookupSubrange(ACharacter: Char): PdxUnicodeSubrange;

    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount;
  end;

  { TdxFontCharacterSet }

  TdxFontCharacterRange = record
    Low: Integer;
    Hi: Integer;
    procedure CopyFrom(ALow, AHi: Integer); overload;
    procedure CopyFrom(const ARange: TWCRange); overload;
  end;

  TdxFontCharacterSet = class
  strict private
    FBits: TBits;
    procedure AddRange(const AFontCharacterRange: TdxFontCharacterRange);
  protected
    FPanose: TPanose;
    property Bits: TBits read FBits;
  public
    constructor Create(const ARanges: TArray<TdxFontCharacterRange>;
      const APanose: TPanose);
    destructor Destroy; override;
    class function CalculatePanoseDistance(const AFontCharacterSet1,
      AFontCharacterSet2: TdxFontCharacterSet): Integer;
    function ContainsChar(C: Char): Boolean; inline;
  end;

  TdxSupportedFontStyle = (Regular, Bold, Italic, Underline, Strikeout,
    BoldItalic);
  TdxSupportedFontStyles = set of TdxSupportedFontStyle;

  TdxSupportedFontStylesInfo = record
    SupportedStyles: TdxSupportedFontStyles;
    NativeStyles: array [TdxSupportedFontStyle] of Boolean;
  end;

  { IdxFontsContainer }

  IdxFontsContainer = interface
    function GetFontCache: TdxFontCache;

    property FontCache: TdxFontCache read GetFontCache;
  end;

  { TdxFontInfo }

  TdxFontInfo = class
{$REGION 'internal types'}
  protected type

    TCharacterDrawingAbilityTable = class
    strict private
      FDrawingAbility: TBits;
      FCalculated: TBits;
    public
      constructor Create;
      destructor Destroy; override;
      procedure SetDrawingAbility(ACharacter: Char; AValue: Boolean);
      function GetDrawingAbility(ACharacter: Char;
        out AAbility: Boolean): Boolean;
    end;

    TGdiSubrangeBits = array [0 .. 3] of DWORD;

    TUnicodeSubrangeBits = record
    private const
      BitPerDWORD = 8 * SizeOf(DWORD);
    public
      Data: TGdiSubrangeBits;
      function GetBit(AIndex: Integer): Boolean; inline;
      procedure SetBit(AIndex: Integer; const Value: Boolean); inline;
      procedure Clear;

      property Bits[Index: Integer]: Boolean read GetBit write SetBit;
    end;
{$ENDREGION}
  strict private
    FApplyCjkUnderline: Boolean;
    FAscent: Integer;
    FBaseFontIndex: Integer;
    FCharacterDrawingAbilityTable: TCharacterDrawingAbilityTable;
    FCharset: Integer;
    FCjkUnderlinePosition: Integer;
    FCjkUnderlineSize: Integer;
    FDashWidth: Integer;
    FDescent: Integer;
    FDotWidth: Integer;
    FDrawingOffset: Integer;
    FEqualSignWidth: Integer;
    FFamilyName: string;
    FFree: Integer;
    FGdiFontHandle: THandle;
    FGdiOffset: Integer;
    FIsCJKFont: Boolean;
    FLineSpacing: Integer;
    FMaxDigitWidth: Single;
    FMiddleDotWidth: Integer;
    FName: string;
    FNonBreakingSpaceWidth: Integer;
    FPanose: TPanose;
    FPilcrowSignWidth: Integer;
    FSizeInPoints: Single;
    FSpaceWidth: Integer;
    FStrikeoutPosition: Integer;
    FStrikeoutThickness: Integer;
    FStyle: TFontStyles;
    FSubscriptOffset: TPoint;
    FSubscriptSize: TSize;
    FSuperscriptOffset: TPoint;
    FSuperscriptSize: TSize;
    FSupportedStyles: TdxSupportedFontStyles;
    FTrueType: Boolean;
    FUnderlinePosition: Integer;
    FUnderlineThickness: Integer;
    FUnderscoreWidth: Integer;
    FUnicodeSubrangeBits: TGdiSubrangeBits;
    FUnicodeSubrangeBitsCalculated: Boolean;
    FUseGetGlyphIndices: Boolean;
    function GetAscentAndFree: Integer;
    function GetBold: Boolean; inline;
    function GetItalic: Boolean; inline;
    function GetUnderline: Boolean; inline;
  strict protected
    procedure AdjustFontParameters; virtual;
    function CalculateCanDrawCharacter(AUnicodeRangeInfo: TdxUnicodeRangeInfo;
      ACanvas: TCanvas; ACharacter: Char): Boolean; virtual;
    procedure CalculateCJKFontParameters(AOutlineTextMetric
      : POutlineTextmetric);
    function CalculateFontCharset(AMeasurer: TdxFontInfoMeasurer): Integer;
    procedure CalculateFontParameters(AMeasurer: TdxFontInfoMeasurer;
      AAllowCjkCorrection, AIsOfficeFont: Boolean); virtual;
    function CalculateFontSizeInLayoutUnits(AFontUnit: TdxGraphicUnit;
      AUnitConverter: TdxDocumentLayoutUnitConverter): Single;
    procedure CalculateFontVerticalParameters(AMeasurer: TdxFontInfoMeasurer;
      AAllowCjkCorrection, AIsOfficeFont: Boolean);
    function CalculateMaxDigitWidth(AMeasurer: TdxFontInfoMeasurer): Single;
    function CalculateSupportedUnicodeSubrangeBits(AUnicodeRangeInfo
      : TdxUnicodeRangeInfo; ACanvas: TCanvas)
      : TdxFontInfo.TUnicodeSubrangeBits; virtual;
    procedure CalculateUnderlineAndStrikeoutParameters(const AOutlineTextMetric
      : TOutlineTextmetric);
    procedure CreateFont(AMeasurer: TdxFontInfoMeasurer; const AName: string;
      ASize: Integer; const AFontStyle: TFontStyles;
      AIsOfficeFont: Boolean); virtual;
    function GetBaseFontInfo(const AContainer: IdxFontsContainer)
      : TdxFontInfo; virtual;
  public
    constructor Create(AMeasurer: TdxFontInfoMeasurer; const AName: string;
      ADoubleSize: Integer; const AFontStyle: TFontStyles;
      AAllowCjkCorrection: Boolean = True; AIsOfficeFont: Boolean = True);
    destructor Destroy; override;
    procedure CalculateSubscriptOffset(ABaseFontInfo: TdxFontInfo);
    procedure CalculateSuperscriptOffset(ABaseFontInfo: TdxFontInfo);
    function CanDrawCharacter(AUnicodeRangeInfo: TdxUnicodeRangeInfo;
      ACanvas: TCanvas; ACharacter: Char): Boolean; virtual;
    function GetBaseAscentAndFree(const AContainer: IdxFontsContainer): Integer;
    function GetBaseDescent(const AContainer: IdxFontsContainer): Integer;
    class function GetFontUnicodeRanges(ADC: HDC; AFont: HFONT)
      : TArray<TdxFontCharacterRange>; static;

    property GdiFontHandle: THandle read FGdiFontHandle;
    property Panose: TPanose read FPanose;
    property UseGetGlyphIndices: Boolean read FUseGetGlyphIndices;
    property Ascent: Integer read FAscent;
    property AscentAndFree: Integer read GetAscentAndFree;
    property BaseFontIndex: Integer read FBaseFontIndex write FBaseFontIndex;
    property Bold: Boolean read GetBold;
    property Charset: Integer read FCharset;
    property DashWidth: Integer read FDashWidth;
    property Descent: Integer read FDescent;
    property DotWidth: Integer read FDotWidth;
    property DrawingOffset: Integer read FDrawingOffset write FDrawingOffset;
    property EqualSignWidth: Integer read FEqualSignWidth;
    property FamilyName: string read FFamilyName;
    property _Free: Integer read FFree;
    property GdiOffset: Integer read FGdiOffset write FGdiOffset;
    property IsCJKFont: Boolean read FIsCJKFont;
    property Italic: Boolean read GetItalic;
    property LineSpacing: Integer read FLineSpacing;
    property MaxDigitWidth: Single read FMaxDigitWidth;
    property MiddleDotWidth: Integer read FMiddleDotWidth;
    property Name: string read FName;
    property NonBreakingSpaceWidth: Integer read FNonBreakingSpaceWidth;
    property PilcrowSignWidth: Integer read FPilcrowSignWidth;
    property SizeInPoints: Single read FSizeInPoints;
    property SpaceWidth: Integer read FSpaceWidth;
    property StrikeoutPosition: Integer read FStrikeoutPosition;
    property StrikeoutThickness: Integer read FStrikeoutThickness;
    property Style: TFontStyles read FStyle;
    property SubscriptOffset: TPoint read FSubscriptOffset;
    property SubscriptSize: TSize read FSubscriptSize;
    property SuperscriptOffset: TPoint read FSuperscriptOffset;
    property SuperscriptSize: TSize read FSuperscriptSize;
    property SupportedStyles: TdxSupportedFontStyles read FSupportedStyles;
    property Underline: Boolean read GetUnderline;
    property UnderlinePosition: Integer read FUnderlinePosition;
    property UnderlineThickness: Integer read FUnderlineThickness;
    property UnderscoreWidth: Integer read FUnderscoreWidth;
  end;

  { TdxFontInfoMeasurer }

  TdxFontInfoMeasurer = class
  private
    FDpi: Single;
    FCanvas: TCanvas;
    FUnitConverter: TdxDocumentLayoutUnitConverter;
  strict private
    function CreateMeasureGraphics: TCanvas;
  public
    constructor Create(AUnitConverter: TdxDocumentLayoutUnitConverter); virtual;
    destructor Destroy; override;
    function MeasureCharacterWidth(ACharacter: Char;
      AFontInfo: TdxFontInfo): Integer;
    function MeasureCharacterWidthF(ACharacter: Char;
      AFontInfo: TdxFontInfo): Single;
    function MeasureString(const AText: string; AFontInfo: TdxFontInfo): TSize;
    function MeasureMaxDigitWidthF(AFontInfo: TdxFontInfo): Single;

    property Canvas: TCanvas read FCanvas;
    property UnitConverter: TdxDocumentLayoutUnitConverter read FUnitConverter;
  end;

  { TdxTrueTypeFontInfo }

  TdxTrueTypeFontInfo = class
  public const
    SymbolFontType = 256;
    FixedPitchFontType = 512;
  private
    FFontName: string;
    FFontType: Integer;
    FSupportedStylesCalculated: Boolean;
    FSupportedStyles: TdxSupportedFontStylesInfo;
    function GetSupportedFontStyles: TdxSupportedFontStylesInfo;
    function GetStylesInfo: TdxSupportedFontStylesInfo;
  public
    constructor Create(const AFontName: string; const AFontType: Integer);
    property FontName: string read FFontName;
    property FontType: Integer read FFontType;
    property StylesInfo: TdxSupportedFontStylesInfo read GetStylesInfo;
  end;

  TdxSystemTrueTypeFontDictionary = TObjectDictionary<string,
    TdxTrueTypeFontInfo>;

  { TdxFontCache }

  TdxCharacterFormattingScript = (Normal, Subscript, Superscript);

  TdxFontCache = class
  public const
    SymbolFontType = 256;
    FixedPitchFontType = 512;
{$REGION 'internal types'}
  strict private
  type

    TTrueTypeFontLoader = class(TcxThread)
    private
      class function EnumFontInfoProc(var ALogFont: TLogFontW;
        ATextMetric: PTextMetricW; AFontType: Integer; AData: Pointer): Integer;
        stdcall; static;
      function CreateFontCharacterSet(ADC: HDC; const AFontName: string)
        : TdxFontCharacterSet;
      function CanContinue: Boolean;
    protected
      procedure Execute; override;
    end;

    TCallbackData = record
      Thread: TTrueTypeFontLoader;
    end;

    PCallbackData = ^TCallbackData;

{$ENDREGION}
  strict private
  class var
    FLoader: TTrueTypeFontLoader;
    FNameToCharacterSetMap: TObjectDictionary<string, TdxFontCharacterSet>;
    FSystemTrueTypeFonts: TdxSystemTrueTypeFontDictionary;
    FUnicodeRangeInfo: TdxUnicodeRangeInfo;
  strict private
    FCharsets: TDictionary<string, Integer>;
    FIndexHash: TDictionary<Int64, Integer>;
    FItems: TdxFastObjectList;
    FMeasurer: TdxFontInfoMeasurer;
    FUnitConverter: TdxDocumentLayoutUnitConverter;
    FUseOfficeFonts: Boolean;
    function GetCount: Integer; inline;
    function GetItem(AIndex: Integer): TdxFontInfo; inline;
  protected
    class procedure Initialize;
    class procedure Finalize;
    function CreateOverrideFontSubstitutes
      : TdxSystemTrueTypeFontDictionary; virtual;
    function CreateFontInfoMeasurer(AUnitConverter
      : TdxDocumentLayoutUnitConverter): TdxFontInfoMeasurer; virtual;
    function CreateFontInfoCore(const AFontName: string;
      ADoubleFontSize: Integer; const AFontStyle: TFontStyles): TdxFontInfo;
    function CalcSubscriptFontSize(ABaseFontIndex: Integer): Integer;
    function CalcSuperscriptFontSize(ABaseFontIndex: Integer): Integer;
    function CalcSuperscriptFontIndex(const AFontName: string;
      ADoubleFontSize: Integer; const AFontStyle: TFontStyles): Integer;
    function CalcSubscriptFontIndex(const AFontName: string;
      ADoubleFontSize: Integer; const AFontStyle: TFontStyles): Integer;
    function CalcFontIndexCore(const AFontName: string;
      ADoubleFontSize: Integer; const AFontStyle: TFontStyles;
      AScript: TdxCharacterFormattingScript): Integer;
    function CalcHash(const AFontName: string; ADoubleFontSize: Integer;
      const AFontStyle: TFontStyles;
      AScript: TdxCharacterFormattingScript): Int64;
    function CreateFontInfo(const AHash: Int64; const AFontName: string;
      ADoubleFontSize: Integer; const AFontStyle: TFontStyles): Integer;
    function CreateFontCharacterSet(const AFontName: string)
      : TdxFontCharacterSet;
    function GetFontCharacterRanges(AFontInfo: TdxFontInfo)
      : TArray<TdxFontCharacterRange>;

    property IndexHash: TDictionary<Int64, Integer> read FIndexHash;
    class property NameToCharacterSetMap
      : TObjectDictionary<string, TdxFontCharacterSet>
      read FNameToCharacterSetMap;
    class property UnicodeRangeInfo: TdxUnicodeRangeInfo read FUnicodeRangeInfo;
  public
    constructor Create(const AUnit: TdxDocumentLayoutUnit; const ADpi: Single;
      AUseOfficeFonts: Boolean); virtual;
    destructor Destroy; override;
    function CalcFontIndex(const AFontName: string; ADoubleFontSize: Integer;
      const AFontStyle: TFontStyles; AScript: TdxCharacterFormattingScript)
      : Integer; virtual;
    function CalcNormalFontIndex(const AFontName: string;
      ADoubleFontSize: Integer; const AFontStyle: TFontStyles)
      : Integer; virtual;
    class procedure CheckInitialize;
    class function CreateSystemTrueTypeFonts: TStrings;
    function FindSubstituteFont(const ASourceFontName: string; ACharacter: Char;
      var AFontCharacterSet: TdxFontCharacterSet): string;
    function GetCharsetByFontName(const AFontName: string): Integer;
    function GetFontCharacterSet(const AFontName: string): TdxFontCharacterSet;
    function ShouldUseDefaultFontToDrawInvisibleCharacter
      (AFontInfo: TdxFontInfo; ACharacter: Char): Boolean; virtual;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TdxFontInfo read GetItem; default;
    property Measurer: TdxFontInfoMeasurer read FMeasurer;
    class property SystemTrueTypeFonts: TdxSystemTrueTypeFontDictionary
      read FSystemTrueTypeFonts;
    property UnitConverter: TdxDocumentLayoutUnitConverter read FUnitConverter;
  end;

  { TdxFontCacheManager }

  TdxFontCacheManager = class sealed
  strict private
    class var FOfficeItems: TObjectList<TdxFontCache>;
    class var FVCLItems: TObjectList<TdxFontCache>;
    class constructor Initialize;
{$HINTS OFF}
    class destructor Finalize;
{$HINTS ON}
  public
    class function GetFontCache(const AUnit: TdxDocumentLayoutUnit;
      const ADpi: Single): TdxFontCache; static;
    class function GetOfficeFontCache(const AUnit: TdxDocumentLayoutUnit;
      const ADpi: Single): TdxFontCache; static;
    class function GetVCLFontCache(const AUnit: TdxDocumentLayoutUnit;
      const ADpi: Single): TdxFontCache; static;
  end;

function dxEnumFontFamilies(DC: HDC; var p2: TLogFont; p3: TFNFontEnumProc;
  p4: LPARAM; p5: DWORD): LongBool; overload;
function dxEnumFontFamilies(DC: HDC; p2: LPCWSTR; p3: TFNFontEnumProc;
  p4: LPARAM): LongBool; overload;

implementation

uses
  RTLConsts, Character, Math, SysUtils, ComObj, ActiveX, Forms,
  dxCore, dxHash, dxHashUtils, dxTypeHelpers, dxCharacters;

const
  dxUnicodeCharacterCount = 65536;
  CLASS_CMultiLanguage: TGUID = '{275C23E2-3747-11D0-9FEA-00AA003F8646}';

type
  IMLangCodePages = interface(IUnknown)
    ['{359F3443-BD4A-11D0-B188-00AA0038C969}']
    function GetCharCodePages(const chSrc: Char; out pdwCodePages: DWORD)
      : HResult; stdcall;
    function GetStrCodePages(const pszSrc: PChar; const cchSrc: ULONG;
      dwPriorityCodePages: DWORD; out pdwCodePages: DWORD;
      out pcchCodePages: ULONG): HResult; stdcall;
    function CodePageToCodePages(const uCodePage: SYSUINT;
      out pdwCodePages: LongWord): HResult; stdcall;
    function CodePagesToCodePage(const dwCodePages: LongWord;
      const uDefaultCodePage: SYSUINT; out puCodePage: SYSUINT)
      : HResult; stdcall;
  end;

  IMLangFontLink = interface(IMLangCodePages)
    ['{359F3441-BD4A-11D0-B188-00AA0038C969}']
    function GetFontCodePages(const HDC: THandle; const HFONT: THandle;
      out pdwCodePages: LongWord): HResult; stdcall;
    function MapFont(const HDC: THandle; const dwCodePages: LongWord;
      hSrcFont: THandle; out phDestFont: THandle): HResult; stdcall;
    function ReleaseFont(const HFONT: THandle): HResult; stdcall;
    function ResetFontMapping: HResult; stdcall;
  end;

  { TdxGdiFontHelper }

  TdxGdiFontHelper = class
{$REGION 'public types'}
  public const
    CJK_CODEPAGE_BITS = (1 shl 17) or (1 shl 18) or (1 shl 19) or (1 shl 20) or
      (1 shl 21);
  public type
    TFontMetrics = record
      OutlineTextmetric: TOutlineTextmetric;
      Ascent: Word;
      Descent: Word;
      ExternalLeading: SmallInt;
      LineSpacing: Word;
      IsCJKFont: Boolean;
      NeedScale: Boolean;
      HasOutlineMetrics: Boolean;
      HasVDMXTable: Boolean;
      UseGetGlyphIndices: Boolean;
    end;

    PFontMetrics = ^TFontMetrics;

    TT_OS2_V2 = packed record
      version: Word;
      xAvgCharWidth: SmallInt;
      usWeightClass: Word;
      usWidthClass: Word;
      fsType: SmallInt;
      ySubscriptXSize: SmallInt;
      ySubscriptYSize: SmallInt;
      ySubscriptXOffset: SmallInt;
      ySubscriptYOffset: SmallInt;
      ySuperscriptXSize: SmallInt;
      ySuperscriptYSize: SmallInt;
      ySuperscriptXOffset: SmallInt;
      ySuperscriptYOffset: SmallInt;
      yStrikeoutSize: SmallInt;
      yStrikeoutPosition: SmallInt;
      sFamilyClass: SmallInt;
      Panose: array [0 .. 9] of Byte;
      ulUnicodeRange1: ULONG;
      ulUnicodeRange3: ULONG;
      ulUnicodeRange2: ULONG;
      ulUnicodeRange4: ULONG;
      achVendID: array [0 .. 3] of AnsiChar;
      fsSelection: Word;
      usFirstCharIndex: Word;
      usLastCharIndex: Word;
      sTypoAscender: SmallInt;
      sTypoDescender: SmallInt;
      sTypoLineGap: SmallInt;
      usWinAscent: Word;
      usWinDescent: Word;
      ulCodePageRange1: ULONG;
      ulCodePageRange2: ULONG;
      sxHeight: SmallInt;
      sCapHeight: SmallInt;
      usDefaultChar: Word;
      usBreakChar: Word;
      usMaxContext: Word;
      usLowerOpticalPointSize: Word;
      usUpperOpticalPointSize: Word;
    end;

    TT_HHEA = packed record
      version: Cardinal;
      Ascender: SmallInt;
      Descender: SmallInt;
      LineGap: SmallInt;
      advanceWidthMax: Word;
      minLeftSideBearing: SmallInt;
      minRightSideBearing: SmallInt;
      xMaxExtent: SmallInt;
      caretSlopeRise: SmallInt;
      caretSlopeRun: SmallInt;
      caretOffset: SmallInt;
      reserved: array [0 .. 3] of SmallInt;
      metricDataFormat: SmallInt;
      numberOfHMetrics: Word;
    end;
{$ENDREGION}
  strict private
  class var
    FDefaultCharSet: Byte;
    FFontQuality: Byte;
    FRasterToTTFMap: TDictionary<string, string>;
  strict private
    class constructor Initialize;
{$HINTS OFF}
    class destructor Finalize;
{$HINTS ON}
    class function CalculateActualFontQuality: Byte; static;
    class function CreateRasterToTTFMap: TDictionary<string, string>; static;
    class function GetActualFontName(const AFontName: string;
      AIsOfficeFont: Boolean): string; static;
    class procedure GetDefaultFontMetrics(ADC: HDC;
      AFontMetrics: PFontMetrics); static;
    class function GetOpenTypeFontMetrics(ADC: HDC; AFontMetrics: PFontMetrics)
      : Boolean; static;
    class function IsFontInstalled(AEnumLogFont: PEnumLogFont;
      AEnumTextMetrics: PEnumTextMetric; AFontType: DWORD; AData: LPARAM)
      : Integer; stdcall; static;
    class function CheckFontStyles(AEnumLogFont: PEnumLogFontEx;
      ANewTextMetrics: PNewTextMetricEx; AFontType: DWORD; AData: LPARAM)
      : Integer; stdcall; static;
    class function SwapBytes(AValue: Word): Word; static;
    class function CreateWithValidFontFamilyName(const AFontName: string;
      var ALogFont: TLogFont): HFONT; static;
  public
    class function CreateFont(ADC: HDC; const AFontName: string; ASize: Single;
      AFontStyle: TFontStyles; ASizeScale: Single; out ARealFontName: string;
      AIsOfficeFont: Boolean): HFONT; static;
    class function GetFontMetrics(ADC: HDC; AFont: HFONT;
      AFontMetrics: PFontMetrics): Boolean; static;
    class function GetFontStyles(const AFontFamilyName: string)
      : TdxSupportedFontStylesInfo; static;

    class property DefaultCharSet: Byte read FDefaultCharSet;
    class property FontQuality: Byte read FFontQuality;
  end;

var
  FontLink: IMLangFontLink;
  FNeedFinalize: Boolean;

function dxEnumFontFamilies(DC: HDC; var p2: TLogFont; p3: TFNFontEnumProc;
  p4: LPARAM; p5: DWORD): LongBool;
begin
{$IFDEF DELPHI104SYDNEY}
  Result := EnumFontFamiliesEx(DC, p2, p3, p4, p5) <> 0;
{$ELSE}
  Result := EnumFontFamiliesEx(DC, p2, p3, p4, p5);
{$ENDIF}
end;

function dxEnumFontFamilies(DC: HDC; p2: LPCWSTR; p3: TFNFontEnumProc;
  p4: LPARAM): LongBool;
begin
{$IFDEF DELPHI104SYDNEY}
  Result := EnumFontFamilies(DC, p2, p3, p4) <> 0;
{$ELSE}
  Result := EnumFontFamilies(DC, p2, p3, p4);
{$ENDIF}
end;

{ TGdiFontHelper }

class constructor TdxGdiFontHelper.Initialize;
begin
  FDefaultCharSet := GetDefFontCharset;
  FFontQuality := CalculateActualFontQuality;
  FRasterToTTFMap := CreateRasterToTTFMap;
end;

{$HINTS OFF}

class destructor TdxGdiFontHelper.Finalize;
begin
  FreeAndNil(FRasterToTTFMap);
end;
{$HINTS ON}

class function TdxGdiFontHelper.SwapBytes(AValue: Word): Word;
begin
  Result := (AValue shr 8) or Word(AValue shl 8);
end;

class function TdxGdiFontHelper.GetFontMetrics(ADC: HDC; AFont: HFONT;
  AFontMetrics: PFontMetrics): Boolean;
var
  AOldFont: HFONT;
begin
  AOldFont := SelectObject(ADC, AFont);
  try
    Result := GetOpenTypeFontMetrics(ADC, AFontMetrics);
    if not Result then
      GetDefaultFontMetrics(ADC, AFontMetrics);
  finally
    SelectObject(ADC, AOldFont);
  end;
end;

class function TdxGdiFontHelper.CheckFontStyles(AEnumLogFont: PEnumLogFontEx;
  ANewTextMetrics: PNewTextMetricEx; AFontType: DWORD; AData: LPARAM): Integer;
const
  AllStyles = [TdxSupportedFontStyle.Regular, TdxSupportedFontStyle.Bold,
    TdxSupportedFontStyle.Italic, TdxSupportedFontStyle.Underline,
    TdxSupportedFontStyle.Strikeout, TdxSupportedFontStyle.BoldItalic];
var
  AFontStylesInfo: ^TdxSupportedFontStylesInfo absolute AData;
  ACurrentFontStyles: TdxSupportedFontStyles;
  I: TdxSupportedFontStyle;
begin
  Result := 1;
  if ANewTextMetrics = nil then
    Exit;

  ACurrentFontStyles := [];
  if (ANewTextMetrics.ntmTm.tmItalic <> 0) and
    (ANewTextMetrics.ntmTm.tmWeight >= FW_BOLD) then
    Include(ACurrentFontStyles, TdxSupportedFontStyle.BoldItalic)
  else
  begin
    if ANewTextMetrics.ntmTm.tmWeight >= FW_BOLD then
      Include(ACurrentFontStyles, TdxSupportedFontStyle.Bold);
    if ANewTextMetrics.ntmTm.tmItalic <> 0 then
      Include(ACurrentFontStyles, TdxSupportedFontStyle.Italic);
  end;

  if ANewTextMetrics.ntmTm.tmUnderlined <> 0 then
    Include(ACurrentFontStyles, TdxSupportedFontStyle.Underline);
  if ANewTextMetrics.ntmTm.tmStruckOut <> 0 then
    Include(ACurrentFontStyles, TdxSupportedFontStyle.Strikeout);

  if ACurrentFontStyles = [] then
    Include(ACurrentFontStyles, TdxSupportedFontStyle.Regular);

  for I := Low(TdxSupportedFontStyle) to High(TdxSupportedFontStyle) do
    if I in ACurrentFontStyles then
      AFontStylesInfo.NativeStyles[I] := True;

  AFontStylesInfo.SupportedStyles := AFontStylesInfo.SupportedStyles +
    ACurrentFontStyles;
end;

class function TdxGdiFontHelper.GetFontStyles(const AFontFamilyName: string)
  : TdxSupportedFontStylesInfo;
var
  ALogFont: TLogFont;
  ADC: HDC;
begin
  ZeroMemory(@Result, SizeOf(Result));
  ADC := CreateCompatibleDC(0);
  try
    cxInitLogFont(ALogFont, AFontFamilyName);
    if not dxEnumFontFamilies(ADC, ALogFont, @CheckFontStyles,
      LPARAM(@Result), 0) then
      Result.SupportedStyles := [TdxSupportedFontStyle.Regular];
  finally
    DeleteDC(ADC);
  end;
end;

class procedure TdxGdiFontHelper.GetDefaultFontMetrics(ADC: HDC;
  AFontMetrics: PFontMetrics);
var
  ATextMetric: TTextMetric;
  ALineHeight: Integer;
begin
  GetTextMetricsW(ADC, ATextMetric);
  AFontMetrics.Ascent := ATextMetric.tmAscent;
  AFontMetrics.Descent := ATextMetric.tmDescent;
  AFontMetrics.ExternalLeading := ATextMetric.tmExternalLeading;
  if AFontMetrics.HasOutlineMetrics then
    Exit;
  FillChar(AFontMetrics.OutlineTextmetric, SizeOf(TOutlineTextmetric), 0);
  ALineHeight := Max(1, ATextMetric.tmHeight div 12);
  AFontMetrics.OutlineTextmetric.otmsUnderscoreSize := ALineHeight;
  AFontMetrics.OutlineTextmetric.otmsStrikeoutSize := ALineHeight;
  if ALineHeight < 3 then
    AFontMetrics.OutlineTextmetric.otmsUnderscorePosition := -ALineHeight
  else
    AFontMetrics.OutlineTextmetric.otmsUnderscorePosition :=
      -MulDiv(ALineHeight, 2, 3);
  AFontMetrics.OutlineTextmetric.otmsStrikeoutPosition :=
    Round((ATextMetric.tmAscent + ATextMetric.tmDescent -
    ATextMetric.tmInternalLeading) * 3 / 11);
end;

class function TdxGdiFontHelper.GetOpenTypeFontMetrics(ADC: HDC;
  AFontMetrics: PFontMetrics): Boolean;
const
  MS_OS2_TAG = $322F534F;
  MS_HHEA_TAG = $61656868;
  MS_VDMX_TAG = $584D4456;
var
  AOutlineTextMetric: TOutlineTextmetric;
  AOs2Metrics: TT_OS2_V2;
  AHorizontalHeader: TT_HHEA;
  AFontSignature: TFontSignature;
  ASize: DWORD;
  ALineGap: Word;
begin
  AFontMetrics.IsCJKFont := False;
  AFontMetrics.HasVDMXTable := False;
  AFontMetrics.NeedScale := False;
  AFontMetrics.UseGetGlyphIndices := True;

  AOutlineTextMetric.otmSize := SizeOf(AOutlineTextMetric);
  AFontMetrics.HasOutlineMetrics := GetOutlineTextMetrics(ADC,
    AOutlineTextMetric.otmSize, @AOutlineTextMetric) <> 0;
  if not AFontMetrics.HasOutlineMetrics then
    Exit(False);

  AFontMetrics.IsCJKFont := (Integer(GetTextCharsetInfo(ADC, @AFontSignature, 0)
    ) <> DEFAULT_CHARSET) and (AFontSignature.fsCsb[0] and
    CJK_CODEPAGE_BITS <> 0);

  AFontMetrics.OutlineTextmetric := AOutlineTextMetric;
  AFontMetrics.HasVDMXTable := GetFontData(ADC, MS_VDMX_TAG, 0, nil, 0) > 0;

  ZeroMemory(@AHorizontalHeader, SizeOf(AHorizontalHeader));
  if GetFontData(ADC, MS_HHEA_TAG, 0, @AHorizontalHeader,
    SizeOf(AHorizontalHeader)) = GDI_ERROR then
    Exit(False);

  AFontMetrics.Ascent := SwapBytes(AHorizontalHeader.Ascender);
  AFontMetrics.Descent := -SwapBytes(AHorizontalHeader.Descender);
  ALineGap := SwapBytes(AHorizontalHeader.LineGap);
  AFontMetrics.ExternalLeading := ALineGap;
  AFontMetrics.LineSpacing := AFontMetrics.Ascent + AFontMetrics.Descent
    + ALineGap;

  ASize := GetFontData(ADC, MS_OS2_TAG, 0, nil, 0);
  if ASize = GDI_ERROR then
    Exit(False);

  AFontMetrics.UseGetGlyphIndices := False;
  AFontMetrics.NeedScale := True;
  if (AFontMetrics.Ascent > 0) and (AFontMetrics.Descent > 0) then
    Exit(True);

  if ASize > SizeOf(AOs2Metrics) then
    ASize := SizeOf(AOs2Metrics);
  ZeroMemory(@AOs2Metrics, SizeOf(AOs2Metrics));
  if GetFontData(ADC, MS_OS2_TAG, 0, @AOs2Metrics, ASize) <> ASize then
    Exit(False);
  AFontMetrics.Ascent := SwapBytes(AOs2Metrics.usWinAscent);
  AFontMetrics.Descent := SwapBytes(AOs2Metrics.usWinDescent);
  if AFontMetrics.Ascent + AFontMetrics.Descent = 0 then
  begin
    AFontMetrics.Ascent := SwapBytes(AOs2Metrics.sTypoAscender);
    AFontMetrics.Descent := SwapBytes(AOs2Metrics.sTypoDescender);
  end;
  ALineGap := SwapBytes(AOs2Metrics.sTypoLineGap);
  AFontMetrics.ExternalLeading :=
    Max(0, ALineGap - ((AFontMetrics.Ascent + AFontMetrics.Descent) -
    (AFontMetrics.Ascent - AFontMetrics.Descent)));
  AFontMetrics.LineSpacing := AFontMetrics.Ascent + AFontMetrics.Descent +
    AFontMetrics.ExternalLeading;
  Result := True;
end;

class function TdxGdiFontHelper.CalculateActualFontQuality: Byte;
const
  CLEARTYPE_QUALITY = 5;
  CLEARTYPE_NATURAL_QUALITY = 6;
  SPI_GETFONTSMOOTHINGTYPE = $200A;
  FE_FONTSMOOTHINGSTANDARD = $0001;
  FE_FONTSMOOTHINGCLEARTYPE = $0002;
var
  ASmoothingType: DWORD;
  AFontSmoothing: BOOL;
begin
  Result := DEFAULT_QUALITY;
  if SystemParametersInfo(SPI_GETFONTSMOOTHING, 0, @AFontSmoothing, 0) and AFontSmoothing
  then
  begin
    if SystemParametersInfo(SPI_GETFONTSMOOTHINGTYPE, 0, @ASmoothingType, 0) and
      (ASmoothingType = FE_FONTSMOOTHINGCLEARTYPE) then
    begin
      if IsWinVistaOrLater then
        Result := CLEARTYPE_NATURAL_QUALITY
      else
        Result := CLEARTYPE_QUALITY;
    end;
  end;
end;

class function TdxGdiFontHelper.CreateRasterToTTFMap
  : TDictionary<string, string>;
begin
  Result := TDictionary<string, string>.Create;
  Result.Add('MS SANS SERIF', 'MICROSOFT SANS SERIF');
  Result.Add('MS SERIF', 'CAMBRIA');
  Result.Add('COURIER', 'COURIER NEW');
end;

class function TdxGdiFontHelper.GetActualFontName(const AFontName: string;
  AIsOfficeFont: Boolean): string;
var
  AUpperFontName: string;
begin
  if AIsOfficeFont then
  begin
{$IFDEF DELPHIXE4}
    AUpperFontName := AFontName.ToUpper;
{$ELSE}
    AUpperFontName := ToUpper(AFontName);
{$ENDIF}
    if not FRasterToTTFMap.TryGetValue(AUpperFontName, Result) then
      Exit(AFontName);

    System.TMonitor.Enter(TdxFontCache.NameToCharacterSetMap);
    try
      if not TdxFontCache.SystemTrueTypeFonts.ContainsKey(Result) then
        Exit(AFontName);
    finally
      System.TMonitor.Exit(TdxFontCache.NameToCharacterSetMap);
    end;
  end
  else
    Result := AFontName;
end;

class function TdxGdiFontHelper.IsFontInstalled(AEnumLogFont: PEnumLogFont;
  AEnumTextMetrics: PEnumTextMetric; AFontType: DWORD; AData: LPARAM)
  : Integer; stdcall;
var
  ALogFont: PLogFont absolute AData;
begin
  if AFontType and RASTER_FONTTYPE <> 0 then
    Exit(1);
  StrLCopy(PChar(@ALogFont.lfFaceName), PChar(@AEnumLogFont.elfFullName),
    LF_FACESIZE);
  Result := 0;
end;

class function TdxGdiFontHelper.CreateWithValidFontFamilyName(const AFontName
  : string; var ALogFont: TLogFont): HFONT;
type
  TFaceName = array [0 .. LF_FACESIZE - 1] of WideChar;
var
  ASaveFont: HFONT;
  AFaceName: TFaceName;
  DC: HDC;
begin
  Result := CreateFontIndirectW(ALogFont);
  DC := CreateCompatibleDC(0);
  try
    ASaveFont := SelectObject(DC, Result);
    FillChar(AFaceName, SizeOf(AFaceName), 0);
    GetTextFaceW(DC, LF_FACESIZE, @AFaceName);
    SelectObject(DC, ASaveFont);
    if StrIComp(PWideChar(@AFaceName), PWideChar(@ALogFont.lfFaceName)) <> 0
    then
    begin
      DeleteObject(Result);
      if not dxEnumFontFamilies(DC, PChar(AFontName), @IsFontInstalled,
        LPARAM(@ALogFont)) then
        Result := CreateFontIndirectW(ALogFont)
      else
        Result := 0;
    end;
  finally
    DeleteDC(DC);
  end;
end;

class function TdxGdiFontHelper.CreateFont(ADC: HDC; const AFontName: string;
  ASize: Single; AFontStyle: TFontStyles; ASizeScale: Single;
  out ARealFontName: string; AIsOfficeFont: Boolean): HFONT;
const
  DefaultSubstitutionFontName: string = 'Times New Roman';
  CLIP_DFA_DISABLE = 64;
var
  ALogFont: TLogFont;
  AActualFontName: string;
begin
  ZeroMemory(@ALogFont, SizeOf(TLogFont));
  ALogFont.lfCharSet := DEFAULT_CHARSET;

  AActualFontName := GetActualFontName(AFontName, AIsOfficeFont);
  StrLCopy(PChar(@ALogFont.lfFaceName), PChar(AActualFontName), LF_FACESIZE);

  ALogFont.lfHeight := -Round(ASize / ASizeScale);
  ALogFont.lfWeight := IfThen(fsBold in AFontStyle, FW_BOLD, FW_NORMAL);
  ALogFont.lfItalic := Byte(fsItalic in AFontStyle);
  ALogFont.lfUnderline := Byte(fsUnderline in AFontStyle);
  ALogFont.lfStrikeOut := Byte(fsStrikeOut in AFontStyle);

  if AIsOfficeFont then
  begin
    ALogFont.lfCharSet := DEFAULT_CHARSET;
    ALogFont.lfOutPrecision := OUT_TT_ONLY_PRECIS;
    ALogFont.lfClipPrecision := CLIP_DFA_DISABLE;
    ALogFont.lfQuality := FontQuality;
    ALogFont.lfPitchAndFamily := DEFAULT_PITCH;
  end
  else
  begin
    ALogFont.lfCharSet := DefFontData.Charset;
  end;

  Result := CreateWithValidFontFamilyName(AActualFontName, ALogFont);
  if Result = 0 then
  begin
    ARealFontName := DefaultSubstitutionFontName;
    StrLCopy(PChar(@ALogFont.lfFaceName), PChar(DefaultSubstitutionFontName),
      LF_FACESIZE);
    Result := CreateFontIndirectW(ALogFont);
  end
  else
    ARealFontName := PChar(@ALogFont.lfFaceName);
end;

function TdxUnicodeSubrange.ContainsChar(ACharacter: Char): Boolean;
begin
  Result := ((ACharacter >= LowValue) and (ACharacter <= HiValue));
end;

{ UnicodeRangeInfo }

constructor TdxUnicodeRangeInfo.Create;
begin
  inherited Create;
  PopulateSubranges;
end;

destructor TdxUnicodeRangeInfo.Destroy;
begin
  FRanges := nil;
  inherited Destroy;
end;

procedure TdxUnicodeRangeInfo.PopulateSubranges;
begin
  Capacity := 135;
  AddSubrange(#$0000, #$007F, 0);
  AddSubrange(#$0080, #$00FF, 1);
  AddSubrange(#$0100, #$017F, 2);
  AddSubrange(#$0180, #$024F, 3);
  AddSubrange(#$0250, #$02AF, 4);
  AddSubrange(#$02B0, #$02FF, 5);
  AddSubrange(#$0300, #$036F, 6);
  AddSubrange(#$0370, #$03FF, 7);
  AddSubrange(#$0400, #$04FF, 9);
  AddSubrange(#$0500, #$052F, 9);
  AddSubrange(#$0530, #$058F, 10);
  AddSubrange(#$0590, #$05FF, 11);
  AddSubrange(#$0600, #$06FF, 13);
  AddSubrange(#$0700, #$074F, 71);
  AddSubrange(#$0750, #$077F, 13);
  AddSubrange(#$0780, #$07BF, 72);
  AddSubrange(#$07C0, #$07FF, 14);
  AddSubrange(#$0900, #$097F, 15);
  AddSubrange(#$0980, #$09FF, 16);
  AddSubrange(#$0A00, #$0A7F, 17);
  AddSubrange(#$0A80, #$0AFF, 18);
  AddSubrange(#$0B00, #$0B7F, 19);
  AddSubrange(#$0B80, #$0BFF, 20);
  AddSubrange(#$0C00, #$0C7F, 21);
  AddSubrange(#$0C80, #$0CFF, 22);
  AddSubrange(#$0D00, #$0D7F, 23);
  AddSubrange(#$0D80, #$0DFF, 73);
  AddSubrange(#$0E00, #$0E7F, 24);
  AddSubrange(#$0E80, #$0EFF, 25);
  AddSubrange(#$0F00, #$0FFF, 70);
  AddSubrange(#$1000, #$109F, 74);
  AddSubrange(#$10A0, #$10FF, 26);
  AddSubrange(#$1100, #$11FF, 28);
  AddSubrange(#$1200, #$137F, 75);
  AddSubrange(#$1380, #$139F, 75);
  AddSubrange(#$13A0, #$13FF, 76);
  AddSubrange(#$1400, #$167F, 77);
  AddSubrange(#$1680, #$169F, 78);
  AddSubrange(#$16A0, #$16FF, 79);
  AddSubrange(#$1700, #$171F, 84);
  AddSubrange(#$1720, #$173F, 84);
  AddSubrange(#$1740, #$175F, 84);
  AddSubrange(#$1760, #$177F, 84);
  AddSubrange(#$1780, #$17FF, 80);
  AddSubrange(#$1800, #$18AF, 81);
  AddSubrange(#$1900, #$194F, 93);
  AddSubrange(#$1950, #$197F, 94);
  AddSubrange(#$1980, #$19DF, 95);
  AddSubrange(#$19E0, #$19FF, 80);
  AddSubrange(#$1A00, #$1A1F, 96);
  AddSubrange(#$1B00, #$1B7F, 27);
  AddSubrange(#$1B80, #$1BBF, 112);
  AddSubrange(#$1C00, #$1C4F, 113);
  AddSubrange(#$1C50, #$1C7F, 114);
  AddSubrange(#$1D00, #$1D7F, 4);
  AddSubrange(#$1D80, #$1DBF, 4);
  AddSubrange(#$1DC0, #$1DFF, 6);
  AddSubrange(#$1E00, #$1EFF, 29);
  AddSubrange(#$1F00, #$1FFF, 30);
  AddSubrange(#$2000, #$206F, 31);
  AddSubrange(#$2070, #$209F, 32);
  AddSubrange(#$20A0, #$20CF, 33);
  AddSubrange(#$20D0, #$20FF, 34);
  AddSubrange(#$2100, #$214F, 35);
  AddSubrange(#$2150, #$218F, 36);
  AddSubrange(#$2190, #$21FF, 37);
  AddSubrange(#$2200, #$22FF, 38);
  AddSubrange(#$2300, #$23FF, 39);
  AddSubrange(#$2400, #$243F, 40);
  AddSubrange(#$2440, #$245F, 41);
  AddSubrange(#$2460, #$24FF, 42);
  AddSubrange(#$2500, #$257F, 43);
  AddSubrange(#$2580, #$259F, 44);
  AddSubrange(#$25A0, #$25FF, 45);
  AddSubrange(#$2600, #$26FF, 46);
  AddSubrange(#$2700, #$27BF, 47);
  AddSubrange(#$27C0, #$27EF, 38);
  AddSubrange(#$27F0, #$27FF, 37);
  AddSubrange(#$2800, #$28FF, 82);
  AddSubrange(#$2900, #$297F, 37);
  AddSubrange(#$2980, #$29FF, 38);
  AddSubrange(#$2A00, #$2AFF, 38);
  AddSubrange(#$2B00, #$2BFF, 37);
  AddSubrange(#$2C00, #$2C5F, 97);
  AddSubrange(#$2C60, #$2C7F, 29);
  AddSubrange(#$2C80, #$2CFF, 8);
  AddSubrange(#$2D00, #$2D2F, 26);
  AddSubrange(#$2D30, #$2D7F, 98);
  AddSubrange(#$2D80, #$2DDF, 75);
  AddSubrange(#$2DE0, #$2DFF, 9);
  AddSubrange(#$2E00, #$2E7F, 31);
  AddSubrange(#$2E80, #$2EFF, 59);
  AddSubrange(#$2F00, #$2FDF, 59);
  AddSubrange(#$2FF0, #$2FFF, 59);
  AddSubrange(#$3000, #$303F, 48);
  AddSubrange(#$3040, #$309F, 49);
  AddSubrange(#$30A0, #$30FF, 50);
  AddSubrange(#$3100, #$312F, 51);
  AddSubrange(#$3130, #$318F, 52);
  AddSubrange(#$3190, #$319F, 59);
  AddSubrange(#$31A0, #$31BF, 51);
  AddSubrange(#$31C0, #$31EF, 61);
  AddSubrange(#$31F0, #$31FF, 50);
  AddSubrange(#$3200, #$32FF, 54);
  AddSubrange(#$3300, #$33FF, 55);
  AddSubrange(#$3400, #$4DBF, 59);
  AddSubrange(#$4DC0, #$4DFF, 99);
  AddSubrange(#$4E00, #$9FFF, 59);
  AddSubrange(#$A000, #$A48F, 83);
  AddSubrange(#$A490, #$A4CF, 83);
  AddSubrange(#$A500, #$A63F, 12);
  AddSubrange(#$A640, #$A69F, 9);
  AddSubrange(#$A700, #$A71F, 5);
  AddSubrange(#$A720, #$A7FF, 29);
  AddSubrange(#$A800, #$A82F, 100);
  AddSubrange(#$A840, #$A87F, 53);
  AddSubrange(#$A880, #$A8DF, 115);
  AddSubrange(#$A900, #$A92F, 116);
  AddSubrange(#$A930, #$A95F, 117);
  AddSubrange(#$AA00, #$AA5F, 118);
  AddSubrange(#$AC00, #$D7AF, 56);
  AddSubrange(#$D800, #$DFFF, 57);
  AddSubrange(#$E000, #$F8FF, 60);
  AddSubrange(#$F900, #$FAFF, 61);
  AddSubrange(#$FB00, #$FB4F, 62);
  AddSubrange(#$FB50, #$FDFF, 63);
  AddSubrange(#$FE00, #$FE0F, 91);
  AddSubrange(#$FE10, #$FE1F, 65);
  AddSubrange(#$FE20, #$FE2F, 64);
  AddSubrange(#$FE30, #$FE4F, 65);
  AddSubrange(#$FE50, #$FE6F, 66);
  AddSubrange(#$FE70, #$FEFF, 67);
  AddSubrange(#$FF00, #$FFEF, 68);
  AddSubrange(#$FFF0, #$FFFF, 69);

end;

procedure TdxUnicodeRangeInfo.AddSubrange(AStartCharacter, AEndCharacter: Char;
  ABit: Integer);
begin
  if FCount = FCapacity then
    Grow;
  with FRanges[FCount] do
  begin
    LowValue := AStartCharacter;
    HiValue := AEndCharacter;
    Bit := ABit;
  end;
  Inc(FCount);
end;

procedure TdxUnicodeRangeInfo.Grow;
begin
  Capacity := Capacity + Max(Capacity div 2, 64);
end;

function TdxUnicodeRangeInfo.LookupSubrange(ACharacter: Char)
  : PdxUnicodeSubrange;
var
  I, H, L: Integer;
begin
  Result := nil;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    if ACharacter < FRanges[I].LowValue then
      H := I - 1
    else if ACharacter > FRanges[I].HiValue then
      L := I + 1
    else
    begin
      Result := @FRanges[I];
      Break;
    end;
  end;
end;

procedure TdxUnicodeRangeInfo.SetCapacity(AValue: Integer);
begin
  if FCapacity <> AValue then
  begin
    FCapacity := AValue;
    SetLength(FRanges, Capacity);
  end;
end;

{ TdxFontCharacterRange }

procedure TdxFontCharacterRange.CopyFrom(ALow, AHi: Integer);
begin
  Low := ALow;
  Hi := AHi;
end;

procedure TdxFontCharacterRange.CopyFrom(const ARange: TWCRange);
begin
  Low := Ord(ARange.wcLow);
  Hi := Low + ARange.cGlyphs - 1;
end;

{ TdxFontCharacterSet }

constructor TdxFontCharacterSet.Create(const ARanges
  : TArray<TdxFontCharacterRange>; const APanose: TPanose);
var
  I, AMaxIndex: Integer;
  ARange: TdxFontCharacterRange;
begin
  inherited Create;
  FBits := TBits.Create;
  FPanose := APanose;
  AMaxIndex := 0;
  for I := Low(ARanges) to High(ARanges) do
  begin
    ARange := ARanges[I];
    AddRange(ARange);
    if ARange.Hi > AMaxIndex then
      AMaxIndex := ARange.Hi;
  end;
  FBits.Size := AMaxIndex + 1;
end;

destructor TdxFontCharacterSet.Destroy;
begin
  FBits.Free;
  inherited Destroy;
end;

class function TdxFontCharacterSet.CalculatePanoseDistance
  (const AFontCharacterSet1, AFontCharacterSet2: TdxFontCharacterSet): Integer;
var
  I, ADist: Integer;
  P1, p2: PByte;
begin
  Result := 0;
  P1 := @AFontCharacterSet1.FPanose;
  p2 := @AFontCharacterSet2.FPanose;
  for I := 0 to 9 do
  begin
    ADist := P1^ - p2^;
    Result := Result + Sqr(ADist);
    Inc(P1);
    Inc(p2);
  end;
end;

function TdxFontCharacterSet.ContainsChar(C: Char): Boolean;
begin
  Result := (Ord(C) < FBits.Size) and FBits[Ord(C)];
end;

procedure TdxFontCharacterSet.AddRange(const AFontCharacterRange
  : TdxFontCharacterRange);
var
  I: Integer;
begin
  for I := AFontCharacterRange.Low to AFontCharacterRange.Hi do
    FBits[I] := True;
end;

{ TdxFontInfo.TUnicodeSubrangeBits }

procedure TdxFontInfo.TUnicodeSubrangeBits.Clear;
begin
  Data[0] := 0;
  Data[1] := 0;
  Data[2] := 0;
  Data[3] := 0;
end;

function TdxFontInfo.TUnicodeSubrangeBits.GetBit(AIndex: Integer): Boolean;
begin
  Result := Data[AIndex div BitPerDWORD] and
    (1 shl (AIndex mod BitPerDWORD)) <> 0;
end;

procedure TdxFontInfo.TUnicodeSubrangeBits.SetBit(AIndex: Integer;
  const Value: Boolean);
begin
  if Value then
    Data[AIndex div BitPerDWORD] := Data[AIndex div BitPerDWORD] or
      (1 shl (AIndex mod BitPerDWORD))
  else
    Data[AIndex div BitPerDWORD] := Data[AIndex div BitPerDWORD] and
      not DWORD((1 shl (AIndex mod BitPerDWORD)));
end;

{ TdxFontInfo.TCharacterDrawingAbilityTable }

constructor TdxFontInfo.TCharacterDrawingAbilityTable.Create;
begin
  FDrawingAbility := TBits.Create;
  FDrawingAbility.Size := $0600;
  FCalculated := TBits.Create;
  FCalculated.Size := $0600;
end;

destructor TdxFontInfo.TCharacterDrawingAbilityTable.Destroy;
begin
  FCalculated.Free;
  FDrawingAbility.Free;
  inherited Destroy;
end;

function TdxFontInfo.TCharacterDrawingAbilityTable.GetDrawingAbility
  (ACharacter: Char; out AAbility: Boolean): Boolean;
begin
  if Ord(ACharacter) >= FCalculated.Size then
  begin
    FCalculated.Size := Ord(ACharacter) + 1;
    FDrawingAbility.Size := Ord(ACharacter) + 1;
  end;
  Result := FCalculated[Ord(ACharacter)];
  if Result then
    AAbility := FDrawingAbility[Ord(ACharacter)];
end;

procedure TdxFontInfo.TCharacterDrawingAbilityTable.SetDrawingAbility
  (ACharacter: Char; AValue: Boolean);
begin
  FCalculated[Ord(ACharacter)] := True;
  FDrawingAbility[Ord(ACharacter)] := AValue;
end;

{ TdxFontInfo }

constructor TdxFontInfo.Create(AMeasurer: TdxFontInfoMeasurer;
  const AName: string; ADoubleSize: Integer; const AFontStyle: TFontStyles;
  AAllowCjkCorrection: Boolean = True; AIsOfficeFont: Boolean = True);
begin
  inherited Create;
  FName := AName;
  FStyle := AFontStyle;
  FBaseFontIndex := -1;
  FSizeInPoints := ADoubleSize / 2;
  CreateFont(AMeasurer, AName, Trunc(FSizeInPoints), AFontStyle, AIsOfficeFont);
  FCharacterDrawingAbilityTable := TCharacterDrawingAbilityTable.Create;
  CalculateFontParameters(AMeasurer, AAllowCjkCorrection, AIsOfficeFont);
end;

destructor TdxFontInfo.Destroy;
begin
  FCharacterDrawingAbilityTable.Free;
  if GdiFontHandle <> 0 then
    DeleteObject(GdiFontHandle);
  inherited Destroy;
end;

function MulDivRound(AValue, ANumerator, ADenominator: Integer): Integer;
begin
  Result := Trunc((Int64(AValue) * ANumerator + ADenominator / 2) /
    ADenominator);
end;

procedure TdxFontInfo.CalculateFontVerticalParameters
  (AMeasurer: TdxFontInfoMeasurer; AAllowCjkCorrection, AIsOfficeFont: Boolean);
const
  USE_TYPO_METRICS = $80;
var
  ASizeInUnits, ARatio: Double;
  AFontMetrics: TdxGdiFontHelper.TFontMetrics;
  AOutlineTextMetric: POutlineTextmetric;
  AExternalLeading, AFixRatio, ARoundedAscent: Integer;
begin
  TdxGdiFontHelper.GetFontMetrics(AMeasurer.Canvas.Handle, FGdiFontHandle,
    @AFontMetrics);
  AOutlineTextMetric := @AFontMetrics.OutlineTextmetric;
  FUseGetGlyphIndices := AFontMetrics.UseGetGlyphIndices;
  FIsCJKFont := AFontMetrics.IsCJKFont and AAllowCjkCorrection;
  if AFontMetrics.NeedScale then
  begin
    FPanose := AOutlineTextMetric.otmPanoseNumber;
    if not AIsOfficeFont and not IsCJKFont and
      ((AOutlineTextMetric.otmfsSelection and USE_TYPO_METRICS) = 0) then
    begin
      FAscent := AOutlineTextMetric.otmTextMetrics.tmAscent;
      FDescent := AOutlineTextMetric.otmTextMetrics.tmDescent;;
      AExternalLeading := AOutlineTextMetric.otmTextMetrics.tmExternalLeading;
      FLineSpacing := AExternalLeading + Ascent + Descent;
    end
    else
    begin
      ASizeInUnits := FSizeInPoints / AMeasurer.UnitConverter.FontSizeScale;
      ARatio := ASizeInUnits / AOutlineTextMetric.otmEMSquare;
      FAscent := Ceil(AFontMetrics.Ascent * ARatio);
      FDescent := Ceil(AFontMetrics.Descent * ARatio);
      FLineSpacing := Ceil(AFontMetrics.LineSpacing * ARatio);
      if not IsCJKFont then
      begin
        AFixRatio := Trunc(ARatio * 65536 + 0.5);
        ARoundedAscent := MulDivRound(AFontMetrics.Ascent, AFixRatio, 65536);
        GdiOffset := AOutlineTextMetric.otmTextMetrics.tmAscent -
          ARoundedAscent;
        if AFontMetrics.HasVDMXTable then
          GdiOffset := Min(GdiOffset, 0);
        FDrawingOffset := AOutlineTextMetric.otmTextMetrics.tmAscent - Ascent;
      end
      else
        FCjkUnderlineSize :=
          Ceil(SizeInPoints / AMeasurer.UnitConverter.FontSizeScale *
          51 / 1024);
    end;
  end
  else
  begin
    FAscent := AFontMetrics.Ascent;
    FDescent := AFontMetrics.Descent;
    AExternalLeading := AFontMetrics.ExternalLeading;
    FLineSpacing := AExternalLeading + Ascent + Descent;
  end;

  if IsCJKFont then
    CalculateCJKFontParameters(AOutlineTextMetric);

  CalculateUnderlineAndStrikeoutParameters(AOutlineTextMetric^);
end;

function TdxFontInfo.CalculateFontSizeInLayoutUnits(AFontUnit: TdxGraphicUnit;
  AUnitConverter: TdxDocumentLayoutUnitConverter): Single;
begin
  case AFontUnit of
    guDocument:
      Result := AUnitConverter.DocumentsToFontUnitsF(FSizeInPoints);
    guInch:
      Result := AUnitConverter.InchesToFontUnitsF(FSizeInPoints);
    guMillimeter:
      Result := AUnitConverter.MillimetersToFontUnitsF(FSizeInPoints);
  else
    Result := AUnitConverter.PointsToFontUnitsF(FSizeInPoints);
  end;
end;

procedure TdxFontInfo.CalculateUnderlineAndStrikeoutParameters
  (const AOutlineTextMetric: TOutlineTextmetric);
var
  AOffset: TPoint;
begin
  if not FApplyCjkUnderline then
  begin
    FUnderlinePosition := -AOutlineTextMetric.otmsUnderscorePosition;
    FUnderlineThickness := AOutlineTextMetric.otmsUnderscoreSize;
  end
  else
  begin
    FUnderlinePosition := FCjkUnderlinePosition;
    FUnderlineThickness := FCjkUnderlineSize;
  end;
  FStrikeoutPosition := AOutlineTextMetric.otmsStrikeoutPosition;
  FStrikeoutThickness := Integer(AOutlineTextMetric.otmsStrikeoutSize);
  FSubscriptSize := TSize(AOutlineTextMetric.otmptSubscriptSize);
  FSubscriptOffset := AOutlineTextMetric.otmptSubscriptOffset;
  FSuperscriptOffset := AOutlineTextMetric.otmptSuperscriptOffset;
  AOffset := SuperscriptOffset;
  AOffset.Y := -AOffset.Y;
  FSuperscriptOffset := AOffset;
  FSuperscriptSize := TSize(AOutlineTextMetric.otmptSuperscriptSize);
end;

function TdxFontInfo.CanDrawCharacter(AUnicodeRangeInfo: TdxUnicodeRangeInfo;
  ACanvas: TCanvas; ACharacter: Char): Boolean;
begin
  if not FCharacterDrawingAbilityTable.GetDrawingAbility(ACharacter, Result)
  then
  begin
    Result := CalculateCanDrawCharacter(AUnicodeRangeInfo, ACanvas, ACharacter);
    FCharacterDrawingAbilityTable.SetDrawingAbility(ACharacter, Result);
  end;
end;

function TdxFontInfo.CalculateCanDrawCharacter(AUnicodeRangeInfo
  : TdxUnicodeRangeInfo; ACanvas: TCanvas; ACharacter: Char): Boolean;
var
  AUnicodeSubrangeBits: TdxFontInfo.TUnicodeSubrangeBits;
  AUnicodeSubRange: PdxUnicodeSubrange;
begin
  AUnicodeSubRange := AUnicodeRangeInfo.LookupSubrange(ACharacter);
  if AUnicodeSubRange <> nil then
  begin
    Assert(AUnicodeSubRange.Bit < 126, '');
    AUnicodeSubrangeBits := CalculateSupportedUnicodeSubrangeBits
      (AUnicodeRangeInfo, ACanvas);
    Result := AUnicodeSubrangeBits.Bits[AUnicodeSubRange.Bit];
  end
  else
    Result := False;
end;

class function TdxFontInfo.GetFontUnicodeRanges(ADC: HDC; AFont: HFONT)
  : TArray<TdxFontCharacterRange>;
var
  I, AGlyphCount, ASize: Integer;
  AGlyphSet: PGlyphSet;
  AOldFont: HFONT;
begin
  if AFont <> 0 then
    AOldFont := SelectObject(ADC, AFont)
  else
    AOldFont := 0;
  try
    ASize := Windows.GetFontUnicodeRanges(ADC, nil);
    if ASize = 0 then
      Exit(nil);
    GetMem(AGlyphSet, ASize);
    try
      Windows.GetFontUnicodeRanges(ADC, AGlyphSet);
      AGlyphCount := AGlyphSet.cRanges;
      SetLength(Result, AGlyphCount + 2);
      for I := 0 to AGlyphCount - 1 do
        Result[I].CopyFrom(AGlyphSet.ranges[I]);

      Result[AGlyphCount].CopyFrom(0, 255);
      Result[AGlyphCount + 1].CopyFrom(61440, 61695);
    finally
      FreeMem(AGlyphSet);
    end;
  finally
    if AFont <> 0 then
      SelectObject(ADC, AOldFont);
  end;
end;

function TdxFontInfo.CalculateSupportedUnicodeSubrangeBits(AUnicodeRangeInfo
  : TdxUnicodeRangeInfo; ACanvas: TCanvas): TdxFontInfo.TUnicodeSubrangeBits;
var
  ADC, AOldFont: THandle;
  AFontSignature: TFontSignature;
  ALocaleSignature: TLocaleSignature;
begin
  if not FUnicodeSubrangeBitsCalculated then
  begin
    if not FTrueType then
    begin
      GetLocaleInfoW(LOCALE_INVARIANT, LOCALE_FONTSIGNATURE,
        PChar(@ALocaleSignature), SizeOf(ALocaleSignature) div SizeOf(Char));
      Move(ALocaleSignature.lsUsb, FUnicodeSubrangeBits,
        SizeOf(FUnicodeSubrangeBits));
    end;
    ADC := ACanvas.Handle;
    AOldFont := SelectObject(ADC, GdiFontHandle);
    try
      if Integer(GetTextCharsetInfo(ADC, @AFontSignature, 0)) = DEFAULT_CHARSET
      then
        Result.Clear
      else
        Move(AFontSignature.fsUsb, FUnicodeSubrangeBits,
          SizeOf(FUnicodeSubrangeBits));
    finally
      SelectObject(ADC, AOldFont);
    end;
    FUnicodeSubrangeBitsCalculated := True;
  end;
  Result.Data := FUnicodeSubrangeBits;
end;

procedure TdxFontInfo.CalculateSuperscriptOffset(ABaseFontInfo: TdxFontInfo);
var
  Y, AResult: Integer;
begin
  AResult := ABaseFontInfo.SuperscriptOffset.Y;
  Y := ABaseFontInfo.AscentAndFree - AscentAndFree + AResult;
  if Y < 0 then
    Dec(AResult, Y);
  FSuperscriptOffset := TPoint.Create(SuperscriptOffset.X, AResult);
end;

procedure TdxFontInfo.CalculateSubscriptOffset(ABaseFontInfo: TdxFontInfo);
var
  Y, AMaxOffset: Integer;
begin
  Y := ABaseFontInfo.SubscriptOffset.Y;
  AMaxOffset := ABaseFontInfo.LineSpacing - LineSpacing + AscentAndFree -
    ABaseFontInfo.AscentAndFree;
  if Y > AMaxOffset then
    Y := AMaxOffset;
  FSubscriptOffset := TPoint.Create(SubscriptOffset.X, Y);
end;

function TdxFontInfo.GetAscentAndFree: Integer;
begin
  Result := FAscent + FFree;
end;

function TdxFontInfo.GetBold: Boolean;
begin
  Result := fsBold in FStyle;
end;

function TdxFontInfo.GetItalic: Boolean;
begin
  Result := fsItalic in FStyle;
end;

function TdxFontInfo.GetUnderline: Boolean;
begin
  Result := fsUnderline in FStyle;
end;

procedure TdxFontInfo.AdjustFontParameters;
begin
  FFree := FLineSpacing - FAscent - FDescent;
  if FFree < 0 then
  begin
    FDescent := FDescent + FFree;
    if FDescent < 0 then
    begin
      FAscent := FAscent + FDescent;
      FDescent := 0;
    end;
    FFree := 0;
  end;
end;

procedure TdxFontInfo.CalculateFontParameters(AMeasurer: TdxFontInfoMeasurer;
  AAllowCjkCorrection, AIsOfficeFont: Boolean);
begin
  CalculateFontVerticalParameters(AMeasurer, AAllowCjkCorrection,
    AIsOfficeFont);
  AdjustFontParameters;
  FSpaceWidth := Round(AMeasurer.MeasureCharacterWidthF
    (TdxCharacters.Space, Self));
  FPilcrowSignWidth := AMeasurer.MeasureCharacterWidth
    (TdxCharacters.PilcrowSign, Self);
  FNonBreakingSpaceWidth := AMeasurer.MeasureCharacterWidth
    (TdxCharacters.NonBreakingSpace, Self);
  FUnderscoreWidth := AMeasurer.MeasureCharacterWidth
    (TdxCharacters.Underscore, Self);
  FMiddleDotWidth := AMeasurer.MeasureCharacterWidth
    (TdxCharacters.MiddleDot, Self);
  FDotWidth := AMeasurer.MeasureCharacterWidth(TdxCharacters.Dot, Self);
  FDashWidth := AMeasurer.MeasureCharacterWidth(TdxCharacters.Dash, Self);
  FEqualSignWidth := AMeasurer.MeasureCharacterWidth
    (TdxCharacters.EqualSign, Self);
  FCharset := CalculateFontCharset(AMeasurer);
  FMaxDigitWidth := CalculateMaxDigitWidth(AMeasurer);
end;

function TdxFontInfo.GetBaseAscentAndFree(const AContainer: IdxFontsContainer)
  : Integer;
begin
  Result := GetBaseFontInfo(AContainer).AscentAndFree;
end;

function TdxFontInfo.GetBaseDescent(const AContainer: IdxFontsContainer)
  : Integer;
begin
  Result := GetBaseFontInfo(AContainer).Descent;
end;

function TdxFontInfo.GetBaseFontInfo(const AContainer: IdxFontsContainer)
  : TdxFontInfo;
begin
  if FBaseFontIndex >= 0 then
    Result := AContainer.FontCache[FBaseFontIndex]
  else
    Result := Self;
end;

function TdxFontInfo.CalculateMaxDigitWidth
  (AMeasurer: TdxFontInfoMeasurer): Single;
begin
  Result := AMeasurer.MeasureMaxDigitWidthF(Self);
end;

procedure TdxFontInfo.CreateFont(AMeasurer: TdxFontInfoMeasurer;
  const AName: string; ASize: Integer; const AFontStyle: TFontStyles;
  AIsOfficeFont: Boolean);
begin
  FGdiFontHandle := TdxGdiFontHelper.CreateFont(AMeasurer.Canvas.Handle, AName,
    FSizeInPoints, AFontStyle, AMeasurer.UnitConverter.FontSizeScale,
    FFamilyName, AIsOfficeFont);
end;

procedure TdxFontInfo.CalculateCJKFontParameters(AOutlineTextMetric
  : POutlineTextmetric);
var
  AHalfTmpExtLeading, AOtherHalfTmpExtLeading, ANewLineSpacing, ADelta,
    AAscentWithoutFree: Integer;
begin
  if (AOutlineTextMetric.otmfsSelection and 128) <> 0 then
  begin
    FAscent := AOutlineTextMetric.otmAscent;
    FDescent := -AOutlineTextMetric.otmDescent;
    FLineSpacing := Ascent + Descent + Integer(AOutlineTextMetric.otmLineGap);
    FDrawingOffset := AOutlineTextMetric.otmTextMetrics.tmAscent - Ascent;
  end
  else
  begin
    FCjkUnderlinePosition := Ceil(Ascent * 1.15 + Descent * 0.85);
    ANewLineSpacing := Ceil(1.3 * (Ascent + Descent));
    ADelta := ANewLineSpacing - (Ascent + Descent);
    AHalfTmpExtLeading := ADelta div 2;
    AOtherHalfTmpExtLeading := ADelta - AHalfTmpExtLeading;
    Inc(FAscent, AHalfTmpExtLeading);
    Inc(FDescent, AOtherHalfTmpExtLeading);
    FLineSpacing := ANewLineSpacing;
    FCjkUnderlinePosition := FCjkUnderlinePosition - Ascent;
    FApplyCjkUnderline := True;
    AAscentWithoutFree := LineSpacing - Descent;
    FDrawingOffset := AOutlineTextMetric.otmTextMetrics.tmAscent -
      AAscentWithoutFree;
  end;
end;

function TdxFontInfo.CalculateFontCharset
  (AMeasurer: TdxFontInfoMeasurer): Integer;
var
  ADC: HDC;
  AOldFontHandle: HFONT;
begin
  ADC := AMeasurer.Canvas.Handle;
  AOldFontHandle := SelectObject(ADC, GdiFontHandle);
  try
    Result := GetTextCharset(ADC);
  finally
    SelectObject(ADC, AOldFontHandle);
  end;
end;

{ TdxFontInfoMeasurer }

constructor TdxFontInfoMeasurer.Create(AUnitConverter
  : TdxDocumentLayoutUnitConverter);
begin
  inherited Create;
  FUnitConverter := AUnitConverter;
  FDpi := AUnitConverter.Dpi;
  FCanvas := CreateMeasureGraphics;
end;

destructor TdxFontInfoMeasurer.Destroy;
begin
  DeleteDC(FCanvas.Handle);
  FCanvas.Handle := 0;
  FCanvas.Free;
  inherited Destroy;
end;

function TdxFontInfoMeasurer.MeasureCharacterWidth(ACharacter: Char;
  AFontInfo: TdxFontInfo): Integer;
begin
  Result := Ceil(MeasureCharacterWidthF(ACharacter, AFontInfo));
end;

function TdxFontInfoMeasurer.CreateMeasureGraphics: TCanvas;
begin
  Result := TCanvas.Create;
  Result.Handle := CreateCompatibleDC(0);
end;

function TdxFontInfoMeasurer.MeasureCharacterWidthF(ACharacter: Char;
  AFontInfo: TdxFontInfo): Single;
var
  ACharacterSize: TSize;
begin
  SelectObject(Canvas.Handle, AFontInfo.GdiFontHandle);
  ACharacterSize := Canvas.TextExtent(ACharacter);
  Result := ACharacterSize.cx;
end;

function TdxFontInfoMeasurer.MeasureString(const AText: string;
  AFontInfo: TdxFontInfo): TSize;
begin
  SelectObject(Canvas.Handle, AFontInfo.GdiFontHandle);
  Result := Canvas.TextExtent(AText);
end;

function TdxFontInfoMeasurer.MeasureMaxDigitWidthF
  (AFontInfo: TdxFontInfo): Single;
type
  TDigitWidths = packed array [0 .. 9] of TABCFloat;
var
  I: Integer;
  ADigitWidths: TDigitWidths;
  AFont, ADC: THandle;
  AWidth: Single;
begin
  AWidth := 0;
  ADC := Canvas.Handle;
  AFont := AFontInfo.GdiFontHandle;
  SelectObject(ADC, AFont);
  if GetCharABCWidthsFloat(ADC, Ord('0'), Ord('9'), ADigitWidths) then
  begin
    for I := Low(ADigitWidths) to High(ADigitWidths) do
      with ADigitWidths[I] do
        AWidth := Max(AWidth, abcfA + abcfB + abcfC);
  end;
  if AWidth > 0 then
    Result := UnitConverter.PixelsToLayoutUnitsF(AWidth, FDpi)
  else
    Result := 0;
end;

{ TdxTrueTypeFontInfo }

constructor TdxTrueTypeFontInfo.Create(const AFontName: string;
  const AFontType: Integer);
begin
  inherited Create;
  FFontName := AFontName;
  FFontType := AFontType;
end;

function TdxTrueTypeFontInfo.GetSupportedFontStyles: TdxSupportedFontStylesInfo;
begin
  Result := TdxGdiFontHelper.GetFontStyles(FFontName);
  FSupportedStylesCalculated := True;
  Result.NativeStyles[TdxSupportedFontStyle.Regular] := True;
  if Result.SupportedStyles = [TdxSupportedFontStyle.Italic] then
  begin
    Result.SupportedStyles := [TdxSupportedFontStyle.Regular,
      TdxSupportedFontStyle.BoldItalic];
    Exit;
  end;
  if Result.SupportedStyles = [TdxSupportedFontStyle.Bold] then
  begin
    Result.SupportedStyles := [TdxSupportedFontStyle.Regular,
      TdxSupportedFontStyle.BoldItalic];
    Exit;
  end;
  if (TdxSupportedFontStyle.Regular in Result.SupportedStyles) or
    (Result.SupportedStyles = []) then
    Result.SupportedStyles := [TdxSupportedFontStyle.Regular,
      TdxSupportedFontStyle.Bold, TdxSupportedFontStyle.Italic,
      TdxSupportedFontStyle.BoldItalic]
  else
    Include(Result.SupportedStyles, TdxSupportedFontStyle.BoldItalic);
end;

function TdxTrueTypeFontInfo.GetStylesInfo: TdxSupportedFontStylesInfo;
begin
  if not FSupportedStylesCalculated then
    FSupportedStyles := GetSupportedFontStyles;
  Result := FSupportedStyles;
end;

{ TdxFontCache.TTrueTypeFontLoader }

class function TdxFontCache.TTrueTypeFontLoader.EnumFontInfoProc
  (var ALogFont: TLogFontW; ATextMetric: PTextMetricW; AFontType: Integer;
  AData: Pointer): Integer;
var
  ATemp: string;
  ACallbackData: PCallbackData absolute AData;
begin
  if AFontType = TRUETYPE_FONTTYPE then
  begin
{$IFDEF DELPHIXE4}
    ATemp := string(PChar(@ALogFont.lfFaceName)).ToUpper;
{$ELSE}
    ATemp := ToUpper(PChar(@ALogFont.lfFaceName));
{$ENDIF}
    if (ATemp <> '') and (ATemp[1] <> '@') then
    begin
      if not SystemTrueTypeFonts.ContainsKey(ATemp) then
      begin
        if ALogFont.lfCharSet = SYMBOL_CHARSET then
          AFontType := AFontType or TdxTrueTypeFontInfo.SymbolFontType;
        if ALogFont.lfPitchAndFamily and FIXED_PITCH = FIXED_PITCH then
          AFontType := AFontType or TdxTrueTypeFontInfo.FixedPitchFontType;
        SystemTrueTypeFonts.Add(ATemp,
          TdxTrueTypeFontInfo.Create(PChar(@ALogFont.lfFaceName), AFontType));
      end;
    end;
  end;
  Result := Ord(ACallbackData.Thread.CanContinue);
end;

procedure TdxFontCache.TTrueTypeFontLoader.Execute;
var
  ALogFont: TLogFontW;
  ADC: HDC;
  AFontName: string;
  AFontCharacterSet: TdxFontCharacterSet;
  ACallbackData: TCallbackData;
begin
  System.TMonitor.Enter(TdxFontCache.NameToCharacterSetMap);
  ADC := CreateCompatibleDC(0);
  try
    ACallbackData.Thread := Self;
    FillChar(ALogFont, SizeOf(ALogFont), 0);
    ALogFont.lfCharSet := DEFAULT_CHARSET;
    dxEnumFontFamilies(ADC, ALogFont, @EnumFontInfoProc,
      TdxNativeInt(@ACallbackData), 0);
    if CanContinue then
    begin
      for AFontName in SystemTrueTypeFonts.Keys do
      begin
        if not TdxFontCache.NameToCharacterSetMap.ContainsKey(AFontName) then
        begin
          AFontCharacterSet := CreateFontCharacterSet(ADC, AFontName);
          if AFontCharacterSet <> nil then
            TdxFontCache.NameToCharacterSetMap.Add(AFontName,
              AFontCharacterSet);
        end;
        if not CanContinue then
          Break;
      end;
    end;
  finally
    DeleteDC(ADC);
    System.TMonitor.Exit(TdxFontCache.NameToCharacterSetMap);
  end;
end;

function TdxFontCache.TTrueTypeFontLoader.CanContinue: Boolean;
begin
  Result := not Terminated and
    ((Application = nil) or not Application.Terminated);
end;

function TdxFontCache.TTrueTypeFontLoader.CreateFontCharacterSet(ADC: HDC;
  const AFontName: string): TdxFontCharacterSet;
var
  AFont, AOldFont: HFONT;
  ACharacterRanges: TArray<TdxFontCharacterRange>;
  AOutlineTextMetrics: TOutlineTextmetric;
  ADummy: string;
begin
  AFont := TdxGdiFontHelper.CreateFont(ADC, AFontName, 22, [], 1,
    ADummy, False);
  if AFont = 0 then
    Exit(nil);
  AOldFont := SelectObject(ADC, AFont);
  try
    ACharacterRanges := TdxFontInfo.GetFontUnicodeRanges(ADC, 0);
    AOutlineTextMetrics.otmSize := SizeOf(AOutlineTextMetrics);
    if Windows.GetOutlineTextMetricsW(ADC, SizeOf(AOutlineTextMetrics),
      @AOutlineTextMetrics) <> 0 then
      Result := TdxFontCharacterSet.Create(ACharacterRanges,
        AOutlineTextMetrics.otmPanoseNumber)
    else
      Result := nil;
  finally
    SelectObject(ADC, AOldFont);
    DeleteObject(AFont);
    ACharacterRanges := nil;
  end;
end;

{ TdxFontCache }

constructor TdxFontCache.Create(const AUnit: TdxDocumentLayoutUnit;
  const ADpi: Single; AUseOfficeFonts: Boolean);
begin
  inherited Create;
  FUseOfficeFonts := AUseOfficeFonts;
  FUnitConverter := TdxDocumentLayoutUnitConverter.CreateConverter(AUnit, ADpi);
  FIndexHash := TDictionary<Int64, Integer>.Create;
  FItems := TdxFastObjectList.Create;
  FMeasurer := CreateFontInfoMeasurer(FUnitConverter);
  FCharsets := TDictionary<string, Integer>.Create;
  CheckInitialize;
end;

destructor TdxFontCache.Destroy;
begin
  FreeAndNil(FIndexHash);
  FreeAndNil(FCharsets);
  FreeAndNil(FMeasurer);
  FreeAndNil(FItems);
  FreeAndNil(FUnitConverter);
  inherited Destroy;
end;

class procedure TdxFontCache.Initialize;
begin
  FNameToCharacterSetMap := TObjectDictionary<string, TdxFontCharacterSet>.
    Create([doOwnsValues], 512);
  FSystemTrueTypeFonts := TdxSystemTrueTypeFontDictionary.Create
    ([doOwnsValues]);
  FUnicodeRangeInfo := TdxUnicodeRangeInfo.Create;
{$IFNDEF DXUSEINCOMSERVERDLL}
{$ENDIF}
end;

class procedure TdxFontCache.Finalize;
begin
  if Assigned(FLoader) then
  begin
    FLoader.Terminate;
    FLoader.WaitFor;
    FreeAndNil(FLoader);
  end;
  FreeAndNil(FUnicodeRangeInfo);
  FreeAndNil(FSystemTrueTypeFonts);
  FreeAndNil(FNameToCharacterSetMap);
end;

function TdxFontCache.CreateOverrideFontSubstitutes
  : TdxSystemTrueTypeFontDictionary;
begin
  Result := TdxSystemTrueTypeFontDictionary.Create([doOwnsValues]);
end;

class procedure TdxFontCache.CheckInitialize;
begin
  if FLoader = nil then
    FLoader := TTrueTypeFontLoader.Create(False);
end;

class function TdxFontCache.CreateSystemTrueTypeFonts: TStrings;
var
  AValue: TdxTrueTypeFontInfo;
  AStrings: TStringList;
begin
  AStrings := TStringList.Create;
  for AValue in FSystemTrueTypeFonts.Values do
    AStrings.AddObject(AValue.FontName, AValue);
  AStrings.Sort;
  Result := AStrings;
end;

function TdxFontCache.CreateFontInfoMeasurer(AUnitConverter
  : TdxDocumentLayoutUnitConverter): TdxFontInfoMeasurer;
begin
  Result := TdxFontInfoMeasurer.Create(AUnitConverter);
end;

function TdxFontCache.CreateFontInfoCore(const AFontName: string;
  ADoubleFontSize: Integer; const AFontStyle: TFontStyles): TdxFontInfo;
begin
  Result := TdxFontInfo.Create(Measurer, AFontName, ADoubleFontSize, AFontStyle,
    True, FUseOfficeFonts);
end;

function TdxFontCache.GetFontCharacterRanges(AFontInfo: TdxFontInfo)
  : TArray<TdxFontCharacterRange>;
begin
  Result := AFontInfo.GetFontUnicodeRanges(Measurer.Canvas.Handle,
    AFontInfo.GdiFontHandle);
end;

function TdxFontCache.ShouldUseDefaultFontToDrawInvisibleCharacter
  (AFontInfo: TdxFontInfo; ACharacter: Char): Boolean;
begin
  Result := AFontInfo.CanDrawCharacter(UnicodeRangeInfo, Measurer.Canvas,
    ACharacter);
end;

function TdxFontCache.GetFontCharacterSet(const AFontName: string)
  : TdxFontCharacterSet;
var
  AUpperFontName: string;
begin
{$IFDEF DELPHIXE4}
  AUpperFontName := AFontName.ToUpper;
{$ELSE}
  AUpperFontName := ToUpper(AFontName);
{$ENDIF}
  System.TMonitor.Enter(NameToCharacterSetMap);
  try
    if not NameToCharacterSetMap.TryGetValue(AUpperFontName, Result) then
    begin
      Result := CreateFontCharacterSet(AFontName);
      if Result <> nil then
        NameToCharacterSetMap.Add(AUpperFontName, Result);
    end;
  finally
    System.TMonitor.Exit(NameToCharacterSetMap);
  end;
end;

function TdxFontCache.FindSubstituteFont(const ASourceFontName: string;
  ACharacter: Char; var AFontCharacterSet: TdxFontCharacterSet): string;
var
  AMinDistance, ADistance: Integer;
  ASourceCharacterSet, ATestFontCharacterSet: TdxFontCharacterSet;
  AFontCharacterSetPair: TPair<string, TdxFontCharacterSet>;
  AUpperFontName, ASubstituteFontName: string;
  ACharCodePages, ACodePage: DWORD;
begin
  Result := ASourceFontName;
  System.TMonitor.Enter(NameToCharacterSetMap);
  try
{$IFDEF DELPHIXE4}
    AUpperFontName := ASourceFontName.ToUpper;
{$ELSE}
    AUpperFontName := ToUpper(ASourceFontName);
{$ENDIF}
    if not FSystemTrueTypeFonts.ContainsKey(AUpperFontName) then
    begin
      if FontLink = nil then
        ACodePage := GetACP
      else if FontLink.GetCharCodePages(ACharacter, ACharCodePages) <> S_OK then
        ACodePage := GetACP
      else if FontLink.CodePagesToCodePage(ACharCodePages, CP_ACP, ACodePage) <> S_OK
      then
        ACodePage := GetACP;
      case ACodePage of
        1250 .. 1259:
          ASubstituteFontName := 'TIMES NEW ROMAN';
        932:
          ASubstituteFontName := 'MS MINCHO';
        949:
          ASubstituteFontName := 'GULIMCHE';
        936:
          ASubstituteFontName := 'MS SONG';
        950:
          ASubstituteFontName := 'NEW MINGLIU';
        874:
          ASubstituteFontName := 'TAHOMA';
      else
        ASubstituteFontName := '';
      end;
      if (ASubstituteFontName <> '') and FSystemTrueTypeFonts.ContainsKey
        (ASubstituteFontName) then
      begin
        AFontCharacterSet := NameToCharacterSetMap.Items[ASubstituteFontName];
        if AFontCharacterSet.ContainsChar(ACharacter) then
          Exit(ASubstituteFontName);
      end;
    end;
    AMinDistance := MaxInt;
    ASourceCharacterSet := NameToCharacterSetMap[AUpperFontName];
    for AFontCharacterSetPair in NameToCharacterSetMap do
    begin
      ATestFontCharacterSet := AFontCharacterSetPair.Value;
      if ATestFontCharacterSet.ContainsChar(ACharacter) then
      begin
        ADistance := TdxFontCharacterSet.CalculatePanoseDistance
          (ASourceCharacterSet, ATestFontCharacterSet);
        if ADistance < AMinDistance then
        begin
          AMinDistance := ADistance;
          Result := AFontCharacterSetPair.Key;
          if AMinDistance = 0 then
            Break;
        end;
      end;
    end;
  finally
    System.TMonitor.Exit(NameToCharacterSetMap);
  end;
end;

function TdxFontCache.CreateFontCharacterSet(const AFontName: string)
  : TdxFontCharacterSet;
var
  ACharacterRanges: TArray<TdxFontCharacterRange>;
  AFontInfo: TdxFontInfo;
begin
  AFontInfo := TdxFontInfo.Create(Measurer, AFontName, 20, [], True,
    FUseOfficeFonts);
  try
    ACharacterRanges := GetFontCharacterRanges(AFontInfo);
    Result := TdxFontCharacterSet.Create(ACharacterRanges, AFontInfo.Panose);
  finally
    AFontInfo.Free;
    ACharacterRanges := nil;
  end;
end;

function TdxFontCache.CalcFontIndex(const AFontName: string;
  ADoubleFontSize: Integer; const AFontStyle: TFontStyles;
  AScript: TdxCharacterFormattingScript): Integer;
var
  ADoubleFontSizeInLayoutFontUnits: Integer;
begin
  ADoubleFontSizeInLayoutFontUnits := UnitConverter.PointsToFontUnits
    (ADoubleFontSize);
  case AScript of
    TdxCharacterFormattingScript.Normal:
      Result := CalcNormalFontIndex(AFontName, ADoubleFontSizeInLayoutFontUnits,
        AFontStyle);
    TdxCharacterFormattingScript.Subscript:
      Result := CalcSubscriptFontIndex(AFontName,
        ADoubleFontSizeInLayoutFontUnits, AFontStyle);
  else
    Result := CalcSuperscriptFontIndex(AFontName,
      ADoubleFontSizeInLayoutFontUnits, AFontStyle);
  end;
end;

function TdxFontCache.CalcSubscriptFontSize(ABaseFontIndex: Integer): Integer;
var
  AFontInfo: TdxFontInfo;
begin
  AFontInfo := Items[ABaseFontIndex];
  Result := Round(AFontInfo.SubscriptSize.cy * UnitConverter.FontSizeScale);
end;

function TdxFontCache.CalcSuperscriptFontSize(ABaseFontIndex: Integer): Integer;
var
  AFontInfo: TdxFontInfo;
begin
  AFontInfo := Items[ABaseFontIndex];
  Result := Round(AFontInfo.SuperscriptSize.cy * UnitConverter.FontSizeScale);
end;

function TdxFontCache.CalcNormalFontIndex(const AFontName: string;
  ADoubleFontSize: Integer; const AFontStyle: TFontStyles): Integer;
begin
  Result := CalcFontIndexCore(AFontName, ADoubleFontSize, AFontStyle,
    TdxCharacterFormattingScript.Normal);
end;

function TdxFontCache.CalcSuperscriptFontIndex(const AFontName: string;
  ADoubleFontSize: Integer; const AFontStyle: TFontStyles): Integer;
var
  AFontInfo: TdxFontInfo;
  ASuperscriptFontSize: Integer;
  ABaseFontIndex: Integer;
begin
  ABaseFontIndex := CalcFontIndexCore(AFontName, ADoubleFontSize, AFontStyle,
    TdxCharacterFormattingScript.Normal);
  ASuperscriptFontSize := CalcSuperscriptFontSize(ABaseFontIndex);
  Result := CalcFontIndexCore(AFontName, ASuperscriptFontSize * 2, AFontStyle,
    TdxCharacterFormattingScript.Superscript);
  AFontInfo := Items[Result];
  AFontInfo.CalculateSuperscriptOffset(Items[ABaseFontIndex]);
  AFontInfo.BaseFontIndex := ABaseFontIndex;
end;

function TdxFontCache.CalcSubscriptFontIndex(const AFontName: string;
  ADoubleFontSize: Integer; const AFontStyle: TFontStyles): Integer;
var
  AFontInfo: TdxFontInfo;
  ASubscriptFontSize: Integer;
  ABaseFontIndex: Integer;
begin
  ABaseFontIndex := CalcFontIndexCore(AFontName, ADoubleFontSize, AFontStyle,
    TdxCharacterFormattingScript.Normal);
  ASubscriptFontSize := CalcSubscriptFontSize(ABaseFontIndex);
  Result := CalcFontIndexCore(AFontName, ASubscriptFontSize * 2, AFontStyle,
    TdxCharacterFormattingScript.Subscript);
  AFontInfo := Items[Result];
  AFontInfo.CalculateSubscriptOffset(Items[ABaseFontIndex]);
  AFontInfo.BaseFontIndex := ABaseFontIndex;
end;

function TdxFontCache.CalcFontIndexCore(const AFontName: string;
  ADoubleFontSize: Integer; const AFontStyle: TFontStyles;
  AScript: TdxCharacterFormattingScript): Integer;
var
  AHash: Int64;
  AUpperFontName: string;
begin
{$IFDEF DELPHIXE4}
  AUpperFontName := AFontName.ToUpper;
{$ELSE}
  AUpperFontName := ToUpper(AFontName);
{$ENDIF}
  AHash := CalcHash(AUpperFontName, ADoubleFontSize, AFontStyle, AScript);
  if not IndexHash.TryGetValue(AHash, Result) then
  begin
    System.TMonitor.Enter(Self);
    try
      if not IndexHash.TryGetValue(AHash, Result) then
        Result := CreateFontInfo(AHash, AFontName, ADoubleFontSize, AFontStyle);
    finally
      System.TMonitor.Exit(Self);
    end;
  end;
end;

function TdxFontCache.CalcHash(const AFontName: string;
  ADoubleFontSize: Integer; const AFontStyle: TFontStyles;
  AScript: TdxCharacterFormattingScript): Int64;
var
  AStyleValue: Integer;
  AFontStyleByte: Byte absolute AFontStyle;
begin
  AStyleValue := Ord(AScript) or (AFontStyleByte shl 2);
  Int64Rec(Result).Lo := (ADoubleFontSize shl 6) or AStyleValue;
  Int64Rec(Result).Hi := dxElfHash(PChar(AFontName), Length(AFontName), nil, 0);
end;

function TdxFontCache.CreateFontInfo(const AHash: Int64;
  const AFontName: string; ADoubleFontSize: Integer;
  const AFontStyle: TFontStyles): Integer;
var
  AFontInfo: TdxFontInfo;
  AUpperFontName: string;
begin
  AFontInfo := CreateFontInfoCore(AFontName, ADoubleFontSize, AFontStyle);
  Result := FItems.Add(AFontInfo);
  IndexHash.Add(AHash, Result);
{$IFDEF DELPHIXE4}
  AUpperFontName := AFontName.ToUpper;
{$ELSE}
  AUpperFontName := ToUpper(AFontName);
{$ENDIF}
  FCharsets.AddOrSetValue(AUpperFontName, AFontInfo.Charset);
end;

function TdxFontCache.GetCharsetByFontName(const AFontName: string): Integer;
var
  AUpperFontName: string;
begin
{$IFDEF DELPHIXE4}
  AUpperFontName := AFontName.ToUpper;
{$ELSE}
  AUpperFontName := ToUpper(AFontName);
{$ENDIF}
  if not FCharsets.TryGetValue(AUpperFontName, Result) then
  begin
    CalcFontIndex(AUpperFontName, 24, [], TdxCharacterFormattingScript.Normal);
    Result := FCharsets[AUpperFontName];
  end;
end;

function TdxFontCache.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxFontCache.GetItem(AIndex: Integer): TdxFontInfo;
begin
  Result := TdxFontInfo(FItems[AIndex]);
end;

{ TdxFontCacheManager }

class constructor TdxFontCacheManager.Initialize;
begin
  FVCLItems := TObjectList<TdxFontCache>.Create;
  FOfficeItems := TObjectList<TdxFontCache>.Create;
end;

{$HINTS OFF}

class destructor TdxFontCacheManager.Finalize;
begin
  FreeAndNil(FOfficeItems);
  FreeAndNil(FVCLItems);
end;
{$HINTS ON}

class function TdxFontCacheManager.GetFontCache(const AUnit
  : TdxDocumentLayoutUnit; const ADpi: Single): TdxFontCache;
begin
  Result := GetOfficeFontCache(AUnit, ADpi);
end;

class function TdxFontCacheManager.GetOfficeFontCache
  (const AUnit: TdxDocumentLayoutUnit; const ADpi: Single): TdxFontCache;
var
  I: Integer;
  AUnitConverter: TdxDocumentLayoutUnitConverter;
begin
  AUnitConverter := TdxDocumentLayoutUnitConverter.CreateConverter(AUnit, ADpi);
  try
    for I := 0 to FOfficeItems.Count - 1 do
      if FOfficeItems[I].UnitConverter.Equals(AUnitConverter) then
        Exit(FOfficeItems[I]);
  finally
    AUnitConverter.Free;
  end;

  Result := TdxFontCache.Create(AUnit, ADpi, True);
  FOfficeItems.Add(Result);
end;

class function TdxFontCacheManager.GetVCLFontCache
  (const AUnit: TdxDocumentLayoutUnit; const ADpi: Single): TdxFontCache;
var
  I: Integer;
  AUnitConverter: TdxDocumentLayoutUnitConverter;
begin
  AUnitConverter := TdxDocumentLayoutUnitConverter.CreateConverter(AUnit, ADpi);
  try
    for I := 0 to FVCLItems.Count - 1 do
      if FVCLItems[I].UnitConverter.Equals(AUnitConverter) then
        Exit(FVCLItems[I]);
  finally
    AUnitConverter.Free;
  end;

  Result := TdxFontCache.Create(AUnit, ADpi, False);
  FVCLItems.Add(Result);
end;

function TryCreateFontLink: IMLangFontLink;
{$IFDEF CPUX86}
var
  ASaveControlWord: Word;
{$ENDIF CPUX86}
begin
{$IFDEF CPUX86}
  ASaveControlWord := Get8087CW;
  try
    Set8087CW(Default8087CW or $08);
{$ENDIF CPUX86}
    if not Succeeded(CoCreateInstance(CLASS_CMultiLanguage, nil,
      CLSCTX_INPROC_SERVER or CLSCTX_LOCAL_SERVER, IMLangFontLink, Result)) then
      Result := nil;
{$IFDEF CPUX86}
  finally
    Set8087CW(ASaveControlWord);
  end;
{$ENDIF CPUX86}
end;

procedure Initialize;
begin
  TdxFontCache.Initialize;
  FontLink := TryCreateFontLink;
end;

procedure Finalize;
begin
{$IFNDEF DXUSEINCOMSERVERDLL}
  if FontLink <> nil then
    FontLink.ResetFontMapping;
{$ENDIF}
  FontLink := nil;
  TdxFontCache.Finalize;
end;

initialization

if Assigned(ComObj.CoInitializeEx) then
  FNeedFinalize := Succeeded(ComObj.CoInitializeEx(nil, COINIT_MULTITHREADED))
else
  FNeedFinalize := Succeeded(CoInitialize(nil));
dxUnitsLoader.AddUnit(@Initialize, @Finalize);

finalization

dxUnitsLoader.RemoveUnit(@Finalize);
if FNeedFinalize then
  CoUninitialize;

end.
