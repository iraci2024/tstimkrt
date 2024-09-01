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

unit dxPDFFontUtils;

{$I cxVer.inc}

interface

uses
  Types, SysUtils, Generics.Defaults, Generics.Collections, Classes, Graphics,
  Windows, dxCoreClasses, cxVariants,
  cxGeometry, dxGDIPlusClasses, dxPDFBase, dxPDFTypes, dxPDFCore, dxPDFFont,
  dxFontFile, dxPDFText,
  dxPDFCharacterMapping, dxPDFFontEncoding, dxPDFType1Font;

const
  sdxPDFTempFolder: string = '';

type
  TdxPDFCFFFontProgramFacade = class;
  TdxPDFTrueTypeFontProgramFacade = class;
  TdxPDFType1FontProgramFacade = class;

  { IdxPDFFontObject }

  IdxPDFFontObject = interface
    ['{F037419E-C902-4EC9-B664-2B0F3EC52615}']
    function GetFont: TdxPDFCustomFont;
    function IsUnsupportedFont: Boolean;
    procedure AddSubset(AMapping: TdxPDFIntegerStringDictionary);
    procedure UpdateFont;
  end;

  TdxPDFFontCharset = (fcBasic, fcGB1, fcCNS1, fcJapan1, fcKorea1);

  { TdxPDFCIDCharset }

  TdxPDFCIDCharset = class(TObject)
  public
    function GetUnicode(ACid: SmallInt): SmallInt;
  end;

  { IdxPDFCodePointMapping }

  IdxPDFCodePointMapping = interface
    ['{E6081248-D7EC-4705-A5DC-AF26C65597A5}']
    function UpdateCodePoints(const ACodePoints: TSmallIntDynArray;
      AUseEmbeddedFontEncoding: Boolean): Boolean;
  end;

  { TdxPDFCodePointMapping }

  TdxPDFCodePointMapping = class(TcxIUnknownObject, IdxPDFCodePointMapping)
  public
    function UpdateCodePoints(const ACodePoints: TSmallIntDynArray;
      AUseEmbeddedFontEncoding: Boolean): Boolean; virtual; abstract;
  end;

  { TdxPDFFontCustomRegistrator }

  TdxPDFFontCustomRegistrator = class
  strict private
    FFont: TdxPDFCustomFont;
    FFontSubstitutionHelper: TdxPDFGDIFontSubstitutionHelper;
  protected
    function PerformRegister: string; virtual; abstract;
    procedure Unregister; virtual; abstract;

    property Font: TdxPDFCustomFont read FFont;
    property FontSubstitutionHelper: TdxPDFGDIFontSubstitutionHelper
      read FFontSubstitutionHelper;
  public
    constructor Create(AFontSubstitutionHelper: TdxPDFGDIFontSubstitutionHelper;
      AFont: TdxPDFCustomFont); overload;
    destructor Destroy; override;

    function CreateSubstituteFontData: TdxPDFFontRegistrationData; virtual;

    class function CreateRegistrator(AFontSubstitutionHelper
      : TdxPDFGDIFontSubstitutionHelper; AFont: TdxPDFCustomFont;
      const AFolderName: string): TdxPDFFontCustomRegistrator;
    function Register: TdxPDFFontRegistrationData;
  end;

  { TdxPDFFontInMemoryRegistrator }

  TdxPDFFontInMemoryRegistrator = class(TdxPDFFontCustomRegistrator)
  strict private
    FFontFileData: TBytes;
    FFontHandle: THandle;
    function CreateHandle: Boolean;
    function HandleAllocated: Boolean;
  protected
    function PerformRegister: string; override;
    procedure Unregister; override;
  public
    constructor Create(AFontSubstitutionHelper: TdxPDFGDIFontSubstitutionHelper;
      AFont: TdxPDFCustomFont; const AFontFileData: TBytes);
  end;

  { TdxPDFOpenTypeFontCreator }

  TdxPDFOpenTypeFontCreator = class
  public
    class function GetFontFileData(AFont: TdxPDFCustomFont): TBytes;
  end;

  { TdxPDFTrueTypeFontRegistrator }

  TdxPDFTrueTypeFontRegistrator = class(TdxPDFFontInMemoryRegistrator)
  public
    constructor Create(AFontSubstitutionHelper: TdxPDFGDIFontSubstitutionHelper;
      AFont: TdxPDFCustomFont; AProgram: TdxPDFTrueTypeFontProgramFacade);
    function CreateSubstituteFontData: TdxPDFFontRegistrationData; override;
  end;

  { TdxPDFOpenTypeFontRegistrator }

  TdxPDFOpenTypeFontRegistrator = class(TdxPDFFontInMemoryRegistrator)
  public
    constructor Create(AFontSubstitutionHelper: TdxPDFGDIFontSubstitutionHelper;
      AFont: TdxPDFCustomFont);
    function CreateSubstituteFontData: TdxPDFFontRegistrationData; override;
  end;

  { TdxPDFType1FontRegistrator }

  TdxPDFType1FontRegistrator = class(TdxPDFFontCustomRegistrator)
  strict private
    FFontName: string;
    FFontResourceName: string;
    FPfbFileName: string;
    FPfmFileName: string;
    class procedure DeleteFontFile(const AFileName: string); static;
  protected
    function PerformRegister: string; override;
  public
    constructor Create(AFontSubstitutionHelper: TdxPDFGDIFontSubstitutionHelper;
      AFont: TdxPDFCustomFont; const AFontFolderName: string;
      AProgram: TdxPDFType1FontProgramFacade);
    destructor Destroy; override;

    procedure Unregister; override;
  end;

  TdxFontFileInfo = record
    Data: TBytes;
    Encoding: TSmallIntDynArray;
    GlyphMapping: TdxPDFWordDictionary;
    GlyphRanges: TList<TdxFontFileCMapGlyphRange>;
  end;

  { TdxFontFileHelper }

  TdxFontFileHelper = class
  strict private
    class function CheckVersion(const AFontFileData, AVersionData
      : TBytes): Boolean;
    class function CreateHeadTable(const AFontBBox: TdxPDFRectangle)
      : TdxFontFileHeadTable;
    class function CreateHheaTable(AFont: TdxPDFCustomFont;
      AGlyphCount, AAscender, ADescender: Integer): TdxFontFileHheaTable;
    class function CreateOS2Table(AFont: TdxPDFCustomFont;
      ACharset: TdxPDFSmallIntegerDictionary;
      AValidatedAscender, AValidatedDescender: Integer): TdxFontFileOS2Table;
    class function CreatePostTable(AFont: TdxPDFCustomFont)
      : TdxFontFilePostTable;
    class function GetOpenTypeVersion: TBytes;
    class function GetTTFVersion: TBytes;
    class function PatchCMapTable(AFile: TdxFontFile; AFont: TdxPDFCustomFont;
      var AEncoding: TSmallIntDynArray): TdxFontFileCMapSegmentMappingRecord;
    class procedure PatchNameTable(AFile: TdxFontFile;
      const ABaseFontName: string);
  public
    class function CreateOpenTypeFile(ACompactFontProgram
      : TdxPDFCFFFontProgramFacade; AFont: TdxPDFCustomFont): TdxFontFile;
    class function GetCFFData(const AData: TBytes): TBytes;
    class function GetFontFileInfo(AFont: TdxPDFCustomFont;
      const AFontData: TBytes): TdxFontFileInfo;
    class function GetSimpleMapping(AFile: TdxFontFile;
      AEncoding: TdxPDFSimpleFontEncoding; AIsSymbolic: Boolean)
      : TdxPDFCodePointMapping;
    class function GetValidCFFData(const AData: TBytes): TBytes;
    class procedure ValidateOS2Table(AFile: TdxFontFile;
      AFontDescriptor: TdxPDFCustomFontDescriptor);

    class function IsOpenType(const AFontFileData: TBytes): Boolean; static;
    class function IsTrueType(const AFontFileData: TBytes): Boolean; static;
  end;

  { TdxPDFFontDataFacade }

  TdxPDFFontDataFacade = class
  strict private
    FFontFamily: string;
    FFontMetrics: TdxFontFileFontMetrics;
    FFontName: string;
    FFontStyle: TdxGPFontStyle;

    function GetBold: Boolean;
    function GetItalic: Boolean;
    function GetUnderline: Boolean;
    function GetStrikeout: Boolean;
    function GetIsSymbolFont: Boolean;
  protected
    function GetEmulateBold: Boolean; virtual; abstract;
    function GetEmulateItalic: Boolean; virtual; abstract;
    function GetFontFileBasedName: string; virtual;
  public
    constructor Create(const AFontFamily: string; AFontStyle: TdxGPFontStyle;
      AFontMetrics: TdxFontFileFontMetrics);

    function CreateFontDescriptorData: TdxPDFFontDescriptorData;
      virtual; abstract;
    function CreateFontFileSubset(ASubset: TdxPDFIntegerStringDictionary)
      : TBytes; virtual; abstract;
    function GetGlyphWidth(AGlyphIndex: Integer): Double; virtual; abstract;

    property Bold: Boolean read GetBold;
    property EmulateBold: Boolean read GetEmulateBold;
    property EmulateItalic: Boolean read GetEmulateItalic;
    property FontFamily: string read FFontFamily;
    property FontFileBasedName: string read GetFontFileBasedName;
    property FontName: string read FFontName;
    property IsSymbolFont: Boolean read GetIsSymbolFont;
    property Italic: Boolean read GetItalic;
    property Metrics: TdxFontFileFontMetrics read FFontMetrics;
    property Strikeout: Boolean read GetStrikeout;
    property Style: TdxGPFontStyle read FFontStyle;
    property Underline: Boolean read GetUnderline;
  end;

  { TdxPDFFontFileDataFacade }

  TdxPDFFontFileDataFacade = class(TdxPDFFontDataFacade)
  strict private
    FFontFile: TdxFontFile;
    FFontFileBasedName: string;
    FEmulateBold: Boolean;
    FEmulateItalic: Boolean;
  protected
    function GetFontFileBasedName: string; override;
    function GetEmulateBold: Boolean; override;
    function GetEmulateItalic: Boolean; override;
  public
    constructor Create(const AFontFamily: string; AFontStyle: TdxGPFontStyle;
      AFontFile: TdxFontFile; AEmulateBold: Boolean);
    destructor Destroy; override;
    function CreateFontDescriptorData: TdxPDFFontDescriptorData; override;
    function CreateFontFileSubset(ASubset: TdxPDFIntegerStringDictionary)
      : TBytes; override;
    function GetGlyphWidth(AGlyphIndex: Integer): Double; override;
  end;

  { TdxPDFCharacterSet }

  TdxPDFCharacterSet = class
  strict private
    FCurrentSet: TdxPDFIntegerStringDictionary;
    FHasNewCharacters: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddSubset(AToUnicode: TdxPDFIntegerStringDictionary);

    property CurrentSet: TdxPDFIntegerStringDictionary read FCurrentSet;
    property HasNewCharacters: Boolean read FHasNewCharacters
      write FHasNewCharacters;
  end;

  { TdxPDFEditableFontObject }

  TdxPDFEditableFontObject = class(TInterfacedObject, IdxPDFFontObject)
  strict private
    FCharSet: TdxPDFCharacterSet;
    FShouldUseTwoByteGlyphIndex: Boolean;
  protected
    function GetFont: TdxPDFCustomFont; virtual; abstract;
    function IsUnsupportedFont: Boolean; virtual; abstract;
    procedure SetFont(const AValue: TdxPDFCustomFont); virtual; abstract;
    procedure UpdateFontFile(ASubset: TdxPDFIntegerStringDictionary);
      virtual; abstract;

    property Font: TdxPDFCustomFont read GetFont write SetFont;
  public
    constructor Create(AShouldUseTwoByteGlyphIndex: Boolean);
    destructor Destroy; override;

    procedure AddSubset(ASubset: TdxPDFIntegerStringDictionary);
    procedure UpdateFont;
  end;

  { TdxPDFEmbeddedEditableFontObject }

  TdxPDFEmbeddedEditableFontObject = class(TdxPDFEditableFontObject)
  strict private
    FFont: TdxPDFDeferredCIDType2Font;
    FFontDataFacade: TdxPDFFontDataFacade;
    FIsUnsupportedFont: Boolean;
  protected
    function GetFont: TdxPDFCustomFont; override;
    function IsUnsupportedFont: Boolean; override;
    procedure SetFont(const AValue: TdxPDFCustomFont); override;
    procedure UpdateFontFile(ASubset: TdxPDFIntegerStringDictionary); override;
  public
    constructor Create(AFontDataFacade: TdxPDFFontDataFacade;
      AIsUnsupportedFont: Boolean);
    destructor Destroy; override;
  end;

  { TdxPDFEditableFontData }

  TdxPDFEditableFontData = class(TdxPDFObject)
  strict private
    FFontDataFacade: TdxPDFFontDataFacade;
    FFontObject: IdxPDFFontObject;
    FFontStyle: TdxGPFontStyle;
    FMapper: TdxPDFGlyphMapper;

    function GetBold: Boolean;
    function GetFont: TdxPDFCustomFont;
    function GetItalic: Boolean;
    function GetMetrics: TdxFontFileFontMetrics;
    function GetNeedEmulateBold: Boolean;
    function GetNeedEmulateItalic: Boolean;
    function GetStrikeout: Boolean;
    function GetUnderline: Boolean;

    function GetCharacterWidth(ACh: Char): Double;
    function HasFlag(AFontStyle: TdxGPFontStyle): Boolean;
  protected
    procedure DestroySubClasses; override;

    property Bold: Boolean read GetBold;
    property FontStyle: TdxGPFontStyle read FFontStyle;
    property Italic: Boolean read GetItalic;
  public
    constructor Create(AFontObject: IdxPDFFontObject;
      AFontDataFacade: TdxPDFFontDataFacade; AMapper: TdxPDFGlyphMapper);
      reintroduce;

    function CreateGlyphRun: TdxPDFGlyphRun;
    function GetTextWidth(const AText: string; AFontSize: Double;
      ATextState: TdxPDFInteractiveFormFieldTextState): Double;
    function ProcessString(const AStr: string; AFlags: TdxPDFGlyphMappingFlags)
      : TdxPDFGlyphRun;

    procedure UpdateFont;
    procedure UpdateFontFile;

    property Font: TdxPDFCustomFont read GetFont;
    property Metrics: TdxFontFileFontMetrics read GetMetrics;
    property NeedEmulateBold: Boolean read GetNeedEmulateBold;
    property NeedEmulateItalic: Boolean read GetNeedEmulateItalic;
    property Strikeout: Boolean read GetStrikeout;
    property Underline: Boolean read GetUnderline;
  end;

  { TdxPDFEditableFontDataCache }

  TdxPDFEditableFontDataCache = class
  strict private
    FFontCache: TObjectDictionary<Integer, TdxPDFEditableFontData>;
    FFontFamilyInfos: TdxFontFamilyInfos;
    FLock: TRTLCriticalSection;
  strict protected
  type
    TCacheKey = record
      FontFamily: string;
      Style: TdxGPFontStyle;
      class function Create(const AFontFamily: string; AStyle: TdxGPFontStyle)
        : TCacheKey; static;
      function GetHash: Integer;
    end;
  protected
    function EmbedFont(const AFontFamily: string): Boolean;
    function TryGetEditableFontData(const AKey: TCacheKey;
      out AFontData: TdxPDFEditableFontData): Boolean;
    procedure CacheEditableFontData(const AKey: TCacheKey;
      AFontData: TdxPDFEditableFontData);

    property FontFamilyInfos: TdxFontFamilyInfos read FFontFamilyInfos;
  public
    constructor Create(AFontFamilyInfos: TdxFontFamilyInfos);
    destructor Destroy; override;

    procedure Clear;
    procedure UpdateFonts;
  end;

  { TdxPDFGDIEditableFontDataCache }

  TdxPDFGDIEditableFontDataCache = class(TdxPDFEditableFontDataCache)
  public
    function GetFontData(const AFontFamily: string; AFontStyle: TdxGPFontStyle;
      ACreateFont: Boolean): TdxPDFEditableFontData;
    function SearchFontData(const AFontFamilyName: string;
      AFontStyle: TdxGPFontStyle): TdxPDFEditableFontData;
  end;

  { TdxPDFType1FontFileCreator }

  TdxPDFType1FontFileCreator = class // for internal use
  strict private
  const
{$REGION 'private const'}
    DefaultNullSegment: array [0 .. 525] of Byte = ($0A, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30,
      $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $30, $0A, $63,
      $6C, $65, $61, $72, $74, $6F, $6D, $61, $72, $6B, $0D);
{$ENDREGION}
  strict private
    FAscent: SmallInt;
    FAvgWidth: SmallInt;
    FCapHeight: SmallInt;
    FDescent: SmallInt;
    FFont: TdxPDFCustomFont;
    FFontProgram: TdxPDFType1FontProgramFacade;
    FFontWeight: SmallInt;
    FHeight: SmallInt;
    FMaxWidth: SmallInt;
    FPitchAndFamily: Byte;
    FTop: SmallInt;
    FXHeight: SmallInt;

    FPfbStream: TFileStream;
    FPfbWriter: TcxWriter;
    FPfmStream: TFileStream;
    FPfmWriter: TcxWriter;

    function CreateStream(const AFileName: string): TFileStream;
    function ConvertToShort(AValue: Double): SmallInt;
    procedure WritePlainText(const AData: TBytes;
      APlainTextLength, ACipherTextLength: Integer);
    procedure WriteNullSegment(const AData: TBytes;
      ANullSegmentLength, APlainTextLength, ACipherTextLength: Integer);
  protected
    function CreateFiles: string;
    procedure WriteByte(AValue: Byte);
    procedure WriteInt(AWriter: TcxWriter; AValue: Integer);
    procedure WriteShort(AValue: SmallInt);
    procedure WriteString(const AValue: string);
  public
    constructor Create(AFont: TdxPDFCustomFont;
      AProgram: TdxPDFType1FontProgramFacade;
      const APfmFileName, APfbFileName: string);
    destructor Destroy; override;

    class function CreateType1FontFiles(AFont: TdxPDFCustomFont;
      AProgram: TdxPDFType1FontProgramFacade;
      const APfmFileName, APfbFileName: string): string; static;
  end;

  { TdxPDFSimpleFontCodePointMapping }

  TdxPDFSimpleFontCodePointMapping = class(TdxPDFCodePointMapping)
  strict private
    FGlyphIndicesMapping: TSmallIntDynArray;
    FUnicodeMapping: TSmallIntDynArray;
    procedure MapCodes(const ACodePoints: TSmallIntDynArray;
      const AMappingTable: TSmallIntDynArray);
  public
    constructor Create(const AGlyphIndicesMapping: TSmallIntDynArray;
      const AUnicodeMapping: TSmallIntDynArray); reintroduce;
    function UpdateCodePoints(const ACodePoints: TSmallIntDynArray;
      AUseEmbeddedFontEncoding: Boolean): Boolean; override;
  end;

  { TdxPDFNonEmbeddedCIDFontCodePointMapping }

  TdxPDFNonEmbeddedCIDFontCodePointMapping = class(TdxPDFCodePointMapping)
  strict private
    FCharSet: TdxPDFCIDCharset;
  public
    constructor Create(ACharset: TdxPDFCIDCharset);
    destructor Destroy; override;
    function UpdateCodePoints(const ACodePoints: TSmallIntDynArray;
      AUseEmbeddedFontEncoding: Boolean): Boolean; override;
  end;

  { TdxPDFCompositeFontCodePointMapping }

  TdxPDFCompositeFontCodePointMapping = class(TdxPDFCodePointMapping)
  strict private
    FCidToGidMap: TSmallIntDynArray;
    FMapping: TdxPDFSmallIntegerDictionary;
  public
    constructor Create(const ACidToGidMap: TSmallIntDynArray;
      AMapping: TdxPDFSmallIntegerDictionary);
    destructor Destroy; override;
    function UpdateCodePoints(const ACodePoints: TSmallIntDynArray;
      AUseEmbeddedFontEncoding: Boolean): Boolean; override;
  end;

  { TdxPDFFontProgramFacade }

  TdxPDFFontProgramFacade = class(TInterfacedObject, IdxPDFCodePointMapping)
  strict private
    FBottom: Double;
    FCharSet: TdxPDFFontCharset;
    FFontBBox: TdxPDFRectangle;
    FMapping: TdxPDFCodePointMapping;
    FTop: Double;
  public
    constructor Create(const AFontBBox: TdxPDFRectangle; ATop, ABottom: Double;
      AMapping: TdxPDFCodePointMapping); overload;
    constructor Create(const AFontBBox: TdxPDFRectangle; ATop, ABottom: Double;
      AMapping: TdxPDFCodePointMapping; ACharset: TdxPDFFontCharset); overload;
    destructor Destroy; override;
    class function GetUnicodeMapping(AEncoding: TdxPDFSimpleFontEncoding)
      : TSmallIntDynArray; static;
    function UpdateCodePoints(const ACodePoints: TSmallIntDynArray;
      AUseEmbeddedFontEncoding: Boolean): Boolean;

    property Bottom: Double read FBottom; // for internal use
    property FontBBox: TdxPDFRectangle read FFontBBox; // for internal use
    property Top: Double read FTop; // for internal use
  end;

  { TdxPDFNonEmbeddedFontProgramFacade }

  TdxPDFNonEmbeddedFontProgramFacade = class(TdxPDFFontProgramFacade)
  public
    constructor Create(AFont: TdxPDFSimpleFont); overload;
    constructor Create(AFont: TdxPDFType0Font; AMapping: TdxPDFCodePointMapping;
      ACharset: TdxPDFFontCharset); overload;
    class function CreateForSimpleFont(AFont: TdxPDFSimpleFont)
      : TdxPDFNonEmbeddedFontProgramFacade;
    class function CreateForType0Font(AFont: TdxPDFType0Font)
      : TdxPDFNonEmbeddedFontProgramFacade;
  end;

  { TdxPDFCFFFontProgramFacade }

  TdxPDFCFFFontProgramFacade = class(TdxPDFFontProgramFacade)
  strict private
    FCharSet: TdxPDFSmallIntegerDictionary;
    FFontFileData: TBytes;
    FOriginalFontData: TBytes;
    class function DoCreate(const ACompactFontFile: TBytes; AFont: TObject;
      ACreateMapping: TFunc<TdxType1FontCompactFontProgram,
      TdxPDFCodePointMapping>): TdxPDFCFFFontProgramFacade; static;
    function GetGlyphCount: Integer;
  public
    constructor Create(const AFontBBox: TdxPDFRectangle;
      AMapping: TdxPDFCodePointMapping); overload;
    destructor Destroy; override;
    class function CreateForSimpleFont(AFont: TdxPDFSimpleFont;
      const ACompactFontFile: TBytes): TdxPDFCFFFontProgramFacade; static;
    class function CreateForType0Font(AFont: TdxPDFType0Font;
      const ACompactFontFile: TBytes): TdxPDFCFFFontProgramFacade; static;

    property Charset: TdxPDFSmallIntegerDictionary read FCharSet write FCharSet;
    property FontFileData: TBytes read FFontFileData write FFontFileData;
    property GlyphCount: Integer read GetGlyphCount;
    property OriginalFontData: TBytes read FOriginalFontData
      write FOriginalFontData;
  end;

  { TdxPDFTrueTypeFontProgramFacade }

  TdxPDFTrueTypeFontProgramFacade = class(TdxPDFFontProgramFacade)
  strict private
    FFontFileData: TBytes;
    class function DoCreate(AFontObject: TObject; const AFontFileData: TBytes;
      ACreateMapping: TFunc<TdxFontFile, TdxPDFCodePointMapping>)
      : TdxPDFTrueTypeFontProgramFacade; static;
  public
    constructor Create(const AFontBBox: TdxPDFRectangle; ATop, ABottom: Double;
      AMapping: TdxPDFCodePointMapping; const AFontFileData: TBytes);
    class function CreateForSimpleFont(AFont: TdxPDFSimpleFont;
      const ATrueTypeFontFile: TBytes): TdxPDFTrueTypeFontProgramFacade; static;
    class function CreateForType0Font(AFont: TdxPDFType0Font;
      const ATrueTypeFontFile: TBytes): TdxPDFTrueTypeFontProgramFacade; static;
    property FontFileData: TBytes read FFontFileData;
  end;

  { TdxPDFType1FontProgramFacade }

  TdxPDFType1FontProgramFacade = class(TdxPDFFontProgramFacade)
  strict private
    FFontFile: TdxPDFType1FontFileData;
    class function DoCreate(AFontObject: TObject;
      AFontFileData: TdxPDFType1FontFileData;
      ACreateMapping: TFunc<TdxPDFType1FontClassicFontProgram,
      TdxPDFCodePointMapping>): TdxPDFType1FontProgramFacade; static;
  public
    constructor Create(const AFontBBox: TdxPDFRectangle;
      AMapping: TdxPDFCodePointMapping);
    class function CreateForSimpleFont(AFont: TdxPDFSimpleFont;
      AFontFileData: TdxPDFType1FontFileData)
      : TdxPDFType1FontProgramFacade; static;
    class function CreateForType0Font(AFont: TdxPDFType0Font;
      AFontFileData: TdxPDFType1FontFileData)
      : TdxPDFType1FontProgramFacade; static;
    property FontFile: TdxPDFType1FontFileData read FFontFile write FFontFile;
  end;

implementation

uses
{$IFDEF DELPHI22}
  System.Hash,
{$ENDIF}
  Math, dxCore, dxTypeHelpers, dxStringHelper, dxCoreGraphics, dxGDIPlusAPI,
  dxHash, dxPDFPostScript,
  dxPDFUtils;

type
  TdxFontFileAccess = class(TdxFontFile);
  TdxFontFileHeadTableAccess = class(TdxFontFileHeadTable);
  TdxFontFileHheaTableAccess = class(TdxFontFileHheaTable);
  TdxFontFileOS2TableAccess = class(TdxFontFileOS2Table);
  TdxFontFilePostTableAccess = class(TdxFontFilePostTable);
  TdxPDFObjectAccess = class(TdxPDFObject);
  TdxPDFPostScriptDictionaryAccess = class(TdxPDFPostScriptDictionary);

  { TdxPDFEditableFontDataFactory }

  TdxPDFEditableFontDataFactory = class
  public
    class function Create(AFont: TdxGPFont): TdxPDFEditableFontData; static;
    class function CreateFontFile(var AFont: TdxGPFont): TdxFontFile; static;
    class function ShouldEmulateBold(AFont: TdxGPFont;
      ABoldFontFile: TdxFontFile): Boolean; static;
  end;

  { TdxPDFType1FontFileCreator }

constructor TdxPDFType1FontFileCreator.Create(AFont: TdxPDFCustomFont;
  AProgram: TdxPDFType1FontProgramFacade;
  const APfmFileName, APfbFileName: string);
var
  AFontDescriptor: TdxPDFCustomFontDescriptor;
  AFontBBox: TdxRectF;
  AFlags: Integer;
begin
  inherited Create;
  FFont := AFont;
  FFontProgram := AProgram;
  AFontDescriptor := AFont.FontDescriptor;
  if AFontDescriptor = nil then
  begin
    FTop := 0;
    FHeight := 0;
    FFontWeight := 400;
    FPitchAndFamily := 48;
    FAvgWidth := 0;
    FMaxWidth := 0;
    FCapHeight := 0;
    FXHeight := 0;
    FAscent := 0;
    FDescent := 0;
  end
  else
  begin
    AFontBBox := AFontDescriptor.FontBBox;
    FTop := ConvertToShort(AFontBBox.Top);
    FHeight := ConvertToShort(Max(0, Abs(AFontBBox.Height) - 1000));
    FFontWeight := SmallInt(AFontDescriptor.FontWeight);
    AFlags := AFontDescriptor.Flags;
    if (AFlags and Integer(ffSerif)) = Integer(ffSerif) then
      FPitchAndFamily := 16
    else
      FPitchAndFamily := IfThen((AFlags and Integer(ffScript))
        = Integer(ffScript), 64, 48);
    if (AFlags and Integer(ffFixedPitch)) = Integer(ffFixedPitch) then
      FPitchAndFamily := FPitchAndFamily + 1;
    FAvgWidth := ConvertToShort(AFontDescriptor.AvgWidth);
    FMaxWidth := ConvertToShort(AFontDescriptor.MaxWidth);
    FCapHeight := ConvertToShort(AFontDescriptor.CapHeight);
    FXHeight := ConvertToShort(AFontDescriptor.XHeight);
    FAscent := ConvertToShort(AFontDescriptor.Ascent);
    FDescent := ConvertToShort(Abs(AFontDescriptor.Descent));
  end;
  FPfmStream := CreateStream(APfmFileName);
  FPfmWriter := TcxWriter.Create(FPfmStream);
  FPfbStream := CreateStream(APfbFileName);
  FPfbWriter := TcxWriter.Create(FPfbStream);
end;

destructor TdxPDFType1FontFileCreator.Destroy;
begin
  FreeAndNil(FPfbWriter);
  FreeAndNil(FPfbStream);
  FreeAndNil(FPfmWriter);
  FreeAndNil(FPfmStream);
  inherited Destroy;
end;

function TdxPDFType1FontFileCreator.CreateStream(const AFileName: string)
  : TFileStream;
begin
  Result := TFileStream.Create(AFileName, fmCreate);
end;

function TdxPDFType1FontFileCreator.ConvertToShort(AValue: Double): SmallInt;
begin
  Result := SmallInt(Ceil(AValue));
end;

procedure TdxPDFType1FontFileCreator.WritePlainText(const AData: TBytes;
  APlainTextLength, ACipherTextLength: Integer);
var
  ABinaryCipher: TdxPDFType1FontEexecBinaryCipher;
  ACipher: TdxPDFType1FontEexecCipher;
  ACipherData: TBytes;
  ACipherDataLength: Integer;
begin
  if TdxPDFType1FontEexecCipher.IsASCIICipher(AData, APlainTextLength) then
  begin
    ACipher := TdxPDFType1FontEexecCipher.CreateCipher(AData, APlainTextLength,
      ACipherTextLength);
    try
      ACipherData := ACipher.Decrypt;
    finally
      ACipher.Free;
    end;
    ABinaryCipher := TdxPDFType1FontEexecBinaryCipher.Create(ACipherData);
    try
      ACipherData := ABinaryCipher.Encrypt;
    finally
      ABinaryCipher.Free;
    end;
    ACipherDataLength := Length(ACipherData);
    WriteInt(FPfbWriter, ACipherDataLength);
    FPfbWriter.Stream.WriteBuffer(ACipherData[0], ACipherDataLength);
  end
  else
  begin
    SetLength(ACipherData, ACipherTextLength);
    TdxPDFUtils.CopyData(AData, APlainTextLength, ACipherData, 0,
      ACipherTextLength);
    WriteInt(FPfbWriter, ACipherTextLength);
    FPfbWriter.Stream.WriteBuffer(ACipherData[0], ACipherTextLength);
  end;
end;

procedure TdxPDFType1FontFileCreator.WriteNullSegment(const AData: TBytes;
  ANullSegmentLength, APlainTextLength, ACipherTextLength: Integer);
var
  ACipherData: TBytes;
begin
  if ANullSegmentLength = 0 then
  begin
    WriteInt(FPfbWriter, Length(DefaultNullSegment));
    FPfbWriter.Stream.Write(DefaultNullSegment[0], Length(DefaultNullSegment));
  end
  else
  begin
    SetLength(ACipherData, ANullSegmentLength);
    TdxPDFUtils.CopyData(AData, APlainTextLength + ACipherTextLength,
      ACipherData, 0, ANullSegmentLength);
    WriteInt(FPfbWriter, ANullSegmentLength);
    FPfbWriter.Stream.WriteBuffer(ACipherData[0], ANullSegmentLength);
  end;
end;

class function TdxPDFType1FontFileCreator.CreateType1FontFiles
  (AFont: TdxPDFCustomFont; AProgram: TdxPDFType1FontProgramFacade;
  const APfmFileName, APfbFileName: string): string;
var
  ACreator: TdxPDFType1FontFileCreator;
begin
  ACreator := TdxPDFType1FontFileCreator.Create(AFont, AProgram, APfmFileName,
    APfbFileName);
  try
    Result := ACreator.CreateFiles;
  finally
    ACreator.Free;
  end;
end;

procedure TdxPDFType1FontFileCreator.WriteByte(AValue: Byte);
begin
  FPfmWriter.WriteByte(AValue);
end;

procedure TdxPDFType1FontFileCreator.WriteShort(AValue: SmallInt);
begin
  FPfmWriter.WriteByte(AValue and $FF);
  FPfmWriter.WriteByte((AValue and $FF00) shr 8);
end;

procedure TdxPDFType1FontFileCreator.WriteInt(AWriter: TcxWriter;
  AValue: Integer);
begin
  AWriter.WriteByte(AValue and $FF);
  AWriter.WriteByte((AValue and $FF00) shr 8);
  AWriter.WriteByte((AValue and $FF0000) shr 16);
  AWriter.WriteByte((AValue and $FF000000) shr 24);
end;

procedure TdxPDFType1FontFileCreator.WriteString(const AValue: string);
var
  C: Char;
begin
  for C in AValue do
    FPfmWriter.WriteByte(Byte(C));
  FPfmWriter.WriteByte(Byte(0));
end;

function TdxPDFType1FontFileCreator.CreateFiles: string;
var
  AData: TBytes;
  AFontName: string;
  AFontFileData: TdxPDFType1FontFileData;
  I, APlainTextLength, ACipherTextLength: Integer;
  AOffsetFullyQualifiedName, AOffsetExtentTable: Int64;
begin
  AFontName := FFont.UniqueName;
  AFontFileData := FFontProgram.FontFile;
  AData := AFontFileData.Data;
  APlainTextLength := AFontFileData.PlainTextLength;
  ACipherTextLength := AFontFileData.CipherTextLength;
  FPfbWriter.WriteByte(Byte($80));
  FPfbWriter.WriteByte(Byte($01));
  WriteInt(FPfbWriter, APlainTextLength);
  FPfbWriter.Stream.WriteBuffer(AData[0], APlainTextLength);
  FPfbWriter.WriteByte(Byte($80));
  FPfbWriter.WriteByte(Byte($02));
  WritePlainText(AData, APlainTextLength, ACipherTextLength);
  FPfbWriter.WriteByte(Byte($80));
  FPfbWriter.WriteByte(Byte($01));
  WriteNullSegment(AData, AFontFileData.NullSegmentLength, APlainTextLength,
    ACipherTextLength);
  FPfbWriter.WriteByte(Byte($80));
  FPfbWriter.WriteByte(Byte($03));
  WriteShort(256);
  WriteInt(FPfmWriter, 0);
  for I := 0 to 60 - 1 do
    WriteByte(0);
  WriteShort(129);
  WriteShort(10);
  WriteShort(300);
  WriteShort(300);
  WriteShort(FTop);
  WriteShort(FHeight);
  WriteShort(196);
  WriteByte(0);
  WriteByte(0);
  WriteByte(0);
  WriteShort(FFontWeight);
  WriteByte(0);
  WriteShort(0);
  WriteShort(1000);
  WriteByte(FPitchAndFamily);
  WriteShort(FAvgWidth);
  WriteShort(FMaxWidth);
  WriteByte(32);
  WriteByte(32);
  WriteByte(0);
  WriteByte(0);
  WriteShort(0);
  WriteInt(FPfmWriter, 199);
  WriteInt(FPfmWriter, 210);
  WriteInt(FPfmWriter, 0);
  WriteInt(FPfmWriter, 0);
  WriteShort(30);
  WriteInt(FPfmWriter, 147);
  WriteInt(FPfmWriter, 0);
  WriteInt(FPfmWriter, 0);
  WriteInt(FPfmWriter, 0);
  WriteInt(FPfmWriter, 0);
  WriteInt(FPfmWriter, 0);
  WriteInt(FPfmWriter, 0);
  WriteShort(52);
  WriteShort(240);
  WriteShort(0);
  WriteShort(1000);
  WriteShort(3);
  WriteShort(1000);
  WriteShort(1000);
  WriteShort(FCapHeight);
  WriteShort(FXHeight);
  WriteShort(FAscent);
  WriteShort(FDescent);
  WriteShort(0);
  WriteShort(-500);
  WriteShort(250);
  WriteShort(500);
  WriteShort(500);
  WriteShort(0);
  WriteShort(0);
  WriteShort(0);
  WriteShort(0);
  WriteShort(0);
  WriteShort(0);
  WriteShort(405);
  WriteShort(50);
  WriteShort(0);
  WriteShort(0);
  WriteString('PostScript');
  WriteString(AFontName);
  AOffsetFullyQualifiedName := FPfmWriter.Stream.Position;
  WriteString(AFontName);
  AOffsetExtentTable := FPfmStream.Position;
  WriteShort(0);
  FPfmWriter.Stream.Position := 2;
  WriteInt(FPfmWriter, FPfmStream.Size);
  FPfmWriter.Stream.Position := 123;
  WriteInt(FPfmWriter, Integer(AOffsetExtentTable));
  FPfmWriter.Stream.Position := 139;
  WriteInt(FPfmWriter, Integer(AOffsetFullyQualifiedName));
  Result := AFontName;
end;

{ TdxPDFEditableFontDataFactory }

class function TdxPDFEditableFontDataFactory.CreateFontFile
  (var AFont: TdxGPFont): TdxFontFile;

  function dxPDFGetFontData(AStream: TMemoryStream; AFont: TFont): Boolean;
  var
    AFontDataLength: Cardinal;
    AOldFont: HFONT;
    DC: HDC;
  begin
    DC := GetDC(0);
    AOldFont := SelectObject(DC, AFont.Handle);
    AFontDataLength := GetFontData(DC, 0, 0, nil, 0);
    Result := AFontDataLength <> GDI_ERROR;
    if Result and Assigned(AStream) then
    begin
      AStream.Size := AFontDataLength;
      GetFontData(DC, 0, 0, AStream.Memory, AStream.Size);
    end;
    SelectObject(DC, AOldFont);
    ReleaseDC(0, DC);
  end;

var
  ABytesStream: TBytesStream;
  AGDIFont: TFont;
begin
  if AFont.FontFamily.Name = 'Symbol' then
  begin
    if AFont <> nil then
      FreeAndNil(AFont);
    AFont := TdxGPFont.Create('Symbol', 9, TdxGPFontStyle(AFont.Style));
  end;
  ABytesStream := TBytesStream.Create;
  try
    AGDIFont := TFont.Create;
    try
      AGDIFont.Handle := AFont.ToHfont;
      dxPDFGetFontData(ABytesStream, AGDIFont);
      Result := TdxFontFile.Create(ABytesStream.Bytes, False);
    finally
      AGDIFont.Free;
    end;
  finally
    ABytesStream.Free;
  end;
end;

class function TdxPDFEditableFontDataFactory.Create(AFont: TdxGPFont)
  : TdxPDFEditableFontData;
var
  AFontFile: TdxFontFile;
  AFontFacade: TdxPDFFontFileDataFacade;
begin
  AFontFile := CreateFontFile(AFont);

  AFontFacade := TdxPDFFontFileDataFacade.Create(AFont.FontFamily.Name,
    TdxGPFontStyle(AFont.Style), AFontFile, ShouldEmulateBold(AFont,
    AFontFile));

  Result := TdxPDFEditableFontData.Create
    (TdxPDFEmbeddedEditableFontObject.Create(AFontFacade,
    not AFontFile.IsTrueTypeFont), AFontFacade,
    TdxPDFEmbeddedGlyphMapper.Create(AFontFile));
end;

class function TdxPDFEditableFontDataFactory.ShouldEmulateBold(AFont: TdxGPFont;
  ABoldFontFile: TdxFontFile): Boolean;
var
  AActualFontFile: TdxFontFile;
  ARegularFont: TdxGPFont;
begin
  Result := AFont.Bold and AFont.FontFamily.IsStyleAvailable
    (Integer(TdxGPFontStyle.FontStyleRegular));
  if Result then
  begin
    ARegularFont := TdxGPFont.Create(AFont, TdxGPFontStyle.FontStyleRegular);
    try
      AActualFontFile := CreateFontFile(ARegularFont);
      try
        Result := TdxFontFile.IsEqual(AActualFontFile, ABoldFontFile);
      finally
        AActualFontFile.Free;
      end;
    finally
      ARegularFont.Free;
    end;
  end;
end;

{ TdxPDFFontCustomRegistrator }

constructor TdxPDFFontCustomRegistrator.Create(AFontSubstitutionHelper
  : TdxPDFGDIFontSubstitutionHelper; AFont: TdxPDFCustomFont);
begin
  inherited Create;
  FFontSubstitutionHelper := AFontSubstitutionHelper;
  FFont := AFont;
end;

destructor TdxPDFFontCustomRegistrator.Destroy;
begin
  Unregister;
  inherited Destroy;
end;

function TdxPDFFontCustomRegistrator.CreateSubstituteFontData
  : TdxPDFFontRegistrationData;
begin
  Result := TdxPDFFontRegistrationData.Create(FFont.Name, FFont.Weight,
    FFont.Italic, FFont.PitchAndFamily, False, nil, False);
end;

class function TdxPDFFontCustomRegistrator.CreateRegistrator
  (AFontSubstitutionHelper: TdxPDFGDIFontSubstitutionHelper;
  AFont: TdxPDFCustomFont; const AFolderName: string)
  : TdxPDFFontCustomRegistrator;
var
  AProgram: TdxPDFFontProgramFacade;
  ATrueTypeProgram: TdxPDFTrueTypeFontProgramFacade;
  AType1Program: TdxPDFType1FontProgramFacade;
  ACffProgram: TdxPDFCFFFontProgramFacade;
begin
  AProgram := AFont.FontProgramFacade as TdxPDFFontProgramFacade;
  ATrueTypeProgram := Safe<TdxPDFTrueTypeFontProgramFacade>.Cast(AProgram);
  if ATrueTypeProgram <> nil then
    Exit(TdxPDFTrueTypeFontRegistrator.Create(AFontSubstitutionHelper, AFont,
      ATrueTypeProgram));
  AType1Program := Safe<TdxPDFType1FontProgramFacade>.Cast(AProgram);
  if AType1Program <> nil then
  begin
    if not DirectoryExists(AFolderName) then
      if not ForceDirectories(AFolderName) then
        TdxPDFUtils.RaiseException('Cannot create temp directory');
    Exit(TdxPDFType1FontRegistrator.Create(AFontSubstitutionHelper, AFont,
      AFolderName, AType1Program));
  end;
  ACffProgram := Safe<TdxPDFCFFFontProgramFacade>.Cast(AProgram);
  if ACffProgram <> nil then
    Exit(TdxPDFOpenTypeFontRegistrator.Create(AFontSubstitutionHelper, AFont));
  if (AFont is TdxPDFTrueTypeFont) or (AFont is TdxPDFCIDType2Font) then
    Result := TdxPDFTrueTypeFontRegistrator.Create(AFontSubstitutionHelper,
      AFont, nil)
  else
    Result := TdxPDFOpenTypeFontRegistrator.Create
      (AFontSubstitutionHelper, AFont);
end;

function TdxPDFFontCustomRegistrator.Register: TdxPDFFontRegistrationData;
var
  AFontName: string;
begin
  AFontName := PerformRegister;
  if AFontName = '' then
    Result := CreateSubstituteFontData
  else
    Result := TdxPDFFontRegistrationData.Create(AFontName, FFont.Weight, False,
      DEFAULT_PITCH or FF_DONTCARE, True, Self, False);
end;

{ TdxPDFFontInMemoryRegistrator }

constructor TdxPDFFontInMemoryRegistrator.Create(AFontSubstitutionHelper
  : TdxPDFGDIFontSubstitutionHelper; AFont: TdxPDFCustomFont;
  const AFontFileData: TBytes);
begin
  inherited Create(AFontSubstitutionHelper, AFont);
  FFontFileData := AFontFileData;
end;

function TdxPDFFontInMemoryRegistrator.PerformRegister: string;
begin
  if CreateHandle then
    Result := Font.UniqueName
  else
    Result := '';
end;

procedure TdxPDFFontInMemoryRegistrator.Unregister;
begin
  if HandleAllocated then
    if not RemoveFontMemResourceEx(FFontHandle) then
      TdxPDFUtils.RaiseTestException('TdxPDFFontInMemoryRegistrator.Unregister')
    else
      FFontHandle := 0;
end;

function TdxPDFFontInMemoryRegistrator.CreateHandle: Boolean;
var
  ACount: DWORD;
begin
  Result := Length(FFontFileData) > 0;
  if Result then
  begin
    FFontHandle := AddFontMemResourceEx(FFontFileData, Length(FFontFileData),
      nil, @ACount);
    Result := HandleAllocated;
  end;
end;

function TdxPDFFontInMemoryRegistrator.HandleAllocated: Boolean;
begin
  Result := FFontHandle <> 0;
end;

{ TdxPDFOpenTypeFontCreator }

class function TdxPDFOpenTypeFontCreator.GetFontFileData
  (AFont: TdxPDFCustomFont): TBytes;
var
  AFile: TdxFontFile;
  AFontProgramFacade: TdxPDFCFFFontProgramFacade;
begin
  AFontProgramFacade := Safe<TdxPDFCFFFontProgramFacade>.Cast
    (AFont.FontProgramFacade);
  if AFontProgramFacade <> nil then
  begin
    AFile := TdxFontFileHelper.CreateOpenTypeFile(AFontProgramFacade, AFont);
    try
      Result := AFile.GetData;
    finally
      AFile.Free;
    end;
  end
  else
    Result := nil;
end;

{ TdxPDFTrueTypeFontRegistrator }

constructor TdxPDFTrueTypeFontRegistrator.Create(AFontSubstitutionHelper
  : TdxPDFGDIFontSubstitutionHelper; AFont: TdxPDFCustomFont;
  AProgram: TdxPDFTrueTypeFontProgramFacade);
var
  AFontFileData: TBytes;
begin
  if AProgram = nil then
    AFontFileData := nil
  else
    AFontFileData := AProgram.FontFileData;
  inherited Create(AFontSubstitutionHelper, AFont, AFontFileData);
end;

function TdxPDFTrueTypeFontRegistrator.CreateSubstituteFontData
  : TdxPDFFontRegistrationData;
var
  AName: string;
  AFontParameters: TdxPDFFontRegistratorParameters;
  APitchAndFamily: Byte;
begin
  AFontParameters := FontSubstitutionHelper.GetSubstituteFontParameters(Font,
    function(AFamilyName: string): Boolean
    begin
      Result := AFamilyName <> 'symbol';
    end);
  AName := AFontParameters.Name;
  APitchAndFamily := Font.PitchAndFamily;
  if (AName = 'LucidaConsole') or (AName = 'Lucida Console') then
  begin
    APitchAndFamily := APitchAndFamily and not VARIABLE_PITCH;
    APitchAndFamily := APitchAndFamily or FIXED_PITCH;
  end;
  Result := TdxPDFFontRegistrationData.Create(AName, AFontParameters.Weight,
    AFontParameters.IsItalic, APitchAndFamily, False, nil, False);
end;

{ TdxPDFOpenTypeFontRegistrator }

constructor TdxPDFOpenTypeFontRegistrator.Create(AFontSubstitutionHelper
  : TdxPDFGDIFontSubstitutionHelper; AFont: TdxPDFCustomFont);
begin
  inherited Create(AFontSubstitutionHelper, AFont,
    TdxPDFOpenTypeFontCreator.GetFontFileData(AFont));
end;

function TdxPDFOpenTypeFontRegistrator.CreateSubstituteFontData
  : TdxPDFFontRegistrationData;
var
  AParameters: TdxPDFFontRegistratorParameters;
begin
  AParameters := FontSubstitutionHelper.GetSubstituteFontParameters(Font);
  Result := TdxPDFFontRegistrationData.Create(AParameters.Name,
    AParameters.Weight, AParameters.IsItalic, Font.PitchAndFamily, False,
    nil, False);
end;

{ TdxPDFType1FontRegistrator }

constructor TdxPDFType1FontRegistrator.Create(AFontSubstitutionHelper
  : TdxPDFGDIFontSubstitutionHelper; AFont: TdxPDFCustomFont;
const AFontFolderName: string; AProgram: TdxPDFType1FontProgramFacade);
var
  AFileName: string;
begin
  inherited Create(AFontSubstitutionHelper, AFont);
  AFileName := AFontFolderName + dxGenerateID;
  FPfmFileName := AFileName + '.pfm';
  FPfbFileName := AFileName + '.pfb';
  FFontResourceName := FPfmFileName + '|' + FPfbFileName;
  FFontName := TdxPDFType1FontFileCreator.CreateType1FontFiles(AFont, AProgram,
    FPfmFileName, FPfbFileName);
end;

destructor TdxPDFType1FontRegistrator.Destroy;
begin
  Unregister;
  DeleteFontFile(FPfmFileName);
  DeleteFontFile(FPfbFileName);
  inherited Destroy;
end;

class procedure TdxPDFType1FontRegistrator.DeleteFontFile
  (const AFileName: string);
begin
  try
    DeleteFile(PWideChar(AFileName));
  except
  end;
end;

procedure TdxPDFType1FontRegistrator.Unregister;
begin
  RemoveFontResourceEx(PWideChar(FFontResourceName), FR_PRIVATE, nil);
end;

function TdxPDFType1FontRegistrator.PerformRegister: string;
begin
  if AddFontResourceEx(PWideChar(FFontResourceName), FR_PRIVATE, nil) = 0 then
    Result := ''
  else
    Result := FFontName;
end;

{ TdxFontFileHelper }

class function TdxFontFileHelper.CreateOpenTypeFile(ACompactFontProgram
  : TdxPDFCFFFontProgramFacade; AFont: TdxPDFCustomFont): TdxFontFile;
var
  AFontData: TBytes;
  ACharset: TdxPDFSmallIntegerDictionary;
  ACMapTable: TdxFontFileCMapTable;
  AHmtxTable: TdxFontFileHmtxTable;
  AMaxpTable: TdxFontFileMaxpTable;
  AFontDescriptor: TdxPDFCustomFontDescriptor;
  AFontProgramBBox, AValidatedBBox: TdxPDFRectangle;
  APoints: TdxPointsF;
  AMaxWidth: Double;
  ARoundedBottom, ARoundedTop: SmallInt;
begin
  AFontProgramBBox := ACompactFontProgram.FontBBox;
  SetLength(APoints, 0);
  if not AFontProgramBBox.IsNull then
  begin
    TdxPDFUtils.AddPoint(AFontProgramBBox.TopLeft, APoints);
    TdxPDFUtils.AddPoint(AFontProgramBBox.TopRight, APoints);
    TdxPDFUtils.AddPoint(AFontProgramBBox.BottomLeft, APoints);
    TdxPDFUtils.AddPoint(AFontProgramBBox.BottomRight, APoints);
  end;
  AFontDescriptor := AFont.FontDescriptor;
  if AFontDescriptor <> nil then
  begin
    TdxPDFUtils.AddPoint(AFontDescriptor.FontBBox.TopLeft, APoints);
    TdxPDFUtils.AddPoint(AFontDescriptor.FontBBox.TopRight, APoints);
    TdxPDFUtils.AddPoint(AFontDescriptor.FontBBox.BottomLeft, APoints);
    TdxPDFUtils.AddPoint(AFontDescriptor.FontBBox.BottomRight, APoints);
    TdxPDFUtils.AddPoint(dxPointF(AFontDescriptor.FontBBox.Left,
      AFontDescriptor.Ascent), APoints);
    TdxPDFUtils.AddPoint(dxPointF(AFontDescriptor.FontBBox.Right,
      AFontDescriptor.Descent), APoints);
  end;
  AValidatedBBox := TdxPDFRectangle.CreateBoundingRectangle(APoints);
  if not AValidatedBBox.IsNull then
  begin
    if AValidatedBBox.Right <= 0 then
      AValidatedBBox := TdxPDFRectangle.Create(0, AValidatedBBox.Bottom, 0,
        AValidatedBBox.Top);
    if AValidatedBBox.Width = 0 then
    begin
      AMaxWidth := AFont.MaxGlyphWidth;
      AValidatedBBox := TdxPDFRectangle.Create(AValidatedBBox.Left,
        AValidatedBBox.Bottom, AValidatedBBox.Left + AMaxWidth,
        AValidatedBBox.Top);
    end;
  end;
  if AValidatedBBox.IsNull or AValidatedBBox.Equal(AValidatedBBox,
    TdxPDFRectangle.Create(0, 0, 0, 0), 0.001) or (AValidatedBBox.Width = 0) or
    (AValidatedBBox.Height = 0) then
    AValidatedBBox := TdxPDFRectangle.Create(-MINSHORT, -MINSHORT, MAXSHORT,
      MAXSHORT);
  ARoundedBottom := Max(-MINSHORT, Floor(AValidatedBBox.Bottom));
  ARoundedTop := Min(MAXSHORT, Floor(AValidatedBBox.Top));
  SetLength(AFontData, 0);
  Result := TdxFontFile.Create(AFontData, True);
  ACharset := ACompactFontProgram.Charset;
  ACMapTable := TdxFontFileCMapTable.CreateFromCharset(ACharset);
  AHmtxTable := TdxFontFileHmtxTable.Create(ACompactFontProgram.GlyphCount);
  AMaxpTable := TdxFontFileMaxpTable.Create(ACompactFontProgram.GlyphCount);
  Result.AddTable(TdxFontFileCFFTable.Create(ACompactFontProgram.FontFileData));
  Result.AddTable(CreateOS2Table(AFont, ACharset, ARoundedTop, ARoundedBottom));
  Result.AddTable(ACMapTable);
  Result.AddTable(CreateHeadTable(AValidatedBBox));
  Result.AddTable(CreateHheaTable(AFont, ACompactFontProgram.GlyphCount,
    ARoundedTop, ARoundedBottom));
  Result.AddTable(AHmtxTable);
  Result.AddTable(AMaxpTable);
  Result.AddTable(TdxFontFileNameTable.Create(ACMapTable, AFont.UniqueName));
  Result.AddTable(CreatePostTable(AFont));
end;

class function TdxFontFileHelper.GetCFFData(const AData: TBytes): TBytes;
begin
  Result := TdxFontFile.GetCFFData(AData);
end;

class function TdxFontFileHelper.GetFontFileInfo(AFont: TdxPDFCustomFont;
const AFontData: TBytes): TdxFontFileInfo;
var
  ACMapRecord: TdxFontFileCMapSegmentMappingRecord;
  AFile: TdxFontFile;
begin
  AFile := TdxFontFile.Create(AFontData);
  Result.Data := AFile.GetData;
  try
    ACMapRecord := PatchCMapTable(AFile, AFont, Result.Encoding);
    PatchNameTable(AFile, AFont.UniqueName);
    if AFile.HheaTable <> nil then
      AFile.HheaTable.Validate;
    if (AFont.FontDescriptor <> nil) and (AFile.OS2Table <> nil) and
      (AFile.HeadTable <> nil) then
      ValidateOS2Table(AFile, AFont.FontDescriptor);

    if (AFile.HeadTable <> nil) and (AFile.GlyphTable <> nil) then
      AFile.HeadTable.Validate(AFile.GlyphTable.Glyphs);

    Result.GlyphMapping := nil;
    Result.GlyphRanges := TList<TdxFontFileCMapGlyphRange>.Create;
    if ACMapRecord <> nil then
    begin
      Result.GlyphRanges.AddRange(ACMapRecord.GlyphRanges);
      if (AFile.PostTable <> nil) and AFile.CMapTable.NeedWrite then
      begin
        Result.GlyphMapping := ACMapRecord.CreateGlyphMapping
          (AFile.PostTable.GlyphNames);
        Result.GlyphMapping.TrimExcess;
      end;
    end;
    Result.Data := AFile.GetData;
  finally
    AFile.Free;
  end;
end;

class function TdxFontFileHelper.GetSimpleMapping(AFile: TdxFontFile;
AEncoding: TdxPDFSimpleFontEncoding; AIsSymbolic: Boolean)
  : TdxPDFCodePointMapping;
var
  ACMapEntry: TdxFontFileCMapTable;
  APostEntry: TdxFontFilePostTable;
  AMicrosoftUnicodeEntry, AMicrosoftUndefinedEntry, AMacRomanEntry,
    AFallbackEntry, AEntry: TdxFontFileCMapCustomFormatRecord;
  AFontFileGlyphNames: TStringList;
  AUnicode, AGlyphIndices: TSmallIntDynArray;
  ACMapNotFound, ANameFound: Boolean;
  I, AIndex: SmallInt;
  AGlyphIndex, AUnicodeSymbol: Word;
  AName: string;
  AEntries: TList<TdxFontFileCMapCustomFormatRecord>;
  AMappedBack: Word;
  AMacReversedEncoding: TdxFontFileMacRomanReversedEncoding;
begin
  ACMapEntry := AFile.CMapTable;
  APostEntry := AFile.PostTable;
  AMicrosoftUnicodeEntry := nil;
  AMicrosoftUndefinedEntry := nil;
  AMacRomanEntry := nil;
  AFallbackEntry := nil;
  if APostEntry <> nil then
    AFontFileGlyphNames := APostEntry.GlyphNames
  else
    AFontFileGlyphNames := nil;
  if (ACMapEntry <> nil) and (ACMapEntry.CMapTables <> nil) then
  begin
    for AEntry in ACMapEntry.CMapTables do
    begin
      if AFallbackEntry = nil then
        AFallbackEntry := AEntry;
      if AEntry.PlatformId = TdxFontFilePlatformID.Microsoft then
      begin
        if AEntry.EncodingId = TdxFontFileEncodingID.Undefined then
          AMicrosoftUndefinedEntry := AEntry
        else if AEntry.EncodingId = TdxFontFileEncodingID.UGL then
          AMicrosoftUnicodeEntry := AEntry;
      end
      else if (AEntry.PlatformId = TdxFontFilePlatformID.Macintosh) and
        (AEntry.EncodingId = TdxFontFileEncodingID.Undefined) then
        AMacRomanEntry := AEntry;
    end;
  end;
  SetLength(AUnicode, 256);
  SetLength(AGlyphIndices, 256);
  ACMapNotFound := False;

  AMacReversedEncoding := TdxFontFileMacRomanReversedEncoding.Create;
  try
    for I := 0 to 255 do
    begin
      AGlyphIndex := TdxFontFileCMapCustomFormatRecord.NotdefGlyphIndex;
      AName := AEncoding.GetGlyphName(Byte(I));
      if AIsSymbolic then
      begin
        AEntries := TList<TdxFontFileCMapCustomFormatRecord>.Create;
        try
          if AMicrosoftUndefinedEntry <> nil then
            AEntries.Add(AMicrosoftUndefinedEntry);
          if AMacRomanEntry <> nil then
            AEntries.Add(AMacRomanEntry);
          if AFallbackEntry <> nil then
            AEntries.Add(AFallbackEntry);
          if AEntries.Count <> 0 then
            for AEntry in AEntries do
            begin
              AGlyphIndex := AEntry.MapCode(Char(I));
              if AGlyphIndex <> TdxFontFileCMapCustomFormatRecord.NotdefGlyphIndex
              then
                Break;
            end
          else
            ACMapNotFound := True;
        finally
          AEntries.Free;
        end;
        AUnicode[I] := I;
      end
      else
      begin
        AUnicodeSymbol := 0;
        ANameFound := dxFontFileUnicodeConverter.FindCode(AName,
          AUnicodeSymbol);
        if not ANameFound and ((AFontFileGlyphNames = nil) or
          (AFontFileGlyphNames.Count = 0)) then
        begin
          AUnicodeSymbol := I;
          ANameFound := True;
        end;
        if ANameFound then
        begin
          AUnicode[I] := AUnicodeSymbol;
          if AMicrosoftUnicodeEntry <> nil then
            AGlyphIndex := AMicrosoftUnicodeEntry.MapCode(Char(AUnicodeSymbol))
          else if AMacRomanEntry <> nil then
          begin
            if AMacReversedEncoding.ReversedDictionary.TryGetValue(AName,
              AMappedBack) then
              AGlyphIndex := AMacRomanEntry.MapCode(Char(AMappedBack))
            else
              AGlyphIndex := AMacRomanEntry.MapCode(Char(AUnicodeSymbol));
          end
          else if AFallbackEntry <> nil then
            AGlyphIndex := AFallbackEntry.MapCode(Char(I))
          else
            ACMapNotFound := True;
        end;
      end;
      if (AGlyphIndex = TdxFontFileCMapCustomFormatRecord.NotdefGlyphIndex) and
        (AIsSymbolic or (AName <> TdxGlyphNames._notdef)) then
      begin
        AIndex := -1;
        if AFontFileGlyphNames <> nil then
          AIndex := AFontFileGlyphNames.IndexOf(AName);
        AGlyphIndex := IfThen(AIndex = -1, IfThen(ACMapNotFound, I), AIndex);
      end;
      AGlyphIndices[I] := AGlyphIndex;
    end;
  finally
    AMacReversedEncoding.Free;
  end;
  Result := TdxPDFSimpleFontCodePointMapping.Create(AGlyphIndices, AUnicode);
end;

class function TdxFontFileHelper.GetValidCFFData(const AData: TBytes): TBytes;
begin
  Result := AData;
  if (Length(AData) <> 0) and (AData[0] <> 1) then
  begin
    Result := GetCFFData(AData);
    if Result = nil then
      Result := AData;
  end;
end;

class function TdxFontFileHelper.IsOpenType(const AFontFileData
  : TBytes): Boolean;
begin
  Result := CheckVersion(AFontFileData, GetOpenTypeVersion);
end;

class function TdxFontFileHelper.IsTrueType(const AFontFileData
  : TBytes): Boolean;
begin
  Result := CheckVersion(AFontFileData, GetTTFVersion);
end;

class function TdxFontFileHelper.CheckVersion(const AFontFileData,
  AVersionData: TBytes): Boolean;
begin
  Result := (Length(AFontFileData) >= Length(AVersionData)) and
    (AFontFileData[0] = AVersionData[0]) and (AFontFileData[1] = AVersionData[1]
    ) and (AFontFileData[2] = AVersionData[2]) and
    (AFontFileData[3] = AVersionData[3]);
end;

class function TdxFontFileHelper.CreateHeadTable(const AFontBBox
  : TdxPDFRectangle): TdxFontFileHeadTable;
var
  AData: TBytes;
begin
  SetLength(AData, 0);
  Result := TdxFontFileHeadTable.Create(AData);
  TdxFontFileHeadTableAccess(Result).FVersion := $00010000;
  TdxFontFileHeadTableAccess(Result).FFontRevision := 0;
  TdxFontFileHeadTableAccess(Result).FUnitsPerEm := 1000;
  TdxFontFileHeadTableAccess(Result).FCheckSumAdjustment := 0;
  TdxFontFileHeadTableAccess(Result).FMagicNumber := $5F0F3CF5;
  TdxFontFileHeadTableAccess(Result).FFlags := TdxFontFileHeadTableFlags(0);
  TdxFontFileHeadTableAccess(Result).FCreated := 19;
  TdxFontFileHeadTableAccess(Result).FModified :=
    TdxFontFileHeadTableAccess(Result).FCreated;
  if not AFontBBox.IsNull then
  begin
    TdxFontFileHeadTableAccess(Result).FXMin := Trunc(AFontBBox.Left);
    TdxFontFileHeadTableAccess(Result).FYMin := Trunc(AFontBBox.Bottom);
    TdxFontFileHeadTableAccess(Result).FXMax := Trunc(AFontBBox.Right);
    TdxFontFileHeadTableAccess(Result).FYMax := Trunc(AFontBBox.Top);
  end;
  TdxFontFileHeadTableAccess(Result).FLowestRecPPEM := 6;
  TdxFontFileHeadTableAccess(Result).Changed;
end;

class function TdxFontFileHelper.CreateHheaTable(AFont: TdxPDFCustomFont;
AGlyphCount, AAscender, ADescender: Integer): TdxFontFileHheaTable;
var
  AItalicAngle, AMinWidth, AMaxWidth, AWidth: Double;
  AFontDescriptor: TdxPDFCustomFontDescriptor;
  AEm: SmallInt;
  AData: TBytes;
  AWidths: TDoubleDynArray;
begin
  SetLength(AData, 0);
  Result := TdxFontFileHheaTable.Create(AData);
  TdxFontFileHheaTableAccess(Result).FVersion := $10000;
  TdxFontFileHheaTableAccess(Result).FAscender := AAscender;
  TdxFontFileHheaTableAccess(Result).FDescender := ADescender;
  AFontDescriptor := AFont.FontDescriptor;
  if AFontDescriptor = nil then
    AItalicAngle := 0
  else
    AItalicAngle := Abs(AFontDescriptor.ItalicAngle);
  AEm := Trunc(TdxFontFileHheaTableAccess(Result).FAscender -
    TdxFontFileHheaTableAccess(Result).FDescender);
  TdxFontFileHheaTableAccess(Result).FLineGap := Trunc(1.2 * AEm);
  AMinWidth := MaxInt;
  AMaxWidth := 0;
  AWidths := AFont.Widths;
  for AWidth in AWidths do
    if AWidth <> 0 then
    begin
      AMinWidth := Min(AWidth, AMinWidth);
      AMaxWidth := Max(AWidth, AMaxWidth);
    end;
  TdxFontFileHheaTableAccess(Result).FAdvanceWidthMax := Trunc(AMaxWidth);

  TdxFontFileHheaTableAccess(Result).FMinLeftSideBearing := 0;
  TdxFontFileHheaTableAccess(Result).FMinRightSideBearing := 0;
  TdxFontFileHheaTableAccess(Result).FXMaxExtent := Trunc(AMinWidth);
  if AItalicAngle = 0 then
    TdxFontFileHheaTableAccess(Result).FCaretSlopeRise := 1
  else
    TdxFontFileHheaTableAccess(Result).FCaretSlopeRise :=
      Trunc(AEm * Sin((90 - AItalicAngle) * PI / 180));
  TdxFontFileHheaTableAccess(Result).FCaretSlopeRun :=
    Trunc(AEm * Sin(AItalicAngle * PI / 180));
  TdxFontFileHheaTableAccess(Result).FMetricDataFormat := 0;

  TdxFontFileHheaTableAccess(Result).FNumberOfHMetrics := AGlyphCount;
  TdxFontFileHheaTableAccess(Result).Changed;
end;

class function TdxFontFileHelper.CreateOS2Table(AFont: TdxPDFCustomFont;
ACharset: TdxPDFSmallIntegerDictionary; AValidatedAscender, AValidatedDescender
  : Integer): TdxFontFileOS2Table;
const
  EmptyValue = $00000000;
var
  AFlags: Integer;
  AIsBold: Boolean;
  AAscender, ADescender, AItalicAngle, AEm: Double;
  AFontDescriptor: TdxPDFCustomFontDescriptor;
  AFirstChar, ALastChar, AWidth: Integer;
  AData: TBytes;
begin
  SetLength(AData, 0);
  Result := TdxFontFileOS2Table.Create(AData);

  TdxFontFileOS2TableAccess(Result).FVersion := TdxFontFileVersion.OpenType_1_5;
  TdxFontFileOS2TableAccess(Result).FAvgCharWidth := AFont.AvgGlyphWidth;
  AFontDescriptor := AFont.FontDescriptor;
  if AFontDescriptor = nil then
  begin
    AFlags := Integer(ffNone);
    AIsBold := False;
    TdxFontFileOS2TableAccess(Result).FWeightClass :=
      TdxFontFileOS2Table.NormalFontWeight;
    TdxFontFileOS2TableAccess(Result).FWidthClass := wcMedium;
    AAscender := 0;
    ADescender := 0;
    AItalicAngle := 0;
    TdxFontFileOS2TableAccess(Result).FXHeight := 0;
    TdxFontFileOS2TableAccess(Result).FCapHeight := 0;
  end
  else
  begin
    AFlags := AFontDescriptor.Flags;
    AIsBold := (AFlags and Integer(ffForceBold)) = Integer(ffForceBold);
    if AIsBold then
      TdxFontFileOS2TableAccess(Result).FWeightClass :=
        TdxFontFileOS2Table.BoldFontWeight
    else
    begin
      TdxFontFileOS2TableAccess(Result).FWeightClass :=
        SmallInt(AFontDescriptor.FontWeight);
      AIsBold := TdxFontFileOS2TableAccess(Result)
        .FWeightClass = TdxFontFileOS2Table.BoldFontWeight;
    end;
    case AFontDescriptor.FontStretch of
      fsCondensed:
        TdxFontFileOS2TableAccess(Result).FWidthClass := wcCondensed;
      fsExpanded:
        TdxFontFileOS2TableAccess(Result).FWidthClass := wcExpanded;
      fsExtraCondensed:
        TdxFontFileOS2TableAccess(Result).FWidthClass := wcExtraCondensed;
      fsExtraExpanded:
        TdxFontFileOS2TableAccess(Result).FWidthClass := wcExtraExpanded;
      fsSemiCondensed:
        TdxFontFileOS2TableAccess(Result).FWidthClass := wcSemiCondensed;
      fsSemiExpanded:
        TdxFontFileOS2TableAccess(Result).FWidthClass := wcSemiExpanded;
      fsUltraCondensed:
        TdxFontFileOS2TableAccess(Result).FWidthClass := wcUltraCondensed;
      fsUltraExpanded:
        TdxFontFileOS2TableAccess(Result).FWidthClass := wcUltraExpanded;
    else
      TdxFontFileOS2TableAccess(Result).FWidthClass := wcMedium;
    end;
    AAscender := AValidatedAscender;
    ADescender := AValidatedDescender;
    AItalicAngle := Abs(AFontDescriptor.ItalicAngle);
    TdxFontFileOS2TableAccess(Result).FXHeight := AFontDescriptor.XHeight;
    TdxFontFileOS2TableAccess(Result).FCapHeight := AFontDescriptor.CapHeight;
  end;
  TdxFontFileOS2TableAccess(Result).FEmbeddingType :=
    TdxFontFileOS2EmbeddingType.PreviewPrintEmbedding;
  AEm := AAscender - ADescender;
  TdxFontFileOS2TableAccess(Result).FSubscriptXSize := Trunc(AEm / 5);
  TdxFontFileOS2TableAccess(Result).FSubscriptYSize :=
    TdxFontFileOS2TableAccess(Result).FSubscriptXSize;
  TdxFontFileOS2TableAccess(Result).FSubscriptXOffset :=
    Trunc(AEm * Sin(AItalicAngle * PI / 180));
  TdxFontFileOS2TableAccess(Result).FSubscriptYOffset := 0;
  TdxFontFileOS2TableAccess(Result).FSuperscriptXSize :=
    TdxFontFileOS2TableAccess(Result).FSubscriptXSize;
  TdxFontFileOS2TableAccess(Result).FSuperscriptYSize :=
    TdxFontFileOS2TableAccess(Result).FSubscriptYSize;
  TdxFontFileOS2TableAccess(Result).FSuperscriptXOffset :=
    TdxFontFileOS2TableAccess(Result).FSubscriptXOffset;
  TdxFontFileOS2TableAccess(Result).FSuperscriptYOffset :=
    Trunc(AAscender * 4 / 5);
  TdxFontFileOS2TableAccess(Result).FStrikeoutSize := Trunc(AEm / 10);
  TdxFontFileOS2TableAccess(Result).FStrikeoutPosition := Trunc(AAscender / 2);
  TdxFontFileOS2TableAccess(Result).FFamilyClass :=
    TdxFontFileOS2FamilyClass.ffcNoClassification;
  TdxFontFileOS2TableAccess(Result).FUnicodeRange1 :=
    TdxFontFileUnicodeRange1(EmptyValue);
  TdxFontFileOS2TableAccess(Result).FUnicodeRange2 :=
    TdxFontFileUnicodeRange2(EmptyValue);
  TdxFontFileOS2TableAccess(Result).FUnicodeRange3 :=
    TdxFontFileUnicodeRange3(EmptyValue);
  TdxFontFileOS2TableAccess(Result).FUnicodeRange4 :=
    TdxFontFileUnicodeRange4(EmptyValue);
  TdxFontFileOS2TableAccess(Result).FVendor := 'DX  ';
  TdxFontFileOS2TableAccess(Result).FSelection := TdxFontFileSelection.Empty;
  if (AFlags and Integer(ffItalic)) = Integer(ffItalic) then
    TdxFontFileOS2TableAccess(Result).FSelection :=
      TdxFontFileSelection(Integer(TdxFontFileOS2TableAccess(Result).FSelection)
      or Integer(TdxFontFileSelection.Italic));
  if AIsBold then
    TdxFontFileOS2TableAccess(Result).FSelection :=
      TdxFontFileSelection(Integer(TdxFontFileOS2TableAccess(Result).FSelection)
      or Integer(TdxFontFileSelection.Bold));

  AFirstChar := MaxInt;
  ALastChar := -MaxInt - 1;
  for AWidth in ACharset.Keys do
  begin
    AFirstChar := Min(AWidth, AFirstChar);
    ALastChar := Max(AWidth, ALastChar);
  end;
  TdxFontFileOS2TableAccess(Result).FFirstCharIndex := AFirstChar;
  TdxFontFileOS2TableAccess(Result).FLastCharIndex := ALastChar;
  TdxFontFileOS2TableAccess(Result).FTypoAscender := Trunc(AAscender);
  TdxFontFileOS2TableAccess(Result).FTypoDescender := Trunc(ADescender);
  TdxFontFileOS2TableAccess(Result).FTypoLineGap := Trunc(1.2 * AEm);
  TdxFontFileOS2TableAccess(Result).FWinAscent :=
    TdxFontFileOS2TableAccess(Result).FTypoAscender;
  TdxFontFileOS2TableAccess(Result).FWinDescent := Trunc(Abs(ADescender));
  TdxFontFileOS2TableAccess(Result).FCodePageRange1 :=
    TdxFontFileCodePageRange1.Latin1;
  TdxFontFileOS2TableAccess(Result).FCodePageRange2 :=
    TdxFontFileCodePageRange2(EmptyValue);
  TdxFontFileOS2TableAccess(Result).Changed;
end;

class function TdxFontFileHelper.CreatePostTable(AFont: TdxPDFCustomFont)
  : TdxFontFilePostTable;
var
  AData: TBytes;
  AProportion: Integer;
  ADescriptor: TdxPDFCustomFontDescriptor;
  AFontBBox: TdxRectF;
begin
  SetLength(AData, 0);
  Result := TdxFontFilePostTable.Create(AData);
  ADescriptor := AFont.FontDescriptor;
  if ADescriptor = nil then
  begin
    TdxFontFilePostTableAccess(Result).FItalicAngle := 0;
    TdxFontFilePostTableAccess(Result).FUnderlinePosition := 0;
    AProportion := TdxFontFilePostTable.FontIsProportionallySpaced;
  end
  else
  begin
    TdxFontFilePostTableAccess(Result).FItalicAngle := ADescriptor.ItalicAngle;
    AFontBBox := ADescriptor.FontBBox;
    if AFontBBox = dxNullRectF then
      TdxFontFilePostTableAccess(Result).FUnderlinePosition := 0
    else
      TdxFontFilePostTableAccess(Result).FUnderlinePosition :=
        Trunc(AFontBBox.Top / 2);
    if (Integer(ADescriptor.Flags) and Integer(ffFixedPitch))
      = Integer(ffFixedPitch) then
      AProportion := TdxFontFilePostTable.FontIsMonospaced
    else
      AProportion := TdxFontFilePostTable.FontIsProportionallySpaced;
  end;
  TdxFontFilePostTableAccess(Result).DataStream.WriteInt
    (TdxFontFilePostTableAccess(Result).Version);
  TdxFontFilePostTableAccess(Result).DataStream.WriteFixed
    (TdxFontFilePostTableAccess(Result).FItalicAngle);
  TdxFontFilePostTableAccess(Result).DataStream.WriteShort
    (TdxFontFilePostTableAccess(Result).FUnderlinePosition);
  TdxFontFilePostTableAccess(Result).DataStream.WriteShort
    (Trunc(TdxFontFilePostTableAccess(Result).FUnderlinePosition / 5));
  TdxFontFilePostTableAccess(Result).DataStream.WriteInt(AProportion);
  TdxFontFilePostTableAccess(Result).DataStream.WriteInt
    (TdxFontFilePostTable.MinMemType42);
  TdxFontFilePostTableAccess(Result).DataStream.WriteInt
    (TdxFontFilePostTable.MaxMemType42);
  TdxFontFilePostTableAccess(Result).DataStream.WriteInt
    (TdxFontFilePostTable.MinMemType1);
  TdxFontFilePostTableAccess(Result).DataStream.WriteInt
    (TdxFontFilePostTable.MaxMemType1);
end;

class function TdxFontFileHelper.GetOpenTypeVersion: TBytes;
begin
  SetLength(Result, 4);
  Result[0] := $4F;
  Result[1] := $54;
  Result[2] := $54;
  Result[3] := $4F;
end;

class function TdxFontFileHelper.GetTTFVersion: TBytes;
begin
  SetLength(Result, 4);
  Result[0] := $00;
  Result[1] := $01;
  Result[2] := $00;
  Result[3] := $00;
end;

class function TdxFontFileHelper.PatchCMapTable(AFile: TdxFontFile;
AFont: TdxPDFCustomFont; var AEncoding: TSmallIntDynArray)
  : TdxFontFileCMapSegmentMappingRecord;
var
  AEncodingID: TdxFontFileEncodingID;
  ASkipEncodingValidation: Boolean;
  ASimpleFontEncoding: TdxPDFSimpleFontEncoding;
begin
  if AFile.CMapTable = nil then
  begin
    if AFont.Symbolic then
      AEncodingID := TdxFontFileEncodingID.Undefined
    else
      AEncodingID := TdxFontFileEncodingID.UGL;
    Result := TdxFontFileCMapSegmentMappingRecord.CreateDefault(AEncodingID);
    AFile.AddTable(TdxFontFileCMapTable.Create(Result));
  end
  else
  begin
    ASkipEncodingValidation := False;
    if AFont is TdxPDFTrueTypeFont then
    begin
      ASkipEncodingValidation := False;
      ASimpleFontEncoding := (AFont as TdxPDFTrueTypeFont).Encoding;
      if (ASimpleFontEncoding is TdxPDFStandardEncoding) and
        ((ASimpleFontEncoding.Differences = nil) or
        (ASimpleFontEncoding.Differences <> nil) and
        (ASimpleFontEncoding.Differences.Count = 0)) then
        AFile.CMapTable.PopulateEncoding(AEncoding);
    end;
    ASkipEncodingValidation := ASkipEncodingValidation or
      (AFont is TdxPDFCIDType2Font);
    Result := AFile.CMapTable.Validate(ASkipEncodingValidation, AFont.Symbolic);
  end;
end;

class procedure TdxFontFileHelper.PatchNameTable(AFile: TdxFontFile;
const ABaseFontName: string);
begin
  if AFile.NameTable = nil then
    AFile.AddTable(TdxFontFileNameTable.Create(AFile.CMapTable, ABaseFontName))
  else
    AFile.NameTable.AddName(AFile.CMapTable, ABaseFontName);
end;

class procedure TdxFontFileHelper.ValidateOS2Table(AFile: TdxFontFile;
AFontDescriptor: TdxPDFCustomFontDescriptor);
var
  AAscent, ADescent, AWinAscent, AWinDescent: SmallInt;
  AFactor: Double;
begin
  if AFile.HheaTable <> nil then
  begin
    AAscent := AFile.HheaTable.Ascender;
    if AAscent > AFile.OS2Table.WinAscent then
      AFile.OS2Table.WinAscent := AAscent;
    ADescent := -AFile.HheaTable.Descender;
    if ADescent > AFile.OS2Table.WinDescent then
      AFile.OS2Table.WinDescent := ADescent;
  end;
  if (AFile.HeadTable <> nil) and (AFontDescriptor <> nil) and
    not TdxPDFUtils.IsRectEmpty(AFontDescriptor.FontBBox) then
  begin
    AFactor := AFile.HeadTable.UnitsPerEm /
      (AFontDescriptor.FontBBox.Top - AFontDescriptor.FontBBox.Bottom);
    AWinAscent := Trunc(AFactor * AFontDescriptor.FontBBox.Top);
    if AWinAscent > AFile.OS2Table.WinAscent then
      AFile.OS2Table.WinAscent := AWinAscent;

    AWinDescent := Trunc(-AFactor * AFontDescriptor.FontBBox.Bottom);
    if AWinDescent > AFile.OS2Table.WinDescent then
      AFile.OS2Table.WinDescent := AWinDescent;
  end;
end;

{ TdxPDFFontDataFacade }

constructor TdxPDFFontDataFacade.Create(const AFontFamily: string;
AFontStyle: TdxGPFontStyle; AFontMetrics: TdxFontFileFontMetrics);
var
  ANameBuilder: TStringBuilder;
  APostfix: string;
  C: Char;
begin
  inherited Create;
  FFontFamily := AFontFamily;
  FFontStyle := AFontStyle;
  FFontMetrics := AFontMetrics;
  ANameBuilder := TdxStringBuilderManager.Get(Length(AFontFamily));
  try
    for C in AFontFamily do
      if ((C <> ' ') and (C <> #13)) and (C <> #10) then
        ANameBuilder.Append(C);
    APostfix := '';
    if Bold then
      APostfix := APostfix + TdxPDFKeywords.Bold;
    if Italic then
      APostfix := APostfix + TdxPDFKeywords.Italic;
    if APostfix <> '' then
    begin
      ANameBuilder.Append(',');
      ANameBuilder.Append(APostfix);
    end;
    FFontName := ANameBuilder.ToString;
  finally
    TdxStringBuilderManager.Release(ANameBuilder);
  end;
end;

function TdxPDFFontDataFacade.GetFontFileBasedName: string;
begin
  Result := FFontName;
end;

function TdxPDFFontDataFacade.GetBold: Boolean;
begin
  Result := TdxPDFTextUtils.HasFlag(FFontStyle, TdxGPFontStyle.FontStyleBold)
end;

function TdxPDFFontDataFacade.GetItalic: Boolean;
begin
  Result := TdxPDFTextUtils.HasFlag(FFontStyle, TdxGPFontStyle.FontStyleItalic)
end;

function TdxPDFFontDataFacade.GetUnderline: Boolean;
begin
  Result := TdxPDFTextUtils.HasFlag(FFontStyle,
    TdxGPFontStyle.FontStyleUnderline);
end;

function TdxPDFFontDataFacade.GetStrikeout: Boolean;
begin
  Result := TdxPDFTextUtils.HasFlag(FFontStyle,
    TdxGPFontStyle.FontStyleStrikeout);
end;

function TdxPDFFontDataFacade.GetIsSymbolFont: Boolean;
begin
  Result := (FontFileBasedName = 'Symbol') or (FontFileBasedName = 'SymbolMT');
end;

{ TdxPDFFontFileDataFacade }

constructor TdxPDFFontFileDataFacade.Create(const AFontFamily: string;
AFontStyle: TdxGPFontStyle; AFontFile: TdxFontFile; AEmulateBold: Boolean);
begin
  inherited Create(AFontFamily, AFontStyle,
    TdxFontFileFontMetrics.Create(AFontFile));
  FFontFile := AFontFile;
  FEmulateBold := AEmulateBold;

  FEmulateItalic := Italic and (AFontFile.PostTable <> nil) and
    (Abs(AFontFile.PostTable.ItalicAngle) < MinDouble);
  FFontFileBasedName := FontName;
  if (AFontFile.NameTable <> nil) and (AFontFile.NameTable.MacFamilyName <> '')
    and (AFontFile.NameTable.MacFamilyName <> AFontFile.NameTable.FamilyName)
    and (AFontFile.NameTable.PostScriptName <> '') then
    FFontFileBasedName := AFontFile.NameTable.PostScriptName;
end;

destructor TdxPDFFontFileDataFacade.Destroy;
begin
  FreeAndNil(FFontFile);
  inherited Destroy;
end;

function TdxPDFFontFileDataFacade.GetFontFileBasedName: string;
begin
  Result := FFontFileBasedName;
end;

function TdxPDFFontFileDataFacade.GetEmulateBold: Boolean;
begin
  Result := FEmulateBold;
end;

function TdxPDFFontFileDataFacade.GetEmulateItalic: Boolean;
begin
  Result := FEmulateItalic;
end;

function TdxPDFFontFileDataFacade.CreateFontDescriptorData
  : TdxPDFFontDescriptorData;
var
  AFlags: Integer;
  ALoca: TdxFontFileLocaTable;
  AMaxp: TdxFontFileMaxpTable;
  AMetrics: TdxFontFileFontMetrics;
  ANumGlyphs: Integer;
  AOs2: TdxFontFileOS2Table;
  APanose: TdxFontFilePanose;
  APost: TdxFontFilePostTable;
  ASerifStyle: TdxFontFilePanoseSerifStyle;
begin
  APost := FFontFile.PostTable;
  AMetrics := Metrics;
  if IsSymbolFont then
    AFlags := Integer(TdxFontFileFlags.ffSymbolic)
  else
    AFlags := Integer(TdxFontFileFlags.ffNonSymbolic);

  AOs2 := FFontFile.OS2Table;
  if AOs2 <> nil then
    APanose := AOs2.Panose;

  if APanose.Proportion = TdxFontFilePanoseProportion.pMonospaced then
    AFlags := AFlags or Integer(TdxFontFileFlags.ffFixedPitch);

  ASerifStyle := APanose.SerifStyle;
  if (ASerifStyle <> TdxFontFilePanoseSerifStyle.ssNormalSans) and
    (ASerifStyle <> TdxFontFilePanoseSerifStyle.ssObtuseSans) and
    (ASerifStyle <> TdxFontFilePanoseSerifStyle.ssPerpendicularSans) then
    AFlags := AFlags or Integer(TdxFontFileFlags.ffSerif);

  if APanose.FamilyKind = TdxFontFilePanoseFamilyKind.fkLatinHandWritten then
    AFlags := AFlags or Integer(TdxFontFileFlags.ffScript);

  if Italic then
    AFlags := AFlags or Integer(TdxFontFileFlags.ffItalic);

  AMaxp := FFontFile.MaxpTable;
  if AMaxp <> nil then
    ANumGlyphs := AMaxp.NumGlyphs
  else
  begin
    ALoca := FFontFile.LocaTable;
    if ALoca <> nil then
      ANumGlyphs := Max(0, Length(ALoca.GlyphOffsets) - 1)
    else
      ANumGlyphs := 0;
  end;
  Result := TdxPDFFontDescriptorData.Create(Metrics, AFlags, APost.ItalicAngle,
    Bold, ANumGlyphs);
end;

function TdxPDFFontFileDataFacade.CreateFontFileSubset
  (ASubset: TdxPDFIntegerStringDictionary): TBytes;
begin
  Result := FFontFile.CreateSubset(ASubset);
end;

function TdxPDFFontFileDataFacade.GetGlyphWidth(AGlyphIndex: Integer): Double;
begin
  Result := FFontFile.GetCharacterWidth(AGlyphIndex);
end;

{ TdxPDFCharacterSet }

constructor TdxPDFCharacterSet.Create;
begin
  inherited Create;
  FCurrentSet := TdxPDFIntegerStringDictionary.Create;
  FHasNewCharacters := True;
end;

destructor TdxPDFCharacterSet.Destroy;
begin
  FreeAndNil(FCurrentSet);
  inherited Destroy;
end;

procedure TdxPDFCharacterSet.AddSubset(AToUnicode
  : TdxPDFIntegerStringDictionary);
var
  ACode: Integer;
  APair: TPair<Integer, string>;
begin
  for APair in AToUnicode do
  begin
    ACode := APair.Key;
    if not FCurrentSet.ContainsKey(ACode) then
    begin
      FCurrentSet.Add(ACode, APair.Value);
      FHasNewCharacters := True;
    end;
  end;
end;

{ TdxPDFEditableFontObject }

constructor TdxPDFEditableFontObject.Create(AShouldUseTwoByteGlyphIndex
  : Boolean);
begin
  inherited Create;
  FCharSet := TdxPDFCharacterSet.Create;
  FShouldUseTwoByteGlyphIndex := AShouldUseTwoByteGlyphIndex;
end;

destructor TdxPDFEditableFontObject.Destroy;
begin
  FreeAndNil(FCharSet);
  inherited Destroy;
end;

procedure TdxPDFEditableFontObject.UpdateFont;
begin
  if FCharSet.HasNewCharacters then
  begin
    Font.CMap := TdxPDFCMapStreamParser.Parse(TdxPDFObjectAccess(Font)
      .Repository, TdxPDFCharacterMapping.CreateCharacterMappingData
      (FCharSet.CurrentSet, 'DEVEXP'));
    UpdateFontFile(FCharSet.CurrentSet);
    FCharSet.HasNewCharacters := False;
  end;
end;

procedure TdxPDFEditableFontObject.AddSubset
  (ASubset: TdxPDFIntegerStringDictionary);
begin
  FCharSet.AddSubset(ASubset);
end;

{ TdxPDFEmbeddedEditableFontObject }

constructor TdxPDFEmbeddedEditableFontObject.Create(AFontDataFacade
  : TdxPDFFontDataFacade; AIsUnsupportedFont: Boolean);
begin
  inherited Create(True);
  FFontDataFacade := AFontDataFacade;
  FIsUnsupportedFont := AIsUnsupportedFont;
  Font := TdxPDFDeferredCIDType2Font.Create
    ('DEVEXP+' + AFontDataFacade.FontName,
    TdxPDFCIDType2FontDescriptor.Create
    (AFontDataFacade.CreateFontDescriptorData));
end;

destructor TdxPDFEmbeddedEditableFontObject.Destroy;
begin
  Font := nil;
  inherited Destroy;
end;

function TdxPDFEmbeddedEditableFontObject.GetFont: TdxPDFCustomFont;
begin
  Result := FFont;
end;

function TdxPDFEmbeddedEditableFontObject.IsUnsupportedFont: Boolean;
begin
  Result := FIsUnsupportedFont;
end;

procedure TdxPDFEmbeddedEditableFontObject.SetFont(const AValue
  : TdxPDFCustomFont);
begin
  dxPDFChangeValue(AValue, TdxPDFReferencedObject(FFont));
end;

procedure TdxPDFEmbeddedEditableFontObject.UpdateFontFile
  (ASubset: TdxPDFIntegerStringDictionary);
var
  AIndex: Integer;
  ADictionary: TdxPDFIntegerDoubleDictionary;
begin
  ADictionary := TdxPDFIntegerDoubleDictionary.Create;
  try
    for AIndex in ASubset.Keys do
      ADictionary.Add(AIndex, FFontDataFacade.GetGlyphWidth(AIndex));
    FFont.SetWidths(ADictionary);
  finally
    ADictionary.Free;
  end;
end;

{ TdxPDFEditableFontData }

constructor TdxPDFEditableFontData.Create(AFontObject: IdxPDFFontObject;
AFontDataFacade: TdxPDFFontDataFacade; AMapper: TdxPDFGlyphMapper);
begin
  inherited Create(nil);
  FFontObject := AFontObject;
  FFontDataFacade := AFontDataFacade;
  FMapper := AMapper;
end;

function TdxPDFEditableFontData.CreateGlyphRun: TdxPDFGlyphRun;
begin
  Result := FMapper.CreateGlyphRun;
end;

function TdxPDFEditableFontData.GetTextWidth(const AText: string;
AFontSize: Double; ATextState: TdxPDFInteractiveFormFieldTextState): Double;
var
  AFactor, ACharWidth: Double;
  ALastIndex, I: Integer;
  C: Char;
begin
  Result := 0;
  if (AFontSize > 0) and (AText <> '') then
  begin
    ALastIndex := Length(AText) - 1;
    AFactor := AFontSize * ATextState.HorizontalScaling / 100000;
    for I := 0 to ALastIndex do
    begin
      C := AText[I + 1];
      ACharWidth := GetCharacterWidth(C) * AFactor;
      if ACharWidth > 0 then
        Result := Result + ACharWidth + ATextState.CharacterSpacing;
      if (C = ' ') and (I < ALastIndex) then
        Result := Result + ATextState.WordSpacing;
    end;
  end;
end;

function TdxPDFEditableFontData.ProcessString(const AStr: string;
AFlags: TdxPDFGlyphMappingFlags): TdxPDFGlyphRun;
var
  AGlyphMapping: TdxPDFGlyphMappingResult;
begin
  AGlyphMapping := FMapper.MapString(AStr, AFlags);
  try
    FFontObject.AddSubset(AGlyphMapping.Mapping);
    Result := AGlyphMapping.GlyphRun;
  finally
    AGlyphMapping.Mapping.Free;
  end;
end;

procedure TdxPDFEditableFontData.UpdateFont;
begin
  FFontObject.UpdateFont;
end;

procedure TdxPDFEditableFontData.UpdateFontFile;
begin
  FFontObject.UpdateFont;
end;

procedure TdxPDFEditableFontData.DestroySubClasses;
begin
  FreeAndNil(FMapper);
  FreeAndNil(FFontDataFacade);
  FFontObject := nil;
  inherited DestroySubClasses;
end;

function TdxPDFEditableFontData.GetBold: Boolean;
begin
  Result := HasFlag(TdxGPFontStyle.FontStyleBold);
end;

function TdxPDFEditableFontData.GetFont: TdxPDFCustomFont;
begin
  Result := FFontObject.GetFont;
end;

function TdxPDFEditableFontData.GetItalic: Boolean;
begin
  Result := HasFlag(TdxGPFontStyle.FontStyleItalic);
end;

function TdxPDFEditableFontData.GetMetrics: TdxFontFileFontMetrics;
begin
  Result := FFontDataFacade.Metrics;
end;

function TdxPDFEditableFontData.GetNeedEmulateBold: Boolean;
begin
  Result := FFontDataFacade.EmulateBold;
end;

function TdxPDFEditableFontData.GetNeedEmulateItalic: Boolean;
begin
  Result := FFontDataFacade.EmulateItalic;
end;

function TdxPDFEditableFontData.GetStrikeout: Boolean;
begin
  Result := HasFlag(TdxGPFontStyle.FontStyleStrikeout);
end;

function TdxPDFEditableFontData.GetUnderline: Boolean;
begin
  Result := HasFlag(TdxGPFontStyle.FontStyleUnderline);
end;

function TdxPDFEditableFontData.GetCharacterWidth(ACh: Char): Double;
begin
  Result := FFontDataFacade.GetGlyphWidth(FMapper.GetGlyphIndex(ACh));
end;

function TdxPDFEditableFontData.HasFlag(AFontStyle: TdxGPFontStyle): Boolean;
begin
  Result := (Integer(FFontStyle) and Integer(AFontStyle)) <> 0;
end;

{ TdxPDFEditableFontDataCache }

constructor TdxPDFEditableFontDataCache.Create(AFontFamilyInfos
  : TdxFontFamilyInfos);
begin
  inherited Create;
  InitializeCriticalSectionAndSpinCount(FLock, 1024);
  FFontFamilyInfos := AFontFamilyInfos;
  FFontCache := TObjectDictionary<Integer, TdxPDFEditableFontData>.Create
    ([doOwnsValues]);
end;

destructor TdxPDFEditableFontDataCache.Destroy;
begin
  FreeAndNil(FFontCache);
  DeleteCriticalSection(FLock);
  inherited Destroy;
end;

procedure TdxPDFEditableFontDataCache.Clear;
begin
  FFontCache.Clear;
end;

procedure TdxPDFEditableFontDataCache.UpdateFonts;
var
  AFontData: TdxPDFEditableFontData;
begin
  EnterCriticalSection(FLock);
  try
    for AFontData in FFontCache.Values do
      AFontData.UpdateFont;
  finally
    LeaveCriticalSection(FLock);
  end;
end;

function TdxPDFEditableFontDataCache.EmbedFont(const AFontFamily
  : string): Boolean;
begin
  Result := True;
end;

function TdxPDFEditableFontDataCache.TryGetEditableFontData
  (const AKey: TCacheKey; out AFontData: TdxPDFEditableFontData): Boolean;
begin
  Result := FFontCache.TryGetValue(AKey.GetHash, AFontData);
end;

procedure TdxPDFEditableFontDataCache.CacheEditableFontData
  (const AKey: TCacheKey; AFontData: TdxPDFEditableFontData);
begin
  EnterCriticalSection(FLock);
  try
    FFontCache.AddOrSetValue(AKey.GetHash, AFontData);
  finally
    LeaveCriticalSection(FLock);
  end;
end;

{ TdxPDFEditableFontDataCache.TCacheKey }

class function TdxPDFEditableFontDataCache.TCacheKey.Create(const AFontFamily
  : string; AStyle: TdxGPFontStyle): TCacheKey;
begin
  Result.FontFamily := AFontFamily;
  Result.Style := AStyle;
end;

function TdxPDFEditableFontDataCache.TCacheKey.GetHash: Integer;

  procedure AddToHash(var AHash: Integer; const AData; ADataSize: Integer);
  begin
    AHash := dxBobJenkinsHash(AData, ADataSize, AHash);
  end;

  procedure AddStringToHash(var AHash: Integer; const AData: AnsiString);
  begin
    AddToHash(AHash, AData[1], Length(AData));
  end;

begin
  Result := 0;
  AddStringToHash(Result, dxStringToAnsiString(FontFamily));
  AddToHash(Result, Style, SizeOf(Style));
end;

{ TdxPDFGDIEditableFontDataCache }

function TdxPDFGDIEditableFontDataCache.GetFontData(const AFontFamily: string;
AFontStyle: TdxGPFontStyle; ACreateFont: Boolean): TdxPDFEditableFontData;
var
  AFont: TdxGPFont;
  AFontID: TCacheKey;
begin
  Result := nil;
  AFontID := TCacheKey.Create(AFontFamily, AFontStyle);
  if ACreateFont and not TryGetEditableFontData(AFontID, Result) then
  begin
    AFont := TdxGPFont.Create(AFontFamily, 9, AFontStyle);
    try
      Result := TdxPDFEditableFontDataFactory.Create(AFont);
      CacheEditableFontData(AFontID, Result);
    finally
      AFont.Free;
    end;
  end;
end;

function TdxPDFGDIEditableFontDataCache.SearchFontData(const AFontFamilyName
  : string; AFontStyle: TdxGPFontStyle): TdxPDFEditableFontData;
begin
  Result := GetFontData(AFontFamilyName, AFontStyle,
    FontFamilyInfos.Contains(AFontFamilyName));
end;

{ TdxPDFCIDCharset }

function TdxPDFCIDCharset.GetUnicode(ACid: SmallInt): SmallInt;
begin
  Result := 0;
end;

{ TdxPDFSimpleFontCodePointMapping }

constructor TdxPDFSimpleFontCodePointMapping.Create(const AGlyphIndicesMapping
  : TSmallIntDynArray; const AUnicodeMapping: TSmallIntDynArray);
begin
  inherited Create;
  FGlyphIndicesMapping := AGlyphIndicesMapping;
  FUnicodeMapping := AUnicodeMapping;
end;

procedure TdxPDFSimpleFontCodePointMapping.MapCodes(const ACodePoints
  : TSmallIntDynArray; const AMappingTable: TSmallIntDynArray);
var
  ACount, ATableLength, I: Integer;
  ACp: SmallInt;
begin
  ACount := Length(ACodePoints);
  ATableLength := Length(AMappingTable);
  for I := 0 to ACount - 1 do
  begin
    ACp := ACodePoints[I];
    if (ACp < 0) or (ACp >= ATableLength) then
      ACodePoints[I] := ACp
    else
      ACodePoints[I] := AMappingTable[ACp];
  end;
end;

function TdxPDFSimpleFontCodePointMapping.UpdateCodePoints(const ACodePoints
  : TSmallIntDynArray; AUseEmbeddedFontEncoding: Boolean): Boolean;
begin
  if (FGlyphIndicesMapping <> nil) and AUseEmbeddedFontEncoding then
  begin
    MapCodes(ACodePoints, FGlyphIndicesMapping);
    Exit(True);
  end
  else if FUnicodeMapping <> nil then
    MapCodes(ACodePoints, FUnicodeMapping);
  Result := False;
end;

{ TdxPDFNonEmbeddedCIDFontCodePointMapping }

constructor TdxPDFNonEmbeddedCIDFontCodePointMapping.Create
  (ACharset: TdxPDFCIDCharset);
begin
  inherited Create;
  FCharSet := ACharset;
end;

destructor TdxPDFNonEmbeddedCIDFontCodePointMapping.Destroy;
begin
  FreeAndNil(FCharSet);
  inherited Destroy;
end;

function TdxPDFNonEmbeddedCIDFontCodePointMapping.UpdateCodePoints
  (const ACodePoints: TSmallIntDynArray;
AUseEmbeddedFontEncoding: Boolean): Boolean;
var
  I: Integer;
begin
  for I := 0 to Length(ACodePoints) - 1 do
    ACodePoints[I] := FCharSet.GetUnicode(ACodePoints[I]);
  Result := False;
end;

{ TdxPDFCompositeFontCodePointMapping }

constructor TdxPDFCompositeFontCodePointMapping.Create(const ACidToGidMap
  : TSmallIntDynArray; AMapping: TdxPDFSmallIntegerDictionary);
begin
  inherited Create;
  FCidToGidMap := ACidToGidMap;
  FMapping := AMapping;
end;

destructor TdxPDFCompositeFontCodePointMapping.Destroy;
begin
  FreeAndNil(FMapping);
  inherited Destroy;
end;

function TdxPDFCompositeFontCodePointMapping.UpdateCodePoints(const ACodePoints
  : TSmallIntDynArray; AUseEmbeddedFontEncoding: Boolean): Boolean;
var
  I, AMapLength, ALength: Integer;
  AGid: SmallInt;
  ACid: USHORT;
begin
  if FCidToGidMap <> nil then
  begin
    AMapLength := Length(FCidToGidMap);
    ALength := Length(ACodePoints);
    for I := 0 to ALength - 1 do
    begin
      ACid := USHORT(ACodePoints[I]);
      if ACid >= AMapLength then
        ACodePoints[I] := 0
      else
        ACodePoints[I] := FCidToGidMap[ACid];
    end;
  end;
  if FMapping <> nil then
  begin
    ALength := Length(ACodePoints);
    for I := 0 to ALength - 1 do
    begin
      if FMapping.TryGetValue(ACodePoints[I], AGid) then
        ACodePoints[I] := AGid
      else
        ACodePoints[I] := 0;
    end;
  end;
  Result := True;
end;

{ TdxPDFFontProgramFacade }

constructor TdxPDFFontProgramFacade.Create(const AFontBBox: TdxPDFRectangle;
ATop, ABottom: Double; AMapping: TdxPDFCodePointMapping);
begin
  Create(AFontBBox, ATop, ABottom, AMapping, fcBasic);
end;

constructor TdxPDFFontProgramFacade.Create(const AFontBBox: TdxPDFRectangle;
ATop, ABottom: Double; AMapping: TdxPDFCodePointMapping;
ACharset: TdxPDFFontCharset);
begin
  inherited Create;
  FCharSet := ACharset;
  FMapping := AMapping;
  FFontBBox := AFontBBox;
  FTop := ATop;
  FBottom := ABottom;
end;

destructor TdxPDFFontProgramFacade.Destroy;
begin
  FreeAndNil(FMapping);
  inherited Destroy;
end;

class function TdxPDFFontProgramFacade.GetUnicodeMapping
  (AEncoding: TdxPDFSimpleFontEncoding): TSmallIntDynArray;
var
  I, AGlyphCode: Word;
begin
  SetLength(Result, 256);
  FillChar(Result[0], Length(Result), 0);
  for I := 0 to 255 do
  begin
    if AEncoding = nil then
      AGlyphCode := I
    else if not TdxPDFUnicodeConverter.TryGetGlyphCode
      (AEncoding.GetGlyphName(I), AGlyphCode) then
      AGlyphCode := 32;
    Result[I] := AGlyphCode;
  end;
end;

function TdxPDFFontProgramFacade.UpdateCodePoints(const ACodePoints
  : TSmallIntDynArray; AUseEmbeddedFontEncoding: Boolean): Boolean;
begin
  if FMapping <> nil then
    Result := FMapping.UpdateCodePoints(ACodePoints, AUseEmbeddedFontEncoding)
  else
    Result := False;
end;

{ TdxPDFNonEmbeddedFontProgramFacade }

constructor TdxPDFNonEmbeddedFontProgramFacade.Create(AFont: TdxPDFType0Font;
AMapping: TdxPDFCodePointMapping; ACharset: TdxPDFFontCharset);
begin
  inherited Create(TdxPDFRectangle.Null, dxPDFInvalidValue, dxPDFInvalidValue,
    AMapping, ACharset);
end;

constructor TdxPDFNonEmbeddedFontProgramFacade.Create(AFont: TdxPDFSimpleFont);
var
  ASimpleFontEncoding: TdxPDFSimpleFontEncoding;
begin
  if AFont.Encoding.IsEmpty and (AFont is TdxPDFTrueTypeFont) then
    ASimpleFontEncoding := nil
  else
    ASimpleFontEncoding := AFont.Encoding;
  inherited Create(TdxPDFRectangle.Null, dxPDFInvalidValue, dxPDFInvalidValue,
    TdxPDFSimpleFontCodePointMapping.Create(nil,
    GetUnicodeMapping(ASimpleFontEncoding)));
end;

class function TdxPDFNonEmbeddedFontProgramFacade.CreateForSimpleFont
  (AFont: TdxPDFSimpleFont): TdxPDFNonEmbeddedFontProgramFacade;
begin
  Result := TdxPDFNonEmbeddedFontProgramFacade.Create(AFont);
end;

class function TdxPDFNonEmbeddedFontProgramFacade.CreateForType0Font
  (AFont: TdxPDFType0Font): TdxPDFNonEmbeddedFontProgramFacade;
begin
  Result := TdxPDFNonEmbeddedFontProgramFacade.Create(AFont,
    TdxPDFCompositeFontCodePointMapping.Create(AFont.CidToGidMap, nil),
    fcBasic);
end;

{ TdxPDFCFFFontProgramFacade }

constructor TdxPDFCFFFontProgramFacade.Create(const AFontBBox: TdxPDFRectangle;
AMapping: TdxPDFCodePointMapping);
begin
  inherited Create(AFontBBox, dxPDFInvalidValue, dxPDFInvalidValue, AMapping);
  FCharSet := TdxPDFSmallIntegerDictionary.Create;
end;

destructor TdxPDFCFFFontProgramFacade.Destroy;
begin
  FreeAndNil(FCharSet);
  inherited Destroy;
end;

class function TdxPDFCFFFontProgramFacade.DoCreate(const ACompactFontFile
  : TBytes; AFont: TObject;
ACreateMapping: TFunc<TdxType1FontCompactFontProgram, TdxPDFCodePointMapping>)
  : TdxPDFCFFFontProgramFacade;
var
  AFontBBox: TdxPDFRectangle;
  AFontProgram: TdxType1FontCompactFontProgram;
  APatchedData: TBytes;
begin
  if ACompactFontFile <> nil then
  begin
    AFontProgram := TdxType1FontCompactFontProgram.Parse(ACompactFontFile);
    try
      if AFontProgram.Validate then
        APatchedData := TdxCompactFontFormatTopDictIndexWriter.
          Write(AFontProgram)
      else
        APatchedData := ACompactFontFile;
      if TdxPDFUtils.RectIsEqual(AFontProgram.FontBBox,
        TdxType1FontCompactFontProgram.DefaultFontBBox, 0.001) then
        AFontBBox := TdxPDFRectangle.Null
      else
        AFontBBox := TdxPDFRectangle.Create(AFontProgram.FontBBox.Left,
          AFontProgram.FontBBox.Bottom, AFontProgram.FontBBox.Right,
          AFontProgram.FontBBox.Top);
      Result := TdxPDFCFFFontProgramFacade.Create(AFontBBox,
        ACreateMapping(AFontProgram));
      Result.FontFileData := APatchedData;
      Result.OriginalFontData := ACompactFontFile;
      Result.Charset.Assign(AFontProgram.Charset.SidToGidMapping);
    finally
      dxPDFFreeObject(AFontProgram);
    end;
  end
  else
    Result := nil;
end;

class function TdxPDFCFFFontProgramFacade.CreateForSimpleFont
  (AFont: TdxPDFSimpleFont; const ACompactFontFile: TBytes)
  : TdxPDFCFFFontProgramFacade;
begin
  Result := DoCreate(ACompactFontFile, AFont,
    function(AProgram: TdxType1FontCompactFontProgram): TdxPDFCodePointMapping
    begin
      Result := AProgram.GetSimpleMapping(AFont.Encoding)
        as TdxPDFCodePointMapping;
    end);
end;

class function TdxPDFCFFFontProgramFacade.CreateForType0Font
  (AFont: TdxPDFType0Font; const ACompactFontFile: TBytes)
  : TdxPDFCFFFontProgramFacade;
begin
  Result := DoCreate(ACompactFontFile, AFont,
    function(AProgram: TdxType1FontCompactFontProgram): TdxPDFCodePointMapping
    begin
      Result := AProgram.GetCompositeMapping(AFont.CidToGidMap)
        as TdxPDFCodePointMapping;
    end);
end;

function TdxPDFCFFFontProgramFacade.GetGlyphCount: Integer;
begin
  if Charset = nil then
    Result := 1
  else
    Result := Charset.Count + 1;
end;

{ TdxPDFTrueTypeFontProgramFacade }

constructor TdxPDFTrueTypeFontProgramFacade.Create(const AFontBBox
  : TdxPDFRectangle; ATop, ABottom: Double; AMapping: TdxPDFCodePointMapping;
const AFontFileData: TBytes);
begin
  inherited Create(AFontBBox, ATop, ABottom, AMapping);
  FFontFileData := AFontFileData;
end;

class function TdxPDFTrueTypeFontProgramFacade.DoCreate(AFontObject: TObject;
const AFontFileData: TBytes;
ACreateMapping: TFunc<TdxFontFile, TdxPDFCodePointMapping>)
  : TdxPDFTrueTypeFontProgramFacade;
var
  AFile: TdxFontFile;
  AMapping: TdxPDFCodePointMapping;
  AFont: TdxPDFCustomFont;
  AFontDescriptor: TdxPDFCustomFontDescriptor;
  AIsSymbolic: Boolean;
  ASegmentMappingFormatEntry: TdxFontFileCMapSegmentMappingRecord;
  ACMapEntry: TdxFontFileCMapTable;
  ANameEntry: TdxFontFileNameTable;
  AHhea: TdxFontFileHheaTable;
  AOs2Entry: TdxFontFileOS2Table;
  AHeadEntry: TdxFontFileHeadTable;
  AFontBBox: TdxPDFRectangle;
  AActualBottom, AActualTop: Double;
  AUnitsPerEm, AFactor: Double;
begin
  AFile := TdxFontFile.Create(AFontFileData, False);
  try
    AMapping := ACreateMapping(AFile);
    AFont := AFontObject as TdxPDFCustomFont;
    AFontDescriptor := AFont.FontDescriptor;
    AIsSymbolic := AFont.Symbolic;
    ACMapEntry := AFile.CMapTable;
    if ACMapEntry = nil then
    begin
      ASegmentMappingFormatEntry := TdxFontFileCMapSegmentMappingRecord.
        CreateDefault(TdxFontFileEncodingID(IfThen(AIsSymbolic, 0, 1)));
      ACMapEntry := TdxFontFileCMapTable.Create(ASegmentMappingFormatEntry);
      AFile.AddTable(ACMapEntry);
    end
    else
      ACMapEntry.Validate(not(AFont is TdxPDFTrueTypeFont), AIsSymbolic);
    ANameEntry := AFile.NameTable;
    if ANameEntry = nil then
    begin
      ANameEntry := TdxFontFileNameTable.Create(ACMapEntry, AFont.UniqueName);
      AFile.AddTable(ANameEntry);
    end
    else
      ANameEntry.AddName(ACMapEntry, AFont.UniqueName);
    AHhea := AFile.HheaTable;
    if AHhea <> nil then
      AHhea.Validate;
    AOs2Entry := AFile.OS2Table;
    AHeadEntry := AFile.HeadTable;
    if AOs2Entry <> nil then
      TdxFontFileHelper.ValidateOS2Table(AFile, AFontDescriptor);
    AFontBBox := TdxPDFRectangle.Null;
    AActualBottom := dxPDFInvalidValue;
    AActualTop := dxPDFInvalidValue;
    if AHeadEntry <> nil then
    begin
      if AFile.GlyphTable <> nil then
        AHeadEntry.Validate(AFile.GlyphTable.Glyphs);
      AUnitsPerEm := AHeadEntry.UnitsPerEm;
      AFactor := 1000.0 / AUnitsPerEm;
      AFontBBox := TdxPDFRectangle.Create(AHeadEntry.XMin * AFactor,
        AHeadEntry.YMin * AFactor, AHeadEntry.XMax * AFactor,
        AHeadEntry.YMax * AFactor);
      if ((AHeadEntry.YMax - AHeadEntry.YMin) / AUnitsPerEm > 2) and
        (AOs2Entry <> nil) then
      begin
        AActualTop := (AOs2Entry.TypoAscender + AOs2Entry.TypoLineGap)
          * AFactor;
        AActualBottom := AOs2Entry.TypoDescender * AFactor;
      end;
    end;
    if AFile.HmtxTable <> nil then
      AFile.HmtxTable.Validate(AFile);
    Exit(TdxPDFTrueTypeFontProgramFacade.Create(AFontBBox, AActualTop,
      AActualBottom, AMapping, AFile.GetData));
  finally
    AFile.Free;
  end;
end;

class function TdxPDFTrueTypeFontProgramFacade.CreateForSimpleFont
  (AFont: TdxPDFSimpleFont; const ATrueTypeFontFile: TBytes)
  : TdxPDFTrueTypeFontProgramFacade;
begin
  Result := DoCreate(AFont, ATrueTypeFontFile,
    function(AFontFile: TdxFontFile): TdxPDFCodePointMapping
    begin
      Result := TdxFontFileHelper.GetSimpleMapping(AFontFile, AFont.Encoding,
        AFont.Symbolic);
    end);
end;

class function TdxPDFTrueTypeFontProgramFacade.CreateForType0Font
  (AFont: TdxPDFType0Font; const ATrueTypeFontFile: TBytes)
  : TdxPDFTrueTypeFontProgramFacade;
begin
  Result := DoCreate(AFont, ATrueTypeFontFile,
    function(AFontFile: TdxFontFile): TdxPDFCodePointMapping
    begin
      Result := TdxPDFCompositeFontCodePointMapping.Create
        (AFont.CidToGidMap, nil);
    end);
end;

{ TdxPDFType1FontProgramFacade }

constructor TdxPDFType1FontProgramFacade.Create(const AFontBBox
  : TdxPDFRectangle; AMapping: TdxPDFCodePointMapping);
begin
  inherited Create(AFontBBox, dxPDFInvalidValue, dxPDFInvalidValue, AMapping);
end;

class function TdxPDFType1FontProgramFacade.DoCreate(AFontObject: TObject;
AFontFileData: TdxPDFType1FontFileData;
ACreateMapping: TFunc<TdxPDFType1FontClassicFontProgram,
  TdxPDFCodePointMapping>): TdxPDFType1FontProgramFacade;
var
  AData, AActualData, AValidFontProgram: TBytes;
  APlainTextLength, AActualPlainTextLength, ACipherTextLength,
    ANullSegmentLength, ARemainLength: Integer;
  AActualFontFileData: TdxPDFType1FontFileData;
  AFontProgram: TdxPDFType1FontClassicFontProgram;
  AMapping: TdxPDFCodePointMapping;
  ABoundingBox: TdxPDFRectangle;
  AFont: TdxPDFCustomFont;
begin
  AFont := AFontObject as TdxPDFCustomFont;
  AData := AFontFileData.Data;
  APlainTextLength := AFontFileData.PlainTextLength;
  AActualData := AData;
  AFontProgram := TdxPDFType1FontClassicFontProgram.Parse(AFont.BaseFont,
    AFontFileData);
  AMapping := nil;
  ABoundingBox := TdxPDFRectangle.Null;
  if AFontProgram <> nil then
  begin
    try
      AFontProgram.Validate(AFont);
      AValidFontProgram := TEncoding.UTF8.GetBytes(AFontProgram.ToPostScript);
      AActualPlainTextLength := Length(AValidFontProgram);
      ACipherTextLength := AFontFileData.CipherTextLength;
      ANullSegmentLength := AFontFileData.NullSegmentLength;
      ARemainLength := ACipherTextLength + ANullSegmentLength;
      SetLength(AActualData, AActualPlainTextLength + ARemainLength);
      SetLength(AActualData, AActualPlainTextLength + ARemainLength);
      TdxPDFUtils.ClearData(AActualData);
      TdxPDFUtils.CopyData(AValidFontProgram, 0, AActualData, 0,
        AActualPlainTextLength);
      TdxPDFUtils.CopyData(AData, APlainTextLength, AActualData,
        AActualPlainTextLength, ARemainLength);
      AActualFontFileData := TdxPDFType1FontFileData.Create(AActualData,
        AActualPlainTextLength, ACipherTextLength, ANullSegmentLength);
      ABoundingBox := TdxPDFRectangle.Create(AFontProgram.FontBBox.Left,
        AFontProgram.FontBBox.Bottom, AFontProgram.FontBBox.Right,
        AFontProgram.FontBBox.Top);
      AMapping := ACreateMapping(AFontProgram);
    finally
      dxPDFFreeObject(AFontProgram);
    end;
  end
  else
    AActualFontFileData := AFontFileData;
  Result := TdxPDFType1FontProgramFacade.Create(ABoundingBox, AMapping);
  Result.FontFile := AActualFontFileData;
end;

class function TdxPDFType1FontProgramFacade.CreateForSimpleFont
  (AFont: TdxPDFSimpleFont; AFontFileData: TdxPDFType1FontFileData)
  : TdxPDFType1FontProgramFacade;
begin
  Result := DoCreate(AFont, AFontFileData,
    function(AProgram: TdxPDFType1FontClassicFontProgram)
      : TdxPDFCodePointMapping
    begin
      Result := AProgram.GetSimpleMapping(AFont.Encoding)
        as TdxPDFCodePointMapping;
    end);
end;

class function TdxPDFType1FontProgramFacade.CreateForType0Font
  (AFont: TdxPDFType0Font; AFontFileData: TdxPDFType1FontFileData)
  : TdxPDFType1FontProgramFacade;
begin
  Result := DoCreate(AFont, AFontFileData,
    function(AProgram: TdxPDFType1FontClassicFontProgram)
      : TdxPDFCodePointMapping
    begin
      Result := AProgram.GetCompositeMapping(AFont.CidToGidMap)
        as TdxPDFCodePointMapping;
    end);
end;

end.
