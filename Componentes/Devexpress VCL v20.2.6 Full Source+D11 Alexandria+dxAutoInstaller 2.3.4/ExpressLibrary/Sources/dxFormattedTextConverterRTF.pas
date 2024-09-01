{ ******************************************************************** }
{ }
{ Developer Express Visual Component Library }
{ Express Cross Platform Library controls }
{ }
{ Copyright (c) 2000-2021 Developer Express Inc. }
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

unit dxFormattedTextConverterRTF;

{$I cxVer.inc}
{$SCOPEDENUMS ON}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  SysUtils, Graphics, Windows, Classes, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxGenerics, cxGraphics, dxFormattedText,
  dxFormattedTextConverterBBCode;

type
  { TdxFormattedTextConverterRTF }

  TdxFormattedTextConverterRTF = class(TdxFormattedTextConverterBBCode)
  public const
    DefaultRichEditCompatibility: Boolean = False;
  public
    class function CanImport(const ASource: string): Boolean; override;
    class procedure Import(ATarget: TdxFormattedText; const ASource: string;
      ADefaultFont: TFont = nil); overload; override;
    class procedure Import(ATarget: TdxFormattedText; const ASource: string;
      ADefaultFont: TFont; ARichEditCompatibility: Boolean);
      reintroduce; overload;
    class function Export(ASource: TdxFormattedText; ADefaultFont: TFont = nil)
      : string; overload; override;
    class function Export(ASource: TdxFormattedText; ADefaultFont: TFont;
      ARichEditCompatibility: Boolean): string; reintroduce; overload;
  end;

  { TdxFormattedTextRtfHelper }

  TdxFormattedTextRtfHelper = class
  public
    class function GetBBCodeText(ADefaultFont: TFont; const ARtfText: string)
      : string; overload;
    class function GetBBCodeText(ADefaultFont: TFont; const ARtfText: string;
      ARichEditCompatibility: Boolean): string; overload;
    class function GetRtfText(ADefaultFont: TFont;
      AFormattedText: TdxFormattedText): string; overload;
    class function GetRtfText(ADefaultFont: TFont;
      AFormattedText: TdxFormattedText; ARichEditCompatibility: Boolean)
      : string; overload;
    class function GetRtfText(ADefaultFont: TFont; const ABBCodeString: string)
      : string; overload;
    class function GetRtfText(ADefaultFont: TFont; const ABBCodeString: string;
      ARichEditCompatibility: Boolean): string; overload;
  end;

implementation

uses
  dxEncoding,
  StrUtils,
  dxStringHelper, dxCharacters, dxFontHelpers, dxCoreGraphics,
  dxDocumentLayoutUnitConverter;

type
  TdxRtfBuilder = class;
  TdxRtfToBBCodeConverter = class;
  TdxRtfToBBCodeConverterCustomState = class;
  TdxRtfToBBCodeConverterEnumItemState = class;
  TdxRtfToBBCodeConverterFontTableItemState = class;

  { TdxRtfKeyWords }

  TdxRtfKeyWords = class
  public const
    OpenGroup = '{';
    CloseGroup = '}';
    RtfSignature = '\rtf';
    Space = ' ';
    EntrySeparator = ';';
    QuotationMark = '"';

    AnsiCodePage = '\ansicpg';
    DefaultFontIndex = '\deff';
    StyleTable = '\stylesheet';
    ColorTable = '\colortbl';
    FontTable = '\fonttbl';
    FontCharset = '\fcharset';
    DefaultCharacterProperties = '\*\defchp';
    ResetParagraphProperties = '\pard';
    ViewKind = '\viewkind';
    UnicodeCharacterByteCount = '\uc';

    ColorRed = '\red';
    ColorGreen = '\green';
    ColorBlue = '\blue';

    FontIndex = '\f';
    FontSize = '\fs';
    FontBold = '\b';
    FontItalic = '\i';
    FontStrikeout = '\strike';
    FontDoubleStrikeout = '\striked1';
    FontUnderline = '\ul';
    FontUnderlineNone = '\ulnone';
    FontUnderlineWordsOnly = '\ulw';
    FontName = '\*\fname';

    RunBackgroundColor = '\chcbpat';
    RunBackgroundColor2 = '\highlight';
    RunForegroundColor = '\cf';
    RunSuperScript = '\super';
    RunSubScript = '\sub';
    RunNoSubScript = '\nosupersub';

    Plain = '\plain';

    Field = '\field';
    FieldInstructions = '\*\fldinst';
    FieldResult = '\fldrslt';
    HyperlinkFieldType = 'HYPERLINK';

    NewParagraph = '\par';
    NewLine = '\line';
    Tab = '\tab';
    Unicode = '\u';

    class function IsSpecialSymbol(ACh: Char): Boolean; static;
  end;

  { TdxFormattedTextRtfExporter }

  TdxFormattedTextRtfExporter = class
  strict private
  type
    TRunPropertiesExporter = procedure(ABuilder: TdxRtfBuilder;
      ARun: TdxFormattedTextRun) of object;
  strict private
    FColorTable: TList<TColor>;
    FFontNameTable: TStringList;
    FForceAddLastParagraph: Boolean;
    FRunPropertiesExporters: TdxClassDictionary<TRunPropertiesExporter>;
    FRunStack: TList<TdxFormattedTextRun>;

    FFont: TFont;
    FIsHyperlinkExporting: Boolean;
    FRuns: TdxFormattedTextRuns;
    FDefaultCharacterProperties: TdxFormattedTextCharacterProperties;

    function GetPreviousOpenedRun(ARun: TdxFormattedTextRun; ADepth: Integer)
      : TdxFormattedTextRun; overload;
    function GetPreviousOpenedRun(AStartRunIndex: Integer;
      ARunClass: TdxFormattedTextRunClass;
      ARunAction: TdxFormattedTextRunAction; ADepth: Integer)
      : TdxFormattedTextRun; overload;
    function GetRuns: TdxFormattedTextRuns;
  protected
    procedure ClearTables;

    function GetColorIndex(const AColor: TColor): Integer;
    function GetCurrentCharacterProperties: TdxFormattedTextCharacterProperties;
    function GetDefaultFontIndex: Integer;
    function GetFontIndex(const AName: string): Integer;

    procedure RemoveRunFromStack(ARun: TdxFormattedTextRun);

{$IFDEF DELPHIXE}
    function CalculateFontNameEncoding(const AFontName: string): TEncoding;
{$ENDIF}
    function CanExportRun(ARun: TdxFormattedTextRun): Boolean;
    function ExportContent: string;
    procedure ExportColorTable(ABuilder: TdxRtfBuilder);
    procedure ExportDefaultPropertiesCore(ABuilder: TdxRtfBuilder);
    procedure ExportDefaultProperties(ABuilder: TdxRtfBuilder);
    procedure ExportFontTable(ABuilder: TdxRtfBuilder);

    procedure ExportStyleRun(ABuilder: TdxRtfBuilder; ARun: TdxFormattedTextRun;
      const AKeyword: string);

    procedure ExportBackgroundColorRun(ABuilder: TdxRtfBuilder;
      ARun: TdxFormattedTextRun);
    procedure ExportBoldRun(ABuilder: TdxRtfBuilder; ARun: TdxFormattedTextRun);
    procedure ExportFontNameRun(ABuilder: TdxRtfBuilder;
      ARun: TdxFormattedTextRun);
    procedure ExportFontSizeRun(ABuilder: TdxRtfBuilder;
      ARun: TdxFormattedTextRun);
    procedure ExportClosingSubScriptRun(ABuilder: TdxRtfBuilder;
      ARun: TdxFormattedTextRun);
    procedure ExportFontSubScriptRun(ABuilder: TdxRtfBuilder;
      ARun: TdxFormattedTextRun);
    procedure ExportFontSuperScriptRun(ABuilder: TdxRtfBuilder;
      ARun: TdxFormattedTextRun);
    procedure ExportForegroundColorRun(ABuilder: TdxRtfBuilder;
      ARun: TdxFormattedTextRun);
    procedure ExportItalicRun(ABuilder: TdxRtfBuilder;
      ARun: TdxFormattedTextRun);
    procedure ExportLinkRun(ABuilder: TdxRtfBuilder; ARun: TdxFormattedTextRun);
    procedure ExportRunProperties(ABuilder: TdxRtfBuilder;
      ARun: TdxFormattedTextRun);
    procedure ExportRunPropertiesCore(ABuilder: TdxRtfBuilder;
      ARun: TdxFormattedTextRun);
    procedure ExportRunText(ABuilder: TdxRtfBuilder; ARun: TdxFormattedTextRun);
    procedure ExportStrikeoutRun(ABuilder: TdxRtfBuilder;
      ARun: TdxFormattedTextRun);
    procedure ExportUnderlineRun(ABuilder: TdxRtfBuilder;
      ARun: TdxFormattedTextRun);

    property ForceAddLastParagraph: Boolean read FForceAddLastParagraph
      write FForceAddLastParagraph;
  public
    constructor Create(AFont: TFont; ARuns: TdxFormattedTextRuns);
    destructor Destroy; override;
    procedure AfterConstruction; override;
    function ExportAsString: string;

    property Runs: TdxFormattedTextRuns read GetRuns;
  end;

  { TdxRtfBuilder }

  TdxRtfBuilder = class
  private
    FSpecialMarks: TDictionary<Char, string>;
    FExporter: TdxFormattedTextRtfExporter;
    FStringBuilder: TStringBuilder;
    FIsPreviousWriteCommand: Boolean;

    procedure Initialize;
    procedure Finalize;

    function GetText: string;
    function GetUnicodeCompatibleString(ACh: Char;
      ACheckSpecialMarks: Boolean = True): string;
  public
    constructor Create(AExporter: TdxFormattedTextRtfExporter);
    destructor Destroy; override;

    procedure AppendContent(const AText: string);
    procedure CloseGroup;
    procedure OpenGroup;

    procedure WriteChar(ACh: Char; ACheckSpecialMarks: Boolean = True);
    procedure WriteCommand(const ACommand: string); overload;
    procedure WriteCommand(const ACommand: string; AParam: Integer); overload;
    procedure WriteCommand(const ACommand: string;
      const AParam: string); overload;
    procedure WriteText(const AText: string;
      ACheckSpecialMarks: Boolean = True);

    procedure ExportRun(ARun: TdxFormattedTextRun);
    property Text: string read GetText;
  end;

  { TdxRtfToBBCodeCharacterDecoder }

  TdxRtfToBBCodeCharacterDecoder = class
  strict private
    FIsHexChars: Boolean;
    FBytes: TBytes;
    FCapacity: Integer;
    FCount: Integer;
    FState: TdxRtfToBBCodeConverterCustomState;
    procedure AddByte(Value: Byte);
    procedure Clear;
  protected
    function GetEncoding: TEncoding; virtual;

    property Bytes: TBytes read FBytes;
    property Count: Integer read FCount;
    property IsHexChars: Boolean read FIsHexChars;
    property State: TdxRtfToBBCodeConverterCustomState read FState;
  public
    constructor Create(AState: TdxRtfToBBCodeConverterCustomState);
    destructor Destroy; override;
    function ToString: string; override;

    class function ContainsBBCode(const AText: string): Boolean; static;

    procedure Flush; virtual;
    procedure ProcessHexChar(C: Char); virtual;
    procedure ProcessChar(C: Char);
  end;

  { TdxRtfToBBCodeConverterCustomState }

  TdxRtfToBBCodeTranslatorDelegate = reference to procedure(AParam: Integer;
    AHasParam: Boolean);
  TdxRtfToBBCodeTranslators = TDictionary<string,
    TdxRtfToBBCodeTranslatorDelegate>;
  TdxRtfToBBCodeNextStateDelegate = reference to function(AParam: Integer;
    AHasParam: Boolean): TdxRtfToBBCodeConverterCustomState;
  TdxRtfToBBCodeNextStates = TDictionary<string,
    TdxRtfToBBCodeNextStateDelegate>;

  TdxRtfToBBCodeConverterCustomState = class
  strict private
    FConverter: TdxRtfToBBCodeConverter;
    FDecoder: TdxRtfToBBCodeCharacterDecoder;
    FTranslators: TdxRtfToBBCodeTranslators;
    FNextStates: TdxRtfToBBCodeNextStates;
  protected
    function CreateDecoder: TdxRtfToBBCodeCharacterDecoder; virtual;
    function CreateNextStates: TdxRtfToBBCodeNextStates; virtual;
    function CreateTranslators: TdxRtfToBBCodeTranslators; virtual;
    function GetNextStateByKeyword(const AKeyword: string; AParam: Integer;
      AHasParam: Boolean): TdxRtfToBBCodeConverterCustomState;
    function GetNextState(var APos: PChar)
      : TdxRtfToBBCodeConverterCustomState; virtual;

    property Decoder: TdxRtfToBBCodeCharacterDecoder read FDecoder;
  public
    constructor Create(AConverter: TdxRtfToBBCodeConverter); virtual;
    destructor Destroy; override;

    procedure Apply; virtual;
    procedure FlushDecoder; virtual;
    procedure ProcessChar(C: Char; AIsHex: Boolean); virtual;
    procedure ProcessKeyword(const AKeyword: string; AParam: Integer;
      AHasParam: Boolean);

    property Converter: TdxRtfToBBCodeConverter read FConverter;
  end;

  TdxRtfToBBCodeConverterStateClass = class of
    TdxRtfToBBCodeConverterCustomState;

  { TdxRtfToBBCodeConverterSkipState }

  TdxRtfToBBCodeConverterSkipState = class(TdxRtfToBBCodeConverterCustomState)
  protected
    function GetNextState(var APos: PChar)
      : TdxRtfToBBCodeConverterCustomState; override;
  public
    procedure FlushDecoder; override;
    procedure ProcessChar(C: Char; AIsHex: Boolean); override;
  end;

  { TdxRtfToBBCodeConverterDefaultState }

  TdxRtfToBBCodeConverterDefaultState = class
    (TdxRtfToBBCodeConverterCustomState)
  strict private
    procedure BackgroundColorHandler(AParam: Integer; AHasParam: Boolean);
    procedure DefaultFontIndexHandler(AParam: Integer; AHasParam: Boolean);
    procedure FontStyleHandler(ATag: TdxBBCode; AParam: Integer;
      AHasParam: Boolean);
    procedure FontBoldHandler(AParam: Integer; AHasParam: Boolean);
    procedure FontItalicHandler(AParam: Integer; AHasParam: Boolean);
    procedure FontStrikeoutHandler(AParam: Integer; AHasParam: Boolean);
    procedure FontUnderlineHandler(AParam: Integer; AHasParam: Boolean);
    procedure FontUnderlineNoneHandler(AParam: Integer; AHasParam: Boolean);
    procedure FontSizeHandler(AParam: Integer; AHasParam: Boolean);
    procedure FontIndexHandler(AParam: Integer; AHasParam: Boolean);
    procedure ForegroundColorHandler(AParam: Integer; AHasParam: Boolean);
    procedure NewLineHandler(AParam: Integer; AHasParam: Boolean);
    procedure NoSupersubHandler(AParam: Integer; AHasParam: Boolean);
    procedure SubscriptHandler(AParam: Integer; AHasParam: Boolean);
    procedure SuperscriptHandler(AParam: Integer; AHasParam: Boolean);
    procedure UnicodeHandler(AParam: Integer; AHasParam: Boolean);
    procedure UnicodeCharacterByteCountHandler(AParam: Integer;
      AHasParam: Boolean);
  protected
    function CreateTranslators: TdxRtfToBBCodeTranslators; override;
  end;

  { TdxRtfToBBCodeConverterFieldState }

  TdxRtfToBBCodeConverterFieldState = class(TdxRtfToBBCodeConverterCustomState)
  strict private
    FHasHyperlink: Boolean;
    function FieldInstructionsHandler(AParam: Integer; AHasParam: Boolean)
      : TdxRtfToBBCodeConverterCustomState;
    function FieldResultHandler(AParam: Integer; AHasParam: Boolean)
      : TdxRtfToBBCodeConverterCustomState;
  protected
    function CreateNextStates
      : TDictionary<string, TdxRtfToBBCodeNextStateDelegate>; override;
    property HasHyperlink: Boolean read FHasHyperlink write FHasHyperlink;
  public
    procedure FlushDecoder; override;
    procedure ProcessChar(C: Char; AIsHex: Boolean); override;
  end;

  { TdxRtfToBBCodeConverterFieldChildState }

  TdxRtfToBBCodeConverterFieldChildState = class
    (TdxRtfToBBCodeConverterCustomState)
  strict private
    FParent: TdxRtfToBBCodeConverterFieldState;
  protected
    property Parent: TdxRtfToBBCodeConverterFieldState read FParent;
  public
    constructor Create(AConverter: TdxRtfToBBCodeConverter;
      AParent: TdxRtfToBBCodeConverterFieldState); reintroduce;
  end;

  { TdxRtfToBBCodeConverterFieldInstructionsState }

  TdxRtfToBBCodeConverterFieldInstructionsState = class
    (TdxRtfToBBCodeConverterFieldChildState)
  strict private
    FFieldType: string;
    FInstructions: string;
  protected
    function GetNextState(var APos: PWideChar)
      : TdxRtfToBBCodeConverterCustomState; override;
    property FieldType: string read FFieldType write FFieldType;
    property Instructions: string read FInstructions write FInstructions;
  public
    procedure Apply; override;
    procedure FlushDecoder; override;
    procedure ProcessChar(C: Char; AIsHex: Boolean); override;
  end;

  { TdxRtfToBBCodeConverterFieldInstructionsFieldTypeState }

  TdxRtfToBBCodeConverterFieldInstructionsFieldTypeState = class
    (TdxRtfToBBCodeConverterCustomState)
  strict private
    FIsReadingFieldType: Boolean;
    FParent: TdxRtfToBBCodeConverterFieldInstructionsState;
  public
    constructor Create(AConverter: TdxRtfToBBCodeConverter;
      AParent: TdxRtfToBBCodeConverterFieldInstructionsState); reintroduce;
    procedure FlushDecoder; override;
    procedure ProcessChar(C: Char; AIsHex: Boolean); override;
  end;

  { TdxRtfToBBCodeConverterFieldResultState }

  TdxRtfToBBCodeConverterFieldResultState = class
    (TdxRtfToBBCodeConverterFieldChildState)
  protected
    function GetNextState(var APos: PChar)
      : TdxRtfToBBCodeConverterCustomState; override;
  public
    procedure Apply; override;
  end;

  { TdxRtfToBBCodeConverterEnumState }

  TdxRtfToBBCodeConverterEnumState = class(TdxRtfToBBCodeConverterCustomState)
  public
    procedure ApplyChild(AChild: TdxRtfToBBCodeConverterEnumItemState); virtual;
  end;

  { TdxRtfToBBCodeConverterEnumItemState }

  TdxRtfToBBCodeConverterEnumItemState = class
    (TdxRtfToBBCodeConverterCustomState)
  strict private
    FIndex: Integer;
    FParent: TdxRtfToBBCodeConverterEnumState;
  private
    procedure SetIndex(const Value: Integer);
  public
    constructor Create(AParent: TdxRtfToBBCodeConverterEnumState;
      AIndex: Integer); reintroduce;
    procedure Apply; override;

    property Index: Integer read FIndex write SetIndex;
    property Parent: TdxRtfToBBCodeConverterEnumState read FParent;
  end;

  { TdxRtfToBBCodeConverterFontTableState }

  TdxRtfToBBCodeConverterRtfFontInfo = record
    Charset: Integer;
    Name: string;
    class function Empty: TdxRtfToBBCodeConverterRtfFontInfo; static;
  end;

  TdxRtfToBBCodeConverterFontTableState = class
    (TdxRtfToBBCodeConverterEnumState)
  strict private
    FList: TList<TdxRtfToBBCodeConverterRtfFontInfo>;
    function FontItemHandler(AParam: Integer; AHasParam: Boolean)
      : TdxRtfToBBCodeConverterCustomState;
  protected
    function CreateNextStates: TdxRtfToBBCodeNextStates; override;
  public
    constructor Create(AConverter: TdxRtfToBBCodeConverter); override;
    destructor Destroy; override;

    procedure Apply; override;
    procedure ApplyChild(AChild: TdxRtfToBBCodeConverterEnumItemState);
      override;
    procedure FlushDecoder; override;
  end;

  { TdxRtfToBBCodeCharacterFontTableItemDecoder }

  TdxRtfToBBCodeCharacterFontTableItemDecoder = class
    (TdxRtfToBBCodeCharacterDecoder)
  strict private
    FUseCharset: Boolean;
    function GetState: TdxRtfToBBCodeConverterFontTableItemState; inline;
  protected
    function GetEncoding: TEncoding; override;
    property State: TdxRtfToBBCodeConverterFontTableItemState read GetState;
  public
    procedure Flush; override;
    procedure ProcessHexChar(C: Char); override;
  end;

  { TdxRtfToBBCodeConverterFontNameState }

  TdxRtfToBBCodeConverterFontNameState = class
    (TdxRtfToBBCodeConverterCustomState)
  strict private
    FOwner: TdxRtfToBBCodeConverterFontTableItemState;
  public
    constructor Create(AOwner: TdxRtfToBBCodeConverterFontTableItemState);
      reintroduce;
    procedure Apply; override;
  end;

  { TdxRtfToBBCodeConverterFontTableItemState }

  TdxRtfToBBCodeConverterFontTableItemState = class
    (TdxRtfToBBCodeConverterEnumItemState)
  strict private
    FCharset: Integer;
    FOriginalFontName: string;
    procedure FontCharsetHandler(AParam: Integer; AHasParam: Boolean);
    function FontNameHandler(AParam: Integer; AHasParam: Boolean)
      : TdxRtfToBBCodeConverterCustomState;
  protected
    function CreateDecoder: TdxRtfToBBCodeCharacterDecoder; override;
    function CreateNextStates
      : TDictionary<string, TdxRtfToBBCodeNextStateDelegate>; override;
    function CreateTranslators: TdxRtfToBBCodeTranslators; override;
  public
    procedure AfterConstruction; override;
    procedure FlushDecoder; override;
    procedure ProcessChar(C: Char; AIsHex: Boolean); override;

    property Charset: Integer read FCharset;
    property OriginalFontName: string read FOriginalFontName
      write FOriginalFontName;
  end;

  { TdxRtfToBBCodeConverterColorTableState }

  TdxRtfToBBCodeConverterColorTableState = class
    (TdxRtfToBBCodeConverterEnumState)
  strict private
    FList: TList<TColor>;
  protected
    procedure ColorRedHandler(AParam: Integer; AHasParam: Boolean);
    procedure ColorGreenHandler(AParam: Integer; AHasParam: Boolean);
    procedure ColorBlueHandler(AParam: Integer; AHasParam: Boolean);

    function CreateTranslators: TdxRtfToBBCodeTranslators; override;
  public
    constructor Create(AConverter: TdxRtfToBBCodeConverter); override;
    destructor Destroy; override;

    procedure Apply; override;
    procedure ApplyChild(AChild: TdxRtfToBBCodeConverterEnumItemState);
      override;
    procedure ProcessChar(C: Char; AIsHex: Boolean); override;
    procedure FlushDecoder; override;
  end;

  { TdxRtfToBBCodeConverterColorTableItemState }

  TdxRtfToBBCodeConverterColorTableItemState = class
    (TdxRtfToBBCodeConverterEnumItemState)
  strict private
    FRed: Byte;
    FGreen: Byte;
    FBlue: Byte;
    FHasColor: Boolean;
    function GetColor: TColor;
    function GetParent: TdxRtfToBBCodeConverterColorTableState; inline;
  protected
    procedure ColorRedHandler(AParam: Integer; AHasParam: Boolean);
    procedure ColorGreenHandler(AParam: Integer; AHasParam: Boolean);
    procedure ColorBlueHandler(AParam: Integer; AHasParam: Boolean);

    function CreateTranslators: TdxRtfToBBCodeTranslators; override;
  public
    procedure FlushDecoder; override;
    procedure ProcessChar(C: Char; AIsHex: Boolean); override;

    property Color: TColor read GetColor;
    property HasColor: Boolean read FHasColor;
    property Parent: TdxRtfToBBCodeConverterColorTableState read GetParent;
  end;

  { TdxRtfToBBCodeConverterHeadState }

  TdxRtfToBBCodeConverterHeadState = class(TdxRtfToBBCodeConverterDefaultState)
  strict private
    function FieldHandler(AParam: Integer; AHasParam: Boolean)
      : TdxRtfToBBCodeConverterCustomState;
    function PlainHandler(AParam: Integer; AHasParam: Boolean)
      : TdxRtfToBBCodeConverterCustomState;
  protected
    function CreateNextStates: TdxRtfToBBCodeNextStates; override;
    function GetNextState(var APos: PChar)
      : TdxRtfToBBCodeConverterCustomState; override;

    function CanStartProcessKeyword(const AKeyword: string; AParam: Integer;
      AHasParam: Boolean): Boolean;
  end;

  { TdxRtfToBBCodeConverterPlainState }

  TdxRtfToBBCodeConverterPlainState = class(TdxRtfToBBCodeConverterHeadState);

  { TdxRtfToBBCodeConverterDefaultCharacterPropertiesState }

  TdxRtfToBBCodeConverterDefaultCharacterPropertiesState = class
    (TdxRtfToBBCodeConverterCustomState)
  strict private
    procedure FontStyleHandler(AStyle: TFontStyle; AParam: Integer;
      AHasParam: Boolean);

    procedure FontBoldHandler(AParam: Integer; AHasParam: Boolean);
    procedure FontItalicHandler(AParam: Integer; AHasParam: Boolean);
    procedure FontStrikeoutHandler(AParam: Integer; AHasParam: Boolean);
    procedure FontUnderlineHandler(AParam: Integer; AHasParam: Boolean);
    procedure FontSizeHandler(AParam: Integer; AHasParam: Boolean);
    procedure FontIndexHandler(AParam: Integer; AHasParam: Boolean);
    procedure ForegroundColorHandler(AParam: Integer; AHasParam: Boolean);
  protected
    function CreateTranslators: TdxRtfToBBCodeTranslators; override;
  public
    constructor Create(AConverter: TdxRtfToBBCodeConverter); override;
  end;

  { TdxRtfToBBCodeConverterRtfState }

  TdxRtfToBBCodeConverterRtfState = class(TdxRtfToBBCodeConverterHeadState)
  strict private
    procedure AnsiCodePageHandler(AParam: Integer; AHasParam: Boolean);
    function ColorTableHandler(AParam: Integer; AHasParam: Boolean)
      : TdxRtfToBBCodeConverterCustomState;
    function DefaultCharacterPropertiesHandler(AParam: Integer;
      AHasParam: Boolean): TdxRtfToBBCodeConverterCustomState;
    function FontTableHandler(AParam: Integer; AHasParam: Boolean)
      : TdxRtfToBBCodeConverterCustomState;
  protected
    function CreateNextStates: TdxRtfToBBCodeNextStates; override;
    function CreateTranslators: TdxRtfToBBCodeTranslators; override;
  end;

  { TdxRtfToBBCodeConverter }

  TdxRtfToBBCodeConverter = class
  strict private
    FAnsiCodePage: Cardinal;
    FBuilder: TStringBuilder;
    FColorTable: TList<TColor>;
    FDefaultColorIndex: Integer;
    FDefaultFont: TFont;
    FDefaultFontIndex: Integer;
    FDefaultFontSize: Integer;
    FDefaultFontStyle: TFontStyles;
    FFontCodePage: Cardinal;
    FFontTable: TList<TdxRtfToBBCodeConverterRtfFontInfo>;
    FHasError: Boolean;
    FKeepLastParagraph: Boolean;
    FSkipCharCount: Integer;
    FStates: TObjectStack<TdxRtfToBBCodeConverterCustomState>;
    FTags: TList<TdxBBCode>;
    FUnicodeCharacterByteCount: Integer;
    FWaitingForHexChar: Boolean;
  protected
    procedure ApplyDefaultFontSettings;
    procedure CloseTagsBeforeTag(ATag: TdxBBCode);
    procedure CloseTag(ATag: TdxBBCode);
    function DoOpenTag(ATag: TdxBBCode): Boolean;
    procedure GetDefaultFontSettings;
    procedure OpenTag(ATag: TdxBBCode); overload;
    procedure OpenTag(ATag: TdxBBCode; AParam: Integer); overload;
    procedure OpenTag(ATag: TdxBBCode; const AParam: string); overload;

    procedure Clear;
    procedure Error;

    procedure FlushDecoder;
    function GetCurrentCodePage: Cardinal;
    procedure PopState;
    procedure PushNextState(var APos: PChar);
    procedure PushSkipState;
    procedure PushState(const AClass
      : TdxRtfToBBCodeConverterStateClass); overload;
    procedure PushState(AState: TdxRtfToBBCodeConverterCustomState); overload;
    function PeekState: TdxRtfToBBCodeConverterCustomState;

    function GetRtfKeyword(var APos: PChar; out AKeyword: string;
      out AParam: Integer; out AHasParam: Boolean): Boolean;
    procedure ParseRtfKeyword(var APos: PChar);
    procedure ProcessChar(const C: Char; AIsHex: Boolean = False);
    procedure ProcessHexChar(var APos: PChar);
    procedure ProcessUnicodeChar(const C: Char);
    procedure TranslateKeyword(const AKeyword: string; AParam: Integer;
      AHasParam: Boolean);

    property AnsiCodePage: Cardinal read FAnsiCodePage write FAnsiCodePage;
    property Builder: TStringBuilder read FBuilder;
    property ColorTable: TList<TColor> read FColorTable;
    property FontCodePage: Cardinal read FFontCodePage write FFontCodePage;
    property FontTable: TList<TdxRtfToBBCodeConverterRtfFontInfo>
      read FFontTable;
    property DefaultColorIndex: Integer read FDefaultColorIndex
      write FDefaultColorIndex;
    property DefaultFontIndex: Integer read FDefaultFontIndex
      write FDefaultFontIndex;
    property DefaultFontSize: Integer read FDefaultFontSize
      write FDefaultFontSize;
    property DefaultFontStyle: TFontStyles read FDefaultFontStyle
      write FDefaultFontStyle;
    property KeepLastParagraph: Boolean read FKeepLastParagraph
      write FKeepLastParagraph;
    property UnicodeCharacterByteCount: Integer read FUnicodeCharacterByteCount
      write FUnicodeCharacterByteCount;
    property WaitingForHexChar: Boolean read FWaitingForHexChar
      write FWaitingForHexChar;
  public
    constructor Create(ADefaultFont: TFont);
    destructor Destroy; override;

    function GetBBCodeText(const ARtfText: string): string;
  end;

  { TdxRtfKeyWords }

class function TdxRtfKeyWords.IsSpecialSymbol(ACh: Char): Boolean;
begin
  Result := CharInSet(ACh, [TdxRtfKeyWords.OpenGroup,
    TdxRtfKeyWords.CloseGroup, '\']);
end;

{ TdxFormattedTextRtfExporter }

constructor TdxFormattedTextRtfExporter.Create(AFont: TFont;
  ARuns: TdxFormattedTextRuns);
begin
  inherited Create;
  FFont := AFont;
  FRuns := ARuns;
  FFontNameTable := TStringList.Create;
  FColorTable := TList<TColor>.Create;
  FRunPropertiesExporters := TdxClassDictionary<TRunPropertiesExporter>.Create;
  FDefaultCharacterProperties.Initialize(AFont);
end;

destructor TdxFormattedTextRtfExporter.Destroy;
begin
  FreeAndNil(FColorTable);
  FreeAndNil(FRunPropertiesExporters);
  FreeAndNil(FFontNameTable);
  inherited Destroy;
end;

procedure TdxFormattedTextRtfExporter.AfterConstruction;
begin
  inherited AfterConstruction;
  FRunPropertiesExporters.Add(TdxFormattedTextURLRun, ExportLinkRun);
  FRunPropertiesExporters.Add(TdxFormattedTextBoldRun, ExportBoldRun);
  FRunPropertiesExporters.Add(TdxFormattedTextItalicRun, ExportItalicRun);
  FRunPropertiesExporters.Add(TdxFormattedTextUnderlineRun, ExportUnderlineRun);
  FRunPropertiesExporters.Add(TdxFormattedTextStrikeoutRun, ExportStrikeoutRun);
  FRunPropertiesExporters.Add(TdxFormattedTextBackgroundColorRun,
    ExportBackgroundColorRun);
  FRunPropertiesExporters.Add(TdxFormattedTextSizeRun, ExportFontSizeRun);
  FRunPropertiesExporters.Add(TdxFormattedTextSupRun, ExportFontSuperScriptRun);
  FRunPropertiesExporters.Add(TdxFormattedTextSubRun, ExportFontSubScriptRun);
  FRunPropertiesExporters.Add(TdxFormattedTextFontRun, ExportFontNameRun);
  FRunPropertiesExporters.Add(TdxFormattedTextColorRun,
    ExportForegroundColorRun);
end;

function TdxFormattedTextRtfExporter.ExportAsString: string;
var
  AContent: string;
  ABuilder: TdxRtfBuilder;
begin
  ClearTables;
  AContent := ExportContent;
  ABuilder := TdxRtfBuilder.Create(Self);
  try
    ABuilder.OpenGroup;
    ABuilder.WriteCommand(TdxRtfKeyWords.RtfSignature, 1);
    ABuilder.WriteCommand(TdxRtfKeyWords.AnsiCodePage, GetACP);
    ABuilder.WriteCommand(TdxRtfKeyWords.DefaultFontIndex, GetDefaultFontIndex);
    ExportFontTable(ABuilder);
    ExportColorTable(ABuilder);
    ExportDefaultProperties(ABuilder);
    ABuilder.AppendContent(AContent);
    ABuilder.CloseGroup;
    Result := ABuilder.Text;
  finally
    ABuilder.Free;
  end;
end;

procedure TdxFormattedTextRtfExporter.ExportStyleRun(ABuilder: TdxRtfBuilder;
  ARun: TdxFormattedTextRun; const AKeyword: string);
begin
  if ARun.Action = TdxFormattedTextRunAction.traOpen then
    ABuilder.WriteCommand(AKeyword)
  else
    ABuilder.WriteCommand(AKeyword, 0);
end;

procedure TdxFormattedTextRtfExporter.ExportBackgroundColorRun
  (ABuilder: TdxRtfBuilder; ARun: TdxFormattedTextRun);
var
  AIndex: Integer;
  AColorRun: TdxFormattedTextColorRun absolute ARun;
  APrevRun: TdxFormattedTextRun;
begin
  if (AColorRun.Color = clNone) or (AColorRun.Color = clDefault) or
    (AColorRun.Color = clWindow) then
  begin
    if AColorRun.Action = TdxFormattedTextRunAction.traOpen then
      AIndex := 0
    else
    begin
      APrevRun := GetPreviousOpenedRun(ARun, 0);
      if APrevRun = nil then
        AIndex := 0
      else
        AIndex := GetColorIndex(TdxFormattedTextColorRun(APrevRun).Color);
    end;
  end
  else
    AIndex := GetColorIndex(AColorRun.Color);
  ABuilder.WriteCommand(TdxRtfKeyWords.RunBackgroundColor2, AIndex);
  ABuilder.WriteCommand(TdxRtfKeyWords.RunBackgroundColor, AIndex);
end;

procedure TdxFormattedTextRtfExporter.ExportBoldRun(ABuilder: TdxRtfBuilder;
  ARun: TdxFormattedTextRun);
begin
  if fsBold in FDefaultCharacterProperties.FontStyle then
    Exit;
  ExportStyleRun(ABuilder, ARun, TdxRtfKeyWords.FontBold);
end;

procedure TdxFormattedTextRtfExporter.ClearTables;
begin
  FFontNameTable.Clear;
  FColorTable.Clear;
  FColorTable.Add(0);
  GetColorIndex(FDefaultCharacterProperties.FontColor);
end;

function TdxFormattedTextRtfExporter.GetColorIndex(const AColor
  : TColor): Integer;
var
  C: TColor;
begin
  C := ColorToRGB(AColor) and $00FFFFFF;
  Result := FColorTable.IndexOf(C);
  if Result = -1 then
    Result := FColorTable.Add(C);
end;

function TdxFormattedTextRtfExporter.GetCurrentCharacterProperties
  : TdxFormattedTextCharacterProperties;
begin
  Result := FDefaultCharacterProperties;
end;

function TdxFormattedTextRtfExporter.GetFontIndex(const AName: string): Integer;
begin
  Result := FFontNameTable.IndexOf(AName);
  if Result = -1 then
    Result := FFontNameTable.Add(AName);
end;

procedure TdxFormattedTextRtfExporter.RemoveRunFromStack
  (ARun: TdxFormattedTextRun);
var
  I: Integer;
begin
  for I := FRunStack.Count - 1 downto 0 do
    if FRunStack[I].InheritsFrom(ARun.ClassType) then
    begin
      FRunStack.Delete(I);
      Break;
    end;
end;

function TdxFormattedTextRtfExporter.GetDefaultFontIndex: Integer;
begin
  Result := GetFontIndex(FFont.Name);
end;

{$IFDEF DELPHIXE}

function TdxFormattedTextRtfExporter.CalculateFontNameEncoding(const AFontName
  : string): TEncoding;
var
  I: Integer;
  AEncoding: TEncoding;
  AEncodings: TArray<TEncoding>;
  AFontCache: TdxFontCache;
  ACharset: Cardinal;
begin
  if TdxEncoding.ASCII.CanBeLosslesslyEncoded(AFontName) then
    Exit(TdxEncoding.ASCII);

  if TdxEncoding.ANSI.CanBeLosslesslyEncoded(AFontName) then
    Exit(TdxEncoding.ANSI);

  AFontCache := TdxFontCacheManager.GetFontCache(TdxDocumentLayoutUnit.Document,
    TdxDocumentModelDpi.Dpi);
  ACharset := AFontCache.GetCharsetByFontName(AFontName);
  if ACharset > 0 then
  begin
    AEncoding := TdxEncoding.GetEncoding
      (TdxEncoding.CodePageFromCharset(ACharset));
    if AEncoding.CanBeLosslesslyEncoded(AFontName) then
      Exit(AEncoding);
  end;

  AEncodings := TdxEncoding.Encodings;
  for I := Low(AEncodings) to High(AEncodings) do
  begin
    try
      AEncoding := AEncodings[I];
      if (AEncoding.CodePage <> 65000) and // Utf-7
        (AEncoding.CodePage <> 65001) and // Utf-8
        (AEncoding.CodePage <> 1200) and // Unicode
        (AEncoding.CodePage <> 1201) then // Unicode (BigEndian)
      begin
        if AEncoding.CanBeLosslesslyEncoded(AFontName) then
          Exit(AEncoding);
      end;
    except
    end;
  end;
  Result := nil;
end;
{$ENDIF}

function TdxFormattedTextRtfExporter.CanExportRun
  (ARun: TdxFormattedTextRun): Boolean;

  function CanExportStyleRun: Boolean;
  var
    I: Integer;
    AResult: Integer;
  begin
    AResult := 0;
    for I := 0 to FRuns.IndexOf(ARun) - 1 do
      if FRuns[I] is ARun.ClassType then
        if FRuns[I].Action = TdxFormattedTextRunAction.traOpen then
          Inc(AResult)
        else
          Dec(AResult);
    if ARun.Action = TdxFormattedTextRunAction.traOpen then
      Result := AResult = 0
    else
      Result := AResult = 1;
  end;

begin
  if (ARun is TdxFormattedTextBoldRun) or (ARun is TdxFormattedTextStrikeoutRun)
    or (ARun is TdxFormattedTextItalicRun) or
    (ARun is TdxFormattedTextUnderlineRun) or (ARun is TdxFormattedTextSubRun)
    or (ARun is TdxFormattedTextSupRun) then
    Result := CanExportStyleRun
  else
    Result := True;
end;

function TdxFormattedTextRtfExporter.ExportContent: string;
var
  ABuilder: TdxRtfBuilder;
  ARun: TdxFormattedTextRun;
  I: Integer;
begin
  if Runs.Count = 0 then
    Exit('');
  FRunStack := TList<TdxFormattedTextRun>.Create;
  try
    ABuilder := TdxRtfBuilder.Create(Self);
    try
      ExportDefaultPropertiesCore(ABuilder);
      for I := 0 to Runs.Count - 1 do
      begin
        ARun := Runs[I];
        ExportRunProperties(ABuilder, ARun);
        ExportRunText(ABuilder, ARun);
      end;
      if ForceAddLastParagraph then
        ABuilder.AppendContent(TdxRtfKeyWords.NewParagraph);
      Result := ABuilder.Text;
    finally
      ABuilder.Free;
    end;
  finally
    FreeAndNil(FRunStack);
  end;
end;

procedure TdxFormattedTextRtfExporter.ExportColorTable(ABuilder: TdxRtfBuilder);
var
  I: Integer;
  AColor: TColor;
begin
  ABuilder.OpenGroup;
  ABuilder.WriteCommand(TdxRtfKeyWords.ColorTable);
  ABuilder.WriteChar(TdxRtfKeyWords.EntrySeparator);
  if FColorTable.Count > 1 then
    for I := 1 to FColorTable.Count - 1 do
    begin
      AColor := ColorToRGB(FColorTable[I]);
      ABuilder.WriteCommand(TdxRtfKeyWords.ColorRed, GetRValue(AColor));
      ABuilder.WriteCommand(TdxRtfKeyWords.ColorGreen, GetGValue(AColor));
      ABuilder.WriteCommand(TdxRtfKeyWords.ColorBlue, GetBValue(AColor));
      ABuilder.WriteChar(TdxRtfKeyWords.EntrySeparator);
    end;
  ABuilder.CloseGroup;
end;

procedure TdxFormattedTextRtfExporter.ExportRunText(ABuilder: TdxRtfBuilder;
  ARun: TdxFormattedTextRun);
begin
  ABuilder.ExportRun(ARun);
end;

procedure TdxFormattedTextRtfExporter.ExportStrikeoutRun
  (ABuilder: TdxRtfBuilder; ARun: TdxFormattedTextRun);
begin
  if fsStrikeOut in FDefaultCharacterProperties.FontStyle then
    Exit;
  ExportStyleRun(ABuilder, ARun, TdxRtfKeyWords.FontStrikeout);
end;

procedure TdxFormattedTextRtfExporter.ExportUnderlineRun
  (ABuilder: TdxRtfBuilder; ARun: TdxFormattedTextRun);
begin
  if fsUnderline in FDefaultCharacterProperties.FontStyle then
    Exit;
  ExportStyleRun(ABuilder, ARun, TdxRtfKeyWords.FontUnderline);
end;

procedure TdxFormattedTextRtfExporter.ExportLinkRun(ABuilder: TdxRtfBuilder;
  ARun: TdxFormattedTextRun);
var
  AUrl: string;
begin
  if ARun.Action = traOpen then
  begin
    ABuilder.OpenGroup;
    ABuilder.WriteCommand(TdxRtfKeyWords.Field);
    ABuilder.OpenGroup;
    ABuilder.WriteCommand(TdxRtfKeyWords.FieldInstructions);
    ABuilder.OpenGroup;

    AUrl := TdxFormattedTextURLRun(ARun).Hyperlink;
    if AUrl = '' then
      AUrl := Runs.ExtractURLText(TdxFormattedTextURLRun(ARun));

    ABuilder.WriteCommand(TdxRtfKeyWords.HyperlinkFieldType, AUrl);
    ABuilder.CloseGroup;
    ABuilder.CloseGroup;
    ABuilder.OpenGroup;
    ABuilder.WriteCommand(TdxRtfKeyWords.FieldResult);
    ABuilder.OpenGroup;
    FIsHyperlinkExporting := True;
  end
  else
  begin
    ABuilder.CloseGroup;
    ABuilder.CloseGroup;
    ABuilder.CloseGroup;
    FIsHyperlinkExporting := False;
  end;
end;

procedure TdxFormattedTextRtfExporter.ExportRunProperties
  (ABuilder: TdxRtfBuilder; ARun: TdxFormattedTextRun);
begin
  if FIsHyperlinkExporting then
  begin
    RemoveRunFromStack(ARun);
    if ARun.Action = TdxFormattedTextRunAction.traOpen then
      FRunStack.Add(ARun);
  end;
  ExportRunPropertiesCore(ABuilder, ARun);
  if not FIsHyperlinkExporting and (FRunStack.Count > 0) then
  begin
    if ARun.Text <> '' then
    begin
      while FRunStack.Count > 0 do
      begin
        ExportRunPropertiesCore(ABuilder, FRunStack.First);
        FRunStack.Delete(0);
      end;
    end
    else
      RemoveRunFromStack(ARun);
  end;
end;

procedure TdxFormattedTextRtfExporter.ExportRunPropertiesCore
  (ABuilder: TdxRtfBuilder; ARun: TdxFormattedTextRun);
var
  APropertiesExporter: TRunPropertiesExporter;
begin
  if FRunPropertiesExporters.TryGetValue(ARun, APropertiesExporter) and
    CanExportRun(ARun) then
    APropertiesExporter(ABuilder, ARun);
end;

procedure TdxFormattedTextRtfExporter.ExportDefaultPropertiesCore
  (ABuilder: TdxRtfBuilder);
var
  AProperties: TdxFormattedTextCharacterProperties;
  AIndex: Integer;
begin
  AProperties := GetCurrentCharacterProperties;
  if AProperties.Bold then
    ABuilder.WriteCommand(TdxRtfKeyWords.FontBold);
  if AProperties.Italic then
    ABuilder.WriteCommand(TdxRtfKeyWords.FontItalic);
  if AProperties.Strikeout then
    ABuilder.WriteCommand(TdxRtfKeyWords.FontStrikeout);
  if AProperties.Underline then
    ABuilder.WriteCommand(TdxRtfKeyWords.FontUnderline);
  case AProperties.CharacterFormattingScript of
    TdxCharacterFormattingScript.Subscript:
      ABuilder.WriteCommand(TdxRtfKeyWords.RunSubScript);
    TdxCharacterFormattingScript.Superscript:
      ABuilder.WriteCommand(TdxRtfKeyWords.RunSuperScript);
  else
  end;
  ABuilder.WriteCommand(TdxRtfKeyWords.FontIndex,
    GetFontIndex(AProperties.FontName));
  ABuilder.WriteCommand(TdxRtfKeyWords.FontSize, Abs(AProperties.FontSize * 2));
  AIndex := GetColorIndex(AProperties.FontColor);
  if AIndex <> 0 then
    ABuilder.WriteCommand(TdxRtfKeyWords.RunForegroundColor, AIndex);
  if (AProperties.BackgroundColor = clNone) or
    (AProperties.BackgroundColor = clDefault) or
    (AProperties.BackgroundColor = clWindow) then
  else
  begin
    AIndex := GetColorIndex(AProperties.BackgroundColor);
    ABuilder.WriteCommand(TdxRtfKeyWords.RunBackgroundColor2, AIndex);
    ABuilder.WriteCommand(TdxRtfKeyWords.RunBackgroundColor, AIndex);
  end;
end;

procedure TdxFormattedTextRtfExporter.ExportDefaultProperties
  (ABuilder: TdxRtfBuilder);
begin
  ABuilder.OpenGroup;
  ABuilder.WriteCommand(TdxRtfKeyWords.DefaultCharacterProperties);
  ExportDefaultPropertiesCore(ABuilder);
  ABuilder.CloseGroup;
end;

procedure TdxFormattedTextRtfExporter.ExportFontNameRun(ABuilder: TdxRtfBuilder;
  ARun: TdxFormattedTextRun);
var
  APrevRun: TdxFormattedTextRun;
begin
  if ARun.Action = TdxFormattedTextRunAction.traOpen then
    ABuilder.WriteCommand(TdxRtfKeyWords.FontIndex,
      GetFontIndex(TdxFormattedTextFontRun(ARun).FontName))
  else
  begin
    APrevRun := GetPreviousOpenedRun(ARun, 0);
    if APrevRun = nil then
      ABuilder.WriteCommand(TdxRtfKeyWords.FontIndex,
        GetFontIndex(FDefaultCharacterProperties.FontName))
    else
      ABuilder.WriteCommand(TdxRtfKeyWords.FontIndex,
        GetFontIndex(TdxFormattedTextFontRun(APrevRun).FontName))
  end;
end;

procedure TdxFormattedTextRtfExporter.ExportFontSubScriptRun
  (ABuilder: TdxRtfBuilder; ARun: TdxFormattedTextRun);
begin
  if ARun.Action = TdxFormattedTextRunAction.traOpen then
    ABuilder.WriteCommand(TdxRtfKeyWords.RunSubScript)
  else
    ExportClosingSubScriptRun(ABuilder, ARun);
end;

procedure TdxFormattedTextRtfExporter.ExportFontSuperScriptRun
  (ABuilder: TdxRtfBuilder; ARun: TdxFormattedTextRun);
begin
  if ARun.Action = TdxFormattedTextRunAction.traOpen then
    ABuilder.WriteCommand(TdxRtfKeyWords.RunSuperScript)
  else
    ExportClosingSubScriptRun(ABuilder, ARun);
end;

procedure TdxFormattedTextRtfExporter.ExportFontSizeRun(ABuilder: TdxRtfBuilder;
  ARun: TdxFormattedTextRun);
var
  ASize: Integer;
  APrevRun: TdxFormattedTextRun;
begin
  if ARun.Action = TdxFormattedTextRunAction.traOpen then
    ASize := TdxFormattedTextSizeRun(ARun).Size
  else
  begin
    APrevRun := GetPreviousOpenedRun(ARun, 0);
    if APrevRun = nil then
      ASize := FDefaultCharacterProperties.FontSize
    else
      ASize := TdxFormattedTextSizeRun(APrevRun).Size;
  end;
  ABuilder.WriteCommand(TdxRtfKeyWords.FontSize, Abs(ASize * 2))
end;

procedure TdxFormattedTextRtfExporter.ExportClosingSubScriptRun
  (ABuilder: TdxRtfBuilder; ARun: TdxFormattedTextRun);
var
  APrevSubRun: TdxFormattedTextRun;
  APrevSupRun: TdxFormattedTextRun;
  AIndex: Integer;
begin
  ABuilder.WriteCommand(TdxRtfKeyWords.RunNoSubScript);

  AIndex := FRuns.IndexOf(ARun);
  if ARun is TdxFormattedTextSubRun then
  begin
    APrevSubRun := GetPreviousOpenedRun(AIndex, TdxFormattedTextSubRun,
      TdxFormattedTextRunAction.traClose, 0);
    APrevSupRun := GetPreviousOpenedRun(AIndex, TdxFormattedTextSupRun,
      TdxFormattedTextRunAction.traOpen, 1);
  end
  else
  begin
    APrevSubRun := GetPreviousOpenedRun(AIndex, TdxFormattedTextSubRun,
      TdxFormattedTextRunAction.traOpen, 1);
    APrevSupRun := GetPreviousOpenedRun(AIndex, TdxFormattedTextSupRun,
      TdxFormattedTextRunAction.traClose, 0);
  end;
  if FRuns.IndexOf(APrevSubRun) > FRuns.IndexOf(APrevSupRun) then
  begin
    if APrevSubRun <> nil then
      ExportFontSubScriptRun(ABuilder, APrevSubRun);
  end
  else
  begin
    if APrevSupRun <> nil then
      ExportFontSuperScriptRun(ABuilder, APrevSupRun);
  end;
end;

procedure TdxFormattedTextRtfExporter.ExportFontTable(ABuilder: TdxRtfBuilder);
var
  AFontName: string;
  I: Integer;
{$IFDEF DELPHIXE}
  ABytes: TArray<Byte>;
  AEncoding: TEncoding;
  AFontCharset: Integer;
  ACount: Integer;
  J: Integer;
{$ENDIF}
begin
  ABuilder.OpenGroup;
  ABuilder.WriteCommand(TdxRtfKeyWords.FontTable);

  for I := 0 to FFontNameTable.Count - 1 do
  begin
    AFontName := FFontNameTable[I];
    ABuilder.OpenGroup;
    ABuilder.WriteCommand(TdxRtfKeyWords.FontIndex, I);
{$IFDEF DELPHIXE}
    if TdxEncoding.ASCII.CanBeLosslesslyEncoded(AFontName) then
      ABuilder.WriteText(AFontName)
    else
    begin
      AEncoding := CalculateFontNameEncoding(AFontName);
      if AEncoding <> nil then
      begin
        AFontCharset := TdxEncoding.CharsetFromCodePage(AEncoding.CodePage);
        ABuilder.WriteCommand(TdxRtfKeyWords.FontCharset, AFontCharset);
      end
      else
      begin
        ABuilder.WriteCommand(TdxRtfKeyWords.FontCharset, 0);
        AEncoding := TdxEncoding.Unicode;
      end;
      ABytes := AEncoding.GetBytes(AFontName);
      ACount := Length(ABytes);
      for J := 0 to ACount - 1 do
        ABuilder.WriteText('\'#$27 + TdxStringHelper.ToHex(ABytes[J]), False);
    end;
{$ELSE}
    ABuilder.WriteText(AFontName);
{$ENDIF}
    ABuilder.WriteChar(TdxRtfKeyWords.EntrySeparator);
    ABuilder.CloseGroup;
  end;
  ABuilder.CloseGroup;
end;

procedure TdxFormattedTextRtfExporter.ExportForegroundColorRun
  (ABuilder: TdxRtfBuilder; ARun: TdxFormattedTextRun);
var
  AColor: TColor;
  APrevRun: TdxFormattedTextRun;
begin
  if ARun.Action = TdxFormattedTextRunAction.traOpen then
    AColor := TdxFormattedTextColorRun(ARun).Color
  else
  begin
    APrevRun := GetPreviousOpenedRun(ARun, 0);
    if APrevRun = nil then
      AColor := FDefaultCharacterProperties.FontColor
    else
      AColor := TdxFormattedTextColorRun(APrevRun).Color;
  end;
  ABuilder.WriteCommand(TdxRtfKeyWords.RunForegroundColor,
    GetColorIndex(AColor));
end;

procedure TdxFormattedTextRtfExporter.ExportItalicRun(ABuilder: TdxRtfBuilder;
  ARun: TdxFormattedTextRun);
begin
  if fsItalic in FDefaultCharacterProperties.FontStyle then
    Exit;
  ExportStyleRun(ABuilder, ARun, TdxRtfKeyWords.FontItalic);
end;

function TdxFormattedTextRtfExporter.GetPreviousOpenedRun
  (ARun: TdxFormattedTextRun; ADepth: Integer): TdxFormattedTextRun;
begin
  Result := GetPreviousOpenedRun(FRuns.IndexOf(ARun),
    TdxFormattedTextRunClass(ARun.ClassType), ARun.Action, ADepth);
end;

function TdxFormattedTextRtfExporter.GetPreviousOpenedRun(AStartRunIndex
  : Integer; ARunClass: TdxFormattedTextRunClass;
  ARunAction: TdxFormattedTextRunAction; ADepth: Integer): TdxFormattedTextRun;
var
  I: Integer;
begin
  Result := nil;
  for I := AStartRunIndex - 1 downto 0 do
  begin
    if FRuns[I].ClassType = ARunClass then
    begin
      if ARunAction = TdxFormattedTextRunAction.traClose then
        Inc(ADepth)
      else
        Dec(ADepth);
      if (ARunAction <> FRuns[I].Action) or
        (ARunAction = TdxFormattedTextRunAction.traClose) or (ADepth <> 0) then
        Result := GetPreviousOpenedRun(I, ARunClass, FRuns[I].Action, ADepth)
      else
        Result := FRuns[I];
      Break;
    end;
  end;
end;

function TdxFormattedTextRtfExporter.GetRuns: TdxFormattedTextRuns;
begin
  Result := FRuns;
end;

{ TdxRtfBuilder }

procedure TdxRtfBuilder.Initialize;
begin
  FSpecialMarks := TDictionary<Char, string>.Create;
  FSpecialMarks.Add(TdxCharacters.EmSpace, '\u8195\''3f');
  FSpecialMarks.Add(TdxCharacters.EnSpace, '\u8194\''3f');
  FSpecialMarks.Add(TdxCharacters.Hyphen, EmptyStr);
  FSpecialMarks.Add(TdxCharacters.LineBreak, TdxRtfKeyWords.NewLine + ' ');
  FSpecialMarks.Add(TdxCharacters.PageBreak, '\page ');
  FSpecialMarks.Add(TdxCharacters.ColumnBreak, '\column ');
  FSpecialMarks.Add(TdxCharacters.NonBreakingSpace, '\~');
  FSpecialMarks.Add(TdxCharacters.QmSpace, '\u8197\''3f');
  FSpecialMarks.Add(TdxCharacters.TabMark, '\tab ');
  FSpecialMarks.Add(TdxCharacters.ParagraphMark, TdxRtfKeyWords.NewLine + ' ');
  FSpecialMarks.Add(#$0A, '');
end;

procedure TdxRtfBuilder.Finalize;
begin
  FreeAndNil(FSpecialMarks);
end;

constructor TdxRtfBuilder.Create(AExporter: TdxFormattedTextRtfExporter);
begin
  inherited Create;
  FExporter := AExporter;
  FStringBuilder := TStringBuilder.Create;
  Initialize;
end;

destructor TdxRtfBuilder.Destroy;
begin
  Finalize;
  FreeAndNil(FStringBuilder);
  inherited Destroy;
end;

procedure TdxRtfBuilder.AppendContent(const AText: string);
begin
  WriteCommand(TdxRtfKeyWords.ViewKind, 4);
  WriteCommand(TdxRtfKeyWords.UnicodeCharacterByteCount, 1);
  WriteCommand(TdxRtfKeyWords.ResetParagraphProperties);
  if (Length(AText) > 0) and not dxCharInSet(AText[1],
    ['\', TdxRtfKeyWords.OpenGroup]) then
    FStringBuilder.Append(TdxRtfKeyWords.Space);
  FStringBuilder.Append(AText);
end;

procedure TdxRtfBuilder.CloseGroup;
begin
  FStringBuilder.Append(TdxRtfKeyWords.CloseGroup);
  FIsPreviousWriteCommand := False;
end;

procedure TdxRtfBuilder.OpenGroup;
begin
  FStringBuilder.Append(TdxRtfKeyWords.OpenGroup);
  FIsPreviousWriteCommand := False;
end;

procedure TdxRtfBuilder.WriteChar(ACh: Char;
  ACheckSpecialMarks: Boolean = True);
var
  AStr: string;
begin
  AStr := GetUnicodeCompatibleString(ACh, ACheckSpecialMarks);
  if FIsPreviousWriteCommand and (Length(AStr) > 0) then
    FStringBuilder.Append(TdxRtfKeyWords.Space);
  FStringBuilder.Append(AStr);
  FIsPreviousWriteCommand := False;
end;

procedure TdxRtfBuilder.WriteCommand(const ACommand: string);
begin
  FStringBuilder.Append(ACommand);
  FIsPreviousWriteCommand := True;
end;

procedure TdxRtfBuilder.WriteCommand(const ACommand: string; AParam: Integer);
begin
  FStringBuilder.Append(ACommand);
  FStringBuilder.Append(AParam);
  FIsPreviousWriteCommand := True;
end;

procedure TdxRtfBuilder.WriteCommand(const ACommand: string;
  const AParam: string);
begin
  FStringBuilder.Append(ACommand);
  FStringBuilder.Append(TdxRtfKeyWords.Space);
  FStringBuilder.Append(TdxRtfKeyWords.QuotationMark);
  FStringBuilder.Append(AParam);
  FStringBuilder.Append(TdxRtfKeyWords.QuotationMark);
  FIsPreviousWriteCommand := True;
end;

procedure TdxRtfBuilder.WriteText(const AText: string;
  ACheckSpecialMarks: Boolean = True);
var
  I: Integer;
  ACh: Char;
  ASpecialMark: string;
begin
  if AText = '' then
    Exit;
  if FIsPreviousWriteCommand then
  begin
    FStringBuilder.Append(TdxRtfKeyWords.Space);
    FIsPreviousWriteCommand := False;
  end;
  for I := 1 to Length(AText) do
  begin
    ACh := AText[I];
    if ACheckSpecialMarks and FSpecialMarks.TryGetValue(ACh, ASpecialMark) then
      FStringBuilder.Append(ASpecialMark)
    else
      WriteChar(ACh, ACheckSpecialMarks);
  end;
end;

procedure TdxRtfBuilder.ExportRun(ARun: TdxFormattedTextRun);
begin
  WriteText(ARun.Text);
end;

function TdxRtfBuilder.GetText: string;
begin
  Result := FStringBuilder.ToString;
end;

function TdxRtfBuilder.GetUnicodeCompatibleString(ACh: Char;
  ACheckSpecialMarks: Boolean = True): string;
var
  ACode: SmallInt;
  ABuilder: TStringBuilder;
  ABytes: TBytes;
begin
  ABuilder := TStringBuilder.Create;
  try
    ACode := SmallInt(ACh);
    if (ACode >= 0) and (ACode <= 127) then
    begin
      if ACheckSpecialMarks and TdxRtfKeyWords.IsSpecialSymbol(ACh) then
        ABuilder.Append('\');
      ABuilder.Append(ACh);
    end
    else
    begin
      ABytes := TEncoding.Unicode.GetBytes(ACh);
      if Length(ABytes) > 1 then
        ABuilder.AppendFormat('\u%d?', [ACode])
      else
        ABuilder.AppendFormat('\u%d\''%s',
          [ACode, TdxStringHelper.ToHex(ABytes[0])]);
    end;
    Result := ABuilder.ToString;
  finally
    ABuilder.Free;
  end;
end;

{ TdxRtfToBBCodeCharacterDecoder }

constructor TdxRtfToBBCodeCharacterDecoder.Create
  (AState: TdxRtfToBBCodeConverterCustomState);
begin
  inherited Create;
  FState := AState;
  FCapacity := 4096;
  SetLength(FBytes, FCapacity);
end;

destructor TdxRtfToBBCodeCharacterDecoder.Destroy;
begin
  FCapacity := 0;
  SetLength(FBytes, 0);
  inherited Destroy;
end;

function TdxRtfToBBCodeCharacterDecoder.ToString: string;
var
  AEncoding: TEncoding;
begin
  if FCount = 0 then
    Exit('');
  AEncoding := GetEncoding;
  Result := AEncoding.GetString(FBytes, 0, FCount);
end;

class function TdxRtfToBBCodeCharacterDecoder.ContainsBBCode
  (const AText: string): Boolean;
const
  APatterns: array [0 .. 23] of string = ('[B]', '[\B]', '[I]', '[\I]', '[U]',
    '[\U]', '[S]', '[\S]', '[SUP]', '[\SUP]', '[SUB]', '[\SUB]', '[NOPARSE]',
    '[\NOPARSE]', '[URL', '[\URL]', '[COLOR', '[\COLOR]', '[BACKCOLOR',
    '[\BACKCOLOR]', '[SIZE', '[\SIZE]', '[FONT', '[\FONT]');
var
  I: Integer;
  S: string;
begin
  S := UpperCase(AText);
  for I := 0 to Length(APatterns) - 1 do
    if Pos(APatterns[I], S) > 0 then
      Exit(True);
  Result := False;
end;

procedure TdxRtfToBBCodeCharacterDecoder.Flush;
var
  APos: Integer;
  ABuilder: TStringBuilder;
begin
  ABuilder := TStringBuilder.Create;
  try
    ABuilder.Append(ToString);
    if ContainsBBCode(ABuilder.ToString) then
    begin
      ABuilder.Insert(0, '[NOPARSE]');
      APos := ABuilder.Length - 1;
      while dxCharInSet(ABuilder[APos], dxBreakLineCharacters) do
        Dec(APos);
      ABuilder.Insert(APos + 1, '[/NOPARSE]')
    end;
    FState.Converter.Builder.Append(ABuilder);
  finally
    ABuilder.Free;
  end;
  Clear;
end;

function TdxRtfToBBCodeCharacterDecoder.GetEncoding: TEncoding;
begin
  if FIsHexChars then
    Result := TdxEncoding.GetEncoding(FState.Converter.GetCurrentCodePage)
  else
    Result := TdxEncoding.ANSI;
end;

procedure TdxRtfToBBCodeCharacterDecoder.ProcessHexChar(C: Char);
begin
  if (FCount = FCapacity) or not FIsHexChars then
    Flush;
  AddByte(Byte(C));
  FIsHexChars := True;
end;

procedure TdxRtfToBBCodeCharacterDecoder.ProcessChar(C: Char);
begin
  if (FCount = FCapacity) or FIsHexChars then
    Flush;
  AddByte(Byte(C));
  FIsHexChars := False;
end;

procedure TdxRtfToBBCodeCharacterDecoder.AddByte(Value: Byte);
begin
  if FCount >= FCapacity then
  begin
    Inc(FCapacity, 4096);
    SetLength(FBytes, FCapacity);
  end;
  FBytes[FCount] := Value;
  Inc(FCount);
end;

procedure TdxRtfToBBCodeCharacterDecoder.Clear;
begin
  FCount := 0;
  SetLength(FBytes, FCapacity);
end;

{ TdxRtfToBBCodeConverterCustomState }

constructor TdxRtfToBBCodeConverterCustomState.Create
  (AConverter: TdxRtfToBBCodeConverter);
begin
  inherited Create;
  FConverter := AConverter;
  FTranslators := CreateTranslators;
  FNextStates := CreateNextStates;
  FDecoder := CreateDecoder;
end;

destructor TdxRtfToBBCodeConverterCustomState.Destroy;
begin
  FreeAndNil(FDecoder);
  FreeAndNil(FNextStates);
  FreeAndNil(FTranslators);
  inherited Destroy;
end;

function TdxRtfToBBCodeConverterCustomState.CreateDecoder
  : TdxRtfToBBCodeCharacterDecoder;
begin
  Result := TdxRtfToBBCodeCharacterDecoder.Create(Self);
end;

function TdxRtfToBBCodeConverterCustomState.CreateNextStates
  : TdxRtfToBBCodeNextStates;
begin
  Result := TdxRtfToBBCodeNextStates.Create;
end;

function TdxRtfToBBCodeConverterCustomState.CreateTranslators
  : TdxRtfToBBCodeTranslators;
begin
  Result := TdxRtfToBBCodeTranslators.Create;
end;

function TdxRtfToBBCodeConverterCustomState.GetNextStateByKeyword
  (const AKeyword: string; AParam: Integer; AHasParam: Boolean)
  : TdxRtfToBBCodeConverterCustomState;
var
  ADelegate: TdxRtfToBBCodeNextStateDelegate;
begin
  if FNextStates.TryGetValue(LowerCase(AKeyword), ADelegate) then
    Result := ADelegate(AParam, AHasParam)
  else if FTranslators.ContainsKey(LowerCase(AKeyword)) then
  begin
    Result := TdxRtfToBBCodeConverterHeadState.Create(Converter);
    if TdxRtfToBBCodeConverterHeadState(Result).CanStartProcessKeyword(AKeyword,
      AParam, AHasParam) then
      Result.ProcessKeyword(AKeyword, AParam, AHasParam);
  end
  else
    Result := TdxRtfToBBCodeConverterSkipState.Create(FConverter);
end;

function TdxRtfToBBCodeConverterCustomState.GetNextState(var APos: PChar)
  : TdxRtfToBBCodeConverterCustomState;
var
  AKeyword: string;
  AParam: Integer;
  AHasParam: Boolean;
begin
  if Converter.GetRtfKeyword(APos, AKeyword, AParam, AHasParam) then
    Result := GetNextStateByKeyword(AKeyword, AParam, AHasParam)
  else
    Result := nil;
end;

procedure TdxRtfToBBCodeConverterCustomState.FlushDecoder;
begin
  FDecoder.Flush;
end;

procedure TdxRtfToBBCodeConverterCustomState.ProcessChar(C: Char;
  AIsHex: Boolean);
begin
  if AIsHex then
    FDecoder.ProcessHexChar(C)
  else
    FDecoder.ProcessChar(C);
end;

procedure TdxRtfToBBCodeConverterCustomState.ProcessKeyword(const AKeyword
  : string; AParam: Integer; AHasParam: Boolean);
var
  ATranslator: TdxRtfToBBCodeTranslatorDelegate;
begin
  if FTranslators.TryGetValue(LowerCase(AKeyword), ATranslator) then
    ATranslator(AParam, AHasParam);
end;

procedure TdxRtfToBBCodeConverterCustomState.Apply;
begin
  FlushDecoder;
end;

{ TdxRtfToBBCodeConverterSkipState }

procedure TdxRtfToBBCodeConverterSkipState.FlushDecoder;
begin
  // do nothing
end;

procedure TdxRtfToBBCodeConverterSkipState.ProcessChar(C: Char;
  AIsHex: Boolean);
begin
  // do nothing
end;

function TdxRtfToBBCodeConverterSkipState.GetNextState(var APos: PChar)
  : TdxRtfToBBCodeConverterCustomState;
begin
  Result := TdxRtfToBBCodeConverterSkipState.Create(Converter);
end;

{ TdxRtfToBBCodeConverterDefaultState }

function TdxRtfToBBCodeConverterDefaultState.CreateTranslators
  : TdxRtfToBBCodeTranslators;
begin
  Result := inherited CreateTranslators;
  Result.Add(TdxRtfKeyWords.FontBold, FontBoldHandler);
  Result.Add(TdxRtfKeyWords.FontItalic, FontItalicHandler);
  Result.Add(TdxRtfKeyWords.FontUnderline, FontUnderlineHandler);
  Result.Add(TdxRtfKeyWords.FontUnderlineNone, FontUnderlineNoneHandler);
  Result.Add(TdxRtfKeyWords.FontStrikeout, FontStrikeoutHandler);
  Result.Add(TdxRtfKeyWords.FontSize, FontSizeHandler);
  Result.Add(TdxRtfKeyWords.FontIndex, FontIndexHandler);
  Result.Add(TdxRtfKeyWords.RunForegroundColor, ForegroundColorHandler);
  Result.Add(TdxRtfKeyWords.RunBackgroundColor2, BackgroundColorHandler);
  Result.Add(TdxRtfKeyWords.DefaultFontIndex, DefaultFontIndexHandler);
  Result.Add(TdxRtfKeyWords.RunSuperScript, SuperscriptHandler);
  Result.Add(TdxRtfKeyWords.RunSubScript, SubscriptHandler);
  Result.Add(TdxRtfKeyWords.RunNoSubScript, NoSupersubHandler);
  Result.Add(TdxRtfKeyWords.NewParagraph, NewLineHandler);
  Result.Add(TdxRtfKeyWords.NewLine, NewLineHandler);
  Result.Add(TdxRtfKeyWords.Unicode, UnicodeHandler);
  Result.Add(TdxRtfKeyWords.UnicodeCharacterByteCount,
    UnicodeCharacterByteCountHandler);
end;

procedure TdxRtfToBBCodeConverterDefaultState.BackgroundColorHandler
  (AParam: Integer; AHasParam: Boolean);
var
  AColor: TColor;
begin
  if AHasParam and (Converter.ColorTable.Count > AParam) then
  begin
    AColor := Converter.ColorTable[AParam];
    if AColor <> clNone then
    begin
      Converter.CloseTag(bbcBackgroundColor);
      if AParam <> 0 then
        Converter.OpenTag(bbcBackgroundColor,
          TdxAlphaColors.ToHtml(TdxAlphaColors.FromColor(AColor)));
    end;
  end;
end;

procedure TdxRtfToBBCodeConverterDefaultState.DefaultFontIndexHandler
  (AParam: Integer; AHasParam: Boolean);
begin
  if AHasParam then
    Converter.DefaultFontIndex := AParam;
end;

procedure TdxRtfToBBCodeConverterDefaultState.FontStyleHandler(ATag: TdxBBCode;
  AParam: Integer; AHasParam: Boolean);
begin
  if AHasParam and (AParam = 0) then
    Converter.CloseTag(ATag)
  else
    Converter.OpenTag(ATag);
end;

procedure TdxRtfToBBCodeConverterDefaultState.FontBoldHandler(AParam: Integer;
  AHasParam: Boolean);
begin
  if not(fsBold in Converter.DefaultFontStyle) then
    FontStyleHandler(bbcBold, AParam, AHasParam);
end;

procedure TdxRtfToBBCodeConverterDefaultState.FontItalicHandler(AParam: Integer;
  AHasParam: Boolean);
begin
  if not(fsItalic in Converter.DefaultFontStyle) then
    FontStyleHandler(bbcItalic, AParam, AHasParam);
end;

procedure TdxRtfToBBCodeConverterDefaultState.FontStrikeoutHandler
  (AParam: Integer; AHasParam: Boolean);
begin
  if not(fsStrikeOut in Converter.DefaultFontStyle) then
    FontStyleHandler(bbcStrikeout, AParam, AHasParam);
end;

procedure TdxRtfToBBCodeConverterDefaultState.FontUnderlineHandler
  (AParam: Integer; AHasParam: Boolean);
begin
  if not(fsUnderline in Converter.DefaultFontStyle) then
    FontStyleHandler(bbcUnderline, AParam, AHasParam);
end;

procedure TdxRtfToBBCodeConverterDefaultState.FontUnderlineNoneHandler
  (AParam: Integer; AHasParam: Boolean);
begin
  FontStyleHandler(bbcUnderline, 0, True);
end;

procedure TdxRtfToBBCodeConverterDefaultState.FontSizeHandler(AParam: Integer;
  AHasParam: Boolean);
begin
  AParam := AParam div 2;
  if AHasParam and (AParam > 0) then
  begin
    Converter.CloseTag(bbcSize);
    if AParam <> Converter.DefaultFontSize then
      Converter.OpenTag(bbcSize, AParam);
  end;
end;

procedure TdxRtfToBBCodeConverterDefaultState.FontIndexHandler(AParam: Integer;
  AHasParam: Boolean);
begin
  if AHasParam and (Converter.FontTable.Count > AParam) then
  begin
    Converter.CloseTag(bbcFont);
    if Converter.DefaultFontIndex <> AParam then
      if (Converter.DefaultFontIndex = -1) or
        (CompareText(Converter.FontTable[AParam].Name,
        Converter.FontTable[Converter.DefaultFontIndex].Name) <> 0) then
        Converter.OpenTag(bbcFont, Converter.FontTable[AParam].Name);
    if Converter.FontTable[AParam].Charset = -1 then
      Converter.FontCodePage := 0
    else
      Converter.FontCodePage := TdxEncoding.CodePageFromCharset
        (Converter.FontTable[AParam].Charset);
  end;
end;

procedure TdxRtfToBBCodeConverterDefaultState.ForegroundColorHandler
  (AParam: Integer; AHasParam: Boolean);
var
  AColor: TColor;
begin
  if AHasParam and (Converter.ColorTable.Count > AParam) then
  begin
    AColor := Converter.ColorTable[AParam];
    if AColor <> clNone then
    begin
      Converter.CloseTag(bbcColor);
      if ((Converter.DefaultColorIndex <> -1) and
        (AParam <> Converter.DefaultColorIndex)) or
        ((Converter.DefaultColorIndex = -1) and (AParam <> 0)) then
        Converter.OpenTag(bbcColor,
          TdxAlphaColors.ToHtml(TdxAlphaColors.FromColor(AColor)));
    end;
  end;
end;

procedure TdxRtfToBBCodeConverterDefaultState.NewLineHandler(AParam: Integer;
  AHasParam: Boolean);
begin
  Decoder.ProcessChar(TdxCharacters.ParagraphMark);
  Decoder.ProcessChar(dxLF);
end;

procedure TdxRtfToBBCodeConverterDefaultState.NoSupersubHandler(AParam: Integer;
  AHasParam: Boolean);
begin
  Converter.CloseTag(TdxBBCode.bbcSup);
  Converter.CloseTag(TdxBBCode.bbcSub);
end;

procedure TdxRtfToBBCodeConverterDefaultState.SubscriptHandler(AParam: Integer;
  AHasParam: Boolean);
begin
  Converter.CloseTag(TdxBBCode.bbcSup);
  Converter.OpenTag(TdxBBCode.bbcSub);
end;

procedure TdxRtfToBBCodeConverterDefaultState.SuperscriptHandler
  (AParam: Integer; AHasParam: Boolean);
begin
  Converter.CloseTag(TdxBBCode.bbcSub);
  Converter.OpenTag(TdxBBCode.bbcSup);
end;

procedure TdxRtfToBBCodeConverterDefaultState.UnicodeHandler(AParam: Integer;
  AHasParam: Boolean);
begin
  if AHasParam then
    Converter.ProcessUnicodeChar(Char(AParam));
end;

procedure TdxRtfToBBCodeConverterDefaultState.UnicodeCharacterByteCountHandler
  (AParam: Integer; AHasParam: Boolean);
begin
  if not AHasParam or (AParam <= 0) then
    AParam := 1;
  Converter.UnicodeCharacterByteCount := AParam;
end;

{ TdxRtfToBBCodeConverterFieldState }

procedure TdxRtfToBBCodeConverterFieldState.FlushDecoder;
begin
  // do nothing
end;

procedure TdxRtfToBBCodeConverterFieldState.ProcessChar(C: Char;
  AIsHex: Boolean);
begin
  // do nothing
end;

function TdxRtfToBBCodeConverterFieldState.CreateNextStates
  : TDictionary<string, TdxRtfToBBCodeNextStateDelegate>;
begin
  Result := inherited CreateNextStates;
  Result.Add(TdxRtfKeyWords.FieldInstructions, FieldInstructionsHandler);
  Result.Add(TdxRtfKeyWords.FieldResult, FieldResultHandler);
end;

function TdxRtfToBBCodeConverterFieldState.FieldInstructionsHandler
  (AParam: Integer; AHasParam: Boolean): TdxRtfToBBCodeConverterCustomState;
begin
  Result := TdxRtfToBBCodeConverterFieldInstructionsState.Create
    (Converter, Self);
end;

function TdxRtfToBBCodeConverterFieldState.FieldResultHandler(AParam: Integer;
  AHasParam: Boolean): TdxRtfToBBCodeConverterCustomState;
begin
  Result := TdxRtfToBBCodeConverterFieldResultState.Create(Converter, Self);
end;

{ TdxRtfToBBCodeConverterFieldChildState }

constructor TdxRtfToBBCodeConverterFieldChildState.Create
  (AConverter: TdxRtfToBBCodeConverter;
  AParent: TdxRtfToBBCodeConverterFieldState);
begin
  inherited Create(AConverter);
  FParent := AParent;
end;

{ TdxRtfToBBCodeConverterFieldInstructionsState }

procedure TdxRtfToBBCodeConverterFieldInstructionsState.Apply;
var
  S: string;
begin
  if CompareText(FFieldType, TdxRtfKeyWords.HyperlinkFieldType) = 0 then
  begin
    Parent.HasHyperlink := True;
    S := Trim(Instructions);
    if S[1] = TdxRtfKeyWords.QuotationMark then
    begin
      Delete(S, 1, 1);
      if S[Length(S)] = TdxRtfKeyWords.QuotationMark then
        Delete(S, Length(S), 1);
    end;
    Converter.OpenTag(TdxBBCode.bbcURL, S);
  end;
end;

procedure TdxRtfToBBCodeConverterFieldInstructionsState.FlushDecoder;
begin
  // do nothing
end;

function TdxRtfToBBCodeConverterFieldInstructionsState.GetNextState
  (var APos: PWideChar): TdxRtfToBBCodeConverterCustomState;
begin
  Result := inherited GetNextState(APos);
  if Result = nil then
    Result := TdxRtfToBBCodeConverterFieldInstructionsFieldTypeState.Create
      (Converter, Self);
end;

procedure TdxRtfToBBCodeConverterFieldInstructionsState.ProcessChar(C: Char;
  AIsHex: Boolean);
begin
  // do nothing
end;

{ TdxRtfToBBCodeConverterFieldInstructionsFieldTypeState }

constructor TdxRtfToBBCodeConverterFieldInstructionsFieldTypeState.Create
  (AConverter: TdxRtfToBBCodeConverter;
  AParent: TdxRtfToBBCodeConverterFieldInstructionsState);
begin
  inherited Create(AConverter);
  FParent := AParent;
  FIsReadingFieldType := True;
end;

procedure TdxRtfToBBCodeConverterFieldInstructionsFieldTypeState.FlushDecoder;
begin
  // do nothing
end;

procedure TdxRtfToBBCodeConverterFieldInstructionsFieldTypeState.ProcessChar
  (C: Char; AIsHex: Boolean);
begin
  if FIsReadingFieldType and
    dxCharInSet(C, [TdxRtfKeyWords.Space, TdxRtfKeyWords.QuotationMark]) then
  begin
    FIsReadingFieldType := False;
    if C = TdxRtfKeyWords.Space then
      Exit;
  end;
  if FIsReadingFieldType then
    FParent.FieldType := FParent.FieldType + C
  else
    FParent.Instructions := FParent.Instructions + C;
end;

{ TdxRtfToBBCodeConverterFieldResultState }

procedure TdxRtfToBBCodeConverterFieldResultState.Apply;
begin
  if Parent.HasHyperlink then
  begin
    Converter.CloseTagsBeforeTag(TdxBBCode.bbcURL);
    Converter.CloseTag(TdxBBCode.bbcURL);
  end;
end;

function TdxRtfToBBCodeConverterFieldResultState.GetNextState(var APos: PChar)
  : TdxRtfToBBCodeConverterCustomState;
begin
  Result := TdxRtfToBBCodeConverterDefaultState.Create(Converter);
end;

{ TdxRtfToBBCodeConverterEnumState }

procedure TdxRtfToBBCodeConverterEnumState.ApplyChild
  (AChild: TdxRtfToBBCodeConverterEnumItemState);
begin
  // do nothing
end;

{ TdxRtfToBBCodeConverterEnumItemState }

procedure TdxRtfToBBCodeConverterEnumItemState.Apply;
begin
  FParent.ApplyChild(Self);
end;

constructor TdxRtfToBBCodeConverterEnumItemState.Create
  (AParent: TdxRtfToBBCodeConverterEnumState; AIndex: Integer);
begin
  inherited Create(AParent.Converter);
  FParent := AParent;
  FIndex := AIndex;
end;

procedure TdxRtfToBBCodeConverterEnumItemState.SetIndex(const Value: Integer);
begin
  FIndex := Value;
end;

{ TdxRtfToBBCodeConverterRtfFontInfo }

class function TdxRtfToBBCodeConverterRtfFontInfo.Empty
  : TdxRtfToBBCodeConverterRtfFontInfo;
begin
  Result.Name := '';
  Result.Charset := 0;
end;

{ TdxRtfToBBCodeConverterFontTableState }

constructor TdxRtfToBBCodeConverterFontTableState.Create
  (AConverter: TdxRtfToBBCodeConverter);
begin
  inherited Create(AConverter);
  FList := TList<TdxRtfToBBCodeConverterRtfFontInfo>.Create;
end;

destructor TdxRtfToBBCodeConverterFontTableState.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TdxRtfToBBCodeConverterFontTableState.FontItemHandler(AParam: Integer;
  AHasParam: Boolean): TdxRtfToBBCodeConverterCustomState;
begin
  Result := TdxRtfToBBCodeConverterFontTableItemState.Create(Self, AParam);
end;

function TdxRtfToBBCodeConverterFontTableState.CreateNextStates
  : TdxRtfToBBCodeNextStates;
begin
  Result := inherited CreateNextStates;
  Result.Add(TdxRtfKeyWords.FontIndex, FontItemHandler);
end;

procedure TdxRtfToBBCodeConverterFontTableState.Apply;
begin
  Converter.FontTable.AddRange(FList);
end;

procedure TdxRtfToBBCodeConverterFontTableState.ApplyChild
  (AChild: TdxRtfToBBCodeConverterEnumItemState);
var
  AItem: TdxRtfToBBCodeConverterRtfFontInfo;
  AFontItem: TdxRtfToBBCodeConverterFontTableItemState;
begin
  while AChild.Index >= FList.Count do
    FList.Add(TdxRtfToBBCodeConverterRtfFontInfo.Empty);
  AFontItem := TdxRtfToBBCodeConverterFontTableItemState(AChild);
  if Length(AFontItem.OriginalFontName) > 0 then
    AItem.Name := AFontItem.OriginalFontName
  else
    AItem.Name := AChild.Decoder.ToString;
  AItem.Charset := TdxRtfToBBCodeConverterFontTableItemState(AChild).Charset;
  FList[AChild.Index] := AItem;
end;

procedure TdxRtfToBBCodeConverterFontTableState.FlushDecoder;
begin
  // do nothing
end;

{ TdxRtfToBBCodeCharacterFontTableItemDecoder }

function TdxRtfToBBCodeCharacterFontTableItemDecoder.GetEncoding: TEncoding;
begin
  if not FUseCharset or (State.Charset = -1) then
    Result := inherited GetEncoding
  else if State.Charset = 0 then
    Result := TEncoding.Unicode
  else
    Result := TdxEncoding.GetEncoding
      (TdxEncoding.CodePageFromCharset(State.Charset));
end;

procedure TdxRtfToBBCodeCharacterFontTableItemDecoder.Flush;
begin
  // do nothing
end;

procedure TdxRtfToBBCodeCharacterFontTableItemDecoder.ProcessHexChar(C: Char);
begin
  inherited ProcessHexChar(C);
  FUseCharset := True;
end;

function TdxRtfToBBCodeCharacterFontTableItemDecoder.GetState
  : TdxRtfToBBCodeConverterFontTableItemState;
begin
  Result := TdxRtfToBBCodeConverterFontTableItemState(inherited State);
end;

{ TdxRtfToBBCodeConverterFontNameState }

constructor TdxRtfToBBCodeConverterFontNameState.Create
  (AOwner: TdxRtfToBBCodeConverterFontTableItemState);
begin
  inherited Create(AOwner.Converter);
  FOwner := AOwner;
end;

procedure TdxRtfToBBCodeConverterFontNameState.Apply;
var
  ALength: Integer;
  AResult: string;
begin
  AResult := Decoder.ToString;
  ALength := Length(AResult);
  if (ALength > 0) and (AResult[ALength] = ';') then
    Delete(AResult, ALength, 1);
  FOwner.OriginalFontName := AResult;
end;

{ TdxRtfToBBCodeConverterFontTableItemState }

procedure TdxRtfToBBCodeConverterFontTableItemState.AfterConstruction;
begin
  inherited AfterConstruction;
  FCharset := -1;
  FOriginalFontName := '';
end;

function TdxRtfToBBCodeConverterFontTableItemState.CreateDecoder
  : TdxRtfToBBCodeCharacterDecoder;
begin
  Result := TdxRtfToBBCodeCharacterFontTableItemDecoder.Create(Self);
end;

function TdxRtfToBBCodeConverterFontTableItemState.CreateNextStates
  : TDictionary<string, TdxRtfToBBCodeNextStateDelegate>;
begin
  Result := inherited CreateNextStates;
  Result.Add(TdxRtfKeyWords.FontName, FontNameHandler);
end;

function TdxRtfToBBCodeConverterFontTableItemState.CreateTranslators
  : TdxRtfToBBCodeTranslators;
begin
  Result := inherited CreateTranslators;
  Result.Add(TdxRtfKeyWords.FontCharset, FontCharsetHandler);
end;

procedure TdxRtfToBBCodeConverterFontTableItemState.FlushDecoder;
begin
  // do nothing
end;

procedure TdxRtfToBBCodeConverterFontTableItemState.ProcessChar(C: Char;
  AIsHex: Boolean);
begin
  if C <> TdxRtfKeyWords.EntrySeparator then
    inherited ProcessChar(C, AIsHex);
end;

procedure TdxRtfToBBCodeConverterFontTableItemState.FontCharsetHandler
  (AParam: Integer; AHasParam: Boolean);
begin
  if AHasParam then
    FCharset := AParam;
end;

function TdxRtfToBBCodeConverterFontTableItemState.FontNameHandler
  (AParam: Integer; AHasParam: Boolean): TdxRtfToBBCodeConverterCustomState;
begin
  Result := TdxRtfToBBCodeConverterFontNameState.Create(Self);
end;

{ TdxRtfToBBCodeConverterColorTableState }

constructor TdxRtfToBBCodeConverterColorTableState.Create
  (AConverter: TdxRtfToBBCodeConverter);
begin
  inherited Create(AConverter);
  FList := TList<TColor>.Create;
end;

destructor TdxRtfToBBCodeConverterColorTableState.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

function TdxRtfToBBCodeConverterColorTableState.CreateTranslators
  : TdxRtfToBBCodeTranslators;
begin
  Result := inherited CreateTranslators;
  Result.Add(TdxRtfKeyWords.ColorRed, ColorRedHandler);
  Result.Add(TdxRtfKeyWords.ColorGreen, ColorGreenHandler);
  Result.Add(TdxRtfKeyWords.ColorBlue, ColorBlueHandler);
end;

procedure TdxRtfToBBCodeConverterColorTableState.ColorRedHandler
  (AParam: Integer; AHasParam: Boolean);
var
  AState: TdxRtfToBBCodeConverterColorTableItemState;
begin
  AState := TdxRtfToBBCodeConverterColorTableItemState.Create(Self,
    FList.Count);
  AState.ColorRedHandler(AParam, AHasParam);
  Converter.PushState(AState);
end;

procedure TdxRtfToBBCodeConverterColorTableState.ColorGreenHandler
  (AParam: Integer; AHasParam: Boolean);
var
  AState: TdxRtfToBBCodeConverterColorTableItemState;
begin
  AState := TdxRtfToBBCodeConverterColorTableItemState.Create(Self,
    FList.Count);
  AState.ColorGreenHandler(AParam, AHasParam);
  Converter.PushState(AState);
end;

procedure TdxRtfToBBCodeConverterColorTableState.ColorBlueHandler
  (AParam: Integer; AHasParam: Boolean);
var
  AState: TdxRtfToBBCodeConverterColorTableItemState;
begin
  AState := TdxRtfToBBCodeConverterColorTableItemState.Create(Self,
    FList.Count);
  AState.ColorGreenHandler(AParam, AHasParam);
  Converter.PushState(AState);
end;

procedure TdxRtfToBBCodeConverterColorTableState.Apply;
begin
  if FList.Count = 0 then
    FList.Add(0);
  Converter.ColorTable.Clear;
  Converter.ColorTable.AddRange(FList);
end;

procedure TdxRtfToBBCodeConverterColorTableState.ApplyChild
  (AChild: TdxRtfToBBCodeConverterEnumItemState);
var
  AColorItem: TdxRtfToBBCodeConverterColorTableItemState absolute AChild;
begin
  while AChild.Index >= FList.Count do
    FList.Add(clNone);
  if AColorItem.HasColor then
    FList[AColorItem.Index] := AColorItem.Color;
end;

procedure TdxRtfToBBCodeConverterColorTableState.ProcessChar(C: Char;
  AIsHex: Boolean);
begin
  if (C = TdxRtfKeyWords.EntrySeparator) and (FList.Count = 0) then
    FList.Add(0);
end;

procedure TdxRtfToBBCodeConverterColorTableState.FlushDecoder;
begin
  // do nothing
end;

{ TdxRtfToBBCodeConverterColorTableItemState }

procedure TdxRtfToBBCodeConverterColorTableItemState.FlushDecoder;
begin
  // do nothing
end;

procedure TdxRtfToBBCodeConverterColorTableItemState.ProcessChar(C: Char;
  AIsHex: Boolean);
begin
  if C = TdxRtfKeyWords.EntrySeparator then
    Converter.PopState;
end;

function TdxRtfToBBCodeConverterColorTableItemState.CreateTranslators
  : TdxRtfToBBCodeTranslators;
begin
  Result := inherited CreateTranslators;
  Result.Add(TdxRtfKeyWords.ColorRed, ColorRedHandler);
  Result.Add(TdxRtfKeyWords.ColorGreen, ColorGreenHandler);
  Result.Add(TdxRtfKeyWords.ColorBlue, ColorBlueHandler);
end;

function TdxRtfToBBCodeConverterColorTableItemState.GetColor: TColor;
begin
  Result := RGB(FRed, FGreen, FBlue);
end;

function TdxRtfToBBCodeConverterColorTableItemState.GetParent
  : TdxRtfToBBCodeConverterColorTableState;
begin
  Result := TdxRtfToBBCodeConverterColorTableState(inherited Parent);
end;

procedure TdxRtfToBBCodeConverterColorTableItemState.ColorRedHandler
  (AParam: Integer; AHasParam: Boolean);
begin
  if AHasParam then
    FRed := AParam;
  FHasColor := True;
end;

procedure TdxRtfToBBCodeConverterColorTableItemState.ColorGreenHandler
  (AParam: Integer; AHasParam: Boolean);
begin
  if AHasParam then
    FGreen := AParam;
  FHasColor := True;
end;

procedure TdxRtfToBBCodeConverterColorTableItemState.ColorBlueHandler
  (AParam: Integer; AHasParam: Boolean);
begin
  if AHasParam then
    FBlue := AParam;
  FHasColor := True;
end;

{ TdxRtfToBBCodeConverterHeadState }

function TdxRtfToBBCodeConverterHeadState.CreateNextStates
  : TdxRtfToBBCodeNextStates;
begin
  Result := inherited CreateNextStates;
  Result.Add(TdxRtfKeyWords.Field, FieldHandler);
  Result.Add(TdxRtfKeyWords.Plain, PlainHandler);
end;

function TdxRtfToBBCodeConverterHeadState.GetNextState(var APos: PChar)
  : TdxRtfToBBCodeConverterCustomState;
begin
  Result := inherited GetNextState(APos);
  if (Result = nil) and (APos^ = TdxRtfKeyWords.OpenGroup) then
    Result := TdxRtfToBBCodeConverterHeadState.Create(Converter);
end;

function TdxRtfToBBCodeConverterHeadState.CanStartProcessKeyword
  (const AKeyword: string; AParam: Integer; AHasParam: Boolean): Boolean;
var
  S: string;
begin
  S := LowerCase(AKeyword);
  Result := (S <> TdxRtfKeyWords.FontUnderlineNone) and
    (not AHasParam or (AParam <> 0));
end;

function TdxRtfToBBCodeConverterHeadState.FieldHandler(AParam: Integer;
  AHasParam: Boolean): TdxRtfToBBCodeConverterCustomState;
begin
  Result := TdxRtfToBBCodeConverterFieldState.Create(Converter);
end;

function TdxRtfToBBCodeConverterHeadState.PlainHandler(AParam: Integer;
  AHasParam: Boolean): TdxRtfToBBCodeConverterCustomState;
begin
  Result := TdxRtfToBBCodeConverterPlainState.Create(Converter);
end;

{ TdxRtfToBBCodeConverterDefaultCharacterPropertiesState }

constructor TdxRtfToBBCodeConverterDefaultCharacterPropertiesState.Create
  (AConverter: TdxRtfToBBCodeConverter);
begin
  inherited Create(AConverter);
  Converter.DefaultFontStyle := [];
end;

function TdxRtfToBBCodeConverterDefaultCharacterPropertiesState.
  CreateTranslators: TdxRtfToBBCodeTranslators;
begin
  Result := inherited CreateTranslators;
  Result.Add(TdxRtfKeyWords.FontBold, FontBoldHandler);
  Result.Add(TdxRtfKeyWords.FontItalic, FontItalicHandler);
  Result.Add(TdxRtfKeyWords.FontUnderline, FontUnderlineHandler);
  Result.Add(TdxRtfKeyWords.FontStrikeout, FontStrikeoutHandler);
  Result.Add(TdxRtfKeyWords.FontSize, FontSizeHandler);
  Result.Add(TdxRtfKeyWords.FontIndex, FontIndexHandler);
  Result.Add(TdxRtfKeyWords.RunForegroundColor, ForegroundColorHandler);
end;

procedure TdxRtfToBBCodeConverterDefaultCharacterPropertiesState.
  FontStyleHandler(AStyle: TFontStyle; AParam: Integer; AHasParam: Boolean);
begin
  if not AHasParam or (AParam <> 0) then
    Converter.DefaultFontStyle := Converter.DefaultFontStyle + [AStyle]
  else
    Converter.DefaultFontStyle := Converter.DefaultFontStyle - [AStyle];
end;

procedure TdxRtfToBBCodeConverterDefaultCharacterPropertiesState.FontBoldHandler
  (AParam: Integer; AHasParam: Boolean);
begin
  FontStyleHandler(fsBold, AParam, AHasParam);
end;

procedure TdxRtfToBBCodeConverterDefaultCharacterPropertiesState.
  FontItalicHandler(AParam: Integer; AHasParam: Boolean);
begin
  FontStyleHandler(fsItalic, AParam, AHasParam);
end;

procedure TdxRtfToBBCodeConverterDefaultCharacterPropertiesState.
  FontStrikeoutHandler(AParam: Integer; AHasParam: Boolean);
begin
  FontStyleHandler(fsStrikeOut, AParam, AHasParam);
end;

procedure TdxRtfToBBCodeConverterDefaultCharacterPropertiesState.
  FontUnderlineHandler(AParam: Integer; AHasParam: Boolean);
begin
  FontStyleHandler(fsUnderline, AParam, AHasParam);
end;

procedure TdxRtfToBBCodeConverterDefaultCharacterPropertiesState.FontSizeHandler
  (AParam: Integer; AHasParam: Boolean);
begin
  if AHasParam then
    Converter.DefaultFontSize := AParam div 2;
end;

procedure TdxRtfToBBCodeConverterDefaultCharacterPropertiesState.
  FontIndexHandler(AParam: Integer; AHasParam: Boolean);
begin
  if AHasParam then
    Converter.DefaultFontIndex := AParam;
end;

procedure TdxRtfToBBCodeConverterDefaultCharacterPropertiesState.
  ForegroundColorHandler(AParam: Integer; AHasParam: Boolean);
begin
  if AHasParam then
    Converter.DefaultColorIndex := AParam;
end;

{ TdxRtfToBBCodeConverterRtfState }

function TdxRtfToBBCodeConverterRtfState.CreateNextStates
  : TdxRtfToBBCodeNextStates;
begin
  Result := inherited CreateNextStates;
  Result.Add(TdxRtfKeyWords.DefaultCharacterProperties,
    DefaultCharacterPropertiesHandler);
  Result.Add(TdxRtfKeyWords.FontTable, FontTableHandler);
  Result.Add(TdxRtfKeyWords.ColorTable, ColorTableHandler);
end;

function TdxRtfToBBCodeConverterRtfState.CreateTranslators
  : TdxRtfToBBCodeTranslators;
begin
  Result := inherited CreateTranslators;
  Result.Add(TdxRtfKeyWords.AnsiCodePage, AnsiCodePageHandler);
end;

procedure TdxRtfToBBCodeConverterRtfState.AnsiCodePageHandler(AParam: Integer;
  AHasParam: Boolean);
begin
  if AHasParam then
    Converter.AnsiCodePage := AParam;
end;

function TdxRtfToBBCodeConverterRtfState.ColorTableHandler(AParam: Integer;
  AHasParam: Boolean): TdxRtfToBBCodeConverterCustomState;
begin
  Result := TdxRtfToBBCodeConverterColorTableState.Create(Converter);
end;

function TdxRtfToBBCodeConverterRtfState.DefaultCharacterPropertiesHandler
  (AParam: Integer; AHasParam: Boolean): TdxRtfToBBCodeConverterCustomState;
begin
  Result := TdxRtfToBBCodeConverterDefaultCharacterPropertiesState.Create
    (Converter);
end;

function TdxRtfToBBCodeConverterRtfState.FontTableHandler(AParam: Integer;
  AHasParam: Boolean): TdxRtfToBBCodeConverterCustomState;
begin
  Result := TdxRtfToBBCodeConverterFontTableState.Create(Converter);
end;

{ TdxRtfToBBCodeConverter }

constructor TdxRtfToBBCodeConverter.Create(ADefaultFont: TFont);
begin
  inherited Create;
  FKeepLastParagraph := True;
  FDefaultFont := ADefaultFont;
  FBuilder := TStringBuilder.Create;
  FStates := TObjectStack<TdxRtfToBBCodeConverterCustomState>.Create;
  FTags := TList<TdxBBCode>.Create;
  FFontTable := TList<TdxRtfToBBCodeConverterRtfFontInfo>.Create;
  FColorTable := TList<TColor>.Create;
end;

destructor TdxRtfToBBCodeConverter.Destroy;
begin
  FreeAndNil(FColorTable);
  FreeAndNil(FFontTable);
  FreeAndNil(FTags);
  FreeAndNil(FStates);
  FreeAndNil(FBuilder);
  inherited Destroy;
end;

procedure TdxRtfToBBCodeConverter.Error;
begin
  FHasError := True;
end;

function TdxRtfToBBCodeConverter.GetBBCodeText(const ARtfText: string): string;
var
  APos: PChar;
  AEnd: PChar;
begin
  Clear;
  FHasError := False;
  if ARtfText = '' then
    Exit('');
  APos := PChar(ARtfText);
  AEnd := APos + Length(ARtfText);
  while APos < AEnd do
  begin
    if FHasError then
      Exit('');
    case APos^ of
      TdxRtfKeyWords.OpenGroup:
        begin
          FSkipCharCount := 0;
          Inc(APos);
          PushNextState(APos);
        end;
      TdxRtfKeyWords.CloseGroup:
        begin
          FSkipCharCount := 0;
          PopState;
          Inc(APos);
        end;
      '\':
        begin
          FSkipCharCount := 0;
          ParseRtfKeyword(APos);
        end;
      TdxCharacters.ParagraphMark, dxLF:
        Inc(APos);
    else
      ProcessChar(APos^);
      Inc(APos);
    end;
  end;
  while FStates.Count > 0 do
    PopState;
  if not KeepLastParagraph then
  begin
    if TdxStringHelper.EndsWith(FBuilder.ToString, dxCRLF) then
      FBuilder.Remove(FBuilder.Length - 2, 2);
  end;
  while FTags.Count > 0 do
    CloseTag(FTags.Last);
  Result := FBuilder.ToString;
  ApplyDefaultFontSettings;
end;

function TdxRtfToBBCodeConverter.GetRtfKeyword(var APos: PChar;
  out AKeyword: string; out AParam: Integer; out AHasParam: Boolean): Boolean;

  function IsAlpha(const C: Char): Boolean;
  begin
    Result := ((C >= 'a') and (C <= 'z')) or ((C >= 'A') and (C <= 'Z'));
  end;

  function IsDigit(const C: Char): Boolean;
  begin
    Result := (C >= '0') and (C <= '9');
  end;

var
  AParamString: string;
  AIsNegative: Boolean;
begin
  Result := False;
  if APos^ <> '\' then
    Exit;
  AKeyword := '\';
  Inc(APos);
  if APos^ = '*' then
  begin
    AKeyword := AKeyword + APos^;
    Inc(APos);
    if APos^ <> '\' then
      Exit;
    AKeyword := AKeyword + APos^;
    Inc(APos);
  end;
  while IsAlpha(APos^) do
  begin
    AKeyword := AKeyword + APos^;
    Inc(APos);
  end;
  if Length(AKeyword) <= 1 then
    Exit;
  AIsNegative := APos^ = '-';
  if AIsNegative then
    Inc(APos);
  AParamString := '';
  while IsDigit(APos^) do
  begin
    AParamString := AParamString + APos^;
    Inc(APos);
  end;
  AHasParam := TryStrToInt(AParamString, AParam);
  if AHasParam and AIsNegative then
    AParam := -AParam;
  Result := True;
  if APos^ = TdxCharacters.Space then
    Inc(APos);
end;

procedure TdxRtfToBBCodeConverter.ParseRtfKeyword(var APos: PChar);
var
  AKeyword: string;
  AParamValue: Integer;
  AHasParam: Boolean;
begin
  if GetRtfKeyword(APos, AKeyword, AParamValue, AHasParam) then
    TranslateKeyword(AKeyword, AParamValue, AHasParam)
  else if TdxRtfKeyWords.IsSpecialSymbol(APos^) then
  begin
    ProcessChar(APos^);
    Inc(APos);
  end
  else if APos^ = '''' then
  begin
    Inc(APos);
    ProcessHexChar(APos);
  end;
end;

procedure TdxRtfToBBCodeConverter.ProcessChar(const C: Char;
  AIsHex: Boolean = False);
begin
  if FSkipCharCount > 0 then
    Dec(FSkipCharCount)
  else if FStates.Count > 0 then
    FStates.Peek.ProcessChar(C, AIsHex);
end;

procedure TdxRtfToBBCodeConverter.ProcessHexChar(var APos: PChar);

  function HexToInt(AChar: Char): Byte;
  begin
    if CharInSet(AChar, ['0' .. '9']) then
      Result := Ord(AChar) - Ord('0')
    else
    begin
      if CharInSet(AChar, ['a' .. 'z']) then
        Result := 10 + Ord(AChar) - Ord('a')
      else if CharInSet(AChar, ['A' .. 'Z']) then
        Result := 10 + Ord(AChar) - Ord('A')
      else
        Result := 0;
    end;
  end;

var
  AHex: Byte;
begin
  AHex := HexToInt(APos^) shl 4;
  Inc(APos);
  AHex := AHex + HexToInt(APos^);
  Inc(APos);
  if not WaitingForHexChar then
    ProcessChar(Chr(AHex), True);
  FWaitingForHexChar := False;
end;

procedure TdxRtfToBBCodeConverter.ProcessUnicodeChar(const C: Char);
begin
  WaitingForHexChar := True;
  FlushDecoder;
  Builder.Append(C);
  FSkipCharCount := UnicodeCharacterByteCount;
end;

procedure TdxRtfToBBCodeConverter.Clear;
begin
  FBuilder.Clear;
  FStates.Clear;
  FTags.Clear;
  FFontTable.Clear;
  FColorTable.Clear;
  FAnsiCodePage := CP_ACP;
  GetDefaultFontSettings;
  FUnicodeCharacterByteCount := 1;
end;

procedure TdxRtfToBBCodeConverter.ApplyDefaultFontSettings;
begin
  if (DefaultColorIndex <> -1) and (DefaultColorIndex < ColorTable.Count) then
    FDefaultFont.Color := ColorTable[DefaultColorIndex];
  if (DefaultFontIndex <> -1) and (DefaultFontIndex < FontTable.Count) then
  begin
    FDefaultFont.Name := FontTable[DefaultFontIndex].Name;
    if FontTable[DefaultFontIndex].Charset > 0 then
      FDefaultFont.Charset := FontTable[DefaultFontIndex].Charset;
  end;
  if FDefaultFontSize <> -1 then
    FDefaultFont.Size := FDefaultFontSize;
  FDefaultFont.Style := FDefaultFontStyle;
end;

procedure TdxRtfToBBCodeConverter.CloseTagsBeforeTag(ATag: TdxBBCode);
var
  I: Integer;
begin
  for I := FTags.Count - 1 downto 0 do
  begin
    if FTags[I] = ATag then
      Break;
    CloseTag(FTags.Last);
  end;
end;

procedure TdxRtfToBBCodeConverter.CloseTag(ATag: TdxBBCode);
begin
  FlushDecoder;
  if FTags.Remove(ATag) >= 0 then
    Builder.Append(Format('[/%s]', [dxBBCodeToString(ATag)]));
end;

function TdxRtfToBBCodeConverter.DoOpenTag(ATag: TdxBBCode): Boolean;
begin
  Result := not FTags.Contains(ATag);
  if not Result then
    Exit;
  FlushDecoder;
  FTags.Add(ATag);
end;

procedure TdxRtfToBBCodeConverter.GetDefaultFontSettings;
begin
  FDefaultFontIndex := -1;
  FDefaultColorIndex := -1;
  FDefaultFontSize := Abs(FDefaultFont.Size);
  FDefaultFontStyle := FDefaultFont.Style;
end;

procedure TdxRtfToBBCodeConverter.OpenTag(ATag: TdxBBCode);
begin
  if DoOpenTag(ATag) then
    Builder.Append(Format('[%s]', [dxBBCodeToString(ATag)]));
end;

procedure TdxRtfToBBCodeConverter.OpenTag(ATag: TdxBBCode; AParam: Integer);
begin
  if DoOpenTag(ATag) then
    Builder.Append(Format('[%s=%d]', [dxBBCodeToString(ATag), AParam]));
end;

procedure TdxRtfToBBCodeConverter.OpenTag(ATag: TdxBBCode;
  const AParam: string);
begin
  if DoOpenTag(ATag) then
    Builder.Append(Format('[%s=%s]', [dxBBCodeToString(ATag), AParam]));
end;

procedure TdxRtfToBBCodeConverter.FlushDecoder;
begin
  if FStates.Count > 0 then
    FStates.Peek.FlushDecoder;
end;

function TdxRtfToBBCodeConverter.GetCurrentCodePage: Cardinal;
begin
  if FontCodePage > 0 then
    Result := FontCodePage
  else if AnsiCodePage > 0 then
    Result := AnsiCodePage
  else
    Result := GetACP;
end;

procedure TdxRtfToBBCodeConverter.PopState;
begin
  FStates.Peek.Apply;
  FStates.Pop;
end;

procedure TdxRtfToBBCodeConverter.PushNextState(var APos: PChar);
var
  ANextState: TdxRtfToBBCodeConverterCustomState;
  AKeyword: string;
  AParam: Integer;
  AHasParam: Boolean;
begin
  FlushDecoder;
  ANextState := nil;
  if FStates.Count > 0 then
    ANextState := PeekState.GetNextState(APos)
  else if GetRtfKeyword(APos, AKeyword, AParam, AHasParam) and
    (CompareText(AKeyword, TdxRtfKeyWords.RtfSignature) = 0) then
    ANextState := TdxRtfToBBCodeConverterRtfState.Create(Self);
  if ANextState <> nil then
    PushState(ANextState)
  else
    PushSkipState;
end;

procedure TdxRtfToBBCodeConverter.PushSkipState;
begin
  PushState(TdxRtfToBBCodeConverterSkipState);
end;

procedure TdxRtfToBBCodeConverter.PushState(const AClass
  : TdxRtfToBBCodeConverterStateClass);
begin
  PushState(AClass.Create(Self));
end;

procedure TdxRtfToBBCodeConverter.PushState
  (AState: TdxRtfToBBCodeConverterCustomState);
begin
  FStates.Push(AState);
end;

function TdxRtfToBBCodeConverter.PeekState: TdxRtfToBBCodeConverterCustomState;
begin
  Result := FStates.Peek;
end;

procedure TdxRtfToBBCodeConverter.TranslateKeyword(const AKeyword: string;
  AParam: Integer; AHasParam: Boolean);
begin
  FStates.Peek.ProcessKeyword(AKeyword, AParam, AHasParam);
end;

{ TdxFormattedTextConverterRTF }

class function TdxFormattedTextConverterRTF.CanImport(const ASource
  : string): Boolean;
begin
  Result := StartsStr(TdxRtfKeyWords.OpenGroup +
    TdxRtfKeyWords.RtfSignature, ASource);
end;

class procedure TdxFormattedTextConverterRTF.Import(ATarget: TdxFormattedText;
  const ASource: string; ADefaultFont: TFont = nil);
begin
  Import(ATarget, ASource, ADefaultFont, DefaultRichEditCompatibility);
end;

class procedure TdxFormattedTextConverterRTF.Import(ATarget: TdxFormattedText;
  const ASource: string; ADefaultFont: TFont; ARichEditCompatibility: Boolean);
begin
  if ADefaultFont = nil then
  begin
    ADefaultFont := TFont.Create;
    try
      Import(ATarget, ASource, ADefaultFont, ARichEditCompatibility);
    finally
      ADefaultFont.Free;
    end;
  end
  else
    inherited Import(ATarget, TdxFormattedTextRtfHelper.GetBBCodeText
      (ADefaultFont, ASource, ARichEditCompatibility), ADefaultFont);
end;

class function TdxFormattedTextConverterRTF.Export(ASource: TdxFormattedText;
  ADefaultFont: TFont): string;
begin
  Result := Export(ASource, ADefaultFont, False);
end;

class function TdxFormattedTextConverterRTF.Export(ASource: TdxFormattedText;
  ADefaultFont: TFont; ARichEditCompatibility: Boolean): string;
begin
  if ADefaultFont = nil then
  begin
    ADefaultFont := TFont.Create;
    try
      Result := Export(ASource, ADefaultFont, ARichEditCompatibility);
    finally
      ADefaultFont.Free;
    end;
  end
  else
    Result := TdxFormattedTextRtfHelper.GetRtfText(ADefaultFont, ASource,
      ARichEditCompatibility);
end;

{ TdxFormattedTextRtfHelper }

class function TdxFormattedTextRtfHelper.GetRtfText(ADefaultFont: TFont;
  AFormattedText: TdxFormattedText; ARichEditCompatibility: Boolean): string;
var
  AExporter: TdxFormattedTextRtfExporter;
begin
  AExporter := TdxFormattedTextRtfExporter.Create(ADefaultFont,
    AFormattedText.Runs);
  try
    AExporter.ForceAddLastParagraph := ARichEditCompatibility;
    Result := AExporter.ExportAsString;
  finally
    AExporter.Free;
  end;
end;

class function TdxFormattedTextRtfHelper.GetBBCodeText(ADefaultFont: TFont;
  const ARtfText: string): string;
begin
  Result := GetBBCodeText(ADefaultFont, ARtfText,
    TdxFormattedTextConverterRTF.DefaultRichEditCompatibility);
end;

class function TdxFormattedTextRtfHelper.GetRtfText(ADefaultFont: TFont;
  const ABBCodeString: string; ARichEditCompatibility: Boolean): string;
var
  AFormattedText: TdxFormattedText;
begin
  AFormattedText := TdxFormattedText.Create;
  try
    TdxFormattedTextConverterBBCode.Import(AFormattedText, ABBCodeString,
      ADefaultFont);
    Result := GetRtfText(ADefaultFont, AFormattedText, ARichEditCompatibility);
  finally
    AFormattedText.Free;
  end;
end;

class function TdxFormattedTextRtfHelper.GetBBCodeText(ADefaultFont: TFont;
  const ARtfText: string; ARichEditCompatibility: Boolean): string;
var
  AConverter: TdxRtfToBBCodeConverter;
begin
  AConverter := TdxRtfToBBCodeConverter.Create(ADefaultFont);
  try
    AConverter.KeepLastParagraph := not ARichEditCompatibility;
    Result := AConverter.GetBBCodeText(ARtfText);
  finally
    AConverter.Free;
  end;
end;

class function TdxFormattedTextRtfHelper.GetRtfText(ADefaultFont: TFont;
  AFormattedText: TdxFormattedText): string;
begin
  Result := GetRtfText(ADefaultFont, AFormattedText,
    TdxFormattedTextConverterRTF.DefaultRichEditCompatibility);
end;

class function TdxFormattedTextRtfHelper.GetRtfText(ADefaultFont: TFont;
  const ABBCodeString: string): string;
begin
  Result := GetRtfText(ADefaultFont, ABBCodeString,
    TdxFormattedTextConverterRTF.DefaultRichEditCompatibility);
end;

initialization

TdxFormattedTextConverters.Register(TdxFormattedTextConverterRTF);

finalization

TdxFormattedTextConverters.Unregister(TdxFormattedTextConverterRTF);

end.
